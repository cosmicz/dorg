;;; dorg-transport-tests.el --- Tests for strict SQL transport policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the DOrg SQL transport policy (`dorg-sql-transport') and the
;; same-host server ownership claim machinery added for the strict
;; server-only transport mode.
;;
;; The routing/conflict/rollback tests are fully mocked — no dolt binary
;; and no sql-server — so they assert the policy contract itself,
;; including the non-vacuous guarantee that strict `server' mode never
;; calls `dorg-sql--cli-exec'.  A handful of
;; claim-liveness tests use real loopback sockets but still need no dolt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-repo)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el

;;; ─── Vendored wire packet contract ─────────────────────────────────────

(ert-deftest dorg-test-transport-packets-use-one-write-per-fragment ()
  "Each wire fragment is one complete write, including empty terminators.
Separate header/payload writes can incur a TCP delayed-ACK round trip per
command.  Assert bytes and write counts rather than a wall-clock threshold."
  (require 'mysql)
  (dolist (size '(0 3 16777215 16777216))
    (let* ((payload (make-string size 97))
           (conn (make-mysql-conn :sequence-id 255))
           (expected-lengths (if (< size #xffffff)
                                 (list size)
                               (list #xffffff (- size #xffffff))))
           writes)
      (cl-letf (((symbol-function 'mysql--live-process) (lambda (_) 'wire))
                ((symbol-function 'mysql--send-string)
                 (lambda (proc bytes)
                   (should (eq 'wire proc))
                   (push bytes writes))))
        (mysql--send-packet conn payload))
      (setq writes (nreverse writes))
      (should (= (length writes) (length expected-lengths)))
      (let ((sequence 255)
            (offset 0))
        (cl-mapc
         (lambda (packet size)
           (should (= (+ 4 size) (length packet)))
           (should (equal (substring packet 0 4)
                          (unibyte-string (logand size 255)
                                          (logand (ash size -8) 255)
                                          (logand (ash size -16) 255)
                                          sequence)))
           (should (equal (substring packet 4)
                          (substring payload offset (+ offset size))))
           (cl-incf offset size)
           (setq sequence (logand (1+ sequence) 255)))
         writes expected-lengths)
        (should (= sequence (mysql-conn-sequence-id conn)))))))

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defvar dorg-test-transport--store-root nil
  "Temp store-root bound by `dorg-test-transport--with-runtime'.
Claims are store-relative (`<store>/.dorg/'), so test \"stores\" live
under an isolated, auto-cleaned root.")

(defvar dorg-test-transport--fallback-root nil
  "Memoized fallback store-root for `dorg-test-transport--repo' calls made
outside `dorg-test-transport--with-runtime', so repeated calls with the
same NAME return the SAME path within a run.")

(defun dorg-test-transport--repo (name)
  "Return an isolated test store dir NAME under the transport store-root.
Outside the fixture, uses a per-run memoized fallback root so the path is
stable across calls with the same NAME."
  (file-name-as-directory
   (expand-file-name name (or dorg-test-transport--store-root
                              (or dorg-test-transport--fallback-root
                                  (setq dorg-test-transport--fallback-root
                                        (dorg-test--make-owned-temp-dir
                                         "dorg-tx-store-" "transport-fallback")))))))

(defmacro dorg-test-transport--with-runtime (&rest body)
  "Run BODY with an isolated store-root and claim state.
The store-relative `<store>/.dorg/' overlays land under an isolated
`dorg-test-transport--store-root'."
  (declare (indent 0))
  `(let ((dorg-test-transport--store-root
          (dorg-test--make-owned-temp-dir "dorg-tx-store-" "transport-runtime"))
         (dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
         (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (dorg-test--delete-owned-root dorg-test-transport--store-root))))

(defun dorg-test-transport--closed-port ()
  "Return a TCP port number with nothing listening on it."
  (dorg-dolt-server--free-port))

(defun dorg-test-transport--make-live-port ()
  "Open a loopback TCP server and return (PROC . PORT) that answers."
  (let* ((proc (make-network-process
                :name "dorg-test-live" :host "127.0.0.1"
                :service 0 :server t :family 'ipv4)))
    (cons proc (process-contact proc :service))))

;;; ─── Wrong-store claim discovery ───────────────────────────────────────

(defconst dorg-test-transport--crlf-wire
  (encode-coding-string "line\r\nbreak \u00fc\r\n" 'utf-8-unix)
  "Exact UTF-8 wire bytes of a text value holding CRLF and a multibyte char.")

(ert-deftest dorg-test-transport-text-row-decoder-preserves-crlf ()
  "The text-protocol row decoder keeps CR LF: wire bytes decode without any
end-of-line conversion, so a stored CRLF value reads back as CRLF."
  (let ((column (list :type mysql-type-var-string :character-set 33 :flags 0)))
    (should (equal "line\r\nbreak \u00fc\r\n"
                   (mysql--parse-typed-value dorg-test-transport--crlf-wire
                                             mysql-type-var-string column)))
    (should (equal "line\r\nbreak \u00fc\r\n"
                   (mysql--parse-value dorg-test-transport--crlf-wire column)))
    (should (equal "a\rb\nc"
                   (mysql--parse-typed-value "a\rb\nc" mysql-type-blob
                                             (list :type mysql-type-blob
                                                   :character-set 33 :flags 0))))))

(ert-deftest dorg-test-transport-binary-row-decoder-preserves-crlf ()
  "The prepared/binary-protocol string decoder keeps CR LF as well."
  (should (equal "line\r\nbreak \u00fc\r\n"
                 (mysql--binary-text-value
                  dorg-test-transport--crlf-wire
                  (list :type mysql-type-var-string :character-set 33 :flags 0)))))

(ert-deftest dorg-test-transport-row-decoders-leave-binary-and-custom-raw ()
  "Controls: binary-charset or BINARY-flag columns and custom type parsers still
receive the exact undecoded wire bytes in both protocols."
  (dolist (column (list (list :type mysql-type-var-string
                              :character-set mysql--binary-character-set :flags 0)
                        (list :type mysql-type-blob :character-set 33
                              :flags mysql--column-flag-binary)))
    (should (equal dorg-test-transport--crlf-wire
                   (mysql--parse-typed-value dorg-test-transport--crlf-wire
                                             (plist-get column :type) column)))
    (should-not (multibyte-string-p
                 (mysql--parse-typed-value dorg-test-transport--crlf-wire
                                           (plist-get column :type) column)))
    (should (equal dorg-test-transport--crlf-wire
                   (mysql--binary-text-value dorg-test-transport--crlf-wire column))))
  (let ((mysql-type-parsers (list (cons mysql-type-var-string #'identity))))
    (should (equal dorg-test-transport--crlf-wire
                   (mysql--parse-typed-value
                    dorg-test-transport--crlf-wire mysql-type-var-string
                    (list :type mysql-type-var-string :character-set 33 :flags 0))))))

(ert-deftest dorg-test-transport-copied-overlay-refuses-before-attach ()
  "A copied DOrg overlay never selects the other store endpoint.
The positive controls use the owner store and a symlink alias of that exact
store.  The negative control uses two same-basename synthetic stores and a
claim copied from OWNER into COPY.  Every attach/classification sentinel must
remain untouched on both first and repeated COPY discovery."
  (dorg-test-transport--with-runtime
    (let* ((owner (dorg-test-transport--repo "owner/.dorg/dorg"))
           (copy (dorg-test-transport--repo "copy/.dorg/dorg"))
           (alias (expand-file-name "owner-alias" dorg-test-transport--store-root))
           (owner-canon nil)
           (copy-canon nil)
           (port 43101)
           (contacts 0)
           (network-calls 0))
      (make-directory owner t)
      (make-directory copy t)
      (make-symbolic-link owner alias)
      (setq owner-canon (dorg-dolt-server--canonical-dir owner)
            copy-canon (dorg-dolt-server--canonical-dir copy))
      (let ((claim (list (cons :dolt-dir owner-canon)
                         (cons :host (system-name))
                         (cons :owner-pid 31337)
                         (cons :server-pid 31337)
                         (cons :owner-kind 'daemon)
                         (cons :port port)
                         (cons :protocol dorg-dolt-server-claim-protocol))))
        (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                  ((symbol-function 'dorg-dolt-server--claim-read)
                   (lambda (dir)
                     (when (member (dorg-dolt-server--canonical-dir dir)
                                   (list owner-canon copy-canon))
                       claim)))
                  ((symbol-function 'dorg-dolt-server--dolt-server-info) (lambda (&rest _) nil))
                  ((symbol-function 'process-attributes)
                   (lambda (_pid)
                     `((comm . "dolt")
                       (args . ,(format "dolt sql-server --port %d --data-dir %s"
                                        port owner-canon)))))
                  ((symbol-function 'dorg-dolt-server--claim-classify)
                   (lambda (&rest _) (cl-incf contacts) 'live))
                  ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--attach-live)
                   (lambda (&rest _) (cl-incf contacts) port))
                  ((symbol-function 'make-network-process)
                   (lambda (&rest _)
                     (cl-incf network-calls)
                     (error "copied claim reached the network primitive"))))
          ;; Owner and its canonical alias remain eligible.
          (should (= (dorg-dolt-server--strict-port owner) port))
          (clrhash dorg-dolt-server--attached-ports)
          (should (= (dorg-dolt-server--strict-port alias) port))
          (should (= contacts 4))
          (setq contacts 0)
          ;; A copied claim must not reach either discovery/attach endpoint,
          ;; including through the attached-port fast path on a repeat call.
          (dotimes (_ 2)
            (should-error (dorg-dolt-server--strict-port copy)
                          :type 'dorg-sql-transport-error)
            (should (= contacts 0))
            (should (= network-calls 0))))))))

(ert-deftest dorg-test-transport-copied-native-info-refuses-before-attach ()
  "Copied native server metadata never selects its foreign process endpoint.
Unlike the overlay case, synthesized native claims set their :dolt-dir from
the requested path, so only the live process advertised data directory can
distinguish the same-basename copy before attach."
  (dorg-test-transport--with-runtime
    (let* ((owner (dorg-test-transport--repo "owner/.dorg/dorg"))
           (copy (dorg-test-transport--repo "copy/.dorg/dorg"))
           (owner-canon nil)
           (copy-canon nil)
           (port 43102)
           (contacts 0)
           (network-calls 0))
      (make-directory owner t)
      (make-directory copy t)
      (setq owner-canon (dorg-dolt-server--canonical-dir owner)
            copy-canon (dorg-dolt-server--canonical-dir copy))
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-read) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (dir)
                   (when (equal (dorg-dolt-server--canonical-dir dir) copy-canon)
                     (list :pid 31338 :port port :uuid "copied-native"))))
                ;; This is the live foreign process evidence a copied
                ;; sql-server.info cannot forge for the requested copy.
                ((symbol-function 'process-attributes)
                 (lambda (_pid)
                   `((comm . "dolt")
                     (args . ,(format "dolt sql-server --port %d --data-dir %s"
                                      port owner-canon)))))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) (cl-incf contacts) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (&rest _) (cl-incf contacts) port))
                ((symbol-function 'make-network-process)
                 (lambda (&rest _)
                   (cl-incf network-calls)
                   (error "copied native metadata reached the network primitive"))))
        (dotimes (_ 2)
          (should-error (dorg-dolt-server--strict-port copy)
                        :type 'dorg-sql-transport-error)
          (should (= contacts 0))
          (should (= network-calls 0)))))))

(ert-deftest dorg-test-transport-claim-evidence-accepts-real-spaced-data-dir ()
  "An unmocked live process with a spaced explicit data directory is proven.
The helper is a local shell symlink named dolt; it is not Dolt and does not
open a store or a network connection.  This exercises the host's real
process-attributes argv formatting, including spaces in --data-dir."
  (dorg-test-transport--with-runtime
    (let* ((root (dorg-test-transport--repo "store with space/.dorg/dorg"))
           (binary (expand-file-name "dolt" dorg-test-transport--store-root))
           (shell (or (executable-find "sh") "/bin/sh"))
           proc)
      (make-directory root t)
      (make-symbolic-link shell binary)
      (unwind-protect
          (progn
            (setq proc (make-process
                        :name "dorg-test-dolt-evidence"
                        :command (list binary "-c" "while :; do sleep 20; done"
                                       "sql-server" "--data-dir" root)
                        :noquery t))
            (accept-process-output proc 0.05)
            (should (process-live-p proc))
            (should (eq (dorg-dolt-server--claim-store-evidence
                         root
                         (list (cons :server-pid (process-id proc))))
                        'proven)))
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest dorg-test-transport-claim-evidence-decodes-linux-argv ()
  "Linux process attributes escape whitespace and literal backslashes.
A space followed by -- inside the directory is data, not another option."
  (let ((system-type 'gnu/linux))
    (dolist (path '("/tmp/store with space/" "/tmp/store --name/"
                    "/tmp/store\\name/" "/tmp/store\tname/"))
      (let ((encoded (replace-regexp-in-string
                      "[[:space:]\\\\]" (lambda (match) (concat "\\" match))
                      path t t)))
        (dolist (separator '(" " "="))
          (cl-letf (((symbol-function 'process-attributes)
                     (lambda (_)
                       `((comm . "dolt")
                         (args . ,(concat "dolt sql-server --data-dir" separator
                                          encoded " --port 12345"))))))
            (should (eq 'proven
                        (dorg-dolt-server--claim-store-evidence
                         path '((:server-pid . 12345)))))
            (should (eq 'contradictory
                        (dorg-dolt-server--claim-store-evidence
                         "/tmp/different-store/" '((:server-pid . 12345)))))))))))

(ert-deftest dorg-test-transport-dead-pid-evidence-keeps-restart-lifecycle ()
  "A dead server PID remains absent evidence and reaches restart recovery."
  (dorg-test-transport--with-runtime
    (let* ((store (dorg-test-transport--repo "raw/.dorg/dorg"))
           (claim (list (cons :server-pid 31339) (cons :port 43103)))
           (classified 0)
           (broken 0)
           (started 0))
      (make-directory store t)
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) claim))
                ((symbol-function 'process-attributes)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) (cl-incf classified) 'dead))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (cl-incf broken)))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (cl-incf started) 43104)))
        (should (= (dorg-dolt-server--ensure-strict store) 43104))
        (should (= classified 1))
        (should (= broken 1))
        (should (= started 1))))))

(ert-deftest dorg-test-transport-raw-dolt-without-flags-still-attaches ()
  "Raw dolt sql-server lacks evidence but remains attachable by lifecycle."
  (dorg-test-transport--with-runtime
    (let* ((store (dorg-test-transport--repo "raw/.dorg/dorg"))
           (claim (list (cons :server-pid 31342) (cons :port 43107)))
           (attached 0))
      (make-directory store t)
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) claim))
                ((symbol-function 'process-attributes)
                 (lambda (&rest _) '((comm . "dolt") (args . "dolt sql-server"))))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (&rest _) (cl-incf attached) 43107)))
        (should (= (dorg-dolt-server--strict-port store) 43107))
        (should (= attached 1))))))

(ert-deftest dorg-test-transport-nil-server-pid-still-attaches ()
  "A normal refresh with no server PID remains absent evidence, not refusal."
  (dorg-test-transport--with-runtime
    (let* ((store (dorg-test-transport--repo "owner/.dorg/dorg"))
           (claim (list (cons :port 43108)))
           (attached 0))
      (make-directory store t)
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) claim))
                ((symbol-function 'process-attributes)
                 (lambda (&rest _)
                   (error "nil server-pid must not query process attributes")))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (&rest _) (cl-incf attached) 43108)))
        (should (= (dorg-dolt-server--strict-port store) 43108))
        (should (= attached 1))))))

(ert-deftest dorg-test-transport-effective-claim-keeps-contradiction-readable ()
  "A copied-claim contradiction is inspectable until an attach is selected."
  (dorg-test-transport--with-runtime
    (let* ((store (dorg-test-transport--repo "copy/.dorg/dorg"))
           (foreign (dorg-test-transport--repo "owner/.dorg/dorg"))
           (claim (list (cons :server-pid 31340) (cons :port 43105))))
      (make-directory store t)
      (make-directory foreign t)
      (cl-letf (((symbol-function 'dorg-dolt-server--claim-read)
                 (lambda (&rest _) claim))
                ((symbol-function 'dorg-dolt-server--dolt-server-info) (lambda (&rest _) nil))
                ((symbol-function 'process-attributes)
                 (lambda (&rest _)
                   '((comm . "dolt")
                     (args . "dolt sql-server --data-dir /definitely/foreign")))))
        (should (eq (dorg-dolt-server--effective-claim store) claim))))))

(ert-deftest dorg-test-transport-publish-refresh-preserves-server-pid ()
  "Refreshing an owner claim without a PID retains its live server identity."
  (dorg-test-transport--with-runtime
    (let ((store (dorg-test-transport--repo "owner/.dorg/dorg")))
      (make-directory store t)
      (dorg-dolt-server--publish-claim store 43106 31341)
      (dorg-dolt-server--publish-claim store 43106)
      (should (= (alist-get :server-pid
                            (dorg-dolt-server--claim-read store))
                 31341)))))

;;; ─── Transport Selector Taxonomy ────────────────────────────────────────

(ert-deftest dorg-test-transport-policy-collapsed-deletes-auto-cli-symbols ()
  "The public transport policy surface has collapsed to server-only.
These symbols belong to the retired public auto/CLI fallback machinery and
must not remain callable after tracked-case."
  (dolist (name (list (concat "dorg-sql--resolve-" "auto")
                      (concat "dorg-sql--resolve-" "cli")
                      (concat "dorg-sql-transport-" "diagnostic")
                      (concat "dorg-sql--log-auto-" "cli-fallback")
                      (concat "dorg-sql--fallback-" "diagnostic-context")
                      (concat "dorg-sql-batch" "--cli")))
    (should-not (fboundp (intern name))))
  (dolist (name (list (concat "dorg-sql-fallback-" "log-interval")
                      (concat "dorg-sql--fallback-" "log-times")
                      (concat "dorg-sql--fallback-" "log-count")
                      (concat "dorg-sql--last-fallback-" "diagnostic")))
    (should-not (boundp (intern name)))))

(ert-deftest dorg-test-transport-default-is-strict-server-everywhere ()
  "The packaged transport default is strict server in every entry point."
  (should (eq (let ((noninteractive nil)) (dorg-sql--default-transport)) 'server))
  (should (eq (let ((noninteractive t)) (dorg-sql--default-transport)) 'server))
  (should (eq (default-value 'dorg-sql-transport) 'server)))

(ert-deftest dorg-test-transport-public-auto-policy-rejected ()
  "Public `auto' is no longer a SQL transport policy."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'auto))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil)))
        (should-error (dorg-sql--resolve-transport (dorg-test-transport--repo "fake"))
                      :type 'dorg-sql-transport-error)))))

(ert-deftest dorg-test-transport-server-no-server-autostart-enabled-uses-daemon ()
  "Strict `server' with no visible server autostarts a detached daemon
when `dorg-dolt-server-autostart' is enabled.
This preserves the v0.1 batch/one-shot autostart policy after deleting
public `auto' resolution: no compatible managed server visible means
ensure a durable daemon, not a per-invocation Emacs-child server or CLI."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'server)
          (dorg-dolt-server-autostart t)
          (detached-calls 0)
          (strict-calls 0))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (cl-incf strict-calls) 65000))
                ((symbol-function 'dorg-dolt-server--ensure-detached)
                 (lambda (&rest _) (cl-incf detached-calls) 64001)))
        (should (equal (dorg-sql--resolve-transport (dorg-test-transport--repo "fake"))
                       '(mysql . 64001)))
        (should (= detached-calls 1))
        (should (= strict-calls 0))))))

(ert-deftest dorg-test-transport-server-no-server-autostart-disabled-errors ()
  "Strict `server' with no visible server and autostart disabled (the
default) signals `dorg-sql-transport-error' and starts no daemon and no
session-scoped server.  Non-vacuous: both `dorg-dolt-server--start-detached-unsafe'
and `dorg-dolt-server--start-unsafe' are failing sentinels, so a
regression that starts a fresh daemon OR routes a no-claim/non-daemon-claim
case into the strict session-server path (the exact rejected-pass bug:
`dorg-dolt-server--strict-port' delegating a non-daemon claim to
`dorg-dolt-server--ensure-strict' unconditionally) fails this test rather
than passing vacuously.  The error text must name the exact remedy
\(`dorg dolt start PATH') and the exact option
\(`dorg-dolt-server-autostart')."
  (dorg-test-transport--with-runtime
    (let* ((dorg-sql-transport 'server)
           (dolt-dir (dorg-test-transport--repo "fake")))
      (should-not dorg-dolt-server-autostart)
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon when autostart is disabled")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server when autostart is disabled"))))
        (let ((err (should-error (dorg-sql--resolve-transport dolt-dir)
                                 :type 'dorg-sql-transport-error)))
          (should (string-match-p
                   (regexp-quote (format "dorg dolt start %s" dolt-dir))
                   (cadr err)))
          (should (string-match-p "dorg-dolt-server-autostart" (cadr err))))))))

(ert-deftest dorg-test-transport-server-sql-no-server-never-cli ()
  "Strict `server' SQL on a no-server repo autostarts detached and uses mysql.
Non-vacuous end-to-end selector pin: `dorg-sql--cli-exec' is a failing
sentinel, while mysql connection/execution must observe the detached
daemon port returned by `dorg-dolt-server--ensure-detached'."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'server)
          (dorg-dolt-server-autostart t)
          (detached-calls 0)
          (mysql-ports nil)
          (mysql-calls 0)
          (cli-called nil))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-detached)
                 (lambda (&rest _) (cl-incf detached-calls) 64002))
                ;; get-connection resolves strict-port lazily (only on a cache
                ;; miss); dorg-sql no longer resolves it up front (tracked-case A1).
                ((symbol-function 'dorg-sql--get-connection)
                 (lambda (dir &optional port)
                   (push (or port (dorg-dolt-server--strict-port dir)) mysql-ports)
                   'conn))
                ((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (&rest _) (cl-incf mysql-calls) '((ok . 1))))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-called t) (error "one-shot CLI used"))))
        (should (equal (dorg-sql (dorg-test-transport--repo "server-sql") "SELECT 1")
                       '((ok . 1))))
        (should (= detached-calls 1))
        (should (equal mysql-ports '(64002)))
        (should (= mysql-calls 1))
        (should-not cli-called)))))

(ert-deftest dorg-test-transport-server-autostart-publishes-daemon-claim ()
  "Strict `server' autostart uses the durable detached-daemon claim lifecycle.
This locks the v0.1 manual lifecycle policy: the no-server path delegates
to `dorg-dolt-server--ensure-detached', whose fresh-start branch publishes a
`:owner-kind daemon' claim with the daemon pid as owner.  A session-owned
claim would be reaped with the launcher and fail cross-process manual stop."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'server)
          (dorg-dolt-server-autostart t)
          (daemon-pid 65005)
          (port 64005)
          (starts 0)
          (dir (dorg-test-transport--repo "daemon-claim")))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (dolt-dir)
                   (cl-incf starts)
                   (dorg-dolt-server--publish-claim
                    dolt-dir port daemon-pid nil nil daemon-pid 'daemon)
                   port)))
        (should (equal (dorg-sql--resolve-transport dir) (cons 'mysql port)))
        (should (= starts 1))
        (let ((claim (dorg-dolt-server--claim-read dir)))
          (should claim)
          (should (eq (alist-get :owner-kind claim) 'daemon))
          (should (= (alist-get :owner-pid claim) daemon-pid))
          (should (= (alist-get :server-pid claim) daemon-pid))
          (should (= (alist-get :port claim) port)))))))

(ert-deftest dorg-test-transport-cli-seam-batch-survives-batch-cli-delete ()
  "The internal CLI seam still supports batch SQL without a batch CLI helper.
The seam pins `(cli . nil)' and the combined batch plus rollback routes
through `dorg-sql', which reaches the retained one-shot primitive."
  (should-not (fboundp (intern (concat "dorg-sql-batch" "--cli"))))
  (let ((seen nil)
        (rollback-transport nil)
        (dorg-sql-transport 'server))
    (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql)
               (lambda (dir query &rest _)
                 (push query seen)
                 (when (string= query "ROLLBACK")
                   (setq rollback-transport (dorg-sql--resolve-transport dir)))
                 (unless (string= query "ROLLBACK")
                   (error "boom")))))
      (dorg-sql--with-cli-seam (dorg-test-transport--repo "fake")
        (should-error
         (dorg-sql-batch (dorg-test-transport--repo "fake")
                         (list "INSERT INTO t VALUES (1)")
                         "msg")
         :type 'error))
      (should (member "ROLLBACK" seen))
      (should (equal rollback-transport '(cli . nil))))))

(ert-deftest dorg-test-transport-cli-seam-resolves-cli-without-claim ()
  "The internal CLI seam resolves to CLI and never consults a server."
  (dorg-test-transport--with-runtime
    (dorg-sql--with-cli-seam (dorg-test-transport--repo "fake")
      (should (equal (dorg-sql--resolve-transport (dorg-test-transport--repo "fake")) '(cli))))))

(ert-deftest dorg-test-transport-public-cli-policy-rejected ()
  "Public `dorg-sql-transport' no longer accepts one-shot CLI policy.
One-shot CLI remains an internal test/recovery seam, not an operator
transport.  Non-vacuous: the resolver must signal before it reaches any
server/claim checks."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'cli))
      (should-error (dorg-sql--resolve-transport (dorg-test-transport--repo "fake"))
                    :type 'dorg-sql-transport-error))))

;;; ─── Strict server mode never falls back to CLI (non-vacuous) ───────────

(ert-deftest dorg-test-transport-strict-sql-never-uses-cli ()
  "Strict `server' `dorg-sql' routes through mysql and never calls CLI.
Non-vacuous: `dorg-sql--cli-exec' is replaced with a sentinel that fails
the test if reached, and the mysql exec is proven to run."
  (let ((mysql-calls 0) (cli-called nil))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 5000))
                ((symbol-function 'dorg-sql--get-connection) (lambda (&rest _) 'conn))
                ((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (&rest _) (cl-incf mysql-calls) nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-called t) (error "CLI fallback used in strict mode"))))
        (dorg-sql (dorg-test-transport--repo "fake") "SELECT 1")
        (should (= mysql-calls 1))
        (should-not cli-called)))))

(ert-deftest dorg-test-transport-strict-batch-never-uses-cli ()
  "Strict `server' `dorg-sql-batch' routes through the mysql batch path.
Endpoint ownership (tracked-case): the batch runs on the warm connection via
`dorg-sql--mysql-batch-on-conn', never the internal CLI seam."
  (let ((mysql-batch nil) (cli-called nil))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 5000))
                ((symbol-function 'dorg-sql--get-connection) (lambda (&rest _) 'conn))
                ((symbol-function 'dorg-sql--mysql-batch-on-conn)
                 (lambda (&rest _) (setq mysql-batch t) nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-called t) nil)))
        (dorg-sql-batch (dorg-test-transport--repo "fake") (list "INSERT INTO t VALUES (1)") "msg")
        (should mysql-batch)
        (should-not cli-called)))))

;;; ─── Public cli is rejected; internal CLI seam stays explicit ───────────

(ert-deftest dorg-test-transport-public-cli-rejected-before-claim-check ()
  "Public `cli' transport signals a transport error, not claim conflict.
The one-shot CLI executor must never run in that case."
  (let ((cli-called nil))
    (let ((dorg-sql-transport 'cli))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-read)
                 (lambda (&rest _) '((:owner-pid . 4242) (:port . 6000) (:policy . server))))
                ((symbol-function 'dorg-dolt-server--claim-classify) (lambda (&rest _) 'live))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-called t) nil)))
        (should-error (dorg-sql (dorg-test-transport--repo "fake") "SELECT 1")
                      :type 'dorg-sql-transport-error)
        (should-not cli-called)))))

(ert-deftest dorg-test-transport-cli-seam-conflict-check-can-be-disabled ()
  "`dorg-sql--with-cli-seam' lets CLI run despite a claim.
This is the internal recovery/test escape hatch."
  (let ((cli-called nil))
    (dorg-sql--with-cli-seam (dorg-test-transport--repo "fake")
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-read)
                 (lambda (&rest _) '((:owner-pid . 4242) (:port . 6000))))
                ((symbol-function 'dorg-dolt-server--claim-classify) (lambda (&rest _) 'live))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-called t) nil)))
        (dorg-sql (dorg-test-transport--repo "fake") "SELECT 1")
        (should cli-called)))))

(defun dorg-test-transport--write-dolt-info (store pid port uuid)
  "Write a Dolt-native `.dolt/sql-server.info' (PID:PORT:UUID) under STORE."
  (let ((dir (expand-file-name ".dolt" store)))
    (make-directory dir t)
    (with-temp-file (expand-file-name "sql-server.info" dir)
      (insert (format "%d:%d:%s" pid port uuid)))))

(defmacro dorg-test-transport--with-fake-cli-process (&rest body)
  "Run BODY with `dorg-sql--cli-exec' process primitives faked.
The lexical variables SPAWNED and KILLED record the exact fake process
object passed to `make-process' and `dorg-dolt-server--kill-process'."
  (declare (indent 0))
  `(let ((spawned nil)
         (killed nil)
         (fake-live nil)
         (native-comp-jit-compilation nil)
         (native-comp-enable-subr-trampolines nil)
         (orig-live (symbol-function 'process-live-p))
         (orig-exit (symbol-function 'process-exit-status)))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq spawned 'dorg-test-sql-cli-proc
                        fake-live t)
                  (let ((buf (plist-get args :buffer)))
                    (when (bufferp buf)
                      (with-current-buffer buf (insert "{\"rows\":[]}"))))
                  spawned))
               ((symbol-function 'process-send-string) (lambda (&rest _) nil))
               ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
               ((symbol-function 'accept-process-output)
                (lambda (proc &rest _)
                  (when (eq proc spawned)
                    (setq fake-live nil))
                  nil))
               ((symbol-function 'process-live-p)
                (lambda (proc)
                  (if (eq proc spawned)
                      fake-live
                    (funcall orig-live proc))))
               ((symbol-function 'process-exit-status)
                (lambda (proc)
                  (if (eq proc spawned) 0 (funcall orig-exit proc))))
               ((symbol-function 'dorg-dolt-server--kill-process)
                (lambda (proc &rest _)
                  (setq killed proc
                        fake-live nil))))
       ,@body)))

;;; ─── Strict batch rollback stays on mysql ───────────────────────────────

(ert-deftest dorg-test-transport-strict-rollback-stays-on-mysql ()
  "A failed strict-server batch rolls back via mysql, never via CLI.
The mysql connection issues BEGIN then ROLLBACK and the original error
is re-signaled; one-shot CLI is never reached."
  (let ((queries nil) (cli-used nil))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 5000))
                ((symbol-function 'dorg-sql--get-connection) (lambda (&rest _) 'conn))
                ((symbol-function 'mysql-query)
                 (lambda (_conn q)
                   (push q queries)
                   (when (string-match-p "POISON" q) (error "boom"))
                   nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (setq cli-used t) nil)))
        (should-error
         (dorg-sql-batch (dorg-test-transport--repo "fake")
                         (list "INSERT INTO t VALUES (1)" "POISON STATEMENT")
                         "commit msg")
         :type 'error)
        (should (member "BEGIN" queries))
        (should (member "ROLLBACK" queries))
        (should-not cli-used)))))

;;; ─── dorg-commit policy gate ─────────────────────────────────────────────

(ert-deftest dorg-test-transport-commit-server-batch-ensures-detached-daemon ()
  "Strict `server' `dorg-commit' in batch ensures a detached daemon.
After public `auto' resolution is deleted, server-only one-shot work still
keeps 24zly's durable daemon autostart policy.  Non-vacuous: strict/session
ensures are sentinels and the detached ensure must be called before commit SQL."
  (let ((strict-calls 0) (detached-calls 0)
        (dorg-dolt-server-autostart t))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (_d) (cl-incf strict-calls) 5000))
                ((symbol-function 'dorg-dolt-server--ensure-detached)
                 (lambda (_d) (cl-incf detached-calls) 64003))
                ((symbol-function 'dorg-sql) (lambda (&rest _) nil)))
        (dorg-commit (dorg-test-transport--repo "fake") "msg")
        (should (= strict-calls 0))
        (should (= detached-calls 1))))))

(ert-deftest dorg-test-transport-commit-public-cli-rejected ()
  "Public `cli' `dorg-commit' signals before touching a server."
  (let ((strict-calls 0))
    (let ((dorg-sql-transport 'cli))
      (cl-letf (((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (_d) (cl-incf strict-calls) 5000))
                ((symbol-function 'dorg-sql) (lambda (&rest _) nil)))
        (should-error (dorg-commit (dorg-test-transport--repo "fake") "msg")
                      :type 'dorg-sql-transport-error)
        (should (= strict-calls 0))))))

(ert-deftest dorg-test-transport-commit-cli-seam-skips-detached-autostart ()
  "The internal CLI seam remains authoritative for `dorg-commit'.
Setup/recovery code running inside `dorg-sql--with-cli-seam' must not
leave a daemon lock behind before server-lifecycle tests explicitly start
their own server."
  (let ((strict-calls 0)
        (detached-calls 0)
        (queries nil)
        (dir (dorg-test-transport--repo "fake")))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (_d) (cl-incf strict-calls) 5000))
                ((symbol-function 'dorg-dolt-server--ensure-detached)
                 (lambda (_d) (cl-incf detached-calls) 64003))
                ((symbol-function 'dorg-sql)
                 (lambda (_d query &rest _)
                   (push query queries)
                   nil)))
        (dorg-sql--with-cli-seam dir
          (dorg-commit dir "msg"))
        (should (= strict-calls 0))
        (should (= detached-calls 0))
        (should (member "CALL DOLT_ADD('.')" queries))))))

;;; ─── Claim persistence ──────────────────────────────────────────────────

(ert-deftest dorg-test-transport-claim-write-read-roundtrip ()
  "A published claim records owner pid/port/token/protocol and reads back."
  (dorg-test-transport--with-runtime
    (dorg-dolt-server--publish-claim (dorg-test-transport--repo "claim-a") 5432 12345)
    (let ((c (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-a"))))
      (should c)
      (should (= (alist-get :port c) 5432))
      (should (= (alist-get :owner-pid c) (emacs-pid)))
      (should (= (alist-get :server-pid c) 12345))
      (should (stringp (alist-get :token c)))
      (should (eq (alist-get :policy c) 'server))
      (should (equal (alist-get :protocol c) dorg-dolt-server-claim-protocol))
      (should (equal (alist-get :host c) (system-name))))))

(ert-deftest dorg-test-transport-claim-release-is-token-checked ()
  "`release-claim' deletes only a claim whose token this process owns.
A claim rewritten by another process (different token) survives release,
and a matching-token claim is removed."
  (dorg-test-transport--with-runtime
    (dorg-dolt-server--publish-claim (dorg-test-transport--repo "claim-b") 5432)
    ;; Simulate another process rewriting the claim with a new token.
    (let ((c (copy-alist (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-b")))))
      (setf (alist-get :token c) "FOREIGN-TOKEN")
      (dorg-dolt-server--claim-write (dorg-test-transport--repo "claim-b") c))
    (dorg-dolt-server--release-claim (dorg-test-transport--repo "claim-b"))
    (should (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-b"))) ; foreign claim kept
    ;; Re-publish (reuses our owner token) then release: now it matches.
    (dorg-dolt-server--publish-claim (dorg-test-transport--repo "claim-b") 5432)
    (dorg-dolt-server--release-claim (dorg-test-transport--repo "claim-b"))
    (should-not (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-b")))))

(ert-deftest dorg-test-transport-break-claim-removes-file ()
  "`break-claim' removes the claim file and forgets owned/attached state."
  (dorg-test-transport--with-runtime
    (dorg-dolt-server--publish-claim (dorg-test-transport--repo "claim-c") 5432)
    (should (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-c")))
    (dorg-dolt-server--break-claim (dorg-test-transport--repo "claim-c"))
    (should-not (dorg-dolt-server--claim-read (dorg-test-transport--repo "claim-c")))
    (should-not (gethash (dorg-dolt-server--canonical-dir (dorg-test-transport--repo "claim-c"))
                         dorg-dolt-server--owned-claims))))

(ert-deftest dorg-test-transport-attach-only-client-keeps-owner-claim ()
  "An attach-only client's stop closes its state but keeps the owner claim.
`dorg-dolt-server--stop' on a directory this process did not start (no managed
process, no owned token) must not delete the owner's claim file."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "claim-d"))
           (canon (dorg-dolt-server--canonical-dir dir)))
      (dorg-dolt-server--claim-write
       dir (list (cons :dolt-dir canon) (cons :host (system-name))
                 (cons :owner-pid 4242) (cons :token "OWNER-TOKEN")
                 (cons :port 9001) (cons :policy 'server)
                 (cons :protocol dorg-dolt-server-claim-protocol)
                 (cons :created-at (float-time)) (cons :last-used (float-time))))
      (puthash canon 9001 dorg-dolt-server--attached-ports)
      (cl-letf (((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil)))
        (dorg-dolt-server--stop dir))
      (should (dorg-dolt-server--claim-read dir))         ; owner claim survives
      (should-not (gethash canon dorg-dolt-server--attached-ports))))) ; our state cleared

(ert-deftest dorg-test-transport-stop-all-scrubs-vanished-store-registries ()
  "`stop-all' removes no-proc registry entries whose canonical dir vanished."
  (dorg-test-transport--with-runtime
    (let* ((dorg-dolt-server--procs (make-hash-table :test 'equal))
           (live-dir (dorg-test-transport--repo "live-store"))
           (stale-dir (dorg-test-transport--repo "stale-store")))
      (make-directory live-dir t)
      (make-directory stale-dir t)
      (let ((live-canon (dorg-dolt-server--canonical-dir live-dir))
            (stale-canon (dorg-dolt-server--canonical-dir stale-dir)))
        (puthash live-canon "live-token" dorg-dolt-server--owned-claims)
        (puthash stale-canon "stale-token" dorg-dolt-server--owned-claims)
        (puthash live-canon 9001 dorg-dolt-server--attached-ports)
        (puthash stale-canon 9002 dorg-dolt-server--attached-ports)
        (delete-directory stale-dir t)
        (should (gethash stale-canon dorg-dolt-server--owned-claims))
        (should (gethash stale-canon dorg-dolt-server--attached-ports))
        (cl-letf (((symbol-function 'dorg-sql--close-all-connections)
                   (lambda () nil)))
          (dorg-dolt-server-stop-all))
        (should (equal (gethash live-canon dorg-dolt-server--owned-claims)
                       "live-token"))
        (should (= (gethash live-canon dorg-dolt-server--attached-ports)
                   9001))
        (should-not (gethash stale-canon dorg-dolt-server--owned-claims))
        (should-not (gethash stale-canon dorg-dolt-server--attached-ports))))))

;;; ─── Claim liveness classification ──────────────────────────────────────

(ert-deftest dorg-test-transport-classify-dead-when-pid-gone ()
  "A same-host claim with an absent owner pid and a dead port is `dead'."
  (let ((claim (list (cons :host (system-name))
                     (cons :owner-pid 999999)   ; not a live process
                     (cons :port (dorg-test-transport--closed-port)))))
    (should (eq (dorg-dolt-server--claim-classify claim) 'dead))))

(ert-deftest dorg-test-transport-classify-live-when-port-answers ()
  "A claim whose port answers is `live' even regardless of pid checks."
  (let* ((live (dorg-test-transport--make-live-port))
         (proc (car live))
         (claim (list (cons :host (system-name))
                      (cons :owner-pid (emacs-pid))
                      (cons :port (cdr live)))))
    (unwind-protect
        (should (eq (dorg-dolt-server--claim-classify claim) 'live))
      (delete-process proc))))

(ert-deftest dorg-test-transport-idle-stale-verdict-removed ()
  "Claim age cannot authorize breaking a live or unknowable owner."
  (should-not (boundp 'dorg-dolt-server-claim-idle-timeout))
  (let ((claim (list (cons :host (system-name))
                     (cons :owner-pid (emacs-pid))
                     (cons :port (dorg-test-transport--closed-port))
                     (cons :last-used (- (float-time) 100)))))
    (should (eq (dorg-dolt-server--claim-classify claim) 'unreachable))))

(ert-deftest dorg-test-transport-classify-unreachable-live-owner ()
  "A live-owner claim with a dead port is `unreachable'."
  (let ((claim (list (cons :host (system-name))
                     (cons :owner-pid (emacs-pid))   ; live pid
                     (cons :port (dorg-test-transport--closed-port))
                     (cons :last-used (float-time)))))
    (should (eq (dorg-dolt-server--claim-classify claim) 'unreachable))))

(ert-deftest dorg-test-transport-classify-remote-claim-not-dead ()
  "A claim from another host with a dead port is never declared `dead'."
  (let ((claim (list (cons :host "some-other-host.invalid")
                     (cons :owner-pid 999999)
                     (cons :port (dorg-test-transport--closed-port))
                     (cons :last-used (float-time)))))
    (should (eq (dorg-dolt-server--claim-classify claim) 'unreachable))))

(ert-deftest dorg-test-transport-compatible-requires-protocol ()
  "`claim-compatible-p' attaches only on a matching claim protocol.
mysql.el loadability is not re-checked — it is a HARD dependency present in
every loaded dorg process (tracked-case collapsed the always-true conjunct)."
  (should (dorg-dolt-server--claim-compatible-p
           (list (cons :protocol dorg-dolt-server-claim-protocol))))
  (should-not (dorg-dolt-server--claim-compatible-p
               (list (cons :protocol (1+ dorg-dolt-server-claim-protocol))))))

;;; ─── Dead connection retry once (failure matrix) ────────────────────────

(ert-deftest dorg-test-transport-get-connection-retries-dead-once ()
  "`dorg-sql--get-connection' drops a dead connection and reconnects once."
  (let ((dorg-sql--connections (make-hash-table :test 'equal))
        (ping-calls 0) (connect-calls 0)
        (repo (dorg-test-transport--repo "fake")))
    (let ((key (dorg-dolt-server--canonical-dir repo)))
      ;; A bound warm entry (endpoint ownership): the pool stores a
      ;; `dorg-sql--conn-entry' (conn + endpoint), not a bare connection.
      (puthash key (dorg-sql--make-conn-entry :conn 'old-conn :port 3307
                                              :identity (list :db "db"))
               dorg-sql--connections)
      (cl-letf (((symbol-function 'mysql-ping)
                 (lambda (_c) (cl-incf ping-calls) (error "dead")))
                ((symbol-function 'mysql-disconnect) (lambda (_c) nil))
                ((symbol-function 'dorg-dolt-server--db-name) (lambda (_d) "db"))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf connect-calls) 'new-conn)))
        (should (eq (dorg-sql--get-connection repo 3307) 'new-conn))
        (should (= ping-calls 1))
        (should (= connect-calls 1))
        (should (eq (dorg-sql--conn-entry-conn
                     (gethash key dorg-sql--connections))
                    'new-conn))))))

;;; ─── Integration: strict server query/commit end-to-end ─────────────────

(ert-deftest dorg-test-transport-strict-server-query-integration ()
  "Strict `server' mode performs a real query through a managed server.
Requires dolt and mysql.el; skips otherwise (documented skip).  Enables
`dorg-dolt-server-autostart' since this repo has no server yet and the
default is disabled."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — strict server path needs it")))
  (dorg-test-transport--with-runtime
    (let ((temp-base (dorg-test--make-owned-temp-dir
                      "dorg-strict-" "transport-strict-query")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base))
                (dorg-sql-transport 'server)
                (dorg-dolt-server-autostart t))
            (unwind-protect
                (progn
                  (dorg-sql-batch dolt-dir
                                  (list "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, 'Root')")
                                  "strict insert")
                  (let ((rows (dorg-sql dolt-dir "SELECT id, title FROM headings WHERE id = 1")))
                    (should (= (length rows) 1))
                    (should (equal (alist-get 'title (car rows)) "Root")))
                  ;; A claim now exists for this repo.
                  (should (dorg-dolt-server-claim dolt-dir)))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-transport-strict-stop-leaves-no-orphan-claim ()
  "After a strict server stop, no ownership claim remains for the repo.
Enables `dorg-dolt-server-autostart' since this repo has no server yet and
the default is disabled."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — strict server path needs it")))
  (dorg-test-transport--with-runtime
    (let ((temp-base (dorg-test--make-owned-temp-dir
                      "dorg-strict-" "transport-strict-stop")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base))
                (dorg-sql-transport 'server)
                (dorg-dolt-server-autostart t))
            (dorg-sql dolt-dir "SELECT 1 AS n")
            (should (dorg-dolt-server-claim dolt-dir))
            (dorg-dolt-server--stop dolt-dir)
            (should-not (dorg-dolt-server-claim dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

;;; ─── Claim lock serialization (never runs BODY unlocked) ────────────────

(ert-deftest dorg-test-transport-claim-lock-runs-and-releases ()
  "`with-claim-lock' holds the lock during BODY and releases it after."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "lock-ok"))
           (lock (concat (dorg-dolt-server--claim-file dir) ".lock"))
           (body-ran nil))
      (dorg-dolt-server--with-claim-lock dir
        (setq body-ran t)
        (should (file-directory-p lock)))  ; held during BODY
      (should body-ran)
      (should-not (file-directory-p lock)))))  ; released after BODY

(ert-deftest dorg-test-transport-claim-lock-refuses-live-holder ()
  "A live holder makes `with-claim-lock' signal — BODY must never run.
This is the blocker-1 contract: after the bounded wait the critical
section is not entered unlocked behind a live holder."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "lock-live"))
           (lock (concat (dorg-dolt-server--claim-file dir) ".lock"))
           (body-ran nil)
           (dorg-dolt-server-claim-lock-timeout 0.2)
           (dorg-dolt-server-claim-lock-stale-after 600))
      (make-directory (file-name-directory (directory-file-name lock)) t)
      (make-directory lock)
      (dorg-dolt-server--lock-write-meta lock)
      (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_pid) t)))
        (should-error
         (dorg-dolt-server--with-claim-lock dir (setq body-ran t))
         :type 'dorg-sql-transport-error)
        (should-not body-ran)
        (should (file-directory-p lock))))))  ; live holder's lock untouched

(ert-deftest dorg-test-transport-claim-lock-reaps-dead-holder ()
  "A same-host holder whose pid is gone is reaped and BODY runs."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "lock-dead"))
           (lock (concat (dorg-dolt-server--claim-file dir) ".lock"))
           (body-ran nil)
           (dorg-dolt-server-claim-lock-timeout 0.2)
           (dorg-dolt-server-claim-lock-stale-after 600))
      (make-directory (file-name-directory (directory-file-name lock)) t)
      (make-directory lock)
      (with-temp-file (dorg-dolt-server--lock-meta-file lock)
        (prin1 (list (cons :host (system-name)) (cons :pid 999999)
                     (cons :acquired-at (float-time)))
               (current-buffer)))
      (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_pid) nil)))
        (dorg-dolt-server--with-claim-lock dir (setq body-ran t)))
      (should body-ran)
      (should-not (file-directory-p lock)))))  ; reaped, then released

(ert-deftest dorg-test-transport-claim-lock-reaps-aged-holder ()
  "An abandoned lock older than the stale threshold is reaped and BODY runs.
Covers a crashed holder whose pid cannot be proven dead (remote/unknown)."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "lock-aged"))
           (lock (concat (dorg-dolt-server--claim-file dir) ".lock"))
           (body-ran nil)
           (dorg-dolt-server-claim-lock-timeout 0.2)
           (dorg-dolt-server-claim-lock-stale-after 1.0))
      (make-directory (file-name-directory (directory-file-name lock)) t)
      (make-directory lock)
      (with-temp-file (dorg-dolt-server--lock-meta-file lock)
        (prin1 (list (cons :host "other-host") (cons :pid 12345)
                     (cons :acquired-at (- (float-time) 100)))
               (current-buffer)))
      (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_pid) 'unknown)))
        (dorg-dolt-server--with-claim-lock dir (setq body-ran t)))
      (should body-ran))))

;;; ─── Legacy public start/ensure honor the claim protocol ────────────────

(ert-deftest dorg-test-transport-server-start-refuses-live-foreign-claim ()
  "`dorg-dolt-server--start' refuses to overwrite a live foreign owner's claim.
Blocker-2 contract: the legacy public start path must not start a second
server (and overwrite the claim token/port) behind a live owner."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "start-foreign"))
           (canon (dorg-dolt-server--canonical-dir dir))
           (started nil))
      (dorg-dolt-server--claim-write
       dir (list (cons :dolt-dir canon) (cons :host (system-name))
                 (cons :owner-pid 4242) (cons :token "FOREIGN")
                 (cons :port 9001) (cons :policy 'server)
                 (cons :protocol dorg-dolt-server-claim-protocol)
                 (cons :created-at (float-time)) (cons :last-used (float-time))))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify) (lambda (_c) 'live))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) 5555)))
        (should-error (dorg-dolt-server--start dir) :type 'dorg-sql-transport-conflict)
        (should-not started)
        (should (equal (alist-get :token (dorg-dolt-server--claim-read dir))
                       "FOREIGN"))))))  ; foreign claim untouched

(ert-deftest dorg-test-transport-server-ensure-attaches-live-foreign-claim ()
  "`dorg-dolt-server--ensure-strict' attaches to a live foreign claim, never starts.
Covers the `dorg-mode' auto-server / `dorg-sql-connect' path, now consolidated
on the canonical strict session selector."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "ensure-foreign"))
           (canon (dorg-dolt-server--canonical-dir dir))
           (started nil))
      (dorg-dolt-server--claim-write
       dir (list (cons :dolt-dir canon) (cons :host (system-name))
                 (cons :owner-pid 4242) (cons :token "FOREIGN")
                 (cons :port 9002) (cons :policy 'server)
                 (cons :protocol dorg-dolt-server-claim-protocol)
                 (cons :created-at (float-time)) (cons :last-used (float-time))))
      ;; Deep classify takes two args on the strict cache-miss path.
      (cl-letf (((symbol-function 'dorg-dolt-server--claim-classify) (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (&rest _) t))
                ;; The live foreign owner serves a valid store, so strict
                ;; attaches rather than breaking the claim and starting over.
                ((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) 5555)))
        (should (= (dorg-dolt-server--ensure-strict dir) 9002))
        (should-not started)
        (should (= (gethash canon dorg-dolt-server--attached-ports) 9002))))))

(ert-deftest dorg-test-transport-server-ensure-replaces-dead-foreign-claim ()
  "`dorg-dolt-server--ensure-strict' reaps a dead foreign claim and starts a server."
  (dorg-test-transport--with-runtime
    (let* ((dir (dorg-test-transport--repo "ensure-dead"))
           (canon (dorg-dolt-server--canonical-dir dir))
           (started nil))
      (dorg-dolt-server--claim-write
       dir (list (cons :dolt-dir canon) (cons :host (system-name))
                 (cons :owner-pid 4242) (cons :token "FOREIGN")
                 (cons :port 9003) (cons :policy 'server)
                 (cons :protocol dorg-dolt-server-claim-protocol)
                 (cons :created-at (float-time)) (cons :last-used (float-time))))
      ;; Deep classify takes two args on the strict cache-miss path.
      (cl-letf (((symbol-function 'dorg-dolt-server--claim-classify) (lambda (&rest _) 'dead))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) 5555)))
        (should (= (dorg-dolt-server--ensure-strict dir) 5555))
        (should started)
        (should-not (dorg-dolt-server--claim-read dir))))))  ; dead claim broken

;;; ─── Strict rollback reports rollback-failure context ───────────────────

(ert-deftest dorg-test-transport-strict-rollback-reports-failure-context ()
  "When a strict-mode ROLLBACK also fails, both contexts are reported.
Blocker-3 contract: the original failure is preserved (symbol + message)
and the rollback failure is appended, never hiding the original."
  (let ((queries nil))
    (let ((dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 5000))
                ((symbol-function 'dorg-sql--get-connection) (lambda (&rest _) 'conn))
                ((symbol-function 'mysql-query)
                 (lambda (_conn q)
                   (push q queries)
                   (cond ((string-match-p "POISON" q) (error "original failed"))
                         ((string-match-p "ROLLBACK" q) (error "rollback failed")))
                   nil)))
        (let ((err (should-error
                    (dorg-sql-batch (dorg-test-transport--repo "fake")
                                    (list "POISON STATEMENT") "commit msg")
                    :type 'error)))
          (should (member "BEGIN" queries))
          (should (member "ROLLBACK" queries))
          (should (string-match-p "original failed" (error-message-string err)))
          (should (string-match-p "ROLLBACK ALSO FAILED"
                                  (error-message-string err)))
          (should (string-match-p "rollback failed"
                                  (error-message-string err))))))))

;;; ─── Server-aware operator SQL boundary (tracked-case) ───────────────────

(ert-deftest dorg-test-transport-public-post-init-sql-boundaries-never-cli ()
  "Ordinary public post-init SQL boundaries stay on mysql, never CLI.
Coverage: direct `dorg-sql', `dorg-sql-batch', HEAD metadata, and
commit-ancestor metadata.  Non-vacuous: `dorg-sql--cli-exec' is a
failing sentinel and each path records the observed mysql port."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport 'server)
          (mysql-ports nil)
          (mysql-queries nil)
          (batch-ports nil)
          (cli-called nil))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 64010))
                ;; Endpoint ownership (tracked-case): callers no longer pass a
                ;; port; the warm connection resolves strict-port lazily only on
                ;; a cache miss.  Mirror that here so the observed port is the
                ;; lazily-resolved 64010, and route the batch through the shared
                ;; `dorg-sql--mysql-batch-on-conn' seam (no `dorg-sql-batch--mysql').
                ((symbol-function 'dorg-sql--get-connection)
                 (lambda (dir &optional port)
                   (let ((p (or port (dorg-dolt-server--strict-port dir))))
                     (push p mysql-ports)
                     (list :port p))))
                ((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (query _conn)
                   (push query mysql-queries)
                   (cond
                    ((string-match-p "HASHOF('HEAD')" query)
                     '(((hash . "abc123"))))
                    ((string-match-p "dolt_log" query)
                     '(((a . 1))))
                    (t '(((ok . 1)))))))
                ((symbol-function 'dorg-sql--mysql-batch-on-conn)
                 (lambda (conn _statements _message)
                   (push (plist-get conn :port) batch-ports)
                   nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _)
                   (setq cli-called t)
                   (error "one-shot CLI used by public SQL boundary"))))
        (let ((repo (dorg-test-transport--repo "operator-public-boundary")))
          (should (equal (dorg-sql repo "SELECT 1 AS ok") '(((ok . 1)))))
          (should-not (dorg-sql-batch repo (list "INSERT INTO t VALUES (1)") "msg"))
          (should (equal (dorg-head-commit repo) "abc123"))
          (should (dorg-commit-ancestor-p repo "abc123"))
          (should-not cli-called)
          ;; SELECT + batch checkout + HEAD + ancestor all observe 64010; the
          ;; batch's conn (from the warm entry) carries the same port.
          (should (equal mysql-ports '(64010 64010 64010 64010)))
          (should (equal batch-ports '(64010))))))))

(ert-deftest dorg-test-transport-cli-exec-refuses-live-dorg-claim-before-spawn ()
  "`dorg-sql--cli-exec' refuses a live DOrg server claim before spawn."
  (dorg-test-transport--with-runtime
    (let* ((repo (dorg-test-transport--repo "live-dorg-claim"))
           (claim (list (cons :dolt-dir (dorg-dolt-server--canonical-dir repo))
                        (cons :host (system-name))
                        (cons :owner-pid (emacs-pid))
                        (cons :owner-kind 'daemon)
                        (cons :server-pid (emacs-pid))
                        (cons :port 64011)
                        (cons :policy 'server)
                        (cons :protocol dorg-dolt-server-claim-protocol))))
      (dorg-test-transport--with-fake-cli-process
        (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim)
                   (lambda (_dir) claim))
                  ((symbol-function 'dorg-dolt-server--claim-classify)
                   (lambda (_claim &optional _deep) 'live)))
          (let ((err (should-error
                      (dorg-sql--cli-exec "SELECT 1" (dorg-dolt--target repo))
                      :type 'dorg-sql-transport-conflict)))
            (should (string-match-p (regexp-quote repo)
                                    (error-message-string err)))
            (should (string-match-p "managed server"
                                    (error-message-string err))))
          (should-not spawned))))))

(ert-deftest dorg-test-transport-cli-exec-refuses-live-dolt-native-info-before-spawn ()
  "`dorg-sql--cli-exec' refuses live `.dolt/sql-server.info' before spawn."
  (dorg-test-transport--with-runtime
    (let ((repo (dorg-test-transport--repo "live-native-info")))
      (unwind-protect
          (progn
            (dorg-test-transport--write-dolt-info repo (emacs-pid) 64012 "uuid-zimef")
            (dorg-test-transport--with-fake-cli-process
              (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p)
                         (lambda (_port) t)))
                (let ((err (should-error
                            (dorg-sql--cli-exec "SELECT 1" (dorg-dolt--target repo))
                            :type 'dorg-sql-transport-conflict)))
                  (should (string-match-p "sql-server.info"
                                          (error-message-string err)))
                  (should (string-match-p "managed server"
                                          (error-message-string err))))
                (should-not spawned))))
        ;; The synthetic native `.dolt/sql-server.info' records THIS live Emacs
        ;; pid; the owned-root teardown reaper can never `confirm-server-gone'
        ;; against our own live process (fail-closed: it refuses to kill it), so
        ;; leaving the file strands this `dorg-tx-store-' root at the crash-net
        ;; (tracked-case).  Delete it now that the refusal has been observed.
        (ignore-errors
          (delete-file (expand-file-name ".dolt/sql-server.info" repo)))))))

(ert-deftest dorg-test-transport-cli-exec-live-server-bypass-is-explicit-dynamic ()
  "The live-server CLI bypass is explicit and dynamically scoped."
  (dorg-test-transport--with-runtime
    (let* ((repo (dorg-test-transport--repo "live-claim-bypass"))
           (claim (list (cons :dolt-dir (dorg-dolt-server--canonical-dir repo))
                        (cons :host (system-name))
                        (cons :owner-pid (emacs-pid))
                        (cons :owner-kind 'daemon)
                        (cons :server-pid (emacs-pid))
                        (cons :port 64013)
                        (cons :policy 'server)
                        (cons :protocol dorg-dolt-server-claim-protocol))))
      (should (boundp 'dorg-sql--cli-conflict-check))
      (dorg-test-transport--with-fake-cli-process
        (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim)
                   (lambda (_dir) claim))
                  ((symbol-function 'dorg-dolt-server--claim-classify)
                   (lambda (_claim &optional _deep) 'live)))
          (should-error (dorg-sql--cli-exec "SELECT 1" (dorg-dolt--target repo))
                        :type 'dorg-sql-transport-conflict)
          (should-not spawned)))
      (dorg-test-transport--with-fake-cli-process
        (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim)
                   (lambda (_dir) claim))
                  ((symbol-function 'dorg-dolt-server--claim-classify)
                   (lambda (_claim &optional _deep) 'live)))
          (let ((dorg-sql--cli-conflict-check nil))
            (should (equal (dorg-sql--cli-exec "SELECT 1" (dorg-dolt--target repo))
                           nil))
            (should (eq spawned 'dorg-test-sql-cli-proc)))))
      (should dorg-sql--cli-conflict-check))))

(ert-deftest dorg-test-transport-cli-timeout-reaps-exact-process-only ()
  "Allowed one-shot CLI keeps timeout and reaps the exact spawned process."
  (let* ((repo (dorg-test-transport--repo "timeout-exact"))
         (target (dorg-dolt--target repo))
         (dorg-sql-cli-timeout 0.01)
         (other-proc (make-symbol "other-proc")))
    (dorg-test-transport--with-fake-cli-process
      (let ((orig-live (symbol-function 'process-live-p)))
        (cl-letf (((symbol-function 'process-live-p)
                   (lambda (proc)
                     (cond
                      ((eq proc spawned) fake-live)
                      ((eq proc other-proc) t)
                      (t (funcall orig-live proc)))))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest _) nil)))
          (should-error (dorg-sql--cli-exec "SELECT 1" target)
                        :type 'dorg-sql-transport-error)
          (should (eq killed spawned))
          (should-not (eq killed other-proc)))))))

;;; ─── Transport policy boundaries ────────────────────────────────────────
;; These prove each post-init SQL boundary stays on one coherent transport
;; under a strict `server' policy (originally the pre-flip boundary work;
;; the packaged default is now interactivity-aware).  Tests
;; bind `dorg-sql-transport' to `server' locally to exercise strict mode
;; deterministically regardless of the process's loaded default.

(ert-deftest dorg-test-transport-pin-overrides-global ()
  "`dorg-sql--with-pinned-transport' pins one resolved transport for a dir.
While the pin is active, `dorg-sql--resolve-transport' returns the pinned
cell even if the global `dorg-sql-transport' is rebound to a policy that
would otherwise signal.  Non-vacuous: the inner rebind to an invalid
policy would fail without the pin."
  (dorg-test-transport--with-runtime
    (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 5000)))
      (dorg-sql--with-cli-seam (dorg-test-transport--repo "fake")
        (dorg-sql--with-pinned-transport (dorg-test-transport--repo "fake")
          (let ((dorg-sql-transport 'bogus))
            (should (equal (dorg-sql--resolve-transport (dorg-test-transport--repo "fake")) '(cli))))))
      ;; Outside the seam/pin the global wins again (control).
      (let ((dorg-sql-transport 'server))
        (should (equal (dorg-sql--resolve-transport (dorg-test-transport--repo "fake"))
                       '(mysql . 5000)))))))

(ert-deftest dorg-test-transport-batch-cli-rollback-same-transport ()
  "`dorg-sql-batch' CLI error path rolls back on the transport the batch
selected, not a freshly re-resolved one.  Non-vacuous: the global policy
is flipped to one that would resolve to mysql AFTER the batch begins; the
pin must keep the ROLLBACK on `(cli)'."
  (dorg-test-transport--with-runtime
    (let ((rollback-transport 'unset)
          (dorg-sql-transport 'server))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) 3307))
                ((symbol-function 'dorg-sql)
                 (lambda (dir q &rest _)
                   (cond
                    ((string-match-p "ROLLBACK" q)
                     (setq rollback-transport (dorg-sql--resolve-transport dir))
                     nil)
                    (t
                     ;; A concurrent policy change once the batch is underway.
                     (setq dorg-sql-transport 'bogus)
                     (error "boom"))))))
        (dorg-sql--with-cli-seam (dorg-test-transport--repo "fake")
          (should-error
           (dorg-sql-batch (dorg-test-transport--repo "fake") (list "INSERT INTO t VALUES (1)") "msg")))
        (should (equal rollback-transport '(cli)))))))

(ert-deftest dorg-test-transport-init-starts-bootstrap-server ()
  "Fresh `dorg-init' starts a managed server before post-init SQL.
The schema batch and initial `dorg-commit' must then resolve the normal
strict `server' transport.  Non-vacuous: the old bootstrap-transport
wrapper resolves both boundaries to `(cli)', and a partial fix that
starts the server but keeps the wrapper still fails the transport
assertions."
  (dorg-test-transport--with-runtime
    (let ((batch-transport 'unset)
          (commit-transport 'unset)
          (bootstrap-calls 0)
          (dorg-sql-transport 'server)
          ;; Real fresh temp dir so `dorg-init's file primitives run for
          ;; real (mocking `file-exists-p' breaks native-comp trampolines).
          ;; Only the dolt subprocess and SQL boundaries are stubbed, each
          ;; recording the transport it resolves at call time.
          (init-dir (make-temp-file "dorg-init-test-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                    ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                    ((symbol-function 'call-process) (lambda (&rest _) 0))
                    ((symbol-function 'dorg-repo--new-store-id)
                     (lambda () "0123456789abcdef0123456789abcdef"))
                    ((symbol-function 'dorg-registered-extensions) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--start-bootstrap)
                     (lambda (_dir) (cl-incf bootstrap-calls) 55001))
                    ((symbol-function 'dorg-dolt-server--strict-port)
                     (lambda (&rest _) 55001))
                    ((symbol-function 'dorg-sql-batch)
                     (lambda (dir &rest _)
                       (setq batch-transport (dorg-sql--resolve-transport dir)) nil))
                    ((symbol-function 'dorg-commit)
                     (lambda (dir &rest _)
                       (setq commit-transport (dorg-sql--resolve-transport dir)) nil)))
            (dorg-init (expand-file-name "repo/" init-dir))
            (should (= bootstrap-calls 1))
            (should (equal batch-transport '(mysql . 55001)))
            (should (equal commit-transport '(mysql . 55001))))
        (ignore-errors (delete-directory init-dir t))))))

(ert-deftest dorg-test-transport-init-failure-stops-bootstrap-server ()
  "A post-bootstrap `dorg-init' failure tears down the bootstrap server.
When `dorg-init' has started a bootstrap sql-server and a later step
signals, the `unwind-protect' cleanup must call
`dorg-dolt-server--stop' on the initialized dolt-dir so no orphaned
server process/claim is leaked.  Non-vacuous: the removed design left
`(ignore bootstrap-started)' as the cleanup, which never stops the
server — RED before the fix."
  (dorg-test-transport--with-runtime
    (let* ((dorg-sql-transport 'server)
           (bootstrap-calls 0)
           (stop-calls 0)
           (stop-dir nil)
           (init-dir (make-temp-file "dorg-init-fail-stop-" t))
           (repo (expand-file-name "repo/" init-dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                    ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                    ((symbol-function 'call-process) (lambda (&rest _) 0))
                    ((symbol-function 'dorg-registered-extensions) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--start-bootstrap)
                     (lambda (_dir) (cl-incf bootstrap-calls) 55002))
                    ((symbol-function 'dorg-dolt-server--stop)
                     (lambda (dir) (cl-incf stop-calls) (setq stop-dir dir)))
                    ;; Force failure AFTER the bootstrap server has started.
                    ((symbol-function 'dorg-sql-batch)
                     (lambda (&rest _) (error "forced schema-batch failure"))))
            (should-error (dorg-init repo) :type 'error)
            (should (= bootstrap-calls 1))
            (should (= stop-calls 1))
            (should (equal (expand-file-name stop-dir)
                           (expand-file-name repo))))
        (ignore-errors (delete-directory init-dir t))))))

(ert-deftest dorg-test-transport-init-success-keeps-bootstrap-server ()
  "A successful `dorg-init' must NOT stop the bootstrap server.
On normal completion the bootstrap sql-server becomes the process
serving the freshly-initialized repo, so cleanup fires only on non-local
exit.  Non-vacuous guard against a naive always-stop cleanup that would
tear down the live server on success."
  (dorg-test-transport--with-runtime
    (let* ((dorg-sql-transport 'server)
           (bootstrap-calls 0)
           (stop-calls 0)
           (init-dir (make-temp-file "dorg-init-ok-keep-" t))
           (repo (expand-file-name "repo/" init-dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                    ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                    ((symbol-function 'call-process) (lambda (&rest _) 0))
                    ((symbol-function 'dorg-repo--new-store-id)
                     (lambda () "0123456789abcdef0123456789abcdef"))
                    ((symbol-function 'dorg-registered-extensions) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--start-bootstrap)
                     (lambda (_dir) (cl-incf bootstrap-calls) 55003))
                    ((symbol-function 'dorg-dolt-server--stop)
                     (lambda (&rest _) (cl-incf stop-calls)))
                    ((symbol-function 'dorg-sql-batch) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-commit) (lambda (&rest _) nil)))
            (dorg-init repo)
            (should (= bootstrap-calls 1))
            (should (= stop-calls 0)))
        (ignore-errors (delete-directory init-dir t))))))

(ert-deftest dorg-test-transport-init-failure-no-bootstrap-no-stop ()
  "A mid-init failure with no bootstrap server started stops nothing.
When transport pins the store (no bootstrap server was launched), the
`unwind-protect' cleanup must not call `dorg-dolt-server--stop' — there
is no server to tear down.  Guards the `bootstrap-started' gate on the
cleanup."
  (dorg-test-transport--with-runtime
    (let* ((dorg-sql-transport 'server)
           (bootstrap-calls 0)
           (stop-calls 0)
           (init-dir (make-temp-file "dorg-init-nostart-" t))
           (repo (expand-file-name "repo/" init-dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                    ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                    ((symbol-function 'call-process) (lambda (&rest _) 0))
                    ((symbol-function 'dorg-registered-extensions) (lambda (&rest _) nil))
                    ;; Pin the transport so no bootstrap server is started.
                    ((symbol-function 'dorg-sql--pinned-transport-for)
                     (lambda (&rest _) '(cli)))
                    ((symbol-function 'dorg-dolt-server--start-bootstrap)
                     (lambda (_dir) (cl-incf bootstrap-calls) 55004))
                    ((symbol-function 'dorg-dolt-server--stop)
                     (lambda (&rest _) (cl-incf stop-calls)))
                    ((symbol-function 'dorg-sql-batch)
                     (lambda (&rest _) (error "forced schema-batch failure"))))
            (should-error (dorg-init repo) :type 'error)
            (should (= bootstrap-calls 0))
            (should (= stop-calls 0)))
        (ignore-errors (delete-directory init-dir t))))))

(ert-deftest dorg-test-transport-fixture-helper-binds-server ()
  "Shared-server fixture creation runs BOTH halves — the schema
`dorg-sql-batch' AND the `dorg-commit' — under strict `server' transport,
independent of the suite's global default.  The fixture DB lives on the
shared sql-server, so it is built server-required.
Non-vacuous: the global is bound to an invalid policy yet both boundaries
must observe `server'; a partial fix leaving `commit-policy' at the global
is rejected."
  (let ((batch-policy 'unset)
        (commit-policy 'unset)
        (dorg-sql-transport 'bogus))
    ;; `dorg-test--create-test-db' now delegates the CREATE DATABASE to the
    ;; opaque fixture (`dorg-dolt-fixture-store-acquire'); mock that seam so no
    ;; real server is touched, and pin that BOTH schema halves still run under
    ;; strict `server' transport.
    (cl-letf (((symbol-function 'dorg-dolt-fixture-store-acquire)
               (lambda (&rest _) 'dorg-test-fixture--store-lease))
              ((symbol-function 'dorg-dolt-fixture-lease-dolt-dir)
               (lambda (_lease) "/tmp/dorg-fixture-store/"))
              ((symbol-function 'dorg-sql-batch)
               (lambda (&rest _) (setq batch-policy dorg-sql-transport) nil))
              ((symbol-function 'dorg-commit)
               (lambda (&rest _) (setq commit-policy dorg-sql-transport) nil)))
      (dorg-test--create-test-db)
      (should (eq batch-policy 'server))
      (should (eq commit-policy 'server)))))

(ert-deftest dorg-test-transport-fixture-template-uses-cli-seam ()
  "The no-shared-server fallback fixture path (copy-repo/template) builds its
`dorg-init' template through the sanctioned CLI seam
\(`dorg-sql--with-cli-seam'), NOT the `auto'/`cli' policy the server-only
collapse (tracked-case) deletes.  The template is a standalone store no server
holds, so `dorg-test--copy-repo' → `dorg-test--ensure-template' → `dorg-init'
must build it policy-independently: inside `dorg-init' the store's transport
is pinned to one-shot CLI and the standalone-store conflict check is
disarmed, regardless of the strict global default.  Non-vacuous: the global
is `server' yet the build must observe the pinned `(cli . nil)'."
  (let ((pinned 'unset)
        (conflict-check 'unset)
        (dorg-sql-transport 'server)
        (dorg-test--server-port nil)
        (dorg-test--template-dir nil)
        (dest (make-temp-file "dorg-fixture-dest-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-init)
                   (lambda (dolt-dir &rest _)
                     (setq pinned (dorg-sql--pinned-transport-for dolt-dir)
                           conflict-check dorg-sql--cli-conflict-check)))
                  ((symbol-function 'copy-directory) (lambda (&rest _) nil)))
          (dorg-test--copy-repo dest)
          (should (equal '(cli . nil) pinned))
          (should (null conflict-check)))
      (ignore-errors (delete-directory dest t))
      (when (and dorg-test--template-dir
                 (file-directory-p dorg-test--template-dir))
        (ignore-errors (delete-directory dorg-test--template-dir t)))
      (setq dorg-test--template-dir nil))))

;;; ─── Strict-server default ──────────────────────────────────────────────
;; The packaged default transport is strict `server' in every entry point.

(ert-deftest dorg-test-transport-packaged-default-interactive-server ()
  "The packaged default transport is strict `server' in an interactive Emacs."
  (let ((noninteractive nil))
    (should (eq (dorg-sql--default-transport) 'server))))

(ert-deftest dorg-test-transport-packaged-default-noninteractive-server ()
  "The packaged default transport is strict `server' under batch too."
  (let ((noninteractive t))
    (should (eq (dorg-sql--default-transport) 'server))))

(ert-deftest dorg-test-transport-policy-cell-is-custom ()
  "`dorg-sql-transport' is a documented, let-bindable Custom option.
The Customize contract admits only the existing public `server' policy;
turning the policy cell into a `defcustom' must not widen its accepted values."
  (should (eq (default-value 'dorg-sql-transport) 'server))
  (should (custom-variable-p 'dorg-sql-transport))
  (let ((custom-type (get 'dorg-sql-transport 'custom-type)))
    (should (eq (car custom-type) 'const))
    (should (eq (car (last custom-type)) 'server)))
  (let ((dorg-sql-transport 'server))
    (should (eq dorg-sql-transport 'server))))

(ert-deftest dorg-test-transport-noninteractive-default-autostart-enabled ()
  "Under the noninteractive packaged default, a no-server repo autostarts
when `dorg-dolt-server-autostart' is enabled.
This pins the default batch surface after public `auto' deletion: the
packaged default is `server', and with autostart enabled its no-server
behavior is detached daemon ensure plus mysql routing."
  (dorg-test-transport--with-runtime
    (let ((dorg-sql-transport (let ((noninteractive t))
                                (dorg-sql--default-transport)))
          (dorg-dolt-server-autostart t)
          (detached-calls 0))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-detached)
                 (lambda (&rest _) (cl-incf detached-calls) 64004)))
        (should (equal (dorg-sql--resolve-transport (dorg-test-transport--repo "fake"))
                       '(mysql . 64004)))
        (should (= detached-calls 1))))))

(ert-deftest dorg-test-transport-noninteractive-default-autostart-disabled-errors ()
  "Under the noninteractive packaged default, a no-server repo signals
`dorg-sql-transport-error' when `dorg-dolt-server-autostart' is disabled
\(the default).  Pins the packaged batch default's actual out-of-the-box
behavior: no silent daemon start, and no silent session-scoped server
start either — both `dorg-dolt-server--start-detached-unsafe' and
`dorg-dolt-server--start-unsafe' are failing sentinels.  The error text
must name the exact remedy (`dorg dolt start PATH') and the exact option
\(`dorg-dolt-server-autostart')."
  (dorg-test-transport--with-runtime
    (let* ((dorg-sql-transport (let ((noninteractive t))
                                 (dorg-sql--default-transport)))
           (dolt-dir (dorg-test-transport--repo "fake")))
      (should-not dorg-dolt-server-autostart)
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon when autostart is disabled")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server when autostart is disabled"))))
        (let ((err (should-error (dorg-sql--resolve-transport dolt-dir)
                                 :type 'dorg-sql-transport-error)))
          (should (string-match-p
                   (regexp-quote (format "dorg dolt start %s" dolt-dir))
                   (cadr err)))
          (should (string-match-p "dorg-dolt-server-autostart" (cadr err))))))))

;;; ─── Honest boundary: policy lifted above the primitive (tracked-case) ─────

(ert-deftest dorg-test-transport-fns-relocated ()
  "Transport POLICY lives in `dorg-transport', above dorg-dolt and dolt-server.
`dorg-sql--resolve-transport' and the claim-based CLI refusal guard reach
into the server/claim layer, so they must not live in the low-level SQL
primitive module."
  (require 'dorg-transport)
  (should (equal "dorg-transport"
                 (file-name-base (symbol-file 'dorg-sql--resolve-transport 'defun))))
  (should (equal "dorg-transport"
                 (file-name-base
                  (symbol-file 'dorg-sql--cli-refuse-live-owner 'defun)))))

(ert-deftest dorg-test-transport-seam-installed ()
  "Loading `dorg-transport' installs its policy seam into `dorg-dolt'.
`dorg-dolt' invokes transport resolution and the CLI preflight guard
through function-variable seams so control flows DOWNWARD (transport ->
server) instead of the primitive reaching up."
  (require 'dorg-transport)
  (should (functionp dorg-sql--resolve-transport-fn))
  (should (functionp dorg-sql--cli-preflight-fn)))

(ert-deftest dorg-test-transport-seam-install-is-owner-guarded-and-atomic ()
  "Only `dorg-transport' may install the transport-policy seam pair.
An owner reinstall of the same `eq' functions is an idempotent no-op so module
reload remains safe.  Foreign attempts and owner replacement with different
functions signal the typed install error before either installed function
changes.  Distinct sentinels make every mutation assertion non-vacuous."
  (let ((dorg-sql--resolve-transport-fn nil)
        (dorg-sql--cli-preflight-fn nil)
        (resolve-1 (lambda (&rest _) 'resolve-1))
        (preflight-1 (lambda (&rest _) 'preflight-1))
        (resolve-2 (lambda (&rest _) 'resolve-2))
        (preflight-2 (lambda (&rest _) 'preflight-2)))
    (should-error
     (dorg-sql--install-transport-policy
      'foreign-provider resolve-1 preflight-1)
     :type 'dorg-seam-install-error)
    (should-not dorg-sql--resolve-transport-fn)
    (should-not dorg-sql--cli-preflight-fn)

    (dorg-sql--install-transport-policy
     'dorg-transport resolve-1 preflight-1)
    (should (eq resolve-1 dorg-sql--resolve-transport-fn))
    (should (eq preflight-1 dorg-sql--cli-preflight-fn))

    (dorg-sql--install-transport-policy
     'dorg-transport resolve-1 preflight-1)
    (should (eq resolve-1 dorg-sql--resolve-transport-fn))
    (should (eq preflight-1 dorg-sql--cli-preflight-fn))

    (should-error
     (dorg-sql--install-transport-policy
      'dorg-transport resolve-2 preflight-2)
     :type 'dorg-seam-install-error)
    (should (eq resolve-1 dorg-sql--resolve-transport-fn))
    (should (eq preflight-1 dorg-sql--cli-preflight-fn))

    (should-error
     (dorg-sql--install-transport-policy
      'foreign-provider resolve-2 preflight-2)
     :type 'dorg-seam-install-error)
    (should (eq resolve-1 dorg-sql--resolve-transport-fn))
    (should (eq preflight-1 dorg-sql--cli-preflight-fn))))

(ert-deftest dorg-test-transport-owner-install-survives-module-reload ()
  "Loading `dorg-transport' twice preserves its same-owner seam installation.
The production installer runs from `with-eval-after-load', so this fresh-image
regression test prevents a strict duplicate guard from breaking `dorg-reload'."
  (let* ((root (file-name-as-directory dorg-run-tests--project-root))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-transport)
             (load (expand-file-name \"dorg-transport.el\" default-directory)
                   nil t t)
             (load (expand-file-name \"dorg-transport.el\" default-directory)
                   nil t t))"))
    (with-temp-buffer
      (let ((status
             (call-process emacs nil (current-buffer) nil
                           "--batch" "-Q"
                           "-L" root
                           "-L" (expand-file-name "lib/mysql" root)
                           "--eval"
                           (format "(let ((default-directory %S)) %s)"
                                   root expr))))
        (ert-info ((format "child Emacs output:\n%s" (buffer-string)))
          (should (= 0 status)))))))

(ert-deftest dorg-test-transport-dolt-claim-refs-removed ()
  "`dorg-dolt.el' no longer names the server CLAIM machinery.
The claim-classification autoloads move out with the policy; only the
connection/lease ENDPOINT-DISCOVERY autoloads (owned elsewhere) remain."
  (let* ((root (or (locate-dominating-file default-directory "dorg-dolt.el")
                   default-directory))
         (src (with-temp-buffer
                (insert-file-contents (expand-file-name "dorg-dolt.el" root))
                (buffer-string))))
    (dolist (sym '("dorg-dolt-server--effective-claim"
                   "dorg-dolt-server--claim-classify"
                   "dorg-dolt-server--claim-host-local-p"))
      (should-not (string-match-p (regexp-quote sym) src)))))

(ert-deftest dorg-test-transport-resolve-autocomposes ()
  "A standalone `dorg-dolt' load self-composes transport policy on demand.
`dorg-repo' lifecycle uses `dorg-sql--with-pinned-transport' while requiring
only `dorg-dolt', so the resolve path must be order-INDEPENDENT: with the
seam nil and `dorg-transport' unloaded, `dorg-sql--ensure-transport-policy'
lazy-loads the policy module and installs the resolve seam.  A fresh
`emacs -Q' isolates the boundary from the full-image load order."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-dolt)
             (when dorg-sql--resolve-transport-fn
               (error \"resolve seam should be nil after dolt-only load\"))
             (when (featurep 'dorg-transport)
               (error \"dorg-transport should not be loaded yet\"))
             (dorg-sql--ensure-transport-policy)
             (unless (featurep 'dorg-transport)
               (error \"ensure-transport-policy did not lazy-load dorg-transport\"))
             (unless (functionp dorg-sql--resolve-transport-fn)
               (error \"ensure-transport-policy did not install the resolve seam\")))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "-L" (expand-file-name "lib/mysql" root)
                               "--eval" expr)))))

(provide 'dorg-transport-tests)
;;; dorg-transport-tests.el ends here
