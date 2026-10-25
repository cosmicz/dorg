;;; dorg-dolt-server-evidence-tests.el --- Claim store evidence from the kernel -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Controls for `dorg-dolt-server--claim-store-verdict': what a claim's live
;; process demonstrably serves, decided before any endpoint contact.  Argv
;; answers when it names an explicit --data-dir; otherwise the kernel answers
;; from the noms LOCK files the process holds open and its working directory,
;; and a dead or replaced recorded pid defers to the owner of the claimed port.
;;
;; Every control here is server-free: no dolt binary, no sql-server, no store
;; opened, no port dialled.  Classifier controls inject kernel observations at
;; the probe seam (`dorg-dolt-server--probe-served-paths',
;; `dorg-dolt-server--probe-socket-owner', `process-attributes'); probe
;; controls read the real kernel for a child shell whose pid, cwd and open
;; files the test itself established.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-dolt-server)
(require 'dorg-test-helpers)

;;; ─── Fixtures ───────────────────────────────────────────────────────────

(defmacro dorg-test-evidence--with-root (root &rest body)
  "Bind ROOT to a fresh owned temp directory and run BODY with clean claim state.
The `native-comp-*' bindings keep primitive redefinitions honored: building a
subr trampoline would otherwise call the very primitives a control forbids."
  (declare (indent 1))
  `(let ((,root (file-name-as-directory
                 (dorg-test--make-owned-temp-dir "dorg-evidence-" "claim-evidence")))
         (native-comp-jit-compilation nil)
         (native-comp-enable-subr-trampolines nil)
         (dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
         (dorg-dolt-server--attached-ports (make-hash-table :test 'equal))
         (real-linux-proc-birth
          (symbol-function 'dorg-dolt-server--linux-proc-birth)))
     ;; Server-free classifier controls inject `process-attributes' for fake
     ;; PIDs.  On Linux their matching birth evidence must be injected at the
     ;; same seam; the real raw-child control dynamically opts out below.
     (cl-letf (((symbol-function 'dorg-dolt-server--linux-proc-birth)
                (lambda (pid)
                  (if dorg-test-evidence--use-real-linux-proc-birth
                      (funcall real-linux-proc-birth pid)
                    (dorg-test-evidence--injected-linux-proc-birth pid)))))
       (unwind-protect
           (progn ,@body)
         (dorg-test--delete-owned-root ,root)))))

(defun dorg-test-evidence--store (root name)
  "Create store NAME under ROOT with a noms LOCK file; return its directory."
  (let ((dir (file-name-as-directory (expand-file-name name root))))
    (make-directory (expand-file-name ".dolt/noms" dir) t)
    (with-temp-file (expand-file-name ".dolt/noms/LOCK" dir))
    dir))

(defun dorg-test-evidence--lock (store)
  "Return STORE's noms LOCK path as a process holding it would report it."
  (directory-file-name (expand-file-name ".dolt/noms/LOCK" store)))

(defun dorg-test-evidence--canon (dir)
  "Return DIR's canonical store path."
  (dorg-dolt-server--canonical-dir dir))

(defconst dorg-test-evidence--start '(26000 100 0 0)
  "A process start time for injected process attributes.")

(defconst dorg-test-evidence--raw-server-selector
  "^dorg-test-evidence-real-kernel-raw-server-shape$"
  "The one selector eligible for an opt-in raw-server evidence repeat.")

(defconst dorg-test-evidence--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                            (or load-file-name buffer-file-name))))
  "Repository root containing this evidence test file.")

(defun dorg-test-evidence--suite-job-refusal (&rest arguments)
  "Run the suite-job launcher with ARGUMENTS and return its status and output.
The malformed requests exercised by this helper are rejected before Docker or
source export, so this is a server-free command-line control."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         (expand-file-name "tools/dorg-suite-job-local"
                                           dorg-test-evidence--root)
                         nil t nil "--image" "ignored" arguments)))
      (cons status (buffer-string)))))

(defun dorg-test-evidence--raw-server-repeat-count ()
  "Return the configured bounded count for raw-server evidence observations.
An unset value keeps ordinary and release runs at one observation."
  (let ((value (getenv "DORG_RAW_SERVER_EVIDENCE_REPEAT")))
    (cond
     ((or (null value) (equal value "")) 1)
     ((and (string-match-p "\\`[1-9][0-9]*\\'" value)
           (<= (string-to-number value) 50))
      (string-to-number value))
     (t
      (ert-fail
       (format "DORG_RAW_SERVER_EVIDENCE_REPEAT must be an integer in 1..50: %S"
               value))))))

(defun dorg-test-evidence--proc-stat-starttime-from-string (contents)
  "Return Linux /proc stat field 22 from CONTENTS, or nil when malformed.
The command field is parenthesized and may itself contain spaces or parens, so
split only after its final closing parenthesis."
  (when (string-match "\\`[0-9]+ (.*) \\(.*\\)\\(?:\n\\)?\\'" contents)
    (let ((value (nth 19 (split-string (match-string 1 contents) " " t))))
      (and value (string-match-p "\\`[0-9]+\\'" value) value))))

(defun dorg-test-evidence--proc-stat-starttime (pid)
  "Return PID's Linux /proc stat field 22, or nil when it is unavailable.
This is diagnostic evidence only; it is never supplied to the production
identity predicate."
  (when (eq system-type 'gnu/linux)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents (format "/proc/%d/stat" pid))
          (dorg-test-evidence--proc-stat-starttime-from-string (buffer-string)))
      (file-error nil)
      (error nil))))

(defun dorg-test-evidence--raw-server-read (ordinal pid attrs)
  "Return diagnostic evidence for classifier read ORDINAL of PID with ATTRS."
  (list :ordinal ordinal
        :start (cdr (assq 'start attrs))
        :args (cdr (assq 'args attrs))
        :identity (dorg-dolt-server--attrs-identity pid attrs)
        :proc-starttime (dorg-test-evidence--proc-stat-starttime pid)))

(defun dorg-test-evidence--raw-server-start-difference (reads)
  "Return the full-precision difference between the first two READS, or nil."
  (let ((first (plist-get (car reads) :start))
        (second (plist-get (cadr reads) :start)))
    (when (and first second)
      (condition-case nil
          (float-time (time-subtract second first))
        (error nil)))))

(defun dorg-test-evidence--raw-server-mismatched-attrs (attrs)
  "Return ATTRS with a distinct, valid process start time for the test seam."
  (let ((copy (copy-tree attrs)))
    (setcdr (assq 'start copy)
            (time-add (cdr (assq 'start copy)) 1))
    copy))

(defun dorg-test-evidence--raw-attrs (&optional start)
  "Return attributes of a raw `dolt sql-server' without --data-dir.
START defaults to `dorg-test-evidence--start'."
  `((comm . "dolt")
    (args . "dolt sql-server --port 3306")
    (start . ,(or start dorg-test-evidence--start))))

(defun dorg-test-evidence--explicit-attrs (data-dir)
  "Return attributes of a `dolt sql-server' with explicit DATA-DIR."
  `((comm . "dolt")
    (args . ,(format "dolt sql-server --port 3306 --data-dir %s" data-dir))
    (start . ,dorg-test-evidence--start)))

(defun dorg-test-evidence--attrs-table (table)
  "Return a `process-attributes' stand-in answering from alist TABLE by pid."
  (lambda (pid) (cdr (assq pid table))))

(defun dorg-test-evidence--refuse (log what)
  "Return a function recording WHAT in LOG's car, then signalling, if called.
The classifier turns every error into absent evidence, so a signal alone
would be swallowed; callers assert that (car LOG) stayed empty."
  (lambda (&rest _)
    (push what (car log))
    (ert-fail (format "%s must not be called" what))))

(defun dorg-test-evidence--tree (root)
  "Return every path under ROOT, sorted, for a before/after comparison."
  (sort (directory-files-recursively root "" t) #'string<))

;;; ─── The served SET ─────────────────────────────────────────────────────

(ert-deftest dorg-test-evidence-served-set-membership ()
  "Proven means IN the served set; contradictory means NOT in it; empty is absent."
  (dorg-test-evidence--with-root root
    (let* ((a (dorg-test-evidence--store root "d/a"))
           (b (dorg-test-evidence--store root "d/b"))
           (other (dorg-test-evidence--store root "elsewhere/a"))
           (claim '((:server-pid . 4101) (:port . 43201)))
           (paths (list :locks (list (dorg-test-evidence--lock a)
                                     (dorg-test-evidence--lock b))
                        :cwd nil))
           (probed 0))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4101 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4101))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (pid) (should (eql pid 4101)) (cl-incf probed) paths)))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence a claim)))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence b claim)))
        (should (eq 'contradictory
                    (dorg-dolt-server--claim-store-evidence other claim)))
        (should (= probed 3))
        ;; An empty set is no evidence at all, never a contradiction.
        (setq paths (list :locks nil :cwd nil))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence other claim)))
        (should (= probed 4))))))

(ert-deftest dorg-test-evidence-multi-database-server-attaches-each ()
  "One process serving D/a and D/b: an attach to EACH succeeds.
A single-valued probe answering D/b for a request naming D/a would refuse
an attach that succeeds without these probes."
  (dorg-test-evidence--with-root root
    (let* ((a (dorg-test-evidence--store root "d/a"))
           (b (dorg-test-evidence--store root "d/b"))
           (port 43202)
           (attached nil))
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-read) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (&rest _) (list :pid 4102 :port port :uuid "multi")))
                ((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4102 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4102))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_)
                   (list :locks (list (dorg-test-evidence--lock b)
                                      (dorg-test-evidence--lock a))
                         :cwd nil)))
                ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (dir &rest _) (push (dorg-test-evidence--canon dir) attached)
                   port)))
        (should (= port (dorg-dolt-server--strict-port a)))
        (should (= port (dorg-dolt-server--strict-port b)))
        (should (equal (sort attached #'string<)
                       (sort (list (dorg-test-evidence--canon a)
                                   (dorg-test-evidence--canon b))
                             #'string<)))))))

(ert-deftest dorg-test-evidence-read-paths-are-canonicalised ()
  "A symlinked spelling of a served store resolves equal to the store."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "real/dorg"))
           (alias (expand-file-name "alias" root))
           (claim '((:server-pid . 4103) (:port . 43203))))
      (make-symbolic-link (directory-file-name store) alias)
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4103 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4103))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 ;; The process reports the lock through the ALIAS.
                 (lambda (_)
                   (list :locks (list (dorg-test-evidence--lock
                                       (file-name-as-directory alias)))
                         :cwd nil))))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence store claim)))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence
                             (file-name-as-directory alias) claim)))))))

(ert-deftest dorg-test-evidence-cwd-answers-only-without-locks-or-data-dir ()
  "The working directory answers only when no lock does and argv is silent.
A raw server serves its cwd if that is a repo, and each repo beneath it."
  (dorg-test-evidence--with-root root
    (let* ((parent (file-name-as-directory (expand-file-name "d" root)))
           (a (dorg-test-evidence--store root "d/a"))
           (other (dorg-test-evidence--store root "elsewhere/a"))
           (attrs (dorg-test-evidence--raw-attrs))
           (claim '((:server-pid . 4104) (:port . 43204))))
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (_) attrs))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4104))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) (list :locks nil :cwd (directory-file-name parent)))))
        (let ((verdict (dorg-dolt-server--claim-store-verdict a claim)))
          (should (eq 'proven (plist-get verdict :evidence)))
          (should (eq 'cwd (plist-get verdict :basis))))
        (should (eq 'contradictory
                    (dorg-dolt-server--claim-store-evidence other claim)))
        ;; A socket owner with an explicit --data-dir does not serve its cwd,
        ;; so without a held lock its evidence is absent.
        (setq attrs (dorg-test-evidence--explicit-attrs (expand-file-name "zz" root)))
        (cl-letf (((symbol-function 'process-attributes)
                   (dorg-test-evidence--attrs-table
                    `((4105 . ,attrs))))
                  ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                   (lambda (_) 4105)))
          (should (eq 'absent (dorg-dolt-server--claim-store-evidence other claim))))))))

;;; ─── Source-tagged process birth identity ─────────────────────────────

(defconst dorg-test-evidence--linux-boot-id
  "01234567-89ab-cdef-0123-456789abcdef"
  "A syntactically valid Linux boot ID for injected identity observations.")

(defun dorg-test-evidence--linux-birth (ticks &optional boot-id)
  "Return injected Linux birth evidence with TICKS and optional BOOT-ID."
  (list (or boot-id dorg-test-evidence--linux-boot-id) ticks))

(defvar dorg-test-evidence--use-real-linux-proc-birth nil
  "When non-nil, bypass injected Linux birth evidence for a real child.")

(defun dorg-test-evidence--injected-linux-proc-birth (_pid)
  "Return stable Linux birth evidence for a fixture-injected process.
Every server-free control injects its process attributes, so this helper
supplies the corresponding kernel-only identity source on Linux."
  (dorg-test-evidence--linux-birth "26000"))

(ert-deftest dorg-test-evidence-linux-proc-identity-holds-through-emacs-start-drift ()
  "Stable Linux ticks hold identity despite the 908xe Emacs-start jitter."
  (let ((system-type 'gnu/linux)
        (attrs (list (dorg-test-evidence--raw-attrs '(26000 100 0 0))
                     (dorg-test-evidence--raw-attrs '(26000 100 10000 0))))
        (births (list (dorg-test-evidence--linux-birth "8033")
                      (dorg-test-evidence--linux-birth "8033"))))
    (cl-letf (((symbol-function 'process-attributes)
               (lambda (_) (pop attrs)))
              ((symbol-function 'dorg-dolt-server--linux-proc-birth)
               (lambda (_) (pop births))))
      (let ((identity (dorg-dolt-server--attrs-identity 5101 (process-attributes 5101))))
        (should (eq 'linux-proc (plist-get identity :source)))
        (should (equal (dorg-test-evidence--linux-birth "8033")
                       (plist-get identity :birth)))
        (should (dorg-dolt-server--identity-held-p identity))))))

(ert-deftest dorg-test-evidence-linux-proc-identity-refuses-torn-or-missing-birth ()
  "Linux rejects changed ticks, boot IDs or argv, and has no fallback."
  (dolist (births (list (list (dorg-test-evidence--linux-birth "8033")
                              (dorg-test-evidence--linux-birth "8034"))
                        (list (dorg-test-evidence--linux-birth "8033")
                              (dorg-test-evidence--linux-birth
                               "8033" "89abcdef-0123-4567-89ab-cdef01234567"))))
    (let ((system-type 'gnu/linux)
          (attrs (list (dorg-test-evidence--raw-attrs)
                       (dorg-test-evidence--raw-attrs))))
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (_) (pop attrs)))
                ((symbol-function 'dorg-dolt-server--linux-proc-birth)
                 (lambda (_) (pop births))))
        (let ((identity (dorg-dolt-server--attrs-identity 5102 (process-attributes 5102))))
          (should-not (dorg-dolt-server--identity-held-p identity))))))
  (let ((system-type 'gnu/linux)
        (attrs (list (dorg-test-evidence--raw-attrs)
                     `((comm . "dolt")
                       (args . "dolt sql-server --port 9999")
                       (start . ,dorg-test-evidence--start))))
        (births (list (dorg-test-evidence--linux-birth "8033")
                      (dorg-test-evidence--linux-birth "8033"))))
    (cl-letf (((symbol-function 'process-attributes)
               (lambda (_) (pop attrs)))
              ((symbol-function 'dorg-dolt-server--linux-proc-birth)
               (lambda (_) (pop births))))
      (let ((identity (dorg-dolt-server--attrs-identity 5102 (process-attributes 5102))))
        (should-not (dorg-dolt-server--identity-held-p identity)))))
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'dorg-dolt-server--linux-proc-birth)
               (lambda (_) nil)))
      ;; A present Emacs start cannot rescue an unreadable Linux proc record.
      (should-not (dorg-dolt-server--attrs-identity
                   5103 (dorg-test-evidence--raw-attrs))))))

(ert-deftest dorg-test-evidence-linux-proc-parser-is-last-paren-and-fail-safe ()
  "Field-22 and boot-id parsing reject malformed Linux proc content."
  (should (equal "8033"
                 (dorg-dolt-server--linux-proc-stat-starttime
                  "5104 (dolt worker) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 8033 21\n")))
  (should (equal "8034"
                 (dorg-dolt-server--linux-proc-stat-starttime
                  "5104 (dolt) worker) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 8034 21\n")))
  (dolist (contents '("5104 dolt S 1 2\n"
                      "5104 (dolt) S 1 2 3\n"
                      "5104 (dolt) S 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 -1 21\n"))
    (should-not (dorg-dolt-server--linux-proc-stat-starttime contents)))
  (should (equal dorg-test-evidence--linux-boot-id
                 (dorg-dolt-server--linux-proc-boot-id
                  (concat dorg-test-evidence--linux-boot-id "\n"))))
  (dolist (contents '("" "not-a-uuid\n" "01234567-89ab-cdef-0123-456789abcdef extra\n"))
    (should-not (dorg-dolt-server--linux-proc-boot-id contents))))

(ert-deftest dorg-test-evidence-identity-source-kind-never-mixes ()
  "A Linux source record cannot recheck through the retained Emacs source.
The synthetic birth is deliberately an Emacs time value: without the source
comparison, the current-source recheck would accept this otherwise equal
tuple."
  (let* ((system-type 'darwin)
         (attrs (dorg-test-evidence--raw-attrs))
         (identity (list :pid 5105 :source 'linux-proc
                         :birth dorg-test-evidence--start
                         :args (cdr (assq 'args attrs)))))
    (cl-letf (((symbol-function 'process-attributes) (lambda (_) attrs)))
      ;; Control: the matching retained-source record is held.
      (should (dorg-dolt-server--identity-held-p
               (plist-put (copy-sequence identity) :source 'emacs-start)))
      ;; The otherwise equal birth must still fail across source kinds.
      (should-not (dorg-dolt-server--identity-held-p identity)))))

;;; ─── One coherent identity across two observations ─────────────────────

(ert-deftest dorg-test-evidence-torn-identity-is-unknown ()
  "A process identity that changes between observations never refuses.
Covers a start-time change after the files probe, an exec (argv change),
and a socket owner that changed before the re-check."
  (let ((system-type 'darwin))
    (dorg-test-evidence--with-root root
      (let* ((owner (dorg-test-evidence--store root "owner/dorg"))
           (copy (dorg-test-evidence--store root "copy/dorg"))
           (paths (list :locks (list (dorg-test-evidence--lock owner)) :cwd nil))
           (reads 0)
           (second nil))
      ;; Recorded pid: start time changes after the files probe.
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (_)
                   (cl-incf reads)
                   (if (> reads 1) second (dorg-test-evidence--raw-attrs))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4106))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) paths)))
        ;; Control: a held identity DOES decide.
        (setq second (dorg-test-evidence--raw-attrs))
        (should (eq 'contradictory
                    (dorg-dolt-server--claim-store-evidence
                     copy '((:server-pid . 4106) (:port . 43206)))))
        (should (= reads 2))
        (setq reads 0
              second (dorg-test-evidence--raw-attrs '(26000 101 0 0)))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                             copy '((:server-pid . 4106) (:port . 43206)))))
        (should (= reads 2))
        (setq reads 0
              second `((comm . "dolt") (args . "dolt sql-server --port 1")
                       (start . ,dorg-test-evidence--start)))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                             copy '((:server-pid . 4106) (:port . 43206))))))
      ;; Socket owner: the port changes hands between the two observations.
      (let ((owners (list 4107 4108)))
        (cl-letf (((symbol-function 'process-attributes)
                   (dorg-test-evidence--attrs-table
                    `((4107 . ,(dorg-test-evidence--raw-attrs)))))
                  ((symbol-function 'dorg-dolt-server--probe-served-paths)
                   (lambda (_) paths))
                  ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                   (lambda (_) (if (cdr owners) (pop owners) (car owners)))))
          (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                               copy '((:server-pid . 4999) (:port . 43207)))))
          (should (equal owners '(4108)))
          ;; Control: a stable owner decides.
          (setq owners (list 4107))
          (should (eq 'contradictory
                      (dorg-dolt-server--claim-store-evidence
                       copy '((:server-pid . 4999) (:port . 43207)))))))))))

(ert-deftest dorg-test-evidence-no-start-time-is-no-identity ()
  "Without a process start time there is no identity to hold: absent, unprobed."
  ;; This is the retained non-Linux source: Linux requires proc birth rather
  ;; than an Emacs start time, and has its own missing-birth control above.
  (let ((system-type 'darwin))
    (dorg-test-evidence--with-root root
      (let ((copy (dorg-test-evidence--store root "copy/dorg"))
            (forbidden (list nil)))
        (cl-letf (((symbol-function 'process-attributes)
                   (lambda (_) '((comm . "dolt") (args . "dolt sql-server"))))
                  ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                   (lambda (_) 4109))
                  ((symbol-function 'dorg-dolt-server--probe-served-paths)
                   (dorg-test-evidence--refuse forbidden "the files probe")))
          (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                               copy '((:server-pid . 4109) (:port . 43209))))))
        (should-not (car forbidden))))))

;;; ─── The two named cases ────────────────────────────────────────────────

(ert-deftest dorg-test-evidence-copied-native-without-argv-never-dials ()
  "Copied native metadata naming a raw server of ANOTHER store never dials it.
Two isolated same-basename stores; the copy's sql-server.info names the
owner's raw server, whose argv has no --data-dir.  Held locks alone refuse
it before any endpoint contact, and the owner still attaches."
  (dorg-test-evidence--with-root root
    (let* ((owner (dorg-test-evidence--store root "owner/.dorg/dorg"))
           (copy (dorg-test-evidence--store root "copy/.dorg/dorg"))
           (port 43210)
           (contacts 0)
           (network-calls 0)
           (probed 0))
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-read) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (&rest _) (list :pid 4110 :port port :uuid "copied")))
                ((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4110 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4110))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_)
                   (cl-incf probed)
                   (list :locks (list (dorg-test-evidence--lock owner)) :cwd nil)))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) (cl-incf contacts) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (&rest _) (cl-incf contacts) port))
                ((symbol-function 'make-network-process)
                 (lambda (&rest _)
                   (cl-incf network-calls)
                   (error "Copied native metadata reached the network primitive"))))
        (dotimes (_ 2)
          (should-error (dorg-dolt-server--strict-port copy)
                        :type 'dorg-sql-transport-error)
          (should (= contacts 0))
          (should (= network-calls 0)))
        (should (> probed 0))
        ;; The real owner is proven and attaches.
        (should (= port (dorg-dolt-server--strict-port owner)))
        (should (= contacts 2))))))

(ert-deftest dorg-test-evidence-dead-or-replaced-pid-defers-to-socket-owner ()
  "A dead or replaced recorded pid is judged by the port's current owner.
The owner serving another store is contradictory, the requested one proven."
  (dorg-test-evidence--with-root root
    (let* ((served (dorg-test-evidence--store root "served/dorg"))
           (copy (dorg-test-evidence--store root "copy/dorg"))
           (claim '((:server-pid . 4111) (:port . 43211)))
           (recorded nil)
           (asked-port nil))
      (dolist (recorded-attrs
               (list nil                ; dead
                     `((comm . "vim") (args . "vim notes.txt")
                       (start . ,dorg-test-evidence--start)))) ; replaced
        (setq recorded recorded-attrs
              asked-port nil)
        (cl-letf (((symbol-function 'process-attributes)
                   (lambda (pid)
                     (pcase pid
                       (4111 recorded)
                       (4112 (dorg-test-evidence--raw-attrs)))))
                  ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                   (lambda (port) (push port asked-port) 4112))
                  ((symbol-function 'dorg-dolt-server--probe-served-paths)
                   (lambda (pid)
                     (should (eql pid 4112))
                     (list :locks (list (dorg-test-evidence--lock served)) :cwd nil))))
          (let ((verdict (dorg-dolt-server--claim-store-verdict copy claim)))
            (should (eq 'contradictory (plist-get verdict :evidence)))
            (should (eq 'socket-owner (plist-get verdict :source)))
            (should (eql 4112 (plist-get verdict :pid))))
          (should (equal asked-port '(43211 43211)))
          (should (eq 'proven (dorg-dolt-server--claim-store-evidence served claim))))))))

(ert-deftest dorg-test-evidence-reused-pid-same-type-defers-to-socket-owner ()
  "A recorded pid reused by ANOTHER raw Dolt server never decides.
The claim records pid 4131, now a raw `dolt sql-server' serving a foreign
store; the claimed port is owned by pid 4132, which serves the requested
store.  The port owner decides (proven).  When the port owner cannot be
resolved, the answer is absent, never a refusal built from the reused pid."
  (dorg-test-evidence--with-root root
    (let* ((requested (dorg-test-evidence--store root "requested/dorg"))
           (foreign (dorg-test-evidence--store root "foreign/dorg"))
           (claim '((:server-pid . 4131) (:port . 43231)))
           (owner 4132)
           (socket-calls 0)
           (probed nil))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4131 . ,(dorg-test-evidence--raw-attrs '(26000 131 0 0)))
                    (4132 . ,(dorg-test-evidence--raw-attrs '(26000 132 0 0))))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (port)
                   (should (eql port 43231))
                   (cl-incf socket-calls)
                   owner))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (pid)
                   (push pid probed)
                   (list :locks (list (dorg-test-evidence--lock
                                       (if (eql pid 4131) foreign requested)))
                         :cwd nil))))
        (let ((verdict (dorg-dolt-server--claim-store-verdict requested claim)))
          (should (eq 'proven (plist-get verdict :evidence)))
          (should (eq 'socket-owner (plist-get verdict :source)))
          (should (eql 4132 (plist-get verdict :pid))))
        (should (= socket-calls 2))
        (should (equal probed '(4132)))
        (should (dorg-dolt-server--assert-claim-not-foreign-store requested claim))
        ;; Unresolvable port owner: absent, and the reused pid is never probed.
        (dolist (unresolved '(unknown nil))
          (setq owner unresolved
                probed nil)
          (should (eq 'absent
                      (dorg-dolt-server--claim-store-evidence requested claim)))
          (should (eq 'absent
                      (dorg-dolt-server--claim-store-evidence foreign claim)))
          (should-not probed))))))

;;; ─── What must not regress ──────────────────────────────────────────────

(ert-deftest dorg-test-evidence-raw-native-own-store-is-proven ()
  "A raw server over its OWN store is proven (previously absent) and attaches."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "raw/.dorg/dorg"))
           (claim '((:server-pid . 4113) (:port . 43213)))
           (attached 0))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4113 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4113))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) (list :locks (list (dorg-test-evidence--lock store))
                                   :cwd nil)))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) claim))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--attach-live)
                 (lambda (&rest _) (cl-incf attached) 43213)))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence store claim)))
        (should (= 43213 (dorg-dolt-server--strict-port store)))
        (should (= attached 1))))))

(ert-deftest dorg-test-evidence-dead-pid-without-listener-keeps-lifecycle ()
  "A dead pid with no socket owner stays absent and reaches dead-claim recovery."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "dead/dorg"))
           (forbidden (list nil))
           (claim '((:server-pid . 4114) (:port . 43214)))
           (asked 0)
           (classified 0)
           (broken 0)
           (started 0))
      (cl-letf (((symbol-function 'process-attributes) (lambda (_) nil))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) (cl-incf asked) 'unknown))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (dorg-test-evidence--refuse forbidden "the files probe"))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) claim))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) (cl-incf classified) 'dead))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (cl-incf broken)))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (cl-incf started) 43215)))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence store claim)))
        (should (= asked 1))
        (should (= 43215 (dorg-dolt-server--ensure-strict store)))
        (should (= classified 1))
        (should (= broken 1))
        (should (= started 1)))
      (should-not (car forbidden)))))

(ert-deftest dorg-test-evidence-explicit-data-dir-unchanged-and-unprobed ()
  "Explicit --data-dir keeps its exact answers and consults no kernel probe."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "explicit/dorg"))
           (other (dorg-test-evidence--store root "other/dorg"))
           (claim '((:server-pid . 4116) (:port . 43216)))
           (forbidden (list nil)))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4116 . ,(dorg-test-evidence--explicit-attrs
                              (dorg-test-evidence--canon store))))))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (dorg-test-evidence--refuse forbidden "the files probe"))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (dorg-test-evidence--refuse forbidden "the socket probe")))
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence store claim)))
        (should (eq 'contradictory
                    (dorg-dolt-server--claim-store-evidence other claim)))
        (should (eq 'argv (plist-get (dorg-dolt-server--claim-store-verdict
                                      other claim)
                                     :source))))
      (should-not (car forbidden)))))

;;; ─── Fail-safe: every probe failure is absent, never a refusal ─────────

(defun dorg-test-evidence--failing-run (mode foreign-lock)
  "Return a `call-process' stand-in that fails in MODE, and a call counter.
MODE is `nonzero', `signalled', `unparseable' or `ambiguous'.  A nonzero or
signalled run still prints well-formed output naming FOREIGN-LOCK (and, for a
port query, a listening pid), so only its exit status keeps it from deciding."
  (let ((calls (list 0)))
    (cons (lambda (_program _infile destination _display &rest args)
            (cl-incf (car calls))
            (let ((out (if (consp destination) (car destination) destination))
                  (pid (cadr (member "-p" args))))
              (when (and (eq out t) (memq mode '(nonzero signalled)))
                (insert (if pid
                            (format "p%s\nfcwd\nn/\nf5\nn%s\n" pid foreign-lock)
                          "p4199\nf5\n")))
              (pcase mode
                ('nonzero 1)
                ('signalled "Killed: 9")
                ('unparseable
                 (when (eq out t) (insert "garbage line\n"))
                 0)
                ('ambiguous
                 (when (eq out t) (insert "p4120\nfcwd\nn/tmp\np4121\nf3\nn/tmp\n"))
                 0))))
          calls)))

(ert-deftest dorg-test-evidence-every-probe-failure-is-absent ()
  "Missing, nonzero, signalled, unparseable, ambiguous and refused probes.
Each scenario asserts its probe actually ran, so a scenario whose probe was
skipped cannot pass, and each leaves the case absent with no refusal."
  (dorg-test-evidence--with-root root
    (let* ((copy (dorg-test-evidence--store root "copy/dorg"))
           (owner (dorg-test-evidence--store root "owner/dorg"))
           (claim '((:server-pid . 4120) (:port . 43220)))
           (attrs (dorg-test-evidence--raw-attrs))
           (socket-probe (symbol-function 'dorg-dolt-server--probe-socket-owner)))
      ;; Files probe through the lsof backend once the recorded pid owns the
      ;; port, and the socket probe itself (dead recorded pid).
      (dolist (path '(recorded-pid socket-owner))
        (dolist (mode '(missing nonzero signalled unparseable ambiguous))
          (ert-info ((format "%s / %s" path mode))
            (let* ((run (dorg-test-evidence--failing-run
                         mode (dorg-test-evidence--lock owner)))
                   (found 0))
              (cl-letf (((symbol-function 'process-attributes)
                         (lambda (pid)
                           (if (eq path 'recorded-pid)
                               attrs
                             (and (eql pid 4199) attrs))))
                        ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                         (if (eq path 'recorded-pid)
                             (lambda (_) 4120)
                           socket-probe))
                        ((symbol-function 'dorg-dolt-server--probe-served-paths)
                         #'dorg-dolt-server--lsof-served-paths)
                        ((symbol-function 'executable-find)
                         (lambda (name)
                           (should (equal name "lsof"))
                           (cl-incf found)
                           (unless (eq mode 'missing) "/usr/sbin/lsof")))
                        ((symbol-function 'call-process) (car run)))
                (when (eq path 'socket-owner)
                  (setq claim '((:server-pid . 4198) (:port . 43220))))
                (should (eq 'absent
                            (dorg-dolt-server--claim-store-evidence copy claim)))
                (should (= found 1))
                (should (= (car (cdr run)) (if (eq mode 'missing) 0 1)))
                (should (dorg-dolt-server--assert-claim-not-foreign-store
                         copy claim)))))))
      ;; Permission refused on procfs: an unreadable descriptor table.
      (let ((proc-root (file-name-as-directory (expand-file-name "proc" root))))
        (make-directory (expand-file-name "4120" proc-root) t)
        (should (eq 'unknown
                    (dorg-dolt-server--proc-served-paths 4120 proc-root)))
        (cl-letf (((symbol-function 'process-attributes) (lambda (_) attrs))
                  ((symbol-function 'dorg-dolt-server--probe-served-paths)
                   (lambda (pid) (dorg-dolt-server--proc-served-paths pid proc-root))))
          (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                               copy '((:server-pid . 4120) (:port . 43220))))))))))

(ert-deftest dorg-test-evidence-probe-absent-on-platform-is-absent ()
  "No procfs and no lsof: both probes answer unknown and nothing is refused."
  (dorg-test-evidence--with-root root
    (let ((copy (dorg-test-evidence--store root "copy/dorg"))
          (looked nil))
      (cl-letf (((symbol-function 'file-directory-p)
                 (let ((real (symbol-function 'file-directory-p)))
                   (lambda (f)
                     (if (equal f "/proc/self/fd")
                         (progn (push 'procfs looked) nil)
                       (funcall real f)))))
                ((symbol-function 'executable-find)
                 (lambda (name) (push (intern name) looked) nil))
                ((symbol-function 'process-attributes)
                 (lambda (_) (dorg-test-evidence--raw-attrs))))
        (should (eq 'unknown (dorg-dolt-server--probe-served-paths 4130)))
        (should (eq 'unknown (dorg-dolt-server--probe-socket-owner 43230)))
        (should (memq 'procfs looked))
        (should (memq 'lsof looked))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence
                             copy '((:server-pid . 4130) (:port . 43230)))))))))

;;; ─── Cost and blast radius ──────────────────────────────────────────────

(ert-deftest dorg-test-evidence-warm-connection-never-probes ()
  "A validated attachment on the query path performs no probe of any kind."
  (dorg-test-evidence--with-root root
    (let ((store (dorg-test-evidence--store root "warm/dorg"))
          (forbidden (list nil)))
      (puthash (dorg-test-evidence--canon store) 43240
               dorg-dolt-server--attached-ports)
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-store-verdict)
                 (dorg-test-evidence--refuse forbidden "the store verdict"))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (dorg-test-evidence--refuse forbidden "the files probe"))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (dorg-test-evidence--refuse forbidden "the socket probe"))
                ((symbol-function 'process-attributes)
                 (dorg-test-evidence--refuse forbidden "process-attributes")))
        (dotimes (_ 3)
          (should (= 43240 (dorg-dolt-server--strict-port store)))))
      (should-not (car forbidden)))))

(ert-deftest dorg-test-evidence-probe-invocations-are-scoped ()
  "Every probe invocation is scoped to one pid or one port."
  (let (invocations)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/sbin/lsof"))
              ((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest args)
                 (push args invocations)
                 1)))
      (dorg-dolt-server--lsof-served-paths 4150)
      (dorg-dolt-server--probe-socket-owner 43250))
    (should (= 2 (length invocations)))
    (dolist (args invocations)
      (ert-info ((format "argv %S" args))
        (should (or (and (member "-a" args)
                         (equal "4150" (cadr (member "-p" args))))
                    (member "-iTCP:43250" args)))))))

(ert-deftest dorg-test-evidence-refusal-changes-nothing ()
  "Refusal deletes, signals, repairs and persists nothing."
  (dorg-test-evidence--with-root root
    (let* ((owner (dorg-test-evidence--store root "owner/dorg"))
           (copy (dorg-test-evidence--store root "copy/dorg"))
           (claim '((:server-pid . 4160) (:port . 43260)))
           (before (dorg-test-evidence--tree root))
           (forbidden (list nil)))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4160 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4160))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) (list :locks (list (dorg-test-evidence--lock owner))
                                   :cwd nil)))
                ((symbol-function 'signal-process)
                 (dorg-test-evidence--refuse forbidden "signal-process"))
                ((symbol-function 'dorg-dolt-server--kill-pid)
                 (dorg-test-evidence--refuse forbidden "a pid kill"))
                ((symbol-function 'delete-file)
                 (dorg-test-evidence--refuse forbidden "delete-file"))
                ((symbol-function 'delete-directory)
                 (dorg-test-evidence--refuse forbidden "delete-directory"))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (dorg-test-evidence--refuse forbidden "breaking the claim"))
                ((symbol-function 'dorg-dolt-server--claim-write)
                 (dorg-test-evidence--refuse forbidden "writing a claim"))
                ((symbol-function 'dorg-dolt-server--index-register)
                 (dorg-test-evidence--refuse forbidden "registering the index")))
        (should-error (dorg-dolt-server--assert-claim-not-foreign-store copy claim)
                      :type 'dorg-sql-transport-error)
        (should (eq 'proven (dorg-dolt-server--claim-store-evidence owner claim))))
      (should (equal before (dorg-test-evidence--tree root)))
      (should-not (car forbidden)))))

;;; ─── Diagnostics ────────────────────────────────────────────────────────

(ert-deftest dorg-test-evidence-refusal-diagnostic-names-everything ()
  "A refusal names the store, what is served, pid, port, evidence and recovery."
  (dorg-test-evidence--with-root root
    (let* ((owner (dorg-test-evidence--store root "owner/dorg"))
           (copy (dorg-test-evidence--store root "copy/dorg"))
           (claim '((:server-pid . 4170) (:port . 43270))))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table
                  `((4170 . ,(dorg-test-evidence--raw-attrs)))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4170))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) (list :locks (list (dorg-test-evidence--lock owner))
                                   :cwd nil))))
        (let ((message (cadr (should-error
                              (dorg-dolt-server--assert-claim-not-foreign-store
                               copy claim)
                              :type 'dorg-sql-transport-error))))
          (dolist (needle (list (dorg-test-evidence--canon copy)
                                (dorg-test-evidence--canon owner)
                                "pid 4170" "port 43270"
                                "store locks held open by the claim's recorded pid"
                                "Manual recovery"
                                "sql-server.info" "server.claim"
                                "Nothing was deleted, signalled or repaired"))
            (ert-info ((format "diagnostic lacks %S: %s" needle message))
              (should (string-search needle message)))))))))

;;; ─── Probes against the real kernel and pure parsers ───────────────────

(ert-deftest dorg-test-evidence-parse-lsof-files ()
  "Parse lsof -Ffn: held store locks and cwd, with Dolt internals excluded."
  (let ((lines '("p4180" "fcwd" "n/srv/d" "ftxt" "n/usr/bin/dolt"
                 "f5" "n/srv/d/a/.dolt/noms/LOCK"
                 "f6" "n/srv/d/a/.dolt/stats/.dolt/noms/LOCK"
                 "f7" "n/srv/d/b with space/.dolt/noms/LOCK")))
    (should (equal (dorg-dolt-server--parse-lsof-files 4180 lines)
                   '(:locks ("/srv/d/a/.dolt/noms/LOCK"
                             "/srv/d/b with space/.dolt/noms/LOCK")
                     :cwd "/srv/d")))
    (should (eq 'unknown (dorg-dolt-server--parse-lsof-files 4181 lines)))
    (should (eq 'unknown (dorg-dolt-server--parse-lsof-files
                          4180 (append lines '("p4182")))))
    (should (eq 'unknown (dorg-dolt-server--parse-lsof-files
                          4180 (append lines '("fcwd" "n/elsewhere")))))
    (should (eq 'unknown (dorg-dolt-server--parse-lsof-files
                          4180 (cons "zjunk" lines))))))

(ert-deftest dorg-test-10z7y-admission-census-is-store-scoped-and-strict ()
  "Admission sees only exact LOCK holders and confirms their full argv.
The file-scoped lsof adapter makes the no-holder result usable.  A probe
failure is indeterminate, whereas one proven sql-server holding the canonical
lock is a conflict candidate.  This is server-free: kernel observations are
injected."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "owner/.dorg/dorg"))
           (lock (dorg-test-evidence--lock store))
           (attrs (dorg-test-evidence--explicit-attrs
                   (dorg-test-evidence--canon store)))
           (args nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--run-probe)
                 (lambda (program &rest argv)
                   (setq args (cons program argv))
                   (list "p4251" "f7" (concat "n" lock)
                         "p4252" "f0" "n/dev/null")))
                ((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table `((4251 . ,attrs)))))
        (should (equal '(4251)
                       (dorg-dolt-server--admission-lock-holders store)))
        (should (equal (list "sh" "-c" dorg-dolt-server--admission-lsof-script
                             "dorg-admission-lsof" lock)
                       args)))
      (cl-letf (((symbol-function 'dorg-dolt-server--run-probe)
                 (lambda (&rest _) 'unknown)))
        (should (eq 'unknown (dorg-dolt-server--admission-lock-holders store))))
      (cl-letf (((symbol-function 'dorg-dolt-server--run-probe)
                 (lambda (&rest _) '("p4252" "f0" "n/dev/null"))))
        (should (null (dorg-dolt-server--admission-lock-holders store)))))))

(ert-deftest dorg-test-10z7y-admission-does-not-change-permissive-verdict ()
  "A failed admission probe is strict without changing legacy verdict callers.
The existing claim reader still maps missing kernel evidence to `absent'; only
the new launch admission reports `unknown' and blocks a new managed start."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "owner/.dorg/dorg"))
           (claim '((:server-pid . 4253) (:port . 43253)))
           (attrs (dorg-test-evidence--raw-attrs)))
      (cl-letf (((symbol-function 'process-attributes)
                 (dorg-test-evidence--attrs-table `((4253 . ,attrs))))
                ((symbol-function 'dorg-dolt-server--probe-socket-owner)
                 (lambda (_) 4253))
                ((symbol-function 'dorg-dolt-server--probe-served-paths)
                 (lambda (_) 'unknown))
                ((symbol-function 'dorg-dolt-server--run-probe)
                 (lambda (&rest _) 'unknown)))
        (should (eq 'absent (dorg-dolt-server--claim-store-evidence store claim)))
        (should (eq 'unknown (dorg-dolt-server--admission-lock-holders store)))))))

(ert-deftest dorg-test-10z7y-admission-census-empty-lock-is-clear ()
  "A real file-scoped lsof observation of an unopened owned LOCK is empty."
  (skip-unless (executable-find "lsof"))
  (dorg-test-evidence--with-root root
    (let ((store (dorg-test-evidence--store root "empty/.dorg/dorg")))
      (should (null (dorg-dolt-server--admission-lock-holders store))))))

(ert-deftest dorg-test-10z7y-admission-census-absent-lock-is-clear ()
  "A fresh store with no noms LOCK is clear without an indeterminate probe."
  (dorg-test-evidence--with-root root
    (let* ((store (expand-file-name "absent/.dorg/dorg" root))
           (probes 0))
      (make-directory (expand-file-name ".dolt/noms" store) t)
      (cl-letf (((symbol-function 'dorg-dolt-server--run-probe)
                 (lambda (&rest _)
                   (cl-incf probes)
                   (ert-fail "an absent LOCK must not be probed"))))
        (should (null (dorg-dolt-server--admission-lock-holders store)))
        (should (= probes 0))))))

(ert-deftest dorg-test-10z7y-admission-precedes-both-launch-seams ()
  "Both managed launch primitives call strict admission before any spawn work.
The census is made to refuse, so each primitive must stop at that call rather
than reaching store validation, claim publication, or either launch helper."
  (dorg-test-evidence--with-root root
    (let ((store (dorg-test-evidence--store root "seams/.dorg/dorg"))
          (calls 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--assert-admission-clear)
                 (lambda (dir)
                   (should (equal (dorg-test-evidence--canon store)
                                  (dorg-test-evidence--canon dir)))
                   (cl-incf calls)
                   (signal 'dorg-sql-transport-error '("admission sentinel"))))
                ((symbol-function 'dorg-dolt-server--expected-binding)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                 (lambda (&rest _) t)))
        (should-error (dorg-dolt-server--start-child-unsafe store t)
                      :type 'dorg-sql-transport-error)
        (should-error (dorg-dolt-server--start-detached-unsafe store)
                      :type 'dorg-sql-transport-error)
        (should (= calls 2))))))

(ert-deftest dorg-test-10z7y-indeterminate-admission-refuses-before-spawn ()
  "Missing or failed owner evidence blocks both launch forms before spawn.
This is the T1 missing-tool/nonzero-probe behavior at the shared probe seam:
the injected `unknown' may not degrade to the legacy permissive verdict."
  (dorg-test-evidence--with-root root
    (let ((store (dorg-test-evidence--store root "indeterminate/.dorg/dorg"))
          (spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--admission-lock-holders)
                 (lambda (&rest _) 'unknown))
                ((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) (ert-fail "must not spawn")))
                ((symbol-function 'dorg-dolt-server--expected-binding)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--detached-launch)
                 (lambda (&rest _) (cl-incf spawns) (ert-fail "must not launch"))))
        (dolist (thunk (list (lambda () (dorg-dolt-server--start-child-unsafe store nil))
                             (lambda () (dorg-dolt-server--start-detached-unsafe store))))
          (let ((err (should-error (funcall thunk) :type 'dorg-sql-transport-error)))
            (should (string-match-p "owner census.*indeterminate" (cadr err)))))
        (should (= spawns 0))))))

(ert-deftest dorg-test-10z7y-proven-owner-conflict-names-pid ()
  "A proven LOCK owner refuses a managed launch with its PID in the conflict."
  (dorg-test-evidence--with-root root
    (let ((store (dorg-test-evidence--store root "held/.dorg/dorg")))
      (cl-letf (((symbol-function 'dorg-dolt-server--admission-lock-holders)
                 (lambda (&rest _) '(4261))))
        (let ((err (should-error (dorg-dolt-server--assert-admission-clear store)
                                 :type 'dorg-sql-transport-conflict)))
          (should (string-match-p "pid 4261" (cadr err))))))))

(ert-deftest dorg-test-evidence-parse-socket-owner ()
  "The socket probe answers exactly one listening pid, else unknown."
  (dolist (case '((("p79208" "f161") . 79208)
                  (("p79208" "f161" "p79208" "f162") . 79208)
                  (("p79208" "f161" "p79209" "f162") . unknown)
                  (("p0" "f1") . unknown)
                  (("q1") . unknown)
                  (() . unknown)))
    (cl-letf (((symbol-function 'dorg-dolt-server--run-probe)
               (lambda (&rest _) (car case))))
      (ert-info ((format "%S" (car case)))
        (should (equal (cdr case)
                       (dorg-dolt-server--probe-socket-owner 43290)))))))

(ert-deftest dorg-test-evidence-procfs-reader ()
  "The procfs backend reads PID/cwd and PID/fd links under a given root."
  (dorg-test-evidence--with-root root
    (let* ((store (dorg-test-evidence--store root "d/a"))
           (proc-root (file-name-as-directory (expand-file-name "proc" root)))
           (pid-dir (expand-file-name "4190" proc-root))
           (fd-dir (expand-file-name "fd" pid-dir))
           (lock (dorg-test-evidence--lock store))
           (stats (expand-file-name ".dolt/stats/.dolt/noms/LOCK" store)))
      (make-directory fd-dir t)
      (make-symbolic-link (directory-file-name store) (expand-file-name "cwd" pid-dir))
      (make-symbolic-link lock (expand-file-name "5" fd-dir))
      (make-symbolic-link stats (expand-file-name "6" fd-dir))
      (make-symbolic-link "/dev/null" (expand-file-name "0" fd-dir))
      (should (equal (dorg-dolt-server--proc-served-paths 4190 proc-root)
                     (list :locks (list lock) :cwd (directory-file-name store))))
      (should (eq 'unknown (dorg-dolt-server--proc-served-paths 4191 proc-root))))))

(defun dorg-test-evidence--raw-server-observation (&optional force-mismatch)
  "Return one real raw-server observation, optionally forcing a torn recheck.
FORCE-MISMATCH changes only the second classifier-local test seam read.  The
real child, file probe, socket-owner seam, and production classifier remain
otherwise unchanged."
  (let ((dorg-test-evidence--use-real-linux-proc-birth t))
    (dorg-test-evidence--with-root root
      (let* ((store (dorg-test-evidence--store root "owner/.dorg/dorg"))
           (copy (dorg-test-evidence--store root "copy/.dorg/dorg"))
           (alias (expand-file-name "alias" root))
           (binary (expand-file-name "dolt" root))
           (shell (or (executable-find "sh") "/bin/sh"))
           proc reads identity-held socket-owner paths verdict copy-evidence)
      (make-symbolic-link (directory-file-name store) alias)
      (make-symbolic-link shell binary)
      (unwind-protect
          (let ((default-directory (file-name-as-directory alias)))
            (setq proc (make-process
                        :name "dorg-test-evidence-raw"
                        :command (list binary "-c"
                                       "exec 3<\"$1\"; while :; do sleep 1; done"
                                       "sql-server"
                                       (dorg-test-evidence--lock
                                        (file-name-as-directory alias)))
                        :noquery t))
            (let ((deadline (+ (float-time) 5)))
              ;; The descriptor opens after exec; wait for it to be visible.
              (while (and (< (float-time) deadline)
                          (not (and (consp (setq paths (dorg-dolt-server--probe-served-paths
                                                        (process-id proc))))
                                    (plist-get paths :locks))))
                (accept-process-output proc 0.05)))
            ;; The port binding is injected: the child owns no socket.  The
            ;; files probe, identity and served set below are the real kernel.
            (let ((claim (list (cons :server-pid (process-id proc))
                               (cons :port 43299)))
                  (real-process-attributes (symbol-function 'process-attributes))
                  (real-linux-proc-birth
                   (symbol-function 'dorg-dolt-server--linux-proc-birth))
                  (real-identity-held-p
                   (symbol-function 'dorg-dolt-server--identity-held-p))
                  (read-count 0)
                  (birth-read-count 0))
              (cl-letf (((symbol-function 'dorg-dolt-server--probe-socket-owner)
                         (lambda (_)
                           (setq socket-owner (process-id proc)))) )
                (cl-letf (((symbol-function 'process-attributes)
                           (lambda (pid)
                             (let ((attrs (funcall real-process-attributes pid)))
                               (if (not (= pid (process-id proc)))
                                   attrs
                                 (cl-incf read-count)
                                 (when (and force-mismatch (= read-count 2))
                                   (setq attrs
                                         (dorg-test-evidence--raw-server-mismatched-attrs
                                          attrs)))
                                 (push (dorg-test-evidence--raw-server-read
                                        read-count pid attrs)
                                       reads)
                                 attrs))))
                          ((symbol-function 'dorg-dolt-server--linux-proc-birth)
                           (lambda (pid)
                             (let ((birth (funcall real-linux-proc-birth pid)))
                               (cl-incf birth-read-count)
                               ;; Linux identity deliberately ignores the
                               ;; retained Emacs `:start' field.  The forced
                               ;; raw packet must therefore tear its actual
                               ;; proc birth tuple on the recheck.
                               (if (and force-mismatch
                                        (eq system-type 'gnu/linux)
                                        (= birth-read-count 2)
                                        birth)
                                   (list (car birth)
                                         (if (equal (cadr birth) "0") "1" "0"))
                                 birth))))
                          ((symbol-function 'dorg-dolt-server--identity-held-p)
                           (lambda (identity)
                             (setq identity-held
                                   (funcall real-identity-held-p identity)))))
                  (setq verdict (dorg-dolt-server--claim-store-verdict store claim)))
                ;; Preserve the original copy-store contradiction control
                ;; outside the diagnostic wrappers: this packet describes
                ;; exactly the primary classifier invocation.
                (setq copy-evidence
                      (dorg-dolt-server--claim-store-evidence copy claim))))
            (setq reads (nreverse reads))
            (let ((packet (list :tag 'dorg-raw-server-evidence
                                :pid (process-id proc)
                                :reads reads
                                :start-difference
                                (dorg-test-evidence--raw-server-start-difference reads)
                                :identity-held identity-held
                                :paths paths
                                :socket-owner socket-owner
                                :verdict verdict
                                :copy-evidence copy-evidence)))
              ;; This is intentionally emitted on success as well as failure.
              (message "DORG_RAW_SERVER_EVIDENCE %S" packet)
              ;; Keep the existing readiness assertions, but after the packet
              ;; so a slow or missing descriptor still leaves its evidence.
              (should (consp paths))
              (should (equal (dorg-test-evidence--canon (plist-get paths :cwd))
                             (dorg-test-evidence--canon store)))
              (should (equal (mapcar (lambda (lock)
                                       (dorg-test-evidence--canon
                                        (dorg-dolt-server--lock-store lock)))
                                     (plist-get paths :locks))
                             (list (dorg-test-evidence--canon store))))
              packet))
        (when (and proc (process-live-p proc))
          (delete-process proc)))))))

(ert-deftest dorg-test-evidence-proc-stat-starttime-parser ()
  "The diagnostic parser selects field 22 after a parenthesized command."
  (should (equal "424242"
                 (dorg-test-evidence--proc-stat-starttime-from-string
                  "123 (raw child) S 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 424242 20\n")))
  (should-not (dorg-test-evidence--proc-stat-starttime-from-string
               "123 raw child S 0")))

(ert-deftest dorg-test-evidence-raw-server-repeat-is-bounded ()
  "The raw-server repeat variable defaults to one and rejects unsafe values."
  (let ((process-environment
         (cl-remove-if (lambda (entry)
                         (string-prefix-p "DORG_RAW_SERVER_EVIDENCE_REPEAT=" entry))
                       process-environment)))
    (should (= 1 (dorg-test-evidence--raw-server-repeat-count))))
  (dolist (value '("0" "51" "-1" "1.5" "many" "9223372036854775808"))
    (let ((process-environment
           (cons (concat "DORG_RAW_SERVER_EVIDENCE_REPEAT=" value)
                 process-environment)))
      (should-error (dorg-test-evidence--raw-server-repeat-count)
                    :type 'ert-test-failed)))
  (let ((process-environment
         (cons "DORG_RAW_SERVER_EVIDENCE_REPEAT=50" process-environment)))
    (should (= 50 (dorg-test-evidence--raw-server-repeat-count)))))

(ert-deftest dorg-test-evidence-raw-server-repeat-launcher-refuses-unpaired-values ()
  "The existing suite-job launcher rejects invalid or unpaired repeat input."
  (dolist (arguments
           (list (list "--raw-server-evidence-repeat" "1")
                 (list "--selector" "^some-other-test$"
                       "--raw-server-evidence-repeat" "1")
                 (list "--selector" dorg-test-evidence--raw-server-selector
                       "--raw-server-evidence-repeat" "51")
                 (list "--selector" dorg-test-evidence--raw-server-selector
                       "--raw-server-evidence-repeat" "9223372036854775808")))
    (pcase-let ((`(,status . ,output)
                 (apply #'dorg-test-evidence--suite-job-refusal arguments)))
      (should (= 64 status))
      (should (string-match-p "refuse: --raw-server-evidence-repeat" output)))))

(ert-deftest dorg-test-evidence-raw-server-packet-reaches-torn-identity ()
  "A forced second read keeps the positive assertion false and is recorded."
  (skip-unless (or (file-directory-p "/proc/self/fd") (executable-find "lsof")))
  (let ((packet (dorg-test-evidence--raw-server-observation t)))
    (should-error
     (should (eq 'proven (plist-get (plist-get packet :verdict) :evidence)))
     :type 'ert-test-failed)
    (should-not (plist-get packet :identity-held))
    (should (= 2 (length (plist-get packet :reads))))
    (should (not (equal (plist-get (car (plist-get packet :reads)) :start)
                        (plist-get (cadr (plist-get packet :reads)) :start))))))

(ert-deftest dorg-test-evidence-real-kernel-raw-server-shape ()
  "Real kernel: a raw `dolt sql-server'-shaped child holding a store lock.
The child is a shell invoked through a symlink named dolt with argv
`sql-server' and no --data-dir; it opens no store and no socket.  Its cwd
and lock are set through a symlinked alias, so the probe must canonicalise
what it reads.  Its own store is proven; a same-basename copy is not."
  (skip-unless (or (file-directory-p "/proc/self/fd") (executable-find "lsof")))
  (dotimes (_ (dorg-test-evidence--raw-server-repeat-count))
    (let ((packet (dorg-test-evidence--raw-server-observation)))
      (ert-info ((format "DORG_RAW_SERVER_EVIDENCE %S" packet))
        (should (eq 'proven (plist-get (plist-get packet :verdict) :evidence)))
        (should (eq 'recorded-pid (plist-get (plist-get packet :verdict) :source)))
        (should (eq 'locks (plist-get (plist-get packet :verdict) :basis)))
        (should (eq 'contradictory (plist-get packet :copy-evidence)))))))

(ert-deftest dorg-test-evidence-real-kernel-socket-owner ()
  "Real kernel: the socket probe names the process listening on a port.
This Emacs opens a loopback listener; nothing connects to it.  A port with
no listener is unknown, never some other pid."
  (skip-unless (executable-find "lsof"))
  (let* ((listener (make-network-process
                    :name "dorg-test-evidence-listener" :host "127.0.0.1"
                    :service 0 :server t :family 'ipv4 :noquery t))
         (port (process-contact listener :service)))
    (unwind-protect
        (progn
          (should (eql (emacs-pid) (dorg-dolt-server--probe-socket-owner port)))
          (delete-process listener)
          (should (eq 'unknown (dorg-dolt-server--probe-socket-owner port))))
      (when (process-live-p listener)
        (delete-process listener)))))

;;; ─── Child spawn form (no Dolt) ────────────────────────────────────────

(ert-deftest dorg-test-dolt-server-spawn-logged-form ()
  "The child spawn is a pipe whose shell execs dolt with output in a file.
The command is the constant exec trampoline, the absolute dolt path and the
unchanged server argv; the log path travels only in the environment, and
the process has no buffer for Emacs to fill.  The trampoline keeps the
Emacs pipe open on fd 3, since Emacs blocks once a live child's output
pipe reaches end of file."
  (let* ((log "/nonexistent-dorg-spawn/.dorg/server.log")
         (captured nil) (env nil) (proc nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &rest _) (and (equal program "dolt") "/opt/bin/dolt")))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured plist env process-environment)
                 (make-pipe-process :name "dorg-test-spawn-form" :noquery t))))
      (setq proc (dorg-dolt-server--spawn-logged
                  "dorg-sql-server" '("sql-server" "--port" "1") log)))
    (unwind-protect
        (progn
          (should (eq 'pipe (plist-get captured :connection-type)))
          (should (plist-member captured :buffer))
          (should-not (plist-get captured :buffer))
          (should (equal (plist-get captured :command)
                         (list "/bin/sh" "-c"
                               "exec \"$0\" \"$@\" 3>&1 >\"$DORG_SERVER_LOG\" 2>&1"
                               "/opt/bin/dolt" "sql-server" "--port" "1")))
          (should (member (concat "DORG_SERVER_LOG=" log) env))
          (should-not (cl-some (lambda (arg) (string-match-p (regexp-quote log) arg))
                               (plist-get captured :command)))
          (should (equal log (process-get proc 'dorg-server-log))))
      (when (processp proc) (delete-process proc)))))

;;; ─── Child log diagnostics never gate lifecycle cleanup (no Dolt) ──────

(defun dorg-test--dead-child (key log &optional ready)
  "Return a dead process registered under KEY with LOG, READY when non-nil.
Its entry is in the let-bound `dorg-dolt-server--procs'."
  (let ((proc (make-pipe-process :name "dorg-test-child" :noquery t)))
    (process-put proc 'dorg-server-log log)
    (when ready (process-put proc 'dorg-server-ready t))
    (puthash key (cons proc 3300) dorg-dolt-server--procs)
    (delete-process proc)
    proc))

(ert-deftest dorg-test-dolt-server-sentinel-ready-exit-skips-log ()
  "A ready child's exit releases its registry entry and claim unread.
Its log grows for the server's whole life, so the sentinel must not load
it: only a death before readiness is a start failure worth a diagnostic."
  (let* ((log (make-temp-file "dorg-lifetime-log"))
         (dorg-dolt-server--procs (make-hash-table :test 'equal))
         (released nil) (reads 0)
         (proc (dorg-test--dead-child "/tmp/dorg-ready-child/" log t)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-dolt-server--release-claim)
                   (lambda (d) (push d released) t))
                  ((symbol-function 'insert-file-contents)
                   (lambda (&rest _) (cl-incf reads) (error "spec: lifetime log read"))))
          (dorg-dolt-server--child-exit-sentinel proc "finished\n")
          (should (equal released '("/tmp/dorg-ready-child/")))
          (should (= 0 (hash-table-count dorg-dolt-server--procs)))
          (should (= 0 reads))
          (should-not (process-get proc 'dorg-startup-output)))
      (delete-file log))))

(ert-deftest dorg-test-dolt-server-sentinel-early-exit-survives-log-error ()
  "A child dying before readiness is cleaned up even if its log is unreadable.
The failed read becomes a note in the startup diagnostic; it never stops
the registry entry and claim from being released."
  (let* ((log (make-temp-file "dorg-early-log"))
         (dorg-dolt-server--procs (make-hash-table :test 'equal))
         (released nil)
         (proc (dorg-test--dead-child "/tmp/dorg-early-child/" log)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-dolt-server--release-claim)
                   (lambda (d) (push d released) t))
                  ((symbol-function 'insert-file-contents)
                   (lambda (&rest _) (error "spec: log unreadable"))))
          (dorg-dolt-server--child-exit-sentinel proc "exited abnormally\n")
          (should (equal released '("/tmp/dorg-early-child/")))
          (should (= 0 (hash-table-count dorg-dolt-server--procs)))
          (should (string-match-p "server log unreadable"
                                  (process-get proc 'dorg-startup-output))))
      (delete-file log))))

(ert-deftest dorg-test-dolt-server-read-log-bounded-tail ()
  "A diagnostic reads only the tail of a large log, never the whole file."
  (let ((log (make-temp-file "dorg-big-log"))
        (spans nil)
        (orig (symbol-function 'insert-file-contents)))
    (unwind-protect
        (progn
          (with-temp-file log
            (dotimes (i 20000) (insert (format "log line %06d\n" i))))
          (let ((text (cl-letf (((symbol-function 'insert-file-contents)
                                 (lambda (file &optional visit beg end replace)
                                   (push (list beg end) spans)
                                   (funcall orig file visit beg end replace))))
                        (dorg-dolt-server--read-log log))))
            (should (= 1 (length spans)))
            (should (<= (- (nth 1 (car spans)) (nth 0 (car spans)))
                        dorg-dolt-server--log-diagnostic-bytes))
            (should (string-match-p "log line 019999" text))
            (should-not (string-match-p "log line 000000" text))))
      (delete-file log))))

(provide 'dorg-dolt-server-evidence-tests)
;;; dorg-dolt-server-evidence-tests.el ends here
