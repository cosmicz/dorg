;;; cert-alpha-large.el --- Certify the >10 MiB alpha path -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One bounded, reproducible acceptance certificate for tracked-case.  It drives
;; one byte-identical >10 MiB Org workload through the public plain-org and
;; standalone Dolt paths.  This is deliberately a batch artifact, not a test
;; framework: it has no arguments, configuration, cache, reusable fixture, or
;; result protocol.  A failure exits nonzero; the sole success sentinel is the
;; final `CERT PASS' line printed after owned-root cleanup succeeds.
;;
;; Run from the repository root:
;;
;;   `emacs' --batch -Q -L . -L tests -L lib/mysql \
;;     -l tools/cert-alpha-large.el

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'dorg-api)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-import)
(require 'dorg-mode)
(require 'dorg-ql)
(require 'dorg-registry)
(require 'dorg-test-helpers)

(defconst dorg-cert-alpha-large--byte-count 10485812
  "Exact byte count of the certification workload.")

(defconst dorg-cert-alpha-large--sha256
  "a9572977f856d3aaad627ca460e19c3d2433d734b0f8f04663d0d22b0eca605f"
  "Pinned SHA-256 digest of the certification workload.")

(defconst dorg-cert-alpha-large--header
  (concat "#+TITLE: DOrg alpha scale certificate\n\n"
          "* TODO Scale witness\n"
          ":PROPERTIES:\n"
          ":DORG_ID: 9001\n"
          ":END:\n")
  "Exact ASCII/LF workload prefix.")

(defconst dorg-cert-alpha-large--pattern
  "scale-payload-0123456789abcdef\n"
  "Repeated ASCII/LF workload payload pattern.")

(defconst dorg-cert-alpha-large--suffix "\nLIKYF-END\n"
  "Exact ASCII/LF workload suffix.")

(defconst dorg-cert-alpha-large--form
  '(on (id 9001) (set (property "CERTIFIED") "yes"))
  "Typed public mutation certified by both storage arms.")

(defconst dorg-cert-alpha-large--drift-form
  '(on (id 9001) (set (property "CERTIFIED") "drift"))
  "Intervening same-field mutation used to prove both CAS refusals.")

(defvar dorg-cert-alpha-large--events nil
  "Reverse-ordered human-readable evidence lines for the current run.")

(defun dorg-cert-alpha-large--emit (format-string &rest args)
  "Record and print FORMAT-STRING with ARGS as one evidence line."
  (let ((line (apply #'format format-string args)))
    (push line dorg-cert-alpha-large--events)
    (princ (concat "CERT " line "\n"))))

(defun dorg-cert-alpha-large--assert (predicate format-string &rest args)
  "Signal unless PREDICATE holds, formatting FORMAT-STRING with ARGS."
  (unless predicate
    (error "Certificate assertion: %s"
           (apply #'format format-string args))))

(defun dorg-cert-alpha-large--generate ()
  "Return the exact deterministic certification workload as unibyte text."
  (let* ((payload-count (- dorg-cert-alpha-large--byte-count
                           (string-bytes dorg-cert-alpha-large--header)
                           (string-bytes dorg-cert-alpha-large--suffix)))
         (whole (/ payload-count (string-bytes dorg-cert-alpha-large--pattern)))
         (tail (% payload-count (string-bytes dorg-cert-alpha-large--pattern))))
    (dorg-cert-alpha-large--assert (>= payload-count 0)
                                   "workload components exceed byte budget")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert dorg-cert-alpha-large--header)
      (dotimes (_ whole)
        (insert dorg-cert-alpha-large--pattern))
      (insert (substring dorg-cert-alpha-large--pattern 0 tail))
      (insert dorg-cert-alpha-large--suffix)
      (buffer-string))))

(defun dorg-cert-alpha-large--sha256 (bytes)
  "Return the lowercase SHA-256 hex digest of unibyte BYTES."
  (secure-hash 'sha256 bytes))

(defun dorg-cert-alpha-large--write-bytes (file bytes)
  "Write unibyte BYTES to FILE without coding conversion."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun dorg-cert-alpha-large--read-bytes (file)
  "Read FILE literally and return its unibyte contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun dorg-cert-alpha-large--file-sha256 (file)
  "Return the SHA-256 hex digest of FILE's literal bytes."
  (dorg-cert-alpha-large--sha256
   (dorg-cert-alpha-large--read-bytes file)))

(defun dorg-cert-alpha-large--timed (backend leg ceiling function)
  "Run FUNCTION for BACKEND LEG within CEILING seconds and return its value."
  (let ((started (float-time))
        result)
    (setq result
          (with-timeout
              (ceiling
               (error "%s/%s exceeded %.1fs ceiling" backend leg ceiling))
            (funcall function)))
    (let ((elapsed (- (float-time) started)))
      (dorg-cert-alpha-large--assert
       (<= elapsed ceiling) "%s/%s took %.3fs (> %.1fs)"
       backend leg elapsed ceiling)
      (dorg-cert-alpha-large--emit
       "leg backend=%s name=%s seconds=%.3f ceiling=%.1f"
       backend leg elapsed ceiling))
    result))

(defun dorg-cert-alpha-large--process-line (program &rest args)
  "Return PROGRAM ARGS output as one trimmed line, or signal on failure."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil t nil args)))
      (unless (zerop status)
        (error "%s %S exited %s: %s"
               program args status (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun dorg-cert-alpha-large--property-count (bytes value)
  "Count exact CERTIFIED property lines carrying VALUE in BYTES."
  (let ((start 0)
        (regexp (format "^:CERTIFIED:[ \t]+%s[ \t]*$"
                        (regexp-quote value)))
        (count 0))
    (while (string-match regexp bytes start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun dorg-cert-alpha-large--query-witness (source)
  "Assert the exact query witness returned from SOURCE."
  (let* ((hits (dorg-ql-select '(id 9001) source))
         (hit (car hits)))
    (dorg-cert-alpha-large--assert (= 1 (length hits))
                                   "query returned %d hits" (length hits))
    (dorg-cert-alpha-large--assert (= 9001 (plist-get hit :heading-id))
                                   "query returned wrong heading id: %S" hit)
    (dorg-cert-alpha-large--assert
     (equal "Scale witness" (plist-get hit :title))
     "query returned wrong title: %S" (plist-get hit :title))
    hit))

(defun dorg-cert-alpha-large--plain-error-pair (function)
  "Run FUNCTION and return its structured plain API error reason/detail pair."
  (condition-case err
      (progn (funcall function) :no-error)
    (dorg-api-error
     (cons (dorg-api-error-reason err)
           (plist-get (dorg-api-error-data err) :detail)))))

(defun dorg-cert-alpha-large--dolt-overlap (function)
  "Run FUNCTION and return structured Dolt overlap data, or signal."
  (condition-case err
      (progn (funcall function) (error "Dolt stale apply unexpectedly succeeded"))
    (dorg-mutate-error
     (dorg-cert-alpha-large--assert
      (eq 'overlap (dorg-mutate-error-reason err))
      "Dolt stale apply reason was %S" (dorg-mutate-error-reason err))
     (dorg-mutate-error-data err))))

(defun dorg-cert-alpha-large--kill-buffer (buffer)
  "Kill BUFFER without a save prompt when it is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun dorg-cert-alpha-large--plain-arm (root initial)
  "Certify the implicit plain-org path under owned ROOT using INITIAL bytes."
  (let* ((file (expand-file-name "plain.org" root))
         (source (list :file file))
         (before-inode nil)
         (query-buffer nil)
         (materialize-buffer nil))
    (dorg-cert-alpha-large--write-bytes file initial)
    (setq before-inode
          (file-attribute-file-identifier (file-attributes file)))
    (cl-labels ((forbid (&rest args)
                  (error "Plain-org path crossed a Dolt seam: %S" args)))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) #'forbid)
                ((symbol-function 'dorg-sql) #'forbid)
                ((symbol-function 'dorg-bind-dolt) #'forbid))
        (unwind-protect
            (progn
              (setq query-buffer (dorg--find-file-noselect file))
              (with-current-buffer query-buffer
                (unless (derived-mode-p 'org-mode)
                  (let ((org-inhibit-startup t)) (org-mode)))
                (dorg-cert-alpha-large--assert
                 (eq 'plain-org
                     (dorg-backend-type
                      (dorg-registration-backend
                       (dorg--ensure-registration))))
                 "unbound file did not default to plain-org")
                (dorg-cert-alpha-large--timed
                 "plain" "query" 60
                 (lambda ()
                   (dorg-cert-alpha-large--query-witness query-buffer)))
                (dorg-cert-alpha-large--assert
                 (equal initial
                        (buffer-substring-no-properties
                         (point-min) (point-max)))
                 "plain query changed its visiting buffer"))
              ;; A visiting buffer would deliberately select the buffer-token
              ;; variant.  Kill it so this certificate proves file-token CAS.
              (dorg-cert-alpha-large--kill-buffer query-buffer)
              (setq query-buffer nil)
              (let* ((hash-before (dorg-cert-alpha-large--file-sha256 file))
                     (inode-before
                      (file-attribute-file-identifier (file-attributes file)))
                     (dry (dorg-cert-alpha-large--timed
                           "plain" "dry-run" 60
                           (lambda ()
                             (dorg-api-execute
                              dorg-cert-alpha-large--form
                              :mode 'dry-run :source source))))
                     (base (plist-get dry :base))
                     (projection (car (plist-get dry :projections))))
                (dorg-cert-alpha-large--assert
                 (eq 'file (plist-get base :kind))
                 "plain dry-run returned non-file base: %S" base)
                (dorg-cert-alpha-large--assert
                 (and (= 1 (length (plist-get dry :patches)))
                      (equal '(9001) (plist-get dry :targets))
                      (= 1 (length (plist-get dry :projections)))
                      (not (equal (plist-get projection :before)
                                  (plist-get projection :after)))
                      (string-match-p
                       "^:CERTIFIED:[ \t]+yes[ \t]*$"
                       (plist-get projection :after)))
                 "plain dry-run omitted exact patch/target/projection evidence")
                (dorg-cert-alpha-large--assert
                 (equal hash-before (dorg-cert-alpha-large--file-sha256 file))
                 "plain dry-run changed file bytes")
                (dorg-cert-alpha-large--assert
                 (equal inode-before
                        (file-attribute-file-identifier (file-attributes file)))
                 "plain dry-run replaced the file")
                (dorg-cert-alpha-large--emit
                 "plain dry-run targets=1 patches=1 base-fingerprint=%s no-write=yes"
                 (plist-get base :fingerprint))
                (dorg-api-execute dorg-cert-alpha-large--drift-form
                                  :mode 'apply :source source :save t)
                (let* ((drift-bytes (dorg-cert-alpha-large--read-bytes file))
                       (drift-hash (dorg-cert-alpha-large--sha256 drift-bytes))
                       (stale
                        (dorg-cert-alpha-large--timed
                         "plain" "cas-refusal" 60
                         (lambda ()
                           (dorg-cert-alpha-large--plain-error-pair
                            (lambda ()
                              (dorg-api-execute
                               dorg-cert-alpha-large--form
                               :mode 'apply :source source :base base
                               :save t)))))))
                  (dorg-cert-alpha-large--assert
                   (and (= 1 (dorg-cert-alpha-large--property-count
                              drift-bytes "drift"))
                        (= 0 (dorg-cert-alpha-large--property-count
                              drift-bytes "yes"))
                        (not (equal drift-hash
                                    dorg-cert-alpha-large--sha256)))
                   "plain intervening mutation did not land exactly once")
                  (dorg-cert-alpha-large--assert
                   (equal '(stale . base-revision) stale)
                   "plain stale apply did not refuse as stale/base-revision")
                  (dorg-cert-alpha-large--assert
                   (equal drift-bytes (dorg-cert-alpha-large--read-bytes file))
                   "plain stale refusal wrote bytes")
                  (dorg-cert-alpha-large--emit
                   "plain refusal=stale/base-revision drift-sha256=%s no-write=yes"
                   drift-hash))
                (let* ((fresh (dorg-api-execute
                               dorg-cert-alpha-large--form
                               :mode 'dry-run :source source))
                       (pre-apply (dorg-cert-alpha-large--read-bytes file))
                       (apply-result
                        (dorg-cert-alpha-large--timed
                         "plain" "cas-apply" 60
                         (lambda ()
                           (dorg-api-execute
                            dorg-cert-alpha-large--form
                            :mode 'apply :source source
                            :base (plist-get fresh :base) :save t))))
                       (final (dorg-cert-alpha-large--read-bytes file)))
                  (dorg-cert-alpha-large--assert
                   (and (not (equal pre-apply final))
                        (not (equal initial final)))
                   "plain fresh apply was a vacuous transformation")
                  (dorg-cert-alpha-large--assert
                   (= 1 (dorg-cert-alpha-large--property-count final "yes"))
                   "plain final CERTIFIED=yes count was not one")
                  (dorg-cert-alpha-large--assert
                   (not (equal before-inode
                               (file-attribute-file-identifier
                                (file-attributes file))))
                   "plain atomic persistence never changed the inode")
                  (dorg-cert-alpha-large--assert
                   (plist-get apply-result :new-token)
                   "plain apply omitted its new token")
                  (setq materialize-buffer (dorg--find-file-noselect file))
                  (with-current-buffer materialize-buffer
                    (goto-char (point-min))
                    (insert "STALE\n")
                    (dorg-cert-alpha-large--timed
                     "plain" "materialize" 60
                     (lambda () (dorg-materialize)))
                    (dorg-cert-alpha-large--assert
                     (equal final
                            (buffer-substring-no-properties
                             (point-min) (point-max)))
                     "plain zero-argument materialize was not exact")
                    (dorg-cert-alpha-large--assert
                     (not (buffer-modified-p))
                     "plain materialize left its buffer modified"))
                  (dorg-cert-alpha-large--emit
                   (concat "plain final-sha256=%s base-kind=file "
                           "new-fingerprint=%s materialize=exact")
                   (dorg-cert-alpha-large--sha256 final)
                   (plist-get (plist-get apply-result :new-token)
                              :fingerprint))
                  (list :bytes final
                        :sha256 (dorg-cert-alpha-large--sha256 final)
                        :new-token (plist-get apply-result :new-token)))))
          (dorg-cert-alpha-large--kill-buffer query-buffer)
          (dorg-cert-alpha-large--kill-buffer materialize-buffer))))))

(defun dorg-cert-alpha-large--clean-dolt-p (dolt-dir)
  "Return non-nil when DOLT-DIR has an empty Dolt working set."
  (let ((rows (dorg-sql dolt-dir
                        "SELECT COUNT(*) AS n FROM dolt_status")))
    (and (= 1 (length rows))
         (= 0 (alist-get 'n (car rows))))))

(defun dorg-cert-alpha-large--property-rows (dolt-dir)
  "Return heading 9001 property rows from DOLT-DIR in ordinal order."
  (dorg-sql
   dolt-dir
   (concat "SELECT prop_key, prop_value FROM heading_properties "
           "WHERE heading_id = 9001 ORDER BY ordinal")))

(defun dorg-cert-alpha-large--strip-binding-lines (bytes)
  "Strip exactly DORG_DOLT_DIR and DORG_HASH lines from rendered BYTES."
  (let ((lines (split-string bytes "\n" nil))
        removed
        kept)
    (dolist (line lines)
      (cond
       ((string-match-p "\\`#\\+DORG_DOLT_DIR:" line)
        (push 'DORG_DOLT_DIR removed))
       ((string-match-p "\\`#\\+DORG_HASH:" line)
        (push 'DORG_HASH removed))
       (t (push line kept))))
    (dorg-cert-alpha-large--assert
     (equal '(DORG_DOLT_DIR DORG_HASH)
            (sort removed (lambda (a b)
                            (string< (symbol-name a) (symbol-name b)))))
     "Dolt render binding lines were not exactly the allowed pair: %S"
     removed)
    (mapconcat #'identity (nreverse kept) "\n")))

(defun dorg-cert-alpha-large--contained-p (path root)
  "Return non-nil when existing PATH resolves beneath existing ROOT."
  (file-in-directory-p (file-truename path)
                       (file-name-as-directory (file-truename root))))

(defun dorg-cert-alpha-large--canonical-dir (directory)
  "Return DIRECTORY truenamed with a trailing slash."
  (file-name-as-directory (file-truename directory)))

(defun dorg-cert-alpha-large--with-fixture-server
    (root dolt-dir function)
  "Call FUNCTION with one strict server for DOLT-DIR contained by ROOT."
  (let* ((target (dorg-dolt--target dolt-dir))
         (data-dir (plist-get target :server-data-dir))
         (cfg-dir (plist-get target :cfg-dir))
         (claim-file (dorg-dolt-server--claim-file dolt-dir))
         (index-file (dorg-dolt-server--index-file dolt-dir))
         (port nil)
         result)
    (dolist (path (list dolt-dir data-dir cfg-dir claim-file index-file))
      (dorg-cert-alpha-large--assert
       (dorg-cert-alpha-large--contained-p
        (if (file-exists-p path) path (file-name-directory path)) root)
       "Fixture server path escaped its owned root: %s" path))
    (unwind-protect
        (progn
          (setq port (dorg-dolt-start dolt-dir))
          (let* ((claim (dorg-dolt-server-claim dolt-dir))
                 (entry
                  (cl-find-if
                   (lambda (candidate)
                     (equal (dorg-cert-alpha-large--canonical-dir dolt-dir)
                            (dorg-cert-alpha-large--canonical-dir
                             (plist-get candidate :dolt-dir))))
                   (dorg-dolt-server-list))))
            (dorg-cert-alpha-large--assert
             (and (integerp port)
                  (dorg-dolt-server--port-live-p port)
                  claim
                  (eq 'daemon (alist-get :owner-kind claim))
                  (eq 'ready (alist-get :state claim))
                  (= port (alist-get :port claim))
                  (equal (dorg-cert-alpha-large--canonical-dir dolt-dir)
                         (dorg-cert-alpha-large--canonical-dir
                          (alist-get :dolt-dir claim)))
                  (equal (dorg-cert-alpha-large--canonical-dir data-dir)
                         (dorg-cert-alpha-large--canonical-dir
                          (alist-get :data-dir claim)))
                  entry
                  (= port (plist-get entry :port))
                  (file-exists-p claim-file)
                  (file-exists-p index-file))
             "Fixture server claim/port/inventory evidence was incomplete: %S"
             claim)
            (dorg-cert-alpha-large--assert
             (null dorg-test--server-port)
             "Fixture attached to shared test server port %S"
             dorg-test--server-port)
            (dorg-cert-alpha-large--emit
             "server start port=%d owner=daemon data-dir-contained=yes"
             port))
          (setq result (funcall function port)))
      (when port
        (dorg-cert-alpha-large--assert
         (dorg-dolt-stop dolt-dir)
         "Explicit fixture server stop reported failure")
        (dorg-cert-alpha-large--assert
         (and (not (dorg-dolt-server--port-live-p port))
              (null (dorg-dolt-server-claim dolt-dir))
              (not (file-exists-p claim-file))
              (not (file-exists-p index-file))
              (null
               (cl-find-if
                (lambda (candidate)
                  (equal (dorg-cert-alpha-large--canonical-dir dolt-dir)
                         (dorg-cert-alpha-large--canonical-dir
                          (plist-get candidate :dolt-dir))))
                (dorg-dolt-server-list))))
         "Fixture server survived explicit cleanup on port %s" port)
        (dorg-cert-alpha-large--emit
         "server stop port=%d process=gone claim=gone index=gone" port)))
    result))

(defun dorg-cert-alpha-large--dolt-arm (root initial plain-final)
  "Certify owned-server Dolt under ROOT using INITIAL and PLAIN-FINAL bytes."
  (let* ((source-file (expand-file-name "import-source.org" root))
         (target-file (expand-file-name "materialized.org" root))
         (output-file (expand-file-name "dolt-output.org" root))
         (dolt-dir (file-name-as-directory (expand-file-name "dolt" root)))
         (source-buffer nil)
         (dest-buffer nil)
         receipt
         registration
         root-id)
    (dorg-cert-alpha-large--write-bytes source-file initial)
    (make-directory dolt-dir t)
    (dorg-sql--with-cli-seam dolt-dir
      (let ((dorg-sql-cli-timeout 300.0))
        (dorg-init dolt-dir)))
    (dorg-cert-alpha-large--assert
     (not (dorg-dolt-server-claim dolt-dir))
     "CLI bootstrap unexpectedly acquired a server claim")
    (dorg-cert-alpha-large--with-fixture-server
     root dolt-dir
     (lambda (_port)
       (let ((dorg-sql-cli-timeout 300.0))
        (setq receipt
              (dorg-cert-alpha-large--timed
               "dolt" "import" 300
               (lambda ()
                 (dorg-import-document
                  source-file :dolt-dir dolt-dir :target-file target-file
                  :mode 'bootstrap))))
        (setq root-id (plist-get receipt :root-id))
        (dorg-cert-alpha-large--assert (plist-get receipt :ok)
                                       "Dolt import receipt was not successful")
        (dorg-cert-alpha-large--assert
         (and (integerp root-id)
              (member root-id (plist-get receipt :created))
              (member 9001 (plist-get receipt :created))
              (stringp (plist-get receipt :base-commit))
              (stringp (plist-get receipt :commit))
              (not (equal (plist-get receipt :base-commit)
                          (plist-get receipt :commit)))
              (equal (plist-get receipt :commit)
                     (dorg-head-commit dolt-dir)))
         "Dolt import receipt omitted root/heading/commit evidence: %S" receipt)
        (dorg-cert-alpha-large--assert
         (equal dorg-cert-alpha-large--sha256
                (dorg-cert-alpha-large--file-sha256 source-file))
         "Dolt import changed its source bytes")
        (dorg-cert-alpha-large--assert
         (not (file-exists-p target-file))
         "Dolt import materialized its target implicitly")
        (dorg-cert-alpha-large--assert (dorg-cert-alpha-large--clean-dolt-p dolt-dir)
                                       "Dolt import left a dirty working set")
        (dorg-cert-alpha-large--emit
         "dolt import root=%s created=%d base=%s commit=%s source-unchanged=yes clean=yes"
         root-id (length (plist-get receipt :created))
         (plist-get receipt :base-commit) (plist-get receipt :commit))
        (setq registration (dorg-bind-dolt target-file dolt-dir))
        (dorg-cert-alpha-large--assert
         (= root-id
            (dorg-backend-root-id
             (dorg-registration-backend registration)))
         "bound Dolt root differs from import receipt")
        (dorg-cert-alpha-large--timed
         "dolt" "query" 60
         (lambda ()
           (dorg-cert-alpha-large--query-witness registration)))
        (let* ((source (list :dolt-dir dolt-dir :root-id root-id))
               (head-before (dorg-head-commit dolt-dir))
               (rows-before (dorg-cert-alpha-large--property-rows dolt-dir))
               (dry (dorg-cert-alpha-large--timed
                     "dolt" "dry-run" 120
                     (lambda ()
                       (dorg-api-execute dorg-cert-alpha-large--form
                                         :mode 'dry-run :source source))))
               (base (plist-get dry :base)))
          (dorg-cert-alpha-large--assert
           (and (equal head-before base)
                (= 1 (length (plist-get dry :patches)))
                (equal '(9001) (plist-get dry :targets)))
           "Dolt dry-run omitted exact base/target evidence")
          (dorg-cert-alpha-large--assert
           (and (equal head-before (dorg-head-commit dolt-dir))
                (equal rows-before
                       (dorg-cert-alpha-large--property-rows dolt-dir))
                (dorg-cert-alpha-large--clean-dolt-p dolt-dir))
           "Dolt dry-run changed HEAD, rows, or working set")
          (dorg-cert-alpha-large--emit
           "dolt dry-run targets=1 patches=1 base=%s no-write=yes clean=yes"
           base)
          (let* ((drift-result
                  (dorg-api-execute dorg-cert-alpha-large--drift-form
                                    :mode 'apply :source source))
                 (drift-head (plist-get drift-result :new-token))
                 (drift-rows (dorg-cert-alpha-large--property-rows dolt-dir))
                 (overlap
                  (dorg-cert-alpha-large--timed
                   "dolt" "cas-refusal" 180
                   (lambda ()
                     (dorg-cert-alpha-large--dolt-overlap
                      (lambda ()
                        (dorg-api-execute
                         dorg-cert-alpha-large--form :mode 'apply
                         :source source :base base)))))))
            (dorg-cert-alpha-large--assert
             (and (equal base (plist-get overlap :base-commit))
                  (equal drift-head (plist-get overlap :commit))
                  (member 9001 (plist-get overlap :overlap)))
             "Dolt overlap evidence was incomplete: %S" overlap)
            (dorg-cert-alpha-large--assert
             (and (equal drift-head (dorg-head-commit dolt-dir))
                  (equal drift-rows
                         (dorg-cert-alpha-large--property-rows dolt-dir))
                  (dorg-cert-alpha-large--clean-dolt-p dolt-dir))
             "Dolt overlap refusal changed HEAD or working set")
            (dorg-cert-alpha-large--emit
             "dolt refusal=overlap heading=9001 base=%s current=%s no-write=yes clean=yes"
             base drift-head)
            (let* ((fresh
                    (dorg-api-execute dorg-cert-alpha-large--form
                                      :mode 'dry-run :source source))
                   (apply-result
                    (dorg-cert-alpha-large--timed
                     "dolt" "cas-apply" 180
                     (lambda ()
                       (dorg-api-execute
                        dorg-cert-alpha-large--form :mode 'apply
                        :source source :base (plist-get fresh :base)))))
                   (final-head (plist-get apply-result :new-token)))
              (dorg-cert-alpha-large--assert
               (and final-head
                    (not (equal drift-head final-head))
                    (equal final-head (dorg-head-commit dolt-dir))
                    (plist-get apply-result :timings)
                    (member 9001
                            (plist-get (plist-get apply-result :affected)
                                       :changed))
                    (dorg-cert-alpha-large--clean-dolt-p dolt-dir))
               "Dolt fresh apply omitted commit/timings/clean evidence")
              (unwind-protect
                  (progn
                    (setq source-buffer (generate-new-buffer " *dorg-cert-source*")
                          dest-buffer (generate-new-buffer " *dorg-cert-dest*"))
                    (with-current-buffer source-buffer
                      (let ((org-inhibit-startup t)) (org-mode))
                      (set-visited-file-name target-file t)
                      (set-buffer-modified-p nil))
                    (with-current-buffer dest-buffer
                      (insert "STALE\n")
                      (let ((descriptor
                             (dorg-cert-alpha-large--timed
                              "dolt" "materialize" 180
                              (lambda ()
                                (dorg-materialize
                                 dest-buffer :source source-buffer
                                 :revision final-head)))))
                        (dorg-cert-alpha-large--assert
                         (and (equal final-head
                                     (plist-get descriptor :revision))
                              (= root-id (plist-get descriptor :root-id))
                              (eq 'whole (plist-get descriptor :scope))
                              (not (string-prefix-p
                                    "STALE\n"
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))
                         "Dolt materialize descriptor/bytes were not exact: %S"
                         descriptor))
                      (dorg-cert-alpha-large--write-bytes
                       output-file
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
                    (let* ((rendered
                            (dorg-cert-alpha-large--read-bytes output-file))
                           (normalized
                            (dorg-cert-alpha-large--strip-binding-lines rendered)))
                      (dorg-cert-alpha-large--assert
                       (equal plain-final normalized)
                       (concat "Dolt/plain byte parity failed after stripping "
                               "only DORG_DOLT_DIR and DORG_HASH"))
                      (dorg-cert-alpha-large--assert
                       (= 1 (dorg-cert-alpha-large--property-count
                             normalized "yes"))
                       "Dolt final CERTIFIED=yes count was not one")
                      (dorg-cert-alpha-large--assert
                       (and (string-match-p
                             "^\\* TODO Scale witness$" normalized)
                            (string-match-p
                             "^:DORG_ID:[ \t]+9001[ \t]*$" normalized)
                            (string-suffix-p dorg-cert-alpha-large--suffix
                                             normalized)
                            (not (equal initial normalized)))
                       "Dolt final bytes lost title/id/body sentinel or did not change")
                      (dorg-cert-alpha-large--assert
                       (equal dorg-cert-alpha-large--sha256
                              (dorg-cert-alpha-large--file-sha256 source-file))
                       "Dolt arm changed its import source")
                      (dorg-cert-alpha-large--assert
                       (dorg-cert-alpha-large--clean-dolt-p dolt-dir)
                       "Dolt materialize left a dirty working set")
                      (dorg-cert-alpha-large--emit
                       (concat "dolt final-commit=%s changed=9001 timings=%S "
                               "normalized-sha256=%s materialize=exact clean=yes")
                       final-head (plist-get apply-result :timings)
                       (dorg-cert-alpha-large--sha256 normalized))
                      (list :sha256
                            (dorg-cert-alpha-large--sha256 normalized)
                            :commit final-head
                            :timings (plist-get apply-result :timings))))
                (dorg-cert-alpha-large--kill-buffer source-buffer)
                (dorg-cert-alpha-large--kill-buffer dest-buffer))))))))))

(defun dorg-cert-alpha-large--with-owned-root (reason function)
  "Call FUNCTION in one isolated test-owned root registered for REASON."
  (let* ((raw (make-temp-file "dorg-cert-alpha-large-" t))
         (dorg-test--registry-dir
          (expand-file-name ".fixture-registry/" raw))
         (dorg-dolt-server-index-dir
          (expand-file-name ".server-index/" raw))
         (owned nil)
         result)
    (unwind-protect
        (progn
          (setq owned (dorg-test--register-owned-root raw reason))
          (setq result (funcall function owned)))
      (when owned
        (let ((status (dorg-test--delete-owned-root owned)))
          (unless (eq 'gone status)
            (error "Owned-root cleanup failed: %S (%s)" status owned)))))
    (dorg-cert-alpha-large--assert
     (not (file-exists-p raw)) "owned root survived cleanup: %s" raw)
    (dorg-cert-alpha-large--assert
     (not (gethash owned dorg-test--owned-roots))
     "owned-root registry retained a deleted root: %s" owned)
    result))

(defun dorg-cert-alpha-large--cleanup-witness ()
  "Prove a post-server-start failure still reaps server and owned root."
  (let (root port observed)
    (condition-case err
        (dorg-cert-alpha-large--with-owned-root
         "likyf-injected-cleanup-witness"
         (lambda (owned)
           (let ((dolt-dir (expand-file-name "dolt" owned)))
             (setq root owned)
             (make-directory dolt-dir t)
             (dorg-sql--with-cli-seam dolt-dir
               (dorg-init dolt-dir))
             (dorg-cert-alpha-large--with-fixture-server
              owned dolt-dir
              (lambda (fixture-port)
                (setq port fixture-port)
                (error "LIKYF injected post-start cleanup witness"))))))
      (error
       (setq observed (string-match-p
                       "LIKYF injected post-start cleanup witness"
                       (error-message-string err)))))
    (dorg-cert-alpha-large--assert observed
                                   "cleanup witness lost injected failure")
    (dorg-cert-alpha-large--assert
     (and root port
          (not (file-exists-p root))
          (not (dorg-dolt-server--port-live-p port)))
     "cleanup witness left root/server alive: root=%S port=%S" root port)
    (dorg-cert-alpha-large--emit
     "cleanup injected-post-start-failure=reaped port=%d" port)))

(defun dorg-cert-alpha-large--run ()
  "Run the complete bounded alpha certificate and return its evidence."
  (setq dorg-cert-alpha-large--events nil)
  (let* ((started (float-time))
         (first
          (dorg-cert-alpha-large--timed
           "fixture" "generate-a" 30 #'dorg-cert-alpha-large--generate))
         (second
          (dorg-cert-alpha-large--timed
           "fixture" "generate-b" 30 #'dorg-cert-alpha-large--generate))
         (digest (dorg-cert-alpha-large--sha256 first)))
    (dorg-cert-alpha-large--assert (equal first second)
                                   "two in-memory generations differ")
    (dorg-cert-alpha-large--assert
     (= dorg-cert-alpha-large--byte-count (string-bytes first))
     "generated %d bytes, expected %d"
     (string-bytes first) dorg-cert-alpha-large--byte-count)
    (dorg-cert-alpha-large--assert
     (equal dorg-cert-alpha-large--sha256 digest)
     "workload SHA-256 was %s, expected %s"
     digest dorg-cert-alpha-large--sha256)
    (dorg-cert-alpha-large--assert
     (and (string-prefix-p dorg-cert-alpha-large--header first)
          (string-suffix-p dorg-cert-alpha-large--suffix first)
          (string-match-p "^\\* TODO Scale witness$" first)
          (string-match-p "^:DORG_ID:[ \t]+9001[ \t]*$" first)
          (not (string-match-p "^:CERTIFIED:" first)))
     "workload lost its fixed heading/id/property/sentinel shape")
    (dorg-cert-alpha-large--emit
     "versions git=%s emacs=%s dolt=%s"
     (dorg-cert-alpha-large--process-line "git" "rev-parse" "HEAD")
     emacs-version
     (dorg-cert-alpha-large--process-line "dolt" "version"))
    (dorg-cert-alpha-large--assert
     (string-empty-p
      (dorg-cert-alpha-large--process-line
       "git" "status" "--porcelain" "--untracked-files=all"))
     "candidate worktree is not clean")
    (dorg-cert-alpha-large--emit "candidate worktree=clean")
    (dorg-cert-alpha-large--emit "workload bytes=%d sha256=%s"
                                 (string-bytes first) digest)
    (let ((dorg--registry nil)
          (dorg-dolt-auto-server nil)
          (dorg-dolt-server-autostart nil)
          (dorg-sql-transport 'server)
          (dorg-test--server-port nil)
          (dorg-test--fallback-cli-seam t))
      (dorg-cert-alpha-large--cleanup-witness)
      (dorg-cert-alpha-large--with-owned-root
       "likyf-alpha-certificate"
       (lambda (root)
         (let ((repo-tracker
                (file-name-as-directory
                 (expand-file-name ".tracker" default-directory)))
               plain
               dolt)
           (dorg-cert-alpha-large--assert
            (and (file-in-directory-p root temporary-file-directory)
                 (not (file-in-directory-p root repo-tracker)))
            "fixture root is non-temporary or inside the live tracker store")
           (dorg-cert-alpha-large--assert
            (null (dorg-dolt-server-list))
            "implicit/shared server appeared in isolated fixture inventory")
           (setq plain (dorg-cert-alpha-large--plain-arm root first))
           (dorg-cert-alpha-large--assert
            (null (dorg-dolt-server-list))
            "plain arm started or attached to a Dolt server")
           (setq dolt
                 (dorg-cert-alpha-large--dolt-arm
                  root first (plist-get plain :bytes)))
           (dorg-cert-alpha-large--assert
            (equal (plist-get plain :sha256) (plist-get dolt :sha256))
            "plain/Dolt final hashes differ")
           (dorg-cert-alpha-large--emit
            "parity final-sha256=%s allowed-strip=DORG_DOLT_DIR,DORG_HASH"
            (plist-get plain :sha256))))))
    (dorg-cert-alpha-large--emit "total seconds=%.3f ceiling=900.0"
                                 (- (float-time) started))
    (nreverse dorg-cert-alpha-large--events)))

(defun dorg-cert-alpha-large--main ()
  "Batch entry point for the alpha certificate."
  (condition-case err
      (with-timeout
          (900 (error "Certificate exceeded 900-second total ceiling"))
        (dorg-cert-alpha-large--run)
        (princ "CERT PASS\n"))
    ((error quit)
     (princ (format "CERT FAIL %s\n" (error-message-string err)))
     (kill-emacs 1))))

(when (and noninteractive
           (not (bound-and-true-p byte-compile-current-file)))
  (dorg-cert-alpha-large--main))

(provide 'cert-alpha-large)
;;; cert-alpha-large.el ends here
