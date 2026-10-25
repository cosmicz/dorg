;;; dorg-test-helpers.el --- Test utilities for dorg  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared test utilities, macros, and helpers.
;;
;; This file owns ZERO Dolt sql-server lifecycle: process spawn, the durable
;; ownership registry (tracked-case), the shared warm server, and owned-root
;; reaping all live in the opaque `dorg-dolt-fixture' module, which this file
;; DELEGATES to.  The `dorg-test--server-*'/`-registry-*'/`-*-owned-*' names
;; used below resolve there via the require.  What remains here is the
;; test-support that has no lifecycle of its own: the temp-db/temp-dir
;; macros (which merely bind and unwind), message capture, the fixture repo
;; template, and org-domain query helpers.

;;; Code:

(require 'cl-lib)
(require 'dorg-schema)
(require 'dorg-dolt-server)
(require 'dorg-dolt-fixture)

;;; ─── Fallback (copy-repo) Transport Pin ───────────────────────────────

(defvar dorg-test--fallback-cli-seam t
  "When non-nil, wrap the copy-repo (non-shared-server) fallback bodies of
`dorg-test--with-temp-db' and `dorg-test--with-temp-db-and-dir' in
`dorg-sql--with-cli-seam'.

A copy-repo store is a standalone Dolt repo that no sql-server holds, so in
the server-only world (tracked-case, which deletes the implicit `auto'/`cli'
policy) its SQL must route through the sanctioned test/recovery CLI seam —
one-shot CLI pinned via `dorg-sql--pinned-transport' — rather than relying on
the implicit policy that fwb3z removes.  Wrapping is therefore the correct
default: a test that merely wants a working standalone store is served CLI.

Bind this nil to OPT OUT for the narrow set of tests that deliberately drive
non-CLI transport against a standalone store — server-lifecycle/classification
tests that bind `dorg-sql-transport' `server', and the transport-policy
resolver tests that probe `auto'/`cli' resolution.  For those, the pin would
mask the transport under test and make the assertion vacuous.")

;;; ─── Test Database Macros ──────────────────────────────────────────────

(defmacro dorg-test--with-temp-db (dolt-var &rest body)
  "Execute BODY with DOLT-VAR bound to a test database dolt-dir.
Uses the shared server when available, falls back to copy-repo."
  (declare (indent 1) (debug (symbolp body)))
  `(if dorg-test--server-port
       (let ((,dolt-var (dorg-test--create-test-db)))
         (unwind-protect
             (progn ,@body)
           (dorg-test--destroy-test-db ,dolt-var)))
     (let* ((temp--internal (dorg-test--register-owned-root
                             (make-temp-file "dorg-test-" t)
                             "with-temp-db"))
            (,dolt-var (dorg-test--copy-repo temp--internal)))
       (unwind-protect
           ;; Standalone copy-repo store — route through the sanctioned CLI
           ;; seam by default so it survives the server-only collapse
           ;; (`dorg-test--fallback-cli-seam').
           (if dorg-test--fallback-cli-seam
               (dorg-sql--with-cli-seam ,dolt-var ,@body)
             (progn ,@body))
         (dorg-test--delete-owned-root temp--internal)))))

(defmacro dorg-test--with-temp-db-and-dir (temp-var dolt-var &rest body)
  "Execute BODY with TEMP-VAR as temp dir and DOLT-VAR as test database.
TEMP-VAR is always a fresh temp directory (for org files, etc.).
DOLT-VAR comes from the shared server or copy-repo under TEMP-VAR."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let ((,temp-var (dorg-test--register-owned-root
                     (make-temp-file "dorg-test-" t)
                     "with-temp-db-and-dir")))
     (unwind-protect
         (if dorg-test--server-port
             (let ((,dolt-var (dorg-test--create-test-db)))
               (unwind-protect
                   (progn ,@body)
                 (dorg-test--destroy-test-db ,dolt-var)))
           (let ((,dolt-var (dorg-test--copy-repo ,temp-var)))
             ;; Standalone copy-repo store — route through the sanctioned CLI
             ;; seam by default (`dorg-test--fallback-cli-seam').
             (if dorg-test--fallback-cli-seam
                 (dorg-sql--with-cli-seam ,dolt-var ,@body)
               (progn ,@body))))
       (dorg-test--delete-owned-root ,temp-var))))

;;; ─── Message Capture ──────────────────────────────────────────────────

(defvar dorg-test--message-log 'inactive
  "Message capture state: symbol `inactive' outside tests, a list during.
Bound to nil by test macros to activate capture.")

(defun dorg-test--capture-message (fn fmt &rest args)
  "Around advice for `message': capture formatted string when active.
Active when `dorg-test--message-log' is a list (including nil).
Calls FN with FMT and ARGS unconditionally."
  (when (and fmt (listp dorg-test--message-log))
    (push (apply #'format-message fmt args) dorg-test--message-log))
  (apply fn fmt args))

(advice-add 'message :around #'dorg-test--capture-message)

;;; ─── Output Dump on Failure ───────────────────────────────────────────

(defun dorg-test--dump-captured-output (output-buf)
  "Print captured output to stderr for debugging a failed test.
OUTPUT-BUF is the `standard-output' buffer (may be dead).
Also prints messages from `dorg-test--message-log'."
  (let ((msgs (nreverse dorg-test--message-log))
        (output (when (buffer-live-p output-buf)
                  (with-current-buffer output-buf
                    (buffer-string)))))
    (when (or msgs (and output (not (string-empty-p output))))
      (let ((inhibit-message nil))
        (message "── captured test output ──")
        (when msgs
          (message "[messages]")
          (dolist (m msgs)
            (message "  %s" m)))
        (when (and output (not (string-empty-p output)))
          (message "[stdout]")
          (message "%s" output))
        (message "── end captured output ──")))))

;;; ─── Cross-Process Server-Index Isolation (tracked-regression) ──────────────

;; Since 5d6de85 `dorg-dolt-server-index-dir' (the cross-process managed-store
;; discovery registry) defaults to a STABLE per-user path so a fresh CLI child
;; can find detached servers.  That defeats TMPDIR/fixture isolation: a test
;; child inherits the operator's real index and discovers the live tracker
;; sql-server, reddening the full suite.  `dorg-test--isolate-server-index'
;; scopes the index to a fresh per-run OWNED dir and points BOTH carriers at it
;; — the in-process var `dorg-dolt-server-index-dir' AND the
;; `DORG_DOLT_SERVER_INDEX_DIR' env var so every spawned child inherits the
;; isolation.  It is process-global idempotent (the run has exactly one index
;; dir), and the dir is a durably-registered reapable owned root so normal
;; suite teardown reclaims it.  Wired into the suite-runner bootstrap
;; `dorg-setup-test-environment' (the single choke point every supported run
;; funnels through) and into the subprocess invoker `dorg-test-cli--invoke'.

(defvar dorg-test--server-index-dir nil
  "The process-global isolated cross-process server-index dir, or nil.
Set once by the first `dorg-test--isolate-server-index' call and reused for the
lifetime of the process so the run has exactly one isolated index.")

(defun dorg-test--isolate-server-index ()
  "Scope the cross-process server index to a fresh per-run OWNED dir.
Create (once per process) a durably-registered reapable owned dir, point BOTH
`dorg-dolt-server-index-dir' and the `DORG_DOLT_SERVER_INDEX_DIR' env var at it
\(children inherit the isolation), and return it.  Idempotent within one
process: a second call returns the SAME dir and re-asserts both carriers,
never the operator's per-user default.  The dir is reclaimed by normal suite
owned-root teardown."
  (unless dorg-test--server-index-dir
    (setq dorg-test--server-index-dir
          (file-name-as-directory
           (dorg-test--make-owned-temp-dir "dorg-server-index-"
                                           "cu9xm server-index isolation"))))
  (setq dorg-dolt-server-index-dir dorg-test--server-index-dir)
  (setenv "DORG_DOLT_SERVER_INDEX_DIR" dorg-test--server-index-dir)
  dorg-test--server-index-dir)

;;; ─── Temp Directory Lifecycle ─────────────────────────────────────────

(defun dorg-test--cleanup-temp (temp-dir output-buf)
  "Clean up TEMP-DIR and output buffer OUTPUT-BUF.
Kills any buffers visiting files under TEMP-DIR before deletion."
  (when (buffer-live-p output-buf)
    (kill-buffer output-buf))
  (dolist (buf (buffer-list))
    (when (and (buffer-file-name buf)
               (string-prefix-p temp-dir (buffer-file-name buf)))
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (kill-buffer buf))))
  (dorg-test--delete-owned-root temp-dir))

(defmacro dorg-test--with-suppressed-output (temp-dir &rest body)
  "Run BODY with output suppression; dump captured output on error.
TEMP-DIR is the root temp directory for cleanup."
  (declare (indent 1))
  `(let ((dorg-test--message-log nil)
         (inhibit-message t)
         (standard-output (generate-new-buffer " *dorg-test-output*")))
     (unwind-protect
         (condition-case err
             (progn ,@body)
           (error
            (dorg-test--dump-captured-output standard-output)
            (signal (car err) (cdr err))))
       (dorg-test--cleanup-temp ,temp-dir standard-output))))

(defmacro dorg-test-with-temp-dir (&rest body)
  "Execute BODY in a temporary directory with output suppression.
Binds `default-directory' to a fresh temp dir.  Suppresses messages
and stdout during execution; dumps captured output on test failure."
  (declare (indent 0) (debug t))
  `(let ((temp-dir (dorg-test--register-owned-root
                    (make-temp-file "dorg-test-" t)
                    "with-temp-dir")))
     (dorg-test--with-suppressed-output temp-dir
       (let ((default-directory (file-name-as-directory temp-dir)))
         ,@body))))

;;; ─── Scratch Tracker Isolation ────────────────────────────────────────

(defmacro dorg-test--with-isolated-tracker (root-var &rest body)
  "Run BODY with an isolated scratch tracker root bound to ROOT-VAR.
Creates a fresh temp directory and binds ROOT-VAR to it (with a trailing
slash).  For the dynamic extent of BODY:
  - `EMBEAD_DIR'/`EMBEAD_FILE' point at a tracker under ROOT-VAR and
    `EMBEAD_ON'/`EMBEAD_AGENT' are cleared, so a downstream-tool
    subprocess a scratch harness spawns cannot inherit a live tracker
    via the environment;
  - `dorg--binding-keyword-write-guard' is armed to ROOT-VAR, so any
    `#+DORG_DOLT_DIR:'/`#+DORG_HASH:' write aimed outside ROOT-VAR fails
    loudly instead of contaminating a live file.
Prior environment is restored, the dynamically bound guard unwinds on its
own, and the temp directory is deleted afterward."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tracker-dir (make-symbol "tracker-dir"))
        (tracker-file (make-symbol "tracker-file"))
        (prior-dir (make-symbol "prior-dir"))
        (prior-file (make-symbol "prior-file"))
        (prior-on (make-symbol "prior-on"))
        (prior-agent (make-symbol "prior-agent")))
    `(let* ((,root-var (dorg-test--register-owned-root
                       (file-name-as-directory (make-temp-file "dorg-iso-" t))
                       "isolated-tracker"))
            (,tracker-dir (expand-file-name ".dorg/" ,root-var))
            (,tracker-file (expand-file-name "beads.org" ,tracker-dir))
            (,prior-dir (getenv "EMBEAD_DIR"))
            (,prior-file (getenv "EMBEAD_FILE"))
            (,prior-on (getenv "EMBEAD_ON"))
            (,prior-agent (getenv "EMBEAD_AGENT"))
            (dorg--binding-keyword-write-guard ,root-var))
       (make-directory ,tracker-dir t)
       (unwind-protect
           (progn
             ;; These EMBEAD_* env-var names are an intentional survivor: the
             ;; real isolation contract that shields `make test' from a live
             ;; tracker dev-env.  Renaming them would re-introduce that leak.
             (setenv "EMBEAD_DIR" ,tracker-dir)
             (setenv "EMBEAD_FILE" ,tracker-file)
             (setenv "EMBEAD_ON" nil)
             (setenv "EMBEAD_AGENT" nil)
             ,@body)
         (setenv "EMBEAD_DIR" ,prior-dir)
         (setenv "EMBEAD_FILE" ,prior-file)
         (setenv "EMBEAD_ON" ,prior-on)
         (setenv "EMBEAD_AGENT" ,prior-agent)
         (dorg-test--delete-owned-root ,root-var)))))

;;; ─── Message Log ──────────────────────────────────────────────────────

(defmacro dorg-test-with-messages (&rest body)
  "Execute BODY while capturing messages.
Bind `dorg-test--message-log' to a list during execution."
  (declare (indent 0) (debug t))
  `(let ((dorg-test--message-log nil))
     ,@body
     (nreverse dorg-test--message-log)))

;;; ─── Domain Object Query Helpers ─────────────────────────────────────

(defun dorg-test--root-heading (headings)
  "Return the level-0 root heading from HEADINGS list."
  (cl-find-if (lambda (h) (= (dorg-heading-level h) 0)) headings))

(defun dorg-test--find-heading (headings title)
  "Return the first heading in HEADINGS with TITLE."
  (cl-find-if (lambda (h) (equal (dorg-heading-title h) title)) headings))

(defun dorg-test--headings-at-level (headings level)
  "Return all headings at LEVEL from HEADINGS list."
  (cl-remove-if-not (lambda (h) (= (dorg-heading-level h) level)) headings))

(defun dorg-test--items-of-type (heading type)
  "Return items of EIEIO TYPE from HEADING's items list."
  (cl-remove-if-not (lambda (item) (cl-typep item type))
                     (dorg-heading-items heading)))

;;; ─── Fixture Repo Template ──────────────────────────────────────────

(defvar dorg-test--template-dir nil
  "Path to the cached initialized dorg repo template.
Lazily created by `dorg-test--ensure-template', deleted on exit.")

(defun dorg-test--ensure-template ()
  "Ensure the template repo exists, creating it if needed.
Returns the template directory path."
  (unless (and dorg-test--template-dir
               (file-directory-p dorg-test--template-dir))
    (let* ((dir (make-temp-file "dorg-template-" t))
           (dolt-dir (expand-file-name "dolt/" dir)))
      ;; The template is a STANDALONE store no sql-server holds, and its copies
      ;; are used the same way.  Build it through the sanctioned test/recovery
      ;; CLI seam (`dorg-sql--with-cli-seam') rather than the `auto'/`cli'
      ;; policy the server-only collapse (tracked-case) deletes, so the build
      ;; stays policy-independent regardless of the suite's global default.
      (dorg-sql--with-cli-seam dolt-dir
        (dorg-init dolt-dir))
      (setq dorg-test--template-dir dir)))
  dorg-test--template-dir)

(defun dorg-test--copy-repo (dest &optional subdir)
  "Copy the template repo to DEST/SUBDIR/.
SUBDIR defaults to \"dolt\".  DEST must be an existing directory.
Returns the dolt-dir path."
  (let* ((template (dorg-test--ensure-template))
         (src (expand-file-name "dolt/" template))
         (dst (expand-file-name (or subdir "dolt/") dest)))
    (copy-directory src dst nil t t)
    ;; A copied fixture is a standalone store.  The template may have
    ;; transient sql-server metadata from bootstrap/autostart tests; copying
    ;; that into a new repo makes discovery attach to the template's server.
    (dolist (file (list (expand-file-name ".dolt/sql-server.info" dst)
                        (dorg-dolt-server--claim-file dst)
                        (concat (dorg-dolt-server--claim-file dst) ".lock")))
      (when (file-exists-p file)
        (if (file-directory-p file)
            (delete-directory file t)
          (delete-file file))))
    dst))

(defun dorg-test--cleanup-template ()
  "Delete the cached template repo if it exists."
  (when (and dorg-test--template-dir
             (file-directory-p dorg-test--template-dir))
    (dorg-test--delete-owned-root dorg-test--template-dir)
    (setq dorg-test--template-dir nil)))

;;; ─── Parse + Seed Helper ────────────────────────────────────────────

(defun dorg-test--fill-level-offsets (headings)
  "Set each nil level offset in HEADINGS from its parent in the same list.
Fixtures that build headings with absolute levels only get the offset the
store would hold: 0 for a parentless root, else the level minus the parent's
level when that parent is in HEADINGS.  An offset already set is kept, and
one that cannot be computed stays nil.  Returns HEADINGS."
  (let ((by-id (make-hash-table :test 'eql)))
    (dolist (h headings) (puthash (dorg-heading-id h) h by-id))
    (dolist (h headings headings)
      (unless (dorg-heading-level-offset h)
        (let ((parent (and (dorg-heading-parent-id h)
                           (gethash (dorg-heading-parent-id h) by-id))))
          (setf (dorg-heading-level-offset h)
                (cond ((null (dorg-heading-parent-id h)) 0)
                      ((and parent (dorg-heading-level parent)
                            (dorg-heading-level h))
                       (- (dorg-heading-level h)
                          (dorg-heading-level parent))))))))))

(defun dorg-test--parse-to-sql (contents &optional root-id skip-root)
  "Parse org CONTENTS string and return SQL INSERT statements.
Uses a deterministic sequential ID generator.  ROOT-ID defaults
to a generated value from the same counter (starting at 0).
When SKIP-ROOT is non-nil, omit the level-0 root heading INSERT
\(use when the root already exists in the DB, e.g. from `dorg-bind-dolt').
Returns a list of SQL strings suitable for `dorg-sql-batch'."
  (require 'dorg-parse)
  (require 'dorg-sql-gen)
  (let* ((counter (or root-id 0))
         (gen (lambda () (cl-incf counter))))
    (with-temp-buffer
      (insert contents)
      (org-mode)
      (goto-char (point-min))
      (let* ((result (dorg-parse-buffer nil root-id))
             (_writeback (dorg-parse-assign-ids result gen))
             (headings (dorg-parse-result-headings result))
             (rid (dorg-parse-result-root-id result))
             (stmts nil))
        (dolist (h headings)
          (unless (and skip-root (= (dorg-heading-level h) 0))
            (setq stmts (nconc stmts
                                (dorg-sql-gen-heading-full-sql h rid gen)))))
        stmts))))

;;; ─── Typed mutation patch helpers ──────────────────────────────────

(defun dorg-test--heading-address (id)
  "Return a typed heading address for ID."
  (list :heading :id id))

(defun dorg-test--subtree-address (id)
  "Return a typed subtree address for ID."
  (list :subtree :id id))

(defun dorg-test--external-address (kind key value)
  "Return a typed external KEY/VALUE address of KIND."
  (list kind :extern t :key key :value value))

(defun dorg-test--property-put-patch (host key value)
  "Return a typed property-put patch for HOST."
  (list :op 'put :target (list :property :host host :name key) :value value))

(defun dorg-test--property-delete-patch (host key)
  "Return a typed property-delete patch for HOST."
  (list :op 'delete :target (list :property :host host :name key)))

(defun dorg-test--heading-update-patch (id &rest fields)
  "Return a typed heading update patch for ID and FIELDS."
  (list :op 'update :target (dorg-test--heading-address id) :fields fields))

(defun dorg-test--planning-patch (id &rest fields)
  "Return a typed planning update patch for ID and FIELDS."
  (append (list :op 'update
                :target (list :planning :host
                              (dorg-test--heading-address id)))
          fields))

(defun dorg-test--tree-insert-patch (under heading &rest anchors)
  "Return a typed tree-insert patch below UNDER with HEADING and ANCHORS."
  (list :op 'insert :target (append (list :tree :under under) anchors)
        :heading heading))

(defun dorg-test--delete-patch (id &optional subtree)
  "Return a typed heading or SUBTREE delete patch for ID."
  (list :op 'delete
        :target (list (if subtree :subtree :heading) :id id)))

(defun dorg-test--move-patch (id under &rest fields)
  "Return a typed subtree move patch for ID beneath UNDER."
  (append (list :op 'move
                :target (if (consp id) id (dorg-test--subtree-address id))
                :to (list :under under))
          fields))

(defun dorg-test--move-before-patch (id under before &rest fields)
  "Return a typed subtree move patch for ID below UNDER and before BEFORE."
  (append (list :op 'move
                :target (if (consp id) id (dorg-test--subtree-address id))
                :to (list :under under :before before))
          fields))

(defun dorg-test--keyword-patch (verb host key &rest fields)
  "Return a current typed keyword insertion or all-occurrences patch."
  (append (list :op verb :target (append (list :keyword :host host :name key)
                                       (unless (eq verb 'insert)
                                         '(:select (:all)))))
          fields))

(defun dorg-test--keyword-occurrence-patch
    (verb host key occurrence &rest fields)
  "Return a typed single-keyword occurrence patch."
  (append (list :op verb
                :target (list :keyword :host host :name key
                              :select (list :occurrence occurrence)))
          fields))

(defun dorg-test--items-replace-patch (host items)
  "Return a typed whole-item-sequence replacement patch."
  (list :op 'replace :target (list :item :host host :select '(:all)) :items items))

(defun dorg-test--plan-patches (source patches &rest options)
  "Plan typed PATCHES for SOURCE with engine OPTIONS."
  (apply #'dorg-mutate--plan-patches source patches nil options))

(defun dorg-test--apply-patches (source patches &rest options)
  "Apply typed PATCHES to SOURCE with engine OPTIONS."
  (apply #'dorg-mutate--apply-patches source patches nil options))

(provide 'dorg-test-helpers)
;;; dorg-test-helpers.el ends here
