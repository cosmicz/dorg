;;; dorg-cli-tests.el --- Tests for the dorg CLI app -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-app.el' (the `bin/dorg' Clime app) and its build /
;; vendoring / compatibility contract.  Coverage:
;;
;;   - Pure helpers: `dorg-app--hit', `dorg-app--text', the `--json'
;;     envelope finalizer `dorg-app--json-finalize' and the server helpers
;;     `dorg-app--dolt-json' /
;;     `dorg-app--dolt-target'.
;;   - E2E (subprocess) reads: text output, convenience-subcommand parity,
;;     the strict `--json' array for 0/1/N hits, source selection via the
;;     `DORG_FILE' env var (and explicit `--file' override), and the clean
;;     nonzero exits for malformed sexp / missing `--file'.
;;   - E2E (subprocess) write: `set' is a non-vacuous transformation (the
;;     property is absent before and present after re-reading the file),
;;     `set --json' is a single OBJECT, and a DORG_ID write is rejected
;;     with identity intact.
;;   - E2E (subprocess) server: read-only `dolt list' returns an empty
;;     enveloped server list when nothing is managed, `dolt stop-all' is a clean object, and a
;;     target-less `dolt start' is a clean nonzero usage error (never a
;;     minibuffer prompt).
;;   - Build/vendoring: `lib/clime' holds exactly the CLI subset, the
;;     committed `bin/dorg' is executable with only relative `-L' flags,
;;     and `make bin/dorg' regenerates the wrapper from `dorg-app.el'
;;     when a local `clime-make' is available.
;;   - Core honesty: `(require \\='dorg)' loads with NO clime on the load
;;     path — core gains no accidental Clime dependency.
;;   - Dolt-gated: CLI writes use typed mutation/import seams so a fresh
;;     DB-backed query sees the committed edit.
;;
;; The app `dorg-cli' is loaded in-process (for helper/introspection
;; tests); end-to-end behavior is exercised through a real Emacs
;; subprocess so the actual CLI dispatch + exit codes are covered.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'dorg)
(require 'dorg-test-helpers)        ;; `dorg-test--with-temp-db' (doctor CLI tests)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Paths ────────────────────────────────────────────────────────────

(defconst dorg-test-cli--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

;; Load the CLI app for in-process helper/introspection tests.  The app
;; file is `dorg-app.el' at repo root (feature `dorg-app', app symbol
;; `dorg-cli').  The trailing entrypoint is a no-op here (CLIME_MAIN_APP is
;; unset in the test process, so `clime-main-script-p' returns nil).
(require 'dorg-app)

(defun dorg-test-cli--path (rel)
  "Return the absolute path of REL under the repo root."
  (expand-file-name rel dorg-test-cli--root))

(defun dorg-test-cli--find-clime-make ()
  "Return a usable clime-make path for build-target tests, or nil."
  (or (executable-find "clime-make")
      (let ((local (dorg-test-cli--path "lib/clime/bin/clime-make"))
            (sibling (expand-file-name "../clime/bin/clime-make"
                                       dorg-test-cli--root)))
        (cond
         ((file-executable-p local) local)
         ((file-executable-p sibling) sibling)))))

;;; ─── Subprocess invoker ───────────────────────────────────────────────

(defun dorg-test-cli--invoke (args &optional extra-env preload-form)
  "Run the dorg app with ARGS (a list of strings); return (CODE . STDOUT).
Uses the running Emacs + `dorg-app.el' so the test is hermetic and does
not depend on \"emacs\" being on PATH.  EXTRA-ENV is a list of
\"NAME=VALUE\" strings prepended to `process-environment'.  Stderr is
discarded so STDOUT carries only the CLI's structured/text output.
When PRELOAD-FORM is non-nil, evaluate it in the fresh child immediately
before loading `dorg-app.el'.  This is reserved for process-boundary
failure injection that cannot be represented by environment variables.

Scopes the child's cross-process server index (tracked-regression): prepends
DORG_DOLT_SERVER_INDEX_DIR=<isolated dir> so a foreign/live index reachable
via the implicit environment cannot leak into the child — UNLESS the caller
already pins one in EXTRA-ENV, in which case the caller override wins."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (index-env
          (unless (cl-some (lambda (e)
                             (string-prefix-p "DORG_DOLT_SERVER_INDEX_DIR=" e))
                           extra-env)
            (list (concat "DORG_DOLT_SERVER_INDEX_DIR="
                          (dorg-test--isolate-server-index)))))
         (process-environment (append (list "CLIME_MAIN_APP=dorg-cli")
                                      extra-env index-env process-environment))
         (default-directory dorg-test-cli--root))
    (with-temp-buffer
      (let* ((preload-args
              (and preload-form
                   (list "--eval" (prin1-to-string preload-form))))
             (child-args
              (append (list "--batch" "-Q"
                            "-L" dorg-test-cli--root
                            "-L" (dorg-test-cli--path "lib/clime"))
                      preload-args
                      (list "-l" (dorg-test-cli--path "dorg-app.el") "--")
                      args))
             (code (apply #'call-process emacs nil
                          (list (current-buffer) nil) nil child-args)))
        (cons code (buffer-string))))))

(defun dorg-test-cli--json (string)
  "Parse STRING as a JSON object alist with arrays as lists."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object :json-false))

(defun dorg-test-cli--json-data (string)
  "Return the shared-envelope data object from JSON STRING."
  (alist-get 'data (dorg-test-cli--json string)))

(defun dorg-test-cli--json-error (string)
  "Return the shared-envelope error object from JSON STRING."
  (alist-get 'error (dorg-test-cli--json string)))

(defvar dorg-test-cli--reader-side-effect nil
  "Sentinel changed only if a reader incorrectly evaluates a `#.' form.")

(defun dorg-test-cli--reader-error (text command)
  "Return the structured reader-error payload for TEXT and COMMAND."
  (cdr (should-error (dorg-app--read-exact-form text command)
                     :type 'dorg-app-error)))

(defun dorg-test-cli--count-regexp (regexp string)
  "Return the number of non-overlapping REGEXP matches in STRING."
  (let ((start 0)
        (count 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun dorg-test-cli--file-bytes (file)
  "Return FILE contents literally as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun dorg-test-cli--single-json-error (result reason &optional detail)
  "Assert RESULT is one nonzero JSON error with REASON and optional DETAIL.
Return the decoded error object."
  (should-not (= 0 (car result)))
  (let ((lines (split-string (cdr result) "\n" t)))
    (should (= 1 (length lines)))
    (let* ((err (dorg-test-cli--json-error (car lines)))
           (details (alist-get 'details err)))
      (should (equal (symbol-name reason)
                     (format "%s" (alist-get 'reason err))))
      (when detail
        (should (equal (symbol-name detail)
                       (format "%s" (alist-get 'detail details)))))
      err)))

(ert-deftest dorg-test-cli-batch-org-persist-hooks-removed ()
  "Batch CLI load neutralizes Org persist hooks but keeps DOrg cleanup."
  (let* ((temp-home (make-temp-file "dorg-org-persist-home-" t))
         (process-environment
          (append (list "CLIME_MAIN_APP="
                        "CLIME_ARGV0="
                        (concat "HOME=" temp-home))
                  process-environment))
         (expr
          '(progn
             (require 'dorg-app)
             (require 'org-element)
             (when (and (boundp 'org-element-cache-persistent)
                        org-element-cache-persistent)
               (error "org-element-cache-persistent remained enabled"))
             (let (org-persist-hooks)
               (dolist (hook kill-emacs-hook)
                 (when (and (symbolp hook)
                            (string-prefix-p "org-persist-"
                                             (symbol-name hook)))
                   (push hook org-persist-hooks)))
               (when org-persist-hooks
                 (error "org-persist kill hooks remain: %S"
                        (nreverse org-persist-hooks))))
             ;; Crash-safe (dorg-fatalex): DOrg cleanup must NOT ride the
             ;; fatal `kill-emacs-hook'; ordinary-exit reaping moved to a
             ;; named `kill-emacs-query-functions' entry instead.
             (when (memq 'dorg-dolt-server-stop-all kill-emacs-hook)
               (error "dorg-dolt-server-stop-all still on fatal kill-emacs-hook"))
             (unless (memq 'dorg-dolt-server--exit-cleanup
                           kill-emacs-query-functions)
               (error "dorg-dolt-server--exit-cleanup missing from query functions"))
             (kill-emacs 0)))
         (args (list "--batch" "-Q"
                     "-L" dorg-test-cli--root
                     "-L" (dorg-test-cli--path "lib/clime")
                     "--eval" (prin1-to-string expr)))
         (output-buf (generate-new-buffer " *dorg-org-persist-probe*"))
         (emacs (expand-file-name invocation-name invocation-directory))
         exit-code)
    (unwind-protect
        (progn
          (let ((default-directory dorg-test-cli--root))
            (setq exit-code
                  (apply #'call-process emacs nil output-buf nil args)))
          (unless (equal 0 exit-code)
            (message "org-persist probe output:\n%s"
                     (with-current-buffer output-buf (buffer-string))))
          (should (equal 0 exit-code)))
      (ignore-errors (delete-directory temp-home t))
      (when (buffer-live-p output-buf)
        (kill-buffer output-buf)))))

(ert-deftest dorg-test-cli-serve-start-preserves-org-persist-hooks ()
  "The long-lived `serve start' path keeps Org persist behavior intact."
  (let* ((temp-home (make-temp-file "dorg-serve-org-persist-home-" t))
         (process-environment
          (append (list "CLIME_MAIN_APP="
                        "CLIME_ARGV0="
                        (concat "HOME=" temp-home))
                  process-environment))
         (expr
          '(progn
             (setq command-line-args-left '("serve" "start"))
             (require 'dorg-app)
             (require 'org-element)
             (unless (and (boundp 'org-element-cache-persistent)
                          org-element-cache-persistent)
               (error "org-element-cache-persistent was disabled"))
             (let (org-persist-hooks)
               (dolist (hook kill-emacs-hook)
                 (when (and (symbolp hook)
                            (string-prefix-p "org-persist-"
                                             (symbol-name hook)))
                   (push hook org-persist-hooks)))
               (unless (and (memq 'org-persist-write-all org-persist-hooks)
                            (memq 'org-persist-gc org-persist-hooks))
                 (error "org-persist kill hooks were removed: %S"
                        (nreverse org-persist-hooks))))
             (kill-emacs 0)))
         (args (list "--batch" "-Q"
                     "-L" dorg-test-cli--root
                     "-L" (dorg-test-cli--path "lib/clime")
                     "--eval" (prin1-to-string expr)))
         (output-buf (generate-new-buffer " *dorg-serve-org-persist-probe*"))
         (emacs (expand-file-name invocation-name invocation-directory))
         exit-code)
    (unwind-protect
        (progn
          (let ((default-directory dorg-test-cli--root))
            (setq exit-code
                  (apply #'call-process emacs nil output-buf nil args)))
          (unless (equal 0 exit-code)
            (message "org-persist serve probe output:\n%s"
                     (with-current-buffer output-buf (buffer-string))))
          (should (equal 0 exit-code)))
      (ignore-errors (delete-directory temp-home t))
      (when (buffer-live-p output-buf)
        (kill-buffer output-buf)))))

(ert-deftest dorg-test-cli-serve-start-command-p-positional ()
  "`dorg-app--serve-start-command-p' detects `serve start' positionally.
It must skip the known global option flags (value-taking --file/-f/
--dolt-dir/--root-id, boolean --json, inline --file=/--dolt-dir=/
--root-id=) to reach the first two positional tokens, and must NOT be
fooled by another subcommand preceding `serve' — the failure mode of the
deleted hand-maintained `dorg-app--command-names' list."
  (cl-flet ((detect (argv)
              (let ((command-line-args-left (copy-sequence argv)))
                (dorg-app--serve-start-command-p))))
    ;; Positive: bare and behind every global-flag shape.
    (should (detect '("serve" "start")))
    (should (detect '("--json" "serve" "start")))
    (should (detect '("--file" "/x/tracker.org" "serve" "start")))
    (should (detect '("-f" "/x/tracker.org" "serve" "start")))
    (should (detect '("--dolt-dir" "/x/dolt" "serve" "start")))
    (should (detect '("--file=/x/tracker.org" "--json" "serve" "start")))
    (should (detect '("--dolt-dir=/x/dolt" "serve" "start")))
    ;; --root-id is a value option (clime-opt root-id): its separate-form
    ;; value token must be consumed, so a following `serve start' is still
    ;; detected, and the equals form is self-contained.
    (should (detect '("--root-id" "5" "serve" "start")))
    (should (detect '("--root-id=5" "serve" "start")))
    ;; Negative: not serve start.
    (should-not (detect '("serve")))
    (should-not (detect '("serve" "status")))
    (should-not (detect '("query")))
    (should-not (detect '("dolt" "start")))
    ;; Regression: `doctor' (and any non-serve command) preceding a
    ;; `serve start' tail must NOT register as serve start.  The former
    ;; list-based scan skipped unknown positionals and would misfire here.
    (should-not (detect '("doctor")))
    (should-not (detect '("doctor" "serve" "start")))
    ;; NAV BLOCKER: --root-id SEPARATE form consumes the NEXT token as its
    ;; value.  `--root-id serve start' means value="serve", positional
    ;; command="start" — NOT serve start.  The deleted command-names scan
    ;; skipped the unknown "--root-id" and read "serve" as the command,
    ;; misfiring here.  This is the primary RED case for the value option.
    (should-not (detect '("--root-id" "serve" "start")))))

;;; ─── Crash-safe serve exit cleanup (dorg-fatalex, AC #1/#2) ─────────────
;;
;; The `serve start' daemon must not hang its owner-safe cleanup off the
;; fatal `kill-emacs-hook' (a SIGTERM-during-GC abort would skip it and, if
;; it allocated, crash).  On a fatal signal DOrg runs NO cleanup Lisp; the
;; durable claim then persists and stays fail-closed — foreign/apparently-dead
;; metadata requires explicit later policy or operator recovery, never
;; automatic raw-PID reaping.  Ordinary interactive exit runs a NAMED
;; `kill-emacs-query-functions' entry that stops the LOCAL clime-serve and
;; removes this owner`s claim ONLY when that stop confirms the exact local
;; server is gone (never a raw-PID signal), guarding each seam against `error'
;; and `quit'.

(require 'find-func)

(defun dorg-test-cli--add-hook-forms (source hook)
  "Return every `(add-hook 'HOOK ...)' FORM in SOURCE.
Collects every registration (named or anonymous) so a hidden lambda on
the fatal hook cannot slip past the scan.  The full form is returned so
callers can inspect the target (nth 2) and the DEPTH argument (nth 3)."
  (let ((max-lisp-eval-depth 10000)
        forms)
    (cl-labels ((walk (form)
                  (when (consp form)
                    (when (and (eq (car-safe form) 'add-hook)
                               (equal (nth 1 form) (list 'quote hook)))
                      (push form forms))
                    (walk (car form))
                    (walk (cdr form)))))
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (nreverse forms)))

(defun dorg-test-cli--named-target-p (target)
  "Non-nil iff TARGET is a literal named function symbol (#'SYM or 'SYM)."
  (and (memq (car-safe target) '(function quote))
       (symbolp (nth 1 target))
       (not (memq (nth 1 target) '(lambda nil)))))

(ert-deftest dorg-test-cli-serve-no-fatal-kill-emacs-hook ()
  "The serve lifecycle registers no cleanup on the fatal `kill-emacs-hook'."
  (let ((forms (dorg-test-cli--add-hook-forms
                (find-library-name "dorg-app") 'kill-emacs-hook)))
    (should (null forms))))

(ert-deftest dorg-test-cli-serve-exit-cleanup-named-query-function ()
  "Serve ordinary-exit cleanup is a NAMED, LATE (depth 99) query entry.
Every registration must be a literal named symbol (an anonymous lambda
capturing port/owner cannot satisfy the crash-safe named contract), and
ours must register at late depth so it runs only after other exit queries
agree."
  (let ((forms (dorg-test-cli--add-hook-forms
                (find-library-name "dorg-app") 'kill-emacs-query-functions)))
    (should forms)
    (dolist (form forms)
      (should (dorg-test-cli--named-target-p (nth 2 form))))
    (let ((ours (cl-find-if
                 (lambda (f)
                   (eq (nth 1 (nth 2 f)) 'dorg-app--serve-exit-cleanup))
                 forms)))
      (should ours)
      (should (equal (nth 3 ours) 99))))
  (should (fboundp 'dorg-app--serve-exit-cleanup)))

(ert-deftest dorg-test-cli-serve-exit-cleanup-runs-after-queries-agree ()
  "Late depth 99 means an earlier exit-query veto cancels exit BEFORE the
serve cleanup stops the local server.  `kill-emacs-query-functions' run
under `run-hook-with-args-until-failure', so a nil from an earlier
lower-depth query stops the run without reaching our cleanup."
  (let ((kill-emacs-query-functions nil)
        (ran nil))
    (cl-letf (((symbol-function 'dorg-app--serve-exit-cleanup)
               (lambda () (setq ran t) t)))
      (add-hook 'kill-emacs-query-functions #'dorg-app--serve-exit-cleanup 99)
      (add-hook 'kill-emacs-query-functions (lambda () nil))
      (should-not (run-hook-with-args-until-failure 'kill-emacs-query-functions))
      (should-not ran))))

(ert-deftest dorg-test-cli-serve-exit-cleanup-seam-isolation ()
  "Serve ordinary-exit cleanup keeps the claim when `clime-serve-stop' cannot
confirm the exact local server is dead (error/quit), removes it only on a
confirmed clean stop, and always returns non-nil so it never vetoes exit."
  ;; Unconfirmed stop (error or quit): the local server`s death is NOT proven,
  ;; so the claim MUST remain (fail-closed) while exit is never vetoed.
  (dolist (mode '(:error :quit))
    (let ((dorg-app--serve-exit-spec (list :server (list (quote srv)) :port 4321 :owner "owner-tok"))
          (removed nil))
      (cl-letf (((symbol-function 'clime-serve-stop)
                 (lambda (&rest _)
                   (if (eq mode :error) (error "stop boom") (signal 'quit nil))))
                ((symbol-function 'dorg-app--serve-remove-claim)
                 (lambda (port owner) (setq removed (list port owner)))))
        (let ((ret (dorg-app--serve-exit-cleanup)))
          (should ret)
          (should (null removed))))))
  ;; Confirmed clean stop: token-checked claim removal runs.
  (let ((dorg-app--serve-exit-spec (list :server (list (quote srv)) :port 4321 :owner "owner-tok"))
        (removed nil))
    (cl-letf (((symbol-function 'clime-serve-stop) (lambda (&rest _) t))
              ((symbol-function 'dorg-app--serve-remove-claim)
               (lambda (port owner) (setq removed (list port owner)))))
      (let ((ret (dorg-app--serve-exit-cleanup)))
        (should ret)
        (should (equal removed (list 4321 "owner-tok")))))))

(ert-deftest dorg-test-cli-serve-exit-cleanup-stops-exact-server ()
  "Serve exit cleanup stops the EXACT retained `clime-serve' object, never a
no-arg stop that could reap an unrelated server sharing this process."
  (let* ((mine (list 'my-server))
         (other (list 'other-server))
         (dorg-app--serve-exit-spec (list :server mine :port 4321 :owner "tok"))
         (stopped nil)
         (removed nil))
    (cl-letf (((symbol-function 'clime-serve-stop)
               (lambda (&optional srv) (push srv stopped)))
              ((symbol-function 'dorg-app--serve-remove-claim)
               (lambda (port owner) (setq removed (list port owner)))))
      (should (dorg-app--serve-exit-cleanup))
      ;; Exactly one stop, targeting OUR exact object by identity.
      (should (= 1 (length stopped)))
      (should (eq (car stopped) mine))
      ;; The unrelated server object was never touched.
      (should-not (memq other stopped))
      ;; Token-checked claim removal ran after the confirmed exact stop.
      (should (equal removed (list 4321 "tok"))))))

(ert-deftest dorg-test-cli-serve-start-claim-write-failure-confirmed-stop ()
  "Publication failure + a CONFIRMED clean stop: serve start stops the EXACT
server object it created (tracked-regression), then drops the retained spec AND
the query hook, and re-signals the original publication error."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir)
         (dorg-app--serve-exit-spec nil)
         (kill-emacs-query-functions nil)
         (mine (list 'my-server))
         (stopped nil))
    (unwind-protect
        (cl-letf (((symbol-function 'clime-serve) (lambda (&rest _) mine))
                  ((symbol-function 'dorg-app--serve-write-claim)
                   (lambda (&rest _) (error "claim write boom")))
                  ((symbol-function 'clime-serve-stop)
                   (lambda (&optional srv) (push srv stopped) t)))
          (let ((e (should-error
                    (dorg-app--serve-start dorg-cli 8099 "127.0.0.1" nil "tok"))))
            ;; The original publication error is surfaced.
            (should (string-match-p "claim write boom"
                                    (error-message-string e))))
          ;; The exact server was stopped by identity.
          (should (= 1 (length stopped)))
          (should (eq (car stopped) mine))
          ;; A confirmed stop drops both the spec and the query hook.
          (should (null dorg-app--serve-exit-spec))
          (should-not (memq 'dorg-app--serve-exit-cleanup
                            kill-emacs-query-functions)))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-start-claim-write-failure-stop-error-retains ()
  "Publication failure where the exact stop ALSO errors: serve start RETAINS
the spec + query hook for local recovery and surfaces BOTH conditions."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir)
         (dorg-app--serve-exit-spec nil)
         (kill-emacs-query-functions nil)
         (mine (list 'my-server)))
    (unwind-protect
        (cl-letf (((symbol-function 'clime-serve) (lambda (&rest _) mine))
                  ((symbol-function 'dorg-app--serve-write-claim)
                   (lambda (&rest _) (error "claim write boom")))
                  ((symbol-function 'clime-serve-stop)
                   (lambda (&optional _) (error "stop boom"))))
          (let ((e (should-error
                    (dorg-app--serve-start dorg-cli 8099 "127.0.0.1" nil "tok"))))
            ;; Neither failure is lost.
            (should (string-match-p "claim write boom" (error-message-string e)))
            (should (string-match-p "stop boom" (error-message-string e))))
          ;; The unconfirmed server is retained (spec + hook) for recovery.
          (should (eq (plist-get dorg-app--serve-exit-spec :server) mine))
          (should (= (plist-get dorg-app--serve-exit-spec :port) 8099))
          (should (memq 'dorg-app--serve-exit-cleanup
                        kill-emacs-query-functions)))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-start-claim-write-failure-stop-quit-retains ()
  "Publication failure where the exact stop QUITS (which `ignore-errors' would
miss): serve start RETAINS the spec + query hook and surfaces the original
publication condition."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir)
         (dorg-app--serve-exit-spec nil)
         (kill-emacs-query-functions nil)
         (mine (list 'my-server)))
    (unwind-protect
        (cl-letf (((symbol-function 'clime-serve) (lambda (&rest _) mine))
                  ((symbol-function 'dorg-app--serve-write-claim)
                   (lambda (&rest _) (error "claim write boom")))
                  ((symbol-function 'clime-serve-stop)
                   (lambda (&optional _) (signal 'quit nil))))
          (let ((e (should-error
                    (dorg-app--serve-start dorg-cli 8099 "127.0.0.1" nil "tok"))))
            (should (string-match-p "claim write boom"
                                    (error-message-string e))))
          ;; Unconfirmed (stop quit) → retained for recovery.
          (should (eq (plist-get dorg-app--serve-exit-spec :server) mine))
          (should (memq 'dorg-app--serve-exit-cleanup
                        kill-emacs-query-functions)))
      (delete-directory dir t))))

;;; ─── Serve start phase helpers (refuse / bind / publish) ──────────────

(ert-deftest dorg-test-cli-serve-refuse-running-json-object ()
  "`dorg-app--serve-refuse-running' returns the single already-running
refusal object on the --json surface and binds NOTHING."
  (let ((bound nil))
    (cl-letf (((symbol-function 'clime-out-format) (lambda () 'json))
              ((symbol-function 'clime-out-text) (lambda (&rest _) nil))
              ((symbol-function 'clime-serve)
               (lambda (&rest _) (setq bound t) 'srv)))
      (let ((obj (dorg-app--serve-refuse-running
                  8091 '((pid . 4321) (owner-token . "o")))))
        (should (eq (alist-get 'started obj) :json-false))
        (should (equal (alist-get 'reason obj) "already-running"))
        (should (= (alist-get 'pid obj) 4321))
        (should (= (alist-get 'port obj) 8091))
        ;; Refusal must not bind or block.
        (should-not bound)))))

(ert-deftest dorg-test-cli-serve-refuse-running-text-line ()
  "In text mode `dorg-app--serve-refuse-running' emits the already-running
line and returns nil (no JSON object)."
  (let ((line nil))
    (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text))
              ((symbol-function 'clime-out-text)
               (lambda (s) (setq line s))))
      (should-not (dorg-app--serve-refuse-running 8092 '((pid . 77))))
      (should (string-match-p "already running on port 8092 (pid 77)" line)))))

(ert-deftest dorg-test-cli-serve-bind-happy-path ()
  "`dorg-app--serve-bind' binds the EXACT server, retains it in the
exit-spec, registers the named query-function cleanup, writes the claim,
and returns the server object."
  (let ((mine (list 'my-server))
        (dorg-app--serve-exit-spec nil)
        (kill-emacs-query-functions nil)
        (wrote nil))
    (cl-letf (((symbol-function 'clime-serve) (lambda (&rest _) mine))
              ((symbol-function 'dorg-app--serve-write-claim)
               (lambda (port host token owner)
                 (setq wrote (list port host token owner)) t)))
      (let ((ret (dorg-app--serve-bind
                  'app 8093 "127.0.0.1" "tok" "owner-B" #'ignore)))
        (should (eq ret mine))
        (should (eq (plist-get dorg-app--serve-exit-spec :server) mine))
        (should (= (plist-get dorg-app--serve-exit-spec :port) 8093))
        (should (equal (plist-get dorg-app--serve-exit-spec :owner) "owner-B"))
        (should (memq 'dorg-app--serve-exit-cleanup
                      kill-emacs-query-functions))
        (should (equal wrote '(8093 "127.0.0.1" "tok" "owner-B")))))))

(defun dorg-test-cli--serve-publish-capture (port host loopback)
  "Run `dorg-app--serve-publish' capturing its `standard-output' emission.
Stubs `accept-process-output' to break the persistent loop after the one
startup emission and returns the captured string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (cl-letf (((symbol-function 'dorg-app--serve-token-file)
                 (lambda (p) (format "/run/tok-%d" p)))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (throw 'dorg-test-publish-done t))))
        (catch 'dorg-test-publish-done
          (dorg-app--serve-publish port host loopback))))
    (buffer-string)))

(ert-deftest dorg-test-cli-serve-publish-emits-once-then-blocks ()
  "`dorg-app--serve-publish' emits EXACTLY ONE startup line carrying the
started/pid/host/port/token-file keys (--json surface), then enters the
blocking loop (escaped via a stubbed `accept-process-output')."
  (cl-letf (((symbol-function 'clime-out-format) (lambda () 'json)))
    (let ((out (dorg-test-cli--serve-publish-capture 8095 "127.0.0.1" t)))
      ;; Exactly one newline-terminated emission.
      (should (= 1 (length (split-string (string-trim out) "\n" t))))
      (should (string-match-p "\"started\"" out))
      (should (string-match-p "\"pid\"" out))
      (should (string-match-p "\"host\"" out))
      (should (string-match-p "8095" out))
      (should (string-match-p "/run/tok-8095" out))
      ;; Loopback bind → no network-exposure warning.
      (should-not (string-match-p "network-exposed" out)))))

(ert-deftest dorg-test-cli-serve-publish-non-loopback-warns ()
  "A non-loopback bind adds the network-exposure warning to the emitted
startup line (text surface)."
  (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text)))
    (let ((out (dorg-test-cli--serve-publish-capture 8096 "0.0.0.0" nil)))
      (should (= 1 (length (split-string (string-trim out) "\n" t))))
      (should (string-match-p "non-loopback bind" out))
      (should (string-match-p "0.0.0.0:8096" out)))))

(defmacro dorg-test-cli--with-fixture (var &rest body)
  "Write a standard plain-org fixture to a temp file bound to VAR; run BODY.
The fixture has three headings with DORG_IDs 100/101/102 and tags."
  (declare (indent 1))
  `(let* ((temp-base (make-temp-file "dorg-cli-test-" t))
          (,var (expand-file-name "doc.org" temp-base)))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert "#+TITLE: Sample\n\n"
                     "* First task\n:PROPERTIES:\n:DORG_ID: 100\n:CATEGORY: red\n:END:\n\n"
                     "* Second task :work:\n:PROPERTIES:\n:DORG_ID: 101\n:CATEGORY: blue\n:END:\n\n"
                     "* Third task :work:\n:PROPERTIES:\n:DORG_ID: 102\n:CATEGORY: red\n:END:\n"))
           ,@body)
       (delete-directory temp-base t))))

;;; ─── Pure helpers ─────────────────────────────────────────────────────

(ert-deftest dorg-test-cli-exact-form-reader-accepts-one-form ()
  "The app reader accepts exactly one form plus trailing whitespace.
Atoms and nil are reader-valid and remain domain validators' concern."
  (should (equal '(tag "work")
                 (dorg-app--read-exact-form "  (tag \"work\") \n" 'query)))
  (should (eq 'id (dorg-app--read-exact-form "id" 'query)))
  (should (null (dorg-app--read-exact-form " nil \t" 'mutate))))

(ert-deftest dorg-test-cli-exact-form-reader-details-and-parity ()
  "Query and mutate share the exact three-detail invalid-form vocabulary."
  (dolist (case '(("  \t\n" empty-input)
                  ("(tag \"x\"" malformed-syntax)
                  ("; comment only\n" malformed-syntax)
                  ("(tag \"x\") (id 1)" trailing-input)
                  ("(tag \"x\") ; trailing comment\n" trailing-input)))
    (let* ((text (car case))
           (detail (cadr case))
           (query (dorg-test-cli--reader-error text 'query))
           (mutate (dorg-test-cli--reader-error text 'mutate)))
      (dolist (payload (list query mutate))
        (should (eq 'invalid-form (plist-get payload :reason)))
        (should (eq detail
                    (plist-get (plist-get payload :details) :detail)))
        (should (plist-get payload :usage)))
      (should (equal (plist-get query :reason)
                     (plist-get mutate :reason)))
      (should (equal (plist-get (plist-get query :details) :detail)
                     (plist-get (plist-get mutate :details) :detail))))))

(ert-deftest dorg-test-cli-exact-form-reader-disables-read-eval ()
  "A side-effecting `#.' form is malformed input and is never evaluated."
  (let ((dorg-test-cli--reader-side-effect nil))
    (let ((payload
           (dorg-test-cli--reader-error
            "#.(setq dorg-test-cli--reader-side-effect t)" 'query)))
      (should (eq 'malformed-syntax
                  (plist-get (plist-get payload :details) :detail)))
      (should-not dorg-test-cli--reader-side-effect))))

(ert-deftest dorg-test-cli-exact-form-reader-is-the-only-reader-seam ()
  "Production has exactly the vetted raw reader seams, each hardened.
Two are allowed: the CLI exact-form reader (dorg-app.el, `#.' disabled by
its reader harness) and the postimage evidence payload parser
\(dorg-mutate.el, tracked-regression, whose closed grammar bans `#' before the
reader runs — its static payload witness proves a `#.' payload is inert).
Any further raw read is a new unvetted seam and must fail here."
  (let ((raw-count 0))
    (dolist (file (directory-files dorg-test-cli--root t "\\`dorg-.*\\.el\\'"))
      (setq raw-count
            (+ raw-count
               (dorg-test-cli--count-regexp
                "(read-from-string\\_>"
                (dorg-test-cli--file-bytes file)))))
    (should (= 2 raw-count)))
  (let ((app (dorg-test-cli--file-bytes
              (dorg-test-cli--path "dorg-app.el"))))
    (should (= 2 (dorg-test-cli--count-regexp
                  "(dorg-app--read-exact-form\\_>" app)))
    (should-not (string-match-p ":conform[ \t\n]+(lambda.*read-from-string"
                                app))))

(ert-deftest dorg-test-cli-hit-shape ()
  "`dorg-app--hit' yields {heading-id,title,properties} with no marker."
  (let ((hit (dorg-app--hit
              (list :heading-id 7 :marker (point-marker)
                    :title "T" :properties '(("K" . "V"))))))
    (should (equal 7 (plist-get hit :heading-id)))
    (should (equal "T" (plist-get hit :title)))
    (should (equal '(("K" . "V")) (plist-get hit :properties)))
    (should-not (plist-member hit :marker))))

(ert-deftest dorg-test-cli-text-line ()
  "`dorg-app--text' is \"<id>\\t<title>\", em dash when id is nil."
  (should (equal "7\tT" (dorg-app--text
                         (list :heading-id 7 :title "T"))))
  (should (equal "—\tU" (dorg-app--text
                         (list :heading-id nil :title "U")))))

(ert-deftest dorg-test-cli-json-finalize-array-contract ()
  "`dorg-app--json-finalize' enforces the shared envelope contract."
  (let ((r (dorg-app--json-finalize nil nil nil)))
    (should (eq t (alist-get 'ok r)))
    (should (alist-get 'data r))
    (should (null (alist-get 'error r))))
  (let ((r (dorg-app--json-finalize '(((a . 1))) nil nil)))
    (should (eq t (alist-get 'ok r)))
    (should (= 1 (length (alist-get 'results (alist-get 'data r))))))
  (let ((r (dorg-app--json-finalize nil '(:heading-id 1 :persisted t) nil)))
    (should (eq t (alist-get 'ok r)))
    (should (equal 1 (alist-get 'heading-id (alist-get 'data r)))))
  (let ((r (dorg-app--json-finalize nil nil '("boom" "second"))))
    (should (eq :json-false (alist-get 'ok r)))
    (should (equal "boom" (alist-get 'message (alist-get 'error r))))))

(ert-deftest dorg-test-cli-json-format-declares-finalizer ()
  "The `--json' output-format declares the explicit custom finalizer."
  (let ((fmt (cl-find-if (lambda (f) (eq (clime-output-format-name f) 'json))
                         (clime-app-output-formats dorg-cli))))
    (should fmt)
    (should (eq (clime-output-format-finalize fmt)
                #'dorg-app--json-finalize))))

;;; ─── Privacy-safe invocation history (tracked-case) ────────────────────

(defun dorg-test-cli--history-records (file)
  "Return the JSON objects from invocation-history FILE."
  (if (not (file-exists-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      (mapcar #'dorg-test-cli--json
              (split-string (buffer-string) "\n" t)))))

(ert-deftest dorg-test-cli-history-control-resolution ()
  "History controls distinguish default, explicit path, and disable.
The `default' sentinel resolves XDG/HOME at event time; unset environment
defers, nonempty overrides, and an explicitly empty environment disables."
  (should (equal "" (getenv "DORG_CLI_HISTORY_FILE")))
  (should (boundp 'dorg-cli-history-file))
  (should (fboundp 'dorg-app--history-file))
  (let ((process-environment (copy-sequence process-environment))
        (dorg-cli-history-file 'default))
    (setenv "DORG_CLI_HISTORY_FILE" nil)
    (setenv "XDG_STATE_HOME" "/tmp/dorg-xdg-state")
    (setenv "HOME" "/tmp/dorg-home")
    (should (equal "/tmp/dorg-xdg-state/dorg/clime-invocations.jsonl"
                   (dorg-app--history-file)))
    (setenv "XDG_STATE_HOME" "")
    (should (equal "/tmp/dorg-home/.local/state/dorg/clime-invocations.jsonl"
                   (dorg-app--history-file)))
    (setq dorg-cli-history-file "/tmp/dorg-lisp-history.jsonl")
    (should (equal "/tmp/dorg-lisp-history.jsonl"
                   (dorg-app--history-file)))
    (setenv "DORG_CLI_HISTORY_FILE" "/tmp/dorg-env-history.jsonl")
    (should (equal "/tmp/dorg-env-history.jsonl"
                   (dorg-app--history-file)))
    (setenv "DORG_CLI_HISTORY_FILE" "")
    (should-not (dorg-app--history-file))
    (setenv "DORG_CLI_HISTORY_FILE" nil)
    (setq dorg-cli-history-file nil)
    (should-not (dorg-app--history-file))))

(ert-deftest dorg-test-cli-history-hook-and-closed-schema ()
  "The named DOrg hook writes only the closed metadata schema.
The sentinel occupies every sensitive event field available to the sink; it
must not occur in the serialized record.  Removing the app hook therefore
makes this integration assertion fail."
  (should (locate-library "clime-telemetry"))
  (require 'clime-telemetry)
  (should (fboundp 'dorg-app--log-invocation))
  (should (member #'dorg-app--log-invocation
                  (clime-app-on-invocation dorg-cli)))
  (let* ((sentinel "DORG_WBAIR_SECRET_SENTINEL")
         (event (clime-invocation-event--create
                 :app dorg-cli :surface 'cli :phase 'runtime-error
                 :argv (list sentinel)
                 :path '(query) :display-path '(query)
                 :params (list :secret sentinel)
                 :context (list :request sentinel :output sentinel
                                :return-value sentinel :environment sentinel
                                :source-content sentinel)
                 :exit-code 1 :error-type 'error :error-message sentinel
                 :start-time 1.0 :duration 0.25 :format nil))
         (record (clime-telemetry-event-record event))
         (encoded (json-encode record)))
    (should (equal '(timestamp app surface phase path exit_code duration
                               output_format error_type)
                   (mapcar #'car record)))
    (should (equal '(query) (alist-get 'path record)))
    (should-not (string-match-p (regexp-quote sentinel) encoded))))

(ert-deftest dorg-test-cli-history-cli-exit-paths-exactly-once ()
  "Version, help, usage, runtime, and no-handler paths emit once.
Subprocess cases exercise the real named DOrg hook.  A tiny in-process app
covers Clime's explicit `no-handler' phase, which DOrg currently folds into
ordinary group behavior rather than exposing as a production command."
  (let* ((dir (make-temp-file "dorg-cli-history-" t))
         (file (expand-file-name "events.jsonl" dir))
         (env (list (concat "DORG_CLI_HISTORY_FILE=" file) "DORG_FILE=")))
    (unwind-protect
        (progn
          (should (= 0 (car (dorg-test-cli--invoke '("--version") env))))
          (should (= 0 (car (dorg-test-cli--invoke '("--help") env))))
          (should (= 2 (car (dorg-test-cli--invoke
                             '("DORG_WBAIR_ARG_SECRET")
                             env))))
          (should (= 1 (car (dorg-test-cli--invoke
                             '("query" "(id 1)") env))))
          (let* ((records (dorg-test-cli--history-records file))
                 (phases (mapcar (lambda (r) (alist-get 'phase r)) records)))
            (should (= 4 (length records)))
            (should (equal '("version" "help" "usage-error" "runtime-error")
                           phases))
            (should-not (string-match-p "DORG_WBAIR_ARG_SECRET"
                                        (dorg-test-cli--file-bytes file)))))
      (delete-directory dir t)))
  (let (events)
    (let* ((app (clime-make-app
                 :name "history-no-handler"
                 :on-invocation (lambda (event) (push event events))))
           (code (clime-run app nil)))
      (should (= 0 code))
      (should (= 1 (length events)))
      (should (eq 'no-handler
                  (clime-invocation-event-phase (car events)))))))

(ert-deftest dorg-test-cli-history-serve-and-not-found ()
  "Authenticated serve dispatch records resolved and unknown routes once.
The unknown URL segment is untrusted and absent from the persisted record."
  (require 'clime-serve)
  (let* ((dir (make-temp-file "dorg-serve-history-" t))
         (file (expand-file-name "events.jsonl" dir))
         (dorg-cli-history-file file)
         (process-environment (copy-sequence process-environment)))
    (setenv "DORG_CLI_HISTORY_FILE" nil)
    (unwind-protect
        (progn
          (let ((ok (clime-serve--dispatch dorg-cli '("dolt" "list") nil))
                (missing (clime-serve--dispatch
                          dorg-cli '("DORG_WBAIR_URL_SECRET") nil)))
            (should (= 200 (plist-get ok :status)))
            (should (= 404 (plist-get missing :status))))
          (let ((records (dorg-test-cli--history-records file)))
            (should (= 2 (length records)))
            (should (equal "serve" (alist-get 'surface (car records))))
            (should (equal '("dorg-cli" "dolt" "list")
                           (alist-get 'path (car records))))
            (should (equal "not-found"
                           (alist-get 'phase (cadr records))))
            (should-not (alist-get 'path (cadr records)))
            (should-not (string-match-p "DORG_WBAIR_URL_SECRET"
                                        (dorg-test-cli--file-bytes file)))))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-history-observer-failure-isolated ()
  "A DOrg ledger path failure preserves output/code and handler cardinality."
  (let ((calls 0)
        (directory-as-file (make-temp-file "dorg-history-failure-" t))
        (process-environment (copy-sequence process-environment)))
    (setenv "DORG_CLI_HISTORY_FILE" nil)
    (unwind-protect
        (let* ((dorg-cli-history-file directory-as-file)
               (app (clime-make-app
                     :name "history-observer-failure"
                     :handler (lambda (_ctx) (cl-incf calls) "ok")
                     :on-invocation #'dorg-app--log-invocation))
               (result (with-output-to-string
                         (should (= 0 (clime-run app nil))))))
          (should (= 1 calls))
          (should (string-match-p "ok" result)))
      (delete-directory directory-as-file t))))

;;; ─── structured command-error channel (tracked-case) ───────────────────────
;; Dolt-free unit tests.  The mutable `dorg-app--pending-error-*' side channel
;; is replaced by a `dorg-app-error' payload carried ON the signalled condition
;; and routed by `dorg-app--with-json-error' to the JSON drains.

(ert-deftest dorg-test-cli-pending-error-globals-removed ()
  "The mutable pending-error globals no longer exist (hidden channel dropped)."
  (should-not (boundp 'dorg-app--pending-error-reason))
  (should-not (boundp 'dorg-app--pending-error-details)))

(ert-deftest dorg-test-cli-json-finalize-structured-error ()
  "`dorg-app--json-finalize' renders a structured payload error element with
reason/message/details, and still renders a bare message string with the
default reason `error'.  Non-vacuous: the two branches assert different reasons."
  (let* ((r (dorg-app--json-finalize
             nil nil
             (list (list :reason 'doctor-findings :message "boom"
                         :details '(:clean :json-false :count 2)))))
         (err (alist-get 'error r)))
    (should (eq :json-false (alist-get 'ok r)))
    (should (equal "doctor-findings" (format "%s" (alist-get 'reason err))))
    (should (equal "boom" (alist-get 'message err)))
    (should (eq :json-false (alist-get 'clean (alist-get 'details err)))))
  (let* ((r (dorg-app--json-finalize nil nil '("plain")))
         (err (alist-get 'error r)))
    (should (equal "error" (format "%s" (alist-get 'reason err))))
    (should (equal "plain" (alist-get 'message err)))
    (should-not (alist-get 'details err))))

(ert-deftest dorg-test-cli-json-error-structured ()
  "`dorg-app--json-error' emits reason/details from a structured payload, and
falls back to reason `error' for a bare string."
  (let* ((out (with-output-to-string
                (dorg-app--json-error (list :reason 'invalid-target
                                            :message "nope" :details '(:target 7)))))
         (err (dorg-test-cli--json-error out)))
    (should (equal "invalid-target" (format "%s" (alist-get 'reason err))))
    (should (equal "nope" (alist-get 'message err)))
    (should (equal 7 (alist-get 'target (alist-get 'details err)))))
  (let* ((out (with-output-to-string (dorg-app--json-error "bare")))
         (err (dorg-test-cli--json-error out)))
    (should (equal "error" (format "%s" (alist-get 'reason err))))
    (should (equal "bare" (alist-get 'message err)))))

(ert-deftest dorg-test-cli-command-error-signals-payload ()
  "`dorg-app--command-error' signals `dorg-app-error' carrying the payload."
  (let* ((err (should-error (dorg-app--command-error 'doctor-findings "m"
                                                     '(:clean t) t)
                            :type 'dorg-app-error))
         (p (cdr err)))
    (should (eq 'doctor-findings (plist-get p :reason)))
    (should (equal "m" (plist-get p :message)))
    (should (equal '(:clean t) (plist-get p :details)))
    (should (eq t (plist-get p :usage)))))

(ert-deftest dorg-test-cli-with-json-error-routes ()
  "`dorg-app--with-json-error' routes a `dorg-app-error' payload per surface:
served usage -> `clime-usage-error'; buffered json -> `clime-out--errors';
served server-fault (usage nil) -> accumulate (not usage); text -> `error'."
  (let ((json-fmt (cl-find-if (lambda (f) (eq (clime-output-format-name f) 'json))
                              (clime-app-output-formats dorg-cli))))
    ;; served CLIENT error (usage t) -> clime-usage-error carrying the payload -> 400
    (let* ((dorg-app--serving t)
           (clime-out--active-format json-fmt)
           (e (should-error
               (dorg-app--with-json-error
                 (dorg-app--command-error 'x "m" nil t))
               :type 'clime-usage-error)))
      (should (equal "m" (plist-get (cadr e) :message))))
    ;; buffered json, not serving -> payload lands on the error accumulator (exit 1)
    (let ((dorg-app--serving nil)
          (clime-out--active-format json-fmt)
          (clime-out--errors nil))
      (dorg-app--with-json-error (dorg-app--command-error 'x "m" nil t))
      (should (equal "m" (plist-get (car clime-out--errors) :message))))
    ;; served SERVER fault (usage nil) -> still accumulates (500/exit-1), not usage
    (let ((dorg-app--serving t)
          (clime-out--active-format json-fmt)
          (clime-out--errors nil))
      (dorg-app--with-json-error (dorg-app--command-error 'doctor-findings "m" nil nil))
      (should (equal "m" (plist-get (car clime-out--errors) :message))))
    ;; text surface -> a plain `error' with the message (streamed report exits 1)
    (let* ((dorg-app--serving nil)
           (clime-out--active-format clime-output-default-format)
           (e (should-error
               (dorg-app--with-json-error
                 (dorg-app--command-error 'x "boom" nil nil))
               :type 'error)))
      (should (equal "boom" (cadr e))))))

(ert-deftest dorg-test-cli-query-path-never-routes-to-sql-cli ()
  "A real CLI query facade against a Dolt binding routes through mysql.
Non-vacuous: the test keeps `dorg-app--emit-query' and
`dorg-command-run-query' real, gives the buffer a real Dolt registration,
sets `dorg-sql--cli-exec' as a failing sentinel, and asserts the observed
mysql port for both the selector query and result property load."
  (let* ((temp (file-name-as-directory (make-temp-file "dorg-cli-query-" t)))
         (org-file (expand-file-name "doc.org" temp))
         (dolt-dir (expand-file-name "dolt/" temp))
         (dorg--registry nil)
         (dorg-sql-transport 'server)
         (mysql-ports nil)
         (sql-queries nil)
         (cli-called nil)
         buf)
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (with-temp-file org-file
            (insert "* Placeholder\n"))
          (setq buf (find-file-noselect org-file))
          (push (cons (file-truename org-file)
                      (make-dorg-registration
                       :backend (dorg-backend-dolt dolt-dir 1000)
                       :file (file-truename org-file)))
                dorg--registry)
          (cl-letf (((symbol-function 'clime-out-format) (lambda () 'json))
                    ((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                    ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 64021))
                    ;; Endpoint ownership (tracked-case): callers pass no port; the
                    ;; warm connection resolves strict-port lazily on a cache miss.
                    ((symbol-function 'dorg-sql--get-connection)
                     (lambda (dir &optional port)
                       (let ((p (or port (dorg-dolt-server--strict-port dir))))
                         (push p mysql-ports)
                         (list :port p))))
                    ((symbol-function 'dorg-sql--mysql-exec)
                     (lambda (query _conn)
                       (push query sql-queries)
                       (cond
                        ((string-match-p "FROM heading_tags" query)
                         '(((id . 101) (title . "Server-aware"))))
                        ((string-match-p "FROM heading_properties" query)
                         '(((heading_id . 101)
                            (prop_key . "CATEGORY")
                            (prop_value . "ops"))))
                        (t nil))))
                    ((symbol-function 'dorg-sql--cli-exec)
                     (lambda (&rest _)
                       (setq cli-called t)
                       (error "raw SQL CLI reached from public query path"))))
            (let ((data (dorg-app--emit-query '(tag "ops") buf)))
              (should-not cli-called)
              (should (equal mysql-ports '(64021 64021)))
              (should (= (length sql-queries) 2))
              (should (eq (plist-get data :command) 'query))
              (should (= (plist-get data :count) 1))
              (let ((hit (aref (plist-get data :results) 0)))
                (should (= (plist-get hit :heading-id) 101))
                (should (equal (plist-get hit :title) "Server-aware"))
                (should (equal (plist-get hit :properties)
                               '(("CATEGORY" . "ops"))))))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory temp t))))

;;; ─── Server helpers (pure) ────────────────────────────────────────────

(ert-deftest dorg-test-cli-dolt-json-shape ()
  "`dorg-app--dolt-json' projects the serializable inventory subset."
  (let ((obj (dorg-app--dolt-json
              (list :dolt-dir "/repo/dolt" :port 3306 :role 'owned
                    :status 'live :policy 'strict :protocol 'mysql
                    :owner-pid 42 :server-pid 99 :host "h"
                    :created-at 1.0 :last-used 2.0
                    :proc 'NOT-SERIALIZABLE))))
    (should (equal "/repo/dolt" (plist-get obj :dolt-dir)))
    (should (equal 3306 (plist-get obj :port)))
    (should (eq 'owned (plist-get obj :role)))
    (should (eq 'live (plist-get obj :status)))
    ;; Non-serializable process handle is not carried.
    (should-not (plist-member obj :proc))))

(ert-deftest dorg-test-cli-mutate-json-projects-canonical-envelope ()
  "`dorg-app--emit-mutate' projects all documented envelope fields.
Non-vacuous: affected/unsupported are non-empty in the fake API response,
so a missing projection fails even though ordinary dry-runs may have empty
lists.  The retired lowered `:ops' echo is not serialized."
  (let* ((obj (let ((clime-out--active-format
                     (cl-find-if (lambda (f)
                                   (eq (clime-output-format-name f) 'json))
                                 (clime-app-output-formats dorg-cli))))
                (dorg-app--emit-mutate
                 '(:ok t :command mutate :mode dry-run
                   :intent ((on (id 100)
                                (replace (keyword "STATUS" (occurrence 1)) "DONE")))
                   :patches ((:op replace
                              :target (:keyword
                                       :host (:heading :id 100)
                                       :name "STATUS" :occurrence 1)
                              :value "DONE" :missing :error))
                   :targets (100)
                   ;; A subtree move can now report its moved root and every
                   ;; descendant whose persisted level changed.
                   :affected (:created (10) :changed (20 21 22) :deleted (30))
                   :supported nil
                   :unsupported ((:op move-subtree :supported nil
                                  :reason unsupported))
                   :base-commit "c0" :base-revision "r0" :new-token "r1"))))
         affected)
    (should (eq t (plist-get obj :ok)))
    (should (eq 'mutate (plist-get obj :command)))
    (should (eq 'dry-run (plist-get obj :mode)))
    (should (= 1 (length (plist-get obj :patches))))
    (should (string-match-p ":occurrence 1"
                            (aref (plist-get obj :patches) 0)))
    (should-not (plist-member obj :ops))
    (setq affected (plist-get obj :affected))
    (should (equal '(10) (append (plist-get affected :created) nil)))
    (should (equal '(20 21 22) (append (plist-get affected :changed) nil)))
    (should (equal '(30) (append (plist-get affected :deleted) nil)))
    (should (eq :json-false (plist-get obj :supported)))
    (should (= 1 (length (plist-get obj :unsupported))))
    (should (equal "c0" (plist-get obj :base-commit)))
    (should (equal "r0" (plist-get obj :base-revision)))
    (should (equal "r1" (plist-get obj :new-token)))))

(ert-deftest dorg-test-cli-mutate-json-preserves-subtree-move-descendant-receipt ()
  "CLI JSON retains every changed id from a subtree move receipt.
The public API reports the moved root plus descendants whose stored levels
changed; this projection must not collapse that receipt to the root id."
  (let* ((obj (let ((clime-out--active-format
                     (cl-find-if (lambda (f)
                                   (eq (clime-output-format-name f) 'json))
                                 (clime-app-output-formats dorg-cli))))
                (dorg-app--emit-mutate
                 '(:ok t :command mutate :mode apply
                   :intent ((on (id 102) (move (subtree) (under (id 106)))))
                   :patches ((:op move :target (:subtree :id 102)
                              :to (:under (:heading :id 106))))
                   :targets (102)
                   :affected (:created nil :changed (102 103 104) :deleted nil)
                   :supported t :unsupported nil))))
         (affected (plist-get obj :affected)))
    (should (equal '(102 103 104)
                   (append (plist-get affected :changed) nil)))))

(ert-deftest dorg-test-cli-dolt-target-resolution ()
  "`dorg-app--dolt-target' prefers DIR, then --dolt-dir, then errors."
  ;; positional DIR wins and is expanded
  (should (equal (expand-file-name "/a/b")
                 (dorg-app--dolt-target "/a/b" nil "/c/d")))
  ;; --dolt-dir used when no positional DIR
  (should (equal "/c/d" (dorg-app--dolt-target nil nil "/c/d")))
  ;; nothing resolvable -> clean usage error (never a prompt)
  (should-error (dorg-app--dolt-target nil nil nil) :type 'user-error))

(ert-deftest dorg-test-cli-dolt-target-rejects-misbound-root ()
  "A positional project-root DIR bound to a nested store is rejected.
Builds a temp project root holding an incidental root `.dolt' and a
tracker org nested one level down (`tracker/beads.org') whose
`#+DORG_DOLT_DIR: dolt' binds the document to the nested `tracker/dolt'
store.  `dorg dolt start <root>' must fail with a `user-error' naming
the bound store and suggesting `--file', while the explicit raw paths
stay usable: pointing DIR directly at the bound store is accepted, and
`--dolt-dir' bypasses the guard entirely.  Non-vacuous: if
`dorg-app--dolt-target' stopped routing positional DIR through
`dorg-app--reject-misbound-dir', the root-rejection assertion would no
longer signal."
  (let* ((root (file-name-as-directory (make-temp-file "dorg-misbound-" t)))
         (sub (expand-file-name "tracker/" root))
         (beads (expand-file-name "beads.org" sub))
         (bound-store (expand-file-name "dolt/" sub)))
    (unwind-protect
        (progn
          ;; Incidental root Dolt repo + the document's real nested store.
          (make-directory (expand-file-name ".dolt" root) t)
          (make-directory bound-store t)
          (make-directory sub t)
          (with-temp-file beads
            (insert "#+TITLE: Tracker\n#+DORG_DOLT_DIR: dolt\n\n* A heading\n"))
          ;; Positional project root: rejected, pointing at the bound store.
          (let ((err (should-error (dorg-app--dolt-target root nil nil)
                                   :type 'user-error)))
            (should (string-match-p (regexp-quote "--file") (cadr err)))
            (should (string-match-p "tracker/dolt" (cadr err)))
            (should (string-match-p "tracker/beads\\.org" (cadr err)))
            ;; DIR holds a real `.dolt', so the accidental-repo clause appears.
            (should (string-match-p "accidental Dolt repo" (cadr err))))
          ;; Pointing DIR directly at the bound store is accepted.
          (should (equal (expand-file-name bound-store)
                         (dorg-app--dolt-target bound-store nil nil)))
          ;; `--dolt-dir' is the raw-Dolt escape hatch: guard never runs, so
          ;; even the misbound root passes through unchanged.
          (should (equal root (dorg-app--dolt-target nil nil root))))
      (delete-directory root t))))

(ert-deftest dorg-test-cli-dolt-misbound-probe-is-convention-neutral ()
  "The misbound guard detects a nested tracker under ANY subdir name.
Proves the depth-1 probe is convention-neutral — not tied to a specific
tooling directory name: a bound tracker nested under a differently-named
dot-directory (`.toolX/beads.org') is rejected just like one under a
plainly-named subdir, because `dorg-dolt-server-bound-org-globs' matches any
one-level-nested `*.org' rather than a hardcoded path."
  (let* ((root (file-name-as-directory (make-temp-file "dorg-neutral-" t)))
         (sub (expand-file-name ".toolX/" root))
         (beads (expand-file-name "beads.org" sub)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "dolt/" sub) t)
          (with-temp-file beads
            (insert "#+TITLE: Tracker\n#+DORG_DOLT_DIR: dolt\n\n* A heading\n"))
          (let ((err (should-error (dorg-app--dolt-target root nil nil)
                                   :type 'user-error)))
            (should (string-match-p "\\.toolX/dolt" (cadr err)))
            (should (string-match-p "\\.toolX/beads\\.org" (cadr err)))))
      (delete-directory root t))))

(ert-deftest dorg-test-cli-dolt-misbound-probe-globs-are-load-bearing ()
  "Disabling the nested glob makes the SAME misbound root pass (before != after).
With `dorg-dolt-server-bound-org-globs' restricted to top-level `*.org', the
one-level-nested tracker is no longer probed, so `dorg-app--dolt-target'
accepts the project root unchanged — proving the `*/*.org' default is
exactly what powers nested-tracker detection (non-vacuity for the
convention-neutral probe)."
  (let* ((root (file-name-as-directory (make-temp-file "dorg-globs-" t)))
         (sub (expand-file-name "tracker/" root))
         (beads (expand-file-name "beads.org" sub)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "dolt/" sub) t)
          (with-temp-file beads
            (insert "#+TITLE: Tracker\n#+DORG_DOLT_DIR: dolt\n\n* A heading\n"))
          ;; Default globs include `*/*.org' -> the nested tracker IS detected.
          (should-error (dorg-app--dolt-target root nil nil) :type 'user-error)
          ;; Restrict to top-level only -> nested tracker invisible -> accepted.
          (let ((dorg-dolt-server-bound-org-globs '("*.org")))
            (should (equal (expand-file-name root)
                           (dorg-app--dolt-target root nil nil)))))
      (delete-directory root t))))

;;; ─── E2E reads ────────────────────────────────────────────────────────

(ert-deftest dorg-test-cli-batch-large-file-prompt-is-never-consulted ()
  "A fresh CLI child cannot enter Emacs's unanswerable large-file prompt.
The threshold is lowered below the tiny fixture size and the prompt function
is replaced before `dorg-app.el' loads.  A raw `find-file-noselect' therefore
fails immediately instead of hanging; the shared batch-open policy must make
the real title query complete with its expected result."
  (dorg-test-cli--with-fixture file
    (let ((res
           (dorg-test-cli--invoke
            (list "title" "First task" "-f" file)
            nil
            '(progn
               (setq large-file-warning-threshold 1)
               (fset 'files--ask-user-about-large-file
                     (lambda (&rest _)
                       (error "large-file prompt was consulted")))))))
      (should (= 0 (car res)))
      (should (equal "100\tFirst task\n" (cdr res))))))

(ert-deftest dorg-test-cli-production-opens-share-one-batch-policy ()
  "All five production file opens route through one registry-owned policy.
The raw Emacs primitive must occur exactly once, inside that shared helper;
the five audited callers account for five shared call forms.  This source
census makes restoring any one raw caller fail the test."
  (let ((raw-count 0)
        (shared-count 0))
    (dolist (file (directory-files dorg-test-cli--root t
                                   "\\`dorg-.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(find-file-noselect\\_>" nil t)
          (setq raw-count (1+ raw-count)))
        (goto-char (point-min))
        (while (re-search-forward "(dorg--find-file-noselect\\_>" nil t)
          (setq shared-count (1+ shared-count)))))
    (should (= 1 raw-count))
    (should (= 5 shared-count))))

(ert-deftest dorg-test-cli-query-text ()
  "A tag query prints one \"<id>\\t<title>\" line per match, exit 0."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke (list "query" "(tag \"work\")" "-f" file))))
      (should (= 0 (car res)))
      (should (equal "101\tSecond task\n102\tThird task\n" (cdr res))))))

(ert-deftest dorg-test-cli-query-empty-text-blank ()
  "An empty text query prints nothing on stdout and exits 0."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke (list "tag" "nonesuch" "-f" file))))
      (should (= 0 (car res)))
      (should (equal "" (cdr res))))))

(ert-deftest dorg-test-cli-convenience-parity ()
  "Convenience subcommands match the equivalent `query' atomic form."
  (dorg-test-cli--with-fixture file
    (dolist (pair '((("property" "CATEGORY" "red") . "(property \"CATEGORY\" \"red\")")
                    (("tag" "work")                . "(tag \"work\")")
                    (("title" "First task")        . "(title \"First task\")")
                    (("id" "100")                  . "(id 100)")))
      (let ((conv (dorg-test-cli--invoke (append (car pair) (list "-f" file))))
            (qry  (dorg-test-cli--invoke (list "query" (cdr pair) "-f" file))))
        (should (= 0 (car conv)))
        (should (= 0 (car qry)))
        (should (equal (cdr qry) (cdr conv)))))))

(ert-deftest dorg-test-cli-json-array-contract-e2e ()
  "End-to-end `--json' returns query hits under the shared envelope."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke (list "tag" "nonesuch" "-f" file "--json")))
           (obj (dorg-test-cli--json (cdr res)))
           (data (alist-get 'data obj)))
      (should (= 0 (car res)))
      (should (eq t (alist-get 'ok obj)))
      (should (= 0 (alist-get 'count data)))
      (should (null (alist-get 'results data)))
      (should (string-prefix-p "{" (string-trim (cdr res)))))
    (let* ((res (dorg-test-cli--invoke (list "id" "100" "-f" file "--json")))
           (data (dorg-test-cli--json-data (cdr res)))
           (results (alist-get 'results data)))
      (should (= 0 (car res)))
      (should (= 1 (length results)))
      (should (equal 100 (alist-get 'heading-id (car results))))
      (should (equal "First task" (alist-get 'title (car results)))))
    (let* ((res (dorg-test-cli--invoke (list "tag" "work" "-f" file "--json")))
           (data (dorg-test-cli--json-data (cdr res)))
           (results (alist-get 'results data)))
      (should (= 0 (car res)))
      (should (= 2 (length results)))
      (should (equal '(101 102)
                     (mapcar (lambda (h) (alist-get 'heading-id h)) results))))))

(ert-deftest dorg-test-cli-json-hit-has-no-marker ()
  "Each `--json' hit object is {heading-id,title,properties}, no marker."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke (list "id" "100" "-f" file "--json")))
           (obj (car (alist-get 'results
                                (dorg-test-cli--json-data (cdr res))))))
      (should (equal 100 (alist-get 'heading-id obj)))
      (should (alist-get 'properties obj))
      (should-not (assq 'marker obj)))))

;;; ─── E2E source selection / env var ───────────────────────────────────

(ert-deftest dorg-test-cli-env-var-selects-file ()
  "DORG_FILE selects the source file when no --file is given."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke (list "tag" "work")
                                      (list (concat "DORG_FILE=" file)))))
      (should (= 0 (car res)))
      (should (equal "101\tSecond task\n102\tThird task\n" (cdr res))))))

(ert-deftest dorg-test-cli-explicit-file-overrides-env ()
  "An explicit --file overrides DORG_FILE."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke
                (list "tag" "work" "-f" file)
                (list "DORG_FILE=/does/not/exist.org"))))
      (should (= 0 (car res)))
      (should (equal "101\tSecond task\n102\tThird task\n" (cdr res))))))

;;; ─── E2E negative cases ───────────────────────────────────────────────

(ert-deftest dorg-test-cli-missing-file-usage-error ()
  "Missing --file (and no env) produces a nonzero usage error."
  (let ((res (dorg-test-cli--invoke (list "tag" "work"))))
    (should-not (= 0 (car res)))))

(ert-deftest dorg-test-cli-malformed-sexp-nonzero ()
  "A malformed query exits nonzero with one typed JSON error envelope."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke
                 (list "--json" "query" "(tag \"x\"" "-f" file)))
           (err (dorg-test-cli--single-json-error
                 res 'invalid-form 'malformed-syntax)))
      (should-not (string-match-p
                   "Debugger\\|backtrace\\|End of file during parsing"
                   (alist-get 'message err))))))

(ert-deftest dorg-test-cli-query-and-mutate-reader-refusals-match ()
  "Query and mutate both reject trailing input through one envelope contract."
  (dorg-test-cli--with-fixture file
    (dolist (args
             (list (list "--json" "query" "(tag \"work\") (id 100)"
                         "-f" file)
                   (list "--json" "mutate" "(batch) (bogus)"
                         "--dry-run" "-f" file)))
      (dorg-test-cli--single-json-error
       (dorg-test-cli--invoke args) 'invalid-form 'trailing-input))))

(ert-deftest dorg-test-cli-reader-valid-atoms-reach-domain-validation ()
  "Reader-valid atoms and nil are rejected by query/mutate domain validators."
  (dorg-test-cli--with-fixture file
    (dorg-test-cli--single-json-error
     (dorg-test-cli--invoke (list "--json" "query" "nil" "-f" file))
     'invalid-query)
    (dorg-test-cli--single-json-error
     (dorg-test-cli--invoke
      (list "--json" "mutate" "nil" "--dry-run" "-f" file))
     'invalid-op)))

(ert-deftest dorg-test-cli-trailing-mutation-never-writes ()
  "A valid apply followed by trailing input fails before any file write."
  (dorg-test-cli--with-fixture file
    (let* ((form "(on (id 100) (set (property \"STATUS\") \"DONE\"))")
           (before (dorg-test-cli--file-bytes file))
           (bad (dorg-test-cli--invoke
                 (list "--json" "mutate" (concat form " (bogus)")
                       "--apply" "-f" file))))
      (dorg-test-cli--single-json-error bad 'invalid-form 'trailing-input)
      (should (equal before (dorg-test-cli--file-bytes file)))
      ;; Positive control: the same first form alone is a real transformation.
      (should (= 0 (car (dorg-test-cli--invoke
                         (list "mutate" form "--apply" "-f" file)))))
      (should-not (equal before (dorg-test-cli--file-bytes file)))
      (should (string-match-p "^:STATUS:[ \t]+DONE"
                              (dorg-test-cli--file-bytes file))))))

(ert-deftest dorg-test-cli-invalid-predicate-is-invalid-query ()
  "An unknown query predicate is typed as invalid-query, not unsupported.
Every timestamp predicate now resolves on both buffer backends, so the
unknown predicate is a grammar failure, not a source capability refusal."
  (dorg-test-cli--with-fixture file
    (dorg-test-cli--single-json-error
     (dorg-test-cli--invoke
      (list "--json" "query" "(bogus \"2026-01-01\" nil)" "-f" file))
     'invalid-query)))

(ert-deftest dorg-test-cli-known-query-refusal-is-unsupported ()
  "A known identity capability refusal preserves unsupported and its detail."
  (dorg-test-cli--with-fixture file
    (let* ((abs (file-truename file))
           (preload
            `(progn
               (require 'dorg)
               (setq dorg--registry
                     (list
                      (cons ,abs
                            (make-dorg-registration
                             :backend (dorg-backend-plain-org)
                             :file ,abs :identity 'none))))))
           (res (dorg-test-cli--invoke
                 (list "--json" "query" "(key \"100\")" "-f" file)
                 nil preload))
           (err (dorg-test-cli--single-json-error res 'unsupported)))
      (should (equal "identity-none"
                     (format "%s"
                             (alist-get 'detail
                                        (alist-get 'details err))))))))

;;; ─── E2E write (transformation) ───────────────────────────────────────

;; NOTE (tracked-case): the former `dorg set' CLI success tests
;; (set-transforms-file / set-json-is-object / set-rejects-dorg-id) are
;; REMOVED — `dorg set' is deleted in v0.1 (docs/api.org "Migration from set").
;; Their coverage moves to the `mutate' surface: property write is
;; `dorg-test-cli-mutate-apply-writes', JSON shape is
;; `dorg-test-cli-mutate-json-is-canonical-object', DORG_ID rejection is
;; `dorg-test-cli-mutate-rejects-dorg-id' (below) plus the typed-IR-layer
;; `dorg-test-api-reject-dorg-id-property'.  Removal itself is
;; `dorg-test-cli-set-command-removed'.

;;; ─── E2E server commands (dolt-free plumbing) ─────────────────────────

(ert-deftest dorg-test-cli-dolt-list-empty-array ()
  "`server list --json' returns an empty server list under the envelope.
Runs with isolated server state so no foreign claims leak into the test.
Listing is read-only and never starts a server."
  (let ((rt (make-temp-file "dorg-cli-rt-" t)))
    (unwind-protect
        (let ((res (dorg-test-cli--invoke
                    (list "dolt" "list" "--json")
                    (list (concat "TMPDIR=" (file-name-as-directory rt))))))
          (should (= 0 (car res)))
          (let ((data (dorg-test-cli--json-data (cdr res))))
            (should (equal "dolt-list" (format "%s" (alist-get 'command data))))
            (should (= 0 (alist-get 'count data)))
            (should (null (alist-get 'servers data)))))
      (delete-directory rt t))))

(ert-deftest dorg-test-cli-dolt-stop-all-object ()
  "`server stop-all --json' is a clean object reporting 0 owned servers."
  (let ((rt (make-temp-file "dorg-cli-rt-" t)))
    (unwind-protect
        (let* ((res (dorg-test-cli--invoke
                     (list "dolt" "stop-all" "--json")
                     (list (concat "TMPDIR=" (file-name-as-directory rt)))))
               (data (dorg-test-cli--json-data (cdr res))))
          (should (= 0 (car res)))
          (should (equal 0 (alist-get 'stopped data))))
      (delete-directory rt t))))

(ert-deftest dorg-test-cli-dolt-start-no-target-usage-error ()
  "A target-less `dolt start' exits nonzero without prompting."
  (let ((res (dorg-test-cli--invoke (list "dolt" "start"))))
    (should-not (= 0 (car res)))
    ;; no minibuffer prompt text leaked, no backtrace
    (should-not (string-match-p "Debugger\\|backtrace" (cdr res)))))

;;; ─── Build / vendoring contract ───────────────────────────────────────

(ert-deftest dorg-test-cli-vendored-clime-subset ()
  "`lib/clime' holds the CLI subset + serve module + LICENSE + VERSION.
clime-serve.el and a plain `lib/web-server' source snapshot are present
(loaded lazily, see `dorg-test-cli-serve-stack-not-loaded-by-base-app');
clime-invoke / clime-make / clime-make-main remain excluded.  The
web-server snapshot is a real directory — not a gitlink/submodule — and
carries no nested `.git' and no compiled `.elc'."
  (let ((dir (dorg-test-cli--path "lib/clime")))
    (dolist (m '("clime.el" "clime-settings.el" "clime-core.el"
                 "clime-param-type.el" "clime-parse.el" "clime-config.el"
                 "clime-dotenv.el" "clime-dsl.el" "clime-help.el"
                 "clime-output.el" "clime-run.el" "clime-serve.el"
                 "clime-telemetry.el"
                 "LICENSE" "VERSION"))
      (should (file-exists-p (expand-file-name m dir))))
    (dolist (x '("clime-invoke.el" "clime-make.el" "clime-make-main.el"))
      (should-not (file-exists-p (expand-file-name x dir))))
    ;; Plain `lib/web-server' source snapshot (nav blocker 3): a real
    ;; directory with the runtime source + license, NOT a gitlink/nested
    ;; repo, and no compiled artifacts.
    (let ((ws (expand-file-name "lib/web-server" dir)))
      (should (file-directory-p ws))
      (dolist (f '("web-server.el" "web-server-status-codes.el" "COPYING"))
        (should (file-exists-p (expand-file-name f ws))))
      ;; Not a submodule gitlink and no nested repo.
      (should-not (file-exists-p (expand-file-name ".git" ws)))
      ;; No byte-compiled files committed in the snapshot.
      (should-not (directory-files ws nil "\\.elc\\'")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "VERSION" dir))
      (should (equal "0.8.2" (string-trim (buffer-string)))))))

(ert-deftest dorg-test-cli-bin-wrapper-relative-only ()
  "The committed `bin/dorg' is executable, relative-`-L' only, no abspaths."
  (let ((bin (dorg-test-cli--path "bin/dorg")))
    (should (file-exists-p bin))
    (should (file-executable-p bin))
    (with-temp-buffer
      (insert-file-contents bin)
      (let ((shebang (progn (goto-char (point-min))
                            (forward-line 1)
                            (buffer-substring (line-beginning-position)
                                              (line-end-position)))))
        (should (string-match-p "CLIME_MAIN_APP=dorg-cli" shebang))
        (should (string-match-p "-L \"\\$D/\\.\\.\"" shebang))
        (should (string-match-p "-L \"\\$D/\\.\\./lib/clime\"" shebang))
        ;; No absolute/machine-specific -L path in the shebang line.
        (should-not (string-match-p "-L \"?/" shebang))))))

(ert-deftest dorg-test-cli-make-target-regenerates-wrapper ()
  "`make bin/dorg' regenerates the wrapper with auto-detected clime-make.
Skips when GNU make or a local clime-make is unavailable."
  (skip-unless (executable-find "make"))
  (skip-unless (dorg-test-cli--find-clime-make))
  (let* ((default-directory dorg-test-cli--root)
         (bin (dorg-test-cli--path "bin/dorg"))
         (old-text (when (file-exists-p bin)
                     (with-temp-buffer
                       (insert-file-contents-literally bin)
                       (buffer-string))))
         (old-modes (when (file-exists-p bin) (file-modes bin)))
         (process-environment (cons "CLIME_MAKE=" process-environment)))
    (unwind-protect
        (progn
          (when (file-exists-p bin)
            (delete-file bin))
          (with-temp-buffer
            (let ((code (call-process "make" nil (list (current-buffer) nil)
                                      nil "bin/dorg")))
              (should (= 0 code))
              (should (string-match-p "Regenerating bin/dorg"
                                      (buffer-string)))))
          (should (file-executable-p bin))
          (should-not (file-exists-p (dorg-test-cli--path "bin/dorg-ql"))))
      (when old-text
        (let ((coding-system-for-write 'no-conversion))
          (write-region old-text nil bin nil 'silent))
        (when old-modes
          (set-file-modes bin old-modes))))))

(ert-deftest dorg-test-cli-bin-wrapper-mirrors-app ()
  "The committed `bin/dorg' body (after its two-line polyglot header) is
byte-identical to `dorg-app.el'.  This is the clime-make-INDEPENDENT drift gate:
any edit to `dorg-app.el' that is not regenerated into `bin/dorg' fails here, so
the CLI can never ship stale embedded behaviour (e.g. an old `--expect-targets'
parser that predates extern identity tokens)."
  (let* ((bin (dorg-test-cli--path "bin/dorg"))
         (app (dorg-test-cli--path "dorg-app.el"))
         (bin-body (with-temp-buffer
                     (insert-file-contents-literally bin)
                     (goto-char (point-min))
                     (forward-line 2)   ; skip #!/bin/sh + the polyglot exec line
                     (buffer-substring-no-properties (point) (point-max))))
         (app-text (with-temp-buffer
                     (insert-file-contents-literally app)
                     (buffer-string))))
    (should (equal app-text bin-body))))

(ert-deftest dorg-test-h2t9n-make-mysql-dir-override-reaches-tests ()
  "The standalone client Make override controls the batch test load path."
  (skip-unless (executable-find "make"))
  (let* ((root dorg-test-cli--root)
         (mysql-dir (make-temp-file "dorg-h2t9n-mysql-" t))
         (fragment (make-temp-file "dorg-h2t9n-make-" nil ".mk"))
         (output (generate-new-buffer " *dorg-h2t9n-make*")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "mysql.el" mysql-dir))
          (with-temp-file fragment
            (insert "include " (expand-file-name "Makefile" root) "\n"
                    ".PHONY: h2t9n-print\n"
                    "h2t9n-print:\n"
                    "\t@printf '%s\\n' '$(BATCH_TEST)'\n"))
          (let ((default-directory root))
            (should (= 0 (call-process
                          "make" nil output nil "-s" "-f" fragment
                          (concat "MYSQL_DIR=" mysql-dir) "h2t9n-print"))))
          (with-current-buffer output
            (should (string-match-p
                     (regexp-quote (concat "-L " mysql-dir))
                     (buffer-string)))))
      (kill-buffer output)
      (delete-file fragment)
      (delete-directory mysql-dir t))))

(ert-deftest dorg-test-cli-bin-parses-extern-expect-targets ()
  "Loading the COMMITTED `bin/dorg' exposes the current extern-aware
`--expect-targets' parser (regression for the stale-wrapper reject at 1ec69ac):
a mixed integer + `key:K=V' list parses to the id plus the extern identity, not
the old `(101 0)' from `string-to-number'."
  (let* ((bin (dorg-test-cli--path "bin/dorg"))
         (out (with-temp-buffer
                (call-process
                 (expand-file-name invocation-name invocation-directory)
                 nil (current-buffer) nil
                 "-Q" "--batch"
                 "-L" dorg-test-cli--root
                 "-L" (dorg-test-cli--path "lib/clime")
                 "-l" bin
                 "--eval"
                 "(prin1 (dorg-app--parse-expect-targets \"101,key:BEAD_ID=bead-002\"))")
                (buffer-string))))
    (should (string-match-p "101" out))
    (should (string-match-p ":extern" out))
    (should (string-match-p "bead-002" out))
    (should-not (string-match-p "101 0)" out))))

;;; ─── Core honesty: no accidental Clime dependency ─────────────────────

(ert-deftest dorg-test-cli-core-loads-without-clime ()
  "`(require \\='dorg)' loads with NO clime on the load path.
The CLI app `dorg-app.el' requires clime, but it must stay out of
`dorg.el' (and `dorg-reload' discovery), so core DOrg gains no Clime
dependency.
Runs a fresh `emacs -Q' with only the repo root on the load path (clime's
`lib/clime' deliberately omitted) and asserts a clean load."
  (let* ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((code (call-process
                   emacs nil (list (current-buffer) nil) nil
                   "--batch" "-Q"
                   "-L" dorg-test-cli--root
                   "--eval"
                   (prin1-to-string
                    '(progn
                       (require 'dorg)
                       (when (featurep 'clime)
                         (error "clime leaked into core dorg load"))
                       (princ "ok"))))))
        (should (= 0 code))
        (should (string-match-p "ok" (buffer-string)))))))

(ert-deftest dorg-test-cli-serve-stack-not-loaded-by-base-app ()
  "Loading the base CLI app does NOT pull the HTTP serve stack.
After vendoring `clime-serve.el' + `lib/web-server' into `lib/clime',
neither `clime-serve' nor `web-server' may be provided merely by loading
`clime' and the base `dorg-app' — they stay lazy, loaded only by an
explicit serve entry point.  Runs a fresh `emacs -Q' with the repo root
and `lib/clime' on the load path and asserts the features are absent."
  (let* ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((code (call-process
                   emacs nil (list (current-buffer) nil) nil
                   "--batch" "-Q"
                   "-L" dorg-test-cli--root
                   "-L" (dorg-test-cli--path "lib/clime")
                   "--eval"
                   (prin1-to-string
                    '(progn
                       (require 'clime)
                       (require 'dorg-app)
                       (when (featurep 'clime-serve)
                         (error "clime-serve loaded by the base CLI app"))
                       (when (featurep 'web-server)
                         (error "web-server loaded by the base CLI app"))
                       (princ "ok"))))))
        (should (= 0 code))
        (should (string-match-p "ok" (buffer-string)))))))

;;; ─── Serve (HTTP) lifecycle + auth ───────────────────────────────────

(ert-deftest dorg-test-cli-serve-token-equal ()
  "`dorg-app--serve-token-equal' is correct and length-checked."
  (should (dorg-app--serve-token-equal "abc123" "abc123"))
  (should-not (dorg-app--serve-token-equal "abc123" "abc124"))
  (should-not (dorg-app--serve-token-equal "abc" "abcd"))
  (should-not (dorg-app--serve-token-equal "abc" nil)))

(ert-deftest dorg-test-cli-serve-token-precedence ()
  "Token resolves --token > DORG_SERVE_TOKEN > generated."
  (let ((process-environment (cons "DORG_SERVE_TOKEN=envtok" process-environment)))
    (should (equal "flagtok" (dorg-app--serve-resolve-token "flagtok")))
    (should (equal "envtok" (dorg-app--serve-resolve-token nil))))
  (let ((process-environment (cons "DORG_SERVE_TOKEN=" process-environment)))
    ;; empty env -> generated (64-char sha256 hex)
    (should (= 64 (length (dorg-app--serve-resolve-token nil))))))

(ert-deftest dorg-test-cli-serve-claim-roundtrip-and-modes ()
  "Claim files are 0600 under a 0700 dir; state omits the token; read roundtrips."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir))
    (unwind-protect
        (progn
          (dorg-app--serve-write-claim 8090 "127.0.0.1" "thetoken" "owner-A")
          (should (= #o700 (file-modes dir)))
          (should (= #o600 (file-modes (dorg-app--serve-state-file 8090))))
          (should (= #o600 (file-modes (dorg-app--serve-token-file 8090))))
          (let ((st (dorg-app--serve-read-claim 8090)))
            (should (equal "127.0.0.1" (alist-get 'host st)))
            (should (equal "owner-A" (alist-get 'owner-token st)))
            (should (integerp (alist-get 'pid st)))
            ;; the auth token NEVER lives in the state file
            (should-not (assq 'token st))
            (should-not (assq 'auth-token st)))
          (with-temp-buffer
            (insert-file-contents (dorg-app--serve-token-file 8090))
            (should (equal "thetoken" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-remove-claim-owner-guard ()
  "`dorg-app--serve-remove-claim' deletes only when the owner-token matches."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir))
    (unwind-protect
        (progn
          (dorg-app--serve-write-claim 8091 "127.0.0.1" "t" "owner-A")
          (dorg-app--serve-remove-claim 8091 "owner-B") ; wrong owner -> kept
          (should (file-exists-p (dorg-app--serve-state-file 8091)))
          (dorg-app--serve-remove-claim 8091 "owner-A") ; right owner -> gone
          (should-not (file-exists-p (dorg-app--serve-state-file 8091)))
          (should-not (file-exists-p (dorg-app--serve-token-file 8091))))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-claim-live-p ()
  "Liveness keys on a live, this-protocol pid."
  (should (dorg-app--serve-claim-live-p
           `((pid . ,(emacs-pid)) (protocol . ,dorg-serve-claim-protocol))))
  (should-not (dorg-app--serve-claim-live-p
               `((pid . 2147480000) (protocol . ,dorg-serve-claim-protocol))))
  (should-not (dorg-app--serve-claim-live-p
               `((pid . ,(emacs-pid)) (protocol . 999)))))

(ert-deftest dorg-test-cli-serve-auth-predicate ()
  "Auth predicate accepts the token via ?token= or X-Dorg-Token; rejects else.
Standard Authorization: Bearer is deliberately NOT honored (web-server
cannot parse it)."
  (let ((pred (dorg-app--serve-auth-predicate "sekret")))
    (should (funcall pred '(:headers (("token" . "sekret")))))
    (should (funcall pred '(:headers ((:X-DORG-TOKEN . "sekret")))))
    (should-not (funcall pred '(:headers (("token" . "nope")))))
    (should-not (funcall pred '(:headers nil)))
    (should-not (funcall pred '(:headers ((:AUTHORIZATION . (:BEARER . "sekret"))))))))

(ert-deftest dorg-test-cli-serve-status-not-running ()
  "`serve status --json' on an empty runtime dir reports not-running, exit 0."
  (let ((dir (make-temp-file "dorg-serve-test-" t)))
    (unwind-protect
        (let* ((res (dorg-test-cli--invoke
                     '("serve" "status" "--port" "8092" "--json")
                     (list (concat "DORG_SERVE_RUNTIME_DIR=" dir))))
               (data (dorg-test-cli--json-data (cdr res))))
          (should (= 0 (car res)))
          (should (eq (alist-get 'running data) :json-false))
          (should (= 8092 (alist-get 'port data))))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-stop-noop ()
  "`serve stop --json' with nothing running is a clean no-op, exit 0."
  (let ((dir (make-temp-file "dorg-serve-test-" t)))
    (unwind-protect
        (let* ((res (dorg-test-cli--invoke
                     '("serve" "stop" "--port" "8093" "--json")
                     (list (concat "DORG_SERVE_RUNTIME_DIR=" dir))))
               (data (dorg-test-cli--json-data (cdr res))))
          (should (= 0 (car res)))
          (should (eq (alist-get 'stopped data) :json-false)))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-stop-live-claim-fail-closed ()
  "`serve stop' against a live claim sends NO raw-PID signal and deletes NO
claim (dorg-fatalex, AC #9).  A serve daemon`s owner token and process
object live in the daemon`s own process, so a separate CLI invocation can
neither prove local ownership nor confirm death; cross-process stop fails
closed with a clear error rather than TERMing a PID read from the claim."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir)
         (signalled nil))
    (unwind-protect
        (progn
          ;; Seed a LIVE claim (this process`s pid is alive).
          (dorg-app--serve-write-claim 8097 "127.0.0.1" "tok" "owner-A")
          (should (dorg-app--serve-claim-live-p
                   (dorg-app--serve-read-claim 8097)))
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (&rest args) (push args signalled) t)))
            ;; Fail-closed: a clear usage error, never a silent success.
            (should-error (dorg-app--serve-stop 8097) :type 'clime-usage-error))
          ;; No raw-PID signal was ever sent.
          (should (null signalled))
          ;; The foreign claim was NOT deleted.
          (should (dorg-app--serve-read-claim 8097)))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-stop-dead-claim-no-deletion ()
  "`serve stop' on a dead/stale claim reports not-running WITHOUT deleting
its files (dorg-fatalex).  Apparent death is not proof of ownership or
exit, so the fail-closed contract forbids reaping foreign claim files."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir))
    (unwind-protect
        (progn
          ;; A claim whose pid is certainly dead (never-allocated high pid).
          (dorg-app--serve-write-claim 8098 "127.0.0.1" "tok" "owner-A")
          (let ((state (dorg-app--serve-read-claim 8098)))
            (setf (alist-get 'pid state) 2147480000)
            (with-temp-file (dorg-app--serve-state-file 8098)
              (insert (json-encode state) "\n")))
          (should-not (dorg-app--serve-claim-live-p
                       (dorg-app--serve-read-claim 8098)))
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (&rest _) (error "must not signal"))))
            (dorg-app--serve-stop 8098))
          ;; Fail-closed: the dead claim file is left in place.
          (should (dorg-app--serve-read-claim 8098)))
      (delete-directory dir t))))

(ert-deftest dorg-test-cli-serve-start-idempotent-refusal ()
  "`serve start' against a live claim refuses with ONE JSON object, no bind.
Guards the ACCEPT watch-out of no double-emitted startup JSON on the
non-blocking refusal path."
  (let* ((dir (make-temp-file "dorg-serve-test-" t))
         (dorg-serve-runtime-dir dir))
    (unwind-protect
        (progn
          ;; Pre-seed a LIVE claim (this test process's pid is alive).
          (dorg-app--serve-write-claim 8094 "127.0.0.1" "tok" "owner-A")
          (let* ((res (dorg-test-cli--invoke
                       '("serve" "start" "--port" "8094" "--json")
                       (list (concat "DORG_SERVE_RUNTIME_DIR=" dir))))
                 (out (cdr res)))
            (should (= 0 (car res)))
            (let ((data (dorg-test-cli--json-data out)))
              (should (eq (alist-get 'started data) :json-false))
              (should (equal "already-running" (alist-get 'reason data))))
            ;; Exactly one JSON line — no second/array emission.
            (should-not (string-match-p "\\[\\]" out))
            (should (= 1 (length (split-string (string-trim out) "\n" t))))))
      (delete-directory dir t))))

;;; ─── Serve (HTTP) route/result round-trips ────────────────────────────

(defun dorg-test-serve--free-port ()
  "Return an OS-assigned free TCP port on 127.0.0.1."
  (let* ((proc (make-network-process :name "dorg-test-port" :server t
                                     :host "127.0.0.1" :service t
                                     :family 'ipv4))
         (port (process-contact proc :service)))
    (delete-process proc)
    port))

(defun dorg-test-serve--web-server-p ()
  "Load the vendored web-server + clime-serve; return non-nil on success.
DOrg vendors web-server, so these round-trips RUN
rather than skip; skip only if the require genuinely fails."
  (or (featurep 'web-server)
      (ignore-errors
        (add-to-list 'load-path (dorg-test-cli--path "lib/clime/lib/web-server"))
        (require 'clime-serve)
        (require 'web-server)
        (featurep 'web-server))))

(defun dorg-test-serve--raw (port method path &optional headers)
  "Send a bare HTTP/1.0 METHOD PATH to 127.0.0.1:PORT; return (CODE HEAD . BODY).
HEADERS is an alist of extra request headers.  Uses a raw socket so a
4xx/5xx is read verbatim without url.el dragging batch into interactive
auth/authinfo."
  (let ((proc (open-network-stream "dorg-test-http" nil "127.0.0.1" port))
        (out ""))
    (set-process-coding-system proc 'binary 'binary)
    (set-process-filter proc (lambda (_p s) (setq out (concat out s))))
    (process-send-string
     proc (concat (format "%s %s HTTP/1.0\r\n" method path)
                  (format "Host: 127.0.0.1:%d\r\n" port)
                  (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h)))
                             headers "")
                  "\r\n"))
    ;; Read budget per chunk.  Routes like `/dolt/start' are handled
    ;; synchronously: the handler emits NO bytes until its real
    ;; `dolt sql-server' cold start finishes, which on a loaded host can take
    ;; longer than a fixed few seconds.  Wait a server-start-sized budget
    ;; (tied to `dorg-dolt-server-startup-timeout', + margin) so a slow-but-valid
    ;; start is not misread as an empty body.  Fast routes respond and close
    ;; immediately, so the loop still ends at EOF regardless of this value.
    (while (accept-process-output proc (+ dorg-dolt-server-startup-timeout 5)))
    (ignore-errors (delete-process proc))
    (let* ((sep (string-match "\r\n\r\n" out))
           (head (if sep (substring out 0 sep) out))
           (body (if sep (substring out (+ sep 4)) ""))
           (code (when (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)" head)
                   (string-to-number (match-string 1 head)))))
      (cons code (cons head body)))))

(defmacro dorg-test-serve--with-server (port-var token file &rest body)
  "Start a real PUBLIC clime-serve for `dorg-cli' on a free PORT-VAR.
TOKEN is the auth token; FILE the org source (bound via DORG_FILE).  Binds
`dorg-app--serving' t to faithfully simulate `dorg serve start' (so
/serve/* routes refuse).  Uses the public entry, never clime-serve
internals; stops the server in `unwind-protect'."
  (declare (indent 3))
  `(progn
     (require 'clime-serve)
     (let* ((,port-var (dorg-test-serve--free-port))
            (server nil)
            (dorg-app--serving t)
            (process-environment (cons (concat "DORG_FILE=" ,file)
                                       process-environment)))
       (unwind-protect
           (progn
             (setq server
                   (clime-serve dorg-cli :port ,port-var :host "127.0.0.1"
                                :default-format 'json
                                :auth (dorg-app--serve-auth-predicate ,token)))
             ,@body)
         (when server (ignore-errors (clime-serve-stop server)))))))

(ert-deftest dorg-test-serve-raw-tolerates-slow-response ()
  "`dorg-test-serve--raw' waits a server-start-sized budget for a reply.
Regression for the order-dependent `dorg-test-serve-http-dolt-start-stop-objects'
failure: the `/dolt/start' route is handled synchronously by
clime-serve, so the handler emits NO response bytes until its real
`dolt sql-server' cold start completes.  On a loaded host that cold start
can exceed the probe's old hardcoded 5s read budget, so the probe read an
empty body and `json-read-from-string' on \"\" errored (\"empty HTTP body
where JSON was expected\").  The probe must instead wait a budget tied to
`dorg-dolt-server-startup-timeout'.

A bare HTTP server replies only after `delay' seconds, which exceeds the
old 5s budget but is well within the derived budget.  Non-vacuous: revert
`dorg-test-serve--raw' to the literal 5s read budget and this read returns
an empty body, failing the JSON parse."
  (let* ((delay 6)                          ; > old 5s budget, < derived budget
         (dorg-dolt-server-startup-timeout 60)   ; derived probe budget = 65s
         (srv (make-network-process
               :name "dorg-slow-http" :server t :host "127.0.0.1"
               :service 0 :family 'ipv4 :coding 'binary
               :filter
               (lambda (cproc _s)
                 ;; Reply after DELAY, mimicking a synchronous handler that
                 ;; blocks while a server cold-starts.  A timer keeps the
                 ;; event loop live (as `dorg-dolt-server--wait-ready' does) so
                 ;; the probe's `accept-process-output' wait is the only
                 ;; thing gating the read.
                 (run-at-time
                  delay nil
                  (lambda ()
                    (when (process-live-p cproc)
                      (process-send-string
                       cproc (concat "HTTP/1.0 200 OK\r\n"
                                     "Content-Type: application/json\r\n\r\n"
                                     "{\"ok\":true}"))
                      (ignore-errors (delete-process cproc))))))))
         (port (process-contact srv :service)))
    (unwind-protect
        (let* ((r (dorg-test-serve--raw port "GET" "/slow"))
               (obj (json-read-from-string (cddr r))))
          (should (= 200 (car r)))
          (should (eq t (cdr (assq 'ok obj)))))
      (delete-process srv))))

(ert-deftest dorg-test-serve-http-read-routes-arrays ()
  "Bare read routes return shared-envelope query result payloads."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let* ((r (dorg-test-serve--raw port "GET" "/tag?tag=work&token=tok"))
             (data (dorg-test-cli--json-data (cddr r)))
             (results (alist-get 'results data)))
        (should (= 200 (car r)))
        (should (equal '(101 102)
                       (sort (mapcar (lambda (h) (alist-get 'heading-id h))
                                     results)
                             #'<))))
      (let* ((r (dorg-test-serve--raw port "GET" "/id?id=100&token=tok"))
             (results (alist-get 'results (dorg-test-cli--json-data (cddr r)))))
        (should (= 200 (car r)))
        (should (= 1 (length results))))
      (let* ((r (dorg-test-serve--raw
                 port "GET" "/property?key=CATEGORY&value=red&token=tok"))
             (results (alist-get 'results (dorg-test-cli--json-data (cddr r)))))
        (should (= 200 (car r)))
        (should (= 2 (length results))))
      ;; /title?title=First task -> [100]
      (let* ((r (dorg-test-serve--raw
                 port "GET" "/title?title=First%20task&token=tok"))
             (results (alist-get 'results (dorg-test-cli--json-data (cddr r)))))
        (should (= 200 (car r)))
        (should (= 1 (length results)))
        (should (= 100 (alist-get 'heading-id (car results)))))
      (let* ((r (dorg-test-serve--raw
                 port "GET" "/query?sexp=%28tag%20%22work%22%29&token=tok"))
             (results (alist-get 'results (dorg-test-cli--json-data (cddr r)))))
        (should (= 200 (car r)))
        (should (= 2 (length results)))))))

(ert-deftest dorg-test-serve-http-empty-array ()
  "A no-match read route returns an empty results payload."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let* ((r (dorg-test-serve--raw port "GET" "/tag?tag=nope&token=tok"))
             (data (dorg-test-cli--json-data (cddr r))))
        (should (= 200 (car r)))
        (should (= 0 (alist-get 'count data)))
        (should (null (alist-get 'results data)))))))

(ert-deftest dorg-test-serve-http-mutate-object-and-reread ()
  "POST /mutate?mode=apply returns the canonical response OBJECT and the change
is visible on re-read (one real write + one real read).  Replaces the removed
POST /set route (docs/api.org: POST /mutate).

A11 real-transport delegation proof (reviewer B4): the in-process HTTP handler
runs the SAME mutate command as the CLI, so a call-through spy proves the real
POST goes through the shared `dorg-app--mutate-source' seam EXACTLY once, that the
built descriptor omits `:identity' (programmatic-only for BOTH transports), and
that it resolves to the derived `dorg' policy — there is NO HTTP-specific default."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let ((mutate-calls 0)
            (mutate-desc 'unset)
            (orig (symbol-function 'dorg-app--mutate-source)))
        (cl-letf (((symbol-function 'dorg-app--mutate-source)
                   (lambda (&rest args)
                     (setq mutate-calls (1+ mutate-calls))
                     (let ((rv (apply orig args)))
                       (setq mutate-desc (car rv))
                       rv))))
          (let* ((sexp (url-hexify-string
                        "(on (id 100) (set (property \"STATUS\") \"done\"))"))
                 (r (dorg-test-serve--raw
                     port "POST" (format "/mutate?sexp=%s&mode=apply&token=tok" sexp)))
                 (obj (dorg-test-cli--json (cddr r)))
                 (data (alist-get 'data obj)))
            (should (= 200 (car r)))
            (should (equal "mutate" (format "%s" (alist-get 'command data))))
            (should (member 100 (alist-get 'targets data)))
            (should-not (assq 'command obj))))
        ;; real POST delegated through the shared helper exactly once, no :identity,
        ;; resolved policy `dorg' (no HTTP-specific default).
        (should (= 1 mutate-calls))
        (should-not (plist-member mutate-desc :identity))
        (should (eq 'dorg (dorg-mutate--source-identity
                           (dorg-mutate--resolve-source mutate-desc)))))
      (let* ((r (dorg-test-serve--raw
                 port "GET" "/property?key=STATUS&value=done&token=tok"))
             (results (alist-get 'results (dorg-test-cli--json-data (cddr r)))))
        (should (= 200 (car r)))
        (should (= 1 (length results)))
        (should (= 100 (alist-get 'heading-id (car results))))))))

(ert-deftest dorg-test-serve-http-set-route-removed ()
  "The POST /set route is REMOVED in v0.1 (docs/api.org: replaced by /mutate).
It must not succeed (no 200) and must not write STATUS."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let ((r (dorg-test-serve--raw
                port "POST" "/set?key=STATUS&value=done&id=100&token=tok")))
        (should-not (= 200 (car r))))
      ;; STATUS was not written, so the property read finds nothing.
      (let* ((r (dorg-test-serve--raw
                 port "GET" "/property?key=STATUS&value=done&token=tok"))
             (data (dorg-test-cli--json-data (cddr r))))
        (should (= 200 (car r)))
        (should (= 0 (alist-get 'count data)))))))

(ert-deftest dorg-test-serve-http-dolt-list-array ()
  "/dolt/list returns an enveloped server-list payload."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let* ((r (dorg-test-serve--raw port "GET" "/dolt/list?token=tok"))
             (data (dorg-test-cli--json-data (cddr r))))
        (should (= 200 (car r)))
        (should (assq 'servers data))))))

(ert-deftest dorg-test-serve-http-error-envelope ()
  "Bad form and bad query each return one typed HTTP 400 JSON envelope."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (dolist (case
               `(("(nope)" invalid-query nil)
                 ("(tag \"work\") (id 100)" invalid-form trailing-input)))
        (let* ((path (format "/query?sexp=%s&token=tok"
                             (url-hexify-string (car case))))
               (r (dorg-test-serve--raw port "GET" path))
               (lines (split-string (cddr r) "\n" t))
               (obj (and (= 1 (length lines))
                         (json-read-from-string (car lines))))
               (err (cdr (assq 'error obj)))
               (details (cdr (assq 'details err))))
          (should (= 400 (car r)))
          (should (string-match-p "application/json" (cadr r)))
          (should (= 1 (length lines)))
          (should (eq (cdr (assq 'ok obj)) json-false))
          (should (equal (symbol-name (cadr case))
                         (format "%s" (cdr (assq 'reason err)))))
          (when (caddr case)
            (should (equal (symbol-name (caddr case))
                           (format "%s" (cdr (assq 'detail details)))))))))))

(ert-deftest dorg-test-serve-http-auth ()
  "Auth over the real server: 401 missing, 401 bad, 200 good (query token
and X-Dorg-Token header); 401 body is application/json + the envelope."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let* ((r (dorg-test-serve--raw port "GET" "/tag?tag=work")))
        (should (= 401 (car r)))
        (should (string-match-p "application/json" (cadr r)))
        (should (equal "unauthorized"
                       (alist-get 'message
                                  (dorg-test-cli--json-error (cddr r))))))
      (let ((r (dorg-test-serve--raw port "GET" "/tag?tag=work&token=wrong")))
        (should (= 401 (car r))))
      (let ((r (dorg-test-serve--raw port "GET" "/tag?tag=work&token=tok")))
        (should (= 200 (car r))))
      (let ((r (dorg-test-serve--raw port "GET" "/tag?tag=work"
                                     '(("X-Dorg-Token" . "tok")))))
        (should (= 200 (car r)))))))

(ert-deftest dorg-test-serve-http-lifecycle-not-exposed ()
  "/serve/* routes refuse over HTTP — no recursion/exposure of the daemon
lifecycle (the serve route serves dorg-cli, so /serve/* IS reachable)."
  (skip-unless (dorg-test-serve--web-server-p))
  (dorg-test-cli--with-fixture file
    (dorg-test-serve--with-server port "tok" file
      (let* ((r (dorg-test-serve--raw port "GET" "/serve/status?token=tok"))
             (obj (json-read-from-string (cddr r))))
        (should (= 400 (car r)))
        (should (string-match-p "not available over HTTP"
                                (cdr (assq 'message
                                           (cdr (assq 'error obj))))))))))

(ert-deftest dorg-test-serve-http-usage-errors-400 ()
  "Public serve usage/client errors return HTTP 400 + the JSON envelope.
A missing source file (no DORG_FILE/--file) and a dolt command with no
Dolt target are CLIENT errors -> 400 (not 500); genuine crashes stay 500."
  (skip-unless (dorg-test-serve--web-server-p))
  (require 'clime-serve)
  (let* ((port (dorg-test-serve--free-port))
         (server nil)
         (dorg-app--serving t)
         ;; Ensure NO source file is resolvable.
         (process-environment
          (seq-remove (lambda (e) (string-prefix-p "DORG_FILE=" e))
                      process-environment)))
    (unwind-protect
        (progn
          (setq server (clime-serve dorg-cli :port port :host "127.0.0.1"
                                    :default-format 'json
                                    :auth (dorg-app--serve-auth-predicate "tok")))
          ;; /tag with no source file -> 400 + envelope
          (let* ((r (dorg-test-serve--raw port "GET" "/tag?tag=work&token=tok"))
                 (obj (json-read-from-string (cddr r))))
            (should (= 400 (car r)))
            (should (string-match-p "application/json" (cadr r)))
            (should (string-match-p "no org file"
                                    (cdr (assq 'message (cdr (assq 'error obj)))))))
          ;; /dolt/start with no Dolt target -> 400 + envelope
          (let* ((r (dorg-test-serve--raw port "POST" "/dolt/start?token=tok"))
                 (obj (json-read-from-string (cddr r))))
            (should (= 400 (car r)))
            (should (string-match-p "needs a Dolt dir"
                                    (cdr (assq 'message (cdr (assq 'error obj)))))))
          ;; /dolt/stop with no Dolt target -> 400 + envelope
          (let* ((r (dorg-test-serve--raw port "POST" "/dolt/stop?token=tok"))
                 (obj (json-read-from-string (cddr r))))
            (should (= 400 (car r)))
            (should (string-match-p "needs a Dolt dir"
                                    (cdr (assq 'message (cdr (assq 'error obj))))))))
      (when server (ignore-errors (clime-serve-stop server))))))

(ert-deftest dorg-test-serve-http-dolt-start-stop-objects ()
  "Successful /dolt/start and /dolt/stop return JSON OBJECTS (not arrays).
Gated on Dolt: starts and stops a real DOrg-managed sql-server over HTTP."
  (skip-unless (dorg-test-serve--web-server-p))
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((dorg-dolt-auto-server nil)
           ;; The /dolt/start handler cold-starts a real dolt sql-server
           ;; synchronously; give it (and the probe's read budget, which is
           ;; derived from this) generous headroom so a loaded host's slow
           ;; start does not read as an empty body.
           (dorg-dolt-server-startup-timeout 120)
           (temp-base (dorg-test--make-owned-temp-dir
                       "dorg-serve-dolt-" "serve-http-dolt"))
           ;; Hoisted out of the inner `let*' so the failure-path cleanup can
           ;; scope its stop to THIS test's server (tracked-case): the serve
           ;; handler registers the proc in a process filter, outside any
           ;; inner dynamic extent, so let-rebinding cannot isolate it.
           (dolt-dir (expand-file-name "dolt/" temp-base)))
      (unwind-protect
          (let* ((dorg--registry nil)
                 (org-file (expand-file-name "doc.org" temp-base)))
            (dorg-init dolt-dir)
            (with-temp-file org-file
              (insert "* X\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"))
            (dorg-test-serve--with-server port "tok" org-file
              (let ((enc (url-hexify-string (expand-file-name dolt-dir))))
                ;; start -> JSON object {dolt-dir, port, started:true}
                (let* ((r (dorg-test-serve--raw
                           port "POST"
                           (format "/dolt/start?dir=%s&token=tok" enc)))
                       (data (dorg-test-cli--json-data (cddr r))))
                  (should (= 200 (car r)))
                  (should (eq t (alist-get 'started data)))
                  (should (integerp (alist-get 'port data))))
                ;; stop -> JSON object {dolt-dir, stopped}
                (let* ((r (dorg-test-serve--raw
                           port "POST"
                           (format "/dolt/stop?dir=%s&token=tok" enc)))
                       (data (dorg-test-cli--json-data (cddr r))))
                  (should (= 200 (car r)))
                  (should (assq 'stopped data))))))
        ;; Failure-path safety net only (the body already stops its own
        ;; server): scope the stop to THIS test's dolt-dir so a mid-test error
        ;; never reaps the shared fixture server via the global stop-all
        ;; (tracked-case).
        (ignore-errors (dorg-dolt-server--stop dolt-dir))
        (dorg-test--delete-owned-root temp-base)))))

;;; ─── doctor (DB consistency) ──────────────────────────────────────────

(ert-deftest dorg-test-cli-doctor-run-install-clean ()
  "`dorg-app--doctor-run' install branch with NO env findings reports
clean and does not signal."
  (let ((reported nil))
    (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text))
              ((symbol-function 'dorg-doctor--env-findings) (lambda () nil))
              ((symbol-function 'dorg-doctor--format-report)
               (lambda (findings _target) (setq reported findings) "clean"))
              ((symbol-function 'clime-out-text) (lambda (&rest _) nil)))
      ;; install=t → env branch regardless of dir/file.
      (should-not (dorg-app--doctor-run nil nil nil nil t))
      (should (null reported)))))

(ert-deftest dorg-test-cli-doctor-run-install-error-signals ()
  "`dorg-app--doctor-run' install branch signals when an env finding has
`error' severity (exit 1)."
  (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text))
            ((symbol-function 'dorg-doctor--env-findings)
             (lambda () '((:check mysql-el :severity error))))
            ((symbol-function 'dorg-doctor--format-report)
             (lambda (&rest _) "bad"))
            ((symbol-function 'clime-out-text) (lambda (&rest _) nil)))
    (let ((e (should-error (dorg-app--doctor-run nil nil nil nil t))))
      (should (string-match-p "install verification failed"
                              (error-message-string e))))))

(ert-deftest dorg-test-cli-doctor-run-delegates-to-core ()
  "AC1: the DB branch of `dorg-app--doctor-run' routes run/apply/re-check
through the shared `dorg-doctor--run-fix-recheck' core — it does NOT run
checks or apply fixes inline.  The core's own fixable guard makes the driver
byte-equivalent to the pre-qwhnl `(and fix (fixable-p findings))'.

Non-vacuity: the pre-qwhnl CLI calls `dorg-doctor--run-checks' /
`dorg-doctor--apply-fixes' directly and never the core, so the guard stubs
below signal and the test fails.  After IMPL the driver delegates and the
guards stay silent.  Also pins the decision seam: the `should-apply' the
driver hands the core yields the `fix' flag (and never prompts)."
  (let ((core-args nil))
    (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text))
              ((symbol-function 'clime-out-text) (lambda (&rest _) nil))
              ((symbol-function 'dorg-app--doctor-dolt-dir)
               (lambda (&rest _) "/seed/"))
              ((symbol-function 'dorg-doctor--format-report)
               (lambda (&rest _) "ok"))
              ;; The driver must NOT run/apply/re-check on its own.
              ((symbol-function 'dorg-doctor--run-checks)
               (lambda (&rest _) (error "CLI ran checks inline; must delegate to core")))
              ((symbol-function 'dorg-doctor--apply-fixes)
               (lambda (&rest _) (error "CLI applied fixes inline; must delegate to core")))
              ((symbol-function 'dorg-doctor--run-fix-recheck)
               (lambda (target should-apply)
                 (setq core-args (list target should-apply))
                 ;; Clean post-fix findings → driver exits 0 (no signal).
                 (list :findings nil :applied 1))))
      (should-not (dorg-app--doctor-run nil nil "/seed/" t nil))
      ;; Delegated to the core with the resolved target...
      (should (equal "/seed/" (car core-args)))
      ;; ...and a decision seam that yields the `fix' flag (t here) without
      ;; prompting on the (non-interactive) CLI path.
      (should (functionp (cadr core-args)))
      (should (funcall (cadr core-args) '((:check "orphan-root" :fix-sql "SQL")))))))

(ert-deftest dorg-test-cli-doctor-run-delegates-declines-when-no-fix ()
  "AC1 seam contract, no `--fix': the driver still delegates to the core, but
the `should-apply' it hands over yields nil so nothing is applied.  Guards the
byte-equivalence of the core's fixable guard with the old inline `fix' gate."
  (let ((core-args nil))
    (cl-letf (((symbol-function 'clime-out-format) (lambda () 'text))
              ((symbol-function 'clime-out-text) (lambda (&rest _) nil))
              ((symbol-function 'dorg-app--doctor-dolt-dir)
               (lambda (&rest _) "/seed/"))
              ((symbol-function 'dorg-doctor--format-report)
               (lambda (&rest _) "ok"))
              ((symbol-function 'dorg-doctor--run-checks)
               (lambda (&rest _) (error "CLI ran checks inline; must delegate to core")))
              ((symbol-function 'dorg-doctor--apply-fixes)
               (lambda (&rest _) (error "CLI applied fixes inline; must delegate to core")))
              ((symbol-function 'dorg-doctor--run-fix-recheck)
               (lambda (target should-apply)
                 (setq core-args (list target should-apply))
                 (list :findings nil :applied 0))))
      ;; fix = nil.
      (should-not (dorg-app--doctor-run nil nil "/seed/" nil nil))
      (should (equal "/seed/" (car core-args)))
      (should (functionp (cadr core-args)))
      (should-not (funcall (cadr core-args) '((:check "orphan-root" :fix-sql "SQL")))))))

(defun dorg-test-cli--doctor-seed (dolt-dir)
  "Seed DOLT-DIR with one valid file root (1000) + child (1001).
Mirrors `dorg-test-with-doctor-repo' so the doctor CLI tests are
self-contained in this file."
  (dorg-sql-batch dolt-dir
    (list
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (1000, 1000, NULL, X'01', 0, FALSE, NULL, 0)"
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (1001, 1000, 1000, X'01', 1, FALSE, 'Child', 0)"
     "INSERT INTO dorg_files (root_id, relative_path) VALUES (1000, 'doc.org')"))
  (dorg-commit dolt-dir "seed"))

(defun dorg-test-cli--doctor-inject-orphan (dolt-dir)
  "Inject a fixable orphan root (heading 9900, no dorg_files row) into DOLT-DIR."
  (dorg-sql dolt-dir
    "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
  (dorg-commit dolt-dir "inject orphan root"))

(defmacro dorg-test-cli--with-doctor-db (dolt-var &rest body)
  "Run BODY with DOLT-VAR a private seeded dolt repo for doctor CLI tests.
Binds `dorg-test--server-port' nil to force a per-test copy-repo (not a
database on the shared test sql-server, whose store a `dorg doctor'
subprocess must not contend for).  Starts one explicit durable daemon
(`dorg-dolt-start') for that copy and binds `dorg-test--fallback-cli-seam'
nil so BOTH the seeding test process and each sub-invoked `dorg doctor'
attach to that single managed server — the design's explicit-daemon
concurrency boundary.  This replaces the pre-`dorg-dolt-server-autostart'
arrangement, where the subprocess relied on now-default-disabled implicit
autostart; one-shot CLI is deliberately avoided here because it would
deadlock against the running daemon.  Seeds one valid file root (1000) +
child (1001)."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((dorg-test--server-port nil)
         (dorg-test--fallback-cli-seam nil))
     (dorg-test--with-temp-db ,dolt-var
       (dorg-dolt-start ,dolt-var)
       (unwind-protect
           (progn
             (dorg-test-cli--doctor-seed ,dolt-var)
             ,@body)
         (dorg-dolt-stop ,dolt-var)))))

(ert-deftest dorg-test-cli-doctor-clean ()
  "`dorg doctor' on a consistent DB exits 0 with a no-issues report (text+JSON)."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-cli--with-doctor-db dolt-dir
    (let ((r (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir))))
      (should (= 0 (car r)))
      (should (string-match-p "No issues found" (cdr r))))
    (let ((j (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir "--json"))))
      (should (= 0 (car j)))
      (let* ((obj (dorg-test-cli--json (cdr j)))
             (data (alist-get 'data obj)))
        (should (eq t (alist-get 'ok obj)))
        (should (null (alist-get 'error obj)))
        (should (eq t (alist-get 'clean data)))
        (should (assq 'findings data))
        (should (null (alist-get 'findings data)))))))

(ert-deftest dorg-test-cli-doctor-inconsistent ()
  "`dorg doctor' on an inconsistent DB exits 1; text reports the finding,
JSON is the error envelope (non-vacuous: clean exits 0 above)."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-cli--with-doctor-db dolt-dir
    (dorg-test-cli--doctor-inject-orphan dolt-dir)
    (let ((r (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir))))
      (should (= 1 (car r)))
      ;; The streamed text report (stdout) names the finding by its message;
      ;; the check-name summary goes to stderr (discarded by the invoker).
      (should (string-match-p "issue" (cdr r)))
      (should (string-match-p "dorg_files" (cdr r))))
    (let ((j (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir "--json"))))
      (should (= 1 (car j)))
      (let* ((err (dorg-test-cli--json-error (cdr j)))
             (details (alist-get 'details err))
             (findings (alist-get 'findings details))
             (finding (car findings)))
        (should (equal "doctor-findings" (format "%s" (alist-get 'reason err))))
        (should (eq :json-false (alist-get 'clean details)))
        (should (equal "orphan-root" (alist-get 'check finding)))
        (should (assq 'severity finding))
        (should (string-match-p "dorg_files" (alist-get 'message finding)))
        (should (eq t (alist-get 'fixable finding)))))))

(ert-deftest dorg-test-cli-doctor-fix ()
  "`dorg doctor --fix' repairs fixable findings: exit 1 before, 0 after, gone."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-cli--with-doctor-db dolt-dir
    (dorg-test-cli--doctor-inject-orphan dolt-dir)
    ;; Precondition (non-vacuity): inconsistent before the fix.
    (should (= 1 (car (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir)))))
    (let ((r (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir "--fix"))))
      (should (= 0 (car r)))
      (should (string-match-p "Applied" (cdr r))))
    ;; The orphan heading is gone and the DB is clean.
    (should (null (dorg-sql dolt-dir "SELECT id FROM headings WHERE id = 9900")))
    (should (= 0 (car (dorg-test-cli--invoke (list "doctor" "--dolt-dir" dolt-dir)))))))

(ert-deftest dorg-test-cli-dolt-rebind-transforms-one-binding-with-receipt ()
  "`dorg dolt rebind' exposes the exact-base metadata-only operation."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-cli--with-doctor-db dolt-dir
    (let* ((new-file (expand-file-name "moved.org" dolt-dir))
           (base (dorg-head-commit dolt-dir))
           (result
            (dorg-test-cli--invoke
             (list "dolt" "rebind" dolt-dir new-file
                   "--root-id" "1000" "--base" base "--json"))))
      (should (= 0 (car result)))
      (let ((data (dorg-test-cli--json-data (cdr result))))
        (should (equal "dolt-rebind" (format "%s" (alist-get 'command data))))
        (should (eq t (alist-get 'changed data)))
        (should (= 1000 (alist-get 'root-id data)))
        (should (equal base (alist-get 'base data)))
        (should (equal (dorg-head-commit dolt-dir)
                       (alist-get 'commit data))))
      (should-not (file-exists-p new-file))
      (should (equal (dorg-repo-encode-relative-path dolt-dir new-file)
                     (alist-get
                      'relative_path
                      (car (dorg-sql
                            dolt-dir
                            "SELECT relative_path FROM dorg_files WHERE root_id = 1000"))))))))

(ert-deftest dorg-test-cli-dolt-rebind-requires-exact-base ()
  "The administrative CLI refuses an incomplete rebind request as usage."
  (let ((result
         (dorg-test-cli--invoke
          (list "dolt" "rebind" "/tmp/store" "/tmp/new.org"
                "--root-id" "1000"))))
    (should-not (= 0 (car result)))
    (should-not (string-match-p "Debugger\\|backtrace" (cdr result)))))

;;; ─── explicit administrative import ──────────────────────────────────────

(ert-deftest dorg-test-cli-import-bootstrap-omits-replace-base ()
  "`dorg import --mode bootstrap' passes no synthetic :base option."
  (let (captured)
    (cl-letf (((symbol-function 'dorg-import-document)
               (lambda (source &rest options)
                 (setq captured (cons source options))
                 '(:ok t :mode bootstrap
                   :source "/tmp/source.org"
                   :target "/tmp/target.org"
                   :dolt-dir "/tmp/store/"
                   :root-id 10
                   :base-commit "base"
                   :commit "commit"
                   :created (10)
                   :changed nil
                   :deleted nil
                   :extensions nil
                   :invalidation (:roots (10) :extensions nil :scope whole)))))
      (with-output-to-string
        (should
         (= 0
            (clime-run
             dorg-cli
             '("import" "-f" "/tmp/source.org"
               "--dolt-dir" "/tmp/store"
               "--target-file" "/tmp/target.org"
               "--mode" "bootstrap"
               "--json"))))))
    (should (equal (car captured) (expand-file-name "/tmp/source.org")))
    (should-not (plist-member (cdr captured) :base))))

(ert-deftest dorg-test-cli-import-rejects-root-id-selector ()
  "Import resolves its root from --target-file and rejects --root-id."
  (let ((called nil))
    (cl-letf (((symbol-function 'dorg-import-document)
               (lambda (&rest _)
                 (setq called t)
                 (ert-fail "invalid request must not reach import"))))
      (with-output-to-string
        (should-not
         (= 0
            (clime-run
             dorg-cli
             '("import" "-f" "/tmp/source.org"
               "--dolt-dir" "/tmp/store"
               "--root-id" "10"
               "--target-file" "/tmp/target.org"
               "--mode" "bootstrap"
               "--json"))))))
    (should-not called)))

;;; ─── mutate command + removed set (RED SPEC) ─────────────────────────────
;; The public v0.1 write surface is `dorg mutate' (docs/api.org, docs/mutate.org).
;; It requires EXACTLY ONE of --dry-run / --apply, renders the canonical
;; response (text + --json), and REPLACES the removed `dorg set' command.

(ert-deftest dorg-test-cli-mutate-requires-a-mode ()
  "`dorg mutate' with neither --dry-run nor --apply is a mode error (nonzero),
while the same form WITH --dry-run succeeds.  The positive control makes this
non-vacuous: it must FAIL until `mutate' exists, not merely because the
command is unknown."
  (dorg-test-cli--with-fixture file
    (let ((form "(on (id 100) (set (property \"STATUS\") \"DONE\"))"))
      (should-not (= 0 (car (dorg-test-cli--invoke (list "mutate" form "-f" file)))))
      (should (= 0 (car (dorg-test-cli--invoke
                         (list "mutate" form "--dry-run" "-f" file))))))))

(ert-deftest dorg-test-cli-mutate-both-modes-is-error ()
  "`dorg mutate --dry-run --apply' (both) is a mode error (nonzero), while
exactly one mode succeeds (non-vacuous positive control)."
  (dorg-test-cli--with-fixture file
    (let ((form "(on (id 100) (set (property \"STATUS\") \"DONE\"))"))
      (should-not (= 0 (car (dorg-test-cli--invoke
                             (list "mutate" form "--dry-run" "--apply" "-f" file)))))
      (should (= 0 (car (dorg-test-cli--invoke
                         (list "mutate" form "--dry-run" "-f" file))))))))

(ert-deftest dorg-test-cli-mutate-dry-run-does-not-write ()
  "`dorg mutate … --dry-run' succeeds and leaves the file unchanged on disk."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke
                (list "mutate" "(on (id 100) (set (property \"STATUS\") \"DONE\"))"
                      "--dry-run" "-f" file))))
      (should (= 0 (car res)))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (string-match-p "^:STATUS:" (buffer-string)))))))

(ert-deftest dorg-test-cli-mutate-apply-writes ()
  "`dorg mutate … --apply' persists the property to disk."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke
                (list "mutate" "(on (id 100) (set (property \"STATUS\") \"DONE\"))"
                      "--apply" "-f" file))))
      (should (= 0 (car res)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "^:STATUS:[ \t]+DONE" (buffer-string)))))))

(ert-deftest dorg-test-cli-mutate-apply-is-one-saving-api-call ()
  "File-backed apply persists inside its sole API call, never via a facade."
  (dorg-test-cli--with-fixture file
    (let ((api-calls 0)
          seen-options)
      (cl-letf (((symbol-function 'dorg-api-execute)
                 (lambda (_form &rest options)
                   (cl-incf api-calls)
                   (setq seen-options options)
                   '(:ok t :command mutate :mode apply :intent nil
                     :patches nil :targets (100)
                     :affected (:created nil :changed (100) :deleted nil)
                     :supported t :unsupported nil :new-token "token-2")))
                ((symbol-function 'dorg-app--persist)
                 (lambda (&rest _)
                   (ert-fail "CLI apply must not persist outside the API"))))
        (let ((out (with-output-to-string
                     (should (= 0 (clime-run
                                   dorg-cli
                                   (list "mutate"
                                         "(on (id 100) (set (property \"A\") \"1\"))"
                                         "--apply" "--file" file "--json")))))))
          (should (string-prefix-p "{" (string-trim out)))))
      (should (= api-calls 1))
      (should (eq (plist-get seen-options :mode) 'apply))
      (should (eq (plist-get seen-options :save) t)))))

(ert-deftest dorg-test-cli-persist-facade-is-removed ()
  "The CLI has no post-API buffer persistence compatibility path."
  (should-not (fboundp 'dorg-app--persist)))

(ert-deftest dorg-test-cli-mutate-json-is-canonical-object ()
  "`dorg mutate … --dry-run --json' emits the canonical response object with
command/mode/intent/patches/targets and stable envelope fields."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke
                 (list "mutate" "(on (id 100) (set (property \"STATUS\") \"DONE\"))"
                       "--dry-run" "--json" "-f" file)))
           (obj (and (= 0 (car res))
                     (ignore-errors (dorg-test-cli--json (cdr res)))))
           (data (alist-get 'data obj)))
      (should (= 0 (car res)))
      (should (string-prefix-p "{" (string-trim (cdr res))))
      (should (eq t (alist-get 'ok obj)))
      (should (assq 'data obj))
      (should (assq 'error obj))
      (dolist (old '(command mode affected supported unsupported
                     base-commit base-revision new-token patches targets))
        (should-not (assq old obj)))
      (should (equal "mutate" (format "%s" (alist-get 'command data))))
      (should (equal "dry-run" (format "%s" (alist-get 'mode data))))
      (should (alist-get 'patches data))
      (should (member 100 (alist-get 'targets data)))
      (let ((affected (alist-get 'affected data)))
        (should (listp (alist-get 'created affected)))
        (should (listp (alist-get 'changed affected)))
        (should (listp (alist-get 'deleted affected))))
      (should (assq 'unsupported data)))))

(ert-deftest dorg-test-cli-mutate-root-id-selects-explicit-dolt-source ()
  "`dorg mutate --dolt-dir DIR --root-id ID' passes an explicit Dolt source.
Non-vacuous: the stub records the actual `dorg-api-execute' options, and no
`--file' is supplied."
  (let (captured-source)
    (cl-letf (((symbol-function 'dorg-api-execute)
               (lambda (_form &rest options)
                 (setq captured-source (plist-get options :source))
                 '(:ok t :command mutate :mode dry-run :intent nil
                   :patches nil :targets nil
                   :affected (:created nil :changed nil :deleted nil)
                   :supported t :unsupported nil)))
              ((symbol-function 'dorg-app--persist)
               (lambda (&rest _)
                 (error "direct Dolt source must not persist a buffer"))))
      (let ((out (with-output-to-string
                   (should (= 0 (clime-run
                                 dorg-cli
                                 (list "mutate"
                                       "(on (id 100) (set (property \"A\") \"1\"))"
                                       "--dry-run" "--dolt-dir" "/tmp/dorg-dolt"
                                       "--root-id" "4242" "--json")))))))
        (should (string-prefix-p "{" (string-trim out)))))
    (should (equal (list :dolt-dir (expand-file-name "/tmp/dorg-dolt")
                         :root-id 4242)
                   captured-source))))

(ert-deftest dorg-test-cli-mutate-root-id-without-dolt-dir-errors ()
  "`--root-id' without `--dolt-dir' is a clean usage error."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke
                 (list "mutate" "(on (id 100) (set (property \"A\") \"1\"))"
                       "--dry-run" "--root-id" "4242" "-f" file "--json")))
           (err (ignore-errors (dorg-test-cli--json-error (cdr res)))))
      (should-not (= 0 (car res)))
      (should (string-match-p "--root-id requires --dolt-dir"
                              (alist-get 'message err)))
      (should (alist-get 'reason err)))))

(ert-deftest dorg-test-cli-mutate-rejects-dorg-id ()
  "`dorg mutate' rejects a DORG_ID property write (nonzero) and leaves identity
intact — the DORG_ id-protection formerly covered by `dorg set --id'.
Non-vacuous positive control: a valid (non-DORG_) property mutate succeeds, so
this fails until `mutate' exists rather than merely because the command is
unknown."
  (dorg-test-cli--with-fixture file
    ;; positive control: a legal property mutate applies (exit 0).
    (should (= 0 (car (dorg-test-cli--invoke
                       (list "mutate" "(on (id 100) (set (property \"OK\") \"1\"))"
                             "--apply" "-f" file)))))
    ;; DORG_ID is protected: rejected (nonzero), identity unchanged on disk.
    (should-not (= 0 (car (dorg-test-cli--invoke
                           (list "mutate" "(on (id 100) (set (property \"DORG_ID\") \"999\"))"
                                 "--apply" "-f" file)))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "DORG_ID: 100" (buffer-string)))
      (should-not (string-match-p "DORG_ID: 999" (buffer-string))))))

(ert-deftest dorg-test-cli-mutate-error-envelope-carries-reason-details ()
  "The mutate handler's `dorg-api-error' catch carries the REAL reason symbol
and details plist onto the JSON error envelope (regression for the latent bug
where reason was set to the message string).  Forces a known `dorg-api-error'
(the DORG_ID identity rejection -> reason `invalid-target', data carrying the
offending key) and pins the emitted envelope's `error.reason' and
`error.details'.  Reverting the catch-site accessor from
`(dorg-api-error-reason err)' to `(cadr err)' makes `error.reason' the message
string instead of `invalid-target', turning this red — the gap the existing
`mutate-rejects-dorg-id' (nonzero/no-write only) does not cover."
  (dorg-test-cli--with-fixture file
    (let* ((res (dorg-test-cli--invoke
                 (list "--json" "mutate"
                       "(on (id 100) (set (property \"DORG_ID\") \"999\"))"
                       "--apply" "-f" file)))
           (err (and (not (= 0 (car res)))
                     (dorg-test-cli--json-error (cdr res)))))
      (should-not (= 0 (car res)))
      (should err)
      ;; Reason is the real symbol name from the condition, NOT the human
      ;; message string; `(cadr err)' at the catch site would yield the latter.
      (should (equal "invalid-target" (alist-get 'reason err)))
      (should-not (equal (alist-get 'message err) (alist-get 'reason err)))
      ;; Details ride `dorg-api-error-data': the offending key is preserved.
      (let ((details (alist-get 'details err)))
        (should details)
        (should (equal "DORG_ID" (alist-get 'key details)))
        (should (equal "invalid-target" (alist-get 'reason details)))))))

(ert-deftest dorg-test-bejl7-cli-mutate-dolt-insert-after-anchor-order ()
  "CLI mutate preserves repeated same-anchor insert order for Dolt sources."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (dorg-test--make-owned-temp-dir
                       "dorg-cli-bejl7-" "cli mutate bejl7"))
           (dolt-dir nil)
           (server-started nil))
      (unwind-protect
          (progn
            (setq dolt-dir (dorg-test--copy-repo temp-base))
            (let ((dorg-dolt-server-startup-timeout 120))
              (dorg-dolt-server--ensure-detached dolt-dir)
              (setq server-started t))
            (let* ((root 100)
                   (_seed (progn
                            (dorg-sql-batch
                             dolt-dir
                             (dorg-test--parse-to-sql "* X\n* Tail\n" root))
                            (dorg-commit dolt-dir "seed")))
                   (xid (alist-get
                         'id
                         (car (dorg-sql
                               dolt-dir
                               "SELECT id FROM headings WHERE title = 'X' LIMIT 1"))))
                   (form
                    (format
                     (concat "(batch "
                             "(insert (heading (title \"A\")) "
                             "(under document (after (id %d)))) "
                             "(insert (heading (title \"B\")) "
                             "(under document (after (id %d)))))")
                     xid xid))
                   (res (dorg-test-cli--invoke
                         (list "mutate" form "--apply"
                               "--dolt-dir" dolt-dir
                               "--root-id" (number-to-string root))))
                   (titles
                    (mapcar (lambda (row) (alist-get 'title row))
                            (dorg-sql
                             dolt-dir
                             (format
                              "SELECT title FROM headings WHERE parent_id = %d ORDER BY sort_key"
                              root)))))
              (should (= 0 (car res)))
              (should (equal titles '("X" "A" "B" "Tail")))))
        (when (and server-started dolt-dir)
          (ignore-errors (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-cli-set-command-removed ()
  "The public `dorg set' command is REMOVED (v0.1 -> mutate).  Invoking it is
an unknown-command error (nonzero exit); it no longer writes the file."
  (dorg-test-cli--with-fixture file
    (let ((res (dorg-test-cli--invoke
                (list "set" "STATUS" "done" "--id" "100" "-f" file))))
      (should-not (= 0 (car res)))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (string-match-p "^:STATUS:" (buffer-string)))))))

;;; ─── doctor install-mode exit code (tracked-case) ──────────────────────────
;; `dorg doctor --install' is the installer's post-install verification, so it
;; MUST exit NONZERO when the mysql.el hard dependency is unavailable (else a
;; `curl | bash' installer cannot detect the failure).  The nonzero exit is
;; driven by a SIGNALED error, because the streaming text format ignores
;; `clime-out-error' for the process exit code.

(defun dorg-test-cli--doctor-install-code (&optional break-mysql)
  "Run `dorg doctor --install' in a subprocess; return its exit code.
When BREAK-MYSQL is non-nil, `dorg-sql--mysql-available-p' is advised to nil
so the env check sees mysql.el as unavailable."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (process-environment (cons "CLIME_MAIN_APP=dorg-cli" process-environment))
         (pre (if break-mysql
                  (list "--eval" "(require 'dorg-dolt)"
                        "--eval" "(advice-add 'dorg-sql--mysql-available-p :override (lambda (&rest _) nil))")
                nil)))
    (apply #'call-process emacs nil nil nil
           "--batch" "-Q"
           "-L" dorg-test-cli--root
           "-L" (dorg-test-cli--path "lib/clime")
           "-L" (dorg-test-cli--path "lib/mysql")
           (append pre
                   (list "-l" (dorg-test-cli--path "dorg-app.el")
                         "--" "doctor" "--install")))))

(ert-deftest dorg-test-cli-doctor-install-exit-code ()
  "`dorg doctor --install' exits 0 with mysql.el present and NONZERO when it is
unavailable.  Non-vacuous: the positive control (mysql present -> 0) proves the
nonzero case is driven by the hard-dependency error, not a constant failure."
  (skip-unless (executable-find (expand-file-name invocation-name invocation-directory)))
  (should (= 0 (dorg-test-cli--doctor-install-code nil)))
  (should-not (= 0 (dorg-test-cli--doctor-install-code t))))

;;; ─── cu9xm: subprocess server-index isolation (tracked-regression) ───────────────
;;
;; `dorg-test-cli--invoke' is the second wiring point for the cross-process index
;; isolation (the first is the suite-runner bootstrap; see dorg-dolt-server-
;; tests.el): it prepends DORG_DOLT_SERVER_INDEX_DIR=<isolated dir> to the
;; child's environment UNLESS the caller already supplies one in EXTRA-ENV, so a
;; foreign/live index reachable via the implicit environment cannot leak into a
;; spawned child while a deliberate caller override still wins.

(defun dorg-test-cli-cu9xm--plant-live-store (index-dir)
  "Plant a foreign store carrying a LIVE-port daemon claim into INDEX-DIR.
Opens a real loopback listener so the claim classifies live and the store is
discoverable via `dorg dolt list'.  Returns a plist (:store PATH :listener PROC)
the caller MUST clean up (delete the process and the store dir)."
  (let* ((store (file-name-as-directory (make-temp-file "cu9xm-cli-store-" t)))
         (listener (make-network-process :name "cu9xm-cli-listen" :host "127.0.0.1"
                                         :service 0 :server t :family 'ipv4))
         (port (process-contact listener :service)))
    (let ((dorg-dolt-server-index-dir index-dir))
      (dorg-dolt-server--claim-write
       store (list (cons :dolt-dir (dorg-dolt-server--canonical-dir store))
                   (cons :host (system-name)) (cons :owner-pid 424242)
                   (cons :owner-kind 'daemon) (cons :server-pid 424242)
                   (cons :port port) (cons :token "cu9xm-tok")
                   (cons :policy 'server)
                   (cons :protocol dorg-dolt-server-claim-protocol)
                   (cons :created-at (float-time)) (cons :last-used (float-time))))
      (dorg-dolt-server--index-register store))
    (list :store store :listener listener)))

(ert-deftest dorg-test-cli-cu9xm-invoke-isolates-server-index ()
  "`dorg-test-cli--invoke' scopes the CHILD's cross-process server index so a
foreign index reachable via the inherited environment does not leak in.  A
foreign store with a LIVE claim is planted into index dir A and A is placed on
the implicit environment; an isolated child must still list count 0.  RED now
\(the child inherits DORG_DOLT_SERVER_INDEX_DIR=A and discovers the foreign
store — the 5d6de85 cross-process-inventory isolation defect)."
  (let* ((orig-env (getenv "DORG_DOLT_SERVER_INDEX_DIR"))
         (idx-a (file-name-as-directory (make-temp-file "cu9xm-cli-idx-" t)))
         (planted (dorg-test-cli-cu9xm--plant-live-store idx-a)))
    (unwind-protect
        (progn
          ;; A naive child inheriting this env would see the foreign store.
          (setenv "DORG_DOLT_SERVER_INDEX_DIR" idx-a)
          (let* ((res (dorg-test-cli--invoke (list "dolt" "list" "--json")))
                 (data (dorg-test-cli--json-data (cdr res))))
            (should (= 0 (car res)))
            (should (= 0 (alist-get 'count data)))))   ; isolated: foreign store unseen
      (setenv "DORG_DOLT_SERVER_INDEX_DIR" orig-env)
      (ignore-errors (delete-process (plist-get planted :listener)))
      (ignore-errors (delete-directory idx-a t))
      (ignore-errors (delete-directory (plist-get planted :store) t)))))

(ert-deftest dorg-test-cli-cu9xm-invoke-honors-caller-index-override ()
  "`dorg-test-cli--invoke' must NOT clobber a caller-supplied
DORG_DOLT_SERVER_INDEX_DIR in EXTRA-ENV: when the caller pins index C (holding a
foreign live store) while the implicit environment points at an EMPTY index, the
child reads C and lists count 1.  Green guard (blocker 3): passes now (EXTRA-ENV
is honored) and after the fix (which injects the isolated dir ONLY when the
caller supplies none).  Non-vacuous — a mutant that unconditionally prepends the
isolated dir AHEAD of EXTRA-ENV re-reddens this: the child would read the empty
implicit index and count 0."
  (let* ((orig-env (getenv "DORG_DOLT_SERVER_INDEX_DIR"))
         (idx-c (file-name-as-directory (make-temp-file "cu9xm-cli-idxc-" t)))
         (idx-implicit (file-name-as-directory (make-temp-file "cu9xm-cli-amb-" t)))
         (planted (dorg-test-cli-cu9xm--plant-live-store idx-c)))
    (unwind-protect
        (progn
          ;; Implicit env points at an EMPTY index; only the caller override (C)
          ;; holds the live store.
          (setenv "DORG_DOLT_SERVER_INDEX_DIR" idx-implicit)
          (let* ((res (dorg-test-cli--invoke
                       (list "dolt" "list" "--json")
                       (list (concat "DORG_DOLT_SERVER_INDEX_DIR=" idx-c))))
                 (data (dorg-test-cli--json-data (cdr res))))
            (should (= 0 (car res)))
            (should (= 1 (alist-get 'count data)))))   ; caller override honored
      (setenv "DORG_DOLT_SERVER_INDEX_DIR" orig-env)
      (ignore-errors (delete-process (plist-get planted :listener)))
      (ignore-errors (delete-directory idx-c t))
      (ignore-errors (delete-directory idx-implicit t))
      (ignore-errors (delete-directory (plist-get planted :store) t)))))

(provide 'dorg-cli-tests)
;;; dorg-cli-tests.el ends here
