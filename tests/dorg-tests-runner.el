;;; dorg-tests-runner.el --- Test runner for dorg  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test runner and utilities for dorg tests.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Provided by `dorg-test-helpers' (required below, before any call at runtime).
(declare-function dorg-test--isolate-server-index "dorg-test-helpers")

;;; ─── Source-binding policy (tracked-case) ───────────────────────────────
;;
;; Focused exact-SHA certification loads this runner directly
;;   emacs --batch -Q -L . -L ./tests -l tests/dorg-tests-runner.el --eval ...
;; which does NOT run `make clean-elc' first.  Emacs' default `load-prefer-newer'
;; is nil, so the `(require 'dorg)' below would load a leftover dorg*.elc from a
;; prior `make lint/compile' even when it is older than the checked-out source
;; (Emacs prints "Source file ... newer than byte-compiled file" and uses the
;; older file anyway) — binding the verdict to stale bytecode rather than the
;; candidate commit.  Two complementary mechanisms, established BEFORE the first
;; project require, remove this:
;;
;;   A. `load-prefer-newer' t — when source is newer than its .elc, load source.
;;   B. a fail-closed mtime guard — any project .elc that is NOT strictly older
;;      than its sibling .el (so it could win under `load-prefer-newer', or has
;;      no source at all) has unprovable provenance for an exact-SHA run, so
;;      signal and name the artifacts instead of emitting a verdict.  A strictly
;;      older .elc is harmless (Mechanism A loads source) and does not trip it.
;;
;; `make test'/`test-fast'/`test-medium'/`perf'/... run `clean-elc' first, so
;; their trees carry no .elc and both mechanisms are no-ops there.  The policy
;; scope is the project source (repo root + tests/), captured from
;; `load-file-name' at LOAD time so it binds to the checked-out worktree,
;; independent of the arbitrary implicit `default-directory' a cert run uses.

(defvar dorg-run-tests--dorg-loaded-at-enforce 'unset
  "Whether `dorg' was already a feature when enforcement last ran.
Sentinel `unset' until `dorg-run-tests--enforce-source-binding' runs; then set
to `(featurep 'dorg)' at that instant.  nil proves enforcement ran BEFORE
`(require 'dorg)'; a non-nil value would mean it ran after.  Read back by the
fresh-process integration guard in dorg-test-infra-tests.el.")

(defconst dorg-run-tests--project-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute repository root (parent of tests/), from `load-file-name'.
Captured at runner LOAD time so the enforced scope binds to the checked-out
source tree, independent of implicit `default-directory' — the exact-SHA cert
invocation runs from an arbitrary cwd.")

(defun dorg-run-tests--default-source-dirs ()
  "Default project source dirs the policy scopes to: repo root and tests/."
  (list dorg-run-tests--project-root
        (expand-file-name "tests" dorg-run-tests--project-root)))

(defun dorg-run-tests--source-bound-elc-guard (dirs)
  "Fail closed if any *.elc directly under DIRS could shadow its source.
Signal an error naming every *.elc whose sibling *.el is NOT strictly newer
\(the artifact would win under `load-prefer-newer', or has no source at all),
instructing `make clean-elc'.  Return nil when every *.elc is strictly older
than its source (harmless — Mechanism A loads source)."
  (let (ambiguous)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (elc (directory-files dir t "\\.elc\\'"))
          (let ((el (substring elc 0 -1)))     ; drop trailing ?c => sibling .el
            (unless (file-newer-than-file-p el elc)
              (push elc ambiguous))))))
    (when ambiguous
      (error (concat "Source-binding guard: %d byte-compiled file(s) not "
                     "strictly older than source; verdict cannot bind to the "
                     "checked-out commit.  Run `make clean-elc'.  Offending: %s")
             (length ambiguous)
             (mapconcat #'identity (nreverse ambiguous) ", ")))
    nil))

(defun dorg-run-tests--enforce-source-binding (&optional dirs)
  "Establish the source-binding policy before any project byte-code loads.
Record whether `dorg' is already loaded (load-order witness), prefer source
over stale bytecode (Mechanism A), then fail closed on ambiguous artifacts
under DIRS, or `dorg-run-tests--default-source-dirs' when DIRS is nil
\(Mechanism B)."
  (setq dorg-run-tests--dorg-loaded-at-enforce (featurep 'dorg))
  (setq load-prefer-newer t)
  (dorg-run-tests--source-bound-elc-guard
   (or dirs (dorg-run-tests--default-source-dirs))))

;; Enforce with the DEFAULT worktree scope, BEFORE the first project require,
;; so no project byte-code can be loaded outside the policy.
(dorg-run-tests--enforce-source-binding)

(require 'dorg)

(defvar org-element-cache-persistent)   ; defined in org-element

;;; ─── Test Environment Setup ───────────────────────────────────────────

(defun dorg-run-tests--strip-org-persist-kill-hooks ()
  "Drop every org-persist entry from `kill-emacs-hook'.
Fatal-path hygiene (tracked-case): the batch test process contributes
nothing to `kill-emacs-hook', and org-persist joins it lazily when
org-element cache persistence engages.  Mirrors the batch hardening in
`dorg-app--harden-batch-org-persist'."
  (let (kept)
    (dolist (entry kill-emacs-hook)
      (unless (and (symbolp entry)
                   (string-prefix-p "org-persist-" (symbol-name entry)))
        (push entry kept)))
    (setq kill-emacs-hook (nreverse kept))))

(defun dorg-setup-test-environment ()
  "Setup the test environment for dorg.
Configure project-specific settings for clean test execution.
Hardens org-persist out of the fatal path in LOAD ORDER: persistence is
disabled, and the `kill-emacs-hook' strip runs both now (when org-persist
is already loaded) and again whenever org-persist (re)loads later — lazy
re-addition after setup returned is covered."
  ;; Scope the cross-process server-index registry to a per-run OWNED dir
  ;; BEFORE any test consumes inventory (tracked-regression).  Points both the
  ;; in-process var and DORG_DOLT_SERVER_INDEX_DIR at an isolated dir so
  ;; neither this process nor its CLI children discover the operator's live
  ;; tracker sql-server.  Idempotent: the run has exactly one isolated index.
  (dorg-test--isolate-server-index)
  ;; Invocation history is default-on for real DOrg use.  The test process and
  ;; all children inherit an explicit disable unless a history test prepends an
  ;; isolated destination of its own; no suite run may touch the user's state.
  (setenv "DORG_CLI_HISTORY_FILE" "")
  (setq max-lisp-eval-depth 1000
        print-level 1000
        print-length 1000)
  (setq org-element-cache-persistent nil)
  (with-eval-after-load 'org-persist
    (dorg-run-tests--strip-org-persist-kill-hooks))
  (message "dorg test environment setup complete."))

;;; ─── Load Test Files ──────────────────────────────────────────────────

(require 'dorg-test-helpers)
(require 'dorg-test-scratch-tests)
(require 'dorg-sortkey-tests)
(require 'dorg-dolt-tests)
(require 'dorg-tracker-cli-dou7f-probe-tests)
(require 'dorg-dolt-server-tests)
(require 'dorg-transport-tests)
(require 'dorg-lease-tests)
(require 'dorg-test-infra-tests)
(require 'dorg-ext-tests)
(require 'dorg-id-tests)
(require 'dorg-freshness-tests)
(require 'dorg-sexp-file-tests)
(require 'dorg-timestamp-tests)
(require 'dorg-parse-tests)
(require 'dorg-import-tests)
(require 'dorg-keywords-tests)
(require 'dorg-git-tests)
(require 'dorg-git-sync-tests)
(require 'dorg-render-tests)
(require 'dorg-projection-tests)
(require 'dorg-sql-gen-tests)
(require 'dorg-placement-tests)
(require 'dorg-doctor-tests)
(require 'dorg-vfs-tests)
(require 'dorg-model-tests)
(require 'dorg-mode-tests)
(require 'dorg-materialize-lifecycle-tests)
(require 'dorg-integration-tests)
(require 'dorg-perf-tests)
(require 'dorg-backend-tests)
(require 'dorg-binding-isolation-tests)
(require 'dorg-find-tests)
(require 'dorg-ql-tests)
(require 'dorg-mutate-tests)
(require 'dorg-mutate-persist-tests)
(require 'dorg-mutate-linearize-tests)
(require 'dorg-envelope-tests)
(require 'dorg-roundtrip-corpus-tests)
(require 'dorg-ql-ast-tests)
(require 'dorg-ql-eval-tests)
(require 'dorg-session-tests)
(require 'dorg-command-tests)
(require 'dorg-cli-tests)
(require 'dorg-reload-tests)
(require 'dorg-project-tests)
(require 'dorg-ext-index-context-tests)
(require 'dorg-ext-index-projector-tests)
(require 'dorg-ext-rebuild-tests)
(require 'dorg-2ij34-postimage-tests)
(require 'dorg-ext-read-tests)
(require 'dorg-history-tests)
(require 'dorg-api-tests)
(require 'dorg-mutate-grammar-tests)
(require 'dorg-error-tests)
(require 'dorg-faithful-tests)
(require 'dorg-grammar-tests)
(require 'dorg-test-gates)

;;; ─── Test Runner Functions ────────────────────────────────────────────

(defvar dorg-test-verbose nil
  "Non-nil lets `message' output through to stderr during tests.
Set via DORG_TEST_VERBOSE env var or VERBOSE make variable.")

(defun dorg-run-tests (&optional selector)
  "Run dorg tests interactively matching SELECTOR."
  (interactive)
  (dorg-setup-test-environment)
  (ert-run-tests-interactively (or selector "^dorg-test-")))

(defun dorg-run-tests--maybe-verbose ()
  "Enable stderr message passthrough when DORG_TEST_VERBOSE is set."
  (let ((env (getenv "DORG_TEST_VERBOSE")))
    (when (and env (not (string-empty-p env)))
      (setq dorg-test-verbose t)))
  (when dorg-test-verbose
    ;; Redirect `message' to stderr so ERT can't swallow it
    (advice-add 'message :after
                (lambda (fmt &rest args)
                  (let ((text (apply #'format-message fmt args)))
                    (princ (concat text "\n") #'external-debugging-output)))
                '((name . dorg-test-verbose)))))

(defun dorg-run-tests--maybe-diag ()
  "Enable test-only diagnostics when DORG_TEST_DIAG_DIR is set.
Creates the directory, starts an empty <dir>/client-db-errors.log, and
advises `dorg-dolt--classify-db-error' to append the RAW database error and
failed statement before the product sanitizes them.  CI evidence only."
  (let ((env (getenv "DORG_TEST_DIAG_DIR")))
    (when (and env (not (string-empty-p env)))
      (setq dorg-test-diag-dir (expand-file-name env))
      (make-directory dorg-test-diag-dir t)
      (let ((log (expand-file-name "client-db-errors.log" dorg-test-diag-dir)))
        (write-region "" nil log nil 'silent)
        (advice-add 'dorg-dolt--classify-db-error :before
                    (lambda (err &optional failed-statement)
                      (condition-case nil
                          (write-region
                           (format "%s\nERROR: %S\nSTATEMENT: %s\n\n"
                                   (format-time-string "%FT%T%z")
                                   err failed-statement)
                           nil log 'append 'silent)
                        (error nil)))
                    '((name . dorg-test-diag))))
      (message "dorg-test: diagnostics -> %s" dorg-test-diag-dir))))

(defun dorg-run-tests--shared-invariant-violation-p
    (armed baseline-pid baseline-cold end-pid end-cold)
  "Return non-nil when the shared test sql-server was stopped or replaced.
ARMED is non-nil only when a real shared server started (integer port);
the nil-start CLI fallback leaves it nil, and this predicate is then nil
for ANY end state (tracked-case Blocker 1 — the legitimate fallback is
DISARMED, never a false-fail).  When ARMED, a violation is either the
baseline proc dying or being replaced (BASELINE-PID present and END-PID
differs) OR a cold-start replacement (BASELINE-COLD and END-COLD both
integers with END-COLD greater than BASELINE-COLD).  The two clauses are
independent: the cold-start clause catches a replace-with-fresh-store even
when a new proc merely has a different pid, and the pid clause catches a
kill/replace when cold-start counts are unavailable."
  (and armed
       (or (and baseline-pid (not (equal end-pid baseline-pid)))
           (and (integerp baseline-cold) (integerp end-cold)
                (> end-cold baseline-cold)))))

(defun dorg-run-tests--batch (selector start-server)
  "Run ERT SELECTOR in batch and exit; manage shared server per START-SERVER.
When START-SERVER is nil the shared sql-server is neither started nor
stopped — used by the FAST gate, whose Dolt-free suites need no server and
must not pay its startup or contend on Dolt.  The materialized-template
cleanup always runs."
  (dorg-setup-test-environment)
  (dorg-run-tests--maybe-verbose)
  (dorg-run-tests--maybe-diag)
  (let ((exit-status 2))
    (unwind-protect
        (setq exit-status
              (condition-case err
                  ;; Reap ownership records left by dead runs at EVERY batch
                  ;; startup — fast gate included (tracked-case): fatal-signal
                  ;; cleanup is deferred to exactly this point, so the NEXT test
                  ;; startup must reap it deterministically whichever gate runs
                  ;; next, and always before this run's shared server can spawn.
                  ;; Any dead-owner record left UNREAPED (an orphan server we
                  ;; could not confirm gone) BLOCKS a fresh shared start — else
                  ;; every crash accumulates one orphan plus one new server —
                  ;; and forces a nonzero exit.
                  (let ((stale-orphans (dorg-test--reap-stale-server-registry)))
                    (if (and start-server stale-orphans)
                        ;; Unreaped orphans + a shared start requested: refuse to
                        ;; spawn (would accumulate one orphan + one server per
                        ;; run), do NOT run tests on a corrupt baseline, and
                        ;; return a numeric nonzero exit.
                        (progn
                          (message "dorg-test: %d unreaped dead-owner orphan record(s) remain; NOT starting a fresh shared server or running tests"
                                   (length stale-orphans))
                          1)
                      ;; Otherwise: optionally start, then ALWAYS run ERT and
                      ;; combine the test status with the stale status.
                      (let (armed baseline-pid baseline-cold)
                        (when start-server
                          ;; A nil return is the legitimate unsupported/
                          ;; mysql-absent CLI fallback.  A SIGNAL means the
                          ;; infrastructure failed (possibly leaving a retained
                          ;; live partial) and MUST propagate to the outer
                          ;; handler (exit 2) — never downgraded to a green
                          ;; fallback that runs tests as if healthy.
                          (let ((port (dorg-test--start-shared-server)))
                            (when port
                              (message "dorg-test: shared server started on port %d" port))
                            ;; ARM the shared-server invariant ONLY on a real
                            ;; shared start (integer port); the nil-start CLI
                            ;; fallback has no shared proc to protect, so the
                            ;; guard stays disarmed (tracked-case).
                            (setq armed (integerp port))
                            (when armed
                              (setq baseline-pid (dorg-test--shared-server-pid)
                                    baseline-cold (plist-get
                                                   (dorg-dolt-fixture-inventory)
                                                   :cold-starts)))))
                        (let* ((stats (ert-run-tests-batch selector))
                               ;; Sample the shared proc BEFORE the unwind
                               ;; teardown legitimately stops it, so END-PID
                               ;; reflects a TEST-caused kill/replace, not the
                               ;; teardown close (tracked-case).
                               (end-pid (dorg-test--shared-server-pid))
                               (end-cold (plist-get (dorg-dolt-fixture-inventory)
                                                    :cold-starts))
                               (violated (dorg-run-tests--shared-invariant-violation-p
                                          armed baseline-pid baseline-cold
                                          end-pid end-cold)))
                          (when violated
                            (message "dorg-test: INVARIANT VIOLATED — shared server stopped/replaced during tests (a test ran stop-all against the global --procs); baseline-pid=%S end-pid=%S cold %S->%S"
                                     baseline-pid end-pid baseline-cold end-cold))
                          (if (or stale-orphans violated
                                  (not (zerop (ert-stats-completed-unexpected stats))))
                              1 0)))))
                (error
                 (message "Error running tests")
                 (message "%s" (error-message-string err))
                 (backtrace)
                 2)))
      ;; DRIVE + VERIFY teardown (tracked-case).  Every cleanup step is attempted
      ;; INDEPENDENTLY under its own error/quit guard so one failure never
      ;; aborts the rest, and ANY failure — a non-nil `release-stores' return, a
      ;; failed shared close, a non-zero final inventory, an unconfirmed
      ;; owned-root reap, a stop-all/template failure — forces a nonzero exit
      ;; EVEN WHEN the final inventory reads zero (the shared close masks a
      ;; leaked store by forgetting it, so the DROP failure must be surfaced by
      ;; `release-stores' itself, not inferred from the inventory).
      (let ((teardown-failed nil))
        (cl-flet ((step (label thunk)
                    (condition-case err
                        (funcall thunk)
                      ((error quit)
                       (setq teardown-failed t)
                       (message "dorg-test: teardown step %s FAILED: %s"
                                label (error-message-string err))
                       nil))))
          (when start-server
            ;; Report the cold-start metric (best-effort; never fatal).
            (step 'cold-start-report
                  (lambda ()
                    (message "dorg-test: fixture cold-starts=%d"
                             (plist-get (dorg-dolt-fixture-inventory)
                                        :cold-starts))))
            ;; Drain outstanding stores; a NON-NIL return is an unmasked leak.
            (step 'release-stores
                  (lambda ()
                    (let ((failures (dorg-dolt-fixture-release-stores)))
                      (when failures
                        (error "%d store release(s) failed" (length failures))))))
            ;; Close protocol-writing clients while the suite-wide server is
            ;; still alive.  `stop-all' remains the later crash net, but on a
            ;; healthy unwind it must find this connection pool empty rather
            ;; than send COM_QUIT to peers killed by `shared-close'.
            (step 'pooled-connection-close
                  (lambda () (dorg-sql--close-all-connections)))
            ;; Close the shared server through the fixture (exact public stop).
            (step 'shared-close (lambda () (dorg-test--stop-shared-server)))
            ;; Reclaim any retained failed-start partial in THIS process (a
            ;; pre-readiness live proc may not be in `--procs', so next-start
            ;; reaping is too late for the zero-after-unwind objective).  A
            ;; non-nil return is an unreclaimed partial -> force nonzero.
            (step 'failed-start-drain
                  (lambda ()
                    (let ((failure (dorg-dolt-fixture-drain-failed-start)))
                      (when failure
                        (error "failed-start partial not reclaimed: %S" failure)))))
            ;; Inventory MUST be exactly zero servers and zero stores.
            (step 'inventory-zero
                  (lambda ()
                    (let ((inv (dorg-dolt-fixture-inventory)))
                      (unless (and (eql 0 (plist-get inv :servers))
                                   (eql 0 (plist-get inv :stores)))
                        (error "fixture not fully closed: %S" inv))))))
          ;; Secondary crash-net (fast gate included): ordinary fixtures reap
          ;; before deleting their roots.  A non-nil return is an unconfirmed
          ;; reap (a root whose server could not be confirmed gone) — surfaced.
          (step 'owned-root-crash-net
                (lambda ()
                  (let ((failures (dorg-test--reap-owned-roots)))
                    (when failures
                      (error "%d owned-root reap(s) unconfirmed" (length failures))))))
          ;; Explicit replacement for the removed `kill-emacs-hook' registration
          ;; (tracked-case): managed servers stop in normal unwind context.
          (step 'stop-all (lambda () (dorg-dolt-server-stop-all)))
          ;; Materialized-template cleanup always runs.
          (step 'template-cleanup (lambda () (dorg-test--cleanup-template))))
        (when (and teardown-failed (zerop exit-status))
          (setq exit-status 1))))
    (kill-emacs exit-status)))

(defun dorg-run-tests-batch (&optional selector)
  "Run dorg tests in batch mode matching SELECTOR (a regexp; default full).
When `dorg-test-verbose' is non-nil, test messages are printed to stderr
instead of being captured by ERT.  Starts the shared sql-server."
  (dorg-run-tests--batch
   (if (or (null selector) (string-empty-p selector)) "^dorg-test-" selector)
   t))

(defun dorg-run-tests-batch-gate (gate-name)
  "Run the GATE-NAME tier in batch and exit: \"fast\", \"medium\", or \"full\".
Resolves the tier's ERT selector via `dorg-test-gate-selector'.  The FAST
gate (Dolt-free suites) skips the shared sql-server entirely, so it pays no
server startup and never contends on Dolt; MEDIUM and FULL start it as
usual.  See `dorg-test-gates' and the Makefile `test-fast'/`test-medium'
targets."
  (let ((gate (intern gate-name)))
    (dorg-run-tests--batch (dorg-test-gate-selector gate)
                           (not (eq gate 'fast)))))

(provide 'dorg-tests-runner)
;;; dorg-tests-runner.el ends here
