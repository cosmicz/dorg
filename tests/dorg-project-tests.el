;;; dorg-project-tests.el --- Module/test/doc map drift checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Deterministic, filesystem-only guards that keep the project's durable
;; maps from drifting apart as modules and tests are added.  They compare,
;; with no tracker / Dolt / sql-server / network dependency:
;;
;;   - actual `dorg-*.el' modules at the repo root
;;   - the self-discovering `dorg-reload' contract (discovery == loaded
;;     root modules minus the CLI app `dorg-app')
;;   - the `dorg--reload-priority' foundational seed in `dorg.el' (every
;;     seed entry has a file; every struct/macro-defining module is a
;;     seed member, so reload never leaves a dependent with a stale macro)
;;   - the direct `(require 'dorg-*)' block in `dorg.el'
;;   - the `(require 'dorg-*-tests)' list in `tests/dorg-tests-runner.el'
;;   - the documented module/test maps in `DEVELOPMENT.org'
;;   - the ERT selector policy (`dorg-test-*' and `dorg-perf-*' are both
;;     covered by the exhaustive `make test-full' target)
;;
;; Each check fails with an actionable message naming the missing
;; symbol/file AND the map/list to update.  Adding a module or test
;; without updating the durable maps turns these red.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'dorg)
(require 'dorg-test-gates)

;;; ─── Paths & fixtures ─────────────────────────────────────────────────

(defconst dorg-test-project--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

(defconst dorg-test-project--tests-dir
  (expand-file-name "tests" dorg-test-project--root)
  "Absolute path to the tests/ directory.")

;; Root `dorg-*.el' files that `dorg-reload' intentionally never reloads
;; into an interactive session.  `dorg-app.el' is the Clime CLI app:
;; keeping it out of `dorg.el' (and out of reload discovery) is what lets
;; `(require 'dorg)' stay Clime-free (see
;; `dorg-test-cli-core-loads-without-clime').
(defconst dorg-test-project--non-module-files '(dorg-app)
  "Root `dorg-*.el' symbols `dorg-reload' deliberately excludes.")

(defconst dorg-test-project--retired-reconciliation-modules
  '(dorg-diff
    dorg-merge
    dorg-sync
    dorg-sync-state
    dorg-sync-snapshot
    dorg-sync-core
    dorg-sync-db
    dorg-sync-buffer
    dorg-sync-local
    dorg-sync-finalize
    dorg-sync-async
    dorg-conflict
    dorg-conflict-serialize
    dorg-conflict-store
    dorg-conflict-lifecycle
    dorg-conflict-view
    dorg-conflict-block
    dorg-conflict-resolve)
  "Legacy reconciliation modules removed by tracked-case.")

(defun dorg-test-project--file-contents (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun dorg-test-project--root-module-symbols ()
  "Return symbols for every root `dorg*.el' file (sans extension)."
  (mapcar (lambda (f) (intern (file-name-sans-extension f)))
          (directory-files dorg-test-project--root nil "^dorg.*\\.el\\'")))

(defun dorg-test-project--test-file-symbols ()
  "Return symbols for every `tests/dorg-*-tests.el' file (sans extension).
This is the canonical \"one test file per module\" set; support files
such as the runner, helpers, and stress harness are excluded by design."
  (mapcar (lambda (f) (intern (file-name-sans-extension f)))
          (directory-files dorg-test-project--tests-dir nil
                           "^dorg-.*-tests\\.el\\'")))

(defun dorg-test-project--requires (path prefix-re)
  "Return symbols `require'd in PATH whose names match PREFIX-RE."
  (let ((text (dorg-test-project--file-contents path))
        (start 0)
        out)
    (while (string-match "^[ \t]*(require '\\(dorg[a-z0-9-]*\\))" text start)
      (let ((sym (match-string 1 text)))
        (when (string-match-p prefix-re sym)
          (push (intern sym) out)))
      (setq start (match-end 0)))
    (nreverse (delete-dups out))))

(defun dorg-test-project--doc-example-block (heading)
  "Return body lines of the first #+begin_example block under Org HEADING.
HEADING is matched as an Org headline in DEVELOPMENT.org."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "DEVELOPMENT.org" dorg-test-project--root))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (unless (re-search-forward
               (concat "^\\(\\*+\\) +" (regexp-quote heading) "[ \t]*$") nil t)
        (error "DEVELOPMENT.org: heading %S not found" heading))
      (let* ((level (length (match-string 1)))
             (limit (save-excursion
                      (if (re-search-forward
                           (format "^\\*\\{1,%d\\}[ \t]" level) nil t)
                          (match-beginning 0)
                        (point-max)))))
        (unless (re-search-forward
                 "^[ \t]*#\\+begin_example\\(?:[ \t]+.*\\)?$" limit t)
          (error "DEVELOPMENT.org: no #+begin_example after %S" heading))
        (let ((start (line-beginning-position 2)))
          (unless (re-search-forward "^[ \t]*#\\+end_example[ \t]*$" limit t)
            (error "DEVELOPMENT.org: unterminated example block after %S" heading))
          (split-string
           (buffer-substring-no-properties start (line-beginning-position))
           "\n" t))))))

(defun dorg-test-project--documented-el-symbols (heading)
  "Return symbols for every `dorg*.el' token in HEADING's example block."
  (let (out)
    (dolist (line (dorg-test-project--doc-example-block heading))
      (let ((start 0))
        (while (string-match "\\(dorg\\(?:-[a-z0-9-]+\\)?\\)\\.el\\b" line start)
          (push (intern (match-string 1 line)) out)
          (setq start (match-end 0)))))
    (delete-dups (nreverse out))))

(defun dorg-test-project--with-doc-fixture (text body)
  "Write TEXT as a temporary DEVELOPMENT.org, then call BODY in its root."
  (let ((dorg-test-project--root (make-temp-file "dorg-project-doc-map-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "DEVELOPMENT.org"
                                            dorg-test-project--root)
            (insert text))
          (funcall body))
      (delete-directory dorg-test-project--root t))))

(ert-deftest dorg-test-project-doc-example-labelled-opener ()
  "A language-labelled Org example block supplies the documented map."
  (dorg-test-project--with-doc-fixture
   "* Modules\n#+BEGIN_EXAMPLE text -n\ndorg-example-module.el\n#+END_EXAMPLE\n"
   (lambda ()
     (should (equal '("dorg-example-module.el")
                    (dorg-test-project--doc-example-block "Modules"))))))

(ert-deftest dorg-test-project-doc-example-bare-opener ()
  "The original bare Org example opener remains valid."
  (dorg-test-project--with-doc-fixture
   "* Modules\n#+begin_example\ndorg-example-module.el\n#+end_example\n"
   (lambda ()
     (should (equal '("dorg-example-module.el")
                    (dorg-test-project--doc-example-block "Modules"))))))

(ert-deftest dorg-test-project-doc-example-absent-in-section ()
  "A later heading's block cannot satisfy a missing Modules block."
  (dorg-test-project--with-doc-fixture
   "* Modules\nNo block here.\n* Tests\n#+begin_example\ndorg-example-module.el\n#+end_example\n"
   (lambda ()
     (should (equal '(error "DEVELOPMENT.org: no #+begin_example after \"Modules\"")
                    (should-error (dorg-test-project--doc-example-block "Modules")))))))

(ert-deftest dorg-test-project-doc-example-rejects-lookalikes ()
  "Similar Org block names do not count as an example opener."
  (dolist (opener '("#+begin_examples" "#+begin_example_x"))
    (dorg-test-project--with-doc-fixture
     (format "* Modules\n%s\ndorg-example-module.el\n#+end_example\n" opener)
     (lambda ()
       (should (equal '(error "DEVELOPMENT.org: no #+begin_example after \"Modules\"")
                      (should-error (dorg-test-project--doc-example-block "Modules"))))))))

(ert-deftest dorg-test-project-doc-example-rejects-unterminated-block ()
  "A block with no terminator in its section is malformed."
  (dorg-test-project--with-doc-fixture
   "* Modules\n#+begin_example\ndorg-example-module.el\n* Tests\n#+end_example\n"
   (lambda ()
     (should (equal '(error "DEVELOPMENT.org: unterminated example block after \"Modules\"")
                    (should-error (dorg-test-project--doc-example-block "Modules")))))))

(ert-deftest dorg-test-project-doc-example-missing-module-still-fails ()
  "A parsed block cannot hide an undocumented root module."
  (dorg-test-project--with-doc-fixture
   "* Modules\n#+begin_example text\ndorg-example-module.el\n#+end_example\n"
   (lambda ()
     (with-temp-file (expand-file-name "dorg-extra-module.el" dorg-test-project--root)
       (insert ";;; fixture\n"))
     (should-error
      (funcall (ert-test-body (ert-get-test 'dorg-test-project-modules-documented)))
      :type 'ert-test-failed))))

(defun dorg-test-project--fmt (syms)
  "Format SYMS as a comma-separated name list."
  (mapconcat #'symbol-name syms ", "))

(defun dorg-test-project--struct-macro-definer-p (sym)
  "Return non-nil when root module SYM defines a `cl-defstruct'/`defmacro'.
Scans the module's source for a top-level (column-0) struct or macro
definition — the load-order-sensitive forms whose stale expansion
`dorg--reload-priority' exists to prevent."
  (let ((text (dorg-test-project--file-contents
               (expand-file-name (format "%s.el" sym)
                                  dorg-test-project--root))))
    (string-match-p "^(\\(?:cl-defstruct\\|defmacro\\|cl-defmacro\\)[ \t\n(]"
                    text)))

;;; ─── Self-discovery contract ↔ filesystem ─────────────────────────────

(ert-deftest dorg-test-project-reload-discovers-all-loaded-modules ()
  "`dorg--reload-discover' == loaded root `dorg-*.el' modules minus `dorg-app'.
The self-discovery contract that replaces the old hand-maintained
`dorg--modules' list: a newly-added module is reloaded with no list edit,
and the CLI app is never dragged in.  Drift guard — if discovery scanned
the wrong directory/glob or dropped the `dorg-app' exclusion, the two
independently-computed sets diverge."
  (let* ((files (dorg-test-project--root-module-symbols))
         (retired (cl-intersection
                   files dorg-test-project--retired-reconciliation-modules))
         (expected (cl-remove-if-not
                    (lambda (s)
                      (and (not (memq s dorg-test-project--non-module-files))
                           (featurep s)))
                    files))
         (discovered (mapcar (lambda (p) (intern (file-name-base p)))
                             (dorg--reload-discover
                              dorg-test-project--root)))
         (missing (cl-set-difference expected discovered))
         (extra   (cl-set-difference discovered expected)))
    (when retired
      (ert-fail
       (format "Retired reconciliation modules still exist: %s. Delete the files; do not hide them from reload discovery."
               (dorg-test-project--fmt retired))))
    (should-not
     (file-directory-p
      (expand-file-name "tools/conflict-fixture" dorg-test-project--root)))
    (when missing
      (ert-fail
       (format "Loaded root modules `dorg--reload-discover' missed: %s."
               (dorg-test-project--fmt missing))))
    (when extra
      (ert-fail
       (format "`dorg--reload-discover' returned non-loaded/excluded modules: %s."
               (dorg-test-project--fmt extra))))))

(ert-deftest dorg-test-project-reload-requires-are-discovered ()
  "Every `(require 'dorg-*)' in dorg.el is reloaded by `dorg-reload'.
Requiring a module makes it `featurep', so self-discovery must reload it
(no direct require may be silently skipped).  Reverse is NOT asserted:
dorg.el relies on transitive requires for some split modules, so the
direct block is a subset of the discovered set."
  (let* ((files (dorg-test-project--root-module-symbols))
         (reqs (dorg-test-project--requires
                (expand-file-name "dorg.el" dorg-test-project--root)
                "\\`dorg-"))
         (discovered (mapcar (lambda (p) (intern (file-name-base p)))
                             (dorg--reload-discover
                              dorg-test-project--root)))
         (no-file (cl-set-difference reqs files))
         (not-discovered (cl-set-difference reqs discovered)))
    (when no-file
      (ert-fail
       (format "dorg.el requires modules with no file: %s."
               (dorg-test-project--fmt no-file))))
    (when not-discovered
      (ert-fail
       (format "dorg.el directly requires %s but `dorg-reload' does not discover them (CLI app or stale require?)."
               (dorg-test-project--fmt not-discovered))))))

(ert-deftest dorg-test-project-top-level-load-excludes-internal-adapters ()
  "`(require \='dorg)' must not drag in the opt-in/internal adapters.
`dorg-git', `dorg-git-sync', and `dorg-envelope' are classified internal in
docs/api.org and each has exactly one real consumer that requires it itself:
`dorg-git-sync' requires `dorg-git', `dorg-app' requires `dorg-envelope', and
the one downstream consumer of the sync facade requires `dorg-git-sync'
itself, lazily, before touching it.  A top-level require of any of them puts
git and envelope machinery in every plain `dorg' session that will never use
it.

This CANNOT be asserted in-process: the test runner loads
`dorg-git-tests', `dorg-git-sync-tests', and `dorg-envelope-tests', each of
which requires the module under assertion, so `featurep' is t here no matter
what dorg.el says.  A genuinely fresh `--batch -Q' Emacs that loads only
`dorg' is the only observer that can tell the two worlds apart, and it goes
RED the moment a require is restored.  The same subprocess also proves the
modules are still REACHABLE on demand, so this asserts a load boundary, not
a removal."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (expr "(progn
                  (require \'dorg)
                  (princ (format \"eager:%s/%s/%s\"
                                 (and (featurep \'dorg-git) t)
                                 (and (featurep \'dorg-git-sync) t)
                                 (and (featurep \'dorg-envelope) t)))
                  (require \'dorg-git-sync)
                  (require \'dorg-envelope)
                  (princ (format \" ondemand:%s/%s/%s\"
                                 (and (featurep \'dorg-git) t)
                                 (and (featurep \'dorg-git-sync) t)
                                 (and (featurep \'dorg-envelope) t))))")
         (out (with-output-to-string
                (with-current-buffer standard-output
                  (should (= 0 (call-process emacs nil t nil "--batch" "-Q"
                                             "-L" dorg-test-project--root
                                             "--eval" expr)))))))
    ;; Nothing eager; everything reachable, and `dorg-git' arrives
    ;; transitively through `dorg-git-sync' exactly as designed.
    (should (string-match-p "eager:nil/nil/nil" out))
    (should (string-match-p "ondemand:t/t/t" out))))

;;; ─── Reload priority seed ↔ filesystem ────────────────────────────────

(ert-deftest dorg-test-project-reload-priority-seed-has-files ()
  "Every `dorg--reload-priority' seed member has a root `dorg-*.el' file.
Guards a stale seed entry left behind after a module rename/removal."
  (let* ((files (dorg-test-project--root-module-symbols))
         (orphan (cl-set-difference dorg--reload-priority files)))
    (when orphan
      (ert-fail
       (format "`dorg--reload-priority' lists symbols with no dorg-*.el file: %s. Remove them or restore the files."
               (dorg-test-project--fmt orphan))))))

(ert-deftest dorg-test-project-reload-priority-covers-struct-macro-definers ()
  "Every struct/macro-defining root module is in `dorg--reload-priority'.
Completeness drift guard (self-validating): the seed exists precisely so
that every module defining a `cl-defstruct'/`defmacro' reloads BEFORE its
dependents, so none is left holding a stale macro expansion.  A new struct
or macro added to a non-seed module turns this red — the failure mode a
hand-maintained list silently allowed."
  (let* ((files (dorg-test-project--root-module-symbols))
         (candidates (cl-set-difference
                      files dorg-test-project--non-module-files))
         (definers (cl-remove-if-not
                    #'dorg-test-project--struct-macro-definer-p candidates))
         (uncovered (cl-set-difference definers dorg--reload-priority)))
    ;; Sanity: the scan actually finds the known definers (non-vacuity).
    (should (memq 'dorg-model definers))
    (should (memq 'dorg-ext definers))
    (when uncovered
      (ert-fail
       (format "Struct/macro-defining modules absent from `dorg--reload-priority' in dorg.el: %s. Add them in dependency order so reload never leaves a dependent with a stale macro."
               (dorg-test-project--fmt uncovered))))))

;;; ─── Test runner ↔ test files ─────────────────────────────────────────

(ert-deftest dorg-test-project-runner-matches-test-files ()
  "`tests/dorg-tests-runner.el' requires exactly the `dorg-*-tests.el' files."
  (let* ((files (dorg-test-project--test-file-symbols))
         (reqs (dorg-test-project--requires
                (expand-file-name "dorg-tests-runner.el"
                                  dorg-test-project--tests-dir)
                "-tests\\'"))
         (unrequired (cl-set-difference files reqs))
         (no-file (cl-set-difference reqs files)))
    (when unrequired
      (ert-fail
       (format "Test files not required by dorg-tests-runner.el: %s. Add (require '...) so `make test-full' can select them."
               (dorg-test-project--fmt unrequired))))
    (when no-file
      (ert-fail
       (format "dorg-tests-runner.el requires test features with no file: %s."
               (dorg-test-project--fmt no-file))))))

;;; ─── Documented maps (DEVELOPMENT.org) ────────────────────────────────

(ert-deftest dorg-test-project-modules-documented ()
  "Every root `dorg-*.el' module is listed in the DEVELOPMENT.org Modules map."
  (let* ((files (dorg-test-project--root-module-symbols))
         (documented (dorg-test-project--documented-el-symbols "Modules"))
         (undocumented (cl-set-difference files documented)))
    (when undocumented
      (ert-fail
       (format "Modules missing from the DEVELOPMENT.org `** Modules' map: %s. Add a one-line entry for each."
               (dorg-test-project--fmt undocumented))))))

(ert-deftest dorg-test-project-test-files-documented ()
  "Every `tests/dorg-*-tests.el' file is listed in the DEVELOPMENT.org Tests map."
  (let* ((files (dorg-test-project--test-file-symbols))
         (documented (dorg-test-project--documented-el-symbols "Tests"))
         (undocumented (cl-set-difference files documented)))
    (when undocumented
      (ert-fail
       (format "Test files missing from the DEVELOPMENT.org `** Tests' map: %s. Add a one-line entry for each."
               (dorg-test-project--fmt undocumented))))))

;; (Removed `dorg-test-project-agents-delegates-map': the top-level
;; AGENTS.md it guarded is intentionally removed from the public tree for
;; the 0.1.0 release, so the delegation invariant no longer applies.)

;;; ─── Public API census artifact ──────────────────────────────────────

(defconst dorg-test-project--api-census-required-symbols
  '("dorg-sql"
    "dorg-sql-batch"
    "dorg-sql-quote"
    "dorg-materialize"
    "dorg-materialized-view-scope"
    "dorg-render"
    "dorg-render-island"
    "dorg-model-load-headings"
    "dorg-model-load-tree"
    "dorg-head-commit"
    "dorg-register-extension"
    "dorg-ext-read-context-root-id"
    "dorg-registered-backend"
    "dorg-registered-dolt-dir"
    "dorg-bind-dolt"
    "dorg-bind-existing-dolt")
  "Public symbols that must be rows in docs/api-census.org.
This is the pre-internalization caller-census guard for the risky API
families named by tracked-case.")

(defconst dorg-test-project--api-census-demoted-symbols
  (append
   (mapcar (lambda (suffix)
             (concat "dorg-model-render-" suffix))
           '("heading" "item"))
   (mapcar (lambda (suffix)
             (concat "dorg-git-" suffix))
           '("run" "ok" "repo-p" "common-dir" "current-branch"
             "branch-exists-p"))
   (list "dorg-ext-read-cache-key"
         "dorg-ext-read-context-p"
         "dorg-ext-read-cache-known-head"
         "dorg-ext-read-cache-fresh"
         "dorg-ext-placement-context-plist"
         "dorg-backend-supports-p"
         "dorg-backend-capabilities")
   (list (concat "dorg-ext-read-" "project"))
   (mapcar (lambda (suffix)
             (concat "dorg-dolt-server-" suffix))
           '("start" "start-bootstrap" "ensure" "ensure-strict"
             "ensure-detached" "strict-port" "stop")))
  "Symbols demoted from public API that must stay out of docs/api.org.")

(defun dorg-test-project--api-census-row-p (text symbol)
  "Return non-nil when TEXT has an org-table row whose first cell is SYMBOL."
  (string-match-p
   (format "^|[ \t]*[=~]%s[=~][ \t]*|" (regexp-quote symbol))
   text))

(ert-deftest dorg-test-project-api-census-covers-risky-public-symbols ()
  "docs/api-census.org contains row-level coverage for risky public APIs.
This is intentionally a filesystem-only artifact guard: it does not
verify the full census mechanically, but it makes the API cleanup gate
fail if the durable table omits the known cross-repo coupling classes."
  (let ((path (expand-file-name "docs/api-census.org"
                                dorg-test-project--root)))
    (should (file-exists-p path))
    (let ((text (dorg-test-project--file-contents path))
          missing)
      (should (string-match-p "|[ \t]*Symbol[ \t]*|" text))
      (dolist (symbol dorg-test-project--api-census-required-symbols)
        (unless (dorg-test-project--api-census-row-p text symbol)
          (push symbol missing)))
      (when missing
        (ert-fail
         (format "docs/api-census.org missing required API census rows: %s"
                 (mapconcat #'identity (nreverse missing) ", ")))))))

(ert-deftest dorg-test-project-api-docs-exclude-demoted-symbols ()
  "docs/api.org does not list demoted plumbing helpers as public API."
  (let ((text (dorg-test-project--file-contents
               (expand-file-name "docs/api.org" dorg-test-project--root)))
        offenders)
    (dolist (symbol dorg-test-project--api-census-demoted-symbols)
      (when (string-match-p
             (format "\\_<%s\\_>" (regexp-quote symbol))
             text)
        (push symbol offenders)))
    (when offenders
      (ert-fail
       (format "docs/api.org still documents demoted API symbols: %s"
               (mapconcat #'identity (nreverse offenders) ", "))))))

(ert-deftest dorg-test-project-docs-state-external-dolt-sql-boundary ()
  "Operator docs state the proved external Dolt SQL boundary narrowly."
  (let ((text (dorg-test-project--file-contents
               (expand-file-name "docs/cli.org" dorg-test-project--root)))
        (case-fold-search t))
    (should (string-match-p "external.*dolt sql\\|dolt sql.*external" text))
    (should (string-match-p "target selection" text))
    (should (string-match-p "health recovery" text))
    (should (string-match-p "bounded guarantees\\|timeout" text))
    (should (string-match-p "does not prove" text))
    (should (string-match-p "native server" text))
    (should (string-match-p "discovery" text))
    (should (string-match-p "dorg sql" text))))

;;; ─── ERT selector policy ──────────────────────────────────────────────

(ert-deftest dorg-test-project-ert-selector-policy ()
  "Every ERT test is named `dorg-test-*' or `dorg-perf-*'.
The exhaustive `make test-full' gate covers both namespaces.  `make test'
is a bounded representative, while `make perf' remains a focused benchmark
target.  Any other prefix would be silently dropped from the full suite."
  (let (offenders)
    (dolist (file (directory-files dorg-test-project--tests-dir t "\\.el\\'"))
      (let ((text (dorg-test-project--file-contents file))
            (start 0))
        (while (string-match "^(ert-deftest +\\([^ \t\n()]+\\)" text start)
          (let ((name (match-string 1 text)))
            (unless (string-match-p "\\`dorg-\\(?:test\\|perf\\)-" name)
              (push (format "%s (%s)" name (file-name-nondirectory file))
                    offenders)))
          (setq start (match-end 0)))))
    (when offenders
      (ert-fail
       (format "ert-deftest names outside the dorg-test-*/dorg-perf-* policy: %s. Rename to dorg-test-* (or dorg-perf-* for benchmarks)."
               (mapconcat #'identity (nreverse offenders) ", "))))))

;;; ─── Test gate partition (fast / medium / representative / full) ──────
;;
;; `dorg-test-gates.el' splits the regular `dorg-test-*' suite into a quick
;; Dolt-free FAST gate and a Dolt/integration MEDIUM gate.  The default
;; REPRESENTATIVE gate combines FAST with a small, named integration-file
;; allowlist; TEST-FULL is the exhaustive acceptance gate and also covers
;; `dorg-perf-*'.  These guards prove the convenience gates cannot silently
;; replace or shrink full coverage.

(defun dorg-test-project--makefile-text ()
  "Return the repository Makefile contents as a string."
  (dorg-test-project--file-contents
   (expand-file-name "Makefile" dorg-test-project--root)))

(ert-deftest dorg-test-project-gate-partition-exhaustive-and-disjoint ()
  "FAST ∪ MEDIUM == every `^dorg-test-' test, and FAST ∩ MEDIUM == ∅.
Computed from the ACTUAL gate selector forms the Make targets run, so a
classification or selector bug that dropped or double-counted tests turns
this red — the core guarantee that the gates never silently lose coverage."
  (let* ((all (mapcar #'ert-test-name
                      (ert-select-tests dorg-test-gate-regular-selector t)))
         (fast (mapcar #'ert-test-name
                       (ert-select-tests (dorg-test-gate-selector 'fast) t)))
         (medium (mapcar #'ert-test-name
                         (ert-select-tests (dorg-test-gate-selector 'medium) t)))
         (overlap (cl-intersection fast medium))
         (union (cl-union fast medium))
         (uncovered (cl-set-difference all union))
         (extra (cl-set-difference union all)))
    (should-not overlap)                ; disjoint
    (should-not uncovered)              ; exhaustive: nothing in full missing
    (should-not extra)                  ; no phantom outside full
    (should (= (length all) (+ (length fast) (length medium))))))

(defun dorg-test-project--owned-ert-tests ()
  "Return ERT tests whose defining file is in this project's tests directory.
This uses `symbol-file' rather than a test-name prefix so macro-generated
tests remain covered and a future project-owned test cannot be omitted by a
namespace-only selector."
  (let ((tests-dir (file-truename dorg-test-project--tests-dir)))
    (seq-filter
     (lambda (test)
       (let ((file (symbol-file (ert-test-name test) 'ert--test)))
         (and file
              (file-in-directory-p (file-truename file) tests-dir))))
     (ert-select-tests t t))))

(ert-deftest dorg-test-project-gate-full-covers-project-test-inventory ()
  "The full gate selects every registered project-owned ERT test.
The assertion is inventory-based, not prefix-based: all ERT definitions in
`tests/' — including macro-generated tests and `dorg-perf-*' benchmarks —
must be selected by the exact FULL selector."
  (let ((owned (sort (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                             (dorg-test-project--owned-ert-tests))
                     #'string-lessp))
        (full (sort (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                            (ert-select-tests
                             (dorg-test-gate-selector 'full) t))
                    #'string-lessp)))
    (should (equal owned full))))

(ert-deftest dorg-test-project-gate-representative-is-strict-full-subset ()
  "The default representative gate is non-empty, bounded, and covered by full."
  (let* ((representative
          (mapcar #'ert-test-name
                  (ert-select-tests (dorg-test-gate-selector 'representative) t)))
         (fast (mapcar #'ert-test-name
                       (ert-select-tests (dorg-test-gate-selector 'fast) t)))
         (full (mapcar #'ert-test-name
                       (ert-select-tests (dorg-test-gate-selector 'full) t)))
         (uncovered (cl-set-difference representative full))
         (missing-fast (cl-set-difference fast representative)))
    (should representative)
    (should-not uncovered)
    (should-not missing-fast)
    (should (< (length representative) (length full)))))

(ert-deftest dorg-test-project-gate-representative-files-exist-and-define-tests ()
  "Every named representative integration file exists and contributes tests.
The default's integration component is a finite file allowlist.  A rename or
stale entry must turn this red instead of silently shrinking the default."
  (dolist (base dorg-test-gate-representative-dolt-files)
    (let ((file (expand-file-name (concat base ".el")
                                  dorg-test-project--tests-dir)))
      (should (file-exists-p file))
      (should
       (ert-select-tests
        `(and ,dorg-test-gate-regular-selector
              (satisfies
               ,(lambda (test)
                  (equal (dorg-test-gate--test-file-base test) base))))
        t)))))

(ert-deftest dorg-test-project-gate-proper-non-empty-subsets ()
  "Neither FAST nor MEDIUM is empty or equal to the full suite.
A gate that matched nothing (useless) or everything (a silent alias of the
full acceptance suite) would defeat the point; pin both bounds."
  (let ((all (length (ert-select-tests dorg-test-gate-regular-selector t)))
        (fast (length (ert-select-tests (dorg-test-gate-selector 'fast) t)))
        (medium (length (ert-select-tests (dorg-test-gate-selector 'medium) t))))
    (should (> fast 0))
    (should (> medium 0))
    (should (< fast all))
    (should (< medium all))))

(ert-deftest dorg-test-project-gate-fast-files-exist-and-define-tests ()
  "Every FAST-allowlisted file exists and defines >=1 `dorg-test-*' test.
Guards a dead/rotted allowlist entry left after a test-file rename."
  (dolist (base dorg-test-gate-fast-files)
    (let ((file (expand-file-name (concat base ".el")
                                  dorg-test-project--tests-dir)))
      (should (file-exists-p file))
      (let ((names (mapcar #'ert-test-name
                           (ert-select-tests
                            `(satisfies
                              ,(lambda (test)
                                 (equal (dorg-test-gate--test-file-base test)
                                        base)))
                            t))))
        (when (null names)
          (ert-fail (format "FAST file %s defines no dorg-test-* tests (stale allowlist entry?)."
                            base)))))))

(ert-deftest dorg-test-project-gate-fast-files-are-dolt-free ()
  "Every FAST-allowlisted file is statically free of dolt/sql-server use.
Enforces the classification PRINCIPLE in code: the FAST gate skips even
starting the shared sql-server, so admitting a file that needs the dolt
binary or a real DB/server fixture would make the quick gate fail or
silently depend on Dolt.  A file gaining a Dolt fixture must move to MEDIUM
\(drop it from the allowlist), and this turns red until it does."
  (let ((dolt-marker
         (concat "dorg-test--dolt-available\\|(dorg-sql \\|(dorg-sql-batch\\|"
                 "with-temp-db-and-dir\\|dorg-test--server-port\\|"
                 "dorg-test--start-shared\\|dorg-test--copy-repo"))
        ;; This very file defines the marker vocabulary above (as a regexp
        ;; literal), so it would self-match; skip it.  It is the policy file
        ;; itself — pure filesystem checks, requiring no dolt/sql module — so
        ;; its FAST membership needs no scan.
        (self "dorg-project-tests")
        offenders)
    (dolist (base dorg-test-gate-fast-files)
      (let* ((file (expand-file-name (concat base ".el")
                                     dorg-test-project--tests-dir))
             (text (and (not (equal base self))
                        (file-exists-p file)
                        (dorg-test-project--file-contents file))))
        (when (and text (string-match-p dolt-marker text))
          (push base offenders))))
    (when offenders
      (ert-fail
       (format "FAST-gate files use dolt/sql-server fixtures (move to MEDIUM by dropping from `dorg-test-gate-fast-files'): %s."
               (mapconcat #'identity (nreverse offenders) ", "))))))

(ert-deftest dorg-test-project-gate-spot-classification ()
  "Non-vacuity: a known pure suite is FAST; a known Dolt suite is MEDIUM.
If attribution silently returned nil for every test (e.g. `symbol-file'
broke), both the exhaustive and subset guards could still pass with a
degenerate split; these concrete anchors prevent that."
  (let ((pure (car (ert-select-tests "^dorg-test-timestamp-" t)))
        (dolt (car (ert-select-tests "^dorg-test-transport-strict-server-query" t))))
    (should pure)
    (should dolt)
    (should (dorg-test-gate-fast-p pure))
    (should-not (dorg-test-gate-fast-p dolt))))

(ert-deftest dorg-test-project-gate-make-targets-preserve-coverage ()
  "Makefile defaults to representative and keeps a separately exhaustive full.
Focused `make test SELECT=...' calls remain arbitrary selectors.  The full
target must run the FULL gate and enable the existing benchmark fixtures;
FAST and MEDIUM retain their current gate routes."
  (let ((mk (dorg-test-project--makefile-text)))
    (should (string-match-p "^test:" mk))
    (should (string-match-p "dorg-run-tests-batch-default-or-selector" mk))
    (should (string-match-p "^test-full:" mk))
    (should (string-match-p
             "^DEFAULT_TEST_ENV[ \t]*=[ \t]*env -u DORG_BENCH"
             mk))
    (should (string-match-p
             "^FOCUSED_TEST_ENV[ \t]*=[ \t]*DORG_TEST_VERBOSE"
             mk))
    (should (string-match-p
             "^TEST_FULL_ENV[ \t]*=[ \t]*DORG_BENCH=1"
             mk))
    (should (string-match-p "DORG_BENCH=1" mk))
    (should (string-match-p "Capability-dependent ERT skips remain reported" mk))
    (should (string-match-p "dorg-run-tests-batch-gate \\\"full\\\"" mk))
    (should (string-match-p "test-fast.*no sql-server" mk))
    (should (string-match-p "test-medium.*starts shared sql-server" mk))
    (should (string-match-p "test-full.*starts shared sql-server" mk))
    ;; Existing narrow gates remain declared and gate-routed.
    (should (string-match-p "^test-fast:" mk))
    (should (string-match-p "^test-medium:" mk))
    (should (string-match-p "dorg-run-tests-batch-gate \"fast\"" mk))
    (should (string-match-p "dorg-run-tests-batch-gate \"medium\"" mk))
    ;; .PHONY lists each target so it cannot collide with a file of that name.
    (should (string-match-p "test-full" mk))
    (should (string-match-p "test-fast" mk))
    (should (string-match-p "test-medium" mk))
    (should (string-match-p
             "shared server not started (fast gate)"
             (dorg-test-project--file-contents
              (expand-file-name "dorg-tests-runner.el"
                                dorg-test-project--tests-dir))))))

;;; ─── Install story: targets + public-tree scrub ───────────────────────
;; End-user install surface: `make install' / `make install-deps' and a hosted
;; `install.sh'.  Both are USER-FACING, so the de-internalize public-tree scrub
;; applies: no internal tracker/coordination tokens may leak.

(ert-deftest dorg-test-project-install-targets-exist ()
  "The Makefile exposes `install' and `install-deps' targets (the end-user
install story)."
  (let ((mk (dorg-test-project--makefile-text)))
    (should (string-match-p "^install-deps:" mk))
    (should (string-match-p "^install:" mk))))

(ert-deftest dorg-test-project-install-sh-exists ()
  "A hosted `install.sh' exists at the repo root for `curl … | bash'."
  (should (file-exists-p (expand-file-name "install.sh" dorg-test-project--root))))

(ert-deftest dorg-test-project-install-sh-static-contract ()
  "install.sh pins its public `curl | bash' contract by static inspection:
it parses under `bash -n', uses strict mode, invokes `make install' (whose
staged `dorg doctor --install' is the installation verification, so the script
never re-runs doctor on the published launcher), and honors the documented
configurable inputs."
  (let ((path (expand-file-name "install.sh" dorg-test-project--root)))
    (should (file-exists-p path))
    (when (executable-find "bash")
      (should (= 0 (call-process "bash" nil nil nil "-n" path))))
    (let ((text (dorg-test-project--file-contents path)))
      (should (string-match-p "set -euo pipefail" text))
      (should (string-match-p "make .*install" text))
      (should (string-match-p "doctor --install" text))
      (should-not (string-match-p "BINDIR/dorg\"? doctor" text))
      (dolist (var '("DORG_REPO" "DORG_REF" "DORG_DIR" "BINDIR" "PREFIX"))
        (should (string-match-p var text)))
      ;; The launcher defaults to ~/bin like `make install', and the
      ;; script forwards both layout roots to it.
      (should (string-match-p (regexp-quote "BINDIR=\"${BINDIR:-$HOME/bin}\"") text))
      (should (string-match-p "make .*install .*PREFIX=" text))
      (should (string-match-p "make .*install .*BINDIR=" text)))))

;;; ─── Relocated install: static contract ───────────────────────────────
;; `make install' copies an explicit runtime inventory into an installation
;; tree and compiles it there (behaviour: tests/dorg-install-tests.el).

(defconst dorg-test-project--install-excluded-modules
  '("dorg-app.el" "dorg-test-job.el")
  "Root `dorg*.el' files the install never copies as modules.
`dorg-app.el' ships as the body of the committed wrapper; `dorg-test-job.el'
is test-only.")

(defun dorg-test-project--install-runtime-modules (makefile-text)
  "Return the `DORG_RUNTIME_MODULES' entries parsed from MAKEFILE-TEXT."
  (with-temp-buffer
    (insert makefile-text)
    (goto-char (point-min))
    (when (re-search-forward "^DORG_RUNTIME_MODULES[ \t]*:?=" nil t)
      (let ((start (point)))
        ;; The value runs through every backslash-continued line.
        (while (progn (end-of-line)
                      (and (eq (char-before) ?\\) (not (eobp))))
          (forward-line 1))
        (split-string (replace-regexp-in-string
                       "\\\\$" "" (buffer-substring-no-properties start (point)))
                      "[ \t\n\\\\]+" t)))))

(defun dorg-test-project--install-inventory-problems (listed actual)
  "Return a description of LISTED modules that differ from ACTUAL, or nil."
  (let ((missing (cl-set-difference actual listed :test #'equal))
        (extra (cl-set-difference listed actual :test #'equal))
        (dups (cl-remove-duplicates
               (cl-remove-if (lambda (m) (= 1 (cl-count m listed :test #'equal)))
                             listed)
               :test #'equal)))
    (when (or missing extra dups)
      (format "DORG_RUNTIME_MODULES in Makefile: missing %S, extra %S, duplicated %S"
              missing extra dups))))

(ert-deftest dorg-test-project-install-runtime-inventory-is-exact ()
  "The Makefile's install inventory is every root `dorg*.el' module except
the wrapper body and the test-only module.  The negative controls prove the
predicate rejects a dropped module and an admitted test-only file."
  (let* ((listed (dorg-test-project--install-runtime-modules
                  (dorg-test-project--makefile-text)))
         (actual (cl-set-difference
                  (directory-files dorg-test-project--root nil "\\`dorg.*\\.el\\'")
                  dorg-test-project--install-excluded-modules :test #'equal)))
    (should (> (length actual) 40))
    (should-not (dorg-test-project--install-inventory-problems listed actual))
    (ert-info ("negative control: a dropped module is reported")
      (should (dorg-test-project--install-inventory-problems (cdr listed) actual)))
    (ert-info ("negative control: an admitted test-only module is reported")
      (should (dorg-test-project--install-inventory-problems
               (cons "dorg-test-job.el" listed) actual)))))

(ert-deftest dorg-test-project-install-target-has-no-checkout-compile ()
  "`install' does not byte-compile the checkout: no `compile' or
`install-deps' prerequisite, so the source tree gains no `.elc'."
  (let ((mk (dorg-test-project--makefile-text)))
    (should (string-match "^install:\\(.*\\)$" mk))
    (let ((prereqs (split-string (match-string 1 mk) "[ \t]+" t)))
      (should-not (member "compile" prereqs))
      (should-not (member "install-deps" prereqs)))))

(ert-deftest dorg-test-project-wrapper-body-matches-app ()
  "The committed wrapper is the two Clime header lines plus `dorg-app.el' verbatim.
`make install' refuses a stale wrapper with the same comparison."
  (let* ((wrapper (dorg-test-project--file-contents
                   (expand-file-name "bin/dorg" dorg-test-project--root)))
         (app (dorg-test-project--file-contents
               (expand-file-name "dorg-app.el" dorg-test-project--root)))
         (newline (string-search "\n" wrapper)))
    (should newline)
    (should (string-prefix-p "#!/bin/sh\n" wrapper))
    (should (equal app (substring wrapper (1+ (string-search "\n" wrapper
                                                              (1+ newline))))))))

(ert-deftest dorg-test-project-install-surface-scrub-clean ()
  "The install surface passes the shared public-export scrub predicate."
  (let ((known-symbols (dorg-test-project--known-dorg-token-table))
        offenders)
    (dorg-test-project--load-public-scrub-source)
    (dolist (rel '("install.sh" "Makefile"))
      (let ((path (expand-file-name rel dorg-test-project--root)))
        (when (file-exists-p path)
          (let ((text (dorg-test-project--file-contents path)))
            (dolist (class
                     (dorg-public-scrub-classify-string text known-symbols))
              (push (format "%s:%s" rel class) offenders))))))
    (when offenders
      (ert-fail (format "public-tree scrub: internal tokens in %s"
                        (mapconcat #'identity (nreverse offenders) ", "))))))

;;; ─── Release mechanics ────────────────────────────────────────────────

(ert-deftest dorg-test-project-version-has-one-authoritative-literal ()
  "The package header is the sole literal declaration of DOrg's version.
`dorg-version' must equal that header while root Elisp, the generated launcher,
and Makefile contain no independent version declaration.  In particular, a
literal clime `:version' is forbidden even when it abbreviates the package
version.  The occurrence and keyword censuses make both transformations
non-vacuous."
  (let* ((dorg-file (expand-file-name "dorg.el" dorg-test-project--root))
         (dorg-source (dorg-test-project--file-contents dorg-file))
         (header-version
          (and (string-match
                "^;; Version: \\([0-9]+\\(?:\\.[0-9]+\\)+\\)$"
                dorg-source)
               (match-string 1 dorg-source)))
         (files
          (append
           (directory-files dorg-test-project--root t "\\.el\\'")
           (list (expand-file-name "Makefile" dorg-test-project--root)
                 (expand-file-name "bin/dorg" dorg-test-project--root))))
         occurrences literal-version-keywords)
    (should (stringp header-version))
    (dolist (file files)
      (let ((source (dorg-test-project--file-contents file))
            (start 0))
        (while (string-match (regexp-quote header-version) source start)
          (push (cons (file-relative-name file dorg-test-project--root)
                      (match-beginning 0))
                occurrences)
          (setq start (match-end 0)))
        (setq start 0)
        (while (string-match ":version[ \t\n]+\\([\"']\\)" source start)
          (push (cons (file-relative-name file dorg-test-project--root)
                      (match-beginning 0))
                literal-version-keywords)
          (setq start (match-end 0)))))
    (should (equal dorg-version header-version))
    (should (= 1 (length occurrences)))
    (should (equal "dorg.el" (caar occurrences)))
    (ert-info ((format "literal :version declarations: %S"
                       (reverse literal-version-keywords)))
      (should-not literal-version-keywords))
    (ert-info ("negative control: a generated literal :version is rejected")
      (let ((planted (format "(clime-defapp probe (:version %S))"
                             header-version)))
        (should (string-match-p ":version[ \t\n]+\\([\"']\\)" planted))
        (should (string-match-p (regexp-quote header-version) planted))))))

(ert-deftest dorg-test-project-changelog-0.1.0-is-public-prose ()
  "CHANGELOG.org has a final 0.1.0 section with public-audience prose.
The release candidate reuses the existing Org changelog: no parallel Markdown
file, Pending heading, tracker id/vocabulary, private path, or unsupported
publication claim may survive in the version section."
  (let* ((org-path (expand-file-name "CHANGELOG.org"
                                     dorg-test-project--root))
         (md-path (expand-file-name "CHANGELOG.md"
                                    dorg-test-project--root))
         (text (dorg-test-project--file-contents org-path))
         (case-fold-search t)
         section
         violations)
    (when (file-exists-p md-path)
      (push "parallel CHANGELOG.md exists" violations))
    (when (string-match-p "^\\* +Pending\\b" text)
      (push "Pending heading remains" violations))
    (if (not (string-match "^\\* +0\\.1\\.0\\(?:[ 	].*\\)?$" text))
        (push "0.1.0 heading missing" violations)
      (let ((start (match-beginning 0)))
        (setq section
              (substring text start
                         (or (and (string-match "^\\* +" text (match-end 0))
                                  (match-beginning 0))
                             (length text))))))
    (when section
      (dolist (required
               '(("definition-only\\|unused internal\\|eager-load"
                  . "definition-only cleanup")
                 ("batch order\\|declared order"
                  . "declared batch order")
                 ("extension index[^\n]*rebuild\\|rebuild[^\n]*extension index"
                  . "extension-index rebuild")
                 ("subtree[^\n]*move\\|moved descendant"
                  . "subtree-move persistence")
                 ("earlier development stores\\(?:.\\|\n\\)*refused\\|pre-v6[^\n]*refused"
                  . "pre-v6 refusal")))
        (unless (string-match-p (car required) section)
          (push (format "missing %s prose" (cdr required)) violations)))
      (dolist (rule
               `(("\\bdorg-[[:alnum:]]\\{5\\}\\b" . "bead id")
                 (,(concat "\\(?:b" "d:\\|\\bem" "bead\\b\\|\\bem"
                            "bd\\b\\|\\bme" "ld\\b\\|\\bswarm\\b"
                            "\\|\\blane\\b\\)")
                  . "internal process vocabulary")
                 (,(concat "\\(?:~/\\|/Us" "ers/\\|\\.me" "ld/\\|\\.em"
                            "bead/\\)")
                  . "private path")
                 ("\\b\\(?:published\\|publicly available\\|released\\)\\b"
                  . "unsupported publication claim")
                 ("\\b\\(?:ERT\\|test suite\\|public-release-gate\\|receipt digest\\)\\b"
                  . "release-verification implementation detail")))
        (when (string-match-p (car rule) section)
          (push (cdr rule) violations))))
    (ert-info ("negative controls classify stale or private changelog prose")
      (dolist (control
               '(("Extension indexes remain available."
                  "extension index[^\n]*rebuild\\|rebuild[^\n]*extension index"
                  "extension-index rebuild")
                 ("Earlier development stores are supported."
                  "earlier development stores\\(?:.\\|\n\\)*refused\\|pre-v6[^\n]*refused"
                  "pre-v6 refusal")))
        (pcase-let ((`(,control-text ,rule ,class) control))
          (should
           (equal (format "missing %s prose" class)
                  (and (not (string-match-p rule control-text))
                       (format "missing %s prose" class))))))
      (let* ((control-text "The test suite passed with a receipt digest.")
             (rule
              "\\b\\(?:ERT\\|test suite\\|public-release-gate\\|receipt digest\\)\\b")
             (class (and (string-match-p rule control-text)
                         "release-verification implementation detail")))
        (should (equal "release-verification implementation detail" class))))
    (should-not violations)))

;;; ─── Public export and one-squash release proof ───────────────────────

(defconst dorg-test-project--public-export-manifest
  "public-export-manifest.txt"
  "Root-relative manifest defining the private paths excluded from public.")

(defconst dorg-test-project--public-release-gate
  "tools/public-release-gate"
  "Root-relative observation-only gate for the public candidate receipt.")

(defconst dorg-test-project--public-scrub-source
  "tools/dorg-public-scrub.el"
  "Shared scrub predicate used by both ERT and the public release gate.")

(defvar dorg-public-scrub-rules)
(defvar dorg-public-scrub-bare-slug-allowlist)
(declare-function dorg-public-scrub-classify-string
                  "tools/dorg-public-scrub" (text known-dorg-tokens))

(defun dorg-test-project--command-output (program &rest args)
  "Run PROGRAM with ARGS and return stdout, or signal with captured output."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil (current-buffer) nil args)))
      (unless (equal status 0)
        (error "%s %S failed (%S):\n%s"
               program args status (buffer-string)))
      (buffer-string))))

(defun dorg-test-project--git (repo &rest args)
  "Run git ARGS in REPO and return stdout without trailing newlines."
  (string-trim-right
   (apply #'dorg-test-project--command-output
          "git" "-C" repo args)))

(defun dorg-test-project--git-root-commit (repo tree message &optional parent)
  "Create and return an unreferenced commit for TREE in REPO.
With PARENT, create one later-release commit above it; without PARENT, create
an orphan/root commit suitable for the first public release."
  (if parent
      (dorg-test-project--git repo "commit-tree" tree "-p" parent "-m" message)
    (dorg-test-project--git repo "commit-tree" tree "-m" message)))

(defun dorg-test-project--tracked-files ()
  "Return every path tracked by the candidate repository."
  (split-string
   (dorg-test-project--git dorg-test-project--root "ls-files" "-z")
   "\0" t))

(defun dorg-test-project--manifest-lines (&optional required)
  "Return active public export manifest lines.
When REQUIRED is non-nil, fail if the manifest does not exist."
  (let ((path (expand-file-name dorg-test-project--public-export-manifest
                                dorg-test-project--root)))
    (when required
      (should (file-exists-p path)))
    (when (file-exists-p path)
      (cl-loop for line in (split-string
                            (dorg-test-project--file-contents path) "\n")
               for trimmed = (string-trim line)
               unless (or (string-empty-p trimmed)
                          (string-prefix-p "#" trimmed))
               collect trimmed))))

(defun dorg-test-project--manifest-excludes-p (path entries)
  "Return non-nil when manifest ENTRIES exclude root-relative PATH."
  (cl-some (lambda (entry)
             (if (string-suffix-p "/" entry)
                 (string-prefix-p entry path)
               (equal entry path)))
           entries))

(defun dorg-test-project--known-dorg-symbols ()
  "Return DOrg tokens backed by loaded state or a source definition."
  (let ((definition-re
         "^[ 	]*(\\(?:[[:alnum:]-]*def[[:alnum:]-]*\\|define-[[:alnum:]-]+\\|provide\\)[ 	\n]+['(]*\\(dorg-[[:alnum:]-]+\\)")
        known)
    (mapatoms
     (lambda (symbol)
       (when (and (string-prefix-p "dorg-" (symbol-name symbol))
                  (or (boundp symbol)
                      (fboundp symbol)
                      (ert-test-boundp symbol)
                      (featurep symbol)))
         (push (symbol-name symbol) known))))
    (dolist (relative (dorg-test-project--tracked-files))
      (when (string-suffix-p ".el" relative)
        (let ((text (dorg-test-project--file-contents
                     (expand-file-name relative dorg-test-project--root)))
              (start 0))
          (while (string-match definition-re text start)
            (push (match-string 1 text) known)
            (setq start (match-end 0))))))
    (delete-dups known)))

(defun dorg-test-project--known-dorg-token-table ()
  "Return a hash set of known DOrg names and their namespace prefixes."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (symbol-name (dorg-test-project--known-dorg-symbols))
      (let ((prefix symbol-name))
        (while (string-prefix-p "dorg-" prefix)
          (puthash prefix t table)
          (if (string-match "\\`\\(.+\\)-[^-]+\\'" prefix)
              (setq prefix (match-string 1 prefix))
            (setq prefix "")))))
    table))

(defun dorg-test-project--load-public-scrub-source ()
  "Load and validate the one rule source shared with the release gate."
  (let ((path (expand-file-name dorg-test-project--public-scrub-source
                                dorg-test-project--root)))
    (should (file-readable-p path))
    (load path nil t)
    (should (boundp 'dorg-public-scrub-rules))
    (should (boundp 'dorg-public-scrub-bare-slug-allowlist))
    (should (fboundp 'dorg-public-scrub-classify-string))
    path))

(defun dorg-test-project--write-fixture (root relative contents &optional mode)
  "Write CONTENTS under ROOT at RELATIVE with MODE (default 0644)."
  (let ((path (expand-file-name relative root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert contents))
    (set-file-modes path (or mode #o644))))

(defun dorg-test-project--call-public-release-gate
    (repo accounting-baseline baseline trunk public receipt &optional scrub-source)
  "Run the public release gate for fixture commits and return status/output."
  (let ((gate (expand-file-name dorg-test-project--public-release-gate
                                dorg-test-project--root)))
    (with-temp-buffer
      (let ((status
             (call-process gate nil (current-buffer) nil
                           "--repo" repo
                           "--manifest" dorg-test-project--public-export-manifest
                           "--scrub-source"
                           (or scrub-source
                               (expand-file-name
                                dorg-test-project--public-scrub-source
                                dorg-test-project--root))
                           "--accounting-baseline" accounting-baseline
                           "--baseline" baseline
                           "--trunk" trunk
                           "--public" public
                           "--receipt" receipt)))
        (cons status (buffer-string))))))

(defun dorg-test-project--protected-ref-listing (repo)
  "Return the publication-relevant ref listing expected from the gate in REPO."
  (mapconcat
   #'identity
   (cl-remove-if-not
    (lambda (line)
      (string-match-p
       "\\(?: refs/heads/\\(?:trunk\\|main\\)\\'\\| refs/\\(?:tags\\|remotes\\)/\\)"
       line))
    (split-string
     (dorg-test-project--git
      repo "for-each-ref" "--format=%(objectname) %(refname)"
      "refs/heads" "refs/tags" "refs/remotes")
     "\n" t))
   "\n"))

(defun dorg-test-project--write-ref-mutating-scrub
    (path repo ref new-value)
  "Write a scrub adapter at PATH that mutates REF in REPO to NEW-VALUE once."
  (let ((source (expand-file-name dorg-test-project--public-scrub-source
                                  dorg-test-project--root)))
    (with-temp-file path
      (insert (format "(load %S nil t)\n" source))
      (insert "(defvar dorg-test-project--ref-mutated nil)\n")
      (insert "(defalias 'dorg-test-project--real-scrub "
              "(symbol-function 'dorg-public-scrub-classify-string))\n")
      (insert
       (format
        (concat
         "(defun dorg-public-scrub-classify-string (text known)\n"
         "  (unless dorg-test-project--ref-mutated\n"
         "    (setq dorg-test-project--ref-mutated t)\n"
         "    (unless (= 0 (call-process \"git\" nil nil nil \"-C\" %S "
         "\"update-ref\" %S %S))\n"
         "      (error \"fixture ref mutation failed\")))\n"
         "  (funcall #'dorg-test-project--real-scrub text known))\n")
        repo ref new-value)))))

(ert-deftest dorg-test-project-public-export-manifest-is-explicit ()
  "The public privacy boundary is explicit, deterministic, and fail-closed.
The source manifest names every exclusion with literal root-relative entries;
the filtered public tree contains none of those paths.  Both contexts track
the public development guide."
  (let* ((manifest-path
          (expand-file-name dorg-test-project--public-export-manifest
                            dorg-test-project--root))
         (manifest-present (file-exists-p manifest-path))
         (entries (dorg-test-project--manifest-lines))
         (required (list (concat ".em" "bead/")
                         (concat ".me" "ld/")
                         "AGENTS.md" "CLAUDE.md"
                         "public-export-manifest.txt"))
         (public-guide "DEVELOPMENT.org")
         (tracked (dorg-test-project--tracked-files)))
    (should (member public-guide tracked))
    (if manifest-present
        (progn
          (should (equal entries (sort (copy-sequence entries) #'string<)))
          (should (= (length entries)
                     (length (delete-dups (copy-sequence entries)))))
          (dolist (entry required)
            (should (member entry entries)))
          (should-not
           (dorg-test-project--manifest-excludes-p public-guide entries))
          (dolist (entry entries)
            (should-not (file-name-absolute-p entry))
            (should-not (member entry '("." ".." "./" "../")))
            (should-not
             (string-match-p "\\(?:^\\|/\\)\\.\\.?\\(?:/\\|$\\)" entry))
            (should-not (string-match-p "[][?*{}]" entry))
            (when (file-directory-p
                   (expand-file-name entry dorg-test-project--root))
              (should (string-suffix-p "/" entry)))))
      (should-not entries)
      (dolist (path tracked)
        (should-not (dorg-test-project--manifest-excludes-p path required))))))

(ert-deftest dorg-test-project-public-export-scrub-is-exhaustive ()
  "Every manifest-filtered tracked byte is free of internal release metadata.
Each forbidden class has a positive control, preventing a silently empty or
disabled scanner.  Actual preimage offenders make the transformation RED."
  (let ((case-fold-search t)
        (entries (dorg-test-project--manifest-lines))
        (known-symbols (dorg-test-project--known-dorg-token-table))
        violations)
    (dorg-test-project--load-public-scrub-source)
    (dolist (relative (dorg-test-project--tracked-files))
      (unless (dorg-test-project--manifest-excludes-p relative entries)
        (let ((path (expand-file-name relative dorg-test-project--root)))
          (when (file-regular-p path)
            (let ((text (with-temp-buffer
                          (insert-file-contents-literally path)
                          (buffer-string))))
              (dolist (class (dorg-public-scrub-classify-string
                              text known-symbols))
                (push (format "%s:%s" relative class) violations)))))))
    (setq violations (delete-dups violations))
    (ert-info ((format "public export scrub offenders (%d): %S"
                       (length violations)
                       (cl-subseq
                        (nreverse (copy-sequence violations)) 0
                        (min 200 (length violations)))))
      (should-not violations))))

(ert-deftest dorg-test-project-public-scrub-rules-have-safe-boundaries ()
  "The shared scrub rules separate private coordination tokens from public text.
Tracker references, seat identities, home-directory paths and the internal
codename are forbidden.  The names of DOrg's sibling tools are public product
names and must not be classified (operator decision, 2026-10-05), so the two
former brand classes are absent and their directory forms pass.  Bare slugs have
the exact one-segment tracker-id shape; legitimate public tokens require a
documented exact-token allowlist."
  (let ((known-symbols (dorg-test-project--known-dorg-token-table)))
    (dorg-test-project--load-public-scrub-source)
    (should (consp dorg-public-scrub-rules))
    (dolist (class '("tracker reference" "agent identity" "private path"
                     "internal codename" "bare tracker slug"))
      (should (assoc class dorg-public-scrub-rules)))
    (dolist (class '("tracker brand" "coordination brand"))
      (ert-info ((format "retired scrub class still present: %s" class))
        (should-not (assoc class dorg-public-scrub-rules))))
    (dolist (control
             `(("tracker reference" . ,(concat "see b" "d:dorg-" "ab12"))
               ("agent identity" . ,(concat "nav-" "ab12"))
               ("private path" . ,(concat "/Us" "ers/example/lane"))
               ("internal codename" . ,(concat "Amb" "ient workload"))
               ("bare tracker slug" . ,(concat "dorg-" "ab12"))
               ("bare tracker slug" . ,(concat "dorg-" "ab12.foo3"))))
      (ert-info ((format "positive scrub control: %s" (car control)))
        (should (member (car control)
                        (dorg-public-scrub-classify-string
                         (cdr control) known-symbols)))))
    (dolist (control
             `(("B3 environment name" . "EMBEAD_DIR")
               ("tracker tool command" . "run embd status")
               ("tracker tool identifier" . "embead-dir")
               ("tracker data directory" . ".embead/")
               ("coordination tool name" . "MelD handoff")
               ("coordination data directory" . ".meld/")
               ("multi-segment DOrg name" . ,(concat "dorg-" "test-run"))
               ("too-short slug" . ,(concat "dorg-" "ab"))
               ("too-long slug" . ,(concat "dorg-" "abcdef"))
               ("uppercase slug" . ,(concat "dorg-" "AB12"))
               ("embedded slug" . ,(concat "xdorg-" "ab12"))))
      (ert-info ((format "negative scrub control: %s" (car control)))
        (should-not (dorg-public-scrub-classify-string
                     (cdr control) known-symbols))))
    (should-not (gethash (concat "dorg-" "ab12") known-symbols))
    (should (consp dorg-public-scrub-bare-slug-allowlist))
    (let (tokens)
      (dolist (entry dorg-public-scrub-bare-slug-allowlist)
        (should (and (consp entry)
                     (string-match-p
                      "\\`dorg-[a-z0-9]\\{3,5\\}\\(?:\\.[a-z0-9]\\{3,5\\}\\)*\\'"
                      (car entry))
                     (stringp (cdr entry))
                     (not (string-empty-p (string-trim (cdr entry))))))
        (should-not (member (car entry) tokens))
        (push (car entry) tokens)
        (ert-info ((format "allowlisted token still classified: %s" (car entry)))
          (should-not (dorg-public-scrub-classify-string
                       (car entry) (make-hash-table :test #'equal))))))))

(ert-deftest dorg-test-project-gitignore-passes-public-scrub ()
  "Every line of the repository .gitignore passes the shared scrub.
The ignore entries for the tracker and coordination directories are spelled
literally; naming an ignored directory is public information, so the file
needs no per-file exemption in the release gate."
  (let* ((known-symbols (dorg-test-project--known-dorg-token-table))
         (path (expand-file-name ".gitignore" dorg-test-project--root))
         (lines (progn
                  (should (file-readable-p path))
                  (with-temp-buffer
                    (insert-file-contents path)
                    (split-string (buffer-string) "\n"))))
         violations)
    (dorg-test-project--load-public-scrub-source)
    (should (member ".embead/" lines))
    (should (member ".meld/" lines))
    (dolist (line lines)
      (when-let ((classes (dorg-public-scrub-classify-string line known-symbols)))
        (push (cons line classes) violations)))
    (should-not violations)))

(ert-deftest dorg-test-project-public-release-gate-reports-net-positive-accounting ()
  "A growing candidate is valid when its accounting is exact.
The fixture grows both physical production lines and definitions.  The gate
must reach byte/scrub verification, write its receipt, and report the declared
baseline plus exact before/after/add/delete values rather than treating growth
as a release failure."
  (let ((repo (make-temp-file "dorg-public-growth-gate-" t))
        (receipt (make-temp-file "dorg-public-growth-receipt-" nil ".json"))
        baseline trunk public)
    (unwind-protect
        (progn
          (dorg-test-project--git repo "init" "-q" "-b" "private-trunk")
          (dorg-test-project--git repo "config" "user.name" "DOrg Test")
          (dorg-test-project--git repo "config" "user.email"
                                  "test@example.invalid")
          (dorg-test-project--write-fixture
           repo "core.el" "(defun dorg-core-a () t)\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m"
                                  "accounting baseline")
          (setq baseline (dorg-test-project--git repo "rev-parse" "HEAD"))

          (dorg-test-project--write-fixture
           repo "core.el"
           (concat "(defun dorg-core-a () t)\n"
                   "(defun dorg-core-b () t)\n"))
          (dorg-test-project--write-fixture repo "private.txt" "private\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest
           "private.txt\npublic-export-manifest.txt\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m" "growing candidate")
          (setq trunk (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--git
           repo "rm" "-q" "--cached" "private.txt"
           dorg-test-project--public-export-manifest)
          (setq public
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "growing public root"))

          (let* ((result (dorg-test-project--call-public-release-gate
                          repo baseline "EMPTY" trunk public receipt))
                 (data (and (equal 0 (car result))
                            (json-parse-string
                             (dorg-test-project--file-contents receipt)
                             :object-type 'alist))))
            (ert-info ((format "net-positive gate output:\n%s" (cdr result)))
              (should (equal 0 (car result))))
            (should (equal baseline
                           (alist-get 'accounting_baseline_sha data)))
            (should (equal 1 (alist-get 'production_lines_before data)))
            (should (equal 2 (alist-get 'production_lines_after data)))
            (should (equal 1 (alist-get 'production_lines_added data)))
            (should (equal 0 (alist-get 'production_lines_deleted data)))
            (should (equal 1
                           (alist-get 'production_definitions_before data)))
            (should (equal 2
                           (alist-get 'production_definitions_after data)))
            (should (eq t (alist-get 'scrub_passed data)))
            (should (eq t (alist-get 'byte_mode_equal data)))))
      (ignore-errors (delete-file receipt))
      (ignore-errors (delete-directory repo t)))))

(ert-deftest dorg-test-project-public-release-gate-rejects-invalid-accounting ()
  "A binary root Elisp delta fails with the accounting-invalid class.
The planted NUL forces Git numstat to emit non-numeric fields.  Failure must be
one parseable JSON document and must not overwrite an existing receipt."
  (let ((repo (make-temp-file "dorg-public-invalid-accounting-" t))
        (receipt (make-temp-file "dorg-public-invalid-receipt-" nil ".json"))
        baseline trunk public)
    (unwind-protect
        (progn
          (dorg-test-project--git repo "init" "-q" "-b" "private-trunk")
          (dorg-test-project--git repo "config" "user.name" "DOrg Test")
          (dorg-test-project--git repo "config" "user.email"
                                  "test@example.invalid")
          (dorg-test-project--write-fixture
           repo "core.el" "(defun dorg-core-a () t)\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m"
                                  "accounting baseline")
          (setq baseline (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--write-fixture
           repo "core.el" (concat "(defun dorg-core-a () t)\n" "\0binary\n"))
          (dorg-test-project--write-fixture repo "private.txt" "private\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest
           "private.txt\npublic-export-manifest.txt\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m"
                                  "binary accounting candidate")
          (setq trunk (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--git
           repo "rm" "-q" "--cached" "private.txt"
           dorg-test-project--public-export-manifest)
          (setq public
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "binary public root"))
          (dorg-test-project--write-fixture repo receipt "preserve me\n")
          (let* ((result (dorg-test-project--call-public-release-gate
                          repo baseline "EMPTY" trunk public receipt))
                 (data (json-parse-string (string-trim (cdr result))
                                          :object-type 'alist)))
            (ert-info ((format "invalid-accounting gate output:\n%s"
                               (cdr result)))
              (should-not (equal 0 (car result))))
            (should (equal "accounting-invalid"
                           (alist-get 'failure_reason data)))
            (should (equal "preserve me\n"
                           (dorg-test-project--file-contents receipt)))))
      (ignore-errors (delete-file receipt))
      (ignore-errors (delete-directory repo t)))))

(ert-deftest dorg-test-project-public-release-gate-scopes-protected-refs ()
  "Only publication-relevant refs participate in the no-mutation witness.
A scrub adapter mutates one ref after the gate's before-snapshot.  An agent ref
is a negative control and must not perturb the receipt; a tag is a positive
control and must fail specifically as protected-refs-changed."
  (let ((repo (make-temp-file "dorg-public-ref-scope-" t))
        (ignored-receipt
         (make-temp-file "dorg-public-ignored-ref-receipt-" nil ".json"))
        (protected-receipt
         (make-temp-file "dorg-public-protected-ref-receipt-" nil ".json"))
        (ignored-scrub (make-temp-file "dorg-public-ignored-ref-scrub-" nil ".el"))
        (protected-scrub
         (make-temp-file "dorg-public-protected-ref-scrub-" nil ".el"))
        baseline trunk public expected-listing expected-digest
        ignored-result ignored-data protected-result protected-data)
    (unwind-protect
        (progn
          (dorg-test-project--git repo "init" "-q" "-b" "private-trunk")
          (dorg-test-project--git repo "config" "user.name" "DOrg Test")
          (dorg-test-project--git repo "config" "user.email"
                                  "test@example.invalid")
          (dorg-test-project--write-fixture
           repo "core.el"
           (concat "(defun dorg-core-a () t)\n"
                   "(defun dorg-core-b () t)\n"))
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m"
                                  "accounting baseline")
          (setq baseline (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--write-fixture
           repo "core.el" "(defun dorg-core-a () t)\n")
          (dorg-test-project--write-fixture repo "private.txt" "private\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest
           "private.txt\npublic-export-manifest.txt\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m"
                                  "shrinking candidate")
          (setq trunk (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--git repo "branch" "trunk" trunk)
          (dorg-test-project--git
           repo "rm" "-q" "--cached" "private.txt"
           dorg-test-project--public-export-manifest)
          (setq public
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "public root")
                expected-listing
                (dorg-test-project--protected-ref-listing repo)
                expected-digest (secure-hash 'sha256 expected-listing))

          (dorg-test-project--write-ref-mutating-scrub
           ignored-scrub repo "refs/heads/agent/noise" trunk)
          (setq ignored-result
                (dorg-test-project--call-public-release-gate
                 repo baseline "EMPTY" trunk public ignored-receipt
                 ignored-scrub)
                ignored-data
                (and (equal 0 (car ignored-result))
                     (json-parse-string
                      (dorg-test-project--file-contents ignored-receipt)
                      :object-type 'alist)))

          (dorg-test-project--git repo "update-ref" "-d"
                                  "refs/heads/agent/noise")
          (dorg-test-project--write-ref-mutating-scrub
           protected-scrub repo "refs/tags/protected-probe" trunk)
          (setq protected-result
                (dorg-test-project--call-public-release-gate
                 repo baseline "EMPTY" trunk public protected-receipt
                 protected-scrub)
                protected-data
                (json-parse-string (string-trim (cdr protected-result))
                                   :object-type 'alist))

          ;; Assert the positive failure control before the deliberately RED
          ;; ignored-ref expectation so a failing current implementation still
          ;; proves the protected class rather than short-circuiting first.
          (ert-info ((format "protected-ref gate output:\n%s"
                             (cdr protected-result)))
            (should-not (equal 0 (car protected-result))))
          (should (equal "protected-refs-changed"
                         (alist-get 'failure_reason protected-data)))
          (ert-info ((format "ignored-ref gate output:\n%s"
                             (cdr ignored-result)))
            (should (equal 0 (car ignored-result))))
          (should (equal expected-digest
                         (alist-get 'protected_refs_before_sha256 ignored-data)))
          (should (equal expected-digest
                         (alist-get 'protected_refs_after_sha256 ignored-data))))
      (ignore-errors (delete-file ignored-receipt))
      (ignore-errors (delete-file protected-receipt))
      (ignore-errors (delete-file ignored-scrub))
      (ignore-errors (delete-file protected-scrub))
      (ignore-errors (delete-directory repo t)))))

(ert-deftest dorg-test-project-public-release-gate-proves-one-squash-and-bytes ()
  "The release gate proves one public commit and exact manifest-filtered bytes.
A synthetic repository exercises the first-release EMPTY baseline with an
orphan public commit and retains a non-empty baseline case for later releases.
Independent controls distinguish zero/two commits, content drift, mode drift,
and a planted scrub leak.  Failed runs emit their class without overwriting the
successful receipt, and no run changes a ref."
  (let* ((gate (expand-file-name dorg-test-project--public-release-gate
                                 dorg-test-project--root))
         (repo (make-temp-file "dorg-public-release-gate-" t))
         (receipt (make-temp-file "dorg-public-release-receipt-" nil ".json"))
         (later-receipt
          (make-temp-file "dorg-public-later-release-receipt-" nil ".json"))
         (empty-baseline "EMPTY")
         later-baseline trunk public public-tree later-public drift mode-drift
         two-commits leaky-trunk leaky-public refs-before receipt-bytes)
    (unwind-protect
        (progn
          (should (file-executable-p gate))
          (dorg-test-project--git repo "init" "-q" "-b" "private-trunk")
          (dorg-test-project--git repo "config" "user.name" "DOrg Test")
          (dorg-test-project--git repo "config" "user.email" "test@example.invalid")
          (dorg-test-project--write-fixture repo "README.org" "baseline\n")
          (dorg-test-project--write-fixture
           repo "core.el" "(defun dorg-core-a ()\n  t)\n(defun dorg-core-b () t)\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m" "later public baseline")
          (setq later-baseline (dorg-test-project--git repo "rev-parse" "HEAD"))
          (should (string-empty-p (dorg-test-project--git repo "remote")))
          (should-error (dorg-test-project--git repo "rev-parse" "origin/main"))

          (dorg-test-project--write-fixture repo "README.org" "public bytes\n")
          (dorg-test-project--write-fixture repo "core.el"
                                            "(defun dorg-core-a () t)\n")
          (dorg-test-project--write-fixture repo "bin/tool" "#!/bin/sh\nexit 0\n" #o755)
          (dorg-test-project--write-fixture repo "private.txt" "internal only\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest
           "private.txt\npublic-export-manifest.txt\n")
          (dorg-test-project--git repo "add" "-A")
          (dorg-test-project--git repo "commit" "-q" "-m" "trunk candidate")
          (setq trunk (dorg-test-project--git repo "rev-parse" "HEAD"))
          (dorg-test-project--git repo "branch" "trunk" trunk)

          ;; The public tree contains exactly the trunk bytes left by the
          ;; manifest.  First-release PUBLIC is deliberately a root commit.
          (dorg-test-project--git
           repo "rm" "-q" "--cached" "private.txt"
           dorg-test-project--public-export-manifest)
          (setq public-tree (dorg-test-project--git repo "write-tree")
                public (dorg-test-project--git-root-commit
                        repo public-tree "first public squash")
                later-public (dorg-test-project--git-root-commit
                              repo public-tree "later public squash"
                              later-baseline)
                two-commits (dorg-test-project--git-root-commit
                             repo public-tree "extra public commit" public))
          (should (= 1 (length (split-string
                                (dorg-test-project--git
                                 repo "rev-list" "--parents" "-n" "1" public)))))

          (dorg-test-project--write-fixture repo "README.org" "wrong bytes\n")
          (dorg-test-project--git repo "add" "README.org")
          (setq drift
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "content-drifted squash"))

          (dorg-test-project--write-fixture repo "README.org" "public bytes\n")
          (dorg-test-project--write-fixture repo "bin/tool" "#!/bin/sh\nexit 0\n" #o644)
          (dorg-test-project--git repo "add" "README.org" "bin/tool")
          (setq mode-drift
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "mode-drifted squash"))

          ;; Build matching leaky trunk/public trees so only the shared scrub
          ;; predicate can reject this candidate.
          (dorg-test-project--write-fixture
           repo "README.org" (concat "public bytes; see b" "d:dorg-" "ab12\n"))
          (dorg-test-project--write-fixture repo "bin/tool" "#!/bin/sh\nexit 0\n" #o755)
          (dorg-test-project--write-fixture repo "private.txt" "internal only\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest
           "private.txt\npublic-export-manifest.txt\n")
          (dorg-test-project--git repo "add" "-A")
          (setq leaky-trunk
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "leaky trunk candidate" trunk))
          (dorg-test-project--git
           repo "rm" "-q" "--cached" "private.txt"
           dorg-test-project--public-export-manifest)
          (setq leaky-public
                (dorg-test-project--git-root-commit
                 repo (dorg-test-project--git repo "write-tree")
                 "leaky public squash")
                refs-before (dorg-test-project--git repo "show-ref"))

          ;; Poison the working tree after every candidate is committed.  A
          ;; valid verdict must read both manifest and exported bytes from the
          ;; named trunk object, never from these implicit files.
          (dorg-test-project--write-fixture
           repo "README.org" "uncommitted working-tree poison\n")
          (dorg-test-project--write-fixture
           repo dorg-test-project--public-export-manifest "README.org\n")

          (let* ((result (dorg-test-project--call-public-release-gate
                          repo later-baseline empty-baseline trunk public receipt))
                 (receipt-data
                  (and (equal 0 (car result))
                       (json-parse-string
                        (dorg-test-project--file-contents receipt)
                        :object-type 'alist)))
                 (refs-digest
                  (secure-hash
                   'sha256 (dorg-test-project--protected-ref-listing repo)))
                 (manifest-digest
                  (secure-hash
                   'sha256
                   (dorg-test-project--command-output
                    "git" "-C" repo "show"
                    (format "%s:%s" trunk
                            dorg-test-project--public-export-manifest)))))
            (ert-info ((format "public release gate output:\n%s" (cdr result)))
              (should (equal 0 (car result))))
            (should (equal empty-baseline
                           (alist-get 'public_baseline_sha receipt-data)))
            (should (equal trunk (alist-get 'trunk_candidate_sha receipt-data)))
            (should (equal (dorg-test-project--git
                            repo "rev-parse" (concat trunk "^{tree}"))
                           (alist-get 'trunk_tree receipt-data)))
            (should (equal public (alist-get 'public_candidate_sha receipt-data)))
            (should (equal (dorg-test-project--git
                            repo "rev-parse" (concat public "^{tree}"))
                           (alist-get 'public_tree receipt-data)))
            (should (equal manifest-digest
                           (alist-get 'manifest_sha256 receipt-data)))
            (should (equal 2 (alist-get 'excluded_path_count receipt-data)))
            (should (equal 1 (alist-get 'public_commit_count receipt-data)))
            (should (equal later-baseline
                           (alist-get 'accounting_baseline_sha receipt-data)))
            (should (equal 3 (alist-get 'production_lines_before receipt-data)))
            (should (equal 1 (alist-get 'production_lines_after receipt-data)))
            (should (equal 1 (alist-get 'production_lines_added receipt-data)))
            (should (equal 3 (alist-get 'production_lines_deleted receipt-data)))
            (should (equal 2
                           (alist-get 'production_definitions_before receipt-data)))
            (should (equal 1
                           (alist-get 'production_definitions_after receipt-data)))
            (should (eq t (alist-get 'scrub_passed receipt-data)))
            (should (equal
                     (secure-hash
                      'sha256
                      (dorg-test-project--file-contents
                       (expand-file-name dorg-test-project--public-scrub-source
                                         dorg-test-project--root)))
                     (alist-get 'scrub_rule_source_sha256 receipt-data)))
            (should (eq t (alist-get 'byte_mode_equal receipt-data)))
            (should (eq t (alist-get 'protected_refs_unchanged receipt-data)))
            (should (equal refs-digest
                           (alist-get 'protected_refs_before_sha256 receipt-data)))
            (should (equal refs-digest
                           (alist-get 'protected_refs_after_sha256 receipt-data)))
            (should (eq :false (alist-get 'push_performed receipt-data)))
            (should (string-match-p
                     "\\`[[:xdigit:]]\\{64\\}\\'"
                     (alist-get 'manifest_sha256 receipt-data)))
            (setq receipt-bytes (dorg-test-project--file-contents receipt)))

          ;; A real commit baseline remains supported for later public releases.
          (let ((result (dorg-test-project--call-public-release-gate
                         repo later-baseline later-baseline trunk later-public
                         later-receipt)))
            (ert-info ((format "later-baseline gate output:\n%s" (cdr result)))
              (should (equal 0 (car result))))
            (let ((data (json-parse-string
                         (dorg-test-project--file-contents later-receipt)
                         :object-type 'alist)))
              (should (equal later-baseline
                             (alist-get 'public_baseline_sha data)))
              (should (equal 1 (alist-get 'public_commit_count data)))))

          (dolist (control
                   `(("zero commits" ,later-baseline ,trunk ,later-baseline
                      "commit-count" nil)
                     ("content drift" ,empty-baseline ,trunk ,drift
                      "content-drift" nil)
                     ("mode drift" ,empty-baseline ,trunk ,mode-drift
                      "mode-drift" nil)
                     ("two commits" ,empty-baseline ,trunk ,two-commits
                      "commit-count" nil)
                     ("planted leak" ,empty-baseline ,leaky-trunk ,leaky-public
                      "scrub-leak" t)))
            (pcase-let ((`(,label ,control-baseline ,control-trunk ,candidate
                           ,failure-reason ,scrub-failure-p)
                         control))
              (let* ((result (dorg-test-project--call-public-release-gate
                              repo later-baseline control-baseline control-trunk
                              candidate receipt))
                     (failure-data
                      (and (not (equal 0 (car result)))
                           (json-parse-string (string-trim (cdr result))
                                              :object-type 'alist))))
                (ert-info ((format "%s gate output:\n%s" label (cdr result)))
                  (should-not (equal 0 (car result))))
                (should (equal failure-reason
                               (alist-get 'failure_reason failure-data)))
                (when scrub-failure-p
                  (should (eq :false
                              (alist-get 'scrub_passed failure-data)))))))
          (should (equal receipt-bytes
                         (dorg-test-project--file-contents receipt)))
          (should (equal refs-before (dorg-test-project--git repo "show-ref"))))
      (ignore-errors (delete-file receipt))
      (ignore-errors (delete-file later-receipt))
      (ignore-errors (delete-directory repo t)))))

(ert-deftest dorg-test-project-public-release-gate-is-observation-only ()
  "The candidate gate cannot publish or mutate protected release refs.
It may inspect local objects and write the requested receipt, but contains no
network, tagging, commit, checkout, branch, or ref-update command."
  (let* ((path (expand-file-name dorg-test-project--public-release-gate
                                 dorg-test-project--root))
         (text (and (file-exists-p path)
                    (dorg-test-project--file-contents path))))
    (should (file-executable-p path))
    (should text)
    (dolist (forbidden '("git[ 	]+push\\b"
                         "git[ 	]+fetch\\b"
                         "git[ 	]+tag\\b"
                         "git[ 	]+commit\\b"
                         "git[ 	]+checkout\\b"
                         "git[ 	]+switch\\b"
                         "git[ 	]+branch\\b"
                         "git[ 	]+update-ref\\b"
                         "gh[ 	]+release\\b"))
      (should-not (string-match-p forbidden text)))))

;;; ─── Installed-root Make guard ─────────────────────────────────────────

(defconst dorg-test-project--installed-root-marker ".installed-root"
  "Per-worktree marker that protects an active installed checkout.")

(defconst dorg-test-project--installed-root-guarded-goals
  '("test" "tests" "test-full" "test-fast" "test-medium" "test-derived" "perf"
    "bench" "lint" "clean-elc" "clean")
  "Make entry goals that must refuse in a marked installed root.")

(defconst dorg-test-project--installed-root-allowed-goals
  '("all" "compile" "install" "install-deps" "mutate-grammar" "help"
    "test-clean-scratch")
  "Make entry goals whose current prerequisite closure may not reach a guard.")

(defun dorg-test-project--make-rule-prerequisites (text)
  "Parse TEXT's simple Make target/prerequisite graph into a hash table.
The DOrg Makefile deliberately uses one target per non-pattern rule; the
parser rejects recipes and variable assignments rather than pretending they
are graph edges.  This static reader is the closure control for the real
candidate Makefile, not a second target list maintained by the test."
  (let ((rules (make-hash-table :test 'equal)))
    (dolist (line (split-string text "\n"))
      (when (string-match
             "^\\([[:alnum:]_.-]+\\):\\(?:[ \t]+\\(.*\\)\\)?$" line)
        (puthash (match-string 1 line)
                 (split-string (or (match-string 2 line) "") "[ \t]+" t)
                 rules)))
    rules))

(defun dorg-test-project--make-prerequisite-closure (rules goal &optional seen)
  "Return GOAL's transitive prerequisite closure in RULES.
SEEN is the recursive cycle guard; it keeps a future accidental Make cycle
from turning this static safety control into a nonterminating test."
  (unless (member goal seen)
    (let ((seen (cons goal seen)) out)
      (dolist (prerequisite (gethash goal rules))
        (push prerequisite out)
        (setq out (nconc out (dorg-test-project--make-prerequisite-closure
                              rules prerequisite seen))))
      (delete-dups out))))

(defun dorg-test-project--make-call (dir &rest goals)
  "Run Make in owned fixture DIR with GOALS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory dir))
      (cons (apply #'call-process "make" nil t nil "--no-print-directory" goals)
            (buffer-string)))))

(defun dorg-test-project--installed-root-fixture (root marker-kind)
  "Populate ROOT with the candidate Makefile and MARKER-KIND entry.
MARKER-KIND is one of `file', `directory', `symlink' or `dangling-symlink'.
The copied Makefile is byte-for-byte the candidate under test; guarded
invocations must stop during parsing, before any of its absent build inputs
could be mistaken for the reason the command failed."
  (copy-file (expand-file-name "Makefile" dorg-test-project--root)
             (expand-file-name "Makefile" root))
  (let ((marker (expand-file-name dorg-test-project--installed-root-marker root)))
    (pcase marker-kind
      ('file (with-temp-file marker (insert "marker\n")))
      ('directory (make-directory marker))
      ('symlink (let ((target (expand-file-name "marker-target" root)))
                  (with-temp-file target (insert "target\n"))
                  (make-symbolic-link target marker)))
      ('dangling-symlink
       (make-symbolic-link (expand-file-name "does-not-exist" root) marker)))
    marker))

(defun dorg-test-project--marker-observation (marker)
  "Return a non-following observation of MARKER for mutation assertions."
  (list :link (file-symlink-p marker)
        :exists (file-exists-p marker)
        :directory (file-directory-p marker)
        :bytes (and (file-regular-p marker)
                    (dorg-test-project--file-contents marker))))

(defun dorg-test-project--installed-root-without-preflight (makefile)
  "Return MAKEFILE with the parse-time marked-root refusal deleted.
This is an in-memory RED witness only: the fixture writes it to an owned
temporary root, never to the candidate."
  (replace-regexp-in-string
   "^\\$(error dorg: REFUSE guarded target.*\n" "" makefile))

(ert-deftest dorg-test-project-installed-root-refuses-every-guarded-goal ()
  "File, directory and both symlink marker forms refuse before Make effects."
  (dolist (kind '(file directory symlink dangling-symlink))
    (let ((root (make-temp-file "dorg-installed-root-" t)))
      (unwind-protect
          (let* ((marker (dorg-test-project--installed-root-fixture root kind))
                 (before (dorg-test-project--marker-observation marker))
                 (elc (expand-file-name "sentinel.elc" root))
                 (bin (expand-file-name "bin/sentinel" root)))
            (make-directory (file-name-directory bin) t)
            (with-temp-file elc (insert "elc-sentinel"))
            (with-temp-file bin (insert "bin-sentinel"))
            (dolist (goal dorg-test-project--installed-root-guarded-goals)
              (pcase-let ((`(,status . ,output)
                           (dorg-test-project--make-call root goal)))
                (ert-info ((format "%s marker, target %s: %s" kind goal output))
                  (should-not (zerop status))
                  (should (string-match-p "REFUSE guarded target" output))
                  (should (string-match-p (regexp-quote goal) output))
                  (should (equal before
                                 (dorg-test-project--marker-observation marker)))
                  (should (equal "elc-sentinel"
                                 (dorg-test-project--file-contents elc)))
                  (should (equal "bin-sentinel"
                                 (dorg-test-project--file-contents bin)))))))
        (delete-directory root t)))))

(ert-deftest dorg-test-project-installed-root-guarded-closure-is-static ()
  "Allowed Make goals cannot acquire a destructive guarded prerequisite.
The RED mutation adds `clean-elc' to `install', demonstrating why inspecting
only the requested top-level goal would be insufficient."
  (let* ((makefile (dorg-test-project--makefile-text))
         (ignore-lines (split-string
                        (dorg-test-project--file-contents
                         (expand-file-name ".gitignore" dorg-test-project--root))
                        "\n" t))
         (rules (dorg-test-project--make-rule-prerequisites makefile)))
    (should (member dorg-test-project--installed-root-marker ignore-lines))
    (should (string-match-p "test -e.*test -L" makefile))
    (dolist (goal dorg-test-project--installed-root-allowed-goals)
      (let ((closure (dorg-test-project--make-prerequisite-closure rules goal)))
        (should-not (cl-intersection closure
                                     dorg-test-project--installed-root-guarded-goals
                                     :test #'equal))))
    ;; The current file has no recursive Make at all.  If one is later added,
    ;; a guarded target cannot be hidden behind an allowed goal's recipe.
    (should-not
     (string-match-p
      (concat "\\$(MAKE).*\\<"
              (regexp-opt dorg-test-project--installed-root-guarded-goals t)
              "\\>")
      makefile))
    (let* ((mutant (replace-regexp-in-string "^install:" "install: clean-elc"
                                              makefile))
           (mutant-rules (dorg-test-project--make-rule-prerequisites mutant)))
      (should (member "clean-elc"
                      (dorg-test-project--make-prerequisite-closure
                       mutant-rules "install"))))))

(ert-deftest dorg-test-project-installed-root-preflight-mutants-are-observable ()
  "Missing or delayed preflight visibly permits protected cleanup.
These two RED witnesses keep the parse-time placement requirement concrete:
removing the error lets `clean-elc' delete a sentinel, while moving refusal
behind that prerequisite deletes it before reporting the delayed refusal."
  (let ((makefile (dorg-test-project--makefile-text)))
    ;; Missing preflight: guarded cleanup succeeds and destroys the sentinel.
    (let ((root (make-temp-file "dorg-installed-root-no-preflight-" t)))
      (unwind-protect
          (let* ((marker (dorg-test-project--installed-root-fixture root 'file))
                 (sentinel (expand-file-name "protected.elc" root)))
            (with-temp-file (expand-file-name "Makefile" root)
              (insert (dorg-test-project--installed-root-without-preflight makefile)))
            (with-temp-file sentinel (insert "must-survive\n"))
            (pcase-let ((`(,status . ,_output)
                         (dorg-test-project--make-call root "clean-elc")))
              (should (zerop status))
              (should (file-exists-p marker))
              (should-not (file-exists-p sentinel))))
        (delete-directory root t)))
    ;; Delayed preflight: the command refuses, but only after clean-elc has
    ;; already removed the sentinel, proving that a recipe-time guard is late.
    (let ((root (make-temp-file "dorg-installed-root-delayed-preflight-" t)))
      (unwind-protect
          (let* ((marker (dorg-test-project--installed-root-fixture root 'file))
                 (sentinel (expand-file-name "protected.elc" root))
                 (mutant
                  (replace-regexp-in-string
                   "^test: clean-elc$"
                   "test: clean-elc\n\t@echo 'REFUSE delayed preflight' >&2; exit 2"
                   (dorg-test-project--installed-root-without-preflight makefile))))
            (with-temp-file (expand-file-name "Makefile" root)
              (insert mutant))
            (with-temp-file sentinel (insert "must-survive\n"))
            (pcase-let ((`(,status . ,output)
                         (dorg-test-project--make-call root "test")))
              (should-not (zerop status))
              (should (string-match-p "REFUSE delayed preflight" output))
              (should (file-exists-p marker))
              (should-not (file-exists-p sentinel))))
        (delete-directory root t)))))

(defun dorg-test-project--installed-root-compile-stubs (root)
  "Add the minimal recorder-backed inputs for Make's real `compile' recipe.
Return (EMACS RECORDER).  The Makefile remains the candidate's actual bytes;
only its external compiler command is replaced, because these rows prove
ordering and marker effects rather than Emacs byte compilation."
  (let ((tools (expand-file-name "tools" root))
        (emacs (expand-file-name "fake-emacs" root))
        (recorder (expand-file-name "compile-recorder" root)))
    (make-directory tools t)
    (with-temp-file (expand-file-name "fixture.el" root)
      (insert "(provide 'fixture)\n"))
    (with-temp-file (expand-file-name "dorg-public-scrub.el" tools)
      (insert "(provide 'dorg-public-scrub)\n"))
    (with-temp-file emacs
      (insert "#!/bin/sh\nprintf 'compile\\n' >> \"$DORG_INSTALLED_ROOT_RECORDER\"\n"
              "exit \"${DORG_INSTALLED_ROOT_COMPILE_STATUS:-0}\"\n"))
    (set-file-modes emacs #o700)
    (list emacs recorder)))

(ert-deftest dorg-test-project-installed-root-lifecycle-and-mixed-preflight ()
  "Compile is allowed; install-root marks after success and mixed goals refuse.
The fake compiler is an explicit recorder, so a guard accidentally placed
behind a compile/clean prerequisite becomes a direct failure rather than a
mere command-status coincidence."
  (let ((root (make-temp-file "dorg-installed-root-lifecycle-" t)))
    (unwind-protect
        (let* ((marker (dorg-test-project--installed-root-fixture root 'file))
               (before (dorg-test-project--marker-observation marker))
               (stubs (dorg-test-project--installed-root-compile-stubs root))
               (emacs (car stubs))
               (recorder (cadr stubs))
               (process-environment
                (cons (concat "DORG_INSTALLED_ROOT_RECORDER=" recorder)
                      process-environment)))
          ;; Explicit compilation and the default all target remain possible
          ;; inside an active checkout and leave the marker untouched.
          (dolist (goal '("compile" "all"))
            (pcase-let ((`(,status . ,output)
                         (dorg-test-project--make-call root goal
                                                        (concat "EMACS=" emacs))))
              (ert-info ((format "%s: %s" goal output))
                (should (zerop status))
                (should (equal before
                               (dorg-test-project--marker-observation marker))))))
          ;; The mixed-goal preflight happens before compile can record or
          ;; install-root can create/alter a marker.
          (let ((mixed-root (make-temp-file "dorg-installed-root-mixed-" t)))
            (unwind-protect
                (let* ((unused-marker
                        (expand-file-name dorg-test-project--installed-root-marker
                                          mixed-root))
                       (mixed-stubs
                        (progn
                          (dorg-test-project--installed-root-fixture mixed-root 'file)
                          ;; The fixture helper intentionally marks; remove it
                          ;; to exercise the *unmarked* mixed-goal rule.
                          (delete-file unused-marker)
                          (dorg-test-project--installed-root-compile-stubs mixed-root)))
                       (mixed-recorder (cadr mixed-stubs))
                       (process-environment
                        (cons (concat "DORG_INSTALLED_ROOT_RECORDER=" mixed-recorder)
                              process-environment)))
                  (pcase-let ((`(,status . ,output)
                               (dorg-test-project--make-call
                                mixed-root "-j4" "install-root" "clean"
                                (concat "EMACS=" (car mixed-stubs)))))
                    (should-not (zerop status))
                    (should (string-match-p "REFUSE install-root combined" output))
                    (should-not (file-exists-p unused-marker))
                    (should-not (file-exists-p mixed-recorder))))
              (delete-directory mixed-root t))))
      (delete-directory root t))))

(ert-deftest dorg-test-project-installed-root-is-per-worktree ()
  "A primary marker does not turn a separate linked worktree into an install.
The marker is intentionally untracked and root-local.  This control creates a
real Git linked worktree, puts the marker only in the primary checkout, then
proves that `clean-elc' can remove bytecode in the linked checkout.  Thus a
global or repository-common marker lookup cannot accidentally make ordinary
lane worktrees unusable."
  (let* ((root (make-temp-file "dorg-installed-root-primary-" t))
         ;; `git -C ROOT worktree add' interprets relative destinations from
         ;; ROOT, while ERT's temporary default-directory is this checkout.
         ;; Make the independent worktree location explicit.
         (linked (make-temp-name
                  (expand-file-name "dorg-installed-root-linked-"
                                    temporary-file-directory)))
         (marker (expand-file-name dorg-test-project--installed-root-marker root))
         (primary-elc (expand-file-name "primary-sentinel.elc" root))
         (linked-elc (expand-file-name "linked-sentinel.elc" linked)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "Makefile" dorg-test-project--root)
                     (expand-file-name "Makefile" root))
          (with-temp-file (expand-file-name "fixture.el" root)
            (insert "(provide 'fixture)\n"))
          (dorg-test-project--command-output "git" "init" "-q" root)
          (dorg-test-project--git root "config" "user.email" "dorg-test@example.invalid")
          (dorg-test-project--git root "config" "user.name" "DOrg project test")
          (dorg-test-project--git root "add" "Makefile" "fixture.el")
          (dorg-test-project--git root "commit" "-qm" "fixture")
          ;; An ordinary, unmarked primary checkout retains normal cleanup.
          (with-temp-file primary-elc (insert "primary-bytecode\n"))
          (pcase-let ((`(,status . ,output)
                       (dorg-test-project--make-call root "clean-elc")))
            (ert-info (output)
              (should (zerop status))
              (should-not (file-exists-p primary-elc))))
          (with-temp-file marker (insert "primary-marker\n"))
          ;; A shared-Git-config implementation would observe this value from
          ;; the linked worktree.  The marker contract is strictly local.
          (dorg-test-project--git root "config" "dorg.installed-root" "true")
          (dorg-test-project--git root "worktree" "add" "--detach" linked "HEAD")
          (with-temp-file linked-elc (insert "linked-bytecode\n"))
          (pcase-let ((`(,status . ,output)
                       (dorg-test-project--make-call linked "clean-elc")))
            (ert-info (output)
              (should (zerop status))
              (should-not (file-exists-p linked-elc))
              (should (equal "primary-marker\n"
                             (dorg-test-project--file-contents marker)))
              (should-not (file-exists-p
                           (expand-file-name dorg-test-project--installed-root-marker
                                             linked)))))
          (dorg-test-project--git root "worktree" "remove" "--force" linked)
          (setq linked nil))
      (when (and linked (file-directory-p linked))
        (ignore-errors (dorg-test-project--git root "worktree" "remove" "--force" linked)))
      (delete-directory root t))))

(ert-deftest dorg-test-project-installed-root-packet-and-ci-stay-unmarked ()
  "The packet control and CI workflow keep installed-root state local.
The Python control uses an untracked marker fixture and proves bundle
reconstruction excludes it.  This source row additionally refuses a CI-side
marker exception or an unconnected packet control."
  (let ((packet-control
         (dorg-test-project--file-contents
          (expand-file-name "tests/test_suite_source_packet.py"
                            dorg-test-project--root)))
        (ci (dorg-test-project--file-contents
             (expand-file-name ".github/workflows/ci.yml"
                               dorg-test-project--root))))
    (should (string-match-p "test_untracked_installed_marker_never_enters_packet"
                            packet-control))
    (should (string-match-p "actions/checkout@v4" ci))
    (should-not (string-match-p
                 (regexp-quote dorg-test-project--installed-root-marker) ci))))

(ert-deftest dorg-test-project-install-root-marks-only-after-compile ()
  "install-root marks after successful compile and preserves failure state."
  (let ((root (make-temp-file "dorg-install-root-ordering-" t)))
    (unwind-protect
        (let* ((marker (dorg-test-project--installed-root-fixture root 'file))
               (stubs (dorg-test-project--installed-root-compile-stubs root))
               (emacs (car stubs))
               (recorder (cadr stubs)))
          ;; First, exercise the success edge from an explicitly unmarked root.
          (delete-file marker)
          (let ((process-environment
                 (cons (concat "DORG_INSTALLED_ROOT_RECORDER=" recorder)
                       process-environment)))
            (pcase-let ((`(,status . ,output)
                         (dorg-test-project--make-call root "install-root"
                                                        (concat "EMACS=" emacs))))
              (ert-info (output)
                (should (zerop status))
                (should (file-regular-p marker))
                (should (string-match-p "Marked installed root" output))
                (should (file-exists-p recorder)))))
          ;; A failure before the install-root recipe cannot create a marker.
          (delete-file marker)
          (let ((process-environment
                 (cons "DORG_INSTALLED_ROOT_COMPILE_STATUS=1"
                       (cons (concat "DORG_INSTALLED_ROOT_RECORDER=" recorder)
                             process-environment))))
            (pcase-let ((`(,status . ,_output)
                         (dorg-test-project--make-call root "install-root"
                                                        (concat "EMACS=" emacs))))
              (should-not (zerop status))
              (should-not (file-exists-p marker))))
          ;; Nor may a failed compile remove or overwrite an existing marker.
          (with-temp-file marker (insert "keep-me\n"))
          (let ((process-environment
                 (cons "DORG_INSTALLED_ROOT_COMPILE_STATUS=1"
                       (cons (concat "DORG_INSTALLED_ROOT_RECORDER=" recorder)
                             process-environment))))
            (pcase-let ((`(,status . ,_output)
                         (dorg-test-project--make-call root "install-root"
                                                        (concat "EMACS=" emacs))))
              (should-not (zerop status))
              (should (equal "keep-me\n"
                             (dorg-test-project--file-contents marker))))))
      (delete-directory root t))))

(provide 'dorg-project-tests)
;;; dorg-project-tests.el ends here
