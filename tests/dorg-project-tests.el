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
;;   - the ERT selector policy (`dorg-test-*' required, `dorg-perf-*' the
;;     one documented benchmark exception outside default `make test')
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
  "Return body lines of the first #+begin_example block after org HEADING.
HEADING is matched as an Org headline in DEVELOPMENT.org."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "DEVELOPMENT.org" dorg-test-project--root))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^\\*+ +" (regexp-quote heading) "[ \t]*$") nil t)
      (error "DEVELOPMENT.org: heading %S not found" heading))
    (unless (re-search-forward "^[ \t]*#\\+begin_example[ \t]*$" nil t)
      (error "DEVELOPMENT.org: no #+begin_example after %S" heading))
    (let ((start (line-beginning-position 2)))
      (unless (re-search-forward "^[ \t]*#\\+end_example[ \t]*$" nil t)
        (error "DEVELOPMENT.org: unterminated example block after %S" heading))
      (split-string
       (buffer-substring-no-properties start (line-beginning-position))
       "\n" t))))

(defun dorg-test-project--documented-el-symbols (heading)
  "Return symbols for every `dorg*.el' token in HEADING's example block."
  (let (out)
    (dolist (line (dorg-test-project--doc-example-block heading))
      (let ((start 0))
        (while (string-match "\\(dorg\\(?:-[a-z0-9-]+\\)?\\)\\.el\\b" line start)
          (push (intern (match-string 1 line)) out)
          (setq start (match-end 0)))))
    (delete-dups (nreverse out))))

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
       (format "Test files not required by dorg-tests-runner.el: %s. Add (require '...) so they run in `make test'."
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
    "dorg-bind-dolt")
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
`dorg-test-*' is the required default-suite namespace (SEL ?= ^dorg-test-);
`dorg-perf-*' is the one documented benchmark exception, excluded from
default `make test' and run via `make perf'.  Any other prefix would be
silently dropped from the default suite."
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

;;; ─── Test gate partition (fast / medium / full) ───────────────────────
;;
;; `dorg-test-gates.el' splits the suite into a quick Dolt-free FAST gate
;; and a Dolt/integration MEDIUM gate, with FULL `make test' unchanged as
;; the acceptance gate.  These guards prove the gates cannot silently
;; replace or shrink full coverage: the partition is exhaustive + disjoint,
;; each gate is a proper non-empty subset, the FAST allowlist is statically
;; Dolt-free, and the Makefile keeps `test' == `^dorg-test-' while ADDING
;; (not replacing) the fast/medium targets.

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
                      (ert-select-tests (dorg-test-gate-selector 'full) t)))
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

(ert-deftest dorg-test-project-gate-proper-non-empty-subsets ()
  "Neither FAST nor MEDIUM is empty or equal to the full suite.
A gate that matched nothing (useless) or everything (a silent alias of the
full acceptance suite) would defeat the point; pin both bounds."
  (let ((all (length (ert-select-tests (dorg-test-gate-selector 'full) t)))
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

(ert-deftest dorg-test-project-gate-make-targets-preserve-full-suite ()
  "Makefile keeps `test' == full `^dorg-test-' and ADDS fast/medium targets.
Proves the new gates are additive conveniences, not a silent replacement of
the acceptance suite: `make test' must still default its selector to
`^dorg-test-', and dedicated `test-fast'/`test-medium' targets must exist
and route through the gate runner."
  (let ((mk (dorg-test-project--makefile-text)))
    ;; Full acceptance selector unchanged (`SEL ?= ^dorg-test-').
    (should (string-match-p "^SEL[ \t]*\\?=[ \t]*\\^dorg-test-" mk))
    ;; Both new targets are declared.
    (should (string-match-p "^test-fast:" mk))
    (should (string-match-p "^test-medium:" mk))
    ;; And they run the gate entry point, not an arbitrary selector.
    (should (string-match-p "dorg-run-tests-batch-gate \"fast\"" mk))
    (should (string-match-p "dorg-run-tests-batch-gate \"medium\"" mk))
    ;; .PHONY lists them so they never collide with a file of the same name.
    (should (string-match-p "test-fast" mk))
    (should (string-match-p "test-medium" mk))))

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
it parses under `bash -n', uses strict mode, invokes `make install', finishes
with the `dorg doctor --install' verification, and honors the documented
configurable inputs."
  (let ((path (expand-file-name "install.sh" dorg-test-project--root)))
    (should (file-exists-p path))
    (when (executable-find "bash")
      (should (= 0 (call-process "bash" nil nil nil "-n" path))))
    (let ((text (dorg-test-project--file-contents path)))
      (should (string-match-p "set -euo pipefail" text))
      (should (string-match-p "make .*install" text))
      (should (string-match-p "doctor --install" text))
      (dolist (var '("DORG_REPO" "DORG_REF" "DORG_DIR" "BINDIR"))
        (should (string-match-p var text))))))

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
  "The shared scrub rules distinguish internal names from public identifiers.
Tracker brands use alphanumeric-or-underscore boundaries, preserving the B3
EMBEAD_* isolation contract.  Bare slugs have the exact one-segment tracker-id
shape; legitimate public tokens require a documented exact-token allowlist."
  (let ((known-symbols (dorg-test-project--known-dorg-token-table)))
    (dorg-test-project--load-public-scrub-source)
    (should (consp dorg-public-scrub-rules))
    (dolist (class '("tracker reference" "tracker brand"
                     "coordination brand" "agent identity" "private path"
                     "internal codename" "bare tracker slug"))
      (should (assoc class dorg-public-scrub-rules)))
    (dolist (control
             `(("tracker reference" . ,(concat "see b" "d:dorg-" "ab12"))
               ("tracker brand" . ,(concat "run em" "bd status"))
               ("tracker brand" . ,(concat "em" "bead-dir"))
               ("tracker brand" . ,(concat ".em" "bead/"))
               ("coordination brand" . ,(concat "Me" "lD handoff"))
               ("agent identity" . ,(concat "nav-" "ab12"))
               ("private path" . ,(concat "/Us" "ers/example/.me" "ld/lane"))
               ("internal codename" . ,(concat "Amb" "ient workload"))
               ("bare tracker slug" . ,(concat "dorg-" "ab12"))
               ("bare tracker slug" . ,(concat "dorg-" "ab12.foo3"))))
      (ert-info ((format "positive scrub control: %s" (car control)))
        (should (member (car control)
                        (dorg-public-scrub-classify-string
                         (cdr control) known-symbols)))))
    (dolist (control
             `(("B3 environment name" . ,(concat "EM" "BEAD_DIR"))
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

(provide 'dorg-project-tests)
;;; dorg-project-tests.el ends here
