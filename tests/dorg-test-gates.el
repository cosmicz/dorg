;;; dorg-test-gates.el --- Fast/medium/full test gate classification -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tiered test gates so controllers/drivers do not default to the full
;; ~6-minute `make test' loop after small changes:
;;
;;   - FAST   (`make test-fast')   — the Dolt-free unit suites.  No dolt
;;     binary, no sql-server (the runner skips even starting the shared
;;     server for this gate), so it is quick and immune to swarm Dolt
;;     contention.  For tracker-only or narrow pure-logic changes.
;;   - MEDIUM (`make test-medium') — every other `dorg-test-*' suite: the
;;     Dolt/sql-server/CLI/serve/transport integration tests.  For
;;     merge-fallout and backend-integration changes.
;;   - REPRESENTATIVE (`make test') — FAST plus named real-integration files;
;;     the bounded default for ordinary feedback.
;;   - FULL   (`make test-full')   — every project-owned ERT test, including
;;     `dorg-perf-*' and opt-in benchmark fixtures.  The release gate.
;;
;; Classification is by DEFINING FILE, attributed authoritatively with
;; `(symbol-file TEST 'ert--test)' — correct even for macro-generated tests
;; and independent of load order.  `dorg-test-gate-fast-files' is an
;; ALLOWLIST of files proven Dolt-free; everything else is MEDIUM by
;; default, so a newly added (possibly Dolt-touching) suite is never
;; silently admitted to the quick gate.  The partition is exhaustive and
;; disjoint by construction (MEDIUM == the regular `^dorg-test-' complement
;; of FAST); `dorg-project-tests.el' proves this, proves that the full gate
;; covers the project test inventory, and that each FAST file is statically
;; Dolt-free.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst dorg-test-gate-fast-files
  '("dorg-binding-isolation-tests"
    "dorg-dolt-server-evidence-tests"
    "dorg-file-lock-tests"
    "dorg-git-maintenance-tests"
    "dorg-git-sync-tests"
    "dorg-git-tests"
    "dorg-id-tests"
    "dorg-level-offset-tests"
    "dorg-project-tests"
    "dorg-ql-door-tests"
    "dorg-ql-ast-tests"
    "dorg-ql-eval-tests"
    "dorg-reload-tests"
    "dorg-sortkey-tests"
    "dorg-test-job-tests"
    "dorg-timestamp-tests")
  "Basenames of the Dolt-free `tests/dorg-*-tests.el' files in the FAST gate.
Every `dorg-test-*' test defined in one of these files runs in `make
test-fast'; all others run in `make test-medium'.  This is an ALLOWLIST:
a file belongs here only when proven free of the dolt binary and
sql-server (enforced by `dorg-test-project-gate-fast-files-are-dolt-free'
in `dorg-project-tests.el').  Any file NOT listed is MEDIUM by default, so
a newly added suite is never silently admitted to the quick gate.")

(defconst dorg-test-gate-regular-selector "^dorg-test-"
  "ERT selector for regular, non-benchmark DOrg tests.
FAST and MEDIUM partition exactly this selector; it deliberately remains
separate from the exhaustive FULL selector.")

(defconst dorg-test-gate-full-selector
  `(or ,dorg-test-gate-regular-selector "^dorg-perf-")
  "ERT selector for the exhaustive acceptance suite.
This selects every project test namespace, including `dorg-perf-*'.  The
Makefile's `test-full' target also enables DORG_BENCH for benchmarks that
are otherwise explicitly opt-in.")

(defconst dorg-test-gate-representative-dolt-files
  '("dorg-dolt-tests"
    "dorg-dolt-server-tests"
    "dorg-transport-tests")
  "Integration-file allowlist added to FAST for the default representative.
These files exercise the Dolt backend, SQL server lifecycle, and transport
path.  They are deliberately a finite, named list so a new integration file
cannot silently enlarge the bounded default; FULL retains all coverage.")

(defun dorg-test-gate--test-file-base (test)
  "Return the base name of the file defining ert TEST, or nil.
Uses `symbol-file' so attribution is correct for macro-generated tests
and independent of `require' order."
  (let ((file (symbol-file (ert-test-name test) 'ert--test)))
    (and file (file-name-base file))))

(defun dorg-test-gate-fast-p (test)
  "Return non-nil when ert TEST is defined in a FAST-gate file."
  (and (member (dorg-test-gate--test-file-base test)
               dorg-test-gate-fast-files)
       t))

(defun dorg-test-gate-representative-p (test)
  "Return non-nil when ERT TEST belongs in the default representative gate."
  (or (dorg-test-gate-fast-p test)
      (member (dorg-test-gate--test-file-base test)
              dorg-test-gate-representative-dolt-files)))

(defun dorg-test-gate-selector (gate)
  "Return the ERT selector for GATE.
GATE is one of `fast', `medium', `representative', or `full'.  FAST and
MEDIUM are exact complements within the regular `^dorg-test-' namespace.
REPRESENTATIVE contains FAST and the named integration-file allowlist.  FULL
includes regular and `dorg-perf-*' tests."
  (pcase gate
    ('fast   `(and ,dorg-test-gate-regular-selector
                   (satisfies dorg-test-gate-fast-p)))
    ('medium `(and ,dorg-test-gate-regular-selector
                   (not (satisfies dorg-test-gate-fast-p))))
    ('representative
     `(and ,dorg-test-gate-regular-selector
           (satisfies dorg-test-gate-representative-p)))
    ('full   dorg-test-gate-full-selector)
    (_ (error "Unknown test gate: %S (expected fast/medium/representative/full)"
              gate))))

(provide 'dorg-test-gates)
;;; dorg-test-gates.el ends here
