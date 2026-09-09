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
;;   - FULL   (`make test')        — the complete acceptance suite
;;     (`^dorg-test-'), unchanged.  The only gate that proves a release.
;;
;; Classification is by DEFINING FILE, attributed authoritatively with
;; `(symbol-file TEST 'ert--test)' — correct even for macro-generated tests
;; and independent of load order.  `dorg-test-gate-fast-files' is an
;; ALLOWLIST of files proven Dolt-free; everything else is MEDIUM by
;; default, so a newly added (possibly Dolt-touching) suite is never
;; silently admitted to the quick gate.  The partition is exhaustive and
;; disjoint by construction (MEDIUM == the `^dorg-test-' complement of
;; FAST); `dorg-project-tests.el' proves this and that each FAST file is
;; statically Dolt-free, so the gates can never silently replace or shrink
;; the full acceptance suite.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst dorg-test-gate-fast-files
  '("dorg-binding-isolation-tests"
    "dorg-git-sync-tests"
    "dorg-git-tests"
    "dorg-id-tests"
    "dorg-project-tests"
    "dorg-ql-ast-tests"
    "dorg-ql-eval-tests"
    "dorg-reload-tests"
    "dorg-sortkey-tests"
    "dorg-timestamp-tests")
  "Basenames of the Dolt-free `tests/dorg-*-tests.el' files in the FAST gate.
Every `dorg-test-*' test defined in one of these files runs in `make
test-fast'; all others run in `make test-medium'.  This is an ALLOWLIST:
a file belongs here only when proven free of the dolt binary and
sql-server (enforced by `dorg-test-project-gate-fast-files-are-dolt-free'
in `dorg-project-tests.el').  Any file NOT listed is MEDIUM by default, so
a newly added suite is never silently admitted to the quick gate.")

(defconst dorg-test-gate-full-selector "^dorg-test-"
  "ERT selector for the FULL acceptance suite.
Must match the Makefile `test' target default (`SEL ?= ^dorg-test-').
`dorg-perf-*' benchmarks are deliberately outside this namespace.")

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

(defun dorg-test-gate-selector (gate)
  "Return the ERT selector for GATE, one of `fast', `medium', `full'.
FAST and MEDIUM are scoped to the `^dorg-test-' namespace (so `dorg-perf-*'
is excluded as it is from the full suite) and are exact complements:
FULL == FAST ∪ MEDIUM, FAST ∩ MEDIUM == ∅."
  (pcase gate
    ('fast   `(and ,dorg-test-gate-full-selector
                   (satisfies dorg-test-gate-fast-p)))
    ('medium `(and ,dorg-test-gate-full-selector
                   (not (satisfies dorg-test-gate-fast-p))))
    ('full   dorg-test-gate-full-selector)
    (_ (error "Unknown test gate: %S (expected fast/medium/full)" gate))))

(provide 'dorg-test-gates)
;;; dorg-test-gates.el ends here
