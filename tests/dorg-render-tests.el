;;; dorg-render-tests.el --- Tests for dorg-render -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dorg Dolt-to-org renderer.
;; Most tests are integration tests that import an org buffer via
;; `dorg-parse', then render it back and compare structure.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-parse)
(require 'dorg-render)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-mode)
(require 'dorg-test-helpers)

;;; Helpers

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

(defmacro dorg-test-with-rendered (contents &rest body)
  "Import CONTENTS into a Dolt repo, render it, execute BODY.
Binds `dolt-dir' to the repo and `rendered' to the render output.
Skips if dolt is not available."
  (declare (indent 1))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       (let ((stmts (dorg-test--parse-to-sql ,contents)))
         (when stmts
           (dorg-sql-batch dolt-dir stmts)
           (dorg-commit dolt-dir "test import")))
       (let ((rendered (dorg-render dolt-dir)))
         ,@body))))

(defmacro dorg-render-tests--with-materialized-buffer (dolt-dir root-id &rest body)
  "Bind a temp org file to DOLT-DIR, materialize it, then run BODY.
ROOT-ID, when nil, is resolved from DOLT-DIR."
  (declare (indent 2) (debug (form form body)))
  `(let ((temp-base (make-temp-file "dorg-render-materialize-" t)))
     (unwind-protect
         (let* ((org-file (expand-file-name "render.org" temp-base))
                (dorg--registry nil))
           (with-temp-file org-file)
           (let ((rid (or ,root-id (dorg-render--resolve-root-id ,dolt-dir))))
             (dorg--store-registration
              (file-truename org-file)
              (make-dorg-registration
               :backend (dorg-backend-dolt ,dolt-dir rid)
               :file (file-truename org-file))))
           (with-temp-buffer
             (org-mode)
             (set-visited-file-name org-file t)
             (dorg-materialize (current-buffer))
             ,@body)))
      (delete-directory temp-base t)))

;;; Integration Tests

(ert-deftest dorg-test-render-empty ()
  "Empty buffer renders with DORG_* keywords but no headings."
  (dorg-test-with-rendered ""
    (should (string-match-p "^#\\+DORG_DOLT_DIR:" rendered))
    (should (string-match-p "^#\\+DORG_HASH:" rendered))
    (should-not (string-match-p "^\\*" rendered))))

(ert-deftest dorg-test-render-single-heading ()
  "Single heading renders with stars and title."
  (dorg-test-with-rendered "* Hello World\n"
    (should (string-match-p "^\\* Hello World" rendered))))

(ert-deftest dorg-test-render-heading-with-todo ()
  "Heading with TODO keyword renders correctly."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-rendered "* TODO Fix the bug\n"
      (should (string-match-p "^\\* TODO Fix the bug" rendered)))))

(ert-deftest dorg-test-render-heading-with-priority ()
  "Heading with priority renders correctly."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-rendered "* TODO [#A] Important\n"
      (should (string-match-p "^\\* TODO \\[#A\\] Important" rendered)))))

(ert-deftest dorg-test-render-heading-with-tags ()
  "Heading with tags renders tags at end of line."
  (dorg-test-with-rendered "* Tagged heading  :foo:bar:\n"
    (should (string-match-p "^\\* Tagged heading.*:foo:bar:" rendered))))

(ert-deftest dorg-test-render-nested-headings ()
  "Nested headings render with correct star levels."
  (dorg-test-with-rendered "* Parent\n** Child\n*** Grandchild\n"
    (should (string-match-p "^\\* Parent" rendered))
    (should (string-match-p "^\\*\\* Child" rendered))
    (should (string-match-p "^\\*\\*\\* Grandchild" rendered))))

(ert-deftest dorg-test-render-sibling-order ()
  "Sibling headings render in original order."
  (dorg-test-with-rendered "* First\n* Second\n* Third\n"
    (let ((first-pos (string-match "First" rendered))
          (second-pos (string-match "Second" rendered))
          (third-pos (string-match "Third" rendered)))
      (should first-pos)
      (should second-pos)
      (should third-pos)
      (should (< first-pos second-pos))
      (should (< second-pos third-pos)))))

(ert-deftest dorg-test-render-file-keyword ()
  "File-level keywords render before any heading."
  (dorg-test-with-rendered "#+TITLE: My Document\n* Heading\n"
    (should (string-match-p "#\\+TITLE: My Document" rendered))
    (let ((kw-pos (string-match "#\\+TITLE" rendered))
          (h-pos (string-match "^\\* Heading" rendered)))
      (should (and kw-pos h-pos (< kw-pos h-pos))))))

(ert-deftest dorg-test-render-multiple-keywords ()
  "Multiple file keywords all render."
  (dorg-test-with-rendered "#+TITLE: Doc\n#+AUTHOR: Me\n#+STARTUP: showall\n"
    (should (string-match-p "#\\+TITLE: Doc" rendered))
    (should (string-match-p "#\\+AUTHOR: Me" rendered))
    (should (string-match-p "#\\+STARTUP: showall" rendered))))

(ert-deftest dorg-test-render-body-text ()
  "Body text under a heading renders."
  (dorg-test-with-rendered "* Heading\nSome body text here.\n"
    (should (string-match-p "^\\* Heading" rendered))
    (should (string-match-p "Some body text here" rendered))))

(ert-deftest dorg-test-render-property-drawer ()
  "Property drawer renders with :PROPERTIES:/:END: and entries."
  (dorg-test-with-rendered "* Heading\n:PROPERTIES:\n:ID: abc-123\n:CUSTOM: value\n:END:\n"
    (should (string-match-p ":PROPERTIES:" rendered))
    (should (string-match-p ":ID: abc-123" rendered))
    (should (string-match-p ":CUSTOM: value" rendered))
    (should (string-match-p ":END:" rendered))))

(ert-deftest dorg-test-render-property-order-plain ()
  "Plain parse -> model render preserves a NON-alphabetical PROPERTIES drawer
order (no DB involved); a naive key-sort would reorder it."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* H\n:PROPERTIES:\n:ZED: 1\n:ALPHA: 2\n:MID: 3\n:END:\n")
    (let* ((h (cl-find-if (lambda (x) (> (dorg-heading-level x) 0))
                          (dorg-parse-result-headings (dorg-parse-buffer))))
           (drawer (cl-find-if
                    (lambda (it) (and (cl-typep it 'dorg-drawer-item)
                                      (equal (dorg-drawer-item-name it) "PROPERTIES")))
                    (dorg-heading-items h))))
      ;; Parsed list is in source order, not alphabetical.
      (should (equal '(("ZED" . "1") ("ALPHA" . "2") ("MID" . "3"))
                     (dorg-drawer-item-properties drawer)))
      (setf (dorg-heading-id h) 7)
      (let ((rendered (dorg-model-render-heading-section h)))
        ;; Re-rendered drawer keeps source order ZED, ALPHA, MID …
        (should (string-match-p ":ZED: 1\n:ALPHA: 2\n:MID: 3" rendered))
        ;; … and is NOT the alphabetical order (non-vacuity).
        (should-not (string-match-p ":ALPHA: 2\n:MID: 3\n:ZED: 1" rendered))))))

(ert-deftest dorg-test-render-property-order-from-db ()
  "Render-from-DB preserves a NON-alphabetical PROPERTIES drawer order
\(parse -> DB -> `dorg-render'); non-vacuous — source order != sorted order."
  (dorg-test-with-rendered "* H\n:PROPERTIES:\n:ZED: 1\n:ALPHA: 2\n:MID: 3\n:END:\n"
    (should (string-match-p ":ZED: 1\n:ALPHA: 2\n:MID: 3" rendered))
    (should-not (string-match-p ":ALPHA: 2\n:MID: 3\n:ZED: 1" rendered))))

(ert-deftest dorg-test-render-generic-drawer ()
  "Generic drawer renders with name and lines."
  (dorg-test-with-rendered "* Heading\n:LOGBOOK:\nLine one\nLine two\n:END:\n"
    (should (string-match-p ":LOGBOOK:" rendered))
    (should (string-match-p "Line one" rendered))
    (should (string-match-p "Line two" rendered))
    (should (string-match-p ":END:" rendered))))

(ert-deftest dorg-test-render-planning ()
  "Planning line renders after heading."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-rendered "* TODO Task\nSCHEDULED: <2026-04-09 Thu>\n"
      (should (string-match-p "SCHEDULED: <2026-04-09" rendered)))))

(ert-deftest dorg-test-render-roundtrip-structure ()
  "Parse+render preserves heading structure and titles."
  (dorg-test-with-rendered "* Alpha\n** Alpha-1\n** Alpha-2\n* Beta\n"
    ;; Re-parse the rendered output and count headings
    (with-temp-buffer
      (insert rendered)
      (org-mode)
      (let ((headings nil))
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (h)
            (push (cons (org-element-property :level h)
                        (org-element-property :raw-value h))
                  headings)))
        (setq headings (nreverse headings))
        (should (= (length headings) 4))
        (should (equal headings
                       '((1 . "Alpha")
                         (2 . "Alpha-1")
                         (2 . "Alpha-2")
                         (1 . "Beta"))))))))

(ert-deftest dorg-test-render-roundtrip-preamble ()
  "Preamble text survives round-trip."
  (dorg-test-with-rendered "Preamble paragraph.\n\n* Heading\n"
    (should (string-match-p "Preamble paragraph" rendered))
    (let ((preamble-pos (string-match "Preamble paragraph" rendered))
          (heading-pos (string-match "^\\* Heading" rendered)))
      (should (and preamble-pos heading-pos (< preamble-pos heading-pos))))))

(ert-deftest dorg-test-render-special-chars ()
  "Text with single quotes survives round-trip."
  (dorg-test-with-rendered "* It's a test\n"
    (should (string-match-p "It's a test" rendered))))

;;; DORG_ID Injection Tests

(ert-deftest dorg-test-render-dorg-id-injected ()
  "Non-root headings get a :DORG_ID: in their PROPERTIES drawer."
  (dorg-test-with-rendered "* Hello\n"
    (should (string-match-p ":PROPERTIES:" rendered))
    (should (string-match-p ":DORG_ID: [0-9]+" rendered))
    (should (string-match-p ":END:" rendered))))

(ert-deftest dorg-test-render-dorg-id-in-existing-drawer ()
  "Existing PROPERTIES drawer gets DORG_ID prepended."
  (dorg-test-with-rendered
      "* H\n:PROPERTIES:\n:CUSTOM: value\n:END:\n"
    (should (string-match-p ":PROPERTIES:" rendered))
    ;; DORG_ID should appear before CUSTOM
    (let ((dorg-pos (string-match ":DORG_ID:" rendered))
          (custom-pos (string-match ":CUSTOM:" rendered)))
      (should (and dorg-pos custom-pos (< dorg-pos custom-pos))))))

(ert-deftest dorg-test-render-dorg-id-not-on-root ()
  "The file root (level 0) does not render a DORG_ID drawer."
  (dorg-test-with-rendered "#+TITLE: Top\n"
    ;; The preamble is under the root heading; no PROPERTIES should
    ;; appear for the synthetic root (no heading line means no drawer
    ;; should be synthesized).
    (should-not (string-match-p ":PROPERTIES:" rendered))
    (should-not (string-match-p ":DORG_ID:" rendered))))

(ert-deftest dorg-test-render-dorg-id-stable-roundtrip ()
  "Round-trip: parse → render → re-parse preserves DORG_IDs."
  (dorg-test-with-rendered "* Keep\n"
    ;; Extract the DORG_ID from render output
    (should (string-match ":DORG_ID: \\([0-9]+\\)" rendered))
    (let ((first-id (string-to-number (match-string 1 rendered))))
      ;; Re-parse the rendered output — the DORG_ID should be picked up
      (with-temp-buffer
        (insert rendered)
        (org-mode)
        (let* ((result (dorg-parse-buffer))
               (headings (dorg-parse-result-headings result))
               (keep-h (dorg-test--find-heading headings "Keep")))
          (should keep-h)
          (should (= (dorg-heading-id keep-h) first-id)))))))

(ert-deftest dorg-test-render-dorg-id-after-planning ()
  "Synthesized PROPERTIES drawer appears after planning line."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-rendered
        "* TODO Task\nSCHEDULED: <2026-04-09 Thu>\n"
      (let ((sched-pos (string-match "SCHEDULED:" rendered))
            (props-pos (string-match ":PROPERTIES:" rendered)))
        (should (and sched-pos props-pos (< sched-pos props-pos)))))))

(ert-deftest dorg-test-render-generic-body-properties-drawer-survives-load ()
  "A generic body :PROPERTIES: drawer survives the Dolt load->render path.
Regression (P0 local-identity corruption): `dorg-model-load-headings'
selected a drawer's representation by NAME, rebuilding EVERY drawer named
PROPERTIES from `property_entries'.  But org parses a :PROPERTIES: block
appearing AFTER body content as a generic drawer, which is stored as raw
`drawer_lines'.  Rebuilding it from the (empty) property table discarded
its lines — including any literal :DORG_ID: — so the block vanished on the
next render.  The loader must pick the representation from the detail table
that actually holds the drawer's rows (property_entries vs drawer_lines),
mirroring the store-side split in `dorg-sql-gen--drawer-detail-inserts'."
  (dorg-test-with-rendered
      "* H\nbody text\n:PROPERTIES:\n:DORG_ID: 123\n:FOO: bar\n:END:\n"
    ;; Loader (isolated): the generic body drawer round-trips as raw LINES,
    ;; not properties — a faithful inverse of how it was stored.
    (let ((tree (dorg-model-load-tree
                 dolt-dir (dorg-render-tests--root-id dolt-dir)))
          (h nil))
      (maphash (lambda (_ x) (when (equal (dorg-heading-title x) "H")
                               (setq h x)))
               tree)
      (should h)
      (let ((drawer (car (dorg-test--items-of-type h 'dorg-drawer-item))))
        (should drawer)
        (should (equal (dorg-drawer-item-lines drawer)
                       '(":DORG_ID: 123" ":FOO: bar")))
        (should (null (dorg-drawer-item-properties drawer)))))
    ;; Render: the generic drawer's own lines reach the output verbatim.  Its
    ;; literal :DORG_ID: 123 is body content and sits AFTER the body text; the
    ;; heading's real identity is a distinct injected drawer before the body.
    (should (string-match-p ":FOO: bar" rendered))
    (should (string-match-p ":DORG_ID: 123" rendered))
    (should (string-match-p "body text" rendered))
    (should (< (string-match "body text" rendered)
               (string-match ":DORG_ID: 123" rendered)))))

;;; Multi-file Scoping

(ert-deftest dorg-test-render-multi-file-scoped ()
  "Render with explicit root-id isolates files."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      ;; Import file 1 (root-id 100)
      (let ((stmts-1 (dorg-test--parse-to-sql "* One\n" 100)))
        (dorg-sql-batch dolt-dir stmts-1))
      ;; Import file 2 (root-id 200)
      (let ((stmts-2 (dorg-test--parse-to-sql "* Two\n" 200)))
        (dorg-sql-batch dolt-dir stmts-2))
      (dorg-commit dolt-dir "two files")
      ;; Auto-detect should fail with multiple roots
      (should-error (dorg-render dolt-dir))
      ;; Explicit root-id 100 renders only file 1
      (let ((out-1 (dorg-render dolt-dir 100)))
        (should (string-match-p "\\* One" out-1))
        (should-not (string-match-p "\\* Two" out-1)))
      ;; Explicit root-id 200 renders only file 2
      (let ((out-2 (dorg-render dolt-dir 200)))
        (should (string-match-p "\\* Two" out-2))
        (should-not (string-match-p "\\* One" out-2))))))

(ert-deftest dorg-test-render-no-root-errors ()
  "Rendering an empty repo auto-detects and errors."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (should-error (dorg-render dolt-dir)))))

(ert-deftest dorg-test-render-to-buffer ()
  "`dorg-materialize' populates a registered buffer in org-mode."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql "* Hello\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (dorg-render-tests--with-materialized-buffer dolt-dir nil
        (should (eq major-mode 'org-mode))
        (should (string-match-p "^\\* Hello" (buffer-string)))))))

(ert-deftest dorg-test-render-to-buffer-returns-clean-tree ()
  "`dorg-render--tree-to-text' leaves the loaded tree clean.
Render-only `#+DORG_DOLT_DIR:'/`#+DORG_HASH:' keyword items must land on a
copy of the root, never on the loaded structs that lifecycle code reuses as
canonical base headings."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql "* Root\n** Child\nbody\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (let* ((root-id (dorg-render--resolve-root-id dolt-dir))
             (commit (dorg-head-commit dolt-dir))
             (tree (dorg-model-load-tree dolt-dir root-id commit))
             (out (dorg-render--tree-to-text tree root-id commit dolt-dir))
             (root nil)
             (loaded-root nil)
             (loaded (dorg-model-load-tree dolt-dir root-id commit)))
        (should (string-match-p "#\\+DORG_HASH:" out))
        (should (string-match-p "#\\+DORG_DOLT_DIR:" out))
        (maphash (lambda (_id h)
                   (when (= (dorg-heading-level h) 0) (setq root h)))
                 tree)
        (maphash (lambda (_id h)
                   (when (= (dorg-heading-level h) 0) (setq loaded-root h)))
                 loaded)
        (should root)
        (dolist (it (dorg-heading-items root))
          (when (cl-typep it 'dorg-keyword-item)
            (should-not (member (dorg-keyword-item-keyword it)
                                '("DORG_HASH" "DORG_DOLT_DIR")))))
        (should (equal (length (dorg-heading-items root))
                       (length (dorg-heading-items loaded-root))))))))

(ert-deftest dorg-test-render-to-buffer-no-replace-buffer-contents ()
  "`dorg-materialize' overwrites the buffer linearly, never via `replace-buffer-contents'.
Regression.  The whole-root materialize path used `replace-buffer-contents',
whose Myers diff (C `compareseq', O(N*D)) hung
for >9 min on the ~9.2 MB buffer during the FIRST Dolt-backed
write.  This is the authoritative CLI write path proven by the
probe.  The render must replace the buffer with a linear erase+insert.
Advise `replace-buffer-contents' to signal so any reintroduction is caught
deterministically, with no multi-megabyte fixture required."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql "* Fresh\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (let ((rbc-called nil))
        (cl-letf (((symbol-function 'replace-buffer-contents)
                   (lambda (&rest _)
                     (setq rbc-called t)
                     (error "replace-buffer-contents must not run on render-to-buffer"))))
          (with-temp-buffer
            ;; Pre-existing stale content that the materialize must overwrite.
            (insert "* Stale heading\nold content to be replaced\n")
            (let ((temp-base (make-temp-file "dorg-render-materialize-" t))
                  (dorg--registry nil))
              (unwind-protect
                (let ((org-file (expand-file-name "render.org" temp-base)))
                    (with-temp-file org-file)
                    (dorg--store-registration
                     (file-truename org-file)
                     (make-dorg-registration
                      :backend (dorg-backend-dolt
                                dolt-dir (dorg-render--resolve-root-id dolt-dir))
                      :file (file-truename org-file)))
                  (set-visited-file-name org-file t)
                  (dorg-materialize (current-buffer)))
                (delete-directory temp-base t)))
            ;; The pathological diff path was never entered.
            (should-not rbc-called)
            ;; Fresh DB content fully replaced the stale buffer text.
            (should (string-match-p "^\\* Fresh" (buffer-string)))
            (should-not (string-match-p "Stale" (buffer-string)))))))))

(ert-deftest dorg-test-render-to-buffer-org-cache-silent-reset ()
  "`dorg-materialize' suppresses Org cache churn on already-Org buffers."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql
                    "* Fresh\n:PROPERTIES:\n:BEAD_ID: fresh-1\n:END:\nbody\n* Next\n:PROPERTIES:\n:BEAD_ID: fresh-2\n:END:\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (with-temp-buffer
        (org-mode)
        (setq-local org-element-use-cache t)
        ;; Warm a real pre-render cache over stale content so the render must
        ;; leave downstream scans with either a coherent cache or no cache.
        (insert "* Stale\n:PROPERTIES:\n:BEAD_ID: stale\n:END:\n")
        (goto-char (point-min))
        (org-element-at-point)
        (let ((cache-after-change-count 0)
              (cache-reset-count 0)
              (cache-reset-args nil)
              (persist-register-count 0)
              (orig-cache-after-change (symbol-function 'org-element--cache-after-change))
              (orig-cache-reset (symbol-function 'org-element-cache-reset))
              (orig-persist-register (symbol-function 'org-persist-register)))
          (cl-letf (((symbol-function 'org-element--cache-after-change)
                     (lambda (&rest args)
                       (cl-incf cache-after-change-count)
                       (apply orig-cache-after-change args)))
                    ((symbol-function 'org-element-cache-reset)
                     (lambda (&rest args)
                       (cl-incf cache-reset-count)
                       (push args cache-reset-args)
                       (apply orig-cache-reset args)))
                    ((symbol-function 'org-persist-register)
                     (lambda (&rest args)
                       (cl-incf persist-register-count)
                       (apply orig-persist-register args))))
            (let* ((root-id (dorg-render--resolve-root-id dolt-dir))
                   (commit (dorg-head-commit dolt-dir))
                   (tree (dorg-model-load-tree dolt-dir root-id commit)))
              (dorg-render--replace-buffer-text
               (dorg-render--tree-to-text tree root-id commit dolt-dir))
              (unless (derived-mode-p 'org-mode)
                (org-mode)))
            (should (= 0 cache-after-change-count))
            (should (= 1 cache-reset-count))
            (should (equal '((nil t)) cache-reset-args))
            (should (= 0 persist-register-count))
            (let (seen)
              (org-map-entries
               (lambda ()
                 (push (cons (org-get-heading t t t t)
                             (org-entry-get (point) "BEAD_ID"))
                       seen)))
              (should (equal '(("Fresh" . "fresh-1") ("Next" . "fresh-2"))
                             (nreverse seen))))))))))

(ert-deftest dorg-test-render-to-buffer-silent-splice-keeps-downstream-state ()
  "Silent render splice still leaves downstream DOrg/Downstream tracker scans coherent."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (require 'dorg-mode)
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql
                    "* Fresh\n:PROPERTIES:\n:BEAD_ID: fresh-1\n:END:\nbody\n* Next\n:PROPERTIES:\n:BEAD_ID: fresh-2\n:END:\nbody\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (with-temp-buffer
        (org-mode)
        (setq-local org-element-use-cache t)
        (insert "* Stale\n:PROPERTIES:\n:BEAD_ID: stale\n:END:\n")
        (goto-char (point-min))
        (org-element-at-point)
        (set-buffer-modified-p nil)
        (let* ((tick-before-render (buffer-chars-modified-tick))
               (root-id (dorg-render--resolve-root-id dolt-dir))
               (commit (dorg-head-commit dolt-dir))
               (tree (dorg-model-load-tree dolt-dir root-id commit)))
          (dorg-render--replace-buffer-text
           (dorg-render--tree-to-text tree root-id commit dolt-dir))
          (unless (derived-mode-p 'org-mode)
            (org-mode))
          (let ((tick-after-render (buffer-chars-modified-tick))
                fresh-1-id)
          (should (> tick-after-render tick-before-render))
          (should (buffer-modified-p))
          ;; Surviving consumers build neutral projection markers directly.
          (let ((markers (dorg-projection-scan-markers))
                ids)
            (org-map-entries
             (lambda ()
               (when-let ((bid (org-entry-get (point) "BEAD_ID")))
                 (let ((id (string-to-number (org-entry-get (point) "DORG_ID"))))
                   (push (cons bid id) ids)
                   (when (equal bid "fresh-1")
                     (setq fresh-1-id id))
                   (should (markerp (gethash id markers)))))))
            (should (equal '(("fresh-1" . t) ("fresh-2" . t))
                           (mapcar (lambda (pair)
                                     (cons (car pair) (integerp (cdr pair))))
                                   (nreverse ids)))))
          ;; Downstream tracker signature refresh is also an explicit post-render
          ;; `org-map-entries' scan over BEAD_ID headings.
          (cl-labels ((collect-sigs ()
                        (let (sigs)
                          (org-map-entries
                           (lambda ()
                             (when-let ((bid (org-entry-get (point) "BEAD_ID")))
                               (let ((beg (point))
                                     (end (save-excursion
                                            (org-end-of-subtree t)
                                            (point))))
                                 (push (cons bid
                                             (md5 (buffer-substring-no-properties
                                                   beg end)))
                                       sigs)))))
                          (nreverse sigs))))
            (let ((before-edit-sigs (collect-sigs)))
              (should (equal '("fresh-1" "fresh-2")
                             (mapcar #'car before-edit-sigs)))
              ;; A later DOrg-owned splice still invalidates only the changed
              ;; heading's downstream signature and releases its capability.
              (dorg-projection-with-programmatic-changes
                (goto-char (point-min))
                (re-search-forward "^\\* Fresh")
                (end-of-line)
                (insert " edited"))
              (should-not (dorg-projection-programmatic-change-p))
              (should fresh-1-id)
              (let ((after-edit-sigs (collect-sigs)))
                (should-not (equal (cdr (assoc "fresh-1" before-edit-sigs))
                                   (cdr (assoc "fresh-1" after-edit-sigs))))
                (should (equal (cdr (assoc "fresh-2" before-edit-sigs))
                               (cdr (assoc "fresh-2" after-edit-sigs)))))))))))))

(ert-deftest dorg-test-render-commented-heading ()
  "COMMENT prefix round-trips through parse and render."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((stmts (dorg-test--parse-to-sql "* COMMENT Hidden\n* Visible\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "init"))
      (let ((text (dorg-render dolt-dir)))
        (should (string-match-p "^\\* COMMENT Hidden" text))
        (should (string-match-p "^\\* Visible" text))))))

;;; Idempotency

(ert-deftest dorg-test-render-idempotent-roundtrip ()
  "Render is idempotent: render₁ → parse → store → render₂ = render₁.
Uses a non-trivial fixture covering multiple element types.
Strips DORG_DOLT_DIR and DORG_HASH before comparing since each repo
has a different path and commit hash."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((fixture (concat
                    "#+TITLE: Roundtrip Test\n"
                    "#+AUTHOR: Test\n"
                    "Preamble paragraph.\n"
                    "* TODO [#A] First heading  :foo:bar:\n"
                    "SCHEDULED: <2026-04-09 Thu>\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM: value\n"
                    ":END:\n"
                    "Body text here.\n"
                    "** COMMENT Hidden child\n"
                    "** Sub heading\n"
                    ":LOGBOOK:\n"
                    "Log entry one\n"
                    ":END:\n"
                    "- list item one\n"
                    "- list item two\n"
                    "* Second heading\n"
                    "#+BEGIN_SRC emacs-lisp\n"
                    "(message \"hello\")\n"
                    "#+END_SRC\n"
                    "| a | b |\n"
                    "| 1 | 2 |\n")))
      ;; Two separate databases for cycle 1 and cycle 2
      (dorg-test--with-temp-db dolt-dir-1
        (dorg-test--with-temp-db dolt-dir-2
          (let* ((org-todo-keywords-1 '("TODO" "DONE"))
                 (strip-dorg-kw (lambda (s)
                                  (replace-regexp-in-string
                                   "^#\\+DORG_[A-Z_]+:.*\n" "" s)))
                 ;; Cycle 1: input → parse → store → render₁
                 (render-1
                  (progn
                    (let ((stmts (dorg-test--parse-to-sql fixture)))
                      (dorg-sql-batch dolt-dir-1 stmts)
                      (dorg-commit dolt-dir-1 "cycle 1"))
                    (dorg-render dolt-dir-1)))
                 ;; Cycle 2: render₁ → parse → store → render₂
                 (render-2
                  (progn
                    (let ((stmts (dorg-test--parse-to-sql render-1)))
                      (dorg-sql-batch dolt-dir-2 stmts)
                      (dorg-commit dolt-dir-2 "cycle 2"))
                    (dorg-render dolt-dir-2))))
            (should (string= (funcall strip-dorg-kw render-1)
                              (funcall strip-dorg-kw render-2)))))))))

;;; Blank-line (post_blank) preservation

(ert-deftest dorg-test-render-blank-lines-between-paragraphs ()
  "Blank lines between paragraphs under a heading are preserved."
  (dorg-test-with-rendered
      "* Heading\nFirst paragraph.\n\nSecond paragraph.\n"
    (should (string-match-p "First paragraph\\.\n\nSecond paragraph\\." rendered))))

(ert-deftest dorg-test-render-blank-lines-between-headings ()
  "Blank lines between sibling headings are preserved."
  (dorg-test-with-rendered
      "* Alpha\nBody.\n\n* Beta\n"
    (let ((alpha-end (string-match "^\\* Beta" rendered)))
      (should alpha-end)
      ;; There should be a blank line before * Beta
      (should (string-match-p "\n\n\\* Beta" rendered)))))

(ert-deftest dorg-test-render-no-blank-line-between-items ()
  "Items with zero post-blank have no blank line between them."
  (dorg-test-with-rendered
      "* Heading\n#+BEGIN_SRC elisp\n(+ 1 2)\n#+END_SRC\nImmediate text.\n"
    ;; No blank line between src block and text
    (should (string-match-p "END_SRC\n.*text\\." rendered))
    (should-not (string-match-p "END_SRC\n\n" rendered))))

(ert-deftest dorg-test-render-multiple-blank-lines ()
  "Two blank lines between paragraphs are preserved."
  (dorg-test-with-rendered
      "* Heading\nFirst.\n\n\nSecond.\n"
    (should (string-match-p "First\\.\n\n\nSecond\\." rendered))))

(ert-deftest dorg-test-render-blank-line-after-keyword ()
  "Blank line between a keyword and body text is preserved."
  (dorg-test-with-rendered
      "#+TITLE: Test\n\n* Heading\n"
    (should (string-match-p "TITLE: Test\n\n\\* Heading" rendered))))

;;; Orphan Heading Detection

(ert-deftest dorg-test-render-orphan-heading-warns ()
  "Render warns about orphan headings with broken parent_id."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((warned nil))
        ;; Seed a valid root + child
        (let ((stmts (dorg-test--parse-to-sql "* Valid\nBody.\n")))
          (dorg-sql-batch dolt-dir stmts)
          (dorg-commit dolt-dir "seed"))
        ;; Insert an orphan heading with broken parent_id
        (dorg-sql-batch dolt-dir
          '("SET FOREIGN_KEY_CHECKS=0"
            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9999, 1, 88888, X'FF', 1, FALSE, 'Orphan', 0)"
            "SET FOREIGN_KEY_CHECKS=1"))
        (dorg-commit dolt-dir "add orphan")
        ;; Render — should warn, not error
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type msg &rest _args)
                     (setq warned msg))))
          (let ((rendered (dorg-render dolt-dir)))
            ;; Orphan should NOT appear as a heading in output
            (should-not (string-match-p "^\\* Orphan" rendered))
            ;; Warning should mention the orphan ID
            (should warned)
            (should (string-match-p "9999" warned))
            (should (string-match-p "dorg-doctor" warned))))))))

(ert-deftest dorg-test-render-cycle-and-island-warn ()
  "Render warns about cyclic/disconnected headings it would silently omit.
Their immediate parents all exist, so the old immediate-parent check
missed them; true root reachability must report them instead."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (let ((warned nil))
        ;; Seed a valid root (root_id 1) + child.
        (let ((stmts (dorg-test--parse-to-sql "* Valid\nBody.\n")))
          (dorg-sql-batch dolt-dir stmts)
          (dorg-commit dolt-dir "seed"))
        ;; A parent cycle (9001 <-> 9002) and a disconnected island
        ;; (9003 -> 9004 -> NULL), all in root 1, all with existing
        ;; immediate parents.  None are root-reachable.
        (dorg-sql-batch dolt-dir
          '("SET FOREIGN_KEY_CHECKS=0"
            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9001, 1, 9002, X'F1', 1, FALSE, 'CycA', 0)"
            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9002, 1, 9001, X'F2', 1, FALSE, 'CycB', 0)"
            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9003, 1, 9004, X'F3', 1, FALSE, 'IslandA', 0)"
            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9004, 1, NULL, X'F4', 2, FALSE, 'IslandB', 0)"
            "SET FOREIGN_KEY_CHECKS=1"))
        (dorg-commit dolt-dir "add cycle and island")
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type msg &rest _args)
                     (setq warned msg))))
          (let ((rendered (dorg-render dolt-dir)))
            ;; None of the unreachable headings appear in output.
            (should-not (string-match-p "CycA\\|CycB\\|IslandA\\|IslandB" rendered))
            ;; Warning fires, names all four ids, classifies, points to doctor.
            (should warned)
            (dolist (id '("9001" "9002" "9003" "9004"))
              (should (string-match-p id warned)))
            (should (string-match-p "cycle" warned))
            (should (string-match-p "disconnected" warned))
            (should (string-match-p "dorg-doctor" warned))))))))

;;; DORG Keyword Preservation

(ert-deftest dorg-test-render-includes-dorg-dir ()
  "Render output includes #+DORG_DOLT_DIR: pointing to the actual repo."
  (dorg-test-with-rendered "* Hello\nBody.\n"
    (should (string-match-p "^#\\+DORG_DOLT_DIR:" rendered))
    (should (string-match-p (regexp-quote (file-truename dolt-dir)) rendered))))

(ert-deftest dorg-test-render-includes-dorg-hash ()
  "Render output includes #+DORG_HASH: with current commit."
  (dorg-test-with-rendered "* Hello\nBody.\n"
    (should (string-match-p "^#\\+DORG_HASH:" rendered))
    (let ((commit (dorg-head-commit dolt-dir)))
      (should (string-match-p (regexp-quote commit) rendered)))))

;;; Partial Materialized View Primitives — pure render helpers (no dolt)

(ert-deftest dorg-test-island-ancestor-context-headline-only ()
  "Ancestor context emits heading lines only: skips level-0 root, no
bodies, no synthetic DORG_ID drawers."
  (let ((root (make-dorg-heading :id 1 :parent-id nil :sort-key (string 1)
                                 :level 0 :level-offset 0 :title "ROOT"))
        (a (make-dorg-heading :id 2 :parent-id 1 :sort-key (string 1)
                              :level 1 :level-offset 1 :title "A"
                              :items (list (dorg-text-item :text "A body."))))
        (hash (make-hash-table :test 'eql)))
    (puthash 1 root hash)
    (puthash 2 a hash)
    (let ((out (dorg-render--render-ancestor-context '(1 2) hash)))
      (should (string-match-p "^\\* A" out))
      (should-not (string-match-p ":PROPERTIES:" out))
      (should-not (string-match-p ":DORG_ID:" out))
      (should-not (string-match-p "A body\\." out))
      (should-not (string-match-p "ROOT" out)))))

(ert-deftest dorg-test-island-ancestor-context-empty ()
  "No non-root ancestors yields the empty string."
  (let ((root (make-dorg-heading :id 1 :parent-id nil :sort-key (string 1)
                                 :level 0 :level-offset 0 :title "ROOT"))
        (hash (make-hash-table :test 'eql)))
    (puthash 1 root hash)
    (should (equal "" (dorg-render--render-ancestor-context '(1) hash)))
    (should (equal "" (dorg-render--render-ancestor-context '() hash)))))

(ert-deftest dorg-test-island-subtree-renders-full ()
  "Island subtree renders the node line, its body, and descendants."
  (let ((b (make-dorg-heading :id 10 :parent-id 1 :sort-key (string 1)
                              :level 1 :level-offset 1 :title "B"
                              :items (list (dorg-text-item :text "B body."))))
        (c (make-dorg-heading :id 11 :parent-id 10 :sort-key (string 1)
                              :level 2 :level-offset 1 :title "C"))
        (hash (make-hash-table :test 'eql)))
    (puthash 10 b hash)
    (puthash 11 c hash)
    (let ((out (dorg-render--render-island-subtree '(10 11) 10 hash)))
      (should (string-match-p "^\\* B" out))
      (should (string-match-p "B body\\." out))
      (should (string-match-p "^\\*\\* C" out)))))

(ert-deftest dorg-test-island-subtree-missing-node-errors ()
  "Rendering an island for a node absent from HEADINGS signals an error."
  (let ((hash (make-hash-table :test 'eql)))
    (should-error (dorg-render--render-island-subtree '(99) 99 hash)
                  :type 'dorg-render-error)))

(ert-deftest dorg-test-island-subtree-sort-key-order ()
  "Island children render in sort-key order, not insertion order."
  (let ((b (make-dorg-heading :id 10 :parent-id 1 :sort-key (string 1)
                              :level 1 :level-offset 1 :title "B"))
        (x (make-dorg-heading :id 12 :parent-id 10 :sort-key (string 2)
                              :level 2 :level-offset 1 :title "XX"))
        (y (make-dorg-heading :id 13 :parent-id 10 :sort-key (string 1)
                              :level 2 :level-offset 1 :title "YY"))
        (hash (make-hash-table :test 'eql)))
    (puthash 10 b hash)
    (puthash 12 x hash)              ; higher sort-key inserted first
    (puthash 13 y hash)
    (let* ((out (dorg-render--render-island-subtree '(10 12 13) 10 hash))
           (xpos (string-match "XX" out))
           (ypos (string-match "YY" out)))
      (should (and xpos ypos (< ypos xpos))))))

;;; Partial Materialized View Primitives — dolt-gated end-to-end

;; Fixture tree: * A / ** B / *** C / ** D / * E.
;; Island B = {B, C}; ancestors of B = root, A; siblings D, E excluded.
;; Ancestor A carries a body line so any leak of ancestor payload into a
;; headline-only spine load or breadcrumb render is detectable.

(defun dorg-render-tests--seed-island-fixture (dolt-dir)
  "Seed DOLT-DIR with the island fixture tree and commit."
  (let ((stmts (dorg-test--parse-to-sql
                "* A\nA ancestor body.\n** B\n*** C\n** D\n* E\n")))
    (let ((dorg-sql-transport 'server))
      (dorg-sql-batch dolt-dir stmts)
      (dorg-commit dolt-dir "island fixture"))))

(defun dorg-render-tests--id (dolt-dir title)
  "Return the heading id with TITLE in DOLT-DIR."
  (alist-get 'id (car (dorg-sql dolt-dir
                                "SELECT id FROM headings WHERE title = %s"
                                (dorg-sql-quote title)))))

(defun dorg-render-tests--root-id (dolt-dir)
  "Return the level-0 root id in DOLT-DIR."
  (alist-get 'id (car (dorg-sql dolt-dir
                                "SELECT id FROM headings WHERE level_offset = 0"))))

(ert-deftest dorg-test-render-subtree-ids-public-seam ()
  "`dorg-render-subtree-ids' is the public cross-module seam; the old
private `dorg-render--subtree-ids' name is gone (tracked-regression rename)."
  (should (fboundp 'dorg-render-subtree-ids))
  (should-not (fboundp 'dorg-render--subtree-ids)))

(ert-deftest dorg-test-island-subtree-ids-partial ()
  "`dorg-render-subtree-ids' for B returns exactly {B, C}."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (b (dorg-render-tests--id dolt-dir "B"))
             (c (dorg-render-tests--id dolt-dir "C"))
             (sub (dorg-render-subtree-ids dolt-dir root b)))
        (should (equal (sort (copy-sequence sub) #'<)
                       (sort (list b c) #'<)))))))

(ert-deftest dorg-test-island-subtree-ids-root-is-full ()
  "`dorg-render-subtree-ids' for the root returns the whole tree."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (all (mapcar (lambda (r) (alist-get 'id r))
                          (dorg-sql dolt-dir
                                    "SELECT id FROM headings WHERE root_id = %d"
                                    root)))
             (sub (dorg-render-subtree-ids dolt-dir root root)))
        (should (equal (sort (copy-sequence sub) #'<) (sort all #'<)))))))

(ert-deftest dorg-test-island-ancestor-ids-order ()
  "`dorg-render--ancestor-ids' for B is (root A); for root it is empty."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (a (dorg-render-tests--id dolt-dir "A"))
             (b (dorg-render-tests--id dolt-dir "B")))
        (should (equal (dorg-render--ancestor-ids dolt-dir root b)
                       (list root a)))
        (should (null (dorg-render--ancestor-ids dolt-dir root root)))))))

(ert-deftest dorg-test-island-node-root-id ()
  "`dorg-render--node-root-id' resolves a node's root; nil when absent."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (b (dorg-render-tests--id dolt-dir "B")))
        (should (equal (dorg-render--node-root-id dolt-dir b) root))
        (should (null (dorg-render--node-root-id dolt-dir 999999)))))))

(ert-deftest dorg-test-island-ancestor-spine-headline-only ()
  "`dorg-render--load-ancestor-spine' fetches no item payload, even when an
ancestor has a body in the DB.  A full load of the same ids would, so this
fails if the spine ever loads ancestor bodies."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (a (dorg-render-tests--id dolt-dir "A"))
             (b (dorg-render-tests--id dolt-dir "B"))
             (anc-ids (dorg-render--ancestor-ids dolt-dir root b))
             (spine (dorg-render--load-ancestor-spine dolt-dir root anc-ids))
             (full (dorg-model-load-headings dolt-dir root anc-ids)))
        ;; Ancestor A is in the spine...
        (should (gethash a spine))
        ;; ...a full load of A fetches its body item (transformation control)...
        (should (dorg-heading-items (gethash a full)))
        ;; ...but the headline-only spine leaves every ancestor's items nil.
        (should (null (dorg-heading-items (gethash a spine))))
        (should (null (dorg-heading-items (gethash root spine))))))))

(ert-deftest dorg-test-island-ancestor-spine-empty-is-not-full-load ()
  "An empty ancestor list yields an empty hash, never a whole-tree load."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (spine (dorg-render--load-ancestor-spine dolt-dir root nil)))
        (should (hash-table-p spine))
        (should (= 0 (hash-table-count spine)))))))

(ert-deftest dorg-test-render-island-with-context ()
  "`dorg-render-island' shows ancestor breadcrumb + full island, excludes
siblings, and keeps ancestors headline-only."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (out (dorg-render-island dolt-dir b)))
        ;; Ancestor A directly precedes island B — no body/drawer for A.
        (should (string-match-p "^\\* A\n\\*\\* B" out))
        (should-not (string-match-p "ancestor body" out)) ; A body not loaded
        (should (string-match-p "^\\*\\*\\* C" out))    ; descendant present
        (should-not (string-match-p "^\\*\\* D" out))   ; sibling excluded
        (should-not (string-match-p "^\\* E" out))      ; sibling excluded
        ;; No #+DORG_* keywords on a partial view.
        (should-not (string-match-p "#\\+DORG_DOLT_DIR:" out))
        (should-not (string-match-p "#\\+DORG_HASH:" out))))))

(ert-deftest dorg-test-render-island-leaf ()
  "A leaf island (C) shows the root→A→B breadcrumb and C, excludes D/E."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((c (dorg-render-tests--id dolt-dir "C"))
             (out (dorg-render-island dolt-dir c)))
        (should (string-match-p "^\\* A" out))
        (should (string-match-p "^\\*\\* B" out))
        (should (string-match-p "^\\*\\*\\* C" out))
        (should-not (string-match-p "^\\*\\* D" out))
        (should-not (string-match-p "^\\* E" out))))))

(ert-deftest dorg-test-render-island-missing-node-errors ()
  "`dorg-render-island' signals for a node id absent from the repo."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (should-error (dorg-render-island dolt-dir 999999)
                    :type 'dorg-render-error))))

(ert-deftest dorg-test-render-island-degenerate-equals-full ()
  "Island at the root is structurally equal to the whole-root render."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((root (dorg-render-tests--root-id dolt-dir))
             (island (dorg-render-island dolt-dir root))
             (full (dorg-render dolt-dir root))
             (parse (lambda (s)
                      (with-temp-buffer
                        (insert s)
                        (org-mode)
                        (let (hs)
                          (org-element-map (org-element-parse-buffer) 'headline
                            (lambda (h)
                              (push (cons (org-element-property :level h)
                                          (org-element-property :raw-value h))
                                    hs)))
                          (nreverse hs))))))
        (should (equal (funcall parse island) (funcall parse full)))))))

(ert-deftest dorg-test-render-to-buffer-clears-stale-partial-marker ()
  "A whole-root `dorg-materialize' clears a stale partial-view record
left on a reused org buffer (entering `org-mode' alone would not, since the
buffer is already in org-mode)."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let ((root (dorg-render-tests--root-id dolt-dir)))
        (with-temp-buffer
          (org-mode)
          ;; Simulate a buffer previously rendered as an island.
          (dorg-projection-install-view
           (current-buffer) (current-buffer) 1 nil 'partial (list :island 2))
          (should (eq (dorg-materialized-view-scope (current-buffer)) 'partial))
          (let ((temp-base (make-temp-file "dorg-render-materialize-" t))
                (dorg--registry nil))
            (unwind-protect
                  (let ((org-file (expand-file-name "render.org" temp-base)))
                    (with-temp-file org-file)
                    (dorg--store-registration
                     (file-truename org-file)
                     (make-dorg-registration
                      :backend (dorg-backend-dolt
                                dolt-dir (dorg-render--resolve-root-id dolt-dir))
                      :file (file-truename org-file)))
                    (set-visited-file-name org-file t)
                    (dorg-materialize (current-buffer)))
              (delete-directory temp-base t)))
          (should (eq major-mode 'org-mode))
          ;; Full render is a complete tree: the partial scope must be gone.
          (should (eq (dorg-materialized-view-scope (current-buffer)) 'whole))
          ;; And the buffer genuinely holds the whole tree.
          (should (string-match-p "^\\* A" (buffer-string)))
          (should (string-match-p "^\\* E" (buffer-string))))))))

;;; Bounded-window materialization (tracked-case)
;;
;; A "window" is the whole-tree analogue of an island for MULTIPLE foci,
;; WITH the root preamble and a DURABLE `#+DORG_WINDOW:' marker.  Unlike an
;; island (a read-only VFS projection with no `#+DORG_*' keywords), a window
;; is written into an ordinary file-visiting `beads.org' that carries the
;; Dolt binding.  Its durable marker re-establishes partial materialized-view
;; scope on a fresh visit, preserving the fact that out-of-window siblings are
;; unknown rather than deleted.

(defun dorg-render-tests--count-headlines (text)
  "Count Org headlines (lines beginning with one or more `*') in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^\\*+ " nil t) (cl-incf n))
      n)))

(defun dorg-render-tests--root-section (text)
  "Return TEXT before the first Org headline."
  (if (string-match "^\\*+ " text)
      (substring text 0 (match-beginning 0))
    text))

(defun dorg-render-tests--root-section-without-dorg-metadata (text)
  "Return root section of TEXT without render-injected DORG metadata."
  (with-temp-buffer
    (insert (dorg-render-tests--root-section text))
    (goto-char (point-min))
    (while (re-search-forward "^#\\+DORG_[[:alnum:]_]+:.*\n" nil t)
      (replace-match ""))
    (buffer-string)))

(defun dorg-render-tests--seed-window-fixture (dolt-dir n)
  "Seed DOLT-DIR with a root of N flat children (each with a body); commit.
Children are titled Child-1..Child-N, giving a materially large full tree
\(thousands of headings) to window against."
  (let ((org (with-temp-buffer
               (dotimes (i n)
                 (insert (format "* Child-%d\nbody line for child %d\n"
                                 (1+ i) (1+ i))))
               (buffer-string))))
    (let ((dorg-sql-transport 'server))
      (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql org))
      (dorg-commit dolt-dir "window fixture"))))

(ert-deftest dorg-test-render-window-bounded-with-preamble ()
  "`dorg-render--window' emits preamble + #+DORG_WINDOW: + breadcrumb + focus
subtree, excluding unrelated siblings.
Focus B on the island fixture: root preamble keywords are injected (unlike
an island), ancestor A is headline-only, B and its descendant C are present,
and siblings D/E are excluded."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (out (dorg-render--window dolt-dir (list b))))
        ;; Preamble + durable window marker present (a window IS a bound file).
        (should (string-match-p "^#\\+DORG_DOLT_DIR:" out))
        (should (string-match-p "^#\\+DORG_HASH:" out))
        (should (string-match-p "^#\\+DORG_WINDOW:" out))
        ;; Ancestor breadcrumb (headline-only) + focus subtree.
        (should (string-match-p "^\\* A$" out))
        (should-not (string-match-p "A ancestor body" out))
        (should (string-match-p "^\\*\\* B" out))
        (should (string-match-p "^\\*\\*\\* C" out))
        ;; Unrelated siblings excluded.
        (should-not (string-match-p "^\\*\\* D" out))
        (should-not (string-match-p "^\\* E" out))))))

(ert-deftest dorg-test-render-window-multi-focus-union ()
  "`dorg-materialize :window' unions multiple focus subtrees and spines.
Foci {B, E} (cross-branch): B+C and E are present, the shared/branch
ancestor breadcrumb is present, and only the non-focus sibling D is excluded."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (e (dorg-render-tests--id dolt-dir "E"))
             (root (dorg-render-tests--root-id dolt-dir)))
        (dorg-render-tests--with-materialized-buffer dolt-dir root
          (dorg-materialize (current-buffer)
                            :window (list :focus (list b e)))
          (let ((out (buffer-string)))
            (should (string-match-p "^\\* A$" out))
            (should (string-match-p "^\\*\\* B" out))
            (should (string-match-p "^\\*\\*\\* C" out))
            (should (string-match-p "^\\* E" out))
            (should-not (string-match-p "^\\*\\* D" out))))))))

(ert-deftest dorg-test-render-window-to-buffer-marks-partial ()
  "`dorg-materialize :window' fills the buffer and sets partial scope."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let ((b (dorg-render-tests--id dolt-dir "B"))
            (root (dorg-render-tests--root-id dolt-dir)))
        (with-temp-buffer
          (let ((temp-base (make-temp-file "dorg-materialize-window-" t))
                (dorg--registry nil))
            (unwind-protect
                (let ((org-file (expand-file-name "window.org" temp-base)))
                  (with-temp-file org-file)
                  (dorg--store-registration
                   (file-truename org-file)
                   (make-dorg-registration
                    :backend (dorg-backend-dolt dolt-dir root)
                    :file (file-truename org-file)))
                  (org-mode)
                  (set-visited-file-name org-file t)
                  (dorg-materialize (current-buffer)
                                    :window (list :focus (list b))))
              (delete-directory temp-base t)))
          (should (eq major-mode 'org-mode))
          (should (string-match-p "^\\*\\* B" (buffer-string)))
          (ignore root)
          (should (eq (dorg-materialized-view-scope (current-buffer))
                      'partial)))))))

(ert-deftest dorg-test-window-marker-restore-round-trip ()
  "The durable #+DORG_WINDOW: marker re-establishes the partial view.
Rendering window text, dropping it into a FRESH buffer with no buffer-local
marker, and calling `dorg-render--restore-window-marker' must reconstruct
partial scope from the file text alone."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (root (dorg-render-tests--root-id dolt-dir))
             (text (dorg-render--window dolt-dir (list b))))
        (with-temp-buffer
          (insert text)
          (org-mode)
          (dorg-projection-clear-view)
          (dorg-render--restore-window-marker)
          (ignore root)
          (should (eq (dorg-materialized-view-scope (current-buffer))
                      'partial)))))))

(ert-deftest dorg-test-window-marker-survives-file-revisit ()
  "A windowed file re-read from DISK re-establishes the partial marker.
Proves durability across save/close/revisit (many processes open beads.org)."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (text (dorg-render--window dolt-dir (list b)))
             (file (expand-file-name "windowed.org" temp-base)))
        (with-temp-file file (insert text))
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (should (null (dorg-materialized-view-scope buf)))
                (dorg-render--restore-window-marker)
                (ignore b)
                (should (eq (dorg-materialized-view-scope buf) 'partial)))
            (kill-buffer buf)))))))

(ert-deftest dorg-test-window-file-restores-exact-partial-context ()
  "A durable window marker restores its exact bounded projection context."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (text (dorg-render--window dolt-dir (list b))))
        (with-temp-buffer
          (insert text)
          (org-mode)
          (dorg-projection-clear-view)
          (dorg-render--restore-window-marker)
          (let ((view (dorg-projection-view-context)))
            (should (eq (plist-get view :scope) 'partial))
            (should (equal (plist-get view :window) (list b)))
            (should (equal (plist-get view :root-id)
                           (dorg-render--resolve-root-id dolt-dir)))))))))

(defun dorg-render-tests--seed-preamble-fixture (dolt-dir)
  "Seed DOLT-DIR with root preamble/config keywords + the island tree; commit.
The root carries a TODO sequence, priorities, and startup keyword so the
window can be checked for preamble/config preservation."
  (let ((stmts (dorg-test--parse-to-sql
                (concat "#+SEQ_TODO: TODO STRT | DONE\n"
                        "#+PRIORITIES: A E C\n"
                        "#+STARTUP: overview\n"
                        "* A\nA ancestor body.\n** B\n*** C\n** D\n* E\n"))))
    (let ((dorg-sql-transport 'server))
      (dorg-sql-batch dolt-dir stmts)
      (dorg-commit dolt-dir "preamble fixture"))))

(defun dorg-render-tests--seed-root-content-window-fixture (dolt-dir)
  "Seed DOLT-DIR with root content plus bounded-window heading content; commit."
  (let ((stmts
         (dorg-test--parse-to-sql
          (concat "#+SEQ_TODO: TODO NEXT | DONE CNCL\n"
                  "#+PRIORITIES: A C B\n"
                  "#+STARTUP: overview\n"
                  "#+CATEGORY: window-root\n"
                  ":PROPERTIES:\n"
                  ":ROOT_KIND: fixture\n"
                  ":OWNER: root\n"
                  ":END:\n"
                  ":ROOT_DRAWER:\n"
                  "root drawer line one\n"
                  "root drawer line two\n"
                  ":END:\n"
                  "Root body paragraph before any heading.\n"
                  "- root list item\n"
                  "- second root list item\n\n"
                  "* Focus Parent\n"
                  "Parent breadcrumb body should be elided.\n"
                  "** Focus Target\n"
                  "Focus body preserved.\n"
                  "*** Focus Child\n"
                  "Child body preserved.\n"
                  "** Out Of Window Peer\n"
                  "Peer body must not render.\n"
                  "* Out Of Window Top\n"
                  "Top body must not render.\n"))))
    (let ((dorg-sql-transport 'server))
      (dorg-sql-batch dolt-dir stmts)
      (dorg-commit dolt-dir "root content window fixture"))))

(ert-deftest dorg-test-render-window-preserves-root-preamble ()
  "The window carries root preamble/config (TODO sequence, priorities,
startup) before the first heading, while unrelated siblings stay excluded.
Org/Downstream tracker commands operating inside the window need this config."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-preamble-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (root (dorg-render-tests--root-id dolt-dir)))
        (dorg-render-tests--with-materialized-buffer dolt-dir root
          (dorg-materialize (current-buffer)
                            :window (list :focus (list b)))
          (let ((out (buffer-string)))
            ;; Root preamble/config survives in the window.
            (should (string-match-p (concat "^#\\+SEQ_TODO: "
                                            (regexp-quote "TODO STRT | DONE"))
                                    out))
            (should (string-match-p "^#\\+PRIORITIES: A E C" out))
            (should (string-match-p "^#\\+STARTUP: overview" out))
            ;; Preamble precedes the first heading.
            (should (< (string-match "^#\\+SEQ_TODO:" out)
                       (string-match "^\\* A" out)))
            ;; Focus subtree present, unrelated siblings excluded.
            (should (string-match-p "^\\*\\* B" out))
            (should-not (string-match-p "^\\*\\* D" out))
            (should-not (string-match-p "^\\* E" out))))))))

(ert-deftest dorg-test-render-window-preserves-full-root-content ()
  "A bounded window preserves full root non-heading content.
The heading tree stays bounded: focus content is present, unrelated siblings
from the full tree are absent."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-root-content-window-fixture dolt-dir)
      (let* ((focus (dorg-render-tests--id dolt-dir "Focus Target"))
             (full (dorg-render dolt-dir))
             (window (dorg-render--window dolt-dir (list focus)))
             (full-root
              (dorg-render-tests--root-section-without-dorg-metadata full))
             (window-root
              (dorg-render-tests--root-section-without-dorg-metadata window)))
        ;; Root dehydration is full-render equivalent once injected metadata
        ;; (`DORG_DOLT_DIR', `DORG_HASH', `DORG_WINDOW') is ignored.
        (should (equal full-root window-root))
        ;; Downstream tracker/root config and binding markers are present inside the window.
        (should (string-match-p "^#\\+SEQ_TODO: TODO NEXT | DONE CNCL" window))
        (should (string-match-p "^#\\+PRIORITIES: A C B" window))
        (should (string-match-p "^#\\+DORG_DOLT_DIR:" window))
        (should (string-match-p "^#\\+DORG_WINDOW:" window))
        ;; The fixture covers more than keywords.
        (should (string-match-p "^:PROPERTIES:$" window-root))
        (should (string-match-p "^:ROOT_DRAWER:$" window-root))
        (should (string-match-p "^root drawer line two$" window-root))
        (should (string-match-p "^Root body paragraph" window-root))
        (should (string-match-p "^- second root list item$" window-root))
        ;; Non-vacuous bounded window: focus subtree is present, unrelated
        ;; siblings remain only in the full render, and ancestor payload is
        ;; still not leaked into the breadcrumb.
        (should (string-match-p "^\\*\\* Focus Target" window))
        (should (string-match-p "^Focus body preserved\\." window))
        (should (string-match-p "^\\*\\*\\* Focus Child" window))
        (should (string-match-p "^\\*\\* Out Of Window Peer" full))
        (should (string-match-p "^\\* Out Of Window Top" full))
        (should-not (string-match-p "^\\*\\* Out Of Window Peer" window))
        (should-not (string-match-p "^\\* Out Of Window Top" window))
        (should-not
         (string-match-p "^Parent breadcrumb body should be elided\\."
                         window))))))

(ert-deftest dorg-test-window-mode-activation-restores-read-only-view ()
  "Enabling `dorg-mode' on a freshly-visited windowed file restores the
partial marker via ACTIVATION (no manual restore) and freezes the projection.

This is the real data-loss path the durable marker exists to close: without
the activation restore, later refresh could lose the established scope."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (require 'dorg-mode)
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (dorg-render-tests--seed-island-fixture dolt-dir)
      (let* ((b (dorg-render-tests--id dolt-dir "B"))
             (text (dorg-render--window dolt-dir (list b)))
             (file (expand-file-name "windowed.org" temp-base))
             (dorg--registry nil)
             (dorg-dolt-auto-server nil))     ; hermetic: no server start on enable
        (with-temp-file file (insert text))
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (should (null (dorg-materialized-view-scope buf)))
                (dorg-mode 1)                 ; activation — NO manual restore
                ;; Activation restored the durable window marker...
                (ignore b)
                (should (eq (dorg-materialized-view-scope buf) 'partial))
                ;; ...and storage authority froze the projection.
                (should buffer-read-only))
            (kill-buffer buf)))))))

(ert-deftest dorg-test-render-window-scale-materially-smaller ()
  "A window is materially smaller than the full materialization (non-vacuous).
With a fixture whose full materialization has HUNDREDS of headings, a
single-focus window has orders of magnitude fewer headlines and bytes than
the full render.  If the windowing were removed (full render returned
instead), window-count would equal full-count and the ratio assertions would
fail.  Bounded at 500 to keep the suite fast (a Dolt-backed test); the live
tracker is ~11k headings and the wall-time comparison there is the tracker
bead's live benchmark."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-render-tests--seed-window-fixture dolt-dir 500)
      (let* ((child1 (dorg-render-tests--id dolt-dir "Child-1"))
             (root (dorg-render-tests--root-id dolt-dir)))
        (dorg-render-tests--with-materialized-buffer dolt-dir root
          (let ((full (buffer-string)))
            (dorg-materialize (current-buffer)
                              :window (list :focus (list child1)))
            (let* ((window (buffer-string))
                   (full-h (dorg-render-tests--count-headlines full))
                   (win-h (dorg-render-tests--count-headlines window)))
              (message "fepa2 scale: full=%d headlines/%d bytes; window=%d headlines/%d bytes"
                       full-h (length full) win-h (length window))
              ;; Full tree is materially large (hundreds); the public window
              ;; lifecycle must replace it with a tiny slice.
              (should (>= full-h 500))
              (should (< win-h 10))
              (should (< (* 50 win-h) full-h))
              (should (< (* 20 (length window)) (length full))))))))))

;;; P0 post_blank drift — pure parse->model-render idempotence (no DB)

(defun dorg-render-tests--pb-roundtrip (text)
  "Parse TEXT and render the model tree back to org text (pure, no DB).
Uses a fixed synthetic root id that cannot collide with the fixtures' DORG_IDs,
so a fixture carrying explicit `:DORG_ID:' drawers is a render fixpoint."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (dorg-model-render-tree
     (dorg-parse-result-headings (dorg-parse-buffer nil 700000)))))

(defun dorg-render-tests--blank-run-after (text needle)
  "Count blank lines immediately after the line containing NEEDLE in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (search-forward needle)
    (forward-line 1)
    (let ((n 0))
      (while (and (not (eobp)) (looking-at-p "^$"))
        (setq n (1+ n))
        (forward-line 1))
      n)))

(defun dorg-render-tests--fixed-width-fixture (n)
  "Two headings; the first holds a stack-trace fixed-width block + N blanks.
Minimized from the P0 site-1 shape (heading 56368228379836963)."
  (concat
   "* Probe evidence\n:PROPERTIES:\n:DORG_ID: 100200\n:END:\n"
   "The bead attributed the hang; stack:\n"
   ": replace-buffer-contents\n"
   ":   <- dorg-materialize\n"
   ":   <- tracker-backend-dolt--materialize\n"
   (make-string n ?\n)
   "So the materialize path hangs, not the sync arm.\n"
   "* Sibling\n:PROPERTIES:\n:DORG_ID: 100300\n:END:\nx\n"))

(defun dorg-render-tests--clock-fixture (n)
  "A heading holding a single CLOCK fixed-width line + N blanks.
Minimized from the P0 site-2 shape (heading 60477899073265668)."
  (concat
   "* Residual gap\n:PROPERTIES:\n:DORG_ID: 100400\n:END:\n"
   "Org clock entries fall into the generic bucket:\n"
   ": CLOCK: [2026-06-16 Tue 10:00]--[2026-06-16 Tue 11:00] =>  1:00\n"
   (make-string n ?\n)
   "The inactive range is already captured as a timestamp.\n"
   "* Sib\n:PROPERTIES:\n:DORG_ID: 100500\n:END:\nx\n"))

(ert-deftest dorg-test-render-fixed-width-run-one-pass-idempotent ()
  "One parse->render pass over a fixed-width block + blank run is byte-identical.
Pre-fix the pass adds one blank line inside the run (org's fixed-width
`:post-blank' +1 over-read, faithfully re-emitted)."
  (let ((text (dorg-render-tests--fixed-width-fixture 6)))
    (should (string= text (dorg-render-tests--pb-roundtrip text)))))

(ert-deftest dorg-test-render-fixed-width-run-repeated-pass-stable ()
  "Repeated parse->render cycles keep a fixed-width block's blank run constant.
Pre-fix the run drifts 6->7->8->9 (org's fixed-width `:post-blank' +1 per
cycle); the fix makes it a fixpoint.  Transformation test: it asserts the run
never reaches the known-wrong drifted value, so deleting/bypassing the fix
fails it."
  (let* ((text (dorg-render-tests--fixed-width-fixture 6))
         (needle "tracker-backend-dolt--materialize")
         (r1 (dorg-render-tests--pb-roundtrip text))
         (r2 (dorg-render-tests--pb-roundtrip r1))
         (r3 (dorg-render-tests--pb-roundtrip r2)))
    (should (= 6 (dorg-render-tests--blank-run-after text needle)))
    (should (= 6 (dorg-render-tests--blank-run-after r1 needle)))
    (should (= 6 (dorg-render-tests--blank-run-after r2 needle)))
    (should (= 6 (dorg-render-tests--blank-run-after r3 needle)))
    ;; known-wrong drifted state must NOT appear (bypassing the fix -> 7,8,9)
    (should-not (= 7 (dorg-render-tests--blank-run-after r1 needle)))))

(ert-deftest dorg-test-render-p0-recovered-shapes-byte-stable ()
  "Both minimized P0 recovered shapes are byte-stable over two render cycles.
Covers the stack-trace fixed-width block (site 1) and the CLOCK fixed-width
line (site 2) together, mirroring the real file which contains both."
  (dolist (text (list (dorg-render-tests--fixed-width-fixture 5)
                      (dorg-render-tests--clock-fixture 4)))
    (let* ((r1 (dorg-render-tests--pb-roundtrip text))
           (r2 (dorg-render-tests--pb-roundtrip r1)))
      (should (string= text r1))
      (should (string= r1 r2)))))

(ert-deftest dorg-test-render-blank-run-controls-byte-identical ()
  "Non-fixed-width blank runs stay byte-identical across a render cycle.
Controls proving the fix does not normalize anything outside the defective
boundary: a paragraph, an example block, a generic drawer, and a keyword each
followed by a blank run must round-trip unchanged (GREEN both pre- and
post-fix)."
  (let ((controls
         (list
          ;; paragraph + run
          (concat "* H\n:PROPERTIES:\n:DORG_ID: 200100\n:END:\n"
                  "first paragraph line\n\n\n\n\ntail para\n"
                  "* S\n:PROPERTIES:\n:DORG_ID: 200200\n:END:\nx\n")
          ;; example block + run
          (concat "* H\n:PROPERTIES:\n:DORG_ID: 200300\n:END:\n"
                  "#+begin_example\nsample\n#+end_example\n\n\n\n\ntail para\n"
                  "* S\n:PROPERTIES:\n:DORG_ID: 200400\n:END:\nx\n")
          ;; generic drawer + run
          (concat "* H\n:PROPERTIES:\n:DORG_ID: 200500\n:END:\n"
                  ":LOGBOOK:\nCLOCK: entry\n:END:\n\n\n\n\ntail para\n"
                  "* S\n:PROPERTIES:\n:DORG_ID: 200600\n:END:\nx\n")
          ;; keyword + run
          (concat "* H\n:PROPERTIES:\n:DORG_ID: 200700\n:END:\n"
                  "#+CAPTION: hi\n\n\n\n\ntail para\n"
                  "* S\n:PROPERTIES:\n:DORG_ID: 200800\n:END:\nx\n"))))
    (dolist (text controls)
      (should (string= text (dorg-render-tests--pb-roundtrip text))))))

(ert-deftest dorg-test-level-offset-render-uses-given-effective-level ()
  "GUARD: renderers emit stars from the effective level slot AS GIVEN.
Downstream detached copies rebase that slot and render without persisting,
so the renderer must never re-derive levels from offsets.  Here the level
slots deliberately disagree with the offsets: the stars follow the levels."
  (let ((headings
         (list (make-dorg-heading :id 1 :parent-id nil :level 0
                                  :sort-key (dorg-sortkey-from-integer 1)
                                  :post-blank 0)
               (make-dorg-heading :id 2 :parent-id 1 :level 2
                                  :sort-key (dorg-sortkey-from-integer 1 2)
                                  :title "given two" :post-blank 0)
               (make-dorg-heading :id 3 :parent-id 2 :level 5
                                  :sort-key (dorg-sortkey-from-integer 1 2)
                                  :title "given five" :post-blank 0))))
    (when (fboundp 'dorg-heading-level-offset)
      ;; Offsets that would derive 1 and 2 if a renderer ever re-derived.
      (setf (dorg-heading-level-offset (nth 0 headings)) 0
            (dorg-heading-level-offset (nth 1 headings)) 1
            (dorg-heading-level-offset (nth 2 headings)) 1))
    (let ((text (dorg-model-render-tree headings)))
      (should (string-match-p "^\\*\\* given two$" text))
      (should (string-match-p "^\\*\\*\\*\\*\\* given five$" text)))))

(provide 'dorg-render-tests)
;;; dorg-render-tests.el ends here
