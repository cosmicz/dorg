;;; dorg-parse-tests.el --- Tests for dorg-parse -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dorg org-mode parser.  Tests inspect the domain model
;; objects (`dorg-heading', `dorg-heading-item' hierarchy) returned by
;; `dorg-parse-buffer'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-parse)
(require 'dorg-sql-gen)
(require 'dorg-model)
(require 'dorg-render)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-test-helpers)

;;; Helpers

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

(defmacro dorg-test-with-org-buffer (contents &rest body)
  "Create a temp org buffer with CONTENTS and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro dorg-test-with-parsed (contents &rest body)
  "Parse CONTENTS and bind `result' and `headings'.
Also runs `dorg-parse-assign-ids' with a deterministic generator
so all headings have integer ids and correct parent-ids."
  (declare (indent 1))
  `(dorg-test-with-org-buffer ,contents
     (let* ((result (dorg-parse-buffer))
            (counter 0)
            (_writeback (dorg-parse-assign-ids
                         result (lambda () (cl-incf counter))))
            (headings (dorg-parse-result-headings result)))
       ,@body)))

(defmacro dorg-test-with-import (contents &rest body)
  "Create a Dolt repo, import CONTENTS as org, execute BODY.
Binds `dolt-dir' to the repo.  Skips if dolt is not available."
  (declare (indent 1))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       (let ((stmts (dorg-test--parse-to-sql ,contents)))
         (when stmts
           (dorg-sql-batch dolt-dir stmts)
           (dorg-commit dolt-dir "test import")))
       ,@body)))

;;; Unit Tests — SQL Generation Helper

(ert-deftest dorg-test-parse-sort-key-hex ()
  "Sort key hex literals are formatted correctly for unibyte strings."
  (should (equal (dorg-sql-gen--sort-key-hex (unibyte-string 1)) "X'01'"))
  (should (equal (dorg-sql-gen--sort-key-hex (unibyte-string 10)) "X'0a'"))
  (should (equal (dorg-sql-gen--sort-key-hex (unibyte-string 255)) "X'ff'"))
  (should (equal (dorg-sql-gen--sort-key-hex (unibyte-string 1 #x80)) "X'0180'")))

;;; Unit Tests — Domain Object Extraction

(ert-deftest dorg-test-parse-empty-buffer ()
  "Empty buffer produces only the synthetic root heading."
  (dorg-test-with-parsed ""
    (should (= (length headings) 1))
    (let ((root (car headings)))
      (should (= (dorg-heading-level root) 0))
      (should (null (dorg-heading-parent-id root))))))

(ert-deftest dorg-test-parse-single-heading ()
  "Single heading produces root + heading."
  (dorg-test-with-parsed "* Hello World\n"
    (should (= (length headings) 2))
    (let ((h (dorg-test--find-heading headings "Hello World")))
      (should h)
      (should (= (dorg-heading-level h) 1)))))

(ert-deftest dorg-test-parse-heading-with-todo ()
  "Heading with TODO keyword is captured."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-parsed "* TODO Fix the bug\n"
      (let ((h (dorg-test--find-heading headings "Fix the bug")))
        (should h)
        (should (equal (dorg-heading-todo h) "TODO"))))))

(ert-deftest dorg-test-parse-heading-with-tags ()
  "Heading tags are captured in order."
  (dorg-test-with-parsed "* Tagged heading  :foo:bar:\n"
    (let ((h (dorg-test--find-heading headings "Tagged heading")))
      (should h)
      (should (equal (dorg-heading-tags h) '("foo" "bar"))))))

(ert-deftest dorg-test-parse-file-keyword ()
  "File-level keywords stored as keyword_items under root."
  (dorg-test-with-parsed "#+TITLE: My Document\n"
    (let* ((root (dorg-test--root-heading headings))
           (kw-items (dorg-test--items-of-type root 'dorg-keyword-item)))
      (should (= (length kw-items) 1))
      (should (equal (dorg-keyword-item-keyword (car kw-items)) "TITLE"))
      (should (equal (dorg-keyword-item-value (car kw-items)) "My Document")))))

(ert-deftest dorg-test-parse-preamble-text ()
  "Text before first heading stored as text_item under root."
  (dorg-test-with-parsed "Some preamble text.\n\n* Heading\n"
    (let* ((root (dorg-test--root-heading headings))
           (text-items (dorg-test--items-of-type root 'dorg-text-item)))
      (should (>= (length text-items) 1))
      (should (cl-some (lambda (ti) (string-match-p "preamble text"
                                                     (dorg-text-item-text ti)))
                       text-items)))))

(ert-deftest dorg-test-parse-property-drawer ()
  "Property drawer generates drawer item with properties."
  (dorg-test-with-parsed "* Heading\n:PROPERTIES:\n:ID: abc-123\n:CUSTOM: value\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      (should (= (length drawers) 1))
      (should (equal (dorg-drawer-item-name (car drawers)) "PROPERTIES"))
      (let ((props (dorg-drawer-item-properties (car drawers))))
        (should (= (length props) 2))
        (should (equal (car (nth 0 props)) "ID"))
        (should (equal (cdr (nth 0 props)) "abc-123"))
        (should (equal (car (nth 1 props)) "CUSTOM"))
        (should (equal (cdr (nth 1 props)) "value"))))))

(ert-deftest dorg-test-parse-generic-drawer ()
  "Generic drawer generates drawer item with lines."
  (dorg-test-with-parsed "* Heading\n:LOGBOOK:\nLine one\nLine two\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      (should (= (length drawers) 1))
      (should (equal (dorg-drawer-item-name (car drawers)) "LOGBOOK"))
      (should (equal (dorg-drawer-item-lines (car drawers))
                     '("Line one" "Line two"))))))

(ert-deftest dorg-test-parse-generic-drawer-interior-blank ()
  "Blank lines INSIDE a generic drawer are preserved (AC1).
The parser used to drop every empty line; only the trailing `:END:'
newline artifact should be dropped, never a genuine interior blank."
  (dorg-test-with-parsed "* Heading\n:LOGBOOK:\nA\n\nB\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      (should (= (length drawers) 1))
      (should (equal (dorg-drawer-item-lines (car drawers))
                     '("A" "" "B"))))))

(ert-deftest dorg-test-parse-generic-drawer-trailing-blank ()
  "A trailing blank line inside a generic drawer is preserved (AC2).
Only the single `:END:' newline artifact is stripped, so a genuine
trailing blank survives as a final \"\" element."
  (dorg-test-with-parsed "* Heading\n:NOTES:\nA\n\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      (should (= (length drawers) 1))
      (should (equal (dorg-drawer-item-lines (car drawers))
                     '("A" ""))))))

(ert-deftest dorg-test-parse-generic-drawer-empty ()
  "An empty generic drawer yields no lines (AC4, unchanged behavior)."
  (dorg-test-with-parsed "* Heading\n:NOTES:\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      (should (= (length drawers) 1))
      (should (equal (dorg-drawer-item-lines (car drawers)) nil)))))

(ert-deftest dorg-test-parse-planning ()
  "Planning line generates planning item."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-parsed "* TODO Task\nSCHEDULED: <2026-04-09 Thu> DEADLINE: <2026-04-10 Fri>\n"
      (let* ((h (dorg-test--find-heading headings "Task"))
             (plans (dorg-test--items-of-type h 'dorg-planning-item)))
        (should (= (length plans) 1))
        (should (string-match-p "2026-04-09"
                                (dorg-planning-item-scheduled (car plans))))
        (should (string-match-p "2026-04-10"
                                (dorg-planning-item-deadline (car plans))))))))

(ert-deftest dorg-test-parse-timestampless-planning-shaped-text ()
  "Planning-shaped prose without a timestamp remains text, never vanishes.
Org classifies each fixture as a planning element even though none carries a
recognized timestamp.  The parser must therefore apply the class-level
all-nil fallback rather than special-case one keyword spelling."
  (dolist (line '("Closed: this is an tracker feature request"
                  "CLOSED: prose, not a timestamp"
                  "closed: lowercase prose"
                  "SCHEDULED: someday"
                  "Scheduled: later"
                  "DEADLINE: none"
                  "Deadline: prose"))
    (dorg-test-with-parsed (format "* Heading\n%s\n" line)
      (let* ((h (dorg-test--find-heading headings "Heading"))
             (texts (dorg-test--items-of-type h 'dorg-text-item))
             (plans (dorg-test--items-of-type h 'dorg-planning-item)))
        (should-not plans)
        (should (= (length texts) 1))
        (should (string= (dorg-text-item-text (car texts))
                         (concat line "\n")))
        (let ((rendered (dorg-model-render-heading-section h)))
          (should (string-match-p (regexp-quote (concat line "\n"))
                                  rendered)))))))

(ert-deftest dorg-test-parse-valid-planning-controls ()
  "Recognized planning timestamps retain their semantic planning shape."
  (dorg-test-with-parsed
      "* Heading\nSCHEDULED: <2026-08-20 Thu> DEADLINE: <2026-08-21 Fri> CLOSED: [2026-08-22 Sat]\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (plans (dorg-test--items-of-type h 'dorg-planning-item)))
      (should (= (length plans) 1))
      (should (string= (dorg-planning-item-scheduled (car plans))
                       "<2026-08-20 Thu>"))
      (should (string= (dorg-planning-item-deadline (car plans))
                       "<2026-08-21 Fri>"))
      (should (string= (dorg-planning-item-closed (car plans))
                       "[2026-08-22 Sat]")))))

(ert-deftest dorg-test-parse-text-helper-single-owned ()
  "Planning fallback and ordinary prose share one text-geometry helper."
  (let ((calls 0)
        (original (symbol-function 'dorg-parse--element-text-item)))
    (cl-letf (((symbol-function 'dorg-parse--element-text-item)
               (lambda (element)
                 (cl-incf calls)
                 (funcall original element))))
      (dorg-test-with-parsed
          "* Heading\nClosed: this is prose\n\nOrdinary paragraph.\n"
        (let ((h (dorg-test--find-heading headings "Heading")))
          (should (= (length (dorg-test--items-of-type h 'dorg-text-item))
                     2)))))
    (should (= calls 2))))

(ert-deftest dorg-test-parse-nested-headings ()
  "Nested headings have correct parent-id relationships."
  (dorg-test-with-parsed "* Parent\n** Child\n*** Grandchild\n"
    ;; root + Parent + Child + Grandchild = 4
    (should (= (length headings) 4))
    (let ((root (dorg-test--root-heading headings))
          (parent (dorg-test--find-heading headings "Parent"))
          (child (dorg-test--find-heading headings "Child"))
          (gc (dorg-test--find-heading headings "Grandchild")))
      ;; root: parent-id nil
      (should (null (dorg-heading-parent-id root)))
      ;; Parent → root
      (should (= (dorg-heading-parent-id parent)
                 (dorg-heading-id root)))
      ;; Child → Parent
      (should (= (dorg-heading-parent-id child)
                 (dorg-heading-id parent)))
      ;; Grandchild → Child
      (should (= (dorg-heading-parent-id gc)
                 (dorg-heading-id child))))))

(ert-deftest dorg-test-parse-body-text ()
  "Body text under a heading stored as text_item."
  (dorg-test-with-parsed "* Heading\nSome body text here.\n"
    (let* ((h (dorg-test--find-heading headings "Heading"))
           (texts (dorg-test--items-of-type h 'dorg-text-item)))
      (should (>= (length texts) 1))
      (should (cl-some (lambda (ti) (string-match-p "body text"
                                                     (dorg-text-item-text ti)))
                       texts)))))

(ert-deftest dorg-test-parse-multiple-keywords ()
  "Multiple file keywords all captured."
  (dorg-test-with-parsed "#+TITLE: Doc\n#+AUTHOR: Me\n#+STARTUP: showall\n"
    (let* ((root (dorg-test--root-heading headings))
           (kw-items (dorg-test--items-of-type root 'dorg-keyword-item)))
      (should (= (length kw-items) 3)))))

(ert-deftest dorg-test-parse-special-chars ()
  "Title with single quotes is preserved."
  (dorg-test-with-parsed "* It's a test\n"
    (let ((h (dorg-test--find-heading headings "It's a test")))
      (should h))))

;;; Unit Tests — DORG_ID + root_id

(ert-deftest dorg-test-parse-root-id-emitted ()
  "Root-id is present in the parse result."
  (dorg-test-with-parsed "* One\n** Two\n* Three\n"
    (let ((root-id (dorg-parse-result-root-id result)))
      (should (integerp root-id))
      ;; Root heading has id = root-id
      (let ((root (dorg-test--root-heading headings)))
        (should (= (dorg-heading-id root) root-id)))
      ;; All non-root headings reference root-id via parent chain
      (dolist (h headings)
        (should (integerp (dorg-heading-id h)))))))

(ert-deftest dorg-test-parse-explicit-root-id ()
  "Explicit root-id argument is honored."
  (dorg-test-with-org-buffer "* Hello\n"
    (let* ((result (dorg-parse-buffer nil 42))
           (counter 100)
           (_wb (dorg-parse-assign-ids
                 result (lambda () (cl-incf counter))))
           (headings (dorg-parse-result-headings result)))
      (should (= (dorg-parse-result-root-id result) 42))
      (let ((root (dorg-test--root-heading headings)))
        (should (= (dorg-heading-id root) 42))))))

(ert-deftest dorg-test-parse-dorg-id-read ()
  "Existing :DORG_ID: is read and used as heading id."
  (dorg-test-with-parsed
      "* Heading\n:PROPERTIES:\n:DORG_ID: 9999\n:END:\n"
    (let ((h (dorg-test--find-heading headings "Heading")))
      (should (= (dorg-heading-id h) 9999)))))

(ert-deftest dorg-test-parse-dorg-id-allocated ()
  "Missing :DORG_ID: results in marker for write-back."
  (dorg-test-with-org-buffer "* Fresh\n"
    (let* ((result (dorg-parse-buffer))
           (markers (dorg-parse-result-new-heading-markers result)))
      ;; One new heading (not counting root)
      (should (= (length markers) 1))
      ;; Entry is (heading-struct . marker)
      (should (dorg-heading-p (car (nth 0 markers))))
      (should (markerp (cdr (nth 0 markers)))))))

(ert-deftest dorg-test-parse-dorg-id-stripped ()
  "DORG_ID property is not emitted as a property_entry."
  (dorg-test-with-parsed
      "* H\n:PROPERTIES:\n:DORG_ID: 42\n:CUSTOM: keep\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "H"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item)))
      ;; Only CUSTOM should appear, not DORG_ID
      (should (= (length drawers) 1))
      (let ((props (dorg-drawer-item-properties (car drawers))))
        (should (= (length props) 1))
        (should (equal (car (nth 0 props)) "CUSTOM"))))))

(ert-deftest dorg-test-parse-dorg-id-only-drawer-dropped ()
  "Property drawer containing only DORG_ID produces no drawer item."
  (dorg-test-with-parsed
      "* H\n:PROPERTIES:\n:DORG_ID: 42\n:END:\n"
    (let* ((h (dorg-test--find-heading headings "H"))
           (drawers (dorg-test--items-of-type h 'dorg-drawer-item))
           (all-items (dorg-heading-items h)))
      (should (= (length drawers) 0))
      (should (= (length all-items) 0)))))

(ert-deftest dorg-test-parse-dorg-id-malformed-errors ()
  "Malformed :DORG_ID: value signals an error."
  (dorg-test-with-org-buffer
      "* H\n:PROPERTIES:\n:DORG_ID: not-a-number\n:END:\n"
    (should-error (dorg-parse-buffer) :type 'error)))

(ert-deftest dorg-test-parse-dorg-id-strict-zero-signals ()
  "Strict parse of `:DORG_ID: 0' signals rather than returning id 0.
Behavior delta (tracked-case): the current reader validates `0' as a digit
run and returns integer 0 — silently minting a heading that COLLIDES with
the synthetic root.  Under the single strict policy `0' is present-but-
invalid (>0 guard), so the import boundary signals instead of admitting the
collision.  RED today (parse succeeds with id 0); GREEN once parse delegates
to the strict reader."
  (dorg-test-with-org-buffer
      "* H\n:PROPERTIES:\n:DORG_ID: 0\n:END:\n"
    (should-error (dorg-parse-buffer) :type 'error)))

(ert-deftest dorg-test-parse-dorg-id-float-signals ()
  "Strict parse of a float `:DORG_ID:' signals; a float never crosses import.
Behavior delta (tracked-case): `string-to-number' parses \"1.5\" -> 1.5, so the
raw >0 guard would admit a FLOAT identity into the store.  The integer
grammar rejects it; at the strict import boundary that is a signalled error,
so a non-integer DORG_ID can never be materialized as a heading id.  RED
today (parse succeeds, minting a heading whose id is the float 1.5)."
  (dorg-test-with-org-buffer
      "* H\n:PROPERTIES:\n:DORG_ID: 1.5\n:END:\n"
    (should-error (dorg-parse-buffer) :type 'error)))

(ert-deftest dorg-test-parse-dorg-id-mixed ()
  "Mixed headings: some with DORG_ID, some without."
  (dorg-test-with-org-buffer
      "* Keep\n:PROPERTIES:\n:DORG_ID: 7777\n:END:\n* Fresh\n"
    (let* ((result (dorg-parse-buffer))
           (markers (dorg-parse-result-new-heading-markers result))
           (headings (dorg-parse-result-headings result)))
      ;; 7777 is existing — should have id set directly by parser
      (let ((keep (dorg-test--find-heading headings "Keep")))
        (should (= (dorg-heading-id keep) 7777)))
      ;; Fresh heading has nil id before assign-ids
      (let ((fresh (dorg-test--find-heading headings "Fresh")))
        (should (null (dorg-heading-id fresh))))
      ;; One marker entry for the Fresh heading
      (should (= (length markers) 1))
      ;; Existing id not in markers
      (should-not (cl-some (lambda (e) (eql (dorg-heading-id (car e)) 7777))
                           markers)))))

;;; Integration Tests — Full Import to Dolt

(ert-deftest dorg-test-parse-import-headings ()
  "Import creates queryable headings in Dolt."
  (dorg-test-with-import "#+TITLE: Test\n* First\n** Nested\n* Second\n"
    (let ((result (dorg-sql dolt-dir "SELECT id, level_offset, title FROM headings ORDER BY id")))
      ;; Stored offsets: root 0, First 1, Nested 1 (level 2 under First), Second 1
      (should (= (length result) 4))
      (should (= (alist-get 'level_offset (nth 0 result)) 0))
      (should (equal (alist-get 'title (nth 1 result)) "First"))
      (should (= (alist-get 'level_offset (nth 1 result)) 1))
      (should (equal (alist-get 'title (nth 2 result)) "Nested"))
      (should (= (alist-get 'level_offset (nth 2 result)) 1))
      (should (equal (alist-get 'title (nth 3 result)) "Second"))
      (should (= (alist-get 'level_offset (nth 3 result)) 1)))))

(ert-deftest dorg-test-parse-import-keywords ()
  "Import stores file keywords queryable via SQL."
  (dorg-test-with-import "#+TITLE: Hello\n#+AUTHOR: World\n"
    (let ((result (dorg-sql dolt-dir
                            "SELECT ki.keyword, ki.value_raw FROM keyword_items ki JOIN heading_items hi ON ki.item_id = hi.id ORDER BY hi.sort_key")))
      (should (= (length result) 2))
      (should (equal (alist-get 'keyword (nth 0 result)) "TITLE"))
      (should (equal (alist-get 'value_raw (nth 0 result)) "Hello"))
      (should (equal (alist-get 'keyword (nth 1 result)) "AUTHOR"))
      (should (equal (alist-get 'value_raw (nth 1 result)) "World")))))

(ert-deftest dorg-test-parse-import-properties ()
  "Import stores properties queryable via heading_properties view."
  (dorg-test-with-import "* Heading\n:PROPERTIES:\n:ID: test-123\n:CATEGORY: work\n:END:\n"
    (let ((result (dorg-sql dolt-dir
                            "SELECT prop_key, prop_value FROM heading_properties ORDER BY ordinal")))
      (should (= (length result) 2))
      (should (equal (alist-get 'prop_key (nth 0 result)) "ID"))
      (should (equal (alist-get 'prop_value (nth 0 result)) "test-123"))
      (should (equal (alist-get 'prop_key (nth 1 result)) "CATEGORY"))
      (should (equal (alist-get 'prop_value (nth 1 result)) "work")))))

(ert-deftest dorg-test-parse-import-tags ()
  "Import stores heading tags."
  (dorg-test-with-import "* Tagged  :alpha:beta:\n"
    (let ((result (dorg-sql dolt-dir
                            "SELECT tag FROM heading_tags ORDER BY ordinal")))
      (should (= (length result) 2))
      (should (equal (alist-get 'tag (nth 0 result)) "alpha"))
      (should (equal (alist-get 'tag (nth 1 result)) "beta")))))

(ert-deftest dorg-test-parse-import-planning ()
  "Import stores planning timestamps."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-import "* TODO Task\nSCHEDULED: <2026-04-09 Thu>\n"
      (let ((result (dorg-sql dolt-dir
                              "SELECT scheduled_raw FROM planning_items")))
        (should (= (length result) 1))
        (should (string-match-p "2026-04-09"
                                (alist-get 'scheduled_raw (car result))))))))

(ert-deftest dorg-test-parse-import-timestampless-planning-shaped-text ()
  "Dolt import/materialize preserves planning-shaped prose as core text."
  (let ((body "Closed: this is an tracker feature request\n"))
    (dorg-test-with-import (concat "* Bead\n** Resolution\n" body)
      (let ((texts (dorg-sql
                    dolt-dir
                    "SELECT ti.text_raw FROM text_items ti JOIN heading_items hi ON hi.id = ti.item_id JOIN headings h ON h.id = hi.heading_id WHERE h.title = 'Resolution'"))
            (plans (dorg-sql
                    dolt-dir
                    "SELECT pi.item_id FROM planning_items pi JOIN heading_items hi ON hi.id = pi.item_id JOIN headings h ON h.id = hi.heading_id WHERE h.title = 'Resolution'"))
            (rendered (dorg-render dolt-dir)))
        (should (= (length texts) 1))
        (should (string= (alist-get 'text_raw (car texts)) body))
        (should-not plans)
        (should (string-match-p (regexp-quote body) rendered))))))

(ert-deftest dorg-test-parse-import-drawer ()
  "Import stores generic drawer with lines."
  (dorg-test-with-import "* Heading\n:MYLOG:\nEntry one\nEntry two\n:END:\n"
    (let ((result (dorg-sql dolt-dir
                            "SELECT name FROM drawer_items")))
      (should (= (length result) 1))
      (should (equal (alist-get 'name (car result)) "MYLOG")))
    (let ((result (dorg-sql dolt-dir
                            "SELECT line_raw FROM drawer_lines ORDER BY ordinal")))
      (should (= (length result) 2))
      (should (equal (alist-get 'line_raw (nth 0 result)) "Entry one"))
      (should (equal (alist-get 'line_raw (nth 1 result)) "Entry two")))))

;;; Previously-dropped element types

(ert-deftest dorg-test-parse-clock-element ()
  "CLOCK entries are captured as text items."
  (dorg-test-with-parsed
      "* Task\nCLOCK: [2026-04-09 Thu 10:00]--[2026-04-09 Thu 11:00] =>  1:00\n"
    (let* ((h (dorg-test--find-heading headings "Task"))
           (texts (dorg-test--items-of-type h 'dorg-text-item)))
      (should (>= (length texts) 1))
      (should (cl-some (lambda (ti) (string-match-p "CLOCK:"
                                                     (dorg-text-item-text ti)))
                       texts)))))

(ert-deftest dorg-test-parse-latex-environment ()
  "LaTeX environments are captured as text items."
  (dorg-test-with-parsed
      "* Math\n\\begin{equation}\nx = 42\n\\end{equation}\n"
    (let* ((h (dorg-test--find-heading headings "Math"))
           (texts (dorg-test--items-of-type h 'dorg-text-item)))
      (should (>= (length texts) 1))
      (should (cl-some (lambda (ti) (string-match-p "equation"
                                                     (dorg-text-item-text ti)))
                       texts)))))

(ert-deftest dorg-test-parse-footnote-definition ()
  "Footnote definitions are captured as text items."
  (dorg-test-with-parsed
      "* Notes\n[fn:1] This is a footnote.\n"
    (let* ((h (dorg-test--find-heading headings "Notes"))
           (texts (dorg-test--items-of-type h 'dorg-text-item)))
      (should (>= (length texts) 1))
      (should (cl-some (lambda (ti) (string-match-p "footnote"
                                                     (dorg-text-item-text ti)))
                       texts)))))

(ert-deftest dorg-test-parse-special-block ()
  "Special blocks (#+BEGIN_name) are captured as text items."
  (dorg-test-with-parsed
      "* Content\n#+BEGIN_WARNING\nDanger!\n#+END_WARNING\n"
    (let* ((h (dorg-test--find-heading headings "Content"))
           (texts (dorg-test--items-of-type h 'dorg-text-item)))
      (should (>= (length texts) 1))
      (should (cl-some (lambda (ti) (string-match-p "WARNING"
                                                     (dorg-text-item-text ti)))
                       texts)))))

;;; Characterization — Parse Granularity Equivalence

;; dorg consumes element boundaries and raw buffer substrings, never inline
;; object trees, so `element' granularity should produce a model identical to
;; the default/object granularity for every dorg-supported construct.  These
;; tests pin that equivalence and make the granularity switch non-vacuous by
;; spying on the actual `org-element-parse-buffer' calls.

(defun dorg-test--heading-fingerprint (h)
  "Return a comparable plist of the DB-bound fields of heading H.
Captures every attribute dorg persists plus `items-hash' (an MD5 of the
structured item content), so two fingerprints are `equal' iff the
parsed models are equivalent."
  (list :id (dorg-heading-id h)
        :parent-id (dorg-heading-parent-id h)
        :level (dorg-heading-level h)
        :title (dorg-heading-title h)
        :todo (dorg-heading-todo h)
        :priority (dorg-heading-priority h)
        :commented (dorg-heading-commented h)
        :tags (dorg-heading-tags h)
        :sort-key (dorg-heading-sort-key h)
        :items-hash (dorg-heading-items-hash h)))

(defun dorg-test--parse-with-granularity-spy (contents gran)
  "Parse CONTENTS with a test-owned Org parser override to GRAN.
Returns a cons (HEADINGS . GRANS-SEEN) where HEADINGS is the id-assigned
heading list and GRANS-SEEN is the list of granularity arguments that
the test passed to the saved `org-element-parse-buffer' implementation.
Production still requests its fixed granularity; only this characterization
fixture substitutes GRAN at the Org parser boundary."
  (dorg-test-with-org-buffer contents
    (let ((grans nil)
          (orig (symbol-function 'org-element-parse-buffer)))
      (cl-letf (((symbol-function 'org-element-parse-buffer)
                 (lambda (&optional _production-granularity &rest rest)
                   (push gran grans)
                   (apply orig gran rest))))
        (let* ((result (dorg-parse-buffer))
               (counter 0)
               (_wb (dorg-parse-assign-ids
                     result (lambda () (cl-incf counter)))))
          (cons (dorg-parse-result-headings result) (nreverse grans)))))))

(defconst dorg-test--granularity-sample
  (concat
   "#+TITLE: Doc\n#+AUTHOR: Me\n"
   "Preamble paragraph with /emphasis/ and a [[https://example.com][link]].\n\n"
   "* TODO Top  :alpha:beta:\n"
   "SCHEDULED: <2026-04-09 Thu> DEADLINE: <2026-04-10 Fri>\n"
   ":PROPERTIES:\n:DORG_ID: 9001\n:CUSTOM: value\n:END:\n"
   "Body paragraph with *bold* text.\n"
   ":LOGBOOK:\nNote one\nNote two\n:END:\n"
   "** Child\nChild body.\n*** Grandchild\n"
   "* Second\n")
  "Sample exercising every dorg-supported construct in one buffer.
Covers properties/DORG_ID, planning, generic drawers, keywords,
paragraphs/inline markup, nested headings, tags, and root preamble.")

(ert-deftest dorg-test-parse-granularity-equivalence ()
  "`element' granularity yields the same model as default/object parse."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (let* ((default-arm (dorg-test--parse-with-granularity-spy
                         dorg-test--granularity-sample nil))
           (element-arm (dorg-test--parse-with-granularity-spy
                         dorg-test--granularity-sample 'element))
           (default-headings (car default-arm))
           (element-headings (car element-arm))
           (default-grans (cdr default-arm))
           (element-grans (cdr element-arm)))
      ;; Non-vacuity: the two arms actually parsed at different granularities.
      ;; If both arms used the same granularity, these assertions fail and the
      ;; equivalence assertion below would be comparing identical parses.
      (should (member nil default-grans))
      (should (member 'element element-grans))
      (should-not (member 'element default-grans))
      (should-not (member nil element-grans))
      ;; Equivalence: identical model across the whole construct matrix.
      (should (= (length default-headings) (length element-headings)))
      (should (equal (mapcar #'dorg-test--heading-fingerprint default-headings)
                     (mapcar #'dorg-test--heading-fingerprint
                             element-headings))))))

(ert-deftest dorg-test-parse-buffer-single-parse-call ()
  "`dorg-parse-buffer' performs one fixed-element Org parse.
Guards against both accidental double-parsing and granularity drift."
  (let ((org-todo-keywords-1 '("TODO" "DONE")))
    (dorg-test-with-org-buffer dorg-test--granularity-sample
      (let ((grans nil)
            (orig (symbol-function 'org-element-parse-buffer)))
        (cl-letf (((symbol-function 'org-element-parse-buffer)
                   (lambda (&optional gran &rest rest)
                     (push gran grans)
                     (apply orig gran rest))))
          (dorg-parse-buffer))
        (should (equal (nreverse grans) '(element)))))))

(ert-deftest dorg-test-parse-granularity-configuration-is-retired ()
  "Production exposes no parser-granularity configuration surface."
  (should-not (boundp (intern (concat "dorg-parse-" "granularity")))))

(ert-deftest dorg-test-parse-region-helpers-element-granularity ()
  "Narrowed region helpers parse via the shared element-granularity path."
  (dorg-test-with-org-buffer
      "Preamble text.\n* One\n:PROPERTIES:\n:DORG_ID: 11\n:END:\n** Two\n"
    (let ((grans nil)
          (orig (symbol-function 'org-element-parse-buffer)))
      (cl-letf (((symbol-function 'org-element-parse-buffer)
                 (lambda (&optional g &rest rest)
                   (push g grans)
                   (apply orig g rest))))
        ;; Root section helper builds the level-0 root from the preamble.
        (goto-char (point-min))
        (let* ((first-heading (progn (re-search-forward org-outline-regexp-bol)
                                     (line-beginning-position)))
               (root (dorg-parse-root-section-region 1 (point-min) first-heading)))
          (should (= (dorg-heading-level root) 0))
          (should (= (dorg-heading-id root) 1))
          (should (dorg-test--items-of-type root 'dorg-text-item)))
        ;; Subtree helper parses complete subtrees under an explicit parent.
        (let* ((start (progn (goto-char (point-min))
                             (re-search-forward "^\\* One")
                             (line-beginning-position)))
               (parsed (dorg-parse-subtree-region 1 start (point-max)))
               (heads (nth 0 parsed)))
          (should (= (length heads) 2))
          (let ((one (cl-find-if (lambda (h) (equal (dorg-heading-title h) "One"))
                                 heads)))
            (should one)
            (should (= (dorg-heading-id one) 11))
            (should (= (dorg-heading-parent-id one) 1)))))
      ;; Every parse routed through element granularity.
      (should grans)
      (should (cl-every (lambda (g) (eq g 'element)) grans)))))

;;; Unit Tests — Section Projection Helper

(ert-deftest dorg-test-parse-section-items-at-heading-matches-full-parse ()
  "`dorg-parse-section-items-at-heading' equals the full parser's items.
For each heading in a rich buffer, the items projected at point are the
same `dorg-heading-item' objects (by content string) the full-buffer
parse attaches to that heading — proving the shared projection path."
  (let ((contents "* Alpha
SCHEDULED: <2026-03-01 Sun>
:PROPERTIES:
:DORG_ID: 1
:DUE: <2026-04-01 Wed>
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-02-01 Sun]
:END:
Body text with an active <2026-05-01 Fri> stamp.

** Beta
Some other body [2026-01-01 Thu].
"))
    (dorg-test-with-org-buffer contents
      (let ((by-title (make-hash-table :test 'equal)))
        ;; Full parse: collect each non-root heading's item content strings.
        (dolist (h (dorg-parse-result-headings (dorg-parse-buffer)))
          (when (> (dorg-heading-level h) 0)
            (puthash (dorg-heading-title h)
                     (mapcar #'dorg-item-content-string (dorg-heading-items h))
                     by-title)))
        ;; Walk each heading and compare the projected items at point.
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (beginning-of-line)
          (let* ((title (org-get-heading t t t t))
                 (projected (mapcar #'dorg-item-content-string
                                    (dorg-parse-section-items-at-heading))))
            (should (equal projected (gethash title by-title))))
          (end-of-line))))))

(ert-deftest dorg-test-parse-section-items-at-heading-ignores-block-drawer ()
  "A fake `:LOGBOOK:' inside a source block is never a drawer item.
The helper inherits `dorg-parse--process-section's direct-section-child
rules, so block content does not leak in as a LOGBOOK drawer."
  (dorg-test-with-org-buffer "* Heading
#+begin_src org
:LOGBOOK:
- State \"DONE\" [2026-02-01 Sun]
:END:
#+end_src
"
    (goto-char (point-min))
    (re-search-forward org-heading-regexp)
    (beginning-of-line)
    (let ((items (dorg-parse-section-items-at-heading)))
      ;; No drawer item named LOGBOOK was produced.
      (should-not (cl-some (lambda (it)
                             (and (cl-typep it 'dorg-drawer-item)
                                  (equal (dorg-drawer-item-name it) "LOGBOOK")))
                           items)))))

;;; Phantom Headings — example/source-block sample headings
;;
;; Org promotes any column-0 heading-looking line to a real headline,
;; even inside a `#+begin_example' block (a headline can never be
;; contained in a block, so it force-closes the block).  Unescaped sample
;; headings in bead bodies therefore parse as real headings; when they
;; carry a literal `:DORG_ID:' they collide on the headings primary key
;; and abort the Dolt import.  The parser's literal verbatim-region scan
;; drops headings that begin inside a closed `#+begin_…'/`#+end_…' pair.

(ert-deftest dorg-test-parse-phantom-simple-example-dropped ()
  "A heading inside a single closed example block is not emitted.
Fails if the verbatim-skip is removed (the phantom would be present)."
  (dorg-test-with-parsed "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
*** QuoteFake
:PROPERTIES:
:DORG_ID: 1
:END:
#+end_example
After text.
"
    ;; root + Real bead only; the phantom is gone.
    (should (= (length headings) 2))
    (should (dorg-test--find-heading headings "Real bead"))
    (should-not (dorg-test--find-heading headings "QuoteFake"))))

(ert-deftest dorg-test-parse-phantom-nested-example-dropped ()
  "The real-tracker nested-example shape drops the phantom heading.
`#+begin_example' is reopened inside the block (org example blocks do not
nest); the first matching `#+end_example' closes the literal region, so
`*** H' is recognised as verbatim sample text."
  (dorg-test-with-parsed "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
*** H
:PROPERTIES:
:DORG_ID: 1
:END:
#+begin_example
:LOGBOOK:
- fake [2026-05-10 Sun]
:END:
#+end_example
#+end_example
tail
"
    (should (= (length headings) 2))
    (should (dorg-test--find-heading headings "Real bead"))
    (should-not (dorg-test--find-heading headings "H"))))

(ert-deftest dorg-test-parse-phantom-quote-and-list-shapes-dropped ()
  "Both the QuoteFake and ListFake real-tracker shapes are dropped.
These mirror a large real-world tracker file: heading-looking lines
inside example blocks that also embed quote-block / list-item bodies."
  (dorg-test-with-parsed "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
*** QuoteFake
:PROPERTIES:
:DORG_ID: 1
:END:
#+begin_quote
:LOGBOOK:
- quote fake [2026-05-10 Sun]
:END:
#+end_quote
#+end_example
#+begin_example
*** ListFake
:PROPERTIES:
:DORG_ID: 2
:END:
- item
  :LOGBOOK:
  - nested [2026-05-10 Sun]
  :END:
#+end_example
"
    (should (= (length headings) 2))
    (should (dorg-test--find-heading headings "Real bead"))
    (should-not (dorg-test--find-heading headings "QuoteFake"))
    (should-not (dorg-test--find-heading headings "ListFake"))))

(ert-deftest dorg-test-parse-phantom-with-real-child-reparented ()
  "A real heading org nested under a phantom survives, re-parented.
Org makes `*** RealChild' a child of the phantom `** Phantom'; once the
phantom is skipped, RealChild must attach to the phantom's parent (the
real bead), not be dropped and not keep the phantom as parent.  Fails if
the skip drops the subtree or assigns the phantom as parent."
  (dorg-test-with-parsed "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
** Phantom
body
#+end_example
*** RealChild
:PROPERTIES:
:DORG_ID: 200
:END:
"
    ;; root + Real bead + RealChild; Phantom dropped.
    (should (= (length headings) 3))
    (should-not (dorg-test--find-heading headings "Phantom"))
    (let ((bead (dorg-test--find-heading headings "Real bead"))
          (child (dorg-test--find-heading headings "RealChild")))
      (should bead)
      (should child)
      (should (= (dorg-heading-parent-id child) (dorg-heading-id bead))))))

(ert-deftest dorg-test-parse-unterminated-example-no-overconsume ()
  "An unterminated example block yields no region, so it cannot drop
real headings that follow it.  Guards the literal-close safety rule: a
`#+begin_example' with no matching `#+end_example' must not swallow the
rest of the buffer."
  (dorg-test-with-parsed "* Bead one
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
unterminated example body
* Bead two
:PROPERTIES:
:DORG_ID: 200
:END:
"
    ;; root + Bead one + Bead two — Bead two is NOT consumed.
    (should (= (length headings) 3))
    (should (dorg-test--find-heading headings "Bead one"))
    (should (dorg-test--find-heading headings "Bead two"))))

(ert-deftest dorg-test-parse-real-example-content-preserved ()
  "A closed example block with no heading-looking lines parses normally.
Only column-0 phantom headings are dropped; ordinary verbatim content
stays as body text on its heading."
  (dorg-test-with-parsed "* Bead with code
:PROPERTIES:
:DORG_ID: 100
:END:
Intro.
#+begin_example
not a heading
just text
#+end_example
Outro.
"
    (should (= (length headings) 2))
    (let* ((bead (dorg-test--find-heading headings "Bead with code"))
           (texts (dorg-test--items-of-type bead 'dorg-text-item)))
      (should bead)
      (should (cl-some (lambda (ti)
                         (string-match-p "not a heading"
                                         (dorg-text-item-text ti)))
                       texts)))))

;;; Unit Tests — Verbatim Region Bound

(ert-deftest dorg-test-parse-bounded-unbalanced-block-no-overreach ()
  "An unbalanced `#+begin_example' must not engulf the real beads between
it and a distant `#+end_example'.  This is the
materialize-reparse loss: org force-closes the block at `*** Sample',
but the literal close sits past two more real beads, so the flat scanner
used to bridge across them.  The one-headline region bound abandons the
block once a SECOND column-0 headline appears before the matching end.
Fails before the bound (Bead two / Bead three swallowed as sample text)."
  (dorg-test-with-parsed "* Bead one
:PROPERTIES:
:DORG_ID: 100
:END:
prose
#+begin_example
*** Sample
sample body
* Bead two
:PROPERTIES:
:DORG_ID: 200
:END:
* Bead three
:PROPERTIES:
:DORG_ID: 300
:END:
#+end_example
tail
"
    (should (dorg-test--find-heading headings "Bead one"))
    (should (dorg-test--find-heading headings "Bead two"))
    (should (dorg-test--find-heading headings "Bead three"))))

(ert-deftest dorg-test-parse-bounded-balanced-block-still-phantom ()
  "The one-headline bound preserves d4bg phantom detection.  A sample
heading inside a block whose matching `#+end_example' arrives before any
further headline is still dropped; the real bead after the block is
untouched.  Guards against the bound over-correcting and re-admitting
sample headings."
  (dorg-test-with-parsed "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
*** Sample
:PROPERTIES:
:DORG_ID: 1
:END:
sample body
#+end_example
* Next bead
:PROPERTIES:
:DORG_ID: 200
:END:
"
    (should (dorg-test--find-heading headings "Real bead"))
    (should (dorg-test--find-heading headings "Next bead"))
    (should-not (dorg-test--find-heading headings "Sample"))))

(ert-deftest dorg-test-parse-bounded-stray-begin-in-prose ()
  "A stray column-0 `#+begin_src' whose only matching close sits beyond
real beads does not consume those beads — not even the first heading
after it.  Distinct from the unterminated case (no close at all): here a
distant `#+end_src' exists, and the old flat scanner bridged to it.
Fails before the bound (Bead two / Bead three dropped)."
  (dorg-test-with-parsed "* Doc bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_src elisp
* Bead two
:PROPERTIES:
:DORG_ID: 200
:END:
* Bead three
:PROPERTIES:
:DORG_ID: 300
:END:
#+end_src
"
    (should (dorg-test--find-heading headings "Doc bead"))
    (should (dorg-test--find-heading headings "Bead two"))
    (should (dorg-test--find-heading headings "Bead three"))))

(ert-deftest dorg-test-parse-bounded-render-reparse-identity ()
  "Render a stored tree whose bead body holds an unbalanced
`#+begin_example' (its close was split into a later bead on import), then
re-parse the rendered org: the sibling beads must survive.  This is the
end-to-end verbatim round-trip in miniature; fails before the
region bound (the siblings are swallowed when the lone begin bridges to
the later end)."
  (let* ((root (make-dorg-heading :id 1 :level 0 :level-offset 0 :title nil))
         (a (make-dorg-heading
             :id 100 :parent-id 1 :level 1 :level-offset 1 :title "Bead A"
             :sort-key (dorg-sortkey-from-integer 1 2)
             :items (list (dorg-text-item
                           :text "#+begin_example\n*** Sample\nsample body"))))
         (b (make-dorg-heading :id 200 :parent-id 1 :level 1 :level-offset 1 :title "Bead B"
                               :sort-key (dorg-sortkey-from-integer 2 2)))
         (c (make-dorg-heading
             :id 300 :parent-id 1 :level 1 :level-offset 1 :title "Bead C"
             :sort-key (dorg-sortkey-from-integer 3 2)
             :items (list (dorg-text-item :text "#+end_example\ntail"))))
         (text (dorg-model-render-tree (list root a b c))))
    ;; Sanity: the rendered org really is the unbalanced split shape.
    (should (string-match-p "#\\+begin_example" text))
    (should (string-match-p "#\\+end_example" text))
    (dorg-test-with-parsed text
      (should (dorg-test--find-heading headings "Bead A"))
      (should (dorg-test--find-heading headings "Bead B"))
      (should (dorg-test--find-heading headings "Bead C")))))

;;; Verbatim Block Comma-Escape Round-Trip
;;
;; Durable follow-up to the verbatim region-bound fix, which stopped bead LOSS; these
;; cover content FIDELITY: a verbatim block carrying a column-0 `*'/`#+'
;; line is comma-escaped on render and unescaped on parse so DOrg's own
;; model -> render -> parse -> model loop is lossless and idempotent.
;; Option B only; bare-legacy first-parse recovery (Option A) is deferred.

;; -- A. Helper unit tests (pure string) ---------------------------------

(ert-deftest dorg-test-escape-verbatim-bodies-escapes-block-interior ()
  "Only the BODY of a verbatim block is comma-escaped; delimiters intact.
Fails if `dorg--escape-verbatim-bodies' is a pass-through (vacuity guard)."
  (should (string= (dorg--escape-verbatim-bodies
                    "#+begin_example\n*** X\n#+end_example\n")
                   "#+begin_example\n,*** X\n#+end_example\n")))

(ert-deftest dorg-test-escape-verbatim-bodies-ignores-prose ()
  "A column-0 line OUTSIDE any block is never escaped; only block bodies are.
The leading heading-looking prose line is left bare; the in-block `#+k'
becomes `,#+k'."
  (should (string= (dorg--escape-verbatim-bodies
                    "* real prose heading\n#+begin_example\n#+k\n#+end_example\n")
                   "* real prose heading\n#+begin_example\n,#+k\n#+end_example\n")))

(ert-deftest dorg-test-escape-verbatim-bodies-nonmatching-close-is-content ()
  "A differently named close inside a block is unambiguous CONTENT.
`#+end_src' inside `#+begin_example' is body (first-matching-NAME close
rule), so it is comma-escaped like any `#+' line.  A SAME-name literal
`#+end_example' as content is indistinguishable from the real delimiter
\(org's own limitation) and must be pre-escaped by the author; that case
is intentionally not exercised here."
  (should (string= (dorg--escape-verbatim-bodies
                    "#+begin_example\n#+end_src\n#+end_example\n")
                   "#+begin_example\n,#+end_src\n#+end_example\n")))

(ert-deftest dorg-test-unescape-verbatim-bodies-inverse ()
  "Unescape is the exact inverse of escape for a multi-line block body."
  (let ((s "#+begin_example\n*** A\n#+k\nplain body\n#+end_example\n"))
    (should (string= (dorg--unescape-verbatim-bodies
                      (dorg--escape-verbatim-bodies s))
                     s))))

(ert-deftest dorg-test-escape-verbatim-bodies-unterminated-noop ()
  "An unterminated block contributes no body region, so escape is a no-op.
Mirrors the unterminated-block safety: no over-reach past the open."
  (let ((s "#+begin_example\n*** X\nstill open"))
    (should (string= (dorg--escape-verbatim-bodies s) s))))

(ert-deftest dorg-test-escape-verbatim-bodies-clean-noop ()
  "Text with no block, or a block with no escapable line, is unchanged."
  (should (string= (dorg--escape-verbatim-bodies "just prose\n* not in a block\n")
                   "just prose\n* not in a block\n"))
  (should (string= (dorg--escape-verbatim-bodies
                    "#+begin_example\nplain only\n#+end_example\n")
                   "#+begin_example\nplain only\n#+end_example\n")))

;; -- B. Parse-side unescape ---------------------------------------------

(ert-deftest dorg-test-parse-unescapes-verbatim-body ()
  "Parsing a buffer whose block body is comma-escaped stores TRUE content.
The commas keep org from force-closing, so the block stays whole and the
text item holds the unescaped `*** Sample' / `#+k'.  Fails if the
unescape on parse is removed (the comma survives in the stored text)."
  (dorg-test-with-parsed "* Bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
,*** Sample
,#+k
#+end_example
"
    (let* ((bead (dorg-test--find-heading headings "Bead"))
           (ti (and bead (cl-find-if (lambda (i) (cl-typep i 'dorg-text-item))
                                     (dorg-heading-items bead)))))
      (should bead)
      (should ti)
      (should (string-match-p "^\\*\\*\\* Sample$" (dorg-text-item-text ti)))
      (should (string-match-p "^#\\+k$" (dorg-text-item-text ti)))
      (should-not (string-match-p ",\\*\\*\\* Sample" (dorg-text-item-text ti))))))

(ert-deftest dorg-test-parse-escaped-block-no-phantom ()
  "A comma-escaped block body produces NO phantom heading and no bead loss.
The escaped `,*** Sample' is not promoted, so the block stays intact and
only the real bead (plus root) is emitted."
  (dorg-test-with-parsed "* Bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
,*** Sample
#+end_example
"
    (should (= (length headings) 2))
    (should (dorg-test--find-heading headings "Bead"))
    (should-not (dorg-test--find-heading headings "Sample"))))

;; -- C. Full round-trip fidelity (model -> render -> parse) -------------

(ert-deftest dorg-test-roundtrip-verbatim-column0-heading-preserved ()
  "PRIMARY: a verbatim block with a column-0 heading survives a round-trip
BYTE-IDENTICALLY.  Render comma-escapes the interior so org keeps the
block whole; re-parse unescapes back to the original body.  Pre-fix the
reparse drops everything after `#+begin_example' and promotes a phantom,
so both the no-phantom and the byte-identity assertions fail (vacuity
guard: deleting either the escape or the unescape flips this red)."
  (let* ((body "#+begin_example\n*** Sample heading\nsample body line\n#+end_example\n")
         (h (make-dorg-heading
             :id 100 :parent-id nil :level 1 :level-offset 1 :title "Real bead"
             :items (list (dorg-text-item :text body))))
         (s1 (dorg-model-render-heading-section h)))
    ;; Render escaped the column-0 heading line.
    (should (string-match-p ",\\*\\*\\* Sample heading" s1))
    (dorg-test-with-parsed s1
      (let* ((rb (dorg-test--find-heading headings "Real bead"))
             (ti (and rb (cl-find-if (lambda (i) (cl-typep i 'dorg-text-item))
                                     (dorg-heading-items rb)))))
        ;; No phantom heading promoted from the sample line.
        (should-not (dorg-test--find-heading headings "Sample heading"))
        (should rb)
        (should ti)
        ;; Content recovered exactly.
        (should (string= (dorg-text-item-text ti) body))))))

(ert-deftest dorg-test-roundtrip-verbatim-idempotent-fixpoint ()
  "A second render -> parse -> render cycle is a fixpoint: no double-escape.
Escape and unescape are paired, so the model always holds unescaped
content and render emits the same bytes each cycle (no `,,*')."
  (let* ((body "#+begin_example\n*** Sample\n#+end_example\n")
         (h (make-dorg-heading
             :id 100 :parent-id nil :level 1 :level-offset 1 :title "Bead"
             :items (list (dorg-text-item :text body))))
         (s1 (dorg-model-render-heading-section h)))
    (should-not (string-match-p ",,\\*" s1))
    (dorg-test-with-parsed s1
      (let* ((rb (dorg-test--find-heading headings "Bead"))
             (s2 (dorg-model-render-heading-section rb)))
        (should (string= s1 s2))
        (should-not (string-match-p ",,\\*" s2))))))

;; -- D. Same-name verbatim terminator marker-depth preservation ---------
;;
;; tracked-case: a comma-escaped SAME-NAME `#+end_NAME' inside a block body is
;; irreducible literal content.  It stays unambiguous only while it retains a
;; comma: the shared close detector
;; (`dorg--map-verbatim-block-bodies' / `dorg-parse--verbatim-regions')
;; matches a close by the UNANCHORED name token `[ \t]*#+end_\([^ \t\n]+\)'
;; (case-insensitive, trailing content ignored).  If unescape strips the last
;; comma, the terminator collapses to a bare structural close: the mapper then
;; treats it as a FALSE close, truncating the block and promoting its tail into
;; live Org structure.  Fix: same-name terminators (`^[ \t]*,+#+end_<open>')
;; pass VERBATIM in both directions (no strip, no add); every other body line
;; keeps normal one-comma handling.

(defun dorg-test-verbatim--m34u-fixture ()
  "Return the exact tracker-cli-m34u witness (utf-8-unix decoded), 7275 bytes.
The pinned downstream fixture: an outer `#+begin_example sh' whose body
nests an indented same-name `     ,#+end_example' terminator (line 29)."
  (let* ((root (or (locate-dominating-file default-directory "dorg.el")
                   default-directory))
         (fixture (expand-file-name
                   "tests/fixtures/verbatim-terminator/tracker-cli-m34u-description.txt"
                   root)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents fixture))
      (buffer-string))))

(defun dorg-test-verbatim--render-tree-once (text)
  "Parse TEXT and render the model tree back to org text (one full cycle).
Exercises the real parser and tree renderer, not just the escape/unescape
helpers: `dorg-parse-buffer' -> model -> `dorg-model-render-tree'."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (dorg-model-render-tree
     (dorg-parse-result-headings (dorg-parse-buffer (current-buffer) 1)))))

(ert-deftest dorg-test-verbatim-same-name-terminator-depth1-unescape-preserves ()
  "Depth-1 `,#+end_NAME' keeps its comma through unescape (parse side).
Pre-fix, unescape strips it to a bare `#+end_example', which is then a false
structural close.  RED: fails on trunk (comma stripped)."
  (let ((s "#+begin_example\n,#+end_example\n#+end_example\n"))
    (should (string= (dorg--unescape-verbatim-bodies s) s))))

(ert-deftest dorg-test-verbatim-same-name-terminator-depth1-escape-preserves ()
  "Depth-1 `,#+end_NAME' in the model renders verbatim (no added comma).
Pre-fix, escape adds a comma -> `,,#+end_example'.  RED: fails on trunk."
  (let ((s "#+begin_example\n,#+end_example\n#+end_example\n"))
    (should (string= (dorg--escape-verbatim-bodies s) s))))

(ert-deftest dorg-test-verbatim-same-name-terminator-depth1-roundtrip-2cycle ()
  "Depth-1 disk form is a fixpoint of escape.unescape across >=2 cycles.
Each cycle is one materialize round-trip (unescape=parse, escape=render).
Pre-fix the first cycle already drops the comma (bare terminator).  RED."
  (let* ((s "#+begin_example\n,#+end_example\n#+end_example\n")
         (rt1 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies s)))
         (rt2 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies rt1))))
    (should (string= rt1 s))
    (should (string= rt2 s))))

(ert-deftest dorg-test-verbatim-same-name-terminator-depth2-model-full-run ()
  "Depth-2 `,,#+end_NAME' keeps its FULL comma run in the model and on render.
Non-vacuity: a round-trip-only depth-2 assertion PASSES on trunk (unescape
,,->, then escape ,->,,), so this asserts the MODEL content instead — trunk
reduces to `,#+end_example' (unescape) and inflates to `,,,#+end_example'
\(escape).  RED: both fail on trunk."
  (let ((s "#+begin_example\n,,#+end_example\n#+end_example\n"))
    (should (string= (dorg--unescape-verbatim-bodies s) s))
    (should (string= (dorg--escape-verbatim-bodies s) s))))

(ert-deftest dorg-test-verbatim-same-name-terminator-generic-mixedcase-boundary ()
  "Generic, case-insensitive NAME with indentation and a trailing token.
Open `#+begin_FooBar org', terminator `   ,#+end_fooBar sh' (case differs
from the open name; indented; a trailing token after the name exercises the
UNANCHORED `[^ \t\n]+' close-token boundary).  A hardcoded `example'-only
fix cannot satisfy this.  RED: escape.unescape drops the comma on trunk."
  (let* ((s "#+begin_FooBar org\n   ,#+end_fooBar sh\n#+end_FooBar\n")
         (rt1 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies s)))
         (rt2 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies rt1))))
    ;; The model retains the full comma run on the terminator.
    (should (string= (dorg--unescape-verbatim-bodies s) s))
    ;; Byte-stable across >=2 materialize cycles.
    (should (string= rt1 s))
    (should (string= rt2 s))))

(ert-deftest dorg-test-verbatim-same-name-terminator-parse-preserves-in-model ()
  "End-to-end: parsing disk form keeps the terminator's comma in the model.
The block body holds a depth-1 same-name terminator followed by tail content
and the real close.  Pre-fix the parsed item-text loses the comma (bare
`#+end_example'), which on re-render would escape the block early and promote
the tail.  RED: item-text is bare on trunk."
  (dorg-test-with-parsed "* Outer
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
,#+end_example
tail content after terminator
#+end_example
* Sibling
:PROPERTIES:
:DORG_ID: 200
:END:
body
"
    (let* ((outer (dorg-test--find-heading headings "Outer"))
           (ti (and outer (cl-find-if (lambda (i) (cl-typep i 'dorg-text-item))
                                      (dorg-heading-items outer)))))
      (should outer)
      (should ti)
      ;; The same-name terminator survives with its comma; the tail stays
      ;; inside the block (no early close, no promotion).
      (should (string-match-p "^,#\\+end_example$" (dorg-text-item-text ti)))
      (should (string-match-p "tail content after terminator"
                              (dorg-text-item-text ti)))
      ;; The real sibling heading is unaffected.
      (should (dorg-test--find-heading headings "Sibling")))))

(ert-deftest dorg-test-verbatim-same-name-terminator-bare-close-is-real-close ()
  "CONTROL: a bare same-name `#+end_NAME' is the real close, untouched.
The fix only sees body lines; the delimiter is consumed by the mapper.  A
plain block (empty or plain body) is an identity for both helpers.  Passes on
trunk and after the fix (regression guard)."
  (let ((empty "#+begin_example\n#+end_example\n")
        (plain "#+begin_example\nplain body\n#+end_example\n"))
    (should (string= (dorg--escape-verbatim-bodies empty) empty))
    (should (string= (dorg--unescape-verbatim-bodies empty) empty))
    (should (string= (dorg--escape-verbatim-bodies plain) plain))
    (should (string= (dorg--unescape-verbatim-bodies plain) plain))))

(ert-deftest dorg-test-verbatim-same-name-terminator-nonmatching-close-normal ()
  "CONTROL: a DIFFERENTLY named close keeps normal one-comma handling.
`#+end_Src foo' inside `#+begin_example' (name `src' != `example') is
unambiguous content: unescape strips one comma, escape adds one.  The fix
must not over-capture by name.  Passes on trunk and after the fix."
  (let ((disk "#+begin_example\n,#+end_Src foo\n#+end_example\n")
        (model "#+begin_example\n#+end_Src foo\n#+end_example\n"))
    (should (string= (dorg--unescape-verbatim-bodies disk) model))
    (should (string= (dorg--escape-verbatim-bodies model) disk))))

(ert-deftest dorg-test-verbatim-same-name-terminator-m34u-witness ()
  "Exact downstream witness: the tracker-cli-m34u description round-trips byte-exact.
The 7275-byte fixture nests `#+begin_example sh' with an indented same-name
`     ,#+end_example' (line 29).  Pre-fix, escape.unescape drops that comma
\(7275 -> 7274 bytes).  RED asserts the fixture's identity and
its stability across >=2 materialize cycles by exact byte length AND SHA-256."
  (let* ((orig (dorg-test-verbatim--m34u-fixture))
         (sha "f0c25b3d07c008af3058563ccf0f7d737310d45e81d345f9ed711f1527fa4016")
         (rt1 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies orig)))
         (rt2 (dorg--escape-verbatim-bodies (dorg--unescape-verbatim-bodies rt1))))
    ;; Fixture is the pinned witness.
    (should (= (string-bytes orig) 7275))
    (should (string= (secure-hash 'sha256 orig) sha))
    ;; One materialize cycle preserves it byte- and hash-exact.
    (should (string= rt1 orig))
    (should (= (string-bytes rt1) 7275))
    (should (string= (secure-hash 'sha256 rt1) sha))
    ;; A second cycle is a fixpoint.
    (should (string= rt2 orig))))

(ert-deftest dorg-test-verbatim-same-name-terminator-m34u-witness-full-cycle ()
  "Exact m34u witness through the full DOrg parse -> model -> render loop.
Wraps the fixture under a canonical local-identity heading and drives the
real parser (`dorg-parse-buffer') and tree renderer (`dorg-model-render-tree'),
not just the escape/unescape helpers.  Pre-fix, the first render corrupts the
nested same-name terminator (line 29) so the reconstructed body diverges from
the fixture (a hash mismatch even though the byte length coincides) and the
render is not the input's identity.  RED asserts a byte- and hash-exact
fixpoint across >=2 render cycles.  The header round-trips unchanged (the fix
touches only verbatim bodies), so the body suffix isolates the witness.

The raw witness has no terminal newline; a rendered org document is
canonically newline-terminated, so the wrapped input carries one explicit
final newline and the extracted body is expected to end in exactly one.  That
single canonical newline is normal document termination, not marker drift —
it is stripped before comparing the 7275-byte / SHA-256 witness."
  (let* ((orig (dorg-test-verbatim--m34u-fixture))
         (sha "f0c25b3d07c008af3058563ccf0f7d737310d45e81d345f9ed711f1527fa4016")
         (header "* Bead\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n")
         (wrapped (concat header orig "\n"))
         (render1 (dorg-test-verbatim--render-tree-once wrapped))
         (render2 (dorg-test-verbatim--render-tree-once render1)))
    ;; Full parse->render is the identity on the canonical wrapped disk form...
    (should (string= wrapped render1))
    ;; ...and a second cycle is a byte-identical fixpoint.
    (should (string= render1 render2))
    ;; The reconstructed body isolates the witness after the header.
    (should (string-prefix-p header render1))
    (let ((body1 (substring render1 (length header))))
      ;; Exactly one canonical terminal newline.
      (should (string-suffix-p "\n" body1))
      (should-not (string-suffix-p "\n\n" body1))
      ;; Stripping it recovers the exact fixture, byte- and hash-identical.
      (let ((witness (substring body1 0 -1)))
        (should (string= witness orig))
        (should (= (string-bytes witness) 7275))
        (should (string= (secure-hash 'sha256 witness) sha))))))

(ert-deftest dorg-test-parse-duplicate-id-guard ()
  "`dorg-parse-assign-ids' fails loudly on a duplicate heading id.
Defense-in-depth: if malformed source ever slips duplicate ids past the
parser, the error names the offending id instead of deferring to an
opaque downstream Dolt primary-key abort."
  (let* ((root (make-dorg-heading :id 1000 :level 0 :level-offset 0 :title nil))
         (h1 (make-dorg-heading :id 5 :level 1 :level-offset 1 :title "A"))
         (h2 (make-dorg-heading :id 5 :level 1 :level-offset 1 :title "B"))
         (result (make-dorg-parse-result
                  :root-id 1000
                  :headings (list root h1 h2)
                  :new-heading-markers nil
                  :heading-markers (make-hash-table :test 'eql)))
         (msg nil))
    (condition-case err
        (dorg-parse-assign-ids result (lambda () (error "should not allocate")))
      (error (setq msg (error-message-string err))))
    (should msg)
    (should (string-match-p "duplicate DORG_ID values.*5" msg))))

(ert-deftest dorg-test-parse-import-phantom-headings-excluded ()
  "Importing the phantom fixture into Dolt succeeds with no PK abort.
Stored heading rows equal the real headings (root + bead); the phantom
sample headings are absent.  This is the phantom-heading import unblock shape in
miniature.  Skips when dolt is unavailable."
  (dorg-test-with-import "* Real bead
:PROPERTIES:
:DORG_ID: 100
:END:
#+begin_example
*** QuoteFake
:PROPERTIES:
:DORG_ID: 1
:END:
#+end_example
#+begin_example
*** ListFake
:PROPERTIES:
:DORG_ID: 2
:END:
#+end_example
"
    (let ((rows (dorg-sql dolt-dir
                          "SELECT title FROM headings ORDER BY level_offset, title")))
      ;; root (NULL title) + Real bead = 2 rows; phantoms excluded.
      (should (= (length rows) 2))
      (should (cl-some (lambda (r) (equal (alist-get 'title r) "Real bead")) rows))
      (should-not (cl-some (lambda (r) (equal (alist-get 'title r) "QuoteFake")) rows))
      (should-not (cl-some (lambda (r) (equal (alist-get 'title r) "ListFake")) rows)))))

;;; P0 post_blank drift — fixed-width :post-blank over-read (dorg-postblank-drift)

(ert-deftest dorg-test-parse-fixed-width-post-blank-true-count ()
  "A fixed-width block followed by N>0 blank lines stores post-blank = N.
Regression for the P0 post_blank drift: `org-element' over-reports a
fixed-width element's `:post-blank' by 1 (it counts the content-terminating
newline as a blank line), so the parser must derive the TRUE trailing
blank-line count.  Non-vacuous: the block is followed by exactly 5 blank
lines, so a naive `:post-blank' read yields 6."
  (dorg-test-with-parsed
      "* Probe\nintro paragraph:\n: stack one\n:   stack two\n\n\n\n\n\ntail para\n"
    (let* ((h (dorg-test--find-heading headings "Probe"))
           (texts (dorg-test--items-of-type h 'dorg-text-item))
           (fw (cl-find-if (lambda (ti)
                             (string-prefix-p ": stack one"
                                              (dorg-text-item-text ti)))
                           texts)))
      (should fw)
      (should (= (dorg-item-post-blank fw) 5)))))

(ert-deftest dorg-test-parse-fixed-width-post-blank-zero ()
  "A fixed-width block flush against following text stores post-blank = 0.
Pre-fix `org-element' reports `:post-blank' = 1 for a fixed-width element with
no trailing blank line, which would inject a spurious blank on render.  Proves
the geometry correction handles the N=0 boundary."
  (dorg-test-with-parsed
      "* Probe\nintro:\n: stack one\n:   stack two\ntail para\n"
    (let* ((h (dorg-test--find-heading headings "Probe"))
           (texts (dorg-test--items-of-type h 'dorg-text-item))
           (fw (cl-find-if (lambda (ti)
                             (string-prefix-p ": stack one"
                                              (dorg-text-item-text ti)))
                           texts)))
      (should fw)
      (should (= (dorg-item-post-blank fw) 0)))))

(ert-deftest dorg-test-parse-trailing-blank-lines-helper ()
  "`dorg-parse--trailing-blank-lines' counts trailing blank lines from geometry.
The final content newline is not a blank line; whitespace-only trailing lines
are.  This is the source-geometry primitive that corrects org's fixed-width
`:post-blank' +1 over-read."
  (should (= (dorg-parse--trailing-blank-lines ": a\n: b\n") 0))
  (should (= (dorg-parse--trailing-blank-lines ": a\n: b\n\n") 1))
  (should (= (dorg-parse--trailing-blank-lines ": a\n: b\n\n\n\n\n\n") 5))
  (should (= (dorg-parse--trailing-blank-lines "para\n\n\n") 2))
  (should (= (dorg-parse--trailing-blank-lines "no final newline") 0))
  ;; whitespace-only lines count as blank (org's notion of a blank line)
  (should (= (dorg-parse--trailing-blank-lines "para\n   \n\n") 2)))

(provide 'dorg-parse-tests)
;;; dorg-parse-tests.el ends here
