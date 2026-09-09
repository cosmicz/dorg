;;; dorg-model-tests.el --- Tests for dorg-model render and load -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for model render methods, DB→model loading, and DORG_*
;; keyword filtering at the SQL boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-history)
(require 'dorg-import)
(require 'dorg-sql-gen)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el
(declare-function dorg-model-children-of "dorg-model")

(defconst dorg-test-model--repo-root
  (file-name-as-directory
   (or (locate-dominating-file
        (or load-file-name buffer-file-name default-directory)
        "dorg.el")
       default-directory))
  "Repository root captured from this test file's load path.")

(defun dorg-test-model--headings-table (&rest headings)
  "Return an id -> heading table containing HEADINGS."
  (let ((table (make-hash-table :test 'eql)))
    (dolist (heading headings table)
      (puthash (dorg-heading-id heading) heading table))))

(defun dorg-test-model--read-defun-form (file function)
  "Return FUNCTION's top-level `defun' form from FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file dorg-test-model--repo-root))
    (goto-char (point-min))
    (catch 'found
      (while t
        (condition-case nil
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (eq (car form) 'defun)
                         (eq (cadr form) function))
                (throw 'found form)))
          (end-of-file
           (throw 'found nil)))))))

(defun dorg-test-model--form-calls-p (form callee)
  "Return non-nil when FORM contains an actual call form to CALLEE."
  (cond
   ((atom form) nil)
   ((eq (car form) 'quote) nil)
   ((eq (car form) callee) t)
   (t (cl-some (lambda (subform)
                 (dorg-test-model--form-calls-p subform callee))
               form))))

(defun dorg-test-model--defun-calls-p (file function callee)
  "Return non-nil when FUNCTION in FILE calls CALLEE."
  (when-let ((form (dorg-test-model--read-defun-form file function)))
    (dorg-test-model--form-calls-p (cddr form) callee)))

;;; Shared model child ordering helper

(ert-deftest dorg-test-model-children-of-sorts-direct-children ()
  "`dorg-model-children-of' returns only direct children in sort-key order."
  (let* ((parent (make-dorg-heading :id 1 :parent-id nil :sort-key "p"))
         (child-b (make-dorg-heading :id 11 :parent-id 1 :sort-key "b"))
         (child-a (make-dorg-heading :id 12 :parent-id 1 :sort-key "a"))
         (grandchild (make-dorg-heading :id 13 :parent-id 11 :sort-key "aa"))
         (table (dorg-test-model--headings-table parent child-b child-a grandchild)))
    (should (equal (mapcar #'dorg-heading-id
                           (dorg-model-children-of 1 table))
                   '(12 11)))))

(ert-deftest dorg-test-model-children-of-excludes-self-parent-and-nil-key ()
  "`dorg-model-children-of' excludes self-parent rows and sorts nil keys."
  (let* ((self-parent (make-dorg-heading :id 1 :parent-id 1 :sort-key "z"))
         (child-b (make-dorg-heading :id 11 :parent-id 1 :sort-key "b"))
         (child-nil (make-dorg-heading :id 12 :parent-id 1 :sort-key nil))
         (child-a (make-dorg-heading :id 13 :parent-id 1 :sort-key "a"))
         (table (dorg-test-model--headings-table self-parent child-b child-nil child-a)))
    (should (equal (mapcar #'dorg-heading-id
                           (dorg-model-children-of 1 table))
                   '(12 13 11)))))

;;; Render Methods — Item Types

(ert-deftest dorg-test-model-render-text-item ()
  "Rendering a text item returns its text."
  (let ((item (dorg-text-item :text "Hello world.\n")))
    (should (equal (dorg-model--render-item item) "Hello world.\n"))))

(ert-deftest dorg-test-model-render-keyword-item ()
  "Rendering a keyword item returns #+KEY: VALUE."
  (let ((item (dorg-keyword-item :keyword "TITLE" :value "My Doc")))
    (should (equal (dorg-model--render-item item) "#+TITLE: My Doc"))))

(ert-deftest dorg-test-model-render-planning-item ()
  "Rendering a planning item returns SCHEDULED/DEADLINE/CLOSED line."
  (let ((item (dorg-planning-item
               :scheduled "<2026-04-14 Tue>"
               :deadline "<2026-04-15 Wed>")))
    (let ((rendered (dorg-model--render-item item)))
      (should (string-match-p "SCHEDULED: <2026-04-14 Tue>" rendered))
      (should (string-match-p "DEADLINE: <2026-04-15 Wed>" rendered)))))

(ert-deftest dorg-test-model-render-planning-item-partial ()
  "Planning item with only SCHEDULED renders just that."
  (let ((item (dorg-planning-item :scheduled "<2026-04-14 Tue>")))
    (let ((rendered (dorg-model--render-item item)))
      (should (string-match-p "SCHEDULED:" rendered))
      (should-not (string-match-p "DEADLINE:" rendered)))))

(ert-deftest dorg-test-model-render-drawer-item ()
  "Rendering a generic drawer returns :NAME:...lines...:END:."
  (let ((item (dorg-drawer-item :name "LOGBOOK" :lines '("line1" "line2"))))
    (let ((rendered (dorg-model--render-item item)))
      (should (string-match-p "^:LOGBOOK:" rendered))
      (should (string-match-p "line1" rendered))
      (should (string-match-p ":END:" rendered)))))

(ert-deftest dorg-test-model-render-property-drawer ()
  "Rendering a PROPERTIES drawer includes property lines."
  (let ((item (dorg-drawer-item
               :name "PROPERTIES"
               :properties '(("ID" . "abc") ("CUSTOM" . "val")))))
    (let ((rendered (dorg-model--render-item item)))
      (should (string-match-p "^:PROPERTIES:" rendered))
      (should (string-match-p ":ID: abc" rendered))
      (should (string-match-p ":CUSTOM: val" rendered))
      (should (string-match-p ":END:" rendered)))))

(ert-deftest dorg-test-model-render-property-drawer-injects-dorg-id ()
  "PROPERTIES drawer with dorg-id injects :DORG_ID: as first property."
  (let ((item (dorg-drawer-item
               :name "PROPERTIES"
               :properties '(("FOO" . "bar")))))
    (let ((rendered (dorg-model--render-item item 12345)))
      (should (string-match-p ":DORG_ID: 12345" rendered))
      (should (string-match-p ":FOO: bar" rendered))
      ;; DORG_ID should come before FOO
      (should (< (string-match ":DORG_ID:" rendered)
                 (string-match ":FOO:" rendered))))))

;;; Render Methods — Heading

(ert-deftest dorg-test-model-render-heading-simple ()
  "Rendering a simple heading produces correct stars + title."
  (let ((h (make-dorg-heading :id 1 :level 2 :title "Hello")))
    (should (equal (dorg-model--render-heading h) "** Hello"))))

(ert-deftest dorg-test-model-render-heading-todo-priority-tags ()
  "Heading with TODO, priority, and tags renders correctly."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "Task"
            :todo "TODO" :priority "A" :tags '("work" "urgent"))))
    (let ((line (dorg-model--render-heading h)))
      (should (string-match-p "^\\* TODO" line))
      (should (string-match-p "\\[#A\\]" line))
      (should (string-match-p "Task" line))
      (should (string-match-p ":work:urgent:" line)))))

(ert-deftest dorg-test-model-render-heading-commented ()
  "Heading with COMMENT flag renders COMMENT keyword."
  (let ((h (make-dorg-heading :id 1 :level 1 :title "Secret" :commented t)))
    (should (string-match-p "COMMENT" (dorg-model--render-heading h)))))

;;; Render Methods — Full Tree

(ert-deftest dorg-test-model-render-tree-basic ()
  "Rendering a tree with root + child produces org text."
  (let* ((child (make-dorg-heading
                 :id 2 :parent-id 1 :level 1 :title "Child"
                 :items (list (dorg-text-item :text "Body text.\n"))))
         (root (make-dorg-heading :id 1 :level 0))
         (headings (list root child)))
    (let ((text (dorg-model-render-tree headings)))
      (should (string-match-p "^\\* Child" text))
      (should (string-match-p "Body text\\." text))
      ;; Root heading (level 0) should not appear as a heading line
      (should-not (string-match-p "^\\*\\*" text)))))

(ert-deftest dorg-test-model-render-tree-with-dorg-keywords ()
  "DORG_* keyword items on root appear in rendered output."
  (let* ((root (make-dorg-heading
                :id 1 :level 0
                :items (list (dorg-keyword-item :keyword "DORG_DOLT_DIR" :value "/tmp/db")
                             (dorg-keyword-item :keyword "DORG_HASH" :value "abc123"))))
         (child (make-dorg-heading :id 2 :parent-id 1 :level 1 :title "H"))
         (headings (list root child)))
    (let ((text (dorg-model-render-tree headings)))
      (should (string-match-p "^#\\+DORG_DOLT_DIR: /tmp/db" text))
      (should (string-match-p "^#\\+DORG_HASH: abc123" text))
      (should (string-match-p "^\\* H" text)))))

;;; Local Identity PROPERTIES Drawer — :DORG_ID: injection placement
;;
;; A heading's stable identity (:DORG_ID:) must render into the ONE local
;; property drawer org reads back as the heading's own: the PROPERTIES
;; `dorg-drawer-item' occurring before any non-planning body item (planning
;; may precede it).  A :PROPERTIES: block appearing LATER in the body is
;; domain content — it must never receive an injected identity, and its own
;; content (including a literal :DORG_ID:) must survive rendering.

(defun dorg-test-model--count-substring (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (let ((n 0) (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq n (1+ n) start (match-end 0)))
    n))

(ert-deftest dorg-test-model-render-local-identity-synthesized-over-body-drawer ()
  "Body :PROPERTIES: after text does NOT capture the heading identity.
A heading whose only PROPERTIES-shaped drawer sits AFTER body text gets a
freshly synthesized local drawer carrying its :DORG_ID:, while the body
drawer's own content — including a literal :DORG_ID: — is preserved in
place.  Regression: the old renderer injected identity into the body
drawer (mis-locating identity) and dropped that drawer's content."
  (let* ((root (make-dorg-heading :id 1 :level 0))
         (child (make-dorg-heading
                 :id 999 :parent-id 1 :level 1 :title "H"
                 :items (list
                         (dorg-text-item :text "body text here\n")
                         (dorg-drawer-item
                          :name "PROPERTIES"
                          :lines '(":DORG_ID: 123" ":FOO: bar")))))
         (out (dorg-model-render-tree (list root child))))
    ;; Synthesized local identity precedes the body text …
    (let ((id999 (string-match ":DORG_ID: 999" out))
          (body  (string-match "body text here" out)))
      (should id999)
      (should body)
      (should (< id999 body)))
    ;; … the body drawer keeps its literal :DORG_ID: 123 and :FOO: bar …
    (should (string-match-p ":DORG_ID: 123" out))
    (should (string-match-p ":FOO: bar" out))
    ;; … and that retained 123 sits AFTER the body text (it is the body
    ;; drawer, not the identity).
    (should (< (string-match "body text here" out)
               (string-match ":DORG_ID: 123" out)))
    ;; Exactly one heading identity (999) is injected.
    (should (= 1 (dorg-test-model--count-substring ":DORG_ID: 999" out)))
    ;; Re-parsing recovers 999 as the heading identity and keeps the body id.
    (with-temp-buffer
      (insert out)
      (org-mode)
      (let* ((headings (dorg-parse-result-headings
                        (dorg-parse-buffer (current-buffer) 1)))
             (h (dorg-test--find-heading headings "H")))
        (should h)
        (should (= (dorg-heading-id h) 999))
        ;; The body drawer survived as generic-drawer lines carrying 123.
        (let ((body-drawer (car (dorg-test--items-of-type h 'dorg-drawer-item))))
          (should body-drawer)
          (should (member ":DORG_ID: 123" (dorg-drawer-item-lines body-drawer))))))))

(ert-deftest dorg-test-model-render-local-identity-planning-then-local-drawer ()
  "Planning may precede the local identity drawer; identity injects there.
A leading planning line followed by a PROPERTIES drawer keeps that drawer
as the heading's local identity drawer — :DORG_ID: is injected into it (no
second synthesized drawer), after the planning line."
  (let* ((root (make-dorg-heading :id 1 :level 0))
         (child (make-dorg-heading
                 :id 999 :parent-id 1 :level 1 :title "T"
                 :items (list
                         (dorg-planning-item :scheduled "<2026-04-09 Thu>")
                         (dorg-drawer-item :name "PROPERTIES"
                                           :properties '(("FOO" . "bar"))))))
         (out (dorg-model-render-tree (list root child))))
    ;; Exactly one PROPERTIES drawer and one identity — no synthesized dup.
    (should (= 1 (dorg-test-model--count-substring ":PROPERTIES:" out)))
    (should (= 1 (dorg-test-model--count-substring ":DORG_ID: 999" out)))
    ;; Order: SCHEDULED < :PROPERTIES: < :DORG_ID: 999 < :FOO: bar.
    (let ((sched (string-match "SCHEDULED:" out))
          (props (string-match ":PROPERTIES:" out))
          (id999 (string-match ":DORG_ID: 999" out))
          (foo   (string-match ":FOO: bar" out)))
      (should (and sched props id999 foo))
      (should (< sched props))
      (should (< props id999))
      (should (< id999 foo)))
    ;; Re-parse: identity 999 recovered; FOO preserved as a property.
    (with-temp-buffer
      (insert out)
      (org-mode)
      (let* ((headings (dorg-parse-result-headings
                        (dorg-parse-buffer (current-buffer) 1)))
             (h (dorg-test--find-heading headings "T")))
        (should (= (dorg-heading-id h) 999))
        (let ((drawer (car (dorg-test--items-of-type h 'dorg-drawer-item))))
          (should drawer)
          (should (equal '(("FOO" . "bar"))
                         (dorg-drawer-item-properties drawer))))))))

(ert-deftest dorg-test-model-render-local-identity-multiple-body-drawers-no-injection ()
  "Multiple later body :PROPERTIES: drawers receive no injected identity.
Only ONE synthesized local identity drawer is emitted (before the first
body item); every later body PROPERTIES-shaped drawer keeps its own
content and is free of the heading :DORG_ID:."
  (let* ((root (make-dorg-heading :id 1 :level 0))
         (child (make-dorg-heading
                 :id 999 :parent-id 1 :level 1 :title "H"
                 :items (list
                         (dorg-text-item :text "intro\n")
                         (dorg-drawer-item :name "PROPERTIES" :lines '(":A: 1"))
                         (dorg-text-item :text "mid\n")
                         (dorg-drawer-item :name "PROPERTIES" :lines '(":B: 2")))))
         (out (dorg-model-render-tree (list root child))))
    ;; Exactly one heading identity, and it precedes all body content.
    (should (= 1 (dorg-test-model--count-substring ":DORG_ID: 999" out)))
    (should (< (string-match ":DORG_ID: 999" out) (string-match "intro" out)))
    ;; Both body drawers keep their own content.
    (should (string-match-p ":A: 1" out))
    (should (string-match-p ":B: 2" out))
    ;; Three PROPERTIES drawers total: one synthesized local + two body.
    (should (= 3 (dorg-test-model--count-substring ":PROPERTIES:" out)))))

(ert-deftest dorg-test-model-render-local-identity-parse-render-parse-stable ()
  "Identity and body content are stable across parse → render → parse.
A heading whose PROPERTIES drawer sits after body text, once assigned an
id, renders a local identity drawer that re-parses back to the same id; a
second render is byte-identical and the body drawer's literal :DORG_ID: is
preserved throughout."
  (let ((input "* H\nbody\n:PROPERTIES:\n:DORG_ID: 123\n:FOO: bar\n:END:\n"))
    (with-temp-buffer
      (insert input)
      (org-mode)
      (let* ((headings (dorg-parse-result-headings
                        (dorg-parse-buffer (current-buffer) 1)))
             (h (dorg-test--find-heading headings "H")))
        ;; No LOCAL drawer in the source → no identity assigned by parse.
        (should (null (dorg-heading-id h)))
        (setf (dorg-heading-id h) 999)
        (let ((render1 (dorg-model-render-tree headings)))
          (should (string-match-p ":DORG_ID: 999" render1))
          (should (string-match-p ":DORG_ID: 123" render1))
          (should (string-match-p ":FOO: bar" render1))
          ;; Re-parse render1 → identity 999 recovered from the local drawer.
          (with-temp-buffer
            (insert render1)
            (org-mode)
            (let* ((h2s (dorg-parse-result-headings
                         (dorg-parse-buffer (current-buffer) 1)))
                   (h2 (dorg-test--find-heading h2s "H")))
              (should (= (dorg-heading-id h2) 999))
              ;; Second render is byte-identical → stable fixpoint.
              (let ((render2 (dorg-model-render-tree h2s)))
                (should (equal render1 render2))
                (should (string-match-p ":DORG_ID: 123" render2))))))))))

(ert-deftest dorg-test-model-render-generic-lines-properties-first-not-identity ()
  "A raw-lines drawer named PROPERTIES at first position is NOT identity.
Even at the first non-planning position, a drawer that carries generic raw
`lines' (rather than key/value `properties') is a body :PROPERTIES: block,
not a heading's local identity drawer.  It must NOT receive the injected
:DORG_ID: — that would render the property form and DISCARD its lines.
Instead a fresh synthetic identity drawer is emitted before it, and the
generic drawer's own lines (including a literal :DORG_ID:) survive.

Regression: the name+position resolver injected identity into this
raw-lines drawer, dropping its content."
  (let* ((root (make-dorg-heading :id 1 :level 0))
         (child (make-dorg-heading
                 :id 999 :parent-id 1 :level 1 :title "H"
                 :items (list
                         (dorg-drawer-item
                          :name "PROPERTIES"
                          :lines '(":FOO: bar" ":DORG_ID: 123")))))
         (out (dorg-model-render-tree (list root child))))
    ;; The generic drawer's raw lines reach the output verbatim …
    (should (string-match-p ":FOO: bar" out))
    (should (string-match-p ":DORG_ID: 123" out))
    ;; … the heading identity (999) is injected exactly once, into a
    ;; SEPARATE synthesized drawer that precedes the generic drawer's lines.
    (should (= 1 (dorg-test-model--count-substring ":DORG_ID: 999" out)))
    (let ((id999 (string-match ":DORG_ID: 999" out))
          (foo   (string-match ":FOO: bar" out))
          (id123 (string-match ":DORG_ID: 123" out)))
      (should (and id999 foo id123))
      (should (< id999 foo))
      (should (< id999 id123)))))

;;; SQL Gen — DORG_* Keyword Filtering

(ert-deftest dorg-test-sql-gen-skips-dorg-keywords ()
  "SQL generation skips DORG_* keyword items."
  (let* ((h (make-dorg-heading
             :id 100 :level 0
             :items (list (dorg-keyword-item :keyword "DORG_DOLT_DIR" :value "/tmp")
                          (dorg-keyword-item :keyword "TITLE" :value "My Doc")
                          (dorg-keyword-item :keyword "DORG_HASH" :value "abc"))))
         (counter 0)
         (id-gen (lambda () (cl-incf counter))))
    (let ((stmts (dorg-sql-gen-heading-items-sql h id-gen)))
      ;; Only TITLE should produce SQL — 1 heading_items + 1 keyword_items = 2
      (should (= (length stmts) 2))
      (should (cl-some (lambda (s) (string-match-p "TITLE" s)) stmts))
      (should-not (cl-some (lambda (s) (string-match-p "DORG_DOLT_DIR" s)) stmts))
      (should-not (cl-some (lambda (s) (string-match-p "DORG_HASH" s)) stmts)))))

;;; DB→Model Loader

(ert-deftest dorg-test-model-load-tree-roundtrip ()
  "Import Org text, load its DB tree, and preserve the parsed model."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (insert "* Hello\nBody text.\n* World\nMore text.\n")
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let* ((receipt (dorg-import-document
                           (current-buffer)
                           :dolt-dir dolt-dir
                           :target-file org-file
                           :mode 'bootstrap))
                 (root-id (plist-get receipt :root-id))
                 (tree (dorg-model-load-tree dolt-dir root-id)))
            ;; Should have root + 2 headings = 3 entries
            (should (= (hash-table-count tree) 3))
            ;; Check a heading has items populated
            (let ((has-items nil))
              (maphash (lambda (_k h)
                         (when (dorg-heading-items h)
                           (setq has-items t)))
                       tree)
              (should has-items))))))))

(ert-deftest dorg-test-model-load-tree-as-of ()
  "Loading tree with AS OF returns historical state."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      ;; Seed "Original" as v1
      (let ((stmts (dorg-test--parse-to-sql "* Original\n")))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "v1"))
      (let ((commit1 (dorg-head-commit dolt-dir))
            (root-id (alist-get 'id (car (dorg-sql dolt-dir
                       "SELECT id FROM headings WHERE level = 0 AND parent_id IS NULL")))))
        ;; Update title for v2
        (dorg-sql dolt-dir
          (format "UPDATE headings SET title = 'Changed' WHERE root_id = %d AND level = 1"
                  root-id))
        (dorg-commit dolt-dir "v2")
        ;; Load at commit1 — should see "Original"
        (let ((old-tree (dorg-model-load-tree dolt-dir root-id commit1))
              (found-title nil))
          (maphash (lambda (_k h)
                     (when (equal (dorg-heading-title h) "Original")
                       (setq found-title t)))
                   old-tree)
          (should found-title))))))

;;; Item Identity Slots
;;
;; Red specs for the `dorg-heading-item' base-class `id'/`sort-key'
;; slots that let item-level diffing reuse DB heading_items.id.  Parser
;; items leave them nil; DB-loaded items carry them; deep-copy and
;; snapshot capture preserve them.

(ert-deftest dorg-test-model-item-identity-slots-accessors ()
  "Items accept :id/:sort-key initargs exposed via accessors (AC1)."
  (let* ((sk (dorg-sortkey-from-integer 3))
         (item (dorg-text-item :id 4242 :sort-key sk :text "x")))
    (should (= (dorg-item-id item) 4242))
    (should (equal (dorg-item-sort-key item) sk))))

(ert-deftest dorg-test-model-parser-items-leave-identity-nil ()
  "Parser-created items leave id/sort-key nil (AC1)."
  (with-temp-buffer
    (insert "* H\nBody text.\n")
    (org-mode)
    (let* ((result (dorg-parse-buffer))
           (headings (dorg-parse-result-headings result))
           (h (cl-find-if (lambda (x) (equal (dorg-heading-title x) "H")) headings))
           (item (car (dorg-heading-items h))))
      (should item)
      (should (null (dorg-item-id item)))
      (should (null (dorg-item-sort-key item))))))

(ert-deftest dorg-test-model-item-deep-copy-preserves-identity ()
  "`dorg-item-deep-copy' carries id and sort-key (AC1)."
  (let* ((sk (dorg-sortkey-from-integer 7))
         (orig (dorg-text-item :id 99 :sort-key sk :text "x"))
         (copy (dorg-item-deep-copy orig)))
    (should (= (dorg-item-id copy) 99))
    (should (equal (dorg-item-sort-key copy) sk))
    ;; Deep, not shared: mutating the copy's key does not reach the orig.
    (should-not (eq (dorg-item-sort-key copy) (dorg-item-sort-key orig)))))

(ert-deftest dorg-test-model-load-items-carry-identity ()
  "DB-loaded items carry heading_items.id and sort_key (AC1)."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (insert "* H\nBody text.\n")
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let* ((receipt (dorg-import-document
                           (current-buffer)
                           :dolt-dir dolt-dir
                           :target-file org-file
                           :mode 'bootstrap))
                 (root-id (plist-get receipt :root-id))
                 (tree (dorg-model-load-tree dolt-dir root-id))
                 (item nil))
            (maphash (lambda (_k h)
                       (dolist (it (dorg-heading-items h))
                         (when (cl-typep it 'dorg-text-item)
                           (setq item it))))
                     tree)
            (should item)
            ;; id matches the persisted heading_items row; sort-key is a key.
            (let ((db-id (alist-get 'id (car (dorg-sql dolt-dir
                            "SELECT id FROM heading_items WHERE kind = 'text' LIMIT 1")))))
              (should (= (dorg-item-id item) db-id)))
            (should (dorg-item-sort-key item))))))))

;;; Item UPDATE SQL Generation
;;
;; Red specs for same-kind detail-table UPDATE helpers used by item-level
;; lowering.  They reuse the existing item id rather than delete+reinsert.

(ert-deftest dorg-test-sql-gen-item-update-text ()
  "Text item update writes text_items.text_raw for the reused id (AC10)."
  (let* ((old (dorg-text-item :id 10 :sort-key (dorg-sortkey-from-integer 1) :text "old"))
         (new (dorg-text-item :text "new body"))
         (sql (mapconcat #'identity (dorg-sql-gen-item-update old new) "\n")))
    (should (string-match-p "UPDATE text_items SET text_raw = " sql))
    (should (string-match-p "new body" sql))
    (should (string-match-p "WHERE item_id = 10" sql))))

(ert-deftest dorg-test-sql-gen-item-update-keyword ()
  "Same-name keyword update writes keyword_items.value_raw (AC10)."
  (let* ((old (dorg-keyword-item :id 11 :sort-key (dorg-sortkey-from-integer 1)
                                 :keyword "TITLE" :value "Old"))
         (new (dorg-keyword-item :keyword "TITLE" :value "New"))
         (sql (mapconcat #'identity (dorg-sql-gen-item-update old new) "\n")))
    (should (string-match-p "UPDATE keyword_items SET value_raw = " sql))
    (should (string-match-p "New" sql))
    (should (string-match-p "WHERE item_id = 11" sql))))

(ert-deftest dorg-test-sql-gen-item-update-planning ()
  "Planning update writes scheduled/deadline/closed raw fields (AC10)."
  (let* ((old (dorg-planning-item :id 12 :sort-key (dorg-sortkey-from-integer 1)
                                  :scheduled "<2026-01-01 Thu>"))
         (new (dorg-planning-item :scheduled "<2026-02-02 Mon>"
                                  :deadline "<2026-02-03 Tue>"))
         (sql (mapconcat #'identity (dorg-sql-gen-item-update old new) "\n")))
    (should (string-match-p "UPDATE planning_items SET" sql))
    (should (string-match-p "scheduled_raw = " sql))
    (should (string-match-p "deadline_raw = " sql))
    (should (string-match-p "closed_raw = " sql))
    (should (string-match-p "WHERE item_id = 12" sql))))

(ert-deftest dorg-test-sql-gen-item-update-drawer-replaces-lines-only ()
  "Same-name drawer update replaces only that drawer's lines (AC10).
Churn is scoped to drawer_lines for the reused drawer item id, not the
whole heading."
  (let* ((old (dorg-drawer-item :id 13 :sort-key (dorg-sortkey-from-integer 1)
                                :name "LOGBOOK" :lines '("a")))
         (new (dorg-drawer-item :name "LOGBOOK" :lines '("a" "b")))
         (sql (mapconcat #'identity (dorg-sql-gen-item-update old new) "\n")))
    (should (string-match-p "DELETE FROM drawer_lines WHERE drawer_item_id = 13" sql))
    (should (string-match-p "INSERT INTO drawer_lines" sql))
    ;; Must NOT wipe the whole heading's items.
    (should-not (string-match-p "DELETE FROM heading_items WHERE heading_id" sql))))

(ert-deftest dorg-test-sql-gen-item-update-drawer-replaces-property-entries ()
  "Same-name PROPERTIES drawer update replaces only its property_entries (AC10).
A PROPERTIES drawer edit reuses the drawer item id and rewrites only that
drawer's property_entries rows, never drawer_lines or the whole heading."
  (let* ((old (dorg-drawer-item :id 14 :sort-key (dorg-sortkey-from-integer 1)
                                :name "PROPERTIES"
                                :properties '(("A" . "1"))))
         (new (dorg-drawer-item :name "PROPERTIES"
                                :properties '(("A" . "1") ("B" . "2"))))
         (sql (mapconcat #'identity (dorg-sql-gen-item-update old new) "\n")))
    (should (string-match-p "DELETE FROM property_entries WHERE drawer_item_id = 14" sql))
    (should (string-match-p "INSERT INTO property_entries" sql))
    (should (string-match-p "'B'" sql))
    ;; Property edits do not touch drawer_lines nor wipe the heading.
    (should-not (string-match-p "INSERT INTO drawer_lines" sql))
    (should-not (string-match-p "DELETE FROM heading_items WHERE heading_id" sql))))

;;; Timestamp Accessors — Planning

(ert-deftest dorg-test-model-scheduled-timestamp ()
  "SCHEDULED planning raw parses to a `dorg-timestamp'."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-planning-item
                           :scheduled "<2026-04-14 Tue>"
                           :deadline "<2026-04-15 Wed>"
                           :closed "[2026-04-13 Mon]"))))
         (sch (dorg-heading-scheduled-timestamp h))
         (dl (dorg-heading-deadline-timestamp h))
         (cl (dorg-heading-closed-timestamp h)))
    (should (dorg-timestamp-p sch))
    (should (equal (dorg-timestamp-start-date sch) '(2026 4 14)))
    (should (dorg-timestamp-active sch))
    (should (dorg-timestamp-p dl))
    (should (equal (dorg-timestamp-start-date dl) '(2026 4 15)))
    (should (dorg-timestamp-p cl))
    (should (equal (dorg-timestamp-start-date cl) '(2026 4 13)))
    (should-not (dorg-timestamp-active cl))))

(ert-deftest dorg-test-model-planning-timestamp-missing ()
  "Missing planning fields return nil without signaling."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "T"
            :items (list (dorg-planning-item :scheduled "<2026-04-14 Tue>")))))
    (should (dorg-heading-scheduled-timestamp h))
    (should-not (dorg-heading-deadline-timestamp h))
    (should-not (dorg-heading-closed-timestamp h))))

(ert-deftest dorg-test-model-planning-timestamp-unparsable ()
  "Unparsable planning raw returns nil, not a partial result."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "T"
            :items (list (dorg-planning-item :scheduled "not a timestamp")))))
    (should-not (dorg-heading-scheduled-timestamp h))))

(ert-deftest dorg-test-model-planning-no-planning-item ()
  "Heading without a planning item returns nil for all planning helpers."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "T"
            :items (list (dorg-text-item :text "Body.\n")))))
    (should-not (dorg-heading-scheduled-timestamp h))
    (should-not (dorg-heading-deadline-timestamp h))
    (should-not (dorg-heading-closed-timestamp h))))

;;; Timestamp Accessors — Body Active Timestamps

(ert-deftest dorg-test-model-active-timestamps-order ()
  "Active body timestamps are returned in document order."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-text-item :text "First <2026-01-01 Thu>.\n")
                          (dorg-text-item :text "Then <2026-02-02 Mon> and <2026-03-03 Tue>.\n"))))
         (tss (dorg-heading-active-timestamps h)))
    (should (= (length tss) 3))
    (should (equal (mapcar #'dorg-timestamp-start-date tss)
                   '((2026 1 1) (2026 2 2) (2026 3 3))))
    (should (cl-every #'dorg-timestamp-active tss))))

(ert-deftest dorg-test-model-active-timestamps-range ()
  "An active date range yields a single timestamp with end-date."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-text-item
                           :text "Span <2026-01-01 Thu>--<2026-01-05 Mon> here.\n"))))
         (tss (dorg-heading-active-timestamps h)))
    (should (= (length tss) 1))
    (should (equal (dorg-timestamp-start-date (car tss)) '(2026 1 1)))
    (should (equal (dorg-timestamp-end-date (car tss)) '(2026 1 5)))))

(ert-deftest dorg-test-model-active-timestamps-excludes-inactive ()
  "Inactive timestamps and malformed text are excluded from body scan."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-text-item
                           :text "Done [2026-01-01 Thu], live <2026-02-02 Mon>, junk <nope>.\n"))))
         (tss (dorg-heading-active-timestamps h)))
    (should (= (length tss) 1))
    (should (equal (dorg-timestamp-start-date (car tss)) '(2026 2 2)))))

(ert-deftest dorg-test-model-active-timestamps-rejects-mixed-range ()
  "A mixed active/inactive range does not leak its active endpoint."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-text-item
                           :text "Mixed <2026-01-01 Thu>--[2026-01-05 Mon] end.\n"))))
         (tss (dorg-heading-active-timestamps h)))
    (should (null tss))))

(ert-deftest dorg-test-model-active-timestamps-ignores-non-text ()
  "Body scan ignores planning lines, drawers, and keywords."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-planning-item :scheduled "<2026-01-01 Thu>")
                          (dorg-keyword-item :keyword "CAPTURED" :value "<2026-02-02 Mon>")
                          (dorg-drawer-item :name "LOGBOOK" :lines '("<2026-03-03 Tue>"))
                          (dorg-drawer-item
                           :name "PROPERTIES"
                           :properties '(("WHEN" . "<2026-04-04 Sat>")))))))
    (should (null (dorg-heading-active-timestamps h)))))

(ert-deftest dorg-test-model-active-timestamps-empty ()
  "Heading with no active body timestamps returns nil."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "T"
            :items (list (dorg-text-item :text "Plain body, no dates.\n")))))
    (should (null (dorg-heading-active-timestamps h)))))

;;; Timestamp Accessors — Property Timestamps

(ert-deftest dorg-test-model-property-timestamps-shape-and-order ()
  "Property timestamps return ordered (:property NAME :timestamp TS) plists."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "PROPERTIES"
                           :properties '(("CREATED" . "[2026-01-01 Thu]")
                                         ("EVENT" . "<2026-02-02 Mon>")
                                         ("NOTE" . "free text"))))))
         (props (dorg-heading-property-timestamps h)))
    (should (= (length props) 2))
    (should (equal (plist-get (nth 0 props) :property) "CREATED"))
    (should (dorg-timestamp-p (plist-get (nth 0 props) :timestamp)))
    (should-not (dorg-timestamp-active (plist-get (nth 0 props) :timestamp)))
    (should (equal (plist-get (nth 1 props) :property) "EVENT"))
    (should (dorg-timestamp-active (plist-get (nth 1 props) :timestamp)))))

(ert-deftest dorg-test-model-property-timestamps-skips-unparsable ()
  "Unparsable property values are skipped without signaling."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "PROPERTIES"
                           :properties '(("X" . "garbage") ("Y" . "also bad"))))))
         (props (dorg-heading-property-timestamps h)))
    (should (null props))))

(ert-deftest dorg-test-model-property-timestamps-only-properties-drawer ()
  "Only PROPERTIES drawers are scanned, not generic drawers."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "LOGBOOK"
                           :lines '(":WHEN: <2026-01-01 Thu>"))))))
    (should (null (dorg-heading-property-timestamps h)))))

;;; Timestamp Accessors — Non-Mutation

(ert-deftest dorg-test-model-timestamp-accessors-non-mutating ()
  "Timestamp accessors do not mutate heading slots, items, or render output."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-planning-item :scheduled "<2026-04-14 Tue>")
                          (dorg-text-item :text "Body <2026-05-05 Tue>.\n")
                          (dorg-drawer-item
                           :name "PROPERTIES"
                           :properties '(("EVENT" . "<2026-06-06 Sat>"))))))
         (before-hash (dorg-heading-compute-items-hash h))
         (before-render (dorg-model-render-heading-section h)))
    (dorg-heading-scheduled-timestamp h)
    (dorg-heading-active-timestamps h)
    (dorg-heading-property-timestamps h)
    (should (equal (dorg-heading-compute-items-hash h) before-hash))
    (should (equal (dorg-model-render-heading-section h) before-render))
    (should (equal (dorg-planning-item-scheduled (nth 0 (dorg-heading-items h)))
                   "<2026-04-14 Tue>"))
    (should (equal (dorg-text-item-text (nth 1 (dorg-heading-items h)))
                   "Body <2026-05-05 Tue>.\n"))))

;;; Timestamp Accessors — LOGBOOK Activity

(ert-deftest dorg-test-model-logbook-state-change ()
  "State-change LOGBOOK lines capture kind, from/to state, raw, and timestamp."
  (let* ((line "- State \"DONE\"       from \"TODO\"       [2026-01-02 Fri 10:00]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((e (car entries)))
      (should (dorg-logbook-entry-p e))
      (should (eq (dorg-logbook-entry-kind e) 'state-change))
      (should (equal (dorg-logbook-entry-to-state e) "DONE"))
      (should (equal (dorg-logbook-entry-from-state e) "TODO"))
      (should (equal (dorg-logbook-entry-raw e) line))
      (should (equal (dorg-timestamp-start-date (dorg-logbook-entry-timestamp e))
                     '(2026 1 2)))
      (should-not (dorg-timestamp-active (dorg-logbook-entry-timestamp e))))))

(ert-deftest dorg-test-model-logbook-state-change-no-from ()
  "State-change line without a from clause yields nil from-state."
  (let* ((line "- State \"TODO\"       [2026-01-01 Thu 09:00]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (should (eq (dorg-logbook-entry-kind (car entries)) 'state-change))
    (should (equal (dorg-logbook-entry-to-state (car entries)) "TODO"))
    (should (null (dorg-logbook-entry-from-state (car entries))))))

(ert-deftest dorg-test-model-logbook-note ()
  "Note LOGBOOK lines classify as note with an inactive timestamp."
  (let* ((line "- Note taken on [2026-02-02 Mon 14:30] \\\\")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (should (eq (dorg-logbook-entry-kind (car entries)) 'note))
    (should (null (dorg-logbook-entry-to-state (car entries))))
    (should (null (dorg-logbook-entry-from-state (car entries))))
    (should (equal (dorg-timestamp-start-date (dorg-logbook-entry-timestamp (car entries)))
                   '(2026 2 2)))))

(ert-deftest dorg-test-model-logbook-fallback-preserves-raw ()
  "Generic LOGBOOK lines fall back to `logbook' kind and keep the raw line."
  (let* ((line "Clocked some work [2026-03-03 Tue 11:11]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (should (eq (dorg-logbook-entry-kind (car entries)) 'logbook))
    (should (equal (dorg-logbook-entry-raw (car entries)) line))
    (should (null (dorg-logbook-entry-to-state (car entries))))))

(ert-deftest dorg-test-model-logbook-inactive-range ()
  "An inactive range yields a single entry with end-date populated."
  (let* ((line "- Clock [2026-01-01 Thu]--[2026-01-05 Mon]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((ts (dorg-logbook-entry-timestamp (car entries))))
      (should (equal (dorg-timestamp-start-date ts) '(2026 1 1)))
      (should (equal (dorg-timestamp-end-date ts) '(2026 1 5)))
      (should-not (dorg-timestamp-active ts)))))

(ert-deftest dorg-test-model-logbook-skips-active-and-mixed ()
  "Active timestamps and mixed active/inactive ranges leak no entries."
  (dolist (line '("- Live <2026-01-01 Thu> only"
                  "- Mixed [2026-01-01 Thu]--<2026-01-05 Mon>"
                  "- Mixed <2026-01-01 Thu>--[2026-01-05 Mon]"
                  "- Junk [nope] and <bad>"
                  "- No timestamp at all"))
    (let* ((h (make-dorg-heading
               :id 1 :level 1 :title "T"
               :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line))))))
      (should (null (dorg-heading-logbook-entries h))))))

(ert-deftest dorg-test-model-logbook-multi-token-order ()
  "Multiple inactive tokens on one line yield ordered entries sharing class."
  (let* ((line "- State \"DONE\" from \"TODO\" [2026-01-01 Thu] redone [2026-01-02 Fri]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 2))
    (should (equal (mapcar (lambda (e)
                             (dorg-timestamp-start-date (dorg-logbook-entry-timestamp e)))
                           entries)
                   '((2026 1 1) (2026 1 2))))
    ;; Both entries repeat the line's classification metadata.
    (should (cl-every (lambda (e) (eq (dorg-logbook-entry-kind e) 'state-change)) entries))
    (should (cl-every (lambda (e) (equal (dorg-logbook-entry-to-state e) "DONE")) entries))))

(ert-deftest dorg-test-model-logbook-document-order ()
  "Entries preserve drawer, line, and token order across multiple drawers."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "LOGBOOK"
                           :lines '("- Note taken on [2026-01-01 Thu]"
                                    "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"))
                          (dorg-text-item :text "interlude\n")
                          (dorg-drawer-item
                           :name "LOGBOOK"
                           :lines '("- Note taken on [2026-01-03 Sat]")))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar (lambda (e)
                             (dorg-timestamp-start-date (dorg-logbook-entry-timestamp e)))
                           entries)
                   '((2026 1 1) (2026 1 2) (2026 1 3))))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(note state-change note)))))

(ert-deftest dorg-test-model-logbook-ignores-non-logbook ()
  "Only LOGBOOK drawers are scanned; other content is ignored."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "[2026-09-09 Wed] in title"
             :items (list (dorg-planning-item :closed "[2026-01-01 Thu]")
                          (dorg-text-item :text "Body [2026-02-02 Mon].\n")
                          (dorg-keyword-item :keyword "CAPTURED" :value "[2026-03-03 Tue]")
                          (dorg-drawer-item
                           :name "PROPERTIES"
                           :properties '(("CREATED" . "[2026-04-04 Sat]")))
                          (dorg-drawer-item
                           :name "VERSES"
                           :lines '("- Note taken on [2026-05-05 Tue]"))))))
    (should (null (dorg-heading-logbook-entries h)))))

(ert-deftest dorg-test-model-logbook-empty ()
  "Heading with no LOGBOOK activity returns nil."
  (let ((h (make-dorg-heading
            :id 1 :level 1 :title "T"
            :items (list (dorg-drawer-item :name "LOGBOOK" :lines nil)))))
    (should (null (dorg-heading-logbook-entries h)))))

(ert-deftest dorg-test-model-logbook-non-mutating ()
  "Computing LOGBOOK entries leaves drawer lines, hash, and render unchanged."
  (let* ((lines '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                  "- Note taken on [2026-01-03 Sat]"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (before-hash (dorg-heading-compute-items-hash h))
         (before-render (dorg-model-render-heading-section h)))
    (dorg-heading-logbook-entries h)
    (should (equal (dorg-heading-compute-items-hash h) before-hash))
    (should (equal (dorg-model-render-heading-section h) before-render))
    (should (equal (dorg-drawer-item-lines (car (dorg-heading-items h))) lines))))

(ert-deftest dorg-test-model-logbook-clock ()
  "A CLOCK range line projects one `clock' entry with duration and range.
AC1: kind clock, duration captured from `=>  H:MM', inactive range
timestamp with start/end dates, from/to-state nil, raw preserved."
  (let* ((line "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((e (car entries)))
      (should (eq (dorg-logbook-entry-kind e) 'clock))
      (should (equal (dorg-logbook-entry-duration e) "1:30"))
      (should (equal (dorg-logbook-entry-raw e) line))
      (should (null (dorg-logbook-entry-from-state e)))
      (should (null (dorg-logbook-entry-to-state e)))
      (let ((ts (dorg-logbook-entry-timestamp e)))
        (should (equal (dorg-timestamp-start-date ts) '(2026 1 1)))
        (should (equal (dorg-timestamp-end-date ts) '(2026 1 1)))
        (should-not (dorg-timestamp-active ts))))))

(ert-deftest dorg-test-model-logbook-clock-indented ()
  "A leading-whitespace CLOCK line is still classified as `clock' (anchor `[ \\t]*')."
  (let* ((line "  CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (should (eq (dorg-logbook-entry-kind (car entries)) 'clock))
    (should (equal (dorg-logbook-entry-duration (car entries)) "1:30"))))

(ert-deftest dorg-test-model-logbook-clock-no-duration ()
  "A running CLOCK line (no `=>') projects `clock' with nil duration.
AC2."
  (let* ((line "CLOCK: [2026-01-01 Thu 10:00]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((e (car entries)))
      (should (eq (dorg-logbook-entry-kind e) 'clock))
      (should (null (dorg-logbook-entry-duration e)))
      (should (equal (dorg-timestamp-start-date (dorg-logbook-entry-timestamp e))
                     '(2026 1 1))))))

(ert-deftest dorg-test-model-logbook-clock-anchor-precise ()
  "The clock anchor requires a literal `CLOCK:'; prose is not promoted.
AC3 non-vacuity guard: `Clocked some work [ts]' stays generic `logbook',
and State/Note lines keep their kinds with nil duration."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "LOGBOOK"
                           :lines '("Clocked some work [2026-03-03 Tue 11:11]"
                                    "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                                    "- Note taken on [2026-01-03 Sat]")))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(logbook state-change note)))
    (should (cl-every (lambda (e) (null (dorg-logbook-entry-duration e))) entries))))

(ert-deftest dorg-test-model-logbook-duration-clock-only ()
  "Only `clock' entries carry a duration; every other kind is nil.
AC4."
  (let* ((h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item
                           :name "LOGBOOK"
                           :lines '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                                    "- Note taken on [2026-01-03 Sat]"
                                    "- Clock [2026-01-04 Sun]"
                                    "CLOCK: [2026-01-05 Mon 09:00]--[2026-01-05 Mon 10:00] =>  1:00")))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(state-change note logbook clock)))
    ;; Non-clock kinds: duration nil; clock kind: duration populated.
    (cl-loop for e in entries
             do (if (eq (dorg-logbook-entry-kind e) 'clock)
                    (should (equal (dorg-logbook-entry-duration e) "1:00"))
                  (should (null (dorg-logbook-entry-duration e)))))))

(ert-deftest dorg-test-model-logbook-mixed-order-and-purity ()
  "Mixed note/state/clock lines project in document order; projection is pure.
AC5: kinds (note state-change clock) in order; computing entries does
not mutate drawer lines, item hash, or rendered output."
  (let* ((lines '("- Note taken on [2026-01-01 Thu]"
                  "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                  "CLOCK: [2026-01-03 Sat 08:00]--[2026-01-03 Sat 09:15] =>  1:15"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (before-hash (dorg-heading-compute-items-hash h))
         (before-render (dorg-model-render-heading-section h))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(note state-change clock)))
    (should (equal (dorg-heading-compute-items-hash h) before-hash))
    (should (equal (dorg-model-render-heading-section h) before-render))
    (should (equal (dorg-drawer-item-lines (car (dorg-heading-items h))) lines))))

(ert-deftest dorg-test-model-logbook-render-roundtrip ()
  "Render-side fidelity lock: a LOGBOOK drawer renders verbatim (AC6, render-side).
The render path is untouched by the projection, so clock/note/state
lines (incl. the `=>  H:MM' duration) emit byte-for-byte unchanged."
  (let* ((lines '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                  "- Note taken on [2026-01-03 Sat]"
                  "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30"))
         (item (dorg-drawer-item :name "LOGBOOK" :lines lines)))
    (should (equal (dorg-model--render-item item)
                   (concat ":LOGBOOK:\n"
                           "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]\n"
                           "- Note taken on [2026-01-03 Sat]\n"
                           "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30\n"
                           ":END:")))))

(ert-deftest dorg-test-model-logbook-parse-render-roundtrip ()
  "Blank-line-free LOGBOOK content survives parse->render byte-exact (AC6, R1).
A strictly stronger lock than the render-only leg: it exercises BOTH the
parser (raw-line extraction, dorg-parse.el) and the renderer.  The
blank-line case is covered separately by
`dorg-test-model-drawer-blank-line-parse-render-roundtrip'."
  (let* ((drawer (concat ":LOGBOOK:\n"
                         "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]\n"
                         "- Note taken on [2026-01-03 Sat]\n"
                         "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30\n"
                         ":END:"))
         (src (concat "* Task\n" drawer "\n"))
         (headings (with-temp-buffer
                     (insert src)
                     (org-mode)
                     (dorg-parse-result-headings (dorg-parse-buffer))))
         (logbook (cl-loop for h in headings
                           thereis (cl-find-if
                                    (lambda (it)
                                      (and (cl-typep it 'dorg-drawer-item)
                                           (equal (dorg-drawer-item-name it) "LOGBOOK")))
                                    (dorg-heading-items h)))))
    (should logbook)
    ;; Parser preserved every content line verbatim, including the clock
    ;; line with its `=>  1:30' duration.
    (should (equal (dorg-drawer-item-lines logbook)
                   '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                     "- Note taken on [2026-01-03 Sat]"
                     "CLOCK: [2026-01-01 Thu 10:00]--[2026-01-01 Thu 11:30] =>  1:30")))
    ;; And rendering the parsed drawer reproduces the source block exactly.
    (should (equal (dorg-model--render-item logbook) drawer))))

(ert-deftest dorg-test-model-drawer-blank-line-parse-render-roundtrip ()
  "Generic-drawer content WITH blank lines round-trips byte-exact.
Closes the fidelity gap that scoped the earlier lock to blank-line-free
content: interior AND trailing blank lines survive parse->render so the
drawer re-renders identical to its source."
  (let* ((drawer (concat ":LOGBOOK:\n"
                         "- State \"DONE\" from \"TODO\" [2026-01-02 Fri]\n"
                         "\n"
                         "- Note taken on [2026-01-03 Sat]\n"
                         "\n"
                         ":END:"))
         (src (concat "* Task\n" drawer "\n"))
         (headings (with-temp-buffer
                     (insert src)
                     (org-mode)
                     (dorg-parse-result-headings (dorg-parse-buffer))))
         (logbook (cl-loop for h in headings
                           thereis (cl-find-if
                                    (lambda (it)
                                      (and (cl-typep it 'dorg-drawer-item)
                                           (equal (dorg-drawer-item-name it) "LOGBOOK")))
                                    (dorg-heading-items h)))))
    (should logbook)
    ;; Parser preserved the interior and trailing blank lines verbatim.
    (should (equal (dorg-drawer-item-lines logbook)
                   '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                     ""
                     "- Note taken on [2026-01-03 Sat]"
                     "")))
    ;; Rendering the parsed drawer reproduces the source block exactly.
    (should (equal (dorg-model--render-item logbook) drawer))))

;;; LOGBOOK note bodies

(ert-deftest dorg-test-model-logbook-note-body ()
  "A note header followed by indented continuation lines attaches the body.
AC2: single note entry whose `body' is the continuation lines joined by
\\n, verbatim (indentation preserved)."
  (let* ((lines '("- Note taken on [2026-02-02 Mon 14:30] \\\\"
                  "  First body line."
                  "  Second body line."))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((e (car entries)))
      (should (eq (dorg-logbook-entry-kind e) 'note))
      (should (equal (dorg-logbook-entry-body e)
                     "  First body line.\n  Second body line."))
      (should (equal (dorg-logbook-entry-raw e)
                     "- Note taken on [2026-02-02 Mon 14:30] \\\\")))))

(ert-deftest dorg-test-model-logbook-note-no-body ()
  "A bare note header (no continuation lines) yields a nil body.
AC2."
  (let* ((line "- Note taken on [2026-02-02 Mon 14:30]")
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines (list line)))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (should (eq (dorg-logbook-entry-kind (car entries)) 'note))
    (should (null (dorg-logbook-entry-body (car entries))))))

(ert-deftest dorg-test-model-logbook-note-body-no-leak ()
  "A body line carrying an inactive timestamp is folded, not emitted.
AC3: the timestamped continuation line does NOT become its own entry."
  (let* ((lines '("- Note taken on [2026-02-02 Mon 14:30] \\\\"
                  "  Saw it first on [2026-01-15 Thu] in the logs."))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (= (length entries) 1))
    (let ((e (car entries)))
      (should (eq (dorg-logbook-entry-kind e) 'note))
      (should (equal (dorg-logbook-entry-body e)
                     "  Saw it first on [2026-01-15 Thu] in the logs.")))))

(ert-deftest dorg-test-model-logbook-note-body-stops-at-next-entry ()
  "Body collection stops at the next sibling entry header.
AC4: a column-0 `- State' terminates the body and projects its own
entry; kinds are (note state-change)."
  (let* ((lines '("- Note taken on [2026-02-02 Mon 14:30] \\\\"
                  "  Body line."
                  "- State \"DONE\" from \"TODO\" [2026-02-03 Tue 10:00]"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(note state-change)))
    (should (equal (dorg-logbook-entry-body (nth 0 entries)) "  Body line."))
    (should (null (dorg-logbook-entry-body (nth 1 entries))))
    (should (equal (dorg-logbook-entry-to-state (nth 1 entries)) "DONE"))))

(ert-deftest dorg-test-model-logbook-note-body-stops-at-clock ()
  "A sibling CLOCK: line at the note's base indent terminates the body.
AC4: the CLOCK: line is excluded from the note body and projects its own
`clock' entry with duration/timestamp preserved and body nil.  Non-vacuous
re the State terminator: a regression that folded the clock into the body
would both drop the `clock' entry (kinds become (note)) and swallow the
CLOCK: text into the note body."
  (let* ((lines '("- Note taken on [2026-02-02 Mon 14:30] \\\\"
                  "  Body line."
                  "CLOCK: [2026-01-05 Mon 09:00]--[2026-01-05 Mon 10:00] =>  1:00"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(note clock)))
    (should (equal (dorg-logbook-entry-body (nth 0 entries)) "  Body line."))
    (let ((clk (nth 1 entries)))
      (should (eq (dorg-logbook-entry-kind clk) 'clock))
      (should (equal (dorg-logbook-entry-duration clk) "1:00"))
      (should (null (dorg-logbook-entry-body clk)))
      (should (equal (dorg-timestamp-start-date (dorg-logbook-entry-timestamp clk))
                     '(2026 1 5))))))

(ert-deftest dorg-test-model-logbook-note-body-indented-bullet ()
  "An indented bullet inside the body does not terminate the body.
AC4: `  - sub' (indent greater than the header) stays in `body'; only the
column-0 sibling entry terminates it."
  (let* ((lines '("- Note taken on [2026-02-02 Mon 14:30] \\\\"
                  "  Steps:"
                  "  - sub item one"
                  "  - sub item two"
                  "- State \"DONE\" from \"TODO\" [2026-02-03 Tue 10:00]"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(note state-change)))
    (should (equal (dorg-logbook-entry-body (nth 0 entries))
                   "  Steps:\n  - sub item one\n  - sub item two"))))

(ert-deftest dorg-test-model-logbook-non-note-kinds-nil-body ()
  "State-change, clock, and generic fallback entries all have a nil body.
AC5."
  (let* ((lines '("- State \"DONE\" from \"TODO\" [2026-01-02 Fri]"
                  "CLOCK: [2026-01-05 Mon 09:00]--[2026-01-05 Mon 10:00] =>  1:00"
                  "Clocked some work [2026-03-03 Tue 11:11]"))
         (h (make-dorg-heading
             :id 1 :level 1 :title "T"
             :items (list (dorg-drawer-item :name "LOGBOOK" :lines lines))))
         (entries (dorg-heading-logbook-entries h)))
    (should (equal (mapcar #'dorg-logbook-entry-kind entries)
                   '(state-change clock logbook)))
    (should (cl-every (lambda (e) (null (dorg-logbook-entry-body e))) entries))))

;;; SQL sort-key hard guard

(ert-deftest dorg-test-sql-gen-sort-key-hex-guard ()
  "`dorg-sql-gen--sort-key-hex' refuses keys longer than 64 bytes.
Keys up to and including 64 bytes remain writable for compatibility
with existing rows; anything longer signals before reaching SQL."
  ;; Short key — normal hex literal
  (should (equal (dorg-sql-gen--sort-key-hex (unibyte-string 1)) "X'01'"))
  ;; Exactly 64 bytes — still allowed
  (let ((k64 (make-string 64 ?A)))
    (should (stringp (dorg-sql-gen--sort-key-hex k64))))
  ;; 65 bytes — hard guard signals
  (let ((k65 (make-string 65 ?A)))
    (should-error (dorg-sql-gen--sort-key-hex k65) :type 'error)))

;;; DORG_* protected-keyword predicate — single case-insensitive rule
;;
;; tracked-regression consolidated four drifted DORG_* checks into one
;; public `dorg-protected-keyword-p' (+ prefix constant + derived SQL LIKE
;; pattern) in dorg-model.el.  These tests pin the predicate contract, the
;; SQL-pattern derivation (B3), the private-predicate removal census (B2),
;; and the wrong-state item-path transformation controls.

(ert-deftest dorg-test-model-protected-keyword-case-insensitive ()
  "`dorg-protected-keyword-p' owns the DORG_* rule case-insensitively.
Org keywords/properties are case-insensitive, so a lowercase #+dorg_id:
is owned exactly like #+DORG_ID:.  Non-strings and non-prefixed names are
not owned.  Reverting the predicate to case-sensitive
`(string-prefix-p \"DORG_\" name)' turns the lowercase assertions RED."
  ;; Owned — any case of the DORG_ prefix.
  (should (dorg-protected-keyword-p "DORG_ID"))
  (should (dorg-protected-keyword-p "dorg_id"))
  (should (dorg-protected-keyword-p "Dorg_Id"))
  (should (dorg-protected-keyword-p "DORG_HASH"))
  ;; Not owned.
  (should-not (dorg-protected-keyword-p nil))
  (should-not (dorg-protected-keyword-p 42))
  (should-not (dorg-protected-keyword-p ""))
  (should-not (dorg-protected-keyword-p "title"))
  (should-not (dorg-protected-keyword-p "dorgid")))  ; no underscore

(ert-deftest dorg-test-model-protected-keyword-sql-like-default ()
  "The SQL LIKE pattern is DERIVED from `dorg-protected-keyword-prefix',
not a duplicated 'DORG\\_%' literal.  For the default \"DORG_\" prefix the
`_' wildcard is escaped and the ESCAPE char is emitted explicitly, so the
SQL rule cannot drift from the Elisp predicate or depend on the server's
implicit backslash handling."
  (should (equal '("DORG!_%" . "!") (dorg-protected-keyword-sql-like))))

(ert-deftest dorg-test-model-protected-keyword-sql-like-derived-not-literal ()
  "The pattern is computed from the prefix constant, escaping EVERY LIKE
metachar (the escape char, `%', `_') — proving it is not a hardcoded
\"DORG_\" literal.  Rebinding the prefix to one containing wildcards yields
a fully-escaped pattern; a literal implementation would ignore the rebind
and this assertion would fail RED."
  (let ((dorg-protected-keyword-prefix "D_G%"))
    (should (equal '("D!_G!%%" . "!") (dorg-protected-keyword-sql-like)))))

(ert-deftest dorg-test-model-protected-predicate-consolidation-census ()
  "Exactly one predicate owns the rule.  The two retired private predicates
are removed (their callers repointed — otherwise the owning modules would
fail to load); the public predicate and prefix constant exist.  Re-adding
a private copy turns the `should-not' assertions RED (static consolidation
proof; behaviour alone cannot prove this because history was already
case-insensitive)."
  (should-not (fboundp 'dorg-history--protected-keyword-p))
  (should-not (fboundp 'dorg-mutate--protected-keyword-p))
  (should (fboundp 'dorg-protected-keyword-p))
  (should (boundp 'dorg-protected-keyword-prefix)))

(ert-deftest dorg-test-model-lowercase-protected-excluded-from-hash ()
  "Transformation (wrong state -> corrected): a heading carrying a lowercase
#+dorg_id: keyword item hashes IDENTICALLY to the same heading without it,
because `dorg-item--db-local-p' (via the shared predicate) filters it.  The
control keyword `TITLE' DOES change the hash, so the assertion is
non-vacuous: reverting the predicate to case-sensitive would let the
lowercase item leak into the hash and the equality would fail RED."
  (let* ((base (make-dorg-heading
                :id 1 :level 1 :title "H"
                :items (list (dorg-text-item :text "body\n"))))
         (with-protected (make-dorg-heading
                          :id 1 :level 1 :title "H"
                          :items (list (dorg-text-item :text "body\n")
                                       (dorg-keyword-item :keyword "dorg_id"
                                                          :value "leak"))))
         (with-normal (make-dorg-heading
                       :id 1 :level 1 :title "H"
                       :items (list (dorg-text-item :text "body\n")
                                    (dorg-keyword-item :keyword "TITLE"
                                                       :value "x"))))
         (h-base (dorg-heading-compute-items-hash base))
         (h-prot (dorg-heading-compute-items-hash with-protected))
         (h-norm (dorg-heading-compute-items-hash with-normal)))
    (should (equal h-base h-prot))          ; lowercase DORG_ item ignored
    (should-not (equal h-base h-norm))))    ; ordinary keyword is DB content

(ert-deftest dorg-test-model-lowercase-protected-excluded-from-sql-gen ()
  "SQL generation emits NO keyword_items insert for a lowercase #+dorg_id:
item, but DOES for an ordinary keyword — proving `dorg-item--db-local-p'
filters case-insensitively at the SQL boundary (non-vacuous control)."
  (let* ((idgen (let ((n 1000)) (lambda () (cl-incf n))))
         (protected (make-dorg-heading
                     :id 1 :level 1 :title "H"
                     :items (list (dorg-keyword-item :keyword "dorg_id"
                                                     :value "leak"))))
         (normal (make-dorg-heading
                  :id 1 :level 1 :title "H"
                  :items (list (dorg-keyword-item :keyword "MYKW"
                                                  :value "v"))))
         (sql-prot (dorg-sql-gen-heading-items-sql protected idgen))
         (sql-norm (dorg-sql-gen-heading-items-sql normal idgen)))
    (should-not (cl-some (lambda (s) (string-match-p "keyword_items" s)) sql-prot))
    (should (cl-some (lambda (s) (string-match-p "keyword_items" s)) sql-norm))))

(ert-deftest dorg-test-model-lowercase-protected-excluded-from-item-diff ()
  "Item-level diffing (`dorg-model-plan-items') treats a lowercase #+dorg_id:
item as db-local: a fresh heading gaining ONLY that item yields NO `:insert'
op, while gaining an ordinary keyword yields exactly one `:insert' (the
non-vacuity control)."
  (let* ((protected (list (dorg-keyword-item :keyword "dorg_id" :value "leak")))
         (normal (list (dorg-keyword-item :keyword "MYKW" :value "v")))
         (ops-prot (dorg-model-plan-items nil protected))
         (ops-norm (dorg-model-plan-items nil normal)))
    (should-not (cl-some (lambda (op) (eq (car op) :insert)) ops-prot))
    (should (= 1 (cl-count-if (lambda (op) (eq (car op) :insert)) ops-norm)))))

(ert-deftest dorg-test-model-history-restore-delegates-to-shared-predicate ()
  "History restore consults the DORG_* rule THROUGH `dorg-protected-keyword-p'.
`dorg-history--item-to-value' excludes a lowercase #+dorg_id: keyword item
(returns nil).  Because the old private `dorg-history--protected-keyword-p'
is removed, the exclusion can only happen by delegation to the shared
predicate — a spy on it MUST fire.  History was already case-insensitive, so
this delegation proof (not a behaviour comparison) is what establishes the
consolidation for the history caller.  Re-adding a private history predicate
would leave the spy unfired -> RED."
  (let ((fired 0)
        (item (dorg-keyword-item :keyword "dorg_id" :value "leak")))
    (advice-add 'dorg-protected-keyword-p :before
                (lambda (&rest _) (cl-incf fired))
                '((name . dorg-test--dxjnl-spy)))
    (unwind-protect
        (progn
          (should (null (dorg-history--item-to-value item)))
          (should (> fired 0)))
      (advice-remove 'dorg-protected-keyword-p 'dorg-test--dxjnl-spy))))

;;; ─── Item-Level Plan (moved by tracked-regression) ──────────────────────────
;;
;; Red specs for `dorg-model-plan-items', the pure LCS/positional item
;; reconciliation planner.  It accepts OLD DB-bound items (carrying
;; `dorg-item-id'/`dorg-item-sort-key') and NEW parsed items (those slots
;; nil) and returns ops in final document order:
;;   (:keep   OLD NEW FINAL-SORT-KEY)
;;   (:update OLD NEW FINAL-SORT-KEY)
;;   (:insert NEW FINAL-SORT-KEY)
;;   (:delete OLD)
;; These tests are RED until both the item identity slots and the planner
;; exist; they reuse existing DB item IDs so unchanged items never churn.

(defun dorg-test--db-text (id sk text)
  "A DB-bound text item: ID, integer sort position SK, TEXT."
  (dorg-text-item :id id :sort-key (dorg-sortkey-from-integer sk) :text text))

(defun dorg-test--new-text (text)
  "A parser-created text item (no id/sort-key) with TEXT."
  (dorg-text-item :text text))

(defun dorg-test--op-old-id (op)
  "Old DB item id referenced by planner OP, or nil for :insert."
  (pcase (car op)
    ((or :keep :update :delete) (dorg-item-id (nth 1 op)))
    (_ nil)))

(defun dorg-test--ops-of (tag ops)
  "Return the subset of OPS whose tag is TAG."
  (cl-remove-if-not (lambda (op) (eq (car op) tag)) ops))

(ert-deftest dorg-test-model-plan-items-lcs-anchor-insert ()
  "LCS anchors keep IDs; a middle insert adds exactly one item (AC3).
old A B C, new A X B C -> A/B/C keep their DB ids, X is the only :insert."
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "B")
                    (dorg-test--db-text 3 3 "C")))
         (new (list (dorg-test--new-text "A")
                    (dorg-test--new-text "X")
                    (dorg-test--new-text "B")
                    (dorg-test--new-text "C")))
         (ops (dorg-model-plan-items old new)))
    ;; Exactly one insert (X), no deletes.
    (should (= (length (dorg-test--ops-of :insert ops)) 1))
    (should (= (length (dorg-test--ops-of :delete ops)) 0))
    (should (equal (dorg-text-item-text (nth 1 (car (dorg-test--ops-of :insert ops))))
                   "X"))
    ;; A/B/C reuse their DB ids and are :keep (content unchanged).
    (should (equal (sort (mapcar #'dorg-test--op-old-id
                                 (append (dorg-test--ops-of :keep ops)
                                         (dorg-test--ops-of :update ops)))
                         #'<)
                   '(1 2 3)))
    (should (null (dorg-test--ops-of :update ops)))))

(ert-deftest dorg-test-model-plan-items-middle-delete ()
  "Middle deletion removes exactly one item; neighbors keep IDs (AC4).
old A B C, new A C -> B is the only :delete, A/C keep DB ids."
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "B")
                    (dorg-test--db-text 3 3 "C")))
         (new (list (dorg-test--new-text "A")
                    (dorg-test--new-text "C")))
         (ops (dorg-model-plan-items old new)))
    (should (= (length (dorg-test--ops-of :insert ops)) 0))
    (let ((dels (dorg-test--ops-of :delete ops)))
      (should (= (length dels) 1))
      (should (= (dorg-test--op-old-id (car dels)) 2)))
    (should (equal (sort (mapcar #'dorg-test--op-old-id
                                 (dorg-test--ops-of :keep ops))
                         #'<)
                   '(1 3)))))

(ert-deftest dorg-test-model-plan-items-content-edit-updates ()
  "Same-position content edit reuses the DB id via :update (AC5).
old A B C, new A B' C -> B keeps id 2 and becomes :update; A/C :keep."
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "B")
                    (dorg-test--db-text 3 3 "C")))
         (new (list (dorg-test--new-text "A")
                    (dorg-test--new-text "B-edited")
                    (dorg-test--new-text "C")))
         (ops (dorg-model-plan-items old new)))
    (should (null (dorg-test--ops-of :insert ops)))
    (should (null (dorg-test--ops-of :delete ops)))
    (let ((ups (dorg-test--ops-of :update ops)))
      (should (= (length ups) 1))
      (should (= (dorg-test--op-old-id (car ups)) 2))
      (should (equal (dorg-text-item-text (nth 2 (car ups))) "B-edited")))
    (should (equal (sort (mapcar #'dorg-test--op-old-id
                                 (dorg-test--ops-of :keep ops))
                         #'<)
                   '(1 3)))))

(ert-deftest dorg-test-model-plan-items-keyword-name-compat ()
  "Keyword items pair only by matching keyword name (AC6).
Same name + new value -> :update; different name -> delete+insert."
  ;; Same keyword name, changed value: reuse id via :update.
  (let* ((old (list (dorg-keyword-item :id 5 :sort-key (dorg-sortkey-from-integer 1)
                                       :keyword "TITLE" :value "Old")))
         (new (list (dorg-keyword-item :keyword "TITLE" :value "New")))
         (ops (dorg-model-plan-items old new)))
    (should (equal (mapcar #'car ops) '(:update)))
    (should (= (dorg-test--op-old-id (car ops)) 5)))
  ;; Different keyword name: not compatible -> delete old + insert new.
  (let* ((old (list (dorg-keyword-item :id 5 :sort-key (dorg-sortkey-from-integer 1)
                                       :keyword "TITLE" :value "Old")))
         (new (list (dorg-keyword-item :keyword "AUTHOR" :value "X")))
         (ops (dorg-model-plan-items old new)))
    (should (= (length (dorg-test--ops-of :delete ops)) 1))
    (should (= (length (dorg-test--ops-of :insert ops)) 1))
    (should (null (dorg-test--ops-of :update ops)))))

(ert-deftest dorg-test-model-plan-items-kind-change-delete-insert ()
  "An incompatible kind change becomes delete+insert for that item (AC6).
old [text] -> new [keyword] cannot reuse the row."
  (let* ((old (list (dorg-test--db-text 7 1 "body")))
         (new (list (dorg-keyword-item :keyword "TITLE" :value "x")))
         (ops (dorg-model-plan-items old new)))
    (should (= (length (dorg-test--ops-of :delete ops)) 1))
    (should (= (dorg-test--op-old-id (car (dorg-test--ops-of :delete ops))) 7))
    (should (= (length (dorg-test--ops-of :insert ops)) 1))))

(ert-deftest dorg-test-model-plan-items-drawer-name-compat ()
  "Drawer items pair only by matching drawer name (AC6).
Same name, changed lines -> :update reusing id; different name -> del+ins."
  (let* ((old (list (dorg-drawer-item :id 9 :sort-key (dorg-sortkey-from-integer 1)
                                      :name "LOGBOOK" :lines '("a"))))
         (new (list (dorg-drawer-item :name "LOGBOOK" :lines '("a" "b"))))
         (ops (dorg-model-plan-items old new)))
    (should (equal (mapcar #'car ops) '(:update)))
    (should (= (dorg-test--op-old-id (car ops)) 9)))
  (let* ((old (list (dorg-drawer-item :id 9 :sort-key (dorg-sortkey-from-integer 1)
                                      :name "LOGBOOK" :lines '("a"))))
         (new (list (dorg-drawer-item :name "NOTES" :lines '("a"))))
         (ops (dorg-model-plan-items old new)))
    (should (= (length (dorg-test--ops-of :delete ops)) 1))
    (should (= (length (dorg-test--ops-of :insert ops)) 1))))

(ert-deftest dorg-test-model-plan-items-planning-singleton ()
  "Planning items pair as the singleton planning item (AC6).
A changed planning line reuses the old planning row id via :update even
when no exact LCS match exists; it never becomes delete+insert."
  (let* ((old (list (dorg-planning-item :id 3 :sort-key (dorg-sortkey-from-integer 1)
                                        :scheduled "<2026-01-01 Thu>")))
         (new (list (dorg-planning-item :scheduled "<2026-02-02 Mon>"
                                        :deadline "<2026-02-03 Tue>")))
         (ops (dorg-model-plan-items old new)))
    (should (equal (mapcar #'car ops) '(:update)))
    (should (= (dorg-test--op-old-id (car ops)) 3))
    ;; The singleton pairs even when surrounded by other unmatched items.
    (let* ((old2 (list (dorg-test--db-text 1 1 "A")
                       (dorg-planning-item :id 2 :sort-key (dorg-sortkey-from-integer 2)
                                           :scheduled "<2026-01-01 Thu>")))
           (new2 (list (dorg-test--new-text "A")
                       (dorg-planning-item :scheduled "<2026-09-09 Wed>")))
           (ops2 (dorg-model-plan-items old2 new2)))
      ;; No planning delete/insert: the planning row id 2 is updated in place.
      (should (null (dorg-test--ops-of :insert ops2)))
      (should (null (dorg-test--ops-of :delete ops2)))
      (let ((plan-op (cl-find-if
                      (lambda (op) (equal (dorg-test--op-old-id op) 2)) ops2)))
        (should (eq (car plan-op) :update))))))

(ert-deftest dorg-test-model-plan-items-duplicate-content ()
  "Duplicate identical items match deterministically in document order (AC7).
Two old A's, two new A's -> both keep, distinct ids, none reused twice."
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "A")))
         (new (list (dorg-test--new-text "A")
                    (dorg-test--new-text "A")))
         (ops (dorg-model-plan-items old new))
         (kept (sort (mapcar #'dorg-test--op-old-id
                             (dorg-test--ops-of :keep ops))
                     #'<)))
    (should (null (dorg-test--ops-of :insert ops)))
    (should (null (dorg-test--ops-of :delete ops)))
    (should (equal kept '(1 2))))
  ;; Two old A's, one new A: keep the first, delete the second; never map
  ;; two ops to the same old id.
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "A")))
         (new (list (dorg-test--new-text "A")))
         (ops (dorg-model-plan-items old new)))
    (should (= (length (dorg-test--ops-of :keep ops)) 1))
    (should (= (dorg-test--op-old-id (car (dorg-test--ops-of :keep ops))) 1))
    (should (= (length (dorg-test--ops-of :delete ops)) 1))
    (should (= (dorg-test--op-old-id (car (dorg-test--ops-of :delete ops))) 2))))

(ert-deftest dorg-test-model-plan-items-reorder-preserves-ids ()
  "Reordering unique items preserves IDs and reassigns sort keys (AC8).
old A(1) B(2), new B A -> ids {1,2} retained, final keys in new order."
  (let* ((old (list (dorg-test--db-text 1 1 "A")
                    (dorg-test--db-text 2 2 "B")))
         (new (list (dorg-test--new-text "B")
                    (dorg-test--new-text "A")))
         (ops (dorg-model-plan-items old new)))
    (should (null (dorg-test--ops-of :insert ops)))
    (should (null (dorg-test--ops-of :delete ops)))
    ;; Both ids survive (keep or update), none duplicated.
    (let ((ids (sort (mapcar #'dorg-test--op-old-id ops) #'<)))
      (should (equal ids '(1 2))))
    ;; Final sort keys follow new document order: B's op carries a
    ;; strictly smaller final key than A's op.
    (let* ((b-op (cl-find-if (lambda (op) (equal (dorg-test--op-old-id op) 2)) ops))
           (a-op (cl-find-if (lambda (op) (equal (dorg-test--op-old-id op) 1)) ops))
           (b-key (nth 3 b-op))
           (a-key (nth 3 a-op)))
      (should (string< b-key a-key)))))

(ert-deftest dorg-test-model-plan-items-sortkey-midpoint-insert ()
  "Insert between reused neighbors gets a midpoint key; neighbors keep keys (AC9)."
  (let* ((sk-a (dorg-sortkey-from-integer 10))
         (sk-c (dorg-sortkey-from-integer 20))
         (old (list (dorg-text-item :id 1 :sort-key sk-a :text "A")
                    (dorg-text-item :id 3 :sort-key sk-c :text "C")))
         (new (list (dorg-test--new-text "A")
                    (dorg-test--new-text "B")
                    (dorg-test--new-text "C")))
         (ops (dorg-model-plan-items old new))
         (a-op (cl-find-if (lambda (op) (equal (dorg-test--op-old-id op) 1)) ops))
         (c-op (cl-find-if (lambda (op) (equal (dorg-test--op-old-id op) 3)) ops))
         (ins (car (dorg-test--ops-of :insert ops))))
    ;; Reused neighbors keep their existing DB keys.
    (should (equal (nth 3 a-op) sk-a))
    (should (equal (nth 3 c-op) sk-c))
    ;; The inserted B's key sorts strictly between A and C.
    (let ((b-key (nth 2 ins)))
      (should (string< sk-a b-key))
      (should (string< b-key sk-c)))))

(ert-deftest dorg-test-model-plan-items-fresh-keys-all-inserts ()
  "With no reusable bounds, inserts get strictly ordered unique keys (AC9)."
  (let* ((ops (dorg-model-plan-items
               nil
               (list (dorg-test--new-text "A")
                     (dorg-test--new-text "B")
                     (dorg-test--new-text "C"))))
         (keys (mapcar (lambda (op) (nth 2 op))
                       (dorg-test--ops-of :insert ops))))
    (should (= (length keys) 3))
    (should (equal keys (sort (copy-sequence keys) #'string<)))
    (should (= (length (delete-dups (copy-sequence keys))) 3))))

(ert-deftest dorg-test-model-plan-items-filters-dorg-keywords ()
  "DORG_* keyword items never appear in the plan (AC2).
A DORG_* item on either side produces no insert/update/delete/keep op."
  (let* ((old (list (dorg-keyword-item :id 1 :sort-key (dorg-sortkey-from-integer 1)
                                       :keyword "DORG_HASH" :value "abc")
                    (dorg-test--db-text 2 2 "A")))
         (new (list (dorg-keyword-item :keyword "DORG_HASH" :value "def")
                    (dorg-test--new-text "A")))
         (ops (dorg-model-plan-items old new)))
    ;; Only the text item A is planned (kept); the DORG_HASH item is ignored.
    (should (equal (mapcar #'car ops) '(:keep)))
    (should (= (dorg-test--op-old-id (car ops)) 2))))

;;; ─── tracked-regression relocation contract (Family D2) ─────────────────────

(defconst dorg-model-tests--qc260-repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-model-tests--qc260-name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-model-tests--qc260-repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-model-retired-child-renderer-absent ()
  "The unused child renderer is absent from loaded code and source.
Non-vacuous before its deletion: both the loaded function and its defining
form contribute distinct violations.  The retired name is assembled from
fragments so this guard cannot keep its own source-census witness alive."
  (let* ((name (concat "dorg-model-render-" "children"))
         (sym (intern name))
         (violations
          (delq nil
                (list (and (fboundp sym) :fboundp)
                      (and (dorg-model-tests--qc260-name-hits (list name))
                           :source)))))
    (should-not violations)))

(ert-deftest dorg-test-model-item-plan-relocation-contract ()
  "tracked-regression Family D2: item comparison/planning lives in dorg-model.el.
RED at SPEC (new names undefined, retired diff-prefixed names still defined
and referenced); GREEN after IMPL."
  (let ((pairs '(("-plan-items"             . dorg-model-plan-items)
                 ("-items-changed-p"        . dorg-model-items-changed-p)
                 ("-items-compatible-p"     . dorg-model--items-compatible-p)
                 ("-plan-items-assign-keys" . dorg-model--plan-items-assign-keys)
                 ("-item-match-key"         . dorg-model--item-match-key)
                 ("-item-semantic-equal-p"  . dorg-model--item-semantic-equal-p)))
        (old-prefix (concat "dorg-diff" "-")))
    (dolist (p pairs)
      (let ((old (intern-soft (concat old-prefix (car p)))))
        (should (fboundp (cdr p)))
        (should-not (and old (fboundp old)))))
    (should-not (dorg-model-tests--qc260-name-hits
                 (mapcar (lambda (p) (concat old-prefix (car p))) pairs)))))

(provide 'dorg-model-tests)
;;; dorg-model-tests.el ends here
