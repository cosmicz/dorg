;;; dorg-level-offset-tests.el --- Parent-relative heading offsets -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dolt-free controls for parent-relative heading storage.  A heading's
;; persisted fact is its LEVEL-OFFSET (root 0, every real heading positive);
;; its effective absolute Org level is DERIVED from complete ancestry and is
;; never a second persisted authority.  These controls exercise the model,
;; the parser, the plain adapter and the pure representability rule without
;; a dolt binary, a sql-server or a store.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-ql-plain)
(require 'dorg-ql-model)
(require 'dorg-model-load)
(require 'dorg-dolt)
(require 'dorg-mutate)
(require 'dorg-api)
(require 'dorg-mutate-grammar)
(require 'dorg-ext)

;;; ─── Fixtures ───────────────────────────────────────────────────────────

(defun dorg-test-level-offset--parse (text)
  "Parse Org TEXT with a fixed root id and assigned ids; return the result."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode))
    (insert text)
    (let ((result (dorg-parse-buffer nil 1))
          (next 100))
      (dorg-parse-assign-ids result (lambda () (cl-incf next)))
      result)))

(defun dorg-test-level-offset--table (headings)
  "Return an id -> heading hash table of HEADINGS."
  (let ((table (make-hash-table :test 'eql)))
    (dolist (h headings table)
      (puthash (dorg-heading-id h) h table))))

(defun dorg-test-level-offset--heading (id parent-id offset &optional sort level)
  "Return a heading ID under PARENT-ID with OFFSET, SORT key and LEVEL."
  (make-dorg-heading :id id :parent-id parent-id
                     :sort-key (dorg-sortkey-from-integer (or sort 1) 2)
                     :level level :level-offset offset
                     :title (format "h%d" id) :post-blank 0))

(defun dorg-test-level-offset--titles-and (result slot)
  "Return (TITLE . SLOT-VALUE) for RESULT's non-root headings."
  (mapcar (lambda (h) (cons (dorg-heading-title h) (funcall slot h)))
          (cdr (dorg-parse-result-headings result))))

;;; ─── Parser ─────────────────────────────────────────────────────────────

(ert-deftest dorg-test-level-offset-parse-result-sets-both-slots ()
  "Every parsed heading carries its effective level AND its local offset.
Skipped levels survive as offsets greater than 1; the root is offset 0."
  (let* ((result (dorg-test-level-offset--parse
                  "* A\n*** B\n** C\n* D\n"))
         (root (car (dorg-parse-result-headings result))))
    (should (eql 0 (dorg-heading-level root)))
    (should (eql 0 (dorg-heading-level-offset root)))
    (should (equal (dorg-test-level-offset--titles-and
                    result #'dorg-heading-level)
                   '(("A" . 1) ("B" . 3) ("C" . 2) ("D" . 1))))
    (should (equal (dorg-test-level-offset--titles-and
                    result #'dorg-heading-level-offset)
                   '(("A" . 1) ("B" . 2) ("C" . 1) ("D" . 1))))
    ;; A bare parse result, which downstream consumers read without identity
    ;; completion, carries the same offsets.
    (let ((bare (with-temp-buffer
                  (let ((org-inhibit-startup t))
                    (org-mode))
                  (insert "* A\n*** B\n** C\n* D\n")
                  (dorg-parse-buffer nil 1))))
      (should (equal (dorg-test-level-offset--titles-and
                      bare #'dorg-heading-level-offset)
                     '(("A" . 1) ("B" . 2) ("C" . 1) ("D" . 1)))))))

(ert-deftest dorg-test-level-offset-skipped-level-parse-render-roundtrip ()
  "Rendering from levels DERIVED from offsets reproduces skipped stars.
The parse levels are erased first, so only the offsets can carry the gap."
  (let* ((text "* A\n*** B\n**** C\n** D\n* E\n")
         (result (dorg-test-level-offset--parse text))
         (headings (dorg-parse-result-headings result))
         (table (dorg-test-level-offset--table headings))
         (given (dorg-model-render-tree headings)))
    (maphash (lambda (_id h) (setf (dorg-heading-level h) nil)) table)
    (dorg-model-derive-levels table 1)
    (let ((derived (dorg-model-render-tree headings)))
      (should (equal derived given))
      (should (equal (mapcar (lambda (line) (car (split-string line " ")))
                             (seq-filter (lambda (line)
                                           (string-match-p "\\`\\*+ " line))
                                         (split-string derived "\n")))
                     (quote ("*" "***" "****" "**" "*")))))))

(ert-deftest dorg-test-level-offset-parser-output-always-representable ()
  "GUARD: parsed direct-sibling levels never increase in render order.
Uses only trunk-stable accessors (parent id, sort key, absolute level), so
it states the parser's structural invariant independently of offsets.  The
corpus deliberately contains no verbatim-block phantom headlines."
  (dolist (text '("* A\n*** B\n** C\n* D\n"
                  "*** deep\n* shallow\n** mid\n"
                  "* A\n** B\n**** C\n*** D\n** E\n* F\n"
                  "** two\n* one\n*** three\n** two-again\n"))
    (let* ((result (dorg-test-level-offset--parse text))
           (by-parent (make-hash-table :test 'eql)))
      (dolist (h (cdr (dorg-parse-result-headings result)))
        (push h (gethash (dorg-heading-parent-id h) by-parent)))
      (maphash
       (lambda (_parent kids)
         (let ((levels (mapcar #'dorg-heading-level
                               (sort kids (lambda (a b)
                                            (string< (dorg-heading-sort-key a)
                                                     (dorg-heading-sort-key b)))))))
           (ert-info ((format "%S fan %S" text levels))
             (should (equal levels (sort (copy-sequence levels) #'>=))))))
       by-parent))))

;;; ─── Derivation ─────────────────────────────────────────────────────────

(ert-deftest dorg-test-level-offset-partial-candidate-no-fabricated-level ()
  "A heading whose ancestry is not closed gets NO level, never 0 or its offset.
The effective accessor then refuses with a typed error."
  (let ((table (dorg-test-level-offset--table
                (list (dorg-test-level-offset--heading 1 nil 0)
                      (dorg-test-level-offset--heading 2 1 2)
                      ;; 4's parent 3 is not loaded.
                      (dorg-test-level-offset--heading 4 3 1)
                      (dorg-test-level-offset--heading 5 4 3)))))
    (dorg-model-derive-levels table 1)
    (should (eql 0 (dorg-heading-level (gethash 1 table))))
    (should (eql 2 (dorg-heading-level (gethash 2 table))))
    (should-not (dorg-heading-level (gethash 4 table)))
    (should-not (dorg-heading-level (gethash 5 table)))
    (should (eql 2 (dorg-model-effective-level (gethash 2 table))))
    (should-error (dorg-model-effective-level (gethash 5 table))
                  :type 'dorg-model-incomplete-ancestry)))

(ert-deftest dorg-test-level-offset-detached-copy-contract ()
  "A downstream detached owner copy is single-authority only when normalized.
Copy rule: synthetic root offset 0, copied owner offset 1, descendants keep
their offsets.  Deriving over that copy reproduces the rebased levels
1 + L(x) - L(owner).  Leaving the owner at its stored offset must DISAGREE."
  (let* ((tree (dorg-test-level-offset--table
                (list (dorg-test-level-offset--heading 1 nil 0)
                      (dorg-test-level-offset--heading 2 1 1)
                      (dorg-test-level-offset--heading 3 2 3) ; owner, L=4
                      (dorg-test-level-offset--heading 4 3 2) ; L=6
                      (dorg-test-level-offset--heading 5 4 1))))) ; L=7
    (dorg-model-derive-levels tree 1)
    (let ((owner-level (dorg-heading-level (gethash 3 tree))))
      (should (eql 4 owner-level))
      (dolist (normalize '(t nil))
        (let ((copy (make-hash-table :test 'eql))
              (rebased nil))
          (puthash -1 (dorg-test-level-offset--heading -1 nil 0 1 0) copy)
          (dolist (id '(3 4 5))
            (let ((c (copy-dorg-heading (gethash id tree))))
              (setf (dorg-heading-level c)
                    (1+ (- (dorg-heading-level (gethash id tree)) owner-level)))
              (when (eql id 3)
                (setf (dorg-heading-parent-id c) -1)
                (when normalize (setf (dorg-heading-level-offset c) 1)))
              (push (cons id (dorg-heading-level c)) rebased)
              (puthash id c copy)))
          (dorg-model-derive-levels copy -1)
          (let ((derived (mapcar (lambda (cell)
                                   (cons (car cell)
                                         (dorg-heading-level
                                          (gethash (car cell) copy))))
                                 rebased)))
            (if normalize
                (should (equal derived rebased))
              (should-not (equal derived rebased)))))))))

;;; ─── Representability ───────────────────────────────────────────────────

(ert-deftest dorg-test-level-offset-rejects-nonrepresentable-sibling-order ()
  "Direct-sibling offsets must be non-increasing in render order.
A placement is valid exactly when pred >= offset >= succ, with a missing
neighbour unconstrained; a whole fan is valid when non-increasing."
  ;; Explicit offset 3 as the LAST child after a sibling at offset 1: refused.
  (should-not (dorg-model-placement-representable-p 3 1 nil))
  ;; Offset 3 as the FIRST child before a sibling at offset 1: accepted.
  (should (dorg-model-placement-representable-p 3 nil 1))
  ;; Default offset 1 before a gapped successor at offset 2: refused.
  (should-not (dorg-model-placement-representable-p 1 nil 2))
  (should (dorg-model-placement-representable-p 1 1 1))
  (should (dorg-model-placement-representable-p 2 nil nil))
  (let ((fan (dorg-test-level-offset--table
              (list (dorg-test-level-offset--heading 1 nil 0)
                    (dorg-test-level-offset--heading 2 1 3 1)
                    (dorg-test-level-offset--heading 3 1 1 2)
                    (dorg-test-level-offset--heading 4 1 1 3)))))
    (should (dorg-model-sibling-fan-representable-p fan 1))
    (setf (dorg-heading-level-offset (gethash 4 fan)) 2)
    (should-not (dorg-model-sibling-fan-representable-p fan 1))))

;;; ─── Plain adapter ──────────────────────────────────────────────────────

(ert-deftest dorg-test-level-offset-plain-index-splices-verbatim-phantoms ()
  "A verbatim phantom is neither indexed nor an ancestor of its real child.

The real child is structurally spliced to Parent, matching `dorg-parse-buffer'.
This is intentionally a direct index witness: removing the verbatim filter
adds Sample and makes Child inherit the wrong parent."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode))
    (insert "* Parent\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
            "#+begin_example\n** Sample\n#+end_example\n"
            "** Child\n:PROPERTIES:\n:DORG_ID: 2\n:END:\n")
    (let* ((index (dorg-ql--plain-build-index))
           (headings (plist-get index :ordered))
           (by-title (lambda (title)
                       (cl-find title headings :key #'dorg-heading-title
                                :test #'equal)))
           (child (funcall by-title "Child"))
           (parent (funcall by-title "Parent")))
      (should (equal '("Parent" "Child")
                     (mapcar #'dorg-heading-title headings)))
      (should child)
      (should parent)
      (should (= (dorg-heading-id parent) (dorg-heading-parent-id child)))
      (should (= 1 (dorg-heading-level-offset child))))))

(ert-deftest dorg-test-level-offset-plain-index-decodes-after-phantom-filter ()
  "The index computes parser regions once and never decodes a phantom.

This observes the real parser and real index; only call recording is wrapped.
The event order makes the region predicate precede every element decode."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode))
    (insert "* Parent\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
            "#+begin_example\n** Sample\n#+end_example\n"
            "** Child\n:PROPERTIES:\n:DORG_ID: 2\n:END:\n")
    (let ((regions 0) (events nil)
          (real-regions (symbol-function 'dorg-parse--verbatim-regions))
          (real-position (symbol-function 'dorg-parse--position-verbatim-p))
          (real-element (symbol-function 'org-element-at-point)))
      (cl-letf (((symbol-function 'dorg-parse--verbatim-regions)
                 (lambda () (setq regions (1+ regions))
                   (funcall real-regions)))
                ((symbol-function 'dorg-parse--position-verbatim-p)
                 (lambda (pos table)
                   (push (list :filter pos) events)
                   (funcall real-position pos table)))
                ((symbol-function 'org-element-at-point)
                 (lambda (&rest args)
                   (push (list :decode (point)) events)
                   (apply real-element args))))
        (let ((index (dorg-ql--plain-build-index)))
          (should (equal '("Parent" "Child")
                         (mapcar #'dorg-heading-title
                                 (plist-get index :ordered))))))
      (should (= 1 regions))
      (let ((chronological (nreverse events)))
        (should (= 3 (length (seq-filter (lambda (event)
                                           (eq (car event) :filter))
                                         chronological))))
        (should (= 2 (length (seq-filter (lambda (event)
                                           (eq (car event) :decode))
                                         chronological))))
        (let ((last-filter nil))
          (dolist (event chronological)
            (pcase event
              (`(:filter ,pos) (setq last-filter pos))
              (`(:decode ,pos) (should (= last-filter pos))))))))))

(ert-deftest dorg-test-level-offset-plain-index-keeps-literal-local-properties ()
  "The one-element index scan keeps only each heading's literal drawer lines.

Lowercase and duplicate keys remain distinct entries, a literal plus key is
not DORG_ID accumulation, and neither inherited nor body drawers leak in."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode))
    (insert "* Owner\n:PROPERTIES:\n:DORG_ID: 1\n:foo: lower\n"
            ":DUPE: one\n:DUPE: two\n:PLUS+: literal\n:END:\n"
            "** Child\n:PROPERTIES:\n:DORG_ID: 2\n:END:\n"
            "* Body drawer\nText before the drawer.\n:PROPERTIES:\n"
            ":LATE: absent\n:END:\n"
            "* Tab planned\nSCHEDULED: <2026-10-01 Wed>\n\t:PROPERTIES:\n"
            "\t:DORG_ID: 3\n\t:TAB: kept\n\t:END:\n")
    (let* ((index (dorg-ql--plain-build-index))
           (headings (plist-get index :ordered))
           (props (plist-get index :props))
           (by-title (lambda (title)
                       (cl-find title headings :key #'dorg-heading-title
                                :test #'equal))))
      (should (equal '(("foo" . "lower") ("DUPE" . "one")
                       ("DUPE" . "two") ("PLUS+" . "literal"))
                     (gethash (dorg-heading-id (funcall by-title "Owner")) props)))
      ;; Child inherits no literal owner property, and the late body drawer is
      ;; not a heading property drawer at all.
      (should-not (gethash (dorg-heading-id (funcall by-title "Child")) props))
      (should-not (gethash (dorg-heading-id (funcall by-title "Body drawer")) props))
      ;; A planning line may precede an indented drawer; its :END: marker is
      ;; never a literal property.
      (should (equal '(("TAB" . "kept"))
                     (gethash (dorg-heading-id (funcall by-title "Tab planned")) props))))))

(ert-deftest dorg-test-level-offset-plain-ql-level-predicate-effective ()
  "The plain index carries effective levels and offsets from the buffer.
A `level' predicate compares the EFFECTIVE level, not the offset."
  (with-temp-buffer
    (let ((org-inhibit-startup t))
      (org-mode))
    (insert "* A\n*** B\n** C\n")
    (let* ((index (dorg-ql--plain-build-index))
           (hs (plist-get index :ordered))
           (by-title (lambda (title)
                       (cl-find title hs :key #'dorg-heading-title
                                :test #'equal))))
      (should (equal (mapcar #'dorg-heading-level-offset hs) '(1 2 1)))
      (should (dorg-ql--cmp-match-p 'level '= 3 (funcall by-title "B")))
      (should-not (dorg-ql--cmp-match-p 'level '= 2 (funcall by-title "B")))
      (should (dorg-ql--cmp-match-p 'level '= 2 (funcall by-title "C"))))))

;;; ─── Loader API and statement counts (stubbed query, no store) ─────────

(defun dorg-test-level-offset--count-statements (&rest args)
  "Call `dorg-model-load-headings' with ARGS and a recording query; return it.
ARGS are the leading arguments up to HEADLINE-ONLY; the recording query is
passed in the QUERY-FN position and answers every statement with no rows.
Returns the list of SQL strings issued, in order."
  (let ((seen nil))
    (apply #'dorg-model-load-headings
           (append args (list (lambda (sql) (push sql seen) nil))))
    (nreverse seen)))

(ert-deftest dorg-test-level-offset-load-api-rejects-unknown-levels-value ()
  "LEVELS accepts only nil, `effective' and `offsets'."
  (dolist (bad '(absolute t 0 "offsets"))
    (should-error (dorg-model-load-headings nil 1 nil nil nil
                                            (lambda (_sql) nil) bad)
                  :type 'wrong-type-argument))
  (dolist (good '(nil effective offsets))
    (should (hash-table-p (dorg-model-load-headings nil 1 nil nil nil
                                                    (lambda (_sql) nil) good)))))

(ert-deftest dorg-test-level-offset-load-api-query-fn-position-preserved ()
  "GUARD: QUERY-FN stays the sixth positional argument and receives every read.
No `dorg-sql' call may happen when a query function is supplied."
  (cl-letf (((symbol-function 'dorg-sql)
             (lambda (&rest _) (error "dorg-sql must not be called"))))
    (let ((seen (dorg-test-level-offset--count-statements nil 1 '(2 3) nil nil)))
      (should seen)
      (should (cl-every #'stringp seen)))))

(ert-deftest dorg-test-level-offset-slice-load-statement-count-unchanged ()
  "GUARD: every load shape issues exactly trunk's statement count.
The effective-level walk REPLACES the headings SELECT; it never adds one."
  (should (= 5 (length (dorg-test-level-offset--count-statements nil 1 nil nil nil))))
  (should (= 2 (length (dorg-test-level-offset--count-statements nil 1 nil nil t))))
  (should (= 3 (length (dorg-test-level-offset--count-statements nil 1 '(2 3) nil nil))))
  (should (= 2 (length (dorg-test-level-offset--count-statements nil 1 '(2 3) nil t)))))

(ert-deftest dorg-test-level-offset-bounded-producer-counts-preserved ()
  "GUARD: the downstream owner-slice read shape keeps its statement count.
That read is an id slice at an explicit AS-OF head with full bodies, in the
default effective mode; it issues exactly trunk's count and never adds a
separate ancestor-spine read."
  (let ((seen (dorg-test-level-offset--count-statements nil 1 '(2 3) "abc123" nil)))
    (should (= 3 (length seen)))
    (should-not (cl-some (lambda (sql) (string-match-p "WITH RECURSIVE anc" sql))
                         seen))))

(ert-deftest dorg-test-level-offset-migration-validator-select-only ()
  "GUARD: the validate window admits one SELECT only, so no recursion or writes.
The v8 migration validator therefore derives levels in Elisp."
  (should (dorg-sql--validate-query-allowed-p "SELECT id, level_offset FROM headings"))
  (should-not (dorg-sql--validate-query-allowed-p
               "WITH RECURSIVE w AS (SELECT 1) SELECT * FROM w"))
  (should-not (dorg-sql--validate-query-allowed-p "UPDATE headings SET level_offset = 1"))
  (should-not (dorg-sql--validate-query-allowed-p "SELECT 1; SELECT 2")))

;;; ─── Move semantics on the candidate model (no store) ──────────────────

(defun dorg-test-level-offset--ps (&optional order-facts-fn)
  "Return a candidate model: root 1, A 2 > X 3 (offset 3) > Xc 4 (offset 2), B 5.
X carries a skipped-level gap so an omitted offset visibly resets it.
ORDER-FACTS-FN, when non-nil, makes the model sparse."
  (let ((table (dorg-test-level-offset--table
                (list (dorg-test-level-offset--heading 1 nil 0 1)
                      (dorg-test-level-offset--heading 2 1 1 1)
                      (dorg-test-level-offset--heading 3 2 3 1)
                      (dorg-test-level-offset--heading 4 3 2 1)
                      (dorg-test-level-offset--heading 5 1 1 2)))))
    (dorg-model-derive-levels table 1)
    (dorg-mutate--ps-create :headings table :root-id 1
                            :order-facts-fn order-facts-fn)))

(defun dorg-test-level-offset--move (ps id under &rest to)
  "Move ID under heading UNDER in PS with extra TO fields; return PS."
  (dorg-mutate--ps-move
   ps (list :op 'move :target (list :subtree :id id)
            :to (append (list :under (list :heading :id under)) to)))
  ps)

(defun dorg-test-level-offset--h (ps id)
  "Return heading ID from candidate PS."
  (gethash id (dorg-mutate--ps-headings ps)))

(ert-deftest dorg-test-level-offset-move-omitted-resets-old-gap ()
  "An omitted offset resets the moved root to 1; descendants keep theirs."
  (let ((ps (dorg-test-level-offset--move (dorg-test-level-offset--ps) 3 5)))
    (should (eql 1 (dorg-heading-level-offset (dorg-test-level-offset--h ps 3))))
    (should (eql 2 (dorg-heading-level-offset (dorg-test-level-offset--h ps 4))))))

(ert-deftest dorg-test-level-offset-move-explicit-offset-honored ()
  "An explicit positive offset is the moved root's new offset."
  (let ((ps (dorg-test-level-offset--move (dorg-test-level-offset--ps) 3 5 :offset 3)))
    (should (eql 3 (dorg-heading-level-offset (dorg-test-level-offset--h ps 3))))
    (should (eql 4 (dorg-heading-level (dorg-test-level-offset--h ps 3))))
    (should (eql 2 (dorg-heading-level-offset (dorg-test-level-offset--h ps 4))))))

(ert-deftest dorg-test-level-offset-effective-level-derived-after-move ()
  "After a move the candidate's levels equal a fresh derivation from offsets."
  (let* ((ps (dorg-test-level-offset--move (dorg-test-level-offset--ps) 3 5))
         (fresh (make-hash-table :test 'eql)))
    (maphash (lambda (id h) (puthash id (dorg-heading-deep-copy h) fresh))
             (dorg-mutate--ps-headings ps))
    (dorg-model-derive-levels fresh 1)
    (maphash (lambda (id h)
               (should (eql (dorg-heading-level h)
                            (dorg-heading-level (gethash id fresh)))))
             (dorg-mutate--ps-headings ps))
    (should (eql 2 (dorg-heading-level (dorg-test-level-offset--h ps 3))))))

(ert-deftest dorg-test-level-offset-candidate-invalidates-after-reparent ()
  "A descendant's level follows its moved ancestor, never a stale value."
  (let ((ps (dorg-test-level-offset--move (dorg-test-level-offset--ps) 3 5)))
    ;; Xc was at level 6 under X at 4; X now sits at 2, so Xc is 2 + 2.
    (should (eql 4 (dorg-heading-level (dorg-test-level-offset--h ps 4))))))

(ert-deftest dorg-test-level-offset-render-after-move ()
  "Rendering the moved candidate emits the new stars for the whole subtree."
  (let* ((ps (dorg-test-level-offset--move (dorg-test-level-offset--ps) 3 5))
         (text (dorg-model-render-tree
                (hash-table-values (dorg-mutate--ps-headings ps)))))
    (should (string-match-p "^\\*\\* h3$" text))
    (should (string-match-p "^\\*\\*\\*\\* h4$" text))))

(ert-deftest dorg-test-level-offset-default-move-before-gapped-sibling-refused ()
  "The default offset 1 before a sibling at offset 2 is refused, not reparented."
  (let ((ps (dorg-test-level-offset--ps)))
    ;; B gets a gapped child G (offset 2) as its first child.
    (puthash 6 (dorg-test-level-offset--heading 6 5 2 1)
             (dorg-mutate--ps-headings ps))
    (let ((err (should-error (dorg-test-level-offset--move ps 3 5 :before '(:heading :id 6))
                             :type 'dorg-mutate-error)))
      ;; A typed refusal naming the rule and the smallest valid offset; the
      ;; batch aborts, so the store is untouched (the MEDIUM twin asserts it).
      (should (eq 'nonrepresentable-sibling-order
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (string-match-p "N >= 2" (cadr err))))))

(ert-deftest dorg-test-level-offset-explicit-offset-before-gapped-sibling-accepted ()
  "An explicit offset >= the gapped successor makes the same placement valid."
  (let ((ps (dorg-test-level-offset--ps)))
    (puthash 6 (dorg-test-level-offset--heading 6 5 2 1)
             (dorg-mutate--ps-headings ps))
    (dorg-test-level-offset--move ps 3 5 :before '(:heading :id 6) :offset 2)
    (should (eql 5 (dorg-heading-parent-id (dorg-test-level-offset--h ps 3))))
    (should (eql 2 (dorg-heading-level-offset (dorg-test-level-offset--h ps 3))))))

(ert-deftest dorg-test-level-offset-sparse-candidate-fan-check-uses-neighbour-offsets ()
  "A sparse candidate checks against an unloaded neighbour's offset from facts.
B's only child G (offset 3) is NOT loaded; the ordering facts report it and
its offset.  Offset 5 at the tail would re-parent under G and is refused;
offset 2 is valid."
  (let* ((g-key (dorg-sortkey-from-integer 1 2))
         (facts (lambda (ps)
                  (lambda (parent-id &optional bound side _limit exclude-ids)
                    ;; Honor the bound like the store read: G answers only
                    ;; on the side of BOUND where its key lies.
                    (when (and (eql parent-id 5) (not (memql 7 exclude-ids))
                               (or (null bound)
                                   (if (eq side 'before)
                                       (string< g-key bound)
                                     (string< bound g-key))))
                      (puthash 7 3 (dorg-mutate--ps-fact-offsets ps))
                      (list (cons 7 g-key))))))
         (ps (dorg-test-level-offset--ps)))
    (setf (dorg-mutate--ps-order-facts-fn ps) (funcall facts ps))
    (should-error (dorg-test-level-offset--move ps 3 5 :offset 5)
                  :type 'dorg-mutate-error)
    (let ((ps2 (dorg-test-level-offset--ps)))
      (setf (dorg-mutate--ps-order-facts-fn ps2) (funcall facts ps2))
      (dorg-test-level-offset--move ps2 3 5 :offset 2)
      (should (eql 2 (dorg-heading-level-offset (dorg-test-level-offset--h ps2 3)))))))

(ert-deftest dorg-test-level-offset-insert-before-gapped-sibling-refused ()
  "An insert is checked like a move: a new heading at offset 1 before a sibling
at offset 2 would re-parent that sibling, so it is refused."
  (let ((ps (dorg-test-level-offset--ps)))
    (puthash 6 (dorg-test-level-offset--heading 6 5 2 1)
             (dorg-mutate--ps-headings ps))
    (let ((err (should-error
                (dorg-mutate--ps-insert-heading
                 ps (list :op 'insert
                          :target '(:tree :under (:heading :id 5) :before (:heading :id 6))
                          :heading '(:id 99 :title "New")))
                :type 'dorg-mutate-error)))
      (should (eq 'nonrepresentable-sibling-order
                  (plist-get (dorg-mutate-error-data err) :detail))))
    ;; The same insert after the gapped sibling is representable.
    (let ((ps2 (dorg-test-level-offset--ps)))
      (puthash 6 (dorg-test-level-offset--heading 6 5 2 1)
               (dorg-mutate--ps-headings ps2))
      (dorg-mutate--ps-insert-heading
       ps2 (list :op 'insert
                 :target '(:tree :under (:heading :id 5) :after (:heading :id 6))
                 :heading '(:id 99 :title "New")))
      (should (eql 1 (dorg-heading-level-offset (dorg-test-level-offset--h ps2 99)))))))

;;; ─── Public clause and IR (no store) ────────────────────────────────────

(ert-deftest dorg-test-level-offset-api-offset-clause-lowers ()
  "The optional (offset N) clause lowers to :to :offset, in any clause order."
  (dolist (tail '(((after (id 9)) (offset 2)) ((offset 2) (after (id 9)))))
    (let ((patch (dorg-api--action->patch
                  '(:heading :id 42)
                  `(move (subtree) (under (id 7) ,@tail))
                  nil)))
      (should (eql 2 (plist-get (plist-get patch :to) :offset)))
      (should (plist-get (plist-get patch :to) :after))))
  (let ((patch (dorg-api--action->patch
                '(:heading :id 42) '(move (subtree) (under (id 7))) nil)))
    (should-not (plist-member (plist-get patch :to) :offset))))

(ert-deftest dorg-test-level-offset-api-offset-rejects-duplicate ()
  "Two offset clauses are refused even when equal."
  (should-error (dorg-api--action->patch
                 '(:heading :id 42)
                 '(move (subtree) (under (id 7) (offset 2) (offset 2))) nil)))

(ert-deftest dorg-test-level-offset-api-offset-rejects-nonpositive ()
  "Only a positive integer offset is accepted."
  (dolist (bad '((offset 0) (offset -1) (offset 1.5) (offset "2") (offset two)
                 (offset) (offset 1 2)))
    (ert-info ((format "%S" bad))
      (should-error (dorg-api--action->patch
                     '(:heading :id 42)
                     `(move (subtree) (under (id 7) ,bad)) nil)))))

(ert-deftest dorg-test-level-offset-ir-validator-accepts-offset ()
  "The typed move validator admits a positive :offset and refuses others."
  (let ((ok (list :op 'move :target '(:subtree :id 42)
                  :to '(:under (:heading :id 7) :offset 2))))
    (should (dorg-mutate--validate-patch ok 0)))
  (dolist (bad '(0 -3 1.5 "2"))
    (should-error (dorg-mutate--validate-patch
                   (list :op 'move :target '(:subtree :id 42)
                         :to (list :under '(:heading :id 7) :offset bad))
                   0))))

(ert-deftest dorg-test-level-offset-ir-tree-offset-internal-only ()
  "A tree slot admits an internal positive :offset; the public form never does.
History restore places a re-created heading at its stored offset through the
typed IR; the public insert form still rejects an offset clause."
  (let ((ok (list :op 'insert :target '(:tree :under (:heading :id 7) :offset 2)
                  :heading '(:title "x"))))
    (should (dorg-mutate--validate-patch ok 0)))
  (dolist (bad '(0 -3 1.5 "2"))
    (ert-info ((format "%S" bad))
      (should-error (dorg-mutate--validate-patch
                     (list :op 'insert
                           :target (list :tree :under '(:heading :id 7) :offset bad)
                           :heading '(:title "x"))
                     0))))
  (should-error (dorg-api--insert-heading->patch
                 '(insert (heading (title "x")) (under (id 7) (offset 2))) nil)))

(ert-deftest dorg-test-level-offset-derive-levels-malformed-root-no-levels ()
  "A named root without the root's shape derives no level for anyone.
A well-formed root (no parent, offset 0) gives its child level 1; a root
with offset 3, or a root with a parent, leaves every level nil, as the
sliced loader does, instead of seeding 0 and fabricating the tree."
  (cl-flet ((tree (root-parent root-offset)
              (let ((ht (make-hash-table :test 'eql)))
                (puthash 1 (make-dorg-heading :id 1 :parent-id root-parent
                                              :level-offset root-offset)
                         ht)
                (puthash 2 (make-dorg-heading :id 2 :parent-id 1 :level-offset 1) ht)
                (puthash 3 (make-dorg-heading :id 3 :parent-id 2 :level-offset 2) ht)
                (dorg-model-derive-levels ht 1))))
    (let ((ok (tree nil 0)))
      (should (equal '(0 1 3) (mapcar (lambda (id) (dorg-heading-level (gethash id ok)))
                                      '(1 2 3)))))
    (dolist (shape '((nil 3) (2 0)))
      (ert-info ((format "root parent %S offset %S" (car shape) (cadr shape)))
        (let ((bad (tree (car shape) (cadr shape))))
          (should-not (cl-some (lambda (id) (dorg-heading-level (gethash id bad)))
                               '(1 2 3))))))))

(ert-deftest dorg-test-level-offset-offset-level-is-the-one-rule ()
  "A level exists only for a known base and a positive integer offset."
  (should (equal 3 (dorg-model-offset-level 1 2)))
  (dolist (bad '((nil 1) (1 0) (1 -1) (1 nil) (1 1.5)))
    (ert-info ((format "%S" bad))
      (should-not (apply #'dorg-model-offset-level bad)))))

(ert-deftest dorg-test-level-offset-derive-closure-fails-closed ()
  "A candidate closure derives like a whole load and refuses an invalid offset.
A valid chain under a level-2 parent derives 3 and 5; with no known base
every level is nil; a zero or negative descendant offset signals rather
than leaving a level the rest of the closure would build on."
  (let* ((a (make-dorg-heading :id 2 :parent-id 1 :level-offset 1 :level 99))
         (b (make-dorg-heading :id 3 :parent-id 2 :level-offset 2 :level 99))
         (kids (lambda (h) (if (eql (dorg-heading-id h) 2) (list b) nil))))
    (should (equal '(2 3) (mapcar #'dorg-heading-id
                                  (dorg-model-derive-closure a 2 kids))))
    (should (equal '(3 5) (list (dorg-heading-level a) (dorg-heading-level b))))
    (dorg-model-derive-closure a nil kids)
    (should (equal '(nil nil) (list (dorg-heading-level a) (dorg-heading-level b))))
    (dolist (bad '(0 -1 nil))
      (ert-info ((format "%S" bad))
        (setf (dorg-heading-level-offset b) bad)
        (should (equal (list 'dorg-model-invalid-offset 3 bad)
                       (should-error (dorg-model-derive-closure a 2 kids)
                                     :type 'dorg-model-invalid-offset)))))))

(ert-deftest dorg-test-level-offset-api-move-unknown-clause-diagnosed ()
  "An unknown move clause is refused as unknown, not as a malformed anchor."
  (let ((err (should-error (dorg-api--action->patch
                            '(:heading :id 42)
                            '(move (subtree) (under (id 7) (bogus 2))) nil))))
    (should (string-match-p "unknown move clause" (error-message-string err)))
    (should-not (string-match-p "anchor" (error-message-string err))))
  ;; Known clauses in any order still lower.
  (should (dorg-api--action->patch
           '(:heading :id 42)
           '(move (subtree) (under (id 7) (offset 2) (after (id 9)))) nil)))

(ert-deftest dorg-test-level-offset-grammar-template-shows-offset ()
  "The public move template advertises the optional OFFSET clause."
  (let ((move (cl-find 'move dorg-mutate-grammar-productions
                       :key (lambda (p) (plist-get p :id)))))
    (should (string-match-p "\\[OFFSET\\]" (plist-get move :template)))))

;;; ─── Index classification and lineage (no store) ───────────────────────

(ert-deftest dorg-test-level-offset-classify-moved-with-unchanged-tuple ()
  "An explicitly MOVED id is a move even when its persisted tuple is unchanged.
Create and delete still win over MOVED, and partitions stay disjoint."
  (let ((orig (make-hash-table :test 'eql))
        (cand (make-hash-table :test 'eql)))
    (dolist (id '(1 2 3))
      (puthash id (dorg-test-level-offset--heading id 1 1 id 1) orig))
    (dolist (id '(1 2 4))
      (puthash id (dorg-test-level-offset--heading id 1 1 id 1) cand))
    (let* ((parts (dorg-ext--index-classify orig cand nil nil nil '(2 3 4)))
           (ids (lambda (k) (mapcar #'dorg-ext-index-entry-id (plist-get parts k)))))
      (should (equal (funcall ids :moved) '(2)))
      (should (equal (funcall ids :created) '(4)))
      (should (equal (funcall ids :deleted) '(3)))
      (should-not (funcall ids :changed)))))

(ert-deftest dorg-test-level-offset-lineage-compares-offset-not-effective ()
  "Lineage is parent, sort key and OFFSET; a derived level change alone is not."
  (let ((a (dorg-test-level-offset--heading 3 2 2 1 5))
        (b (dorg-test-level-offset--heading 3 2 2 1 9)))
    (should-not (dorg-ext--index-lineage-changed-p a b))
    (setf (dorg-heading-level-offset b) 1)
    (should (dorg-ext--index-lineage-changed-p a b))))

(ert-deftest dorg-test-level-offset-canonical-tuple-carries-offset ()
  "The canonical persisted tuple carries the offset, never the derived level."
  (let ((a (dorg-test-level-offset--heading 3 2 2 1 5))
        (b (dorg-test-level-offset--heading 3 2 2 1 9)))
    (should (equal (dorg-ext--index-heading-canonical a)
                   (dorg-ext--index-heading-canonical b)))
    (setf (dorg-heading-level-offset b) 1)
    (should-not (equal (dorg-ext--index-heading-canonical a)
                       (dorg-ext--index-heading-canonical b)))))

(ert-deftest dorg-test-level-offset-ql-model-keyword-host-by-offset ()
  "Keyword predicates find the root by its offset, not by a derived level.
Heading 2 comes from an offsets-mode load, so its level is nil; it is still
a keyword host.  The root, offset 0 with no parent, never is."
  (let* ((kw (lambda () (list (dorg-keyword-item :keyword "CATEGORY" :value "x"))))
         (headings
          (list (make-dorg-heading :id 1 :parent-id nil :level-offset 0 :items (funcall kw))
                (make-dorg-heading :id 2 :parent-id 1 :level-offset 1 :title "Two"
                                   :items (funcall kw)))))
    (should (equal '(2)
                   (mapcar (lambda (r) (plist-get r :heading-id))
                           (dorg-ql-select '(keyword "CATEGORY" "x") headings))))))

(provide 'dorg-level-offset-tests)
;;; dorg-level-offset-tests.el ends here
