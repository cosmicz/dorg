;;; dorg-ql-eval-tests.el --- Tests for the dorg-ql model evaluator -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-ql--model-eval': the in-memory evaluator over the
;; normalized v2 AST (`dorg-ql-normalize') against a tree of
;; `dorg-heading' structs.  Covers boolean composition, field/property/
;; tag/id/timestamp predicates, Org priority ordering, the pipe/where/
;; unique combinators, and the self/parent/children/ancestors/descendants/
;; siblings traversal relations (order + de-duplication).  The evaluator is
;; pure structure over model objects, so these tests need no org buffer or
;; Dolt backend.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-ql-ast)
(require 'dorg-ql-eval)
(require 'dorg-ql-model)

;;; ─── Fixtures ──────────────────────────────────────────────────────────

(defun dorg-test-ql-eval--props (alist)
  "Wrap property ALIST in a PROPERTIES `dorg-drawer-item' list, or nil."
  (when alist
    (list (dorg-drawer-item :name "PROPERTIES" :properties alist))))

(cl-defun dorg-test-ql-eval--h
    (id parent-id level &key title todo priority commented tags props items)
  "Build a `dorg-heading' with the given fields.
PROPS is a property alist promoted to a PROPERTIES drawer; ITEMS, when
given, supplies the full item list instead (e.g. planning/text items)."
  (make-dorg-heading
   :id id :parent-id parent-id :level level
   :title title :todo todo :priority priority :commented commented
   :tags tags
   :items (or items (dorg-test-ql-eval--props props))))

(defun dorg-test-ql-eval--tree ()
  "Return a document-order heading list for traversal/predicate tests.

  root (0)
  * A           (1)  [#A] :work:        DEADLINE <2026-06-03 Wed>
  ** A1         (2)  [#B] TODO          FOO=bar, #+EXPORT_FILE_NAME: alpha-one
  ** A2         (3)  [#C] DONE COMMENT
  * B           (4)       :home:
  ** B1         (5)  [#A]
  *** B1a       (6)"
  (list
   (dorg-test-ql-eval--h
    0 nil 0 :title "root"
    :items (list (dorg-keyword-item :keyword "EXPORT_FILE_NAME"
                                    :value "root-file")))
   (dorg-test-ql-eval--h
    1 0 1 :title "Alpha" :priority "A" :tags '("work")
    :items (list (dorg-planning-item :deadline "<2026-06-03 Wed>")))
   (dorg-test-ql-eval--h 2 1 2 :title "Alpha One" :priority "B" :todo "TODO"
                         :items (append
                                 (dorg-test-ql-eval--props '(("FOO" . "bar")))
                                 (list
                                  (dorg-keyword-item
                                   :keyword "EXPORT_FILE_NAME"
                                   :value "alpha-one")
                                  (dorg-keyword-item
                                   :keyword "ALIAS"
                                   :value "alpha-alias"))))
   (dorg-test-ql-eval--h 3 1 2 :title "Alpha Two" :priority "C" :todo "DONE"
                         :commented t)
   (dorg-test-ql-eval--h 4 0 1 :title "Beta" :tags '("home"))
   (dorg-test-ql-eval--h 5 4 2 :title "Beta One" :priority "A")
   (dorg-test-ql-eval--h 6 5 3 :title "Beta One a")))

(defun dorg-test-ql-eval--ids (query headings)
  "Evaluate QUERY (a public reader form) against HEADINGS; return id list."
  (mapcar (lambda (r) (plist-get r :heading-id))
          (dorg-ql--model-eval (dorg-ql-normalize query) headings)))

;;; ─── Envelope & result shape ───────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-result-shape ()
  "Evaluator returns the canonical four-key markerless plist."
  (let* ((headings (dorg-test-ql-eval--tree))
         (r (car (dorg-ql--model-eval (dorg-ql-normalize '(id 2)) headings))))
    (should (equal '(:heading-id :marker :title :properties)
                   (cl-loop for (k _v) on r by #'cddr collect k)))
    (should (= 2 (plist-get r :heading-id)))
    (should (equal "Alpha One" (plist-get r :title)))
    (should (null (plist-get r :marker)))
    (should (equal '(("FOO" . "bar")) (plist-get r :properties)))))

(ert-deftest dorg-test-ql-eval-source-all-preserves-input-order ()
  "A bare source pipe over :all returns every heading in input order."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(0 1 2 3 4 5 6)
                   (dorg-test-ql-eval--ids '(pipe (where (>= (field level) 0)))
                                           headings)))))

(ert-deftest dorg-test-ql-eval-non-pipe-ast-errors ()
  "A non-pipe AST is rejected rather than silently widening."
  (should-error (dorg-ql--model-eval '(:filter (:id = 1)) nil)
                :type 'dorg-ql-error))

;;; ─── Field predicates ──────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-field-id-identity ()
  "The id predicate owns DORG_ID; a property lookup for it misses."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(id 2) headings)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(field id 2) headings)))
    ;; Model properties never carry DORG_ID.
    (should (null (dorg-test-ql-eval--ids '(property "DORG_ID" "2") headings)))))

(ert-deftest dorg-test-ql-eval-field-title-string-ops ()
  "Title supports =, contains, prefix, suffix, and !=."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(1) (dorg-test-ql-eval--ids '(field title "Alpha")
                                                headings)))
    (should (equal '(1 2 3)
                   (dorg-test-ql-eval--ids '(prefix (field title) "Alpha")
                                           headings)))
    ;; "Alpha", "Beta", and "Beta One a" all end in "a".
    (should (equal '(1 4 6)
                   (dorg-test-ql-eval--ids '(suffix (field title) "a")
                                           headings)))
    ;; Only the "* One" titles contain "One"; "Alpha Two" does not.
    (should (equal '(2 5 6)
                   (dorg-test-ql-eval--ids '(contains (field title) "One")
                                           headings)))))

(ert-deftest dorg-test-ql-eval-field-todo-nil-never-matches ()
  "A heading with no TODO keyword matches no todo comparison operator."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(field todo "TODO")
                                                headings)))
    ;; Only the two headings that HAVE a todo keyword can satisfy !=.
    (should (equal '(2) (dorg-test-ql-eval--ids '(!= (field todo) "DONE")
                                                headings)))))

(ert-deftest dorg-test-ql-eval-field-level-and-parent-id ()
  "Numeric fields level and parent-id compare with ordered operators."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2 3 5)
                   (dorg-test-ql-eval--ids '(field level 2) headings)))
    (should (equal '(0 1 4)
                   (dorg-test-ql-eval--ids '(< (field level) 2) headings)))
    ;; Children of heading 1 (root id 0 has nil parent and never matches).
    (should (equal '(2 3)
                   (dorg-test-ql-eval--ids '(field parent-id 1) headings)))))

(ert-deftest dorg-test-ql-eval-field-commented-boolean ()
  "Commented is a real boolean: = nil selects the non-commented headings."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(3) (dorg-test-ql-eval--ids '(field commented t)
                                                headings)))
    (should (equal '(0 1 2 4 5 6)
                   (dorg-test-ql-eval--ids '(field commented nil)
                                           headings)))))

;;; ─── Priority ordering ─────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-priority-org-order ()
  "Priority orders by Org rank (A highest), not raw string comparison."
  (let ((headings (dorg-test-ql-eval--tree)))
    ;; :at-or-above B keeps A and B, drops C; nil-priority headings drop.
    (should (equal '(1 2 5)
                   (dorg-test-ql-eval--ids '(>= (field priority) "B")
                                           headings)))
    ;; :below B keeps only C.
    (should (equal '(3) (dorg-test-ql-eval--ids '(< (field priority) "B")
                                                headings)))
    ;; `>' means higher priority than B -> only A-priority headings.
    (should (equal '(1 5) (dorg-test-ql-eval--ids '(> (field priority) "B")
                                                  headings)))
    ;; Raw string order would have made "C" > "B"; Org order must not.
    (should (equal '(1 2 5) (dorg-test-ql-eval--ids '(>= (field priority) "B")
                                                    headings)))
    ;; `=' stays exact.
    (should (equal '(1 5) (dorg-test-ql-eval--ids '(field priority "A")
                                                  headings)))))

;;; ─── Property predicate ────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-property-ops ()
  "Property exists/missing/=/contains require presence except missing."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(property "FOO" "bar")
                                                headings)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(exists (property "FOO"))
                                                headings)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(contains (property "FOO") "a")
                                                headings)))
    ;; A comparison operator never matches an absent key.
    (should (null (dorg-test-ql-eval--ids '(!= (property "FOO") "bar")
                                          headings)))
    ;; Only `missing' matches absence: every heading but #2 lacks FOO.
    (should (equal '(0 1 3 4 5 6)
                   (dorg-test-ql-eval--ids '(missing (property "FOO"))
                                           headings)))))

;;; ─── Keyword predicate ─────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-keyword-heading-scoped ()
  "Keyword predicates match heading-owned keyword items, not the root host."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids '(keyword "EXPORT_FILE_NAME")
                                           headings)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids
                    '(keyword "EXPORT_FILE_NAME" "alpha-one") headings)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids
                    '(contains (keyword "EXPORT_FILE_NAME") "alpha") headings)))
    ;; The root owns EXPORT_FILE_NAME=root-file, but keyword predicates are
    ;; heading-scoped and must not select the level-0 root host.
    (should (null (dorg-test-ql-eval--ids
                   '(keyword "EXPORT_FILE_NAME" "root-file") headings)))
    (should (equal '(1 3 4 5 6)
                   (dorg-test-ql-eval--ids
                    '(missing (keyword "EXPORT_FILE_NAME")) headings)))))

;;; ─── Tag predicate ─────────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-tag-set-semantics ()
  "Tag is set membership; != matches the empty set, exists/missing test it."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(1) (dorg-test-ql-eval--ids '(tag "work") headings)))
    (should (equal '(1 4) (dorg-test-ql-eval--ids '(exists (tag)) headings)))
    ;; != work matches every heading that is not tagged work, including the
    ;; untagged ones (empty set is "not a member").
    (should (equal '(0 2 3 4 5 6)
                   (dorg-test-ql-eval--ids '(!= (tag) "work") headings)))
    (should (equal '(0 2 3 5 6)
                   (dorg-test-ql-eval--ids '(missing (tag)) headings)))
    (should (equal '(1) (dorg-test-ql-eval--ids '(prefix (tag) "wor")
                                                headings)))))

;;; ─── Boolean combinators ───────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-boolean-and-or-not ()
  "and/or/not compose predicates over the candidate set."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids
                    '(and (field level 2) (>= (field priority) "B")
                          (exists (property "FOO")))
                    headings)))
    (should (equal '(1 4)
                   (dorg-test-ql-eval--ids '(or (tag "work") (tag "home"))
                                           headings)))
    ;; ids 1 and 5 carry priority A; everything else (incl. nil-priority)
    ;; survives the negation.
    (should (equal '(0 2 3 4 6)
                   (dorg-test-ql-eval--ids '(not (field priority "A"))
                                           headings)))))

;;; ─── Pipe / where / unique ─────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-pipe-interleaved-where ()
  "A pipe applies interleaved where filters in order."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(5)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (field priority "A"))
                           (where (field level 2)))
                    headings)))))

(ert-deftest dorg-test-ql-eval-unique-dedup-preserves-order ()
  "A unique step de-duplicates by id while preserving order."
  (let ((headings (dorg-test-ql-eval--tree)))
    ;; children of {A1,A2} are none; children of {A} are {A1,A2}.  Two
    ;; traverses to parent from {A1,A2} both yield A -> unique-by-id anyway,
    ;; but an explicit (unique) after a filter must be a no-op on a clean set.
    (should (equal '(1 4)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (field level 1)) (unique))
                    headings)))))

;;; ─── (one QUERY) cardinality assertion ─────────────────────────────────
;; AC-2/AC-3 (tracked-case): the terminal (:one) step asserts the working set
;; holds exactly one heading, else `dorg-ql-error' (a DATA cardinality fact).

(ert-deftest dorg-test-ql-eval-one-exactly-one-passes ()
  "(one PRED) with a single match returns that heading."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2) (dorg-test-ql-eval--ids '(one (id 2)) headings)))))

(ert-deftest dorg-test-ql-eval-one-zero-matches-errors ()
  "(one PRED) matching nothing signals a cardinality error."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should-error (dorg-test-ql-eval--ids '(one (id 999)) headings)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-eval-one-multiple-matches-errors ()
  "(one PRED) matching >1 signals a cardinality error.
Non-vacuity: (field priority \"A\") matches ids 1 and 5; without the (:one)
assertion this would return two ids, so the should-error binds to the guard."
  (let ((headings (dorg-test-ql-eval--tree)))
    ;; the bare inner query really does return two, proving the guard is live:
    (should (equal '(1 5)
                   (dorg-test-ql-eval--ids '(field priority "A") headings)))
    (should-error (dorg-test-ql-eval--ids '(one (field priority "A")) headings)
                  :type 'dorg-ql-error)))

;;; ─── Traversal ─────────────────────────────────────────────────────────

(ert-deftest dorg-test-ql-eval-traverse-self-and-parent ()
  "self is identity; parent resolves via parent-id and drops the rootless."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 2)) (traverse self)) headings)))
    (should (equal '(1)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 2)) (traverse parent)) headings)))
    ;; Parent of a level-1 heading is the level-0 root (id 0), present here.
    (should (equal '(0)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 1)) (traverse parent)) headings)))))

(ert-deftest dorg-test-ql-eval-traverse-children-order ()
  "children returns direct children in input/tree order, deduped."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(2 3)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 1)) (traverse children)) headings)))
    ;; children of {A,B} union, ordered by input position and deduped.
    (should (equal '(2 3 5)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (field level 1)) (traverse children))
                    headings)))))

(ert-deftest dorg-test-ql-eval-traverse-ancestors-descendants ()
  "ancestors walks up to the root; descendants collects the whole subtree."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(0 4 5)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 6)) (traverse ancestors)) headings)))
    (should (equal '(2 3)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 1)) (traverse descendants)) headings)))
    (should (equal '(5 6)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 4)) (traverse descendants)) headings)))))

(ert-deftest dorg-test-ql-eval-traverse-siblings ()
  "siblings returns same-parent peers excluding self, even under the root."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(3)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 2)) (traverse siblings)) headings)))
    ;; A and B share the level-0 root as parent.
    (should (equal '(4)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 1)) (traverse siblings)) headings)))))

(ert-deftest dorg-test-ql-eval-traverse-union-self-descendants ()
  "A multi-relation traverse unions the relations (self + descendants)."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(4 5 6)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (id 4)) (traverse self descendants))
                    headings)))))

(ert-deftest dorg-test-ql-eval-traverse-dedup-shared-parent ()
  "Traversing to a shared parent de-duplicates the parent heading."
  (let ((headings (dorg-test-ql-eval--tree)))
    ;; A1 and A2 both have parent A: parent traversal must yield A once.
    (should (equal '(1)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (field parent-id 1)) (traverse parent))
                    headings)))))

;;; ─── Timestamp predicate over the model ────────────────────────────────

(ert-deftest dorg-test-ql-eval-timestamp-deadline-window ()
  "A timestamp filter windows the heading's DEADLINE projection."
  (let ((headings (dorg-test-ql-eval--tree)))
    (should (equal '(1)
                   (dorg-test-ql-eval--ids
                    '(deadline "2026-06-01" "2026-06-30") headings)))
    (should (null (dorg-test-ql-eval--ids
                   '(deadline "2026-07-01" "2026-07-31") headings)))))

;;; ─── Cross-semantic compositions ───────────────────────────────────────

(ert-deftest dorg-test-ql-eval-high-priority-children-of-due-this-week ()
  "High-priority children of headings due this week compose cleanly.
Alpha (id 1) is due 2026-06-03; with the reference week anchored on that
date its only :at-or-above B child is Alpha One (id 2)."
  (let ((headings (dorg-test-ql-eval--tree))
        ;; 2026-06-03 is a Wednesday; anchor the relative week there.
        (dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
    (should (equal '(2)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (deadline (this-week)))
                           (traverse children)
                           (where (>= (field priority) "B")))
                    headings)))))

(ert-deftest dorg-test-ql-eval-ancestors-of-due-this-week ()
  "Ancestors of headings due this week are the chain up to the root."
  (let ((headings (dorg-test-ql-eval--tree))
        (dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
    ;; Only Alpha (id 1) is due this week; its ancestor is the root (id 0).
    (should (equal '(0)
                   (dorg-test-ql-eval--ids
                    '(pipe (where (deadline (this-week)))
                           (traverse ancestors))
                    headings)))))

;;; ─── Neutral evaluation core (dorg-ql-eval.el) ─────────────────────────

;; The tests below pin the extracted, backend-neutral primitives in
;; `dorg-ql-eval.el' DIRECTLY --- boolean AST recursion, the pipeline-step
;; driver, the AST filter walkers/collectors, the shared alist matcher, the
;; scalar operators, and the tree-index/traversal helpers --- independently
;; of any adapter, so the shared substrate cannot regress silently.

(ert-deftest dorg-test-ql-eval-core-expr-boolean-and-leaf-routing ()
  "`dorg-ql--eval-expr-match-p' composes :and/:or/:not and routes leaves.
Every non-boolean node is delegated to LEAF-FN with the heading forwarded;
:or returns t (not the raw leaf value)."
  (let* ((seen nil)
         (heading 'the-heading)
         (leaf (lambda (node h)
                 (should (eq h heading))
                 (push node seen)
                 (eq (car node) :yes))))
    ;; :and requires every leaf true.
    (should (dorg-ql--eval-expr-match-p '(:and (:yes) (:yes)) heading leaf))
    (should-not (dorg-ql--eval-expr-match-p '(:and (:yes) (:no)) heading leaf))
    ;; :or requires any, and normalizes to t.
    (should (eq t (dorg-ql--eval-expr-match-p '(:or (:no) (:yes)) heading leaf)))
    (should-not (dorg-ql--eval-expr-match-p '(:or (:no) (:no)) heading leaf))
    ;; :not inverts.
    (should (dorg-ql--eval-expr-match-p '(:not (:no)) heading leaf))
    (should-not (dorg-ql--eval-expr-match-p '(:not (:yes)) heading leaf))
    ;; Bare leaf is delegated directly.
    (should (dorg-ql--eval-expr-match-p '(:yes) heading leaf))
    ;; Both leaves of the failing :and actually reached LEAF-FN.
    (setq seen nil)
    (dorg-ql--eval-expr-match-p '(:and (:yes) (:no)) heading leaf)
    (should (member '(:yes) seen))
    (should (member '(:no) seen))))

(ert-deftest dorg-test-ql-eval-core-expr-short-circuits ()
  "`dorg-ql--eval-expr-match-p' short-circuits :and on false and :or on true.
An exploding leaf placed *after* the deciding leaf must never be reached:
the leaf-fn errors on `:boom', so surviving the call proves the walk stopped.
The visited log pins that exactly the deciding leaf --- and nothing past it ---
was consulted."
  (let* ((visited nil)
         (leaf (lambda (node _h)
                 (push (car node) visited)
                 (pcase (car node)
                   (:t t)
                   (:f nil)
                   (:boom (error "leaf evaluated past short-circuit"))))))
    ;; :and — a leading false decides the result and skips the exploding leaf.
    (setq visited nil)
    (should-not (dorg-ql--eval-expr-match-p '(:and (:f) (:boom)) 'h leaf))
    (should (equal '(:f) visited))
    ;; :or — a leading true decides the result and skips the exploding leaf.
    (setq visited nil)
    (should (eq t (dorg-ql--eval-expr-match-p '(:or (:t) (:boom)) 'h leaf)))
    (should (equal '(:t) visited))))

(ert-deftest dorg-test-ql-eval-core-step-filter-honors-filter-fn ()
  "`dorg-ql--eval-step' :filter applies the supplied FILTER-FN, not the expr.
Non-vacuity: the unfiltered input has all seven headings, so asserting the
two level-1 ids proves the step consulted FILTER-FN."
  (let* ((hs (dorg-test-ql-eval--tree))
         (idx (dorg-ql--build-tree-index hs))
         (level1 (lambda (_expr h) (= (dorg-heading-level h) 1))))
    (should (= 7 (length hs)))
    (should (equal '(1 4)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--eval-step
                            '(:filter (:ignored)) hs idx level1))))))

(ert-deftest dorg-test-ql-eval-core-step-traverse-unique-one ()
  "`dorg-ql--eval-step' handles :traverse/:unique/:one and rejects unknowns."
  (let* ((hs (dorg-test-ql-eval--tree))
         (idx (dorg-ql--build-tree-index hs))
         (alpha (nth 1 hs)))
    ;; :traverse children of Alpha => A1,A2.
    (should (equal '(2 3)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--eval-step
                            '(:traverse children) (list alpha) idx nil))))
    ;; :unique collapses a duplicate, preserving order.
    (should (equal '(1 2)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--eval-step
                            '(:unique) (list alpha (nth 2 hs) alpha) idx nil))))
    ;; :one passes a singleton, errors otherwise.
    (should (dorg-ql--eval-step '(:one) (list alpha) idx nil))
    (should-error (dorg-ql--eval-step '(:one) hs idx nil)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql--eval-step '(:bogus) hs idx nil)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-eval-core-pipeline-runs-steps-in-order ()
  "`dorg-ql--eval-pipeline' threads WORKING through each step left to right.
Filter to Alpha, then take its children: order-sensitive transformation."
  (let* ((hs (dorg-test-ql-eval--tree))
         (idx (dorg-ql--build-tree-index hs))
         (is-alpha (lambda (_e h) (equal (dorg-heading-title h) "Alpha"))))
    (should (equal '(2 3)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--eval-pipeline
                            '((:filter (:x)) (:traverse children))
                            hs idx is-alpha))))
    ;; Empty step list is the identity.
    (should (eq hs (dorg-ql--eval-pipeline nil hs idx is-alpha)))))

(ert-deftest dorg-test-ql-eval-core-alist-match ()
  "`dorg-ql--alist-match-p' unifies exists/missing/string-op alist matching."
  (let ((al '(("K" . "hello") ("E" . ""))))
    (should (dorg-ql--alist-match-p "K" 'exists nil al))
    (should-not (dorg-ql--alist-match-p "X" 'exists nil al))
    (should (dorg-ql--alist-match-p "X" 'missing nil al))
    (should-not (dorg-ql--alist-match-p "K" 'missing nil al))
    (should (dorg-ql--alist-match-p "K" '= "hello" al))
    (should-not (dorg-ql--alist-match-p "K" '= "bye" al))
    (should (dorg-ql--alist-match-p "K" '!= "bye" al))
    (should-not (dorg-ql--alist-match-p "K" '!= "hello" al))
    (should (dorg-ql--alist-match-p "K" 'contains "ell" al))
    (should (dorg-ql--alist-match-p "K" 'prefix "he" al))
    (should (dorg-ql--alist-match-p "K" 'suffix "lo" al))
    ;; An absent key never satisfies a comparison operator.
    (should-not (dorg-ql--alist-match-p "X" '= "hello" al))))

(ert-deftest dorg-test-ql-eval-core-collect-and-probe-nodes ()
  "`collect-nodes'/`any-node-p'/`map-filters' walk top-level :filter exprs.
`collect-nodes' collects every node whose car is HEAD, deduped by `equal'
over the WHOLE node (not by car): two DISTINCT :timestamp nodes sharing the
`:timestamp' head both survive, while a repeated `equal' node collapses to
one --- pinning the plain/Dolt timestamp-cache-building behavior.
`map-filters' visits only the `:filter' step exprs; the interposed
`:traverse' step must never reach FN."
  (let* ((ts-deadline '(:timestamp deadline nil (a . b)))
         (ts-scheduled '(:timestamp scheduled nil (c . d)))
         (kw '(:keyword "K" = "v"))
         (filter1 (list :and ts-deadline (list :not kw)))
         ;; ts-scheduled is distinct from ts-deadline (same head, different
         ;; body); the second ts-deadline is `equal' to the one in filter1.
         (filter2 (list :or ts-scheduled ts-deadline))
         (ast (list :pipe '(:source :all)
                    (list :filter filter1)
                    '(:traverse children)   ; non-filter step: never visited
                    (list :filter filter2))))
    ;; Both distinct :timestamp nodes survive; the equal repeat is deduped.
    (should (equal (list ts-deadline ts-scheduled)
                   (dorg-ql--collect-nodes ast :timestamp)))
    (should (equal (list kw)
                   (dorg-ql--collect-nodes ast :keyword)))
    (should (null (dorg-ql--collect-nodes ast :property)))
    (should (dorg-ql--any-node-p ast :timestamp))
    (should (dorg-ql--any-node-p ast :keyword))
    (should-not (dorg-ql--any-node-p ast :property))
    ;; map-filters receives exactly the two :filter exprs (order preserved),
    ;; and NOT the interposed (:traverse children) step.
    (let (visited)
      (dorg-ql--map-filters ast (lambda (e) (push e visited)))
      (should (equal (list filter1 filter2) (nreverse visited))))))

(ert-deftest dorg-test-ql-eval-core-scalar-operators ()
  "Ordered/string operators and priority rank behave and reject unknowns."
  (should (dorg-ql--apply-order-op '< 1 2))
  (should-not (dorg-ql--apply-order-op '< 2 1))
  (should (dorg-ql--apply-order-op '>= 2 2))
  (should (dorg-ql--apply-order-op '!= 1 2))
  (should-error (dorg-ql--apply-order-op 'bogus 1 2) :type 'dorg-ql-error)
  (should (dorg-ql--apply-string-op '= "a" "a"))
  (should (dorg-ql--apply-string-op 'contains "hello" "ell"))
  (should (dorg-ql--apply-string-op 'prefix "hello" "he"))
  (should (dorg-ql--apply-string-op 'suffix "hello" "lo"))
  (should-not (dorg-ql--apply-string-op '= "a" "b"))
  (should-error (dorg-ql--apply-string-op 'bogus "a" "b") :type 'dorg-ql-error)
  ;; Org priority: A outranks B; nil/empty has no rank.
  (should (> (dorg-ql--priority-rank "A") (dorg-ql--priority-rank "B")))
  (should-not (dorg-ql--priority-rank nil))
  (should-not (dorg-ql--priority-rank "")))

(ert-deftest dorg-test-ql-eval-core-order-dedup-corrects-order ()
  "`dorg-ql--order-dedup' restores input order and collapses duplicates.
Transformation test: the scrambled, duplicated input is NOT already the
corrected order, so deleting the sort/dedup would fail the assertion."
  (let* ((hs (dorg-test-ql-eval--tree))
         (idx (dorg-ql--build-tree-index hs))
         (scrambled (list (nth 4 hs) (nth 1 hs) (nth 4 hs) (nth 2 hs)))
         (fixed (dorg-ql--order-dedup scrambled idx)))
    (should (equal '(1 2 4) (mapcar #'dorg-heading-id fixed)))
    (should-not (equal '(1 2 4) (mapcar #'dorg-heading-id scrambled)))))

(ert-deftest dorg-test-ql-eval-core-relate-and-traverse-step ()
  "`dorg-ql--relate' and `dorg-ql--traverse-step' resolve relations via IDX."
  (let* ((hs (dorg-test-ql-eval--tree))
         (idx (dorg-ql--build-tree-index hs))
         (alpha (nth 1 hs)))
    (should (equal '(2 3)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--relate 'descendants alpha idx))))
    (should (equal (list (nth 0 hs))
                   (dorg-ql--relate 'parent alpha idx)))
    ;; Union of self+children over {Alpha}, ordered by input position.
    (should (equal '(1 2 3)
                   (mapcar #'dorg-heading-id
                           (dorg-ql--traverse-step
                            '(self children) (list alpha) idx))))))

(ert-deftest dorg-test-ql-eval-core-unique-preserves-order ()
  "`dorg-ql--unique' de-duplicates by id, keeping first-seen order."
  (let* ((hs (dorg-test-ql-eval--tree))
         (dup (list (nth 1 hs) (nth 2 hs) (nth 1 hs))))
    (should (equal '(1 2) (mapcar #'dorg-heading-id (dorg-ql--unique dup))))))

(provide 'dorg-ql-eval-tests)
;;; dorg-ql-eval-tests.el ends here
