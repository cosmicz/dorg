;;; dorg-ql-model.el --- Model-list adapter for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The model adapter answers a normalized query against a list of
;; `dorg-heading' structs, returning markerless result plists in list
;; order.  It is the SEMANTIC BASELINE for every predicate — including the
;; timestamp-window families: the timestamp accessors in `dorg-model'
;; (planning, active body, LOGBOOK, and property timestamps) are pure
;; projections over a heading's parsed items, and the plain-org and Dolt
;; adapters reuse the shared leaf matchers here
;; (`dorg-ql--timestamp-pred-match-p', `dorg-ql--keyword-pred-match-p') so
;; the three converge by construction rather than by maintaining parallel
;; scanners.
;;
;; This module owns the in-memory evaluator for the normalized v2 AST
;; produced by `dorg-ql-normalize' (`dorg-ql-ast').  `dorg-ql--model-eval'
;; runs a whole `(:pipe (:source :all) STEP...)' pipeline of
;; filter/traverse/unique steps over a tree of `dorg-heading' structs.  It
;; is the REFERENCE SEMANTICS slice: the plain-org and Dolt adapters will
;; later lower the same AST, so the choices made here (boolean composition,
;; field/property/keyword/tag/id/timestamp matching, Org priority ordering, and
;; traversal order/de-duplication) define what those adapters must
;; reproduce.  The evaluator never re-parses reader forms and never widens
;; a malformed node to match-all: it consumes the validated AST only.
;;
;; Predicate semantics (model reference):
;;
;; - field `priority' orders by Org priority (`A' highest), not raw string
;;   order: ordered operators rank by the negated priority character, while
;;   `='/`!=' stay exact-string.  A
;;   heading with no priority never matches an ordering/string operator.
;; - the other comparison fields treat a nil/absent value as a non-match
;;   for every operator; `commented' is a real boolean (t or nil).
;; - a `property' comparison operator requires the key to be PRESENT;
;;   only `missing' matches an absent key and `exists' a present one.
;; - a `keyword' predicate is heading-scoped: it inspects `dorg-keyword-item'
;;   objects owned by real headings only.  The level-0 root host never
;;   matches, including `missing'.
;; - a `tag' predicate is set membership: `=' is member, `!=' is
;;   non-member (so an empty tag set satisfies `!='), `contains'/`prefix'/
;;   `suffix' match ANY tag, and `exists'/`missing' test tag-set emptiness.
;;
;; Traversal semantics: a `(:traverse REL...)' step takes the UNION of each
;; relation applied to the working set (so `self' is meaningful, e.g.
;; `(traverse self descendants)' is the whole subtree); chain hops with
;; separate steps.  Every traversal de-duplicates by heading id and orders
;; results by their position in the input heading list (input/tree order).
;; The child index is keyed by parent-id, so `children'/`siblings' work
;; even when the level-0 root is absent from the input list.  Public
;; model-list v2 sources are ordered forests: filter-only pipelines run over
;; the supplied list as-is, while traversal and `unique' require unique,
;; non-nil heading ids.

;;; Code:

(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-ql-core)
(require 'dorg-ql-eval)

(defun dorg-ql--model-properties (heading)
  "Return an alist of PROPERTIES drawer key/value pairs for HEADING.
Collects entries from every `dorg-drawer-item' named PROPERTIES in
document order.  DORG_ID is intentionally absent: the parser excludes
it from model properties, so the `id' predicate owns that lookup."
  (let (out)
    (dolist (item (dorg-heading-items heading))
      (when (and (cl-typep item 'dorg-drawer-item)
                 (equal (dorg-drawer-item-name item) "PROPERTIES"))
        (dolist (kv (dorg-drawer-item-properties item))
          (push (cons (car kv) (cdr kv)) out))))
    (nreverse out)))

(defun dorg-ql--model-keywords (heading)
  "Return an alist of non-DORG keyword item key/value pairs for HEADING.
Keyword predicates are heading-scoped only: a level-0 root host returns
nil even when it owns file-level keyword items."
  (let (out)
    (when (> (or (dorg-heading-level heading) 0) 0)
      (dolist (item (dorg-heading-items heading))
        (when (and (cl-typep item 'dorg-keyword-item)
                   (not (dorg-item--db-local-p item)))
          (push (cons (dorg-keyword-item-keyword item)
                      (dorg-keyword-item-value item))
                out))))
    (nreverse out)))

;;; ─── Normalized v2 AST evaluator ──────────────────────────────────────

;; The evaluator below consumes the backend-neutral AST from
;; `dorg-ql-normalize' --- a pipeline `(:pipe (:source :all) STEP...)' whose
;; steps are `(:filter EXPR)', `(:traverse REL...)', and `(:unique)' --- and
;; answers it against an in-memory tree of `dorg-heading' structs.

;; The neutral scalar/string operators (`dorg-ql--apply-order-op',
;; `dorg-ql--apply-string-op') and `dorg-ql--priority-rank' now live in
;; `dorg-ql-eval.el', shared with the plain-org and Dolt adapters.

;;;; Field comparison

(defun dorg-ql--priority-match-p (op value heading)
  "Return non-nil when HEADING's priority satisfies OP against VALUE.
`='/`!=' compare exact strings; the ordered operators compare Org
priority rank (`A' highest).  A heading with no priority never matches an
ordering or `=' operator."
  (let ((pri (dorg-heading-priority heading)))
    (pcase op
      ('= (equal pri value))
      ('!= (and pri (not (equal pri value))))
      (_ (let ((pr (dorg-ql--priority-rank pri))
               (vr (dorg-ql--priority-rank value)))
           (and pr vr
                (pcase op
                  ('< (< pr vr))
                  ('<= (<= pr vr))
                  ('> (> pr vr))
                  ('>= (>= pr vr))
                  (_ (dorg-ql--error
                      "unsupported priority operator: %S" op)))))))))

(defun dorg-ql--cmp-match-p (field op value heading)
  "Return non-nil when HEADING's FIELD satisfies OP against VALUE.
FIELD is one of `title todo priority commented level parent-id'.  A
nil/absent string or parent-id value never matches; `commented' is a
real boolean and `level' is always present."
  (pcase field
    ('title (let ((s (dorg-heading-title heading)))
              (and (stringp s) (dorg-ql--apply-string-op op s value))))
    ('todo (let ((s (dorg-heading-todo heading)))
             (and (stringp s) (dorg-ql--apply-string-op op s value))))
    ('priority (dorg-ql--priority-match-p op value heading))
    ('commented
     (let ((c (and (dorg-heading-commented heading) t)))
       (pcase op
         ('= (eq c value))
         ('!= (not (eq c value)))
         (_ (dorg-ql--error "unsupported commented operator: %S" op)))))
    ('level (dorg-ql--apply-order-op op (dorg-heading-level heading) value))
    ('parent-id (let ((p (dorg-heading-parent-id heading)))
                  (and p (dorg-ql--apply-order-op op p value))))
    (_ (dorg-ql--error "unknown comparison field: %S" field))))

;;;; Property / tag / timestamp predicates

(defun dorg-ql--property-pred-match-p (key op value heading)
  "Return non-nil when HEADING's PROPERTIES drawer satisfies KEY OP VALUE.
`exists'/`missing' test KEY presence; the comparison operators require
KEY present and then compare its value string."
  (dorg-ql--alist-match-p key op value (dorg-ql--model-properties heading)))

(defun dorg-ql--keyword-pred-match-p (key op value heading)
  "Return non-nil when HEADING's local keyword items satisfy KEY OP VALUE.
The level-0 root host is never matched by keyword predicates; root-owned
file-level keywords are addressed only by explicit root mutation targets."
  (and (> (or (dorg-heading-level heading) 0) 0)
       (dorg-ql--alist-match-p key op value (dorg-ql--model-keywords heading))))

(defun dorg-ql--tag-pred-match-p (op value heading)
  "Return non-nil when HEADING's local tag set satisfies OP against VALUE.
Tags are a set: `=' is membership, `!=' non-membership (an empty set
satisfies `!='), `contains'/`prefix'/`suffix' match any tag, and
`exists'/`missing' test set emptiness."
  (let ((tags (dorg-heading-tags heading)))
    (pcase op
      ('exists (and tags t))
      ('missing (not tags))
      ('= (and (member value tags) t))
      ('!= (not (member value tags)))
      ((or 'contains 'prefix 'suffix)
       (and (cl-some (lambda (tag) (dorg-ql--apply-string-op op tag value))
                     tags)
            t))
      (_ (dorg-ql--error "unsupported tag operator: %S" op)))))

(defun dorg-ql--timestamp-pred-match-p (source span heading)
  "Return non-nil when a HEADING timestamp from SOURCE overlaps SPAN.
SOURCE is a `dorg-ql--timestamp-sources' symbol or a `(:property KEY|*)'
form; SPAN is a `(START . END)' date pair.  Reuses the same `dorg-model'
projections and `dorg-ql--timestamp-in-window-p' as the atomic matcher."
  (let ((from (car span))
        (to (cdr span)))
    (pcase source
      ('scheduled (let ((ts (dorg-heading-scheduled-timestamp heading)))
                    (and ts (dorg-ql--timestamp-in-window-p ts from to))))
      ('deadline (let ((ts (dorg-heading-deadline-timestamp heading)))
                   (and ts (dorg-ql--timestamp-in-window-p ts from to))))
      ('closed (let ((ts (dorg-heading-closed-timestamp heading)))
                 (and ts (dorg-ql--timestamp-in-window-p ts from to))))
      ('active-ts
       (and (cl-some (lambda (ts) (dorg-ql--timestamp-in-window-p ts from to))
                     (dorg-heading-active-timestamps heading))
            t))
      ('logbook
       (and (cl-some (lambda (e)
                       (dorg-ql--timestamp-in-window-p
                        (dorg-logbook-entry-timestamp e) from to))
                     (dorg-heading-logbook-entries heading))
            t))
      (`(:property ,name)
       (and (cl-some (lambda (pt)
                       (and (or (eq name '*)
                                (equal (plist-get pt :property) name))
                            (dorg-ql--timestamp-in-window-p
                             (plist-get pt :timestamp) from to)))
                     (dorg-heading-property-timestamps heading))
            t))
      (_ (dorg-ql--error "unknown timestamp source: %S" source)))))

;;;; Filter expression dispatch

(defun dorg-ql--model-leaf-match-p (expr heading)
  "Return non-nil when HEADING satisfies the normalized leaf predicate EXPR.
EXPR is one of the model's leaf nodes (`:id', `:cmp', `:property',
`:keyword', `:tag', `:timestamp'); boolean composition is handled by
`dorg-ql--eval-expr-match-p'.  An unknown node signals `dorg-ql-error'."
  (pcase expr
    (`(:id ,op ,n)
     (let ((id (dorg-heading-id heading)))
       (and id (dorg-ql--apply-order-op op id n))))
    (`(:cmp ,field ,op ,value)
     (dorg-ql--cmp-match-p field op value heading))
    (`(:property ,key ,op ,value)
     (dorg-ql--property-pred-match-p key op value heading))
    (`(:keyword ,key ,op ,value)
     (dorg-ql--keyword-pred-match-p key op value heading))
    (`(:tag ,op ,value)
     (dorg-ql--tag-pred-match-p op value heading))
    (`(:timestamp ,source ,_rel ,span)
     (dorg-ql--timestamp-pred-match-p source span heading))
    (_ (dorg-ql--error "unknown filter expression: %S" expr))))

(defun dorg-ql--expr-match-p (expr heading)
  "Return non-nil when HEADING satisfies the normalized filter EXPR.
EXPR is a `(:and ...)'/`(:or ...)'/`(:not ...)' tree or a leaf predicate
node (`:id', `:cmp', `:property', `:keyword', `:tag', `:timestamp').  A
thin wrapper over the shared `dorg-ql--eval-expr-match-p' boolean driver
with the model leaf matcher; the plain-org and Dolt adapters fall back to
this function for the headline leaves they do not override."
  (dorg-ql--eval-expr-match-p expr heading #'dorg-ql--model-leaf-match-p))

;;;; Tree index & traversal

(defun dorg-ql--model-identity-step-p (ast)
  "Non-nil when AST contains a step that needs stable heading identity."
  (and (consp ast)
       (eq (car ast) :pipe)
       (cl-some (lambda (step)
                  (pcase step
                    (`(:traverse . ,_) t)
                    (`(:unique) t)))
                (cddr ast))))

(defun dorg-ql--model-validate-identity (headings)
  "Validate model-list HEADINGS have unique non-nil ids for identity steps.
Signals `dorg-ql-error' when a heading id is nil or duplicated."
  (let ((seen (make-hash-table :test 'eql)))
    (dolist (h headings)
      (let ((id (dorg-heading-id h)))
        (unless id
          (dorg-ql--error
           "model-list traversal/unique requires non-nil heading ids"))
        (when (gethash id seen)
          (dorg-ql--error
           "model-list traversal/unique requires unique heading ids: %S"
           id))
        (puthash id t seen)))))

;; The tree index, traversal relations, order/de-duplication, and the
;; pipeline-step driver now live in `dorg-ql-eval.el' (`dorg-ql--build-tree-index',
;; `dorg-ql--relate', `dorg-ql--order-dedup', `dorg-ql--unique',
;; `dorg-ql--traverse-step', `dorg-ql--eval-step', `dorg-ql--eval-pipeline'),
;; shared with the plain-org and Dolt adapters.

(defun dorg-ql--model-eval (ast headings)
  "Evaluate normalized AST against HEADINGS and return result plists.
AST is a normalized pipeline `(:pipe (:source :all) STEP...)' from
`dorg-ql-normalize'.  HEADINGS is the in-memory tree of `dorg-heading'
structs that supplies both the `:all' candidate set and the traversal
context.  Steps run left to right over a working heading list; the final
list is mapped to markerless result plists in input/tree order, the same
stable four-key shape `dorg-ql--make-result' produces.

For public model-list sources, HEADINGS are an ordered forest.  Filter-only
pipelines do not require identity.  Pipelines containing traversal or
`unique' require every heading to have a unique non-nil id so traversal,
de-duplication, and result order are deterministic."
  (unless (and (consp ast) (eq (car ast) :pipe)
               (equal (cadr ast) '(:source :all)))
    (dorg-ql--error
     "model evaluator expects a normalized (:pipe (:source :all) ...) AST: %S"
     ast))
  (when (dorg-ql--model-identity-step-p ast)
    (dorg-ql--model-validate-identity headings))
  (let* ((idx (dorg-ql--build-tree-index headings))
         ;; (:source :all) is the only source; skip it and the :pipe head.
         (working (dorg-ql--eval-pipeline
                   (cddr ast) (copy-sequence headings) idx
                   #'dorg-ql--expr-match-p)))
    (mapcar (lambda (h)
              (dorg-ql--make-result
               :heading-id (dorg-heading-id h)
               :marker nil
               :title (dorg-heading-title h)
               :properties (dorg-ql--model-properties h)))
            working)))

(provide 'dorg-ql-model)
;;; dorg-ql-model.el ends here
