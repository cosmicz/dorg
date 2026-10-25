;;; dorg-ql-eval.el --- Neutral evaluation core for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The backend-neutral evaluation core shared by every dorg-ql adapter
;; (model, plain-org, Dolt).  It owns the machinery that is identical
;; across backends and differs only in which leaf predicates or filter
;; matcher a caller supplies:
;;
;; - the scalar/string operators (`dorg-ql--apply-order-op',
;;   `dorg-ql--apply-string-op') and Org priority rank
;;   (`dorg-ql--priority-rank');
;; - the shared alist matcher `dorg-ql--alist-match-p' (one
;;   exists/missing/string-op matcher over an assoc list) that replaces the
;;   per-adapter property/keyword matchers;
;; - the boolean AST recursion `dorg-ql--eval-expr-match-p', which walks
;;   `:and'/`:or'/`:not' and delegates every other (leaf) node to a
;;   caller-supplied LEAF-FN;
;; - the pipeline-step driver `dorg-ql--eval-step' /
;;   `dorg-ql--eval-pipeline', parameterized by a caller-supplied FILTER-FN
;;   for `:filter' while sharing `:traverse'/`:unique'/`:one';
;; - the AST filter walkers/collectors `dorg-ql--map-filters',
;;   `dorg-ql--collect-nodes', `dorg-ql--any-node-p' over the top-level
;;   `:filter' expressions of a normalized pipeline;
;; - the in-memory tree index and traversal substrate
;;   (`dorg-ql--build-tree-index', `dorg-ql--relate', `dorg-ql--order-dedup',
;;   `dorg-ql--unique', `dorg-ql--traverse-step').
;;
;; There is deliberately NO adapter protocol/struct/generic here: plain
;; function parameters (a leaf matcher, a filter matcher) carry every
;; backend difference.  This module requires only `cl-lib', `dorg-model',
;; and `dorg-ql-core'; it never requires an adapter, so the require graph
;; stays acyclic.

;;; Code:

(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-ql-core)

;;;; Scalar / string operators

(defun dorg-ql--apply-order-op (op a b)
  "Apply ordered comparison OP between integers A and B.
OP is one of `= != < <= > >='.  Signals `dorg-ql-error' otherwise."
  (pcase op
    ('=  (= a b))
    ('!= (/= a b))
    ('<  (< a b))
    ('<= (<= a b))
    ('>  (> a b))
    ('>= (>= a b))
    (_ (dorg-ql--error "unsupported ordered operator: %S" op))))

(defun dorg-ql--case-canonical-string (value)
  "Return VALUE's per-character standard-case uppercase spelling.
ASCII unibyte text is text; all other raw-byte/non-Unicode strings are a typed
limitation rather than an implicit byte or locale collation."
  (unless (stringp value)
    (dorg-ql--error "case canonicalization requires a string: %S" value))
  (with-case-table (standard-case-table)
    (apply #'string
           (mapcar
            (lambda (char)
              (unless (and (<= 0 char #x10ffff)
                           (not (<= #xd800 char #xdfff))
                           (or (multibyte-string-p value) (<= char #x7f)))
                (dorg-ql--unsupported
                 "case-insensitive text contains a non-Unicode/raw-byte character"))
              (upcase char))
            (string-to-list value)))))

(defun dorg-ql--apply-string-op (op actual value &optional mode)
  "Apply string OP comparing ACTUAL against VALUE under literal MODE.
ACTUAL must be a non-nil string; VALUE is the query string.  OP is one
of `= != contains prefix suffix'.  MODE defaults to `sensitive';
`insensitive' canonicalizes each character under `standard-case-table' before
using the same literal operations.  Signals `dorg-ql-error' otherwise."
  (pcase-let* ((`(,actual . ,value)
                (if (or (null mode) (eq mode 'sensitive))
                    (cons actual value)
                  (if (eq mode 'insensitive)
                      (cons (dorg-ql--case-canonical-string actual)
                            (dorg-ql--case-canonical-string value))
                    (dorg-ql--error "unknown string case mode: %S" mode)))))
    (pcase op
      ('= (string= actual value))
      ('!= (not (string= actual value)))
      ('contains (and (string-search value actual) t))
      ('prefix (string-prefix-p value actual))
      ('suffix (string-suffix-p value actual))
      (_ (dorg-ql--error "unsupported string operator: %S" op)))))

(defun dorg-ql--priority-rank (priority)
  "Return an integer rank for PRIORITY where a higher Org priority ranks higher.
PRIORITY is a one-character cookie string such as \"A\".  Org orders
priorities with `A' highest, so the rank is the negated character code
and \"A\" outranks \"B\".  Returns nil for a nil or empty PRIORITY."
  (and (stringp priority)
       (> (length priority) 0)
       (- (aref priority 0))))

;;;; Shared alist predicate matching

(defun dorg-ql--alist-match-p (key op value alist &optional mode)
  "Return non-nil when the assoc ALIST satisfies KEY OP VALUE.
`exists'/`missing' test KEY presence; every other operator requires KEY
present and then compares its value string with `dorg-ql--apply-string-op'.
This is the single matcher shared by the model, plain-org, and Dolt
property/keyword leaf predicates."
  (let ((cell (assoc key alist)))
    (pcase op
      ('exists (and cell t))
      ('missing (not cell))
      (_ (and cell (dorg-ql--apply-string-op op (cdr cell) value mode))))))

;;;; Boolean AST recursion

(defun dorg-ql--eval-expr-match-p (expr heading leaf-fn)
  "Return non-nil when HEADING satisfies normalized filter EXPR.
EXPR is a `(:and ...)'/`(:or ...)'/`(:not ...)' tree whose boolean nodes
recurse here; every other (leaf) node is delegated to LEAF-FN, called as
\(funcall LEAF-FN NODE HEADING).  `:and'/`:or' short-circuit through
`cl-every'/`cl-some' and `:or' normalizes its result to t."
  (pcase expr
    (`(:and . ,exprs)
     (cl-every (lambda (e) (dorg-ql--eval-expr-match-p e heading leaf-fn)) exprs))
    (`(:or . ,exprs)
     (and (cl-some (lambda (e) (dorg-ql--eval-expr-match-p e heading leaf-fn))
                   exprs)
          t))
    (`(:not ,e) (not (dorg-ql--eval-expr-match-p e heading leaf-fn)))
    (_ (funcall leaf-fn expr heading))))

;;;; AST filter walkers / collectors

(defun dorg-ql--map-filters (ast fn)
  "Call FN on each top-level `:filter' EXPR of normalized AST.
Skips the `:pipe' head and the `(:source :all)' source step, and never
visits non-`:filter' steps such as `:traverse'/`:unique'/`:one'."
  (dolist (step (cddr ast))
    (pcase step (`(:filter ,expr) (funcall fn expr)))))

(defun dorg-ql--collect-nodes (ast head)
  "Return the distinct top-level filter nodes of AST whose car is HEAD.
Walks the top-level `:filter' expressions, recursing through `:and'/`:or'/
`:not' boolean nodes, and collects every node selected BY CAR (car eq
HEAD).  Nodes are de-duplicated by `equal' over the WHOLE node, so two
distinct nodes that share the same head both survive while an exactly
repeated node collapses to one.  Returns them in first-occurrence order."
  (let (nodes)
    (cl-labels ((walk (e)
                  (pcase e
                    (`(:and . ,es) (mapc #'walk es))
                    (`(:or . ,es) (mapc #'walk es))
                    (`(:not ,e2) (walk e2))
                    (_ (when (and (consp e) (eq (car e) head))
                         (cl-pushnew e nodes :test #'equal))))))
      (dorg-ql--map-filters ast #'walk))
    (nreverse nodes)))

(defun dorg-ql--any-node-p (ast head)
  "Non-nil when AST contains a top-level filter node whose car is HEAD.
Walks the top-level `:filter' expressions, recursing through boolean
nodes, and returns t on the first leaf node selected by car."
  (catch 'found
    (cl-labels ((walk (e)
                  (pcase e
                    (`(:and . ,es) (mapc #'walk es))
                    (`(:or . ,es) (mapc #'walk es))
                    (`(:not ,e2) (walk e2))
                    (_ (when (and (consp e) (eq (car e) head))
                         (throw 'found t))))))
      (dorg-ql--map-filters ast #'walk))
    nil))

;;;; Tree index & traversal

(defun dorg-ql--build-tree-index (headings)
  "Build a traversal index over HEADINGS, in input order.
Returns a plist `(:by-id H :children H :pos H)' where `:by-id' maps a
heading id to its struct, `:children' maps a parent id to its child
structs in input order, and `:pos' maps a heading id to its index in
HEADINGS (the canonical output order).  The child map is keyed by
parent-id, so children/siblings resolve even when a parent (such as the
level-0 root) is absent from HEADINGS."
  (let ((by-id (make-hash-table :test 'eql))
        (children (make-hash-table :test 'eql))
        (pos (make-hash-table :test 'eql))
        (i 0))
    (dolist (h headings)
      (puthash (dorg-heading-id h) h by-id)
      (puthash (dorg-heading-id h) i pos)
      (setq i (1+ i)))
    (dolist (h headings)
      (let ((pid (dorg-heading-parent-id h)))
        (when pid
          (push h (gethash pid children)))))
    (maphash (lambda (k v) (puthash k (nreverse v) children)) children)
    (list :by-id by-id :children children :pos pos)))

(defun dorg-ql--relate (rel heading idx)
  "Return the headings related to HEADING by relation REL via index IDX.
REL is one of `self parent children ancestors descendants siblings'.
`ancestors' and `descendants' are guarded against parent-id cycles."
  (let ((by-id (plist-get idx :by-id))
        (children (plist-get idx :children)))
    (pcase rel
      ('self (list heading))
      ('parent
       (let* ((pid (dorg-heading-parent-id heading))
              (p (and pid (gethash pid by-id))))
         (and p (list p))))
      ('children (copy-sequence (gethash (dorg-heading-id heading) children)))
      ('siblings
       (let ((pid (dorg-heading-parent-id heading)))
         (and pid (cl-remove heading (gethash pid children) :test #'eq))))
      ('ancestors
       (let ((seen (make-hash-table :test 'eql))
             (pid (dorg-heading-parent-id heading))
             out)
         (puthash (dorg-heading-id heading) t seen)
         (while (and pid (gethash pid by-id) (not (gethash pid seen)))
           (let ((p (gethash pid by-id)))
             (push p out)
             (puthash pid t seen)
             (setq pid (dorg-heading-parent-id p))))
         (nreverse out)))
      ('descendants
       (let ((seen (make-hash-table :test 'eql))
             out)
         (cl-labels ((walk (h)
                       (dolist (c (gethash (dorg-heading-id h) children))
                         (unless (gethash (dorg-heading-id c) seen)
                           (puthash (dorg-heading-id c) t seen)
                           (push c out)
                           (walk c)))))
           (walk heading))
         (nreverse out)))
      (_ (dorg-ql--error "unknown traverse relation: %S" rel)))))

(defun dorg-ql--order-dedup (headings idx)
  "De-duplicate HEADINGS by id and order them by input position via IDX."
  (let ((pos (plist-get idx :pos))
        (seen (make-hash-table :test 'eql))
        out)
    (dolist (h headings)
      (let ((id (dorg-heading-id h)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push h out))))
    (sort out
          (lambda (a b)
            (< (gethash (dorg-heading-id a) pos most-positive-fixnum)
               (gethash (dorg-heading-id b) pos most-positive-fixnum))))))

(defun dorg-ql--unique (headings)
  "De-duplicate HEADINGS by id, preserving their current order."
  (let ((seen (make-hash-table :test 'eql))
        out)
    (dolist (h headings)
      (let ((id (dorg-heading-id h)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push h out))))
    (nreverse out)))

(defun dorg-ql--traverse-step (relations working idx)
  "Apply a traverse step's RELATIONS as a union over the WORKING set.
Each relation in RELATIONS is applied to every heading in WORKING; the
combined results are de-duplicated by id and returned in input order."
  (let (collected)
    (dolist (h working)
      (dolist (rel relations)
        (setq collected (nconc collected (dorg-ql--relate rel h idx)))))
    (dorg-ql--order-dedup collected idx)))

;;;; Pipeline driver

(defun dorg-ql--eval-step (step working idx filter-fn)
  "Apply one normalized pipeline STEP to the WORKING heading list via IDX.
A `:filter' step keeps each heading for which FILTER-FN, called as
\(funcall FILTER-FN EXPR HEADING), returns non-nil; `:traverse',
`:unique', and `:one' are backend-agnostic and use the shared helpers.
An unknown step signals `dorg-ql-error'."
  (pcase step
    (`(:filter ,expr)
     (cl-remove-if-not (lambda (h) (funcall filter-fn expr h)) working))
    (`(:traverse . ,relations)
     (dorg-ql--traverse-step relations working idx))
    (`(:unique) (dorg-ql--unique working))
    (`(:one) (dorg-ql--assert-one working))
    (_ (dorg-ql--error "unknown pipeline step: %S" step))))

(defun dorg-ql--eval-pipeline (steps working idx filter-fn)
  "Thread WORKING through STEPS left to right and return the final list.
Each step is applied with `dorg-ql--eval-step' using IDX and FILTER-FN.
An empty STEPS list is the identity: WORKING is returned unchanged."
  (dolist (step steps working)
    (setq working (dorg-ql--eval-step step working idx filter-fn))))

(provide 'dorg-ql-eval)
;;; dorg-ql-eval.el ends here
