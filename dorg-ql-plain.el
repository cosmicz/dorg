;;; dorg-ql-plain.el --- Plain-org buffer adapter for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The plain-org adapter answers a normalized v2 AST against a registered
;; org file buffer, walking it heading by heading and returning one result
;; plist per match, carrying a live marker.  Every public query — a
;; historical atomic alias or a composable v2 form — reaches it as the one
;; normalized `(:pipe (:source :all) STEP...)' AST from `dorg-ql-normalize';
;; there is no separate atomic executor.
;;
;; Identity, property, title, and tag matches use light, local
;; (non-inherited) Org APIs against the entry's own data, with DORG_ID
;; owned exclusively by the `id' predicate.
;;
;; Timestamp-window predicates do NOT maintain their own section scanners.
;; They project the entry into the SAME `dorg-heading-item' objects the
;; real parser would attach (via `dorg-parse-section-items-at-heading')
;; and delegate to the shared model timestamp matcher
;; `dorg-ql--timestamp-pred-match-p'.  This makes plain-org
;; `scheduled'/`deadline'/`closed'/`property-ts'/`logbook'/`active-ts'
;; converge with the model and Dolt backends by construction: the
;; direct-section-child rules for planning, the PROPERTIES drawer, LOGBOOK
;; drawers, and text bodies live once, in the parser, eliminating the
;; duplicated LOGBOOK/body scanners that previously drifted.
;;
;; The plain-org executor for the normalized v2 AST
;; (`dorg-ql--plain-eval') builds one light, headline-only index over the
;; registered buffer keyed by an adapter-local document-order ordinal --- so
;; headings without a DORG_ID do not collide --- and reuses the model
;; evaluator's matching, traversal, order, and de-duplication helpers.
;; Item/body projection happens exactly once per heading when the query
;; contains a `:timestamp' or `:keyword' predicate.  Plain-org v2 results
;; carry live markers and the real DORG_ID as `:heading-id', and negated
;; timestamps are sound (exact item projection), so no Dolt-style negation
;; guard is needed.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'dorg-id)
(require 'dorg-registry)
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-ql-core)
(require 'dorg-ql-eval)
(require 'dorg-ql-model)

(defun dorg-ql--plain-org-buffer (reg)
  "Return a buffer visiting REG's plain-org file.
Reuses an existing buffer if one is already visiting the file;
otherwise opens it through the shared batch-safe source policy.  Signals
when the file does not exist."
  (let ((file (dorg-registration-file reg)))
    (unless (and file (file-exists-p file))
      (error "dorg-ql: plain-org file %s does not exist"
             (or file "<nil>")))
    (or (find-buffer-visiting file)
        (dorg--find-file-noselect file))))

(defun dorg-ql--plain-org-local-properties (element)
  "Return the local PROPERTIES alist owned by headline ELEMENT.
Reads the literal `node-property' entries of the entry's own drawer via
the headline boundary, exactly as the parser does, so inherited and
special values — most notably CATEGORY, which `org-entry-get' always
synthesizes with inheritance — are never returned.  DORG_ID is excluded:
identity is owned by the `id' predicate, mirroring the model and Dolt
adapters, which omit DORG_ID from their property sets."
  (let (out)
    (save-excursion
      ;; Do not call `org-get-property-block': it performs another
      ;; `org-element-at-point' lookup.  The already decoded headline gives
      ;; the exact local drawer start boundary, including an optional planning
      ;; line, and the literal scan preserves duplicate and `+' keys.
      (goto-char (org-element-property :begin element))
      (forward-line 1)
      (when (looking-at-p org-planning-line-re)
        (forward-line 1))
      (when (looking-at org-property-drawer-re)
        (let ((end (match-end 0)))
          (beginning-of-line)
          (forward-line 1)
          (while (< (point) end)
            (unless (looking-at-p "^[ \t]*:END:[ \t]*$")
              (when (looking-at org-property-re)
              (let ((key (match-string-no-properties 2))
                    (value (or (match-string-no-properties 3) "")))
                (unless (equal (upcase key) dorg-id-property)
                    (push (cons key value) out)))))
            (forward-line 1)))))
    (nreverse out)))

;;; ─── Normalized v2 AST adapter ────────────────────────────────────────

;; The v2 adapter consumes the backend-neutral AST from `dorg-ql-normalize'
;; --- a pipeline `(:pipe (:source :all) STEP...)' --- and answers it over a
;; single registered plain-org buffer.  It reuses the model evaluator's
;; matching, traversal, order, and de-duplication helpers so the plain-org,
;; model, and Dolt baselines converge by construction.
;;
;; Identity model: a registered plain-org file may carry headings with NO
;; DORG_ID, and two such headings would collide on a nil id.  So the working
;; unit is a `dorg-heading' struct whose `id'/`parent-id' slots hold an
;; ADAPTER-LOCAL document-order ordinal (a surrogate) and the ordinal of its
;; STRUCTURAL parent — never the real DORG_ID.  The model traversal/order/
;; unique helpers therefore apply unchanged over a collision-free, total
;; identity.  The real DORG_ID lives in a side hash keyed by ordinal and is
;; consulted only by the `id'/`field id'/`field parent-id' predicates and by
;; result materialization, so identity matching and the public `:heading-id'
;; use the real DORG_ID while traversal/de-dup use the surrogate.
;;
;; Cost discipline mirrors the Dolt planner: ONE light, headline-only buffer
;; walk builds the index (title/todo/priority/commented/level/tags via local
;; Org APIs, the local PROPERTIES drawer, and the DORG_ID), with NO item/body
;; projection.  Item projection (`dorg-parse-section-items-at-heading', the
;; same parser-equivalent path the atomic timestamp predicates use) happens
;; exactly once per heading and ONLY when the query contains a `:timestamp' or
;; `:keyword' node, so pure field/tag/property/traversal queries never parse a
;; body.
;;
;; Unlike the markerless model and Dolt results, plain-org v2 results carry a
;; LIVE marker, matching the atomic plain-org adapter.  Because timestamp
;; matching here is exact (full parser-equivalent item projection, not a
;; conservative scan), a `not' over a timestamp predicate has a sound
;; complement and is allowed --- the Dolt planner's negation guard is
;; unnecessary.  A non-`(:pipe (:source :all) ...)' AST and unknown node
;; shapes signal `dorg-ql-error'; the adapter never widens to a full scan or
;; silently empties the result.

(defun dorg-ql--plain-build-index ()
  "Build the headline index for the plain-org buffer at point.
The current buffer must be the registered plain-org buffer, widened.
Returns a plist `(:ordered H :real-id H :markers H :props H)' where
`:ordered' is the document-order list of `dorg-heading' structs carrying
an adapter-local ordinal in `id' and the structural parent's ordinal in
`parent-id' (nil at top level); `:real-id' maps an ordinal to the real
DORG_ID integer (nil when the heading has none); `:markers' maps an
ordinal to a live buffer marker; and `:props' maps an ordinal to the
heading's local PROPERTIES alist (DORG_ID excluded).  Reads headline
fields with light, local Org APIs only --- no item/body projection."
  (let ((ordered nil)
        (real-id (make-hash-table :test 'eql))
        (markers (make-hash-table :test 'eql))
        (props (make-hash-table :test 'eql))
        (ord 0)
        ;; Ancestor stack of (LEVEL . ORDINAL), nearest ancestor first.
        (stack nil)
        ;; This is the parser's phantom policy.  Compute it once, and test
        ;; before decoding so sample headings never become rows or ancestors.
        (verbatim-regions (dorg-parse--verbatim-regions)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (unless (dorg-parse--position-verbatim-p (point) verbatim-regions)
          ;; `org-heading-regexp' also sees recognised inline-task lines.
          ;; Decode each non-phantom candidate once, then retain only actual
          ;; headline elements; an inline task is never an ancestor.
          (let ((element (org-element-at-point)))
            (when (eq (org-element-type element) 'headline)
              (let ((lvl (org-element-property :level element))
                    (this-ord (cl-incf ord)))
                ;; Pop any ancestors at this level or deeper; the remaining top is
                ;; the structural parent.
                (while (and stack (>= (caar stack) lvl))
                  (setq stack (cdr stack)))
                (let* ((parent-ord (and stack (cdar stack)))
                       (title (let ((raw (org-element-property :raw-value element)))
                                (and raw (substring-no-properties raw))))
                       (todo (let ((keyword (org-element-property :todo-keyword element)))
                               (and keyword (substring-no-properties keyword))))
                       (priority (let ((c (org-element-property :priority element)))
                                   (and c (char-to-string c))))
                       (commented (and (org-element-property :commentedp element) t))
                       (tags (mapcar #'substring-no-properties
                                     (org-element-property :tags element)))
                       (real (dorg-id-at-point-plain))
                       (h (make-dorg-heading
                           :id this-ord :parent-id parent-ord :level lvl
                           ;; A top-level heading's parent is the document (0).
                           :level-offset (- lvl (if stack (caar stack) 0))
                           :title title :todo todo :priority priority
                           :commented commented :tags tags)))
                  (push h ordered)
                  (puthash this-ord real real-id)
                  (puthash this-ord (copy-marker (point)) markers)
                  (puthash this-ord
                           (dorg-ql--plain-org-local-properties element) props)
                  (push (cons lvl this-ord) stack))))))
        ;; Always advance: a verbatim phantom has no decoded row but still
        ;; occupies the current regexp match.
        (end-of-line)))
    (list :ordered (nreverse ordered)
          :real-id real-id :markers markers :props props)))

(defun dorg-ql--plain-project-items (ordered markers)
  "Populate ORDERED headings' item slots from MARKERS.
ORDERED is the heading list (adapter-local ordinals), and MARKERS maps an
ordinal to its live heading marker.  This eager query-local projection is
used only when the normalized AST contains a `:timestamp' or `:keyword'
node; each heading is parsed exactly once, and the shared model matcher reads
the resulting `dorg-heading-items' directly."
  (dolist (h ordered)
    (let* ((ord (dorg-heading-id h))
           (marker (gethash ord markers)))
      (goto-char marker)
      (setf (dorg-heading-items h)
            (dorg-parse-section-items-at-heading)))))

(defun dorg-ql--plain-leaf-match-p (expr heading ctx)
  "Return non-nil when HEADING satisfies the normalized leaf EXPR under CTX.
CTX is a plist `(:real-id H :props H)'.
`:id' and `field parent-id' resolve the REAL DORG_ID from CTX (the
struct's id/parent-id are adapter-local ordinals); `:property' consults
the loaded local PROPS via the shared alist matcher; every remaining leaf
delegates to the shared model matcher over the query-local heading struct, so
plain-org and model semantics cannot drift.  Boolean composition is handled
by `dorg-ql--eval-expr-match-p'."
  (pcase expr
    (`(:id ,op ,n)
     (let ((real (gethash (dorg-heading-id heading) (plist-get ctx :real-id))))
       (and real (dorg-ql--apply-order-op op real n))))
    (`(:cmp parent-id ,op ,value)
     (let* ((pord (dorg-heading-parent-id heading))
            (real (and pord (gethash pord (plist-get ctx :real-id)))))
       (and real (dorg-ql--apply-order-op op real value))))
    (`(:cmp parent-id ,_ ,_ :case ,_)
     (dorg-ql--error ":case is not supported for parent-id"))
    (`(:cmp ,field ,op ,value :case ,mode)
     (if (dorg-ql--query-field field)
         (dorg-ql--extension-match-p
          field op value
          (lambda (expr) (dorg-ql--plain-value expr heading ctx)) mode)
       (dorg-ql--model-leaf-match-p expr heading)))
    (`(:cmp ,field ,op ,value)
     (if (dorg-ql--query-field field)
         (dorg-ql--extension-match-p
          field op value
          (lambda (expr) (dorg-ql--plain-value expr heading ctx)))
       (dorg-ql--model-leaf-match-p expr heading)))
    (`(:property ,key ,op ,value :case ,mode)
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :props)) mode))
    (`(:property ,key ,op ,value)
     (unless (memq op '(exists missing))
       (dorg-ql--error "string property comparison lacks canonical :case mode: %S" expr))
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :props))))
    (_ (dorg-ql--expr-match-p expr heading))))

(defun dorg-ql--plain-expr-match-p (expr heading ctx)
  "Return non-nil when HEADING satisfies normalized filter EXPR under CTX.
A thin wrapper over the shared boolean driver `dorg-ql--eval-expr-match-p'
with the plain-org leaf matcher closed over CTX."
  (dorg-ql--eval-expr-match-p
   expr heading
   (lambda (node h) (dorg-ql--plain-leaf-match-p node h ctx))))

(defun dorg-ql--plain-value (expr heading index)
  "Read EXPR from HEADING using INDEX's real identity and local properties."
  (let ((ids (plist-get index :real-id)))
    (pcase expr
      (`(:field id) (gethash (dorg-heading-id heading) ids))
      (`(:field parent-id) (gethash (dorg-heading-parent-id heading) ids))
      (`(:property ,key)
       (cdr (assoc key (gethash (dorg-heading-id heading)
                               (plist-get index :props)))))
      (`(:field ,field)
       (if (dorg-ql--query-field field)
           (dorg-ql--extension-value
            field (lambda (dependency)
                    (dorg-ql--plain-value dependency heading index)))
         (dorg-ql--model-value expr heading)))
      (_ (dorg-ql--model-value expr heading)))))

(defun dorg-ql--plain-materialize (headings index &optional columns)
  "Map HEADINGS to result plists in their current order using INDEX.
`:heading-id' is the real DORG_ID (nil when the heading has none),
`:marker' is the heading's live marker, and `:properties' is its local
PROPERTIES alist --- the same stable shape the atomic plain-org adapter
produces."
  (let ((real-id (plist-get index :real-id))
        (markers (plist-get index :markers))
        (props (plist-get index :props)))
    (mapcar (lambda (h)
              (let ((ord (dorg-heading-id h)))
                (if columns
                    (dorg-ql--project-row
                     columns (gethash ord real-id) (gethash ord markers)
                     (lambda (expr) (dorg-ql--plain-value expr h index)))
                  (dorg-ql--make-result
                   :heading-id (gethash ord real-id)
                   :marker (gethash ord markers)
                   :title (dorg-heading-title h)
                   :properties (gethash ord props)))))
            headings)))

(defun dorg-ql--plain-eval (ast reg &optional buffer)
  "Plan and execute a normalized v2 AST against REG's plain-org buffer.
AST is `(:pipe (:source :all) STEP...)' from `dorg-ql-normalize'.  Returns
result plists in document/tree order with live markers and de-duplication
matching the model baseline.  Builds one light headline-only index; items
are projected exactly once per heading only when `:timestamp' or `:keyword'
predicates are present.  Signals `dorg-ql-error' for a non-pipeline AST or an
unknown node.  When BUFFER is non-nil, evaluate that already-captured source
rather than opening REG again."
  (unless (and (consp ast) (eq (car ast) :pipe)
               (equal (cadr ast) '(:source :all)))
    (dorg-ql--error
     "plain-org planner expects a normalized (:pipe (:source :all) ...) AST: %S"
     ast))
  (let ((buf (or buffer (dorg-ql--plain-org-buffer reg)))
        (project-items (or (dorg-ql--any-node-p ast :timestamp)
                           (dorg-ql--any-node-p ast :keyword))))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let* ((index (dorg-ql--plain-build-index))
              (ordered (plist-get index :ordered))
              (idx (dorg-ql--build-tree-index ordered)))
         (when project-items
           (dorg-ql--plain-project-items ordered (plist-get index :markers)))
         (let* ((ctx (list :real-id (plist-get index :real-id)
                           :props (plist-get index :props)))
                (working
                 (dorg-ql--eval-pipeline
                  (cddr (dorg-ql--selection-ast ast)) ordered idx
                  (lambda (expr h) (dorg-ql--plain-expr-match-p expr h ctx)))))
           (dorg-ql--plain-materialize
            working index (dorg-ql--projection-columns ast))))))))

(provide 'dorg-ql-plain)
;;; dorg-ql-plain.el ends here
