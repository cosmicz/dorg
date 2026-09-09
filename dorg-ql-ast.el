;;; dorg-ql-ast.el --- Normalized v2 AST and Lisp reader for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The central normalizer / validator for the composable dorg-ql v2 query
;; language.  `dorg-ql-normalize' is the single public-form parsing layer:
;; it lowers BOTH the historical atomic forms (kept as compatibility
;; aliases) AND the new composable v2 reader forms into one backend-neutral
;; AST.  The plain-org and Dolt buffer adapters consume that AST rather
;; than re-parsing reader forms independently, as does the public model-list
;; evaluator `dorg-ql--model-eval' (the semantic baseline).
;;
;; This module owns validation and error shape only.  It performs no Org,
;; model, or SQL evaluation; the evaluator/adapter slices build on it.
;; It does NOT own timestamp/window-bound expansion: that lives in
;; `dorg-ql-core' (the window-normalizer slice).  This module
;; delegates every window to that surface --- `dorg-ql--normalize-window-form'
;; for convenience forms (between/on/before/after/today/this-week) and
;; `dorg-ql--window-bound-span' for a single value in the explicit v2
;; `timestamp' form, and `dorg-ql--normalize-window-spec' for the atomic
;; timestamp aliases (which accept either `(HEAD FROM TO)' bounds or a
;; single `(HEAD WINDOW-FORM)', exactly as the core validator does) --- so
;; it never re-implements window parsing.
;;
;;; Public reader forms
;;
;; Compatibility aliases (unchanged surface, lowered to v2 AST):
;;
;;   (id ID)                 (property KEY VALUE)   (title TITLE)
;;   (tag TAG)               (scheduled FROM TO)    (deadline FROM TO)
;;   (closed FROM TO)        (active-ts FROM TO)    (logbook FROM TO)
;;   (property-ts FROM TO [NAME])
;;
;; Composable v2 forms:
;;
;;   (pipe STEP...)          STEP = (where P) | (traverse REL...) | (unique)
;;   (where PREDICATE)       (traverse RELATION...)        (unique)
;;   (and P...) (or P...) (not P)
;;   (field FIELD OP VALUE)  (property KEY OP VALUE)
;;   (tag OP VALUE)          (id OP VALUE)
;;   (timestamp SOURCE REL WINDOW)
;;
;;; Normalized AST
;;
;; A normalized query is always a pipeline whose first step is the source:
;;
;;   (:pipe (:source :all) STEP...)
;;
;; where each subsequent STEP is one of, in order:
;;
;;   (:filter EXPR)          (:traverse REL...)            (:unique)
;;
;; Steps stay ORDERED (filters and traversals may interleave), which is why
;; the pipeline is a flat ordered list rather than the bucketed slot shape
;; sketched in the parent design.  A top-level predicate P is equivalent to
;; (:pipe (:source :all) (:filter EXPR)).
;;
;; Filter EXPR nodes:
;;
;;   (:and EXPR...) (:or EXPR...) (:not EXPR)
;;   (:id OP INTEGER)
;;   (:cmp FIELD OP VALUE)
;;   (:property KEY OP VALUE)          ; VALUE nil for exists/missing
;;   (:keyword KEY OP VALUE)           ; VALUE nil for exists/missing
;;   (:tag OP VALUE)                   ; VALUE nil for exists/missing
;;   (:timestamp SOURCE REL (START . END))
;;
;; SOURCE is a planning/body/logbook symbol or (:property KEY) / (:property *).
;; START and END are `(YEAR MONTH DAY)' date triples or nil (open bound).
;;
;; Invalid arity, unknown fields/operators/predicates, and bad reader forms
;; all signal `dorg-ql-error' with reason `invalid-query'.  Normalization never
;; widens to a broad scan: a malformed form errors, it does not degrade to
;; "match everything".

;;; Code:

(require 'cl-lib)
(require 'dorg-ql-core)

;;; ─── Vocabularies ─────────────────────────────────────────────────────

(defconst dorg-ql--scalar-operators '(= != < <= > >=)
  "Ordered/equality comparison operators valid where a field has an order.")

(defconst dorg-ql--string-operators '(= != contains prefix suffix)
  "Operators valid for string-valued predicates.")

(defconst dorg-ql--existence-operators '(exists missing)
  "Existence operators; these take no VALUE argument.")

(defconst dorg-ql--prefix-operators
  '(= != < <= > >= contains prefix suffix)
  "Value-taking prefix operators accepted by the public reader.")

(defconst dorg-ql--field-operators
  '((id        . (= != < <= > >=))
    (title     . (= != contains prefix suffix))
    (todo      . (= != contains prefix suffix))
    (priority  . (= != < <= > >=))
    (commented . (= !=))
    (level     . (= != < <= > >=))
    (parent-id . (= != < <= > >=)))
  "Allowed operators per `field' name.
The car set is also the closed set of known fields: a `field' form whose
FIELD is not a key here signals `dorg-ql-error'.")

(defconst dorg-ql--property-operators
  (append dorg-ql--string-operators dorg-ql--existence-operators)
  "Operators valid for the v2 `property' predicate.")

(defconst dorg-ql--tag-operators
  (append dorg-ql--string-operators dorg-ql--existence-operators)
  "Operators valid for the v2 `tag' predicate.")

(defconst dorg-ql--keyword-operators
  (append dorg-ql--string-operators dorg-ql--existence-operators)
  "Operators valid for the v2 `keyword' predicate.")

(defconst dorg-ql--timestamp-sources '(scheduled deadline closed active-ts logbook)
  "Symbol timestamp SOURCEs (besides the (:property KEY)/(:property *) forms).")

(defconst dorg-ql--timestamp-relations '(:overlaps)
  "Timestamp relations supported in v1.
The design also lists `:within', `:starts-before', and `:starts-after';
those are later slices and currently signal `dorg-ql-error'.")

(defconst dorg-ql--traverse-relations
  '(self parent children ancestors descendants siblings)
  "Tree relations accepted by a `traverse' step.")

;;; ─── Window-span delegation ───────────────────────────────────────────

;; Window normalization is owned entirely by `dorg-ql-core' (the
;; window-normalizer slice).  This module re-uses that surface to lower a
;; v2 WINDOW form to
;; the `(START . END)' span the AST stores; it never re-implements window
;; parsing.  Malformed windows surface as `dorg-ql-error' from core and are
;; never widened to match-all.

(defun dorg-ql--ast-normalize-window (window)
  "Normalize a v2 timestamp WINDOW form to a `(START . END)' date span.
Delegates to the window normalizer owned by `dorg-ql-core': the
convenience forms `(between FROM TO)', `(on B)', `(before B)',
`(after B)', `(today)', and `(this-week)' expand via
`dorg-ql--normalize-window-form'; nil is an open span; any other single
value --- a bare \"YYYY-MM-DD\" string, a raw Org timestamp, or a
`dorg-timestamp' --- resolves via `dorg-ql--window-bound-span'.
Malformed windows signal `dorg-ql-error' (never widened to match-all)."
  (cond
   ((null window) (cons nil nil))
   ((dorg-ql--window-form-p window)
    (let ((span (dorg-ql--normalize-window-form window)))
      (cons (nth 0 span) (nth 1 span))))
   (t (dorg-ql--window-bound-span window))))

;;; ─── Operator / value validation ──────────────────────────────────────

(defun dorg-ql--check-operator (op allowed kind)
  "Signal `dorg-ql-error' unless OP is in ALLOWED for predicate KIND."
  (unless (memq op allowed)
    (dorg-ql--error "%s operator must be one of %S: %S" kind allowed op))
  op)

(defun dorg-ql--normalize-field-value (field value)
  "Validate and normalize VALUE for comparison FIELD.
Numeric fields normalize numeric strings to integers; the others enforce
their value type.  Signals `dorg-ql-error' on a type mismatch."
  (pcase field
    ((or 'id 'level 'parent-id)
     (dorg-ql--normalize-id value))
    ('priority
     (cond
      ((and (stringp value) (= (length value) 1)) value)
      ((characterp value) (char-to-string value))
      (t (dorg-ql--error
          "field priority value must be a one-character string or character: %S"
          value))))
    ('commented
     (unless (memq value '(t nil))
       (dorg-ql--error "field commented value must be t or nil: %S" value))
     value)
    ((or 'title 'todo)
     (unless (stringp value)
       (dorg-ql--error "field %s value must be a string: %S" field value))
     value)
    (_ (dorg-ql--error "unknown field `%S'; expected one of %S"
                       field (mapcar #'car dorg-ql--field-operators)))))

;;; ─── Predicate normalization ──────────────────────────────────────────

(defun dorg-ql--normalize-field-comparison (field op value _query)
  "Normalize FIELD OP VALUE into a field AST node.
_QUERY is accepted for normalizer signature parity and is unused.
A `field id' folds into the dedicated `(:id OP INTEGER)' identity node."
  (let ((allowed (cdr (assq field dorg-ql--field-operators))))
    (unless allowed
      (dorg-ql--error "unknown field `%S'; expected one of %S"
                      field (mapcar #'car dorg-ql--field-operators)))
    (dorg-ql--check-operator op allowed (format "field %s" field))
    (let ((nval (dorg-ql--normalize-field-value field value)))
      (if (eq field 'id)
          (list :id op nval)
        (list :cmp field op nval)))))

(defun dorg-ql--normalize-field (args query)
  "Normalize a `(field FIELD VALUE)' equality atom into an AST node.
QUERY is the original form for error messages.  A `field id' folds into
the dedicated `(:id OP INTEGER)' identity node."
  (unless (= (length args) 2)
    (dorg-ql--error "(field FIELD VALUE) takes two arguments: %S" query))
  (dorg-ql--normalize-field-comparison (nth 0 args) '= (nth 1 args) query))

(defun dorg-ql--normalize-id-form (args query)
  "Normalize the `id' predicate ARGS into a `(:id OP INTEGER)' node.
Accepts the `(id ID)' alias and the v2 `(id OP VALUE)' form."
  (pcase (length args)
    (1 (list :id '= (dorg-ql--normalize-id (nth 0 args))))
    (2 (let ((op (dorg-ql--check-operator (nth 0 args)
                                          dorg-ql--scalar-operators "id")))
         (list :id op (dorg-ql--normalize-id (nth 1 args)))))
    (_ (dorg-ql--error
        "(id ID) or (id OP VALUE) takes one or two arguments: %S" query))))

(defun dorg-ql--normalize-property-form (args query)
  "Normalize the `property' predicate ARGS into a `(:property KEY OP VALUE)' node.
Accepts only the `(property KEY VALUE)' equality atom in predicate position."
  (let ((key (car args)))
    (unless (stringp key)
      (dorg-ql--error "property KEY must be a string: %S" query))
    (pcase (cdr args)
      ((and `(,value) (guard (stringp value)))
       (list :property key '= value))
      (_ (dorg-ql--error
          "property form must be (property KEY VALUE): %S"
          query)))))

(defun dorg-ql--normalize-tag-form (args query)
  "Normalize the `tag' predicate ARGS into a `(:tag OP VALUE)' node.
Accepts only the `(tag TAG)' membership atom in predicate position."
  (pcase args
    ((and `(,tag) (guard (stringp tag)))
     (list :tag '= tag))
    (_ (dorg-ql--error
        "tag form must be (tag TAG): %S"
        query))))

(defun dorg-ql--normalize-keyword-form (args query)
  "Normalize a keyword predicate ARGS into a `(:keyword KEY OP VALUE)' node."
  (let ((key (car args)))
    (unless (stringp key)
      (dorg-ql--error "keyword KEY must be a string: %S" query))
    (pcase (cdr args)
      ('() (list :keyword key 'exists nil))
      ((and `(,value) (guard (stringp value)))
       (list :keyword key '= value))
      (_ (dorg-ql--error
          "keyword form must be (keyword KEY) or (keyword KEY VALUE): %S"
          query)))))

(defun dorg-ql--normalize-operator-expr (expr query)
  "Normalize prefix-operator operand EXPR from QUERY into an operand plist."
  (unless (and (consp expr) (symbolp (car expr)) (proper-list-p (cdr expr)))
    (dorg-ql--error "operator operand must be (field F), (property K), (keyword K), or (tag): %S"
                    query))
  (pcase (car expr)
    ('field
     (unless (= (length (cdr expr)) 1)
       (dorg-ql--error "(field FIELD) is the operator operand form: %S" query))
     (let ((field (cadr expr)))
       (unless (assq field dorg-ql--field-operators)
         (dorg-ql--error "unknown field `%S'; expected one of %S"
                         field (mapcar #'car dorg-ql--field-operators)))
       (list :field field)))
    ('property
     (unless (and (= (length (cdr expr)) 1) (stringp (cadr expr)))
       (dorg-ql--error "(property KEY) needs one string KEY: %S" query))
     (list :property (cadr expr)))
    ('keyword
     (unless (and (= (length (cdr expr)) 1) (stringp (cadr expr)))
       (dorg-ql--error "(keyword KEY) needs one string KEY: %S" query))
     (list :keyword (cadr expr)))
    ('tag
     (unless (null (cdr expr))
       (dorg-ql--error "(tag) takes no arguments as an operator operand: %S"
                       query))
     (list :tag))
    (_ (dorg-ql--error
        "operator operand must be (field F), (property K), (keyword K), or (tag): %S"
        query))))

(defun dorg-ql--normalize-prefix-operator (op args query)
  "Normalize `(OP EXPR VALUE)' from QUERY into an AST leaf."
  (unless (= (length args) 2)
    (dorg-ql--error "(%s EXPR VALUE) takes two arguments: %S" op query))
  (let ((target (dorg-ql--normalize-operator-expr (nth 0 args) query))
        (value (nth 1 args)))
    (pcase target
      (`(:field ,field)
       (dorg-ql--normalize-field-comparison field op value query))
      (`(:property ,key)
       (dorg-ql--check-operator op dorg-ql--property-operators "property")
       (unless (stringp value)
         (dorg-ql--error "property VALUE must be a string: %S" value))
       (list :property key op value))
      (`(:keyword ,key)
       (dorg-ql--check-operator op dorg-ql--keyword-operators "keyword")
       (unless (stringp value)
         (dorg-ql--error "keyword VALUE must be a string: %S" value))
       (list :keyword key op value))
      ('(:tag)
       (dorg-ql--check-operator op dorg-ql--tag-operators "tag")
       (unless (stringp value)
         (dorg-ql--error "tag VALUE must be a string: %S" value))
       (list :tag op value)))))

(defun dorg-ql--normalize-existence-operator (op args query)
  "Normalize `(exists EXPR)' or `(missing EXPR)' from QUERY into an AST leaf."
  (unless (= (length args) 1)
    (dorg-ql--error "(%s EXPR) takes one argument: %S" op query))
  (pcase (dorg-ql--normalize-operator-expr (car args) query)
    (`(:property ,key) (list :property key op nil))
    (`(:keyword ,key) (list :keyword key op nil))
    ('(:tag) (list :tag op nil))
    (`(:field ,field)
     (dorg-ql--error "%s is not supported for field %s: %S" op field query))))

(defun dorg-ql--normalize-timestamp-source (source query)
  "Validate and return the canonical timestamp SOURCE for QUERY."
  (cond
   ((memq source dorg-ql--timestamp-sources) source)
   ((and (consp source) (eq (car source) :property) (= (length source) 2))
    (let ((key (nth 1 source)))
      (unless (or (stringp key) (eq key '*))
        (dorg-ql--error
         "timestamp (:property KEY) needs a string key or *: %S" query))
      (list :property key)))
   (t (dorg-ql--error
       "unknown timestamp source %S; expected one of %S or (:property KEY|*)"
       source dorg-ql--timestamp-sources))))

(defun dorg-ql--normalize-timestamp-form (args query)
  "Normalize a `(timestamp SOURCE REL WINDOW)' form ARGS into an AST node."
  (unless (= (length args) 3)
    (dorg-ql--error
     "(timestamp SOURCE REL WINDOW) takes three arguments: %S" query))
  (let ((source (dorg-ql--normalize-timestamp-source (nth 0 args) query))
        (rel (dorg-ql--check-operator (nth 1 args)
                                      dorg-ql--timestamp-relations "timestamp"))
        (span (dorg-ql--ast-normalize-window (nth 2 args))))
    (list :timestamp source rel span)))

(defun dorg-ql--normalize-ts-alias (source head spec query)
  "Lower a timestamp alias to a `(:timestamp SOURCE :overlaps (START . END))' node.
SPEC is the alias's window argument list --- either explicit `(FROM TO)'
bounds or a single convenience window form such as `((today))' or
`((on \"2026-07-01\"))'.  Both shapes lower through the shared
`dorg-ql--normalize-window-spec' (core), so the alias accepts
exactly the window syntax that surface accepts.  HEAD names the
alias and QUERY is the original form, both for error messages."
  (let ((span (dorg-ql--normalize-window-spec head spec query)))
    (list :timestamp source :overlaps
          (cons (nth 0 span) (nth 1 span)))))

(defun dorg-ql--normalize-ts-alias-named (head args query)
  "Lower a `window-name' timestamp alias (property-ts) to a `:timestamp' node.
ARGS is `(FROM TO [NAME])' or `(WINDOW-FORM [NAME])'; NAME is optional and,
when present, must be a string naming the property timestamp source (nil
selects the `*' wildcard).  HEAD names the alias and QUERY is the original
form, both for error messages.  This is the metadata-driven counterpart of
`dorg-ql--normalize-ts-alias' for the sole `window-name' predicate."
  (let (spec name)
    (cond
     ((and args (dorg-ql--window-form-p (car args)))
      (unless (memq (length args) '(1 2))
        (dorg-ql--error
         "(%s WINDOW-FORM [NAME]) takes a window form then optional NAME: %S"
         head query))
      (setq spec (list (car args))
            name (nth 1 args)))
     (t
      (unless (memq (length args) '(2 3))
        (dorg-ql--error
         "(%s FROM TO [NAME]) takes two or three arguments: %S" head query))
      (setq spec (list (nth 0 args) (nth 1 args))
            name (nth 2 args))))
    (when (and name (not (stringp name)))
      (dorg-ql--error "%s NAME must be a string: %S" head name))
    (dorg-ql--normalize-ts-alias
     (list :property (if name name '*)) head spec query)))

(defun dorg-ql--normalize-predicate (query)
  "Normalize a predicate/combinator/alias QUERY into a filter EXPR node.
Pipeline steps (`pipe', `where', `traverse', `unique') are not predicates
and signal `dorg-ql-error' in predicate position."
  (unless (and (consp query) (symbolp (car query)))
    (dorg-ql--error "predicate must be a list form (OP ARGS...): %S" query))
  (let ((head (car query))
        (args (cdr query)))
    (unless (proper-list-p args)
      (dorg-ql--error "predicate arguments must form a proper list: %S" query))
    (pcase head
      ;; Boolean combinators.
      ('and
       (when (null args)
         (dorg-ql--error "(and PREDICATE...) requires at least one predicate"))
       (cons :and (mapcar #'dorg-ql--normalize-predicate args)))
      ('or
       (when (null args)
         (dorg-ql--error "(or PREDICATE...) requires at least one predicate"))
       (cons :or (mapcar #'dorg-ql--normalize-predicate args)))
      ('not
       (unless (= (length args) 1)
         (dorg-ql--error "(not PREDICATE) takes exactly one predicate: %S" query))
       (list :not (dorg-ql--normalize-predicate (car args))))
      ;; v2 predicates.
      ('field     (dorg-ql--normalize-field args query))
      ('id        (dorg-ql--normalize-id-form args query))
      ('property  (dorg-ql--normalize-property-form args query))
      ('keyword   (dorg-ql--normalize-keyword-form args query))
      ('tag       (dorg-ql--normalize-tag-form args query))
      ('timestamp (dorg-ql--normalize-timestamp-form args query))
      ((guard (memq head dorg-ql--prefix-operators))
       (dorg-ql--normalize-prefix-operator head args query))
      ((or 'exists 'missing)
       (dorg-ql--normalize-existence-operator head args query))
      ;; Atomic alias: title.
      ('title
       (unless (and (= (length args) 1) (stringp (car args)))
         (dorg-ql--error "(title TITLE) takes one string argument: %S" query))
       (list :cmp 'title '= (car args)))
      ;; Pipeline steps are not predicates.
      ((or 'pipe 'where 'traverse 'unique)
       (dorg-ql--error
        "`%s' is a pipeline step, not a predicate: %S" head query))
      ;; Atomic timestamp aliases lower into the v2 :timestamp node.  The
      ;; alias set and its per-alias arity are DERIVED from the single
      ;; timestamp metadata table (`dorg-ql--timestamp-predicate-metadata',
      ;; core), so the normalizer shares ONE vocabulary with the capability
      ;; gate rather than hard-coding a parallel predicate list.  A `window'
      ;; alias takes `(HEAD FROM TO)' bounds or a single `(HEAD WINDOW-FORM)';
      ;; the sole `window-name' alias (`property-ts') additionally takes an
      ;; optional trailing NAME.  An unknown head falls through to the
      ;; "unknown predicate" error.
      (_
       (let ((meta (dorg-ql--timestamp-metadata head)))
         (if (null meta)
             (dorg-ql--error "unknown predicate `%S': %S" head query)
           (pcase (plist-get meta :arity)
             ('window      (dorg-ql--normalize-ts-alias head head args query))
             ('window-name (dorg-ql--normalize-ts-alias-named head args query))
             (other (error "dorg-ql: unknown predicate arity %S for `%s'"
                           other head)))))))))

;;; ─── Pipeline step normalization ──────────────────────────────────────

(defun dorg-ql--normalize-step (step)
  "Normalize one pipeline STEP into its AST form.
A step is `(where PREDICATE)', `(traverse RELATION...)', or `(unique)'."
  (unless (and (consp step) (symbolp (car step)))
    (dorg-ql--error "pipeline step must be a list form: %S" step))
  (let ((head (car step))
        (args (cdr step)))
    (unless (proper-list-p args)
      (dorg-ql--error "pipeline step arguments must form a proper list: %S" step))
    (pcase head
      ('where
       (unless (= (length args) 1)
         (dorg-ql--error "(where PREDICATE) takes exactly one predicate: %S" step))
       (list :filter (dorg-ql--normalize-predicate (car args))))
      ('traverse
       (when (null args)
         (dorg-ql--error "(traverse RELATION...) requires at least one relation"))
       (dolist (rel args)
         (unless (memq rel dorg-ql--traverse-relations)
           (dorg-ql--error "unknown traverse relation `%S'; expected one of %S"
                           rel dorg-ql--traverse-relations)))
       (cons :traverse args))
      ('unique
       (unless (null args)
         (dorg-ql--error "(unique) takes no arguments: %S" step))
       (list :unique))
      (_ (dorg-ql--error
          "unknown pipeline step `%S'; expected where, traverse, or unique: %S"
          head step)))))

;;; ─── Public entry point ───────────────────────────────────────────────

;;;###autoload
(defun dorg-ql-normalize (query)
  "Normalize a public dorg-ql QUERY into the backend-neutral v2 AST.

QUERY may be a composable pipeline `(pipe STEP...)', any v2 predicate or
boolean combinator, or one of the historical atomic compatibility aliases
\(`id', `property', `title', `tag', `scheduled', `deadline', `closed',
`active-ts', `logbook', `property-ts').  A top-level predicate P is treated
as `(pipe (where P))' over all headings in the source.

Returns `(:pipe (:source :all) STEP...)'.  Invalid arity, unknown fields,
operators, or predicates, and malformed reader forms signal
`dorg-ql-error' / `invalid-query'; normalization never widens to a broad scan."
  (unless (and (consp query) (symbolp (car query)))
    (dorg-ql--error "query must be a list form (OP ARGS...): %S" query))
  (pcase (car query)
    ('pipe
     (let ((steps (cdr query)))
       (unless (proper-list-p steps)
         (dorg-ql--error "(pipe STEP...) arguments must form a proper list: %S"
                         query))
       (when (null steps)
         (dorg-ql--error "(pipe STEP...) requires at least one step: %S" query))
       (cons :pipe
             (cons '(:source :all)
                   (mapcar #'dorg-ql--normalize-step steps)))))
    ('one
     ;; `(one QUERY)' — the public cardinality WRAPPER (docs/mutate.org,
     ;; docs/api.org "Grammar resolution").  Normalize the inner query and
     ;; append a terminal `(:one)' assertion step to its pipe.  A single
     ;; spelling shared by read and mutate; NOT a bare pipeline step.
     (let ((args (cdr query)))
       (unless (and (proper-list-p args) (= (length args) 1))
         (dorg-ql--error "(one QUERY) takes exactly one query: %S" query))
       (append (dorg-ql-normalize (car args)) (list '(:one)))))
    ((or 'where 'traverse 'unique)
     (dorg-ql--error
      "`%s' is a pipeline step; wrap it in (pipe ...) at top level: %S"
      (car query) query))
    (_
     (list :pipe '(:source :all)
           (list :filter (dorg-ql--normalize-predicate query))))))

(provide 'dorg-ql-ast)
;;; dorg-ql-ast.el ends here
