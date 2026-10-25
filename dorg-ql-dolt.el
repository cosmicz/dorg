;;; dorg-ql-dolt.el --- Dolt SQL adapter for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Dolt adapter lowers a validated atomic query to SQL over the
;; registered root_id and never queries across roots.  It stays boring
;; and explicit: one selector per source kind, candidate-bounded, with
;; every date-window decision kept in Elisp through the shared window
;; predicate `dorg-ql--timestamp-in-window-p'.
;;
;; No selector materializes the whole tree, calls
;; `dorg-model-load-headings', issues SQL date filtering, or opens a
;; direct SQL write path.  Only sound structural narrowing — root scope,
;; whitelisted planning columns, the LOGBOOK drawer-name filter, and the
;; `LIKE '%<%'' active-timestamp prefilter — is pushed into SQL; matching
;; semantics mirror the model adapter via the shared scanners in
;; `dorg-model' and the one timestamp parser in `dorg-timestamp'.

;;; Code:

(require 'cl-lib)
(require 'dorg-backend)
(require 'dorg-dolt)
(require 'dorg-ext)
(require 'dorg-ext-read)
(require 'dorg-bm25)
(require 'dorg-model-load)               ; hydration loaders (tracked-case)
(require 'dorg-registry)
(require 'dorg-model)
(require 'dorg-ql-core)
;; The v2 planner reuses the neutral evaluation core (matching/traversal/
;; order helpers in `dorg-ql-eval') plus the model adapter's headline-leaf
;; matcher, so the Dolt and model baselines converge by construction.  Both
;; are required before this file and neither requires the Dolt adapter, so
;; this is a one-way dependency.
(require 'dorg-ql-eval)
(require 'dorg-ql-model)

(defun dorg-ql--dolt-query (dolt-dir revision query &rest args)
  "Run QUERY in DOLT-DIR at captured REVISION, formatting it with ARGS.
REVISION is mandatory for public QL reads.  A nil value remains available to
internal legacy callers while revision threading is completed, and uses the
ordinary live-head reader."
  (if revision
      (apply #'dorg-sql--at-revision dolt-dir revision query args)
    (apply #'dorg-sql dolt-dir query args)))

(defun dorg-ql--dolt-pushable-filter-p (expr)
  "Non-nil when normalized filter EXPR is wholly SQL-pushable.
This is an all-or-nothing classifier: unsupported leaves keep the existing
residual evaluator rather than partially narrowing and hydrating a root."
  (pcase expr
    (`(:and . ,xs) (and xs (cl-every #'dorg-ql--dolt-pushable-filter-p xs)))
    (`(:or . ,xs) (and xs (cl-every #'dorg-ql--dolt-pushable-filter-p xs)))
    (`(:not ,x) (dorg-ql--dolt-pushable-filter-p x))
    (`(:id ,(or '= '!= '< '<= '> '>=) ,(pred integerp)) t)
    (`(:cmp todo ,(or '= '!=) ,(pred stringp) :case sensitive) t)
    (`(:cmp title ,(or '= '!= 'contains 'prefix 'suffix) ,(pred stringp)
            :case sensitive)
     t)
    (`(:cmp ,field ,_ ,_ :case sensitive)
     (let ((descriptor (dorg-ql--query-field field)))
       (and (eq (plist-get descriptor :cardinality) 'one)
            (plist-get descriptor :sql))))
    ;; Non-string scalar fields never carry `:case'.  This is not an old AST
    ;; compatibility arm: canonical string fields must match the case-tagged
    ;; clause above, while boolean/integer/identity extension fields retain
    ;; their established typed shape.
    (`(:cmp ,field ,_ ,_)
     (let ((descriptor (dorg-ql--query-field field)))
       (and (not (eq (plist-get descriptor :type) 'string))
            (eq (plist-get descriptor :cardinality) 'one)
            (plist-get descriptor :sql))))
    (`(:property ,(pred stringp) ,(or 'exists 'missing) nil) t)
    (`(:extension-member ,(pred symbolp) ,(pred symbolp)
                         ,(pred integerp) ,(and values (pred listp)))
     (and (> (nth 3 expr) 0)
          values
          (cl-every #'stringp values)
          (dorg-ext-query-membership (nth 1 expr) (nth 2 expr))))
    (_ nil)))

(defun dorg-ql--dolt-selection-filters (ast classifier)
  "Return AST's filter expressions when its selection fits CLASSIFIER.
The selection may hold only WHERE steps, each accepted by CLASSIFIER, and a
terminal `:one' assertion.  Returns `(t . FILTERS)' with FILTERS in step
order, or nil; traversals and other residual steps never fit."
  (let ((steps (and (consp ast) (eq (car ast) :pipe)
                    (cddr (dorg-ql--selection-ast ast))))
        filters)
    (when (cl-every
           (lambda (step)
             (pcase step
               (`(:filter ,expr)
                (and (funcall classifier expr) (push expr filters)))
               (`(:one) t)
               (_ nil)))
           steps)
      (cons t (nreverse filters)))))

(defun dorg-ql--dolt-conjunction (filters)
  "Return the one expression equivalent to the conjunction of FILTERS."
  (if (cdr filters) (cons :and filters) (car filters)))

(defun dorg-ql--dolt-pushdown-filter (ast)
  "Return the wholly pushable filter conjunction in AST, or nil.
Projection may follow several WHERE steps and a terminal `:one' assertion;
their selection remains one SQL read, with cardinality enforced before row
projection.  Traversals and other residual steps remain out of this path."
  (let ((selection (dorg-ql--dolt-selection-filters
                    ast #'dorg-ql--dolt-pushable-filter-p)))
    (when selection
      (if (cdr selection)
          (dorg-ql--dolt-conjunction (cdr selection))
        (and (dorg-ql--projection-columns ast) '(:true))))))

;; Insensitive string leaves cannot be lowered to SQL: `standard-case-table'
;; maps ß to ẞ and ǆ to Ǆ and leaves ſ, ı and the Kelvin sign alone, and no
;; SQL UPPER/LOWER has been shown to agree with it.  When such a leaf reads a
;; stored scalar heading column, the selection is still answered without
;; heading models: one root-scoped statement returns each heading's id, that
;; column and one flag per SQL-pushable leaf; the ORIGINAL boolean tree is
;; evaluated per row in Elisp with the shared leaf evaluator; and the matching
;; ids are projected by the ordinary pushdown select.  Both statements read
;; the captured revision.
;;
;; The scan reads the headings row and nothing else, except the one
;; uncorrelated, cacheable property-existence subquery.  A tree holding any
;; other pushable leaf (an extension field, which may need a declared join,
;; or a membership) is not scanned and keeps its residual behaviour.

(defconst dorg-ql--dolt-scanned-columns '((title . "h.title"))
  "Stored scalar heading string columns whose insensitive leaves are scanned.
Each entry is (FIELD . SQL-EXPRESSION) over the `headings' alias `h'.  A
further column is one entry here plus its parity rows; only title is
registered.")

(defun dorg-ql--dolt-scanned-leaf-p (expr)
  "Non-nil when EXPR is an insensitive string leaf over a scanned column."
  (pcase expr
    (`(:cmp ,field ,(or '= '!= 'contains 'prefix 'suffix) ,(pred stringp)
            :case insensitive)
     (and (assq field dorg-ql--dolt-scanned-columns) t))))

(defun dorg-ql--dolt-scan-flag-leaf-p (expr)
  "Non-nil when pushable leaf EXPR is a flag the candidate scan may read.
Flags read only the headings row (id, TODO keyword, sensitive title) or the
property-existence subquery; extension fields and memberships never qualify."
  (and (dorg-ql--dolt-pushable-filter-p expr)
       (pcase expr
         (`(:id . ,_) t)
         (`(:cmp ,(or 'todo 'title) . ,_) t)
         (`(:property ,_ ,(or 'exists 'missing) nil) t))))

(defun dorg-ql--dolt-scannable-filter-p (expr)
  "Non-nil when every leaf of EXPR is a scan flag or a scanned leaf."
  (pcase expr
    (`(:and . ,xs) (and xs (cl-every #'dorg-ql--dolt-scannable-filter-p xs)))
    (`(:or . ,xs) (and xs (cl-every #'dorg-ql--dolt-scannable-filter-p xs)))
    (`(:not ,x) (dorg-ql--dolt-scannable-filter-p x))
    (_ (or (dorg-ql--dolt-scanned-leaf-p expr)
           (dorg-ql--dolt-scan-flag-leaf-p expr)))))

(defun dorg-ql--dolt-scan-filter (ast)
  "Return AST's selection expression for the column scan, or nil.
Non-nil only when the selection is not wholly pushable but every leaf is a
scan flag or a scanned leaf, so a scan answers it without heading models."
  (unless (dorg-ql--dolt-pushdown-filter ast)
    (let ((selection (dorg-ql--dolt-selection-filters
                      ast #'dorg-ql--dolt-scannable-filter-p)))
      (and (cdr selection) (dorg-ql--dolt-conjunction (cdr selection))))))

(defun dorg-ql--dolt-sql-selection-p (ast)
  "Non-nil when AST's selection needs no residual evaluator."
  (or (dorg-ql--dolt-pushdown-filter ast) (dorg-ql--dolt-scan-filter ast)))

(defun dorg-ql--dolt-extension-member-p (node)
  "Non-nil when NODE contains an `:extension-member' leaf."
  (pcase node
    (`(:extension-member . ,_) t)
    ((pred consp) (cl-some #'dorg-ql--dolt-extension-member-p node))
    (_ nil)))

(defun dorg-ql--dolt-first-residual-node (node)
  "Return NODE's first leaf or pipeline step unsupported by SQL pushdown."
  (pcase node
    (`(:and . ,xs)
     (cl-some #'dorg-ql--dolt-first-residual-node xs))
    (`(:or . ,xs)
     (cl-some #'dorg-ql--dolt-first-residual-node xs))
    (`(:not ,x) (dorg-ql--dolt-first-residual-node x))
    (`(:filter ,x) (dorg-ql--dolt-first-residual-node x))
    ((and (pred consp) (guard (dorg-ql--dolt-pushable-filter-p node))) nil)
    ((pred consp) node)
    (_ nil)))

(eval-and-compile
  ;; The macro below validates KIND against this list at expansion time, so
  ;; the list must exist while this file is being compiled, not only after
  ;; it is loaded.
  (defconst dorg-ql--dolt-statement-kinds
    '(candidate-scan pushdown-select pushdown-properties by-id-materialization)
    "Closed revision-query vocabulary annotated onto `dorg-sql-lease-broken'.
This lives beside its only user.  The SQL layer raises the condition; naming
which QL statement was in flight is QL vocabulary, and pushing it down into
the transport would make the lower layer depend on the upper one."))

(defmacro dorg-ql--dolt-with-statement-kind (kind &rest body)
  "Run BODY, annotating a broken lease with the statement KIND.
A `dorg-sql-lease-broken' raised inside BODY is re-signalled with
`:statement-kind' KIND APPENDED to its data, leaving the historical
human-readable message first and `:cause-class' after it, so existing
consumers read exactly what they read before.  Any other error propagates
untouched.

KIND is checked against `dorg-ql--dolt-statement-kinds' at macro-expansion
time, so a typo is a compile-time failure rather than a silently unlabelled
statement."
  (declare (indent 1) (debug (symbolp body)))
  (unless (memq kind dorg-ql--dolt-statement-kinds)
    (error "Unknown QL statement kind: %S" kind))
  `(condition-case dorg-ql--dolt-lease-err
       (progn ,@body)
     (dorg-sql-lease-broken
      (signal 'dorg-sql-lease-broken
              (append (cdr dorg-ql--dolt-lease-err)
                      (list :statement-kind ',kind))))))

(defun dorg-ql--dolt-validate-extension-memberships (ast)
  "Validate every extension-membership leaf in normalized AST.
Signals `dorg-ql-error' before snapshot capture or data SQL when an extension
or relation is not registered.  This prevents an unknown declaration from
falling through to either a broad residual read or an accidental empty set."
  (cl-labels ((walk (node)
                (pcase node
                  (`(:extension-member ,extension ,relation . ,_)
                   (unless (dorg-ext-query-membership extension relation)
                     (dorg-ql--unsupported
                      "extension membership is not registered: %S/%S"
                      extension relation)))
                  ((pred consp) (mapc #'walk node)))))
    (walk ast))
  (let ((fields (dorg-ql--extension-fields ast)))
    (dolist (field fields)
      (let ((descriptor (dorg-ql--query-field field)))
        (unless (plist-get descriptor :sql)
          (dorg-ql--unsupported "field has no SQL lowering: %S" field))
        (when (and (eq (plist-get descriptor :cardinality) 'many)
                   (memq field (dorg-ql--extension-fields
                                (dorg-ql--selection-ast ast))))
          (dorg-ql--unsupported "collection field cannot filter headings: %S" field))))
    (when (and fields (not (dorg-ql--dolt-sql-selection-p ast)))
      (dorg-ql--unsupported "extension fields cannot use this residual plan: %S" fields)))
  (when (and (dorg-ql--dolt-extension-member-p ast)
             (not (dorg-ql--dolt-sql-selection-p ast)))
    (let ((blocker
           (cl-some #'dorg-ql--dolt-first-residual-node (cddr ast))))
      (dorg-ql--unsupported
       "extension membership cannot compose with residual filter: %S"
       blocker)))
  nil)

(defun dorg-ql--dolt-membership-sql (extension relation position values root-id)
  "Lower one declared membership predicate to an UNCORRELATED set membership.
The subquery names no outer row, so the planner may evaluate it once and cache
it rather than re-running it for every joined row; the correlated form this
replaced is the producer mechanism behind roster read-idle timeouts.  Root,
position, the binary-lowered value comparison and SQL quoting are unchanged.

Heading ids are guarded IS NOT NULL.  On this leg the caller\='s COALESCE
already collapses UNKNOWN before any enclosing NOT, so the guard is defence in
depth rather than load-bearing: it stops the shape depending on that wrapper."
  (let ((descriptor (dorg-ext-query-membership extension relation)))
    (unless descriptor
      (dorg-ql--unsupported
       "extension membership is not registered: %S/%S" extension relation))
    (unless (and (integerp position) (> position 0)
                 (consp values) (cl-every #'stringp values))
      (dorg-ql--error "invalid extension membership predicate"))
    (let ((table (plist-get descriptor :table))
          (member-key (plist-get descriptor :member-key))
          (position-column (plist-get descriptor :position-column))
          (value-column (plist-get descriptor :value-column))
          (bridge-table (plist-get descriptor :bridge-table))
          (bridge-key (plist-get descriptor :bridge-key))
          (heading-column (plist-get descriptor :heading-column))
          (root-column (plist-get descriptor :root-column)))
      (format
       (concat "h.id IN (SELECT eb.%s FROM %s em"
               " JOIN %s eb ON eb.%s = em.%s"
               " WHERE eb.%s = %d"
               " AND em.%s = %d AND BINARY LOWER(em.%s) IN (%s)"
               " AND eb.%s IS NOT NULL)")
       heading-column table bridge-table bridge-key member-key
       root-column root-id
       position-column position value-column
       (mapconcat (lambda (value)
                    (format "BINARY LOWER(%s)" (dorg-sql-quote value)))
                  values ", ")
       heading-column))))

(defun dorg-ql--dolt-filter-sql (expr root-id)
  "Return SQL predicate for wholly pushable normalized EXPR at ROOT-ID."
  (pcase expr
    (`(:true) "TRUE")
    (`(:and . ,xs)
     (format "(%s)" (mapconcat (lambda (x)
                                  (dorg-ql--dolt-filter-sql x root-id))
                                xs " AND ")))
    (`(:or . ,xs)
     (format "(%s)" (mapconcat (lambda (x)
                                  (dorg-ql--dolt-filter-sql x root-id))
                                xs " OR ")))
    (`(:not ,x)
     (format "NOT (%s)" (dorg-ql--dolt-filter-sql x root-id)))
    (`(:id ,op ,value)
     (format "h.id %s %d" (if (eq op '!=) "<>" (symbol-name op)) value))
    (`(:cmp todo ,op ,value :case sensitive)
     ;; SQL comparisons over nullable TODO are three-valued, while the shared
     ;; Elisp leaf matcher is two-valued (nil never matches).  Collapse UNKNOWN
     ;; at the leaf boundary so boolean NOT/AND/OR preserve residual semantics.
     (format "COALESCE((h.todo_keyword %s %s), FALSE)"
             (if (eq op '=) "=" "<>") (dorg-sql-quote value)))
    (`(:cmp title ,op ,value :case sensitive)
     ;; TITLE is a DOrg core field, not an extension descriptor.  Rank uses
     ;; this same direct stored-column comparison to constrain candidates
     ;; before its postings aggregate and top-k bound.
     (dorg-ql--dolt-exact-string-sql "h.title" op value))
    (`(:cmp ,field ,op ,value :case sensitive)
     (dorg-ql--dolt-field-comparison field op value root-id))
    (`(:cmp ,field ,op ,value)
     (dorg-ql--dolt-field-comparison field op value root-id))
    (`(:property ,key ,op nil)
     ;; Uncorrelated set: the subquery references no outer row, so the planner
     ;; may evaluate it once and cache it instead of re-running it per joined
     ;; row.  The IS NOT NULL guard is what keeps NOT IN safe by construction
     ;; rather than by a schema invariant reachable only through the property
     ;; view definition: a single NULL in the set makes NOT IN UNKNOWN for
     ;; EVERY row, and the COALESCE below would then turn a `missing\=' query
     ;; into zero rows with no error at all.
     (format (concat "COALESCE((h.id %sIN (SELECT hp.heading_id"
                     " FROM heading_properties hp"
                     " WHERE hp.prop_key = %s"
                     " AND hp.heading_id IS NOT NULL))"
                     ", FALSE)")
             (if (eq op 'missing) "NOT " "") (dorg-sql-quote key)))
    (`(:extension-member ,extension ,relation ,position ,values)
     (format "COALESCE((%s), FALSE)"
             (dorg-ql--dolt-membership-sql
              extension relation position values root-id)))
    (_ (dorg-ql--unsupported "filter is not wholly SQL-pushable: %S" expr))))

(defun dorg-ql--dolt-exact-string-sql (expression op value)
  "Return a two-valued SQL test of string EXPRESSION OP literal VALUE.
The result equals `dorg-ql--apply-string-op' in sensitive mode for every
valid UTF-8 value.  It is built from operations measured to be exact on Dolt
rather than from their MySQL documentation:

- `LOCATE' and `POSITION' fold case even over BINARY operands (`a' is found in
  \"XA\", `ß' in \"ẞ\", `k' in the Kelvin sign), so containment uses `INSTR',
  which does not.  No `LIKE' is used, so `%', `_' and backslash are literal.
- `LEFT' and `RIGHT' count characters even on BINARY values, so prefix and
  suffix take VALUE's code-point count, computed here; `CHAR_LENGTH' is not
  applied to the literal because Dolt rejects one containing U+FFFD.
- Comparisons are BINARY, so there is no case folding and no pad-space rule.

Every test is wrapped in COALESCE(..., FALSE): a NULL value never matches,
and NOT/AND/OR keep the shared evaluator's two-valued semantics."
  (let ((literal (dorg-sql-quote value)))
    (format "COALESCE((%s), FALSE)"
            (pcase op
              ('= (format "BINARY %s = BINARY %s" expression literal))
              ('!= (format "BINARY %s <> BINARY %s" expression literal))
              ('contains (format "INSTR(BINARY %s, BINARY %s) > 0" expression literal))
              ('prefix (format "BINARY LEFT(%s, %d) = BINARY %s"
                               expression (length value) literal))
              ('suffix (format "BINARY RIGHT(%s, %d) = BINARY %s"
                               expression (length value) literal))
              (_ (dorg-ql--unsupported "no exact SQL for string operator %S" op))))))

(defconst dorg-ql--dolt-field-sql
  '((id . "h.id") (title . "h.title") (todo . "h.todo_keyword")
    (priority . "h.priority") (commented . "h.commented")
    (parent-id . "h.parent_id") (level . "q.level"))
  "SQL expressions for stored heading fields in an ordered query.")

(defvar dorg-ql--dolt-field-plans nil
  "Dynamically bound field plans for one synchronous SQL statement compilation.
The SQL builder binds this while its filter, select, and joins share lowering
work; nested compilation is stack-scoped and unwind restores the caller.  It
is not a persistent cache or a second field registry.")

(defun dorg-ql--dolt-field-plan (field root-id)
  "Return FIELD's scalar plan at ROOT-ID, compiling once per statement."
  (or (cdr (assq field dorg-ql--dolt-field-plans))
      (let* ((descriptor (dorg-ql--query-field field))
             (lower (plist-get descriptor :sql)))
        (unless lower (dorg-ql--unsupported "no SQL lowering for field %S" field))
        (let ((plan (funcall lower (dorg-ql--field-context descriptor)
                             (list :heading "h" :root root-id))))
          (unless (or (stringp (plist-get plan :expression))
                      (functionp (plist-get plan :batch)))
            (dorg-ql--unsupported "field %S returned no SQL expression or batch" field))
          (push (cons field plan) dorg-ql--dolt-field-plans)
          plan))))

(defun dorg-ql--dolt-field-comparison (field op value root-id)
  "Lower a typed FIELD comparison using its shared scalar expression.
String containment, prefix and suffix use `dorg-ql--dolt-exact-string-sql',
the same exact lowering as the title column; every other comparison keeps
its typed binary, numeric or boolean form."
  (let* ((descriptor (dorg-ql--query-field field))
         (type (plist-get descriptor :type))
         (plan (dorg-ql--dolt-field-plan field root-id))
         (sql (plist-get plan :expression))
         (literal (pcase type
                    ('integer (number-to-string value))
                    ('boolean (if (eq value t) "TRUE" "FALSE"))
                    (_ (dorg-sql-quote value))))
         (operator (if (eq op '!=) "<>" (symbol-name op))))
    (pcase type
      ('identity
       (setq sql (plist-get plan :key-expression))
       (unless (stringp sql)
         (dorg-ql--unsupported "identity field %S needs a comparison key" field))
       (setq sql (concat "BINARY " sql)
             literal (concat "BINARY " (dorg-sql-quote (dorg-query-identity-key value)))))
      ((or 'string 'timestamp)
       (setq sql (concat "BINARY " sql) literal (concat "BINARY " literal)))
      ('boolean (setq literal (if (eq value t) "TRUE" "FALSE"))))
    (if (and (eq type 'string) (memq op '(contains prefix suffix)))
        (dorg-ql--dolt-exact-string-sql (plist-get plan :expression) op value)
      (format "COALESCE((%s %s %s), FALSE)" sql operator literal))))

(defun dorg-ql--dolt-field-joins ()
  "Return deduplicated named joins for the current field plans."
  (let (joins)
    (dolist (entry (reverse dorg-ql--dolt-field-plans))
      (dolist (join (plist-get (cdr entry) :joins))
        (unless (and (consp join) (symbolp (car join)) (stringp (cdr join)))
          (dorg-ql--unsupported "invalid named field join: %S" join))
        (let ((existing (assq (car join) joins)))
          (when (and existing (not (equal (cdr existing) (cdr join))))
            (dorg-ql--unsupported "conflicting field join: %S" (car join)))
          (unless existing (push join joins)))))
    (mapconcat #'cdr (nreverse joins) " ")))

(defun dorg-ql--dolt-projection-select-list (columns root-id &optional level-expression)
  "Return scalar SQL selections for COLUMNS, with private positional aliases."
  ;; This must be a fresh list: `nreverse' destructively reverses its input.
  ;; A quoted literal made one process's first projection corrupt every later
  ;; library/session/serve projection.
  (let ((index 0) (selections (list "h.id AS id")))
    (dolist (column columns)
      (pcase (nth 2 column)
        (`(:field ,field)
         (unless (eq (plist-get (dorg-ql--query-field field) :cardinality) 'many)
             (let ((sql (if (eq field 'level)
                            (or level-expression "q.level")
                          (or (alist-get field dorg-ql--dolt-field-sql)
                              (plist-get (dorg-ql--dolt-field-plan field root-id)
                                         :expression)))))
             (unless sql (dorg-ql--unsupported "no scalar SQL lowering: %S" field))
             (push (format "%s AS c%d" sql index) selections)))))
      (cl-incf index))
    (mapconcat #'identity (nreverse selections) ", ")))

(defun dorg-ql--dolt-pushdown-select-sql (expr root-id &optional columns)
  "Return one root-scoped ordered selector SQL statement for EXPR.
EXPR must be wholly pushable.  The recursive tree preserves document order
while filtering in SQL and returns only selected ids/titles for bounded result
hydration.  The CHAR(4096) anchor cast seeds the recursive CTE's path type; it
is not a product depth cap, because Dolt preserves wider recursive `CONCAT'
results.  Ordering is explicitly binary so connection collation cannot ignore
the slash separator or case-fold the hexadecimal path."
  (unless (or (equal expr '(:true)) (dorg-ql--dolt-pushable-filter-p expr))
    ;; Distinguish an unknown declared membership from an ordinary residual
    ;; expression: callers must never turn the former into an empty/broad read.
    (pcase expr
      (`(:extension-member ,extension ,relation . ,_)
       (unless (dorg-ext-query-membership extension relation)
         (dorg-ql--unsupported
          "extension membership is not registered: %S/%S" extension relation))))
    (dorg-ql--unsupported "filter is not wholly SQL-pushable: %S" expr))
  (dorg-ql--dolt-ordered-select-sql
   (lambda () (dorg-ql--dolt-filter-sql expr root-id)) root-id columns))

(defun dorg-ql--dolt-ordered-select-sql (predicate-fn root-id &optional columns)
  "Return the ROOT-ID document-order selector for PREDICATE-FN's SQL.
PREDICATE-FN returns the WHERE predicate over the `headings' alias `h'; it
runs while this statement's field plans are bound, so its field joins are
emitted once beside those of the selected COLUMNS."
  (let* ((dorg-ql--dolt-field-plans nil)
         (select (if columns (dorg-ql--dolt-projection-select-list columns root-id)
                   "h.id AS id, h.title AS title"))
         (predicate (funcall predicate-fn))
         (joins (dorg-ql--dolt-field-joins)))
    (format
   (concat
    "WITH RECURSIVE qtree (id, parent_id, sort_key, path, level) AS ("
    " SELECT id, parent_id, sort_key, CAST(HEX(sort_key) AS CHAR(4096)), 0"
    " FROM headings WHERE id = %d AND root_id = %d"
    " UNION ALL"
    " SELECT h.id, h.parent_id, h.sort_key,"
    " CONCAT(q.path, '/', HEX(h.sort_key)), q.level + h.level_offset"
    " FROM headings h JOIN qtree q ON h.parent_id = q.id"
    " WHERE h.root_id = %d)"
    " SELECT %s"
    " FROM qtree q JOIN headings h ON h.id = q.id %s"
    " WHERE h.id <> %d AND %s ORDER BY BINARY q.path")
   root-id root-id root-id
   select joins
   root-id
   predicate)))

(defun dorg-ql--dolt-scan-plan (expr root-id)
  "Return (SQL . FLAGS) for the root-scoped column scan answering EXPR.
SQL selects every non-root heading of ROOT-ID with its id, each registered
scanned column and one flag per distinct SQL-pushable leaf of EXPR.  FLAGS
maps each such leaf to its flag column symbol.  Flags reuse the ordinary
leaf lowering, so a flag means exactly what the pushdown predicate means."
  (let ((dorg-ql--dolt-field-plans nil)
        flags)
    (cl-labels ((walk (node)
                  (pcase node
                    (`(,(or :and :or) . ,xs) (mapc #'walk xs))
                    (`(:not ,x) (walk x))
                    ((guard (dorg-ql--dolt-scanned-leaf-p node)) nil)
                    (_ (unless (assoc node flags)
                         (push (cons node (intern (format "f%d" (length flags))))
                               flags))))))
      (walk expr))
    (setq flags (nreverse flags))
    (let* ((selections
            (append
             (list "h.id AS id")
             (mapcar (lambda (entry) (format "%s AS %s" (cdr entry) (car entry)))
                     dorg-ql--dolt-scanned-columns)
             (mapcar (lambda (flag)
                       (format "%s AS %s" (dorg-ql--dolt-filter-sql (car flag) root-id)
                               (cdr flag)))
                     flags)))
           (joins (dorg-ql--dolt-field-joins)))
      (cons (format "SELECT %s FROM headings h %s WHERE h.root_id = %d AND h.id <> %d"
                    (mapconcat #'identity selections ", ") joins root-id root-id)
            flags))))

(defun dorg-ql--dolt-scan-row-match-p (expr row flags)
  "Return non-nil when scan ROW satisfies EXPR; FLAGS name pushed-leaf columns.
Scanned leaves call the shared string evaluator on the row's column value; a
NULL value never matches, exactly as the model leaf matcher treats nil."
  (dorg-ql--eval-expr-match-p
   expr row
   (lambda (leaf row)
     (if (dorg-ql--dolt-scanned-leaf-p leaf)
         (pcase-let ((`(:cmp ,field ,op ,value :case ,mode) leaf))
           (let ((actual (alist-get field row)))
             (and (stringp actual)
                  (dorg-ql--apply-string-op op actual value mode))))
       (and (member (alist-get (cdr (assoc leaf flags)) row) '(1 t "1")) t)))))

(defun dorg-ql--dolt-scan-select (expr root-id dolt-dir revision &optional columns)
  "Answer scan-filter EXPR in DOLT-DIR at REVISION like a pushdown read.
Stage one scans ROOT-ID's ids, scanned columns and flags; the ORIGINAL
boolean EXPR is evaluated per row.  Stage two projects the matching ids with
the ordinary document-order selector and COLUMNS.  Both stages read the
same captured REVISION.  No matching id issues no second statement."
  (pcase-let* ((`(,sql . ,flags) (dorg-ql--dolt-scan-plan expr root-id))
               (ids (delq nil
                          (mapcar (lambda (row)
                                    (and (dorg-ql--dolt-scan-row-match-p expr row flags)
                                         (alist-get 'id row)))
                                  (dorg-ql--dolt-with-statement-kind candidate-scan
                                    (dorg-ql--dolt-query dolt-dir revision sql))))))
    (when ids
      (dorg-ql--dolt-with-statement-kind pushdown-select
        (dorg-ql--dolt-query
         dolt-dir revision
         (dorg-ql--dolt-ordered-select-sql
          (lambda () (format "h.id IN (%s)" (mapconcat #'number-to-string ids ", ")))
          root-id columns))))))

(defun dorg-ql--dolt-properties-batch (dolt-dir heading-ids &optional revision keys)
  "Return a hash mapping each id in HEADING-IDS to its property alist.
Loads all property rows for HEADING-IDS from DOLT-DIR with a single
set-based query (`heading_id IN (...)'), mirroring the batching used by
`dorg-model-load-headings'.  Each heading's alist preserves PROPERTIES
drawer order via the `ORDER BY heading_id, ordinal' clause, exactly as
the per-row path did with its `ORDER BY ordinal'.

When HEADING-IDS is empty no query is issued (avoiding invalid `IN ()'
SQL) and an empty hash is returned.  Ids absent from the result simply
have no entry, so callers `gethash' nil for property-less headings.

Ids are DB-side integers, so the IN list is built with
`number-to-string' and carries no SQL-injection surface; the assembled
query contains no `%' and is passed to `dorg-sql' with no format args."
  (let ((table (make-hash-table :test 'eql)))
    (when heading-ids
      (let ((rows (dorg-ql--dolt-query
                   dolt-dir
                   revision
                   (format
                    (concat "SELECT heading_id, prop_key, prop_value"
                            " FROM heading_properties"
                            " WHERE heading_id IN (%s)%s"
                            " ORDER BY heading_id, ordinal")
                    (mapconcat #'number-to-string heading-ids ", ")
                    (if keys
                        (format " AND prop_key IN (%s)"
                                (mapconcat #'dorg-sql-quote keys ", "))
                      "")))))
        (dolist (row rows)
          (let ((hid (alist-get 'heading_id row)))
            (puthash hid
                     (append (gethash hid table)
                             (list (cons (alist-get 'prop_key row)
                                         (alist-get 'prop_value row))))
                     table)))))
    table))

(defun dorg-ql--dolt-keywords-batch (dolt-dir heading-ids &optional revision)
  "Return a hash mapping each id in HEADING-IDS to keyword item alists.
Loads keyword rows for the supplied non-root heading ids from DOLT-DIR in
one set-based query.  Root-owned file-level keywords never enter keyword
predicate matching because callers pass only heading candidate ids."
  (let ((table (make-hash-table :test 'eql)))
    (when heading-ids
      (let ((rows (dorg-ql--dolt-query
                   dolt-dir
                   revision
                   (format
                    (concat "SELECT hi.heading_id, ki.keyword, ki.value_raw"
                            " FROM keyword_items ki"
                            " JOIN heading_items hi ON ki.item_id = hi.id"
                            " WHERE hi.heading_id IN (%s)"
                            " ORDER BY hi.heading_id, hi.sort_key")
                    (mapconcat #'number-to-string heading-ids ", ")))))
        (dolist (row rows)
          (let ((hid (alist-get 'heading_id row))
                (key (alist-get 'keyword row)))
            (unless (dorg-protected-keyword-p key)
              (puthash hid
                       (append (gethash hid table)
                               (list (cons key (alist-get 'value_raw row))))
                       table))))))
    table))

(defun dorg-ql--dolt-fold-timestamp-rows (rows decide)
  "Fold ordered timestamp ROWS according to the DECIDE function.
ROWS are grouped by heading id and each row carries `id' and `title'.
DECIDE receives one row and returns `no-decision',
`decided/no-match', or `decided/match'.  The first token continues the
same heading; either decided token completes it, and only
`decided/match' emits the exact id/title alist expected by the Dolt
selector boundary.  An unknown token is a programming error."
  (let ((completed (make-hash-table :test 'eql))
        (out nil))
    (dolist (row rows)
      (let ((hid (alist-get 'id row)))
        (unless (gethash hid completed)
          (pcase (funcall decide row)
            ('no-decision nil)
            ('decided/no-match
             (puthash hid t completed))
            ('decided/match
             (puthash hid t completed)
             (push (list (cons 'id hid)
                         (cons 'title (alist-get 'title row)))
                   out))
            (token
             (error "Unknown Dolt timestamp-fold decision: %S" token))))))
    (nreverse out)))

(defun dorg-ql--dolt-planning-rows
    (dolt-dir root-id field from to &optional revision)
  "Return id/title rows whose FIELD planning timestamp falls in [FROM, TO].
FIELD is the planning predicate symbol `scheduled', `deadline', or
`closed'.  Selects candidate raw strings from `planning_items',
root-scoped to ROOT-ID and never across roots, ordered by
`heading_items.sort_key' so the FIRST PARSEABLE raw value per heading
wins — matching `dorg-heading--planning-timestamp'.  That candidate is
parsed with `dorg-timestamp-parse' and window-filtered with
`dorg-ql--timestamp-in-window-p' in Elisp (SQL date prefilters are
unsound over ranges/repeaters); only the structural narrowing
`root_id = ROOT-ID' and `<field>_raw IS NOT NULL' is pushed into SQL.
Issues exactly one query and returns one (id . title) row per matching
heading.  FIELD is whitelisted to a fixed column name, carrying no
SQL-injection surface."
  (let* ((col (pcase field
                ('scheduled "scheduled_raw")
                ('deadline  "deadline_raw")
                ('closed    "closed_raw")))
         (rows (dorg-ql--dolt-query
                dolt-dir
                revision
                ;; Whitelisted column + integer root-id are baked in via
                ;; `format', leaving no `%' for `dorg-sql' to expand.
                (format
                 (concat "SELECT hi.heading_id AS id, h.title AS title,"
                         " pi.%s AS raw"
                         " FROM planning_items pi"
                         " JOIN heading_items hi ON pi.item_id = hi.id"
                         " JOIN headings h ON hi.heading_id = h.id"
                         " WHERE h.root_id = %d AND pi.%s IS NOT NULL"
                         " ORDER BY hi.heading_id, hi.sort_key")
                 col root-id col))))
    ;; Rows arrive grouped by heading in sort_key order.  The first
    ;; parseable candidate decides; an unparseable row falls through.
    (dorg-ql--dolt-fold-timestamp-rows
     rows
     (lambda (row)
       (let ((ts (dorg-timestamp-parse (alist-get 'raw row))))
         (cond
          ((null ts) 'no-decision)
          ((dorg-ql--timestamp-in-window-p ts from to) 'decided/match)
          (t 'decided/no-match)))))))

(defun dorg-ql--dolt-property-ts-rows
    (dolt-dir root-id from to name &optional revision)
  "Return id/title rows with a PROPERTIES-drawer timestamp in [FROM, TO].
Selects candidate property values from the `heading_properties' view
(PROPERTIES-drawer entries only; DORG_ID is not stored as a property
entry, so identity stays owned by the `id' predicate), joined to
`headings' for root scoping (the view carries no `root_id') and the
title.  Root scope `h.root_id = ROOT-ID' and, when NAME is non-nil, the
structural narrowing `hp.prop_key = NAME' are the only filters pushed
into SQL — window bounds stay in Elisp because SQL date prefilters are
unsound over ranges/repeaters.  Each candidate value is parsed with
`dorg-timestamp-parse' and window-tested with
`dorg-ql--timestamp-in-window-p'; both active and inactive parseable
values match, mirroring `dorg-heading-property-timestamps'.  Issues
exactly one query and returns one (id . title) row per matching heading
(rows arrive grouped by `heading_id', ordered by `ordinal', so document
order is preserved and each heading yields at most one result)."
  (let* ((name-clause (if name
                          ;; Baked via `format' below; since the call
                          ;; passes no `dorg-sql' format args, a literal
                          ;; `%' in the quoted key is never expanded.
                          (concat " AND hp.prop_key = " (dorg-sql-quote name))
                        ""))
         (rows (dorg-ql--dolt-query
                dolt-dir
                revision
                (format
                 (concat "SELECT hp.heading_id AS id, h.title AS title,"
                         " hp.prop_value AS raw"
                         " FROM heading_properties hp"
                         " JOIN headings h ON hp.heading_id = h.id"
                         " WHERE h.root_id = %d%s"
                         " ORDER BY hp.heading_id, hp.ordinal")
                 root-id name-clause))))
    ;; An unparseable or out-of-window property falls through; only an
    ;; in-window candidate completes the heading.
    (dorg-ql--dolt-fold-timestamp-rows
     rows
     (lambda (row)
       (let ((ts (dorg-timestamp-parse (alist-get 'raw row))))
         (if (and ts (dorg-ql--timestamp-in-window-p ts from to))
             'decided/match
           'no-decision))))))

(defun dorg-ql--dolt-logbook-rows
    (dolt-dir root-id from to &optional revision)
  "Return id/title rows with a LOGBOOK-drawer timestamp in [FROM, TO].
Selects candidate raw drawer lines from `drawer_lines', joined through
`drawer_items' (narrowed to `name = LOGBOOK') and `heading_items' to
`headings' for root scoping and the title.  Root scope `h.root_id =
ROOT-ID' and the drawer-name filter `di.name = LOGBOOK' are the only
structural filters pushed into SQL — window bounds stay in Elisp because
SQL date prefilters are unsound over ranges/repeaters.  Each candidate
line is scanned for inactive timestamps with
`dorg-heading--scan-inactive-timestamps' and each window-tested with
`dorg-ql--timestamp-in-window-p', mirroring the model adapter's LOGBOOK
matching (active and mixed-range tokens are excluded by the scanner).
Issues exactly one query and returns one (id . title) row per matching
heading (rows arrive grouped by `heading_id', ordered by `item_id' and
`ordinal', so a heading with multiple matching lines yields one result)."
  (let* ((rows (dorg-ql--dolt-query
                dolt-dir
                revision
                ;; Integer root-id is baked in via `format'; the literal
                ;; drawer name carries no `%', so `dorg-sql' expands no
                ;; further format args.
                (format
                 (concat "SELECT hi.heading_id AS id, h.title AS title,"
                         " dl.line_raw AS raw"
                         " FROM drawer_lines dl"
                         " JOIN drawer_items di ON dl.drawer_item_id = di.item_id"
                         " JOIN heading_items hi ON di.item_id = hi.id"
                         " JOIN headings h ON hi.heading_id = h.id"
                         " WHERE h.root_id = %d AND di.name = 'LOGBOOK'"
                         " ORDER BY hi.heading_id, di.item_id, dl.ordinal")
                 root-id))))
    ;; A line with no in-window inactive timestamp falls through to the
    ;; next candidate line for the heading.
    (dorg-ql--dolt-fold-timestamp-rows
     rows
     (lambda (row)
       (if (cl-some
            (lambda (ts) (dorg-ql--timestamp-in-window-p ts from to))
            (dorg-heading--scan-inactive-timestamps (alist-get 'raw row)))
           'decided/match
         'no-decision)))))

(defun dorg-ql--dolt-active-ts-rows
    (dolt-dir root-id from to &optional revision)
  "Return id/title rows with an active body timestamp in [FROM, TO].
Selects candidate raw body strings from `text_items', joined through
`heading_items' to `headings' for root scoping and the title.  Root scope
`h.root_id = ROOT-ID' and the structural prefilter
`ti.text_raw LIKE '%<%'' are the only filters pushed into SQL: every
active timestamp contains a `<', so the LIKE is sound (it never drops a
match), while window bounds stay in Elisp because SQL date prefilters are
unsound over ranges/repeaters.  Each candidate text is scanned for active
timestamps with `dorg-heading--scan-active-timestamps' and each
window-tested with `dorg-ql--timestamp-in-window-p', mirroring the model
adapter's body matching (inactive and mixed-range tokens are excluded by
the scanner).  Issues exactly one query and returns one (id . title) row
per matching heading (rows arrive grouped by `heading_id', ordered by
`sort_key', so a heading with multiple matching text items yields one
result)."
  (let* ((rows (dorg-ql--dolt-query
                dolt-dir
                revision
                ;; Integer root-id is baked in via `format'; the LIKE
                ;; pattern's `%' chars are escaped as `%%' so `format'
                ;; emits the literal `%<%', and the query is then passed
                ;; to `dorg-sql' with no further format args.
                (format
                 (concat "SELECT hi.heading_id AS id, h.title AS title,"
                         " ti.text_raw AS raw"
                         " FROM text_items ti"
                         " JOIN heading_items hi ON ti.item_id = hi.id"
                         " JOIN headings h ON hi.heading_id = h.id"
                         " WHERE h.root_id = %d AND ti.text_raw LIKE '%%<%%'"
                         " ORDER BY hi.heading_id, hi.sort_key")
                 root-id))))
    ;; A text item with no in-window active timestamp falls through to
    ;; the next candidate text for the heading.
    (dorg-ql--dolt-fold-timestamp-rows
     rows
     (lambda (row)
       (if (cl-some
            (lambda (ts) (dorg-ql--timestamp-in-window-p ts from to))
            (dorg-heading--scan-active-timestamps (alist-get 'raw row)))
           'decided/match
         'no-decision)))))

(defun dorg-ql--dolt-rows (query reg &optional revision)
  "Return id/title rows for the SIX exact point-lookup forms in REG's Dolt repo.
QUERY is one of `(id N)', `(property K V)', `(keyword K)', `(keyword K V)',
`(title T)', or `(tag TAG)' — the bounded-SQL fast path the v2 Dolt planner
recognizes via `dorg-ql--dolt-ast->point-form' and answers directly.  All
selectors constrain the registered root_id and never query across roots.

This seam answers point lookups ONLY.  Timestamp aliases and every other
form execute on the general v2 planner (which reaches the conservative
scanners through `dorg-ql--dolt-timestamp-id-set'), so any form that is not
one of the six is a programming error and signals `dorg-ql-error' via the
catch-all rather than silently returning an empty result."
  (let* ((backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         ;; DORG_* protected keywords are DOrg-owned metadata and must never
         ;; surface as query results.  The LIKE pattern + escape are derived
         ;; from the one prefix constant (`dorg-protected-keyword-sql-like')
         ;; and compared against UPPER(keyword) so the exclusion is
         ;; case-insensitive and cannot drift from the Elisp predicate.
         (protected (dorg-protected-keyword-sql-like))
         (protected-like (dorg-sql-quote (car protected)))
         (protected-escape (dorg-sql-quote (cdr protected))))
    (pcase query
      (`(id ,id)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT id, title FROM headings WHERE root_id = %d AND id = %d"
        root-id id))
      (`(property ,key ,value)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT DISTINCT hp.heading_id AS id, h.title AS title
           FROM heading_properties hp
           JOIN headings h ON hp.heading_id = h.id
          WHERE h.root_id = %d
            AND hp.prop_key = %s
            AND hp.prop_value = %s"
        root-id
        (dorg-sql-quote key)
        (dorg-sql-quote value)))
      (`(keyword ,key)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT DISTINCT hi.heading_id AS id, h.title AS title
           FROM keyword_items ki
           JOIN heading_items hi ON ki.item_id = hi.id
           JOIN headings h ON hi.heading_id = h.id
          WHERE h.root_id = %d
            AND h.id <> %d
            AND ki.keyword = %s
            AND UPPER(ki.keyword) NOT LIKE %s ESCAPE %s"
        root-id
        root-id
        (dorg-sql-quote key)
        protected-like
        protected-escape))
      (`(keyword ,key ,value)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT DISTINCT hi.heading_id AS id, h.title AS title
           FROM keyword_items ki
           JOIN heading_items hi ON ki.item_id = hi.id
           JOIN headings h ON hi.heading_id = h.id
          WHERE h.root_id = %d
            AND h.id <> %d
            AND ki.keyword = %s
            AND ki.value_raw = %s
            AND UPPER(ki.keyword) NOT LIKE %s ESCAPE %s"
        root-id
        root-id
        (dorg-sql-quote key)
        (dorg-sql-quote value)
        protected-like
        protected-escape))
      (`(title ,title)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT id, title FROM headings WHERE root_id = %d AND title = %s"
        root-id
        (dorg-sql-quote title)))
      (`(tag ,tag)
       (dorg-ql--dolt-query
        dolt-dir
        revision
        "SELECT DISTINCT h.id AS id, h.title AS title
           FROM heading_tags ht
           JOIN headings h ON ht.heading_id = h.id
          WHERE h.root_id = %d
            AND ht.tag = %s"
        root-id
        (dorg-sql-quote tag)))
      ;; Fail closed: the fast-path translator guarantees only the six exact
      ;; point forms reach here, so any other form — including a timestamp
      ;; alias, whose atomic dispatch arms were retired in favour of the v2
      ;; scanner path — is a programming error and must SIGNAL, never
      ;; silently return an empty result.
      (_ (dorg-ql--error
          "dorg-ql--dolt-rows: not an exact point-lookup form: %S" query)))))

(defun dorg-ql--dolt-select (query reg &optional revision)
  "Run normalized QUERY against REG's Dolt repo.
Returns markerless result plists with heading id, title, and the
heading's property alist, in `dorg-ql--dolt-rows' order.

Costs two queries regardless of result count: one selector query (which
owns root scoping) plus one batched property load via
`dorg-ql--dolt-properties-batch'.  An empty selector result issues no
property query."
  (let ((rows (dorg-ql--dolt-rows query reg revision)))
    (when rows
      (let* ((dolt-dir (dorg-backend-dolt-dir (dorg-registration-backend reg)))
             (ids (mapcar (lambda (row) (alist-get 'id row)) rows))
             (props (dorg-ql--dolt-properties-batch dolt-dir ids revision)))
        (mapcar (lambda (row)
                  (let ((id (alist-get 'id row)))
                    (dorg-ql--make-result
                     :heading-id id
                     :title      (alist-get 'title row)
                     :properties (gethash id props))))
                rows)))))

;;; ─── Normalized v2 AST planner / executor ─────────────────────────────

;; The v2 planner consumes the backend-neutral AST from `dorg-ql-normalize'
;; --- a pipeline `(:pipe (:source :all) STEP...)' --- and answers it over a
;; single Dolt root.  A selection SQL can answer alone (a wholly pushable
;; filter, or one whose only other leaves are insensitive scanned-column
;; leaves, see `dorg-ql--dolt-scan-filter') loads no heading models.  Any
;; other selection is residual: the planner loads ONE root-scoped,
;; headline-only adjacency map (`dorg-model-load-headings' with HEADLINE-ONLY:
;; the `headings' table plus tags, no body/item/drawer/planning payload) and
;; reuses the model
;; evaluator's matching/traversal/order helpers, so the Dolt and model
;; baselines converge by construction.  Local properties load lazily, once,
;; through the existing batched `heading_properties' query and only when a
;; `property' predicate (or result materialization) needs them.  Timestamp
;; predicates reuse the conservative scanners above, each of which scans
;; structured items in a bounded, root-scoped way and yields a candidate id
;; set.
;;
;; The synthetic level-0 root (id = root_id) is excluded from the candidate
;; universe and the traversal adjacency, matching the plain-org/model
;; baseline which carries no synthetic root.
;;
;; Fail-closed: a `not' over a timestamp/content predicate has no sound
;; bounded complement (the scanners are conservative over ranges/repeaters),
;; so it signals `dorg-ql-error' rather than a broad body scan.  A
;; non-`(:pipe (:source :all) ...)' AST and unknown node shapes signal
;; `dorg-ql-error' too; the planner never widens to a full scan or silently
;; empties the result.

(defun dorg-ql--dolt-assert-negation-bounded (ast)
  "Signal `dorg-ql-error' if a `:timestamp' appears under a `:not' in AST.
Negating a conservative timestamp/content scan has no sound bounded
complement, so the planner fails closed instead of widening to a broad
body scan.  A doubly-negated timestamp is positive again and allowed."
  (cl-labels ((walk (e negated)
                (pcase e
                  (`(:and . ,es) (dolist (x es) (walk x negated)))
                  (`(:or . ,es) (dolist (x es) (walk x negated)))
                  (`(:not ,e2) (walk e2 (not negated)))
                  (`(:timestamp . ,_)
                   (when negated
                     (dorg-ql--unsupported
                      (concat "Dolt planner cannot negate a timestamp"
                              " predicate (no bounded complement): %S")
                      e))))))
    (dorg-ql--map-filters ast (lambda (expr) (walk expr nil)))))

;; Property/keyword presence probes and the distinct-timestamp collector
;; are now the shared `dorg-ql--any-node-p' / `dorg-ql--collect-nodes' in
;; `dorg-ql-eval.el'.

(defun dorg-ql--dolt-timestamp-id-set
    (dolt-dir root-id node &optional revision)
  "Return a hash set (id → t) of headings matching the `:timestamp' NODE.
NODE is `(:timestamp SOURCE :overlaps (START . END))'.  Dispatches to the
conservative scanner for SOURCE; each scans structured items in a
bounded, root-scoped way and returns one row per matching heading."
  (pcase-let ((`(:timestamp ,source ,_rel (,from . ,to)) node))
    (let* ((rows (pcase source
                   ((or 'scheduled 'deadline 'closed)
                    (dorg-ql--dolt-planning-rows
                     dolt-dir root-id source from to revision))
                   ('active-ts
                    (dorg-ql--dolt-active-ts-rows
                     dolt-dir root-id from to revision))
                   ('logbook
                    (dorg-ql--dolt-logbook-rows
                     dolt-dir root-id from to revision))
                   (`(:property ,name)
                    (dorg-ql--dolt-property-ts-rows
                     dolt-dir root-id from to (unless (eq name '*) name)
                     revision))
                   (_ (dorg-ql--error
                       "unsupported Dolt timestamp source: %S" source))))
           (set (make-hash-table :test 'eql)))
      (dolist (r rows) (puthash (alist-get 'id r) t set))
      set)))

(defun dorg-ql--dolt-ordered-headings (by-id root-id)
  "Return the non-root headings under ROOT-ID in document (pre-order) order.
BY-ID is the id → `dorg-heading' hash from a headline-only load.  Siblings
are ordered by their decoded `sort-key' with `string<' --- the same
lexicographic order the model and renderer use --- and the level-0 root
\(id = ROOT-ID) is the walk origin but is itself excluded, matching the
plain-org/model baseline which carries no synthetic root.  The walk guards
against parent-id cycles."
  (let ((children (make-hash-table :test 'eql))
        (seen (make-hash-table :test 'eql))
        ordered)
    (maphash (lambda (_id h)
               (push h (gethash (dorg-heading-parent-id h) children)))
             by-id)
    (maphash (lambda (k v)
               (puthash k
                        (sort v (lambda (a b)
                                  (string< (or (dorg-heading-sort-key a) "")
                                           (or (dorg-heading-sort-key b) ""))))
                        children))
             children)
    (cl-labels ((walk (h)
                  (let ((id (dorg-heading-id h)))
                    (unless (gethash id seen)
                      (puthash id t seen)
                      (unless (eql id root-id)
                        (push h ordered))
                      (dolist (c (gethash id children)) (walk c))))))
      (let ((root (gethash root-id by-id)))
        (if root
            (walk root)
          ;; No explicit root row (defensive): walk every group so headings
          ;; are still ordered rather than silently dropped.
          (let (roots)
            (maphash (lambda (_id h) (push h roots)) by-id)
            (mapc #'walk
                  (sort roots
                        (lambda (a b)
                          (string< (or (dorg-heading-sort-key a) "")
                                   (or (dorg-heading-sort-key b) "")))))))))
    (nreverse ordered)))

(defun dorg-ql--dolt-leaf-match-p (expr heading ctx)
  "Return non-nil when HEADING satisfies the normalized leaf EXPR under CTX.
CTX is a plist `(:props PROPS-HASH :keywords KW-HASH :ts-cache CACHE)'.
`:property' consults the loaded PROPS-HASH and `:keyword' the loaded
KW-HASH via the shared alist matcher, `:timestamp' tests membership in
its precomputed candidate set, and every remaining headline leaf (`:id',
`:cmp', `:tag') delegates to the shared model matcher so the Dolt and
model semantics cannot drift.  Boolean composition is handled by
`dorg-ql--eval-expr-match-p'."
  (pcase expr
    (`(:property ,key ,op ,value :case ,mode)
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :props)) mode))
    (`(:property ,key ,op ,value)
     (unless (memq op '(exists missing))
       (dorg-ql--error "string property comparison lacks canonical :case mode: %S" expr))
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :props))))
    (`(:keyword ,key ,op ,value :case ,mode)
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :keywords)) mode))
    (`(:keyword ,key ,op ,value)
     (unless (memq op '(exists missing))
       (dorg-ql--error "string keyword comparison lacks canonical :case mode: %S" expr))
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :keywords))))
    (`(:timestamp . ,_)
     (and (gethash (dorg-heading-id heading)
                   (cdr (assoc expr (plist-get ctx :ts-cache))))
          t))
    (_ (dorg-ql--expr-match-p expr heading))))

(defun dorg-ql--dolt-expr-match-p (expr heading ctx)
  "Return non-nil when HEADING satisfies normalized filter EXPR under CTX.
A thin wrapper over the shared boolean driver `dorg-ql--eval-expr-match-p'
with the Dolt leaf matcher closed over CTX."
  (dorg-ql--eval-expr-match-p
   expr heading
   (lambda (node h) (dorg-ql--dolt-leaf-match-p node h ctx))))

(defun dorg-ql--projection-property-keys (columns)
  "Return the distinct stored property keys requested by COLUMNS."
  (delete-dups
   (delq nil (mapcar (lambda (column)
                      (pcase (nth 2 column) (`(:property ,key) key)))
                    columns))))

(defun dorg-ql--dolt-collection-values (columns ids root-id dolt-dir revision)
  "Fetch selected cardinality-many COLUMNS as ordered arrays per heading ID.
Each field's `:sql' plan supplies `:batch', a function of selected IDs that
returns one SQL statement with columns heading_id, ordinal, and value."
  (let ((dorg-ql--dolt-field-plans nil)
        (out (make-hash-table :test 'eq))
        (fields (delete-dups
                 (delq nil
                       (mapcar (lambda (column)
                                 (let ((expr (nth 2 column)))
                                   (and (eq (car-safe expr) :field) (nth 1 expr))))
                               columns)))))
    (dolist (field fields)
      (when (eq (plist-get (dorg-ql--query-field field) :cardinality) 'many)
        (let* ((plan (dorg-ql--dolt-field-plan field root-id))
               (batch (plist-get plan :batch))
               (sql (and batch (funcall batch ids))))
          (unless (stringp sql)
            (dorg-ql--unsupported "collection field %S returned no batch SQL" field))
          (let ((per-id (make-hash-table :test 'eql)))
            (dolist (row (dorg-ql--dolt-query dolt-dir revision sql))
              (let ((id (alist-get 'heading_id row))
                    (ordinal (alist-get 'ordinal row))
                    (value (dorg-ql--dolt-coerce-extension-value
                            field (alist-get 'value row))))
                (unless (and (integerp id) (integerp ordinal))
                  (dorg-ql--unsupported "collection field %S returned invalid row" field))
                (puthash id (cons (cons ordinal value) (gethash id per-id)) per-id)))
            (dolist (id ids)
              (puthash id
                       (mapcar #'cdr (sort (gethash id per-id)
                                           (lambda (a b) (< (car a) (car b)))))
                       per-id))
            (puthash field per-id out)))))
    out))

(defun dorg-ql--dolt-coerce-extension-value (field value)
  "Map FIELD's SQL wire VALUE to its declared public scalar representation."
  (if (eq (plist-get (dorg-ql--query-field field) :type) 'boolean)
      (cond ((null value) nil)
            ((member value '(t 1 "1")) t)
            ((member value '(:json-false 0 "0")) :json-false)
            (t value))
    value))

(defun dorg-ql--dolt-selected-row (row columns props &optional collections)
  "Project one SQL ROW to COLUMNS, consulting preloaded PROPS by heading ID."
  (let ((id (alist-get 'id row)) (index -1))
    (dorg-ql--project-row
     columns id nil
     (lambda (expr)
       (cl-incf index)
       (pcase expr
         (`(:property ,key) (cdr (assoc key (and props (gethash id props)))))
         (`(:field ,field)
          (if (eq (plist-get (dorg-ql--query-field field) :cardinality) 'many)
              (gethash id (gethash field collections))
            (let ((value (dorg-ql--dolt-coerce-extension-value
                          field (alist-get (intern (format "c%d" index)) row))))
              (if (eq field 'commented)
                  (if (member value '(nil 0 "0" :json-false)) :json-false t)
                value)))))))))

(defun dorg-ql--dolt-materialize (headings dolt-dir props &optional revision columns)
  "Map HEADINGS to markerless result plists in their current order.
PROPS, when non-nil, is a preloaded id → property-alist hash reused for
`:properties'; otherwise the final result ids are batched once through
`dorg-ql--dolt-properties-batch'.  An empty HEADINGS issues no query."
  (when headings
    (let* ((keys (dorg-ql--projection-property-keys columns))
           (props (or props
                      (and (or (not columns) keys)
                           (dorg-ql--dolt-properties-batch
                            dolt-dir (mapcar #'dorg-heading-id headings)
                            revision keys)))))
      (mapcar (lambda (h)
                (if columns
                    (dorg-ql--project-row
                     columns (dorg-heading-id h) nil
                     (lambda (expr)
                       (pcase expr
                         (`(:property ,key)
                          (cdr (assoc key (and props (gethash (dorg-heading-id h)
                                                              props)))))
                         (_ (dorg-ql--model-value expr h)))))
                  (dorg-ql--make-result
                   :heading-id (dorg-heading-id h)
                   :marker nil
                   :title (dorg-heading-title h)
                   :properties (gethash (dorg-heading-id h) props))))
              headings))))

(defun dorg-ql--dolt-ast->point-form (ast)
  "Map AST to its atomic point-lookup form, or nil when it is not one.
Recognizes ONLY the exact shape `(:pipe (:source :all) (:filter LEAF))' —
a single filter step, no traverse/unique/one — where LEAF is one of the six
equality/presence point leaves, and returns the atomic form the retained
bounded-SQL selector (`dorg-ql--dolt-select' / `dorg-ql--dolt-rows')
answers:

  (:id = N)              -> (id N)
  (:property K = V :case sensitive) -> (property K V)
  (:cmp title = T :case sensitive)  -> (title T)
  (:tag = TAG :case sensitive)      -> (tag TAG)
  (:keyword K exists nil) -> (keyword K)
  (:keyword K = V)       -> (keyword K V)

Any other AST shape or leaf op — ranges, `!=', `contains'/`prefix'/
`suffix', booleans, traversals, and every `:timestamp' node — yields nil,
so those (a timestamp alias included) always route to the general planner
and its scanners rather than to the point fast path.  This is how the fast
path is reached ON the v2 AST path by SHAPE, not by a top-level atomic
branch."
  (pcase ast
    (`(:pipe (:source :all) (:filter ,leaf))
     (pcase leaf
       (`(:id = ,n) (list 'id n))
       (`(:property ,(and k (pred stringp)) = ,(and v (pred stringp))
                    :case sensitive)
        (list 'property k v))
       (`(:cmp title = ,(and tt (pred stringp)) :case sensitive) (list 'title tt))
       (`(:tag = ,(and tag (pred stringp)) :case sensitive) (list 'tag tag))
       (`(:keyword ,(and k (pred stringp)) exists nil) (list 'keyword k))
       (`(:keyword ,(and k (pred stringp)) = ,(and v (pred stringp))
                   :case sensitive)
        (list 'keyword k v))
       (_ nil)))
    (_ nil)))

(defun dorg-ql--dolt-eval (ast reg &optional revision)
  "Plan and execute a normalized v2 AST against REG's Dolt root.
AST is `(:pipe (:source :all) STEP...)' from `dorg-ql-normalize'.  Returns
markerless result plists in document/tree order with de-duplication
matching the model baseline.  Loads only a root-scoped headline-only
adjacency map plus on-demand local properties; body/items are touched only
by timestamp scanners.  Signals `dorg-ql-error' for a non-pipeline AST, an
unsupported source, a negated timestamp predicate, or an unknown node.

An AST of the exact single-filter point-lookup shape (see
`dorg-ql--dolt-ast->point-form') takes the measured bounded-SQL fast path:
it answers via `dorg-ql--dolt-select' WITHOUT loading the headline
adjacency map, executing the same SQL in the same storage order as the
historical atomic Dolt selector."
  (unless (and (consp ast) (eq (car ast) :pipe)
               (equal (cadr ast) '(:source :all)))
    (dorg-ql--error
     "Dolt planner expects a normalized (:pipe (:source :all) ...) AST: %S"
     ast))
  (dorg-ql--dolt-assert-negation-bounded ast)
  ;; Fast path: an exact single-filter point lookup answers via the bounded
  ;; SQL selector, before any adjacency load.  `dorg-ql--dolt-select' may
  ;; legitimately return nil (no match), so we branch on the recognized
  ;; FORM, never on the result — falling through on an empty result would
  ;; re-run the query on the general planner.
  (let ((point-form (dorg-ql--dolt-ast->point-form ast)))
    (if point-form
        (dorg-ql--dolt-select point-form reg revision)
      (dorg-ql--dolt-eval-general ast reg revision))))

(defun dorg-ql--dolt-eval-general (ast reg &optional revision)
  "Execute a non-point-form v2 AST against REG's Dolt root via the planner.
The general path for `dorg-ql--dolt-eval': loads a root-scoped
headline-only adjacency map plus on-demand local properties/keywords and
per-timestamp-node scanner sets, then runs the pipeline over the shared
evaluator.  Reached for every AST the point-lookup fast path does not
recognize (compound filters, traversals, timestamp aliases)."
  (let* ((columns (dorg-ql--projection-columns ast))
         (selection (dorg-ql--selection-ast ast))
         (keys (dorg-ql--projection-property-keys columns))
         (backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         (scan-expr (dorg-ql--dolt-scan-filter ast))
         ;; Non-nil when SQL alone selects the rows, by pushdown or column scan.
         (pushdown-expr (or (dorg-ql--dolt-pushdown-filter ast) scan-expr))
         (pushdown-rows
          (cond
           (scan-expr
            (dorg-ql--dolt-scan-select scan-expr root-id dolt-dir revision columns))
           (pushdown-expr
            (dorg-ql--dolt-with-statement-kind pushdown-select
              (dorg-ql--dolt-query
               dolt-dir revision
               (if columns
                   (dorg-ql--dolt-pushdown-select-sql pushdown-expr root-id columns)
                 (dorg-ql--dolt-pushdown-select-sql pushdown-expr root-id)))))))
         (pushdown-props
          (and pushdown-expr pushdown-rows (or (not columns) keys)
               (dorg-ql--dolt-with-statement-kind pushdown-properties
                 (dorg-ql--dolt-properties-batch
                  dolt-dir
                  (mapcar (lambda (row) (alist-get 'id row)) pushdown-rows)
                  revision keys))))
         (collections
          (and pushdown-expr pushdown-rows columns
               (dorg-ql--dolt-collection-values
                columns (mapcar (lambda (row) (alist-get 'id row)) pushdown-rows)
                root-id dolt-dir revision)))
         ;; One root-scoped, headline-only adjacency map (headings + tags).
         (by-id (and (not pushdown-expr)
                     (dorg-model-load-headings
                      dolt-dir root-id nil nil t
                      (lambda (sql)
                        (dorg-ql--dolt-with-statement-kind by-id-materialization
                          (dorg-ql--dolt-query dolt-dir revision sql))))))
         (ordered (and by-id (dorg-ql--dolt-ordered-headings by-id root-id)))
         (idx (dorg-ql--build-tree-index ordered))
         ;; Local properties: loaded once over the candidate set, but only
         ;; when a `property' predicate needs them; reused to materialize.
         (props (when (and (not pushdown-expr)
                           (dorg-ql--any-node-p selection :property))
                  (dorg-ql--dolt-properties-batch
                   dolt-dir (mapcar #'dorg-heading-id ordered) revision)))
         (keywords (when (dorg-ql--any-node-p ast :keyword)
                     (dorg-ql--dolt-keywords-batch
                      dolt-dir (mapcar #'dorg-heading-id ordered) revision)))
         ;; One bounded scanner pass per distinct timestamp node.
         (ts-cache (mapcar (lambda (node)
                             (cons node (dorg-ql--dolt-timestamp-id-set
                                         dolt-dir root-id node revision)))
                           (dorg-ql--collect-nodes ast :timestamp)))
         (ctx (list :props props :keywords keywords :ts-cache ts-cache))
         ;; Do not run selection eagerly when SQL already supplied it.  Besides
         ;; needless residual work, an eager `:one' would assert against the
         ;; deliberately absent residual candidate set rather than SQL rows.
         (working (and (not pushdown-expr)
                       (dorg-ql--eval-pipeline
                        (cddr selection) ordered idx
                        (lambda (expr h)
                          (dorg-ql--dolt-expr-match-p expr h ctx))))))
    (if pushdown-expr
        (progn
          ;; `:one' belongs to selection, before its requested columns are
          ;; projected.  SQL already bounded the set at this revision.
          (when (member '(:one) (cddr selection))
            (dorg-ql--assert-one pushdown-rows))
        (mapcar (lambda (row)
                  (if columns
                      (dorg-ql--dolt-selected-row row columns pushdown-props collections)
                    (let ((id (alist-get 'id row)))
                      (dorg-ql--make-result
                       :heading-id id :marker nil :title (alist-get 'title row)
                       :properties (gethash id pushdown-props)))))
                pushdown-rows))
      (dorg-ql--dolt-materialize working dolt-dir props revision columns))))

;;; ─── Terminal BM25 ranked reads ───────────────────────────────────────

(defun dorg-ql--dolt-bm25-pushable-filter-p (expr)
  "Return non-nil when EXPR can narrow a ranked candidate set in SQL.

The general planner's classifier treats built-in stored fields as residual
because it deliberately shares extension descriptor planning.  The ranked
planner has its own fixed `dorg-ql--dolt-field-sql' map and needs those
ordinary title/todo/etc. filters before TOP-K, so it admits that closed map
while retaining the general classifier for every other leaf."
  (or (dorg-ql--dolt-pushable-filter-p expr)
      (pcase expr
        (`(:and . ,xs) (and xs (cl-every #'dorg-ql--dolt-bm25-pushable-filter-p xs)))
        (`(:or . ,xs) (and xs (cl-every #'dorg-ql--dolt-bm25-pushable-filter-p xs)))
        (`(:not ,x) (dorg-ql--dolt-bm25-pushable-filter-p x))
        (`(:cmp ,field ,_ ,_ :case sensitive)
         (assq field dorg-ql--dolt-field-sql))
        (`(:cmp ,field ,_ ,_)
         (assq field dorg-ql--dolt-field-sql))
        (_ nil))))

(defun dorg-ql--dolt-bm25-rank-components (ast root-id)
  "Return the validated ranked-read bundle for AST at ROOT-ID.

The rank normalizer already enforces terminal placement.  This lowerer adds
the execution constraint: every preceding filter must be wholly SQL-pushable,
so it narrows candidate documents before score aggregation and TOP-K rather
than silently materializing a root for residual filtering."
  (let* ((steps (cddr ast))
         (rank (cl-find-if (lambda (step) (eq (car-safe step) :rank-bm25))
                           steps)))
    (unless rank
      (dorg-ql--error "Dolt BM25 planner requires one rank-bm25 step: %S" ast))
    (let* ((before (cl-subseq steps 0 (cl-position rank steps :test #'eq)))
           (filters (mapcar (lambda (step)
                              (pcase step (`(:filter ,expr) expr)
                                (_ (dorg-ql--unsupported
                                    "rank-bm25 requires SQL-pushable preceding filters: %S"
                                    step))))
                            before)))
      (unless (cl-every #'dorg-ql--dolt-bm25-pushable-filter-p filters)
        (dorg-ql--unsupported
         "rank-bm25 cannot use a residual pre-rank filter: %S" ast))
      (let* ((predicate (cond ((null filters) '(:true))
                              ((null (cdr filters)) (car filters))
                              (t (cons :and filters))))
             (dorg-ql--dolt-field-plans nil)
             (filter-sql (dorg-ql--dolt-filter-sql predicate root-id))
             (joins (dorg-ql--dolt-field-joins))
             (raw-terms (dorg-bm25-tokenize
                         (nth 1 rank) (plist-get (nthcdr 2 rank) :profile)))
             (_bounded-query
              (unless (<= (length raw-terms) dorg-bm25-max-query-terms)
                (dorg-ql--error "rank-bm25 accepts at most %d tokenizer terms"
                                dorg-bm25-max-query-terms)))
             (terms (delete-dups raw-terms)))
        ;; A rank operation with no retained query terms is a well-defined
        ;; empty result.  It still reads the manifest at one snapshot, so an
        ;; absent or incompatible index cannot masquerade as an OOV success.
        (list :profile (plist-get (nthcdr 2 rank) :profile)
              :terms terms :filter-sql filter-sql :joins joins
              :top-k (plist-get (nthcdr 2 rank) :top-k))))))

(defun dorg-ql--dolt-bm25--unavailable (fmt &rest args)
  "Signal the typed ranked-index unavailable outcome formatted by FMT."
  (apply #'dorg-ql--raise 'unsupported (apply #'format fmt args)
         (list :detail 'search-index-unavailable)))

(defun dorg-ql--dolt-bm25--unbuilt (root-id profile)
  "Signal that ROOT-ID's installed PROFILE index has never been built.
An installed index whose root has no manifest is unbuilt, not empty:
maintenance leaves such a root's index wholly absent rather than partial,
and an explicit rebuild builds it."
  (dorg-ql--raise 'unsupported
                  (format (concat "BM25 index for root %d profile %s is not built;"
                                  " run dorg-ext-rebuild-index")
                          root-id profile)
                  :detail 'derived-index-unbuilt))

(defun dorg-ql--dolt-bm25-projection-levels (dolt-dir root-id revision ids)
  "Return a bounded heading-id → recursive level map for ranked IDS.

The ordinary general planner carries one root-wide qtree.  A ranked final
projection instead follows the at-most-top-K parent chains upward from its
already selected IDs, keeping both score and projection hydration bounded."
  (let ((levels (make-hash-table :test 'eql)))
    (dolist
        (row
         (dorg-ql--dolt-query
          dolt-dir revision
          (format
           (concat "WITH RECURSIVE ancestry (heading_id, parent_id, level) AS ("
                   " SELECT id, parent_id, level_offset FROM headings"
                   " WHERE root_id = %d AND id IN (%s)"
                   " UNION ALL"
                   " SELECT a.heading_id, p.parent_id, a.level + p.level_offset"
                   " FROM ancestry a JOIN headings p ON p.id = a.parent_id"
                   " WHERE p.root_id = %d AND p.id <> %d)"
                   " SELECT heading_id, MAX(level) AS level FROM ancestry"
                   " GROUP BY heading_id")
           root-id (mapconcat #'number-to-string ids ", ") root-id root-id)))
      (puthash (alist-get 'heading_id row) (alist-get 'level row) levels))
    levels))

(defun dorg-ql--dolt-bm25-projection-rows (dolt-dir root-id revision ids columns)
  "Return bounded scalar projection rows for ranked IDS at REVISION.

Ranking scores only query-term postings.  A terminal `select' gets one
separate ID-bounded hydration, rather than widening the score query into a
root scan.  Collections and stored properties use their existing batch
helpers after this scalar row read."
  (let* ((level-indices
          (cl-loop for column in columns
                   for index from 0
                   when (equal (nth 2 column) '(:field level)) collect index))
         (dorg-ql--dolt-field-plans nil)
         ;; Recursive levels are written after the scalar query from bounded
         ;; parent chains.  NULL reserves each public cN slot here.
         (select (dorg-ql--dolt-projection-select-list columns root-id "NULL"))
         (joins (dorg-ql--dolt-field-joins))
         (rows (dorg-ql--dolt-query
                dolt-dir revision
                (format (concat "SELECT %s FROM headings h %s"
                                " WHERE h.root_id = %d AND h.id IN (%s)")
                        select joins root-id
                        (mapconcat #'number-to-string ids ", "))))
         (by-id (make-hash-table :test 'eql)))
    (let ((levels (and level-indices
                       (dorg-ql--dolt-bm25-projection-levels
                        dolt-dir root-id revision ids))))
      (dolist (row rows)
        (let ((id (alist-get 'id row)))
          (dolist (index level-indices)
            (setf (alist-get (intern (format "c%d" index)) row)
                  (gethash id levels)))
          (puthash id row by-id))))
    by-id))

(defun dorg-ql--dolt-bm25-project-matches
    (matches columns dolt-dir root-id revision)
  "Attach terminal select COLUMNS to bounded ranked MATCHES.

MATCHES is already ordered by score then canonical heading id.  The returned
list preserves that order and reads only the TOP-K ids at the exact captured
revision."
  (if (null columns)
      matches
    (if (null matches)
        ;; An all-OOV query still validated the manifest, but has no ids to
        ;; hydrate.  In particular, do not manufacture an invalid SQL IN ().
        nil
      (let* ((ids (mapcar (lambda (match) (plist-get match :heading-id)) matches))
           (property-keys (dorg-ql--projection-property-keys columns))
           (props (and property-keys
                       (dorg-ql--dolt-properties-batch
                        dolt-dir ids revision property-keys)))
           (rows (dorg-ql--dolt-bm25-projection-rows
                  dolt-dir root-id revision ids columns))
           (collections (dorg-ql--dolt-collection-values
                         columns ids root-id dolt-dir revision)))
      (mapcar
       (lambda (match)
         (let* ((id (plist-get match :heading-id))
                (row (gethash id rows)))
           (unless row
             (dorg-ql--dolt-bm25--unavailable
              "BM25 ranked projection lost heading %s at the captured revision" id))
           (append match
                   (list :values
                         (plist-get
                          (dorg-ql--dolt-selected-row row columns props collections)
                          :values)))))
       matches)))))

(defun dorg-ql--dolt-bm25-search (ast reg &optional _revision)
  "Execute terminal ranked AST through the BM25 extension snapshot reader.

Returns `(:matches ... :profile ... :index-generation ...
:corpus-generation ... :snapshot HEAD)'.  The extension reader captures one
committed Dolt token and revision-binds both manifest and bounded postings
statements to it.  Missing/incompatible derived rows are surfaced as the
public typed unavailable condition, never as a canonical-content fallback."
  (let* ((backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         (components (dorg-ql--dolt-bm25-rank-components ast root-id))
         (columns (dorg-ql--projection-columns ast))
         (profile (plist-get components :profile)))
    (condition-case err
        (let* ((read (dorg-ext-read-snapshot
                        (dorg-ext-read-cache-make) 'bm25
                        (list :dolt-dir dolt-dir :root-id root-id
                              :file (dorg-registration-file reg))
                        components))
                 (payload (plist-get read :rows))
                 (manifest (plist-get payload :manifest))
                 (matches (plist-get payload :matches)))
            (cond ((null manifest) (dorg-ql--dolt-bm25--unbuilt root-id profile))
                  ((/= (length manifest) 1)
                   (dorg-ql--dolt-bm25--unavailable
                    "BM25 index manifest is ambiguous for root %d profile %s"
                    root-id profile)))
            (let ((row (car manifest))
                  (snapshot (plist-get read :head)))
              (unless (and (equal (alist-get 'profile row) (symbol-name profile))
                           (equal (alist-get 'profile_version row)
                                  dorg-bm25-profile-version)
                           (equal (alist-get 'tokenizer_version row)
                                  dorg-bm25-tokenizer-version)
                           (= (alist-get 'schema_version row) dorg-bm25-schema-version)
                           (stringp (alist-get 'corpus_generation row)))
                (dorg-ql--dolt-bm25--unavailable
                 "BM25 index manifest is incompatible for root %d profile %s"
                 root-id profile))
              (list :matches
                    (dorg-ql--dolt-bm25-project-matches
                     (mapcar (lambda (match)
                               (list :heading-id (alist-get 'heading_id match)
                                     :title (alist-get 'title match)
                                     :score (alist-get 'score match)))
                             matches)
                     columns dolt-dir root-id snapshot)
                    :profile profile
                    :index-generation (dorg-bm25--index-generation
                                       (symbol-name profile)
                                       (alist-get 'corpus_generation row))
                    :corpus-generation (alist-get 'corpus_generation row)
                    :snapshot snapshot)))
      ;; A typed QL outcome raised above is already the answer.
      (dorg-ql-error (signal (car err) (cdr err)))
      (dorg-ext-read-error
       (dorg-ql--dolt-bm25--unavailable "%s" (error-message-string err)))
      (error
       (dorg-ql--dolt-bm25--unavailable "%s" (error-message-string err))))))

(provide 'dorg-ql-dolt)
;;; dorg-ql-dolt.el ends here
