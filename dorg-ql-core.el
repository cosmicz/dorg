;;; dorg-ql-core.el --- Shared query core for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Backend-agnostic foundations shared by every `dorg-ql' adapter:
;;
;;   - the `dorg-ql-error' condition and its two typed reason classes;
;;   - the single source of truth for timestamp predicate semantics
;;     (`dorg-ql--timestamp-predicate-metadata') from which the predicate
;;     set, per-backend capability table, and normalization arities are all
;;     derived, so adding a predicate cannot update one list and forget
;;     another;
;;   - the shared window-bound / window-form normalizers
;;     (`dorg-ql--normalize-window-spec' et al.) the v2 normalizer
;;     (`dorg-ql-normalize', dorg-ql-ast) reuses for timestamp aliases;
;;   - the stable result-plist constructor (`dorg-ql--make-result');
;;   - the one date-window predicate (`dorg-ql--timestamp-in-window-p')
;;     every backend reuses so all three converge on identical matches.
;;
;; This module owns no Org, model, or SQL specifics; the model,
;; plain-org, and Dolt adapter modules build on it, and `dorg-ql.el'
;; re-exports the public surface.

;;; Code:

(require 'cl-lib)
(require 'dorg-error)
(require 'dorg-timestamp)

(declare-function dorg-ext-query-field "dorg-ext" (name))
(declare-function dorg-get-extension "dorg-ext" (name))

(defvar dorg-ql--extension-field-plan nil
  "Dynamically bound field -> immutable descriptor copies for one query.
The public facade compiles this after AST normalization and binds it for the
complete invocation.  It is deliberately neither global nor persistent: an
extension registration replacement is observed by the next invocation.")

(defun dorg-ql--query-field (field)
  "Return FIELD's invocation descriptor, falling back before plan capture."
  (when field
    (let ((entry (assq field dorg-ql--extension-field-plan)))
      ;; A nil cdr is an intentional negative entry for a built-in field.  It
      ;; must not fall through to the registry for every selected heading.
      (if entry (cdr entry) (dorg-ext-query-field field)))))

(defun dorg-ql--compile-field-plan (ast)
  "Capture query metadata and owner identities for one invocation.
The plan includes direct fields and each referenced descriptor's declared
stored dependencies.  Owner identities are captured beside their descriptor
copies, before extension context callbacks can replace registrations."
  (let ((pending (dorg-ql--field-references ast)) plan identities)
    (while pending
      (let ((field (pop pending)))
        (unless (assq field plan)
          (let ((descriptor (dorg-ext-query-field field)))
            (push (cons field descriptor) plan)
            (when descriptor
              (let ((owner (plist-get descriptor :extension)))
                (unless (assq owner identities)
                  (push (cons owner (dorg-get-extension owner)) identities)))
              ;; Registration accepts stored dependencies only, so this is a
              ;; bounded expansion rather than an extension dependency graph.
              (dolist (dependency (plist-get descriptor :dependencies))
                (pcase dependency
                  (`(field ,stored) (push stored pending)))))))))
    (list :field-plan (nreverse plan)
          :extension-identities (nreverse identities))))

(defun dorg-ql--field-references (ast)
  "Return the distinct stored or extension fields referenced in normalized AST."
  (let (fields)
    (cl-labels ((walk (node)
                  (pcase node
                    ((or `(:field ,field) `(:cmp ,field . ,_))
                     (push field fields))
                    ((pred consp) (mapc #'walk node)))))
      (walk ast))
    (delete-dups (nreverse fields))))

(defun dorg-ql--extension-fields (ast)
  "Return the distinct registered fields referenced in normalized AST."
  (cl-remove-if-not #'dorg-ql--query-field (dorg-ql--field-references ast)))

(defconst dorg-query-identity-key-version 2
  "Persisted comparison-key codec version for identity-typed fields.
Extensions that store identity keys must store this version with their rows and
migrate old rows explicitly before comparing them with a new codec.")

(defun dorg-query-identity-key (value)
  "Return VALUE's case-equivalent identity key as UTF-8 hex bytes.
The v2 codec upcases each character as an integer under the standard case
table, then encodes that sequence as UTF-8 hex.  This is the equivalence used
by `string-equal-ignore-case', without whole-string case-expansion or
canonical-equivalence folding.  Non-Unicode/raw-byte characters are refused
before encoding because their UTF-8 bytes can collide with Unicode text.
Original display spelling is kept separately by extensions."
  (unless (stringp value) (dorg-ql--error "identity must be a string"))
  (let ((canonical
         (with-case-table (standard-case-table)
           (apply #'string
                  (mapcar
                   (lambda (char)
                     ;; An ASCII-only unibyte string denotes the same text as
                     ;; its multibyte form.  Other unibyte characters, and
                     ;; Emacs raw-byte characters (> U+10FFFF), do not have a
                     ;; Unicode identity and must not share UTF-8 byte keys.
                     (unless (and (<= 0 char #x10ffff)
                                  (not (<= #xd800 char #xdfff))
                                  (or (multibyte-string-p value) (<= char #x7f)))
                       (dorg-ql--unsupported
                        "identity contains a non-Unicode/raw-byte character: %S" char))
                     (upcase char))
                   (string-to-list value))))))
    (concat (format "v%d:" dorg-query-identity-key-version)
            (mapconcat (lambda (byte) (format "%02x" byte))
                       (encode-coding-string canonical 'utf-8 t) ""))))

(defun dorg-ql--validate-extension-scalar (field type value)
  "Return VALUE or refuse a declared FIELD value that violates TYPE."
  (unless (or (null value)
              (pcase type
                ((or 'string 'identity 'timestamp) (stringp value))
                ('integer (integerp value))
                ('boolean (memq value '(t :json-false)))))
    (dorg-ql--unsupported "field %S returned invalid %S value: %S"
                          field type value))
  value)

(defun dorg-ql--validate-extension-value (field value)
  "Validate FIELD's declared type and cardinality for returned VALUE."
  (when-let ((descriptor (dorg-ql--query-field field)))
    (let ((type (plist-get descriptor :type)))
      (if (eq (plist-get descriptor :cardinality) 'many)
          (progn
            (unless (listp value)
              (dorg-ql--unsupported "collection field %S returned non-list: %S"
                                    field value))
            (mapc (lambda (item)
                      ;; Collections distinguish [] from missing scalar; nil
                      ;; elements therefore have no representation.
                      (when (null item)
                        (dorg-ql--unsupported "collection field %S returned nil element" field))
                      (dorg-ql--validate-extension-scalar field type item))
                    value))
        (dorg-ql--validate-extension-scalar field type value))))
  value)

(defun dorg-ql--projection-columns (ast)
  "Return AST's terminal projection columns, or nil for heading results."
  (let ((last-step (car (last ast))))
    (and (eq (car-safe last-step) :select) (cdr last-step))))

(defun dorg-ql--selection-ast (ast)
  "Return AST's heading-selection steps, excluding terminal projection."
  (if (dorg-ql--projection-columns ast) (butlast ast) ast))

(defun dorg-ql--project-row (columns id marker reader)
  "Build selected COLUMNS using READER, retaining internal ID and MARKER.
READER receives each normalized value expression.  The :values alist is the
public row; identity and marker are separate navigation metadata."
  (list :heading-id id :marker marker
        :values
        (mapcar (lambda (column)
                  (let* ((expr (nth 2 column))
                         (value (funcall reader expr))
                         (field (and (eq (car-safe expr) :field) (nth 1 expr))))
                    (when field
                      (dorg-ql--validate-extension-value field value))
                    ;; Vectors preserve the public distinction between an
                    ;; empty collection ([] in JSON) and a missing scalar
                    ;; (null), while evaluators may naturally return lists.
                    (cons (nth 1 column)
                          (if (eq (plist-get (dorg-ql--query-field field) :cardinality)
                                  'many)
                              (vconcat value)
                            value))))
                columns)))

(defun dorg-ql-result-text (result)
  "Return tab-separated selected values for a projected query RESULT."
  (mapconcat (lambda (entry)
               (let ((value (cdr entry)))
                 (cond ((null value) "null")
                       ((eq value :json-false) "false")
                       ((eq value t) "true")
                       ((stringp value) value)
                       (t (prin1-to-string value)))))
             (plist-get result :values) "\t"))

;;; ─── Errors ───────────────────────────────────────────────────────────

(define-error 'dorg-ql-error "Invalid dorg-ql query" 'dorg-error)

(defconst dorg-ql--error-reasons '(invalid-query unsupported)
  "Closed set of public top-level reasons carried by `dorg-ql-error'.
`invalid-query' covers grammar, arity, value, and cardinality failures.
`unsupported' is reserved for a known source or backend capability refusal.")

(defun dorg-ql--raise (reason message &rest data)
  "Signal `dorg-ql-error' with REASON, MESSAGE, and optional plist DATA."
  (unless (memq reason dorg-ql--error-reasons)
    (error "Invalid dorg-ql error reason: %S" reason))
  (signal 'dorg-ql-error
          (list message (append (list :reason reason) data))))

(defun dorg-ql--error (fmt &rest args)
  "Signal an `invalid-query' error formatted from FMT and ARGS."
  (dorg-ql--raise 'invalid-query (apply #'format fmt args)))

(defun dorg-ql--unsupported (fmt &rest args)
  "Signal an `unsupported' source/capability refusal from FMT and ARGS."
  (dorg-ql--raise 'unsupported (apply #'format fmt args)))

(defun dorg-ql--assert-one (working)
  "Return WORKING when it holds exactly one heading, else signal `dorg-ql-error'.
The shared enforcement of the terminal `(:one)' pipeline step (the public
`(one QUERY)' cardinality wrapper): a count of 0 or >1 is a DATA fact,
distinct from the mutate-boundary `identity-violation'."
  (let ((n (length working)))
    (unless (= n 1)
      (dorg-ql--error "(one QUERY) requires exactly one result, got %d" n)))
  working)

;;; ─── Timestamp Predicate Metadata ─────────────────────────────────────

(defconst dorg-ql--timestamp-predicate-metadata
  '((scheduled   :arity window)
    (deadline    :arity window)
    (closed      :arity window)
    (property-ts :arity window-name)
    (logbook     :arity window)
    (active-ts   :arity window))
  "Single source of truth for timestamp-window predicate semantics.
Each entry is (PREDICATE . PLIST) recording only stable semantics, never
executable SQL or Org details:

- `:arity'   — argument shape: `window' for (PRED FROM TO), `window-name'
  for (PRED FROM TO [NAME]).

`dorg-ql--timestamp-predicates' and validation arity are both DERIVED
from this table, so the predicate set and the validator cannot drift
apart.  All six predicates lower to every buffer backend; the model-list
adapter answers every predicate too.")

(defun dorg-ql--timestamp-metadata (predicate)
  "Return the metadata plist for timestamp PREDICATE, or nil when unknown."
  (cdr (assq predicate dorg-ql--timestamp-predicate-metadata)))

(defconst dorg-ql--timestamp-predicates
  (mapcar #'car dorg-ql--timestamp-predicate-metadata)
  "Timestamp-window predicates over structured Org timestamps.
Derived from `dorg-ql--timestamp-predicate-metadata'.  Every predicate is
lowered to both buffer backends and answered by the model adapter.")

;;; ─── Identity Normalization ───────────────────────────────────────────

(defun dorg-ql--normalize-id (id)
  "Return ID as an integer, or signal `dorg-ql-error'.
Accepts an integer or a purely numeric string."
  (cond
   ((integerp id) id)
   ((and (stringp id) (string-match-p "\\`[0-9]+\\'" id))
    (string-to-number id))
   (t (dorg-ql--error "id must be an integer or numeric string: %S" id))))

;;; ─── Window Bounds & Forms ────────────────────────────────────────────

;; KEEP (test-seam): clock-injection seam; tests let-bind a fixed time for determinism.
(defvar dorg-ql-current-time nil
  "Reference time for the relative window forms `today' and `this-week'.
When nil, the real current time (`current-time') is used.  Bind to a
fixed Emacs time value in tests to make relative windows deterministic.")

(defcustom dorg-ql-week-start-day 1
  "First day of the week for the `this-week' window form.
An integer 0..6 where 0 is Sunday and 6 is Saturday; defaults to Monday."
  :type '(integer :tag "Weekday (0=Sunday .. 6=Saturday)")
  :group 'dorg)

(defconst dorg-ql--window-form-heads '(between on before after today this-week)
  "Recognized convenience window-form heads.
A timestamp predicate may take a single one of these forms in place of
its `FROM TO' bounds; each expands to a normalized `(FROM TO)' pair.")

(defun dorg-ql--bare-date (x)
  "Return X as a `(YEAR MONTH DAY)' triple when it is a bare date string.
X matches only a zero-padded `\"YYYY-MM-DD\"'; otherwise return nil."
  (when (and (stringp x)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" x))
    (list (string-to-number (match-string 1 x))
          (string-to-number (match-string 2 x))
          (string-to-number (match-string 3 x)))))

(defun dorg-ql--window-bound-span (x)
  "Return the `(START . END)' date triples of window value X, or nil for nil.
X is nil (an open bound), a bare `\"YYYY-MM-DD\"' string, a raw Org
active/inactive timestamp or `--' range string, or a `dorg-timestamp'
struct.  START is the value's start date; END its range end date, or
START when the value is not a range.  Active/inactive delimiters do not
affect the result.  Any other value, or an unparseable string, signals
`dorg-ql-error'."
  (cond
   ((null x) nil)
   ((dorg-timestamp-p x)
    (let ((s (dorg-timestamp-start-date x)))
      (unless s (dorg-ql--error "timestamp has no start date: %S" x))
      (cons s (or (dorg-timestamp-end-date x) s))))
   ((stringp x)
    (let ((bare (dorg-ql--bare-date x)))
      (if bare
          (cons bare bare)
        (let ((ts (and (not (string= x "")) (dorg-timestamp-parse x))))
          (if (and ts (dorg-timestamp-start-date ts))
              (cons (dorg-timestamp-start-date ts)
                    (or (dorg-timestamp-end-date ts)
                        (dorg-timestamp-start-date ts)))
            (dorg-ql--error
             (concat "window bound must be nil, a \"YYYY-MM-DD\" string, "
                     "a raw Org timestamp, or a dorg-timestamp: %S")
             x))))))
   (t (dorg-ql--error
       (concat "window bound must be nil, a \"YYYY-MM-DD\" string, "
               "a raw Org timestamp, or a dorg-timestamp: %S")
       x))))

(defun dorg-ql--normalize-window-bound (x &optional end)
  "Normalize window bound X to a `(YEAR MONTH DAY)' triple, or nil.
X is any value accepted by `dorg-ql--window-bound-span'.  With END
non-nil, a range value yields its end date; otherwise its start date.
nil yields nil (an open bound)."
  (let ((span (dorg-ql--window-bound-span x)))
    (and span (if end (cdr span) (car span)))))

(defun dorg-ql--reference-time ()
  "Return the reference time for relative window forms.
`dorg-ql-current-time' when bound, otherwise the real current time."
  (or dorg-ql-current-time (current-time)))

(defun dorg-ql--today ()
  "Return today's date as a `(YEAR MONTH DAY)' triple.
Today is taken from `dorg-ql--reference-time' so tests are deterministic."
  (let ((d (decode-time (dorg-ql--reference-time))))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun dorg-ql--date-shift (triple n)
  "Return date TRIPLE shifted by N days as a fresh `(YEAR MONTH DAY)' triple.
The shift is anchored at local noon so daylight-saving transitions never
move the result into an adjacent day."
  (let* ((time (encode-time
                (list 0 0 12 (nth 2 triple) (nth 1 triple) (nth 0 triple)
                      nil -1 nil)))
         (d (decode-time (time-add time (* n 86400)))))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun dorg-ql--this-week ()
  "Return the current week as a `(START END)' pair of date triples.
The week contains today (`dorg-ql--today') and starts on
`dorg-ql-week-start-day'; END is six days after START, inclusive."
  (let* ((today (dorg-ql--today))
         (time (encode-time
                (list 0 0 12 (nth 2 today) (nth 1 today) (nth 0 today)
                      nil -1 nil)))
         (dow (nth 6 (decode-time time)))
         (offset (mod (- dow dorg-ql-week-start-day) 7))
         (start (dorg-ql--date-shift today (- offset))))
    (list start (dorg-ql--date-shift start 6))))

(defun dorg-ql--window-form-p (x)
  "Non-nil when X is a convenience window form, e.g. `(today)' or `(on D)'.
Recognizes any list whose head is in `dorg-ql--window-form-heads'."
  (and (consp x) (memq (car x) dorg-ql--window-form-heads) t))

(defun dorg-ql--normalize-window-form (form)
  "Expand convenience window FORM to a normalized `(FROM TO)' triple pair.
FORM is one of `(between FROM TO)', `(on X)', `(before X)', `(after X)',
`(today)', or `(this-week)'.  `before' is on-or-before (open FROM) and
`after' is on-or-after (open TO), matching the inclusive `[FROM, TO]'
window matcher.  Bad arity or an unrecognized form signals
`dorg-ql-error'."
  (pcase form
    (`(between ,a ,b)
     (list (dorg-ql--normalize-window-bound a)
           (dorg-ql--normalize-window-bound b t)))
    (`(on ,x)
     (let ((span (dorg-ql--window-bound-span x)))
       (unless span (dorg-ql--error "(on X) requires a date: %S" form))
       (list (car span) (cdr span))))
    (`(before ,x)
     (let ((span (dorg-ql--window-bound-span x)))
       (unless span (dorg-ql--error "(before X) requires a date: %S" form))
       (list nil (cdr span))))
    (`(after ,x)
     (let ((span (dorg-ql--window-bound-span x)))
       (unless span (dorg-ql--error "(after X) requires a date: %S" form))
       (list (car span) nil)))
    (`(today) (let ((d (dorg-ql--today))) (list d d)))
    (`(this-week) (dorg-ql--this-week))
    (_ (dorg-ql--error "Malformed window form: %S" form))))

(defun dorg-ql--normalize-window-spec (head spec query)
  "Normalize SPEC, a window predicate's bound or window-form arguments.
SPEC is HEAD's argument list with any trailing NAME removed; it is either
two bounds `(FROM TO)' or a single window form `((today))', `((on X))',
etc.  Return a normalized `(FROM TO)' triple pair.  Bad arity or shape
signals `dorg-ql-error', and HEAD/QUERY appear in the message."
  (cond
   ((and (= (length spec) 1) (dorg-ql--window-form-p (car spec)))
    (dorg-ql--normalize-window-form (car spec)))
   ((= (length spec) 2)
    (list (dorg-ql--normalize-window-bound (nth 0 spec))
          (dorg-ql--normalize-window-bound (nth 1 spec) t)))
   (t (dorg-ql--error
       "(%s ...) takes FROM and TO bounds or a single window form: %S"
       head query))))

;;; ─── Result Shape ─────────────────────────────────────────────────────

(defun dorg-ql--make-result (&rest plist)
  "Build a query result plist from PLIST.
Normalizes missing keys to nil and returns a fresh list with exactly
`:heading-id', `:marker', `:title', and `:properties'."
  (list :heading-id  (plist-get plist :heading-id)
        :marker      (plist-get plist :marker)
        :title       (plist-get plist :title)
        :properties  (plist-get plist :properties)))

;;; ─── Timestamp Windows ────────────────────────────────────────────────

(defun dorg-ql--date<= (a b)
  "Non-nil when date triple A is on or before date triple B.
A and B are `(YEAR MONTH DAY)' integer lists."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
           (or (< (nth 1 a) (nth 1 b))
               (and (= (nth 1 a) (nth 1 b))
                    (<= (nth 2 a) (nth 2 b)))))))

(defun dorg-ql--timestamp-in-window-p (ts from to)
  "Non-nil when TS's date span overlaps the window [FROM, TO].
TS is a `dorg-timestamp'.  FROM and TO are `(YEAR MONTH DAY)' triples or
nil (an open bound).  The span is [start-date .. (end-date|start-date)]
inclusive; clock times are ignored.  Repeaters are matched by their base
occurrence only — no calendar expansion is performed here."
  (let* ((s (dorg-timestamp-start-date ts))
         (e (or (dorg-timestamp-end-date ts) s)))
    (and s
         (or (null to)   (dorg-ql--date<= s to))
         (or (null from) (dorg-ql--date<= from e)))))

(provide 'dorg-ql-core)
;;; dorg-ql-core.el ends here
