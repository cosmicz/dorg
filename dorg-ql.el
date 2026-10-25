;;; dorg-ql.el --- Core query API for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small, backend-agnostic query core for dorg.  `dorg-ql-select'
;; answers one validated query — a historical atomic form or a composable
;; v2 form — against any of three sources:
;;
;;   - a plain-org file buffer (walked with org APIs, returning live
;;     markers);
;;   - a list of `dorg-heading' model structs (markerless results);
;;   - a Dolt-backed registration (lowered to SQL over the registered
;;     root_id, markerless results).
;;
;; Every result is a plist with exactly the keys `:heading-id',
;; `:marker', `:title', and `:properties' — the same stable shape
;; produced by `dorg-find'.  Plain-org results carry live markers;
;; model and Dolt results carry nil markers.
;;
;; This file is the PUBLIC FACADE.  It owns query dispatch — registration
;; lookup, effective-backend identity, the timestamp capability gate, and
;; `dorg-ql-select' — and re-exports `dorg-ql-error'.  Every public query,
;; historical atomic alias or composable v2 form, is normalized to ONE
;; backend-neutral AST (`dorg-ql-normalize', dorg-ql-ast) and dispatched to
;; ONE executor per source; there is no separate atomic execution path.
;; The adapter implementations live in internal modules loaded before it:
;;
;;   - `dorg-ql-core'  — errors, the timestamp predicate metadata that is
;;     the single source of truth for the predicate set, capability table,
;;     and normalization arity, the shared window-bound normalizers, the
;;     result-plist constructor, and the shared date-window predicate;
;;   - `dorg-ql-model' — the model-list evaluator `dorg-ql--model-eval' (the
;;     semantic baseline) and the shared leaf matchers the plain-org and
;;     Dolt adapters reuse;
;;   - `dorg-ql-plain' — the plain-org buffer executor
;;     (`dorg-ql--plain-eval'), which runs a normalized AST over a light
;;     headline-only buffer index reusing the model evaluator helpers, and
;;     whose timestamp predicates reuse parser-equivalent section projection
;;     (`dorg-parse-section-items-at-heading') plus the shared model
;;     timestamp matcher so they cannot drift from the parser;
;;   - `dorg-ql-dolt'  — the Dolt SQL adapter (root-scoped,
;;     candidate-bounded, no whole-tree materialization, no SQL
;;     date-window filtering), whose v2 planner keeps a bounded-SQL
;;     point-lookup fast path for the six exact single-filter forms.
;;
;; The historical ATOMIC query forms (still valid, and reused as v2
;; aliases) are:
;;
;;   (id ID)             exact DORG_ID match.  ID is an integer or a
;;                       numeric string normalized to an integer.
;;   (property KEY VALUE) exact, non-inherited custom PROPERTIES match.
;;                       KEY and VALUE are strings.
;;   (title TITLE)       exact heading-title match.  TITLE is a string.
;;   (tag TAG)           exact local-tag match (no inheritance).  TAG is
;;                       a string.
;;
;; Timestamp-window predicates select headings whose structured Org
;; timestamps fall in a date window.  FROM and TO are each a
;; "YYYY-MM-DD" string or nil (an open bound):
;;
;;   (scheduled FROM TO)   heading SCHEDULED planning timestamp in window.
;;   (deadline FROM TO)    heading DEADLINE planning timestamp in window.
;;   (closed FROM TO)      heading CLOSED planning timestamp in window.
;;   (active-ts FROM TO)   ANY active body timestamp overlaps the window.
;;   (logbook FROM TO)     ANY LOGBOOK entry timestamp overlaps the window.
;;   (property-ts FROM TO [NAME])
;;                         ANY property-drawer timestamp overlaps the
;;                         window; with NAME (a string) the match is
;;                         restricted to that property key.
;;
;; Window semantics: a timestamp covers the date span
;; [start-date .. (end-date|start-date)], inclusive, at DATE granularity
;; (clock times are ignored for window matching).  It matches when that
;; span overlaps [FROM, TO].  This module does NOT own calendar
;; expansion: repeaters match by their base (literal start-date)
;; occurrence only, with no future projection — recurrence/agenda policy
;; belongs downstream.
;;
;; All six timestamp-window predicates lower to both the plain-org and
;; Dolt buffer adapters; the model-list adapter answers all of them too.
;; All lowered paths reuse the one parser (`dorg-timestamp-parse') and the
;; one window predicate (`dorg-ql--timestamp-in-window-p'), so the three
;; backends produce identical matches and no lowered path materializes the
;; whole tree.  A query on any other backend fails closed with
;; `dorg-ql-error' rather than silently widening or emptying the result.
;;
;; Unsupported predicates, wrong arity, and wrong argument types signal
;; `dorg-ql-error'.  Invalid forms never fall back to a broad scan.
;;
;; Composable v2 forms — `(pipe STEP...)', the boolean combinators
;; (`and'/`or'/`not'), `field', `traverse', extended `id'/`property'/`tag',
;; and `timestamp' — are dispatched here too: `dorg-ql-select' normalizes
;; them through `dorg-ql-normalize' and routes them to the model evaluator,
;; the Dolt planner, or the plain-org executor.  Fuzzy matching, updates,
;; and Clime commands remain out of scope here.
;;
;; `dorg-ql' must not depend on `dorg-find': `dorg-find' is the stable
;; by-id/by-property facade and is free to route through this core, so
;; the dependency points one way only.

;;; Code:

(require 'dorg-backend)
(require 'dorg-registry)
(require 'dorg-ql-core)
(require 'dorg-ql-ast)
(require 'dorg-ql-eval)
(require 'dorg-ql-model)
(require 'dorg-ql-plain)
;; `(key V)' uses the mutation source resolver even for plain-org reads.
(require 'dorg-mutate)
;; B8/D6: the Dolt SQL adapter is loaded ON THE DOLT PATH ONLY — its entry point
;; autoloads `dorg-ql-dolt' (which pulls `dorg-dolt') when a Dolt-backed query
;; first dispatches, so a plain-org/model `(require 'dorg-ql)' stays Dolt-free.
;; `dorg-ql--dolt-eval' is the single Dolt entry point; loading it defines the
;; whole adapter (including the point-lookup `dorg-ql--dolt-select' it reaches
;; internally), so no separate `dorg-ql--dolt-select' autoload is needed.
(autoload 'dorg-ql--dolt-eval "dorg-ql-dolt")
(autoload 'dorg-ql--dolt-validate-extension-memberships "dorg-ql-dolt")
(autoload 'dorg-ql--dolt-bm25-search "dorg-ql-dolt")
(declare-function dorg-head-commit "dorg-dolt")
(autoload 'dorg-repo-root-id "dorg-repo")

(defun dorg-ql--resolved-dolt-registration (registration revision)
  "Return REGISTRATION with a root suitable for REVISION-bound QL reads.
An existing positive root remains authoritative.  A classification-only nil
root is resolved read-only at REVISION and never creates a binding."
  (let* ((backend (dorg-registration-backend registration))
         (root (dorg-backend-root-id backend)))
    (if root
        registration
      (let ((resolved (dorg-repo-root-id (dorg-backend-dolt-dir backend)
                                         (dorg-registration-file registration)
                                         revision)))
        (unless resolved
          (signal 'dorg-binding-error
                  (list (format "Dolt source %s has no root at %s; run dorg-bind-dolt"
                                (dorg-registration-file registration) revision)
                        (list :reason 'missing-root
                              :dolt-dir (dorg-backend-dolt-dir backend)
                              :file (dorg-registration-file registration)
                              :revision revision))))
        (make-dorg-registration
         :backend (dorg-backend-dolt (dorg-backend-dolt-dir backend) resolved)
         :file (dorg-registration-file registration)
         :identity (dorg-registration-identity registration))))))

(defun dorg-ql--context-digest (value)
  "Return a stable digest for the read/print-safe context VALUE."
  (condition-case err
      (let* ((print-length nil) (print-level nil) (print-circle nil)
             (printed (prin1-to-string value)))
        ;; Round-tripping rejects markers, buffers, closures, and other host
        ;; state before it can enter an envelope or a warm cache key.
        (read printed)
        (secure-hash 'sha256 printed))
    (error (dorg-ql--unsupported "extension query context is not host-safe: %s"
                                 (error-message-string err)))))

(defun dorg-ql--copy-context-value (value)
  "Return an isolated copy of read/print-safe extension context VALUE."
  (cond ((or (hash-table-p value) (recordp value))
         (dorg-ql--unsupported "extension query context contains mutable host data"))
        ((stringp value) (copy-sequence value))
        ((vectorp value) (vconcat (mapcar #'dorg-ql--copy-context-value value)))
        ((consp value) (cons (dorg-ql--copy-context-value (car value))
                             (dorg-ql--copy-context-value (cdr value))))
        (t value)))

(defun dorg-ql--resolve-extension-contexts (ast invocation)
  "Capture immutable context bundles needed by extension fields in AST.
An extension's `:query-context' callback receives INVOCATION once per public
invocation.  INVOCATION carries the captured `:revision', source registration,
root, and a revision-bound `:read' function.  It returns a plist whose
`:values' are passed to field evaluators/lowerers and whose remaining,
read/print-safe provenance participates in the context digest."
  (let (contexts)
    (dolist (field (dorg-ql--extension-fields ast))
      (let* ((descriptor (dorg-ql--query-field field))
             (extension (plist-get descriptor :extension))
             (needs-context (plist-get descriptor :context))
             (existing (assq extension contexts)))
        (when (and needs-context (not existing))
          (let* ((registered (dorg-get-extension extension))
                 (resolve (and registered
                               (dorg-extension-query-context registered))))
            (unless resolve
              (dorg-ql--unsupported
               "field %S requires query context but extension %S has no resolver"
               field extension))
            (let ((bundle (funcall resolve invocation)))
              (unless (and (listp bundle) (plist-member bundle :values))
                (dorg-ql--unsupported
                 "extension %S returned invalid query context for field %S"
                 extension field))
              ;; Validate first so cyclic input fails before the deep copy,
              ;; then digest the isolated representation.  Context identity
              ;; must describe values, not the resolver's object sharing.
              (dorg-ql--context-digest bundle)
              (let ((captured (dorg-ql--copy-context-value bundle)))
                (push (cons extension
                            (list :values (plist-get captured :values)
                                  :digest (dorg-ql--context-digest captured)))
                      contexts)))))))
    (setq contexts (nreverse contexts))
    (list :contexts contexts
          :digest (and contexts
                       (dorg-ql--context-digest
                        (mapcar (lambda (entry)
                                  (list (car entry)
                                        (plist-get (cdr entry) :digest)))
                                contexts))))))

(defun dorg-ql--make-invocation (source &optional revision)
  "Build an extension invocation capability for SOURCE at REVISION.
For a Dolt source, `:read' executes every resolver statement under the exact
captured revision.  Resolver output, not this capability record, is retained
in result envelopes and warm cache keys."
  (let* ((reg (and (not (and source (consp source)))
                   (dorg-ql--registration source)))
         (backend (and reg (dorg-registration-backend reg)))
         (type (and backend (dorg-ql--effective-backend-type backend)))
         (dolt-dir (and backend (dorg-backend-dolt-dir backend))))
    (list :source source :registration reg :backend type :revision revision
          :root (and backend (dorg-backend-root-id backend))
          :read (and (eq type 'dolt)
                     revision
                     (lambda (sql &rest args)
                       (apply #'dorg-ql--dolt-query dolt-dir revision sql args))))))

(defun dorg-ql--validate-canonical-string-leaves (ast field-plan)
  "Reject legacy string leaves in AST once, using FIELD-PLAN descriptors.
The public normalizer always emits a `:case' mode for a string comparison.
Keeping this defensive invariant at preparation time avoids a registry lookup
per selected heading when a caller supplies an internal AST directly."
  (let ((dorg-ql--extension-field-plan field-plan))
    (cl-labels ((walk (node)
                  (pcase node
                    (`(:cmp ,field ,_op ,_value)
                     (when (dorg-ql--string-field-p field)
                       (dorg-ql--error
                        "string comparison lacks canonical :case mode: %S" node)))
                    ((or `(:property ,_ ,op ,_value)
                         `(:keyword ,_ ,op ,_value)
                         `(:tag ,op ,_value))
                     (unless (memq op '(exists missing))
                       (dorg-ql--error
                        "string comparison lacks canonical :case mode: %S" node)))
                    ((pred consp) (mapc #'walk node)))))
      (walk ast))))

(defun dorg-ql--prepare-query (query source &optional invocation)
  "Normalize QUERY and capture its invocation context once for SOURCE.
The returned internal record is deliberately consumed by the public facade and
the warm session layer together, so a cache-key probe cannot resolve workflow
configuration separately from the query it subsequently serves."
  (setq query (dorg-ql--resolve-key-refs query source))
  (let* ((ast (dorg-ql-normalize query))
         (field-plan-record (dorg-ql--compile-field-plan ast))
         (field-plan (plist-get field-plan-record :field-plan))
         (_canonical-leaves
          (dorg-ql--validate-canonical-string-leaves ast field-plan))
         (extension-identities
          (plist-get field-plan-record :extension-identities))
         ;; Model sources have no deferred backend capture.  Buffer sources
         ;; resolve only after `dorg-ql--buffer-select-v2' captures HEAD,
         ;; unless a warm session supplied that already-captured invocation.
         (context (let ((dorg-ql--extension-field-plan field-plan))
                    (and (or invocation (and source (consp source)))
                         (dorg-ql--resolve-extension-contexts
                          ast (or invocation (list :source source)))))))
    (list :ast ast
          :field-plan field-plan
          :extension-identities extension-identities
          :invocation invocation
          :revision (plist-get invocation :revision)
          :context context
          :contexts (plist-get context :contexts)
          :context-digest (plist-get context :digest))))

;;; ─── Dispatch ─────────────────────────────────────────────────────────

(defun dorg-ql--registration (buffer)
  "Return the registration for BUFFER or signal if absent.
BUFFER may already BE a `dorg-registration' — an internal, possibly
UNREGISTERED binding synthesized by a caller such as `dorg-api--ql-source'
for a no-buffer Dolt source — in which case it is returned as-is (no buffer
lookup, no `dorg--registry' access).  Otherwise the registration is looked up
from BUFFER (or the current buffer).  This surfaces backend/registration
errors with a plain `error', distinct from the `dorg-ql-error' used for
invalid query forms."
  (if (dorg-registration-p buffer)
      buffer
    ;; Strict: a query executes against the backing store, so a keyword Dolt
    ;; buffer must be bound to a real root_id — ensure, not classify.
    (let ((reg (with-current-buffer (or buffer (current-buffer))
                 (dorg--ensure-registration))))
      (unless reg
        (error "dorg-ql: buffer %s has no registered backing storage"
               (buffer-name (or buffer (current-buffer)))))
      reg)))

(defun dorg-ql--effective-backend-type (backend)
  "Return the dispatch identity for BACKEND: `plain-org', `dolt', or nil.
Drives `dorg-ql--buffer-select-v2' dispatch so the capability gate and the
adapter dispatch agree on one backend identity: a registration with no
explicit type but a Dolt directory dispatches — and is gated — as
`dolt', so legacy Dolt registrations using the fallback are not treated
as an unknown unsupported pair.  Returns nil when BACKEND has neither a
known type nor a Dolt directory."
  (pcase (and backend (dorg-backend-type backend))
    ('plain-org 'plain-org)
    ('dolt      'dolt)
    (_ (and backend (dorg-backend-dolt-dir backend) 'dolt))))

(defun dorg-ql--snapshot-source (registration type)
  "Return explicit public source identity for REGISTRATION of backend TYPE.
Files and Dolt stores are canonicalized only when supplied by the registration;
non-file library sources are never assigned an invented path."
  (let* ((backend (dorg-registration-backend registration))
         (file (dorg-registration-file registration))
         (store (and (eq type 'dolt) (dorg-backend-dolt-dir backend))))
    (append (list :kind type)
            (when file (list :file (file-truename file)))
            (when store (list :store (file-truename store))))))

(defun dorg-ql--plain-content-snapshot (buffer)
  "Capture BUFFER's exact UTF-8 text identity before a plain query reads it."
  (with-current-buffer buffer
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (list :tick (buffer-chars-modified-tick)
            :text text
            :content-sha256
            (secure-hash 'sha256 (encode-coding-string text 'utf-8 t))))))

(defun dorg-ql--plain-content-stable-p (buffer captured)
  "Return non-nil when BUFFER still has CAPTURED's exact text and edit tick."
  (with-current-buffer buffer
    (and (= (plist-get captured :tick) (buffer-chars-modified-tick))
         (equal (plist-get captured :text)
                (buffer-substring-no-properties (point-min) (point-max))))))

(defun dorg-ql--buffer-select-v2 (ast buffer &optional snapshot-p prepared)
  "Run normalized v2 AST against BUFFER's backend, or fail closed.
Every buffer query — a historical atomic alias or a composable v2 form —
reaches this one dispatcher as the normalized AST from `dorg-ql-normalize'.
Queries are supported on a Dolt backend (the Dolt planner) and a plain-org
backend (the plain-org executor); all six timestamp predicates lower to
both.  On any other backend they signal `dorg-ql-error' rather than
widening or silently emptying the result."
  (let* ((reg (dorg-ql--registration buffer))
         (backend (dorg-registration-backend reg))
         (type (dorg-ql--effective-backend-type backend)))
    (pcase type
      ('dolt
       (dorg-ql--dolt-validate-extension-memberships ast)
       (let* ((dolt-dir (dorg-backend-dolt-dir backend))
              ;; Capture exactly once before any data read.  A capture error
              ;; escapes before the adapter is entered.
              (revision (or (plist-get prepared :revision)
                            (dorg-head-commit dolt-dir)))
              (reg (dorg-ql--resolved-dolt-registration reg revision))
              (backend (dorg-registration-backend reg))
              (invocation (or (plist-get prepared :invocation)
                              (dorg-ql--make-invocation reg revision)))
              (context (or (plist-get prepared :context)
                           (dorg-ql--resolve-extension-contexts ast invocation)))
              (dorg-ql--extension-contexts (plist-get context :contexts))
              (rows (dorg-ql--dolt-eval ast reg revision)))
         (if snapshot-p
             (list :rows rows
                   :source (dorg-ql--snapshot-source reg type)
                   :root (dorg-backend-root-id backend)
                   :snapshot (list :kind 'dolt :head revision)
                   :context-digest (plist-get context :digest))
           rows)))
      ('plain-org
       (let* ((plain-buffer (dorg-ql--plain-org-buffer reg))
              ;; A snapshot identifies precisely the text the planner reads.
              ;; If a callback edits the live buffer while it runs, refuse
              ;; rather than report this hash for different result rows.
              (content (and snapshot-p
                            (dorg-ql--plain-content-snapshot plain-buffer)))
              (invocation (or (plist-get prepared :invocation)
                              (dorg-ql--make-invocation buffer nil)))
              (context (or (plist-get prepared :context)
                           (dorg-ql--resolve-extension-contexts ast invocation)))
              (dorg-ql--extension-contexts (plist-get context :contexts))
              (rows (dorg-ql--plain-eval ast reg plain-buffer)))
         (if snapshot-p
             (progn
               (unless (dorg-ql--plain-content-stable-p plain-buffer content)
                 (dorg-ql--unsupported
                  "plain-org source changed during snapshot evaluation"))
               (list :rows rows
                     :source (dorg-ql--snapshot-source reg type)
                     :root nil
                     :snapshot (list :kind 'plain-org
                                     :content-sha256 (plist-get content :content-sha256))
                     :context-digest (plist-get context :digest)))
           rows)))
      (_ (dorg-ql--unsupported
          "dorg-ql queries are not supported on the %s backend"
          (or type "unknown"))))))

;;; ─── (key V) identity selector: the source-context seam ──────────────────
;; `(key V)' is a value-only identity predicate in the shared selector
;; sublanguage: it matches the heading whose binding identity equals V.  The
;; identity key NAME is NOT in the form; it is resolved from the SOURCE's
;; normalized identity policy through the single resolver
;; `dorg-mutate--source-identity' (the one seam both the read verb and the mutate
;; projection anchor on).  Under a (:property K) policy it lowers to a match on
;; that PROPERTIES key; under a `dorg' policy it lowers to the `(id V)' identity
;; predicate (DORG_ID owned by `id', never a property scan).  A model-list /
;; unregistered source has no binding => `(key V)' is UNSUPPORTED (a loud
;; `dorg-ql-error'), never degraded to a fallback scan.

(defun dorg-ql--query-references-key-p (form)
  "Non-nil when query FORM contains a well-shaped `(key V)' selector.
Reject dotted lists and malformed key selectors before identity lookup.  This
walks raw input, so ordinary list helpers cannot safely assume a proper list."
  (when (consp form)
    (let ((tail form)
          (found nil))
      (when (eq (car form) 'key)
        (unless (and (consp (cdr form))
                     (null (cddr form))
                     (stringp (cadr form)))
          (dorg-ql--error "(key VALUE) requires exactly one string value: %S"
                          form))
        (setq found t))
      (while (consp tail)
        (when (dorg-ql--query-references-key-p (car tail))
          (setq found t))
        (setq tail (cdr tail)))
      (unless (null tail)
        (dorg-ql--error "query contains an improper list: %S" form))
      found)))

(defun dorg-ql--identity-key-spec (source)
  "Resolve the identity spec for a `(key V)' selector against SOURCE.
Consults the SINGLE resolver `dorg-mutate--source-identity' (one identity
interpretation, D4).  Returns (KEY . KIND): a (:property K) policy -> (K
. property); a `dorg' policy -> (nil . dorg); a `none' policy signals
`dorg-ql-error' carrying (:reason unsupported :detail identity-none) — never a
fallback property/id scan.  A raw model-list source (a heading list) or an
unregistered/unbound source carries no binding identity and is a `dorg-ql-error'."
  (let* ((s (cond
             ((dorg-registration-p source)
              ;; A synthesized/registered binding: route it through the SAME
              ;; resolver seam, UNCAUGHT (no default collapse), so a direct or
              ;; embedded `(key V)' on a registration source resolves identity.
              (dorg-mutate--source-from-registration source nil))
             ((and (consp source) (keywordp (car source)))
              (dorg-mutate--resolve-source source))       ; descriptor plist
             ((consp source)
              (dorg-ql--unsupported
               "(key V) is unsupported on a model-list source: no binding identity"))
             (t
              (let ((reg (condition-case nil (dorg-ql--registration source) (error nil))))
                (unless reg
                  (dorg-ql--unsupported
                   "(key V) requires a registered binding with an identity"))
                (dorg-mutate--source-from-registration
                 reg (and (bufferp source) source))))))
         (policy (dorg-mutate--source-identity s)))
    (pcase policy
      (`(:property ,k) (cons k 'property))
      ('dorg (cons nil 'dorg))
      ('none (dorg-ql--raise
              'unsupported
              "(key V) is unavailable under identity=none"
              :detail 'identity-none)))))

(defun dorg-ql--rewrite-key-refs (form spec)
  "Rewrite each `(key V)' predicate in query FORM using SPEC (KEY . KIND).
KIND `property' -> `(property KEY V)'; `dorg' -> `(id V)'."
  (pcase form
    (`(key ,v)
     (unless (stringp v)
       (dorg-ql--error "(key VALUE) requires a string value: %S" form))
     (pcase (cdr spec)
       ('property    (list 'property (car spec) v))
       ('dorg        (list 'id v))
       (_ (dorg-ql--error "unresolved identity-key spec: %S" spec))))
    ((and `(key . ,_))
     (dorg-ql--error "(key V) takes exactly one value: %S" form))
    ((pred consp)
     (cons (car form)
           (mapcar (lambda (a) (dorg-ql--rewrite-key-refs a spec)) (cdr form))))
    (_ form)))

(defun dorg-ql--resolve-key-refs (query source)
  "Return QUERY with every `(key V)' selector resolved against SOURCE.
No-op (and no registration lookup) when QUERY references no `(key …)'."
  (if (dorg-ql--query-references-key-p query)
      (dorg-ql--rewrite-key-refs query (dorg-ql--identity-key-spec source))
    query))

;;;###autoload
(defun dorg-ql--execute-prepared (prepared source &optional snapshot-p)
  "Execute PREPARED query once, preserving its captured context bundle."
  (let ((dorg-ql--extension-contexts (plist-get prepared :contexts))
        (dorg-ql--extension-field-plan (plist-get prepared :field-plan))
        (ast (plist-get prepared :ast)))
    (if (and source (consp source))
        (if snapshot-p
            (dorg-ql--unsupported "snapshot reads require a Dolt-backed registration")
          (dorg-ql--model-eval ast source))
      (dorg-ql--buffer-select-v2 ast source snapshot-p prepared))))

;;;###autoload
(defun dorg-ql-select (query &optional source prepared)
  "Run one validated QUERY (atomic or composable v2) and return result plists.

An atomic QUERY is one of: (id ID), (property KEY VALUE), (title TITLE),
(tag TAG), or a timestamp-window form — (scheduled FROM TO),
(deadline FROM TO), (closed FROM TO), (active-ts FROM TO),
(logbook FROM TO), or (property-ts FROM TO [NAME]) — where FROM and TO
are each a \"YYYY-MM-DD\" string or nil.  Invalid predicates, arity, or
argument types signal `dorg-ql-error'.

The model adapter answers every timestamp-window predicate.  For a
plain-org or Dolt buffer, the planning predicates `scheduled',
`deadline', `closed', plus `property-ts', `logbook', and `active-ts' are
all lowered to both backends.  A query on any other backend signals
`dorg-ql-error' rather than silently widening or emptying the result.

SOURCE selects the adapter:
  - nil       — the current buffer, dispatched via its registered
                backend (plain-org or Dolt);
  - a buffer  — that buffer, dispatched via its registered backend;
  - a `dorg-registration' struct — an internal, possibly UNREGISTERED
                binding dispatched via its own backend (no buffer/registry
                lookup).  Used by mutate target expansion to query a
                no-buffer Dolt source (see `dorg-api--ql-source');
  - a non-empty list of `dorg-heading' structs — the model adapter.
                For composable v2, this list is an ordered forest:
                filter-only pipelines run over the supplied order, while
                traversal and `unique' require unique non-nil ids.

Without a terminal `select' step, each result is a plist with `:heading-id',
`:marker', `:title', and `:properties'.  A selected row retains internal
`:heading-id'/`:marker' navigation metadata and exposes only the requested
ordered `:values' alist.  Plain-org results carry live markers; model and Dolt
results carry nil markers.  An empty match returns nil.

Both the historical atomic aliases and the composable v2 forms —
`(pipe STEP...)', boolean combinators, `field', extended
`id'/`property'/`keyword'/`tag', and `timestamp' — run through ONE path:
`dorg-ql-normalize' lowers every form into the same backend-neutral AST,
which the model evaluator, Dolt planner, or plain-org executor consumes.
Malformed forms always signal `dorg-ql-error'.  The query form is
normalized (and thereby validated) up front, so an invalid form fails
the same way regardless of backing storage.

EXCEPTION for `(key V)' identity selectors: those are resolved against
SOURCE by `dorg-ql--resolve-key-refs' BEFORE normalization, and that
resolution may consult the source's registration
(`dorg-ql--identity-key-spec' -> `dorg-ql--registration') to pick the
identity predicate.  So a query containing `(key V)' can consult — or
fail on — registration before it is normalized; only key-free queries
are normalized before any registration lookup.  This ordering is
deliberate and preserved until [[tracked-regression]]."
  ;; Resolve `(key V)' identity selectors against SOURCE first (the seam),
  ;; so the rest of the pipeline sees only concrete property/id predicates.
  ;; This step may consult SOURCE's registration to choose the identity
  ;; predicate, so a `(key V)' query touches registration BEFORE it is
  ;; normalized (preserved until [[tracked-regression]]).
  ;; Then normalize to the one v2 AST — this both validates the form (for a
  ;; key-free query, bad forms signal `dorg-ql-error' before any registration
  ;; lookup) and lowers the historical atomic aliases onto the same executor
  ;; path the composable forms use, so there is no second atomic execution
  ;; branch.
  (dorg-ql--execute-prepared (or prepared (dorg-ql--prepare-query query source))
                             source))

;;;###autoload
(defun dorg-ql-select-snapshot (query &optional source)
  "Run QUERY through the public QL pipeline and return rows plus snapshot.
The result is `(:rows ROWS :source SOURCE :root ROOT-ID :snapshot SNAPSHOT
:context-digest DIGEST)'.  SOURCE identifies its backend kind and canonical
file when present (plus the Dolt store); SNAPSHOT is `(:kind dolt :head COMMIT)'
or `(:kind plain-org :content-sha256 SHA256)'.  The Dolt token is captured once
before any data statement; plain snapshots refuse a source changed while the
query evaluates.  Extension context remains a separate identity."
  (dorg-ql--execute-prepared (dorg-ql--prepare-query query source) source t))

(defun dorg-ql--rank-bm25-step (ast)
  "Return AST's one normalized BM25 rank step, or nil.

Placement and uniqueness are normalizer invariants.  Keeping this extraction at
the public search boundary prevents the ordinary select dispatcher from gaining
a second result shape for a terminal rank operation."
  (cl-find-if (lambda (step) (eq (car-safe step) :rank-bm25))
              (and (consp ast) (cddr ast))))

;;;###autoload
(defun dorg-ql-search-snapshot (query &optional source)
  "Run terminal BM25 QUERY and return its revision-bound search envelope.

QUERY must contain one normalized terminal `(rank-bm25 ...)' step.  Ranked
results are deliberately a sibling public envelope rather than
`dorg-ql-select' rows: ordinary select keeps its stable four-key result plist
shape.  The returned envelope names the captured source/root/snapshot, lexical
profile, immutable index and corpus generations, and ordered `:matches'; each
match carries canonical `:heading-id', title, and rounded numeric `:score'.

Only a registered Dolt source is currently supported.  Plain and model sources
signal typed QL unsupported rather than silently substituting a scan or a
different ranker.  The conventional BM25 extension loader is entered only
after grammar validation and backend admission; a missing or incompatible
derived index is handled by the Dolt read adapter as a typed unavailable
outcome."
  (let* ((prepared (dorg-ql--prepare-query query source))
         (ast (plist-get prepared :ast))
         (rank (dorg-ql--rank-bm25-step ast)))
    (unless rank
      (dorg-ql--error "search snapshot requires one terminal rank-bm25 step: %S"
                      query))
    (when (and source (consp source))
      (dorg-ql--unsupported
       "BM25 ranked search is unavailable for model-list sources"))
    (let* ((reg (dorg-ql--registration source))
           (backend (dorg-registration-backend reg))
           (type (dorg-ql--effective-backend-type backend)))
      (unless (eq type 'dolt)
        (dorg-ql--unsupported "BM25 ranked search is unavailable for %s sources"
                              (or type "unknown")))
      ;; Registration is explicit and lazy: unrelated DOrg stores do not need
      ;; the optional schema merely because they load the public QL facade.
      (dorg-load-extension "bm25")
      (let* ((dolt-dir (dorg-backend-dolt-dir backend))
             (revision (dorg-head-commit dolt-dir))
             (reg (dorg-ql--resolved-dolt-registration reg revision))
             (result (let ((dorg-ql--extension-contexts
                            (plist-get prepared :contexts))
                           (dorg-ql--extension-field-plan
                            (plist-get prepared :field-plan)))
                       (dorg-ql--dolt-bm25-search ast reg revision)))
             ;; The extension reader owns the SQL snapshot pin.  On a normal
             ;; read it equals the captured head; if it captured a later
             ;; committed token, report that token with its rows rather than
             ;; falsely labelling them as the earlier QL probe.
             (snapshot (or (plist-get result :snapshot) revision)))
        (list :command 'search
              :source (dorg-ql--snapshot-source reg type)
              :root (dorg-backend-root-id (dorg-registration-backend reg))
              :snapshot (list :kind 'dolt :head snapshot)
              :profile (plist-get result :profile)
              :index-generation (plist-get result :index-generation)
              :corpus-generation (plist-get result :corpus-generation)
              :matches (plist-get result :matches)
              :count (length (plist-get result :matches)))))))

;;;###autoload
(defun dorg-ql-snapshot-public (envelope)
  "Return JSON-ready public query data from one internal snapshot ENVELOPE.
This is the common pure boundary for DOrg and downstream CLI wrappers.  It
removes internal result markers, preserves requested string column/property
names exactly, and retains source, root, typed snapshot, context identity,
results, and count."
  (let ((results
         (vconcat
          (mapcar
           (lambda (row)
             (if (plist-member row :values)
                 (plist-get row :values)
               (list :heading-id (plist-get row :heading-id)
                     :title (or (plist-get row :title) "")
                     :properties (plist-get row :properties))))
           (plist-get envelope :rows)))))
    (list :command 'query
          :source (plist-get envelope :source)
          :root (plist-get envelope :root)
          :snapshot (plist-get envelope :snapshot)
          :context-digest (plist-get envelope :context-digest)
          :results results :count (length results))))

(provide 'dorg-ql)
;;; dorg-ql.el ends here
