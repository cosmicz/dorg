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
;; B8/D6: the Dolt SQL adapter is loaded ON THE DOLT PATH ONLY — its entry point
;; autoloads `dorg-ql-dolt' (which pulls `dorg-dolt') when a Dolt-backed query
;; first dispatches, so a plain-org/model `(require 'dorg-ql)' stays Dolt-free.
;; `dorg-ql--dolt-eval' is the single Dolt entry point; loading it defines the
;; whole adapter (including the point-lookup `dorg-ql--dolt-select' it reaches
;; internally), so no separate `dorg-ql--dolt-select' autoload is needed.
(autoload 'dorg-ql--dolt-eval "dorg-ql-dolt")

;; The single identity resolver lives in `dorg-mutate' (loaded via `dorg-api' at
;; runtime); declared here to avoid a load-time cycle while the QL `(key V)' seam
;; consults it directly (one identity interpretation, per D4).
(declare-function dorg-mutate--source-identity "dorg-mutate")
(declare-function dorg-mutate--resolve-source "dorg-mutate")
(declare-function dorg-mutate--source-from-registration "dorg-mutate")

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

(defun dorg-ql--buffer-select-v2 (ast buffer)
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
      ((or 'dolt 'plain-org)
       (if (eq type 'dolt)
           (dorg-ql--dolt-eval ast reg)
         (dorg-ql--plain-eval ast reg)))
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
  "Non-nil when query FORM contains a `(key …)' selector anywhere."
  (pcase form
    (`(key . ,_) t)
    ((pred consp) (cl-some #'dorg-ql--query-references-key-p form))
    (_ nil)))

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
(defun dorg-ql-select (query &optional source)
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

Each result is a plist with `:heading-id', `:marker', `:title', and
`:properties'.  Plain-org results carry live markers; model and Dolt
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
  (setq query (dorg-ql--resolve-key-refs query source))
  (let ((ast (dorg-ql-normalize query)))
    (if (and source (consp source))
        (dorg-ql--model-eval ast source)
      (dorg-ql--buffer-select-v2 ast source))))

(provide 'dorg-ql)
;;; dorg-ql.el ends here
