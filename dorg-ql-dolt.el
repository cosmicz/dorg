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

(defun dorg-ql--dolt-properties-batch (dolt-dir heading-ids)
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
      (let ((rows (dorg-sql
                   dolt-dir
                   (format
                    (concat "SELECT heading_id, prop_key, prop_value"
                            " FROM heading_properties"
                            " WHERE heading_id IN (%s)"
                            " ORDER BY heading_id, ordinal")
                    (mapconcat #'number-to-string heading-ids ", ")))))
        (dolist (row rows)
          (let ((hid (alist-get 'heading_id row)))
            (puthash hid
                     (append (gethash hid table)
                             (list (cons (alist-get 'prop_key row)
                                         (alist-get 'prop_value row))))
                     table)))))
    table))

(defun dorg-ql--dolt-keywords-batch (dolt-dir heading-ids)
  "Return a hash mapping each id in HEADING-IDS to keyword item alists.
Loads keyword rows for the supplied non-root heading ids from DOLT-DIR in
one set-based query.  Root-owned file-level keywords never enter keyword
predicate matching because callers pass only heading candidate ids."
  (let ((table (make-hash-table :test 'eql)))
    (when heading-ids
      (let ((rows (dorg-sql
                   dolt-dir
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

(defun dorg-ql--dolt-planning-rows (dolt-dir root-id field from to)
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
         (rows (dorg-sql
                dolt-dir
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

(defun dorg-ql--dolt-property-ts-rows (dolt-dir root-id from to name)
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
         (rows (dorg-sql
                dolt-dir
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

(defun dorg-ql--dolt-logbook-rows (dolt-dir root-id from to)
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
  (let* ((rows (dorg-sql
                dolt-dir
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

(defun dorg-ql--dolt-active-ts-rows (dolt-dir root-id from to)
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
  (let* ((rows (dorg-sql
                dolt-dir
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

(defun dorg-ql--dolt-rows (query reg)
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
       (dorg-sql
        dolt-dir
        "SELECT id, title FROM headings WHERE root_id = %d AND id = %d"
        root-id id))
      (`(property ,key ,value)
       (dorg-sql
        dolt-dir
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
       (dorg-sql
        dolt-dir
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
       (dorg-sql
        dolt-dir
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
       (dorg-sql
        dolt-dir
        "SELECT id, title FROM headings WHERE root_id = %d AND title = %s"
        root-id
        (dorg-sql-quote title)))
      (`(tag ,tag)
       (dorg-sql
        dolt-dir
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

(defun dorg-ql--dolt-select (query reg)
  "Run normalized QUERY against REG's Dolt repo.
Returns markerless result plists with heading id, title, and the
heading's property alist, in `dorg-ql--dolt-rows' order.

Costs two queries regardless of result count: one selector query (which
owns root scoping) plus one batched property load via
`dorg-ql--dolt-properties-batch'.  An empty selector result issues no
property query."
  (let ((rows (dorg-ql--dolt-rows query reg)))
    (when rows
      (let* ((dolt-dir (dorg-backend-dolt-dir (dorg-registration-backend reg)))
             (ids (mapcar (lambda (row) (alist-get 'id row)) rows))
             (props (dorg-ql--dolt-properties-batch dolt-dir ids)))
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
;; single Dolt root.  It loads ONE root-scoped, headline-only adjacency map
;; (`dorg-model-load-headings' with HEADLINE-ONLY: the `headings' table plus
;; tags, no body/item/drawer/planning payload) and reuses the model
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

(defun dorg-ql--dolt-timestamp-id-set (dolt-dir root-id node)
  "Return a hash set (id → t) of headings matching the `:timestamp' NODE.
NODE is `(:timestamp SOURCE :overlaps (START . END))'.  Dispatches to the
conservative scanner for SOURCE; each scans structured items in a
bounded, root-scoped way and returns one row per matching heading."
  (pcase-let ((`(:timestamp ,source ,_rel (,from . ,to)) node))
    (let* ((rows (pcase source
                   ((or 'scheduled 'deadline 'closed)
                    (dorg-ql--dolt-planning-rows dolt-dir root-id source from to))
                   ('active-ts
                    (dorg-ql--dolt-active-ts-rows dolt-dir root-id from to))
                   ('logbook
                    (dorg-ql--dolt-logbook-rows dolt-dir root-id from to))
                   (`(:property ,name)
                    (dorg-ql--dolt-property-ts-rows
                     dolt-dir root-id from to (unless (eq name '*) name)))
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
    (`(:property ,key ,op ,value)
     (dorg-ql--alist-match-p
      key op value (gethash (dorg-heading-id heading) (plist-get ctx :props))))
    (`(:keyword ,key ,op ,value)
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

(defun dorg-ql--dolt-materialize (headings dolt-dir props)
  "Map HEADINGS to markerless result plists in their current order.
PROPS, when non-nil, is a preloaded id → property-alist hash reused for
`:properties'; otherwise the final result ids are batched once through
`dorg-ql--dolt-properties-batch'.  An empty HEADINGS issues no query."
  (when headings
    (let ((props (or props
                     (dorg-ql--dolt-properties-batch
                      dolt-dir (mapcar #'dorg-heading-id headings)))))
      (mapcar (lambda (h)
                (dorg-ql--make-result
                 :heading-id (dorg-heading-id h)
                 :marker nil
                 :title (dorg-heading-title h)
                 :properties (gethash (dorg-heading-id h) props)))
              headings))))

(defun dorg-ql--dolt-ast->point-form (ast)
  "Map AST to its atomic point-lookup form, or nil when it is not one.
Recognizes ONLY the exact shape `(:pipe (:source :all) (:filter LEAF))' —
a single filter step, no traverse/unique/one — where LEAF is one of the six
equality/presence point leaves, and returns the atomic form the retained
bounded-SQL selector (`dorg-ql--dolt-select' / `dorg-ql--dolt-rows')
answers:

  (:id = N)              -> (id N)
  (:property K = V)      -> (property K V)
  (:cmp title = T)       -> (title T)
  (:tag = TAG)           -> (tag TAG)
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
       (`(:property ,(and k (pred stringp)) = ,(and v (pred stringp)))
        (list 'property k v))
       (`(:cmp title = ,(and tt (pred stringp))) (list 'title tt))
       (`(:tag = ,(and tag (pred stringp))) (list 'tag tag))
       (`(:keyword ,(and k (pred stringp)) exists nil) (list 'keyword k))
       (`(:keyword ,(and k (pred stringp)) = ,(and v (pred stringp)))
        (list 'keyword k v))
       (_ nil)))
    (_ nil)))

(defun dorg-ql--dolt-eval (ast reg)
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
        (dorg-ql--dolt-select point-form reg)
      (dorg-ql--dolt-eval-general ast reg))))

(defun dorg-ql--dolt-eval-general (ast reg)
  "Execute a non-point-form v2 AST against REG's Dolt root via the planner.
The general path for `dorg-ql--dolt-eval': loads a root-scoped
headline-only adjacency map plus on-demand local properties/keywords and
per-timestamp-node scanner sets, then runs the pipeline over the shared
evaluator.  Reached for every AST the point-lookup fast path does not
recognize (compound filters, traversals, timestamp aliases)."
  (let* ((backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         ;; One root-scoped, headline-only adjacency map (headings + tags).
         (by-id (dorg-model-load-headings dolt-dir root-id nil nil t))
         (ordered (dorg-ql--dolt-ordered-headings by-id root-id))
         (idx (dorg-ql--build-tree-index ordered))
         ;; Local properties: loaded once over the candidate set, but only
         ;; when a `property' predicate needs them; reused to materialize.
         (props (when (dorg-ql--any-node-p ast :property)
                  (dorg-ql--dolt-properties-batch
                   dolt-dir (mapcar #'dorg-heading-id ordered))))
         (keywords (when (dorg-ql--any-node-p ast :keyword)
                     (dorg-ql--dolt-keywords-batch
                      dolt-dir (mapcar #'dorg-heading-id ordered))))
         ;; One bounded scanner pass per distinct timestamp node.
         (ts-cache (mapcar (lambda (node)
                             (cons node (dorg-ql--dolt-timestamp-id-set
                                         dolt-dir root-id node)))
                           (dorg-ql--collect-nodes ast :timestamp)))
         (ctx (list :props props :keywords keywords :ts-cache ts-cache))
         (working (dorg-ql--eval-pipeline
                   (cddr ast) ordered idx
                   (lambda (expr h) (dorg-ql--dolt-expr-match-p expr h ctx)))))
    (dorg-ql--dolt-materialize working dolt-dir props)))

(provide 'dorg-ql-dolt)
;;; dorg-ql-dolt.el ends here
