;;; dorg-model-load.el --- DB → model hydration bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The DB → model hydration layer: reads Dolt projection tables and
;; reconstructs `dorg-heading' / `dorg-heading-item' domain objects.  This
;; is the honest home for `dorg-model-load-tree' / `dorg-model-load-headings'
;; — they wear the MODEL namespace but are a DB↔model BRIDGE (they issue
;; SQL), so they belong in a dedicated hydration module that requires both
;; the transport (`dorg-dolt') and the model (`dorg-model'), not inside the
;; low-level transport module where render/sync/mutate/ql appeared to "call
;; the pure model" while actually issuing SQL.
;;
;; The public names are unchanged from when they lived in `dorg-dolt.el';
;; only the home file moved.  `dorg-dolt' keeps back-compat autoloads so
;; `(require \\='dorg-dolt)' + call still resolves them.
;;
;; Composition: loading this module self-registers the `:dolt-dir' loader on
;; the `dorg-keywords' seam (`dorg-root--dolt-loader') so a keyword read from
;; Dolt works once the provider is present — without `dorg-keywords' itself
;; having to know about the transport.

;;; Code:

(require 'dorg-dolt)                     ; `dorg-sql'
(require 'dorg-model)                    ; structs, `dorg-model-from-db-row', `dorg-model--json-nil'
(require 'dorg-sortkey)                  ; `dorg-sortkey-from-hex'
(require 'dorg-sql-gen)                  ; `dorg-sql-gen--as-of-suffix'

;; Seam owned by `dorg-keywords' (loaded before this registrar runs via the
;; `with-eval-after-load' below).  Forward-declared so a compiled load of this
;; bridge without `dorg-keywords' on the path is warning-free.
(defvar dorg-root--dolt-loader)

;;; DB → Model Loader

(defun dorg-model-load--heading-ids-by-property
    (dolt-dir root-id key value &optional as-of)
  "Return heading ids matching property KEY=VALUE under ROOT-ID at AS-OF.
The lookup is exact-snapshot and root-scoped.  It deliberately returns every
distinct match, in id order, so higher layers can enforce their own
missing-versus-ambiguous identity contracts without duplicating SQL."
  (let ((rows
          (dorg-sql
           dolt-dir
           (dorg-sql-gen--heading-ids-by-property
            root-id key value as-of))))
    (mapcar (lambda (row) (alist-get 'id row)) rows)))

(defun dorg-model-load--drawer-item-ids (item-rows)
  "Return drawer item ids from already root/slice-scoped ITEM-ROWS.
The item query left-joins `drawer_items', whose `name' is NOT NULL in schema
v6.  A non-nil `drawer_name' therefore proves the `drawer_items' parent that
`drawer_lines' and `property_entries' foreign-key to."
  (let (ids)
    (dolist (row item-rows (nreverse ids))
      (when (alist-get 'drawer_name row)
        (push (alist-get 'item_id row) ids)))))

(defconst dorg-model-load--max-depth 512
  "Recursion bound for the ancestor walk; a deeper chain is treated as broken.")

(defconst dorg-model-load--heading-columns
  "h.id, h.parent_id, HEX(h.sort_key) AS sort_key, %s, h.todo_keyword, h.priority, h.commented, h.title, h.post_blank, h.items_hash"
  "Headings columns for a load; %s is the level column expression.")

(defun dorg-model-load--headings-sql (root-id ids sfx mode schema)
  "Return the one headings SELECT for ROOT-ID, IDS and AS-OF suffix SFX.
MODE is `effective' or `offsets'; SCHEMA is `v8' or `v7'.  A v8 id slice in
`effective' mode returns each row's effective LEVEL (the sum of offsets on
its path) and REACHED (1 exactly when that path ends at the root row ROOT-ID
in root shape, no parent and offset 0, and every other row on it has a
positive offset) from ONE uncorrelated recursive walk;
it replaces the plain SELECT.  A v8 load of any other shape is a plain SELECT
of the stored offset.  A v7 load joins each row to its parent at the same
token and decodes the offset from the two absolute levels, so a slice whose
parent lies outside IDS still carries its offset."
  (let ((id-list (and ids (mapconcat #'number-to-string ids ","))))
    (if (and ids (eq mode 'effective) (eq schema 'v8))
        (format
         (concat
          "WITH RECURSIVE walk (seed, id, parent_id, level_offset, depth) AS ("
          " SELECT id, id, parent_id, level_offset, 0 FROM headings%s"
          " WHERE root_id = %d AND id IN (%s)"
          " UNION ALL"
          " SELECT w.seed, p.id, p.parent_id, p.level_offset, w.depth + 1"
          " FROM walk w JOIN headings%s p ON p.id = w.parent_id"
          " WHERE p.root_id = %d AND w.depth < %d)"
          " SELECT " dorg-model-load--heading-columns ","
          " lv.reached AS reached"
          " FROM headings%s h JOIN (SELECT seed,"
          " CAST(SUM(level_offset) AS SIGNED) AS level,"
          ;; REACHED is 1 only for a path ending at the named root in root
          ;; shape with every non-root offset positive: the SQL image of
          ;; `dorg-model-offset-level', so a slice never sums through a
          ;; zero or negative offset.
          " CAST(CASE WHEN SUM(CASE WHEN parent_id IS NOT NULL AND level_offset <= 0"
          " THEN 1 ELSE 0 END) = 0"
          " THEN SUM(CASE WHEN id = %d AND parent_id IS NULL AND level_offset = 0"
          " THEN 1 ELSE 0 END) ELSE 0 END AS SIGNED) AS reached"
          " FROM walk GROUP BY seed) lv ON lv.seed = h.id"
          " WHERE h.root_id = %d AND h.id IN (%s)")
         sfx root-id id-list sfx root-id dorg-model-load--max-depth
         "h.level_offset, lv.level AS level" sfx root-id root-id id-list)
      (let ((where (format " WHERE h.root_id = %d%s" root-id
                           (if ids (format " AND h.id IN (%s)" id-list) ""))))
        (if (eq schema 'v8)
            (format (concat "SELECT " dorg-model-load--heading-columns
                            " FROM headings%s h%s")
                    "h.level_offset" sfx where)
          ;; A parentless row keeps its absolute level as its offset, so
          ;; only a level-0 root decodes to the root offset 0.
          (format (concat "SELECT " dorg-model-load--heading-columns
                          " FROM headings%s h LEFT JOIN headings%s p"
                          " ON p.id = h.parent_id AND p.root_id = h.root_id%s")
                  (concat "h.level, CASE WHEN h.parent_id IS NULL THEN h.level"
                          " ELSE h.level - p.level END AS level_offset")
                  sfx sfx where))))))

(defun dorg-model-load--missing-offset-column-p (err)
  "Return non-nil when ERR reports that headings has no `level_offset' column.
That is the signature of a pre-v8 AS-OF token, and nothing else."
  (let ((msg (error-message-string err)))
    (and (string-match-p "level_offset" msg)
         (string-match-p "does not have column\\|could not be found\\|[Uu]nknown column" msg))))

(defun dorg-model-load--heading-rows (query root-id ids sfx mode)
  "Read ROOT-ID's heading rows with QUERY; return (SCHEMA . ROWS).
IDS, SFX and MODE are as for `dorg-model-load--headings-sql'.
Issues the v8 statement.  Only when the engine reports that the token has no
`level_offset' column does it issue the v7 statement instead, so a current
store never pays an extra statement.  In a v8 effective slice a row whose
path does not reach a root has its derived level cleared."
  (condition-case err
      (let ((rows (funcall query (dorg-model-load--headings-sql
                                  root-id ids sfx mode 'v8))))
        (dolist (row rows)
          (when (and (assq 'reached row) (not (eql 1 (alist-get 'reached row))))
            (setf (alist-get 'level row) nil)))
        (cons 'v8 rows))
    (error
     (if (dorg-model-load--missing-offset-column-p err)
         (cons 'v7 (funcall query (dorg-model-load--headings-sql
                                   root-id ids sfx mode 'v7)))
       (signal (car err) (cdr err))))))

(defun dorg-model-load--finish-levels (result root-id schema mode ids)
  "Complete RESULT's level slots under ROOT-ID for SCHEMA, MODE and IDS.
IDS is the id restriction, nil for a whole-root load.
v8: a whole-root effective load derives levels from the offsets; an
effective slice already carries them; an offsets load leaves them nil.  v7:
the stored absolute level is the effective level and the statement already
decoded each offset against the parent row at the same token (a dangling
parent leaves it nil); an offsets load clears the level."
  (pcase schema
    ('v8
     (when (and (eq mode 'effective) (null ids))
       (dorg-model-derive-levels result root-id)))
    ('v7
     (when (eq mode 'offsets)
       (maphash (lambda (_id h) (setf (dorg-heading-level h) nil)) result))))
  result)

(defun dorg-model-load-headings (dolt-dir root-id ids
                                          &optional as-of headline-only
                                          query-fn levels)
  "Load headings for ROOT-ID from DOLT-DIR as hash id → `dorg-heading'.
IDS, when non-nil, restricts the load to that list of heading ids — a
partial slice (e.g. an island subtree or ancestor spine).  When IDS is
nil the whole ROOT-ID tree is loaded.  When AS-OF is non-nil (a commit
hash), queries historical state.

When HEADLINE-ONLY is non-nil, only headline data is fetched (the
`headings' table plus tags); the item/body/drawer/planning payload
queries are skipped entirely and every heading's items are left nil.
This is the cheap load used for breadcrumb ancestors, which are context
rather than content.

QUERY-FN, when non-nil, replaces the implicit `dorg-sql' read path: it
receives one fully formatted SELECT and returns typed row alists.
The postimage transaction proof (tracked-regression) reaches THIS loader only
through the internal transaction read port's owner-model operation, so the
committed-readback model is built by the same construction path as every
ordinary load while keeping the proof caller on a closed typed interface.

LEVELS chooses what each heading's level slots carry:
- nil or `effective' (the default): the effective absolute level.  A
  whole-root load derives it in Elisp from the offsets; an id slice gets it
  from ONE uncorrelated recursive walk to the root that replaces the plain
  headings SELECT, so the statement count is unchanged.  A heading whose
  chain does not reach a root gets level nil, never a fabricated 0.
- `offsets': the persisted level offset only; the effective level is nil.
Any other value signals `wrong-type-argument'.  A token whose schema still
has the absolute `level' column (a pre-v8 AS-OF revision) is decoded from
that column, never by reading an absolute level as an offset.

Returns a hash table of heading-id → `dorg-heading' structs with items
populated as EIEIO objects (nil when HEADLINE-ONLY)."
  (unless (memq levels '(nil effective offsets))
    (signal 'wrong-type-argument (list '(member effective offsets) levels)))
  (let* ((dorg-model-load--query
          (or query-fn (lambda (sql) (dorg-sql dolt-dir sql))))
         (sfx (dorg-sql-gen--as-of-suffix as-of))
         ;; Optional id restriction for the joined queries (alias h); the
         ;; headings statement builds its own.
         (jidf (if ids
                   (format " AND h.id IN (%s)"
                           (mapconcat #'number-to-string ids ","))
                 ""))
         ;; 1. Headings (v8 offsets; a pre-v8 token decodes its absolute level)
         (mode (or levels 'effective))
         (h-read (dorg-model-load--heading-rows
                  dorg-model-load--query root-id ids sfx mode))
         (h-rows (cdr h-read))
         ;; 2. Tags
         (tag-rows (funcall dorg-model-load--query
                     (format "SELECT ht.heading_id, ht.tag FROM heading_tags%s ht JOIN headings%s h ON ht.heading_id = h.id WHERE h.root_id = %d%s ORDER BY ht.heading_id, ht.ordinal"
                             sfx sfx root-id jidf)))
         ;; 3. Items with detail joins (one row per item).  Skipped for a
         ;; headline-only load — ancestor breadcrumbs need no body payload.
         (item-rows (unless headline-only
                      (funcall dorg-model-load--query
                       (format (concat
                                "SELECT hi.id AS item_id, hi.heading_id, hi.kind, hi.post_blank,"
                                " HEX(hi.sort_key) AS item_sort_key,"
                                " ti.text_raw,"
                                " ki.keyword, ki.value_raw,"
                                " pi.scheduled_raw, pi.deadline_raw, pi.closed_raw,"
                                " di.name AS drawer_name"
                                " FROM heading_items%s hi"
                                " JOIN headings%s h ON hi.heading_id = h.id"
                                " LEFT JOIN text_items%s ti ON ti.item_id = hi.id"
                                " LEFT JOIN keyword_items%s ki ON ki.item_id = hi.id"
                                " LEFT JOIN planning_items%s pi ON pi.item_id = hi.id"
                                " LEFT JOIN drawer_items%s di ON di.item_id = hi.id"
                                " WHERE h.root_id = %d%s"
                                " ORDER BY hi.heading_id, hi.sort_key")
                               sfx sfx sfx sfx sfx sfx root-id jidf))))
         (drawer-item-ids (and ids
                               (not headline-only)
                               (dorg-model-load--drawer-item-ids item-rows)))
         (drawer-item-id-list (when drawer-item-ids
                                (mapconcat #'number-to-string
                                           drawer-item-ids ",")))
         ;; 4. Drawer lines (multi-row per drawer item)
         (dl-rows (unless headline-only
                    (cond
                     ((null ids)
                      (funcall dorg-model-load--query
                       (format "SELECT dl.drawer_item_id, dl.line_raw FROM drawer_lines%s dl JOIN heading_items%s hi ON dl.drawer_item_id = hi.id JOIN headings%s h ON hi.heading_id = h.id WHERE h.root_id = %d%s ORDER BY dl.drawer_item_id, dl.ordinal"
                               sfx sfx sfx root-id jidf)))
                     (drawer-item-ids
                      (funcall dorg-model-load--query
                       (format "SELECT dl.drawer_item_id, dl.line_raw FROM drawer_lines%s dl WHERE dl.drawer_item_id IN (%s) ORDER BY dl.drawer_item_id, dl.ordinal"
                               sfx drawer-item-id-list))))))
         ;; 5. Property entries (multi-row per drawer item)
         (pe-rows (unless headline-only
                    (cond
                     ((null ids)
                      (funcall dorg-model-load--query
                       (format "SELECT pe.drawer_item_id, pe.prop_key, pe.prop_value FROM property_entries%s pe JOIN heading_items%s hi ON pe.drawer_item_id = hi.id JOIN headings%s h ON hi.heading_id = h.id WHERE h.root_id = %d%s ORDER BY pe.drawer_item_id, pe.ordinal"
                               sfx sfx sfx root-id jidf)))
                     (drawer-item-ids
                      (funcall dorg-model-load--query
                       (format "SELECT pe.drawer_item_id, pe.prop_key, pe.prop_value FROM property_entries%s pe WHERE pe.drawer_item_id IN (%s) ORDER BY pe.drawer_item_id, pe.ordinal"
                               sfx drawer-item-id-list)))))))
    ;; Build lookup tables
    (let ((tags-ht (make-hash-table :test 'eql))
          (dl-ht (make-hash-table :test 'eql))
          (pe-ht (make-hash-table :test 'eql))
          (items-by-hid (make-hash-table :test 'eql))
          (result (make-hash-table :test 'eql)))
      ;; Index tags by heading_id
      (dolist (tr tag-rows)
        (let ((hid (alist-get 'heading_id tr))
              (tag (alist-get 'tag tr)))
          (puthash hid (append (gethash hid tags-ht) (list tag)) tags-ht)))
      ;; Index drawer lines by drawer_item_id
      (dolist (r dl-rows)
        (let ((did (alist-get 'drawer_item_id r)))
          (puthash did (append (gethash did dl-ht)
                               (list (alist-get 'line_raw r)))
                  dl-ht)))
      ;; Index property entries by drawer_item_id
      (dolist (r pe-rows)
        (let ((did (alist-get 'drawer_item_id r)))
          (puthash did (append (gethash did pe-ht)
                               (list (cons (alist-get 'prop_key r)
                                           (alist-get 'prop_value r))))
                  pe-ht)))
      ;; Convert item rows to EIEIO objects, grouped by heading_id
      (dolist (ir item-rows)
        (let* ((item-id (alist-get 'item_id ir))
               (hid (alist-get 'heading_id ir))
               (kind (alist-get 'kind ir))
               (pb (or (alist-get 'post_blank ir) 0))
               ;; DB item identity for item-level diffing.
               (sk (when-let ((hex (alist-get 'item_sort_key ir)))
                     (dorg-sortkey-from-hex hex)))
               (obj (pcase kind
                      ("text"
                       (when-let ((text (alist-get 'text_raw ir)))
                         (dorg-text-item :post-blank pb :id item-id :sort-key sk
                                         :text text)))
                      ("keyword"
                       (when-let ((kw (alist-get 'keyword ir)))
                         (dorg-keyword-item :post-blank pb :id item-id :sort-key sk
                                            :keyword kw
                                            :value (or (alist-get 'value_raw ir) ""))))
                      ("planning"
                       (dorg-planning-item
                        :post-blank pb :id item-id :sort-key sk
                        :scheduled (dorg-model--json-nil (alist-get 'scheduled_raw ir))
                        :deadline (dorg-model--json-nil (alist-get 'deadline_raw ir))
                        :closed (dorg-model--json-nil (alist-get 'closed_raw ir))))
                      ("drawer"
                       (when-let ((name (alist-get 'drawer_name ir)))
                         ;; Reconstruct the drawer's representation from the
                         ;; detail table that actually holds its rows —
                         ;; property-form drawers store `property_entries',
                         ;; generic (raw-lines) drawers store `drawer_lines'
                         ;; (see `dorg-sql-gen--drawer-detail-inserts').
                         ;; Dispatching on the NAME would be wrong: org parses
                         ;; a body `:PROPERTIES:' block after content as a
                         ;; generic drawer stored as lines, and rebuilding it
                         ;; from the empty property table would discard them.
                         ;; A property-form drawer always writes >=1 property
                         ;; entry, so their presence is the faithful
                         ;; discriminator (an empty drawer, ambiguous but
                         ;; renders identically either way, defaults to lines).
                         (if (gethash item-id pe-ht)
                             (dorg-drawer-item :post-blank pb :id item-id :sort-key sk
                                               :name name
                                               :properties (gethash item-id pe-ht))
                           (dorg-drawer-item :post-blank pb :id item-id :sort-key sk
                                             :name name
                                             :lines (gethash item-id dl-ht))))))))
          (when obj
            (puthash hid (append (gethash hid items-by-hid) (list obj))
                    items-by-hid))))
      ;; Build heading structs with items attached
      (dolist (row h-rows)
        (let* ((id (alist-get 'id row))
               (h (dorg-model-from-db-row row (gethash id tags-ht))))
          (setf (dorg-heading-items h) (gethash id items-by-hid))
          (puthash id h result)))
      (dorg-model-load--finish-levels result root-id (car h-read) mode ids))))

(defun dorg-model-load-tree (dolt-dir root-id &optional as-of)
  "Load the whole heading tree for ROOT-ID from DOLT-DIR.
Degenerate case of `dorg-model-load-headings' with no id restriction.
When AS-OF is non-nil (a commit hash), queries historical state.
Returns a hash table of heading-id → `dorg-heading' structs with items
populated as EIEIO objects."
  (dorg-model-load-headings dolt-dir root-id nil as-of))

;;; :dolt-dir keyword-seam registration (composition contract)

(defun dorg-model-load--root-loader (dir root-id)
  "Load ROOT-ID's root heading from Dolt DIR for the `dorg-keywords' seam.
Root-only slice — load just the root heading and its items, never the
whole tree — so config/keyword reads stay bounded.  Returns the root
`dorg-heading' (or nil)."
  (gethash root-id (dorg-model-load-headings dir root-id (list root-id))))

;; Provider self-registration: when `dorg-keywords' is (or becomes) loaded,
;; install the loader on its `:dolt-dir' dispatch seam.  No hard upward
;; `require' — `(require \\='dorg-model-load)' stays a downward-only load.
(with-eval-after-load 'dorg-keywords
  (dorg-root--install-dolt-loader
   'dorg-model-load #'dorg-model-load--root-loader))

(provide 'dorg-model-load)
;;; dorg-model-load.el ends here
