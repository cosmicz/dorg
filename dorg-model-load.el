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

(defun dorg-model-load-headings (dolt-dir root-id ids
                                          &optional as-of headline-only
                                          query-fn)
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
receives one fully formatted SELECT and returns rows as coerced alists.
The postimage transaction proof (tracked-regression) reaches THIS loader only
through the internal transaction read port's owner-model operation, so the
committed-readback model is built by the same construction path as every
ordinary load while keeping the proof caller on a closed typed interface.

Returns a hash table of heading-id → `dorg-heading' structs with items
populated as EIEIO objects (nil when HEADLINE-ONLY)."
  (let* ((dorg-model-load--query
          (or query-fn (lambda (sql) (dorg-sql dolt-dir sql))))
         (sfx (dorg-sql-gen--as-of-suffix as-of))
         ;; Optional id restriction for partial loads.  `idf' targets the
         ;; bare headings query, `jidf' the joined queries (alias h).
         (idf (if ids
                  (format " AND id IN (%s)"
                          (mapconcat #'number-to-string ids ","))
                ""))
         (jidf (if ids
                   (format " AND h.id IN (%s)"
                           (mapconcat #'number-to-string ids ","))
                 ""))
         ;; 1. Headings
         (h-rows (funcall dorg-model-load--query
                   (format "SELECT id, parent_id, HEX(sort_key) AS sort_key, level, todo_keyword, priority, commented, title, post_blank, items_hash FROM headings%s WHERE root_id = %d%s"
                           sfx root-id idf)))
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
         ;; 4. Drawer lines (multi-row per drawer item)
         (dl-rows (unless headline-only
                    (funcall dorg-model-load--query
                    (format "SELECT dl.drawer_item_id, dl.line_raw FROM drawer_lines%s dl JOIN heading_items%s hi ON dl.drawer_item_id = hi.id JOIN headings%s h ON hi.heading_id = h.id WHERE h.root_id = %d%s ORDER BY dl.drawer_item_id, dl.ordinal"
                            sfx sfx sfx root-id jidf))))
         ;; 5. Property entries (multi-row per drawer item)
         (pe-rows (unless headline-only
                    (funcall dorg-model-load--query
                    (format "SELECT pe.drawer_item_id, pe.prop_key, pe.prop_value FROM property_entries%s pe JOIN heading_items%s hi ON pe.drawer_item_id = hi.id JOIN headings%s h ON hi.heading_id = h.id WHERE h.root_id = %d%s ORDER BY pe.drawer_item_id, pe.ordinal"
                            sfx sfx sfx root-id jidf)))))
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
      result)))

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
