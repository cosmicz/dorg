;;; dorg-sql-gen.el --- SQL generation from dorg domain model -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Converts `dorg-heading' and `dorg-heading-item' domain objects into
;; SQL INSERT/UPDATE/DELETE statements.  Separated from the parser so
;; that SQL generation is a pure function of domain objects, usable by
;; both the parser (for backward-compat `dorg-parse-buffer') and the
;; diff engine.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'dorg-model)
(require 'dorg-sortkey)
;; SQL literal quoting is the dependency-free leaf (`dorg-sql-quote'); this
;; module is pure SQL-text generation and must not drag in the whole
;; `dorg-dolt' transport (and its hard `mysql.el' load) just to quote values.
(require 'dorg-sql-quote)

;;; AS-OF SQL fragment

(defun dorg-sql-gen--as-of-suffix (commit)
  "Return SQL AS OF clause for COMMIT, or empty string.
An `AS OF <hash>' fragment is SQL-text generation (used by the render,
mutate, and hydration SQL-building sites), so its honest home is here, not
the model layer where it formerly wore the `dorg-model--as-of-suffix' name."
  (if commit (format " AS OF %s" (dorg-sql-quote commit)) ""))

(defun dorg-sql-gen--heading-ids-by-property (root-id key value &optional as-of)
  "Return SQL selecting heading ids with property KEY=VALUE under ROOT-ID.
The generated query is exact-snapshot and root-scoped.  It returns every
distinct match in id order so callers can classify missing and ambiguous
identities without owning a second copy of the join."
  (let ((sfx (dorg-sql-gen--as-of-suffix as-of)))
    (format
     (concat
      "SELECT DISTINCT h.id FROM headings%s h"
      " JOIN heading_items%s hi ON hi.heading_id = h.id"
      " JOIN property_entries%s pe ON pe.drawer_item_id = hi.id"
      " WHERE h.root_id = %d"
      " AND pe.prop_key = %s AND pe.prop_value = %s"
      " ORDER BY h.id")
     sfx sfx sfx root-id
     (dorg-sql-quote key)
     (dorg-sql-quote value))))

;;; Sort Key Helper

(defun dorg-sql-gen--sort-key-hex (key)
  "Return a SQL hex literal for sort KEY (unibyte string).
Returns e.g. X'01', X'0a80'.

Hard storage guard: signals an error if KEY exceeds
`dorg-sortkey-max-bytes', so an overlong key can never reach SQL
generation and silently truncate or corrupt the VARBINARY(64) column.
This is a backstop: every allocated key is exactly
`dorg-sortkey-boundary-width' bytes.  Keys of exactly
`dorg-sortkey-max-bytes' remain writable for compatibility with
existing rows."
  (when (> (length key) dorg-sortkey-max-bytes)
    (error "dorg-sql-gen: sort key length %d exceeds VARBINARY(%d) limit"
           (length key) dorg-sortkey-max-bytes))
  (dorg-sortkey-to-hex key))

(defun dorg-sql-gen--sort-key-value (key)
  "Return sort KEY as a typed row value, under the same storage guard.
The value lowers to the literal `dorg-sql-gen--sort-key-hex' returns."
  (dorg-sql-gen--sort-key-hex key)
  (cons :hex key))

(defun dorg-sql-gen--integer (value)
  "Return VALUE, an integer column value, or signal `wrong-type-argument'."
  (unless (integerp value)
    (signal 'wrong-type-argument (list 'integerp value)))
  value)

;;; Item SQL Generation

(defun dorg-sql-gen--bind-item-row (item item-id sort-key)
  "Record ITEM-ID and SORT-KEY on ITEM and return ITEM.
SQL lowering is the point where new item rows receive database identity; binding
that identity back onto the candidate model lets transaction postimage checks
compare the complete persisted item-row image."
  (setf (dorg-item-id item) item-id)
  (setf (dorg-item-sort-key item) sort-key)
  item)

(cl-defgeneric dorg-item-to-rows (item item-id heading-id sort-key)
  "Return the typed insert rows for ITEM, in insert order.
ITEM-ID is the snowflake id for the heading_items row, HEADING-ID the parent
heading's id and SORT-KEY the item's unibyte sort key among its siblings.
The heading_items row comes first, then the item's detail rows.")

(defun dorg-sql-gen--item-row (item-id heading-id sort-key kind post-blank)
  "Return the heading_items row for ITEM-ID of KIND under HEADING-ID.
SORT-KEY is the unibyte sort key and POST-BLANK the trailing blank count."
  (dorg-sql-row-make "heading_items" '(id heading_id sort_key kind post_blank)
                     (list (dorg-sql-gen--integer item-id)
                           (dorg-sql-gen--integer heading-id)
                           (dorg-sql-gen--sort-key-value sort-key)
                           kind
                           (dorg-sql-gen--integer post-blank))))

(cl-defmethod dorg-item-to-rows ((item dorg-text-item) item-id heading-id sort-key)
  (list
   (dorg-sql-gen--item-row item-id heading-id sort-key "text"
                           (dorg-item-post-blank item))
   (dorg-sql-row-make "text_items" '(item_id text_raw)
                      (list item-id (dorg-text-item-text item)))))

(cl-defmethod dorg-item-to-rows ((item dorg-keyword-item) item-id heading-id sort-key)
  (list
   (dorg-sql-gen--item-row item-id heading-id sort-key "keyword"
                           (dorg-item-post-blank item))
   (dorg-sql-row-make "keyword_items" '(item_id keyword value_raw)
                      (list item-id
                            (dorg-keyword-item-keyword item)
                            (dorg-keyword-item-value item)))))

(cl-defmethod dorg-item-to-rows ((item dorg-planning-item) item-id heading-id sort-key)
  (list
   (dorg-sql-gen--item-row item-id heading-id sort-key "planning"
                           (dorg-item-post-blank item))
   (dorg-sql-row-make "planning_items"
                      '(item_id scheduled_raw deadline_raw closed_raw)
                      (list item-id
                            (dorg-planning-item-scheduled item)
                            (dorg-planning-item-deadline item)
                            (dorg-planning-item-closed item)))))

(defun dorg-sql-gen--drawer-detail-rows (item-id item)
  "Return the typed rows of drawer ITEM's child rows under heading_items ITEM-ID.
A PROPERTIES drawer (ITEM has properties) yields `property_entries' rows;
any other drawer yields `drawer_lines' rows."
  (let ((ord 0)
        (rows nil))
    (if (dorg-drawer-item-properties item)
        (dolist (kv (dorg-drawer-item-properties item))
          (cl-incf ord)
          (let ((key (car kv))
                (value (cdr kv)))
            (push (dorg-sql-row-make
                   "property_entries"
                   '(drawer_item_id ordinal prop_key prop_value raw_line)
                   (list item-id ord key value (format ":%s: %s" key value)))
                  rows)))
      (dolist (line (dorg-drawer-item-lines item))
        (cl-incf ord)
        (push (dorg-sql-row-make "drawer_lines" '(drawer_item_id ordinal line_raw)
                                 (list item-id ord line))
              rows)))
    (nreverse rows)))

(defun dorg-sql-gen--drawer-detail-inserts (item-id item)
  "Return INSERTs for drawer ITEM's child rows under heading_items ITEM-ID.
A PROPERTIES drawer (ITEM has properties) emits `property_entries' rows;
any other drawer emits `drawer_lines' rows.  Shared by full-item INSERT
and item-level UPDATE so both forms stay consistent."
  (mapcar #'dorg-sql-row-sql (dorg-sql-gen--drawer-detail-rows item-id item)))

(cl-defmethod dorg-item-to-rows ((item dorg-drawer-item) item-id heading-id sort-key)
  (nconc
   (list
    (dorg-sql-gen--item-row item-id heading-id sort-key "drawer"
                            (dorg-item-post-blank item))
    (dorg-sql-row-make "drawer_items" '(item_id name)
                       (list item-id (dorg-drawer-item-name item))))
   (dorg-sql-gen--drawer-detail-rows item-id item)))

(cl-defgeneric dorg-item-to-sql (item item-id heading-id sort-n)
  "Return a list of SQL INSERT strings for ITEM.
ITEM-ID is the snowflake id for the heading_items row.
HEADING-ID is the parent heading's id.
SORT-N is the item's sort key among its siblings.
Each string is one row of `dorg-item-to-rows'.")

(cl-defmethod dorg-item-to-sql (item item-id heading-id sort-n)
  (mapcar #'dorg-sql-row-sql (dorg-item-to-rows item item-id heading-id sort-n)))

;;; Item UPDATE SQL Generation
;;
;; Same-kind detail-table updates that reuse an existing `heading_items.id'
;; instead of delete+reinsert.  Used by item-level lowering when an old DB
;; item is paired with an edited new item of compatible kind.

(defun dorg-sql-gen-item-update (old-item new-item)
  "Return SQL updating reused OLD-ITEM's detail rows to NEW-ITEM's content.
OLD-ITEM supplies the persisted `heading_items.id'; NEW-ITEM supplies the
edited content.  Both are the same compatible kind (see
`dorg-model--items-compatible-p'), so the `heading_items' row id is reused
and only its detail table(s) change.  For drawers, only that drawer's own
child rows are rewritten — churn never escalates to the whole heading."
  (let ((id (dorg-item-id old-item)))
    (cl-etypecase new-item
      (dorg-text-item
       (list (format "UPDATE text_items SET text_raw = %s WHERE item_id = %d"
                     (dorg-sql-quote (dorg-text-item-text new-item)) id)))
      (dorg-keyword-item
       (list (format "UPDATE keyword_items SET value_raw = %s WHERE item_id = %d"
                     (dorg-sql-quote (dorg-keyword-item-value new-item)) id)))
      (dorg-planning-item
       (list (format "UPDATE planning_items SET scheduled_raw = %s, deadline_raw = %s, closed_raw = %s WHERE item_id = %d"
                     (dorg-sql-quote (dorg-planning-item-scheduled new-item))
                     (dorg-sql-quote (dorg-planning-item-deadline new-item))
                     (dorg-sql-quote (dorg-planning-item-closed new-item))
                     id)))
      (dorg-drawer-item
       ;; Clear both child tables so a form change leaves no orphan rows,
       ;; then re-insert NEW-ITEM's current form.
       (nconc
        (list (format "DELETE FROM drawer_lines WHERE drawer_item_id = %d" id)
              (format "DELETE FROM property_entries WHERE drawer_item_id = %d" id))
        (dorg-sql-gen--drawer-detail-inserts id new-item))))))

(defun dorg-sql-gen-item-row-update (item-id sort-key post-blank)
  "Return SQL updating reused `heading_items' row ITEM-ID.
Sets SORT-KEY (unibyte string) and POST-BLANK.  Used by item-level
lowering for a reused item whose position or trailing blank count changed;
the row id and kind are preserved."
  (format "UPDATE heading_items SET sort_key = %s, post_blank = %d WHERE id = %d"
          (dorg-sql-gen--sort-key-hex sort-key) post-blank item-id))

;;; Heading SQL Generation

(define-error 'dorg-sql-gen-missing-level-offset
  "dorg: heading has no persisted level offset")

(defun dorg-sql-gen--level-offset (heading)
  "Return HEADING's persisted level offset, or signal when it is missing.
Only the offset is ever written; an absolute level is never inferred from the
effective LEVEL slot, so a constructed heading cannot smuggle one into storage."
  (let ((offset (dorg-heading-level-offset heading)))
    (unless (and (integerp offset) (>= offset 0))
      (signal 'dorg-sql-gen-missing-level-offset
              (list (dorg-heading-id heading) offset)))
    offset))

(defun dorg-sql-gen-heading-row (heading root-id)
  "Return the typed headings row for HEADING under ROOT-ID.
Includes the items_hash column."
  (dorg-sql-row-make
   "headings"
   '(id root_id parent_id sort_key level_offset todo_keyword priority commented
        title post_blank items_hash)
   (list (dorg-sql-gen--integer (dorg-heading-id heading))
         (dorg-sql-gen--integer root-id)
         (and (dorg-heading-parent-id heading)
              (dorg-sql-gen--integer (dorg-heading-parent-id heading)))
         (dorg-sql-gen--sort-key-value (dorg-heading-sort-key heading))
         (dorg-sql-gen--level-offset heading)
         (dorg-heading-todo heading)
         (dorg-heading-priority heading)
         (if (dorg-heading-commented heading) :true :false)
         (dorg-heading-title heading)
         (dorg-sql-gen--integer (or (dorg-heading-post-blank heading) 0))
         (dorg-heading-items-hash heading))))

(defun dorg-sql-gen-heading-insert (heading root-id)
  "Return SQL INSERT string for HEADING under ROOT-ID.
Includes items_hash column."
  (dorg-sql-row-sql (dorg-sql-gen-heading-row heading root-id)))

(defun dorg-sql-gen-heading-update (heading)
  "Return SQL UPDATE string for HEADING.
Updates all mutable attributes including items_hash."
  (format "UPDATE headings SET parent_id = %s, sort_key = %s, level_offset = %d, todo_keyword = %s, priority = %s, commented = %s, title = %s, post_blank = %d, items_hash = %s WHERE id = %d"
          (if (dorg-heading-parent-id heading)
              (number-to-string (dorg-heading-parent-id heading))
            "NULL")
          (dorg-sql-gen--sort-key-hex (dorg-heading-sort-key heading))
          (dorg-sql-gen--level-offset heading)
          (dorg-sql-quote (dorg-heading-todo heading))
          (dorg-sql-quote (dorg-heading-priority heading))
          (if (dorg-heading-commented heading) "TRUE" "FALSE")
          (dorg-sql-quote (dorg-heading-title heading))
          (or (dorg-heading-post-blank heading) 0)
          (dorg-sql-quote (dorg-heading-items-hash heading))
          (dorg-heading-id heading)))

(defun dorg-sql-gen-tag-rows (heading-id tags)
  "Return the typed heading_tags rows for TAGS on HEADING-ID, in tag order."
  (let ((ord 0))
    (mapcar (lambda (tag)
              (cl-incf ord)
              (dorg-sql-row-make "heading_tags" '(heading_id ordinal tag)
                                 (list (dorg-sql-gen--integer heading-id) ord tag)))
            tags)))

(defun dorg-sql-gen-tag-inserts (heading-id tags)
  "Return list of SQL INSERT strings for TAGS on HEADING-ID."
  (mapcar #'dorg-sql-row-sql (dorg-sql-gen-tag-rows heading-id tags)))

(defun dorg-sql-gen-heading-items-rows (heading id-generator)
  "Return the typed insert rows for all items in HEADING, in item order.
ID-GENERATOR is a function returning the next snowflake BIGINT id; it is
called once per inserted item, in item order, and each item is bound to its
new id and sort key.  Skips `dorg-keyword-item' instances whose keyword
starts with \"DORG_\" -- these are buffer-local metadata, not DB content."
  (let* ((items (cl-remove-if #'dorg-item--db-local-p
                              (dorg-heading-items heading)))
         (ops (dorg-model-plan-items nil items))
         (rows nil))
    (dolist (op ops)
      (pcase op
        (`(:insert ,item ,sort-key)
         (let ((item-id (funcall id-generator)))
           (dorg-sql-gen--bind-item-row item item-id sort-key)
           (setq rows (nconc rows (dorg-item-to-rows item item-id
                                                     (dorg-heading-id heading)
                                                     sort-key)))))))
    rows))

(defun dorg-sql-gen-heading-items-sql (heading id-generator)
  "Return flat list of SQL INSERTs for all items in HEADING.
ID-GENERATOR is a function returning the next snowflake BIGINT id.
Skips `dorg-keyword-item' instances whose keyword starts with
\"DORG_\" — these are buffer-local metadata, not DB content."
  (mapcar #'dorg-sql-row-sql (dorg-sql-gen-heading-items-rows heading id-generator)))

;;; Full Heading SQL (for parser backward compat)

(defun dorg-sql-gen-heading-full-rows (heading root-id id-generator)
  "Return every typed insert row for HEADING: heading, tags, then items.
ID-GENERATOR is a function returning snowflake ids for items."
  (append (list (dorg-sql-gen-heading-row heading root-id))
          (dorg-sql-gen-tag-rows (dorg-heading-id heading) (dorg-heading-tags heading))
          (dorg-sql-gen-heading-items-rows heading id-generator)))

(defun dorg-sql-gen-heading-full-sql (heading root-id id-generator)
  "Return all SQL statements for HEADING: heading + tags + items.
ID-GENERATOR is a function returning snowflake ids for items."
  (mapcar #'dorg-sql-row-sql
          (dorg-sql-gen-heading-full-rows heading root-id id-generator)))

;;; Core Table Order
;;
;; A whole-document import inserts many rows into each core table.  Grouping
;; them by table gives one adjacent run per table, which the lowering merges
;; into a few multi-row INSERTs.  The order below follows DOrg's own foreign
;; keys (`dorg-schema-ddl'): every non-self key points from a later table to
;; an earlier one.  The two headings self-references (parent_id and root_id)
;; hold because the stable partition keeps headings in document pre-order,
;; so a parent row always precedes its children.

(defconst dorg-sql-gen-core-table-order
  '("headings" "heading_tags" "heading_items" "text_items" "keyword_items"
    "planning_items" "drawer_items" "property_entries" "drawer_lines")
  "Core row tables in foreign-key order: parents before children.")

(defun dorg-sql-gen-order-core-rows (rows)
  "Return core ROWS grouped by `dorg-sql-gen-core-table-order', each stable.
ROWS are typed rows of core tables in generation order; within one table
the result keeps that order.  A row of any other table signals
`dorg-sql-row-error' rather than being dropped.  ROWS is not modified; it
must be a finite proper list."
  (unless (proper-list-p rows)
    (signal 'dorg-sql-row-error (list "core rows must be a proper list")))
  (let ((buckets (mapcar #'list dorg-sql-gen-core-table-order)))
    (dolist (row rows)
      (let ((bucket (assoc (dorg-sql-row-table row) buckets)))
        (unless bucket
          (signal 'dorg-sql-row-error
                  (list "row table has no place in the core order"
                        (dorg-sql-row-table row))))
        (push row (cdr bucket))))
    (cl-mapcan (lambda (bucket) (nreverse (cdr bucket))) buckets)))

;;; Insert Ordering

(defun dorg-sql-gen--sort-insert-groups (groups parsed-by-id)
  "Sort INSERT GROUPS so parents come before children.
GROUPS is a list of (HEADING-ID . SQL-STMTS).  PARSED-BY-ID is a
hash of id -> `dorg-heading'.  Returns a flat list of SQL strings
with parent INSERTs preceding their children's."
  (if (null (cdr groups))
      ;; 0 or 1 group — nothing to sort
      (apply #'append (mapcar #'cdr groups))
    (let ((insert-ids (make-hash-table :test 'eql))
          (sorted nil))
      ;; Build set of IDs being inserted
      (dolist (g groups)
        (puthash (car g) t insert-ids))
      ;; Emit groups in dependency order: a group is ready when its
      ;; heading's parent_id is NOT in the remaining insert set.
      ;; Simple iterative approach — O(n^2) but n is small (tens).
      (let ((remaining (copy-sequence groups)))
        (while remaining
          (let ((emitted nil))
            (setq remaining
                  (cl-remove-if
                   (lambda (g)
                     (let* ((hid (car g))
                            (ph (gethash hid parsed-by-id))
                            (pid (and ph (dorg-heading-parent-id ph))))
                       (when (or (null pid)
                                 (not (gethash pid insert-ids)))
                         ;; Parent already exists or not in insert set
                         (remhash hid insert-ids)
                         (setq sorted (nconc sorted (cdr g)))
                         (setq emitted t)
                         t)))
                   remaining))
            (unless emitted
              ;; Cycle or missing parent — emit remaining to avoid hang
              (dolist (g remaining)
                (setq sorted (nconc sorted (cdr g))))
              (setq remaining nil)))))
      sorted)))

;;; Item-Level Lowering

(defun dorg-sql-gen--item-plan-possible-p (db-h)
  "Return non-nil if DB-H carries DB items with ids for item-level planning.
A nil-item (legacy/headline) DB heading, or any DB item lacking its
`heading_items.id', forces the whole-heading delete+reinsert fallback so
correctness beats partial optimization for unexpected cache states."
  (let ((items (and db-h (cl-remove-if #'dorg-item--db-local-p
                                       (dorg-heading-items db-h)))))
    (and items (cl-every #'dorg-item-id items))))

(defun dorg-sql-gen--lower-item-plan (new-h db-h id-gen)
  "Lower the item-level reconciliation of DB-H -> NEW-H to SQL statements.
Reuses existing `heading_items.id' via `dorg-model-plan-items': removed
items are deleted (cascading their detail rows), reused items get same-kind
detail UPDATEs and a `heading_items' row UPDATE only when their sort_key or
post_blank changed, and new items are inserted with fresh ids.  Every fresh
key is distinct from every old key of the list (`dorg-model-plan-items'), so
no statement order can violate `uq_heading_item_order (heading_id, sort_key)'
and no temporary keys are needed.  ID-GEN supplies fresh snowflake ids for
inserts."
  (let* ((hid (dorg-heading-id new-h))
         (ops (dorg-model-plan-items (dorg-heading-items db-h)
                                     (dorg-heading-items new-h)))
         (delete-ids nil)
         (detail-stmts nil)
         (row-stmts nil)
         (insert-stmts nil))
    (dolist (op ops)
      (pcase (car op)
        (:delete
         (push (dorg-item-id (nth 1 op)) delete-ids))
        ((or :keep :update)
         (let* ((kind (car op))
                (old (nth 1 op))
                (new (nth 2 op))
                (final-key (nth 3 op))
                (id (dorg-item-id old))
                (key-diff (not (equal (dorg-item-sort-key old) final-key)))
                (pb-diff (not (equal (dorg-item-post-blank old)
                                     (dorg-item-post-blank new)))))
           (dorg-sql-gen--bind-item-row new id final-key)
           (when (eq kind :update)
             (setq detail-stmts
                   (nconc detail-stmts (dorg-sql-gen-item-update old new))))
           (when (or key-diff pb-diff)
             (push (dorg-sql-gen-item-row-update
                    id final-key (dorg-item-post-blank new))
                   row-stmts))))
        (:insert
         (let* ((new (nth 1 op))
                (final-key (nth 2 op))
                (item-id (funcall id-gen)))
           (dorg-sql-gen--bind-item-row new item-id final-key)
           (setq insert-stmts
                 (nconc insert-stmts
                        (dorg-item-to-sql new item-id hid final-key)))))))
    (append
     ;; 1. Delete removed items (FK cascade clears their detail rows).
     (when delete-ids
       (list (format "DELETE FROM heading_items WHERE id IN (%s)"
                     (mapconcat #'number-to-string (nreverse delete-ids) ", "))))
     ;; 2. Detail-table updates for edited reused items.
     detail-stmts
     ;; 3. Final heading_items row updates (sort_key, post_blank).
     (nreverse row-stmts)
     ;; 4. Insert new items with fresh ids + planned sort keys.
     insert-stmts)))

(defun dorg-sql-gen-rekey-sql (table pairs)
  "Return one UPDATE moving every (ID . SORT-KEY) in PAIRS on TABLE to its key.
The new keys arrive as a VALUES row set joined on the primary key, so the
server does one index lookup per pair: linear in the window, one statement,
no fabricated row data.  (A CASE over N arms costs N^2 per statement and a
UNION ALL derived table does not plan; measured on Dolt 1.86.4, 10,000 rows:
VALUES join 0.3 s, CASE 13.6 s, UNION ALL over 300 s.)  The keys are fresh: no
row of TABLE holds any of them, so the statement cannot collide with itself in
any row order and needs no temporary keys."
  (format "UPDATE %s h JOIN (VALUES %s) AS v (id, k) ON v.id = h.id SET h.sort_key = v.k"
          table
          (mapconcat (lambda (pair)
                       (format "ROW(%d, %s)"
                               (car pair) (dorg-sql-gen--sort-key-hex (cdr pair))))
                     pairs ", ")))

;;; SQL Lowering from Heading Ops

(defun dorg-sql-gen-lower-heading-ops (db-ops db-headings id-gen)
  "Lower semantic DB-OPS to SQL statements.
DB-OPS is a list of tagged heading-level ops:
  (:insert-heading HEADING ROOT-ID)
  (:update-heading HEADING)
  (:rekey PARENT-ID ((HEADING-ID . SORT-KEY) ...))
  (:delete-heading HEADING-ID)
DB-HEADINGS is the DB snapshot heading hash (for tag/item diffing).
ID-GEN is a function returning snowflake IDs for items.
Returns a list of SQL strings for execution inside one transaction."
  (let ((delete-stmts nil)
        (update-stmts nil)
        (insert-groups nil)
        (insert-headings (make-hash-table :test 'eql)))
    (dolist (op db-ops)
      (pcase (car op)
        (:delete-heading
         (push (format "DELETE FROM headings WHERE id = %d" (cadr op))
               delete-stmts))
        (:insert-heading
         (let* ((h (cadr op))
                (root-id (caddr op))
                (hid (dorg-heading-id h)))
           (puthash hid h insert-headings)
           (push (cons hid (dorg-sql-gen-heading-full-sql h root-id id-gen))
                 insert-groups)))
        (:update-heading
         (let* ((h (cadr op))
                (hid (dorg-heading-id h))
                (db-h (gethash hid db-headings)))
           ;; Heading attr UPDATE
           (setq update-stmts
                 (nconc update-stmts
                        (list (dorg-sql-gen-heading-update h))))
           ;; Tag replacement if changed
           (let ((db-tags (and db-h (dorg-heading-tags db-h)))
                 (new-tags (dorg-heading-tags h)))
             (unless (equal new-tags db-tags)
               (setq update-stmts
                     (nconc update-stmts
                            (list (format "DELETE FROM heading_tags WHERE heading_id = %d" hid))
                            (dorg-sql-gen-tag-inserts hid new-tags)))))
           ;; Item replacement if changed.  Prefer item-level reconciliation
           ;; (reuse heading_items.id, minimal churn) when the DB heading
           ;; carries plan-able item ids; otherwise fall back to the
           ;; whole-heading delete+reinsert for legacy/headline snapshots.
           ;; Item plan whenever the DB snapshot makes one possible, even for
           ;; equal content: the plan is the sole point that binds persisted
           ;; item identities onto the candidate (`dorg-sql-gen--bind-item-row'),
           ;; which the in-transaction partition proof compares.  Unchanged
           ;; items lower to :keep ops and emit no SQL.  A legacy snapshot
           ;; without item ids keeps the changed-only whole-heading fallback,
           ;; so an unchanged metadata-poor heading never churns.
           (cond
            ((dorg-sql-gen--item-plan-possible-p db-h)
             (setq update-stmts
                   (nconc update-stmts
                          (dorg-sql-gen--lower-item-plan h db-h id-gen))))
            ((dorg-model-items-changed-p h db-h)
             (setq update-stmts
                   (nconc update-stmts
                          (cons (format "DELETE FROM heading_items WHERE heading_id = %d" hid)
                                (dorg-sql-gen-heading-items-sql h id-gen))))))))
        (:rekey
         (when (caddr op)
           (setq update-stmts
                 (nconc update-stmts
                        (list (dorg-sql-gen-rekey-sql "headings" (caddr op)))))))))
    ;; Topological sort inserts (parents before children for FK safety)
    (let ((insert-stmts (dorg-sql-gen--sort-insert-groups
                         insert-groups insert-headings)))
      (nconc (nreverse delete-stmts)
             update-stmts
             insert-stmts))))

(provide 'dorg-sql-gen)
;;; dorg-sql-gen.el ends here
