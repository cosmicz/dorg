;;; dorg-repo.el --- Dolt repository lifecycle for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Repository lifecycle operations for dorg: initialization, extension
;; schema application, and strict-bind root resolution.  Split from
;; `dorg-dolt.el' to break the circular dependency between dorg-dolt (SQL
;; primitive) and dorg-ext (extension registry).  This module sits above
;; both and can require them at top level.
;;
;; This is the backend layer that owns ALL strict-bind DDL: repo init,
;; schema migration, and root lookup/create.  `dorg-repo-ensure-root' is
;; the single entry through which the registry (`dorg-bind-dolt') resolves
;; a root, so the registry stays pure source classification + bookkeeping.

;;; Code:

(require 'dorg-schema)
(require 'dorg-dolt)
(require 'dorg-error)
(require 'dorg-ext)
(require 'dorg-model)
(require 'dorg-sql-gen)
(require 'cl-lib)
(require 'subr-x)
;; `dorg--create-root' (relocated here from `dorg-registry') allocates a
;; snowflake id via `dorg-id-generate', so the backend layer requires
;; `dorg-id' directly rather than leaning on the registry's require.
(require 'dorg-id)

(autoload 'dorg-dolt-server--start-bootstrap "dorg-dolt-server")
(autoload 'dorg-dolt-server--stop "dorg-dolt-server")
(autoload 'dorg-transaction-commit "dorg-transaction")
(autoload 'dorg-mutate-call-with-store-gate "dorg-mutate")
(autoload 'dorg-model-load--missing-offset-column-p "dorg-model-load")

(define-error 'dorg-binding-error
  "DOrg file/root binding is invalid"
  'dorg-error)

(defun dorg-binding-error-data (err)
  "Return structured metadata carried by binding condition ERR."
  (and (eq (car-safe err) 'dorg-binding-error)
       (dorg-error-data err)))

(defun dorg-binding-error-reason (err)
  "Return the stable reason symbol carried by binding condition ERR."
  (plist-get (dorg-binding-error-data err) :reason))

(defun dorg-repo--binding-error (reason message &rest data)
  "Signal `dorg-binding-error' with REASON, MESSAGE, and DATA."
  (signal 'dorg-binding-error
          (list message (append (list :reason reason) data))))

;;; ─── Extension Schema Migration ───────────────────────────────────
;;
;; Extension schema evolution is a first-class migration contract.
;; Dorg orchestrates version discovery, ordering, the `dorg_meta'
;; version bump, commit boundaries, and error reporting; each extension
;; owns the DDL/data statements that move it from version N to N+1.
;; Dorg never infers schema diffs and never downgrades.  A missing
;; migration step is an actionable error: the extension or operator must
;; supply a step before dorg will use that extension at a newer version.

(defvar dorg-repo--migration-resolver-scope nil
  "Dynamic scope authorizing migration resolver repo reads.
The value is a plist with :dolt-dir, :canonical-dolt-dir, and :lease while a
function-valued migration resolver is running on the explicit migration path.")

(defun dorg-repo--schema-required (reason message &rest data)
  "Signal `dorg-schema-migration-required' with REASON, MESSAGE, and DATA."
  (signal 'dorg-schema-migration-required
          (list message (append (list :reason reason) data))))

(defmacro dorg-repo--with-migration-resolver-scope (dolt-dir lease &rest body)
  "Run BODY with resolver read authority for DOLT-DIR on LEASE."
  (declare (indent 2) (debug (form form body)))
  `(let ((dorg-repo--migration-resolver-scope
          (list :dolt-dir ,dolt-dir
                :canonical-dolt-dir (dorg-dolt-server--canonical-dir ,dolt-dir)
                :lease ,lease)))
     ,@body))

(defun dorg-repo-migration-query (dolt-dir query &rest args)
  "Run resolver-scoped read QUERY for DOLT-DIR on the migration lease.
This helper is valid only inside a function-valued migration resolver invoked by
`dorg-migrate-schema'.  DOLT-DIR must name the exact store whose resolver is
active.  QUERY must format to one SELECT statement; writes, transaction control,
and multi-statement requests are refused before execution.  The helper is a read
route only; DOrg still owns returned migration SQL, version updates, and
commits."
  (unless dorg-repo--migration-resolver-scope
    (dorg-repo--schema-required
     'resolver-read-outside-scope
     "dorg: migration resolver read helper called outside an active migration resolver"))
  (let* ((scope dorg-repo--migration-resolver-scope)
         (expected (plist-get scope :canonical-dolt-dir))
         (actual (dorg-dolt-server--canonical-dir dolt-dir)))
    (unless (equal actual expected)
      (dorg-repo--schema-required
       'resolver-read-wrong-store
       (format "dorg: migration resolver read helper refused wrong store %s (active store is %s)"
               dolt-dir (plist-get scope :dolt-dir))
       :requested dolt-dir :active (plist-get scope :dolt-dir)))
    (let ((formatted (if args (apply #'format query args) query)))
      (unless (dorg-sql--validate-query-allowed-p formatted)
        (dorg-repo--schema-required
         'resolver-read-unsafe-sql
         (format "dorg: migration resolver read helper is SELECT-only: %s"
                 formatted)
         :query formatted))
      (dorg-sql--lease-exec (plist-get scope :lease) formatted))))

(defun dorg-repo--v6-to-v7-receipt-schema (dolt-dir)
  "Return the explicit v6 -> v7 receipt migration SQL for DOLT-DIR.

The resolver reads only through `dorg-repo-migration-query' on the already
leased store.  It creates missing receipt storage, upgrades only the known
legacy four-column shape, and refuses every other shape without repair."
  (pcase (dorg-sql--mutation-log-schema-status
          (lambda (sql) (dorg-repo-migration-query dolt-dir sql)))
    ('missing-table (list dorg-schema-mutation-log-ddl))
    ('legacy-missing-postimages
     '("ALTER TABLE dorg_mutation_log ADD COLUMN postimages_v1 TEXT NULL"))
    ('compatible nil)
    (`(incompatible . ,detail)
     (dorg-repo--schema-required
      'receipt-schema-incompatible
      (format "dorg: refusing to repair incompatible mutation receipt schema: %S"
              detail)
      :component 'receipt :receipt-status detail))))

;;; ─── v7 -> v8: parent-relative level offsets ─────────────────────────────
;;
;; Schema 8 replaces the absolute `headings.level' with `level_offset', each
;; heading's level relative to its parent.  The step validates the whole v7
;; store before emitting any DDL, generates the offset writes from that
;; validated expectation, and hands the step a validator that re-derives
;; effective levels from the written offsets inside the same transaction and
;; compares them with the v7 levels before the Dolt commit.  Any refusal or
;; mismatch leaves the store exactly as it was.

(define-error 'dorg-repo-v8-validation-failed
  "dorg: v7 -> v8 migration validation failed")

(defconst dorg-repo--v8-offset-batch 500
  "Rows per generated offset UPDATE in the v7 -> v8 step.")

(defun dorg-repo--v7-hierarchy-expectation (rows)
  "Validate v7 heading ROWS and return the expected v8 hierarchy, or refuse.
ROWS are alists of id, root_id, parent_id, sort_key (hex) and absolute
level for the whole store.  Every root must be level 0 with no parent;
every other row must have a parent in its own root at a strictly shallower
level, no cycle, and a sibling fan whose offsets never increase in sort
order.  Returns a hash id -> (PARENT ROOT SORT-KEY EFFECTIVE-LEVEL OFFSET)."
  (let ((by-id (make-hash-table :test 'eql))
        (expect (make-hash-table :test 'eql))
        (fans (make-hash-table :test 'eql)))
    (dolist (row rows)
      (puthash (alist-get 'id row) row by-id))
    (cl-flet ((refuse (detail &rest data)
                (dorg-repo--schema-required
                 'level-offset-source-inconsistent
                 (format "dorg: refusing v7 -> v8 migration, %s: %S" detail data)
                 :component 'core :detail detail :data data)))
      (maphash
       (lambda (id row)
         (let ((parent (alist-get 'parent_id row))
               (root (alist-get 'root_id row))
               (level (alist-get 'level row))
               (key (alist-get 'sort_key row)))
           (if (null parent)
               (progn
                 (unless (and (eql id root) (eql level 0))
                   (refuse 'root-shape id root level))
                 (puthash id (list nil root key 0 0) expect))
             (let ((p (gethash parent by-id)))
               (unless p (refuse 'dangling-parent id parent))
               (unless (eql (alist-get 'root_id p) root)
                 (refuse 'cross-root-parent id parent))
               (let ((offset (- level (alist-get 'level p))))
                 (unless (> offset 0) (refuse 'non-positive-offset id offset))
                 (push (cons key offset) (gethash parent fans))
                 (puthash id (list parent root key level offset) expect))))))
       by-id)
      (maphash
       (lambda (id _row)
         (let ((cur id) (steps 0))
           (while (alist-get 'parent_id (gethash cur by-id))
             (setq cur (alist-get 'parent_id (gethash cur by-id)))
             (when (> (cl-incf steps) (hash-table-count by-id))
               (refuse 'cycle id)))))
       by-id)
      (maphash
       (lambda (parent fan)
         (let ((offsets (mapcar #'cdr (sort fan (lambda (a b)
                                                   (string< (car a) (car b)))))))
           (while (cdr offsets)
             (when (< (car offsets) (cadr offsets))
               (refuse 'nonrepresentable-sibling-order parent))
             (setq offsets (cdr offsets)))))
       fans))
    expect))

(defun dorg-repo--v8-hierarchy-validator (expect)
  "Return a pre-commit validator comparing the migrated store with EXPECT.
The validator receives the transaction's SELECT-only QUERY function.  It
requires `level' gone and `level_offset' present on headings, re-derives
effective levels in Elisp from one flat SELECT (the window forbids
recursion), and signals unless every row's (PARENT ROOT SORT-KEY
EFFECTIVE-LEVEL OFFSET) equals EXPECT exactly."
  (lambda (query)
    (let ((cols (mapcar (lambda (row)
                          (downcase (format "%s" (cdar row))))
                        (funcall query "SELECT column_name FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'headings'"))))
      (when (member "level" cols)
        (signal 'dorg-repo-v8-validation-failed '(level-column-present)))
      (unless (member "level_offset" cols)
        (signal 'dorg-repo-v8-validation-failed '(level-offset-column-missing))))
    (let ((rows (funcall query "SELECT id, parent_id, root_id, HEX(sort_key) AS sort_key, level_offset FROM headings"))
          (by-root (make-hash-table :test 'eql)))
      (unless (= (length rows) (hash-table-count expect))
        (signal 'dorg-repo-v8-validation-failed
                (list 'row-count (length rows) (hash-table-count expect))))
      (dolist (row rows)
        (let ((table (or (gethash (alist-get 'root_id row) by-root)
                         (puthash (alist-get 'root_id row)
                                  (make-hash-table :test 'eql) by-root))))
          (puthash (alist-get 'id row)
                   (make-dorg-heading :id (alist-get 'id row)
                                      :parent-id (alist-get 'parent_id row)
                                      :level-offset (alist-get 'level_offset row))
                   table)))
      (maphash (lambda (root table) (dorg-model-derive-levels table root)) by-root)
      (dolist (row rows)
        (let* ((id (alist-get 'id row))
               (h (gethash id (gethash (alist-get 'root_id row) by-root)))
               (got (list (alist-get 'parent_id row) (alist-get 'root_id row)
                          (alist-get 'sort_key row) (dorg-heading-level h)
                          (alist-get 'level_offset row))))
          (unless (equal got (gethash id expect))
            (signal 'dorg-repo-v8-validation-failed
                    (list 'heading id got (gethash id expect)))))))))

(defun dorg-repo--v7-to-v8-level-offset (dolt-dir)
  "Return the explicit v7 -> v8 level-offset step for DOLT-DIR.
Reads the whole v7 hierarchy through `dorg-repo-migration-query' on the
leased store, refuses an inconsistent store before any statement, and
returns (:statements STATEMENTS :validate FN).  The offsets are generated
from the validated expectation, so no multi-table UPDATE is required."
  (let* ((rows (dorg-repo-migration-query
                dolt-dir
                "SELECT id, root_id, parent_id, HEX(sort_key) AS sort_key, level FROM headings"))
         (expect (dorg-repo--v7-hierarchy-expectation rows))
         (pairs nil)
         (updates nil))
    (maphash (lambda (id e) (push (cons id (nth 4 e)) pairs)) expect)
    (setq pairs (sort pairs (lambda (a b) (< (car a) (car b)))))
    (while pairs
      (let ((batch (seq-take pairs dorg-repo--v8-offset-batch)))
        (setq pairs (nthcdr dorg-repo--v8-offset-batch pairs))
        (push (format "UPDATE headings SET level_offset = CASE id %s END WHERE id IN (%s)"
                      (mapconcat (lambda (p) (format "WHEN %d THEN %d" (car p) (cdr p)))
                                 batch " ")
                      (mapconcat (lambda (p) (number-to-string (car p))) batch ","))
              updates)))
    (list :statements
          (append (list "ALTER TABLE headings ADD COLUMN level_offset INT NULL")
                  (nreverse updates)
                  (list "ALTER TABLE headings MODIFY COLUMN level_offset INT NOT NULL"
                        "ALTER TABLE headings DROP COLUMN level"))
          :validate (dorg-repo--v8-hierarchy-validator expect))))

(defun dorg-repo--ensure-managed-migration-transport (dolt-dir)
  "Refuse explicit schema migration when DOLT-DIR is pinned away from server.
The refusal happens before gate/lease acquisition, server startup/attachment,
resolver evaluation, or writes."
  (let ((pinned (dorg-sql--pinned-transport-for dolt-dir)))
    (when (eq (car-safe pinned) 'cli)
      (dorg-repo--schema-required
       'unsupported-offline-migration
       (format (concat "dorg: dorg-migrate-schema requires managed server "
                       "transport for %s; offline/CLI schema migration is "
                       "unsupported pending the offline migration follow-up")
               dolt-dir)
       :transport 'cli))
    (unless (eq dorg-sql-transport 'server)
      (dorg-repo--schema-required
       'unsupported-offline-migration
       (format (concat "dorg: dorg-migrate-schema requires managed server "
                       "transport for %s; `dorg-sql-transport' is %S")
               dolt-dir dorg-sql-transport)
       :transport dorg-sql-transport))))

(defun dorg-repo--ext-version-key (name)
  "Return the `dorg_meta' key string tracking extension NAME's version."
  (format "ext:%s:version" (symbol-name name)))

(defconst dorg-repo--store-id-key "store-id"
  "DOrg-owned store identity key in the existing dorg_meta table.")

(defun dorg-repo--new-store-id ()
  "Return 128 random bits from the OS as canonical lowercase hex."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; Emacs cannot seek /dev/urandom, so read exactly one block via head.
    (unless (and (eq 0 (call-process "head" nil (current-buffer) nil
                                     "-c" "16" "/dev/urandom"))
                 (= 16 (buffer-size)))
      (error "dorg: OS random source returned fewer than 16 bytes"))
    (mapconcat (lambda (byte) (format "%02x" byte))
               (string-to-list (buffer-string)) "")))

(defun dorg-repo--store-id-value (rows)
  "Validate ROWS from one store-id lookup and return its value or nil."
  (when (cdr rows)
    (error "dorg: multiple store-id rows"))
  (when rows
    (let ((value (alist-get 'value (car rows))))
      (unless (and (stringp value)
                   (string-match-p "\\`[a-f0-9]\\{32\\}\\'" value))
        (error "dorg: malformed store-id metadata"))
      value)))

(defun dorg-repo-store-id (dolt-dir)
  "Read DOLT-DIR's stable DOrg store identity, or nil if unidentified."
  (dorg-repo--store-id-value
   (dorg-sql dolt-dir "SELECT value FROM dorg_meta WHERE `key` = %s"
             (dorg-sql-quote dorg-repo--store-id-key))))

(defun dorg-repo-identify-store (dolt-dir fork)
  "Identify DOLT-DIR under its store gate, or return a typed refusal.
FORK permits replacing an existing identity while recording the prior value.
No identity is minted until the clean-workset and existing-ID checks pass."
  (dorg-mutate-call-with-store-gate
   dolt-dir
   (lambda (lease _key)
     (if (dorg-sql--lease-exec
          lease "SELECT table_name FROM dolt_status LIMIT 1")
         (list :refusal 'dirty-working-set)
       (let* ((rows (dorg-sql--lease-exec
                     lease "SELECT value FROM dorg_meta WHERE `key` = %s"
                     (dorg-sql-quote dorg-repo--store-id-key)))
              (old (dorg-repo--store-id-value rows)))
         (if (and old (not fork))
             (list :refusal 'store-identified :store-id old)
           (let* ((new (dorg-repo--new-store-id))
                  (statements
                   (if old
                       (list
                        (format
                         "INSERT INTO dorg_meta (`key`, value) VALUES (%s, %s)"
                         (dorg-sql-quote (concat "store-id:prior:" new))
                         (dorg-sql-quote old))
                        (format
                         "UPDATE dorg_meta SET value = %s WHERE `key` = %s AND value = %s"
                         (dorg-sql-quote new)
                         (dorg-sql-quote dorg-repo--store-id-key)
                         (dorg-sql-quote old)))
                     (list
                      (format
                       "INSERT INTO dorg_meta (`key`, value) VALUES (%s, %s)"
                       (dorg-sql-quote dorg-repo--store-id-key)
                       (dorg-sql-quote new)))))
                  (commit (dorg-sql--lease-batch
                           lease statements
                           (if old "dorg: fork store identity"
                             "dorg: identify store"))))
             (list :store-id new :previous-id old :fork (and old t)
                   :commit commit))))))))

(defun dorg-repo--ext-version-update-sql (name version)
  "Return SQL that records extension NAME at VERSION in `dorg_meta'.
Dorg, not the extension, owns this metadata update; it is appended to
the same batch as the schema/migration statements so a failure before
it runs leaves the recorded version unchanged."
  (format (concat "INSERT INTO dorg_meta (`key`, value) VALUES (%s, '%d') "
                  "ON DUPLICATE KEY UPDATE value = '%d'")
          (dorg-sql-quote (dorg-repo--ext-version-key name))
          version version))

(defun dorg-repo--schema-version-update-sql (version)
  "Return SQL that records core schema VERSION in `dorg_meta'."
  (format (concat "UPDATE dorg_meta SET value = '%d' "
                  "WHERE `key` = 'schema_version'")
          version))

(defun dorg-repo--ext-db-version (dolt-dir name)
  "Return the recorded schema version integer for extension NAME in DOLT-DIR.
Returns nil when the extension has no recorded version (not installed).
Signals an error when the stored value is not a non-negative integer."
  (let ((rows (dorg-sql dolt-dir
                        "SELECT value FROM dorg_meta WHERE `key` = %s"
                        (dorg-sql-quote (dorg-repo--ext-version-key name)))))
    (when rows
      (let ((val (alist-get 'value (car rows))))
        (cond
         ((integerp val) val)
         ((and (stringp val) (string-match-p "\\`[0-9]+\\'" val))
          (string-to-number val))
         (t (error "dorg: malformed %s metadata in %s: %S"
                   (dorg-repo--ext-version-key name) dolt-dir val)))))))

(defun dorg-repo--read-meta-value (read-fn dolt-dir key)
  "Read metadata KEY in DOLT-DIR through READ-FN.
READ-FN receives DOLT-DIR, a query, and format arguments, and returns rows as
alist rows.  The return value is (:ok VALUE), (:absent nil),
(:missing-table ERR), or (:multiple ROWS).  Transport, lease, and unrelated
read failures are re-signaled unchanged."
  (condition-case err
      (let ((rows (funcall read-fn
                           dolt-dir
                           "SELECT value FROM dorg_meta WHERE `key` = %s"
                           (dorg-sql-quote key))))
        (cond
         ((null rows) (list :absent nil))
         ((cdr rows) (list :multiple rows))
         (t (list :ok (alist-get 'value (car rows))))))
    (error
     (if (dorg-repo--missing-meta-table-error-p err)
         (list :missing-table err)
       (signal (car err) (cdr err))))))

(defun dorg-repo--missing-meta-table-error-p (err)
  "Return non-nil when ERR means the `dorg_meta' table itself is absent."
  (and (not (memq 'dorg-sql-transport-error
                  (get (car-safe err) 'error-conditions)))
       (let ((message (error-message-string err)))
         (and (string-match-p "\\bdorg_meta\\b" message)
              (string-match-p
               (concat "doesn'?t exist\\|does not exist\\|no such table"
                       "\\|unknown table\\|not found")
               (downcase message))))))

(defun dorg-repo--parse-metadata-version (raw)
  "Return integer version parsed from RAW, or nil when malformed."
  (cond
   ((integerp raw) raw)
   ((and (stringp raw) (string-match-p "\\`[0-9]+\\'" raw))
    (string-to-number raw))
   (t nil)))

(defun dorg-repo--migration-chain-complete-p (from to migrations)
  "Return non-nil when MIGRATIONS has every FROM..TO-1 step."
  (let ((ok t)
        (version from))
    (while (< version to)
      (unless (alist-get version migrations)
        (setq ok nil))
      (setq version (1+ version)))
    ok))

(defun dorg-repo--component-states (dolt-dir extensions read-fn)
  "Return compatibility states for core and EXTENSIONS using READ-FN.
This is the single component-state evaluator shared by ordinary checks,
explicit leased migration, and offline/test seams.  EXTENSIONS nil means all
registered extensions; the private value `:core-only' skips extension states."
  (let* ((core-read (dorg-repo--read-meta-value read-fn dolt-dir "schema_version"))
         (core-raw (cadr core-read))
         (core-version (and (eq (car core-read) :ok)
                            (dorg-repo--parse-metadata-version core-raw)))
         (states nil))
    (push
     (cond
      ((eq (car core-read) :missing-table)
       (list :component 'core :state 'missing :detail core-raw
             :target dorg-schema-version))
      ((eq (car core-read) :absent)
       (list :component 'core :state 'missing :target dorg-schema-version))
      ((eq (car core-read) :multiple)
       (list :component 'core :state 'malformed :current core-raw
             :target dorg-schema-version))
      ((null core-version)
       (list :component 'core :state 'malformed :current core-raw
             :target dorg-schema-version))
      ((> core-version dorg-schema-version)
       (list :component 'core :state 'newer :current core-version
             :target dorg-schema-version))
      ((< core-version 6)
       (list :component 'core :state 'pre-v6 :current core-version
             :target dorg-schema-version))
      ((< core-version dorg-schema-version)
       (list :component 'core
             :state (if (dorg-repo--migration-chain-complete-p
                         core-version dorg-schema-version dorg-schema-migrations)
                        'needs-migration
                      'missing-step)
             :current core-version :target dorg-schema-version))
      (t
       (list :component 'core :state 'current :current core-version
             :target dorg-schema-version)))
     states)
    (dolist (ext (cond
                  ((eq extensions :core-only) nil)
                  (extensions extensions)
                  (t (dorg-registered-extensions))))
      (let* ((name (dorg-extension-name ext))
             (target (dorg-extension-version ext))
             (read (dorg-repo--read-meta-value
                    read-fn dolt-dir (dorg-repo--ext-version-key name)))
             (raw (cadr read))
             (current (and (eq (car read) :ok)
                           (dorg-repo--parse-metadata-version raw))))
        (push
         (cond
          ((eq (car read) :missing-table)
           (list :component 'extension :name name :state 'missing
                 :detail raw :target target))
          ((eq (car read) :absent)
           (list :component 'extension :name name :state 'absent
                 :target target))
          ((eq (car read) :multiple)
           (list :component 'extension :name name :state 'malformed
                 :current raw :target target))
          ((null current)
           (list :component 'extension :name name :state 'malformed
                 :current raw :target target))
          ((> current target)
           (list :component 'extension :name name :state 'newer
                 :current current :target target))
          ((< current target)
           (list :component 'extension :name name
                 :state (if (dorg-repo--migration-chain-complete-p
                             current target (dorg-extension-migrations ext))
                            'needs-migration
                          'missing-step)
                 :current current :target target))
          (t
           (list :component 'extension :name name :state 'current
                 :current current :target target)))
         states)))
    (nreverse states)))

(defun dorg-repo--ordinary-read (dolt-dir query &rest args)
  "Ordinary read seam for the compatibility evaluator."
  (apply #'dorg-sql dolt-dir query args))

(defun dorg-repo--leased-read (lease)
  "Return a leased read seam for the compatibility evaluator."
  (lambda (_dolt-dir query &rest args)
    (apply #'dorg-sql--lease-exec lease query args)))

(defun dorg-repo--states-all-current-p (states)
  "Return non-nil when every component in STATES is current."
  (cl-every (lambda (state) (eq (plist-get state :state) 'current)) states))

(defun dorg-repo--state-label (state)
  "Return a compact diagnostic label for component STATE."
  (pcase (plist-get state :component)
    ('core (format "core:%s"
                   (plist-get state :state)))
    ('extension
     (format "extension:%s:%s"
             (plist-get state :name)
             (plist-get state :state)))
    (_ (format "%S" state))))

(defun dorg-repo--schema-refusal-message (dolt-dir states)
  "Return ordinary schema refusal text for DOLT-DIR and component STATES."
  (format (concat "dorg: repo at %s needs explicit schema reconciliation "
                  "for %s; run M-x dorg-migrate-schema with the required "
                  "extensions registered")
          dolt-dir
          (mapconcat #'dorg-repo--state-label
                     (cl-remove-if (lambda (state)
                                     (eq (plist-get state :state) 'current))
                                   states)
                     ", ")))

(defun dorg-repo--signal-component-states (dolt-dir states)
  "Signal the typed refusal represented by STATES for DOLT-DIR."
  (let ((newer (cl-find 'newer states :key (lambda (s) (plist-get s :state)))))
    (if newer
        (signal 'dorg-schema-repo-newer
                (list (format
                       "dorg: repo at %s has newer schema component %s than this dorg supports"
                       dolt-dir (dorg-repo--state-label newer))
                      (list :reason 'schema-newer
                            :components states)))
      (dorg-repo--schema-required
       'schema-migration-required
       (dorg-repo--schema-refusal-message dolt-dir states)
       :components states))))

(defun dorg-repo--ordinary-component-check (dolt-dir &optional extensions)
  "Read-only ordinary compatibility check for DOLT-DIR and EXTENSIONS."
  (let ((states (dorg-repo--component-states
                 dolt-dir extensions #'dorg-repo--ordinary-read)))
    (unless (dorg-repo--states-all-current-p states)
      (dorg-repo--signal-component-states dolt-dir states))
    states))

(defun dorg-repo--ext-resolve-step (dolt-dir ext from-version step)
  "Resolve STEP migrating EXT from FROM-VERSION to a list of SQL strings.
STEP is SQL (string or list of strings) or a function of one argument
\(DOLT-DIR) returning SQL.  Signals an actionable error if STEP names a
function that is not defined."
  (let ((sql (condition-case _err
                 (cond
                  ((functionp step) (funcall step dolt-dir))
                  ((and (symbolp step) step)
                   (error (concat "dorg: migration step for %s v%d -> v%d names "
                                  "undefined function %S")
                          (dorg-extension-name ext) from-version (1+ from-version)
                          step))
                  (t step))
               (dorg-sql-lease-busy
                (dorg-repo--schema-required
                 'resolver-ordinary-sql
                 (format (concat "dorg: migration resolver for extension %s v%d -> v%d "
                                 "used ordinary SQL while the store is leased; "
                                 "use dorg-repo-migration-query for repo reads")
                         (dorg-extension-name ext) from-version (1+ from-version))
                 :extension (dorg-extension-name ext)
                 :from from-version :to (1+ from-version))))))
    (dorg-ext-normalize-sql sql)))

(defun dorg-repo--ext-fresh-statements (ext)
  "Return the SQL statements that install EXT's fresh schema and record version.
The `dorg_meta' version bump is the last statement so that, when run
through `dorg-sql-batch's transaction path, a failure in the schema
statements rolls back without advancing the recorded version."
  (append (dorg-ext-normalize-sql (dorg-extension-schema ext))
          (list (dorg-repo--ext-version-update-sql
                 (dorg-extension-name ext)
                 (dorg-extension-version ext)))))

(defun dorg-repo--apply-extension-fresh (dolt-dir ext)
  "Apply EXT's fresh schema to DOLT-DIR and record its version (no commit).
Used by `dorg-init', where the statements are folded into the single
init commit.  Existing-store extension installation is owned by the
explicit leased migration path."
  (dorg-sql-batch dolt-dir (dorg-repo--ext-fresh-statements ext)))

(defun dorg-repo--working-set-clean-p (dolt-dir)
  "Return non-nil when DOLT-DIR has no uncommitted Dolt working-set changes.
A clean working set is one where `dolt_status' reports no staged or
unstaged table changes."
  (null (dorg-sql dolt-dir "SELECT table_name FROM dolt_status LIMIT 1")))

(defun dorg-repo--ensure-clean-working-set (dolt-dir operation)
  "Signal an actionable error when DOLT-DIR has uncommitted Dolt changes.
OPERATION is a short description of the step being guarded, used in the
error message.  Committed schema steps — core schema migrations and
extension install/migration alike — must run against a clean working
set: their failure path issues `DOLT_RESET --hard', which discards the
entire working set, so enforcing cleanliness up front is what keeps that
reset from erasing unrelated pending work."
  (unless (dorg-repo--working-set-clean-p dolt-dir)
    (error (concat "dorg: refusing \"%s\" — %s has uncommitted Dolt "
                   "working-set changes; commit or discard them before "
                   "running this schema step")
           operation dolt-dir)))

(defun dorg-repo-ensure-extensions (dolt-dir &optional _extensions)
  "Refuse the retired direct extension installer for DOLT-DIR.
Existing-store extension install/upgrade is owned by the explicit operator
entry `dorg-migrate-schema', which runs on the leased migration path.  Keeping
this public function as an ordinary-SQL mutator would create a second schema
apply path."
  (dorg-repo--schema-required
   'explicit-migration-required
   (format (concat "dorg: dorg-repo-ensure-extensions no longer mutates "
                   "existing stores; run M-x dorg-migrate-schema for %s")
           dolt-dir)))

;;; ─── Schema Migration ────────────────────────────────────────────────

(defun dorg-repo--db-schema-version (dolt-dir)
  "Return the schema version integer stored in DOLT-DIR's dorg_meta table."
  (let ((rows (dorg-sql dolt-dir
                "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
    (if rows
        (let ((val (alist-get 'value (car rows))))
          (if (stringp val) (string-to-number val) val))
      (error "dorg: no schema_version in dorg_meta for %s" dolt-dir))))

(defun dorg-repo-check-schema-compat (dolt-dir)
  "Compare DOLT-DIR's recorded core schema version against this dorg.
Read-only preflight that performs NO migration or mutation.  Returns
`:current' when the repo matches `dorg-schema-version' and
`:needs-migration' when the repo is older.

Signals `dorg-schema-repo-newer' — with an actionable message naming the
repo path, both versions, and this dorg's schema source location — when
the repo is NEWER than this dorg can handle.  A newer repo cannot be made
compatible by migrating (dorg never downgrades): the dorg installation
that opened it must be upgraded.  If it is a vendored copy, such as a
CLI's `lib/dorg' submodule, update the submodule and rebuild the CLI.

Catchable by callers (CLI/sync) that prefer to degrade to a warning for
read-only access rather than propagate the error."
  (let* ((state (car (dorg-repo--component-states
                      dolt-dir :core-only #'dorg-repo--ordinary-read)))
         (status (plist-get state :state))
         (db-version (plist-get state :current)))
    (cond
     ((eq status 'newer)
      (signal 'dorg-schema-repo-newer
              (list (format
                     (concat "dorg: repo at %s is schema v%d but this dorg "
                             "only supports %s; upgrade this dorg installation "
                             "(a vendored lib/dorg copy must be updated and its "
                             "CLI rebuilt) or open a repo of schema v%d or lower")
                     dolt-dir db-version
                     (dorg-schema--source-location)
                     dorg-schema-version)
                    (list :reason 'schema-newer :components (list state)))))
     ((eq status 'current) :current)
     ((memq status '(needs-migration pre-v6 missing malformed missing-step))
      :needs-migration)
     (t :needs-migration))))

(defun dorg-repo--announce-migration (dolt-dir from to)
  "Warn that DOLT-DIR is being migrated FROM->TO before the upgrade runs.
The migration is a ONE-WAY schema upgrade of a possibly-shared repo: any
older co-installed dorg (e.g. a CLI's vendored `lib/dorg') is locked out
of writing the repo until it too is upgraded.  Emitting this BEFORE the
migration makes the upgrade auditable rather than silent — the core
complaint about silent schema skew."
  (display-warning
   'dorg
   (format (concat "Migrating Dolt repo %s from schema v%d to v%d (%s). "
                   "This is a ONE-WAY upgrade; any older co-installed dorg "
                   "(e.g. a CLI's vendored lib/dorg) will be LOCKED OUT of "
                   "writes until it too is upgraded.")
           dolt-dir from to (dorg-schema--source-location))
   :warning))

(defun dorg-repo--resolve-core-migration-step (dolt-dir migration)
  "Resolve core MIGRATION to SQL for DOLT-DIR.
Returns the normalized statement list.  A resolver function that returns
\\(:statements STATEMENTS :validate FN) instead yields that plist with its
statements normalized; FN is the step's pre-commit validator."
  (let ((sql (condition-case _err
                 (cond
                  ((functionp migration) (funcall migration dolt-dir))
                  ((and (symbolp migration) (fboundp migration))
                   (funcall migration dolt-dir))
                  ((symbolp migration)
                   (error "dorg: core migration names undefined function %S" migration))
                  (t migration))
               (dorg-sql-lease-busy
                (dorg-repo--schema-required
                 'resolver-ordinary-sql
                 (concat "dorg: core migration resolver used ordinary SQL while "
                         "the store is leased; use dorg-repo-migration-query "
                         "for repo reads"))))))
    (if (eq (car-safe sql) :statements)
        (list :statements (dorg-ext-normalize-sql (plist-get sql :statements))
              :validate (plist-get sql :validate))
      (dorg-ext-normalize-sql sql))))

(defun dorg-repo-ensure-schema (dolt-dir &optional extensions)
  "Read-only ordinary compatibility check for DOLT-DIR and EXTENSIONS.
Ordinary open/read/write/import/rebind never installs, upgrades, repairs, or
runs migration resolvers.  Incompatible existing stores signal a typed
`dorg-schema-migration-required' or `dorg-schema-repo-newer' before any write."
  (dorg-repo--ordinary-component-check dolt-dir extensions)
  nil)

(defun dorg-repo--lease-working-set-clean-p (lease)
  "Return non-nil when LEASE sees no Dolt working-set changes."
  (null (dorg-sql--lease-exec lease "SELECT table_name FROM dolt_status LIMIT 1")))

(defun dorg-repo--ensure-lease-clean-working-set (lease dolt-dir operation)
  "Signal a typed refusal if LEASE sees a dirty working set for OPERATION."
  (unless (dorg-repo--lease-working-set-clean-p lease)
    (dorg-repo--schema-required
     'dirty-working-set
     (format (concat "dorg: refusing \"%s\" — %s has uncommitted Dolt "
                     "working-set changes; commit or discard them before "
                     "running dorg-migrate-schema")
             operation dolt-dir))))

(defun dorg-repo--migration-preflight-blockers (states allow-absent-extensions)
  "Return preflight blockers in STATES.
When ALLOW-ABSENT-EXTENSIONS is non-nil, absent extension states are installable
by the explicit operator path and are not blockers."
  (cl-remove-if
   (lambda (state)
     (or (memq (plist-get state :state) '(current needs-migration))
         (and allow-absent-extensions
              (eq (plist-get state :component) 'extension)
              (eq (plist-get state :state) 'absent))))
   states))

(defun dorg-repo--apply-schema-step-on-lease
    (lease dolt-dir statements commit-message &optional validate)
  "Apply STATEMENTS as one explicit migration step on LEASE.
VALIDATE, when non-nil, runs inside the step transaction after every
statement and before the Dolt commit with a SELECT-only query function; a
signal from it rolls the whole step back through the handler below."
  (condition-case err
      (dorg-sql--lease-batch lease statements commit-message validate t)
    (dorg-sql-transaction-rolled-back
     (condition-case nil
         (dorg-sql--lease-exec lease "CALL DOLT_RESET('--hard')")
       (error
        (signal 'dorg-sql-transaction-rollback-failed
                (list (format (concat "Dolt schema step transaction rolled back "
                                      "but reset did not complete for %s")
                              dolt-dir)
                      (append (or (dorg-sql-transaction-rollback-data err) nil)
                              (list :reset-confirmed nil
                                    :reset-error 'reset-failed))))))
     (signal (car err)
             (list (cadr err)
                   (append (or (dorg-sql-transaction-rollback-data err) nil)
                           (list :reset-confirmed t)))))))

(defun dorg-repo--migrate-core-on-lease (lease dolt-dir from-version)
  "Migrate core schema on LEASE from FROM-VERSION to current."
  (let ((version from-version))
    (while (< version dorg-schema-version)
      (let ((migration (alist-get version dorg-schema-migrations)))
        (unless migration
          (dorg-repo--schema-required
           'missing-core-migration-step
           (format "dorg: no migration defined for v%d -> v%d"
                   version (1+ version))
           :component 'core :from version :to (1+ version)))
        (let* ((resolved (dorg-repo--with-migration-resolver-scope dolt-dir lease
                           (dorg-repo--resolve-core-migration-step dolt-dir migration)))
               (step (if (eq (car-safe resolved) :statements)
                         resolved
                       (list :statements resolved))))
          (dorg-repo--apply-schema-step-on-lease
           lease dolt-dir
           (append (plist-get step :statements)
                   (list (dorg-repo--schema-version-update-sql (1+ version))))
           (format "dorg: migrate schema v%d -> v%d" version (1+ version))
           (plist-get step :validate)))
        (setq version (1+ version))))))

(defun dorg-repo--install-extension-on-lease (lease dolt-dir ext)
  "Install EXT as one explicit migration step on LEASE."
  (dorg-repo--apply-schema-step-on-lease
   lease dolt-dir
   (dorg-repo--ext-fresh-statements ext)
   (format "dorg: install extension %s v%d"
           (dorg-extension-name ext)
           (dorg-extension-version ext))))

(defun dorg-repo--migrate-extension-on-lease (lease dolt-dir ext from-version)
  "Migrate EXT on LEASE from FROM-VERSION to its registered version."
  (let ((version from-version)
        (target (dorg-extension-version ext))
        (name (dorg-extension-name ext)))
    (while (< version target)
      (let ((step (alist-get version (dorg-extension-migrations ext))))
        (unless step
          (dorg-repo--schema-required
           'missing-extension-migration-step
           (format "dorg: no migration for extension %s v%d -> v%d"
                   name version (1+ version))
           :component 'extension :extension name
           :from version :to (1+ version)))
        (dorg-repo--apply-schema-step-on-lease
         lease dolt-dir
         (append
          (dorg-repo--with-migration-resolver-scope dolt-dir lease
            (dorg-repo--ext-resolve-step dolt-dir ext version step))
          (list (dorg-repo--ext-version-update-sql name (1+ version))))
         (format "dorg: migrate extension %s v%d -> v%d"
                 name version (1+ version)))
        (setq version (1+ version))))))

(defun dorg-repo--migrate-schema-on-lease (dolt-dir lease &optional extensions)
  "Explicitly migrate DOLT-DIR on LEASE."
  (let* ((selected (or extensions (dorg-registered-extensions)))
         (states (dorg-repo--component-states
                  dolt-dir selected (dorg-repo--leased-read lease)))
         (blockers (dorg-repo--migration-preflight-blockers states t)))
    (when blockers
      (dorg-repo--signal-component-states dolt-dir blockers))
    (unless (dorg-repo--states-all-current-p states)
      (dorg-repo--ensure-lease-clean-working-set lease dolt-dir "dorg-migrate-schema"))
    (let ((core-state (car states)))
      (when (eq (plist-get core-state :state) 'needs-migration)
        (dorg-repo--migrate-core-on-lease
         lease dolt-dir (plist-get core-state :current))))
    (dolist (ext selected)
      (let* ((name (dorg-extension-name ext))
             (state (cl-find-if
                     (lambda (entry)
                       (and (eq (plist-get entry :component) 'extension)
                            (eq (plist-get entry :name) name)))
                     states)))
        (pcase (plist-get state :state)
          ('absent (dorg-repo--install-extension-on-lease lease dolt-dir ext))
          ('needs-migration
           (dorg-repo--migrate-extension-on-lease
            lease dolt-dir ext (plist-get state :current)))
          ('current nil))))
    ;; A current v7 metadata row does not prove the table was not damaged after
    ;; migration.  Report that integrity fault honestly; never auto-repair it.
    (dorg-sql--require-mutation-log-schema lease))
  nil)

;;;###autoload
(defun dorg-migrate-schema (dolt-dir)
  "Explicitly migrate DOLT-DIR to this dorg's current core schema.
The operator path owns schema changes through the leased migration critical
section over managed-server transport.  Ordinary open never calls this function
implicitly, and offline/CLI-pinned schema migration is unsupported.  A repo
NEWER than this dorg still signals `dorg-schema-repo-newer' — migration never
downgrades.  If all selected components are already current, this returns
without applying a schema batch or requiring a clean working set."
  (interactive "DDolt directory to migrate: ")
  (dorg-repo--ensure-managed-migration-transport dolt-dir)
  (dorg-mutate-call-with-store-gate
   dolt-dir
   (lambda (lease _key)
     (dorg-repo--migrate-schema-on-lease dolt-dir lease)))
  (when (called-interactively-p 'any)
    (message "dorg: %s now at %s" dolt-dir (dorg-schema--source-location))))

;;; ─── Root Resolution ───────────────────────────────────────────────
;;
;; Strict-bind root DDL/DML.  Relocated verbatim from `dorg-registry' so
;; the backend layer owns ALL init/schema/root lifecycle and the registry
;; stays pure source classification + registry bookkeeping + bind UI.
;; `dorg-repo-ensure-root' is the single backend entry through which the
;; registry resolves a root; it is not public-API-censused (consistent
;; with `dorg-repo-ensure-schema' / `dorg-init').

(defun dorg-repo--canonical-local-path (path role)
  "Return canonical absolute local PATH, or refuse it as ROLE.
ROLE is a short diagnostic noun such as \"Dolt directory\" or \"Org file\"."
  (unless (and (stringp path) (not (string-empty-p path)))
    (dorg-repo--binding-error
     'invalid-locator (format "dorg: %s must be a nonempty local path" role)
     :path path))
  (when (file-remote-p path)
    (dorg-repo--binding-error
     'invalid-locator (format "dorg: %s must not be remote: %s" role path)
     :path path))
  (unless (file-name-absolute-p path)
    (dorg-repo--binding-error
     'invalid-locator (format "dorg: %s must be absolute: %s" role path)
     :path path))
  (let ((expanded (expand-file-name path)))
    (when (file-directory-p expanded)
      (if (string= role "Dolt directory")
          (setq expanded (file-name-as-directory expanded))
        (dorg-repo--binding-error
         'invalid-locator (format "dorg: %s names a directory: %s" role path)
         :path path)))
    ;; `file-truename' resolves every existing ancestor even when the leaf
    ;; itself does not exist.  That matters on macOS where /var aliases
    ;; /private/var: encoding before the store or Org file is created must
    ;; produce the same bytes as encoding after creation.
    (dorg--canonical-existing-case (file-truename expanded))))

(defun dorg-repo-encode-relative-path (dolt-dir abs-file)
  "Encode ABS-FILE relative to canonical DOLT-DIR.
The result may contain leading parent segments (the canonical Downstream tracker layout
uses `../beads.org'), but it must be normalized and round-trip to the same
canonical local target."
  (let* ((dir (file-name-as-directory
               (dorg-repo--canonical-local-path dolt-dir "Dolt directory")))
         (file (dorg-repo--canonical-local-path abs-file "Org file"))
         (relative (file-relative-name file dir)))
    (when (or (file-name-absolute-p relative)
              (file-remote-p relative)
              (member relative '("" "." "./"))
              (not (equal relative
                          (file-relative-name
                           (expand-file-name relative dir) dir))))
      (dorg-repo--binding-error
       'invalid-locator
       (format "dorg: cannot encode %s relative to Dolt store %s"
               abs-file dolt-dir)
       :dolt-dir dir :file file :relative-path relative))
    relative))

(defun dorg-repo-decode-relative-path (dolt-dir relative-path)
  "Decode persisted RELATIVE-PATH against canonical DOLT-DIR.
Signals `dorg-binding-error' when the representation is absolute, remote,
empty, dot-only, non-normal, or does not round-trip byte-for-byte."
  (let ((dir (file-name-as-directory
              (dorg-repo--canonical-local-path dolt-dir "Dolt directory"))))
    (unless (and (stringp relative-path)
                 (not (string-empty-p relative-path))
                 (not (member relative-path '("." "./")))
                 (not (file-name-absolute-p relative-path))
                 (not (file-remote-p relative-path)))
      (dorg-repo--binding-error
       'invalid-locator
       (format "dorg: malformed store-relative binding %S" relative-path)
       :dolt-dir dir :relative-path relative-path))
    (let* ((expanded (expand-file-name relative-path dir))
           (canonical (dorg--canonical-existing-case
                       (file-truename expanded)))
           (round-trip (file-relative-name canonical dir)))
      (when (or (file-directory-p canonical)
                (file-name-absolute-p round-trip)
                (not (equal relative-path round-trip)))
        (dorg-repo--binding-error
         'invalid-locator
         (format "dorg: non-round-tripping store-relative binding %S"
                 relative-path)
         :dolt-dir dir :relative-path relative-path :decoded canonical))
      canonical)))

(defun dorg-repo--relative-path-match (column dolt-dir abs-file)
  "Return SQL matching COLUMN to ABS-FILE's store-relative binding."
  (let* ((relative (dorg-repo-encode-relative-path dolt-dir abs-file))
         (quoted (dorg-sql-quote relative)))
    (if (file-name-case-insensitive-p abs-file)
        (format "LOWER(%s) = LOWER(%s)" column quoted)
      (format "%s = %s" column quoted))))

(defun dorg-repo-file-row-statement (dolt-dir root-id abs-file)
  "Return an upsert binding ABS-FILE to ROOT-ID in DOLT-DIR.
The persisted value is the canonical store-relative pathname.  On a
case-insensitive filesystem a case alias keeps the existing canonical bytes;
a genuine explicitly validated change updates the row."
  (let* ((relative (dorg-repo-encode-relative-path dolt-dir abs-file))
         (quoted (dorg-sql-quote relative)))
    (if (file-name-case-insensitive-p abs-file)
        (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (%d, %s) \
ON DUPLICATE KEY UPDATE relative_path = IF(LOWER(relative_path) = LOWER(VALUES(relative_path)), relative_path, VALUES(relative_path))"
                root-id quoted)
      (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (%d, %s) \
ON DUPLICATE KEY UPDATE relative_path = %s"
              root-id quoted quoted))))

(defun dorg-repo-root-candidates (dolt-dir abs-file &optional revision)
  "Return every stored root candidate for ABS-FILE in DOLT-DIR at REVISION.
Every persisted locator is decoded through the central store-relative law
before matching, so malformed bytes fail closed instead of looking absent.
Rows contain root-shape evidence, ordered by root id.  Omitting REVISION
preserves the live query exactly.  A historical token without `level_offset'
is rejected with a typed binding error rather than mixing schemas."
  (let* ((wanted (dorg-repo--canonical-local-path abs-file "Org file"))
         (rows
          (if (null revision)
              (dorg-sql
               dolt-dir
               "SELECT f.root_id AS root_id, f.relative_path AS relative_path, \
h.id AS heading_id, h.root_id AS heading_root_id, \
h.parent_id AS parent_id, h.level_offset AS level_offset \
FROM dorg_files f LEFT JOIN headings h ON h.id = f.root_id \
ORDER BY f.root_id ASC")
            (condition-case err
                (progn
                  (unless (and (stringp revision) (not (string-empty-p revision)))
                    (dorg-repo--binding-error
                     'invalid-revision
                     "dorg: historical root lookup requires a nonempty revision token"
                     :dolt-dir dolt-dir :file abs-file :revision revision))
                  (let ((sfx (dorg-sql-gen--as-of-suffix revision)))
                  (dorg-sql
                   dolt-dir
                   (format
                    (concat "SELECT f.root_id AS root_id, f.relative_path AS relative_path, "
                            "h.id AS heading_id, h.root_id AS heading_root_id, "
                            "h.parent_id AS parent_id, h.level_offset AS level_offset "
                            "FROM dorg_files%s f LEFT JOIN headings%s h ON h.id = f.root_id "
                            "ORDER BY f.root_id ASC")
                    sfx sfx))))
              (error
               (if (dorg-model-load--missing-offset-column-p err)
                   (dorg-repo--binding-error
                    'unsupported-revision-schema
                    (format "dorg: root lookup at %s requires level_offset" revision)
                    :dolt-dir dolt-dir :file abs-file :revision revision)
                 (signal (car err) (cdr err))))))))
    (cl-remove-if-not
     (lambda (row)
       (let ((decoded
              (dorg-repo-decode-relative-path
               dolt-dir (alist-get 'relative_path row))))
         (if (or (file-name-case-insensitive-p wanted)
                 (file-name-case-insensitive-p decoded))
             (string-equal (downcase wanted) (downcase decoded))
           (string-equal wanted decoded))))
     rows)))

(defun dorg-repo-root-id (dolt-dir abs-file &optional revision)
  "Return ABS-FILE's exact valid root in DOLT-DIR at REVISION, or nil.
Ambiguous case-equivalent bindings and malformed root shapes fail closed.
Nil REVISION retains the existing live lookup contract."
  (let ((rows (dorg-repo-root-candidates dolt-dir abs-file revision)))
    (cond
     ((null rows) nil)
     ((cdr rows)
      (dorg-repo--binding-error
       'ambiguous-locator
       (format "dorg: %s matches multiple bindings in %s" abs-file dolt-dir)
       :dolt-dir dolt-dir :file abs-file
       :root-ids (mapcar (lambda (row) (alist-get 'root_id row)) rows)))
     (t
      (let* ((row (car rows))
             (root-id (alist-get 'root_id row)))
        (unless (and (integerp root-id)
                     (> root-id 0)
                     (eql root-id (alist-get 'heading_id row))
                     (eql root-id (alist-get 'heading_root_id row))
                     (null (alist-get 'parent_id row))
                     (zerop (or (alist-get 'level_offset row) -1)))
          (dorg-repo--binding-error
           'missing-root
           (format "dorg: binding for %s names an invalid root %S"
                   abs-file root-id)
           :dolt-dir dolt-dir :file abs-file :root-id root-id))
        root-id)))))

(defun dorg-repo-file-row-count (dolt-dir)
  "Return the number of file/root bindings in DOLT-DIR."
  (or (alist-get 'n (car (dorg-sql dolt-dir
                                    "SELECT COUNT(*) AS n FROM dorg_files")))
      0))

(defun dorg-repo--root-binding-row (dolt-dir root-id)
  "Return ROOT-ID's single valid binding row in DOLT-DIR, or refuse."
  (let ((rows
         (dorg-sql
          dolt-dir
          (format (concat "SELECT f.root_id AS root_id, "
                          "f.relative_path AS relative_path, "
                          "h.id AS heading_id, "
                          "h.root_id AS heading_root_id, "
                          "h.parent_id AS parent_id, "
                          "h.level_offset AS level_offset FROM dorg_files f "
                          "LEFT JOIN headings h ON h.id = f.root_id "
                          "WHERE f.root_id = %d")
                  root-id))))
    (unless (= (length rows) 1)
      (dorg-repo--binding-error
       'missing-root
       (format "dorg: root %S has no unique file binding in %s"
               root-id dolt-dir)
       :dolt-dir dolt-dir :root-id root-id))
    (let ((row (car rows)))
      (unless (and (integerp root-id)
                   (> root-id 0)
                   (eql root-id (alist-get 'root_id row))
                   (eql root-id (alist-get 'heading_id row))
                   (eql root-id (alist-get 'heading_root_id row))
                   (null (alist-get 'parent_id row))
                   (zerop (or (alist-get 'level_offset row) -1)))
        (dorg-repo--binding-error
         'missing-root
         (format "dorg: root %S has an invalid file/root binding" root-id)
         :dolt-dir dolt-dir :root-id root-id))
      ;; Validate persisted bytes at the same boundary that returns them.
      (dorg-repo-decode-relative-path
       dolt-dir (alist-get 'relative_path row))
      row)))

(defun dorg-repo--locator-collision-root (dolt-dir abs-file root-id)
  "Return another root colliding with ABS-FILE, or nil."
  (when-let ((row
              (cl-find-if
               (lambda (candidate)
                 (not (eql root-id (alist-get 'root_id candidate))))
               (dorg-repo-root-candidates dolt-dir abs-file))))
    (alist-get 'root_id row)))

(defun dorg-repo--check-base-under-gate (dolt-dir base)
  "Return BASE after proving it is current under DOLT-DIR's mutation gate."
  (dorg-mutate-call-with-store-gate
   dolt-dir
   (lambda (lease _key)
     (let ((head (dorg-sql--lease-scalar
                  lease "SELECT HASHOF('HEAD') AS h")))
       (unless (equal head base)
         (signal 'dorg-transaction-stale
                 (list (format "planned base %s is stale; current HEAD is %s"
                               base head)
                       :base base :head head)))
       head))))

;;;###autoload
(defun dorg-rebind-file (dolt-dir root-id new-file base)
  "Rebind ROOT-ID in DOLT-DIR to NEW-FILE at exact BASE.
This administrative operation changes one `dorg_files.relative_path' value
and no domain data or filesystem bytes.  It returns a sanitized receipt plist.
An identical binding is an idempotent no-op after the same validation."
  (require 'dorg-transaction)
  (unless (and (integerp root-id) (> root-id 0))
    (dorg-repo--binding-error
     'missing-root "dorg: rebind requires an integer root id"
     :dolt-dir dolt-dir :root-id root-id))
  (unless (and (stringp base) (not (string-empty-p base)))
    (dorg-repo--binding-error
     'path-mismatch "dorg: rebind requires an exact nonempty base revision"
     :dolt-dir dolt-dir :root-id root-id :base base))
  (dorg-repo-ensure-schema dolt-dir)
  (let* ((row (dorg-repo--root-binding-row dolt-dir root-id))
         (old-relative (alist-get 'relative_path row))
         (old-file (dorg-repo-decode-relative-path dolt-dir old-relative))
         (new-relative (dorg-repo-encode-relative-path dolt-dir new-file))
         (new-canonical (dorg-repo-decode-relative-path
                         dolt-dir new-relative))
         (head (dorg-head-commit dolt-dir))
         (collision (dorg-repo--locator-collision-root
                     dolt-dir new-canonical root-id)))
    (unless (equal head base)
      (signal 'dorg-transaction-stale
              (list (format "planned base %s is stale; current HEAD is %s"
                            base head)
                    :base base :head head)))
    (when collision
      (dorg-repo--binding-error
       'locator-collision
       (format "dorg: rebind target is already owned by root %s" collision)
       :dolt-dir dolt-dir :root-id root-id :other-root-id collision
       :relative-path new-relative))
    (if (if (file-name-case-insensitive-p new-canonical)
            (string-equal (downcase old-file) (downcase new-canonical))
          (string-equal old-file new-canonical))
        (let ((checked-head
               (dorg-repo--check-base-under-gate dolt-dir base)))
          (list :ok t :root-id root-id
                :old-relative-path old-relative
                :new-relative-path old-relative
                :base base :commit checked-head :changed nil))
      (let* ((statement
              (format "UPDATE dorg_files SET relative_path = %s WHERE root_id = %d"
                      (dorg-sql-quote new-relative) root-id))
             (commit
              (dorg-transaction-commit
               dolt-dir base (list statement)
               (format "dorg: rebind root %d" root-id)
               (lambda (query)
                 (let ((rows (funcall
                              query
                              (format (concat "SELECT relative_path FROM "
                                              "dorg_files WHERE root_id = %d")
                                      root-id))))
                   (unless (and (= (length rows) 1)
                                (equal new-relative
                                       (alist-get 'relative_path (car rows))))
                     (dorg-repo--binding-error
                      'path-mismatch
                      (format "dorg: pending rebind for root %d did not match"
                              root-id)
                      :root-id root-id :relative-path new-relative)))))))
        (list :ok t :root-id root-id
              :old-relative-path old-relative
              :new-relative-path new-relative
              :base base :commit commit :changed t)))))

(defun dorg--create-root (dolt-dir abs-file)
  "Allocate a snowflake root id, insert root heading + dorg_files row.
Commits the change in DOLT-DIR for ABS-FILE.  Returns the new root id."
  (let ((new-id (dorg-id-generate)))
    (dorg-sql-batch
     dolt-dir
     (list
      (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, todo_keyword, priority, commented, title) VALUES (%d, %d, NULL, X'01', 0, NULL, NULL, FALSE, NULL)"
              new-id new-id)
      (dorg-repo-file-row-statement dolt-dir new-id abs-file)))
    (dorg-commit dolt-dir (format "dorg: register %s" abs-file))
    new-id))

(defun dorg-repo-ensure-root (dolt-dir abs-file)
  "Resolve the strict-bind root id for ABS-FILE in DOLT-DIR.
Owns the full strict-bind DDL sequence: initializes the Dolt repo when
DOLT-DIR does not yet contain one, ensures the current schema, then reuses
ABS-FILE's existing root_id when `dorg-repo-root-id' hits.  A miss allocates
the initial root only when the binding table is empty; a nonempty-store miss
refuses and requires explicit bootstrap or rebind.  Returns the root id.

This is the single backend entry through which `dorg-bind-dolt' resolves a
root; the registry performs no init/schema/root DDL itself.  DOLT-DIR and
ABS-FILE are expected already canonicalized by the caller."
  (unless (file-exists-p (expand-file-name ".dolt" dolt-dir))
    (dorg-init dolt-dir))
  (dorg-repo-ensure-schema dolt-dir)
  (or (dorg-repo-root-id dolt-dir abs-file)
      (let ((count (dorg-repo-file-row-count dolt-dir)))
        (if (zerop count)
            (dorg--create-root dolt-dir abs-file)
          (dorg-repo--binding-error
           'path-mismatch
           (format (concat "dorg: %s is not bound in nonempty store %s; "
                           "use explicit import --mode bootstrap or rebind")
                   abs-file dolt-dir)
           :dolt-dir dolt-dir :file abs-file :binding-count count)))))

;;; ─── Repository Initialization ─────────────────────────────────────

(defun dorg-init (dolt-dir &optional extensions)
  "Initialize a new dorg Dolt repository at DOLT-DIR.
Creates the directory, runs `dolt init', applies the core schema,
applies any EXTENSIONS (list of `dorg-extension' structs; if nil,
uses all currently-registered extensions), and makes an initial
commit.  Signals an error if DOLT-DIR already contains a Dolt
repo.  Uses `dorg-dolt-user-name' and `dorg-dolt-user-email' for
the repo identity."
  (dorg-dolt--ensure-available)
  (let ((expanded (expand-file-name dolt-dir)))
    (when (file-exists-p (expand-file-name ".dolt" expanded))
      (error "Dolt repo already exists at %s" expanded))
    (make-directory expanded t)
    (let ((default-directory expanded))
      ;; Initialize Dolt repo with identity.  Capture stdout/stderr so a
      ;; failure surfaces Dolt's own diagnostics, not just the exit code.
      ;; `call-process' returns a string (signal description) when the
      ;; subprocess is killed, so compare the status with `eq' rather than
      ;; `=' to avoid a type error masking the enriched diagnostic.
      (let ((buf (generate-new-buffer " *dorg-init*")))
        (unwind-protect
            (let ((exit-code (call-process "dolt" nil buf nil
                                           "init"
                                           "--name" dorg-dolt-user-name
                                           "--email" dorg-dolt-user-email)))
              (unless (eq exit-code 0)
                (error (concat "dolt init failed (exit %s) in %s\n"
                               "Command: dolt init --name %s --email %s\n"
                               "Dolt output: %s")
                       exit-code expanded
                       dorg-dolt-user-name dorg-dolt-user-email
                       (dorg-dolt--trim-output
                        (with-current-buffer buf (buffer-string))))))
          (kill-buffer buf)))
      (let ((bootstrap-started nil)
            (init-ok nil))
        (when (and (eq dorg-sql-transport 'server)
                   (not (dorg-sql--pinned-transport-for expanded)))
          (dorg-dolt-server--start-bootstrap expanded)
          (setq bootstrap-started t))
        (unwind-protect
            (progn
              ;; Apply core schema, then fresh extension schemas.  Both are
              ;; folded into the single initial commit below.  Fresh-init and
              ;; later-install share `dorg-repo--apply-extension-fresh' so their
              ;; schema application and version recording cannot diverge.
              ;; In public server mode, the bootstrap server above is started
              ;; after bare `dolt init' and before any SQL so schema and commit
              ;; use managed-server transport while the store is still
              ;; pre-schema.  The internal CLI seam remains able to build
              ;; standalone fixtures.
              (dorg-sql-batch expanded dorg-schema-ddl)
              (dorg-sql-batch
               expanded
               (list (format
                      "INSERT INTO dorg_meta (`key`, value) VALUES (%s, %s)"
                      (dorg-sql-quote dorg-repo--store-id-key)
                      (dorg-sql-quote (dorg-repo--new-store-id)))))
              (dolist (ext (or extensions (dorg-registered-extensions)))
                (dorg-repo--apply-extension-fresh expanded ext))
              ;; Commit
              (dorg-commit expanded (format "dorg: init schema v%d" dorg-schema-version))
              (setq init-ok t))
          ;; If a post-bootstrap step signalled, tear down the bootstrap
          ;; sql-server we started so a failed init leaves no orphaned
          ;; process/claim.  On success the server stays up — it now serves
          ;; the freshly-initialized repo.  When we never started a
          ;; bootstrap server (cli/pinned transport) this is a no-op.
          (when (and bootstrap-started (not init-ok))
            (dorg-dolt-server--stop expanded)))))))

(provide 'dorg-repo)
;;; dorg-repo.el ends here
