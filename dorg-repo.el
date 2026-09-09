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
(require 'subr-x)
;; `dorg--create-root' (relocated here from `dorg-registry') allocates a
;; snowflake id via `dorg-id-generate', so the backend layer requires
;; `dorg-id' directly rather than leaning on the registry's require.
(require 'dorg-id)

(autoload 'dorg-dolt-server--start-bootstrap "dorg-dolt-server")
(autoload 'dorg-dolt-server--stop "dorg-dolt-server")
(autoload 'dorg-transaction-commit "dorg-transaction")
(declare-function dorg-mutate-call-with-store-gate "dorg-mutate")

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
;; migration step is an actionable error: the extension (or operator)
;; must supply a step — or set `ext:<name>:version' manually as an
;; escape hatch — before dorg will use that extension at a newer version.

(defun dorg-repo--ext-version-key (name)
  "Return the `dorg_meta' key string tracking extension NAME's version."
  (format "ext:%s:version" (symbol-name name)))

(defun dorg-repo--ext-version-update-sql (name version)
  "Return SQL that records extension NAME at VERSION in `dorg_meta'.
Dorg, not the extension, owns this metadata update; it is appended to
the same batch as the schema/migration statements so a failure before
it runs leaves the recorded version unchanged."
  (format (concat "INSERT INTO dorg_meta (`key`, value) VALUES (%s, '%d') "
                  "ON DUPLICATE KEY UPDATE value = '%d'")
          (dorg-sql-quote (dorg-repo--ext-version-key name))
          version version))

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

(defun dorg-repo--ext-resolve-step (dolt-dir ext from-version step)
  "Resolve STEP migrating EXT from FROM-VERSION to a list of SQL strings.
STEP is SQL (string or list of strings) or a function of one argument
\(DOLT-DIR) returning SQL.  Signals an actionable error if STEP names a
function that is not defined."
  (let ((sql (cond
              ((functionp step) (funcall step dolt-dir))
              ((and (symbolp step) step)
               (error (concat "dorg: migration step for %s v%d -> v%d names "
                              "undefined function %S")
                      (dorg-extension-name ext) from-version (1+ from-version)
                      step))
              (t step))))
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
init commit.  `dorg-repo--install-extension' applies the same statements
inside a committed transaction instead."
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

(defun dorg-repo--commit-schema-step (dolt-dir statements commit-message)
  "Run STATEMENTS as one committed schema lifecycle step in DOLT-DIR.
Used for both core schema migrations and extension install/migration
steps so their transaction, preflight, reset, and error-resignal
behavior cannot drift.  COMMIT-MESSAGE both labels the Dolt commit and
names the operation in the clean-working-set preflight error.

STATEMENTS must end with the `dorg_meta' version bump (core
`schema_version' or `ext:<name>:version').  The batch runs through
`dorg-sql-batch's transaction path, which rolls back DML and the version
bump on failure so the recorded version never advances past a failed
step.

Before running anything, this requires a clean Dolt working set via
`dorg-repo--ensure-clean-working-set'; a dirty repo is rejected before
any schema or data is touched.  That precondition is what makes the
failure reset below safe: with a clean entry, the only working-set drift
the reset can discard is this step's own partial work, never unrelated
pre-existing uncommitted changes.

Dolt follows MySQL implicit-commit-on-DDL semantics: an ALTER/CREATE that
succeeds before a later statement fails stays in the working set even
after the transaction rolls back.  To keep a failed step truly atomic —
no partial schema drift that would break a re-run with a duplicate-DDL
error — this discards any working-set changes with `DOLT_RESET --hard'
and then re-signals the original error."
  (dorg-repo--ensure-clean-working-set dolt-dir commit-message)
  ;; Pin one transport for the batch AND its failure reset so the
  ;; `DOLT_RESET' runs on the same transport the batch selected rather than
  ;; re-resolving `dorg-sql-transport' independently.
  (dorg-sql--with-pinned-transport dolt-dir
    (condition-case err
        (dorg-sql-batch dolt-dir statements commit-message)
      (error
       (ignore-errors (dorg-sql dolt-dir "CALL DOLT_RESET('--hard')"))
       (signal (car err) (cdr err))))))

(defun dorg-repo--install-extension (dolt-dir ext)
  "Install EXT's fresh schema into DOLT-DIR as one committed transaction.
The schema statements and the `dorg_meta' version bump share the
transaction/commit path, and a mid-install failure is rolled back to HEAD
so no partial schema or version remains."
  (dorg-repo--commit-schema-step
   dolt-dir
   (dorg-repo--ext-fresh-statements ext)
   (format "dorg: install extension %s v%d"
           (dorg-extension-name ext)
           (dorg-extension-version ext))))

(defun dorg-repo--migrate-extension-step (dolt-dir ext from-version)
  "Run EXT's migration step from FROM-VERSION to FROM-VERSION+1 in DOLT-DIR.
The migration SQL and the `dorg_meta' version bump run as one committed
step via `dorg-repo--commit-schema-step', so a mid-step failure rolls the
repo back to HEAD and leaves the recorded version unchanged.  Signals an
actionable error if no step is registered."
  (let* ((name (dorg-extension-name ext))
         (to (1+ from-version))
         (step (alist-get from-version (dorg-extension-migrations ext))))
    (unless step
      (error (concat "dorg: no migration for extension %s v%d -> v%d; supply a "
                     "migration step or update %s manually")
             name from-version to (dorg-repo--ext-version-key name)))
    (dorg-repo--commit-schema-step
     dolt-dir
     (append (dorg-repo--ext-resolve-step dolt-dir ext from-version step)
             (list (dorg-repo--ext-version-update-sql name to)))
     (format "dorg: migrate extension %s v%d -> v%d" name from-version to))))

(defun dorg-repo-ensure-extensions (dolt-dir &optional extensions)
  "Install or migrate EXTENSIONS in DOLT-DIR to their registered versions.
EXTENSIONS is a list of `dorg-extension' structs; when nil, all
currently-registered extensions are used.  For each extension dorg reads
`ext:<name>:version' from `dorg_meta' and:

- absent    -> installs the fresh schema and records the version
  (lets a repo created before an extension was enabled acquire it later);
- equal     -> no-op (fresh schema is not reapplied);
- lower     -> runs one registered migration step per version until current,
  committing each step;
- higher    -> signals a future-version error (dorg never downgrades);
- missing step -> signals an actionable error, leaving the repo unchanged.

Metadata for unregistered extensions is left untouched; dorg does not drop
extension tables or require every extension to be loaded for every repo.

Any step that installs or migrates an extension requires a clean Dolt
working set (see `dorg-repo--commit-schema-step'): a dirty repo is rejected
before any schema or data is touched, so the failure-path
`DOLT_RESET --hard' can never discard unrelated uncommitted changes.  A
pure no-op ensure (every extension already at its registered version)
does not touch the working set and so does not require cleanliness."
  (dolist (ext (or extensions (dorg-registered-extensions)))
    (let* ((name (dorg-extension-name ext))
           (target (dorg-extension-version ext))
           (current (dorg-repo--ext-db-version dolt-dir name)))
      (cond
       ((null current)
        (dorg-repo--install-extension dolt-dir ext))
       ((= current target) nil)
       ((> current target)
        (error (concat "dorg: repo at %s has extension %s v%d but this dorg "
                       "only supports v%d")
               dolt-dir name current target))
       (t
        (while (< current target)
          (dorg-repo--migrate-extension-step dolt-dir ext current)
          (setq current (1+ current))))))))

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
  (let ((db-version (dorg-repo--db-schema-version dolt-dir)))
    (cond
     ((> db-version dorg-schema-version)
      (signal 'dorg-schema-repo-newer
              (list (format
                     (concat "dorg: repo at %s is schema v%d but this dorg "
                             "only supports %s; upgrade this dorg installation "
                             "(a vendored lib/dorg copy must be updated and its "
                             "CLI rebuilt) or open a repo of schema v%d or lower")
                     dolt-dir db-version
                     (dorg-schema--source-location)
                     dorg-schema-version))))
     ((< db-version dorg-schema-version) :needs-migration)
     (t :current))))

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
  "Resolve core MIGRATION to SQL for DOLT-DIR."
  (cond
   ((functionp migration) (funcall migration dolt-dir))
   ((and (symbolp migration) (fboundp migration))
    (funcall migration dolt-dir))
   ((symbolp migration)
    (error "dorg: core migration names undefined function %S" migration))
   (t migration)))

(defun dorg-repo-ensure-schema (dolt-dir &optional extensions)
  "Migrate DOLT-DIR to the current core schema, then ensure EXTENSIONS.
First runs the read-only `dorg-repo-check-schema-compat' preflight: a
repo NEWER than this dorg signals `dorg-schema-repo-newer' before any
write is attempted.

When the repo is OLDER, migration is gated on `dorg-schema-auto-migrate':
if that is nil the repo is left untouched and `dorg-schema-migration-
required' is signaled (the opt-in path for shared deployments — migrate
explicitly with `dorg-migrate-schema'); if non-nil a `display-warning'
announces the one-way upgrade before any SQL runs, so it is never silent.

Applies core migrations from `dorg-schema-migrations' in order, running
each step through `dorg-repo--commit-schema-step' so the migration SQL
and the `schema_version' bump share one committed transaction.  A
mid-step failure hard-resets the working set and re-signals, leaving no
partial schema drift and `schema_version' unchanged; a dirty working set
is rejected before any migration SQL runs.  Core migrations run first so
extension migrations may assume the current core schema.  After the core
schema is current, registered extensions (or the explicit EXTENSIONS
list, when non-nil) are installed/upgraded via
`dorg-repo-ensure-extensions'.  No-op when nothing needs migrating; a
no-op core schema does not require a clean working set."
  (when (eq (dorg-repo-check-schema-compat dolt-dir) :needs-migration)
    (let ((db-version (dorg-repo--db-schema-version dolt-dir)))
      (when (< db-version 6)
        (signal 'dorg-schema-migration-required
                (list (format
                       (concat "dorg: repo at %s uses pre-release schema v%d; "
                               "there is no supported upgrade path to %s")
                       dolt-dir db-version (dorg-schema--source-location))
                      :from db-version :to dorg-schema-version)))
      (unless dorg-schema-auto-migrate
        (signal 'dorg-schema-migration-required
                (list (format
                       (concat "dorg: repo at %s needs a schema migration to %s "
                               "but `dorg-schema-auto-migrate' is nil; run "
                               "`M-x dorg-migrate-schema' (or set the variable) "
                               "once every co-installed dorg is current")
                       dolt-dir (dorg-schema--source-location)))))
      (dorg-repo--announce-migration dolt-dir db-version dorg-schema-version)
      (while (< db-version dorg-schema-version)
        (let ((migration (alist-get db-version dorg-schema-migrations)))
          (unless migration
            (error "dorg: no migration defined for v%d → v%d"
                   db-version (1+ db-version)))
          (dorg-repo--commit-schema-step
           dolt-dir (dorg-repo--resolve-core-migration-step dolt-dir migration)
           (format "dorg: migrate schema v%d → v%d"
                   db-version (1+ db-version)))
          (setq db-version (1+ db-version))))))
  (dorg-repo-ensure-extensions dolt-dir extensions))

;;;###autoload
(defun dorg-migrate-schema (dolt-dir)
  "Explicitly migrate DOLT-DIR to this dorg's current core schema.
The opt-in counterpart to `dorg-schema-auto-migrate': forces the
migration even when that variable is nil, so a shared or CLI deployment
can keep auto-migration disabled yet still upgrade the repo deliberately
once every co-installed dorg is current.  A repo NEWER than this dorg still
signals `dorg-schema-repo-newer' — migration never downgrades."
  (interactive "DDolt directory to migrate: ")
  (let ((dorg-schema-auto-migrate t))
    (dorg-repo-ensure-schema dolt-dir))
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

(defun dorg-repo-root-candidates (dolt-dir abs-file)
  "Return every stored root candidate for ABS-FILE in DOLT-DIR.
Every persisted locator is decoded through the central store-relative law
before matching, so malformed bytes fail closed instead of looking absent.
Rows contain root-shape evidence, ordered by root id."
  (let* ((wanted (dorg-repo--canonical-local-path abs-file "Org file"))
         (rows
          (dorg-sql
           dolt-dir
           "SELECT f.root_id AS root_id, f.relative_path AS relative_path, \
h.id AS heading_id, h.root_id AS heading_root_id, \
h.parent_id AS parent_id, h.level AS level \
FROM dorg_files f LEFT JOIN headings h ON h.id = f.root_id \
ORDER BY f.root_id ASC")))
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

(defun dorg-repo-root-id (dolt-dir abs-file)
  "Return ABS-FILE's exact valid root in DOLT-DIR, or nil when unbound.
Ambiguous case-equivalent bindings and malformed root shapes fail closed."
  (let ((rows (dorg-repo-root-candidates dolt-dir abs-file)))
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
                     (zerop (or (alist-get 'level row) -1)))
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
                          "h.level AS level FROM dorg_files f "
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
                   (zerop (or (alist-get 'level row) -1)))
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
      (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level, todo_keyword, priority, commented, title) VALUES (%d, %d, NULL, X'01', 0, NULL, NULL, FALSE, NULL)"
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
