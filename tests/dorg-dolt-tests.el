;;; dorg-dolt-tests.el --- Tests for dorg-schema and dorg-dolt -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dorg Dolt CLI interface and schema.
;; Tests that require dolt are skipped when the binary is not available.

;;; Code:

(require 'ert)
(require 'dorg-schema)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-registry)
(require 'dorg-render)
(require 'dorg-envelope)
(require 'dorg-test-helpers)
(require 'json)

;;; Helpers

(defvar dorg-test--dolt-available (dorg-dolt-supported-p)
  "Non-nil if a supported Dolt version is available for integration tests.
Gated on the `dorg-dolt-minimum-version' floor, not merely PATH
presence, so hosts with an old Dolt skip rather than fail noisily.")

(defmacro dorg-test-with-dolt (&rest body)
  "Execute BODY with a temporary Dolt repo.
Binds `dolt-dir' to the initialized repo path.
Skips if dolt is not installed or unsupported."
  (declare (indent 0))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed or unsupported")
     (dorg-test--with-temp-db dolt-dir
       ,@body)))

(defmacro dorg-test-with-clean-extensions (&rest body)
  "Execute BODY with `dorg--extensions' let-bound to nil."
  (declare (indent 0))
  `(let ((dorg--extensions nil))
     ,@body))

(defconst dorg-test-mxwnw--transport-entry-allowlist
  '(".doltcfg" ".dorg" "config.yaml")
  "Store-local entries transport may create while attaching a reader.
`.doltcfg' and `config.yaml' are Dolt sql-server artifacts; `.dorg' holds
DOrg's server lifecycle claim.  They are not repository initialization.")

(defun dorg-test-mxwnw--existing-store-snapshot (dolt-dir)
  "Return the observable existing-store state around a safe-reader bind.
The directory comparison deliberately excludes only the documented transport
allowlist; table and status observations remain exact."
  (let (entries)
    (dolist (entry (directory-files dolt-dir nil))
      (unless (or (member entry '("." ".."))
                  (member entry dorg-test-mxwnw--transport-entry-allowlist))
        (push entry entries)))
    (list :head (dorg-head-commit dolt-dir)
          :status (dorg-sql dolt-dir
                            "SELECT * FROM dolt_status ORDER BY table_name")
          :counts
          (mapcar
           (lambda (table)
             (cons table
                   (alist-get 'n
                              (car (dorg-sql dolt-dir
                                             (format "SELECT COUNT(*) AS n FROM %s"
                                                     table))))))
           '("dorg_files" "headings" "dorg_meta"))
          :entries (sort entries #'string<))))

(ert-deftest dorg-test-bind-existing-dolt-preserves-existing-store-state ()
  "Safe existing-store binding reuses a root and refuses an unbound sibling.
Neither outcome may create a root or commit after the strict setup bind."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-mxwnw-" t))
           (file (expand-file-name "doc.org" root))
           (other (expand-file-name "other.org" root)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Existing\n"))
            (with-temp-file other (insert "* Unbound\n"))
            (let ((dorg--registry nil))
              (let* ((strict (dorg-bind-dolt file dolt-dir))
                     (root-id (dorg-backend-root-id
                               (dorg-registration-backend strict)))
                     (snapshot
                      (dorg-test-mxwnw--existing-store-snapshot dolt-dir)))
                (should-not (plist-get snapshot :status))
                (setq dorg--registry nil)
                (let ((safe (dorg-bind-existing-dolt file dolt-dir)))
                  (should (= root-id
                             (dorg-backend-root-id
                              (dorg-registration-backend safe))))
                  (should (equal (dorg-registration-backend strict)
                                 (dorg-registration-backend safe)))
                  (should (equal (file-truename file)
                                 (dorg-registration-file safe)))
                  (should (assoc (file-truename file) dorg--registry)))
                (should (equal snapshot
                               (dorg-test-mxwnw--existing-store-snapshot
                                dolt-dir)))
                (let ((err (should-error
                            (dorg-bind-existing-dolt other dolt-dir)
                            :type 'dorg-binding-error)))
                  (should (eq 'path-mismatch (dorg-binding-error-reason err)))
                  (should (equal snapshot
                                 (dorg-test-mxwnw--existing-store-snapshot
                                  dolt-dir))))))
        (delete-directory root t))))))

(ert-deftest dorg-test-bind-existing-dolt-empty-store-miss-preserves-state ()
  "An empty initialized store refuses an unbound safe-reader bind without root creation."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-mxwnw-empty-" t))
           (file (expand-file-name "doc.org" root)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Unbound\n"))
            (let ((dorg--registry nil)
                  (snapshot (dorg-test-mxwnw--existing-store-snapshot dolt-dir)))
              (should-not (plist-get snapshot :status))
              (should (zerop (cdr (assoc "dorg_files"
                                         (plist-get snapshot :counts)))))
              (let ((err (should-error (dorg-bind-existing-dolt file dolt-dir)
                                       :type 'dorg-binding-error)))
                (should (eq 'path-mismatch (dorg-binding-error-reason err)))
                (should (equal snapshot
                               (dorg-test-mxwnw--existing-store-snapshot
                                dolt-dir)))
                (should-not dorg--registry))))
        (delete-directory root t)))))

(defvar dorg-test-bx6op--receipt-schema-reads nil
  "Receipt metadata reads made by the explicit-migration fixtures.")

(defun dorg-test-bx6op--receipt-schema-rows (query)
  "Return concrete metadata for an existing compatible receipt table.
Only the leased SQL transport is doubled; production validates these facts."
  (cond
   ((equal query
           (concat "SELECT table_name FROM information_schema.tables"
                   " WHERE table_schema = DATABASE()"
                   " AND table_name = 'dorg_mutation_log'"))
    (push 'tables dorg-test-bx6op--receipt-schema-reads)
    '(((TABLE_NAME . "dorg_mutation_log"))))
   ((equal query
           (concat "SELECT column_name AS column_name,"
                   " column_type AS column_type, is_nullable AS is_nullable"
                   " FROM information_schema.columns"
                   " WHERE table_schema = DATABASE()"
                   " AND table_name = 'dorg_mutation_log'"
                   " ORDER BY ordinal_position"))
    (push 'columns dorg-test-bx6op--receipt-schema-reads)
    '(((COLUMN_NAME . "op_id")
       (COLUMN_TYPE . "varchar(255)") (IS_NULLABLE . "NO"))
      ((COLUMN_NAME . "gen_id")
       (COLUMN_TYPE . "varchar(64)") (IS_NULLABLE . "NO"))
      ((COLUMN_NAME . "intent_digest")
       (COLUMN_TYPE . "varchar(64)") (IS_NULLABLE . "NO"))
      ((COLUMN_NAME . "ts")
       (COLUMN_TYPE . "bigint") (IS_NULLABLE . "NO"))
      ((COLUMN_NAME . "postimages_v1")
       (COLUMN_TYPE . "text") (IS_NULLABLE . "YES"))))
   (t (ert-fail (format "unexpected receipt metadata SQL: %s" query)))))

;;; Schema Tests

(ert-deftest dorg-test-schema-constant-exists ()
  "Schema DDL is a non-empty list of SQL statements."
  (should (listp dorg-schema-ddl))
  (should (> (length dorg-schema-ddl) 5))
  (should (cl-every #'stringp dorg-schema-ddl)))

(ert-deftest dorg-test-schema-version-defined ()
  "Schema version is defined and current."
  (should (= dorg-schema-version 8)))

(ert-deftest dorg-test-dzx00-fresh-schema-owns-receipt-storage ()
  "Fresh core schema declares the complete exactly-once receipt shape.

Receipt storage is versioned core schema rather than operational metadata.  A
fresh init must therefore receive the table and postimage column from
`dorg-schema-ddl', before any ordinary apply/replay path runs."
  (let ((ddl (mapconcat #'identity dorg-schema-ddl "\n")))
    (should (string-match-p "CREATE TABLE dorg_mutation_log" ddl))
    (should (string-match-p "postimages_v1[[:space:]]+TEXT" ddl))))

(ert-deftest dorg-test-dzx00-ordinary-receipt-check-is-read-only ()
  "Missing receipt storage is a typed ordinary refusal with SELECT-only SQL."
  (let (seen)
    (cl-letf (((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease sql &rest _)
                 (push sql seen)
                 nil)))
      (let ((err (should-error
                  (dorg-sql--require-mutation-log-schema 'lease)
                  :type 'dorg-schema-migration-required)))
        (should (eq 'schema-migration-required
                    (plist-get (dorg-error-data err) :reason)))))
    (should (= 2 (length seen)))
    (should (cl-every (lambda (sql) (string-match-p "\\`SELECT\\_>" sql)) seen))
    (should-not (cl-some (lambda (sql)
                           (string-match-p "\\`[[:space:]]*\\(CREATE\\|ALTER\\|DROP\\|INSERT\\|UPDATE\\|DELETE\\)\\_>" sql))
                         seen))))

(ert-deftest dorg-test-dzx00-receipt-status-handles-client-column-labels ()
  "Receipt status accepts the uppercase labels emitted by the SQL client.

The query aliases its information-schema fields to the lowercase keys DOrg
uses.  The uppercase rows retain the exact shape from the failed owned-Dolt
receipt, so this remains a witness if a client ignores those aliases."
  (let (seen)
    (should
     (eq 'compatible
         (dorg-sql--mutation-log-schema-status
          (lambda (sql)
            (push sql seen)
            (if (string-match-p "information_schema.tables" sql)
                '(((TABLE_NAME . "dorg_mutation_log")))
              '(((COLUMN_NAME . "op_id")
                 (COLUMN_TYPE . "varchar(255)") (IS_NULLABLE . "NO"))
                ((COLUMN_NAME . "gen_id")
                 (COLUMN_TYPE . "varchar(64)") (IS_NULLABLE . "NO"))
                ((COLUMN_NAME . "intent_digest")
                 (COLUMN_TYPE . "varchar(64)") (IS_NULLABLE . "NO"))
                ((COLUMN_NAME . "ts")
                 (COLUMN_TYPE . "bigint") (IS_NULLABLE . "NO"))
                ((COLUMN_NAME . "postimages_v1")
                 (COLUMN_TYPE . "text") (IS_NULLABLE . "YES"))))))))
    (let ((column-query
           (cl-find-if (lambda (sql)
                         (string-match-p "information_schema.columns" sql))
                       seen)))
      (should column-query)
      (should (string-match-p
               (concat "column_name AS column_name, column_type AS column_type,"
                       " is_nullable AS is_nullable")
               column-query)))))

(ert-deftest dorg-test-dzx00-v7-corrupt-receipt-refuses-without-migration-remedy ()
  "A current-schema store with damaged receipt storage reports integrity, not migration."
  (let (seen)
    (cl-letf (((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease sql &rest _)
                 (push sql seen)
                 (cond ((string-match-p "information_schema.tables" sql) nil)
                       ((string-match-p "schema_version" sql)
                        (list (list (cons 'value
                                          (number-to-string dorg-schema-version)))))))))
      (let ((err (should-error
                  (dorg-sql--require-mutation-log-schema 'lease)
                  :type 'dorg-schema-migration-required)))
        (should (eq 'receipt-schema-incompatible
                    (plist-get (dorg-error-data err) :reason)))
        (should-not (string-match-p "dorg-migrate-schema"
                                    (error-message-string err)))))
    (should-not (cl-some (lambda (sql)
                           (string-match-p "\\`[[:space:]]*\\(CREATE\\|ALTER\\|DROP\\|INSERT\\|UPDATE\\|DELETE\\)\\_>" sql))
                         seen))))

(ert-deftest dorg-test-dzx00-v6-receipt-resolver-only-upgrades-known-shapes ()
  "The v6 resolver creates/adds only known receipt shapes and preserves others."
  (cl-labels
      ((resolve (tables columns)
         (cl-letf (((symbol-function 'dorg-repo-migration-query)
                    (lambda (_dir sql &rest _)
                      (if (string-match-p "information_schema.tables" sql)
                          tables
                        columns))))
           (dorg-repo--v6-to-v7-receipt-schema "/tmp/dzx00"))))
    (should (equal (list dorg-schema-mutation-log-ddl) (resolve nil nil)))
    (should (equal '("ALTER TABLE dorg_mutation_log ADD COLUMN postimages_v1 TEXT NULL")
                   (resolve '((table_name . "dorg_mutation_log"))
                            '(((column_name . "op_id") (column_type . "varchar(255)") (is_nullable . "NO"))
                              ((column_name . "gen_id") (column_type . "varchar(64)") (is_nullable . "NO"))
                              ((column_name . "intent_digest") (column_type . "varchar(64)") (is_nullable . "NO"))
                              ((column_name . "ts") (column_type . "bigint") (is_nullable . "NO"))))))
    (should-error
     (resolve '((table_name . "dorg_mutation_log"))
              '(((column_name . "wrong") (column_type . "text") (is_nullable . "YES"))))
     :type 'dorg-schema-migration-required)))

(ert-deftest dorg-test-dolt-binary-secondary-index-fk-update-keeps-connection-live ()
  "A headings UPDATE through the binary-index FK path keeps the server alive.
This is the observed shape of Dolt issue #10903 / PR #10906: before Dolt
1.86.4, the secondary-index no-op check compared an interface holding a byte
slice with `!=', panicking the server.  Success and the exact postimage are
portable witnesses; the client-side timeout/closed-connection text is not."
  (dorg-test-with-dolt
    (unless dorg-test--server-port
      (ert-skip "requires the shared live sql-server"))
    (dorg-sql
     dolt-dir
     (concat
      "INSERT INTO headings "
      "(id, root_id, parent_id, sort_key, level_offset, commented, title, "
      "post_blank, items_hash) VALUES "
      "(100, 100, NULL, X'0000', 0, FALSE, NULL, 0, NULL), "
      "(101, 100, 100, X'0001', 1, FALSE, 'before', 0, NULL)"))
    (let ((connection (dorg-sql--get-connection dolt-dir)))
      (dorg-sql
       dolt-dir
       (concat
        "UPDATE headings SET parent_id = 100, sort_key = X'0001', "
        "level_offset = 1, todo_keyword = NULL, priority = NULL, "
        "commented = FALSE, title = 'after', post_blank = 0, "
        "items_hash = '7015d21b5ae3ba277cc23462a9359d76' WHERE id = 101"))
      (should
       (equal (dorg-sql
               dolt-dir
               (concat
                "SELECT parent_id, HEX(sort_key) AS sort_key, level_offset, title, "
                "items_hash FROM headings WHERE id = 101"))
              '(((parent_id . 100)
                 (sort_key . "0001")
                 (level_offset . 1)
                 (title . "after")
                 (items_hash . "7015d21b5ae3ba277cc23462a9359d76")))))
      ;; A reconnect would conceal the server dropping the updating session.
      (should (eq connection (dorg-sql--get-connection dolt-dir)))
      (should (equal (dorg-sql dolt-dir "SELECT 1 AS alive")
                     '(((alive . 1))))))))

(ert-deftest dorg-test-release-hygiene-retires-complete-pre-v6-ledger ()
  "The v7 release retains only its explicit v6 receipt migration.
Every legacy
v4/v5 definition/reference is absent from production source and runtime, and
the generic explicit migration entry has no retired root-map parameter.  The
preimage contains every survivor, making this deletion test non-vacuous."
  (let* ((root (file-name-as-directory
                (symbol-value 'dorg-run-tests--project-root)))
         (files '("dorg-repo.el" "dorg-schema.el"))
         (names '("dorg-schema--conflicts-table"
                  "dorg-schema-legacy-conflicts-present"
                  "dorg-repo--legacy-open-conflict-count"
                  "dorg-repo--explicit-binding-map"
                  "dorg-repo--explicit-core-migration"
                  "dorg-repo--legacy-file-bindings"
                  "dorg-repo--binding-map-path"
                  "dorg-repo--migration-relative-bindings"
                  "dorg-repo--v5-to-v6-statements"
                  "dorg-repo--preflight-core-migration-step"))
         (sources
          (mapcar
           (lambda (file)
             (cons file
                   (with-temp-buffer
                     (insert-file-contents (expand-file-name file root))
                     (buffer-string))))
           files))
         (doc-files
          '("README.org" "CHANGELOG.org" "DEVELOPMENT.org"
            "docs/api.org" "docs/configuration.org" "docs/schema.org"))
         obsolete-tests
         survivors)
    (dolist (name names)
      (when (cl-some (lambda (source)
                       (string-search name (cdr source)))
                     sources)
        (push name survivors)))
    (unless (equal dorg-schema-migrations
                   '((6 . dorg-repo--v6-to-v7-receipt-schema)
                     (7 . dorg-repo--v7-to-v8-level-offset)))
      (push (format "migration-ledger:%S" dorg-schema-migrations) survivors))
    (dolist (symbol '(dorg-schema--conflicts-table
                      dorg-repo--explicit-binding-map
                      dorg-repo--explicit-core-migration))
      (when (boundp symbol)
        (push (format "bound:%s" symbol) survivors)))
    (dolist (symbol '(dorg-repo--legacy-open-conflict-count
                      dorg-repo--legacy-file-bindings
                      dorg-repo--binding-map-path
                      dorg-repo--migration-relative-bindings
                      dorg-repo--v5-to-v6-statements
                      dorg-repo--preflight-core-migration-step))
      (when (fboundp symbol)
        (push (format "fbound:%s" symbol) survivors)))
    (dolist (symbol '(dorg-test--downgrade-file-binding-table-to-v5
                      dorg-test--install-legacy-v4-conflict-store))
      (when (fboundp symbol)
        (push (format "obsolete-test:%s" symbol) obsolete-tests)))
    (dolist (symbol
             '(dorg-test-ttp2t-nonempty-v5-needs-explicit-atomic-migration
               dorg-test-ttp2t-v5-migration-preflight-is-all-or-nothing
               dorg-test-ttp2t-v5-migration-reverts-to-byte-equivalent-table
               dorg-test-ttp2t-empty-v5-store-auto-migrates-structurally
               dorg-test-schema-v5-open-legacy-conflicts-refuse-atomically
               dorg-test-schema-v6-resolved-legacy-conflicts-retire
               dorg-test-ensure-schema-migrates-v1-to-current))
      (when (ert-test-boundp symbol)
        (push (format "obsolete-test:%s" symbol) obsolete-tests)))
    (let ((case-fold-search t)
          (legacy-re
           "\\(?:schema[- ]v[1-5]\\|v4[ 	]*→[ 	]*v5\\|v5[ 	]*→[ 	]*v6\\)"))
      (dolist (file doc-files)
        (when (string-match-p
               legacy-re
               (with-temp-buffer
                 (insert-file-contents (expand-file-name file root))
                 (buffer-string)))
          (push (format "legacy-doc:%s" file) survivors))))
    (when (get 'dorg-schema-legacy-conflicts-present 'error-conditions)
      (push "error:dorg-schema-legacy-conflicts-present" survivors))
    (unless (equal (help-function-arglist 'dorg-migrate-schema t)
                   '(dolt-dir))
      (push (format "dorg-migrate-schema-arglist:%S"
                    (help-function-arglist 'dorg-migrate-schema t))
            survivors))
    (ert-info ((format "obsolete test survivors: %S"
                       (nreverse (copy-sequence obsolete-tests))))
      (should-not obsolete-tests))
    (should-not survivors)))

(ert-deftest dorg-test-release-hygiene-pre-v6-store-refuses-before-write ()
  "A pre-release schema below v6 has no supported upgrade path and writes nil.
The typed refusal occurs before announcement, migration commit, or extension
ensure.  On the current preimage a v5 store follows the live migration ledger,
so the expected refusal is absent and this test is RED."
  (let ((dorg-schema-auto-migrate t)
        calls)
    (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
               (lambda (_dir _query &rest args)
                 (let ((key (car args)))
                   (cond
                    ((and key (string-search "schema_version" key))
                     '(((value . "5"))))
                    (t nil)))))
              ((symbol-function 'dorg-repo--announce-migration)
               (lambda (&rest _) (push 'announce calls)))
              ((symbol-function 'dorg-repo--resolve-core-migration-step)
               (lambda (&rest _) (push 'resolver calls)))
              ((symbol-function 'dorg-repo--commit-schema-step)
               (lambda (&rest _) (push 'commit calls)))
              ((symbol-function 'dorg-repo-ensure-extensions)
               (lambda (&rest _) (push 'extensions calls))))
      (let ((err (should-error
                  (dorg-repo-ensure-schema "/pre-v6")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "core:pre-v6" (error-message-string err)))
        (should (string-match-p "M-x dorg-migrate-schema"
                                (error-message-string err)))))
    (should-not calls)))

(ert-deftest dorg-test-bx6op-ordinary-core-migration-refuses-before-write ()
  "Ordinary open refuses an older existing core schema without auto-migrating.
The old `dorg-schema-auto-migrate' escape hatch must not reach announcement,
resolver, schema-step, extension ensure, or any write path."
  (let ((dorg-schema-version 7)
        (dorg-schema-auto-migrate t)
        (dorg-schema-migrations '((6 . "ALTER TABLE headings ADD COLUMN bx6op INT")))
        calls)
    (cl-letf (((symbol-function 'dorg-repo--db-schema-version) (lambda (_) 6))
              ((symbol-function 'dorg-repo--ordinary-read)
               (lambda (_dir query &rest args)
                 (let ((key (car args)))
                   (cond
                    ((and key (string-search "schema_version" key))
                     '(((value . "6"))))
                    (t (error "unexpected ordinary read: %s %S" query args))))))
              ((symbol-function 'dorg-repo--announce-migration)
               (lambda (&rest _) (push 'announce calls)))
              ((symbol-function 'dorg-repo--resolve-core-migration-step)
               (lambda (&rest _) (push 'resolver calls)))
              ((symbol-function 'dorg-repo--commit-schema-step)
               (lambda (&rest _) (push 'commit calls)))
              ((symbol-function 'dorg-repo-ensure-extensions)
               (lambda (&rest _) (push 'extensions calls))))
      (let ((err (should-error
                  (dorg-repo-ensure-schema "/tmp/bx6op-store")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "M-x dorg-migrate-schema"
                                (error-message-string err)))
        (should-not (string-match-p "dorg-schema-auto-migrate"
                                    (error-message-string err)))
        (should-not (string-match-p "ext:.*:version"
                                    (error-message-string err)))))
    (should-not calls)))

(ert-deftest dorg-test-bx6op-ordinary-absent-extension-refuses-before-install ()
  "Ordinary open refuses a missing registered extension row without installing it."
  (let ((calls nil))
    (dorg-test-with-clean-extensions
      (dorg-register-extension
       'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
      (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
                 (lambda (_dir query &rest args)
                   (let ((key (car args)))
                     (cond
                      ((and key (string-search "schema_version" key))
                       '(((value . "6"))))
                      ((and key (string-search "ext:demo:version" key))
                       nil)
                      (t (error "unexpected ordinary read: %s %S" query args))))))
                ((symbol-function 'dorg-repo--install-extension)
                 (lambda (&rest _) (push 'install calls)))
                ((symbol-function 'dorg-repo--migrate-extension-step)
                 (lambda (&rest _) (push 'migrate calls))))
        (let ((err (should-error
                    (dorg-repo-ensure-schema "/tmp/bx6op-store")
                    :type 'dorg-schema-migration-required)))
          (should (string-match-p "demo" (error-message-string err)))
          (should (string-match-p "absent" (error-message-string err)))
          (should-not calls))))))

(ert-deftest dorg-test-bx6op-ordinary-transport-error-propagates ()
  "Ordinary open reports transport failures as transport failures, not schema gaps."
  (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
             (lambda (&rest _)
               (signal 'dorg-sql-transport-error
                       '("server unavailable")))))
    (should-error (dorg-repo-ensure-schema "/tmp/bx6op-store")
                  :type 'dorg-sql-transport-error)))

(ert-deftest dorg-test-bx6op-legacy-extension-ensure-is-retired ()
  "The old public extension installer is not a second existing-store apply path."
  (dorg-test-with-clean-extensions
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (cl-letf (((symbol-function 'dorg-repo--ext-db-version)
               (lambda (&rest _) nil))
              ((symbol-function 'dorg-repo--install-extension)
               (lambda (&rest _)
                 (ert-fail "retired installer must not install directly")))
              ((symbol-function 'dorg-repo--migrate-extension-step)
               (lambda (&rest _)
                 (ert-fail "retired installer must not migrate directly"))))
      (let ((err (should-error
                  (dorg-repo-ensure-extensions "/tmp/bx6op-store")
                  :type 'dorg-schema-migration-required)))
        (should (eq 'explicit-migration-required (dorg-error-reason err)))
        (should (string-match-p "dorg-migrate-schema"
                                (error-message-string err)))))))

(ert-deftest dorg-test-bx6op-explicit-migration-uses-one-lease-batch-per-step ()
  "The operator migration path applies each needed step under the real gate.
This source-only witness pins the leased apply shape: the migration SQL and
version bump share one `dorg-sql--lease-batch' call with REPORT-ROLLBACK non-nil,
the batch sees the exact lease acquired by `dorg-mutate-call-with-store-gate',
and no ordinary commit helper or second commit is reached.  If the public entry
bypasses the gate while still executing the callback, the batch assertion that
the lock is held goes RED."
  (let ((dorg-schema-version 7)
        (dorg-schema-migrations '((6 . "ALTER TABLE headings ADD COLUMN bx6op INT")))
        (dorg-sql-transport 'server)
        (db-version 6)
        (lease (dorg-sql--make-lease :dolt-dir "/tmp/bx6op-store"))
        (gate-held nil)
        (dorg-test-bx6op--receipt-schema-reads nil)
        demo-installed
        calls)
    (dorg-test-with-clean-extensions
      (dorg-register-extension
       'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
      (cl-letf (((symbol-function 'dorg-sql--acquire-lease)
                 (lambda (dir)
                   (push (list :acquire dir) calls)
                   lease))
                ((symbol-function 'dorg-sql--release-lease)
                 (lambda (seen broken)
                   (push (list :release seen broken) calls)))
                ((symbol-function 'dorg-sql--mutate-lock-key)
                 (lambda (seen)
                   (should (eq seen lease))
                   (push (list :lock-key seen) calls)
                   "bx6op-gate-key"))
                ((symbol-function 'dorg-sql--lease-get-lock)
                 (lambda (seen key timeout)
                   (should (eq seen lease))
                   (should (equal key "bx6op-gate-key"))
                   (push (list :get-lock seen key timeout) calls)
                   (setq gate-held t)))
                ((symbol-function 'dorg-sql--lease-release-lock)
                 (lambda (seen key)
                   (should (eq seen lease))
                   (should (equal key "bx6op-gate-key"))
                   (should gate-held)
                   (setq gate-held nil)
                   (push (list :release-lock seen key) calls)))
                ((symbol-function 'dorg-repo--working-set-clean-p)
                 (lambda (&rest _) (push 'ordinary-clean calls) t))
                ((symbol-function 'dorg-sql--lease-exec)
                 (lambda (seen query &rest args)
                   (should (eq seen lease))
                   (let ((key (car args)))
                     (cond
                      ((string-match-p "information_schema" query)
                       (dorg-test-bx6op--receipt-schema-rows query))
                      ((string-match-p "dolt_status" query) nil)
                      ((and key (string-search "schema_version" key))
                       `(((value . ,(number-to-string db-version)))))
                      ((and key (string-search "ext:demo:version" key))
                       (and demo-installed '(((value . "1")))))
                      (t (error "unexpected leased read: %s %S" query args))))))
                ((symbol-function 'dorg-sql--lease-batch)
                 (lambda (seen statements commit-message &optional validate report-rollback)
                   (should (eq seen lease))
                   (should gate-held)
                   (push (list :batch seen statements commit-message validate report-rollback)
                         calls)
                   (cond
                    ((equal commit-message "dorg: migrate schema v6 -> v7")
                     (setq db-version 7))
                    ((equal commit-message "dorg: install extension demo v1")
                     (setq demo-installed t))
                    (t (ert-fail (format "unexpected commit message: %s"
                                         commit-message))))
                   "commit-hash"))
                ((symbol-function 'dorg-repo--commit-schema-step)
                 (lambda (&rest _) (push 'ordinary-commit calls)))
                ((symbol-function 'dorg-commit)
                 (lambda (&rest _) (push 'second-commit calls))))
        (dorg-migrate-schema "/tmp/bx6op-store")))
    (should-not gate-held)
    (should (equal '(columns tables) dorg-test-bx6op--receipt-schema-reads))
    (should-not (member 'ordinary-clean calls))
    (should-not (member 'ordinary-commit calls))
    (should-not (member 'second-commit calls))
    (should (= 1 (cl-count-if (lambda (x) (eq (car-safe x) :acquire)) calls)))
    (should (= 1 (cl-count-if (lambda (x) (eq (car-safe x) :release)) calls)))
    (should (= 1 (cl-count-if (lambda (x) (eq (car-safe x) :release-lock)) calls)))
    (let ((batches (nreverse (cl-remove-if-not
                              (lambda (x) (and (consp x) (eq (car x) :batch)))
                              calls))))
      (should (= 2 (length batches)))
      (let ((core (car batches))
            (ext (cadr batches)))
        (should (eq lease (plist-get core :batch)))
        (should (member "ALTER TABLE headings ADD COLUMN bx6op INT"
                        (nth 2 core)))
        (should (cl-some (lambda (sql)
                           (string-match-p "schema_version" sql))
                         (nth 2 core)))
        (should (equal "dorg: migrate schema v6 -> v7" (nth 3 core)))
        (should (eq t (nth 5 core)))
        (should (eq lease (plist-get ext :batch)))
        (should (member "CREATE TABLE demo_t (id INT PRIMARY KEY);"
                        (nth 2 ext)))
        (should (cl-some (lambda (sql)
                           (string-match-p "ext:demo:version" sql))
                         (nth 2 ext)))
        (should (equal "dorg: install extension demo v1" (nth 3 ext)))
        (should (eq t (nth 5 ext)))))))

(ert-deftest dorg-test-bx6op-explicit-migration-releases-gate-after-step-failure ()
  "A signaled schema step failure releases the real gate once before surfacing."
  (let ((dorg-schema-version 7)
        (dorg-schema-migrations '((6 . "ALTER TABLE headings ADD COLUMN bx6op INT")))
        (dorg-sql-transport 'server)
        (db-version 6)
        (lease (dorg-sql--make-lease :dolt-dir "/tmp/bx6op-store"))
        (gate-held nil)
        events)
    (cl-letf (((symbol-function 'dorg-sql--acquire-lease)
               (lambda (dir) (push (list :acquire dir) events) lease))
              ((symbol-function 'dorg-sql--release-lease)
               (lambda (seen broken)
                 (push (list :release seen broken) events)))
              ((symbol-function 'dorg-sql--mutate-lock-key)
               (lambda (seen)
                 (should (eq seen lease))
                 "bx6op-gate-key"))
              ((symbol-function 'dorg-sql--lease-get-lock)
               (lambda (seen _key _timeout)
                 (should (eq seen lease))
                 (setq gate-held t)))
              ((symbol-function 'dorg-sql--lease-release-lock)
               (lambda (seen _key)
                 (should (eq seen lease))
                 (should gate-held)
                 (setq gate-held nil)
                 (push :release-lock events)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (seen query &rest args)
                 (should (eq seen lease))
                 (let ((key (car args)))
                   (cond
                    ((string-match-p "dolt_status" query) nil)
                    ((and key (string-search "schema_version" key))
                     `(((value . ,(number-to-string db-version)))))
                    ((and key (string-search "ext:" key)) nil)
                    (t nil)))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (seen _statements _message &rest _)
                 (should (eq seen lease))
                 (should gate-held)
                 (signal 'dorg-sql-commit-indeterminate '("commit lost")))))
      (should-error (dorg-migrate-schema "/tmp/bx6op-store")
                    :type 'dorg-sql-commit-indeterminate))
    (should-not gate-held)
    (should (= 1 (cl-count-if (lambda (x) (eq (car-safe x) :acquire)) events)))
    (should (= 1 (cl-count :release-lock events)))
    (should (= 1 (cl-count-if (lambda (x) (eq (car-safe x) :release)) events)))))

(ert-deftest dorg-test-bx6op-all-current-migration-skips-clean-check ()
  "An all-current migration skips dirty-set checks but checks receipt shape.
Non-vacuous: if the all-current guard is removed, the stubbed dirty
`dolt_status' row makes the leased clean check signal before this test can
finish."
  (let ((dorg-schema-version 6)
        (dorg-test-bx6op--receipt-schema-reads nil)
        clean-checks
        batches)
    (dorg-test-with-clean-extensions
      (dorg-register-extension
       'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
      (cl-letf (((symbol-function 'dorg-sql--lease-exec)
                 (lambda (_lease query &rest args)
                   (let ((key (car args)))
                     (cond
                      ((string-match-p "information_schema" query)
                       (dorg-test-bx6op--receipt-schema-rows query))
                      ((string-match-p "dolt_status" query)
                       (push 'dirty-read clean-checks)
                       '(((table_name . "demo_t"))))
                      ((and key (string-search "schema_version" key))
                       '(((value . "6"))))
                      ((and key (string-search "ext:demo:version" key))
                       '(((value . "1"))))
                      (t (error "unexpected leased read: %s %S" query args))))))
                ((symbol-function 'dorg-sql--lease-batch)
                 (lambda (&rest _)
                   (push 'batch batches)
                   "commit")))
        (should-not (dorg-repo--migrate-schema-on-lease
                     "/tmp/bx6op-store" 'lease))))
    (should-not clean-checks)
    (should (equal '(columns tables) dorg-test-bx6op--receipt-schema-reads))
    (should-not batches)))

(ert-deftest dorg-test-bx6op-migrate-schema-cold-loads-store-gate ()
  "A fresh public migration entry resolves the real store gate dependency.
The subprocess requires only `dorg-repo' before calling `dorg-migrate-schema':
no umbrella `dorg', no suite runner and no preloaded `dorg-mutate'.  It stubs
only lower lease primitives, so removing the autoload makes the public entry
fail with `void-function' before the lease-entry sentinel."
  (let* ((root (file-name-directory (locate-library "dorg-repo")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'cl-lib)
             (require 'dorg-repo)
             (when (featurep 'dorg-mutate)
               (error \"dorg-mutate preloaded before public entry\"))
             (let ((events nil))
               (cl-letf (((symbol-function 'dorg-repo--ensure-managed-migration-transport)
                          (lambda (&rest _) (push 'transport events)))
                         ((symbol-function 'dorg-sql--acquire-lease)
                          (lambda (dir) (push (list 'acquire dir) events) 'lease))
                         ((symbol-function 'dorg-sql--release-lease)
                          (lambda (lease broken)
                            (push (list 'release lease broken) events)))
                         ((symbol-function 'dorg-sql--mutate-lock-key)
                          (lambda (lease) (push (list 'lock-key lease) events) 'key))
                         ((symbol-function 'dorg-sql--lease-get-lock)
                          (lambda (lease key timeout)
                            (push (list 'get-lock lease key timeout) events)
                            t))
                         ((symbol-function 'dorg-sql--lease-release-lock)
                          (lambda (lease key)
                            (push (list 'release-lock lease key) events)))
                         ((symbol-function 'dorg-repo--migrate-schema-on-lease)
                          (lambda (dir lease)
                            (push (list 'lease-entry dir lease
                                        (featurep 'dorg-mutate))
                                  events)
                            nil)))
                 (dorg-migrate-schema \"/tmp/bx6op-store\"))
               (unless (member '(lease-entry \"/tmp/bx6op-store\" lease t)
                               events)
                 (error \"missing lease-entry through loaded gate: %S\"
                        (nreverse events)))
               (princ (format \"events=%S\\n\" (nreverse events)))))"))
    (with-temp-buffer
      (let ((status
             (call-process emacs nil (current-buffer) nil
                           "--batch" "-Q"
                           "-L" root
                           "-L" (expand-file-name "lib/mysql" root)
                           "-L" (expand-file-name "lib/clime" root)
                           "--eval" expr)))
        (ert-info ((format "child Emacs output:\n%s" (buffer-string)))
          (should (= 0 status)))))))

(ert-deftest dorg-test-bx6op-function-resolver-uses-scoped-read-helper ()
  "A real function-valued resolver keeps the one-arg DOLT-DIR contract and reads
through the explicit scoped helper on the owned session."
  (let ((dorg-schema-version 7)
        (seen-arg nil)
        (helper-queries nil)
        sql)
    (cl-letf (((symbol-function 'dorg-sql--lease-exec)
               (lambda (lease query &rest args)
                 (push (list lease query args) helper-queries)
                 '(((n . 3)))))
              ((symbol-function 'dorg-dolt-server--canonical-dir)
               (lambda (dir) (file-name-as-directory (expand-file-name dir)))))
      (setq sql
            (dorg-repo--with-migration-resolver-scope
                "/tmp/bx6op-store" 'lease
              (dorg-repo--resolve-core-migration-step
               "/tmp/bx6op-store"
               (lambda (dolt-dir)
                 (setq seen-arg dolt-dir)
                 (let ((rows (dorg-repo-migration-query
                              dolt-dir
                              "SELECT COUNT(*) AS n FROM headings WHERE level = %d"
                              1)))
                   (format "INSERT INTO audit VALUES (%s)"
                           (alist-get 'n (car rows)))))))))
    (should (equal seen-arg "/tmp/bx6op-store"))
    (should (equal sql '("INSERT INTO audit VALUES (3)")))
    (should (equal helper-queries
                   '((lease
                      "SELECT COUNT(*) AS n FROM headings WHERE level = 1"
                      nil))))))

(ert-deftest dorg-test-bx6op-resolver-helper-refuses-non-read-sql ()
  "Resolver-scoped reads refuse writes, transaction control, and multi-statements."
  (cl-letf (((symbol-function 'dorg-sql--lease-exec)
             (lambda (&rest _)
               (ert-fail "unsafe resolver query must not execute")))
            ((symbol-function 'dorg-dolt-server--canonical-dir)
             (lambda (dir) (file-name-as-directory (expand-file-name dir)))))
    (dorg-repo--with-migration-resolver-scope "/tmp/bx6op-store" 'lease
      (dolist (query '("UPDATE headings SET title = 'x'"
                       "BEGIN"
                       "SELECT 1; DELETE FROM headings"))
        (let ((err (should-error
                    (dorg-repo-migration-query "/tmp/bx6op-store" query)
                    :type 'dorg-schema-migration-required)))
          (should (eq 'resolver-read-unsafe-sql (dorg-error-reason err)))
          (should (string-match-p "SELECT-only" (error-message-string err))))))))

(ert-deftest dorg-test-bx6op-resolver-helper-refuses-out-of-scope-and-wrong-store ()
  "The resolver read helper is scoped to the active callback and exact store."
  (let ((err (should-error
              (dorg-repo-migration-query "/tmp/bx6op-store" "SELECT 1")
              :type 'dorg-schema-migration-required)))
    (should (eq 'resolver-read-outside-scope (dorg-error-reason err)))
    (should (string-match-p "outside" (error-message-string err))))
  (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
             (lambda (dir) (file-name-as-directory (expand-file-name dir)))))
    (dorg-repo--with-migration-resolver-scope "/tmp/bx6op-store" 'lease
      (let ((err (should-error
                  (dorg-repo-migration-query "/tmp/other-store" "SELECT 1")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "wrong store" (error-message-string err)))))))

(ert-deftest dorg-test-bx6op-resolver-helper-scope-unwinds-on-error ()
  "Resolver helper authority does not survive a callback error."
  (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
             (lambda (dir) (file-name-as-directory (expand-file-name dir)))))
    (should-error
     (dorg-repo--with-migration-resolver-scope "/tmp/bx6op-store" 'lease
       (error "boom"))
     :type 'error))
  (let ((err (should-error
              (dorg-repo-migration-query "/tmp/bx6op-store" "SELECT 1")
              :type 'dorg-schema-migration-required)))
    (should (string-match-p "outside" (error-message-string err)))))

(ert-deftest dorg-test-bx6op-legacy-resolver-ordinary-sql-refuses-actionably ()
  "A resolver that re-enters ordinary same-store SQL while leased gets a
compatibility refusal naming the scoped helper."
  (let ((dorg-schema-version 7)
        (dorg-schema-migrations
         (list (cons 6 (lambda (dolt-dir)
                         (dorg-sql dolt-dir "SELECT 1")
                         "SELECT 1"))))
        (db-version 6))
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease query &rest args)
                 (let ((key (car args)))
                   (cond
                    ((string-match-p "dolt_status" query) nil)
                    ((and key (string-search "schema_version" key))
                     `(((value . ,(number-to-string db-version)))))
                    (t nil)))))
              ((symbol-function 'dorg-sql)
               (lambda (&rest _)
                 (signal 'dorg-sql-lease-busy '("ordinary SQL refused"))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (ert-fail "batch must not run"))))
      (let ((err (should-error
                  (dorg-migrate-schema "/tmp/bx6op-store")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "dorg-repo-migration-query"
                                (error-message-string err)))))))

(ert-deftest dorg-test-bx6op-resolver-chain-sees-prior-committed-step ()
  "A later data-dependent resolver reads after an earlier migration step lands."
  (let ((dorg-schema-version 8)
        (dorg-schema-migrations
         (list (cons 6 (lambda (dolt-dir)
                         (let ((rows (dorg-repo-migration-query
                                      dolt-dir "SELECT marker AS m FROM phase")))
                           (format "INSERT INTO seen VALUES ('%s')"
                                   (alist-get 'm (car rows))))))
               (cons 7 (lambda (dolt-dir)
                         (let ((rows (dorg-repo-migration-query
                                      dolt-dir "SELECT marker AS m FROM phase")))
                           (format "INSERT INTO seen VALUES ('%s')"
                                   (alist-get 'm (car rows))))))))
        (db-version 6)
        (phase "before")
        (dorg-test-bx6op--receipt-schema-reads nil)
        resolver-reads
        batches)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease query &rest args)
                 (let ((key (car args)))
                   (cond
                    ((string-match-p "dolt_status" query) nil)
                    ((and key (string-search "schema_version" key))
                     `(((value . ,(number-to-string db-version)))))
                    ((string-match-p "SELECT marker" query)
                     (push phase resolver-reads)
                     `(((m . ,phase))))
                    ((string-match-p "information_schema" query)
                     (dorg-test-bx6op--receipt-schema-rows query))
                    (t nil)))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (_lease statements _message &optional _validate report-rollback)
                 (should (eq t report-rollback))
                 (push statements batches)
                 (setq db-version (1+ db-version))
                 (setq phase "after")
                 "commit-hash")))
      (dorg-migrate-schema "/tmp/bx6op-store"))
    (should (equal (nreverse resolver-reads) '("before" "after")))
    (should (equal '(columns tables) dorg-test-bx6op--receipt-schema-reads))
    (should (= 2 (length batches)))))

(ert-deftest dorg-test-bx6op-ordinary-open-never-runs-resolvers ()
  "Ordinary compatibility refuses without executing migration resolvers."
  (let ((dorg-schema-version 7)
        (resolver-calls 0))
    (let ((dorg-schema-migrations
           (list (cons 6 (lambda (_dolt-dir)
                           (cl-incf resolver-calls)
                           "SELECT 1")))))
      (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
                 (lambda (_dir _query &rest args)
                   (let ((key (car args)))
                     (cond
                      ((and key (string-search "schema_version" key))
                       '(((value . "6"))))
                      (t nil)))))
                ((symbol-function 'dorg-repo--commit-schema-step)
                 (lambda (&rest _) (ert-fail "ordinary open must not write"))))
        (should-error (dorg-repo-ensure-schema "/tmp/bx6op-store")
                      :type 'dorg-schema-migration-required)))
    (should (= 0 resolver-calls))))

(ert-deftest dorg-test-bx6op-explicit-migration-installs-absent-extension ()
  "Explicit migration may install an absent registered extension on the lease."
  (let ((batches nil)
        (dorg-test-bx6op--receipt-schema-reads nil))
    (dorg-test-with-clean-extensions
      (dorg-register-extension
       'demo
       :schema '("CREATE TABLE demo_t (id INT PRIMARY KEY)")
       :version 2)
      (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
                 (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
                ((symbol-function 'dorg-sql--lease-exec)
                 (lambda (_lease query &rest args)
                   (let ((key (car args)))
                     (cond
                      ((string-match-p "dolt_status" query) nil)
                      ((and key (string-search "schema_version" key))
                       `(((value . ,(number-to-string dorg-schema-version)))))
                      ((and key (string-search "ext:demo:version" key))
                       nil)
                      ((string-match-p "information_schema" query)
                       (dorg-test-bx6op--receipt-schema-rows query))
                      (t nil)))))
                ((symbol-function 'dorg-sql--lease-batch)
                 (lambda (_lease statements message &optional _validate report-rollback)
                   (push (list statements message report-rollback) batches)
                   "commit-hash")))
        (dorg-migrate-schema "/tmp/bx6op-store")))
    (should (equal '(columns tables) dorg-test-bx6op--receipt-schema-reads))
    (should (= 1 (length batches)))
    (let ((batch (car batches)))
      (should (member "CREATE TABLE demo_t (id INT PRIMARY KEY)" (car batch)))
      (should (cl-some (lambda (sql)
                         (string-match-p "ext:demo:version" sql))
                       (car batch)))
      (should (equal "dorg: install extension demo v2" (cadr batch)))
      (should (eq t (nth 2 batch))))))

(ert-deftest dorg-test-bx6op-explicit-newer-refuses-before-write ()
  "Explicit migration refuses a newer selected component before any DDL batch."
  (let ((dorg-schema-version 6)
        batches)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease _query &rest args)
                 (let ((key (car args)))
                   (cond
                    ((and key (string-search "schema_version" key))
                     '(((value . "7"))))
                    (t nil)))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (push 'batch batches) "commit")))
      (should-error (dorg-migrate-schema "/tmp/bx6op-store")
                    :type 'dorg-schema-repo-newer))
    (should-not batches))
  (let ((dorg-schema-version 6)
        migrated)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-repo--migrate-schema-on-lease)
               (lambda (_dir _lease) (setq migrated t))))
      (dorg-migrate-schema "/tmp/bx6op-store"))
    (should migrated)))

(ert-deftest dorg-test-bx6op-migrate-schema-refuses-cli-seam-before-effects ()
  "Explicit migration refuses unsupported offline/CLI-pinned requests up front."
  (let (effects)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (&rest _) (push 'gate effects)))
              ((symbol-function 'dorg-dolt-server--strict-port)
               (lambda (&rest _) (push 'server effects)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (&rest _) (push 'lease-read effects)))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (push 'batch effects))))
      (let ((err (should-error
                  (dorg-sql--with-cli-seam "/tmp/bx6op-store"
                    (dorg-migrate-schema "/tmp/bx6op-store"))
                  :type 'dorg-schema-migration-required)))
        (should (eq 'unsupported-offline-migration (dorg-error-reason err)))
        (should (string-match-p "managed server" (error-message-string err)))))
    (should-not effects)))

(ert-deftest dorg-test-bx6op-migrate-schema-server-transport-reaches-gate ()
  "Supported server transport reaches the existing leased migration gate."
  (let (events)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _)
                 (push 'gate events)
                 (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-repo--migrate-schema-on-lease)
               (lambda (_dir lease) (push (list 'migrate lease) events))))
      (let ((dorg-sql-transport 'server))
        (dorg-migrate-schema "/tmp/bx6op-store")))
    (should (equal (nreverse events) '(gate (migrate lease))))))

(ert-deftest dorg-test-bx6op-cli-open-caller-refuses-ordinary-migration ()
  "The CLI open path reaches the ordinary compatibility boundary without writes."
  (require 'dorg-app)
  (let* ((root (make-temp-file "dorg-bx6op-cli-" t))
         (org-file (expand-file-name "task.org" root))
         (dolt-dir (expand-file-name "dolt" root))
         calls)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".dolt" dolt-dir) t)
          (with-temp-file org-file (insert "* Task\n"))
          (let ((dorg-schema-version 7)
                (dorg-schema-migrations '((6 . "ALTER TABLE headings ADD COLUMN bx6op INT"))))
            (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
                       (lambda (_dir _query &rest args)
                         (let ((key (car args)))
                           (cond
                            ((and key (string-search "schema_version" key))
                             '(((value . "6"))))
                            (t nil)))))
                      ((symbol-function 'dorg-repo--commit-schema-step)
                       (lambda (&rest _) (push 'commit calls)))
                      ((symbol-function 'dorg-repo-ensure-extensions)
                       (lambda (&rest _) (push 'extensions calls)))
                      ((symbol-function 'dorg-repo-root-id)
                       (lambda (&rest _) (push 'root calls))))
              (let ((err (should-error
                          (dorg-app--open org-file dolt-dir)
                          :type 'dorg-schema-migration-required)))
                (should (string-match-p "M-x dorg-migrate-schema"
                                        (error-message-string err)))))))
      (when (get-file-buffer org-file)
        (kill-buffer (get-file-buffer org-file)))
      (delete-directory root t))
    (should-not calls)))

(ert-deftest dorg-test-bx6op-rolled-back-step-resets-before-resignal ()
  "A confirmed transaction rollback is restored only after owned reset succeeds."
  (let (resets)
    (cl-letf (((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _)
                 (signal 'dorg-sql-transaction-rolled-back
                         '("rolled back" (:rollback-confirmed t)))))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease query &rest _)
                 (push query resets)
                 nil)))
      (let ((err (should-error
                  (dorg-repo--apply-schema-step-on-lease
                   'lease "/tmp/bx6op-store" '("ALTER TABLE t ADD COLUMN c INT")
                   "dorg: migrate schema v6 -> v7")
                  :type 'dorg-sql-transaction-rolled-back)))
        (should (eq t (plist-get (dorg-sql-transaction-rollback-data err)
                                 :rollback-confirmed)))
        (should (eq t (plist-get (dorg-sql-transaction-rollback-data err)
                                 :reset-confirmed)))))
    (should (equal resets '("CALL DOLT_RESET('--hard')")))))

(ert-deftest dorg-test-bx6op-reset-failure-reports-unresolved-cleanup ()
  "A failed owned reset reports unresolved cleanup without raw reset payloads."
  (let (resets)
    (cl-letf (((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _)
                 (signal 'dorg-sql-transaction-rolled-back
                         '("rolled back" (:rollback-confirmed t)))))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease query &rest _)
                 (push query resets)
                 (error "PRIVATE-RESET-PAYLOAD-921"))))
      (let* ((err (should-error
                   (dorg-repo--apply-schema-step-on-lease
                    'lease "/tmp/bx6op-store" '("ALTER TABLE t ADD COLUMN c INT")
                    "dorg: migrate schema v6 -> v7")
                   :type 'dorg-sql-transaction-rollback-failed))
             (message (error-message-string err))
             (data (dorg-sql-transaction-rollback-data err))
             (public (format "%S %S" message data)))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should (eq nil (plist-get data :reset-confirmed)))
        (should (eq 'reset-failed (plist-get data :reset-error)))
        (should-not (string-match-p "PRIVATE-RESET-PAYLOAD-921" public))))
    (should (equal resets '("CALL DOLT_RESET('--hard')")))))

(ert-deftest dorg-test-bx6op-broken-or-indeterminate-step-never-resets-or-continues ()
  "Broken lease and indeterminate commit abort without reset, replay or later steps."
  (dolist (fault '((dorg-sql-lease-broken . "lease broke")
                   (dorg-sql-commit-indeterminate . "commit indeterminate")))
    (let ((dorg-schema-version 8)
          (dorg-schema-migrations '((6 . "STEP 6") (7 . "STEP 7")))
          (db-version 6)
          batches
          resets)
      (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
                 (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
                ((symbol-function 'dorg-sql--lease-exec)
                 (lambda (_lease query &rest args)
                   (cond
                    ((string-match-p "DOLT_RESET" query)
                     (push query resets)
                     nil)
                    ((string-match-p "dolt_status" query) nil)
                    ((and (car args) (string-search "schema_version" (car args)))
                     `(((value . ,(number-to-string db-version)))))
                    (t nil))))
                ((symbol-function 'dorg-sql--lease-batch)
                 (lambda (_lease statements _message &optional _validate report-rollback)
                   (should (eq t report-rollback))
                   (push statements batches)
                   (signal (car fault) (list (cdr fault))))))
        (should-error (dorg-migrate-schema "/tmp/bx6op-store")
                      :type (car fault)))
      (should (= 1 (length batches)))
      (should (equal (car batches)
                     '("STEP 6"
                       "UPDATE dorg_meta SET value = '7' WHERE `key` = 'schema_version'")))
      (should-not resets))))

(ert-deftest dorg-test-bx6op-explicit-preflight-refuses-missing-step-before-write ()
  "Whole-selection chain preflight refuses a missing step before any DDL batch."
  (let ((dorg-schema-version 8)
        (dorg-schema-migrations '((6 . "STEP 6")))
        (db-version 6)
        batches)
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease 'key)))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (_lease query &rest args)
                 (cond
                  ((string-match-p "dolt_status" query) nil)
                  ((and (car args) (string-search "schema_version" (car args)))
                   `(((value . ,(number-to-string db-version)))))
                  (t nil))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (push 'batch batches) "commit")))
      (let ((err (should-error
                  (dorg-migrate-schema "/tmp/bx6op-store")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "core:missing-step" (error-message-string err)))))
    (should-not batches)))

(ert-deftest dorg-test-bx6op-malformed-core-metadata-refuses-before-write ()
  "Malformed existing core metadata refuses; it is not fresh creation or repair."
  (let ((writes nil))
    (cl-letf (((symbol-function 'dorg-repo--ordinary-read)
               (lambda (_dir _query &rest args)
                 (cond
                  ((and (car args) (string-search "schema_version" (car args)))
                   '(((value . "not-an-integer"))))
                  (t nil))))
              ((symbol-function 'dorg-repo--commit-schema-step)
               (lambda (&rest _) (push 'commit writes)))
              ((symbol-function 'dorg-repo-ensure-extensions)
               (lambda (&rest _) (push 'extensions writes))))
      (let ((err (should-error
                  (dorg-repo-ensure-schema "/tmp/bx6op-store")
                  :type 'dorg-schema-migration-required)))
        (should (string-match-p "core:malformed" (error-message-string err)))))
    (should-not writes)))

(ert-deftest dorg-test-ttp2t-schema-v7-hard-renames-file-binding-column ()
  "Fresh v7 stores encode one table-wide relative binding representation.
The hard `path' -> `relative_path' rename is the fail-closed read fence: no
per-row kind discriminator or legacy absolute-path column survives."
  (let ((ddl (mapconcat #'identity dorg-schema-ddl "\n")))
    (should (string-search "CREATE TABLE dorg_files (" ddl))
    (should (string-search "relative_path   VARCHAR(1024) NOT NULL" ddl))
    (should (string-search
             "UNIQUE KEY uq_dorg_files_relative_path (relative_path)"
             ddl))
    (should-not (string-search "locator_kind" ddl))
    (should-not (string-search "store-relative-v1" ddl))
    (should-not (string-search "uq_dorg_files_path (path)" ddl))
    (should (string-search
             (format "VALUES ('schema_version', '%d')" dorg-schema-version)
             ddl))))

(ert-deftest dorg-test-ttp2t-relative-binding-round-trips-sibling-layout ()
  "The canonical Downstream tracker sibling layout stores `../beads.org' and round-trips.
Parent segments are legal; escaping is defined by failure to reconstruct the
same canonical local file, not by the mere presence of `..'."
  (let* ((root (make-temp-file "dorg-ttp2t-layout-" t))
         (tracker (expand-file-name ".tracker" root))
         (dolt-dir (expand-file-name "dolt" tracker))
         (file (expand-file-name "beads.org" tracker)))
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (with-temp-file file (insert "* Bead\n"))
          (let ((relative (dorg-repo-encode-relative-path dolt-dir file)))
            (should (equal "../beads.org" relative))
            (should (equal (file-truename file)
                           (dorg-repo-decode-relative-path
                            dolt-dir relative)))))
      (delete-directory root t))))

(ert-deftest dorg-test-ttp2t-relative-binding-survives-bundle-relocation ()
  "A stored relative binding resolves to the same file after bundle relocation."
  (let* ((parent (make-temp-file "dorg-ttp2t-move-" t))
         (old-root (expand-file-name "old" parent))
         (new-root (expand-file-name "new" parent))
         (old-dolt (expand-file-name ".tracker/dolt" old-root))
         (old-file (expand-file-name ".tracker/beads.org" old-root)))
    (unwind-protect
        (progn
          (make-directory old-dolt t)
          (with-temp-file old-file (insert "* Bead\n"))
          (let ((relative (dorg-repo-encode-relative-path old-dolt old-file)))
            (rename-file old-root new-root)
            (let ((new-dolt (expand-file-name ".tracker/dolt" new-root))
                  (new-file (expand-file-name ".tracker/beads.org" new-root)))
              (should (equal "../beads.org" relative))
              (should (equal (file-truename new-file)
                             (dorg-repo-decode-relative-path
                              new-dolt relative))))))
      (delete-directory parent t))))

(ert-deftest dorg-test-ttp2t-relative-binding-rejects-malformed-encoding ()
  "Malformed persisted relative bindings fail closed with structured metadata."
  (dolist (relative '("" "." "/absolute.org" "x/../beads.org"))
    (let ((err (should-error
                (dorg-repo-decode-relative-path "/tmp/store" relative)
                :type 'dorg-binding-error)))
      (should (eq 'invalid-locator (dorg-error-reason err)))
      (should (equal relative
                     (plist-get (dorg-error-data err) :relative-path))))))

(ert-deftest dorg-test-ttp2t-lookup-refuses-malformed-persisted-locator ()
  "Lookup validates the complete persisted locator inventory before matching."
  (dorg-test-with-dolt
    (let* ((file (expand-file-name "valid.org" dolt-dir))
           (root-id (dorg-repo-ensure-root dolt-dir file)))
      (dorg-sql
       dolt-dir
       (format "UPDATE dorg_files SET relative_path = 'x/../valid.org' WHERE root_id = %d"
               root-id))
      (dorg-commit dolt-dir "seed malformed locator")
      (let ((err (should-error
                  (dorg-repo-root-id dolt-dir file)
                  :type 'dorg-binding-error)))
        (should (eq 'invalid-locator (dorg-error-reason err)))
        (should (equal "x/../valid.org"
                       (plist-get (dorg-error-data err) :relative-path)))))))

(ert-deftest dorg-test-7o3q2-omitted-revision-keeps-live-query-and-error ()
  "Live lookup retains its exact query and ambiguous-binding error kind."
  (let (queries)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dir query &rest _)
                 (push query queries)
                 '(((root_id . 1) (relative_path . "beads.org")
                    (heading_id . 1) (heading_root_id . 1)
                    (parent_id . nil) (level_offset . 0))
                   ((root_id . 2) (relative_path . "beads.org")
                    (heading_id . 2) (heading_root_id . 2)
                    (parent_id . nil) (level_offset . 0))))))
      (let ((err (should-error
                  (dorg-repo-root-id "/tmp/store" "/tmp/store/beads.org")
                  :type 'dorg-binding-error)))
        (should (eq 'ambiguous-locator (dorg-error-reason err)))))
    (should
     (equal (car queries)
            (concat "SELECT f.root_id AS root_id, f.relative_path AS relative_path, "
                    "h.id AS heading_id, h.root_id AS heading_root_id, "
                    "h.parent_id AS parent_id, h.level_offset AS level_offset "
                    "FROM dorg_files f LEFT JOIN headings h ON h.id = f.root_id "
                    "ORDER BY f.root_id ASC")))))

(ert-deftest dorg-test-7o3q2-pre-offset-revision-refuses-typed ()
  "A pre-level-offset historical token cannot be read as a mixed schema."
  (cl-letf (((symbol-function 'dorg-sql)
             (lambda (&rest _)
               (error "Unknown column 'h.level_offset' in 'field list"))))
    (let ((err (should-error
                (dorg-repo-root-id "/tmp/store" "/tmp/store/beads.org" "H")
                :type 'dorg-binding-error)))
      (should (eq 'unsupported-revision-schema (dorg-error-reason err)))
      (should (equal "H" (plist-get (dorg-error-data err) :revision))))))

(ert-deftest dorg-test-7o3q2-revision-token-refuses-before-live-query ()
  "A non-string or empty revision cannot silently select the live root."
  (dolist (revision '("" 42))
    (let ((called nil))
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (&rest _)
                   (setq called t)
                   (error "revision validation reached SQL"))))
        (let ((err (should-error
                    (dorg-repo-root-id "/tmp/store" "/tmp/store/beads.org" revision)
                    :type 'dorg-binding-error)))
          (should (eq 'invalid-revision (dorg-error-reason err)))
          (should-not called))))))

(ert-deftest dorg-test-7o3q2-revision-lookup-cold-loads-sql-suffix ()
  "A fresh `emacs -Q' needs only `dorg-repo' for historical lookup."
  (let* ((root (file-name-directory (locate-library "dorg-repo")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
              (require 'cl-lib)
              (require 'dorg-repo)
              (when (featurep 'dorg-ext-read)
                (error \"dorg-ext-read preloaded\"))
              (cl-letf (((symbol-function 'dorg-sql)
                         (lambda (&rest _)
                           '(((root_id . 1) (relative_path . \"beads.org\")
                              (heading_id . 1) (heading_root_id . 1)
                              (parent_id . nil) (level_offset . 0))))))
                (unless (eql 1 (dorg-repo-root-id \"/tmp/store\" \"/tmp/store/beads.org\" \"H\"))
                  (error \"historical root lookup failed\"))))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q" "-L" root "--eval" expr)))))

(ert-deftest dorg-test-7o3q2-historical-rebind-is-snapshot-bound ()
  "A real committed rebind resolves R1 at H1 and R2 at H2/live."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-7o3q2-rebind-" t))
           (file (expand-file-name "beads.org" root))
           (other (expand-file-name "other.org" root)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* First\n"))
            (with-temp-file other (insert "* Replacement\n"))
            (let* ((r1 (dorg-repo-ensure-root dolt-dir file))
                   (h1 (dorg-head-commit dolt-dir))
                   (r2 (dorg-id-generate)))
              (dorg-sql-batch
               dolt-dir
               (list
                (format (concat "INSERT INTO headings "
                                "(id, root_id, parent_id, sort_key, level_offset, "
                                "todo_keyword, priority, commented, title) VALUES "
                                "(%d, %d, NULL, X'01', 0, NULL, NULL, FALSE, NULL)")
                        r2 r2)
                (dorg-repo-file-row-statement dolt-dir r2 other)))
              (dorg-commit dolt-dir "test: seed replacement root")
              (dorg-sql-batch
               dolt-dir
               (list (format "DELETE FROM dorg_files WHERE root_id = %d" r2)
                     (format "UPDATE dorg_files SET root_id = %d WHERE root_id = %d"
                             r2 r1)))
              (dorg-commit dolt-dir "test: replace file root binding")
              (let ((h2 (dorg-head-commit dolt-dir)))
                (should (eql r1 (dorg-repo-root-id dolt-dir file h1)))
                (should (eql r2 (dorg-repo-root-id dolt-dir file h2)))
                (should (eql r2 (dorg-repo-root-id dolt-dir file))))))
        (delete-directory root t)))))

(ert-deftest dorg-test-ttp2t-lookup-refuses-non-self-rooted-binding ()
  "A level-zero heading is not a file root unless its root id is itself."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-ttp2t-root-shape-" t))
           (bound-file (expand-file-name "bound.org" root))
           (wrong-file (expand-file-name "wrong.org" root)))
      (unwind-protect
          (progn
            (with-temp-file bound-file (insert "* Bound\n"))
            (with-temp-file wrong-file (insert "* Wrong\n"))
            (let ((real-root (dorg-repo-ensure-root dolt-dir bound-file))
                  (wrong-root 6199))
              (dorg-sql-batch
               dolt-dir
               (list
                (format (concat "INSERT INTO headings "
                                "(id, root_id, parent_id, sort_key, level_offset, "
                                "commented, title, post_blank) VALUES "
                                "(%d, %d, NULL, X'02', 0, FALSE, NULL, 0)")
                        wrong-root real-root)
                (dorg-repo-file-row-statement dolt-dir wrong-root wrong-file)))
              (dorg-commit dolt-dir "seed non-self-rooted binding")
              (let ((err (should-error
                          (dorg-repo-root-id dolt-dir wrong-file)
                          :type 'dorg-binding-error)))
                (should (eq 'missing-root (dorg-error-reason err))))))
        (delete-directory root t)))))

(ert-deftest dorg-test-ttp2t-strict-bind-nonempty-miss-never-creates-root ()
  "A missing locator in a nonempty binding table refuses before root creation.
Deleting the refusal makes this witness return the synthetic new root, proving
the second-blank-root failure rather than merely checking an error symbol."
  (let ((created nil))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_path) t))
              ((symbol-function 'dorg-repo-ensure-schema) #'ignore)
              ((symbol-function 'dorg-repo-root-id)
               (lambda (_dir _file) nil))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (_dir) 1))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _)
                 (setq created t)
                 999)))
      (let ((err (should-error
                  (dorg-repo-ensure-root "/tmp/store/" "/tmp/other.org")
                  :type 'dorg-binding-error)))
        (should (eq 'path-mismatch (dorg-error-reason err)))
        (should (= 1 (plist-get (dorg-error-data err) :binding-count)))
        (should-not created)))))

(ert-deftest dorg-test-ttp2t-strict-bind-empty-store-creates-once ()
  "Strict bind retains the one supported implicit creation: the initial root."
  (let ((created 0))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_path) t))
              ((symbol-function 'dorg-repo-ensure-schema) #'ignore)
              ((symbol-function 'dorg-repo-root-id)
               (lambda (_dir _file) nil))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (_dir) 0))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _)
                 (cl-incf created)
                 999)))
      (should (= 999
                 (dorg-repo-ensure-root "/tmp/store/" "/tmp/first.org")))
      (should (= 1 created)))))

(ert-deftest dorg-test-ttp2t-rebind-is-exact-base-metadata-only ()
  "Explicit rebind changes one locator while preserving identity and file bytes."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-ttp2t-rebind-" t))
           (old-file (expand-file-name "old.org" root))
           (new-file (expand-file-name "new.org" root))
           (old-bytes "* Old bytes\n")
           (new-bytes "* New bytes\n"))
      (unwind-protect
          (progn
            (with-temp-file old-file (insert old-bytes))
            (with-temp-file new-file (insert new-bytes))
            (let* ((root-id (dorg-repo-ensure-root dolt-dir old-file))
                   (base (dorg-head-commit dolt-dir))
                   (heading-count
                    (alist-get 'n (car (dorg-sql
                                        dolt-dir
                                        "SELECT COUNT(*) AS n FROM headings"))))
                   (receipt (dorg-rebind-file
                             dolt-dir root-id new-file base)))
              (should (plist-get receipt :changed))
              (should (= root-id (plist-get receipt :root-id)))
              (should (equal base (plist-get receipt :base)))
              (should (equal (dorg-head-commit dolt-dir)
                             (plist-get receipt :commit)))
              (should-not (dorg-repo-root-id dolt-dir old-file))
              (should (= root-id (dorg-repo-root-id dolt-dir new-file)))
              (should (= heading-count
                         (alist-get 'n (car (dorg-sql
                                             dolt-dir
                                             "SELECT COUNT(*) AS n FROM headings")))))
              (should (equal old-bytes
                             (with-temp-buffer
                               (insert-file-contents old-file)
                               (buffer-string))))
              (should (equal new-bytes
                             (with-temp-buffer
                               (insert-file-contents new-file)
                               (buffer-string))))
              (let* ((same-base (dorg-head-commit dolt-dir))
                     (same (dorg-rebind-file
                            dolt-dir root-id new-file same-base)))
                (should-not (plist-get same :changed))
                (should (equal same-base (plist-get same :commit))))))
        (delete-directory root t)))))

(ert-deftest dorg-test-ttp2t-rebind-refusals-preserve-head-and-bindings ()
  "Stale, missing-root, invalid, and collision rebinds change no store state."
  (dorg-test-with-dolt
    (let* ((root (make-temp-file "dorg-ttp2t-rebind-refuse-" t))
           (file-a (expand-file-name "a.org" root))
           (file-b (expand-file-name "b.org" root)))
      (unwind-protect
          (progn
            (with-temp-file file-a (insert "* A\n"))
            (with-temp-file file-b (insert "* B\n"))
            (let ((root-a (dorg-repo-ensure-root dolt-dir file-a))
                  (root-b 6200))
              (dorg-sql-batch
               dolt-dir
               (list
                (format (concat "INSERT INTO headings "
                                "(id, root_id, parent_id, sort_key, level_offset, "
                                "commented, title, post_blank) VALUES "
                                "(%d, %d, NULL, X'02', 0, FALSE, NULL, 0)")
                        root-b root-b)
                (dorg-repo-file-row-statement dolt-dir root-b file-b)))
              (dorg-commit dolt-dir "seed second binding")
              (let* ((head (dorg-head-commit dolt-dir))
                     (before (dorg-sql
                              dolt-dir
                              "SELECT root_id, relative_path FROM dorg_files ORDER BY root_id")))
                (should-error
                 (dorg-rebind-file dolt-dir root-a file-b head)
                 :type 'dorg-binding-error)
                (should-error
                 (dorg-rebind-file dolt-dir 999999 file-a head)
                 :type 'dorg-binding-error)
                (should-error
                 (dorg-rebind-file dolt-dir root-a "." head)
                 :type 'dorg-binding-error)
                (should-error
                 (dorg-rebind-file dolt-dir root-a
                                   (expand-file-name "new.org" root)
                                   "stale-base")
                 :type 'dorg-transaction-stale)
                (should (equal head (dorg-head-commit dolt-dir)))
                (should (equal before
                               (dorg-sql
                                dolt-dir
                                "SELECT root_id, relative_path FROM dorg_files ORDER BY root_id"))))))
        (delete-directory root t)))))

(ert-deftest dorg-test-ttp2t-noop-rebind-rechecks-base-under-gate ()
  "A no-op receipt still proves its exact base under the mutation gate."
  (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
             (lambda (_dir fn) (funcall fn 'lease "key")))
            ((symbol-function 'dorg-sql--lease-scalar)
             (lambda (&rest _) "moved")))
    (should-error
     (dorg-repo--check-base-under-gate "/tmp/store" "base")
     :type 'dorg-transaction-stale)))

;;; Dolt Availability

(ert-deftest dorg-test-dolt-available-p ()
  "dorg-dolt-available-p returns non-nil when dolt is installed."
  (if dorg-test--dolt-available
      (should (dorg-dolt-available-p))
    (ert-skip "dolt not installed")))

;;; Canonical project store default (tracked-regression)

(ert-deftest dorg-test-jmay6-default-store-directory-is-pure ()
  "The project default is explicit, normalized, and side-effect free.
The caller supplies PROJECT-ROOT; implicit cwd, project.el, and environment
state must not choose the result.  Computing the default creates neither the
outer `.dorg' control directory nor the inner `dorg' repository."
  (let* ((project-root (make-temp-file "dorg-jmay6-project-" t))
         (alien-cwd (make-temp-file "dorg-jmay6-alien-" t))
         (expected (file-name-as-directory
                    (expand-file-name ".dorg/dorg" project-root))))
    (unwind-protect
        (let ((default-directory alien-cwd)
              (process-environment
               (cons "DORG_DOLT_DIR=/implicit/must-not-win"
                     process-environment)))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _)
                       (ert-fail "default helper consulted project.el"))))
            (should (equal expected
                           (dorg-default-store-directory project-root)))
            (should (file-name-absolute-p
                     (dorg-default-store-directory project-root)))
            (should (string-suffix-p "/"
                                     (dorg-default-store-directory
                                      project-root)))
            (should-not (file-exists-p
                         (expand-file-name ".dorg" project-root)))
            (let ((target
                   (dorg-dolt--target
                    (dorg-default-store-directory project-root))))
              (should (equal "dorg" (plist-get target :db-name)))
              (should (equal expected (plist-get target :repo-dir))))))
      (delete-directory project-root t)
      (delete-directory alien-cwd t))))

(ert-deftest dorg-test-jmay6-canonical-locators-survive-bundle-move ()
  "Both canonical source topologies round-trip from `.dorg/dorg'.
A root-level source encodes as `../../beads.org'; an Downstream tracker projection encodes
as `../../.tracker/beads.org'.  Moving the complete project bundle preserves
both locators and resolves them to the moved files."
  (let* ((parent (make-temp-file "dorg-jmay6-move-" t))
         (old-root (expand-file-name "old-project" parent))
         (new-root (expand-file-name "new-project" parent))
         (old-store (dorg-default-store-directory old-root))
         (old-root-source (expand-file-name "beads.org" old-root))
         (old-tracker-source
          (expand-file-name ".tracker/beads.org" old-root)))
    (unwind-protect
        (progn
          (make-directory old-store t)
          (make-directory (file-name-directory old-tracker-source) t)
          (with-temp-file old-root-source (insert "* Root source\n"))
          (with-temp-file old-tracker-source (insert "* Downstream tracker source\n"))
          (let ((root-locator
                 (dorg-repo-encode-relative-path old-store old-root-source))
                (tracker-locator
                 (dorg-repo-encode-relative-path old-store
                                                 old-tracker-source)))
            (should (equal "../../beads.org" root-locator))
            (should (equal "../../.tracker/beads.org" tracker-locator))
            (rename-file old-root new-root)
            (let ((new-store (dorg-default-store-directory new-root))
                  (new-root-source (expand-file-name "beads.org" new-root))
                  (new-tracker-source
                   (expand-file-name ".tracker/beads.org" new-root)))
              (should (equal (file-truename new-root-source)
                             (dorg-repo-decode-relative-path
                              new-store root-locator)))
              (should (equal (file-truename new-tracker-source)
                             (dorg-repo-decode-relative-path
                              new-store tracker-locator))))))
      (delete-directory parent t))))

(ert-deftest dorg-test-jmay6-same-default-name-keeps-targets-disjoint ()
  "Two default stores both serve `dorg' without sharing local identity.
The database display name is deliberately uniform; canonical path, data-dir,
and config-dir remain store-specific and therefore keep lifecycle ownership
and managed connection keys disjoint."
  (let* ((parent (make-temp-file "dorg-jmay6-targets-" t))
         (root-a (expand-file-name "project-a" parent))
         (root-b (expand-file-name "project-b" parent))
         (store-a (dorg-default-store-directory root-a))
         (store-b (dorg-default-store-directory root-b)))
    (unwind-protect
        (progn
          (make-directory store-a t)
          (make-directory store-b t)
          (let ((target-a (dorg-dolt--target store-a))
                (target-b (dorg-dolt--target store-b)))
            (should (equal "dorg" (plist-get target-a :db-name)))
            (should (equal "dorg" (plist-get target-b :db-name)))
            (dolist (key '(:canonical-repo-dir :server-data-dir :cfg-dir))
              (should-not (equal (plist-get target-a key)
                                 (plist-get target-b key))))))
      (delete-directory parent t))))

(ert-deftest dorg-test-jmay6-explicit-legacy-store-is-unchanged ()
  "An explicit legacy `.tracker/dolt' store keeps its path-derived identity.
The new project default is a helper, not fallback search or a compatibility
branch in target normalization."
  (let* ((project-root (make-temp-file "dorg-jmay6-legacy-" t))
         (legacy (file-name-as-directory
                  (expand-file-name ".tracker/dolt" project-root))))
    (unwind-protect
        (progn
          (make-directory legacy t)
          (let ((target (dorg-dolt--target legacy)))
            (should (equal "dolt" (plist-get target :db-name)))
            (should (equal legacy (plist-get target :repo-dir)))
            (should-not (equal legacy
                               (dorg-default-store-directory project-root)))))
      (delete-directory project-root t))))

(ert-deftest dorg-test-dolt-target-normalizes-nested-store ()
  "A nested Dolt store is served from the store itself, config nested within.
The server data-dir is the store (not its parent) so every sql-server
artifact stays inside the store; `:cfg-dir' is the store-local `.doltcfg'."
  (let* ((root (file-name-as-directory
                (make-temp-file "dorg-target-nested-" t)))
         (tracker (expand-file-name ".tracker/" root))
         (dolt-dir (expand-file-name "dolt/" tracker)))
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (let ((target (dorg-dolt--target dolt-dir)))
            (should (equal (plist-get target :repo-dir)
                           (file-name-as-directory
                            (expand-file-name dolt-dir))))
            (should (equal (plist-get target :cfg-dir)
                           (expand-file-name ".doltcfg" dolt-dir)))
            (should (equal (plist-get target :db-name) "dolt"))
            ;; The server data-dir is the store itself, so artifacts land
            ;; inside it and the parent `.tracker/' stays clean.
            (should (equal (file-truename
                            (plist-get target :server-data-dir))
                           (file-truename dolt-dir)))
            (should-not
             (equal (file-truename (plist-get target :server-data-dir))
                    (file-truename tracker)))
            ;; Config dir is nested under the served store, not a sibling.
            ;; Compare canonicalized (cfg-dir keeps the logical spelling,
            ;; server-data-dir is the truename) so a symlinked TMPDIR does
            ;; not make this vacuous.
            (should (equal (file-truename (plist-get target :cfg-dir))
                           (file-truename
                            (expand-file-name
                             ".doltcfg"
                             (plist-get target :server-data-dir)))))))
      (delete-directory root t))))

(defun dorg-test-h2t9n--literal-sha256 (file)
  "Return the SHA-256 digest of FILE's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun dorg-test-h2t9n--identity-hits (root)
  "Return non-historical retired dependency identity hits below ROOT."
  (let* ((token (concat "clu" "tch"))
         (case-fold-search t)
         (files
          (append
           (directory-files root t "\\.\\(?:el\\|org\\|sh\\)\\'")
           (list (expand-file-name "Makefile" root)
                 (expand-file-name "bin/dorg" root))
           (cl-mapcan
            (lambda (dir)
              (let ((path (expand-file-name dir root)))
                (when (file-directory-p path)
                  (directory-files-recursively
                   path "\\.\\(?:el\\|org\\|sh\\)\\'"))))
            '("docs" "lib" "tests" "tools"))))
         hits)
    (dolist (file files (nreverse hits))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward (regexp-quote token) nil t)
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (and (string-equal (file-name-nondirectory file)
                                         "DEVELOPMENT.org")
                           (string-match-p "historical" line))
                (push (format "%s:%d:%s"
                              (file-relative-name file root)
                              (line-number-at-pos)
                              line)
                      hits)))))))))

(ert-deftest dorg-test-h2t9n-standalone-mysql-layout-and-bytes ()
  "The standalone client has one canonical layout with reviewed bytes."
  (let* ((root (file-name-directory (locate-library "dorg-dolt")))
         (dir (expand-file-name "lib/mysql" root))
         (retired (expand-file-name (concat "lib/" "clu" "tch") root)))
    (should-not (file-exists-p retired))
    (dolist (entry
             '(("mysql.el" .
                "f56b75b236ca5223913dbed8cfd395158fd63b67eec911892ec25f6529d74624")
               ("LICENSE" .
                "3972dc9744f6499f0f9b2dbf76696f2ae7ad8af9b23dde66d6af86c9dfb36986")
               ("UPSTREAM.org" .
                "e3164aaff2a120df8bdeb231186c1eded5025a30ae6765ce878f9c5355e59299")))
      (let ((file (expand-file-name (car entry) dir)))
        (should (file-exists-p file))
        (should (equal (cdr entry)
                       (dorg-test-h2t9n--literal-sha256 file)))))))

(ert-deftest dorg-test-h2t9n-retired-dependency-identity-is-absent ()
  "Only an explicitly historical ledger sentence may retain the old name."
  (let ((root (file-name-directory (locate-library "dorg-dolt"))))
    (should-not (dorg-test-h2t9n--identity-hits root))))

(ert-deftest dorg-test-sql-bundled-mysql-dir-ignores-stale-load-file-name ()
  "Bundled mysql lookup should survive a launcher-owned `load-file-name'."
  (let* ((dorg-file (locate-library "dorg-dolt"))
         (expected (expand-file-name "lib/mysql"
                                     (file-name-directory dorg-file)))
         (load-file-name (make-temp-file "dorg-stale-load-file-" nil ".el"))
         (buffer-file-name nil))
    (unwind-protect
        (progn
          (should (file-exists-p (expand-file-name "mysql.el" expected)))
          (should (equal (file-truename expected)
                         (file-truename (dorg-sql--bundled-mysql-dir)))))
      (delete-file load-file-name))))

(ert-deftest dorg-test-h2t9n-source-root-only-launcher-finds-mysql ()
  "A launcher adding only the DOrg root gets the standalone client."
  (let* ((root (file-name-directory (locate-library "dorg-dolt")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-dolt)
             (let ((dir (dorg-sql--bundled-mysql-dir)))
               (unless (and (featurep 'mysql)
                            dir
                            (string-suffix-p \"/lib/mysql\"
                                             (directory-file-name dir)))
                 (error \"standalone mysql self-discovery failed: %S\" dir))))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q" "-L" root "--eval" expr)))))

;;; mysql.el hard dependency (tracked-case)

(ert-deftest dorg-test-sql-require-mysql-hard-errors-clearly-when-missing ()
  "`dorg-sql--require-mysql-hard' rewraps a MISSING mysql.el (require returns
via the not-found path) into the HARD-dependency diagnostic.  Simulated by
making the bundle unfindable and `require' fail for `mysql' (the live suite
cannot unload mysql itself)."
  (cl-letf* ((orig-require (symbol-function 'require))
             ((symbol-function 'dorg-sql--bundled-mysql-dir) (lambda () nil))
             ((symbol-function 'require)
              (lambda (feature &rest args)
                (if (eq feature 'mysql)
                    (signal 'file-missing (list "Cannot open load file" "mysql"))
                  (apply orig-require feature args)))))
    (let* ((err (should-error (dorg-sql--require-mysql-hard) :type 'error))
           (msg (error-message-string err)))
      (should (string-match-p "mysql\\.el" msg))
      (should (string-match-p "HARD dependency" msg))
      (should (string-match-p "CLI-only fallback" msg)))))

(ert-deftest dorg-test-sql-require-mysql-hard-wraps-signaled-load-error ()
  "`dorg-sql--require-mysql-hard' rewraps a SIGNALED load failure (a present
but broken mysql.el that errors mid-load) into the HARD-dependency diagnostic
instead of letting the raw loader error escape — and preserves the underlying
error for debugging.  Regression for the REVIEW-1 blocker: `require's NOERROR
would NOT have caught this."
  (cl-letf* ((orig-require (symbol-function 'require))
             ((symbol-function 'dorg-sql--bundled-mysql-dir) (lambda () nil))
             ((symbol-function 'require)
              (lambda (feature &rest args)
                (if (eq feature 'mysql)
                    (error "boom from mysql loader")
                  (apply orig-require feature args)))))
    (let* ((err (should-error (dorg-sql--require-mysql-hard) :type 'error))
           (msg (error-message-string err)))
      ;; Wrapped in the hard-dep diagnostic, NOT the raw loader message.
      (should (string-match-p "mysql\\.el" msg))
      (should (string-match-p "HARD dependency" msg))
      (should (string-match-p "CLI-only fallback" msg))
      ;; Underlying error preserved for debugging.
      (should (string-match-p "boom from mysql loader" msg)))))

(ert-deftest dorg-test-sql-require-mysql-hard-ok-when-present ()
  "`dorg-sql--require-mysql-hard' returns without error when mysql.el loads.
Non-vacuous companion to the failure-path test: the happy path must NOT
signal."
  (skip-unless (featurep 'mysql))
  (should (progn (dorg-sql--require-mysql-hard) t)))

(ert-deftest dorg-test-sql-require-dorg-dolt-does-not-load-server ()
  "`dorg-dolt' can load standalone without requiring `dorg-dolt-server'."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-dolt)
             (when (featurep 'dorg-dolt-server)
               (error \"dorg-dolt-server loaded during standalone dorg-dolt require\")))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "-L" (expand-file-name "lib/mysql" root)
                               "--eval" expr)))))

;;; ─── Honest model/storage boundary (tracked-case) ──────────────────────

(ert-deftest dorg-test-sql-quote-module-is-pure ()
  "`(require \\='dorg-sql-quote)' pulls neither the transport nor mysql.
The SQL literal-quoting helpers are a dependency-free leaf so pure
consumers (`dorg-sql-gen', `dorg-keywords') don't drag in the hard mysql
transport.  Runs a fresh `emacs -Q' with only the repo root on the load
path and asserts `dorg-dolt' / `mysql' stay unloaded."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-sql-quote)
             (unless (and (fboundp 'dorg-sql-quote)
                          (fboundp 'dorg-sql-escape-string))
               (error \"quoting helpers missing from dorg-sql-quote\"))
             (when (featurep 'dorg-dolt)
               (error \"dorg-dolt leaked into the dorg-sql-quote leaf\"))
             (when (featurep 'mysql)
               (error \"mysql leaked into the dorg-sql-quote leaf\")))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "--eval" expr)))))

(ert-deftest dorg-test-model-load-symbols-relocated ()
  "Hydration loaders live in `dorg-model-load'; as-of-suffix in `dorg-sql-gen'.
The MODEL-namespaced DB->model loaders must not masquerade as pure model
while sitting in the transport module, and `dorg-model--as-of-suffix' is
SQL text that belongs with SQL generation under an honest name."
  (require 'dorg-model-load)
  (require 'dorg-sql-gen)
  (should (equal "dorg-model-load"
                 (file-name-base (symbol-file 'dorg-model-load-tree 'defun))))
  (should (equal "dorg-model-load"
                 (file-name-base (symbol-file 'dorg-model-load-headings 'defun))))
  (should (equal "dorg-sql-gen"
                 (file-name-base (symbol-file 'dorg-sql-gen--as-of-suffix 'defun))))
  ;; Old MODEL-namespaced SQL helper is gone (renamed, not aliased).
  (should-not (fboundp 'dorg-model--as-of-suffix)))

(ert-deftest dorg-test-dolt-provides-relocated-public-api ()
  "`(require \\='dorg-dolt)' alone preserves the public LOAD contract.
Relocation must not break external Downstream tracker consumers that reach the public
symbols through `(require \\='dorg-dolt)': `dorg-sql-quote' /
`dorg-sql-escape-string' stay defined (dolt hard-requires the leaf) and
`dorg-model-load-tree' / `-headings' stay resolvable via back-compat
autoloads, each with its home in the RELOCATED module — proving the shim
reaches the new file, not a lingering in-dolt copy."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-dolt)
             (dolist (pair '((dorg-sql-quote . \"dorg-sql-quote\")
                             (dorg-sql-escape-string . \"dorg-sql-quote\")
                             (dorg-model-load-tree . \"dorg-model-load\")
                             (dorg-model-load-headings . \"dorg-model-load\")))
               (let ((sym (car pair)) (home (cdr pair)))
                 (unless (fboundp sym)
                   (error \"public symbol %s not resolvable after (require 'dorg-dolt)\" sym))
                 (unless (equal home (file-name-base (symbol-file sym 'defun)))
                   (error \"public symbol %s home is %s, expected %s\"
                          sym (file-name-base (symbol-file sym 'defun)) home)))))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "-L" (expand-file-name "lib/mysql" root)
                               "--eval" expr)))))

(ert-deftest dorg-test-model-load-callers-require-provider ()
  "Every runtime hydration caller owns an explicit `dorg-model-load' require.
Ownership is explicit — no module relies on `dorg.el' or `dorg-render'
transitively loading the bridge.  `dorg-keywords' is deliberately excluded:
it reaches the loaders only through the lazy seam, never a static require."
  (let ((root (or (locate-dominating-file default-directory "dorg-dolt.el")
                  default-directory)))
    (dolist (module '("dorg-render" "dorg-history" "dorg-ql-dolt"
                      "dorg-mode" "dorg-mutate"))
      (let ((src (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name (concat module ".el") root))
                   (buffer-string))))
        (should (string-match-p (regexp-quote "(require 'dorg-model-load)") src))))))

;;; Dolt Version Floor

(ert-deftest dorg-test-dolt-version-parse ()
  "dorg-dolt--parse-version extracts (MAJOR MINOR PATCH) from output."
  (should (equal (dorg-dolt--parse-version "dolt version 1.86.1") '(1 86 1)))
  (should (equal (dorg-dolt--parse-version "dolt version 1.86.0") '(1 86 0)))
  (should (equal (dorg-dolt--parse-version "Dolt version 2.0.13 (release)") '(2 0 13)))
  ;; Noise / no numeric version
  (should-not (dorg-dolt--parse-version "dolt version unknown"))
  (should-not (dorg-dolt--parse-version "dolt version 1.86"))
  (should-not (dorg-dolt--parse-version ""))
  (should-not (dorg-dolt--parse-version nil)))

(ert-deftest dorg-test-dolt-version-compare ()
  "dorg-dolt--version-< orders (MAJOR MINOR PATCH) lists."
  (should (dorg-dolt--version-< '(1 85 9) '(1 86 0)))
  (should (dorg-dolt--version-< '(1 86 0) '(1 86 1)))
  (should (dorg-dolt--version-< '(0 99 99) '(1 0 0)))
  (should-not (dorg-dolt--version-< '(1 86 0) '(1 86 0)))
  (should-not (dorg-dolt--version-< '(1 86 1) '(1 86 0)))
  (should-not (dorg-dolt--version-< '(2 0 0) '(1 86 0))))

(ert-deftest dorg-test-dolt-minimum-version-string ()
  "dorg-dolt-minimum-version-string renders the floor as dotted text."
  (should (equal (dorg-dolt-minimum-version-string)
                 (mapconcat #'number-to-string dorg-dolt-minimum-version "."))))

(defmacro dorg-test--with-dolt-version (spec &rest body)
  "Run BODY with `dolt version' stubbed per SPEC and a clean cache.
SPEC is a plist: :exit EXIT-CODE (default 0) and :output STRING
written to the process buffer, or :signal MESSAGE to raise an
error instead of running the subprocess.  `executable-find' is
stubbed to report dolt present on PATH."
  (declare (indent 1))
  `(let ((dorg-dolt--version-cache 'unknown))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (cmd &rest _) (when (equal cmd "dolt") "/usr/bin/dolt")))
               ((symbol-function 'call-process)
                (lambda (_program _infile buffer _display &rest _args)
                  ,(if (plist-get spec :signal)
                       `(error "%s" ,(plist-get spec :signal))
                     `(progn
                        ;; `call-process' destination is `t' (current buffer)
                        ;; in `dorg-dolt--probe-version'.
                        (let ((target (cond ((bufferp buffer) buffer)
                                            ((eq buffer t) (current-buffer)))))
                          (when target
                            (with-current-buffer target
                              (insert ,(or (plist-get spec :output) "")))))
                        ,(or (plist-get spec :exit) 0))))))
       ,@body)))

(ert-deftest dorg-test-dolt-version-floor-excludes-binary-index-panic ()
  "DOrg refuses the last affected Dolt and accepts the first fixed release."
  (dorg-test--with-dolt-version (:output "dolt version 1.86.3")
    (should-not (dorg-dolt-supported-p))
    (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
      (should (string-match-p "1\\.86\\.3" (error-message-string err)))
      (should (string-match-p "1\\.86\\.4" (error-message-string err)))))
  (dorg-test--with-dolt-version (:output "dolt version 1.86.4")
    (should (dorg-dolt-supported-p))
    (should-not (dorg-dolt--ensure-available))))

(ert-deftest dorg-test-dolt-ensure-missing-binary ()
  "dorg-dolt--ensure-available reports install URL when dolt is absent."
  (let ((dorg-dolt--version-cache 'unknown))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
        (should (string-match-p "not found" (cadr err)))
        (should (string-match-p (regexp-quote (dorg-dolt-minimum-version-string))
                                (cadr err)))
        (should (string-match-p (regexp-quote dorg-dolt-install-url) (cadr err)))))))

(ert-deftest dorg-test-dolt-ensure-old-version ()
  "dorg-dolt--ensure-available rejects a too-old Dolt with detail."
  (dorg-test--with-dolt-version (:output "dolt version 1.50.0")
    (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
      ;; Detected version, path, and required minimum all present.
      (should (string-match-p "1\\.50\\.0" (cadr err)))
      (should (string-match-p "/usr/bin/dolt" (cadr err)))
      (should (string-match-p (regexp-quote (dorg-dolt-minimum-version-string))
                              (cadr err))))
    (should-not (dorg-dolt-supported-p))))

(ert-deftest dorg-test-dolt-ensure-supported-version ()
  "dorg-dolt--ensure-available accepts a version at/above the floor."
  (dorg-test--with-dolt-version (:output "dolt version 1.86.5")
    (should-not (dorg-dolt--ensure-available))
    (should (dorg-dolt-supported-p)))
  ;; Exactly at the floor is supported.
  (dorg-test--with-dolt-version
      (:output (format "dolt version %s" (dorg-dolt-minimum-version-string)))
    (should (dorg-dolt-supported-p))))

(ert-deftest dorg-test-dolt-ensure-version-command-fails ()
  "dorg-dolt--ensure-available reports a non-zero `dolt version' exit."
  (dorg-test--with-dolt-version (:exit 1 :output "boom")
    (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
      (should (string-match-p "Could not determine Dolt version" (cadr err)))
      (should (string-match-p "boom" (cadr err)))
      (should (string-match-p (regexp-quote (dorg-dolt-minimum-version-string))
                              (cadr err))))
    (should-not (dorg-dolt-supported-p))))

(ert-deftest dorg-test-dolt-ensure-unparseable-version ()
  "dorg-dolt--ensure-available reports unparseable version output."
  (dorg-test--with-dolt-version (:output "dolt version unknown-build")
    (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
      (should (string-match-p "Could not determine Dolt version" (cadr err)))
      (should (string-match-p "unknown-build" (cadr err))))
    (should-not (dorg-dolt-supported-p))))

(ert-deftest dorg-test-dolt-ensure-version-probe-signals ()
  "dorg-dolt--ensure-available surfaces a failure to spawn `dolt version'."
  (dorg-test--with-dolt-version (:signal "no such file")
    (let ((err (should-error (dorg-dolt--ensure-available) :type 'error)))
      (should (string-match-p "Could not determine Dolt version" (cadr err))))))

(ert-deftest dorg-test-dolt-version-cache-resettable ()
  "The version probe caches its result and resets on demand."
  (let ((dorg-dolt--version-cache 'unknown)
        (calls 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/usr/bin/dolt"))
              ((symbol-function 'call-process)
               (lambda (_p _i buffer _d &rest _a)
                 (cl-incf calls)
                 (let ((target (cond ((bufferp buffer) buffer)
                                     ((eq buffer t) (current-buffer)))))
                   (when target
                     (with-current-buffer target (insert "dolt version 1.86.4"))))
                 0)))
      (should (dorg-dolt-supported-p))
      (should (dorg-dolt-supported-p))
      ;; Second call served from cache — no extra subprocess.
      (should (= calls 1))
      (dorg-dolt-reset-version-cache)
      (should (dorg-dolt-supported-p))
      (should (= calls 2)))))

(ert-deftest dorg-test-dolt-trim-output ()
  "dorg-dolt--trim-output normalizes empty output and tail-limits long output."
  (should (equal (dorg-dolt--trim-output "") "(no output)"))
  (should (equal (dorg-dolt--trim-output "   \n  ") "(no output)"))
  (should (equal (dorg-dolt--trim-output nil) "(no output)"))
  (should (equal (dorg-dolt--trim-output "  hello\nworld  ") "hello\nworld"))
  (let* ((lines (mapcar #'number-to-string (number-sequence 1 100)))
         (trimmed (dorg-dolt--trim-output (mapconcat #'identity lines "\n") 10)))
    (should (string-prefix-p "...\n" trimmed))
    (should (string-suffix-p "100" trimmed))
    ;; Marker line plus the last 10 numbers.
    (should (= (length (split-string trimmed "\n")) 11))))

;;; Init Tests

(ert-deftest dorg-test-init-failure-includes-dolt-output ()
  "dorg-init surfaces the dolt init exit code, directory, and stderr."
  (let ((tmp (make-temp-file "dorg-init-fail-" t)))
    (unwind-protect
        (let ((dolt-dir (expand-file-name "myrepo" tmp))
              (dorg-dolt--version-cache (list :version dorg-dolt-minimum-version
                                              :raw "stubbed")))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _) "/usr/bin/dolt"))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile buffer _display &rest args)
                       ;; Only intercept `dolt init'; write fake stderr and fail.
                       (when (member "init" args)
                         (when (bufferp buffer)
                           (with-current-buffer buffer
                             (insert "fatal: could not initialize dolt repo")))
                         1))))
            (let ((err (should-error (dorg-init dolt-dir) :type 'error)))
              (should (string-match-p "dolt init failed" (cadr err)))
              (should (string-match-p "exit 1" (cadr err)))
              (should (string-match-p (regexp-quote (expand-file-name dolt-dir))
                                      (cadr err)))
              (should (string-match-p "could not initialize dolt repo"
                                      (cadr err))))))
      (delete-directory tmp t))))

(ert-deftest dorg-test-init-failure-handles-signal-status ()
  "dorg-init enriches diagnostics when `dolt init' is killed by a signal.
`call-process' returns a string status in that case, which must not raise a
Lisp type error in place of the actionable error."
  (let ((tmp (make-temp-file "dorg-init-signal-" t)))
    (unwind-protect
        (let ((dolt-dir (expand-file-name "myrepo" tmp))
              (dorg-dolt--version-cache (list :version dorg-dolt-minimum-version
                                              :raw "stubbed")))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _) "/usr/bin/dolt"))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile buffer _display &rest args)
                       ;; Simulate a killed `dolt init': string signal status.
                       (when (member "init" args)
                         (when (bufferp buffer)
                           (with-current-buffer buffer
                             (insert "interrupted")))
                         "Killed"))))
            (let ((err (should-error (dorg-init dolt-dir) :type 'error)))
              (should (string-match-p "dolt init failed" (cadr err)))
              (should (string-match-p "exit Killed" (cadr err)))
              (should (string-match-p (regexp-quote (expand-file-name dolt-dir))
                                      (cadr err)))
              (should (string-match-p "interrupted" (cadr err))))))
      (delete-directory tmp t))))

(ert-deftest dorg-test-init-creates-repo ()
  "dorg-init creates a Dolt repo with schema applied."
  (dorg-test-with-dolt
    (should (file-exists-p (expand-file-name ".dolt" dolt-dir)))
    ;; Verify schema was applied by querying metadata
    (let ((result (dorg-sql dolt-dir
                            "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
      (should result)
      (should (equal (alist-get 'value (car result))
                     (number-to-string dorg-schema-version))))))

(ert-deftest dorg-test-init-rejects-existing ()
  "dorg-init signals error if repo already exists."
  (dorg-test-with-dolt
    (should-error (dorg-init dolt-dir) :type 'error)))

(ert-deftest dorg-test-init-creates-all-tables ()
  "dorg-init creates all expected tables."
  (dorg-test-with-dolt
    (let* ((result (dorg-sql dolt-dir "SELECT table_name FROM information_schema.tables WHERE table_schema = database() AND table_type = 'BASE TABLE'"))
           (tables (mapcar (lambda (row) (alist-get 'TABLE_NAME row)) result)))
      (dolist (expected '("dorg_meta" "dorg_files" "headings" "heading_tags"
                          "heading_items" "text_items" "keyword_items"
                          "planning_items" "drawer_items" "drawer_lines"
                          "property_entries"))
        (should (member expected tables))))))

(ert-deftest dorg-test-init-end-to-end ()
  "dorg-init on a fresh directory creates repo, schema, meta, and commit."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((tmp (make-temp-file "dorg-init-e2e-" t)))
      (unwind-protect
          (let ((dolt-dir (expand-file-name "myrepo" tmp)))
            ;; 1. Call dorg-init on a non-existent subdirectory
            (dorg-init dolt-dir)
            ;; 2. .dolt directory created
            (should (file-directory-p (expand-file-name ".dolt" dolt-dir)))
            ;; 3. All expected tables present
            (let* ((result (dorg-sql dolt-dir
                            "SELECT table_name FROM information_schema.tables WHERE table_schema = database() AND table_type = 'BASE TABLE'"))
                   (tables (mapcar (lambda (row) (alist-get 'TABLE_NAME row)) result)))
              (dolist (expected '("dorg_meta" "dorg_files" "headings" "heading_tags"
                                  "heading_items" "text_items" "keyword_items"
                                  "planning_items" "drawer_items" "drawer_lines"
                                  "property_entries"))
                (should (member expected tables))))
            ;; 4. schema_version in dorg_meta
            (let ((meta (dorg-sql dolt-dir
                          "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
              (should meta)
              (should (equal (alist-get 'value (car meta))
                             (number-to-string dorg-schema-version))))
            ;; 5. At least one commit exists
            (should (dorg-head-commit dolt-dir)))
        (delete-directory tmp t)))))

;;; SQL Tests

(ert-deftest dorg-test-sql-select ()
  "dorg-sql returns parsed results for SELECT queries."
  (dorg-test-with-dolt
    (dorg-sql dolt-dir
              "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, NULL)")
    (dorg-commit dolt-dir "test insert")
    (let ((result (dorg-sql dolt-dir "SELECT id, level_offset FROM headings WHERE id = 1")))
      (should (= (length result) 1))
      (should (= (alist-get 'id (car result)) 1))
      (should (= (alist-get 'level_offset (car result)) 0)))))

(ert-deftest dorg-test-sql-error-signals ()
  "dorg-sql signals error on invalid SQL."
  (dorg-test-with-dolt
    (should-error (dorg-sql dolt-dir "SELECT * FROM nonexistent_table")
                  :type 'error)))

(ert-deftest dorg-test-sql-with-format-args ()
  "dorg-sql supports format arguments."
  (dorg-test-with-dolt
    (dorg-sql dolt-dir
              "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, NULL)")
    (let ((result (dorg-sql dolt-dir "SELECT id FROM headings WHERE level_offset = %d" 0)))
      (should (= (length result) 1)))))

;;; Batch SQL Tests

(ert-deftest dorg-test-sql-batch-writes ()
  "dorg-sql-batch executes multiple statements."
  (dorg-test-with-dolt
    (dorg-sql-batch dolt-dir
                    (list "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, NULL)"
                          "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (2, 1, 1, X'01', 1, 'Heading 1')"))
    (let ((result (dorg-sql dolt-dir "SELECT COUNT(*) as cnt FROM headings")))
      (should (= (alist-get 'cnt (car result)) 2)))))

;;; Commit Tests

(ert-deftest dorg-test-commit-clean-repo ()
  "dorg-commit on clean repo does not error."
  (dorg-test-with-dolt
    ;; Should not error even with nothing to commit
    (dorg-commit dolt-dir "no-op commit")))

(ert-deftest dorg-test-commit-after-write ()
  "dorg-commit after writes creates a commit."
  (dorg-test-with-dolt
    (dorg-sql dolt-dir
              "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, 'Test')")
    (dorg-commit dolt-dir "add heading")
    ;; Verify the data persists after commit
    (let ((result (dorg-sql dolt-dir "SELECT title FROM headings WHERE id = 1")))
      (should (equal (alist-get 'title (car result)) "Test")))))

;;; Head Commit

(ert-deftest dorg-test-head-commit ()
  "dorg-head-commit returns a non-empty hash string."
  (dorg-test-with-dolt
    (let ((hash (dorg-head-commit dolt-dir)))
      (should (stringp hash))
      (should (> (length hash) 0))
      ;; Dolt commit hashes are 32-char hex
      (should (string-match-p "\\`[0-9a-v]\\{32\\}\\'" hash)))))

(ert-deftest dorg-test-head-commit-changes-after-commit ()
  "dorg-head-commit returns a different hash after a new commit."
  (dorg-test-with-dolt
    (let ((hash1 (dorg-head-commit dolt-dir)))
      (dorg-sql dolt-dir
                "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, 'Test')")
      (dorg-commit dolt-dir "add heading")
      (let ((hash2 (dorg-head-commit dolt-dir)))
        (should (stringp hash2))
        (should-not (equal hash1 hash2))))))

;;; SQL String Helpers

(ert-deftest dorg-test-sql-escape-string ()
  "dorg-sql-escape-string handles quotes, backslashes, and passthrough."
  (should (equal (dorg-sql-escape-string "it's") "it''s"))
  (should (equal (dorg-sql-escape-string "a\\b") "a\\\\b"))
  (should (equal (dorg-sql-escape-string "normal") "normal"))
  (should (equal (dorg-sql-escape-string "") ""))
  (should (equal (dorg-sql-escape-string "line1\nline2") "line1\nline2"))
  (should (equal (dorg-sql-escape-string "a'b\\c'd") "a''b\\\\c''d")))

(ert-deftest dorg-test-sql-quote ()
  "dorg-sql-quote wraps in quotes or returns NULL."
  (should (equal (dorg-sql-quote "hello") "'hello'"))
  (should (equal (dorg-sql-quote nil) "NULL"))
  (should (equal (dorg-sql-quote "it's") "'it''s'"))
  (should (equal (dorg-sql-quote "") "''"))
  (should (equal (dorg-sql-quote "a\\b") "'a\\\\b'")))

;;; Error Handling

(defmacro dorg-test-sql--with-fake-cli (output exit &rest body)
  "Run BODY with `dorg-sql--cli-exec's bounded async dolt child faked.
Captures the spawned `:command' list in the lexical var CMD and any
stdin streamed to the child in STDIN, writes OUTPUT into the process
buffer, and simulates a child that has already exited with EXIT.  This
mirrors the bounded `make-process' transport for tests that
assert the argv-vs-stdin ARG_MAX contract, the way the previous
`call-process' fakes mirrored the synchronous transport.  The
`native-comp-*' bindings keep the primitive redefinitions honored."
  (declare (indent 2))
  `(let ((cmd nil) (stdin nil)
         (native-comp-jit-compilation nil)
         (native-comp-enable-subr-trampolines nil)
         (orig-live (symbol-function 'process-live-p))
         (orig-exit (symbol-function 'process-exit-status)))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq cmd (plist-get args :command))
                  (let ((b (plist-get args :buffer)))
                    (when (bufferp b)
                      (with-current-buffer b (insert ,output))))
                  'dorg-test-fake-proc))
               ((symbol-function 'process-send-string)
                (lambda (_p s) (setq stdin (concat stdin s))))
               ((symbol-function 'process-send-eof) (lambda (&rest _) nil))
               ((symbol-function 'process-live-p)
                (lambda (p) (unless (eq p 'dorg-test-fake-proc)
                              (funcall orig-live p))))
               ((symbol-function 'process-exit-status)
                (lambda (p) (if (eq p 'dorg-test-fake-proc) ,exit
                              (funcall orig-exit p)))))
       ,@body)))

(ert-deftest dorg-test-sql-malformed-json-signals ()
  "dorg-sql signals dorg-dolt-query-error on unparseable JSON output."
  (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
            ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) nil)))
    (dorg-test-sql--with-fake-cli "NOT VALID JSON {{" 0
      (should-error
       (dorg-sql--with-cli-seam "/tmp/fake-dolt-dir"
         (dorg-sql "/tmp/fake-dolt-dir" "SELECT 1"))
       :type 'dorg-dolt-query-error))))

(ert-deftest dorg-test-sql-parse-result-malformed-signals ()
  "Parse failure on both fast and fallback paths signals query-error.
The raw output is included for diagnosis."
  (let ((err (should-error
              (with-temp-buffer
                (insert "NOT VALID JSON {{")
                (dorg-sql--parse-result (current-buffer) 0 "SELECT 1"))
              :type 'dorg-dolt-query-error)))
    ;; cadr of the signal data carries the diagnostic message + raw output.
    (should (string-match-p "NOT VALID JSON" (cadr err)))))

;;; JSON result parsing — braces-in-strings regression

(defun dorg-test-sql--last-json-object-backward (str)
  "Reimplementation of the pre-fix backward brace-scan extractor.
Counts `}' (depth++) and `{' (depth--) right-to-left with NO JSON
string or escape awareness, exactly as `dorg-sql--last-json-object'
did before the brace-aware fix.  Used only to demonstrate that the old
extractor truncates braces-in-string payloads (before != after)."
  (let ((end (string-match-p "}[[:space:]]*\\'" str)))
    (if end
        (let ((depth 0) (i end) (start nil))
          (while (and (>= i 0) (null start))
            (pcase (aref str i)
              (?\} (setq depth (1+ depth)))
              (?\{ (setq depth (1- depth))
                   (when (= depth 0) (setq start i))))
            (setq i (1- i)))
          (if start (substring str start (1+ end)) str))
      str)))

(ert-deftest dorg-test-sql-parse-result-braces-in-strings ()
  "Whole valid Dolt JSON parses despite braces/escapes inside strings.
Covers literal unbalanced braces, escaped quotes, escaped backslashes,
and `${VAR}'-style text in a single-object {\"rows\":...} payload — the
multi-megabyte materialize regression.  Also proves the OLD backward
brace-scan would have truncated this same input (before != after) and
produced unparseable JSON, so the test is not vacuous."
  (let ((output
         (concat "{\"rows\":[{\"id\":1,"
                 "\"text\":\"code {ids,capabilities,audit,redact}.ts "
                 "${VAR} \\\"q\\\" back\\\\slash {{{\"}]}")))
    ;; New parser: the whole trimmed buffer parses; the row survives intact.
    (let ((rows (with-temp-buffer
                  (insert output)
                  (dorg-sql--parse-result (current-buffer) 0 "SELECT ..."))))
      (should (= (length rows) 1))
      (should (= (alist-get 'id (car rows)) 1))
      (should (equal (alist-get 'text (car rows))
                     "code {ids,capabilities,audit,redact}.ts ${VAR} \"q\" back\\slash {{{")))
    ;; Old behavior: the backward `}'-anchored scan lands on a `{' inside
    ;; the string value and returns a different, truncated substring that
    ;; fails to parse — the exact source of the materialize failure.
    (let ((old (dorg-test-sql--last-json-object-backward output)))
      (should-not (equal old output))
      (should-error (json-parse-string old) :type 'json-parse-error))))

(ert-deftest dorg-test-sql-parse-result-legacy-multi-object-fallback ()
  "Legacy multi-object/progress output still yields the FINAL rows object.
Full-buffer parse fails (trailing content after the first object), so
the string/escape-aware fallback extracts the last {\"rows\":...} object.
An earlier object whose string contains stray braces must not confuse
the scan."
  (let ((output (concat "Processed 50.0% of the file\n"
                        "{\"rows\":[{\"t\":\"earlier {brace} ${X}\"}]}\n"
                        "{\"rows\":[{\"n\":7}]}")))
    (let ((rows (with-temp-buffer
                  (insert output)
                  (dorg-sql--parse-result (current-buffer) 0 "BATCH"))))
      (should (= (length rows) 1))
      ;; The fallback keeps the writer's JSON number.
      (should (eql 7 (alist-get 'n (car rows)))))))

(ert-deftest dorg-test-sql-last-json-object-string-escape-aware ()
  "The fallback extractor ignores braces/quotes inside JSON strings.
Forward scan with string + escape state returns the final top-level
object even when an earlier object's string value contains unbalanced
braces and escaped quotes."
  (should (equal (dorg-sql--last-json-object
                  (concat "{\"a\":\"{{ unbalanced \\\" }} ${X}\"}\n"
                          "{\"rows\":[{\"n\":1}]}"))
                 "{\"rows\":[{\"n\":1}]}"))
  ;; No complete top-level object -> return STR unchanged so the caller
  ;; surfaces a parse error on the raw output.
  (should (equal (dorg-sql--last-json-object "no json here {{{")
                 "no json here {{{")))

;;; Large CLI SQL transport (ARG_MAX avoidance)

(ert-deftest dorg-test-sql-large-query-uses-stdin-not-argv ()
  "Large CLI SQL streams over stdin; the body is never a -q argv.
Proves `dorg-sql--cli-exec' switches transport at the byte-length
threshold without depending on the host's actual ARG_MAX."
  (let ((big (concat "SELECT " (make-string 4096 ?1) " AS n")))
    (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) nil))
              (dorg-sql-cli-arg-max-threshold 64))
      (dorg-test-sql--with-fake-cli "{\"rows\":[]}" 0
        (dorg-sql--with-cli-seam "/tmp/fake-dolt-dir"
          (dorg-sql "/tmp/fake-dolt-dir" big))
        ;; The SQL body was delivered over stdin, not as an argument.
        (should (equal stdin big))
        (should (member "--doltcfg-dir" cmd))
        (should (equal (cadr (member "--doltcfg-dir" cmd))
                       (expand-file-name ".doltcfg" "/tmp/fake-dolt-dir/")))
        (should (member "sql" cmd))
        (should-not (member "-q" cmd))
        ;; The oversized body never became a process argument.
        (should-not (cl-find big cmd :test #'equal))))))

(ert-deftest dorg-test-sql-small-query-uses-argv ()
  "Short CLI SQL keeps the existing -q argv transport unchanged."
  (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
            ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) nil)))
    (dorg-test-sql--with-fake-cli "{\"rows\":[]}" 0
      (dorg-sql--with-cli-seam "/tmp/fake-dolt-dir"
        (dorg-sql "/tmp/fake-dolt-dir" "SELECT 1"))
      ;; Short body: -q argv, nothing streamed over stdin.
      (should-not stdin)
      (should (member "--doltcfg-dir" cmd))
      (should (equal (cadr (member "--doltcfg-dir" cmd))
                     (expand-file-name ".doltcfg" "/tmp/fake-dolt-dir/")))
      (should (member "-q" cmd))
      (should (member "SELECT 1" cmd)))))

(ert-deftest dorg-test-sql-large-query-parses-with-progress-text ()
  "Result parsing tolerates Dolt progress lines on the large path.
The backward `}' scan extracts the final JSON object even when
file/stdin execution prepends \"Processed N%\" progress noise."
  (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
            ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) nil))
            (dorg-sql-cli-arg-max-threshold 1))
    (dorg-test-sql--with-fake-cli
        "Processed 50.0% of the file\n{\"rows\":[{\"n\":7}]}" 0
      (let ((rows (dorg-sql--with-cli-seam "/tmp/fake-dolt-dir"
                    (dorg-sql "/tmp/fake-dolt-dir" "SELECT 7 AS n"))))
        (should (equal (alist-get 'n (car rows)) 7))))))

(ert-deftest dorg-test-sql-large-query-creates-no-temp-files ()
  "The large stdin transport allocates no temp files to clean up.
Guards against a regression to a `--file' temp-file approach, which
would also emit stderr progress text into the parse buffer."
  (let ((temp-calls 0))
    (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) nil))
              (dorg-sql-cli-arg-max-threshold 1)
              ((symbol-function 'make-temp-file)
               (lambda (&rest _) (cl-incf temp-calls) "/tmp/dorg-should-not-exist"))
              ((symbol-function 'make-temp-name)
               (lambda (&rest _) (cl-incf temp-calls) "/tmp/dorg-should-not-exist")))
      (dorg-test-sql--with-fake-cli "{\"rows\":[]}" 0
        (dorg-sql--with-cli-seam "/tmp/fake-dolt-dir"
          (dorg-sql "/tmp/fake-dolt-dir" "SELECT 1"))
        (should (= temp-calls 0))))))

;;; Bounded one-shot CLI (no orphaned `dolt sql' child)

(ert-deftest dorg-test-sql-cli-exec-bounded-and-reaps-child ()
  "A one-shot `dolt sql' that hangs is bounded by `dorg-sql-cli-timeout'
and its child process is reaped, never left orphaned.

Regression for the pre-start deadlock: a
one-shot reader deadlocked on a live server's Dolt write lock, tracker-cli's
outer 12s guard killed tracker-cli, and a `dolt sql' child was orphaned.  A fake
`dolt' that `exec's a long `sleep' stands in for the deadlocked reader.

Non-vacuous: against the unbounded `call-process' implementation this
blocks for the full sleep and signals no `dorg-sql-transport-error', so
the `should-error' fails."
  (skip-unless (executable-find "sh"))
  (let* ((bin (make-temp-file "dorg-fakedolt-" t))
         (repo (make-temp-file "dorg-fakerepo-" t))
         (dolt (expand-file-name "dolt" bin))
         (exec-path (cons bin exec-path))
         (process-environment
          (cons (concat "PATH=" bin path-separator (getenv "PATH"))
                process-environment))
         (dorg-sql-cli-timeout 0.5)
         (target (dorg-dolt--target repo))
         (pre (length (process-list))))
    (unwind-protect
        (progn
          (with-temp-file dolt (insert "#!/bin/sh\nexec sleep 30\n"))
          (set-file-modes dolt #o755)
          (let ((start (float-time)))
            ;; Bounded: signals a transport error at ~the timeout, never
            ;; blocks for the full 30s sleep.
            (should-error (dorg-sql--cli-exec "SELECT 1" target)
                          :type 'dorg-sql-transport-error)
            (should (< (- (float-time) start) 5.0)))
          ;; No orphan: the live-process count returns to baseline.
          (let ((deadline (+ (float-time) 3.0)))
            (while (and (> (length (process-list)) pre)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (= (length (process-list)) pre)))
      (delete-directory bin t)
      (delete-directory repo t))))

(ert-deftest dorg-test-sql-batch-large-stdin-transport ()
  "Real-Dolt batch forced onto the stdin path inserts, commits, queries.
Binding the threshold to 1 routes every statement through stdin,
exercising the large-transport insert/commit/select end-to-end against
a real Dolt repo."
  (dorg-test-with-dolt
    (let ((dorg-sql-cli-arg-max-threshold 1))
      (dorg-sql-batch dolt-dir
                      (list "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, NULL)"
                            "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (2, 1, 1, X'01', 1, 'Heading 1')")
                      "large batch via stdin")
      (let ((rows (dorg-sql dolt-dir "SELECT COUNT(*) AS cnt FROM headings")))
        (should (= (alist-get 'cnt (car rows)) 2))))))

(ert-deftest dorg-test-commit-add-failure-signals ()
  "dorg-commit signals when DOLT_ADD fails."
  (cl-letf (((symbol-function 'dorg-sql)
             (lambda (_dir query &rest _args)
               (when (string-match-p "DOLT_ADD" query)
                 (error "DOLT_ADD failed")))))
    (should-error
     (dorg-commit "/tmp/fake-dolt-dir" "test commit")
     :type 'error)))

;;; Schema Lifecycle Tests

(defun dorg-test--table-exists-p (dolt-dir table)
  "Return non-nil when TABLE exists in DOLT-DIR's current database."
  (let ((rows (dorg-sql
               dolt-dir
               (format
                (concat "SELECT COUNT(*) AS n FROM information_schema.tables "
                        "WHERE table_schema = DATABASE() AND table_name = %s")
                (dorg-sql-quote table)))))
    (> (alist-get 'n (car rows)) 0)))

(ert-deftest dorg-test-schema-v7-fresh-omits-legacy-conflicts ()
  "Fresh schema (v7 onward) has no durable conflict table."
  (dorg-test-with-dolt
    (should (= 8 dorg-schema-version))
    (should (equal "8"
                   (alist-get
                    'value
                    (car (dorg-sql
                          dolt-dir
                          "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))))
    (should-not (dorg-test--table-exists-p dolt-dir "dorg_conflicts"))))

(ert-deftest dorg-test-ensure-schema-noop-when-current ()
  "dorg-repo-ensure-schema is a no-op when repo is already at current version."
  (dorg-test-with-dolt
    ;; Fresh repo is already at current version
    (dorg-repo-ensure-schema dolt-dir)
    (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                    "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                   (number-to-string dorg-schema-version)))))

(ert-deftest dorg-test-ensure-schema-errors-on-future-version ()
  "dorg-repo-ensure-schema errors when repo version exceeds code version."
  (dorg-test-with-dolt
    ;; Set version to something ahead of code
    (dorg-sql dolt-dir (format "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ dorg-schema-version)))
    (dorg-commit dolt-dir "bump to future version for test")
    (should-error (dorg-repo-ensure-schema dolt-dir) :type 'error)))

(ert-deftest dorg-test-migrate-schema-core-migration-rolls-back-on-failure ()
  "A failing explicit core migration resets partial DDL and leaves schema_version unchanged.
Uses a synthetic let-bound migration from the current version to
current+1 whose first statement adds a column — implicitly committed into
the Dolt working set under Dolt's MySQL DDL semantics — and whose second
statement fails before the `schema_version' bump.  The managed migration
step helper must hard-reset that partial DDL and re-signal,
leaving the recorded version unchanged and no orphaned column.  Real
schema constants are not bumped; both are let-bound for the test only."
  (dorg-test-with-dolt
    (let* ((current dorg-schema-version)
           (dorg-schema-version (1+ current))
           (dorg-schema-migrations
            (list (cons current
                        (list "ALTER TABLE headings ADD COLUMN dj8h_marker INT"
                              "INSERT INTO dj8h_nonexistent (x) VALUES (1)"
                              (format
                               "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ current)))))))
      (should-error (dorg-migrate-schema dolt-dir) :type 'error)
      ;; schema_version stayed at the original current — the bump never committed.
      (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                     (number-to-string current)))
      ;; The partial DDL was hard-reset: the synthetic column is gone, so a
      ;; re-run could ALTER cleanly rather than failing on a duplicate column.
      (should-error (dorg-sql dolt-dir "SELECT dj8h_marker FROM headings") :type 'error)
      ;; The working set is clean again — nothing left drifting.
      (should (dorg-repo--working-set-clean-p dolt-dir)))))

(ert-deftest dorg-test-migrate-schema-core-migration-rejects-dirty-working-set ()
  "An explicit core migration is rejected when the Dolt working set is dirty.
The clean-working-set preflight must reject before any migration SQL
runs, preserving the pre-existing uncommitted change and applying no
synthetic DDL.  Without this guard the failure-path `DOLT_RESET --hard'
could erase unrelated uncommitted work.  Schema constants are let-bound
for the test only."
  (dorg-test-with-dolt
    (let* ((current dorg-schema-version)
           (dorg-schema-version (1+ current))
           (dorg-schema-migrations
            (list (cons current
                        (list "ALTER TABLE headings ADD COLUMN dj8h_marker INT"
                              (format
                               "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ current)))))))
      ;; Pre-existing uncommitted working-set change, unrelated to the migration.
      (dorg-sql dolt-dir
                "INSERT INTO dorg_meta (`key`, value) VALUES ('dj8h_marker', 'pre')")
      (should-error (dorg-migrate-schema dolt-dir) :type 'error)
      ;; schema_version unchanged — no migration step ran.
      (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                     (number-to-string current)))
      ;; Migration DDL never touched the schema.
      (should-error (dorg-sql dolt-dir "SELECT dj8h_marker FROM headings") :type 'error)
      ;; The pre-existing uncommitted row survives — it was not hard-reset away.
      (should (equal "pre" (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'dj8h_marker'"))))))))

;;; Schema-version skew hardening

(ert-deftest dorg-test-schema-source-location-path-free-by-default ()
  "`dorg-schema--source-location' names the version WITHOUT a local path.
Public version-skew diagnostics must not leak a local checkout path by
default; the absolute source file is operator detail behind an explicit
WITH-PATH argument or `dorg-schema-diagnostics-show-source-path'."
  (let ((lib (locate-library "dorg-schema")))
    ;; Default: the version is named, but no absolute source path leaks.
    (let ((dorg-schema-diagnostics-show-source-path nil))
      (let ((loc (dorg-schema--source-location)))
        (should (string-match-p (format "v%d" dorg-schema-version) loc))
        (should-not (string-search "/" loc))
        (when lib (should-not (string-search lib loc)))))
    ;; Operator detail: an explicit arg restores the source path ...
    (when lib
      (should (string-search lib (dorg-schema--source-location t)))
      ;; ... and so does the defcustom.
      (let ((dorg-schema-diagnostics-show-source-path t))
        (should (string-search lib (dorg-schema--source-location)))))))

(ert-deftest dorg-test-check-schema-compat-current-and-needs-migration ()
  "The read-only preflight classifies current vs older repos without mutating.
A fresh repo is `:current'; lowering only the recorded version makes it
`:needs-migration' — and the preflight itself performs no migration."
  (dorg-test-with-dolt
    (should (eq (dorg-repo-check-schema-compat dolt-dir) :current))
    (dorg-sql dolt-dir
              (format "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                      (1- dorg-schema-version)))
    (dorg-commit dolt-dir "downgrade recorded version for preflight test")
    (should (eq (dorg-repo-check-schema-compat dolt-dir) :needs-migration))
    ;; Preflight did not migrate: the recorded version is still the lowered one.
    (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                    "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                   (number-to-string (1- dorg-schema-version))))))

(ert-deftest dorg-test-check-schema-compat-signals-on-newer-repo ()
  "A repo NEWER than this dorg signals an actionable `dorg-schema-repo-newer'.
The message must name the repo path, the newer version, this dorg's
supported version, and the path-free schema version identifier so the
operator knows which install to upgrade — without leaking a local source
path by default — and the check must not mutate the repo."
  (dorg-test-with-dolt
    (dorg-sql dolt-dir
              (format "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                      (1+ dorg-schema-version)))
    (dorg-commit dolt-dir "bump to future version for test")
    (should-error (dorg-repo-check-schema-compat dolt-dir)
                  :type 'dorg-schema-repo-newer)
    (let ((msg (condition-case err
                   (progn (dorg-repo-check-schema-compat dolt-dir) "")
                 (dorg-schema-repo-newer (error-message-string err)))))
      (should (string-match-p (regexp-quote dolt-dir) msg))
      (should (string-match-p (number-to-string (1+ dorg-schema-version)) msg))
      (should (string-match-p (number-to-string dorg-schema-version) msg))
      ;; Path-free version identifier embedded verbatim — identifies the
      ;; stale install without leaking a local source path by default.
      (should (string-match-p (regexp-quote (dorg-schema--source-location)) msg))
      (let ((lib (locate-library "dorg-schema")))
        (when lib (should-not (string-match-p (regexp-quote lib) msg)))))
    ;; Read-only: recorded version unchanged after the signal.
    (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                    "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                   (number-to-string (1+ dorg-schema-version))))))

(ert-deftest dorg-test-ensure-schema-refuses-older-repo-without-write ()
  "Ordinary schema ensure refuses an older repo without migrating.
The obsolete `dorg-schema-auto-migrate' variable must not authorize writes.
Uses a synthetic let-bound migration so the test is independent of real schema
internals."
  (dorg-test-with-dolt
    (let* ((current dorg-schema-version)
           (dorg-schema-version (1+ current))
           (dorg-schema-migrations
            (list (cons current
                        (list "ALTER TABLE headings ADD COLUMN nf7q_gate INT NULL"
                              (format
                               "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ current)))))))
      (let ((dorg-schema-auto-migrate nil))
        (should-error (dorg-repo-ensure-schema dolt-dir)
                      :type 'dorg-schema-migration-required))
      ;; The gate ran before any migration SQL: synthetic column never added.
      (should-error (dorg-sql dolt-dir "SELECT nf7q_gate FROM headings") :type 'error)
      ;; Recorded version unchanged.
      (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                     (number-to-string current))))))

(ert-deftest dorg-test-ensure-schema-auto-migrate-obsolete-refuses ()
  "The obsolete auto-migrate variable does not restore ordinary migration."
  (dorg-test-with-dolt
    (let* ((current dorg-schema-version)
           (dorg-schema-version (1+ current))
           (dorg-schema-migrations
            (list (cons current
                        (list "ALTER TABLE headings ADD COLUMN nf7q_announce INT NULL"
                               (format
                               "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ current))))))
           (warnings '()))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &optional _level &rest _)
                   (push message warnings))))
        (let ((dorg-schema-auto-migrate t))
          (should-error (dorg-repo-ensure-schema dolt-dir)
                        :type 'dorg-schema-migration-required))
        (should-not warnings))
      ;; Ordinary ensure did not migrate.
      (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                     (number-to-string current)))
      (should-error (dorg-sql dolt-dir "SELECT nf7q_announce FROM headings LIMIT 1")
                    :type 'error))))

(ert-deftest dorg-test-migrate-schema-overrides-gate ()
  "`dorg-migrate-schema' migrates even when `dorg-schema-auto-migrate' is nil.
This is the explicit opt-in lever: a deployment can keep auto-migration
off yet upgrade deliberately.  Uses a synthetic let-bound migration."
  (dorg-test-with-dolt
    (let* ((current dorg-schema-version)
           (dorg-schema-version (1+ current))
           (dorg-schema-migrations
            (list (cons current
                        (list "ALTER TABLE headings ADD COLUMN nf7q_optin INT NULL"
                              (format
                               "UPDATE dorg_meta SET value = '%d' WHERE `key` = 'schema_version'"
                               (1+ current)))))))
      (let ((dorg-schema-auto-migrate nil))
        (dorg-migrate-schema dolt-dir))
      (should (equal (alist-get 'value (car (dorg-sql dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
                     (number-to-string (1+ current))))
      (dorg-sql dolt-dir "SELECT nf7q_optin FROM headings LIMIT 1"))))

;;; SQL result typing — the CLI parser keeps the JSON writer's types

(defconst dorg-test-sql--pinned-cli-json
  (concat
   "{\"rows\": [{\"binary_hex\":\"007FFF\",\"binary_raw\":\"\\u0000\\ufffd\","
   "\"decimal_exact\":\"1.250\",\"empty_text\":\"\","
   "\"newline_text\":\"line\\nbreak\",\"nul_text\":\"a\\u0000b\","
   "\"signed_max\":9223372036854775807,\"signed_over_2p53\":9007199254740993,"
   "\"sort_key_hex\":\"697473\",\"tag\":\"007\",\"text_numeric\":\"007\","
   "\"title\":\"007\",\"unsigned_max\":18446744073709551615,"
   "\"utf8_text\":\"caf\u00e9 \u2615\",\"value_raw\":\"1.250\","
   "\"varchar_numeric\":\"-01\"}]}")
  "One row of raw CLI JSON as the pinned Dolt writer emits it.
SQL numbers are JSON numbers and text and DECIMAL are JSON strings.  The SQL
NULL column `null_text' is omitted, and VARBINARY X'007FFF' arrives as
U+0000 U+FFFD, so a byte-exact reader selects HEX() instead.")

(ert-deftest dorg-test-sql-parse-result-keeps-pinned-json-types ()
  "The CLI parser returns exactly the types the pinned JSON writer emitted.
JSON numbers stay integers past 2^53 and through the unsigned 64-bit maximum;
JSON strings stay byte-exact however numeric they look, whatever their
column name.  The writer's own limits stay explicit: NULL is an absent
member and raw binary is not byte-exact."
  (let ((row (car (with-temp-buffer
                    (insert dorg-test-sql--pinned-cli-json)
                    (dorg-sql--parse-result (current-buffer) 0 "SELECT")))))
    (should (eql 9007199254740993 (alist-get 'signed_over_2p53 row)))
    (should (eql 9223372036854775807 (alist-get 'signed_max row)))
    (should (eql 18446744073709551615 (alist-get 'unsigned_max row)))
    (dolist (cell '((text_numeric . "007") (varchar_numeric . "-01")
                    (decimal_exact . "1.250") (title . "007") (tag . "007")
                    (value_raw . "1.250") (sort_key_hex . "697473")
                    (binary_hex . "007FFF") (empty_text . "")
                    (newline_text . "line\nbreak")))
      (should (equal (cdr cell) (alist-get (car cell) row))))
    (should (equal (string ?a 0 ?b) (alist-get 'nul_text row)))
    (should (equal "caf\u00e9 \u2615" (alist-get 'utf8_text row)))
    (should-not (assq 'null_text row))
    (should (equal (string 0 #xfffd) (alist-get 'binary_raw row)))))

(ert-deftest dorg-test-sql-parsed-text-fields-survive-deep-copy ()
  "Numeric-looking text read through the CLI parser hydrates and deep-copies.
A title \"007\" and a keyword value \"1.250\" stay strings through
`dorg-model-from-db-row' and `dorg-heading-deep-copy'; as numbers the deep
copy would signal `wrong-type-argument'.  The integer id stays an integer."
  (let* ((row (car (with-temp-buffer
                     (insert (concat "{\"rows\":[{\"id\":7,\"sort_key\":\"01\","
                                     "\"level_offset\":0,\"commented\":0,"
                                     "\"title\":\"007\",\"post_blank\":0,"
                                     "\"keyword\":\"KEY\",\"value_raw\":\"1.250\"}]}"))
                     (dorg-sql--parse-result (current-buffer) 0 "SELECT"))))
         (heading (dorg-model-from-db-row row)))
    (setf (dorg-heading-items heading)
          (list (dorg-keyword-item :keyword (alist-get 'keyword row)
                                   :value (alist-get 'value_raw row))))
    (should (eql 7 (dorg-heading-id heading)))
    (let ((copy (dorg-heading-deep-copy heading)))
      (should (equal "007" (dorg-heading-title copy)))
      (should (equal "1.250" (dorg-keyword-item-value
                              (car (dorg-heading-items copy))))))))

;;; ─── Path-case alias identity ─────────────────────────────────────────
;;
;; On a case-insensitive filesystem two letter-case variants of one physical
;; beads.org file must never resolve to an arbitrary dorg_files root.
;; `file-truename' does not canonicalize letter-case on macOS, and the
;; dorg_files.relative_path collation is binary (case-sensitive), so identity
;; resolution must fold case and refuse ambiguity.  These regressions seed a
;; populated root and a blank case-variant root and assert the resolver refuses
;; the ambiguous identity while construction preserves canonical bytes.
;; `file-name-case-insensitive-p' is stubbed in the unit tests so the
;; assertions are deterministic on a case-sensitive CI host as well.

(defun dorg-test--seed-case-alias-roots (dolt-dir)
  "Seed DOLT-DIR with a populated root and a blank case-variant root.
Root 100 (lowercase path, two child headings) is populated; root 200
(uppercase path, root heading only) is the blank phantom for the SAME
physical beads file."
  (let ((lower (dorg-repo-encode-relative-path
                dolt-dir "/x/dev/dorg/.dorg/beads.org"))
        (upper (dorg-repo-encode-relative-path
                dolt-dir "/x/dev/DOrg/.dorg/beads.org")))
    (dorg-sql-batch dolt-dir
      (list
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (100, 100, NULL, X'01', 0, FALSE, NULL, 0)"
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (101, 100, 100, X'02', 1, FALSE, 'Task', 0)"
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (102, 100, 100, X'03', 1, FALSE, 'Task 2', 0)"
     "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (200, 200, NULL, X'01', 0, FALSE, NULL, 0)"
       (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (100, %s)"
               (dorg-sql-quote lower))
       (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (200, %s)"
               (dorg-sql-quote upper))))
    (dorg-commit dolt-dir "seed case-alias roots")))

(ert-deftest dorg-test-lookup-root-id-refuses-case-alias-ambiguity ()
  "Case-variant duplicate bindings fail closed instead of choosing a root."
  (dorg-test-with-dolt
    (dorg-test--seed-case-alias-roots dolt-dir)
    (cl-letf (((symbol-function 'file-name-case-insensitive-p)
               (lambda (&rest _) t)))
      (let ((err (should-error
                  (dorg-repo-root-id
                   dolt-dir "/x/dev/DOrg/.dorg/beads.org")
                  :type 'dorg-binding-error)))
        (should (eq 'ambiguous-locator (dorg-error-reason err)))
        (should (equal '(100 200)
                       (plist-get (dorg-error-data err) :root-ids)))))))

(ert-deftest dorg-test-lookup-root-id-case-sensitive-exact ()
  "On a case-sensitive filesystem lookup stays exact (no folding).
Guards against over-folding: distinct-case paths are distinct files there."
  (dorg-test-with-dolt
    (dorg-test--seed-case-alias-roots dolt-dir)
    (cl-letf (((symbol-function 'file-name-case-insensitive-p)
               (lambda (&rest _) nil)))
      (should (eql 100 (dorg-repo-root-id
                        dolt-dir "/x/dev/dorg/.dorg/beads.org")))
      ;; Exact match: the uppercase path maps to its own (blank) root.
      (should (eql 200 (dorg-repo-root-id
                        dolt-dir "/x/dev/DOrg/.dorg/beads.org"))))))

(ert-deftest dorg-test-lookup-root-id-absent-returns-nil ()
  "An unregistered path still returns nil under the new resolver."
  (dorg-test-with-dolt
    (dorg-test--seed-case-alias-roots dolt-dir)
    (cl-letf (((symbol-function 'file-name-case-insensitive-p)
               (lambda (&rest _) t)))
      (should (null (dorg-repo-root-id
                     dolt-dir "/x/dev/dorg/.dorg/other.org"))))))

(ert-deftest dorg-test-dorg-files-stmt-case-alias-no-flip ()
  "The dorg_files upsert keeps the canonical path on a case-alias re-bind.
Re-binding root 100 under the uppercase variant must not flip the stored
path nor create a second row."
  (dorg-test-with-dolt
    (let ((relative (dorg-repo-encode-relative-path
                     dolt-dir "/x/dev/dorg/.dorg/beads.org")))
      (dorg-sql-batch dolt-dir
        (list
       "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (100, 100, NULL, X'01', 0, FALSE, NULL, 0)"
         (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (100, %s)"
                 (dorg-sql-quote relative)))))
    (dorg-commit dolt-dir "seed canonical")
    (cl-letf (((symbol-function 'file-name-case-insensitive-p)
               (lambda (&rest _) t)))
      (dorg-sql dolt-dir
                (dorg-repo-file-row-statement
                 dolt-dir 100 "/x/dev/DOrg/.dorg/beads.org")))
    (dorg-commit dolt-dir "alias re-bind")
    (let ((rows (dorg-sql dolt-dir
                          "SELECT root_id, relative_path FROM dorg_files")))
      (should (= 1 (length rows)))
      (should (equal (dorg-repo-encode-relative-path
                      dolt-dir "/x/dev/dorg/.dorg/beads.org")
                     (alist-get 'relative_path (car rows)))))))

(ert-deftest dorg-test-dorg-files-stmt-rename-updates ()
  "A genuine rename (non-equivalent path, same root) still updates the path."
  (dorg-test-with-dolt
    (let ((relative (dorg-repo-encode-relative-path
                     dolt-dir "/x/dev/dorg/old.org")))
      (dorg-sql-batch dolt-dir
        (list
       "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (100, 100, NULL, X'01', 0, FALSE, NULL, 0)"
         (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (100, %s)"
                 (dorg-sql-quote relative)))))
    (dorg-commit dolt-dir "seed")
    (cl-letf (((symbol-function 'file-name-case-insensitive-p)
               (lambda (&rest _) t)))
      (dorg-sql dolt-dir
                (dorg-repo-file-row-statement
                 dolt-dir 100 "/x/dev/dorg/new.org")))
    (dorg-commit dolt-dir "rename")
    (let ((rows (dorg-sql dolt-dir
                          "SELECT relative_path FROM dorg_files WHERE root_id = 100")))
      (should (equal (dorg-repo-encode-relative-path
                      dolt-dir "/x/dev/dorg/new.org")
                     (alist-get 'relative_path (car rows)))))))

(ert-deftest dorg-test-bind-dolt-case-alias-reuses-root ()
  "Binding the same physical file under a case-variant path reuses its root.
Real-filesystem regression: on a case-insensitive volume a second bind via a
differently-cased path must return the SAME root and create no second
dorg_files row.  Skipped on a case-sensitive volume (the variant is a
different file there)."
  (dorg-test-with-dolt
    (let* ((tmp (make-temp-file "dorg-case-" t))
           (file (expand-file-name "Beads.org" tmp))
           (variant (expand-file-name "beads.org" tmp)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "* Task\n"))
            (if (not (file-name-case-insensitive-p file))
                (ert-skip "filesystem is case-sensitive")
              (let ((dorg--registry nil))
                (let* ((reg1 (dorg-bind-dolt file dolt-dir))
                       (root1 (dorg-backend-root-id
                               (dorg-registration-backend reg1)))
                       (n1 (alist-get 'n (car (dorg-sql
                                               dolt-dir
                                               "SELECT COUNT(*) AS n FROM dorg_files")))))
                  (should (integerp root1))
                  ;; Re-bind via the case-variant path of the same file.
                  (let* ((reg2 (dorg-bind-dolt variant dolt-dir))
                         (root2 (dorg-backend-root-id
                                 (dorg-registration-backend reg2)))
                         (n2 (alist-get 'n (car (dorg-sql
                                                 dolt-dir
                                                 "SELECT COUNT(*) AS n FROM dorg_files")))))
                    (should (eql root1 root2))
                    (should (eql n1 n2)))))))
        (delete-directory tmp t)))))

;;; ─── Canonical on-disk case for bound paths (tracked-regression) ──────────────
;;
;; `file-truename' resolves symlinks but does NOT rewrite a path component to
;; its true on-disk letter-case on a case-insensitive volume — it echoes the
;; caller's case verbatim.  So a project bound through a case-alias of its real
;; path baked the ALIAS case into `dorg_files.relative_path' and the rendered
;; `#+DORG_DOLT_DIR:', which works on macOS but breaks on a case-sensitive host.
;; The fix canonicalizes case to the real on-disk spelling at ingestion (bind)
;; and at render.  These specs are case-insensitive-FS regressions (skipped on a
;; genuinely case-sensitive volume, where a differently-cased path is a distinct
;; file and no aliasing occurs).

(ert-deftest dorg-test-canonical-existing-case-corrects-and-preserves-tail ()
  "`dorg--canonical-existing-case' rewrites existing components to their real
on-disk case and preserves a not-yet-existing tail.  Non-vacuity: an identity
or `file-truename' implementation leaves the alias case and fails the
anti-assertion."
  (let* ((base (make-temp-file "dorg-canon-" t))
         (realdir (expand-file-name "MixedDir" base)))
    (unwind-protect
        (progn
          (make-directory realdir)
          (if (not (file-name-case-insensitive-p base))
              (ert-skip "filesystem is case-sensitive")
            ;; A wrong-case existing component is corrected to on-disk spelling.
            (let ((canon (dorg--canonical-existing-case
                          (expand-file-name "MIXEDDIR" base))))
              (should (equal canon (expand-file-name "MixedDir" base)))
              (should-not (equal canon (expand-file-name "MIXEDDIR" base))))
            ;; A non-existing tail is preserved verbatim under a corrected parent.
            (should (equal (dorg--canonical-existing-case
                            (expand-file-name "MIXEDDIR/NewLeaf" base))
                           (expand-file-name "MixedDir/NewLeaf" base)))))
      (delete-directory base t))))

(ert-deftest dorg-test-bind-dolt-persists-canonical-file-case ()
  "Binding via a case-alias persists the real on-disk spelling in
`dorg_files.relative_path', not the caller's alias, and creates exactly one row.
Non-vacuity: a bare `file-truename' bind stores the alias case and fails the
`should-not' anti-assertion (tracked-regression)."
  (dorg-test-with-dolt
    (let* ((tmp (make-temp-file "dorg-canoncase-" t))
           (real (expand-file-name "Beads.org" tmp))    ; real on-disk spelling
           (alias (expand-file-name "BEADS.ORG" tmp)))   ; caller's case-alias
      (unwind-protect
          (progn
            (with-temp-file real (insert "* Task\n"))
            (if (not (file-name-case-insensitive-p real))
                (ert-skip "filesystem is case-sensitive")
              (let ((dorg--registry nil))
                (dorg-bind-dolt alias dolt-dir)
                (let ((paths (mapcar (lambda (r) (alist-get 'relative_path r))
                                     (dorg-sql dolt-dir
                                               "SELECT relative_path FROM dorg_files"))))
                  (should (= 1 (length paths)))
                  ;; Case-SENSITIVE check: the stored leaf is the real on-disk
                  ;; spelling `Beads.org', never the alias `BEADS.ORG'.  (A bare
                  ;; `file-truename' bind stores `BEADS.ORG' and fails here.)
                  (should (equal (file-name-nondirectory (car paths))
                                 "Beads.org"))))))
        (delete-directory tmp t)))))

(ert-deftest dorg-test-render-emits-canonical-dolt-dir-case ()
  "Direct DOrg materialize renders `#+DORG_DOLT_DIR:' with the real on-disk
store spelling even when the caller passes a case-alias of the store dir.
Exercises the `dorg-render--tree-to-text' emit site directly (pure string
render, no server); the Downstream tracker batch-refresh path renders through this same
site and is covered transitively.  Non-vacuity: echoing `file-truename' of the
alias emits the alias case and fails the `should-not' (tracked-regression)."
  (let* ((base (make-temp-file "dorg-rendercase-" t))
         (real (expand-file-name "StoreCase" base))
         (alias (expand-file-name "STORECASE" base)))
    (unwind-protect
        (progn
          (make-directory real)
          (if (not (file-name-case-insensitive-p base))
              (ert-skip "filesystem is case-sensitive")
            (let* ((root (make-dorg-heading
                          :id 1 :parent-id nil :level 0 :level-offset 0
                          :sort-key (unibyte-string 1) :title nil :items nil))
                   (tree (let ((h (make-hash-table))) (puthash 1 root h) h))
                   ;; Render with the store dir spelled as an UPPERCASE alias.
                   (out (dorg-render--tree-to-text tree 1 "deadbeef" alias)))
              (should (string-match-p "^#\\+DORG_DOLT_DIR:" out))
              (let ((case-fold-search nil))
                (should (string-match-p "StoreCase" out))
                (should-not (string-match-p "STORECASE" out))))))
      (delete-directory base t))))

;;; ─── Bounded reads against a wedged server ──────────────────────────────
;;
;; A wedged sql-server accepts the TCP connection but never speaks the
;; MySQL wire protocol.  Without bounded connect/read timeouts a client
;; parks in `accept-process-output' until the library idle timeout (30s)
;; — an indefinite-feeling hang for an interactive read.  These specs
;; require fast-failing reads.  Server restart/recovery is handled during
;; managed-server ensure, not by changing public SQL transport policy during
;; a timed-out query.

(defun dorg-test--wedged-listener ()
  "Open a TCP listener that accepts but never answers; return (PROC . PORT).
Models a wedged dolt sql-server: the OS completes the accept (so
`port-live-p' is t) but no MySQL handshake or query reply is ever sent."
  (let ((proc (make-network-process :name "dorg-test-wedged"
                                    :host "127.0.0.1" :service 0
                                    :server t :family 'ipv4)))
    (cons proc (process-contact proc :service))))

(ert-deftest dorg-test-get-connection-bounded-against-wedged ()
  "`dorg-sql--get-connection' fails fast against a wedged server.
The connection attempt must signal within a bounded wall-clock budget
rather than blocking on the library's default 30s idle timeout."
  (if (not (dorg-sql--mysql-available-p))
      (ert-skip "mysql.el unavailable")
    (let ((listener (dorg-test--wedged-listener)))
      (unwind-protect
          (let ((dorg-dolt-server-health-timeout 0.75)
                (dorg-sql-read-timeout 1.0)
                (start (float-time)))
            (should-error (dorg-sql--get-connection "/tmp/fake-wedged"
                                                    (cdr listener)))
            (should (< (- (float-time) start) 4.0)))
        (delete-process (car listener))))))

(ert-deftest dorg-test-sql-strict-reactive-timeout-resignals-no-recovery ()
  "Strict `server' mode re-signals mysql timeout and performs no recovery.
Pins the policy boundary: query-time timeout handling must not demote or
restart under the SQL transport layer."
  (if (not (and (dorg-sql--mysql-available-p) (require 'mysql nil t)))
      (ert-skip "mysql.el unavailable")
    (let ((reaped 0)
          (closed 0)
          (ensured 0)
          (exec-calls 0)
          (cli-called 0))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--resolve-transport)
                 (lambda (&rest _) (cons 'mysql 12345)))
                ((symbol-function 'dorg-sql--get-connection)
                 (lambda (_dir &optional port) (list 'conn port)))
                ((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (&rest _)
                   (cl-incf exec-calls)
                   (signal 'mysql-timeout '("stalled"))))
                ((symbol-function 'dorg-sql--close-connection)
                 (lambda (&rest _) (cl-incf closed)))
                ((symbol-function 'dorg-dolt-server--reap-wedged)
                 (lambda (&rest _) (cl-incf reaped)))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (cl-incf ensured) 23456))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (cl-incf cli-called))))
        (let ((dorg-sql-transport 'server))
          (should-error (dorg-sql "/tmp/fake-wedged" "SELECT 1")
                        :type 'mysql-timeout)
          (should (= reaped 0))
          (should (= closed 0))
          (should (= ensured 0))
          (should (= exec-calls 1))
          (should (= cli-called 0)))))))

;;; P0 post_blank drift — DB resync round-trip (fixed-width block)

(ert-deftest dorg-test-dolt-fixed-width-post-blank-roundtrip ()
  "DB resync keeps a fixed-width item's post_blank and its heading's items_hash.
Reproduces the P0 at DB scale without live infra: import a fixed-width+run
fixture (DB1), render it back to org (DB -> org via `dorg-render'), then import
the render into a fresh DB2 — a second resync cycle.  `heading_items.post_blank'
and `headings.items_hash' for the fixed-width heading must be identical across
the two cycles and equal the TRUE blank count (6).  Pre-fix, post_blank climbs
6 -> 7 and items_hash flips each cycle."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((fixture
           (concat "* Probe\n:PROPERTIES:\n:DORG_ID: 100200\n:END:\n"
                   ": replace-buffer-contents\n"
                   ":   <- dorg-materialize\n"
                   (make-string 6 ?\n)
                   "* Sib\n:PROPERTIES:\n:DORG_ID: 100500\n:END:\nx\n"))
          (pb-of (lambda (dir)
                   (let ((rows (dorg-sql
                                dir
                                (concat "SELECT hi.post_blank AS pb "
                                        "FROM heading_items hi "
                                        "JOIN headings h ON hi.heading_id = h.id "
                                        "WHERE h.title = 'Probe' AND hi.kind = 'text'"))))
                     (should (= (length rows) 1))
                     (alist-get 'pb (car rows)))))
          (hash-of (lambda (dir)
                     (let ((rows (dorg-sql
                                  dir
                                  "SELECT items_hash AS ih FROM headings WHERE title = 'Probe'")))
                       (should (= (length rows) 1))
                       (alist-get 'ih (car rows)))))
          rendered pb1 hash1)
      ;; Cycle 1: import the fixture.
      (dorg-test--with-temp-db db1
        (dorg-sql-batch db1 (dorg-test--parse-to-sql fixture))
        (dorg-commit db1 "import fixed-width fixture")
        (setq pb1 (funcall pb-of db1)
              hash1 (funcall hash-of db1)
              rendered (dorg-render db1)))
      ;; Cycle 2: import the rendered org (the resync) into a fresh DB.
      (dorg-test--with-temp-db db2
        (dorg-sql-batch db2 (dorg-test--parse-to-sql rendered))
        (dorg-commit db2 "import rendered resync")
        (should (= pb1 6))                              ; true count, not 7
        (should (= pb1 (funcall pb-of db2)))           ; post_blank stable across resync
        (should (equal hash1 (funcall hash-of db2))))))) ; items_hash stable across resync

;;; ─── Retry seam (moved by tracked-regression) ──────────────────────────────

(ert-deftest dorg-test-dolt-retry-contract-public-seam ()
  "`dorg-dolt-with-retry' is the ONE public cross-module retry seam
(tracked-regression, relocated to the Dolt transport by tracked-regression): the
wrapper macro is public, the retired public predicate/delay names are
gone, and their private successors exist for in-module and test use."
  :tags '(dolt iob)
  (should (fboundp 'dorg-dolt-with-retry))
  (should-not (fboundp 'dorg-sync-retryable-error-p))
  (should-not (fboundp 'dorg-sync-retry-delay))
  (should (fboundp 'dorg-dolt--retryable-error-p))
  (should (fboundp 'dorg-dolt--retry-delay)))

(ert-deftest dorg-test-dolt-with-retry-nil-return-single-attempt ()
  "A body that successfully returns nil exits after exactly ONE attempt —
the wrapper distinguishes success-with-nil from retry via an internal
done flag, never by inspecting the returned value (tracked-regression
nil-protection; `dorg-restore-to-commit's already-at-that-state path
legitimately returns nil)."
  :tags '(dolt iob)
  (let ((runs 0))
    (should (null (dorg-dolt-with-retry (cl-incf runs) nil)))
    (should (= runs 1))))

(ert-deftest dorg-test-dolt-retryable-error-p-1213 ()
  "Serialization failure [1213] is retryable."
  :tags '(dolt iob)
  (should (dorg-dolt--retryable-error-p
           '(error "Dolt error [1213]: serialization failure"))))

(ert-deftest dorg-test-dolt-retryable-error-p-1105-constraint ()
  "Transaction constraint violation [1105] is retryable."
  :tags '(dolt iob)
  (should (dorg-dolt--retryable-error-p
           '(error "Dolt error [1105]: constraint violations from transaction"))))

(ert-deftest dorg-test-dolt-retryable-error-p-1105-non-constraint ()
  "[1105] without 'constraint violations' is NOT retryable."
  :tags '(dolt iob)
  (should-not (dorg-dolt--retryable-error-p
               '(error "Dolt error [1105]: some other problem"))))

(ert-deftest dorg-test-dolt-retryable-error-p-generic ()
  "Generic errors are NOT retryable."
  :tags '(dolt iob)
  (should-not (dorg-dolt--retryable-error-p
               '(error "table not found"))))

(ert-deftest dorg-test-dolt-retryable-error-p-malformed ()
  "Malformed error values return nil (no crash)."
  :tags '(dolt iob)
  (should-not (dorg-dolt--retryable-error-p nil))
  (should-not (dorg-dolt--retryable-error-p '(error)))
  (should-not (dorg-dolt--retryable-error-p '(error 42))))

;;; ─── Stable database-failure facts (tracked-regression) ───────────────────

(ert-deftest dorg-test-dolt-db-failure-classifier-stable-facts ()
  "Raw MySQL conditions are classified once into closed, sanitized facts.
The injected markers model duplicate tuple values and server prose.  None may
survive in the returned plist; only the stable class, numeric code, and
normalized sibling-order constraint are public to the mutation layer."
  :tags '(dolt iob)
  (let ((cases
         '(((mysql-query-error
             "[1213] (40001) Deadlock RAW-1213 while committing")
            serialization 1213 nil)
           ((mysql-query-error
             "[1105] (HY000) constraint violations from transaction RAW-1105")
            serialization 1105 nil)
           ((mysql-query-error
             "[1062] (23000) Duplicate entry 'RAW-TUPLE-A' for key 'headings.uq_heading_sibling_order'")
            sibling-order-duplicate 1062 uq-heading-sibling-order)
           ((mysql-query-error
             "[1062] duplicate unique key given: uq_heading_sibling_order RAW-TUPLE-B")
            sibling-order-duplicate 1062 uq-heading-sibling-order)
           ((mysql-query-error
             "[1062] (23000) Duplicate entry 'RAW-OTHER' for key 'headings.PRIMARY'")
            deterministic-constraint 1062 nil)
           ((mysql-query-error "RAW-UNKNOWN database fault")
            database-failure nil nil))))
    (dolist (case cases)
      (pcase-let ((`(,err ,class ,code ,constraint) case))
        (let* ((facts (dorg-dolt--classify-db-error err))
               (printed (prin1-to-string facts)))
          (should (eq class (plist-get facts :class)))
          (should (equal code (plist-get facts :db-code)))
          (should (equal constraint (plist-get facts :constraint)))
          (should-not (plist-member facts :underlying))
          (should-not (string-match-p "RAW-" printed)))))
    ;; Dolt's observed native duplicate spelling omits the index name.  The
    ;; active statement makes it unambiguous only for the headings table; the
    ;; same tuple-shaped message from heading_items must not be mislabeled.
    (let* ((err '(mysql-query-error
                  "[1062] (HY000) duplicate unique key given: [101,RAW-HEX]"))
           (heading-facts
            (dorg-dolt--classify-db-error
             err "INSERT INTO headings (id, parent_id, sort_key) VALUES (...)"))
           (item-facts
            (dorg-dolt--classify-db-error
             err "INSERT INTO heading_items (id, heading_id, sort_key) VALUES (...)"))
           (primary-facts
            (dorg-dolt--classify-db-error
             '(mysql-query-error
               "[1062] (HY000) duplicate unique key given: [101]")
             "INSERT INTO `headings` (id, parent_id, sort_key) VALUES (...)")))
      (should (eq 'sibling-order-duplicate
                  (plist-get heading-facts :class)))
      (should (eq 'uq-heading-sibling-order
                  (plist-get heading-facts :constraint)))
      (should (eq 'deterministic-constraint
                  (plist-get item-facts :class)))
      (should (eq 'deterministic-constraint
                  (plist-get primary-facts :class)))
      (should-not (string-match-p "RAW-" (prin1-to-string heading-facts))))))

(ert-deftest dorg-test-dolt-postimage-parity-classifier-bounds-closed-ids ()
  "Core parity IDs cross the classifier only as a bounded closed fact."
  (dolist (ids (list '(42) '(7 11 19) (number-sequence 1 33)))
    (let* ((err (condition-case caught
                    (signal 'dorg-sql-postimage-parity
                            (list "in-transaction readback diverged"
                                  (list :plane 'core :ids ids)))
                  (dorg-sql-postimage-parity caught)))
           (facts (dorg-dolt--classify-db-error err)))
      (should (eq 'postimage-parity (plist-get facts :class)))
      (should (eq 'core (plist-get facts :parity-plane)))
      (should (= (length ids) (plist-get facts :parity-id-count)))
      (should (equal (cl-subseq ids 0 (min 32 (length ids)))
                     (plist-get facts :parity-ids)))))
  (dolist (ids (list nil '(1 1) '(0) '(1 "RAW-SENTINEL") 42 '(1 . 2)))
    (let* ((err (condition-case caught
                    (signal 'dorg-sql-postimage-parity
                            (list "in-transaction readback diverged"
                                  (list :plane 'core :ids ids)))
                  (dorg-sql-postimage-parity caught)))
           (facts (dorg-dolt--classify-db-error err)))
      (should-not (plist-member facts :parity-ids))
      (should-not (plist-member facts :parity-id-count)))))

(ert-deftest dorg-test-dolt-postimage-rows-classifier-omits-ids ()
  "Rows-plane parity remains table-scoped and never exposes identity IDs."
  (let* ((err (condition-case caught
                  (signal 'dorg-sql-postimage-parity
                          (list "in-transaction readback diverged"
                                (list :plane 'rows :table "private" :ids '(1 2))))
                (dorg-sql-postimage-parity caught)))
         (facts (dorg-dolt--classify-db-error err)))
    (should (eq 'rows (plist-get facts :parity-plane)))
    (should-not (plist-member facts :parity-ids))
    (should-not (plist-member facts :parity-id-count))))

(ert-deftest dorg-test-dolt-retry-predicate-delegates-to-classifier ()
  "The older generic retry seam consumes the central classifier, not raw text."
  :tags '(dolt iob)
  (let ((calls 0))
    (cl-letf (((symbol-function 'dorg-dolt--classify-db-error)
               (lambda (_err)
                 (cl-incf calls)
                 '(:class serialization :db-code 1213))))
      (should (dorg-dolt--retryable-error-p '(error "not parseable here")))
      (should (= calls 1)))))

(ert-deftest dorg-test-dolt-opt-in-rollback-receipt-is-sanitized ()
  "The typed commit seam can request proof of a completed same-session rollback.
Ordinary callers retain the original mysql condition.  The opt-in path instead
signals one private receipt containing only classifier facts and
`:rollback-confirmed t'; the raw duplicate value is absent from its complete
condition message and data."
  :tags '(dolt iob)
  (let ((raw "[1062] Duplicate entry 'RAW-ROLLBACK' for key 'uq_heading_sibling_order'"))
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (when (equal sql "POISON")
                   (signal 'mysql-query-error (list raw)))
                 nil)))
      ;; Compatibility: no opt-in means the original condition still escapes.
      (let ((err (should-error
                  (dorg-sql--mysql-batch-on-conn
                   'conn '("POISON") "ordinary")
                  :type 'mysql-query-error)))
        (should (string-match-p "RAW-ROLLBACK" (error-message-string err))))
      ;; Typed mutation opts in through the fifth argument.
      (let* ((err (should-error
                   (dorg-sql--mysql-batch-on-conn
                    'conn '("POISON") "typed" nil t)
                   :type 'dorg-sql-transaction-rolled-back))
             (data (dorg-sql-transaction-rollback-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should (eq 'statements (plist-get data :transaction-phase)))
        (should (eq 'sibling-order-duplicate (plist-get data :class)))
        (should (= 1062 (plist-get data :db-code)))
        (should (eq 'uq-heading-sibling-order (plist-get data :constraint)))
        (should-not (plist-member data :underlying))
        (should-not (string-match-p "RAW-ROLLBACK" printed))))))

(ert-deftest dorg-test-dolt-begin-failure-never-claims-rollback ()
  "A failed BEGIN has no transaction to roll back and propagates unchanged."
  :tags '(dolt iob)
  (let (queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql queries)
                 (when (equal sql "BEGIN")
                   (signal 'mysql-connection-error '("RAW-BEGIN"))))))
      (let ((err (should-error
                  (dorg-sql--mysql-batch-on-conn
                   'conn '("S1") "begin-failure" nil t)
                  :type 'mysql-connection-error)))
        (should (string-match-p "RAW-BEGIN" (error-message-string err))))
      (should (equal '("BEGIN") (nreverse queries)))
      (should-not (member "ROLLBACK" queries)))))

(ert-deftest dorg-test-dolt-validation-failure-records-definite-phase ()
  "A validation database fault rolls back once with sanitized validate facts."
  :tags '(dolt iob)
  (let (queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql) (push sql queries) nil)))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("S1") "validate-failure"
                (lambda (_query)
                  (signal 'mysql-query-error
                          '("[1213] RAW-VALIDATION deadlock")))
                t)
               :type 'dorg-sql-transaction-rolled-back))
             (data (dorg-sql-transaction-rollback-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should (eq 'validate (plist-get data :transaction-phase)))
        (should (eq 'serialization (plist-get data :class)))
        (should (= 1213 (plist-get data :db-code)))
        (should (= 1 (cl-count "ROLLBACK" queries :test #'equal)))
        (should-not (string-match-p "RAW-VALIDATION" printed))))))

(ert-deftest dorg-test-dolt-statement-serialization-records-definite-phase ()
  "A statement 1213 rolls back once and remains a definite serialization fact."
  :tags '(dolt iob)
  (let (queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql queries)
                 (when (equal sql "S1")
                   (signal 'mysql-query-error
                           '("[1213] RAW-STATEMENT deadlock")))
                 nil)))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("S1") "statement-failure" nil t)
               :type 'dorg-sql-transaction-rolled-back))
             (data (dorg-sql-transaction-rollback-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should (eq 'statements (plist-get data :transaction-phase)))
        (should (eq 'serialization (plist-get data :class)))
        (should (= 1213 (plist-get data :db-code)))
        (should (= 1 (cl-count "ROLLBACK" queries :test #'equal)))
        (should-not (string-match-p "RAW-STATEMENT" printed))))))

(ert-deftest dorg-test-dolt-commit-sql-fault-remains-definitely-rolled-back ()
  "In-commit phase alone does not turn a database fault into transport loss."
  :tags '(dolt iob)
  (let (queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql) (push sql queries) nil))
              ((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (when (string-match-p "CALL DOLT_COMMIT" sql)
                   (signal 'mysql-query-error
                           '("[1213] RAW-COMMIT-SQL deadlock"))))))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("S1") "commit-sql-failure" nil t)
               :type 'dorg-sql-transaction-rolled-back))
             (data (dorg-sql-transaction-rollback-data err)))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should (eq 'in-commit (plist-get data :transaction-phase)))
        (should (eq 'serialization (plist-get data :class)))
        (should (= 1 (cl-count "ROLLBACK" queries :test #'equal)))))))

(ert-deftest dorg-test-dolt-in-commit-transport-is-hashless-indeterminate ()
  "Transport type plus in-commit phase wins before prose classification.
The timeout text deliberately resembles database code 1213: the central
database classifier must never see it, no rollback may be sent, and the
outcome is indeterminate even though the response hash was lost."
  :tags '(dolt iob)
  (let ((classifier-calls 0)
        queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql) (push sql queries) nil))
              ((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (when (string-match-p "CALL DOLT_COMMIT" sql)
                   (signal 'mysql-timeout
                           '("[1213] RAW-TRANSPORT response lost")))))
              ((symbol-function 'dorg-dolt--classify-db-error)
               (lambda (&rest _)
                 (cl-incf classifier-calls)
                 '(:class serialization :db-code 1213))))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("S1") "lost-response" nil t)
               :type 'dorg-sql-commit-indeterminate))
             (data (dorg-sql-commit-indeterminate-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (eq 'in-commit (plist-get data :transaction-phase)))
        (should-not (plist-member data :commit))
        (should-not (plist-member data :underlying))
        (should (= 0 classifier-calls))
        (should (= 0 (cl-count "ROLLBACK" queries :test #'equal)))
        (should-not (string-match-p "RAW-TRANSPORT" printed))))))

(ert-deftest dorg-test-dolt-transport-condition-family-is-type-based ()
  "Every typed MySQL transport condition is recognized without prose parsing."
  :tags '(dolt iob)
  (dolist (type '(mysql-connection-error mysql-timeout mysql-protocol-error))
    (should (dorg-sql--transport-condition-p
             (list type "database-looking [1213] prose"))))
  (should-not
   (dorg-sql--transport-condition-p
    '(mysql-query-error "transport-looking response lost prose"))))

(ert-deftest dorg-test-dolt-dead-in-commit-bare-error-is-indeterminate ()
  "A bare error on a provably dead in-commit connection sends no rollback."
  :tags '(dolt iob)
  (let ((classifier-calls 0)
        queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql) (push sql queries) nil))
              ((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (when (string-match-p "CALL DOLT_COMMIT" sql)
                   (error "RAW-DEAD-CONNECTION"))))
              ((symbol-function 'dorg-sql--conn-process-dead-p)
               (lambda (_conn) t))
              ((symbol-function 'dorg-dolt--classify-db-error)
               (lambda (&rest _)
                 (cl-incf classifier-calls)
                 '(:class database-failure))))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("S1") "dead-response" nil t)
               :type 'dorg-sql-commit-indeterminate))
             (data (dorg-sql-commit-indeterminate-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (eq 'in-commit (plist-get data :transaction-phase)))
        (should (= 0 classifier-calls))
        (should (= 0 (cl-count "ROLLBACK" queries :test #'equal)))
        (should-not (string-match-p "RAW-DEAD-CONNECTION" printed))))))

(ert-deftest dorg-test-dolt-retry-delay-increases ()
  "Retry delay increases with attempt number (exponential backoff)."
  :tags '(dolt iob)
  (let ((dorg-dolt-retry-base-ms 100))
    ;; Sample multiple times to account for jitter, check median trend
    (let ((delays-0 (cl-loop repeat 20 collect (dorg-dolt--retry-delay 0)))
          (delays-2 (cl-loop repeat 20 collect (dorg-dolt--retry-delay 2))))
      ;; Median of attempt-2 should be ~4x median of attempt-0
      (let ((med-0 (nth 10 (sort delays-0 #'<)))
            (med-2 (nth 10 (sort delays-2 #'<))))
        (should (> med-2 (* 2 med-0)))))))

(ert-deftest dorg-test-dolt-retry-delay-positive ()
  "Retry delay is always positive (floor at 10ms)."
  :tags '(dolt iob)
  (let ((dorg-dolt-retry-base-ms 1))
    (cl-loop repeat 50
             do (should (> (dorg-dolt--retry-delay 0) 0)))))

;;; ─── tracked-regression relocation contract (Family C, retry) ───────────────

(defconst dorg-dolt-tests--qc260-repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-dolt-tests--qc260-name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-dolt-tests--qc260-repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-dolt-retry-relocation-contract ()
  "tracked-regression Family C: the tracked-regression retry seam lives in dorg-dolt.el.
Pure — no sql-server needed.  RED at SPEC (new names undefined, retired
sync-prefixed names still defined and referenced); GREEN after IMPL.  The
head-diverged error definition intentionally stays with its signaler in the
sync module and is NOT part of this contract."
  (let ((fns '(("-retryable-error-p" . dorg-dolt--retryable-error-p)
               ("-retry-delay"       . dorg-dolt--retry-delay)))
        (macros '(("with-retry" . dorg-dolt-with-retry)))
        (vars '(("max-retries"   . dorg-dolt-max-retries)
                ("retry-base-ms" . dorg-dolt-retry-base-ms)))
        (old-prefix (concat "dorg-sync" "-")))
    (dolist (p (append fns macros))
      (let ((old (intern-soft (concat old-prefix (car p)))))
        (should (fboundp (cdr p)))
        (should-not (and old (fboundp old)))))
    (dolist (p vars)
      (let ((old (intern-soft (concat old-prefix (car p)))))
        ;; Pin defcustom status, not mere binding: a plain defvar at the
        ;; destination would satisfy boundp yet lose the customize surface.
        (should (custom-variable-p (cdr p)))
        (should-not (and old (boundp old)))))
    (should-not (dorg-dolt-tests--qc260-name-hits
                 (mapcar (lambda (p) (concat old-prefix (car p)))
                         (append fns macros vars))))))

;;; ─── Private failure capture ───────────────────────────────────────────────
;;
;; A statement-phase database failure inside the typed mutation batch keeps
;; only closed, sanitized facts in the public receipt.  These tests pin the
;; diagnostic contract: the batch executor counts the execution ordinal at
;; its own loop, carries a closed verb/table class drawn from a finite public
;; set, and, unless the operator turns the capture off, writes the original
;; condition and the failed SQL to a private file under the store's runtime
;; overlay after the rollback.  `mysql-query' is stubbed at the same seam as
;; the rollback-receipt tests above; the suite fixture may hold a server, but
;; no statement here reaches one.

(defun dorg-dolt-tests--capture-scratch-store ()
  "Return a fresh scratch store directory shaped like a Dolt dir."
  (let ((dir (make-temp-file "dorg-capture-store" t)))
    (make-directory (expand-file-name ".dolt" dir))
    (file-name-as-directory dir)))

(defun dorg-dolt-tests--capture-files (store)
  "Return the capture files present under STORE's overlay, if any."
  (let ((dir (expand-file-name ".dorg/failures" store)))
    (and (file-directory-p dir)
         (directory-files dir t "\\`[^.]"))))

(defun dorg-dolt-tests--capture-run (store statements &rest opts)
  "Run STATEMENTS through the real typed batch executor against STORE.
The mysql seam signals the raw condition in OPTS `:raw' when it sees the
statement equal to `:poison' (default \"POISON\"); every other statement
succeeds.  Returns the `dorg-sql-transaction-rolled-back' receipt data."
  (let* ((raw (or (plist-get opts :raw) "[1105] RAW-PRIVATE server prose"))
         (poison (or (plist-get opts :poison) "POISON"))
         (queries nil))
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql queries)
                 (when (equal sql poison)
                   (signal 'mysql-query-error (list raw)))
                 nil)))
      (let* ((dorg-sql--capture-store-dir store)
             (err (should-error
                   (dorg-sql--mysql-batch-on-conn
                    'conn statements "typed" nil t)
                   :type 'dorg-sql-transaction-rolled-back)))
        (list :data (dorg-sql-transaction-rollback-data err)
              :message (error-message-string err)
              :error err
              :queries (nreverse queries))))))

(ert-deftest dorg-test-dolt-failure-capture-on-by-default ()
  "The capture is on unless the operator turns it off.
The default value is the fact under test; the behavioral witnesses bind the
variable explicitly so they hold whichever way the default is set."
  :tags '(dolt iob)
  (should (eq t (default-value 'dorg-sql-capture-failed-statements)))
  (should (eq t (custom--standard-value 'dorg-sql-capture-failed-statements))))

(ert-deftest dorg-test-dolt-failure-capture-disabled-by-binding ()
  "With the capture bound off, a statement failure adds only closed correlation facts.
No file appears under the store overlay and no prose reaches the receipt."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements nil)
         (run (dorg-dolt-tests--capture-run
               store '("INSERT INTO embead_notes (bead_id) VALUES ('x')"
                       "POISON")))
         (data (plist-get run :data))
         (printed (format "%s %S" (plist-get run :message) data)))
    (should (eq t (plist-get data :rollback-confirmed)))
    (should (eq 'statements (plist-get data :transaction-phase)))
    (should (eq 'database-failure (plist-get data :class)))
    (should (= 1105 (plist-get data :db-code)))
    (should (= 2 (plist-get data :statement-index)))
    (should (equal '(unknown nil) (plist-get data :statement-class)))
    (should-not (plist-member data :capture-id))
    (should-not (plist-member data :capture))
    (should-not (string-match-p "RAW-PRIVATE" printed))
    (should-not (dorg-dolt-tests--capture-files store))
    ;; The only SQL after the failure is the same-session ROLLBACK.
    (should (equal "ROLLBACK" (car (last (plist-get run :queries)))))))

(ert-deftest dorg-test-dolt-failure-capture-writes-private-file ()
  "With the opt-in, the original condition and SQL land in one private file.
The file is created 0600 inside a 0700 overlay subdirectory, named by the
receipt's capture id; the receipt itself still carries no prose or SQL."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (run (dorg-dolt-tests--capture-run
               store '("INSERT INTO embead_notes (bead_id) VALUES ('x')"
                       "POISON")))
         (data (plist-get run :data))
         (printed (format "%s %S" (plist-get run :message) data))
         (files (dorg-dolt-tests--capture-files store)))
    (should (eq 'written (plist-get data :capture)))
    (should (stringp (plist-get data :capture-id)))
    (should (= 1 (length files)))
    (should (equal (plist-get data :capture-id)
                   (file-name-base (car files))))
    (should (= #o700 (logand #o777 (file-modes
                                    (expand-file-name ".dorg/failures" store)))))
    (should (= #o600 (logand #o777 (file-modes (car files)))))
    (let ((captured (with-temp-buffer
                      (insert-file-contents (car files))
                      (read (current-buffer)))))
      (should (string-match-p "RAW-PRIVATE" (plist-get captured :message)))
      (should (equal "POISON" (plist-get captured :statement)))
      (should (= 2 (plist-get captured :statement-index)))
      (should (eq 'statements (plist-get captured :transaction-phase)))
      (should (= 1105 (plist-get captured :db-code))))
    (should-not (string-match-p "RAW-PRIVATE" printed))
    (should-not (string-match-p "POISON" printed))))

(ert-deftest dorg-test-dolt-failure-capture-never-leaks-values ()
  "Duplicate-value and syntax prose quoting data never reach a public surface.
Both raw shapes are captured privately when opted in, and absent otherwise."
  :tags '(dolt iob)
  (dolist (raw '("[1062] Duplicate entry 'RAW-VALUE-77' for key 'headings.PRIMARY'"
                 "[1064] You have an error in your SQL syntax near 'VALUES (''RAW-FRAG'')'"))
    (dolist (opt-in '(nil t))
      (let* ((store (dorg-dolt-tests--capture-scratch-store))
             (dorg-sql-capture-failed-statements opt-in)
             (run (dorg-dolt-tests--capture-run
                   store '("POISON") :raw raw))
             (data (plist-get run :data))
             (printed (format "%s %S" (plist-get run :message) data)))
        (should-not (string-match-p "RAW-" printed))
        (should-not (plist-member data :underlying))
        (if opt-in
            (should (= 1 (length (dorg-dolt-tests--capture-files store))))
          (should-not (dorg-dolt-tests--capture-files store)))))))

(ert-deftest dorg-test-dolt-failure-capture-failure-isolated ()
  "A failing capture never changes the refusal, its facts or the rollback.
An unwritable overlay yields `:capture failed', no partial file, and a
receipt otherwise equal to the capture-off run; the prose stays private."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (overlay (expand-file-name ".dorg" store)))
    (make-directory overlay)
    ;; A plain file where the failures directory must go: creation fails.
    (with-temp-file (expand-file-name "failures" overlay) (insert "blocked"))
    (let* ((dorg-sql-capture-failed-statements t)
           (run (dorg-dolt-tests--capture-run store '("POISON")))
           (data (plist-get run :data))
           (printed (format "%s %S" (plist-get run :message) data))
           (off (let ((dorg-sql-capture-failed-statements nil))
                  (plist-get (dorg-dolt-tests--capture-run store '("POISON"))
                             :data))))
      (should (eq 'failed (plist-get data :capture)))
      (should-not (plist-member data :capture-id))
      (should-not (string-match-p "RAW-PRIVATE" printed))
      (should-not (string-match-p "blocked" printed))
      (should (equal "ROLLBACK" (car (last (plist-get run :queries)))))
      ;; Decision fields agree with the capture-off receipt.
      (dolist (key '(:rollback-confirmed :transaction-phase :class :db-code
                     :statement-index :statement-class))
        (should (equal (plist-get off key) (plist-get data key)))))))

(ert-deftest dorg-test-dolt-failure-capture-refuses-unsafe-destination ()
  "A symlinked overlay subdirectory is refused as a capture destination.
The refusal is reported as a failed capture; nothing is written through the
link and nothing outside the store is touched."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (overlay (expand-file-name ".dorg" store))
         (elsewhere (make-temp-file "dorg-capture-elsewhere" t)))
    (make-directory overlay)
    (make-symbolic-link elsewhere (expand-file-name "failures" overlay))
    (let* ((dorg-sql-capture-failed-statements t)
           (run (dorg-dolt-tests--capture-run store '("POISON")))
           (data (plist-get run :data)))
      (should (eq 'failed (plist-get data :capture)))
      (should-not (directory-files elsewhere nil "\\`[^.]")))))

(ert-deftest dorg-test-dolt-failure-ordinal-second-of-identical-statements ()
  "The ordinal is counted at the batch loop, never found by text search.
Two identical statements where the second fails report index 2 with the
same statement text, and exactly one ROLLBACK follows."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (seen 0)
         (run
          (cl-letf (((symbol-function 'mysql-query)
                     (lambda (_conn sql)
                       (when (equal sql "UPDATE embead_notes SET body = 'same'")
                         (cl-incf seen)
                         (when (= seen 2)
                           (signal 'mysql-query-error
                                   '("[1105] RAW-PRIVATE second failure"))))
                       nil)))
            (let* ((dorg-sql--capture-store-dir store)
                   (err (should-error
                         (dorg-sql--mysql-batch-on-conn
                          'conn
                          '("UPDATE embead_notes SET body = 'same'"
                            "UPDATE embead_notes SET body = 'same'")
                          "typed" nil t)
                         :type 'dorg-sql-transaction-rolled-back)))
              (dorg-sql-transaction-rollback-data err))))
         (files (dorg-dolt-tests--capture-files store)))
    (should (= 2 (plist-get run :statement-index)))
    ;; An extension table is outside the finite public set: the class names
    ;; the verb and `unknown'; the exact statement is only in the capture.
    (should (equal '(update unknown) (plist-get run :statement-class)))
    (should (= 1 (length files)))
    (let ((captured (with-temp-buffer
                      (insert-file-contents (car files))
                      (read (current-buffer)))))
      (should (= 2 (plist-get captured :statement-index)))
      (should (equal "UPDATE embead_notes SET body = 'same'"
                     (plist-get captured :statement))))))

(ert-deftest dorg-test-dolt-failure-non-statement-failure-has-no-stale-ordinal ()
  "A validation-phase failure after the statements succeeded carries no ordinal.
Unknown is an absent fact: the last successful data statement's index must
not survive into a later phase's receipt."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (run
          (cl-letf (((symbol-function 'mysql-query) (lambda (_conn _sql) nil)))
            (let* ((dorg-sql--capture-store-dir store)
                   (err (should-error
                         (dorg-sql--mysql-batch-on-conn
                          'conn '("INSERT INTO embead_notes (bead_id) VALUES ('x')")
                          "typed"
                          (lambda (_query) (error "RAW-PRIVATE validator says no"))
                          t)
                         :type 'dorg-sql-transaction-rolled-back)))
              (dorg-sql-transaction-rollback-data err)))))
    (should (eq 'validate (plist-get run :transaction-phase)))
    (should-not (plist-member run :statement-index))
    (should-not (plist-member run :statement-class))
    (should-not (dorg-dolt-tests--capture-files store))))

(ert-deftest dorg-test-dolt-failure-statement-class-is-closed ()
  "The public statement class is a closed (VERB TABLE) list, never SQL text.
The table is one of the finite public core names or the symbol `unknown';
identifiers outside that set, quoted or bare, are never echoed, comments and
unfamiliar shapes fall to (unknown nil), and every shape encodes as a JSON
array so the public error envelope can carry it."
  :tags '(dolt iob)
  (dolist (case '(("INSERT INTO ext_notes (bead_id) VALUES ('RAW-V')" insert unknown)
                  ("UPDATE `headings` SET title = 'RAW-V'" update "headings")
                  ("DELETE FROM heading_items WHERE id = 1" delete "heading_items")
                  ("UPDATE RAW_SENTINEL_TABLE SET x = 1" update unknown)
                  ("UPDATE `RAW_SENTINEL_TABLE` SET x = 1" update unknown)
                  ("DELETE FROM `Headings` WHERE id = 1" delete "headings")
                  ("CALL DOLT_ADD('.')" unknown nil)
                  ("INSERT INTO `we`ird` (x) VALUES (1)" unknown nil)
                  ("/* RAW-COMMENT */ INSERT INTO ext_notes (x) VALUES (1)" unknown nil)))
    (let ((class (dorg-sql--statement-class (car case))))
      (should (equal (cdr case) class))
      (should (= 2 (safe-length class)))
      (should (stringp (json-encode (list :statement-class class))))
      (should-not (string-match-p "RAW" (prin1-to-string class)))
      ;; A public table string is the set's own object, never a substring
      ;; of the statement.
      (when (stringp (cadr class))
        (should (memq (cadr class) dorg-sql--public-table-set))))))

(ert-deftest dorg-test-dolt-failure-capture-classification-stable ()
  "The opt-in changes no classification: every closed class is unchanged."
  :tags '(dolt iob)
  (let ((cases
         '(("[1213] (40001) Deadlock RAW-1213 while committing" serialization 1213)
           ("[1105] (HY000) constraint violations from transaction RAW-1105" serialization 1105)
           ("[1062] (23000) Duplicate entry 'RAW-A' for key 'headings.uq_heading_sibling_order'" sibling-order-duplicate 1062)
           ("[1062] (23000) Duplicate entry 'RAW-B' for key 'headings.PRIMARY'" deterministic-constraint 1062)
           ("RAW-UNKNOWN database fault" database-failure nil))))
    (dolist (opt-in '(nil t))
      (dolist (case cases)
        (pcase-let ((`(,raw ,class ,code) case))
          (let* ((store (dorg-dolt-tests--capture-scratch-store))
                 (dorg-sql-capture-failed-statements opt-in)
                 (data (plist-get
                        (dorg-dolt-tests--capture-run store '("POISON") :raw raw)
                        :data)))
            (should (eq class (plist-get data :class)))
            (should (equal code (plist-get data :db-code)))))))))

(ert-deftest dorg-test-dolt-lease-batch-binds-capture-store ()
  "The lease seam carries the owning store to the executor; nothing rediscovers it."
  :tags '(dolt iob)
  (let* ((dir "/tmp/dorg-capture-lease-store/")
         (lease (dorg-sql--make-lease :dolt-dir dir :owner (make-symbol "l")))
         (seen 'unset))
    (cl-letf (((symbol-function 'dorg-sql--lease-validate) (lambda (_l) nil))
              ((symbol-function 'dorg-sql--lease-conn) (lambda (_l) 'conn))
              ((symbol-function 'dorg-sql--mysql-batch-on-conn)
               (lambda (&rest _) (setq seen dorg-sql--capture-store-dir) nil)))
      (dorg-sql--lease-batch lease '("SELECT 1") "typed" nil t)
      (should (equal dir seen)))))

(ert-deftest dorg-test-dolt-failure-public-class-never-echoes-identifiers ()
  "Through the real batch executor, an unfamiliar table never reaches the public class.
Bare and backquoted identifiers outside the finite public set become
`unknown' with the capture on and off; a core table keeps its public name."
  :tags '(dolt iob)
  (dolist (opt-in '(nil t))
    (dolist (case '(("UPDATE RAW_SENTINEL_TABLE SET x = 1" update unknown)
                    ("UPDATE `RAW_SENTINEL_TABLE` SET x = 1" update unknown)
                    ("DELETE FROM headings WHERE id = 1" delete "headings")))
      (let* ((store (dorg-dolt-tests--capture-scratch-store))
             (dorg-sql-capture-failed-statements opt-in)
             (run (dorg-dolt-tests--capture-run
                   store (list "INSERT INTO headings (id) VALUES (1)" (car case))
                   :poison (car case)))
             (data (plist-get run :data))
             (printed (format "%s %S" (plist-get run :message) data)))
        (should (equal (cdr case) (plist-get data :statement-class)))
        (should (= 2 (plist-get data :statement-index)))
        (should-not (string-match-p "sentinel" (downcase printed)))
        (if opt-in
            (should (eq 'written (plist-get data :capture)))
          (should-not (plist-member data :capture)))))))

(ert-deftest dorg-test-dolt-failure-public-refusal-uses-real-receipt ()
  "The real executor receipt drives the public mutation refusal without leaking.
The rolled-back receipt produced by the batch loop, not a hand-built one,
reaches `dorg-mutate--rolled-back-decision'; the refusal carries the closed
facts and no identifier or prose from the failed statement."
  :tags '(dolt iob)
  (require 'dorg-mutate)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (poison "UPDATE `RAW_SENTINEL_TABLE` SET x = 1")
         (run (dorg-dolt-tests--capture-run
               store (list "INSERT INTO headings (id) VALUES (1)" poison)
               :poison poison))
         (plan (list :caller-base "b0" :plan-base "b0"
                     :root-scope '(1) :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision
                (plist-get run :error) plan "h1" nil nil 0)
               :type 'dorg-mutate-error))
         (data (dorg-mutate-error-data err))
         (printed (format "%s %S" (error-message-string err) data)))
    (should (eq 'invariant (plist-get data :reason)))
    (should (eq 'database-failure (plist-get data :detail)))
    (should (= 2 (plist-get data :statement-index)))
    (should (equal '(update unknown) (plist-get data :statement-class)))
    (should (eq 'written (plist-get data :capture)))
    (should (equal (plist-get data :capture-id)
                   (file-name-base (car (dorg-dolt-tests--capture-files store)))))
    (should-not (string-match-p "sentinel" (downcase printed)))
    (should-not (string-match-p "RAW-PRIVATE" printed))
    (should-not (plist-member data :underlying))
    (should-not (plist-member data :message))
    ;; The public error envelope must encode this refusal, closed facts
    ;; included: the class is a JSON array, never a dotted pair.
    (let ((envelope (json-encode
                     (dorg-envelope-error-json
                      (dorg-mutate-error-reason err) (cadr err) data))))
      (should (stringp envelope))
      (should (string-match-p "\"statement-class\":\\[\"update\",\"unknown\"\\]" envelope))
      (should-not (string-match-p "sentinel" (downcase envelope))))))

(ert-deftest dorg-test-dolt-failure-capture-refuses-unsafe-existing-modes ()
  "Existing destination directories with unsafe modes are refused, not repaired.
A failures directory readable or writable by others, or a group/world-writable
overlay parent, yields `:capture failed' and no file; an owned readable overlay
with a private failures directory, and the 0700/0700 pair, are the positive
controls.  No mode of an existing directory is changed."
  :tags '(dolt iob)
  (dolist (case '((#o700 #o755 failed) (#o700 #o777 failed) (#o777 #o700 failed)
                  (#o755 #o700 written) (#o700 #o700 written)))
    (pcase-let ((`(,overlay-mode ,dir-mode ,expected) case))
      (let* ((store (dorg-dolt-tests--capture-scratch-store))
             (overlay (expand-file-name ".dorg" store))
             (dir (expand-file-name "failures" overlay)))
        (make-directory overlay)
        (make-directory dir)
        (set-file-modes overlay overlay-mode)
        (set-file-modes dir dir-mode)
        (let* ((dorg-sql-capture-failed-statements t)
               (data (plist-get (dorg-dolt-tests--capture-run store '("POISON"))
                                :data)))
          (should (eq expected (plist-get data :capture)))
          (should (= overlay-mode (logand #o777 (file-modes overlay))))
          (should (= dir-mode (logand #o777 (file-modes dir))))
          (if (eq expected 'written)
              (should (= 1 (length (dorg-dolt-tests--capture-files store))))
            (should-not (dorg-dolt-tests--capture-files store))))))))

(ert-deftest dorg-test-dolt-failure-capture-refuses-foreign-owner ()
  "A destination owned by another user is refused through the ownership seam.
No chown is attempted: the owner query is stubbed to report a foreign uid for
the failures directory while everything else is owned and 0700; the same
store with the real owner query is the positive control."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (overlay (expand-file-name ".dorg" store))
         (dir (expand-file-name "failures" overlay))
         (real (symbol-function 'dorg-sql--capture-path-owner-uid)))
    (with-file-modes #o700 (make-directory overlay))
    (with-file-modes #o700 (make-directory dir))
    (cl-letf (((symbol-function 'dorg-sql--capture-path-owner-uid)
               (lambda (path)
                 (if (equal (file-name-as-directory path)
                            (file-name-as-directory dir))
                     (1+ (user-uid))
                   (funcall real path)))))
      (let* ((dorg-sql-capture-failed-statements t)
             (data (plist-get (dorg-dolt-tests--capture-run store '("POISON"))
                              :data)))
        (should (eq 'failed (plist-get data :capture)))
        (should-not (dorg-dolt-tests--capture-files store))))
    (let* ((dorg-sql-capture-failed-statements t)
           (data (plist-get (dorg-dolt-tests--capture-run store '("POISON"))
                            :data)))
      (should (eq 'written (plist-get data :capture)))
      (should (= 1 (length (dorg-dolt-tests--capture-files store)))))))

(ert-deftest dorg-test-dolt-failure-capture-write-failure-cleans-own-file ()
  "A failure while writing the payload removes only the file this capture created.
The exclusive file exists when the payload write fails; afterwards the
failures directory is empty again, the receipt says `:capture failed', the
rollback facts stand and the failure prose stays private."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (created nil)
         (real-write (symbol-function 'write-region)))
    (cl-letf (((symbol-function 'write-region)
               (lambda (start end filename &rest rest)
                 (if (and (stringp filename)
                          (string-match-p "/\\.dorg/failures/cap-" filename))
                     (progn (setq created (file-exists-p filename))
                            (signal 'file-error
                                    (list "Writing" "RAW-DISK-FULL" filename)))
                   (apply real-write start end filename rest)))))
      (let* ((dorg-sql-capture-failed-statements t)
             (run (dorg-dolt-tests--capture-run store '("POISON")))
             (data (plist-get run :data))
             (printed (format "%s %S" (plist-get run :message) data)))
        (should created)
        (should (eq 'failed (plist-get data :capture)))
        (should-not (plist-member data :capture-id))
        (should-not (dorg-dolt-tests--capture-files store))
        (should (equal "ROLLBACK" (car (last (plist-get run :queries)))))
        (should (eq t (plist-get data :rollback-confirmed)))
        (should-not (string-match-p "RAW-" printed))))))

(ert-deftest dorg-test-dolt-failure-capture-needs-confirmed-rollback ()
  "Diagnostic file I/O never precedes a confirmed rollback.
Without confirmation the facts say `:capture skipped' and nothing under the
store is created or touched; with confirmation the same call writes the
capture."
  :tags '(dolt iob)
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (dorg-sql--capture-store-dir store)
         (err (list 'mysql-query-error "[1105] RAW-PRIVATE server prose")))
    (cl-letf (((symbol-function 'make-directory)
               (lambda (&rest _) (ert-fail "file I/O before a confirmed rollback")))
              ((symbol-function 'make-temp-file)
               (lambda (&rest _) (ert-fail "file I/O before a confirmed rollback"))))
      (let ((facts (dorg-sql--failed-statement-facts
                    err 'statements "POISON" 1 nil)))
        (should (eq 'skipped (plist-get facts :capture)))
        (should-not (plist-member facts :capture-id))
        (should (= 1 (plist-get facts :statement-index)))))
    (should-not (file-exists-p (expand-file-name ".dorg" store)))
    (let ((facts (dorg-sql--failed-statement-facts err 'statements "POISON" 1 t)))
      (should (eq 'written (plist-get facts :capture)))
      (should (= 1 (length (dorg-dolt-tests--capture-files store)))))))

(defun dorg-dolt-tests--capture-envelope (err)
  "Return ERR's public refusal as the parsed JSON error envelope plus its text.
ERR is a `dorg-mutate-error'; the envelope goes through the real
`dorg-envelope-error-json' and `json-encode', then `json-parse-string'."
  (let* ((encoded (json-encode
                   (dorg-envelope-error-json
                    (dorg-mutate-error-reason err) (cadr err)
                    (dorg-mutate-error-data err))))
         (parsed (json-parse-string encoded :object-type 'alist
                                    :array-type 'list :null-object nil
                                    :false-object :json-false)))
    (list :text encoded :parsed parsed
          :details (alist-get 'details (alist-get 'error parsed)))))

(ert-deftest dorg-test-dolt-failure-public-envelope-round-trips-closed-facts ()
  "The real receipt survives refusal, envelope, JSON encoding and parsing intact.
For a known table, an unknown table and an unknown verb, with the capture on
and off, the real executor's rolled-back receipt drives
`dorg-mutate--rolled-back-decision', whose refusal goes through
`dorg-envelope-error-json' and `json-encode'; parsing the result back yields
the closed facts as JSON values (the class as an array), and never the
statement text.  A non-statement failure carries no index and no class."
  :tags '(dolt iob)
  (require 'dorg-mutate)
  (dolist (opt-in '(nil t))
    (dolist (case '(("DELETE FROM headings WHERE id = 1" ("delete" "headings"))
                    ("UPDATE `RAW_SENTINEL_TABLE` SET x = 1" ("update" "unknown"))
                    ("CALL RAW_SENTINEL_PROC(1)" ("unknown" nil))))
      (let* ((store (dorg-dolt-tests--capture-scratch-store))
             (dorg-sql-capture-failed-statements opt-in)
             (poison (car case))
             (run (dorg-dolt-tests--capture-run
                   store (list "INSERT INTO headings (id) VALUES (1)" poison)
                   :poison poison))
             (plan (list :caller-base "b0" :plan-base "b0"
                         :root-scope '(1) :identity-scope '((1 1 1))))
             (err (should-error
                   (dorg-mutate--rolled-back-decision
                    (plist-get run :error) plan "h1" nil nil 0)
                   :type 'dorg-mutate-error))
             (envelope (dorg-dolt-tests--capture-envelope err))
             (details (plist-get envelope :details))
             (text (plist-get envelope :text)))
        (should (eq :json-false (alist-get 'ok (plist-get envelope :parsed))))
        (should (equal "database-failure" (alist-get 'detail details)))
        (should (equal 2 (alist-get 'statement-index details)))
        (should (equal (cadr case) (alist-get 'statement-class details)))
        (if opt-in
            (progn (should (equal "written" (alist-get 'capture details)))
                   (should (stringp (alist-get 'capture-id details))))
          (should-not (assq 'capture details)))
        (should-not (string-match-p "sentinel" (downcase text)))
        (should-not (string-match-p "RAW-" text)))))
  ;; A validation-phase failure after the statements: the envelope carries
  ;; neither an ordinal nor a class, and no validator prose.
  (let* ((store (dorg-dolt-tests--capture-scratch-store))
         (dorg-sql-capture-failed-statements t)
         (receipt
          (cl-letf (((symbol-function 'mysql-query) (lambda (_conn _sql) nil)))
            (let ((dorg-sql--capture-store-dir store))
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("INSERT INTO headings (id) VALUES (1)")
                "typed"
                (lambda (_query) (error "RAW-PRIVATE validator says no"))
                t)
               :type 'dorg-sql-transaction-rolled-back))))
         (plan (list :caller-base "b0" :plan-base "b0"
                     :root-scope '(1) :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision receipt plan "h1" nil nil 0)
               :type 'dorg-mutate-error))
         (envelope (dorg-dolt-tests--capture-envelope err))
         (details (plist-get envelope :details)))
    (should (equal "validate" (alist-get 'transaction-phase details)))
    (should-not (assq 'statement-index details))
    (should-not (assq 'statement-class details))
    (should-not (assq 'capture details))
    (should-not (string-match-p "RAW-" (plist-get envelope :text)))
    (should-not (dorg-dolt-tests--capture-files store))))

(provide 'dorg-dolt-tests)
;;; dorg-dolt-tests.el ends here
