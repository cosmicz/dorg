;;; dorg-ext-tests.el --- Tests for dorg-ext -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dorg extension API and registration entry point.

;;; Code:

(require 'ert)
(require 'dorg-ext)
(require 'dorg)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; Helpers

(defmacro dorg-test-with-clean-extensions (&rest body)
  "Execute BODY with `dorg--extensions' let-bound to nil.
Restores the original registry after BODY."
  (declare (indent 0))
  `(let ((dorg--extensions nil))
     ,@body))

(defmacro dorg-test-with-clean-registry (&rest body)
  "Execute BODY with `dorg--registry' let-bound to nil."
  (declare (indent 0))
  `(let ((dorg--registry nil))
     ,@body))

;;; Unit Tests — Extension Registry

(ert-deftest dorg-test-ext-query-fields-share-extension-registration ()
  "Namespaced query fields belong to the extension, including replacement."
  (dorg-test-with-clean-extensions
    (let ((field (list :name 'demo/holder :type 'identity :cardinality 'one
                       :dependencies '((property "HOLDER"))
                       :context '(:pause-states)
                       :eval #'ignore :sql #'ignore)))
      (dorg-register-extension 'demo :schema "SELECT 1" :version 1
                               :fields (list field) :query-context #'ignore)
      (should (equal (plist-get (dorg-ext-query-field 'demo/holder) :type)
                     'identity))
      (should (eq (plist-get (dorg-ext-query-field 'demo/holder) :extension) 'demo))
      (should-not (dorg-ext-query-field 'other/holder))
      (dorg-register-extension 'demo :schema "SELECT 1" :version 1)
      (should-not (dorg-ext-query-field 'demo/holder)))))

(ert-deftest dorg-test-ext-query-fields-reject-invalid-before-registering ()
  "Malformed field declarations cannot enter the live registry."
  (dorg-test-with-clean-extensions
    (dolist (field '((:name other/holder :type string :cardinality one :eval ignore)
                     (:name demo/holder :type bogus :cardinality one :eval ignore)
                     (:name demo/holder :type string :cardinality bogus :eval ignore)
                     (:name demo/holder :type string :cardinality one)
                     (:name demo/holder :type string :cardinality one :eval 3)))
      (should-error (dorg-register-extension 'demo :schema "SELECT 1" :version 1
                                             :fields (list field)))
      (should-not (dorg-get-extension 'demo)))))

(ert-deftest dorg-test-ext-query-fields-reject-nonstored-dependencies ()
  "Field evaluators may depend only on declared stored heading/property values."
  (dorg-test-with-clean-extensions
    (should-error
     (dorg-register-extension
      'demo :schema "SELECT 1" :version 1
      :fields (list (list :name 'demo/name :type 'string :cardinality 'one
                          :dependencies '((field demo/other)) :eval #'ignore))))
    (should-not (dorg-get-extension 'demo))))

(ert-deftest dorg-test-ext-standalone-loader-requires-conventional-feature ()
  "DOrg loads a declared extension entry point, never its application layer."
  (dorg-test-with-clean-extensions
    (let (required)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional _filename _noerror)
                   (setq required feature)
                   (when (eq feature 'dorg-extension-demo)
                     (dorg-register-extension 'demo :schema "SELECT 1" :version 1))
                   feature)))
        (should (dorg-extension-p (dorg-load-extension "demo")))
        (should (eq required 'dorg-extension-demo))))))

(ert-deftest dorg-test-ext-register-basic ()
  "Registering an extension stores it in the registry."
  (dorg-test-with-clean-extensions
    (dorg-register-extension 'demo
                             :schema "CREATE TABLE demo (id INT);"
                             :version 1)
    (let ((ext (dorg-get-extension 'demo)))
      (should ext)
      (should (eq (dorg-extension-name ext) 'demo))
      (should (equal (dorg-extension-schema ext) "CREATE TABLE demo (id INT);"))
      (should (= (dorg-extension-version ext) 1)))))

(ert-deftest dorg-test-d0hyz-registers-optional-producer-resolver ()
  "Policy migration producers are descriptor-owned and optional elsewhere."
  (dorg-test-with-clean-extensions
    (let ((resolver (lambda (_request) '(:project ignore))))
      (dorg-register-extension
       'demo :schema "SELECT 1" :version 1 :producer-resolver resolver)
      (should (eq resolver
                  (dorg-extension-producer-resolver (dorg-get-extension 'demo))))
      ;; Re-registration without the optional seam keeps ordinary extensions
      ;; free of a policy-upgrade producer contract.
      (dorg-register-extension 'demo :schema "SELECT 1" :version 1)
      (should-not (dorg-extension-producer-resolver (dorg-get-extension 'demo)))))
  (dorg-test-with-clean-extensions
    (should-error
     (dorg-register-extension
      'demo :schema "SELECT 1" :version 1 :producer-resolver 'not-a-function))))

(ert-deftest dorg-test-ext-register-replaces ()
  "Re-registering the same name replaces the entry."
  (dorg-test-with-clean-extensions
    (dorg-register-extension 'demo :schema "CREATE TABLE a (id INT);" :version 1)
    (dorg-register-extension 'demo :schema "CREATE TABLE b (id INT);" :version 2)
    (should (= (length (dorg-registered-extensions)) 1))
    (let ((ext (dorg-get-extension 'demo)))
      (should (equal (dorg-extension-schema ext) "CREATE TABLE b (id INT);"))
      (should (= (dorg-extension-version ext) 2)))))

(ert-deftest dorg-test-ext-register-preserves-order ()
  "Multiple extensions are returned in registration order."
  (dorg-test-with-clean-extensions
    (dorg-register-extension 'alpha :schema "SELECT 1;" :version 1)
    (dorg-register-extension 'bravo :schema "SELECT 2;" :version 1)
    (dorg-register-extension 'charlie :schema "SELECT 3;" :version 1)
    (should (equal (mapcar #'dorg-extension-name (dorg-registered-extensions))
                   '(alpha bravo charlie)))))

(ert-deftest dorg-test-ext-register-requires-schema ()
  "Registering without a :schema string signals an error."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension 'bad :version 1))))

(ert-deftest dorg-test-ext-register-requires-version ()
  "Registering without a :version integer signals an error."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension 'bad :schema "SELECT 1;"))))

(ert-deftest dorg-test-ext-register-rejects-invalid-names ()
  "Registering an extension with an invalid name signals an error."
  (dorg-test-with-clean-extensions
    ;; SQL injection attempt
    (should-error (dorg-register-extension
                   (intern "foo'; DROP TABLE headings; --")
                   :schema "SELECT 1;" :version 1))
    ;; Leading digit
    (should-error (dorg-register-extension
                   '1bad :schema "SELECT 1;" :version 1))
    ;; Empty name
    (should-error (dorg-register-extension
                   (intern "") :schema "SELECT 1;" :version 1))
    ;; Special characters
    (should-error (dorg-register-extension
                   (intern "bad.name") :schema "SELECT 1;" :version 1))
    (should-error (dorg-register-extension
                   (intern "bad name") :schema "SELECT 1;" :version 1))
    ;; Non-symbol
    (should-error (dorg-register-extension
                   "string-name" :schema "SELECT 1;" :version 1))))

(ert-deftest dorg-test-ext-register-accepts-valid-names ()
  "Valid extension names are accepted."
  (dorg-test-with-clean-extensions
    (should (dorg-register-extension 'demo :schema "SELECT 1;" :version 1))
    (should (dorg-register-extension 'my-ext :schema "SELECT 1;" :version 1))
    (should (dorg-register-extension 'my_ext2 :schema "SELECT 1;" :version 1))
    (should (dorg-register-extension 'MixedCase :schema "SELECT 1;" :version 1))))

(defconst dorg-test-85e9w--membership-table
  '(:name "assignment_rows"
    :key-column "subject_id"
    :scope-column "subject_id"
    :memberships
    ((:name assignee-at
      :member-key "subject_id"
      :position-column "ordinal"
      :value-column "assignee"
      :bridge-table "subjects"
      :bridge-key "subject_id"
      :heading-column "heading_id"
      :root-column "root_id")))
  "Valid literal membership descriptor used by registration REDs.")

(ert-deftest dorg-test-85e9w-register-accepts-literal-membership-descriptor ()
  "A closed literal membership descriptor registers without a callback."
  (dorg-test-with-clean-extensions
    (let* ((tables (list dorg-test-85e9w--membership-table
                         '(:name "subjects"
                           :key-column "subject_id"
                           :scope-column "root_id")))
           (ext (dorg-register-extension
                 'demo :schema "SELECT 1" :version 1
                 :read-tables tables)))
      (should (equal tables (dorg-extension-read-tables ext))))))

(ert-deftest dorg-test-85e9w-register-rejects-unknown-read-descriptor-key ()
  "A misspelled declaration key refuses at registration, never at query time."
  (dorg-test-with-clean-extensions
    (should-error
     (dorg-register-extension
      'demo :schema "SELECT 1" :version 1
      :read-tables
      (list (append dorg-test-85e9w--membership-table
                    '(:positon-column "ordinal")))))))

(ert-deftest dorg-test-85e9w-register-rejects-every-unsafe-membership-identifier ()
  "Every identifier DOrg will splice is validated when the extension registers."
  (dolist (key '(:member-key :position-column :value-column :bridge-table
                 :bridge-key :heading-column :root-column))
    (dorg-test-with-clean-extensions
      (let* ((table (copy-tree dorg-test-85e9w--membership-table))
             (memberships (plist-get table :memberships))
             (membership (copy-sequence (car memberships))))
        (setq membership (plist-put membership key "bad.name"))
        (setq table (plist-put table :memberships (list membership)))
        (should-error
         (dorg-register-extension
          'demo :schema "SELECT 1" :version 1
          :read-tables (list table)))))))

(ert-deftest dorg-test-85e9w-register-rejects-duplicate-membership-name ()
  "One extension cannot register two ambiguous declarations for one relation."
  (dorg-test-with-clean-extensions
    (let* ((first (copy-tree dorg-test-85e9w--membership-table))
           (second (copy-tree dorg-test-85e9w--membership-table)))
      (setq second (plist-put second :name "other_assignment_rows"))
      (should-error
       (dorg-register-extension
        'demo :schema "SELECT 1" :version 1
        :read-tables (list first second))))))

(ert-deftest dorg-test-85e9w-register-rejects-missing-or-duplicate-descriptor-fields ()
  "Membership SQL facts are complete and single-valued at registration."
  (dorg-test-with-clean-extensions
    (let* ((table (copy-tree dorg-test-85e9w--membership-table))
           (membership (copy-sequence (car (plist-get table :memberships)))))
      (setq membership
            (cl-loop for (key value) on membership by #'cddr
                     unless (eq key :root-column)
                     append (list key value)))
      (setq table (plist-put table :memberships (list membership)))
      (should-error
       (dorg-register-extension
        'demo :schema "SELECT 1" :version 1 :read-tables (list table)))))
  (dorg-test-with-clean-extensions
    (let ((table (append (copy-tree dorg-test-85e9w--membership-table)
                         '(:name "duplicate_name"))))
      (should-error
       (dorg-register-extension
        'demo :schema "SELECT 1" :version 1 :read-tables (list table))))))

(ert-deftest dorg-test-ext-unregister ()
  "`dorg-unregister-extension' removes an extension."
  (dorg-test-with-clean-extensions
    (dorg-register-extension 'demo :schema "SELECT 1;" :version 1)
    (should (dorg-get-extension 'demo))
    (should (dorg-unregister-extension 'demo))
    (should (null (dorg-get-extension 'demo)))
    (should (null (dorg-unregister-extension 'demo)))))

;;; Unit Tests — Schema Spec and Migration Validation

(ert-deftest dorg-test-ext-register-accepts-schema-list ()
  "A :schema may be a list of SQL strings (real extensions need many tables)."
  (dorg-test-with-clean-extensions
    (let ((schema '("CREATE TABLE a (id INT);" "CREATE TABLE b (id INT);")))
      (dorg-register-extension 'demo :schema schema :version 1)
      (should (equal (dorg-extension-schema (dorg-get-extension 'demo)) schema)))))

(ert-deftest dorg-test-ext-register-stores-migrations ()
  "A :migrations alist is stored on the extension."
  (dorg-test-with-clean-extensions
    (let ((migs '((1 . "ALTER TABLE demo_t ADD COLUMN label TEXT")
                  (2 . ("ALTER TABLE demo_t ADD COLUMN note TEXT")))))
      (dorg-register-extension 'demo
                               :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);"
                               :version 3
                               :migrations migs)
      (should (equal (dorg-extension-migrations (dorg-get-extension 'demo)) migs)))))

(ert-deftest dorg-test-ext-register-accepts-function-migration ()
  "A migration step may be a function or a deferred (unbound) function name."
  (dorg-test-with-clean-extensions
    (should (dorg-register-extension
             'demo :schema "SELECT 1;" :version 2
             :migrations (list (cons 1 (lambda (_dir) "SELECT 1;")))))
    ;; A bare symbol naming a not-yet-defined function is accepted at
    ;; registration time and validated lazily when the migration runs.
    (should (dorg-register-extension
             'demo2 :schema "SELECT 1;" :version 2
             :migrations '((1 . demo2--migrate-1-to-2))))))

(ert-deftest dorg-test-ext-register-rejects-bad-version ()
  "Versions must be positive integers."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension 'demo :schema "SELECT 1;" :version 0))
    (should-error (dorg-register-extension 'demo :schema "SELECT 1;" :version -1))
    (should-error (dorg-register-extension 'demo :schema "SELECT 1;" :version 1.5))))

(ert-deftest dorg-test-ext-register-rejects-bad-schema ()
  "A :schema must be a SQL string or a list of SQL strings."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension 'demo :schema 42 :version 1))
    (should-error (dorg-register-extension 'demo :schema '("ok" 99) :version 1))))

(ert-deftest dorg-test-ext-register-rejects-bad-migration-key ()
  "Migration from-version keys must be positive integers."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension
                   'demo :schema "SELECT 1;" :version 2
                   :migrations '(("1" . "SELECT 1;"))))
    (should-error (dorg-register-extension
                   'demo :schema "SELECT 1;" :version 2
                   :migrations '((0 . "SELECT 1;"))))))

(ert-deftest dorg-test-ext-register-rejects-bad-migration-step ()
  "Migration steps must be SQL string(s) or a function."
  (dorg-test-with-clean-extensions
    (should-error (dorg-register-extension
                   'demo :schema "SELECT 1;" :version 2
                   :migrations '((1 . 42))))
    (should-error (dorg-register-extension
                   'demo :schema "SELECT 1;" :version 2
                   :migrations '((1 . ("ok" 7)))))
    ;; Not an alist
    (should-error (dorg-register-extension
                   'demo :schema "SELECT 1;" :version 2
                   :migrations '(1 2 3)))))

(ert-deftest dorg-test-ext-normalize-sql ()
  "`dorg-ext-normalize-sql' returns a list of SQL strings or errors."
  (should (equal (dorg-ext-normalize-sql "SELECT 1;") '("SELECT 1;")))
  (should (equal (dorg-ext-normalize-sql '("a" "b")) '("a" "b")))
  (should-error (dorg-ext-normalize-sql 42))
  (should-error (dorg-ext-normalize-sql '("ok" 7))))

;;; Unit Tests — Registration

(ert-deftest dorg-test-register-stores-mapping ()
  "`dorg-bind-dolt' stores the file→dolt-dir mapping."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-reg-test-" t))
           (base-dir (file-name-as-directory temp-base))
           (dolt-dir (expand-file-name "dolt/" base-dir))
           (org-file (expand-file-name "doc.org" base-dir)))
      (unwind-protect
          (dorg-test-with-clean-registry
            (dorg-bind-dolt org-file dolt-dir)
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (should (equal (dorg-registered-dolt-dir)
                             (file-name-as-directory
                              (file-truename dolt-dir))))
              (should (integerp (dorg-backend-root-id
                                 (dorg-registered-backend))))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-register-allocates-root ()
  "`dorg-bind-dolt' inserts a root heading and dorg_files row."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-reg-test-" t))
           (base-dir (file-name-as-directory temp-base))
           (dolt-dir (expand-file-name "dolt/" base-dir))
           (org-file (expand-file-name "doc.org" base-dir)))
      (unwind-protect
          (dorg-test-with-clean-registry
            (let* ((reg (dorg-bind-dolt org-file dolt-dir))
                   (root-id (dorg-backend-root-id (dorg-registration-backend reg))))
              (should (integerp root-id))
              ;; Root heading row exists with level_offset=0, parent NULL
              (let ((rows (dorg-sql
                           dolt-dir
                           "SELECT level_offset, parent_id FROM headings WHERE id = %d"
                           root-id)))
                (should (= (length rows) 1))
                (should (= (alist-get 'level_offset (car rows)) 0))
                (should (null (alist-get 'parent_id (car rows)))))
              ;; dorg_files row binds a store-relative path → root_id.
              (let ((rows (dorg-sql
                           dolt-dir
                           "SELECT relative_path FROM dorg_files WHERE root_id = %d"
                           root-id)))
                (should (= (length rows) 1))
                (should (equal (alist-get 'relative_path (car rows))
                               (dorg-repo-encode-relative-path
                                dolt-dir org-file))))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-register-reuses-root ()
  "Re-registering the same file returns the existing root_id."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-reg-test-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (org-file (expand-file-name "doc.org" temp-base)))
      (unwind-protect
          (dorg-test-with-clean-registry
            (let ((reg-1 (dorg-bind-dolt org-file dolt-dir))
                  (reg-2 (dorg-bind-dolt org-file dolt-dir)))
              (should (= (dorg-backend-root-id (dorg-registration-backend reg-1))
                         (dorg-backend-root-id (dorg-registration-backend reg-2)))))
            ;; Exactly one dorg_files row
            (let ((rows (dorg-sql
                         dolt-dir
                         "SELECT COUNT(*) AS c FROM dorg_files")))
              (should (= (alist-get 'c (car rows)) 1))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-buffer-dolt-dir-file-keyword ()
  "`dorg--buffer-dolt-dir' reads a file-level #+DORG_DIR keyword."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DIR: /tmp/my-dolt\n* Heading\nBody.\n")
    (should (equal (dorg--buffer-dolt-dir) "/tmp/my-dolt"))))

(ert-deftest dorg-test-buffer-dolt-dir-ignores-example-block ()
  "`dorg--buffer-dolt-dir' ignores #+DORG_DIR inside example blocks."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n#+begin_example\n#+DORG_DIR: /tmp/fake\n#+end_example\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-register-unknown-buffer ()
  "`dorg-registered-dolt-dir' returns nil for unregistered buffers."
  (dorg-test-with-clean-registry
    (with-temp-buffer
      (should (null (dorg-registered-dolt-dir))))))

(ert-deftest dorg-test-unregister-removes-mapping ()
  "`dorg-unregister' removes the file from the registry."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-reg-test-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (org-file (expand-file-name "doc.org" temp-base)))
      (unwind-protect
          (dorg-test-with-clean-registry
            (let ((dorg-sql-transport 'server))
              (dorg-bind-dolt org-file dolt-dir))
            (dorg-unregister org-file)
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (should (null (dorg-registered-dolt-dir)))))
        (delete-directory temp-base t)))))

;;; Integration Tests — Extension Schemas

(ert-deftest dorg-test-ext-init-applies-schema ()
  "Extension schemas are applied at init time."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-ext-test-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base)))
      (unwind-protect
          (dorg-test-with-clean-extensions
            (dorg-register-extension
             'demo
             :schema "CREATE TABLE demo_items (id INT PRIMARY KEY, label TEXT);"
             :version 1)
            (dorg-init dolt-dir)
            ;; Extension table should exist and be queryable
            (dorg-sql dolt-dir
                      "INSERT INTO demo_items (id, label) VALUES (1, 'hello')")
            (let ((result (dorg-sql dolt-dir
                                    "SELECT id, label FROM demo_items")))
              (should (= (length result) 1))
              (should (equal (alist-get 'label (car result)) "hello")))
            ;; Version should be recorded
            (let ((result (dorg-sql dolt-dir
                                    "SELECT value FROM dorg_meta WHERE `key` = 'ext:demo:version'")))
              (should (equal (alist-get 'value (car result)) "1"))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-ext-init-no-extensions ()
  "Init with no registered extensions works fine."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-ext-test-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base)))
      (unwind-protect
          (dorg-test-with-clean-extensions
            (dorg-init dolt-dir)
            ;; Core tables should still exist
            (let ((result (dorg-sql dolt-dir
                                    "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'")))
              (should (equal (alist-get 'value (car result))
                             (number-to-string dorg-schema-version)))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-ext-init-explicit-extensions ()
  "Init with explicit extensions argument overrides the registry."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (make-temp-file "dorg-ext-test-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base)))
      (unwind-protect
          (dorg-test-with-clean-extensions
            ;; Register one, pass another explicitly
            (dorg-register-extension 'registered
                                     :schema "CREATE TABLE registered_t (id INT);"
                                     :version 1)
            (let ((explicit (list (make-dorg-extension
                                   :name 'explicit
                                   :schema "CREATE TABLE explicit_t (id INT);"
                                   :version 2))))
              (dorg-init dolt-dir explicit))
            ;; Only the explicit one should exist
            (let ((tables (mapcar
                           (lambda (row) (alist-get 'TABLE_NAME row))
                           (dorg-sql dolt-dir
                                     "SELECT table_name FROM information_schema.tables WHERE table_schema = database() AND table_type = 'BASE TABLE'"))))
              (should (member "explicit_t" tables))
              (should-not (member "registered_t" tables))))
        (delete-directory temp-base t)))))

;;; Integration Tests — Explicit Extension Migration (install / upgrade)

(defmacro dorg-test-ext--with-core-repo (dolt-var &rest body)
  "Run BODY over a fresh core-only Dolt repo with a clean extension registry.
DOLT-VAR is bound to the repo path.  Skips when dolt is unavailable."
  (declare (indent 1))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db ,dolt-var
       (dorg-test-with-clean-extensions
         ,@body))))

(defun dorg-test-ext--db-version (dolt-dir name)
  "Return the recorded `ext:NAME:version' string in DOLT-DIR, or nil."
  (alist-get 'value
             (car (dorg-sql dolt-dir
                            "SELECT value FROM dorg_meta WHERE `key` = %s"
                            (dorg-sql-quote (format "ext:%s:version" name))))))

(ert-deftest dorg-test-ext-ensure-installs-on-existing-repo ()
  "An existing core-only repo acquires a newly registered extension."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo
     :schema "CREATE TABLE demo_t (id INT PRIMARY KEY, label TEXT);"
     :version 1)
    (dorg-migrate-schema dolt-dir)
    ;; Table created and usable.
    (dorg-sql dolt-dir "INSERT INTO demo_t (id, label) VALUES (1, 'x')")
    (should (= 1 (length (dorg-sql dolt-dir "SELECT id FROM demo_t"))))
    ;; Version recorded without manual SQL.
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "1"))))

(ert-deftest dorg-test-ext-migrate-noop-when-current ()
  "Explicit migration is a no-op when the extension is already current.
Re-applying the fresh CREATE TABLE schema would error, so survival of
seeded data proves the schema was not reapplied.  A current store returns
before the clean-working-set check: no schema step can run, so no reset risk
exists for unrelated uncommitted data."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    (dorg-sql dolt-dir "INSERT INTO demo_t (id) VALUES (42)")
    ;; Second migration: version equals registered, so no-op and no error even
    ;; with an unrelated dirty row.  It must not apply or commit a schema batch.
    (let ((batches 0))
      (cl-letf* ((orig-batch (symbol-function 'dorg-sql--lease-batch))
                 ((symbol-function 'dorg-sql--lease-batch)
                  (lambda (&rest args)
                    (cl-incf batches)
                    (apply orig-batch args))))
        (dorg-migrate-schema dolt-dir))
      (should (= batches 0)))
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "1"))
    (should (= 42 (alist-get 'id (car (dorg-sql dolt-dir
                                                "SELECT id FROM demo_t")))))))

(ert-deftest dorg-test-ext-ensure-upgrades-v1-to-v2 ()
  "A lower stored version triggers the registered migration chain.
The migration also backfills data to prove data migrations run."
  (dorg-test-ext--with-core-repo dolt-dir
    ;; Install at v1 and seed a row.  Commit the seed so ensure runs against
    ;; a clean working set at HEAD, as the real bind/open lifecycle does.
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    (dorg-sql dolt-dir "INSERT INTO demo_t (id) VALUES (1)")
    (dorg-commit dolt-dir "seed demo row")
    ;; Re-register at v2 with a migration that adds and backfills a column.
    (dorg-register-extension
     'demo
     :schema "CREATE TABLE demo_t (id INT PRIMARY KEY, label TEXT);"
     :version 2
     :migrations '((1 . ("ALTER TABLE demo_t ADD COLUMN label TEXT"
                         "UPDATE demo_t SET label = 'backfilled'"))))
    (dorg-migrate-schema dolt-dir)
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "2"))
    (should (equal (alist-get 'label (car (dorg-sql dolt-dir
                                                    "SELECT label FROM demo_t WHERE id = 1")))
                   "backfilled"))))

(ert-deftest dorg-test-ext-migrate-chains-v1-to-v3 ()
  "Explicit migration chains multiple steps until the registered version.
Includes a function-valued step to exercise the callable contract."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    (dorg-register-extension
     'demo
     :schema "CREATE TABLE demo_t (id INT PRIMARY KEY, a TEXT, b TEXT);"
     :version 3
     :migrations
     (list (cons 1 "ALTER TABLE demo_t ADD COLUMN a TEXT")
           ;; Function step: receives the dolt-dir, returns SQL.
           (cons 2 (lambda (_dir) "ALTER TABLE demo_t ADD COLUMN b TEXT"))))
    (dorg-migrate-schema dolt-dir)
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "3"))
    ;; Both columns exist (queries would error otherwise).
    (dorg-sql dolt-dir "SELECT a, b FROM demo_t LIMIT 1")))

(ert-deftest dorg-test-ext-ensure-errors-on-missing-migration ()
  "A missing migration step is an actionable error; version is unchanged."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    ;; Re-register at v2 with no migration step for v1 -> v2.
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 2)
    (should-error (dorg-migrate-schema dolt-dir) :type 'error)
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "1"))))

(ert-deftest dorg-test-ext-ensure-errors-on-future-version ()
  "A stored version above the registered version errors without downgrade."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    ;; Force a future recorded version.
    (dorg-sql dolt-dir
              "UPDATE dorg_meta SET value = '5' WHERE `key` = 'ext:demo:version'")
    (dorg-commit dolt-dir "force future ext version for test")
    (should-error (dorg-migrate-schema dolt-dir) :type 'error)
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "5"))))

(ert-deftest dorg-test-ext-migrate-leaves-unregistered-metadata ()
  "Explicit migration does not touch metadata or tables for unregistered extensions."
  (dorg-test-ext--with-core-repo dolt-dir
    ;; Simulate a foreign extension installed by some other tooling.
    (dorg-sql dolt-dir "CREATE TABLE ghost_t (id INT PRIMARY KEY)")
    (dorg-sql dolt-dir
              "INSERT INTO dorg_meta (`key`, value) VALUES ('ext:ghost:version', '1')")
    (dorg-commit dolt-dir "seed foreign extension")
    ;; Only 'demo is registered.
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    ;; Ghost metadata and table untouched.
    (should (equal (dorg-test-ext--db-version dolt-dir "ghost") "1"))
    (dorg-sql dolt-dir "SELECT id FROM ghost_t LIMIT 1")))

(ert-deftest dorg-test-ext-ensure-schema-refuses-absent-extension ()
  "`dorg-repo-ensure-schema' refuses an absent extension without installing it."
  (dorg-test-ext--with-core-repo dolt-dir
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (should-error (dorg-repo-ensure-schema dolt-dir)
                  :type 'dorg-schema-migration-required)
    (should-not (dorg-test-ext--db-version dolt-dir "demo"))
    (should-error (dorg-sql dolt-dir "SELECT id FROM demo_t LIMIT 1")
                  :type 'error)))

(ert-deftest dorg-test-ext-ensure-migration-rolls-back-on-mid-step-failure ()
  "A multi-statement migration that fails partway rolls back atomically.
The first statement (an ALTER that adds and backfills a column) succeeds,
the second references a nonexistent table and fails.  The version bump is
the last statement and never runs.  Afterward neither the schema/data
change nor the recorded version may persist — they share the same
transaction/rollback path."
  (dorg-test-ext--with-core-repo dolt-dir
    ;; Install at v1 and seed a committed row.  Committing models the explicit
    ;; migration lifecycle, where migrations run against a working set at HEAD;
    ;; the failure reset discards working-set drift back to that committed state.
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    (dorg-sql dolt-dir "INSERT INTO demo_t (id) VALUES (1)")
    (dorg-commit dolt-dir "seed demo row")
    ;; Re-register at v2 with a two-statement migration whose second
    ;; statement fails after the first has already altered the table.
    (dorg-register-extension
     'demo
     :schema "CREATE TABLE demo_t (id INT PRIMARY KEY, label TEXT);"
     :version 2
     :migrations
     '((1 . ("ALTER TABLE demo_t ADD COLUMN label TEXT"
             "INSERT INTO nonexistent_table (x) VALUES (1)"))))
    (should-error (dorg-migrate-schema dolt-dir) :type 'error)
    ;; Version must remain at v1 — the metadata bump never committed.
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "1"))
    ;; The partial DDL must have rolled back: the column is gone, so the
    ;; original schema is intact and re-running ensure could ALTER cleanly.
    (should-error (dorg-sql dolt-dir "SELECT label FROM demo_t") :type 'error)
    ;; Seeded data survives unchanged.
    (should (= 1 (alist-get 'id (car (dorg-sql dolt-dir
                                               "SELECT id FROM demo_t")))))))

(ert-deftest dorg-test-ext-ensure-rejects-dirty-working-set ()
  "Explicit extension migration refuses to run a step on a dirty Dolt working set.
The failure path issues `DOLT_RESET --hard', which discards the *entire*
working set — so without a clean-working-set precondition a failing
migration could erase unrelated uncommitted work.  A preflight rejects
before touching schema/data, leaving the pre-existing uncommitted change
intact and the recorded version unadvanced."
  (dorg-test-ext--with-core-repo dolt-dir
    ;; Install demo v1 and commit so the repo starts clean at HEAD.
    (dorg-register-extension
     'demo :schema "CREATE TABLE demo_t (id INT PRIMARY KEY);" :version 1)
    (dorg-migrate-schema dolt-dir)
    ;; Pre-existing uncommitted working-set change, unrelated to the migration.
    (dorg-sql dolt-dir "INSERT INTO demo_t (id) VALUES (99)")
    ;; Re-register at v2 with a migration that would fail mid-step — and thus
    ;; trigger the hard-reset failure path — if the preflight let it run.
    (dorg-register-extension
     'demo
     :schema "CREATE TABLE demo_t (id INT PRIMARY KEY, label TEXT);"
     :version 2
     :migrations
     '((1 . ("ALTER TABLE demo_t ADD COLUMN label TEXT"
             "INSERT INTO nonexistent_table (x) VALUES (1)"))))
    ;; Explicit migration must reject because the working set is dirty.
    (should-error (dorg-migrate-schema dolt-dir) :type 'error)
    ;; Version unchanged — no migration step ran.
    (should (equal (dorg-test-ext--db-version dolt-dir "demo") "1"))
    ;; Migration DDL never touched the schema.
    (should-error (dorg-sql dolt-dir "SELECT label FROM demo_t") :type 'error)
    ;; The pre-existing uncommitted row survives — it was not hard-reset away.
    (should (= 99 (alist-get 'id (car (dorg-sql dolt-dir
                                                "SELECT id FROM demo_t WHERE id = 99")))))))

;;; Integration Tests — Materialize + Hooks

(ert-deftest dorg-test-materialize-renders-buffer ()
  "`dorg-materialize' renders the repo into the current buffer."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let ((org-file (expand-file-name "doc.org" temp-base)))
        (dorg-test-with-clean-registry
          (dorg-test-with-clean-extensions
            ;; Register first → allocates root_id
            (let* ((reg (dorg-bind-dolt org-file dolt-dir))
                   (root-id (dorg-backend-root-id (dorg-registration-backend reg))))
              ;; Import content under the registered root
              (let ((stmts (dorg-test--parse-to-sql
                            "* Hello\nBody text.\n" root-id t)))
                (dorg-sql-batch dolt-dir stmts)
                (dorg-commit dolt-dir "seed")))
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (dorg-materialize)
              (should (string-match-p "^\\* Hello" (buffer-string)))
              (should (string-match-p "Body text" (buffer-string))))))))))

(ert-deftest dorg-test-materialize-unregistered-errors ()
  "`dorg-materialize' on an unregistered buffer signals an error."
  (dorg-test-with-clean-registry
    (with-temp-buffer
      (should-error (dorg-materialize)))))

(ert-deftest dorg-test-materialize-runs-hook ()
  "`dorg-after-materialize-functions' runs after render."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let ((org-file (expand-file-name "doc.org" temp-base))
            (hook-calls nil))
        (dorg-test-with-clean-registry
          (dorg-test-with-clean-extensions
            (let* ((reg (dorg-bind-dolt org-file dolt-dir))
                   (root-id (dorg-backend-root-id (dorg-registration-backend reg))))
              (let ((stmts (dorg-test--parse-to-sql "* X\n" root-id t)))
                (dorg-sql-batch dolt-dir stmts)
                (dorg-commit dolt-dir "seed")))
            (let ((dorg-after-materialize-functions
                   (list (lambda (dd buf)
                           (push (cons dd (buffer-name buf)) hook-calls)))))
              (with-temp-buffer
                (set-visited-file-name org-file t)
                (dorg-materialize)))
            (should (= (length hook-calls) 1))))))))

;;; Minor Mode

(ert-deftest dorg-test-mode-enables ()
  "`dorg-mode' can be toggled on an org buffer."
  (with-temp-buffer
    (org-mode)
    (dorg-mode 1)
    (should dorg-mode)
    (dorg-mode -1)
    (should-not dorg-mode)))

(ert-deftest dorg-test-reconciliation-hooks-retired ()
  "Whole-file reconciliation hooks are absent, not inert compatibility state."
  (should-not (boundp 'dorg-before-sync-functions))
  (should-not (boundp 'dorg-after-sync-functions)))

(provide 'dorg-ext-tests)
;;; dorg-ext-tests.el ends here
