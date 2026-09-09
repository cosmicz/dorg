;;; dorg-import-tests.el --- Explicit document import tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Specifications for the administrative whole-document import boundary.
;; Import is deliberately separate from deprecated buffer reconciliation:
;; callers declare the complete source, target, repository, mode, and (for
;; replace) exact base revision.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg-ext)
(require 'dorg-import)
(require 'dorg-model)
(require 'dorg-model-load)
(require 'dorg-mode)
(require 'dorg-parse)
(require 'dorg-render)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)

(ert-deftest dorg-test-import-assigns-ids-only-in-parse-model ()
  "ID completion mutates only the isolated parse model, never source bytes."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n** Two\n")
    (let* ((before (buffer-string))
           (next 100)
           (parsed (dorg-parse-buffer))
           (writeback
            (dorg-parse-assign-ids parsed (lambda () (prog1 next (cl-incf next)))))
           (headings (dorg-parse-result-headings parsed)))
      (should (equal (buffer-string) before))
      (should (equal (mapcar #'dorg-heading-id headings) '(100 101 102)))
      (should (equal (mapcar #'dorg-heading-parent-id (cdr headings))
                     '(100 101)))
      (should (equal (mapcar #'car writeback) '(101 102)))
      (should-not (string-match-p ":DORG_ID:" (buffer-string))))))

(ert-deftest dorg-test-import-id-completion-rejects-duplicates ()
  "A duplicate authored DORG_ID fails before any store operation."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n:PROPERTIES:\n:DORG_ID: 42\n:END:\n"
            "* Two\n:PROPERTIES:\n:DORG_ID: 42\n:END:\n")
    (let ((parsed (dorg-parse-buffer nil 1)))
      (should-error
       (dorg-parse-assign-ids parsed (lambda () 99))
       :type 'dorg-parse-duplicate-id))))

(ert-deftest dorg-test-import-requires-complete-explicit-request ()
  "The public boundary rejects missing repository/target/mode fields."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n")
    (should-error (dorg-import-document (current-buffer))
                  :type 'dorg-import-request-error)
    (should-error
     (dorg-import-document
      (current-buffer)
      :dolt-dir "/tmp/dorg-import-missing-target"
      :mode 'bootstrap)
     :type 'dorg-import-request-error)))

(ert-deftest dorg-test-import-rejects-narrowed-source-before-repository-write ()
  "A narrowed source is refused before initialization or schema mutation."
  (with-temp-buffer
    (org-mode)
    (insert "* One\nBody\n* Two\n")
    (setq buffer-file-name "/tmp/dorg-import-narrowed-source.org")
    (narrow-to-region (point-min) (save-excursion
                                    (goto-char (point-min))
                                    (outline-next-heading)
                                    (point)))
    (let ((store-touched nil))
      (cl-letf (((symbol-function 'dorg-import--ensure-repository)
                 (lambda (&rest _) (setq store-touched t))))
        (should-error
         (dorg-import-document
          (current-buffer)
          :dolt-dir "/tmp/dorg-import-narrowed"
          :target-file "/tmp/dorg-import-target.org"
          :mode 'bootstrap)
         :type 'dorg-import-source-error)
        (should-not store-touched)))))

(ert-deftest dorg-test-import-rejects-bootstrap-base-before-repository-write ()
  "A bootstrap-only option error cannot initialize or migrate a repository."
  (let ((store-touched nil))
    (cl-letf (((symbol-function 'dorg-import--ensure-repository)
               (lambda (&rest _) (setq store-touched t))))
      (should-error
       (dorg-import-document
        "/tmp/source-does-not-need-to-exist.org"
        :dolt-dir "/tmp/dorg-import-invalid-request"
        :target-file "/tmp/dorg-import-target.org"
        :mode 'bootstrap
        :base nil)
       :type 'dorg-import-request-error)
      (should-not store-touched))))

(ert-deftest dorg-test-import-bootstrap-refuses-marked-empty-document ()
  "A prior empty import is not mistaken for a legacy synthetic binding."
  (should-error
   (dorg-import--preflight-target
    'bootstrap nil "head"
    '(:root-id 10 :ids (10) :imported t :empty-synthetic nil))
   :type 'dorg-import-target-error))

(ert-deftest dorg-test-import-rejects-ambiguous-case-alias-target ()
  "Explicit ingest fails closed when one target path resolves to two roots."
  (cl-letf (((symbol-function 'dorg-repo-root-id)
             (lambda (&rest _)
               (signal 'dorg-binding-error
                       '("ambiguous" (:reason ambiguous-locator
                                              :root-ids (10 20))))))
            ((symbol-function 'dorg-sql)
             (lambda (&rest _) (ert-fail "ambiguous target must stop querying"))))
    (should-error
     (dorg-import--target-state "/tmp/store" "/tmp/target.org")
     :type 'dorg-binding-error)))

(ert-deftest dorg-test-import-rejects-live-canonical-projection-source ()
  "A materialized output cannot silently become a replace input."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_HASH: abc123\n* One\n")
    (set-visited-file-name "/tmp/dorg-import-projection.org" t)
    (should-error
     (dorg-import-document
      (current-buffer)
      :dolt-dir "/tmp/dorg-import-store"
      :target-file "/tmp/dorg-import-target.org"
      :mode 'replace
      :base "abc123")
     :type 'dorg-import-source-error)))

(ert-deftest dorg-test-import-finds-projection-metadata-after-a-heading ()
  "Projection markers are rejected anywhere, not only in the preamble."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n#+DORG_WINDOW: (:focus (10))\n")
    (should (dorg-import--projection-metadata-p (current-buffer)))))

(ert-deftest dorg-test-import-does-not-reject-projection-syntax-in-example ()
  "Documentation text is not mistaken for actual projection metadata."
  (with-temp-buffer
    (org-mode)
    (insert "* Notes\n#+begin_example\n#+DORG_HASH: sample\n#+end_example\n")
    (should-not (dorg-import--projection-metadata-p (current-buffer)))))

(ert-deftest dorg-test-import-detects-participant-source-snapshot-mutation ()
  "The read-only source snapshot is checked again before commit."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n")
    (let ((tick (buffer-chars-modified-tick)))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "Changed\n"))
      (should-error
       (dorg-import--assert-snapshot-unchanged
        (current-buffer) tick)
       :type 'dorg-import-source-error))))

(ert-deftest dorg-test-import-root-replace-plan-preserves-root-identity ()
  "Replace deletes descendants and reconciles, rather than reinserting, ROOT."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Replacement\n* New child\n")
    (let* ((parsed (dorg-parse-buffer nil 10))
           (next 20))
      (dorg-parse-assign-ids parsed (lambda () (prog1 next (cl-incf next))))
      (let ((sql (dorg-import--core-sql
                  "/tmp/store" parsed "/tmp/target.org" 10 'replace
                  (lambda () (prog1 next (cl-incf next))))))
        (should (string-match-p
                 "DELETE FROM headings WHERE parent_id = 10"
                 (car sql)))
        (should (cl-some
                 (lambda (stmt)
                   (string-match-p "UPDATE headings SET .* WHERE id = 10" stmt))
                 sql))
        (should-not
         (cl-some
          (lambda (stmt)
            (string-match-p "INSERT INTO headings .*VALUES (10," stmt))
          sql))))))

(ert-deftest dorg-test-import-plan-carries-durable-import-marker ()
  "Every successful document plan marks the root as previously imported."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n")
    (let ((parsed (dorg-parse-buffer nil 10))
          (next 20))
      (dorg-parse-assign-ids parsed (lambda () (prog1 next (cl-incf next))))
      (let ((sql (dorg-import--core-sql
                  "/tmp/store" parsed "/tmp/target.org" 10 'bootstrap
                  (lambda () (prog1 next (cl-incf next))))))
        (should
         (cl-some
          (lambda (stmt)
            (string-match-p
             "document-import:10"
             stmt))
          sql))))))

(ert-deftest dorg-test-import-extension-inputs-key-is-retired ()
  "Administrative import accepts no caller-authored extension payload channel."
  (should-error
   (dorg-import--options
    '(:dolt-dir "/tmp/dorg-import-store"
                :target-file "/tmp/dorg-import-target.org"
                :mode bootstrap
                :extension-inputs nil))
   :type 'dorg-import-request-error))

(ert-deftest dorg-test-import-registration-rejects-import-participant ()
  "Extension registration accepts no separate administrative import projector."
  (let ((dorg--extensions nil))
    (should-error
     (dorg-register-extension
      'legacy-import
      :schema "CREATE TABLE legacy_ix (heading_id BIGINT PRIMARY KEY)"
      :version 1
      :import (lambda (_ctx) nil))
     :type 'error)))

(ert-deftest dorg-test-import-bootstrap-derives-index-from-canonical-candidate ()
  "Import runs the canonical index projector over parsed candidate state.

This is the RED for the current dual-write door: the old import path can only
write extension rows through a separate `:import' SQL participant, so a normal
`:project' index projector sees no imported candidate and no canonical derived
row reaches the transaction."
  (with-temp-buffer
    (org-mode)
    (insert "* Canonical\n")
    (setq buffer-file-name "/tmp/dorg-import-source.org")
    (let ((dorg--extensions nil)
          (next 100)
          captured)
      (dorg-register-extension
       'canonical-import
       :schema "CREATE TABLE demo_ix (heading_id BIGINT PRIMARY KEY, title TEXT)"
       :version 1
       :read-tables '((:name "demo_ix" :key-column "heading_id"))
       :project
       (lambda (ctx)
         (let* ((entry
                 (cl-find-if
                  (lambda (candidate)
                    (let ((after (dorg-ext-index-entry-after candidate)))
                      (equal "Canonical" (and after (dorg-heading-title after)))))
                  (dorg-ext-index-context-created ctx)))
                (after (and entry (dorg-ext-index-entry-after entry))))
           (should entry)
           (list (list :table "demo_ix"
                       :insert (list (cons 'heading_id
                                           (dorg-ext-index-entry-id entry))
                                     (cons 'title
                                           (dorg-heading-title after))))))))
      (cl-letf (((symbol-function 'dorg-import--ensure-repository)
                 (lambda (&rest _) "/tmp/dorg-import-store/"))
                ((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) "base"))
                ((symbol-function 'dorg-import--target-state)
                 (lambda (&rest _)
                   '(:root-id nil :ids nil :imported nil
                              :empty-synthetic nil)))
                ((symbol-function 'dorg-import--assert-no-foreign-ids)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-id-generate)
                 (lambda () (prog1 next (cl-incf next))))
                ((symbol-function 'dorg-transaction-commit)
                 (lambda (_dir _base statements _message &optional _validate)
                   (setq captured statements)
                   "committed"))
                ((symbol-function 'dorg-import--validate-committed)
                 (lambda (&rest _) nil)))
        (let ((receipt
               (dorg-import-document
                (current-buffer)
                :dolt-dir "/tmp/dorg-import-store"
                :target-file "/tmp/dorg-import-target.org"
                :mode 'bootstrap)))
          (should (equal '((canonical-import :statements 1))
                         (plist-get receipt :extensions)))
          (should
           (cl-some
            (lambda (stmt)
              (and (string-match-p "INSERT INTO demo_ix" stmt)
                   (string-match-p "'Canonical'" stmt)))
            captured)))))))

(ert-deftest dorg-test-import-hostile-index-projector-fails-before-transaction ()
  "Import confines canonical projector effects before any transaction opens."
  (let ((dorg--extensions nil)
        (next 100)
        transaction-opened)
    (dorg-register-extension
     'hostile-import
     :schema "CREATE TABLE demo_ix (heading_id BIGINT PRIMARY KEY, title TEXT)"
     :version 1
     :read-tables '((:name "demo_ix" :key-column "heading_id"))
     :project
     (lambda (_ctx)
       (list (list :table "headings"
                   :insert '((id . 1) (root_id . 1) (title . "bad"))))))
    (with-temp-buffer
      (org-mode)
      (insert "* One\n")
      (setq buffer-file-name "/tmp/dorg-import-source.org")
      (cl-letf (((symbol-function 'dorg-import--ensure-repository)
                 (lambda (&rest _) "/tmp/dorg-import-store/"))
                ((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) "base"))
                ((symbol-function 'dorg-import--target-state)
                 (lambda (&rest _)
                   '(:root-id nil :ids nil :imported nil
                              :empty-synthetic nil)))
                ((symbol-function 'dorg-import--assert-no-foreign-ids)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-id-generate)
                 (lambda () (prog1 next (cl-incf next))))
                ((symbol-function 'dorg-transaction-commit)
                 (lambda (&rest _)
                   (setq transaction-opened t)
                   "committed")))
        (should-error
         (dorg-import-document
          (current-buffer)
          :dolt-dir "/tmp/dorg-import-store"
          :target-file "/tmp/dorg-import-target.org"
          :mode 'bootstrap)
         :type 'dorg-ext-index-error)
        (should-not transaction-opened)))))

(defun dorg-test-import--title-projector (ctx)
  "Project heading titles for import replace/rebuild tests."
  (let (effects)
    (dolist (entry (append (dorg-ext-index-context-created ctx)
                           (dorg-ext-index-context-changed ctx)
                           (dorg-ext-index-context-moved ctx)
                           (dorg-ext-index-context-deleted ctx)))
      (when (dorg-ext-index-entry-before entry)
        (setq effects
              (nconc effects
                     (list (list :table "import_ix"
                                 :delete
                                 (list (cons 'heading_id
                                             (dorg-ext-index-entry-id entry))))))))
      (when (dorg-ext-index-entry-after entry)
        (setq effects
              (nconc effects
                     (list (list :table "import_ix"
                                 :insert
                                 (list (cons 'heading_id
                                             (dorg-ext-index-entry-id entry))
                                       (cons 'title
                                             (dorg-heading-title
                                              (dorg-ext-index-entry-after
                                               entry))))))))))
    effects))

(defun dorg-test-import--scalar (dolt-dir sql)
  "Return the first scalar from SQL in DOLT-DIR."
  (let ((row (car (dorg-sql dolt-dir sql))))
    (and row (cdr (car row)))))

(ert-deftest dorg-test-import-materialized-revision-matches-canonical-render ()
  "Materialized import bytes match canonical render bytes at the import commit."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((source-file (expand-file-name "source.org" temp-base))
             (target-file (expand-file-name "target.org" temp-base))
             (dorg--extensions nil)
             (dorg--registry nil)
             receipt)
        (with-temp-file target-file)
        (with-temp-buffer
          (org-mode)
          (insert "#+TITLE: Import Parity\n"
                  "* Alpha\nBody\n"
                  "** Beta\n")
          (set-visited-file-name source-file t)
          (setq receipt
                (dorg-import-document
                 (current-buffer)
                 :dolt-dir dolt-dir
                 :target-file target-file
                 :mode 'bootstrap)))
        (let* ((commit (plist-get receipt :commit))
               (root-id (plist-get receipt :root-id))
               (reg
                (make-dorg-registration
                 :backend (dorg-backend-dolt dolt-dir root-id)
                 :file target-file))
               canonical materialized)
          (dorg-sql dolt-dir
                    "UPDATE headings SET title = 'HEAD drift' WHERE title = 'Alpha'")
          (dorg-commit dolt-dir "advance head after import")
          (should-not (equal commit (dorg-head-commit dolt-dir)))
          (setq canonical
                (dorg-render--tree-to-text
                 (dorg-model-load-tree dolt-dir root-id commit)
                 root-id commit dolt-dir))
          (setq dorg--registry
                (list (cons (file-truename target-file) reg)))
          (with-temp-buffer
            (org-mode)
            (set-visited-file-name target-file t)
            (let ((result (dorg-materialize (current-buffer) :revision commit)))
              (should (eq (plist-get result :scope) 'whole))
              (should (equal (plist-get result :revision) commit))
              (should (= (plist-get result :root-id) root-id)))
            (setq materialized (buffer-string)))
          (should (equal materialized canonical))
          (should (string-match-p "^\\* Alpha" materialized))
          (should-not (string-match-p "HEAD drift" materialized)))))))

(ert-deftest dorg-test-import-replace-rebuilds-derived-index-from-committed-core ()
  "Replace reprojects create/change/delete rows from exact committed core."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
                                     (let* ((target-file (expand-file-name "target.org" temp-base))
                                            (recovery-file (expand-file-name "recovery.org" temp-base))
                                            (dorg--extensions nil)
                                            (dorg--registry nil)
                                            owner-keys)
                                       (dorg-register-extension
                                        'import-rebuild
                                        :schema "CREATE TABLE import_ix (heading_id BIGINT PRIMARY KEY, title TEXT)"
                                        :version 1
                                        :read-tables '((:name "import_ix" :key-column "heading_id"))
                                        :project
                                        (lambda (ctx)
                                          (setq owner-keys
                                                (append owner-keys
                                                        (mapcar #'dorg-ext-owner-partition-owner-key
                                                                (dorg-ext-index-context-owner-partitions ctx))))
                                          (dorg-test-import--title-projector ctx)))
                                       (dorg-repo-ensure-extensions dolt-dir)
                                       (dorg-commit dolt-dir "install import rebuild projector")
                                       (setq dorg--registry
                                             (list
                                              (cons target-file
                                                    (make-dorg-registration
                                                     :backend (dorg-backend-dolt dolt-dir nil)
                                                     :file target-file
                                                     :identity '(:property "BEAD_ID")))))
                                       (with-temp-buffer
                                         (org-mode)
                                         (insert "* Keep\n:PROPERTIES:\n:DORG_ID: 101\n:BEAD_ID: keep\n:END:\n"
                                                 "* Change old\n:PROPERTIES:\n:DORG_ID: 102\n:BEAD_ID: change\n:END:\n"
                                                 "* Delete old\n:PROPERTIES:\n:DORG_ID: 103\n:BEAD_ID: delete\n:END:\n")
                                         (setq buffer-file-name target-file)
                                         (dorg-import-document
                                          (current-buffer)
                                          :dolt-dir dolt-dir
                                          :target-file target-file
                                          :mode 'bootstrap))
                                       (dorg-sql dolt-dir "UPDATE import_ix SET title = 'STALE keep' WHERE heading_id = 101")
                                       (dorg-sql dolt-dir "UPDATE import_ix SET title = 'STALE change' WHERE heading_id = 102")
                                       (dorg-commit dolt-dir "seed stale derived rows")
                                       (let ((base (dorg-head-commit dolt-dir))
                                             receipt)
                                         (with-temp-buffer
                                           (org-mode)
                                           (insert "* Keep\n:PROPERTIES:\n:DORG_ID: 101\n:BEAD_ID: keep\n:END:\n"
                                                   "* Change new\n:PROPERTIES:\n:DORG_ID: 102\n:BEAD_ID: change\n:END:\n"
                                                   "* Create new\n:PROPERTIES:\n:DORG_ID: 104\n:BEAD_ID: create\n:END:\n")
                                           (setq buffer-file-name recovery-file)
                                           (setq receipt
                                                 (dorg-import-document
                                                  (current-buffer)
                                                  :dolt-dir dolt-dir
                                                  :target-file target-file
                                                  :mode 'replace
                                                  :base base)))
                                         (let* ((commit (plist-get receipt :commit))
                                                (root-id (plist-get receipt :root-id))
                                                (readback
                                                 (dorg-model-load-headings dolt-dir root-id nil commit))
                                                (expected nil))
                                           (maphash
                                            (lambda (id heading)
                                              (when (and (>= id 101)
                                                         (stringp (dorg-heading-title heading)))
                                                (push (cons id (dorg-heading-title heading)) expected)))
                                            readback)
                                           (setq expected (sort expected (lambda (a b) (< (car a) (car b)))))
                                           (should (equal expected
                                                          '((101 . "Keep")
                                                            (102 . "Change new")
                                                            (104 . "Create new"))))
                                           (should (equal "Keep"
                                                          (dorg-test-import--scalar
                                                           dolt-dir
                                                           "SELECT title FROM import_ix WHERE heading_id = 101")))
                                           (should (equal "Change new"
                                                          (dorg-test-import--scalar
                                                           dolt-dir
                                                           "SELECT title FROM import_ix WHERE heading_id = 102")))
                                           (should (equal "0"
                                                          (format "%s"
                                                                  (dorg-test-import--scalar
                                                                   dolt-dir
                                                                   "SELECT COUNT(*) FROM import_ix WHERE heading_id = 103"))))
                                           (should (equal "Create new"
                                                          (dorg-test-import--scalar
                                                           dolt-dir
                                                           "SELECT title FROM import_ix WHERE heading_id = 104")))
                                           (should (member "BEAD_ID" owner-keys))))))))

(ert-deftest dorg-test-import-public-plan-is-one-core-plus-derived-transaction ()
  "Public bootstrap preserves source bytes and combines all SQL in one gate."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n")
    (setq buffer-file-name "/tmp/dorg-import-source.org")
    (let ((before (buffer-string))
          (next 100)
          captured)
      (cl-letf (((symbol-function 'dorg-import--ensure-repository)
                 (lambda (&rest _) "/tmp/dorg-import-store/"))
                ((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) "base"))
                ((symbol-function 'dorg-import--target-state)
                 (lambda (&rest _)
                   '(:root-id nil :ids nil :imported nil
                              :empty-synthetic nil)))
                ((symbol-function 'dorg-import--assert-no-foreign-ids)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-ext-collect-index-projection)
                 (lambda (&rest _)
                   '(:sql ("INSERT INTO ext_table VALUES (1)")
                          :effects nil
                          :extensions ((demo :statements 1))
                          :ext-dirty ((demo :tables ("ext_table"))))))
                ((symbol-function 'dorg-mutate--postimage-plan)
                 (lambda (&rest _)
                   (list :validate
                         (list :mode 'transaction-read-port
                               :extension-tables nil
                               :function (lambda (_read) nil)))))
                ((symbol-function 'dorg-id-generate)
                 (lambda () (prog1 next (cl-incf next))))
                ((symbol-function 'dorg-transaction-commit)
                 (lambda (_dir base statements _message &optional _validate)
                   (setq captured (list base statements))
                   "committed"))
                ((symbol-function 'dorg-import--validate-committed)
                 (lambda (&rest _) nil)))
        (let ((receipt
               (dorg-import-document
                (current-buffer)
                :dolt-dir "/tmp/dorg-import-store"
                :target-file "/tmp/dorg-import-target.org"
                :mode 'bootstrap)))
          (should (equal (buffer-string) before))
          (should (equal (car captured) "base"))
          (should (member "INSERT INTO ext_table VALUES (1)"
                          (cadr captured)))
          (should (equal (plist-get receipt :commit) "committed"))
          (should (equal (plist-get receipt :extensions)
                         '((demo :statements 1))))
          (should (equal (plist-get (plist-get receipt :invalidation)
                                    :ext-dirty)
                         '((demo :tables ("ext_table"))))))))))

(ert-deftest dorg-test-transaction-rejects-raced-head-before-sql ()
  "The shared store gate compares the exact planned base before writes."
  (let ((batch-called nil))
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease "key")))
              ((symbol-function 'dorg-sql--lease-scalar)
               (lambda (&rest _) "moved"))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (&rest _) nil))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (setq batch-called t))))
      (should-error
       (dorg-transaction-commit
        "/tmp/store" "base" '("INSERT INTO x VALUES (1)") "import")
       :type 'dorg-transaction-stale)
      (should-not batch-called))))

(ert-deftest dorg-test-transaction-refuses-uncommitted-working-set ()
  "Administrative import never sweeps unrelated working-set changes into its commit."
  (let ((batch-called nil))
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease "key")))
              ((symbol-function 'dorg-sql--lease-scalar)
               (lambda (&rest _) "base"))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (&rest _) '(((table_name . "headings")))))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) (setq batch-called t))))
      (should-error
       (dorg-transaction-commit
        "/tmp/store" "base" '("INSERT INTO x VALUES (1)") "import")
       :type 'dorg-transaction-dirty)
      (should-not batch-called))))

(ert-deftest dorg-test-transaction-returns-dolt-commit-result-not-later-head ()
  "The administrative receipt uses the hash returned by DOLT_COMMIT."
  (let ((head-reads 0))
    (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
               (lambda (_dir fn &rest _) (funcall fn 'lease "key")))
              ((symbol-function 'dorg-sql--lease-scalar)
               (lambda (&rest _)
                 (cl-incf head-reads)
                 "base"))
              ((symbol-function 'dorg-sql--lease-exec)
               (lambda (&rest _) nil))
              ((symbol-function 'dorg-sql--lease-batch)
               (lambda (&rest _) "exact-commit")))
      (should
       (equal
        (dorg-transaction-commit
         "/tmp/store" "base" '("INSERT INTO x VALUES (1)") "import")
        "exact-commit"))
      (should (= head-reads 1)))))

(ert-deftest dorg-test-mysql-batch-returns-dolt-commit-hash ()
  "The lease batch seam preserves the exact hash produced by DOLT_COMMIT."
  (cl-letf (((symbol-function 'mysql-query)
             (lambda (_conn _sql) nil))
            ((symbol-function 'dorg-sql--mysql-exec)
             (lambda (sql _conn)
               (when (string-match-p "CALL DOLT_COMMIT" sql)
                 '(((hash . "exact-commit")))))))
    (should
     (equal
      (dorg-sql--mysql-batch-on-conn
       'conn '("INSERT INTO x VALUES (1)") "import")
      "exact-commit"))))

(ert-deftest dorg-test-mysql-batch-finalization-failure-is-indeterminate ()
  "A failure after DOLT_COMMIT reports the durable commit instead of rollback."
  (let (queries)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql queries)
                 (when (equal sql "COMMIT")
                   (error "RAW-FINALIZATION failed"))))
              ((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (when (string-match-p "CALL DOLT_COMMIT" sql)
                   '(((hash . "landed-commit")))))))
      (let* ((err
              (should-error
               (dorg-sql--mysql-batch-on-conn
                'conn '("INSERT INTO x VALUES (1)") "import")
               :type 'dorg-sql-commit-indeterminate))
             (data (dorg-sql-commit-indeterminate-data err))
             (printed (format "%s %S" (error-message-string err) data)))
        (should (equal (plist-get data :commit) "landed-commit"))
        (should (eq 'in-commit (plist-get data :transaction-phase)))
        (should-not (plist-member data :underlying))
        (should (= 0 (cl-count "ROLLBACK" queries :test #'equal)))
        (should-not (string-match-p "RAW-FINALIZATION" printed))))))

(ert-deftest dorg-test-transaction-commit-retains-terminal-indeterminate-policy ()
  "Administrative import maps the commit-indeterminate child condition terminally.
This pins the condition parentage consumed by `dorg-transaction-commit': the
shared fix must not turn a lost administrative commit response into retry or a
definite rollback claim."
  (should (memq 'dorg-sql-lease-broken
                (get 'dorg-sql-commit-indeterminate 'error-conditions)))
  (cl-letf (((symbol-function 'dorg-mutate-call-with-store-gate)
             (lambda (_dir fn &rest _) (funcall fn 'lease "key")))
            ((symbol-function 'dorg-sql--lease-scalar)
             (lambda (&rest _) "base"))
            ((symbol-function 'dorg-sql--lease-exec)
             (lambda (&rest _) nil))
            ((symbol-function 'dorg-sql--lease-batch)
             (lambda (&rest _)
               (signal 'dorg-sql-commit-indeterminate
                       '("Dolt commit outcome is indeterminate"
                         :transaction-phase in-commit)))))
    (let ((err
           (should-error
            (dorg-transaction-commit
             "/tmp/store" "base" '("INSERT INTO x VALUES (1)") "import")
            :type 'dorg-transaction-indeterminate)))
      (should (equal "base" (plist-get (cddr err) :base))))))

(ert-deftest dorg-test-import-validator-proves-exact-core-state ()
  "The transaction validator checks ids, root, parents, file, and marker."
  (let ((query
         (lambda (sql &rest args)
           (let ((formatted (if args (apply #'format sql args) sql)))
             (cond
              ((string-match-p "SELECT id FROM headings WHERE root_id" formatted)
               '(((id . 10)) ((id . 11))))
              ((string-match-p "parent_id IS NULL AND level = 0" formatted)
               '(((id . 10))))
              ((string-match-p "LEFT JOIN headings p" formatted) nil)
              ((string-match-p "SELECT relative_path FROM dorg_files" formatted)
               '(((relative_path . "../target.org"))))
              ((string-match-p "document-import:10" formatted)
               '(((value . "1"))))
              (t (ert-fail (format "unexpected validation query: %s"
                                   formatted))))))))
    (should-not
     (dorg-import--validate-pending
      query "/tmp/store" 10 "/tmp/target.org" '(10 11)))))

(ert-deftest dorg-test-import-postcondition-queries-exact-committed-revision ()
  "Postconditions read every core table AS OF the returned commit."
  (let (queries)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (push sql queries)
                 (cond
                  ((string-match-p
                    "SELECT id FROM headings AS OF" sql)
                   '(((id . 10))))
                  ((string-match-p
                    "parent_id IS NULL AND level = 0" sql)
                   '(((id . 10))))
                  ((string-match-p "LEFT JOIN headings AS OF" sql) nil)
                  ((string-match-p "SELECT relative_path FROM dorg_files AS OF" sql)
                   '(((relative_path . "../target.org"))))
                  ((string-match-p "SELECT value FROM dorg_meta AS OF" sql)
                   '(((value . "1"))))
                  (t (ert-fail (format "unexpected exact query: %s" sql)))))))
      (should-not
       (dorg-import--validate-committed
        "/tmp/store" 10 "/tmp/target.org" '(10) "committed"))
      (should (= (length queries) 5))
      (should
       (cl-every (lambda (sql)
                   (string-match-p "AS OF 'committed'" sql))
                 queries)))))

(ert-deftest dorg-test-import-composite-validator-runs-both-proofs ()
  "The typed postimage proof and raw core proof run in one validate window."
  (let (queries proofs)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql queries)
                 nil))
              ((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (push sql queries)
                 (cond
                  ((string-match-p "CALL DOLT_COMMIT" sql)
                   '(((hash . "validated-commit"))))
                  (t nil)))))
      (should
       (equal
        (dorg-sql--mysql-batch-on-conn
         'conn
         '("INSERT core" "INSERT extension")
         "import"
         (list :mode 'transaction-read-port
               :extension-tables '("demo_ix")
               :function (lambda (_read) (push 'typed proofs))
               :query-function
               (lambda (query)
                 (push 'raw proofs)
                 (funcall query "SELECT 1"))))
        "validated-commit"))
      (should (equal (nreverse proofs) '(typed raw)))
      (should (member "SELECT 1" queries)))))

(ert-deftest dorg-test-import-composite-validator-rolls-back-from-either-proof ()
  "A signal from either composite validator branch rolls back all writes."
  (let (typed-events raw-events)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql typed-events)
                 nil)))
      (should-error
       (dorg-sql--mysql-batch-on-conn
        'conn
        '("INSERT core")
        "import"
        (list :mode 'transaction-read-port
              :extension-tables nil
              :function (lambda (_read)
                          (error "typed proof failed"))
              :query-function (lambda (_query) nil))))
      (should (equal (nreverse typed-events)
                     '("BEGIN" "INSERT core" "ROLLBACK"))))
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql raw-events)
                 nil)))
      (should-error
       (dorg-sql--mysql-batch-on-conn
        'conn
        '("INSERT core")
        "import"
        (list :mode 'transaction-read-port
              :extension-tables nil
              :function (lambda (_read) nil)
              :query-function
              (lambda (_query)
                (error "raw proof failed")))))
      (should (equal (nreverse raw-events)
                     '("BEGIN" "INSERT core" "ROLLBACK"))))))

(ert-deftest dorg-test-import-validation-failure-rolls-back-core-and-derived-sql ()
  "A failing precommit validator rolls back both core and derived writes."
  (let (events)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql events)
                 nil)))
      (should-error
       (dorg-sql--mysql-batch-on-conn
        'conn
        '("INSERT core" "INSERT extension")
        "import"
        (lambda (_query) (error "import postcondition failed"))))
      (should
       (equal (nreverse events)
              '("BEGIN" "INSERT core" "INSERT extension" "ROLLBACK"))))))

(ert-deftest dorg-test-import-validator-query-seam-is-select-only ()
  "A validator cannot smuggle another write through its pending-state query."
  (let (events)
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql)
                 (push sql events)
                 nil)))
      (should-error
       (dorg-sql--mysql-batch-on-conn
        'conn
        '("INSERT core")
        "import"
        (lambda (query)
          (funcall query "UPDATE headings SET title = 'bad'"))))
      (should
       (equal (nreverse events)
              '("BEGIN" "INSERT core" "ROLLBACK"))))))

(provide 'dorg-import-tests)
;;; dorg-import-tests.el ends here
