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

(ert-deftest dorg-test-import-owned-metadata-census-sees-drawer-storage ()
  "The import preflight sees canonical family keys in a drawer image."
  (let ((image (make-hash-table :test 'eql)))
    (puthash 1
             (make-dorg-heading
              :id 1 :level 1 :level-offset 1 :title "Clock"
              :items (list
                      (dorg-drawer-item
                       :name "PROPERTIES"
                       :properties '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z")
                                     ("OWNER" . "me")))))
             image)
    (should (equal '("DORG_CLOCK_")
                   (dorg-import--image-owned-metadata-families image)))))

(ert-deftest dorg-test-import-owned-metadata-uses-prior-image-for-removal ()
  "Replacing away the last family key cannot evade unavailable-owner refusal."
  (let ((before (make-hash-table :test 'eql))
        (after (make-hash-table :test 'eql)))
    (puthash 1
             (make-dorg-heading
              :id 1 :level 1 :level-offset 1 :title "Clock"
              :items (list (dorg-drawer-item
                            :name "PROPERTIES"
                            :properties '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z")))))
             before)
    (puthash 1 (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "Clock") after)
    (let ((context
           (dorg-ext-index-context-create
            :source-kind 'dolt :root-id 1 :dolt-dir "/tmp/dorg-import-test"
            :base-image before :candidate-postimage after :owner-partitions nil)))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner) nil))
        (condition-case err
            (progn
              (dorg-import--validate-owned-metadata context)
              (ert-fail "expected unavailable owner refusal"))
          (dorg-import-target-error
           (should (eq 'owner-unavailable (plist-get (cddr err) :detail)))
           (should (equal '("DORG_CLOCK_") (plist-get (cddr err) :families)))))))))

(ert-deftest dorg-test-import-owned-metadata-refuses-noncanonical-family-key ()
  "Import rejects lower-case and KEY+ family storage before lazy-owner lookup."
  (dolist (key '("dorg_clock_start" "DORG_CLOCK_START+"))
    (let ((candidate (make-hash-table :test 'eql)))
      (puthash 1
               (make-dorg-heading
                :id 1 :items
                (list (dorg-drawer-item :name "PROPERTIES"
                                        :properties (list (cons key "x")))))
               candidate)
      (let ((context (dorg-ext-index-context-create
                      :source-kind 'dolt :root-id 1 :dolt-dir "/tmp/dorg-import-test"
                      :base-image (make-hash-table :test 'eql)
                      :candidate-postimage candidate)))
        (let ((err (should-error (dorg-import--validate-owned-metadata context)
                                 :type 'dorg-import-target-error)))
          (should (eq 'owned-metadata-noncanonical
                      (plist-get (cddr err) :detail)))
          (should (equal key (plist-get (cddr err) :key))))))))

(ert-deftest dorg-test-import-owned-metadata-refuses-case-alias-duplicate ()
  "A drawer cannot carry two case aliases for the same owned family key."
  (let ((candidate (make-hash-table :test 'eql)))
    (puthash 1
             (make-dorg-heading
              :id 1 :items
              (list (dorg-drawer-item
                     :name "PROPERTIES"
                     :properties '(("DORG_CLOCK_START" . "old")
                                   ("dorg_clock_start" . "new")))))
             candidate)
    (let* ((context (dorg-ext-index-context-create
                     :source-kind 'dolt :root-id 1 :dolt-dir "/tmp/dorg-import-test"
                     :base-image (make-hash-table :test 'eql)
                     :candidate-postimage candidate))
           (err (should-error (dorg-import--validate-owned-metadata context)
                              :type 'dorg-import-target-error)))
      (should (eq 'owned-metadata-noncanonical
                  (plist-get (cddr err) :detail)))
      (should (equal "dorg_clock_start" (plist-get (cddr err) :key))))))

(ert-deftest dorg-test-import-owned-metadata-delegates-complete-context ()
  "An available import owner receives the complete immutable context once."
  (let ((before (make-hash-table :test 'eql))
        (after (make-hash-table :test 'eql))
        seen)
    (puthash 1
             (make-dorg-heading
              :id 1 :items
              (list (dorg-drawer-item :name "PROPERTIES"
                                      :properties
                                      '(("DORG_CLOCK_START" . "old")))))
             before)
    (puthash 1
             (make-dorg-heading
              :id 1 :items
              (list (dorg-drawer-item :name "PROPERTIES"
                                      :properties
                                      '(("DORG_CLOCK_START" . "new")))))
             after)
    (let ((context (dorg-ext-index-context-create
                    :source-kind 'dolt :root-id 1 :dolt-dir "/tmp/dorg-import-test"
                    :base-image before :candidate-postimage after)))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda () (list :import (lambda (arg) (setq seen arg))))))
        (dorg-import--validate-owned-metadata context))
      (should (eq context seen)))))

(ert-deftest dorg-test-import-owned-metadata-normalizes-owner-refusal ()
  "A rejecting import owner aborts before any projection with its family named."
  (let ((image (make-hash-table :test 'eql))
        (base (make-hash-table :test 'eql)))
    (puthash 1
             (make-dorg-heading
              :id 1 :items
              (list (dorg-drawer-item :name "PROPERTIES"
                                      :properties
                                      '(("DORG_CLOCK_START" . "new")))))
             image)
    (let ((context (dorg-ext-index-context-create
                    :source-kind 'dolt :root-id 1 :dolt-dir "/tmp/dorg-import-test"
                    :base-image base :candidate-postimage image)))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda () (list :import (lambda (_context)
                                             (error "owner rejects import"))))))
        (condition-case err
            (progn
              (dorg-import--validate-owned-metadata context)
              (ert-fail "expected owner refusal"))
          (dorg-import-target-error
           (should (eq 'owner-refused (plist-get (cddr err) :detail)))
           (should (equal "DORG_CLOCK_" (plist-get (cddr err) :family)))
           (should (equal "owner rejects import"
                          (plist-get (cddr err) :owner-message)))))))))

(ert-deftest dorg-test-import-replace-owned-metadata-refusal-is-atomic ()
  "A rejecting owner aborts a real replace before it can advance Dolt HEAD.

The initial bootstrap uses the test-only lazy-owner seam.  The replacement
changes the literal clock value, then the same owner rejects it; HEAD and the
recovery source bytes must both remain unchanged.  Removing the live
`dorg-import--validate-owned-metadata' call makes this test turn RED by
allowing a replacement commit."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((target-file (expand-file-name "target.org" temp-base))
             (recovery-file (expand-file-name "recovery.org" temp-base))
             (dorg--registry nil)
             head)
        (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                   (lambda () (list :import (lambda (_context) nil)))))
          (with-temp-buffer
            (org-mode)
            (insert "* Clock\n:PROPERTIES:\n:DORG_ID: 101\n:OWNER: me\n:DORG_CLOCK_START: 2026-10-14T08:00:00Z\n:END:\n")
            (set-visited-file-name target-file t)
            (dorg-import-document (current-buffer)
                                  :dolt-dir dolt-dir :target-file target-file
                                  :mode 'bootstrap)))
        (setq head (dorg-head-commit dolt-dir))
        (with-temp-buffer
          (org-mode)
          (insert "* Clock\n:PROPERTIES:\n:DORG_ID: 101\n:OWNER: me\n:DORG_CLOCK_START: 2026-10-14T09:00:00Z\n:END:\n")
          (set-visited-file-name recovery-file t)
          (let ((before (buffer-string)))
            (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                       (lambda ()
                         (list :import (lambda (_context)
                                         (error "owner rejects replacement"))))))
              (let ((err (should-error
                          (dorg-import-document
                           (current-buffer)
                           :dolt-dir dolt-dir :target-file target-file
                           :mode 'replace :base head)
                          :type 'dorg-import-target-error)))
                (should (eq 'owner-refused (plist-get (cddr err) :detail)))))
            (should (equal head (dorg-head-commit dolt-dir)))
            (should (equal before (buffer-string)))))))))

(ert-deftest dorg-test-import-export-preserves-mixed-owned-drawer-bytes ()
  "Accepted import/export retains the literal mixed-drawer property ordering."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((source-file (expand-file-name "source.org" temp-base))
             (target-file (expand-file-name "target.org" temp-base))
             (text (concat "* Clock\n:PROPERTIES:\n:DORG_ID: 101\n:OWNER: me\n"
                           ":DORG_CLOCK_START: 2026-10-14T08:00:00Z\n:END:\n"))
             (dorg--registry nil)
             receipt)
        (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                   (lambda () (list :import (lambda (_context) nil)))))
          (with-temp-buffer
            (org-mode)
            (insert text)
            (set-visited-file-name source-file t)
            (setq receipt
                  (dorg-import-document
                   (current-buffer) :dolt-dir dolt-dir :target-file target-file
                   :mode 'bootstrap))))
        (let (headings)
          (maphash (lambda (_id heading) (push heading headings))
                   (dorg-model-load-tree dolt-dir (plist-get receipt :root-id)))
          (should (equal text (dorg-model-render-tree headings))))))))

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

(ert-deftest dorg-test-import-refuses-owned-metadata-keyword-storage ()
  "A registered family may not enter import through a db-local keyword item."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n#+dorg_clock_start: 2026-10-14T08:00:00Z\n")
    (set-visited-file-name "/tmp/dorg-import-owned-keyword.org" t)
    (should (dorg-import--owned-metadata-keyword-p (current-buffer)))
    (should-error (dorg-import--assert-source-buffer (current-buffer))
                  :type 'dorg-import-source-error)))

(ert-deftest dorg-test-import-allows-owned-metadata-keyword-example-text ()
  "Example text is not a keyword storage form and stays an importable source."
  (with-temp-buffer
    (org-mode)
    (insert "* Notes\n#+begin_example\n#+DORG_CLOCK_START: sample\n#+end_example\n")
    (should-not (dorg-import--owned-metadata-keyword-p (current-buffer)))))

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
                                       (dorg-migrate-schema dolt-dir)
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
              ((string-match-p "parent_id IS NULL AND level_offset = 0" formatted)
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
                    "parent_id IS NULL AND level_offset = 0" sql)
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

;;; ─── Multi-row insert lowering against the frozen base oracle ────────────
;;
;; Import lowers its core rows (grouped by the FK-safe core table order) and
;; its extension effects (adjacent only) into multi-row INSERTs.  These specs
;; compare the candidate with the frozen base capture and with an independent
;; string-level reference model; see tests/fixtures/insert-batching/.

(defconst dorg-test-import--ib-dir
  (expand-file-name "fixtures/insert-batching/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The insert-batching base oracle, reference model and test extension.")

(require 'dorg-insert-batching-capture
         (expand-file-name "capture" dorg-test-import--ib-dir))
(require 'dorg-insert-batching-reference
         (expand-file-name "reference" dorg-test-import--ib-dir))
(require 'dorg-insert-batching-extension
         (expand-file-name "extension" dorg-test-import--ib-dir))

(defvar dorg-test--server-port)

(defmacro dorg-test-import--with-ib-store (&rest body)
  "Run BODY with `dolt-dir' a fresh store and `dir' a fresh source directory."
  (declare (indent 0))
  `(progn
     (unless (and dorg-test--dolt-available dorg-test--server-port)
       (ert-skip "dolt sql-server unavailable"))
     (dorg-test--with-temp-db dolt-dir
       (let ((dir (make-temp-file "dorg-ib-" t)))
         (unwind-protect (progn ,@body)
           (delete-directory dir t))))))

(defun dorg-test-import--ib-candidate ()
  "Return the candidate's normalized observation of every oracle scenario."
  (dorg-test-import--with-ib-store
    (dorg-ib-scenarios dolt-dir dir)))

(defun dorg-test-import--ib-golden (key)
  "Return the frozen base observation of scenario KEY."
  (cdr (assq key (plist-get (dorg-ib-read-golden) :scenarios))))

(defconst dorg-test-import--ib-import-keys
  '(:bootstrap :replace :second-bootstrap)
  "The oracle scenarios that are document imports.")

(ert-deftest dorg-test-import-lowered-transaction-equals-reference-model ()
  "RED mechanism control: an import sends the lowered transaction, exactly.
For bootstrap, replace and a second root's bootstrap, the one transaction's
statements equal the independent reference model built from the frozen base
statements.  That model reorders core rows by table and partitions the BM25
insert block, merges adjacent same-shape rows under the 500-row and wire-byte
caps, and keeps every barrier.  The base sends one statement per row and
fails."
  (let ((candidate (dorg-test-import--ib-candidate)))
    (dolist (key dorg-test-import--ib-import-keys)
      (let* ((golden (dorg-test-import--ib-golden key))
             (base (car (plist-get golden :commits)))
             (actual (plist-get (cdr (assq key candidate)) :commits)))
        (should (= 1 (length actual)))
        (should (equal (dorg-ibr-expected-import base (plist-get golden :core-sql))
                       (car actual)))))
    (let ((base (car (plist-get (dorg-test-import--ib-golden :bootstrap) :commits)))
          (actual (car (plist-get (cdr (assq :bootstrap candidate)) :commits))))
      (should (<= (* 10 (length actual)) (length base))))))

(defun dorg-test-import--ib-row-p (statement)
  "Non-nil when STATEMENT is a core row INSERT."
  (and (dorg-ibr-split statement dorg-ibr-core-order) t))

(defun dorg-test-import--ib-heading-refs (statements)
  "Return (ID ROOT-ID PARENT-ID) of every headings row in STATEMENTS, in order."
  (let (refs)
    (dolist (sql statements)
      (when (equal "headings" (and (dorg-test-import--ib-row-p sql)
                                   (dorg-ibr-table sql)))
        (let ((start 0))
          (while (string-match
                  "(\\(-?[0-9]+\\), \\(-?[0-9]+\\), \\(NULL\\|-?[0-9]+\\), X'"
                  sql start)
            (push (list (string-to-number (match-string 1 sql))
                        (string-to-number (match-string 2 sql))
                        (let ((p (match-string 3 sql)))
                          (and (not (equal p "NULL")) (string-to-number p))))
                  refs)
            (setq start (match-end 0))))))
    (nreverse refs)))

(ert-deftest dorg-test-import-core-sql-keeps-barriers-first-and-groups-tables ()
  "Core SQL: barriers first, one run per table in FK order, binding last.
On bootstrap and on replace with an existing root (root tags and items,
reparented 10-deep chain), every barrier precedes every row in the base
relative order; row tables appear once each in the core order; the file
binding and the import marker stay last.  Every inserted heading's parent
and root were inserted earlier or pre-exist.  The one self-reference allowed
is the bootstrap root naming itself as root with a NULL parent."
  (let ((candidate (dorg-test-import--ib-candidate)))
    (dolist (key '(:bootstrap :replace))
      (let* ((golden (plist-get (dorg-test-import--ib-golden key) :core-sql))
             (actual (plist-get (cdr (assq key candidate)) :core-sql))
             (barriers (cl-remove-if #'dorg-test-import--ib-row-p
                                     (butlast actual 2)))
             (first-row (cl-position-if #'dorg-test-import--ib-row-p actual))
             (tables (delq nil (mapcar (lambda (s)
                                         (and (dorg-test-import--ib-row-p s)
                                              (dorg-ibr-table s)))
                                       actual))))
        (should (equal (last golden 2) (last actual 2)))
        (should (equal (cl-remove-if #'dorg-test-import--ib-row-p (butlast golden 2))
                       barriers))
        (should (= (length barriers) first-row))
        (should (equal tables (delete-dups (copy-sequence tables))))
        (should (equal tables (cl-remove-if-not (lambda (tb) (member tb tables))
                                                dorg-ibr-core-order)))
        (let* ((root (plist-get (plist-get (cdr (assq key candidate)) :value)
                                :root-id))
               (seen (if (eq key :replace) (list root) nil)))
          (dolist (ref (dorg-test-import--ib-heading-refs actual))
            (pcase-let ((`(,id ,root-id ,parent) ref))
              (if (and (eq key :bootstrap) (eql id root))
                  (progn (should (eql root-id id)) (should (null parent)))
                (should (memql root-id seen))
                (should (memql parent seen))))
            (push (car ref) seen)))))))

(ert-deftest dorg-test-import-identity-bindings-match-base-oracle ()
  "Control: id-generator calls and item id/sort-key bindings are unchanged.
For every import scenario the sequence of generated ids and the binding of
each heading's and item's id and sort key equal the frozen base."
  (let ((candidate (dorg-test-import--ib-candidate)))
    (dolist (key dorg-test-import--ib-import-keys)
      (let ((golden (dorg-test-import--ib-golden key))
            (actual (cdr (assq key candidate))))
        (should (equal (plist-get golden :id-calls) (plist-get actual :id-calls)))
        (should (equal (plist-get golden :bindings) (plist-get actual :bindings)))))))

(defun dorg-test-import--ib-by-table (effects)
  "Return EFFECTS grouped as an alist of (TABLE . EFFECTS-IN-ORDER)."
  (let (groups)
    (dolist (effect effects)
      (let ((cell (assoc (plist-get effect :table) groups)))
        (if cell (setcdr cell (append (cdr cell) (list effect)))
          (setq groups (append groups (list (list (plist-get effect :table) effect)))))))
    groups))

(ert-deftest dorg-test-import-final-state-and-receipts-match-base-oracle ()
  "Control: every scenario ends in the frozen base's exact stored state.
Every core and BM25 table, the receipts (normalized commit and path facts
only) and the extension summaries equal the base for:
- bootstrap and replace;
- an ordinary edit and an in-root move;
- a second root and a cross-root move;
- a rebuild repair.
Projection effects equal the base table by table, in order.  Dry-run plans
equal the base apart from their :db-ops."
  (let ((candidate (dorg-test-import--ib-candidate)))
    (dolist (entry (plist-get (dorg-ib-read-golden) :scenarios))
      (let ((golden (cdr entry))
            (actual (cdr (assq (car entry) candidate))))
        (should (equal (plist-get golden :dump) (plist-get actual :dump)))
        (should (equal (plist-get golden :value) (plist-get actual :value)))
        (dolist (side (list nil :plan :apply))
          (let ((g (if side (plist-get golden side) golden))
                (a (if side (plist-get actual side) actual)))
            ;; A dry-run plan's :db-ops carry the lowered extension SQL; the
            ;; BM25 mutation spec pins them against the reference model.
            (should (equal (plist-put (copy-sequence (plist-get g :value)) :db-ops nil)
                           (plist-put (copy-sequence (plist-get a :value)) :db-ops nil)))
            (should (= (length (plist-get g :projections))
                       (length (plist-get a :projections))))
            (cl-loop for gp in (plist-get g :projections)
                     for ap in (plist-get a :projections)
                     do (should (equal (plist-get gp :extensions)
                                       (plist-get ap :extensions)))
                        (should (equal (dorg-test-import--ib-by-table
                                        (plist-get gp :effects))
                                       (dorg-test-import--ib-by-table
                                        (plist-get ap :effects)))))))))))

(ert-deftest dorg-test-import-base-scalar-statements-replay-to-base-state ()
  "Control: the frozen scalar statements still commit the frozen state.
Bootstrap and replace run through the real import entry with the
transaction's statements replaced, in this test only, by the frozen base
list.  The one store-local statement, the file binding, is regenerated for
this store.  Every in-transaction validator accepts the base statements, and
the store equals the frozen dumps, which the candidate must also reach."
  (dorg-test-import--with-ib-store
    (let ((dorg--extensions nil)
          (ids (dorg-ib-id-generator))
          (orig (symbol-function 'dorg-transaction-commit)))
      (dorg-ib--install-bm25 dolt-dir)
      (dolist (step (list (list :bootstrap dorg-ib-corpus-bootstrap 'bootstrap)
                          (list :replace dorg-ib-corpus-replace 'replace)))
        (let ((golden (dorg-test-import--ib-golden (car step)))
              (base (and (eq (nth 2 step) 'replace) (dorg-head-commit dolt-dir))))
          (cl-letf (((symbol-function 'dorg-transaction-commit)
                     (lambda (d b statements &rest rest)
                       (let ((file-row (cl-find-if
                                        (lambda (s) (string-prefix-p
                                                     "INSERT INTO dorg_files " s))
                                        statements)))
                         (should file-row)
                         (apply orig d b
                                (mapcar (lambda (s)
                                          (if (eq s :dorg-files-row) file-row s))
                                        (car (plist-get golden :commits)))
                                rest)))))
            (dorg-ib-import ids dolt-dir dir "corpus" (nth 1 step) (nth 2 step) base))
          (should (equal (plist-get golden :dump)
                         (dorg-ib-normalize (dorg-ib-dump dolt-dir) nil))))))))

(defun dorg-test-import--fk-outcome (dolt-dir statements)
  "Commit STATEMENTS on DOLT-DIR and return the outcome, then restore HEAD.
The outcome is `ok' or (CONDITION DB-CODE CLASS) of the propagated error."
  (let ((base (dorg-head-commit dolt-dir)))
    (condition-case err
        (progn
          (dorg-transaction-commit dolt-dir base statements "fk control")
          (dorg-sql dolt-dir (format "CALL DOLT_RESET('--hard', '%s')" base))
          'ok)
      (error
       (let ((facts (dorg-dolt--classify-db-error err)))
         (should (equal base (dorg-head-commit dolt-dir)))
         (list (car err) (plist-get facts :db-code) (plist-get facts :class)))))))

(ert-deftest dorg-test-import-multi-row-fk-outcomes-match-scalar-statements ()
  "Pinned Dolt: a multi-row INSERT accepts and refuses exactly as its rows.
Inside a DOrg transaction on the server path, each case runs once as one
multi-row statement and once as the scalar sequence:
- (i) a self-FK parent before its child: both commit;
- (ii) a forward reference: both refused, same condition and code;
- (iii) a dangling parent: both refused;
- (iv) a cross-table child of a dangling parent: both refused.
Outcomes are logged with the prefix IB-FK-CONTROL for the evidence archive."
  (dorg-test-import--with-ib-store
    (dorg-sql-batch
     dolt-dir
     (list "CREATE TABLE ibfk_h (id BIGINT PRIMARY KEY, parent_id BIGINT NULL, FOREIGN KEY (parent_id) REFERENCES ibfk_h(id) ON DELETE CASCADE)"
           "CREATE TABLE ibfk_c (id BIGINT PRIMARY KEY, h_id BIGINT NOT NULL, FOREIGN KEY (h_id) REFERENCES ibfk_h(id) ON DELETE CASCADE)"
           "INSERT INTO ibfk_h (id, parent_id) VALUES (1, NULL)"))
    (dorg-commit dolt-dir "fk control tables")
    (dolist (case '((parent-before-child ok "ibfk_h (id, parent_id)" "(2, 1)" "(3, 2)")
                    (forward-reference error "ibfk_h (id, parent_id)" "(5, 4)" "(4, 1)")
                    (dangling-parent error "ibfk_h (id, parent_id)" "(6, 1)" "(7, 99)")
                    (cross-table-dangling error "ibfk_c (id, h_id)" "(10, 1)" "(11, 99)")))
      (pcase-let* ((`(,name ,expect ,shape . ,tuples) case)
                   (prefix (format "INSERT INTO %s VALUES " shape))
                   (scalar (dorg-test-import--fk-outcome
                            dolt-dir (mapcar (lambda (tu) (concat prefix tu)) tuples)))
                   (multi (dorg-test-import--fk-outcome
                           dolt-dir (list (concat prefix (mapconcat #'identity tuples ", "))))))
        (message "IB-FK-CONTROL dolt=%s case=%s scalar=%S multi=%S"
                 (string-trim (shell-command-to-string "dolt version"))
                 name scalar multi)
        (should (equal scalar multi))
        (should (eq expect (if (eq scalar 'ok) 'ok 'error)))))))

(ert-deftest dorg-test-import-planning-inputs-are-not-mutated ()
  "Lowering mutates no planning input beyond the established item binding.
During bootstrap and replace:
- the parsed headings equal deep copies taken before core SQL generation,
  apart from the item id and sort key the generators bind;
- the projector's effect lists equal copies taken as they were returned;
- the index context's candidate image equals its copy taken before
  projection."
  (dorg-test-import--with-ib-store
    (let ((dorg--extensions nil)
          (ids (dorg-ib-id-generator))
          (core-fn (symbol-function 'dorg-import--core-sql))
          (collect-fn (symbol-function 'dorg-ext-collect-index-projection))
          (project-fn (symbol-function 'dorg-bm25-project))
          (project-log nil)
          (checks 0))
      (dorg-ib--install-bm25 dolt-dir)
      (cl-flet ((unbind (headings)
                  (mapcar (lambda (h)
                            (let ((copy (dorg-heading-deep-copy h)))
                              (dolist (item (dorg-heading-items copy))
                                (setf (dorg-item-id item) nil
                                      (dorg-item-sort-key item) nil))
                              copy))
                          headings))
                (image (context)
                  (let (rows)
                    (maphash (lambda (id h) (push (cons id (dorg-heading-deep-copy h)) rows))
                             (dorg-ext-index-context-candidate-postimage context))
                    (sort rows (lambda (a b) (< (car a) (car b)))))))
        (cl-letf (((symbol-function 'dorg-import--core-sql)
                   (lambda (d parsed &rest rest)
                     (let* ((before (unbind (dorg-parse-result-headings parsed)))
                            (sql (apply core-fn d parsed rest)))
                       (should (equal before (unbind (dorg-parse-result-headings parsed))))
                       (cl-incf checks)
                       sql)))
                  ((symbol-function 'dorg-bm25-project)
                   (lambda (context)
                     (let ((effects (funcall project-fn context)))
                       (push (cons effects (copy-tree effects)) project-log)
                       effects)))
                  ((symbol-function 'dorg-ext-collect-index-projection)
                   (lambda (context &rest rest)
                     (let* ((before (image context))
                            (result (apply collect-fn context rest)))
                       (should (equal before (image context)))
                       (dolist (pair project-log)
                         (should (equal (cdr pair) (car pair))))
                       (cl-incf checks)
                       result))))
          (dorg-ib-import ids dolt-dir dir "corpus" dorg-ib-corpus-bootstrap 'bootstrap)
          (dorg-ib-import ids dolt-dir dir "corpus" dorg-ib-corpus-replace 'replace
                          (dorg-head-commit dolt-dir))))
      (should (= 4 checks)))))

;;; Failure atomicity on a fault inside a merged statement.

(defun dorg-test-import--ib-state (dolt-dir)
  "Return HEAD, the working-set status and every oracle and test table."
  (list (dorg-head-commit dolt-dir)
        (dorg-sql dolt-dir "SELECT * FROM dolt_status")
        (dorg-ib-normalize (dorg-ib-dump dolt-dir dorg-ibx-dump-tables) nil)))

(defun dorg-test-import--ib-fault-store (dolt-dir fault)
  "Install BM25 and the FK test extension in DOLT-DIR; FAULT is its cell."
  (dorg-bm25-register-extension)
  (dorg-ibx-register fault)
  (dorg-migrate-schema dolt-dir)
  (dorg-commit dolt-dir "install bm25 and the fk test extension"))

(defun dorg-test-import--ib-scalar-projection (thunk)
  "Call THUNK with every projection's SQL replaced by scalar effect SQL.
The scalar form is `dorg-ext--index-effect-sql' per effect, which the
effect control proves byte-identical to the frozen base."
  (let ((collect-fn (symbol-function 'dorg-ext-collect-index-projection)))
    (cl-letf (((symbol-function 'dorg-ext-collect-index-projection)
               (lambda (&rest args)
                 (let ((result (copy-sequence (apply collect-fn args))))
                   (plist-put result :sql
                              (mapcar #'dorg-ext--index-effect-sql
                                      (plist-get result :effects)))))))
      (funcall thunk))))

(defun dorg-test-import--ib-error-facts (err)
  "Return (CONDITION DB-CODE) for a propagated database ERR."
  (list (car err) (plist-get (dorg-dolt--classify-db-error err) :db-code)))

(ert-deftest dorg-test-import-fault-in-merged-statement-rolls-back ()
  "A real fault inside one merged insert escapes the import and changes nothing.
The FK test extension places a duplicate-key row, or a row with a missing
parent, in the middle of its merged child insert run.  For bootstrap and for
a seeded replace, the import propagates the original database condition:
`mysql-query-error' with code 1062 for the duplicate and 1452 for the
missing parent, identical to the scalar effect path for the same fault.
HEAD, the working set, every core, BM25 and extension table, the file
binding and the import marker are unchanged.  The clean counterpart changes
state, so the snapshot is sensitive."
  (dorg-test-import--with-ib-store
    (let ((dorg--extensions nil)
          (fault (list nil))
          (ids (dorg-ib-id-generator))
          merged)
      (dorg-test-import--ib-fault-store dolt-dir fault)
      (cl-flet ((attempt (text mode base)
                  (let ((before (dorg-test-import--ib-state dolt-dir))
                        (lease-fn (symbol-function 'dorg-sql--lease-batch)))
                    (dolist (kind '(duplicate dangling))
                      (setcar fault kind)
                      (let* ((scalar (dorg-test-import--ib-scalar-projection
                                      (lambda ()
                                        (condition-case err
                                            (progn (dorg-ib-import ids dolt-dir dir "corpus"
                                                                   text mode base)
                                                   nil)
                                          (error (dorg-test-import--ib-error-facts err))))))
                             (actual
                              (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                                         (lambda (lease statements &rest rest)
                                           (setq merged
                                                 (cl-find-if
                                                  (lambda (s) (string-prefix-p
                                                               "INSERT INTO ib_child " s))
                                                  statements))
                                           (apply lease-fn lease statements rest))))
                                (condition-case err
                                    (progn (dorg-ib-import ids dolt-dir dir "corpus"
                                                           text mode base)
                                           nil)
                                  (error (dorg-test-import--ib-error-facts err))))))
                        ;; The intended fault was reached on both paths.
                        (should (equal (list 'mysql-query-error
                                             (if (eq kind 'duplicate) 1062 1452))
                                       scalar))
                        (should (equal scalar actual))
                        ;; The fault really sat inside one merged statement.
                        (should (> (cl-count ?\( (substring merged (string-match " VALUES " merged)))
                                   2))
                        (should (equal before (dorg-test-import--ib-state dolt-dir)))))
                    (setcar fault nil)
                    (dorg-ib-import ids dolt-dir dir "corpus" text mode base)
                    (should-not (equal before (dorg-test-import--ib-state dolt-dir))))))
        (attempt dorg-ib-corpus-bootstrap 'bootstrap nil)
        (attempt dorg-ib-corpus-replace 'replace (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-import-mutation-fault-in-merged-statement-is-typed ()
  "A mutation fault inside one merged insert rolls back with pinned facts.
On a seeded store, a title edit of several headings makes the FK test
extension project a merged child insert run with a bad row in the middle.
The internal receipt is observed at the lease-batch boundary and re-signalled
unchanged.  It is a confirmed rollback in phase `statements', whose
:statement-index is the actual ordinal of the merged statement.  Its
statement class is (insert unknown) and its database class equals the scalar
effect path's.  The public refusal carries the same closed facts, and the
private capture holds the failed lowered statement.  HEAD and all state are
unchanged; the clean edit commits."
  (dorg-test-import--with-ib-store
    (let ((dorg--extensions nil)
          (fault (list nil))
          (ids (dorg-ib-id-generator))
          (lease-fn (symbol-function 'dorg-sql--lease-batch)))
      (dorg-test-import--ib-fault-store dolt-dir fault)
      (let* ((root (plist-get (plist-get (dorg-ib-import ids dolt-dir dir "corpus"
                                                         dorg-ib-corpus-bootstrap
                                                         'bootstrap)
                                         :value)
                              :root-id))
             (patches (mapcar (lambda (id)
                                (list :op 'update :target (list :heading :id id)
                                      :fields (list :title (format "Edited %d" id))))
                              '(17 31 29 23 19)))
             (apply-mutation
              (lambda ()
                (dorg-mutate--apply-patches
                 (dorg-ib--source dolt-dir root) patches nil))))
        (dolist (kind '(duplicate dangling))
          (setcar fault kind)
          (let* ((before (dorg-test-import--ib-state dolt-dir))
                 (scalar-receipt nil)
                 (receipt nil)
                 (batch nil)
                 (observe
                  (lambda (sink)
                    (lambda (lease statements &rest rest)
                      (condition-case e
                          (apply lease-fn lease statements rest)
                        (dorg-sql-transaction-rolled-back
                         (funcall sink (cons statements
                                             (dorg-sql-transaction-rollback-data e)))
                         (signal (car e) (cdr e)))))))
                 (public
                  (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                             (funcall observe (lambda (r) (setq batch (car r)
                                                                receipt (cdr r))))))
                    (should-error (funcall apply-mutation) :type 'dorg-mutate-error))))
            (dorg-test-import--ib-scalar-projection
             (lambda ()
               (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                          (funcall observe (lambda (r) (setq scalar-receipt (cdr r))))))
                 (should-error (funcall apply-mutation) :type 'dorg-mutate-error))))
            (let* ((index (1+ (cl-position-if
                               (lambda (s) (string-prefix-p "INSERT INTO ib_child " s))
                               batch)))
                   (facts (dorg-mutate-error-data public))
                   (capture (and (eq 'written (plist-get receipt :capture))
                                 (expand-file-name
                                  (concat (plist-get receipt :capture-id) ".eld")
                                  (expand-file-name ".dorg/failures" dolt-dir)))))
              (should (eq 'statements (plist-get receipt :transaction-phase)))
              (should (eq t (plist-get receipt :rollback-confirmed)))
              (should (= index (plist-get receipt :statement-index)))
              (should (equal '(insert unknown) (plist-get receipt :statement-class)))
              (should (equal (plist-get scalar-receipt :class) (plist-get receipt :class)))
              (should (equal (plist-get scalar-receipt :db-code)
                             (plist-get receipt :db-code)))
              (should (equal (plist-get scalar-receipt :statement-class)
                             (plist-get receipt :statement-class)))
              (dolist (key '(:statement-index :statement-class :db-code :capture))
                (should (equal (plist-get receipt key) (plist-get facts key))))
              ;; Public facts are closed: no SQL text or row values.
              (should-not (cl-some (lambda (v) (and (stringp v)
                                                    (string-match-p "INSERT\\|Edited" v)))
                                   facts))
              (should capture)
              (let ((recorded (with-temp-buffer
                                (insert-file-contents capture)
                                (read (current-buffer)))))
                (should (equal (nth (1- index) batch) (plist-get recorded :statement)))
                (should (= index (plist-get recorded :statement-index)))))
            (should (equal before (dorg-test-import--ib-state dolt-dir)))))
        (setcar fault nil)
        (let ((before (dorg-test-import--ib-state dolt-dir)))
          (funcall apply-mutation)
          (should-not (equal before (dorg-test-import--ib-state dolt-dir))))))))

(defun dorg-test-import--core-state (dolt-dir)
  "Return HEAD, the working-set status and every core row table of DOLT-DIR."
  (list (dorg-head-commit dolt-dir)
        (dorg-sql dolt-dir "SELECT * FROM dolt_status")
        (mapcar (lambda (spec)
                  (dorg-sql dolt-dir (format "SELECT * FROM %s ORDER BY %s"
                                             (car spec) (cdr spec))))
                (cl-subseq dorg-ib-dump-tables 0 9))))

(defun dorg-test-import--commit-outcome (dolt-dir statements)
  "Commit STATEMENTS on DOLT-DIR at HEAD; return `ok' or the error's db code."
  (condition-case err
      (progn (dorg-transaction-commit dolt-dir (dorg-head-commit dolt-dir)
                                      statements "multi-fault control")
             'ok)
    (error (plist-get (dorg-dolt--classify-db-error err) :db-code))))

(ert-deftest dorg-test-import-core-multi-fault-precedence-may-differ ()
  "Documented permitted difference: with two faults, which one fails first may change.
Core rows are generated per heading but executed grouped by table.  A plan
with an orphan heading_items row generated BEFORE a duplicate headings row
fails first on the FK (1452) in generation order, and first on the duplicate
key (1062) in grouped order.  Both run through the real import executor,
roll back, and leave HEAD and every core table unchanged; only that is
promised.  A clean counterpart commits and changes state."
  (dorg-test-import--with-ib-store
    (let* ((root (make-dorg-heading :id 101 :level 0 :level-offset 0
                                    :sort-key (unibyte-string 1) :title "root"))
           (child (make-dorg-heading :id 102 :parent-id 101 :level 1 :level-offset 1
                                     :sort-key (unibyte-string 2) :title "child")))
      (dorg-transaction-commit dolt-dir (dorg-head-commit dolt-dir)
                               (list (dorg-sql-gen-heading-insert root 101))
                               "seed root")
      (let* ((orphan (dorg-sql-row-make
                      "heading_items" '(id heading_id sort_key kind post_blank)
                      (list 900 99999 (cons :hex (unibyte-string 1)) "text" 0)))
             (duplicate (dorg-sql-gen-heading-row root 101))
             (generated (list orphan duplicate))
             (before (dorg-test-import--core-state dolt-dir))
             (scalar (dorg-test-import--commit-outcome
                      dolt-dir (mapcar #'dorg-sql-row-sql generated)))
             (scalar-after (dorg-test-import--core-state dolt-dir))
             (lowered (dorg-test-import--commit-outcome
                       dolt-dir (dorg-sql-lower-statements
                                 (dorg-sql-gen-order-core-rows generated))))
             (lowered-after (dorg-test-import--core-state dolt-dir)))
        (should (eql 1452 scalar))
        (should (eql 1062 lowered))
        (should (equal before scalar-after))
        (should (equal before lowered-after))
        (should (eq 'ok (dorg-test-import--commit-outcome
                         dolt-dir (dorg-sql-lower-statements
                                   (dorg-sql-gen-order-core-rows
                                    (list (dorg-sql-gen-heading-row child 101)))))))
        (should-not (equal before (dorg-test-import--core-state dolt-dir)))))))

(provide 'dorg-import-tests)
;;; dorg-import-tests.el ends here
