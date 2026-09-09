;;; dorg-ext-rebuild-tests.el --- Derived-index rebuild specifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED specifications for the exact-revision derived-index rebuild boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-ext)
(require 'dorg-import)
(require 'dorg-model-load)
(require 'dorg-mutate-tests)
(require 'dorg-repo)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)

(defun dorg-test-pu52f--project-titles (context)
  "Project candidate heading titles into the rebuild fixture index.
The projector deliberately derives only candidate post-state; the rebuild
contract supplies its full exact-revision image as `created' entries."
  (let ((root (dorg-ext-index-context-root-id context))
        effects)
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)
                           (dorg-ext-index-context-moved context)))
      (let ((heading (dorg-ext-index-entry-after entry)))
        (when (and heading (/= (dorg-ext-index-entry-id entry) root))
          (push (list :table "pu52f_ix"
                      :delete
                      (list (cons 'heading_id (dorg-ext-index-entry-id entry))))
                effects)
          (push (list :table "pu52f_ix"
                      :insert
                      (list (cons 'root_id root)
                            (cons 'heading_id (dorg-ext-index-entry-id entry))
                            (cons 'title (or (dorg-heading-title heading) ""))))
                effects))))
    (dolist (entry (dorg-ext-index-context-deleted context))
      (push (list :table "pu52f_ix"
                  :delete
                  (list (cons 'heading_id (dorg-ext-index-entry-id entry))))
            effects))
    (nreverse effects)))

(defun dorg-test-pu52f--project-impure-kind (context)
  "Intentionally non-derivable projector used to falsify parity.
It emits a different marker for an incremental change than for the equivalent
create-only full-snapshot rebuild context."
  (let ((root (dorg-ext-index-context-root-id context))
        effects)
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)))
      (when-let ((heading (dorg-ext-index-entry-after entry)))
        (unless (= (dorg-ext-index-entry-id entry) root)
          (push (list :table "pu52f_impure_ix"
                      :insert
                      (list (cons 'root_id root)
                            (cons 'heading_id (dorg-ext-index-entry-id entry))
                            (cons 'marker (symbol-name
                                           (dorg-ext-index-entry-kind entry)))
                            (cons 'title (or (dorg-heading-title heading) ""))))
                effects))))
    (nreverse effects)))

(defvar dorg-test-pu52f--local-allowed-sections nil
  "Fixture resolver used to expose an import/local configuration split.
The import fixture dynamically supplies inherited sections; mutation and
rebuild use this local-only value, exactly as the downstream split does.")

(defun dorg-test-pu52f--project-local-sections (context)
  "Project Review only when the fixture's local resolver permits it.
This deliberately models a downstream projector whose import resolver can
receive inherited policy while its mutation/rebuild resolver sees local policy
or defaults.  DOrg does not implement either policy resolver."
  (let ((root (dorg-ext-index-context-root-id context))
        effects)
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)
                           (dorg-ext-index-context-moved context)))
      (let ((heading (dorg-ext-index-entry-after entry))
            (id (dorg-ext-index-entry-id entry)))
        (when (and heading (/= id root)
                   (equal (dorg-heading-title heading) "Review"))
          (push (list :table "pu52f_config_ix"
                      :delete (list (cons 'heading_id id)))
                effects)
          (when (member "Review" dorg-test-pu52f--local-allowed-sections)
            (push (list :table "pu52f_config_ix"
                        :insert (list (cons 'root_id root)
                                      (cons 'heading_id id)
                                      (cons 'section "Review")))
                  effects)))))
    (dolist (entry (dorg-ext-index-context-deleted context))
      (push (list :table "pu52f_config_ix"
                  :delete (list (cons 'heading_id
                                      (dorg-ext-index-entry-id entry))))
            effects))
    (nreverse effects)))

(defun dorg-test-pu52f--register-config-split-extension (dolt-dir)
  "Install the config-split fixture extension into DOLT-DIR."
  (dorg-register-extension
   'pu52f-config-split
   :schema "CREATE TABLE pu52f_config_ix (root_id BIGINT, heading_id BIGINT, section TEXT, PRIMARY KEY (root_id, heading_id))"
   :version 1 :capabilities '(dolt)
   :read-tables '((:name "pu52f_config_ix" :key-column "heading_id"
                   :scope-column "root_id"))
   :project #'dorg-test-pu52f--project-local-sections)
  (dorg-repo-ensure-extensions dolt-dir)
  (dorg-commit dolt-dir "install config-split rebuild fixture"))

(defmacro dorg-test-pu52f--with-extension (dolt-dir &rest body)
  "Register rebuild fixture extensions in DOLT-DIR, then evaluate BODY."
  (declare (indent 1))
  `(let ((dorg--extensions nil))
     (dorg-register-extension
      'pu52f-demo
      :schema "CREATE TABLE pu52f_ix (root_id BIGINT, heading_id BIGINT, title TEXT, PRIMARY KEY (root_id, heading_id))"
      :version 1 :capabilities '(dolt)
      :read-tables '((:name "pu52f_ix" :key-column "heading_id" :scope-column "root_id"))
      :project #'dorg-test-pu52f--project-titles)
     (dorg-register-extension
      'pu52f-impure
      :schema "CREATE TABLE pu52f_impure_ix (root_id BIGINT, heading_id BIGINT, marker TEXT, title TEXT, PRIMARY KEY (root_id, heading_id))"
      :version 1 :capabilities '(dolt)
      :read-tables '((:name "pu52f_impure_ix" :key-column "heading_id" :scope-column "root_id"))
      :project #'dorg-test-pu52f--project-impure-kind)
     (dorg-repo-ensure-extensions ,dolt-dir)
     (dorg-commit ,dolt-dir "install pu52f rebuild fixtures")
     ,@body))

(defun dorg-test-pu52f--rows (dolt-dir table root-id)
  "Return TABLE rows in ROOT-ID scope as canonical sorted alists."
  (dorg-sql dolt-dir
            (format "SELECT * FROM %s WHERE root_id = %d ORDER BY heading_id"
                    table root-id)))

(defun dorg-test-pu52f--assert-audit-clean (dolt-dir root)
  "Assert PU52F-DEMO's persisted incremental rows match exact rebuild at ROOT."
  (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                 (plist-get
                  (dorg-ext-rebuild-index
                   'pu52f-demo root :dolt-dir dolt-dir
                   :revision (dorg-head-commit dolt-dir) :mode 'audit)
                  :drift))))

(defun dorg-test-pu52f--no-db-context ()
  "Return a minimal Dolt index context for no-database boundary probes."
  (dorg-ext-index-context-create
   :source-kind 'dolt :root-id 1 :dolt-dir "never-opened"
   :base-commit "fixture-revision" :candidate-id "fixture-candidate"
   :patches nil :created nil :changed nil :moved nil :deleted nil
   :base-image (dorg-ext--index-empty-image)
   :candidate-postimage (dorg-ext--index-empty-image)
   :owner-partitions nil))

(defun dorg-test-pu52f--register-core-scope-probe ()
  "Register the acceptance core-table manifest used by no-DB rebuild probes."
  (dorg-register-extension
   'pu52f-core-scope-probe
   :schema "" :version 1 :capabilities '(dolt)
   :read-tables '((:name "headings" :key-column "id" :scope-column "root_id"))
   :project (lambda (_context) nil)))

(defvar dorg-test-pu52f--collision-projection-effects nil
  "Projection effects returned by the collision no-DB fixture.")

(defun dorg-test-pu52f--collision-row (bead-id section body)
  "Return one full canonical collision-fixture row."
  `((root_id . 1) (bead_id . ,bead-id)
    (section . ,section) (body . ,body)))

(defun dorg-test-pu52f--collision-effects (rows)
  "Return INSERT effects for complete ROWS in the collision fixture."
  (mapcar (lambda (row)
            (list :table "pu52f_collision_ix" :insert row))
          rows))

(defun dorg-test-pu52f--register-collision-probe ()
  "Register a non-unique-key manifest whose projector returns fixture effects."
  (dorg-register-extension
   'pu52f-collision-probe
   :schema "" :version 1 :capabilities '(dolt)
   :read-tables '((:name "pu52f_collision_ix" :key-column "bead_id"
                   :scope-column "root_id"))
   :project (lambda (_context)
              (copy-tree dorg-test-pu52f--collision-projection-effects))))

(ert-deftest dorg-test-pu52f-rebuild-full-row-drift-refuses-key-collisions ()
  "Full-row multiset drift exposes a duplicate-key Review in either input order.
The documented nil key-column fallback has the same complete-row behavior.
Repair must refuse before a transaction because the current Review row is not
fully reproduced by the expected set."
  (let* ((review (dorg-test-pu52f--collision-row 1 "Review" "must survive"))
         (known (dorg-test-pu52f--collision-row 1 "Design" "known"))
         (missing (dorg-test-pu52f--collision-row 2 "Design" "missing"))
         (descriptor '(:name "pu52f_collision_ix" :key-column "bead_id"
                        :scope-column "root_id"))
         (no-key (plist-put (copy-sequence descriptor) :key-column nil)))
    (dolist (current (list (list review known) (list known review)))
      (dolist (expected (list (list known missing) (list missing known)))
        (dolist (scope (list descriptor no-key))
          (cl-letf (((symbol-function 'dorg-ext-rebuild--table-rows)
                     (lambda (&rest _) current)))
            (let ((drift (dorg-ext-rebuild--drift
                          "never-opened" (list scope) 1 "fixture-revision"
                          (dorg-test-pu52f--collision-effects expected))))
              (should (= 1 (plist-get drift :missing-count)))
              (should (= 1 (plist-get drift :extra-count)))
              (should (= 0 (plist-get drift :differing-count)))
              (should (member
                       (list :table "pu52f_collision_ix" :kind 'extra
                             :row (dorg-ext-rebuild--canonical-row review))
                       (plist-get drift :examples)))
              (should-not (dorg-ext-rebuild--repair-safe-p drift)))))))
    (let ((dorg--extensions nil)
          (dorg-test-pu52f--collision-projection-effects
           (dorg-test-pu52f--collision-effects (list known missing)))
          transaction-sql)
      (dorg-test-pu52f--register-collision-probe)
      (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-ext-rebuild--full-context)
                 (lambda (&rest _) (dorg-test-pu52f--no-db-context)))
                ((symbol-function 'dorg-ext-rebuild--table-rows)
                 (lambda (&rest _) (list review known)))
                ((symbol-function 'dorg-head-commit) (lambda (&rest _) "fixture-revision"))
                ((symbol-function 'dorg-transaction-commit)
                 (lambda (_dir _base sql _message &optional _validate)
                   (setq transaction-sql sql)
                   "unexpected-token")))
        (should-error
         (dorg-ext-rebuild-index
          'pu52f-collision-probe 1 :dolt-dir "never-opened"
          :revision "fixture-revision" :mode 'repair)
         :type 'dorg-ext-rebuild-error)
        (should-not transaction-sql)))))

(ert-deftest dorg-test-pu52f-rebuild-collision-repair-guard-has-mutation-teeth ()
  "Removing only the collision safety gate would clear the unexplained Review."
  (let* ((review (dorg-test-pu52f--collision-row 1 "Review" "must survive"))
         (known (dorg-test-pu52f--collision-row 1 "Design" "known"))
         (missing (dorg-test-pu52f--collision-row 2 "Design" "missing"))
         (dorg--extensions nil)
         (dorg-test-pu52f--collision-projection-effects
          (dorg-test-pu52f--collision-effects (list known missing)))
         transaction-sql)
    (dorg-test-pu52f--register-collision-probe)
    (cl-letf (((symbol-function 'dorg-ext-rebuild--repair-safe-p)
               (lambda (_drift) t))
              ((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-ext-rebuild--full-context)
               (lambda (&rest _) (dorg-test-pu52f--no-db-context)))
              ((symbol-function 'dorg-ext-rebuild--table-rows)
               (lambda (&rest _) (list review known)))
              ((symbol-function 'dorg-head-commit) (lambda (&rest _) "fixture-revision"))
              ((symbol-function 'dorg-transaction-commit)
               (lambda (_dir _base sql _message &optional _validate)
                 (setq transaction-sql sql)
                 "unsafe-token")))
      (should (plist-get
               (dorg-ext-rebuild-index
                'pu52f-collision-probe 1 :dolt-dir "never-opened"
                :revision "fixture-revision" :mode 'repair)
               :ok))
      (should (member "DELETE FROM pu52f_collision_ix WHERE root_id = 1"
                      transaction-sql)))))

(ert-deftest dorg-test-pu52f-rebuild-refuses-core-manifest-before-no-db-write ()
  "A DOrg-generated scoped clear can never target a core table.
This is the acceptance boundary probe: no database is opened, and the empty
projector ensures only DOrg's generated clear could reach the transaction."
  (let ((dorg--extensions nil)
        (sql-opened nil)
        (transaction-sql nil))
    (dorg-test-pu52f--register-core-scope-probe)
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-ext-rebuild--full-context)
               (lambda (&rest _) (dorg-test-pu52f--no-db-context)))
              ((symbol-function 'dorg-head-commit) (lambda (&rest _) "fixture-revision"))
              ((symbol-function 'dorg-sql)
               (lambda (&rest _)
                 (setq sql-opened t)
                 '(((id . 1) (root_id . 1)))))
              ((symbol-function 'dorg-transaction-commit)
               (lambda (_dir _base sql _message &optional _validate)
                 (setq transaction-sql sql)
                 "stub-token")))
      (let ((err (should-error
                  (dorg-ext-rebuild-index
                   'pu52f-core-scope-probe 1 :dolt-dir "never-opened"
                   :revision "fixture-revision" :mode 'repair)
                  :type 'dorg-ext-index-error)))
        (should (eq 'core-table (dorg-ext-index-error-reason err)))
        (should-not sql-opened)
        (should-not transaction-sql)))))

(ert-deftest dorg-test-pu52f-rebuild-shared-clear-guard-has-mutation-teeth ()
  "Bypassing the shared effect validator reintroduces the forbidden core DELETE."
  (let ((dorg--extensions nil)
        transaction-sql)
    (dorg-test-pu52f--register-core-scope-probe)
    (cl-letf (((symbol-function 'dorg-ext-validate-index-effect)
               (lambda (effect _owned) (plist-get effect :table)))
              ;; Isolate the shared-effect boundary: this control disables the
              ;; later non-destructive repair gate, not the guard under test.
              ((symbol-function 'dorg-ext-rebuild--repair-safe-p)
               (lambda (_drift) t))
              ((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-ext-rebuild--full-context)
               (lambda (&rest _) (dorg-test-pu52f--no-db-context)))
              ((symbol-function 'dorg-head-commit) (lambda (&rest _) "fixture-revision"))
              ((symbol-function 'dorg-sql)
               (lambda (&rest _) '(((id . 1) (root_id . 1)))) )
              ((symbol-function 'dorg-transaction-commit)
               (lambda (_dir _base sql _message &optional _validate)
                 (setq transaction-sql sql)
                 "stub-token")))
      (let ((receipt
             (dorg-ext-rebuild-index
              'pu52f-core-scope-probe 1 :dolt-dir "never-opened"
              :revision "fixture-revision" :mode 'repair)))
        (should (plist-get receipt :ok))
        (should (equal '("DELETE FROM headings WHERE root_id = 1")
                       transaction-sql))))))

(ert-deftest dorg-test-pu52f-rebuild-legal-scope-uses-shared-effect-guard ()
  "A declared non-core scope remains legal through the shared effect guard."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'pu52f-legal-scope :schema "" :version 1 :capabilities '(dolt)
     :read-tables '((:name "pu52f_legal_ix" :key-column "id"
                     :scope-column "root_id"))
     :project (lambda (_context) nil))
    (let* ((ext (dorg-get-extension 'pu52f-legal-scope))
           (owned (dorg-ext--owned-tables ext))
           (descriptors (dorg-ext-rebuild-scope-tables ext))
           (clears (dorg-ext-rebuild--clear-effects descriptors 1)))
      (should (equal '("pu52f_legal_ix")
                     (mapcar (lambda (effect)
                               (dorg-ext-validate-index-effect effect owned))
                             clears))))))

(ert-deftest dorg-test-pu52f-rebuild-refuses-config-split-destructive-repair ()
  "Repair preserves inherited Review and exposes the local-projector blind spot.
The import leg supplies inherited Review policy while the ordinary mutation and
rebuild legs use the local/default resolver.  This is configuration-equivalent
to the downstream split, but does not claim to implement its resolver."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let ((dorg--extensions nil)
            (dorg--registry nil)
            (source-file (expand-file-name "child.org" temp-base))
            (recovery-file (expand-file-name "recovery.org" temp-base))
            root review-id)
        (dorg-test-pu52f--register-config-split-extension dolt-dir)
        ;; The child has no local allowed-sections keyword.  The binding below
        ;; is its ancestor/import resolver supplying inherited Review.
        (with-temp-buffer
          (insert "#+INHERITED_ALLOWED_SECTIONS: Review\n* Review\n")
          (org-mode)
          (set-visited-file-name source-file t)
          (set-buffer-modified-p nil)
          (let ((dorg-test-pu52f--local-allowed-sections '("Review")))
            (setq root
                  (plist-get
                   (dorg-import-document
                    (current-buffer) :dolt-dir dolt-dir
                    :target-file source-file :mode 'bootstrap)
                   :root-id))))
        (setq review-id (dorg-test-mutate--id-by-title dolt-dir "Review"))
        (should review-id)
        ;; Import maintains Review.  Rebuild's local resolver omits it, so
        ;; audit sees one extra while the canonical core row remains intact.
        (should (equal (dorg-test-pu52f--rows dolt-dir "pu52f_config_ix" root)
                       (list `((root_id . ,root) (heading_id . ,review-id)
                               (section . "Review")))))
        (let ((audit (dorg-ext-rebuild-index
                      'pu52f-config-split root :dolt-dir dolt-dir
                      :revision (dorg-head-commit dolt-dir) :mode 'audit)))
          (should (= 1 (plist-get (plist-get audit :drift) :extra-count))))
        ;; The repair gate refuses before its scope clear, preserving the
        ;; inherited row that this resolver cannot reproduce.
        (should-error
         (dorg-ext-rebuild-index
          'pu52f-config-split root :dolt-dir dolt-dir
          :revision (dorg-head-commit dolt-dir) :mode 'repair)
         :type 'dorg-ext-rebuild-error)
        (should (dorg-test-mutate--id-by-title dolt-dir "Review"))
        (should (dorg-test-pu52f--rows dolt-dir "pu52f_config_ix" root))
        ;; An ordinary local/default mutation deletes the inherited row and
        ;; the same local rebuild cannot discover or recreate it.  This is a
        ;; documented blind spot, not a claimed SIHAX repair.
        (dorg-test-mutate--apply-patch-batch
         (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id root))
         (list (dorg-test-mutate--property-put-patch review-id "MUTATED" "yes")))
        (should (dorg-test-mutate--id-by-title dolt-dir "Review"))
        (should-not (dorg-test-pu52f--rows dolt-dir "pu52f_config_ix" root))
        (let* ((revision (dorg-head-commit dolt-dir))
               (audit (dorg-ext-rebuild-index
                       'pu52f-config-split root :dolt-dir dolt-dir
                       :revision revision :mode 'audit))
               (repair (dorg-ext-rebuild-index
                        'pu52f-config-split root :dolt-dir dolt-dir
                        :revision revision :mode 'repair)))
          (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                         (plist-get audit :drift)))
          (should-not (plist-get repair :new-token)))
        ;; Mutation tooth for the repair gate: a fresh inherited-policy import
        ;; recreates Review, while removing only that gate makes the identical
        ;; local rebuild clear it despite the intact core heading.
        (with-temp-buffer
          (insert "#+INHERITED_ALLOWED_SECTIONS: Review\n* Review\n")
          (org-mode)
          (set-visited-file-name recovery-file t)
          (set-buffer-modified-p nil)
          (let ((dorg-test-pu52f--local-allowed-sections '("Review")))
            (dorg-import-document
             (current-buffer) :dolt-dir dolt-dir
             :target-file source-file :mode 'replace
             :base (dorg-head-commit dolt-dir))))
        (should (dorg-test-pu52f--rows dolt-dir "pu52f_config_ix" root))
        (cl-letf (((symbol-function 'dorg-ext-rebuild--repair-safe-p)
                   (lambda (_drift) t)))
          (dorg-ext-rebuild-index
           'pu52f-config-split root :dolt-dir dolt-dir
           :revision (dorg-head-commit dolt-dir) :mode 'repair))
        (should (dorg-test-mutate--id-by-title dolt-dir "Review"))
        (should-not (dorg-test-pu52f--rows dolt-dir "pu52f_config_ix" root))))))

(ert-deftest dorg-test-pu52f-rebuild-refuses-unreproduced-scoped-rows ()
  "Repair refuses rather than erasing wrong or extra scoped rows."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n* Beta\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (beta (dorg-test-mutate--id-by-title dolt-dir "Beta")))
        (dorg-sql-batch
         dolt-dir
         (list (format "INSERT INTO pu52f_ix VALUES (%d, %d, 'stale')" root alpha)
               (format "INSERT INTO pu52f_ix VALUES (%d, 999999, 'extra')" root)))
        (dorg-commit dolt-dir "seed pu52f scoped drift")
        (let ((before (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root)))
          (should-error
           (dorg-ext-rebuild-index
            'pu52f-demo root :dolt-dir dolt-dir
            :revision (dorg-head-commit dolt-dir) :mode 'repair)
           :type 'dorg-ext-rebuild-error)
          (should (equal before (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root))))))))

(ert-deftest dorg-test-pu52f-rebuild-historical-revision-audits-but-refuses-repair ()
  "Historical exact revisions are read AS OF that core token and cannot repair."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* At revision R\n" 100))
             (revision-r (dorg-head-commit dolt-dir)))
        (dorg-sql-batch dolt-dir
                        (list "UPDATE headings SET title = 'Later HEAD' WHERE title = 'At revision R'"))
        (dorg-commit dolt-dir "advance canonical core past R")
        (let ((audit (dorg-ext-rebuild-index
                      'pu52f-demo root :dolt-dir dolt-dir
                      :revision revision-r :mode 'audit)))
          (should (equal revision-r (plist-get audit :observed-core-revision)))
          (should (= 1 (plist-get (plist-get audit :drift) :missing-count))))
        (should-error
         (dorg-ext-rebuild-index
          'pu52f-demo root :dolt-dir dolt-dir
          :revision revision-r :mode 'repair))))))

(ert-deftest dorg-test-pu52f-rebuild-failure-preserves-other-root-and-core ()
  "A failed replacement leaves target, other root, and canonical core intact."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root-a (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (root-b (dorg-test-mutate--seed dolt-dir "* Beta\n" 200))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (beta (dorg-test-mutate--id-by-title dolt-dir "Beta")))
        (dorg-sql-batch
         dolt-dir
         (list (format "INSERT INTO pu52f_ix VALUES (%d, %d, 'target stale')" root-a alpha)
               (format "INSERT INTO pu52f_ix VALUES (%d, %d, 'other root')" root-b beta)))
        (dorg-commit dolt-dir "seed two-root rebuild state")
        (let ((before-a (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root-a))
              (before-b (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root-b))
              (before-core (dorg-model-load-tree dolt-dir root-a)))
          (cl-letf (((symbol-function 'dorg-ext-collect-index-projection)
                     (lambda (&rest _)
                       (error "projector failure after planning"))))
            (should-error
             (dorg-ext-rebuild-index
              'pu52f-demo root-a :dolt-dir dolt-dir
              :revision (dorg-head-commit dolt-dir) :mode 'repair)
             :type 'dorg-ext-rebuild-error))
          (should (equal before-a (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root-a)))
          (should (equal before-b (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root-b)))
          (should (equal (dorg-ext--index-image-canonical before-core)
                         (dorg-ext--index-image-canonical
                          (dorg-model-load-tree dolt-dir root-a)))))))))

(ert-deftest dorg-test-pu52f-rebuild-matches-incremental-derived-row-set ()
  "A full exact-revision rebuild equals the persisted incremental row set."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root))))
        ;; A real ordinary mutation writes the canonical core title and its
        ;; incremental derived row in one transaction.
        (dorg-test-mutate--apply-patch-batch
         source (list (dorg-test-mutate--heading-update-patch alpha :title "Beta")))
        (let* ((revision (dorg-head-commit dolt-dir))
               (incremental (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root))
               (audit (dorg-ext-rebuild-index
                       'pu52f-demo root :dolt-dir dolt-dir
                       :revision revision :mode 'audit))
               (repair (dorg-ext-rebuild-index
                        'pu52f-demo root :dolt-dir dolt-dir
                        :revision revision :mode 'repair)))
          (should (equal incremental (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root)))
          (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                         (plist-get audit :drift)))
          (should-not (plist-get repair :new-token))
          (should (equal revision (dorg-head-commit dolt-dir))))))))

(ert-deftest dorg-test-pu52f-rebuild-audits-all-ordinary-entry-kinds-and-empty-root ()
  "Exact rebuild agrees after ordinary create/change/move/delete and empty root."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n* Beta\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root))))
        ;; Establish the persisted baseline, then exercise each normal
        ;; mutation classification.  Every audit reads persisted Dolt rows.
        (dorg-ext-rebuild-index
         'pu52f-demo root :dolt-dir dolt-dir
         :revision (dorg-head-commit dolt-dir) :mode 'repair)
        (dolist (patch
                 (list
                  (dorg-test-mutate--tree-insert-patch
                   :root '(:id 300 :title "Created"))
                  (dorg-test-mutate--heading-update-patch alpha :title "Changed")
                  (dorg-test-mutate--move-patch
                   beta (dorg-test-mutate--heading-address alpha))
                  (dorg-test-mutate--delete-patch 300)))
          (dorg-test-mutate--apply-patch-batch source (list patch))
          (dorg-test-pu52f--assert-audit-clean dolt-dir root))
        (let ((empty-root (dorg-test-mutate--seed dolt-dir "" 500)))
          (dorg-test-pu52f--assert-audit-clean dolt-dir empty-root))))))

(ert-deftest dorg-test-pu52f-rebuild-audit-falsifies-entry-kind-dependent-projector ()
  "Parity audit reports drift when a projector depends on entry kind, not post-state."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha")))
        ;; This row is what an incremental change-kind projector would maintain.
        (dorg-sql-batch
         dolt-dir
         (list (format "INSERT INTO pu52f_impure_ix VALUES (%d, %d, 'change', 'Alpha')"
                       root alpha)))
        (dorg-commit dolt-dir "seed impure incremental witness")
        (let ((audit (dorg-ext-rebuild-index
                      'pu52f-impure root :dolt-dir dolt-dir
                      :revision (dorg-head-commit dolt-dir) :mode 'audit)))
          ;; A create-only rebuild expects marker "create", proving the audit
          ;; can expose a non-derivable projector rather than bless it.  Full
          ;; rows differ, so the conservative multiset reports one missing and
          ;; one unexplained extra instead of assuming heading_id is unique.
          (should (= 1 (plist-get (plist-get audit :drift) :missing-count)))
          (should (= 1 (plist-get (plist-get audit :drift) :extra-count))))))))

(ert-deftest dorg-test-pu52f-rebuild-rejects-hostile-scope-column-before-transaction ()
  "Unsafe rebuild scope metadata is a typed refusal before any write opens."
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil)
          (transaction-opened nil))
      (dorg-register-extension
       'pu52f-hostile-scope
       :schema "CREATE TABLE pu52f_hostile_ix (root_id BIGINT, heading_id BIGINT)"
       :version 1 :capabilities '(dolt)
       :read-tables '((:name "pu52f_hostile_ix" :key-column "heading_id"
                       :scope-column "root_id; DELETE FROM headings"))
       :project (lambda (_context) nil))
      (dorg-repo-ensure-extensions dolt-dir)
      (dorg-commit dolt-dir "install hostile-scope fixture")
      (let ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100)))
        (cl-letf (((symbol-function 'dorg-transaction-commit)
                   (lambda (&rest _)
                     (setq transaction-opened t)
                     (error "unexpected rebuild transaction"))))
          (let ((err (should-error
                      (dorg-ext-rebuild-index
                       'pu52f-hostile-scope root :dolt-dir dolt-dir
                       :revision (dorg-head-commit dolt-dir) :mode 'repair)
                      :type 'dorg-ext-index-error)))
            (should (eq 'bad-scope-column (dorg-ext-index-error-reason err)))
            (should-not transaction-opened)))))))

(ert-deftest dorg-test-pu52f-rebuild-root-conjunct-protects-foreign-scope ()
  "The DOrg-generated root conjunct is necessary to preserve foreign rows."
  (dorg-test-mutate--with-dolt
    (dorg-test-pu52f--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (foreign-root 200)
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha")))
        ;; The composite key permits this deliberately adversarial row.  The
        ;; raw projector delete is keyed only by heading_id, so only DOrg's
        ;; generated root predicate can preserve it.
        (dorg-sql-batch
         dolt-dir
         (list (format "INSERT INTO pu52f_ix VALUES (%d, %d, 'foreign')" foreign-root alpha)))
        (dorg-commit dolt-dir "seed foreign-scope delete witness")
        (dorg-ext-rebuild-index
         'pu52f-demo root :dolt-dir dolt-dir
         :revision (dorg-head-commit dolt-dir) :mode 'repair)
        (should (equal (dorg-test-pu52f--rows dolt-dir "pu52f_ix" foreign-root)
                       (list `((root_id . ,foreign-root) (heading_id . ,alpha)
                               (title . "foreign")))))
        ;; Recreate target-root drift without an unexplained current row, so
        ;; conservative repair remains safe to open its transaction.  Mutation
        ;; proof: remove just the generated scope adaptation.  The
        ;; same persisted rebuild must now delete the foreign row, proving the
        ;; preceding preservation assertion is not vacuous.
        (dorg-sql-batch
         dolt-dir
         (list (format "DELETE FROM pu52f_ix WHERE root_id = %d AND heading_id = %d"
                       root alpha)))
        (dorg-commit dolt-dir "reseed scope-conjunct mutation witness")
        (cl-letf (((symbol-function 'dorg-ext-rebuild--scope-effect)
                   (lambda (effect _descriptors _root-id) effect)))
          (dorg-ext-rebuild-index
           'pu52f-demo root :dolt-dir dolt-dir
           :revision (dorg-head-commit dolt-dir) :mode 'repair))
        (should-not (dorg-test-pu52f--rows dolt-dir "pu52f_ix" foreign-root))))))

(provide 'dorg-ext-rebuild-tests)
;;; dorg-ext-rebuild-tests.el ends here
