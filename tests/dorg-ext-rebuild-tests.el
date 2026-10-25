;;; dorg-ext-rebuild-tests.el --- Derived-index rebuild specifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED specifications for the exact-revision derived-index rebuild boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-ext)
(require 'dorg-ext-rebuild)
(require 'dorg-import)
(require 'dorg-api)
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

(defun dorg-test-zjpp4--owner-local-ids (owner-id image)
  "Return OWNER-ID's full plain-descendant closure in IMAGE.
The fixture's rebuild branch derives canonical membership from the complete H1
image, stopping before another BEAD_ID owner independently of partitions."
  (let ((children (make-hash-table :test 'eql))
        (ids (list owner-id))
        queue)
    (maphash (lambda (id heading)
               (push id (gethash (dorg-heading-parent-id heading) children)))
             image)
    (setq queue (copy-sequence (gethash owner-id children)))
    (while queue
      (let* ((id (pop queue))
             (heading (gethash id image)))
        (when (and heading
                   (not (assoc "BEAD_ID"
                               (dorg-model-heading-property-snapshot heading))))
          (push id ids)
          (setq queue (append queue (gethash id children))))))
    (sort ids #'<)))

(defun dorg-test-zjpp4--owner-row (root-id owner-id heading)
  "Return the derived row for HEADING owned by OWNER-ID at ROOT-ID."
  (list (cons 'root_id root-id)
        (cons 'owner_id owner-id)
        (cons 'heading_id (dorg-heading-id heading))
        (cons 'parent_id (dorg-heading-parent-id heading))
        (cons 'title (or (dorg-heading-title heading) ""))))

(defun dorg-test-zjpp4--project-owner-rows (context)
  "Project owner rows for incremental contexts and full rebuild contexts.
The normal mutation path replaces only its affected owner partitions.  Rebuild
uses the same complete-image partition construction, so this fixture consumes
partitions uniformly in both paths."
  (let* ((root-id (dorg-ext-index-context-root-id context))
         (parts (dorg-ext-index-context-owner-partitions context))
         (image (dorg-ext-index-context-candidate-postimage context))
         (rows
          (if parts
              (apply #'append
                     (mapcar
                      (lambda (part)
                        (let ((owner-id (dorg-ext-owner-partition-owner-id part)))
                          (mapcar
                           (lambda (heading)
                             (dorg-test-zjpp4--owner-row root-id owner-id heading))
                           (hash-table-values
                            (dorg-ext-owner-partition-candidate-postimage part)))))
                      parts))
            (let (all)
              (maphash
               (lambda (owner-id owner)
                 (when (assoc "BEAD_ID"
                              (dorg-model-heading-property-snapshot owner))
                   (dolist (id (dorg-test-zjpp4--owner-local-ids owner-id image))
                     (push (dorg-test-zjpp4--owner-row
                            root-id owner-id (gethash id image))
                           all))))
               image)
              all))))
    (append
     (when parts
       (mapcar
        (lambda (part)
          (list :table "zjpp4_owner_ix" :delete
                (list (cons 'root_id root-id)
                      (cons 'owner_id
                            (dorg-ext-owner-partition-owner-id part)))))
        parts))
     (mapcar (lambda (row) (list :table "zjpp4_owner_ix" :insert row)) rows))))

;;; ─── BNP46 — owner-partition rebuild with a transitive child scope ───────

(defun dorg-test-bnp46--owner-row (root-id partition)
  "Return the direct owner row derived from PARTITION."
  (list (cons 'root_id root-id)
        (cons 'owner_id (dorg-ext-owner-partition-owner-id partition))
        (cons 'parent_owner_id
              (dorg-ext-owner-partition-parent-owner-id partition))))

(defun dorg-test-bnp46--project-owner-partitions (context)
  "Project owner and child rows only from CONTEXT owner partitions.
The fixture deliberately has no complete-image fallback.  It therefore proves
that the public rebuild context carries the same canonical owner partitions as
import, including a nested owner\='s parent-owner identity."
  (let ((root-id (dorg-ext-index-context-root-id context))
        effects)
    (dolist (partition (dorg-ext-index-context-owner-partitions context))
      (let ((owner-id (dorg-ext-owner-partition-owner-id partition)))
        (push (list :table "bnp46_owner_ix"
                    :insert (dorg-test-bnp46--owner-row root-id partition))
              effects)
        (maphash
         (lambda (heading-id heading)
           (push (list :table "bnp46_child_ix"
                       :insert
                       (list (cons 'bead_id owner-id)
                             (cons 'heading_id heading-id)
                             (cons 'title (or (dorg-heading-title heading) ""))))
                 effects))
         (dorg-ext-owner-partition-candidate-postimage partition))))
    (nreverse effects)))

(defmacro dorg-test-bnp46--with-extension (dolt-dir &rest body)
  "Install the local owner-partition/transitive-scope rebuild fixture."
  (declare (indent 1))
  `(let ((dorg--extensions nil))
     (dorg-register-extension
      'bnp46-demo
      :schema (list
               "CREATE TABLE bnp46_owner_ix (root_id BIGINT, owner_id BIGINT, parent_owner_id BIGINT NULL, PRIMARY KEY (root_id, owner_id))"
               ;; No key: duplicate full-row accounting is part of the rebuild
               ;; contract and must not be hidden by schema uniqueness.
               "CREATE TABLE bnp46_child_ix (bead_id BIGINT, heading_id BIGINT, title TEXT)")
      :version 1 :capabilities '(dolt)
      :read-tables
      '((:name "bnp46_owner_ix" :key-column "owner_id" :scope-column "root_id")
        (:name "bnp46_child_ix" :key-column "heading_id" :scope-column "bead_id"
         :scope-parent (:table "bnp46_owner_ix" :key-column "owner_id")))
      :project #'dorg-test-bnp46--project-owner-partitions)
     (dorg-migrate-schema ,dolt-dir)
     (dorg-commit ,dolt-dir "install bnp46 rebuild fixture")
     ,@body))

(defun dorg-test-bnp46--audit (dolt-dir root identity &optional mode)
  "Run BNP46 rebuild for ROOT with required IDENTITY."
  (dorg-ext-rebuild-index
   'bnp46-demo root :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
   :identity identity :mode (or mode 'audit)))

(defun dorg-test-dz7ir--audit-with-query
    (extension dolt-dir revision identity query)
  "Run the private audit worker with explicit QUERY for a focused read spy."
  (ignore dolt-dir identity)
  (dorg-ext-rebuild--audit-store-with-query
   extension (dorg-get-extension extension) revision query))

(ert-deftest dorg-test-d0hyz-resolves-only-exact-descriptor-producers ()
  "Resolver output must prove every requested immutable producer component."
  (let ((dorg--extensions nil)
        (policy "(:section-policy v1)"))
    (dorg-register-extension
     'd0hyz-demo :schema "SELECT 1" :version 1
     :producer-resolver
     (lambda (_request)
       (list :project #'ignore :implementation "implementation-v1"
             :dependencies "dependency-v1" :policy policy)))
    (let* ((identity (list :implementation "implementation-v1"
                           :dependencies "dependency-v1"
                           :policy-digest (secure-hash 'sha256 policy)))
           (resolved
            (dorg-ext-rebuild--resolve-policy-producer
             (dorg-get-extension 'd0hyz-demo) (list :producer identity))))
      (should (equal identity (plist-get resolved :identity)))
      (should (eq #'ignore (plist-get resolved :project)))
      (should (equal policy (plist-get resolved :policy))))
    (let ((err (should-error
                (dorg-ext-rebuild--resolve-policy-producer
                 (dorg-get-extension 'd0hyz-demo)
                 (list :producer (list :implementation "implementation-v2"
                                       :dependencies "dependency-v1"
                                       :policy-digest (secure-hash 'sha256 policy))))
                :type 'dorg-ext-rebuild-error)))
      (should (eq 'producer-identity-mismatch
                  (plist-get (dorg-error-data err) :reason))))
    (dorg-register-extension 'd0hyz-demo :schema "SELECT 1" :version 1)
    (let ((err (should-error
                (dorg-ext-rebuild--resolve-policy-producer
                 (dorg-get-extension 'd0hyz-demo)
                 (list :producer (list :implementation "implementation-v1"
                                       :dependencies "dependency-v1"
                                       :policy-digest (secure-hash 'sha256 policy))))
                :type 'dorg-ext-rebuild-error)))
      (should (eq 'producer-resolver-missing
                  (plist-get (dorg-error-data err) :reason))))))

(ert-deftest dorg-test-d0hyz-policy-planner-binds-complete-frozen-images ()
  "The public planner derives a whole-scope C-to-N delta at one revision."
  (let* ((dorg--extensions nil)
         (policy "(:policy v1)")
         (digest (secure-hash 'sha256 policy))
         (old-id (list :implementation "old-v1" :dependencies "dep-v1"
                       :policy-digest digest))
         (new-id (list :implementation "new-v1" :dependencies "dep-v1"
                       :policy-digest digest))
         (request (list :dolt-dir temporary-file-directory :revision "H42" :identity 'dorg
                        :scope '(:root-id 42) :old-producer old-id :new-producer new-id)))
    (dorg-register-extension
     'd0hyz-demo :schema "SELECT 1" :version 1 :capabilities '(dolt)
     :read-tables '((:name "d0hyz_ix" :key-column "heading_id" :scope-column "root_id"))
     :producer-resolver
     (lambda (producer-request)
       (let ((old-p (equal old-id (plist-get producer-request :producer))))
         (list :implementation (if old-p "old-v1" "new-v1")
               :dependencies "dep-v1" :policy policy
               :project (lambda (_context)
                          (list (list :table "d0hyz_ix" :insert
                                      (list (cons 'root_id 42) (cons 'heading_id 7)
                                            (cons 'value (if old-p "old" "new"))))))))))
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-ext-rebuild--policy-upgrade-canonical-root)
               (lambda (_dir root _revision) root))
              ((symbol-function 'dorg-ext-rebuild--full-context) (lambda (&rest _) 'context))
              ((symbol-function 'dorg-ext-rebuild--scope-memberships)
               (lambda (&rest _) (make-hash-table :test 'equal)))
              ((symbol-function 'dorg-ext-rebuild--policy-current-rows)
               (lambda (&rest _) '(("d0hyz_ix" (("heading_id" . 7)
                                                 ("root_id" . 42) ("value" . "old")))))))
      (let ((plan (dorg-ext-rebuild-plan-policy-upgrade 'd0hyz-demo request)))
        (should (plist-get plan :ok))
        (should (plist-get plan :applicable))
        (should (equal "H42" (plist-get plan :base)))
        (should (= 2 (plist-get plan :write-count)))
        (should (= 1 (cdr (assq 'upgrade (plist-get plan :class-counts)))))
        (should (equal old-id (plist-get plan :old-producer)))
        (should (equal new-id (plist-get plan :new-producer)))))))

(ert-deftest dorg-test-d0hyz-policy-upgrade-scope-is-one-closed-root-plist ()
  "Policy upgrades accept exactly one positive canonical root identifier."
  (should (equal '(:root-id 42)
                 (dorg-ext-rebuild--policy-upgrade-scope '(:root-id 42))))
  (dolist (scope '(() (:root-id 0) (:root-id -1) (:root-id "42")
                   (:root-id 42 :extra t) (:extra t :root-id 42)
                   (:root-id 42 :root-id 43) (:root-id)))
    (let ((err (should-error
                (dorg-ext-rebuild--policy-upgrade-scope scope)
                :type 'dorg-ext-rebuild-error)))
      (should (memq (plist-get (dorg-error-data err) :reason)
                    '(scope-malformed scope-root-invalid))))))

(ert-deftest dorg-test-d0hyz-policy-upgrade-request-is-closed-before-read ()
  "The public planner's future request boundary accepts no surrounding authority."
  (let* ((producer (list :implementation "impl-v1" :dependencies "deps-v1"
                         :policy-digest (make-string 64 ?a)))
         (request (list :dolt-dir temporary-file-directory :revision "H42"
                        :identity 'dorg :scope '(:root-id 42)
                        :old-producer producer :new-producer producer))
         (normalized (dorg-ext-rebuild--policy-upgrade-request request)))
    (should (equal request normalized))
    (dolist (bad (list
                  (append request '(:extra t))
                  (append request '(:scope (:root-id 43)))
                  (plist-put (copy-sequence request) :scope '(:root-id 0))
                  (plist-put (copy-sequence request) :old-producer
                             '(:implementation "impl"))))
      (should-error (dorg-ext-rebuild--policy-upgrade-request bad)
                    :type 'dorg-ext-rebuild-error))))

(ert-deftest dorg-test-d0hyz-policy-projection-admits-only-owned-complete-inserts ()
  "Producer images use the existing typed effect guard, not a new SQL seam."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'd0hyz-projection :schema "SELECT 1" :version 1
     :read-tables '((:name "d0hyz_ix" :scope-column "root_id")) )
    (let ((ext (dorg-get-extension 'd0hyz-projection)))
      (should
       (equal '(("d0hyz_ix" (root_id . 42)))
              (dorg-ext-rebuild--policy-projection
               ext (list :project (lambda (_context)
                                    (list (list :table "d0hyz_ix"
                                                :insert '((root_id . 42))))))
               'context)))
      (dolist (effects
               (list (list '(:table "d0hyz_ix" :delete ((root_id . 42))))
                     (list '(:table "foreign_ix" :insert ((root_id . 42))))))
        (should-error
         (dorg-ext-rebuild--policy-projection
          ext (list :project (lambda (_context) effects)) 'context)
         :type 'dorg-ext-rebuild-error)))))

(ert-deftest dorg-test-d0hyz-policy-upgrade-scope-requires-canonical-root-at-h ()
  "A positive scope id is insufficient unless it names a frozen canonical root."
  (let (queries)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (push sql queries)
                 '((id . 42)))))
      (should (= 42 (dorg-ext-rebuild--policy-upgrade-canonical-root
                     "/tmp/d0hyz" 42 "H42")))
      (should (string-match-p "AS OF 'H42'" (car queries)))
      (should (string-match-p "root_id = id" (car queries))))
    (cl-letf (((symbol-function 'dorg-sql) (lambda (&rest _) nil)))
      (let ((err (should-error
                  (dorg-ext-rebuild--policy-upgrade-canonical-root
                   "/tmp/d0hyz" 42 "H42")
                  :type 'dorg-ext-rebuild-error)))
        (should (eq 'scope-root-not-canonical
                    (plist-get (dorg-error-data err) :reason)))))))

(ert-deftest dorg-test-d0hyz-policy-rows-include-empty-declared-tables ()
  "OLD, NEW, and current images retain every manifest table explicitly."
  (let* ((descriptors '((:name "parent_ix" :scope-column "root_id")
                        (:name "child_ix" :scope-column "owner_id"
                         :scope-parent (:table "parent_ix" :key-column "owner_id"))))
         (projection '(("parent_ix" . ((root_id . 42) (value . "A"))))))
    (should
     (equal '(("parent_ix" (("root_id" . 42) ("value" . "A"))) ("child_ix"))
            (dorg-ext-rebuild--policy-rows-by-table projection descriptors)))
    (cl-letf (((symbol-function 'dorg-ext-rebuild--table-rows)
               (lambda (_dir descriptor _root _revision _memberships)
                 (if (string= (plist-get descriptor :name) "child_ix")
                     '(((owner_id . 7) (value . "B"))) nil))))
      (should
       (equal '(("parent_ix") ("child_ix" (("owner_id" . 7) ("value" . "B"))))
              (dorg-ext-rebuild--policy-current-rows
               "/tmp/d0hyz" descriptors 42 "H42" (make-hash-table)))))))

(ert-deftest dorg-test-d0hyz-policy-deltas-refuse-scope-as-one-unit ()
  "One unexplained table suppresses every otherwise valid scoped effect."
  (let* ((descriptors '((:name "left_ix" :scope-column "root_id")
                        (:name "right_ix" :scope-column "root_id")))
         (old '(("left_ix" (("root_id" . 1) ("value" . "old")))
                ("right_ix" (("root_id" . 1) ("value" . "same")))))
         (new '(("left_ix" (("root_id" . 1) ("value" . "new")))
                ("right_ix" (("root_id" . 1) ("value" . "same")))))
         (current '(("left_ix" (("root_id" . 1) ("value" . "old")))
                    ("right_ix" (("root_id" . 1) ("value" . "foreign")))))
         (report (dorg-ext-rebuild--policy-deltas descriptors old new current)))
    (should-not (plist-get report :applicable))
    (should-not (plist-get report :effects))
    (should (= 0 (plist-get report :write-count)))
    (should (= 2 (length (plist-get report :tables))))))

(ert-deftest dorg-test-dz7ir-audit-store-accounts-raw-unowned-rows-at-h ()
  "Raw committed rows remain complete findings, never repair candidates.
The fixture creates stored-only, orphan, ambiguous, undeclared, duplicate and
SQL-NULL cases below the public audit door, then pins the exact frozen read
plan and the ambiguous owner candidates in the returned report."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let ((root (dorg-test-mutate--seed
                   dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100)))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch
         dolt-dir
         (list
          ;; Direct rows establish a stored-only root, an ambiguous parent
          ;; key (one canonical and one stored-only owner), and no scope.
          (format "INSERT INTO bnp46_owner_ix VALUES (999, 777, NULL)")
          (format "INSERT INTO bnp46_owner_ix VALUES (%d, 800, NULL)" root)
          "INSERT INTO bnp46_owner_ix VALUES (999, 800, NULL)"
          ;; The duplicate retains its full-row multiplicity; the NULL scope
          ;; stays distinct from the empty string and the orphan has no parent.
          "INSERT INTO bnp46_child_ix VALUES (777, 701, 'stored')"
          "INSERT INTO bnp46_child_ix VALUES (777, 701, 'stored')"
          "INSERT INTO bnp46_child_ix VALUES (800, 702, 'ambiguous')"
          "INSERT INTO bnp46_child_ix VALUES (9999, 703, 'orphan')"
          "INSERT INTO bnp46_child_ix VALUES (NULL, 704, NULL)"))
        (dorg-commit dolt-dir "seed whole-store audit raw findings")
        (let* ((revision (dorg-head-commit dolt-dir))
               (queries nil)
               (report (dorg-test-dz7ir--audit-with-query
                        'bnp46-demo dolt-dir revision '(:property "BEAD_ID")
                        (lambda (sql) (push sql queries) (dorg-sql dolt-dir sql))))
               (child (cl-find "bnp46_child_ix" (plist-get report :tables)
                               :key (lambda (table) (plist-get table :table))
                               :test #'string=))
               (stored (car (alist-get 'owned-stored-only-root
                                       (plist-get child :classes))))
               (ambiguous (car (alist-get 'ambiguous (plist-get child :classes)))))
          (should (plist-get report :complete))
          (should-not (plist-get report :clean))
          (should (= (length queries) (plist-get report :statement-count)))
          (should (= 3 (plist-get report :statement-count)))
          (should (cl-every (lambda (sql)
                              (string-match-p (regexp-quote (format "AS OF '%s'" revision)) sql))
                            queries))
          (should (= 2 (plist-get stored :multiplicity)))
          (should (equal (list 999) (plist-get stored :owners)))
          (should (equal (sort (list root 999) #'<) (plist-get ambiguous :owners)))
          (should (= 1 (alist-get 'orphan-child (plist-get child :class-counts))))
          (should (= 1 (alist-get 'undeclared-scope (plist-get child :class-counts))))
          (should (equal '(("bead_id") ("heading_id" . 704) ("title"))
                         (plist-get (car (alist-get 'undeclared-scope
                                                    (plist-get child :classes))) :row))))))))

(ert-deftest dorg-test-dz7ir-audit-store-statement-count-is-root-invariant ()
  "The 1+T census is invariant under canonical-root and row counts."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (cl-labels
          ((audit-count
            ()
            (let ((queries nil))
              (let ((report (dorg-test-dz7ir--audit-with-query
                             'bnp46-demo dolt-dir (dorg-head-commit dolt-dir)
                             '(:property "BEAD_ID")
                             (lambda (sql) (push sql queries) (dorg-sql dolt-dir sql)))))
                (should (plist-get report :complete))
                (should (= (length queries) (plist-get report :statement-count)))
                (plist-get report :statement-count)))))
        (let* ((root-one (dorg-test-mutate--seed
                          dolt-dir "* First\n:PROPERTIES:\n:BEAD_ID: first\n:END:\n** First leaf\n" 100)))
          (dorg-test-bnp46--audit dolt-dir root-one '(:property "BEAD_ID") 'repair)
          (let ((one (audit-count))
                (root-two (dorg-test-mutate--seed
                           dolt-dir "* Second\n:PROPERTIES:\n:BEAD_ID: second\n:END:\n** Second leaf\n" 200)))
            (dorg-test-bnp46--audit dolt-dir root-two '(:property "BEAD_ID") 'repair)
            (let ((two (audit-count)))
              ;; Extra stored rows do not add a per-row lookup or statement.
              (dorg-sql-batch dolt-dir
                              (list "INSERT INTO bnp46_child_ix VALUES (424242, 424243, 'orphan')"))
              (dorg-commit dolt-dir "add a second audit-row witness")
              (let ((same-roots (audit-count)))
                (should (= 3 one))
                (should (= one two))
                (should (= two same-roots))))))))))

(ert-deftest dorg-test-dz7ir-audit-store-pins-every-read-through-head-interloper ()
  "An external HEAD advance cannot mix post-H rows into the whole-store report."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let ((root (dorg-test-mutate--seed
                   dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100)))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (let* ((revision (dorg-head-commit dolt-dir))
               (calls 0)
               (queries nil)
               (report (dorg-test-dz7ir--audit-with-query
                        'bnp46-demo dolt-dir revision '(:property "BEAD_ID")
                        (lambda (sql)
                          (let ((rows (dorg-sql dolt-dir sql)))
                            (push sql queries)
                            (cl-incf calls)
                            (when (= calls 1)
                              ;; This intentionally advances live HEAD only after the
                              ;; frozen inventory read; the audit must stay at H.
                              (dorg-sql-batch dolt-dir
                                              (list "INSERT INTO bnp46_owner_ix VALUES (999, 777, NULL)"))
                              (dorg-commit dolt-dir "advance HEAD during audit witness"))
                            rows)))))
          (let ((owner (cl-find "bnp46_owner_ix" (plist-get report :tables)
                                :key (lambda (table) (plist-get table :table))
                                :test #'string=)))
            (should (= 1 (plist-get owner :rows-read)))
            (should (= 1 (alist-get 'owned-canonical-root
                                    (plist-get owner :class-counts)))))
          (should (plist-get report :complete))
          (should-not (equal revision (dorg-head-commit dolt-dir)))
          (should (cl-every (lambda (sql)
                              (string-match-p (regexp-quote (format "AS OF '%s'" revision)) sql))
                            queries)))))))

(ert-deftest dorg-test-dz7ir-audit-classifies-complete-row-multisets ()
  "Whole-store classification preserves every row occurrence and owner facts."
  (let* ((descriptors
          '((:name "parent_ix" :scope-column "root_id" :key-column "owner_id")
            (:name "child_ix" :scope-column "owner_id" :key-column "heading_id"
             :scope-parent (:table "parent_ix" :key-column "owner_id"))))
         ;; Owner 10 occurs under both roots: a child naming it is ambiguous.
         ;; Owner 20 is stored-only, and 99 is an orphan.  The nil root is an
         ;; undeclared scope rather than a fabricated owner.
         (rows (list (cons "parent_ix"
                           '(((root_id . 1) (owner_id . 10))
                             ((root_id . 1) (owner_id . 1))
                             ((root_id . 2) (owner_id . 10))
                             ((root_id . 2) (owner_id . 20))
                             ((root_id . nil) (owner_id . 30))))
                     (cons "child_ix"
                           '(((owner_id . 1) (heading_id . 101) (title . "A"))
                             ((owner_id . 20) (heading_id . 102) (title . "B"))
                             ((owner_id . 10) (heading_id . 103) (title . "C"))
                             ((owner_id . 99) (heading_id . 104) (title . "D"))
                             ((owner_id . nil) (heading_id . 105) (title . "E"))
                             ((owner_id . 1) (heading_id . 101) (title . "A"))))))
         (report (dorg-ext-rebuild--audit-classify-rows descriptors rows '(1)))
         (parent (cl-find "parent_ix" (plist-get report :tables)
                          :key (lambda (r) (plist-get r :table))
                          :test #'string=))
         (child (cl-find "child_ix" (plist-get report :tables)
                         :key (lambda (r) (plist-get r :table))
                         :test #'string=)))
    (should (= 5 (plist-get parent :rows-read)))
    (should (= 5 (plist-get parent :accounted-count)))
    (should (= 6 (plist-get child :rows-read)))
    (should (= 6 (plist-get child :accounted-count)))
    (should (= 2 (alist-get 'owned-canonical-root (plist-get child :class-counts))))
    (should (= 1 (alist-get 'owned-stored-only-root (plist-get child :class-counts))))
    (should (= 1 (alist-get 'ambiguous (plist-get child :class-counts))))
    (should (= 1 (alist-get 'orphan-child (plist-get child :class-counts))))
    (should (= 1 (alist-get 'undeclared-scope (plist-get child :class-counts))))
    (should-not (plist-get report :clean))))

(ert-deftest dorg-test-d0hyz-policy-classifies-complete-row-absence-states ()
  "Policy planning distinguishes full-row states without NULL collapse.

Rows which share an application-level column but have different complete
payloads are separate multiset members.  In particular, an OLD payload kept
in CURRENT and a changed NEW payload require one deletion and one insertion;
they are never paired into an UPDATE."
  (let* ((old '(((id . 1) (value . "same"))
                ((id . 2) (value . "old"))
                ((id . 5) (value . "removed"))))
         (new '(((id . 1) (value . "same"))
                ((id . 2) (value . "new"))
                ((id . 3) (value . "already"))
                ((id . 4) (value . "missing"))))
         (current '(((id . 1) (value . "same"))
                    ((id . 2) (value . "old"))
                    ((id . 3) (value . "already"))
                    ((id . 6) (value . "extra"))))
         (report (dorg-ext-rebuild--policy-classify old new current)))
    (should (= 1 (alist-get 'unchanged (plist-get report :class-counts))))
    (should (= 1 (alist-get 'upgrade (plist-get report :class-counts))))
    (should (= 2 (alist-get 'already-new (plist-get report :class-counts))))
    (should (= 2 (alist-get 'missing-new (plist-get report :class-counts))))
    (should (= 1 (alist-get 'extra (plist-get report :class-counts))))
    (should (= 0 (alist-get 'unexplained (plist-get report :class-counts))))
    ;; `id' 5 is OLD-only and absent currently: already removed/new.  `id' 2
    ;; is two full rows, so its OLD value is deleted and its NEW value is
    ;; inserted; `id' 4 is a second insertion.  Neither absence is NULL.
    ;; An extra row makes the whole plan inapplicable, so its otherwise
    ;; meaningful C→N delta is deliberately not emitted.
    (should-not (plist-get report :applicable))
    (should (= 0 (plist-get report :delete-count)))
    (should (= 0 (plist-get report :insert-count)))))

(ert-deftest dorg-test-d0hyz-policy-counts-identical-duplicates-by-rank ()
  "The C→N delta uses complete-row multiplicity, not O→N membership.

With O=3, N=1, C=2, one current occurrence remains NEW and only the excess
one is deleted.  The old unique-key classifier would have refused this valid
identical-duplicate transition."
  (let* ((row '((id . 7) (value . "same") (blob . "\0bytes")))
         (report (dorg-ext-rebuild--policy-classify
                  (list row row row) (list row) (list row row))))
    (should (= 1 (alist-get 'unchanged (plist-get report :class-counts))))
    (should (= 1 (alist-get 'upgrade (plist-get report :class-counts))))
    (should (= 1 (alist-get 'already-new (plist-get report :class-counts))))
    (should (= 0 (alist-get 'unexplained (plist-get report :class-counts))))
    (should (= 1 (plist-get report :delete-count)))
    (should (= 0 (plist-get report :insert-count)))))

(ert-deftest dorg-test-d0hyz-policy-inserts-missing-unchanged-row ()
  "C=0 still inserts an identical O=N complete row.

The write delta is C→N after explainability, so OLD and NEW having the same
row does not make a missing CURRENT occurrence a no-op."
  (let* ((row '((id . 9) (value . nil) (payload . "trailing ")))
         (report (dorg-ext-rebuild--policy-classify (list row) (list row) nil)))
    (should (= 1 (alist-get 'missing-new (plist-get report :class-counts))))
    (should (= 0 (plist-get report :delete-count)))
    (should (= 1 (plist-get report :insert-count)))))

(ert-deftest dorg-test-d0hyz-policy-refuses-excess-complete-duplicate ()
  "CURRENT multiplicity beyond max(O,N) is unexplained before writes."
  (let* ((row '((id . 10) (value . "same")))
         (report (dorg-ext-rebuild--policy-classify (list row) (list row)
                                                   (list row row))))
    (should (= 1 (alist-get 'unchanged (plist-get report :class-counts))))
    (should (= 1 (alist-get 'unexplained (plist-get report :class-counts))))
    (should-not (plist-get report :applicable))
    (should (= 0 (plist-get report :delete-count)))
    (should (= 0 (plist-get report :insert-count)))))

(ert-deftest dorg-test-d0hyz-policy-lowers-a-counted-byte-exact-delete ()
  "Policy deletes retain every cell and limit precisely one duplicate copy."
  (should
   (equal
    "DELETE FROM policy_ix WHERE id <=> 7 AND BINARY payload <=> BINARY 'A''B\\\\n' AND nullable <=> NULL LIMIT 1"
    (dorg-ext-rebuild--policy-delete-sql
     '(:table "policy_ix" :delete ((id . 7) (payload . "A'B\\n") (nullable)))
     '("policy_ix") 1))))

(ert-deftest dorg-test-d0hyz-policy-refuses-an-uncounted-delete ()
  "The duplicate-safe lowerer cannot fall back to a whole-row mass delete."
  (let ((err (should-error
              (dorg-ext-rebuild--policy-delete-sql
               '(:table "policy_ix" :delete ((id . 7))) '("policy_ix") 0)
              :type 'dorg-ext-rebuild-error)))
    (should (eq 'policy-delete-count (plist-get (dorg-error-data err) :reason)))))

(defun dorg-test-d0hyz--byte-corpus-expected (targets near-misses)
  "Sorted multiset left once one copy of each of TARGETS is deleted.
The seed holds TARGETS twice and NEAR-MISSES once.  `sort' reuses the conses
of the list it is given, so that list must be wholly fresh: sorting
\(append (copy-sequence TARGETS) NEAR-MISSES) rewrites the caller's
NEAR-MISSES before they are seeded."
  (sort (append targets near-misses nil) #'string<))

(ert-deftest dorg-test-d0hyz-byte-corpus-expected-keeps-its-inputs ()
  "The byte-corpus expectation is the exact sorted multiset and leaves both
input lists untouched.  A destructive sort over conses shared with the near
misses once rewrote the seeded corpus, and the native result looked like an
engine fault."
  (let* ((targets (list "it's" "line\nbreak" "\u00fcn\u00efc\u00f6d\u00e9 \u2713 \u65e5\u672c"
                        "back\\slash"))
         (near-misses (list "its" "line\r\nbreak" "unicode \u2713 \u65e5\u672c"
                            "back/slash"))
         (snapshot (copy-tree (list targets near-misses))))
    (should (equal '("back/slash" "back\\slash" "it's" "its" "line\nbreak"
                     "line\r\nbreak" "unicode \u2713 \u65e5\u672c"
                     "\u00fcn\u00efc\u00f6d\u00e9 \u2713 \u65e5\u672c")
                   (dorg-test-d0hyz--byte-corpus-expected targets near-misses)))
    (should (equal snapshot (list targets near-misses)))))

(defun dorg-test-d0hyz--native-byte-corpus (dolt-dir targets near-misses)
  "Round-trip TARGETS (twice each) and NEAR-MISSES through generated SQL.
Every insert effect passes the closed effect validator before it is lowered.
Before any delete the seed must read back as exactly that multiset, both as
stored bytes (SQL HEX against the expected UTF-8 encoding) and as decoded
values, so neither a write-side nor a read-side coercion can pass as success.
One copy of each target is then removed by the counted policy delete and the
remaining payloads are returned sorted.  This runs on an owned native store;
the public apply sends the same statements through `dorg-transaction-commit'
under its live lease."
  (let* ((owned '("d0hyz_bytes_ix"))
         (seed (append targets targets near-misses))
         (payloads (lambda ()
                     (sort (mapcar (lambda (row) (alist-get 'payload row))
                                   (dorg-sql dolt-dir "SELECT payload FROM d0hyz_bytes_ix"))
                           #'string<))))
    (dorg-sql-batch dolt-dir (list "CREATE TABLE d0hyz_bytes_ix (id BIGINT, payload TEXT)"))
    (dorg-sql-batch
     dolt-dir
     (mapcar (lambda (payload)
               (let ((effect (list :table "d0hyz_bytes_ix"
                                   :insert (list (cons 'id 1) (cons 'payload payload)))))
                 (should (equal "d0hyz_bytes_ix" (dorg-ext-validate-index-effect effect owned)))
                 (dorg-ext--index-effect-sql effect)))
             seed))
    (dorg-commit dolt-dir "seed policy byte corpus")
    ;; Stored bytes first (independent of any client decoding), then the
    ;; decoded values.
    (should (equal (sort (mapcar (lambda (payload)
                                   (upcase (mapconcat (lambda (byte) (format "%02x" byte))
                                                      (encode-coding-string payload 'utf-8-unix)
                                                      "")))
                                 seed)
                         #'string<)
                   (sort (mapcar (lambda (row) (alist-get 'hex row))
                                 (dorg-sql dolt-dir
                                           "SELECT HEX(payload) AS hex FROM d0hyz_bytes_ix"))
                         #'string<)))
    (should (equal (sort (copy-sequence seed) #'string<) (funcall payloads)))
    (dorg-sql-batch
     dolt-dir
     (mapcar (lambda (payload)
               (dorg-ext-rebuild--policy-delete-sql
                (list :table "d0hyz_bytes_ix"
                      :delete (list (cons 'id 1) (cons 'payload payload)))
                owned 1))
             targets))
    (dorg-commit dolt-dir "delete one copy of each corpus row")
    (funcall payloads)))

(ert-deftest dorg-test-d0hyz-policy-byte-corpus-is-native-dolt-exact ()
  "On pinned Dolt a quote, a real newline, a backslash and multibyte Unicode
round-trip through the generated INSERT and the counted binary DELETE: one copy
of each is removed, and near misses (no quote, CRLF, ASCII u, slash) survive."
  (dorg-test-mutate--with-dolt
    (let* ((targets (list "it's" "line\nbreak" "\u00fcn\u00efc\u00f6d\u00e9 \u2713 \u65e5\u672c"
                          "back\\slash"))
           (near-misses (list "its" "line\r\nbreak" "unicode \u2713 \u65e5\u672c"
                              "back/slash"))
           (snapshot (copy-tree (list targets near-misses)))
           (expected (dorg-test-d0hyz--byte-corpus-expected targets near-misses)))
      ;; The expectation must not have rewritten the corpus about to be seeded.
      (should (equal snapshot (list targets near-misses)))
      (should (equal expected
                     (dorg-test-d0hyz--native-byte-corpus dolt-dir targets near-misses))))))

(ert-deftest dorg-test-d0hyz-policy-nul-byte-is-native-dolt-exact ()
  "On pinned Dolt a payload containing NUL round-trips through the generated
INSERT and one copy is removed by the counted binary DELETE, leaving the
NUL-free near miss intact.  Kept separate so its outcome qualifies NUL alone."
  (dorg-test-mutate--with-dolt
    (should (equal (sort (list "nul" "nul\0byte") #'string<)
                   (dorg-test-d0hyz--native-byte-corpus
                    dolt-dir (list "nul\0byte") (list "nul"))))))

(ert-deftest dorg-test-d0hyz-policy-counted-delete-is-native-dolt-exact ()
  "Native Dolt deletes one exact duplicate without collapsing bytes or NULL.

The retained case-distinct row proves the predicate is binary rather than a
collation-dependent text equality; the retained duplicate proves LIMIT is part
of the generated transition rather than a diagnostic count."
  (dorg-test-mutate--with-dolt
    (dorg-sql-batch
     dolt-dir
     (list "CREATE TABLE d0hyz_policy_ix (id BIGINT, payload TEXT, nullable TEXT)"
           "INSERT INTO d0hyz_policy_ix (id, payload, nullable) VALUES (7, 'Case ', NULL)"
           "INSERT INTO d0hyz_policy_ix (id, payload, nullable) VALUES (7, 'Case ', NULL)"
           "INSERT INTO d0hyz_policy_ix (id, payload, nullable) VALUES (7, 'case ', NULL)"))
    (dorg-commit dolt-dir "seed counted policy delete")
    (let* ((sql
            (dorg-ext-rebuild--policy-delete-sql
             '(:table "d0hyz_policy_ix"
               :delete ((id . 7) (payload . "Case ") (nullable)))
             '("d0hyz_policy_ix") 1)))
      ;; Executing the generated statement on this owned native store proves
      ;; pinned Dolt accepts its binary/NULL/LIMIT form.  The public planner later sends the same
      ;; statement through `dorg-transaction-commit' under its live lease.
      (dorg-sql-batch dolt-dir (list sql))
      (dorg-commit dolt-dir "delete one policy duplicate")
      (let ((rows (dorg-sql dolt-dir
                            "SELECT payload, nullable FROM d0hyz_policy_ix ORDER BY payload")))
        (should (equal '(((payload . "Case ") (nullable))
                         ((payload . "case ") (nullable)))
                       rows))))))

(ert-deftest dorg-test-d0hyz-policy-delta-follows-current-to-new-counts ()
  "Effects delete C−N and insert N−C, never the OLD-to-NEW difference.
Inputs are canonical image rows; the effects carry symbol columns."
  (let* ((same (dorg-ext-rebuild--canonical-row '((id . 1) (value . "same"))))
         (retained (dorg-ext-rebuild--canonical-row
                    '((id . 2) (value . "retained-new"))))
         (report
          (dorg-ext-rebuild--policy-delta
           "policy_ix"
           (list same same same retained)
           (list same retained)
           (list same same retained))))
    ;; O=3, N=1, C=2 for SAME: delete one, not two.  RETAINED is unchanged.
    (should (plist-get report :applicable))
    (should
     (equal '((:table "policy_ix" :delete ((id . 1) (value . "same")) :count 1))
            (plist-get report :deletes)))
    (should-not (plist-get report :inserts))))

(ert-deftest dorg-test-d0hyz-policy-delta-inserts-missing-unchanged-row ()
  "O=N does not make a missing CURRENT row a no-op."
  (let* ((row (dorg-ext-rebuild--canonical-row '((id . 8) (value . "same"))))
         (report (dorg-ext-rebuild--policy-delta "policy_ix" (list row) (list row) nil)))
    (should (plist-get report :applicable))
    (should-not (plist-get report :deletes))
    (should (equal '((:table "policy_ix" :insert ((id . 8) (value . "same"))))
                   (plist-get report :inserts)))))

(ert-deftest dorg-test-d0hyz-policy-delta-lowers-only-owned-typed-effects ()
  "The delta SQL uses the counted delete lowerer and shared insert guard."
  (let ((delta
         '(:deletes ((:table "policy_ix" :delete ((id . 1) (value . "old")) :count 2))
           :inserts ((:table "policy_ix" :insert ((id . 2) (value . "new")))))))
    (should
     (equal
      '("DELETE FROM policy_ix WHERE id <=> 1 AND BINARY value <=> BINARY 'old' LIMIT 2"
        "INSERT INTO policy_ix (id, value) VALUES (2, 'new')")
      (dorg-ext-rebuild--policy-delta-sql delta '("policy_ix"))))))

;;; ─── Planner review corrections: scope, row boundary, stable digests ───

(defun dorg-test-d0hyz--producer-id (implementation)
  "Return a closed producer identity for IMPLEMENTATION."
  (list :implementation implementation :dependencies "deps"
        :policy-digest (secure-hash 'sha256 "(:policy)")))

(defun dorg-test-d0hyz--register-scoped (old-effects new-effects &optional drift)
  "Register a parent/child policy fixture whose producers emit OLD/NEW-EFFECTS.
DRIFT, when non-nil, is a cons cell whose car is a plist of resolver output
overrides (`:implementation', `:dependencies', `:policy') applied to the OLD
producer, so a test can change the installed producer between plan and apply."
  (dorg-register-extension
   'd0hyz-scoped :schema "SELECT 1" :version 1 :capabilities '(dolt)
   :read-tables
   '((:name "d0p_parent_ix" :key-column "owner_id" :scope-column "root_id")
     (:name "d0p_child_ix" :key-column "heading_id" :scope-column "owner_id"
      :scope-parent (:table "d0p_parent_ix" :key-column "owner_id")))
   :producer-resolver
   (lambda (request)
     (let* ((implementation (plist-get (plist-get request :producer) :implementation))
            (old-p (equal implementation "old"))
            (effects (if old-p old-effects new-effects))
            (override (and old-p drift (car drift))))
       (list :implementation (or (plist-get override :implementation) implementation)
             :dependencies (or (plist-get override :dependencies) "deps")
             :policy (or (plist-get override :policy) "(:policy)")
             :project (lambda (_context) (copy-tree effects)))))))

(defun dorg-test-d0hyz--plan-scoped (current &optional ambiguous queries-cell)
  "Plan the registered `d0hyz-scoped' upgrade over fake frozen store reads.
CURRENT is an alist of table name to stored rows (returned in that order).
AMBIGUOUS rows answer the parent-key ambiguity query.  Every SQL statement is
pushed onto QUERIES-CELL's car.  A transaction call fails the test."
  (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
            ((symbol-function 'dorg-ext-rebuild--full-context) (lambda (&rest _) 'ctx))
            ((symbol-function 'dorg-transaction-commit)
             (lambda (&rest _) (ert-fail "planner opened a transaction")))
            ((symbol-function 'dorg-sql)
             (lambda (_dir sql)
               (when queries-cell (push sql (car queries-cell)))
               (cond
                ((string-match-p "FROM headings" sql) '(((id . 42))))
                ((string-match-p "!= 42" sql) ambiguous)
                ((string-match-p "\\`SELECT owner_id FROM d0p_parent_ix" sql)
                 (mapcar (lambda (row) (list (assq 'owner_id row)))
                         (cdr (assoc "d0p_parent_ix" current))))
                ((string-match-p "\\`SELECT \\* FROM d0p_parent_ix" sql)
                 (copy-tree (cdr (assoc "d0p_parent_ix" current))))
                ((string-match-p "\\`SELECT \\* FROM d0p_child_ix" sql)
                 (copy-tree (cdr (assoc "d0p_child_ix" current))))
                (t (error "unexpected planner SQL: %s" sql))))))
    (dorg-ext-rebuild-plan-policy-upgrade
     'd0hyz-scoped
     (list :dolt-dir temporary-file-directory :revision "H42" :identity 'dorg
           :scope '(:root-id 42)
           :old-producer (dorg-test-d0hyz--producer-id "old")
           :new-producer (dorg-test-d0hyz--producer-id "new")))))

(defconst dorg-test-d0hyz--scoped-old
  '((:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 10)))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 1) (payload . "old"))))
  "Valid OLD image for root 42.")

(defconst dorg-test-d0hyz--scoped-new
  '((:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 10)))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 1) (payload . "new"))))
  "Valid NEW image for root 42.")

(defconst dorg-test-d0hyz--scoped-current
  '(("d0p_parent_ix" ((root_id . 42) (owner_id . 10)))
    ("d0p_child_ix" ((owner_id . 10) (heading_id . 1) (payload . "old"))))
  "Stored OLD state at H.")

(defun dorg-test-d0hyz--scope-refusal (old new &optional ambiguous)
  "Assert planning OLD/NEW over the stored OLD state refuses; return its data.
The current derived tables are never read for a producer scope violation."
  (let ((dorg--extensions nil) (queries (list nil)))
    (dorg-test-d0hyz--register-scoped old new)
    (let ((err (should-error
                (dorg-test-d0hyz--plan-scoped dorg-test-d0hyz--scoped-current
                                              ambiguous queries)
                :type 'dorg-ext-rebuild-error)))
      (list :data (dorg-error-data err) :queries (car queries)))))

(ert-deftest dorg-test-d0hyz-policy-planner-scope-positive-control ()
  "Both producer images inside the requested root plan an applicable C-to-N."
  (let ((dorg--extensions nil))
    (dorg-test-d0hyz--register-scoped dorg-test-d0hyz--scoped-old
                                      dorg-test-d0hyz--scoped-new)
    (let ((plan (dorg-test-d0hyz--plan-scoped dorg-test-d0hyz--scoped-current)))
      (should (plist-get plan :applicable))
      (should (= 2 (plist-get plan :write-count)))
      (should (= 1 (cdr (assq 'upgrade (plist-get plan :class-counts))))))))

(ert-deftest dorg-test-d0hyz-policy-planner-refuses-producer-rows-outside-root ()
  "A producer row naming another direct root refuses the whole plan.
Both images are checked: an OLD-only violation refuses as well as one emitted
by both producers, before any current derived table is read."
  (let ((foreign '(:table "d0p_parent_ix" :insert ((root_id . 99) (owner_id . 90)))))
    (dolist (images (list (list (list foreign) (list foreign))
                          (list (cons foreign dorg-test-d0hyz--scoped-old)
                                dorg-test-d0hyz--scoped-new)
                          (list dorg-test-d0hyz--scoped-old
                                (cons foreign dorg-test-d0hyz--scoped-new))))
      (let ((result (dorg-test-d0hyz--scope-refusal (nth 0 images) (nth 1 images))))
        (should (eq 'producer-scope-violation
                    (plist-get (plist-get result :data) :reason)))
        (should-not (cl-some (lambda (sql) (string-match-p "\\`SELECT \\*" sql))
                             (plist-get result :queries)))))))

(ert-deftest dorg-test-d0hyz-policy-planner-refuses-child-outside-parent-scope ()
  "A child row whose parent key is not projected under the root refuses."
  (let* ((stray '(:table "d0p_child_ix"
                  :insert ((owner_id . 77) (heading_id . 2) (payload . "x"))))
         (result (dorg-test-d0hyz--scope-refusal
                  dorg-test-d0hyz--scoped-old
                  (append dorg-test-d0hyz--scoped-new (list stray)))))
    (should (eq 'producer-scope-violation
                (plist-get (plist-get result :data) :reason)))))

(ert-deftest dorg-test-d0hyz-policy-planner-scopes-each-image-on-its-own-parents ()
  "One producer image's projected parent cannot authorize the other's child.
OLD projects parent 77 while NEW emits a child under 77 without projecting that
parent, and the converse; both refuse before any current table is read."
  (let ((parent77 '(:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 77))))
        (child77 '(:table "d0p_child_ix"
                   :insert ((owner_id . 77) (heading_id . 7) (payload . "x")))))
    (dolist (images (list (list (append dorg-test-d0hyz--scoped-old (list parent77))
                                (append dorg-test-d0hyz--scoped-new (list child77)))
                          (list (append dorg-test-d0hyz--scoped-old (list child77))
                                (append dorg-test-d0hyz--scoped-new (list parent77)))))
      (let ((result (dorg-test-d0hyz--scope-refusal (nth 0 images) (nth 1 images))))
        (should (eq 'producer-scope-violation
                    (plist-get (plist-get result :data) :reason)))
        (should-not (cl-some (lambda (sql) (string-match-p "\\`SELECT \\*" sql))
                             (plist-get result :queries)))))
    ;; Positive control: an image that projects its own parent 77 plans.
    (let ((dorg--extensions nil))
      (dorg-test-d0hyz--register-scoped
       (append dorg-test-d0hyz--scoped-old (list parent77 child77))
       (append dorg-test-d0hyz--scoped-new (list parent77 child77)))
      (should (plist-get (dorg-test-d0hyz--plan-scoped
                          (list (list "d0p_parent_ix" '((root_id . 42) (owner_id . 10))
                                      '((root_id . 42) (owner_id . 77)))
                                (list "d0p_child_ix"
                                      '((owner_id . 10) (heading_id . 1) (payload . "old"))
                                      '((owner_id . 77) (heading_id . 7) (payload . "x")))))
                         :applicable)))))

(ert-deftest dorg-test-d0hyz-policy-planner-refuses-ambiguous-parent-scope ()
  "A projected parent key also stored under another root refuses the plan."
  (let ((result (dorg-test-d0hyz--scope-refusal
                 dorg-test-d0hyz--scoped-old dorg-test-d0hyz--scoped-new
                 '(((owner_id . 10))))))
    (should (eq 'scope-parent-ambiguous
                (plist-get (plist-get result :data) :reason)))))

(ert-deftest dorg-test-d0hyz-policy-planner-rows-compose-through-the-lowerer ()
  "Planner images, the delta and the typed lowerer agree on one row form.
Canonical image rows use string columns; the delta's effects carry symbol
columns accepted unchanged by the closed effect validator.  Duplicates, SQL
NULL, case and trailing bytes survive into exact counted SQL."
  (let* ((descriptors '((:name "policy_ix" :key-column "id" :scope-column "id")))
         (row (lambda (value) (list (cons 'id 1) (cons 'value value))))
         (old (dorg-ext-rebuild--policy-rows-by-table
               (list (cons "policy_ix" (funcall row "a"))
                     (cons "policy_ix" (funcall row "a"))
                     (cons "policy_ix" (funcall row nil)))
               descriptors))
         (new (dorg-ext-rebuild--policy-rows-by-table
               (list (cons "policy_ix" (funcall row "A"))
                     (cons "policy_ix" (funcall row "a ")))
               descriptors))
         (current old)
         (delta (dorg-ext-rebuild--policy-delta
                 "policy_ix" (cdr (assoc "policy_ix" old))
                 (cdr (assoc "policy_ix" new)) (cdr (assoc "policy_ix" current))))
         (sql (dorg-ext-rebuild--policy-delta-sql delta '("policy_ix"))))
    (should (plist-get delta :applicable))
    (dolist (effect (append (plist-get delta :deletes) (plist-get delta :inserts)))
      (should (cl-every (lambda (cell) (symbolp (car cell)))
                        (or (plist-get effect :delete) (plist-get effect :insert)))))
    (should (member "DELETE FROM policy_ix WHERE id <=> 1 AND BINARY value <=> BINARY 'a' LIMIT 2"
                    sql))
    (should (member "DELETE FROM policy_ix WHERE id <=> 1 AND value <=> NULL LIMIT 1" sql))
    (should (member "INSERT INTO policy_ix (id, value) VALUES (1, 'A')" sql))
    (should (member "INSERT INTO policy_ix (id, value) VALUES (1, 'a ')" sql))
    (should (= 4 (length sql)))))

(ert-deftest dorg-test-d0hyz-policy-plan-effects-are-validated-symbol-rows ()
  "The public plan's effects pass the unchanged closed effect validator."
  (let ((dorg--extensions nil))
    (dorg-test-d0hyz--register-scoped dorg-test-d0hyz--scoped-old
                                      dorg-test-d0hyz--scoped-new)
    (let* ((plan (dorg-test-d0hyz--plan-scoped dorg-test-d0hyz--scoped-current))
           (owned '("d0p_parent_ix" "d0p_child_ix")))
      (should (= 2 (length (plist-get plan :effects))))
      (dolist (effect (plist-get plan :effects))
        (let ((cells (or (plist-get effect :delete) (plist-get effect :insert))))
          (should (cl-every (lambda (cell) (symbolp (car cell))) cells))
          (should (equal "d0p_child_ix"
                         (dorg-ext-validate-index-effect
                          (list :table (plist-get effect :table)
                                (if (plist-member effect :delete) :delete :insert)
                                cells)
                          owned))))))))

(ert-deftest dorg-test-d0hyz-policy-image-digest-is-a-canonical-multiset ()
  "Image digests depend on the complete-row multiset only.
Row and table order do not matter; one duplicate, one byte or NULL versus
empty content does; surrounding print controls neither change nor collapse it."
  (let* ((r1 '(("id" . 1) ("value" . "a")))
         (r2 '(("id" . 2) ("value" . nil)))
         (r3 '(("id" . 3) ("value" . "")))
         (image (list (cons "t1" (list r1 r2 r1)) (cons "t2" (list r3))))
         (digest (dorg-ext-rebuild--policy-image-digest image)))
    (should (equal digest (dorg-ext-rebuild--policy-image-digest
                           (list (cons "t2" (list r3)) (cons "t1" (list r1 r1 r2))))))
    (should-not (equal digest (dorg-ext-rebuild--policy-image-digest
                               (list (cons "t1" (list r1 r2)) (cons "t2" (list r3))))))
    (should-not (equal digest (dorg-ext-rebuild--policy-image-digest
                               (list (cons "t1" (list '(("id" . 1) ("value" . "b")) r2 r1))
                                     (cons "t2" (list r3))))))
    (should-not (equal digest (dorg-ext-rebuild--policy-image-digest
                               (list (cons "t1" (list r1 r2 r1))
                                     (cons "t2" (list '(("id" . 3) ("value" . nil))))))))
    (let ((print-level 1) (print-length 1)
          (print-escape-newlines t) (print-escape-nonascii t))
      (should (equal digest (dorg-ext-rebuild--policy-image-digest image)))
      (should-not (equal (dorg-ext-rebuild--policy-image-digest
                          (list (cons "t1" (list r1))))
                         (dorg-ext-rebuild--policy-image-digest
                          (list (cons "t1" (list r2)))))))))

(ert-deftest dorg-test-d0hyz-policy-table-reports-are-bounded-and-complete ()
  "Public per-table reports carry complete counts but bounded, order-stable
examples; occurrences beyond the cap still count, write and digest."
  (let* ((cap dorg-ext-rebuild--policy-example-cap)
         (descriptors '((:name "policy_ix" :scope-column "id")))
         (rows (lambda (n value)
                 (cl-loop for i from 1 to n
                          collect (dorg-ext-rebuild--canonical-row
                                   (list (cons 'id i) (cons 'value value))))))
         (old (list (cons "policy_ix" (append (funcall rows (+ cap 4) "old")
                                              (funcall rows 2 "dup") (funcall rows 2 "dup")))))
         (new (list (cons "policy_ix" (funcall rows (+ cap 4) "new"))))
         (forward (dorg-ext-rebuild--policy-deltas descriptors old new old))
         (backward (dorg-ext-rebuild--policy-deltas
                    descriptors
                    (mapcar (lambda (e) (cons (car e) (reverse (cdr e)))) old)
                    (mapcar (lambda (e) (cons (car e) (reverse (cdr e)))) new)
                    (mapcar (lambda (e) (cons (car e) (reverse (cdr e)))) old)))
         (table (car (plist-get forward :tables))))
    (should (plist-get forward :applicable))
    (should (= (+ cap 8) (alist-get 'upgrade (plist-get table :class-counts))))
    (should (= (+ cap 4) (alist-get 'missing-new (plist-get table :class-counts))))
    (should (= (+ cap 8) (plist-get table :delete-count)))
    (should (= (+ cap 4) (plist-get table :insert-count)))
    (should (= (+ cap 8 cap 4) (plist-get forward :write-count)))
    (should (equal '(:table :applicable :class-counts :delete-count :insert-count :examples)
                   (cl-loop for (key _v) on table by #'cddr collect key)))
    (dolist (entry (plist-get table :examples))
      (should (<= (length (cdr entry)) cap)))
    (should (= cap (length (alist-get 'upgrade (plist-get table :examples)))))
    (should (equal (plist-get forward :tables) (plist-get backward :tables)))
    (should (equal (plist-get forward :effects) (plist-get backward :effects)))
    ;; Duplicates beyond the cap are still deleted by exact count.
    (should (member '(:table "policy_ix" :delete ((id . 1) (value . "dup")) :count 2)
                    (plist-get forward :effects)))))

(ert-deftest dorg-test-d0hyz-policy-rejects-unsupported-current-rows ()
  "Only lossless complete rows are classified.  A duplicate, unsafe or
non-symbol column, an improper row, or a lossy value type (a float or any
non-integer/non-string value) is unsupported: the whole transition is
inapplicable, the row is counted and shown as unexplained, with no effects."
  (should (dorg-ext-rebuild--policy-row-supported-p '((id . 1) (v . "x") (n))))
  (dolist (row '(((id . 1) (id . 2)) ((id . 1.5)) (("id" . 1)) ((id . [1]))
                 ((id . 1) . tail) ((|bad col| . 1)) nil))
    (should-not (dorg-ext-rebuild--policy-row-supported-p row))
    (should (eq :unsupported (car (dorg-ext-rebuild--policy-image-row row)))))
  (let* ((descriptors '((:name "policy_ix" :scope-column "id")))
         (row (dorg-ext-rebuild--canonical-row '((id . 1) (value . "same"))))
         (lossy (dorg-ext-rebuild--policy-image-row '((id . 2) (value . 2.5))))
         (report (dorg-ext-rebuild--policy-deltas
                  descriptors (list (list "policy_ix" row)) (list (list "policy_ix" row))
                  (list (list "policy_ix" row lossy))))
         (table (car (plist-get report :tables))))
    (should-not (plist-get report :applicable))
    (should-not (plist-get report :effects))
    (should (= 0 (plist-get report :write-count)))
    (should-not (plist-get table :applicable))
    (should (= 1 (alist-get 'unexplained (plist-get table :class-counts))))
    (should (equal '((:row ((id . 2) (value . 2.5)) :reason unsupported-row))
                   (alist-get 'unexplained (plist-get table :examples))))))

(ert-deftest dorg-test-d0hyz-policy-classification-is-input-order-free ()
  "Class records, counts and effects do not depend on input row order."
  (let* ((rows (mapcar #'dorg-ext-rebuild--canonical-row
                       '(((id . 1) (value . "o")) ((id . 2) (value . "o"))
                         ((id . 2) (value . "o")) ((id . 3) (value . nil)))))
         (new (mapcar #'dorg-ext-rebuild--canonical-row
                      '(((id . 1) (value . "n")) ((id . 3) (value . nil)))))
         (forward (dorg-ext-rebuild--policy-delta "policy_ix" rows new rows))
         (backward (dorg-ext-rebuild--policy-delta
                    "policy_ix" (reverse rows) (reverse new) (reverse rows))))
    (should (plist-get forward :applicable))
    (should (equal (plist-get forward :classes) (plist-get backward :classes)))
    (should (equal (plist-get forward :deletes) (plist-get backward :deletes)))
    (should (equal (plist-get forward :inserts) (plist-get backward :inserts)))))

(ert-deftest dorg-test-d0hyz-policy-plan-digest-ignores-read-and-emit-order ()
  "Plan and current digests are identical for permuted producer and SELECT rows."
  (let* ((child (lambda (heading payload)
                  (list :table "d0p_child_ix"
                        :insert (list (cons 'owner_id 10) (cons 'heading_id heading)
                                      (cons 'payload payload)))))
         (parent '(:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 10))))
         (old (list parent (funcall child 1 "o1") (funcall child 2 "o2")
                    (funcall child 2 "o2")))
         (new (list parent (funcall child 1 "n1") (funcall child 2 "n2")))
         (stored (lambda (rows)
                   (list (list "d0p_parent_ix" '((root_id . 42) (owner_id . 10)))
                         (cons "d0p_child_ix" rows))))
         (c1 '((owner_id . 10) (heading_id . 1) (payload . "o1")))
         (c2 '((owner_id . 10) (heading_id . 2) (payload . "o2")))
         plans)
    (dolist (order (list (list old new (list c1 c2 c2))
                         (list (reverse old) (reverse new) (list c2 c1 c2))))
      (let ((dorg--extensions nil))
        (dorg-test-d0hyz--register-scoped (nth 0 order) (nth 1 order))
        (push (dorg-test-d0hyz--plan-scoped (funcall stored (nth 2 order))) plans)))
    (should (plist-get (car plans) :applicable))
    (should (equal (plist-get (nth 0 plans) :current-digest)
                   (plist-get (nth 1 plans) :current-digest)))
    (should (equal (plist-get (nth 0 plans) :plan-digest)
                   (plist-get (nth 1 plans) :plan-digest)))
    (should (equal (plist-get (nth 0 plans) :effects)
                   (plist-get (nth 1 plans) :effects)))
    (should (equal (plist-get (nth 0 plans) :tables)
                   (plist-get (nth 1 plans) :tables)))))

;;; ─── Policy upgrade apply / inverse over a fake versioned store ─────────
;; A small in-memory Dolt stand-in: revisions map table names to row lists,
;; reads honour `AS OF', and the transaction stub applies exactly the counted
;; DELETE and INSERT shapes the policy lowerer generates, runs the real
;; validator over the would-be working set, and only then advances HEAD.  It
;; can lose its reply in the three ways reconciliation must handle.  Every
;; statement and transaction call is recorded.  Native Dolt controls remain
;; a separate requirement.

(defun dorg-test-d0hyz--store (tables)
  "Return a fake store whose only revision H holds TABLES."
  (let ((store (make-hash-table :test 'eq))
        (revs (make-hash-table :test 'equal)))
    (puthash "H" (copy-tree tables) revs)
    (puthash :revs revs store)
    (puthash :head "H" store)
    (puthash :parents (make-hash-table :test 'equal) store)
    (puthash :changed (make-hash-table :test 'equal) store)
    (puthash :dirty nil store)
    (puthash :queries nil store)
    (puthash :tx-calls 0 store)
    (puthash :tx-mode 'normal store)
    (puthash :next 1 store)
    store))

(defun dorg-test-d0hyz--store-tables (store &optional rev)
  "Return STORE's tables at REV (default HEAD)."
  (gethash (or rev (gethash :head store)) (gethash :revs store)))

(defun dorg-test-d0hyz--store-commit (store tables &optional changed)
  "Commit TABLES as a child of STORE's HEAD; CHANGED overrides the diff list.
A `:forced-schema-change' list marks those tables' schema as changed too."
  (let* ((name (format "F%d" (gethash :next store)))
         (base (gethash :head store))
         (old (dorg-test-d0hyz--store-tables store base)))
    (puthash :next (1+ (gethash :next store)) store)
    (puthash name tables (gethash :revs store))
    (puthash name base (gethash :parents store))
    (puthash name
             (or changed
                 (delete-dups
                  (cl-remove-if
                   (lambda (table)
                     (equal (cdr (assoc table old)) (cdr (assoc table tables))))
                   (mapcar #'car (append old tables)))))
             (gethash :changed store))
    (when-let ((schema (gethash :forced-schema-change store)))
      (unless (gethash :schema-changed store)
        (puthash :schema-changed (make-hash-table :test 'equal) store))
      (puthash name schema (gethash :schema-changed store)))
    (puthash :head name store)
    name))

(defun dorg-test-d0hyz--sql-value (text)
  "Parse one generated SQL literal TEXT."
  (cond ((string= text "NULL") nil)
        ((string-match "\\`BINARY '\\(.*\\)'\\'" text) (match-string 1 text))
        ((string-match "\\`'\\(.*\\)'\\'" text) (match-string 1 text))
        ((string-match-p "\\`-?[0-9]+\\'" text) (string-to-number text))
        (t (error "fake store cannot parse literal %S" text))))

(defun dorg-test-d0hyz--split (text separator)
  "Split TEXT on SEPARATOR outside single quotes."
  (let ((parts nil) (start 0) (quoted nil) (i 0) (len (length separator)))
    (while (< i (length text))
      (cond ((eq (aref text i) ?') (setq quoted (not quoted)) (cl-incf i))
            ((and (not quoted) (string-prefix-p separator (substring text i)))
             (push (substring text start i) parts)
             (setq i (+ i len) start i))
            (t (cl-incf i))))
    (nreverse (cons (substring text start) parts))))

(defun dorg-test-d0hyz--row-matches (row conjuncts)
  "Return non-nil when ROW satisfies parsed WHERE CONJUNCTS."
  (cl-every
   (lambda (conjunct)
     (pcase-let ((`(,op ,column ,value) conjunct))
       (let ((cell (assq column row)))
         (pcase op
           ('eq (and cell (equal (cdr cell) value)))
           ('ne (and cell (not (equal (cdr cell) value))))
           ('in (and cell (member (cdr cell) value)))
           ('nulleq (and cell (equal (cdr cell) value)))))))
   conjuncts))

(defun dorg-test-d0hyz--parse-where (where)
  "Parse generated WHERE text into conjuncts."
  (and where
       (mapcar
        (lambda (part)
          (setq part (string-trim part))
          (cond
           ((string-match "\\`\\(?:BINARY \\)?\\([a-z_]+\\) <=> \\(.*\\)\\'" part)
            (list 'nulleq (intern (match-string 1 part))
                  (dorg-test-d0hyz--sql-value (match-string 2 part))))
           ((string-match "\\`\\([a-z_]+\\) IN (\\(.*\\))\\'" part)
            (list 'in (intern (match-string 1 part))
                  (mapcar (lambda (v) (dorg-test-d0hyz--sql-value (string-trim v)))
                          (dorg-test-d0hyz--split (match-string 2 part) ","))))
           ((string-match "\\`\\([a-z_]+\\) != \\(.*\\)\\'" part)
            (list 'ne (intern (match-string 1 part))
                  (dorg-test-d0hyz--sql-value (match-string 2 part))))
           ((string-match "\\`\\([a-z_]+\\) = \\(.*\\)\\'" part)
            (list 'eq (intern (match-string 1 part))
                  (dorg-test-d0hyz--sql-value (match-string 2 part))))
           (t (error "fake store cannot parse predicate %S" part))))
        (dorg-test-d0hyz--split where " AND "))))

(defun dorg-test-d0hyz--select (tables sql)
  "Answer generated SELECT SQL against TABLES, or nil when not a table read."
  (when (string-match
         (concat "\\`SELECT \\(\\*\\|[a-z_]+\\) FROM \\([a-z_0-9]+\\)"
                 "\\(?: AS OF '[^']+'\\)?\\(?: WHERE \\(.*\\)\\)?\\'")
         sql)
    (let ((column (match-string 1 sql))
          (rows (cdr (assoc (match-string 2 sql) tables)))
          (conjuncts (dorg-test-d0hyz--parse-where (match-string 3 sql))))
      (mapcar (lambda (row)
                (if (string= column "*") (copy-tree row)
                  (list (assq (intern column) row))))
              (cl-remove-if-not
               (lambda (row) (dorg-test-d0hyz--row-matches row conjuncts))
               rows)))))

(defun dorg-test-d0hyz--apply-statement (tables sql)
  "Apply one generated policy DELETE/INSERT SQL to TABLES; return new TABLES."
  (cond
   ((string-match "\\`DELETE FROM \\([a-z_0-9]+\\) WHERE \\(.*\\) LIMIT \\([0-9]+\\)\\'" sql)
    (let* ((table (match-string 1 sql))
           (where (match-string 2 sql))
           (limit (string-to-number (match-string 3 sql)))
           (conjuncts (dorg-test-d0hyz--parse-where where))
           (kept nil))
      (dolist (row (cdr (assoc table tables)))
        (if (and (> limit 0) (dorg-test-d0hyz--row-matches row conjuncts)
                 (= (length row) (length conjuncts)))
            (cl-decf limit)
          (push row kept)))
      (cons (cons table (nreverse kept))
            (cl-remove table tables :key #'car :test #'string=))))
   ((string-match "\\`INSERT INTO \\([a-z_0-9]+\\) (\\(.*\\)) VALUES (\\(.*\\))\\'" sql)
    (let* ((table (match-string 1 sql))
           (column-text (match-string 2 sql))
           (value-text (match-string 3 sql))
           (columns (mapcar (lambda (c) (intern (string-trim c)))
                            (split-string column-text ",")))
           (values (mapcar (lambda (v) (dorg-test-d0hyz--sql-value (string-trim v)))
                           (dorg-test-d0hyz--split value-text ",")))
           (row (cl-mapcar #'cons columns values)))
      (cons (cons table (append (cdr (assoc table tables)) (list row)))
            (cl-remove table tables :key #'car :test #'string=))))
   (t (error "fake store cannot apply %S" sql))))

(defun dorg-test-d0hyz--fake-sql (store dir sql)
  "Answer read-only SQL against STORE when DIR is its store, else as empty.
A one-shot `:on-derived-read' hook runs before the first derived-table read."
  (push sql (gethash :queries store))
  (when-let ((hook (and (string-match-p "FROM d0p_" sql) (gethash :on-derived-read store))))
    (puthash :on-derived-read nil store)
    (funcall hook))
  (let ((head (gethash :head store)))
    (cond
     ((not (equal (file-truename (file-name-as-directory dir)) (gethash :dir store)))
      (and (string-match-p "\\`SELECT HASHOF('HEAD')" sql) '(((hash . "X0")))))
     ((string-match-p "\\`SELECT HASHOF('HEAD')" sql) (list (list (cons 'hash head))))
     ((string-match "FROM dolt_log WHERE commit_hash = '\\([^']+\\)'" sql)
      (and (gethash (match-string 1 sql) (gethash :revs store)) '(((a . 1)))))
     ((string-match-p "FROM dolt_status" sql)
      (and (gethash :dirty store) '(((table_name . "dirty")))))
     ((string-match "FROM dolt_commit_ancestors WHERE commit_hash = '\\([^']+\\)'" sql)
      (let ((parent (gethash (match-string 1 sql) (gethash :parents store))))
        (and parent (list (list (cons 'parent_hash parent))))))
     ((string-match "FROM dolt_diff WHERE commit_hash = '\\([^']+\\)'" sql)
      (let ((commit (match-string 1 sql)))
        (mapcar (lambda (table)
                  (list (cons 'table_name table)
                        (cons 'schema_change
                              (if (member table (gethash commit (gethash :schema-changed store
                                                                         (make-hash-table))))
                                  1 0))))
                (gethash commit (gethash :changed store)))))
     ((string-match "FROM headings AS OF '\\([^']+\\)' WHERE id = \\([0-9]+\\)" sql)
      (and (gethash (match-string 1 sql) (gethash :revs store))
           (list (list (cons 'id (string-to-number (match-string 2 sql)))))))
     (t
      (let ((rev (and (string-match " AS OF '\\([^']+\\)'" sql) (match-string 1 sql))))
        (unless (string-match-p "\\`SELECT \\(\\*\\|[a-z_]+\\) FROM [a-z_0-9]+" sql)
          (error "fake store cannot answer %S" sql))
        (dorg-test-d0hyz--select
         (or (dorg-test-d0hyz--store-tables store (or rev head))
             (error "fake store has no revision %S" rev))
         sql))))))

(defun dorg-test-d0hyz--fake-transaction (store dir base statements message validate)
  "Transaction stub over STORE honouring the real stale/dirty/validate order."
  (ignore dir message)
  (puthash :tx-calls (1+ (gethash :tx-calls store)) store)
  (push statements (gethash :tx-statements store))
  (when (gethash :dirty store)
    (signal 'dorg-transaction-dirty (list "dirty")))
  (unless (equal base (gethash :head store))
    (signal 'dorg-transaction-stale
            (list "stale" :base base :head (gethash :head store))))
  (let* ((mode (gethash :tx-mode store))
         (tables (copy-tree (dorg-test-d0hyz--store-tables store base)))
         (applied (if (eq mode 'drop-last-statement) (butlast statements) statements)))
    (dolist (sql applied)
      (setq tables (dorg-test-d0hyz--apply-statement tables sql)))
    (when validate
      (funcall validate (lambda (sql) (dorg-test-d0hyz--select tables sql))))
    (pcase mode
      ('lose-uncommitted
       (signal 'dorg-transaction-indeterminate
               (list "lost reply" :base base :commit nil :underlying "fake")))
      ('lose-ambiguous
       ;; HEAD moved to a commit that is not this transition (it rewrites
       ;; primary data instead), and no commit was reported: unprovable.
       (dorg-test-d0hyz--store-commit
        store (cons '("headings" ((id . 42) (title . "foreign")))
                    (cl-remove "headings" (copy-tree (dorg-test-d0hyz--store-tables store base))
                               :key #'car :test #'string=)))
       (signal 'dorg-transaction-indeterminate
               (list "lost reply" :base base :commit nil :underlying "fake")))
      (_
       (let ((new (dorg-test-d0hyz--store-commit
                   store tables (gethash :forced-changed store))))
         (if (eq mode 'lose-committed)
             (signal 'dorg-transaction-indeterminate
                     (list "lost reply" :base base :commit new :underlying "fake"))
           new))))))

(defmacro dorg-test-d0hyz--with-store (store-var tables &rest body)
  "Bind STORE-VAR to a fake store holding TABLES; route reads/commits to it."
  (declare (indent 2))
  `(let ((,store-var (dorg-test-d0hyz--store ,tables)))
     (cl-letf (((symbol-function 'dorg-sql)
                (lambda (dir sql) (dorg-test-d0hyz--fake-sql ,store-var dir sql)))
               ((symbol-function 'dorg-ext-rebuild--full-context)
                (lambda (&rest _) 'ctx))
               ((symbol-function 'dorg-transaction-commit)
                (lambda (dir base statements message &optional validate)
                  (dorg-test-d0hyz--fake-transaction
                   ,store-var dir base statements message validate)))
               ((symbol-function 'dorg-mutate-call-with-store-gate)
                (lambda (dir fn &rest _)
                  (puthash :gate-calls (1+ (gethash :gate-calls ,store-var 0)) ,store-var)
                  (funcall fn (list :dir dir) 'key)))
               ((symbol-function 'dorg-sql--lease-scalar)
                (lambda (lease sql &rest _)
                  (cdr (car (car (dorg-test-d0hyz--fake-sql
                                  ,store-var (plist-get lease :dir) sql))))))
               ((symbol-function 'dorg-sql--lease-exec)
                (lambda (lease sql &rest _)
                  (dorg-test-d0hyz--fake-sql ,store-var (plist-get lease :dir) sql))))
       ,@body)))

;; Fixture: root 42 holds parent 10 with children; root 7 is an unrelated root
;; in the same derived tables; headings stands for primary data.

(defconst dorg-test-d0hyz--fx-old
  '((:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 10)))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 1) (payload . "same")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 2) (payload . "old")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 2) (payload . "old")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 3) (payload . nil)))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 5) (payload . "removed"))))
  "OLD producer image: unchanged, a duplicated upgrade, NULL, and an OLD-only row.")

(defconst dorg-test-d0hyz--fx-new
  '((:table "d0p_parent_ix" :insert ((root_id . 42) (owner_id . 10)))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 1) (payload . "same")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 2) (payload . "new")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 3) (payload . "")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 4) (payload . "Case ")))
    (:table "d0p_child_ix" :insert ((owner_id . 10) (heading_id . 6) (payload . "already"))))
  "NEW producer image: replacements, NULL to empty, missing-new, already-new.")

(defconst dorg-test-d0hyz--fx-tables
  '(("headings" ((id . 42) (title . "root")) ((id . 7) (title . "other")))
    ("d0p_parent_ix" ((root_id . 42) (owner_id . 10)) ((root_id . 7) (owner_id . 70)))
    ("d0p_child_ix"
     ((owner_id . 10) (heading_id . 1) (payload . "same"))
     ((owner_id . 10) (heading_id . 2) (payload . "old"))
     ((owner_id . 10) (heading_id . 2) (payload . "old"))
     ((owner_id . 10) (heading_id . 3) (payload . nil))
     ((owner_id . 10) (heading_id . 6) (payload . "already"))
     ((owner_id . 70) (heading_id . 9) (payload . "unrelated"))))
  "Stored C at H: mixed OLD/NEW rows; OLD-only row 5 removed, row 4 missing.")

(defun dorg-test-d0hyz--fx-request (dir)
  "Return the fixture planning request for store DIR."
  (list :dolt-dir dir :revision "H" :identity 'dorg :scope '(:root-id 42)
        :old-producer (dorg-test-d0hyz--producer-id "old")
        :new-producer (dorg-test-d0hyz--producer-id "new")))

(defmacro dorg-test-d0hyz--with-fixture-at (dir store-var tables drift &rest body)
  "Run BODY with the fixture extension (resolver DRIFT cell) and a fake STORE
holding TABLES for the existing store directory DIR."
  (declare (indent 4))
  `(let ((dorg--extensions nil))
     (dorg-test-d0hyz--register-scoped dorg-test-d0hyz--fx-old
                                       dorg-test-d0hyz--fx-new ,drift)
     (dorg-test-d0hyz--with-store ,store-var ,tables
       (puthash :dir (file-truename (file-name-as-directory ,dir)) ,store-var)
       ,@body)))

(defmacro dorg-test-d0hyz--with-fixture (dir-var store-var tables &rest body)
  "Run BODY with a registered fixture extension, a fresh store dir and fake STORE."
  (declare (indent 3))
  `(let ((,dir-var (make-temp-file "dorg-test-d0hyz-store-" t)))
     (unwind-protect
         (dorg-test-d0hyz--with-fixture-at ,dir-var ,store-var ,tables nil ,@body)
       (delete-directory ,dir-var t))))

(defun dorg-test-d0hyz--unscoped (tables)
  "Return every row outside root 42's derived scope, including primary data."
  (list (cdr (assoc "headings" tables))
        (cl-remove-if (lambda (row) (equal (alist-get 'root_id row) 42))
                      (cdr (assoc "d0p_parent_ix" tables)))
        (cl-remove-if (lambda (row) (equal (alist-get 'owner_id row) 10))
                      (cdr (assoc "d0p_child_ix" tables)))))

(defun dorg-test-d0hyz--images-of (rows)
  "Return the canonical, sorted image of complete symbol ROWS."
  (sort (mapcar (lambda (row) (prin1-to-string (dorg-ext-rebuild--canonical-row row)))
                rows)
        #'string<))

(ert-deftest dorg-test-d0hyz-apply-forward-reaches-new-and-preserves-the-rest ()
  "Forward writes exactly C->N for the mixed fixture in one transaction.
Primary data and the unrelated root stay identical; the receipt binds source,
extension, scope, tables, both producers and the C-at-H preimage."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                  'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
           (before (dorg-test-d0hyz--store-tables store))
           (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
           (after (dorg-test-d0hyz--store-tables store)))
      (should (equal '((unchanged . 2) (upgrade . 3) (already-new . 2)
                       (missing-new . 3) (extra . 0) (unexplained . 0))
                     (plist-get plan :class-counts)))
      ;; The count unit is complete-row occurrences (delete 2 old + NULL,
      ;; insert new + empty + Case = 6), not statements or effects (5).
      (should (= 6 (plist-get plan :write-count)))
      (should (= 5 (length (plist-get plan :effects))))
      (should (= 1 (gethash :tx-calls store)))
      (should (= 5 (length (car (gethash :tx-statements store)))))
      (should (= 6 (plist-get receipt :write-count)))
      (should (equal "F1" (plist-get receipt :post-head)))
      (should (equal '(:ok :kind :source :extension :base :post-head :scope :tables
                       :identity :old-producer :new-producer :preimage-digest
                       :postimage-digest :plan-digest :write-count)
                     (cl-loop for (key _v) on receipt by #'cddr collect key)))
      (should (eq 'policy-upgrade-forward (plist-get receipt :kind)))
      (should (equal (file-truename (file-name-as-directory dir))
                     (plist-get receipt :source)))
      (should (equal (plist-get plan :current-digest) (plist-get receipt :preimage-digest)))
      (should (equal (plist-get plan :plan-digest) (plist-get receipt :plan-digest)))
      (should (equal (dorg-test-d0hyz--images-of
                      (mapcar (lambda (e) (plist-get e :insert)) dorg-test-d0hyz--fx-new))
                     (sort (apply #'append
                                  (mapcar #'cdr (dorg-test-d0hyz--scoped-canonical after)))
                           #'string<)))
      (should (equal (dorg-test-d0hyz--unscoped before) (dorg-test-d0hyz--unscoped after))))))

(defun dorg-test-d0hyz--scoped-canonical (tables)
  "Return root 42's derived rows from TABLES as canonical printed images."
  (mapcar (lambda (table)
            (cons table
                  (dorg-test-d0hyz--images-of
                   (cl-remove-if-not
                    (lambda (row)
                      (or (equal (alist-get 'root_id row) 42)
                          (equal (alist-get 'owner_id row) 10)))
                    (cdr (assoc table tables))))))
          '("d0p_parent_ix" "d0p_child_ix")))

(ert-deftest dorg-test-d0hyz-apply-then-invert-restores-the-exact-preimage ()
  "Inverse restores C at H exactly: multiplicity, the retained already-new row,
the removed missing-new insert and NULL, and nothing outside the scope."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let* ((before (dorg-test-d0hyz--store-tables store))
           (plan (dorg-ext-rebuild-plan-policy-upgrade
                  'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
           (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
           (inverse (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt))
           (restored (dorg-test-d0hyz--store-tables store)))
      (should (= 2 (gethash :tx-calls store)))
      (should (eq 'policy-upgrade-inverse (plist-get inverse :kind)))
      (should (equal "F1" (plist-get inverse :base)))
      (should (equal "F2" (plist-get inverse :new-head)))
      (should (equal receipt (plist-get inverse :forward-receipt)))
      (should (equal (plist-get receipt :preimage-digest)
                     (plist-get inverse :restored-digest)))
      (should (equal (dorg-test-d0hyz--scoped-canonical before)
                     (dorg-test-d0hyz--scoped-canonical restored)))
      (should (equal (dorg-test-d0hyz--unscoped before)
                     (dorg-test-d0hyz--unscoped restored))))))

(ert-deftest dorg-test-d0hyz-already-new-forward-and-inverse-open-no-transaction ()
  "An all-NEW scope is a proven no-op: zero transaction calls, HEAD unchanged,
and inverting its explicit no-op receipt also makes no call.  Both no-ops
check HEAD and the working set under the store gate: dirty or stale refuses,
still with zero transaction calls."
  (let ((tables (list (car dorg-test-d0hyz--fx-tables)
                      (list "d0p_parent_ix" '((root_id . 42) (owner_id . 10)))
                      (cons "d0p_child_ix"
                            (mapcar (lambda (e) (plist-get e :insert))
                                    (cdr dorg-test-d0hyz--fx-new))))))
    (dorg-test-d0hyz--with-fixture dir store tables
      (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                    'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
             (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
             (inverse (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt)))
        (should (= 0 (plist-get plan :write-count)))
        (should (plist-get receipt :no-op))
        (should (equal "H" (plist-get receipt :post-head)))
        (should (plist-get inverse :no-op))
        (should (equal "H" (plist-get inverse :new-head)))
        (should (= 0 (gethash :tx-calls store)))
        (should (= 2 (gethash :gate-calls store)))
        (should (equal "H" (gethash :head store)))
        (puthash :dirty t store)
        (dorg-test-d0hyz--refuses
         'dirty-working-set
         (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)) store)
        (dorg-test-d0hyz--refuses
         'dirty-working-set
         (lambda () (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt)) store)
        (puthash :dirty nil store)
        (dorg-test-d0hyz--store-commit store (dorg-test-d0hyz--store-tables store))
        (dorg-test-d0hyz--refuses
         'stale-head
         (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)) store)
        (dorg-test-d0hyz--refuses
         'stale-head
         (lambda () (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt)) store)))))

(ert-deftest dorg-test-d0hyz-planner-reads-only-as-of-h ()
  "Planning is read-only and frozen: every derived or heading read is AS OF H
and no transaction opens.  An interloper commit landing DURING one planner
call, after its first derived read, cannot change that call's current digest
or base, although HEAD has moved."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let* ((baseline (dorg-ext-rebuild-plan-policy-upgrade
                      'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
           (reads (cl-remove-if-not
                   (lambda (sql) (string-match-p "FROM \\(headings\\|d0p_\\)" sql))
                   (gethash :queries store))))
      (should reads)
      (should (cl-every (lambda (sql) (string-match-p " AS OF 'H'" sql)) reads))
      (should (equal "H" (gethash :head store)))
      (should (= 0 (gethash :tx-calls store)))
      (puthash :on-derived-read
               (lambda ()
                 (dorg-test-d0hyz--store-commit
                  store
                  (cons '("d0p_child_ix" ((owner_id . 10) (heading_id . 99) (payload . "x")))
                        (cl-remove "d0p_child_ix" (dorg-test-d0hyz--store-tables store)
                                   :key #'car :test #'string=))))
               store)
      (let ((during (dorg-ext-rebuild-plan-policy-upgrade
                     'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
        (should (equal "F1" (gethash :head store)))
        (should-not (gethash :on-derived-read store))
        (should (equal "H" (plist-get during :base)))
        (should (equal (plist-get baseline :current-digest)
                       (plist-get during :current-digest)))
        (should (equal baseline during))))))

(defun dorg-test-d0hyz--refuses (reason thunk store)
  "Assert THUNK signals a rebuild error with REASON and STORE saw no transaction."
  (let ((err (should-error (funcall thunk) :type 'dorg-ext-rebuild-error)))
    (should (eq reason (plist-get (dorg-error-data err) :reason)))
    (should (= 0 (gethash :tx-calls store)))))

(ert-deftest dorg-test-d0hyz-apply-refuses-tampered-or-stale-plans-before-writing ()
  "Apply re-derives the plan: dropped or added effects with a recomputed
digest, a source that is not the plan's own normalized identity, a stale HEAD
or a dirty working set refuse before the transaction."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                  'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
           (redigest
            (lambda (p)
              (plist-put (copy-tree p) :plan-digest
                         (dorg-ext-rebuild--policy-digest
                          (list :source (plist-get p :source)
                                :extension (plist-get p :extension)
                                :base (plist-get p :base) :scope (plist-get p :scope)
                                :identity (plist-get p :identity)
                                :old-producer (plist-get p :old-producer)
                                :new-producer (plist-get p :new-producer)
                                :current-digest (plist-get p :current-digest)
                                :tables (plist-get p :tables)
                                :effects (plist-get p :effects)))))))
      (should (equal (file-truename (file-name-as-directory dir)) (plist-get plan :source)))
      (dorg-test-d0hyz--refuses
       'plan-mismatch
       (lambda ()
         (dorg-ext-rebuild-apply-policy-upgrade
          'd0hyz-scoped
          (funcall redigest (plist-put (copy-tree plan) :effects
                                       (cdr (plist-get plan :effects))))))
       store)
      (dorg-test-d0hyz--refuses
       'plan-mismatch
       (lambda ()
         (dorg-ext-rebuild-apply-policy-upgrade
          'd0hyz-scoped
          (funcall redigest
                   (plist-put (copy-tree plan) :effects
                              (cons '(:table "d0p_child_ix"
                                      :insert ((owner_id . 10) (heading_id . 8)
                                               (payload . "added")))
                                    (plist-get plan :effects))))))
       store)
      ;; The same store named by a non-normalized path is not this plan's bound
      ;; source: the re-derived plan carries the normalized identity.
      (dorg-test-d0hyz--refuses
       'plan-mismatch
       (lambda ()
         (dorg-ext-rebuild-apply-policy-upgrade
          'd0hyz-scoped
          (funcall redigest (plist-put (copy-tree plan) :source
                                       (directory-file-name (plist-get plan :source))))))
       store)
      (puthash :dirty t store)
      (dorg-test-d0hyz--refuses
       'dirty-working-set
       (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)) store)
      (puthash :dirty nil store)
      (dorg-test-d0hyz--store-commit store (dorg-test-d0hyz--store-tables store))
      (dorg-test-d0hyz--refuses
       'stale-head
       (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)) store))))

(ert-deftest dorg-test-d0hyz-resolver-requests-bind-source-base-identity-scope ()
  "Every OLD and NEW resolution at plan, apply and inverse receives the
canonical source, base H, identity and scope with its requested producer."
  (let ((dir (make-temp-file "dorg-test-d0hyz-store-" t)) (requests nil))
    (unwind-protect
        (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables nil
          (let ((resolver (dorg-extension-producer-resolver (dorg-get-extension 'd0hyz-scoped))))
            (setf (dorg-extension-producer-resolver (dorg-get-extension 'd0hyz-scoped))
                  (lambda (request) (push request requests) (funcall resolver request))))
          (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                        'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
                 (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)))
            (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt))
          (should (= 6 (length requests)))
          (dolist (request requests)
            (should (equal '(:producer :source :revision :identity :scope)
                           (cl-loop for (key _v) on request by #'cddr collect key)))
            (should (equal (file-truename (file-name-as-directory dir))
                           (plist-get request :source)))
            (should (equal "H" (plist-get request :revision)))
            (should (eq 'dorg (plist-get request :identity)))
            (should (equal '(:root-id 42) (plist-get request :scope))))
          (should (= 3 (cl-count (dorg-test-d0hyz--producer-id "old") requests
                                 :key (lambda (r) (plist-get r :producer)) :test #'equal)))
          (should (= 3 (cl-count (dorg-test-d0hyz--producer-id "new") requests
                                 :key (lambda (r) (plist-get r :producer)) :test #'equal))))
      (delete-directory dir t))))

(ert-deftest dorg-test-d0hyz-apply-refuses-producer-drift-since-planning ()
  "Producers are resolved anew at apply.  A changed implementation with the
same projection, a changed dependency, or a changed policy input refuses
before the transaction."
  (dolist (override '((:implementation "old-rebuilt")
                      (:dependencies "deps-v2")
                      (:policy "(:policy v2)")))
    (let ((drift (list nil))
          (dir (make-temp-file "dorg-test-d0hyz-store-" t)))
      (unwind-protect
          (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables drift
            (let ((plan (dorg-ext-rebuild-plan-policy-upgrade
                         'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
              (setcar drift override)
              (ert-info ((format "%S" override) :prefix "drift: ")
                (dorg-test-d0hyz--refuses
                 'producer-identity-mismatch
                 (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
                 store))))
        (delete-directory dir t)))))

(ert-deftest dorg-test-d0hyz-apply-validation-failure-rolls-back ()
  "An incomplete write fails the in-transaction NEW postimage check: the
transaction rolls back, HEAD is unchanged and no receipt is returned."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let ((plan (dorg-ext-rebuild-plan-policy-upgrade
                 'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
      (puthash :tx-mode 'drop-last-statement store)
      (let ((err (should-error (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)
                               :type 'dorg-ext-rebuild-error)))
        (should (eq 'postimage-mismatch (plist-get (dorg-error-data err) :reason))))
      (should (= 1 (gethash :tx-calls store)))
      (should (equal "H" (gethash :head store))))))

(ert-deftest dorg-test-d0hyz-apply-reconciles-indeterminate-commits ()
  "A lost reply is reconciled only by a read-only proof: (a) a proven commit
yields a receipt field-identical to direct success on the same source and
inverts; (b) no commit stays indeterminate at H with a re-derived next plan;
(c) HEAD moved to a commit that is not this transition stays indeterminate
with no receipt and no further write."
  (let ((dir (make-temp-file "dorg-test-d0hyz-store-" t)) direct)
    (unwind-protect
        (progn
          (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables nil
            (setq direct (dorg-ext-rebuild-apply-policy-upgrade
                          'd0hyz-scoped (dorg-ext-rebuild-plan-policy-upgrade
                                         'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))))
          (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables nil
            (puthash :tx-mode 'lose-committed store)
            (let ((receipt (dorg-ext-rebuild-apply-policy-upgrade
                            'd0hyz-scoped (dorg-ext-rebuild-plan-policy-upgrade
                                           'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))))
              (should (equal direct receipt))
              (puthash :tx-mode 'normal store)
              (should (plist-get (dorg-ext-rebuild-invert-policy-upgrade
                                  'd0hyz-scoped receipt)
                                 :ok))))
          (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables nil
            (puthash :tx-mode 'lose-uncommitted store)
            (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                          'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
                   (err (should-error (dorg-ext-rebuild-apply-policy-upgrade
                                       'd0hyz-scoped plan)
                                      :type 'dorg-transaction-indeterminate))
                   (facts (cddr err)))
              (should (eq 'not-committed (plist-get facts :reconciled)))
              (should (equal "H" (plist-get facts :base)))
              (should (equal "H" (gethash :head store)))
              (should (equal plan (plist-get facts :next-plan)))
              (should (= 1 (gethash :tx-calls store)))))
          (dorg-test-d0hyz--with-fixture-at dir store dorg-test-d0hyz--fx-tables nil
            (puthash :tx-mode 'lose-ambiguous store)
            (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                          'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
                   (err (should-error (dorg-ext-rebuild-apply-policy-upgrade
                                       'd0hyz-scoped plan)
                                      :type 'dorg-transaction-indeterminate))
                   (facts (cddr err)))
              (should (eq 'ambiguous (plist-get facts :reconciled)))
              (should-not (plist-get facts :next-plan))
              (should (equal "F1" (gethash :head store)))
              (should (= 1 (gethash :tx-calls store)))
              (should (equal "foreign" (alist-get 'title
                                                  (car (cdr (assoc "headings"
                                                                   (dorg-test-d0hyz--store-tables
                                                                    store))))))))))
      (delete-directory dir t))))

(ert-deftest dorg-test-d0hyz-invert-refuses-drift-and-foreign-receipts ()
  "Inverse requires an intact receipt for this store, HEAD = F, a clean
working set, re-derived digests matching the receipt and derived rows at F
equal to its postimage; otherwise it refuses before writing."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (let* ((plan (dorg-ext-rebuild-plan-policy-upgrade
                  'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
           (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
           (calls (gethash :tx-calls store))
           (refuses
            (lambda (reason r)
              (let ((err (should-error
                          (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped r)
                          :type 'dorg-ext-rebuild-error)))
                (should (eq reason (plist-get (dorg-error-data err) :reason)))
                (should (= calls (gethash :tx-calls store)))))))
      (funcall refuses 'receipt-malformed (plist-put (copy-tree receipt) :scope nil))
      (funcall refuses 'receipt-malformed
               (cl-loop for (k v) on receipt by #'cddr
                        unless (eq k :plan-digest) append (list k v)))
      (funcall refuses 'receipt-mismatch
               (plist-put (copy-tree receipt) :postimage-digest (make-string 64 ?0)))
      (funcall refuses 'receipt-mismatch
               (plist-put (copy-tree receipt) :preimage-digest (make-string 64 ?0)))
      (funcall refuses 'receipt-mismatch
               (plist-put (copy-tree receipt) :new-producer
                          (dorg-test-d0hyz--producer-id "old")))
      (let ((other (make-temp-file "dorg-test-d0hyz-other-" t)))
        (unwind-protect
            (funcall refuses 'source-mismatch
                     (plist-put (copy-tree receipt) :source
                                (file-truename (file-name-as-directory other))))
          (delete-directory other t)))
      ;; HEAD = F and clean, but F's derived rows differ from the receipt.
      (let* ((revs (gethash :revs store))
             (saved (copy-tree (gethash "F1" revs))))
        (puthash "F1"
                 (cons (cons "d0p_child_ix"
                             (cons '((owner_id . 10) (heading_id . 1) (payload . "drift"))
                                   (cdr (cdr (assoc "d0p_child_ix" saved)))))
                       (cl-remove "d0p_child_ix" (copy-tree saved) :key #'car :test #'string=))
                 revs)
        (funcall refuses 'derived-drift receipt)
        (puthash "F1" saved revs))
      (puthash :dirty t store)
      (funcall refuses 'dirty-working-set receipt)
      (puthash :dirty nil store)
      (dorg-test-d0hyz--store-commit store (dorg-test-d0hyz--store-tables store))
      (funcall refuses 'stale-head receipt))))

(ert-deftest dorg-test-d0hyz-commit-proof-refuses-schema-change-in-derived-table ()
  "Identical rows are not enough: a commit that also changes an allowed
derived table's schema is not the authorized transition.  Inverse refuses it
with no write, and a lost reply on such a commit yields no receipt."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (puthash :forced-schema-change '("d0p_child_ix") store)
    (let* ((receipt (dorg-ext-rebuild-apply-policy-upgrade
                     'd0hyz-scoped (dorg-ext-rebuild-plan-policy-upgrade
                                    'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
           (err (should-error (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt)
                              :type 'dorg-ext-rebuild-error)))
      (should (eq 'non-derived-diff (plist-get (dorg-error-data err) :reason)))
      (should (= 1 (gethash :tx-calls store)))))
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (puthash :forced-schema-change '("d0p_child_ix") store)
    (puthash :tx-mode 'lose-committed store)
    (let ((err (should-error
                (dorg-ext-rebuild-apply-policy-upgrade
                 'd0hyz-scoped (dorg-ext-rebuild-plan-policy-upgrade
                                'd0hyz-scoped (dorg-test-d0hyz--fx-request dir)))
                :type 'dorg-transaction-indeterminate)))
      (should (eq 'ambiguous (plist-get (cddr err) :reconciled))))))

(ert-deftest dorg-test-d0hyz-invert-refuses-a-forward-diff-outside-derived-tables ()
  "A forward commit whose diff also touches a non-derived table is not the
authorized transition; inverse refuses before writing."
  (dorg-test-d0hyz--with-fixture dir store dorg-test-d0hyz--fx-tables
    (puthash :forced-changed '("d0p_child_ix" "headings") store)
    (let* ((receipt (dorg-ext-rebuild-apply-policy-upgrade
                     'd0hyz-scoped (dorg-ext-rebuild-plan-policy-upgrade
                                    'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
           (err (should-error (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-scoped receipt)
                              :type 'dorg-ext-rebuild-error)))
      (should (eq 'non-derived-diff (plist-get (dorg-error-data err) :reason)))
      (should (= 1 (gethash :tx-calls store))))))

(ert-deftest dorg-test-d0hyz-unsupported-current-row-makes-plan-inapplicable ()
  "A lossy stored row makes the public plan inapplicable; apply then refuses
before any transaction."
  (let ((tables (cons (list "d0p_child_ix"
                            '((owner_id . 10) (heading_id . 1) (payload . 1.5)))
                      (cl-remove "d0p_child_ix" (copy-tree dorg-test-d0hyz--fx-tables)
                                 :key #'car :test #'string=))))
    (dorg-test-d0hyz--with-fixture dir store tables
      (let ((plan (dorg-ext-rebuild-plan-policy-upgrade
                   'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
        (should-not (plist-get plan :applicable))
        (should-not (plist-get plan :effects))
        (should (= 1 (alist-get 'unexplained (plist-get plan :class-counts))))
        (dorg-test-d0hyz--refuses
         'plan-inapplicable
         (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan)) store)))))

(ert-deftest dorg-test-d0hyz-unsupported-producer-row-makes-plan-inapplicable ()
  "O, N and C share one complete-row law: a lossy value emitted by the OLD or
by the NEW producer makes the plan inapplicable with unexplained rows and no
effects, and apply refuses before any transaction."
  (let ((lossy '(:table "d0p_child_ix"
                 :insert ((owner_id . 10) (heading_id . 7) (payload . 1.5)))))
    (dolist (images (list (list (append dorg-test-d0hyz--fx-old (list lossy))
                                dorg-test-d0hyz--fx-new)
                          (list dorg-test-d0hyz--fx-old
                                (append dorg-test-d0hyz--fx-new (list lossy)))))
      (let ((dir (make-temp-file "dorg-test-d0hyz-store-" t)))
        (unwind-protect
            (let ((dorg--extensions nil))
              (dorg-test-d0hyz--register-scoped (nth 0 images) (nth 1 images))
              (dorg-test-d0hyz--with-store store dorg-test-d0hyz--fx-tables
                (puthash :dir (file-truename (file-name-as-directory dir)) store)
                (let ((plan (dorg-ext-rebuild-plan-policy-upgrade
                             'd0hyz-scoped (dorg-test-d0hyz--fx-request dir))))
                  (should-not (plist-get plan :applicable))
                  (should-not (plist-get plan :effects))
                  (should (= 0 (plist-get plan :write-count)))
                  (should (= 1 (alist-get 'unexplained (plist-get plan :class-counts))))
                  (dorg-test-d0hyz--refuses
                   'plan-inapplicable
                   (lambda () (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-scoped plan))
                   store))))
          (delete-directory dir t))))))

;;; ─── Native Dolt policy upgrade controls ───────────────────────────────
;; Owned temp store through the shared fixture: the real store gate,
;; dorg-transaction-commit, counted binary DELETE ... LIMIT, keyless
;; duplicates, NULL versus empty, and the system tables the read-only commit
;; proof relies on (dolt_commit_ancestors, dolt_diff).

(defun dorg-test-d0hyz--native-effects (root payloads)
  "Return root ROOT's parent row plus child rows with PAYLOADS (heading . value)."
  (cons (list :table "d0n_parent_ix" :insert (list (cons 'root_id root) (cons 'owner_id 10)))
        (mapcar (lambda (cell)
                  (list :table "d0n_child_ix"
                        :insert (list (cons 'owner_id 10) (cons 'heading_id (car cell))
                                      (cons 'payload (cdr cell)))))
                payloads)))

(defmacro dorg-test-d0hyz--with-native (dolt-dir root-var &rest body)
  "Install the native policy fixture in DOLT-DIR; bind ROOT-VAR; run BODY.
Root ROOT-VAR carries the upgrade scope; root 200 is an unrelated root with its
own derived rows in the same tables."
  (declare (indent 2))
  `(let ((dorg--extensions nil)
         (,root-var nil))
     (dorg-register-extension
      'd0hyz-native
      :schema (list "CREATE TABLE d0n_parent_ix (root_id BIGINT, owner_id BIGINT)"
                    "CREATE TABLE d0n_child_ix (owner_id BIGINT, heading_id BIGINT, payload TEXT NULL)")
      :version 1 :capabilities '(dolt)
      :read-tables
      '((:name "d0n_parent_ix" :key-column "owner_id" :scope-column "root_id")
        (:name "d0n_child_ix" :key-column "heading_id" :scope-column "owner_id"
         :scope-parent (:table "d0n_parent_ix" :key-column "owner_id")))
      :producer-resolver
      (lambda (request)
        (let* ((implementation (plist-get (plist-get request :producer) :implementation))
               (root (plist-get (plist-get request :scope) :root-id))
               (payloads (if (equal implementation "old")
                             '((1 . "same") (2 . "old") (2 . "old") (3) (5 . "removed"))
                           '((1 . "same") (2 . "new") (3 . "") (4 . "Case ") (6 . "already")))))
          (list :implementation implementation :dependencies "deps" :policy "(:policy)"
                :project (lambda (_context)
                           (dorg-test-d0hyz--native-effects root payloads))))))
     (dorg-migrate-schema ,dolt-dir)
     (dorg-commit ,dolt-dir "install d0hyz native fixture")
     (setq ,root-var (dorg-test-mutate--seed ,dolt-dir "* Upgrade root\n" 100))
     (dorg-test-mutate--seed ,dolt-dir "* Unrelated root\n" 200)
     (dorg-sql-batch
      ,dolt-dir
      (list "INSERT INTO d0n_parent_ix VALUES (100, 10), (200, 20)"
            "INSERT INTO d0n_child_ix VALUES (10, 1, 'same'), (10, 2, 'old'), (10, 2, 'old'), (10, 3, NULL), (10, 6, 'already')"
            "INSERT INTO d0n_child_ix VALUES (20, 9, 'unrelated'), (20, 9, 'unrelated'), (20, 8, NULL)"))
     (dorg-commit ,dolt-dir "seed d0hyz derived state")
     ,@body))

(defun dorg-test-d0hyz--native-request (dolt-dir root)
  "Return the native planning request at DOLT-DIR's HEAD for ROOT."
  (list :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir) :identity 'dorg
        :scope (list :root-id root)
        :old-producer (dorg-test-d0hyz--producer-id "old")
        :new-producer (dorg-test-d0hyz--producer-id "new")))

(defun dorg-test-d0hyz--native-rows (dolt-dir sql)
  "Return SQL's rows from DOLT-DIR as a sorted list of printed complete rows."
  (sort (mapcar #'prin1-to-string (dorg-sql dolt-dir sql)) #'string<))

(defun dorg-test-d0hyz--native-snapshot (dolt-dir)
  "Return scoped child rows, unrelated derived rows and primary rows."
  (list :scoped (dorg-test-d0hyz--native-rows
                 dolt-dir "SELECT * FROM d0n_child_ix WHERE owner_id = 10")
        :parents (dorg-test-d0hyz--native-rows dolt-dir "SELECT * FROM d0n_parent_ix")
        :unrelated (dorg-test-d0hyz--native-rows
                    dolt-dir "SELECT * FROM d0n_child_ix WHERE owner_id = 20")
        :headings (dorg-test-d0hyz--native-rows
                   dolt-dir "SELECT * FROM headings ORDER BY id")))

(ert-deftest dorg-test-d0hyz-native-dolt-forward-and-inverse-are-byte-exact ()
  "On pinned Dolt: forward writes exactly C->N (counted keyless duplicate
delete, NULL->empty, case and trailing space), the commit proof's system tables
report one parent and only the changed derived table, and inverse restores C
exactly; the unrelated root and primary rows never change."
  (dorg-test-mutate--with-dolt
    (dorg-test-d0hyz--with-native dolt-dir root
      (let* ((before (dorg-test-d0hyz--native-snapshot dolt-dir))
             (h (dorg-head-commit dolt-dir))
             (plan (dorg-ext-rebuild-plan-policy-upgrade
                    'd0hyz-native (dorg-test-d0hyz--native-request dolt-dir root)))
             (receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-native plan))
             (f (plist-get receipt :post-head))
             (after (dorg-test-d0hyz--native-snapshot dolt-dir)))
        (should (plist-get plan :applicable))
        (should (= 6 (plist-get plan :write-count)))
        (should (equal f (dorg-head-commit dolt-dir)))
        (should (equal (list h)
                       (mapcar (lambda (row) (alist-get 'parent_hash row))
                               (dorg-sql dolt-dir
                                         (format "SELECT parent_hash FROM dolt_commit_ancestors WHERE commit_hash = %s"
                                                 (dorg-sql-quote f))))))
        (should (equal '("d0n_child_ix")
                       (mapcar (lambda (row) (alist-get 'table_name row))
                               (dorg-sql dolt-dir
                                         (format "SELECT table_name FROM dolt_diff WHERE commit_hash = %s"
                                                 (dorg-sql-quote f))))))
        (should (equal (dorg-test-d0hyz--native-rows
                        dolt-dir "SELECT * FROM d0n_child_ix WHERE owner_id = 10")
                       (sort (mapcar #'prin1-to-string
                                     '(((owner_id . 10) (heading_id . 1) (payload . "same"))
                                       ((owner_id . 10) (heading_id . 2) (payload . "new"))
                                       ((owner_id . 10) (heading_id . 3) (payload . ""))
                                       ((owner_id . 10) (heading_id . 4) (payload . "Case "))
                                       ((owner_id . 10) (heading_id . 6) (payload . "already"))))
                             #'string<)))
        (should (equal (plist-get before :unrelated) (plist-get after :unrelated)))
        (should (equal (plist-get before :parents) (plist-get after :parents)))
        (should (equal (plist-get before :headings) (plist-get after :headings)))
        (let ((inverse (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-native receipt)))
          (should (equal (plist-get inverse :new-head) (dorg-head-commit dolt-dir)))
          (should (equal before (dorg-test-d0hyz--native-snapshot dolt-dir))))))))

(defun dorg-test-d0hyz--native-with-index (real)
  "Wrap transaction function REAL to add an index-only schema change."
  (lambda (dir base statements message &optional validate)
    (funcall real dir base
             (append statements
                     ;; A BIGINT column: a TEXT key needs a prefix length on Dolt.
                     (list "CREATE INDEX d0n_child_heading ON d0n_child_ix (heading_id)"))
             message validate)))

(ert-deftest dorg-test-d0hyz-native-dolt-schema-change-defeats-the-commit-proof ()
  "On pinned Dolt a forward commit carrying the intended row delta PLUS an
index-only schema change in the same allowed derived table is not proven:
inverse refuses with no write, and a lost reply on such a commit yields no
receipt.  The data-only positive is the byte-exact forward/inverse control."
  (dorg-test-mutate--with-dolt
    (dorg-test-d0hyz--with-native dolt-dir root
      (let* ((real (symbol-function 'dorg-transaction-commit))
             (plan (dorg-ext-rebuild-plan-policy-upgrade
                    'd0hyz-native (dorg-test-d0hyz--native-request dolt-dir root)))
             ;; Direct success: apply trusts its own transaction, so a receipt
             ;; exists; the inverse proof must then refuse the schema change.
             (receipt (cl-letf (((symbol-function 'dorg-transaction-commit)
                                 (dorg-test-d0hyz--native-with-index real)))
                        (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-native plan)))
             (f (dorg-head-commit dolt-dir)))
        (should (equal f (plist-get receipt :post-head)))
        ;; Premise: the rows are exactly the planned NEW image, and Dolt
        ;; reports a schema change for the only changed table.
        (should (equal (dorg-test-d0hyz--native-rows
                        dolt-dir "SELECT * FROM d0n_child_ix WHERE owner_id = 10")
                       (sort (mapcar #'prin1-to-string
                                     '(((owner_id . 10) (heading_id . 1) (payload . "same"))
                                       ((owner_id . 10) (heading_id . 2) (payload . "new"))
                                       ((owner_id . 10) (heading_id . 3) (payload . ""))
                                       ((owner_id . 10) (heading_id . 4) (payload . "Case "))
                                       ((owner_id . 10) (heading_id . 6) (payload . "already"))))
                             #'string<)))
        (let ((diff (dorg-sql dolt-dir
                              (format "SELECT table_name, schema_change FROM dolt_diff WHERE commit_hash = %s"
                                      (dorg-sql-quote f)))))
          (should (equal '("d0n_child_ix")
                         (mapcar (lambda (row) (alist-get 'table_name row)) diff)))
          (should-not (dorg-ext-rebuild--policy-sql-false-p
                       (alist-get 'schema_change (car diff)))))
        (let ((err (should-error
                    (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-native receipt)
                    :type 'dorg-ext-rebuild-error)))
          (should (eq 'non-derived-diff (plist-get (dorg-error-data err) :reason))))
        (should (equal f (dorg-head-commit dolt-dir))))))
  (dorg-test-mutate--with-dolt
    (dorg-test-d0hyz--with-native dolt-dir root
      (let* ((real (symbol-function 'dorg-transaction-commit))
             (with-index (dorg-test-d0hyz--native-with-index real))
             (h (dorg-head-commit dolt-dir))
             (plan (dorg-ext-rebuild-plan-policy-upgrade
                    'd0hyz-native (dorg-test-d0hyz--native-request dolt-dir root))))
        (cl-letf (((symbol-function 'dorg-transaction-commit)
                   (lambda (&rest args)
                     (let ((new (apply with-index args)))
                       (signal 'dorg-transaction-indeterminate
                               (list "lost reply" :base h :commit new))))))
          (let ((err (should-error (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-native plan)
                                   :type 'dorg-transaction-indeterminate)))
            (should (eq 'ambiguous (plist-get (cddr err) :reconciled)))
            (should-not (equal h (dorg-head-commit dolt-dir)))))))))

(ert-deftest dorg-test-d0hyz-native-dolt-lost-reply-reconciles-by-proof ()
  "On pinned Dolt a lost reply after a real commit reconciles to a receipt
equal to direct success, and one before any commit stays not-committed."
  (dorg-test-mutate--with-dolt
    (dorg-test-d0hyz--with-native dolt-dir root
      (let* ((real (symbol-function 'dorg-transaction-commit))
             (h (dorg-head-commit dolt-dir))
             (plan (dorg-ext-rebuild-plan-policy-upgrade
                    'd0hyz-native (dorg-test-d0hyz--native-request dolt-dir root))))
        (cl-letf (((symbol-function 'dorg-transaction-commit)
                   (lambda (&rest _)
                     (signal 'dorg-transaction-indeterminate
                             (list "lost reply" :base h :commit nil)))))
          (let ((err (should-error (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-native plan)
                                   :type 'dorg-transaction-indeterminate)))
            (should (eq 'not-committed (plist-get (cddr err) :reconciled)))
            (should (equal plan (plist-get (cddr err) :next-plan)))
            (should (equal h (dorg-head-commit dolt-dir)))))
        (cl-letf (((symbol-function 'dorg-transaction-commit)
                   (lambda (&rest args)
                     (let ((new (apply real args)))
                       (signal 'dorg-transaction-indeterminate
                               (list "lost reply" :base h :commit new))))))
          (let ((receipt (dorg-ext-rebuild-apply-policy-upgrade 'd0hyz-native plan)))
            (should (equal (dorg-head-commit dolt-dir) (plist-get receipt :post-head)))
            (should (equal (plist-get plan :plan-digest) (plist-get receipt :plan-digest)))
            (should (plist-get (dorg-ext-rebuild-invert-policy-upgrade 'd0hyz-native receipt)
                               :ok))))))))

(ert-deftest dorg-test-dz7ir-audit-store-spies-frozen-whole-table-reads ()
  "The public audit reports the independently observable exact-H read plan."
  (let ((dorg--extensions nil)
        queries)
    (dorg-register-extension
     'dz7ir-demo :schema "CREATE TABLE ignored (id BIGINT)" :version 1
     :capabilities '(dolt)
     :read-tables
     '((:name "dz_parent_ix" :key-column "owner_id" :scope-column "root_id")
       (:name "dz_child_ix" :key-column "heading_id" :scope-column "owner_id"
        :scope-parent (:table "dz_parent_ix" :key-column "owner_id")))
     :project (lambda (_context) nil))
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (push sql queries)
                 (cond
                  ((string-match-p "FROM headings" sql) '(((id . 1))))
                  ((string-match-p "FROM dz_parent_ix" sql)
                   '(((root_id . 1) (owner_id . 10))
                     ((root_id . 20) (owner_id . 20))))
                  ((string-match-p "FROM dz_child_ix" sql)
                   '(((owner_id . 10) (heading_id . 101) (payload . "A"))
                     ((owner_id . 20) (heading_id . 102) (payload . "B"))
                     ((owner_id . 99) (heading_id . 103) (payload . nil))))
                  (t (error "unexpected audit SQL: %s" sql))))))
      (let* ((report (dorg-ext-rebuild-audit-store
                      'dz7ir-demo :dolt-dir "audit-fixture" :revision "f00d"
                      :identity 'dorg))
             (child (cl-find "dz_child_ix" (plist-get report :tables)
                             :key (lambda (table) (plist-get table :table))
                             :test #'string=)))
        (should (plist-get report :complete))
        (should-not (plist-get report :clean))
        (should (= 3 (plist-get report :statement-count)))
        (should (= 3 (length queries)))
        (should (cl-some (lambda (sql) (string-match-p "level_offset = 0" sql))
                         queries))
        (should (cl-every (lambda (sql) (string-match-p " AS OF 'f00d'" sql))
                          queries))
        (should (= 1 (alist-get 'owned-canonical-root
                                (plist-get child :class-counts))))
        (should (= 1 (alist-get 'owned-stored-only-root
                                (plist-get child :class-counts))))
        (should (= 1 (alist-get 'orphan-child
                                (plist-get child :class-counts))))))))

(ert-deftest dorg-test-dz7ir-audit-store-keeps-unread-count-unknown ()
  "A descriptor read failure returns an incomplete, non-clean report."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'dz7ir-partial :schema "CREATE TABLE ignored (id BIGINT)" :version 1
     :capabilities '(dolt)
     :read-tables '((:name "dz_partial_ix" :key-column "id" :scope-column "root_id"))
     :project (lambda (_context) nil))
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (cond ((string-match-p "FROM headings" sql) nil)
                       ((string-match-p "FROM dz_partial_ix" sql)
                        (error "injected grouped-read failure"))
                       (t (error "unexpected audit SQL: %s" sql))))))
      (let* ((report (dorg-ext-rebuild-audit-store
                      'dz7ir-partial :dolt-dir "audit-fixture" :revision "f00d"
                      :identity 'dorg))
             (table (car (plist-get report :tables))))
        (should (plist-get report :ok))
        (should-not (plist-get report :complete))
        (should (eq 'incomplete (plist-get report :outcome)))
        (should-not (plist-get report :clean))
        (should-not (plist-get table :rows-read))
        (should (eq 'table-read (plist-get (car (plist-get report :errors)) :reason)))))))

(ert-deftest dorg-test-dz7ir-audit-store-does-not-call-an-unread-parent-an-orphan ()
  "An unread parent leaves a read child incomplete, not falsely orphaned."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'dz7ir-parent-read :schema "CREATE TABLE ignored (id BIGINT)" :version 1
     :capabilities '(dolt)
     :read-tables
     '((:name "dz_parent_read_ix" :key-column "id" :scope-column "root_id")
       (:name "dz_child_read_ix" :key-column "id" :scope-column "parent_id"
        :scope-parent (:table "dz_parent_read_ix" :key-column "id")))
     :project (lambda (_context) nil))
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (cond ((string-match-p "FROM headings" sql) nil)
                       ((string-match-p "FROM dz_parent_read_ix" sql)
                        (error "injected parent failure"))
                       ((string-match-p "FROM dz_child_read_ix" sql)
                        '(((parent_id . 10) (id . 20))))
                       (t (error "unexpected audit SQL: %s" sql))))))
      (let* ((report (dorg-ext-rebuild-audit-store
                      'dz7ir-parent-read :dolt-dir "audit-fixture" :revision "f00d"
                      :identity 'dorg))
             (child (cl-find "dz_child_read_ix" (plist-get report :tables)
                             :key (lambda (table) (plist-get table :table))
                             :test #'string=)))
        (should-not (plist-get report :complete))
        (should (= 1 (plist-get child :rows-read)))
        (should (= 0 (plist-get child :accounted-count)))
        (should (= 0 (alist-get 'orphan-child (plist-get child :class-counts))))
        (should (cl-find 'parent-unread (plist-get report :errors)
                         :key (lambda (error) (plist-get error :reason))))))))

(ert-deftest dorg-test-dz7ir-audit-store-refuses-bad-revisions-before-reads ()
  "Every invalid H is typed and opens no audit data read port."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'dz7ir-refusal :schema "CREATE TABLE ignored (id BIGINT)" :version 1
     :capabilities '(dolt)
     :read-tables '((:name "dz_refusal_ix" :key-column "id" :scope-column "root_id"))
     :project (lambda (_context) nil))
    (dolist (case '((nil . revision-missing)
                    ("" . revision-empty)
                    ("not-a-commit" . revision-malformed)
                    ("f00d" . revision-unreachable)))
      (let (sql-opened)
        (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) nil))
                  ((symbol-function 'dorg-sql) (lambda (&rest _) (setq sql-opened t))))
          (let ((err (should-error
                      (dorg-ext-rebuild-audit-store
                       'dz7ir-refusal :dolt-dir "audit-fixture" :revision (car case)
                       :identity 'dorg)
                      :type 'dorg-ext-rebuild-error)))
            (should (eq (cdr case) (plist-get (dorg-error-data err) :reason)))
            (should-not sql-opened)))))))

(ert-deftest dorg-test-dz7ir-audit-as-of-removal-mutant-is-observable ()
  "Removing AS OF after the interloper makes the frozen-read assertion RED."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let ((root (dorg-test-mutate--seed
                   dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100)))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (let ((revision (dorg-head-commit dolt-dir)))
          (cl-letf (((symbol-function 'dorg-sql-gen--as-of-suffix)
                     (lambda (_revision) "")))
            (let (queries calls)
              (let* ((report (dorg-test-dz7ir--audit-with-query
               'bnp46-demo dolt-dir revision '(:property "BEAD_ID")
               (lambda (sql)
                 (prog1 (dorg-sql dolt-dir sql)
                   (push sql queries)
                   (cl-incf calls)
                   (when (= calls 1)
                     (dorg-sql-batch
                      dolt-dir
                      (list "INSERT INTO bnp46_owner_ix VALUES (999, 777, NULL)"))
                     (dorg-commit dolt-dir "advance HEAD during AS-OF mutant"))))))
                     (owner (cl-find "bnp46_owner_ix" (plist-get report :tables)
                                     :key (lambda (table) (plist-get table :table))
                                     :test #'string=)))
              (should-error
               (should (and owner
                            (numberp (plist-get owner :rows-read))
                            (= 1 (plist-get owner :rows-read))
                            (numberp (alist-get 'owned-canonical-root
                                                (plist-get owner :class-counts)))
                            (= 1 (alist-get 'owned-canonical-root
                                            (plist-get owner :class-counts)))))
               :type 'ert-test-failed)))))))))

(ert-deftest dorg-test-dz7ir-audit-per-row-lookup-mutant-is-observable ()
  "A per-row replacement for grouped reads exceeds the audited statement bound."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'dz7ir-mutant-count :schema "CREATE TABLE ignored (id BIGINT)" :version 1
     :capabilities '(dolt)
     :read-tables
     '((:name "dz_count_parent_ix" :key-column "owner_id" :scope-column "root_id")
       (:name "dz_count_child_ix" :key-column "heading_id" :scope-column "owner_id"
        :scope-parent (:table "dz_count_parent_ix" :key-column "owner_id")))
     :project (lambda (_context) nil))
    (let ((original (symbol-function 'dorg-ext-rebuild--audit-table-rows)))
      (cl-letf (((symbol-function 'dorg-ext-rebuild--audit-table-rows)
                 (lambda (query descriptor revision)
                   (let ((rows (funcall original query descriptor revision)))
                     (dolist (_ rows)
                       (funcall query (format "SELECT 1%s"
                                              (dorg-sql-gen--as-of-suffix revision))))
                     rows))))
        (let ((report
               (dorg-test-dz7ir--audit-with-query
                'dz7ir-mutant-count "audit-fixture" "f00d" 'dorg
                (lambda (sql)
                  (cond
                   ((string-match-p "FROM headings" sql) '(((id . 1))))
                   ((string-match-p "FROM dz_count_parent_ix" sql)
                    '(((root_id . 1) (owner_id . 1))))
                   ((string-match-p "FROM dz_count_child_ix" sql)
                    (mapcar (lambda (id) `((owner_id . 1) (heading_id . ,id)))
                            (number-sequence 1 10)))
                   ((string-match-p "SELECT 1" sql) nil)
                   (t (error "unexpected audit SQL: %s" sql)))))))
          (should-error
           (should (<= (plist-get report :statement-count) 3))
           :type 'ert-test-failed))))))

(ert-deftest dorg-test-dz7ir-audit-omitted-classification-mutant-is-observable ()
  "Dropping a class record leaves source accounting incomplete, never clean."
  (let* ((descriptors '((:name "dz_omit_ix" :key-column "id" :scope-column "root_id")))
         (rows '(("dz_omit_ix" . (((root_id . 1) (id . 11))))))
         (original (symbol-function 'dorg-ext-rebuild--audit-records)))
    (should (plist-get (dorg-ext-rebuild--audit-classify-rows descriptors rows '(1))
                       :complete))
    (cl-letf (((symbol-function 'dorg-ext-rebuild--audit-records)
               (lambda (entries) (butlast (funcall original entries)))))
      (let ((report (dorg-ext-rebuild--audit-classify-rows descriptors rows '(1))))
        (should-error (should (plist-get report :complete)) :type 'ert-test-failed)
        (should-not (plist-get report :clean))
        (should (equal 'accounting-mismatch
                       (plist-get (car (plist-get report :errors)) :reason)))))))

(ert-deftest dorg-test-dz7ir-audit-null-collapse-mutant-is-observable ()
  "Collapsing SQL NULL to empty text makes the full-row class assertion RED."
  (let* ((descriptors '((:name "dz_null_ix" :key-column "id" :scope-column "root_id")))
         (rows '(("dz_null_ix" . (((root_id . nil) (id . 11) (payload . nil))))))
         (original (symbol-function 'dorg-ext-rebuild--audit-row-value)))
    (should (= 1 (alist-get 'undeclared-scope
                             (plist-get (car (plist-get
                                              (dorg-ext-rebuild--audit-classify-rows
                                               descriptors rows nil)
                                              :tables))
                                        :class-counts))))
    (cl-letf (((symbol-function 'dorg-ext-rebuild--audit-row-value)
               (lambda (row column) (or (funcall original row column) ""))))
      (let ((report (dorg-ext-rebuild--audit-classify-rows descriptors rows nil)))
        (should-error
         (should (= 1 (alist-get 'undeclared-scope
                                  (plist-get (car (plist-get report :tables))
                                             :class-counts))))
         :type 'ert-test-failed)))))

(ert-deftest dorg-test-bnp46-rebuild-requires-explicit-supported-identity ()
  "Missing, malformed, and none identity refuse before any Dolt request."
  (dolist (case '(((:dolt-dir "never" :revision "r" :mode audit) identity-missing)
                  ((:dolt-dir "never" :revision "r" :identity nil :mode audit) identity-missing)
                  ((:dolt-dir "never" :revision "r" :identity none :mode audit) identity-none)
                  ((:dolt-dir "never" :revision "r" :identity bad :mode audit) identity-malformed)))
    (let (sql-opened ancestor-checked)
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (&rest _) (setq sql-opened t)))
                ((symbol-function 'dorg-commit-ancestor-p)
                 (lambda (&rest _) (setq ancestor-checked t))))
        (let ((err (should-error
                    (apply #'dorg-ext-rebuild-index 'missing 1 (car case))
                    :type 'dorg-ext-rebuild-error)))
          (should (eq (cadr case) (plist-get (dorg-error-data err) :reason)))
          (should-not sql-opened)
          (should-not ancestor-checked))))))

(ert-deftest dorg-test-bnp46-child-effect-requires-expected-parent-key ()
  "Child effects use projected parent keys, never direct root equality."
  (let ((descriptors '((:name "bnp46_owner_ix" :scope-column "root_id")
                       (:name "bnp46_child_ix" :scope-column "bead_id"
                        :scope-parent (:table "bnp46_owner_ix" :key-column "owner_id"))))
        (keys (make-hash-table :test 'equal)))
    (puthash "bnp46_child_ix" '(11) keys)
    (should (equal
             '(:table "bnp46_child_ix" :insert ((bead_id . 11) (heading_id . 4)))
             (dorg-ext-rebuild--scope-effect
              '(:table "bnp46_child_ix" :insert ((bead_id . 11) (heading_id . 4)))
              descriptors 1 keys)))
    (should-error
     (dorg-ext-rebuild--scope-effect
      '(:table "bnp46_child_ix" :insert ((bead_id . 12) (heading_id . 4)))
      descriptors 1 keys)
     :type 'dorg-ext-rebuild-error)
    (should-error
     (dorg-ext-rebuild--scope-effect
      '(:table "bnp46_child_ix" :delete ((heading_id . 4)))
      descriptors 1 keys)
     :type 'dorg-ext-rebuild-error)))

(ert-deftest dorg-test-bnp46-register-validates-one-edge-scope-parent ()
  "A parent may follow its child declaration; invalid parents are refused."
  (let ((dorg--extensions nil))
    (should
     (dorg-register-extension
      'bnp46-registration :schema "" :version 1
      :read-tables
      '((:name "bnp46_child" :scope-column "bead_id"
         :scope-parent (:table "bnp46_parent" :key-column "owner_id"))
        (:name "bnp46_parent" :scope-column "root_id")))))
  (let ((dorg--extensions nil))
    (should-error
     (dorg-register-extension
      'bnp46-self :schema "" :version 1
      :read-tables
      '((:name "bnp46_self" :scope-column "bead_id"
         :scope-parent (:table "bnp46_self" :key-column "bead_id"))))))
  (dolist (tables
           '(((:name "bnp46_child" :scope-column "bead_id"
               :scope-parent (:table "bnp46_absent" :key-column "owner_id")))
             ((:name "bnp46_child" :scope-column "bead_id"
               :scope-parent (:table "bnp46_parent" :key-column "owner_id"))
              (:name "bnp46_parent" :scope-column "owner_id"
               :scope-parent (:table "bnp46_root" :key-column "root_id"))
              (:name "bnp46_root" :scope-column "root_id"))))
    (let ((dorg--extensions nil))
      (should-error
       (dorg-register-extension 'bnp46-invalid :schema "" :version 1
                                :read-tables tables)))))

(ert-deftest dorg-test-bnp46-memberships-query-once-per-parent-table ()
  "Shared parent membership needs one stored and one ambiguity query."
  (let ((descriptors
         '((:name "bnp46_owner_ix" :scope-column "root_id")
           (:name "bnp46_child_a_ix" :scope-column "bead_id"
            :scope-parent (:table "bnp46_owner_ix" :key-column "owner_id"))
           (:name "bnp46_child_b_ix" :scope-column "bead_id"
            :scope-parent (:table "bnp46_owner_ix" :key-column "owner_id"))))
        (expected (make-hash-table :test 'equal))
        queries)
    (puthash "bnp46_child_a_ix" '(11 10) expected)
    (puthash "bnp46_child_b_ix" '(11 10) expected)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dir sql)
                 (push sql queries)
                 (if (string-match-p "WHERE root_id = 1" sql)
                     '(((owner_id . 10)) ((owner_id . 11)))
                   nil))))
      (let ((memberships (dorg-ext-rebuild--scope-memberships
                          "never-opened" "r" descriptors 1 expected)))
        (should (= 2 (length queries)))
        (should (equal '(10 11) (gethash "bnp46_child_a_ix" memberships)))
        (should (equal '(10 11) (gethash "bnp46_child_b_ix" memberships)))
        (should (equal '(11 10) (gethash "bnp46_child_a_ix" expected)))
        (should (equal '(11 10) (gethash "bnp46_child_b_ix" expected)))))))

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
  (dorg-migrate-schema dolt-dir)
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
     (dorg-migrate-schema ,dolt-dir)
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
                   :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'audit)
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
          :revision "fixture-revision" :identity 'dorg :mode 'repair)
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
                :revision "fixture-revision" :identity 'dorg :mode 'repair)
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
                   :revision "fixture-revision" :identity 'dorg :mode 'repair)
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
              :revision "fixture-revision" :identity 'dorg :mode 'repair)))
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
                      :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'audit)))
          (should (= 1 (plist-get (plist-get audit :drift) :extra-count))))
        ;; The repair gate refuses before its scope clear, preserving the
        ;; inherited row that this resolver cannot reproduce.
        (should-error
         (dorg-ext-rebuild-index
          'pu52f-config-split root :dolt-dir dolt-dir
          :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
                       :revision revision :identity 'dorg :mode 'audit))
               (repair (dorg-ext-rebuild-index
                        'pu52f-config-split root :dolt-dir dolt-dir
                        :revision revision :identity 'dorg :mode 'repair)))
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
           :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair))
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
            :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
                      :revision revision-r :identity 'dorg :mode 'audit)))
          (should (equal revision-r (plist-get audit :observed-core-revision)))
          (should (= 1 (plist-get (plist-get audit :drift) :missing-count))))
        (should-error
         (dorg-ext-rebuild-index
          'pu52f-demo root :dolt-dir dolt-dir
          :revision revision-r :identity 'dorg :mode 'repair))))))

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
              :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
                       :revision revision :identity 'dorg :mode 'audit))
               (repair (dorg-ext-rebuild-index
                        'pu52f-demo root :dolt-dir dolt-dir
                        :revision revision :identity 'dorg :mode 'repair)))
          (should (equal incremental (dorg-test-pu52f--rows dolt-dir "pu52f_ix" root)))
          (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                         (plist-get audit :drift)))
          (should-not (plist-get repair :new-token))
          (should (equal revision (dorg-head-commit dolt-dir))))))))

(ert-deftest dorg-test-zjpp4-public-cross-owner-move-audits-at-commit ()
  "A public cross-owner move's rows equal an audit rebuild at its commit.
This is a synthetic owned Dolt store, not a tracker fixture.  The public apply
uses the ordinary mutation/projector transaction; the only rebuild operation is
read-only `audit' at that exact post-move revision.  Owner Other is untouched
and proves the audit compares the whole owned row scope, not partitions."
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'zjpp4-owner-rows
       :schema "CREATE TABLE zjpp4_owner_ix (root_id BIGINT, owner_id BIGINT, heading_id BIGINT, parent_id BIGINT, title TEXT, PRIMARY KEY (root_id, owner_id, heading_id))"
       :version 1 :capabilities '(dolt)
       :read-tables '((:name "zjpp4_owner_ix" :key-column "heading_id"
                       :scope-column "root_id"))
       :project #'dorg-test-zjpp4--project-owner-rows)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install zjpp4 owner-row fixture")
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir
                    (concat "* Left\n:PROPERTIES:\n:BEAD_ID: left\n:END:\n"
                            "** Moving\n*** Child\n**** Grandchild\n"
                            "* Right\n:PROPERTIES:\n:BEAD_ID: right\n:END:\n"
                            "** Untouched right\n"
                            "* Other\n:PROPERTIES:\n:BEAD_ID: other\n:END:\n"
                            "** Untouched other\n")
                    100))
             (left (dorg-test-mutate--id-by-title dolt-dir "Left"))
             (moving (dorg-test-mutate--id-by-title dolt-dir "Moving"))
             (child (dorg-test-mutate--id-by-title dolt-dir "Child"))
             (grandchild
              (dorg-test-mutate--id-by-title dolt-dir "Grandchild"))
             (right (dorg-test-mutate--id-by-title dolt-dir "Right"))
             (other (dorg-test-mutate--id-by-title dolt-dir "Other"))
             (other-child
              (dorg-test-mutate--id-by-title dolt-dir "Untouched other"))
             (source (list :dolt-dir dolt-dir :root-id root
                           :identity '(:property "BEAD_ID"))))
        ;; The unrelated owner is present before the move.  Its rows must
        ;; remain through the incremental transaction and be reproduced by the
        ;; canonical full-image audit; the two moved-side owners are populated
        ;; by the public move's actual projector effects.
        (dorg-sql-batch
         dolt-dir
         (list
          (format "INSERT INTO zjpp4_owner_ix VALUES (%d, %d, %d, %d, 'Other')"
                  root other other root)
          (format "INSERT INTO zjpp4_owner_ix VALUES (%d, %d, %d, %d, 'Untouched other')"
                  root other other-child other)))
        (dorg-commit dolt-dir "seed unrelated owner projection")
        (let ((result
               (dorg-api-execute
                `(on (id ,moving) (move (subtree) (under (id ,right))))
                :identity 'dorg :mode 'apply :source source)))
          (should (plist-get result :ok))
          (should (plist-get result :new-token))
          (let ((tree (dorg-model-load-tree dolt-dir root)))
            (should (= root (dorg-heading-parent-id (gethash left tree))))
            (should (= right (dorg-heading-parent-id (gethash moving tree))))
            (should (= moving (dorg-heading-parent-id (gethash child tree))))
            (should (= child (dorg-heading-parent-id (gethash grandchild tree)))))
          ;; The public move's real projector replaces the two affected owner
          ;; scopes.  The left scope loses the complete plain closure and the
          ;; right scope gains it before the rebuild independently audits rows.
          (should (= 0 (alist-get 'n (car (dorg-sql
                                             dolt-dir
                                             (format "SELECT COUNT(*) AS n FROM zjpp4_owner_ix WHERE owner_id = %d AND heading_id IN (%d, %d, %d)"
                                                     left moving child grandchild))))))
          (should (= 3 (alist-get 'n (car (dorg-sql
                                             dolt-dir
                                             (format "SELECT COUNT(*) AS n FROM zjpp4_owner_ix WHERE owner_id = %d AND heading_id IN (%d, %d, %d)"
                                                     right moving child grandchild))))))
          (should (equal `((owner_id . ,other) (heading_id . ,other)
                           (title . "Other"))
                         (car (dorg-sql
                               dolt-dir
                               (format "SELECT owner_id, heading_id, title FROM zjpp4_owner_ix WHERE owner_id = %d AND heading_id = %d"
                                       other other)))))
          (let* ((revision (dorg-head-commit dolt-dir))
                 (audit (dorg-ext-rebuild-index
                         'zjpp4-owner-rows root :dolt-dir dolt-dir
                         :revision revision :identity '(:property "BEAD_ID") :mode 'audit)))
            (should (equal revision (plist-get audit :observed-core-revision)))
            (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                           (plist-get audit :drift)))))))))

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
         :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
                      :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'audit)))
          ;; A create-only rebuild expects marker "create", proving the audit
          ;; can expose a non-derivable projector rather than bless it.  Full
          ;; rows differ, so the conservative multiset reports one missing and
          ;; one unexplained extra instead of assuming heading_id is unique.
          (should (= 1 (plist-get (plist-get audit :drift) :missing-count)))
          (should (= 1 (plist-get (plist-get audit :drift) :extra-count))))))))

(ert-deftest dorg-test-pu52f-register-rejects-hostile-scope-column ()
  "Unsafe rebuild scope metadata cannot enter through public registration."
  (let ((dorg--extensions nil))
    (should-error
     (dorg-register-extension
      'pu52f-hostile-scope
      :schema "CREATE TABLE pu52f_hostile_ix (root_id BIGINT, heading_id BIGINT)"
      :version 1 :capabilities '(dolt)
      :read-tables '((:name "pu52f_hostile_ix" :key-column "heading_id"
                      :scope-column "root_id; DELETE FROM headings"))
      :project (lambda (_context) nil)))
    (should-not (dorg-get-extension 'pu52f-hostile-scope))))

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
                       :scope-column "root_id"))
       :project (lambda (_context) nil))
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install hostile-scope fixture")
      (let ((root (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100)))
        ;; Registration now rejects hostile identifiers.  Corrupt the private
        ;; fixture after registering a valid descriptor so the rebuild guard
        ;; remains an independent defense-in-depth witness.
        (setf (dorg-extension-read-tables
               (dorg-get-extension 'pu52f-hostile-scope))
              '((:name "pu52f_hostile_ix" :key-column "heading_id"
                 :scope-column "root_id; DELETE FROM headings")))
        (cl-letf (((symbol-function 'dorg-transaction-commit)
                   (lambda (&rest _)
                     (setq transaction-opened t)
                     (error "unexpected rebuild transaction"))))
          (let ((err (should-error
                      (dorg-ext-rebuild-index
                       'pu52f-hostile-scope root :dolt-dir dolt-dir
                       :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
         :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair)
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
                   (lambda (effect _descriptors _root-id &rest _) effect)))
          (dorg-ext-rebuild-index
           'pu52f-demo root :dolt-dir dolt-dir
           :revision (dorg-head-commit dolt-dir) :identity 'dorg :mode 'repair))
        (should-not (dorg-test-pu52f--rows dolt-dir "pu52f_ix" foreign-root))))))

(ert-deftest dorg-test-bnp46-owner-partitioned-rebuild-is-clean-after-repair ()
  "A rebuild supplies property-owner partitions and scopes child rows via parent.
The nested Inner owner must retain Outer as its parent owner.  The projector
has no image fallback, so an empty rebuild partition set cannot make this
audit clean."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir
                    (concat "* Outer\n:PROPERTIES:\n:BEAD_ID: outer\n:END:\n"
                            "** Plain\n*** Inner\n:PROPERTIES:\n:BEAD_ID: inner\n:END:\n"
                            "**** Leaf\n")
                    100))
             (outer (dorg-test-mutate--id-by-title dolt-dir "Outer"))
             (inner (dorg-test-mutate--id-by-title dolt-dir "Inner")))
        (let ((repair (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)))
          (should (plist-get repair :new-token)))
        (let ((audit (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID"))))
          (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                         (plist-get audit :drift))))
        (should (equal `((owner_id . ,inner) (parent_owner_id . ,outer))
                       (car (dorg-sql
                             dolt-dir
                             (format "SELECT owner_id, parent_owner_id FROM bnp46_owner_ix WHERE root_id = %d AND owner_id = %d"
                                     root inner)))))))))

(ert-deftest dorg-test-bnp46-child-missing-repairs-and-extra-refuses ()
  "A child missing under M repairs; an unexplained child remains protected."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100))
             (owner (dorg-test-mutate--id-by-title dolt-dir "Owner"))
             (leaf (dorg-test-mutate--id-by-title dolt-dir "Leaf")))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch dolt-dir
                        (list (format "DELETE FROM bnp46_child_ix WHERE bead_id = %d AND heading_id = %d"
                                      owner leaf)))
        (dorg-commit dolt-dir "remove expected bnp46 child")
        (should (= 1 (plist-get (plist-get (dorg-test-bnp46--audit
                                             dolt-dir root '(:property "BEAD_ID")) :drift)
                                :missing-count)))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch dolt-dir
                        (list (format "INSERT INTO bnp46_child_ix VALUES (%d, 999, 'extra')" owner)))
        (dorg-commit dolt-dir "add unexplained bnp46 child")
        (let ((audit (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID"))))
          (should (= 1 (plist-get (plist-get audit :drift) :extra-count)))
          (should-error (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
                        :type 'dorg-ext-rebuild-error))))))

(ert-deftest dorg-test-bnp46-child-changes-and-duplicates-refuse-repair ()
  "A changed or duplicated full child row is unexplained drift, never repaired."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100))
             (owner (dorg-test-mutate--id-by-title dolt-dir "Owner"))
             (leaf (dorg-test-mutate--id-by-title dolt-dir "Leaf")))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch
         dolt-dir
         (list (format "UPDATE bnp46_child_ix SET title = 'changed' WHERE bead_id = %d AND heading_id = %d"
                       owner leaf)))
        (dorg-commit dolt-dir "change expected bnp46 child")
        (let ((drift (plist-get (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID")) :drift)))
          (should (= 1 (plist-get drift :missing-count)))
          (should (= 1 (plist-get drift :extra-count)))
          (should-error (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
                        :type 'dorg-ext-rebuild-error))
        ;; Restore the expected row, then add a byte-identical duplicate.  The
        ;; child table deliberately has no primary key, so multiset comparison
        ;; (rather than key comparison) must catch it.
        (dorg-sql-batch
         dolt-dir
         (list (format "UPDATE bnp46_child_ix SET title = 'Leaf' WHERE bead_id = %d AND heading_id = %d"
                       owner leaf)
               (format "INSERT INTO bnp46_child_ix VALUES (%d, %d, 'Leaf')" owner leaf)))
        (dorg-commit dolt-dir "duplicate full bnp46 child")
        (let ((drift (plist-get (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID")) :drift)))
          (should (= 1 (plist-get drift :extra-count)))
          (should-error (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
                        :type 'dorg-ext-rebuild-error))))))

(ert-deftest dorg-test-bnp46-missing-stored-parent-keeps-its-child-single ()
  "Projected parent membership preserves a stored child through a parent repair."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n** Leaf\n" 100))
             (owner (dorg-test-mutate--id-by-title dolt-dir "Owner"))
             (leaf (dorg-test-mutate--id-by-title dolt-dir "Leaf")))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch dolt-dir
                        (list (format "DELETE FROM bnp46_owner_ix WHERE root_id = %d AND owner_id = %d"
                                      root owner)))
        (dorg-commit dolt-dir "remove bnp46 parent but retain child")
        (let ((drift (plist-get (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID")) :drift)))
          ;; The missing parent is the only missing row; the retained child was
          ;; read through M = stored UNION projected parent keys.
          (should (= 1 (plist-get drift :missing-count)))
          (should (= 0 (plist-get drift :extra-count))))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (should (= 1 (length (dorg-sql dolt-dir
                                      (format "SELECT * FROM bnp46_child_ix WHERE bead_id = %d AND heading_id = %d"
                                              owner leaf)))))))))

(ert-deftest dorg-test-bnp46-root-repair-leaves-second-root-byte-identical ()
  "Root-local rebuild ignores and preserves a second root's owner and children."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root-a (dorg-test-mutate--seed
                      dolt-dir "* Owner A\n:PROPERTIES:\n:BEAD_ID: owner-a\n:END:\n** Leaf A\n" 100))
             (owner-a (dorg-test-mutate--id-by-title dolt-dir "Owner A"))
             (leaf-a (dorg-test-mutate--id-by-title dolt-dir "Leaf A"))
             (root-b (dorg-test-mutate--seed
                      dolt-dir "* Owner B\n:PROPERTIES:\n:BEAD_ID: owner-b\n:END:\n** Leaf B\n" 200))
             (owner-b (dorg-test-mutate--id-by-title dolt-dir "Owner B")))
        (dorg-test-bnp46--audit dolt-dir root-a '(:property "BEAD_ID") 'repair)
        (dorg-test-bnp46--audit dolt-dir root-b '(:property "BEAD_ID") 'repair)
        (let ((before-owner (dorg-sql dolt-dir (format "SELECT * FROM bnp46_owner_ix WHERE root_id = %d" root-b)))
              (before-child (dorg-sql dolt-dir (format "SELECT * FROM bnp46_child_ix WHERE bead_id = %d" owner-b))))
          (dorg-sql-batch dolt-dir
                          (list (format "DELETE FROM bnp46_child_ix WHERE bead_id = %d AND heading_id = %d"
                                        owner-a leaf-a)))
          (dorg-commit dolt-dir "remove one root-a bnp46 child")
          (should (= 1 (plist-get
                        (plist-get (dorg-test-bnp46--audit
                                    dolt-dir root-a '(:property "BEAD_ID")) :drift)
                        :missing-count)))
          (dorg-test-bnp46--audit dolt-dir root-a '(:property "BEAD_ID") 'repair)
          (should (equal before-owner
                         (dorg-sql dolt-dir (format "SELECT * FROM bnp46_owner_ix WHERE root_id = %d" root-b))))
          (should (equal before-child
                         (dorg-sql dolt-dir (format "SELECT * FROM bnp46_child_ix WHERE bead_id = %d" owner-b)))))))))

(ert-deftest dorg-test-bnp46-wrong-valid-identity-reports-drift-and-refuses-repair ()
  "A valid but wrong identity cannot claim a BEAD_ID-owned index is clean."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let ((root (dorg-test-mutate--seed
                   dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n" 100)))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (should-not (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                           (plist-get (dorg-test-bnp46--audit dolt-dir root 'dorg) :drift)))
        (should-error (dorg-test-bnp46--audit dolt-dir root 'dorg 'repair)
                      :type 'dorg-ext-rebuild-error)))))

(ert-deftest dorg-test-bnp46-cross-root-parent-key-refuses ()
  "A parent key visible under a second root makes child scope ambiguous."
  (dorg-test-mutate--with-dolt
    (dorg-test-bnp46--with-extension dolt-dir
      (let* ((root (dorg-test-mutate--seed
                    dolt-dir "* Owner\n:PROPERTIES:\n:BEAD_ID: owner\n:END:\n" 100))
             (other-root (dorg-test-mutate--seed dolt-dir "* Other\n" 200))
             (owner (dorg-test-mutate--id-by-title dolt-dir "Owner")))
        (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
        (dorg-sql-batch
         dolt-dir
         (list (format "INSERT INTO bnp46_owner_ix VALUES (%d, %d, NULL)"
                       other-root owner)))
        (dorg-commit dolt-dir "add cross-root bnp46 parent-key collision")
        (should-error (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID"))
                      :type 'dorg-ext-rebuild-error)
        (should-error (dorg-test-bnp46--audit dolt-dir root '(:property "BEAD_ID") 'repair)
                      :type 'dorg-ext-rebuild-error)))))

;;; ─── Multi-row insert lowering leaves rebuild SQL unchanged ──────────────

(defconst dorg-test-ext-rebuild--ib-dir
  (expand-file-name "fixtures/insert-batching/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The insert-batching base oracle.")

(require 'dorg-insert-batching-capture
         (expand-file-name "capture" dorg-test-ext-rebuild--ib-dir))
(require 'dorg-insert-batching-reference
         (expand-file-name "reference" dorg-test-ext-rebuild--ib-dir))

(ert-deftest dorg-test-ext-rebuild-sql-is-unchanged-by-insert-lowering ()
  "Control: a BM25 rebuild repair keeps its scalar SQL, in the base sequence.
Rebuild still sends one statement per effect, never lowered.  Its statements
equal the frozen base sequence with exactly one permitted change: the
contiguous block of BM25 document and posting inserts is stably partitioned,
documents first, because the projector now emits them that way.  Every
clear, delete and accumulator statement keeps its base position.  The
repaired store equals the base."
  (unless (and dorg-test--dolt-available dorg-test--server-port)
    (ert-skip "dolt sql-server unavailable"))
  (let* ((golden (cdr (assq :rebuild (plist-get (dorg-ib-read-golden) :scenarios))))
         (actual (cdr (assq :rebuild
                            (dorg-test--with-temp-db dolt-dir
                              (let ((dir (make-temp-file "dorg-ib-" t)))
                                (unwind-protect (dorg-ib-scenarios dolt-dir dir)
                                  (delete-directory dir t)))))))
         (base (car (plist-get golden :commits))))
    (should base)
    (should (equal (list (dorg-ibr-reorder-bm25 base)) (plist-get actual :commits)))
    (should (equal (plist-get golden :dump) (plist-get actual :dump)))))

(provide 'dorg-ext-rebuild-tests)
;;; dorg-ext-rebuild-tests.el ends here
