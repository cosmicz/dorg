;;; dorg-tracker-cli-dou7f-probe-tests.el --- Command composition capability probe -*- lexical-binding: t; -*-

;;; Commentary:

;; Step-zero probe for tracked regression.  This is deliberately a characterization
;; of the pinned public DOrg surface, not an implementation.  It proves which
;; Downstream tracker command legs compose in one apply and whether a later batch member can
;; fill a heading created by an earlier member.

;;; Code:

(require 'ert)
(require 'dorg)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)

(defmacro dorg-test-dou7f--with-dolt (&rest body)
  "Run BODY in a fresh Dolt store with an isolated extension registry."
  (declare (indent 0) (debug (body)))
  `(if (not (bound-and-true-p dorg-test--dolt-available))
       (ert-skip "dolt not installed or unsupported")
     (dorg-test--with-temp-db dolt-dir
       (let ((dorg--extensions nil))
         ,@body))))

(defun dorg-test-dou7f--seed (dolt-dir contents root-id)
  "Seed CONTENTS under ROOT-ID in DOLT-DIR and return ROOT-ID."
  (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql contents root-id))
  (dorg-commit dolt-dir "dou7f probe seed")
  root-id)

(defun dorg-test-dou7f--id (dolt-dir title)
  "Return the heading id titled TITLE in DOLT-DIR."
  (alist-get
   'id
   (car (dorg-sql
         dolt-dir
         (format "SELECT id FROM headings WHERE title = %s LIMIT 1"
                 (dorg-sql-quote title))))))

(defun dorg-test-dou7f--property (heading name)
  "Return property NAME from HEADING's PROPERTIES drawer."
  (cl-loop for item in (dorg-heading-items heading)
           when (and (cl-typep item 'dorg-drawer-item)
                     (equal "PROPERTIES" (dorg-drawer-item-name item)))
           thereis (cdr (assoc name (dorg-drawer-item-properties item)))))

(defun dorg-test-dou7f--text (heading)
  "Return HEADING's concatenated text item content."
  (mapconcat (lambda (item)
               (if (cl-typep item 'dorg-text-item)
                   (dorg-text-item-text item)
                 ""))
             (dorg-heading-items heading) ""))

(defun dorg-test-dou7f--drawer-lines (heading name)
  "Return the first NAME drawer's raw lines from HEADING."
  (when-let ((drawer
              (cl-find-if
               (lambda (item)
                 (and (cl-typep item 'dorg-drawer-item)
                      (equal name (dorg-drawer-item-name item))))
               (dorg-heading-items heading))))
    (dorg-drawer-item-lines drawer)))

(defun dorg-test-dou7f--project (ctx)
  "Return typed probe-table effects for immutable index context CTX."
  (let (effects)
    (dolist (entry (append (dorg-ext-index-context-created ctx)
                           (dorg-ext-index-context-changed ctx)))
      (let ((id (dorg-ext-index-entry-id entry)))
        (push (list :table "dou7f_probe"
                    :delete (list (cons 'heading_id id)))
              effects)
        (push (list :table "dou7f_probe"
                    :insert (list (cons 'heading_id id)))
              effects)))
    (nreverse effects)))

(ert-deftest dorg-test-dou7f-pinned-command-composition-probe ()
  "Probe the complete dou7f command shape in one public apply.

Known-target property/TODO/assignee normalization, LOGBOOK insertion, note and
section appends, and extension projection must all land in one commit.  A
query-targeted content insert aimed at a heading created earlier in the same
batch does not resolve against the simulated postimage on pinned 5ff0cf2; the
heading lands empty.  This proves the bounded rich-constructor gap by executing
the public API, rather than inferring it from grammar text."
  (dorg-test-dou7f--with-dolt
    (dorg-register-extension
     'dou7f-probe
     :schema '("CREATE TABLE dou7f_probe (heading_id BIGINT PRIMARY KEY)")
     :version 1
     :capabilities '(dolt)
     :read-tables '((:name "dou7f_probe" :key-column "heading_id"))
     :project #'dorg-test-dou7f--project)
    (dorg-repo-ensure-extensions dolt-dir)
    (dorg-commit dolt-dir "install dou7f probe extension")
    (let* ((root
            (dorg-test-dou7f--seed
             dolt-dir
             (concat
              "* TODO Task\n"
              ":PROPERTIES:\n:BEAD_ID: probe-task\n:BEAD_ASSIGNED: ,driver\n:END:\n"
              ":LOGBOOK:\n- State \\\"REVIEW\\\" from \\\"SPEC\\\"\n:END:\n"
              "** Notes\nOld note.\n"
              "** Design\nOld design.\n")
             7000))
           (task (dorg-test-dou7f--id dolt-dir "Task"))
           (notes (dorg-test-dou7f--id dolt-dir "Notes"))
           (design (dorg-test-dou7f--id dolt-dir "Design"))
           (source (list :dolt-dir dolt-dir :root-id root
                         :identity '(:property "BEAD_ID")))
           (head0 (dorg-head-commit dolt-dir))
           (form
            `(batch
              (insert (heading (title "Specification")
                               (property "DOU7F_PROBE_KEY" "new-section"))
                      (under (id ,task)))
              (on (where (property "DOU7F_PROBE_KEY" "new-section"))
                  (insert (text) (after-last) "Rich section body."))
              (on (id ,task)
                  (set (heading todo) "IMPL")
                  (set (property "BEAD_ASSIGNED") "driver")
                  (set (property "BEAD_BRANCH_ORIGIN") "SPEC")
                  (insert (drawer-line "LOGBOOK" (occurrence 1))
                          (before-first) "- State \\\"IMPL\\\" from \\\"REVIEW\\\""))
              (on (id ,notes)
                  (insert (text) (after-last) "Appended note."))
              (on (id ,design)
                  (insert (text) (after-last) "Appended section text."))))
           (result
            (dorg-api-execute
             form :mode 'apply :source source :base head0
             :op-id "dou7f-step-zero"
             :expected-targets (list task notes design)))
           (head1 (dorg-head-commit dolt-dir))
           (tree (dorg-model-load-tree dolt-dir root))
           (task-h (gethash task tree))
           (notes-h (gethash notes tree))
           (design-h (gethash design tree))
           (spec-id (dorg-test-dou7f--id dolt-dir "Specification"))
           (spec-h (gethash spec-id tree)))
      ;; One API apply produced exactly one commit and projected its extension.
      (should (plist-get result :ok))
      (should-not (equal head0 head1))
      (should (equal head1 (plist-get result :new-token)))
      (should (= 1 (alist-get 'n (car (dorg-sql
                                      dolt-dir
                                      "SELECT COUNT(*) AS n FROM dolt_log WHERE commit_hash = HASHOF('HEAD')")))))
      (should (> (alist-get 'n (car (dorg-sql dolt-dir
                                             "SELECT COUNT(*) AS n FROM dou7f_probe")))
                 0))
      ;; Every known-target command leg composes successfully.
      (should (equal "IMPL" (dorg-heading-todo task-h)))
      (should (equal "driver"
                     (dorg-test-dou7f--property task-h "BEAD_ASSIGNED")))
      (should (equal "SPEC"
                     (dorg-test-dou7f--property task-h "BEAD_BRANCH_ORIGIN")))
      (should (member "- State \\\"IMPL\\\" from \\\"REVIEW\\\""
                      (dorg-test-dou7f--drawer-lines task-h "LOGBOOK")))
      (should (string-match-p "Appended note\."
                              (dorg-test-dou7f--text notes-h)))
      (should (string-match-p "Appended section text\."
                              (dorg-test-dou7f--text design-h)))
      ;; Capability result: constructor succeeds, same-batch fill does not.
      (should spec-id)
      (should (equal "" (dorg-test-dou7f--text spec-h))))))

(ert-deftest dorg-test-dou7f-rich-constructor-one-apply-red ()
  "A typed rich constructor creates a complete subtree in one apply.

This is the bounded DOrg RED proven necessary by the pinned capability probe.
The constructor carries ordered item forms and recursive child headings; core
and extension projection must commit together exactly once."
  (dorg-test-dou7f--with-dolt
    (dorg-register-extension
     'dou7f-probe
     :schema '("CREATE TABLE dou7f_probe (heading_id BIGINT PRIMARY KEY)")
     :version 1
     :capabilities '(dolt)
     :read-tables '((:name "dou7f_probe" :key-column "heading_id"))
     :project #'dorg-test-dou7f--project)
    (dorg-repo-ensure-extensions dolt-dir)
    (dorg-commit dolt-dir "install dou7f rich probe extension")
    (let* ((dorg-api-broad-write-threshold 1)
           (root (dorg-test-dou7f--seed
                  dolt-dir
                  (concat "* TODO Task\n"
                          ":PROPERTIES:\n:BEAD_ID: probe-task\n:END:\n")
                  7100))
           (task (dorg-test-dou7f--id dolt-dir "Task"))
           (source (list :dolt-dir dolt-dir :root-id root
                         :identity '(:property "BEAD_ID")))
           (head0 (dorg-head-commit dolt-dir))
           (apply-form
            `(insert
              (heading
               (title "Specification")
               (property "AUDIT" "planned")
               (items
                (drawer "DETAILS" :lines ("validated"))
                (text "Rich section body."))
               (children
                (heading (title "Nested detail")
                         (items (text "Nested body."))
                         (children
                          (heading (title "Deep detail")
                                   (items (text "Deep body.")))))
                (heading (title "Second detail")
                         (items (text "Second body.")))))
              (under (id ,task))))
           (result
            (dorg-api-execute
             apply-form :mode 'apply :source source :base head0
             :op-id "dou7f-rich-step-zero"))
           (head1 (dorg-head-commit dolt-dir))
           (replay
            (dorg-api-execute
             apply-form :mode 'apply :source source
             :op-id "dou7f-rich-step-zero"))
           (tree (dorg-model-load-tree dolt-dir root))
           (created (plist-get (plist-get result :affected) :created))
           (parent-id (dorg-test-dou7f--id dolt-dir "Specification"))
           (child-id (dorg-test-dou7f--id dolt-dir "Nested detail"))
           (deep-id (dorg-test-dou7f--id dolt-dir "Deep detail"))
           (second-id (dorg-test-dou7f--id dolt-dir "Second detail"))
           (parent (gethash parent-id tree))
           (child (gethash child-id tree))
           (deep (gethash deep-id tree))
           (second (gethash second-id tree)))
      (should parent-id)
      (should (= 4 (length created)))
      (should (= 4 (length (delete-dups (copy-sequence created)))))
      ;; Literal descendants are finite constructor payload, not additional
      ;; expanded targets for the broad-write threshold.
      (should (> (length created) dorg-api-broad-write-threshold))
      (should child-id)
      (should deep-id)
      (should second-id)
      (should (plist-get result :ok))
      (should-not (equal head0 head1))
      (should (equal head1 (plist-get result :new-token)))
      (should (plist-get replay :idempotent))
      (should (equal head1 (dorg-head-commit dolt-dir)))
      (should (equal "planned" (dorg-test-dou7f--property parent "AUDIT")))
      (should (equal '("validated")
                     (dorg-test-dou7f--drawer-lines parent "DETAILS")))
      (should (string-match-p "Rich section body\."
                              (dorg-test-dou7f--text parent)))
      (should (string-match-p "Nested body\."
                              (dorg-test-dou7f--text child)))
      (should (string-match-p "Deep body\."
                              (dorg-test-dou7f--text deep)))
      (should (string-match-p "Second body\."
                              (dorg-test-dou7f--text second)))
      (should (= parent-id (dorg-heading-parent-id child)))
      (should (= child-id (dorg-heading-parent-id deep)))
      (should (= parent-id (dorg-heading-parent-id second)))
      (should (dorg-sortkey-ordered-p
               (list (dorg-heading-sort-key child)
                     (dorg-heading-sort-key second))))
      (should (= 4 (alist-get 'n (car (dorg-sql
                                      dolt-dir
                                      "SELECT COUNT(*) AS n FROM dou7f_probe"))))))))

(ert-deftest dorg-test-dou7f-rich-constructor-validation-and-plain-org-refusal ()
  "Rich constructors validate recursively and plain Org refuses byte-safely."
  (dolist (form
           '((insert (heading (items (text "one")) (items (text "two")))
                     (under document))
             (insert (heading (children) (children)) (under document))
             (insert (heading (children (heading (title "Child"))
                                         . malformed-tail))
                     (under document))
             (insert (heading (children (text "not a heading")))
                     (under document))
             (insert (heading (children (heading (unknown "clause"))))
                     (under document))
             (insert (heading (children (heading (items (unknown "item")))))
                     (under document))
             (insert (heading (id 99))
                     (under document))))
    (should
     (eq 'invalid-target
         (condition-case err
             (progn (dorg-api-normalize form) :no-error)
           (dorg-api-error (dorg-api-error-reason err))))))
  (let* ((file (make-temp-file "dorg-dou7f-" nil ".org"
                               "* Task\n:PROPERTIES:\n:DORG_ID: 7201\n:END:\n"))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (let ((before (buffer-string))
                (form '(insert
                        (heading (title "Specification")
                                 (items (text "Body"))
                                 (children (heading (title "Child"))))
                        (under document))))
            (should
             (eq 'unsupported
                 (condition-case err
                     (progn
                       (dorg-api-execute form :mode 'apply :source buf)
                       :no-error)
                   (dorg-api-error (dorg-api-error-reason err))
                   (dorg-mutate-error (dorg-mutate-error-reason err)))))
            (should (equal before (buffer-string)))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-file file))))

(ert-deftest dorg-test-dou7f-constructor-clause-shapes-red ()
  "Empty rich clauses stay present; improper item tails fail as typed input."
  (let* ((items-patch
          (car (dorg-api-normalize
                '(insert (heading (title "Empty items") (items))
                         (under document)))))
         (items-heading (plist-get items-patch :heading))
         (children-patch
          (car (dorg-api-normalize
                '(insert (heading (title "Empty children") (children))
                         (under document)))))
         (children-heading (plist-get children-patch :heading)))
    (should (plist-member items-heading :items))
    (should-not (plist-get items-heading :items))
    (should (plist-member children-heading :children))
    (should-not (plist-get children-heading :children)))
  (should
   (eq 'invalid-target
       (condition-case err
           (progn
             (dorg-api-normalize
              '(insert (heading (items (text "Body") . malformed-tail))
                       (under document)))
             :no-error)
         (dorg-api-error (dorg-api-error-reason err))
         (error :untyped-error)))))

(ert-deftest dorg-test-dou7f-recursive-id-pinning-is-load-bearing ()
  "Caller-pinned nodes still recurse; descendants pin once without mutation."
  (let* ((patch
          '(:op insert :target (:tree :under :root)
            :heading (:id 8100 :title "Parent"
                      :children ((:title "Child"
                                  :children ((:id 8300 :title "Grandchild")))))))
         (original (copy-tree patch))
         (generated '(8200))
         pinned)
    (cl-letf (((symbol-function 'dorg-id-generate)
               (lambda () (or (pop generated) (error "unexpected id mint")))))
      (setq pinned (car (dorg-mutate--pin-insert-heading-ids (list patch)))))
    (let* ((parent (plist-get pinned :heading))
           (child (car (plist-get parent :children)))
           (grandchild (car (plist-get child :children))))
      (should (= 8100 (plist-get parent :id)))
      (should (= 8200 (plist-get child :id)))
      (should (= 8300 (plist-get grandchild :id)))
      (should-not generated)
      (should (equal original patch)))))

(provide 'dorg-tracker-cli-dou7f-probe-tests)
;;; dorg-tracker-cli-dou7f-probe-tests.el ends here
