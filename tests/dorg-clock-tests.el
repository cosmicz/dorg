;;; dorg-clock-tests.el --- Persisted clock owner contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the clock owner's immutable request and typed owner boundary.
;; Fixtures are inert at load time; each test owns its candidate images.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-test-helpers)

(defun dorg-test-clock--fresh-load-output (form)
  "Evaluate FORM in a fresh Emacs with this checkout on its load path."
  (let ((root (locate-dominating-file default-directory "dorg-clock.el")))
    (should root)
    (with-temp-buffer
      (let ((status (call-process
                     "emacs" nil (current-buffer) nil "-Q" "--batch"
                     "-L" root
                     "-L" (expand-file-name "lib/clime" root)
                     "-L" (expand-file-name "lib/mysql" root)
                     "--eval" (prin1-to-string form))))
        (should (= 0 status))
        (buffer-string)))))

(ert-deftest dorg-test-clock-load-keeps-dolt-lazy ()
  "Loading the clock participant alone keeps the Dolt backend unloaded."
  (let ((out (dorg-test-clock--fresh-load-output
              '(progn
                 (princ (format "DOLT-BEFORE=%S " (featurep 'dorg-dolt)))
                 (require 'dorg-clock)
                 (princ (format "DOLT-AFTER=%S CLOCK=%S"
                                (featurep 'dorg-dolt)
                                (featurep 'dorg-clock)))))))
    (should (string-match-p "DOLT-BEFORE=nil DOLT-AFTER=nil CLOCK=t" out))))

(ert-deftest dorg-test-clock-history-only-restore-resolves-owner ()
  "A history-only image loads the clock participant at the restore boundary."
  (let ((out (dorg-test-clock--fresh-load-output
              '(progn
                 (require 'dorg-history)
                 (princ (format "CLOCK-BEFORE=%S " (featurep 'dorg-clock)))
                 (let ((current
                        (make-dorg-heading
                         :id 10 :items
                         (list (dorg-drawer-item
                                :name "PROPERTIES"
                                :properties '(("DORG_CLOCK_START" . "old"))))))
                       (target
                        (make-dorg-heading
                         :id 10 :items
                         (list (dorg-drawer-item
                                :name "PROPERTIES"
                                :properties '(("DORG_CLOCK_START" . "new")))))))
                   (condition-case err
                       (progn
                         (dorg-history--owned-metadata-restore-plan
                          current target "base")
                         (princ "PLAN=t "))
                     (dorg-mutate-error
                      (princ (format "DETAIL=%S "
                                     (plist-get (dorg-error-data err) :detail))))))
                 (princ (format "CLOCK-AFTER=%S" (featurep 'dorg-clock)))))))
    (should (string-match-p "CLOCK-BEFORE=nil " out))
    (should (string-match-p "DETAIL=owner-refused " out))
    (should (string-match-p "CLOCK-AFTER=t" out))))

(defun dorg-test-clock--managed-heading (id start)
  "Return a minimal managed heading ID with START for owner-boundary tests."
  (make-dorg-heading
   :id id :level 1 :level-offset 1 :title "Clock"
   :items (list (dorg-drawer-item
                 :name "PROPERTIES"
                 :properties (list (cons "DORG_CLOCK_START" start))))))

(defun dorg-test-clock--record (id parent properties)
  "Return a clock record ID below PARENT with PROPERTIES."
  (make-dorg-heading
   :id id :parent-id parent :level (if parent 2 1)
   :level-offset 1 :title "Clock"
   :items (list (dorg-drawer-item :name "PROPERTIES"
                                  :properties properties))))

(defun dorg-test-clock--image (&optional rogue)
  "Return a complete scope image, with a second open interval if ROGUE."
  (let ((image (make-hash-table :test 'eql)))
    (puthash 71
             (dorg-test-clock--record
              71 nil '(("DORG_CLOCK_KIND" . "scope")
                       ("DORG_CLOCK_ACTIVE_ID" . "81")))
             image)
    (puthash 81
             (dorg-test-clock--record
              81 71 '(("DORG_CLOCK_KIND" . "interval")
                      ("DORG_CLOCK_SCOPE_ID" . "71")
                      ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
                      ("DORG_CLOCK_START_UTC" . "1793536799")
                      ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
             image)
    (when rogue
      (puthash 82
               (dorg-test-clock--record
                82 71 '(("DORG_CLOCK_KIND" . "interval")
                        ("DORG_CLOCK_SCOPE_ID" . "71")
                        ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:10")
                        ("DORG_CLOCK_START_UTC" . "1793536800")
                        ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
               image))
    image))

(defun dorg-test-clock--owned-context ()
  "Return one changed owned heading in isolated before/after images."
  (let* ((before (make-hash-table :test 'eql))
         (after (make-hash-table :test 'eql))
         (old (dorg-test-clock--managed-heading 81 "old"))
         (new (dorg-test-clock--managed-heading 81 "new")))
    (puthash 81 old before)
    (puthash 81 new after)
    (dorg-ext-index-context-create
     :source-kind 'dolt :root-id 71
     :created nil
     :changed (list (dorg-ext-index-entry-create
                     :kind 'change :id 81 :before old :after new))
     :base-image before :candidate-postimage after)))

(defun dorg-test-clock--signal-owner-case (case)
  "Signal CASE for a registered owner callback."
  (pcase case
    ('declared (signal 'dorg-owner-refusal
                       '("DORG_CLOCK_" conflict "typed refusal" (:secret t))))
    ('undeclared (signal 'dorg-owner-refusal
                         '("DORG_CLOCK_" alien "alien refusal" (:secret t))))
    ('generic (error "generic refusal"))))

(defun dorg-test-clock--seed-open-store (dolt-dir)
  "Seed a scope with active interval 80 and return its committed HEAD."
  (dorg-sql-batch
   dolt-dir
   (dorg-test--parse-to-sql
    (concat
     "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n"
     ":DORG_CLOCK_KIND: scope\n:DORG_CLOCK_ACTIVE_ID: 80\n:END:\n"
     "** Current\n:PROPERTIES:\n:DORG_ID: 80\n"
     ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
     ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:8\n"
     ":DORG_CLOCK_START_UTC: 1793536700\n"
     ":DORG_CLOCK_ZONE: America/Los_Angeles\n:END:\n")
    100))
  (dorg-commit dolt-dir "seed open clock")
  (dorg-head-commit dolt-dir))

(defun dorg-test-clock--assert-normalized (err expected)
  "Assert ERR carries EXPECTED closed owner reason and no secret details."
  (let ((data (if (eq (car err) 'dorg-import-target-error)
                  (cddr err)
                (dorg-error-data err))))
    (should (eq 'owner-refused (plist-get data :detail)))
    (should (eq expected (plist-get data :owner-reason)))
    (should (stringp (plist-get data :owner-message)))
    (should-not (plist-member data :secret))))

(ert-deftest dorg-test-clock-owner-digest-ignores-plist-order-and-base ()
  "Equivalent requests have one digest; active preconditions change it."
  (let* ((source '(:dolt-dir "/tmp/clock-digest" :root-id 71))
         (request '(:action switch :scope-id "71" :instant-utc 1793536799
                    :zone "America/Los_Angeles" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active "80"))
         (reordered '(:expected-active "80" :interval-id "81"
                      :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :zone "America/Los_Angeles"
                      :instant-utc 1793536799 :scope-id "71" :action switch))
         (a (dorg-api-make-owner-intent "DORG_CLOCK_" request
                                        :source source :base "old" :op-id "op-a"))
         (b (dorg-api-make-owner-intent "DORG_CLOCK_" reordered
                                        :source source :base "new" :op-id "op-a")))
    (should (equal (dorg-api-owner-intent-digest a)
                   (dorg-api-owner-intent-digest b)))
    (let* ((other (copy-sequence request))
           (changed (progn (plist-put other :expected-active "79") other)))
      (should-not (equal (dorg-api-owner-intent-digest a)
                         (dorg-api-owner-intent-digest
                          (dorg-api-make-owner-intent "DORG_CLOCK_" changed
                                                      :source source
                                                      :base "old"
                                                      :op-id "op-a")))))))

(ert-deftest dorg-test-clock-plain-only-refuses-before-gate ()
  "Constructing a clock intent for plain authority never acquires a lock."
  (let ((gate-called nil))
    (cl-letf (((symbol-function 'dorg-mutate--with-saved-plain-file-gate)
               (lambda (&rest _) (setq gate-called t) (ert-fail "gate ran"))))
      (let ((err (should-error
                  (dorg-clock-make-intent
                   'start :source '(:file "/tmp/task.org") :op-id "op-a"
                   :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                   :interval-id "81" :expected-active nil
                   :instant-utc 1793536799
                   :zone "America/Los_Angeles")
                  :type 'dorg-api-error)))
        (should (eq 'unsupported (dorg-error-reason err)))
        (should-not gate-called)))))

(ert-deftest dorg-test-clock-generic-owner-intent-also-refuses-plain ()
  "The generic owner constructor cannot bypass clock's Dolt-only rule."
  (let ((gate-called nil))
    (cl-letf (((symbol-function 'dorg-mutate--with-saved-plain-file-gate)
               (lambda (&rest _) (setq gate-called t) (ert-fail "gate ran"))))
      (let ((err (should-error
                  (dorg-api-make-owner-intent
                   "DORG_CLOCK_"
                   '(:action start :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                     :interval-id "81" :expected-active nil
                     :instant-utc 1793536799
                     :zone "America/Los_Angeles")
                   :source '(:file "/tmp/task.org") :op-id "op-a")
                  :type 'dorg-api-error)))
        (should (eq 'unsupported (dorg-error-reason err)))
        (should-not gate-called)))))

(ert-deftest dorg-test-clock-lower-refuses-inconsistent-request ()
  "A caller cannot use the generic owner intent to bypass action fields."
  (let ((lower (plist-get (dorg-clock-metadata-owner) :lower)))
    (let ((err (should-error
                (funcall lower
                         '(:action start :scope-id "71"
                           :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :interval-id "81"
                           :expected-active "80" :instant-utc 1793536799
                           :zone "America/Los_Angeles"))
                :type 'dorg-owner-refusal)))
      (should (eq 'invalid-record (nth 2 err))))))

(ert-deftest dorg-test-clock-refuses-unqualified-task-and-zone ()
  "Stored clock identity and zone cannot be a title, path, or POSIX rule."
  (dolist (task '("Meeting" "/tmp/tasks.org" "task:9"))
    (should-error
     (dorg-clock-make-intent
      'start :source '(:dolt-dir "/tmp/store" :root-id 100) :base "h0"
      :op-id "bad-ref" :scope-id "71" :interval-id "81"
      :expected-active nil :task-ref task :instant-utc 1793536799
      :zone "America/Los_Angeles")
     :type 'dorg-api-error))
  (should-error
   (dorg-clock-make-intent
    'start :source '(:dolt-dir "/tmp/store" :root-id 100) :base "h0"
    :op-id "bad-zone" :scope-id "71" :interval-id "81"
    :expected-active nil :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
    :instant-utc 1793536799 :zone "PST8PDT")
   :type 'dorg-api-error))

(ert-deftest dorg-test-clock-task-ref-uses-root-identity-across-paths ()
  "Aliases and copies with one store ID produce one task reference."
  (cl-letf (((symbol-function 'dorg-repo-store-id)
             (lambda (dir)
               (if (equal dir "/tmp/store-b")
                   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
            ((symbol-function 'dorg-mutate--resolve-source)
             (lambda (source)
               (dorg-mutate--source-make
                :kind 'dolt :dolt-dir (plist-get source :dolt-dir)
                :root-id (plist-get source :root-id)))))
    (let* ((first (dorg-clock-task-ref
                   '(:dolt-dir "/tmp/store-a" :root-id 100) "9"))
           (alias (dorg-clock-task-ref
                   '(:dolt-dir "/tmp/store-a-link" :root-id 100) "9"))
           (copy (dorg-clock-task-ref
                  '(:dolt-dir "/tmp/store-copy" :root-id 100) "9"))
           (other (dorg-clock-task-ref
                   '(:dolt-dir "/tmp/store-b" :root-id 200) "9")))
      (should (equal (plist-get first :canonical)
                     (plist-get alias :canonical)))
      (should (equal (plist-get first :canonical)
                     (plist-get copy :canonical)))
      (should-not (equal (plist-get first :canonical)
                         (plist-get other :canonical)))
      (should (equal first
                     (dorg-clock-task-ref-decode
                      (plist-get first :canonical)))))))

(ert-deftest dorg-test-clock-task-ref-plain-edit-keeps-identity ()
  "Unrelated byte edits do not change a plain task's root identity."
  (let ((file (make-temp-file "dorg-clock-ref-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: plain-root\n* Task\n:PROPERTIES:\n"
                    ":DORG_ID: 9\n:END:\n"))
          (let ((before (dorg-clock-task-ref (list :file file) "9")))
            (with-temp-file file
              (insert "#+DORG_STORE_ID: plain-root\n* Task changed\n"
                      ":PROPERTIES:\n:DORG_ID: 9\n:END:\nbody\n"))
            (should (equal (plist-get before :canonical)
                           (plist-get (dorg-clock-task-ref
                                       (list :file file) "9") :canonical)))))
      (delete-file file))))

(ert-deftest dorg-test-clock-task-ref-unidentified-root-is-typed ()
  "No identity keyword produces a typed unresolved value without a write."
  (let ((file (make-temp-file "dorg-clock-ref-unidentified-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"))
          (let ((before (with-temp-buffer
                          (insert-file-contents-literally file)
                          (buffer-string)))
                (ref (dorg-clock-task-ref (list :file file) "9")))
            (should (eq 'unresolved (plist-get ref :kind)))
            (should (eq 'store-unidentified (plist-get ref :reason)))
            (should-not (plist-get ref :canonical))
            (should (equal before
                           (with-temp-buffer
                             (insert-file-contents-literally file)
                             (buffer-string))))))
      (delete-file file))))

(ert-deftest dorg-test-clock-task-ref-resolves-after-authored-keyword ()
  "An authored root keyword identifies a plain task across path aliases."
  (let ((file (make-temp-file "dorg-clock-ref-identify-" nil ".org"))
        (alias (make-temp-name
                (expand-file-name "dorg-clock-ref-link-" temporary-file-directory))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"))
          (make-symbolic-link file alias)
          (should (eq 'unresolved
                      (plist-get (dorg-clock-task-ref (list :file file) "9")
                                 :kind)))
          (should-error
           (dorg-api-execute
            '(on document
                 (insert (keyword "DORG_STORE_ID") (before-first) "tasks"))
            :mode 'apply :source (list :file file) :save t)
           :type 'dorg-api-error)
          (should (eq 'unresolved
                      (plist-get (dorg-clock-task-ref (list :file file) "9")
                                 :kind)))
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n"
                    ":DORG_ID: 9\n:END:\n"))
          (let ((direct (dorg-clock-task-ref (list :file file) "9"))
                (linked (dorg-clock-task-ref (list :file alias) "9")))
            (should (equal "tasks" (plist-get direct :root-identity)))
            (should (equal (plist-get direct :canonical)
                           (plist-get linked :canonical)))))
      (when (file-exists-p alias) (delete-file alias))
      (delete-file file))))

(ert-deftest dorg-test-clock-store-id-identify-and-fork ()
  "Identify/fork changes store identity without editing heading content."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n" 100))
    (dorg-commit dolt-dir "seed task")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (content-before (dorg-sql dolt-dir
                                     "SELECT id, items_hash FROM headings WHERE root_id = 100 ORDER BY id")))
      (should-not (dorg-repo-store-id dolt-dir))
      (should (eq 'store-unidentified
                  (plist-get (dorg-clock-task-ref source "9") :reason)))
      (let* ((identified (dorg-api-identify-store dolt-dir))
             (new (plist-get identified :store-id)))
        (should (string-match-p "\\`[a-f0-9]\\{32\\}\\'" new))
        (should (equal new (dorg-repo-store-id dolt-dir)))
        (should (equal new
                       (plist-get (dorg-clock-task-ref source "9") :store-id)))
        (should-error (dorg-api-identify-store dolt-dir) :type 'dorg-api-error)
        (should (equal content-before
                       (dorg-sql dolt-dir
                                 "SELECT id, items_hash FROM headings WHERE root_id = 100 ORDER BY id")))
        (let* ((forked (dorg-api-identify-store dolt-dir :fork t))
               (fork-id (plist-get forked :store-id))
               (rows (dorg-sql
                      dolt-dir "SELECT value FROM dorg_meta WHERE `key` = %s"
                      (dorg-sql-quote (concat "store-id:prior:" fork-id)))))
          (should-not (equal new fork-id))
          (should (equal new (plist-get forked :previous-id)))
          (should (equal new (alist-get 'value (car rows))))
          (should (equal fork-id (dorg-repo-store-id dolt-dir)))
          (should (equal content-before
                         (dorg-sql dolt-dir
                                   "SELECT id, items_hash FROM headings WHERE root_id = 100 ORDER BY id"))))))))

(ert-deftest dorg-test-clock-store-id-identify-refuses-dirty-working-set ()
  "Identification cannot commit an unrelated pending Dolt change."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let ((head (dorg-head-commit dolt-dir)))
      (dorg-sql dolt-dir
                "INSERT INTO dorg_meta (`key`, value) VALUES ('pending-test', 'keep')")
      (let ((status (dorg-sql dolt-dir "SELECT table_name FROM dolt_status"))
            (err (should-error (dorg-api-identify-store dolt-dir)
                               :type 'dorg-api-error)))
        (should (eq 'conflict (dorg-api-error-reason err)))
        (should (eq 'dirty-working-set
                    (plist-get (dorg-api-error-data err) :detail)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (equal status
                       (dorg-sql dolt-dir "SELECT table_name FROM dolt_status")))
        (should-not (dorg-repo-store-id dolt-dir))))))

(ert-deftest dorg-test-clock-store-id-is-stable-across-alias-and-copy ()
  "A symlink and a copied Dolt store retain identity; new stores differ."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let* ((root (make-temp-file "dorg-clock-store-id-" t))
         (first-dir (expand-file-name "first" root))
         (other-dir (expand-file-name "other" root))
         (alias (expand-file-name "alias" root))
         (copy (expand-file-name "copy" root)))
    (unwind-protect
        (progn
          (dolist (dir (list first-dir other-dir))
            (dorg-sql--with-cli-seam dir
              (dorg-init dir)
              (dorg-sql-batch
               dir (dorg-test--parse-to-sql
                    "* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n" 100))
              (dorg-commit dir "seed task")))
          (make-symbolic-link first-dir alias)
          (copy-directory first-dir copy)
          (let ((first (dorg-sql--with-cli-seam (file-name-as-directory first-dir)
                         (dorg-clock-task-ref
                          (list :dolt-dir first-dir :root-id 100) "9")))
                (linked (dorg-sql--with-cli-seam (file-name-as-directory alias)
                          (dorg-clock-task-ref
                           (list :dolt-dir alias :root-id 100) "9")))
                (copied (dorg-sql--with-cli-seam (file-name-as-directory copy)
                          (dorg-clock-task-ref
                           (list :dolt-dir copy :root-id 100) "9")))
                (other (dorg-sql--with-cli-seam (file-name-as-directory other-dir)
                         (dorg-clock-task-ref
                          (list :dolt-dir other-dir :root-id 100) "9"))))
            (should (equal (plist-get first :canonical)
                           (plist-get linked :canonical)))
            (should (equal (plist-get first :canonical)
                           (plist-get copied :canonical)))
            (should-not (equal (plist-get first :canonical)
                               (plist-get other :canonical)))))
      (delete-directory root t))))

(ert-deftest dorg-test-clock-owner-reason-normalizer-is-closed ()
  "Only declared owner reasons cross the generic error boundary."
  (should (get 'dorg-owner-refusal 'error-conditions))
  (let ((owner '(:reasons (stale-active-interval unsupported))))
    (dolist (cell '((stale-active-interval . stale-active-interval)
                    (unknown-clock-reason . undeclared)))
      (let ((err (condition-case caught
                     (signal 'dorg-owner-refusal
                             (list "DORG_CLOCK_" (car cell) "refused"))
                   (error caught))))
        (should (eq (cdr cell)
                    (plist-get (dorg-owned-metadata-normalize-owner-signal
                                "DORG_CLOCK_" owner err)
                               :owner-reason)))))
    (let ((err (condition-case caught (error "foreign") (error caught))))
      (should-not (plist-get (dorg-owned-metadata-normalize-owner-signal
                              "DORG_CLOCK_" owner err)
                             :owner-reason)))))

(ert-deftest dorg-test-clock-owner-signals-normalize-at-all-entry-sites ()
  "Owner construction and callbacks normalize declared, alien, generic errors."
  (dolist (case '((declared . conflict) (undeclared . undeclared)
                  (generic . nil)))
    (let* ((which (car case))
           (expected (cdr case))
           (context (dorg-test-clock--owned-context))
           (old (dorg-test-clock--managed-heading 81 "old"))
           (new (dorg-test-clock--managed-heading 81 "new")))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :reasons '(conflict unsupported)
                         :validate (lambda (_) (dorg-test-clock--signal-owner-case which))
                         :lower (lambda (_) (dorg-test-clock--signal-owner-case which))
                         :import (lambda (_) (dorg-test-clock--signal-owner-case which))
                         :restore (lambda (_) (dorg-test-clock--signal-owner-case which))))))
        (dorg-test-clock--assert-normalized
         (should-error (dorg-mutate--validate-owned-metadata-context context)
                       :type 'dorg-mutate-error)
         expected)
        (dorg-test-clock--assert-normalized
         (should-error (dorg-mutate--owner-lowering-execute-locked
                        nil "DORG_CLOCK_" '(:action start))
                       :type 'dorg-mutate-error)
         expected)
        (dorg-test-clock--assert-normalized
         (should-error (dorg-import--validate-owned-metadata context)
                       :type 'dorg-import-target-error)
         expected)
        (dorg-test-clock--assert-normalized
         (should-error (dorg-history--owned-metadata-restore-plan old new "h0")
                       :type 'dorg-mutate-error)
         expected))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda () (dorg-test-clock--signal-owner-case which))))
        (dorg-test-clock--assert-normalized
         (should-error (dorg-mutate--validate-owned-metadata-context context)
                       :type 'dorg-mutate-error)
         (unless (eq which 'generic) 'undeclared))
        (dorg-test-clock--assert-normalized
         (should-error (dorg-mutate--owner-lowering-execute-locked
                        nil "DORG_CLOCK_" '(:action start))
                       :type 'dorg-mutate-error)
         (unless (eq which 'generic) 'undeclared))
        (dorg-test-clock--assert-normalized
         (should-error (dorg-import--validate-owned-metadata context)
                       :type 'dorg-import-target-error)
         (unless (eq which 'generic) 'undeclared))
        (dorg-test-clock--assert-normalized
         (should-error (dorg-history--owned-metadata-restore-plan old new "h0")
                       :type 'dorg-mutate-error)
         (unless (eq which 'generic) 'undeclared))))))

(ert-deftest dorg-test-clock-owner-route-uses-one-locked-seam ()
  "The API owner route enters the gate once and returns the public owner form."
  (let* ((source '(:dolt-dir "/tmp/clock-route" :root-id 71))
         (request '(:scope-id "71" :action start :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active nil
                    :instant-utc 1793536799 :zone "America/Los_Angeles"))
         (intent (dorg-api-make-owner-intent "DORG_CLOCK_" request
                                              :source source :base "h0"
                                              :op-id "op-a"))
         (gates 0)
         (lowered 0))
    (cl-letf (((symbol-function 'dorg-mutate--with-saved-plain-file-gate)
               (lambda (_source _mode _options fn &optional _raise)
                 (cl-incf gates) (funcall fn)))
              ((symbol-function 'dorg-mutate--owner-lowering-execute-locked)
               (lambda (_source family owned-request &rest options)
                 (cl-incf lowered)
                 (should (equal "DORG_CLOCK_" family))
                 (should (equal request owned-request))
                 (should (equal "h0" (plist-get options :base-commit)))
                 (should (stringp (plist-get options :intent-digest)))
                 (list :ok t :dry-run t :source-kind 'dolt
                       :base-commit "h0" :head-commit "h0"
                       :created nil :changed nil :deleted nil))))
      (let ((result (dorg-api-execute intent :mode 'dry-run)))
        (should (= 1 gates))
        (should (= 1 lowered))
        (should (equal `(owner "DORG_CLOCK_" ,request)
                       (car (plist-get result :intent))))))))

(ert-deftest dorg-test-clock-owner-request-option-is-forbidden ()
  "A caller option cannot select restore mode, even when its value is nil."
  (let ((validated nil))
    (cl-letf (((symbol-function 'dorg-clock--validate)
               (lambda (_) (setq validated t) (ert-fail "owner ran"))))
      (dolist (value '(nil (:action restore)))
        (let ((err (should-error
                    (dorg-mutate--apply-patches
                     '(:file "/tmp/nonexistent-clock-owner.org") nil nil
                     :owner-request value)
                    :type 'dorg-mutate-error)))
          (should (eq 'owner-request-forged
                      (plist-get (dorg-error-data err) :detail)))))
      (should-error
       (dorg-mutate--owner-lowering-execute-locked
        nil "DORG_CLOCK_" '(:action start)
        :owner-request '(:action restore))
       :type 'dorg-mutate-error)
      (should-not validated))))

(ert-deftest dorg-test-clock-nested-owner-validation-has-no-plan-authority ()
  "A nested simulation in owner validation gets no inherited clock request."
  (let* ((request '(:action restore :scope-id "71"))
         (cap (dorg-mutate--owner-capability--make
               :family "DORG_CLOCK_" :request request
               :execution-id (make-symbol "clock-owner")))
         (plan (list :owner-capabilities (list cap)
                     :owner-execution-ids
                     (list (dorg-mutate--owner-capability-execution-id cap))))
         (nested nil))
    (should (equal request (dorg-mutate--planning-owner-request plan)))
    (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
               (lambda ()
                 (list :validate
                       (lambda (_context)
                         (setq nested
                               (catch 'nested
                                 (dorg-mutate--dolt-simulate
                                  (dorg-mutate--source-make
                                   :kind 'dolt :dolt-dir "/tmp/other-store"
                                   :root-id 200)
                                  nil nil)))))))
              ((symbol-function 'dorg-head-commit)
               (lambda (_dir)
                 (should-not (dorg-mutate--planning-owner-request nil))
                 (throw 'nested t))))
      (dorg-mutate--validate-owned-metadata-context
       (dorg-test-clock--owned-context)))
    (should nested)))

(ert-deftest dorg-test-clock-planning-request-uses-the-verified-family ()
  "A second owner request is selected, while two requests refuse ambiguity."
  (let* ((other (dorg-mutate--owner-capability--make
                 :family "DORG_OTHER_" :request '(:scope-id "72")
                 :execution-id (make-symbol "other-owner")))
         (clock (dorg-mutate--owner-capability--make
                 :family "DORG_CLOCK_" :request '(:scope-id "71")
                 :execution-id (make-symbol "clock-owner")))
         (plan (list :owner-capabilities (list other)
                     :owner-execution-ids
                     (list (dorg-mutate--owner-capability-execution-id other)))))
    (should (equal '(:scope-id "72")
                   (dorg-mutate--planning-owner-request plan)))
    (setq plan (list :owner-capabilities (list other clock)
                     :owner-execution-ids
                     (list (dorg-mutate--owner-capability-execution-id other)
                           (dorg-mutate--owner-capability-execution-id clock))))
    (should-error (dorg-mutate--planning-owner-request plan)
                  :type 'dorg-mutate-error)))

(ert-deftest dorg-test-clock-conflict-classifier-reason-is-closed ()
  "An owner classifier cannot signal an undeclared API reason."
  (let ((err (should-error
              (dorg-api--raise-owner-classification
               '(:reason alien :message "unlisted" :data nil))
              :type 'dorg-api-error)))
    (should (eq 'malformed (dorg-api-error-reason err)))
    (should (eq 'undeclared-owner-reason
                (plist-get (dorg-api-error-data err) :detail))))
  (let ((err (should-error
              (dorg-api--raise-owner-classification
               '(:reason stale-active-interval :message "stale"
                 :data (:commit "h2")))
              :type 'dorg-api-error)))
    (should (eq 'stale-active-interval (dorg-api-error-reason err)))
    (should (equal "h2" (plist-get (dorg-api-error-data err) :commit)))))

(ert-deftest dorg-test-clock-owner-import-refuses-rogue-open-child ()
  "Full import validates all children; removing validation makes it vacuous."
  (let* ((owner (dorg-clock-metadata-owner))
         (validate (plist-get owner :import))
         (base (make-hash-table :test 'eql))
         (clean (dorg-ext-index-context-create
                 :source-kind 'dolt :root-id 1
                 :base-image base :candidate-postimage
                 (dorg-test-clock--image)))
         (corrupt (dorg-ext-index-context-create
                   :source-kind 'dolt :root-id 1
                   :base-image base :candidate-postimage
                   (dorg-test-clock--image t))))
    (should (functionp validate))
    (should-not (funcall validate clean))
    (should-error (funcall validate corrupt) :type 'dorg-owner-refusal)))

(ert-deftest dorg-test-clock-import-refuses-closed-orphan ()
  "A closed interval still needs its canonical scope parent on import."
  (let* ((image (dorg-test-clock--image))
         (child (gethash 81 image)))
    (remhash 71 image)
    (setf (dorg-drawer-item-properties (car (dorg-heading-items child)))
          '(("DORG_CLOCK_KIND" . "interval")
            ("DORG_CLOCK_SCOPE_ID" . "71")
            ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
            ("DORG_CLOCK_START_UTC" . "1793536799")
            ("DORG_CLOCK_END_UTC" . "1793536899")
            ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
    (let ((context (dorg-ext-index-context-create
                    :source-kind 'dolt :root-id 1
                    :base-image (make-hash-table :test 'eql)
                    :candidate-postimage image)))
      (should-error (dorg-clock--import context) :type 'dorg-owner-refusal))))

(ert-deftest dorg-test-clock-owner-lower-start-is-pure-and-transforming ()
  "Start lowering is repeatable and creates a child plus active pointer."
  (let* ((lower (plist-get (dorg-clock-metadata-owner) :lower))
         (request '(:scope-id "71" :action start :instant-utc 1793536799
                    :zone "America/Los_Angeles" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active nil
                    :correction nil :adoption nil))
         (first (funcall lower request))
         (second (funcall lower request))
         (patches (plist-get first :patches)))
    (should (equal first second))
    (should (= 2 (length patches)))
    (should (equal '(insert put) (mapcar (lambda (p) (plist-get p :op)) patches)))
    (should (equal "81" (plist-get (cadr patches) :value)))))

(ert-deftest dorg-test-clock-start-persists-and-same-op-replays ()
  "A real authority gains one child and one pointer; retry writes nothing."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (head (dorg-head-commit dolt-dir))
           (intent (dorg-clock-make-intent
                    'start :source source :base head :op-id "clock-start-81"
                    :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active nil
                    :instant-utc 1793536799 :zone "America/Los_Angeles"))
           (before (dorg-model-load-tree dolt-dir 100))
           (first (dorg-api-execute intent :mode 'apply))
           (committed-head (dorg-head-commit dolt-dir))
           (after (dorg-model-load-tree dolt-dir 100))
           (replay (dorg-api-execute intent :mode 'apply)))
      (should-not (gethash 81 before))
      (should (gethash 81 after))
      (should (equal "81" (cdr (assoc "DORG_CLOCK_ACTIVE_ID"
                                     (dorg-model-heading-property-snapshot
                                      (gethash 71 after))))))
      (should-not (equal head committed-head))
      (should (equal committed-head (dorg-head-commit dolt-dir)))
      (should (plist-get replay :idempotent))
      (should (equal committed-head (plist-get first :new-token)))
      (should (equal committed-head (plist-get replay :new-token)))
      (should (equal "71" (plist-get replay :scope-id)))
      (should (equal "81" (plist-get replay :interval-id)))
      (should (equal (dorg-api-owner-intent-digest intent)
                     (plist-get replay :intent-digest))))))

(ert-deftest dorg-test-clock-start-refuses-unpointed-open-child-at-base ()
  "A sparse scope load cannot conceal an already-open sibling."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      (concat
       "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n"
       ":DORG_CLOCK_KIND: scope\n:END:\n"
       "** Rogue open\n:PROPERTIES:\n:DORG_ID: 80\n"
       ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
       ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:8\n"
       ":DORG_CLOCK_START_UTC: 1793536700\n"
       ":DORG_CLOCK_ZONE: America/Los_Angeles\n:END:\n")
      100))
    (dorg-commit dolt-dir "seed corrupted clock")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (head (dorg-head-commit dolt-dir))
           (intent (dorg-clock-make-intent
                    'start :source source :base head :op-id "clock-start-81"
                    :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active nil
                    :instant-utc 1793536799 :zone "America/Los_Angeles"))
           (err (should-error (dorg-api-execute intent :mode 'apply)
                              :type 'dorg-api-error)))
      (should (eq 'invariant (dorg-error-reason err)))
      (should (equal head (dorg-head-commit dolt-dir)))
      (should-not (gethash 81 (dorg-model-load-tree dolt-dir 100))))))

(ert-deftest dorg-test-clock-start-refuses-active-scope-without-switch ()
  "A valid active pointer requires switch; start leaves bytes unchanged."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((head (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'start :source source :base head :op-id "bad-start"
                    :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active nil
                    :instant-utc 1793536799 :zone "America/Los_Angeles"))
           (err (should-error (dorg-api-execute intent :mode 'apply)
                              :type 'dorg-api-error)))
      (should (eq 'stale-active-interval (dorg-error-reason err)))
      (should (equal head (dorg-head-commit dolt-dir)))
      (should-not (gethash 81 (dorg-model-load-tree dolt-dir 100))))))

(ert-deftest dorg-test-clock-switch-closes-old-and-opens-new-atomically ()
  "Switch at one instant changes both intervals and the pointer."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((head (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'switch :source source :base head :op-id "switch-80-81"
                    :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active "80"
                    :instant-utc 1793536799 :zone "America/Los_Angeles"))
           (before (dorg-model-load-tree dolt-dir 100))
           (result (dorg-api-execute intent :mode 'apply))
           (after (dorg-model-load-tree dolt-dir 100)))
      (should-not (gethash 81 before))
      (should (gethash 81 after))
      (should (equal "1793536799"
                     (cdr (assoc "DORG_CLOCK_END_UTC"
                                 (dorg-model-heading-property-snapshot
                                  (gethash 80 after))))))
      (should (equal "81"
                     (cdr (assoc "DORG_CLOCK_ACTIVE_ID"
                                 (dorg-model-heading-property-snapshot
                                  (gethash 71 after))))))
      (should-not (equal head (plist-get result :new-token))))))

(ert-deftest dorg-test-clock-start-replay-retains-original-commit-after-switch ()
  "A stale-base retry after later work returns the first commit and no write."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir))
           (start (dorg-clock-make-intent
                   'start :source source :base base :op-id "start-81"
                   :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                   :interval-id "81" :expected-active nil
                   :instant-utc 1793536799 :zone "America/Los_Angeles"))
           (first (dorg-api-execute start :mode 'apply))
           (start-head (dorg-head-commit dolt-dir))
           (switch (dorg-clock-make-intent
                    'switch :source source :base start-head
                    :op-id "switch-81-82" :scope-id "71"
                    :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:10" :interval-id "82"
                    :expected-active "81" :instant-utc 1793536900
                    :zone "America/Los_Angeles")))
      (dorg-api-execute switch :mode 'apply)
      (let* ((later-head (dorg-head-commit dolt-dir))
             (replay (dorg-api-execute start :mode 'apply)))
        (should (not (equal start-head later-head)))
        (should (plist-get replay :idempotent))
        (should (equal start-head (plist-get first :new-token)))
        (should (equal start-head (plist-get replay :new-token)))
        (should (equal later-head (dorg-head-commit dolt-dir)))))))

(ert-deftest dorg-test-clock-read-uses-one-frozen-snapshot-and-observation ()
  "Read derives elapsed seconds from its supplied instant without a write."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (before (dorg-head-commit dolt-dir))
           (read (dorg-clock-read "71" :source source :snapshot base
                                  :observed-at 1793536799)))
      (should (equal base (plist-get read :snapshot-token)))
      (should (= 1793536799 (plist-get read :observed-at)))
      (should (equal "80" (plist-get read :interval-id)))
      (should (= 99 (plist-get read :elapsed-seconds)))
      (should (equal before (dorg-head-commit dolt-dir)))
      (let ((stop (dorg-clock-make-intent
                   'stop :source source :base base :op-id "read-stop-80"
                   :scope-id "71" :interval-id "80" :expected-active "80"
                   :instant-utc 1793536800 :zone "America/Los_Angeles")))
        (dorg-api-execute stop :mode 'apply))
      (should (equal "80" (plist-get
                           (dorg-clock-read "71" :source source :snapshot base
                                            :observed-at 1793536799)
                           :interval-id)))
      (should-not (plist-get
                   (dorg-clock-read "71" :source source
                                    :observed-at 1793536900)
                   :interval-id)))))

(ert-deftest dorg-test-clock-read-refuses-rogue-open-child ()
  "Read does not trust a pointer if a second child is open at that revision."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      (concat
       "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n"
       ":DORG_CLOCK_KIND: scope\n:DORG_CLOCK_ACTIVE_ID: 80\n:END:\n"
       "** Current\n:PROPERTIES:\n:DORG_ID: 80\n"
       ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
       ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:8\n"
       ":DORG_CLOCK_START_UTC: 1793536700\n"
       ":DORG_CLOCK_ZONE: America/Los_Angeles\n:END:\n"
       "** Rogue\n:PROPERTIES:\n:DORG_ID: 81\n"
       ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
       ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9\n"
       ":DORG_CLOCK_START_UTC: 1793536701\n"
       ":DORG_CLOCK_ZONE: America/Los_Angeles\n:END:\n")
      100))
    (dorg-commit dolt-dir "seed rogue clock")
    (let ((err (should-error
                (dorg-clock-read "71"
                                 :source (list :dolt-dir dolt-dir :root-id 100)
                                 :observed-at 1793536799)
                :type 'dorg-api-error)))
      (should (eq 'invariant (dorg-error-reason err))))))

(ert-deftest dorg-test-clock-cancel-active-clears-pointer-without-end ()
  "Cancellation is an audited transition and does not forge an end instant."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'cancel :source source :base base :op-id "cancel-80"
                    :scope-id "71" :interval-id "80" :expected-active "80"
                    :instant-utc 1793536799 :zone "America/Los_Angeles")))
      (dorg-api-execute intent :mode 'apply)
      (let* ((after (dorg-model-load-tree dolt-dir 100))
             (scope (dorg-model-heading-property-snapshot (gethash 71 after)))
             (child (dorg-model-heading-property-snapshot (gethash 80 after))))
        (should-not (assoc "DORG_CLOCK_ACTIVE_ID" scope))
        (should (equal "t" (cdr (assoc "DORG_CLOCK_CANCELLED" child))))
        (should-not (assoc "DORG_CLOCK_END_UTC" child))))))

(ert-deftest dorg-test-clock-correct-start-preserves-active-pointer ()
  "A correction changes exact seconds while the active identity stays put."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'correct :source source :base base :op-id "correct-80"
                    :scope-id "71" :interval-id "80" :expected-active "80"
                    :instant-utc 1793536799 :zone "America/Los_Angeles"
                    :correction '(:start-utc 1793536601))))
      (dorg-api-execute intent :mode 'apply)
      (let* ((after (dorg-model-load-tree dolt-dir 100))
             (scope (dorg-model-heading-property-snapshot (gethash 71 after)))
             (child (dorg-model-heading-property-snapshot (gethash 80 after))))
        (should (equal "80" (cdr (assoc "DORG_CLOCK_ACTIVE_ID" scope))))
        (should (equal "1793536601"
                       (cdr (assoc "DORG_CLOCK_START_UTC" child))))))))

(ert-deftest dorg-test-clock-correct-stale-active-refuses ()
  "A correction cannot bypass the exact expected pointer precondition."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'correct :source source :base base :op-id "correct-wrong"
                    :scope-id "71" :interval-id "80" :expected-active nil
                    :instant-utc 1793536799 :zone "America/Los_Angeles"
                    :correction '(:start-utc 1793536601)))
           (err (should-error (dorg-api-execute intent :mode 'apply)
                              :type 'dorg-api-error)))
      (should (eq 'stale-active-interval (dorg-error-reason err)))
      (should (equal base (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-clock-restore-refuses-active-resurrection ()
  "A whole-scope historical restore cannot reopen a settled old interval."
  (let* ((current (dorg-test-clock--image))
         (target (dorg-test-clock--image))
         (current-scope (gethash 71 current))
         (current-child (gethash 81 current)))
    (setf (dorg-drawer-item-properties (car (dorg-heading-items current-scope)))
          '(("DORG_CLOCK_KIND" . "scope")))
    (setf (dorg-drawer-item-properties (car (dorg-heading-items current-child)))
          '(("DORG_CLOCK_KIND" . "interval")
            ("DORG_CLOCK_SCOPE_ID" . "71")
            ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
            ("DORG_CLOCK_START_UTC" . "1793536799")
            ("DORG_CLOCK_END_UTC" . "1793536899")
            ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
    (should-error
     (dorg-clock--restore
      (list :identity '(:heading :id 71)
            :before (dorg-clock--properties current-scope)
            :after (dorg-clock--properties (gethash 71 target))
            :current-image current :target-image target
            :scope-ids '(71 81) :base "h0"))
     :type 'dorg-owner-refusal)))

(ert-deftest dorg-test-clock-restore-closed-correction-produces-patches ()
  "A complete settled scope restores changed metadata through typed patches."
  (let* ((current (dorg-test-clock--image))
         (target (dorg-test-clock--image))
         (scope (gethash 71 current))
         (old (gethash 81 current))
         (new (gethash 81 target)))
    (dolist (image (list current target))
      (setf (dorg-drawer-item-properties
             (car (dorg-heading-items (gethash 71 image))))
            '(("DORG_CLOCK_KIND" . "scope"))))
    (setf (dorg-drawer-item-properties (car (dorg-heading-items old)))
          '(("DORG_CLOCK_KIND" . "interval")
            ("DORG_CLOCK_SCOPE_ID" . "71")
            ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
            ("DORG_CLOCK_START_UTC" . "1793536799")
            ("DORG_CLOCK_END_UTC" . "1793536899")
            ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
    (setf (dorg-drawer-item-properties (car (dorg-heading-items new)))
          '(("DORG_CLOCK_KIND" . "interval")
            ("DORG_CLOCK_SCOPE_ID" . "71")
            ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
            ("DORG_CLOCK_START_UTC" . "1793536700")
            ("DORG_CLOCK_END_UTC" . "1793536899")
            ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
    (let* ((result (dorg-clock--restore
                    (list :identity '(:heading :id 81)
                          :before (dorg-clock--properties old)
                          :after (dorg-clock--properties new)
                          :current-image current :target-image target
                          :scope-ids '(71 81) :base "h0")))
           (patches (plist-get result :patches)))
      (should (= 1 (length patches)))
      (should (equal "1793536700" (plist-get (car patches) :value)))
      (ignore scope))))

(ert-deftest dorg-test-clock-history-restores-settled-metadata ()
  "Real history restore retains interval identity and changes only its start."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      (concat
       "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n"
       ":DORG_CLOCK_KIND: scope\n:END:\n"
       "** Settled\n:PROPERTIES:\n:DORG_ID: 80\n"
       ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
       ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:8\n"
       ":DORG_CLOCK_START_UTC: 1793536700\n"
       ":DORG_CLOCK_END_UTC: 1793536800\n"
       ":DORG_CLOCK_ZONE: America/Los_Angeles\n:END:\n")
      100))
    (dorg-commit dolt-dir "seed settled clock")
    (let* ((base (dorg-head-commit dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'correct :source source :base base :op-id "correct-closed"
                    :scope-id "71" :interval-id "80" :expected-active nil
                    :instant-utc 1793536900 :zone "America/Los_Angeles"
                    :correction '(:start-utc 1793536600))))
      (dorg-api-execute intent :mode 'apply)
      (should (equal "1793536600"
                     (cdr (assoc "DORG_CLOCK_START_UTC"
                                 (dorg-model-heading-property-snapshot
                                  (gethash 80 (dorg-model-load-tree dolt-dir 100)))))))
      (dorg-restore-to-commit dolt-dir 100 nil base)
      (let ((after (dorg-model-load-tree dolt-dir 100)))
        (should (equal "1793536700"
                       (cdr (assoc "DORG_CLOCK_START_UTC"
                                   (dorg-model-heading-property-snapshot
                                    (gethash 80 after))))))
        (should (gethash 80 after))))))

(ert-deftest dorg-test-clock-history-refuses-removal-of-running-record ()
  "Restoring a scope before start must not delete the active interval."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((base (dorg-head-commit dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (start (dorg-clock-make-intent
                   'start :source source :base base :op-id "restore-start-81"
                   :scope-id "71" :interval-id "81" :expected-active nil
                   :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :instant-utc 1793536799
                   :zone "America/Los_Angeles")))
      (dorg-api-execute start :mode 'apply)
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error
                   (dorg-restore-to-commit dolt-dir 100 nil base)
                   :type 'dorg-mutate-error))
             (after (dorg-model-load-tree dolt-dir 100)))
        (should (eq 'stale-active-interval
                    (plist-get (dorg-error-data err) :owner-reason)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (gethash 81 after))
        (should (equal "81"
                       (cdr (assoc "DORG_CLOCK_ACTIVE_ID"
                                   (dorg-model-heading-property-snapshot
                                    (gethash 71 after))))))))))

(ert-deftest dorg-test-clock-history-refuses-deleting-settled-record ()
  "A historical restore cannot erase a closed managed interval either."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((base (dorg-head-commit dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (start (dorg-clock-make-intent
                   'start :source source :base base :op-id "settled-start-81"
                   :scope-id "71" :interval-id "81" :expected-active nil
                   :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :instant-utc 1793536799
                   :zone "America/Los_Angeles")))
      (dorg-api-execute start :mode 'apply)
      (let* ((started (dorg-head-commit dolt-dir))
             (stop (dorg-clock-make-intent
                    'stop :source source :base started :op-id "settled-stop-81"
                    :scope-id "71" :interval-id "81" :expected-active "81"
                    :instant-utc 1793536900 :zone "America/Los_Angeles")))
        (dorg-api-execute stop :mode 'apply))
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error
                   (dorg-restore-to-commit dolt-dir 100 nil base)
                   :type 'dorg-mutate-error))
             (after (dorg-model-load-tree dolt-dir 100)))
        (should (eq 'conflict (plist-get (dorg-error-data err) :owner-reason)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (gethash 81 after))))))

(ert-deftest dorg-test-clock-owner-dry-run-and-apply-use-same-plan ()
  "Dry-run returns a typed plan; apply commits once with the same patches."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((base (dorg-head-commit dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'start :source source :base base :op-id "dry-start-81"
                    :scope-id "71" :interval-id "81" :expected-active nil
                    :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :instant-utc 1793536799
                    :zone "America/Los_Angeles"))
           (dry (dorg-api-execute intent :mode 'dry-run)))
      (should (equal base (dorg-head-commit dolt-dir)))
      (let ((applied (dorg-api-execute intent :mode 'apply)))
        (should (equal (plist-get dry :patches)
                       (plist-get applied :patches)))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (should (plist-get (dorg-api-execute intent :mode 'apply)
                           :idempotent))))))

(ert-deftest dorg-test-clock-render-floors-dst-wall-minutes ()
  "Canonical seconds remain exact across the repeated local 01:00 hour."
  (let* ((start 1793523599)          ; 2026-11-01 08:59:59 UTC
         (end 1793523600)            ; 2026-11-01 09:00:00 UTC
         (line (dorg-clock-render-interval
                start end "America/Los_Angeles")))
    (should (equal
             "CLOCK: [2026-11-01 Sun 01:59]--[2026-11-01 Sun 01:00] => 0:00"
             line))
    (should (= 1 (- end start)))
    (should (equal "CLOCK: [2026-11-01 Sun 01:59]"
                   (dorg-clock-render-interval
                    start nil "America/Los_Angeles")))))

(ert-deftest dorg-test-clock-reused-op-id-with-changed-active-refuses ()
  "Expected-active belongs to the receipt digest even when patches can match."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (common (list :source source :base base :op-id "correct-one-id"
                         :scope-id "71" :interval-id "80"
                         :instant-utc 1793536799
                         :zone "America/Los_Angeles"
                         :correction '(:start-utc 1793536601)))
           (first (apply #'dorg-clock-make-intent
                         'correct (append common '(:expected-active "80"))))
           (changed (apply #'dorg-clock-make-intent
                           'correct (append common '(:expected-active nil)))))
      (dorg-api-execute first :mode 'apply)
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error (dorg-api-execute changed :mode 'apply)
                                :type 'dorg-api-error)))
        (should (eq 'idempotency-key-mismatch (dorg-error-reason err)))
        (should (equal head (dorg-head-commit dolt-dir)))))))

(ert-deftest dorg-test-clock-stale-stop-after-switch-keeps-successor ()
  "A delayed stop of predecessor 80 cannot close successor 81."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let* ((base (dorg-test-clock--seed-open-store dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (switch (dorg-clock-make-intent
                    'switch :source source :base base :op-id "switch-80-81"
                    :scope-id "71" :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9"
                    :interval-id "81" :expected-active "80"
                    :instant-utc 1793536799 :zone "America/Los_Angeles")))
      (dorg-api-execute switch :mode 'apply)
      (let* ((head (dorg-head-commit dolt-dir))
             (stop (dorg-clock-make-intent
                    'stop :source source :base head :op-id "late-stop-80"
                    :scope-id "71" :interval-id "80"
                    :expected-active "80" :instant-utc 1793536900
                    :zone "America/Los_Angeles"))
             (err (should-error (dorg-api-execute stop :mode 'apply)
                                :type 'dorg-api-error))
             (after (dorg-model-load-tree dolt-dir 100)))
        (should (eq 'stale-active-interval (dorg-error-reason err)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (equal "81"
                       (cdr (assoc "DORG_CLOCK_ACTIVE_ID"
                                   (dorg-model-heading-property-snapshot
                                    (gethash 71 after))))))))))

(ert-deftest dorg-test-clock-adoption-parses-only-unique-wall-minutes ()
  "Legacy wall minutes need a single UTC instant in the named IANA zone."
  (let* ((raw "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:05")
         (parsed (dorg-clock--parse-legacy-clock raw "America/Los_Angeles")))
    (should (= 1793466000 (plist-get parsed :start-utc)))
    (should (= 1793466300 (plist-get parsed :end-utc)))
    (should (equal raw (plist-get parsed :raw))))
  (dolist (raw '("CLOCK: [2026-11-01 Sun 01:30]"
                 "CLOCK: [2026-03-08 Sun 02:30]"
                 "CLOCK: [2026-11-01 Sun 01:59]--[2026-11-01 Sun 01:00] => 0:01"
                 "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:06"))
    (should-error (dorg-clock--parse-legacy-clock
                   raw "America/Los_Angeles")
                  :type 'dorg-owner-refusal)))

(ert-deftest dorg-test-clock-adopt-closed-plain-line-preserves-source ()
  "Adoption stores qualified provenance and leaves the plain source byte equal."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let* ((file (make-temp-file "dorg-clock-adopt-" nil ".org"))
         (raw "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:05")
         (body (concat "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                       ":LOGBOOK:\n" raw "\n:END:\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert body))
          (let ((digest (with-temp-buffer
                          (insert-file-contents-literally file)
                          (secure-hash 'sha256 (current-buffer)))))
            (dorg-test--with-temp-db dolt-dir
              (dorg-sql-batch
               dolt-dir
               (dorg-test--parse-to-sql
                "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
                100))
              (dorg-commit dolt-dir "seed clock scope")
              (let* ((base (dorg-head-commit dolt-dir))
                     (source (list :dolt-dir dolt-dir :root-id 100))
                     (intent (dorg-clock-make-intent
                              'adopt :source source :base base :op-id "adopt-81"
                              :scope-id "71" :interval-id "81"
                              :expected-active nil
                              :task-ref (plist-get
                                         (dorg-clock-task-ref (list :file file) "9")
                                         :canonical)
                              :instant-utc 1793466400
                              :zone "America/Los_Angeles"
                              :adoption (list :source (list :file file)
                                              :revision digest :heading-id "9"
                                              :occurrence 0 :raw raw)))
                     (_ (dorg-api-execute intent :mode 'apply))
                     (after (dorg-model-load-tree dolt-dir 100))
                     (props (dorg-model-heading-property-snapshot
                             (gethash 81 after))))
                (should (equal "1793466000"
                               (cdr (assoc "DORG_CLOCK_START_UTC" props))))
                (should (equal "1793466300"
                               (cdr (assoc "DORG_CLOCK_END_UTC" props))))
                (should (string-prefix-p
                         "v1:" (cdr (assoc "DORG_CLOCK_ADOPTION" props))))
                (should (equal body
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))))))))
      (delete-file file))))

(ert-deftest dorg-test-clock-adopt-shifted-occurrence-twice-refuses ()
  "Moving a claimed raw line to a new occurrence cannot count it twice."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let* ((file (make-temp-file "dorg-clock-adopt-twice-" nil ".org"))
         (raw "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:05"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                    ":LOGBOOK:\n" raw "\n:END:\n"))
          (let ((digest (with-temp-buffer
                          (insert-file-contents-literally file)
                          (secure-hash 'sha256 (current-buffer)))))
            (dorg-test--with-temp-db dolt-dir
              (dorg-sql-batch
               dolt-dir
               (dorg-test--parse-to-sql
                "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
                100))
              (dorg-commit dolt-dir "seed clock scope")
              (let* ((source (list :dolt-dir dolt-dir :root-id 100))
                     (adoption (list :source (list :file file)
                                     :revision digest :heading-id "9"
                                     :occurrence 0 :raw raw))
                     (task (plist-get
                            (dorg-clock-task-ref (list :file file) "9")
                            :canonical)))
                (dorg-api-execute
                 (dorg-clock-make-intent
                  'adopt :source source :base (dorg-head-commit dolt-dir)
                  :op-id "adopt-once" :scope-id "71" :interval-id "81"
                  :expected-active nil :task-ref task :instant-utc 1793466400
                  :zone "America/Los_Angeles" :adoption adoption)
                 :mode 'apply)
                (with-temp-buffer
                  (insert-file-contents-literally file)
                  (goto-char (point-min))
                  (search-forward ":LOGBOOK:\n")
                  (insert "CLOCK: [2026-10-31 Sat 09:00]--[2026-10-31 Sat 09:02] => 0:02\n")
                  (write-region (point-min) (point-max) file nil 'silent))
                (setq adoption
                      (plist-put
                       (plist-put (copy-sequence adoption) :occurrence 1)
                       :revision
                       (with-temp-buffer
                         (insert-file-contents-literally file)
                         (secure-hash 'sha256 (current-buffer)))))
                (let ((head (dorg-head-commit dolt-dir)))
                  (should-error
                   (dorg-api-execute
                    (dorg-clock-make-intent
                     'adopt :source source :base head :op-id "adopt-again"
                     :scope-id "71" :interval-id "82"
                     :expected-active nil :task-ref task
                     :instant-utc 1793466500 :zone "America/Los_Angeles"
                     :adoption adoption)
                    :mode 'apply)
                   :type 'dorg-api-error)
                  (should (equal head (dorg-head-commit dolt-dir)))
                  (let ((image (dorg-model-load-tree dolt-dir 100)))
                    (should (gethash 81 image))
                    (should-not (gethash 82 image))))))))
      (delete-file file))))

(ert-deftest dorg-test-clock-adopt-identical-raw-lines-are-ambiguous ()
  "An occurrence index cannot disambiguate byte-identical source lines."
  (let* ((file (make-temp-file "dorg-clock-adopt-duplicates-" nil ".org"))
         (raw "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:05"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n"
                    ":DORG_ID: 9\n:END:\n:LOGBOOK:\n"
                    raw "\n" raw "\n:END:\n"))
          (let* ((digest (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash 'sha256 (current-buffer))))
                 (err (should-error
                       (dorg-clock--adoption-check-source
                        (list :source (list :file file) :revision digest
                              :heading-id "9" :occurrence 0 :raw raw))
                       :type 'dorg-owner-refusal)))
            (should (eq 'ambiguous-adoption (nth 2 err)))))
      (delete-file file))))

(ert-deftest dorg-test-clock-adopt-refuses-source-changed-after-intent ()
  "The precommit adoption check rejects a changed raw source revision."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let* ((file (make-temp-file "dorg-clock-adopt-change-" nil ".org"))
         (raw "CLOCK: [2026-10-31 Sat 10:00]--[2026-10-31 Sat 10:05] => 0:05")
         (body (concat "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                       ":LOGBOOK:\n" raw "\n:END:\n")))
    (unwind-protect
        (progn
          (with-temp-file file (insert body))
          (let ((digest (with-temp-buffer
                          (insert-file-contents-literally file)
                          (secure-hash 'sha256 (current-buffer)))))
            (dorg-test--with-temp-db dolt-dir
              (dorg-sql-batch
               dolt-dir
               (dorg-test--parse-to-sql
                "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
                100))
              (dorg-commit dolt-dir "seed clock scope")
              (let* ((base (dorg-head-commit dolt-dir))
                     (source (list :dolt-dir dolt-dir :root-id 100))
                     (intent (dorg-clock-make-intent
                              'adopt :source source :base base :op-id "adopt-stale"
                              :scope-id "71" :interval-id "81"
                              :expected-active nil
                              :task-ref (plist-get
                                         (dorg-clock-task-ref (list :file file) "9")
                                         :canonical)
                              :instant-utc 1793466400
                              :zone "America/Los_Angeles"
                              :adoption (list :source (list :file file)
                                              :revision digest :heading-id "9"
                                              :occurrence 0 :raw raw))))
                (with-temp-file file (insert (concat body "# changed\n")))
                (let ((err (should-error (dorg-api-execute intent :mode 'apply)
                                         :type 'dorg-api-error)))
                  (should (eq 'conflict (dorg-error-reason err)))
                  (should (equal base (dorg-head-commit dolt-dir))))))))
      (delete-file file))))

(ert-deftest dorg-test-clock-adopt-repeated-hour-refuses-typed ()
  "Public adoption refuses the repeated DST wall hour without guessing."
  (let* ((file (make-temp-file "dorg-clock-dst-" nil ".org"))
         (raw "CLOCK: [2026-11-01 Sun 01:30]"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                    ":LOGBOOK:\n" raw "\n:END:\n"))
          (let* ((digest (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash 'sha256 (current-buffer))))
                 (err (should-error
                       (dorg-clock-make-intent
                        'adopt :source '(:dolt-dir "/tmp/store" :root-id 100)
                        :base "h0" :op-id "dst-adopt"
                        :scope-id "71" :interval-id "81"
                        :expected-active nil
                        :task-ref (plist-get
                                   (dorg-clock-task-ref (list :file file) "9")
                                   :canonical)
                        :instant-utc 1793523600
                        :zone "America/Los_Angeles"
                        :adoption (list :source (list :file file)
                                        :revision digest :heading-id "9"
                                        :occurrence 0 :raw raw
                                        :open-choice 'takeover))
                       :type 'dorg-api-error)))
            (should (eq 'ambiguous-adoption (dorg-error-reason err)))))
      (delete-file file))))

(ert-deftest dorg-test-clock-adoption-validates-complete-affected-scope ()
  "A hidden closed sibling with a wrong scope key blocks adoption."
  (let* ((before (make-hash-table :test 'eql))
         (after (make-hash-table :test 'eql))
         (full (make-hash-table :test 'eql))
         (scope (dorg-test-clock--record
                 71 100 '(("DORG_CLOCK_KIND" . "scope"))))
         (new (dorg-test-clock--record
               81 71 '(("DORG_CLOCK_KIND" . "interval")
                       ("DORG_CLOCK_SCOPE_ID" . "71")
                       ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9")
                       ("DORG_CLOCK_START_UTC" . "1793466000")
                       ("DORG_CLOCK_END_UTC" . "1793466300")
                       ("DORG_CLOCK_ZONE" . "America/Los_Angeles"))))
         (rogue (dorg-test-clock--record
                 82 71 '(("DORG_CLOCK_KIND" . "interval")
                         ("DORG_CLOCK_SCOPE_ID" . "99")
                         ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:8")
                         ("DORG_CLOCK_START_UTC" . "1793466000")
                         ("DORG_CLOCK_END_UTC" . "1793466300")
                         ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))))
    (puthash 71 scope before)
    (puthash 71 scope after)
    (puthash 81 new after)
    (puthash 71 scope full)
    (puthash 82 rogue full)
    (let ((context (dorg-ext-index-context-create
                    :source-kind 'dolt :root-id 100 :dolt-dir "/tmp/store"
                    :base-commit "h0" :base-image before
                    :candidate-postimage after
                    :owner-request
                    '(:action adopt :scope-id "71" :interval-id "81"
                      :expected-active nil :adoption (:raw "CLOCK")))))
      (cl-letf (((symbol-function 'dorg-clock--adoption-check-source)
                 (lambda (_) "CLOCK"))
                ((symbol-function 'dorg-model-load-tree)
                 (lambda (&rest _) full)))
        (should-error (dorg-clock--validate-normal context)
                      :type 'dorg-owner-refusal)))))

(ert-deftest dorg-test-clock-restore-refuses-identity-change-or-new-record ()
  "Restore cannot rewrite task identity or introduce an interval by history."
  (let* ((before (dorg-test-clock--image))
         (changed (copy-hash-table before))
         (introduced (copy-hash-table before)))
    (puthash 81
             (dorg-test-clock--record
              81 71 '( ("DORG_CLOCK_KIND" . "interval")
                      ("DORG_CLOCK_SCOPE_ID" . "71")
                      ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:99")
                      ("DORG_CLOCK_START_UTC" . "1793536799")
                      ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
             changed)
    (puthash 82
             (dorg-test-clock--record
              82 71 '(("DORG_CLOCK_KIND" . "interval")
                      ("DORG_CLOCK_SCOPE_ID" . "71")
                      ("DORG_CLOCK_TASK_REF" . "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:10")
                      ("DORG_CLOCK_START_UTC" . "1793536799")
                      ("DORG_CLOCK_END_UTC" . "1793536800")
                      ("DORG_CLOCK_ZONE" . "America/Los_Angeles")))
             introduced)
    (should-error (dorg-clock--validate-restore-images before changed)
                  :type 'dorg-owner-refusal)
    (should-error (dorg-clock--validate-restore-images before introduced)
                  :type 'dorg-owner-refusal)))

(ert-deftest dorg-test-clock-adopt-open-needs-choice-and-takes-over ()
  "An exact open legacy line becomes active only with explicit takeover."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let* ((file (make-temp-file "dorg-clock-open-" nil ".org"))
         (raw "CLOCK: [2026-10-31 Sat 10:00]"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                    ":LOGBOOK:\n" raw "\n:END:\n"))
          (let* ((digest (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash 'sha256 (current-buffer))))
                 (provenance (list :source (list :file file)
                                   :revision digest :heading-id "9"
                                   :occurrence 0 :raw raw)))
            (dorg-test--with-temp-db dolt-dir
              (dorg-sql-batch
               dolt-dir
               (dorg-test--parse-to-sql
                "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
                100))
              (dorg-commit dolt-dir "seed clock scope")
              (let* ((source (list :dolt-dir dolt-dir :root-id 100))
                     (common (list :source source :base (dorg-head-commit dolt-dir)
                                   :op-id "adopt-open-81" :scope-id "71"
                                   :interval-id "81" :expected-active nil
                                   :task-ref (plist-get
                                              (dorg-clock-task-ref
                                               (list :file file) "9")
                                              :canonical)
                                   :instant-utc 1793466400
                                   :zone "America/Los_Angeles")))
                (should-error
                 (apply #'dorg-clock-make-intent
                        'adopt (append common (list :adoption provenance)))
                 :type 'dorg-api-error)
                (let* ((intent (apply #'dorg-clock-make-intent
                                      'adopt
                                      (append common
                                              (list :adoption
                                                    (append provenance
                                                            '(:open-choice takeover))))))
                       (_ (dorg-api-execute intent :mode 'apply))
                       (state (dorg-clock-read "71" :source source
                                               :observed-at 1793466400)))
                  (should (equal "81" (plist-get state :interval-id)))
                  (should (= 1793466000 (plist-get state :start-utc)))
                  (should (equal digest
                                 (with-temp-buffer
                                   (insert-file-contents-literally file)
                                   (secure-hash 'sha256 (current-buffer))))))))))
      (delete-file file))))

(defun dorg-test-clock--wait-file (path seconds &optional process)
  "Wait at most SECONDS for PATH while optional PROCESS remains live."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (file-exists-p path)) (< (float-time) deadline)
                (or (null process) (process-live-p process)))
      (accept-process-output nil 0.05))
    (file-exists-p path)))

(defun dorg-test-clock--read-sexp-file (path)
  "Read one inert s-expression from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (read (current-buffer))))

(defun dorg-test-clock--write-race-client
    (path source base interval port ready release result)
  "Write independent Emacs client PATH for a start at BASE."
  (with-temp-file path
    (insert
     ";;; -*- lexical-binding: t; -*-\n(require 'dorg)\n"
     (format "(advice-add 'dorg-dolt-server-port :around (lambda (_orig _dir) %d))\n"
             port)
     (format
      (concat
       "(let* ((source '%S) (base %S) (ready %S) (release %S) (result %S) "
       "(intent (dorg-clock-make-intent 'start :source source :base base "
       ":op-id %S :scope-id \"71\" :interval-id %S "
       ":expected-active nil :task-ref \"dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9\" "
       ":instant-utc 1793536799 :zone \"UTC\" :lock-timeout 15)) "
       "(deadline (+ (float-time) 30)) outcome) "
       "(with-temp-file ready (insert \"ready\")) "
       "(while (and (not (file-exists-p release)) (< (float-time) deadline)) "
       "(sleep-for 0.05)) "
       "(setq outcome "
       "(condition-case err "
       "(if (not (file-exists-p release)) '(:status timeout) "
       "(let ((reply (dorg-api-execute intent :mode 'apply))) "
       "(list :status 'ok :interval-id (plist-get reply :interval-id)))) "
       "(dorg-api-error (list :status 'refused :reason (dorg-error-reason err))) "
       "(error (list :status 'error :message (error-message-string err))))) "
       "(with-temp-file result (prin1 outcome (current-buffer))))\n")
      source base ready release result
      (format "race-start-%s" interval) interval))))

(ert-deftest dorg-test-clock-two-independent-emacs-starts-one-scope ()
  "Two processes sharing a base cannot both create a live scope interval."
  (unless (and (bound-and-true-p dorg-test--dolt-available)
               (bound-and-true-p dorg-test--server-port))
    (ert-skip "independent clients require the shared Dolt test server"))
  (dorg-test--with-temp-db-and-dir temp-dir dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
      100))
    (dorg-commit dolt-dir "seed clock scope")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir))
           (release (expand-file-name "release" temp-dir))
           (root (file-name-directory (locate-library "dorg-clock")))
           (emacs (expand-file-name invocation-name invocation-directory))
           (a-ready (expand-file-name "a.ready" temp-dir))
           (b-ready (expand-file-name "b.ready" temp-dir))
           (a-result (expand-file-name "a.result" temp-dir))
           (b-result (expand-file-name "b.result" temp-dir))
           (a-script (expand-file-name "a.el" temp-dir))
           (b-script (expand-file-name "b.el" temp-dir))
           (a-output (generate-new-buffer " *clock-child-a*"))
           (b-output (generate-new-buffer " *clock-child-b*"))
           (a nil) (b nil))
      (dorg-test-clock--write-race-client
       a-script source base "81" dorg-test--server-port
       a-ready release a-result)
      (dorg-test-clock--write-race-client
       b-script source base "82" dorg-test--server-port
       b-ready release b-result)
      (unwind-protect
          (progn
            (setq a (make-process :name "dorg-clock-client-a" :buffer a-output
                                  :command (list emacs "-Q" "--batch"
                                                 "-L" root "-L"
                                                 (expand-file-name "lib/mysql" root)
                                                 "-l" a-script)))
            (setq b (make-process :name "dorg-clock-client-b" :buffer b-output
                                  :command (list emacs "-Q" "--batch"
                                                 "-L" root "-L"
                                                 (expand-file-name "lib/mysql" root)
                                                 "-l" b-script)))
            (should (dorg-test-clock--wait-file a-ready 30 a))
            (should (dorg-test-clock--wait-file b-ready 30 b))
            (with-temp-file release (insert "go"))
            (unless (dorg-test-clock--wait-file a-result 60 a)
              (ert-fail (list 'client-a-no-result
                              (process-status a)
                              (with-current-buffer a-output (buffer-string)))))
            (unless (dorg-test-clock--wait-file b-result 60 b)
              (ert-fail (list 'client-b-no-result
                              (process-status b)
                              (with-current-buffer b-output (buffer-string)))))
            (let* ((outcomes (list (dorg-test-clock--read-sexp-file a-result)
                                   (dorg-test-clock--read-sexp-file b-result)))
                   (winner (cl-find-if (lambda (x) (eq 'ok (plist-get x :status)))
                                       outcomes))
                   (loser (cl-find-if (lambda (x) (eq 'refused
                                                      (plist-get x :status)))
                                      outcomes))
                   (image (dorg-model-load-tree dolt-dir 100))
                   (state (dorg-clock-read "71" :source source
                                           :observed-at 1793536800)))
              (unless (= 1 (cl-count-if
                            (lambda (x) (eq 'ok (plist-get x :status)))
                            outcomes))
                (ert-fail (list 'race-outcomes outcomes
                                (with-current-buffer a-output (buffer-string))
                                (with-current-buffer b-output (buffer-string)))))
              (should loser)
              (should (memq (plist-get loser :reason)
                            '(stale stale-active-interval)))
              (should (equal (plist-get winner :interval-id)
                             (plist-get state :interval-id)))
              (should (= 1 (+ (if (gethash 81 image) 1 0)
                            (if (gethash 82 image) 1 0))))))
        (when (process-live-p a) (delete-process a))
        (when (process-live-p b) (delete-process b))
        (kill-buffer a-output)
        (kill-buffer b-output)))))

(ert-deftest dorg-test-clock-overlap-classifies-only-pointer-change ()
  "An unrelated overlap retains the engine reason; a changed pointer is stale."
  (let* ((source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/clock-overlap" :root-id 100)))
         (request '(:scope-id "71" :expected-active nil))
         (image (make-hash-table :test 'eql)))
    (puthash 71
             (dorg-test-clock--record
              71 100 '(("DORG_CLOCK_KIND" . "scope")
                       ("DORG_CLOCK_ACTIVE_ID" . "81")))
             image)
    (cl-letf (((symbol-function 'dorg-model-load-headings)
               (lambda (_dir _root ids commit &rest _)
                 (should (equal '(100 71) ids))
                 (should (equal "their-commit" commit))
                 image)))
      (should (dorg-clock--overlap-changed-active-p
               source request '(:overlap (71) :commit "their-commit")))
      (should-not (dorg-clock--overlap-changed-active-p
                   source request '(:overlap (99) :commit "their-commit")))
      (puthash 71
               (dorg-test-clock--record
                71 100 '(("DORG_CLOCK_KIND" . "scope")))
               image)
      (should-not (dorg-clock--overlap-changed-active-p
                   source request '(:overlap (71) :commit "their-commit"))))))

(ert-deftest dorg-test-clock-one-authority-root-keeps-other-root-query-visible ()
  "Clock writes touch one root; explicit QL on another root still reads it."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (append
      (dorg-test--parse-to-sql
       "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
       100)
      (dorg-test--parse-to-sql
       "* Other task\n:PROPERTIES:\n:DORG_ID: 201\n:END:\n" 200)))
    (dorg-commit dolt-dir "seed two roots")
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (other (make-dorg-registration
                   :backend (dorg-backend-dolt dolt-dir 200)
                   :file (expand-file-name "other.org" dolt-dir)))
           (before (dorg-model-load-tree dolt-dir 200))
           (query-before (dorg-ql-select '(id 201) other))
           (intent (dorg-clock-make-intent
                    'start :source source :base (dorg-head-commit dolt-dir)
                    :op-id "isolated-start-81" :scope-id "71"
                    :interval-id "81" :expected-active nil
                    :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:200:201"
                    :instant-utc 1793536799 :zone "UTC")))
      (should query-before)
      (dorg-api-execute intent :mode 'apply)
      (let ((after (dorg-model-load-tree dolt-dir 200)))
        (should (equal (gethash 201 before) (gethash 201 after)))
        (should (equal query-before
                       (dorg-ql-select '(id 201) other)))))))

(ert-deftest dorg-test-clock-generated-line-imports-as-unmanaged-logbook ()
  "A rendered CLOCK line survives import as raw text, not a new interval."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (dorg-test--with-temp-db-and-dir temp-dir dolt-dir
    (let* ((file (expand-file-name "task.org" temp-dir))
           (line (dorg-clock-render-interval
                  1793536799 1793537101 "America/Los_Angeles"))
           (body (concat "* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"
                         ":LOGBOOK:\n" line "\n:END:\n")))
      (with-temp-file file (insert body))
      (with-temp-buffer
        (insert body)
        (org-mode)
        (setq buffer-file-name file)
        (dorg-import-document (current-buffer) :dolt-dir dolt-dir
                              :target-file file :mode 'bootstrap))
      (let* ((root (dorg-repo-root-id dolt-dir file))
             (image (dorg-model-load-tree dolt-dir root))
             (task (gethash 9 image))
             (managed nil))
        (maphash (lambda (_ heading)
                   (when (dorg-clock--properties heading)
                     (setq managed t)))
                 image)
        (should task)
        (should (equal (list line) (dorg-clock--legacy-lines task)))
        (should-not managed)))))

(defun dorg-test-clock--history-document (count &optional active)
  "Return a clock scope with COUNT settled intervals and optional ACTIVE."
  (concat
   "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n"
   ":DORG_CLOCK_KIND: scope\n"
   (when active ":DORG_CLOCK_ACTIVE_ID: 81\n")
   ":END:\n"
   (mapconcat
    (lambda (i)
      (format (concat "** Settled %d\n:PROPERTIES:\n:DORG_ID: %d\n"
                      ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
                      ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9\n"
                      ":DORG_CLOCK_START_UTC: 1793536000\n"
                      ":DORG_CLOCK_END_UTC: 1793536100\n"
                      ":DORG_CLOCK_ZONE: UTC\n:END:\n")
              i (+ 1000 i)))
    (number-sequence 1 count) "")
   (when active
     (concat "** Running\n:PROPERTIES:\n:DORG_ID: 81\n"
             ":DORG_CLOCK_KIND: interval\n:DORG_CLOCK_SCOPE_ID: 71\n"
             ":DORG_CLOCK_TASK_REF: dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9\n"
             ":DORG_CLOCK_START_UTC: 1793536799\n"
             ":DORG_CLOCK_ZONE: UTC\n:END:\n"))))

(defun dorg-test-clock--bounded-start-measure (count)
  "Measure exact query and hydration work for a start with COUNT history."
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      (dorg-test-clock--history-document count) 100))
    (dorg-commit dolt-dir (format "seed %d settled clocks" count))
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (intent (dorg-clock-make-intent
                    'start :source source :base (dorg-head-commit dolt-dir)
                    :op-id (format "bounded-start-%d" count)
                    :scope-id "71" :interval-id "81" :expected-active nil
                    :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:9" :instant-utc 1793536799
                    :zone "UTC"))
           (original-query (symbol-function 'dorg-sql--mysql-exec))
           (original-load (symbol-function 'dorg-model-load-headings))
           (statements 0) (probe-rows nil) (loads nil)
           (started (float-time)))
      (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (query conn)
                   (let ((rows (funcall original-query query conn)))
                     (cl-incf statements)
                     (when (string-match-p "AS open_count" query)
                       (push (length rows) probe-rows))
                     rows)))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (dir root ids &rest rest)
                   (let ((image (apply original-load dir root ids rest)))
                     (when (equal dir dolt-dir)
                       (push (list (sort (copy-sequence ids) #'<)
                                   (hash-table-count image)) loads))
                     image))))
        (let ((result (dorg-api-execute intent :mode 'dry-run)))
          (should (eq 'dry-run (plist-get result :mode)))))
      (list :history count :statements statements
            :probe-rows (nreverse probe-rows) :loads (nreverse loads)
            :seconds (- (float-time) started)))))

(ert-deftest dorg-test-clock-bounded-start-history-after-lowering ()
  "Start's hydrated IDs, rows and executed statements ignore settled history.
The SQL spy is below the statement executor, after write batching; the one
scalar open-child probe is counted separately from model hydration."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let ((short (dorg-test-clock--bounded-start-measure 10))
        (long (dorg-test-clock--bounded-start-measure 1000)))
    ;; 100 is the authority root; 71 is its clock scope.  The new ID 81
    ;; is probed as absent; no historical interval is hydrated.
    (should (equal '(((71 100) 2) ((81) 0)) (plist-get short :loads)))
    (should (equal (plist-get short :loads) (plist-get long :loads)))
    (should (equal '(1) (plist-get short :probe-rows)))
    (should (equal (plist-get short :probe-rows)
                   (plist-get long :probe-rows)))
    (should (= (plist-get short :statements)
               (plist-get long :statements)))
    (message "clock bounds 10=%0.3fs 1000=%0.3fs statements=%d"
             (plist-get short :seconds) (plist-get long :seconds)
             (plist-get short :statements))))

(defun dorg-test-clock--bounded-active-measure (count)
  "Measure read/stop/switch/correct/cancel with COUNT settled intervals."
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql
      (dorg-test-clock--history-document count t) 100))
    (dorg-commit dolt-dir (format "seed %d settled and one running clock" count))
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir))
           (original-query (symbol-function 'dorg-sql--mysql-exec))
           (original-load (symbol-function 'dorg-model-load-headings))
           results)
      (dolist (action '(read stop switch correct cancel))
        (let ((statements 0) (loads nil) (probe-rows nil)
              (started (float-time)))
          (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                     (lambda (query conn)
                       (let ((rows (funcall original-query query conn)))
                         (cl-incf statements)
                         (when (string-match-p "AS open_count" query)
                           (push (length rows) probe-rows))
                         rows)))
                    ((symbol-function 'dorg-model-load-headings)
                     (lambda (dir root ids &rest rest)
                       (let ((image (apply original-load dir root ids rest)))
                         (when (equal dir dolt-dir)
                           (push (list (sort (copy-sequence ids) #'<)
                                       (hash-table-count image)) loads))
                         image))))
            (if (eq action 'read)
                (should (equal "81"
                               (plist-get (dorg-clock-read
                                           "71" :source source :snapshot base
                                           :observed-at 1793536899)
                                          :interval-id)))
              (let ((intent
                     (apply #'dorg-clock-make-intent action
                            (append
                             (list :source source :base base
                                   :op-id (format "bounds-%s" action)
                                   :scope-id "71" :interval-id
                                   (if (eq action 'switch) "82" "81")
                                   :expected-active "81"
                                   :instant-utc 1793536899 :zone "UTC")
                             (pcase action
                               ('switch (list :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:10"))
                               ('correct (list :correction
                                               '(:start-utc 1793536700))))))))
                (should (eq 'dry-run
                            (plist-get (dorg-api-execute intent :mode 'dry-run)
                                       :mode))))))
          (push (cons action
                      (list :loads (nreverse loads)
                            :probe-rows (nreverse probe-rows)
                            :statements statements
                            :seconds (- (float-time) started)))
                results)))
      (nreverse results))))

(ert-deftest dorg-test-clock-bounded-active-history-after-lowering ()
  "Active lifecycle hydration and executed SQL stay bounded by named IDs."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let ((short (dorg-test-clock--bounded-active-measure 10))
        (long (dorg-test-clock--bounded-active-measure 1000))
        (expected '((read . (((71 100) 2) ((71 81 100) 3)))
                    (stop . (((71 81 100) 3)))
                    (switch . (((71 81 100) 3) ((82) 0)))
                    (correct . (((71 81 100) 3)))
                    (cancel . (((71 81 100) 3))))))
    (dolist (action '(read stop switch correct cancel))
      (let ((s (cdr (assq action short)))
            (l (cdr (assq action long))))
        ;; 100 is the authority root, 71 its scope, 81 the named active
        ;; child; switch also probes that its new ID 82 is absent.
        (should (equal (cdr (assq action expected)) (plist-get s :loads)))
        (should (equal (plist-get s :loads) (plist-get l :loads)))
        (should (equal (plist-get s :probe-rows)
                       (plist-get l :probe-rows)))
        (should (= (plist-get s :statements)
                   (plist-get l :statements)))
        (message "clock bounds %s 10=%0.3fs 1000=%0.3fs statements=%d loads=%S"
                 action (plist-get s :seconds) (plist-get l :seconds)
                 (plist-get s :statements) (plist-get s :loads))))))

(defun dorg-test-clock--bounded-apply-assert-base (dolt-dir base)
  "Return DOLT-DIR's clean BASE state, or signal on a stale fixture."
  (let ((head (dorg-head-commit dolt-dir))
        (status (dorg-sql dolt-dir "SELECT table_name FROM dolt_status LIMIT 1")))
    (unless (and (equal base head) (null status))
      (error "clock bounds fixture is not clean at seed %s: head=%s status=%S"
             base head status))
    (list :head head :clean t)))

(defun dorg-test-clock--bounded-apply-measure (dolt-dir count action base)
  "Apply ACTION from clean BASE with COUNT settled intervals in DOLT-DIR."
  (dorg-sql dolt-dir
            (format "CALL DOLT_RESET('--hard', %s)" (dorg-sql-quote base)))
  (let ((before (dorg-test-clock--bounded-apply-assert-base dolt-dir base)))
    (message "clock apply seed %s history=%d head=%s clean=%S"
             action count (plist-get before :head) (plist-get before :clean))
    (let* ((source (list :dolt-dir dolt-dir :root-id 100))
           (intent (apply #'dorg-clock-make-intent action
                          (append
                           (list :source source :base base
                                 :op-id (format "bounds-apply-%s-%d" action count)
                                 :scope-id "71" :interval-id
                                 (if (memq action '(start switch)) "82" "81")
                                 :expected-active
                                 (unless (eq action 'start) "81")
                                 :instant-utc 1793536899 :zone "UTC")
                           (pcase action
                             ((or 'start 'switch)
                              (list :task-ref "dolt:v1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:1:10"))
                             ('correct (list :correction
                                             '(:start-utc 1793536700)))))))
           (original-query (symbol-function 'dorg-sql--mysql-exec))
           (original-load (symbol-function 'dorg-model-load-headings))
           (statements 0) (loads nil) (probe-rows nil)
           (started (float-time)))
      (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (query conn)
                   (let ((rows (funcall original-query query conn)))
                     (cl-incf statements)
                     (when (string-match-p "AS open_count" query)
                       (push (length rows) probe-rows))
                     rows)))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (dir root ids &rest rest)
                   (let ((image (apply original-load dir root ids rest)))
                     (when (equal dir dolt-dir)
                       (push (list (sort (copy-sequence ids) #'<)
                                   (hash-table-count image)) loads))
                     image))))
        (should (eq 'apply
                    (plist-get (dorg-api-execute intent :mode 'apply) :mode))))
      (let ((head (dorg-head-commit dolt-dir))
            (snapshot (dorg-clock-read "71" :source source
                                       :observed-at 1793536999)))
        (should-not (equal base head))
        (should (equal (pcase action
                         ('start "82")
                         ('correct "81")
                         ('switch "82")
                         (_ nil))
                       (plist-get snapshot :interval-id))))
      (list :loads (nreverse loads) :probe-rows (nreverse probe-rows)
            :statements statements :seconds (- (float-time) started)
            :before before))))

(defun dorg-test-clock--bounded-apply-series (count)
  "Return measurements for five writes on one COUNT-history store."
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch
     dolt-dir
     (dorg-test--parse-to-sql (dorg-test-clock--history-document count t)
                              100))
    (dorg-commit dolt-dir (format "seed %d settled clocks" count))
    (let* ((active-base (dorg-head-commit dolt-dir))
           (source (list :dolt-dir dolt-dir :root-id 100))
           (seed-stop (dorg-clock-make-intent
                       'stop :source source :base active-base
                       :op-id (format "bounds-seed-stop-%d" count)
                       :scope-id "71" :interval-id "81"
                       :expected-active "81" :instant-utc 1793536899
                       :zone "UTC")))
      (should (eq 'apply
                  (plist-get (dorg-api-execute seed-stop :mode 'apply) :mode)))
      (let ((inactive-base (dorg-head-commit dolt-dir))
            (results nil))
        (should-not (equal active-base inactive-base))
        (dolist (action '(start stop switch correct cancel))
          (push (cons action
                      (dorg-test-clock--bounded-apply-measure
                       dolt-dir count action
                       (if (eq action 'start) inactive-base active-base)))
                results))
        ;; Skipping the last reset must fail the same seed guard.
        (should-error
         (dorg-test-clock--bounded-apply-assert-base dolt-dir active-base))
        (nreverse results)))))

(ert-deftest dorg-test-clock-bounded-apply-history-after-lowering ()
  "Actual executed SQL counts for every write ignore settled history length."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let ((short-results (dorg-test-clock--bounded-apply-series 10))
        (long-results (dorg-test-clock--bounded-apply-series 1000)))
    (dolist (action '(start stop switch correct cancel))
      (let ((short (cdr (assq action short-results)))
            (long (cdr (assq action long-results))))
      (should (equal (plist-get short :loads) (plist-get long :loads)))
      (should (equal (plist-get short :probe-rows)
                     (plist-get long :probe-rows)))
      (should (= (plist-get short :statements)
                 (plist-get long :statements)))
      (message "clock apply bounds %s 10=%0.3fs 1000=%0.3fs SQL=%d loads=%S"
               action (plist-get short :seconds) (plist-get long :seconds)
               (plist-get short :statements) (plist-get short :loads))))))

(provide 'dorg-clock-tests)
;;; dorg-clock-tests.el ends here
