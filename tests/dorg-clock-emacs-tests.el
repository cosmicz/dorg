;;; dorg-clock-emacs-tests.el --- Local clock reconciliation -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'dorg-clock-emacs)
(require 'dorg-test-helpers)

(defvar org-state)

(defmacro dorg-test-clock-emacs--with-buffer (&rest body)
  "Run BODY in an isolated managed Org buffer with mocked authority I/O."
  `(with-temp-buffer
     (org-mode)
     (insert "* Task\n:PROPERTIES:\n:DORG_ID: 91\n:END:\n")
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (let ((buffer-file-name "/tmp/dorg-clock-emacs-task.org")
           (dorg-clock-emacs--config
            '(:source (:dolt-dir "/tmp/clock-store" :root-id 100)
              :scope-id "71" :zone "UTC"))
           (dorg-clock-emacs--state nil)
           (dorg-clock-emacs--marker nil)
           (dorg-clock-emacs--pending-intent nil)
           (dorg-clock-emacs-mode t))
       (unwind-protect
           (progn ,@body)
         (setq dorg-clock-emacs--pending-intent nil)))))

(ert-deftest dorg-test-clock-emacs-in-routes-before-native-edit ()
  "Managed clock-in starts one owner intent and never calls native Org."
  (dorg-test-clock-emacs--with-buffer
   (let ((native nil) (actions nil)
         (snapshot '(:scope-id "71" :snapshot-token "h0" :interval-id nil)))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) snapshot))
               ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
                (lambda () "plain:v1:dGFza3M=:91"))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (action &rest options)
                  (push (cons action options) actions) 'intent))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _) (setq snapshot
                                   '(:scope-id "71" :snapshot-token "h1"
                                     :interval-id "81" :task-ref
                                     "plain:v1:dGFza3M=:91"
                                     :start-utc 100 :zone "UTC")))))
       (dorg-clock-emacs--around-in
        (lambda (&rest _) (setq native t)) nil nil)
       (should-not native)
       (should (equal 'start (caar actions)))
       (should (equal "h0" (plist-get (cdar actions) :base)))
       (should (equal "81" (plist-get dorg-clock-emacs--state :interval-id)))))))

(ert-deftest dorg-test-clock-emacs-dirty-view-defers-without-write ()
  "Dirty managed buffers reject clock-in before consulting the writer."
  (dorg-test-clock-emacs--with-buffer
   (set-buffer-modified-p t)
   (let ((native nil) (write nil))
     (cl-letf (((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (setq write t))))
       (should-error
        (dorg-clock-emacs--around-in
         (lambda (&rest _) (setq native t)) nil nil)
        :type 'user-error)
       (should-not native)
       (should-not write)))))

(ert-deftest dorg-test-clock-emacs-externally-changed-file-defers ()
  "A clean but externally changed visited file cannot supply task identity."
  (dorg-test-clock-emacs--with-buffer
   (let ((writes 0))
     (cl-letf (((symbol-function 'get-file-buffer)
                (lambda (&rest _) (current-buffer)))
               ((symbol-function 'verify-visited-file-modtime)
                (lambda (&rest _) nil))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (cl-incf writes))))
       (should-error
        (dorg-clock-emacs--around-in
         (lambda (&rest _) (ert-fail "native")))
        :type 'user-error)
       (should (= 0 writes))))))

(ert-deftest dorg-test-clock-emacs-locked-file-defers-without-write ()
  "A lock on the task file refuses the action before constructing an intent."
  (dorg-test-clock-emacs--with-buffer
   (let ((writes 0))
     (cl-letf (((symbol-function 'file-locked-p)
                (lambda (file)
                  (should (equal file buffer-file-name))
                  "another editor"))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (cl-incf writes))))
       (should-error
        (dorg-clock-emacs--around-in
         (lambda (&rest _) (ert-fail "native")))
        :type 'user-error)
       (should (= 0 writes))))))

(ert-deftest dorg-test-clock-emacs-external-stop-reconcile-only-reads ()
  "A remote stop clears the local indicator without echoing a mutation."
  (dorg-test-clock-emacs--with-buffer
   (let ((reads 0) (writes 0)
         (dorg-clock-emacs--state '(:interval-id "81")))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _)
                  (cl-incf reads)
                  '(:scope-id "71" :snapshot-token "h2"
                    :interval-id nil)))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _) (cl-incf writes))))
       (dorg-clock-emacs-reconcile)
       (should (= 1 reads))
       (should (= 0 writes))
       (should-not (plist-get dorg-clock-emacs--state :interval-id))
       (should (equal " DClock" (dorg-clock-emacs--mode-line)))))))

(ert-deftest dorg-test-clock-emacs-stale-resume-only-reads ()
  "Org persistence cannot resurrect an authority clock that has closed."
  (dorg-test-clock-emacs--with-buffer
   (let ((native nil) (write nil)
         (org-clock-in-resume 'auto-restart)
         (dorg-clock-emacs--state '(:interval-id "old")))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) '(:scope-id "71" :snapshot-token "h1"
                                      :interval-id nil)))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (setq write t))))
       (dorg-clock-emacs--around-in
        (lambda (&rest _) (setq native t)) nil nil)
       (should-not native)
       (should-not write)
       (should-not (plist-get dorg-clock-emacs--state :interval-id))))))

(ert-deftest dorg-test-clock-emacs-unmanaged-keeps-stock-command ()
  "No adapter configuration means the original Org command runs."
  (with-temp-buffer
    (org-mode)
    (let ((called nil))
      (dorg-clock-emacs--around-in
       (lambda (&rest args) (setq called args) 'native) 'prefix 'time)
      (should (equal called '(prefix time))))))

(ert-deftest dorg-test-clock-emacs-unknown-response-retains-exact-intent ()
  "An unknown apply response blocks new actions and retries the same object."
  (dorg-test-clock-emacs--with-buffer
   (let ((writes nil)
         (snapshot '(:scope-id "71" :snapshot-token "h0" :interval-id nil)))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) snapshot))
               ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
                (lambda () "plain:v1:dGFza3M=:91"))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (list 'exact-intent (gensym))))
               ((symbol-function 'dorg-api-execute)
                (lambda (intent &rest _)
                  (push intent writes)
                  (if (= (length writes) 1)
                      (error "response lost")
                    (setq snapshot '(:scope-id "71" :snapshot-token "h1"
                                     :interval-id "81"))
                    '(:idempotent t)))))
       (should-error
        (dorg-clock-emacs--around-in (lambda (&rest _) (ert-fail "native")))
        :type 'error)
       (let ((pending dorg-clock-emacs--pending-intent))
         (should pending)
         (should-error
          (dorg-clock-emacs--around-in (lambda (&rest _) (ert-fail "native")))
          :type 'user-error)
         (should (= 1 (length writes)))
         (should (equal '(:idempotent t)
                        (dorg-clock-emacs-retry-pending)))
         (should (eq pending (car writes)))
         (should-not dorg-clock-emacs--pending-intent))))))

(ert-deftest dorg-test-clock-emacs-out-routes-before-native-edit ()
  "Managed clock-out closes exactly the observed interval through the owner."
  (dorg-test-clock-emacs--with-buffer
   (let ((native nil) (action nil) (options nil)
         (snapshot '(:scope-id "71" :snapshot-token "h0"
                     :interval-id "81" :task-ref "plain:v1:dGFza3M=:91"
                     :start-utc 100 :zone "UTC")))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) snapshot))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (a &rest args)
                  (setq action a options args) 'intent))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _)
                  (setq snapshot '(:scope-id "71" :snapshot-token "h1"
                                   :interval-id nil)) '(:new-token "h1"))))
       (dorg-clock-emacs--around-out
        (lambda (&rest _) (setq native t)))
       (should-not native)
       (should (eq action 'stop))
       (should (equal (plist-get options :interval-id) "81"))
       (should (equal (plist-get options :expected-active) "81"))
       (should-not (plist-get dorg-clock-emacs--state :interval-id))))))

(ert-deftest dorg-test-clock-emacs-todo-hook-ignores-other-state-and-heading ()
  "TODO completion cannot close a clock for another state or heading."
  (dorg-test-clock-emacs--with-buffer
   (let ((dorg-clock-emacs--state '(:interval-id "81" :task-ref "plain:v1:dGFza3M=:91"))
         (dorg-clock-emacs--marker (copy-marker (point-min)))
         (org-clock-out-when-done t)
         (org-state "TODO")
         (reads 0) (writes 0))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) (cl-incf reads)))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _) (cl-incf writes))))
       (dorg-clock-emacs--around-out-if-current
        (lambda () (ert-fail "native")))
       (setq org-state "DONE")
       (goto-char (point-max))
       (insert "* Different\n:PROPERTIES:\n:DORG_ID: 92\n:END:\n")
       (goto-char (point-max))
       (dorg-clock-emacs--around-out-if-current
        (lambda () (ert-fail "native")))
       (should (= 0 reads))
       (should (= 0 writes))))))

(ert-deftest dorg-test-clock-emacs-todo-hook-stops-only-clocked-done-task ()
  "DONE on the clocked heading stops once without native Org clock hooks."
  (dorg-test-clock-emacs--with-buffer
   (let ((dorg-clock-emacs--state '(:interval-id "81" :task-ref "plain:v1:dGFza3M=:91"))
         (dorg-clock-emacs--marker (copy-marker (point-min)))
         (org-clock-out-when-done t)
         (org-state "DONE")
         (org-clock-out-hook (list (lambda () (ert-fail "native out hook"))))
         (snapshot '(:scope-id "71" :snapshot-token "h1"
                     :interval-id "81" :task-ref "plain:v1:dGFza3M=:91"
                     :start-utc 100 :zone "UTC"))
         (writes 0))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) snapshot))
               ((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) 'stop-intent))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _)
                  (cl-incf writes)
                  (setq snapshot '(:scope-id "71" :snapshot-token "h2"
                                   :interval-id nil))
                  '(:new-token "h2"))))
       (dorg-clock-emacs--around-out-if-current
        (lambda () (ert-fail "native boundary")))
       (should (= 1 writes))
       (should-not (plist-get dorg-clock-emacs--state :interval-id))))))

(ert-deftest dorg-test-clock-emacs-typed-refusal-clears-pending ()
  "A definite API refusal leaves no unknown-response retry behind."
  (dorg-test-clock-emacs--with-buffer
   (cl-letf (((symbol-function 'dorg-clock-read)
              (lambda (&rest _) '(:scope-id "71" :snapshot-token "h0"
                                    :interval-id nil)))
             ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
              (lambda () "plain:v1:dGFza3M=:91"))
             ((symbol-function 'dorg-clock-make-intent)
              (lambda (&rest _) 'intent))
             ((symbol-function 'dorg-api-execute)
              (lambda (&rest _) (dorg-api--raise 'conflict "stale"))))
     (should-error
      (dorg-clock-emacs--around-in (lambda (&rest _) (ert-fail "native")))
      :type 'dorg-api-error)
     (should-not dorg-clock-emacs--pending-intent))))

(ert-deftest dorg-test-clock-emacs-indeterminate-api-result-keeps-pending ()
  "A typed indeterminate result still needs the exact operation-ID retry."
  (dorg-test-clock-emacs--with-buffer
   (cl-letf (((symbol-function 'dorg-clock-read)
              (lambda (&rest _) '(:scope-id "71" :snapshot-token "h0"
                                    :interval-id nil)))
             ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
              (lambda () "plain:v1:dGFza3M=:91"))
             ((symbol-function 'dorg-clock-make-intent)
              (lambda (&rest _) 'immutable-intent))
             ((symbol-function 'dorg-api-execute)
              (lambda (&rest _)
                (dorg-api--raise 'indeterminate "commit result unknown"))))
     (should-error
      (dorg-clock-emacs--around-in (lambda (&rest _) (ert-fail "native")))
      :type 'dorg-api-error)
     (should (eq 'immutable-intent dorg-clock-emacs--pending-intent)))))

(ert-deftest dorg-test-clock-emacs-pending-intent-blocks-disable-and-kill ()
  "An unknown operation cannot disappear with its buffer or mode."
  (dorg-test-clock-emacs--with-buffer
   (let ((dorg-clock-emacs--pending-intent 'immutable-intent))
     (should-error (dorg-clock-emacs-mode -1) :type 'user-error)
     (should dorg-clock-emacs-mode)
     (should (eq 'immutable-intent dorg-clock-emacs--pending-intent))
     (should-not (dorg-clock-emacs--may-kill-p))
     (should (eq 'immutable-intent dorg-clock-emacs--pending-intent)))))

(ert-deftest dorg-test-clock-emacs-stale-projection-refuses-task-identity ()
  "A task selected in an old Dolt projection cannot initiate a new clock."
  (dorg-test-clock-emacs--with-buffer
   (cl-letf (((symbol-function 'dorg-current-registration)
              (lambda (&rest _)
                (make-dorg-registration
                 :backend (dorg-backend-dolt "/tmp/clock-task-store" 100)
                 :file buffer-file-name)))
             ((symbol-function 'dorg-projection-view-context)
              (lambda (&rest _) '(:root-id 100 :revision "h0" :scope whole)))
             ((symbol-function 'dorg-head-commit)
              (lambda (&rest _) "h1")))
     (should-error (dorg-clock-emacs--task-ref-at-point)
                   :type 'user-error))))

(ert-deftest dorg-test-clock-emacs-same-task-in-does-not-split-interval ()
  "Clocking into the already active task reconciles without another write."
  (dorg-test-clock-emacs--with-buffer
   (let ((writes 0)
         (state '(:scope-id "71" :snapshot-token "h1" :interval-id "81"
                  :task-ref "plain:v1:dGFza3M=:91" :start-utc 100
                  :zone "UTC")))
     (cl-letf (((symbol-function 'dorg-clock-read)
                (lambda (&rest _) state))
               ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
                (lambda () "plain:v1:dGFza3M=:91"))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _) (cl-incf writes))))
       (dorg-clock-emacs--around-in (lambda (&rest _) (ert-fail "native")))
       (should (= 0 writes))
       (should (equal "81" (plist-get dorg-clock-emacs--state :interval-id)))))))

(ert-deftest dorg-test-clock-emacs-same-task-alias-does-not-switch ()
  "A second path to one root cannot turn a re-clock-in into a switch."
  (dorg-test-clock-emacs--with-buffer
   (let* ((identity "same-root")
          (canonical (format "plain:v1:%s:91"
                             (base64-encode-string identity t)))
          (writes 0))
     (cl-letf (((symbol-function 'dorg-root-keywords)
                (lambda (_source) (list (cons "DORG_STORE_ID" identity))))
               ((symbol-function 'dorg-mutate--resolve-source)
                (lambda (source)
                  (dorg-mutate--source-make
                   :kind 'plain-org :file (plist-get source :file))))
               ((symbol-function 'dorg-clock-emacs--task-ref-at-point)
                (lambda ()
                  (plist-get
                   (dorg-clock-task-ref
                    '(:file "/tmp/other-path-to-task.org") "91")
                   :canonical)))
               ((symbol-function 'dorg-clock-read)
                (lambda (&rest _)
                  (list :scope-id "71" :snapshot-token "h1"
                        :interval-id "81" :task-ref canonical
                        :start-utc 100 :zone "UTC")))
               ((symbol-function 'dorg-api-execute)
                (lambda (&rest _) (cl-incf writes))))
       (dorg-clock-emacs--around-in
        (lambda (&rest _) (ert-fail "native")))
       (should (= 0 writes))
       (should (equal canonical
                      (plist-get dorg-clock-emacs--state :task-ref)))))))

(ert-deftest dorg-test-clock-emacs-foreign-open-line-defers ()
  "An unadopted open CLOCK line is not silently merged with a managed clock."
  (dorg-test-clock-emacs--with-buffer
   (goto-char (point-max))
   (insert ":LOGBOOK:\nCLOCK: [2026-10-31 Sat 10:00]\n:END:\n")
   (goto-char (point-min))
   (set-buffer-modified-p nil)
   (let ((writes 0))
     (cl-letf (((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (cl-incf writes))))
       (should-error
        (dorg-clock-emacs--around-in
         (lambda (&rest _) (ert-fail "native")))
        :type 'user-error)
       (should (= 0 writes))))))

(ert-deftest dorg-test-clock-emacs-duplicate-task-id-defers ()
  "Two equal heading IDs in one view cannot select a unique task."
  (dorg-test-clock-emacs--with-buffer
   (goto-char (point-max))
   (insert "* Duplicate\n:PROPERTIES:\n:DORG_ID: 91\n:END:\n")
   (goto-char (point-min))
   (set-buffer-modified-p nil)
   (let ((writes 0))
     (cl-letf (((symbol-function 'dorg-clock-make-intent)
                (lambda (&rest _) (cl-incf writes))))
       (should-error
        (dorg-clock-emacs--around-in
         (lambda (&rest _) (ert-fail "native")))
        :type 'user-error)
       (should (= 0 writes))))))

(ert-deftest dorg-test-clock-emacs-real-org-commands-write-only-authority ()
  "Actual advised Org commands start and stop without editing the task file."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let ((file (make-temp-file "dorg-clock-editor-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* Task\n:PROPERTIES:\n:DORG_ID: 9\n:END:\n"))
          (dorg-test--with-temp-db dolt-dir
            (dorg-sql-batch
             dolt-dir
             (dorg-test--parse-to-sql
              "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
              100))
            (dorg-commit dolt-dir "seed clock scope")
            (let ((source (list :dolt-dir dolt-dir :root-id 100))
                  (now 1793536799))
              (with-temp-buffer
                (org-mode)
                (setq buffer-file-name file)
                (insert-file-contents file)
                (goto-char (point-min))
                (org-next-visible-heading 1)
                (set-buffer-modified-p nil)
                (let ((before (buffer-string)))
                  (cl-letf (((symbol-function 'dorg-clock-emacs--now)
                             (lambda () now)))
                    (dorg-clock-emacs-enable source "71" "UTC")
                    (unwind-protect
                        (progn
                          (org-clock-in)
                          (should (plist-get dorg-clock-emacs--state
                                             :interval-id))
                          (should-not (org-clocking-p))
                          (setq now (1+ now))
                          (org-clock-out)
                          (should-not (plist-get dorg-clock-emacs--state
                                                 :interval-id))
                          (should (equal before (buffer-string)))
                          (should (equal before
                                         (with-temp-buffer
                                           (insert-file-contents file)
                                           (buffer-string)))))
                      (dorg-clock-emacs-mode -1))))))))
      (delete-file file))))

(ert-deftest dorg-test-clock-emacs-real-todo-stops-after-keyword-edit ()
  "A real DONE edit stops one managed clock and leaves only that edit locally."
  (unless (bound-and-true-p dorg-test--dolt-available)
    (ert-skip "Dolt unavailable"))
  (let ((file (make-temp-file "dorg-clock-todo-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+DORG_STORE_ID: tasks\n* TODO Task\n"
                    ":PROPERTIES:\n:DORG_ID: 9\n:END:\n"))
          (dorg-test--with-temp-db dolt-dir
            (dorg-sql-batch
             dolt-dir
             (dorg-test--parse-to-sql
              "* Clock authority\n:PROPERTIES:\n:DORG_ID: 71\n:DORG_CLOCK_KIND: scope\n:END:\n"
              100))
            (dorg-commit dolt-dir "seed clock scope")
            (let ((source (list :dolt-dir dolt-dir :root-id 100))
                  (now 1793536799)
                  (writes 0)
                  (original-execute (symbol-function 'dorg-api-execute)))
              (let ((task-buffer (find-file-noselect file)))
                (unwind-protect
                    (with-current-buffer task-buffer
                      (org-mode)
                      (goto-char (point-min))
                      (org-next-visible-heading 1)
                      (should (eq task-buffer (get-file-buffer file)))
                      (let ((before (buffer-string))
                      (org-todo-keywords '((sequence "TODO" "|" "DONE")))
                      (org-clock-out-when-done t)
                      (org-after-todo-state-change-hook
                       (list #'org-clock-out-if-current)))
                  (cl-letf (((symbol-function 'dorg-clock-emacs--now)
                             (lambda () now))
                            ((symbol-function 'dorg-api-execute)
                             (lambda (&rest args)
                               (cl-incf writes)
                               (apply original-execute args))))
                    (dorg-clock-emacs-enable source "71" "UTC")
                    (unwind-protect
                        (progn
                          (org-clock-in)
                          (should (= 1 writes))
                          (should (markerp dorg-clock-emacs--marker))
                          (setq now (1+ now))
                          (org-todo "DONE")
                          (should (= 2 writes))
                          (should-not (plist-get dorg-clock-emacs--state
                                                 :interval-id))
                          (should (buffer-modified-p))
                          (should (equal (replace-regexp-in-string
                                          "\\* TODO Task" "* DONE Task" before)
                                         (buffer-string)))
                          (should (equal before
                                         (with-temp-buffer
                                           (insert-file-contents file)
                                           (buffer-string)))))
                      (dorg-clock-emacs-mode -1))))
                  (when (buffer-live-p task-buffer)
                    (with-current-buffer task-buffer
                      (set-buffer-modified-p nil))
                    (kill-buffer task-buffer)))))))
      (delete-file file)))))

(ert-deftest dorg-test-clock-emacs-todo-edit-still-checks-file-and-lock ()
  "The TODO stop exception preserves external-change and lock refusals."
  (dorg-test-clock-emacs--with-buffer
   (set-buffer-modified-p t)
   (let ((fresh nil) (locked nil) (reads 0))
     (cl-letf (((symbol-function 'get-file-buffer)
                (lambda (&rest _) (current-buffer)))
               ((symbol-function 'verify-visited-file-modtime)
                (lambda (&rest _) fresh))
               ((symbol-function 'file-locked-p)
                (lambda (&rest _) locked))
               ((symbol-function 'dorg-clock-read)
                (lambda (&rest _) (cl-incf reads))))
       (should-error
        (dorg-clock-emacs--around-out
         (lambda (&rest _) (ert-fail "native")) nil t nil t)
        :type 'user-error)
       (setq fresh t locked "another editor")
       (should-error
        (dorg-clock-emacs--around-out
         (lambda (&rest _) (ert-fail "native")) nil t nil t)
        :type 'user-error)
       (should (= 0 reads))))))

(provide 'dorg-clock-emacs-tests)
;;; dorg-clock-emacs-tests.el ends here
