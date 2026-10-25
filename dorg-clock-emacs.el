;;; dorg-clock-emacs.el --- Opt-in local clock reconciliation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; This package runs only inside an Emacs session that explicitly enables it.
;; It intercepts managed Org clock actions before Org edits the buffer.  The
;; clock participant in the named Dolt authority is the only durable state;
;; this buffer's marker and mode line are read-only derivatives.  Stock Org
;; clocks continue to operate in buffers without this mode.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'dorg-clock)
(require 'dorg-id)
(require 'dorg-projection)
(require 'dorg-registry)

(defvar org-state)

(defvar-local dorg-clock-emacs--config nil
  "Explicit authority source, scope and zone for this buffer.")
(defvar-local dorg-clock-emacs-mode nil
  "Non-nil in buffers that opted into managed clock reconciliation.")
(defvar-local dorg-clock-emacs--state nil
  "Last read-only clock snapshot for this buffer.")
(defvar-local dorg-clock-emacs--marker nil
  "Local heading marker derived from the last authority read.")
(defvar-local dorg-clock-emacs--pending-intent nil
  "Immutable intent retained when an apply response is unknown.")
(defvar dorg-clock-emacs--buffers nil
  "Buffers with an enabled local adapter; shared advice uses this refcount.")
(defvar dorg-clock-emacs--timer nil
  "One shared read-only timer while the enabled-buffer refcount is nonzero.")

(defun dorg-clock-emacs--now ()
  "Return an exact UTC epoch second captured for one action."
  (floor (float-time)))

(defun dorg-clock-emacs--defer (message)
  "Refuse an unsafe local action with MESSAGE."
  (user-error "DOrg clock deferred: %s" message))

(defun dorg-clock-emacs--ready (&optional todo-edit)
  "Check that the local view is safe for a managed clock action.
TODO-EDIT permits the keyword change already made by `org-todo' before its
completion hook.  The file freshness and lock checks still apply."
  (unless (and dorg-clock-emacs-mode dorg-clock-emacs--config
               (derived-mode-p 'org-mode))
    (dorg-clock-emacs--defer "adapter is not configured"))
  (when (and (buffer-modified-p) (not todo-edit))
    (dorg-clock-emacs--defer "buffer has unsaved edits"))
  (when (and buffer-file-name
             (eq (get-file-buffer buffer-file-name) (current-buffer))
             (not (verify-visited-file-modtime (current-buffer))))
    (dorg-clock-emacs--defer "visited file changed on disk"))
  (when buffer-file-name
    (let ((locker (file-locked-p buffer-file-name)))
      ;; Org's in-flight TODO edit owns the visited file lock in this Emacs.
      (when (and locker (not (and todo-edit (eq locker t))))
        (dorg-clock-emacs--defer "file is locked"))))
  (when (org-clocking-p)
    (dorg-clock-emacs--defer "a native Org clock is already running")))

(defun dorg-clock-emacs--task-ref-at-point ()
  "Return the stable, source-qualified DORG_ID task reference at point.
No ID is minted in a read-only projection or a plain file by this adapter."
  (let* ((id (save-excursion
               (org-back-to-heading t)
               (dorg-id-at-point t)))
         (view (dorg-projection-view-context))
         (registration (dorg-current-registration))
         (backend (and registration (dorg-registration-backend registration))))
    (unless id
      (dorg-clock-emacs--defer "heading has no stable DORG_ID"))
    (cond
     ((and (dorg-backend-dolt-p backend)
           (integerp (dorg-backend-root-id backend))
           (equal (plist-get view :root-id)
                  (dorg-backend-root-id backend))
           (memq (plist-get view :scope) '(whole partial)))
      (unless (equal (plist-get view :revision)
                     (dorg-head-commit (dorg-backend-dolt-dir backend)))
        (dorg-clock-emacs--defer "task projection is stale"))
      (let ((ref (dorg-clock-task-ref
                  (list :dolt-dir (dorg-backend-dolt-dir backend)
                        :root-id (dorg-backend-root-id backend)) id)))
        (unless (plist-get ref :canonical)
          (dorg-api--raise 'unresolved "Dolt store has no DOrg store ID"
                           :detail 'store-unidentified))
        (plist-get ref :canonical)))
     ((and (dorg-backend-plain-org-p backend) buffer-file-name
           (null view))
      (let ((ref (dorg-clock-task-ref (list :file buffer-file-name) id)))
        (unless (plist-get ref :canonical)
          (dorg-api--raise 'unresolved "task root has no DORG_STORE_ID"
                           :detail 'store-unidentified))
        (plist-get ref :canonical)))
     (t (dorg-clock-emacs--defer "task view has no unambiguous source")))))

(defun dorg-clock-emacs--validate-task-buffer ()
  "Refuse ambiguous task identities and foreign open CLOCK lines."
  (condition-case nil
      (let ((parsed (dorg-parse-assert-unique-ids
                     (dorg-parse-buffer (current-buffer)))))
        (dolist (heading (dorg-parse-result-headings parsed))
          (dolist (line (dorg-clock--legacy-lines heading))
            (when (and (string-match-p "\\`[ \t]*CLOCK:" line)
                       (not (string-match-p "--\\[" line)))
              (dorg-clock-emacs--defer
               "foreign open CLOCK line needs explicit adoption")))))
    (dorg-parse-duplicate-id
     (dorg-clock-emacs--defer "task buffer has duplicate DORG_ID values"))))

(defun dorg-clock-emacs--read (&optional observed)
  "Read this buffer's configured scope at OBSERVED epoch second."
  (dorg-clock-read
   (plist-get dorg-clock-emacs--config :scope-id)
   :source (plist-get dorg-clock-emacs--config :source)
   :observed-at (or observed (dorg-clock-emacs--now))))

(defun dorg-clock-emacs--show (snapshot)
  "Publish SNAPSHOT only to local display state, without any authority write."
  (setq dorg-clock-emacs--state snapshot)
  (when (markerp dorg-clock-emacs--marker)
    (set-marker dorg-clock-emacs--marker nil))
  (setq dorg-clock-emacs--marker nil)
  (when (and (plist-get snapshot :interval-id)
             (not (buffer-modified-p)))
    (let* ((task (plist-get snapshot :task-ref))
           (id (and (stringp task)
                    (string-match "\\([1-9][0-9]*\\)\\'" task)
                    (string-to-number (match-string 1 task))))
           (markers (and id (dorg-projection-scan-markers)))
           (candidate (and markers (gethash id markers))))
      (when candidate
        (save-excursion
          (goto-char candidate)
          (condition-case nil
              (when (equal task (dorg-clock-emacs--task-ref-at-point))
                (setq dorg-clock-emacs--marker candidate))
            (error nil))))
      (when markers
        (maphash (lambda (_ marker)
                   (unless (eq marker dorg-clock-emacs--marker)
                     (set-marker marker nil)))
                 markers))))
  (force-mode-line-update)
  snapshot)

(defun dorg-clock-emacs-reconcile ()
  "Refresh this buffer's derived clock state without mutation or Org hooks."
  (interactive)
  (unless dorg-clock-emacs-mode
    (dorg-clock-emacs--defer "adapter is not enabled"))
  (dorg-clock-emacs--show (dorg-clock-emacs--read)))

(defun dorg-clock-emacs--mode-line ()
  "Return the local authority clock indicator."
  (if (plist-get dorg-clock-emacs--state :interval-id)
      (format " DClock[%s]" (plist-get dorg-clock-emacs--state :interval-id))
    " DClock"))

(defun dorg-clock-emacs--execute (action snapshot instant &optional task)
  "Execute ACTION against SNAPSHOT at INSTANT for optional TASK."
  (when dorg-clock-emacs--pending-intent
    (dorg-clock-emacs--defer "retry the pending intent before a new action"))
  (let* ((active (plist-get snapshot :interval-id))
         (interval (if (memq action '(start switch))
                       (number-to-string (dorg-id-generate))
                     active))
         (op (format "clock-%s" (dorg-id-generate)))
         (intent
          (dorg-clock-make-intent
           action :source (plist-get dorg-clock-emacs--config :source)
           :base (plist-get snapshot :snapshot-token)
           :scope-id (plist-get dorg-clock-emacs--config :scope-id)
           :expected-active active :interval-id interval
           :task-ref task :instant-utc instant
           :zone (plist-get dorg-clock-emacs--config :zone)
           :op-id op)))
    (setq dorg-clock-emacs--pending-intent intent)
    (condition-case err
        (prog1 (dorg-api-execute intent :mode 'apply)
          (setq dorg-clock-emacs--pending-intent nil))
      (dorg-api-error
       (unless (eq 'indeterminate (dorg-error-reason err))
         (setq dorg-clock-emacs--pending-intent nil))
       (signal (car err) (cdr err))))))

(defun dorg-clock-emacs-retry-pending ()
  "Retry the exact immutable intent retained after an unknown response.
The same operation ID and original base are used; no new clock action is
inferred from the current buffer or current time."
  (interactive)
  (unless dorg-clock-emacs--pending-intent
    (dorg-clock-emacs--defer "no pending intent"))
  (let ((result (condition-case err
                    (dorg-api-execute dorg-clock-emacs--pending-intent
                                      :mode 'apply)
                  (dorg-api-error
                   (unless (eq 'indeterminate (dorg-error-reason err))
                     (setq dorg-clock-emacs--pending-intent nil))
                   (signal (car err) (cdr err))))))
    (setq dorg-clock-emacs--pending-intent nil)
    (dorg-clock-emacs--after-commit result)))

(defun dorg-clock-emacs--after-commit (result)
  "Reconcile after durable RESULT, preserving its success if the view defers."
  (condition-case err
      (progn (dorg-clock-emacs-reconcile) result)
    (error
     (list :authority-result result :reconciliation 'deferred
           :message (error-message-string err)))))

(defun dorg-clock-emacs--around-in (original &optional select start-time)
  "Route managed `org-clock-in' before ORIGINAL can edit an Org buffer."
  (if (not dorg-clock-emacs-mode)
      (funcall original select start-time)
    (dorg-clock-emacs--ready)
    (when (or select start-time org-clock-resolving-clocks-due-to-idleness
              org-clock-continuously org-clock-in-switch-to-state
              (and org-clock-in-resume
                   (not (eq org-clock-in-resume 'auto-restart))))
      (dorg-clock-emacs--defer "native selection or time adjustment needs an explicit clock intent"))
    (if (eq org-clock-in-resume 'auto-restart)
        (dorg-clock-emacs-reconcile)
      (progn
        (dorg-clock-emacs--validate-task-buffer)
        (let* ((instant (dorg-clock-emacs--now))
               (snapshot (dorg-clock-emacs--read instant))
               (task (dorg-clock-emacs--task-ref-at-point))
               (active (plist-get snapshot :interval-id))
               (action (if active 'switch 'start)))
          (if (and active (equal task (plist-get snapshot :task-ref)))
              (dorg-clock-emacs-reconcile)
            (dorg-clock-emacs--after-commit
             (dorg-clock-emacs--execute action snapshot instant task))))))))

(defun dorg-clock-emacs--around-out
    (original &optional switch-state quiet at-time todo-edit)
  "Route managed `org-clock-out' before ORIGINAL can edit a CLOCK line."
  (if (not dorg-clock-emacs-mode)
      (funcall original switch-state quiet at-time)
    (dorg-clock-emacs--ready todo-edit)
    (when (or switch-state at-time org-clock-out-switch-to-state)
      (dorg-clock-emacs--defer "native state change or time adjustment needs an explicit clock intent"))
    (let* ((instant (dorg-clock-emacs--now))
           (snapshot (dorg-clock-emacs--read instant)))
      (if (not (plist-get snapshot :interval-id))
          (if quiet
              (dorg-clock-emacs--show snapshot)
            (dorg-clock-emacs--defer "no active interval"))
        (dorg-clock-emacs--after-commit
         (dorg-clock-emacs--execute 'stop snapshot instant))))))

(defun dorg-clock-emacs--around-cancel (original)
  "Route managed `org-clock-cancel' before ORIGINAL can remove a line."
  (if (not dorg-clock-emacs-mode)
      (funcall original)
    (dorg-clock-emacs--ready)
    (let* ((instant (dorg-clock-emacs--now))
           (snapshot (dorg-clock-emacs--read instant)))
      (unless (plist-get snapshot :interval-id)
        (dorg-clock-emacs--defer "no active interval to cancel"))
      (dorg-clock-emacs--after-commit
       (dorg-clock-emacs--execute 'cancel snapshot instant)))))

(defun dorg-clock-emacs--around-in-last (original &rest args)
  "Keep native history selection from mutating a managed view."
  (if dorg-clock-emacs-mode
      (dorg-clock-emacs--defer "clock-in-last requires an explicit task")
    (apply original args)))

(defun dorg-clock-emacs--around-resolve-clock (original clock &rest args)
  "Keep native idle resolution from editing a managed CLOCK line."
  (let ((buffer (and (consp clock) (markerp (car clock))
                     (marker-buffer (car clock)))))
    (if (and buffer (buffer-local-value 'dorg-clock-emacs-mode buffer))
        (dorg-clock-emacs--defer "idle clock resolution needs an explicit intent")
      (apply original clock args))))

(defun dorg-clock-emacs--around-out-if-current (original)
  "Handle Org's TODO-completion clock-out boundary for managed buffers."
  (if (not dorg-clock-emacs-mode)
      (funcall original)
    (when (and (plist-get dorg-clock-emacs--state :interval-id)
               (or (and (eq t org-clock-out-when-done)
                        (member org-state org-done-keywords))
                   (and (listp org-clock-out-when-done)
                        (member org-state org-clock-out-when-done)))
               (markerp dorg-clock-emacs--marker)
               (eq (marker-buffer dorg-clock-emacs--marker)
                   (current-buffer))
               (save-excursion
                 (org-back-to-heading t)
                 (= (point) (marker-position dorg-clock-emacs--marker))))
      (condition-case err
          (dorg-clock-emacs--around-out (lambda (&rest _) nil) nil t nil t)
        (user-error
         (message "%s" (error-message-string err)))))))

(defconst dorg-clock-emacs--advices
  '((org-clock-in . dorg-clock-emacs--around-in)
    (org-clock-out . dorg-clock-emacs--around-out)
    (org-clock-cancel . dorg-clock-emacs--around-cancel)
    (org-clock-in-last . dorg-clock-emacs--around-in-last)
    (org-clock-resolve-clock . dorg-clock-emacs--around-resolve-clock)
    (org-clock-out-if-current . dorg-clock-emacs--around-out-if-current))
  "Pre-edit Org boundaries intercepted while at least one buffer opts in.")

(defun dorg-clock-emacs--tick ()
  "Read active managed buffers; never write from an idle timer."
  (setq dorg-clock-emacs--buffers
        (cl-remove-if-not #'buffer-live-p dorg-clock-emacs--buffers))
  (unless dorg-clock-emacs--buffers
    (dorg-clock-emacs--uninstall))
  (dolist (buffer dorg-clock-emacs--buffers)
    (with-current-buffer buffer
      (when (and dorg-clock-emacs-mode (not (buffer-modified-p)))
        (condition-case err
            (dorg-clock-emacs-reconcile)
          (error (message "DOrg clock reconciliation deferred: %s"
                          (error-message-string err))))))))

(defun dorg-clock-emacs--install ()
  "Install pre-edit advice and a read-only idle reconciliation timer."
  (dolist (cell dorg-clock-emacs--advices)
    (advice-add (car cell) :around (cdr cell)))
  (setq dorg-clock-emacs--timer
        (run-with-idle-timer 30 t #'dorg-clock-emacs--tick)))

(defun dorg-clock-emacs--uninstall ()
  "Remove advice and the timer when no managed buffer remains."
  (dolist (cell dorg-clock-emacs--advices)
    (advice-remove (car cell) (cdr cell)))
  (when dorg-clock-emacs--timer
    (cancel-timer dorg-clock-emacs--timer)
    (setq dorg-clock-emacs--timer nil)))

(defun dorg-clock-emacs--on-kill ()
  "Release this buffer's adapter before its local state disappears."
  (when dorg-clock-emacs-mode
    (dorg-clock-emacs-mode -1)))

(defun dorg-clock-emacs--may-kill-p ()
  "Keep an indeterminate operation available for exact retry."
  (if dorg-clock-emacs--pending-intent
      (progn
        (message "DOrg clock deferred: retry the pending intent before killing this buffer")
        nil)
    t))

(define-minor-mode dorg-clock-emacs-mode
  "Reconcile this Org buffer with an explicitly named DOrg clock authority.
Call `dorg-clock-emacs-enable' with SOURCE, SCOPE-ID and ZONE to configure."
  :lighter (:eval (dorg-clock-emacs--mode-line))
  (if dorg-clock-emacs-mode
      (progn
        (unless (and dorg-clock-emacs--config (derived-mode-p 'org-mode))
          (setq dorg-clock-emacs-mode nil)
          (dorg-clock-emacs--defer "configure an Org buffer first"))
        (when (org-clocking-p)
          (setq dorg-clock-emacs-mode nil)
          (dorg-clock-emacs--defer
           "stop the running native Org clock before enabling"))
        (unless dorg-clock-emacs--buffers
          (dorg-clock-emacs--install))
        (cl-pushnew (current-buffer) dorg-clock-emacs--buffers)
        (add-hook 'kill-buffer-hook #'dorg-clock-emacs--on-kill nil t)
        (add-hook 'kill-buffer-query-functions #'dorg-clock-emacs--may-kill-p nil t)
        (condition-case err
            (dorg-clock-emacs-reconcile)
          (error
           (dorg-clock-emacs-mode -1)
           (signal (car err) (cdr err)))))
    (when dorg-clock-emacs--pending-intent
      (setq dorg-clock-emacs-mode t)
      (dorg-clock-emacs--defer "retry the pending intent before disabling"))
    (setq dorg-clock-emacs--buffers
          (delq (current-buffer) dorg-clock-emacs--buffers))
    (remove-hook 'kill-buffer-hook #'dorg-clock-emacs--on-kill t)
    (remove-hook 'kill-buffer-query-functions #'dorg-clock-emacs--may-kill-p t)
    (when (markerp dorg-clock-emacs--marker)
      (set-marker dorg-clock-emacs--marker nil))
    (setq dorg-clock-emacs--marker nil dorg-clock-emacs--state nil)
    (unless dorg-clock-emacs--buffers
      (dorg-clock-emacs--uninstall))))

(defun dorg-clock-emacs-enable (source scope-id zone)
  "Opt this Org buffer into SOURCE's managed SCOPE-ID using IANA ZONE.
SOURCE is an explicit Dolt descriptor.  Neither enabling nor reconciling
starts, stops or resumes an interval."
  (interactive
   (list (list :dolt-dir (read-directory-name "Clock authority Dolt dir: ")
               :root-id (read-number "Clock authority root DORG_ID: "))
         (read-string "Clock scope DORG_ID: ")
         (read-string "Clock IANA zone: " "UTC")))
  (unless (and (derived-mode-p 'org-mode)
               (stringp scope-id) (dorg-clock--decimal-id-p scope-id)
               (dorg-clock--zone-p zone)
               (let ((resolved (dorg-mutate--resolve-source source)))
                 (and resolved (eq 'dolt (dorg-mutate-source-kind resolved)))))
    (dorg-clock-emacs--defer "requires Org, Dolt authority, scope ID and IANA zone"))
  (when dorg-clock-emacs--pending-intent
    (dorg-clock-emacs--defer "retry the pending intent before reconfiguration"))
  (setq dorg-clock-emacs--config
        (list :source (copy-tree source) :scope-id scope-id :zone zone))
  (dorg-clock-emacs-mode 1))

(provide 'dorg-clock-emacs)
;;; dorg-clock-emacs.el ends here
