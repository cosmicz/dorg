;;; dorg-file-lock-tests.el --- Atomic local lock controls -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Exact owner-publication, liveness and reclaim controls for the leaf lock.
;; Every filesystem fixture is private; native file subrs are never mocked.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-file-lock)

(defvar dorg-test-file-lock--reader-side-effect nil)

(defmacro dorg-test-file-lock--with-lock (lock &rest body)
  "Create a private directory and bind LOCK to a lock path in it."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((dir (make-temp-file "dorg-file-lock-test-" t))
          (,lock (expand-file-name ".source.org.dorg-write.lock" dir)))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest dorg-test-file-lock-atomic-record-roundtrip-and-release ()
  "A non-ASCII owner record is read exactly from the held dangling link."
  (dorg-test-file-lock--with-lock lock
    (cl-letf (((symbol-function 'dorg-file-lock--host-name)
               (lambda () "hôte-東京")))
      (let ((result
             (dorg-file-lock-call
              lock 0
              (lambda (record)
                (should (equal record (file-symlink-p lock)))
                (should (dorg-file-lock--present-p lock))
                (should (equal (list :kind 'link :record record)
                               (dorg-file-lock--snapshot lock)))
                (should (equal "hôte-東京"
                               (alist-get :host (dorg-file-lock--read-owner lock))))
                (should (eq :live-holder
                            (plist-get (dorg-file-lock-inspect lock) :status)))
                :body-ran))))
        (should (eq :acquired (plist-get result :status)))
        (should (eq :body-ran (plist-get result :value)))
        (should-not (file-symlink-p lock))))))

(ert-deftest dorg-test-file-lock-owner-decoder-rejects-nondata ()
  "Invalid reader syntax and malformed owners decode to nil without effects."
  (let* ((dorg-test-file-lock--reader-side-effect nil)
         (record
          (concat dorg-file-lock--record-prefix
                  (base64-encode-string
                   "#.(setq dorg-test-file-lock--reader-side-effect t)" t))))
    (should-not (dorg-file-lock--decode-owner record))
    (should-not
     (dorg-file-lock--decode-owner
      (concat dorg-file-lock--record-prefix
              (base64-encode-string "(:host" t))))
    (should-not dorg-test-file-lock--reader-side-effect)))

(ert-deftest dorg-test-file-lock-post-publish-error-cleans-exact-record ()
  "An injected error after symlink(2), before owner-cell assignment, cleans it."
  (dorg-test-file-lock--with-lock lock
    (let ((real (symbol-function 'dorg-file-lock--publish))
          (published nil))
      (should-error
       (cl-letf (((symbol-function 'dorg-file-lock--publish)
                  (lambda (record path)
                    (funcall real record path)
                    (setq published t)
                    (error "after publish"))))
         (dorg-file-lock-call lock 0 #'ignore)))
      (should published)
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-post-publish-quit-cleans-exact-record ()
  "An injected quit after symlink(2), before owner-cell assignment, cleans it."
  (dorg-test-file-lock--with-lock lock
    (let ((real (symbol-function 'dorg-file-lock--publish))
          (published nil)
          (saw-quit nil))
      (condition-case nil
          (cl-letf (((symbol-function 'dorg-file-lock--publish)
                     (lambda (record path)
                       (funcall real record path)
                       (setq published t)
                       (signal 'quit nil))))
            (dorg-file-lock-call lock 0 #'ignore))
        (quit (setq saw-quit t)))
      (should published)
      (should saw-quit)
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-symlink-unsupported-is-typed ()
  "A filesystem refusal cannot be mistaken for contention or acquisition."
  (dorg-test-file-lock--with-lock lock
    (cl-letf (((symbol-function 'dorg-file-lock--publish)
               (lambda (_record _path)
                 (signal 'file-error '("symlinks unavailable")))))
      (should (eq :unsupported
                  (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-live-holder-zero-deadline-contends ()
  "A zero-deadline free probe succeeds, but a live holder refuses typed."
  (dorg-test-file-lock--with-lock lock
    (let ((outer
           (dorg-file-lock-call
            lock 0
            (lambda (_record)
              (should (file-symlink-p lock))
              (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))))
      (should (eq :acquired (plist-get outer :status)))
      (should (eq :live-holder (plist-get outer :value)))
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-positive-deadline-is-bounded ()
  "A live owner past a positive wait deadline produces a bounded refusal."
  (dorg-test-file-lock--with-lock lock
    (dorg-file-lock-call
     lock 0
     (lambda (_record)
       (let* ((started (float-time))
              (result (dorg-file-lock-call lock 0.03 #'ignore))
              (elapsed (- (float-time) started)))
         (should (eq :live-holder (plist-get result :status)))
         (should (< elapsed 1.0))
         (should (file-symlink-p lock)))))))

(ert-deftest dorg-test-file-lock-unobservable-own-identity-refuses-publication ()
  "An unobservable process birth cannot publish an unreclaimable lock."
  (dorg-test-file-lock--with-lock lock
    (cl-letf (((symbol-function 'dorg-file-lock--process-identity)
               (lambda (&rest _) (list :unknown))))
      (should (eq :unsupported
                  (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-dead-holder-is-reclaimed ()
  "A proven-dead PID can be replaced, with no lock left after release."
  (dorg-test-file-lock--with-lock lock
    (let* ((owner (dorg-file-lock--owner))
           (dead (copy-tree owner)))
      (setf (alist-get :pid dead) 999999999)
      (make-symbolic-link (dorg-file-lock--encode-owner dead) lock nil)
      (should (dorg-file-lock--owner-gone-p lock))
      (should (eq :acquired
                  (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))
      (should-not (file-symlink-p lock))
      (should-not (dorg-file-lock--present-p
                   (dorg-file-lock--tombstone lock))))))

(ert-deftest dorg-test-file-lock-unknown-holder-is-not-stolen ()
  "Malformed owner text remains present and returns holder-unknown."
  (dorg-test-file-lock--with-lock lock
    (make-symbolic-link "malformed-owner" lock nil)
    (should (eq :holder-unknown
                (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))
    (should (equal "malformed-owner" (file-symlink-p lock)))))

(ert-deftest dorg-test-file-lock-orphan-tombstone-blocks-reclaim ()
  "A killed reclaimer's fixed tombstone yields a named refusal."
  (dorg-test-file-lock--with-lock lock
    (make-directory (dorg-file-lock--tombstone lock))
    (should (eq :reclaim-blocked
                (plist-get (dorg-file-lock-call lock 0 #'ignore) :status)))
    (should-not (file-symlink-p lock))))

(ert-deftest dorg-test-file-lock-transient-tombstone-retries-until-free ()
  "A live reclaimer's temporary tombstone does not end a bounded wait."
  (dorg-test-file-lock--with-lock lock
    (let* ((tomb (dorg-file-lock--tombstone lock))
           (removed nil))
      (make-directory tomb)
      (run-at-time 0.2 nil
                   (lambda () (delete-directory tomb) (setq removed t)))
      (let ((result (dorg-file-lock-call lock 1.0 (lambda (_) :ran))))
        (should removed)
        (should (eq :acquired (plist-get result :status)))
        (should (eq :ran (plist-get result :value)))))))

(ert-deftest dorg-test-file-lock-releasing-unknown-holder-retries ()
  "An unknown holder that releases within the deadline permits acquisition."
  (dorg-test-file-lock--with-lock lock
    (let* ((owner (dorg-file-lock--owner))
           (released nil))
      (setf (alist-get :host owner) "other-host.invalid")
      (make-symbolic-link (dorg-file-lock--encode-owner owner) lock nil)
      (run-at-time 0.2 nil
                   (lambda () (delete-file lock) (setq released t)))
      (let ((result (dorg-file-lock-call lock 1.0 (lambda (_) :ran))))
        (should released)
        (should (eq :acquired (plist-get result :status)))
        (should (eq :ran (plist-get result :value)))))))

(ert-deftest dorg-test-file-lock-stale-release-cannot-remove-successor ()
  "A stale token release leaves a successor's exact record untouched."
  (dorg-test-file-lock--with-lock lock
    (let* ((first (dorg-file-lock--encode-owner (dorg-file-lock--owner)))
           (second (dorg-file-lock--encode-owner (dorg-file-lock--owner))))
      (make-symbolic-link second lock nil)
      (should-not (dorg-file-lock--release lock first))
      (should (equal second (file-symlink-p lock))))))

(defun dorg-test-file-lock--wait-for (predicate &optional seconds)
  "Pump child output until PREDICATE succeeds or SECONDS elapse."
  (let ((deadline (+ (float-time) (or seconds 15))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(ert-deftest dorg-test-file-lock-killed-publisher-has-no-ownerless-window ()
  "SIGKILL before publication leaves no lock; after it leaves a reclaimable owner."
  (dorg-test-file-lock--with-lock lock
    (dolist (phase '(before after))
      (let* ((dir (file-name-directory lock))
             (script (expand-file-name (format "%s.el" phase) dir))
             (ready (expand-file-name (format "%s.ready" phase) dir))
             (output (generate-new-buffer " *dorg-killed-publisher*"))
             (emacs (expand-file-name invocation-name invocation-directory))
             (root (file-name-directory (locate-library "dorg-file-lock")))
             (process nil))
        (with-temp-file script
          (insert ";;; -*- lexical-binding: t; -*-\n"
                  "(require 'cl-lib)\n(require 'dorg-file-lock)\n"
                  (prin1-to-string
                   `(let ((real (symbol-function 'dorg-file-lock--publish)))
                      (cl-letf (((symbol-function 'dorg-file-lock--publish)
                                 (lambda (record path)
                                   ,(when (eq phase 'after)
                                      '(funcall real record path))
                                   (with-temp-file ,ready (insert "ready"))
                                   (sleep-for 30)
                                   ,(when (eq phase 'before)
                                      '(funcall real record path)))))
                        (dorg-file-lock-call ,lock 0
                                             (lambda (_record) (sleep-for 30))))))
                  "\n"))
        (unwind-protect
            (progn
              (setq process
                    (make-process :name "dorg-lock-kill" :buffer output
                                  :command (list emacs "-Q" "-L" root "--batch"
                                                 "-l" script)))
              (should (dorg-test-file-lock--wait-for
                       (lambda () (file-exists-p ready))))
              (if (eq phase 'before)
                  (should-not (file-symlink-p lock))
                (should (string-prefix-p "dorg-lock-v1:" (file-symlink-p lock)))
                (should (dorg-file-lock--read-owner lock)))
              (signal-process (process-id process) 9)
              (should (dorg-test-file-lock--wait-for
                       (lambda () (not (process-live-p process)))))
              (if (eq phase 'before)
                  (should-not (file-symlink-p lock))
                (should (dorg-test-file-lock--wait-for
                         (lambda () (dorg-file-lock--owner-gone-p lock))))
                (should (eq :acquired
                            (plist-get (dorg-file-lock-call lock 0 #'ignore)
                                       :status)))
                (should-not (file-symlink-p lock))))
          (when (and process (process-live-p process))
            (signal-process (process-id process) 9))
          (kill-buffer output))))))

(ert-deftest dorg-test-file-lock-killed-reclaimer-blocks-successors ()
  "SIGKILL of the elected reclaimer leaves a tombstone and blocks entry."
  (dorg-test-file-lock--with-lock lock
    (let* ((dir (file-name-directory lock))
           (script (expand-file-name "reclaim.el" dir))
           (ready (expand-file-name "reclaim.ready" dir))
           (output (generate-new-buffer " *dorg-killed-reclaimer*"))
           (emacs (expand-file-name invocation-name invocation-directory))
           (root (file-name-directory (locate-library "dorg-file-lock")))
           (owner (dorg-file-lock--owner))
           (process nil))
      (setf (alist-get :pid owner) 999999999)
      (make-symbolic-link (dorg-file-lock--encode-owner owner) lock nil)
      (with-temp-file script
        (insert ";;; -*- lexical-binding: t; -*-\n"
                "(require 'cl-lib)\n(require 'dorg-file-lock)\n"
                (prin1-to-string
                 `(let ((real (symbol-function 'dorg-file-lock--mkdir-exclusive)))
                    (cl-letf (((symbol-function 'dorg-file-lock--mkdir-exclusive)
                               (lambda (path)
                                 (let ((won (funcall real path)))
                                   (when won
                                     (with-temp-file ,ready (insert "ready"))
                                     (sleep-for 30))
                                   won))))
                      (dorg-file-lock-call ,lock 0 #'ignore))))
                "\n"))
      (unwind-protect
          (progn
            (setq process
                  (make-process :name "dorg-reclaim-kill" :buffer output
                                :command (list emacs "-Q" "-L" root "--batch"
                                               "-l" script)))
            (should (dorg-test-file-lock--wait-for
                     (lambda () (file-exists-p ready))))
            (should (file-directory-p (dorg-file-lock--tombstone lock)))
            (signal-process (process-id process) 9)
            (should (dorg-test-file-lock--wait-for
                     (lambda () (not (process-live-p process)))))
            (should (eq :reclaim-blocked
                        (plist-get (dorg-file-lock-call lock 0 #'ignore)
                                   :status)))
            (should (file-symlink-p lock)))
        (when (and process (process-live-p process))
          (signal-process (process-id process) 9))
        (kill-buffer output)))))

(provide 'dorg-file-lock-tests)
;;; dorg-file-lock-tests.el ends here
