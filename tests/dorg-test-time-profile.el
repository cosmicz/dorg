;;; dorg-test-time-profile.el --- Opt-in ERT fixture phase profiler -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `DORG_TEST_TIME_PROFILE_FILE' enables a JSONL profiler for a bounded ERT
;; sample.  It observes real functions with advice: it never replaces SQL,
;; process, or fixture behavior.  Its logical SQL entry-point count is not a
;; wire-cost measure; use its `mysql_queries' field for that comparison.  The
;; instrument is intentionally separate
;; from the QL fixture so a baseline can be measured before that fixture moves
;; to the shared-server seam.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)

(defvar dorg-test-time-profile--records (make-hash-table :test 'eq))
(defvar dorg-test-time-profile--current-test nil)
(defvar dorg-test-time-profile--enabled nil)
(defvar dorg-test-time-profile--output-file nil)
(defvar dorg-test-time-profile--label nil)
(defvar dorg-test-time-profile--temperature nil)
(defvar dorg-test-time-profile--installed-seams nil)

(defconst dorg-test-time-profile--fixture-phases
  '(owned-root test-db-create init import bind seed test-db-release cleanup)
  "Observed phases whose sum is outside a fixture test's body.")

(defun dorg-test-time-profile--record (test)
  "Return TEST's mutable profile plist, creating it if necessary."
  (or (gethash test dorg-test-time-profile--records)
      (let ((record (list :test (symbol-name test) :phases nil
                          :sql-entry-points 0 :mysql-queries 0 :call-processes 0
                          :start-processes 0 :detached-launches 0)))
        (puthash test record dorg-test-time-profile--records)
        record)))

(defun dorg-test-time-profile--add-phase (record phase seconds)
  "Add SECONDS to RECORD's named PHASE."
  (let* ((phases (plist-get record :phases))
         (cell (assq phase phases)))
    (if cell
        (setcdr cell (+ (cdr cell) seconds))
      (setq phases (cons (cons phase seconds) phases))
      (setq record (plist-put record :phases phases)))
    record))

(defun dorg-test-time-profile--inventory ()
  "Return fixture inventory when its public accessor is available."
  (when (fboundp 'dorg-dolt-fixture-inventory)
    (dorg-dolt-fixture-inventory)))

(defun dorg-test-time-profile--shared-server-pid ()
  "Return the current shared-server PID, if the fixture exposes one."
  (when (fboundp 'dorg-test--shared-server-pid)
    (dorg-test--shared-server-pid)))

(defun dorg-test-time-profile--phase-total (record)
  "Return RECORD's non-body fixture phase total."
  (cl-loop for (phase . seconds) in (plist-get record :phases)
           when (memq phase dorg-test-time-profile--fixture-phases)
           sum seconds))

(defun dorg-test-time-profile--around-ert-run-test (original test)
  "Time one real ERT TEST while keeping its result and failures unchanged."
  (let* ((name (ert-test-name test))
         (record (dorg-test-time-profile--record name))
         (start (float-time))
         (before (dorg-test-time-profile--inventory))
         (dorg-test-time-profile--current-test name))
    (setq record (plist-put record :inventory-before before)
          record (plist-put record :server-pid-before
                            (dorg-test-time-profile--shared-server-pid)))
    (unwind-protect
        (funcall original test)
      (setq record (dorg-test-time-profile--add-phase
                    record 'ert-total (- (float-time) start)))
      (let ((unclamped-body-seconds
             (- (cdr (assq 'ert-total (plist-get record :phases)))
                (dorg-test-time-profile--phase-total record))))
        (setq record (plist-put record :inventory-after
                                 (dorg-test-time-profile--inventory))
              record (plist-put record :server-pid-after
                                (dorg-test-time-profile--shared-server-pid))
              record (plist-put record :unclamped-body-seconds unclamped-body-seconds)
              record (plist-put record :body-seconds
                                (max 0.0 unclamped-body-seconds))))
      (puthash name record dorg-test-time-profile--records))))

(defun dorg-test-time-profile--around-phase (phase original &rest args)
  "Time the real ORIGINAL call as PHASE for the active ERT test."
  (let ((test dorg-test-time-profile--current-test)
        (start (float-time)))
    (unwind-protect
        (apply original args)
      (when test
        (let ((record (dorg-test-time-profile--record test)))
          (dorg-test-time-profile--add-phase
           record phase (- (float-time) start))
          (puthash test record dorg-test-time-profile--records))))))

(defun dorg-test-time-profile--detached-launch-p (args)
  "Return non-nil when ARGS describe a shell `nohup' detached launch."
  (string-match-p
   "\\(?:^\\|[[:space:]]\\)nohup\\(?:[[:space:]]\\|$\\)"
   (mapconcat (lambda (arg) (if (stringp arg) arg (format "%S" arg)))
              args " ")))

(defun dorg-test-time-profile--around-counter (counter original &rest args)
  "Count COUNTER for the active test and invoke ORIGINAL exactly once."
  (let ((test dorg-test-time-profile--current-test))
    (when test
      (let ((record (dorg-test-time-profile--record test)))
        (setq record (plist-put record counter
                                 (1+ (or (plist-get record counter) 0))))
        (when (and (memq counter '(:call-processes :start-processes))
                   (dorg-test-time-profile--detached-launch-p args))
          (setq record (plist-put record :detached-launches
                                   (1+ (or (plist-get record :detached-launches) 0)))))
        (puthash test record dorg-test-time-profile--records)))
    (apply original args)))

(defun dorg-test-time-profile--advice-name (symbol)
  "Return this profiler's stable advice name for SYMBOL."
  (intern (format "dorg-test-time-profile/%s" symbol)))

(defconst dorg-test-time-profile--phase-entries
  '((dorg-test--make-owned-temp-dir . owned-root)
    (dorg-test--create-test-db . test-db-create)
    (dorg-init . init)
    (dorg-import-document . import)
    (dorg-bind-dolt . bind)
    (dorg-test-ql--seed-document . seed)
    (dorg-test--destroy-test-db . test-db-release)
    (dorg-test--delete-owned-root . cleanup))
  "Functions whose real execution is attributed to fixture phases.")

(defconst dorg-test-time-profile--counter-entries
  '((dorg-sql . :sql-entry-points)
    (dorg-sql--at-revision . :sql-entry-points)
    (mysql-query . :mysql-queries)
    (call-process . :call-processes)
    (start-process . :start-processes)
    (dorg-dolt-server--spawn-logged . :start-processes))
  "Functions whose real execution is counted for the active test.")

(defconst dorg-test-time-profile--expected-seams
  (append '(ert-run-test)
          (mapcar #'car dorg-test-time-profile--phase-entries)
          (mapcar #'car dorg-test-time-profile--counter-entries))
  "Every function the profiler is expected to observe in a loaded test runner.")

(defun dorg-test-time-profile--install-phase-advice (symbol phase)
  "Observe SYMBOL as PHASE when it is defined."
  (when (fboundp symbol)
    (advice-add symbol :around
                (apply-partially #'dorg-test-time-profile--around-phase phase)
                `((name . ,(dorg-test-time-profile--advice-name symbol))))
    symbol))

(defun dorg-test-time-profile--install-counter-advice (symbol counter)
  "Count calls to SYMBOL as COUNTER when it is defined."
  (when (fboundp symbol)
    (advice-add symbol :around
                (apply-partially #'dorg-test-time-profile--around-counter counter)
                `((name . ,(dorg-test-time-profile--advice-name symbol))))
    symbol))

(defun dorg-test-time-profile-disable ()
  "Remove every profiler advice and reset opt-in process state.
Used by isolated unit tests; production batch runs leave profiling enabled
until their single JSONL write after ERT finishes."
  (dolist (symbol (cons 'ert-run-test
                         (mapcar #'car dorg-test-time-profile--phase-entries)
                         ))
    (when (fboundp symbol)
      (advice-remove symbol (dorg-test-time-profile--advice-name symbol))))
  (dolist (symbol (mapcar #'car dorg-test-time-profile--counter-entries))
    (when (fboundp symbol)
      (advice-remove symbol (dorg-test-time-profile--advice-name symbol))))
  (setq dorg-test-time-profile--enabled nil
        dorg-test-time-profile--output-file nil
        dorg-test-time-profile--label nil
        dorg-test-time-profile--temperature nil
        dorg-test-time-profile--installed-seams nil
        dorg-test-time-profile--records (make-hash-table :test 'eq)))

(defun dorg-test-time-profile-enable ()
  "Enable the opt-in bounded test profiler from its environment variable."
  (let ((output (getenv "DORG_TEST_TIME_PROFILE_FILE")))
    (when (and output (not (string-empty-p output))
               (not dorg-test-time-profile--enabled))
      (setq dorg-test-time-profile--enabled t
            dorg-test-time-profile--output-file (expand-file-name output)
            dorg-test-time-profile--label (or (getenv "DORG_TEST_TIME_PROFILE_LABEL")
                                               "unlabeled")
            dorg-test-time-profile--temperature
            (or (getenv "DORG_TEST_TIME_PROFILE_TEMPERATURE") "unspecified")
            dorg-test-time-profile--installed-seams nil)
      (advice-add 'ert-run-test :around #'dorg-test-time-profile--around-ert-run-test
                  `((name . ,(dorg-test-time-profile--advice-name 'ert-run-test))))
      (push 'ert-run-test dorg-test-time-profile--installed-seams)
      (dolist (entry dorg-test-time-profile--phase-entries)
        (when (dorg-test-time-profile--install-phase-advice (car entry) (cdr entry))
          (push (car entry) dorg-test-time-profile--installed-seams)))
      (dolist (entry dorg-test-time-profile--counter-entries)
        (when (dorg-test-time-profile--install-counter-advice (car entry) (cdr entry))
          (push (car entry) dorg-test-time-profile--installed-seams)))
      (setq dorg-test-time-profile--installed-seams
            (reverse dorg-test-time-profile--installed-seams)))))

(defun dorg-test-time-profile-write ()
  "Write bounded-profile records as JSONL, if profiling is enabled."
  (when dorg-test-time-profile--enabled
    (make-directory (file-name-directory dorg-test-time-profile--output-file) t)
    (with-temp-file dorg-test-time-profile--output-file
      (maphash
       (lambda (_test record)
         (insert
          (json-encode
           `((test . ,(plist-get record :test))
             (label . ,dorg-test-time-profile--label)
             (temperature . ,dorg-test-time-profile--temperature)
             (transport . ,(format "%s" (and (boundp 'dorg-sql-transport)
                                               dorg-sql-transport)))
             (autostart . ,(and (boundp 'dorg-dolt-server-autostart)
                                 dorg-dolt-server-autostart))
             (stop_term_grace . ,(and (boundp 'dorg-dolt-server-stop-term-grace)
                                       dorg-dolt-server-stop-term-grace))
             (installed_seams . ,(mapcar #'symbol-name
                                          dorg-test-time-profile--installed-seams))
             (phases . ,(mapcar (lambda (cell)
                                  (cons (symbol-name (car cell)) (cdr cell)))
                                (reverse (plist-get record :phases))))
             (sql_entry_points . ,(plist-get record :sql-entry-points))
             (mysql_queries . ,(plist-get record :mysql-queries))
             (call_processes . ,(plist-get record :call-processes))
             (start_processes . ,(plist-get record :start-processes))
             (detached_launches . ,(plist-get record :detached-launches))
             (inventory_before . ,(plist-get record :inventory-before))
             (inventory_after . ,(plist-get record :inventory-after))
             (server_pid_before . ,(plist-get record :server-pid-before))
             (server_pid_after . ,(plist-get record :server-pid-after))
             (unclamped_body_seconds .
                                     ,(plist-get record :unclamped-body-seconds))
             (body_seconds . ,(plist-get record :body-seconds))))
          "\n"))
       dorg-test-time-profile--records))
    (message "dorg-test: time profile -> %s" dorg-test-time-profile--output-file)))

(provide 'dorg-test-time-profile)
;;; dorg-test-time-profile.el ends here
