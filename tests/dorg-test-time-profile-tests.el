;;; dorg-test-time-profile-tests.el --- Tests for bounded profiler -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'dorg)
(require 'dorg-test-helpers)
(require 'dorg-test-time-profile)

(ert-deftest dorg-test-time-profile-phase-totals-accumulate ()
  "Repeated observations of a phase produce one summed JSON-ready value."
  (let ((record (list :phases nil)))
    (setq record (dorg-test-time-profile--add-phase record 'import 0.25))
    (setq record (dorg-test-time-profile--add-phase record 'import 0.75))
    (should (= 1.0 (cdr (assq 'import (plist-get record :phases)))))))

(ert-deftest dorg-test-time-profile-detects-nohup-launch-only ()
  "Detached-launch accounting matches shell nohup, not ordinary commands."
  (should (dorg-test-time-profile--detached-launch-p
           '("sh" "-c" "nohup dolt sql-server >/tmp/x 2>&1 &")))
  (should-not (dorg-test-time-profile--detached-launch-p
               '("dolt" "sql" "-q" "SELECT 1"))))

(ert-deftest dorg-test-time-profile-disabled-and-enabled-advice-states ()
  "Every required profiler seam is absent when disabled and present when on."
  (when dorg-test-time-profile--enabled
    (ert-skip "profiled run must retain its active observer"))
  (let ((prior-file (getenv "DORG_TEST_TIME_PROFILE_FILE"))
        (file (make-temp-file "dorg-test-time-profile-" nil ".jsonl")))
    (unwind-protect
        (progn
          (delete-file file)
          (dorg-test-time-profile-disable)
          (dolist (symbol dorg-test-time-profile--expected-seams)
            (should (fboundp symbol))
            (should-not (advice-member-p
                         (dorg-test-time-profile--advice-name symbol) symbol)))
          (should-not (file-exists-p file))
          (setenv "DORG_TEST_TIME_PROFILE_FILE" file)
          (dorg-test-time-profile-enable)
          (should (equal dorg-test-time-profile--expected-seams
                         dorg-test-time-profile--installed-seams))
          (dolist (symbol dorg-test-time-profile--expected-seams)
            (should (advice-member-p
                     (dorg-test-time-profile--advice-name symbol) symbol))))
      (dorg-test-time-profile-disable)
      (setenv "DORG_TEST_TIME_PROFILE_FILE" prior-file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest dorg-test-time-profile-writes-jsonl-with-run-label ()
  "The opt-in writer emits a JSON object carrying the sample label."
  (let* ((file (make-temp-file "dorg-test-time-profile-" nil ".jsonl"))
         (dorg-test-time-profile--enabled t)
         (dorg-test-time-profile--label "baseline")
         (dorg-test-time-profile--temperature "cold")
         (dorg-test-time-profile--installed-seams '(ert-run-test mysql-query))
         (dorg-test-time-profile--output-file file)
         (dorg-test-time-profile--records (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (puthash 'dorg-test-time-profile-fixture
                   (list :test "dorg-test-time-profile-fixture"
                         :phases (list (cons 'bind 0.25)
                                       (cons 'import 1.0)
                                       (cons 'init 0.5))
                         :sql-entry-points 3 :mysql-queries 4
                         :call-processes 1 :start-processes 0
                         :detached-launches 0
                         :unclamped-body-seconds 0.5 :body-seconds 0.5)
                   dorg-test-time-profile--records)
          (dorg-test-time-profile-write)
          (dorg-test-time-profile-write)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((record (json-parse-string (buffer-string)
                                             :object-type 'alist :array-type 'list)))
              (should (equal "baseline" (alist-get 'label record)))
              (should (equal "cold" (alist-get 'temperature record)))
              (should (equal '("ert-run-test" "mysql-query")
                             (alist-get 'installed_seams record)))
              (should (equal '((init . 0.5) (import . 1.0) (bind . 0.25))
                             (alist-get 'phases record)))
              (should (= 3 (alist-get 'sql_entry_points record)))
              (should (= 4 (alist-get 'mysql_queries record)))
              (should (= 0.5 (alist-get 'unclamped_body_seconds record)))
              (should (= 0.5 (alist-get 'body_seconds record))))))
      (delete-file file))))

(provide 'dorg-test-time-profile-tests)
;;; dorg-test-time-profile-tests.el ends here
