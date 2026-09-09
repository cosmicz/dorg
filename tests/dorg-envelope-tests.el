;;; dorg-envelope-tests.el --- Tests for command envelopes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'dorg-envelope)

(ert-deftest dorg-test-envelope-success-shape ()
  (let ((obj (dorg-envelope-success-json
              (list :command 'query :results [] :count 0))))
    (should (eq t (alist-get 'ok obj)))
    (should (assq 'data obj))
    (should (assq 'error obj))
    (should (null (alist-get 'error obj)))))

(ert-deftest dorg-test-envelope-error-shape ()
  (let* ((obj (dorg-envelope-error-json
               'mode-error "bad mode" (list :dolt_dir "/tmp/x")))
         (err (alist-get 'error obj)))
    (should (eq :json-false (alist-get 'ok obj)))
    (should (null (alist-get 'data obj)))
    (should (eq 'mode-error (alist-get 'reason err)))
    (should (equal "bad mode" (alist-get 'message err)))
    (should (equal "/tmp/x" (alist-get 'dolt-dir (alist-get 'details err))))
    (should-not (assq 'dolt_dir (alist-get 'details err)))))

(ert-deftest dorg-test-envelope-recursive-kebab-and-vectors ()
  (let* ((obj (dorg-envelope-serialize
               (list :heading_id 7
                     :base-commit "c"
                     :items (vector (list :dolt_dir "/d" :ok t)))))
         (items (alist-get 'items obj)))
    (should (equal 7 (alist-get 'heading-id obj)))
    (should (equal "c" (alist-get 'base-commit obj)))
    (should (vectorp items))
    (should (equal "/d" (alist-get 'dolt-dir (aref items 0))))
    (should-not (assq 'dolt_dir (aref items 0)))))

(provide 'dorg-envelope-tests)
;;; dorg-envelope-tests.el ends here
