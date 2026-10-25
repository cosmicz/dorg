;;; dorg-envelope-tests.el --- Tests for command envelopes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
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

(ert-deftest dorg-test-envelope-keeps-string-object-keys-opaque ()
  "Only protocol symbols/keywords are normalized; caller strings are data."
  (let* ((obj (dorg-envelope-serialize
               (list :context_digest "ctx"
                     :results (vector '(("matched_seat" . "Alpha")
                                        ("BEAD_ID" . "x-1"))))))
         (row (aref (alist-get 'results obj) 0)))
    (should (equal "ctx" (alist-get 'context-digest obj)))
    (should (equal "Alpha" (alist-get "matched_seat" row nil nil #'equal)))
    (should (equal "x-1" (alist-get "BEAD_ID" row nil nil #'equal)))))

(ert-deftest dorg-test-envelope-explicit-null-encodes-as-wire-null ()
  "Explicit null markers become JSON null at every serialization depth."
  (let* ((normalized
          (dorg-envelope-success-json
           (list :base-commit :json-null
                 :nested (vector '(("OPAQUE_KEY" . :json-null)))
                 :sequence (list 7 :json-null))))
         (parsed (json-parse-string (json-encode normalized)
                                    :object-type 'alist :array-type 'array
                                    :null-object :wire-null
                                    :false-object :wire-false))
         (data (alist-get 'data parsed)))
    (should (eq t (alist-get 'ok parsed)))
    (should (eq :wire-null (alist-get 'error parsed)))
    (should (assq 'base-commit data))
    (should (eq :wire-null (alist-get 'base-commit data)))
    (should (eq :wire-null
                (alist-get 'OPAQUE_KEY (aref (alist-get 'nested data) 0))))
    (should (equal [7 :wire-null] (alist-get 'sequence data)))
    (should (equal normalized (dorg-envelope-serialize normalized)))))

(ert-deftest dorg-test-envelope-preserves-non-null-wire-types ()
  "Literal strings, booleans, empty arrays and omission stay distinct."
  (let* ((parsed
          (json-parse-string
           (json-encode
            (dorg-envelope-success-json
             (list :literal "json-null" :false :json-false :true t
                   :empty [] :base-commit "abc123"
                   :base-revision (list :kind 'plain-org :digest "content-v1")
                   :new-token "content-v2")))
           :object-type 'alist :array-type 'array
           :null-object :wire-null :false-object :wire-false))
         (data (alist-get 'data parsed)))
    (should (equal "json-null" (alist-get 'literal data)))
    (should (eq :wire-false (alist-get 'false data)))
    (should (eq t (alist-get 'true data)))
    (should (equal [] (alist-get 'empty data)))
    (should-not (assq 'omitted data))
    (should (equal "abc123" (alist-get 'base-commit data)))
    (should (equal '((kind . "plain-org") (digest . "content-v1"))
                   (alist-get 'base-revision data)))
    (should (equal "content-v2" (alist-get 'new-token data)))))

(ert-deftest dorg-test-envelope-error-preserves-null-and-false ()
  "Error envelopes retain false and null after actual JSON encoding."
  (let* ((parsed
          (json-parse-string
           (json-encode
            (dorg-envelope-error-json
             'invalid-target "json-null" (list :optional :json-null)))
           :object-type 'alist :null-object :wire-null
           :false-object :wire-false))
         (error (alist-get 'error parsed)))
    (should (eq :wire-false (alist-get 'ok parsed)))
    (should (eq :wire-null (alist-get 'data parsed)))
    (should (equal "invalid-target" (alist-get 'reason error)))
    (should (equal "json-null" (alist-get 'message error)))
    (should (eq :wire-null (alist-get 'optional (alist-get 'details error))))))

(provide 'dorg-envelope-tests)
;;; dorg-envelope-tests.el ends here
