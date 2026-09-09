;;; dorg-freshness-tests.el --- Tests for dorg-freshness -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the shared content-hash primitive.  These pin the
;; behavior both `dorg-session' and `dorg-ext-read' depend on: a
;; raw-bytes SHA1 that flips on any content change, nil when the file
;; cannot be read.

;;; Code:

(require 'ert)
(require 'dorg-freshness)

(defmacro dorg-freshness-tests--with-file (var content &rest body)
  "Write CONTENT to a temp file bound to VAR, run BODY, then delete it."
  (declare (indent 2))
  `(let ((,var (make-temp-file "dorg-freshness-")))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'binary))
             (with-temp-file ,var (insert ,content)))
           ,@body)
       (delete-file ,var))))

(ert-deftest dorg-test-freshness-file-hash-matches-secure-hash ()
  "Hash of a readable file equals the raw-bytes SHA1 of its contents."
  (dorg-freshness-tests--with-file f "hello world\n"
    (should (equal (dorg-freshness-file-hash f)
                   (secure-hash 'sha1 "hello world\n")))))

(ert-deftest dorg-test-freshness-file-hash-nil-when-absent ()
  "A nonexistent file (and a nil FILE) hash to nil."
  (let ((missing (make-temp-name
                  (expand-file-name "dorg-fresh-missing-" temporary-file-directory))))
    (should-not (file-exists-p missing))
    (should (null (dorg-freshness-file-hash missing)))
    (should (null (dorg-freshness-file-hash nil)))))

(ert-deftest dorg-test-freshness-file-hash-stable-across-identical-content ()
  "Two files with byte-identical content hash equal."
  (dorg-freshness-tests--with-file a "same bytes"
    (dorg-freshness-tests--with-file b "same bytes"
      (should (equal (dorg-freshness-file-hash a)
                     (dorg-freshness-file-hash b))))))

(ert-deftest dorg-test-freshness-file-hash-flips-on-single-byte-change ()
  "A one-byte edit changes the hash — the whole point over a size/mtime signature.
Both files are the same size (same byte length), so a coarse signature
would collide; the content hash must not."
  (dorg-freshness-tests--with-file a "abcde"
    (dorg-freshness-tests--with-file b "abcdX"
      (should-not (equal (dorg-freshness-file-hash a)
                         (dorg-freshness-file-hash b))))))

(provide 'dorg-freshness-tests)
;;; dorg-freshness-tests.el ends here
