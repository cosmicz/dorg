;;; dorg-sortkey-tests.el --- Tests for dorg-sortkey -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the fractional sort key module.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-sortkey)

;;; from-integer / to-hex / from-hex round-trip

(ert-deftest dorg-test-sortkey-from-integer-basic ()
  "from-integer returns single-byte unibyte string."
  (should (equal (dorg-sortkey-from-integer 1) (unibyte-string 1)))
  (should (equal (dorg-sortkey-from-integer 255) (unibyte-string 255)))
  (should (equal (dorg-sortkey-from-integer 128) (unibyte-string 128))))

(ert-deftest dorg-test-sortkey-from-integer-rejects-invalid ()
  "from-integer rejects zero, negative, and non-integer values."
  (should-error (dorg-sortkey-from-integer 0))
  (should-error (dorg-sortkey-from-integer -1))
  (should-error (dorg-sortkey-from-integer 1.5)))

(ert-deftest dorg-test-sortkey-from-integer-multi-byte ()
  "from-integer produces correctly ordered multi-byte keys."
  (let ((k256 (dorg-sortkey-from-integer 256))
        (k257 (dorg-sortkey-from-integer 257))
        (k65535 (dorg-sortkey-from-integer 65535)))
    (should (= (length k256) 2))
    (should (= (length k65535) 2))
    (should (equal k256 (unibyte-string 1 0)))
    (should (string< k256 k257))
    (should (string< k257 k65535))))

(ert-deftest dorg-test-sortkey-from-integer-explicit-width ()
  "Explicit width produces uniform-length keys."
  (let ((k1 (dorg-sortkey-from-integer 1 2))
        (k255 (dorg-sortkey-from-integer 255 2))
        (k256 (dorg-sortkey-from-integer 256 2)))
    (should (= (length k1) 2))
    (should (= (length k255) 2))
    (should (= (length k256) 2))
    (should (equal k1 (unibyte-string 0 1)))
    (should (string< k1 k255))
    (should (string< k255 k256))))

(ert-deftest dorg-test-sortkey-from-integer-overflow ()
  "from-integer errors when N exceeds WIDTH capacity."
  (should-error (dorg-sortkey-from-integer 256 1))
  (should-error (dorg-sortkey-from-integer 65536 2)))

(ert-deftest dorg-test-sortkey-width-for-count ()
  "width-for-count returns correct byte widths."
  (should (= (dorg-sortkey-width-for-count 1) 1))
  (should (= (dorg-sortkey-width-for-count 255) 1))
  (should (= (dorg-sortkey-width-for-count 256) 2))
  (should (= (dorg-sortkey-width-for-count 65535) 2))
  (should (= (dorg-sortkey-width-for-count 65536) 3)))

(ert-deftest dorg-test-sortkey-300-siblings-ordered ()
  "300 siblings get valid, ordered, unique 2-byte sort keys."
  (let ((w (dorg-sortkey-width-for-count 300))
        (keys nil))
    (should (= w 2))
    (dotimes (i 300)
      (push (dorg-sortkey-from-integer (1+ i) w) keys))
    (setq keys (nreverse keys))
    ;; All unique
    (should (= (length (cl-remove-duplicates keys :test #'equal)) 300))
    ;; Strictly ordered
    (let ((prev (car keys)))
      (dolist (k (cdr keys))
        (should (string< prev k))
        (setq prev k)))))

(ert-deftest dorg-test-sortkey-to-hex ()
  "to-hex emits SQL hex literal."
  (should (equal (dorg-sortkey-to-hex (unibyte-string 1)) "X'01'"))
  (should (equal (dorg-sortkey-to-hex (unibyte-string 1 #x80)) "X'0180'"))
  (should (equal (dorg-sortkey-to-hex (unibyte-string #xff)) "X'ff'")))

(ert-deftest dorg-test-sortkey-from-hex-bare ()
  "from-hex parses bare hex string."
  (should (equal (dorg-sortkey-from-hex "01") (unibyte-string 1)))
  (should (equal (dorg-sortkey-from-hex "01FF") (unibyte-string 1 #xff)))
  (should (equal (dorg-sortkey-from-hex "0180") (unibyte-string 1 #x80))))

(ert-deftest dorg-test-sortkey-from-hex-wrapped ()
  "from-hex handles X'...' wrapper."
  (should (equal (dorg-sortkey-from-hex "X'01ff'") (unibyte-string 1 #xff))))

(ert-deftest dorg-test-sortkey-hex-round-trip ()
  "to-hex and from-hex round-trip."
  (let ((key (unibyte-string 3 #x80 #x40)))
    (should (equal (dorg-sortkey-from-hex (dorg-sortkey-to-hex key)) key))))

;;; Midpoint — basic cases

(ert-deftest dorg-test-sortkey-midpoint-basic ()
  "Midpoint between two keys with room."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string 1) (unibyte-string 3))))
    (should (string< (unibyte-string 1) mid))
    (should (string< mid (unibyte-string 3)))))

(ert-deftest dorg-test-sortkey-midpoint-wide-gap ()
  "Midpoint with wide gap."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string #x10) (unibyte-string #xf0))))
    (should (string< (unibyte-string #x10) mid))
    (should (string< mid (unibyte-string #xf0)))))

(ert-deftest dorg-test-sortkey-midpoint-adjacent ()
  "Adjacent keys extend to deeper byte."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string 1) (unibyte-string 2))))
    (should (string< (unibyte-string 1) mid))
    (should (string< mid (unibyte-string 2)))
    ;; Must be longer than 1 byte since there's no integer between 1 and 2
    (should (> (length mid) 1))))

(ert-deftest dorg-test-sortkey-midpoint-nil-before ()
  "nil A gives key before B."
  (let ((mid (dorg-sortkey-midpoint nil (unibyte-string 5))))
    (should (string< mid (unibyte-string 5)))))

(ert-deftest dorg-test-sortkey-midpoint-nil-after ()
  "nil B gives key after A."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string 5) nil)))
    (should (string< (unibyte-string 5) mid))))

(ert-deftest dorg-test-sortkey-midpoint-both-nil ()
  "Both nil gives a default key."
  (let ((mid (dorg-sortkey-midpoint nil nil)))
    (should (stringp mid))
    (should (> (length mid) 0))))

(ert-deftest dorg-test-sortkey-midpoint-error-on-equal ()
  "Signals error when A = B."
  (should-error (dorg-sortkey-midpoint (unibyte-string 5) (unibyte-string 5))))

(ert-deftest dorg-test-sortkey-midpoint-error-on-reversed ()
  "Signals error when A > B."
  (should-error (dorg-sortkey-midpoint (unibyte-string 5) (unibyte-string 3))))

;;; Midpoint — multi-byte keys

(ert-deftest dorg-test-sortkey-midpoint-multi-byte ()
  "Midpoint between multi-byte keys."
  (let* ((a (unibyte-string 1 #x80))
         (b (unibyte-string 2 0))
         (mid (dorg-sortkey-midpoint a b)))
    (should (string< a mid))
    (should (string< mid b))))

(ert-deftest dorg-test-sortkey-midpoint-different-lengths ()
  "Midpoint between keys of different lengths."
  (let* ((a (unibyte-string 1))
         (b (unibyte-string 1 #x80))
         (mid (dorg-sortkey-midpoint a b)))
    (should (string< a mid))
    (should (string< mid b))))

;;; Midpoint — stress test: repeated bisection

(ert-deftest dorg-test-sortkey-midpoint-repeated-bisection ()
  "20 repeated bisections between adjacent keys all produce valid ordering."
  (let ((keys (list (unibyte-string 1) (unibyte-string 2))))
    ;; Insert 20 keys between the first two
    (dotimes (_ 20)
      (let* ((a (car keys))
             (b (cadr keys))
             (mid (dorg-sortkey-midpoint a b)))
        (should (string< a mid))
        (should (string< mid b))
        ;; Insert after first key
        (setcdr keys (cons mid (cdr keys)))))
    ;; Verify entire sequence is strictly sorted
    (let ((prev (car keys)))
      (dolist (k (cdr keys))
        (should (string< prev k))
        (setq prev k)))))

(ert-deftest dorg-test-sortkey-midpoint-repeated-append ()
  "20 repeated appends (nil upper bound) produce strictly increasing keys."
  (let ((keys nil)
        (last-key (unibyte-string 1)))
    (dotimes (_ 20)
      (let ((next (dorg-sortkey-midpoint last-key nil)))
        (should (string< last-key next))
        (push next keys)
        (setq last-key next)))
    ;; All unique and ordered
    (setq keys (nreverse keys))
    (let ((prev (car keys)))
      (dolist (k (cdr keys))
        (should (string< prev k))
        (setq prev k)))))

(ert-deftest dorg-test-sortkey-midpoint-repeated-prepend ()
  "20 repeated prepends (nil lower bound) produce strictly decreasing keys."
  (let ((keys nil)
        (first-key (unibyte-string #x80)))
    (dotimes (_ 20)
      (let ((prev (dorg-sortkey-midpoint nil first-key)))
        (should (string< prev first-key))
        (push prev keys)
        (setq first-key prev)))
    ;; All unique and ordered (keys is in reverse-decreasing = increasing)
    (let ((prev (car keys)))
      (dolist (k (cdr keys))
        (should (string< prev k))
        (setq prev k)))))

;;; Edge cases

(ert-deftest dorg-test-sortkey-midpoint-low-boundary ()
  "Midpoint before \\x01."
  (let ((mid (dorg-sortkey-midpoint nil (unibyte-string 1))))
    (should (string< mid (unibyte-string 1)))))

(ert-deftest dorg-test-sortkey-midpoint-high-boundary ()
  "Midpoint after \\xff."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string #xff) nil)))
    (should (string< (unibyte-string #xff) mid))))

(ert-deftest dorg-test-sortkey-midpoint-fe-ff ()
  "Midpoint between \\xfe and \\xff."
  (let ((mid (dorg-sortkey-midpoint (unibyte-string #xfe) (unibyte-string #xff))))
    (should (string< (unibyte-string #xfe) mid))
    (should (string< mid (unibyte-string #xff)))))

;;; Storage limits & exhaustion

(ert-deftest dorg-test-sortkey-max-bytes-matches-schema ()
  "Storage limit constant is 64, matching VARBINARY(64)."
  (should (= dorg-sortkey-max-bytes 64))
  (should (= dorg-sortkey-rebalance-threshold 64)))

(ert-deftest dorg-test-sortkey-midpoint-limited-passes-short-keys ()
  "Limited midpoint returns the key when well under the threshold."
  (let ((mid (dorg-sortkey-midpoint-limited (unibyte-string 1)
                                            (unibyte-string 3))))
    (should (string< (unibyte-string 1) mid))
    (should (string< mid (unibyte-string 3)))
    (should (< (length mid) dorg-sortkey-rebalance-threshold))))

(ert-deftest dorg-test-sortkey-midpoint-limited-signals-at-boundary ()
  "Limited midpoint signals `dorg-sortkey-exhausted' at the cliff edge.
Two adjacent 63-byte keys bisect to a 64-byte candidate, which reaches
the rebalance threshold and must not be assigned."
  (let* ((lo (concat (make-string 62 0) (unibyte-string 1)))   ; 63 bytes …01
         (hi (concat (make-string 62 0) (unibyte-string 2))))  ; 63 bytes …02
    ;; Raw midpoint produces a 64-byte key (at the cliff)
    (should (>= (length (dorg-sortkey-midpoint lo hi))
                dorg-sortkey-rebalance-threshold))
    ;; Limited midpoint refuses it
    (should-error (dorg-sortkey-midpoint-limited lo hi)
                  :type 'dorg-sortkey-exhausted)))

(ert-deftest dorg-test-sortkey-midpoint-limited-detects-repeated-bisection ()
  "Repeated bisection is detected before producing a SQL-bound key.
Narrowing the gap toward a fixed lower bound grows the key length; the
limited allocator signals exhaustion and never returns a key that
reaches the threshold."
  (let ((lo (unibyte-string 1))
        (hi (unibyte-string 2))
        (exhausted nil))
    (cl-block done
      (dotimes (_ 500)
        (condition-case nil
            (let ((mid (dorg-sortkey-midpoint-limited lo hi)))
              ;; Every returned key stays strictly below the threshold
              (should (< (length mid) dorg-sortkey-rebalance-threshold))
              (setq hi mid))               ; narrow toward lo
          (dorg-sortkey-exhausted
           (setq exhausted t)
           (cl-return-from done)))))
    (should exhausted)))

(ert-deftest dorg-test-sortkey-ordered-p ()
  "`dorg-sortkey-ordered-p': strict order over unibyte keys.
Moved coverage for the relocated key-order predicate (tracked-regression)."
  (let ((k1 (dorg-sortkey-from-integer 1))
        (k2 (dorg-sortkey-from-integer 2))
        (k3 (dorg-sortkey-from-integer 3)))
    (should (dorg-sortkey-ordered-p nil))
    (should (dorg-sortkey-ordered-p (list k1)))
    (should (dorg-sortkey-ordered-p (list k1 k2 k3)))
    (should-not (dorg-sortkey-ordered-p (list k1 k3 k2)))
    (should-not (dorg-sortkey-ordered-p (list k2 k2)))))

;;; ─── tracked-regression relocation contract ─────────────────────────────────

(defconst dorg-sortkey-tests--qc260-repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-sortkey-tests--qc260-name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-sortkey-tests--qc260-repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-sortkey-ordered-p-relocation-contract ()
  "tracked-regression: the key-order predicate lives in dorg-sortkey.el.
RED at SPEC (new name undefined, retired diff-prefixed name still defined
and referenced); GREEN after IMPL."
  (let* ((old-name (concat "dorg-diff" "--keys-ordered-p"))
         (old (intern-soft old-name)))
    (should (fboundp 'dorg-sortkey-ordered-p))
    (should-not (and old (fboundp old)))
    (should-not (dorg-sortkey-tests--qc260-name-hits (list old-name)))))

(provide 'dorg-sortkey-tests)
;;; dorg-sortkey-tests.el ends here
