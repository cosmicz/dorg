;;; dorg-sortkey-tests.el --- Tests for dorg-sortkey -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the fixed-width sort key module: hex round trips,
;; interval bounds over stored keys of any length, step allocation at
;; the head and tail, and spread allocation inside an interval.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-id)
(require 'dorg-sortkey)

(defconst dorg-test-sortkey--live-tail
  "03ffffffffffffffffffffffffffffffffffdf"
  "Hex of the 19-byte legacy tail key observed on a live root fan.")

(defun dorg-test-sortkey--ordered-p (keys)
  "Non-nil when KEYS are strictly increasing unibyte strings."
  (dorg-sortkey-ordered-p keys))

;;; Hex round trips

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

(ert-deftest dorg-test-sortkey-from-integer ()
  "from-integer builds ordered small keys, honours WIDTH, rejects bad input."
  (should (equal (dorg-sortkey-from-integer 1) (unibyte-string 1)))
  (should (equal (dorg-sortkey-from-integer 255) (unibyte-string 255)))
  (should (equal (dorg-sortkey-from-integer 256) (unibyte-string 1 0)))
  (should (equal (dorg-sortkey-from-integer 1 2) (unibyte-string 0 1)))
  (should (string< (dorg-sortkey-from-integer 256 2) (dorg-sortkey-from-integer 257 2)))
  (should (= (length (dorg-sortkey-from-integer 42 dorg-sortkey-boundary-width)) 16))
  (should-error (dorg-sortkey-from-integer 0))
  (should-error (dorg-sortkey-from-integer 256 1)))

;;; Encode / decode

(ert-deftest dorg-test-sortkey-encode-width-and-order ()
  "encode produces exactly 16 bytes and preserves integer order bytewise."
  (let ((a (dorg-sortkey-encode 1))
        (b (dorg-sortkey-encode (ash 1 64)))
        (c (dorg-sortkey-encode dorg-sortkey-boundary-max)))
    (should (= (length a) dorg-sortkey-boundary-width))
    (should (= (length c) dorg-sortkey-boundary-width))
    (should (dorg-test-sortkey--ordered-p (list a b c)))
    (should (= (dorg-sortkey-int16 b) (ash 1 64)))
    (should-error (dorg-sortkey-encode (1+ dorg-sortkey-boundary-max))
                  :type 'dorg-sortkey-exhausted)
    (should-error (dorg-sortkey-encode -1) :type 'dorg-sortkey-exhausted)))

(ert-deftest dorg-test-sortkey-int16-is-a-prefix-coordinate ()
  "int16 reads the 16-byte prefix, zero-padding shorter keys."
  (should (= (dorg-sortkey-int16 (unibyte-string #x80)) dorg-sortkey-boundary-center))
  (should (= (dorg-sortkey-int16 (dorg-sortkey-from-hex "4142"))
             (dorg-sortkey-int16 (dorg-sortkey-from-hex "414200"))))
  (should (= (dorg-sortkey-int16 (dorg-sortkey-from-hex dorg-test-sortkey--live-tail))
             (dorg-sortkey-int16 (dorg-sortkey-from-hex "03ffffffffffffffffffffffffffffff")))))

;;; Bounds: the open interval strictly between two stored keys of any length

(ert-deftest dorg-test-sortkey-bounds-open-domain ()
  "No neighbours means the whole domain."
  (should (equal (dorg-sortkey-bounds nil nil)
                 (cons 1 dorg-sortkey-boundary-max))))

(ert-deftest dorg-test-sortkey-bounds-short-neighbours ()
  "Every key in the interval sorts strictly between short neighbours."
  (let* ((left (dorg-sortkey-from-hex "41"))
         (right (dorg-sortkey-from-hex "43"))
         (b (dorg-sortkey-bounds left right))
         (lo-key (dorg-sortkey-encode (car b)))
         (hi-key (dorg-sortkey-encode (cdr b))))
    (should (< (car b) (cdr b)))
    (should (string< left lo-key))
    (should (string< hi-key right))
    ;; The zero-padded RIGHT itself is excluded: it sorts after RIGHT.
    (should (string< right (dorg-sortkey-encode (1+ (cdr b)))))))

(ert-deftest dorg-test-sortkey-bounds-legacy-long-left-edge ()
  "W1: a neighbour longer than 16 bytes is a valid bound, not an exhaustion.
The live root fan's 19-byte tail refused every append at width 16."
  (let* ((tail (dorg-sortkey-from-hex dorg-test-sortkey--live-tail))
         (b (dorg-sortkey-bounds tail nil))
         (lo-key (dorg-sortkey-encode (car b))))
    (should (= (length tail) 19))
    (should (string< tail lo-key))
    (should (= (length lo-key) 16))
    ;; The prefix itself must be excluded: it is a proper prefix of TAIL.
    (should (string< (dorg-sortkey-encode (1- (car b))) tail))))

(ert-deftest dorg-test-sortkey-bounds-legacy-long-right-edge ()
  "A right neighbour longer than 16 bytes admits its own prefix as the top."
  (let* ((right (dorg-sortkey-from-hex dorg-test-sortkey--live-tail))
         (b (dorg-sortkey-bounds nil right))
         (hi-key (dorg-sortkey-encode (cdr b))))
    (should (string< hi-key right))
    (should (= (length hi-key) 16))
    ;; One more is already past RIGHT.
    (should (string< right (dorg-sortkey-encode (1+ (cdr b)))))))

(ert-deftest dorg-test-sortkey-bounds-adjacent-prefix-interval-is-empty ()
  "X'41' < X'4100' has no representable interior key: bounds cross."
  (let ((b (dorg-sortkey-bounds (dorg-sortkey-from-hex "41")
                                (dorg-sortkey-from-hex "4100"))))
    (should (> (car b) (cdr b)))
    (should-error (dorg-sortkey-spread 1 (car b) (cdr b))
                  :type 'dorg-sortkey-exhausted)))

;;; Step allocation at the head and tail

(ert-deftest dorg-test-sortkey-step-after-live-tail ()
  "W1 RED case: appending after the live 19-byte tail allocates X'04' + 15 zero bytes."
  (let* ((tail (dorg-sortkey-from-hex dorg-test-sortkey--live-tail))
         (next (dorg-sortkey-step-after tail)))
    (should (equal (dorg-sortkey-to-hex next)
                   "X'04000000000000000000000000000000'"))
    (should (string< tail next))))

(ert-deftest dorg-test-sortkey-step-after-is-step-aligned-from-center ()
  "From the canonical centre, the first tail step is centre + step."
  (should (= (dorg-sortkey-int16
              (dorg-sortkey-step-after (dorg-sortkey-encode dorg-sortkey-boundary-center)))
             (+ dorg-sortkey-boundary-center dorg-sortkey-boundary-step)))
  ;; The 1-byte legacy centre key X'80' is the same coordinate.
  (should (= (dorg-sortkey-int16 (dorg-sortkey-step-after (unibyte-string #x80)))
             (+ dorg-sortkey-boundary-center dorg-sortkey-boundary-step))))

(ert-deftest dorg-test-sortkey-step-before-is-step-aligned-from-center ()
  "From the canonical centre, the first head step is centre - step."
  (should (= (dorg-sortkey-int16
              (dorg-sortkey-step-before (dorg-sortkey-encode dorg-sortkey-boundary-center)))
             (- dorg-sortkey-boundary-center dorg-sortkey-boundary-step))))

(ert-deftest dorg-test-sortkey-step-after-width-stays-constant ()
  "Six hundred tail steps from a legacy 1-byte key stay 16 bytes and ordered.
Every returned key is compared with the preceding ORIGINAL key."
  (let ((key (unibyte-string #x80)))
    (dotimes (_ 600)
      (let ((next (dorg-sortkey-step-after key)))
        (should (string< key next))
        (should (= (length next) 16))
        (setq key next)))))

(ert-deftest dorg-test-sortkey-step-before-width-stays-constant ()
  "Six hundred head steps from a legacy 1-byte key stay 16 bytes and ordered."
  (let ((key (unibyte-string #x80)))
    (dotimes (_ 600)
      (let ((previous (dorg-sortkey-step-before key)))
        (should (string< previous key))
        (should (= (length previous) 16))
        (setq key previous)))))

(ert-deftest dorg-test-sortkey-step-fails-closed-at-the-domain-edges ()
  "A step that would leave the domain signals exhaustion instead of wrapping."
  (should-error (dorg-sortkey-step-after (dorg-sortkey-encode dorg-sortkey-boundary-max))
                :type 'dorg-sortkey-exhausted)
  (should-error (dorg-sortkey-step-before (dorg-sortkey-encode 0))
                :type 'dorg-sortkey-exhausted))

(ert-deftest dorg-test-sortkey-boundary-trailing-zero-uses-original-edge ()
  "A trailing-zero legacy edge is ordered against its original bytes.
Zero padding is not injective: X'4142' and X'414200' alias after padding.
Allocation must compare with the original ordering bound, not the alias."
  (let* ((short (dorg-sortkey-from-hex "4142"))
         (trailing-zero (dorg-sortkey-from-hex "414200"))
         (next (dorg-sortkey-step-after trailing-zero))
         (previous (dorg-sortkey-step-before short)))
    (should-not (equal short trailing-zero))
    (should (= (dorg-sortkey-int16 short) (dorg-sortkey-int16 trailing-zero)))
    (should (string< trailing-zero next))
    (should (string< previous short))
    (should (= (length next) 16))))

(ert-deftest dorg-test-sortkey-boundary-capacity-covers-id-domain ()
  "Pure boundary capacity is derived from and covers the DOrg ID domain.
The tail has zero spare step, so changing either identity width or boundary
constants must break this executable coupling instead of silently invalidating
the fixed-width claim."
  (dolist (symbol '(dorg-id-max
                    dorg-sortkey-boundary-width
                    dorg-sortkey-boundary-step
                    dorg-sortkey-boundary-center
                    dorg-sortkey-boundary-max))
    (should (boundp symbol)))
  (let* ((width dorg-sortkey-boundary-width)
         (step dorg-sortkey-boundary-step)
         (center dorg-sortkey-boundary-center)
         (maximum dorg-sortkey-boundary-max)
         (tail-capacity (/ (- maximum center) step))
         (head-capacity (/ center step)))
    (should (= width 16))
    (should (= step (ash 1 64)))
    (should (= center (ash 1 127)))
    (should (= maximum (1- (ash 1 128))))
    (should (= tail-capacity dorg-id-max))
    (should (>= head-capacity dorg-id-max))))

(ert-deftest dorg-test-sortkey-first-key-is-the-center ()
  "An empty group starts at the canonical centre so head and tail capacity match."
  (should (equal (dorg-sortkey-first-key)
                 (dorg-sortkey-encode dorg-sortkey-boundary-center))))

;;; Spread allocation inside an interval

(ert-deftest dorg-test-sortkey-spread-even-and-ordered ()
  "COUNT keys are strictly increasing, 16 bytes, and strictly inside the interval."
  (let* ((b (dorg-sortkey-bounds (dorg-sortkey-from-hex "41")
                                 (dorg-sortkey-from-hex "43")))
         (keys (dorg-sortkey-spread 7 (car b) (cdr b))))
    (should (= (length keys) 7))
    (should (dorg-test-sortkey--ordered-p keys))
    (dolist (k keys)
      (should (= (length k) 16))
      (should (<= (car b) (dorg-sortkey-int16 k) (cdr b))))
    (should (string< (dorg-sortkey-from-hex "41") (car keys)))
    (should (string< (car (last keys)) (dorg-sortkey-from-hex "43")))))

(ert-deftest dorg-test-sortkey-spread-zero-count ()
  "Zero keys is an empty list, never an error."
  (should (null (dorg-sortkey-spread 0 1 10))))

(ert-deftest dorg-test-sortkey-spread-avoids-occupied-keys ()
  "Candidates equal to an occupied key move up; order and bounds survive.
The occupied set is seeded with every unnudged candidate, so every key has to
move, and with two adjacent occupied values one key has to move twice."
  (let* ((lo 1000) (hi 1999)
         (plain (dorg-sortkey-spread 4 lo hi))
         (occupied (make-hash-table :test 'equal)))
    (dolist (k plain) (puthash k t occupied))
    (puthash (dorg-sortkey-encode (1+ (dorg-sortkey-int16 (car plain)))) t occupied)
    (let ((keys (dorg-sortkey-spread 4 lo hi occupied)))
      (should (= (length keys) 4))
      (should (dorg-test-sortkey--ordered-p keys))
      (dolist (k keys)
        (should-not (gethash k occupied))
        (should (<= lo (dorg-sortkey-int16 k) hi))))))

(ert-deftest dorg-test-sortkey-spread-exhausts-when-the-interval-is-too-small ()
  "Too little room for COUNT keys plus the occupied margin signals exhaustion."
  (should-error (dorg-sortkey-spread 5 1 10) :type 'dorg-sortkey-exhausted)
  (let ((occupied (make-hash-table :test 'equal)))
    (dotimes (i 6) (puthash (dorg-sortkey-encode (+ 100 i)) t occupied))
    ;; 3 keys need spacing >= 6 + 2 = 8 -> 32 units; 20 is not enough.
    (should-error (dorg-sortkey-spread 3 100 119 occupied)
                  :type 'dorg-sortkey-exhausted)
    ;; 40 units are.
    (should (= 3 (length (dorg-sortkey-spread 3 100 139 occupied))))))

(ert-deftest dorg-test-sortkey-spread-whole-group ()
  "Normalising N keys over the open domain yields N ordered 16-byte keys."
  (let* ((b (dorg-sortkey-bounds nil nil))
         (keys (dorg-sortkey-spread 300 (car b) (cdr b))))
    (should (= (length keys) 300))
    (should (= (length (cl-remove-duplicates keys :test #'equal)) 300))
    (should (dorg-test-sortkey--ordered-p keys))
    (should (cl-every (lambda (k) (= (length k) 16)) keys))))

(ert-deftest dorg-test-sortkey-max-bytes-matches-schema ()
  "Storage limit constant is 64, matching VARBINARY(64)."
  (should (= dorg-sortkey-max-bytes 64)))

(ert-deftest dorg-test-sortkey-ordered-p ()
  "ordered-p accepts empty, singleton and strictly increasing lists only."
  (should (dorg-sortkey-ordered-p nil))
  (should (dorg-sortkey-ordered-p (list (unibyte-string 1))))
  (should (dorg-sortkey-ordered-p (list (unibyte-string 1) (unibyte-string 2))))
  (should-not (dorg-sortkey-ordered-p (list (unibyte-string 2) (unibyte-string 2))))
  (should-not (dorg-sortkey-ordered-p (list (unibyte-string 2) (unibyte-string 1)))))

(provide 'dorg-sortkey-tests)
;;; dorg-sortkey-tests.el ends here
