;;; dorg-sortkey.el --- Fixed-width fractional sort keys -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sort keys are unibyte strings stored in a VARBINARY(64) column.
;; They compare lexicographically with `string<' and are emitted as
;; SQL hex literals (X'01ff' etc).
;;
;; Allocation works in one fixed-width coordinate domain: every key
;; this module allocates is exactly `dorg-sortkey-boundary-width' (16)
;; bytes, read as a big-endian unsigned integer.  Equal-width unsigned
;; order is bytewise lexicographic order, so integer arithmetic on the
;; domain is order arithmetic on the column.
;;
;; Stored keys of any other length up to `dorg-sortkey-max-bytes'
;; remain valid neighbours: earlier allocators produced 1..64-byte
;; keys, and `dorg-sortkey-bounds' derives the integer interval
;; strictly between two arbitrary-length keys from their 16-byte
;; prefixes.  Every key allocated inside that interval sorts strictly
;; between the original byte strings; the proof is in the docstring of
;; `dorg-sortkey-bounds'.  Legacy keys migrate to the fixed width only
;; when a rebalance touches them; nothing rewrites them on its own.
;;
;; Entry points:
;; - `dorg-sortkey-first-key'   — the canonical centre for an empty group
;; - `dorg-sortkey-step-after'  — pure tail allocation, one step up
;; - `dorg-sortkey-step-before' — pure head allocation, one step down
;; - `dorg-sortkey-bounds'      — open integer interval between two keys
;; - `dorg-sortkey-spread'      — COUNT evenly spaced keys inside an interval
;; - `dorg-sortkey-from-integer' — explicit small keys (imports, fixtures)
;; - `dorg-sortkey-to-hex' / `dorg-sortkey-from-hex' — SQL hex literals
;;
;; Steps are multiples of `dorg-sortkey-boundary-step' (2^64), so from
;; the centre the tail has 2^63-1 steps of headroom (the DOrg ID
;; domain, see `dorg-test-sortkey-boundary-capacity-covers-id-domain')
;; and every step leaves 64 bits of room below it for interior
;; placement.  `dorg-sortkey-exhausted' is signalled only when the
;; requested interval cannot hold the requested keys — the caller
;; widens its window — or when a step would leave the domain, which
;; fails closed rather than wrap, truncate or widen the column.

;;; Code:

(require 'cl-lib)

;;; Constants

(defconst dorg-sortkey-max-bytes 64
  "Maximum stored sort-key length in bytes.
Matches the VARBINARY(64) storage contract for the sort_key columns in
`dorg-schema.el'.  `dorg-sql-gen--sort-key-hex' refuses anything longer.")

(defconst dorg-sortkey-boundary-width 16
  "Byte width of every key this module allocates.")

(defconst dorg-sortkey-boundary-step (ash 1 64)
  "Numeric step used by head and tail allocation.")

(defconst dorg-sortkey-boundary-center (ash 1 127)
  "Canonical first key of an empty group.
This is exactly the 16-byte zero-padding of the legacy X'80' key.")

(defconst dorg-sortkey-boundary-max (1- (ash 1 128))
  "Maximum unsigned value in the allocation domain.")

(define-error 'dorg-sortkey-exhausted
  "Sort-key allocation has no room in the requested interval")

;;; Coordinate transform

(defun dorg-sortkey--pad (s len)
  "Pad unibyte string S with null bytes to LEN.  Truncates if longer."
  (let ((slen (length s)))
    (cond
     ((= slen len) s)
     ((> slen len) (substring s 0 len))
     (t (concat s (make-string (- len slen) 0))))))

(defun dorg-sortkey-int16 (key)
  "Return the 16-byte prefix of KEY, zero-padded if shorter, as an integer.
This is a coordinate, not key identity: a key longer than the width shares
the coordinate of its first 16 bytes, and a shorter key shares it with its
zero padding.  `dorg-sortkey-bounds' accounts for both."
  (let ((n 0)
        (padded (dorg-sortkey--pad key dorg-sortkey-boundary-width)))
    (dotimes (index dorg-sortkey-boundary-width)
      (setq n (+ (ash n 8) (aref padded index))))
    n))

(defun dorg-sortkey-encode (n)
  "Encode unsigned integer N as an exactly 16-byte sort key.
Signals `dorg-sortkey-exhausted' outside the domain."
  (unless (and (integerp n) (<= 0 n dorg-sortkey-boundary-max))
    (signal 'dorg-sortkey-exhausted (list n)))
  (let ((bytes nil)
        (x n))
    (dotimes (_ dorg-sortkey-boundary-width)
      (push (logand x #xff) bytes)
      (setq x (ash x -8)))
    (apply #'unibyte-string bytes)))

(defun dorg-sortkey-from-integer (n &optional width)
  "Convert positive integer N to a big-endian unibyte sort key of WIDTH bytes.
WIDTH defaults to the smallest width that holds N.  This is the constructor
for small explicit keys (import ordinals, fixtures); allocation never uses it."
  (unless (and (integerp n) (>= n 1))
    (error "dorg-sortkey-from-integer: N must be positive, got %S" n))
  (let ((w (or width (max 1 (ceiling (/ (log (1+ n)) (log 256)))))))
    (unless (<= n (1- (ash 1 (* w 8))))
      (error "dorg-sortkey-from-integer: N=%d does not fit in %d bytes" n w))
    (let ((bytes nil))
      (dotimes (_ w)
        (push (logand n #xff) bytes)
        (setq n (ash n -8)))
      (apply #'unibyte-string bytes))))

;;; Bounds

(defun dorg-sortkey-bounds (left right)
  "Return (LO . HI), the integers whose keys sort strictly between LEFT and RIGHT.
LEFT and RIGHT are stored keys of any length, or nil for an open end.  LO is
the smallest coordinate whose 16-byte key sorts after LEFT and HI the largest
whose key sorts before RIGHT; LO > HI means the interval is empty.

Proof.  Let p be the 16-byte prefix of LEFT, zero-padded when LEFT is shorter.
A 16-byte key K with int(K) > int(p) first differs from p at a byte i < 16
where K[i] > p[i].  If i lies inside LEFT, K[i] > LEFT[i] and K sorts after
LEFT; if i lies in the padding, K agrees with the whole of LEFT and is longer,
which also sorts after.  K = p itself is excluded because it is a proper
prefix of a longer LEFT.  For RIGHT: when RIGHT is longer than 16 bytes, p is
a proper prefix of RIGHT and sorts before it, so int(K) <= int(p) suffices;
otherwise p is RIGHT zero-padded, which sorts at or after RIGHT, so K needs
int(K) < int(p).  Hence every key in [LO, HI] sorts strictly between the
original byte strings, whatever their lengths."
  (cons (if left (1+ (dorg-sortkey-int16 left)) 1)
        (cond ((null right) dorg-sortkey-boundary-max)
              ((> (length right) dorg-sortkey-boundary-width)
               (dorg-sortkey-int16 right))
              (t (1- (dorg-sortkey-int16 right))))))

;;; Head and tail steps

(defun dorg-sortkey-first-key ()
  "Return the canonical first key of an empty group."
  (dorg-sortkey-encode dorg-sortkey-boundary-center))

(defun dorg-sortkey-step-after (left)
  "Return the first step-aligned key strictly after stored key LEFT.
The result is exactly 16 bytes.  Signals `dorg-sortkey-exhausted' when the
step would leave the domain."
  (let* ((lo (car (dorg-sortkey-bounds left nil)))
         (step dorg-sortkey-boundary-step)
         (n (* step (/ (+ lo step -1) step))))
    (when (> n dorg-sortkey-boundary-max)
      (signal 'dorg-sortkey-exhausted (list n)))
    (dorg-sortkey-encode n)))

(defun dorg-sortkey-step-before (right)
  "Return the last step-aligned key strictly before stored key RIGHT.
The result is exactly 16 bytes.  Signals `dorg-sortkey-exhausted' when the
step would leave the domain."
  (let* ((hi (cdr (dorg-sortkey-bounds nil right)))
         (step dorg-sortkey-boundary-step))
    (when (< hi 0)
      (signal 'dorg-sortkey-exhausted (list hi)))
    (dorg-sortkey-encode (* step (/ hi step)))))

;;; Spread

(defun dorg-sortkey-spread (count lo hi &optional occupied)
  "Return COUNT strictly increasing 16-byte keys evenly spaced inside [LO, HI].
OCCUPIED is a hash set (test `equal') of keys the result must not equal; a
candidate that hits an occupied key moves up one unit at a time.  With M
occupied keys inside the interval the spacing is at least M + 2 units, so at
most M moves are ever needed and the moved key still sorts strictly below the
next candidate and inside the interval.  Occupied keys outside the interval
cost nothing.  Signals `dorg-sortkey-exhausted' when the interval
cannot hold COUNT keys at that spacing.  COUNT of zero returns nil."
  (unless (and (integerp count) (>= count 0))
    (error "dorg-sortkey-spread: COUNT must be non-negative, got %S" count))
  (when (> count 0)
    (let* ((cap (1+ (- hi lo)))
           (inside 0)
           (margin (progn
                     (when occupied
                       (maphash (lambda (key _)
                                  (when (<= lo (dorg-sortkey-int16 key) hi)
                                    (cl-incf inside)))
                                occupied))
                     (+ 2 inside)))
           (gap (and (> cap 0) (/ cap (1+ count)))))
      (when (or (null gap) (< gap margin))
        (signal 'dorg-sortkey-exhausted (list count cap)))
      (cl-loop for i from 1 to count
               collect (let* ((n (+ lo (* i gap)))
                              (key (dorg-sortkey-encode n)))
                         (while (and occupied (gethash key occupied))
                           (setq n (1+ n)
                                 key (dorg-sortkey-encode n)))
                         key)))))

;;; Conversion utilities

(defun dorg-sortkey-to-hex (key)
  "Return SQL hex literal for unibyte string KEY.  E.g. X'01ff'."
  (concat "X'"
          (mapconcat (lambda (byte) (format "%02x" byte)) key "")
          "'"))

(defun dorg-sortkey-from-hex (hex)
  "Parse a hex string HEX (no X'' wrapper) into a unibyte string.
E.g. \"01FF\" -> \"\\x01\\xff\"."
  (let* ((clean (if (and (>= (length hex) 3)
                         (string-prefix-p "X'" hex)
                         (string-suffix-p "'" hex))
                    (substring hex 2 -1)
                  hex))
         (len (/ (length clean) 2))
         (bytes nil))
    (dotimes (i len)
      (push (string-to-number (substring clean (* i 2) (+ (* i 2) 2)) 16)
            bytes))
    (apply #'unibyte-string (nreverse bytes))))

;;; Order predicate

(defun dorg-sortkey-ordered-p (keys)
  "Return non-nil if KEYS (list of unibyte strings) is strictly ordered."
  (or (null keys)
      (null (cdr keys))
      (cl-loop for (a b) on keys
               while b
               always (string< a b))))

(provide 'dorg-sortkey)
;;; dorg-sortkey.el ends here
