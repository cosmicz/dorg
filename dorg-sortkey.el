;;; dorg-sortkey.el --- Fractional sort key operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Sort keys are unibyte strings representing positions in a
;; VARBINARY(64) column.  They compare lexicographically with
;; `string<' and are emitted as SQL hex literals (X'01ff' etc).
;;
;; The midpoint algorithm computes a key strictly between two
;; neighbors, extending the byte length only when adjacent keys
;; have no room at the current depth.  This gives O(log N)
;; key growth for repeated bisection.
;;
;; Public API:
;; - `dorg-sortkey-midpoint'         — key strictly between A and B
;; - `dorg-sortkey-midpoint-limited' — midpoint that signals on exhaustion
;; - `dorg-sortkey-from-integer'     — convert 1-based integer to key
;; - `dorg-sortkey-to-hex'           — emit SQL hex literal
;; - `dorg-sortkey-from-hex'         — parse hex string to unibyte key
;;
;; Storage budget: keys live in a VARBINARY(64) column (see
;; `dorg-schema.el').  Repeated bisection of a narrow gap grows the
;; byte length, so a newly allocated midpoint can approach the column
;; limit.  `dorg-sortkey-midpoint-limited' refuses to allocate at the
;; boundary and signals `dorg-sortkey-exhausted', letting callers
;; rebalance the affected sibling group instead of writing an overlong
;; or truncated key.

;;; Code:

(require 'cl-lib)

;;; Storage Limits

(defconst dorg-sortkey-max-bytes 64
  "Maximum sort-key length in bytes.
Matches the VARBINARY(64) storage contract for the sort_key columns
in `dorg-schema.el'.  Keys longer than this cannot be stored without
truncation or error.")

(defconst dorg-sortkey-rebalance-threshold 64
  "Byte length at which a newly allocated midpoint triggers rebalance.
A fresh midpoint candidate whose length reaches this threshold is
treated as exhausted: it sits at the storage cliff edge and must not
be assigned.  Callers rebalance the sibling group instead.  Existing
keys read from the DB up to `dorg-sortkey-max-bytes' remain valid.")

(define-error 'dorg-sortkey-exhausted
  "Sort-key midpoint allocation reached the storage budget")

;;; Midpoint Algorithm

(defun dorg-sortkey-midpoint (a b)
  "Return a unibyte string strictly between A and B.
A and B are unibyte strings compared lexicographically.
A = nil means \"before everything\", B = nil means \"after everything\".
Signals an error if A >= B (when both are non-nil)."
  (cond
   ;; Both nil — return a default key
   ((and (null a) (null b))
    (unibyte-string #x80))
   ;; A is nil — return something before B
   ((null a)
    (dorg-sortkey--before b))
   ;; B is nil — return something after A
   ((null b)
    (dorg-sortkey--after a))
   ;; Both present
   (t
    (unless (string< a b)
      (error "dorg-sortkey-midpoint: A must be < B, got A=%s B=%s"
             (dorg-sortkey-to-hex a) (dorg-sortkey-to-hex b)))
    (dorg-sortkey--between a b))))

(defun dorg-sortkey-midpoint-limited (a b)
  "Return a key strictly between A and B, signalling on exhaustion.
Like `dorg-sortkey-midpoint', but when the allocated key reaches
`dorg-sortkey-rebalance-threshold' bytes it signals
`dorg-sortkey-exhausted' (with the candidate length as data) instead
of returning a key at the storage cliff edge.  Callers should rebalance
the affected sibling group on this signal.

Argument and ordering errors from `dorg-sortkey-midpoint' propagate
unchanged — only the boundary condition raises `dorg-sortkey-exhausted'."
  (let ((key (dorg-sortkey-midpoint a b)))
    (if (>= (length key) dorg-sortkey-rebalance-threshold)
        (signal 'dorg-sortkey-exhausted (list (length key)))
      key)))

(defun dorg-sortkey--before (b)
  "Return a key before B.
If B's first byte > 1, halve it.  Otherwise prepend and halve."
  (let ((b0 (aref b 0)))
    (if (> b0 1)
        (unibyte-string (/ b0 2))
      ;; B starts with \x01 or \x00 — extend: midpoint of "" and B
      ;; at depth 1+
      (let ((extended-b (dorg-sortkey--pad b (1+ (length b)))))
        (dorg-sortkey--between (make-string (length extended-b) 0)
                               extended-b)))))

(defun dorg-sortkey--after (a)
  "Return a key after A.
Increment the trailing byte if room, otherwise extend with \\x80."
  (let* ((len (length a))
         (last-byte (aref a (1- len))))
    (if (< last-byte #xff)
        ;; Room to increment — midpoint between A and A-with-last-byte-FF
        (let ((result (copy-sequence a)))
          (aset result (1- len)
                (+ last-byte (/ (- #xff last-byte) 2)
                   (if (= (- #xff last-byte) 1) 1 0)))
          ;; Ensure we actually moved past A
          (if (string< a result)
              result
            ;; Edge: last-byte is #xfe — extend
            (concat a (unibyte-string #x80))))
      ;; Last byte is #xff — extend
      (concat a (unibyte-string #x80)))))

(defun dorg-sortkey--between (a b)
  "Return a key strictly between A and B, both non-nil, A < B.
Works byte-by-byte, extending depth when adjacent."
  (let* ((max-len (max (length a) (length b)))
         (pa (dorg-sortkey--pad a max-len))
         (pb (dorg-sortkey--pad b max-len))
         (result nil)
         (found nil)
         (i 0))
    ;; Walk shared prefix, find first differing position
    (while (and (< i max-len) (not found))
      (let ((ba (aref pa i))
            (bb (aref pb i)))
        (if (= ba bb)
            ;; Same byte — emit and continue
            (progn (push ba result) (cl-incf i))
          ;; Different — compute midpoint
          (let ((mid (/ (+ ba bb) 2)))
            (if (> mid ba)
                ;; Room between — take midpoint
                (progn (push mid result) (setq found t))
              ;; Adjacent bytes (bb = ba+1) — emit ba and recurse deeper
              (push ba result)
              (cl-incf i)
              ;; Now we need midpoint between a[i..] (padded with 0x00)
              ;; and b[i..] (padded with 0x00, but b diverged so it's
              ;; effectively 0x00..0x00 from b's perspective since bb > ba+1
              ;; is impossible here)
              ;; Actually: a = ...ba XX YY, b = ...(ba+1) 00 00
              ;; We emitted ba, now need between a[i:] and "\xff..."
              ;; because anything starting with ba and then > a[i:] but
              ;; starting with ba is < b (which starts with ba+1)
              (let* ((remaining-len (max 1 (- max-len i)))
                     (sub-a (dorg-sortkey--pad
                             (if (< i (length a))
                                 (substring a i)
                               "")
                             remaining-len))
                     ;; Upper bound is all 0x00 from b's side, but in
                     ;; our prefix context that means anything up to \xff..\xff
                     (sub-b (make-string remaining-len #xff))
                     (sub-mid (dorg-sortkey--between-bytes sub-a sub-b)))
                (dolist (byte sub-mid) (push byte result))
                (setq found t)))))))
    (unless found
      ;; All bytes equal up to max-len — extend: midpoint between
      ;; pa+\x00 and pb+\x00 but pa < pb only at extension
      ;; This means they were equal at max-len but a < b, so a must
      ;; be shorter.  Extend with \x80.
      (push #x80 result))
    (apply #'unibyte-string (nreverse result))))

(defun dorg-sortkey--between-bytes (a b)
  "Return list of bytes for a key between unibyte strings A and B.
A < B must hold.  Returns a list of byte values."
  (let ((len (max (length a) (length b)))
        (result nil)
        (found nil)
        (i 0))
    (let ((pa (dorg-sortkey--pad a len))
          (pb (dorg-sortkey--pad b len)))
      (while (and (< i len) (not found))
        (let ((ba (aref pa i))
              (bb (aref pb i)))
          (if (= ba bb)
              (progn (push ba result) (cl-incf i))
            (let ((mid (/ (+ ba bb) 2)))
              (if (> mid ba)
                  (progn (push mid result) (setq found t))
                ;; Adjacent — emit ba, extend
                (push ba result)
                (cl-incf i)
                (push #x80 result)
                (setq found t)))))))
    (unless found
      (push #x80 result))
    (nreverse result)))

(defun dorg-sortkey--pad (s len)
  "Pad unibyte string S with null bytes to LEN.  Truncates if longer."
  (let ((slen (length s)))
    (cond
     ((= slen len) s)
     ((> slen len) (substring s 0 len))
     (t (concat s (make-string (- len slen) 0))))))

;;; Conversion Utilities

(defun dorg-sortkey-width-for-count (count)
  "Return minimum byte width needed to encode COUNT ordinals (1..COUNT)."
  (cond ((<= count 255) 1)
        ((<= count 65535) 2)
        ((<= count 16777215) 3)
        (t 4)))

(defun dorg-sortkey-from-integer (n &optional width)
  "Convert positive integer N to a big-endian unibyte sort key.
WIDTH is the number of bytes; auto-computed from N when nil.
All siblings in a group should use the same WIDTH for correct
`string<' ordering."
  (unless (and (integerp n) (>= n 1))
    (error "dorg-sortkey-from-integer: N must be positive, got %S" n))
  (let ((w (or width (dorg-sortkey-width-for-count n))))
    (unless (<= n (1- (ash 1 (* w 8))))
      (error "dorg-sortkey-from-integer: N=%d does not fit in %d bytes" n w))
    (let ((bytes nil))
      (dotimes (_ w)
        (push (logand n #xff) bytes)
        (setq n (ash n -8)))
      (apply #'unibyte-string bytes))))

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
