;;; dorg-id.el --- Snowflake ID generation + DORG_ID readers for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Client-side snowflake ID generator for dorg.  Produces
;; monotonically-increasing 64-bit BIGINTs without any round-trip to
;; the database.
;;
;; Layout (63 usable bits; sign bit stays 0):
;;
;;   bit  63 | 62                  22 | 21      12 | 11    0
;;       [0] | [41b ms since epoch ] | [10b shard] | [12b seq]
;;
;; - 41 ms bits → ~69 years from the custom epoch (2026-01-01 UTC)
;; - 10 shard bits → 1024 shards (single-user: 0)
;; - 12 seq bits → 4096 IDs per ms per shard
;;
;; Collisions are handled by incrementing the sequence within a ms
;; and busy-waiting to the next ms on sequence overflow.  Clock
;; skew backward signals an error — user must fix the clock.

;;; Code:

(require 'org)
(require 'org-element)

(defconst dorg-id-property "DORG_ID"
  "Org property key carrying a heading's dorg identity.
The single source for the plain-string spelling of the identity property;
readers and writers across the codebase reference this constant rather than
re-typing the literal.  The `:DORG_ID' keyword-symbol form used with
`org-element-property' is a deliberate exception (rewriting it through this
constant is a net readability loss).")

(defconst dorg-id-epoch-ms 1767225600000
  "Custom epoch for dorg snowflake IDs.
2026-01-01 00:00:00 UTC in milliseconds since unix epoch.")

(defconst dorg-id--seq-bits 12
  "Number of sequence bits in a snowflake id.")

(defconst dorg-id--shard-bits 10
  "Number of shard bits in a snowflake id.")

(defconst dorg-id--timestamp-bits
  (- 63 dorg-id--shard-bits dorg-id--seq-bits)
  "Number of timestamp bits in a positive snowflake id.")

(defconst dorg-id-max
  (1- (ash 1 (+ dorg-id--timestamp-bits
                dorg-id--shard-bits
                dorg-id--seq-bits)))
  "Maximum positive DOrg snowflake id.
Derived from the sign-bit-clear id layout so code that couples to the
identity domain fails loudly if the layout width changes.")

(defconst dorg-id--seq-max (1- (ash 1 dorg-id--seq-bits))
  "Maximum sequence value (4095).")

(defconst dorg-id--shard-max (1- (ash 1 dorg-id--shard-bits))
  "Maximum shard value (1023).")

(defcustom dorg-id-shard (+ 16 (random (- (ash 1 dorg-id--shard-bits) 16)))
  "Shard id for this process, in the range [0, 1023].
Defaults to a random value in [16, 1023] so concurrent processes
get distinct shards without coordination.  Shards 0-15 are
reserved for explicit assignment."
  :type '(integer 0 1023)
  :group 'dorg)

;; KEEP (mutable-state): monotonic id-clock state, setq-mutated; must persist across calls.
(defvar dorg-id--last-ms 0
  "Last millisecond at which an id was generated.
Internal state; tests may rebind this directly.")

;; KEEP (mutable-state): per-millisecond id sequence counter, setq-mutated across calls.
(defvar dorg-id--last-seq 0
  "Sequence counter within the current millisecond.
Internal state; tests may rebind this directly.")

(defun dorg-id--current-ms ()
  "Return the current time in milliseconds since the unix epoch.
Uses `current-time' so it is deterministic under `cl-letf' in
tests."
  (let ((now (current-time)))
    (+ (* (nth 0 now) 65536000)
       (* (nth 1 now) 1000)
       (/ (nth 2 now) 1000))))

(defun dorg-id--wait-next-ms (last)
  "Busy-wait until the current ms is strictly greater than LAST.
Returns the new ms."
  (let ((ms (dorg-id--current-ms)))
    (while (<= ms last)
      (setq ms (dorg-id--current-ms)))
    ms))

(defun dorg-id-generate ()
  "Return the next snowflake BIGINT id.
Signals an error on clock skew backward or if the current time
is before `dorg-id-epoch-ms'."
  (unless (and (integerp dorg-id-shard)
               (<= 0 dorg-id-shard dorg-id--shard-max))
    (error "dorg-id-shard must be an integer in [0, %d]"
           dorg-id--shard-max))
  (let ((ms (dorg-id--current-ms)))
    (cond
     ((< ms dorg-id--last-ms)
      (error "dorg-id: clock skew backward (%d < %d); refusing to generate id"
             ms dorg-id--last-ms))
     ((= ms dorg-id--last-ms)
      (setq dorg-id--last-seq (1+ dorg-id--last-seq))
      (when (> dorg-id--last-seq dorg-id--seq-max)
        (setq dorg-id--last-ms (dorg-id--wait-next-ms dorg-id--last-ms))
        (setq dorg-id--last-seq 0)))
     (t
      (setq dorg-id--last-ms ms)
      (setq dorg-id--last-seq 0)))
    (let ((ms-offset (- dorg-id--last-ms dorg-id-epoch-ms)))
      (when (< ms-offset 0)
        (error "dorg-id: current time (%d ms) is before dorg epoch (%d ms)"
               dorg-id--last-ms dorg-id-epoch-ms))
      (logior (ash ms-offset (+ dorg-id--shard-bits dorg-id--seq-bits))
              (ash dorg-id-shard dorg-id--seq-bits)
              dorg-id--last-seq))))

(defun dorg-id-extract-ms (id)
  "Return the epoch-relative ms encoded in snowflake ID."
  (ash id (- (+ dorg-id--shard-bits dorg-id--seq-bits))))

(defun dorg-id-extract-shard (id)
  "Return the shard bits encoded in snowflake ID."
  (logand (ash id (- dorg-id--seq-bits)) dorg-id--shard-max))

(defun dorg-id-extract-seq (id)
  "Return the sequence bits encoded in snowflake ID."
  (logand id dorg-id--seq-max))

;;; DORG_ID readers — one malformed-value policy
;;
;; Five readers historically implemented four divergent malformed-value
;; policies (parse signalled, command/ql digit-validated to nil,
;; sync-local dropped `DORG_ID+', mode/conflict coerced garbage to id 0).
;; `dorg-id--validate' is now the single decision; every reader delegates.

(defun dorg-id--validate (raw &optional strict)
  "Return the integer identity of raw property value RAW, or nil.
This is the ONE malformed-value policy shared by every DORG_ID reader.

RAW is `leading-integer + positivity': `string-to-number' of the trimmed
value, kept only when it is an INTEGER strictly greater than 0.  This
matches `org-entry-get' numeric reading (so \"123abc\" -> 123,
\"111 222\" -> 111) with two guards — integrality and `>0' — that reject
the synthetic root id 0 AND non-integer forms.

The integrality guard is load-bearing: `string-to-number' also parses
decimal and exponent forms (\"1.5\" -> 1.5, \"1e3\" -> 1000.0), which a
bare `>0' guard would admit as a FLOAT DORG_ID.  A returned identity is
always an integer (fixnum or bignum), never a float — the snowflake/id
domain is integral.  Leading-integer compatibility survives because
\"123abc\" still parses to the integer 123.

- absent (nil) or empty/whitespace RAW  -> nil (heading has no identity);
- present positive integer               -> that integer;
- present-but-invalid (<=0, or a float / -exponent form) -> nil, or a
  signalled error when STRICT is non-nil (the import-boundary tripwire —
  see `dorg-id-of-element' in `dorg-parse').

STRICT never signals on absent/empty RAW: a heading may legitimately
carry no DORG_ID (a new heading)."
  (when (and raw (not (string-blank-p raw)))
    (let ((n (string-to-number (string-trim raw))))
      (cond
       ((and (integerp n) (> n 0)) n)
       (strict (error "dorg-id: malformed :DORG_ID: value %S" raw))
       (t nil)))))

(defun dorg-id-at-point (&optional strict)
  "Return the integer DORG_ID of the heading at point, or nil.
Uses `org-entry-get' (inheriting its DORG_ID/DORG_ID+ combining) then the
single `dorg-id--validate' policy.  With STRICT, a present-but-invalid
value signals instead of reading as nil."
  (dorg-id--validate (org-entry-get (point) dorg-id-property) strict))

(defun dorg-id-of-element (element &optional strict)
  "Return the integer DORG_ID of org-element ELEMENT, or nil.
Element-based sibling of `dorg-id-at-point', same policy.  With STRICT, a
present-but-invalid value signals — used at the parse/import boundary so a
corrupt id is rejected rather than silently minting a fresh heading."
  (dorg-id--validate (org-element-property :DORG_ID element) strict))

(defconst dorg-id--drawer-line-re
  "^[ \t]*:DORG_ID\\(\\+\\)?:\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*$"
  "Matches a `:DORG_ID:' or `:DORG_ID+:' property line.
Group 1 is \"+\" for the accumulator key (nil for the base key);
group 2 is the raw value (nil for a bare key).  Case-insensitive when
scanned under `case-fold-search'.")

(defun dorg-id-drawer-value (beg end)
  "Reproduce (org-entry-get POS \"DORG_ID\") for the drawer body BEG..END.
Mirrors `org--property-local-values' + `org-entry-get': the base
`:DORG_ID:' takes its last value with `org-not-nil' applied, `:DORG_ID+:'
values accumulate VERBATIM in order (org-not-nil is applied to the whole
extra list, so it never drops an individual `+' value — e.g. \"nil 20\"
stays \"nil 20\"), and the parts are space-joined.  Returns the combined
value string, or nil when the drawer has no DORG_ID at all."
  (let ((base 'none) (extras nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dorg-id--drawer-line-re end t)
        (if (match-string 1)
            (push (or (match-string 2) "") extras)
          (setq base (or (match-string 2) "")))))
    (let* ((base* (and (not (eq base 'none)) (org-not-nil base)))
           (parts (delq nil (cons base* (nreverse extras)))))
      (when parts (mapconcat #'identity parts " ")))))

(defun dorg-id-at-point-plain ()
  "Return the integer DORG_ID of the heading at point, or nil.
Parser-free hot-loop reader for the cheap scan family: a single
plain-text pass over the heading's identity drawer (past an optional
planning line) via `dorg-id-drawer-value', then `dorg-id--validate'
\(it does allocate lists/strings while combining, but never invokes the
org parser).

Agrees with `dorg-id-at-point' on every well-formed heading WITHOUT
invoking `org-entry-get' / `org-element' (preserving the tracked-case
no-org-parser seam).  A heading has identity only when its drawer is
properly terminated (`org-property-drawer-re' requires `:END:'), matching
org: an unterminated drawer reads as nil, not a phantom id.  Tolerant
only — there is no strict variant on this path."
  (save-excursion
    (let ((case-fold-search t))
      (forward-line 1)
      (when (looking-at-p org-planning-line-re)
        (forward-line 1))
      (when (looking-at org-property-drawer-re)
        (dorg-id--validate (dorg-id-drawer-value (point) (match-end 0)))))))

(provide 'dorg-id)
;;; dorg-id.el ends here
