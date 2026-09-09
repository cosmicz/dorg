;;; dorg-id-tests.el --- Tests for dorg-id snowflake generator -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the snowflake id generator in dorg-id.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'dorg-id)
;; Consumer modules — the transformation tests below drive the real call
;; sites (not just the reader units) so a converted reader is provable
;; end-to-end (tracked-case REVIEW blockers 1a/1b).

;;; Helpers

(defmacro dorg-id-test-with-clean-state (&rest body)
  "Execute BODY with dorg-id internal state reset."
  (declare (indent 0))
  `(let ((dorg-id--last-ms 0)
         (dorg-id--last-seq 0)
         (dorg-id-shard 0))
     ,@body))

(defmacro dorg-id-test-with-mock-clock (ms-var &rest body)
  "Execute BODY with `dorg-id--current-ms' returning MS-VAR.
MS-VAR is a symbol referring to a variable that BODY may mutate
to advance or rewind the mock clock."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'dorg-id--current-ms)
              (lambda () ,ms-var)))
     ,@body))

;;; Layout Tests

(ert-deftest dorg-test-id-generate-positive-bigint ()
  "Generated ids are positive integers (sign bit stays 0)."
  (dorg-id-test-with-clean-state
    (let ((now (+ dorg-id-epoch-ms 1000)))
      (dorg-id-test-with-mock-clock now
        (let ((id (dorg-id-generate)))
          (should (integerp id))
          (should (> id 0)))))))

(ert-deftest dorg-test-id-epoch-encoded ()
  "Ms-offset is encoded in the high bits of the id."
  (dorg-id-test-with-clean-state
    (let* ((offset 123456)
           (now (+ dorg-id-epoch-ms offset)))
      (dorg-id-test-with-mock-clock now
        (let ((id (dorg-id-generate)))
          (should (= (dorg-id-extract-ms id) offset)))))))

(ert-deftest dorg-test-id-shard-encoded ()
  "Shard bits appear in the id."
  (dorg-id-test-with-clean-state
    (setq dorg-id-shard 42)
    (let ((now (+ dorg-id-epoch-ms 500)))
      (dorg-id-test-with-mock-clock now
        (let ((id (dorg-id-generate)))
          (should (= (dorg-id-extract-shard id) 42)))))))

(ert-deftest dorg-test-id-seq-starts-at-zero ()
  "First id in a ms has sequence bits = 0."
  (dorg-id-test-with-clean-state
    (let ((now (+ dorg-id-epoch-ms 1)))
      (dorg-id-test-with-mock-clock now
        (let ((id (dorg-id-generate)))
          (should (= (dorg-id-extract-seq id) 0)))))))

;;; Monotonicity and Uniqueness

(ert-deftest dorg-test-id-monotonic-within-ms ()
  "Successive calls within one ms return strictly increasing ids."
  (dorg-id-test-with-clean-state
    (let ((now (+ dorg-id-epoch-ms 100)))
      (dorg-id-test-with-mock-clock now
        (let ((a (dorg-id-generate))
              (b (dorg-id-generate))
              (c (dorg-id-generate)))
          (should (< a b))
          (should (< b c))
          (should (= (dorg-id-extract-seq a) 0))
          (should (= (dorg-id-extract-seq b) 1))
          (should (= (dorg-id-extract-seq c) 2)))))))

(ert-deftest dorg-test-id-seq-resets-on-ms-advance ()
  "Sequence resets to 0 when the clock advances to a new ms."
  (dorg-id-test-with-clean-state
    (let ((clock (+ dorg-id-epoch-ms 100)))
      (dorg-id-test-with-mock-clock clock
        (let ((a (dorg-id-generate))
              (b (dorg-id-generate)))
          (should (= (dorg-id-extract-seq a) 0))
          (should (= (dorg-id-extract-seq b) 1))
          (setq clock (+ dorg-id-epoch-ms 101))
          (let ((c (dorg-id-generate)))
            (should (= (dorg-id-extract-seq c) 0))
            (should (= (dorg-id-extract-ms c) 101))))))))

(ert-deftest dorg-test-id-unique-over-10k ()
  "10,000 consecutive generations produce unique, strictly
increasing ids."
  (dorg-id-test-with-clean-state
    (let* ((clock (+ dorg-id-epoch-ms 1000))
           (ids nil))
      (dorg-id-test-with-mock-clock clock
        ;; Run the loop — within a single mock ms, the sequence
        ;; will roll over several times because we advance the clock
        ;; inside `dorg-id--wait-next-ms' which calls
        ;; `dorg-id--current-ms' → our mock, returning the same value.
        ;; To avoid an infinite loop on sequence overflow, advance
        ;; the mock clock periodically.
        (dotimes (i 10000)
          (when (and (> i 0) (zerop (mod i 4000)))
            (setq clock (1+ clock)))
          (push (dorg-id-generate) ids)))
      (setq ids (nreverse ids))
      (should (= (length ids) 10000))
      (should (= (length (delete-dups (copy-sequence ids))) 10000))
      ;; Strictly increasing
      (let ((prev (car ids)))
        (dolist (id (cdr ids))
          (should (< prev id))
          (setq prev id))))))

;;; Sequence Overflow

(ert-deftest dorg-test-id-seq-overflow-advances-ms ()
  "Sequence overflow advances to the next ms."
  (dorg-id-test-with-clean-state
    (let ((clock (+ dorg-id-epoch-ms 500)))
      (dorg-id-test-with-mock-clock clock
        ;; Burn 4096 ids (seq 0..4095) within one ms
        (dotimes (_ (1+ dorg-id--seq-max))
          (dorg-id-generate))
        ;; The next call would overflow — advance the mock clock
        ;; so the busy-wait inside generate can succeed.
        (setq clock (1+ clock))
        (let ((id (dorg-id-generate)))
          (should (= (dorg-id-extract-seq id) 0))
          (should (= (dorg-id-extract-ms id) 501)))))))

;;; Clock Skew

(ert-deftest dorg-test-id-clock-skew-errors ()
  "Clock skew backward signals an error."
  (dorg-id-test-with-clean-state
    (let ((clock (+ dorg-id-epoch-ms 1000)))
      (dorg-id-test-with-mock-clock clock
        (dorg-id-generate)
        (setq clock (+ dorg-id-epoch-ms 500))
        (should-error (dorg-id-generate))))))

(ert-deftest dorg-test-id-before-epoch-errors ()
  "Generating before the dorg epoch signals an error."
  (dorg-id-test-with-clean-state
    (let ((clock (- dorg-id-epoch-ms 1)))
      (dorg-id-test-with-mock-clock clock
        (should-error (dorg-id-generate))))))

;;; Shard Validation

(ert-deftest dorg-test-id-invalid-shard-errors ()
  "Out-of-range shard values signal an error."
  (dorg-id-test-with-clean-state
    (let ((now (+ dorg-id-epoch-ms 1)))
      (dorg-id-test-with-mock-clock now
        (let ((dorg-id-shard -1))
          (should-error (dorg-id-generate)))
        (let ((dorg-id-shard (1+ dorg-id--shard-max)))
          (should-error (dorg-id-generate)))
        (let ((dorg-id-shard "bad"))
          (should-error (dorg-id-generate)))))))

(ert-deftest dorg-test-id-different-shards-different-ids ()
  "Ids from different shards at the same ms differ."
  (dorg-id-test-with-clean-state
    (let ((now (+ dorg-id-epoch-ms 1)))
      (dorg-id-test-with-mock-clock now
        (let ((dorg-id-shard 1))
          (let ((id1 (dorg-id-generate)))
            (setq dorg-id--last-seq 0) ;; reset for fair comparison
            (let ((dorg-id-shard 2))
              (let ((id2 (dorg-id-generate)))
                (should (/= id1 id2))
                (should (= (dorg-id-extract-shard id1) 1))
                (should (= (dorg-id-extract-shard id2) 2))))))))))

;;; Shard Default

(ert-deftest dorg-test-id-default-shard-in-random-range ()
  "Default shard is in the random range [16, 1023]."
  ;; Re-evaluate the defcustom standard value
  (let ((std (eval (car (get 'dorg-id-shard 'standard-value)))))
    (should (integerp std))
    (should (>= std 16))
    (should (<= std 1023))))

(ert-deftest dorg-test-id-concurrent-shards-no-collision ()
  "Two processes with different random shards produce distinct IDs
at the same millisecond and sequence."
  (let ((now (+ dorg-id-epoch-ms 1000)))
    (dorg-id-test-with-mock-clock now
      (let* ((dorg-id--last-ms 0)
             (dorg-id--last-seq 0)
             (dorg-id-shard 100)
             (id1 (dorg-id-generate))
             (dorg-id--last-ms 0)
             (dorg-id--last-seq 0)
             (dorg-id-shard 200)
             (id2 (dorg-id-generate)))
        (should (/= id1 id2))
        (should (= (dorg-id-extract-ms id1) (dorg-id-extract-ms id2)))
        (should (= (dorg-id-extract-seq id1) (dorg-id-extract-seq id2)))
        (should (/= (dorg-id-extract-shard id1) (dorg-id-extract-shard id2)))))))

;;; DORG_ID Reader — single malformed-value policy (tracked-case)

;; Helper: first headline element from an org string, as dorg-parse reads it.
(defun dorg-id-test--first-headline (org-text)
  "Return the first headline org-element parsed from ORG-TEXT."
  (with-temp-buffer
    (insert org-text)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'headline #'identity nil t)))

(defmacro dorg-id-test--at-heading (org-text &rest body)
  "Run BODY with point at the first heading of ORG-TEXT in an org buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,org-text)
     (org-mode)
     (goto-char (point-min))
     (re-search-forward org-heading-regexp)
     (beginning-of-line)
     ,@body))

;;; Policy — dorg-id--validate

(ert-deftest dorg-test-id-validate-absent-and-empty ()
  "Absent/empty values read as nil (no identity), never signalling."
  (should (null (dorg-id--validate nil)))
  (should (null (dorg-id--validate "")))
  (should (null (dorg-id--validate "   ")))
  ;; STRICT never signals on absent/empty — a heading may legitimately
  ;; have no DORG_ID (new heading).
  (should (null (dorg-id--validate nil t)))
  (should (null (dorg-id--validate "" t))))

(ert-deftest dorg-test-id-validate-positive-int ()
  "Positive integers read through, with trimming."
  (should (= 42 (dorg-id--validate "42")))
  (should (= 42 (dorg-id--validate "  42  ")))
  (should (= 900 (dorg-id--validate "900"))))

(ert-deftest dorg-test-id-validate-non-positive-is-malformed ()
  "Zero / negative leading integers are malformed -> nil (>0 guard)."
  (should (null (dorg-id--validate "0")))
  (should (null (dorg-id--validate "-5"))))

(ert-deftest dorg-test-id-validate-leading-int-org-equivalent ()
  "Leading-integer parse matches org-entry-get / the drawer scanner."
  (should (= 123 (dorg-id--validate "123abc")))
  (should (= 111 (dorg-id--validate "111 222"))))

(ert-deftest dorg-test-id-validate-rejects-float-and-exponent ()
  "Decimal / exponent forms are NOT integer identities.
`string-to-number' parses \"1.5\" -> 1.5 and \"1e3\" -> 1000.0; the >0
guard alone would admit these floats.  The integer grammar must reject
them: nil in tolerant mode, signal under STRICT.  The intended
leading-integer compatibility (\"123abc\" -> 123) is preserved because
its `string-to-number' result is an integer."
  ;; Tolerant: floats/exponents -> nil (never a non-integer identity).
  (should (null (dorg-id--validate "1.5")))
  (should (null (dorg-id--validate "0.5")))
  (should (null (dorg-id--validate "1e3")))
  (should (null (dorg-id--validate "  2.0  ")))
  ;; STRICT: present-but-invalid float/exponent signals.
  (should-error (dorg-id--validate "1.5" t))
  (should-error (dorg-id--validate "0.5" t))
  (should-error (dorg-id--validate "1e3" t))
  ;; A returned identity is ALWAYS an integer, never a float.
  (should (integerp (dorg-id--validate "42")))
  (should (integerp (dorg-id--validate "123abc"))))

(ert-deftest dorg-test-id-validate-non-numeric ()
  "Non-numeric present values -> nil (non-strict) or error (strict)."
  (should (null (dorg-id--validate "abc")))
  (should (null (dorg-id--validate "not-a-number")))
  (should-error (dorg-id--validate "abc" t))
  (should-error (dorg-id--validate "not-a-number" t))
  ;; "0" is present-but-invalid under STRICT too.
  (should-error (dorg-id--validate "0" t))
  ;; Valid values never signal under STRICT.
  (should (= 42 (dorg-id--validate "42" t)))
  (should (= 123 (dorg-id--validate "123abc" t))))

;;; dorg-id-at-point

(ert-deftest dorg-test-id-at-point-reads-heading ()
  "Reads the DORG_ID of the heading at point."
  (dorg-id-test--at-heading
      "* H\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n"
    (should (= 555 (dorg-id-at-point)))))

(ert-deftest dorg-test-id-at-point-absent ()
  "A heading with no DORG_ID reads as nil."
  (dorg-id-test--at-heading "* H\nno id here\n"
    (should (null (dorg-id-at-point)))))

(ert-deftest dorg-test-id-at-point-malformed ()
  "Malformed value -> nil (non-strict), error (strict)."
  (dorg-id-test--at-heading
      "* H\n:PROPERTIES:\n:DORG_ID: abc\n:END:\n"
    (should (null (dorg-id-at-point)))
    (should-error (dorg-id-at-point t))))

;;; dorg-id-of-element

(ert-deftest dorg-test-id-of-element-reads ()
  "Reads DORG_ID from an org-element headline."
  (let ((el (dorg-id-test--first-headline
             "* H\n:PROPERTIES:\n:DORG_ID: 7777\n:END:\n")))
    (should (= 7777 (dorg-id-of-element el)))))

(ert-deftest dorg-test-id-of-element-malformed ()
  "Malformed element value -> nil (non-strict), error (strict)."
  (let ((el (dorg-id-test--first-headline
             "* H\n:PROPERTIES:\n:DORG_ID: not-a-number\n:END:\n")))
    (should (null (dorg-id-of-element el)))
    (should-error (dorg-id-of-element el t))))

;;; dorg-id-at-point-plain — parser-free hot-loop reader

(ert-deftest dorg-test-id-at-point-plain-basic ()
  "Plain reader returns the id; skips a planning line before the drawer."
  (dorg-id-test--at-heading
      "* H\n:PROPERTIES:\n:DORG_ID: 321\n:END:\n"
    (should (= 321 (dorg-id-at-point-plain))))
  (dorg-id-test--at-heading
      "* H\nDEADLINE: <2026-07-17 Fri>\n:PROPERTIES:\n:DORG_ID: 654\n:END:\n"
    (should (= 654 (dorg-id-at-point-plain)))))

(ert-deftest dorg-test-id-at-point-plain-matches-at-point ()
  "Plain reader agrees with `dorg-id-at-point' across well-formed headings."
  (dolist (case '(("* H\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n" . 100)
                  ("* H\nno id\n" . nil)
                  ("* H\n:PROPERTIES:\n:DORG_ID: abc\n:END:\n" . nil)
                  ("* H\n:PROPERTIES:\n:DORG_ID: 0\n:END:\n" . nil)))
    (dorg-id-test--at-heading (car case)
      (should (equal (cdr case) (dorg-id-at-point)))
      (should (equal (cdr case) (dorg-id-at-point-plain))))))

(ert-deftest dorg-test-id-at-point-plain-no-org-parser ()
  "Plain reader must NOT call org-element / org-entry-get (tracked-case seam).
With the org readers stubbed to signal, it still returns the id."
  (dorg-id-test--at-heading
      "* H\n:PROPERTIES:\n:DORG_ID: 246\n:END:\n"
    (cl-letf (((symbol-function 'org-entry-get)
               (lambda (&rest _) (error "org-entry-get called")))
              ((symbol-function 'org-element-at-point)
               (lambda (&rest _) (error "org-element-at-point called")))
              ((symbol-function 'org--property-local-values)
               (lambda (&rest _) (error "org--property-local-values called"))))
      (should (= 246 (dorg-id-at-point-plain))))))

(ert-deftest dorg-test-id-at-point-plain-dorg-id-plus ()
  "Plain reader reproduces DORG_ID+ accumulation / literal-nil combining."
  (dolist (case '(("* H\n:PROPERTIES:\n:DORG_ID+: 424242\n:END:\n" . 424242)
                  ("* H\n:PROPERTIES:\n:DORG_ID: 111\n:DORG_ID+: 222\n:END:\n" . 111)
                  ("* H\n:PROPERTIES:\n:DORG_ID: nil\n:DORG_ID+: 222\n:END:\n" . 222)
                  ("* H\n:PROPERTIES:\n:DORG_ID: nil\n:END:\n" . nil)
                  ("* H\n:PROPERTIES:\n:DORG_ID+: nil\n:DORG_ID+: 20\n:END:\n" . nil)))
    (dorg-id-test--at-heading (car case)
      (should (equal (cdr case) (dorg-id-at-point-plain))))))

;;; dorg-id-drawer-value — combined-string extractor

(ert-deftest dorg-test-id-drawer-value-combines ()
  "Drawer extractor mirrors org-entry-get combined-string semantics."
  (with-temp-buffer
    (insert "* H\n:PROPERTIES:\n:DORG_ID: 111\n:DORG_ID+: 222\n:END:\n")
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^[ \t]*:PROPERTIES:")
    (beginning-of-line)
    (let ((beg (point))
          (end (progn (re-search-forward "^[ \t]*:END:") (point))))
      (should (equal "111 222" (dorg-id-drawer-value beg end))))))

(provide 'dorg-id-tests)
;;; dorg-id-tests.el ends here
