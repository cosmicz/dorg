;;; dorg-timestamp-tests.el --- Tests for dorg-timestamp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the pure structured Org timestamp parser.

;;; Code:

(require 'ert)
(require 'dorg-timestamp)

;;; ─── Active / Inactive ─────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-active ()
  "Active `<...>' timestamps set ACTIVE to t."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri>")))
    (should (dorg-timestamp-p ts))
    (should (eq (dorg-timestamp-active ts) t))))

(ert-deftest dorg-test-timestamp-inactive ()
  "Inactive `[...]' timestamps set ACTIVE to nil."
  (let ((ts (dorg-timestamp-parse "[2026-05-01 Fri]")))
    (should (dorg-timestamp-p ts))
    (should (null (dorg-timestamp-active ts)))))

(ert-deftest dorg-test-timestamp-mismatched-delimiters ()
  "Mismatched open/close delimiters do not parse."
  (should (null (dorg-timestamp-parse "<2026-05-01]")))
  (should (null (dorg-timestamp-parse "[2026-05-01>"))))

(ert-deftest dorg-test-timestamp-read-only ()
  "Parsed timestamps are immutable; `setf' on an accessor errors.
The `setf' form is built at runtime with the struct spliced in so the
missing read-only setter is not macroexpanded at file load time."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri>")))
    (should (dorg-timestamp-p ts))
    (should-error (eval `(setf (dorg-timestamp-raw ,ts) "mutated") t))
    (should-error (eval `(setf (dorg-timestamp-start-date ,ts) '(1 1 1)) t))
    ;; The original value is intact after the failed mutation.
    (should (equal (dorg-timestamp-raw ts) "<2026-05-01 Fri>"))))

;;; ─── Date-only ─────────────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-date-only ()
  "Date-only timestamps parse START-DATE as integers and leave the
rest nil."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri>")))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 1)))
    (should (null (dorg-timestamp-end-date ts)))
    (should (null (dorg-timestamp-start-time ts)))
    (should (null (dorg-timestamp-end-time ts)))
    (should (null (dorg-timestamp-repeater-type ts)))))

(ert-deftest dorg-test-timestamp-date-only-no-dayname ()
  "Day name is optional."
  (let ((ts (dorg-timestamp-parse "<2026-12-09>")))
    (should (dorg-timestamp-p ts))
    (should (equal (dorg-timestamp-start-date ts) '(2026 12 9)))))

(ert-deftest dorg-test-timestamp-raw-preserved ()
  "RAW holds the trimmed source text."
  (let ((ts (dorg-timestamp-parse "  <2026-05-01 Fri>  ")))
    (should (equal (dorg-timestamp-raw ts) "<2026-05-01 Fri>"))))

;;; ─── Date Ranges ───────────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-date-range ()
  "Two-token date ranges preserve start and end dates."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri>--<2026-05-03 Sun>")))
    (should (dorg-timestamp-p ts))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 1)))
    (should (equal (dorg-timestamp-end-date ts) '(2026 5 3)))
    (should (eq (dorg-timestamp-active ts) t))))

(ert-deftest dorg-test-timestamp-inactive-date-range ()
  "Inactive date ranges parse and report ACTIVE nil."
  (let ((ts (dorg-timestamp-parse "[2026-05-01]--[2026-05-03]")))
    (should (dorg-timestamp-p ts))
    (should (null (dorg-timestamp-active ts)))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 1)))
    (should (equal (dorg-timestamp-end-date ts) '(2026 5 3)))))

(ert-deftest dorg-test-timestamp-date-range-with-times ()
  "A date range carries START-TIME from the first and END-TIME from
the second timestamp."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 09:00>--<2026-05-03 17:30>")))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 1)))
    (should (equal (dorg-timestamp-end-date ts) '(2026 5 3)))
    (should (equal (dorg-timestamp-start-time ts) '(9 0)))
    (should (equal (dorg-timestamp-end-time ts) '(17 30)))))

;;; ─── Times ─────────────────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-time ()
  "A single time sets START-TIME and leaves END-TIME nil."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri 09:30>")))
    (should (equal (dorg-timestamp-start-time ts) '(9 30)))
    (should (null (dorg-timestamp-end-time ts)))
    (should (null (dorg-timestamp-end-date ts)))))

(ert-deftest dorg-test-timestamp-same-day-time-range ()
  "A same-day `HH:MM-HH:MM' range sets both times but no END-DATE."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri 09:00-11:00>")))
    (should (equal (dorg-timestamp-start-time ts) '(9 0)))
    (should (equal (dorg-timestamp-end-time ts) '(11 0)))
    (should (null (dorg-timestamp-end-date ts)))))

;;; ─── Repeaters ─────────────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-repeater-plus ()
  "`+' repeaters parse marker, value, and unit."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri +1w>")))
    (should (equal (dorg-timestamp-repeater-type ts) "+"))
    (should (equal (dorg-timestamp-repeater-value ts) 1))
    (should (equal (dorg-timestamp-repeater-unit ts) "w"))))

(ert-deftest dorg-test-timestamp-repeater-plusplus ()
  "`++' catch-up repeaters parse the two-character marker."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 ++2m>")))
    (should (equal (dorg-timestamp-repeater-type ts) "++"))
    (should (equal (dorg-timestamp-repeater-value ts) 2))
    (should (equal (dorg-timestamp-repeater-unit ts) "m"))))

(ert-deftest dorg-test-timestamp-repeater-dotplus ()
  "`.+' restart repeaters parse the dotted marker."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 .+3d>")))
    (should (equal (dorg-timestamp-repeater-type ts) ".+"))
    (should (equal (dorg-timestamp-repeater-value ts) 3))
    (should (equal (dorg-timestamp-repeater-unit ts) "d"))))

(ert-deftest dorg-test-timestamp-repeater-units ()
  "All d/w/m/y units are accepted."
  (dolist (unit '("d" "w" "m" "y"))
    (let ((ts (dorg-timestamp-parse (format "<2026-05-01 +1%s>" unit))))
      (should (equal (dorg-timestamp-repeater-unit ts) unit)))))

(ert-deftest dorg-test-timestamp-time-and-repeater ()
  "Time and repeater coexist in one timestamp."
  (let ((ts (dorg-timestamp-parse "<2026-05-01 Fri 09:00 +1w>")))
    (should (equal (dorg-timestamp-start-time ts) '(9 0)))
    (should (equal (dorg-timestamp-repeater-type ts) "+"))
    (should (equal (dorg-timestamp-repeater-value ts) 1))
    (should (equal (dorg-timestamp-repeater-unit ts) "w"))))

;;; ─── Warning Cookies ───────────────────────────────────────────────

(ert-deftest dorg-test-timestamp-warning-relative ()
  "A `-Nd' warning cookie parses marker, value, and unit."
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed -3d>")))
    (should (dorg-timestamp-p ts))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 20)))
    (should (equal (dorg-timestamp-warning-type ts) "-"))
    (should (equal (dorg-timestamp-warning-value ts) 3))
    (should (equal (dorg-timestamp-warning-unit ts) "d"))))

(ert-deftest dorg-test-timestamp-warning-absolute ()
  "The absolute `--Nd' warning form keeps the two-character marker."
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed --3d>")))
    (should (equal (dorg-timestamp-warning-type ts) "--"))
    (should (equal (dorg-timestamp-warning-value ts) 3))
    (should (equal (dorg-timestamp-warning-unit ts) "d"))))

(ert-deftest dorg-test-timestamp-warning-units ()
  "All d/w/m/y warning units are accepted."
  (dolist (unit '("d" "w" "m" "y"))
    (let ((ts (dorg-timestamp-parse (format "<2026-05-20 -2%s>" unit))))
      (should (equal (dorg-timestamp-warning-unit ts) unit)))))

(ert-deftest dorg-test-timestamp-warning-with-time-and-repeater ()
  "A warning cookie coexists with a time and a repeater in any order."
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed 09:00 +1w -2d>")))
    (should (equal (dorg-timestamp-start-time ts) '(9 0)))
    (should (equal (dorg-timestamp-repeater-type ts) "+"))
    (should (equal (dorg-timestamp-repeater-value ts) 1))
    (should (equal (dorg-timestamp-repeater-unit ts) "w"))
    (should (equal (dorg-timestamp-warning-type ts) "-"))
    (should (equal (dorg-timestamp-warning-value ts) 2))
    (should (equal (dorg-timestamp-warning-unit ts) "d")))
  ;; Warning before repeater must parse identically.
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed -2d +1w>")))
    (should (equal (dorg-timestamp-repeater-unit ts) "w"))
    (should (equal (dorg-timestamp-warning-unit ts) "d"))))

(ert-deftest dorg-test-timestamp-warning-inactive ()
  "Inactive timestamps carry a warning cookie."
  (let ((ts (dorg-timestamp-parse "[2026-05-20 Wed -1m]")))
    (should (null (dorg-timestamp-active ts)))
    (should (equal (dorg-timestamp-warning-type ts) "-"))
    (should (equal (dorg-timestamp-warning-unit ts) "m"))))

(ert-deftest dorg-test-timestamp-warning-date-range ()
  "A date range takes its warning cookie from the first timestamp."
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed -3d>--<2026-05-22 Fri>")))
    (should (equal (dorg-timestamp-start-date ts) '(2026 5 20)))
    (should (equal (dorg-timestamp-end-date ts) '(2026 5 22)))
    (should (equal (dorg-timestamp-warning-type ts) "-"))
    (should (equal (dorg-timestamp-warning-value ts) 3))
    (should (equal (dorg-timestamp-warning-unit ts) "d"))))

(ert-deftest dorg-test-timestamp-warning-absent-nil ()
  "Timestamps without a warning cookie leave the warning slots nil."
  (let ((ts (dorg-timestamp-parse "<2026-05-20 Wed +1w>")))
    (should (null (dorg-timestamp-warning-type ts)))
    (should (null (dorg-timestamp-warning-value ts)))
    (should (null (dorg-timestamp-warning-unit ts)))))

(ert-deftest dorg-test-timestamp-invalid-warning-rejected ()
  "A bare or malformed warning token rejects the whole timestamp."
  (should (null (dorg-timestamp-parse "<2026-05-20 -1>")))
  (should (null (dorg-timestamp-parse "<2026-05-20 -d>")))
  (should (null (dorg-timestamp-parse "<2026-05-20 -1h>"))))

;;; ─── Whitespace & Invalid Input ────────────────────────────────────

(ert-deftest dorg-test-timestamp-whitespace-trimmed ()
  "Leading/trailing whitespace is ignored."
  (should (dorg-timestamp-p (dorg-timestamp-parse "\t<2026-05-01>\n"))))

(ert-deftest dorg-test-timestamp-embedded-text-no-match ()
  "Arbitrary text around a timestamp does not produce a partial match."
  (should (null (dorg-timestamp-parse "see <2026-05-01 Fri> for details")))
  (should (null (dorg-timestamp-parse "<2026-05-01> <2026-05-02>"))))

(ert-deftest dorg-test-timestamp-invalid-input ()
  "Non-timestamp and malformed input returns nil."
  (should (null (dorg-timestamp-parse "")))
  (should (null (dorg-timestamp-parse "   ")))
  (should (null (dorg-timestamp-parse "not a timestamp")))
  (should (null (dorg-timestamp-parse "<>")))
  (should (null (dorg-timestamp-parse "<2026-5-1>")))
  (should (null (dorg-timestamp-parse "2026-05-01")))
  (should (null (dorg-timestamp-parse nil))))

(ert-deftest dorg-test-timestamp-invalid-repeater-rejected ()
  "A bare or malformed repeater token rejects the whole timestamp."
  (should (null (dorg-timestamp-parse "<2026-05-01 +1>")))
  (should (null (dorg-timestamp-parse "<2026-05-01 +w>")))
  (should (null (dorg-timestamp-parse "<2026-05-01 +1h>"))))

(ert-deftest dorg-test-timestamp-unclassifiable-token-rejected ()
  "A non-date, non-time, non-repeater, non-alpha token rejects."
  (should (null (dorg-timestamp-parse "<2026-05-01 12:99:99>")))
  (should (null (dorg-timestamp-parse "<2026-05-01 9am>"))))

(provide 'dorg-timestamp-tests)
;;; dorg-timestamp-tests.el ends here
