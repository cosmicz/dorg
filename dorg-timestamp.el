;;; dorg-timestamp.el --- Structured Org timestamp parsing -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure structured parser for Org timestamp raw text.  Downstream
;; tools (heading accessors, LOGBOOK activity helpers, dorg-ql
;; predicates) should project raw Org timestamp strings through this
;; one parser rather than re-parsing strings independently.
;;
;; This module is parsing only.  It does not implement query windows,
;; agenda display, overdue policy, or calendar expansion — those
;; belong in dorg-ql or downstream callers.  It also does not change
;; how `dorg-planning-item' stores or renders its raw scheduled,
;; deadline, and closed strings.
;;
;; Public API:
;; - `dorg-timestamp-parse'  — RAW Org timestamp text -> struct or nil
;; - `dorg-timestamp-p'      — predicate
;; - accessors: `dorg-timestamp-raw', `dorg-timestamp-active',
;;   `dorg-timestamp-start-date', `dorg-timestamp-end-date',
;;   `dorg-timestamp-start-time', `dorg-timestamp-end-time',
;;   `dorg-timestamp-repeater-type', `dorg-timestamp-repeater-value',
;;   `dorg-timestamp-repeater-unit', `dorg-timestamp-warning-type',
;;   `dorg-timestamp-warning-value', `dorg-timestamp-warning-unit'
;;
;; Representations:
;; - Dates are `(YEAR MONTH DAY)' integer lists.
;; - Times are `(HOUR MINUTE)' integer lists.
;; - `active' is t for `<...>' timestamps, nil for `[...]'.
;; - `repeater-type' is the raw Org marker string: "+", "++", or ".+".
;; - `warning-type' is the raw Org warning-cookie marker string: "-"
;;   (relative) or "--" (absolute).  The warning offset is preserved but
;;   does not shift the matched date span — see commentary at
;;   `dorg-timestamp--parse-warning'.
;; - `end-date' is non-nil only for genuine `--' date ranges.
;; - `end-time' is non-nil only for same-day `HH:MM-HH:MM' time ranges
;;   or for the trailing timestamp of a date range that carries a time.
;; - All optional fields are nil when absent.

;;; Code:

(require 'cl-lib)

(cl-defstruct (dorg-timestamp
               (:constructor dorg-timestamp--create)
               (:copier nil)
               (:predicate dorg-timestamp-p))
  "An immutable structured Org timestamp.

Construct only via `dorg-timestamp-parse'.  All slots are
`:read-only', so accessors have no `setf' setter and mutation
attempts signal an error.  See this module's commentary for the
field representations."
  (raw nil :read-only t)
  (active nil :read-only t)
  (start-date nil :read-only t)
  (end-date nil :read-only t)
  (start-time nil :read-only t)
  (end-time nil :read-only t)
  (repeater-type nil :read-only t)
  (repeater-value nil :read-only t)
  (repeater-unit nil :read-only t)
  (warning-type nil :read-only t)
  (warning-value nil :read-only t)
  (warning-unit nil :read-only t))

;;; ─── Atom Parsers ──────────────────────────────────────────────────

(defun dorg-timestamp--parse-date (str)
  "Parse STR as a `YYYY-MM-DD' date atom.
Return `(YEAR MONTH DAY)' integers, or nil if STR is not a bare date."
  (when (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
                      str)
    (list (string-to-number (match-string 1 str))
          (string-to-number (match-string 2 str))
          (string-to-number (match-string 3 str)))))

(defun dorg-timestamp--parse-time (str)
  "Parse STR as `HH:MM' or `HH:MM-HH:MM'.
Return `(START . END)' where START is `(HOUR MINUTE)' and END is
either `(HOUR MINUTE)' for a same-day range or nil.  Return nil when
STR is not a time atom."
  (when (string-match
         "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?:-\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\'"
         str)
    (cons (list (string-to-number (match-string 1 str))
                (string-to-number (match-string 2 str)))
          (when (match-string 3 str)
            (list (string-to-number (match-string 3 str))
                  (string-to-number (match-string 4 str)))))))

(defun dorg-timestamp--parse-repeater (str)
  "Parse STR as an Org repeater atom.
Return `(MARKER VALUE UNIT)' where MARKER is one of \"+\", \"++\",
or \".+\", VALUE is an integer, and UNIT is one of d/w/m/y as a
string.  Return nil when STR is not a supported repeater."
  (when (string-match "\\`\\(\\.\\+\\|\\+\\+\\|\\+\\)\\([0-9]+\\)\\([dwmy]\\)\\'"
                      str)
    (list (match-string 1 str)
          (string-to-number (match-string 2 str))
          (match-string 3 str))))

(defun dorg-timestamp--parse-warning (str)
  "Parse STR as an Org deadline/scheduled warning-cookie atom.
Return `(MARKER VALUE UNIT)' where MARKER is \"-\" (relative warning)
or \"--\" (absolute warning), VALUE is an integer, and UNIT is one of
d/w/m/y as a string.  Return nil when STR is not a warning cookie.

The warning offset is parsed and preserved but is advisory metadata:
it tells agenda display when to begin warning and never shifts the
timestamp's own date span (see `dorg-ql' window matching)."
  (when (string-match "\\`\\(--\\|-\\)\\([0-9]+\\)\\([dwmy]\\)\\'" str)
    (list (match-string 1 str)
          (string-to-number (match-string 2 str))
          (match-string 3 str))))

;;; ─── Single Timestamp ──────────────────────────────────────────────

(defun dorg-timestamp--parse-single (str)
  "Parse STR as one delimited Org timestamp.
STR must be exactly one `<...>' (active) or `[...]' (inactive) group
with no surrounding text.  Return a plist of parsed fields, or nil.

Tokens after the leading date are classified as a time atom, a
repeater, a warning cookie, or a single alphabetic day name (ignored
for structure).  Any unclassifiable token rejects the whole timestamp."
  (when (>= (length str) 3)
    (let* ((open (aref str 0))
           (close (aref str (1- (length str))))
           (active (cond ((and (eq open ?<) (eq close ?>)) t)
                         ((and (eq open ?\[) (eq close ?\])) nil)
                         (t 'invalid))))
      (unless (eq active 'invalid)
        (let* ((inner (substring str 1 (1- (length str))))
               (tokens (split-string inner)))
          (when tokens
            (let ((start-date (dorg-timestamp--parse-date (car tokens)))
                  (start-time nil) (end-time nil)
                  (rtype nil) (rval nil) (runit nil)
                  (wtype nil) (wval nil) (wunit nil)
                  (day-seen nil) (time-seen nil) (rep-seen nil)
                  (warn-seen nil) (ok t))
              (when start-date
                (dolist (tok (cdr tokens))
                  (when ok
                    (let ((time (and (not time-seen)
                                     (dorg-timestamp--parse-time tok)))
                          (rep (and (not rep-seen)
                                    (dorg-timestamp--parse-repeater tok)))
                          (warn (and (not warn-seen)
                                     (dorg-timestamp--parse-warning tok))))
                      (cond
                       (time (setq start-time (car time)
                                   end-time (cdr time)
                                   time-seen t))
                       (rep (setq rtype (nth 0 rep)
                                  rval (nth 1 rep)
                                  runit (nth 2 rep)
                                  rep-seen t))
                       (warn (setq wtype (nth 0 warn)
                                   wval (nth 1 warn)
                                   wunit (nth 2 warn)
                                   warn-seen t))
                       ((and (not day-seen)
                             (string-match-p "\\`[[:alpha:]]+\\'" tok))
                        (setq day-seen t))
                       (t (setq ok nil))))))
                (when ok
                  (list :active active
                        :start-date start-date
                        :start-time start-time
                        :end-time end-time
                        :repeater-type rtype
                        :repeater-value rval
                        :repeater-unit runit
                        :warning-type wtype
                        :warning-value wval
                        :warning-unit wunit))))))))))

;;; ─── Date Ranges ───────────────────────────────────────────────────

(defun dorg-timestamp--split-range (str)
  "Split STR as two delimited timestamps joined by `--'.
Return `(FIRST . SECOND)' raw substrings, or nil when STR is not a
two-token Org date range.  The match is anchored so embedded text
cannot produce a partial range."
  (when (string-match
         "\\`\\(<[^<>]*>\\|\\[[^][]*\\]\\)--\\(<[^<>]*>\\|\\[[^][]*\\]\\)\\'"
         str)
    (cons (match-string 1 str) (match-string 2 str))))

;;; ─── Public Entry Point ────────────────────────────────────────────

(defun dorg-timestamp-parse (raw)
  "Parse RAW Org timestamp text into a `dorg-timestamp' struct.
Return nil when RAW is not a single complete Org timestamp.

Leading and trailing whitespace is ignored, but RAW must be exactly
one timestamp (or one `--' date range); arbitrary embedded text does
not produce a partial match."
  (when (stringp raw)
    (let ((trimmed (replace-regexp-in-string
                    "\\`[ \t\n\r\f]+\\|[ \t\n\r\f]+\\'" "" raw)))
      (unless (string= trimmed "")
        (let ((range (dorg-timestamp--split-range trimmed)))
          (if range
              (let ((a (dorg-timestamp--parse-single (car range)))
                    (b (dorg-timestamp--parse-single (cdr range))))
                (when (and a b)
                  (dorg-timestamp--create
                   :raw trimmed
                   :active (plist-get a :active)
                   :start-date (plist-get a :start-date)
                   :end-date (plist-get b :start-date)
                   :start-time (plist-get a :start-time)
                   :end-time (plist-get b :start-time)
                   :repeater-type (plist-get a :repeater-type)
                   :repeater-value (plist-get a :repeater-value)
                   :repeater-unit (plist-get a :repeater-unit)
                   :warning-type (plist-get a :warning-type)
                   :warning-value (plist-get a :warning-value)
                   :warning-unit (plist-get a :warning-unit))))
            (let ((p (dorg-timestamp--parse-single trimmed)))
              (when p
                (dorg-timestamp--create
                 :raw trimmed
                 :active (plist-get p :active)
                 :start-date (plist-get p :start-date)
                 :end-date nil
                 :start-time (plist-get p :start-time)
                 :end-time (plist-get p :end-time)
                 :repeater-type (plist-get p :repeater-type)
                 :repeater-value (plist-get p :repeater-value)
                 :repeater-unit (plist-get p :repeater-unit)
                 :warning-type (plist-get p :warning-type)
                 :warning-value (plist-get p :warning-value)
                 :warning-unit (plist-get p :warning-unit))))))))))

(provide 'dorg-timestamp)
;;; dorg-timestamp.el ends here
