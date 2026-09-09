;;; dorg-error.el --- Public error taxonomy root for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The single public ROOT of dorg's error taxonomy (audit finding F2).
;;
;; Historically the three public surface conditions — `dorg-api-error'
;; (command envelope), `dorg-mutate-error' (mutation engine), and
;; `dorg-ql-error' (query validation) — were parallel siblings under plain
;; `error', with no shared ancestor and two byte-identical accessor pairs
;; duplicated across `dorg-api.el' and `dorg-mutate.el'.  A caller could not
;; catch "any dorg error" in one `condition-case' clause, and introspection
;; required knowing which of the three symbols was in hand.
;;
;; This module introduces one catchable root and one generic introspection
;; surface, WITHOUT collapsing the typed leaves:
;;
;;   - `dorg-error' — the public root condition.  The three surface conditions
;;     are reparented under it (an additive change: each leaf symbol stays in
;;     its own `error-conditions' list, so every existing `condition-case'
;;     handler and every `should-error :type ...' keeps matching).  A single
;;     `(dorg-error ...)' handler now catches a signal from any leaf.
;;
;;   - `dorg-error-data' / `dorg-error-reason' — one generic accessor pair that
;;     spans the whole taxonomy.  `dorg-error-data' returns the structured
;;     plist carried by api/mutate/query errors and degrades to nil for any
;;     non-dorg condition.
;;
;; The per-symbol accessors in `dorg-api.el' / `dorg-mutate.el' are kept for
;; backward compatibility but rewritten to DELEGATE to `dorg-error-data',
;; removing the duplication while preserving their exact per-symbol behavior
;; (including cross-symbol isolation).
;;
;; This module is foundational: it requires only `cl-lib' and is required by
;; the three leaf modules before their `define-error' forms.

;;; Code:

(require 'cl-lib)

;;; ─── Root condition ───────────────────────────────────────────────────────

(define-error 'dorg-error "dorg operation failed")

(define-error 'dorg-seam-install-error
  "DOrg dependency seam installation refused"
  'dorg-error)

(defconst dorg-seam-install-error 'dorg-seam-install-error
  "Condition symbol for refused dependency-seam installation.")

;;; ─── Generic introspection accessors ──────────────────────────────────────

(defun dorg-error-data (err)
  "Return the structured data plist of a `dorg-error'-rooted condition ERR.
ERR is the object bound by `condition-case' — a list whose car is the
condition symbol.  Returns the third element (the structured plist) only
when ERR belongs to the `dorg-error' taxonomy AND that slot is a plist;
otherwise returns nil.

The plist guard is deliberate: a foreign `error' is not part of the
taxonomy, and a malformed rooted condition may carry non-plist data, so
both degrade to nil rather than returning raw list data."
  (when (memq 'dorg-error (get (car-safe err) 'error-conditions))
    (let ((slot (nth 2 err)))
      (and (plistp slot) slot))))

(defun dorg-error-reason (err)
  "Return the `:reason' symbol of a `dorg-error'-rooted condition ERR.
Delegates to `dorg-error-data', so it degrades to nil wherever that does
(foreign errors and non-plist data slots)."
  (plist-get (dorg-error-data err) :reason))

(provide 'dorg-error)
;;; dorg-error.el ends here
