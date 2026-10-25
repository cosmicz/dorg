;;; dorg-public-scrub.el --- Shared public-export scrub predicate -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One dependency-free predicate classifies private coordination metadata in
;; public-export bytes.  Both the ERT census and the release gate load this
;; file, so they cannot silently drift onto different rules.
;;
;; Private means tracker references and bead-shaped slugs, seat identities,
;; home-directory paths and the internal codename.  The names of the sibling
;; tools that consume DOrg, and their data directories, are public product
;; names and are not classified (operator decision, 2026-10-05).  The
;; home-directory pattern is spelled in two pieces because this file is
;; itself exported.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst dorg-public-scrub--identifier-boundary "[^[:alnum:]_]"
  "Regexp fragment matching a character outside an identifier.")

(defun dorg-public-scrub--bounded (body)
  "Return an Emacs regexp matching BODY outside identifier characters."
  (concat "\\(?:\\`\\|" dorg-public-scrub--identifier-boundary "\\)"
          "\\(?:" body "\\)"
          "\\(?:\\'\\|" dorg-public-scrub--identifier-boundary "\\)"))

(defconst dorg-public-scrub--bare-slug-regexp
  (concat "\\(?:\\`\\|[^[:alnum:]_-]\\)"
          "\\(dorg-[a-z0-9]\\{3,5\\}"
          "\\(?:\\.[a-z0-9]\\{3,5\\}\\)*\\)"
          "\\(?:\\'\\|[^[:alnum:]_-]\\)")
  "Regexp recognizing a complete tracker-shaped bare slug.")

(defconst dorg-public-scrub-rules
  `(("tracker reference"
     . ,(dorg-public-scrub--bounded
         (concat "b" "d:[a-z0-9][a-z0-9-]*")))
    ("agent identity"
     . ,(dorg-public-scrub--bounded
         "\\(?:drv\\|nav\\|Ctl\\)-[[:alnum:]]\\{4,\\}"))
    ("private path" . ,(regexp-quote (concat "/Us" "ers/")))
    ("internal codename"
     . ,(dorg-public-scrub--bounded (concat "Amb" "ient")))
    ("bare tracker slug" . ,dorg-public-scrub--bare-slug-regexp))
  "Named private-token classes forbidden in public-export bytes.
Tool names and their data directories are deliberately absent: they are public
product names (operator decision, 2026-10-05).")

(defconst dorg-public-scrub-bare-slug-allowlist
  '(("dorg-api.elc" . "Public compiled-library filename used in a fixture.")
    ("dorg-api.org" . "Public example document filename used in a fixture.")
    ("dorg-aware" . "Public descriptive term for DOrg-aware behavior.")
    ("dorg-bound" . "Public descriptive term for a DOrg-bound buffer.")
    ("dorg-demo" . "Public example repository name used in documentation.")
    ("dorg-diff" . "Public conceptual name for DOrg diff behavior.")
    ("dorg-dir" . "Public fixture variable and directory name.")
    ("dorg-fake" . "Public fake service name used in tests.")
    ("dorg-file" . "Public example file stem used in tests and documentation.")
    ("dorg-foo" . "Public placeholder package name used in a fixture.")
    ("dorg-home" . "Public example configuration directory name.")
    ("dorg-merge" . "Public conceptual name for DOrg merge behavior.")
    ("dorg-pos" . "Public test helper name for a DOrg buffer position.")
    ("dorg-probe" . "Public descriptive name for a DOrg health probe.")
    ("dorg-scope" . "Public fixture name for a DOrg operation scope.")
    ("dorg-sync" . "Public conceptual name for DOrg synchronization behavior.")
    ("dorg-test" . "Public test fixture and scratch namespace.")
    ("dorg-test.org" . "Public fixture document filename.")
    ("dorg-write.lock" . "Stable local writer-lock filename stem."))
  "Exact public tokens exempt from tracker-shaped bare-slug classification.
Every entry is (TOKEN . RATIONALE); prefix, suffix, and regexp exemptions are
deliberately unsupported.")

(defun dorg-public-scrub--known-p (token known-dorg-tokens)
  "Return non-nil when TOKEN belongs to KNOWN-DORG-TOKENS.
KNOWN-DORG-TOKENS may be a hash table or a list of strings."
  (if (hash-table-p known-dorg-tokens)
      (gethash token known-dorg-tokens)
    (member token known-dorg-tokens)))

(defun dorg-public-scrub--bare-slug-p (text known-dorg-tokens)
  "Return non-nil when TEXT contains an unapproved tracker-shaped slug."
  (let ((case-fold-search nil)
        (start 0)
        found)
    (while (and (not found)
                (string-match dorg-public-scrub--bare-slug-regexp text start))
      (let ((token (match-string 1 text)))
        (unless (or (dorg-public-scrub--known-p token known-dorg-tokens)
                    (assoc token dorg-public-scrub-bare-slug-allowlist))
          (setq found t)))
      (setq start (match-end 0)))
    found))

(defun dorg-public-scrub-classify-string (text known-dorg-tokens)
  "Return each forbidden class found in TEXT, at most once.
KNOWN-DORG-TOKENS contains legitimate public Lisp symbols which must not be
classified as tracker-shaped bare slugs."
  (let ((case-fold-search t)
        classes)
    (dolist (rule dorg-public-scrub-rules)
      (when (and (not (equal "bare tracker slug" (car rule)))
                 (string-match-p (cdr rule) text))
        (push (car rule) classes)))
    (when (dorg-public-scrub--bare-slug-p text known-dorg-tokens)
      (push "bare tracker slug" classes))
    (nreverse classes)))

(provide 'dorg-public-scrub)
;;; dorg-public-scrub.el ends here
