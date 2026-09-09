;;; dorg-keywords.el --- Root keyword read primitive -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Public read primitive for file-level (`#+KEY: value') keywords.
;;
;; In the dorg model file-level keywords are `dorg-keyword-item' objects
;; attached to the synthetic level-0 root heading (see `dorg-parse.el'
;; and `dorg-model.el').  Neither the parser result nor `dorg-ql' project
;; these items, so consumers that want the raw preamble keywords — e.g.
;; a downstream tool reading its `#+MYAPP_*:' project config — had to either
;; parse a buffer by hand or hand-roll SQL against a derived projection table.
;;
;; `dorg-root-keywords' is the single source-agnostic read path: it loads
;; the root heading from a plain-org buffer/file or from Dolt and returns
;; the keyword items as ordered (KEYWORD . VALUE) cons cells.  Document
;; order and repeated keywords are preserved; values are the raw stored
;; strings with no coercion or cardinality shaping — the caller owns any
;; nil/string/list interpretation.
;;
;; Persistence asymmetry: `dorg-sql-gen' deliberately drops `DORG_*'
;; keywords (buffer-local metadata) before writing to Dolt, so the Dolt
;; source never returns them.  Org sources return every keyword as
;; authored, including `DORG_*'.  Callers that key off user keywords
;; (e.g. `MYAPP_*') are unaffected.

;;; Code:

(require 'cl-lib)
(require 'dorg-error)
(require 'dorg-model)
(require 'dorg-parse)                    ; `dorg-parse-buffer'

;; The `:dolt-dir' read is inverted through a loader seam so this core read
;; primitive stays transport-free: `(require \\='dorg-keywords)' loads neither
;; `dorg-dolt' nor the hard `mysql.el'.  The Dolt hydration provider
;; (`dorg-model-load') self-registers a loader here on load; a `:dolt-dir'
;; read composes the provider lazily on first use when the seam is still nil.
(defvar dorg-root--dolt-loader nil
  "Loader seam for `:dolt-dir' root reads, or nil until a provider registers.
Called as (FN DIR ROOT-ID) and returns the root `dorg-heading' (or nil).
`dorg-model-load' installs `dorg-model-load--root-loader' here on load; the
`:dolt-dir' dispatch below lazy-`require's that provider when the seam is
nil so a direct `(require \\='dorg-keywords)' + Dolt read still self-composes.")

(defun dorg-root--install-dolt-loader (owner loader)
  "Install OWNER's LOADER into the Dolt root-reader seam.
Only `dorg-model-load' owns this seam.  Reinstalling the identical
symbol-valued function is an idempotent no-op; a foreign, malformed, or
different replacement signals `dorg-seam-install-error' before changing the
installed loader."
  (unless (and (eq owner 'dorg-model-load) (functionp loader))
    (signal 'dorg-seam-install-error
            (list "dorg: refused foreign or malformed root-loader install"
                  :owner owner)))
  (cond
   ((null dorg-root--dolt-loader)
    (setq dorg-root--dolt-loader loader))
   ((eq loader dorg-root--dolt-loader)
    nil)
   (t
    (signal 'dorg-seam-install-error
            (list "dorg: refused replacement of installed root-loader seam"
                  :owner owner)))))

(defun dorg-root--load-root-heading (source)
  "Return the level-0 root `dorg-heading' for SOURCE, or nil.
See `dorg-root-keywords' for the accepted SOURCE forms."
  (pcase source
    (`(:dolt-dir ,dir :root-id ,id)
     ;; Dispatch through the loader seam (never a direct transport reach).
     ;; When no provider has registered yet, lazy-load `dorg-model-load' —
     ;; it registers the loader on load — then re-check; only a genuinely
     ;; absent provider (not on load-path) errors.  The loader itself does
     ;; the root-only slice (just the root heading + items, never the whole
     ;; tree) so config reads stay bounded.
     (unless dorg-root--dolt-loader
       (require 'dorg-model-load nil t))
     (unless dorg-root--dolt-loader
       (error "dorg-root-keywords: no :dolt-dir loader registered (dorg-model-load unavailable)"))
     (funcall dorg-root--dolt-loader dir id))
    (`(:buffer ,buf)
     (car (dorg-parse-result-headings (dorg-parse-buffer buf))))
    (`(:buffer)
     (car (dorg-parse-result-headings (dorg-parse-buffer))))
    (`(:file ,path)
     (with-temp-buffer
       (insert-file-contents path)
       (org-mode)
       (car (dorg-parse-result-headings (dorg-parse-buffer)))))
    (_ (error "dorg-root-keywords: unrecognized source %S" source))))

(defun dorg-root-keywords (source)
  "Return SOURCE's root-level keywords as an ordered (KEYWORD . VALUE) list.
Each element is a cons of the keyword name and its raw string value.
Document order is preserved and repeated keywords are kept as separate
elements; values are returned verbatim with no coercion or shaping.

SOURCE is a plist selecting the backing store:
  (:dolt-dir DIR :root-id ID)  ROOT-ID's root heading loaded from Dolt DIR
  (:buffer BUF)                org BUF parsed in place (BUF may be nil)
  (:buffer)                    the current buffer
  (:file PATH)                 the plain org file at PATH

Returns nil when the source has no root heading (e.g. an unknown
ROOT-ID) or no keyword items."
  (let ((root (dorg-root--load-root-heading source)))
    (when root
      (mapcan (lambda (item)
                (when (cl-typep item 'dorg-keyword-item)
                  (list (cons (dorg-keyword-item-keyword item)
                              (dorg-keyword-item-value item)))))
              (dorg-heading-items root)))))

(defun dorg-root-keyword-values (source keyword)
  "Return the ordered raw values of KEYWORD on SOURCE's root heading.
KEYWORD is matched case-sensitively against stored keyword names; org
upcases `#+KEY:' names on parse, so pass an upper-case KEYWORD.  Returns
nil when KEYWORD is absent, otherwise a list of every value in document
order (one element per occurrence).  See `dorg-root-keywords' for SOURCE
forms."
  (mapcan (lambda (kv)
            (when (equal (car kv) keyword)
              (list (cdr kv))))
          (dorg-root-keywords source)))

(provide 'dorg-keywords)
;;; dorg-keywords.el ends here
