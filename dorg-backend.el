;;; dorg-backend.el --- Storage backend abstraction for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Storage backend abstraction for dorg.  A backend identifies how a
;; document is persisted (plain org file vs. Dolt-backed).
;;
;; Two backend kinds are defined here:
;;
;;   plain-org  — the org file itself is the source of truth.  No Dolt
;;                binary is required.  Materialize re-reads the file;
;;                sync saves the buffer.
;;
;;   dolt       — the Dolt repository is the source of truth.  The
;;                file is a materialized view.  Materialize renders
;;                from SQL; sync parses the buffer and writes back.
;;
;; The registry (`dorg-registry.el') stores a backend struct on every
;; `dorg-registration', and the materialize/sync entry points dispatch
;; on the registered backend.

;;; Code:

(require 'cl-lib)

;;; ─── Root-preamble binding keywords ───────────────────────────────────

(defconst dorg-dolt-dir-keyword "DORG_DOLT_DIR"
  "Root-preamble keyword naming the on-disk Dolt store directory.
Single source for the current spelling; the legacy pre-rename alias lives
only in `dorg--dolt-dir-marker-keywords' (see
`dorg-test-dorg-dir-literal-confined-to-reader').")

(defconst dorg-hash-keyword "DORG_HASH"
  "Root-preamble keyword carrying the last-synced Dolt commit hash.")

;;; ─── Backend Struct ───────────────────────────────────────────────────

(cl-defstruct (dorg-backend
               (:constructor dorg-backend--make)
               (:copier nil))
  "A storage backend descriptor for a registered dorg document.
TYPE is a symbol identifying the backend kind (`plain-org' or `dolt').
OPTIONS is a plist of backend-specific configuration (e.g. `:dolt-dir')."
  type
  options)

;;; ─── Constructors ─────────────────────────────────────────────────────

(defun dorg-backend-plain-org ()
  "Return a backend descriptor for the plain-org backend."
  (dorg-backend--make
   :type 'plain-org
   :options nil))

(defun dorg-backend-dolt (dolt-dir &optional root-id)
  "Return a backend descriptor for the Dolt backend backed by DOLT-DIR.
ROOT-ID, when non-nil, is the Dolt-side root heading id and is
stored in the backend options alongside the resolved directory."
  (unless (stringp dolt-dir)
    (error "dorg-backend-dolt: DOLT-DIR must be a string"))
  (dorg-backend--make
   :type 'dolt
   :options (list :dolt-dir
                  (file-name-as-directory (expand-file-name dolt-dir))
                  :root-id root-id)))

;;; ─── Accessors ────────────────────────────────────────────────────────

(defun dorg-backend-option (backend key)
  "Return BACKEND's option for KEY, or nil."
  (plist-get (dorg-backend-options backend) key))

(defun dorg-backend-dolt-dir (backend)
  "Return the Dolt dir option for BACKEND, or nil for non-Dolt backends."
  (when (eq (dorg-backend-type backend) 'dolt)
    (dorg-backend-option backend :dolt-dir)))

(defun dorg-backend-root-id (backend)
  "Return the Dolt root id option for BACKEND, or nil for non-Dolt backends."
  (when (eq (dorg-backend-type backend) 'dolt)
    (dorg-backend-option backend :root-id)))

;;; ─── Predicates ───────────────────────────────────────────────────────

(defun dorg-backend-plain-org-p (backend)
  "Return non-nil when BACKEND is the plain-org backend."
  (and backend (eq (dorg-backend-type backend) 'plain-org)))

(defun dorg-backend-dolt-p (backend)
  "Return non-nil when BACKEND is the Dolt backend."
  (and backend (eq (dorg-backend-type backend) 'dolt)))

;;; ─── Binding-Keyword Write Guard ──────────────────────────────────────

;; The binding preamble writer (`dorg-bind-dolt--insert-keyword') mutates the
;; tracker file resolved from the current buffer/storage context.  A scratch Dolt bind
;; whose context fell through to a live tracker once wrote a stray
;; `#+DORG_DOLT_DIR:'/`#+DORG_HASH:' preamble into the live downstream tracker,
;; auto-binding every command to an alien /tmp Dolt repo.  This
;; opt-in guard lets scratch/benchmark harnesses fail loudly BEFORE such a write
;; instead of silently contaminating a live file.  It is off by default, so
;; production binding behavior is unchanged.

;; KEEP (test-seam): let-bound by scratch/benchmark harnesses to fence writes to an
;; isolated root; nil in production so binding behavior is unchanged.
(defvar dorg--binding-keyword-write-guard nil
  "When non-nil, restrict where binding preamble keywords may be written.
Default nil means unrestricted — production behavior is unchanged.

When set to a directory string, or a list of directory strings, the
`dorg-bind-dolt--insert-keyword' signals an error before mutating a
buffer whose backing file is NOT under one of those directories.
Scratch/benchmark harnesses arm this with their isolated temp root so a
stray write into a live tracker fails loudly rather than silently
contaminating it.  Paths are compared by
`file-truename'.")

(defun dorg--binding-write-allowed-p (file)
  "Return non-nil when writing a binding/hash keyword to FILE is permitted.
Always permitted when `dorg--binding-keyword-write-guard' is nil.  When the
guard is armed, FILE must reside under one of its allowed root directories,
compared by `file-truename'.  A nil FILE (a buffer with no backing file) is
permitted — there is no on-disk tracker to contaminate."
  (let ((roots dorg--binding-keyword-write-guard))
    (or (null roots)
        (null file)
        (let ((truename (file-truename file)))
          (cl-some (lambda (root)
                     (string-prefix-p
                      (file-name-as-directory (file-truename root))
                      truename))
                   (if (listp roots) roots (list roots)))))))

(defun dorg--binding-write-check (file)
  "Signal an error when writing a binding keyword to FILE is disallowed.
No-op unless `dorg--binding-keyword-write-guard' is armed and FILE falls
outside its allowed roots.  See `dorg--binding-write-allowed-p'."
  (unless (dorg--binding-write-allowed-p file)
    (error "dorg: refusing to write binding keyword into %s (outside guard roots %S)"
           file dorg--binding-keyword-write-guard)))

(provide 'dorg-backend)
;;; dorg-backend.el ends here
