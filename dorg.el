;;; dorg.el --- Org-mode files backed by plain org or Dolt -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;; Version: 0.1.0
;; Keywords: outlines, data, org
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Entry point for dorg.
;;
;; dorg is a generic library for backing org-mode files with a
;; storage backend.  Two backends are supported:
;;
;;   plain-org — the org file IS the source of truth.  No Dolt
;;               binary is required.  Suitable for environments
;;               without Dolt and for documents that do not need
;;               versioning or SQL queries.
;;
;;   dolt      — a Dolt (git-for-data) database is the source of
;;               truth and the org file is a materialized view.
;;
;; This file provides the top-level require for the package,
;; the customization group, the version constant, the DORG_ID
;; assignment helper, and a reload helper.
;; Most functionality lives in sub-modules.
;;
;; Public entry points:
;;
;; - `dorg-default-store-directory' — pure canonical project-store path
;; - `dorg-bind-dolt'           — bind a file to a Dolt repo (plain-org is the default)
;; - `dorg-import-document'     — explicit administrative bootstrap/recovery ingest
;; - `dorg-setup'               — bootstrap + bind + exact materialize + projection policy
;; - `dorg-registered-dolt-dir' — look up the dolt dir for a buffer
;; - `dorg-registered-backend'  — look up the backend descriptor for a buffer
;; - `dorg-materialize'         — render the backing store → buffer
;; - `dorg-command-run-query'   — UI-free atomic query (downstream facade)
;; - `dorg-set-property'        — one-property typed mutation + exact refresh
;; - `dorg-assign-id'           — assign `DORG_ID' to headings missing one
;; - `dorg-find-heading-by-id'  — locate a heading by DORG_ID
;; - `dorg-find-headings-by-property' — locate headings by property
;; - `dorg-session-open'        — open a warm read-only `dorg-ql' session
;; - `dorg-history'             — browse a bead/root's Dolt history (DB-level undo)
;; - `dorg-restore-to-commit'   — restore a bead/root to a prior Dolt commit
;; - `dorg-ext-rebuild-index'   — audit or repair one extension index at exact revision
;; - `dorg-vfs-enable'          — install the read-only =/dorg:= file-name handler
;; - `dorg-doctor'              — diagnose and fix DB consistency
;; - `dorg-mode'                — minor mode for dorg-backed buffers
;; - `global-dorg-mode'         — auto-enable dorg-mode on dorg-keyword buffers
;;
;; Extension API (see `dorg-ext.el'):
;;
;; - `dorg-register-extension'  — register extension schema
;; - `dorg-after-materialize-functions' — hook run after render

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'org)

;;; ─── Customization ─────────────────────────────────────────────────

;; defgroup must precede module requires so that defcustom forms in
;; sub-modules can reference :group 'dorg at load time.
(defgroup dorg nil
  "Org-mode files backed by plain org or Dolt."
  :group 'org
  :prefix "dorg-")

;;; ─── Module Requires ───────────────────────────────────────────────

(require 'dorg-error)
(require 'dorg-sexp-file)                 ; zero-dep leaf: atomic single-sexp file I/O
(require 'dorg-id)
(require 'dorg-freshness)
(require 'dorg-sortkey)
(require 'dorg-timestamp)
(require 'dorg-schema)
(require 'dorg-backend)
(require 'dorg-sql-quote)                ; dependency-free SQL quoting leaf (before dolt)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-transport)                ; SQL transport policy (above dolt + dolt-server)
(require 'dorg-ext)
(require 'dorg-ext-read)
(require 'dorg-repo)
(require 'dorg-registry)
(require 'dorg-parse)
(require 'dorg-keywords)
(require 'dorg-model-load)               ; DB→model hydration bridge; self-registers :dolt-dir loader
(require 'dorg-render)
(require 'dorg-ql-core)
(require 'dorg-ql-eval)
(require 'dorg-ql-model)
(require 'dorg-ql-plain)
(require 'dorg-ql-dolt)
(require 'dorg-ql)
(require 'dorg-session)
(require 'dorg-mutate-grammar)           ; pure public grammar inventory
(require 'dorg-mutate)
(require 'dorg-transaction)
(require 'dorg-ext-rebuild)
(require 'dorg-import)
(require 'dorg-find)
(require 'dorg-command)
(require 'dorg-api)
(require 'dorg-doctor)
(require 'dorg-mode)
(require 'dorg-vfs)
(require 'dorg-history)

;;; ─── Version ───────────────────────────────────────────────────────

(defconst dorg-version
  (let* ((library (or load-file-name (locate-library "dorg")))
         (source (and library
                      (if (string-suffix-p ".elc" library)
                          (substring library 0 -1)
                        library))))
    (unless (and source (file-readable-p source))
      (error "dorg: cannot read package source header for version"))
    (lm-version source))
  "Current version of dorg.")

;;; ─── DORG_ID Assignment (plain-org) ────────────────────────────────

(defun dorg-assign-id (&optional buffer)
  "Assign a fresh `DORG_ID' to every heading in BUFFER missing one.
BUFFER defaults to the current buffer.  Existing `DORG_ID'
values are preserved.  Returns the list of newly-assigned
(MARKER . ID) pairs.

This helper is intended for ordinary plain-org buffers.  Administrative
import completes IDs only in its isolated parse model, and exact
materialization publishes them to a canonical Dolt projection."
  (interactive)
  (let ((buf (or buffer (current-buffer)))
        assigned)
    (with-current-buffer buf
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
         (beginning-of-line)
         (unless (org-entry-get (point) dorg-id-property)
           (let ((id (dorg-id-generate))
                 (marker (copy-marker (point))))
             (org-entry-put (point) dorg-id-property (number-to-string id))
             (push (cons marker id) assigned)))
         (end-of-line))))
    (nreverse assigned)))

;;; ─── Module Reload ─────────────────────────────────────────────────

(defconst dorg--reload-priority
  '(;; Foundational defconst/state anchors (loaded first).
    dorg-id
    dorg-sortkey
    dorg-timestamp
    dorg-schema
    dorg-backend
    dorg-projection
    dorg-dolt
    dorg-dolt-server
    dorg-ext
    dorg-ext-read
    dorg-repo
    dorg-registry
    ;; Mid-layer struct/macro definers, in dependency order.  Every
    ;; module that defines a `cl-defstruct'/`defmacro' MUST appear here
    ;; (see `dorg-test-project-reload-priority-covers-struct-macro-definers')
    ;; so it reloads BEFORE any dependent, leaving no stale macro
    ;; expansion behind.
    dorg-model
    dorg-parse
    dorg-git-sync
    dorg-ql-core
    dorg-session
    dorg-mutate
    dorg-api)
  "Foundational `dorg-*' modules to reload first, in dependency order.
A best-effort prefix for the macro/struct-defining layer: reloading an
already-loaded file is order-insensitive for plain defuns (their macros
are already defined), but a module whose `defmacro'/`cl-defstruct' a
dependent expands at load time must reload first.  Completeness is
handled by self-discovery in `dorg--reload-discover' — this seed never
has to list every module, only the foundational/definer prefix.")

(defun dorg--reload-discover (dir)
  "Return loaded `dorg' source files under DIR, foundational modules first.
Self-discovery replacement for a hand-maintained module list: scans DIR
for `dorg*.el' files whose feature is already loaded (`featurep'), so a
renamed or newly-added module is reloaded without editing any list.  The
Clime CLI app `dorg-app' is always dropped — `dorg.el' never `require's
it, so it stays out of an interactive session, and the explicit guard
covers a session that happens to have loaded it.  Members of
`dorg--reload-priority' float to the front in seed order; the remainder
keep discovery (directory) order, with `dorg.el' itself last."
  (let ((loaded (cl-remove-if-not
                 (lambda (f)
                   (let ((feat (intern (file-name-base f))))
                     (and (not (eq feat 'dorg-app))
                          (featurep feat))))
                 (directory-files dir t "\\`dorg.*\\.el\\'"))))
    (sort loaded
          (lambda (a b)
            (< (or (cl-position (intern (file-name-base a))
                                dorg--reload-priority)
                   most-positive-fixnum)
               (or (cl-position (intern (file-name-base b))
                                dorg--reload-priority)
                   most-positive-fixnum))))))

(defun dorg--reload-clear-caches ()
  "Invalidate dorg's derived runtime caches so reloaded code starts fresh.

CLEAR (derived, safely re-computed on next access):
  - `dorg--registry' — file->backend registrations.  Every entry is
    re-derivable from the `#+DORG_DOLT_DIR' buffer keyword, and clearing
    forces a re-bind that re-runs `dorg-repo-ensure-schema' — the lever
    that heals an in-session schema-version bump (e.g. v3 -> v4).
  - `dorg-dolt--version-cache' — cached `dolt version' probe.
  - `dorg-dolt-server--attached-ports' — cached ports attached from
    foreign or detached server claims; re-discovered on next access.

PRESERVE (NOT touched — live resources / registrations whose clearing
would orphan processes or drop externally-registered state): open mysql
connections (`dorg-sql--connections'), owned sql-server processes and claim
locks (`dorg-dolt-server--procs'/`dorg-dolt-server--owned-claims'), the
extension registry (`dorg--extensions',
which reloading core modules does not re-populate), and the snowflake-id
monotonic counters (`dorg-id--last-ms'/`dorg-id--last-seq').

NO-OP (audit: nothing to clear): there is no HEAD/HASHOF cache —
`dorg-head-commit' probes fresh every call; no config-values cache —
config is read from buffer keywords on each access; and the
`dorg-parse--*' defvars are per-parse dynamically-bound accumulators,
not a memo."
  (setq dorg--registry nil)
  (dorg-dolt-reset-version-cache)
  (clrhash dorg-dolt-server--attached-ports))

;;;###autoload
(defun dorg-reload ()
  "Force-reload every loaded dorg module and invalidate caches.

Self-healing: discovers all `dorg' source files next to this one and
re-`load's each whose feature is already present (`featurep'), so a
renamed or newly-added definition is picked up without restarting Emacs.
This replaces a hand-maintained module list, which silently went stale
and left renamed definitions void after refactors.  Only loaded modules
are reloaded, so the CLI layer (`dorg-app') is never dragged into an
interactive session.

The `.el' sources are loaded directly (never a stale `.elc') from the
directory containing this file, not via `locate-library', to avoid
loading stale copies from other `load-path' entries.  All derived
runtime caches are cleared afterwards (see `dorg--reload-clear-caches');
a schema-version bump is healed in-session via the forced re-bind."
  (interactive)
  (let* ((dir (file-name-directory (or load-file-name
                                       (locate-library "dorg")
                                       buffer-file-name
                                       default-directory)))
         (files (dorg--reload-discover dir)))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))
    ;; Load the .el paths directly (NOSUFFIX=nil NOMESSAGE=t MUST-SUFFIX=t)
    ;; so source edits take effect, never a stale .elc.
    (dolist (file files)
      (load file nil t t))
    (dorg--reload-clear-caches)
    ;; Reinstall identity guards for already-open Org buffers.  Reloading a
    ;; minor-mode definition can leave an existing buffer with `dorg-mode' disabled;
    ;; projection authority therefore follows storage identity, not that bit.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (dorg--install-projection-policy))))
    (message "dorg-reload: %d modules reloaded, caches cleared"
             (length files))))

(provide 'dorg)
;;; dorg.el ends here
