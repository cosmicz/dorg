;;; dorg-registry.el --- File registry for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Registry mapping org files to their backing storage.  Holds the
;; `dorg-registration' struct, the `dorg--registry' alist, and the
;; `dorg-bind-dolt' entry point.
;;
;; This module is split out from `dorg.el' so lower-level query, mutation,
;; materialization, and import adapters can look up a registration without
;; requiring the top-level feature.  Keeping registration here avoids a
;; circular dependency.
;;
;; Source classification is PURE and separate from effectful binding.
;; The registry stores ONLY explicit Dolt bindings created via
;; `dorg-bind-dolt' (a Dolt repository is the source of truth and the
;; file is a materialized view).  Reading a buffer's source is
;; `dorg--classify-registration', which never initializes, migrates,
;; allocates a root, or writes the registry; it classifies a buffer to:
;;   1. its explicit stored Dolt binding, if one exists; else
;;   2. a TRANSIENT Dolt registration with a NIL root_id when the
;;      buffer carries a `#+DORG_DOLT_DIR:' file keyword — the source
;;      is Dolt-backed, but no binding is created; else
;;   3. a transient plain-org registration (the file is the source of
;;      truth).
;; Operations that need a concrete backing store (a real root_id) call
;; `dorg--ensure-registration', the sole implicit path that may bind a
;; keyword buffer via `dorg-bind-dolt'.  `dorg-bind-dolt' remains the
;; sole explicit, user-facing binding entry point.

;;; Code:

(require 'cl-lib)
(require 'dorg-backend)
(require 'dorg-projection)
;; B8/D6: `dorg-repo' is the Dolt binding backend — it owns ALL strict-bind
;; DDL (repo init, schema migration, root lookup/create) behind the single
;; `dorg-repo-ensure-root' entry and pulls `dorg-dolt' transitively.  It is
;; loaded ONLY on the Dolt path: a plain-org file needs no binding
;; (`dorg--classify-registration' returns a transient plain-org
;; registration without touching it), so it is required lazily inside
;; `dorg-bind-dolt' (the sole strict Dolt-binding entry).
;; `dorg-repo-ensure-root' is DECLARED (not required) so neither an
;; interpreted nor a compiled load pulls the Dolt
;; stack.  `dorg-repo' sorts after `dorg-registry' in the `*.el' lint batch,
;; so the stub is needed for a warning-clean compile.
(declare-function dorg-repo-ensure-root "dorg-repo")

(autoload 'dorg-mode "dorg-mode" nil t)

;;; Struct and State

(cl-defstruct dorg-registration
  "A registration binding an org file to a storage backend.
BACKEND is a `dorg-backend' struct identifying the storage kind.
Dolt metadata (the resolved directory and root heading id) lives
in the backend's options plist, not on the registration.
FILE is the absolute path of the registered org file.

IDENTITY is the binding's normalized identity POLICY — `dorg', (:property K),
or `none' — or nil to derive the backend default (=> `dorg').  It is read
through the single resolver `dorg-mutate--source-identity'.  Fidelity is
backend-derived (plain-org edits are source-preserving), not a registration
slot."
  backend
  file
  identity)

;; KEEP (mutable-state): binding registry, setq/push-mutated; must persist across calls.
(defvar dorg--registry nil
  "Alist of (ABS-ORG-FILE . `dorg-registration') mappings.
Holds ONLY explicit Dolt bindings created by `dorg-bind-dolt'.
A buffer with no entry is classified transiently (never stored) by
`dorg--classify-registration': a `#+DORG_DOLT_DIR:' keyword buffer to
a Dolt registration with a nil root_id, any other file to plain-org.")

;;; Shared Source Opening

(defun dorg--find-file-noselect (file)
  "Visit FILE without an unanswerable large-file prompt in batch Emacs.
`find-file-noselect' normally consults `large-file-warning-threshold' and
asks for confirmation above it.  A noninteractive DOrg caller cannot answer
that prompt, so disable only that threshold for the dynamic extent of this
open.  Interactive callers retain their current threshold and normal Emacs
confirmation behavior."
  (let ((large-file-warning-threshold
         (if noninteractive nil large-file-warning-threshold)))
    (find-file-noselect file)))

;;; Internal Helpers — Registry Storage

(defun dorg--store-registration (abs-file registration)
  "Insert REGISTRATION into `dorg--registry' at ABS-FILE, replacing any prior."
  (let ((existing (assoc abs-file dorg--registry)))
    (if existing
        (setcdr existing registration)
      (push (cons abs-file registration) dorg--registry))
    registration))

;;; Public API

(defun dorg-bind-dolt (file dolt-dir)
  "Bind FILE to the Dolt repository at DOLT-DIR.
The strict-bind DDL — initializing the repo if absent, ensuring the
schema, and reusing or allocating FILE's root — is owned by the
`dorg-repo' backend layer and resolved here through the single
`dorg-repo-ensure-root' entry.  This function only canonicalizes the
paths, records the resulting binding, and (interactively) updates the UI.

Returns a `dorg-registration' struct whose backend carries the
resolved Dolt directory and root id.  The mapping is stored in
`dorg--registry' keyed by the absolute path of FILE.

Plain-org needs no binding: a file with no Dolt binding is
plain-org by default (see `dorg--classify-registration').

When called interactively, prompts for a Dolt directory, uses
the current buffer's file, inserts a `#+DORG_DOLT_DIR:' keyword
if missing, and enables `dorg-mode'."
  (interactive
   (let ((file (or (buffer-file-name)
                   (error "dorg: buffer has no file")))
         (dir (read-directory-name "Dolt directory: "
                                   (file-name-directory
                                    (or (buffer-file-name) default-directory)))))
     (list file dir)))
  ;; B8/D6 Dolt boundary: strict binding runs schema migration + root
  ;; lookup/create against the Dolt repo — all owned by the `dorg-repo'
  ;; backend layer behind `dorg-repo-ensure-root'.  Load it here (the
  ;; plain-org path never reaches this); it pulls `dorg-dolt' transitively.
  (require 'dorg-repo)
  ;; Correct letter-case to the real on-disk spelling (tracked-regression):
  ;; `file-truename' preserves the caller's case on a case-insensitive volume,
  ;; so a bind reached through a case-alias would persist that alias into
  ;; `dorg_files.relative_path' / the registry and break on another host.
  (let* ((abs-file (dorg--canonical-existing-case (file-truename file)))
         (abs-dir (file-name-as-directory
                   (dorg--canonical-existing-case (file-truename dolt-dir))))
         ;; Delegate ALL strict-bind DDL (init, schema, root lookup/create)
         ;; to the backend; the registry only records the resulting binding.
         (root-id (dorg-repo-ensure-root abs-dir abs-file))
         (reg (dorg--store-registration
               abs-file
               (make-dorg-registration
                :backend (dorg-backend-dolt abs-dir root-id)
                :file abs-file))))
    (when (called-interactively-p 'any)
      (dorg-bind-dolt--insert-keyword abs-dir)
      (unless (bound-and-true-p dorg-mode)
        (dorg-mode 1))
      (message "dorg: bound %s" (file-name-nondirectory abs-file)))
    reg))

(defun dorg-bind-dolt--insert-keyword (dolt-dir)
  "Insert a `#+DORG_DOLT_DIR:' keyword at buffer top if absent.
DOLT-DIR is the absolute directory path.  The keyword value is
made relative to the buffer's file directory when possible.

The \"already present\" predicate checks specifically for a GENUINE
preamble `#+DORG_DOLT_DIR:' keyword element (`dorg--preamble-keyword-
present-p'), NOT a whole-buffer regexp: a `#+DORG_DOLT_DIR:' literal in a
heading body, in prose, or inside an example/source block does NOT
suppress insertion.  It is also DORG_DOLT_DIR-specific (not the
soft-migration reader), so a buffer carrying only a legacy
`#+DORG_DIR:' preamble keyword still gets a `#+DORG_DOLT_DIR:' line
written and thereby migrates forward.  The writer is insert-only at
buffer top, so body literals are left untouched, and a second call is a
no-op once the genuine preamble keyword exists (idempotent)."
  (let* ((file-dir (and (buffer-file-name)
                        (file-name-directory
                         (file-truename (buffer-file-name)))))
         (relative (if file-dir
                       (file-relative-name dolt-dir file-dir)
                     dolt-dir)))
    (dorg-projection-call-with-programmatic-changes
     (lambda ()
       (save-excursion
         (save-restriction
           (widen)
           (unless (dorg--preamble-keyword-present-p dorg-dolt-dir-keyword)
             (dorg--binding-write-check (buffer-file-name))
             (goto-char (point-min))
             (insert (format "#+DORG_DOLT_DIR: %s\n" relative)))))))))

(defun dorg-unregister (org-file)
  "Remove ORG-FILE from the dorg registry.
Does not touch the backing storage on disk."
  (let ((abs-file (file-truename org-file)))
    (setq dorg--registry
          (assoc-delete-all abs-file dorg--registry))))

(defun dorg--preamble-keyword-alist (keys &optional buffer)
  "Return an alist of (KEY . VALUE) for genuine preamble keyword elements.
KEYS is a list of upper-case keyword names to look for.  Scans ONLY the
buffer preamble — the region before the first headline — and accepts only
true `keyword' elements, so an occurrence in a heading body, in prose, or
inside an example/source block is excluded.  The first value seen per key
wins; values are untrimmed.  In a non-Org buffer the element check is
skipped and the preamble bound alone applies.

`org-collect-keywords' cannot be used here: it scans the whole buffer
\(collecting body-level keywords) and ignores narrowing, so a preamble
restriction layered on top of it would be vacuous.  Shared by the reader
\(`dorg--preamble-dolt-keyword') and the writer presence check
\(`dorg--preamble-keyword-present-p')."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (let ((limit (save-excursion
                       (goto-char (point-min))
                       (if (re-search-forward org-heading-regexp nil t)
                           (match-beginning 0)
                         (point-max))))
              (re (concat "^[ \t]*#\\+\\("
                          (mapconcat #'regexp-quote keys "\\|")
                          "\\):[ \t]*\\(.*\\)$"))
              (found nil))
          (goto-char (point-min))
          (while (re-search-forward re limit t)
            ;; `org-element-at-point' clobbers the match data, so capture
            ;; the key and value BEFORE calling it.  The element check
            ;; (which excludes keywords inside example/source blocks)
            ;; needs an Org buffer; on a non-Org buffer the preamble bound
            ;; alone is the guard.
            (let ((key (upcase (match-string 1)))
                  (val (match-string 2)))
              (when (and (or (not (derived-mode-p 'org-mode))
                             (eq (org-element-type (org-element-at-point))
                                 'keyword))
                         (not (assoc key found)))
                (push (cons key val) found))))
          (nreverse found))))))

(defun dorg--preamble-keyword-present-p (key &optional buffer)
  "Return non-nil iff a genuine preamble keyword element named KEY exists.
KEY is matched case-insensitively.  Unlike a whole-buffer regexp scan,
this ignores a `#+KEY:' line in a heading body, in prose, or inside an
example/source block, so a documentation literal never counts as the
file's keyword.  The write-side mirror of `dorg--preamble-dolt-keyword'."
  (and (assoc (upcase key)
              (dorg--preamble-keyword-alist (list (upcase key)) buffer))
       t))

(defconst dorg--dolt-dir-marker-keywords (list dorg-dolt-dir-keyword "DORG_DIR")
  "Root-preamble keyword names that mark a root as Dolt-backed.
`DORG_DOLT_DIR' is the current spelling; the second entry is the legacy
spelling retained for the soft-migration window.  This is the single
sanctioned home for the legacy literal in production sources (see
`dorg-test-dorg-dir-literal-confined-to-reader'); other modules — e.g.
`dorg-git-sync' — must consume this constant rather than relist the
legacy name.  Ordering encodes preference: current spelling first.")

(defun dorg--preamble-dolt-keyword (&optional buffer)
  "Return the raw Dolt-dir keyword value from BUFFER's file preamble, or nil.
Scans ONLY the preamble — the region before the first headline — and
accepts only genuine `keyword' elements.  This deliberately excludes a
`#+DORG_DOLT_DIR:'/`#+DORG_DIR:' line that appears in a heading body or
prose (a documentation mention must never become the file's binding) as
well as occurrences inside example or source blocks.  Prefers
`DORG_DOLT_DIR' over the legacy `DORG_DIR'; returns the first value seen
per key, untrimmed, or nil when neither is present in the preamble."
  (let ((alist (dorg--preamble-keyword-alist dorg--dolt-dir-marker-keywords
                                             buffer)))
    (or (cdr (assoc dorg-dolt-dir-keyword alist))
        (cdr (assoc "DORG_DIR" alist)))))

(defun dorg--buffer-dolt-dir (&optional buffer)
  "Return the Dolt directory declared by BUFFER's file keyword, or nil.
Prefers the `#+DORG_DOLT_DIR:' keyword and falls back to the
legacy `#+DORG_DIR:' keyword during the soft-migration window.
This reader fallback is the sole sanctioned use of the legacy
keyword name in production sources.

Only a true file-preamble keyword (before the first headline) is
honored — see `dorg--preamble-dolt-keyword'.  A keyword in a heading
body, in prose, or inside an example/source block is ignored, so
documentation that mentions `#+DORG_DOLT_DIR:' cannot flip the file to a
bogus Dolt binding.  The path is resolved relative to the buffer's file
when it is not absolute; leading/trailing whitespace is stripped."
  (let* ((buf (or buffer (current-buffer)))
         (file (buffer-file-name buf))
         (raw (when-let ((value (dorg--preamble-dolt-keyword buf)))
                (string-trim value))))
    (when (and raw (not (string-empty-p raw)))
      (if (and file (not (file-name-absolute-p raw)))
          (expand-file-name raw (file-name-directory file))
        (expand-file-name raw)))))

(defun dorg--classify-registration (&optional buffer)
  "Return the `dorg-registration' for BUFFER's file by PURE classification.
This is a read-only SOURCE classifier: it never initializes a repo,
migrates a schema, allocates or looks up a root row, runs any SQL, or
writes to `dorg--registry'.  Resolution order:
  1. an explicit Dolt binding already stored in `dorg--registry';
  2. a TRANSIENT, un-stored Dolt registration carrying the
     `#+DORG_DOLT_DIR:' (or legacy) keyword's directory and a NIL
     root_id — the source is Dolt-backed, but no binding is created;
  3. a transient, non-stored plain-org registration for any other
     file-backed buffer.
Returns nil only when BUFFER has no backing file.

Callers that need a concrete backing store (a real Dolt root_id) must use
`dorg--ensure-registration' instead — this classifier deliberately leaves
the keyword-Dolt root_id nil so a read path can never bind."
  (let* ((buf (or buffer (current-buffer)))
         (file (buffer-file-name buf)))
    (when file
      (or (cdr (assoc (file-truename file) dorg--registry))
          (when-let ((dolt-dir (dorg--buffer-dolt-dir buf)))
            (make-dorg-registration
             :backend (dorg-backend-dolt
                       (file-name-as-directory (file-truename dolt-dir))
                       nil)
             :file (file-truename file)))
          (make-dorg-registration
           :backend (dorg-backend-plain-org)
           :file (file-truename file))))))

(defun dorg--ensure-registration (&optional buffer)
  "Return the `dorg-registration' for BUFFER's file, BINDING strictly if needed.
This is the effectful counterpart of `dorg--classify-registration', for
operation callers (materialize, sync, query, session, mutate, conflict
reload, history) that need a concrete backing store.  Resolution order:
  1. an explicit Dolt binding already stored in `dorg--registry';
  2. a keyword-declared Dolt buffer is bound STRICTLY via `dorg-bind-dolt'
     (schema migration + root lookup/create), storing the binding and
     returning a registration with a real root_id;
  3. a transient, non-stored plain-org registration for any other
     file-backed buffer.
Returns nil only when BUFFER has no backing file.

This is the ONLY registry path that may initialize/migrate/create,
because it is reached only from operations that need storage-backed
semantics.  Passive/read paths must use `dorg--classify-registration'."
  (let* ((buf (or buffer (current-buffer)))
         (file (buffer-file-name buf)))
    (when file
      (or (cdr (assoc (file-truename file) dorg--registry))
          (when-let ((dolt-dir (dorg--buffer-dolt-dir buf)))
            (dorg-bind-dolt file dolt-dir))
          (make-dorg-registration
           :backend (dorg-backend-plain-org)
           :file (file-truename file))))))

(defun dorg-current-registration (&optional buffer)
  "Return the full `dorg-registration' for BUFFER's file, or nil.
BUFFER defaults to the current buffer.  This is the public,
read-only accessor for the complete registration record (its
backend descriptor and registered file), for callers that need more than
`dorg-registered-backend' or `dorg-registered-dolt-dir' expose
individually.

Resolution is PURE source classification (`dorg--classify-registration'):
  1. an explicit Dolt binding stored in `dorg--registry';
  2. a transient, un-stored Dolt registration carrying a
     `#+DORG_DOLT_DIR:' (or legacy) keyword's directory and a NIL
     root_id — the source is classified as Dolt-backed but no binding
     is created;
  3. a transient, non-stored plain-org registration for any other
     file-backed buffer.
Returns nil only when BUFFER has no backing file.

This accessor performs NO mutation: it never initializes a repo,
migrates schema, allocates a root, or stores a binding.  To create a
binding use `dorg-bind-dolt' (the sole public binding entry point).
Treat the returned struct as read-only."
  (dorg--classify-registration buffer))

(defun dorg-registered-dolt-dir (&optional buffer)
  "Return the Dolt dir registered for BUFFER's file, or nil.
BUFFER defaults to the current buffer.  Returns nil if the buffer
has no backing file or its registration uses the plain-org backend."
  (when-let ((reg (dorg--classify-registration buffer)))
    (dorg-backend-dolt-dir (dorg-registration-backend reg))))

(defun dorg-registered-backend (&optional buffer)
  "Return the `dorg-backend' registered for BUFFER's file, or nil.
BUFFER defaults to the current buffer.  Unbound file-backed
buffers report a fresh plain-org backend descriptor."
  (when-let ((reg (dorg--classify-registration buffer)))
    (dorg-registration-backend reg)))

(provide 'dorg-registry)
;;; dorg-registry.el ends here
