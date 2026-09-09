;;; dorg-vfs.el --- Read-only /dorg: file-name-handler (Phase 1 spike) -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Phase 1 of the VFS spike: an
;; ADDITIVE, read-only fileless addressing layer over the existing render
;; engine.  It is NOT a replacement for `#+DORG_DOLT_DIR' / `dorg--registry'
;; / `dorg-bind-dolt'.
;;
;; Register a magic file-name handler so `find-file' on a /dorg: path
;; materializes a Dolt tree (or island) into an org buffer, read-only:
;;
;;   /dorg:<DOLT-DIR>[@<REF>]:/<NODE-ID>
;;
;; mirroring remoto.el's `/github:OWNER/REPO@REF:/PATH'.  Dispatch:
;;   - empty NODE-ID   -> `dorg-render' (whole tree)
;;   - integer NODE-ID -> `dorg-render-island' (island; honors as-of=REF)
;;
;; Spike preconditions (signalled, relaxed in later phases): DOLT-DIR is
;; absolute and free of ":" and "@"; NODE-ID is an integer heading id or
;; empty.  Whole-tree @REF is an engine gap (`dorg-render' takes no as-of),
;; so it is SIGNALLED rather than silently ignored until the follow-up lands.
;;
;; Coexistence: the "\\`/dorg:" regexp is anchored, so no real on-disk path
;; can match — plain-org files and `#+DORG_DOLT_DIR'-keyword files never
;; enter the handler (the layer is purely additive, regression-guarded).
;;
;; Freshness: file-attributes modtime is the commit DATE of the resolved ref
;; (REF if given, else HEAD) via `dolt_log', so a re-visit after a new commit
;; reports externally-changed while a @REF-pinned island stays stable.
;;
;; Non-goals (explicit follow-ups): write-back
;; (Phase 2), dired/directory-files, find-file node-id completion, whole-tree
;; @REF.  This module makes NO edits to registry/sync/mode; it consumes only
;; public `dorg-render' / `dorg-render-island' / `dorg-head-commit' / `dorg-sql'.

;;; Code:

(require 'cl-lib)
(require 'dorg-render)
(require 'dorg-dolt)
(require 'dorg-projection)

(define-error 'dorg-vfs-error "Malformed or unsupported /dorg: path")

;; `insert-file-contents' sets the island no-push record BEFORE `find-file'
;; selects the major mode, whose `kill-all-local-variables' would otherwise
;; wipe it.  The projection-view context is permanent-local in
;; `dorg-projection', so it survives the mode switch.

(defconst dorg-vfs--prefix "/dorg:"
  "Anchored magic prefix for the read-only dorg VFS.")

;;; ─── Path grammar ───────────────────────────────────────────────────

(defun dorg-vfs--parse (name)
  "Parse a /dorg: magic NAME into (:dolt-dir D :ref R :node-id N).
Returns nil when NAME lacks the `dorg-vfs--prefix'.  Signals `dorg-vfs-error'
on a precondition violation: a non-absolute DOLT-DIR, a \":\"-bearing
DOLT-DIR (detected as a localname not starting with \"/\"), an \"@\"-bearing
DOLT-DIR, or a non-integer NODE-ID.  :node-id is nil for the whole tree, else
a positive integer; :ref is nil or the ref string."
  (when (string-prefix-p dorg-vfs--prefix name)
    (let* ((rest (substring name (length dorg-vfs--prefix)))
           (colon (string-search ":" rest)))
      (unless colon
        (signal 'dorg-vfs-error
                (list (format "no host/localname separator in %S" name))))
      (let ((host-spec (substring rest 0 colon))
            (localname (substring rest (1+ colon))))
        ;; localname is "/<NODE-ID>", or "" when `find-file' stripped the
        ;; whole-tree trailing slash (".../db:/" -> ".../db:").  Anything else
        ;; non-empty-not-"/" means the first ":" fell inside a (precondition-
        ;; violating) DOLT-DIR.
        (unless (or (string-empty-p localname) (string-prefix-p "/" localname))
          (signal 'dorg-vfs-error
                  (list (format "DOLT-DIR must be colon-free (bad localname in %S)"
                                name))))
        (let* ((at (cl-position ?@ host-spec :from-end t))
               (dolt-dir (if at (substring host-spec 0 at) host-spec))
               (ref (and at (substring host-spec (1+ at))))
               (node-str (if (string-empty-p localname) "" (substring localname 1))))
          (unless (string-prefix-p "/" dolt-dir)
            (signal 'dorg-vfs-error
                    (list (format "DOLT-DIR must be absolute: %S" dolt-dir))))
          (when (string-search "@" dolt-dir)
            (signal 'dorg-vfs-error
                    (list (format "DOLT-DIR must be @-free: %S" dolt-dir))))
          (when (and ref (string-empty-p ref))
            (signal 'dorg-vfs-error (list (format "empty REF in %S" name))))
          (let ((node-id
                 (cond
                  ((string-empty-p node-str) nil)
                  ((string-match-p "\\`[0-9]+\\'" node-str)
                   (string-to-number node-str))
                  (t (signal 'dorg-vfs-error
                             (list (format "NODE-ID must be an integer or empty: %S"
                                           node-str)))))))
            (list :dolt-dir dolt-dir :ref ref :node-id node-id)))))))

(defun dorg-vfs-unparse (plist)
  "Return the canonical /dorg: string for PLIST (:dolt-dir :ref :node-id)."
  (format "%s%s%s:/%s"
          dorg-vfs--prefix
          (plist-get plist :dolt-dir)
          (if (plist-get plist :ref) (concat "@" (plist-get plist :ref)) "")
          (or (plist-get plist :node-id) "")))

(defun dorg-vfs--connection (plist)
  "Return the connection prefix \"/dorg:DOLT-DIR[@REF]:\" for PLIST."
  (format "%s%s%s:"
          dorg-vfs--prefix
          (plist-get plist :dolt-dir)
          (if (plist-get plist :ref) (concat "@" (plist-get plist :ref)) "")))

;;; ─── Freshness (synthetic modtime from the resolved ref's commit) ────

(defun dorg-vfs--head-time (dolt-dir &optional ref)
  "Return the Emacs time of REF's commit (or HEAD) in DOLT-DIR.
Stable per resolved-ref commit and advances when that commit changes, so
`verify-visited-file-modtime' reports externally-changed exactly when the
resolved ref moved.  A @REF-pinned view therefore stays clean across HEAD
advances (its content is immutable)."
  (let* ((refish (or ref "HEAD"))
         ;; TIMESTAMPDIFF against the naive epoch reads the (UTC) commit date
         ;; without a session-time-zone reinterpretation — unlike
         ;; UNIX_TIMESTAMP, which would shift the result by the session offset.
         (rows (dorg-sql
                dolt-dir
                (format (concat "SELECT TIMESTAMPDIFF(SECOND, '1970-01-01 00:00:00', date)"
                                " AS ts FROM dolt_log(%s) LIMIT 1")
                        (dorg-sql-quote refish))))
         (ts (alist-get 'ts (car rows))))
    (unless ts
      (error "dorg-vfs: cannot read commit time for %s in %s" refish dolt-dir))
    (seconds-to-time (if (stringp ts) (string-to-number ts) ts))))

;;; ─── Content (render dispatch) ──────────────────────────────────────

(defun dorg-vfs--assert-tree-no-ref (plist)
  "Signal when PLIST asks for a whole-tree render pinned to a REF.
`dorg-render' takes no as-of, so whole-tree @REF is unsupported in Phase 1;
signal rather than silently ignore the REF (a deliberate follow-up boundary)."
  (when (and (null (plist-get plist :node-id)) (plist-get plist :ref))
    (signal 'dorg-vfs-error
            (list "whole-tree @REF is not supported in Phase 1 (engine gap); use an island NODE-ID or drop @REF"))))

(defun dorg-vfs--render (plist)
  "Return the org text for PLIST: whole tree or island (as-of REF)."
  (dorg-vfs--assert-tree-no-ref plist)
  (let ((dolt-dir (plist-get plist :dolt-dir))
        (node-id (plist-get plist :node-id)))
    (if node-id
        (dorg-render-island dolt-dir node-id nil (plist-get plist :ref))
      (dorg-render dolt-dir))))

(defun dorg-vfs--node-exists-p (plist)
  "Return non-nil when PLIST names a present node.
Whole tree -> the repo has a resolvable unique root; island -> NODE-ID
resolves to a root under as-of REF."
  (condition-case nil
      (progn
        (dorg-vfs--assert-tree-no-ref plist)
        (let ((dolt-dir (plist-get plist :dolt-dir))
              (node-id (plist-get plist :node-id)))
          (and (if node-id
                   (dorg-render--node-root-id dolt-dir node-id (plist-get plist :ref))
                 (dorg-render--resolve-root-id dolt-dir))
               t)))
    (error nil)))

;;; ─── Handler operations (read-only minimal set) ─────────────────────

(defun dorg-vfs--op-expand-file-name (name &optional _default-directory)
  "A /dorg: NAME is already canonical; return it unchanged."
  name)

(defun dorg-vfs--op-file-name-directory (name)
  "Return the connection directory \"/dorg:DOLT-DIR[@REF]:/\" for NAME."
  (concat (dorg-vfs--connection (dorg-vfs--parse name)) "/"))

(defun dorg-vfs--op-file-name-nondirectory (name)
  "Return NAME's NODE-ID (\"\" for the whole tree)."
  (let ((n (plist-get (dorg-vfs--parse name) :node-id)))
    (if n (number-to-string n) "")))

(defun dorg-vfs--op-file-exists-p (name)
  "Return non-nil when NAME's node is present."
  (ignore-errors (dorg-vfs--node-exists-p (dorg-vfs--parse name))))

(defun dorg-vfs--op-file-writable-p (_name)
  "Read-only spike: never writable."
  nil)

(defun dorg-vfs--op-file-truename (name)
  "A /dorg: NAME is already canonical; return it unchanged."
  name)

(defun dorg-vfs--op-file-remote-p (name &optional identification _connected)
  "Mark NAME remote so Emacs skips lockfiles, backups, and VC.
With IDENTIFICATION nil return the connection prefix; otherwise return the
requested component (method/host/localname)."
  (let ((p (dorg-vfs--parse name)))
    (pcase identification
      ('method "dorg")
      ('host (plist-get p :dolt-dir))
      ('user nil)
      ('localname (format "/%s" (or (plist-get p :node-id) "")))
      (_ (dorg-vfs--connection p)))))

(defun dorg-vfs--op-file-attributes (name &optional _id-format)
  "Return synthetic, read-only file attributes for NAME.
Modtime is the resolved ref's commit time; size is lazy (-1)."
  (let* ((p (dorg-vfs--parse name))
         (mt (dorg-vfs--head-time (plist-get p :dolt-dir) (plist-get p :ref))))
    (list nil 1 0 0 mt mt mt -1 "-r--r--r--" nil 0 0)))

(defun dorg-vfs--op-insert-file-contents (name &optional visit beg end replace)
  "Render NAME's org text and insert it; honor VISIT/REPLACE.
On VISIT record the resolved-ref modtime (so freshness works) and, for an
island, install the private partial materialized-view record.  BEG/END are not
supported in the spike (whole render only); a non-nil BEG or END signals."
  (when (or beg end)
    (signal 'dorg-vfs-error (list "partial (BEG/END) reads are not supported in Phase 1")))
  (let* ((p (dorg-vfs--parse name))
         (text (dorg-vfs--render p)))
    (when replace
      (delete-region (point-min) (point-max)))
    (let ((start (point)))
      (insert text)
      (let ((nchars (- (point) start)))
        (when visit
          (setq buffer-file-name name)
          (set-buffer-modified-p nil)
          (set-visited-file-modtime
           (dorg-vfs--head-time (plist-get p :dolt-dir) (plist-get p :ref)))
          (when (plist-get p :node-id)
            (dorg-projection-install-view
             (current-buffer) (current-buffer) nil (plist-get p :ref)
             'partial (list :island (plist-get p :node-id)))))
        (cons name nchars)))))

(defun dorg-vfs--op-verify-visited-file-modtime (&optional buffer)
  "Return t when BUFFER's recorded modtime still matches the resolved ref.
Must be specialized: the generic op would inhibit this handler and stat a
nonexistent real file, always reporting a spurious change."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer (if (bufferp buf) buf (current-buffer))
      (or (null buffer-file-name)
          (let* ((p (dorg-vfs--parse buffer-file-name))
                 (stored (visited-file-modtime)))
            (or (and (consp stored) (eql (car stored) 0))   ; 0 => unset => clean
                (time-equal-p stored
                              (dorg-vfs--head-time (plist-get p :dolt-dir)
                                                   (plist-get p :ref)))))))))

(defun dorg-vfs--op-write-region (&rest _args)
  "Read-only spike: writing through the VFS is refused."
  (signal 'dorg-vfs-error (list "dorg VFS is read-only (Phase 1)")))

(defun dorg-vfs--op-vc-registered (_name)
  "Suppress VC scanning of a /dorg: path."
  nil)

(defun dorg-vfs--op-make-auto-save-file-name (&rest _args)
  "Suppress auto-save against the store."
  nil)

(defconst dorg-vfs--ops
  (list (cons 'expand-file-name        #'dorg-vfs--op-expand-file-name)
        (cons 'file-name-directory     #'dorg-vfs--op-file-name-directory)
        (cons 'file-name-nondirectory  #'dorg-vfs--op-file-name-nondirectory)
        (cons 'file-exists-p           #'dorg-vfs--op-file-exists-p)
        (cons 'file-readable-p         #'dorg-vfs--op-file-exists-p)
        (cons 'file-writable-p         #'dorg-vfs--op-file-writable-p)
        (cons 'file-truename           #'dorg-vfs--op-file-truename)
        (cons 'file-remote-p           #'dorg-vfs--op-file-remote-p)
        (cons 'file-attributes         #'dorg-vfs--op-file-attributes)
        (cons 'insert-file-contents    #'dorg-vfs--op-insert-file-contents)
        (cons 'verify-visited-file-modtime #'dorg-vfs--op-verify-visited-file-modtime)
        (cons 'write-region            #'dorg-vfs--op-write-region)
        (cons 'vc-registered           #'dorg-vfs--op-vc-registered)
        (cons 'make-auto-save-file-name #'dorg-vfs--op-make-auto-save-file-name))
  "Alist of OPERATION -> specialized handler function for the read-only VFS.")

;;; ─── Handler + delegation shim ──────────────────────────────────────

(defun dorg-vfs--real (operation args)
  "Run OPERATION on ARGS with the real implementation (canonical shim).
Inhibits `dorg-vfs-handler' for this one call so an unspecialized op never
recurses or crashes — it runs against the literal string."
  (let ((inhibit-file-name-handlers
         (cons 'dorg-vfs-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;;;###autoload
(defun dorg-vfs-handler (operation &rest args)
  "Magic file-name handler for read-only /dorg: paths.
Dispatches OPERATION via `dorg-vfs--ops'; anything else delegates to the
real implementation via `dorg-vfs--real'."
  (let ((fn (cdr (assq operation dorg-vfs--ops))))
    (if fn
        (apply fn args)
      (dorg-vfs--real operation args))))

;;; ─── Registration ───────────────────────────────────────────────────

(defconst dorg-vfs--alist-entry (cons "\\`/dorg:" 'dorg-vfs-handler)
  "The `file-name-handler-alist' entry installed by `dorg-vfs-enable'.")

;;;###autoload
(defun dorg-vfs-enable ()
  "Install the read-only /dorg: file-name handler.
Idempotent.  Explicit (not auto-enabled on require) so the spike adds no
global side effect until asked."
  (interactive)
  (add-to-list 'file-name-handler-alist dorg-vfs--alist-entry))

(defun dorg-vfs-disable ()
  "Remove the /dorg: file-name handler."
  (interactive)
  (setq file-name-handler-alist
        (delete dorg-vfs--alist-entry file-name-handler-alist)))

(provide 'dorg-vfs)
;;; dorg-vfs.el ends here
