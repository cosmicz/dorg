;;; dorg-mode.el --- Minor mode for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; User-facing minor mode for dorg-backed org buffers.  Provides
;; `dorg-mode', `dorg-materialize', `dorg-refresh-materialized-view',
;; `dorg-setup', and `global-dorg-mode'.  Dolt-backed buffers are
;; read-only projections; semantic writes go through typed commands.
;; Split from `dorg.el' to keep the top-level entry point minimal.

;;; Code:

(require 'dorg-id)
(require 'dorg-backend)
(require 'dorg-registry)
(require 'dorg-render)
(require 'dorg-projection)
(require 'dorg-model-load)               ; dorg-model-load-tree (AS OF exact revision)
(require 'dorg-doctor)
(require 'dorg-dolt-server)
(require 'dorg-command)
(require 'dorg-import)

;;; ─── Hooks ─────────────────────────────────────────────────────────

(defcustom dorg-after-materialize-functions nil
  "Hook run after `dorg-materialize' successfully renders a buffer.
Each function is called with two arguments: BACKING-LOCATION and
BUFFER.  BACKING-LOCATION is the Dolt dir for Dolt-backed
registrations and the org file path for plain-org registrations."
  :type 'hook
  :group 'dorg)

;;; ─── Materialize ───────────────────────────────────────────────────

(defun dorg-materialize--dolt (buf reg)
  "Refresh BUF exactly from REG's Dolt repo.  Return the Dolt dir.
Captures HEAD once, then routes through the same exact whole-view materializer
as the keyword lifecycle.  External changes are observed only by an explicit
refresh; legacy dirty/base reconciliation is not a projection refresh path."
  (let* ((backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         (revision (dorg-head-commit dolt-dir)))
    (dorg-materialize--view-whole
     buf buf dolt-dir root-id revision)
    dolt-dir))

(defun dorg-materialize--plain-org (buf reg)
  "Reload REG's plain-org file into BUF.
Reads REG's file into BUF, replacing its contents.  Marks the
buffer unmodified after the load.  Signals if the file does not
exist.  Returns the canonical (truenamed) file path."
  (let* ((raw (or (dorg-registration-file reg)
                  (buffer-file-name buf)))
         (file (and raw (file-truename raw))))
    (unless (and file (file-exists-p file))
      (error "dorg: plain-org file %s does not exist"
             (or file "<nil>")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file)
        (set-buffer-modified-p nil)))
    file))

(defun dorg-materialize--parse-keys (keys)
  "Validate and return materialize lifecycle KEYS.
Only :source, :revision, and :window are accepted, each at most once."
  (let ((length (proper-list-p keys))
        seen)
    (unless (and length (zerop (% length 2)))
      (error "dorg: materialize keywords must be a proper key/value list"))
    (let ((rest keys))
      (while rest
        (let ((key (pop rest)))
          (unless (memq key '(:source :revision :window))
            (error "dorg: unknown materialize keyword %S" key))
          (when (memq key seen)
            (error "dorg: duplicate materialize keyword %S" key))
          (push key seen)
          (pop rest))))
    keys))

(defun dorg-materialize (&optional buffer &rest keys)
  "Render a DOrg-backed view into BUFFER (default current buffer).
This is the single public buffer-mutating view lifecycle command.  With no
keyword arguments it preserves the legacy contract: render BUFFER's own
registered backing store into BUFFER at HEAD (Dolt subtree, or plain-org
file reload).

Keyword arguments select an exact snapshot and projection that DOrg resolves,
loads, renders, and publishes from ONE owned snapshot, so buffer bytes and
canonical lifecycle state never diverge:

  :SOURCE    a REGISTERED buffer supplying the backing store/root.  Defaults
             to BUFFER; pass a different source to materialize its view into a
             fresh destination BUFFER without first rendering into it.
  :REVISION  an exact PUBLIC revision token (normally a `dorg-api-execute'
             `:new-token').  When supplied it is used for every load and label
             with NO HEAD read; nil captures the current revision ONCE.
  :WINDOW    nil for the whole registered root, or (:focus FOCI) for a bounded
             projection (root preamble, durable marker, ancestor spine, and
             each focus subtree rendered in full).  Each focus is a positive
             heading id or canonical public external identity
             (:extern :key K :value V), resolved at the exact revision.

DOrg loads and validates the projection AS OF the revision BEFORE mutating
BUFFER, so an invalid registration/revision/root/focus signals with BUFFER
untouched.  A whole view publishes canonical whole-tree sync state; a partial
view is push-refused and never masquerades as a whole-tree base.

For the Dolt backend returns a view-result descriptor
\(:scope SCOPE :revision REVISION :root-id RID :window WINDOW :buffer BUF);
observe an established view's scope with `dorg-materialized-view-scope'.

WARNING: destructive — it overwrites BUFFER content from the DB with no
merge.  Dolt projections are read-only; use typed mutations for writes.

Runs `dorg-after-materialize-functions' on success with the backing location
and BUFFER as arguments."
  (interactive)
  (when (keywordp buffer)
    (setq keys (cons buffer keys)
          buffer nil))
  (let* ((options (dorg-materialize--parse-keys keys))
         (source (plist-get options :source))
         (revision (plist-get options :revision))
         (window (plist-get options :window)))
    (if keys
        (dorg-materialize--view (or buffer (current-buffer)) source revision window)
      (dorg-materialize--legacy buffer))))

(defun dorg-materialize--legacy (&optional buffer)
  "Render BUFFER's own registered backing store into BUFFER at HEAD.
The pre-lifecycle whole-root materialize path, kept byte-for-byte so existing
zero/one-argument callers are unaffected.  Runs
`dorg-after-materialize-functions' on success."
  (let* ((buf (or buffer (current-buffer)))
         ;; Strict: materialize renders a Dolt subtree from its root_id, so it
         ;; needs a concrete backing store (ensure), not a pure classification.
         (reg (with-current-buffer buf (dorg--ensure-registration))))
    (unless reg
      (error "dorg: buffer %s has no registered backing storage"
             (buffer-name buf)))
    (let* ((backend (dorg-registration-backend reg))
           (location
            (pcase (and backend (dorg-backend-type backend))
              ('plain-org (dorg-materialize--plain-org buf reg))
              ('dolt      (dorg-materialize--dolt buf reg))
              (_ (if (dorg-backend-dolt-dir backend)
                     (dorg-materialize--dolt buf reg)
                   (error "dorg: registration for %s has no recognized backend"
                          (buffer-name buf)))))))
      (run-hook-with-args 'dorg-after-materialize-functions location buf))))

(defun dorg-materialize--view (dest source revision window)
  "Materialize DEST from SOURCE's registered store AT REVISION with WINDOW.
The exact-snapshot lifecycle behind `dorg-materialize''s keyword surface.
SOURCE (a registered buffer, default DEST) supplies the store/root only;
REVISION/WINDOW select the snapshot/projection.  Returns the view descriptor."
  (let ((src (or source dest)))
    (unless (buffer-live-p dest)
      (error "dorg: destination must be a live buffer"))
    (unless (buffer-live-p src)
      (error "dorg: source must be a live registered buffer"))
    (let ((reg (with-current-buffer src (dorg--ensure-registration))))
      (unless reg
        (error "dorg: source buffer %s has no registered backing storage"
               (buffer-name src)))
      (let* ((backend (dorg-registration-backend reg))
             (dolt-dir (and backend (dorg-backend-dolt-dir backend)))
             (root-id (and backend (dorg-backend-root-id backend))))
        (unless (and dolt-dir root-id)
          (error "dorg: :revision/:window require a Dolt-backed source (%s)"
                 (buffer-name src)))
        ;; Resolve the revision ONCE at entry: a supplied token means zero HEAD
        ;; reads; nil captures the current revision a single time.
        (let* ((rev (or revision (dorg-head-commit dolt-dir)))
               (descriptor
                (if window
                    (dorg-materialize--view-window
                     dest src dolt-dir root-id rev window)
                  (dorg-materialize--view-whole
                   dest src dolt-dir root-id rev))))
          (run-hook-with-args 'dorg-after-materialize-functions dolt-dir dest)
          descriptor)))))

(defun dorg-materialize--replace-bytes (dest text)
  "Replace DEST's whole contents with TEXT, preserving point; ensure org-mode."
  (with-current-buffer dest
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (let ((opoint (point)))
      (save-restriction
        (widen)
        (dorg-render--replace-buffer-text text))
      (goto-char (min opoint (point-max))))))

(defun dorg-materialize--lifecycle-snapshot (buffer)
  "Capture BUFFER's mutable materialize lifecycle state."
  (with-current-buffer buffer
    (list :bytes (buffer-substring-no-properties (point-min) (point-max))
          :point (point)
          :modified (buffer-modified-p)
          :read-only buffer-read-only
          :projection (dorg-projection-state-snapshot buffer))))

(defun dorg-materialize--restore-lifecycle (buffer snapshot)
  "Restore BUFFER's lifecycle fields from SNAPSHOT after byte rollback."
  (with-current-buffer buffer
    ;; `dorg-render--replace-buffer-text' intentionally uses
    ;; `with-silent-modifications' on cached Org buffers, which suppresses undo
    ;; recording and therefore cannot be reconstructed by a change group.
    ;; Restore the owned byte snapshot explicitly before resetting markers.
    (dorg-projection-with-programmatic-changes
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (dorg-render--replace-buffer-text (plist-get snapshot :bytes)))))
    (dorg-projection-state-restore
     (plist-get snapshot :projection) buffer)
    (setq buffer-read-only (plist-get snapshot :read-only))
    (goto-char (min (plist-get snapshot :point) (point-max)))
    (set-buffer-modified-p (plist-get snapshot :modified))))

(defun dorg-materialize--atomic-publication (dest function)
  "Run DEST mutation FUNCTION atomically across bytes and lifecycle state.
Any error cancels the buffer change group and restores the projection bytes
and established-view state.  The explicit byte snapshot also covers render
splices that deliberately suppress undo."
  (with-current-buffer dest
    (let ((snapshot (dorg-materialize--lifecycle-snapshot dest)))
      (condition-case err
          (progn
            (atomic-change-group
              (dorg-projection-with-programmatic-changes
                (funcall function)))
            ;; Exact Dolt views are output projections.  Seal the successful
            ;; publication only after bytes and projection state agree; error
            ;; paths below restore the caller's complete prior lifecycle.
            (setq buffer-read-only t)
            (set-buffer-modified-p nil))
        (error
         (dorg-materialize--restore-lifecycle dest snapshot)
         (signal (car err) (cdr err)))))))

(defun dorg-materialize--view-whole (dest source dolt-dir root-id rev)
  "Materialize DEST as the whole tree AS OF REV and publish projection state.
Loads and validates the tree BEFORE touching DEST (an invalid REV or a
revision with no root signals with DEST untouched); then renders bytes and
installs the established view from the SAME (REV, TREE) snapshot.  The
deprecated dirty/base/authored reconciliation graph is not mutated.  Returns
the descriptor."
  (let ((tree (dorg-model-load-tree dolt-dir root-id rev)))
    (unless (gethash root-id tree)
      (error "dorg: revision %s has no root %s in %s" rev root-id dolt-dir))
    (dorg-materialize--atomic-publication
     dest
     (lambda ()
       (dorg-materialize--replace-bytes
        dest (dorg-render--tree-to-text tree root-id rev dolt-dir))
       (dorg-projection-install-view
        dest source root-id rev 'whole nil)))
    (list :scope 'whole :revision rev :root-id root-id :window nil :buffer dest)))

(defun dorg-materialize--extern-focus-p (focus)
  "Return non-nil when FOCUS is a canonical public external identity."
  (and (equal (proper-list-p focus) 5)
       (eq (nth 0 focus) :extern)
       (eq (nth 1 focus) :key)
       (stringp (nth 2 focus))
       (eq (nth 3 focus) :value)
       (stringp (nth 4 focus))))

(defun dorg-materialize--normalize-window-foci
    (dolt-dir root-id rev foci)
  "Resolve public FOCI at REV into canonical targets and private heading ids.
Each focus is a positive numeric heading id or a canonical external identity
~(:extern :key K :value V)~.  External identities are resolved root-scoped AS
OF REV through the shared DB-to-model lookup.  Returns
~(:targets TARGETS :ids IDS)~, each duplicate-free in caller order."
  (unless (and (proper-list-p foci) foci)
    (error "dorg: :window requires a non-empty proper focus list"))
  (let (targets ids)
    (dolist (focus foci)
      (let
          ((id
            (cond
             ((and (integerp focus) (> focus 0))
              focus)
             ((dorg-materialize--extern-focus-p focus)
              (let* ((key (plist-get (cdr focus) :key))
                     (value (plist-get (cdr focus) :value))
                     (matches
                      (dorg-model-load--heading-ids-by-property
                       dolt-dir root-id key value rev)))
                (cond
                 ((null matches)
                  (error
                   "dorg: no heading with %s=%S under root %s at revision %s"
                   key value root-id rev))
                 ((cdr matches)
                  (error
                   (concat "dorg: %d headings match %s=%S under root %s"
                           " at revision %s; identity must be unique")
                   (length matches) key value root-id rev))
                 (t (car matches)))))
             (t
              (error
               (concat
                "dorg: window focus must be a positive heading id or canonical"
                " (:extern :key K :value V): %S")
               focus)))))
        (unless (member focus targets)
          (setq targets (append targets (list focus))))
        (unless (memql id ids)
          (setq ids (append ids (list id))))))
    (list :targets targets :ids ids)))

(defun dorg-materialize--view-window (dest source dolt-dir root-id rev window)
  "Materialize DEST as the bounded WINDOW AS OF REV; keep it push-refused.
WINDOW is (:focus FOCI), where each focus is a positive heading id or canonical
public external identity (:extern :key K :value V).  Resolves external identity
and renders/validates the projection BEFORE touching DEST; then installs the
partial view.  It never publishes or mutates legacy whole-tree reconciliation
state.  Returns the descriptor."
  (let ((window-length (proper-list-p window)))
    (unless (and (= (or window-length -1) 2)
                 (eq (car window) :focus))
      (error "dorg: :window must be exactly (:focus FOCI)"))
    (let* ((resolved
            (dorg-materialize--normalize-window-foci
             dolt-dir root-id rev (cadr window)))
           (targets (plist-get resolved :targets))
           (ids (plist-get resolved :ids)))
      ;; Render (validating each focus) AS OF REV before mutating DEST.
      (let ((text (dorg-render--window dolt-dir ids root-id rev))
            (normalized-window (list :focus targets)))
        (dorg-materialize--atomic-publication
         dest
         (lambda ()
           (dorg-materialize--replace-bytes dest text)
           (dorg-projection-install-view
            dest source root-id rev 'partial targets)))
        (list :scope 'partial :revision rev :root-id root-id
              :window normalized-window :buffer dest)))))

;;; ─── Established View Refresh ─────────────────────────────────────

(defun dorg--refresh-materialized-view-context (&optional buffer)
  "Return validated refresh context for BUFFER's established Dolt view.
BUFFER defaults to the current buffer.  This is the narrow lazy boundary used
by typed interactive commands before they commit: it validates the private
materialized-view record, source liveness, registered root, scope, and window
without reading HEAD, loading a model, mutating a buffer, or touching storage.

The returned plist contains :buffer, :source, :root-id, :scope, and :window.
:window is nil for a whole view and the public normalized (:focus TARGETS)
form for a partial view.  Signals `user-error' when no coherent established
view exists."
  (let* ((dest (or buffer (current-buffer))))
    (unless (buffer-live-p dest)
      (user-error "dorg: refresh requires a live projection buffer"))
    (let ((view (dorg-projection-view-context dest)))
      (unless view
        (user-error
         "dorg: no established materialized view; materialize the projection first"))
      (let* ((source (plist-get view :source))
             (root-id (plist-get view :root-id))
             (scope (plist-get view :scope))
             (targets (plist-get view :window)))
        (unless (buffer-live-p source)
          (user-error "dorg: materialized view source buffer is no longer live"))
        (let* ((registration (dorg--classify-registration source))
               (backend (and registration
                             (dorg-registration-backend registration)))
               (registered-root (and backend
                                     (dorg-backend-root-id backend))))
          (unless (and backend (dorg-backend-dolt-p backend)
                       (integerp root-id) (integerp registered-root)
                       (= root-id registered-root))
            (user-error
             "dorg: materialized view identity is stale; materialize it again")))
        (pcase scope
          ('whole
           (when targets
             (user-error "dorg: whole materialized view carries a stale window")))
          ('partial
           (unless (and (proper-list-p targets) targets)
             (user-error "dorg: partial materialized view has no valid window")))
          (_ (user-error "dorg: materialized view has invalid scope %S" scope)))
        (list :buffer dest :source source :root-id root-id :scope scope
              :window (and (eq scope 'partial) (list :focus targets)))))))

(defun dorg-refresh-materialized-view (&optional buffer revision)
  "Refresh BUFFER's established Dolt view, optionally at exact REVISION.
BUFFER defaults to the current buffer.  The command preserves the established
source, root, and whole/partial window.  With REVISION non-nil, all loading and
publication uses that exact public token and performs no HEAD selection; with
nil, `dorg-materialize' captures HEAD once.  Identity and scope are validated
before any materialization work.  Returns `dorg-materialize''s descriptor."
  (interactive)
  (let* ((context (dorg--refresh-materialized-view-context buffer))
         (dest (plist-get context :buffer))
         (source (plist-get context :source))
         (window (plist-get context :window)))
    (if window
        (dorg-materialize dest :source source :revision revision :window window)
      (dorg-materialize dest :source source :revision revision))))

;;; ─── Minor Mode ────────────────────────────────────────────────────

(defcustom dorg-dolt-auto-server t
  "If non-nil, `dorg-mode' starts a dolt sql-server when enabled.
The server is shared across all buffers backed by the same Dolt
repository.  Queries routed through the server are significantly
faster than CLI mode."
  :type 'boolean
  :group 'dorg)

(defcustom dorg-activation-claim-lock-timeout 0.25
  "Seconds `dorg-mode' activation may spend acquiring the server claim lock.
Bounds the PASSIVE mode-enable path only: while `dorg-mode' is being
enabled, `dorg-dolt-server-claim-lock-timeout' is dynamically bound to
this (small) value so server discovery fails FAST when a
live holder owns `<store>/.dorg/server.claim.lock' — degrading to a
warning instead of blocking the full operation timeout before the minor
mode finishes enabling.

Only the claim-lock ACQUIRE spin is bounded, not query execution, so an
uncontended activation still starts the server at full speed.
Explicit `dorg-bind-dolt'/`dorg-setup' and user-initiated operations are
unaffected: they keep the default `dorg-dolt-server-claim-lock-timeout'
and remain strict/fail-closed."
  :type 'number
  :group 'dorg)

(defvar-local dorg--owns-buffer-read-only nil
  "Non-nil when DOrg made this Dolt projection read-only.
The state follows the Dolt binding rather than `dorg-mode': disabling the
minor mode does not make a database projection an authoring surface.")

(defun dorg--engage-strict-transport-p (port)
  "Return non-nil when this buffer should freeze strict `server' transport.
PORT is the result of `dorg-dolt-server--ensure-strict' (non-nil means a usable managed
server is available).  True only when PORT is non-nil and Emacs is interactive
\(mysql.el is a HARD dependency present in every loaded dorg process, so it is
not re-checked here).  The public SQL transport is now server-only; this
buffer-local binding is an explicit freeze for the interactive projection
path."
  (and port
       (not noninteractive)))

(defun dorg--ensure-dolt-server ()
  "Start a dolt sql-server for the current buffer's repo if configured.
Does nothing when `dorg-dolt-auto-server' is nil or the buffer has no
registration.  On a successful ensure for an interactive buffer, freezes the
buffer-local SQL transport to strict `server' so typed commands route through
the managed server we just ensured.
Errors are caught so mode activation is never blocked.  External DB changes
are observed by `dorg-refresh-materialized-view', never by a background poll."
  (when (and dorg-dolt-auto-server (dorg--classify-registration))
    (condition-case err
        (let* ((dolt-dir (dorg-registered-dolt-dir))
               (port (dorg-dolt-server--ensure-strict dolt-dir)))
          (when (dorg--engage-strict-transport-p port)
            (setq-local dorg-sql-transport 'server)))
      (error
       (display-warning 'dorg
                        (format "Auto-server failed: %s"
                                (error-message-string err))
                        :warning)))))

(defun dorg--dolt-projection-p (&optional buffer)
  "Return non-nil when BUFFER is purely classified as Dolt-backed.
This predicate reads registration/file identity or, for a buffer without a
file registration, a genuine Dolt preamble binding.  It never binds a root,
runs SQL, starts a server, imports, materializes, or mutates registry state."
  (with-current-buffer (or buffer (current-buffer))
    ;; Modification hooks run inside Org editing primitives that may depend on
    ;; point, narrowing, and match data.  Identity classification must be pure
    ;; with respect to those dynamic editor values as well as storage.
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (let* ((registration (dorg--classify-registration (current-buffer)))
                 (backend (and registration
                               (dorg-registration-backend registration))))
            (or (and backend (dorg-backend-dolt-p backend))
                (and (null registration)
                     (dorg--preamble-dolt-keyword (current-buffer))))))))))

(defun dorg--projection-policy-before-change (&rest _change)
  "Refuse direct edits to the current Dolt projection.
DOrg-owned byte publication is admitted only through the existing dynamic
programmatic-change capability."
  (when (and (dorg--dolt-projection-p)
             (not (dorg-projection-programmatic-change-p)))
    (user-error
     "dorg: Dolt projections are read-only; use a typed command or refresh")))

(defun dorg--projection-policy-before-save ()
  "Refuse direct saves of the current Dolt projection.
Plain Org keeps ordinary Emacs save semantics.  The programmatic-change
capability admits DOrg's exact setup/publication save only."
  (when (and (dorg--dolt-projection-p)
             (not (dorg-projection-programmatic-change-p)))
    (user-error
     "dorg: Dolt projections cannot be saved; use a typed command or refresh")))

(defun dorg--projection-policy-apply-read-only ()
  "Apply the current buffer's identity-derived projection read-only state."
  (if (dorg--dolt-projection-p)
      (progn
        (unless buffer-read-only
          (setq dorg--owns-buffer-read-only t))
        (setq buffer-read-only t))
    (when dorg--owns-buffer-read-only
      (setq buffer-read-only nil
            dorg--owns-buffer-read-only nil))))

(defun dorg--projection-policy-after-revert ()
  "Re-apply identity-derived projection state after reverting the buffer."
  (dorg--projection-policy-apply-read-only))

(defun dorg--install-projection-policy ()
  "Install buffer-local identity guards on the current Org buffer.
The hooks dynamically classify every attempted edit/save, so a late explicit
binding is protected without a registry callback.  They intentionally survive
`dorg-mode' disable; the storage authority, not a UI mode bit, owns policy."
  (add-hook 'before-change-functions
            #'dorg--projection-policy-before-change nil t)
  (add-hook 'before-save-hook #'dorg--projection-policy-before-save nil t)
  (add-hook 'after-revert-hook #'dorg--projection-policy-after-revert nil t)
  (dorg--projection-policy-apply-read-only))

;; `dorg-history' lives in dorg-history.el (loaded via dorg.el's require
;; chain); declared here to bind it in the keymap without a require cycle.
(declare-function dorg-history "dorg-history" (&optional root-scope))

;; KEEP (keymap): mode map bound by `define-derived-mode dorg-mode'.
(defvar dorg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x s") #'dorg-refresh-materialized-view)
    (define-key map (kbd "C-c C-x m") #'dorg-materialize)
    (define-key map (kbd "C-c C-x q") #'dorg-query)
    (define-key map (kbd "C-c C-x g") #'dorg-goto-id)
    (define-key map (kbd "C-c C-x p") #'dorg-set-property)
    (define-key map (kbd "C-c C-x d") #'dorg-doctor)
    (define-key map (kbd "C-c C-x h") #'dorg-history)
    map)
  "Keymap for `dorg-mode'.")

(defun dorg--current-backend-plain-org-p ()
  "Return non-nil when the current buffer's registered backend is plain-org."
  (when-let* ((reg (dorg--classify-registration))
              (backend (dorg-registration-backend reg)))
    (dorg-backend-plain-org-p backend)))

;; Projection safety follows storage authority even when no DOrg UI mode is
;; enabled.  Installation is metadata-only; the guards classify dynamically.
(add-hook 'org-mode-hook #'dorg--install-projection-policy)

(define-minor-mode dorg-mode
  "Minor mode for dorg-backed org buffers.
Buffers in `dorg-mode' have their content backed by a storage
backend: Dolt when bound via `dorg-bind-dolt' (or a
`#+DORG_DOLT_DIR:' file keyword), and plain-org by default.

Dolt-backed buffers are read-only projections independently of this mode.
Semantic writes use typed commands and `C-c C-x s' refreshes the established
view.  Mode enable never parses or reconciles the whole buffer.  Plain-org
buffers retain ordinary editing and saving.  External Dolt changes become
visible through explicit refresh.

\\{dorg-mode-map}"
  :lighter " dorg"
  :keymap dorg-mode-map
  :group 'dorg
  (when dorg-mode
      ;; Passive activation must return promptly.  Source classification is
      ;; pure by construction now (`dorg--classify-registration' never
      ;; binds), so the only thing to bound is the server claim-lock
      ;; acquire: bind it to
      ;; `dorg-activation-claim-lock-timeout' so a held live claim lock
      ;; degrades to a warning instead of blocking the full operation timeout.
      ;; The binding is dynamic and scoped to enable only; explicit bind/setup
      ;; and user operations keep the default (strict) timeout.
      (let ((dorg-dolt-server-claim-lock-timeout dorg-activation-claim-lock-timeout))
        (dorg--install-projection-policy)
        (unless (dorg--current-backend-plain-org-p)
          ;; Re-establish the partial-view marker from a durable #+DORG_WINDOW:
          ;; keyword BEFORE any sync/push: a freshly-visited bounded-window
          ;; `beads.org' must be push-protected so out-of-window siblings are
          ;; never inferred as deletions.  A no-op on a full materialization.
          (dorg-render--restore-window-marker)
          (dorg--ensure-dolt-server)
          (dorg--projection-policy-apply-read-only)))))

;;; ─── Setup ─────────────────────────────────────────────────────────

(defun dorg-setup--heading-count (&optional buffer)
  "Return the number of Org headlines in BUFFER (default current).
Counts widened, for the first-run setup summary; an approximate
synced-heading count, not a DB query."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((n 0))
          (while (re-search-forward "^\\*+ " nil t)
            (setq n (1+ n)))
          n)))))

(defun dorg-setup (dolt-dir)
  "Full first-time setup for the current buffer.
Checks for a supported Dolt binary, explicitly bootstraps the complete
current file into DOLT-DIR, binds the imported root, materializes the
exact committed revision, saves that canonical projection, and only then
enables `dorg-mode'.  The first ingest never relies on save hooks,
mode-enable sync, or metadata writeback before the import commits.

Interactively, prompts for DOLT-DIR and, when that directory has no
Dolt repo yet, confirms before initializing a new one.  Programmatic
callers are never prompted or confirmed.  Returns the structured
`dorg-import-document' receipt."
  (interactive
   (progn
     ;; Fail fast on a missing/old Dolt before prompting for anything.
     (dorg-dolt--ensure-available)
     (let ((dir (read-directory-name
                 "Dolt directory: "
                 (file-name-directory
                  (or (buffer-file-name) default-directory)))))
       ;; Confirm before initializing a brand-new repo in the chosen dir.
       (when (not (file-exists-p (expand-file-name ".dolt" dir)))
         (unless (y-or-n-p
                  (format "No Dolt repo in %s — initialize a new one there? "
                          (abbreviate-file-name dir)))
           (user-error "dorg-setup cancelled")))
       (list dir))))
  ;; Eager availability gate — also covers programmatic callers (the version
  ;; probe is cached, so re-checking after the interactive spec is cheap).
  (dorg-dolt--ensure-available)
  (unless (buffer-file-name)
    (error "dorg: buffer has no file"))
  (let* ((buffer (current-buffer))
         (file (buffer-file-name))
         (receipt
          (dorg-import-document
           buffer
           :dolt-dir dolt-dir
           :target-file file
           :mode 'bootstrap))
         (reg (dorg-bind-dolt file dolt-dir))
         (bound-dir
          (dorg-backend-dolt-dir (dorg-registration-backend reg))))
    (dorg-materialize
     buffer :source buffer :revision (plist-get receipt :commit))
    ;; This is byte-exact publication of an already-committed projection, not
    ;; another ingest.  Suppress DOrg and user save transforms: a formatter or
    ;; legacy sync hook must not make the on-disk projection diverge from the
    ;; exact revision just materialized.
    (dorg-projection-with-programmatic-changes
      (let ((before-save-hook nil)
            (after-save-hook nil)
            (write-file-functions nil)
            (write-contents-functions nil))
        ;; Materialization seals the projection clean.  Setup is the one
        ;; explicit exact-byte publication path, so opt its committed bytes
        ;; back into `save-buffer' without weakening the render boundary.
        (set-buffer-modified-p t)
        (save-buffer)))
    ;; Re-enable from a clean state so a buffer that previously had plain-org
    ;; UI state receives Dolt server/projection behavior.  Mode enable cannot
    ;; import or reconcile buffer bytes.
    (when dorg-mode
      (dorg-mode -1))
    (dorg-mode 1)
    (let ((n (dorg-setup--heading-count)))
      (message
       (concat "dorg: setup complete — %s\n"
               "  Dolt dir: %s\n"
               "  %d heading%s imported\n"
               "  Query with C-c C-x q (M-x dorg-query)")
       (file-name-nondirectory file)
       (abbreviate-file-name bound-dir)
       n (if (= n 1) "" "s")))
    receipt))

;;; ─── Global Mode ───────────────────────────────────────────────────

(defun dorg--maybe-enable-mode ()
  "Enable `dorg-mode' if the current buffer declares a Dolt directory.
Triggered by a `#+DORG_DOLT_DIR:' (or legacy fallback) file
keyword.  No-keyword buffers are left alone so plain-org files
are not silently auto-enabled globally; enable `dorg-mode'
manually for those.  Intended for use on `org-mode-hook'."
  (when (and (buffer-file-name)
             (not dorg-mode)
             (dorg--buffer-dolt-dir))
    (dorg-mode 1)))

;;;###autoload
(define-minor-mode global-dorg-mode
  "Global minor mode that auto-enables `dorg-mode' on org buffers.
When active, any org buffer declaring a Dolt directory via a
`#+DORG_DOLT_DIR:' (or legacy fallback) file keyword gets
`dorg-mode' enabled automatically.  Plain-org (no-keyword) files
are not auto-enabled — enable `dorg-mode' manually for those.
Add `(global-dorg-mode 1)' to your init file for hands-free dorg."
  :global t
  :group 'dorg
  (if global-dorg-mode
      (add-hook 'org-mode-hook #'dorg--maybe-enable-mode)
    (remove-hook 'org-mode-hook #'dorg--maybe-enable-mode)))

(provide 'dorg-mode)
;;; dorg-mode.el ends here
