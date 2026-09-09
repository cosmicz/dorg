;;; dorg-history.el --- DB-level undo: browse + restore a prior Dolt commit -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; DB-level "undo" for a Dolt-backed org buffer: the
;; authoritative history of a Dolt-backed buffer is the Dolt commit graph,
;; not Emacs buffer undo (which is semantically broken across whole-buffer
;; re-renders and catastrophically expensive to persist on the materialized
;; view).  This module exposes that history as first-class
;; user commands:
;;
;;   - BROWSE prior versions of a bead (or the whole root) — `dorg-history'.
;;   - RESTORE a bead/root to a chosen prior commit — `dorg-restore-to-commit'
;;     and the `*dorg-history*' restore action.
;;
;; A restore is FORWARD-ONLY: it reads the target state AS OF the chosen
;; commit and writes the typed patches that transform the CURRENT scope into that
;; state as a NEW commit on HEAD (revert semantics).  History is never
;; rewritten — no DOLT_RESET/REVERT/CHECKOUT — so `dolt_log' keeps every
;; intervening commit and the restore is itself undoable by restoring
;; forward again.  This matches DOrg's optimistic/non-blocking sync model.
;;
;; Everything rides existing machinery: as-of reads (`dorg-model-load-tree'),
;; subtree ids (`dorg-render-subtree-ids'), the durable typed-patch write path
;; (with `:base-commit'/`:message' and the Dolt retry
;; contract), the read-only VFS island for as-of viewing, and
;; `dorg-materialize' for re-materialization.  No schema or serializer
;; change; the restore rides the existing v4 write path.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'dorg-dolt)
(require 'dorg-model-load)               ; hydration loaders (tracked-case)
(require 'dorg-model)
(require 'dorg-render)
(require 'dorg-mutate)
(require 'dorg-vfs)
(require 'dorg-command)
(require 'dorg-id)
(require 'dorg-registry)

;;; ─── History data API ─────────────────────────────────────────────────

(defun dorg-history--short (commit)
  "Return the 7-char short form of COMMIT (or COMMIT when shorter)."
  (and commit (substring commit 0 (min 7 (length commit)))))

(defun dorg-history--log-commits (dolt-dir)
  "Return DOLT-DIR's commit graph (root scope) as newest-first plists.
Each entry is (:commit H :short S :date D :committer C :message M :title nil)."
  (mapcar
   (lambda (lr)
     (let ((h (alist-get 'commit_hash lr)))
       (list :commit h
             :short (dorg-history--short h)
             :date (alist-get 'date lr)
             :committer (alist-get 'committer lr)
             :message (alist-get 'message lr)
             :title nil)))
   (dorg-sql dolt-dir
             "SELECT commit_hash, committer, date, message FROM dolt_log")))

(defun dorg-history--bead-commits (dolt-dir root-id bead-id)
  "Return newest-first history plists for BEAD-ID's subtree in DOLT-DIR.
A commit is included when it CHANGED any heading row OR any item under the
bead's subtree (so a body/item-only edit is in the bead's history, not just
headline edits — resolved decision 3).  Changes are detected via the Dolt
`dolt_diff_*' system tables; ordering and committer/message/date come from
`dolt_log' (newest-first, tie-safe).  `:title' is the bead title as of each
heading-changing commit (nil for an item-only commit)."
  (let ((changed (make-hash-table :test 'equal))   ; commit_hash -> t
        (titles (make-hash-table :test 'equal))    ; commit_hash -> title-as-of
        (ids (dorg-render-subtree-ids dolt-dir root-id bead-id)))
    (when ids
      (let ((in (mapconcat #'number-to-string ids ",")))
        ;; Heading-row changes (added/modified) carry the as-of title.
        (dolist (r (dorg-sql
                    dolt-dir
                    (format (concat "SELECT to_commit AS c, to_title AS t "
                                    "FROM dolt_diff_headings "
                                    "WHERE to_id IN (%s) AND to_commit IS NOT NULL")
                            in)))
          (let ((c (alist-get 'c r)))
            (puthash c t changed)
            (when (alist-get 't r) (puthash c (alist-get 't r) titles))))
        ;; Item changes (body/keyword/planning/drawer) under the subtree.
        (dolist (r (dorg-sql
                    dolt-dir
                    (format (concat "SELECT DISTINCT to_commit AS c "
                                    "FROM dolt_diff_heading_items "
                                    "WHERE to_heading_id IN (%s) AND to_commit IS NOT NULL")
                            in)))
          (puthash (alist-get 'c r) t changed))))
    ;; Walk dolt_log (newest-first) and keep only changed commits.
    (let (entries)
      (dolist (lr (dorg-sql dolt-dir
                            "SELECT commit_hash, committer, date, message FROM dolt_log"))
        (let ((h (alist-get 'commit_hash lr)))
          (when (gethash h changed)
            (push (list :commit h
                        :short (dorg-history--short h)
                        :date (alist-get 'date lr)
                        :committer (alist-get 'committer lr)
                        :message (alist-get 'message lr)
                        :title (gethash h titles))
                  entries))))
      (nreverse entries))))

(defun dorg-history-commits (dolt-dir root-id &optional bead-id)
  "Return prior-commit history plists for DOLT-DIR, newest-first.
Without BEAD-ID this is the root timeline (the `dolt_log' graph from HEAD);
with BEAD-ID it is the commits that touched that bead's subtree (heading
rows and items).  Each entry is a plist
\(:commit H :short S :date D :committer C :message M :title T)."
  (if bead-id
      (dorg-history--bead-commits dolt-dir root-id bead-id)
    (dorg-history--log-commits dolt-dir)))

;;; ─── Restore planner (pure: two trees -> typed patches) ───────────────

(defun dorg-history--item-to-value (item)
  "Serialize an EIEIO `dorg-heading-item' ITEM to a typed `:items' value plist.
Returns nil for a DOrg-owned item (a DORG_* keyword, or a drawer left empty
after dropping DORG_* properties) so the restore never re-emits protected
metadata DOrg manages itself."
  (cond
   ((cl-typep item 'dorg-text-item)
    (list :kind 'text :text (dorg-text-item-text item)))
   ((cl-typep item 'dorg-keyword-item)
    (let ((k (dorg-keyword-item-keyword item)))
      (unless (dorg-protected-keyword-p k)
        (list :kind 'keyword :keyword k :value (dorg-keyword-item-value item)))))
   ((cl-typep item 'dorg-planning-item)
    (list :kind 'planning
          :scheduled (dorg-planning-item-scheduled item)
          :deadline (dorg-planning-item-deadline item)
          :closed (dorg-planning-item-closed item)))
   ((cl-typep item 'dorg-drawer-item)
    (let ((props (cl-remove-if
                  (lambda (kv) (dorg-protected-keyword-p (car kv)))
                  (dorg-drawer-item-properties item)))
          (lines (dorg-drawer-item-lines item))
          (name (dorg-drawer-item-name item)))
      (cond (props (list :kind 'drawer :name name :properties props))
            (lines (list :kind 'drawer :name name :lines lines))
            (t nil))))
   (t (error "dorg-history: unknown heading-item type %S" item))))

(defun dorg-history--items-to-values (items)
  "Serialize a heading's ITEMS (EIEIO objects) to typed `:items' value plists,
dropping DOrg-owned items (see `dorg-history--item-to-value')."
  (delq nil (mapcar #'dorg-history--item-to-value items)))

(defun dorg-history--headline-fields (heading)
  "Return HEADING's exact typed mutable headline fields.
Nil values are intentional: restore is whole-state recovery, so fields removed
after the target commit must be cleared rather than silently retained."
  (list :title (dorg-heading-title heading)
        :todo (dorg-heading-todo heading)
        :priority (dorg-heading-priority heading)
        :tags (dorg-heading-tags heading)
        :commented (and (dorg-heading-commented heading) t)
        :post-blank (or (dorg-heading-post-blank heading) 0)))

(defun dorg-history--headline-differs-p (cur tgt)
  "Return non-nil when CUR and TGT differ in any mutable headline field."
  (not (and (equal (dorg-heading-title cur) (dorg-heading-title tgt))
            (equal (dorg-heading-todo cur) (dorg-heading-todo tgt))
            (equal (dorg-heading-priority cur) (dorg-heading-priority tgt))
            (equal (dorg-heading-tags cur) (dorg-heading-tags tgt))
            (eq (and (dorg-heading-commented cur) t)
                (and (dorg-heading-commented tgt) t))
            (eql (dorg-heading-post-blank cur) (dorg-heading-post-blank tgt)))))

(defun dorg-history--update-patches (cur tgt)
  "Return typed patches restoring CUR to TGT for an id in both trees.
A heading update is emitted when fields differ, plus a whole-items replacement
when the (DOrg-owned-stripped) item content differs."
  (let (patches)
    (when (dorg-history--headline-differs-p cur tgt)
      (push (list :op 'update
                  :target (list :heading :id (dorg-heading-id tgt))
                  :fields (dorg-history--headline-fields tgt))
            patches))
    (unless (equal (dorg-history--items-to-values (dorg-heading-items cur))
                   (dorg-history--items-to-values (dorg-heading-items tgt)))
      (push (list :op 'replace
                  :target (list :items
                                :host (list :heading :id (dorg-heading-id tgt)))
                  :items (dorg-history--items-to-values (dorg-heading-items tgt)))
            patches))
    (nreverse patches)))

(defun dorg-history--insert-patch (tgt)
  "Return a private typed tree insert re-creating TGT exactly."
  (let ((parent (dorg-heading-parent-id tgt)))
    (list :op 'insert
          :target (list :tree :under
                        (if parent (list :heading :id parent) :root))
          :heading
          (append (list :id (dorg-heading-id tgt))
                  (dorg-history--headline-fields tgt)
                  (list :items
                        (dorg-history--items-to-values
                         (dorg-heading-items tgt)))))))

(defun dorg-restore--patches (current-ht target-ht scope-ids)
  "Return typed patches transforming CURRENT-HT into
TARGET-HT within SCOPE-IDS.

For each id in SCOPE-IDS:
  - in CURRENT only        -> typed heading delete (emitted leaf-up),
  - in TARGET only         -> typed tree insert preserving id+items+parent
                              (emitted root-down),
  - in both, content differ -> typed heading update and/or items replace,
  - unchanged              -> no op.
Deletes are ordered deepest-first and inserts shallowest-first so parent
invariants hold when the plan is applied in order."
  (let (deletes inserts updates)
    (dolist (id scope-ids)
      (let ((cur (gethash id current-ht))
            (tgt (gethash id target-ht)))
        (cond
         ((and cur (null tgt))
          (push (cons (dorg-heading-level cur)
                      (list :op 'delete :target (list :heading :id id)))
                deletes))
         ((and tgt (null cur))
          (push (cons (dorg-heading-level tgt)
                      (dorg-history--insert-patch tgt))
                inserts))
         ((and cur tgt)
          (setq updates
                (nconc updates (dorg-history--update-patches cur tgt)))))))
    (nconc
     ;; deletes deepest-first (leaf-up)
     (mapcar #'cdr (sort deletes (lambda (a b) (> (car a) (car b)))))
     updates
     ;; inserts shallowest-first (root-down)
     (mapcar #'cdr (sort inserts (lambda (a b) (< (car a) (car b))))))))

;;; ─── Restore command (durable, forward-only write) ────────────────────

(defun dorg-history--scope-ids (dolt-dir root-id scope-id commit current target)
  "Return the heading ids in restore scope.
Root scope (SCOPE-ID nil): every id present in CURRENT or TARGET.
Bead scope: the subtree of SCOPE-ID at HEAD unioned with its subtree as of
COMMIT (so children added or removed between COMMIT and HEAD are covered)."
  (if (null scope-id)
      (let ((seen (make-hash-table :test 'eql)) acc)
        (maphash (lambda (k _v) (puthash k t seen)) current)
        (maphash (lambda (k _v) (puthash k t seen)) target)
        (maphash (lambda (k _v) (push k acc)) seen)
        acc)
    (delete-dups
     (append (dorg-render-subtree-ids dolt-dir root-id scope-id)
             (dorg-render-subtree-ids dolt-dir root-id scope-id commit)))))

(defun dorg-history--default-message (scope-id commit)
  "Default commit message for restoring SCOPE-ID (or root) to COMMIT."
  (format "dorg: restore %s to %s"
          (if scope-id (format "bead %s" scope-id) "root")
          (dorg-history--short commit)))

(defun dorg-restore-to-commit (dolt-dir root-id scope-id commit &optional message)
  "Restore SCOPE-ID (a bead id, or nil for the whole root) in DOLT-DIR to
its state AS OF COMMIT, as a durable forward-only Dolt write, and return the
typed mutation result (or nil for a no-op restore).

Guards (no write on failure): COMMIT must be an ancestor of HEAD, and a bead
SCOPE-ID must have existed at COMMIT — otherwise a `user-error' is signalled.
A no-op (target already current) messages and returns nil.  The write is
wrapped in the Dolt retry seam (`dorg-dolt-with-retry',
`dorg-dolt-max-retries'): a retryable Dolt failure
reloads the current tree, re-plans against the fresh HEAD, and retries."
  (unless (dorg-commit-ancestor-p dolt-dir commit)
    (user-error "dorg: commit %s is not an ancestor of HEAD"
                (dorg-history--short commit)))
  (let ((target (dorg-model-load-tree dolt-dir root-id commit)))
    (when (and scope-id (null (gethash scope-id target)))
      (user-error "dorg: bead %s did not exist at %s"
                  scope-id (dorg-history--short commit)))
    ;; Retry FROM SCRATCH on transient Dolt errors: every attempt reloads
    ;; the live tree and recomputes the plan.  The already-at-that-state
    ;; path returns nil — the wrapper's nil-protection exits after one
    ;; attempt rather than looping on a sentinel.
    (dorg-dolt-with-retry
      (let* ((current (dorg-model-load-tree dolt-dir root-id))
             (base (dorg-head-commit dolt-dir))
             (scope-ids (dorg-history--scope-ids
                         dolt-dir root-id scope-id commit current target))
             (patches (dorg-restore--patches current target scope-ids)))
        (if (null patches)
            (progn (message "dorg: already at that state") nil)
          (dorg-mutate--apply-patches
           (list :dolt-dir dolt-dir :root-id root-id)
           patches nil
           :base-commit base
           :message (or message
                        (dorg-history--default-message scope-id commit))))))))

;;; ─── Command-level UX (thin, testable entry points) ───────────────────

(defun dorg-history--rows (dolt-dir root-id scope-id)
  "Return `*dorg-history*' tabulated rows for the history of SCOPE-ID (or the
root when SCOPE-ID is nil) in DOLT-DIR — one row per commit, newest-first.
Each row is (COMMIT-HASH . [SHORT DATE COMMITTER TITLE-OR-MESSAGE]) so the
list id is the commit and the columns are display-only."
  (mapcar
   (lambda (c)
     (list (plist-get c :commit)
           (vector (or (plist-get c :short) "")
                   (format "%s" (or (plist-get c :date) ""))
                   (format "%s" (or (plist-get c :committer) ""))
                   (or (plist-get c :title)
                       (plist-get c :message)
                       ""))))
   (dorg-history-commits dolt-dir root-id scope-id)))

(defun dorg-history--restore (dolt-dir root-id scope-id commit origin-buffer)
  "Restore SCOPE-ID (or root) in DOLT-DIR to COMMIT, then re-materialize
ORIGIN-BUFFER from the new HEAD so the restored state is visible there.
Returns the restore result."
  (prog1 (dorg-restore-to-commit dolt-dir root-id scope-id commit)
    (when (buffer-live-p origin-buffer)
      (with-current-buffer origin-buffer
        (dorg-materialize origin-buffer)))))

(defun dorg-history--view-path (dolt-dir scope-id commit)
  "Return a read-only as-of VFS island path for bead SCOPE-ID at COMMIT.
Bead scope yields `/dorg:DIR@COMMIT:/ID' (visiting it never writes — the
partial-view marker blocks push).  Root scope (SCOPE-ID nil) signals a
`user-error': whole-tree as-of VIEW is out of scope for v0.1 (root RESTORE
remains supported)."
  (if (null scope-id)
      (user-error
       "dorg: whole-tree as-of view is not available in v0.1; restore the root or view a single bead")
    (dorg-vfs-unparse (list :dolt-dir dolt-dir :ref commit :node-id scope-id))))

;;; ─── Interactive command + *dorg-history* mode ────────────────────────

(defvar-local dorg-history--origin nil
  "Buffer-local state of a `*dorg-history*' buffer: a plist
\(:dolt-dir D :root-id R :scope-id S :buffer ORIGIN).")

;; KEEP (keymap): mode map bound by `define-derived-mode dorg-history-mode'.
(defvar dorg-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dorg-history-view)
    (define-key map (kbd "v")   #'dorg-history-view)
    (define-key map (kbd "r")   #'dorg-history-restore-at-point)
    (define-key map (kbd "g")   #'dorg-history-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `dorg-history-mode'.")

(define-derived-mode dorg-history-mode tabulated-list-mode "DOrg-History"
  "Major mode for browsing and restoring a bead/root's Dolt history."
  (setq tabulated-list-format [("Commit" 9 t)
                               ("Date" 22 t)
                               ("Author" 14 t)
                               ("Title/Message" 0 t)])
  (tabulated-list-init-header))

(defun dorg-history--render-buffer (state)
  "Render the `*dorg-history*' buffer for STATE (the origin plist)."
  (let ((buf (get-buffer-create "*dorg-history*")))
    (with-current-buffer buf
      (dorg-history-mode)
      (setq dorg-history--origin state)
      (setq tabulated-list-entries
            (dorg-history--rows (plist-get state :dolt-dir)
                                (plist-get state :root-id)
                                (plist-get state :scope-id)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;;###autoload
(defun dorg-history (&optional root-scope)
  "Browse the Dolt history of the bead at point (or the whole root).
With a prefix arg ROOT-SCOPE, browse the whole registered tree's history
instead of the bead at point.  Opens `*dorg-history*'; press `r' to restore
the commit at point, `RET'/`v' to view it read-only, `g' to refresh, `q' to
quit."
  (interactive "P")
  ;; Strict: history browses/restores as-of reads against the Dolt store and
  ;; must scope them to a REAL root_id.  Pure source classification yields a
  ;; nil root_id for a keyword Dolt buffer with no stored binding, which would
  ;; mis-scope every subsequent as-of read/restore, so resolve through the
  ;; ensure path (binding if necessary).
  (let* ((reg (dorg--ensure-registration))
         (backend (and reg (dorg-registration-backend reg)))
         (dolt-dir (or (and backend (dorg-backend-dolt-dir backend))
                       (user-error "dorg: this buffer is not bound to a Dolt repo")))
         (root-id (and backend (dorg-backend-root-id backend)))
         (scope-id (unless root-scope (dorg-id-at-point))))
    (dorg-history--render-buffer
     (list :dolt-dir dolt-dir :root-id root-id
           :scope-id scope-id :buffer (current-buffer)))))

(defun dorg-history-refresh ()
  "Refresh the `*dorg-history*' listing from the backing repo."
  (interactive)
  (unless dorg-history--origin
    (user-error "dorg: not in a *dorg-history* buffer"))
  (dorg-history--render-buffer dorg-history--origin))

(defun dorg-history-view ()
  "View the commit at point read-only (bead scope) via the VFS island path."
  (interactive)
  (let* ((state dorg-history--origin)
         (commit (tabulated-list-get-id)))
    (unless (and state commit)
      (user-error "dorg: no commit at point"))
    ;; Ensure the /dorg: file-name handler is installed before visiting the
    ;; island path.  `dorg-vfs-enable' is idempotent (add-to-list) and
    ;; install-and-leave: the read-only buffer relies on the handler for
    ;; follow-on modtime/revert file operations.  Without this, a default session (no
    ;; manual M-x dorg-vfs-enable) fails with "file does not exist".
    (dorg-vfs-enable)
    (find-file-read-only
     (dorg-history--view-path (plist-get state :dolt-dir)
                              (plist-get state :scope-id)
                              commit))))

(defun dorg-history-restore-at-point ()
  "Restore the scope to the commit at point (durable DB write), with confirm."
  (interactive)
  (let* ((state dorg-history--origin)
         (commit (tabulated-list-get-id)))
    (unless (and state commit)
      (user-error "dorg: no commit at point"))
    (when (yes-or-no-p
           (format "Restore %s to commit %s? "
                   (if (plist-get state :scope-id)
                       (format "bead %s" (plist-get state :scope-id))
                     "the whole root")
                   (dorg-history--short commit)))
      (dorg-history--restore (plist-get state :dolt-dir)
                             (plist-get state :root-id)
                             (plist-get state :scope-id)
                             commit
                             (plist-get state :buffer))
      (dorg-history-refresh)
      (message "dorg: restored to %s" (dorg-history--short commit)))))

(provide 'dorg-history)
;;; dorg-history.el ends here
