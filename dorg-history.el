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

(defun dorg-history--item-to-value (item &optional omit-owned-metadata)
  "Serialize an EIEIO `dorg-heading-item' ITEM to a typed `:items' value plist.
Returns nil for a DOrg-owned item (a DORG_* keyword, or a drawer left empty
after dropping system DORG_* properties) so the restore never re-emits
protected metadata DOrg manages itself.  Registered owned-family properties
remain in the typed value: the normal typed boundary then routes them to their
owner, or refuses atomically when that owner is unavailable."
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
                  (lambda (kv)
                    (and (dorg-protected-keyword-p (car kv))
                         (or omit-owned-metadata
                             (not (dorg-owned-metadata-key-p (car kv))))))
                  (dorg-drawer-item-properties item)))
          (lines (dorg-drawer-item-lines item))
          (name (dorg-drawer-item-name item)))
      (cond (props (list :kind 'drawer :name name :properties props))
            (lines (list :kind 'drawer :name name :lines lines))
            (t nil))))
   (t (error "dorg-history: unknown heading-item type %S" item))))

(defun dorg-history--items-to-values (items &optional omit-owned-metadata)
  "Serialize a heading's ITEMS (EIEIO objects) to typed `:items' value plists,
dropping DOrg-owned items (see `dorg-history--item-to-value').  When
OMIT-OWNED-METADATA is non-nil, registered drawer properties are also omitted
so their owner, rather than the generic item replacement, supplies their
restore patches."
  (delq nil (mapcar (lambda (item)
                      (dorg-history--item-to-value item omit-owned-metadata))
                    items)))

(defun dorg-history--heading-owned-metadata-families (heading)
  "Return canonical registered families represented by HEADING's drawers."
  (let (families)
    (dolist (item (dorg-heading-items heading))
      (when (cl-typep item 'dorg-drawer-item)
        (dolist (cell (dorg-drawer-item-properties item))
          (when-let ((family (dorg-owned-metadata-family (car cell))))
            (cl-pushnew (car family) families :test #'equal)))))
    (sort families #'string<)))

(defun dorg-history--heading-owned-metadata-values (heading family)
  "Return HEADING's FAMILY drawer entries as stored key/value pairs."
  (let (values)
    (dolist (item (dorg-heading-items heading))
      (when (cl-typep item 'dorg-drawer-item)
        (dolist (cell (dorg-drawer-item-properties item))
          (when (equal family (car-safe (dorg-owned-metadata-family (car cell))))
            (push cell values)))))
    (nreverse values)))

(defun dorg-history--owned-metadata-restore-plan
    (current target base &optional current-image target-image scope-ids)
  "Delegate one heading's changed registered families from CURRENT to TARGET.

BASE identifies the snapshot being restored.  Each owner receives immutable
identity and before/after value lists, then returns its typed patches.  An
absent or refusing owner fails the entire restore before any persistence."
  (let ((heading (or target current))
        (families
         (delete-dups
          (append (and current
                       (dorg-history--heading-owned-metadata-families current))
                  (and target
                       (dorg-history--heading-owned-metadata-families target)))))
        plans)
    (dolist (family families)
      (let ((before (and current
                         (dorg-history--heading-owned-metadata-values current family)))
            (after (and target
                        (dorg-history--heading-owned-metadata-values target family))))
        (unless (equal before after)
          (let* ((participants
                  (condition-case err
                      (dorg-owned-metadata-participants family)
                    (error
                     (apply #'dorg-mutate--raise
                            'invalid-target "owned metadata owner refused restore"
                            :family family
                            (dorg-owned-metadata-normalize-owner-signal
                             family nil err)))))
                 (restore (and (listp participants) (plist-get participants :restore))))
            (unless (functionp restore)
              (dorg-mutate--raise 'invalid-target
                                  "owned metadata owner is unavailable"
                                  :family family :detail 'owner-unavailable))
            (let* ((context (list :family family
                                  :identity (list :heading :id (dorg-heading-id heading))
                                  :before before :after after :base base
                                  :current-image current-image
                                  :target-image target-image
                                  :scope-ids scope-ids))
                   (result
                    (condition-case err
                        (funcall restore context)
                      (error
                       (apply #'dorg-mutate--raise
                              'invalid-target "owned metadata owner refused restore"
                              :family family
                              (dorg-owned-metadata-normalize-owner-signal
                               family participants err)))))
                   (patches (and (listp result) (plist-get result :patches))))
              (unless (listp patches)
                (dorg-mutate--raise 'invalid-target
                                    "owned metadata owner refused restore"
                                    :family family :detail 'owner-refused
                                    :owner-message "owner returned no patches"
                                    :owner-reason nil))
              (push (list :family family :patches patches
                          :capability
                          (dorg-mutate--owner-capability--make
                           :family family
                           :request (list :action 'restore
                                          :scope-ids (copy-sequence scope-ids))
                           :execution-id
                           (make-symbol "dorg-owned-metadata-restore-")))
                    plans))))))
    (if (= (length plans) 1) (car plans) (nreverse plans))))

(defun dorg-history--owned-metadata-restore-plans
    (current target scope-ids base)
  "Return all owner restore plans for CURRENT -> TARGET within SCOPE-IDS."
  (let (plans)
    (dolist (id scope-ids)
      (let ((result (dorg-history--owned-metadata-restore-plan
                     (gethash id current) (gethash id target) base
                     current target scope-ids)))
        (cond ((null result))
              ((plist-member result :family) (push result plans))
              (t (setq plans (nconc (nreverse result) plans))))))
    (nreverse plans)))

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

(defun dorg-history--update-patches (cur tgt &optional omit-owned-metadata)
  "Return typed patches restoring CUR to TGT for an id in both trees.
A heading update is emitted when fields differ, plus a whole-items replacement
when the (DOrg-owned-stripped) item content differs.  An empty target body uses
descending occurrence deletes: the v2 item-all cell requires a nonempty body,
and descending order keeps each ordinal valid as the batch removes items."
  (let ((current-items (dorg-heading-items cur))
        (target-values (dorg-history--items-to-values
                        (dorg-heading-items tgt) omit-owned-metadata))
        (host (list :heading :id (dorg-heading-id tgt)))
        patches)
    (when (dorg-history--headline-differs-p cur tgt)
      (push (list :op 'update
                  :target (list :heading :id (dorg-heading-id tgt))
                  :fields (dorg-history--headline-fields tgt))
            patches))
    (setq patches (nreverse patches))
    (unless (equal (dorg-history--items-to-values current-items omit-owned-metadata)
                   target-values)
      (setq patches
            (append patches
                    (if target-values
                        (list (list :op 'replace
                                    :target (list :item :host host :select '(:all))
                                    :items target-values))
                      (cl-loop for item in current-items for n from 1
                               when (dorg-history--item-to-value item omit-owned-metadata)
                               collect (list :op 'delete
                                             :target (list :item :host host
                                                           :select (list :occurrence n)))
                               into deletes
                               finally return (nreverse deletes))))))
    patches))

(defun dorg-history--insert-patch (tgt &optional anchor omit-owned-metadata)
  "Return a private typed tree insert re-creating TGT exactly.
The internal :offset carries TGT's level offset, so a skipped level is
restored.  ANCHOR, when non-nil, is a (:before HOST) or (:after HOST) tail
placing TGT at its target sibling position."
  (let ((parent (dorg-heading-parent-id tgt)))
    (list :op 'insert
          :target (append (list :tree :under
                                (if parent (list :heading :id parent) :root))
                          anchor
                          (list :offset (dorg-heading-level-offset tgt)))
          :heading
          (append (list :id (dorg-heading-id tgt))
                  (dorg-history--headline-fields tgt)
                  (list :items
                        (dorg-history--items-to-values
                         (dorg-heading-items tgt) omit-owned-metadata))))))

(defun dorg-history--fans (headings)
  "Return a hash from parent id to HEADINGS' child ids in sort-key order."
  (let ((fans (make-hash-table :test 'eql)))
    (maphash (lambda (id h)
               (when (dorg-heading-parent-id h)
                 (push (cons (or (dorg-heading-sort-key h) "") id)
                       (gethash (dorg-heading-parent-id h) fans))))
             headings)
    (maphash (lambda (parent pairs)
               (puthash parent
                        (mapcar #'cdr (sort pairs (lambda (a b)
                                                    (string< (car a) (car b)))))
                        fans))
             fans)
    fans))

(defun dorg-history--anchor (id fan pred-p &optional succ-p)
  "Return an anchor tail keeping ID at its place in FAN, an ordered id list.
The anchor is (:after P) for the nearest preceding id satisfying PRED-P,
else (:before S) for the nearest following id satisfying SUCC-P (default
PRED-P), else nil (ID alone)."
  (let* ((tail (memq id fan))
         (before (cl-loop for x in fan until (eql x id) collect x))
         (pred (cl-find-if pred-p (reverse before)))
         (succ (cl-find-if (or succ-p pred-p) (cdr tail))))
    (cond (pred (list :after (list :heading :id pred)))
          (succ (list :before (list :heading :id succ))))))

(defun dorg-history--offset-move-patch (cur tgt current-fans present-p)
  "Return a same-parent move restoring TGT's level offset onto CUR.
The move is anchored to CUR's current position among its PRESENT-P siblings
in CURRENT-FANS, so only the offset changes, through the single move door."
  (let ((id (dorg-heading-id cur))
        (parent (dorg-heading-parent-id cur)))
    (list :op 'move
          :target (list :subtree :id id)
          :to (append (list :under (list :heading :id parent))
                      (dorg-history--anchor
                       id (gethash parent current-fans)
                       (lambda (x) (and (not (eql x id)) (funcall present-p x))))
                      (list :offset (dorg-heading-level-offset tgt))))))

(defun dorg-restore--patches (current-ht target-ht scope-ids
                                          &optional omit-owned-metadata)
  "Return typed patches transforming CURRENT-HT into
TARGET-HT within SCOPE-IDS.

For each id in SCOPE-IDS:
  - in CURRENT only        -> typed heading delete (emitted leaf-up),
  - in TARGET only         -> typed tree insert preserving id+items+parent
                              and level offset, anchored at its target
                              sibling position (emitted root-down),
  - in both, content differ -> typed heading update and/or items replace,
  - in both, same parent, offset differs -> a same-parent move restoring
                              the offset in place,
  - unchanged              -> no op.
A parent or order difference is not restored.
Deletes are ordered deepest-first and inserts shallowest-first (by tree
depth) so parent invariants hold when the plan is applied in order.  Offset
moves come after the deletes: decreases last sibling first, then increases
first sibling first, so every intermediate fan stays representable as Org.
Inserts of one fan run in target sibling order, so each one's anchor already
exists.  A step Org cannot represent is refused before anything is written."
  (let ((current-fans (dorg-history--fans current-ht))
        (target-fans (dorg-history--fans target-ht))
        (deleted (make-hash-table :test 'eql))
        (inserted (make-hash-table :test 'eql))
        deletes inserts updates lowered raised)
    (dolist (id scope-ids)
      (let ((cur (gethash id current-ht))
            (tgt (gethash id target-ht)))
        (cond ((and cur (null tgt)) (puthash id t deleted))
              ((and tgt (null cur)) (puthash id t inserted)))))
    (dolist (id scope-ids)
      (let ((cur (gethash id current-ht))
            (tgt (gethash id target-ht)))
        (cond
         ((and cur (null tgt))
          (push (cons (dorg-heading-level cur)
                      (list :op 'delete :target (list :heading :id id)))
                deletes))
         ((and tgt (null cur))
          (let* ((parent (dorg-heading-parent-id tgt))
                 (present-p
                  (lambda (x)
                    (or (gethash x inserted)
                        (let ((c (gethash x current-ht)))
                          (and c (not (gethash x deleted))
                               (eql (dorg-heading-parent-id c) parent)))))))
            (push (list (dorg-history--depth id target-ht)
                        (or (dorg-heading-sort-key tgt) "")
                        (dorg-history--insert-patch
                         tgt (and parent
                                  (dorg-history--target-anchor
                                   id (gethash parent target-fans) present-p
                                   inserted))
                         omit-owned-metadata))
                  inserts)))
         ((and cur tgt)
          (setq updates
                (nconc updates (dorg-history--update-patches
                                cur tgt omit-owned-metadata)))
          (when (and (eql (dorg-heading-parent-id cur) (dorg-heading-parent-id tgt))
                     (dorg-heading-parent-id cur)
                     (not (eql (dorg-heading-level-offset cur)
                               (dorg-heading-level-offset tgt))))
            (let ((move (cons (or (dorg-heading-sort-key cur) "")
                              (dorg-history--offset-move-patch
                               cur tgt current-fans
                               (lambda (x) (not (gethash x deleted)))))))
              (if (< (dorg-heading-level-offset tgt)
                     (dorg-heading-level-offset cur))
                  (push move lowered)
                (push move raised))))))))
    (nconc
     ;; deletes deepest-first (leaf-up)
     (mapcar #'cdr (sort deletes (lambda (a b) (> (car a) (car b)))))
     updates
     (mapcar #'cdr (sort lowered (lambda (a b) (string< (car b) (car a)))))
     (mapcar #'cdr (sort raised (lambda (a b) (string< (car a) (car b)))))
     ;; inserts shallowest-first by tree depth (root-down; an effective
     ;; level would split a gapped fan), then in target sibling order
     (mapcar #'caddr
             (sort inserts (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (string< (cadr a) (cadr b))))))))))

(defun dorg-history--target-anchor (id fan present-p inserted)
  "Return the anchor tail placing inserted ID at its place in target FAN.
A preceding sibling anchors (:after P) when it will exist at insert time:
PRESENT-P holds for it, and an INSERTED one is inserted earlier because a
fan's inserts run in target order.  Otherwise the nearest following sibling
that already exists (PRESENT-P but not INSERTED) anchors (:before S)."
  (dorg-history--anchor
   id fan present-p
   (lambda (x) (and (funcall present-p x) (not (gethash x inserted))))))

(defun dorg-history--depth (id headings)
  "Return ID's number of ancestors in HEADINGS, bounded by their count."
  (let ((depth 0)
        (limit (hash-table-count headings))
        (h (gethash id headings)))
    (while (and h (dorg-heading-parent-id h) (< depth limit))
      (setq depth (1+ depth)
            h (gethash (dorg-heading-parent-id h) headings)))
    depth))

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
             ;; Generic history restoration deliberately excludes registered
             ;; drawer values.  Their typed owner receives immutable before /
             ;; after values first and supplies its own patches plus private
             ;; plan authority; no unavailable owner can silently degrade to
             ;; an ordinary item replacement.
             (owner-plans
              (dorg-history--owned-metadata-restore-plans
               current target scope-ids base))
             (patches
              (append (dorg-restore--patches current target scope-ids t)
                      (apply #'append (mapcar (lambda (plan)
                                                (plist-get plan :patches))
                                              owner-plans))))
             (owner-capabilities
              (mapcar (lambda (plan) (plist-get plan :capability)) owner-plans)))
        (if (null patches)
            (progn (message "dorg: already at that state") nil)
          (if owner-capabilities
              ;; Only an owner restore needs the trusted internal authority
              ;; carrier.  Keep ordinary history restores on their long-lived
              ;; public entrypoint, including its retry/receipt seam.
              (dorg-mutate--apply-patches-internal
               (list :dolt-dir dolt-dir :root-id root-id)
               patches nil owner-capabilities
               :base-commit base
               :message (or message
                            (dorg-history--default-message scope-id commit)))
            (dorg-mutate--apply-patches
             (list :dolt-dir dolt-dir :root-id root-id)
             patches nil
             :base-commit base
             :message (or message
                          (dorg-history--default-message scope-id commit)))))))))

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
