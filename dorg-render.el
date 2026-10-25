;;; dorg-render.el --- Dolt to org buffer renderer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Renderer for reconstructing an org buffer from a dorg Dolt database.
;; Inverse of `dorg-parse-buffer' — reads SQL and emits org text.
;;
;; Delegates to model layer: `dorg-model-load-tree' loads the DB into
;; `dorg-heading' structs with EIEIO items, then `dorg-model-render-tree'
;; produces org text.
;;
;; The DB-loading path (`dorg-render') injects `#+DORG_DOLT_DIR:' and
;; `#+DORG_HASH:' as keyword items on the root heading before
;; rendering, so the output is a complete dorg-aware org buffer.
;; The pure model path (`dorg-model-render-tree') renders whatever
;; items are on the headings without special-casing.
;;
;; Multi-file: all queries are scoped by `root_id'.  If no root-id is
;; given, the unique root heading is discovered via
;; `dorg-render--resolve-root-id'.
;;
;; Main entry points:
;; - `dorg-render' — query Dolt repo, return org text as string
;; Buffer lifecycle is owned by `dorg-materialize'.

;;; Code:

(require 'cl-lib)
(require 'dorg-backend)                   ; `dorg-dolt-dir-keyword' / `dorg-hash-keyword'
(require 'dorg-dolt)
(require 'dorg-ext)
(require 'dorg-model)
(require 'dorg-model-load)               ; `dorg-model-load-tree' / `-headings'
(require 'dorg-sql-gen)                  ; `dorg-sql-gen--as-of-suffix'
(require 'dorg-projection)               ; established projection-view state

;;; Root ID Resolution

(defun dorg-render--resolve-root-id (dolt-dir)
  "Find the unique synthetic root heading id in DOLT-DIR.
Signals an error if zero or multiple roots are present."
  (let ((rows (dorg-sql dolt-dir
                        "SELECT id FROM headings WHERE level_offset = 0 AND parent_id IS NULL")))
    (cond ((null rows)
           (error "dorg-render: no root heading in %s" dolt-dir))
          ((> (length rows) 1)
           (error "dorg-render: multiple roots in %s; specify root-id"
                  dolt-dir))
          (t (alist-get 'id (car rows))))))

;;; Public API

(defun dorg-render--load-tree (dolt-dir root-id)
  "Load the clean whole-tree for DOLT-DIR at HEAD.
Returns a list (RID COMMIT TREE).  TREE is the id -> `dorg-heading'
hash exactly as `dorg-model-load-tree' returns it — no render-only
keyword injection — so it doubles as reusable sync-state base headings.
RID is resolved from the unique root heading when ROOT-ID is nil.
Signals an error if the repo has zero or multiple roots."
  (let* ((rid (or root-id (dorg-render--resolve-root-id dolt-dir)))
         (commit (dorg-head-commit dolt-dir))
         (tree (dorg-model-load-tree dolt-dir rid commit)))
    (list rid commit tree)))

(defun dorg-render--warn-unreachable (tree rid)
  "Warn about headings in TREE not root-reachable under RID.
These are the headings `dorg-model-render-tree' will silently omit.
Uses true root reachability (not immediate-parent existence) so
disconnected islands and parent cycles are reported, not just dangling
parents."
  (let ((parents (make-hash-table :test 'eql))
        (dangling nil) (cycles nil) (disconnected nil))
    (maphash (lambda (id h)
               (puthash id (dorg-heading-parent-id h) parents))
             tree)
    (maphash
     (lambda (id _h)
       (pcase (dorg-model-heading-reachability id parents rid)
         ('reachable nil)
         ('dangling-parent (push id dangling))
         ('cycle (push id cycles))
         ('disconnected (push id disconnected))))
     tree)
    (when (or dangling cycles disconnected)
      (let ((omitted (sort (append dangling cycles disconnected) #'<))
            (detail (lambda (label ids)
                      (when ids
                        (format " %s: %s;" label
                                (mapconcat #'number-to-string
                                           (sort ids #'<) ", "))))))
        (display-warning
         'dorg
         (format "Render: %d heading(s) not root-reachable, omitted: %s\n%s%s%s\nRun `dorg-doctor' to diagnose."
                 (length omitted)
                 (mapconcat #'number-to-string omitted ", ")
                 (or (funcall detail "dangling-parent" dangling) "")
                 (or (funcall detail "cycle" cycles) "")
                 (or (funcall detail "disconnected" disconnected) ""))
         :warning)))))

(defun dorg-render--classify-placements (tree rid)
  "Consult placement invariants for already-stored level-1 headings in TREE.
TREE is an id -> `dorg-heading' hash; RID the level-0 root id.  This is the
render-side secondary guard for tracked-regression: a promoted orphan (a stored
heading at level=1/parent=root that an owning invariant classifies corrupt)
is root-reachable, so `dorg-render--warn-unreachable' cannot catch it and the
renderer would silently re-emit it as a top-level sibling forever.

For each non-root heading sitting directly under RID at level 1, build a
`dorg-ext-placement-context' and call `dorg-ext-classify-placement'.  On a
`quarantine' verdict, emit a classified `dorg-placement-diagnostic' (content
is preserved — the heading is still rendered; the guard diagnoses, it does
not drop data).  Inert when no invariant is registered or none has an
opinion.

At render there is no distinct prior lineage to compare, so the emitted
at-root placement is presented as the buffer side and the heading's own id
as a non-root `stored-parent-id' marker (i.e. \"was not a legitimate root
child\").  A real invariant classifies from its SNAPSHOT model + the at-root
placement; the marker only unlocks shape checks that key on a non-root
origin."
  (maphash
   (lambda (id h)
     ;; A direct root child one level down: offset 1 under the root is
     ;; effective level 1, and needs no derived level.
     (when (and (eql (dorg-heading-level-offset h) 1)
                (eql (dorg-heading-parent-id h) rid))
       (let* ((context (make-dorg-ext-placement-context
                        :heading-id id :dorg-id id :root-id rid
                        :stored-parent-id id :stored-level nil
                        :buffer-parent-id rid :buffer-level 1
                        :move-intent nil
                        :snapshot (dorg-model-heading-property-snapshot h)
                        :source-kind nil))
              (verdict (dorg-ext-classify-placement context)))
         (when (and verdict
                    (eq (dorg-ext-placement-verdict-action verdict) 'quarantine))
           (dorg-placement-diagnostic-emit
            (make-dorg-placement-diagnostic
             :classification (dorg-ext-placement-verdict-classification verdict)
             :heading-id id :dorg-id id
             :stored-parent-id id :stored-level nil
             :buffer-parent-id rid :buffer-level 1
             :reason (dorg-ext-placement-verdict-reason verdict)))))))
   tree))

(defun dorg-render--tree-to-text (tree rid commit dolt-dir)
  "Render TREE to an org-mode text string WITHOUT mutating TREE.
Injects `#+DORG_DOLT_DIR:' / `#+DORG_HASH:' onto a COPY of the root
heading, so TREE's structs stay the clean DB state and remain reusable
as sync-state base headings.  COMMIT is the HEAD hash; RID the root id."
  (dorg-render--warn-unreachable tree rid)
  (dorg-render--classify-placements tree rid)
  ;; Build the render list, replacing the root with an injected copy.
  (let ((headings nil))
    (maphash
     (lambda (_id h)
       (if (dorg-model-root-p h)
           (let ((root-copy (copy-dorg-heading h)))
             (setf (dorg-heading-items root-copy)
                   (append
                    (list (dorg-keyword-item
                           :keyword dorg-dolt-dir-keyword
                           ;; Render the real on-disk store spelling, not a
                           ;; case-alias the caller may have passed
                           ;; (tracked-regression).  Covers the direct materialize
                           ;; path and the Downstream tracker batch refresh, which both
                           ;; render through here.
                           :value (file-name-as-directory
                                   (dorg--canonical-existing-case
                                    (file-truename dolt-dir))))
                          (dorg-keyword-item
                           :keyword dorg-hash-keyword
                           :value commit))
                    (dorg-heading-items h)))
             (push root-copy headings))
         (push h headings)))
     tree)
    (dorg-model-render-tree headings)))

(defun dorg-render (dolt-dir &optional root-id)
  "Render the dorg repo at DOLT-DIR as an org-mode text string.
If ROOT-ID is nil, the unique root heading is auto-detected.
Injects `#+DORG_DOLT_DIR:' and `#+DORG_HASH:' as keyword items on the
root heading so the output is a complete dorg-aware org buffer.
Signals an error if the repo has zero or multiple roots."
  (pcase-let ((`(,rid ,commit ,tree)
               (dorg-render--load-tree dolt-dir root-id)))
    (dorg-render--tree-to-text tree rid commit dolt-dir)))

(defun dorg-render--org-element-cache-reset-no-persistence ()
  "Reset the current Org element cache without registering persistence.
Org 9.7 accepts a NO-PERSISTENCE argument.  Older Org builds do not, so
  fall back to the older signature when necessary."
  (when (fboundp 'org-element-cache-reset)
    (condition-case nil
        (org-element-cache-reset nil t)
      (wrong-number-of-arguments
       (org-element-cache-reset nil)))))

(defun dorg-render--replace-buffer-text (text)
  "Replace the current buffer contents with TEXT.
Keep the deliberate linear erase+insert strategy.  In already-Org buffers
with the Org element cache enabled, suppress per-edit cache hooks during the
whole-buffer splice and reset the cache once afterward, without touching
org-persist.  Preserve Emacs' modified bit semantics: a silent splice still
marks the buffer modified when the rendered text differs."
  (if (and (derived-mode-p 'org-mode)
           (bound-and-true-p org-element-use-cache)
           (fboundp 'org-element-cache-reset))
      (let ((changed (or (/= (buffer-size) (length text))
                         (not (string= text
                                       (buffer-substring-no-properties
                                        (point-min) (point-max)))))))
        (with-silent-modifications
          (erase-buffer)
          (insert text))
        (when changed
          (set-buffer-modified-p t))
        (dorg-render--org-element-cache-reset-no-persistence))
    (erase-buffer)
    (insert text)))

;;; Partial Materialized Views — Read Primitives

;; A partial view renders a selected *island* (a heading plus its full
;; subtree) together with its *ancestor path* for context, scoped by
;; `root_id', without loading the whole tree's payload.  The whole-root
;; `dorg-render' above remains the degenerate
;; full view (island = root, no ancestors).
;;
;; Id discovery uses recursive CTEs (dolt is MySQL-8 compatible) so only
;; the island and spine rows are touched, not the entire tree.

(define-error 'dorg-render-error "dorg render failed")

(defun dorg-render--node-root-id (dolt-dir node-id &optional as-of)
  "Return the `root_id' of heading NODE-ID in DOLT-DIR, or nil if absent.
AS-OF, when non-nil, selects a historical commit."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows (dorg-sql dolt-dir
                         (format "SELECT root_id FROM headings%s WHERE id = %d"
                                 sfx node-id))))
    (alist-get 'root_id (car rows))))

(defun dorg-render-subtree-ids (dolt-dir root-id node-id &optional as-of)
  "Return ids of NODE-ID plus all descendants, scoped to ROOT-ID.
Order is unspecified.  Empty list when NODE-ID is absent from ROOT-ID.
AS-OF, when non-nil, selects a historical commit."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows (dorg-sql
                dolt-dir
                (format (concat
                         "WITH RECURSIVE sub AS ("
                         " SELECT id, parent_id FROM headings%s"
                         " WHERE id = %d AND root_id = %d"
                         " UNION ALL"
                         " SELECT h.id, h.parent_id FROM headings%s h"
                         " JOIN sub ON h.parent_id = sub.id"
                         " WHERE h.root_id = %d)"
                         " SELECT id FROM sub")
                        sfx node-id root-id sfx root-id))))
    (mapcar (lambda (r) (alist-get 'id r)) rows)))

(defun dorg-render--ancestor-ids (dolt-dir root-id node-id &optional as-of)
  "Return ancestor ids of NODE-ID in root→parent order, excluding NODE-ID.
Scoped to ROOT-ID.  Empty when NODE-ID is the root or has no ancestors.
AS-OF, when non-nil, selects a historical commit."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows (dorg-sql
                dolt-dir
                (format (concat
                         ;; Order by recursion depth (0 at the node, growing
                         ;; toward the root), never by a stored level: an
                         ;; offset is local and says nothing about depth.
                         "WITH RECURSIVE anc AS ("
                         " SELECT id, parent_id, 0 AS depth FROM headings%s"
                         " WHERE id = %d AND root_id = %d"
                         " UNION ALL"
                         " SELECT h.id, h.parent_id, anc.depth + 1 FROM headings%s h"
                         " JOIN anc ON anc.parent_id = h.id"
                         " WHERE h.root_id = %d)"
                         " SELECT id FROM anc WHERE id <> %d ORDER BY depth DESC")
                        sfx node-id root-id sfx root-id node-id))))
    (mapcar (lambda (r) (alist-get 'id r)) rows)))

(defun dorg-render--load-ancestor-spine (dolt-dir root-id ancestor-ids
                                                  &optional as-of)
  "Load ANCESTOR-IDS from DOLT-DIR as a headline-only id → `dorg-heading' hash.
Scoped to ROOT-ID.  Only headline data (the `headings' table plus tags)
is fetched — no item/body/drawer/planning payload — because ancestors
are breadcrumb context, not content.  Returns an empty hash table when
ANCESTOR-IDS is nil (a root island has no ancestors); never falls
through to a whole-tree load.  AS-OF, when non-nil, selects a historical
commit."
  (if (null ancestor-ids)
      (make-hash-table :test 'eql)
    (dorg-model-load-headings dolt-dir root-id ancestor-ids as-of t)))

;;; Partial Materialized Views — Render Primitives

(defun dorg-render--render-ancestor-context (ancestor-ids headings)
  "Render the breadcrumb heading lines for ANCESTOR-IDS as org text.
ANCESTOR-IDS is in root→parent order; HEADINGS is a hash
id → `dorg-heading'.  Each ancestor emits only its heading line (via
`dorg-model--render-heading' — no items, no synthetic DORG_ID drawer);
the level-0 root is skipped since it has no heading line.  Returns a
string ending in a newline, or \"\" when there are no non-root ancestors."
  (let ((lines nil))
    (dolist (id ancestor-ids)
      (when-let ((h (gethash id headings)))
        (unless (dorg-model-root-p h)
          (push (dorg-model--render-heading h) lines))))
    (if lines
        (concat (mapconcat #'identity (nreverse lines) "\n") "\n")
      "")))

(defun dorg-render--render-island-subtree (subtree-ids node-id headings)
  "Render the island rooted at NODE-ID from HEADINGS as org text.
SUBTREE-IDS scopes the parent→children map to the island; HEADINGS is a
hash id → `dorg-heading'.  The island renders in full (heading line +
items, with `:DORG_ID:' injected for non-root headings, plus all
descendants) via `dorg-model--render-subtree'.  Signals `dorg-render-error'
when NODE-ID is not present in HEADINGS."
  (let ((node (gethash node-id headings))
        (by-parent (make-hash-table :test 'eql)))
    (unless node
      (signal 'dorg-render-error
              (list (format "island node %s not loaded" node-id))))
    (dolist (id subtree-ids)
      (when-let ((h (gethash id headings)))
        (when-let ((pid (dorg-heading-parent-id h)))
          (puthash pid (append (gethash pid by-parent) (list h)) by-parent))))
    (maphash (lambda (k v)
               (puthash k (sort v (lambda (a b)
                                    (string< (dorg-heading-sort-key a)
                                             (dorg-heading-sort-key b))))
                        by-parent))
             by-parent)
    (with-temp-buffer
      (dorg-model--render-subtree node by-parent (current-buffer))
      (buffer-string))))

(defun dorg-render-island (dolt-dir node-id &optional root-id as-of)
  "Render the island at NODE-ID in DOLT-DIR as an org-mode text string.
The island is NODE-ID plus its full subtree, preceded by a breadcrumb of
NODE-ID's ancestor headings (headline-only context).  ROOT-ID is resolved
from NODE-ID when nil.  AS-OF, when non-nil, selects a historical commit.

Unlike `dorg-render', this does NOT inject `#+DORG_DOLT_DIR:' /
`#+DORG_HASH:' keywords: a partial view is a read-only projection of a DB
slice, not a registerable whole-tree buffer.  Signals `dorg-render-error'
when NODE-ID is not found under its root."
  (let ((rid (or root-id (dorg-render--node-root-id dolt-dir node-id as-of))))
    (unless rid
      (signal 'dorg-render-error
              (list (format "node %s not found in %s" node-id dolt-dir))))
    (let ((sub-ids (dorg-render-subtree-ids dolt-dir rid node-id as-of)))
      (unless sub-ids
        (signal 'dorg-render-error
                (list (format "island node %s not found under root %s"
                              node-id rid))))
      ;; Load the spine and the island separately so ancestors stay
      ;; headline-only: a combined load would fetch ancestor body/drawer/
      ;; planning payload that the breadcrumb never renders.
      (let* ((anc-ids (dorg-render--ancestor-ids dolt-dir rid node-id as-of))
             (anc-headings (dorg-render--load-ancestor-spine
                            dolt-dir rid anc-ids as-of))
             (island-headings (dorg-model-load-headings
                               dolt-dir rid sub-ids as-of)))
        (concat (dorg-render--render-ancestor-context anc-ids anc-headings)
                (dorg-render--render-island-subtree
                 sub-ids node-id island-headings))))))

;;; Partial Materialized Views — Bounded Window (tracked-case)
;;
;; A "window" is the whole-tree analogue of an island for MULTIPLE foci, WITH
;; the root preamble and a DURABLE `#+DORG_WINDOW:' marker.  Unlike an island
;; (a read-only VFS projection carrying no `#+DORG_*' keywords), a window is
;; written into an ordinary file-visiting `beads.org' that keeps the Dolt
;; binding.  The durable marker lets DOrg re-establish a private partial
;; materialized-view record on a fresh visit; absent out-of-window siblings
;; remain unknown rather than being misrepresented as part of the projection.

(defun dorg-render--window-marker-string (root-id focus-ids commit)
  "Return the `#+DORG_WINDOW:' value encoding ROOT-ID, FOCUS-IDS, COMMIT.
Read back by `dorg-render--restore-window-marker' to re-establish the
private partial-view record on a fresh visit."
  (format "root:%d foci:%s head:%s"
          root-id
          (mapconcat #'number-to-string focus-ids ",")
          commit))

(defun dorg-render--window-preamble (root root-id commit dolt-dir focus-ids)
  "Render the window's level-0 preamble string.
ROOT is the root `dorg-heading' (its config items — TODO sequence,
priorities, startup, user keywords — are preserved).  The injected
`#+DORG_DOLT_DIR:'/`#+DORG_HASH:'/`#+DORG_WINDOW:' keywords are prepended on
a COPY so ROOT is not mutated."
  (let ((root-copy (copy-dorg-heading root)))
    (setf (dorg-heading-items root-copy)
          (append
           (list (dorg-keyword-item
                  :keyword dorg-dolt-dir-keyword
                  ;; Real on-disk store spelling, not a case-alias (tracked-regression).
                  :value (file-name-as-directory
                          (dorg--canonical-existing-case
                           (file-truename dolt-dir))))
                 (dorg-keyword-item :keyword dorg-hash-keyword :value commit)
                 (dorg-keyword-item
                  :keyword "DORG_WINDOW"
                  :value (dorg-render--window-marker-string
                          root-id focus-ids commit)))
           (dorg-heading-items root)))
    (dorg-model-render-heading-section root-copy)))

(defun dorg-render--window (dolt-dir focus-ids &optional root-id as-of)
  "Render a bounded WINDOW of the repo at DOLT-DIR as an org-mode text string.
FOCUS-IDS is a non-empty list of heading ids rendered in full.  The window
carries the root preamble (root config items plus injected
`#+DORG_DOLT_DIR:'/`#+DORG_HASH:' and a durable `#+DORG_WINDOW:' marker), a
headline-only ancestor breadcrumb for the foci, and each focus subtree in
full; unrelated siblings are excluded.  ROOT-ID is resolved from the first
focus when nil; AS-OF selects a historical commit.

Unlike `dorg-render' this is a PARTIAL projection: headings outside the
window are absent-but-unknown, not deleted.  The `#+DORG_WINDOW:' marker lets
DOrg re-establish a private materialized-view record on a fresh visit.
Signals `dorg-render-error' when
FOCUS-IDS is empty or a focus is not found under the root."
  (unless focus-ids
    (signal 'dorg-render-error (list "window requires at least one focus id")))
  (let* ((rid (or root-id
                  (dorg-render--node-root-id dolt-dir (car focus-ids) as-of)))
         ;; Stamp the EXACT revision the rows are read at (tracked-regression): when
         ;; AS-OF selects a historical commit, DORG_HASH/DORG_WINDOW must label
         ;; that commit, not the current HEAD (which would split the snapshot).
         (commit (or as-of (dorg-head-commit dolt-dir))))
    (unless rid
      (signal 'dorg-render-error
              (list (format "window focus %s not found in %s"
                            (car focus-ids) dolt-dir))))
    ;; Per-focus subtree id sets (validates each focus resolves under RID).
    (let ((focus-subtrees (make-hash-table :test 'eql))
          (foci (delete-dups (copy-sequence focus-ids))))
      (dolist (fid foci)
        (let ((sub (dorg-render-subtree-ids dolt-dir rid fid as-of)))
          (unless sub
            (signal 'dorg-render-error
                    (list (format "window focus %s not found under root %s"
                                  fid rid))))
          (puthash fid sub focus-subtrees)))
      ;; Maximal foci: drop any focus contained in another focus's subtree so
      ;; an ancestor focus's full render is not duplicated by a nested one.
      (let* ((maximal
              (cl-remove-if
               (lambda (fid)
                 (cl-some (lambda (other)
                            (and (/= other fid)
                                 (memql fid (gethash other focus-subtrees))))
                          foci))
               foci))
             (subtree-ids
              (delete-dups
               (apply #'append
                      (mapcar (lambda (f) (gethash f focus-subtrees)) maximal))))
             (anc-ids
              (delete-dups
               (apply #'append
                      (mapcar (lambda (f)
                                (dorg-render--ancestor-ids dolt-dir rid f as-of))
                              maximal))))
             ;; Breadcrumb = ancestors that are neither the level-0 root nor
             ;; already rendered in full inside a focus subtree.
             (breadcrumb-ids
              (cl-remove-if (lambda (id) (or (= id rid) (memql id subtree-ids)))
                            anc-ids))
             (root-headings (dorg-model-load-headings dolt-dir rid (list rid) as-of))
             (root (gethash rid root-headings))
             (anc-headings (dorg-render--load-ancestor-spine
                            dolt-dir rid breadcrumb-ids as-of))
             (island-headings (dorg-model-load-headings
                               dolt-dir rid subtree-ids as-of))
             ;; Root->leaf breadcrumb order (by level); deterministic focus
             ;; order (by sort-key).
             (ordered-breadcrumb
              (sort (copy-sequence breadcrumb-ids)
                    (lambda (a b)
                      (< (dorg-heading-level (gethash a anc-headings))
                         (dorg-heading-level (gethash b anc-headings))))))
             (ordered-foci
              (sort (copy-sequence maximal)
                    (lambda (a b)
                      (string< (dorg-heading-sort-key (gethash a island-headings))
                               (dorg-heading-sort-key (gethash b island-headings)))))))
        (unless root
          (signal 'dorg-render-error
                  (list (format "window root %s not loaded in %s" rid dolt-dir))))
        (concat
         (dorg-render--window-preamble root rid commit dolt-dir focus-ids)
         (dorg-render--render-ancestor-context ordered-breadcrumb anc-headings)
         (mapconcat
          (lambda (fid)
            (dorg-render--render-island-subtree
             (gethash fid focus-subtrees) fid island-headings))
          ordered-foci
          ""))))))

(defun dorg-render--restore-window-marker (&optional buffer)
  "Re-establish a private partial record from a `#+DORG_WINDOW:' keyword.
Scans BUFFER's preamble (default current, the region before the first
heading) for a `#+DORG_WINDOW:' keyword and, when present, reconstructs the
partial materialized-view record so a freshly-visited windowed file is
push-protected before any sync.  Returns `partial', or nil when the buffer
carries no window keyword (a full materialization is left pushable)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (let ((limit (save-excursion
                       (goto-char (point-min))
                       (if (re-search-forward "^\\*+ " nil t)
                           (match-beginning 0)
                         (point-max)))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+DORG_WINDOW:[ \t]*\\(.*\\)$" limit t)
            (let* ((val (match-string 1))
                   (root (and (string-match "root:\\([0-9]+\\)" val)
                              (string-to-number (match-string 1 val))))
                   (foci (and (string-match "foci:\\([0-9,]+\\)" val)
                              (mapcar #'string-to-number
                                      (split-string (match-string 1 val) "," t))))
                   (commit (and (string-match "head:\\([^ \t]+\\)" val)
                                (match-string 1 val))))
              (when (and root foci)
                (dorg-projection-install-view
                 (current-buffer) (current-buffer) root commit 'partial foci)
                'partial))))))))

(provide 'dorg-render)
;;; dorg-render.el ends here
