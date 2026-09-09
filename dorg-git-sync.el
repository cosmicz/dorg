;;; dorg-git-sync.el --- Plain-org git sync-branch adapter for DOrg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; DOrg-owned git sync branch for plain-org stores.  This is the
;; backend-neutral *store-publish* mechanism: a plain-org root's
;; authority Org file (and any sibling source files) are mirrored into a
;; configured git branch.  It absorbs the plain-org half of a tracker's
;; git sync-branch feature into DOrg's storage layer.
;;
;; `dorg-git-sync' is deliberately an explicit store-publish adapter, not a
;; buffer lifecycle: projection refresh and typed mutation remain separate.
;;
;; The surface is a synchronous data API: a caller (e.g. a tracker's CLI
;; wrappers, during migration) builds a `dorg-git-sync-config' and calls
;; `dorg-git-sync', which returns a structured `dorg-git-sync-result'.
;; DOrg never globs the cwd, never reads implicit globals, and never
;; depends on any tracker-specific feature.
;;
;; Sync mode is explicit (`mode' slot): `:sync-branch', `:current-branch',
;; or `:disabled'.  `branch' is consulted ONLY in `:sync-branch' mode, so
;; a nil branch is never overloaded to mean "current branch" or "off".
;;
;; Main API:
;; - `dorg-git-sync'        - Mirror the source files per CONFIG's mode
;; - `dorg-git-sync-push'   - Push the effective branch to a remote
;; - `dorg-git-sync-status' - Diff/log summary between sync and current branch

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'dorg-git)
(require 'dorg-keywords)
(require 'dorg-registry)

;;; ─── Config & Result ──────────────────────────────────────────────────

(cl-defstruct (dorg-git-sync-config
               (:constructor dorg-git-sync-config-create))
  "Explicit, implicit-free configuration for a plain-org git sync.
SYNC MODE is the sole directive; `branch' is never used to infer intent.

Slots:
  mode              REQUIRED.  Exactly one of:
                    `:sync-branch'    mirror `source-files' into a dedicated
                                      sync-branch worktree; `branch' MUST
                                      name that branch.
                    `:current-branch' commit `source-files' in place on the
                                      repository's CURRENT branch; `branch'
                                      is ignored.
                    `:disabled'       no-op; always returns :skipped :disabled.
                    Any other value (including nil) is a misconfiguration and
                    returns :skipped :misconfigured.
  root-file         ABS path of the authority Org file (required).
  source-files      explicit list of ABS files to mirror (required; the
                    caller derives them — DOrg never globs a directory).
  branch            sync branch name; consulted ONLY when mode is
                    `:sync-branch' (required there, ignored otherwise).
  sparse-subdir     subdir to cone-sparse-checkout in the sync worktree.
  push              non-nil => push after a successful commit.
  push-remote       remote name (default \"origin\").
  push-branch       branch to push (default = effective committed branch).
  message           commit message string (the caller pre-formats it).
  identity-keywords ordered root keyword names establishing document
                    identity; the FIRST present value on each side is the
                    identity.  DOrg default is (\"DORG_STORE_ID\").
  source-content-overrides
                    alist mapping normalized absolute source paths to
                    already-materialized Org text for :sync-branch mirror
                    writes.
  sync-worktree-path
                    optional ABS path of a caller-owned sync-branch
                    worktree to reuse in `:sync-branch' mode.  When nil,
                    DOrg uses its common-dir-derived checkout or safely
                    adopts an existing registered same-repo checkout of
                    `branch'.
  backend-type      caller-declared storage backend of the root.  `dolt'
                    marks the root Dolt-backed regardless of any preamble
                    marker; nil defers to content classification.
  allow-dolt-export non-nil opts a Dolt-backed root into a one-way derived
                    snapshot export.  Honored ONLY in `:sync-branch' mode;
                    a Dolt root without it never publishes to Git."
  mode
  root-file
  source-files
  branch
  sparse-subdir
  push
  (push-remote "origin")
  push-branch
  message
  (identity-keywords '("DORG_STORE_ID"))
  source-content-overrides
  sync-worktree-path
  backend-type
  allow-dolt-export)

(cl-defstruct (dorg-git-sync-result
               (:constructor dorg-git-sync-result-create))
  "Structured outcome of a sync operation — never a buried `message'.

Slots:
  status   `:committed' | `:skipped' | `:failed'.
  reason   machine reason: `:committed' `:no-changes' `:disabled'
           `:misconfigured' `:not-a-git-repo' `:identity-mismatch'
           `:worktree-contention' `:git-error' `:dolt-export-disabled'.
  commit   SHA on `:committed', else nil.
  branch   effective branch (sync branch in `:sync-branch' mode, current
           branch in `:current-branch' mode).
  identity (:source S :mirror M) when an identity check ran.
  detail   human-readable string.
  warnings list of (:kind K :detail D [:path P]) sub-records."
  status
  reason
  commit
  branch
  identity
  detail
  warnings)

;;; ─── Write Guard (test seam) ──────────────────────────────────────────

;; KEEP (test-seam): let-bound by scratch/benchmark harnesses to fence
;; mirror writes to an isolated root; nil in production so behavior is
;; unchanged.  Mirrors `dorg--binding-keyword-write-guard'.
(defvar dorg-git-sync--write-guard nil
  "When non-nil, restrict where mirror destination files may be written.
Default nil means unrestricted — production behavior is unchanged.  When
set to a directory string, or a list of directory strings, a mirror copy
whose destination is NOT under one of those directories signals an error
before any copy.  Paths are compared by `file-truename'.")

(defun dorg-git-sync--write-allowed-p (file)
  "Return non-nil when writing mirror FILE is permitted.
Always permitted when `dorg-git-sync--write-guard' is nil.  When armed,
FILE must reside under one of its allowed roots, compared by
`file-truename'.  A nil FILE is permitted."
  (let ((roots dorg-git-sync--write-guard))
    (or (null roots)
        (null file)
        (let ((truename (file-truename file)))
          (cl-some (lambda (root)
                     (string-prefix-p
                      (file-name-as-directory (file-truename root))
                      truename))
                   (if (listp roots) roots (list roots)))))))

(defun dorg-git-sync--write-check (file)
  "Signal an error when writing mirror FILE is disallowed by the guard."
  (unless (dorg-git-sync--write-allowed-p file)
    (error "dorg-git-sync: refusing to write mirror file into %s (outside guard roots %S)"
           file dorg-git-sync--write-guard)))

;;; ─── Warning accumulator ──────────────────────────────────────────────

(defvar dorg-git-sync--warnings nil
  "Dynamic accumulator of warning sub-records for the in-flight sync call.
`dorg-git-sync' / `dorg-git-sync-push' let-bind this to nil; the result
builder drains it into the result's `warnings' slot.")

(defun dorg-git-sync--warn (kind detail &rest plist)
  "Record a structured warning of KIND with DETAIL into the accumulator.
PLIST may carry context such as `:path FILE'."
  (push (append (list :kind kind :detail detail) plist)
        dorg-git-sync--warnings))

(cl-defun dorg-git-sync--result (status reason &key commit branch identity detail)
  "Build a `dorg-git-sync-result', draining the warning accumulator."
  (dorg-git-sync-result-create
   :status status :reason reason :commit commit :branch branch
   :identity identity :detail detail
   :warnings (reverse dorg-git-sync--warnings)))

;;; ─── Identity guard ───────────────────────────────────────────────────

(defun dorg-git-sync--identity-of (kvs identity-keywords)
  "Return the FIRST present value among IDENTITY-KEYWORDS in KVS.
KVS is an ordered (KEYWORD . VALUE) alist as from `dorg-root-keywords'."
  (cl-loop for kw in identity-keywords
           for hit = (assoc kw kvs)
           when hit return (cdr hit)))

(defun dorg-git-sync--file-identity (file identity-keywords)
  "Return FILE's identity: first present value among IDENTITY-KEYWORDS.
Returns nil when FILE is missing or carries none of the keywords."
  (when (and file (file-exists-p file))
    (dorg-git-sync--identity-of
     (dorg-root-keywords (list :file file)) identity-keywords)))

(defun dorg-git-sync--string-identity (text identity-keywords)
  "Return the identity of org TEXT: first present value among IDENTITY-KEYWORDS."
  (when (and text (not (string-empty-p text)))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (dorg-git-sync--identity-of
       (dorg-root-keywords (list :buffer (current-buffer))) identity-keywords))))

(defun dorg-git-sync--identity-ok-p (source-id mirror-id)
  "Return non-nil unless SOURCE-ID and MIRROR-ID are both present and differ.
A first-ever sync (mirror absent) or an identity-less source is permitted;
only a concrete mismatch is refused (the foreign-root incident lesson).

String identities compare CASE-INSENSITIVELY: case variants such as
`ABCD' and `abcd' name the same document root (the case-corruption
incident lesson).  The comparison is read-only — raw
source and mirror spelling is never rewritten, so durable data and
result payloads keep their original case.  Unexpected non-string values
retain conservative exact equality rather than being coerced."
  (not (and source-id mirror-id
            (not (if (and (stringp source-id) (stringp mirror-id))
                     (string-equal-ignore-case source-id mirror-id)
                   (equal source-id mirror-id))))))

;;; ─── Dolt materialization publication policy ──────────────────────────

;; Operator policy (tracked-case): ordinary Git publication of a Dolt-backed
;; DOrg materialization is DEFAULT-DENIED.  Dolt is the authority; the Org
;; file mirrored to Git is a derived snapshot.  Publishing it by default
;; would let a stale/one-directional export masquerade as source of truth
;; and drift from the Dolt database, whose own replication uses Dolt
;; remotes — not this adapter.  A caller must consciously opt in with
;; `allow-dolt-export', and even then only onto a dedicated `:sync-branch'.

;; The set of Dolt-marker keyword names (current + legacy) is owned by the
;; reader in `dorg-registry' (`dorg--dolt-dir-marker-keywords'); we consume it
;; rather than relist the legacy spelling here, keeping that literal confined
;; to its single sanctioned home.  Only a genuine root-preamble occurrence
;; counts — a body/example literal under a heading is not a root binding (see
;; `dorg-root-keywords').

(defun dorg-git-sync--keywords-mark-dolt-p (kvs)
  "Return non-nil when root keyword alist KVS carries a Dolt marker.
A marker counts only when its root-preamble value is non-empty after
trimming whitespace, matching the bounded scanner semantics used by
downstream compatibility callers."
  (cl-some (lambda (kw)
             (when-let ((hit (assoc kw kvs)))
               (let ((value (cdr hit)))
                 (and (stringp value)
                      (not (string-empty-p (string-trim value)))))))
           dorg--dolt-dir-marker-keywords))

(defun dorg-git-sync--string-dolt-p (text)
  "Return non-nil when org TEXT's root preamble carries a Dolt marker."
  (when (and text (not (string-empty-p text)))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (dorg-git-sync--keywords-mark-dolt-p
       (dorg-root-keywords (list :buffer (current-buffer)))))))

(defun dorg-git-sync--file-dolt-p (file)
  "Return non-nil when FILE's root preamble carries a Dolt marker.
Returns nil when FILE is missing."
  (when (and file (file-exists-p file))
    (dorg-git-sync--keywords-mark-dolt-p
     (dorg-root-keywords (list :file file)))))

(defun dorg-git-sync--dolt-backed-p (config)
  "Return non-nil when CONFIG's effective root is Dolt-backed.
Classification, with NO Git access:
  - an explicit `backend-type' of `dolt' always wins; else
  - the effective root content is inspected for a genuine preamble
    marker — the root override bytes when present (the live caller path
    where the freshly rendered snapshot is in memory and disk may be
    stale), otherwise the disk file.
A caller never asserts `plain-org' to override a genuine Dolt marker."
  (or (eq (dorg-git-sync-config-backend-type config) 'dolt)
      (let* ((root-file (expand-file-name
                         (dorg-git-sync-config-root-file config)))
             (overrides (dorg-git-sync--normalize-source-content-overrides
                         (dorg-git-sync-config-source-content-overrides config)))
             (override (dorg-git-sync--source-content-override
                        root-file overrides)))
        (if (car override)
            (dorg-git-sync--string-dolt-p (cdr override))
          (dorg-git-sync--file-dolt-p root-file)))))

(defun dorg-git-sync--publication-decision (config)
  "Return the centralized Dolt-publication decision for CONFIG.
This is the SINGLE policy point; every entry point consults it BEFORE any
Git access so a Dolt-backed root is gated before repository/worktree/push
or status Git calls.  One of:

  :allow                 publication may proceed (a plain-org root, or a
                         Dolt root opted into export in `:sync-branch').
  :dolt-export-disabled  a Dolt-backed root without `allow-dolt-export'.
  :misconfigured         Dolt export opted in but not in `:sync-branch'
                         mode; a derived snapshot never lands on the
                         active code branch."
  (cond
   ((not (dorg-git-sync--dolt-backed-p config)) :allow)
   ((not (dorg-git-sync-config-allow-dolt-export config)) :dolt-export-disabled)
   ((eq (dorg-git-sync-config-mode config) :sync-branch) :allow)
   (t :misconfigured)))

(defun dorg-git-sync--denied-result (decision)
  "Build the `dorg-git-sync-result' for a non-`:allow' publication DECISION."
  (pcase decision
    (:dolt-export-disabled
     (dorg-git-sync--result
      :skipped :dolt-export-disabled
      :detail (concat "Dolt-backed root: Git materialization export is "
                      "disabled by default; pass :allow-dolt-export t on a "
                      "dedicated :sync-branch to publish a derived snapshot")))
    (:misconfigured
     (dorg-git-sync--result
      :skipped :misconfigured
      :detail (concat "Dolt export is permitted only in :sync-branch mode; "
                      "derived snapshots never land on the active code branch")))))

;;; ─── Path helpers ─────────────────────────────────────────────────────

(defun dorg-git-sync--repo-dir (config)
  "Return the working directory of CONFIG's root-file (the source repo)."
  (file-name-directory (expand-file-name (dorg-git-sync-config-root-file config))))

(defun dorg-git-sync--rel (file toplevel)
  "Return FILE's path relative to TOPLEVEL, resolving symlinks first.
Truename both sides so a /tmp vs /private/tmp split (macOS) does not
yield a spurious \"../\" relative path."
  (file-relative-name (file-truename (expand-file-name file))
                      (file-truename toplevel)))

(defun dorg-git-sync--normalize-source-content-overrides (overrides)
  "Return OVERRIDES keyed by normalized absolute source path.
OVERRIDES is an alist of source path to already-materialized Org text.
Path matching is exact after `expand-file-name'."
  (mapcar (lambda (entry)
            (cons (expand-file-name (car entry)) (cdr entry)))
          overrides))

(defun dorg-git-sync--source-content-override (file overrides)
  "Return (FOUND . TEXT) for FILE in normalized OVERRIDES.
FOUND is non-nil only when FILE has an explicit override.  A present
override with empty text is still present."
  (let ((key (expand-file-name file))
        found)
    (while (and overrides (not found))
      (let ((entry (pop overrides)))
        (when (equal key (car entry))
          (setq found (cons t (cdr entry))))))
    (or found (cons nil nil))))

(defun dorg-git-sync--source-identity (file identity-keywords overrides)
  "Return (OVERRIDE-PRESENT . IDENTITY) for FILE.
When FILE has an override, identity is read strictly from the override
text.  Disk identity is used only when no override is present."
  (let ((override (dorg-git-sync--source-content-override file overrides)))
    (if (car override)
        (cons t (dorg-git-sync--string-identity (cdr override)
                                                identity-keywords))
      (cons nil (dorg-git-sync--file-identity file identity-keywords)))))

(defun dorg-git-sync--copy-source-to-mirror (src dest overrides)
  "Copy SRC to DEST, using override text from OVERRIDES when present."
  (let ((override (dorg-git-sync--source-content-override src overrides)))
    (if (car override)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (cdr override) nil dest nil 'silent))
      (copy-file src dest t))))

;;; ─── Worktree management (mode :sync-branch only) ─────────────────────

(defconst dorg-git-sync--worktree-root "dorg-git-sync"
  "Name of the subdir, under the common git dir, holding sync worktrees.")

(defun dorg-git-sync--worktree-path (repo-dir branch)
  "Absolute path of the shared sync worktree for BRANCH in REPO-DIR's repo.
Located under the repository's COMMON git dir so every linked worktree
resolves to the SAME checkout — never a per-worktree duplicate of the
sync branch (linked-worktree incident lesson).  The path is derived from
BRANCH so distinct sync branches get distinct, still-shared checkouts.
Returns nil when REPO-DIR is not a git repository."
  (when-let ((common (dorg-git--common-dir repo-dir)))
    (expand-file-name
     (replace-regexp-in-string "/" "-" branch)
     (expand-file-name dorg-git-sync--worktree-root common))))

(defun dorg-git-sync--worktree-valid-p (repo-dir wt-path)
  "Return non-nil when WT-PATH is a live worktree of REPO-DIR's repository.
Directory existence is not enough: a repository directory move leaves the
checkout on disk while every `git -C WT-PATH' dies (fatal: not a git
repository).  Also rejects a checkout whose common dir resolves to some
other repository."
  (when (and wt-path (file-directory-p wt-path))
    (let ((result (dorg-git--run wt-path "rev-parse" "--git-common-dir"))
          (common (dorg-git--common-dir repo-dir)))
      (and (zerop (car result))
           common
           (file-equal-p (expand-file-name (cdr result) wt-path) common)))))

(defun dorg-git-sync--worktree-on-branch-p (repo-dir wt-path branch)
  "Return non-nil when WT-PATH is a same-repo worktree on BRANCH."
  (and (dorg-git-sync--worktree-valid-p repo-dir wt-path)
       (equal (dorg-git--current-branch wt-path) branch)))

(defun dorg-git-sync--worktree-contention-result (branch detail)
  "Build a structured resolver contention result for BRANCH with DETAIL."
  (dorg-git-sync--result :failed :worktree-contention
                         :branch branch
                         :detail detail))

(defun dorg-git-sync--validate-caller-worktree (repo-dir branch wt-path)
  "Validate caller-supplied WT-PATH for BRANCH.
Returns WT-PATH expanded on success, or a `dorg-git-sync-result' failure."
  (let ((path (expand-file-name wt-path)))
    (cond
     ((not (file-directory-p path))
      (dorg-git-sync--worktree-contention-result
       branch
       (format "caller sync worktree %s does not exist" path)))
     ((not (dorg-git-sync--worktree-valid-p repo-dir path))
      (dorg-git-sync--worktree-contention-result
       branch
       (format "caller sync worktree %s is not a live worktree of this repository"
               path)))
     ((not (equal (dorg-git--current-branch path) branch))
      (dorg-git-sync--worktree-contention-result
       branch
       (format "caller sync worktree %s is on branch %S, not %S"
               path (dorg-git--current-branch path) branch)))
     (t path))))

(defun dorg-git-sync--registered-worktree-for-branch (repo-dir branch)
  "Return an existing same-repo worktree registered for BRANCH, or nil."
  (let ((output (dorg-git--ok repo-dir "worktree" "list" "--porcelain"))
        current)
    (when output
      (catch 'found
        (dolist (line (split-string output "\n"))
          (cond
           ((string-prefix-p "worktree " line)
            (setq current (substring line (length "worktree "))))
           ((and current
                 (string-prefix-p "branch refs/heads/" line))
            (let ((listed-branch
                   (substring line (length "branch refs/heads/"))))
              (when (and (equal listed-branch branch)
                         (dorg-git-sync--worktree-on-branch-p
                          repo-dir current branch))
                (throw 'found current))))))))))

(defun dorg-git-sync--repair-worktree (repo-dir wt-path)
  "Attempt `git worktree repair' on WT-PATH; return non-nil if it validates.
Relinks a checkout whose linkage dangled after a repository move, provided
the admin entry under .git/worktrees/ still exists."
  (dorg-git--run repo-dir "worktree" "repair" wt-path)
  (dorg-git-sync--worktree-valid-p repo-dir wt-path))

(defun dorg-git-sync--rebuild-worktree (repo-dir wt-path)
  "Remove the stale checkout at WT-PATH and prune its dead registration.
Deletion loses nothing: the directory holds only mirror copies; durable
history lives on the sync branch in the common git dir."
  (delete-directory wt-path t)
  (dorg-git--run repo-dir "worktree" "prune"))

(defun dorg-git-sync--create-orphan-branch (repo-dir branch)
  "Create an orphan BRANCH with an initial empty commit in REPO-DIR's repo."
  (let* ((tree (dorg-git--ok repo-dir "hash-object" "-t" "tree" "/dev/null"))
         (commit (and tree (dorg-git--ok repo-dir "commit-tree" tree
                                        "-m" "Initialize dorg sync branch"))))
    (unless commit
      (error "dorg-git-sync: failed to create orphan branch %s" branch))
    (let ((result (dorg-git--run repo-dir "branch" branch commit)))
      (unless (zerop (car result))
        (error "dorg-git-sync: failed to create branch %s: %s"
               branch (cdr result))))))

(defun dorg-git-sync--configure-sparse-checkout (wt-path sparse-subdir)
  "Configure cone sparse checkout in WT-PATH limited to SPARSE-SUBDIR.
No-op when SPARSE-SUBDIR is nil (the whole tree is checked out)."
  (when sparse-subdir
    (dorg-git--run wt-path "sparse-checkout" "init" "--cone")
    (dorg-git--run wt-path "sparse-checkout" "set" sparse-subdir)))

(defun dorg-git-sync--ensure-worktree (repo-dir branch sparse-subdir
                                                &optional caller-worktree)
  "Ensure the shared sync worktree for BRANCH exists; return its path.
When CALLER-WORKTREE is non-nil, validate and reuse that path before any
branch-creation side effect.  Otherwise reuse DOrg's derived checkout,
adopt an existing registered same-repo checkout of BRANCH, or create the
(orphan) branch and DOrg-owned worktree when missing.  A derived checkout
left invalid by a repository move is self-healed: repaired in place when
its admin entry survives, otherwise removed and recreated from BRANCH.
`git worktree prune' runs only on the (re)create path — pruning before a
repair attempt would garbage-collect the dangling admin entry repair
needs.  Returns either a path string or a structured failure result."
  (if caller-worktree
      (dorg-git-sync--validate-caller-worktree repo-dir branch caller-worktree)
    (let ((wt-path (dorg-git-sync--worktree-path repo-dir branch)))
      (unless wt-path
        (error "dorg-git-sync: cannot determine git common dir for %s" repo-dir))
      (when (and (file-directory-p wt-path)
                 (not (dorg-git-sync--worktree-valid-p repo-dir wt-path)))
        (if (dorg-git-sync--repair-worktree repo-dir wt-path)
            (dorg-git-sync--warn 'sync-worktree-repaired
                                 (format "%s: relinked after repository move" wt-path)
                                 :path wt-path)
          (dorg-git-sync--rebuild-worktree repo-dir wt-path)
          (dorg-git-sync--warn 'sync-worktree-rebuilt
                               (format "%s: stale checkout removed; recreating from %s"
                                       wt-path branch)
                               :path wt-path)))
      (cond
       ((and (file-directory-p wt-path)
             (not (dorg-git-sync--worktree-on-branch-p repo-dir wt-path branch)))
        (dorg-git-sync--worktree-contention-result
         branch
         (format "DOrg sync worktree %s is on branch %S, not %S"
                 wt-path (dorg-git--current-branch wt-path) branch)))
       ((file-directory-p wt-path)
        wt-path)
       ((dorg-git-sync--registered-worktree-for-branch repo-dir branch))
       (t
        ;; Prune dead registrations first — a dangling admin entry at this
        ;; path would block `git worktree add'.
        (dorg-git--run repo-dir "worktree" "prune")
        (unless (dorg-git--branch-exists-p repo-dir branch)
          (dorg-git-sync--create-orphan-branch repo-dir branch))
        (let ((result (dorg-git--run repo-dir "worktree" "add" wt-path branch)))
          (if (zerop (car result))
              (progn
                (dorg-git-sync--configure-sparse-checkout wt-path sparse-subdir)
                wt-path)
            (dorg-git-sync--worktree-contention-result
             branch
             (format "failed to create sync worktree %s: %s"
                     wt-path (cdr result))))))))))

;;; ─── Commit paths ─────────────────────────────────────────────────────

(cl-defun dorg-git-sync--stage-and-commit
    (work-dir sources toplevel message branch &key identity no-changes-detail)
  "Stage SOURCES from WORK-DIR and commit them on BRANCH.
Shared tail of `dorg-git-sync--commit-sync-branch' and
`dorg-git-sync--commit-current-branch'.  SOURCES are staged by their
TOPLEVEL-relative pathspecs (cwd-relative, so WORK-DIR must be the tree
whose toplevel is TOPLEVEL); a failed `git add' degrades to a warning and
drops that path.  Commits with MESSAGE only when the staged set differs
from HEAD.

Returns a `dorg-git-sync-result':
- :skipped :no-changes with NO-CHANGES-DETAIL when nothing changed,
- :committed :committed (carrying the new HEAD) on success,
- :failed :git-error (carrying only :detail) when the commit fails.
IDENTITY, when non-nil, is carried on the :skipped and :committed
results — the sync-branch caller publishes (:source .. :mirror ..); the
current-branch caller passes nil so no identity is attached."
  (let ((rels (mapcar (lambda (s) (dorg-git-sync--rel s toplevel)) sources))
        staged-rels)
    (dolist (rel rels)
      (let ((r (dorg-git--run work-dir "add" "--" rel)))
        (if (zerop (car r))
            (push rel staged-rels)
          (dorg-git-sync--warn 'git-add-failed
                               (format "%s: %s" rel (cdr r)) :path rel))))
    (setq staged-rels (nreverse staged-rels))
    (if (or (null staged-rels)
            (zerop (car (apply #'dorg-git--run work-dir
                               "diff" "--cached" "--quiet" "--" staged-rels))))
        (dorg-git-sync--result :skipped :no-changes :branch branch
                               :identity identity
                               :detail no-changes-detail)
      (let ((res (apply #'dorg-git--run work-dir
                        "commit" "--no-verify" "-m" message "--" staged-rels)))
        (if (zerop (car res))
            (dorg-git-sync--result
             :committed :committed
             :commit (dorg-git--ok work-dir "rev-parse" "HEAD")
             :branch branch
             :identity identity)
          (dorg-git-sync--result :failed :git-error :branch branch
                                 :detail (cdr res)))))))

(cl-defun dorg-git-sync--commit-sync-branch (config)
  "Mirror CONFIG's source files into the sync-branch worktree and commit.
Identity guard runs FIRST: a foreign source against an established mirror
is refused with NO copy/stage/commit.  Commits only when the mirror
differs; an up-to-date mirror yields :skipped :no-changes."
  (let* ((repo-dir (dorg-git-sync--repo-dir config))
         (branch (dorg-git-sync-config-branch config))
         (sparse (dorg-git-sync-config-sparse-subdir config))
         (id-kws (dorg-git-sync-config-identity-keywords config))
         (root-file (expand-file-name (dorg-git-sync-config-root-file config)))
         (sources (dorg-git-sync-config-source-files config))
         (overrides (dorg-git-sync--normalize-source-content-overrides
                     (dorg-git-sync-config-source-content-overrides config)))
         (message (or (dorg-git-sync-config-message config) "dorg: sync"))
         (wt-path (dorg-git-sync--ensure-worktree
                   repo-dir branch sparse
                   (dorg-git-sync-config-sync-worktree-path config))))
    (when (dorg-git-sync-result-p wt-path)
      (cl-return-from dorg-git-sync--commit-sync-branch wt-path))
    (let* ((toplevel (dorg-git--ok repo-dir "rev-parse" "--show-toplevel"))
           (root-rel (dorg-git-sync--rel root-file toplevel))
           (mirror-root (expand-file-name root-rel wt-path))
           (source-identity (dorg-git-sync--source-identity root-file id-kws
                                                            overrides))
           (source-override-p (car source-identity))
           (source-id (cdr source-identity))
           (mirror-id (dorg-git-sync--file-identity mirror-root id-kws)))
    ;; A root override is the authority being published.  Never validate
    ;; disk identity while publishing override bytes.
    (when (and source-override-p (null source-id))
      (cl-return-from dorg-git-sync--commit-sync-branch
        (dorg-git-sync--result
         :failed :identity-mismatch :branch branch
         :identity (list :source source-id :mirror mirror-id)
         :detail "root override has no parseable identity; refusing sync")))
    ;; Refuse a foreign-identity source against an established mirror.
    (unless (dorg-git-sync--identity-ok-p source-id mirror-id)
      (cl-return-from dorg-git-sync--commit-sync-branch
        (dorg-git-sync--result
         :failed :identity-mismatch :branch branch
         :identity (list :source source-id :mirror mirror-id)
         :detail (format "source identity %S differs from mirror identity %S; refusing sync"
                         source-id mirror-id))))
    ;; Copy each source into the worktree, preserving its repo-relative path.
    (dolist (src sources)
      (let* ((rel (dorg-git-sync--rel src toplevel))
             (dest (expand-file-name rel wt-path)))
        ;; Guard the resolved write OUTSIDE the condition-case so a guard
        ;; signal aborts loudly rather than degrading to a copy warning.
        (dorg-git-sync--write-check dest)
        (unless (file-directory-p (file-name-directory dest))
          (make-directory (file-name-directory dest) t))
        (condition-case err
            (dorg-git-sync--copy-source-to-mirror src dest overrides)
          (error
           (dorg-git-sync--warn 'file-copy-failed
                                (format "%s: %s"
                                        (file-name-nondirectory src)
                                        (error-message-string err))
                                :path src)))))
    ;; Stage in the worktree, then commit only when something changed.
    (dorg-git-sync--stage-and-commit
     wt-path sources toplevel message branch
     :identity (list :source source-id :mirror mirror-id)
     :no-changes-detail "mirror up to date"))))

(cl-defun dorg-git-sync--commit-current-branch (config)
  "Stage and commit CONFIG's source files in place on the current branch.
No sync worktree is created.  Identity guard compares the live source
against the HEAD-committed authority file: a foreign root replacing a
different committed identity is refused.  Up-to-date -> :skipped :no-changes."
  (let* ((repo-dir (dorg-git-sync--repo-dir config))
         (id-kws (dorg-git-sync-config-identity-keywords config))
         (root-file (expand-file-name (dorg-git-sync-config-root-file config)))
         (sources (dorg-git-sync-config-source-files config))
         (message (or (dorg-git-sync-config-message config) "dorg: sync"))
         (current (dorg-git--current-branch repo-dir))
         (toplevel (dorg-git--ok repo-dir "rev-parse" "--show-toplevel"))
         (root-rel (dorg-git-sync--rel root-file toplevel))
         (source-id (dorg-git-sync--file-identity root-file id-kws))
         (head-content (dorg-git--ok repo-dir "show" (format "HEAD:%s" root-rel)))
         (mirror-id (dorg-git-sync--string-identity head-content id-kws)))
    ;; Detached HEAD has no current branch to commit on; a commit here
    ;; would be unreachable/dangling.  Refuse visibly rather than landing
    ;; a commit and reporting branch "HEAD".
    (unless current
      (cl-return-from dorg-git-sync--commit-current-branch
        (dorg-git-sync--result
         :skipped :misconfigured :branch nil
         :detail ":current-branch mode requires a checked-out branch; HEAD is detached")))
    (unless (dorg-git-sync--identity-ok-p source-id mirror-id)
      (cl-return-from dorg-git-sync--commit-current-branch
        (dorg-git-sync--result
         :failed :identity-mismatch :branch current
         :identity (list :source source-id :mirror mirror-id)
         :detail (format "source identity %S differs from committed identity %S; refusing sync"
                         source-id mirror-id))))
    ;; Pathspecs are cwd-relative, so run staging/commit from the repo
    ;; TOPLEVEL (not the root-file's subdir) to match the toplevel-relative
    ;; paths computed above.  No identity is carried on the current-branch
    ;; results (in-place commits publish no source/mirror identity).
    (dorg-git-sync--stage-and-commit
     toplevel sources toplevel message current
     :no-changes-detail "working tree up to date")))

;;; ─── Mode dispatch / entry points ─────────────────────────────────────

(defun dorg-git-sync--dispatch-git (config fn)
  "Run FN on CONFIG behind a git-repo check, then auto-push if requested.
Returns FN's `dorg-git-sync-result', or :skipped :not-a-git-repo when
CONFIG's root is not in a git repository (no error signaled)."
  (let ((repo-dir (dorg-git-sync--repo-dir config)))
    (if (not (dorg-git--repo-p repo-dir))
        (dorg-git-sync--result :skipped :not-a-git-repo
                               :detail "not a git repository")
      (let ((result (funcall fn config)))
        (when (and (eq (dorg-git-sync-result-status result) :committed)
                   (dorg-git-sync-config-push config))
          (let ((push (dorg-git-sync-push config)))
            (unless (eq (dorg-git-sync-result-status push) :committed)
              (dorg-git-sync--warn 'push-failed
                                   (or (dorg-git-sync-result-detail push)
                                       "push failed"))
              ;; Refresh the committed result's warnings with the push note.
              (setf (dorg-git-sync-result-warnings result)
                    (reverse dorg-git-sync--warnings)))))
        result))))

(cl-defun dorg-git-sync (config)
  "Mirror CONFIG's source files into git per CONFIG's explicit `mode'.
Dispatches strictly on `mode'; `branch' is never used to infer intent:

  :disabled        -> :skipped :disabled (no git access at all).
  mode nil/unknown -> :skipped :misconfigured.
  :sync-branch     -> mirror into the sync-branch worktree; a nil/empty
                      `branch' is :skipped :misconfigured.
  :current-branch  -> commit source files in place on the current branch.

A Dolt-backed root is gated by the centralized publication policy BEFORE
any git access: default-denied `:skipped :dolt-export-disabled' unless
`allow-dolt-export' is set, and even then only in `:sync-branch' mode
(other modes -> `:skipped :misconfigured').

Not a git repo (any non-disabled mode) -> :skipped :not-a-git-repo.
Returns a `dorg-git-sync-result'.  Idempotent: commits only when the
mirror differs."
  (let* ((dorg-git-sync--warnings nil)
         (mode (dorg-git-sync-config-mode config))
         (decision (and (not (eq mode :disabled))
                        (dorg-git-sync--publication-decision config))))
    (pcase mode
      (:disabled
       (dorg-git-sync--result :skipped :disabled :detail "sync disabled"))
      ((guard (not (eq decision :allow)))
       (dorg-git-sync--denied-result decision))
      (:sync-branch
       (let ((branch (dorg-git-sync-config-branch config)))
         (if (or (null branch) (string-empty-p branch))
             (dorg-git-sync--result
              :skipped :misconfigured
              :detail ":sync-branch mode requires a non-empty branch")
           (dorg-git-sync--dispatch-git
            config #'dorg-git-sync--commit-sync-branch))))
      (:current-branch
       (dorg-git-sync--dispatch-git
        config #'dorg-git-sync--commit-current-branch))
      (_
       (dorg-git-sync--result
        :skipped :misconfigured
        :detail (format "unknown sync mode %S" mode))))))

(cl-defun dorg-git-sync-push (config)
  "Push CONFIG's effective branch to its remote.  Returns a result.
The effective branch is the sync branch in `:sync-branch' mode and the
current branch in `:current-branch' mode; `push-branch' overrides it.
Default remote is \"origin\".  A missing branch or non-git repo is
reported, not signaled.  A Dolt-backed root is gated by the same
centralized publication policy BEFORE any git access (including the
current-branch resolution below), so a default-disabled Dolt root can
never push a derived snapshot."
  (let ((dorg-git-sync--warnings nil)
        (decision (dorg-git-sync--publication-decision config)))
    (if (not (eq decision :allow))
        (dorg-git-sync--denied-result decision)
      (dorg-git-sync--push-allowed config))))

(cl-defun dorg-git-sync--push-allowed (config)
  "Push CONFIG's effective branch, assuming publication policy permits it."
  (let* ((repo-dir (dorg-git-sync--repo-dir config))
         (mode (dorg-git-sync-config-mode config))
         (effective (pcase mode
                      (:sync-branch (dorg-git-sync-config-branch config))
                      (:current-branch (dorg-git--current-branch repo-dir))
                      (_ nil)))
         (remote (or (dorg-git-sync-config-push-remote config) "origin"))
         (push-branch (or (dorg-git-sync-config-push-branch config) effective)))
    (cond
     ((not (dorg-git--repo-p repo-dir))
      (dorg-git-sync--result :skipped :not-a-git-repo
                             :detail "not a git repository"))
     ((or (null push-branch) (string-empty-p push-branch))
      (dorg-git-sync--result :skipped :misconfigured
                             :detail "no branch to push"))
     (t
      (let ((res (dorg-git--run repo-dir "push" remote push-branch)))
        (if (zerop (car res))
            (dorg-git-sync--result :committed :committed :branch push-branch
                                   :detail (format "pushed %s to %s"
                                                   push-branch remote))
          (dorg-git-sync--result :failed :git-error :branch push-branch
                                 :detail (cdr res))))))))

(defun dorg-git-sync-status (config)
  "Return the log summary between sync branch and current branch for CONFIG.
Returns the `git log --oneline current..sync-branch' output, or nil when
CONFIG's mode is not `:sync-branch' or the sync branch does not exist.

Routes through the same centralized publication policy as sync/push
BEFORE any status-path Git helper (`dorg-git--current-branch',
`dorg-git--branch-exists-p', `dorg-git--ok'): a Dolt-backed root that may
not publish returns nil before any of them is consulted, so status cannot
disclose a default-disabled derived snapshot."
  (when (and (eq (dorg-git-sync--publication-decision config) :allow)
             (eq (dorg-git-sync-config-mode config) :sync-branch))
    (let* ((repo-dir (dorg-git-sync--repo-dir config))
           (branch (dorg-git-sync-config-branch config))
           (current (dorg-git--current-branch repo-dir)))
      (when (and branch current
                 (dorg-git--branch-exists-p repo-dir branch))
        (dorg-git--ok repo-dir "log" "--oneline"
                     (format "%s..%s" current branch))))))

(provide 'dorg-git-sync)
;;; dorg-git-sync.el ends here
