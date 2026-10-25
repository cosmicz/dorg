;;; dorg-git-sync-tests.el --- Tests for dorg-git-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the plain-org git sync-branch adapter (`dorg-git-sync.el').
;; Each test runs against a throwaway `git init' temp repo with a
;; `.tracker/beads.org' authority file carrying a `#+DORG_STORE_ID'
;; identity.  Tests cover the git-sync store-identity Acceptance Criteria
;; (A backend-neutral surface, B identity guard, C linked worktrees,
;; D explicit-mode dispatch, E atomic copy/commit, F push & status).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg-git)
(require 'dorg-git-sync)

;;; ─── Fixtures ──────────────────────────────────────────────────────────

(defun dorg-git-sync-tests--store-content (id &optional body)
  "Return org store content stamped with `#+DORG_STORE_ID' ID and BODY."
  (format "#+TITLE: Test Store\n#+DORG_STORE_ID: %s\n\n%s"
          id (or body "* Root\n")))

(defun dorg-git-sync-tests--write (file content)
  "Write CONTENT to FILE, creating parent directories as needed."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert content)))

(defmacro dorg-git-sync-tests--with-repo (top-var root-var &rest body)
  "Create a git repo with a store file; bind TOP-VAR and ROOT-VAR; run BODY.
TOP-VAR is the repo toplevel; ROOT-VAR is `.tracker/beads.org' stamped with
identity \"store-A\".  The repo is removed afterwards."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let* ((,top-var (file-name-as-directory (make-temp-file "dorg-gs-" t)))
          (,root-var (expand-file-name ".tracker/beads.org" ,top-var)))
     (unwind-protect
         (progn
           (dorg-git--run ,top-var "init" "-q")
           (dorg-git--run ,top-var "config" "user.email" "test@example.com")
           (dorg-git--run ,top-var "config" "user.name" "DOrg Test")
           (dorg-git--run ,top-var "config" "commit.gpgsign" "false")
           (dorg-git-sync-tests--write
            ,root-var (dorg-git-sync-tests--store-content "store-A"))
           ,@body)
       (delete-directory ,top-var t))))

(cl-defun dorg-git-sync-tests--config (root &key (mode :sync-branch)
                                            (branch "dorg/sync")
                                            (sparse ".tracker")
                                            sources push (message "test sync")
                                            identity-keywords
                                            source-content-overrides
                                            backend-type
                                            allow-dolt-export
                                            sync-worktree-path)
  "Build a `dorg-git-sync-config' for ROOT with sensible test defaults."
  (apply #'dorg-git-sync-config-create
         :mode mode :root-file root
         :source-files (or sources (list root))
         :branch branch :sparse-subdir sparse
         :push push :message message
         (append
          (when identity-keywords (list :identity-keywords identity-keywords))
          (when source-content-overrides
            (list :source-content-overrides source-content-overrides))
          (when backend-type (list :backend-type backend-type))
          (when allow-dolt-export (list :allow-dolt-export t))
          (when sync-worktree-path
            (list :sync-worktree-path sync-worktree-path)))))

(defun dorg-git-sync-tests--git-blob (repo spec)
  "Return byte-preserving `git show' output for SPEC in REPO, or nil."
  (let ((default-directory (file-name-as-directory (expand-file-name repo))))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "show" spec))
        (buffer-string)))))

(defun dorg-git-sync-tests--git-status (repo &rest args)
  "Return porcelain status for ARGS in REPO."
  (apply #'dorg-git--ok repo "status" "--porcelain" "--" args))

;;; ─── A. Backend-neutral surface, no private downstream dependency ──────

(ert-deftest dorg-test-git-sync-no-tracker-dependency ()
  "New modules carry zero private downstream references in source."
  (let ((private-name (concat "em" "bead")))
    (dolist (lib '("dorg-git" "dorg-git-sync"))
      (let* ((found (locate-library lib))
           (src (if (and found (string-suffix-p ".elc" found))
                    (substring found 0 -1)
                  found)))
        (should src)
        (with-temp-buffer
          (insert-file-contents src)
          (goto-char (point-min))
          (let ((case-fold-search t))
            (should-not (re-search-forward private-name nil t))))))))

;;; ─── B. Identity guard ─────────────────────────────────────────────────

(ert-deftest dorg-test-git-sync-first-sync-adopts-and-commits ()
  "First-ever sync (no mirror) adopts the source identity and commits."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (eq (dorg-git-sync-result-reason res) :committed))
      (should (stringp (dorg-git-sync-result-commit res)))
      (should (dorg-git--branch-exists-p top "dorg/sync")))))

(ert-deftest dorg-test-git-sync-matching-identity-proceeds ()
  "A mirror sharing the source identity continues to sync (here: no-op).
CHARACTERIZATION (pins the sync-branch no-change slots before the shared
`--stage-and-commit' helper extraction, tracked-case): the no-change path
carries :identity (:source \"store-A\" :mirror \"store-A\") and the detail
\"mirror up to date\".  Must stay green after the extraction."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
      (should (eq (dorg-git-sync-result-status res) :skipped))
      (should (eq (dorg-git-sync-result-reason res) :no-changes))
      (should (equal (dorg-git-sync-result-identity res)
                     '(:source "store-A" :mirror "store-A")))
      (should (equal (dorg-git-sync-result-detail res)
                     "mirror up to date")))))

(ert-deftest dorg-test-git-sync-identity-mismatch-refuses ()
  "A foreign source against an established mirror is refused; mirror unchanged."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync (dorg-git-sync-tests--config root)) ; mirror = store-A
    (let ((sha1 (dorg-git--ok top "rev-parse" "dorg/sync")))
      ;; Rewrite the source with a FOREIGN identity.
      (dorg-git-sync-tests--write
       root (dorg-git-sync-tests--store-content "store-B"))
      (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
        (should (eq (dorg-git-sync-result-status res) :failed))
        (should (eq (dorg-git-sync-result-reason res) :identity-mismatch))
        (should (equal (dorg-git-sync-result-identity res)
                       '(:source "store-B" :mirror "store-A")))
        ;; Transformation test: NO new commit, mirror content untouched.
        (should (equal sha1 (dorg-git--ok top "rev-parse" "dorg/sync")))
        (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
          (should (string-match-p "store-A" mirror))
          (should-not (string-match-p "store-B" mirror)))))))

(ert-deftest dorg-test-git-sync-identity-first-keyword-fallback ()
  "Identity resolves via the FIRST present keyword in the ordered list."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write root "#+TITLE: only-title\n\n* R\n")
    (should (equal (dorg-git-sync--file-identity
                    root '("EMBEAD_TRACKER_ID" "EMBEAD_PREFIX" "TITLE"))
                   "only-title"))))

;;; ─── B2. Case-insensitive identity comparison (tracked-case) ─────────────
;; A mirror and its source that differ ONLY by case (TRACKER vs tracker) name
;; the SAME document root and must sync.  Genuinely distinct roots (tracker
;; vs coordinator) must still be refused.  Raw source/mirror spelling is never
;; canonicalized.

(ert-deftest dorg-test-git-sync-identity-case-insensitive-comparator ()
  "`dorg-git-sync--identity-ok-p' treats string identities case-insensitively.
Absent identities are permitted; genuinely distinct roots are refused;
unexpected non-string values retain conservative exact equality (no
coercion).  The identity resolved from override/content text preserves its
raw spelling (the delegated source-content path)."
  ;; Case variants identify the same root.
  (should (dorg-git-sync--identity-ok-p "TRACKER" "tracker"))
  (should (dorg-git-sync--identity-ok-p "tracker" "TRACKER"))
  (should (dorg-git-sync--identity-ok-p "MiXeD" "mixed"))
  ;; Genuinely distinct roots stay refused.
  (should-not (dorg-git-sync--identity-ok-p "tracker" "coordinator"))
  ;; Absent identities remain permitted (first-sync / identity-less source).
  (should (dorg-git-sync--identity-ok-p nil "tracker"))
  (should (dorg-git-sync--identity-ok-p "tracker" nil))
  (should (dorg-git-sync--identity-ok-p nil nil))
  ;; Non-string values keep conservative exact equality (no downcasing).
  (should (dorg-git-sync--identity-ok-p 5 5))
  (should-not (dorg-git-sync--identity-ok-p 5 6))
  (should-not (dorg-git-sync--identity-ok-p 'tracker "tracker"))
  ;; Identity resolved from override/content TEXT keeps its raw spelling and
  ;; still compares case-insensitively.
  (let ((src-id (dorg-git-sync--string-identity
                 (dorg-git-sync-tests--store-content "TRACKER") '("DORG_STORE_ID")))
        (mir-id (dorg-git-sync--string-identity
                 (dorg-git-sync-tests--store-content "tracker") '("DORG_STORE_ID"))))
    (should (equal src-id "TRACKER"))       ; raw spelling preserved
    (should (equal mir-id "tracker"))
    (should (dorg-git-sync--identity-ok-p src-id mir-id))))

(ert-deftest dorg-test-git-sync-identity-case-variant-sync-branch-publishes ()
  "Sync-branch: a case-variant source (TRACKER) syncs to a `tracker' mirror.
The branch tip advances, the changed body is published, and the result is
not `:identity-mismatch'.  Raw source/mirror spelling is preserved in the
published bytes and the result identity payload."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    ;; Establish the mirror with identity `tracker'.
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "tracker" "* Root\n"))
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let ((sha0 (dorg-git--ok top "rev-parse" "dorg/sync")))
      ;; Rewrite the source with the CASE-VARIANT identity `TRACKER' plus a
      ;; deliberately changed body.
      (dorg-git-sync-tests--write
       root (dorg-git-sync-tests--store-content "TRACKER" "* Root\n* CASEVAR-SB\n"))
      (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
        (should (eq (dorg-git-sync-result-status res) :committed))
        (should-not (eq (dorg-git-sync-result-reason res) :identity-mismatch))
        ;; Transformation: branch tip MOVED (before != after).
        (should-not (equal sha0 (dorg-git--ok top "rev-parse" "dorg/sync")))
        (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
          ;; Changed body published; source spelling `TRACKER' kept verbatim.
          (should (string-match-p "CASEVAR-SB" mirror))
          (should (string-match-p "TRACKER" mirror)))
        ;; Result payload retains raw source/mirror spelling (no canonicalize).
        (should (equal (dorg-git-sync-result-identity res)
                       '(:source "TRACKER" :mirror "tracker")))))))

(ert-deftest dorg-test-git-sync-identity-case-variant-override-text-publishes ()
  "Source-content-override: identity is validated from OVERRIDE TEXT.
The delegated Downstream tracker path resolves identity through
`dorg-git-sync--source-identity'/overrides, not disk.  Here the mirror is
`tracker', the override text carries the case variant `TRACKER' plus a changed
body, and the DISK file carries a genuinely FOREIGN root `coordinator'.

Isolation proof: identity is taken from the override text `TRACKER', which
case-matches the mirror `tracker', so the changed override bytes publish.
Were identity read from disk (`coordinator'), the comparator would refuse even
case-insensitively (`coordinator' != `tracker'); a `:committed' result therefore
proves the identity came from the override text.  Old exact `equal'
refused `TRACKER' vs `tracker' (RED); the case-insensitive comparator accepts
it (GREEN)."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    ;; Establish the mirror with identity `tracker'.
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "tracker" "* Root\n"))
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let ((sha0 (dorg-git--ok top "rev-parse" "dorg/sync")))
      ;; Disk now carries a FOREIGN root `coordinator' with a body that must NOT
      ;; be published; the override is the authority being synced.
      (dorg-git-sync-tests--write
       root (dorg-git-sync-tests--store-content "coordinator" "* Root\n* DISK-FOREIGN\n"))
      (let* ((override (dorg-git-sync-tests--store-content
                        "TRACKER" "* Root\n* CASEVAR-OVR\n"))
             (res (dorg-git-sync
                   (dorg-git-sync-tests--config
                    root :source-content-overrides
                    (list (cons (expand-file-name root) override))))))
        (should (eq (dorg-git-sync-result-status res) :committed))
        (should-not (eq (dorg-git-sync-result-reason res) :identity-mismatch))
        ;; Transformation: branch tip MOVED (before != after).
        (should-not (equal sha0 (dorg-git--ok top "rev-parse" "dorg/sync")))
        (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
          ;; Override bytes published; the foreign disk body never leaks.
          (should (string-match-p "CASEVAR-OVR" mirror))
          (should (string-match-p "TRACKER" mirror))
          (should-not (string-match-p "DISK-FOREIGN" mirror))
          (should-not (string-match-p "coordinator" mirror)))
        ;; Result payload: source identity is the override spelling `TRACKER'
        ;; (NOT the disk `coordinator'), mirror keeps raw `tracker'.
        (should (equal (dorg-git-sync-result-identity res)
                       '(:source "TRACKER" :mirror "tracker")))))))

(ert-deftest dorg-test-git-sync-identity-case-variant-current-branch-publishes ()
  "Current-branch: a case-variant source (TRACKER) syncs to a committed `tracker'.
The committed identity is resolved from HEAD content text; the changed body
lands on the current branch and the result is not `:identity-mismatch'."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "tracker" "* Root\n"))
    (dorg-git--run top "add" "-A")
    (dorg-git--run top "commit" "-m" "seed") ; HEAD identity = tracker
    ;; Case-variant working source + changed body.
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "TRACKER" "* Root\n* CASEVAR-CB\n"))
    (let* ((current (dorg-git--current-branch top))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config root :mode :current-branch
                                              :branch nil))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should-not (eq (dorg-git-sync-result-reason res) :identity-mismatch))
      (should (equal (dorg-git-sync-result-branch res) current))
      (let ((head (dorg-git--ok top "show"
                                 (format "%s:.tracker/beads.org" current))))
        (should (string-match-p "CASEVAR-CB" head))))))

(ert-deftest dorg-test-git-sync-identity-foreign-root-still-refused ()
  "Case-insensitivity must NOT weaken foreign-root refusal (tracker vs coordinator).
A genuinely distinct root against an established mirror is refused with NO
copy/stage/commit; the branch tip and mirror bytes are unchanged."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "tracker" "* Root\n"))
    (dorg-git-sync (dorg-git-sync-tests--config root)) ; mirror = tracker
    (let ((sha (dorg-git--ok top "rev-parse" "dorg/sync")))
      ;; Genuinely different root plus a changed body: must be refused.
      (dorg-git-sync-tests--write
       root (dorg-git-sync-tests--store-content "coordinator" "* Root\n* FOREIGN\n"))
      (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
        (should (eq (dorg-git-sync-result-status res) :failed))
        (should (eq (dorg-git-sync-result-reason res) :identity-mismatch))
        (should (equal (dorg-git-sync-result-identity res)
                       '(:source "coordinator" :mirror "tracker")))
        ;; Transformation guard: NO new commit, mirror body untouched.
        (should (equal sha (dorg-git--ok top "rev-parse" "dorg/sync")))
        (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
          (should (string-match-p "tracker" mirror))
          (should-not (string-match-p "FOREIGN" mirror)))))))

;;; ─── C. Linked worktrees ───────────────────────────────────────────────

(ert-deftest dorg-test-git-sync-worktree-path-shared-across-linked ()
  "Two linked worktrees of one repo resolve to the SAME sync worktree path."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top _root
    (dorg-git--run top "commit" "--allow-empty" "-m" "seed")
    (let* ((parent (file-name-as-directory (make-temp-file "dorg-gs-lw-" t)))
           (lw (expand-file-name "lw" parent)))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              "-b" "lane" lw))))
            ;; The path strings may differ only by a /tmp vs /private/tmp
            ;; symlink prefix; truename normalizes to the one on-disk path.
            (should (equal
                     (file-truename (dorg-git-sync--worktree-path top "dorg/sync"))
                     (file-truename (dorg-git-sync--worktree-path lw "dorg/sync")))))
        (delete-directory parent t)))))

(ert-deftest dorg-test-git-sync-secondary-worktree-no-duplicate-checkout ()
  "Syncing from a secondary linked worktree reuses the one shared checkout."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "commit" "--allow-empty" "-m" "seed")
    (should (eq (dorg-git-sync-result-status
                 (dorg-git-sync (dorg-git-sync-tests--config root)))
                :committed))
    (let* ((parent (file-name-as-directory (make-temp-file "dorg-gs-lw-" t)))
           (lw (expand-file-name "lw" parent))
           (lw-root (expand-file-name ".tracker/beads.org"
                                      (file-name-as-directory lw))))
      (unwind-protect
          (progn
            (dorg-git--run top "worktree" "add" "-b" "lane" lw)
            (dorg-git-sync-tests--write
             lw-root (dorg-git-sync-tests--store-content "store-A"))
            (let ((res (dorg-git-sync (dorg-git-sync-tests--config lw-root))))
              ;; No "already checked out" failure from the secondary worktree.
              (should (memq (dorg-git-sync-result-status res)
                            '(:committed :skipped)))
              (should-not (eq (dorg-git-sync-result-reason res) :git-error))))
        (delete-directory parent t)))))

(ert-deftest dorg-test-git-sync-caller-worktree-path-reused ()
  "A caller-supplied live sync worktree is reused, not replaced.
Non-vacuity: deleting the `:sync-worktree-path' delivery would make DOrg
try its derived path instead, where Git refuses the already checked-out
branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let* ((parent (file-name-as-directory (make-temp-file "dorg-gs-adopt-" t)))
           (caller-wt (expand-file-name "caller-sync" parent))
           (sentinel (expand-file-name "CALLER-SENTINEL" caller-wt))
           (derived (dorg-git-sync--worktree-path top "dorg/sync")))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              caller-wt "dorg/sync"))))
            (with-temp-file sentinel (insert "keep"))
            (dorg-git-sync-tests--write
             root (dorg-git-sync-tests--store-content "store-A" "* Via caller\n"))
            (let ((res (dorg-git-sync
                        (dorg-git-sync-tests--config
                         root :sync-worktree-path caller-wt))))
              (should (eq (dorg-git-sync-result-status res) :committed))
              (should (equal (dorg-git-sync-result-branch res) "dorg/sync"))
              (should (file-exists-p sentinel))
              (should-not (file-exists-p derived))
              (should (string-match-p
                       "Via caller"
                       (dorg-git-sync-tests--git-blob
                        top "dorg/sync:.tracker/beads.org")))))
        (delete-directory parent t)))))

(ert-deftest dorg-test-git-sync-adopts-existing-registered-branch-worktree ()
  "An existing same-repo worktree for the sync branch is adopted safely.
This is the b7gcj singleton topology: the branch is already checked out
at a legacy/caller-owned path, so DOrg must not attempt a second checkout."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let* ((existing (expand-file-name "tracker-sync" (dorg-git--common-dir top)))
           (derived (dorg-git-sync--worktree-path top "dorg/sync")))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              existing "dorg/sync"))))
            (dorg-git-sync-tests--write
             root (dorg-git-sync-tests--store-content "store-A" "* Adopted\n"))
            (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
              (should (eq (dorg-git-sync-result-status res) :committed))
              (should-not (eq (dorg-git-sync-result-reason res) :git-error))
              (should-not (file-exists-p derived))
              (should (string-match-p
                       "Adopted"
                       (dorg-git-sync-tests--git-blob
                        top "dorg/sync:.tracker/beads.org")))))
        (when (file-directory-p existing)
          (delete-directory existing t))))))

(ert-deftest dorg-test-git-sync-adopted-worktree-commits-only-mirror-rels ()
  "Adoption must not sweep unrelated caller-staged files into sync commits."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let* ((existing (expand-file-name "tracker-sync" (dorg-git--common-dir top)))
           (unrelated "caller-staged.txt")
           (unrelated-path (expand-file-name unrelated existing)))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              existing "dorg/sync"))))
            (dorg-git-sync-tests--write unrelated-path "caller owns this\n")
            (should (zerop (car (dorg-git--run existing "add" "--" unrelated))))
            (dorg-git-sync-tests--write
             root
             (dorg-git-sync-tests--store-content "store-A" "* Scoped commit\n"))
            (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
              (should (eq (dorg-git-sync-result-status res) :committed))
              (should (string-match-p
                       "Scoped commit"
                       (dorg-git-sync-tests--git-blob
                        top "dorg/sync:.tracker/beads.org")))
              (should-not (dorg-git-sync-tests--git-blob
                           top (format "dorg/sync:%s" unrelated)))
              (should (equal "A  caller-staged.txt"
                             (dorg-git-sync-tests--git-status
                              existing unrelated)))))
        (when (file-directory-p existing)
          (delete-directory existing t))))))

(ert-deftest dorg-test-git-sync-caller-worktree-no-change-ignores-staged ()
  "Caller-staged files do not turn an unchanged mirror into a sync commit."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let* ((parent (file-name-as-directory (make-temp-file "dorg-gs-index-" t)))
           (caller-wt (expand-file-name "caller-sync" parent))
           (unrelated "caller-staged.txt")
           (unrelated-path (expand-file-name unrelated caller-wt)))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              caller-wt "dorg/sync"))))
            (let ((first (dorg-git-sync
                          (dorg-git-sync-tests--config
                           root :sync-worktree-path caller-wt))))
              (should (eq (dorg-git-sync-result-status first) :committed)))
            (dorg-git-sync-tests--write unrelated-path "caller owns this\n")
            (should (zerop (car (dorg-git--run caller-wt "add" "--" unrelated))))
            (let ((res (dorg-git-sync
                        (dorg-git-sync-tests--config
                         root :sync-worktree-path caller-wt))))
              (should (eq (dorg-git-sync-result-status res) :skipped))
              (should (eq (dorg-git-sync-result-reason res) :no-changes))
              (should-not (dorg-git-sync-tests--git-blob
                           top (format "dorg/sync:%s" unrelated)))
              (should (equal "A  caller-staged.txt"
                             (dorg-git-sync-tests--git-status
                              caller-wt unrelated)))))
        (delete-directory parent t)))))

(ert-deftest dorg-test-git-sync-foreign-caller-worktree-refused-structured ()
  "A caller path from another repository is refused as contention.
The failure is a structured DOrg result, not a hidden warning and not a
raw `git worktree add' error."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((foreign (file-name-as-directory (make-temp-file "dorg-gs-foreign-" t))))
      (unwind-protect
          (progn
            (dorg-git--run foreign "init" "-q")
            (dorg-git--run foreign "config" "user.email" "test@example.com")
            (dorg-git--run foreign "config" "user.name" "DOrg Test")
            (let ((res (dorg-git-sync
                        (dorg-git-sync-tests--config
                         root :sync-worktree-path foreign))))
              (should (eq (dorg-git-sync-result-status res) :failed))
              (should (eq (dorg-git-sync-result-reason res)
                          :worktree-contention))
              (should (stringp (dorg-git-sync-result-detail res)))
              (should-not (dorg-git--branch-exists-p top "dorg/sync"))))
        (delete-directory foreign t)))))

(ert-deftest dorg-test-git-sync-wrong-branch-caller-worktree-refused ()
  "A same-repo caller path on the wrong branch is refused before writes."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "add" "-A")
    (dorg-git--run top "commit" "-m" "seed")
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let* ((parent (file-name-as-directory (make-temp-file "dorg-gs-wrong-" t)))
           (wrong-wt (expand-file-name "wrong" parent)))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              "-b" "wrong-branch"
                                              wrong-wt))))
            (let ((res (dorg-git-sync
                        (dorg-git-sync-tests--config
                         root :sync-worktree-path wrong-wt))))
              (should (eq (dorg-git-sync-result-status res) :failed))
              (should (eq (dorg-git-sync-result-reason res)
                          :worktree-contention))
              (should (string-match-p
                       "wrong-branch"
                       (dorg-git-sync-result-detail res)))))
        (delete-directory parent t)))))

(ert-deftest dorg-test-git-sync-missing-caller-worktree-refused-before-branch-create ()
  "A missing caller path is refused before creating the sync branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((missing-parent (file-name-as-directory
                            (make-temp-file "dorg-gs-missing-" t)))
           (missing (expand-file-name "missing-sync" missing-parent)))
      (unwind-protect
          (let ((res (dorg-git-sync
                      (dorg-git-sync-tests--config
                       root :sync-worktree-path missing))))
            (should (eq (dorg-git-sync-result-status res) :failed))
            (should (eq (dorg-git-sync-result-reason res)
                        :worktree-contention))
            (should-not (dorg-git--branch-exists-p top "dorg/sync")))
        (delete-directory missing-parent t)))))

(ert-deftest dorg-test-git-sync-adopted-worktree-identity-mismatch-refuses-before-write ()
  "A foreign mirror in an adopted worktree is refused before copy/stage.
Adoption proves only same repo and branch; the identity guard still owns
content safety."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync--create-orphan-branch top "dorg/sync")
    (let ((existing (expand-file-name "tracker-sync" (dorg-git--common-dir top))))
      (unwind-protect
          (progn
            (should (zerop (car (dorg-git--run top "worktree" "add"
                                              existing "dorg/sync"))))
            (dorg-git-sync-tests--write
             (expand-file-name ".tracker/beads.org" existing)
             (dorg-git-sync-tests--store-content "store-B" "* Foreign\n"))
            (dorg-git--run existing "add" ".tracker/beads.org")
            (dorg-git--run existing "commit" "-m" "foreign mirror")
            (let ((mirror-before (dorg-git-sync-tests--git-blob
                                  top "dorg/sync:.tracker/beads.org")))
              (dorg-git-sync-tests--write
               root (dorg-git-sync-tests--store-content "store-A" "* Source\n"))
              (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
                (should (eq (dorg-git-sync-result-status res) :failed))
                (should (eq (dorg-git-sync-result-reason res)
                            :identity-mismatch))
                (should (equal (dorg-git-sync-result-identity res)
                               '(:source "store-A" :mirror "store-B")))
                (should (equal mirror-before
                               (dorg-git-sync-tests--git-blob
                                top "dorg/sync:.tracker/beads.org"))))))
        (when (file-directory-p existing)
          (delete-directory existing t))))))

(ert-deftest dorg-test-git-sync-worktree-repair-preserves-checkout ()
  "A dangling worktree with a surviving admin entry is repaired in place.
The sentinel file survives, proving repair (not rebuild) ran and prune
did NOT run before the repair attempt."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top _root
    (let* ((wt (dorg-git-sync--ensure-worktree top "dorg/sync" ".tracker"))
           (sentinel (expand-file-name "SENTINEL" wt)))
      (should (dorg-git-sync--worktree-valid-p top wt))
      (with-temp-file sentinel (insert "keep"))
      ;; Corrupt linkage but keep the admin entry.
      (with-temp-file (expand-file-name ".git" wt)
        (insert "gitdir: /nonexistent/path\n"))
      (should-not (dorg-git-sync--worktree-valid-p top wt))
      (dorg-git-sync--ensure-worktree top "dorg/sync" ".tracker")
      (should (dorg-git-sync--worktree-valid-p top wt))
      (should (file-exists-p sentinel)))))

(ert-deftest dorg-test-git-sync-worktree-rebuild-when-unrepairable ()
  "Without an admin entry the stale checkout is rebuilt (and pruned)."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top _root
    (let* ((wt (dorg-git-sync--ensure-worktree top "dorg/sync" ".tracker"))
           (sentinel (expand-file-name "SENTINEL" wt))
           (common (dorg-git--common-dir top))
           (admin (expand-file-name "worktrees/dorg-sync" common)))
      (with-temp-file sentinel (insert "keep"))
      (delete-directory admin t)
      (with-temp-file (expand-file-name ".git" wt) (insert "gitdir: /nope\n"))
      (should-not (dorg-git-sync--worktree-valid-p top wt))
      (dorg-git-sync--ensure-worktree top "dorg/sync" ".tracker")
      (should (dorg-git-sync--worktree-valid-p top wt))
      (should-not (file-exists-p sentinel)))))

;;; ─── D. Explicit mode: disabled / no-git / no-op ───────────────────────

(ert-deftest dorg-test-git-sync-disabled-skips-before-git ()
  "`:disabled' returns :skipped :disabled even in a NON-git directory."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-nogit-" t)))
         (root (expand-file-name "beads.org" plain)))
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root (dorg-git-sync-tests--store-content "x"))
          (let ((res (dorg-git-sync
                      (dorg-git-sync-tests--config root :mode :disabled))))
            (should (eq (dorg-git-sync-result-status res) :skipped))
            (should (eq (dorg-git-sync-result-reason res) :disabled))))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-nil-mode-misconfigured ()
  "An unset mode is a misconfiguration, distinct from :disabled."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root :mode nil))))
      (should (eq (dorg-git-sync-result-status res) :skipped))
      (should (eq (dorg-git-sync-result-reason res) :misconfigured)))))

(ert-deftest dorg-test-git-sync-branch-nil-never-current-commit ()
  "`branch' nil/empty never triggers a current-branch commit (overloaded-nil)."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "commit" "--allow-empty" "-m" "seed")
    (let ((head-before (dorg-git--ok top "rev-parse" "HEAD")))
      ;; :sync-branch with no branch -> misconfigured, NO commit anywhere.
      (let ((res (dorg-git-sync
                  (dorg-git-sync-tests--config root :mode :sync-branch
                                               :branch nil))))
        (should (eq (dorg-git-sync-result-reason res) :misconfigured)))
      (should (equal head-before (dorg-git--ok top "rev-parse" "HEAD")))
      ;; empty branch likewise.
      (should (eq (dorg-git-sync-result-reason
                   (dorg-git-sync
                    (dorg-git-sync-tests--config root :mode :sync-branch
                                                 :branch "")))
                  :misconfigured))
      ;; nil mode + nil branch likewise.
      (should (eq (dorg-git-sync-result-reason
                   (dorg-git-sync
                    (dorg-git-sync-tests--config root :mode nil :branch nil)))
                  :misconfigured)))))

(ert-deftest dorg-test-git-sync-current-branch-commits-in-place ()
  "`:current-branch' commits source files on the current branch, no worktree."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "add" "-A")
    (dorg-git--run top "commit" "-m" "seed") ; HEAD now has store-A beads.org
    ;; Modify the source (same identity).
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "store-A" "* Root\n* New\n"))
    (let* ((current (dorg-git--current-branch top))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config root :mode :current-branch
                                              :branch nil))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (equal (dorg-git-sync-result-branch res) current))
      ;; No sync worktree created on the current-branch path.
      (should-not (file-exists-p
                   (expand-file-name "dorg-git-sync"
                                     (dorg-git--common-dir top))))
      ;; Up-to-date second sync -> :no-changes.  CHARACTERIZATION (pins the
      ;; no-change slots before the `--stage-and-commit' helper extraction,
      ;; tracked-case): current-branch no-change carries NO :identity and the
      ;; detail "working tree up to date".  Must stay green after extraction.
      (let ((res2 (dorg-git-sync
                   (dorg-git-sync-tests--config root :mode :current-branch
                                                :branch nil))))
        (should (eq (dorg-git-sync-result-status res2) :skipped))
        (should (eq (dorg-git-sync-result-reason res2) :no-changes))
        (should (null (dorg-git-sync-result-identity res2)))
        (should (equal (dorg-git-sync-result-detail res2)
                       "working tree up to date"))))))

(ert-deftest dorg-test-git-sync-current-branch-detached-head-skips ()
  "`:current-branch' on a detached HEAD makes no commit and skips structured.
Regression: a detached checkout has no current branch, so committing would
leave a dangling/unreachable commit reported as branch \"HEAD\".  It must
return :skipped :misconfigured with no commit and no \"HEAD\" branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "add" "-A")
    (dorg-git--run top "commit" "-m" "seed")
    (let ((head-before (dorg-git--ok top "rev-parse" "HEAD")))
      ;; Detach HEAD at the seed commit.
      (dorg-git--run top "checkout" "--detach" head-before)
      ;; Modify the source (same identity) so there would be something to commit.
      (dorg-git-sync-tests--write
       root (dorg-git-sync-tests--store-content "store-A" "* Root\n* New\n"))
      (let ((res (dorg-git-sync
                  (dorg-git-sync-tests--config root :mode :current-branch
                                               :branch nil))))
        (should (eq (dorg-git-sync-result-status res) :skipped))
        (should (eq (dorg-git-sync-result-reason res) :misconfigured))
        (should-not (equal (dorg-git-sync-result-branch res) "HEAD"))
        (should-not (dorg-git-sync-result-commit res))
        ;; No new commit landed: HEAD still points at the seed commit.
        (should (equal (dorg-git--ok top "rev-parse" "HEAD") head-before))))))

(ert-deftest dorg-test-git-sync-not-a-git-repo-skips ()
  "A non-git root (non-disabled mode) yields :skipped :not-a-git-repo."
  (skip-unless (executable-find "git"))
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-nogit-" t)))
         (root (expand-file-name "beads.org" plain)))
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root (dorg-git-sync-tests--store-content "x"))
          (let ((res (dorg-git-sync
                      (dorg-git-sync-tests--config root :mode :sync-branch
                                                   :branch "dorg/sync"))))
            (should (eq (dorg-git-sync-result-status res) :skipped))
            (should (eq (dorg-git-sync-result-reason res) :not-a-git-repo))))
      (delete-directory plain t))))

;;; ─── E. Atomic copy/commit ─────────────────────────────────────────────

;;; ─── Dolt materialization publication policy ────────────────────────────

(ert-deftest dorg-test-git-sync-dolt-marker-default-disabled-before-git ()
  "A Dolt-bound materialization is disabled before any Git access."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-dolt-" t)))
         (root (expand-file-name "beads.org" plain))
         git-called)
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root
           (concat "#+DORG_DOLT_DIR: ./dolt\n"
                   (dorg-git-sync-tests--store-content "store-A")))
          (cl-letf (((symbol-function 'dorg-git--repo-p)
                     (lambda (&rest _)
                       (setq git-called t)
                       (error "Git must not be consulted"))))
            (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
              (should (eq (dorg-git-sync-result-status res) :skipped))
              (should (eq (dorg-git-sync-result-reason res)
                          :dolt-export-disabled))
              (should-not git-called))))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-empty-dolt-marker-is-plain-org ()
  "An empty preamble Dolt marker does not classify the root as Dolt-backed.
This subsumes the b7gcj bounded scanner rule that the current/legacy
preamble key must have a non-empty value."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root
     (concat "#+DORG_DOLT_DIR:   \n"
             (dorg-git-sync-tests--store-content "store-A"
                                                 "* Empty marker\n")))
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (eq (dorg-git-sync-result-reason res) :committed))
      (should (string-match-p
               "Empty marker"
               (dorg-git-sync-tests--git-blob
                top "dorg/sync:.tracker/beads.org"))))))

(ert-deftest dorg-test-git-sync-empty-legacy-dorg-dir-marker-is-plain-org ()
  "An empty legacy preamble Dolt marker does not classify as Dolt-backed."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root
     (concat "#+DORG_DIR:   \n"
             (dorg-git-sync-tests--store-content "store-A"
                                                 "* Empty legacy marker\n")))
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (eq (dorg-git-sync-result-reason res) :committed))
      (should (string-match-p
               "Empty legacy marker"
               (dorg-git-sync-tests--git-blob
                top "dorg/sync:.tracker/beads.org"))))))

(ert-deftest dorg-test-git-sync-dolt-override-default-disabled ()
  "Fresh override bytes classify Dolt even when the disk file is plain."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-dolt-" t)))
         (root (expand-file-name "beads.org" plain))
         (fresh (concat "#+DORG_DOLT_DIR: ./dolt\n"
                        (dorg-git-sync-tests--store-content
                         "store-A" "* Fresh Dolt snapshot\n"))))
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root (dorg-git-sync-tests--store-content "store-A" "* Stale disk\n"))
          (cl-letf (((symbol-function 'dorg-git--repo-p)
                     (lambda (&rest _) (error "Git must not be consulted"))))
            (let ((res
                   (dorg-git-sync
                    (dorg-git-sync-tests--config
                     root :source-content-overrides (list (cons root fresh))))))
              (should (eq (dorg-git-sync-result-reason res)
                          :dolt-export-disabled)))))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-explicit-dolt-backend-default-disabled ()
  "Explicit backend identity blocks an unmarked Dolt materialization."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-dolt-" t)))
         (root (expand-file-name "beads.org" plain)))
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root (dorg-git-sync-tests--store-content "store-A"))
          (cl-letf (((symbol-function 'dorg-git--repo-p)
                     (lambda (&rest _) (error "Git must not be consulted"))))
            (should
             (eq (dorg-git-sync-result-reason
                  (dorg-git-sync
                   (dorg-git-sync-tests--config root :backend-type 'dolt)))
                 :dolt-export-disabled))))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-body-dolt-literal-remains-plain-org ()
  "A body literal is not a root binding and must not disable publication."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root
     (dorg-git-sync-tests--store-content
      "store-A" "* Root\n#+DORG_DOLT_DIR: body literal\n"))
    (let ((res (dorg-git-sync (dorg-git-sync-tests--config root))))
      (should (eq (dorg-git-sync-result-status res) :committed)))))

(ert-deftest dorg-test-git-sync-dolt-explicit-export-commits-effective-bytes ()
  "Explicit Dolt export publishes fresh bytes to a dedicated sync branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((fresh (concat "#+DORG_DOLT_DIR: ./dolt\n"
                          (dorg-git-sync-tests--store-content
                           "store-A" "* Fresh exported snapshot\n")))
           (res
            (dorg-git-sync
             (dorg-git-sync-tests--config
              root
              :allow-dolt-export t
              :source-content-overrides (list (cons root fresh))))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (equal (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")
                     (string-trim-right fresh))))))

(ert-deftest dorg-test-git-sync-dolt-current-branch-refused-even-opted-in ()
  "Dolt snapshots never commit onto the current source-code branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "add" "-A")
    (dorg-git--run top "commit" "-m" "seed")
    (let ((head-before (dorg-git--ok top "rev-parse" "HEAD")))
      (dorg-git-sync-tests--write
       root
       (concat "#+DORG_DOLT_DIR: ./dolt\n"
               (dorg-git-sync-tests--store-content
                "store-A" "* Changed snapshot\n")))
      (let ((res
             (dorg-git-sync
              (dorg-git-sync-tests--config
               root :mode :current-branch :branch nil
               :allow-dolt-export t))))
        (should (eq (dorg-git-sync-result-status res) :skipped))
        (should (eq (dorg-git-sync-result-reason res) :misconfigured))
        (should (equal head-before (dorg-git--ok top "rev-parse" "HEAD")))))))

(ert-deftest dorg-test-git-sync-dolt-push-default-disabled-before-git ()
  "Push cannot bypass default-disabled Dolt publication and never reaches Git."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-dolt-" t)))
         (root (expand-file-name "beads.org" plain))
         git-called)
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root
           (concat "#+DORG_DOLT_DIR: ./dolt\n"
                   (dorg-git-sync-tests--store-content "store-A")))
          (cl-letf (((symbol-function 'dorg-git--repo-p)
                     (lambda (&rest _)
                       (setq git-called t)
                       (error "Git must not be consulted"))))
            (let ((push (dorg-git-sync-push (dorg-git-sync-tests--config root))))
              (should (eq (dorg-git-sync-result-status push) :skipped))
              (should (eq (dorg-git-sync-result-reason push)
                          :dolt-export-disabled))
              (should-not git-called))))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-dolt-status-default-disabled-before-git ()
  "Status is gated by the Dolt policy before any status-path Git helper runs.

Non-vacuity: `dorg-git-sync-status' reaches Git through
`dorg-git--current-branch', `dorg-git--branch-exists-p', and
`dorg-git--ok' — never `dorg-git--repo-p'.  Each is stubbed to SIGNAL if
consulted, so a nil result cannot be a non-git-directory harness artifact:
it can only mean the centralized Dolt policy gate returned nil before Git
access.  This fails today because status consults Git before any Dolt gate."
  (let* ((plain (file-name-as-directory (make-temp-file "dorg-gs-dolt-" t)))
         (root (expand-file-name "beads.org" plain))
         git-called)
    (unwind-protect
        (progn
          (dorg-git-sync-tests--write
           root
           (concat "#+DORG_DOLT_DIR: ./dolt\n"
                   (dorg-git-sync-tests--store-content "store-A")))
          (cl-letf* ((signal-git
                      (lambda (&rest _)
                        (setq git-called t)
                        (error "Git must not be consulted")))
                     ((symbol-function 'dorg-git--current-branch) signal-git)
                     ((symbol-function 'dorg-git--branch-exists-p) signal-git)
                     ((symbol-function 'dorg-git--ok) signal-git))
            (should-not (dorg-git-sync-status (dorg-git-sync-tests--config root)))
            (should-not git-called)))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-sync-single-commit-all-source-files ()
  "One commit on the sync branch contains all source files."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((extra (expand-file-name ".tracker/archive.org" top)))
      (dorg-git-sync-tests--write extra "#+TITLE: Archive\n\n* Done\n")
      (let ((res (dorg-git-sync
                  (dorg-git-sync-tests--config root
                                               :sources (list root extra)))))
        (should (eq (dorg-git-sync-result-status res) :committed))
        (let ((names (dorg-git--ok top "show" "--name-only" "--format="
                                  "dorg/sync")))
          (should (string-match-p "beads\\.org" names))
          (should (string-match-p "archive\\.org" names)))
        (should (equal (dorg-git-sync-result-commit res)
                       (dorg-git--ok top "rev-parse" "dorg/sync")))))))

(ert-deftest dorg-test-git-sync-source-content-override-writes-bytes-not-disk ()
  "A matching source override publishes override text, not stale disk bytes."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "store-A" "* STALE-DISK\n"))
    (let* ((fresh (dorg-git-sync-tests--store-content "store-A"
                                                       "* FRESH-OVERRIDE\n"))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config
                  root :source-content-overrides
                  (list (cons (expand-file-name root) fresh))))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
        (should (string-match-p "FRESH-OVERRIDE" mirror))
        (should-not (string-match-p "STALE-DISK" mirror))))))

(ert-deftest dorg-test-git-sync-source-content-override-unmatched-entry-ignored ()
  "An override whose path is not in source-files is ignored."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync-tests--write
     root (dorg-git-sync-tests--store-content "store-A" "* DISK\n"))
    (let* ((other (expand-file-name ".tracker/other.org" top))
           (override (dorg-git-sync-tests--store-content
                      "store-A" "* OVERRIDE\n"))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config
                  root :source-content-overrides
                  (list (cons other override))))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should-not (dorg-git-sync-result-warnings res))
      (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
        (should (string-match-p "DISK" mirror))
        (should-not (string-match-p "OVERRIDE" mirror))))))

(ert-deftest dorg-test-git-sync-source-content-override-root-identity-refuses-foreign-before-write ()
  "A foreign root identity in override text refuses before mirror mutation."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let* ((sha1 (dorg-git--ok top "rev-parse" "dorg/sync"))
           (foreign (dorg-git-sync-tests--store-content
                     "store-B" "* FOREIGN\n"))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config
                  root :source-content-overrides
                  (list (cons (expand-file-name root) foreign))))))
      (should (eq (dorg-git-sync-result-status res) :failed))
      (should (eq (dorg-git-sync-result-reason res) :identity-mismatch))
      (should (equal (dorg-git-sync-result-identity res)
                     '(:source "store-B" :mirror "store-A")))
      (should (equal sha1 (dorg-git--ok top "rev-parse" "dorg/sync")))
      (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
        (should (string-match-p "store-A" mirror))
        (should-not (string-match-p "FOREIGN" mirror))))))

(ert-deftest dorg-test-git-sync-source-content-override-sibling-copy-still-uses-disk ()
  "A sibling source without an override is still copied from disk."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((archive (expand-file-name ".tracker/archive.org" top))
           (fresh (dorg-git-sync-tests--store-content "store-A"
                                                       "* ROOT-OVERRIDE\n")))
      (dorg-git-sync-tests--write root
                                  (dorg-git-sync-tests--store-content
                                   "store-A" "* ROOT-DISK\n"))
      (dorg-git-sync-tests--write archive "#+TITLE: Archive\n\n* ARCHIVE-DISK\n")
      (let ((res (dorg-git-sync
                  (dorg-git-sync-tests--config
                   root :sources (list root archive)
                   :source-content-overrides
                   (list (cons (expand-file-name root) fresh))))))
        (should (eq (dorg-git-sync-result-status res) :committed))
        (let ((root-mirror (dorg-git--ok top "show"
                                         "dorg/sync:.tracker/beads.org"))
              (archive-mirror (dorg-git--ok top "show"
                                            "dorg/sync:.tracker/archive.org")))
          (should (string-match-p "ROOT-OVERRIDE" root-mirror))
          (should-not (string-match-p "ROOT-DISK" root-mirror))
          (should (string-match-p "ARCHIVE-DISK" archive-mirror)))))))

(ert-deftest dorg-test-git-sync-source-content-override-no-change-detection-uses-effective-bytes ()
  "No-change detection compares the effective override bytes, not disk bytes."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((effective (dorg-git-sync-tests--store-content
                       "store-A" "* EFFECTIVE\n"))
           (overrides (list (cons (expand-file-name root) effective))))
      (dorg-git-sync (dorg-git-sync-tests--config
                      root :source-content-overrides overrides))
      (let ((sha1 (dorg-git--ok top "rev-parse" "dorg/sync")))
        (dorg-git-sync-tests--write
         root (dorg-git-sync-tests--store-content "store-A" "* STALE-DISK-2\n"))
        (let ((res (dorg-git-sync
                    (dorg-git-sync-tests--config
                     root :source-content-overrides overrides))))
          (should (eq (dorg-git-sync-result-status res) :skipped))
          (should (eq (dorg-git-sync-result-reason res) :no-changes))
          (should (equal sha1 (dorg-git--ok top "rev-parse" "dorg/sync"))))))))

(ert-deftest dorg-test-git-sync-source-content-override-root-identity-missing-refuses-before-write ()
  "A root override without identity refuses; disk identity is not a fallback."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let* ((sha1 (dorg-git--ok top "rev-parse" "dorg/sync"))
           (no-id "#+TITLE: Test Store\n\n* NO-ID-OVERRIDE\n")
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config
                  root :source-content-overrides
                  (list (cons (expand-file-name root) no-id))))))
      (should (eq (dorg-git-sync-result-status res) :failed))
      (should (eq (dorg-git-sync-result-reason res) :identity-mismatch))
      (should (equal (dorg-git-sync-result-identity res)
                     '(:source nil :mirror "store-A")))
      (should (equal sha1 (dorg-git--ok top "rev-parse" "dorg/sync")))
      (let ((mirror (dorg-git--ok top "show" "dorg/sync:.tracker/beads.org")))
        (should (string-match-p "store-A" mirror))
        (should-not (string-match-p "NO-ID-OVERRIDE" mirror))))))

(ert-deftest dorg-test-git-sync-file-copy-error-warns-and-continues ()
  "A per-file copy error is recorded as a warning; other files still commit."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((missing (expand-file-name ".tracker/missing.org" top))
           (res (dorg-git-sync
                 (dorg-git-sync-tests--config root
                                              :sources (list root missing)))))
      (should (eq (dorg-git-sync-result-status res) :committed))
      (should (cl-some (lambda (w) (eq (plist-get w :kind) 'file-copy-failed))
                       (dorg-git-sync-result-warnings res))))))

(ert-deftest dorg-test-git-sync-write-guard-blocks-out-of-root-dest ()
  "An armed write guard signals before copying a dest outside its roots."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let* ((guard (file-name-as-directory (make-temp-file "dorg-gs-guard-" t)))
           (dorg-git-sync--write-guard guard))
      (unwind-protect
          (should-error (dorg-git-sync (dorg-git-sync-tests--config root)))
        (delete-directory guard t)))))

;;; ─── F. Push & status ──────────────────────────────────────────────────

(ert-deftest dorg-test-git-sync-push-advances-remote ()
  "`dorg-git-sync-push' advances the remote ref for the effective branch."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((remote (file-name-as-directory (make-temp-file "dorg-gs-remote-" t))))
      (unwind-protect
          (progn
            (dorg-git--run remote "init" "--bare" "-q")
            (dorg-git--run top "remote" "add" "origin" remote)
            (dorg-git-sync (dorg-git-sync-tests--config root))
            (let ((res (dorg-git-sync-push (dorg-git-sync-tests--config root))))
              (should (eq (dorg-git-sync-result-status res) :committed))
              ;; Verify via ls-remote from the working repo — a bare repo
              ;; cannot be opened directly under safe.bareRepository=explicit.
              (let ((ls (dorg-git--ok top "ls-remote" remote
                                     "refs/heads/dorg/sync")))
                (should ls)
                (should-not (string-empty-p ls)))))
        (delete-directory remote t)))))

(ert-deftest dorg-test-git-sync-status-sync-branch-only ()
  "Status returns a log summary in :sync-branch mode, nil otherwise."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (dorg-git--run top "commit" "--allow-empty" "-m" "seed")
    (dorg-git-sync (dorg-git-sync-tests--config root))
    (let ((s (dorg-git-sync-status (dorg-git-sync-tests--config root))))
      (should (stringp s))
      (should-not (string-empty-p s)))
    (should-not (dorg-git-sync-status
                 (dorg-git-sync-tests--config root :mode :current-branch)))))

(ert-deftest dorg-test-git-sync-auto-push-only-on-commit ()
  "Auto-push fires after a commit but never after a skipped (no-changes) sync."
  (skip-unless (executable-find "git"))
  (dorg-git-sync-tests--with-repo top root
    (let ((remote (file-name-as-directory (make-temp-file "dorg-gs-remote-" t))))
      (unwind-protect
          (progn
            (dorg-git--run remote "init" "--bare" "-q")
            (dorg-git--run top "remote" "add" "origin" remote)
            (dorg-git-sync (dorg-git-sync-tests--config root :push t))
            (let ((before (dorg-git--ok top "ls-remote" remote
                                       "refs/heads/dorg/sync")))
              (should before)
              (should-not (string-empty-p before))
              (let ((res (dorg-git-sync
                          (dorg-git-sync-tests--config root :push t))))
                (should (eq (dorg-git-sync-result-status res) :skipped))
                ;; A skipped (no-changes) sync must not push: ref unchanged.
                (should (equal before (dorg-git--ok top "ls-remote" remote
                                                   "refs/heads/dorg/sync"))))))
        (delete-directory remote t)))))


;;; ─── Maintenance dispatch: the committer dispatches ────────────────────

;; The party that lands a sync-branch commit is the party that launches
;; maintenance.  This facade is one of two such parties — a standalone DOrg
;; caller never passes through a consumer, so a consumer-side trigger alone
;; would never fire for it.

(ert-deftest dorg-test-git-sync-dispatches-maintenance-after-sync-branch-commit ()
  "A landed sync-branch commit launches exactly one worker, with its SHA."
  (dorg-git-sync-tests--with-repo top root
    (let ((calls nil))
      (cl-letf (((symbol-function 'dorg-git-maintenance-dispatch)
                 (lambda (&rest args) (push args calls) nil)))
        (let ((result (dorg-git-sync (dorg-git-sync-tests--config root))))
          (should (eq :committed (dorg-git-sync-result-status result)))
          (should (= 1 (length calls)))
          (pcase-let ((`(,repo ,common ,branch ,commit) (car calls)))
            ;; REPO is a directory inside this repository (the facade passes
            ;; the authority file's directory, which git resolves from), and
            ;; COMMON is that repository's own common git dir.
            (should (string-prefix-p (file-truename top)
                                     (file-truename (expand-file-name repo))))
            (should (file-directory-p common))
            (should (equal (file-truename common)
                           (file-truename (dorg-git--common-dir repo))))
            (should (equal branch (dorg-git-sync-result-branch result)))
            ;; The SHA the facade captured, not a ref read back later.
            (should (equal commit (dorg-git-sync-result-commit result)))))))))

(ert-deftest dorg-test-git-sync-does-not-dispatch-maintenance-on-current-branch ()
  "A successful CURRENT-branch commit is a negative dispatch control."
  (dorg-git-sync-tests--with-repo top root
    (ignore top)
    (let ((calls 0))
      (cl-letf (((symbol-function 'dorg-git-maintenance-dispatch)
                 (lambda (&rest _) (cl-incf calls) nil)))
        (let ((result (dorg-git-sync
                       (dorg-git-sync-tests--config root :mode :current-branch))))
          (should (eq :committed (dorg-git-sync-result-status result)))
          (should (= 0 calls)))))))

(ert-deftest dorg-test-git-sync-does-not-dispatch-maintenance-without-a-commit ()
  "No commit, no dispatch: a second identical sync has nothing to publish."
  (dorg-git-sync-tests--with-repo top root
    (ignore top)
    (let ((calls 0))
      (dorg-git-sync (dorg-git-sync-tests--config root))
      (cl-letf (((symbol-function 'dorg-git-maintenance-dispatch)
                 (lambda (&rest _) (cl-incf calls) nil)))
        (let ((result (dorg-git-sync (dorg-git-sync-tests--config root))))
          (should-not (eq :committed (dorg-git-sync-result-status result)))
          (should (= 0 calls)))))))

(provide 'dorg-git-sync-tests)
;;; dorg-git-sync-tests.el ends here
