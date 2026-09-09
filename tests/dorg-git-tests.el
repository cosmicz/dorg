;;; dorg-git-tests.el --- Tests for dorg-git -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the generic git wrappers in `dorg-git.el'.  Each test
;; runs against a throwaway `git init' temp repository and asserts the
;; wrappers operate against an EXPLICIT directory with no implicit cwd.

;;; Code:

(require 'ert)
(require 'dorg-git)

(defmacro dorg-git-tests--with-repo (dir-var &rest body)
  "Create a throwaway git repo, bind DIR-VAR to its toplevel, run BODY.
The repo is removed afterwards.  Identity/signing config is set so
commits succeed in any CI environment."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir-var (file-name-as-directory (make-temp-file "dorg-git-test-" t))))
     (unwind-protect
         (progn
           (dorg-git--run ,dir-var "init" "-q")
           (dorg-git--run ,dir-var "config" "user.email" "test@example.com")
           (dorg-git--run ,dir-var "config" "user.name" "DOrg Test")
           (dorg-git--run ,dir-var "config" "commit.gpgsign" "false")
           ,@body)
       (delete-directory ,dir-var t))))

(ert-deftest dorg-test-git-run-returns-exit-and-output ()
  "`dorg-git--run' returns (EXIT . OUTPUT); failures carry a non-zero exit."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (let ((ok (dorg-git--run dir "rev-parse" "--git-dir")))
      (should (zerop (car ok)))
      (should (stringp (cdr ok)))
      (should (not (string-empty-p (cdr ok)))))
    (let ((bad (dorg-git--run dir "no-such-subcommand")))
      (should-not (zerop (car bad))))))

(ert-deftest dorg-test-git-ok-output-or-nil ()
  "`dorg-git--ok' returns output on success and nil on failure."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (should (stringp (dorg-git--ok dir "rev-parse" "--git-dir")))
    (should-not (dorg-git--ok dir "no-such-subcommand"))))

(ert-deftest dorg-test-git-repo-p ()
  "`dorg-git--repo-p' is non-nil inside a repo, nil outside one."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (should (dorg-git--repo-p dir)))
  (let ((plain (file-name-as-directory (make-temp-file "dorg-git-plain-" t))))
    (unwind-protect
        (should-not (dorg-git--repo-p plain))
      (delete-directory plain t))))

(ert-deftest dorg-test-git-current-branch ()
  "`dorg-git--current-branch' returns the checked-out branch name."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (dorg-git--run dir "commit" "--allow-empty" "-m" "init")
    (let ((branch (dorg-git--current-branch dir)))
      (should (stringp branch))
      (should (not (string-empty-p branch)))
      (should-not (equal branch "HEAD")))))

(ert-deftest dorg-test-git-current-branch-detached-returns-nil ()
  "`dorg-git--current-branch' returns nil on a detached HEAD.
Regression: `rev-parse --abbrev-ref HEAD' prints the literal \"HEAD\"
when detached, which must NOT be reported as a branch name."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (dorg-git--run dir "commit" "--allow-empty" "-m" "init")
    (let ((sha (dorg-git--ok dir "rev-parse" "HEAD")))
      (dorg-git--run dir "checkout" "--detach" sha)
      (should-not (dorg-git--current-branch dir)))))

(ert-deftest dorg-test-git-branch-exists-p ()
  "`dorg-git--branch-exists-p' detects present vs absent local branches."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (dorg-git--run dir "commit" "--allow-empty" "-m" "init")
    (dorg-git--run dir "branch" "feature/x")
    (should (dorg-git--branch-exists-p dir "feature/x"))
    (should-not (dorg-git--branch-exists-p dir "nope/missing"))))

(ert-deftest dorg-test-git-common-dir-from-linked-worktree ()
  "`dorg-git--common-dir' resolves to the SAME shared dir from a linked worktree."
  (skip-unless (executable-find "git"))
  (dorg-git-tests--with-repo dir
    (dorg-git--run dir "commit" "--allow-empty" "-m" "init")
    (let ((wt (expand-file-name "linked"
                                (file-name-as-directory
                                 (make-temp-file "dorg-git-wt-" t)))))
      (unwind-protect
          (progn
            (let ((r (dorg-git--run dir "worktree" "add" "-b" "lw" wt)))
              (should (zerop (car r))))
            ;; Linked worktree's per-worktree git dir differs, but the
            ;; common dir must equal the main worktree's common dir.
            (should (file-equal-p (dorg-git--common-dir wt)
                                  (dorg-git--common-dir dir))))
        (when (file-directory-p (file-name-directory (directory-file-name wt)))
          (delete-directory (file-name-directory (directory-file-name wt)) t))))))

(provide 'dorg-git-tests)
;;; dorg-git-tests.el ends here
