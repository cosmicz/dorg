;;; dorg-git.el --- Generic git primitives for DOrg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Thin, backend-neutral git wrappers used by DOrg's storage layer.
;;
;; Every entry point takes an EXPLICIT working directory and never relies
;; on an implicit `default-directory' global beyond the directory passed
;; in.  This is the generic half of a prior tracker's git helpers, with
;; the project/tracker knowledge removed so DOrg owns no domain or
;; tracker vocabulary.
;;
;; Internal helpers:
;; - `dorg-git--run'            - Run git in DIR, return (EXIT-CODE . OUTPUT)
;; - `dorg-git--ok'             - Run git in DIR, return output or nil
;; - `dorg-git--repo-p'         - Whether DIR is inside a git repository
;; - `dorg-git--common-dir'     - Absolute path to the shared (common) .git dir
;; - `dorg-git--current-branch' - Current branch name in DIR
;; - `dorg-git--branch-exists-p'- Whether a local branch exists

;;; Code:

(eval-when-compile (require 'subr-x))

(defun dorg-git--run (dir &rest args)
  "Run git with ARGS synchronously from DIR, return (EXIT-CODE . OUTPUT).
OUTPUT is the trimmed combined stdout/stderr.  DIR is explicit; no
implicit cwd is consulted."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "git" nil t nil args)))
        (cons exit-code (string-trim (buffer-string)))))))

(defun dorg-git--ok (dir &rest args)
  "Run git with ARGS in DIR; return the output string on success, else nil."
  (let ((result (apply #'dorg-git--run dir args)))
    (when (zerop (car result))
      (cdr result))))

(defun dorg-git--repo-p (dir)
  "Return non-nil when DIR is inside a git repository."
  (and (dorg-git--ok dir "rev-parse" "--git-dir") t))

(defun dorg-git--common-dir (dir)
  "Return the absolute path to the shared (common) .git directory for DIR.
In a linked worktree `--git-common-dir' points at the repository's
singleton .git rather than the per-worktree git dir, so resources shared
across all worktrees resolve to a single instance.  Git may return a
relative path (\".git\") from the main worktree; it is expanded against
DIR so the result is always absolute.  Returns nil when DIR is not a
git repository."
  (when-let ((common (dorg-git--ok dir "rev-parse" "--git-common-dir")))
    (expand-file-name common dir)))

(defun dorg-git--current-branch (dir)
  "Return the name of the current git branch in DIR, or nil if detached.
Uses `git symbolic-ref' rather than `rev-parse --abbrev-ref HEAD':
the latter prints the literal string \"HEAD\" on a detached checkout,
which would be mistaken for a real branch.  `symbolic-ref --quiet'
exits non-zero (so `dorg-git--ok' yields nil) when HEAD is detached."
  (dorg-git--ok dir "symbolic-ref" "--quiet" "--short" "HEAD"))

(defun dorg-git--branch-exists-p (dir branch)
  "Return non-nil when local BRANCH exists in DIR's repository."
  (and (dorg-git--ok dir "rev-parse" "--verify" (format "refs/heads/%s" branch))
       t))

(provide 'dorg-git)
;;; dorg-git.el ends here
