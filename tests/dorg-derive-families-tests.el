;;; dorg-derive-families-tests.el --- Derived pre-ACCEPT test families -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Specifications for tools/dorg-derive-families.el, which derives the test
;; files a candidate must pass from its changes against the merge-base and
;; runs them as whole files through the real runner in a child Emacs.
;;
;; Most specs build a small synthetic git repository, commit a change and
;; compare the EXACT selected file set.  The repository carries stub files
;; for every census and behaviour-family test name, so expected sets include
;; the census entries that legitimately fire.  Historical specs replay real
;; commits of this repository and run only where their objects exist.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst dorg-dft--repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load.")

(defconst dorg-dft--tool
  (expand-file-name "tools/dorg-derive-families.el" dorg-dft--repo-root)
  "The tool under test, always loaded from source.")

(load dorg-dft--tool t t t)

(declare-function dorg-derive-families-records "tools/dorg-derive-families")
(declare-function dorg-derive-families-index "tools/dorg-derive-families")
(declare-function dorg-derive-families-select "tools/dorg-derive-families")
(declare-function dorg-derive-families--parse-name-status "tools/dorg-derive-families")
(declare-function dorg-derive-families-definitions "tools/dorg-derive-families")
(declare-function dorg-derive-families-flagged-enumerators "tools/dorg-derive-families")
(declare-function dorg-derive-families-undeclared-enumerators "tools/dorg-derive-families")
(defvar dorg-derive-families-census)
(defvar dorg-derive-families-exemptions)

;;; ─── Synthetic repositories ─────────────────────────────────────────────

(defconst dorg-dft--git-environment
  '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
  "Hermetic git configuration: no user or system hooks, templates or aliases.")

(defun dorg-dft--git (dir &rest args)
  "Run git ARGS in DIR; return stdout, or signal with its output.
Git runs without global or system configuration, so host hooks cannot refuse
the synthetic commits."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (process-environment (append dorg-dft--git-environment
                                        process-environment))
           (status (apply #'call-process "git" nil t nil
                          "-c" "user.name=dft" "-c" "user.email=dft@example.invalid"
                          "-c" "commit.gpgsign=false" "-c" "init.defaultBranch=main"
                          args)))
      (unless (eql status 0)
        (error "git %S failed (%S): %s" args status (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun dorg-dft--write (dir path content)
  "Write CONTENT to PATH under DIR, creating directories."
  (let ((file (expand-file-name path dir)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region content nil file nil 'silent))))

(defconst dorg-dft--alpha
  (concat
   ";;; dorg-alphamod.el --- alpha -*- lexical-binding: t; -*-\n"
   "(defun dorg-alphamod-run (x)\n"
   "  \"Run X.\n"
   "(defun dorg-fake-name ()\n"
   "is only docstring text.\"\n"
   "  (let ((y (* x 2)))\n"
   "    ;; a comment with (dorg-fake-name\n"
   "    (+ y 1)))\n"
   "\n"
   "(defun dorg-alphamod-run-extra () 2)\n"
   "\n"
   "(cl-defstruct (dorg-alphamod-rec (:constructor dorg-alphamod-rec-make))\n"
   "  a b)\n"
   "\n"
   "(cl-defmethod dorg-alphamod-show ((r dorg-alphamod-rec))\n"
   "  (list r))\n"
   "\n"
   "(defalias 'dorg-alphamod-alias #'dorg-alphamod-run)\n"
   "\n"
   ";; top-level comment\n"
   "(provide 'dorg-alphamod)\n")
  "A root module with every definition shape the R3 specs edit.")

(defconst dorg-dft--census-stubs
  '("dorg-backend-tests.el" "dorg-cli-tests.el" "dorg-dolt-tests.el"
    "dorg-faithful-tests.el" "dorg-grammar-tests.el" "dorg-install-tests.el"
    "dorg-model-tests.el" "dorg-mutate-linearize-tests.el" "dorg-project-tests.el"
    "dorg-projection-tests.el" "dorg-sql-gen-tests.el" "dorg-test-infra-tests.el"
    "dorg-mutate-persist-tests.el")
  "Stub test files named by the census and behaviour tables.")

(defconst dorg-dft--base-files
  `(("dorg-alphamod.el" . ,dorg-dft--alpha)
    ("tools/thing.sh" . "echo thing\n")
    ("tests/dorg-alphamod-tests.el" . ";; module tests\n")
    ("tests/dorg-alphamod-extra-tests.el" . ";; module family tests\n")
    ("tests/dorg-beta-tests.el" . "(dorg-alphamod-run 1)\n")
    ("tests/dorg-gamma-tests.el" . "(dorg-alphamod-run-extra)\n")
    ("tests/dorg-fake-tests.el" . "(dorg-fake-name)\n")
    ("tests/dorg-rec-tests.el" . "(cl-typep r 'dorg-alphamod-rec)\n")
    ("tests/dorg-show-tests.el" . "(dorg-alphamod-show r)\n")
    ("tests/dorg-alias-tests.el" . "(dorg-alphamod-alias 1)\n")
    ("tests/dorg-test-helpers.el"
     . "(defun dorg-test-help-me ()\n  1)\n(provide 'dorg-test-helpers)\n")
    ("tests/dorg-uses-helper-tests.el" . "(require 'dorg-test-helpers)\n")
    ("tests/dorg-calls-helper-tests.el" . "(dorg-test-help-me)\n")
    ("tests/fixtures/data-one/input.txt" . "one\n")
    ("tests/fixtures/data one/\u00fc.txt" . "two\n")
    ("tests/dorg-fixture-user-tests.el" . "\"fixtures/data-one\"\n")
    ("tests/dorg-fixture-space-tests.el" . "\"fixtures/data one\"\n")
    ("tests/dorg-path-tests.el" . "\"tools/thing.sh\"\n")
    ,@(mapcar (lambda (name) (cons (concat "tests/" name) ";; stub\n"))
              dorg-dft--census-stubs))
  "The synthetic repository's base commit.")

(defun dorg-dft--repo (&optional files)
  "Return a fresh synthetic repository holding FILES (default the base set).
The base commit is tagged `base'."
  (let ((dir (make-temp-file "dorg-dft-" t)))
    (dorg-dft--git dir "init" "-q")
    (dolist (file (or files dorg-dft--base-files))
      (dorg-dft--write dir (car file) (cdr file)))
    (dorg-dft--git dir "add" "-A")
    (dorg-dft--git dir "commit" "-q" "-m" "base")
    (dorg-dft--git dir "tag" "base")
    dir))

(defun dorg-dft--change (dir changes)
  "Apply CHANGES in DIR and commit; return the new commit id.
Each change is (PATH . CONTENT), (PATH . :delete) or (PATH :rename NEW)."
  (dolist (change changes)
    (pcase change
      (`(,path :rename ,new)
       (make-directory (file-name-directory (expand-file-name new dir)) t)
       (dorg-dft--git dir "mv" path new))
      (`(,path . :delete) (dorg-dft--git dir "rm" "-q" path))
      (`(,path . ,content) (dorg-dft--write dir path content))))
  (dorg-dft--git dir "add" "-A")
  (dorg-dft--git dir "commit" "-q" "-m" "change")
  (dorg-dft--git dir "rev-parse" "HEAD"))

(defun dorg-dft--edit (text from to)
  "Return TEXT with its single occurrence of FROM replaced by TO."
  (cl-assert (= 2 (length (split-string text (regexp-quote from)))))
  (string-replace from to text))

(defun dorg-dft--selected (dir &optional base cand)
  "Return the sorted selected test basenames for BASE..CAND in DIR."
  (let ((cand (or cand "HEAD")))
    (sort (mapcar #'car (dorg-derive-families-select
                         (dorg-derive-families-records dir (or base "base") cand)
                         (dorg-derive-families-index dir cand)))
          #'string<)))

(defun dorg-dft--with-repo-cleanup (dir thunk)
  "Call THUNK, then delete DIR."
  (unwind-protect (funcall thunk) (delete-directory dir t)))

(defmacro dorg-dft--with-repo (var &rest body)
  "Bind VAR to a fresh synthetic repository for BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (dorg-dft--repo)))
     (dorg-dft--with-repo-cleanup ,var (lambda () ,@body))))

(defconst dorg-dft--c1
  '("dorg-backend-tests.el" "dorg-cli-tests.el" "dorg-dolt-tests.el"
    "dorg-faithful-tests.el" "dorg-grammar-tests.el" "dorg-install-tests.el"
    "dorg-model-tests.el" "dorg-mutate-linearize-tests.el" "dorg-project-tests.el"
    "dorg-projection-tests.el" "dorg-sql-gen-tests.el")
  "Census class C1 (root-el) as frozen after the E1 ruling.")

(defconst dorg-dft--c2
  '("dorg-dolt-tests.el" "dorg-model-tests.el" "dorg-project-tests.el"
    "dorg-projection-tests.el" "dorg-sql-gen-tests.el")
  "Census class C2 (tests-el).")

(defun dorg-dft--set (&rest lists)
  "Return the sorted, deduplicated union of LISTS of basenames."
  (sort (delete-dups (apply #'append (mapcar #'copy-sequence lists))) #'string<))

(defconst dorg-dft--alpha-module
  '("dorg-alphamod-tests.el" "dorg-alphamod-extra-tests.el")
  "R2's selection for a change to dorg-alphamod.el.")

;;; ─── AC1: change records, end to end ─────────────────────────────────────

(ert-deftest dorg-test-derive-families-renamed-test-selected-by-new-name ()
  "A renamed test is selected under its new basename, with no module change."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tests/dorg-beta-tests.el" :rename "tests/dorg-delta-tests.el")))
    (let ((selected (dorg-dft--selected dir)))
      (should (member "dorg-delta-tests.el" selected))
      (should-not (member "dorg-beta-tests.el" selected))
      (should (equal (dorg-dft--set '("dorg-delta-tests.el") dorg-dft--c2)
                     selected)))))

(ert-deftest dorg-test-derive-families-deleted-test-never-selected ()
  "A deleted test file is never selected; the tree census still fires."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tests/dorg-gamma-tests.el" . :delete)))
    (should (equal (dorg-dft--set dorg-dft--c2) (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-renamed-module-keeps-old-name ()
  "A root module renamed, or moved to lib/, still selects its old module tests."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("dorg-alphamod.el" :rename "dorg-omegamod.el")))
    (should (equal (dorg-dft--set dorg-dft--alpha-module dorg-dft--c1)
                   (dorg-dft--selected dir))))
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("dorg-alphamod.el" :rename "lib/dorg-alphamod.el")))
    (should (equal (dorg-dft--set dorg-dft--alpha-module dorg-dft--c1
                                  '("dorg-cli-tests.el" "dorg-dolt-tests.el"
                                    "dorg-faithful-tests.el" "dorg-install-tests.el"))
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-typechange-is-a-modification ()
  "A T status (file to symlink) counts as a modification of its path."
  (dorg-dft--with-repo dir
    (delete-file (expand-file-name "tools/thing.sh" dir))
    (make-symbolic-link "../dorg-alphamod.el" (expand-file-name "tools/thing.sh" dir))
    (dorg-dft--git dir "add" "-A")
    (dorg-dft--git dir "commit" "-q" "-m" "typechange")
    (should (equal '("dorg-path-tests.el" "dorg-project-tests.el")
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-paths-with-spaces-and-unicode ()
  "NUL-delimited records keep spaces and non-ASCII path bytes intact."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tests/fixtures/data one/\u00fc.txt" . "changed\n")))
    (should (equal '("dorg-fixture-space-tests.el" "dorg-project-tests.el")
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-copy-status-refuses ()
  "A C (copy) record means copies were configured: the parser refuses."
  (should-error (dorg-derive-families--parse-name-status
                 (concat "C75" "\0" "a.el" "\0" "b.el" "\0")))
  (should (equal '((:status R :old-path "a b.el" :path "c\u00fc.el")
                   (:status M :path "d.el"))
                 (dorg-derive-families--parse-name-status
                  (concat "R100" "\0" "a b.el" "\0" "c\u00fc.el" "\0"
                          "M" "\0" "d.el" "\0")))))

(ert-deftest dorg-test-derive-families-index-is-read-at-candidate ()
  "An uncommitted worktree edit is invisible: the index is the candidate's."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tools/thing.sh" . "echo other\n")))
    (dorg-dft--write dir "tests/dorg-gamma-tests.el" "\"tools/thing.sh\"\n")
    (should (equal '("dorg-path-tests.el" "dorg-project-tests.el")
                   (dorg-dft--selected dir)))))

;;; ─── AC2: containing definitions (R3) ────────────────────────────────────

(defun dorg-dft--alpha-change (from to)
  "Return a change replacing FROM by TO in dorg-alphamod.el."
  (list (cons "dorg-alphamod.el" (dorg-dft--edit dorg-dft--alpha from to))))

(ert-deftest dorg-test-derive-families-body-only-edit-finds-its-definition ()
  "A body-only edit deep in a multi-line defun selects references to it.
This is also arc's diff that touches no test file.  dorg-alphamod-run-extra is
not selected: symbol boundaries treat hyphens as constituents."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir (dorg-dft--alpha-change "(+ y 1)" "(+ y 2)"))
    (should (equal (dorg-dft--set dorg-dft--alpha-module dorg-dft--c1
                                  '("dorg-beta-tests.el"))
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-removed-definition-selects-old-references ()
  "Deleting a definition selects the files that referenced it."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir (dorg-dft--alpha-change "(defun dorg-alphamod-run-extra () 2)\n" ""))
    (should (member "dorg-gamma-tests.el" (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-strings-and-comments-are-not-containers ()
  "A form-like line inside a docstring belongs to its real container.
dorg-fake-name exists only in docstring and comment text."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir (dorg-dft--alpha-change "(defun dorg-fake-name ()"
                                                  "(defun dorg-fake-name (arg)"))
    (let ((selected (dorg-dft--selected dir)))
      (should (member "dorg-beta-tests.el" selected))
      (should-not (member "dorg-fake-tests.el" selected)))))

(ert-deftest dorg-test-derive-families-struct-method-and-alias-names ()
  "cl-defstruct with options, cl-defmethod and defalias resolve to their names."
  (dolist (case '(("  a b)" "  a b c)" "dorg-rec-tests.el")
                  ("  (list r))" "  (list r r))" "dorg-show-tests.el")
                  ("#'dorg-alphamod-run)" "#'dorg-alphamod-run-extra)" "dorg-alias-tests.el")))
    (dorg-dft--with-repo dir
      (dorg-dft--change dir (dorg-dft--alpha-change (nth 0 case) (nth 1 case)))
      (should (member (nth 2 case) (dorg-dft--selected dir))))))

(ert-deftest dorg-test-derive-families-comment-only-edit-selects-no-definition ()
  "A top-level comment edit selects module and census files only."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir (dorg-dft--alpha-change ";; top-level comment"
                                                  ";; top-level remark"))
    (should (equal (dorg-dft--set dorg-dft--alpha-module dorg-dft--c1)
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-definition-extraction-is-syntactic ()
  "Containing definitions come from the text, never from hunk headers."
  (should (equal '("dorg-alphamod-run")
                 (dorg-derive-families-definitions dorg-dft--alpha '((8 . 8)))))
  (should (equal '("dorg-alphamod-rec")
                 (dorg-derive-families-definitions dorg-dft--alpha '((13 . 13)))))
  (should (null (dorg-derive-families-definitions dorg-dft--alpha '((20 . 20)))))
  ;; Quoted and backquoted data is never a definition, even when its head is
  ;; a definer.
  (should (equal '("dorg-alphamod-heads")
                 (dorg-derive-families-definitions
                  "(defconst dorg-alphamod-heads\n  '(defun defmacro defvar))\n" '((2 . 2)))))
  (should (equal '("dorg-alphamod-template")
                 (dorg-derive-families-definitions
                  "(defmacro dorg-alphamod-template (name)\n  `(ert-deftest ,name () (should t)))\n"
                  '((2 . 2))))))

;;; ─── AC3: helper, fixture, path and family rules ─────────────────────────

(ert-deftest dorg-test-derive-families-helper-only-change ()
  "A shared helper change selects its requirers and its definitions' callers."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tests/dorg-test-helpers.el"
                             . "(defun dorg-test-help-me ()\n  2)\n(provide 'dorg-test-helpers)\n")))
    (should (equal (dorg-dft--set dorg-dft--c2 '("dorg-uses-helper-tests.el"
                                                 "dorg-calls-helper-tests.el"))
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-fixture-directory-users ()
  "A fixture-directory change selects exactly the tests naming that directory."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tests/fixtures/data-one/input.txt" . "changed\n")))
    (should (equal '("dorg-fixture-user-tests.el" "dorg-project-tests.el")
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-non-el-path-references ()
  "A non-.el path outside tests/ selects the tests naming it verbatim."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("tools/thing.sh" . "echo changed\n")))
    (should (equal '("dorg-path-tests.el" "dorg-project-tests.el")
                   (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-lock-writer-family ()
  "A lock, maintenance or plain-writer module selects infra and persist tests."
  (dolist (module '("dorg-file-lock.el" "dorg-git-maintenance.el" "dorg-mutate.el"))
    (dorg-dft--with-repo dir
      (dorg-dft--change dir (list (cons module ";; module\n")))
      (let ((selected (dorg-dft--selected dir)))
        (should (member "dorg-test-infra-tests.el" selected))
        (should (member "dorg-mutate-persist-tests.el" selected)))))
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("dorg-othermod.el" . ";; module\n")))
    (should-not (member "dorg-mutate-persist-tests.el" (dorg-dft--selected dir)))))

(ert-deftest dorg-test-derive-families-exit-hook-family ()
  "A module mentioning kill-emacs-hook selects infra tests; others do not."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("dorg-zetamod.el" . "(add-hook 'kill-emacs-hook #'ignore)\n")))
    (should (member "dorg-test-infra-tests.el" (dorg-dft--selected dir))))
  (dorg-dft--with-repo dir
    (dorg-dft--change dir '(("dorg-zetamod.el" . "(add-hook 'after-init-hook #'ignore)\n")))
    (should-not (member "dorg-test-infra-tests.el" (dorg-dft--selected dir)))))

;;; ─── AC4: census classes ─────────────────────────────────────────────────

(ert-deftest dorg-test-derive-families-census-classes ()
  "Each census class fires on its trigger and stays silent outside it."
  (dolist (case
           `((("dorg-newmod.el" . "(defvar dorg-newmod-x \"DORG_DIR\")\n") ,dorg-dft--c1)
             (("tests/dorg-helper-two.el" . ";; support\n") ,dorg-dft--c2)
             (("tests/fixtures/data-one/case.org" . "* x\n")
              ("dorg-dolt-tests.el" "dorg-fixture-user-tests.el" "dorg-project-tests.el"))
             (("tools/new.el" . ";; tool\n")
              ("dorg-dolt-tests.el" "dorg-faithful-tests.el" "dorg-model-tests.el"
               "dorg-project-tests.el" "dorg-projection-tests.el" "dorg-sql-gen-tests.el"))
             (("lib/vendor/x.el" . ";; vendored\n")
              ("dorg-cli-tests.el" "dorg-dolt-tests.el" "dorg-faithful-tests.el"
               "dorg-install-tests.el" "dorg-project-tests.el"))
             (("docs/guide.org" . "* guide\n")
              ("dorg-dolt-tests.el" "dorg-project-tests.el"))
             (("Makefile" . "all:\n")
              ("dorg-dolt-tests.el" "dorg-install-tests.el" "dorg-project-tests.el"))
             (("bin/dorg" . "#!/bin/sh\n")
              ("dorg-dolt-tests.el" "dorg-install-tests.el" "dorg-project-tests.el"))
             (("docs/notes.txt" . "notes\n") ("dorg-project-tests.el"))))
    (dorg-dft--with-repo dir
      (dorg-dft--change dir (list (car case)))
      (should (equal (cons (caar case) (dorg-dft--set (cadr case)))
                     (cons (caar case) (dorg-dft--selected dir)))))))

(ert-deftest dorg-test-derive-families-content-only-root-change-selects-install ()
  "After the E1 ruling, any root .el change selects install, app included."
  (dolist (module '("dorg-alphamod.el" "dorg-app.el"))
    (dorg-dft--with-repo dir
      (dorg-dft--change dir (list (cons module ";; changed\n")))
      (should (member "dorg-install-tests.el" (dorg-dft--selected dir))))))

;;; ─── AC5: tree-enumerator guard ──────────────────────────────────────────

(ert-deftest dorg-test-derive-families-every-enumerator-is-declared ()
  "Every repository-enumerating test file is a census member or an exemption."
  (should (null (dorg-derive-families-undeclared-enumerators
                 (expand-file-name "tests" dorg-dft--repo-root)))))

(ert-deftest dorg-test-derive-families-guard-is-sensitive ()
  "A new root-scanning text is flagged; a temp-directory listing is not."
  (should (dorg-derive-families-flagged-enumerators
           "(let ((root (file-name-directory (locate-library \"dorg\"))))\n  (directory-files root t \"\\\\.el\\\\'\"))\n"))
  (should-not (dorg-derive-families-flagged-enumerators
               "(let ((dir (make-temp-file \"x\" t)))\n  (directory-files dir))\n")))

;;; ─── AC6-AC8: runner, refusals and receipts ──────────────────────────────

(defconst dorg-dft--runner
  (concat
   "(require 'ert)\n"
   "(defconst dft-root (expand-file-name \"..\" (file-name-directory load-file-name)))\n"
   "(when (and (getenv \"DFT_DRIFT_AT_LOAD\")\n"
   "           (not (file-exists-p (expand-file-name \".git/dft-drifted\" dft-root))))\n"
   "  (write-region \"\" nil (expand-file-name \".git/dft-drifted\" dft-root))\n"
   "  (let ((default-directory dft-root))\n"
   "    (write-region \";; drift\\n\" nil \"drift.txt\")\n"
   "    (unless (equal (getenv \"DFT_DRIFT_AT_LOAD\") \"untracked\")\n"
   "      (call-process \"git\" nil nil nil \"add\" \"drift.txt\")\n"
   "      (call-process \"git\" nil nil nil \"-c\" \"user.name=d\" \"-c\" \"user.email=d@e.invalid\"\n"
   "                    \"commit\" \"-q\" \"-m\" \"drift\"))))\n"
   "(dolist (f (directory-files (expand-file-name \"tests\" dft-root) t \"-tests\\\\.el\\\\'\"))\n"
   "  (unless (with-temp-buffer (insert-file-contents f) (search-forward \";; unregistered\" nil t))\n"
   "    (load f nil t)))\n"
   "(defun dorg-run-tests--batch (selector _start)\n"
   "  (pcase (getenv \"DFT_RUNNER_MODE\")\n"
   "    (\"teardown-fail\" (kill-emacs 1))\n"
   "    (\"silent-zero\" (kill-emacs 0))\n"
   ;; Both modes report exactly the selected count (four), so only a name
   ;; comparison can tell them from a real run.
   "    (\"substitute\"\n"
   "     (princ \"   passed  1/4  dft-pass (0.0 sec)\\n   passed  2/4  dft-macro-made (0.0 sec)\\n   passed  3/4  dft-aaa-early (0.0 sec)\\n   passed  4/4  dft-not-selected (0.0 sec)\\n\\nRan 4 tests, 4 results as expected, 0 unexpected\\n\")\n"
   "     (kill-emacs 0))\n"
   "    (\"duplicate\"\n"
   "     (princ \"   passed  1/4  dft-pass (0.0 sec)\\n   passed  2/4  dft-pass (0.0 sec)\\n   passed  3/4  dft-macro-made (0.0 sec)\\n   passed  4/4  dft-aaa-early (0.0 sec)\\n\\nRan 4 tests, 4 results as expected, 0 unexpected\\n\")\n"
   "     (kill-emacs 0))\n"
   ;; Same executed names as a real run, but dft-pass is first redefined from a
   ;; support file, so its defining file at execution differs from COUNT's.
   ;; A real run whose output carries one fabricated extra row: the executed
   ;; files and the summary total match, only the printed names differ.
   "    (\"extra-row\"\n"
   "     (let ((stats (ert-run-tests-batch selector)))\n"
   "       (princ \"   passed  4/3  dft-phantom (0.0 sec)\\n\")\n"
   "       (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))\n"
   "    (\"reattribute\"\n"
   "     (load (expand-file-name \"tests/dorg-zzz-shadow.el\" dft-root) nil t)\n"
   "     (let ((stats (ert-run-tests-batch '(member dft-pass dft-fail dft-macro-made dft-aaa-early))))\n"
   "       (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))\n"
   "    (mode\n"
   "     (when (equal mode \"drift-during\")\n"
   "       (let ((default-directory dft-root))\n"
   "         (write-region \";; moved\\n\" nil \"dorg-alphamod.el\" t)))\n"
   "     (let ((stats (ert-run-tests-batch selector)))\n"
   "       (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))))\n"
   "(provide 'dorg-tests-runner)\n")
  "A minimal stand-in for tests/dorg-tests-runner.el with failure modes.")

(defun dorg-dft--runner-repo (&optional extra)
  "Return a synthetic repository for runner specs, plus EXTRA files.
It has a minimal runner, a selected passing and failing test, an unselected
sentinel writing $DFT_SENTINEL, and a macro-generated test."
  (dorg-dft--repo
   (append
    `(("dorg-alphamod.el" . "(defun dorg-alphamod-run () 1)\n")
      ("tests/dorg-tests-runner.el" . ,dorg-dft--runner)
      ;; The helper's function body holds an ert-deftest, expanded when the
      ;; helper loads; calling it from a test file defines the test there.
      ;; Its defining file (symbol-file) is the calling test file, while
      ;; ert-test-file-name names the helper.
      ("tests/dorg-mac-helper.el"
       . "(defun dft-define-macro-made ()\n  (ert-deftest dft-macro-made () (should t)))\n(provide 'dorg-mac-helper)\n")
      ;; Two tests in the last selected file, one sorting before every other
      ;; selected name: the global comparison must not disturb per-file data.
      ("tests/dorg-alphamod-tests.el"
       . "(ert-deftest dft-pass () (should (= 1 1)))\n(ert-deftest dft-aaa-early () (should t))\n")
      ("tests/dorg-alphamod-fail-tests.el"
       . "(ert-deftest dft-fail () (should (getenv \"DFT_PASS_FAILING\")))\n")
      ("tests/dorg-alphamod-mac-tests.el"
       . "(require 'dorg-mac-helper (expand-file-name \"dorg-mac-helper\" (file-name-directory load-file-name)))\n(dft-define-macro-made)\n")
      ("tests/dorg-zzz-sentinel-tests.el"
       . "(ert-deftest dft-sentinel () (write-region \"ran\" nil (getenv \"DFT_SENTINEL\")))\n")
      ("tests/dorg-zzz-shadow.el"
       . "(ert-delete-test 'dft-pass)\n(ert-deftest dft-pass () (should t))\n"))
    extra)))

(defun dorg-dft--entry (dir env &optional tool)
  "Run the tool's batch entry in DIR with ENV; return (EXIT . OUTPUT).
TOOL defaults to the source under test.  The child is killed after 120 s."
  (let* ((default-directory (file-name-as-directory dir))
         (process-environment (append env dorg-dft--git-environment
                                      process-environment))
         (buffer (generate-new-buffer " *dft-entry*"))
         (proc (make-process
                :name "dft-entry" :buffer buffer :noquery t
                :command (list (expand-file-name invocation-name invocation-directory)
                               "-Q" "--batch" "-l" (or tool dorg-dft--tool)
                               "-f" "dorg-derive-families-run-batch")))
         (deadline (+ (float-time) 120)))
    (while (and (process-live-p proc) (< (float-time) deadline))
      (accept-process-output proc 0.1))
    (prog1 (cons (if (process-live-p proc)
                     (progn (delete-process proc) 'timeout)
                   (process-exit-status proc))
                 (with-current-buffer buffer (buffer-string)))
      (kill-buffer buffer))))

(defun dorg-dft--receipt (out name)
  "Return the contents of receipt file NAME in OUT, or nil."
  (let ((file (expand-file-name name out)))
    (and (file-exists-p file)
         (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defmacro dorg-dft--with-run (bindings &rest body)
  "Run the entry for a runner repository and bind its outcome.
BINDINGS is (DIR OUT EXIT SENTINEL CHANGES ENV); CHANGES are committed after
the base, and ENV extends the entry's environment."
  (declare (indent 1))
  (pcase-let ((`(,dir ,out ,exit ,sentinel ,changes ,env) bindings))
    `(let* ((,dir (dorg-dft--runner-repo))
            (,out (make-temp-file "dft-out-" t))
            (,sentinel (make-temp-name (expand-file-name "dft-sentinel-" temporary-file-directory))))
       (unwind-protect
           (progn
             (when ,changes (dorg-dft--change ,dir ,changes))
             ;; ENV comes first: `getenv' takes the first binding.
             (let ((,exit (car (dorg-dft--entry
                                ,dir (append ,env
                                             (list "DORG_DERIVED_BASE=base"
                                                   (concat "DORG_DERIVED_OUT=" ,out)
                                                   (concat "DFT_SENTINEL=" ,sentinel)))))))
               ,@body))
         (delete-directory ,dir t)
         (delete-directory ,out t)
         (when (file-exists-p ,sentinel) (delete-file ,sentinel))))))

(defconst dorg-dft--alpha-touch
  '(("dorg-alphamod.el" . "(defun dorg-alphamod-run () 2)\n"))
  "A root change selecting the three dorg-alphamod test files by R2.")

(ert-deftest dorg-test-derive-families-runs-selected-files-only ()
  "Selected passing tests run, the unselected sentinel never runs, exit 0.
The receipt holds identity, derivation, counts, selector, censuses and result,
and macro-generated tests are counted under their defining file."
  (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch '("DFT_PASS_FAILING=1"))
    (should (eql 0 exit))
    (should-not (file-exists-p sentinel))
    (dolist (name '("identity.txt" "derived.eld" "counts.eld" "selector.el"
                    "census-before.txt" "census-after.txt" "run.log" "result.txt"))
      (should (dorg-dft--receipt out name)))
    (should (equal (car (read-from-string (dorg-dft--receipt out "counts.eld")))
                   (car (read-from-string (dorg-dft--receipt out "executed.eld")))))
    (let ((counts (car (read-from-string (dorg-dft--receipt out "counts.eld")))))
      (should (equal '(("dorg-alphamod-fail-tests.el" "dft-fail")
                       ("dorg-alphamod-mac-tests.el" "dft-macro-made")
                       ("dorg-alphamod-tests.el" "dft-aaa-early" "dft-pass"))
                     counts)))
    (dolist (census '("census-before.txt" "census-after.txt"))
      ;; The entry's own Emacs is always present, so an empty census means
      ;; the filter lost every row.
      (should (split-string (dorg-dft--receipt out census) "\n" t))
      (dolist (row (split-string (dorg-dft--receipt out census) "\n" t))
        (should (string-match-p "\\`[ \t]*[0-9]+[ \t]+[0-9]+[ \t]+[^ \t/]+[ \t]*\\'" row))))))

(ert-deftest dorg-test-derive-families-selected-failure-propagates ()
  "A selected failing test makes the exit nonzero; receipts survive."
  (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch nil)
    (should (eql 1 exit))
    (should (dorg-dft--receipt out "result.txt"))
    (should (dorg-dft--receipt out "census-after.txt"))
    (should-not (file-exists-p sentinel))))

(ert-deftest dorg-test-derive-families-runner-failures-never-succeed ()
  "A teardown failure stays nonzero; silence, substitution or duplication is 6.
A run executing exactly the counted names, but with one test defined by a
different file at execution, is 6 too: execution attribution is per file."
  (dolist (case '(("teardown-fail" . 1) ("silent-zero" . 6)
                  ("substitute" . 6) ("duplicate" . 6) ("extra-row" . 6)
                  ("reattribute" . 6)))
    (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch
                             (list "DFT_PASS_FAILING=1"
                                   (concat "DFT_RUNNER_MODE=" (car case))))
      (should (equal case (cons (car case) exit)))
      (should (dorg-dft--receipt out "result.txt")))))

(ert-deftest dorg-test-derive-families-identity-drift-refuses ()
  "A checkout that moves between COUNT and RUN, or during RUN, exits 4.
Between COUNT and RUN the drift is a new commit, or only a new untracked
file, which just the before-run recheck can see; during RUN it is an edit
of a tracked file, which the after-run recheck sees."
  (dolist (env '(("DFT_DRIFT_AT_LOAD=commit") ("DFT_DRIFT_AT_LOAD=untracked")
                 ("DFT_RUNNER_MODE=drift-during")))
    (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch
                             (cons "DFT_PASS_FAILING=1" env))
      (should (equal (cons env 4) (cons env exit)))
      (should (string-match-p "identity" (dorg-dft--receipt out "result.txt"))))))

(ert-deftest dorg-test-derive-families-dry-run-runs-nothing ()
  "DRY=1 writes identity, derivation, counts and selector, and runs no test."
  (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch '("DORG_DERIVED_DRY=1"))
    (should (eql 0 exit))
    (should (dorg-dft--receipt out "selector.el"))
    (should-not (dorg-dft--receipt out "run.log"))
    (should-not (file-exists-p sentinel))
    (let ((selector (car (read-from-string (dorg-dft--receipt out "selector.el")))))
      (should (eq 'satisfies (car selector))))))

(ert-deftest dorg-test-derive-families-refusals ()
  "Each refusal exits with its code before any test runs."
  (dolist (case `((("DORG_DERIVED_BASE=") nil 4)
                  (("DORG_DERIVED_BASE=no-such-ref") nil 4)
                  (nil (("tools/none.txt" . "x\n")) 2)
                  (nil (("tests/dorg-alphamod-tests.el" . ";; unregistered\n(ert-deftest dft-unreg () t)\n")) 3)))
    (dorg-dft--with-run (dir out exit sentinel (nth 1 case) (nth 0 case))
      (should (equal case (list (nth 0 case) (nth 1 case) exit)))
      (should-not (dorg-dft--receipt out "run.log"))
      (should-not (file-exists-p sentinel))))
  ;; A dirty tree, and a base with no merge-base, refuse with 4.
  (dorg-dft--with-run (dir out exit sentinel nil '("DORG_DERIVED_BASE=HEAD"))
    (ignore exit)
    (dorg-dft--write dir "untracked.el" ";; dirty\n")
    (should (eql 4 (car (dorg-dft--entry dir (list "DORG_DERIVED_BASE=base"
                                                   (concat "DORG_DERIVED_OUT=" out)))))))
  (dorg-dft--with-run (dir out exit sentinel nil '("DORG_DERIVED_BASE=HEAD"))
    (ignore exit)
    (let ((orphan (dorg-dft--git dir "commit-tree" "-m" "orphan"
                                 (dorg-dft--git dir "rev-parse" "HEAD^{tree}"))))
      (should (eql 4 (car (dorg-dft--entry
                           dir (list (concat "DORG_DERIVED_BASE=" orphan)
                                     (concat "DORG_DERIVED_OUT=" out)))))))))

(ert-deftest dorg-test-derive-families-stale-tool-bytecode-refuses ()
  "A tool .elc not strictly older than its source refuses with 5."
  (let ((copy (make-temp-file "dft-tool-" t)))
    (unwind-protect
        (let ((tool (expand-file-name "dorg-derive-families.el" copy)))
          (copy-file dorg-dft--tool tool)
          (write-region ";; stale\n" nil (concat tool "c"))
          (set-file-times (concat tool "c") (time-add (current-time) 10))
          (dorg-dft--with-run (dir out exit sentinel nil '("DORG_DERIVED_BASE=HEAD"))
            (ignore exit)
            (should (eql 5 (car (dorg-dft--entry
                                 dir (list "DORG_DERIVED_BASE=base"
                                           (concat "DORG_DERIVED_OUT=" out))
                                 tool))))))
      (delete-directory copy t))))

;;; Read and parse failures refuse (exit 5).

(ert-deftest dorg-test-derive-families-required-blob-failure-refuses ()
  "A required blob that cannot be read refuses; it never reads as absent.
The adapter refuses when the base side of a modification is unreadable, and
the batch entry exits 5 with no run when an object is missing."
  (dorg-dft--with-repo dir
    (dorg-dft--change dir (dorg-dft--alpha-change "(+ y 1)" "(+ y 2)"))
    (let ((raw (symbol-function 'dorg-derive-families--git-raw)))
      (cl-letf (((symbol-function 'dorg-derive-families--git-raw)
                 (lambda (repo &rest args)
                   (if (and (equal (car args) "cat-file")
                            (string-prefix-p "base:" (car (last args))))
                       (cons 128 "")
                     (apply raw repo args)))))
        (should (equal 5 (condition-case err
                             (progn (dorg-derive-families-records dir "base" "HEAD") nil)
                           (dorg-derive-families-refusal (nth 1 err))))))))
  (dorg-dft--with-run (dir out exit sentinel dorg-dft--alpha-touch '("DORG_DERIVED_BASE=HEAD"))
    (ignore exit)
    (let* ((blob (dorg-dft--git dir "rev-parse" "base:dorg-alphamod.el"))
           (object (expand-file-name (concat ".git/objects/" (substring blob 0 2) "/"
                                             (substring blob 2))
                                     dir)))
      (set-file-modes object #o644)
      (delete-file object)
      (should (eql 5 (car (dorg-dft--entry dir (list "DORG_DERIVED_BASE=base"
                                                     (concat "DORG_DERIVED_OUT=" out))))))
      (should-not (dorg-dft--receipt out "run.log"))
      (should (string-match-p "refused" (dorg-dft--receipt out "result.txt"))))))

(ert-deftest dorg-test-derive-families-malformed-changed-lisp-refuses ()
  "Changed Lisp that does not read refuses, in the core and in the batch entry."
  (should (equal 5 (condition-case err
                       (progn (dorg-derive-families-definitions
                               "(defun broken (x)\n  (+ x 1)\n" '((2 . 2)))
                              nil)
                     (dorg-derive-families-refusal (nth 1 err)))))
  (dorg-dft--with-run (dir out exit sentinel
                           '(("dorg-alphamod.el" . "(defun dorg-alphamod-run ()\n  2\n"))
                           nil)
    (should (eql 5 exit))
    (should-not (dorg-dft--receipt out "run.log"))
    (should-not (file-exists-p sentinel))
    (should (string-match-p "malformed" (dorg-dft--receipt out "result.txt")))))

;;; ─── AC9: Makefile integration ───────────────────────────────────────────

(defun dorg-dft--makefile ()
  "Return the repository Makefile text."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "Makefile" dorg-dft--repo-root))
    (buffer-string)))

(ert-deftest dorg-test-derive-families-make-target-is-pinned ()
  "test-derived is phony, guarded, cleans bytecode and runs under TEST_RUN."
  (let ((text (dorg-dft--makefile)))
    (should (string-match-p "^\\.PHONY:[^#]*\\_<test-derived\\_>"
                            (replace-regexp-in-string "\\\\\n" " " text)))
    (should (string-match-p "^DORG_GUARDED_GOALS :=.*\\_<test-derived\\_>" text))
    (should (string-match-p "^test-derived: clean-elc$" text))
    (should (string-match-p "^test-derived: export DORG_DERIVED_BASE = \\$(BASE)$" text))
    (should (string-match-p
             "^\t@\\$(GATE_TEST_ENV) \\$(TEST_RUN) \\$(BATCH_TEST) -l \\./tools/dorg-derive-families\\.el"
             text))
    (should (string-match-p "-f dorg-derive-families-run-batch < /dev/null$" text))))

(ert-deftest dorg-test-derive-families-installed-root-refuses ()
  "A marked installed root refuses test-derived at parse time, bytecode intact."
  (let ((root (make-temp-file "dft-installed-" t)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "Makefile" dorg-dft--repo-root)
                     (expand-file-name "Makefile" root))
          (write-region "" nil (expand-file-name ".installed-root" root))
          (write-region "sentinel" nil (expand-file-name "keep.elc" root))
          (with-temp-buffer
            (let ((default-directory (file-name-as-directory root)))
              (should-not (eql 0 (call-process "make" nil t nil "test-derived"
                                               "BASE=HEAD" "DRY=1")))
              (should (string-match-p "REFUSE guarded target test-derived"
                                      (buffer-string)))))
          (should (file-exists-p (expand-file-name "keep.elc" root))))
      (delete-directory root t))))

(ert-deftest dorg-test-derive-families-make-passes-base-verbatim ()
  "A BASE with quotes and spaces reaches the tool unaltered and refuses with 4."
  (let ((dir (dorg-dft--runner-repo
              `(("tools/dorg-derive-families.el"
                 . ,(with-temp-buffer (insert-file-contents dorg-dft--tool)
                                      (buffer-string))))))
        (out (make-temp-file "dft-out-" t))
        (base "x \"y z' w"))
    (unwind-protect
        (with-temp-buffer
          (let ((default-directory (file-name-as-directory dir)))
            (should (eql 2 (call-process
                            "make" nil t nil "-f"
                            (expand-file-name "Makefile" dorg-dft--repo-root)
                            "test-derived" (concat "BASE=" base)
                            (concat "OUT=" out) "DRY=1" "TEST_RUN=" "GATE_TEST_ENV="
                            (concat "BATCH_TEST="
                                    (expand-file-name invocation-name invocation-directory)
                                    " --batch -Q"))))
            (should (string-search base (dorg-dft--receipt out "identity.txt")))))
      (delete-directory dir t)
      (delete-directory out t))))

;;; ─── AC10: historical acceptance ─────────────────────────────────────────

(defun dorg-dft--have-commits (&rest commits)
  "Non-nil when every one of COMMITS exists in this repository."
  (cl-every (lambda (commit)
              (eql 0 (let ((default-directory dorg-dft--repo-root))
                       (call-process "git" nil nil nil "cat-file" "-e"
                                     (concat commit "^{commit}")))))
            commits))

(defmacro dorg-dft--historical (name doc commits &rest body)
  "Define historical spec NAME with DOC, skipped unless COMMITS exist."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,name () ,doc
     (skip-unless (dorg-dft--have-commits ,@commits))
     ,@body))

(dorg-dft--historical dorg-test-derive-families-history-merge-with-six-failures
    "A superseded landing merge selects every file holding its six remote failures."
  ("0fb1eaed3" "7fe9f98b0")
  (let ((selected (dorg-dft--selected dorg-dft--repo-root "0fb1eaed3" "7fe9f98b0")))
    (dolist (file '("dorg-api-tests.el" "dorg-grammar-tests.el"
                    "dorg-mutate-tests.el" "dorg-mutate-persist-tests.el"))
      (should (member file selected)))))

(dorg-dft--historical dorg-test-derive-families-history-lock-writer-merge
    "The plain-writer serialization merge selects infra and persist tests."
  ("d95c8018e")
  (let ((selected (dorg-dft--selected dorg-dft--repo-root "d95c8018e^1" "d95c8018e")))
    (should (member "dorg-test-infra-tests.el" selected))
    (should (member "dorg-mutate-persist-tests.el" selected))))

(dorg-dft--historical dorg-test-derive-families-history-new-modules-merge
    "The merge that added two root modules selects the install census."
  ("6f10770c4" "ae6e809b7")
  (should (member "dorg-install-tests.el"
                  (dorg-dft--selected dorg-dft--repo-root "6f10770c4" "ae6e809b7"))))

(dorg-dft--historical dorg-test-derive-families-history-lock-file-only-change
    "A change to one CI lock file selects only the tree census and its readers."
  ("e6136b7dd")
  (let ((selected (dorg-dft--selected dorg-dft--repo-root "e6136b7dd^" "e6136b7dd")))
    (should (member "dorg-project-tests.el" selected))
    (should-not (member "dorg-mutate-tests.el" selected))
    (should-not (member "dorg-install-tests.el" selected))
    (should-not (member "dorg-test-infra-tests.el" selected))))

(provide 'dorg-derive-families-tests)
;;; dorg-derive-families-tests.el ends here
