;;; dorg-derive-families.el --- Derived pre-ACCEPT test families -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Which whole test files must a candidate pass before ACCEPT?  This tool
;; derives them from the candidate's changes against its merge-base, then
;; runs them through the real runner and keeps a receipt.
;;
;; Layers:
;; - The git adapter reads only git objects (name-status records, changed
;;   line ranges, blobs, the candidate's tests/ tree), so a historical
;;   candidate derives without a checkout.
;; - The pure core, `dorg-derive-families-select', applies the named rules
;;   R1-R7 to change records and the candidate's test index.  Each selected
;;   file carries the reasons that selected it.
;; - The batch entry, `dorg-derive-families-run-batch' (make test-derived),
;;   checks identity, derives, counts the selected tests in a child that
;;   loads the real runner, and runs them in a second child that owns the
;;   runner's own teardown and exit.  It rechecks identity around the run,
;;   compares the executed names with the counted ones, and writes the
;;   receipt.  Any failure exits nonzero; nothing falls back to a default
;;   or full selection.
;;
;; Exit codes: 0 pass or dry run; 2 empty derivation; 3 a derived file with no
;; loaded test; 4 identity (BASE, merge-base, dirty tree, drift); 5 a git,
;; read or parse failure, or stale tool bytecode; 6 a missing summary or an
;; executed set that differs from the counted one.  A failing test run
;; returns the runner's own nonzero status.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Used only inside the counting child, which has loaded ERT with the runner.
(declare-function ert-select-tests "ert" (selector universe))
(declare-function ert-test-name "ert" (test))
(declare-function ert-run-test "ert" (ert-test))

(defconst dorg-derive-families--source
  (or load-file-name buffer-file-name)
  "This tool's source file, captured at load time.")

(define-error 'dorg-derive-families-refusal "dorg derived families: refused")

(defun dorg-derive-families--refuse (code message &rest args)
  "Signal a refusal with exit CODE and MESSAGE formatted with ARGS."
  (signal 'dorg-derive-families-refusal
          (list code (apply #'format message args))))

;;; ─── Git adapter ─────────────────────────────────────────────────────────

(defun dorg-derive-families--git-raw (repo &rest args)
  "Run git ARGS in REPO; return (STATUS . STDOUT), never via a shell."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory repo))
           (coding-system-for-read 'utf-8-unix)
           (status (apply #'call-process "git" nil (list t nil) nil args)))
      (cons status (buffer-string)))))

(defun dorg-derive-families--git (repo &rest args)
  "Run git ARGS in REPO and return stdout; refuse with 5 on failure."
  (let ((result (apply #'dorg-derive-families--git-raw repo args)))
    (unless (eql 0 (car result))
      (dorg-derive-families--refuse 5 "git %S failed (%S)" args (car result)))
    (cdr result)))

(defun dorg-derive-families--parse-name-status (string)
  "Parse NUL-delimited git --name-status -z output STRING into records.
A rename is (:status R :old-path OLD :path NEW); every other record is
\(:status S :path P) with S one of A, M, D, T.  A copy (C) means copy
detection was configured and refuses, as does any unknown status."
  (let ((fields (split-string string "\0" t))
        records)
    (while fields
      (let* ((token (pop fields))
             (kind (aref token 0)))
        (pcase kind
          (?R (let* ((old (pop fields)) (new (pop fields)))
                (unless (and old new)
                  (dorg-derive-families--refuse 5 "truncated rename record"))
                (push (list :status 'R :old-path old :path new) records)))
          ((or ?A ?M ?D ?T)
           (let ((path (pop fields)))
             (unless path (dorg-derive-families--refuse 5 "truncated record"))
             (push (list :status (intern (string kind)) :path path) records)))
          (?C (dorg-derive-families--refuse 5 "copy status in diff: copies must be disabled"))
          (_ (dorg-derive-families--refuse 5 "unknown diff status %S" token)))))
    (nreverse records)))

(defun dorg-derive-families--blob (repo rev path)
  "Return the text of PATH at REV in REPO, or refuse with 5.
Callers read only sides that must exist (a record's present sides, the
candidate's listed test files), so a failed read is a git failure, never an
absence."
  (let ((result (dorg-derive-families--git-raw
                 repo "cat-file" "blob" (concat rev ":" path))))
    (unless (eql 0 (car result))
      (dorg-derive-families--refuse 5 "cannot read %s:%s (git status %S)"
                                    rev path (car result)))
    (cdr result)))

(defun dorg-derive-families--check-lisp (text label)
  "Refuse with 5 unless TEXT reads as complete Lisp; LABEL names it.
Every top-level form is read, never evaluated.  A premature end of input, an
unbalanced closer or invalid read syntax is malformed source."
  (when text
    (with-temp-buffer
      (insert text)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (condition-case err
          (while (progn (forward-comment (buffer-size)) (not (eobp)))
            (read (current-buffer)))
        (error (dorg-derive-families--refuse
                5 "malformed Lisp in %s: %s" label (error-message-string err)))))))

(defun dorg-derive-families--range (start count)
  "Return the line range for a hunk side starting at START with COUNT lines.
A zero-length side is the insertion line, at least line 1."
  (if (zerop count)
      (cons (max 1 start) (max 1 start))
    (cons start (+ start count -1))))

(defun dorg-derive-families--changed-lines (repo base cand paths)
  "Return (OLD-RANGES . NEW-RANGES) changed between BASE and CAND for PATHS."
  (let ((diff (apply #'dorg-derive-families--git repo "diff" "-U0" "-M"
                     "--no-ext-diff" "--no-textconv" "--no-color" base cand "--"
                     paths))
        old new (start 0))
    (while (string-match
            "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
            diff start)
      (let ((os (string-to-number (match-string 1 diff)))
            (oc (if (match-string 2 diff) (string-to-number (match-string 2 diff)) 1))
            (ns (string-to-number (match-string 3 diff)))
            (nc (if (match-string 4 diff) (string-to-number (match-string 4 diff)) 1)))
        (push (dorg-derive-families--range os oc) old)
        (push (dorg-derive-families--range ns nc) new)
        (setq start (match-end 0))))
    (cons (nreverse old) (nreverse new))))

(defun dorg-derive-families--el-p (path)
  "Non-nil when PATH names an Emacs Lisp source file."
  (and path (string-suffix-p ".el" path)))

(defun dorg-derive-families-records (repo base cand)
  "Return the change records of REPO between BASE and CAND.
Each record is a `dorg-derive-families--parse-name-status' plist; an .el
record also carries :old-text, :new-text, :old-lines and :new-lines."
  (mapcar
   (lambda (record)
     (let* ((path (plist-get record :path))
            (old-path (or (plist-get record :old-path) path)))
       (if (not (or (dorg-derive-families--el-p path)
                    (dorg-derive-families--el-p old-path)))
           record
         (let* ((lines (dorg-derive-families--changed-lines
                        repo base cand (delete-dups (list old-path path))))
                (old-text (and (not (eq 'A (plist-get record :status)))
                               (dorg-derive-families--blob repo base old-path)))
                (new-text (and (not (eq 'D (plist-get record :status)))
                               (dorg-derive-families--blob repo cand path))))
           (dorg-derive-families--check-lisp old-text (concat base ":" old-path))
           (dorg-derive-families--check-lisp new-text (concat cand ":" path))
           (append record
                   (list :old-text old-text
                         :new-text new-text
                         :old-lines (car lines)
                         :new-lines (cdr lines)))))))
   (dorg-derive-families--parse-name-status
    (dorg-derive-families--git repo "diff" "-z" "--name-status" "-M"
                               "--no-ext-diff" "--no-textconv" base cand))))

(defun dorg-derive-families-index (repo cand)
  "Return a hash of every tests/**/*.el path at CAND in REPO to its text."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (path (split-string (dorg-derive-families--git
                                 repo "ls-tree" "-r" "-z" "--name-only" cand "--" "tests")
                                "\0" t))
      (when (dorg-derive-families--el-p path)
        (puthash path (dorg-derive-families--blob repo cand path) index)))
    index))

;;; ─── Containing definitions ──────────────────────────────────────────────

(defconst dorg-derive-families--definers
  '(defun defmacro defvar defvar-local defconst defcustom defsubst defalias
     define-error define-derived-mode define-minor-mode cl-defun cl-defmacro
     cl-defsubst cl-defstruct cl-defgeneric cl-defmethod ert-deftest)
  "Heads of forms that define a referenceable name.")

(defun dorg-derive-families--quoted-p (paren)
  "Non-nil when the form at PAREN is, or sits inside, quoted data.
A quote, backquote or function quote before the form or before any form
enclosing it marks data, so a definer list or a macro template defines
nothing."
  (cl-some (lambda (open)
             (memq (char-before open) '(?' ?` ?#)))
           (cons paren (save-excursion (nth 9 (syntax-ppss paren))))))

(defun dorg-derive-families--read-next ()
  "Read the next form at point, or return nil when the list ends there.
Any other read failure refuses with 5: the text was validated as complete."
  (forward-comment (buffer-size))
  (unless (eq (char-after) ?\))
    (condition-case err
        (read (current-buffer))
      (error (dorg-derive-families--refuse
              5 "malformed Lisp at %d: %s" (point) (error-message-string err))))))

(defun dorg-derive-families--definition-at (paren)
  "Return the name defined by the form opening at PAREN, or nil.
Quoted data never defines a name."
  (save-excursion
    (goto-char (1+ paren))
    (let ((head (and (not (dorg-derive-families--quoted-p paren))
                     (dorg-derive-families--read-next))))
      (when (memq head dorg-derive-families--definers)
        (let ((name (dorg-derive-families--read-next)))
          (when (and (eq head 'defalias) (eq 'quote (car-safe name)))
            (setq name (cadr name)))
          (when (and (eq head 'cl-defstruct) (consp name))
            (setq name (car name)))
          (and name (symbolp name) (symbol-name name)))))))

(defun dorg-derive-families-definitions (text ranges)
  "Return the sorted names of definitions in TEXT containing line RANGES.
Containment is syntactic, under the Emacs Lisp syntax table: the forms open
at the start and end of each changed line, plus every form opening on it.
Parentheses inside strings and comments never count.  RANGES are
\(START . END) line pairs.  TEXT that does not read as complete Lisp refuses
with 5."
  (dorg-derive-families--check-lisp text "changed source")
  (let (names)
    (with-temp-buffer
      (insert text)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (let ((last (count-lines (point-min) (point-max))))
        (dolist (range ranges)
          (cl-loop for line from (car range) to (min (cdr range) (max 1 last))
                   do (goto-char (point-min))
                   (forward-line (1- line))
                   (let ((bol (line-beginning-position))
                         (eol (line-end-position))
                         (parens nil))
                     (setq parens (append (save-excursion (nth 9 (syntax-ppss bol)))
                                          (save-excursion (nth 9 (syntax-ppss eol)))))
                     ;; `syntax-ppss' may move point, so each step resumes
                     ;; from the recorded match end.
                     (let ((from bol))
                       (while (progn (goto-char from)
                                     (re-search-forward "(" eol t))
                         (let* ((paren (match-beginning 0))
                                (state (save-excursion (syntax-ppss paren))))
                           (setq from (1+ paren))
                           (unless (or (nth 3 state) (nth 4 state)
                                       (eq ?\\ (char-before paren)))
                             (push paren parens)))))
                     (dolist (paren (delete-dups parens))
                       (let ((name (dorg-derive-families--definition-at paren)))
                         (when name (push name names)))))))))
    (sort (delete-dups names) #'string<)))

;;; ─── Rules ───────────────────────────────────────────────────────────────

(defun dorg-derive-families--root-el-p (path)
  "Non-nil when PATH is an .el file at the repository root."
  (and (dorg-derive-families--el-p path) (not (string-search "/" path))))

(defun dorg-derive-families--top-test-p (path)
  "Non-nil when PATH is a top-level tests/*-tests.el file."
  (and path (string-match-p "\\`tests/[^/]+-tests\\.el\\'" path)))

(defun dorg-derive-families--support-p (path)
  "Non-nil when PATH is a top-level tests/*.el support file."
  (and path (string-match-p "\\`tests/[^/]+\\.el\\'" path)
       (not (dorg-derive-families--top-test-p path))))

(defun dorg-derive-families--paths (record)
  "Return RECORD's new and old paths, without duplicates."
  (delete-dups (delq nil (list (plist-get record :path)
                               (plist-get record :old-path)))))

(defun dorg-derive-families--tests (index)
  "Return INDEX's top-level test paths, sorted."
  (let (tests)
    (maphash (lambda (path _text)
               (when (dorg-derive-families--top-test-p path) (push path tests)))
             index)
    (sort tests #'string<)))

(defun dorg-derive-families--symbols (text)
  "Return a hash set of every symbol token in TEXT (Emacs Lisp syntax)."
  (let ((set (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert text)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\_>" nil t)
        (puthash (match-string-no-properties 0) t set)))
    set))

(defun dorg-derive-families--pairs (tests predicate reason)
  "Return (TEST . REASON) for each path in TESTS satisfying PREDICATE."
  (cl-loop for test in tests
           when (funcall predicate test)
           collect (cons test reason)))

(defun dorg-derive-families--r1-changed-tests (records tests)
  "R1: added, modified or type-changed tests, and renamed tests' new paths."
  (cl-loop for record in records
           for status = (plist-get record :status)
           for path = (plist-get record :path)
           when (and (memq status '(A M T R)) (member path tests))
           collect (cons path (if (eq status 'R)
                                  (list 'renamed-test (plist-get record :old-path) path)
                                (list 'changed-test path)))))

(defun dorg-derive-families--r2-modules (records tests)
  "R2: tests named after a changed root module, by old or new name."
  (cl-loop for record in records
           append
           (cl-loop for path in (dorg-derive-families--paths record)
                    when (dorg-derive-families--root-el-p path)
                    append
                    (let* ((module (file-name-sans-extension path))
                           (re (concat "\\`tests/" (regexp-quote module)
                                       "\\(?:-[^/]+\\)?-tests\\.el\\'")))
                      (dorg-derive-families--pairs
                       tests (lambda (test) (string-match-p re test))
                       (list 'module path))))))

(defun dorg-derive-families--definition-source-p (path)
  "Non-nil when definitions in PATH can be referenced by tests."
  (and (dorg-derive-families--el-p path)
       (or (dorg-derive-families--root-el-p path)
           (string-prefix-p "tools/" path)
           (string-prefix-p "lib/" path)
           (dorg-derive-families--support-p path)
           (string-prefix-p "tests/fixtures/" path))))

(defun dorg-derive-families--changed-definitions (record)
  "Return the names of definitions RECORD changes, in its old and new text."
  (delete-dups
   (append
    (and (plist-get record :old-text)
         (dorg-derive-families-definitions (plist-get record :old-text)
                                           (plist-get record :old-lines)))
    (and (plist-get record :new-text)
         (dorg-derive-families-definitions (plist-get record :new-text)
                                           (plist-get record :new-lines))))))

(defun dorg-derive-families--r3-definitions (records tests symbols)
  "R3: tests referencing a definition a record changes.
SYMBOLS maps each test path to its symbol set."
  (cl-loop for record in records
           for path = (plist-get record :path)
           when (cl-some #'dorg-derive-families--definition-source-p
                         (dorg-derive-families--paths record))
           append
           (cl-loop for name in (dorg-derive-families--changed-definitions record)
                    append (dorg-derive-families--pairs
                            tests (lambda (test) (gethash name (gethash test symbols)))
                            (list 'definition name path)))))

(defun dorg-derive-families--features (text)
  "Return the features TEXT provides."
  (let (features (start 0))
    (while (and text (string-match "(provide '\\([^ \t\n()]+\\)" text start))
      (push (match-string 1 text) features)
      (setq start (match-end 0)))
    features))

(defun dorg-derive-families--r4-helpers (records tests index)
  "R4: tests requiring or loading a changed support file, or using a fixture dir."
  (cl-loop
   for record in records
   append
   (cl-loop
    for path in (dorg-derive-families--paths record)
    append
    (cond
     ((dorg-derive-families--support-p path)
      (let ((features (delete-dups
                       (append (dorg-derive-families--features (plist-get record :old-text))
                               (dorg-derive-families--features (plist-get record :new-text)))))
            (base (file-name-sans-extension (file-name-nondirectory path))))
        (append
         (cl-loop for feature in features
                  append (dorg-derive-families--pairs
                          tests (lambda (test)
                                  (string-search (concat "(require '" feature)
                                                 (gethash test index)))
                          (list 'feature feature path)))
         (dorg-derive-families--pairs
          tests (lambda (test) (string-search (concat "\"" base) (gethash test index)))
          (list 'feature base path)))))
     ((string-match "\\`tests/fixtures/\\([^/]+\\)/" path)
      (let ((dir (match-string 1 path)))
        (dorg-derive-families--pairs
         tests (lambda (test) (string-search (concat "fixtures/" dir) (gethash test index)))
         (list 'fixture-dir dir))))))))

(defun dorg-derive-families--r5-paths (records tests index)
  "R5: tests naming a changed non-test path that is not a root .el verbatim."
  (cl-loop for record in records
           append
           (cl-loop for path in (dorg-derive-families--paths record)
                    unless (or (string-prefix-p "tests/" path)
                               (dorg-derive-families--root-el-p path))
                    append (dorg-derive-families--pairs
                            tests (lambda (test) (string-search path (gethash test index)))
                            (list 'path path)))))

;;; Census (R6): repository enumerators, by trigger class.

(defun dorg-derive-families--any-path (records predicate)
  "Non-nil when some path of some record in RECORDS satisfies PREDICATE."
  (cl-some (lambda (record) (cl-some predicate (dorg-derive-families--paths record)))
           records))

(defun dorg-derive-families--matching (regexp)
  "Return a path predicate matching REGEXP."
  (lambda (path) (string-match-p regexp path)))

(defconst dorg-derive-families-census
  `((root-el
     ,(lambda (records) (dorg-derive-families--any-path
                         records #'dorg-derive-families--root-el-p))
     ("dorg-backend-tests.el" "dorg-cli-tests.el" "dorg-dolt-tests.el"
      "dorg-faithful-tests.el" "dorg-grammar-tests.el" "dorg-install-tests.el"
      "dorg-model-tests.el" "dorg-mutate-linearize-tests.el" "dorg-project-tests.el"
      "dorg-projection-tests.el" "dorg-sql-gen-tests.el")
     "root source scans: confinement, vetted readers, relocation contracts, DDL and commit allowlists, legacy surface, bundle and install compile, literals, reload and inventory maps")
    (tests-el
     ,(lambda (records) (dorg-derive-families--any-path
                         records (dorg-derive-families--matching "\\`tests/[^/]+\\.el\\'")))
     ("dorg-dolt-tests.el" "dorg-model-tests.el" "dorg-project-tests.el"
      "dorg-projection-tests.el" "dorg-sql-gen-tests.el")
     "tests/*.el scans: relocation contracts, test naming, runner and test maps")
    (tests-tree
     ,(lambda (records) (dorg-derive-families--any-path
                         records (dorg-derive-families--matching
                                  "\\`tests/.*\\.\\(?:el\\|org\\|sh\\)\\'")))
     ("dorg-dolt-tests.el")
     "recursive retired-token scan of tests/")
    (tools-el
     ,(lambda (records) (dorg-derive-families--any-path
                         records (dorg-derive-families--matching "\\`tools/.*\\.el\\'")))
     ("dorg-dolt-tests.el" "dorg-faithful-tests.el" "dorg-model-tests.el"
      "dorg-project-tests.el" "dorg-projection-tests.el" "dorg-sql-gen-tests.el")
     "tools/**/*.el scans: relocation contracts, production legacy surface, scrub tokens")
    (lib
     ,(lambda (records) (dorg-derive-families--any-path
                         records (dorg-derive-families--matching "\\`lib/")))
     ("dorg-cli-tests.el" "dorg-dolt-tests.el" "dorg-faithful-tests.el"
      "dorg-install-tests.el")
     "lib/ scans: vendored subsets, retired token, bundle compile, installed copy")
    (docs
     ,(lambda (records) (dorg-derive-families--any-path
                         records (dorg-derive-families--matching
                                  "\\`docs/.*\\.\\(?:el\\|org\\|sh\\)\\'")))
     ("dorg-dolt-tests.el")
     "docs/ retired-token scan")
    (build
     ,(lambda (records)
        (dorg-derive-families--any-path
         records (lambda (path)
                   (or (member path '("Makefile" "bin/dorg" "LICENSE" "install.sh"))
                       (string-match-p "\\`[^/]+\\.\\(?:org\\|sh\\)\\'" path)))))
     ("dorg-dolt-tests.el" "dorg-install-tests.el" "dorg-project-tests.el")
     "build surface: retired token, install runtime, version literal, install scrub, maps")
    (shipped-set
     ,(lambda (records)
        (cl-some (lambda (record)
                   (and (memq (plist-get record :status) '(A D R))
                        (cl-some (lambda (path)
                                   (or (dorg-derive-families--root-el-p path)
                                       (string-prefix-p "bin/" path)
                                       (string-prefix-p "lib/" path)))
                                 (dorg-derive-families--paths record))))
                 records))
     ("dorg-install-tests.el" "dorg-project-tests.el")
     "shipped file set: runtime inventory equality and relocated install set")
    (any
     ,(lambda (records) (and records t))
     ("dorg-project-tests.el")
     "public-export scrub over every tracked byte, manifest, ignore rules"))
  "Census classes: (NAME TRIGGER TEST-FILES REASON).
Each tree-enumerating test file runs whole when its class fires.")

(defconst dorg-derive-families-exemptions
  '(("dorg-test-infra-tests.el"
     . "source-binding child wraps the load and asserts only the guard's flag, root and dirs; selected by behaviour")
    ("dorg-derive-families-tests.el"
     . "enumeration text lives in synthetic fixture strings; it scans only repositories it creates")
    ("dorg-sexp-file-tests.el" . "lists only its temp directory; reads one fixed source")
    ("dorg-dolt-server-tests.el" . "lists owned temp roots; the repo root is used only for -L")
    ("dorg-dolt-server-evidence-tests.el" . "lists an owned temp root")
    ("dorg-test-scratch-tests.el" . "lists only its temp scratch base")
    ("dorg-test-job-tests.el" . "lists only its temp directory; bundle output is not asserted")
    ("dorg-mutate-persist-tests.el" . "lists only its spec temp directory; selected by behaviour"))
  "Enumerating-looking test files that need no census class, with the reason.
Untracked build output is not a committed change; every test target runs
clean-elc first and a dirty tree refuses.")

(defconst dorg-derive-families-behaviour
  '((lock-writer
     ("dorg-file-lock.el" "dorg-git-maintenance.el" "dorg-mutate.el")
     ("dorg-test-infra-tests.el" "dorg-mutate-persist-tests.el")
     "kill-time cleanup hooks and the refuse-before-fs-access writer order are asserted without naming the changed symbols"))
  "Behaviour families: (NAME TRIGGER-MODULES TEST-FILES REASON).")

(defun dorg-derive-families--r6-census (records tests)
  "R6: census classes whose trigger fires on RECORDS."
  (cl-loop for (name trigger files _reason) in dorg-derive-families-census
           when (funcall trigger records)
           append (dorg-derive-families--pairs
                   tests (lambda (test) (member (file-name-nondirectory test) files))
                   (list 'census name))))

(defun dorg-derive-families--mentions-symbol-p (text symbol)
  "Non-nil when TEXT contains SYMBOL as a whole symbol."
  (and text (gethash symbol (dorg-derive-families--symbols text))))

(defun dorg-derive-families--r7-families (records tests)
  "R7: behaviour families, plus root modules touching `kill-emacs-hook'."
  (append
   (cl-loop for (name modules files _reason) in dorg-derive-families-behaviour
            when (dorg-derive-families--any-path
                  records (lambda (path) (member path modules)))
            append (dorg-derive-families--pairs
                    tests (lambda (test) (member (file-name-nondirectory test) files))
                    (list 'family name)))
   (when (cl-some (lambda (record)
                    (and (cl-some #'dorg-derive-families--root-el-p
                                  (dorg-derive-families--paths record))
                         (or (dorg-derive-families--mentions-symbol-p
                              (plist-get record :old-text) "kill-emacs-hook")
                             (dorg-derive-families--mentions-symbol-p
                              (plist-get record :new-text) "kill-emacs-hook"))))
                  records)
     (dorg-derive-families--pairs
      tests (lambda (test) (equal "tests/dorg-test-infra-tests.el" test))
      (list 'family 'exit-hooks)))))

(defun dorg-derive-families-select (records index)
  "Return ((TEST-BASENAME . REASONS) ...) selected by RECORDS against INDEX.
INDEX maps the candidate's tests/**/*.el paths to their text.  Only top-level
tests/*-tests.el files present at the candidate are selected; REASONS are the
sorted, deduplicated tagged reasons."
  (let* ((tests (dorg-derive-families--tests index))
         (symbols (make-hash-table :test 'equal))
         (table (make-hash-table :test 'equal)))
    (dolist (test tests)
      (puthash test (dorg-derive-families--symbols (gethash test index)) symbols))
    (dolist (pair (append (dorg-derive-families--r1-changed-tests records tests)
                          (dorg-derive-families--r2-modules records tests)
                          (dorg-derive-families--r3-definitions records tests symbols)
                          (dorg-derive-families--r4-helpers records tests index)
                          (dorg-derive-families--r5-paths records tests index)
                          (dorg-derive-families--r6-census records tests)
                          (dorg-derive-families--r7-families records tests)))
      (push (cdr pair) (gethash (file-name-nondirectory (car pair)) table)))
    (let (result)
      (maphash (lambda (file reasons)
                 (push (cons file (sort (delete-dups reasons)
                                        (lambda (a b) (string< (prin1-to-string a)
                                                               (prin1-to-string b)))))
                       result))
               table)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

;;; ─── Tree-enumerator guard ───────────────────────────────────────────────

(defconst dorg-derive-families--enumeration-idioms
  '("(directory-files" "(directory-files-recursively" "file-expand-wildcards")
  "Calls that list a directory.")

(defconst dorg-derive-families--root-idioms
  '("locate-library" "find-library-name" "locate-dominating-file" "load-file-name"
    "repo-root" "--root" "project-root" "--repo-dir")
  "Expressions that name the repository root.")

(defconst dorg-derive-families--tree-idioms
  '("\"ls-files\"" "public-export-manifest")
  "Idioms that enumerate the tracked tree by themselves.")

(defun dorg-derive-families-flagged-enumerators (text)
  "Return the enumeration idioms TEXT uses on the repository, or nil.
TEXT enumerates the repository when it uses a tracked-tree idiom, or when it
both lists a directory and names the repository root."
  (let ((tree (cl-remove-if-not (lambda (idiom) (string-search idiom text))
                                dorg-derive-families--tree-idioms))
        (listing (cl-remove-if-not (lambda (idiom) (string-search idiom text))
                                   dorg-derive-families--enumeration-idioms))
        (root (cl-some (lambda (idiom) (string-search idiom text))
                       dorg-derive-families--root-idioms)))
    (append tree (and root listing))))

(defun dorg-derive-families-undeclared-enumerators (tests-dir)
  "Return the tests/*-tests.el files in TESTS-DIR that enumerate undeclared.
A flagged file is declared when a census class or behaviour family names it,
or it is listed in `dorg-derive-families-exemptions'."
  (let ((declared (append (cl-mapcan (lambda (entry) (copy-sequence (nth 2 entry)))
                                     dorg-derive-families-census)
                          (cl-mapcan (lambda (entry) (copy-sequence (nth 2 entry)))
                                     dorg-derive-families-behaviour)
                          (mapcar #'car dorg-derive-families-exemptions))))
    (cl-loop for file in (directory-files tests-dir t "-tests\\.el\\'")
             for name = (file-name-nondirectory file)
             when (and (not (member name declared))
                       (dorg-derive-families-flagged-enumerators
                        (with-temp-buffer (insert-file-contents file) (buffer-string))))
             collect name)))

;;; ─── Batch entry ─────────────────────────────────────────────────────────

(defun dorg-derive-families--write (out name content)
  "Write CONTENT to receipt file NAME under OUT."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil (expand-file-name name out) nil 'silent)))

(defun dorg-derive-families--phase (out phase)
  "Record the start of PHASE in OUT's phase log."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (format "%s %s\n" (format-time-string "%FT%T%z") phase)
                  nil (expand-file-name "phase.txt" out) t 'silent)))

(defun dorg-derive-families--census (out name)
  "Write NAME: the pid, ppid and basename of every dolt and emacs process.
Command lines are never recorded."
  (dorg-derive-families--write
   out name
   (mapconcat
    #'identity
    (cl-loop for line in (process-lines "ps" "-axo" "pid=,ppid=,ucomm=")
             for fields = (split-string line "[ \t]+" t)
             for comm = (string-join (nthcdr 2 fields) "_")
             when (and (>= (length fields) 3)
                       (string-match-p "\\`\\(?:dolt\\|emacs\\)" (downcase comm)))
             collect (format "%s %s %s" (nth 0 fields) (nth 1 fields) comm))
    "\n")))

(defun dorg-derive-families--identity (repo &optional untracked)
  "Return (HEAD TREE CLEAN) for REPO.
CLEAN considers untracked files when UNTRACKED is non-nil."
  (list (string-trim (dorg-derive-families--git repo "rev-parse" "--verify" "HEAD^{commit}"))
        (string-trim (dorg-derive-families--git repo "rev-parse" "--verify" "HEAD^{tree}"))
        (string-empty-p (dorg-derive-families--git
                         repo "status" "--porcelain=v1" "-z"
                         (if untracked "--untracked-files=all" "--untracked-files=no")))))

(defun dorg-derive-families--recheck (repo expected phase untracked)
  "Refuse with 4 unless REPO's HEAD, tree and cleanliness equal EXPECTED.
PHASE names the check; UNTRACKED includes untracked files."
  (let ((now (dorg-derive-families--identity repo untracked)))
    (unless (and (equal (nth 0 now) (nth 0 expected))
                 (equal (nth 1 now) (nth 1 expected))
                 (nth 2 now))
      (dorg-derive-families--refuse 4 "identity drift at %s: %S" phase now))))

(defun dorg-derive-families--emacs-args (runner)
  "Return the child Emacs argv prefix loading RUNNER from the candidate root."
  (append (list (expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "-L" "." "-L" "tests")
          (cl-loop for dir in '("lib/mysql" "lib/clime")
                   when (file-directory-p dir) append (list "-L" dir))
          (list "-l" runner)))

(defun dorg-derive-families--child (args log)
  "Run argv ARGS with stdout and stderr to LOG; return the exit status."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (let ((status (apply #'call-process (car args) nil t nil (cdr args))))
        (write-region (point-min) (point-max) log nil 'silent)
        status))))

(defun dorg-derive-families--count-batch (out files)
  "In a child that loaded the runner, write OUT with each of FILES' tests.
Tests are attributed by their defining file, as the gate tables do."
  (let ((table (mapcar #'list files)))
    (dolist (test (ert-select-tests t t))
      (let* ((name (ert-test-name test))
             (file (symbol-file name 'ert--test))
             (cell (and file (assoc (file-name-nondirectory file) table))))
        (when cell (push (prin1-to-string name) (cdr cell)))))
    (let ((coding-system-for-write 'utf-8-unix)
          (print-length nil) (print-level nil))
      (write-region (prin1-to-string
                     (mapcar (lambda (cell) (cons (car cell) (sort (cdr cell) #'string<)))
                             table))
                    nil out nil 'silent))
    (kill-emacs 0)))

(defun dorg-derive-families--record-executions (file)
  "In the RUN child, append each executed test and its defining file to FILE.
An observation-only advice on `ert-run-test': the runner's selection,
teardown and exit are untouched.  Each line is (DEFINING-FILE . NAME)."
  (advice-add 'ert-run-test :before
              (lambda (test)
                (let* ((name (ert-test-name test))
                       (defined (symbol-file name 'ert--test)))
                  (let ((coding-system-for-write 'utf-8-unix))
                    (write-region (format "%S\n" (cons (and defined (file-name-nondirectory defined))
                                                        (prin1-to-string name)))
                                  nil file t 'silent))))
              '((name . dorg-derive-families-record))))

(defun dorg-derive-families--executed-by-file (log files)
  "Return the executed names per defining file from execution LOG text.
The result lists every FILE in FILES, in order, then any other file seen,
each with its sorted names; it has the shape of counts.eld."
  (let ((table (mapcar #'list files)))
    (dolist (line (split-string (or log "") "\n" t))
      (let* ((entry (car (read-from-string line)))
             (cell (assoc (car entry) table)))
        (unless cell
          (setq cell (list (car entry)))
          (setq table (append table (list cell))))
        (push (cdr entry) (cdr cell))))
    (mapcar (lambda (cell) (cons (car cell) (sort (cdr cell) #'string<))) table)))

(defun dorg-derive-families--selector (files)
  "Return the ERT selector running exactly the tests defined in FILES."
  `(satisfies
    (lambda (test)
      (member (file-name-nondirectory
               (or (symbol-file (ert-test-name test) 'ert--test) ""))
              ',files))))

(defun dorg-derive-families--executed (log)
  "Return (SUMMARY-COUNT . EXECUTED-NAMES) parsed from runner LOG text.
SUMMARY-COUNT is nil when no ERT summary line is present."
  (let (names count (start 0))
    (while (string-match "^ *[A-Za-z]+ +[0-9]+/[0-9]+ +\\(\\S-+\\)" log start)
      (push (match-string 1 log) names)
      (setq start (match-end 0)))
    (when (string-match "^Ran \\([0-9]+\\) tests," log)
      (setq count (string-to-number (match-string 1 log))))
    (cons count (sort names #'string<))))

(defun dorg-derive-families--run (repo base-arg out dry runner)
  "Derive and run the families of REPO's HEAD against BASE-ARG into OUT.
DRY non-nil stops after the selector.  RUNNER is the runner file relative to
REPO.  Returns the exit code, or signals a refusal."
  (dorg-derive-families--write out "identity.txt" (format "base-arg %s\n" base-arg))
  (dorg-derive-families--phase out 'identity)
  (when (string-empty-p base-arg)
    (dorg-derive-families--refuse 4 "BASE is required"))
  (let* ((source dorg-derive-families--source)
         (elc (concat source "c")))
    (when (and (file-exists-p elc) (not (file-newer-than-file-p source elc)))
      (dorg-derive-families--refuse 5 "stale tool bytecode %s" elc)))
  (let* ((head (condition-case nil (dorg-derive-families--identity repo t)
                 (dorg-derive-families-refusal
                  (dorg-derive-families--refuse 4 "no candidate HEAD"))))
         (base (let ((r (dorg-derive-families--git-raw
                         repo "rev-parse" "--verify" "--quiet" (concat base-arg "^{commit}"))))
                 (unless (eql 0 (car r))
                   (dorg-derive-families--refuse 4 "invalid BASE %S" base-arg))
                 (string-trim (cdr r))))
         (merge-base (let ((r (dorg-derive-families--git-raw
                               repo "merge-base" base (nth 0 head))))
                       (unless (eql 0 (car r))
                         (dorg-derive-families--refuse 4 "no merge-base for %s" base))
                       (string-trim (cdr r)))))
    (unless (nth 2 head)
      (dorg-derive-families--refuse 4 "the worktree is not clean"))
    (dorg-derive-families--write
     out "identity.txt"
     (format "base-arg %s\nbase %s\nmerge-base %s\nhead %s\ntree %s\n"
             base-arg base merge-base (nth 0 head) (nth 1 head)))
    (dorg-derive-families--phase out 'derive)
    (let* ((selection (dorg-derive-families-select
                       (dorg-derive-families-records repo merge-base (nth 0 head))
                       (dorg-derive-families-index repo (nth 0 head))))
           (files (mapcar #'car selection)))
      (dorg-derive-families--write out "derived.eld"
                                   (let ((print-length nil) (print-level nil))
                                     (prin1-to-string selection)))
      (unless files
        (dorg-derive-families--refuse 2 "the derivation selected no test file"))
      (dorg-derive-families--phase out 'count)
      (let* ((counts-file (expand-file-name "counts.eld" out))
             (status (dorg-derive-families--child
                      (append (dorg-derive-families--emacs-args runner)
                              (list "-l" dorg-derive-families--source "--eval"
                                    (format "(dorg-derive-families--count-batch %S '%S)"
                                            counts-file files)))
                      (expand-file-name "count.log" out)))
             (counts (and (eql 0 status) (file-exists-p counts-file)
                          (with-temp-buffer
                            (insert-file-contents counts-file)
                            (condition-case nil (read (current-buffer)) (error nil))))))
        (unless counts
          (dorg-derive-families--refuse 5 "counting child failed (%S)" status))
        (let ((empty (cl-remove-if #'cdr counts)))
          (when empty
            (dorg-derive-families--refuse 3 "no loaded test in %S" (mapcar #'car empty))))
        (dorg-derive-families--write
         out "selector.el" (prin1-to-string (dorg-derive-families--selector files)))
        (if dry
            (progn (dorg-derive-families--write out "result.txt" "exit 0\nverdict dry-run\n")
                   0)
          (dorg-derive-families--recheck repo head 'before-run t)
          (dorg-derive-families--phase out 'run)
          (dorg-derive-families--census out "census-before.txt")
          (let ((child nil))
            (unwind-protect
                (setq child (dorg-derive-families--child
                             (append (dorg-derive-families--emacs-args runner)
                                     (list "-l" dorg-derive-families--source "--eval"
                                           (format "(progn (dorg-derive-families--record-executions %S) (dorg-run-tests--batch (quote %S) t))"
                                                   (expand-file-name "executions.log" out)
                                                   (dorg-derive-families--selector files))))
                             (expand-file-name "run.log" out)))
              (dorg-derive-families--census out "census-after.txt"))
            (dorg-derive-families--phase out 'verdict)
            (dorg-derive-families--recheck repo head 'after-run nil)
            (let* ((log (with-temp-buffer
                          (insert-file-contents (expand-file-name "run.log" out))
                          (buffer-string)))
                   (executed (dorg-derive-families--executed log))
                   ;; `apply #'append' shares the last list with COUNTS; sort a
                   ;; copy so the per-file comparison below sees COUNTS intact.
                   (selected (sort (copy-sequence (apply #'append (mapcar #'cdr counts)))
                                   #'string<))
                   (by-file (dorg-derive-families--executed-by-file
                             (let ((file (expand-file-name "executions.log" out)))
                               (and (file-exists-p file)
                                    (with-temp-buffer (insert-file-contents file)
                                                      (buffer-string))))
                             files))
                   (per-file (equal by-file counts))
                   (verdict (cond ((not (eql 0 child)) (format "runner-exit %S" child))
                                  ((null (car executed)) "no-summary")
                                  ((not (and (= (car executed) (length selected))
                                             (equal (cdr executed) selected)))
                                   "executed-set-mismatch")
                                  ((not per-file) "executed-file-mismatch")
                                  (t "pass")))
                   (code (cond ((not (eql 0 child)) (if (integerp child) child 1))
                               ((equal verdict "pass") 0)
                               (t 6))))
              (dorg-derive-families--write out "executed.eld"
                                           (let ((print-length nil) (print-level nil))
                                             (prin1-to-string by-file)))
              (dorg-derive-families--write
               out "result.txt"
               (format "exit %d\nverdict %s\nchild-exit %S\nsummary-count %S\nexecuted %d\nselected %d\nper-file-match %S\n"
                       code verdict child (car executed) (length (cdr executed))
                       (length selected) per-file))
              code)))))))

(defun dorg-derive-families-run-batch ()
  "Batch entry for make test-derived; exits with the derived verdict code.
Reads DORG_DERIVED_BASE (required), DORG_DERIVED_OUT, DORG_DERIVED_DRY and
DORG_DERIVED_RUNNER from the environment; the candidate is the checkout in
`default-directory'."
  (let* ((repo default-directory)
         (base-arg (or (getenv "DORG_DERIVED_BASE") ""))
         (out (let ((given (getenv "DORG_DERIVED_OUT")))
                (if (and given (not (string-empty-p given)))
                    (expand-file-name given)
                  (make-temp-file "dorg-derived-" t))))
         (dry (let ((value (getenv "DORG_DERIVED_DRY")))
                (and value (not (member value '("" "0"))))))
         (runner (let ((value (getenv "DORG_DERIVED_RUNNER")))
                   (if (and value (not (string-empty-p value)))
                       value
                     "tests/dorg-tests-runner.el")))
         (code nil))
    (make-directory out t)
    (setq code
          (condition-case err
              (dorg-derive-families--run repo base-arg out dry runner)
            (dorg-derive-families-refusal
             (dorg-derive-families--write
              out "result.txt" (format "exit %d\nverdict refused: %s\n"
                                       (nth 1 err) (nth 2 err)))
             (nth 1 err))
            (error
             (dorg-derive-families--write
              out "result.txt" (format "exit 5\nverdict error: %s\n"
                                       (error-message-string err)))
             5)))
    (message "dorg derived families: exit %d, receipt %s" code out)
    (kill-emacs code)))

(provide 'dorg-derive-families)
;;; dorg-derive-families.el ends here
