;;; dorg-sexp-file-tests.el --- Tests for dorg-sexp-file -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the shared sexp-file protocol primitives
;; `dorg-sexp-read-file' and `dorg-sexp-write-file-atomic'.
;;
;; These pin the contract that dorg-dolt-server depends on:
;;
;;   - a lenient reader that returns nil (never signals) for an absent,
;;     unreadable, empty, or malformed sexp file, and returns the raw
;;     form otherwise (type validation is the caller's job);
;;
;;   - an ATOMIC writer whose load-bearing property is that a reader
;;     never observes a partially written target and a crash never
;;     truncates it — the write publishes via a temp-file + rename so
;;     the target transitions from old-content to new-content in one
;;     indivisible step.
;;
;; The atomicity proof (dorg-test-sexp-file-atomic-*) simulates a crash
;; BETWEEN the temp-write and the publish by stubbing the internal
;; `dorg-sexp-file--rename' seam (an ordinary defun, NOT a C primitive —
;; cl-letf over file primitives breaks native-comp) to signal, then
;; asserts the target is left fully old / absent, never truncated.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Soft require: the behavioral tests below need the module (they are RED
;; with `void-function' pre-IMPL), but the source-structure ADOPTION guards
;; scan the production sources as TEXT and must run — and fail on stale call
;; sites — INDEPENDENTLY of whether the helper symbols are defined yet.  A
;; hard require would abort the whole file load pre-IMPL and collapse those
;; two distinct RED signals into one.
(require 'dorg-sexp-file nil t)
;; The three reader type-guard BEHAVIOR proofs below drive the real production
;; readers with valid-but-wrong-type file contents, so they need the module
;; that defines them.  It is a stable, already-extant production module (the
;; readers exist pre-IMPL with their guards inline), so a hard require is safe
;; and does not collapse the pre-IMPL adoption RED signal the way a missing
;; dorg-sexp-file would.
(require 'dorg-dolt-server)

(defmacro dorg-sexp-file-tests--in-dir (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, then remove it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "dorg-sexp-file-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

;;; ─── Round-trip ─────────────────────────────────────────────────────

(ert-deftest dorg-test-sexp-file-roundtrip-forms ()
  "read(write(FORM)) == FORM for alist, plist, string, and nested forms."
  (dorg-sexp-file-tests--in-dir dir
    (dolist (form (list '((:host . "h") (:pid . 42) (:acquired-at . 1.5))
                        '(:mode :pull :request-id 7 :dirty-ids (1 2 3))
                        "/canonical/store/path"
                        '(a (b (c (d (e "deep")))))))
      (let ((file (expand-file-name "form.eld" dir)))
        (dorg-sexp-write-file-atomic file form)
        (should (equal (dorg-sexp-read-file file) form))))))

(ert-deftest dorg-test-sexp-file-overwrite-yields-new ()
  "A second write replaces the stored form with the new one."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "form.eld" dir)))
      (dorg-sexp-write-file-atomic file '(:v 1))
      (dorg-sexp-write-file-atomic file '(:v 2))
      (should (equal (dorg-sexp-read-file file) '(:v 2))))))

;;; ─── Lenient reader (never signals) ─────────────────────────────────

(ert-deftest dorg-test-sexp-file-read-absent-nil ()
  "Reading a nonexistent file returns nil, no signal."
  (dorg-sexp-file-tests--in-dir dir
    (should (null (dorg-sexp-read-file (expand-file-name "missing.eld" dir))))))

(ert-deftest dorg-test-sexp-file-read-empty-nil ()
  "Reading an empty file returns nil, no signal."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "empty.eld" dir)))
      (with-temp-file file)                ; create empty
      (should (null (dorg-sexp-read-file file))))))

(ert-deftest dorg-test-sexp-file-read-malformed-nil ()
  "Reading a file whose contents are not a readable Lisp form returns nil."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "garbage.eld" dir)))
      (with-temp-file file (insert "(unterminated ((("))
      (should (null (dorg-sexp-read-file file))))))

(ert-deftest dorg-test-sexp-file-read-unreadable-nil ()
  "Reading an EXISTING but unreadable file returns nil, no signal (AC3).

Uses a real filesystem-permission change (chmod 000) — native-comp-safe,
no `cl-letf' over a file primitive — so the reader's actual `file-error'
boundary is exercised, not a stubbed one.  A reader that handles absent/
empty/malformed input but lets `insert-file-contents' propagate its
permission-denied `file-error' fails this test (RED for the no-boundary
mutant).  Permissions are restored in cleanup so teardown is reliable, and
the test skips explicitly if the platform will not enforce unreadability
(e.g. running as root), rather than silently asserting on a readable file."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "denied.eld" dir)))
      ;; A valid, readable-as-Lisp form: any nil result must come from the
      ;; permission boundary, not from absent/empty/malformed content.
      (with-temp-file file (insert "(:secret t)\n"))
      (set-file-modes file #o000)
      (unwind-protect
          (if (file-readable-p file)
              (ert-skip "platform will not make the fixture unreadable")
            (should (null (dorg-sexp-read-file file))))
        (set-file-modes file #o600)))))

;;; ─── Print-depth preservation (writer nil-binding is load-bearing) ──

(ert-deftest dorg-test-sexp-file-write-survives-bound-print-level ()
  "Deep structures survive the write even when the caller bound
`print-level'/`print-length' — the writer must locally rebind them to nil
so nothing is serialized as `...'."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "deep.eld" dir))
          (form '(a (b (c (d (e (f "leaf")))))))
          (print-level 2)
          (print-length 2))
      (dorg-sexp-write-file-atomic file form)
      (should (equal (dorg-sexp-read-file file) form)))))

;;; ─── No temp-file leak ──────────────────────────────────────────────

(ert-deftest dorg-test-sexp-file-write-leaves-no-temp ()
  "After a successful write the directory holds the target and nothing else.
The scan excludes only `.' and `..' (via `directory-files-no-dot-files-regexp'),
NOT every dot-prefixed name, so a hidden/`.#'-prefixed leftover temp is
counted as a leak."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "only.eld" dir)))
      (dorg-sexp-write-file-atomic file '(:ok t))
      (should (equal (directory-files dir nil directory-files-no-dot-files-regexp)
                     '("only.eld"))))))

;;; ─── ATOMICITY: crash between temp-write and publish ────────────────
;;
;; Non-vacuity: these are the load-bearing tests.  Removing the temp+rename
;; discipline (writing FORM directly into the target) makes them RED, because
;; a mid-write crash would then truncate/partially write the target itself.

(ert-deftest dorg-test-sexp-file-atomic-overwrite-crash-preserves-old ()
  "A crash between temp-write and publish leaves the EXISTING target fully
intact with its OLD form — never a truncated/partial file."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "claim.eld" dir)))
      (dorg-sexp-write-file-atomic file '(:generation :old))
      ;; Simulate a crash at the publish step.
      (cl-letf (((symbol-function 'dorg-sexp-file--rename)
                 (lambda (&rest _) (error "simulated crash before publish"))))
        (should-error (dorg-sexp-write-file-atomic file '(:generation :new))))
      ;; The old content must still be fully readable and unchanged.
      (should (equal (dorg-sexp-read-file file) '(:generation :old))))))

(ert-deftest dorg-test-sexp-file-atomic-fresh-crash-no-partial-target ()
  "A crash between temp-write and publish on a FRESH target leaves no file
at the target name — a reader sees absence (nil), never a partial form."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "fresh.eld" dir)))
      (cl-letf (((symbol-function 'dorg-sexp-file--rename)
                 (lambda (&rest _) (error "simulated crash before publish"))))
        (should-error (dorg-sexp-write-file-atomic file '(:generation :new))))
      (should-not (file-exists-p file))
      (should (null (dorg-sexp-read-file file))))))

(ert-deftest dorg-test-sexp-file-atomic-crash-leaves-no-temp ()
  "Even when the publish fails, the aborted temp file is cleaned up — INCLUDING
a hidden/`.#'-prefixed temp.  The scan excludes only `.'/`..', so a mutant that
uses a dot-prefixed temp and omits failure cleanup is caught as a leak."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "form.eld" dir)))
      (cl-letf (((symbol-function 'dorg-sexp-file--rename)
                 (lambda (&rest _) (error "simulated crash before publish"))))
        (should-error (dorg-sexp-write-file-atomic file '(:v 1))))
      (should (null (directory-files dir nil directory-files-no-dot-files-regexp))))))

;;; ─── ATOMICITY: publish is a same-directory rename ──────────────────
;;
;; Same-directory rename is what makes the publish atomic: a cross-directory
;; move can cross a filesystem boundary, where rename degrades to copy+delete
;; and is NO LONGER atomic.  This pins the seam's call contract so a mutant
;; that stages its temp with `make-temp-file' in the global TMPDIR (then
;; renames onto the target) FAILS — the temp must live in the TARGET's own
;; directory, TO must be the requested target, and overwrite must be enabled.

(ert-deftest dorg-test-sexp-file-atomic-temp-in-target-dir ()
  "The publish renames a temp that lives in the TARGET's own directory onto
the target, with overwrite non-nil.  Captures the seam's FROM/TO/overwrite
arguments and asserts: TO is exactly the target; FROM shares TO's directory
(so the rename is same-filesystem/atomic, never a TMPDIR cross-fs move);
FROM is not the target itself; overwrite is non-nil."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "claim.eld" dir))
          captured)
      ;; Signal after capturing so we inspect the intended publish arguments
      ;; without actually moving anything.
      (cl-letf (((symbol-function 'dorg-sexp-file--rename)
                 (lambda (from to &optional overwrite)
                   (setq captured (list from to overwrite))
                   (error "captured — no real publish"))))
        (should-error (dorg-sexp-write-file-atomic file '(:v 1))))
      (should captured)
      (cl-destructuring-bind (from to overwrite) captured
        ;; TO is exactly the requested target.
        (should (equal (expand-file-name to) (expand-file-name file)))
        ;; FROM (the temp) lives in the SAME directory as TO — a mutant using
        ;; make-temp-file in TMPDIR fails here.
        (should (equal (file-name-directory (expand-file-name from))
                       (file-name-directory (expand-file-name to))))
        ;; FROM is a distinct temp, not the target written in place.
        (should-not (equal (expand-file-name from) (expand-file-name file)))
        ;; Overwrite must be enabled so the publish replaces an existing target.
        (should overwrite)))))

;;; ─── AC7 adoption guard (source-structure, no runtime symbol) ───────
;;
;; These read the two production sources as TEXT, parse their top-level
;; def-forms, and assert every one of the six named sexp sites has DROPPED its
;; inline idiom (`prin1'/`with-temp-file' for writers, `insert-file-contents'
;; for readers) and now REFERENCES the shared helper.  They require NO symbol
;; from dorg-sexp-file, so they run and fail meaningfully against the
;; pre-extraction tree — RED precisely because the stale call sites remain,
;; a signal DISTINCT from the behavioral tests' `void-function' RED.  This
;; closes the tqlm8 gap: a define-only helper (module created, all six callers
;; untouched) keeps every behavioral test green yet leaves these two RED.

(defconst dorg-sexp-file-tests--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

(defconst dorg-sexp-file-tests--writer-sites
  '(("dorg-dolt-server.el" dorg-dolt-server--index-register
                           dorg-dolt-server--claim-write
                           dorg-dolt-server--lock-write-meta))
  "The three genuine sexp WRITERS, grouped by source file.  Each must adopt
`dorg-sexp-write-file-atomic' and drop its `with-temp-file'+`prin1' body.")

(defconst dorg-sexp-file-tests--reader-sites
  '(("dorg-dolt-server.el" dorg-dolt-server--indexed-store-dirs
                           dorg-dolt-server--read-claim-form
                           dorg-dolt-server--lock-read-meta))
  "The three genuine sexp READERS, grouped by source file.  Each must adopt
`dorg-sexp-read-file' and drop its `insert-file-contents'+`read' body.")

(defun dorg-sexp-file-tests--forms (path)
  "Alist of (NAME . FORM) for each top-level def-form read from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((eof (make-symbol "eof")) out)
      (while (let ((form (condition-case nil (read (current-buffer))
                           (end-of-file eof))))
               (if (eq form eof)
                   nil
                 (when (and (consp form)
                            (memq (car form) '(defun defsubst cl-defun cl-defsubst))
                            (symbolp (cadr form)))
                   (push (cons (cadr form) form) out))
                 t)))
      out)))

(defun dorg-sexp-file-tests--form-named (file name)
  "Return the top-level form defining NAME in repo FILE, or signal if absent."
  (let* ((path (expand-file-name file dorg-sexp-file-tests--root))
         (form (cdr (assq name (dorg-sexp-file-tests--forms path)))))
    (or form (error "No top-level definition of %s in %s" name file))))

(defun dorg-sexp-file-tests--uses-p (sym form)
  "Non-nil when symbol SYM appears anywhere inside FORM.
Used only for the FORBIDDEN-idiom checks (a stale writer/reader that still
mentions `prin1'/`with-temp-file'/`insert-file-contents' anywhere is an
offender), never for the positive adoption check — see `--calls-p'."
  (cond ((eq form sym) t)
        ((consp form) (or (dorg-sexp-file-tests--uses-p sym (car form))
                          (dorg-sexp-file-tests--uses-p sym (cdr form))))
        (t nil)))

(defun dorg-sexp-file-tests--calls-p (sym form)
  "Non-nil when FORM, read as CODE, contains a call (SYM ...) with SYM in
FUNCTION position.  Quoted data — (quote ...), (function ...)/#'... — is NOT
descended into, so a quoted helper symbol (`'dorg-sexp-read-file') or a
quoted list that merely NAMES the helper does NOT count as a call.

This is the call-aware adoption predicate: `--uses-p' matches any textual
occurrence of the symbol (including quoted data), which lets a non-adopting
body sneak the helper symbol in as data while still writing/reading in place.
`--calls-p' proves the site actually INVOKES the helper."
  (and (consp form)
       ;; Do not treat the contents of quoted/function-ref data as code.
       (not (memq (car form) '(quote function)))
       (or
        ;; A direct call: SYM in function position of this form.
        (eq (car form) sym)
        ;; The head may itself be a compound form (e.g. a lambda application).
        (dorg-sexp-file-tests--calls-p sym (car form))
        ;; Every argument is itself code.
        (cl-some (lambda (arg) (dorg-sexp-file-tests--calls-p sym arg))
                 (cdr form)))))

(ert-deftest dorg-test-sexp-file-adopt-writers ()
  "Every genuine sexp WRITER routes through `dorg-sexp-write-file-atomic' and
no longer inlines the `with-temp-file'+`prin1' idiom.
RED while any of the five writers still serializes in place."
  (let (offenders)
    (dolist (group dorg-sexp-file-tests--writer-sites)
      (dolist (name (cdr group))
        (let ((form (dorg-sexp-file-tests--form-named (car group) name)))
          (when (or (not (dorg-sexp-file-tests--calls-p
                          'dorg-sexp-write-file-atomic form))
                    (dorg-sexp-file-tests--uses-p 'prin1 form)
                    (dorg-sexp-file-tests--uses-p 'with-temp-file form))
            (push name offenders)))))
    (should (null offenders))))

(ert-deftest dorg-test-sexp-file-adopt-readers ()
  "Every genuine sexp READER routes through `dorg-sexp-read-file' (an actual
call, per `--calls-p') and no longer inlines the `insert-file-contents'+`read'
idiom.  The three caller-side type guards are NOT preserved by construction —
a direct replacement can drop them — so they are pinned separately by
`dorg-test-sexp-file-reader-type-guards-present' and the per-reader
wrong-type behavior proofs.
RED while any of the five readers still reads in place."
  (let (offenders)
    (dolist (group dorg-sexp-file-tests--reader-sites)
      (dolist (name (cdr group))
        (let ((form (dorg-sexp-file-tests--form-named (car group) name)))
          (when (or (not (dorg-sexp-file-tests--calls-p
                          'dorg-sexp-read-file form))
                    (dorg-sexp-file-tests--uses-p 'insert-file-contents form))
            (push name offenders)))))
    (should (null offenders))))

;;; ─── Call-aware predicate meta-tests (RED controls for blocker 1) ───
;;
;; reviewer's residual blocker: the prior adoption guard used `--uses-p',
;; which accepts ANY occurrence of the helper symbol — including quoted data.
;; A body like (quote dorg-sexp-write-file-atomic) plus a direct in-place
;; write passed the guard while remaining non-atomic.  These meta-tests pin
;; `--calls-p' as genuinely call-aware: it accepts a real (HELPER ...) call
;; and REJECTS the exact counterexamples the reviewer exhibited.

(ert-deftest dorg-test-sexp-file-calls-p-accepts-real-call ()
  "`--calls-p' detects an actual helper call in function position, including
one nested inside a `let' binding's init-form (the reader idiom)."
  (should (dorg-sexp-file-tests--calls-p
           'dorg-sexp-write-file-atomic
           '(defun w (f x) (dorg-sexp-write-file-atomic f x))))
  (should (dorg-sexp-file-tests--calls-p
           'dorg-sexp-read-file
           '(defun r (file)
              (let ((form (dorg-sexp-read-file file)))
                (and (consp form) form))))))

(ert-deftest dorg-test-sexp-file-calls-p-rejects-quoted-symbol ()
  "The reviewer's counterexamples: a quoted helper SYMBOL is data, not a
call.  Both a reader that only quotes the symbol and a writer that quotes
the symbol while writing in place must be REJECTED (would pass `--uses-p')."
  ;; reader counterexample: (quote dorg-sexp-read-file) — no real read call.
  (should-not (dorg-sexp-file-tests--calls-p
               'dorg-sexp-read-file
               '(defun r (file) (quote dorg-sexp-read-file))))
  ;; writer counterexample: names the helper as data, writes in place.
  (should-not (dorg-sexp-file-tests--calls-p
               'dorg-sexp-write-file-atomic
               '(defun w (file form)
                  (ignore (quote dorg-sexp-write-file-atomic))
                  (write-region (prin1-to-string form) nil file)))))

(ert-deftest dorg-test-sexp-file-calls-p-rejects-quoted-list-and-fn-ref ()
  "A quoted LIST whose head is the helper, and a `#''/`function' reference,
are both data/references — not calls in function position — and are rejected."
  (should-not (dorg-sexp-file-tests--calls-p
               'dorg-sexp-write-file-atomic
               '(defun w (f x) (quote (dorg-sexp-write-file-atomic f x)))))
  (should-not (dorg-sexp-file-tests--calls-p
               'dorg-sexp-read-file
               '(defun r (file) (mapcar #'dorg-sexp-read-file (list file))))))

;;; ─── AC7 reader type-guard proofs (blocker 2) ──────────────────────
;;
;; `dorg-sexp-read-file' deliberately returns the RAW form and delegates type
;; validation to the caller.  AC7 requires the three type-discriminating
;; readers to keep their caller-side guard (`stringp'/`consp') around the
;; helper's return.  A direct helper replacement that drops the guard is a
;; regression the adoption guard alone cannot catch (a bare
;; `(dorg-sexp-read-file file)' body satisfies `--calls-p').  Two complementary
;; proofs per reviewer's two accepted options:
;;
;;   (a) SOURCE-STRUCTURE: each reader must CALL its guard predicate.
;;   (b) BEHAVIOR: fed a valid-but-wrong-TYPE Lisp form, the reader must
;;       yield the nil/empty contract — never pass the wrong-type value
;;       through.  Deleting the guard turns each RED (pass-through / signal).

(defconst dorg-sexp-file-tests--reader-type-guards
  '(("dorg-dolt-server.el" dorg-dolt-server--indexed-store-dirs stringp)
    ("dorg-dolt-server.el" dorg-dolt-server--read-claim-form    consp)
    ("dorg-dolt-server.el" dorg-dolt-server--lock-read-meta     consp))
  "AC7 caller-side type guards that must survive adoption: (FILE READER GUARD).")

(ert-deftest dorg-test-sexp-file-reader-type-guards-present ()
  "Each type-discriminating reader still CALLS its guard predicate
(`stringp'/`consp') on the helper's raw return.  RED if a direct
`dorg-sexp-read-file' replacement drops the guard."
  (let (offenders)
    (dolist (spec dorg-sexp-file-tests--reader-type-guards)
      (cl-destructuring-bind (file name guard) spec
        (let ((form (dorg-sexp-file-tests--form-named file name)))
          (unless (dorg-sexp-file-tests--calls-p guard form)
            (push (list name guard) offenders)))))
    (should (null offenders))))

(ert-deftest dorg-test-sexp-file-read-claim-form-rejects-non-consp ()
  "A claim file holding a valid-but-wrong-type form (a bare string) is not a
claim: `--read-claim-form' returns nil, never the raw string.  Deleting the
`(consp form)' guard makes this RED (the string passes through)."
  (dorg-sexp-file-tests--in-dir dir
    (let ((file (expand-file-name "claim.eld" dir)))
      (with-temp-file file (prin1 "i-am-a-string-not-a-claim-alist" (current-buffer)))
      (should (null (dorg-dolt-server--read-claim-form file))))))

(ert-deftest dorg-test-sexp-file-lock-read-meta-rejects-non-consp ()
  "Lock owner metadata that reads back as a valid-but-wrong-type form (a
number) is rejected: `--lock-read-meta' returns nil.  Deleting the
`(consp form)' guard makes this RED."
  (dorg-sexp-file-tests--in-dir dir
    (let ((lock (file-name-as-directory (expand-file-name "claim.lock" dir))))
      (make-directory lock t)
      (with-temp-file (dorg-dolt-server--lock-meta-file lock)
        (prin1 42 (current-buffer)))
      (should (null (dorg-dolt-server--lock-read-meta lock))))))

(ert-deftest dorg-test-sexp-file-indexed-store-dirs-rejects-non-stringp ()
  "A discovery pointer holding a valid-but-wrong-type form (not a string
path) contributes nothing to the store list: `--indexed-store-dirs' yields
nil.  Deleting the `(stringp form)' guard makes this RED (the non-string is
treated as a path and `file-directory-p' signals a wrong-type error)."
  (dorg-sexp-file-tests--in-dir dir
    (let ((dorg-dolt-server-index-dir (file-name-as-directory dir)))
      (with-temp-file (expand-file-name "bogus.store" dir)
        (prin1 42 (current-buffer)))
      (should (null (dorg-dolt-server--indexed-store-dirs))))))

(provide 'dorg-sexp-file-tests)
;;; dorg-sexp-file-tests.el ends here
