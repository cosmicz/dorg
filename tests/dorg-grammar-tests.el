;;; dorg-grammar-tests.el --- Golden tests for hoisted grammar constants -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Golden / round-trip tests for the shared grammar constants hoisted in
;; dorg-health.tqlm8.  The whole contract of that cleanup is that every
;; generated string and regexp is BYTE-IDENTICAL to the hand-written literal
;; it replaced.  These tests pin each builder's output to the exact literal
;; captured from the pre-hoist source so a drift is caught mechanically.
;;
;; The literals below are the source strings as they appeared before the
;; hoist — do not "simplify" them; they are the golden reference.
;;
;; The GOLDEN and ROUND-TRIP tests below prove each builder/constant
;; PRODUCES the right string.  They do NOT prove production ADOPTS it: an
;; implementation that defines every symbol yet leaves the literal call
;; sites untouched keeps all of them green.  The SOURCE-STRUCTURE guards at
;; the end close that gap — they read the production sources as text and
;; fail while any scoped literal survives OUTSIDE its canonical home.  They
;; are therefore RED against the pre-hoist tree *because stale call sites
;; remain*, a signal distinct from the goldens (RED only because the new
;; symbols are undefined).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-id)
(require 'dorg-backend)
(require 'dorg-model)

;;; ─── Item 1: property + keyword constants ──────────────────────────

(ert-deftest dorg-test-grammar-id-property-value ()
  "`dorg-id-property' is exactly the org property key \"DORG_ID\"."
  (should (equal dorg-id-property "DORG_ID")))

(ert-deftest dorg-test-grammar-dolt-dir-keyword-value ()
  "`dorg-dolt-dir-keyword' is exactly \"DORG_DOLT_DIR\"."
  (should (equal dorg-dolt-dir-keyword "DORG_DOLT_DIR")))

(ert-deftest dorg-test-grammar-hash-keyword-value ()
  "`dorg-hash-keyword' is exactly \"DORG_HASH\"."
  (should (equal dorg-hash-keyword "DORG_HASH")))

;;; ─── Item 2: identity-drawer serialization ─────────────────────────

(ert-deftest dorg-test-grammar-identity-drawer-golden ()
  "The drawer string matches the pre-hoist literal for a sample id."
  (should (equal (dorg-model-identity-drawer-string 42)
                 ":PROPERTIES:\n:DORG_ID: 42\n:END:\n")))

(ert-deftest dorg-test-grammar-identity-drawer-tracks-property ()
  "The drawer key is `dorg-id-property', not an independent literal."
  (should (string-match-p (concat "^:PROPERTIES:\n:"
                                  (regexp-quote dorg-id-property)
                                  ": 7\n:END:\n$")
                          (dorg-model-identity-drawer-string 7))))

;;; ─── Source-structure guards: production ADOPTS the hoist ──────────
;;
;; These read the production `dorg*.el' sources as text — they require NO
;; new runtime symbol, so they run and fail meaningfully against the
;; pre-hoist tree, RED precisely because stale literal call sites remain.
;; A define-only implementation (symbols defined, call sites untouched)
;; keeps every golden green yet leaves these RED.

(defconst dorg-grammar-tests--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

(defun dorg-grammar-tests--prod-files ()
  "Absolute paths of every root `dorg*.el' production module.
Includes `dorg.el' and `dorg-app.el'; excludes the tests/ tree."
  (mapcar (lambda (f) (expand-file-name f dorg-grammar-tests--root))
          (directory-files dorg-grammar-tests--root nil "\\`dorg.*\\.el\\'")))

(defun dorg-grammar-tests--strings-in (x)
  "Return a list of every string literal contained in form X."
  (cond
   ((stringp x) (list x))
   ((consp x) (append (dorg-grammar-tests--strings-in (car x))
                      (dorg-grammar-tests--strings-in (cdr x))))
   ((and (arrayp x) (not (stringp x)))  ; vector / char-table
    (let (out)
      (dotimes (i (length x))
        (setq out (nconc out (dorg-grammar-tests--strings-in (aref x i)))))
      out))
   (t nil)))

(defun dorg-grammar-tests--def-name (form)
  "Return the symbol defined by top-level FORM, or nil."
  (when (and (consp form)
             (memq (car form)
                   '(defun defmacro defsubst defvar defvar-local defconst
                      cl-defun cl-defsubst cl-defmethod cl-defgeneric
                      define-inline)))
    (and (symbolp (cadr form)) (cadr form))))

(defun dorg-grammar-tests--code-strings (form)
  "Return FORM's string literals with any docstring slot removed.
For a def-form the docstring (4th element, when a string) is dropped so
prose that merely mentions a grammar token is never mistaken for a live
call site."
  (let ((body form))
    (when (and (dorg-grammar-tests--def-name form)
               (nthcdr 3 form)
               (stringp (nth 3 form)))
      (setq body (append (cl-subseq form 0 3) (nthcdr 4 form))))
    (dorg-grammar-tests--strings-in body)))

(defun dorg-grammar-tests--forms (path)
  "Return a list of (NAME . FORM) for each top-level form read from PATH.
NAME is the defined symbol for def-forms, else nil."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((eof (make-symbol "eof")) out)
      (while (let ((form (condition-case nil (read (current-buffer))
                           (end-of-file eof))))
               (if (eq form eof)
                   nil
                 (push (cons (dorg-grammar-tests--def-name form) form) out)
                 t)))
      (nreverse out))))

(defun dorg-grammar-tests--offenders (files predicate allowed)
  "Return \"FILE:NAME\" for each form whose code holds a matching literal.
FILES are production paths; PREDICATE is applied to every non-docstring
string of every top-level form; a form is exempt when its defined symbol
is in ALLOWED.  The result is the set of surviving stale call sites."
  (let (out)
    (dolist (path files)
      (let ((base (file-name-nondirectory path)))
        (dolist (nf (dorg-grammar-tests--forms path))
          (let ((name (car nf)))
            (unless (memq name allowed)
              (when (cl-some predicate
                             (dorg-grammar-tests--code-strings (cdr nf)))
                (push (format "%s:%s" base (or name "<top>")) out)))))))
    (nreverse out)))

(defun dorg-grammar-tests--symbol-uses (sym form)
  "Count occurrences of symbol SYM anywhere inside FORM."
  (cond ((eq form sym) 1)
        ((consp form) (+ (dorg-grammar-tests--symbol-uses sym (car form))
                         (dorg-grammar-tests--symbol-uses sym (cdr form))))
        (t 0)))

(defun dorg-grammar-tests--call-count (sym)
  "Count references to SYM across production, excluding its own definition."
  (let ((n 0))
    (dolist (path (dorg-grammar-tests--prod-files))
      (dolist (nf (dorg-grammar-tests--forms path))
        (unless (eq (car nf) sym)
          (setq n (+ n (dorg-grammar-tests--symbol-uses sym (cdr nf)))))))
    n))

;;; Item 1 — property + keyword constants are the single source

(ert-deftest dorg-test-grammar-adopt-id-property-single-source ()
  "The plain \"DORG_ID\" string literal survives only on `dorg-id-property'.
RED while any of the 17 pre-hoist call sites still hold the raw literal."
  (should (null (dorg-grammar-tests--offenders
                 (dorg-grammar-tests--prod-files)
                 (lambda (s) (equal s "DORG_ID"))
                 '(dorg-id-property)))))

(ert-deftest dorg-test-grammar-adopt-dolt-dir-keyword-single-source ()
  "\"DORG_DOLT_DIR\" survives only on `dorg-dolt-dir-keyword'.
RED while render/registry (incl. the marker list) hold the raw literal."
  (should (null (dorg-grammar-tests--offenders
                 (dorg-grammar-tests--prod-files)
                 (lambda (s) (equal s "DORG_DOLT_DIR"))
                 '(dorg-dolt-dir-keyword)))))

(ert-deftest dorg-test-grammar-adopt-hash-keyword-single-source ()
  "\"DORG_HASH\" survives only on `dorg-hash-keyword'.
RED while render/sync-state hold the raw literal."
  (should (null (dorg-grammar-tests--offenders
                 (dorg-grammar-tests--prod-files)
                 (lambda (s) (equal s "DORG_HASH"))
                 '(dorg-hash-keyword)))))

;;; Item 2 — identity-drawer emitters call the shared helper

(ert-deftest dorg-test-grammar-adopt-identity-drawer-single-source ()
  "No production form emits an inline \":DORG_ID: %d/%s\" drawer/key literal.
Matches only the drawer frame and single-line key emitters — NOT the
`:DORG_ID:' scan regexp, error message, or property-line matcher, which
are out of scope.  RED while the six emitters hold the raw literal."
  (should (null (dorg-grammar-tests--offenders
                 (dorg-grammar-tests--prod-files)
                 (lambda (s) (string-match-p ":DORG_ID: %[ds]" s))
                 '(dorg-model-identity-drawer-string)))))

(ert-deftest dorg-test-grammar-adopt-identity-drawer-callers ()
  "The identity-drawer emitters call `dorg-model-identity-drawer-string'.
The two surviving model emitters must reference the helper exactly."
  (should (= (dorg-grammar-tests--call-count 'dorg-model-identity-drawer-string)
             2)))

(provide 'dorg-grammar-tests)
;;; dorg-grammar-tests.el ends here
