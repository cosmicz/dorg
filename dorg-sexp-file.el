;;; dorg-sexp-file.el --- Atomic single-sexp file read/write -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The sexp-file protocol idiom — serialize ONE Lisp form with `prin1'
;; and read it back with `read' — is used by `dorg-dolt-server' for claim
;; files, the discovery index, and LOCK owner metadata.  Before this module
;; the protocol was repeated with crash-safety drift: some writers renamed a
;; temp into place atomically, others wrote in place where a concurrent reader
;; could observe a half-written file and a crash could truncate the target.
;;
;; This leaf extracts two primitives with ATOMICITY AS THE DEFAULT and no
;; dorg dependencies (built-ins only):
;;
;;   `dorg-sexp-read-file'         — lenient reader; nil (never signals) for
;;                                   absent/unreadable/empty/malformed input.
;;   `dorg-sexp-write-file-atomic' — writes a temp in the TARGET's own
;;                                   directory, then publishes it with a
;;                                   same-directory `rename-file' (atomic on
;;                                   POSIX), cleaning the temp on any failure.
;;
;; The reader deliberately returns the RAW form and imposes NO type
;; validation — each caller keeps its own `(consp form)' / `(stringp form)'
;; guard, so behavior is preserved exactly.
;;
;; The publish step routes through the internal `dorg-sexp-file--rename'
;; seam (a thin `rename-file' wrapper) so a crash BETWEEN temp-write and
;; publish can be simulated in tests without stubbing a C primitive
;; (cl-letf over a file primitive breaks native-comp).

;;; Code:

(defun dorg-sexp-file--rename (from to &optional overwrite)
  "Publish FROM onto TO with `rename-file' (OVERWRITE passed through).
The single publish seam: same-directory rename is atomic on POSIX, so a
concurrent reader never observes a partial TO.  Isolated as an ordinary
`defun' so the SPEC can simulate a crash at the publish step by stubbing
this symbol (never a C primitive — cl-letf over file primitives breaks
native-comp)."
  (rename-file from to overwrite))

(defun dorg-sexp-read-file (file)
  "Return the single Lisp form stored in FILE, or nil.
Returns nil — never signals — when FILE is absent, unreadable, empty, or
does not contain a readable Lisp form, so a corrupt runtime file never
wedges transport selection, server listing, or a worker.  The raw form is
returned as-is; type validation is the caller's responsibility."
  (when (file-exists-p file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun dorg-sexp-write-file-atomic (file form)
  "Serialize FORM into FILE atomically.
Writes FORM with `prin1' (binding `print-level'/`print-length' to nil so
deep/long structures are never truncated) into a temp file in FILE's OWN
directory, then publishes it via `dorg-sexp-file--rename' with overwrite
enabled.  A same-directory rename is atomic on POSIX, so a concurrent
reader never observes a partially written target and a crash never
truncates it.  The temp is deleted on any failure — no temp leak."
  (let* ((dir (file-name-directory (expand-file-name file)))
         (tmp (make-temp-file (expand-file-name "dorg-sexp-" dir))))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (let ((print-level nil) (print-length nil))
              (prin1 form (current-buffer))))
          (dorg-sexp-file--rename tmp file t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp))
        (ignore-errors (delete-file tmp))))))

(provide 'dorg-sexp-file)
;;; dorg-sexp-file.el ends here
