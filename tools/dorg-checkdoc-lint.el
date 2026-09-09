;;; dorg-checkdoc-lint.el --- Non-gating checkdoc pass for `make lint'  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run checkdoc over the root `dorg*.el' files and report per-file finding
;; counts.  Findings are NON-GATING until the tracked-case debt project
;; closes; only a reporter error (checkdoc itself failing on a file) exits
;; non-zero.  Invoked by the Makefile `lint' target as
;; `emacs --batch -Q -L . -L ./tests -l tools/dorg-checkdoc-lint.el'.
;;
;; This body used to live inline in the Makefile recipe as a multi-line
;; `--eval' with backslash continuations.  GNU make 3.81 joins those lines
;; inside the quoted string; GNU make 4 passes the backslash-newline through
;; literally, so Emacs read a bare `\' symbol and CI lint failed with
;; "void-variable \".  A file has no such portability seam.

;;; Code:

(require 'checkdoc)

(let* ((files (directory-files "." t "^dorg.*\\.el$"))
       (diagnostic-name "*dorg-checkdoc-lint*")
       (checkdoc-diagnostic-buffer diagnostic-name)
       (checkdoc-pending-errors nil)
       (counts (make-hash-table :test 'equal))
       (ok t)
       (total 0)
       rows)
  (when (get-buffer diagnostic-name)
    (kill-buffer diagnostic-name))
  (unwind-protect
      (progn
        (dolist (f files)
          (with-temp-buffer
            (setq buffer-file-name f)
            (insert-file-contents f)
            (emacs-lisp-mode)
            (condition-case err
                (checkdoc-current-buffer t)
              (error (setq ok nil)
                     (message "checkdoc: %s: reporter error: %s"
                              f (error-message-string err))))))
        (when (get-buffer diagnostic-name)
          (with-current-buffer diagnostic-name
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\(.+\\.el\\):[0-9]+: " nil t)
              (let ((file (match-string-no-properties 1)))
                (puthash file (1+ (gethash file counts 0)) counts)
                (setq total (1+ total))))))
        (maphash (lambda (file count)
                   (push (cons file count) rows))
                 counts)
        (setq rows (sort rows (lambda (a b)
                                (string< (car a) (car b)))))
        (dolist (row rows)
          (message "checkdoc: %s: %d finding%s"
                   (car row) (cdr row) (if (= (cdr row) 1) "" "s")))
        (message
         "checkdoc: NON-GATING tracked-case: %d findings across %d/%d files"
         total (length rows) (length files))
        (unless ok (kill-emacs 1)))
    (when (get-buffer diagnostic-name)
      (kill-buffer diagnostic-name))))

;;; dorg-checkdoc-lint.el ends here
