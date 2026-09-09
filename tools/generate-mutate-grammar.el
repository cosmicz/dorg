;;; generate-mutate-grammar.el --- Emit generated mutation grammar -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Batch generator for docs/generated/mutate-grammar.org.  The semantic source
;; is dorg-mutate-grammar.el; this program only prints its deterministic Org
;; rendering to standard output.

;;; Code:

(let* ((script (or load-file-name buffer-file-name))
       (root (file-name-as-directory
              (expand-file-name ".." (file-name-directory script)))))
  (add-to-list 'load-path root))

(require 'dorg-mutate-grammar)

(princ (dorg-mutate-grammar-render-org))

;;; generate-mutate-grammar.el ends here
