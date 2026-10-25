;;; dorg-extension-bm25.el --- Load the DOrg BM25 extension -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The optional lexical read model follows the conventional extension loader:
;; `dorg-load-extension' loads this feature only when a ranked request needs
;; it.  Ordinary DOrg startup therefore never makes a non-search store require
;; the BM25 schema migration.

;;; Code:

(require 'dorg-bm25)

(dorg-bm25-register-extension)

(provide 'dorg-extension-bm25)
;;; dorg-extension-bm25.el ends here
