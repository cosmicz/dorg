;;; dorg-freshness.el --- Shared content-hash primitive for freshness tokens -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One content-hash primitive shared by every freshness/cache-identity
;; token in DOrg.  A backend freshness signature folds together the cheap
;; on-disk file attributes (size, mtime) with a raw-bytes content hash so
;; that a same-size, same-mtime edit — which the coarse (size . mtime)
;; signature would miss — still invalidates a warm cached read.
;;
;; The read plane (`dorg-ext-read') and the warm session layer
;; (`dorg-session') both need that content hash, and previously carried
;; verbatim copies of it.  Per the cache-identity doctrine (a cache key
;; and its invalidation stamp must share ONE identity function) the hash
;; lives here, once.
;;
;; This module is a zero-dependency leaf: it hashes bytes, nothing more.
;; It does not know about backends, Dolt HEADs, sessions, or cache
;; storage — those compose this primitive into their own tokens.
;;
;; Public API:
;; - `dorg-freshness-file-hash' — FILE -> SHA1 content hash string, or nil

;;; Code:

(defun dorg-freshness-file-hash (file)
  "Return a SHA1 content hash of FILE, or nil when FILE is unreadable.
Reads raw bytes (far cheaper than an Org parse) so the freshness token
detects same-size, same-mtime edits the coarse file signature would miss."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (condition-case nil
          (progn (insert-file-contents-literally file)
                 (secure-hash 'sha1 (current-buffer)))
        (error nil)))))

(provide 'dorg-freshness)
;;; dorg-freshness.el ends here
