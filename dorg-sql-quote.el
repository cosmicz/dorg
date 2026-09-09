;;; dorg-sql-quote.el --- Dependency-free SQL literal quoting -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; SQL string-literal quoting/escaping helpers, extracted as a leaf so
;; pure SQL-text consumers (`dorg-sql-gen', `dorg-keywords') can quote
;; values WITHOUT dragging in the whole `dorg-dolt' transport and its hard
;; `mysql.el' wire client.  This module depends only on the bottom layer;
;; it knows nothing about transport, connections, or the server.
;;
;; `dorg-sql-quote' / `dorg-sql-escape-string' are public API (see
;; `docs/api.org').  Their names are unchanged from when they lived in
;; `dorg-dolt.el'; only the home file moved.  `dorg-dolt' hard-requires
;; this module, so `(require \\='dorg-dolt)' still resolves both symbols.

;;; Code:

(require 'cl-lib)

(defun dorg-sql-escape-string (str)
  "Escape STR for use in a SQL string literal.
Doubles single quotes and escapes backslashes."
  (replace-regexp-in-string
   "'" "''"
   (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))

(defun dorg-sql-quote (val)
  "Return VAL as a SQL string literal, or NULL if nil."
  (if val
      (format "'%s'" (dorg-sql-escape-string val))
    "NULL"))

(defun dorg-sql--literal-sql (val)
  "Return the SQL literal for typed effect value VAL."
  (cond ((null val) "NULL")
        ((numberp val) (number-to-string val))
        (t (dorg-sql-quote val))))

(defun dorg-sql--effect-predicate-sql (payload)
  "Return the SQL predicate matching typed column alist PAYLOAD, or nil.
A nil cell value lowers as `IS NULL', while nil PAYLOAD is the distinct
whole-table predicate and returns nil.  Callers validate the typed effect
shape before delegating here, so delete generation and transaction proof
counts share the same final lowering and cannot drift."
  (when payload
    (mapconcat (lambda (c)
                 (if (null (cdr c))
                     (format "%s IS NULL" (symbol-name (car c)))
                   (format "%s = %s" (symbol-name (car c))
                           (dorg-sql--literal-sql (cdr c)))))
               payload " AND ")))

(provide 'dorg-sql-quote)
;;; dorg-sql-quote.el ends here
