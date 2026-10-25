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

;;; ─── Typed rows and multi-row insert lowering ───────────────────────────
;;
;; A write that inserts many rows used to send one single-row INSERT per row,
;; each a separate round trip inside the transaction.  Generators now describe
;; an insert as a typed row; this lowering turns a statement list into SQL.
;;
;; The lowering is ORDER-EXACT.  It merges only ADJACENT rows of one shape
;; (table plus exact column list) into a multi-row INSERT ... VALUES (r1), (r2)
;; in the original row order, and passes every SQL string through unchanged
;; as a barrier.  It needs no foreign-key knowledge: a parent row still
;; precedes its child.  Pinned Dolt (1.86.4) refuses a forward reference or a
;; dangling parent inside one multi-row statement exactly as it refuses the
;; scalar sequence, and a failing statement inserts none of its rows.
;; Producers that want large runs order their own rows under their own key
;; knowledge (see `dorg-sql-gen-order-core-rows').
;;
;; A row's literals are rendered once, at construction, from a closed set of
;; typed values, so no caller can hand raw SQL to the lowering.

(define-error 'dorg-sql-row-error "dorg: invalid typed SQL row")

(defconst dorg-sql-lower-max-rows 500
  "Most rows one lowered multi-row INSERT carries.")

(defconst dorg-sql-lower-max-bytes 1048576
  "Most UTF-8 wire bytes of one lowered multi-row INSERT, complete statement.
A single row larger than this is still emitted, alone.  The bound sits far
below the 16 MiB packet the client negotiates.")

(defconst dorg-sql--row-identifier-re "\\`[A-Za-z_][A-Za-z0-9_]*\\'"
  "A bare SQL identifier: the only form a row's table or column may take.")

(cl-defstruct (dorg-sql-row (:constructor nil) (:copier nil))
  "One typed single-row INSERT, built only by `dorg-sql-row-make'.
TABLE is the table name, COLUMNS the column symbols and LITERALS the rendered
SQL literal of each value, in column order."
  (table nil :read-only t)
  (columns nil :read-only t)
  (literals nil :read-only t))

(defun dorg-sql-row--raise (message &rest data)
  "Signal `dorg-sql-row-error' with MESSAGE and DATA."
  (signal 'dorg-sql-row-error (cons message data)))

(defun dorg-sql-row--literal (value)
  "Return the SQL literal for typed VALUE, or signal.
The closed value set:
- nil is NULL;
- an integer (fixnum or bignum) or a float is its decimal form;
- a string is a quoted text literal;
- `:true' and `:false' are TRUE and FALSE;
- (:hex . BYTES) is X\\='..\\=' for BYTES, a unibyte (or all-ASCII) string."
  (cond
   ((null value) "NULL")
   ((numberp value) (number-to-string value))
   ((stringp value) (dorg-sql-quote value))
   ((eq value :true) "TRUE")
   ((eq value :false) "FALSE")
   ((and (consp value) (eq (car value) :hex) (stringp (cdr value))
         (or (not (multibyte-string-p (cdr value)))
             (string-match-p "\\`[[:ascii:]]*\\'" (cdr value))))
    (concat "X'" (mapconcat (lambda (byte) (format "%02x" byte)) (cdr value) "")
            "'"))
   (t (dorg-sql-row--raise "unsupported SQL literal value" value))))

(defun dorg-sql-row-make (table columns values)
  "Return a typed row inserting VALUES into COLUMNS of TABLE.
TABLE is a bare identifier string and COLUMNS a non-empty list of
bare-identifier symbols, one per value.  VALUES are typed as documented by
`dorg-sql-row--literal'.  Anything else signals `dorg-sql-row-error'."
  (unless (and (stringp table) (string-match-p dorg-sql--row-identifier-re table))
    (dorg-sql-row--raise "row table is not a bare identifier" table))
  ;; Finite proper lists first: nothing below may walk a dotted or cyclic one.
  (unless (and (proper-list-p columns) (proper-list-p values))
    (dorg-sql-row--raise "row columns and values must be proper lists"))
  (unless (and (consp columns)
               (cl-every (lambda (column)
                           (and (symbolp column) column
                                (string-match-p dorg-sql--row-identifier-re
                                                (symbol-name column))))
                         columns))
    (dorg-sql-row--raise "row columns are not bare identifier symbols" columns))
  (unless (= (length columns) (length values))
    (dorg-sql-row--raise "row values do not match its columns" columns values))
  ;; `record' is the struct's own layout: the type tag, then the slots in
  ;; declaration order.  No other constructor exists.
  (record 'dorg-sql-row table columns (mapcar #'dorg-sql-row--literal values)))

(defun dorg-sql-row--prefix (row)
  "Return the INSERT prefix of ROW, through \" VALUES \"."
  (format "INSERT INTO %s (%s) VALUES "
          (dorg-sql-row-table row)
          (mapconcat #'symbol-name (dorg-sql-row-columns row) ", ")))

(defun dorg-sql-row--tuple (row)
  "Return ROW's parenthesized value tuple."
  (concat "(" (mapconcat #'identity (dorg-sql-row-literals row) ", ") ")"))

(defun dorg-sql-row-sql (row)
  "Return the single-row INSERT statement for ROW."
  (concat (dorg-sql-row--prefix row) (dorg-sql-row--tuple row)))

(defun dorg-sql--wire-bytes (string)
  "Return the number of UTF-8 bytes STRING occupies on the wire."
  (string-bytes (encode-coding-string string 'utf-8)))

(defun dorg-sql-lower-statements (items)
  "Return SQL statements for ITEMS, merging adjacent same-shape rows.
ITEMS is a list whose elements are SQL strings or `dorg-sql-row' values.  A
string is emitted unchanged, in place, and ends any run.  A maximal run of
ADJACENT rows with the same table and exact column list becomes multi-row
INSERTs in row order.  A run splits before a row that would make the
statement exceed `dorg-sql-lower-max-rows' rows or `dorg-sql-lower-max-bytes'
UTF-8 bytes; a single row over the byte cap stands alone.  ITEMS is not
modified; it must be a finite proper list."
  (unless (proper-list-p items)
    (dorg-sql-row--raise "lowering items must be a proper list"))
  (let (out shape prefix tuples count bytes)
    (cl-flet ((flush ()
                (when shape
                  (push (concat prefix (mapconcat #'identity (nreverse tuples) ", "))
                        out)
                  (setq shape nil tuples nil))))
      (dolist (item items)
        (cond
         ((stringp item) (flush) (push item out))
         ((dorg-sql-row-p item)
          (let* ((row-shape (cons (dorg-sql-row-table item)
                                  (dorg-sql-row-columns item)))
                 (tuple (dorg-sql-row--tuple item))
                 (tuple-bytes (dorg-sql--wire-bytes tuple)))
            (if (and shape (equal shape row-shape)
                     (< count dorg-sql-lower-max-rows)
                     ;; Two bytes for the ", " delimiter.
                     (<= (+ bytes 2 tuple-bytes) dorg-sql-lower-max-bytes))
                (setq tuples (cons tuple tuples)
                      count (1+ count)
                      bytes (+ bytes 2 tuple-bytes))
              (flush)
              (setq shape row-shape
                    prefix (dorg-sql-row--prefix item)
                    tuples (list tuple)
                    count 1
                    bytes (+ (dorg-sql--wire-bytes prefix) tuple-bytes)))))
         (t (dorg-sql-row--raise
             "lowering takes SQL strings and typed rows only" item))))
      (flush))
    (nreverse out)))

(provide 'dorg-sql-quote)
;;; dorg-sql-quote.el ends here
