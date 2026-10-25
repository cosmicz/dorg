;;; reference.el --- Independent reference model for insert lowering -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A test-side model of multi-row insert lowering that works only from the
;; frozen base oracle's scalar statement strings.  It shares no code with the
;; product, which lowers typed rows: the model re-derives every expected
;; multi-row statement by string surgery on the base's one-row INSERTs, under
;; the frozen design's rules:
;;
;; - core import rows move after every core barrier, grouped by the core
;;   table order (stable inside each table); the dorg_files upsert and the
;;   import marker stay last;
;; - BM25 document and posting inserts form one contiguous block, which is
;;   stably partitioned documents first;
;; - adjacent same-shape rows merge, at most 500 per statement and at most
;;   1,048,576 UTF-8 bytes of complete statement text;
;; - every other statement is an unmoved barrier.
;;
;; The model's table order is written out here independently of the
;; product's constant.

;;; Code:

(require 'cl-lib)

(defconst dorg-ibr-core-order
  '("headings" "heading_tags" "heading_items" "text_items" "keyword_items"
    "planning_items" "drawer_items" "property_entries" "drawer_lines")
  "The frozen design's core table order, restated for the model.")

(defconst dorg-ibr-max-rows 500
  "The frozen row cap per lowered statement.")

(defconst dorg-ibr-max-bytes 1048576
  "The frozen UTF-8 wire-byte cap per lowered statement.")

(defun dorg-ibr-split (statement tables)
  "Return (PREFIX . TUPLE) when STATEMENT is a one-row INSERT into TABLES.
PREFIX runs through \" VALUES \" and TUPLE is the parenthesized row.  The
base generators emit no trailing clause for these tables, so the tuple is
the statement's remainder."
  (when (and (stringp statement)
             (string-match "\\`INSERT INTO \\([a-z_0-9]+\\) ([^()]*) VALUES " statement)
             (member (match-string 1 statement) tables))
    (cons (substring statement 0 (match-end 0))
          (substring statement (match-end 0)))))

(defun dorg-ibr-table (statement)
  "Return the table named by INSERT, DELETE or UPDATE STATEMENT, or nil."
  (when (and (stringp statement)
             (string-match
              "\\`\\(?:INSERT INTO\\|DELETE FROM\\|UPDATE\\) \\([a-z_0-9]+\\)"
              statement))
    (match-string 1 statement)))

(defun dorg-ibr-bytes (string)
  "Return the UTF-8 wire bytes of STRING."
  (string-bytes (encode-coding-string string 'utf-8)))

(defun dorg-ibr-lower (statements tables)
  "Merge adjacent same-prefix one-row INSERTs into TABLES within STATEMENTS."
  (let (out prefix tuples bytes)
    (cl-flet ((flush ()
                (when prefix
                  (push (concat prefix (mapconcat #'identity (nreverse tuples) ", "))
                        out)
                  (setq prefix nil tuples nil bytes 0))))
      (dolist (statement statements)
        (let ((split (dorg-ibr-split statement tables)))
          (cond
           ((null split) (flush) (push statement out))
           ((and (equal (car split) prefix)
                 (< (length tuples) dorg-ibr-max-rows)
                 (<= (+ bytes 2 (dorg-ibr-bytes (cdr split))) dorg-ibr-max-bytes))
            (push (cdr split) tuples)
            (setq bytes (+ bytes 2 (dorg-ibr-bytes (cdr split)))))
           (t (flush)
              (setq prefix (car split)
                    tuples (list (cdr split))
                    bytes (+ (dorg-ibr-bytes (car split))
                             (dorg-ibr-bytes (cdr split))))))))
      (flush))
    (nreverse out)))

(defun dorg-ibr-reorder-core (core-sql)
  "Return base CORE-SQL in the frozen design's producer order.
The last two statements (file binding and import marker) stay last."
  (let* ((n (length core-sql))
         (body (cl-subseq core-sql 0 (- n 2)))
         (tail (last core-sql 2))
         (barriers (cl-remove-if (lambda (s) (dorg-ibr-split s dorg-ibr-core-order))
                                 body))
         (rows (cl-remove-if-not (lambda (s) (dorg-ibr-split s dorg-ibr-core-order))
                                 body)))
    (append barriers
            (cl-mapcan (lambda (table)
                         (cl-remove-if-not
                          (lambda (s) (equal table (dorg-ibr-table s)))
                          rows))
                       dorg-ibr-core-order)
            tail)))

(defconst dorg-ibr-bm25-tables
  '("dorg_bm25_documents" "dorg_bm25_postings" "dorg_bm25_terms"
    "dorg_bm25_manifest")
  "The BM25 extension's tables.")

(defun dorg-ibr-reorder-bm25 (ext-sql)
  "Return base EXT-SQL with its BM25 document/posting insert block partitioned.
Signals when those inserts are not one contiguous block, because the frozen
partition is defined only for that shape."
  (let* ((block-p (lambda (s)
                    (and (dorg-ibr-split s '("dorg_bm25_documents"
                                             "dorg_bm25_postings"))
                         t)))
         (start (cl-position-if block-p ext-sql))
         (end (and start (cl-position-if-not block-p ext-sql :start start))))
    (if (null start)
        ext-sql
      (let* ((end (or end (length ext-sql)))
             (block (cl-subseq ext-sql start end)))
        (when (cl-position-if block-p ext-sql :start end)
          (error "BM25 insert block is not contiguous"))
        (append (cl-subseq ext-sql 0 start)
                (cl-remove-if-not (lambda (s) (equal "dorg_bm25_documents"
                                                     (dorg-ibr-table s)))
                                  block)
                (cl-remove-if-not (lambda (s) (equal "dorg_bm25_postings"
                                                     (dorg-ibr-table s)))
                                  block)
                (cl-subseq ext-sql end))))))

(defun dorg-ibr-expected-import (statements core-sql)
  "Return the expected lowered transaction for base import STATEMENTS.
CORE-SQL is the base core prefix of STATEMENTS; the rest is extension SQL."
  (let ((ext (nthcdr (length core-sql) statements)))
    (cl-assert (equal core-sql (cl-subseq statements 0 (length core-sql))))
    (append (dorg-ibr-lower (dorg-ibr-reorder-core core-sql) dorg-ibr-core-order)
            (dorg-ibr-lower (dorg-ibr-reorder-bm25 ext) dorg-ibr-bm25-tables))))

(defun dorg-ibr-ext-p (statement)
  "Non-nil when STATEMENT writes a BM25 table."
  (member (dorg-ibr-table statement) dorg-ibr-bm25-tables))

(defun dorg-ibr-expected-mutation (statements)
  "Return the expected lowered batch for base mutation STATEMENTS.
Core statements stay scalar and unmoved; the trailing BM25 block is
partitioned and lowered."
  (let* ((start (or (cl-position-if #'dorg-ibr-ext-p statements)
                    (length statements)))
         (core (cl-subseq statements 0 start))
         (ext (nthcdr start statements)))
    (cl-assert (cl-every #'dorg-ibr-ext-p ext))
    (append core (dorg-ibr-lower (dorg-ibr-reorder-bm25 ext) dorg-ibr-bm25-tables))))

(provide 'dorg-insert-batching-reference)
;;; reference.el ends here
