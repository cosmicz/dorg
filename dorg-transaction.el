;;; dorg-transaction.el --- Exact-base administrative transactions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Narrow transaction boundary for explicit administrative operations.
;; It reuses the typed-mutation store gate, compares the in-gate Dolt HEAD
;; with an exact planned base, validates the pending transaction state, and
;; commits once.  It does not implement mutation re-planning or whole-file
;; reconciliation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dorg-dolt)
(require 'dorg-mutate)

(define-error 'dorg-transaction-error
  "DOrg administrative transaction failed")

(define-error 'dorg-transaction-stale
  "DOrg administrative transaction base is stale"
  'dorg-transaction-error)

(define-error 'dorg-transaction-empty
  "DOrg administrative transaction produced no commit"
  'dorg-transaction-error)

(define-error 'dorg-transaction-dirty
  "DOrg administrative transaction found an uncommitted working set"
  'dorg-transaction-error)

(define-error 'dorg-transaction-indeterminate
  "DOrg administrative commit outcome requires reconciliation"
  'dorg-transaction-error)

(defun dorg-transaction-commit
    (dolt-dir base statements message &optional validate-function)
  "Commit STATEMENTS in DOLT-DIR only when in-gate HEAD equals BASE.
MESSAGE is the Dolt commit message.  VALIDATE-FUNCTION, when non-nil,
receives a read-only QUERY function after STATEMENTS execute but before
the commit, or uses the typed proof shape
\(:mode transaction-read-port :extension-tables TABLES :function FN
[:query-function QFN]).  In the typed shape, FN receives the closed typed read
port and optional QFN receives the SELECT-only raw QUERY function during the
same validation pass.  A signal from any validator rolls the complete
transaction back.

The critical section uses `dorg-mutate-call-with-store-gate', so typed
mutations and administrative import serialize on the same canonical
store lock.  Signals `dorg-transaction-stale' before executing any
statement when HEAD differs, and `dorg-transaction-empty' if no new
commit was produced.  Returns the exact new HEAD."
  (unless (and (stringp base) (not (string-empty-p base)))
    (signal 'dorg-transaction-error
            (list "an exact non-empty base revision is required")))
  (unless (and (listp statements) statements
               (cl-every #'stringp statements))
    (signal 'dorg-transaction-error
            (list "a non-empty SQL statement list is required")))
  (unless (and (stringp message) (not (string-empty-p message)))
    (signal 'dorg-transaction-error
            (list "a non-empty commit message is required")))
  (dorg-mutate-call-with-store-gate
   dolt-dir
   (lambda (lease _key)
     (let ((head
            (dorg-sql--lease-scalar
             lease "SELECT HASHOF('HEAD') AS h")))
       (unless (equal head base)
         (signal 'dorg-transaction-stale
                 (list (format "planned base %s is stale; current HEAD is %s"
                               base head)
                       :base base :head head)))
       (when (dorg-sql--lease-exec
              lease "SELECT table_name FROM dolt_status LIMIT 1")
         (signal 'dorg-transaction-dirty
                 (list
                  (format
                   (concat "refusing administrative import in %s: "
                           "the Dolt working set has uncommitted changes")
                   dolt-dir))))
       (let ((new-head
              (condition-case err
                  (dorg-sql--lease-batch
                   lease statements message validate-function)
                (dorg-sql-lease-broken
                 (signal
                  'dorg-transaction-indeterminate
                  (list
                   (concat
                    "administrative commit transport failed; "
                   "inspect the reported commit or current HEAD before retry")
                   :base base
                   :commit (plist-get (cddr err) :commit)
                   :underlying (error-message-string err)))))))
         (unless (and (stringp new-head)
                      (not (string-empty-p new-head))
                      (not (equal new-head base)))
           (signal 'dorg-transaction-empty
                   (list "administrative transaction produced no new commit"
                         :base base)))
         new-head)))))

(provide 'dorg-transaction)
;;; dorg-transaction.el ends here
