;;; dorg-level-offset-store-tests.el --- Level offsets against a real store -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Store-bound controls for parent-relative heading storage: the v7 -> v8
;; migration on the pinned Dolt (success, refusal and forced-validation-failure
;; rollback), historical decoding of a pre-v8 token, the loader's LEVELS modes
;; and its single recursive ancestor walk, the depth-ordered render spine, the
;; doctor's offset check, and skipped-level import/export.  These need the
;; shared sql-server and so run in the MEDIUM gate.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-doctor)
(require 'dorg-render)
(require 'dorg-model-load)
(require 'dorg-mutate)
(require 'dorg-api)
(require 'dorg-test-helpers)

;;; ─── Fixtures ───────────────────────────────────────────────────────────

(defmacro dorg-test-level-offset-store--with-db (&rest body)
  "Run BODY with `dolt-dir' bound to a fresh current-schema test store."
  (declare (indent 0))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       ,@body)))

(defun dorg-test-level-offset-store--import (dolt-dir text)
  "Import Org TEXT into DOLT-DIR with sequential ids (root 1); commit."
  (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql text))
  (dorg-commit dolt-dir "level-offset fixture import"))

(defun dorg-test-level-offset-store--as-v7 (dolt-dir rows)
  "Reshape DOLT-DIR's headings table to schema v7 and insert v7 ROWS.
ROWS are (ID PARENT SORT-HEX ABSOLUTE-LEVEL TITLE) in root 1.  Commits and
returns the v7 HEAD token."
  (dorg-sql-batch
   dolt-dir
   (append
    (list "ALTER TABLE headings ADD COLUMN level INT NOT NULL DEFAULT 0"
          "ALTER TABLE headings DROP COLUMN level_offset"
          "UPDATE dorg_meta SET value = '7' WHERE `key` = 'schema_version'")
    (mapcar (lambda (row)
              (pcase-let ((`(,id ,parent ,sort ,level ,title) row))
                (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (%d, 1, %s, X'%s', %d, FALSE, %s, 0)"
                        id (if parent (number-to-string parent) "NULL")
                        sort level (if title (dorg-sql-quote title) "NULL"))))
            rows)))
  (dorg-commit dolt-dir "level-offset fixture: v7 store")
  (dorg-head-commit dolt-dir))

(defconst dorg-test-level-offset-store--v7-rows
  '((1 nil "01" 0 nil)
    (2 1 "01" 1 "A")
    (3 2 "01" 3 "B")       ; skipped level: offset 2
    (4 3 "01" 4 "C")
    (5 1 "02" 1 "D"))
  "A consistent v7 store with a skipped level.")

(defun dorg-test-level-offset-store--columns (dolt-dir)
  "Return the sorted column names of DOLT-DIR's headings table."
  (sort (mapcar (lambda (row) (downcase (format "%s" (cdar row))))
                (dorg-sql dolt-dir "SELECT column_name FROM information_schema.columns WHERE table_schema = DATABASE() AND table_name = 'headings'"))
        #'string<))

(defun dorg-test-level-offset-store--schema-version (dolt-dir)
  "Return DOLT-DIR's recorded schema_version string."
  (alist-get 'value (car (dorg-sql dolt-dir "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'"))))

(defun dorg-test-level-offset-store--levels (headings)
  "Return (ID LEVEL OFFSET) for HEADINGS, sorted by id."
  (let (out)
    (maphash (lambda (id h)
               (push (list id (dorg-heading-level h) (dorg-heading-level-offset h))
                     out))
             headings)
    (sort out (lambda (a b) (< (car a) (car b))))))

;;; ─── Migration ──────────────────────────────────────────────────────────

(ert-deftest dorg-test-level-offset-dolt-migration-v7-to-v8-parity ()
  "A consistent v7 store migrates to v8 with exact offsets and levels.
The level column is gone, level_offset is present, schema_version is 8, and
the loaded effective levels equal the v7 absolute levels."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--as-v7 dolt-dir dorg-test-level-offset-store--v7-rows)
    (dorg-migrate-schema dolt-dir)
    (should (equal "8" (dorg-test-level-offset-store--schema-version dolt-dir)))
    (let ((cols (dorg-test-level-offset-store--columns dolt-dir)))
      (should (member "level_offset" cols))
      (should-not (member "level" cols)))
    (should (equal (dorg-sql dolt-dir "SELECT id, level_offset FROM headings ORDER BY id")
                   '(((id . 1) (level_offset . 0)) ((id . 2) (level_offset . 1))
                     ((id . 3) (level_offset . 2)) ((id . 4) (level_offset . 1))
                     ((id . 5) (level_offset . 1)))))
    (should (equal (dorg-test-level-offset-store--levels (dorg-model-load-tree dolt-dir 1))
                   '((1 0 0) (2 1 1) (3 3 2) (4 4 1) (5 1 1))))))

(ert-deftest dorg-test-level-offset-dolt-migration-refuses-inconsistent-store ()
  "A v7 store whose fan is not representable is refused before any statement.
HEAD, the column set and schema_version are byte-identical afterwards."
  (dorg-test-level-offset-store--with-db
    (let* ((head (dorg-test-level-offset-store--as-v7
                  dolt-dir '((1 nil "01" 0 nil)
                             (2 1 "01" 1 "A")
                             (3 1 "02" 2 "Deeper later sibling"))))
           (cols (dorg-test-level-offset-store--columns dolt-dir)))
      (should-error (dorg-migrate-schema dolt-dir) :type 'dorg-schema-migration-required)
      (should (equal head (dorg-head-commit dolt-dir)))
      (should (equal cols (dorg-test-level-offset-store--columns dolt-dir)))
      (should (equal "7" (dorg-test-level-offset-store--schema-version dolt-dir))))))

(ert-deftest dorg-test-level-offset-dolt-migration-forced-validation-failure-rolls-back ()
  "A validator failure inside the step rolls back DDL, data and version.
The typed rolled-back error carries reset-confirmed t; HEAD, the column set
and schema_version are byte-identical to before the step."
  (dorg-test-level-offset-store--with-db
    (let* ((head (dorg-test-level-offset-store--as-v7
                  dolt-dir dorg-test-level-offset-store--v7-rows))
           (cols (dorg-test-level-offset-store--columns dolt-dir))
           (validated nil))
      (cl-letf (((symbol-function 'dorg-repo--v8-hierarchy-validator)
                 (lambda (_expect)
                   (lambda (query)
                     ;; The DDL and offset writes are visible in the window...
                     (setq validated
                           (funcall query "SELECT COUNT(*) AS n FROM headings WHERE level_offset > 0"))
                     ;; ...and a failure here must undo all of them.
                     (error "forced v8 validation failure")))))
        (let ((err (should-error (dorg-migrate-schema dolt-dir)
                                 :type 'dorg-sql-transaction-rolled-back)))
          (should (eq t (plist-get (dorg-sql-transaction-rollback-data err)
                                   :reset-confirmed)))))
      (should validated)
      (should (equal head (dorg-head-commit dolt-dir)))
      (should (equal cols (dorg-test-level-offset-store--columns dolt-dir)))
      (should (equal "7" (dorg-test-level-offset-store--schema-version dolt-dir))))))

(ert-deftest dorg-test-level-offset-dolt-migration-render-identity ()
  "The whole tree renders byte-identically before and after the migration.
The pre-migration render reads the v7 absolute levels through the loader's
pre-v8 decode; the post-migration render derives them from offsets."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--as-v7 dolt-dir dorg-test-level-offset-store--v7-rows)
    (let ((before (dorg-model-render-tree
                   (hash-table-values (dorg-model-load-tree dolt-dir 1)))))
      (dorg-migrate-schema dolt-dir)
      (should (string-match-p "^\\*\\*\\* B$" before))
      (should (equal before
                     (dorg-model-render-tree
                      (hash-table-values (dorg-model-load-tree dolt-dir 1))))))))

(ert-deftest dorg-test-level-offset-dolt-as-of-schema7-decodes-absolute ()
  "A pre-v8 token decodes its absolute level column, never as an offset.
After migration, loading AS OF the v7 token yields the same levels and
offsets as the current HEAD."
  (dorg-test-level-offset-store--with-db
    (let ((v7 (dorg-test-level-offset-store--as-v7
               dolt-dir dorg-test-level-offset-store--v7-rows)))
      (dorg-migrate-schema dolt-dir)
      (should (equal (dorg-test-level-offset-store--levels
                      (dorg-model-load-tree dolt-dir 1 v7))
                     (dorg-test-level-offset-store--levels
                      (dorg-model-load-tree dolt-dir 1))))
      ;; A slice at the old token takes the stored absolute level directly.
      (let ((slice (dorg-model-load-headings dolt-dir 1 '(3 4) v7)))
        (should (equal (dorg-heading-level (gethash 3 slice)) 3))
        (should (equal (dorg-heading-level (gethash 4 slice)) 4))
        (should (equal (dorg-heading-level-offset (gethash 4 slice)) 1))))))

(ert-deftest dorg-test-level-offset-dolt-as-of-schema7-singleton-slice-offset ()
  "A pre-v8 singleton slice decodes its offset against an unloaded parent.
Id 3 (absolute level 3 under A at level 1) is loaded alone at the v7 token:
its effective level is 3 and its offset 2 in both LEVELS modes.  Each pre-v8
load costs one failed v8 statement; this test was split from the one above
while the shared test server could still stall on a few logged errors."
  (dorg-test-level-offset-store--with-db
    (let ((v7 (dorg-test-level-offset-store--as-v7
               dolt-dir dorg-test-level-offset-store--v7-rows)))
      (dorg-migrate-schema dolt-dir)
      (let ((slice (dorg-model-load-headings dolt-dir 1 '(3) v7)))
        (should (= 1 (hash-table-count slice)))
        (should (equal (dorg-heading-level (gethash 3 slice)) 3))
        (should (equal (dorg-heading-level-offset (gethash 3 slice)) 2)))
      (let ((slice (dorg-model-load-headings dolt-dir 1 '(3) v7 nil nil 'offsets)))
        (should-not (dorg-heading-level (gethash 3 slice)))
        (should (equal (dorg-heading-level-offset (gethash 3 slice)) 2))))))

(ert-deftest dorg-test-level-offset-dolt-schema7-parentless-row-not-root ()
  "A parentless v7 row above level 0 keeps its absolute level as its offset.
Decoding it as offset 0 would give a corrupt row the synthetic root's shape."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--as-v7
     dolt-dir (append dorg-test-level-offset-store--v7-rows '((9 nil "09" 2 "stray"))))
    (let ((stray (gethash 9 (dorg-model-load-headings dolt-dir 1 '(9) nil nil nil 'offsets))))
      (should (equal (dorg-heading-level-offset stray) 2))
      (should-not (dorg-model-root-p stray)))))

;;; ─── Loader ─────────────────────────────────────────────────────────────

(defconst dorg-test-level-offset-store--text
  "* A\n*** B\n**** C\n* D\n"
  "Import fixture: ids root 1, A 2, B 3 (offset 2), C 4, D 5.")

(ert-deftest dorg-test-level-offset-slice-load-derives-effective-level ()
  "An id slice without its ancestors gets effective levels from one walk."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (let ((slice (dorg-model-load-headings dolt-dir 1 '(4))))
      (should (= 1 (hash-table-count slice)))
      (should (equal (dorg-heading-level (gethash 4 slice)) 4))
      (should (equal (dorg-heading-level-offset (gethash 4 slice)) 1)))))

(ert-deftest dorg-test-level-offset-load-api-default-effective-on-slice ()
  "The default LEVELS on an id slice is effective, through the recursive walk."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (let (first)
      (let ((slice (dorg-model-load-headings
                    dolt-dir 1 '(3) nil nil
                    (lambda (sql)
                      (unless first (setq first sql))
                      (dorg-sql dolt-dir sql)))))
        (should (string-prefix-p "WITH RECURSIVE walk" first))
        (should (equal (dorg-heading-level (gethash 3 slice)) 3))))))

(ert-deftest dorg-test-level-offset-load-api-offsets-mode-leaves-level-nil ()
  "`offsets' mode reads the persisted offset and derives no level."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (let ((slice (dorg-model-load-headings dolt-dir 1 '(3 4) nil nil nil 'offsets))
          (whole (dorg-model-load-headings dolt-dir 1 nil nil nil nil 'offsets)))
      (should-not (dorg-heading-level (gethash 3 slice)))
      (should (equal (dorg-heading-level-offset (gethash 3 slice)) 2))
      ;; A whole-root offsets load derives nothing either.
      (should (= 5 (hash-table-count whole)))
      (should-not (cl-some (lambda (h) (dorg-heading-level h))
                           (hash-table-values whole))))))

(ert-deftest dorg-test-level-offset-slice-load-corrupt-ancestry-no-fabrication ()
  "A slice whose chain never reaches a root gets no level, and terminates.
The two rows form a parent cycle; the bounded walk must stop and answer nil."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (dorg-sql-batch
     dolt-dir
     (list "SET FOREIGN_KEY_CHECKS = 0"
           "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9001, 1, 9002, X'F1', 1, FALSE, 'cyc1', 0)"
           "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (9002, 1, 9001, X'F2', 1, FALSE, 'cyc2', 0)"
           "SET FOREIGN_KEY_CHECKS = 1"))
    (dorg-commit dolt-dir "corrupt cycle")
    (let ((slice (dorg-model-load-headings dolt-dir 1 '(9001 4))))
      (should-not (dorg-heading-level (gethash 9001 slice)))
      (should (equal (dorg-heading-level (gethash 4 slice)) 4)))))

(defun dorg-test-level-offset-store--insert-orphan (dolt-dir id offset)
  "Insert a parentless non-root heading ID with OFFSET into DOLT-DIR's root 1."
  (dorg-sql dolt-dir
            (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (%d, 1, NULL, X'F%d', %d, FALSE, 'orphan', 0)"
                    id offset offset))
  (dorg-commit dolt-dir "corrupt orphan"))

(ert-deftest dorg-test-level-offset-slice-load-parentless-orphan-no-level ()
  "A parentless row that is not the named root reaches no root.
Neither a positive offset (the level it would fabricate) nor offset 0 (the
root's own shape under another id) makes it reached."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (dorg-test-level-offset-store--insert-orphan dolt-dir 9001 2)
    (dorg-test-level-offset-store--insert-orphan dolt-dir 9002 0)
    (let ((slice (dorg-model-load-headings dolt-dir 1 '(9001 9002 4))))
      (should-not (dorg-heading-level (gethash 9001 slice)))
      (should-not (dorg-heading-level (gethash 9002 slice)))
      (should (equal (dorg-heading-level (gethash 4 slice)) 4)))))

(ert-deftest dorg-test-level-offset-slice-load-malformed-root-no-level ()
  "A root row with a nonzero offset is not in root shape, so nothing reaches it."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (dorg-sql dolt-dir "UPDATE headings SET level_offset = 3 WHERE id = 1")
    (dorg-commit dolt-dir "malformed root")
    (let ((slice (dorg-model-load-headings dolt-dir 1 '(4 1))))
      (should-not (dorg-heading-level (gethash 4 slice)))
      (should-not (dorg-heading-level (gethash 1 slice))))))

(ert-deftest dorg-test-level-offset-slice-load-nonpositive-offset-no-level ()
  "A slice path through a zero or negative non-root offset derives no level.
B (id 3) is corrupted to each bad offset: B and its child C lose their
level, exactly as a whole-tree derivation would, while D is unaffected."
  (dolist (bad '(0 -2))
    (ert-info ((format "offset %d" bad))
      (dorg-test-level-offset-store--with-db
        (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
        (dorg-sql dolt-dir (format "UPDATE headings SET level_offset = %d WHERE id = 3" bad))
        (dorg-commit dolt-dir "corrupt offset")
        (let ((slice (dorg-model-load-headings dolt-dir 1 '(3 4 5))))
          (should-not (dorg-heading-level (gethash 3 slice)))
          (should-not (dorg-heading-level (gethash 4 slice)))
          (should (equal (dorg-heading-level (gethash 5 slice)) 1)))
        (should-not (dorg-heading-level
                     (gethash 4 (dorg-model-load-tree dolt-dir 1))))))))

(ert-deftest dorg-test-level-offset-dolt-move-refuses-corrupt-descendant ()
  "A move whose subtree carries a zero offset is refused before any write.
Moving A (with corrupt B below it) under D must neither resurrect a level
for B and C nor commit over the invalid offset."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves
                dolt-dir "* A\n** B\n*** C\n* D\n")))
      (dorg-sql dolt-dir "UPDATE headings SET level_offset = 0 WHERE id = 102")
      (dorg-commit dolt-dir "corrupt B")
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error
                   (dorg-test-level-offset-store--apply
                    src '(on (id 101) (move (subtree) (under (id 104)))))
                   :type 'dorg-mutate-error)))
        (should (eq 'invalid-level-offset
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (eql 100 (alist-get 'parent_id (car (dorg-sql dolt-dir "SELECT parent_id FROM headings WHERE id = 101")))))))))

(ert-deftest dorg-test-level-offset-whole-load-malformed-root-no-levels ()
  "A whole-root load under a malformed named root fabricates no level.
The root row is corrupted to offset 3, then separately given a parent; in
both cases `dorg-model-load-tree' leaves every heading's level nil, exactly
as the sliced loader answers, instead of seeding the root at 0."
  (dolist (corruption '("UPDATE headings SET level_offset = 3 WHERE id = 1"
                        "UPDATE headings SET parent_id = 5 WHERE id = 1"))
    (ert-info (corruption)
      (dorg-test-level-offset-store--with-db
        (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
        (dorg-sql dolt-dir corruption)
        (dorg-commit dolt-dir "malformed root")
        (let ((tree (dorg-model-load-tree dolt-dir 1)))
          (should (= 5 (hash-table-count tree)))
          (should-not (cl-some #'dorg-heading-level (hash-table-values tree))))))))

;;; ─── Render, doctor, import/export ──────────────────────────────────────

(ert-deftest dorg-test-level-offset-spine-orders-by-depth ()
  "Ancestor ids come root to parent even when offsets disagree with depth."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir "*** A\n**** B\n***** C\n")
    ;; A is offset 3, B and C offset 1: offset order is not depth order.
    (should (equal (dorg-render--ancestor-ids dolt-dir 1 4) '(1 2 3)))))

(ert-deftest dorg-test-level-offset-doctor-flags-nonpositive-offset ()
  "The doctor reports a non-root heading whose offset is not positive."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir dorg-test-level-offset-store--text)
    (dorg-sql dolt-dir "UPDATE headings SET level_offset = 0 WHERE id = 4")
    (dorg-commit dolt-dir "break offset")
    (let ((found (cl-remove-if-not
                  (lambda (f) (equal (plist-get f :check) "nonpositive-level-offset"))
                  (dorg-doctor dolt-dir))))
      (should (= 1 (length found)))
      (should (string-match-p "Heading 4" (plist-get (car found) :message))))))

(ert-deftest dorg-test-level-offset-dolt-skipped-level-import-export ()
  "Skipped levels survive import and a whole-tree render from the store."
  (dorg-test-level-offset-store--with-db
    (dorg-test-level-offset-store--import dolt-dir "* A\n*** B\n**** C\n** D\n")
    (should (equal (mapcar (lambda (r) (alist-get 'level_offset r))
                           (dorg-sql dolt-dir "SELECT level_offset FROM headings ORDER BY id"))
                   '(0 1 2 1 1)))
    (let ((text (dorg-model-render-tree
                 (hash-table-values (dorg-model-load-tree dolt-dir 1)))))
      (should (equal (mapcar (lambda (line) (car (split-string line " ")))
                             (seq-filter (lambda (line) (string-match-p "\\`\\*+ " line))
                                         (split-string text "\n")))
                     '("*" "***" "****" "**"))))))

;;; ─── Moves through the public API against a store ──────────────────────

(defconst dorg-test-level-offset-store--move-text
  "* A\n**** X\n****** Xc\n* B\n"
  "Move fixture (root 100): A 101, X 102 at offset 3 under A, Xc 103 at
offset 2 under X, B 104.")

(defun dorg-test-level-offset-store--seed-moves (dolt-dir &optional text)
  "Seed TEXT (default the move fixture) under root 100; return the source."
  (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql
                            (or text dorg-test-level-offset-store--move-text) 100))
  (dorg-commit dolt-dir "level-offset move fixture")
  (list :dolt-dir dolt-dir :root-id 100))

(defun dorg-test-level-offset-store--apply (src form)
  "Apply public FORM on SRC and return the response."
  (dorg-api-execute form :mode 'apply :source src))

(defun dorg-test-level-offset-store--offset (dolt-dir id)
  "Return heading ID's stored level offset."
  (alist-get 'level_offset
             (car (dorg-sql dolt-dir (format "SELECT level_offset FROM headings WHERE id = %d" id)))))

(ert-deftest dorg-test-level-offset-dolt-move-omitted-resets-old-gap ()
  "Through the public API an omitted offset stores 1 for the moved root."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir)))
      (should (eql 3 (dorg-test-level-offset-store--offset dolt-dir 102)))
      (should (plist-get (dorg-test-level-offset-store--apply
                          src '(on (id 102) (move (subtree) (under (id 104)))))
                         :ok))
      (should (eql 1 (dorg-test-level-offset-store--offset dolt-dir 102)))
      (should (eql 2 (dorg-test-level-offset-store--offset dolt-dir 103))))))

(ert-deftest dorg-test-level-offset-dolt-move-explicit-offset-honored ()
  "Through the public API an explicit (offset N) is stored as given."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir)))
      (should (plist-get (dorg-test-level-offset-store--apply
                          src '(on (id 102) (move (subtree) (under (id 104) (offset 2)))))
                         :ok))
      (should (eql 2 (dorg-test-level-offset-store--offset dolt-dir 102)))
      (should (eql 2 (dorg-test-level-offset-store--offset dolt-dir 103))))))

(ert-deftest dorg-test-level-offset-dolt-same-parent-move-resets ()
  "A move within the same parent also resets an omitted offset to 1."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir)))
      (should (plist-get (dorg-test-level-offset-store--apply
                          src '(on (id 102) (move (subtree) (under (id 101)))))
                         :ok))
      (should (eql 101 (alist-get 'parent_id (car (dorg-sql dolt-dir "SELECT parent_id FROM headings WHERE id = 102")))))
      (should (eql 1 (dorg-test-level-offset-store--offset dolt-dir 102))))))

(ert-deftest dorg-test-level-offset-dolt-move-writes-no-descendant-offset ()
  "A move writes the moved root, never a descendant's offset or row."
  (dorg-test-level-offset-store--with-db
    (let* ((src (dorg-test-level-offset-store--seed-moves dolt-dir))
           (sim (dorg-mutate--dolt-simulate
                 (dorg-mutate--resolve-source src)
                 (dorg-mutate--validate-patches
                  (list (list :op 'move :target '(:subtree :id 102)
                              :to '(:under (:heading :id 104)))))
                 nil)))
      (should-not (cl-some (lambda (sql) (string-match-p "WHERE id = 103\\'" sql))
                           (plist-get sim :sql)))
      (should (equal (plist-get sim :changed) '(102))))))

(ert-deftest dorg-test-level-offset-dolt-move-reload-parity ()
  "Levels reloaded after a committed move equal the candidate's derived levels."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir)))
      (dorg-test-level-offset-store--apply
       src '(on (id 102) (move (subtree) (under (id 104)))))
      (let ((tree (dorg-model-load-tree dolt-dir 100)))
        (should (eql 2 (dorg-heading-level (gethash 102 tree))))
        (should (eql 4 (dorg-heading-level (gethash 103 tree))))))))

(ert-deftest dorg-test-level-offset-dolt-render-after-move ()
  "The store renders the moved subtree at its new depth with its inner gap."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir)))
      (dorg-test-level-offset-store--apply
       src '(on (id 102) (move (subtree) (under (id 104)))))
      (let ((text (dorg-model-render-tree
                   (hash-table-values (dorg-model-load-tree dolt-dir 100)))))
        (should (string-match-p "^\\*\\* X$" text))
        (should (string-match-p "^\\*\\*\\*\\* Xc$" text))))))

(ert-deftest dorg-test-level-offset-dolt-nonrepresentable-leaves-head ()
  "A refused non-representable move leaves HEAD and every row unchanged."
  (dorg-test-level-offset-store--with-db
    (let* ((src (dorg-test-level-offset-store--seed-moves
                 dolt-dir "* A\n** X\n* B\n*** G\n"))
           (head (dorg-head-commit dolt-dir))
           (err (should-error
                 (dorg-test-level-offset-store--apply
                  src '(on (id 102) (move (subtree) (under (id 103) (before (id 104))))))
                 :type 'dorg-mutate-error)))
      (should (eq 'nonrepresentable-sibling-order
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal head (dorg-head-commit dolt-dir)))
      (should (eql 101 (alist-get 'parent_id (car (dorg-sql dolt-dir "SELECT parent_id FROM headings WHERE id = 102"))))))))

(ert-deftest dorg-test-level-offset-dolt-cycle-still-refused ()
  "GUARD: moving a heading under its own descendant is still refused."
  (dorg-test-level-offset-store--with-db
    (let* ((src (dorg-test-level-offset-store--seed-moves dolt-dir))
           (head (dorg-head-commit dolt-dir)))
      (should-error (dorg-api-execute '(on (id 101) (move (subtree) (under (id 102))))
                                      :mode 'apply :source src)
                    :type 'dorg-mutate-error)
      (should (equal head (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-level-offset-same-root-sim-returns-moved-closure ()
  "A same-root move reports its whole moved closure; only the root is changed."
  (dorg-test-level-offset-store--with-db
    (let* ((src (dorg-test-level-offset-store--seed-moves dolt-dir))
           (sim (dorg-mutate--dolt-simulate
                 (dorg-mutate--resolve-source src)
                 (dorg-mutate--validate-patches
                  (list (list :op 'move :target '(:subtree :id 102)
                              :to '(:under (:heading :id 104)))))
                 nil)))
      (should (equal (plist-get sim :moved) '(102 103)))
      (should (equal (plist-get sim :changed) '(102))))))

(ert-deftest dorg-test-level-offset-heading-delete-records-promoted-closure ()
  "Heading-only delete records each promoted child's subtree as moved."
  (dorg-test-level-offset-store--with-db
    (let* ((src (dorg-test-level-offset-store--seed-moves dolt-dir))
           (sim (dorg-mutate--dolt-simulate
                 (dorg-mutate--resolve-source src)
                 (dorg-mutate--validate-patches
                  (list (list :op 'delete :target '(:heading :id 101))))
                 nil)))
      (should (equal (plist-get sim :moved) '(102 103))))))

(ert-deftest dorg-test-level-offset-dolt-heading-delete-promotes-with-removed-offset ()
  "A promoted child takes the removed heading's offset; its child keeps its own."
  (dorg-test-level-offset-store--with-db
    (let ((src (dorg-test-level-offset-store--seed-moves dolt-dir "* A\n*** H\n**** K\n")))
      ;; A 101, H 102 (offset 2 under A), K 103 (offset 1 under H).
      (should (plist-get (dorg-test-level-offset-store--apply
                          src '(on (id 102) (delete (heading))))
                         :ok))
      (should (eql 2 (dorg-test-level-offset-store--offset dolt-dir 103)))
      (should (eql 3 (dorg-heading-level (gethash 103 (dorg-model-load-tree dolt-dir 100))))))))

(provide 'dorg-level-offset-store-tests)
;;; dorg-level-offset-store-tests.el ends here
