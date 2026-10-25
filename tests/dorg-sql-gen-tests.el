;;; dorg-sql-gen-tests.el --- Tests for dorg-sql-gen -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dedicated tests for the SQL generation module.  Created by tracked-regression;
;; until the relocation lands this file carries the Family D relocation
;; contract guard, and the lowering tests move here with the implementation.
;;
;; The guard assembles the retired names from fragments so a post-relocation
;; repository grep for the full old names is zero-hit, including this file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-sortkey)
(require 'dorg-sql-gen)

(defconst dorg-sql-gen-tests--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-sql-gen-tests--name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-sql-gen-tests--repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-sql-gen-relocation-contract ()
  "tracked-regression Family D: the lowering cluster lives in dorg-sql-gen.el.
RED at SPEC (new names undefined, retired names still defined and
referenced); GREEN after IMPL (lowering under the sql-gen prefix, the
sync-prefixed originals deleted repository-wide)."
  (let ((pairs '(("-lower-core-ops"      . dorg-sql-gen-lower-heading-ops)
                 ("-lower-item-plan"     . dorg-sql-gen--lower-item-plan)
                 ("-item-plan-possible-p" . dorg-sql-gen--item-plan-possible-p)
                 ("-sort-insert-groups"  . dorg-sql-gen--sort-insert-groups)))
        (old-prefix (concat "dorg-sync" "-")))
    (dolist (p pairs)
      (let ((old (intern-soft (concat old-prefix (car p)))))
        (should (fboundp (cdr p)))
        (should-not (and old (fboundp old)))))
    (should-not (dorg-sql-gen-tests--name-hits
                 (mapcar (lambda (p) (concat old-prefix (car p))) pairs)))))

;;; ─── Direct lowerer tests (moved by tracked-regression) ─────────────────────

(ert-deftest dorg-test-sql-gen-lower-item-plan-no-whole-heading-delete ()
  "Changed-heading lowering with loadable item ids emits no whole-heading
item delete (AC11).  When the DB heading carries item objects with ids, an
item-level plan replaces the DELETE FROM heading_items WHERE heading_id."
  (let* ((db-h (make-dorg-heading
                :id 1 :level 1 :level-offset 1 :title "H"
                :items (list (dorg-text-item :id 100 :sort-key (dorg-sortkey-from-integer 1)
                                             :text "A")
                             (dorg-text-item :id 101 :sort-key (dorg-sortkey-from-integer 2)
                                             :text "B"))
                :items-hash "OLDHASH"))
         (new-h (make-dorg-heading
                 :id 1 :level 1 :level-offset 1 :title "H"
                 :items (list (dorg-text-item :text "A")
                              (dorg-text-item :text "B-edited"))
                 :items-hash "NEWHASH"))
         (db-headings (make-hash-table :test 'eql))
         (counter 5000)
         (id-gen (lambda () (cl-incf counter))))
    (puthash 1 db-h db-headings)
    (let* ((stmts (dorg-sql-gen-lower-heading-ops
                   (list (list :update-heading new-h)) db-headings id-gen))
           (sql (mapconcat #'identity stmts "\n")))
      (should-not (string-match-p "DELETE FROM heading_items WHERE heading_id" sql))
      ;; The reused item id 101 receives a same-kind text update.
      (should (string-match-p "UPDATE text_items SET text_raw = .*WHERE item_id = 101" sql)))))

(ert-deftest dorg-test-sql-gen-lower-item-plan-fallback-when-no-ids ()
  "Lowering falls back to whole-heading delete when DB items are absent (AC11).
A legacy DB heading with nil items cannot be planned, so the safe
delete-all/reinsert path is kept."
  (let* ((db-h (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H"
                                  :items nil :items-hash "OLDHASH"))
         (new-h (make-dorg-heading
                 :id 1 :level 1 :level-offset 1 :title "H"
                 :items (list (dorg-text-item :text "A"))
                 :items-hash "NEWHASH"))
         (db-headings (make-hash-table :test 'eql))
         (counter 5000)
         (id-gen (lambda () (cl-incf counter))))
    (puthash 1 db-h db-headings)
    (let* ((stmts (dorg-sql-gen-lower-heading-ops
                   (list (list :update-heading new-h)) db-headings id-gen))
           (sql (mapconcat #'identity stmts "\n")))
      (should (string-match-p "DELETE FROM heading_items WHERE heading_id = 1" sql)))))

(ert-deftest dorg-test-sql-gen-heading-items-delegates-fresh-keys-to-model-plan ()
  "Fresh full-heading item inserts get sort keys from the model ordering owner."
  (let ((calls 0)
        (heading (make-dorg-heading
                  :id 77 :level 1 :level-offset 1 :title "H"
                  :items (list (dorg-text-item :text "A")
                               (dorg-text-item :text "B"))))
        (next-id 9000))
    (advice-add
     'dorg-model-plan-items
     :around
     (lambda (orig old-items new-items)
       (cl-incf calls)
       (should-not old-items)
       (should (= 2 (length new-items)))
       (funcall orig old-items new-items))
     '((name . dorg-test-sql-gen--fresh-item-plan-spy)))
    (unwind-protect
        (let ((sql (dorg-sql-gen-heading-items-sql
                    heading (lambda () (prog1 next-id (cl-incf next-id))))))
          (should (= 1 calls))
          (should (= 2 (cl-count-if
                        (lambda (stmt)
                          (string-match-p "INSERT INTO heading_items" stmt))
                        sql))))
      (advice-remove 'dorg-model-plan-items
                     'dorg-test-sql-gen--fresh-item-plan-spy))))

(ert-deftest dorg-test-sql-gen-lower-item-plan-append-after-aged-keys-rekeys-nothing ()
  "Appending after aged fixed-width boundary keys touches no reused row.
The fixture uses 13 ordered keys: be16(42), then be16(42)||be4(1..12).  The
new item gets a key after the shared prefix; every reused item keeps its key,
so the lowering carries no sort_key UPDATE and no temporary key."
  (let* ((base-key (dorg-sortkey-from-integer 42 dorg-sortkey-boundary-width))
         (keys (cons base-key
                     (cl-loop for n from 1 to 12
                              collect (concat base-key
                                              (dorg-sortkey-from-integer n 4)))))
         (rows (cl-loop for key in keys
                        for id from 42
                        collect (dorg-text-item :id id
                                                :sort-key key
                                                :text (format "item-%d" id))))
         (requested (append (mapcar (lambda (row)
                                      (dorg-text-item
                                       :text (dorg-text-item-text row)))
                                    rows)
                            (list (dorg-text-item :text "appended"))))
         (old-h (make-dorg-heading :id 100 :items rows))
         (new-h (make-dorg-heading :id 100 :items requested))
         (next-id 55)
         (sql (dorg-sql-gen--lower-item-plan
               new-h old-h (lambda () (prog1 next-id (cl-incf next-id))))))
    (should (dorg-sortkey-ordered-p keys))
    (should-not (cl-some (lambda (stmt)
                           (string-match-p "UPDATE heading_items SET sort_key" stmt))
                         sql))
    (should (= 1 (cl-count-if (lambda (stmt)
                                (string-match-p "INSERT INTO heading_items" stmt))
                              sql)))
    ;; The appended key sorts after the last aged key.
    (let ((insert (cl-find-if (lambda (stmt)
                                (string-match-p "INSERT INTO heading_items" stmt))
                              sql)))
      (should (string-match "X'\\([0-9a-f]+\\)'" insert))
      (should (string< (car (last keys))
                       (dorg-sortkey-from-hex (match-string 1 insert)))))))

(ert-deftest dorg-test-sql-gen-item-reorder-keys-are-disjoint-from-old-keys ()
  "W7: a swap rewrites keys that equal none of the old keys, in any order.
No statement of the lowering can therefore collide on the item unique index,
whatever order the engine applies rows in, and nothing is staged."
  (let* ((k1 (dorg-sortkey-from-integer 1))
         (k2 (dorg-sortkey-from-integer 2))
         (rows (list (dorg-text-item :id 1 :sort-key k1 :text "A")
                     (dorg-text-item :id 2 :sort-key k2 :text "B")))
         (old-h (make-dorg-heading :id 100 :items rows))
         (new-h (make-dorg-heading :id 100 :items (list (dorg-text-item :text "B")
                                                        (dorg-text-item :text "A"))))
         (sql (dorg-sql-gen--lower-item-plan new-h old-h (lambda () 999)))
         (updates (cl-remove-if-not
                   (lambda (stmt) (string-match-p "UPDATE heading_items SET sort_key" stmt))
                   sql)))
    (should (= 2 (length updates)))
    (dolist (stmt updates)
      (should (string-match "X'\\([0-9a-f]+\\)'" stmt))
      (let ((key (dorg-sortkey-from-hex (match-string 1 stmt))))
        (should-not (member key (list k1 k2)))
        (should (= (length key) 16))))))

(ert-deftest dorg-test-sql-gen-rekey-sql-is-one-statement ()
  "A rekey of several rows is one VALUES-join UPDATE naming every id once."
  (let* ((pairs (list (cons 7 (dorg-sortkey-encode 1000))
                      (cons 9 (dorg-sortkey-encode 2000))))
         (stmt (dorg-sql-gen-rekey-sql "headings" pairs)))
    (should (equal stmt
                   (concat "UPDATE headings h JOIN (VALUES "
                           "ROW(7, X'000000000000000000000000000003e8'), "
                           "ROW(9, X'000000000000000000000000000007d0')"
                           ") AS v (id, k) ON v.id = h.id SET h.sort_key = v.k")))
    (should (equal (dorg-sql-gen-lower-heading-ops
                    (list (list :rekey 1 pairs)) (make-hash-table) (lambda () 1))
                   (list stmt)))
    (should (null (dorg-sql-gen-lower-heading-ops
                   (list (list :rekey 1 nil)) (make-hash-table) (lambda () 1))))))

(provide 'dorg-sql-gen-tests)
;;; dorg-sql-gen-tests.el ends here

;;; ─── Item identity binding for unchanged replacements ──────────────────
;; Witnesses through the actual lowerer (`dorg-sql-gen-lower-heading-ops'):
;; a whole-items replacement whose content equals the persisted items must
;; leave the candidate items bound to their persisted ids and keys with zero
;; item-table SQL and zero id allocation, so the in-transaction partition
;; proof (item-row identity) can hold.  Kill: restore the old
;; `dorg-model-items-changed-p' outer guard and the first witness goes RED.

(defun dorg-sql-gen-tests--6x6b6-db-heading (texts &optional ids)
  "Return a DB-bound heading with TEXTS as text items carrying IDS and keys."
  (let* ((ids (or ids (cl-loop for i from 100 below (+ 100 (length texts)) collect i)))
         (items (cl-loop for text in texts for id in ids for k from 1
                         collect (dorg-text-item :id id
                                                 :sort-key (dorg-sortkey-from-integer k)
                                                 :text text)))
         (h (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H" :items items)))
    (dorg-heading-compute-items-hash h)
    h))

(defun dorg-sql-gen-tests--6x6b6-new-heading (texts)
  "Return a candidate heading with TEXTS as identity-less text items."
  (let ((h (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H"
                              :items (mapcar (lambda (text) (dorg-text-item :text text))
                                             texts))))
    (dorg-heading-compute-items-hash h)
    h))

(defun dorg-sql-gen-tests--6x6b6-lower (db-h new-h)
  "Lower one :update-heading of NEW-H over DB-H; return (STMTS . ID-GEN-CALLS)."
  (let ((db-headings (make-hash-table :test 'eql))
        (calls 0))
    (puthash 1 db-h db-headings)
    (cons (dorg-sql-gen-lower-heading-ops
           (list (list :update-heading new-h)) db-headings
           (lambda () (cl-incf calls) (+ 5000 calls)))
          calls)))

(defun dorg-sql-gen-tests--6x6b6-item-sql (stmts)
  "Return the statements in STMTS that touch item tables."
  (cl-remove-if-not (lambda (s) (string-match-p "heading_items\\|text_items" s)) stmts))

(ert-deftest dorg-test-sql-gen-6x6b6-unchanged-replacement-binds-identity-with-zero-item-sql ()
  "Equal-content replacement: candidate items acquire the persisted ids and
keys, no item-table statement is emitted, and no id is allocated."
  (let* ((db-h (dorg-sql-gen-tests--6x6b6-db-heading '("A" "B")))
         (new-h (dorg-sql-gen-tests--6x6b6-new-heading '("A" "B")))
         (res (dorg-sql-gen-tests--6x6b6-lower db-h new-h)))
    (should-not (dorg-model-items-changed-p new-h db-h))
    (should (null (dorg-sql-gen-tests--6x6b6-item-sql (car res))))
    (should (= 0 (cdr res)))
    (should (equal (mapcar #'dorg-item-id (dorg-heading-items new-h)) '(100 101)))
    (should (equal (mapcar #'dorg-item-sort-key (dorg-heading-items new-h))
                   (mapcar #'dorg-item-sort-key (dorg-heading-items db-h))))))

(ert-deftest dorg-test-sql-gen-6x6b6-changed-content-updates-and-preserves-identity ()
  "Changed content still lowers to a same-kind detail UPDATE on the reused
row; the candidate item is bound to that id and no id is allocated."
  (let* ((db-h (dorg-sql-gen-tests--6x6b6-db-heading '("A" "B")))
         (new-h (dorg-sql-gen-tests--6x6b6-new-heading '("A" "B-edited")))
         (res (dorg-sql-gen-tests--6x6b6-lower db-h new-h))
         (sql (mapconcat #'identity (car res) "\n")))
    (should (dorg-model-items-changed-p new-h db-h))
    (should (string-match-p "UPDATE text_items SET text_raw = .*WHERE item_id = 101" sql))
    (should-not (string-match-p "DELETE FROM heading_items WHERE heading_id" sql))
    (should (= 0 (cdr res)))
    (should (equal (mapcar #'dorg-item-id (dorg-heading-items new-h)) '(100 101)))))

(ert-deftest dorg-test-sql-gen-6x6b6-original-snapshot-is-immutable ()
  "Lowering binds identities onto the candidate only; the DB snapshot's
items keep their ids, keys and text."
  (let* ((db-h (dorg-sql-gen-tests--6x6b6-db-heading '("A" "B")))
         (before (mapcar (lambda (it) (list (dorg-item-id it) (dorg-item-sort-key it)
                                            (dorg-text-item-text it)))
                         (dorg-heading-items db-h)))
         (new-h (dorg-sql-gen-tests--6x6b6-new-heading '("A" "B"))))
    (dorg-sql-gen-tests--6x6b6-lower db-h new-h)
    (should (equal before
                   (mapcar (lambda (it) (list (dorg-item-id it) (dorg-item-sort-key it)
                                              (dorg-text-item-text it)))
                           (dorg-heading-items db-h))))))

(ert-deftest dorg-test-sql-gen-6x6b6-duplicate-items-bind-positionally-and-reorder-keeps-rows ()
  "Duplicate equal items bind to distinct persisted rows in document order
with zero SQL; a pure reorder reuses every row (no detail UPDATE, no id
allocation) and rekeys only."
  (let* ((db-h (dorg-sql-gen-tests--6x6b6-db-heading '("A" "A" "B") '(100 101 102)))
         (same (dorg-sql-gen-tests--6x6b6-new-heading '("A" "A" "B")))
         (res-same (dorg-sql-gen-tests--6x6b6-lower db-h same)))
    (should (null (dorg-sql-gen-tests--6x6b6-item-sql (car res-same))))
    (should (= 0 (cdr res-same)))
    (should (equal (mapcar #'dorg-item-id (dorg-heading-items same)) '(100 101 102)))
    (let* ((db-h2 (dorg-sql-gen-tests--6x6b6-db-heading '("A" "A" "B") '(100 101 102)))
           (reordered (dorg-sql-gen-tests--6x6b6-new-heading '("B" "A" "A")))
           (res (dorg-sql-gen-tests--6x6b6-lower db-h2 reordered))
           (sql (mapconcat #'identity (car res) "\n")))
      (should-not (string-match-p "text_items" sql))
      (should-not (string-match-p "DELETE FROM heading_items" sql))
      (should (= 0 (cdr res)))
      (should (equal (sort (mapcar #'dorg-item-id (dorg-heading-items reordered)) #'<)
                     '(100 101 102))))))

(ert-deftest dorg-test-sql-gen-6x6b6-empty-items-unchanged-emits-nothing ()
  "A heading with no items replaced by no items lowers with zero item SQL
and no allocation; the unplannable-empty path never reinserts."
  (let* ((db-h (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H" :items nil))
         (new-h (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H" :items nil))
         (res (dorg-sql-gen-tests--6x6b6-lower db-h new-h)))
    (should (null (dorg-sql-gen-tests--6x6b6-item-sql (car res))))
    (should (= 0 (cdr res)))))

(ert-deftest dorg-test-sql-gen-6x6b6-legacy-unplannable-unchanged-control ()
  "A legacy DB heading whose items lack ids cannot be planned; an equal-content
replacement must keep the changed-only fallback silent (zero item SQL, no
delete/reinsert churn), while changed content still takes the fallback."
  (let* ((legacy (let ((h (make-dorg-heading
                           :id 1 :level 1 :level-offset 1 :title "H"
                           :items (list (dorg-text-item :text "A")
                                        (dorg-text-item :text "B")))))
                   (dorg-heading-compute-items-hash h) h))
         (same (dorg-sql-gen-tests--6x6b6-new-heading '("A" "B")))
         (res-same (dorg-sql-gen-tests--6x6b6-lower legacy same))
         (changed (dorg-sql-gen-tests--6x6b6-new-heading '("A" "C")))
         (res-changed (dorg-sql-gen-tests--6x6b6-lower legacy changed))
         (sql-changed (mapconcat #'identity (car res-changed) "\n")))
    (should-not (dorg-sql-gen--item-plan-possible-p legacy))
    (should (null (dorg-sql-gen-tests--6x6b6-item-sql (car res-same))))
    (should (= 0 (cdr res-same)))
    (should (string-match-p "DELETE FROM heading_items WHERE heading_id = 1" sql-changed))))

;;; ─── Multi-row insert lowering ───────────────────────────────────────────
;;
;; Typed rows lower to SQL in two separated steps.  The lowering merges only
;; ADJACENT single-row inserts of one shape (table plus exact column list),
;; preserving row order, under a 500-row and a 1 MiB UTF-8 wire-byte cap; every
;; other statement is an unmoved barrier.  Producers choose a groupable order
;; under their own foreign-key knowledge.  The independent oracle is the frozen
;; base capture in tests/fixtures/insert-batching/.

(defconst dorg-sql-gen-tests--fixture-dir
  (expand-file-name "fixtures/insert-batching/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The insert-batching base oracle and its reference model.")

(require 'dorg-insert-batching-capture
         (expand-file-name "capture" dorg-sql-gen-tests--fixture-dir))
(require 'dorg-insert-batching-reference
         (expand-file-name "reference" dorg-sql-gen-tests--fixture-dir))

(defun dorg-sql-gen-tests--rows (n &optional table)
  "Return N one-column rows of TABLE (default \"t\") holding 1..N."
  (cl-loop for i from 1 to n
           collect (dorg-sql-row-make (or table "t") '(a) (list i))))

(defun dorg-sql-gen-tests--row-counts (statements)
  "Return the number of VALUES tuples in each of STATEMENTS, in order.
Every tuple here is a single integer, so the tuples are counted by their
opening parentheses after VALUES."
  (mapcar (lambda (sql)
            (let ((values (substring sql (string-match " VALUES " sql))))
              (cl-count ?\( values)))
          statements))

(ert-deftest dorg-test-sql-lower-row-cap-splits-runs ()
  "Adjacent same-shape rows merge in order, at most 500 per statement."
  (should (equal 500 dorg-sql-lower-max-rows))
  (dolist (case '((499 . (499)) (500 . (500)) (501 . (500 1))
                  (1001 . (500 500 1))))
    (let ((lowered (dorg-sql-lower-statements
                    (dorg-sql-gen-tests--rows (car case)))))
      (should (equal (cdr case) (dorg-sql-gen-tests--row-counts lowered)))
      ;; Row order inside and across statements is the input order.
      (should (equal (number-sequence 1 (car case))
                     (mapcar #'string-to-number
                             (split-string
                              (mapconcat
                               (lambda (sql)
                                 (replace-regexp-in-string
                                  "\\`INSERT INTO t (a) VALUES " "" sql))
                               lowered ", ")
                              "[(), ]+" t)))))
    (should (string-prefix-p "INSERT INTO t (a) VALUES (1), (2)"
                             (car (dorg-sql-lower-statements
                                   (dorg-sql-gen-tests--rows 3)))))))

(defun dorg-sql-gen-tests--text-row (text)
  "Return a one-column text row of table \"t\" holding TEXT."
  (dorg-sql-row-make "t" '(a) (list text)))

(ert-deftest dorg-test-sql-lower-byte-cap-counts-utf8-wire-bytes ()
  "The byte cap measures the complete statement's UTF-8 bytes, not characters.
Two rows whose merged statement is exactly the cap stay one statement; one
more byte splits them.  The second row is mostly two-byte characters, so a
character-counting cap would wrongly merge the over-cap pair."
  (should (equal 1048576 dorg-sql-lower-max-bytes))
  (let* ((prefix-bytes (string-bytes "INSERT INTO t (a) VALUES "))
         (wide (make-string 1000 ?é))
         ;; ('A') + , + ('W...B'): 4 + a + 2 + 4 + 2000 + b bytes plus prefix.
         (a 1000)
         (b (- dorg-sql-lower-max-bytes prefix-bytes 4 a 2 4 2000))
         (exact (dorg-sql-lower-statements
                 (list (dorg-sql-gen-tests--text-row (make-string a ?x))
                       (dorg-sql-gen-tests--text-row
                        (concat wide (make-string b ?y))))))
         (over (dorg-sql-lower-statements
                (list (dorg-sql-gen-tests--text-row (make-string a ?x))
                      (dorg-sql-gen-tests--text-row
                       (concat wide (make-string (1+ b) ?y)))))))
    (should (= 1 (length exact)))
    (should (= dorg-sql-lower-max-bytes
               (string-bytes (encode-coding-string (car exact) 'utf-8))))
    (should (= 2 (length over)))))

(ert-deftest dorg-test-sql-lower-oversized-row-stands-alone ()
  "A row over the byte cap is its own statement; its neighbours still merge.
Zero items lower to nothing."
  (let ((huge (dorg-sql-gen-tests--text-row
               (make-string (+ dorg-sql-lower-max-bytes 10) ?z)))
        (small (lambda (s) (dorg-sql-gen-tests--text-row s))))
    (should (= 3 (length (dorg-sql-lower-statements
                          (list (funcall small "a") huge (funcall small "b"))))))
    (let ((lowered (dorg-sql-lower-statements
                    (list huge (funcall small "a") (funcall small "b")))))
      (should (= 2 (length lowered)))
      (should (equal "INSERT INTO t (a) VALUES ('a'), ('b')" (cadr lowered))))
    (should (null (dorg-sql-lower-statements nil)))))

(ert-deftest dorg-test-sql-lower-merges-only-same-shape-adjacent-rows ()
  "Different tables or column lists never merge; a barrier splits a run.
Barriers keep their position and the input list and rows are unmodified."
  (let* ((r1 (dorg-sql-row-make "t" '(a b) '(1 "x")))
         (r2 (dorg-sql-row-make "t" '(a b) '(2 "y")))
         (r3 (dorg-sql-row-make "t" '(b a) '("z" 3)))
         (r4 (dorg-sql-row-make "u" '(a b) '(4 "w")))
         (r5 (dorg-sql-row-make "t" '(a b) '(5 "v")))
         (barrier "DELETE FROM t WHERE a = 0")
         (items (list r1 r2 r3 r4 r5 barrier r5 r1))
         (snapshot (mapcar (lambda (item)
                             (if (stringp item) item (dorg-sql-row-sql item)))
                           items))
         (spine (copy-sequence items)))
    (should (equal (list "INSERT INTO t (a, b) VALUES (1, 'x'), (2, 'y')"
                         "INSERT INTO t (b, a) VALUES ('z', 3)"
                         "INSERT INTO u (a, b) VALUES (4, 'w')"
                         "INSERT INTO t (a, b) VALUES (5, 'v')"
                         barrier
                         "INSERT INTO t (a, b) VALUES (5, 'v'), (1, 'x')")
                   (dorg-sql-lower-statements items)))
    (should (cl-every #'eq spine items))
    (should (equal snapshot
                   (mapcar (lambda (item)
                             (if (stringp item) item (dorg-sql-row-sql item)))
                           items)))))

(ert-deftest dorg-test-sql-row-renders-a-closed-literal-set ()
  "Each literal kind renders exactly; every other value or name is refused."
  (should
   (equal (concat "INSERT INTO t (a, b, c, d, e, f, g, h, i) VALUES "
                  "(NULL, 42, -3, 1180591620717411303424, 1.5, "
                  (dorg-sql-quote "it's \\ ; é\U0001F600")
                  ", TRUE, FALSE, X'00ff10')")
          (dorg-sql-row-sql
           (dorg-sql-row-make
            "t" '(a b c d e f g h i)
            (list nil 42 -3 (ash 1 70) 1.5 "it's \\ ; é\U0001F600"
                  :true :false (cons :hex (unibyte-string 0 255 16)))))))
  (dolist (bad (list 'foo t :maybe '(1 . 2) [1] (cons :hex 5)
                     (cons :hex "é") (list :hex "00")))
    (should-error (dorg-sql-row-make "t" '(a) (list bad))
                  :type 'dorg-sql-row-error))
  (dolist (table '("t;x" "1t" "a.b" "" "t x" "`t`"))
    (should-error (dorg-sql-row-make table '(a) '(1)) :type 'dorg-sql-row-error))
  (dolist (columns '(("a") (a\ b) (a\;) (\1a)))
    (should-error (dorg-sql-row-make "t" columns '(1)) :type 'dorg-sql-row-error))
  (should-error (dorg-sql-row-make "t" '(a b) '(1)) :type 'dorg-sql-row-error)
  (should-error (dorg-sql-row-make "t" nil nil) :type 'dorg-sql-row-error))

(ert-deftest dorg-test-sql-row-has-no-raw-constructor-or-copier ()
  "Literals are rendered only by `dorg-sql-row-make'.
No default constructor or copier exists, so no caller can supply raw SQL as a
literal."
  (should (fboundp 'dorg-sql-row-make))
  (should-not (fboundp 'make-dorg-sql-row))
  (should-not (fboundp 'copy-dorg-sql-row))
  (let ((row (dorg-sql-row-make "t" '(a) '("x"))))
    (should (dorg-sql-row-p row))
    (should (equal '("'x'") (dorg-sql-row-literals row)))))

(ert-deftest dorg-test-sql-gen-scalar-surface-matches-base-oracle ()
  "Control: every scalar generator string is byte-identical to the frozen base.
Covers heading, tag, item (all kinds), drawer-detail, items and full-heading
INSERTs, and the unchanged mutation lowering (item, row and heading updates,
rekey).  Green on base; it must stay green after the generators move to rows."
  (let ((golden (plist-get (dorg-ib-read-golden) :scalar))
        (actual (dorg-ib-normalize (dorg-ib-scalar-surface) nil)))
    (should (= (length golden) (length actual)))
    (cl-loop for g in golden
             for a in actual
             do (should (equal g a)))))

(ert-deftest dorg-test-sql-gen-effect-sql-matches-base-oracle ()
  "Control: typed effect SQL is byte-identical to the frozen base.
Covers NULL, BIGINT, negative, float, empty, quote, backslash, semicolon and
Unicode literals, and keyed and whole-table deletes."
  (should (equal (plist-get (dorg-ib-read-golden) :effects)
                 (dorg-ib-effect-surface dorg-ib-effect-samples))))

(ert-deftest dorg-test-sql-gen-row-generators-lower-to-the-scalar-strings ()
  "The row generators and the string APIs describe the same statements.
Each heading's full row list, lowered one row per statement, equals its
`dorg-sql-gen-heading-full-sql' strings, and both consume the item-id
generator in the same order."
  (let* ((left (dorg-ib-parse dorg-ib-corpus-bootstrap))
         (right (dorg-ib-parse dorg-ib-corpus-bootstrap))
         (root-id (dorg-parse-result-root-id left))
         (left-ids nil) (right-ids nil)
         (left-gen (let ((g (dorg-ib-id-generator)))
                     (lambda () (car (push (funcall g) left-ids)))))
         (right-gen (let ((g (dorg-ib-id-generator)))
                      (lambda () (car (push (funcall g) right-ids))))))
    (cl-loop for lh in (dorg-parse-result-headings left)
             for rh in (dorg-parse-result-headings right)
             do (should (equal (dorg-sql-gen-heading-full-sql lh root-id left-gen)
                               (mapcar #'dorg-sql-row-sql
                                       (dorg-sql-gen-heading-full-rows
                                        rh root-id right-gen)))))
    (should left-ids)
    (should (equal left-ids right-ids))
    (should (equal (dorg-ib-bindings (dorg-parse-result-headings left))
                   (dorg-ib-bindings (dorg-parse-result-headings right))))))

(defun dorg-sql-gen-tests--schema-fks ()
  "Return every (CHILD COLUMN PARENT) foreign key in `dorg-schema-ddl'."
  (let (fks)
    (dolist (ddl dorg-schema-ddl)
      (when (and (stringp ddl)
                 (string-match "\\`CREATE TABLE \\([a-z_]+\\)" ddl))
        (let ((child (match-string 1 ddl))
              (start 0))
          (while (string-match
                  "FOREIGN KEY (\\([a-z_]+\\))[[:space:]]+REFERENCES \\([a-z_]+\\)("
                  ddl start)
            (push (list child (match-string 1 ddl) (match-string 2 ddl)) fks)
            (setq start (match-end 0))))))
    (nreverse fks)))

(ert-deftest dorg-test-sql-gen-core-order-respects-every-schema-fk ()
  "Every non-self FK points from a later core table to an earlier one.
The only exemptions are the two named headings self-references, which the
stable partition satisfies by keeping document pre-order.  The only FK child
outside the order is the dorg_files binding, which is a barrier emitted last."
  (require 'dorg-schema)
  (let ((order dorg-sql-gen-core-table-order)
        (fks (dorg-sql-gen-tests--schema-fks))
        self outside)
    (should (equal order dorg-ibr-core-order))
    (should (> (length fks) 8))
    (dolist (fk fks)
      (pcase-let ((`(,child ,column ,parent) fk))
        (cond
         ((equal child parent) (push (cons child column) self))
         ((not (member child order)) (push child outside))
         (t (should (member parent order))
            (should (< (cl-position parent order :test #'equal)
                       (cl-position child order :test #'equal)))))))
    (should (equal '(("headings" . "parent_id") ("headings" . "root_id"))
                   (sort self (lambda (a b) (string< (cdr a) (cdr b))))))
    (should (equal '("dorg_files") outside))))

(ert-deftest dorg-test-sql-gen-order-core-rows-groups-tables-stably ()
  "Core rows come out grouped by table order, each table in input order.
An unknown table fails closed instead of being dropped or appended."
  (let* ((mk (lambda (table id) (dorg-sql-row-make table '(id) (list id))))
         (input (list (funcall mk "headings" 1) (funcall mk "heading_tags" 2)
                      (funcall mk "heading_items" 3) (funcall mk "text_items" 4)
                      (funcall mk "headings" 5) (funcall mk "heading_items" 6)
                      (funcall mk "keyword_items" 7) (funcall mk "drawer_items" 8)
                      (funcall mk "property_entries" 9) (funcall mk "drawer_lines" 10)
                      (funcall mk "headings" 11) (funcall mk "heading_tags" 12)
                      (funcall mk "planning_items" 13) (funcall mk "text_items" 14)
                      (funcall mk "property_entries" 15)))
         (spine (copy-sequence input))
         (ordered (dorg-sql-gen-order-core-rows input)))
    (should (equal '(1 5 11 2 12 3 6 4 14 7 13 8 9 15 10)
                   (mapcar (lambda (row)
                             (string-to-number (car (dorg-sql-row-literals row))))
                           ordered)))
    (should (cl-every #'eq spine input))
    (should-error (dorg-sql-gen-order-core-rows
                   (list (funcall mk "dorg_files" 1)))
                  :type 'dorg-sql-row-error)))

(defun dorg-sql-gen-tests--emacs (&rest args)
  "Run a fresh batch Emacs with ARGS; return (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((exit (apply #'call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil t nil "-Q" "--batch" args)))
      (cons exit (buffer-string)))))

(ert-deftest dorg-test-sql-gen-row-api-is-transport-free ()
  "The row API and the generators load cold without the SQL transport."
  (let ((result (dorg-sql-gen-tests--emacs
                 "-L" dorg-sql-gen-tests--repo-dir
                 "--eval"
                 (concat "(progn (require 'dorg-sql-quote)"
                         " (unless (fboundp 'dorg-sql-lower-statements) (kill-emacs 3))"
                         " (require 'dorg-sql-gen)"
                         " (unless (fboundp 'dorg-sql-gen-heading-full-rows) (kill-emacs 4))"
                         " (when (or (featurep 'dorg-dolt) (featurep 'mysql)) (kill-emacs 5))"
                         " (kill-emacs 0))"))))
    (should (equal 0 (car result)))))

(ert-deftest dorg-test-sql-gen-touched-files-compile-without-warnings ()
  "Each file of the lowering change byte-compiles warning-free in a fresh Emacs."
  (let* ((out (make-temp-file "dorg-lowering-compile-" t))
         (root dorg-sql-gen-tests--repo-dir)
         (files (mapcar (lambda (f) (expand-file-name f root))
                        '("dorg-sql-quote.el" "dorg-sql-gen.el" "dorg-ext.el"
                          "dorg-import.el" "dorg-bm25.el"))))
    (unwind-protect
        (let ((result
               (dorg-sql-gen-tests--emacs
                "-L" root "-L" (expand-file-name "lib/mysql" root)
                "-L" (expand-file-name "lib/clime" root)
                "--eval"
                (format
                 (concat "(progn (require 'bytecomp)"
                         " (setq byte-compile-error-on-warn t"
                         " byte-compile-warnings '(not docstrings-wide)"
                         " byte-compile-dest-file-function"
                         " (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") %S)))"
                         " (dolist (f '%S) (unless (byte-compile-file f) (kill-emacs 6)))"
                         " (kill-emacs 0))")
                 out files))))
          (should (equal 0 (car result))))
      (delete-directory out t))))

(defun dorg-sql-gen-tests--emacs-bounded (seconds &rest args)
  "Run a fresh batch Emacs with ARGS for at most SECONDS.
Return its exit status, or `timeout' after killing a process still running
at the deadline, so a hanging probe fails its test instead of the suite."
  (let* ((proc (apply #'start-process "dorg-bounded-emacs" nil
                      (expand-file-name invocation-name invocation-directory)
                      "-Q" "--batch" args))
         (deadline (+ (float-time) seconds)))
    (while (and (process-live-p proc) (< (float-time) deadline))
      (accept-process-output proc 0.1))
    (if (process-live-p proc)
        (progn (delete-process proc) 'timeout)
      (process-exit-status proc))))

(ert-deftest dorg-test-sql-row-entry-points-refuse-malformed-lists ()
  "Improper and cyclic lists are typed refusals at every row entry point.
`dorg-sql-row-make' (columns and values), `dorg-sql-lower-statements' (items)
and `dorg-sql-gen-order-core-rows' (rows) each signal `dorg-sql-row-error'
for a dotted list and for a cyclic list, and none of them loops.  Valid rows
near the byte cap and a full 500-row run still lower.  The probe runs in a
bounded child Emacs."
  (should
   (equal
    0
    (dorg-sql-gen-tests--emacs-bounded
     60 "-L" dorg-sql-gen-tests--repo-dir
     "--eval"
     (concat
      "(progn (require 'dorg-sql-gen)"
      " (let* ((cyc (lambda (x) (let ((c (list x))) (setcdr c c) c)))"
      "        (row (dorg-sql-row-make \"t\" '(a) '(1)))"
      ;; A core-table row: the ordering must reach the malformed tail, not
      ;; refuse an unknown table at the first element.
      "        (core (dorg-sql-row-make \"headings\" '(id) '(1)))"
      "        (cases (list (lambda () (dorg-sql-row-make \"t\" '(a . b) '(1)))"
      "                     (lambda () (dorg-sql-row-make \"t\" '(a) '(1 . 2)))"
      "                     (lambda () (dorg-sql-row-make \"t\" (funcall cyc 'a) '(1)))"
      "                     (lambda () (dorg-sql-row-make \"t\" '(a) (funcall cyc 1)))"
      "                     (lambda () (dorg-sql-lower-statements (cons \"x\" \"y\")))"
      "                     (lambda () (dorg-sql-lower-statements (funcall cyc row)))"
      "                     (lambda () (dorg-sql-gen-order-core-rows (cons core core)))"
      "                     (lambda () (dorg-sql-gen-order-core-rows (funcall cyc core))))))"
      "   (dolist (case cases)"
      "     (unless (eq 'refused (condition-case nil (progn (funcall case) 'accepted)"
      "                            (dorg-sql-row-error 'refused)))"
      "       (kill-emacs 7)))"
      ;; Valid rows near both caps are still accepted and lowered.
      "   (let* ((near (dorg-sql-row-make \"t\" '(a b) (list 1 (make-string (- dorg-sql-lower-max-bytes 100) ?x))))"
      "          (many (make-list dorg-sql-lower-max-rows (dorg-sql-row-make \"t\" '(a) '(1))))"
      "          (out (dorg-sql-lower-statements (cons near many))))"
      "     (unless (and (= 2 (length out))"
      "                  (< (- dorg-sql-lower-max-bytes 100) (string-bytes (car out)) dorg-sql-lower-max-bytes))"
      "       (kill-emacs 8))))"
      " (kill-emacs 0))")))))
