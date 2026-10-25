;;; dorg-postimage-readback-tests.el --- Batched postimage readback -*- lexical-binding: t; -*-

;;; Commentary:

;; The commit-time proof reads back, inside the pending transaction, how many
;; derived rows match each present and absent check.  These specs pin the
;; batched read-port operation that answers many checks in one statement:
;; its closed request and response grammar, its exact equivalence with the
;; per-check count on the transaction's uncommitted postimage, the order in
;; which the proof evaluates checks and reports the first failure, the
;; number of requests the proof issues, and complete rollback on a failure.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-test-helpers)
(require 'dorg-mutate-tests)

;;; ─── Grammar (no connection) ─────────────────────────────────────────

(defun dorg-test-readback--port-error (port request)
  "Return the refusal data plist of PORT on REQUEST, failing if it answers."
  (let ((err (should-error (funcall port request)
                           :type 'dorg-sql-transaction-read-port-error)))
    (nth 2 err)))

(ert-deftest dorg-test-readback-batch-request-grammar-refuses-before-sql ()
  "A malformed batch request is refused whole, before any SQL is sent.
Unknown, duplicate or missing keys, another version, an empty, improper,
non-list or oversized check list and a malformed pair are request errors; a
bad table, core table, bad column or bad value refuses with the per-check
reason and its position, whether it comes first or last."
  (let ((sent 0)
        (ok '("a_ix" (k . 1)))
        (cap dorg-sql-transaction-read-port-max-checks))
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (&rest _) (cl-incf sent) '(((n0 . 0))))))
      (let ((port (dorg-sql--transaction-read-port 'conn '("a_ix" "b_ix"))))
        (pcase-dolist (`(,reason ,position . ,request)
                       `((bad-request nil :version 1 :op extension-row-counts
                                      :checks (,ok) :extra t)
                         (bad-request nil :version 1 :op extension-row-counts
                                      :checks (,ok) :checks (,ok))
                         (bad-version nil :version 2 :op extension-row-counts
                                      :checks (,ok))
                         (bad-checks nil :version 1 :op extension-row-counts)
                         (bad-checks nil :version 1 :op extension-row-counts
                                     :checks nil)
                         (bad-checks nil :version 1 :op extension-row-counts
                                     :checks "a_ix")
                         (bad-checks nil :version 1 :op extension-row-counts
                                     :checks (,ok ,ok . "tail"))
                         (bad-checks nil :version 1 :op extension-row-counts
                                     :checks ,(make-list (1+ cap) ok))
                         (bad-checks 1 :version 1 :op extension-row-counts
                                     :checks (,ok "a_ix"))
                         (core-table 0 :version 1 :op extension-row-counts
                                     :checks (("headings" (id . 1)) ,ok))
                         (core-table 1 :version 1 :op extension-row-counts
                                     :checks (,ok ("headings" (id . 1))))
                         (unowned-table 1 :version 1 :op extension-row-counts
                                        :checks (,ok ("c_ix" (k . 1))))
                         (bad-table 1 :version 1 :op extension-row-counts
                                    :checks (,ok ("a_ix; DROP" (k . 1))))
                         (bad-predicate 1 :version 1 :op extension-row-counts
                                        :checks (,ok ("a_ix" . "k = 1")))
                         (bad-column 1 :version 1 :op extension-row-counts
                                     :checks (,ok ("a_ix" (bad-col . 1))))
                         (bad-value 1 :version 1 :op extension-row-counts
                                    :checks (,ok ("a_ix" (k raw))))))
          (let ((data (dorg-test-readback--port-error port request)))
            (should (eq reason (plist-get data :reason)))
            (should (equal position (plist-get data :position)))))
        (should (= 0 sent))
        ;; A full batch at the cap is accepted.
        (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                   (lambda (&rest _)
                     (cl-incf sent)
                     (list (cl-loop for i below cap
                                    collect (cons (intern (format "n%d" i)) 0))))))
          (should (equal (make-list cap 0)
                         (funcall port (list :version 1 :op 'extension-row-counts
                                             :checks (make-list cap ok)))))
          (should (= 1 sent)))))))

(ert-deftest dorg-test-readback-batch-where-must-be-a-proper-alist ()
  "Each check's predicate is nil or a proper list of (COLUMN . VALUE) cells.
An improper predicate, or one with a non-cell element, refuses typed with
its position, first or last, before any SQL is sent."
  (let ((sent 0) (ok '("a_ix" (k . 1))))
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (&rest _) (cl-incf sent) '(((n0 . 0) (n1 . 0))))))
      (let ((port (dorg-sql--transaction-read-port 'conn '("a_ix"))))
        (pcase-dolist (`(,position . ,checks)
                       `((0 ("a_ix" (k . 1) . tail) ,ok)
                         (1 ,ok ("a_ix" (k . 1) . tail))
                         (0 ("a_ix" (k . 1) 5) ,ok)
                         (1 ,ok ("a_ix" (k . 1) 5))))
          (let ((data (dorg-test-readback--port-error
                       port (list :version 1 :op 'extension-row-counts :checks checks))))
            (should (eq 'bad-predicate (plist-get data :reason)))
            (should (equal position (plist-get data :position)))))
        (should (= 0 sent))))))

(ert-deftest dorg-test-readback-batch-cyclic-where-is-refused ()
  "A cyclic predicate refuses typed with its position instead of looping.
Before the fix the traversal of a cyclic predicate never returns, so this
spec's red state is observed in an isolated process under a timeout."
  (let ((sent 0)
        (print-circle t)
        (ok '("a_ix" (k . 1))))
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (&rest _) (cl-incf sent) '(((n0 . 0) (n1 . 0))))))
      (let ((port (dorg-sql--transaction-read-port 'conn '("a_ix"))))
        (dolist (position '(0 1))
          (let* ((cycle (list (cons 'k 1)))
                 (_ (setcdr cycle cycle))
                 (bad (cons "a_ix" cycle))
                 (checks (if (= position 0) (list bad ok) (list ok bad)))
                 (data (dorg-test-readback--port-error
                        port (list :version 1 :op 'extension-row-counts :checks checks))))
            (should (eq 'bad-predicate (plist-get data :reason)))
            (should (equal position (plist-get data :position)))))
        (should (= 0 sent))))))

(ert-deftest dorg-test-readback-batch-sends-the-per-check-statement-text ()
  "One statement holds each check's own per-check COUNT, in order.
Every subquery is exactly the statement the per-check count sends, so the
batch inherits its NULL, collation and coercion semantics."
  (let (single batched)
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (sql _conn)
                 (if (string-prefix-p "SELECT COUNT(*)" sql)
                     (progn (push sql single) '(((n . 0))))
                   (setq batched sql)
                   '(((n0 . 0) (n1 . 0) (n2 . 0)))))))
      (let ((port (dorg-sql--transaction-read-port 'conn '("a_ix" "b_ix")))
            (checks '(("a_ix" (k . 1) (v)) ("b_ix") ("a_ix" (w . "x; y")))))
        (dolist (check checks)
          (funcall port (list :version 1 :op 'extension-row-count
                              :table (car check) :where (cdr check))))
        (funcall port (list :version 1 :op 'extension-row-counts :checks checks))
        (should (equal batched
                       (concat "SELECT "
                               (mapconcat #'identity
                                          (cl-loop for sql in (reverse single)
                                                   for i from 0
                                                   collect (format "(%s) AS n%d"
                                                                   (string-replace " AS n FROM" " FROM" sql)
                                                                   i))
                                          ", "))))
        (should (equal batched
                       (concat "SELECT (SELECT COUNT(*) FROM a_ix WHERE k = 1 AND v IS NULL) AS n0,"
                               " (SELECT COUNT(*) FROM b_ix) AS n1,"
                               " (SELECT COUNT(*) FROM a_ix WHERE w = 'x; y') AS n2")))))))

(ert-deftest dorg-test-readback-batch-response-is-exactly-one-typed-row ()
  "The batch answer is one row holding exactly one native count per check.
Counts are taken by alias, not by column position; a missing or extra row,
a missing, extra or renamed alias, NULL, a negative, a float or a numeric
string is a typed refusal, never a default and never a parsed spelling."
  (let ((request '(:version 1 :op extension-row-counts
                   :checks (("a_ix" (k . 1)) ("a_ix" (k . 2))))))
    (pcase-dolist (`(,rows . ,expected)
                   '(((((n0 . 3) (n1 . 0))) . (3 0))
                     ((((n1 . 0) (n0 . 3))) . (3 0))))
      (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (_sql _conn) (copy-tree rows))))
        (should (equal expected
                       (funcall (dorg-sql--transaction-read-port 'conn '("a_ix"))
                                request)))))
    (dolist (rows '(nil
                    (((n0 . 3) (n1 . 0)) ((n0 . 3) (n1 . 0)))
                    (((n0 . 3)))
                    (((n0 . 3) (n1 . 0) (n2 . 1)))
                    (((n0 . 3) (m1 . 0)))
                    (((n0 . 3) (n1)))
                    (((n0 . 3) (n1 . -1)))
                    (((n0 . 3) (n1 . 1.0)))
                    (((n0 . 3) (n1 . "1")))))
      (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (_sql _conn) (copy-tree rows))))
        (should (eq 'bad-count
                    (plist-get (dorg-test-readback--port-error
                                (dorg-sql--transaction-read-port 'conn '("a_ix"))
                                request)
                               :reason)))))))

;;; ─── Requests and order (recording read) ─────────────────────────────

(defun dorg-test-readback--checks (present absent)
  "Return row checks with PRESENT present and ABSENT absent entries.
Check I names table t(9999-I), so tables sort opposite to check order."
  (list :present (cl-loop for i below present
                          collect (list (format "t%04d" (- 9999 i)) `((k . ,i)) 1))
        :absent (cl-loop for i from present below (+ present absent)
                         collect (list (format "t%04d" (- 9999 i)) `((k . ,i)) 0))))

(defun dorg-test-readback--recording-read (present failing)
  "Return (READ . LOG): READ answers checks as a correct postimage would.
Check I is present when I < PRESENT (readback 1) and absent otherwise
\(readback 0); the checks whose index is in FAILING read back the other
value.  LOG records each request."
  (let ((log (list nil)))
    (cl-flet ((answer (where)
                (let* ((i (alist-get 'k where))
                       (expected (if (< i present) 1 0)))
                  (if (memq i failing) (- 1 expected) expected))))
      (cons (lambda (request)
              (push request (car log))
              (pcase (plist-get request :op)
                ('extension-row-counts
                 (mapcar (lambda (check) (answer (cdr check)))
                         (plist-get request :checks)))
                ('extension-row-count (answer (plist-get request :where)))))
            log))))

(defun dorg-test-readback--run (present absent failing)
  "Assert rows for PRESENT/ABSENT checks with FAILING readbacks.
Return (ABORT-TABLE BATCH-REQUESTS SINGLE-REQUESTS CHECKS-READ)."
  (pcase-let* ((`(,read . ,log) (dorg-test-readback--recording-read present failing))
               (table (condition-case err
                          (progn (dorg-mutate--postimage-assert-rows
                                  read (dorg-test-readback--checks present absent))
                                 nil)
                        (dorg-sql-postimage-parity
                         (plist-get (nth 2 err) :table))))
               (requests (reverse (car log))))
    (list table
          (cl-count 'extension-row-counts requests :key (lambda (r) (plist-get r :op)))
          (cl-count 'extension-row-count requests :key (lambda (r) (plist-get r :op)))
          (apply #'+ (mapcar (lambda (r) (if (eq 'extension-row-counts (plist-get r :op))
                                             (length (plist-get r :checks))
                                           1))
                             requests)))))

(ert-deftest dorg-test-readback-proof-issues-one-request-per-chunk ()
  "N passing checks cost ceil(N/256) batched requests and no per-check read."
  (dolist (n '(1 255 256 257 1000))
    (pcase-let ((`(,table ,batched ,single ,read) (dorg-test-readback--run n 0 nil)))
      (should-not table)
      (should (= single 0))
      (should (= batched (ceiling n 256.0)))
      (should (= read n)))))

(ert-deftest dorg-test-readback-first-failure-and-no-later-chunk ()
  "The first failing check in proof order is the one reported, and no chunk
after the one holding it is requested.  Failures sit at both sides of the
256-check chunk boundary and of the present-to-absent boundary; checks
alternate tables, so regrouping by table would change the report."
  (pcase-dolist (`(,present ,absent ,failing ,reported)
                 '((600 0 (254) 254) (600 0 (255) 255) (600 0 (256) 256)
                   (600 0 (257) 257) (600 0 (257 3) 3) (600 0 (500 300) 300)
                   (256 10 (256) 256) (256 10 (255 256) 255)
                   (250 20 (260 249) 249) (250 20 (260) 260)))
    (pcase-let ((`(,table ,batched ,single ,_)
                 (dorg-test-readback--run present absent failing)))
      (should (equal table (format "t%04d" (- 9999 reported))))
      (should (= single 0))
      (should (= batched (1+ (/ reported 256)))))))

;;; ─── The real transaction ────────────────────────────────────────────

(define-error 'dorg-test-readback-rollback "readback probe rollback")

(defun dorg-test-readback--in-transaction (dolt-dir statements tables fn &optional commit)
  "Run STATEMENTS in one lease transaction on DOLT-DIR, then FN with its port.
FN receives the transaction's typed read port over TABLES; its value is
returned.  The transaction then rolls back, unless COMMIT."
  (let (value)
    (condition-case nil
        (dorg-sql--with-lease (lease dolt-dir)
          (dorg-sql--lease-batch
           lease statements "readback probe"
           (list :mode 'transaction-read-port :extension-tables tables
                 :function (lambda (read)
                             (setq value (funcall fn read))
                             (unless commit
                               (signal 'dorg-test-readback-rollback nil))))))
      (dorg-test-readback-rollback nil))
    value))

(defun dorg-test-readback--seed (dolt-dir)
  "Commit two plain extension tables at HEAD of DOLT-DIR."
  (dorg-sql-batch
   dolt-dir
   (list "CREATE TABLE vis_a (k INT, v TEXT NULL, w TEXT)"
         "CREATE TABLE vis_b (k INT)"
         (concat "INSERT INTO vis_a VALUES (1, 'x', 'p'), (1, 'x', 'p'),"
                 " (2, NULL, 'q'), (3, 'y', 'r')")
         "INSERT INTO vis_b VALUES (1), (1), (9)"))
  (dorg-commit dolt-dir "seed readback tables"))

(defun dorg-test-readback--snapshot (dolt-dir)
  "Return HEAD and the full contents of both tables in DOLT-DIR."
  (list (dorg-head-commit dolt-dir)
        (dorg-sql dolt-dir "SELECT k, v, w FROM vis_a ORDER BY k, v, w")
        (dorg-sql dolt-dir "SELECT k FROM vis_b ORDER BY k")))

(defconst dorg-test-readback--postimage-statements
  '("DELETE FROM vis_a WHERE k = 3"
    "INSERT INTO vis_a VALUES (4, 'z', 's'), (4, 'z', 's'), (5, NULL, 't')"
    "DELETE FROM vis_b WHERE k = 9"
    "INSERT INTO vis_b VALUES (7)")
  "Statements whose uncommitted postimage differs from HEAD.")

(defconst dorg-test-readback--mixed-checks
  '(("vis_a" (k . 1) (v . "x") (w . "p"))
    ("vis_a" (k . 4))
    ("vis_a" (v))
    ("vis_a" (k . 3))
    ("vis_a")
    ("vis_b" (k . 7))
    ("vis_b" (k . 9))
    ("vis_b" (k . 1))
    ("vis_a" (k . 1))
    ("vis_a" (k . 1) (v . "x")))
  "Present rows with counts above one, an omitted field, a NULL cell, rows
the transaction deleted and inserted, a whole-table count, two tables, a
duplicate and an overlapping predicate.")

(ert-deftest dorg-test-readback-batch-equals-per-check-on-the-postimage ()
  "Inside one transaction the batch answers exactly the per-check counts of
the uncommitted postimage, which differs from HEAD; a nonexistent SQL column
fails the whole request closed instead of counting zero."
  (dorg-test-mutate--with-dolt
    (unless (eq 'mysql (dorg-sql--transport-kind dolt-dir))
      (ert-skip "the transaction read port needs the server transport"))
    (dorg-test-readback--seed dolt-dir)
    (let* ((before (dorg-test-readback--snapshot dolt-dir))
           (answers
            (dorg-test-readback--in-transaction
             dolt-dir dorg-test-readback--postimage-statements '("vis_a" "vis_b")
             (lambda (read)
               (list (funcall read (list :version 1 :op 'extension-row-counts
                                         :checks dorg-test-readback--mixed-checks))
                     (mapcar (lambda (check)
                               (funcall read (list :version 1 :op 'extension-row-count
                                                   :table (car check) :where (cdr check))))
                             dorg-test-readback--mixed-checks)
                     (condition-case err
                         (funcall read (list :version 1 :op 'extension-row-counts
                                             :checks '(("vis_a" (k . 1))
                                                       ("vis_a" (nope . 1)))))
                       (mysql-query-error (list :refused (cadr err)))))))))
      (should (equal '(2 2 2 0 6 1 0 2 2 2) (nth 0 answers)))
      (should (equal (nth 0 answers) (nth 1 answers)))
      ;; HEAD holds one k = 3 row and no k = 7 row: an off-connection or HEAD
      ;; read could not produce these counts.
      (should (equal '(((n . 1))) (dorg-sql dolt-dir "SELECT COUNT(*) AS n FROM vis_a WHERE k = 3")))
      (should (equal '(((n . 0))) (dorg-sql dolt-dir "SELECT COUNT(*) AS n FROM vis_b WHERE k = 7")))
      ;; The executor's own typed refusal, never a count.
      (should (eq :refused (car (nth 2 answers))))
      (should (string-match-p "column \"nope\" could not be found" (cadr (nth 2 answers))))
      (should (equal before (dorg-test-readback--snapshot dolt-dir))))))

(defun dorg-test-readback--reference-assert-rows (read checks)
  "Assert CHECKS through READ one check at a time, as the per-check proof did."
  (dolist (check (plist-get checks :present))
    (pcase-let ((`(,table ,row ,count) check))
      (when (< (funcall read (list :version 1 :op 'extension-row-count
                                   :table table :where row))
               count)
        (dorg-mutate--postimage-abort
         'rows "a validated derived row is missing or altered at readback"
         :table table))))
  (dolist (check (plist-get checks :absent))
    (pcase-let ((`(,table ,where ,count) check))
      (when (/= (funcall read (list :version 1 :op 'extension-row-count
                                    :table table :where where))
                count)
        (dorg-mutate--postimage-abort
         'rows "a deleted derived scope holds unexpected rows at readback"
         :table table)))))

(defun dorg-test-readback--attempt (dolt-dir statements assert checks)
  "Run STATEMENTS, then ASSERT over CHECKS, in one transaction on DOLT-DIR.
The transaction commits when ASSERT returns.  A parity abort is NOT caught
inside the transaction: it escapes validation, the transaction rolls back,
and this function catches it at the boundary.  Return `:committed' or
\(parity MESSAGE PLIST)."
  (condition-case err
      (progn
        (dorg-sql--with-lease (lease dolt-dir)
          (dorg-sql--lease-batch
           lease statements "readback attempt"
           (list :mode 'transaction-read-port :extension-tables '("vis_a")
                 :function (lambda (read) (funcall assert read checks)))))
        :committed)
    (dorg-sql-postimage-parity (cons 'parity (cdr err)))))

(defun dorg-test-readback--full-snapshot (dolt-dir heading)
  "Return HEAD, HEADING's title, the evidence rows and both tables of DOLT-DIR."
  (append (dorg-test-readback--snapshot dolt-dir)
          (list (dorg-sql dolt-dir (format "SELECT title FROM headings WHERE id = %d" heading))
                (dorg-sql dolt-dir "SELECT op_id, gen_id FROM dorg_mutation_log ORDER BY op_id"))))

(ert-deftest dorg-test-readback-corruption-aborts-like-per-check-and-rolls-back ()
  "Each corruption aborts exactly as the per-check proof does and rolls back.
The planned transaction changes a heading title, writes an evidence row,
inserts one derived row and deletes a derived scope.  Each corruption (the
inserted row missing, the inserted row altered, an extra row surviving in
the deleted scope) is attempted twice in separate transactions, once per
proof.  The production parity abort escapes validation, both proofs report
the identical typed abort, and every observed state (HEAD, the heading,
the evidence rows and both derived tables) is restored.  An extra row that
also matches a present check is not a failure under its at-least count;
inside a deleted scope it is the survivor case.  Without corruption the same
transaction commits and changes all of that state."
  (dorg-test-mutate--with-dolt
    (unless (eq 'mysql (dorg-sql--transport-kind dolt-dir))
      (ert-skip "the transaction read port needs the server transport"))
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Alpha\nalpha\n" 100))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (planned (list (format "UPDATE headings SET title = 'changed in transaction' WHERE id = %d"
                                  alpha)
                          (concat "INSERT INTO dorg_mutation_log (op_id, gen_id, intent_digest, ts)"
                                  " VALUES ('readback-probe-op', 'readback-probe-gen', 'digest', 1)")
                          "INSERT INTO vis_a VALUES (6, 'u', 'v')"
                          "DELETE FROM vis_a WHERE k = 2"))
           (checks (dorg-mutate--postimage-row-checks
                    '((:table "vis_a" :insert ((k . 6) (v . "u") (w . "v")))
                      (:table "vis_a" :delete ((k . 2))))))
           before)
      (ignore root)
      (dorg-test-readback--seed dolt-dir)
      (setq before (dorg-test-readback--full-snapshot dolt-dir alpha))
      (pcase-dolist (`(,corruption . ,message)
                     '(("DELETE FROM vis_a WHERE k = 6"
                        . "a validated derived row is missing or altered at readback")
                       ("UPDATE vis_a SET w = 'X' WHERE k = 6"
                        . "a validated derived row is missing or altered at readback")
                       ("INSERT INTO vis_a VALUES (2, 'e', 'e')"
                        . "a deleted derived scope holds unexpected rows at readback")))
        (let ((reference (dorg-test-readback--attempt
                          dolt-dir (append planned (list corruption))
                          #'dorg-test-readback--reference-assert-rows checks))
              (_ (should (equal before (dorg-test-readback--full-snapshot dolt-dir alpha))))
              (batched (dorg-test-readback--attempt
                        dolt-dir (append planned (list corruption))
                        #'dorg-mutate--postimage-assert-rows checks)))
          (should (equal `(parity ,message (:plane rows :table "vis_a")) reference))
          (should (equal reference batched))
          (should (equal before (dorg-test-readback--full-snapshot dolt-dir alpha)))))
      (should (eq :committed (dorg-test-readback--attempt
                              dolt-dir planned #'dorg-mutate--postimage-assert-rows checks)))
      (let ((after (dorg-test-readback--full-snapshot dolt-dir alpha)))
        (should-not (equal (nth 0 before) (nth 0 after)))
        (should (equal '(((title . "changed in transaction"))) (nth 3 after)))
        (should (equal '(((op_id . "readback-probe-op") (gen_id . "readback-probe-gen")))
                       (nth 4 after)))
        (should (equal '(((n . 1))) (dorg-sql dolt-dir "SELECT COUNT(*) AS n FROM vis_a WHERE k = 6")))
        (should (equal '(((n . 0))) (dorg-sql dolt-dir "SELECT COUNT(*) AS n FROM vis_a WHERE k = 2")))))))

(provide 'dorg-postimage-readback-tests)

;;; dorg-postimage-readback-tests.el ends here
