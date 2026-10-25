;;; dorg-postimage-row-checks-tests.el --- Postimage row-check planning -*- lexical-binding: t; -*-

;;; Commentary:

;; The commit-time proof turns the validated derived effects into net-effect
;; readback checks: which inserted rows must be present, and how many rows
;; each delete scope must still hold.  These specs pin that output exactly,
;; against a frozen copy of the original planner and against hand-stated
;; values, and bound the planner's work deterministically, so a linear
;; planner can replace the quadratic one without changing a single check.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-mutate)

;;; ─── Reference ───────────────────────────────────────────────────────

(defun dorg-test-rowchecks--reference (effects)
  "Return the row checks of EFFECTS as the original planner computed them.
A verbatim copy of the quadratic planner, kept as the semantic oracle."
  (let (survivors deletes)
    (dolist (effect effects)
      (let ((table (plist-get effect :table)))
        (cond
         ((plist-member effect :insert)
          (push (cons table (plist-get effect :insert)) survivors))
         ((plist-member effect :delete)
          (let ((where (plist-get effect :delete)))
            (setq survivors
                  (cl-delete-if (lambda (pr)
                                  (and (equal (car pr) table)
                                       (dorg-mutate--postimage-row-matches-p
                                        (cdr pr) where)))
                                survivors))
            (push (cons table where) deletes))))))
    (setq survivors (nreverse survivors)
          deletes (nreverse deletes))
    (let (present)
      (dolist (pr survivors)
        (let ((hit (cl-find-if (lambda (c)
                                 (and (equal (nth 0 c) (car pr))
                                      (equal (nth 1 c) (cdr pr))))
                               present)))
          (if hit (cl-incf (nth 2 hit))
            (push (list (car pr) (cdr pr) 1) present))))
      (list :present (nreverse present)
            :absent
            (mapcar (lambda (d)
                      (list (car d) (cdr d)
                            (cl-count-if
                             (lambda (pr)
                               (and (equal (car pr) (car d))
                                    (dorg-mutate--postimage-row-matches-p
                                     (cdr pr) (cdr d))))
                             survivors)))
                    deletes)))))

(defun dorg-test-rowchecks--ins (table &rest cells)
  "Return an insert effect of CELLS into TABLE."
  (list :table table :insert cells))

(defun dorg-test-rowchecks--del (table &rest cells)
  "Return a delete effect of predicate CELLS on TABLE."
  (list :table table :delete cells))

(defun dorg-test-rowchecks--both (effects expected)
  "Assert the planner and the reference both return EXPECTED for EFFECTS."
  (should (equal expected (dorg-test-rowchecks--reference (copy-tree effects))))
  (should (equal expected (dorg-mutate--postimage-row-checks (copy-tree effects)))))

;;; ─── Exact semantics ─────────────────────────────────────────────────

(ert-deftest dorg-test-rowchecks-named-cases ()
  "Each named shape of the net-effect checks has one exact answer.
A later delete removes earlier matching inserts of its table only; an insert
after it survives and counts in its absent scope; present rows keep their
first-appearance order and multiplicity; a missing column never matches and
nil matches only nil; an empty predicate deletes the whole table; every
delete keeps its own absent entry."
  (let ((ins #'dorg-test-rowchecks--ins) (del #'dorg-test-rowchecks--del))
    ;; Re-insert after a delete of the same scope (case R).
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(k . 1)) (funcall del "t" '(k . 1))
           (funcall ins "t" '(k . 1)) (funcall ins "t" '(k . 1)))
     '(:present (("t" ((k . 1)) 2)) :absent (("t" ((k . 1)) 2))))
    ;; First-appearance order and multiplicity.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(k . 2)) (funcall ins "t" '(k . 1)) (funcall ins "t" '(k . 2)))
     '(:present (("t" ((k . 2)) 2) ("t" ((k . 1)) 1)) :absent nil))
    ;; A missing column never matches; nil matches only nil.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(a)) (funcall ins "t" '(b . 1)) (funcall ins "t" '(a . 0))
           (funcall del "t" '(a)))
     '(:present (("t" ((b . 1)) 1) ("t" ((a . 0)) 1)) :absent (("t" ((a)) 0))))
    ;; An empty predicate deletes its whole table, and only its table.
    (dorg-test-rowchecks--both
     (list (funcall ins "t1" '(k . 1)) (funcall ins "t2" '(k . 1)) (list :table "t1" :delete nil))
     '(:present (("t2" ((k . 1)) 1)) :absent (("t1" nil 0))))
    ;; Duplicate deletes keep one absent entry each.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(k . 1)) (funcall del "t" '(k . 2)) (funcall del "t" '(k . 2)))
     '(:present (("t" ((k . 1)) 1)) :absent (("t" ((k . 2)) 0) ("t" ((k . 2)) 0))))
    ;; A duplicated predicate column must match in both cells; column order
    ;; in a predicate does not matter.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(k . 1)) (funcall del "t" '(k . 1) '(k . 2))
           (funcall ins "t" '(a . 1) '(b . 2)) (funcall del "t" '(b . 2) '(a . 1)))
     '(:present (("t" ((k . 1)) 1))
       :absent (("t" ((k . 1) (k . 2)) 0) ("t" ((b . 2) (a . 1)) 0))))))

(ert-deftest dorg-test-rowchecks-overlapping-scopes-and-late-scopes ()
  "Deletes of different column sets on one table see one survivor set.
Overlapping scopes (case O): a row removed through one scope is gone for
every other scope, and its re-insert is live in all of them.  A late scope
(case L), first used after inserts, re-inserts and removals through another
scope, sees exactly the rows live at that moment and every later insert."
  (let ((ins #'dorg-test-rowchecks--ins) (del #'dorg-test-rowchecks--del))
    ;; Case O.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(a . 1) '(b . 1))
           (funcall ins "t" '(a . 1) '(b . 2))
           (funcall del "t" '(a . 1) '(b . 1))
           (funcall del "t" '(a . 1))
           (funcall ins "t" '(a . 1) '(b . 1))
           (funcall del "t" '(a . 1) '(b . 2)))
     '(:present (("t" ((a . 1) (b . 1)) 1))
       :absent (("t" ((a . 1) (b . 1)) 1)
                ("t" ((a . 1)) 1)
                ("t" ((a . 1) (b . 2)) 0))))
    ;; Case L.
    (dorg-test-rowchecks--both
     (list (funcall ins "t" '(a . 1) '(b . 1))
           (funcall ins "t" '(a . 2) '(b . 1))
           (funcall del "t" '(a . 1))
           (funcall ins "t" '(a . 1) '(b . 1))
           (funcall del "t" '(b . 1))
           (funcall ins "t" '(a . 3) '(b . 1)))
     '(:present (("t" ((a . 3) (b . 1)) 1))
       :absent (("t" ((a . 1)) 0) ("t" ((b . 1)) 1))))))

(defun dorg-test-rowchecks--random-effects (state length)
  "Return (EFFECTS . STATE): LENGTH pseudo-random effects from seed STATE.
Two tables, three columns over a small domain including nil; inserts carry
a random column subset in random order, repeats included; deletes carry a
random predicate, possibly empty, with missing, nil-valued, duplicated and
reordered columns."
  (let (effects)
    (cl-flet ((next (n)
                (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
                (mod (/ state 65536) n)))
      (dotimes (_ length)
        (let* ((table (nth (next 2) '("t1" "t2")))
               (cells (cl-loop repeat (next 4)
                               collect (cons (nth (next 3) '(a b c))
                                             (nth (next 4) '(0 1 2 nil))))))
          (push (if (< (next 3) 2)
                    (list :table table :insert (cl-remove-duplicates
                                                cells :key #'car :from-end t))
                  (list :table table :delete cells))
                effects))))
    (cons (nreverse effects) state)))

(ert-deftest dorg-test-rowchecks-random-sequences-match-the-reference ()
  "Seeded random effect sequences plan exactly the reference checks."
  (let ((state 20261022))
    (dotimes (_ 400)
      (pcase-let ((`(,effects . ,next) (dorg-test-rowchecks--random-effects
                                        state (1+ (mod state 40)))))
        (setq state next)
        (should (equal (dorg-test-rowchecks--reference (copy-tree effects))
                       (dorg-mutate--postimage-row-checks (copy-tree effects))))))))

;;; ─── Work ────────────────────────────────────────────────────────────

(defmacro dorg-test-rowchecks--counting (&rest body)
  "Run BODY; return (VALUE MATCHES SCANNED).
MATCHES counts calls of the row-match predicate; SCANNED sums the length of
every sequence searched with `cl-find-if'.  Both are deterministic units of
planning work, independent of the host."
  (declare (indent 0))
  `(let ((matches 0) (scanned 0))
     (cl-flet ((count-match (&rest _) (cl-incf matches))
               (count-scan (_pred seq &rest _) (cl-incf scanned (length seq))))
       (advice-add 'dorg-mutate--postimage-row-matches-p :before #'count-match)
       (advice-add 'cl-find-if :before #'count-scan)
       (unwind-protect
           (list (progn ,@body) matches scanned)
         (advice-remove 'dorg-mutate--postimage-row-matches-p #'count-match)
         (advice-remove 'cl-find-if #'count-scan)))))

(defun dorg-test-rowchecks--keys (n sign)
  "Return the one-cell rows ((k . I)) for I in 1..N, times SIGN."
  (mapcar (lambda (i) (list (cons 'k (* sign i)))) (number-sequence 1 n)))

(ert-deftest dorg-test-rowchecks-delete-work-is-linear ()
  "N inserts then a delete of each cost at most 4N row matches."
  (dolist (n '(500 1000 2000))
    (pcase-let ((`(,checks ,matches ,_)
                 (dorg-test-rowchecks--counting
                   (dorg-mutate--postimage-row-checks
                    (append (mapcar (lambda (row) (list :table "t" :insert row))
                                    (dorg-test-rowchecks--keys n 1))
                            (mapcar (lambda (row) (list :table "t" :delete row))
                                    (dorg-test-rowchecks--keys n 1)))))))
      (should (equal checks
                     (list :present nil
                           :absent (mapcar (lambda (row) (list "t" row 0))
                                           (dorg-test-rowchecks--keys n 1)))))
      (should (<= matches (* 4 n))))))

(ert-deftest dorg-test-rowchecks-present-work-is-linear ()
  "N distinct inserts cost no row matches and no growing list scan."
  (dolist (n '(500 1000 2000))
    (pcase-let ((`(,checks ,matches ,scanned)
                 (dorg-test-rowchecks--counting
                   (dorg-mutate--postimage-row-checks
                    (mapcar (lambda (row) (list :table "t" :insert row))
                            (dorg-test-rowchecks--keys n 1))))))
      (should (equal checks
                     (list :present (mapcar (lambda (row) (list "t" row 1))
                                            (dorg-test-rowchecks--keys n 1))
                           :absent nil)))
      (should (= 0 matches))
      (should (<= scanned (* 4 n))))))

(ert-deftest dorg-test-rowchecks-absent-work-is-linear ()
  "N deletes before N surviving inserts cost at most 4N row matches."
  (dolist (n '(500 1000 2000))
    (pcase-let ((`(,checks ,matches ,_)
                 (dorg-test-rowchecks--counting
                   (dorg-mutate--postimage-row-checks
                    (append (mapcar (lambda (row) (list :table "t" :delete row))
                                    (dorg-test-rowchecks--keys n -1))
                            (mapcar (lambda (row) (list :table "t" :insert row))
                                    (dorg-test-rowchecks--keys n 1)))))))
      (should (equal checks
                     (list :present (mapcar (lambda (row) (list "t" row 1))
                                            (dorg-test-rowchecks--keys n 1))
                           :absent (mapcar (lambda (row) (list "t" row 0))
                                           (dorg-test-rowchecks--keys n -1)))))
      (should (<= matches (* 4 n))))))

(provide 'dorg-postimage-row-checks-tests)

;;; dorg-postimage-row-checks-tests.el ends here
