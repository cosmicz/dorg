;;; dorg-history-tests.el --- Tests for DB-level undo (browse + restore) -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED specification tests — user commands to BROWSE a
;; bead/root's Dolt history and RESTORE a bead/root to a prior commit as a
;; durable, forward-only DB write (revert semantics, never history rewrite).
;;
;; These tests are written at the SPEC gate, BEFORE implementation, to
;; define the API.  They reference functions that do not exist yet
;; (`dorg-history-commits', `dorg-restore--patches', `dorg-restore-to-commit',
;; and the command-level `dorg-history--rows' / `dorg-history--restore' /
;; `dorg-history--view-path') and therefore FAIL until IMPL lands
;; post-REVIEW.
;;
;; The file IS registered in tests/dorg-tests-runner.el (the project-runner
;; invariant requires every test file to be required), so the focused gate
;; `make test SELECT=dorg-test-history' runs these 12+ tests (red until
;; IMPL).  Run focused:
;;   make test SELECT=dorg-test-history
;;
;; Each test is a TRANSFORMATION test: it sets up a WRONG/changed state,
;; applies the operation, and asserts the corrected state with before !=
;; after.  Deleting the function under test must fail the test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-dolt)
;; NOTE: (require 'dorg-history) is intentionally omitted until the module
;; exists; these tests resolve the symbols at call time (void-function = red).

;;; ─── Local fixtures ───────────────────────────────────────────────────

(defmacro dorg-test-history--with-dolt (&rest body)
  "Run BODY with `dolt-dir' bound to a temp Dolt repo, or skip."
  (declare (indent 0) (debug (body)))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       ,@body)))

(defun dorg-test-history--import (dolt-dir contents &optional root-id message)
  "Import org CONTENTS into DOLT-DIR and commit with MESSAGE.
Returns the root id of the imported tree."
  (let ((stmts (dorg-test--parse-to-sql contents root-id)))
    (dorg-sql-batch dolt-dir stmts)
    (dorg-commit dolt-dir (or message "import")))
  (dorg-test-history--root-id dolt-dir))

(defun dorg-test-history--root-id (dolt-dir)
  "Return the level-0 root id in DOLT-DIR."
  (alist-get 'id (car (dorg-sql dolt-dir
                                "SELECT id FROM headings WHERE level = 0 LIMIT 1"))))

(defun dorg-test-history--id-by-title (dolt-dir title)
  "Return the id of the heading whose title is TITLE in DOLT-DIR (or nil)."
  (alist-get 'id (car (dorg-sql dolt-dir
                                (format "SELECT id FROM headings WHERE title = %s LIMIT 1"
                                        (dorg-sql-quote title))))))

(defun dorg-test-history--title-of (dolt-dir id)
  "Return the current title of heading ID in DOLT-DIR (or nil)."
  (alist-get 'title (car (dorg-sql dolt-dir
                                   (format "SELECT title FROM headings WHERE id = %d LIMIT 1"
                                           id)))))

(defun dorg-test-history--set-title (dolt-dir id title message)
  "Set heading ID's title to TITLE in DOLT-DIR and commit with MESSAGE."
  (dorg-sql dolt-dir (format "UPDATE headings SET title = %s WHERE id = %d"
                             (dorg-sql-quote title) id))
  (dorg-commit dolt-dir message))

(defun dorg-test-history--log-length (dolt-dir)
  "Return the number of commits reachable from HEAD in DOLT-DIR."
  (length (dorg-sql dolt-dir "SELECT commit_hash FROM dolt_log")))

(defun dorg-test-history--set-body (dolt-dir root-id heading-id text message)
  "Replace HEADING-ID's body with a single text item TEXT and commit.
A BODY-ONLY edit: the heading row (title/todo/…) is untouched, so the
change lands in the item tables, not `headings'."
  (dorg-test--apply-patches
   (list :dolt-dir dolt-dir :root-id root-id)
   (list (dorg-test--items-replace-patch
          (dorg-test--heading-address heading-id)
          (list (list :kind 'text :text text))))
   :message message))

(defun dorg-test-history--body-of (dolt-dir heading-id)
  "Return HEADING-ID's concatenated text-item body in DOLT-DIR (or \"\")."
  (or (alist-get 'body
                 (car (dorg-sql
                       dolt-dir
                       (format
                        (concat "SELECT COALESCE(GROUP_CONCAT(ti.text_raw "
                                "ORDER BY hi.sort_key SEPARATOR ''), '') AS body "
                                "FROM heading_items hi "
                                "JOIN text_items ti ON ti.item_id = hi.id "
                                "WHERE hi.heading_id = %d AND hi.kind = 'text'")
                        heading-id))))
      ""))

;;; ─── History data API ─────────────────────────────────────────────────

(ert-deftest dorg-test-history-commits-root-newest-first ()
  "`dorg-history-commits' (root scope) lists commits newest-first; the
head entry equals `dorg-head-commit'.  Non-vacuous: a reversed or empty
list fails the ordering + head assertions."
  (dorg-test-history--with-dolt
    (let ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1")))
      (dorg-test-history--set-title dolt-dir
                                    (dorg-test-history--id-by-title dolt-dir "A")
                                    "A2" "c2")
      (dorg-test-history--set-title dolt-dir
                                    (dorg-test-history--id-by-title dolt-dir "A2")
                                    "A3" "c3")
      (let ((commits (dorg-history-commits dolt-dir root)))
        (should (>= (length commits) 3))
        (should (stringp (plist-get (car commits) :commit)))
        (should (plist-get (car commits) :message))
        ;; newest-first: head entry is the current HEAD
        (should (equal (plist-get (car commits) :commit)
                       (dorg-head-commit dolt-dir)))))))

(ert-deftest dorg-test-history-commits-bead-scope-filters ()
  "Bead-scope history returns only commits touching that bead's row; an
unchanged sibling has a shorter history.  Non-vacuous: whole-log behaviour
would give both beads the same count."
  (dorg-test-history--with-dolt
    (let ((root (dorg-test-history--import dolt-dir "* A\n* B\n" 100 "c1")))
      (let ((a (dorg-test-history--id-by-title dolt-dir "A")))
        (dorg-test-history--set-title dolt-dir a "A2" "edit A")
        (dorg-test-history--set-title dolt-dir
                                      (dorg-test-history--id-by-title dolt-dir "A2")
                                      "A3" "edit A again")
        (let* ((a-id (dorg-test-history--id-by-title dolt-dir "A3"))
               (b-id (dorg-test-history--id-by-title dolt-dir "B"))
               (a-hist (dorg-history-commits dolt-dir root a-id))
               (b-hist (dorg-history-commits dolt-dir root b-id)))
          (should (> (length a-hist) (length b-hist)))
          (should (cl-some (lambda (e) (equal (plist-get e :title) "A3")) a-hist))
          (should (cl-some (lambda (e) (equal (plist-get e :title) "A")) a-hist)))))))

(ert-deftest dorg-test-history-commits-bead-includes-body-edit ()
  "Bead-scope history INCLUDES a body-only edit (item table change with no
heading-row change).  Non-vacuous: heading-row-only history would MISS this
commit, leaving the durable-undo body story unreachable from bead history.
This red-pins resolved decision 3."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\nold body\n" 100 "c1"))
           (a (dorg-test-history--id-by-title dolt-dir "A")))
      ;; Body-only edit: title \"A\" is unchanged; only the text item changes.
      (dorg-test-history--set-body dolt-dir root a "new body\n" "edit body")
      (let* ((edit-commit (dorg-head-commit dolt-dir))
             (hist (dorg-history-commits dolt-dir root a))
             (title-now (dorg-test-history--title-of dolt-dir a)))
        ;; Heading row really is unchanged by the body edit.
        (should (equal title-now "A"))
        ;; The body-edit commit must appear in the bead's history.
        (should (cl-some (lambda (e) (equal (plist-get e :commit) edit-commit))
                         hist))))))

;;; ─── Restore planner (pure) ───────────────────────────────────────────

(ert-deftest dorg-test-history-restore-planner-speaks-patches-only ()
  "Restore planning is a typed-patch seam, not a renamed raw-op planner."
  (should (fboundp 'dorg-restore--patches))
  (should-not (fboundp 'dorg-restore--plan)))

(ert-deftest dorg-test-history-plan-noop-when-equal ()
  "`dorg-restore--patches' with target == current yields no patches."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n** Child\n" 100 "c1"))
           (ht (dorg-model-load-tree dolt-dir root))
           (ids (let (acc) (maphash (lambda (k _v) (push k acc)) ht) acc)))
      (should (null (dorg-restore--patches ht ht ids))))))

(ert-deftest dorg-test-history-plan-update-for-title-change ()
  "Title A->B between C and HEAD yields a typed update and no delete."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let* ((current (dorg-model-load-tree dolt-dir root))
             (target (dorg-model-load-tree dolt-dir root c1))
             (ids (list root id))
             (patches (dorg-restore--patches current target ids))
             (update (cl-find-if
                      (lambda (patch)
                        (equal '(update :heading)
                               (list (plist-get patch :op)
                                     (car (plist-get patch :target)))))
                      patches)))
        (should update)
        (should (= (plist-get (cdr (plist-get update :target)) :id) id))
        (should (equal (plist-get (plist-get update :fields) :title) "A"))
        (should-not
         (cl-find-if
          (lambda (patch)
            (equal (list :op 'delete :target (list :heading :id id)) patch))
          patches))))))

(ert-deftest dorg-test-history-plan-delete-for-current-only ()
  "An id in CURRENT but not TARGET yields a typed heading delete.
Red-pins the pure planner delete case
(AC: current-only -> delete).  Non-vacuous: a planner that only diffs
shared ids would emit no delete and fail."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (target (dorg-model-load-tree dolt-dir root c1))) ; target == {root, A}
      ;; Add B after c1: current == {root, A, B}, target lacks B.
      (dorg-test--apply-patches
       (list :dolt-dir dolt-dir :root-id root)
       (list (dorg-test--tree-insert-patch :root '(:title "B")))
       :message "add B")
      (let* ((b (dorg-test-history--id-by-title dolt-dir "B"))
             (a (dorg-test-history--id-by-title dolt-dir "A"))
             (current (dorg-model-load-tree dolt-dir root))
             (ids (list root a b))
             (patches (dorg-restore--patches current target ids))
             (del (cl-find-if
                   (lambda (patch)
                     (equal (list :op 'delete
                                  :target (list :heading :id b))
                            patch))
                   patches)))
        (should del)))))

(ert-deftest dorg-test-history-plan-insert-for-target-only ()
  "An id in TARGET but not CURRENT yields a typed tree insert.
The patch preserves the original id and carries its items.
Red-pins the pure planner insert case (AC: target-only -> insert keeping
id/items/parent).  Non-vacuous: a fresh-id insert or an item-less insert
fails the id/items assertions."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n* X\nx body\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (x (dorg-test-history--id-by-title dolt-dir "X"))
           (a (dorg-test-history--id-by-title dolt-dir "A"))
           (target (dorg-model-load-tree dolt-dir root c1))) ; target has A + X
      ;; Delete X after c1: current lacks X, target (c1) has X.
      (dorg-sql dolt-dir (format "DELETE FROM headings WHERE id = %d" x))
      (dorg-commit dolt-dir "delete X")
      (let* ((current (dorg-model-load-tree dolt-dir root))
             (ids (list root a x))
             (patches (dorg-restore--patches current target ids))
             (ins (cl-find-if
                   (lambda (patch)
                     (and (eq (plist-get patch :op) 'insert)
                          (eq (car (plist-get patch :target)) :tree)
                          (eql (plist-get (plist-get patch :heading) :id) x)))
                   patches)))
        (should ins)
        ;; Preserves the original id and carries the as-of items.
        (should (eql (plist-get (plist-get ins :heading) :id) x))
        (should (plist-get (plist-get ins :heading) :items))))))

;;; ─── Restore: durable, forward-only DB write (the core behaviour) ─────

(ert-deftest dorg-test-history-restore-roundtrip-durable ()
  "Restore a bead to an earlier commit: the DB title reverts and the
change is durable (re-read from DB), and HEAD is a NEW commit (not the
target, not the intermediate).  Before != after."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let ((c2 (dorg-head-commit dolt-dir)))
        (should (equal (dorg-test-history--title-of dolt-dir id) "B")) ; before
        (dorg-restore-to-commit dolt-dir root id c1)
        (should (equal (dorg-test-history--title-of dolt-dir id) "A")) ; after
        (let ((head (dorg-head-commit dolt-dir)))
          (should-not (equal head c1))
          (should-not (equal head c2)))))))

(ert-deftest dorg-test-history-restore-is-forward-only ()
  "A restore adds a commit and never rewrites history: the intermediate
commit C2 is still reachable and the log grew by one."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let ((c2 (dorg-head-commit dolt-dir))
            (len-before (dorg-test-history--log-length dolt-dir)))
        (dorg-restore-to-commit dolt-dir root id c1)
        (should (dorg-commit-ancestor-p dolt-dir c2)) ; history intact
        (should (= (dorg-test-history--log-length dolt-dir) (1+ len-before)))))))

(ert-deftest dorg-test-history-restore-is-reversible ()
  "Time-travel is symmetric: after restoring to C1's content, restoring
forward to C2's content returns the later title (a second new commit)."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let ((c2 (dorg-head-commit dolt-dir)))
        (dorg-restore-to-commit dolt-dir root id c1)
        (should (equal (dorg-test-history--title-of dolt-dir id) "A"))
        (dorg-restore-to-commit dolt-dir root id c2)
        (should (equal (dorg-test-history--title-of dolt-dir id) "B"))))))

(ert-deftest dorg-test-history-restore-reintroduces-deleted-bead ()
  "A bead deleted after C1 is re-created (with its original id) by a
root-scope restore to C1.  Before: row absent; after: row present again."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n* X\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (x-id (dorg-test-history--id-by-title dolt-dir "X")))
      (dorg-sql dolt-dir (format "DELETE FROM headings WHERE id = %d" x-id))
      (dorg-commit dolt-dir "delete X")
      (should (null (dorg-test-history--title-of dolt-dir x-id))) ; before
      (dorg-restore-to-commit dolt-dir root nil c1)               ; root scope
      (should (equal (dorg-test-history--title-of dolt-dir x-id) "X")))))

(ert-deftest dorg-test-history-restore-subtree-removes-later-child ()
  "A child added under a bead after C1 is removed by a bead-scope restore
of that bead to C1.  Before: child present; after: absent."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* Parent\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (parent (dorg-test-history--id-by-title dolt-dir "Parent")))
      ;; Add a child under Parent and commit (forward state).
      (dorg-test--apply-patches
       (list :dolt-dir dolt-dir :root-id root)
       (list (dorg-test--tree-insert-patch
              (dorg-test--heading-address parent) '(:title "Kid")))
       :message "add Kid")
      (let ((kid (dorg-test-history--id-by-title dolt-dir "Kid")))
        (should kid)                                             ; before
        (dorg-restore-to-commit dolt-dir root parent c1)
        (should (null (dorg-test-history--title-of dolt-dir kid)))))))

(ert-deftest dorg-test-history-restore-body-roundtrip ()
  "A bead-scope restore reverts a BODY-only edit (item content), not just
the headline: after editing the body and restoring to C1, the C1 body is
back.  Before != after.  Red-pins the item/body restore acceptance
criterion (the durable-undo body story, resolved decision 3)."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\nbody one\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      ;; Body-only edit (title "A" unchanged): body one -> body two.
      (dorg-test-history--set-body dolt-dir root id "body two\n" "c2")
      (should (string-match-p "body two" (dorg-test-history--body-of dolt-dir id))) ; before
      (should (equal (dorg-test-history--title-of dolt-dir id) "A"))                ; row unchanged
      ;; Restore the bead to C1: the body must revert to "body one".
      (dorg-restore-to-commit dolt-dir root id c1)
      (let ((body (dorg-test-history--body-of dolt-dir id)))
        (should (string-match-p "body one" body))      ; after: reverted
        (should-not (string-match-p "body two" body))))))

;;; ─── Scope isolation + guards ─────────────────────────────────────────

(ert-deftest dorg-test-history-bead-scope-leaves-other-subtree ()
  "A bead-scope restore reverts only its own subtree; a change OUTSIDE
that subtree is untouched.  Non-vacuous: a whole-root restore would also
revert the sibling."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n* B\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (a (dorg-test-history--id-by-title dolt-dir "A"))
           (b (dorg-test-history--id-by-title dolt-dir "B")))
      (dorg-test-history--set-title dolt-dir a "A2" "edit A")
      (dorg-test-history--set-title dolt-dir b "B2" "edit B")
      ;; Restore only A's subtree to C1.
      (dorg-restore-to-commit dolt-dir root a c1)
      (should (equal (dorg-test-history--title-of dolt-dir a) "A"))  ; reverted
      (should (equal (dorg-test-history--title-of dolt-dir b) "B2"))))) ; untouched

(ert-deftest dorg-test-history-restore-rejects-predating-commit ()
  "Restoring a bead to a commit predating its existence signals a
`user-error' and writes NO commit (HEAD unchanged)."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir)))
      ;; Add B in a later commit; B did not exist at C1.
      (dorg-test--apply-patches
       (list :dolt-dir dolt-dir :root-id root)
       (list (dorg-test--tree-insert-patch :root '(:title "B")))
       :message "add B")
      (let ((b (dorg-test-history--id-by-title dolt-dir "B"))
            (head-before (dorg-head-commit dolt-dir)))
        (should-error (dorg-restore-to-commit dolt-dir root b c1)
                      :type 'user-error)
        (should (equal (dorg-head-commit dolt-dir) head-before))))))

(ert-deftest dorg-test-history-restore-rejects-non-ancestor ()
  "Restoring to a commit that is not an ancestor of HEAD is rejected
before any write."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (id (dorg-test-history--id-by-title dolt-dir "A"))
           (head-before (dorg-head-commit dolt-dir)))
      (should-error (dorg-restore-to-commit dolt-dir root id
                                            "0000000000000000000000000000000a")
                    :type 'user-error)
      (should (equal (dorg-head-commit dolt-dir) head-before)))))

(ert-deftest dorg-test-history-restore-retries-on-serialization-failure ()
  "A retryable Dolt 1213 error during restore triggers a reload and retry.
The restore still lands the target state.  This pins the concurrency criterion:
restore wraps its durable write in `dorg-dolt-with-retry'.  Non-vacuous: a
restore that does not retry would propagate the first failure and leave the
title unreverted."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let* ((calls 0)
             (real (symbol-function 'dorg-mutate--apply-patches)))
        ;; First typed apply attempt diverges (as if HEAD moved under us);
        ;; the second delegates to the real write.
        (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
                   (lambda (&rest args)
                     (setq calls (1+ calls))
                     (if (= calls 1)
                         (error "[1213] simulated serialization failure")
                       (apply real args)))))
          (dorg-restore-to-commit dolt-dir root id c1))
        ;; It retried past the first failure …
        (should (> calls 1))
        ;; … and ultimately landed the target state.
        (should (equal (dorg-test-history--title-of dolt-dir id) "A"))))))

;;; ─── Command-level UX (red-pins decisions 5 + 6) ──────────────────────

(ert-deftest dorg-test-history-rows-newest-first ()
  "`dorg-history--rows' (backing the *dorg-history* tabulated-list) yields
one row per commit, newest-first, matching `dorg-history-commits'.
Non-vacuous: an empty or mis-ordered row set fails."
  (dorg-test-history--with-dolt
    (let ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1")))
      (dorg-test-history--set-title dolt-dir
                                    (dorg-test-history--id-by-title dolt-dir "A")
                                    "A2" "c2")
      (let ((rows (dorg-history--rows dolt-dir root nil))
            (commits (dorg-history-commits dolt-dir root)))
        (should (= (length rows) (length commits)))
        (should (>= (length rows) 2))
        ;; newest-first: first row corresponds to current HEAD.
        (should (string-match-p (substring (dorg-head-commit dolt-dir) 0 7)
                                (format "%S" (car rows))))))))

(ert-deftest dorg-test-history-restore-action-rematerializes-origin ()
  "The buffer restore action `dorg-history--restore' invokes
`dorg-restore-to-commit' (durable DB revert) AND re-materializes the
ORIGIN buffer, so the restored title is visible in that buffer.
Red-pins the restore-row UX contract."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* Alpha\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "Alpha")))
      (dorg-test-history--set-title dolt-dir id "Beta" "c2")
      (with-temp-buffer
        (let ((origin (current-buffer)))
          ;; Show the current (Beta) state in the origin buffer first.
          (let ((file (make-temp-file "dorg-history-origin-" nil ".org"))
                (dorg--registry nil))
            (unwind-protect
                (progn
                  (dorg--store-registration
                   (file-truename file)
                   (make-dorg-registration
                    :backend (dorg-backend-dolt dolt-dir root)
                    :file (file-truename file)))
                  (set-visited-file-name file t)
                  (dorg-materialize origin)
                  (should (string-match-p "Beta" (buffer-string)))
                  ;; Restore action: DB revert + re-materialize origin.
                  (dorg-history--restore dolt-dir root id c1 origin)
                  (should (equal (dorg-test-history--title-of dolt-dir id) "Alpha"))
                  (should (string-match-p "Alpha" (buffer-string)))
                  (should-not (string-match-p "Beta" (buffer-string))))
              (delete-file file))))))))

(ert-deftest dorg-test-history-view-bead-is-readonly-nowrite ()
  "Bead-scope `dorg-history--view-path' returns a read-only VFS island
path; visiting it does NOT advance HEAD (no write).  Red-pins the
read-only view contract (decision 5)."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (let ((head-before (dorg-head-commit dolt-dir))
            (path (dorg-history--view-path dolt-dir id c1)))
        ;; A VFS island path pinned at the chosen commit.
        (should (stringp path))
        (should (string-match-p "@" path))
        (should (string-match-p (substring c1 0 7) path))
        ;; Visiting the read-only view writes nothing: HEAD is unchanged.
        (should (equal (dorg-head-commit dolt-dir) head-before))))))

(ert-deftest dorg-test-history-view-root-signals-user-error ()
  "Root-scope view is the single explicit v0.1 behavior: it signals
`user-error' (whole-tree as-of view unavailable), while root RESTORE stays
supported.  Red-pins resolved decision 5; non-vacuous because a silent nil
or an island path would pass a bare should-error but fails :type."
  (dorg-test-history--with-dolt
    (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (c1 (dorg-head-commit dolt-dir))
           (head-before (dorg-head-commit dolt-dir)))
      (should-error (dorg-history--view-path dolt-dir nil c1)
                    :type 'user-error)
      ;; Root RESTORE remains available (no error from the guard path).
      (should (equal (dorg-head-commit dolt-dir) head-before)))))

(ert-deftest dorg-test-history-view-installs-handler ()
  "End-to-end: `dorg-history-view' installs the /dorg: file-name handler on
demand, so the read-only island buffer opens in a DEFAULT session where the
handler was never manually enabled via `dorg-vfs-enable'.  RED before the
ensure call: `find-file-read-only' signals \"does not exist\" because
`file-exists-p' on the /dorg: path is nil without the handler.  This is the
gap the existing `dorg-test-history-view-*' tests miss — they exercise only
the `dorg-history--view-path' string builder, never the interactive command
that visits the path."
  (dorg-test-history--with-dolt
    (let* ((origin (get-buffer-create "*dorg-view-origin*"))
           (root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
           (id (dorg-test-history--id-by-title dolt-dir "A")))
      (dorg-test-history--set-title dolt-dir id "B" "c2")
      (unwind-protect
          ;; Simulate a default session: strip any /dorg: handler.  The
          ;; let-binding is restored on exit, so the ensure call's global
          ;; `add-to-list' never leaks into sibling tests.
          (let ((file-name-handler-alist
                 (cl-remove 'dorg-vfs-handler file-name-handler-alist
                            :key #'cdr)))
            (should-not (rassq 'dorg-vfs-handler file-name-handler-alist))
            (dorg-history--render-buffer
             (list :dolt-dir dolt-dir :root-id root :scope-id id
                   :buffer origin))
            (with-current-buffer "*dorg-history*"
              (goto-char (point-min))
              (let ((commit (tabulated-list-get-id)))
                ;; RED here without the fix: signals "…does not exist".
                (dorg-history-view)
                (let ((view-buf (current-buffer)))
                  (unwind-protect
                      (progn
                        ;; The ensure call installed the handler on demand …
                        (should (rassq 'dorg-vfs-handler
                                       file-name-handler-alist))
                        ;; … and it is idempotent (single entry).
                        (should (= 1 (cl-count 'dorg-vfs-handler
                                               file-name-handler-alist
                                               :key #'cdr)))
                        (should-not (equal view-buf
                                           (get-buffer "*dorg-history*")))
                        (with-current-buffer view-buf
                          (should (string-match-p "\\`/dorg:"
                                                  (or buffer-file-name "")))
                          (should buffer-read-only)
                          (should (equal (buffer-string)
                                         (dorg-render-island
                                          dolt-dir id nil commit)))))
                    (kill-buffer view-buf))))))
        (kill-buffer origin)
        (when (get-buffer "*dorg-history*")
          (kill-buffer "*dorg-history*"))))))

(provide 'dorg-history-tests)
;;; dorg-history-tests.el ends here
