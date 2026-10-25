;;; dorg-postimage-partitions-tests.el --- Batched postimage partition proof -*- lexical-binding: t; -*-

;;; Commentary:

;; The commit-time proof reads back every affected owner partition and checks
;; four facts per heading: it is present under its root, its stored item
;; digest equals a recomputation, its canonical row equals the candidate's,
;; and its persisted item identity equals the candidate's.  These specs pin a
;; proof that reads each root's partitions as one union instead of one model
;; load per partition: the exact requests it sends below the bounded read
;; wrapper, image equality with per-partition loads on a real store, the
;; isolation and order of the four checks, and rollback on a failure.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-test-helpers)
(require 'dorg-mutate-tests)
(require 'dorg-2ij34-postimage-tests)

;;; ─── Synthetic partitions ───────────────────────────────────────────

(defun dorg-test-partitions--heading (id &rest overrides)
  "Return a fresh heading ID with one text item and its computed digest.
OVERRIDES may set :title, :item-id, :sort-key or :stale-hash (a stored
digest left unrecomputed)."
  (let ((h (make-dorg-heading
            :id id :title (or (plist-get overrides :title) (format "t%d" id))
            :items (list (dorg-text-item :id (or (plist-get overrides :item-id) (+ 100000 id))
                                         :sort-key (or (plist-get overrides :sort-key)
                                                       (unibyte-string #x80))
                                         :text (format "body %d" id))))))
    (dorg-heading-compute-items-hash h)
    (when (plist-get overrides :stale-hash)
      (setf (dorg-heading-items-hash h) (plist-get overrides :stale-hash)))
    h))

(defun dorg-test-partitions--spec (root owner ids &optional deleted)
  "Return an owner spec for ROOT and OWNER over IDS, expecting intact headings."
  (let ((expected (make-hash-table :test 'eql)))
    (dolist (id ids)
      (let ((h (dorg-test-partitions--heading id)))
        (puthash id (cons (dorg-mutate--index-heading-canonical h)
                          (dorg-mutate--postimage-item-row-canonical h))
                 expected)))
    (list root owner ids expected deleted)))

(defun dorg-test-partitions--port (answer)
  "Return (PORT . LOG): PORT answers owner-model requests through ANSWER.
ANSWER is called with (ROOT ID) and returns a fresh heading or nil.  LOG
records every request the port receives, newest first."
  (let ((log (list nil)))
    (cons (lambda (request)
            (push (copy-tree request) (car log))
            (should (eq 'owner-model (plist-get request :op)))
            (let ((image (make-hash-table :test 'eql)))
              (dolist (id (plist-get request :ids))
                (when-let ((h (funcall answer (plist-get request :root-id) id)))
                  (puthash id h image)))
              image))
          log)))

(defun dorg-test-partitions--intact (_root id)
  "Answer every requested ID with an intact heading."
  (dorg-test-partitions--heading id))

(defun dorg-test-partitions--prove (answer specs)
  "Prove SPECS through the production proof and wrapper over ANSWER.
Return (OUTCOME . REQUESTS): OUTCOME is `:passed' or the parity data."
  (pcase-let ((`(,port . ,log) (dorg-test-partitions--port answer)))
    (cons (condition-case err
              (progn (dorg-mutate--postimage-assert-partitions
                      (dorg-mutate--postimage-read-batched port) specs)
                     :passed)
            (dorg-sql-postimage-parity (cdr err)))
          (reverse (car log)))))

(defun dorg-test-partitions--union (specs)
  "Return ((ROOT . IDS) ...) for SPECS: each root's present ids, once each."
  (let (roots)
    (pcase-dolist (`(,root ,_owner ,ids ,_expected ,deleted) specs)
      (unless deleted
        (let ((cell (or (assq root roots) (car (push (list root) roots)))))
          (dolist (id ids)
            (unless (memq id (cdr cell))
              (setcdr cell (append (cdr cell) (list id))))))))
    (nreverse roots)))

(ert-deftest dorg-test-partitions-requests-cover-each-root-union-exactly ()
  "Below the wrapper, each root's present partitions are read as one union.
The requests for a root carry exactly its deduplicated present ids, each at
most 1024 and non-empty, under that root, so a root costs ceil(ids/1024)
requests however many partitions it has.  No specs and all-deleted specs
read nothing."
  (cl-flet ((range (from to) (number-sequence from to))
            (chunks (ids size) (cl-loop while ids
                                        collect (seq-take ids size)
                                        do (setq ids (nthcdr size ids)))))
    (dolist (specs
             (list
              ;; 1023, 1024, 1025 and 2100 unique ids, overlapping specs.
              (list (dorg-test-partitions--spec 1 1 (range 1 600))
                    (dorg-test-partitions--spec 1 2 (range 500 1023))
                    (dorg-test-partitions--spec 1 3 (range 1 10)))
              (list (dorg-test-partitions--spec 1 1 (range 1 1024)))
              (list (dorg-test-partitions--spec 1 1 (range 1 512))
                    (dorg-test-partitions--spec 1 2 (range 513 1025)))
              (mapcar (lambda (ids) (dorg-test-partitions--spec 1 (car ids) ids))
                      (chunks (range 1 2100) 7))
              ;; Two roots, interleaved, with a deleted spec.
              (list (dorg-test-partitions--spec 1 1 (range 1 5))
                    (dorg-test-partitions--spec 2 6 (range 6 9))
                    (dorg-test-partitions--spec 1 10 (range 10 12))
                    (dorg-test-partitions--spec 2 13 (range 13 14) t)
                    (dorg-test-partitions--spec 2 15 (range 15 16)))
              ;; One-id partitions.
              (mapcar (lambda (id) (dorg-test-partitions--spec 1 id (list id)))
                      (range 1 2000))
              nil
              (list (dorg-test-partitions--spec 1 1 (range 1 3) t))))
      (pcase-let* ((`(,outcome . ,requests)
                    (dorg-test-partitions--prove #'dorg-test-partitions--intact specs))
                   (union (dorg-test-partitions--union specs)))
        (should (eq :passed outcome))
        (dolist (request requests)
          (should (plist-get request :ids))
          (should (<= (length (plist-get request :ids)) 1024)))
        (should (equal (mapcar (lambda (cell) (cons (car cell) (sort (copy-sequence (cdr cell)) #'<)))
                               union)
                       (mapcar (lambda (cell)
                                 (cons (car cell)
                                       (sort (cl-loop for r in requests
                                                      when (eql (car cell) (plist-get r :root-id))
                                                      append (copy-sequence (plist-get r :ids)))
                                             #'<)))
                               union)))
        (should (= (length requests)
                   (apply #'+ (mapcar (lambda (cell) (ceiling (length (cdr cell)) 1024.0))
                                      union))))))))

;; Snapshot every spec's ids and expectation entries, to prove the proof
;; leaves its inputs untouched.
(defun dorg-test-partitions--spec-contents (specs)
  "Return SPECS' ids and expectation entries as plain, comparable data."
  (mapcar (lambda (spec)
            (list (copy-sequence (nth 2 spec))
                  (let (entries)
                    (maphash (lambda (id row) (push (cons id (copy-tree row)) entries))
                             (nth 3 spec))
                    (sort entries (lambda (a b) (< (car a) (car b)))))))
          specs))

(ert-deftest dorg-test-partitions-equal-root-ids-share-one-union ()
  "Equal root ids form one union even when they are distinct objects.
Root ids are BIGINT values decoded row by row, so two specs of one root can
carry ids that are `eql' but not `eq'.  Interleaved with another root, each
root is still read as exactly one deduplicated union under its own root,
and the specs' ids and expectations are left unchanged."
  (let* ((big-a (string-to-number "9223372036854775800"))
         (big-b (string-to-number "9223372036854775800"))
         (specs (list (dorg-test-partitions--spec big-a 1 '(1 2 3))
                      (dorg-test-partitions--spec 7 4 '(4 5))
                      (dorg-test-partitions--spec big-b 6 '(3 6))
                      (dorg-test-partitions--spec 7 8 '(5 9))))
         (before (dorg-test-partitions--spec-contents specs)))
    (should (eql big-a big-b))
    (should-not (eq big-a big-b))
    (pcase-let ((`(,outcome . ,requests)
                 (dorg-test-partitions--prove #'dorg-test-partitions--intact specs)))
      (should (eq :passed outcome))
      (should (= 2 (length requests)))
      (should (eql big-a (plist-get (nth 0 requests) :root-id)))
      (should (equal '(1 2 3 6) (plist-get (nth 0 requests) :ids)))
      (should (eql 7 (plist-get (nth 1 requests) :root-id)))
      (should (equal '(4 5 9) (plist-get (nth 1 requests) :ids))))
    (should (equal before (dorg-test-partitions--spec-contents specs)))))

;;; ─── The four checks ────────────────────────────────────────────────

(defun dorg-test-partitions--reference (read owner-specs)
  "Assert OWNER-SPECS through READ one partition load at a time.
A frozen copy of the per-partition proof, kept as the oracle."
  (pcase-dolist (`(,root-id ,_owner-id ,ids ,expected ,deleted) owner-specs)
    (unless deleted
      (let ((image (funcall read
                            (list :version 1 :op 'owner-model
                                  :root-id root-id :ids ids))))
        (dolist (id ids)
          (let ((h (gethash id image)))
            (unless h
              (dorg-mutate--postimage-abort
               'core
               "a partition row is missing or misplaced at readback"
               :ids (list id) :root root-id))
            (let ((stored (dorg-heading-items-hash h)))
              (dorg-heading-compute-items-hash h)
              (unless (equal stored (dorg-heading-items-hash h))
                (dorg-mutate--postimage-abort
                 'core
                 "committed item rows diverge from their stored digest"
                 :ids (list id))))
            (let ((expected-row (gethash id expected)))
              (unless (equal (car expected-row)
                             (dorg-mutate--index-heading-canonical h))
                (dorg-mutate--postimage-abort
                 'core
                 "readback model diverges from the validated candidate"
                 :ids (list id)))
              (unless (equal (cdr expected-row)
                             (dorg-mutate--postimage-item-row-canonical h))
                (dorg-mutate--postimage-abort
                 'core
                 "committed item row identity diverges from the validated candidate"
                 :ids (list id))))))))))

(defun dorg-test-partitions--oracle (answer specs)
  "Return the reference proof's outcome over ANSWER for SPECS."
  (condition-case err
      (progn (dorg-test-partitions--reference
              (dorg-mutate--postimage-read-batched (car (dorg-test-partitions--port answer)))
              specs)
             :passed)
    (dorg-sql-postimage-parity (cdr err))))

(ert-deftest dorg-test-partitions-each-check-aborts-first-in-spec-order ()
  "Each of the four checks fails alone, and the first failure in spec and id
order is the one reported.  Each corruption is placed in two partitions, the
later one first in id terms, and also in a different root, so reading or
checking in any other order would report another heading."
  (let* ((specs (list (dorg-test-partitions--spec 1 1 '(1 2 3))
                      (dorg-test-partitions--spec 2 4 '(40 41 42))
                      (dorg-test-partitions--spec 1 5 '(9 8 7))
                      (dorg-test-partitions--spec 1 6 '(5 6))))
         (homes '((40 . 2) (41 . 2) (42 . 2))))
    (pcase-dolist (`(,kind ,message . ,corrupt)
                   '((missing "a partition row is missing or misplaced at readback")
                     (misplaced "a partition row is missing or misplaced at readback")
                     (digest "committed item rows diverge from their stored digest")
                     (canonical "readback model diverges from the validated candidate")
                     (identity "committed item row identity diverges from the validated candidate")))
      (ignore corrupt)
      ;; Corrupt heading 8 (third spec, second id) and heading 5 (fourth
      ;; spec), and heading 42 in root 2 (second spec, last id).
      (let* ((bad '(42 8 5))
             (answer
              (lambda (root id)
                (let ((home (or (alist-get id homes) 1)))
                  (cond
                   ((not (memq id bad)) (and (= root home) (dorg-test-partitions--heading id)))
                   ((eq kind 'missing) nil)
                   ;; Stored under the other root: absent from its own.
                   ((eq kind 'misplaced) (and (/= root home) (dorg-test-partitions--heading id)))
                   ((not (= root home)) nil)
                   ((eq kind 'digest) (dorg-test-partitions--heading id :stale-hash "0badd16e57"))
                   ((eq kind 'canonical) (dorg-test-partitions--heading id :title "changed"))
                   ((eq kind 'identity) (dorg-test-partitions--heading id :item-id 999999))))))
             (expected (dorg-test-partitions--oracle answer specs))
             (actual (car (dorg-test-partitions--prove answer specs))))
        (should (equal message (car expected)))
        (should (equal '(42) (plist-get (cadr expected) :ids)))
        (should (equal expected actual))))
    ;; Each check on its own: only that check can see the corruption.
    (pcase-dolist (`(,overrides . ,message)
                   '(((:stale-hash "0badd16e57") . "committed item rows diverge from their stored digest")
                     ((:title "changed") . "readback model diverges from the validated candidate")
                     ((:item-id 999999) . "committed item row identity diverges from the validated candidate")
                     ((:sort-key "\x7f") . "committed item row identity diverges from the validated candidate")))
      (let* ((answer (lambda (_root id)
                       (if (= id 2)
                           (apply #'dorg-test-partitions--heading id overrides)
                         (dorg-test-partitions--heading id))))
             (one (list (dorg-test-partitions--spec 1 1 '(1 2 3)))))
        (should (equal (dorg-test-partitions--oracle answer one)
                       (car (dorg-test-partitions--prove answer one))))
        (should (equal message (car (car (dorg-test-partitions--prove answer one)))))))))

;;; ─── The real store ─────────────────────────────────────────────────

(define-error 'dorg-test-partitions-rollback "partition probe rollback")

(defun dorg-test-partitions--in-transaction (dolt-dir statements fn)
  "Run STATEMENTS on DOLT-DIR, then FN with the transaction's read port.
Return FN's value; the transaction always rolls back."
  (let (value)
    (condition-case nil
        (dorg-sql--with-lease (lease dolt-dir)
          (dorg-sql--lease-batch
           lease statements "partition probe"
           (list :mode 'transaction-read-port :extension-tables nil
                 :function (lambda (read)
                             (setq value (funcall fn read))
                             (signal 'dorg-test-partitions-rollback nil)))))
      (dorg-test-partitions-rollback nil))
    value))

(defun dorg-test-partitions--rich-text (count)
  "Return Org text for COUNT headings with tags, planning, drawers and keywords.
Titles and values include numeric-looking and non-ASCII text."
  (mapconcat
   (lambda (i)
     (format (concat "* TODO 00%d Tïtle %d :tag%d:x:\n"
                     "SCHEDULED: <2026-10-%02d>\n"
                     ":PROPERTIES:\n:KEY: 0.%d\n:EMPTY:\n:END:\n"
                     ":LOGBOOK:\n- note %d\n:END:\n"
                     "#+CAPTION: café %d\n"
                     "body ünïcode %d\n"
                     "%s")
             i i (mod i 5) (1+ (mod i 28)) i i i i
             (if (= 0 (mod i 9)) "** Nested child\nchild body\n" "")))
   (number-sequence 1 count) ""))

(defun dorg-test-partitions--root-ids (dolt-dir root)
  "Return ROOT's heading ids in DOLT-DIR, ascending, the root excluded."
  (mapcar (lambda (row) (alist-get 'id row))
          (dorg-sql dolt-dir (format "SELECT id FROM headings WHERE root_id = %d AND id <> %d ORDER BY id"
                                     root root))))

(defun dorg-test-partitions--specs-from-loads (read groups)
  "Return owner specs for GROUPS ((ROOT . IDS) ...) from per-partition loads.
Each expectation is taken from that partition's own owner-model image,
copied before any recomputation, so it records the stored digest."
  (cl-loop for (root . ids) in groups
           for owner from 1
           collect
           (let ((image (funcall read (list :version 1 :op 'owner-model :root-id root :ids ids)))
                 (expected (make-hash-table :test 'eql)))
             (dolist (id ids)
               (let ((h (dorg-heading-deep-copy (gethash id image))))
                 (puthash id (cons (dorg-mutate--index-heading-canonical h)
                                   (dorg-mutate--postimage-item-row-canonical h))
                          expected)))
             (list root owner ids expected nil))))

(defun dorg-test-partitions--groups (a-ids b-ids)
  "Return interleaved, overlapping partition groups over root 100's A-IDS and
root 900000's B-IDS: groups of seven sharing their last id with the next."
  (let (groups)
    (cl-loop for rest on a-ids by (lambda (l) (nthcdr 6 l))
             for i from 0
             do (push (cons 100 (seq-take rest 7)) groups)
             when (and (= 0 (mod i 40)) b-ids)
             do (push (cons 900000 (seq-take b-ids 3)) groups)
             (setq b-ids (nthcdr 2 b-ids)))
    (nreverse groups)))

(ert-deftest dorg-test-partitions-union-image-equals-partition-images-on-slices ()
  "On a real uncommitted postimage the union proof accepts exactly the rows
per-partition loads return.  Over 1024 headings of one root with tags,
planning, property and logbook drawers, keywords, text, nested children,
NULL, numeric-looking and non-ASCII values, plus a second root, partitions
overlap and interleave; the production proof, reading each root's union
through the real wrapper, passes every check against expectations taken
from each partition's own load before any recomputation."
  (dorg-test-mutate--with-dolt
    (unless (eq 'mysql (dorg-sql--transport-kind dolt-dir))
      (ert-skip "the transaction read port needs the server transport"))
    (dorg-test-mutate--seed dolt-dir (dorg-test-partitions--rich-text 1100) 100)
    (dorg-test-mutate--seed dolt-dir "* 007\n:PROPERTIES:\n:KEY: 1.250\n:END:\n* Ünïcode\n" 900000)
    (let* ((a-ids (dorg-test-partitions--root-ids dolt-dir 100))
           (b-ids (dorg-test-partitions--root-ids dolt-dir 900000))
           (changed (nth 3 a-ids))
           (outcome
            (dorg-test-partitions--in-transaction
             dolt-dir
             (list (format "UPDATE headings SET title = 'changed in transaction' WHERE id = %d" changed))
             (lambda (read)
               (let* ((batched (dorg-mutate--postimage-read-batched read))
                      (specs (dorg-test-partitions--specs-from-loads
                              batched (dorg-test-partitions--groups a-ids b-ids))))
                 (list (condition-case err
                           (progn (dorg-mutate--postimage-assert-partitions batched specs)
                                  :passed)
                         (dorg-sql-postimage-parity (cdr err)))
                       (cl-loop for spec in specs
                                thereis (and (gethash changed (nth 3 spec))
                                             (equal "changed in transaction"
                                                    (nth 7 (car (gethash changed (nth 3 spec)))))))))))))
      (should (> (length a-ids) 1024))
      (should (equal '(:passed t) outcome)))))

(defun dorg-test-partitions--image-row (h)
  "Return H's checked facts from a fresh copy: canonical tuple with its STORED
digest, persisted item identity, and the recomputed digest."
  (let* ((copy (dorg-heading-deep-copy h))
         (canonical (dorg-mutate--index-heading-canonical copy))
         (items (dorg-mutate--postimage-item-row-canonical copy)))
    (dorg-heading-compute-items-hash copy)
    (list canonical items (dorg-heading-items-hash copy))))

(defun dorg-test-partitions--observe (read specs)
  "Compare per-partition and production-union images for SPECS through READ.
Return a plist: :equal (every partition's own image equals its root's
union image id by id), :shared (ids present in more than one partition),
:partitions ((ROOT OWNER IDS) ...) of the present specs, and :max-union."
  (let ((unions (dorg-mutate--postimage-partition-unions specs))
        (union-images (make-hash-table :test 'eql))
        (seen (make-hash-table :test 'eql))
        (equal-p t) shared partitions)
    (dolist (union unions)
      (puthash (car union)
               (funcall read (list :version 1 :op 'owner-model
                                   :root-id (car union) :ids (cdr union)))
               union-images))
    (pcase-dolist (`(,root ,owner ,ids ,_expected ,deleted) specs)
      (unless deleted
        (push (list root owner (copy-sequence ids)) partitions)
        (let ((own (funcall read (list :version 1 :op 'owner-model :root-id root :ids ids)))
              (union (gethash root union-images)))
          (dolist (id ids)
            (when (gethash id seen) (push id shared))
            (puthash id t seen)
            (unless (equal (dorg-test-partitions--image-row (gethash id own))
                           (dorg-test-partitions--image-row (gethash id union)))
              (setq equal-p nil))))))
    (list :equal equal-p :shared shared :partitions (nreverse partitions)
          :roots (mapcar #'car unions)
          :max-union (apply #'max 0 (mapcar (lambda (u) (length (cdr u))) unions)))))

(defun dorg-test-partitions--capture (thunk)
  "Run THUNK, observing every production partition proof it triggers.
Return the list of observations from `dorg-test-partitions--observe', made
inside each real transaction on the proof's own read before the proof runs."
  (let ((proof (symbol-function 'dorg-mutate--postimage-assert-partitions))
        observations)
    (cl-letf (((symbol-function 'dorg-mutate--postimage-assert-partitions)
               (lambda (read specs)
                 (push (dorg-test-partitions--observe read specs) observations)
                 (funcall proof read specs))))
      (funcall thunk))
    (nreverse observations)))

(defconst dorg-test-partitions--nested-seed
  (concat "* Alpha\n:PROPERTIES:\n:BEAD_ID: bead-alpha\n:END:\nalpha body\n"
          (mapconcat (lambda (i)
                       (format "** C%d :t%d:\nSCHEDULED: <2026-10-%02d>\n:PROPERTIES:\n:KEY: 0.%d\n:END:\n#+CAPTION: café %d\nbody %d\n"
                               i (mod i 4) (1+ (mod i 28)) i i i))
                     (number-sequence 1 1100) "")
          "** Nested\n:PROPERTIES:\n:BEAD_ID: bead-nested\n:END:\nnested body\n"
          "*** Child\nchild body\n"
          "* Beta\n:PROPERTIES:\n:BEAD_ID: bead-beta\n:END:\n")
  "An identity owner over 1100 owner-local headings with a nested identity
owner inside it, and a sibling owner.")

(ert-deftest dorg-test-partitions-union-image-equals-partition-images-for-real-owners ()
  "Production owner partitions read as unions match per-partition loads.
A real identity mutation touching both a heading local to an owner whose
partition holds over 1024 headings and a heading of an identity owner nested
inside it, and a real cross-root move of an owned child, each run the production
proof.  Inside each transaction, before the proof, every partition's own
load and its root's production union give equal images, taken from fresh
copies with the stored digest captured before recomputation.  The observed
partitions really nest (the inner owner's parent heading lies in the outer
partition), cross the 1024-id chunk boundary and span two roots, and each
proof commits.  Production partitions are disjoint, so no heading is shared
between them here; overlapping and duplicate ids are covered by the
coverage and slice specs."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed dolt-dir dorg-test-partitions--nested-seed 100)
    (let* ((alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (nested (dorg-test-mutate--id-by-title dolt-dir "Nested"))
           (child (dorg-test-mutate--id-by-title dolt-dir "Child"))
           (local (dorg-test-mutate--id-by-title dolt-dir "C5"))
           (observed
            (dorg-test-partitions--capture
             (lambda ()
               (should (plist-get (dorg-test-mutate--apply-patch-batch
                                   (dorg-test-2ij34--src dolt-dir 100)
                                   (list (dorg-test-mutate--heading-update-patch
                                          local :title "C5 renamed")
                                         (dorg-test-mutate--heading-update-patch
                                          child :title "Child renamed")))
                                  :ok)))))
           (partitions (plist-get (car observed) :partitions))
           (outer (cl-find alpha partitions :key #'cadr))
           (inner (cl-find nested partitions :key #'cadr)))
      (should (= 1 (length observed)))
      (should (plist-get (car observed) :equal))
      (should outer)
      (should inner)
      (should (memql child (nth 2 inner)))
      (should (memql (alist-get 'parent_id (car (dorg-sql dolt-dir (format "SELECT parent_id FROM headings WHERE id = %d" nested))))
                     (nth 2 outer)))
      (should (> (plist-get (car observed) :max-union) 1024))
      (should-not (plist-get (car observed) :shared))))
  (dorg-test-mutate--with-dolt
    ;; Move the owned child Kid (102) out of Movable to DestParent: the
    ;; source owner keeps a present partition, so both roots are read.
    (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-src-seed 100)
    (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-dst-seed 500)
    (let ((observed (dorg-test-partitions--capture
                     (lambda ()
                       (should (plist-get
                                (dorg-test-mutate--apply-patch-batch
                                 (dorg-test-2ij34--src dolt-dir 100)
                                 (list (dorg-test-mutate--move-patch
                                        102 (dorg-test-mutate--heading-address 501)
                                        :to-source (dorg-mutate--resolve-source
                                                    (list :dolt-dir dolt-dir :root-id 500
                                                          :identity '(:property "BEAD_ID"))))))
                                :ok))))))
      (should observed)
      (should (cl-every (lambda (o) (plist-get o :equal)) observed))
      (should (cl-some (lambda (o) (>= (length (plist-get o :roots)) 2)) observed)))))

(defun dorg-test-partitions--attempt (dolt-dir statements assert specs)
  "Run STATEMENTS, then ASSERT over SPECS, in one committing transaction.
The parity abort escapes validation and is caught here, outside the
transaction.  Return `:committed' or (parity MESSAGE PLIST)."
  (condition-case err
      (progn
        (dorg-sql--with-lease (lease dolt-dir)
          (dorg-sql--lease-batch
           lease statements "partition attempt"
           (list :mode 'transaction-read-port :extension-tables nil
                 :function (lambda (read)
                             (funcall assert (dorg-mutate--postimage-read-batched read) specs)))))
        :committed)
    (dorg-sql-postimage-parity (cons 'parity (cdr err)))))

(defun dorg-test-partitions--snapshot (dolt-dir)
  "Return HEAD, every heading and item row, the evidence and derived rows."
  (list (dorg-head-commit dolt-dir)
        (dorg-sql dolt-dir "SELECT id, root_id, title, items_hash FROM headings ORDER BY id")
        (dorg-sql dolt-dir "SELECT id, heading_id, HEX(sort_key) AS k FROM heading_items ORDER BY id")
        (dorg-sql dolt-dir "SELECT op_id FROM dorg_mutation_log ORDER BY op_id")
        (dorg-sql dolt-dir "SELECT k, v FROM derived_ix ORDER BY k")))

(ert-deftest dorg-test-partitions-corruption-aborts-like-per-partition-and-rolls-back ()
  "Each corruption aborts exactly as the per-partition proof does and rolls back.
The planned transaction retitles a heading, writes an evidence row and
changes and adds derived rows;
expectations come from its uncorrupted postimage.  A misplaced row, a missing
row, a wrong stored digest, a heading field altered under a correct digest
and an altered item sort key are each attempted in separate transactions,
once per proof, with the parity abort escaping validation: both proofs
report the same abort and every observed state is restored.  Without
corruption the same transaction commits and changes that state."
  (dorg-test-mutate--with-dolt
    (unless (eq 'mysql (dorg-sql--transport-kind dolt-dir))
      (ert-skip "the transaction read port needs the server transport"))
    (dorg-test-mutate--seed dolt-dir (dorg-test-partitions--rich-text 30) 100)
    (dorg-test-mutate--seed dolt-dir "* Other root\n" 900000)
    (dorg-sql-batch dolt-dir
                    (list "CREATE TABLE derived_ix (k INT PRIMARY KEY, v TEXT)"
                          "INSERT INTO derived_ix VALUES (1, 'seeded'), (2, 'kept')"))
    (dorg-commit dolt-dir "seed derived rows")
    (let* ((a-ids (dorg-test-partitions--root-ids dolt-dir 100))
           (target (nth 5 a-ids))
           ;; The heading's last item: raising its key keeps the item order,
           ;; so the content digest stays correct and only identity changes.
           (item (alist-get 'id (car (dorg-sql dolt-dir (format "SELECT id FROM heading_items WHERE heading_id = %d ORDER BY sort_key DESC LIMIT 1" target)))))
           (planned (list (format "UPDATE headings SET title = 'planned' WHERE id = %d" (nth 1 a-ids))
                          (concat "INSERT INTO dorg_mutation_log (op_id, gen_id, intent_digest, ts)"
                                  " VALUES ('partition-probe-op', 'partition-probe-gen', 'digest', 1)")
                          "UPDATE derived_ix SET v = 'planned' WHERE k = 1"
                          "INSERT INTO derived_ix VALUES (3, 'added')"))
           (groups (list (cons 100 (seq-take a-ids 4)) (cons 100 (seq-drop a-ids 4))))
           (specs (dorg-test-partitions--in-transaction
                   dolt-dir planned
                   (lambda (read) (dorg-test-partitions--specs-from-loads
                                   (dorg-mutate--postimage-read-batched read) groups))))
           (before (dorg-test-partitions--snapshot dolt-dir)))
      (pcase-dolist (`(,corruption . ,message)
                     `((,(format "UPDATE headings SET root_id = 900000 WHERE id = %d" target)
                        . "a partition row is missing or misplaced at readback")
                       (,(format "DELETE FROM headings WHERE id = %d" target)
                        . "a partition row is missing or misplaced at readback")
                       (,(format "UPDATE headings SET items_hash = 'feedfacefeedfacefeedfacefeedface' WHERE id = %d" target)
                        . "committed item rows diverge from their stored digest")
                       (,(format "UPDATE headings SET priority = 'A' WHERE id = %d" target)
                        . "readback model diverges from the validated candidate")
                       (,(format "UPDATE heading_items SET sort_key = X'FFFFFFFF' WHERE id = %d" item)
                        . "committed item row identity diverges from the validated candidate")))
        (let ((reference (dorg-test-partitions--attempt
                          dolt-dir (append planned (list corruption))
                          #'dorg-test-partitions--reference specs))
              (_ (should (equal before (dorg-test-partitions--snapshot dolt-dir))))
              (batched (dorg-test-partitions--attempt
                        dolt-dir (append planned (list corruption))
                        #'dorg-mutate--postimage-assert-partitions specs)))
          (should (eq 'parity (car reference)))
          (should (equal message (cadr reference)))
          (should (equal (list target) (plist-get (nth 2 reference) :ids)))
          (should (equal reference batched))
          (should (equal before (dorg-test-partitions--snapshot dolt-dir)))))
      (should (eq :committed (dorg-test-partitions--attempt
                              dolt-dir planned #'dorg-mutate--postimage-assert-partitions specs)))
      (let ((after (dorg-test-partitions--snapshot dolt-dir)))
        (should-not (equal (car before) (car after)))
        (should (equal '(((title . "planned")))
                       (dorg-sql dolt-dir (format "SELECT title FROM headings WHERE id = %d" (nth 1 a-ids)))))
        (should (equal '(((op_id . "partition-probe-op"))) (nth 3 after)))
        (should (equal '(((k . 1) (v . "planned")) ((k . 2) (v . "kept")) ((k . 3) (v . "added")))
                       (nth 4 after)))))))

(provide 'dorg-postimage-partitions-tests)

;;; dorg-postimage-partitions-tests.el ends here
