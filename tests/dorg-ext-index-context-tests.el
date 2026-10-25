;;; dorg-ext-index-context-tests.el --- Post-mutation projection context -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED-first specification for tracked-regression — the immutable canonical
;; post-mutation projection context (`dorg-ext-index-context').
;;
;; DOrg constructs ONE immutable candidate-state context AFTER typed core
;; simulation and BEFORE persistence.  Registered index projectors will
;; (tracked-regression) consume this context instead of caller-authored semantic
;; extension payloads.  This file covers only the context CONTRACT: define,
;; construct, and expose.  It asserts, per the ratified child slice:
;;
;;   - identity timing: the context binds the exact base/CAS (S0) token and a
;;     transaction-scoped candidate identity, but NEVER a future commit token
;;     (`:new-token' is post-commit only, in the native result);
;;   - create/change/move/delete classification with before/after or tombstone
;;     data sufficient to derive additions and removals;
;;   - a real host-safe Dolt candidate-POSTIMAGE (candidate post-state), not
;;     merely plan/patch/affected/freshness data and not a private mutable
;;     model object;
;;   - immutable S0-versus-candidate: both the S0 image and the candidate
;;     postimage are bound so a consumer can prove `core remains N' BEFORE
;;     apply (the tracker-cli-id8i6 16-children witness);
;;   - backend parity: the plain-org path constructs the same logical context
;;     WITHOUT inventing a durable shadow index (nil candidate-postimage).
;;
;; The Dolt-path specs drive `dorg-mutate--dolt-simulate' through pure
;; in-memory stubs (no dolt binary / sql-server), so this file is FAST-gate
;; eligible but is MEDIUM by default until proven in the allowlist.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-ext)
(require 'dorg-mutate)
(require 'dorg-mutate-tests)          ; typed-patch + tree fixtures

;;; ─── Pure Dolt simulation harness (no server) ─────────────────────────

(ert-deftest dorg-test-fhhwd-index-context-refuses-cyclic-cons-spines ()
  "Unsupported circular patch batches and nested cells refuse boundedly."
  (let* ((max-lisp-eval-depth 1000)
         (patches (list '(:op update)))
         (cells (list "cyclic")))
    (setcdr patches patches)
    (setcdr cells cells)
    (should-error (dorg-ext-index-context-create :patches patches)
                  :type 'circular-list)
    (should-error (dorg-ext-index-context-create
                   :patches (list (list :op 'update :cells cells)))
                  :type 'circular-list)))

(ert-deftest dorg-test-fhhwd-wide-index-context-create-and-read-own-patches ()
  "512 typed patches retain copy-in/read isolation at the runner depth bound."
  (let* ((max-lisp-eval-depth 1000)
         (patches (cl-loop for id from 1 to 512
                           collect (dorg-test-mutate--heading-update-patch
                                    id :title (copy-sequence "ORIGINAL"))))
         (context (dorg-ext-index-context-create
                   :source-kind 'dolt :root-id 1 :patches patches))
         (inspection (dorg-ext-index-context-patches context)))
    (should (equal patches inspection))
    (should (= 512 (length inspection)))
    (aset (plist-get (plist-get (car (last patches)) :fields) :title) 0 ?X)
    (setcar patches '(:poisoned t))
    (should-not (plist-member (car (dorg-ext-index-context-patches context))
                              :poisoned))
    (should (equal "ORIGINAL"
                   (plist-get
                    (plist-get (car (last
                                     (dorg-ext-index-context-patches context)))
                               :fields)
                    :title)))
    (aset (plist-get (plist-get (car (last inspection)) :fields) :title) 0 ?Y)
    (setcdr inspection nil)
    (let ((again (dorg-ext-index-context-patches context)))
      (should (= 512 (length again)))
      (should (equal "ORIGINAL"
                     (plist-get (plist-get (car (last again)) :fields)
                                :title))))))

(ert-deftest dorg-test-fhhwd-index-context-dotted-vector-leaves-are-owned ()
  "Context copy-in/read detaches dotted tails, vectors and repeated leaves."
  (with-temp-buffer
    (let* ((handle (current-buffer))
           (leaf (copy-sequence "leaf"))
           (tail (copy-sequence "tail"))
           (cells (vector (cons leaf tail) leaf handle))
           (context (dorg-ext-index-context-create
                     :source-kind 'plain-org :root-id 1
                     :patches (list (list :op 'update :cells cells))))
           (copy (plist-get (car (dorg-ext-index-context-patches context))
                            :cells)))
      (should (equal cells copy))
      (should-not (eq cells copy))
      (should-not (eq (aref cells 0) (aref copy 0)))
      (should-not (eq (car (aref copy 0)) (aref copy 1)))
      (should (eq handle (aref copy 2)))
      (aset leaf 0 ?X)
      (aset tail 0 ?X)
      (aset cells 2 nil)
      (should (equal '("leaf" . "tail") (aref copy 0)))
      (should (equal "leaf" (aref copy 1)))
      (aset (cdr (aref copy 0)) 0 ?Y)
      (setcar (aref copy 0) "replaced")
      (aset copy 1 "replaced")
      (let ((again (plist-get
                    (car (dorg-ext-index-context-patches context)) :cells)))
        (should (equal '("leaf" . "tail") (aref again 0)))
        (should (equal "leaf" (aref again 1)))
        (should (eq handle (aref again 2)))))))

(defun dorg-test-ic--scope-tree (headings &optional ids)
  "Return an id -> `dorg-heading' hash of HEADINGS, restricted to IDS when non-nil."
  (let ((tree (make-hash-table :test 'eql)))
    (dolist (h headings tree)
      (when (or (null ids) (member (dorg-heading-id h) ids))
        (puthash (dorg-heading-id h) h tree)))))

(defun dorg-test-ic--order-facts (headings parent-id &optional bound side limit exclude-ids
                                         offsets-sink)
  "Serve an ordering read over in-memory HEADINGS like the store reader does.
Like the store read, it reports each returned heading's level offset into
OFFSETS-SINK when that is a hash."
  (let* ((rows (cl-loop for h in headings
                        when (and (eql parent-id (dorg-heading-parent-id h))
                                  (not (memql (dorg-heading-id h) exclude-ids))
                                  (or (null bound)
                                      (if (eq side 'before)
                                          (string< (dorg-heading-sort-key h) bound)
                                        (string< bound (dorg-heading-sort-key h)))))
                        collect (cons (dorg-heading-id h) (dorg-heading-sort-key h))))
         (sorted (sort rows (if (eq side 'before)
                                (lambda (a b) (string> (cdr a) (cdr b)))
                              (lambda (a b) (string< (cdr a) (cdr b)))))))
    (let ((out (if limit (seq-take sorted limit) sorted)))
      (when offsets-sink
        (dolist (cell out)
          (let ((h (cl-find (car cell) headings :key #'dorg-heading-id)))
            (puthash (car cell) (dorg-heading-level-offset h) offsets-sink))))
      out)))

(defun dorg-test-ic--dolt-sim (root-id headings patches &optional head identity scope-ids)
  "Simulate PATCHES over an in-memory ROOT-ID tree of HEADINGS at HEAD.
Runs the production `dorg-mutate--dolt-simulate' with the store I/O stubbed
out, returning its result plist (including `:index-context').
When IDENTITY is non-nil, it is threaded into the Dolt source descriptor.
When SCOPE-IDS is non-nil, the core simulation's bounded scope is restricted to
those ids while the stubbed loader can still serve later explicit owner-partition
loads from the full in-memory store."
  (dorg-test--fill-level-offsets headings)
  (let* ((desc (append (list :dolt-dir "/tmp/dorg-ic/" :root-id root-id)
                       (and identity (list :identity identity))))
         (src (dorg-mutate--resolve-source desc))
         (head (or head "H0")))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (_dolt-dir) head))
             ((symbol-function 'dorg-mutate--dolt-scope-ids)
               (lambda (&rest _) (or scope-ids (mapcar #'dorg-heading-id headings))))
              ((symbol-function 'dorg-mutate--dolt-child-ids)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of)
                 (mapcar #'dorg-heading-id
                         (cl-remove-if-not
                          (lambda (h) (eql parent-id (dorg-heading-parent-id h)))
                          headings))))
              ;; Keys-only ordering reads (neighbours, neighbourhoods or the
              ;; whole group), served from the same in-memory headings.
              ((symbol-function 'dorg-mutate--dolt-order-facts)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of
                                  bound side limit exclude-ids offsets-sink)
                 (dorg-test-ic--order-facts headings parent-id
                                            bound side limit exclude-ids offsets-sink)))
              ((symbol-function 'dorg-mutate--dolt-ancestor-ids)
               (lambda (_dolt-dir _root-id start-id &optional _as-of)
                 (dorg-test-ic--ancestor-ids start-id headings)))
              ((symbol-function 'dorg-mutate--dolt-owner-local-ids)
               (lambda (_dolt-dir _root-id owner-id key &optional _as-of _query-fn)
                 (dorg-test-ic--owner-local-ids owner-id key headings)))
              ((symbol-function 'dorg-model-load-tree)
               (lambda (&rest _)
                 (ert-fail "index-context sim must not whole-root load")))
              ((symbol-function 'dorg-model-load-headings)
               (lambda (_dolt-dir _root-id ids &optional _as-of _headline-only &rest _)
                 (dorg-test-ic--scope-tree headings ids))))
      (let ((dorg-mutate--dolt-item-id-generator
             (let ((n 700000)) (lambda () (prog1 n (cl-incf n))))))
        (dorg-mutate--dolt-simulate src patches nil)))))

(defun dorg-test-ic--children-of (parent-id image)
  "Return the ids in IMAGE (id -> heading hash) whose parent is PARENT-ID."
  (let (ids)
    (maphash (lambda (id h)
               (when (eql (dorg-heading-parent-id h) parent-id)
                 (push id ids)))
             image)
    (sort ids #'<)))

(defun dorg-test-ic--entry-by-id (entries id)
  "Return the `dorg-ext-index-entry' in ENTRIES with ID, or nil."
  (cl-find id entries :key #'dorg-ext-index-entry-id :test #'eql))

(defun dorg-test-ic--props (&rest pairs)
  "Return a PROPERTIES drawer item carrying PAIRS."
  (dorg-drawer-item :name "PROPERTIES" :properties pairs))

(cl-defun dorg-test-ic--heading (id parent level title sort &key bead-id)
  "Return a test heading with ID/PARENT/LEVEL/TITLE/SORT and optional BEAD-ID."
  (make-dorg-heading
   :id id :parent-id parent :level level :title title
   :sort-key (unibyte-string sort)
   :items (and bead-id
               (list (dorg-test-ic--props (cons "BEAD_ID" bead-id))))))

(defun dorg-test-ic--image-ids (image)
  "Return sorted heading ids present in IMAGE."
  (let (ids)
    (maphash (lambda (id _h) (push id ids)) image)
    (sort ids #'<)))

(defun dorg-test-ic--owner-partition-by-value (partitions value)
  "Return the owner partition in PARTITIONS whose owner value is VALUE."
  (cl-find value partitions
           :key #'dorg-ext-owner-partition-owner-value
           :test #'equal))

(defun dorg-test-ic--owner-local-ids (owner-id key headings)
  "Return OWNER-ID plus owner-local descendant ids in HEADINGS for KEY."
  (let ((ids (list owner-id))
        (queue (mapcar #'dorg-heading-id
                       (cl-remove-if-not
                        (lambda (h) (eql owner-id (dorg-heading-parent-id h)))
                        headings))))
    (while queue
      (let* ((id (pop queue))
             (h (cl-find id headings :key #'dorg-heading-id :test #'eql))
             (owner-p (and h (assoc key (dorg-model-heading-property-snapshot h)))))
        (when (and h (not owner-p))
          (push id ids)
          (setq queue
                (append queue
                        (mapcar #'dorg-heading-id
                                (cl-remove-if-not
                                 (lambda (child)
                                   (eql id (dorg-heading-parent-id child)))
                                 headings)))))))
    (sort ids #'<)))

(defun dorg-test-ic--ancestor-ids (start-id headings)
  "Return START-ID and ancestors from HEADINGS, nearest first."
  (let ((by-id (make-hash-table :test 'eql))
        ids)
    (dolist (h headings)
      (puthash (dorg-heading-id h) h by-id))
    (while (integerp start-id)
      (if-let ((h (gethash start-id by-id)))
          (progn
            (push start-id ids)
            (setq start-id (dorg-heading-parent-id h)))
        (setq start-id nil)))
    (nreverse ids)))

;;; ─── Identity timing: base bound, no future commit token ──────────────

(ert-deftest dorg-test-ext-index-context-dolt-binds-base-and-omits-commit-token ()
  "The Dolt index-context binds the exact base/CAS (S0) token and a distinct
candidate identity, but exposes NO future commit token.
Non-vacuity: the struct itself must not carry a commit-token slot."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))
               "BASE-CAS"))
         (ctx (plist-get sim :index-context)))
    (should (dorg-ext-index-context-p ctx))
    (should (eq 'dolt (dorg-ext-index-context-source-kind ctx)))
    (should (eql 100 (dorg-ext-index-context-root-id ctx)))
    (should (equal "/tmp/dorg-ic/" (dorg-ext-index-context-dolt-dir ctx)))
    ;; Immutable S0: the exact plan-base CAS token.
    (should (equal "BASE-CAS" (dorg-ext-index-context-base-commit ctx)))
    ;; Candidate identity is present and DISTINCT from the base token; it is a
    ;; transaction-scoped digest, never the future commit token.
    (should (dorg-ext-index-context-candidate-id ctx))
    (should-not (equal (dorg-ext-index-context-candidate-id ctx)
                       (dorg-ext-index-context-base-commit ctx)))
    ;; Structural guarantee: no future commit token can leak through the seam.
    (should-not (fboundp 'dorg-ext-index-context-new-token))))

(ert-deftest dorg-test-ext-index-context-binds-canonical-patches ()
  "The context binds the canonical resolved typed patches, not caller payloads."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (patches (list (dorg-test-mutate--heading-update-patch 101 :title "A*")))
         (sim (dorg-test-ic--dolt-sim 100 (list root a) patches))
         (ctx (plist-get sim :index-context)))
    (should (equal (plist-get sim :patches)
                   (dorg-ext-index-context-patches ctx)))))

;;; ─── create / change / move / delete classification ──────────────────

(ert-deftest dorg-test-ext-index-context-dolt-classifies-create-change-move-delete ()
  "One batch producing all four effect kinds is partitioned with before/after
or tombstone data sufficient to derive additions and removals."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x20)))
         (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                               :sort-key (unibyte-string #x40)))
         (c (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "C"
                               :sort-key (unibyte-string #x60)))
         (p (make-dorg-heading :id 104 :parent-id 100 :level 1 :level-offset 1 :title "P"
                               :sort-key (unibyte-string #x70)))
         (patches
          (list (dorg-test-mutate--tree-insert-patch
                 :root '(:id 300 :title "New"))
                (dorg-test-mutate--heading-update-patch 101 :title "A*")
                (dorg-test-mutate--move-patch 102 (dorg-test-mutate--heading-address 104))
                (dorg-test-mutate--delete-patch 103)))
         (sim (dorg-test-ic--dolt-sim 100 (list root a b c p) patches))
         (ctx (plist-get sim :index-context)))
    ;; create: heading 300, after present, no before.
    (let ((e (dorg-test-ic--entry-by-id
              (dorg-ext-index-context-created ctx) 300)))
      (should e)
      (should (eq 'create (dorg-ext-index-entry-kind e)))
      (should-not (dorg-ext-index-entry-before e))
      (should (dorg-heading-p (dorg-ext-index-entry-after e)))
      (should (equal "New" (dorg-heading-title (dorg-ext-index-entry-after e)))))
    ;; change: heading 101, before+after, lineage unchanged.
    (let ((e (dorg-test-ic--entry-by-id
              (dorg-ext-index-context-changed ctx) 101)))
      (should e)
      (should (eq 'change (dorg-ext-index-entry-kind e)))
      (should (equal "A" (dorg-heading-title (dorg-ext-index-entry-before e))))
      (should (equal "A*" (dorg-heading-title (dorg-ext-index-entry-after e))))
      (should (eql (dorg-heading-parent-id (dorg-ext-index-entry-before e))
                   (dorg-heading-parent-id (dorg-ext-index-entry-after e)))))
    ;; move: heading 102, lineage changed 100 -> 104.
    (let ((e (dorg-test-ic--entry-by-id
              (dorg-ext-index-context-moved ctx) 102)))
      (should e)
      (should (eq 'move (dorg-ext-index-entry-kind e)))
      (should (eql 100 (dorg-heading-parent-id (dorg-ext-index-entry-before e))))
      (should (eql 104 (dorg-heading-parent-id (dorg-ext-index-entry-after e)))))
    ;; delete: heading 103, tombstone before, no after.
    (let ((e (dorg-test-ic--entry-by-id
              (dorg-ext-index-context-deleted ctx) 103)))
      (should e)
      (should (eq 'delete (dorg-ext-index-entry-kind e)))
      (should (equal "C" (dorg-heading-title (dorg-ext-index-entry-before e))))
      (should-not (dorg-ext-index-entry-after e)))
    ;; The partition is disjoint: a moved heading is not also reported as changed.
    (should-not (dorg-test-ic--entry-by-id
                 (dorg-ext-index-context-changed ctx) 102))))

;;; ─── host-safe images, not the private mutable model ──────────────────

(ert-deftest dorg-test-ext-index-context-dolt-exposes-host-safe-images ()
  "Base image and candidate postimage are host-safe id -> heading maps, never
the private mutable working model, and the seam carries no projection SQL."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (base (dorg-ext-index-context-base-image ctx))
         (post (dorg-ext-index-context-candidate-postimage ctx)))
    (should (hash-table-p base))
    (should (hash-table-p post))
    ;; NOT the private mutable model object.
    (should-not (dorg-mutate--ps-p post))
    (should-not (dorg-mutate--ps-p base))
    ;; Entries are the shared read-view heading model.
    (should (dorg-heading-p (gethash 101 post)))
    (should (dorg-heading-p (gethash 101 base)))
    ;; No projection SQL / legacy op vocabulary escapes through the seam.
    (should-not (fboundp 'dorg-ext-index-context-sql))
    (should-not (fboundp 'dorg-ext-index-context-db-ops))))

(ert-deftest dorg-test-ext-index-context-dolt-images-are-immutable-snapshots ()
  "The candidate postimage is an immutable snapshot: mutating the working model
AFTER construction does not alter the exposed context.
Non-vacuity: the working-model heading really does change."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (post (dorg-ext-index-context-candidate-postimage ctx))
         (ps-heading (gethash 101 (dorg-mutate--ps-headings (plist-get sim :ps)))))
    (should (equal "A*" (dorg-heading-title (gethash 101 post))))
    ;; Mutate the live working-model heading.
    (setf (dorg-heading-title ps-heading) "MUTATED")
    (should (equal "MUTATED" (dorg-heading-title ps-heading)))     ; non-vacuity
    ;; The exposed snapshot is decoupled from the working model.
    (should (equal "A*" (dorg-heading-title (gethash 101 post))))))

;;; ─── immutable S0-versus-candidate (tracker-cli-id8i6 16-children witness) ────

(ert-deftest dorg-test-ext-index-context-dolt-s0-versus-candidate-core-remains-n ()
  "S0 vs candidate is bound at the context level: 16 core children deleted and
1 recreated yields a base image of 16 children and a candidate of 1, so a
consumer can prove `core remains N' BEFORE apply and refuse to retain stale
rows.  This is the tracker-cli-id8i6 refusal affordance, not production policy."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (children
          (cl-loop for i from 1 to 16
                   collect (make-dorg-heading
                            :id (+ 100 i) :parent-id 100 :level 1 :level-offset 1
                            :title (format "K%02d" i)
                            :sort-key (unibyte-string i))))
         (patches
          (append
           (cl-loop for i from 1 to 16
                    collect (dorg-test-mutate--delete-patch (+ 100 i)))
           (list (dorg-test-mutate--tree-insert-patch
                  :root '(:id 500 :title "Reborn")))))
         (sim (dorg-test-ic--dolt-sim 100 (cons root children) patches))
         (ctx (plist-get sim :index-context))
         (base (dorg-ext-index-context-base-image ctx))
         (post (dorg-ext-index-context-candidate-postimage ctx)))
    ;; Immutable S0: root still has its full 16 core children.
    (should (= 16 (length (dorg-test-ic--children-of 100 base))))
    ;; Candidate: exactly the one recreated child.
    (should (equal '(500) (dorg-test-ic--children-of 100 post)))
    ;; The typed deletion of 15+1 children is declared, with tombstones.
    (should (= 16 (length (dorg-ext-index-context-deleted ctx))))
    (should (cl-every (lambda (e) (dorg-heading-p (dorg-ext-index-entry-before e)))
                      (dorg-ext-index-context-deleted ctx)))
    (should (= 1 (length (dorg-ext-index-context-created ctx))))))

(ert-deftest dorg-test-ext-index-context-dolt-postimage-is-real-not-plan-data ()
  "The candidate postimage is a real candidate post-state (updated field values
plus untouched siblings), not merely plan/patch/affected/freshness data."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x20)))
         (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a b)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (post (dorg-ext-index-context-candidate-postimage ctx)))
    ;; The updated heading carries its POST value in the candidate image.
    (should (equal "A*" (dorg-heading-title (gethash 101 post))))
    ;; Untouched siblings survive in the candidate post-state (not affected-only).
    (should (dorg-heading-p (gethash 102 post)))
    (should (equal "B" (dorg-heading-title (gethash 102 post))))))

;;; ─── backend parity: plain-org constructs the logical context ─────────

(ert-deftest dorg-test-ext-index-context-plain-org-logical-context-no-shadow-index ()
  "The plain-org path constructs the SAME context type after core simulation,
binding source/base/patches and id-bearing changed entries, WITHOUT inventing a
durable shadow index (nil candidate postimage) and with no future commit token."
  (dorg-test-mutate--with-content buf
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((patches (list (dorg-test-mutate--property-put-patch 1 "CATEGORY" "work")))
           (res (dorg-test-mutate--apply-patch-batch buf patches :save nil))
           (ctx (plist-get res :index-context)))
      (should (dorg-ext-index-context-p ctx))
      (should (eq 'plain-org (dorg-ext-index-context-source-kind ctx)))
      ;; Base identity is the plain-org revision; no future commit token.
      (should (dorg-ext-index-context-base-commit ctx))
      ;; No durable shadow index materialized for plain-org.
      (should-not (dorg-ext-index-context-candidate-postimage ctx))
      (should-not (dorg-ext-index-context-base-image ctx))
      ;; Canonical patches are bound; the changed heading is reported by id.
      (should (equal patches (dorg-ext-index-context-patches ctx)))
      (should (dorg-test-ic--entry-by-id
               (dorg-ext-index-context-changed ctx) 1)))))

(ert-deftest dorg-test-ext-index-context-plain-org-binds-s0-not-s1 ()
  "Plain-org BASE-COMMIT binds the pre-mutation S0/CAS identity, not the
post-mutation revision (S1).  Direct receipt: base = S0, base != S1, S0 != S1."
  (dorg-test-mutate--with-content buf
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((source (dorg-mutate--resolve-source buf))
           (s0 (dorg-mutate--plain-org-revision source))
           (patches (list (dorg-test-mutate--property-put-patch 1 "CATEGORY" "work")))
           (res (dorg-test-mutate--apply-patch-batch buf patches :save nil))
           (ctx (plist-get res :index-context))
           (s1 (dorg-mutate--plain-org-revision source)))
      ;; Non-vacuity: the surgical mutation really advanced the revision.
      (should-not (equal s0 s1))
      ;; The context binds S0 exactly, never the post-mutation S1.
      (should (equal s0 (dorg-ext-index-context-base-commit ctx)))
      (should-not (equal s1 (dorg-ext-index-context-base-commit ctx))))))

;;; ─── immutability: read-only slots ────────────────────────────────────

(ert-deftest dorg-test-ext-index-context-slots-are-read-only ()
  "Context and entry slots are immutable: `setf' on a public accessor is
rejected, so a consumer cannot rewrite the seam it was handed.  Direct receipt
from the Navigator: (setf (dorg-ext-index-context-root-id ctx) 2) must NOT work."
  (let ((ctx (dorg-ext-index-context-create :source-kind 'dolt :root-id 1))
        (entry (dorg-ext-index-entry-create :kind 'create :id 9)))
    (should (eql 1 (dorg-ext-index-context-root-id ctx)))
    ;; Read-only slots define no setf-expander, so these forms fail to expand.
    (should-error (eval `(setf (dorg-ext-index-context-root-id ',ctx) 2) t))
    (should-error (eval `(setf (dorg-ext-index-context-candidate-postimage ',ctx)
                               ,(make-hash-table))
                        t))
    (should-error (eval `(setf (dorg-ext-index-entry-kind ',entry) 'delete) t))
    ;; State is unchanged after the rejected writes.
    (should (eql 1 (dorg-ext-index-context-root-id ctx)))
    (should (eq 'create (dorg-ext-index-entry-kind entry)))))

;;; ─── candidate identity covers every persisted field ──────────────────

(ert-deftest dorg-test-ext-index-context-candidate-id-covers-all-persisted-fields ()
  "The candidate identity distinguishes a change in ANY persisted heading field,
including `commented' and `post-blank' (the Navigator's named omissions).
Transformation: differ-in-exactly-one-field => differ-in-candidate-id."
  (dolist (mut (list
                (lambda (h) (setf (dorg-heading-commented h) t))
                (lambda (h) (setf (dorg-heading-post-blank h) 3))
                (lambda (h) (setf (dorg-heading-title h) "T2"))
                (lambda (h) (setf (dorg-heading-todo h) "DONE"))
                (lambda (h) (setf (dorg-heading-priority h) ?A))
                (lambda (h) (setf (dorg-heading-level-offset h) 2))
                (lambda (h) (setf (dorg-heading-parent-id h) 9))
                (lambda (h) (setf (dorg-heading-sort-key h) (unibyte-string #x50)))
                (lambda (h) (setf (dorg-heading-tags h) '("x")))
                (lambda (h) (setf (dorg-heading-items-hash h) "deadbeef"))))
    (let* ((base (make-dorg-heading :id 1 :parent-id 0 :level 1 :level-offset 1 :title "T"
                                    :sort-key (unibyte-string #x40)))
           (variant (copy-dorg-heading base))
           (h1 (make-hash-table :test 'eql))
           (h2 (make-hash-table :test 'eql)))
      (funcall mut variant)
      (puthash 1 base h1)
      (puthash 1 variant h2)
      ;; Non-vacuity control: identical images share an id.
      (should (equal (dorg-mutate--index-candidate-id h1)
                     (dorg-mutate--index-candidate-id
                      (let ((h (make-hash-table :test 'eql)))
                        (puthash 1 (copy-dorg-heading base) h) h))))
      ;; The single-field change flips the candidate id.
      (should-not (equal (dorg-mutate--index-candidate-id h1)
                         (dorg-mutate--index-candidate-id h2))))))

;;; ─── affected owner partitions (tracked-case RED) ──────────────────────

(ert-deftest dorg-test-ext-index-context-owner-resolution-is-linear ()
  "Changing N owners resolves membership once per side, not N times per owner."
  (dolist (n '(4 8))
    (let ((headings (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)))
          patches
          (calls 0)
          (resolve (symbol-function 'dorg-mutate--index-nearest-owner)))
      (dotimes (i n)
        (let ((id (+ 101 i)))
          (push (dorg-test-ic--heading id 100 1 "Before" (+ 16 i)
                                     :bead-id (format "owner-%s" i))
                headings)
          (push (dorg-test-mutate--heading-update-patch id :title "After") patches)))
      (let* ((sim
              (cl-letf (((symbol-function 'dorg-mutate--index-nearest-owner)
                         (lambda (&rest args)
                           (cl-incf calls)
                           (apply resolve args))))
                (dorg-test-ic--dolt-sim
                 100 headings patches "H0" '(:property "BEAD_ID"))))
             (parts (dorg-ext-index-context-owner-partitions
                     (plist-get sim :index-context))))
        (should (= n (length parts)))
        (dolist (part parts)
          (let* ((id (dorg-ext-owner-partition-owner-id part))
                 (before (dorg-ext-owner-partition-base-image part))
                 (after (dorg-ext-owner-partition-candidate-postimage part)))
            (should (equal (list id) (dorg-test-ic--image-ids after)))
            (should (equal "Before" (dorg-heading-title (gethash id before))))
            (should (equal "After" (dorg-heading-title (gethash id after))))))
        ;; Two entry sides and the nearest parent per owner.  The old overlay
        ;; adds 2*N*N resolutions, including SQL for unrelated owners.
        (should (= (* 3 n) calls))))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-indexes-each-image-once ()
  "Affected partitions share one child index per immutable side image."
  (dolist (n '(4 8))
    (let ((headings (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)))
          patches
          (calls 0)
          (cells 0)
          (build (symbol-function 'dorg-mutate--index-image-children)))
      (dotimes (i n)
        (let ((id (+ 101 i)))
          (push (dorg-test-ic--heading id 100 1 "Before" (+ 16 i)
                                       :bead-id (format "owner-%s" i))
                headings)
          (push (dorg-test-mutate--heading-update-patch id :title "After") patches)))
      (cl-letf (((symbol-function 'dorg-mutate--index-image-children)
                 (lambda (image)
                   (cl-incf calls)
                   (cl-incf cells (hash-table-count image))
                   (funcall build image))))
        (dorg-test-ic--dolt-sim
         100 headings patches "H0" '(:property "BEAD_ID")))
      ;; One complete base image and one complete candidate image, independent
      ;; of the number of affected owner paths.
      (should (= 2 calls))
      (should (= (* 2 (1+ n)) cells)))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-preserves-move-sides ()
  "Moving a plain heading removes its old ownership and installs the new one."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Left" #x20 :bead-id "left")
                (dorg-test-ic--heading 102 100 1 "Right" #x40 :bead-id "right")
                (dorg-test-ic--heading 103 101 2 "Moving" #x20)
                (dorg-test-ic--heading 105 102 2 "Untouched" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      103 (dorg-test-mutate--heading-address 102)))
               "H0" '(:property "BEAD_ID")))
         (parts (dorg-ext-index-context-owner-partitions
                 (plist-get sim :index-context)))
         (left (dorg-test-ic--owner-partition-by-value parts "left"))
         (right (dorg-test-ic--owner-partition-by-value parts "right")))
    (should (equal '(101 103)
                   (dorg-test-ic--image-ids (dorg-ext-owner-partition-base-image left))))
    (should (equal '(101)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage left))))
    (should (equal '(102 105)
                   (dorg-test-ic--image-ids (dorg-ext-owner-partition-base-image right))))
    (should (equal '(102 103 105)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage right))))))

(ert-deftest dorg-test-ext-index-context-owner-local-image-keeps-dorg-owner-singleton ()
  "DOrg identity treats every heading as an owner despite hidden DORG_ID props."
  (let* ((source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-ic/" :root-id 100 :identity dorg)))
         (image (make-hash-table :test 'eql)))
    (puthash 101 (dorg-test-ic--heading 101 100 1 "Owner" #x20) image)
    (puthash 102 (dorg-test-ic--heading 102 101 2 "Child" #x20) image)
    (should (equal '(101)
                   (dorg-mutate--index-image-owner-local-ids
                    source image (dorg-mutate--index-image-children image) 101)))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-removes-transitive-descendants ()
  "A moved plain depth-three branch leaves no stale row in its old owner."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Left" #x20 :bead-id "left")
                (dorg-test-ic--heading 102 100 1 "Right" #x40 :bead-id "right")
                (dorg-test-ic--heading 103 101 2 "Moving" #x20)
                (dorg-test-ic--heading 104 103 3 "Child" #x20)
                (dorg-test-ic--heading 106 104 4 "Grandchild" #x20)
                (dorg-test-ic--heading 105 102 2 "Untouched" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      103 (dorg-test-mutate--heading-address 102)))
               "H0" '(:property "BEAD_ID")))
         (parts (dorg-ext-index-context-owner-partitions
                 (plist-get sim :index-context)))
         (left (dorg-test-ic--owner-partition-by-value parts "left")))
    (should (equal '(101 103 104 106)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-base-image left))))
    ;; RED on 7ec9049a5: the entry-only overlay leaves 104 and 106 behind.
    (should (equal '(101)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage left))))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-adds-transitive-descendants ()
  "A moved plain depth-three branch reaches its new owner completely."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Left" #x20 :bead-id "left")
                (dorg-test-ic--heading 102 100 1 "Right" #x40 :bead-id "right")
                (dorg-test-ic--heading 103 101 2 "Moving" #x20)
                (dorg-test-ic--heading 104 103 3 "Child" #x20)
                (dorg-test-ic--heading 106 104 4 "Grandchild" #x20)
                (dorg-test-ic--heading 105 102 2 "Untouched" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      103 (dorg-test-mutate--heading-address 102)))
               "H0" '(:property "BEAD_ID")))
         (parts (dorg-ext-index-context-owner-partitions
                 (plist-get sim :index-context)))
         (right (dorg-test-ic--owner-partition-by-value parts "right")))
    (should (equal '(102 105)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-base-image right))))
    ;; RED on 7ec9049a5: the entry-only overlay installs only 103.
    (should (equal '(102 103 104 105 106)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage right))))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-stops-before-nested-owner ()
  "A moved branch does not fold a nested identity owner into its outer owner."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Left" #x20 :bead-id "left")
                (dorg-test-ic--heading 102 100 1 "Right" #x40 :bead-id "right")
                (dorg-test-ic--heading 103 101 2 "Moving" #x20)
                (dorg-test-ic--heading 104 103 3 "Nested" #x20 :bead-id "nested")
                (dorg-test-ic--heading 106 104 4 "Nested child" #x20)
                (dorg-test-ic--heading 105 102 2 "Untouched" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      103 (dorg-test-mutate--heading-address 102)))
               "H0" '(:property "BEAD_ID")))
         (parts (dorg-ext-index-context-owner-partitions
                 (plist-get sim :index-context)))
         (left (dorg-test-ic--owner-partition-by-value parts "left"))
         (right (dorg-test-ic--owner-partition-by-value parts "right")))
    (should (equal '(101 103)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-base-image left))))
    (should (equal '(101)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage left))))
    (should (equal '(102 105)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-base-image right))))
    (should (equal '(102 103 105)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage right))))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-does-not-readd-deleted-descendant ()
  "A moved branch never readds a descendant deleted in the same batch."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Left" #x20 :bead-id "left")
                (dorg-test-ic--heading 102 100 1 "Right" #x40 :bead-id "right")
                (dorg-test-ic--heading 103 101 2 "Moving" #x20)
                (dorg-test-ic--heading 104 103 3 "Deleted" #x20)
                (dorg-test-ic--heading 105 102 2 "Untouched" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      103 (dorg-test-mutate--heading-address 102))
                     (dorg-test-mutate--delete-patch 104 t))
               "H0" '(:property "BEAD_ID")))
         (parts (dorg-ext-index-context-owner-partitions
                 (plist-get sim :index-context)))
         (left (dorg-test-ic--owner-partition-by-value parts "left"))
         (right (dorg-test-ic--owner-partition-by-value parts "right")))
    (should (equal '(101)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage left))))
    (should (equal '(102 103 105)
                   (dorg-test-ic--image-ids
                    (dorg-ext-owner-partition-candidate-postimage right))))))

(ert-deftest dorg-test-ext-index-context-owner-overlay-preserves-within-owner-content ()
  "Within-owner reparenting preserves descendants and leaves other owners alone."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Owner" #x20 :bead-id "owner")
                (dorg-test-ic--heading 102 101 2 "Old parent" #x20)
                (dorg-test-ic--heading 103 101 2 "New parent" #x40)
                (dorg-test-ic--heading 104 102 3 "Moving" #x20)
                (dorg-test-ic--heading 106 104 4 "Grandchild" #x20)
                (dorg-test-ic--heading 105 101 2 "Unchanged" #x60)
                (dorg-test-ic--heading 107 100 1 "Other owner" #x40 :bead-id "other")
                (dorg-test-ic--heading 108 107 2 "Other child" #x20)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      104 (dorg-test-mutate--heading-address 103)))
               "H0" '(:property "BEAD_ID")))
         (ctx (plist-get sim :index-context))
         (parts (dorg-ext-index-context-owner-partitions ctx))
         (owner (dorg-test-ic--owner-partition-by-value parts "owner"))
         (post (dorg-ext-owner-partition-candidate-postimage owner))
         (core (dorg-ext-index-context-candidate-postimage ctx)))
    (should (equal '(101 102 103 104 105 106)
                   (dorg-test-ic--image-ids post)))
    (should (eql 103 (dorg-heading-parent-id (gethash 104 post))))
    (should (eql 104 (dorg-heading-parent-id (gethash 106 post))))
    (should (equal "Unchanged" (dorg-heading-title (gethash 105 post))))
    (should (equal (unibyte-string #x60) (dorg-heading-sort-key (gethash 105 post))))
    (should-not (dorg-test-ic--owner-partition-by-value parts "other"))
    (should (equal "Other child" (dorg-heading-title (gethash 108 core))))
    (should (equal (unibyte-string #x20) (dorg-heading-sort-key (gethash 108 core))))))

(ert-deftest dorg-test-ext-index-context-owner-partitions-use-nearest-ancestor-owner ()
  "c4l63 witness A: a nested child-bead nonstructural update exposes the child
owner plus the nearest ANCESTOR identity owner, not the literal parent.
The literal parent (102) has no BEAD_ID; the parent bead identity is on ancestor
101.  The old dda5ad0 base/post images stay byte-stable as the core working
scope {root,target}; owner closure must arrive only through the dedicated
partition accessor."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Parent bead" #x20
                                       :bead-id "parent")
                (dorg-test-ic--heading 102 101 2 "Plain wrapper" #x20)
                (dorg-test-ic--heading 103 102 3 "Child bead" #x20
                                       :bead-id "child")))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--property-put-patch 103 "STATUS" "done"))
               "H0" '(:property "BEAD_ID") '(100 103)))
         (ctx (plist-get sim :index-context)))
    (should (equal '(100 103)
                   (dorg-test-ic--image-ids
                    (dorg-ext-index-context-base-image ctx))))
    (should (equal '(100 103)
                   (dorg-test-ic--image-ids
                    (dorg-ext-index-context-candidate-postimage ctx))))
    (should (fboundp 'dorg-ext-index-context-owner-partitions))
    (let* ((partitions (dorg-ext-index-context-owner-partitions ctx))
           (child (dorg-test-ic--owner-partition-by-value partitions "child")))
      (should child)
      (should (eql 103 (dorg-ext-owner-partition-owner-id child)))
      (should (equal "BEAD_ID" (dorg-ext-owner-partition-owner-key child)))
      (should (equal "child" (dorg-ext-owner-partition-owner-value child)))
      (should (eql 101 (dorg-ext-owner-partition-parent-owner-id child)))
      (should (equal "parent" (dorg-ext-owner-partition-parent-owner-value child)))
      (should (member '(:id 103 :side after :path (103))
                      (dorg-ext-owner-partition-affected-paths child)))
      (should (member 103 (dorg-ext-owner-partition-affected-ids child))))))

(ert-deftest dorg-test-level-offset-nested-owner-partition-rebuilt-same-root ()
  "A same-root move rebuilds the partition of an owner nested in the subtree.
N (bead \"n\") sits inside P's subtree behind a plain wrapper.  Moving P under
D changes N's ancestry but no stored field of N, so N reaches classification
only through the explicit moved closure; its partition must still be rebuilt."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "P bead" #x20 :bead-id "p")
                (dorg-test-ic--heading 102 101 2 "Wrapper" #x20)
                (dorg-test-ic--heading 103 102 3 "N bead" #x20 :bead-id "n")
                (dorg-test-ic--heading 104 100 1 "D bead" #x40 :bead-id "d")))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--move-patch
                      101 (dorg-test-mutate--heading-address 104)))
               "H0" '(:property "BEAD_ID")))
         (ctx (plist-get sim :index-context))
         (partitions (dorg-ext-index-context-owner-partitions ctx))
         (nested (dorg-test-ic--owner-partition-by-value partitions "n")))
    (should (memql 103 (plist-get sim :moved)))
    (should (memql 103 (mapcar #'dorg-ext-index-entry-id
                               (dorg-ext-index-context-moved ctx))))
    (should nested)
    (should (eql 103 (dorg-ext-owner-partition-owner-id nested)))))

(ert-deftest dorg-test-ext-index-context-owner-partitions-expose-dense-owner-local-section ()
  "c4l63 witness B: a nested section/note change exposes one unique owner with a
complete dense ordered owner-local partition, excluding nested identity owners.
The touched section 103 lives under owner 101.  Nested bead 104 and its child 105
must not be folded into owner 101's local rows, while sibling 106 must be present
because dense owner-local projection needs ordered peer context."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Owner bead" #x20
                                       :bead-id "owner")
                (dorg-test-ic--heading 102 101 2 "Note A" #x20)
                (dorg-test-ic--heading 103 101 2 "Note B" #x40)
                (dorg-test-ic--heading 104 101 2 "Nested bead" #x60
                                       :bead-id "nested")
                (dorg-test-ic--heading 105 104 3 "Nested note" #x20)
                (dorg-test-ic--heading 106 101 2 "Note C" #x70)))
         (sim (dorg-test-ic--dolt-sim
               100 headings
               (list (dorg-test-mutate--heading-update-patch 103 :title "Note B*"))
               "H0" '(:property "BEAD_ID") '(100 103)))
         (ctx (plist-get sim :index-context)))
    (should (equal '(100 103)
                   (dorg-test-ic--image-ids
                    (dorg-ext-index-context-candidate-postimage ctx))))
    (let* ((partitions (dorg-ext-index-context-owner-partitions ctx))
           (owner (dorg-test-ic--owner-partition-by-value partitions "owner"))
           (post (and owner (dorg-ext-owner-partition-candidate-postimage owner))))
      (should owner)
      (should (equal '(101 102 103 106) (dorg-test-ic--image-ids post)))
      (should (equal "Note B*" (dorg-heading-title (gethash 103 post))))
      (should-not (gethash 104 post))
      (should-not (gethash 105 post))
      (should (member '(:id 103 :side after :path (103 101))
                      (dorg-ext-owner-partition-affected-paths owner)))
      (should (member 103 (dorg-ext-owner-partition-affected-ids owner))))))

(ert-deftest dorg-test-ext-index-context-candidate-id-covers-owner-partition-fields ()
  "c4l63 candidate identity witness: two simulations with identical core
candidate-postimages but different owner-local partition content must produce
different candidate ids.  In dda5ad0 both contexts hash only {root,target}, so
this fails until projector-visible owner partition fields enter the digest."
  (let* ((mk (lambda (sibling-title)
               (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                     (dorg-test-ic--heading 101 100 1 "Owner bead" #x20
                                            :bead-id "owner")
                     (dorg-test-ic--heading 102 101 2 "Target" #x20)
                     (dorg-test-ic--heading 103 101 2 sibling-title #x40)))))
    (let* ((sim-a (dorg-test-ic--dolt-sim
                   100 (funcall mk "Sibling A")
                   (list (dorg-test-mutate--heading-update-patch 102 :title "Target*"))
                   "H0" '(:property "BEAD_ID") '(100 102)))
           (sim-b (dorg-test-ic--dolt-sim
                   100 (funcall mk "Sibling B")
                   (list (dorg-test-mutate--heading-update-patch 102 :title "Target*"))
                   "H0" '(:property "BEAD_ID") '(100 102)))
           (ctx-a (plist-get sim-a :index-context))
           (ctx-b (plist-get sim-b :index-context)))
      (should (equal (dorg-test-ic--image-ids
                      (dorg-ext-index-context-candidate-postimage ctx-a))
                     (dorg-test-ic--image-ids
                      (dorg-ext-index-context-candidate-postimage ctx-b))))
      (should-not (equal (dorg-ext-index-context-candidate-id ctx-a)
                         (dorg-ext-index-context-candidate-id ctx-b))))))

(ert-deftest dorg-test-ext-index-context-owner-partition-loads-are-bounded-explicit-id-slices ()
  "c4l63 bounded-load witness: owner partition construction uses explicit id
slices and never the full-root loader.  The core scope is deliberately only
{root,target}; a GREEN implementation must make an additional bounded owner
closure load that includes owner/path/local ids outside that core scope."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Owner bead" #x20
                                       :bead-id "owner")
                (dorg-test-ic--heading 102 101 2 "Target" #x20)
                (dorg-test-ic--heading 103 101 2 "Sibling" #x40)))
         (src (dorg-mutate--resolve-source
               (list :dolt-dir "/tmp/dorg-ic/" :root-id 100
                     :identity '(:property "BEAD_ID"))))
         (patches (list (dorg-test-mutate--heading-update-patch 102 :title "Target*")))
         (load-calls nil))
    (dorg-test--fill-level-offsets headings)
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (_dolt-dir) "H0"))
              ((symbol-function 'dorg-mutate--dolt-scope-ids)
               (lambda (&rest _) '(100 102)))
              ((symbol-function 'dorg-mutate--dolt-child-ids)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of)
                 (mapcar #'dorg-heading-id
                         (cl-remove-if-not
                          (lambda (h) (eql parent-id (dorg-heading-parent-id h)))
                          headings))))
              ;; Keys-only ordering reads (neighbours, neighbourhoods or the
              ;; whole group), served from the same in-memory headings.
              ((symbol-function 'dorg-mutate--dolt-order-facts)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of
                                  bound side limit exclude-ids offsets-sink)
                 (dorg-test-ic--order-facts headings parent-id
                                            bound side limit exclude-ids offsets-sink)))
              ((symbol-function 'dorg-mutate--dolt-ancestor-ids)
               (lambda (_dolt-dir _root-id start-id &optional _as-of)
                 (dorg-test-ic--ancestor-ids start-id headings)))
              ((symbol-function 'dorg-mutate--dolt-owner-local-ids)
               (lambda (_dolt-dir _root-id owner-id key &optional _as-of _query-fn)
                 (dorg-test-ic--owner-local-ids owner-id key headings)))
              ((symbol-function 'dorg-model-load-tree)
               (lambda (&rest _)
                 (ert-fail "owner partition construction must not full-root load")))
              ((symbol-function 'dorg-model-load-headings)
               (lambda (_dolt-dir _root-id ids &optional _as-of _headline-only &rest _)
                 (push ids load-calls)
                 (should ids)
                 (dorg-test-ic--scope-tree headings ids))))
      (let ((dorg-mutate--dolt-item-id-generator
             (let ((n 700000)) (lambda () (prog1 n (cl-incf n))))))
        (dorg-mutate--dolt-simulate src patches nil)))
    (should (cl-some (lambda (ids)
                       (and (member 101 ids) (member 103 ids)
                            (not (member 100 ids))))
                     load-calls))))

(ert-deftest dorg-test-ext-index-context-owner-local-query-is-recursive-and-single-shot ()
  "Owner-local discovery is one bounded recursive query, not a per-node child walk."
  (let (queries)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dolt-dir query)
                 (push query queries)
                 '(((id . 101)) ((id . 102)) ((id . 103)))))
              ((symbol-function 'dorg-mutate--dolt-child-ids)
               (lambda (&rest _)
                 (ert-fail "owner-local partition discovery must not walk per child"))))
      (should (equal '(101 102 103)
                     (dorg-mutate--dolt-owner-local-ids
                      "/tmp/dorg-ic/" 100 101 "BEAD_ID" "H0"))))
    (should (= 1 (length queries)))
    (should (string-match-p "WITH RECURSIVE owner_local" (car queries)))
    (should (string-match-p "ol\\.nested_owner = 0" (car queries)))
    (should (string-match-p "pe\\.prop_key = 'BEAD_ID'" (car queries)))))

;;; ─── cross-root aggregation (real two-root store) ─────────────────────

(ert-deftest dorg-test-ext-index-context-cross-root-aggregates-both-roots ()
  "A cross-root archive move exposes an index context that aggregates BOTH roots:
the moved id leaves FROM-ROOT-ID (source lineage in base image) and arrives at
TO-ROOT-ID (destination lineage in candidate postimage)."
  (dorg-test-mutate--with-dolt
    (let* ((src-root (dorg-test-mutate--seed
                      dolt-dir "* Movable\n** Kid\n* SrcSibling\n" 100))
           (dst-root (dorg-test-mutate--seed
                      dolt-dir "* DstParent\n** DstChild\n" 500))
           (movable (dorg-test-mutate--id-by-title dolt-dir "Movable"))
           (dst-parent (dorg-test-mutate--id-by-title dolt-dir "DstParent"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id src-root)))
           (dest (dorg-mutate--resolve-source
                  (list :dolt-dir dolt-dir :root-id dst-root)))
           (patch (dorg-test-mutate--move-patch
                   movable (dorg-test-mutate--heading-address dst-parent)
                   :to-source dest))
           (sim (dorg-mutate--dolt-cross-root-simulate source patch nil))
           (ctx (plist-get sim :index-context))
           (base (dorg-ext-index-context-base-image ctx))
           (post (dorg-ext-index-context-candidate-postimage ctx)))
      (should (dorg-ext-index-context-p ctx))
      (should (eq 'dolt (dorg-ext-index-context-source-kind ctx)))
      ;; Both roots named for a root-aggregate projector.
      (should (eql src-root (dorg-ext-index-context-from-root-id ctx)))
      (should (eql dst-root (dorg-ext-index-context-to-root-id ctx)))
      ;; The moved id is a `move' entry: source lineage before, destination after.
      (let ((e (dorg-test-ic--entry-by-id
                (dorg-ext-index-context-moved ctx) movable)))
        (should e)
        (should (eq 'move (dorg-ext-index-entry-kind e)))
        (should (eql src-root (dorg-heading-parent-id (dorg-ext-index-entry-before e))))
        (should (eql dst-parent (dorg-heading-parent-id (dorg-ext-index-entry-after e)))))
      ;; Aggregation: base image has the id under the SOURCE root; candidate has
      ;; it re-homed under the DESTINATION parent and gone from the source root.
      (should (member movable (dorg-test-ic--children-of src-root base)))
      (should (member movable (dorg-test-ic--children-of dst-parent post)))
      (should-not (member movable (dorg-test-ic--children-of src-root post)))
      ;; Both roots' scopes are unioned into each image.
      (should (gethash src-root base))
      (should (gethash dst-root base))
      (should (gethash src-root post))
      (should (gethash dst-root post))
      ;; Candidate identity present and distinct from the base token.
      (should (dorg-ext-index-context-candidate-id ctx))
      (should-not (equal (dorg-ext-index-context-candidate-id ctx)
                         (dorg-ext-index-context-base-commit ctx))))))

;;; ─── production-shaped bounded-scope postimage (real loader) ──────────

(ert-deftest dorg-test-ext-index-context-dolt-postimage-is-bounded-working-scope ()
  "The production candidate postimage equals the transaction's BOUNDED WORKING
SCOPE (the real loader's id slice), carrying untouched loaded state — not a stub
and not the whole root.  Direct receipt: postimage ids == production scope ids,
root (untouched) present, updated heading holds its post value, and an unrelated
descendant OUTSIDE the scope is excluded (bounded, not full-root)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* A\n* B\n** B1\n" 100))
           (a (dorg-test-mutate--id-by-title dolt-dir "A"))
           (b1 (dorg-test-mutate--id-by-title dolt-dir "B1"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id root)))
           (patches (list (dorg-test-mutate--heading-update-patch a :title "A*")))
           (head (dorg-head-commit dolt-dir))
           ;; Production scope, computed by the real resolver (no stub).
           (scope (sort (copy-sequence
                         (dorg-mutate--dolt-scope-ids source patches root head))
                        #'<))
           (sim (dorg-mutate--dolt-simulate source patches nil))
           (ctx (plist-get sim :index-context))
           (post (dorg-ext-index-context-candidate-postimage ctx))
           (post-ids (let (ks) (maphash (lambda (k _v) (push k ks)) post)
                          (sort ks #'<))))
      ;; The image IS the bounded working scope — the defined canonical view
      ;; (a pure update neither adds nor removes ids, so image ids == scope ids).
      (should (equal scope post-ids))
      ;; Untouched loaded state (the root row) is present in the image.
      (should (member root post-ids))
      ;; Real post-state: the updated heading carries its new title.
      (should (equal "A*" (dorg-heading-title (gethash a post))))
      ;; Bounded, NOT whole-root: an unrelated grandchild outside the scope of an
      ;; update to A is not materialized.
      (should-not (member b1 post-ids)))))

;;; ─── host-safe immutability: deep copy + consumer isolation ───────────

(ert-deftest dorg-test-ext-index-context-postimage-deep-copies-nested-state ()
  "The candidate postimage deep-copies NESTED tags/items, so mutating a tag
through the exposed image cannot reach DOrg's private simulation (`ps') state.
Non-vacuity: a shallow copy aliases the tags list and poisons `ps'."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)
                               :tags (list "alpha")))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (ps-h (gethash 101 (dorg-mutate--ps-headings (plist-get sim :ps))))
         (exposed (gethash 101 (dorg-ext-index-context-candidate-postimage ctx))))
    (should (equal '("alpha") (dorg-heading-tags exposed)))
    (should (equal '("alpha") (dorg-heading-tags ps-h)))   ; non-vacuity precondition
    ;; Poison the first tag through the exposed image heading.
    (setcar (dorg-heading-tags exposed) "POISON")
    ;; DOrg's private working-model tag is untouched (deep copy, not aliased).
    (should (equal "alpha" (car (dorg-heading-tags ps-h))))))

(ert-deftest dorg-test-ext-index-context-image-is-copy-on-read-isolated ()
  "Each image read returns an INDEPENDENT deep copy: one consumer poisoning its
view (injecting an id, mutating a heading) cannot affect a later consumer's read.
Non-vacuity: a shared hash would carry the injection and mutation forward."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context)))
    ;; Consumer 1 poisons its own view.
    (let ((view1 (dorg-ext-index-context-candidate-postimage ctx)))
      (puthash 99999 (make-dorg-heading :id 99999 :title "INJECT") view1)
      (setf (dorg-heading-title (gethash 101 view1)) "MUTATED")
      (should (gethash 99999 view1)))                        ; non-vacuity: the write landed
    ;; Consumer 2 observes a pristine view.
    (let ((view2 (dorg-ext-index-context-candidate-postimage ctx)))
      (should-not (gethash 99999 view2))
      (should (equal "A*" (dorg-heading-title (gethash 101 view2)))))))

(ert-deftest dorg-test-ext-index-context-entry-snapshots-are-copy-on-read ()
  "Entry BEFORE/AFTER are copy-on-read deep copies: mutating one read affects
neither a later read nor DOrg state."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40) :tags (list "alpha")))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (e (dorg-test-ic--entry-by-id (dorg-ext-index-context-changed ctx) 101)))
    (should e)
    ;; Mutate one read of AFTER and BEFORE.
    (setf (dorg-heading-title (dorg-ext-index-entry-after e)) "X")
    (setcar (dorg-heading-tags (dorg-ext-index-entry-before e)) "Z")
    ;; A later read is pristine (fresh deep copy each call).
    (should (equal "A*" (dorg-heading-title (dorg-ext-index-entry-after e))))
    (should (equal "alpha" (car (dorg-heading-tags (dorg-ext-index-entry-before e)))))))

;;; ─── plain-org: construction precedes persistence ─────────────────────

(ert-deftest dorg-test-ext-index-context-plain-org-constructs-before-persistence ()
  "The plain-org index context is constructed AFTER core simulation and BEFORE
persistence — so it is a valid precommit projector input.  Direct receipt: the
construction event is observed strictly before the persistence (commit) event.
Non-vacuity: constructing in the return plist would order persist before it."
  (dorg-test-mutate--with-content buf
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((events nil)
          (real-ctx (symbol-function 'dorg-mutate--plain-org-index-context))
          (real-commit (symbol-function 'dorg-mutate--plain-org-commit)))
      (cl-letf (((symbol-function 'dorg-mutate--plain-org-index-context)
                 (lambda (&rest args) (push 'construct events) (apply real-ctx args)))
                ((symbol-function 'dorg-mutate--plain-org-commit)
                 (lambda (&rest args) (push 'persist events) (apply real-commit args))))
        (dorg-test-mutate--apply-patch-batch
         buf (list (dorg-test-mutate--property-put-patch 1 "CATEGORY" "work"))
         :save t))
      (setq events (nreverse events))
      ;; Both fired, and construction strictly precedes persistence.
      (should (memq 'construct events))
      (should (memq 'persist events))
      (should (< (cl-position 'construct events)
                 (cl-position 'persist events))))))

;;; ─── whole-context consumer isolation: composites + raw reachability ──

(ert-deftest dorg-test-ext-index-context-composites-are-consumer-isolated ()
  "Every exposed composite view is consumer-isolated in a PRODUCTION context:
a first consumer that `setcar's the patches or the changed-entry list cannot
corrupt what a later consumer reads.  Direct Navigator receipt on the real
`dorg-mutate--dolt-simulate' path (production-patches=(:poisoned),
changed-first=999).  Non-vacuity: returning the stored list by reference makes
the second consumer observe the poison."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (pristine-patches (dorg-ext-index-context-patches ctx)))
    ;; Preconditions: both composites are non-empty (else the test is vacuous).
    (should pristine-patches)
    (should (dorg-ext-index-context-changed ctx))
    ;; Consumer 1 poisons the SPINE of each composite it is handed.
    (let ((c1-patches (dorg-ext-index-context-patches ctx))
          (c1-changed (dorg-ext-index-context-changed ctx)))
      (setcar c1-patches :poisoned)
      (setcar c1-changed 999)
      (should (eq :poisoned (car c1-patches)))   ; non-vacuity: the write landed
      (should (eql 999 (car c1-changed))))
    ;; Consumer 2 observes pristine composites.
    (should (equal pristine-patches (dorg-ext-index-context-patches ctx)))
    (should-not (eq :poisoned (car (dorg-ext-index-context-patches ctx))))
    (let ((c2-changed (dorg-ext-index-context-changed ctx)))
      (should (dorg-ext-index-entry-p (car c2-changed)))
      (should-not (eql 999 (car c2-changed))))))

(ert-deftest dorg-test-ext-index-context-every-composite-is-copy-on-read ()
  "Each of the FIVE exposed composites (patches, created, changed, moved, deleted)
returns an independent copy per read, so a consumer's spine mutation never
reaches a later reader.  Built through the public constructor so all five lists
are guaranteed non-empty (a production sim rarely populates all four kinds at
once)."
  (let* ((h (make-dorg-heading :id 1 :parent-id 0 :level 1 :level-offset 1 :title "H"
                               :sort-key (unibyte-string #x40)))
         (mk (lambda (kind id) (dorg-ext-index-entry-create
                                :kind kind :id id :before h :after h)))
         (ctx (dorg-ext-index-context-create
               :source-kind 'dolt :root-id 100
               :patches (list :p1 :p2)
               :created (list (funcall mk 'create 11))
               :changed (list (funcall mk 'change 12))
               :moved (list (funcall mk 'move 13))
               :deleted (list (funcall mk 'delete 14)))))
    (dolist (probe (list (cons #'dorg-ext-index-context-patches :p1)
                         (cons #'dorg-ext-index-context-created 11)
                         (cons #'dorg-ext-index-context-changed 12)
                         (cons #'dorg-ext-index-context-moved 13)
                         (cons #'dorg-ext-index-context-deleted 14)))
      (let* ((reader (car probe))
             (first-read (funcall reader ctx)))
        (should first-read)                       ; non-vacuity: composite present
        (setcar first-read :POISON)               ; consumer 1 poisons its spine
        (should (eq :POISON (car first-read)))    ; non-vacuity: the write landed
        ;; A later consumer's read is pristine — the spine mutation did not
        ;; reach the stored composite (copy-on-read boundary).
        (should-not (eq :POISON (car (funcall reader ctx))))))))

(ert-deftest dorg-test-ext-index-context-no-raw-accessor-reaches-owned-state ()
  "The stored mutable images/snapshots are NOT reachable through any generated
struct accessor.  The `--raw' slot accessors that used to expose them must not
exist: reachability of the raw image is precisely how a first consumer poisons a
later consumer's copy-on-read image (Navigator receipt:
later-public-image-injected=t)."
  (should-not (fboundp 'dorg-ext-index-context-base-image--raw))
  (should-not (fboundp 'dorg-ext-index-context-candidate-postimage--raw))
  (should-not (fboundp 'dorg-ext-index-entry-before--raw))
  (should-not (fboundp 'dorg-ext-index-entry-after--raw)))

;;; ─── no generated accessor exposes owned state (vault introspection) ──

(ert-deftest dorg-test-ext-index-context-no-vault-accessor-is-generated ()
  "The owned state is not stored in a struct slot whose generated accessor would
return a closure over it.  The prior design's `---vault' slot accessors must not
exist: the Navigator recovered the raw image by introspecting the closure they
returned (context-vault-type=interpreted-function raw-image-reached=t)."
  (should-not (fboundp 'dorg-ext-index-context---vault))
  (should-not (fboundp 'dorg-ext-index-entry---vault)))

(ert-deftest dorg-test-ext-index-context-no-generated-accessor-exposes-owned-state ()
  "No `cl-defstruct'-generated accessor on the context or entry hands back an
object from which owned mutable state (images, snapshots, patches, entry lists) is
recoverable: every generated accessor yields a scalar or the bare opaque token —
never a closure (whose captured environment is introspectable via `aref') nor a
live owned hash.  Direct Navigator receipt: a slot-stored vault closure returned
by `...---vault' let `(aref it 2)' reach the captured `base-image'/`before' by
reference.  Non-vacuity: a closure-valued slot fails the `functionp' guard."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40) :tags (list "alpha")))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (entry (car (dorg-ext-index-context-changed ctx))))
    (should entry)
    (dolist (spec (list (cons 'dorg-ext-index-context ctx)
                        (cons 'dorg-ext-index-entry entry)))
      (let ((type (car spec)) (obj (cdr spec)))
        (dolist (slot (cl-struct-slot-info type))
          (let ((name (car slot)))
            (unless (eq name 'cl-tag-slot)
              (let* ((acc (intern (format "%s-%s" type name)))
                     (val (funcall acc obj)))
                ;; A generated accessor must never return a function (its captured
                ;; environment is reachable by reference) or a live owned hash.
                (should-not (functionp val))
                (should-not (hash-table-p val))))))))
    ;; And the copy-on-read boundary still holds end-to-end: a consumer poisoning
    ;; the owned image/entry it reads cannot reach DOrg's simulation state.
    (let ((ps-h (gethash 101 (dorg-mutate--ps-headings (plist-get sim :ps)))))
      (puthash 42 (make-dorg-heading :id 42) (dorg-ext-index-context-base-image ctx))
      (setf (dorg-heading-title (gethash 101 (dorg-ext-index-context-candidate-postimage ctx)))
            "POISON")
      (should-not (gethash 42 (dorg-ext-index-context-base-image ctx)))
      (should (equal "A*" (dorg-heading-title
                           (gethash 101 (dorg-ext-index-context-candidate-postimage ctx)))))
      (should-not (equal "POISON" (dorg-heading-title ps-h))))))

;;; ─── mutable-string isolation: identity/path slots + patch leaves ─────

(defun dorg-test-ic--find-string (tree wanted)
  "Return the first string `equal' to WANTED found in TREE, or nil.
Walks cons cells and vectors; returns the actual (mutable) string object so a
test can `aset' it to probe sharing."
  (cond
   ((and (stringp tree) (equal tree wanted)) tree)
   ((consp tree) (or (dorg-test-ic--find-string (car tree) wanted)
                     (dorg-test-ic--find-string (cdr tree) wanted)))
   ((vectorp tree)
    (let ((found nil) (i 0) (n (length tree)))
      (while (and (not found) (< i n))
        (setq found (dorg-test-ic--find-string (aref tree i) wanted) i (1+ i)))
      found))
   (t nil)))

(ert-deftest dorg-test-ext-index-context-string-identity-accessors-are-copy-on-read ()
  "Every string-valued identity/path accessor — DOLT-DIR, FILE, BASE-COMMIT, and
CANDIDATE-ID — is copied IN and copy-on-read: a consumer that `aset's a returned
string cannot change what a later consumer observes, and mutating the SOURCE string
after construction cannot reach the stored context.  Strings are mutable in Emacs,
so returning a stored string by reference falsifies the exact S0/candidate binding
(Navigator receipt: later-base=\"XASE-CAS\" later-dir=\"/Xmp/dorg-ic/\").  FILE is
exercised explicitly (its hand-written copy-on-read branch)."
  (let* ((dir (copy-sequence "/tmp/dorg-ic/"))
         (file (copy-sequence "/tmp/source.org"))
         (base (copy-sequence "BASE-CAS"))
         (cand (copy-sequence "c2c4deadbeef"))
         (ctx (dorg-ext-index-context-create
               :source-kind 'dolt :root-id 100
               :dolt-dir dir :file file :base-commit base :candidate-id cand)))
    ;; Copy-on-read isolation for EACH string accessor, FILE included.
    (dolist (probe (list (cons #'dorg-ext-index-context-dolt-dir "/tmp/dorg-ic/")
                         (cons #'dorg-ext-index-context-file "/tmp/source.org")
                         (cons #'dorg-ext-index-context-base-commit "BASE-CAS")
                         (cons #'dorg-ext-index-context-candidate-id "c2c4deadbeef")))
      (let* ((reader (car probe)) (want (cdr probe)) (s1 (funcall reader ctx)))
        (should (and (stringp s1) (> (length s1) 0)))     ; non-vacuity
        (aset s1 0 ?X)                                     ; consumer-1 poisons its read
        (should (eq ?X (aref s1 0)))                       ; non-vacuity: write landed
        (should-not (eq ?X (aref (funcall reader ctx) 0))) ; later read pristine
        (should (equal want (funcall reader ctx)))))       ; exact binding intact
    ;; Copy-in ownership: mutating the SOURCE strings cannot reach the context.
    (aset dir 0 ?Z) (aset file 0 ?Z) (aset base 0 ?Z) (aset cand 0 ?Z)
    (should (equal "/tmp/dorg-ic/" (dorg-ext-index-context-dolt-dir ctx)))
    (should (equal "/tmp/source.org" (dorg-ext-index-context-file ctx)))
    (should (equal "BASE-CAS" (dorg-ext-index-context-base-commit ctx)))
    (should (equal "c2c4deadbeef" (dorg-ext-index-context-candidate-id ctx)))))

(ert-deftest dorg-test-ext-index-context-patch-string-leaves-are-isolated ()
  "The canonical patches are deep-copied at BOTH copy-in and copy-on-read: a
consumer that `aset's a nested title string cannot poison a later reader OR the
source typed patch.  `copy-tree' copies only cons spines and shares mutable string
leaves (Navigator receipt: first=later=original=\"XRIGINAL\" same-string=t)."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (sim (dorg-test-ic--dolt-sim
               100 (list root a)
               (list (dorg-test-mutate--heading-update-patch 101 :title "A*"))))
         (ctx (plist-get sim :index-context))
         (leaf (dorg-test-ic--find-string
                (dorg-ext-index-context-patches ctx) "A*")))
    (should leaf)                                        ; non-vacuity: the leaf exists
    (aset leaf 0 ?X)                                     ; consumer-1 poisons a nested string
    (should (equal "X*" leaf))                           ; non-vacuity: the write landed
    ;; A later reader's patches still carry the pristine title.
    (should (dorg-test-ic--find-string
             (dorg-ext-index-context-patches ctx) "A*"))
    ;; The SOURCE typed patch batch is untouched (copy-in ownership).
    (should (dorg-test-ic--find-string (plist-get sim :patches) "A*"))))

(ert-deftest dorg-test-ext-index-context-patch-vector-leaves-are-isolated ()
  "The patch deep-copy recurses into VECTORS: a mutable string nested in a patch
vector, and the vector slot itself, are isolated at BOTH copy-in and copy-on-read.
`copy-tree' does not copy vectors at all, so it would share the vector and its
leaves; `dorg-ext--deep-copy' copies the vector and recurses its elements.
Non-vacuity: the consumer's mutations land on its own copy."
  (let* ((leaf (copy-sequence "VLEAF"))
         (vec (vector leaf "SLOT1"))
         (patch (list :op 'update :cells vec))
         (patches (list patch))
         (ctx (dorg-ext-index-context-create
               :source-kind 'dolt :root-id 1 :patches patches)))
    ;; Consumer 1 mutates a string leaf INSIDE the vector and a vector slot.
    (let* ((v1 (plist-get (car (dorg-ext-index-context-patches ctx)) :cells)))
      (should (vectorp v1))                                 ; non-vacuity
      (should (equal "VLEAF" (aref v1 0)))
      (aset (aref v1 0) 0 ?X)                               ; poison nested string leaf
      (aset v1 1 "REPLACED")                                ; poison a vector slot
      (should (equal "XLEAF" (aref v1 0)))                  ; non-vacuity: writes landed
      (should (equal "REPLACED" (aref v1 1))))
    ;; A later reader is pristine (vector recursion in the deep copy).
    (let ((v2 (plist-get (car (dorg-ext-index-context-patches ctx)) :cells)))
      (should (equal "VLEAF" (aref v2 0)))
      (should (equal "SLOT1" (aref v2 1))))
    ;; Copy-in ownership: mutating the SOURCE vector/leaf cannot reach the context.
    (aset leaf 0 ?Z)
    (aset vec 1 "SRCMUT")
    (let ((v3 (plist-get (car (dorg-ext-index-context-patches ctx)) :cells)))
      (should (equal "VLEAF" (aref v3 0)))
      (should (equal "SLOT1" (aref v3 1))))))


;;; ─── Final state wins over intermediate rekeys (review at bdae3d133) ─────

(defun dorg-test-ic--rekey-fixture (later)
  "Simulate an insert that rebalances owner 101's aged children, then LATER.
LATER is nil (rebalance only), `move' (move child 103 to the end of the same
parent) or `delete' (delete child 103, then insert another child after 102).
Runs the production simulate and owner-partition builder through the
fake-store harness; returns the sim plist."
  (let* ((headings
          (list (dorg-test-ic--heading 100 nil 0 "Root" #x80)
                (dorg-test-ic--heading 101 100 1 "Owner" #x40 :bead-id "owner")
                (dorg-test-ic--heading 102 101 2 "A" #x20)
                (dorg-test-ic--heading 103 101 2 "B" #x40)
                (dorg-test-ic--heading 104 101 2 "C" #x60)))
         (patches (list (dorg-test-mutate--tree-insert-patch
                         (dorg-test-mutate--heading-address 101)
                         '(:id 300 :title "Inserted")
                         :before (dorg-test-mutate--heading-address 102)))))
    (dolist (h (cddr headings))
      (setf (dorg-heading-sort-key h)
            (dorg-test-mutate--aged-key (- (dorg-heading-id h) 101))))
    (pcase later
      ('move
       (setq patches (append patches
                             (list (dorg-test-mutate--move-patch
                                    103 (dorg-test-mutate--heading-address 101))))))
      ('delete
       (setq patches (append patches
                             (list (dorg-test-mutate--delete-patch 103 t)
                                   (dorg-test-mutate--tree-insert-patch
                                    (dorg-test-mutate--heading-address 101)
                                    '(:id 301 :title "After A")
                                    :after (dorg-test-mutate--heading-address 102)))))))
    (dorg-test-ic--dolt-sim 100 headings patches "H0" '(:property "BEAD_ID"))))

(defun dorg-test-ic--owner-children-order (sim)
  "Return (CORE . PROJECTED): child ids of owner 101 in key order from the PS and the owner image."
  (let* ((ps (plist-get sim :ps))
         (ctx (plist-get sim :index-context))
         (owner (dorg-test-ic--owner-partition-by-value
                 (dorg-ext-index-context-owner-partitions ctx) "owner"))
         (image (dorg-ext-owner-partition-candidate-postimage owner)))
    (cl-flet ((order (table)
                (mapcar #'car
                        (sort (let (cells)
                                (maphash (lambda (id h)
                                           (when (eql 101 (dorg-heading-parent-id h))
                                             (push (cons id (dorg-heading-sort-key h)) cells)))
                                         table)
                                cells)
                              (lambda (a b) (string< (cdr a) (cdr b)))))))
      (cons (order (dorg-mutate--ps-headings ps)) (order image)))))

(ert-deftest dorg-test-ic-rekey-only-owner-image-matches-core ()
  "Control: a rebalance alone leaves the owner image equal to the core state."
  (let* ((sim (dorg-test-ic--rekey-fixture nil))
         (orders (dorg-test-ic--owner-children-order sim)))
    (should (equal (car orders) '(300 102 103 104)))
    (should (equal (car orders) (cdr orders)))
    (should (null (plist-get sim :changed)))))

(ert-deftest dorg-test-ic-rekey-then-move-final-key-wins ()
  "F1: a move after a rebalance owns the row's final key in core, lowering and the owner image."
  (let* ((sim (dorg-test-ic--rekey-fixture 'move))
         (ps (plist-get sim :ps))
         (orders (dorg-test-ic--owner-children-order sim))
         (ctx (plist-get sim :index-context))
         (owner (dorg-test-ic--owner-partition-by-value
                 (dorg-ext-index-context-owner-partitions ctx) "owner"))
         (final (dorg-heading-sort-key (gethash 103 (dorg-mutate--ps-headings ps))))
         (projected (dorg-heading-sort-key
                     (gethash 103 (dorg-ext-owner-partition-candidate-postimage owner)))))
    (should (member 103 (plist-get sim :changed)))
    (should (equal final projected))
    (should (equal (car orders) '(300 102 104 103)))
    (should (equal (car orders) (cdr orders)))
    ;; No pending rekey survives for a row a later semantic op re-keyed, and
    ;; no rekey op names it.
    (should-not (gethash 103 (dorg-mutate--ps-rekeys ps)))
    (should-not (cl-some (lambda (op)
                           (and (eq (car op) :rekey) (assq 103 (caddr op))))
                         (plist-get sim :db-ops)))))

(ert-deftest dorg-test-ic-rekey-then-delete-leaves-no-pending-row ()
  "F2: a deleted rekeyed row is neither a pending ordering row nor in the owner image,
and a later placement in the same batch orders correctly without it."
  (let* ((sim (dorg-test-ic--rekey-fixture 'delete))
         (ps (plist-get sim :ps))
         (delta (dorg-mutate--ps-order-delta ps 101))
         (orders (dorg-test-ic--owner-children-order sim)))
    (should (member 103 (plist-get sim :deleted)))
    (should-not (gethash 103 (dorg-mutate--ps-headings ps)))
    (should-not (assq 103 (cdr delta)))
    (should (member 103 (car delta)))
    (should-not (gethash 103 (dorg-mutate--ps-rekeys ps)))
    (should (equal (car orders) '(300 102 301 104)))
    (should (equal (car orders) (cdr orders)))))

(provide 'dorg-ext-index-context-tests)
;;; dorg-ext-index-context-tests.el ends here
