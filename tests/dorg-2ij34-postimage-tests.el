;;; dorg-2ij34-postimage-tests.el --- Full transaction proof -*- lexical-binding: t; -*-

;;; Commentary:

;; Pre-commit core/item/owner/extension parity, compact idempotency receipts,
;; exact scoped revision reads and text fidelity.  Derived payload/wire-only
;; assertions retire; their transaction corruption and identity controls stay.
;; Material tests use fresh owned fixtures, never live stores.  Pure read-port
;; and bounded hand-off controls also run directly without a Dolt fixture.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-test-helpers)
(require 'dorg-mutate-tests)
(require 'dorg-ext-index-projector-tests)

(defconst dorg-test-2ij34--seed
  (concat "* Alpha\n"
          ":PROPERTIES:\n:BEAD_ID: bead-alpha\n:END:\n"
          "alpha body\n"
          "** Design\ndesign body\n"
          "* Beta\n"
          ":PROPERTIES:\n:BEAD_ID: bead-beta\n:END:\n"
          "beta body\n")
  "Two identity-owned headings; Design is an owned child of Alpha.")

(defun dorg-test-2ij34--src (dolt-dir rid)
  "Return an identity-carrying mutation source for DOLT-DIR / RID."
  (dorg-mutate--resolve-source
   (list :dolt-dir dolt-dir :root-id rid
         :identity '(:property "BEAD_ID"))))

(defun dorg-test-2ij34--state (dolt-dir)
  "Return a comparable snapshot of DOLT-DIR's durable state."
  (list :head (dorg-head-commit dolt-dir)
        :titles (dorg-sql dolt-dir
                          "SELECT id, title FROM headings ORDER BY id")
        :log (alist-get 'n (car (dorg-sql
                                 dolt-dir
                                 "SELECT COUNT(*) AS n FROM dolt_log")))))

;;; ─── Positive identity-owner transformations ─────────────────────────────

(ert-deftest dorg-test-2ij34-single-owner-apply-keeps-parity-without-wire ()
  "An identity owner commits its validated state without derived wire fields."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (before (dorg-head-commit dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--heading-update-patch
                            alpha :title "Alpha renamed")))))
      (should (plist-get res :ok))
      (should-not (equal before (dorg-head-commit dolt-dir)))
      (should (= alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha renamed")))
      (should-not (plist-member res :postimages))
      (should-not (plist-member res :candidate-models)))))

(ert-deftest dorg-test-2ij34-multi-owner-batch-keeps-parity-without-wire ()
  "A genuine two-owner batch commits both validated transformations."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (before (dorg-head-commit dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--heading-update-patch alpha :title "Alpha two")
                           (dorg-test-mutate--heading-update-patch beta :title "Beta two")))))
      (should (plist-get res :ok))
      (should-not (equal before (dorg-head-commit dolt-dir)))
      (should (= alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha two")))
      (should (= beta (dorg-test-mutate--id-by-title dolt-dir "Beta two")))
      (should-not (plist-member res :postimages)))))

(ert-deftest dorg-test-2ij34-deleted-owner-apply-keeps-absence-proof ()
  "Deleting an identity owner removes its rows without a wire reconstruction."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (before (dorg-head-commit dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--delete-patch beta t)))))
      (should (plist-get res :ok))
      (should-not (equal before (dorg-head-commit dolt-dir)))
      (should-not (dorg-test-mutate--id-by-title dolt-dir "Beta"))
      (should-not (gethash beta (dorg-model-load-tree dolt-dir rid)))
      (should-not (plist-member res :postimages)))))

(ert-deftest dorg-test-2ij34-receipt-noop-emits-none ()
  "AC1 boundary control (may pass at trunk): empty net effect emits no
commit and no postimage entries."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (s0 (dorg-test-2ij34--state dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--heading-update-patch
                            alpha :title "Alpha")))))
      (should (equal s0 (dorg-test-2ij34--state dolt-dir)))
      (should-not (plist-get res :postimages)))))

;;; ─── AC2 — the transaction proof, non-vacuous ───────────────────────

(defmacro dorg-test-2ij34--with-injected-statement (statement &rest body)
  "Run BODY with STATEMENT appended to every lease batch's statements.
The injection lands after the planned statements and before the Dolt
commit — inside the window the validate-function must police."
  (declare (indent 1))
  `(let ((real (symbol-function 'dorg-sql--lease-batch)))
     (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                (lambda (lease statements &optional message validate report)
                  (funcall real lease
                           (append statements (list ,statement))
                           message validate report))))
       ,@body)))

(ert-deftest dorg-test-2ij34-validator-aborts-core-corruption ()
  "AC2 RED: core rows corrupted after the planned statements must abort.
On trunk there is no validator, so the corruption COMMITS and this fails."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (s0 (dorg-test-2ij34--state dolt-dir)))
      (dorg-test-2ij34--with-injected-statement
          (format "UPDATE headings SET title = 'corrupted' WHERE id = %d"
                  alpha)
        (should-error
         (dorg-test-mutate--apply-patch-batch
          src (list (dorg-test-mutate--heading-update-patch
                     alpha :title "Alpha honest")))
         :type 'dorg-mutate-error))
      ;; Zero durable delta: the abort rolled everything back.
      (should (equal s0 (dorg-test-2ij34--state dolt-dir))))))

(ert-deftest dorg-test-2ij34-validator-aborts-row-corruption ()
  "AC2 RED: extension rows corrupted post-statements must abort.
Uses the demo index projector as the extension row plane."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
             (src (dorg-test-2ij34--src dolt-dir rid))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (s0 nil))
        ;; First apply installs the derived row cleanly.
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch
                    alpha :title "Alpha seeded")))
        (setq s0 (dorg-test-2ij34--state dolt-dir))
        (dorg-test-2ij34--with-injected-statement
            (format "UPDATE demo_ix SET title = 'row corrupted' WHERE heading_id = %d"
                    alpha)
          (should-error
           (dorg-test-mutate--apply-patch-batch
            src (list (dorg-test-mutate--heading-update-patch
                       alpha :title "Alpha again")))
           :type 'dorg-mutate-error))
        (should (equal s0 (dorg-test-2ij34--state dolt-dir)))
        (should (equal "Alpha seeded"
                       (dorg-test-ixp--scalar
                        dolt-dir
                        (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                alpha))))))))

;;; ─── Compact operation evidence ─────────────────────────────────────────

(ert-deftest dorg-test-2ij34-compact-evidence-row-omits-derived-payload ()
  "An identity mutation atomically commits a compact row and NULL legacy payload."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (op-id "test:compact:identity")
           (res (dorg-api-execute
                 `(on (id ,alpha) (set (heading title) "Alpha compact"))
                 :mode 'apply :source src :op-id op-id))
           (rows (dorg-sql dolt-dir
                           (format (concat "SELECT op_id AS op_id, gen_id AS gen_id,"
                                           " intent_digest AS intent_digest, ts AS ts,"
                                           " postimages_v1 AS postimages_v1"
                                           " FROM dorg_mutation_log WHERE op_id = %s")
                                   (dorg-sql-quote op-id)))))
      (should (plist-get res :ok))
      (should (= 1 (length rows)))
      (should (equal op-id (alist-get 'op_id (car rows))))
      (should (stringp (alist-get 'gen_id (car rows))))
      (should (stringp (alist-get 'intent_digest (car rows))))
      (should-not (alist-get 'postimages_v1 (car rows)))
      (should-not (plist-member res :postimages)))))

(ert-deftest dorg-test-2ij34-validator-aborts-item-row-corruption-intact-hash ()
  "AC2 RED (K#025 B1): item ROWS corrupted while the stored items_hash
column stays intact must abort — the committed-core proof recomputes item
digests from the item rows for every affected partition, unconditionally,
never only under a model request.  At the K#025 baseline the corruption
COMMITS because the unconditional path trusts the hash column."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (s0 (dorg-test-2ij34--state dolt-dir)))
      (dorg-test-2ij34--with-injected-statement
          (format (concat "UPDATE text_items SET text_raw = 'row corrupted'"
                          " WHERE item_id IN"
                          " (SELECT id FROM heading_items WHERE heading_id = %d)")
                  alpha)
        (should-error
         (dorg-test-mutate--apply-patch-batch
          src (list (dorg-test-mutate--heading-update-patch
                     alpha :title "Alpha honest")))
         :type 'dorg-mutate-error))
      (should (equal s0 (dorg-test-2ij34--state dolt-dir)))
      (should (equal "alpha body\n"
                     (alist-get
                      'text_raw
                      (car (dorg-sql
                            dolt-dir
                            (format (concat "SELECT ti.text_raw FROM text_items ti"
                                            " JOIN heading_items hi ON ti.item_id = hi.id"
                                            " WHERE hi.heading_id = %d")
                                    alpha)))))))))

(defconst dorg-test-2ij34--xroot-src-seed
  (concat "* Movable\n:PROPERTIES:\n:BEAD_ID: bead-mov\n:END:\nmov body\n"
          "** Kid\nkid body\n"
          "* SrcSib\n:PROPERTIES:\n:BEAD_ID: bead-sib\n:END:\n")
  "Source root for cross-root witnesses: identity owner Movable (101)
with owner-local Kid (102), identity sibling SrcSib (103).")

(defconst dorg-test-2ij34--xroot-dst-seed
  "* DestParent\n:PROPERTIES:\n:BEAD_ID: bead-dst\n:END:\n"
  "Destination root for cross-root witnesses: identity owner DestParent (501).")

(defun dorg-test-2ij34--xroot-apply (dolt-dir)
  "Seed both roots and apply the identity cross-root move; return the result."
  (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-src-seed 100)
  (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-dst-seed 500)
  (let ((src (dorg-test-2ij34--src dolt-dir 100))
        (dst (dorg-mutate--resolve-source
              (list :dolt-dir dolt-dir :root-id 500
                    :identity '(:property "BEAD_ID")))))
    (dorg-test-mutate--apply-patch-batch
     src (list (dorg-test-mutate--move-patch
                101 (dorg-test-mutate--heading-address 501)
                :to-source dst)))))

(ert-deftest dorg-test-2ij34-cross-root-proof-still-covers-both-sides ()
  "The real validator covers deleted source and present destination owner scope."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((proof (symbol-function 'dorg-mutate--postimage-assert-partitions))
          (seen nil))
      (cl-letf (((symbol-function 'dorg-mutate--postimage-assert-partitions)
                 (lambda (read specs)
                   (setq seen (copy-tree specs))
                   (funcall proof read specs))))
        (should (plist-get (dorg-test-2ij34--xroot-apply dolt-dir) :ok)))
      (should (cl-find-if (lambda (spec)
                           (and (= 100 (nth 0 spec)) (= 101 (nth 1 spec))
                                (nth 4 spec))) seen))
      (should (cl-find-if (lambda (spec)
                           (and (= 500 (nth 0 spec)) (= 101 (nth 1 spec))
                                (not (nth 4 spec)))) seen)))))

(ert-deftest dorg-test-2ij34-validator-aborts-root-misplacement-cross-root ()
  "AC2 RED (K#025 B1): a cross-root move whose re-homed row is corrupted
back to the source root after the planned statements must abort — root
placement is part of the committed-core proof (the per-partition readback
binds every expected row to its partition's root).  At the K#025 baseline
the misplacement COMMITS because no root fact is verified."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-src-seed 100)
    (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--xroot-dst-seed 500)
    (let* ((src (dorg-test-2ij34--src dolt-dir 100))
           (dst (dorg-mutate--resolve-source
                 (list :dolt-dir dolt-dir :root-id 500
                       :identity '(:property "BEAD_ID"))))
           (s0 (dorg-test-2ij34--state dolt-dir)))
      (dorg-test-2ij34--with-injected-statement
          "UPDATE headings SET root_id = 100 WHERE id = 101"
        (should-error
         (dorg-test-mutate--apply-patch-batch
          src (list (dorg-test-mutate--move-patch
                     101 (dorg-test-mutate--heading-address 501)
                     :to-source dst)))
         :type 'dorg-mutate-error))
      (should (equal s0 (dorg-test-2ij34--state dolt-dir))))))

(ert-deftest dorg-test-2ij34-validator-tolerates-semicolon-content ()
  "AC2 RED: content values containing semicolons must not false-abort the
transaction proof.  The demo extension row carries the heading title; the
readback predicate embeds it as a quoted literal, and the SELECT-only
window guard must treat quoted semicolons as data, not statement
separators.  Predicted failure at the RED baseline: the guard rejects the
readback query and the honest apply aborts."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
             (src (dorg-test-2ij34--src dolt-dir rid))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (res (dorg-test-mutate--apply-patch-batch
                   src (list (dorg-test-mutate--heading-update-patch
                              alpha :title "Alpha; still honest")))))
        (should (plist-get res :ok))
        (should (equal "Alpha; still honest"
                       (dorg-test-ixp--scalar
                        dolt-dir
                        (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                alpha))))))))

;;; ─── Compact settled replay and explicit revision reads ──────────────────

(ert-deftest dorg-test-2ij34-replay-settled-op-keeps-original-compact-result ()
  "Settled replay keeps original commit/op-id without derived response fields."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha replayed")))
           (op-id "test:compact:retry")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id))
           (head (dorg-head-commit dolt-dir))
           (res2 (dorg-api-execute form :mode 'apply :source src :op-id op-id)))
      (should (plist-get res1 :ok))
      (should (plist-get res2 :idempotent))
      (should (equal op-id (plist-get res2 :op-id)))
      (should (equal (plist-get res1 :new-token) (plist-get res2 :new-token)))
      (should (equal head (dorg-head-commit dolt-dir)))
      (should-not (plist-member res2 :postimages))
      (should-not (plist-member res2 :candidate-models)))))

(ert-deftest dorg-test-2ij34-replay-commit-supports-explicit-scoped-revision-read ()
  "A known heading can be loaded explicitly at the ORIGINAL recovered revision.
Replay does not invent an owner/model scope or restore current HEAD."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha v1")))
           (op-id "test:compact:revision")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id)))
      (should (plist-get res1 :ok))
      (should (plist-get
               (dorg-api-execute `(on (id ,alpha) (set (heading title) "Alpha v2"))
                                 :mode 'apply :source src :op-id "test:compact:later") :ok))
      (let* ((head (dorg-head-commit dolt-dir))
             (res2 (dorg-api-execute form :mode 'apply :source src :op-id op-id))
             (loaded (dorg-model-load-headings
                      dolt-dir rid (list alpha) (plist-get res2 :new-token))))
        (should (plist-get res2 :idempotent))
        (should (equal (plist-get res1 :new-token) (plist-get res2 :new-token)))
        (should (equal "Alpha v1" (dorg-heading-title (gethash alpha loaded))))
        (should (= alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha v2")))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should-not (plist-member res2 :candidate-models))))))

(ert-deftest dorg-test-2ij34-replay-legacy-row-old-caller-unchanged ()
  "A NULL legacy column has no bearing on compact idempotent recovery."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha old")))
           (op-id "test:2ij34:replay-legacy:2")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id)))
      (should (plist-get res1 :ok))
      (dorg-sql-batch dolt-dir
                      (list (format
                             "UPDATE dorg_mutation_log SET postimages_v1 = NULL WHERE op_id = %s"
                             (dorg-sql-quote op-id)))
                      "test: simulate legacy evidence row")
      (let ((res2 (dorg-api-execute form :mode 'apply :source src
                                    :op-id op-id)))
        (should (plist-get res2 :ok))
        (should (plist-get res2 :idempotent))
        (should (equal (plist-get res1 :new-token) (plist-get res2 :new-token)))
        (should-not (plist-get res2 :postimages))))))

;;; ─── Pure item grammar and transaction-read-port controls ─────────────────

(defun dorg-test-2ij34--static-partition (&optional title)
  "Return a minimal synthetic owner partition for static pins."
  (let ((base (make-hash-table :test 'eql))
        (cand (make-hash-table :test 'eql)))
    (puthash 7 (make-dorg-heading :id 7 :parent-id nil :level 1 :level-offset 1
                                  :title "before" :post-blank 0)
             base)
    (puthash 7 (make-dorg-heading :id 7 :parent-id nil :level 1 :level-offset 1
                                  :title (or title "after") :post-blank 0)
             cand)
    (dorg-ext-owner-partition-create
     :root-id 1 :owner-id 7 :owner-key "BEAD_ID" :owner-value "bead-x"
     :parent-owner-id nil :parent-owner-value nil
     :affected-ids '(7) :affected-paths nil
     :base-image base :candidate-postimage cand)))

(ert-deftest dorg-test-2ij34-static-public-item-grammar-keeps-post-blank ()
  "The public item grammar preserves all kinds and explicit item spacing.
No wire serializer participates: constructor forms lower through real API
normalization and real model construction."
  (dolist
      (case
       (list
        (cons '(text "body" :post-blank 2)
              (dorg-text-item :post-blank 2 :text "body"))
        (cons '(keyword "K" "V" :post-blank 1)
              (dorg-keyword-item :post-blank 1 :keyword "K" :value "V"))
        (cons '(planning :scheduled "<s>" :closed "[c]" :post-blank 0)
              (dorg-planning-item :post-blank 0 :scheduled "<s>" :closed "[c]"))
        (cons '(drawer "P" ("A" "1") :post-blank 1)
              (dorg-drawer-item :post-blank 1 :name "P" :properties '(("A" . "1"))))
        (cons '(drawer "L" :lines ("l1" "l2") :post-blank 0)
              (dorg-drawer-item :post-blank 0 :name "L" :lines '("l1" "l2")))))
    (let* ((value (dorg-api--item-value (car case)))
           (rebuilt (dorg-mutate--item-from-value value)))
      (should (equal (dorg-item-post-blank (cdr case))
                     (plist-get value :post-blank)))
      (should (equal (dorg-item-post-blank (cdr case))
                     (dorg-item-post-blank rebuilt)))
      (should (dorg-model--item-semantic-equal-p (cdr case) rebuilt))))
  ;; The extended grammar validates: :post-blank must be a non-negative
  ;; integer, at the API boundary and at the engine boundary alike.
  (should-error (dorg-api--item-value '(text "b" :post-blank -1))
                :type 'dorg-api-error)
  (should-error (dorg-api--item-value '(text "b" :post-blank "x"))
                :type 'dorg-api-error)
  (should-error (dorg-mutate--patch-validate-item-value
                 '(:kind text :text "b" :post-blank -1) 'test)
                :type 'dorg-mutate-error)
  (should-error (dorg-mutate--patch-validate-item-value
                 '(:kind text :text "b" :post-blank "x") 'test)
                :type 'dorg-mutate-error)
  ;; K#027: a PRESENT :post-blank must be a non-negative integer — an
  ;; explicit nil is noncanonical and refused at the engine seam exactly
  ;; as at the public boundary; ABSENCE remains valid.
  (should-error (dorg-mutate--patch-validate-item-value
                 '(:kind text :text "b" :post-blank nil) 'test)
                :type 'dorg-mutate-error)
  (should (dorg-mutate--patch-validate-item-value
           '(:kind text :text "b") 'test))
  (should (dorg-mutate--patch-validate-item-value
           '(:kind text :text "b" :post-blank 2) 'test)))

(ert-deftest dorg-test-2ij34-static-item-post-blank-remains-in-validation-hash ()
  "Two items differing only in spacing remain distinguishable by the core proof."
  (let* ((a (dorg-text-item :post-blank 0 :text "b"))
         (b (dorg-text-item :post-blank 2 :text "b"))
         (ha (make-dorg-heading :id 7 :items (list a)))
         (hb (make-dorg-heading :id 7 :items (list b))))
    (should (dorg-model--item-semantic-equal-p a b))
    (should-not (equal (dorg-item-post-blank a) (dorg-item-post-blank b)))
    (dorg-heading-compute-items-hash ha)
    (dorg-heading-compute-items-hash hb)
    (should-not (equal (dorg-mutate--index-heading-canonical ha)
                       (dorg-mutate--index-heading-canonical hb)))))

(ert-deftest dorg-test-e3cah-postimage-plan-uses-typed-read-port ()
  "tracked-regression RED: the 2ij34 validator receives a typed read port, not
the legacy SELECT-string callback.

The wrapper shape is intentionally small and internal.  It exists only to let
`dorg-sql--lease-batch' keep the legacy administrative SELECT verifier seam
while the postimage proof path opts into the closed transaction-read port."
  (let* ((ctx (dorg-ext-index-context-create
               :source-kind 'dolt :root-id 1 :from-root-id nil :to-root-id nil
               :patches nil :created nil :changed nil :moved nil :deleted nil
               :base-image (make-hash-table :test 'eql)
               :candidate-postimage (make-hash-table :test 'eql)
               :owner-partitions (list (dorg-test-2ij34--static-partition))
               :candidate-id (make-string 40 ?c) :base-commit "base"))
         (proof (dorg-mutate--postimage-plan ctx nil))
         (validate (plist-get proof :validate)))
    (should (equal '(:validate) (cl-loop for (key _value) on proof by #'cddr
                                        collect key)))
    (should (listp validate))
    (should (eq 'transaction-read-port (plist-get validate :mode)))
    (should (functionp (plist-get validate :function)))))

(ert-deftest dorg-test-e3cah-transaction-read-port-refuses-escape-requests ()
  "tracked-regression RED: the transaction proof port has no SQL escape hatch.

Every invalid request below must fail before touching a DB connection: raw SQL
strings, unknown versions/ops, unbounded row materialization, oversized id
sets, SQL-shaped predicates, core tables through the extension-count family,
and unowned extension tables."
  (let ((port (dorg-sql--transaction-read-port nil))
        (too-many (number-sequence
                   1 (1+ dorg-sql-transaction-read-port-max-ids))))
    (dolist (request
             (list
              "SELECT 1"
              '(:version 2 :op core-heading-rows :ids (1) :root-id 1)
              '(:version 1 :op raw-select :sql "SELECT 1")
              '(:version 1 :op core-heading-rows :ids nil :root-id 1)
              `(:version 1 :op core-heading-rows :ids ,too-many :root-id 1)
              '(:version 1 :op owner-model :ids nil :root-id 1)
              '(:version 1 :op extension-rows :table "demo_ix"
                :where ((heading_id . 1)))
              '(:version 1 :op extension-row-count :table "headings"
                :where ((id . 1)))))
      (should-error (funcall port request)
                    :type 'dorg-sql-transaction-read-port-error)))
  (let ((port (dorg-sql--transaction-read-port nil '("demo_ix"))))
    (should-error
     (funcall port
              '(:version 1 :op extension-row-count :table "demo_ix"
                :where "heading_id = 1"))
     :type 'dorg-sql-transaction-read-port-error))
  (let ((port (dorg-sql--transaction-read-port nil '("other_ix"))))
    (should-error
     (funcall port
              '(:version 1 :op extension-row-count :table "demo_ix"
                :where ((heading_id . 1))))
     :type 'dorg-sql-transaction-read-port-error)))

;;; ─── Transaction read port keeps decoder types ────────────────────────

(defun dorg-test-sql--typed-port-rows (sql)
  "Answer SQL as the metadata-typed MySQL decoder does for one heading.
Heading 1 has title \"007\", tag \"007\", a text item \"007\", a keyword item
KEY = \"1.250\", a property drawer with value \"0.50\" and a generic drawer
line \"042\".  Ids, offsets and counts are integers, NULL columns are nil and
HEX() results are strings.  Each call returns fresh conses."
  (copy-tree
   (cond
    ((string-match-p "COUNT(\\*)" sql) '(((n . 2))))
    ((string-match-p "FROM drawer_lines" sql)
     '(((drawer_item_id . 104) (line_raw . "042"))))
    ((string-match-p "FROM property_entries" sql)
     '(((drawer_item_id . 103) (prop_key . "LIMIT") (prop_value . "0.50"))))
    ((string-match-p "FROM heading_items" sql)
     '(((item_id . 101) (heading_id . 1) (kind . "text") (post_blank . 0)
        (item_sort_key . "01") (text_raw . "007") (keyword) (value_raw)
        (scheduled_raw) (deadline_raw) (closed_raw) (drawer_name))
       ((item_id . 102) (heading_id . 1) (kind . "keyword") (post_blank . 0)
        (item_sort_key . "02") (text_raw) (keyword . "KEY") (value_raw . "1.250")
        (scheduled_raw) (deadline_raw) (closed_raw) (drawer_name))
       ((item_id . 103) (heading_id . 1) (kind . "drawer") (post_blank . 0)
        (item_sort_key . "03") (text_raw) (keyword) (value_raw)
        (scheduled_raw) (deadline_raw) (closed_raw) (drawer_name . "PROPERTIES"))
       ((item_id . 104) (heading_id . 1) (kind . "drawer") (post_blank . 0)
        (item_sort_key . "04") (text_raw) (keyword) (value_raw)
        (scheduled_raw) (deadline_raw) (closed_raw) (drawer_name . "NOTES"))))
    ((string-match-p "FROM heading_tags" sql)
     '(((heading_id . 1) (tag . "007"))))
    ((string-match-p "\\`SELECT id FROM headings" sql) '(((id . 1))))
    (t '(((id . 1) (parent_id) (sort_key . "01") (level_offset . 0)
          (todo_keyword) (priority) (commented . 0) (title . "007")
          (post_blank . 0) (items_hash)))))))

(defconst dorg-test-sql--typed-port-items
  '("text:0:007" "keyword:0:KEY:1.250" "drawer:0:PROPERTIES:LIMIT=0.50"
    "drawer:0:NOTES:042")
  "Content strings of heading 1's items in `dorg-test-sql--typed-port-rows'.")

(defun dorg-test-sql--assert-typed-port (read table)
  "Invoke all five READ operations on heading 1 and extension TABLE.
Assert each result keeps the decoder's types: integer ids and count, nil
NULLs, string HEX, and numeric-looking text that hydrates and deep-copies
unchanged."
  (let* ((core (funcall read '(:version 1 :op core-heading-rows
                               :ids (1) :root-id 1)))
         (tags (funcall read '(:version 1 :op core-heading-tags :ids (1))))
         (ids (funcall read '(:version 1 :op core-heading-ids :ids (1))))
         (image (funcall read '(:version 1 :op owner-model
                                :root-id 1 :ids (1))))
         (count (funcall read (list :version 1 :op 'extension-row-count
                                    :table table :where '((root_id . 1)))))
         (heading (gethash 1 image)))
    (should (equal '((id . 1) (parent_id) (sort_key . "01") (level_offset . 0)
                     (todo_keyword) (priority) (commented . 0) (title . "007")
                     (post_blank . 0) (items_hash))
                   (car core)))
    (should (equal '(((heading_id . 1) (tag . "007"))) tags))
    (should (equal '(((id . 1))) ids))
    (should (equal "007" (dorg-heading-title heading)))
    (should (equal '("007") (dorg-heading-tags heading)))
    (should (equal dorg-test-sql--typed-port-items
                   (mapcar #'dorg-item-content-string (dorg-heading-items heading))))
    (let ((copy (dorg-heading-deep-copy heading)))
      (should (equal "007" (dorg-heading-title copy)))
      (should (equal dorg-test-sql--typed-port-items
                     (mapcar #'dorg-item-content-string (dorg-heading-items copy)))))
    (should (eql 2 count))))

(ert-deftest dorg-test-sql-transaction-read-port-keeps-decoder-types ()
  "All five closed read operations return the decoder's typed rows unchanged.
`dorg-sql--mysql-exec' answers as the metadata-typed MySQL decoder does, so
the test isolates the read port: a numeric-looking title, tag and owner-model
text stay strings and deep-copy, and ids and the count stay integers."
  (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
             (lambda (sql _conn) (dorg-test-sql--typed-port-rows sql))))
    (dorg-test-sql--assert-typed-port
     (dorg-sql--transaction-read-port 'conn '("demo_ix")) "demo_ix")))

(ert-deftest dorg-test-sql-transaction-read-port-count-is-a-native-integer ()
  "The extension count accepts only a nonnegative integer COUNT(*) result.
A missing row or column, SQL NULL, a negative, a float or a string is a typed
refusal; it is never defaulted to zero or converted by its spelling."
  (dolist (case '(((((n . 0))) . 0) ((((n . 2))) . 2)))
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (_sql _conn) (copy-tree (car case)))))
      (should (eql (cdr case)
                   (funcall (dorg-sql--transaction-read-port 'conn '("demo_ix"))
                            '(:version 1 :op extension-row-count
                              :table "demo_ix" :where nil))))))
  (dolist (rows '(nil (((m . 2))) (((n))) (((n . -1))) (((n . 1.5)))
                  (((n . "2")))))
    (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
               (lambda (_sql _conn) (copy-tree rows))))
      (let ((err (should-error
                  (funcall (dorg-sql--transaction-read-port 'conn '("demo_ix"))
                           '(:version 1 :op extension-row-count
                             :table "demo_ix" :where nil))
                  :type 'dorg-sql-transaction-read-port-error)))
        (should (eq 'bad-count (plist-get (nth 2 err) :reason)))))))

(ert-deftest dorg-test-sql-transaction-read-port-keeps-native-types ()
  "On pinned Dolt the five read operations keep the server's typed values.
Seeds numeric-looking title, tag, text, keyword, property and drawer-line
text, integer ids, NULL columns, a binary sort key and an owned extension
table, then reads them through the real port on the store's own connection.
Needs the server transport."
  (dorg-test-mutate--with-dolt
    (unless (eq 'mysql (dorg-sql--transport-kind dolt-dir))
      (ert-skip "the transaction read port needs the server transport"))
    (dorg-sql-batch
     dolt-dir
     (list
      (concat "INSERT INTO headings (id, root_id, parent_id, sort_key,"
              " level_offset, title) VALUES (1, 1, NULL, X'01', 0, '007')")
      "INSERT INTO heading_tags (heading_id, ordinal, tag) VALUES (1, 0, '007')"
      (concat "INSERT INTO heading_items (id, heading_id, sort_key, kind,"
              " post_blank) VALUES (101, 1, X'01', 'text', 0),"
              " (102, 1, X'02', 'keyword', 0), (103, 1, X'03', 'drawer', 0),"
              " (104, 1, X'04', 'drawer', 0)")
      "INSERT INTO text_items (item_id, text_raw) VALUES (101, '007')"
      (concat "INSERT INTO keyword_items (item_id, keyword, value_raw)"
              " VALUES (102, 'KEY', '1.250')")
      (concat "INSERT INTO drawer_items (item_id, name)"
              " VALUES (103, 'PROPERTIES'), (104, 'NOTES')")
      (concat "INSERT INTO property_entries (drawer_item_id, ordinal, prop_key,"
              " prop_value, raw_line) VALUES (103, 0, 'LIMIT', '0.50',"
              " ':LIMIT: 0.50')")
      (concat "INSERT INTO drawer_lines (drawer_item_id, ordinal, line_raw)"
              " VALUES (104, 0, '042')")
      "CREATE TABLE typed_port_ix (root_id BIGINT, payload TEXT)"
      "INSERT INTO typed_port_ix VALUES (1, '007'), (1, '1.250')"))
    (dorg-commit dolt-dir "seed typed read port")
    (dorg-test-sql--assert-typed-port
     (dorg-sql--transaction-read-port (dorg-sql--get-connection dolt-dir)
                                      '("typed_port_ix"))
     "typed_port_ix")))

(ert-deftest dorg-test-e3cah-effect-predicate-lowering-is-shared ()
  "tracked-regression B1 RED: delete generation and proof counts share one lowerer.

The extension delete predicate and transaction read-port count predicate must
delegate to the same shared quoting-layer helper, so future literal or NULL
lowering changes cannot split committed scope from verified scope."
  (let (calls)
    (cl-letf (((symbol-function 'dorg-sql--effect-predicate-sql)
               (lambda (payload)
                 (push payload calls)
                 "shared_predicate")))
      (should (equal "shared_predicate"
                     (dorg-ext-index-effect-predicate-sql
                      '((heading_id . 7)))))
      (should (equal "shared_predicate"
                     (dorg-sql--transaction-read-port-predicate-sql
                      '((heading_id . 7)))))
      (should-error (dorg-sql--transaction-read-port-predicate-sql
                     '((bad-column . 7)))
                    :type 'dorg-sql-transaction-read-port-error)
      (should-error (dorg-sql--transaction-read-port-predicate-sql
                     '((heading_id . (raw))))
                    :type 'dorg-sql-transaction-read-port-error)
      (should (equal '(((heading_id . 7)) ((heading_id . 7)))
                     (nreverse calls))))))

(ert-deftest dorg-test-e3cah-transaction-read-port-predicate-lowering ()
  "tracked-regression: the extension-count family lowers only typed predicates.
Nil means the censused whole-table COUNT case; numbers stay numeric literals;
strings are quoted data, including semicolon content."
  (should-not (dorg-sql--transaction-read-port-predicate-sql nil))
  (should (equal "heading_id = 7"
                 (dorg-sql--transaction-read-port-predicate-sql
                  '((heading_id . 7)))))
  (should (equal "title = 'Alpha; still honest'"
                 (dorg-sql--transaction-read-port-predicate-sql
                  '((title . "Alpha; still honest")))))
  (should (equal "parent_id IS NULL"
                 (dorg-sql--transaction-read-port-predicate-sql
                  '((parent_id))))))

;;; ─── Exact text-detail postimage parity ─────────────────────────────

(defconst dorg-test-nm45t--matrix-seed
  (concat "* Case A\n:PROPERTIES:\n:BEAD_ID: case-a\n:END:\nold a\n"
          "* Case B\n:PROPERTIES:\n:BEAD_ID: case-b\n:END:\nold b\n"
          "* Case C\n:PROPERTIES:\n:BEAD_ID: case-c\n:END:\nold c\n"
          "* Case D\n:PROPERTIES:\n:BEAD_ID: case-d\n:END:\nold d\n")
  "Four identity-owned headings for the nm45t mutation matrix.")

(defun dorg-test-nm45t--replace-one (id key text)
  "Return a whole-items replacement for ID preserving identity KEY and TEXT."
  `(on (id ,id)
       (replace (item (all))
                (drawer "PROPERTIES" ("BEAD_ID" ,key))
                (text ,text))))

(defun dorg-test-nm45t--replace-two (id key first second)
  "Return a whole-items replacement for ID with identity KEY and two texts."
  `(on (id ,id)
       (replace (item (all))
                (drawer "PROPERTIES" ("BEAD_ID" ,key))
                (text ,first)
                (text ,second))))

(defun dorg-test-nm45t--append (id text)
  "Return a typed append of TEXT after ID's last item."
  `(on (id ,id) (insert (text) (after-last) ,text)))

(defvar dorg-test-nm45t--candidate-image nil
  "Test observation of the REAL same-root attempt, never a public wire field.")

(defun dorg-test-nm45t--apply (src form)
  "Apply FORM, observing the real attempt separately from its public result."
  (let ((builder (symbol-function 'dorg-mutate--dolt-attempt-from-sim)))
    (setq dorg-test-nm45t--candidate-image nil)
    (cl-letf (((symbol-function 'dorg-mutate--dolt-attempt-from-sim)
               (lambda (source patches options sim)
                 (let ((attempt (funcall builder source patches options sim)))
                   (setq dorg-test-nm45t--candidate-image
                         (dorg-ext-index-context-candidate-postimage
                          (plist-get (plist-get attempt :sim) :index-context)))
                   attempt))))
      (dorg-api-execute form :mode 'apply :source src))))

(defun dorg-test-nm45t--heading (dolt-dir root-id owner-id)
  "Load OWNER-ID's real model with explicit ROOT-ID scope."
  (gethash owner-id (dorg-model-load-tree dolt-dir root-id)))

(defun dorg-test-nm45t--assert-items (heading forms)
  "Assert exact item content, order and spacing against public constructor FORMS."
  (should (dorg-heading-p heading))
  (let ((actual (dorg-heading-items heading))
        (expected (mapcar (lambda (form)
                            (dorg-mutate--item-from-value (dorg-api--item-value form)))
                          forms)))
    (should (= (length expected) (length actual)))
    (cl-mapc (lambda (want got)
               (should (dorg-model--item-semantic-equal-p want got))
               (should (equal (dorg-item-post-blank want) (dorg-item-post-blank got))))
             expected actual)))

(defun dorg-test-nm45t--assert-parity (candidate readback)
  "Assert canonical metadata plus exact ordered item content and spacing."
  (should (dorg-heading-p candidate))
  (should (dorg-heading-p readback))
  (should (equal (dorg-mutate--index-heading-canonical candidate)
                 (dorg-mutate--index-heading-canonical readback)))
  (let ((want (dorg-heading-items candidate))
        (got (dorg-heading-items readback)))
    (should (= (length want) (length got)))
    (cl-mapc (lambda (a b)
               (should (dorg-model--item-semantic-equal-p a b))
               (should (equal (dorg-item-post-blank a) (dorg-item-post-blank b))))
             want got)))

(defun dorg-test-nm45t--attempt-heading (attempt owner-id)
  "Read OWNER-ID from private ATTEMPT's immutable candidate, not a wire model."
  (let* ((context (plist-get (plist-get attempt :sim) :index-context))
         (heading (gethash owner-id (dorg-ext-index-context-candidate-postimage context))))
    (should (dorg-heading-p heading))
    heading))

(defun dorg-test-nm45t--last-text-id (dolt-dir heading-id)
  "Return the last text detail row id belonging to HEADING-ID."
  (alist-get
   'item_id
   (car (dorg-sql
         dolt-dir
         (format
          (concat "SELECT ti.item_id AS item_id FROM heading_items hi "
                  "JOIN text_items ti ON ti.item_id = hi.id "
                  "WHERE hi.heading_id = %d ORDER BY hi.sort_key DESC LIMIT 1")
          heading-id)))))

(defun dorg-test-nm45t--durable-state (dolt-dir)
  "Return the HEAD and exact ordered text-item state of DOLT-DIR."
  (list
   :head (dorg-head-commit dolt-dir)
   :texts
   (dorg-sql
    dolt-dir
    (concat "SELECT hi.heading_id AS heading_id, hi.id AS item_id, "
            "HEX(hi.sort_key) AS sort_key, hi.post_blank AS post_blank, "
            "ti.text_raw AS text_raw FROM heading_items hi "
            "JOIN text_items ti ON ti.item_id = hi.id "
            "ORDER BY hi.heading_id, hi.sort_key"))))

(ert-deftest dorg-test-nm45t-text-line-terminator-plans-detail-update ()
  "Line-terminator shifts preserve row identity but update exact text_raw.
Both add and remove transformations are non-vacuous: OLD and NEW differ, and
deleting the planner call cannot satisfy the asserted single :update op."
  (dolist (texts '(("beta tail" "beta tail\n")
                   ("beta tail\n" "beta tail")))
    (let* ((old (dorg-text-item :id 7
                                :sort-key (dorg-sortkey-from-integer 1)
                                :post-blank 0 :text (car texts)))
           (new (dorg-text-item :post-blank 0 :text (cadr texts))))
      (should-not (equal (dorg-text-item-text old)
                         (dorg-text-item-text new)))
      ;; Whitespace-normalized identity keeps the existing DB row.
      (should (equal (dorg-model--item-match-key old)
                     (dorg-model--item-match-key new)))
      (let ((ops (dorg-model-plan-items (list old) (list new))))
        (should (= 1 (length ops)))
        (should (eq :update (caar ops)))
        (should (= 7 (dorg-item-id (nth 1 (car ops)))))
        (should (equal (cadr texts)
                       (dorg-text-item-text (nth 2 (car ops)))))))))

(ert-deftest dorg-test-nm45t-four-case-direct-dolt-matrix ()
  "All four downstream text shapes commit with candidate/readback parity.
Case B also proves append owns exactly one separator via post_blank.  Case D
asserts the generated SQL includes the exact trailing-newline detail update."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed
                 dolt-dir dorg-test-nm45t--matrix-seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (a (dorg-test-mutate--id-by-title dolt-dir "Case A"))
           (b (dorg-test-mutate--id-by-title dolt-dir "Case B"))
           (c (dorg-test-mutate--id-by-title dolt-dir "Case C"))
           (d (dorg-test-mutate--id-by-title dolt-dir "Case D")))
      ;; A: one paragraph, then a whole-items replacement.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one a "case-a" "alpha body\n"))
      (let ((before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--replace-one a "case-a" "ALPHA body\n"))
        (should-not (equal before (dorg-head-commit dolt-dir))))

      ;; B: one paragraph, then typed append.  DOrg owns one separator only.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one b "case-b" "alpha body\n"))
      (let* ((before (dorg-head-commit dolt-dir))
             (_res (dorg-test-nm45t--apply
                    src (dorg-test-nm45t--append b "beta tail")))
             (candidate (gethash b dorg-test-nm45t--candidate-image))
             (fresh (dorg-test-nm45t--heading dolt-dir rid b))
             (expected '((drawer "PROPERTIES" ("BEAD_ID" "case-b")
                                  :post-blank 0)
                         (text "alpha body\n" :post-blank 1)
                         (text "beta tail" :post-blank 0))))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--assert-items candidate expected)
        (dorg-test-nm45t--assert-parity candidate fresh))

      ;; C: one embedded-newline paragraph, then replacement.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one
            c "case-c" "alpha body\nbeta tail\n"))
      (let ((before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--replace-one
              c "case-c" "ALPHA body\nbeta tail\n"))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--assert-items
         (dorg-test-nm45t--heading dolt-dir rid c)
         '((drawer "PROPERTIES" ("BEAD_ID" "case-c") :post-blank 0)
           (text "ALPHA body\nbeta tail\n" :post-blank 0))))

      ;; D: replace, append, then whole-items replacement.  The final
      ;; paragraph's parser line terminator must reach text_items.text_raw.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one d "case-d" "alpha body\n"))
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--append d "beta tail"))
      (let* ((form (dorg-test-nm45t--replace-two
                    d "case-d" "ALPHA body" "beta tail\n"))
             (tail-id (dorg-test-nm45t--last-text-id dolt-dir d))
             (patches (dorg-mutate--validate-patches (dorg-api-normalize form src)))
             (attempt (dorg-mutate--dolt-build-attempt
                       src patches nil))
             (candidate (dorg-test-nm45t--attempt-heading attempt d))
             (expected-update
              (format "UPDATE text_items SET text_raw = %s WHERE item_id = %d"
                      (dorg-sql-quote "beta tail\n") tail-id))
             (before (dorg-head-commit dolt-dir)))
        (should (member expected-update (plist-get attempt :sql)))
        (should (plist-get (dorg-test-nm45t--apply src form) :ok))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (let ((readback (dorg-test-nm45t--heading dolt-dir rid d)))
          (dorg-test-nm45t--assert-parity candidate readback)
          (dorg-test-nm45t--assert-parity
           (gethash d dorg-test-nm45t--candidate-image) readback)
          (dorg-test-nm45t--assert-items
           readback '((drawer "PROPERTIES" ("BEAD_ID" "case-d") :post-blank 0)
                      (text "ALPHA body" :post-blank 0)
                      (text "beta tail\n" :post-blank 0))))))))

(ert-deftest dorg-test-nm45t-injected-text-mismatch-refuses-atomically ()
  "A proven-valid sequence refuses an injected precommit text mismatch.
The control owner commits the same final operation before the sabotage owner;
the refusal therefore exercises parity rather than an intrinsically bad form."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((seed (concat "* Control\n:PROPERTIES:\n:BEAD_ID: control\n:END:\nold\n"
                         "* Sabotage\n:PROPERTIES:\n:BEAD_ID: sabotage\n:END:\nold\n"))
           (rid (dorg-test-mutate--seed dolt-dir seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (control (dorg-test-mutate--id-by-title dolt-dir "Control"))
           (sabotage (dorg-test-mutate--id-by-title dolt-dir "Sabotage")))
      (dolist (spec `((,control "control") (,sabotage "sabotage")))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--replace-one
              (car spec) (cadr spec) "alpha body\n"))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--append (car spec) "beta tail")))
      ;; Non-vacuity control: the exact final mutation shape commits.
      (let ((before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--replace-two
              control "control" "ALPHA body" "beta tail\n"))
        (should-not (equal before (dorg-head-commit dolt-dir))))
      (let* ((tail-id (dorg-test-nm45t--last-text-id dolt-dir sabotage))
             (before (dorg-test-nm45t--durable-state dolt-dir))
             (err nil))
        (dorg-test-2ij34--with-injected-statement
            (format "UPDATE text_items SET text_raw = 'injected mismatch' WHERE item_id = %d"
                    tail-id)
          (setq err
                (should-error
                 (dorg-test-nm45t--apply
                  src (dorg-test-nm45t--replace-two
                       sabotage "sabotage" "ALPHA body" "beta tail\n"))
                 :type 'dorg-mutate-error)))
        (let ((data (dorg-mutate-error-data err)))
          (should (eq 'invariant (dorg-mutate-error-reason err)))
          (should (eq 'postimage-parity (plist-get data :detail)))
          (should (eq 'core (plist-get data :parity-plane)))
          (should (eq 'validate (plist-get data :transaction-phase)))
          (should (eq t (plist-get data :head-equal-at-failure))))
        (should (equal before (dorg-test-nm45t--durable-state dolt-dir)))))))

(provide 'dorg-2ij34-postimage-tests)
;;; dorg-2ij34-postimage-tests.el ends here

;;; ─── Bounded read wrapper at the proof hand-off ───────────────────────────
;; The transaction read port refuses any single request whose `:ids' exceed
;; `dorg-sql-transaction-read-port-max-ids'.  The proof must therefore submit
;; oversized candidate id sets in bounded chunks through ONE private wrapper
;; at its hand-off, merging results by op shape, while every request at or
;; under the cap, every unknown op, and every malformed id set passes through
;; untouched so the port's own refusals still govern.

(defun dorg-test-2g25t--recording-read (result-fn)
  "Return (READ . LOG-CELL): READ records each request and answers via RESULT-FN."
  (let ((log (list nil)))
    (cons (lambda (request)
            (push request (car log))
            (funcall result-fn request))
          log)))

(defun dorg-test-2g25t--requests (log-cell)
  "Return the recorded requests of LOG-CELL in call order."
  (reverse (car log-cell)))

(defun dorg-test-2g25t--rows-for (request)
  "Answer a row-op REQUEST with one `((id . N))' row per requested id."
  (mapcar (lambda (id) (list (cons 'id id))) (plist-get request :ids)))

(ert-deftest dorg-test-2g25t-batched-read-chunks-row-ops-at-cap ()
  "An oversized id set on each row op is split into consecutive chunks of at
most the cap, every chunk keeps the request's other keys, and the delivered
rows are the concatenation in input order."
  (let ((cap dorg-sql-transaction-read-port-max-ids)
        (ids (number-sequence 1 2500)))
    (dolist (op '(core-heading-rows core-heading-tags core-heading-ids))
      (let* ((rec (dorg-test-2g25t--recording-read #'dorg-test-2g25t--rows-for))
             (read (dorg-mutate--postimage-read-batched (car rec)))
             (rooted (eq op 'core-heading-rows))
             (request (if rooted
                          (list :version 1 :op op :ids ids :root-id 7)
                        (list :version 1 :op op :ids ids)))
             (rows (funcall read request))
             (reqs (dorg-test-2g25t--requests (cdr rec))))
        (should (= (length reqs) 3))
        (should (equal (mapcar (lambda (r) (length (plist-get r :ids))) reqs)
                       (list cap cap (- 2500 (* 2 cap)))))
        (should (equal (apply #'append (mapcar (lambda (r) (plist-get r :ids)) reqs)) ids))
        (dolist (r reqs)
          (should (= 1 (plist-get r :version)))
          (should (eq op (plist-get r :op)))
          (should (equal (plist-get r :root-id) (and rooted 7))))
        (should (equal (mapcar (lambda (row) (alist-get 'id row)) rows) ids))))))

(ert-deftest dorg-test-2g25t-batched-read-merges-owner-model-images ()
  "An oversized owner-model read returns one image holding every chunk's
headings under its own id, with the root id preserved on each chunk."
  (let* ((ids (number-sequence 1 2500))
         (rec (dorg-test-2g25t--recording-read
               (lambda (request)
                 (let ((image (make-hash-table :test 'eql)))
                   (dolist (id (plist-get request :ids))
                     (puthash id (make-dorg-heading :id id) image))
                   image))))
         (read (dorg-mutate--postimage-read-batched (car rec)))
         (image (funcall read (list :version 1 :op 'owner-model :root-id 9 :ids ids)))
         (reqs (dorg-test-2g25t--requests (cdr rec))))
    (should (hash-table-p image))
    (should (= (hash-table-count image) 2500))
    (should (cl-every (lambda (id) (= id (dorg-heading-id (gethash id image)))) ids))
    (should (= (length reqs) 3))
    (should (cl-every (lambda (r) (= 9 (plist-get r :root-id))) reqs))))

(ert-deftest dorg-test-2g25t-batched-read-passes-under-cap-requests-unchanged ()
  "A request at or under the cap reaches the port as the very same object."
  (let* ((rec (dorg-test-2g25t--recording-read #'dorg-test-2g25t--rows-for))
         (read (dorg-mutate--postimage-read-batched (car rec)))
         (request (list :version 1 :op 'core-heading-rows
                        :ids (number-sequence 1 dorg-sql-transaction-read-port-max-ids)
                        :root-id 1)))
    (funcall read request)
    (let ((reqs (dorg-test-2g25t--requests (cdr rec))))
      (should (= (length reqs) 1))
      (should (eq (car reqs) request)))))

(ert-deftest dorg-test-2g25t-batched-read-never-turns-malformed-into-accepted ()
  "Chunking never converts a request the port refuses into an accepted one.
Through the REAL port's pure validation (no connection is touched before a
refusal): an unknown op, a non-integer id, a version the port does not
serve, unknown keys, an owner-model read without its root id, and an empty
id set are all still refused with the port's own error, oversized or not.
The wrapper adds no grammar of its own."
  (let* ((big (number-sequence 1 2500))
         (read (dorg-mutate--postimage-read-batched
                (dorg-sql--transaction-read-port nil))))
    (dolist (request (list (list :version 1 :op 'extension-rows :ids big :table "x")
                           (list :version 1 :op 'core-heading-rows
                                 :ids (append big (list "not-an-id")) :root-id 1)
                           (list :version 2 :op 'core-heading-rows :ids big :root-id 1)
                           (list :version 1 :op 'core-heading-tags :ids big :extra t)
                           (list :version 1 :op 'owner-model :ids big)
                           (list :version 1 :op 'core-heading-rows :ids nil :root-id 1)))
      (should-error (funcall read request)
                    :type 'dorg-sql-transaction-read-port-error))))

(ert-deftest dorg-test-2g25t-later-chunk-survivor-is-still-refused ()
  "A row the candidate deletes that survives in the SECOND chunk of an
oversized absence check is still refused by the core proof."
  (let* ((absent (number-sequence 1 1500))
         (rec (dorg-test-2g25t--recording-read
               (lambda (request)
                 (and (eq 'core-heading-ids (plist-get request :op))
                      (memq 1300 (plist-get request :ids))
                      (list (list (cons 'id 1300)))))))
         (read (dorg-mutate--postimage-read-batched (car rec))))
    (should-error (dorg-mutate--postimage-assert-core
                   read (make-hash-table :test 'eql) absent nil)
                  :type 'dorg-sql-postimage-parity)
    (should (= (length (dorg-test-2g25t--requests (cdr rec))) 2))))

(ert-deftest dorg-test-2g25t-later-chunk-partition-absence-and-corruption-are-refused ()
  "Within one owner partition larger than the cap, a heading missing from the
second chunk and a heading whose readback diverges in the second chunk are
each refused by the partition proof."
  (let* ((ids (number-sequence 1 1500))
         (expected (make-hash-table :test 'eql)))
    (dolist (id ids)
      (let ((h (make-dorg-heading :id id :title (format "t%d" id))))
        (puthash id (cons (dorg-mutate--index-heading-canonical h)
                          (dorg-mutate--postimage-item-row-canonical h))
                 expected)))
    (let ((specs (list (list 100 1 ids expected nil))))
      (cl-flet ((image-for (request mutate)
                  (let ((image (make-hash-table :test 'eql)))
                    (dolist (id (plist-get request :ids))
                      (let ((h (make-dorg-heading :id id :title (format "t%d" id))))
                        (funcall mutate id h image)))
                    image)))
        ;; absence in the second chunk
        (let* ((rec (dorg-test-2g25t--recording-read
                     (lambda (request)
                       (image-for request (lambda (id h image)
                                            (unless (= id 1300) (puthash id h image)))))))
               (read (dorg-mutate--postimage-read-batched (car rec))))
          (should-error (dorg-mutate--postimage-assert-partitions read specs)
                        :type 'dorg-sql-postimage-parity)
          (should (= (length (dorg-test-2g25t--requests (cdr rec))) 2)))
        ;; corruption in the second chunk
        (let* ((rec (dorg-test-2g25t--recording-read
                     (lambda (request)
                       (image-for request (lambda (id h image)
                                            (when (= id 1300) (setf (dorg-heading-title h) "corrupt"))
                                            (puthash id h image))))))
               (read (dorg-mutate--postimage-read-batched (car rec))))
          (should-error (dorg-mutate--postimage-assert-partitions read specs)
                        :type 'dorg-sql-postimage-parity))
        ;; control: intact second chunk passes
        (let* ((rec (dorg-test-2g25t--recording-read
                     (lambda (request)
                       (image-for request (lambda (id h image) (puthash id h image))))))
               (read (dorg-mutate--postimage-read-batched (car rec))))
          (dorg-mutate--postimage-assert-partitions read specs)
          (should (= (length (dorg-test-2g25t--requests (cdr rec))) 2)))))))

(ert-deftest dorg-test-2g25t-proof-hand-off-wraps-the-read-port ()
  "The proof's validate function hands every assertion one read wrapped by
the bounded wrapper, built exactly once, never the raw port."
  (let* ((ctx (dorg-ext-index-context-create
               :source-kind 'dolt :root-id 1 :from-root-id nil :to-root-id nil
               :patches nil :created nil :changed nil :moved nil :deleted nil
               :base-image (make-hash-table :test 'eql)
               :candidate-postimage (make-hash-table :test 'eql)
               :owner-partitions (list (dorg-test-2ij34--static-partition))
               :candidate-id (make-string 40 ?c) :base-commit "base"))
         (validate (plist-get (dorg-mutate--postimage-plan ctx nil) :validate))
         (raw (lambda (_request) :raw))
         (wrapped-calls 0)
         (seen nil)
         (real (symbol-function 'dorg-mutate--postimage-read-batched)))
    (cl-letf (((symbol-function 'dorg-mutate--postimage-read-batched)
               (lambda (read) (cl-incf wrapped-calls) (funcall real read)))
              ((symbol-function 'dorg-ext-run-index-validators) (lambda (&rest _) nil))
              ((symbol-function 'dorg-mutate--postimage-assert-core)
               (lambda (read &rest _) (push read seen)))
              ((symbol-function 'dorg-mutate--postimage-assert-partitions)
               (lambda (read &rest _) (push read seen)))
              ((symbol-function 'dorg-mutate--postimage-assert-rows)
               (lambda (read &rest _) (push read seen))))
      (funcall (plist-get validate :function) raw)
      (should (= wrapped-calls 1))
      (should (= (length seen) 3))
      (should (cl-every (lambda (r) (eq r (car seen))) seen))
      (should-not (eq (car seen) raw))
      (should (functionp (car seen))))))
