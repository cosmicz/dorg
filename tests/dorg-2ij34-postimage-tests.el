;;; dorg-2ij34-postimage-tests.el --- Postimage receipt + transaction proof -*- lexical-binding: t; -*-

;;; Commentary:

;; Failure-capable acceptance tests for tracked-regression (Contract C v5):
;; canonical postimage receipt entries per affected owner partition, the
;; pre-commit transaction proof in the existing validate-function window,
;; and pre-transaction digest entitlement in the operational mutation log.
;; Every test uses a fresh temporary Dolt store; no live store is touched.
;;
;; RED discipline: the receipt and validator tests fail on unmodified
;; trunk in the predicted direction (absent :postimages; sabotage commits
;; where it must abort).  Boundary controls that legitimately pass at
;; trunk are labeled as such in their docstrings.

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

(defun dorg-test-2ij34--entry-for (res owner-id)
  "Return RES's :postimages entry whose :owner-heading is OWNER-ID."
  (cl-find owner-id (plist-get res :postimages)
           :key (lambda (e) (plist-get e :owner-heading))))

(defun dorg-test-2ij34--state (dolt-dir)
  "Return a comparable snapshot of DOLT-DIR's durable state."
  (list :head (dorg-head-commit dolt-dir)
        :titles (dorg-sql dolt-dir
                          "SELECT id, title FROM headings ORDER BY id")
        :log (alist-get 'n (car (dorg-sql
                                 dolt-dir
                                 "SELECT COUNT(*) AS n FROM dolt_log")))))

;;; ─── AC1 — receipt completeness ─────────────────────────────────────

(ert-deftest dorg-test-2ij34-receipt-single-owner-entry ()
  "AC1 RED: a committed apply carries one versioned entry for its owner.
On trunk :postimages is absent entirely, so this fails there."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--heading-update-patch
                            alpha :title "Alpha renamed")))))
      (should (plist-get res :ok))
      (let ((entry (dorg-test-2ij34--entry-for res alpha)))
        (should entry)
        (should (equal 1 (plist-get entry :version)))
        (should (equal rid (plist-get entry :root)))
        (should (equal alpha (plist-get entry :owner-heading)))
        (should (plist-member entry :parent-owner-heading))
        (should (stringp (plist-get entry :candidate-id)))
        (should (string-match-p "\\`[0-9a-f]\\{40,64\\}\\'"
                                (plist-get entry :digest)))
        (should-not (plist-get entry :deleted))))))

(ert-deftest dorg-test-2ij34-receipt-multi-owner-batch ()
  "AC1 RED: a batch touching two owners emits one entry per owner."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--heading-update-patch
                            alpha :title "Alpha two")
                           (dorg-test-mutate--heading-update-patch
                            beta :title "Beta two")))))
      (should (plist-get res :ok))
      (should (dorg-test-2ij34--entry-for res alpha))
      (should (dorg-test-2ij34--entry-for res beta))
      (should (= 2 (length (plist-get res :postimages)))))))

(ert-deftest dorg-test-2ij34-receipt-deleted-owner-entry ()
  "AC1 RED: deleting an owner subtree emits an entry with :deleted t."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--delete-patch beta t)))))
      (should (plist-get res :ok))
      (let ((entry (dorg-test-2ij34--entry-for res beta)))
        (should entry)
        (should (plist-get entry :deleted))))))

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

;;; ─── AC3 — digest entitlement in the operational log ────────────────

(ert-deftest dorg-test-2ij34-digest-logged-with-evidence-row ()
  "AC3 RED: an op-id apply stores postimages_v1 with the evidence row,
matching the digests the receipt reports.  On trunk the column does not
exist, so this fails."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (op-id "test:2ij34:digest-log:1")
           (res (dorg-api-execute
                 `(on (id ,alpha) (set (heading title) "Alpha logged"))
                 :mode 'apply :source src :op-id op-id)))
      (should (plist-get res :ok))
      (let* ((entry (dorg-test-2ij34--entry-for res alpha))
             (stored (alist-get
                      'postimages_v1
                      (car (dorg-sql
                            dolt-dir
                            (format "SELECT postimages_v1 FROM dorg_mutation_log WHERE op_id = %s"
                                    (dorg-sql-quote op-id)))))))
        (should entry)
        (should (stringp stored))
        (should (string-match-p (regexp-quote (plist-get entry :digest))
                                stored))))))

;;; ─── AC4 — candidate-model wire shape v1 ────────────────────────────
;; RED baseline for this family is the lane tip AFTER the AC1-AC3
;; implementation landed: `:return-candidate-models' is an unknown raw
;; option there (historically ignored), so each test fails on the absent
;; `:candidate-models' field in its predicted direction.

(defun dorg-test-2ij34--model-for (res owner-id)
  "Return RES's :candidate-models entry whose :owner-heading is OWNER-ID."
  (cl-find owner-id (plist-get res :candidate-models)
           :key (lambda (e) (plist-get e :owner-heading))))

(defconst dorg-test-2ij34--rich-seed
  (concat "* TODO [#B] Gamma :tagx:tagy:\n"
          "SCHEDULED: <2026-09-02 Wed>\n"
          ":PROPERTIES:\n:BEAD_ID: bead-gamma\n:CUSTOM: v1\n:END:\n"
          ":LOGBOOK:\n- note one\n:END:\n"
          "gamma body\n"
          "** COMMENT Delta\n"
          "delta body\n")
  "One owner exercising every NODE field class: todo, priority, tags,
properties (sorted), a planning item, a property-form drawer, a lines
drawer, text items, a commented child in semantic order.")

(ert-deftest dorg-test-2ij34-wire-candidate-models-returned ()
  "AC4 RED: `:return-candidate-models t' returns one versioned envelope per
affected owner, its NODE serialized from the committed readback model."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (res (dorg-api-execute
                 `(on (id ,alpha) (set (heading title) "Alpha wired"))
                 :mode 'apply :source src :return-candidate-models t)))
      (should (plist-get res :ok))
      (let ((entry (dorg-test-2ij34--model-for res alpha)))
        (should entry)
        (should (equal 1 (plist-get entry :version)))
        (should (equal rid (plist-get entry :root)))
        (should (plist-member entry :parent-owner-heading))
        (let ((model (plist-get entry :model)))
          (should (eq 'heading (car-safe model)))
          (let ((fields (cdr model)))
            (should (equal alpha (plist-get fields :id)))
            (should (equal "Alpha wired" (plist-get fields :title)))
            (should (member '(text "alpha body\n" :post-blank 0) (plist-get fields :items)))
            (should (equal "bead-alpha"
                           (cdr (assoc "BEAD_ID"
                                       (plist-get fields :properties)))))
            (let ((children (plist-get fields :children)))
              (should (= 1 (length children)))
              (should (equal "Design"
                             (plist-get (cdr (car children)) :title)))
              (should (member '(text "design body\n" :post-blank 0)
                              (plist-get (cdr (car children)) :items))))))))))

(ert-deftest dorg-test-2ij34-wire-deleted-owner-model-nil ()
  "AC4 RED: a deleted owner's wire entry carries :model nil."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (res (dorg-api-execute
                 `(on (id ,beta) (delete (subtree)))
                 :mode 'apply :source src :return-candidate-models t)))
      (should (plist-get res :ok))
      (let ((entry (dorg-test-2ij34--model-for res beta)))
        (should entry)
        (should (plist-member entry :model))
        (should-not (plist-get entry :model))))))

(ert-deftest dorg-test-2ij34-wire-node-field-completeness ()
  "AC4 RED (serializer completeness witness): every NODE field class
round-trips — todo, priority, tags, post-blank shape, commented, sorted
properties, exact typed item forms of every kind, semantic-order children."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--rich-seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (gamma (dorg-test-mutate--id-by-title dolt-dir "Gamma"))
           (res (dorg-api-execute
                 `(on (id ,gamma) (set (heading title) "Gamma"))
                 :mode 'apply :source src :return-candidate-models t
                 :op-id "test:2ij34:wire-completeness:1"))
           (entry (dorg-test-2ij34--model-for res gamma))
           (fields (cdr (plist-get entry :model))))
      (should (plist-get res :ok))
      (should entry)
      (should (equal "TODO" (plist-get fields :todo)))
      ;; The persisted model carries priority as its cookie letter string;
      ;; the wire is faithful to the model representation (the contract's
      ;; CHAR-OR-NIL annotation is an erratum, disclosed for review).
      (should (equal "B" (plist-get fields :priority)))
      (should (equal '("tagx" "tagy") (plist-get fields :tags)))
      (should (integerp (plist-get fields :post-blank)))
      (should-not (plist-get fields :commented))
      ;; Properties sorted by name.
      (let ((props (plist-get fields :properties)))
        (should (equal props (sort (copy-sequence props)
                                   (lambda (a b) (string< (car a) (car b))))))
        (should (equal "v1" (cdr (assoc "CUSTOM" props))))
        (should (equal "bead-gamma" (cdr (assoc "BEAD_ID" props)))))
      ;; Exact typed item forms, public-constructor vocabulary.
      (let ((items (plist-get fields :items)))
        (should (member '(text "gamma body\n" :post-blank 0) items))
        (should (member '(planning :scheduled "<2026-09-02 Wed>" :post-blank 0) items))
        (should (cl-find-if (lambda (i)
                              (and (eq (car-safe i) 'drawer)
                                   (equal (nth 1 i) "LOGBOOK")))
                            items)))
      ;; Semantic-order children; the COMMENT child keeps :commented t.
      (let ((children (plist-get fields :children)))
        (should (= 1 (length children)))
        (let ((delta (cdr (car children))))
          (should (equal "Delta" (plist-get delta :title)))
          (should (plist-get delta :commented))
          (should (member '(text "delta body\n" :post-blank 0) (plist-get delta :items))))))))

(ert-deftest dorg-test-2ij34-wire-sabotaged-serializer-visible ()
  "AC4 RED (single-serializer seam): sabotaging the one wire serializer is
consumer-visible — the returned NODE loses the dropped field, proving the
public result flows through THAT serializer and no parallel path exists.
Predicted failure at the RED baseline: the seam function does not exist."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--rich-seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (gamma (dorg-test-mutate--id-by-title dolt-dir "Gamma"))
           (real (symbol-function 'dorg-mutate--postimage-wire-node)))
      (cl-letf (((symbol-function 'dorg-mutate--postimage-wire-node)
                 (lambda (heading children)
                   (let ((node (funcall real heading children)))
                     (cons (car node)
                           (let ((fields (copy-sequence (cdr node))))
                             (cl-remf fields :todo)
                             fields))))))
        (let* ((res (dorg-api-execute
                     `(on (id ,gamma) (set (heading title) "Gamma sab"))
                     :mode 'apply :source src :return-candidate-models t))
               (entry (dorg-test-2ij34--model-for res gamma)))
          (should entry)
          (should-not (plist-member (cdr (plist-get entry :model)) :todo)))))))

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

(ert-deftest dorg-test-2ij34-receipt-cross-root-both-sides ()
  "AC1 witness (K#025 B1; may pass at the baseline): an identity
cross-root move emits one entry per affected owner on EACH side — the
source-side owner entry is :deleted (the owner left that root), the
destination-side entry is present."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((res (dorg-test-2ij34--xroot-apply dolt-dir)))
      (should (plist-get res :ok))
      (let ((entries (plist-get res :postimages)))
        (should (cl-find-if
                 (lambda (e) (and (equal 100 (plist-get e :root))
                                  (equal 101 (plist-get e :owner-heading))
                                  (plist-get e :deleted)))
                 entries))
        (should (cl-find-if
                 (lambda (e) (and (equal 500 (plist-get e :root))
                                  (equal 101 (plist-get e :owner-heading))
                                  (not (plist-get e :deleted))))
                 entries))))))

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

;;; ─── AC5 — settled-op replay evidence ───────────────────────────────

(ert-deftest dorg-test-2ij34-replay-settled-op-reemits-postimages ()
  "AC5 RED: replaying a settled op-id re-emits the stored :postimages
verbatim on the idempotent result."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha replayed")))
           (op-id "test:2ij34:replay-receipt:1")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id))
           (res2 (dorg-api-execute form :mode 'apply :source src :op-id op-id)))
      (should (plist-get res1 :postimages))
      (should (plist-get res2 :idempotent))
      (should (equal (plist-get res1 :new-token) (plist-get res2 :new-token)))
      (should (equal (plist-get res1 :postimages)
                     (plist-get res2 :postimages))))))

(ert-deftest dorg-test-2ij34-replay-models-reconstructed-as-of ()
  "AC5 RED: an evidence-requesting replay reconstructs candidate models AS
OF the settled commit over the stored owner scope — NOT from current state.
A later mutation moves current state away; the replay must return the
settled title, not the current one."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha v1")))
           (op-id "test:2ij34:replay-models:1")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id
                                   :return-candidate-models t)))
      (should (plist-get res1 :ok))
      ;; Move current state past the settled commit.
      (should (plist-get
               (dorg-api-execute
                `(on (id ,alpha) (set (heading title) "Alpha v2"))
                :mode 'apply :source src :op-id "test:2ij34:replay-models:2")
               :ok))
      (let* ((res2 (dorg-api-execute form :mode 'apply :source src
                                     :op-id op-id
                                     :return-candidate-models t))
             (m1 (dorg-test-2ij34--model-for res1 alpha))
             (m2 (dorg-test-2ij34--model-for res2 alpha)))
        (should (plist-get res2 :idempotent))
        (should (equal (plist-get res1 :new-token) (plist-get res2 :new-token)))
        (should m1)
        (should m2)
        (should (equal "Alpha v1" (plist-get (cdr (plist-get m2 :model)) :title)))
        (should (equal m1 m2))))))

(ert-deftest dorg-test-2ij34-replay-legacy-row-refuses-evidence ()
  "AC5 RED: a settled LEGACY evidence row (no postimages_v1 payload) with an
evidence-requesting caller yields the typed `legacy-evidence-unavailable'
refusal carrying the settled commit and op identity — never a verified
success."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir dorg-test-2ij34--seed 100))
           (src (dorg-test-2ij34--src dolt-dir rid))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (form `(on (id ,alpha) (set (heading title) "Alpha legacy")))
           (op-id "test:2ij34:replay-legacy:1")
           (res1 (dorg-api-execute form :mode 'apply :source src :op-id op-id)))
      (should (plist-get res1 :ok))
      ;; Simulate a pre-receipt legacy row: evidence without postimages.
      (dorg-sql-batch dolt-dir
                      (list (format
                             "UPDATE dorg_mutation_log SET postimages_v1 = NULL WHERE op_id = %s"
                             (dorg-sql-quote op-id)))
                      "test: simulate legacy evidence row")
      (let ((err (should-error
                  (dorg-api-execute form :mode 'apply :source src
                                    :op-id op-id
                                    :return-candidate-models t)
                  :type 'dorg-mutate-error)))
        (should (eq 'legacy-evidence-unavailable
                    (dorg-mutate-error-reason err)))
        (let ((data (dorg-mutate-error-data err)))
          (should (equal op-id (plist-get data :op-id)))
          (should (equal (plist-get res1 :new-token)
                         (plist-get data :commit))))))))

(ert-deftest dorg-test-2ij34-replay-legacy-row-old-caller-unchanged ()
  "AC5 boundary control (may pass at the RED baseline): a legacy evidence
row replayed WITHOUT an evidence request keeps the old idempotent result —
ok, same commit, no postimages, no refusal."
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

;;; ─── AC6 — static contract pins (no Dolt required) ──────────────────
;; Pure-struct pins of the contract's closed shapes.  A future edit that
;; drops, renames, or reorders a receipt/wire field, or changes the
;; operational payload arity, fails HERE before any consumer drifts.

(defun dorg-test-2ij34--plist-keys (plist)
  "Return PLIST's keys in order."
  (cl-loop for (k _v) on plist by #'cddr collect k))

(defun dorg-test-2ij34--static-partition (&optional title)
  "Return a minimal synthetic owner partition for static pins."
  (let ((base (make-hash-table :test 'eql))
        (cand (make-hash-table :test 'eql)))
    (puthash 7 (make-dorg-heading :id 7 :parent-id nil :level 1
                                  :title "before" :post-blank 0)
             base)
    (puthash 7 (make-dorg-heading :id 7 :parent-id nil :level 1
                                  :title (or title "after") :post-blank 0)
             cand)
    (dorg-ext-owner-partition-create
     :root-id 1 :owner-id 7 :owner-key "BEAD_ID" :owner-value "bead-x"
     :parent-owner-id nil :parent-owner-value nil
     :affected-ids '(7) :affected-paths nil
     :base-image base :candidate-postimage cand)))

(ert-deftest dorg-test-2ij34-static-receipt-entry-census ()
  "AC6: the receipt entry's key census is the contract's, exactly."
  (let ((entry (dorg-mutate--postimage-entry
                (dorg-test-2ij34--static-partition) "cand-id")))
    (should (equal '(:version :root :owner-heading :parent-owner-heading
                     :candidate-id :digest :deleted)
                   (dorg-test-2ij34--plist-keys entry)))
    (should (equal 1 (plist-get entry :version)))))

(ert-deftest dorg-test-2ij34-static-digest-tracks-candidate ()
  "AC6 transformation pin: the digest is a function of the candidate
canonical — equal partitions digest equal, a changed candidate digests
differently."
  (let ((a (dorg-mutate--postimage-digest (dorg-test-2ij34--static-partition)))
        (b (dorg-mutate--postimage-digest (dorg-test-2ij34--static-partition)))
        (c (dorg-mutate--postimage-digest
            (dorg-test-2ij34--static-partition "tampered"))))
    (should (equal a b))
    (should-not (equal a c))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" a))))

(ert-deftest dorg-test-2ij34-static-wire-node-census ()
  "AC6: the wire NODE field census is the contract's, exactly."
  (let* ((h (make-dorg-heading :id 7 :parent-id nil :level 1 :title "t"
                               :todo "TODO" :priority "A" :tags '("x")
                               :post-blank 1 :commented nil))
         (node (dorg-mutate--postimage-wire-node h nil)))
    (should (eq 'heading (car node)))
    (should (equal '(:id :title :todo :priority :tags :post-blank
                     :commented :properties :items :children)
                   (dorg-test-2ij34--plist-keys (cdr node))))))

(ert-deftest dorg-test-2ij34-static-wire-item-forms ()
  "AC6: every item class serializes to its public-constructor form, with
the always-present `:post-blank' tail (K#025 B2)."
  (should (equal '(text "body" :post-blank 0)
                 (dorg-mutate--postimage-wire-item
                  (dorg-text-item :post-blank 0 :text "body"))))
  (should (equal '(keyword "K" "V" :post-blank 3)
                 (dorg-mutate--postimage-wire-item
                  (dorg-keyword-item :post-blank 3 :keyword "K" :value "V"))))
  (should (equal '(planning :scheduled "<s>" :closed "[c]" :post-blank 0)
                 (dorg-mutate--postimage-wire-item
                  (dorg-planning-item :post-blank 0
                                      :scheduled "<s>" :closed "[c]"))))
  (should (equal '(drawer "P" ("A" "1") ("B" "2") :post-blank 1)
                 (dorg-mutate--postimage-wire-item
                  (dorg-drawer-item :post-blank 1 :name "P"
                                    :properties '(("A" . "1") ("B" . "2"))))))
  (should (equal '(drawer "L" :lines ("l1" "l2") :post-blank 0)
                 (dorg-mutate--postimage-wire-item
                  (dorg-drawer-item :post-blank 0 :name "L"
                                    :lines '("l1" "l2"))))))

(ert-deftest dorg-test-2ij34-static-wire-item-grammar-roundtrip ()
  "AC4 RED (K#026): every wire item form IS the public constructor grammar.
Table-driven over all four kinds (both drawer discriminants): a model item
serializes to a wire form, `dorg-api--item-value' accepts that exact form
and carries `:post-blank' in the normalized value, `dorg-mutate--item-from-value'
rebuilds the model item from it, and re-serialization is byte-identical —
ONE grammar, no wire-only parser.  At the K#026 baseline the constructor
rejects every `:post-blank'-tailed form."
  (dolist (item (list (dorg-text-item :post-blank 2 :text "body")
                      (dorg-keyword-item :post-blank 1 :keyword "K" :value "V")
                      (dorg-planning-item :post-blank 0
                                          :scheduled "<s>" :closed "[c]")
                      (dorg-drawer-item :post-blank 1 :name "P"
                                        :properties '(("A" . "1")))
                      (dorg-drawer-item :post-blank 0 :name "L"
                                        :lines '("l1" "l2"))))
    (let* ((wire (dorg-mutate--postimage-wire-item item))
           (value (dorg-api--item-value wire))
           (rebuilt (dorg-mutate--item-from-value value)))
      (should (equal (dorg-item-post-blank item)
                     (plist-get value :post-blank)))
      (should (equal wire (dorg-mutate--postimage-wire-item rebuilt)))))
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

(ert-deftest dorg-test-2ij34-static-payload-roundtrip ()
  "AC6: the operational payload round-trips entries exactly; malformed
payloads parse to nil (legacy semantics), never fabricated evidence."
  (let* ((entries (list (dorg-mutate--postimage-entry
                         (dorg-test-2ij34--static-partition)
                         (make-string 40 ?c))))
         (payload (dorg-mutate--postimage-log-payload entries)))
    (should (equal entries
                   (dorg-mutate--postimage-entries-from-payload payload))))
  (should-not (dorg-mutate--postimage-entries-from-payload "not a payload ("))
  (should-not (dorg-mutate--postimage-entries-from-payload "((1 2 3))"))
  (should-not (dorg-mutate--postimage-log-payload nil))
  ;; A tampered evidence column carrying a read-time-eval form is malformed
  ;; input, never evaluated (the reader-census seam law).
  (should-not (dorg-mutate--postimage-entries-from-payload
               "#.(setq dorg-test-2ij34--evil t)"))
  (should-not (boundp 'dorg-test-2ij34--evil)))

(ert-deftest dorg-test-2ij34-static-wire-item-post-blank-carried ()
  "AC4 RED (K#025 B2): item-level post-blank is persisted, participates in
the items digest, and MUST cross the wire — two items differing only in
post-blank serialize to different forms, and the value is explicit."
  (let ((a (dorg-mutate--postimage-wire-item
            (dorg-text-item :post-blank 0 :text "b")))
        (b (dorg-mutate--postimage-wire-item
            (dorg-text-item :post-blank 2 :text "b"))))
    (should-not (equal a b))
    (should (equal '(text "b" :post-blank 0) a))
    (should (equal '(text "b" :post-blank 2) b)))
  (should (equal '(drawer "L" :lines ("l1") :post-blank 1)
                 (dorg-mutate--postimage-wire-item
                  (dorg-drawer-item :post-blank 1 :name "L" :lines '("l1"))))))

(ert-deftest dorg-test-2ij34-static-payload-rejects-trailing-and-mistyped ()
  "AC3/AC5 RED (K#025 B3): stored-evidence parsing consumes exactly ONE
form (trailing bytes are malformed) and validates the closed field types —
integer roots/owners, integer-or-nil parent, 40-hex candidate-id, 64-hex
digest, boolean deleted.  Every rejection degrades to legacy semantics."
  (let ((good (dorg-mutate--postimage-log-payload
               (list (list :version 1 :root 1 :owner-heading 7
                           :parent-owner-heading nil
                           :candidate-id (make-string 40 ?a)
                           :digest (make-string 64 ?b)
                           :deleted nil)))))
    ;; The well-typed payload parses…
    (should (dorg-mutate--postimage-entries-from-payload good))
    ;; …but not with trailing bytes after the one form,
    (should-not (dorg-mutate--postimage-entries-from-payload
                 (concat good " trailing")))
    (should-not (dorg-mutate--postimage-entries-from-payload
                 (concat good "(1)")))
    ;; …and not with any mistyped closed field.
    (dolist (bad (list
                  ;; root as string
                  (format "((%S 7 nil %S %S nil))" "1"
                          (make-string 40 ?a) (make-string 64 ?b))
                  ;; parent-owner as string
                  (format "((1 7 %S %S %S nil))" "x"
                          (make-string 40 ?a) (make-string 64 ?b))
                  ;; candidate-id not 40-hex
                  (format "((1 7 nil %S %S nil))" "zz"
                          (make-string 64 ?b))
                  ;; digest not 64-hex
                  (format "((1 7 nil %S %S nil))" (make-string 40 ?a) "zz")
                  ;; deleted non-boolean
                  (format "((1 7 nil %S %S 5))"
                          (make-string 40 ?a) (make-string 64 ?b))))
      (should-not (dorg-mutate--postimage-entries-from-payload bad)))))

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
         (validate (plist-get (dorg-mutate--postimage-plan ctx nil) :validate)))
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
       (replace (items)
                (drawer "PROPERTIES" ("BEAD_ID" ,key))
                (text ,text))))

(defun dorg-test-nm45t--replace-two (id key first second)
  "Return a whole-items replacement for ID with identity KEY and two texts."
  `(on (id ,id)
       (replace (items)
                (drawer "PROPERTIES" ("BEAD_ID" ,key))
                (text ,first)
                (text ,second))))

(defun dorg-test-nm45t--append (id text)
  "Return a typed append of TEXT after ID's last item."
  `(on (id ,id) (insert (text) (after-last) ,text)))

(defun dorg-test-nm45t--apply (src form)
  "Apply FORM to SRC and request its same-transaction candidate model."
  (dorg-api-execute form :mode 'apply :source src
                    :return-candidate-models t))

(defun dorg-test-nm45t--wire (dolt-dir root-id owner-id)
  "Return OWNER-ID's canonical wire model freshly loaded from DOLT-DIR."
  (dorg-mutate--postimage-wire-tree
   (dorg-model-load-tree dolt-dir root-id) owner-id))

(defun dorg-test-nm45t--wire-items (wire)
  "Return WIRE's ordered semantic item forms."
  (plist-get (cdr wire) :items))

(defun dorg-test-nm45t--attempt-wire (attempt owner-id)
  "Return OWNER-ID's candidate wire model from private ATTEMPT."
  (let* ((context (plist-get (plist-get attempt :sim) :index-context))
         (partition
          (cl-find owner-id
                   (dorg-ext-index-context-owner-partitions context)
                   :key #'dorg-ext-owner-partition-owner-id)))
    (should partition)
    (dorg-mutate--postimage-wire-tree
     (dorg-ext-owner-partition-candidate-postimage partition) owner-id)))

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
             (res (dorg-test-nm45t--apply
                   src (dorg-test-nm45t--append b "beta tail")))
             (candidate (plist-get (dorg-test-2ij34--model-for res b) :model))
             (fresh (dorg-test-nm45t--wire dolt-dir rid b))
             (expected '((drawer "PROPERTIES" ("BEAD_ID" "case-b")
                                  :post-blank 0)
                         (text "alpha body\n" :post-blank 1)
                         (text "beta tail" :post-blank 0))))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (should (equal expected (dorg-test-nm45t--wire-items candidate)))
        (should (equal candidate fresh)))

      ;; C: one embedded-newline paragraph, then replacement.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one
            c "case-c" "alpha body\nbeta tail\n"))
      (let ((before (dorg-head-commit dolt-dir)))
        (dorg-test-nm45t--apply
         src (dorg-test-nm45t--replace-one
              c "case-c" "ALPHA body\nbeta tail\n"))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (should (member '(text "ALPHA body\nbeta tail\n" :post-blank 0)
                        (dorg-test-nm45t--wire-items
                         (dorg-test-nm45t--wire dolt-dir rid c)))))

      ;; D: replace, append, then whole-items replacement.  The final
      ;; paragraph's parser line terminator must reach text_items.text_raw.
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--replace-one d "case-d" "alpha body\n"))
      (dorg-test-nm45t--apply
       src (dorg-test-nm45t--append d "beta tail"))
      (let* ((form (dorg-test-nm45t--replace-two
                    d "case-d" "ALPHA body" "beta tail\n"))
             (tail-id (dorg-test-nm45t--last-text-id dolt-dir d))
             (patches (dorg-api-normalize form src))
             (attempt (dorg-mutate--dolt-build-attempt
                       src patches '(:return-candidate-models t)))
             (candidate (dorg-test-nm45t--attempt-wire attempt d))
             (expected-update
              (format "UPDATE text_items SET text_raw = %s WHERE item_id = %d"
                      (dorg-sql-quote "beta tail\n") tail-id))
             (before (dorg-head-commit dolt-dir))
             (res nil))
        (should (member expected-update (plist-get attempt :sql)))
        (setq res (dorg-test-nm45t--apply src form))
        (should-not (equal before (dorg-head-commit dolt-dir)))
        (let ((readback
               (plist-get (dorg-test-2ij34--model-for res d) :model)))
          (should (equal candidate readback))
          (should (equal readback (dorg-test-nm45t--wire dolt-dir rid d)))
          (should (equal '((drawer "PROPERTIES" ("BEAD_ID" "case-d")
                                  :post-blank 0)
                           (text "ALPHA body" :post-blank 0)
                           (text "beta tail\n" :post-blank 0))
                         (dorg-test-nm45t--wire-items readback))))))))

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
