;;; dorg-ext-index-projector-tests.el --- Atomic derived-index projector -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED-first specification for tracked-regression — run a registered DOrg-owned index
;; projector automatically against the immutable candidate-state context
;; (`dorg-ext-index-context', tracked-regression) after core simulation, validate every
;; derived effect against the registering extension's declared table manifest, and
;; persist core mutation plus derived rows in ONE atomic transaction.
;;
;; Phase A1 (this section): the DOrg-owned MANIFEST / effect validator — a pure
;; function that confines a derived index effect to the extension's declared,
;; unqualified, owned tables and fails closed on core/materialized, qualified,
;; foreign/cross-extension, zero-target, multi-statement, and disallowed-verb
;; shapes.  No caller payload participates.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-ext)
(require 'dorg-mutate)
(require 'dorg-api)
(require 'dorg-mutate-tests)           ; Dolt fixtures: with-dolt / seed / apply

;;; ─── Phase A1: manifest / effect validator (pure, no server) ──────────

;; A projector returns TYPED ROW DELTAS, never SQL text — DOrg generates the SQL.
;; Reads/subqueries/joins/multi-table/non-deterministic escapes are therefore
;; IMPOSSIBLE BY CONSTRUCTION: an effect names exactly one table and carries only
;; literal column values.  These tests cover the structural validation boundary
;; and DOrg's deterministic SQL generation.

(defun dorg-test-ixp--reason (effect owned)
  "Return the `dorg-ext-index-error' reason from validating typed EFFECT against
OWNED, or `ok' plus the returned table as a cons when validation passes."
  (condition-case e
      (cons 'ok (dorg-ext-validate-index-effect effect owned))
    (dorg-ext-index-error (dorg-ext-index-error-reason e))))

(ert-deftest dorg-test-ixp-validator-accepts-owned-insert-and-delete ()
  "The validator accepts an insert or delete typed effect against a declared,
unqualified, owned table and returns the touched table name."
  (should (equal (cons 'ok "mytable")
                 (dorg-test-ixp--reason
                  '(:table "mytable" :insert ((root_id . 1) (id . 2) (title . "x")))
                  '("mytable" "other_owned"))))
  (should (equal (cons 'ok "mytable")
                 (dorg-test-ixp--reason
                  '(:table "mytable" :delete ((id . 2))) '("mytable"))))
  ;; A whole-table delete (nil where, e.g. a rebuild) is allowed on an owned table.
  (should (equal (cons 'ok "mytable")
                 (dorg-test-ixp--reason '(:table "mytable" :delete nil) '("mytable")))))

(ert-deftest dorg-test-ixp-validator-rejects-core-table ()
  "A typed effect targeting a core DOrg table fails closed, even if the extension
were to (illegitimately) name it — core tables are reserved and unreachable."
  (dolist (core '("headings" "heading_tags" "property_entries" "dorg_meta"
                  "text_items" "dorg_files"))
    (should (eq 'core-table
                (dorg-test-ixp--reason
                 (list :table core :delete '((id . 1))) (list core "mytable"))))))

(ert-deftest dorg-test-ixp-validator-rejects-qualified-target ()
  "A database-qualified target (db.table) is rejected: an effect must name an
unqualified owned table (no foreign-db reach)."
  (should (eq 'qualified
              (dorg-test-ixp--reason
               '(:table "otherdb.mytable" :insert ((id . 1))) '("mytable")))))

(ert-deftest dorg-test-ixp-validator-rejects-unowned-table ()
  "A target absent from the extension's declared manifest is rejected — covering
cross-extension and undeclared tables alike (multi-table reach is impossible: an
effect names exactly ONE table)."
  (should (eq 'unowned
              (dorg-test-ixp--reason
               '(:table "other_ext" :insert ((id . 1))) '("mytable"))))
  (should (eq 'unowned
              (dorg-test-ixp--reason
               '(:table "not_mine" :delete ((id . 1))) '("mytable" "other_owned")))))

(ert-deftest dorg-test-ixp-validator-rejects-malformed-effects ()
  "Structural rejections: a non-plist / missing table / missing op / bad column /
non-literal value all fail closed with distinct typed reasons."
  (should (eq 'not-an-effect (dorg-test-ixp--reason "INSERT INTO x ..." '("x"))))
  (should (eq 'not-an-effect (dorg-test-ixp--reason 42 '("x"))))
  (should (eq 'no-table (dorg-test-ixp--reason '(:table "" :insert ((id . 1))) '("x"))))
  (should (eq 'bad-op (dorg-test-ixp--reason '(:table "mytable") '("mytable"))))
  (should (eq 'bad-op (dorg-test-ixp--reason '(:table "mytable" :insert nil) '("mytable"))))
  ;; A column that is not a safe identifier symbol (hyphen/space/non-symbol):
  ;; DOrg splices column names raw, so only ^[A-Za-z_][A-Za-z0-9_]*$ is allowed.
  (should (eq 'bad-column
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id-with-hyphen . 1))) '("mytable"))))
  (should (eq 'bad-column
              (dorg-test-ixp--reason
               '(:table "mytable" :insert (("id" . 1))) '("mytable"))))
  ;; A value that is not a literal (a list / symbol — could smuggle an expression).
  (should (eq 'bad-value
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id . (foo)))) '("mytable"))))
  (should (eq 'bad-value
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id . some-symbol))) '("mytable")))))

(ert-deftest dorg-test-ixp-validator-rejects-table-name-escape ()
  "A manifest/effect table NAME is validated by a CLOSED identifier grammar before
lowering, so a delimiter/multi-statement escape via the table name (which DOrg
splices raw) cannot reach the generated SQL.  Exact Navigator probe:
  (:table \"demo_ix; DELETE FROM headings\") must NOT pass."
  (should (eq 'bad-table
              (dorg-test-ixp--reason
               '(:table "demo_ix; DELETE FROM headings" :delete nil)
               '("demo_ix; DELETE FROM headings"))))
  (should (eq 'bad-table
              (dorg-test-ixp--reason '(:table "a b" :delete nil) '("a b"))))
  (should (eq 'bad-table
              (dorg-test-ixp--reason '(:table "t-1" :insert ((id . 1))) '("t-1")))))

(ert-deftest dorg-test-ixp-validator-rejects-non-closed-effect-shape ()
  "The typed effect shape is CLOSED: EXACTLY one of :insert/:delete, and ONLY the
keys :table/:insert/:delete.  Both ops, an unknown key, or a duplicate key fail
closed — no silent op selection.  Exact Navigator probes."
  ;; Both :insert AND :delete present.
  (should (eq 'bad-op
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id . 1)) :delete ((id . 2))) '("mytable"))))
  ;; An unknown plist key.
  (should (eq 'not-an-effect
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id . 1)) :bogus yes) '("mytable"))))
  ;; A duplicate key.
  (should (eq 'not-an-effect
              (dorg-test-ixp--reason
               '(:table "mytable" :insert ((id . 1)) :insert ((id . 2))) '("mytable")))))

(ert-deftest dorg-test-ixp-registration-rejects-unsafe-read-table-name ()
  "Defense in depth: `dorg-register-extension' rejects a `:read-tables' descriptor
whose name is not a safe identifier, so an unsafe manifest never even registers."
  (let ((dorg--extensions nil))
    (should-error
     (dorg-register-extension
      'bad :schema '("CREATE TABLE x (id INT)") :version 1 :capabilities '(dolt)
      :read-tables '((:name "demo_ix; DELETE FROM headings"))
      :project #'ignore))))

(ert-deftest dorg-test-ixp-effect-sql-is-dorg-generated-and-bound ()
  "DOrg — not the extension — generates the SQL from a validated typed effect,
emitting only the owned table and literal, properly-quoted values (so a string
value cannot break out of its literal)."
  (should (equal "INSERT INTO demo_ix (root_id, heading_id, title) VALUES (100, 5, 'x')"
                 (dorg-ext--index-effect-sql
                  '(:table "demo_ix" :insert ((root_id . 100) (heading_id . 5) (title . "x"))))))
  (should (equal "DELETE FROM demo_ix WHERE heading_id = 5"
                 (dorg-ext--index-effect-sql
                  '(:table "demo_ix" :delete ((heading_id . 5))))))
  ;; A nil predicate VALUE lowers as `IS NULL' (SQL `= NULL' never matches), so a
  ;; NULL-keyed derived row can actually be removed.  A nil PAYLOAD (no cells) is
  ;; the distinct whole-table delete.
  (should (equal "DELETE FROM demo_ix WHERE parent_id IS NULL"
                 (dorg-ext--index-effect-sql
                  '(:table "demo_ix" :delete ((parent_id))))))
  (should (equal "DELETE FROM demo_ix WHERE parent_id IS NULL AND heading_id = 5"
                 (dorg-ext--index-effect-sql
                  '(:table "demo_ix" :delete ((parent_id) (heading_id . 5))))))
  (should (equal "DELETE FROM demo_ix"
                 (dorg-ext--index-effect-sql '(:table "demo_ix" :delete nil))))
  ;; NULL literal, and a hostile string value stays a single bound literal:
  ;; the embedded quote is DOUBLED by `dorg-sql-quote', so it cannot break out.
  (let ((sql (dorg-ext--index-effect-sql
              '(:table "demo_ix" :insert ((heading_id) (title . "a') DROP TABLE headings; --"))))))
    (should (string-prefix-p "INSERT INTO demo_ix (heading_id, title) VALUES (NULL, " sql))
    (should (string-match-p "'a'') DROP TABLE headings; --'" sql))))

;;; ─── Phase A2: single projector collector over the index-context ──────

(defun dorg-test-ixp--ctx (&optional source-kind)
  "Return a minimal `dorg-ext-index-context' for SOURCE-KIND (default `dolt')."
  (dorg-ext-index-context-create
   :source-kind (or source-kind 'dolt) :root-id 100 :dolt-dir "/tmp/ixp/"))

(defmacro dorg-test-ixp--with-clean-registry (&rest body)
  "Run BODY with an empty, restored-on-exit extension registry."
  (declare (indent 0))
  `(let ((dorg--extensions nil)) ,@body))

(ert-deftest dorg-test-ixp-collect-runs-projector-over-index-context ()
  "The collector runs a registered `:project' index projector against the
DOrg-owned index-context (NOT a caller payload), returns its SQL, and reports the
participant.  The projector receives the exact `dorg-ext-index-context'."
  (dorg-test-ixp--with-clean-registry
    (let ((seen nil))
      (dorg-register-extension
       'demo :schema "CREATE TABLE demo_ix (id INT)" :version 1
       :capabilities '(dolt) :read-tables '((:name "demo_ix" :key-column "id"))
       :project (lambda (ctx)
                  (setq seen ctx)
                  (list '(:table "demo_ix" :insert ((root_id . 100) (id . 1))))))
      (let* ((ctx (dorg-test-ixp--ctx))
             (proj (dorg-ext-collect-index-projection ctx)))
        (should (dorg-ext-index-context-p seen))
        (should (eql 100 (dorg-ext-index-context-root-id seen)))
        ;; DOrg generated the SQL from the typed delta.
        (should (equal '("INSERT INTO demo_ix (root_id, id) VALUES (100, 1)")
                       (plist-get proj :sql)))
        (should (equal '(:statements 1)
                       (cdr (assq 'demo (plist-get proj :extensions)))))))))

(ert-deftest dorg-test-ixp-collect-skips-ineligible-extensions ()
  "Only extensions declaring `:project' whose capabilities include the context
source kind run: a schema-only extension and a capability-excluded one are skipped."
  (dorg-test-ixp--with-clean-registry
    (dorg-register-extension 'schemaonly :schema "CREATE TABLE s (id INT)"
                             :version 1 :capabilities '(dolt))
    (dorg-register-extension
     'plainonly :schema "CREATE TABLE p (id INT)" :version 1
     :capabilities '(plain-org) :read-tables '((:name "p"))
     :project (lambda (_ctx) (list '(:table "p" :insert ((id . 1))))))
    (let ((proj (dorg-ext-collect-index-projection (dorg-test-ixp--ctx 'dolt))))
      (should-not (plist-get proj :sql))
      (should-not (plist-get proj :extensions)))))

(ert-deftest dorg-test-ixp-collect-rejects-manifest-violation ()
  "A projector effect that escapes its manifest (here a core table) aborts the
collection with a typed `dorg-ext-index-error' before any persistence."
  (dorg-test-ixp--with-clean-registry
    (dorg-register-extension
     'rogue :schema "CREATE TABLE demo_ix (id INT)" :version 1
     :capabilities '(dolt) :read-tables '((:name "demo_ix"))
     :project (lambda (_ctx) (list '(:table "headings" :delete ((id . 1))))))
    (should (eq 'core-table
               (condition-case e
                   (progn (dorg-ext-collect-index-projection (dorg-test-ixp--ctx))
                          nil)
                 (dorg-ext-index-error (dorg-ext-index-error-reason e)))))))

(ert-deftest dorg-test-ixp-collect-validate-signal-aborts ()
  "A registered `:validate' that signals aborts the collection (fail-closed)."
  (dorg-test-ixp--with-clean-registry
    (dorg-register-extension
     'vetoer :schema "CREATE TABLE demo_ix (id INT)" :version 1
     :capabilities '(dolt) :read-tables '((:name "demo_ix"))
     :validate (lambda (_ctx) (error "veto"))
     :project (lambda (_ctx) (list '(:table "demo_ix" :insert ((id . 1))))))
    (should-error (dorg-ext-collect-index-projection (dorg-test-ixp--ctx)))))

(ert-deftest dorg-test-ixp-collect-dirty-scope-is-owned-tables-touched ()
  "The collector's `:ext-dirty' is DOrg-owned: the set of owned tables the
validated effects actually touched — not a separate extension callback."
  (dorg-test-ixp--with-clean-registry
    (dorg-register-extension
     'two :schema "CREATE TABLE a_ix (id INT)" :version 1
     :capabilities '(dolt)
     :read-tables '((:name "a_ix") (:name "b_ix"))
     :project (lambda (_ctx)
                (list '(:table "a_ix" :delete ((root_id . 100)))
                      '(:table "b_ix" :insert ((root_id . 100) (id . 1))))))
    (let* ((proj (dorg-ext-collect-index-projection (dorg-test-ixp--ctx)))
           (dirty (cdr (assq 'two (plist-get proj :ext-dirty)))))
      (should (equal '(:statements 2)
                     (cdr (assq 'two (plist-get proj :extensions)))))
      (should (equal '("a_ix" "b_ix")
                     (sort (copy-sequence (plist-get dirty :tables)) #'string<))))))

;;; ─── Phase A3: e2e — atomic derived-row persistence + dry-run parity ──

(defun dorg-test-ixp--demo-project (ctx)
  "Demo index projector maintaining demo_ix over the FULL lifecycle.
Pure fn of the `dorg-ext-index-context' returning TYPED ROW DELTAS: upserts a row
for each created/changed/moved heading (delete-then-insert from its candidate
after-state) and removes the stale row for each deleted/tombstoned heading —
never SQL text and never a caller payload."
  (let ((root (dorg-ext-index-context-root-id ctx))
        ;; A per-candidate marker (the exact S0 base) so a discarded attempt's row
        ;; is distinguishable from the accepted attempt's (tracked-regression replan).
        (base (dorg-ext-index-context-base-commit ctx))
        effects)
    (dolist (e (append (dorg-ext-index-context-created ctx)
                       (dorg-ext-index-context-changed ctx)
                       (dorg-ext-index-context-moved ctx)))
      (let ((id (dorg-ext-index-entry-id e))
            (h (dorg-ext-index-entry-after e)))
        (push (list :table "demo_ix" :delete (list (cons 'heading_id id))) effects)
        (when h
          (push (list :table "demo_ix"
                      :insert (list (cons 'root_id root)
                                    (cons 'heading_id id)
                                    (cons 'title (or (dorg-heading-title h) ""))
                                    (cons 'base base)))
                effects))))
    (dolist (e (dorg-ext-index-context-deleted ctx))
      (push (list :table "demo_ix"
                  :delete (list (cons 'heading_id (dorg-ext-index-entry-id e))))
            effects))
    (nreverse effects)))

(defmacro dorg-test-ixp--with-demo (dolt-dir &rest body)
  "Register the demo index-projector extension in DOLT-DIR, then run BODY."
  (declare (indent 1))
  `(let ((dorg--extensions nil))
     (dorg-register-extension
      'demo
      :schema '("CREATE TABLE demo_ix (root_id BIGINT, heading_id BIGINT PRIMARY KEY, title TEXT, base TEXT)")
      :version 1 :capabilities '(dolt)
      :read-tables '((:name "demo_ix" :key-column "heading_id"))
      :project #'dorg-test-ixp--demo-project)
     (dorg-repo-ensure-extensions ,dolt-dir)
     (dorg-commit ,dolt-dir "install demo index projector")
     ,@body))

(defun dorg-test-ixp--scalar (dolt-dir sql)
  "Return the single scalar value of SQL against DOLT-DIR (nil if no row)."
  (let ((row (car (dorg-sql dolt-dir sql))))
    (and row (cdar row))))

(ert-deftest dorg-test-ixp-e2e-persists-derived-rows-atomically ()
  "On apply, the index projector runs automatically and its derived row is
committed in the SAME Dolt commit as the core change: exactly ONE new commit,
core title AND derived row updated, result reports :extensions + :ext-dirty."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (id (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (logn (lambda () (string-to-number
                               (format "%s" (dorg-test-ixp--scalar
                                             dolt-dir "SELECT COUNT(*) FROM dolt_log")))))
             (n0 (funcall logn))
             (res (dorg-test-mutate--apply-patch-batch
                   src (list (dorg-test-mutate--heading-update-patch id :title "Beta")))))
        ;; Core change persisted.
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM headings WHERE id = %d" id))))
        ;; Derived row persisted with the candidate title.
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d" id))))
        ;; Atomic: exactly ONE new commit carries BOTH lanes.
        (should (= (1+ n0) (funcall logn)))
        ;; Result surfaces the projector run + DOrg-owned dirty scope.
        (should (assq 'demo (plist-get res :extensions)))
        (should (member "demo_ix"
                        (plist-get (cdr (assq 'demo (plist-get (plist-get res :invalidation)
                                                               :ext-dirty)))
                                   :tables)))))))

(ert-deftest dorg-test-ixp-e2e-dry-run-derives-without-persisting ()
  "A dry-run runs the SAME derivation (reporting :extensions and a stable
:invalidation preview) but persists NOTHING: HEAD unchanged, demo_ix empty."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (id (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (h0 (dorg-head-commit dolt-dir))
             (plan (dorg-test-mutate--plan-patch-batch
                    src (list (dorg-test-mutate--heading-update-patch id :title "Beta")))))
        ;; The projector ran in the plan (same derivation as apply).
        (should (assq 'demo (plist-get plan :extensions)))
        (should (plist-get plan :invalidation))          ; stable preview present
        ;; Nothing persisted: HEAD unchanged and no derived row.
        (should (equal h0 (dorg-head-commit dolt-dir)))
        (should (equal "0" (format "%s" (dorg-test-ixp--scalar
                                         dolt-dir "SELECT COUNT(*) FROM demo_ix"))))
        ;; Core is likewise untouched by the dry-run.
        (should (equal "Alpha" (dorg-test-ixp--scalar
                                dolt-dir (format "SELECT title FROM headings WHERE id = %d" id))))))))

;;; ─── B3: lifecycle e2e — create / delete / move / cross-root / redo ───

(ert-deftest dorg-test-ixp-e2e-create-inserts-derived-row ()
  "A created heading yields a derived demo_ix row (from its candidate after-state),
committed atomically with the core insert."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (res (dorg-test-mutate--apply-patch-batch
                   src (list (dorg-test-mutate--tree-insert-patch
                              :root '(:title "Third")))))
             (new-id (car (plist-get res :created))))
        (should (integerp new-id))
        (should (equal "Third" (dorg-test-ixp--scalar
                                dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                                 new-id))))))))

(ert-deftest dorg-test-ixp-e2e-delete-removes-stale-derived-row ()
  "A deleted heading's stale demo_ix row is removed (tombstone maintenance) in the
same commit as the core delete."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n* Beta\n" 100))
             (a (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid))))
        ;; Seed a derived row for Alpha via an update, then delete Alpha.
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch a :title "Alpha*")))
        (should (dorg-test-ixp--scalar
                 dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d" a)))
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--delete-patch a)))
        ;; Stale row gone.
        (should-not (dorg-test-ixp--scalar
                     dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d" a)))))))

(ert-deftest dorg-test-ixp-e2e-public-move-updates-descendant-derived-rows ()
  "A public moved subtree updates derived rows for every changed descendant.
The API receipt and extension projector must both name the root plus its
descendants; otherwise their stale derived rows prove the widened `:changed'
contract was not propagated into the immutable index context."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed
                   dolt-dir "* Alpha\n** Kid\n*** Leaf\n* Beta\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (kid (dorg-test-mutate--id-by-title dolt-dir "Kid"))
             (leaf (dorg-test-mutate--id-by-title dolt-dir "Leaf"))
             (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
             (src (list :dolt-dir dolt-dir :root-id rid))
             (result nil)
             (moved-ids nil))
        (cl-letf (((symbol-function 'dorg-test-ixp--demo-project)
                   (let ((project (symbol-function 'dorg-test-ixp--demo-project)))
                     (lambda (ctx)
                       (setq moved-ids
                             (mapcar #'dorg-ext-index-entry-id
                                     (dorg-ext-index-context-moved ctx)))
                       (funcall project ctx)))))
          (setq result (dorg-api-execute
                        `(on (id ,alpha) (move (subtree) (under (id ,beta))))
                        :mode 'apply :source src)))
        (should (equal (sort (list alpha kid leaf) #'<)
                       (sort (copy-sequence
                              (plist-get (plist-get result :affected) :changed))
                             #'<)))
        ;; A level shift is a lineage move for the extension context, even
        ;; though descendants retain their parent_id and sort_key.
        (should (equal (sort (list alpha kid leaf) #'<)
                       (sort moved-ids #'<)))
        ;; `demo_ix' rows exist only when the projector receives every changed
        ;; descendant from the public receipt's immutable candidate context.
        (dolist (pair `((,alpha . "Alpha") (,kid . "Kid") (,leaf . "Leaf")))
          (should (equal (cdr pair)
                         (dorg-test-ixp--scalar
                          dolt-dir
                          (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                  (car pair))))))))))

(ert-deftest dorg-test-ixp-e2e-extension-effect-cannot-touch-document-lane ()
  "An extension index effect can NEVER write the core document lane: a projector
targeting `headings' is refused (core-table) before any write; the mutation
aborts, the document is unchanged, and HEAD does not advance.  Non-vacuity: the
same core mutation WITHOUT the rogue projector does change the document."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
           (a (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (src (dorg-mutate--resolve-source
                 (list :dolt-dir dolt-dir :root-id rid))))
      ;; Non-vacuity control: core-only mutation DOES change the document.
      (let ((dorg--extensions nil))
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch a :title "Beta")))
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM headings WHERE id = %d" a)))))
      ;; Rogue projector targeting the document lane is refused; document frozen.
      (let ((dorg--extensions nil) (h0 (dorg-head-commit dolt-dir)))
        (dorg-register-extension
         'rogue :schema '("CREATE TABLE demo_ix (id INT)") :version 1
         :capabilities '(dolt) :read-tables '((:name "demo_ix"))
         :project (lambda (_ctx)
                    (list (list :table "headings"
                                :delete (list (cons 'id a))))))
        (should-error (dorg-test-mutate--apply-patch-batch
                       src (list (dorg-test-mutate--heading-update-patch a :title "Gamma"))))
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM headings WHERE id = %d" a))))
        (should (equal h0 (dorg-head-commit dolt-dir)))))))

(ert-deftest dorg-test-9diah-dolt-validator-veto-runs-in-transaction ()
  "A Dolt validator veto in the transaction window rolls back the core change."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
           (a (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (src (dorg-mutate--resolve-source
                 (list :dolt-dir dolt-dir :root-id rid)))
           (h0 nil)
           (phases nil))
      (let ((dorg--extensions nil))
        (dorg-register-extension
         'dolt-veto
         :schema "CREATE TABLE validator_veto_ix (id INT)"
         :version 1
         :capabilities '(dolt)
         :validate (lambda (ctx)
                     (should (eq 'dolt
                                 (dorg-ext-index-context-source-kind ctx)))
                     (let ((phase (and (boundp 'dorg-ext--index-validation-phase)
                                       dorg-ext--index-validation-phase)))
                       (push phase phases)
                       (when (eq phase 'transaction)
                         (dorg-mutate--raise
                          'invariant "validator veto"
                          :detail 'candidate-validator-veto)))))
        (dorg-repo-ensure-extensions dolt-dir)
        (dorg-commit dolt-dir "install validator")
        (setq h0 (dorg-head-commit dolt-dir))
        (let ((data (dorg-test-mutate--error-data
                     (dorg-test-mutate--apply-patch-batch
                      src (list (dorg-test-mutate--heading-update-patch
                                 a :title "Beta"))))))
          (should (eq 'invariant (plist-get data :reason)))
          (should (eq 'candidate-validator-veto (plist-get data :detail)))))
      (should (member 'transaction phases))
      (should (equal h0 (dorg-head-commit dolt-dir)))
      (should (equal "Alpha"
                     (dorg-test-ixp--scalar
                      dolt-dir (format "SELECT title FROM headings WHERE id = %d"
                                       a)))))))

(ert-deftest dorg-test-ixp-e2e-re-derives-per-candidate ()
  "The projector is a function of the candidate: successive executions re-derive
from each new candidate post-state (never a cached/replayed prior effect)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (a (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid))))
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch a :title "Beta")))
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d" a))))
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch a :title "Gamma")))
        ;; Re-derived from the NEW candidate, not the stale prior effect.
        (should (equal "Gamma" (dorg-test-ixp--scalar
                                dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d" a))))))))

(ert-deftest dorg-test-ixp-e2e-cross-root-derives-destination ()
  "A cross-root archive move runs the projector against the aggregate cross-root
context and maintains the moved heading's derived row, committed atomically."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((src-root (dorg-test-mutate--seed dolt-dir "* Movable\n** Kid\n" 100))
             (dst-root (dorg-test-mutate--seed dolt-dir "* DstParent\n" 500))
             (movable (dorg-test-mutate--id-by-title dolt-dir "Movable"))
             (dst-parent (dorg-test-mutate--id-by-title dolt-dir "DstParent"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id src-root)))
             (dest (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id dst-root)))
             (patch (dorg-test-mutate--move-patch
                     movable (dorg-test-mutate--heading-address dst-parent)
                     :to-source dest)))
        ;; Dry-run/apply PARITY: the same projector runs and reports :extensions in
        ;; the dry-run plan as in apply (both share one consistency+projection
        ;; path).  Checked BEFORE apply (after apply the move is no longer valid).
        (should (assq 'demo (plist-get
                             (dorg-test-mutate--plan-patch-batch source (list patch))
                             :extensions)))
        (dorg-test-mutate--apply-patch-batch source (list patch))
        (should (equal "Movable"
                       (dorg-test-ixp--scalar
                        dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                         movable))))))))

(defun dorg-test-ixp--nd-project (ctx)
  "Projector emitting a nil-predicate delete (removes NULL-tag rows) per changed
heading — exercises IS NULL lowering."
  (mapcar (lambda (_e) '(:table "demo_nd" :delete ((tag))))
          (dorg-ext-index-context-changed ctx)))

(ert-deftest dorg-test-ixp-e2e-null-delete-removes-null-keyed-row ()
  "A typed delete with a nil predicate VALUE lowers as `IS NULL' and removes an
existing NULL-keyed derived row: before count 1, after count 0 (a `= NULL'
predicate would leave the stale row forever)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'nd :schema '("CREATE TABLE demo_nd (heading_id BIGINT, tag TEXT)")
       :version 1 :capabilities '(dolt) :read-tables '((:name "demo_nd"))
       :project #'dorg-test-ixp--nd-project)
      (dorg-repo-ensure-extensions dolt-dir)
      (dorg-commit dolt-dir "install nd")
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (a (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (cnt (lambda () (format "%s" (dorg-test-ixp--scalar
                                           dolt-dir "SELECT COUNT(*) FROM demo_nd")))))
        ;; A pre-existing NULL-keyed derived row.
        (dorg-sql dolt-dir (format "INSERT INTO demo_nd (heading_id, tag) VALUES (%d, NULL)" a))
        (dorg-commit dolt-dir "seed null row")
        (should (equal "1" (funcall cnt)))
        ;; A core change fires the projector's nil-predicate delete (IS NULL).
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch a :title "Beta")))
        (should (equal "0" (funcall cnt)))))))

;;; ─── B3: relation (edge) maintenance transformation ───────────────────

(defun dorg-test-ixp--rel-project (ctx)
  "Relation index projector: maintain a demo_rel (child_id -> parent_id) EDGE per
affected heading, derived from the candidate lineage.  A move transitions the edge
and removes the stale one; a delete removes it.  Typed row deltas."
  (let (effects)
    (dolist (e (append (dorg-ext-index-context-created ctx)
                       (dorg-ext-index-context-changed ctx)
                       (dorg-ext-index-context-moved ctx)))
      (let ((id (dorg-ext-index-entry-id e))
            (h (dorg-ext-index-entry-after e)))
        (push (list :table "demo_rel" :delete (list (cons 'child_id id))) effects)
        (when (and h (dorg-heading-parent-id h))
          (push (list :table "demo_rel"
                      :insert (list (cons 'child_id id)
                                    (cons 'parent_id (dorg-heading-parent-id h))))
                effects))))
    (dolist (e (dorg-ext-index-context-deleted ctx))
      (push (list :table "demo_rel"
                  :delete (list (cons 'child_id (dorg-ext-index-entry-id e))))
            effects))
    (nreverse effects)))

(ert-deftest dorg-test-ixp-e2e-relation-edge-transition ()
  "Relation maintenance: a projector derives a parent-child EDGE from the
candidate; a MOVE transitions the edge (old parent -> new parent) and removes the
stale edge, committed atomically with the core move."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'rel :schema '("CREATE TABLE demo_rel (child_id BIGINT PRIMARY KEY, parent_id BIGINT)")
       :version 1 :capabilities '(dolt) :read-tables '((:name "demo_rel"))
       :project #'dorg-test-ixp--rel-project)
      (dorg-repo-ensure-extensions dolt-dir)
      (dorg-commit dolt-dir "install rel")
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n** Kid\n* Beta\n" 100))
             (kid (dorg-test-mutate--id-by-title dolt-dir "Kid"))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (beta (dorg-test-mutate--id-by-title dolt-dir "Beta"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (edge (lambda () (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT parent_id FROM demo_rel WHERE child_id = %d"
                                                kid)))))
        ;; Seed the edge via a touch on Kid -> (kid -> alpha).
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--heading-update-patch kid :title "Kid*")))
        (should (equal (format "%d" alpha) (format "%s" (funcall edge))))
        ;; Move Kid under Beta: the edge TRANSITIONS to beta; the stale edge is gone
        ;; (exactly one row for kid, now pointing at beta).
        (dorg-test-mutate--apply-patch-batch
         src (list (dorg-test-mutate--move-patch
                    kid (dorg-test-mutate--heading-address beta))))
        (should (equal (format "%d" beta) (format "%s" (funcall edge))))
        (should (equal "1" (format "%s" (dorg-test-ixp--scalar
                                         dolt-dir (format "SELECT COUNT(*) FROM demo_rel WHERE child_id = %d"
                                                          kid)))))))))

;;; ─── B2b: forced accepted-replan re-derives from the new candidate ────

(ert-deftest dorg-test-ixp-e2e-accepted-replan-re-derives ()
  "On an ACCEPTED stale-CAS replan, DOrg DISCARDS attempt 1's context/effects and
re-derives from the replacement exact base.  A disjoint writer is injected BETWEEN
the target's attempt-1 planning (built at H0) and its gate check, moving HEAD to
H1; the gate discards attempt 1 and re-plans at H1.  The projector therefore runs
for the target on TWO DISTINCT bases (H0 then H1), attempt-1's derived effects are
discarded, and ONLY attempt 2 commits — exactly one derived row, carrying the
replacement candidate."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (dorg-test-ixp--with-demo dolt-dir
      (let* ((rid (dorg-test-mutate--seed dolt-dir "* Alpha\n* Other\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (other (dorg-test-mutate--id-by-title dolt-dir "Other"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid)))
             (h0 (dorg-head-commit dolt-dir))
             (h1 nil)
             ;; Per projector run: (BASE-COMMIT . CHANGED-IDS).
             (proj-log nil)
             (injected nil)
             (real-collect (symbol-function 'dorg-ext-collect-index-projection))
             (real-sim (symbol-function 'dorg-mutate--dolt-simulate)))
        (cl-letf (((symbol-function 'dorg-ext-collect-index-projection)
                   (lambda (ctx)
                     (push (cons (dorg-ext-index-context-base-commit ctx)
                                 (mapcar #'dorg-ext-index-entry-id
                                         (dorg-ext-index-context-changed ctx)))
                           proj-log)
                     (funcall real-collect ctx)))
                  ((symbol-function 'dorg-mutate--dolt-simulate)
                   (lambda (source patches options)
                     (let ((r (funcall real-sim source patches options)))
                       ;; After the TARGET's first sim (built at H0) and BEFORE its
                       ;; gate check, a DISJOINT writer commits, moving HEAD to H1.
                       (when (and (not injected) (member alpha (plist-get r :changed)))
                         (setq injected t)
                         (dorg-test--apply-patches
                          src (list (dorg-test--heading-update-patch other :title "Other*"))
                          :base-commit h0)
                         (setq h1 (dorg-head-commit dolt-dir)))
                       r))))
          (dorg-test--apply-patches
           src (list (dorg-test--heading-update-patch alpha :title "Beta"))
           :base-commit h0))
        (should injected)
        (should-not (equal h0 h1))
        ;; The projector ran for the TARGET on two attempts with DISTINCT bases:
        ;; attempt 1 at the stale H0 (discarded) and attempt 2 at the replacement H1.
        (let* ((target (cl-remove-if-not (lambda (e) (member alpha (cdr e))) proj-log))
               (bases (mapcar #'car target)))
          (should (>= (length target) 2))
          (should (member h0 bases))
          (should (member h1 bases)))
        ;; Only attempt 2 committed: exactly one derived row, carrying the H1
        ;; per-candidate marker — H0's discarded effect is ABSENT.  (Title alone
        ;; is identical across attempts, so the base marker is what discriminates a
        ;; discard from a replay of H0's effect.)
        (should (equal "1" (format "%s" (dorg-test-ixp--scalar
                                         dolt-dir (format "SELECT COUNT(*) FROM demo_ix WHERE heading_id = %d"
                                                          alpha)))))
        (should (equal h1 (dorg-test-ixp--scalar
                           dolt-dir (format "SELECT base FROM demo_ix WHERE heading_id = %d"
                                            alpha))))
        (should (equal "0" (format "%s" (dorg-test-ixp--scalar
                                         dolt-dir
                                         (format "SELECT COUNT(*) FROM demo_ix WHERE heading_id = %d AND base = %s"
                                                 alpha (dorg-sql-quote h0))))))
        (should (equal "Beta" (dorg-test-ixp--scalar
                               dolt-dir (format "SELECT title FROM demo_ix WHERE heading_id = %d"
                                                alpha))))))))

(defun dorg-test-ixp--owner-partition-for-affected (ctx heading-id)
  "Return CTX's owner partition whose affected ids include HEADING-ID."
  (cl-find-if
   (lambda (part)
     (member heading-id (dorg-ext-owner-partition-affected-ids part)))
   (dorg-ext-index-context-owner-partitions ctx)))

(defun dorg-test-ixp--owner-replan-project (ctx)
  "Project owner-local sibling state for changed headings.
This c4l63 RED projector is intentionally pure: it reads only the dedicated
owner-partition accessor and emits typed row deltas."
  (let ((base (dorg-ext-index-context-base-commit ctx))
        effects)
    (dolist (entry (dorg-ext-index-context-changed ctx))
      (let* ((id (dorg-ext-index-entry-id entry))
             (part (dorg-test-ixp--owner-partition-for-affected ctx id))
             (post (and part (dorg-ext-owner-partition-candidate-postimage part)))
             (sibling-title nil))
        (when post
          (maphash (lambda (_ hid)
                     (when (string-prefix-p "Sibling" (or (dorg-heading-title hid) ""))
                       (setq sibling-title (dorg-heading-title hid))))
                   post))
        (push (list :table "owner_ix" :delete (list (cons 'heading_id id))) effects)
        (push (list :table "owner_ix"
                    :insert (list (cons 'heading_id id)
                                  (cons 'base base)
                                  (cons 'sibling_title sibling-title)))
              effects)))
    (nreverse effects)))

(ert-deftest dorg-test-ixp-e2e-accepted-replan-rebuilds-owner-partitions ()
  "c4l63 R4: an accepted stale-CAS replan rebuilds owner partitions from the
replacement base.  Attempt 1 plans at H0, then a disjoint writer changes an
owner-local sibling and advances HEAD to H1 before the gate check.  The accepted
attempt must replan at H1, rebuild the owner partition from H1, and persist the
derived row carrying Sibling H1, not attempt 1's stale Sibling H0."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'owner-replan
       :schema '("CREATE TABLE owner_ix (heading_id BIGINT PRIMARY KEY, base TEXT, sibling_title TEXT)")
       :version 1 :capabilities '(dolt)
       :read-tables '((:name "owner_ix" :key-column "heading_id"))
       :project #'dorg-test-ixp--owner-replan-project)
      (dorg-repo-ensure-extensions dolt-dir)
      (dorg-commit dolt-dir "install owner replan projector")
      (let* ((rid (dorg-test-mutate--seed
                   dolt-dir
                   "* Parent\n:PROPERTIES:\n:BEAD_ID: parent\n:END:\n** Target\n** Sibling H0\n* Other\n"
                   100))
             (target (dorg-test-mutate--id-by-title dolt-dir "Target"))
             (sibling (dorg-test-mutate--id-by-title dolt-dir "Sibling H0"))
             (src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id rid
                         :identity '(:property "BEAD_ID"))))
             (h0 (dorg-head-commit dolt-dir))
             (h1 nil)
             (owner-log nil)
             (injected nil)
             (real-project (symbol-function 'dorg-test-ixp--owner-replan-project))
             (real-sim (symbol-function 'dorg-mutate--dolt-simulate)))
        (cl-letf (((symbol-function 'dorg-test-ixp--owner-replan-project)
                   (lambda (ctx)
                     (let* ((part (dorg-test-ixp--owner-partition-for-affected
                                   ctx target))
                            (post (and part
                                       (dorg-ext-owner-partition-candidate-postimage part)))
                            (sibling-title nil))
                       (when post
                         (maphash
                          (lambda (_ h)
                            (when (string-prefix-p "Sibling"
                                                   (or (dorg-heading-title h) ""))
                              (setq sibling-title (dorg-heading-title h))))
                          post))
                       (push (cons (dorg-ext-index-context-base-commit ctx)
                                   sibling-title)
                             owner-log))
                     (funcall real-project ctx)))
                  ((symbol-function 'dorg-mutate--dolt-simulate)
                   (lambda (source patches options)
                     (let ((r (funcall real-sim source patches options)))
                       (when (and (not injected)
                                  (member target (plist-get r :changed)))
                         (setq injected t)
                         (dorg-test--apply-patches
                          src (list (dorg-test--heading-update-patch
                                     sibling :title "Sibling H1"))
                          :base-commit h0)
                         (setq h1 (dorg-head-commit dolt-dir)))
                       r))))
          (dorg-test--apply-patches
           src (list (dorg-test--heading-update-patch target :title "Target*"))
           :base-commit h0))
        (should injected)
        (should-not (equal h0 h1))
        (should (member (cons h0 "Sibling H0") owner-log))
        (should (member (cons h1 "Sibling H1") owner-log))
        (should (equal h1 (dorg-test-ixp--scalar
                           dolt-dir
                           (format "SELECT base FROM owner_ix WHERE heading_id = %d"
                                   target))))
        (should (equal "Sibling H1" (dorg-test-ixp--scalar
                                     dolt-dir
                                     (format "SELECT sibling_title FROM owner_ix WHERE heading_id = %d"
                                             target))))))))

;;; ─── B2: plain-org explicitly refuses an eligible projector ───────────

(ert-deftest dorg-test-ixp-plain-org-eligible-projector-fails-closed ()
  "An eligible index projector on a plain-org source is EXPLICITLY refused (typed
`unsupported' / `:detail derived-index-unsupported-source') in BOTH plan and
apply — never silently dropped — and no core change persists.  Core-only
plain-org (no eligible projector) is unchanged."
  (dorg-test-mutate--with-content buf
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    ;; Core-only plain-org still applies (no eligible projector registered).
    (let ((dorg--extensions nil))
      (dorg-test-mutate--apply-patch-batch
       buf (list (dorg-test-mutate--property-put-patch 1 "A" "1")) :save t))
    ;; With an eligible plain-org projector, plan AND apply fail closed.
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'demo :schema '("CREATE TABLE demo_ix (id INT)") :version 1
       :capabilities '(plain-org dolt) :read-tables '((:name "demo_ix"))
       :project (lambda (_ctx) nil))
      (let ((err (should-error
                  (dorg-test-mutate--plan-patch-batch
                   buf (list (dorg-test-mutate--property-put-patch 1 "B" "2")))
                  :type 'dorg-mutate-error)))
        (should (eq 'unsupported (dorg-mutate-error-reason err)))
        (should (eq 'derived-index-unsupported-source
                    (plist-get (dorg-mutate-error-data err) :detail))))
      (should-error (dorg-test-mutate--apply-patch-batch
                     buf (list (dorg-test-mutate--property-put-patch 1 "B" "2"))
                     :save t)
                    :type 'dorg-mutate-error))))

;;; ─── Phase D: operation-qualified pre-apply consistency bar ───────────

(defun dorg-test-ixp--image (headings)
  "Return an id -> `dorg-heading' hash of HEADINGS."
  (let ((h (make-hash-table :test 'eql)))
    (dolist (x headings h) (puthash (dorg-heading-id x) x h))))

(ert-deftest dorg-test-ixp-consistency-refuses-undeclared-child-drop ()
  "S0 + candidate + declared typed effects must be mutually consistent: a child
present in S0 that the candidate DROPS without any declaring typed effect (the
request implicitly PRESERVED it) is an undeclared-drop violation."
  (let* ((p (make-dorg-heading :id 1 :title "P"))
         (a (make-dorg-heading :id 2 :parent-id 1 :title "A"))
         (b (make-dorg-heading :id 3 :parent-id 1 :title "B"))
         (orig (dorg-test-ixp--image (list p a b)))
         ;; The candidate dropped A (id 2); the patches declare NO deletion.
         (candidate (dorg-test-ixp--image (list p b)))
         (patches (list (dorg-test-mutate--heading-update-patch 1 :title "P*")))
         (declared (dorg-mutate--declared-removed-ids patches orig)))
    (should-not (member 2 declared))                 ; not declared removed
    (should (member 2 (dorg-mutate--candidate-drop-violations
                       orig candidate declared)))))

(ert-deftest dorg-test-ixp-consistency-accepts-declared-single-deletion ()
  "An explicit typed deletion of a child is a DECLARED removal — dropping it in
the candidate is consistent, never a violation."
  (let* ((p (make-dorg-heading :id 1 :title "P"))
         (a (make-dorg-heading :id 2 :parent-id 1 :title "A"))
         (orig (dorg-test-ixp--image (list p a)))
         (candidate (dorg-test-ixp--image (list p)))
         (patches (list (list :op 'delete :target (list :heading :id 2))))
         (declared (dorg-mutate--declared-removed-ids patches orig)))
    (should (member 2 declared))
    (should-not (dorg-mutate--candidate-drop-violations orig candidate declared))))

(ert-deftest dorg-test-ixp-consistency-mass-deletion-is-not-a-cardinality-veto ()
  "The bar is OPERATION-QUALIFIED, not a blanket cardinality veto (tracker-cli-id8i6
16-to-1): an explicit subtree deletion that declares removal of a parent and its
16 children is consistent even though the candidate drops all 17 identities."
  (let* ((root (make-dorg-heading :id 100 :title "Root"))
         (parent (make-dorg-heading :id 1 :parent-id 100 :title "P"))
         (kids (cl-loop for i from 2 to 17
                        collect (make-dorg-heading :id i :parent-id 1
                                                   :title (format "K%d" i))))
         (orig (dorg-test-ixp--image (append (list root parent) kids)))
         ;; ONE explicit subtree delete -> declares parent + all descendants.
         (patches (list (list :op 'delete :target (list :subtree :id 1))))
         (candidate (dorg-test-ixp--image (list root)))
         (declared (dorg-mutate--declared-removed-ids patches orig)))
    ;; Parent + all 16 kids are declared removed by the single subtree delete.
    (should (member 1 declared))
    (should (cl-every (lambda (k) (member (dorg-heading-id k) declared)) kids))
    ;; 17 drops, ALL declared -> zero violations (not vetoed for cardinality).
    (should-not (dorg-mutate--candidate-drop-violations orig candidate declared))))

(ert-deftest dorg-test-ixp-consistency-cross-root-aggregate-preserves-ids ()
  "The operation-qualified bar runs over the cross-root AGGREGATE S0/candidate
(both roots unioned): a valid move relocates an id (present in both) with NO false
positive, but a candidate that DROPS a preserved id across the move is refused.
No cross-root exemption exists."
  (let* ((sp (make-dorg-heading :id 100 :title "Src"))
         (m (make-dorg-heading :id 5 :parent-id 100 :title "M"))
         (dp (make-dorg-heading :id 200 :parent-id 300 :title "Dst"))
         (agg-orig (dorg-test-ixp--image (list sp m dp)))
         ;; Valid move: id 5 re-homes under 200, still present in the aggregate.
         (moved (make-dorg-heading :id 5 :parent-id 200 :title "M"))
         (agg-cand-ok (dorg-test-ixp--image (list sp moved dp)))
         ;; Corrupt: id 5 dropped entirely (the move patch declares no deletion).
         (agg-cand-drop (dorg-test-ixp--image (list sp dp)))
         (patches (list (list :op 'move :target (list :subtree :id 5))))
         (declared (dorg-mutate--declared-removed-ids patches agg-orig)))
    (should-not (dorg-mutate--candidate-drop-violations agg-orig agg-cand-ok declared))
    (should (member 5 (dorg-mutate--candidate-drop-violations
                       agg-orig agg-cand-drop declared)))))

(ert-deftest dorg-test-ixp-consistency-assertion-aborts-undeclared-drop ()
  "The pre-apply assertion signals a typed `invariant' `dorg-mutate-error' when
the candidate drops an undeclared child, before any persistence."
  (let* ((p (make-dorg-heading :id 1 :title "P"))
         (a (make-dorg-heading :id 2 :parent-id 1 :title "A"))
         (orig (dorg-test-ixp--image (list p a)))
         (candidate (dorg-test-ixp--image (list p)))
         (patches (list (dorg-test-mutate--heading-update-patch 1 :title "P*"))))
    (let ((err (should-error
                (dorg-mutate--assert-candidate-consistency orig candidate patches)
                :type 'dorg-mutate-error)))
      (should (eq 'invariant (dorg-mutate-error-reason err))))
    ;; A declared deletion does NOT abort.
    (should-not (dorg-mutate--assert-candidate-consistency
                 orig candidate
                 (list (list :op 'delete :target (list :heading :id 2)))))))

(provide 'dorg-ext-index-projector-tests)
;;; dorg-ext-index-projector-tests.el ends here
