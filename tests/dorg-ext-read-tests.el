;;; dorg-ext-read-tests.el --- Extension read-plane SPEC tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED specification tests — the extension READ-PLANE:
;; grouped warm hydration from extension projection tables plus a cache
;; invalidation contract that CONSUMES the mutation-result invalidation
;; scopes the write-side participant seam emits.
;;
;; This is the read counterpart to dorg-ext-participant-tests.el.  It
;; mirrors that seam's purity model: an extension DECLARES read capability
;; (`:read-tables' manifest + `:hydrate' grouped-read callback) and returns
;; a bounded, set-based query PLAN; DOrg owns execution, caching, freshness,
;; and host-safe projection.  No bead/workflow vocabulary leaks into DOrg
;; (OC hard constraint).
;;
;; Proposed generic API (all domain-neutral):
;;
;;   (dorg-register-extension 'NAME
;;     :schema ... :version ... :capabilities '(dolt)
;;     :read-tables '((:name "T" :key-column "heading_id" :scope-column "root_id"))
;;     :hydrate #'FN)            ; pure: ctx -> grouped query plan
;;
;;   `dorg-ext-read-context' fields, all DOrg-owned:
;;     root-id source-kind dolt-dir file head query
;;   plan return: (:sql (S1 S2 ...) :reduce FN) — BOUNDED set-based SQL,
;;   never one query per row/heading; a bare SQL string/list is sugar.
;;
;;   Cache + invalidation:
;;     (dorg-ext-read-cache-make) -> cache
;;     (dorg-ext-hydrate cache ext-name source query) -> host-safe rows
;;     (dorg-ext-read-invalidate cache INVALIDATION) -> evicted count,
;;        consuming the kld9w (:roots :ext-dirty :head :dolt-dir :file) plist
;;     (dorg-ext-read--project value &rest opts) -> host-safe rows
;;     (dorg-ext-read-capability name source-kind) -> supported|unsupported
;;
;; The read-plane is now IMPLEMENTED (dorg-ext-read.el), which the file
;; `(require)'s; these tests are GREEN.  Run focused:
;;   make test SELECT=dorg-test-ext-read
;;
;; Each behavioral test is a transformation test (cache empty != warm) and
;; non-vacuous: the grouped-vs-per-row guard would PASS at N rows for a
;; per-row implementation and so pins the rejected one-shot path.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-ext)
(require 'dorg-mutate)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-ext-read)
(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el
(declare-function dorg-test--with-temp-db "dorg-test-helpers")
(declare-function dorg-test--parse-to-sql "dorg-test-helpers")
;; Read-plane API under specification (defined post-REVIEW in dorg-ext-read.el):
(declare-function dorg-ext-read-cache-make "dorg-ext-read")
(declare-function dorg-ext-hydrate "dorg-ext-read")
(declare-function dorg-ext-read-invalidate "dorg-ext-read")
(declare-function dorg-ext-read--resolve-source "dorg-ext-read")
(declare-function dorg-ext-read--freshness "dorg-ext-read")
(declare-function dorg-ext-read--cache-key "dorg-ext-read")
(declare-function dorg-ext-read--project "dorg-ext-read")
(declare-function dorg-ext-read-capability "dorg-ext")
(declare-function dorg-ext-read-context-root-id "dorg-ext-read")

;;; ─── Local fixtures ───────────────────────────────────────────────────

(defmacro dorg-test-extr--with-dolt (&rest body)
  "Run BODY with `dolt-dir' bound to a fresh temp Dolt repo, or skip.
Extensions are isolated via a clean `dorg--extensions' registry."
  (declare (indent 0) (debug (body)))
  `(if (not (bound-and-true-p dorg-test--dolt-available))
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       (let ((dorg--extensions nil))
         ,@body))))

;; SQL execution capture.  `dorg-sql' and its revision-bound sibling
;; `dorg-sql--at-revision' are DOrg's SQL chokepoint, so logging every call
;; lets a test PROVE grouped (set-based) reads do not fall back to
;; per-row/per-heading one-shot SQL.
(defvar dorg-test-extr--sql-log nil
  "Reversed list of SQL query strings issued while capturing.")

(defmacro dorg-test-extr--capturing-sql (&rest body)
  "Run BODY recording every chokepoint SQL query string into the log.
Captures both `dorg-sql' and the revision-bound `dorg-sql--at-revision'
\(structural snapshot binding routes plan statements through the latter)."
  (declare (indent 0) (debug (body)))
  `(let ((dorg-test-extr--sql-log nil))
     (cl-letf* ((orig (symbol-function 'dorg-sql))
                (orig-at (symbol-function 'dorg-sql--at-revision))
                ((symbol-function 'dorg-sql)
                 (lambda (dolt-dir query &rest args)
                   (push query dorg-test-extr--sql-log)
                   (apply orig dolt-dir query args)))
                ((symbol-function 'dorg-sql--at-revision)
                 (lambda (dolt-dir revision query &rest args)
                   (push query dorg-test-extr--sql-log)
                   (apply orig-at dolt-dir revision query args))))
       ,@body)))

(defun dorg-test-extr--proj-selects ()
  "Count captured SELECTs against the demo projection table."
  (cl-count-if (lambda (q) (string-match-p "demo_read_proj" q))
               dorg-test-extr--sql-log))

;; --- Demo read extension --------------------------------------------
;; demo_read_proj(heading_id, root_id, title, body) is a generic
;; projection table.  The :hydrate callback returns ONE grouped query
;; scoped by root_id — set-based, not per-row.

(defconst dorg-test-extr--schema
  '("CREATE TABLE demo_read_proj (heading_id BIGINT PRIMARY KEY, root_id BIGINT, title TEXT, body LONGTEXT)"))

(defconst dorg-test-extr--manifest
  '((:name "demo_read_proj" :key-column "heading_id" :scope-column "root_id"))
  "Generic read-table manifest for the demo extension.")

(defun dorg-test-extr--hydrate (ctx)
  "Demo :hydrate participant.  CTX is a `dorg-ext-read-context'.
Returns a GROUPED query plan: one set-based SELECT scoped by root_id.
Pure: SQL plan only, no execution."
  (list :sql
        (list (format (concat "SELECT heading_id, title, body FROM demo_read_proj "
                              "WHERE root_id = %d ORDER BY heading_id")
                      (dorg-ext-read-context-root-id ctx)))))

(defun dorg-test-extr--register (&optional opts)
  "Register the demo read extension.  OPTS overrides plist members."
  (apply #'dorg-register-extension 'demoread
         (append opts
                 (list :schema dorg-test-extr--schema
                       :version 1
                       :capabilities '(dolt)
                       :read-tables dorg-test-extr--manifest
                       :hydrate #'dorg-test-extr--hydrate))))

(defun dorg-test-extr--install (dolt-dir)
  "Apply the demo extension schema to an already-initialized DOLT-DIR."
  (dorg-repo-ensure-extensions dolt-dir)
  (dorg-commit dolt-dir "install demo read ext"))

(defun dorg-test-extr--seed (dolt-dir n &optional root-id)
  "Seed a root with N demo_read_proj rows into DOLT-DIR; commit.
Returns the root id (default 100)."
  (let* ((rid (or root-id 100))
         (rows (cl-loop for i from 1 to n
                        collect (format "(%d, %d, %s, %s)"
                                        (+ (* rid 100) i) rid
                                        (dorg-sql-quote (format "T%d" i))
                                        (dorg-sql-quote (format "body %d" i))))))
    (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql "* Root\n" rid))
    (dorg-sql-batch
     dolt-dir
     (list (format (concat "INSERT INTO demo_read_proj "
                           "(heading_id, root_id, title, body) VALUES %s")
                   (mapconcat #'identity rows ", "))))
    (dorg-commit dolt-dir "seed demo read proj")
    rid))

(defun dorg-test-extr--src (dolt-dir root)
  "Return a Dolt read source for DOLT-DIR / ROOT."
  (list :dolt-dir dolt-dir :root-id root))

(defun dorg-test-extr--cache-key (ext-name source query)
  "Return the private read cache key for EXT-NAME, SOURCE, and QUERY."
  (let ((resolved (dorg-ext-read--resolve-source source)))
    (dorg-ext-read--cache-key
     ext-name resolved query (dorg-ext-read--freshness resolved))))

;;; ─── Grouped warm hydration ──────────────────────────────────────────

(ert-deftest dorg-test-ext-read-hydrate-returns-grouped-rows ()
  "A registered :hydrate participant serves host-safe rows from its
projection table.  Transformation: an empty cache hydrates to the seeded
rows; their titles match the projected data."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (rows (dorg-ext-hydrate cache 'demoread
                                   (dorg-test-extr--src dolt-dir root)
                                   '(:scope :root))))
      (should (= (length rows) 3))
      (should (cl-every (lambda (r) (string-match-p "^T[0-9]+$"
                                                    (format "%s" (cdr (assq 'title r)))))
                        rows)))))

(ert-deftest dorg-test-ext-read-grouped-not-per-row ()
  "NON-VACUITY GUARD: a grouped hydrate of N projection rows issues a
CONSTANT (set-based) number of projection SELECTs, NOT one per row.  A
per-bead/per-heading one-shot implementation would issue N SELECTs and
trip this guard — that is exactly the path the bead rejects."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 20))
           (cache (dorg-ext-read-cache-make)))
      (dorg-test-extr--capturing-sql
        (dorg-ext-hydrate cache 'demoread
                          (dorg-test-extr--src dolt-dir root)
                          '(:scope :root))
        ;; Exactly one grouped projection SELECT — independent of N=20.
        (should (= (dorg-test-extr--proj-selects) 1))
        (should-not (= (dorg-test-extr--proj-selects) 20))))))

(ert-deftest dorg-test-ext-read-cache-hit-skips-sql ()
  "A warm cache hit re-serves rows WITHOUT re-issuing the grouped
projection query: the second identical hydrate at an unchanged HEAD makes
zero demo_read_proj SELECTs."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 4))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root)))
      ;; Warm it.
      (dorg-ext-hydrate cache 'demoread src '(:scope :root))
      ;; Second hydrate is a cache hit: no projection read.
      (dorg-test-extr--capturing-sql
        (let ((rows (dorg-ext-hydrate cache 'demoread src '(:scope :root))))
          (should (= (length rows) 4))
          (should (= (dorg-test-extr--proj-selects) 0)))))))

;;; ─── Pull invalidation: freshness discriminator in the cache key ─────

(ert-deftest dorg-test-ext-read-head-change-invalidates ()
  "PULL path: a Dolt HEAD change invalidates a warm Dolt-backed read.
After a commit advances HEAD, the next hydrate re-issues the grouped
projection query (the cache key folds HEAD, so the warm entry no longer
matches)."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root)))
      (dorg-ext-hydrate cache 'demoread src '(:scope :root))
      ;; Advance HEAD with any commit.
      (dorg-sql-batch dolt-dir
                      (list (format (concat "INSERT INTO demo_read_proj "
                                            "(heading_id, root_id, title, body) "
                                            "VALUES (%d, %d, 'T4', 'b4')")
                                    (+ (* root 100) 4) root)))
      (dorg-commit dolt-dir "advance head")
      ;; Re-hydrate: HEAD changed → key changed → grouped read re-runs.
      (dorg-test-extr--capturing-sql
        (let ((rows (dorg-ext-hydrate cache 'demoread src '(:scope :root))))
          (should (= (dorg-test-extr--proj-selects) 1))
          (should (= (length rows) 4)))))))

(ert-deftest dorg-test-ext-read-head-folds-into-cache-key ()
  "PULL key contract (Dolt): the private read cache key folds the Dolt
HEAD, so the same query against the same source yields a DIFFERENT key
after a commit.  Keys are `equal'-comparable and read/print-safe."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (src (dorg-test-extr--src dolt-dir root))
           (k1 (dorg-test-extr--cache-key 'demoread src '(:scope :root))))
      ;; Print-safe + stable.
      (should (equal k1 (read (prin1-to-string k1))))
      (should (equal k1 (dorg-test-extr--cache-key 'demoread src '(:scope :root))))
      ;; A commit changes HEAD → the key changes.
      (dorg-sql-batch dolt-dir
                      (list (format (concat "INSERT INTO demo_read_proj "
                                            "(heading_id, root_id, title, body) "
                                            "VALUES (%d, %d, 'T3', 'b3')")
                                    (+ (* root 100) 3) root)))
      (dorg-commit dolt-dir "advance head")
      (should-not (equal k1 (dorg-test-extr--cache-key 'demoread src '(:scope :root)))))))

(ert-deftest dorg-test-ext-read-unreadable-head-fails-closed ()
  "FAIL-CLOSED contract (Dolt): when `dorg-head-commit' cannot read HEAD,
repeated probes for the SAME source/query must produce DIFFERENT, still
read/print-safe private cache key values — never one shared
sentinel.  Otherwise a transient HEAD-read failure lets the second probe
hit warm cache and serve rows it could not prove fresh.

NON-VACUITY: on the unpatched code `dorg-ext-read--head' returned the
CONSTANT sentinel, so K1 and K2 below were `equal' and this test FAILED."
  (let ((src (list :dolt-dir "/nonexistent/dolt" :root-id 100)))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (&rest _) (error "HEAD unreadable"))))
      (let ((k1 (dorg-test-extr--cache-key 'demoread src '(:scope :root)))
            (k2 (dorg-test-extr--cache-key 'demoread src '(:scope :root))))
        ;; Both keys are read/print-safe.
        (should (equal k1 (read (prin1-to-string k1))))
        (should (equal k2 (read (prin1-to-string k2))))
        ;; Two unreadable probes never collapse onto one key → fail closed.
        (should-not (equal k1 k2))))))

(ert-deftest dorg-test-ext-read-plain-file-tick-folds-into-cache-key ()
  "PULL key contract (plain/file-backed): for a plain-org/file source the
cache key folds the file tick/mtime/hash, so editing the backing file
yields a DIFFERENT key.  Pins the file-backed invalidation half of the
AC without requiring a SQL backend."
  (let* ((file (make-temp-file "dorg-extr-" nil ".org" "* A\n"))
         (k1 (dorg-test-extr--cache-key 'demoread file '(:scope :root))))
    (unwind-protect
        (progn
          (should (equal k1 (read (prin1-to-string k1))))
          ;; Mutate the file contents (and hash); the key must change.
          (with-temp-buffer
            (insert "* A\n** B added\n")
            (write-region (point-min) (point-max) file nil 'silent))
          (should-not (equal k1 (dorg-test-extr--cache-key 'demoread file '(:scope :root)))))
      (delete-file file))))

;;; ─── Push invalidation: consume the kld9w mutation-result scopes ─────

(ert-deftest dorg-test-ext-read-push-invalidate-by-root ()
  "PUSH path: `dorg-ext-read-invalidate' consumes a mutation-result
:invalidation plist and evicts warm entries whose root is in :roots.  After
eviction the next hydrate re-runs the grouped read."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root)))
      (dorg-ext-hydrate cache 'demoread src '(:scope :root))
      ;; Feed the kld9w invalidation shape: (:roots :ext-dirty :head :file).
      (let ((evicted (dorg-ext-read-invalidate
                      cache (list :roots (list root)
                                  :ext-dirty nil
                                  :head "deadbeefdeadbeefdeadbeefdeadbeef"
                                  :file nil))))
        (should (>= evicted 1)))
      ;; Entry was evicted → next hydrate re-queries.
      (dorg-test-extr--capturing-sql
        (dorg-ext-hydrate cache 'demoread src '(:scope :root))
        (should (= (dorg-test-extr--proj-selects) 1))))))

(ert-deftest dorg-test-ext-read-push-invalidate-other-root-is-noop ()
  "NON-VACUITY control for push invalidation: invalidating a DIFFERENT
root does NOT evict this root's warm entry, so the next hydrate is still
a cache hit (zero projection reads).  Proves eviction is scope-targeted,
not a blanket clear."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root)))
      (dorg-ext-hydrate cache 'demoread src '(:scope :root))
      (let ((evicted (dorg-ext-read-invalidate
                      cache (list :roots (list 999999)
                                  :head "cafecafecafecafecafecafecafecafe"))))
        (should (= evicted 0)))
      ;; Untouched entry → cache hit, no projection read.
      (dorg-test-extr--capturing-sql
        (dorg-ext-hydrate cache 'demoread src '(:scope :root))
        (should (= (dorg-test-extr--proj-selects) 0))))))

(ert-deftest dorg-test-ext-read-push-invalidate-by-ext-dirty ()
  "PUSH path (ext dirty scope): an :ext-dirty entry naming the extension
evicts that extension's warm entries; an :ext-dirty naming a DIFFERENT
extension leaves them intact (non-vacuity)."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root)))
      (dorg-ext-hydrate cache 'demoread src '(:scope :root))
      ;; Unrelated ext dirty → no eviction.
      (should (= (dorg-ext-read-invalidate
                  cache (list :roots nil :ext-dirty '((otherext . t))))
                 0))
      ;; Our ext dirty → eviction.
      (should (>= (dorg-ext-read-invalidate
                   cache (list :roots nil :ext-dirty '((demoread . t))))
                  1)))))

(ert-deftest dorg-test-ext-read-known-head-observability-removed ()
  "The write-only known-head table and public accessor are absent before v0.1."
  (should-not (fboundp 'dorg-ext-read-cache-known-head))
  (should-not (fboundp 'dorg-ext-read-cache-fresh)))

;;; ─── Host-safe projection across a warm boundary ─────────────────────

(ert-deftest dorg-test-ext-read-result-is-host-safe ()
  "Hydrated rows are host-safe: read/print round-trippable with no live
markers, buffers, processes, or SQL connections.  `dorg-ext-read--project'
with :verify asserts the same."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3))
           (cache (dorg-ext-read-cache-make))
           (rows (dorg-ext-hydrate cache 'demoread
                                   (dorg-test-extr--src dolt-dir root)
                                   '(:scope :root))))
      ;; Round-trips through prin1/read unchanged.
      (should (equal rows (read (prin1-to-string rows))))
      ;; Explicit verify path does not signal.
      (should (dorg-ext-read--project rows :verify t)))))

;;; ─── Capability: fail-closed, source-generic ─────────────────────────

(ert-deftest dorg-test-ext-read-capability-fail-closed ()
  "A read participant declaring :capabilities '(dolt) is `supported' for a
dolt source and `unsupported' for plain-org — a deterministic fail-closed
verdict.  A schema-only ext with no :hydrate is `unsupported' even for a
declared source kind."
  (let ((dorg--extensions nil))
    (dorg-test-extr--register)
    (should (eq (dorg-ext-read-capability 'demoread 'dolt) 'supported))
    (should (eq (dorg-ext-read-capability 'demoread 'plain-org) 'unsupported))
    ;; Schema-only: no :hydrate → unsupported even for dolt.
    (dorg-register-extension 'schemaonly
                             :schema dorg-test-extr--schema :version 1
                             :capabilities '(dolt))
    (should (eq (dorg-ext-read-capability 'schemaonly 'dolt) 'unsupported))
    ;; Unknown extension → unsupported.
    (should (eq (dorg-ext-read-capability 'nope 'dolt) 'unsupported))))

;;; ─── Manifest declaration ────────────────────────────────────────────

(ert-deftest dorg-test-ext-read-manifest-is-recorded ()
  "The :read-tables manifest is stored on the registered extension and
retrievable as generic table metadata (no domain vocabulary)."
  (let ((dorg--extensions nil))
    (dorg-test-extr--register)
    (let ((tables (dorg-extension-read-tables (dorg-get-extension 'demoread))))
      (should (equal tables dorg-test-extr--manifest))
      (should (equal (plist-get (car tables) :name) "demo_read_proj")))))

;;; ─── OC constraint: no bead/workflow vocabulary in the read-plane API ─

(ert-deftest dorg-test-ext-read-api-is-domain-neutral ()
  "Hard OC constraint: the read-plane public API must not leak bead/
workflow/status/dependency/note vocabulary into DOrg.  Every planned API
symbol exists (pins the contract) and is domain-neutral."
  (let ((banned "bead\\|workflow\\|\\bstatus\\b\\|dependenc\\|\\bnote\\b\\|assignee\\|priority")
        (api '(dorg-ext-read-context
               dorg-ext-read-context-root-id
               dorg-ext-read-context-source-kind
               dorg-ext-read-context-head
               dorg-ext-read-context-query
               dorg-ext-read-cache
               dorg-ext-read-cache-make
               dorg-ext-hydrate
               dorg-ext-read-snapshot
               dorg-ext-read-as-of-suffix
               dorg-ext-read-invalidate
               dorg-ext-read--project
               dorg-ext-read-capability)))
    (dolist (sym api)
      (should (or (fboundp sym) (cl-find-class sym))))
    (dolist (sym api)
      (should-not (string-match-p banned (symbol-name sym))))))

(ert-deftest dorg-test-ext-read-removed-public-cache-symbols-absent ()
  "Removed read-cache helper/predicate symbols must not survive loading."
  (should-not (fboundp 'dorg-ext-read-cache-key))
  (should-not (fboundp 'dorg-ext-read-context-p)))

(ert-deftest dorg-test-ext-read-head-unreadable-predicate-removed ()
  "The definition-only unreadable-HEAD predicate is gone; its token is not.
`dorg-ext-read--head-unreadable-p' had no caller anywhere in DOrg or in any
non-vendored downstream, so it shipped nothing but a name.  Deleting it must
not disturb the unreadable-HEAD MECHANISM, which lives in the token constant
and counter that `dorg-ext-read--head' conses, nor the committed-head
predicate that actually gates AS-OF pinning.  Re-adding the predicate turns
the `should-not' RED (static proof; a pure deletion has no behavioural
signal of its own)."
  (should-not (fboundp 'dorg-ext-read--head-unreadable-p))
  (should (boundp 'dorg-ext-read--head-unreadable))
  (should (boundp 'dorg-ext-read--head-unreadable-counter))
  (should (fboundp 'dorg-ext-read--head-committed-p))
  ;; The surviving predicate still answers the three HEAD shapes correctly:
  ;; committed hash, plain (nil) source, and the unreadable-HEAD token.
  (should (dorg-ext-read--head-committed-p "deadbeef"))
  (should-not (dorg-ext-read--head-committed-p nil))
  (should-not (dorg-ext-read--head-committed-p
               (cons dorg-ext-read--head-unreadable 1))))

;;; ─── Snapshot-coherent versioned read (tracked-case) ───────────────────
;;
;; `dorg-ext-read-snapshot' returns (:rows :head :root) where :head is the
;; ONE committed Dolt HEAD the rows were read at.  Binding is DOrg-owned:
;; plans are ORDINARY SQL and DOrg executes every statement inside a
;; revision-bound scope at the captured token, so the rows are SELECTED AT
;; the token, not merely accompanied by a nearby HEAD probe.  `AS OF' is
;; REJECTED inside plans (the scope is the sole pin);
;; `dorg-ext-read-as-of-suffix' survives only for the separate multi-call
;; read-session pattern outside hydrate plans.

(declare-function dorg-ext-read-snapshot "dorg-ext-read")
(declare-function dorg-ext-read-as-of-suffix "dorg-ext-read")
(declare-function dorg-ext-read-context-head "dorg-ext-read")
(declare-function dorg-head-commit "dorg-dolt")
(declare-function dorg-commit "dorg-dolt")

(defun dorg-test-extr--merge-title-body (per-stmt)
  "Merge PER-STMT (TITLE-ROWS BODY-ROWS) into per-heading title+body rows."
  (let ((titles (car per-stmt)) (bodies (cadr per-stmt)) (out nil))
    (dolist (tr titles (nreverse out))
      (let* ((hid (cdr (assq 'heading_id tr)))
             (br (cl-find hid bodies
                          :key (lambda (r) (cdr (assq 'heading_id r))))))
        (push (list (cons 'heading_id hid)
                    (cons 'title (cdr (assq 'title tr)))
                    (cons 'body (and br (cdr (assq 'body br)))))
              out)))))

(defmacro dorg-test-extr--advancing-at (n dolt-dir hid col new-val &rest body)
  "Run BODY; before the Nth demo_read_proj SELECT executes, advance HEAD.
The advance sets COL of heading HID to NEW-VAL and commits, simulating a
concurrent writer landing a commit mid-read.  Spies both `dorg-sql' and
the revision-bound `dorg-sql--at-revision' (structural snapshot binding
routes plan statements through the latter); the injected UPDATE uses the
ORIGINAL `dorg-sql' — a live, unbound write — and fires exactly once."
  (declare (indent 5) (debug (form form form form form body)))
  `(let* ((seen 0) (fired nil)
          (orig (symbol-function 'dorg-sql))
          (orig-at (symbol-function 'dorg-sql--at-revision))
          (note (lambda (query)
                  (when (and (string-match-p "SELECT" query)
                             (string-match-p "demo_read_proj" query))
                    (setq seen (1+ seen))
                    (when (and (not fired) (= seen ,n))
                      (setq fired t)
                      (funcall orig ,dolt-dir
                               (format "UPDATE demo_read_proj SET %s = %s WHERE heading_id = %d"
                                       ,col (dorg-sql-quote ,new-val) ,hid))
                      (dorg-commit ,dolt-dir "concurrent advance"))))))
     (cl-letf (((symbol-function 'dorg-sql)
                (lambda (dd query &rest args)
                  (funcall note query)
                  (apply orig dd query args)))
               ((symbol-function 'dorg-sql--at-revision)
                (lambda (dd rev query &rest args)
                  (funcall note query)
                  (apply orig-at dd rev query args))))
       ,@body)))

(ert-deftest dorg-test-ext-read-snapshot-binds-rows-to-token ()
  "T1: HEAD advancing AFTER the token is captured but BEFORE the SQL read
does not leak into the result — the snapshot returns the captured-commit
state and `:head' names that commit.  The participant emits ordinary SQL;
the revision-bound scope supplies the pin.  Non-vacuity of the interleave
is asserted (HEAD really advanced); the structural-binding non-vacuity
lives in the tracked-case tests below, which were RED before execution
became revision-bound."
  (dorg-test-extr--with-dolt
    ;; Bound scope: advance-before-SQL is invisible; rows + token are pre-advance.
    (dorg-test-extr--register (list :hydrate #'dorg-test-extr--hydrate))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (hid (+ (* root 100) 1))
           (base (dorg-head-commit dolt-dir)))
      (let* ((cache (dorg-ext-read-cache-make))
             (res (dorg-test-extr--advancing-at 1 dolt-dir hid "title" "ADVANCED"
                    (dorg-ext-read-snapshot cache 'demoread
                                            (dorg-test-extr--src dolt-dir root)
                                            '(:scope :root))))
             (row (cl-find hid (plist-get res :rows)
                           :key (lambda (r) (cdr (assq 'heading_id r))))))
        (should (equal (cdr (assq 'title row)) "T1"))
        (should (equal (plist-get res :head) base))
        (should-not (equal (dorg-head-commit dolt-dir) base)))))) ; HEAD did advance

(ert-deftest dorg-test-ext-read-snapshot-no-torn-read-across-statements ()
  "T2: a commit landing BETWEEN the two statements of one hydrate plan does
not tear the result — the body read reflects the pre-advance snapshot,
coherent with the title read.  Ordinary two-statement SQL; the
revision-bound scope supplies the pin (see also
`dorg-test-ext-read-snapshot-unpinned-two-stmt-not-torn', RED before
execution became revision-bound)."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register
     (list :hydrate #'dorg-test-extr--hydrate-2stmt-unpinned))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (hid (+ (* root 100) 1)))
      (let* ((cache (dorg-ext-read-cache-make))
             ;; Advance the BODY between statement 1 (titles) and 2 (bodies).
             (res (dorg-test-extr--advancing-at 2 dolt-dir hid "body" "TORN"
                    (dorg-ext-read-snapshot cache 'demoread
                                            (dorg-test-extr--src dolt-dir root)
                                            '(:scope :root))))
             (row (cl-find hid (plist-get res :rows)
                           :key (lambda (r) (cdr (assq 'heading_id r))))))
        (should (equal (cdr (assq 'body row)) "body 1")))))) ; pre-advance, coherent

(ert-deftest dorg-test-ext-read-snapshot-token-is-committed-head ()
  "T4/AC2: at a stable HEAD the snapshot `:head' is the committed HEAD hash
\(`dorg-head-commit') — the SAME token the materialized Org carries as
`#+DORG_HASH:' — and the versioned `:rows' equal the bare-hydrate rows."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register (list :hydrate #'dorg-test-extr--hydrate))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 3)))
      (let* ((cache (dorg-ext-read-cache-make))
             (src (dorg-test-extr--src dolt-dir root))
             (res (dorg-ext-read-snapshot cache 'demoread src '(:scope :root))))
        (should (equal (plist-get res :head) (dorg-head-commit dolt-dir)))
        (should (stringp (plist-get res :head)))
        (should (= (length (plist-get res :rows)) 3))
        ;; #+DORG_HASH the materialized Org carries is dorg-head-commit too,
        ;; so the visible projection and the warm read name ONE commit.
        (let ((hash (with-temp-buffer
                      (insert (dorg-render dolt-dir))
                      (goto-char (point-min))
                      (when (re-search-forward "^#\\+DORG_HASH: *\\(.*\\)$" nil t)
                        (match-string 1)))))
          (should (equal hash (plist-get res :head))))))))

(ert-deftest dorg-test-ext-read-snapshot-warm-hit-carries-stored-token ()
  "T5/T7: a warm cache HIT re-serves the STORED rows AND their stored token
\(no new projection SELECT, no re-probed live HEAD): cold read computes the
token, the identical warm read returns the same token+rows."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register (list :hydrate #'dorg-test-extr--hydrate))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2)))
      (let* ((cache (dorg-ext-read-cache-make))
             (src (dorg-test-extr--src dolt-dir root))
             (cold (dorg-ext-read-snapshot cache 'demoread src '(:scope :root))))
        (dorg-test-extr--capturing-sql
          (let ((warm (dorg-ext-read-snapshot cache 'demoread src '(:scope :root))))
            (should (= (dorg-test-extr--proj-selects) 0)) ; served from cache
            (should (equal (plist-get warm :head) (plist-get cold :head)))
            (should (equal (plist-get warm :rows) (plist-get cold :rows)))))))))

(ert-deftest dorg-test-ext-read-snapshot-single-head-probe ()
  "Guardrail: one snapshot read probes committed HEAD EXACTLY once — the
cache key, context head, AS-OF pin, and `:head' all reuse ONE capture, so no
second probe can straddle a concurrent advance."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register (list :hydrate #'dorg-test-extr--hydrate))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (calls 0)
           (orig (symbol-function 'dorg-ext-read--head)))
      (cl-letf (((symbol-function 'dorg-ext-read--head)
                 (lambda (r) (cl-incf calls) (funcall orig r))))
        (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                (dorg-test-extr--src dolt-dir root)
                                '(:scope :root)))
      (should (= calls 1)))))

(ert-deftest dorg-test-ext-read-snapshot-fails-closed-on-unreadable-head ()
  "T8: a Dolt source whose HEAD is unreadable fails CLOSED — the snapshot
read signals rather than returning rows tagged with a non-committed token."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register (list :hydrate #'dorg-test-extr--hydrate))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2)))
      (cl-letf (((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) (error "HEAD unreadable"))))
        (should-error
         (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                 (dorg-test-extr--src dolt-dir root)
                                 '(:scope :root))
         :type 'dorg-ext-read-error)))))

;;; ─── Structural snapshot binding (tracked-case) ────────────────────────
;;
;; `dorg-ext-read-snapshot' must be a DOrg-OWNED execution property: rows
;; returned as (:rows R :head H) were provably selected at committed
;; revision H even when the :hydrate participant emits ORDINARY SQL with
;; no `AS OF' pin.  The tests below use deliberately UNPINNED participants
;; and interleave a commit between the token capture and plan execution
;; (or between two plan statements); on a caller-discipline implementation
;; they are RED — the later commit leaks into rows tagged with the earlier
;; token.  Each binding test also asserts the concurrent commit really
;; landed, so a broken injection cannot make it pass vacuously.

(defun dorg-test-extr--hydrate-2stmt-unpinned (ctx)
  "Unpinned two-statement :hydrate (titles, then bodies) — NO `AS OF'.
Structural snapshot binding, not participant discipline, must keep the
merged result coherent when a commit lands between the statements."
  (let ((rid (dorg-ext-read-context-root-id ctx)))
    (list :sql
          (list (format "SELECT heading_id, title FROM demo_read_proj WHERE root_id = %d ORDER BY heading_id" rid)
                (format "SELECT heading_id, body FROM demo_read_proj WHERE root_id = %d ORDER BY heading_id" rid))
          :reduce #'dorg-test-extr--merge-title-body)))

(defun dorg-test-extr--hydrate-nested-unpinned (ctx)
  "Unpinned :hydrate referencing the projection table THREE times in one
statement: outer, IN-subquery, and a scalar COUNT subquery.  A
one-occurrence pin (or any per-reference rewriting that misses nested
uses) leaves the inner references live and is provably insufficient."
  (let ((rid (dorg-ext-read-context-root-id ctx)))
    (list :sql
          (list (format
                 (concat
                  "SELECT p.heading_id, p.title, "
                  "(SELECT COUNT(*) FROM demo_read_proj i WHERE i.root_id = p.root_id) AS n "
                  "FROM demo_read_proj p WHERE p.root_id = %d "
                  "AND p.heading_id IN (SELECT heading_id FROM demo_read_proj WHERE root_id = %d) "
                  "ORDER BY p.heading_id")
                 rid rid)))))

(defmacro dorg-test-extr--advance-before-nth-select (n advance &rest body)
  "Run BODY; immediately before the Nth projection SELECT executes, call
ADVANCE (a zero-arg function) exactly once.  ADVANCE should mutate and
commit the store via `dorg-sql-batch'/`dorg-commit' (neither routes
through the spied executors, so the injected write itself is never
revision-bound).  Spies BOTH `dorg-sql' and — when the structural
executor exists — `dorg-sql--at-revision', so the injection point is the
same before and after snapshot binding becomes DOrg-owned."
  (declare (indent 2) (debug (form form body)))
  `(let* ((seen 0) (fired nil) (adv ,advance)
          (orig-sql (symbol-function 'dorg-sql))
          (orig-at (symbol-function 'dorg-sql--at-revision))
          (note (lambda (query)
                  (when (and (string-match-p "SELECT" query)
                             (string-match-p "demo_read_proj" query))
                    (setq seen (1+ seen))
                    (when (and (not fired) (= seen ,n))
                      (setq fired t)
                      (funcall adv))))))
     (cl-letf (((symbol-function 'dorg-sql)
                (lambda (dd query &rest args)
                  (funcall note query)
                  (apply orig-sql dd query args)))
               ((symbol-function 'dorg-sql--at-revision)
                (lambda (dd rev query &rest args)
                  (funcall note query)
                  (apply orig-at dd rev query args))))
       ,@body)))

(defun dorg-test-extr--advance-title (dolt-dir hid new-title)
  "Return a thunk that sets HID's title to NEW-TITLE in DOLT-DIR and commits."
  (lambda ()
    (dorg-sql-batch dolt-dir
                    (list (format "UPDATE demo_read_proj SET title = %s WHERE heading_id = %d"
                                  (dorg-sql-quote new-title) hid)))
    (dorg-commit dolt-dir "concurrent advance")))

(ert-deftest dorg-test-ext-read-snapshot-binds-unpinned-plan-structurally ()
  "AC1/AC3: a snapshot over an UNPINNED plan returns the captured-commit
rows with `:head' naming that commit, even when HEAD advances after the
token capture but before the first plan statement.  RED on
caller-discipline trunk: the post-capture commit leaks into rows tagged
with the pre-advance token."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)            ; plain unpinned hydrate
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (hid (+ (* root 100) 1))
           (base (dorg-head-commit dolt-dir))
           (cache (dorg-ext-read-cache-make))
           (res (dorg-test-extr--advance-before-nth-select 1
                    (dorg-test-extr--advance-title dolt-dir hid "ADVANCED")
                  (dorg-ext-read-snapshot cache 'demoread
                                          (dorg-test-extr--src dolt-dir root)
                                          '(:scope :root))))
           (row (cl-find hid (plist-get res :rows)
                         :key (lambda (r) (cdr (assq 'heading_id r))))))
      ;; The concurrent commit really landed (non-vacuity of the interleave).
      (should-not (equal (dorg-head-commit dolt-dir) base))
      ;; Structural binding: rows and token both name the captured commit.
      (should (equal (plist-get res :head) base))
      (should (equal (cdr (assq 'title row)) "T1")))))

(ert-deftest dorg-test-ext-read-snapshot-unpinned-two-stmt-not-torn ()
  "AC4: a commit landing BETWEEN two statements of an UNPINNED plan must
not tear the result — the reducer never receives mixed-revision input.
RED on caller-discipline trunk: statement two runs live and the merged
row pairs a pre-advance title with a post-advance body."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register
     (list :hydrate #'dorg-test-extr--hydrate-2stmt-unpinned))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (hid (+ (* root 100) 1))
           (base (dorg-head-commit dolt-dir))
           (cache (dorg-ext-read-cache-make))
           (res (dorg-test-extr--advance-before-nth-select 2
                    (lambda ()
                      (dorg-sql-batch dolt-dir
                                      (list (format "UPDATE demo_read_proj SET body = 'TORN' WHERE heading_id = %d" hid)))
                      (dorg-commit dolt-dir "concurrent advance"))
                  (dorg-ext-read-snapshot cache 'demoread
                                          (dorg-test-extr--src dolt-dir root)
                                          '(:scope :root))))
           (row (cl-find hid (plist-get res :rows)
                         :key (lambda (r) (cdr (assq 'heading_id r))))))
      (should-not (equal (dorg-head-commit dolt-dir) base))
      (should (equal (plist-get res :head) base))
      (should (equal (cdr (assq 'body row)) "body 1")))))

(ert-deftest dorg-test-ext-read-snapshot-nested-repeated-refs-bound ()
  "AC5: repeated and nested-subquery references to a registered table are
ALL bound to the captured revision — a row inserted after capture is
invisible to the outer read, the IN-subquery, and the scalar COUNT.
RED on caller-discipline trunk; also red for any one-occurrence fix."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register
     (list :hydrate #'dorg-test-extr--hydrate-nested-unpinned))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (base (dorg-head-commit dolt-dir))
           (cache (dorg-ext-read-cache-make))
           (res (dorg-test-extr--advance-before-nth-select 1
                    (lambda ()
                      (dorg-sql-batch dolt-dir
                                      (list (format "INSERT INTO demo_read_proj (heading_id, root_id, title, body) VALUES (%d, %d, 'H1ONLY', 'late')"
                                                    (+ (* root 100) 9) root)))
                      (dorg-commit dolt-dir "concurrent insert"))
                  (dorg-ext-read-snapshot cache 'demoread
                                          (dorg-test-extr--src dolt-dir root)
                                          '(:scope :root))))
           (rows (plist-get res :rows)))
      (should-not (equal (dorg-head-commit dolt-dir) base))
      (should (equal (plist-get res :head) base))
      ;; Outer reference bound: the late row is absent.
      (should (= (length rows) 2))
      (should-not (cl-find "H1ONLY" rows
                           :key (lambda (r) (cdr (assq 'title r)))
                           :test #'equal))
      ;; Nested scalar-subquery reference bound: COUNT sees two rows.
      (should (cl-every (lambda (r) (equal (cdr (assq 'n r)) 2)) rows)))))

(ert-deftest dorg-test-ext-read-snapshot-fails-closed-on-blank-head ()
  "AC9: a Dolt source whose HEAD reads back BLANK (empty string) cannot
pin a revision; the snapshot read fails closed with a typed error BEFORE
any plan SQL executes.  RED on trunk: only the unreadable-HEAD cons was
rejected, so a blank head executed unpinned and returned rows tagged
with the non-committed token \"\"."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)
    (dorg-test-extr--install dolt-dir)
    (let ((root (dorg-test-extr--seed dolt-dir 2)))
      (cl-letf (((symbol-function 'dorg-head-commit) (lambda (&rest _) "")))
        (dorg-test-extr--capturing-sql
          (should-error
           (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                   (dorg-test-extr--src dolt-dir root)
                                   '(:scope :root))
           :type 'dorg-ext-read-error)
          ;; Fail-closed means fail BEFORE the first plan statement.
          (should (= (dorg-test-extr--proj-selects) 0)))))))

(ert-deftest dorg-test-ext-read-snapshot-zero-statement-plan-bound ()
  "Boundary: a nil plan (zero statements) yields zero rows tagged with
the captured committed token — vacuously bound, never an error."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register (list :hydrate (lambda (_ctx) nil)))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 1))
           (res (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                        (dorg-test-extr--src dolt-dir root)
                                        '(:scope :root))))
      (should (null (plist-get res :rows)))
      (should (equal (plist-get res :head) (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-ext-read-reducer-failure-caches-nothing ()
  "Failure atomicity: a signalling :reduce caches NO entry — the next
identical read re-executes the plan instead of serving a phantom hit."
  (dorg-test-extr--with-dolt
    (let ((boom t))
      (dorg-test-extr--register
       (list :hydrate
             (lambda (ctx)
               (list :sql
                     (list (format "SELECT heading_id, title, body FROM demo_read_proj WHERE root_id = %d ORDER BY heading_id"
                                   (dorg-ext-read-context-root-id ctx)))
                     :reduce (lambda (per-stmt)
                               (if boom (error "reducer failed")
                                 (car per-stmt)))))))
      (dorg-test-extr--install dolt-dir)
      (let* ((root (dorg-test-extr--seed dolt-dir 2))
             (cache (dorg-ext-read-cache-make))
             (src (dorg-test-extr--src dolt-dir root)))
        (should-error
         (dorg-ext-read-snapshot cache 'demoread src '(:scope :root)))
        (setq boom nil)
        (dorg-test-extr--capturing-sql
          (let ((res (dorg-ext-read-snapshot cache 'demoread src '(:scope :root))))
            ;; Re-executed (no phantom cache hit), and now serves rows.
            (should (= (dorg-test-extr--proj-selects) 1))
            (should (= (length (plist-get res :rows)) 2))))))))

(ert-deftest dorg-test-ext-read-hydrate-shares-structurally-bound-entry ()
  "Shared-entry coherence: `dorg-ext-hydrate' and `dorg-ext-read-snapshot'
share one cache entry keyed by the same captured token, and a snapshot
warm hit serves a hydrate-populated entry as-is.  The hydrate execution
must therefore be revision-bound too — an unbound hydrate whose plan
straddles a commit would populate mixed-revision rows that a snapshot at
the captured head returns tagged with it.  RED on caller-discipline
trunk: the rows-only hydrate leaks the post-capture commit."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register)            ; plain unpinned hydrate
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (hid (+ (* root 100) 1))
           (base (dorg-head-commit dolt-dir))
           (cache (dorg-ext-read-cache-make))
           (src (dorg-test-extr--src dolt-dir root))
           (rows (dorg-test-extr--advance-before-nth-select 1
                     (dorg-test-extr--advance-title dolt-dir hid "LEAKED")
                   (dorg-ext-hydrate cache 'demoread src '(:scope :root))))
           (row (cl-find hid rows
                         :key (lambda (r) (cdr (assq 'heading_id r))))))
      (should-not (equal (dorg-head-commit dolt-dir) base))
      ;; The rows-only hydrate is itself bound to the captured token.
      (should (equal (cdr (assq 'title row)) "T1"))
      ;; Shared entry: at a now-STABLE head, a fresh hydrate populates the
      ;; entry and the snapshot warm-hits it — zero plan SQL, stored rows
      ;; and stored token returned together.
      (let* ((cache2 (dorg-ext-read-cache-make))
             (head2 (dorg-head-commit dolt-dir))
             (rows2 (dorg-ext-hydrate cache2 'demoread src '(:scope :root))))
        (dorg-test-extr--capturing-sql
          (let ((warm (dorg-ext-read-snapshot cache2 'demoread src '(:scope :root))))
            (should (= (dorg-test-extr--proj-selects) 0))
            (should (equal warm (list :rows rows2 :head head2 :root root)))))))))

;; Fail-closed plan validation (acceptance-review P1, final form per the
;; review decision of [2026-08-13 Thu 15:22]): a revision-bound plan
;; rejects, BEFORE its first statement executes, every form that would
;; resolve outside the DOrg-owned scope — database qualification (live or
;; revision/branch, comment-obscured included), statement smuggling, and
;; ANY `AS OF', including one naming the captured hash: the scope is the
;; sole snapshot authority.  Checks are declarative regexps over a
;; literal/comment mask, not a SQL parser.

(defun dorg-test-extr--reject-plan-test (make-stmt)
  "Run a snapshot over a single-statement plan built by MAKE-STMT.
MAKE-STMT receives (DB-NAME CTX) and returns the statement string.
Asserts the read fails typed (`dorg-ext-read-error') with ZERO plan
statements executed."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register
     (list :hydrate
           (lambda (ctx)
             (list :sql
                   (list (funcall make-stmt
                                  (dorg-dolt-server--db-name
                                   (dorg-ext-read-context-dolt-dir ctx))
                                  ctx))))))
    (dorg-test-extr--install dolt-dir)
    (let ((root (dorg-test-extr--seed dolt-dir 2)))
      (dorg-test-extr--capturing-sql
        (should-error
         (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                 (dorg-test-extr--src dolt-dir root)
                                 '(:scope :root))
         :type 'dorg-ext-read-error)
        (should (= (dorg-test-extr--proj-selects) 0))))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-live-db-qualified-plan ()
  "P1: a plan statement that database-qualifies a table against the LIVE
database would resolve outside the revision scope; rejected typed BEFORE
execution.  RED pre-validator: the statement executed live and returned
rows DOrg tagged with the captured token."
  (dorg-test-extr--reject-plan-test
   (lambda (db ctx)
     (format "SELECT heading_id, title FROM %s.demo_read_proj WHERE root_id = %d"
             db (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-backtick-db-qualified-plan ()
  "P1: backtick-quoted live-database qualification is rejected alike."
  (dorg-test-extr--reject-plan-test
   (lambda (db ctx)
     (format "SELECT heading_id, title FROM `%s`.demo_read_proj WHERE root_id = %d"
             db (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-comment-hidden-qualification ()
  "P1: a block comment between the database name and the dot is whitespace
to the SQL engine; the mask turns comments into spaces, so
`db/**/.tbl' is still live qualification and is rejected."
  (dorg-test-extr--reject-plan-test
   (lambda (db ctx)
     (format "SELECT heading_id, title FROM %s/**/.demo_read_proj WHERE root_id = %d"
             db (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-revision-qualified-plan ()
  "P1: an explicitly revision/branch-qualified identifier (`db/main`.tbl)
names history DOrg does not own for this read; rejected typed."
  (dorg-test-extr--reject-plan-test
   (lambda (db ctx)
     (format "SELECT heading_id, title FROM `%s/main`.demo_read_proj WHERE root_id = %d"
             db (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-foreign-as-of-plan ()
  "P1: `AS OF' naming a moving branch ref breaks the snapshot claim;
rejected typed before execution."
  (dorg-test-extr--reject-plan-test
   (lambda (_db ctx)
     (format "SELECT heading_id, title FROM demo_read_proj AS OF 'main' WHERE root_id = %d"
             (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-captured-hash-as-of-plan ()
  "Review decision: even `AS OF' naming the CAPTURED hash is rejected —
the DOrg revision scope is the sole snapshot authority, and a redundant
caller-supplied pin only expands the validator/error surface.  Uses
`dorg-ext-read-as-of-suffix' exactly as a migration-era participant
would, and asserts that plan now fails closed."
  (dorg-test-extr--reject-plan-test
   (lambda (_db ctx)
     (format "SELECT heading_id, title FROM demo_read_proj%s WHERE root_id = %d"
             (dorg-ext-read-as-of-suffix (dorg-ext-read-context-head ctx))
             (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-rejects-multi-statement-plan ()
  "P1: a plan element containing a statement separator could smuggle a
session-scope change (e.g. a USE) past the pin; each `:sql' element is
ONE statement and anything else is rejected typed before execution."
  (dorg-test-extr--reject-plan-test
   (lambda (_db ctx)
     (format "SELECT heading_id FROM demo_read_proj WHERE root_id = %d; SELECT 1"
             (dorg-ext-read-context-root-id ctx)))))

(ert-deftest dorg-test-ext-read-snapshot-allows-db-name-inside-literal ()
  "Guard against false positives: the database name followed by a dot
INSIDE a string literal is data, not qualification — the plan executes
and the snapshot serves rows.  Pins the literal-awareness of the mask."
  (dorg-test-extr--with-dolt
    (dorg-test-extr--register
     (list :hydrate
           (lambda (ctx)
             (let ((db (dorg-dolt-server--db-name
                        (dorg-ext-read-context-dolt-dir ctx))))
               (list :sql
                     (list (format
                            "SELECT heading_id, title FROM demo_read_proj WHERE root_id = %d AND title <> '%s.x' ORDER BY heading_id"
                            (dorg-ext-read-context-root-id ctx) db)))))))
    (dorg-test-extr--install dolt-dir)
    (let* ((root (dorg-test-extr--seed dolt-dir 2))
           (res (dorg-ext-read-snapshot (dorg-ext-read-cache-make) 'demoread
                                        (dorg-test-extr--src dolt-dir root)
                                        '(:scope :root))))
      (should (= (length (plist-get res :rows)) 2))
      (should (equal (plist-get res :head) (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-ext-read-snapshot-plain-source-explicitly-unversioned ()
  "AC10: a snapshot over a plain (non-Dolt) source returns `:head' nil —
an explicitly unversioned result, never a bogus token — and never
attempts revision binding (there is no revision to pin).  The rows-only
plain hydrate contract is unchanged: same rows, no token."
  (let ((dorg--extensions nil)
        (file (make-temp-file "dorg-extr-plain-" nil ".org" "* Root\n")))
    (unwind-protect
        (progn
          (dorg-register-extension 'plainread
            :schema "CREATE TABLE plain_unused (id INT)"
            :version 1
            :capabilities '(plain-org)
            :hydrate (lambda (_ctx)
                       ;; No SQL against a plain source; rows come from the
                       ;; reducer over an empty statement list.
                       (list :sql nil
                             :reduce (lambda (_per-stmt)
                                       (list '((title . "PLAIN")))))))
          (let* ((cache (dorg-ext-read-cache-make))
                 (res (dorg-ext-read-snapshot cache 'plainread file nil))
                 (rows (dorg-ext-hydrate cache 'plainread file nil)))
            (should (equal (plist-get res :rows) '(((title . "PLAIN")))))
            (should (null (plist-get res :head)))
            (should (equal rows (plist-get res :rows)))))
      (delete-file file))))

(ert-deftest dorg-test-ext-read-snapshot-binding-cli-seam ()
  "AC6 (CLI half): under the sanctioned one-shot CLI seam against a
standalone store, the same unpinned plan is revision-bound with the same
outcome — the post-capture commit does not leak.  RED on
caller-discipline trunk.  (The managed-MySQL half is the same scenario
run under the shared-server fixture, i.e. the other tests in this file
when the suite server is up.)"
  (if (not (bound-and-true-p dorg-test--dolt-available))
      (ert-skip "dolt not installed")
    (let ((dorg-test--server-port nil))   ; force standalone copy-repo + CLI seam
      (dorg-test--with-temp-db dolt-dir
        (let ((dorg--extensions nil))
          (dorg-test-extr--register)
          (dorg-test-extr--install dolt-dir)
          (let* ((root (dorg-test-extr--seed dolt-dir 2))
                 (hid (+ (* root 100) 1))
                 (base (dorg-head-commit dolt-dir))
                 (cache (dorg-ext-read-cache-make))
                 (res (dorg-test-extr--advance-before-nth-select 1
                          (dorg-test-extr--advance-title dolt-dir hid "ADVANCED")
                        (dorg-ext-read-snapshot cache 'demoread
                                                (dorg-test-extr--src dolt-dir root)
                                                '(:scope :root))))
                 (row (cl-find hid (plist-get res :rows)
                               :key (lambda (r) (cdr (assq 'heading_id r))))))
            (should-not (equal (dorg-head-commit dolt-dir) base))
            (should (equal (plist-get res :head) base))
            (should (equal (cdr (assq 'title row)) "T1"))))))))

(provide 'dorg-ext-read-tests)
;;; dorg-ext-read-tests.el ends here
