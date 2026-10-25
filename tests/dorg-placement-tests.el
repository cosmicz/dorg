;;; dorg-placement-tests.el --- RED acceptance spec: promoted-orphan quarantine seam -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED acceptance specification for tracked-regression — the promoted-orphan
;; data-loss seam.  A stored CHILD heading that appears at column 1 in the
;; buffer is currently persisted as level=1/parent_id=root and re-emitted as a
;; top-level sibling forever (see the bead Design for the full root cause and
;; call-site map).  DOrg has no invariant tying `level' to a genuine top-level
;; identity, and — because post-promotion the state is structurally valid and
;; root-reachable — neither `dorg-render--warn-unreachable' nor `dorg-doctor'
;; catches it.
;;
;; These tests specify an EXTENSION-NEUTRAL placement/quarantine seam:
;;   - a new `:placement-invariant' participant on the `dorg-ext' contract;
;;   - `dorg-ext-classify-placement' invoked at the write-back (assign-ids) and
;;     render chokepoints;
;;   - quarantine semantics: preserve content, emit a classified
;;     `dorg-placement-diagnostic', mutate neither DB lineage nor buffer before
;;     the decision.
;;
;; SCOPE (tracked-regression dispatch): these are RED — the seam is NOT implemented in
;; this dispatch (design + spec only, stop at REVIEW).  They fail today (unbuilt
;; API / silent promotion) and are the acceptance bar for the follow-up impl.
;; Generic DOrg hard-codes NOTHING about Downstream tracker/BEAD_ID/section titles: every
;; test registers a LOCAL fixture invariant so the seam is exercised
;; extension-neutrally.  With NO registered invariant the seam is inert.
;;
;; ── reviewer REVIEW REJECT remediation (2026-07-10) ──────────────────────────
;; This revision addresses the three blocking SPEC findings:
;;   (1) The integration harness bound a fresh temp org-file whose path was NOT
;;       in `dorg_files', so `dorg-bind-dolt' allocated a SECOND root and
;;       `dorg-render--resolve-root-id' aborted with "multiple roots" BEFORE the
;;       parent_id/level assertion.  Fix: the seed now registers `dorg_files'
;;       with the REAL org-file path (`--seed-tree' takes the org-file), so bind
;;       reuses root 1 and the corrupt round-trip reaches `dorg-parse-assign-ids'
;;       and the assertion — which RED-fails because the owned child is silently
;;       promoted to parent=root.
;;   (2) `heading_properties' is a VIEW (columns prop_key/prop_value); a direct
;;       INSERT is rejected by Dolt, so the render-guard test failed at SEEDING,
;;       never reaching the classifier.  Fix: `--seed-property' writes through the
;;       real drawer/property base tables (heading_items[kind=drawer] ->
;;       drawer_items[PROPERTIES] -> property_entries), so the owned marker is
;;       present in the rendered snapshot and the render guard is load-bearing.
;;   (3) The no-false-positive case used an UNOWNED heading (ordinary / no
;;       opinion), which does not exercise the hard OC tension.  Fix: the fixture
;;       now returns an explicit `allow' verdict for an owned heading of a MOVABLE
;;       class; the added integration + unit cases prove the seam discriminates a
;;       legitimate owned-child-to-root move from corruption by the extension's
;;       CLASS MODEL (and move-intent), not by the observable star shape alone.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-ext)
(require 'dorg-model)
(require 'dorg-dolt)
(require 'dorg-registry)
(require 'dorg-render)
(require 'dorg-command)

;; Loaded lazily by the integration specs; declared so byte-compile is clean and
;; the void-function RED (unbuilt seam) surfaces at run time, not load time.
(declare-function dorg-ext-classify-placement "dorg-ext" (context))
(declare-function make-dorg-ext-placement-context "dorg-ext" (&rest args))
(declare-function make-dorg-ext-placement-verdict "dorg-ext" (&rest args))
(declare-function dorg-ext-placement-verdict-action "dorg-ext" (v))
(declare-function dorg-ext-placement-verdict-classification "dorg-ext" (v))
(declare-function dorg-placement-diagnostic-classification "dorg-ext" (d))

(defvar dorg-test--dolt-available)

;;; ─── Fixture: a local, extension-neutral placement invariant ────────────
;;
;; The fixture models an extension that OWNS headings carrying a "SECTION_OF"
;; property (a domain-neutral stand-in for Downstream tracker's section-ownership model).
;; The class model has two sub-classes, so the fixture proves the seam
;; discriminates by MODEL rather than by observable star shape:
;;   - a plain owned heading (SECTION_OF, no MOVABLE) is NEVER a legitimate
;;     top-level entity -> a buffer placement recording it as level=1/parent=root
;;     is a `promoted-orphan' to QUARANTINE;
;;   - an owned heading of a MOVABLE class (SECTION_OF + MOVABLE) legitimately
;;     may live at, or move to, root -> explicit `allow' verdict, no false
;;     quarantine even though the star shape is identical to the corrupt case;
;;   - a heading with an explicit move-intent on the path draws nil (the mutate
;;     DSL carried a reparent op -> intentional);
;;   - a heading WITHOUT the owner marker (an ordinary bead / top-level entity)
;;     draws nil (no opinion).
;; No Downstream tracker/BEAD_ID/section-title vocabulary appears in DOrg itself.

(defun dorg-test-placement--fixture-invariant (context)
  "Fixture `:placement-invariant': classify an owned child promoted to root.
Returns a `quarantine' verdict for a plain owned heading (a `SECTION_OF' marker
in the CONTEXT snapshot, NO `MOVABLE' marker) whose buffer-derived placement is
level=1/parent=root while its stored parent was a non-root heading.  Returns an
explicit `allow' verdict for an owned MOVABLE-class heading of the same shape
\(proving discrimination by class model, not star shape).  Ordinary headings and
explicit moves draw nil (no opinion)."
  (let* ((snap (dorg-ext-placement-context-snapshot context))
         (owned (and snap (assoc "SECTION_OF" snap)))
         (movable (and snap (assoc "MOVABLE" snap)))
         (buf-level (dorg-ext-placement-context-buffer-level context))
         (buf-parent (dorg-ext-placement-context-buffer-parent-id context))
         (stored-parent (dorg-ext-placement-context-stored-parent-id context))
         (root (dorg-ext-placement-context-root-id context))
         (move-intent (dorg-ext-placement-context-move-intent context))
         (promoted-shape (and (eql buf-level 1) (eql buf-parent root)
                              stored-parent (not (eql stored-parent root)))))
    (cond
     ;; Explicit reparent op on the path -> intentional move, no opinion.
     (move-intent nil)
     ;; Owner has no opinion unless the buffer would record the promoted shape.
     ((not (and owned promoted-shape)) nil)
     ;; Movable class: promotion-to-root is legitimate for this owned heading.
     (movable (dorg-test-placement--make-verdict 'movable-to-root 'allow
                                                 "owned movable-class heading moved to root"))
     ;; Plain owned section child promoted to root -> corruption.
     (t (dorg-test-placement--make-verdict 'promoted-orphan 'quarantine
                                           "owned section child promoted to root")))))

(ert-deftest dorg-test-placement-context-plist-facade-removed ()
  "Placement invariants consume the generated context accessors directly."
  (should-not (fboundp 'dorg-ext-placement-context-plist)))

;; The helper below names the TARGET verdict surface the seam must provide; it is
;; intentionally thin so the specs read against a stable shape and fail RED
;; (void-function) until dorg-ext supplies the real struct.
(defun dorg-test-placement--make-verdict (classification action reason)
  (make-dorg-ext-placement-verdict :classification classification
                                   :action action :reason reason))

(defmacro dorg-test-placement--with-fixture-ext (&rest body)
  "Run BODY with the fixture placement-invariant registered as extension `plc-fx'."
  (declare (indent 0))
  `(unwind-protect
       (progn
         ;; :version is required by the current registrar; :placement-invariant
         ;; is the NEW keyword the seam must add (ignored today -> seam inert ->
         ;; RED).
         (dorg-register-extension 'plc-fx
           :version 1
           :schema "CREATE TABLE IF NOT EXISTS plc_fx_noop (id BIGINT)"
           :placement-invariant #'dorg-test-placement--fixture-invariant)
         ,@body)
     (ignore-errors (dorg-unregister-extension 'plc-fx))))

;;; ─── Seeding helpers (real base tables; view-safe) ──────────────────────
;;
;; `heading_properties' is a VIEW (prop_key/prop_value); properties are seeded
;; through the real base tables so the rendered snapshot carries the owner marker
;; and the render/write-back classifier is actually reached.

(defvar dorg-test-placement--item-seq 5000
  "Monotone counter for synthetic heading_items ids in seeds (view-safe).")

(defun dorg-test-placement--seed-property (dolt-dir heading-id kvs)
  "Attach a PROPERTIES drawer carrying KVS to HEADING-ID in DOLT-DIR.
KVS is an alist of (KEY . VALUE) strings.  Writes through the real base tables
\(heading_items[kind=drawer] -> drawer_items[PROPERTIES] -> property_entries), so
the seed is accepted by Dolt (a direct INSERT into the `heading_properties' view
is rejected) and the rendered/parsed snapshot exposes the marker to the seam."
  (let ((item-id (cl-incf dorg-test-placement--item-seq))
        (ord 0)
        (stmts nil))
    (push (format "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (%d, %d, X'01', 'drawer', 0)"
                  item-id heading-id)
          stmts)
    (push (format "INSERT INTO drawer_items (item_id, name) VALUES (%d, 'PROPERTIES')"
                  item-id)
          stmts)
    (dolist (kv kvs)
      (cl-incf ord)
      (push (format "INSERT INTO property_entries (drawer_item_id, ordinal, prop_key, prop_value, raw_line) VALUES (%d, %d, %s, %s, %s)"
                    item-id ord
                    (dorg-sql-quote (car kv))
                    (dorg-sql-quote (cdr kv))
                    (dorg-sql-quote (format ":%s: %s" (car kv) (cdr kv))))
            stmts))
    (dorg-sql-batch dolt-dir (nreverse stmts))))

;;; ─── Classifier contract (unit) ─────────────────────────────────────────

(ert-deftest dorg-test-placement-classify-routes-quarantine-allow-and-noopinion ()
  "`dorg-ext-classify-placement' returns the owning invariant's verdict, else nil.
Covers all four class-model branches: plain owned -> quarantine; owned MOVABLE ->
explicit allow (SAME star shape, different verdict — the OC tension); ordinary
\(unowned) -> nil; explicit move-intent -> nil.
RED: the classifier + placement structs are unbuilt (void-function)."
  (dorg-test-placement--with-fixture-ext
    ;; Plain owned child promoted to root -> quarantine.
    (let ((v (dorg-ext-classify-placement
              (make-dorg-ext-placement-context
               :heading-id 101 :dorg-id 101 :root-id 1
               :stored-parent-id 100 :stored-level 2
               :buffer-parent-id 1 :buffer-level 1 :move-intent nil
               :snapshot '(("SECTION_OF" . "100"))))))
      (should (eq (dorg-ext-placement-verdict-action v) 'quarantine))
      (should (eq (dorg-ext-placement-verdict-classification v) 'promoted-orphan)))
    ;; Owned MOVABLE-class heading, IDENTICAL promoted shape -> explicit allow.
    (let ((v (dorg-ext-classify-placement
              (make-dorg-ext-placement-context
               :heading-id 101 :dorg-id 101 :root-id 1
               :stored-parent-id 100 :stored-level 2
               :buffer-parent-id 1 :buffer-level 1 :move-intent nil
               :snapshot '(("SECTION_OF" . "100") ("MOVABLE" . "t"))))))
      (should (eq (dorg-ext-placement-verdict-action v) 'allow))
      (should (eq (dorg-ext-placement-verdict-classification v) 'movable-to-root)))
    ;; Ordinary heading (not owned) -> no opinion.
    (should (null (dorg-ext-classify-placement
                   (make-dorg-ext-placement-context
                    :heading-id 102 :dorg-id 102 :root-id 1
                    :stored-parent-id 100 :stored-level 2
                    :buffer-parent-id 1 :buffer-level 1 :move-intent nil
                    :snapshot nil))))
    ;; Owned but explicit move-intent -> no opinion (intentional reparent op).
    (should (null (dorg-ext-classify-placement
                   (make-dorg-ext-placement-context
                    :heading-id 101 :dorg-id 101 :root-id 1
                    :stored-parent-id 100 :stored-level 2
                    :buffer-parent-id 1 :buffer-level 1 :move-intent t
                    :snapshot '(("SECTION_OF" . "100"))))))))

(ert-deftest dorg-test-placement-seam-inert-without-registered-invariant ()
  "With NO registered placement-invariant the classifier yields nil (seam inert).
Guards extension-neutrality: generic DOrg has no built-in opinion on a level-1
DORG_ID heading.  RED: unbuilt."
  (should (null (dorg-ext-classify-placement
                 (make-dorg-ext-placement-context
                  :heading-id 101 :dorg-id 101 :root-id 1
                  :stored-parent-id 100 :stored-level 2
                  :buffer-parent-id 1 :buffer-level 1 :move-intent nil
                  :snapshot '(("SECTION_OF" . "100")))))))

;;; ─── Render-side guard ──────────────────────────────────────────────────

(ert-deftest dorg-test-placement-render-guard-flags-stored-promoted-orphan ()
  "If a promoted orphan is ALREADY stored (level=1/parent=root but the owning
invariant classifies it corrupt), render must not silently re-emit it as a
top-level sibling — it consults the same classifier and diagnoses/refuses.
RED: the render-side classifier call is unbuilt.

The owner marker is seeded through the real property base tables (finding #2):
a direct INSERT into the `heading_properties' VIEW is rejected by Dolt."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-placement--with-fixture-ext
    (dorg-test--with-temp-db dolt-dir
      (dorg-sql-batch dolt-dir
        (list
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (1, 1, NULL, X'01', 0, FALSE, NULL, 0)"
         ;; 101 already corrupt: promoted to level1/parent=root but owned.
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (101, 1, 1, X'02', 1, FALSE, 'Owned child', 0)"
         "INSERT INTO dorg_files (root_id, relative_path) VALUES (1, 'beads.org')"))
      ;; SECTION_OF via the real drawer/property tables, NOT the view.
      (dorg-test-placement--seed-property dolt-dir 101 '(("SECTION_OF" . "100")))
      (dorg-commit dolt-dir "seed stored promoted orphan")
      (let ((diagnostics nil))
        (cl-letf (((symbol-function 'dorg-placement-diagnostic-emit)
                   (lambda (d) (push d diagnostics))))
          (dorg-render dolt-dir))
        (should diagnostics)
        (should (eq 'promoted-orphan
                    (dorg-placement-diagnostic-classification (car diagnostics))))))))

(ert-deftest dorg-test-level-offset-placement-context-effective ()
  "Only a direct root child one level down is classified, never by offset alone.
Owned 101 sits under the root at offset 1 and is classified.  Owned 201 also
has offset 1, but under 200, a level-2 heading, so it is at level 3 and is
not a top-level placement.  GUARD: green before offsets existed too."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-placement--with-fixture-ext
    (dorg-test--with-temp-db dolt-dir
      (dorg-sql-batch dolt-dir
        (list
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (1, 1, NULL, X'01', 0, FALSE, NULL, 0)"
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (101, 1, 1, X'02', 1, FALSE, 'Owned root child', 0)"
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (200, 1, 1, X'01', 2, FALSE, 'Gapped parent', 0)"
         "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank) VALUES (201, 1, 200, X'01', 1, FALSE, 'Owned nested child', 0)"
         "INSERT INTO dorg_files (root_id, relative_path) VALUES (1, 'beads.org')"))
      (dorg-test-placement--seed-property dolt-dir 101 '(("SECTION_OF" . "100")))
      (dorg-test-placement--seed-property dolt-dir 201 '(("SECTION_OF" . "100")))
      (dorg-commit dolt-dir "seed root and nested owned children")
      (let ((diagnostics nil))
        (cl-letf (((symbol-function 'dorg-placement-diagnostic-emit)
                   (lambda (d) (push d diagnostics))))
          (dorg-render dolt-dir))
        (should (equal '(101)
                       (mapcar #'dorg-placement-diagnostic-heading-id
                               diagnostics)))))))

(provide 'dorg-placement-tests)
;;; dorg-placement-tests.el ends here
