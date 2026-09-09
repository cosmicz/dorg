;;; dorg-error-tests.el --- Public error taxonomy (dorg-error.el) SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED SPEC for the consolidated public error taxonomy (audit finding F2,
;; bead tracked-case).  The three surface conditions — `dorg-api-error'
;; (command), `dorg-mutate-error' (engine), and `dorg-ql-error' (query) — are
;; reparented under a single public ROOT condition `dorg-error', and a single
;; generic accessor pair (`dorg-error-data' / `dorg-error-reason') spans the
;; whole taxonomy.  The typed leaves are PRESERVED (a caller may still branch
;; on the specific condition); only a common ancestor and one introspection
;; surface are added.
;;
;; SOURCE OF TRUTH is docs/api.org + docs/mutate.org (rooted-taxonomy section).
;;
;; `dorg-error.el' does not exist yet and the leaves are not yet reparented, so
;; the parent/accessor tests are RED (void-function / failed `equal') until the
;; IMPL lands.  These tests require only the already-loadable leaf modules and
;; exercise the real signal helpers (`dorg-api--raise', `dorg-mutate--raise',
;; `dorg-ql--error') so the taxonomy contract is pinned against live signals.
;;
;; Non-vacuity notes (per navigator review, this SPEC revision):
;;   * Ancestry tests pin the EXACT `error-conditions' list of the root and
;;     every leaf — this excludes extra parents and any root/leaf cycle, and
;;     the root message is pinned to the exact string.
;;   * Live shapes from all three real raisers are pinned by `equal' (message
;;     + plist order/content), so an implementation cannot silently reshape a
;;     raised condition while the suite stays green.
;;   * `dorg-error-data' is guarded against a naive `(nth 2 err)': a rooted
;;     condition whose third slot is non-plist data must return nil.
;;   * Leaf-accessor isolation is pinned BIDIRECTIONALLY for BOTH accessors
;;     (data and reason), since both duplicated pairs are rewritten to delegate.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-api nil t)                ; loads dorg-api + dorg-mutate + dorg-ql
(require 'dorg-ql nil t)
(require 'dorg-error nil t)             ; RED: root module not implemented yet

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun dorg-test-error--conditions (sym)
  "Return the `error-conditions' list recorded for condition SYM."
  (get sym 'error-conditions))

;;; ─── Root condition (exact shape, no extra parent, exact message) ─────────

(ert-deftest dorg-test-error-root-defined ()
  "The public root `dorg-error' has EXACTLY the `(dorg-error error)' chain.
Pinning the full list (not a `memq') excludes any extra parent or a
root/leaf cycle; the message is pinned to the designed string."
  (should (equal (dorg-test-error--conditions 'dorg-error)
                 '(dorg-error error)))                       ; RED: undefined -> nil
  (should (equal (get 'dorg-error 'error-message)
                 "dorg operation failed")))

(ert-deftest dorg-test-error-seam-install-is-shared-leaf ()
  "The shared DI refusal is defined in the edge-free error-root module.
Both the keywords and Dolt layers consume this leaf, so its exact ancestry and
source home prevent either module from inventing a parallel error or requiring
the other layer merely to obtain the condition symbol."
  (should (equal (dorg-test-error--conditions 'dorg-seam-install-error)
                 '(dorg-seam-install-error dorg-error error)))
  (should (equal (file-name-base (symbol-file 'dorg-seam-install-error))
                 "dorg-error")))

;;; ─── Leaves reparented under the root (EXACT condition lists) ─────────────

(ert-deftest dorg-test-error-api-parented ()
  "`dorg-api-error' has EXACTLY `(dorg-api-error dorg-error error)'.
Exact-list equality keeps the leaf, pins the single new parent, and
forbids an accidental second parent or a cycle."
  (should (equal (dorg-test-error--conditions 'dorg-api-error)
                 '(dorg-api-error dorg-error error))))       ; RED: no dorg-error yet

(ert-deftest dorg-test-error-mutate-parented ()
  "`dorg-mutate-error' has EXACTLY `(dorg-mutate-error dorg-error error)'."
  (should (equal (dorg-test-error--conditions 'dorg-mutate-error)
                 '(dorg-mutate-error dorg-error error))))    ; RED

(ert-deftest dorg-test-error-ql-parented ()
  "`dorg-ql-error' has EXACTLY `(dorg-ql-error dorg-error error)'."
  (should (equal (dorg-test-error--conditions 'dorg-ql-error)
                 '(dorg-ql-error dorg-error error))))        ; RED

;;; ─── One catch point for the whole taxonomy (live signals) ────────────────

(ert-deftest dorg-test-error-api-caught-as-root ()
  "A live `dorg-api-error' signal is caught by a `dorg-error' handler."
  (should (eq :caught
              (condition-case nil
                  (dorg-api--raise 'mode-error "boom")
                (dorg-error :caught)))))    ; RED: not a child, escapes handler

(ert-deftest dorg-test-error-mutate-caught-as-root ()
  "A live `dorg-mutate-error' signal is caught by a `dorg-error' handler."
  (should (eq :caught
              (condition-case nil
                  (dorg-mutate--raise 'invalid-op "boom")
                (dorg-error :caught)))))    ; RED

(ert-deftest dorg-test-error-ql-caught-as-root ()
  "A live `dorg-ql-error' signal is caught by a `dorg-error' handler."
  (should (eq :caught
              (condition-case nil
                  (dorg-ql--error "bad query")
                (dorg-error :caught)))))    ; RED

;;; ─── Exact live error-object shapes from the three real raisers ───────────

(ert-deftest dorg-test-error-live-shapes-pinned ()
  "Pin the EXACT condition objects raised by all three surface raisers.
Guards the (SYMBOL MESSAGE . DATA) contract — message plus plist order
and content — so no implementation may silently reshape a raised
condition while this suite stays green.  (Regression guard: GREEN pre-
and post-IMPL; reparenting does not alter the raised object.)"
  (let ((api (should-error (dorg-api--raise 'mode-error "m" :extra 7)))
        (mut (should-error (dorg-mutate--raise 'invalid-op "b")))
        (ql  (should-error (dorg-ql--error "bad"))))
    (should (equal api '(dorg-api-error "m" (:reason mode-error :extra 7))))
    (should (equal mut '(dorg-mutate-error "b" (:reason invalid-op))))
    (should (equal ql '(dorg-ql-error "bad" (:reason invalid-query))))))

;;; ─── One generic introspection accessor pair ──────────────────────────────

(ert-deftest dorg-test-error-data-reads-api-plist ()
  "`dorg-error-data' returns the EXACT structured plist of an API error."
  (let ((err (should-error (dorg-api--raise 'mode-error "m" :extra 7))))
    (should (equal (dorg-error-data err) '(:reason mode-error :extra 7)))  ; RED: void
    (should (eq 'mode-error (plist-get (dorg-error-data err) :reason)))
    (should (eq 7 (plist-get (dorg-error-data err) :extra)))))

(ert-deftest dorg-test-error-reason-spans-taxonomy ()
  "`dorg-error-reason' extracts the reason from api AND mutate errors alike."
  (let ((api (should-error (dorg-api--raise 'mode-error "a")))
        (mut (should-error (dorg-mutate--raise 'invalid-op "b"))))
    (should (eq 'mode-error (dorg-error-reason api)))     ; RED
    (should (eq 'invalid-op (dorg-error-reason mut)))))

(ert-deftest dorg-test-error-data-reads-ql-plist ()
  "`dorg-error-data' and `dorg-error-reason' read typed query data."
  (let ((err (should-error (dorg-ql--error "bad"))))
    (should (equal '(:reason invalid-query) (dorg-error-data err)))
    (should (eq 'invalid-query (dorg-error-reason err)))))

(ert-deftest dorg-test-error-data-nil-on-foreign ()
  "`dorg-error-data' returns nil for a non-dorg `error' object."
  (let ((err (should-error (error "plain"))))
    (should (null (dorg-error-data err)))))   ; RED: void-function

(ert-deftest dorg-test-error-data-guards-non-plist-slot ()
  "`dorg-error-data' returns nil when a rooted condition's data slot is NOT a plist.
`dorg-mutate-error' is rooted under `dorg-error' post-IMPL, but here the
third slot holds a bare symbol rather than a `:key value' plist.  A naive
`(nth 2 err)' implementation would return that symbol and fail this test;
the documented guard must reject the non-plist slot and return nil."
  (let ((err (list 'dorg-mutate-error "boom" 'not-a-plist)))
    ;; Precondition: the carrier symbol is genuinely rooted under `dorg-error'.
    (should (memq 'dorg-error (dorg-test-error--conditions 'dorg-mutate-error))) ; RED
    (should (null (dorg-error-data err)))     ; RED: void; post-impl must GUARD to nil
    (should (null (dorg-error-reason err)))))

;;; ─── Regression: typed-leaf accessors survive the delegation refactor ──────

(ert-deftest dorg-test-error-leaf-accessors-preserved ()
  "The per-symbol accessors keep exact behavior AND bidirectional isolation.
Both duplicated pairs are rewritten to delegate through the generic
accessor, so BOTH directions of cross-symbol isolation are pinned for
BOTH the data and reason accessors."
  (let ((api (should-error (dorg-api--raise 'mode-error "a" :extra 1)))
        (mut (should-error (dorg-mutate--raise 'invalid-op "b" :extra 2))))
    ;; Positive: each leaf reads its own condition.
    (should (eq 'mode-error (dorg-api-error-reason api)))
    (should (eq 1 (plist-get (dorg-api-error-data api) :extra)))
    (should (eq 'invalid-op (dorg-mutate-error-reason mut)))
    (should (eq 2 (plist-get (dorg-mutate-error-data mut) :extra)))
    ;; Cross-symbol isolation — DATA accessor, both directions.
    (should (null (dorg-mutate-error-data api)))
    (should (null (dorg-api-error-data mut)))
    ;; Cross-symbol isolation — REASON accessor, both directions.
    (should (null (dorg-mutate-error-reason api)))
    (should (null (dorg-api-error-reason mut)))))

(provide 'dorg-error-tests)
;;; dorg-error-tests.el ends here
