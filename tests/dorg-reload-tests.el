;;; dorg-reload-tests.el --- Tests for dorg-reload self-discovery + cache clear -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Behavioral coverage for the self-discovering, cache-clearing
;; `dorg-reload' (dorg.el).  These exercise the three decomposed
;; helpers directly so each acceptance criterion maps to a concrete,
;; non-vacuous test (set the WRONG state first, transform, assert the
;; corrected state — before != after):
;;
;;   `dorg--reload-discover'      — directory scan + featurep filter +
;;                                  dorg-app drop + foundational ordering
;;   `dorg--reload-clear-caches'  — invalidate derived caches, preserve
;;                                  live resources
;;   `dorg-schema-version' reload — the in-session v3->v4 schema heal
;;
;; No tracker / Dolt / sql-server / network dependency: discovery runs
;; against throwaway temp directories and the cache helpers operate on
;; module-level defvars.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)

;;; ─── Fixtures ─────────────────────────────────────────────────────────

(defconst dorg-test-reload--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

(defun dorg-test-reload--touch (dir name)
  "Create an empty file NAME under DIR and return its absolute path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert ";; throwaway\n"))
    path))

(defun dorg-test-reload--bases (paths)
  "Return the feature symbols (file-name-base) for PATHS."
  (mapcar (lambda (p) (intern (file-name-base p))) paths))

;;; ─── AC1: self-discovery, no hand-maintained list ─────────────────────

(ert-deftest dorg-test-reload-discovers-new-module-without-list ()
  "`dorg--reload-discover' picks up a newly-added loaded module with no list edit.
A throwaway `dorg-throwaway-ztest.el' placed in the scanned dir and
marked loaded (featurep) is returned, while an un-loaded sibling is
NOT — proving discovery is driven by the filesystem + featurep, not by
membership in any hand-maintained list."
  (let ((dir (make-temp-file "dorg-reload-discover" t))
        (loaded 'dorg-throwaway-ztest)
        (unloaded 'dorg-throwaway-notloaded))
    (unwind-protect
        (progn
          (dorg-test-reload--touch dir "dorg-throwaway-ztest.el")
          (dorg-test-reload--touch dir "dorg-throwaway-notloaded.el")
          ;; Mark only one as loaded — no list anywhere is edited.
          (push loaded features)
          (let ((found (dorg-test-reload--bases
                        (dorg--reload-discover dir))))
            (should (memq loaded found))
            (should-not (memq unloaded found))))
      (setq features (delq loaded features))
      (delete-directory dir t))))

;;; ─── AC2: dorg-app (CLI) never reloaded, even if loaded ───────────────

(ert-deftest dorg-test-reload-excludes-cli-app-even-if-loaded ()
  "`dorg--reload-discover' drops `dorg-app' even when its feature is present.
Proves the explicit CLI guard, not merely the featurep filter: a session
that has loaded the CLI app must never drag it into an interactive
reload."
  (let ((dir (make-temp-file "dorg-reload-app" t))
        (app 'dorg-app))
    (unwind-protect
        (progn
          (dorg-test-reload--touch dir "dorg-app.el")
          (dorg-test-reload--touch dir "dorg-id.el") ; a real loaded feature
          (push app features)              ; force the CLI app "loaded"
          (let ((found (dorg-test-reload--bases
                        (dorg--reload-discover dir))))
            (should-not (memq app found))
            ;; sanity: a genuinely-loaded core module IS discovered
            (should (memq 'dorg-id found))))
      (setq features (delq app features))
      (delete-directory dir t))))

;;; ─── AC4 (order): foundational modules float to the front ─────────────

(ert-deftest dorg-test-reload-priority-orders-foundational-first ()
  "Seed (foundational) modules sort ahead of non-seed modules in discovery.
`dorg--reload-priority' members must load before the discovery tail so
their macros/structs are fresh when dependents reload."
  (let ((dir (make-temp-file "dorg-reload-order" t)))
    (unwind-protect
        (progn
          ;; All four are already loaded by (require 'dorg); two are seed
          ;; members (id, schema), two are not (render, keywords).
          (dorg-test-reload--touch dir "dorg-render.el")
          (dorg-test-reload--touch dir "dorg-id.el")
          (dorg-test-reload--touch dir "dorg-keywords.el")
          (dorg-test-reload--touch dir "dorg-schema.el")
          (let* ((found (dorg-test-reload--bases
                         (dorg--reload-discover dir)))
                 (pos (lambda (s) (cl-position s found))))
            (should (funcall pos 'dorg-id))
            (should (funcall pos 'dorg-schema))
            (should (funcall pos 'dorg-render))
            (should (funcall pos 'dorg-keywords))
            ;; every seed member precedes every non-seed member
            (should (< (funcall pos 'dorg-id) (funcall pos 'dorg-render)))
            (should (< (funcall pos 'dorg-id) (funcall pos 'dorg-keywords)))
            (should (< (funcall pos 'dorg-schema) (funcall pos 'dorg-render)))
            (should (< (funcall pos 'dorg-schema)
                       (funcall pos 'dorg-keywords)))))
      (delete-directory dir t))))

;;; ─── AC3: runtime caches cleared ──────────────────────────────────────

(ert-deftest dorg-test-reload-clears-runtime-caches ()
  "`dorg--reload-clear-caches' resets every derived cache to its fresh value.
Sets each cache to a DIRTY non-fresh value, clears, and asserts the
fresh value — before != after."
  (let ((orig-registry dorg--registry)
        (orig-version dorg-dolt--version-cache))
    (unwind-protect
        (progn
          (setq dorg--registry '((dirty . t))
                dorg-dolt--version-cache '(:raw "9.9.9" :version (9 9 9)))
          (dorg--reload-clear-caches)
          (should (null dorg--registry))
          (should (eq dorg-dolt--version-cache 'unknown)))
      (setq dorg--registry orig-registry
            dorg-dolt--version-cache orig-version))))

;;; ─── (regression) live resources preserved across a cache clear ───────

(ert-deftest dorg-test-reload-preserves-live-state ()
  "`dorg--reload-clear-caches' leaves live resources / registrations intact.
Clearing the open mysql-connection table or the extension registry would
orphan processes / drop externally-registered extensions, so they MUST
survive a reload."
  (let ((orig-conns dorg-sql--connections)
        (orig-exts dorg--extensions))
    (unwind-protect
        (let ((conns (make-hash-table :test 'equal))
              (exts '((probe . placeholder))))
          (puthash "live-conn" 'process conns)
          (setq dorg-sql--connections conns
                dorg--extensions exts)
          (dorg--reload-clear-caches)
          (should (eq dorg-sql--connections conns))
          (should (eq (gethash "live-conn" dorg-sql--connections) 'process))
          (should (eq dorg--extensions exts)))
      (setq dorg-sql--connections orig-conns
            dorg--extensions orig-exts))))

(ert-deftest dorg-test-reload-protects-open-dolt-projection ()
  "Reload repairs read-only state on an already-open Dolt-bound Org buffer."
  (let ((buf (generate-new-buffer " *dorg-reload-projection*"))
        (noninteractive nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
            (org-mode)
            ;; Seed the wrong post-reload state non-vacuously.  This mirrors
            ;; the incident where re-evaluating the minor-mode definition left
            ;; an already-open tracker writable.
            (setq buffer-read-only nil)
            (setq dorg--owns-buffer-read-only nil)
            (should-not buffer-read-only))
          (cl-letf (((symbol-function 'dorg--reload-discover)
                     (lambda (_dir) nil))
                    ((symbol-function 'dorg--reload-clear-caches)
                     #'ignore))
            (let ((load-file-name (locate-library "dorg")))
              (dorg-reload)))
          (with-current-buffer buf
            (should buffer-read-only)
            (should dorg--owns-buffer-read-only)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; ─── AC5: in-session schema-version heal (v3 -> v4) ───────────────────

(ert-deftest dorg-test-reload-refreshes-schema-version-from-file ()
  "Re-loading dorg-schema.el heals a stale in-session `dorg-schema-version'.
This is the exact mechanism that lets a v3-session accept a v4 repo
without restarting Emacs: `dorg-schema-version' is a defconst, so the
reload load-step (load FILE nil t t) re-evaluates it back to the file's
value.  before != after."
  (let ((file (expand-file-name "dorg-schema.el" dorg-test-reload--root))
        (orig dorg-schema-version))
    (should (file-exists-p file))
    (unwind-protect
        (progn
          ;; Simulate a stale pre-bump session.
          (setq dorg-schema-version (1- orig))
          (should (= dorg-schema-version (1- orig)))
          ;; The exact load-step `dorg-reload' performs.
          (load file nil t t)
          (should (= dorg-schema-version orig))
          (should-not (= dorg-schema-version (1- orig))))
      (setq dorg-schema-version orig))))

(provide 'dorg-reload-tests)
;;; dorg-reload-tests.el ends here
