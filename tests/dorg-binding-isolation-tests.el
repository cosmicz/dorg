;;; dorg-binding-isolation-tests.el --- Scratch-tracker binding isolation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Regression tests: scratch DOrg/Dolt operations must not
;; write a `#+DORG_DOLT_DIR:' binding keyword into a non-scratch tracker file.
;;
;; `dorg-bind-dolt--insert-keyword' mutates whatever tracker file the current
;; buffer resolved to.  When the opt-in guard
;; `dorg--binding-keyword-write-guard' is armed to a scratch root, a write aimed
;; at a file OUTSIDE that root must fail loudly instead of contaminating it
;; (the live-tracker contamination outage).  These tests also pin the default-off behavior
;; (unrestricted writes) and prove the writers actually write, so the
;; protective assertions are not vacuous.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-backend)
(require 'dorg-registry)
(require 'dorg-test-helpers)

;;; ─── Local fixtures ───────────────────────────────────────────────────

(defmacro dorg-test--binding-iso-with-buffer (file content &rest body)
  "Run BODY in an `org-mode' temp buffer whose `buffer-file-name' is FILE.
CONTENT is the initial buffer text.  No disk save occurs; the writers
mutate the buffer, and the contamination of interest is a stray preamble
keyword, so buffer state is the faithful observable."
  (declare (indent 2) (debug (form form body)))
  `(with-temp-buffer
     (insert ,content)
     (org-mode)
     (setq buffer-file-name ,file)
     (set-buffer-modified-p nil)
     (goto-char (point-min))
     ,@body))

(defconst dorg-test--binding-iso-plain
  "#+TITLE: Live-like tracker\n#+EMBEAD_PREFIX: dorg\n\n* TODO A heading\n"
  "Minimal live-like plain-org tracker preamble — carries NO DORG keywords.")

;;; ─── Guard predicate ──────────────────────────────────────────────────

(ert-deftest dorg-test-binding-isolation-guard-predicate ()
  "`dorg--binding-write-allowed-p' honors the guard root, default-off."
  ;; Default off: every write is permitted.
  (let ((dorg--binding-keyword-write-guard nil))
    (should (dorg--binding-write-allowed-p "/anywhere/at/all/beads.org")))
  (let* ((root (file-name-as-directory (make-temp-file "dorg-guard-" t)))
         (inside (expand-file-name ".dorg/beads.org" root))
         (outside (expand-file-name "../elsewhere/beads.org" root)))
    (unwind-protect
        (let ((dorg--binding-keyword-write-guard root))
          (should (dorg--binding-write-allowed-p inside))
          (should-not (dorg--binding-write-allowed-p outside))
          ;; nil file: no on-disk tracker to contaminate -> permitted.
          (should (dorg--binding-write-allowed-p nil))
          ;; The signaling wrapper mirrors the predicate.
          (should-error (dorg--binding-write-check outside))
          (should (progn (dorg--binding-write-check inside) t)))
      (delete-directory root t))))

(ert-deftest dorg-test-binding-isolation-guard-accepts-list ()
  "A list of guard roots permits writes under ANY listed root."
  (let* ((root-a (file-name-as-directory (make-temp-file "dorg-ga-" t)))
         (root-b (file-name-as-directory (make-temp-file "dorg-gb-" t))))
    (unwind-protect
        (let ((dorg--binding-keyword-write-guard (list root-a root-b)))
          (should (dorg--binding-write-allowed-p
                   (expand-file-name "x/beads.org" root-a)))
          (should (dorg--binding-write-allowed-p
                   (expand-file-name "y/beads.org" root-b)))
          (should-not (dorg--binding-write-allowed-p "/tmp/other/beads.org")))
      (delete-directory root-a t)
      (delete-directory root-b t))))

;;; ─── Protective regression: live-like bystander stays clean ───────────

(ert-deftest dorg-test-binding-isolation-blocks-bystander-write ()
  "An armed guard blocks a binding write into an out-of-root tracker.
Models the contamination path: a scratch bind whose buffer resolved to the
live tracker.  With the guard armed to the scratch root, the keyword
writer must signal and leave the live-like buffer free of binding metadata."
  (let* ((scratch-root (file-name-as-directory (make-temp-file "dorg-scr-" t)))
         (live-root (file-name-as-directory (make-temp-file "dorg-live-" t)))
         (live-file (expand-file-name ".dorg/beads.org" live-root))
         (scratch-dolt (expand-file-name ".dorg/dolt/" scratch-root)))
    (unwind-protect
        (let ((dorg--binding-keyword-write-guard scratch-root))
          (dorg-test--binding-iso-with-buffer live-file dorg-test--binding-iso-plain
            ;; Scratch DOLT_DIR write aimed at the live-like file: blocked.
            (should-error (dorg-bind-dolt--insert-keyword scratch-dolt))
            ;; Live-like buffer preamble remains uncontaminated.
            (should-not (dorg--preamble-dolt-keyword))))
      (delete-directory scratch-root t)
      (delete-directory live-root t))))

;;; ─── Non-vacuity: the writer works when the guard permits it ───────────

(ert-deftest dorg-test-binding-isolation-allows-scratch-write ()
  "Under-root writes succeed, proving the protective test is non-vacuous.
If the writer were a no-op the blocked-bystander assertion above would
pass vacuously; here the same call, aimed at an in-root scratch file with
the guard armed, must actually insert the keyword."
  (let* ((scratch-root (file-name-as-directory (make-temp-file "dorg-scr-" t)))
         (scratch-file (expand-file-name ".dorg/beads.org" scratch-root))
         (some-dolt (expand-file-name ".dorg/dolt/" scratch-root)))
    (unwind-protect
        (let ((dorg--binding-keyword-write-guard scratch-root))
          (dorg-test--binding-iso-with-buffer scratch-file dorg-test--binding-iso-plain
            (dorg-bind-dolt--insert-keyword some-dolt)
            (should (dorg--preamble-dolt-keyword))))
      (delete-directory scratch-root t))))

(ert-deftest dorg-test-binding-isolation-disarmed-writes-unchanged ()
  "With the guard nil (default), writes are unrestricted — behavior unchanged.
Pins that layer-C is opt-in and does not alter production binding."
  (let* ((root (file-name-as-directory (make-temp-file "dorg-off-" t)))
         (file (expand-file-name ".dorg/beads.org" root))
         (dolt (expand-file-name ".dorg/dolt/" root)))
    (unwind-protect
        (let ((dorg--binding-keyword-write-guard nil))
          (dorg-test--binding-iso-with-buffer file dorg-test--binding-iso-plain
            (dorg-bind-dolt--insert-keyword dolt)
            (should (dorg--preamble-dolt-keyword))))
      (delete-directory root t))))

;;; ─── Isolated-tracker helper ──────────────────────────────────────────

(ert-deftest dorg-test-binding-isolation-helper-isolates-and-restores ()
  "`dorg-test--with-isolated-tracker' arms the guard, isolates env, restores."
  (let ((prior-file (getenv "EMBEAD_FILE"))
        (captured-guard nil)
        (captured-env nil))
    (dorg-test--with-isolated-tracker root
      (setq captured-guard dorg--binding-keyword-write-guard)
      (setq captured-env (getenv "EMBEAD_FILE"))
      ;; Inside: guard armed to the temp root; EMBEAD_FILE points under it.
      (should (equal dorg--binding-keyword-write-guard root))
      (should (string-prefix-p (file-truename root)
                               (file-truename (getenv "EMBEAD_FILE")))))
    ;; Outside: env restored, guard back to its default (nil here).
    (should captured-guard)
    (should captured-env)
    (should (equal (getenv "EMBEAD_FILE") prior-file))
    (should-not dorg--binding-keyword-write-guard)))

(provide 'dorg-binding-isolation-tests)
;;; dorg-binding-isolation-tests.el ends here
