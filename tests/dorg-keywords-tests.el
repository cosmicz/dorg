;;; dorg-keywords-tests.el --- Tests for dorg-keywords -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the root keyword read primitive (`dorg-root-keywords' /
;; `dorg-root-keyword-values').  Covers absent / single / repeated
;; keywords and document-order preservation for both plain-org sources
;; (buffer, file) and the Dolt root-only load source.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-keywords)
(require 'dorg-parse)
(require 'dorg-sql-gen)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)       ;; defined in dorg-dolt-tests.el

(defconst dorg-test-keywords--preamble
  (concat "#+TITLE: Demo\n"
          "#+EMBEAD_PREFIX: proj\n"
          "#+EMBEAD_TAG: alpha\n"
          "#+EMBEAD_TAG: beta\n"
          "\n"
          "* Heading\n"
          "body\n")
  "Preamble fixture: one TITLE, one single-valued and one repeated key.")

;;; ─── Org buffer source ──────────────────────────────────────────────

(ert-deftest dorg-test-keywords-buffer-all-pairs-in-order ()
  "`dorg-root-keywords' returns every preamble keyword in document order."
  (with-temp-buffer
    (insert dorg-test-keywords--preamble)
    (org-mode)
    (should (equal '(("TITLE" . "Demo")
                     ("EMBEAD_PREFIX" . "proj")
                     ("EMBEAD_TAG" . "alpha")
                     ("EMBEAD_TAG" . "beta"))
                   (dorg-root-keywords `(:buffer ,(current-buffer)))))))

(ert-deftest dorg-test-keywords-buffer-values-cardinality ()
  "`dorg-root-keyword-values' shapes absent/single/repeated correctly."
  (with-temp-buffer
    (insert dorg-test-keywords--preamble)
    (org-mode)
    (let ((src `(:buffer ,(current-buffer))))
      ;; absent -> nil
      (should (null (dorg-root-keyword-values src "EMBEAD_NOPE")))
      ;; single -> one-element list
      (should (equal '("proj") (dorg-root-keyword-values src "EMBEAD_PREFIX")))
      ;; repeated -> ordered list
      (should (equal '("alpha" "beta")
                     (dorg-root-keyword-values src "EMBEAD_TAG"))))))

(ert-deftest dorg-test-keywords-buffer-current-buffer-form ()
  "The (:buffer) form (no explicit buffer) reads the current buffer."
  (with-temp-buffer
    (insert dorg-test-keywords--preamble)
    (org-mode)
    (should (equal '("proj") (dorg-root-keyword-values '(:buffer) "EMBEAD_PREFIX")))))

(ert-deftest dorg-test-keywords-empty-preamble-nil ()
  "A document with no preamble keywords yields nil."
  (with-temp-buffer
    (insert "* Just a heading\nbody\n")
    (org-mode)
    (should (null (dorg-root-keywords `(:buffer ,(current-buffer)))))))

;;; ─── Org file source ────────────────────────────────────────────────

(ert-deftest dorg-test-keywords-file-source ()
  "The (:file PATH) form parses a plain org file from disk."
  (let ((path (make-temp-file "dorg-kw-" nil ".org"
                              dorg-test-keywords--preamble)))
    (unwind-protect
        (let ((src `(:file ,path)))
          (should (equal '("proj") (dorg-root-keyword-values src "EMBEAD_PREFIX")))
          (should (equal '("alpha" "beta")
                         (dorg-root-keyword-values src "EMBEAD_TAG"))))
      (delete-file path))))

;;; ─── Dolt source (root-only load) ───────────────────────────────────

(ert-deftest dorg-test-keywords-dolt-source-cardinality ()
  "Dolt root-only load returns the same keyword cardinality as org."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch dolt-dir
                    (dorg-test--parse-to-sql dorg-test-keywords--preamble 1)
                    "seed keywords")
    (let ((src `(:dolt-dir ,dolt-dir :root-id 1)))
      (should (null (dorg-root-keyword-values src "EMBEAD_NOPE")))
      (should (equal '("proj") (dorg-root-keyword-values src "EMBEAD_PREFIX")))
      (should (equal '("alpha" "beta")
                     (dorg-root-keyword-values src "EMBEAD_TAG"))))))

(ert-deftest dorg-test-keywords-dolt-source-order ()
  "Dolt root-only load preserves document order across keyword names."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch dolt-dir
                    (dorg-test--parse-to-sql dorg-test-keywords--preamble 1)
                    "seed keywords")
    (should (equal '(("TITLE" . "Demo")
                     ("EMBEAD_PREFIX" . "proj")
                     ("EMBEAD_TAG" . "alpha")
                     ("EMBEAD_TAG" . "beta"))
                   (dorg-root-keywords `(:dolt-dir ,dolt-dir :root-id 1))))))

(ert-deftest dorg-test-keywords-dolt-unknown-root-nil ()
  "An unknown ROOT-ID yields nil rather than erroring."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql-batch dolt-dir
                    (dorg-test--parse-to-sql dorg-test-keywords--preamble 1)
                    "seed keywords")
    (should (null (dorg-root-keywords `(:dolt-dir ,dolt-dir :root-id 999))))))

;;; ─── Honest boundary: keyword primitive stays transport-free (tracked-case) ─

(ert-deftest dorg-test-keywords-loads-without-transport ()
  "`(require \\='dorg-keywords)' loads neither the transport nor mysql.
The root keyword primitive is a core read path: its :file / :buffer
branches are pure org parsing and its :dolt-dir branch dispatches through
a registered loader seam, so requiring it must not force the hard mysql
load.  Runs a fresh `emacs -Q' with only the repo root on the load path."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-keywords)
             (unless (fboundp 'dorg-root-keywords)
               (error \"dorg-root-keywords missing after require\"))
             (when (featurep 'dorg-dolt)
               (error \"dorg-dolt leaked into the dorg-keywords read primitive\"))
             (when (featurep 'mysql)
               (error \"mysql leaked into the dorg-keywords read primitive\")))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "--eval" expr)))))

(ert-deftest dorg-test-keywords-dolt-dispatch-uses-loader-seam ()
  "A :dolt-dir read dispatches THROUGH the `dorg-root--dolt-loader' seam.
The inversion means the branch never reaches directly into the transport:
it funcalls the registered loader with (DIR ROOT-ID) and returns its value.
Binding a spy proves the dispatch is invoked via the seam."
  (let* ((seen nil)
         (sentinel (make-dorg-heading :id 7 :level 0 :title "root"))
         (dorg-root--dolt-loader
          (lambda (dir id) (setq seen (list dir id)) sentinel)))
    (should (eq sentinel
                (dorg-root--load-root-heading '(:dolt-dir "/d" :root-id 7))))
    (should (equal seen '("/d" 7)))))

(ert-deftest dorg-test-keywords-provider-registers-loader ()
  "Loading the Dolt provider self-registers the :dolt-dir loader seam.
Default-registration half of the composition contract: `(require
\\='dorg-keywords)' alone leaves the seam nil (load-time purity — no static
Dolt edge); loading `dorg-model-load' installs a `functionp' loader.  A
fresh `emacs -Q' isolates the boundary from the full-image load order."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (expr
          "(progn
             (require 'dorg-keywords)
             (when (and (boundp 'dorg-root--dolt-loader) dorg-root--dolt-loader)
               (error \"loader must be nil after loading dorg-keywords alone\"))
             (require 'dorg-model-load)
             (unless (functionp dorg-root--dolt-loader)
               (error \"loading dorg-model-load must register the :dolt-dir loader\")))"))
    (should (= 0 (call-process emacs nil nil nil
                               "--batch" "-Q"
                               "-L" root
                               "-L" (expand-file-name "lib/mysql" root)
                               "--eval" expr)))))

(ert-deftest dorg-test-keywords-loader-install-is-owner-guarded ()
  "Only `dorg-model-load' may install the Dolt loader seam.
A foreign first registration leaves the nil preimage unchanged.  The owner's
same-function reinstall is an idempotent no-op; owner replacement with a
different function and every foreign attempt signal the typed install error
and preserve the exact installed function."
  (let ((dorg-root--dolt-loader nil)
        (loader-1 (lambda (&rest _) 'loader-1))
        (loader-2 (lambda (&rest _) 'loader-2)))
    (should-error
     (dorg-root--install-dolt-loader 'foreign-provider loader-1)
     :type 'dorg-seam-install-error)
    (should-not dorg-root--dolt-loader)

    (dorg-root--install-dolt-loader 'dorg-model-load loader-1)
    (should (eq loader-1 dorg-root--dolt-loader))

    (dorg-root--install-dolt-loader 'dorg-model-load loader-1)
    (should (eq loader-1 dorg-root--dolt-loader))

    (should-error
     (dorg-root--install-dolt-loader 'dorg-model-load loader-2)
     :type 'dorg-seam-install-error)
    (should (eq loader-1 dorg-root--dolt-loader))

    (should-error
     (dorg-root--install-dolt-loader 'foreign-provider loader-2)
     :type 'dorg-seam-install-error)
    (should (eq loader-1 dorg-root--dolt-loader))))

(ert-deftest dorg-test-keywords-dolt-dispatch-lazy-loads-provider ()
  "The :dolt-dir dispatch ITSELF lazy-loads the provider when the seam is nil.
Joins the two halves the other keyword tests exercise separately: begin
with `dorg-keywords' loaded, the loader seam nil, and the provider feature
genuinely absent; a single `(:dolt-dir …)' read must trigger the dispatch's
own runtime `(require \\='dorg-model-load nil t)', whose load registers the
loader, and then return that loader's result.

A controlled shadow `dorg-model-load' (a temp dir placed FIRST on the load
path) stands in for the real provider so the composition is exercised
Dolt-free.  Non-vacuity: if the dispatch-site lazy require is removed the
seam stays nil, the dispatch never composes the provider, and the read
fails — this test goes RED.  Runs a fresh `emacs -Q' to isolate the load
order from the full-suite image (which preloads the provider via dorg.el)."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file default-directory "dorg-dolt.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (provider-dir (file-name-as-directory (make-temp-file "dorg-fake-ml" t)))
         (provider-file (expand-file-name "dorg-model-load.el" provider-dir)))
    (unwind-protect
        (progn
          ;; Controlled provider: loading it registers a sentinel-returning
          ;; loader on the keyword seam, exactly as the real bridge will.
          (with-temp-file provider-file
            (insert ";;; dorg-model-load.el --- controlled provider stub  -*- lexical-binding: t; -*-\n"
                    "(setq dorg-root--dolt-loader\n"
                    "      (lambda (dir id) (list 'fake-loader-sentinel dir id)))\n"
                    "(provide 'dorg-model-load)\n"))
          (let ((expr
                 "(progn
                    (require 'dorg-keywords)
                    (when (featurep 'dorg-model-load)
                      (error \"provider must be unloaded after requiring dorg-keywords alone\"))
                    (when (and (boundp 'dorg-root--dolt-loader) dorg-root--dolt-loader)
                      (error \"loader seam must be nil before the dispatch runs\"))
                    (let ((result (dorg-root--load-root-heading
                                   '(:dolt-dir \"/spec-dir\" :root-id 42))))
                      (unless (featurep 'dorg-model-load)
                        (error \"dispatch failed to lazy-load the provider\"))
                      (unless (equal result '(fake-loader-sentinel \"/spec-dir\" 42))
                        (error \"dispatch did not reach the registered loader: %S\" result))))"))
            (should (= 0 (call-process emacs nil nil nil
                                       "--batch" "-Q"
                                       "-L" provider-dir ; shadows the real provider
                                       "-L" root
                                       "-L" (expand-file-name "lib/mysql" root)
                                       "--eval" expr)))))
      (delete-directory provider-dir t))))

;;; ─── Error path ─────────────────────────────────────────────────────

(ert-deftest dorg-test-keywords-unrecognized-source-errors ()
  "An unrecognized SOURCE form signals an error."
  (should-error (dorg-root-keywords '(:bogus "x"))))

(provide 'dorg-keywords-tests)
;;; dorg-keywords-tests.el ends here
