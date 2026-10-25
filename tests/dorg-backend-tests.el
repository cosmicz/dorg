;;; dorg-backend-tests.el --- Tests for dorg backend abstraction -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the storage backend abstraction, the implicit plain-org
;; default (no registration needed), plain-org materialize/sync
;; dispatch, and DORG_ID assignment.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-backend)
(require 'dorg-registry)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Backend Struct ───────────────────────────────────────────────────

(ert-deftest dorg-test-backend-plain-org-kind ()
  "Plain-org backend reports its storage kind."
  (let ((b (dorg-backend-plain-org)))
    (should (eq (dorg-backend-type b) 'plain-org))
    (should (dorg-backend-plain-org-p b))
    (should-not (dorg-backend-dolt-p b))))

(ert-deftest dorg-test-backend-dolt-kind ()
  "Dolt backend reports its storage kind."
  (let ((b (dorg-backend-dolt "/tmp/db/")))
    (should (eq (dorg-backend-type b) 'dolt))
    (should (dorg-backend-dolt-p b))
    (should-not (dorg-backend-plain-org-p b))))

(ert-deftest dorg-test-backend-dolt-dir-is-expanded ()
  "`dorg-backend-dolt-dir' expands and trailing-slashes the path."
  (let ((b (dorg-backend-dolt "/tmp/some/db")))
    (should (string-suffix-p "/" (dorg-backend-dolt-dir b)))
    (should (file-name-absolute-p (dorg-backend-dolt-dir b)))))

(ert-deftest dorg-test-backend-dolt-dir-nil-for-plain ()
  "`dorg-backend-dolt-dir' is nil for the plain-org backend."
  (should-not (dorg-backend-dolt-dir (dorg-backend-plain-org))))

(ert-deftest dorg-test-backend-dolt-root-id-positional ()
  "`dorg-backend-dolt' stores an optional positional ROOT-ID in options."
  (let ((with-root (dorg-backend-dolt "/tmp/db/" 4242))
        (without-root (dorg-backend-dolt "/tmp/db/")))
    (should (= 4242 (dorg-backend-root-id with-root)))
    ;; ROOT-ID is optional — omitting it leaves the option nil.
    (should-not (dorg-backend-root-id without-root))))

(ert-deftest dorg-test-backend-root-id-nil-for-plain ()
  "`dorg-backend-root-id' is nil for the plain-org backend."
  (should-not (dorg-backend-root-id (dorg-backend-plain-org))))

;;; ─── Implicit Plain-Org Default (no registration, no Dolt) ───────────

(ert-deftest dorg-test-batch-source-open-skips-large-file-consultation ()
  "A DOrg-owned batch open suppresses only the large-file consultation.
The caller's threshold remains unchanged after the dynamically scoped open."
  (let* ((file (make-temp-file "dorg-batch-large-open-" nil ".org"
                               "* Batch source\n"))
         (large-file-warning-threshold 1)
         (noninteractive t)
         (consulted nil)
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'files--ask-user-about-large-file)
                   (lambda (&rest _)
                     (setq consulted t)
                     (error "large-file consultation reached"))))
          (setq buffer (dorg--find-file-noselect file))
          (should (buffer-live-p buffer))
          (should-not consulted)
          (should (= 1 large-file-warning-threshold)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest dorg-test-interactive-source-open-preserves-large-file-warning ()
  "A dynamically interactive DOrg open preserves Emacs's confirmation path."
  (let* ((file (make-temp-file "dorg-interactive-large-open-" nil ".org"
                               "* Interactive source\n"))
         (large-file-warning-threshold 1)
         (noninteractive nil)
         (consultations 0)
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'files--ask-user-about-large-file)
                   (lambda (&rest _)
                     (setq consultations (1+ consultations))
                     nil)))
          (setq buffer (dorg--find-file-noselect file))
          (should (buffer-live-p buffer))
          (should (= 1 consultations))
          (should (= 1 large-file-warning-threshold)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest dorg-test-dorg-register-symbol-removed ()
  "`dorg-register' no longer exists as a symbol (renamed to `dorg-bind-dolt')."
  (should-not (fboundp 'dorg-register))
  (should (fboundp 'dorg-bind-dolt)))

(ert-deftest dorg-test-plain-org-is-implicit-default ()
  "A file buffer with no Dolt keyword resolves to plain-org with no registry entry."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-plain-impl-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* H\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (let ((reg (dorg--classify-registration)))
              (should (dorg-registration-p reg))
              (should (eq (dorg-backend-type
                           (dorg-registration-backend reg))
                          'plain-org))
              (should-not (dorg-backend-dolt-dir (dorg-registration-backend reg)))
              (should-not (dorg-backend-root-id (dorg-registration-backend reg)))
              (should (equal (dorg-registration-file reg)
                             (file-truename org-file))))
            ;; The transient plain-org reg is NOT stored, and no .dolt
            ;; directory is created.
            (should-not (assoc (file-truename org-file) dorg--registry))
            (should-not (file-exists-p (expand-file-name ".dolt" temp-base)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-registered-backend-plain-org-for-unbound ()
  "`dorg-registered-backend' returns a fresh plain-org descriptor when unbound."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-unbound-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* H\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (should (dorg-backend-plain-org-p (dorg-registered-backend)))
            (should-not (dorg-registered-dolt-dir))
            (should-not (dorg-backend-root-id (dorg-registered-backend)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-plain-org-without-dolt-binary ()
  "Plain-org resolution succeeds even if dolt is unavailable."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-no-dolt-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-dolt-available-p)
                   (lambda () nil)))
          (let ((org-file (expand-file-name "doc.org" temp-base)))
            (with-temp-file org-file (insert "* H\n"))
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (set-buffer-modified-p nil)
              (should (eq (dorg-backend-type
                           (dorg-registration-backend
                            (dorg--classify-registration)))
                          'plain-org)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-bind-dolt-creates-dolt-backend ()
  "`dorg-bind-dolt' binds a file to a Dolt backend carrying dir + root id.
The explicit binding is the only thing stored in `dorg--registry'."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((dorg--registry nil)
           (temp-base (make-temp-file "dorg-dolt-bind-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (org-file (expand-file-name "doc.org" temp-base)))
      (unwind-protect
          (let ((reg (dorg-bind-dolt org-file dolt-dir)))
            (should (eq (dorg-backend-type
                         (dorg-registration-backend reg))
                        'dolt))
            (should (dorg-backend-dolt-dir (dorg-registration-backend reg)))
            (should (integerp (dorg-backend-root-id
                               (dorg-registration-backend reg))))
            (should (assoc (file-truename org-file) dorg--registry)))
        (delete-directory temp-base t)))))

;;; ─── Public Full-Registration Accessor ───────────────────────────────

(ert-deftest dorg-test-current-registration-plain-org-unbound ()
  "`dorg-current-registration' returns a plain-org reg for an unbound buffer.
The transient registration is not stored in `dorg--registry'."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-plain-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* H\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (let ((reg (dorg-current-registration)))
              (should (dorg-registration-p reg))
              (should (dorg-backend-plain-org-p
                       (dorg-registration-backend reg)))
              (should (equal (dorg-registration-file reg)
                             (file-truename org-file))))
            ;; Read-only: nothing persisted into the registry.
            (should-not (assoc (file-truename org-file) dorg--registry))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-explicit-dolt ()
  "`dorg-current-registration' returns the stored Dolt reg after binding."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((dorg--registry nil)
           (temp-base (make-temp-file "dorg-curreg-dolt-" t))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (org-file (expand-file-name "doc.org" temp-base)))
      (unwind-protect
          (progn
            (dorg-bind-dolt org-file dolt-dir)
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (set-buffer-modified-p nil)
              (let ((reg (dorg-current-registration)))
                (should (dorg-registration-p reg))
                (should (eq (dorg-backend-type
                             (dorg-registration-backend reg))
                            'dolt))
                (should (dorg-backend-dolt-dir
                         (dorg-registration-backend reg)))
                (should (integerp (dorg-backend-root-id
                                   (dorg-registration-backend reg)))))))
        (delete-directory temp-base t)))))

(ert-deftest dorg-test-current-registration-no-file-buffer ()
  "`dorg-current-registration' returns nil for a buffer with no file."
  (let ((dorg--registry nil))
    (with-temp-buffer
      (should-not (dorg-current-registration)))))

(ert-deftest dorg-test-current-registration-file-source-is-pure-and-nonvisiting ()
  "A file string classifies its preamble without binding or visiting it."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-file-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n* Heading\n"))
          (cl-letf (((symbol-function 'dorg-bind-dolt)
                     (lambda (&rest _) (ert-fail "file classification bound")))
                    ((symbol-function 'dorg--ensure-registration)
                     (lambda (&rest _) (ert-fail "file classification ensured"))))
            (let ((reg (dorg-current-registration org-file)))
              (should (dorg-registration-p reg))
              (should (dorg-backend-dolt-p (dorg-registration-backend reg)))
              (should-not (dorg-backend-root-id (dorg-registration-backend reg)))
              (should (equal (file-truename org-file)
                             (dorg-registration-file reg)))
              (should-not (find-buffer-visiting org-file))
              (should-not dorg--registry))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-prefers-modified-visiting-buffer ()
  "A file string returns a visiting buffer's registration without ensuring it."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-visiting-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file (insert "* Disk plain\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (insert "#+DORG_DOLT_DIR: store\n* Modified\n")
            (set-buffer-modified-p t)
            (cl-letf (((symbol-function 'dorg-bind-dolt)
                       (lambda (&rest _) (ert-fail "visiting file bound")))
                      ((symbol-function 'dorg--ensure-registration)
                       (lambda (&rest _) (ert-fail "visiting file ensured"))))
              (let ((reg (dorg-current-registration org-file)))
                (should (dorg-registration-p reg))
                (should (dorg-backend-dolt-p (dorg-registration-backend reg)))
                (should-not (dorg-backend-root-id (dorg-registration-backend reg)))
                (should-not dorg--registry)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-ignores-block-marker ()
  "A DOrg keyword in a source block cannot bind a file string."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-block-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+begin_src text\n#+DORG_DOLT_DIR: store\n#+end_src\n* Heading\n"))
          (let ((reg (dorg-current-registration org-file)))
            (should (dorg-registration-p reg))
            (should (dorg-backend-plain-org-p (dorg-registration-backend reg)))
            (should-not (find-buffer-visiting org-file))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-ignores-example-and-body-markers ()
  "Example and body literals cannot become file-source binding declarations."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-example-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file
            (insert "#+begin_example\n#+DORG_DOLT_DIR: ignored-example\n#+end_example\n"
                    "* Heading\n#+DORG_DOLT_DIR: ignored-body\n"))
          (let ((reg (dorg-current-registration org-file)))
            (should (dorg-backend-plain-org-p (dorg-registration-backend reg)))
            (should-not (find-buffer-visiting org-file))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-missing-signals-file-error ()
  "A relative missing file has an explicit ordinary file failure."
  (let ((temp-base (make-temp-file "dorg-curreg-missing-" t)))
    (unwind-protect
        (let ((default-directory temp-base))
          (should-error (dorg-current-registration "missing.org")
                        :type 'file-missing))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-unreadable-preserves-file-error ()
  "A present but unreadable file is not misreported as missing."
  (let ((temp-base (make-temp-file "dorg-curreg-unreadable-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* Present\n"))
          (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
            (let ((err (should-error (dorg-current-registration org-file)
                                     :type 'file-error)))
              (should-not (eq 'file-missing (car err))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-long-preamble-is-equivalent ()
  "A long preamble retains its genuine leading Dolt keyword."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-long-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n")
            (dotimes (_ 2100) (insert "#+AUTHOR: preamble filler\n"))
            (insert "* Heading\n"))
          (let ((file-reg (dorg-current-registration org-file))
                (buffer-reg (with-temp-buffer
                              (set-visited-file-name org-file t)
                              (insert-file-contents org-file)
                              (org-mode)
                              (dorg-current-registration))))
            (should (dorg-backend-dolt-p (dorg-registration-backend file-reg)))
            (should (equal (dorg-backend-dolt-dir (dorg-registration-backend file-reg))
                           (dorg-backend-dolt-dir (dorg-registration-backend buffer-reg))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-multibyte-preamble-is-equivalent ()
  "One byte-zero decode preserves a multibyte preamble exactly as a visit does."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-utf8-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n")
            ;; Crosses the old 8 KiB chunk boundary in a multibyte character.
            (insert "#+DESCRIPTION: " (make-string 8200 ?x) "\u2603\n* Heading\n"))
          (let ((file-reg (dorg-current-registration org-file))
                (buffer-reg (with-temp-buffer
                              (set-visited-file-name org-file t)
                              (insert-file-contents org-file)
                              (org-mode)
                              (dorg-current-registration))))
            (should (dorg-backend-dolt-p (dorg-registration-backend file-reg)))
            (should (equal (dorg-backend-dolt-dir (dorg-registration-backend file-reg))
                           (dorg-backend-dolt-dir (dorg-registration-backend buffer-reg))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-inlinetask-preamble-terminates ()
  "An inline task before the first ordinary heading cannot stall classification."
  (require 'org-inlinetask)
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-inline-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n*************** Inline\n*************** END\n* Heading\n"))
          (let ((reg (dorg-current-registration org-file)))
            (should (dorg-backend-dolt-p (dorg-registration-backend reg)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-legacy-and-literals-match-buffer ()
  "Legacy, absolute, example, and body literals retain canonical semantics."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-literals-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert (format "#+DORG_DIR: %s\n" dolt-dir)
                    "#+begin_example\n#+DORG_DOLT_DIR: ignored-example\n#+end_example\n"
                    "* Heading\n#+DORG_DOLT_DIR: ignored-body\n"))
          (let ((file-reg (dorg-current-registration org-file))
                (buffer-reg (with-temp-buffer
                              (set-visited-file-name org-file t)
                              (insert-file-contents org-file)
                              (org-mode)
                              (dorg-current-registration))))
            (should (dorg-backend-dolt-p (dorg-registration-backend file-reg)))
            (should (equal (file-name-as-directory (file-truename dolt-dir))
                           (dorg-backend-dolt-dir (dorg-registration-backend file-reg))))
            (should (equal (dorg-backend-dolt-dir (dorg-registration-backend file-reg))
                           (dorg-backend-dolt-dir (dorg-registration-backend buffer-reg))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-prefers-deleted-visiting-buffer ()
  "A visiting buffer remains authoritative after its backing file disappears."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-curreg-deleted-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base)))
          (make-directory dolt-dir)
          (with-temp-file org-file (insert "* Disk plain\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (insert "#+DORG_DOLT_DIR: store\n* Unsaved\n")
            (delete-file org-file)
            (cl-letf (((symbol-function 'dorg-bind-dolt)
                       (lambda (&rest _) (ert-fail "visiting file bound")))
                      ((symbol-function 'dorg--ensure-registration)
                       (lambda (&rest _) (ert-fail "visiting file ensured"))))
              (should (dorg-backend-dolt-p
                       (dorg-registration-backend (dorg-current-registration org-file)))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-cleans-scratch-on-error ()
  "An error while creating the reader cannot retain the owned preamble buffer."
  (let ((temp-base (make-temp-file "dorg-curreg-cleanup-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* Heading\n"))
          (cl-letf (((symbol-function 'org-mode)
                     (lambda (&rest _) (error "fixture reader error"))))
            (should-error (dorg-current-registration org-file))
            (should-not
             (catch 'found
               (dolist (buffer (buffer-list))
                 (when (string-prefix-p " *dorg-preamble*" (buffer-name buffer))
                   (throw 'found t)))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-reader-delays-major-mode-hooks ()
  "A private reader does not activate generic major-mode hooks."
  (let ((temp-base (make-temp-file "dorg-curreg-hooks-" t))
        (major-mode-ran nil))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base))
              (after-change-major-mode-hook
               (list (lambda () (setq major-mode-ran t)))))
          (with-temp-file org-file (insert "* Plain\n"))
          (dorg-current-registration org-file)
          (should-not major-mode-ran))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-enters-org-mode-empty ()
  "The reader initializes Org before it owns any projection bytes."
  (let ((temp-base (make-temp-file "dorg-curreg-empty-mode-" t))
        sizes)
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base))
              (original-org-mode (symbol-function 'org-mode)))
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n" (make-string 50000 ?x) "\n* Heading\n"))
          (cl-letf (((symbol-function 'org-mode)
                     (lambda (&rest args)
                       (push (buffer-size) sizes)
                       (apply original-org-mode args))))
            (should (dorg-current-registration org-file))
            ;; The insert-before-org-mode mutant observes the full body here.
            (should (equal '(0) sizes))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-current-registration-file-does-not-read-setupfile ()
  "A source SETUPFILE is inert while the empty reader enters Org mode."
  (let ((temp-base (make-temp-file "dorg-curreg-setupfile-" t))
        reads)
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base))
              (original-insert-file-contents (symbol-function 'insert-file-contents)))
          (with-temp-file org-file
            (insert "#+SETUPFILE: missing-setup.org\n#+DORG_DOLT_DIR: store\n* Heading\n"))
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (file &rest args)
                       (push (expand-file-name file) reads)
                       (apply original-insert-file-contents file args))))
            (should (dorg-current-registration org-file))
            (should (equal (list (file-truename org-file))
                           (mapcar #'file-truename reads)))))
      (delete-directory temp-base t))))

;;; ─── Pure Classification vs. Strict Ensure (tracked-regression) ────────────
;;
;; The registry must separate PURE source classification from EFFECTFUL
;; binding.  Read-only accessors classify a buffer's source (plain-org vs.
;; keyword-declared Dolt) without ever initializing a repo, migrating a
;; schema, or allocating a root row.  Only the explicit strict resolver
;; (`dorg--ensure-registration', reached from operations that need a real
;; backing store) may bind via `dorg-bind-dolt'.

(defmacro dorg-test--recording-registry-effects (var &rest body)
  "Run BODY with all effectful registry/repo primitives stubbed to record.
VAR is bound to a list (most-recent-first) of symbols naming each effect
primitive that fired.  `dorg-bind-dolt', `dorg-init',
`dorg-repo-ensure-schema', `dorg-repo-root-id', and `dorg--create-root'
are replaced with recorders that do NO real work, so BODY may classify a
keyword-declared Dolt buffer with no live Dolt repo present.  A pure path
leaves VAR empty."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var '()))
     (cl-letf (((symbol-function 'dorg-bind-dolt)
                (lambda (&rest _) (push 'dorg-bind-dolt ,var) nil))
               ((symbol-function 'dorg-init)
                (lambda (&rest _) (push 'dorg-init ,var) nil))
               ((symbol-function 'dorg-repo-ensure-schema)
                (lambda (&rest _) (push 'dorg-repo-ensure-schema ,var) nil))
               ((symbol-function 'dorg-repo-root-id)
                (lambda (&rest _) (push 'dorg-repo-root-id ,var) nil))
               ((symbol-function 'dorg--create-root)
                (lambda (&rest _) (push 'dorg--create-root ,var) 4242)))
       ,@body)))

(defmacro dorg-test--with-keyword-dolt-buffer (spec &rest body)
  "Evaluate BODY in a file-visiting buffer whose preamble declares a Dolt dir.
SPEC is (FILE-VAR DIR-VAR): FILE-VAR is bound to the visited file's truename
and DIR-VAR to the declared (never-created) Dolt directory.  `dorg--registry'
is rebound to nil for isolation.  The temp tree is removed afterward.  The
Dolt directory intentionally does not exist on disk: classification must not
require it."
  (declare (indent 1) (debug ((symbolp symbolp) body)))
  (let ((file-var (nth 0 spec))
        (dir-var (nth 1 spec)))
    `(let* ((dorg--registry nil)
            (temp-base (make-temp-file "dorg-awzwf-kw-" t))
            (,dir-var (file-name-as-directory (expand-file-name "db" temp-base)))
            (,file-var (file-truename (expand-file-name "doc.org" temp-base))))
       (unwind-protect
           (with-temp-buffer
             (insert "#+DORG_DOLT_DIR: ./db\n* H\n")
             (set-visited-file-name ,file-var t)
             (set-buffer-modified-p nil)
             ,@body)
         (delete-directory temp-base t)))))

(ert-deftest dorg-test-classify-keyword-dolt-is-pure ()
  "`dorg--classify-registration' classifies a keyword Dolt buffer purely.
It returns a Dolt registration carrying the declared dir and a nil root-id,
without firing any effectful primitive and without storing in the registry."
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (dorg-test--recording-registry-effects effects
      (let ((reg (dorg--classify-registration)))
        (should (dorg-registration-p reg))
        (should (eq (dorg-backend-type (dorg-registration-backend reg)) 'dolt))
        (should (string= (dorg-backend-dolt-dir (dorg-registration-backend reg))
                         (file-truename dolt-dir)))
        (should-not (dorg-backend-root-id (dorg-registration-backend reg)))
        ;; No init / schema / root / bind effect fired.
        (should-not effects)
        ;; Nothing persisted to the registry.
        (should-not dorg--registry)))))

(ert-deftest dorg-test-current-registration-keyword-dolt-is-read-only ()
  "`dorg-current-registration' stays read-only on a keyword Dolt buffer.
The public accessor must classify the source without initializing a repo,
migrating schema, allocating a root, or persisting a binding."
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (dorg-test--recording-registry-effects effects
      (let ((reg (dorg-current-registration)))
        (should (dorg-registration-p reg))
        (should (eq (dorg-backend-type (dorg-registration-backend reg)) 'dolt))
        (should-not (dorg-backend-root-id (dorg-registration-backend reg)))
        (should-not effects)
        (should-not dorg--registry)))))

(ert-deftest dorg-test-registered-accessors-keyword-dolt-are-pure ()
  "`dorg-registered-*' accessors classify a keyword Dolt buffer without effects.
The dir is reported, the root-id is nil, and no binding is created."
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (dorg-test--recording-registry-effects effects
      (should (string= (dorg-registered-dolt-dir) (file-truename dolt-dir)))
      (should-not (dorg-backend-root-id (dorg-registered-backend)))
      (should (dorg-backend-dolt-p (dorg-registered-backend)))
      (should-not effects)
      (should-not dorg--registry))))

(ert-deftest dorg-test-registered-root-id-facade-removed ()
  "The consumer-less root-id facade is absent before v0.1.

Root ids remain readable through the backend descriptor returned by
`dorg-current-registration' / `dorg-registered-backend'."
  (should-not (fboundp 'dorg-registered-root-id)))

(ert-deftest dorg-test-classify-plain-org-is-pure ()
  "`dorg--classify-registration' returns a transient plain-org reg for a
keyword-less buffer, firing no effect and storing nothing."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-awzwf-plain-" t)))
    (unwind-protect
        (let ((org-file (file-truename (expand-file-name "doc.org" temp-base))))
          (with-temp-buffer
            (insert "* H\n")
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (dorg-test--recording-registry-effects effects
              (let ((reg (dorg--classify-registration)))
                (should (dorg-backend-plain-org-p (dorg-registration-backend reg)))
                (should-not effects)
                (should-not dorg--registry)))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-classify-no-file-buffer-is-nil ()
  "`dorg--classify-registration' returns nil for a buffer with no file."
  (let ((dorg--registry nil))
    (with-temp-buffer
      (dorg-test--recording-registry-effects effects
        (should-not (dorg--classify-registration))
        (should-not effects)))))

(ert-deftest dorg-test-ensure-keyword-dolt-binds-strictly ()
  "`dorg--ensure-registration' on a keyword Dolt buffer binds via
`dorg-bind-dolt' and returns a registration carrying a real root-id."
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (let ((bind-calls '()))
      (cl-letf (((symbol-function 'dorg-bind-dolt)
                 (lambda (file dir)
                   (push (list (file-truename file) dir) bind-calls)
                   (make-dorg-registration
                    :backend (dorg-backend-dolt dir 4242)
                    :file (file-truename file)))))
        (let ((reg (dorg--ensure-registration)))
          (should (= 1 (length bind-calls)))
          (should (string= (caar bind-calls) org-file))
          (should (eq (dorg-backend-type (dorg-registration-backend reg)) 'dolt))
          (should (= 4242 (dorg-backend-root-id
                           (dorg-registration-backend reg)))))))))

(ert-deftest dorg-test-ensure-explicit-binding-does-not-rebind ()
  "`dorg--ensure-registration' returns an already-stored binding as-is,
without calling `dorg-bind-dolt' again."
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (let ((stored (make-dorg-registration
                   :backend (dorg-backend-dolt dolt-dir 77)
                   :file org-file))
          (bind-calls '()))
      (setq dorg--registry (list (cons org-file stored)))
      (cl-letf (((symbol-function 'dorg-bind-dolt)
                 (lambda (&rest _) (push t bind-calls) nil)))
        (let ((reg (dorg--ensure-registration)))
          (should-not bind-calls)
          (should (= 77 (dorg-backend-root-id
                         (dorg-registration-backend reg)))))))))

(ert-deftest dorg-test-ensure-plain-org-does-not-bind ()
  "`dorg--ensure-registration' returns a plain-org reg for a keyword-less
buffer and never calls `dorg-bind-dolt'."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-awzwf-ensure-plain-" t)))
    (unwind-protect
        (let ((org-file (file-truename (expand-file-name "doc.org" temp-base)))
              (bind-calls '()))
          (with-temp-buffer
            (insert "* H\n")
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (cl-letf (((symbol-function 'dorg-bind-dolt)
                       (lambda (&rest _) (push t bind-calls) nil)))
              (let ((reg (dorg--ensure-registration)))
                (should-not bind-calls)
                (should (dorg-backend-plain-org-p
                         (dorg-registration-backend reg)))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-defer-strict-binding-gate-removed ()
  "The dynamic `dorg--defer-strict-binding' gate is retired.
The pure/strict split replaces the deferral flag; leaving it bound would
mean the mixed classify/bind behavior it controlled still exists."
  (should-not (boundp 'dorg--defer-strict-binding)))

(ert-deftest dorg-test-history-resolves-root-via-ensure ()
  "`dorg-history' resolves its Dolt root through the strict ensure path.
On a keyword Dolt buffer, source classification is pure and yields a nil
root-id (no binding).  An interactive history browse operates on the store,
so it must obtain a REAL root-id by ensuring the binding — reading the pure
classification would hand `dorg-history--render-buffer' a nil root-id and
mis-scope every subsequent as-of read/restore.  RED until `dorg-history'
routes root-id resolution through `dorg--ensure-registration'."
  (require 'dorg-history)
  (dorg-test--with-keyword-dolt-buffer (org-file dolt-dir)
    (let ((captured nil))
      (cl-letf (;; Pure classification: keyword Dolt buffer, nil root-id.
                ((symbol-function 'dorg--classify-registration)
                 (lambda (&optional _)
                   (make-dorg-registration
                    :backend (dorg-backend-dolt dolt-dir nil)
                    :file org-file)))
                ;; Strict resolution binds and yields a real root-id.
                ((symbol-function 'dorg--ensure-registration)
                 (lambda (&optional _)
                   (make-dorg-registration
                    :backend (dorg-backend-dolt dolt-dir 4242)
                    :file org-file)))
                ((symbol-function 'dorg-history--render-buffer)
                 (lambda (state) (setq captured state))))
        ;; Root-scope prefix so no heading-id-at-point lookup is needed.
        (dorg-history t)
        (should (equal dolt-dir (plist-get captured :dolt-dir)))
        (should (= 4242 (plist-get captured :root-id)))))))

;;; ─── Capability API ───────────────────────────────────────────────────

(ert-deftest dorg-test-buffer-capability-facades-removed ()
  "The unused backend capability wrappers are absent before v0.1."
  (should-not (fboundp 'dorg-capabilities))
  (should-not (fboundp 'dorg-supports-p))
  (should-not (fboundp 'dorg-backend-supports-p))
  (should-not (fboundp 'dorg-backend-capabilities)))

;;; ─── Plain-Org Materialize ────────────────────────────────────────────

(ert-deftest dorg-test-materialize-plain-org-loads-file ()
  "Plain-org materialize replaces buffer contents with the file contents."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-mat-plain-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (contents "* Stored\nBody on disk.\n"))
          (with-temp-file org-file (insert contents))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (insert "* In-memory only\n")
            (set-buffer-modified-p t)
            (dorg-materialize)
            (should (equal (buffer-string) contents))
            (should-not (buffer-modified-p))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-materialize-plain-org-missing-file-errors ()
  "Plain-org materialize signals when the file does not exist."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-mat-missing-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "absent.org" temp-base)))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (should-error (dorg-materialize))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-materialize-plain-org-runs-hook ()
  "Plain-org materialize calls `dorg-after-materialize-functions'."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-mat-hook-" t))
        (calls nil))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* X\n"))
          (let ((dorg-after-materialize-functions
                 (list (lambda (loc buf)
                         (push (cons loc (buffer-name buf)) calls)))))
            (with-temp-buffer
              (set-visited-file-name org-file t)
              (set-buffer-modified-p nil)
              (dorg-materialize))
            (should (= 1 (length calls)))
            (should (equal (caar calls) (file-truename org-file)))))
      (delete-directory temp-base t))))

;;; ─── DORG_ID Assignment ───────────────────────────────────────────────

(ert-deftest dorg-test-assign-id-fills-missing-only ()
  "`dorg-assign-id' adds DORG_ID to headings missing one; preserves existing."
  (with-temp-buffer
    (org-mode)
    (insert "* Alpha
:PROPERTIES:
:DORG_ID: 42
:END:
* Beta
* Gamma
")
    (let ((assigned (dorg-assign-id)))
      ;; Alpha already had one — only two new assignments.
      (should (= 2 (length assigned))))
    ;; Alpha keeps its 42.
    (goto-char (point-min))
    (re-search-forward "^\\* Alpha")
    (should (equal (org-entry-get (point) "DORG_ID") "42"))
    ;; Beta and Gamma both got IDs.
    (goto-char (point-min))
    (re-search-forward "^\\* Beta")
    (should (string-match-p "\\`[0-9]+\\'"
                            (or (org-entry-get (point) "DORG_ID") "")))
    (goto-char (point-min))
    (re-search-forward "^\\* Gamma")
    (should (string-match-p "\\`[0-9]+\\'"
                            (or (org-entry-get (point) "DORG_ID") "")))))

(ert-deftest dorg-test-assign-id-idempotent ()
  "Running `dorg-assign-id' twice does not add new IDs on the second pass."
  (with-temp-buffer
    (org-mode)
    (insert "* One\n* Two\n")
    (dorg-assign-id)
    (let ((second (dorg-assign-id)))
      (should (null second)))))

(ert-deftest dorg-test-assign-id-no-headings ()
  "`dorg-assign-id' on a heading-free buffer is a no-op."
  (with-temp-buffer
    (org-mode)
    (insert "Just paragraph text.\n")
    (should (null (dorg-assign-id)))))

;;; ─── Manual dorg-mode on a no-keyword file is plain-org ──────────────

(ert-deftest dorg-test-manual-dorg-mode-no-keyword-is-plain-org ()
  "Manually enabling `dorg-mode' on a no-keyword file is plain-org.
No registry entry is allocated, and property lookup works through the
implicit registration."
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-manual-plain-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file
            (insert "* Alpha\n:PROPERTIES:\n:DORG_ID: 7\n:KIND: note\n:END:\n"))
          (let ((buf (find-file-noselect org-file)))
            (unwind-protect
                (with-current-buffer buf
                  (dorg-mode 1)
                  (should dorg-mode)
                  (should (dorg-backend-plain-org-p (dorg-registered-backend)))
                  (should-not (assoc (file-truename org-file) dorg--registry))
                  ;; DORG_ID is identity-only: looked up via the id facade,
                  ;; never via the custom-property facade (which mirrors
                  ;; model/Dolt storage that omit DORG_ID from properties).
                  (should (= 1 (length (dorg-find-heading-by-id 7))))
                  (should-not (dorg-find-headings-by-property "DORG_ID" "7"))
                  ;; Custom (non-identity) property lookup works through the
                  ;; implicit plain-org registration.
                  (should (= 1 (length (dorg-find-headings-by-property
                                        "KIND" "note"))))
                  (dorg-mode -1))
              (when (buffer-live-p buf)
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))))
      (delete-directory temp-base t))))

;;; ─── Legacy DORG_DIR literal scope (production sources) ──────────────

(ert-deftest dorg-test-dorg-dir-literal-confined-to-reader ()
  "The legacy `DORG_DIR' literal survives in exactly one production source.
Every other production .el uses `DORG_DOLT_DIR'; the legacy name may
appear only in `dorg-registry.el' (the reader fallback in
`dorg--buffer-dolt-dir').  Note `DORG_DOLT_DIR' does not contain the
substring `DORG_DIR', so this check is unambiguous."
  (let* ((registry (find-library-name "dorg-registry"))
         (src-dir (file-name-directory registry))
         (prod-files (directory-files src-dir t "\\`dorg.*\\.el\\'"))
         (offenders nil))
    (dolist (f prod-files)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward "DORG_DIR" nil t)
          (push (file-name-nondirectory f) offenders))))
    (should (equal offenders (list "dorg-registry.el")))))

;;; ─── Backend-Layer Strict-Bind DDL Ownership (dorg-health.1x1mv) ───────
;;
;; Honest layering: the storage backend layer (`dorg-repo') owns ALL
;; strict-bind DDL/DML (repo init, schema migration, root lookup/create).
;; The registry (`dorg-bind-dolt') keeps only registry bookkeeping and the
;; interactive binding UI, delegating the root resolution to the backend.

(require 'dorg-repo)

(ert-deftest dorg-test-repo-ensure-root-orchestration ()
  "`dorg-repo-ensure-root' owns the exact strict-bind DDL sequence.
The orchestrator probes DOLT-DIR/.dolt, inits only when the repo is absent,
always ensures the schema, then reuses an existing root (never re-creating)
when lookup hits, or allocates a fresh root when it misses.  The FULL ordered
call trace WITH ARGUMENTS is asserted so a mis-ordered sequence, a wrong
directory/file, or a skipped lookup is caught."
  (let ((dolt-dir "/db/")
        (abs-file "/db/x.org"))
    ;; Case A: repo present, root exists -> probe, schema, lookup; return 55.
    ;; NO init, NO create; schema must precede lookup.
    (let ((calls '()) (probed '()))
      (cl-letf (((symbol-function 'file-exists-p)
                 (lambda (f) (push f probed) t))
                ((symbol-function 'dorg-init)
                 (lambda (dir &rest _) (push (list 'init dir) calls) nil))
                ((symbol-function 'dorg-repo-ensure-schema)
                 (lambda (dir &rest _) (push (list 'schema dir) calls) nil))
                ((symbol-function 'dorg-repo-root-id)
                 (lambda (dir file) (push (list 'lookup dir file) calls) 55))
                ((symbol-function 'dorg-repo-file-row-count)
                 (lambda (dir) (push (list 'count dir) calls) 1))
                ((symbol-function 'dorg--create-root)
                 (lambda (dir file) (push (list 'create dir file) calls) 999)))
        (let ((root (dorg-repo-ensure-root dolt-dir abs-file)))
          (should (eql 55 root))
          ;; The existence probe targets DOLT-DIR/.dolt, not some other path.
          (should (member (expand-file-name ".dolt" dolt-dir) probed))
          ;; EXACT ordered trace with arguments: schema then lookup, nothing else.
          (should (equal (nreverse calls)
                         (list (list 'schema dolt-dir)
                               (list 'lookup dolt-dir abs-file)))))))
    ;; Case B: repo absent, lookup misses -> probe, init, schema, lookup, create.
    ;; Init must precede schema; lookup must run before create; return 4242.
    (let ((calls '()) (probed '()))
      (cl-letf (((symbol-function 'file-exists-p)
                 (lambda (f) (push f probed) nil))
                ((symbol-function 'dorg-init)
                 (lambda (dir &rest _) (push (list 'init dir) calls) nil))
                ((symbol-function 'dorg-repo-ensure-schema)
                 (lambda (dir &rest _) (push (list 'schema dir) calls) nil))
                ((symbol-function 'dorg-repo-root-id)
                 (lambda (dir file) (push (list 'lookup dir file) calls) nil))
                ((symbol-function 'dorg-repo-file-row-count)
                 (lambda (dir) (push (list 'count dir) calls) 0))
                ((symbol-function 'dorg--create-root)
                 (lambda (dir file) (push (list 'create dir file) calls) 4242)))
        (let ((root (dorg-repo-ensure-root dolt-dir abs-file)))
          (should (eql 4242 root))
          (should (member (expand-file-name ".dolt" dolt-dir) probed))
          ;; EXACT ordered trace with arguments: init, schema, lookup, create.
          (should (equal (nreverse calls)
                         (list (list 'init dolt-dir)
                               (list 'schema dolt-dir)
                               (list 'lookup dolt-dir abs-file)
                               (list 'count dolt-dir)
                               (list 'create dolt-dir abs-file)))))))))

(ert-deftest dorg-test-bind-dolt-delegates-root-to-backend ()
  "`dorg-bind-dolt' resolves the root via `dorg-repo-ensure-root', canonicalized.
The registry delegates ALL root DDL to the backend: it must pass the exact
CANONICALIZED (directory . file) pair to `dorg-repo-ensure-root' and store the
returned root-id, and it must NOT run any strict-bind DDL inline.
Non-vacuity: the canonicalizer stub maps the caller's alias-case paths to a
distinct on-disk case, so asserting the canonical pair fails if bind forwards
the raw inputs; and the inline-DDL leak guard fails if bind resolves the root
itself instead of delegating."
  (let ((dorg--registry nil)
        (ensure-args 'unset)
        (leaks '())
        ;; Alias-case inputs; the canonicalizer maps them to the on-disk case.
        (raw-file "/Base/Doc.org")
        (raw-dir  "/Base/DB")
        (want-file "/base/doc.org")
        (want-dir  "/base/db/"))       ; `file-name-as-directory' adds the slash
    (cl-letf (;; Neutralize truename so only the canonicalizer transforms paths.
              ((symbol-function 'file-truename) #'identity)
              ((symbol-function 'file-exists-p) (lambda (_f) t))
              ((symbol-function 'dorg--canonical-existing-case)
               (lambda (p) (downcase p)))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (dir file) (setq ensure-args (list dir file)) 31337))
              ;; Registry-side strict-bind DDL must NOT fire: bind delegates.
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (push 'init leaks) nil))
              ((symbol-function 'dorg-repo-ensure-schema)
               (lambda (&rest _) (push 'schema leaks) nil))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (&rest _) (push 'lookup leaks) nil))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (push 'create leaks) nil)))
      (let ((reg (dorg-bind-dolt raw-file raw-dir)))
        ;; Delegated with the EXACT canonicalized (dir . file) pair.
        (should (equal ensure-args (list want-dir want-file)))
        ;; Registry stored the backend-returned root-id.
        (should (= 31337 (dorg-backend-root-id
                          (dorg-registration-backend reg))))
        (should (assoc want-file dorg--registry))
        ;; No strict-bind DDL leaked into the registry.
        (should-not leaks)))))

(ert-deftest dorg-test-bind-existing-dolt-resolves-only-existing-root ()
  "The safe reader bind uses only the read-only schema/root path.
It records a registration only after canonicalization, compatibility, and a
concrete existing root all succeed; every strict or write primitive is a hard
failure seam."
  (let ((dorg--registry nil)
        (calls '())
        (raw-file "/Base/Doc.org")
        (raw-dir "/Base/DB")
        (want-file "/base/doc.org")
        (want-dir "/base/db/"))
    (cl-letf (((symbol-function 'file-truename) #'identity)
              ((symbol-function 'dorg--canonical-existing-case)
               (lambda (path) (downcase path)))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (member path (list want-dir
                                    (expand-file-name ".dolt" want-dir)))))
              ((symbol-function 'dorg-repo-ensure-schema)
               (lambda (dir) (push (list 'schema dir) calls)))
              ((symbol-function 'dorg-repo-check-schema-compat)
               (lambda (&rest _)
                 (ert-fail "safe bind used core-only compatibility")))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (dir file) (push (list 'root dir file) calls) 31337))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "safe bind reached strict bind")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "safe bind reached strict root")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "safe bind initialized")))
              ((symbol-function 'dorg-migrate-schema)
               (lambda (&rest _) (ert-fail "safe bind migrated")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "safe bind created root")))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (&rest _) (ert-fail "safe bind counted rows")))
              ((symbol-function 'dorg-sql-batch)
               (lambda (&rest _) (ert-fail "safe bind batched SQL")))
              ((symbol-function 'dorg-commit)
               (lambda (&rest _) (ert-fail "safe bind committed"))))
      (let ((reg (dorg-bind-existing-dolt raw-file raw-dir)))
        (should (equal (nreverse calls)
                       (list (list 'schema want-dir)
                             (list 'root want-dir want-file))))
        (should (= 31337 (dorg-backend-root-id
                          (dorg-registration-backend reg))))
        (should (assoc want-file dorg--registry))))))

(ert-deftest dorg-test-bind-existing-dolt-unbound-is-path-mismatch ()
  "An unbound file refuses without observing row count or mutating registry.
This reason is intentionally distinct from the existing corrupt-root
`missing-root' condition emitted by `dorg-repo-root-id'."
  (let* ((stored (make-dorg-registration
                  :backend (dorg-backend-dolt "/other/" 9)
                  :file "/other/doc.org"))
         (dorg--registry (list (cons "/other/doc.org" stored)))
         (before dorg--registry)
         (dir "/store/")
         (file "/source.org"))
    (cl-letf (((symbol-function 'file-truename) #'identity)
              ((symbol-function 'dorg--canonical-existing-case) #'identity)
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (member path (list dir (expand-file-name ".dolt" dir)))))
              ((symbol-function 'dorg-repo-ensure-schema) (lambda (&rest _) nil))
              ((symbol-function 'dorg-repo-root-id) (lambda (&rest _) nil))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "unbound safe bind reached strict bind")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "unbound safe bind reached strict root")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "unbound safe bind initialized")))
              ((symbol-function 'dorg-migrate-schema)
               (lambda (&rest _) (ert-fail "unbound safe bind migrated")))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (&rest _) (ert-fail "unbound safe bind counted rows")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "unbound safe bind created root")))
              ((symbol-function 'dorg-sql-batch)
               (lambda (&rest _) (ert-fail "unbound safe bind batched SQL")))
              ((symbol-function 'dorg-commit)
               (lambda (&rest _) (ert-fail "unbound safe bind committed"))))
      (let ((err (should-error (dorg-bind-existing-dolt file dir)
                               :type 'dorg-binding-error)))
        (should (eq 'path-mismatch (dorg-binding-error-reason err)))
        (should (equal dir (plist-get (dorg-binding-error-data err) :dolt-dir)))
        (should (equal file (plist-get (dorg-binding-error-data err) :file)))
        (should (eq before dorg--registry))))))

(ert-deftest dorg-test-bind-existing-dolt-missing-store-precedes-effects ()
  "A missing store produces the typed refusal before SQL or server startup."
  (let ((dorg--registry nil)
        (dir "/missing-store/")
        (file "/source.org"))
    (cl-letf (((symbol-function 'file-truename) #'identity)
              ((symbol-function 'dorg--canonical-existing-case) #'identity)
              ((symbol-function 'file-directory-p) (lambda (&rest _) nil))
              ((symbol-function 'dorg-sql)
               (lambda (&rest _) (ert-fail "missing store reached SQL")))
              ((symbol-function 'dorg-repo--ordinary-read)
               (lambda (&rest _) (ert-fail "missing store read schema")))
              ((symbol-function 'dorg-dolt-server--start-bootstrap)
               (lambda (&rest _) (ert-fail "missing store started server")))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "missing store reached strict bind")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "missing store reached strict root")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "missing store initialized")))
              ((symbol-function 'dorg-migrate-schema)
               (lambda (&rest _) (ert-fail "missing store migrated")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "missing store created root")))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (&rest _) (ert-fail "missing store counted rows")))
              ((symbol-function 'dorg-sql-batch)
               (lambda (&rest _) (ert-fail "missing store batched SQL")))
              ((symbol-function 'dorg-commit)
               (lambda (&rest _) (ert-fail "missing store committed")))
              ((symbol-function 'dorg-repo-ensure-schema)
               (lambda (&rest _) (ert-fail "missing store checked schema")))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (&rest _) (ert-fail "missing store looked up root"))))
      (let ((err (should-error (dorg-bind-existing-dolt file dir)
                               :type 'dorg-binding-error)))
        (should (eq 'missing-store (dorg-binding-error-reason err)))
        (should (equal dir (plist-get (dorg-binding-error-data err) :dolt-dir)))
        (should (equal file (plist-get (dorg-binding-error-data err) :file)))
        (should-not dorg--registry)))))

(ert-deftest dorg-test-bind-existing-dolt-rejects-missing-or-file-dolt-dir ()
  "A real store directory without a `.dolt' directory never reaches effects.
This distinguishes a missing `.dolt' entry from a missing store directory and
also rejects a regular file named `.dolt'."
  (let* ((root (make-temp-file "dorg-mxwnw-missing-store-" t))
         (store (expand-file-name "store" root))
         (file (expand-file-name "source.org" root))
         (dorg--registry nil))
    (unwind-protect
        (progn
          (make-directory store)
          (with-temp-file file (insert "* Source\n"))
          (cl-letf (((symbol-function 'dorg-sql)
                     (lambda (&rest _) (ert-fail "missing store reached SQL")))
                    ((symbol-function 'dorg-repo--ordinary-read)
                     (lambda (&rest _) (ert-fail "missing store read schema")))
                    ((symbol-function 'dorg-dolt-server--start-bootstrap)
                     (lambda (&rest _) (ert-fail "missing store started server")))
                    ((symbol-function 'dorg-bind-dolt)
                     (lambda (&rest _) (ert-fail "missing store reached strict bind")))
                    ((symbol-function 'dorg-repo-ensure-root)
                     (lambda (&rest _) (ert-fail "missing store reached strict root")))
                    ((symbol-function 'dorg-init)
                     (lambda (&rest _) (ert-fail "missing store initialized")))
                    ((symbol-function 'dorg-migrate-schema)
                     (lambda (&rest _) (ert-fail "missing store migrated")))
                    ((symbol-function 'dorg--create-root)
                     (lambda (&rest _) (ert-fail "missing store created root")))
                    ((symbol-function 'dorg-repo-file-row-count)
                     (lambda (&rest _) (ert-fail "missing store counted rows")))
                    ((symbol-function 'dorg-sql-batch)
                     (lambda (&rest _) (ert-fail "missing store batched SQL")))
                    ((symbol-function 'dorg-commit)
                     (lambda (&rest _) (ert-fail "missing store committed"))))
            (dolist (case '(absent regular-file))
              (when (eq case 'regular-file)
                (with-temp-file (expand-file-name ".dolt" store)
                  (insert "not a directory")))
              (let ((err (should-error (dorg-bind-existing-dolt file store)
                                       :type 'dorg-binding-error)))
                (should (eq 'missing-store (dorg-binding-error-reason err)))
                (should (equal (file-name-as-directory (file-truename store))
                               (plist-get (dorg-binding-error-data err) :dolt-dir)))
                (should (equal (file-truename file)
                               (plist-get (dorg-binding-error-data err) :file)))
                (should-not dorg--registry)))))
      (delete-directory root t))))

(ert-deftest dorg-test-bind-existing-dolt-cold-loads-typed-missing-store ()
  "A cold registry-only process reaches the typed missing-store refusal.
The child proves the function loads `dorg-repo' before it constructs the
binding condition; deleting that require yields an untyped void-function."
  (let* ((root (file-name-directory (locate-library "dorg-registry")))
         (expr
          "(progn (require 'dorg-registry) (let ((err (condition-case e (progn (dorg-bind-existing-dolt \"/nonexistent-mx/source.org\" \"/nonexistent-mx/db\") nil) (error e)))) (unless (and (eq (car err) 'dorg-binding-error) (eq (dorg-binding-error-reason err) 'missing-store)) (error \"wrong cold refusal: %S\" err))))"))
    (with-temp-buffer
      (let ((status (call-process "emacs" nil (current-buffer) nil
                                  "--batch" "-Q"
                                  "-L" root
                                  "-L" (expand-file-name "lib/clime" root)
                                  "--eval" expr)))
        (ert-info ((buffer-string))
          (should (equal 0 status)))))))

(ert-deftest dorg-test-bind-existing-dolt-preserves-corrupt-root-refusal ()
  "The safe reader does not collapse a corrupt binding into an unbound miss."
  (let ((dorg--registry nil)
        (dir "/store/")
        (file "/source.org"))
    (cl-letf (((symbol-function 'file-truename) #'identity)
              ((symbol-function 'dorg--canonical-existing-case) #'identity)
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (member path (list dir (expand-file-name ".dolt" dir)))))
              ((symbol-function 'dorg-repo-ensure-schema) (lambda (&rest _) nil))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (&rest _)
                 (dorg-repo--binding-error
                  'missing-root "corrupt root" :root-id 88)))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "corrupt safe bind reached strict bind")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "corrupt safe bind reached strict root")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "corrupt safe bind initialized")))
              ((symbol-function 'dorg-migrate-schema)
               (lambda (&rest _) (ert-fail "corrupt safe bind migrated")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "corrupt safe bind created root")))
              ((symbol-function 'dorg-repo-file-row-count)
               (lambda (&rest _) (ert-fail "corrupt safe bind counted rows")))
              ((symbol-function 'dorg-sql-batch)
               (lambda (&rest _) (ert-fail "corrupt safe bind batched SQL")))
              ((symbol-function 'dorg-commit)
               (lambda (&rest _) (ert-fail "corrupt safe bind committed"))))
      (let ((err (should-error (dorg-bind-existing-dolt file dir)
                               :type 'dorg-binding-error)))
        (should (eq 'missing-root (dorg-binding-error-reason err)))
        (should (= 88 (plist-get (dorg-binding-error-data err) :root-id)))
        (should-not dorg--registry)))))

(ert-deftest dorg-test-registry-owns-no-strict-bind-ddl ()
  "The registry no longer straddles into strict-bind DDL.
The relocated primitives and the orchestrator are owned by the `dorg-repo'
backend layer.  The registry source must (i) declare only its backend entry
point and the projection-policy capability, (ii) make NO direct call to the
full strict-bind DDL set, and (iii) no longer `require' `dorg-id' — that
dependency belongs to `dorg-repo', which owns
`dorg--create-root'/`dorg-id-generate'.
Non-vacuity: every clause fails against the pre-refactor registry, which
defined the DDL, declared six Dolt stubs, and required `dorg-id' itself."
  (should (fboundp 'dorg-repo-ensure-root))
  ;; The relocated primitives + orchestrator resolve to dorg-repo, not registry.
  (dolist (sym '(dorg-repo-ensure-root dorg--create-root dorg-repo-root-id))
    (should (string-match-p "dorg-repo\\.elc?\\'"
                            (symbol-file sym 'defun))))
  (let ((reg-src (with-temp-buffer
                   (insert-file-contents (find-library-name "dorg-registry"))
                   (buffer-string)))
        (repo-src (with-temp-buffer
                    (insert-file-contents (find-library-name "dorg-repo"))
                    (buffer-string)))
        (case-fold-search nil))
    ;; (i) The registry declares its strict entry and the three read-only
    ;;     existing-store helpers.  The neutral projection capability is a
    ;;     direct lightweight dependency.
    (let ((decls '()))
      (with-temp-buffer
        (insert reg-src)
        (goto-char (point-min))
        (while (re-search-forward
                "(declare-function[ \t]+\\([^ \t\n]+\\)" nil t)
          (push (match-string 1) decls)))
      (should (equal (sort (delete-dups decls) #'string<)
                     '("dorg-repo--binding-error"
                       "dorg-repo-ensure-root"
                       "dorg-repo-ensure-schema"
                       "dorg-repo-root-id"))))
    (should (string-match-p "(require 'dorg-projection)" reg-src))
    ;; (ii) NO direct call to any member of the full strict-bind DDL set.
    (dolist (fn '("dorg-sql" "dorg-sql-batch" "dorg-sql-quote" "dorg-commit"
                  "dorg-init" "dorg--create-root"))
      (should-not (string-match-p
                   (concat "(" (regexp-quote fn) "\\_>") reg-src)))
    ;; (iii) The dorg-id dependency moved with the helper: registry drops it,
    ;;       dorg-repo picks it up (masked before by the registry require).
    (should-not (string-match-p "(require '?dorg-id\\_>" reg-src))
    (should (string-match-p "(require '?dorg-id\\_>" repo-src))))

(ert-deftest dorg-test-registry-registration-alias-collapsed ()
  "One private name owns pure source classification; the alias is gone.
`dorg--registration' was an exact-delegate rename of
`dorg--classify-registration' with no downstream consumer, so it bought
nothing and cost a second name for one contract.  What must NOT collapse
with it is the three-way split the registry is built on: the pure
classifier, the effectful `dorg--ensure-registration' that may initialize
and bind, and the public `dorg-current-registration' facade.  Re-adding the
alias turns the `should-not' RED; deleting any of the three survivors turns
a `should' RED."
  (should-not (fboundp 'dorg--registration))
  (should (fboundp 'dorg--classify-registration))
  (should (fboundp 'dorg--ensure-registration))
  (should (fboundp 'dorg-current-registration))
  ;; The public facade still resolves through the surviving classifier: a
  ;; stubbed classifier is observed through it, which is exactly the reach
  ;; the collapse widens and the reason it is asserted rather than assumed.
  (let ((reg (make-dorg-registration
              :backend (dorg-backend-plain-org)
              :file "/tmp/dorg-alias-collapse.org")))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&rest _) reg)))
      (should (eq (dorg-current-registration) reg))
      (should (eq (dorg-registered-backend) (dorg-registration-backend reg)))))
  ;; And the pure classifier is genuinely still pure: classifying a
  ;; file-backed buffer stores no binding.
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "dorg-alias-collapse-" t)))
    (unwind-protect
        (let ((org-file (expand-file-name "doc.org" temp-base)))
          (with-temp-file org-file (insert "* H\n"))
          (with-temp-buffer
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (should (dorg--classify-registration))
            (should-not dorg--registry)))
      (delete-directory temp-base t))))

(provide 'dorg-backend-tests)
;;; dorg-backend-tests.el ends here
