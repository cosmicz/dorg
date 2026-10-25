;;; dorg-find-tests.el --- Tests for dorg-find lookup APIs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for heading/property lookup across plain-org and Dolt backends.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-find)
(require 'dorg-registry)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Helpers ──────────────────────────────────────────────────────────

(defmacro dorg-test--with-plain-org-file (contents bindings &rest body)
  "Write CONTENTS to a temp file, open it plain-org, then run BODY.
The file declares no Dolt-dir keyword, so it is plain-org by
default — no explicit registration call is needed.
BINDINGS is a plist of names: (:file FVAR :buffer BVAR).  Either
key may be omitted to skip binding."
  (declare (indent 2))
  (let ((file-var (or (plist-get bindings :file) (gensym "file")))
        (buf-var (or (plist-get bindings :buffer) (gensym "buf"))))
    `(let ((dorg--registry nil)
           (temp-base (make-temp-file "dorg-find-test-" t)))
       (unwind-protect
           (let ((,file-var (expand-file-name "doc.org" temp-base)))
             (with-temp-file ,file-var (insert ,contents))
             (let ((,buf-var (find-file-noselect ,file-var)))
               (unwind-protect
                   (progn ,@body)
                 (when (buffer-live-p ,buf-var)
                   (with-current-buffer ,buf-var
                     (set-buffer-modified-p nil))
                   (kill-buffer ,buf-var)))))
         (delete-directory temp-base t)))))

;;; ─── Plain-Org Lookups ────────────────────────────────────────────────

(ert-deftest dorg-test-find-plain-by-id-hit ()
  "Plain-org `dorg-find-heading-by-id' locates a heading by DORG_ID."
  (dorg-test--with-plain-org-file
      "* Alpha
:PROPERTIES:
:DORG_ID: 100
:END:
* Beta
:PROPERTIES:
:DORG_ID: 200
:END:
"
      (:buffer buf)
    (let ((results (dorg-find-heading-by-id 200 buf)))
      (should (= 1 (length results)))
      (let ((r (car results)))
        (should (= 200 (plist-get r :heading-id)))
        (should (equal "Beta" (plist-get r :title)))
        (should (markerp (plist-get r :marker)))))))

(ert-deftest dorg-test-find-plain-by-id-miss ()
  "Plain-org `dorg-find-heading-by-id' returns nil when no heading matches."
  (dorg-test--with-plain-org-file
      "* Only\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (null (dorg-find-heading-by-id 9999 buf)))))

(ert-deftest dorg-test-find-plain-by-property-hit ()
  "Plain-org `dorg-find-headings-by-property' filters by KEY = VALUE."
  (dorg-test--with-plain-org-file
      "* Match
:PROPERTIES:
:CATEGORY: red
:END:
* Skip
:PROPERTIES:
:CATEGORY: blue
:END:
* Also match
:PROPERTIES:
:CATEGORY: red
:END:
"
      (:buffer buf)
    (let ((results (dorg-find-headings-by-property "CATEGORY" "red" buf)))
      (should (= 2 (length results)))
      (let ((titles (mapcar (lambda (r) (plist-get r :title)) results)))
        (should (member "Match" titles))
        (should (member "Also match" titles))
        (should-not (member "Skip" titles))))))

(ert-deftest dorg-test-find-plain-by-property-miss ()
  "Plain-org property lookup returns empty when no heading matches."
  (dorg-test--with-plain-org-file
      "* H\n:PROPERTIES:\n:K: v\n:END:\n"
      (:buffer buf)
    (should (null (dorg-find-headings-by-property "K" "other" buf)))))

(ert-deftest dorg-test-find-plain-by-id-nil-heading-id ()
  "Plain-org headings without DORG_ID can still be returned by property."
  (dorg-test--with-plain-org-file
      "* No id\n:PROPERTIES:\n:TAG: x\n:END:\n"
      (:buffer buf)
    (let* ((results (dorg-find-headings-by-property "TAG" "x" buf))
           (r (car results)))
      (should r)
      (should (null (plist-get r :heading-id)))
      (should (equal "No id" (plist-get r :title))))))

(ert-deftest dorg-test-find-unregistered-errors ()
  "Lookup on an unregistered buffer signals."
  (let ((dorg--registry nil))
    (with-temp-buffer
      (insert "* H\n")
      (should-error (dorg-find-heading-by-id 1 (current-buffer))))))

;;; ─── Dolt Lookups ─────────────────────────────────────────────────────

(defmacro dorg-test--with-dolt-synced-find (contents bindings &rest body)
  "Import CONTENTS, bind its Dolt-backed registration, and run BODY.
BINDINGS plist: (:dolt-dir DDVAR :reg RVAR :buffer BVAR).
Skips if Dolt is unavailable."
  (declare (indent 2))
  (let ((dolt-dir (or (plist-get bindings :dolt-dir) (gensym "dd")))
        (reg (or (plist-get bindings :reg) (gensym "reg")))
        (buf (or (plist-get bindings :buffer) (gensym "buf"))))
    `(if (not dorg-test--dolt-available)
         (ert-skip "dolt not installed")
       (let* ((temp-base (dorg-test--make-owned-temp-dir
                          "dorg-find-dolt-" "find-dolt-synced"))
              (,dolt-dir (expand-file-name "dolt/" temp-base))
              (org-file (expand-file-name "doc.org" temp-base)))
         (unwind-protect
             (let ((dorg--registry nil))
               (dorg-init ,dolt-dir)
               (with-temp-buffer
                 (insert ,contents)
                 (org-mode)
                 (set-visited-file-name org-file t)
                 (set-buffer-modified-p nil)
                 (dorg-import-document
                  (current-buffer)
                  :dolt-dir ,dolt-dir
                  :target-file org-file
                  :mode 'bootstrap)
                 (let ((,reg (dorg-bind-dolt org-file ,dolt-dir))
                       (,buf (current-buffer)))
                   (ignore ,reg)
                   ,@body)))
           (dorg-test--delete-owned-root temp-base))))))

(ert-deftest dorg-test-find-dolt-by-id-hit ()
  "Dolt `dorg-find-heading-by-id' returns the matching row."
  (dorg-test--with-dolt-synced-find
      "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n* Other\n"
      (:buffer buf)
    (let* ((results (dorg-find-heading-by-id 555 buf))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 555 (plist-get r :heading-id)))
      (should (equal "Find me" (plist-get r :title))))))

(ert-deftest dorg-test-find-dolt-by-id-miss ()
  "Dolt `dorg-find-heading-by-id' returns nil when no row matches."
  (dorg-test--with-dolt-synced-find
      "* Just one\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (null (dorg-find-heading-by-id 9999 buf)))))

(ert-deftest dorg-test-find-dolt-by-property-hit ()
  "Dolt `dorg-find-headings-by-property' returns rows matching KEY = VALUE."
  (dorg-test--with-dolt-synced-find
      "* Red one
:PROPERTIES:
:CATEGORY: red
:END:
* Blue
:PROPERTIES:
:CATEGORY: blue
:END:
* Red two
:PROPERTIES:
:CATEGORY: red
:END:
"
      (:buffer buf)
    (let* ((results (dorg-find-headings-by-property "CATEGORY" "red" buf))
           (titles (mapcar (lambda (r) (plist-get r :title)) results)))
      (should (= 2 (length results)))
      (should (member "Red one" titles))
      (should (member "Red two" titles))
      (should-not (member "Blue" titles)))))

(ert-deftest dorg-test-find-dolt-by-property-miss ()
  "Dolt property lookup returns empty when no row matches."
  (dorg-test--with-dolt-synced-find
      "* H\n:PROPERTIES:\n:K: v\n:END:\n"
      (:buffer buf)
    (should (null (dorg-find-headings-by-property "K" "other" buf)))))

(ert-deftest dorg-test-find-dolt-by-property-batches-load ()
  "`dorg-find-headings-by-property' over >=2 rows keeps shape/order and
loads result properties with a single batched query.

The facade lowers to a `(property KEY VALUE)' selector, so the selector
query itself reads `heading_properties' once BEFORE property hydration.
The broad `heading_properties' count is therefore not a one-query guard
here; instead this counts only the batched load (`WHERE heading_id IN (')
and asserts exactly one for the multi-row result."
  (dorg-test--with-dolt-synced-find
      "* Red one
:PROPERTIES:
:CATEGORY: red
:KEY: a
:END:
* Blue
:PROPERTIES:
:CATEGORY: blue
:END:
* Red two
:PROPERTIES:
:CATEGORY: red
:KEY: b
:END:
"
      (:buffer buf)
    (let ((in-count 0)
          (hp-count 0)
          (orig-live (symbol-function 'dorg-sql))
          (orig-pinned (symbol-function 'dorg-sql--at-revision)))
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (dolt-dir query &rest args)
                   (let ((statement (if args (apply #'format query args) query)))
                     (when (string-match-p "WHERE heading_id IN (" statement)
                       (setq in-count (1+ in-count)))
                     (when (string-match-p "heading_properties" statement)
                       (setq hp-count (1+ hp-count))))
                   (apply orig-live dolt-dir query args)))
                ((symbol-function 'dorg-sql--at-revision)
                 (lambda (dolt-dir revision query &rest args)
                   (let ((statement (if args (apply #'format query args) query)))
                     (when (string-match-p "WHERE heading_id IN (" statement)
                       (setq in-count (1+ in-count)))
                     (when (string-match-p "heading_properties" statement)
                       (setq hp-count (1+ hp-count))))
                   (apply orig-pinned dolt-dir revision query args))))
        (let* ((results (dorg-find-headings-by-property "CATEGORY" "red" buf))
               (titles (mapcar (lambda (r) (plist-get r :title)) results)))
          ;; Shape/order: both reds present, blue absent, markerless plists.
          (should (= 2 (length results)))
          (should (member "Red one" titles))
          (should (member "Red two" titles))
          (should-not (member "Blue" titles))
          (dolist (r results)
            (should (equal '(:heading-id :marker :title :properties)
                           (cl-loop for (k _v) on r by #'cddr collect k)))
            (should (null (plist-get r :marker))))
          ;; Exactly one batched property-load query, regardless of M.
          (should (= 1 in-count))
          ;; Two broad heading_properties queries: selector + batch.
          (should (= 2 hp-count)))))))

(provide 'dorg-find-tests)
;;; dorg-find-tests.el ends here
