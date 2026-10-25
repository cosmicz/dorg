;;; dorg-vfs-tests.el --- Tests for the read-only /dorg: VFS (Phase 1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-vfs' (Phase 1).  Pure parser/precondition
;; units run without Dolt; the find-file / freshness / regression-guard
;; integration tests are Dolt-gated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-parse)
(require 'dorg-render)
(require 'dorg-dolt)
(require 'dorg-vfs)
(require 'dorg-mutate)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Helpers ────────────────────────────────────────────────────────

(defmacro dorg-test-vfs--with-handler (&rest body)
  "Run BODY with the /dorg: handler installed, removed afterward."
  (declare (indent 0) (debug t))
  `(progn
     (dorg-vfs-enable)
     (unwind-protect (progn ,@body)
       (dorg-vfs-disable))))

(defun dorg-test-vfs--populate (dolt-dir contents)
  "Import CONTENTS into DOLT-DIR and commit; return non-nil on import."
  (let ((stmts (dorg-test--parse-to-sql contents)))
    (when stmts
      (let ((dorg-sql-transport 'server))
        (dorg-sql-batch dolt-dir stmts)
        (dorg-commit dolt-dir "vfs test import"))
      t)))

(defun dorg-test-vfs--a-node-id (dolt-dir)
  "Return some level>0 heading id in DOLT-DIR."
  (alist-get 'id (car (dorg-sql dolt-dir
                                "SELECT id FROM headings WHERE level_offset > 0 ORDER BY id LIMIT 1"))))

;;; ─── Parser units (no Dolt) ─────────────────────────────────────────

(ert-deftest dorg-test-vfs-parse-whole-tree ()
  "Whole-tree path parses to an empty node id."
  (let ((p (dorg-vfs--parse "/dorg:/abs/db:/")))
    (should (equal (plist-get p :dolt-dir) "/abs/db"))
    (should (null (plist-get p :ref)))
    (should (null (plist-get p :node-id)))))

(ert-deftest dorg-test-vfs-parse-island ()
  "Island path parses the integer node id."
  (let ((p (dorg-vfs--parse "/dorg:/abs/db:/100")))
    (should (equal (plist-get p :dolt-dir) "/abs/db"))
    (should (= (plist-get p :node-id) 100))
    (should (null (plist-get p :ref)))))

(ert-deftest dorg-test-vfs-parse-ref ()
  "@REF splits on the last @ into dolt-dir and ref."
  (let ((p (dorg-vfs--parse "/dorg:/abs/db@main:/100")))
    (should (equal (plist-get p :dolt-dir) "/abs/db"))
    (should (equal (plist-get p :ref) "main"))
    (should (= (plist-get p :node-id) 100))))

(ert-deftest dorg-test-vfs-parse-non-magic-returns-nil ()
  "A non-/dorg: name is not parsed."
  (should (null (dorg-vfs--parse "/tmp/real.org"))))

(ert-deftest dorg-test-vfs-unparse-public-seam ()
  "`dorg-vfs-unparse' is the public cross-module seam; the old private
`dorg-vfs--unparse' name is gone (tracked-regression rename)."
  (should (fboundp 'dorg-vfs-unparse))
  (should-not (fboundp 'dorg-vfs--unparse)))

(ert-deftest dorg-test-vfs-unparse-roundtrip ()
  "unparse o parse is identity for canonical inputs."
  (dolist (name '("/dorg:/abs/db:/" "/dorg:/abs/db:/100" "/dorg:/abs/db@r1:/7"))
    (should (equal (dorg-vfs-unparse (dorg-vfs--parse name)) name))))

(ert-deftest dorg-test-vfs-parse-rejects-relative-dolt-dir ()
  "A relative DOLT-DIR is rejected."
  (should-error (dorg-vfs--parse "/dorg:rel/db:/") :type 'dorg-vfs-error))

(ert-deftest dorg-test-vfs-parse-rejects-colon-in-dolt-dir ()
  "A colon inside DOLT-DIR (localname not starting with /) is rejected."
  (should-error (dorg-vfs--parse "/dorg:/a:b/db:/") :type 'dorg-vfs-error))

(ert-deftest dorg-test-vfs-parse-rejects-at-in-dolt-dir ()
  "An @ inside DOLT-DIR (beyond the ref split) is rejected."
  (should-error (dorg-vfs--parse "/dorg:/a@b/db@r:/") :type 'dorg-vfs-error))

(ert-deftest dorg-test-vfs-parse-rejects-non-integer-node ()
  "A non-integer NODE-ID is rejected."
  (should-error (dorg-vfs--parse "/dorg:/abs/db:/abc") :type 'dorg-vfs-error))

(ert-deftest dorg-test-vfs-whole-tree-ref-is-signalled ()
  "Whole-tree @REF is unsupported in Phase 1 and signals (not silently ignored)."
  ;; parse succeeds (a valid grammar); the render/dispatch refuses it.
  (let ((p (dorg-vfs--parse "/dorg:/abs/db@r1:/")))
    (should (plist-get p :ref))
    (should (null (plist-get p :node-id)))
    (should-error (dorg-vfs--render p) :type 'dorg-vfs-error)
    (should-error (dorg-vfs--assert-tree-no-ref p) :type 'dorg-vfs-error)))

;;; ─── Freshness helper (Dolt-gated) — verify dolt_log(refish) works ──

(ert-deftest dorg-test-vfs-head-time-is-a-time ()
  "`dorg-vfs--head-time' returns a usable Emacs time for HEAD and @REF."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-test-vfs--populate dolt-dir "* One\n")
      (let* ((head (dorg-head-commit dolt-dir))
             (t-head (dorg-vfs--head-time dolt-dir))
             (t-ref (dorg-vfs--head-time dolt-dir head)))
        (should (time-less-p t-head (current-time)))     ; a real, past time
        (should (time-equal-p t-head t-ref))))))         ; @HEAD == HEAD

;;; ─── Content / find-file integration (Dolt-gated) ───────────────────

(ert-deftest dorg-test-vfs-whole-tree-materializes ()
  "AC1: find-file on a whole-tree path == (dorg-render dolt-dir)."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-test-vfs--populate dolt-dir "* Alpha\n* Beta\n")
      (dorg-test-vfs--with-handler
        (let ((buf (find-file-noselect (format "/dorg:%s:/" dolt-dir))))
          (unwind-protect
              (with-current-buffer buf
                (should (equal (buffer-string) (dorg-render dolt-dir)))
                (should (string-match-p "Alpha" (buffer-string)))
                (should-not (buffer-modified-p)))
            (kill-buffer buf)))))))

(ert-deftest dorg-test-vfs-island-materializes-partial ()
  "AC2: island path equals pure island render and installs partial scope."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-test-vfs--populate dolt-dir "* Alpha\n** Child\n* Beta\n")
      (let ((nid (dorg-test-vfs--a-node-id dolt-dir)))
        (dorg-test-vfs--with-handler
          (let ((buf (find-file-noselect (format "/dorg:%s:/%d" dolt-dir nid))))
            (unwind-protect
                (with-current-buffer buf
                  (should (equal (buffer-string)
                                 (dorg-render-island dolt-dir nid)))
                  (should (eq (dorg-materialized-view-scope buf) 'partial)))
              (kill-buffer buf))))))))

(ert-deftest dorg-test-vfs-freshness ()
  "AC3: unchanged HEAD verifies clean; a new commit advances the modtime and
content; a @REF-pinned modtime stays stable across the HEAD advance."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-test-vfs--populate dolt-dir "* Alpha\n")
      (let* ((ref0 (dorg-head-commit dolt-dir))
             (t-ref0 (dorg-vfs--head-time dolt-dir ref0))
             (name (format "/dorg:%s:/" dolt-dir)))
        (dorg-test-vfs--with-handler
          (let ((buf (find-file-noselect name)))
            (unwind-protect
                (progn
                  (should (buffer-live-p buf))
                  ;; unchanged HEAD -> verifies clean
                  (should (verify-visited-file-modtime buf))
                  ;; advance HEAD with a strictly-later commit
                  (sleep-for 1.1)
                  (dorg-sql dolt-dir "UPDATE headings SET title = 'Alpha2' WHERE title = 'Alpha'")
                  (dorg-commit dolt-dir "vfs test second commit")
                  ;; HEAD advanced -> buffer reports externally-changed
                  (should-not (verify-visited-file-modtime buf))
                  ;; the resolved-ref pin (ref0) did NOT advance
                  (should (time-less-p t-ref0 (dorg-vfs--head-time dolt-dir)))
                  (should (time-equal-p t-ref0 (dorg-vfs--head-time dolt-dir ref0)))
                  ;; re-render reflects the new content
                  (should (string-match-p "Alpha2"
                                          (dorg-vfs--render (dorg-vfs--parse name)))))
              (when (buffer-live-p buf) (kill-buffer buf)))))))))

(ert-deftest dorg-test-vfs-file-exists-p ()
  "AC5: existence is t for the whole tree and a present node, nil otherwise."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-test-vfs--populate dolt-dir "* Alpha\n")
      (let ((nid (dorg-test-vfs--a-node-id dolt-dir)))
        (dorg-test-vfs--with-handler
          (should (file-exists-p (format "/dorg:%s:/" dolt-dir)))
          (should (file-exists-p (format "/dorg:%s:/%d" dolt-dir nid)))
          (should-not (file-exists-p (format "/dorg:%s:/999999999" dolt-dir)))
          (should-not (file-exists-p "/dorg:/no/such/repo:/")))))))

(ert-deftest dorg-test-vfs-read-only ()
  "AC6: file-writable-p is nil and write-region signals a read-only error."
  (dorg-test-vfs--with-handler
    (should-not (file-writable-p "/dorg:/abs/db:/"))
    (should-error (write-region "x" nil "/dorg:/abs/db:/")
                  :type 'dorg-vfs-error)))

(ert-deftest dorg-test-vfs-delegation-no-crash ()
  "AC7: an unspecialized op on a /dorg: path delegates to the real impl, no crash."
  (dorg-test-vfs--with-handler
    ;; file-name-sans-versions is not in dorg-vfs--ops -> delegates.
    (should (stringp (file-name-sans-versions "/dorg:/abs/db:/100")))
    (should (equal (file-name-sans-versions "/dorg:/abs/db:/100")
                   "/dorg:/abs/db:/100"))))

(ert-deftest dorg-test-vfs-remote-suppresses-lockfile ()
  "Refinement: file-remote-p is non-nil (suppresses lockfiles/backups/VC)."
  (dorg-test-vfs--with-handler
    (should (file-remote-p "/dorg:/abs/db:/"))
    (should (equal (file-remote-p "/dorg:/abs/db:/" 'method) "dorg"))
    (should (equal (file-remote-p "/dorg:/abs/db:/" 'host) "/abs/db"))
    (should-not (vc-registered "/dorg:/abs/db:/"))))

(ert-deftest dorg-test-vfs-name-does-not-enter-plain-writer-gate ()
  "The remote-looking /dorg: name retains Dolt dispatch without a file gate."
  (dorg-test-vfs--with-handler
    (let* ((path "/dorg:/abs/db:/100")
           (source (dorg-mutate--source-make
                    :kind 'dolt :file path :dolt-dir "/abs/db/" :root-id 100)))
      (should (file-remote-p path))
      (cl-letf (((symbol-function 'dorg-file-lock-call)
                 (lambda (&rest _) (ert-fail "VFS must not enter file gate"))))
        (should (eq :dolt
                    (dorg-mutate--with-saved-plain-file-gate
                     source 'apply '(:save t) (lambda () :dolt))))))))

;;; ─── Regression guard (AC4) — real paths are untouched ──────────────

(ert-deftest dorg-test-vfs-regression-real-files-untouched ()
  "AC4: opening real on-disk files is byte-for-byte identical with the handler
installed vs not — the anchored regexp never matches a real path."
  (let* ((dir (make-temp-file "dorg-vfs-reg-" t))
         (plain (expand-file-name "plain.org" dir))
         (kw (expand-file-name "kw.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file plain (insert "* Plain heading\nbody\n"))
          (with-temp-file kw (insert "#+DORG_DOLT_DIR: ./db\n* Bound heading\n"))
          (dolist (f (list plain kw))
            (let* ((without (with-temp-buffer (insert-file-contents f) (buffer-string)))
                   (with (dorg-test-vfs--with-handler
                           (with-temp-buffer (insert-file-contents f) (buffer-string)))))
              (should (equal without with)))))
      (delete-directory dir t))))

(provide 'dorg-vfs-tests)
;;; dorg-vfs-tests.el ends here
