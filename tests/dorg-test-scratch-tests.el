;;; dorg-test-scratch-tests.el --- Restorative test scratch contract  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Executable specification for tracked regression.  The test scratch runner groups
;; every suite's fixtures beneath a pid-owned parent.  Its sweeper is deliberately
;; narrower than a generic tmp cleaner: only well-formed Downstream tracker/DOrg parents with
;; dead owners and sufficient marker age may be removed.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst dorg-test-scratch--project-root
  (file-name-as-directory
   (file-truename
    (expand-file-name ".." (file-name-directory
                             (or load-file-name buffer-file-name)))))
  "Absolute DOrg checkout root captured while this test file loads.")

(defconst dorg-test-scratch--script
  (expand-file-name "tools/test-scratch" dorg-test-scratch--project-root)
  "Absolute path to the shared test scratch runner/sweeper.")

(defconst dorg-test-scratch--marker-name ".test-scratch-owner"
  "Ownership marker basename fixed by the scratch protocol.")

(defun dorg-test-scratch--run (&rest args)
  "Run the scratch executable with ARGS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory dorg-test-scratch--project-root)
          (status (apply #'call-process dorg-test-scratch--script nil t nil args)))
      (cons status (buffer-string)))))

(defun dorg-test-scratch--write-marker
    (candidate namespace owner-pid created-at &optional recorded-root body)
  "Write a protocol marker beneath CANDIDATE.
NAMESPACE, OWNER-PID, CREATED-AT, and optional RECORDED-ROOT populate the
strict fields.  When BODY is non-nil, write it verbatim instead."
  (with-temp-file (expand-file-name dorg-test-scratch--marker-name candidate)
    (if body
        (insert body)
      (let* ((identity (file-attribute-file-identifier
                        (file-attributes candidate)))
             (inode (car identity))
             (device (cadr identity)))
        (insert "format=tracker-test-scratch-v1\n"
                (format "namespace=%s\n" namespace)
                (format "root=%s\n" (or recorded-root
                                        (directory-file-name
                                         (file-truename candidate))))
                (format "owner_pid=%d\n" owner-pid)
                (format "owner_uid=%d\n" (user-uid))
                (format "created_at=%d\n" created-at)
                (format "root_dev=%d\n" device)
                (format "root_ino=%d\n" inode))))))

(defun dorg-test-scratch--candidate
    (base namespace suffix owner-pid created-at &optional recorded-root body)
  "Create and mark a scratch candidate below BASE; return its path."
  (let ((candidate (expand-file-name
                    (format "%s-test-run-%s" namespace suffix) base)))
    (make-directory candidate)
    (dorg-test-scratch--write-marker
     candidate namespace owner-pid created-at recorded-root body)
    candidate))

(defun dorg-test-scratch--entries (base)
  "Return sorted scratch candidate basenames immediately below BASE."
  (sort (directory-files base nil "\\`\\(?:tracker\\|dorg\\)-test-run-")
        #'string<))

(ert-deftest dorg-test-scratch-run-publishes-owned-tmpdir-and-cleans-success ()
  "The command sees a complete marker and successful run cleanup is exact."
  (let ((witness (make-temp-file "dorg-scratch-run-witness-")))
    (unwind-protect
        (pcase-let* ((`(,status . ,output)
                       (dorg-test-scratch--run
                        "run" "dorg" "--" "/bin/sh" "-c"
                        (concat
                         "test -d \"$TMPDIR\" && "
                         "test -f \"$TMPDIR/.test-scratch-owner\" && "
                         "grep -Fx \"namespace=dorg\" \"$TMPDIR/.test-scratch-owner\" && "
                         "canonical_tmp=$(CDPATH='' cd -P \"$TMPDIR\" && pwd -P) && "
                         "grep -Fx \"root=$canonical_tmp\" \"$TMPDIR/.test-scratch-owner\" && "
                         "grep -Fx \"owner_pid=$$\" \"$TMPDIR/.test-scratch-owner\" && "
                         "grep -Ex \"owner_uid=[0-9]+\" \"$TMPDIR/.test-scratch-owner\" && "
                         "grep -Ex \"root_dev=[0-9]+\" \"$TMPDIR/.test-scratch-owner\" && "
                         "grep -Ex \"root_ino=[0-9]+\" \"$TMPDIR/.test-scratch-owner\" && "
                         "printf '%s\\n' \"$TMPDIR\" > \"$1\"")
                        "scratch-probe" witness))
                      (run-root (string-trim
                                 (with-temp-buffer
                                   (insert-file-contents witness)
                                   (buffer-string)))))
          (should (equal 0 status))
          (should (string-match-p "RUN namespace=dorg" output))
          (should (string-match-p "/dorg-test-run-" run-root))
          (should-not (file-exists-p run-root)))
      (when (file-exists-p witness) (delete-file witness)))))

(ert-deftest dorg-test-scratch-run-preserves-failed-run-for-recovery ()
  "A failed command leaves its exact marked parent for later audited recovery."
  (let ((witness (make-temp-file "dorg-scratch-fail-witness-"))
        run-root)
    (unwind-protect
        (pcase-let ((`(,status . ,output)
                     (dorg-test-scratch--run
                      "run" "dorg" "--" "/bin/sh" "-c"
                      "printf '%s\\n' \"$TMPDIR\" > \"$1\"; exit 7"
                      "scratch-probe" witness)))
          (setq run-root
                (string-trim
                 (with-temp-buffer (insert-file-contents witness) (buffer-string))))
          (should (equal 7 status))
          (should (string-match-p "PRESERVE namespace=dorg" output))
          (should (file-directory-p run-root))
          (should (file-regular-p
                   (expand-file-name dorg-test-scratch--marker-name run-root))))
      (when (and run-root (file-exists-p run-root))
        (delete-directory run-root t))
      (when (file-exists-p witness) (delete-file witness)))))

(ert-deftest dorg-test-scratch-sweep-default-is-audited-dry-run ()
  "Default sweep reports an eligible root but does not remove it."
  (let* ((base (make-temp-file "dorg-scratch-scan-" t))
         (candidate (dorg-test-scratch--candidate
                     base "dorg" "dead-old" 2147483647 1)))
    (unwind-protect
        (pcase-let ((`(,status . ,output)
                     (dorg-test-scratch--run
                      "sweep" "--root" base "--min-age-seconds" "0")))
          (should (equal 0 status))
          (should (file-directory-p candidate))
          (should (string-match-p
                   (concat "WOULD_REMOVE .*"
                           (regexp-quote (file-truename candidate))) output))
          (should (string-match-p "SUMMARY mode=dry-run" output)))
      (delete-directory base t))))

(ert-deftest dorg-test-scratch-sweep-apply-transforms-stale-not-live ()
  "Apply removes stale dead-owner state and preserves a live sibling lane."
  (let* ((base (make-temp-file "dorg-scratch-transform-" t))
         (stale (dorg-test-scratch--candidate
                 base "tracker" "dead-old" 2147483647 1))
         (live (dorg-test-scratch--candidate
                base "dorg" "live-old" (emacs-pid) 1))
         (before (dorg-test-scratch--entries base)))
    (unwind-protect
        (pcase-let* ((`(,status . ,output)
                       (dorg-test-scratch--run
                        "sweep" "--root" base "--min-age-seconds" "0"
                        "--apply"))
                      (after (dorg-test-scratch--entries base)))
          (should (equal 0 status))
          (should (member (file-name-nondirectory stale) before))
          (should (member (file-name-nondirectory live) before))
          ;; Transformation witness: removing the sweep call makes this fail.
          (should-not (equal before after))
          (should-not (file-exists-p stale))
          (should (file-directory-p live))
          (should (string-match-p
                   (concat "REMOVED .*" (regexp-quote (file-truename stale)))
                   output))
          (should (string-match-p
                   (concat "KEEP reason=owner-live .*"
                           (regexp-quote (file-truename live))) output)))
      (delete-directory base t))))

(ert-deftest dorg-test-scratch-sweep-preserves-unproven-candidates ()
  "Every missing proof fails closed while an eligible control is removed."
  (let* ((base (make-temp-file "dorg-scratch-unproven-" t))
         (now (truncate (float-time)))
         (young (dorg-test-scratch--candidate
                 base "dorg" "young" 2147483647 now))
         (malformed (dorg-test-scratch--candidate
                     base "dorg" "malformed" 2147483647 1 nil "owner_pid=oops\n"))
         (overflow (dorg-test-scratch--candidate
                    base "dorg" "overflow-age" 2147483647 1))
         (wrong-ns (dorg-test-scratch--candidate
                    base "dorg" "wrong-ns" 2147483647 1 nil nil))
         (path-mismatch (dorg-test-scratch--candidate
                         base "tracker" "wrong-root" 2147483647 1
                         (expand-file-name "elsewhere" base)))
         (markerless (expand-file-name "tracker-test-run-markerless" base))
         (ordinary (expand-file-name "unrelated-test-run-dead" base))
         (eligible (dorg-test-scratch--candidate
                    base "dorg" "eligible" 2147483647 1))
         (symlink-marker-root (dorg-test-scratch--candidate
                               base "tracker" "symlink-marker" 2147483647 1))
         (symlink-marker (expand-file-name dorg-test-scratch--marker-name
                                           symlink-marker-root))
         (foreign-marker (make-temp-file "dorg-scratch-foreign-marker-"))
         (identity-root (dorg-test-scratch--candidate
                         base "dorg" "identity-mismatch" 2147483647 1))
         (identity-marker (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name dorg-test-scratch--marker-name
                                               identity-root))
                            (buffer-string)))
         (identity-original-id
          (file-attribute-file-identifier (file-attributes identity-root)))
         (identity-replacement
          (make-temp-file (expand-file-name "identity-replacement-" base) t))
         (identity-replacement-id
          (file-attribute-file-identifier (file-attributes identity-replacement)))
         (identity-original
          (make-temp-name (expand-file-name "identity-original-" base)))
         (symlink-target (make-temp-file "dorg-scratch-symlink-target-" t))
         (symlink-candidate (expand-file-name "dorg-test-run-symlink" base)))
    (let ((marker (expand-file-name dorg-test-scratch--marker-name overflow)))
      (with-temp-buffer
        (insert-file-contents marker)
        (goto-char (point-min))
        (re-search-forward "^created_at=[0-9]+$")
        (replace-match "created_at=999999999999999999999999999999999999")
        (write-region nil nil marker nil 'silent)))
    (dorg-test-scratch--write-marker wrong-ns "tracker" 2147483647 1)
    (make-directory markerless)
    (make-directory ordinary)
    (delete-file symlink-marker)
    (make-symbolic-link foreign-marker symlink-marker)
    ;; Preserve the original object outside the sweep globs and install a
    ;; replacement allocated while it still existed.  This establishes an
    ;; observed mismatch without assuming delete+recreate changes the inode.
    (rename-file identity-root identity-original)
    (rename-file identity-replacement identity-root)
    (should-not (equal identity-original-id identity-replacement-id))
    (should (equal identity-replacement-id
                   (file-attribute-file-identifier
                    (file-attributes identity-root))))
    (with-temp-file (expand-file-name dorg-test-scratch--marker-name identity-root)
      (insert identity-marker))
    (make-symbolic-link symlink-target symlink-candidate)
    (unwind-protect
        (pcase-let ((`(,status . ,output)
                     (dorg-test-scratch--run
                      "sweep" "--root" base "--min-age-seconds" "3600"
                      "--apply")))
          (should (equal 0 status))
          (dolist (path (list young malformed overflow wrong-ns path-mismatch
                              markerless ordinary symlink-marker-root identity-root
                              identity-original symlink-candidate))
            (should (file-exists-p path)))
          (should-not (file-exists-p eligible))
          (dolist (reason '("too-young" "invalid-marker" "namespace-mismatch"
                            "root-mismatch" "missing-marker" "symlink-marker"
                            "identity-mismatch" "symlink-candidate"))
            (should (string-match-p (concat "reason=" reason) output))))
      (delete-directory base t)
      (delete-file foreign-marker)
      (delete-directory symlink-target t))))

(ert-deftest dorg-test-scratch-make-test-targets-use-runner ()
  "Every supported DOrg test target expands through the scratch runner."
  (dolist (target '("test" "test-fast" "test-medium" "perf" "bench"))
    (with-temp-buffer
      (let ((default-directory dorg-test-scratch--project-root)
            (status (call-process "make" nil t nil "--no-print-directory"
                                  "-n" target)))
        (should (equal 0 status))
        (should (string-match-p
                 "tools/test-scratch run dorg --" (buffer-string)))
        (should (string-match-p
                 "dorg-tests-runner" (buffer-string)))))))

(ert-deftest dorg-test-scratch-sweep-refuses-unsafe-scan-roots ()
  "Relative, missing, symlinked, filesystem-root, and home targets are refused."
  (let* ((base (make-temp-file "dorg-scratch-unsafe-" t))
         (missing (expand-file-name "missing" base))
         (link (expand-file-name "link" base))
         (home (getenv "HOME")))
    (make-symbolic-link base link)
    (unwind-protect
        (dolist (root (list "relative" missing link "/" home))
          (pcase-let ((`(,status . ,output)
                       (dorg-test-scratch--run
                        "sweep" "--root" root "--min-age-seconds" "0"
                        "--apply")))
            (should-not (equal 0 status))
            (should (string-match-p "REFUSE" output))))
      (delete-directory base t))))

(provide 'dorg-test-scratch-tests)
;;; dorg-test-scratch-tests.el ends here
