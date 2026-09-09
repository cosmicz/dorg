;;; dorg-mode-tests.el --- Tests for dorg-mode UX features -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for interactive register, dorg-setup, global-dorg-mode, and
;; read-only projection activation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-mode)
(require 'dorg-dolt)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

(defconst dorg-mode-tests--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root captured from this test file, never implicit cwd.")

(defun dorg-mode-tests--source (file)
  "Return repository FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file dorg-mode-tests--repo-dir))
    (buffer-string)))

;;; Interactive Register

(ert-deftest dorg-test-register-is-interactive ()
  "`dorg-bind-dolt' is an interactive command."
  (should (commandp 'dorg-bind-dolt)))

(ert-deftest dorg-test-register-inserts-keyword ()
  "Interactive `dorg-bind-dolt' inserts #+DORG_DOLT_DIR: keyword."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (org-mode)
          (insert "* Hello\n")
          (set-visited-file-name org-file t)
          ;; Simulate interactive call
          (dorg-bind-dolt org-file dolt-dir)
          (dorg-bind-dolt--insert-keyword
           (file-name-as-directory (file-truename dolt-dir)))
          (goto-char (point-min))
          (should (looking-at-p "^#\\+DORG_DOLT_DIR:")))))))

(ert-deftest dorg-test-register-keyword-idempotent ()
  "Second call to insert keyword does not duplicate it."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (org-mode)
          (insert "* Hello\n")
          (set-visited-file-name org-file t)
          (let ((abs-dir (file-name-as-directory (file-truename dolt-dir))))
            (dorg-bind-dolt org-file dolt-dir)
            (dorg-bind-dolt--insert-keyword abs-dir)
            (dorg-bind-dolt--insert-keyword abs-dir)
            (goto-char (point-min))
            (let ((count 0))
              (while (re-search-forward "^#\\+DORG_DOLT_DIR:" nil t)
                (cl-incf count))
              (should (= count 1)))))))))

(ert-deftest dorg-test-register-noninteractive-unchanged ()
  "Non-interactive `dorg-bind-dolt' does not insert keyword or enable mode."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (org-mode)
          (insert "* Hello\n")
          (set-visited-file-name org-file t)
          (dorg-bind-dolt org-file dolt-dir)
          ;; No keyword inserted
          (goto-char (point-min))
          (should-not (re-search-forward "^#\\+DORG_DOLT_DIR:" nil t))
          ;; Mode not enabled
          (should-not dorg-mode))))))

;;; Setup

(ert-deftest dorg-test-setup-is-interactive ()
  "`dorg-setup' is an interactive command."
  (should (commandp 'dorg-setup)))

(ert-deftest dorg-test-setup-full-flow ()
  "`dorg-setup' imports, binds, exact-materializes, and enables the projection."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (org-mode)
          (insert "* Setup Test\n")
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (dorg-setup dolt-dir)
          ;; Keyword inserted
          (goto-char (point-min))
          (should (re-search-forward "^#\\+DORG_DOLT_DIR:" nil t))
          ;; Mode enabled
          (should dorg-mode)
          ;; Registration exists
          (should (dorg--classify-registration))
          ;; Content synced to DB
          (let* ((reg (dorg--classify-registration))
                 (root-id (dorg-backend-root-id (dorg-registration-backend reg)))
                 (rows (dorg-sql dolt-dir
                         (format "SELECT title FROM headings WHERE root_id = %d AND level > 0"
                                 root-id))))
            (should (= (length rows) 1))
            (should (equal (alist-get 'title (car rows))
                           "Setup Test"))))))))

(ert-deftest dorg-test-setup-orders-explicit-import-before-projection-enable ()
  "`dorg-setup' never relies on mode-enable sync for first ingest."
  (dorg-test-with-temp-dir
    (let* ((org-file (expand-file-name "doc.org" default-directory))
           (dolt-dir (expand-file-name "db/" default-directory))
           (backend (dorg-backend-dolt dolt-dir 77))
           (reg (make-dorg-registration :backend backend :file org-file))
           (events nil))
      (with-temp-buffer
        (org-mode)
        (insert "* One\n")
        (set-visited-file-name org-file t)
        (cl-letf (((symbol-function 'dorg-dolt--ensure-available)
                   (lambda () t))
                  ((symbol-function 'dorg-import-document)
                   (lambda (&rest _)
                     (push 'import events)
                     (list :ok t :root-id 77 :commit "new-head")))
                  ((symbol-function 'dorg-bind-dolt)
                   (lambda (&rest _)
                     (push 'bind events)
                     reg))
                  ((symbol-function 'dorg-materialize)
                   (lambda (&rest args)
                     (push (list 'materialize (plist-get (cdr args) :revision))
                           events)))
                  ((symbol-function 'save-buffer)
                   (lambda (&rest _) (push 'save events)))
                  ((symbol-function 'dorg-mode)
                   (lambda (&optional arg)
                     (should (= arg 1))
                     (push 'enable events)))
                  ((symbol-function 'dorg-setup--heading-count)
                   (lambda (&rest _) 1)))
          (dorg-setup dolt-dir)
          (should
           (equal (nreverse events)
                  '(import bind (materialize "new-head") save enable))))))))

;;; Global Mode

(ert-deftest dorg-test-global-mode-toggles ()
  "`global-dorg-mode' can be toggled."
  (unwind-protect
      (progn
        (global-dorg-mode 1)
        (should global-dorg-mode)
        (should (memq 'dorg--maybe-enable-mode org-mode-hook)))
    (global-dorg-mode -1))
  (should-not global-dorg-mode)
  (should-not (memq 'dorg--maybe-enable-mode org-mode-hook)))

(ert-deftest dorg-test-global-mode-activates-dorg ()
  "`global-dorg-mode' enables `dorg-mode' on buffers with #+DORG_DOLT_DIR:."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (global-dorg-mode 1)
        (unwind-protect
            (with-temp-buffer
              (insert (format "#+DORG_DOLT_DIR: %s\n* Auto\n" dolt-dir))
              (set-visited-file-name org-file t)
              (org-mode)
              ;; org-mode-hook should have triggered dorg--maybe-enable-mode
              (should dorg-mode))
          (global-dorg-mode -1))))))

(ert-deftest dorg-test-global-mode-skips-plain-org ()
  "`global-dorg-mode' does not enable `dorg-mode' without #+DORG_DOLT_DIR:."
  (unwind-protect
      (progn
        (global-dorg-mode 1)
        (with-temp-buffer
          (insert "* Plain org\n")
          (org-mode)
          (should-not dorg-mode)))
    (global-dorg-mode -1)))

;;; Auto Server

(ert-deftest dorg-test-dolt-auto-server-starts-on-mode-enable ()
  "`dorg-mode' starts a server when `dorg-dolt-auto-server' is non-nil."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil)
             (dorg-dolt-auto-server t))
        (dorg-bind-dolt org-file dolt-dir)
        (with-temp-buffer
          (org-mode)
          (insert "* Test\n")
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (dorg-mode 1)
          (unwind-protect
              (should (dorg-dolt-server-port dolt-dir))
            (dorg-dolt-server--stop dolt-dir)))))))

(ert-deftest dorg-test-dolt-auto-server-respects-flag ()
  "`dorg-mode' skips server start when `dorg-dolt-auto-server' is nil."
  (let* ((dorg--registry nil)
         (dorg-dolt-auto-server nil)
         (ensure-called nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration :backend (dorg-backend-dolt "/tmp/fake/" 1))))
              ((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (_dir) (setq ensure-called t) 3307)))
      (with-temp-buffer
        (org-mode)
        (dorg-mode 1)
        (should-not ensure-called)))))

(ert-deftest dorg-test-dolt-auto-server-failure-warns ()
  "Server failure during mode enable warns but does not block."
  (let* ((dorg--registry nil)
         (dorg-dolt-auto-server t)
         (warned nil))
    ;; Fake a registration so ensure-server fires
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration :backend (dorg-backend-dolt "/tmp/nonexistent/" 1))))
              ((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (_dir) (error "dolt not found")))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _args)
                 (when (string-match-p "Auto-server failed" msg)
                   (setq warned t)))))
      (with-temp-buffer
        (org-mode)
        (dorg-mode 1)
        (should warned)
        (should dorg-mode)))))

(ert-deftest dorg-test-ensure-dolt-server-delegates-to-strict ()
  "`dorg--ensure-dolt-server' acquires via the strict session selector.
After the legacy non-strict `dorg-dolt-server--ensure' is retired,
interactive mode activation must route through the sole session-owned
acquire `dorg-dolt-server--ensure-strict'.  The legacy selector is stubbed
to signal so a regression that reintroduces it as the caller fails here."
  (let ((dorg-dolt-auto-server t)
        (strict-dir nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf) t))
              ((symbol-function 'dorg-registered-dolt-dir)
               (lambda (&rest _) "/tmp/mmgen-delegate/"))
              ((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (dir) (setq strict-dir dir) 3307))
              ((symbol-function 'dorg-dolt-server--ensure)
               (lambda (&rest _) (error "legacy non-strict selector must not be called")))
              ((symbol-function 'display-warning)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (dorg--ensure-dolt-server))
      ;; The strict selector was called with the registered store dir.
      (should (equal strict-dir "/tmp/mmgen-delegate/")))))

(ert-deftest dorg-test-ensure-dolt-server-swallows-strict-signal ()
  "Strict acquisition errors during mode enable warn but never block activation.
Consolidating session-owned acquire on `dorg-dolt-server--ensure-strict' must
preserve the non-blocking contract: `dorg--ensure-dolt-server' catches an
acquisition signal from the strict selector, emits the `Auto-server failed'
warning, and leaves `dorg-mode' enabled.  The LEGACY non-strict
`dorg-dolt-server--ensure' is stubbed to SUCCEED, so a caller that has not
migrated silently starts a server, emits no warning, and fails this test —
proving mode activation now routes its fail-closed handling through the strict
selector.  RED now (mode enable still calls `--ensure', which succeeds, so no
warning fires); GREEN once the caller migrates."
  (let* ((dorg--registry nil)
         (dorg-dolt-auto-server t)
         (warned nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration :backend (dorg-backend-dolt "/tmp/nonexistent/" 1))))
              ((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (&rest _) (error "strict acquire signaled")))
              ((symbol-function 'dorg-dolt-server--ensure)
               (lambda (&rest _) 3307))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _args)
                 (when (string-match-p "Auto-server failed" msg)
                   (setq warned t)))))
      (with-temp-buffer
        (org-mode)
        (dorg-mode 1)
        (should warned)
        (should dorg-mode)))))

(ert-deftest dorg-test-mode-enable-nonblocking-under-held-claim ()
  "Enabling `dorg-mode' classifies cheaply when the claim lock is held.

Regression for tracked-case: passive mode activation used to classify the
buffer via `dorg--current-backend-plain-org-p' ->
`dorg--classify-registration' ->
`dorg--auto-bind-dolt' -> `dorg-bind-dolt' -> `dorg-repo-ensure-schema',
running strict, lock-taking schema/root SQL on the critical activation
path.  Under `server' transport with a live holder owning
`<store>/.dorg/server.claim.lock', that SQL blocked for the full
`dorg-dolt-server-claim-lock-timeout' and then signalled
`dorg-sql-transport-error' OUT of `dorg-mode' enable (classification runs
before the `condition-case' wrappers in `dorg--ensure-dolt-server' /
`dorg--sync-on-enable').  Passive activation must classify from the cheap
preamble keyword with no SQL, never wait the full lock timeout, and never
abort mode enable.

The buffer carries ONLY the `#+DORG_DOLT_DIR:' keyword with an empty
`dorg--registry', so classification takes the transient keyword-Dolt path
\(the real fresh-open case), not a pre-stored registration.  After this
bead that pure classification allocates no root and takes no lock; the
test asserts activation stays cheap regardless."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (require 'dorg-dolt-server)
    (let ((dorg-test--server-port nil)      ; standalone copy-repo store, no shared server
          (dorg-test--fallback-cli-seam nil)) ; drive real `server' transport, not the CLI seam
      (dorg-test--with-temp-db-and-dir temp-base dolt-dir
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (lock (concat (dorg-dolt-server--claim-file dolt-dir) ".lock"))
               (dorg--registry nil)                       ; force transient keyword-Dolt classification
               (dorg-sql-transport 'server)               ; route SQL through the managed server
               (dorg-dolt-server-claim-lock-timeout 0.3)  ; small per AC
               (dorg-dolt-auto-server nil))               ; isolate classification from server lifecycle
          ;; A live holder owns the claim lock (current pid => never reaped stale),
          ;; so any lock-taking SQL on the activation path blocks then signals.
          (make-directory (dorg-dolt-server--store-dorg-dir dolt-dir) t)
          (make-directory lock t)
          (dorg-dolt-server--lock-write-meta lock)
          (unwind-protect
              (with-temp-buffer
                (org-mode)
                (insert (format "#+DORG_DOLT_DIR: %s\n* Test\n" dolt-dir))
                (set-visited-file-name org-file t)
                (set-buffer-modified-p nil)
                (let ((start (float-time))
                      (enabled nil)
                      (err nil))
                  (condition-case e
                      (progn (dorg-mode 1)
                             (setq enabled dorg-mode))
                    (error (setq err e)))
                  (let ((elapsed (- (float-time) start)))
                    ;; No signal escaped `dorg-mode' enable.
                    (should (null err))
                    ;; The mode actually enabled.
                    (should enabled)
                    ;; Classification was cheap: nowhere near the default 10s timeout.
                    (should (< elapsed 2.0)))))
            (ignore-errors (delete-directory lock t))))))))

(ert-deftest dorg-test-mode-enable-default-path-nonblocking-under-held-claim ()
  "Default `dorg-mode' activation stays prompt when the claim lock is held.

Regression for tracked-case: mode activation may ensure server readiness, but
must not synchronously wait the configured claim-lock timeout.  Projection
activation performs no whole-buffer reconciliation."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (require 'dorg-dolt-server)
    (let ((dorg-test--server-port nil)      ; standalone copy-repo store, no shared server
          (dorg-test--fallback-cli-seam nil)) ; drive real `server' transport, not the CLI seam
      (dorg-test--with-temp-db-and-dir temp-base dolt-dir
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (lock (concat (dorg-dolt-server--claim-file dolt-dir) ".lock"))
               (dorg--registry nil)
               (dorg-sql-transport 'server)
               (dorg-dolt-server-claim-lock-timeout 3.0)  ; moderate operation timeout
               (dorg-dolt-auto-server t)                  ; DEFAULT — server ensured on enable
               (warnings '()))
          (make-directory (dorg-dolt-server--store-dorg-dir dolt-dir) t)
          (make-directory lock t)
          (dorg-dolt-server--lock-write-meta lock)        ; live holder — never reaped stale
          (unwind-protect
              (cl-letf (((symbol-function 'display-warning)
                         (lambda (_type _msg &rest _args)
                           (push _msg warnings))))
                (with-temp-buffer
                  (org-mode)
                  (insert (format "#+DORG_DOLT_DIR: %s\n* Test\n" dolt-dir))
                  (set-visited-file-name org-file t)
                  (set-buffer-modified-p nil)
                  (let ((start (float-time))
                        (enabled nil)
                        (err nil))
                    (condition-case e
                        (progn (dorg-mode 1)
                               (setq enabled dorg-mode))
                      (error (setq err e)))
                    (let ((elapsed (- (float-time) start)))
                      ;; No signal escaped `dorg-mode' enable.
                      (should (null err))
                      ;; The mode actually enabled.
                      (should enabled)
                      ;; Bounded/deferred: did NOT wait the full operation timeout.
                      (should (< elapsed 1.5))
                      ;; Readiness under contention degraded to a warning, not a hang.
                      (should warnings))
                    (dorg-mode -1))))
            (ignore-errors (delete-directory lock t))))))))

;;; ─── Interactive Dolt Projection Safety ───────────────────────────

(ert-deftest dorg-test-mode-interactive-dolt-buffer-is-read-only ()
  "Interactive Dolt-backed buffers reject ordinary text edits."
  (let ((dorg--registry nil)
        (dorg-dolt-auto-server nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration
                  :backend (dorg-backend-dolt "/tmp/fake/" 1)))))
      (with-temp-buffer
        (insert "* Projection\n")
        (org-mode)
        (dorg-mode 1)
        (unwind-protect
            (progn
              (should buffer-read-only)
              (should-error (insert "ordinary edit")
                            :type 'buffer-read-only))
          (dorg-mode -1))
        ;; The Dolt binding, not minor-mode state, owns projection safety.
        (should buffer-read-only)))))

(ert-deftest dorg-test-org-mode-hook-protects-dolt-bound-buffer ()
  "Visiting a Dolt-bound Org file is read-only without enabling `dorg-mode'."
  (let ((noninteractive nil))
    (with-temp-buffer
      (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
      (set-visited-file-name
       (expand-file-name "hook-projection.org" temporary-file-directory) t)
      (org-mode)
      (should-not dorg-mode)
      (should buffer-read-only)
      (should-error (insert "ordinary edit") :type 'buffer-read-only))))

(ert-deftest dorg-test-projection-policy-refuses-edit-after-manual-unlock ()
  "Clearing read-only does not turn a Dolt projection into an authoring buffer."
  (let ((noninteractive nil))
    (with-temp-buffer
      (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
      (set-visited-file-name
       (expand-file-name "unlock-projection.org" temporary-file-directory) t)
      (org-mode)
      (setq buffer-read-only nil)
      (should-error (insert "ordinary edit") :type 'user-error)
      (should-not (string-match-p "ordinary edit" (buffer-string))))))

(ert-deftest dorg-test-projection-policy-follows-late-explicit-binding ()
  "A binding installed after `org-mode' started is protected dynamically."
  (let ((dorg--registry nil)
        (noninteractive nil))
    (with-temp-buffer
      (let ((file (expand-file-name "late-bound.org" temporary-file-directory)))
        (set-visited-file-name file t)
        (org-mode)
        (insert "* Writable before binding\n")
        (push (cons (file-truename file)
                    (make-dorg-registration
                     :backend (dorg-backend-dolt "/tmp/fake/" 7)
                     :file (file-truename file)))
              dorg--registry)
        (should-error (insert "blocked after binding") :type 'user-error)
        (should-not (string-match-p "blocked after binding" (buffer-string)))))))

(ert-deftest dorg-test-projection-policy-refuses-direct-save ()
  "Saving a Dolt projection refuses instead of reconciling its whole buffer."
  (let ((noninteractive nil))
    (with-temp-buffer
      (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
      (set-visited-file-name
       (expand-file-name "save-projection.org" temporary-file-directory) t)
      (org-mode)
      ;; Seed the wrong state without exercising the edit guard.  The save
      ;; refusal itself is the behavior under test.
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (goto-char (point-max))
        (insert "changed\n"))
      (should-error (run-hooks 'before-save-hook) :type 'user-error))))

(ert-deftest dorg-test-projection-policy-after-revert-restores-read-only ()
  "The projection policy repairs read-only state after a revert lifecycle."
  (let ((noninteractive nil))
    (with-temp-buffer
      (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
      (set-visited-file-name
       (expand-file-name "revert-projection.org" temporary-file-directory) t)
      (org-mode)
      (setq buffer-read-only nil)
      (run-hooks 'after-revert-hook)
      (should buffer-read-only))))

(ert-deftest dorg-test-projection-policy-allows-owned-byte-replacement ()
  "The existing programmatic-change guard remains the sole edit capability."
  (let ((noninteractive nil))
    (with-temp-buffer
      (insert "#+DORG_DOLT_DIR: /tmp/fake/\n* Projection\n")
      (set-visited-file-name
       (expand-file-name "owned-projection.org" temporary-file-directory) t)
      (org-mode)
      (setq buffer-read-only nil)
      (dorg-projection-with-programmatic-changes
        (goto-char (point-max))
        (insert "owned replacement\n"))
      (should (string-match-p "owned replacement" (buffer-string))))))

(ert-deftest dorg-test-mode-removes-whole-buffer-authoring-surface ()
  "The mode exposes refresh, not configurable reconciliation entry points."
  (dolist (symbol '(dorg-sync-on-save dorg-sync-on-enable
                    dorg--before-save dorg--sync-on-enable
                    dorg--track-dirty-heading))
    (should-not (boundp symbol))
    (should-not (fboundp symbol)))
  (should (eq (lookup-key dorg-mode-map (kbd "C-c C-x s"))
              #'dorg-refresh-materialized-view)))

(ert-deftest dorg-test-mode-consolidates-refresh-and-strong-deletes-polling ()
  "The supported projection lifecycle is exact explicit refresh, not polling.
This filesystem-backed guard is RED while mode still requires sync/async,
calls surgical pull, or defines the automatic poll surface.  It also refuses
a second public `dorg-refresh' spelling."
  (let ((mode (dorg-mode-tests--source "dorg-mode.el"))
        (top (dorg-mode-tests--source "dorg.el"))
        (configuration (dorg-mode-tests--source "docs/configuration.org")))
    (dolist (symbol '(dorg-poll-interval
                      dorg--poll-timer
                      dorg--poll-for-changes
                      dorg--start-poll-timer
                      dorg--stop-poll-timer))
      (should-not (boundp symbol))
      (should-not (fboundp symbol)))
    (should-not (string-match-p
                 "^[ \\t]*(require 'dorg-sync\\(?:-[-a-z]+\\)?)" mode))
    (should-not (string-match-p "\\_<dorg-sync-pull\\_>" mode))
    (should-not (string-match-p "\\_<dorg-sync-async-\\(?:start\\|cancel\\)\\_>"
                                mode))
    (should-not (string-match-p
                 "^[ \\t]*(require 'dorg-sync\\(?:-state\\)?)" top))
    (should-not (string-match-p
                 "^(defun[ \\t]+dorg-refresh[ \\t\\n)]" mode))
    (should-not (string-match-p "=dorg-poll-interval=" configuration))
    (should (eq (lookup-key dorg-mode-map (kbd "C-c C-x s"))
                #'dorg-refresh-materialized-view))))

(ert-deftest dorg-test-materialize-dolt-legacy-routes-to-exact-whole-view ()
  "Legacy Dolt materialize captures HEAD once and renders the exact whole view."
  (let ((buf (generate-new-buffer " *dorg-materialize-exact-legacy*"))
        (backend (dorg-backend-dolt "/tmp/dorg-exact-legacy/" 77))
        (head-reads 0)
        seen)
    (unwind-protect
        (let ((reg (make-dorg-registration :backend backend
                                           :file "/tmp/view.org")))
          (with-current-buffer buf
            (insert "#+DORG_HASH: old-rev\n")
            (org-mode)
            (set-buffer-modified-p nil))
          (cl-letf (((symbol-function 'dorg-head-commit)
                     (lambda (_dir)
                       (setq head-reads (1+ head-reads))
                       "exact-rev"))
                    ((symbol-function 'dorg-materialize--view-whole)
                     (lambda (&rest args)
                       (setq seen args)
                       '(:scope whole :revision "exact-rev"))))
            (should (equal (dorg-materialize--dolt buf reg)
                           "/tmp/dorg-exact-legacy/")))
          (should (= head-reads 1))
          (should (equal seen
                         (list buf buf "/tmp/dorg-exact-legacy/" 77
                               "exact-rev"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest dorg-test-refresh-materialized-view-preserves-whole-exact-revision ()
  "Refresh reuses the established source/root/scope at the supplied token."
  (let ((source (generate-new-buffer " *dorg-refresh-source*"))
        (dest (generate-new-buffer " *dorg-refresh-dest*"))
        (dorg--registry nil)
        seen)
    (unwind-protect
        (let ((file (expand-file-name "refresh-source.org"
                                      temporary-file-directory)))
          (with-current-buffer source
            (set-visited-file-name file t)
            (org-mode))
          (push (cons (file-truename file)
                      (make-dorg-registration
                       :backend (dorg-backend-dolt "/tmp/fake/" 7)
                       :file (file-truename file)))
                dorg--registry)
          (with-current-buffer dest (org-mode))
          (dorg-projection-install-view
           dest source 7 "commit-1" 'whole nil)
          (cl-letf (((symbol-function 'dorg-materialize)
                     (lambda (&rest args)
                       (setq seen args)
                       '(:scope whole :revision "commit-2"))))
            (should (equal '(:scope whole :revision "commit-2")
                           (dorg-refresh-materialized-view dest "commit-2"))))
          (should (eq dest (car seen)))
          (should (eq source (plist-get (cdr seen) :source)))
          (should (equal "commit-2" (plist-get (cdr seen) :revision)))
          (should-not (plist-member (cdr seen) :window)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p dest) (kill-buffer dest)))))

(ert-deftest dorg-test-refresh-materialized-view-preserves-normalized-window ()
  "A partial refresh reconstructs the exact established focus window."
  (let ((source (generate-new-buffer " *dorg-refresh-window-source*"))
        (dest (generate-new-buffer " *dorg-refresh-window-dest*"))
        (dorg--registry nil)
        seen)
    (unwind-protect
        (let ((file (expand-file-name "refresh-window.org"
                                      temporary-file-directory))
              (targets '(100 (:extern :key "BEAD_ID" :value "x"))))
          (with-current-buffer source
            (set-visited-file-name file t)
            (org-mode))
          (push (cons (file-truename file)
                      (make-dorg-registration
                       :backend (dorg-backend-dolt "/tmp/fake/" 7)
                       :file (file-truename file)))
                dorg--registry)
          (with-current-buffer dest (org-mode))
          (dorg-projection-install-view
           dest source 7 "commit-1" 'partial targets)
          (cl-letf (((symbol-function 'dorg-materialize)
                     (lambda (&rest args)
                       (setq seen args)
                       '(:scope partial :revision "commit-2"))))
            (dorg-refresh-materialized-view dest "commit-2"))
          (should (equal (list :focus targets)
                         (plist-get (cdr seen) :window))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p dest) (kill-buffer dest)))))

(ert-deftest dorg-test-refresh-materialized-view-refuses-stale-root-before-work ()
  "A view/root mismatch refuses before invoking materialization."
  (let ((source (generate-new-buffer " *dorg-refresh-stale-source*"))
        (dest (generate-new-buffer " *dorg-refresh-stale-dest*"))
        (dorg--registry nil)
        (materialized nil))
    (unwind-protect
        (let ((file (expand-file-name "refresh-stale.org"
                                      temporary-file-directory)))
          (with-current-buffer source
            (set-visited-file-name file t)
            (org-mode))
          (push (cons (file-truename file)
                      (make-dorg-registration
                       :backend (dorg-backend-dolt "/tmp/fake/" 8)
                       :file (file-truename file)))
                dorg--registry)
          (with-current-buffer dest (org-mode))
          (dorg-projection-install-view
           dest source 7 "commit-1" 'whole nil)
          (cl-letf (((symbol-function 'dorg-materialize)
                     (lambda (&rest _)
                       (setq materialized t))))
            (should-error (dorg-refresh-materialized-view dest "commit-2")
                          :type 'user-error))
          (should-not materialized))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p dest) (kill-buffer dest)))))

(ert-deftest dorg-test-mode-interactive-plain-org-remains-writable ()
  "Interactive plain-org buffers remain ordinary editable Org files."
  (let ((dorg--registry nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration :backend (dorg-backend-plain-org)))))
      (with-temp-buffer
        (org-mode)
        (dorg-mode 1)
        (unwind-protect
            (progn
              (should-not buffer-read-only)
              (insert "plain edit")
              (should (equal (buffer-string) "plain edit")))
          (dorg-mode -1))))))

(ert-deftest dorg-test-mode-batch-dolt-buffer-requires-owned-writes ()
  "Batch Dolt projections obey the same capability boundary as interactive ones."
  (let ((dorg--registry nil)
        (dorg-dolt-auto-server nil)
        (noninteractive t))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration
                  :backend (dorg-backend-dolt "/tmp/fake/" 1)))))
      (with-temp-buffer
        (org-mode)
        (dorg-mode 1)
        (unwind-protect
            (progn
              (should buffer-read-only)
              (setq buffer-read-only nil)
              (should-error (insert "unowned batch edit") :type 'user-error)
              (dorg-projection-with-programmatic-changes
                (insert "owned batch edit"))
              (should (equal (buffer-string) "owned batch edit")))
          (dorg-mode -1))))))

(ert-deftest dorg-test-mode-disable-preserves-preexisting-read-only ()
  "Disabling DOrg does not clear read-only state that DOrg did not acquire."
  (let ((dorg--registry nil)
        (dorg-dolt-auto-server nil)
        (noninteractive nil))
    (cl-letf (((symbol-function 'dorg--classify-registration)
               (lambda (&optional _buf)
                 (make-dorg-registration
                  :backend (dorg-backend-dolt "/tmp/fake/" 1)))))
      (with-temp-buffer
        (org-mode)
        (setq buffer-read-only t)
        (dorg-mode 1)
        (should buffer-read-only)
        (dorg-mode -1)
        (should buffer-read-only)))))

;;; Dolt-dir Keyword Soft Migration

(ert-deftest dorg-test-buffer-dolt-dir-reads-dolt-dir-keyword ()
  "`dorg--buffer-dolt-dir' reads the preferred `#+DORG_DOLT_DIR:' keyword."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DOLT_DIR: /tmp/my-dolt\n* H\n")
    (should (equal (dorg--buffer-dolt-dir)
                   (expand-file-name "/tmp/my-dolt")))))

(ert-deftest dorg-test-buffer-dolt-dir-legacy-fallback ()
  "`dorg--buffer-dolt-dir' falls back to the legacy `#+DORG_DIR:' keyword."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DIR: /tmp/legacy-dolt\n* H\n")
    (should (equal (dorg--buffer-dolt-dir)
                   (expand-file-name "/tmp/legacy-dolt")))))

(ert-deftest dorg-test-buffer-dolt-dir-prefers-dolt-dir-over-legacy ()
  "`#+DORG_DOLT_DIR:' takes precedence over a legacy `#+DORG_DIR:' keyword."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DOLT_DIR: /tmp/new\n#+DORG_DIR: /tmp/old\n* H\n")
    (should (equal (dorg--buffer-dolt-dir)
                   (expand-file-name "/tmp/new")))))

;;; Dolt-dir Keyword Preamble Restriction
;;
;; `dorg--buffer-dolt-dir' must honor ONLY a true file-preamble keyword.
;; A `#+DORG_DOLT_DIR:' mentioned in a heading body / prose (the live
;; bug: a bead documenting the keyword flipped the tracker to a bogus
;; Dolt binding) or inside an example/src block must be ignored.  These
;; are transformation tests: they set up the WRONG state (keyword outside
;; the preamble) and assert nil, so deleting the preamble-position check
;; makes them fail.

(ert-deftest dorg-test-buffer-dolt-dir-ignores-body-keyword ()
  "A `#+DORG_DOLT_DIR:' in a heading body is NOT honored."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n#+DORG_DOLT_DIR: /tmp/bogus\nmore body\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-buffer-dolt-dir-ignores-indented-body-keyword ()
  "An indented `#+DORG_DOLT_DIR:' in a heading body is NOT honored.
This is the live repro shape from a Dolt-backed tracker file (prose
quoting the keyword)."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n  #+DORG_DOLT_DIR: /tmp/bogus from prose\nmore\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-buffer-dolt-dir-ignores-example-block-body ()
  "A `#+DORG_DOLT_DIR:' inside an example block in the body is ignored."
  (with-temp-buffer
    (org-mode)
    (insert "* H\n#+begin_example\n#+DORG_DOLT_DIR: /tmp/bogus\n#+end_example\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-buffer-dolt-dir-ignores-example-block-preamble ()
  "A `#+DORG_DOLT_DIR:' inside an example block in the preamble is ignored."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_example\n#+DORG_DOLT_DIR: /tmp/bogus\n#+end_example\n* H\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-buffer-dolt-dir-ignores-src-block-preamble ()
  "A `#+DORG_DOLT_DIR:' inside a source block in the preamble is ignored."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src org\n#+DORG_DOLT_DIR: /tmp/bogus\n#+end_src\n* H\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-buffer-dolt-dir-preamble-only-no-heading ()
  "A preamble keyword in a file with no headline is honored."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DOLT_DIR: /tmp/onlyfile\n")
    (should (equal (dorg--buffer-dolt-dir)
                   (expand-file-name "/tmp/onlyfile")))))

(ert-deftest dorg-test-buffer-dolt-dir-empty-value-is-nil ()
  "A preamble keyword with an empty/whitespace value yields nil."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DOLT_DIR:   \n* H\n")
    (should (null (dorg--buffer-dolt-dir)))))

(ert-deftest dorg-test-insert-keyword-writes-dolt-dir-only ()
  "`dorg-bind-dolt--insert-keyword' writes `#+DORG_DOLT_DIR:', never `#+DORG_DIR:'."
  (with-temp-buffer
    (org-mode)
    (insert "* H\n")
    (dorg-bind-dolt--insert-keyword "/tmp/some-dolt/")
    (goto-char (point-min))
    (should (re-search-forward "^#\\+DORG_DOLT_DIR:" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "^#\\+DORG_DIR:" nil t))))

(ert-deftest dorg-test-insert-keyword-migrates-legacy-only-file ()
  "A buffer carrying only a legacy `#+DORG_DIR:' still gets `#+DORG_DOLT_DIR:'.
The insert predicate checks specifically for `#+DORG_DOLT_DIR:', so a
legacy keyword does not suppress the forward-migration write."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DIR: /tmp/legacy/\n* H\n")
    (dorg-bind-dolt--insert-keyword "/tmp/legacy/")
    (goto-char (point-min))
    (should (re-search-forward "^#\\+DORG_DOLT_DIR:" nil t))))

;;; Writer preamble/element safety
;;
;; The "already present" predicate must honor only a GENUINE preamble
;; `#+DORG_DOLT_DIR:' keyword element.  A literal in a heading body, in
;; prose, or inside an example/source block must NOT suppress insertion
;; (the whole-buffer regexp bug that left fresh processes resolving
;; plain-org).  These are transformation tests: set up the WRONG state (a
;; body/block literal, no preamble keyword) and assert the preamble
;; keyword IS written, with the literal preserved.

(defun dorg-test--count-dolt-dir-lines ()
  "Count lines in the current buffer beginning with `#+DORG_DOLT_DIR:'."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^[ \t]*#\\+DORG_DOLT_DIR:" nil t)
        (setq n (1+ n)))
      n)))

(ert-deftest dorg-test-insert-keyword-ignores-body-literal ()
  "A body `#+DORG_DOLT_DIR:' does not suppress the preamble write.
The body literal is preserved (the writer is insert-only at point-min)."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n#+DORG_DOLT_DIR: auto-bind > prose, not a binding\nmore\n")
    (dorg-bind-dolt--insert-keyword "/scratch/dolt")
    ;; preamble keyword written at the top
    (goto-char (point-min))
    (should (looking-at "^#\\+DORG_DOLT_DIR: "))
    ;; a fresh preamble-only read now resolves the dir
    (should (dorg--buffer-dolt-dir))
    ;; the body literal is still present (preserved)
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward "auto-bind > prose, not a binding" nil t)))
    ;; exactly two #+DORG_DOLT_DIR: lines: the new preamble + the body one
    (should (= 2 (dorg-test--count-dolt-dir-lines)))))

(ert-deftest dorg-test-insert-keyword-ignores-example-literal ()
  "A `#+DORG_DOLT_DIR:' inside an example block does not suppress the write."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_example\n#+DORG_DOLT_DIR: /fake\n#+end_example\n* H\n")
    (dorg-bind-dolt--insert-keyword "/scratch/dolt")
    (goto-char (point-min))
    (should (looking-at "^#\\+DORG_DOLT_DIR: "))
    (should (dorg--buffer-dolt-dir))
    ;; the example-block literal is preserved
    (should (save-excursion
              (goto-char (point-min))
              (re-search-forward "#\\+begin_example" nil t)))))

(ert-deftest dorg-test-insert-keyword-ignores-src-literal ()
  "A `#+DORG_DOLT_DIR:' inside a source block does not suppress the write."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src org\n#+DORG_DOLT_DIR: /fake\n#+end_src\n* H\n")
    (dorg-bind-dolt--insert-keyword "/scratch/dolt")
    (goto-char (point-min))
    (should (looking-at "^#\\+DORG_DOLT_DIR: "))
    (should (dorg--buffer-dolt-dir))))

(ert-deftest dorg-test-insert-keyword-idempotent-genuine-preamble ()
  "A genuine preamble `#+DORG_DOLT_DIR:' is not duplicated on repeat bind."
  (with-temp-buffer
    (org-mode)
    (insert "#+DORG_DOLT_DIR: /real/dolt\n* H\n")
    (dorg-bind-dolt--insert-keyword "/real/dolt")
    (dorg-bind-dolt--insert-keyword "/real/dolt")
    (should (= 1 (dorg-test--count-dolt-dir-lines)))))

(ert-deftest dorg-test-preamble-keyword-present-p ()
  "`dorg--preamble-keyword-present-p' is true only for a genuine preamble keyword."
  (with-temp-buffer
    (org-mode) (insert "#+DORG_DOLT_DIR: /x\n* H\n")
    (should (dorg--preamble-keyword-present-p "DORG_DOLT_DIR")))
  (with-temp-buffer
    (org-mode) (insert "* H\n#+DORG_DOLT_DIR: /x\n")
    (should-not (dorg--preamble-keyword-present-p "DORG_DOLT_DIR")))
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_example\n#+DORG_DOLT_DIR: /x\n#+end_example\n* H\n")
    (should-not (dorg--preamble-keyword-present-p "DORG_DOLT_DIR")))
  (with-temp-buffer
    (org-mode) (insert "#+DORG_DIR: /x\n* H\n")
    ;; legacy preamble key is present, but DORG_DOLT_DIR is NOT -> migrate
    (should-not (dorg--preamble-keyword-present-p "DORG_DOLT_DIR"))
    (should (dorg--preamble-keyword-present-p "DORG_DIR"))))

(ert-deftest dorg-test-global-mode-activates-legacy-dorg-dir ()
  "`global-dorg-mode' enables `dorg-mode' on legacy `#+DORG_DIR:' buffers."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (global-dorg-mode 1)
        (unwind-protect
            (with-temp-buffer
              (insert (format "#+DORG_DIR: %s\n* Auto\n" dolt-dir))
              (set-visited-file-name org-file t)
              (org-mode)
              (should dorg-mode))
          (global-dorg-mode -1))))))

(ert-deftest dorg-test-materialize-renders-current-keyword-without-raw-save ()
  "Materialization renders `#+DORG_DOLT_DIR:' but does not publish it.
The canonical projection is sealed read-only and clean, so a subsequent raw
`save-buffer' is a no-op and the legacy file remains unchanged on disk."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (dorg-bind-dolt org-file dolt-dir)
        (with-temp-buffer
          (org-mode)
          (insert (format "#+DORG_DIR: %s\n* Legacy\n" dolt-dir))
          (set-visited-file-name org-file t)
          (let ((inhibit-message t)) (save-buffer))
          (dorg-materialize (current-buffer))
          (should buffer-read-only)
          (should-not (buffer-modified-p))
          (should (string-match-p "^#\\+DORG_DOLT_DIR:" (buffer-string)))
          (should-not (string-match-p "^#\\+DORG_DIR:" (buffer-string)))
          (let ((inhibit-message t)) (save-buffer)))
        (with-temp-buffer
          (insert-file-contents org-file)
          (should-not (string-match-p "^#\\+DORG_DOLT_DIR:" (buffer-string)))
          (should (string-match-p "^#\\+DORG_DIR:" (buffer-string))))))))

;;; First-run setup UX hardening

(ert-deftest dorg-test-setup-eager-dolt-check ()
  "`dorg-setup' aborts up front when Dolt is unavailable, before binding."
  (dorg-test-with-temp-dir
    (let ((org-file (expand-file-name "doc.org" default-directory))
          (bound nil))
      (with-temp-buffer
        (org-mode)
        (insert "* X\n")
        (set-visited-file-name org-file t)
        (set-buffer-modified-p nil)
        (cl-letf (((symbol-function 'dorg-dolt--ensure-available)
                   (lambda () (error "dolt binary not found on PATH")))
                  ((symbol-function 'dorg-bind-dolt)
                   (lambda (&rest _) (setq bound t) nil)))
          (should-error (dorg-setup (expand-file-name "db/" default-directory)))
          ;; Aborted at the eager gate, before any binding/init.
          (should-not bound))))))

(ert-deftest dorg-test-setup-confirms-new-repo-init ()
  "Interactive `dorg-setup' confirms before initializing a new repo.
Declining cancels with a `user-error' and binds/initializes nothing."
  (dorg-test-with-temp-dir
    (let ((db-dir (expand-file-name "db/" default-directory))
          (org-file (expand-file-name "doc.org" default-directory))
          (bound nil))
      (with-temp-buffer
        (org-mode)
        (insert "* X\n")
        (set-visited-file-name org-file t)
        (set-buffer-modified-p nil)
        (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda () t))
                  ((symbol-function 'read-directory-name) (lambda (&rest _) db-dir))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'dorg-bind-dolt)
                   (lambda (&rest _) (setq bound t) nil)))
          (should-error (call-interactively 'dorg-setup) :type 'user-error)
          (should-not bound)
          (should-not (file-exists-p (expand-file-name ".dolt" db-dir))))))))

(ert-deftest dorg-test-setup-success-summary ()
  "`dorg-setup' reports a summary: Dolt dir, heading count, and how to query."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "doc.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (org-mode)
          (insert "* One\n* Two\n")
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          ;; No need for a live sql-server for a message assertion; binding it
          ;; off keeps this test isolated and fast.
          (let ((all (mapconcat
                      #'identity
                      (let ((dorg-dolt-auto-server nil))
                        (dorg-test-with-messages (dorg-setup dolt-dir)))
                      "\n")))
            (should (string-match-p "setup complete" all))
            (should (string-match-p (regexp-quote "Dolt dir:") all))
            ;; 4ca775c routes setup through explicit document import and
            ;; intentionally rewords the summary accordingly.
            (should (string-match-p "2 headings imported" all))
            (should (string-match-p "dorg-query" all))))))))

;;; ─── Interactive Strict-Server Engage (Slice 1) ─────────────────────────
;;
;; On the interactive edit path, after `dorg-mode' successfully ensures a
;; managed server, the buffer's SQL transport should freeze to strict
;; `server' (buffer-local).  The public SQL transport default is already
;; server-only; these tests cover the local freeze and the cases where mode
;; activation must not fail.  These mock the server so they need no live Dolt
;; repo.
;; Interactive cases rebind `noninteractive' to nil.
;; (The packaged default flip to interactive `server' is covered in
;; dorg-transport-tests.el `dorg-test-transport-packaged-default-*'.)

(defmacro dorg-test--with-engage-fixture (ensure-form &rest body)
  "Enable `dorg-mode' on a mocked Dolt registration and run BODY.
ENSURE-FORM is the `dorg-dolt-server--ensure-strict' lambda body (a port, or an error).
Leaves `dorg-sql-transport' at its global default so the engage, if any,
is observable as a buffer-local change.  mysql.el is a HARD dependency present
in every loaded dorg process, so the engage no longer gates on its availability."
  (declare (indent 1))
  `(let* ((dorg--registry nil)
          (dorg-dolt-auto-server t))
     (cl-letf (((symbol-function 'dorg--classify-registration)
                (lambda (&optional _buf)
                  (make-dorg-registration
                   :backend (dorg-backend-dolt "/tmp/fake/" 1))))
               ((symbol-function 'dorg-dolt-server--ensure-strict) ,ensure-form))
       (with-temp-buffer
         (org-mode)
         ,@body))))

(ert-deftest dorg-test-engage-strict-on-interactive-mysql ()
  "Interactive + mysql + successful ensure freezes buffer-local strict `server'."
  (dorg-test--with-engage-fixture (lambda (_dir) 3307)
    (let ((noninteractive nil))
      (dorg-mode 1)
      (should (eq dorg-sql-transport 'server))
      (should (local-variable-p 'dorg-sql-transport))
      (should dorg-mode))))

(ert-deftest dorg-test-engage-is-buffer-local-not-global ()
  "The engage never mutates the global `dorg-sql-transport' default."
  (dorg-test--with-engage-fixture (lambda (_dir) 3307)
    (let ((noninteractive nil))
      (dorg-mode 1)
      (should (eq (default-value 'dorg-sql-transport) 'server)))))

(ert-deftest dorg-test-engage-skips-noninteractive ()
  "Batch/noninteractive enable does NOT add a buffer-local transport."
  (dorg-test--with-engage-fixture (lambda (_dir) 3307)
    (let ((noninteractive t))
      (dorg-mode 1)
      (should (eq dorg-sql-transport 'server))
      (should-not (local-variable-p 'dorg-sql-transport)))))

(ert-deftest dorg-test-engage-skips-on-ensure-failure ()
  "A failed server ensure leaves server transport and still enables the mode."
  (dorg-test--with-engage-fixture (lambda (_dir) (error "dolt not found"))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((noninteractive nil))
        (dorg-mode 1)
        (should (eq dorg-sql-transport 'server))
        (should-not (local-variable-p 'dorg-sql-transport))
        (should dorg-mode)))))

(ert-deftest dorg-test-engage-preserves-global-explicit-server ()
  "An explicit global `server' policy is never overridden by the engage.
Even on the interactive + mysql + successful-ensure path, a user who
chose `server' keeps that policy value."
  (dorg-test--with-engage-fixture (lambda (_dir) 3307)
    (let ((noninteractive nil)
          (dorg-sql-transport 'server))
      (dorg-mode 1)
      (should (eq dorg-sql-transport 'server))
      (should (local-variable-p 'dorg-sql-transport)))))

(ert-deftest dorg-test-engage-preserves-buffer-local-explicit-server ()
  "A preexisting buffer-local `server' policy remains `server'."
  (dorg-test--with-engage-fixture (lambda (_dir) 3307)
    (setq-local dorg-sql-transport 'server)
    (let ((noninteractive nil))
      (dorg-mode 1)
      (should (eq dorg-sql-transport 'server)))))

(provide 'dorg-mode-tests)
;;; dorg-mode-tests.el ends here
