;;; dorg-dolt-server-tests.el --- Tests for dorg-dolt-server -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for dolt sql-server lifecycle and dual-mode query execution.
;; All tests require dolt binary (skipped when unavailable).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-dolt-server)
(require 'dorg-history)              ; dorg-restore-to-commit (undo path)
(require 'dorg-history-tests)        ; reuse --import/--id-by-title/--title-of/--set-title
(require 'dorg-test-helpers)

;;; ─── Server Lifecycle ──────────────────────────────────────────────────

(ert-deftest dorg-test-dolt-server-start-returns-port ()
  "dorg-dolt-server--start launches a server and returns a port."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (let ((port (dorg-dolt-server--start dolt-dir)))
              (unwind-protect
                  (progn
                    (should (integerp port))
                    (should (> port 0))
                    ;; Process should be alive
                    (should (process-live-p
                             (car (gethash (file-truename dolt-dir)
                                           dorg-dolt-server--procs)))))
                (dorg-dolt-server--stop dolt-dir))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-stop-kills-process ()
  "dorg-dolt-server--stop kills the server and clears registry."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-dolt-server--start dolt-dir)
            (let ((key (file-truename dolt-dir)))
              (should (gethash key dorg-dolt-server--procs))
              (dorg-dolt-server--stop dolt-dir)
              (should-not (gethash key dorg-dolt-server--procs))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-ensure-idempotent ()
  "dorg-dolt-server--ensure-strict returns same port on repeated calls."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (let ((port1 (dorg-dolt-server--ensure-strict dolt-dir))
                  (port2 (dorg-dolt-server--ensure-strict dolt-dir)))
              (unwind-protect
                  (should (= port1 port2))
                (dorg-dolt-server--stop dolt-dir))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-port-nil-when-not-running ()
  "dorg-dolt-server-port returns nil when no server is running."
  (should-not (dorg-dolt-server-port "/tmp/nonexistent-dolt-dir/")))

(ert-deftest dorg-test-dolt-server-dead-process-detected ()
  "Dead server process is detected and cleaned up."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-dolt-server--start dolt-dir)
            (let* ((key (file-truename dolt-dir))
                   (entry (gethash key dorg-dolt-server--procs))
                   (proc (car entry)))
              ;; Kill the process behind the server's back
              (delete-process proc)
              ;; port should now return nil (detects dead process)
              (should-not (dorg-dolt-server-port dolt-dir))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-stop-all ()
  "dorg-dolt-server-stop-all kills all managed servers.
Runs inside `dorg-test--with-dolt-server-runtime' so `stop-all' reaps an
ISOLATED `dorg-dolt-server--procs' — the two servers this test starts,
never the shared fixture server registered in the global table (tracked-case)."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let ((temp1 (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests"))
            (temp2 (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
        (unwind-protect
            (let ((dir1 (dorg-test--copy-repo temp1))
                  (dir2 (dorg-test--copy-repo temp2)))
              (dorg-dolt-server--start dir1)
              (dorg-dolt-server--start dir2)
              (should (> (hash-table-count dorg-dolt-server--procs) 0))
              (dorg-dolt-server-stop-all)
              (should (= (hash-table-count dorg-dolt-server--procs) 0)))
          (dorg-test--delete-owned-root temp1)
          (dorg-test--delete-owned-root temp2))))))

(ert-deftest dorg-test-jmay6-two-default-dorg-servers-stay-disjoint ()
  "Two canonical project stores may serve database `dorg' concurrently.
The live servers have distinct ports, UUIDs, claims, cached connections, and
canonical paths even though their human-facing database names are equal."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let* ((dorg-dolt-server-startup-timeout 120)
             (dorg-sql--connections (make-hash-table :test 'equal))
             (project-a (expand-file-name "project-a" dorg-test--store-root))
             (project-b (expand-file-name "project-b" dorg-test--store-root))
             (parent-a (expand-file-name ".dorg" project-a))
             (parent-b (expand-file-name ".dorg" project-b))
             store-a store-b)
        (unwind-protect
            (progn
              (make-directory parent-a t)
              (make-directory parent-b t)
              (setq store-a (dorg-test--copy-repo parent-a "dorg")
                    store-b (dorg-test--copy-repo parent-b "dorg"))
              (should (equal (file-name-as-directory store-a)
                             (dorg-default-store-directory project-a)))
              (should (equal (file-name-as-directory store-b)
                             (dorg-default-store-directory project-b)))
              (let ((port-a (dorg-dolt-server--start store-a))
                    (port-b (dorg-dolt-server--start store-b)))
                (should (integerp port-a))
                (should (integerp port-b))
                (should-not (= port-a port-b))
                ;; Exercise the managed connection cache against both live
                ;; same-named databases rather than proving only claim shape.
                (should (dorg-sql store-a "SELECT 1 AS ok"))
                (should (dorg-sql store-b "SELECT 1 AS ok"))
                (should (= 2 (hash-table-count dorg-sql--connections)))
                (let ((claim-a (dorg-dolt-server-claim store-a))
                      (claim-b (dorg-dolt-server-claim store-b))
                      (native-a
                       (dorg-dolt-server--dolt-server-info store-a))
                      (native-b
                       (dorg-dolt-server--dolt-server-info store-b)))
                  (should (equal "dorg" (alist-get :db-name claim-a)))
                  (should (equal "dorg" (alist-get :db-name claim-b)))
                  (should-not (equal (alist-get :dolt-dir claim-a)
                                     (alist-get :dolt-dir claim-b)))
                  ;; Session-owned claims need not persist UUID, but the Dolt
                  ;; native server identity always does and is what managed
                  ;; connections capture for reuse validation.
                  (should (stringp (plist-get native-a :uuid)))
                  (should (stringp (plist-get native-b :uuid)))
                  (should-not (equal (plist-get native-a :uuid)
                                     (plist-get native-b :uuid))))))
          (when store-a (ignore-errors (dorg-dolt-server--stop store-a)))
          (when store-b (ignore-errors (dorg-dolt-server--stop store-b))))))))

;;; ─── Crash-safe fatal-signal cleanup (dorg-fatalex, AC #1/#2) ────────────
;;
;; The confirmed Emacs 30.2 failure mode: when SIGTERM interrupts GC, any
;; error or allocation in a `kill-emacs-hook' Lisp handler aborts before it
;; can finish.  So NO DOrg cleanup Lisp may run on the fatal path.  Ordinary
;; interactive exit instead reaps only locally-retained process objects
;; through a NAMED `kill-emacs-query-functions' entry, with every independent
;; seam guarded against both `error' and `quit', and always returns non-nil
;; so it never vetoes exit.

(require 'find-func)

(defun dorg-test-dolt-server--add-hook-forms (source hook)
  "Return every `(add-hook 'HOOK ...)' FORM in SOURCE.
SOURCE is an absolute path to an Elisp file; every registration is
collected (named or anonymous) so a hidden lambda cannot slip past.  The
full form is returned so callers can inspect the target (nth 2) and the
DEPTH argument (nth 3)."
  (let ((max-lisp-eval-depth 10000)
        forms)
    (cl-labels ((walk (form)
                  (when (consp form)
                    (when (and (eq (car-safe form) 'add-hook)
                               (equal (nth 1 form) (list 'quote hook)))
                      (push form forms))
                    (walk (car form))
                    (walk (cdr form)))))
      (with-temp-buffer
        (insert-file-contents source)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (nreverse forms)))

(defun dorg-test-dolt-server--named-target-p (target)
  "Non-nil iff TARGET is a literal named function symbol (#'SYM or 'SYM)."
  (and (memq (car-safe target) '(function quote))
       (symbolp (nth 1 target))
       (not (memq (nth 1 target) '(lambda nil)))))

(ert-deftest dorg-test-dolt-server-no-fatal-kill-emacs-hook ()
  "No DOrg cleanup Lisp is registered on the fatal `kill-emacs-hook'.
Source scan collects EVERY registration (named or anonymous); the fatal
path must carry none from dorg-dolt-server."
  (let ((forms (dorg-test-dolt-server--add-hook-forms
                (find-library-name "dorg-dolt-server") 'kill-emacs-hook)))
    (should (null forms)))
  ;; Runtime: the loaded module left nothing of ours on the fatal hook.
  (should-not (memq 'dorg-dolt-server-stop-all kill-emacs-hook))
  (should-not (memq 'dorg-dolt-server--exit-cleanup kill-emacs-hook)))

(ert-deftest dorg-test-dolt-server-exit-cleanup-named-query-function ()
  "Ordinary-exit cleanup is a NAMED, LATE (depth 99) query-functions entry.
Every registration must be a literal named symbol — an anonymous lambda
cannot satisfy the crash-safe named-cleanup contract — and ours must be
registered at late depth so it runs only after other exit queries agree."
  (let ((forms (dorg-test-dolt-server--add-hook-forms
                (find-library-name "dorg-dolt-server")
                'kill-emacs-query-functions)))
    (should forms)
    (dolist (form forms)
      (should (dorg-test-dolt-server--named-target-p (nth 2 form))))
    (let ((ours (cl-find-if
                 (lambda (f)
                   (eq (nth 1 (nth 2 f)) 'dorg-dolt-server--exit-cleanup))
                 forms)))
      (should ours)
      ;; Late depth 99 (see `run-hook-with-args-until-failure' veto ordering).
      (should (equal (nth 3 ours) 99))))
  ;; Runtime: the named function is on the hook and callable.
  (should (fboundp 'dorg-dolt-server--exit-cleanup))
  (should (memq 'dorg-dolt-server--exit-cleanup kill-emacs-query-functions)))

(ert-deftest dorg-test-dolt-server-exit-cleanup-runs-after-queries-agree ()
  "Late depth 99 means an earlier exit-query veto cancels exit BEFORE the
cleanup ever reaps a local server.  `kill-emacs-query-functions' run under
`run-hook-with-args-until-failure', so a nil from an earlier (lower-depth)
query stops the run without reaching our cleanup."
  (let ((kill-emacs-query-functions nil)
        (ran nil))
    (cl-letf (((symbol-function 'dorg-dolt-server--exit-cleanup)
               (lambda () (setq ran t) t)))
      ;; Our cleanup at the production depth, an earlier query at default 0.
      (add-hook 'kill-emacs-query-functions #'dorg-dolt-server--exit-cleanup 99)
      (add-hook 'kill-emacs-query-functions (lambda () nil))
      ;; Exit is vetoed by the earlier query...
      (should-not (run-hook-with-args-until-failure 'kill-emacs-query-functions))
      ;; ...so our late cleanup never ran.
      (should-not ran))))

(ert-deftest dorg-test-dolt-server-exit-cleanup-seam-isolation ()
  "Ordinary-exit cleanup isolates each seam against `error' AND `quit',
continues later seams, releases only confirmed-dead local claims, and
returns non-nil so it never vetoes exit."
  (let ((dorg-dolt-server--procs (make-hash-table :test 'equal))
        (closed nil)
        (killed nil)
        (released nil))
    ;; proc-a: kill errors, then its exact object is CONFIRMED DEAD.
    ;; proc-b: kill quits, and its exact object SURVIVES (still live).
    (puthash "/dir/a" (cons 'proc-a 3306) dorg-dolt-server--procs)
    (puthash "/dir/b" (cons 'proc-b 3307) dorg-dolt-server--procs)
    (cl-letf (((symbol-function 'dorg-sql--close-all-connections)
               (lambda () (setq closed t) (signal 'quit nil)))
              ((symbol-function 'dorg-dolt-server--kill-process)
               (lambda (proc &rest _)
                 (push proc killed)
                 ;; First seam errors, the other quits — neither may abort.
                 (if (eq proc 'proc-a) (error "kill boom") (signal 'quit nil))))
              ;; Explicit death evidence: only proc-a is provably gone.
              ((symbol-function 'process-live-p)
               (lambda (proc) (eq proc 'proc-b)))
              ((symbol-function 'dorg-dolt-server--release-claim)
               (lambda (dir) (push dir released))))
      (let ((ret (dorg-dolt-server--exit-cleanup)))
        ;; Never veto exit.
        (should ret)
        ;; The connection-close seam ran (its quit did not abort the rest).
        (should closed)
        ;; BOTH retained procs were attempted despite the first erroring.
        (should (memq 'proc-a killed))
        (should (memq 'proc-b killed))
        ;; Confirmed-dead proc-a: its owned claim is released and its registry
        ;; entry pruned.
        (should (member "/dir/a" released))
        (should-not (gethash "/dir/a" dorg-dolt-server--procs))
        ;; Survivor proc-b: NO claim deletion and its registry entry REMAINS
        ;; (fail-closed — never drop a live local object).
        (should-not (member "/dir/b" released))
        (should (gethash "/dir/b" dorg-dolt-server--procs))))))

(ert-deftest dorg-test-dolt-server-exit-cleanup-snapshots-before-kill ()
  "Exit cleanup snapshots `--procs' BEFORE any kill: a process sentinel that
mutates `--procs' while the first kill runs must not skip the second
retained object.  Iterating the live table would drop it.

`maphash' is stubbed to a DETERMINISTIC order (a first, then b only if
still present) so the discriminator is real regardless of native hash
order: a snapshot-first cleanup records both objects before any kill runs;
a live-table cleanup that kills a — removing b — inside the first callback
skips b entirely."
  (let ((dorg-dolt-server--procs (make-hash-table :test 'equal))
        (killed nil))
    (puthash "/dir/a" (cons 'proc-a 3306) dorg-dolt-server--procs)
    (puthash "/dir/b" (cons 'proc-b 3307) dorg-dolt-server--procs)
    (cl-letf (((symbol-function 'dorg-sql--close-all-connections) (lambda () nil))
              ((symbol-function 'maphash)
               (lambda (fn table)
                 (when (gethash "/dir/a" table)
                   (funcall fn "/dir/a" (gethash "/dir/a" table)))
                 (when (gethash "/dir/b" table)
                   (funcall fn "/dir/b" (gethash "/dir/b" table)))))
              ((symbol-function 'dorg-dolt-server--kill-process)
               (lambda (proc &rest _)
                 (push proc killed)
                 ;; Killing proc-a fires a sentinel that yanks the OTHER entry
                 ;; from the live table mid-run.
                 (when (eq proc 'proc-a)
                   (remhash "/dir/b" dorg-dolt-server--procs))))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'dorg-dolt-server--release-claim)
               (lambda (_d) nil)))
      (should (dorg-dolt-server--exit-cleanup))
      ;; BOTH snapshot objects were attempted despite the mid-kill mutation.
      (should (memq 'proc-a killed))
      (should (memq 'proc-b killed)))))

(ert-deftest dorg-test-dolt-server-exit-cleanup-snapshot-failure-returns-t ()
  "If snapshot construction (the `maphash') errors or quits, exit cleanup
skips process iteration and STILL returns non-nil — it never vetoes exit.
The snapshot seam is as fallible as the kill seams and must be guarded."
  (dolist (mode '(:error :quit))
    (let ((dorg-dolt-server--procs (make-hash-table :test 'equal))
          (killed nil))
      (puthash "/dir/a" (cons 'proc-a 3306) dorg-dolt-server--procs)
      (cl-letf (((symbol-function 'dorg-sql--close-all-connections) (lambda () nil))
                ((symbol-function 'maphash)
                 (lambda (&rest _)
                   (if (eq mode :error) (error "maphash boom") (signal 'quit nil))))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (proc &rest _) (push proc killed))))
        ;; Never vetoes exit despite the snapshot seam throwing.
        (should (dorg-dolt-server--exit-cleanup))
        ;; Snapshot unavailable → no process iteration was attempted.
        (should (null killed))))))

(ert-deftest dorg-test-dolt-server-start-failure-includes-output ()
  "dorg-dolt-server--start enriches startup failures with sql-server output.
Simulates a failed startup without a real failing Dolt binary by
stubbing the readiness probe to signal and seeding the process
buffer with fake sql-server output."
  (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-startup-" "dolt-server-tests"))
        (orig-start (symbol-function 'start-process)))
    (unwind-protect
        (let ((dorg-dolt--version-cache (list :version dorg-dolt-minimum-version
                                              :raw "stubbed")))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _) "/usr/bin/dolt"))
                    ((symbol-function 'start-process)
                     (lambda (name buffer &rest _args)
                       ;; Spawn a harmless real process so liveness checks and
                       ;; cleanup behave, and seed fake sql-server output.
                       (let ((proc (funcall orig-start name buffer "sleep" "30")))
                         (when (bufferp buffer)
                           (with-current-buffer buffer
                             (insert "error: bind: address already in use\n")))
                         proc)))
                    ;; Pre-start store validation passes so the test reaches
                    ;; the server-start/readiness-failure path it covers.
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--wait-ready)
                     (lambda (&rest _) (error "probe failed"))))
            (let ((err (should-error (dorg-dolt-server--start temp-base) :type 'error)))
              (should (string-match-p "failed to start dolt sql-server" (cadr err)))
              (should (string-match-p (regexp-quote (expand-file-name temp-base))
                                      (cadr err)))
              (should (string-match-p "--doltcfg-dir" (cadr err)))
              (should (string-match-p
                       (regexp-quote (expand-file-name ".doltcfg" temp-base))
                       (cadr err)))
              (should (string-match-p "address already in use" (cadr err)))
              (should (string-match-p "probe failed" (cadr err))))
            ;; Registry stays clean after a failed start.
            (should-not (dorg-dolt-server-port temp-base))))
      (dorg-test--delete-owned-root temp-base))))

(ert-deftest dorg-test-dolt-server-start-pins-doltcfg-dir ()
  "Managed server startup passes the target repo's config dir explicitly.
`--doltcfg-dir' rides as a `sql-server' SUBCOMMAND flag (after the
subcommand), since `dolt sql-server' ignores the global one.  `--data-dir'
is the store itself so artifacts stay inside it."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (file-name-as-directory
                       (dorg-test--make-owned-temp-dir "dorg-start-doltcfg-" "dolt-server-tests")))
           (orig-start (symbol-function 'start-process))
           (captured-argv nil)
           (proc nil))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'dorg-dolt-server--free-port)
                       (lambda () 39111))
                      ((symbol-function 'start-process)
                       (lambda (name buffer program &rest args)
                         (setq captured-argv (cons program args))
                         (setq proc (funcall orig-start name buffer "sleep" "30"))))
                      ((symbol-function 'dorg-dolt-server--wait-ready)
                       (lambda (&rest _) t))
                      ;; Post-start served-DB gate: the launched server serves
                      ;; the DB our dolt-dir maps to.  Stub the served-set
                      ;; SNAPSHOT boundary (`--list-served-dbs'); the real pure
                      ;; predicate then exercises the derived db-name.
                      ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                       (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                      ((symbol-function 'dorg-dolt-server--list-served-dbs)
                       (lambda (&rest _)
                         (list (cdr (dorg-dolt-server--data-dir-and-db
                                     temp-base))))))
              (unwind-protect
                  (let* ((port (dorg-dolt-server--start-unsafe temp-base))
                         (cfg-tail (member "--doltcfg-dir" captured-argv))
                         (server-pos (cl-position "sql-server" captured-argv
                                                  :test #'equal))
                         (cfg-pos (cl-position "--doltcfg-dir" captured-argv
                                               :test #'equal))
                         (target (dorg-dolt--target temp-base)))
                    (should (= port 39111))
                    (should (equal (car captured-argv) "dolt"))
                    (should cfg-tail)
                    (should (equal (cadr cfg-tail)
                                   (expand-file-name ".doltcfg" temp-base)))
                    ;; sql-server ignores the global --doltcfg-dir, so it
                    ;; must ride AFTER the subcommand to be honored.
                    (should (> cfg-pos server-pos))
                    (should (equal (file-truename
                                    (cadr (member "--data-dir" captured-argv)))
                                   (file-truename
                                    (plist-get target :server-data-dir)))))
                (when (and proc (process-live-p proc))
                  (delete-process proc)))))
        (dorg-test--delete-owned-root temp-base)))))

;;; ─── Interactive SQL Shell ─────────────────────────────────────────────

(ert-deftest dorg-test-sql-connect-publishes-connection ()
  "`dorg-sql-connect' publishes a temporary sql.el connection entry."
  (require 'sql)
  (let (captured-connection
        captured-buffer
        captured-entry)
    (cl-letf (((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (_dolt-dir) 3307))
              ((symbol-function 'sql-connect)
               (lambda (connection buffer-name)
                 (setq captured-connection connection
                       captured-buffer buffer-name
                       captured-entry (assoc connection sql-connection-alist))
                 'connected)))
      (let ((sql-connection-alist nil))
        (should (eq (dorg-sql-connect "/tmp/example-repo/") 'connected))
        (should (eq captured-connection 'dorg-example-repo))
        (should (equal captured-buffer "*dorg-sql:example-repo*"))
        (should
         (equal captured-entry
                '(dorg-example-repo
                  (sql-product 'mysql)
                  (sql-server "127.0.0.1")
                  (sql-port 3307)
                  (sql-database "example-repo")
                  (sql-user "root"))))))))

(ert-deftest dorg-test-sql-connect-delegates-to-strict ()
  "`dorg-sql-connect' acquires its port via the strict session selector.
Retiring the legacy non-strict `dorg-dolt-server--ensure' makes
`dorg-dolt-server--ensure-strict' the sole session-owned acquire for the
interactive SQL shell.  The legacy selector is stubbed to signal so a
regression that keeps calling it fails here."
  (require 'sql)
  (let ((strict-called nil))
    (cl-letf (((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (_dir) (setq strict-called t) 3307))
              ((symbol-function 'dorg-dolt-server--ensure)
               (lambda (&rest _) (error "legacy non-strict selector must not be called")))
              ((symbol-function 'sql-connect)
               (lambda (&rest _) 'connected)))
      (let ((sql-connection-alist nil))
        (should (eq (dorg-sql-connect "/tmp/example-repo/") 'connected))
        (should strict-called)))))

(ert-deftest dorg-test-dolt-server-ensure-selector-removed ()
  "The legacy non-strict `dorg-dolt-server--ensure' selector is retired.
Consolidation leaves exactly one session-owned acquire
\(`dorg-dolt-server--ensure-strict'); the legacy symbol must be absent from
production so it cannot be reintroduced as a caller."
  (should-not (fboundp 'dorg-dolt-server--ensure)))

;;; ─── Dual-mode Query Execution ─────────────────────────────────────────

(ert-deftest dorg-test-dolt-server-sql-select ()
  "dorg-sql works through server mode with same results as CLI."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            ;; Insert data via CLI mode (no server)
            (dorg-sql--with-cli-seam dolt-dir
              (dorg-sql dolt-dir
                        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (1, 1, NULL, X'01', 0, 'Root')")
              (dorg-commit dolt-dir "seed data"))
            ;; Start server and query through it
            (dorg-dolt-server--ensure-strict dolt-dir)
            (unwind-protect
                (let ((result (dorg-sql dolt-dir "SELECT id, title FROM headings WHERE id = 1")))
                  (should (= (length result) 1))
                  (should (= (alist-get 'id (car result)) 1))
                  (should (equal (alist-get 'title (car result)) "Root")))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-sql-batch ()
  "dorg-sql-batch works through server mode."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-dolt-server--ensure-strict dolt-dir)
            (unwind-protect
                (progn
                  (dorg-sql-batch dolt-dir
                                  (list "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (1, 1, NULL, X'01', 0, NULL)"
                                        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (2, 1, 1, X'01', 1, 'Child')"))
                  (let ((result (dorg-sql dolt-dir "SELECT COUNT(*) as cnt FROM headings")))
                    (should (= (alist-get 'cnt (car result)) 2))))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(defun dorg-test-yybio--file-sha256 (file)
  "Return the SHA-256 hex digest of FILE's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(ert-deftest dorg-test-yybio-vendored-source-is-exact-upstream ()
  "The vendored client is the selected standalone source, without a patch."
  (let* ((root (file-name-directory (locate-library "dorg")))
         (mysql-file (expand-file-name "lib/mysql/mysql.el" root))
         (provenance (expand-file-name "lib/mysql/UPSTREAM.org" root))
         (mysql-sha
          "42d058c1d8d6463c160402ecba787b8597673e25248a117c9790380e80b14718"))
    (should (file-exists-p provenance))
    (should (equal mysql-sha (dorg-test-yybio--file-sha256 mysql-file)))
    (with-temp-buffer
      (insert-file-contents mysql-file)
      (should (re-search-forward "^;; Version: 0\\.2\\.4$" nil t))
      (should (re-search-forward
               "^;; URL: https://github\\.com/LuciusChen/mysql\\.el$" nil t)))
    (with-temp-buffer
      (insert-file-contents provenance)
      (should (search-forward
               "a59dd867884938b2c6d7ea528ffb2a8b093674f5" nil t))
      (should (search-forward mysql-sha nil t)))
    (should (fboundp 'mysql-connect-uri))
    (should-not (fboundp 'mysql-connect/uri))
    (dolist (condition '(mysql-connection-error mysql-protocol-error
                         mysql-query-error mysql-timeout))
      (should (equal (list condition 'mysql-error 'error)
                     (get condition 'error-conditions))))))

(ert-deftest dorg-test-yybio-vendored-mysql-call-surface ()
  "The selected client preserves DOrg's real connect/query/error surface."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base
           (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                 (port (dorg-dolt-server--ensure-strict dolt-dir))
                 (database (dorg-dolt-server--db-name dolt-dir))
                 conn process)
            (unwind-protect
                (progn
                  ;; This is DOrg's production connection shape: root with no
                  ;; password, explicit plaintext, selected database, and both
                  ;; bounded timeouts.
                  (setq conn (mysql-connect
                              :host "127.0.0.1" :port port :user "root"
                              :database database :tls nil
                              :connect-timeout dorg-sql-connect-timeout
                              :read-idle-timeout dorg-sql-read-timeout)
                        process (mysql-conn-process conn))
                  (should (process-live-p process))
                  (should (mysql-ping conn))
                  (let ((result
                         (mysql-query
                          conn
                          "SELECT '' AS empty_value, 'café ☕' AS utf8_value, 42 AS int_value")))
                    (should
                     (equal '("empty_value" "utf8_value" "int_value")
                            (mapcar (lambda (column) (plist-get column :name))
                                    (mysql-result-columns result))))
                    (should (equal '(("" "café ☕" 42))
                                   (mysql-result-rows result))))
                  ;; A complete server ERR response is typed and leaves the
                  ;; same session synchronized for the next command.
                  (should-error
                   (mysql-query conn
                                "SELECT * FROM __dorg_yybio_missing_table__")
                   :type 'mysql-query-error)
                  (should (eq process (mysql-conn-process conn)))
                  (should (mysql-ping conn))
                  (should (equal '((7))
                                 (mysql-result-rows
                                  (mysql-query conn "SELECT 7 AS value"))))
                  (mysql-disconnect conn)
                  (setq conn nil)
                  (should-not (process-live-p process)))
              (when conn
                (ignore-errors (mysql-disconnect conn)))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-sql-empty-leading-column ()
  "Server/mysql mode preserves rows whose first selected column is empty.

Regression for the bundled `lib/mysql/mysql.el' text-row reader
dropping a row whose first/only selected value is a non-NULL empty
string.  In the MySQL text protocol such a value is length-encoded
with a leading byte #x00, which `mysql--packet-type' misclassified as
an OK/end-of-resultset packet, so `mysql--read-text-rows' stopped
before parsing the row.

Requires the native mysql.el path; it skips when mysql.el is
unavailable rather than silently passing through the CLI JSON
fallback, which does not exhibit the bug.  The test also spies on
`dorg-sql--mysql-exec' / `dorg-sql--cli-exec' to prove the native
wire-protocol path is the one exercised."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — native sql-server path not exercised")))
  (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
    (unwind-protect
        (let ((dolt-dir (dorg-test--copy-repo temp-base)))
          ;; Seed via CLI mode (no server yet): id=1 has an empty title,
          ;; id=2 has a non-empty title for the unchanged-behavior check.
          (dorg-sql--with-cli-seam dolt-dir
            (dorg-sql dolt-dir
                      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (1, 1, NULL, X'01', 0, '')")
            (dorg-sql dolt-dir
                      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (2, 1, 1, X'02', 1, 'Child')")
            (dorg-commit dolt-dir "seed empty-title row"))
          (dorg-dolt-server--ensure-strict dolt-dir)
          (unwind-protect
              (let ((mysql-calls 0)
                    (cli-called nil)
                    (orig-mysql (symbol-function 'dorg-sql--mysql-exec)))
                (cl-letf (((symbol-function 'dorg-sql--mysql-exec)
                           (lambda (&rest args)
                             (cl-incf mysql-calls)
                             (apply orig-mysql args)))
                          ((symbol-function 'dorg-sql--cli-exec)
                           (lambda (&rest _)
                             (setq cli-called t)
                             (error "CLI fallback used — native mysql path not exercised"))))
                  ;; (a) first/only selected column is the empty string.
                  (let ((r (dorg-sql dolt-dir
                                     "SELECT title AS value FROM headings WHERE id = 1")))
                    (should (= (length r) 1))
                    (should (equal (alist-get 'value (car r)) "")))
                  ;; (b) empty first column followed by a coerced numeric id.
                  (let ((r (dorg-sql dolt-dir
                                     "SELECT title AS value, id FROM headings WHERE id = 1")))
                    (should (= (length r) 1))
                    (should (equal (alist-get 'value (car r)) ""))
                    (should (= (alist-get 'id (car r)) 1)))
                  ;; (c) normal non-empty leading column is unchanged.
                  (let ((r (dorg-sql dolt-dir
                                     "SELECT id, title AS value FROM headings WHERE id = 2")))
                    (should (= (length r) 1))
                    (should (= (alist-get 'id (car r)) 2))
                    (should (equal (alist-get 'value (car r)) "Child")))
                  ;; Prove the native wire-protocol path ran and CLI never did.
                  (should (> mysql-calls 0))
                  (should-not cli-called)))
            (dorg-dolt-server--stop dolt-dir)))
      (dorg-test--delete-owned-root temp-base))))

;;; ─── Dual-mode Commit ──────────────────────────────────────────────────

(ert-deftest dorg-test-dolt-server-commit ()
  "dorg-commit works through server mode using DOLT_ADD/DOLT_COMMIT."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-dolt-server--ensure-strict dolt-dir)
            (unwind-protect
                (let ((hash1 (dorg-head-commit dolt-dir)))
                  (dorg-sql dolt-dir
                            "INSERT INTO headings (id, root_id, parent_id, sort_key, level, title) VALUES (1, 1, NULL, X'01', 0, 'Test')")
                  (dorg-commit dolt-dir "server-mode commit")
                  (let ((hash2 (dorg-head-commit dolt-dir)))
                    (should-not (equal hash1 hash2))))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-commit-clean-repo ()
  "dorg-commit via server mode on clean repo does not error."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-dolt-server--ensure-strict dolt-dir)
            (unwind-protect
                (dorg-commit dolt-dir "no-op server commit")
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base)))))

;;; ─── Parent-Artifact Containment (real server) ─────────────────────────

(ert-deftest dorg-test-dolt-server-start-leaves-no-parent-artifacts ()
  "A real managed sql-server start for a nested store creates no parent artifacts.
Regression for tracked-case: pointing `--data-dir' at the store itself
keeps every dolt server artifact (`.dolt/sql-server.info', `.doltcfg',
`config.yaml') INSIDE the store, so the parent dir stays clean and the
stale one-shot CLI paths never hit \"multiple .doltcfg directories
detected\".  Observes the REAL filesystem after an actual sql-server
startup (not mocked argv).  Non-vacuous: were `--data-dir' the parent
\(the pre-fix behavior), the parent would gain `.doltcfg' and `.dolt',
and the `should-not' assertions would fail."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-io3e8-artifacts-" "dolt-server-tests")))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (parent (file-name-as-directory (expand-file-name temp-base)))
                   (stray (lambda ()
                            (seq-filter
                             #'file-exists-p
                             (list (expand-file-name ".dolt" parent)
                                   (expand-file-name ".doltcfg" parent)
                                   (expand-file-name "config.yaml" parent)
                                   (expand-file-name ".dolt/sql-server.info"
                                                     parent))))))
              ;; The parent holds only the store before anything starts.
              (should (equal (directory-files parent nil "\\`[^.]") '("dolt")))
              (dorg-dolt-server--start dolt-dir)
              (unwind-protect
                  (progn
                    ;; A real server is up and serving the store ...
                    (should (dorg-dolt-server-port dolt-dir))
                    ;; ... yet the parent gained no dolt server artifacts.
                    (should-not (funcall stray)))
                (dorg-dolt-server--stop dolt-dir))
              ;; Still clean after the server stops.
              (should-not (funcall stray)))
          (dorg-test--delete-owned-root temp-base))))))

;;; ─── Server Management Commands (no dolt required) ─────────────────────
;;
;; These exercise the structured inventory and owner-safe user commands
;; against synthetic runtime state (claim files, faked owned processes,
;; held TCP ports) so they run without a real dolt binary or sql-server.

(defvar dorg-test--store-root nil
  "Temp store-root bound by `dorg-test--with-dolt-server-runtime'.
Since claims are now store-relative (`<store>/.dorg/'), test \"stores\"
must live under an isolated, auto-cleaned root so their overlay dirs do
not leak into a shared `/tmp' location across tests or runs.")

(defvar dorg-test--fallback-root nil
  "Memoized fallback store-root for `dorg-test--repo' calls made outside
`dorg-test--with-dolt-server-runtime', so repeated calls with the same NAME
return the SAME path within a run.")

(defun dorg-test--repo (name)
  "Return an isolated test store dir NAME under `dorg-test--store-root'.
Outside the fixture, uses a per-run memoized fallback root so the path is
stable across calls with the same NAME (a single test may reference a repo
more than once)."
  (file-name-as-directory
   (expand-file-name name (or dorg-test--store-root
                              (or dorg-test--fallback-root
                                  (setq dorg-test--fallback-root
                                        (dorg-test--make-owned-temp-dir "dorg-test-store-" "dolt-server-tests")))))))

(defun dorg-test--runtime-server-metadata-file-p (file)
  "Non-nil when FILE names runtime server metadata this fixture may clean."
  (let ((base (file-name-nondirectory file))
        (parent (file-name-nondirectory
                 (directory-file-name (file-name-directory file)))))
    (or (and (equal parent ".dorg") (equal base "server.claim"))
        (and (equal parent ".dolt") (equal base "sql-server.info")))))

(defun dorg-test--runtime-server-metadata-files (root)
  "Return exact server metadata file paths under ROOT without following dir links."
  (let ((root (file-name-as-directory (expand-file-name root)))
        files)
    (cl-labels
        ((walk (dir)
           (when (and (file-directory-p dir)
                      (not (file-symlink-p (directory-file-name dir))))
             (dolist (entry (directory-files dir t nil t))
               (let ((name (file-name-nondirectory entry)))
                 (unless (member name '("." ".."))
                   (cond
                    ((and (file-directory-p entry)
                          (not (file-symlink-p (directory-file-name entry))))
                     (walk entry))
                    ((dorg-test--runtime-server-metadata-file-p entry)
                     (push entry files)))))))))
      (walk root))
    (nreverse files)))

(defun dorg-test--runtime-server-metadata-cleanup-debt-p (root file)
  "Non-nil when FILE is synthetic runtime metadata safe to delete before ROOT.
The decision is fail-closed: metadata is cleanup debt only when its canonical
store remains contained by ROOT and process-table evidence is definitely
`none'.  A live or indeterminate scan leaves the file for the normal owned-root
reaper."
  (when-let* ((store (dorg-test--store-dir-from-server-metadata file))
              (canon (ignore-errors (dorg-dolt-server--canonical-dir store))))
    (and (dorg-test--dir-contained-p canon root)
         (eq 'none (dorg-test--live-dolt-process-evidence canon)))))

(defun dorg-test--delete-runtime-server-metadata (root)
  "Delete synthetic server metadata under runtime ROOT before owned-root cleanup.
Only exact enumerated `server.claim' and `sql-server.info' paths are deleted;
directory symlinks are not traversed, and canonical store containment is checked
before deletion."
  (dolist (file (dorg-test--runtime-server-metadata-files root))
    (when (dorg-test--runtime-server-metadata-cleanup-debt-p root file)
      (when (or (file-exists-p file) (file-symlink-p file))
        (condition-case err
            (delete-file file)
          (file-missing nil)
          (error (signal (car err) (cdr err))))))))

(defmacro dorg-test--with-dolt-server-runtime (&rest body)
  "Run BODY with isolated, empty DOrg server runtime state.
Binds an isolated `dorg-test--store-root' (where store-relative
`<store>/.dorg/' overlays land), and the process, attach, and owned-claim
tables to empty hash tables so no real same-host claim or this-process
server leaks into the test."
  (declare (indent 0))
  `(let* ((dorg-test--store-root (dorg-test--make-owned-temp-dir "dorg-test-store-" "dolt-server-tests"))
          (dorg-dolt-server-index-dir
           (expand-file-name "server-index/" dorg-test--store-root))
          (dorg-dolt-server--procs (make-hash-table :test 'equal))
          (dorg-dolt-server--attached-ports (make-hash-table :test 'equal))
          (dorg-dolt-server--owned-claims (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (dorg-test--delete-runtime-server-metadata dorg-test--store-root)
       (dorg-test--delete-owned-root dorg-test--store-root))))

(defun dorg-test--listen-port ()
  "Open a listening TCP server on a free port; return (PROC . PORT).
Used to make a claim's port classify as `live' without a real server."
  (let* ((proc (make-network-process :name "dorg-test-listen"
                                     :host "127.0.0.1" :service 0
                                     :server t :family 'ipv4))
         (port (process-contact proc :service)))
    (cons proc port)))

(defun dorg-test--write-claim (dolt-dir &rest overrides)
  "Write a synthetic claim file for DOLT-DIR, applying OVERRIDES.
OVERRIDES is a plist of claim keys; defaults describe a same-host
foreign owner with an absent pid and a dead port."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (claim (list (cons :dolt-dir canon)
                      (cons :host (or (plist-get overrides :host) (system-name)))
                      (cons :owner-pid (if (plist-member overrides :owner-pid)
                                           (plist-get overrides :owner-pid)
                                         999999))
                      (cons :server-pid (plist-get overrides :server-pid))
                      (cons :owner-kind (or (plist-get overrides :owner-kind) 'emacs))
                      (cons :port (plist-get overrides :port))
                      (cons :token (or (plist-get overrides :token) "foreign-token"))
                      (cons :policy 'server)
                      (cons :protocol (if (plist-member overrides :protocol)
                                          (plist-get overrides :protocol)
                                        dorg-dolt-server-claim-protocol))
                      (cons :created-at (or (plist-get overrides :created-at)
                                            (float-time)))
                      (cons :last-used (or (plist-get overrides :last-used)
                                           (float-time))))))
    (dorg-dolt-server--claim-write dolt-dir claim)
    canon))

(ert-deftest dorg-test-dolt-server-runtime-cleans-synthetic-live-pid-metadata ()
  "Runtime unwind deletes synthetic claim+native info even when pid is live.
The synthetic pid is this Emacs process, so a claim/native-info residue would
hit the fail-closed reaper as live-but-not-dolt and preserve the owned root."
  (let (root)
    (dorg-test--with-dolt-server-runtime
      (setq root dorg-test--store-root)
      (let* ((store (dorg-test--repo "fixture-synthetic-live-pid"))
             (canon (dorg-dolt-server--canonical-dir store))
             (dolt (expand-file-name ".dolt" canon))
             (claim-file (dorg-dolt-server--claim-file store))
             (info-file (expand-file-name "sql-server.info" dolt)))
        (make-directory dolt t)
        (dorg-test--write-claim store
                                :owner-pid (emacs-pid)
                                :server-pid (emacs-pid)
                                :port 1
                                :owner-kind 'daemon)
        (with-temp-file info-file
          (insert (format "%d:1:synthetic-live-pid" (emacs-pid))))
        (should (file-exists-p claim-file))
        (should (file-exists-p info-file))))
    (should root)
    (should-not (file-directory-p root))))

(ert-deftest dorg-test-dolt-server-runtime-keeps-metadata-on-unknown-process-scan ()
  "Runtime pre-cleanup preserves metadata when process evidence is unknown."
  (let* ((root (dorg-test--make-owned-temp-dir "dorg-sx6cn-unknown-" "dolt-server-tests"))
         (store (expand-file-name "store/" root))
         (dolt (expand-file-name ".dolt" store))
         (info-file (expand-file-name "sql-server.info" dolt)))
    (unwind-protect
        (progn
          (make-directory dolt t)
          (with-temp-file info-file (insert "4242:1:unknown-scan"))
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (&rest _) 'unknown)))
            (dorg-test--delete-runtime-server-metadata root))
          (should (file-exists-p info-file)))
      (ignore-errors (delete-file info-file))
      (ignore-errors (dorg-test--delete-owned-root root)))))

(ert-deftest dorg-test-dolt-server-runtime-cleanup-deletes-metadata-symlink-only ()
  "Runtime pre-cleanup deletes the metadata symlink, not its outside target."
  (let* ((root (dorg-test--make-owned-temp-dir "dorg-sx6cn-link-" "dolt-server-tests"))
         (store (expand-file-name "store/" root))
         (dolt (expand-file-name ".dolt" store))
         (outside (make-temp-file "dorg-sx6cn-outside-info-"))
         (info-link (expand-file-name "sql-server.info" dolt)))
    (unwind-protect
        (progn
          (make-directory dolt t)
          (with-temp-file outside (insert "outside"))
          (make-symbolic-link outside info-link)
          (dorg-test--delete-runtime-server-metadata root)
          (should-not (file-symlink-p info-link))
          (should (file-exists-p outside)))
      (ignore-errors (delete-file info-link))
      (ignore-errors (delete-file outside))
      (ignore-errors (dorg-test--delete-owned-root root)))))

(ert-deftest dorg-test-dolt-server-runtime-cleanup-refuses-symlink-store-escape ()
  "Runtime pre-cleanup never descends through a store symlink escaping ROOT."
  (let* ((root (dorg-test--make-owned-temp-dir "dorg-sx6cn-storelink-" "dolt-server-tests"))
         (outside (file-name-as-directory
                   (make-temp-file "dorg-sx6cn-outside-store-" t)))
         (outside-dolt (expand-file-name ".dolt" outside))
         (outside-info (expand-file-name "sql-server.info" outside-dolt))
         (store-link (expand-file-name "store-link" root)))
    (unwind-protect
        (progn
          (make-directory outside-dolt t)
          (with-temp-file outside-info (insert "4242:1:outside"))
          (make-symbolic-link (directory-file-name outside) store-link)
          (dorg-test--delete-runtime-server-metadata root)
          (should (file-symlink-p store-link))
          (should (file-exists-p outside-info)))
      (ignore-errors (delete-file store-link))
      (ignore-errors (delete-directory outside t))
      (ignore-errors (dorg-test--delete-owned-root root)))))

(ert-deftest dorg-test-dolt-server-dir-sql-rows-uses-bounded-cli-seam ()
  "Start-path probes reuse the bounded CLI primitive with explicit bypasses."
  (let* ((root (file-name-as-directory
                (dorg-test--make-owned-temp-dir "dorg-dir-sql-doltcfg-" "dolt-server-tests")))
         (dolt-dir (expand-file-name "dolt/" root))
         (captured-query nil)
         (captured-target nil)
         (captured-conflict-check 'unset)
         (rows '(((Tables_in_dolt . "dorg_files")))))
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (let ((dorg-sql--pinned-transport nil)
                (dorg-sql--cli-conflict-check t))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _)
                         (ert-fail "start-path probe bypassed dorg-sql--cli-exec")))
                      ((symbol-function 'dorg-sql)
                       (lambda (&rest _)
                         (ert-fail "start-path probe entered the leased query path")))
                      ((symbol-function 'dorg-sql--cli-exec)
                       (lambda (query target)
                         (setq captured-query query
                               captured-target target
                               captured-conflict-check
                               dorg-sql--cli-conflict-check)
                         rows)))
              (should (equal rows
                             (dorg-dolt-server--dir-sql-rows
                              dolt-dir "SHOW TABLES"))))
            ;; The bypass is bounded to the start-path probe.
            (should dorg-sql--cli-conflict-check)
            (should-not dorg-sql--pinned-transport))
          (should (equal captured-query "SHOW TABLES"))
          (should-not captured-conflict-check)
          (should (equal (plist-get captured-target :repo-dir)
                         (file-name-as-directory (expand-file-name dolt-dir))))
          (should (equal (plist-get captured-target :cfg-dir)
                         (expand-file-name ".doltcfg" dolt-dir))))
      (dorg-test--delete-owned-root root))))

(ert-deftest dorg-test-dolt-server-dir-sql-rows-restores-seam-after-error ()
  "A failed bounded probe returns nil and restores the outer policy state."
  (let* ((root (file-name-as-directory
                (dorg-test--make-owned-temp-dir "dorg-dir-sql-error-" "dolt-server-tests")))
         (dolt-dir (expand-file-name "dolt/" root))
         (outer-pin (cons "/unrelated/store/" '(mysql . 3306)))
         (dorg-sql--pinned-transport outer-pin)
         (dorg-sql--cli-conflict-check t)
         (seam-called nil))
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest _)
                       (ert-fail "start-path probe bypassed dorg-sql--cli-exec")))
                    ((symbol-function 'dorg-sql)
                     (lambda (&rest _)
                       (ert-fail "start-path probe entered the leased query path")))
                    ((symbol-function 'dorg-sql--cli-exec)
                     (lambda (&rest _)
                       (setq seam-called t)
                       (should-not dorg-sql--cli-conflict-check)
                       (error "synthetic bounded CLI failure"))))
            (should-not
             (dorg-dolt-server--dir-sql-rows dolt-dir "SHOW TABLES")))
          (should seam-called)
          (should (eq dorg-sql--pinned-transport outer-pin))
          (should dorg-sql--cli-conflict-check))
      (dorg-test--delete-owned-root root))))

(ert-deftest dorg-test-dolt-server-data-dir-and-db-derivation ()
  "`dorg-dolt-server--data-dir-and-db' derives (DATA-DIR . DB) from a dolt-dir.
Nested `.dorg/dolt' serves DB `dolt' from the store ITSELF as the
data-dir (so sql-server artifacts stay inside the store, not the parent);
a standalone repo serves a DB named after the repo from its own dir.
Covers the launch contract: the served store is explicit, not
cwd-derived.  Compares against canonicalized paths so a symlinked TMPDIR
\(macOS /var -> /private/var) does not make the assertion vacuous."
  ;; Nested store: /<tmp>/proj/.dorg/dolt/ -> data-dir the store, db dolt.
  (let* ((root (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-deriv-nested-" "dolt-server-tests")))
         (nested (expand-file-name ".dorg/" root))
         (dolt (expand-file-name "dolt/" nested)))
    (unwind-protect
        (progn
          (make-directory dolt t)
          (let ((res (dorg-dolt-server--data-dir-and-db dolt)))
            (should (equal (cdr res) "dolt"))
            ;; The derived data-dir is the store itself, NOT its parent —
            ;; guards against a regression back to parent-rooted artifacts.
            (should (equal (file-truename (car res)) (file-truename dolt)))
            (should-not (equal (file-truename (car res)) (file-truename nested)))))
      (dorg-test--delete-owned-root root)))
  ;; Standalone repo: /<tmp>/myrepo/ (with .dolt) -> data-dir the repo, db myrepo.
  (let* ((parent (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-deriv-standalone-" "dolt-server-tests")))
         (repo (expand-file-name "myrepo/" parent)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".dolt" repo) t)
          (let ((res (dorg-dolt-server--data-dir-and-db repo)))
            (should (equal (cdr res) "myrepo"))
            (should (equal (file-truename (car res)) (file-truename repo)))
            (should-not (equal (file-truename (car res)) (file-truename parent)))))
      (dorg-test--delete-owned-root parent))))

(ert-deftest dorg-test-dolt-server-start-fails-closed-on-wrong-db ()
  "A server that comes up but does not serve the expected DB fails closed.
Drives `dorg-dolt-server--start' with the spawn, readiness probe, and
SHOW DATABASES probe stubbed so a sql-server process appears to come up
but serves the wrong store.  The launch contract
requires DOrg to publish a provisional owner before readiness, kill the
process it started, delete that claim only after confirmed death, and signal a
bounded `dorg-sql-transport-error' — never falling back to one-shot CLI.
Non-vacuous: omitting provisional publication or retaining it after confirmed
death fails the two claim assertions."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (dorg-test--make-owned-temp-dir "dorg-wrongdb-" "dolt-server-tests"))
           (orig-start (symbol-function 'start-process))
           (orig-kill (symbol-function 'dorg-dolt-server--kill-process))
           (orig-publish (symbol-function 'dorg-dolt-server--publish-claim))
           (killed nil)
           (published-states nil))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ;; Spawn a harmless real process so liveness/cleanup work.
                      ((symbol-function 'start-process)
                       (lambda (name buffer &rest _args)
                         (funcall orig-start name buffer "sleep" "30")))
                      ((symbol-function 'dorg-dolt-server--wait-ready)
                       (lambda (&rest _) t))
                      ;; Pre-start dir validation passes (store content OK);
                      ;; this test exercises the post-start served-DB gate.
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) t))
                      ;; The server is up but serves some OTHER store, not the
                      ;; expected DB derived from the bound dolt-dir: the served
                      ;; SNAPSHOT omits it, so the pure membership predicate
                      ;; yields `wrong-db'.
                      ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                       (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                      ((symbol-function 'dorg-dolt-server--list-served-dbs)
                       (lambda (&rest _) '("information_schema" "wrongdb")))
                      ((symbol-function 'dorg-dolt-server--kill-process)
                       (lambda (proc) (setq killed t) (funcall orig-kill proc)))
                      ((symbol-function 'dorg-dolt-server--publish-claim)
                       (lambda (&rest args)
                         (push (nth 8 args) published-states)
                         (apply orig-publish args))))
              (let ((err (should-error (dorg-dolt-server--start temp-base)
                                       :type 'dorg-sql-transport-error)))
                ;; Bounded diagnostic names both the failure and death-gated
                ;; provisional-claim disposition.
                (should (string-match-p "does not serve the expected database"
                                        (cadr err)))
                (should (string-match-p "only if death was confirmed" (cadr err)))))
            ;; Killed the process it started ...
            (should killed)
            ;; ... after first publishing exactly one provisional owner ...
            (should (equal published-states '(probe-inconclusive)))
            ;; ... and left no claim file behind after confirmed death ...
            (should-not (dorg-dolt-server-claim temp-base))
            ;; ... and registered no owned server (no silent leak/CLI fallback).
            (should-not (dorg-dolt-server-port temp-base)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-bootstrap-start-allows-fresh-no-schema ()
  "Bootstrap server start allows a just-`dolt init' repo before schema exists.
This is narrower than public `dorg-dolt-server--start': it must skip only
the pre-start DOrg schema validation that a fresh repo cannot satisfy,
while still waiting for readiness, verifying that the expected database is
served, registering the owned process, and upgrading the pre-readiness managed
claim after those checks.  Non-vacuous: calling the public start here
would trip the sentinel `dorg-dolt-server--dir-store-valid-p' stub."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (file-name-as-directory
                       (dorg-test--make-owned-temp-dir "dorg-bootstrap-server-" "dolt-server-tests")))
           (orig-start (symbol-function 'start-process))
           (orig-kill (symbol-function 'dorg-dolt-server--kill-process))
           (orig-publish (symbol-function 'dorg-dolt-server--publish-claim))
           (captured-argv nil)
           (published nil)
           (wait-ready-calls 0)
           (served-list-calls 0)
           (proc nil))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) (error "bootstrap must not require preexisting dorg_files")))
                      ((symbol-function 'dorg-dolt-server--free-port)
                       (lambda () 39222))
                      ((symbol-function 'start-process)
                       (lambda (name buffer program &rest args)
                         (setq captured-argv (cons program args))
                         (setq proc (funcall orig-start name buffer "sleep" "30"))))
                      ((symbol-function 'dorg-dolt-server--wait-ready)
                       (lambda (port &rest _)
                         (should (= port 39222))
                         (cl-incf wait-ready-calls)
                         t))
                      ;; Post-start served-DB gate probes the served SNAPSHOT
                      ;; once on the right port and finds the DB our dolt-dir
                      ;; maps to; the real pure predicate then confirms
                      ;; membership (proving the derived db-name), so bootstrap
                      ;; publishes its claim.
                      ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                       (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                      ((symbol-function 'dorg-dolt-server--list-served-dbs)
                       (lambda (port)
                         (should (= port 39222))
                         (cl-incf served-list-calls)
                         (list (cdr (dorg-dolt-server--data-dir-and-db
                                     temp-base)))))
                      ((symbol-function 'dorg-dolt-server--publish-claim)
                       (lambda (&rest args)
                         (setq published args)
                         (apply orig-publish args))))
              (unwind-protect
                  (let ((port (dorg-dolt-server--start-bootstrap temp-base)))
                    (should (= port 39222))
                    (should (equal (car captured-argv) "dolt"))
                    (should (member "sql-server" captured-argv))
                    (should (member "--data-dir" captured-argv))
                    (should (process-live-p proc))
                    (should (= wait-ready-calls 1))
                    (should (= served-list-calls 1))
                    (should published)
                    (should (dorg-dolt-server-claim temp-base))
                    (should (= (dorg-dolt-server-port temp-base) 39222)))
                (when (and proc (process-live-p proc))
                  (funcall orig-kill proc)))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-publish-claim-records-normalized-target ()
  "`dorg-dolt-server--publish-claim' records normalized :data-dir/:db-name.
For a nested `.dorg/dolt' store the published claim must carry the
explicit `dolt sql-server' target derived by
`dorg-dolt-server--data-dir-and-db' — data-dir the store itself, db-name
`dolt' — so downstream readers see exactly which store the claim covers.
Pins the additive metadata that existing claim
roundtrip tests do not assert.  Non-vacuous: data-dir must equal the
store, never its parent."
  (dorg-test--with-dolt-server-runtime
    (let* ((root (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-claim-target-" "dolt-server-tests")))
           (nested (expand-file-name ".dorg/" root))
           (dolt (expand-file-name "dolt/" nested)))
      (unwind-protect
          (progn
            (make-directory dolt t)
            ;; Refresh path: data-dir/db-name omitted -> derived from dolt-dir.
            (dorg-dolt-server--publish-claim dolt 3309)
            (let ((claim (dorg-dolt-server-claim dolt)))
              (should claim)
              (should (equal (alist-get :db-name claim) "dolt"))
              (should (equal (file-truename (alist-get :data-dir claim))
                             (file-truename dolt)))
              ;; data-dir is the store itself, never its parent.
              (should-not (equal (file-truename (alist-get :data-dir claim))
                                 (file-truename nested)))))
        (dorg-test--delete-owned-root root)))))

(ert-deftest dorg-test-dolt-server-list-empty ()
  "`dorg-dolt-server-list' returns nil with no managed servers or claims."
  (dorg-test--with-dolt-server-runtime
    (should-not (dorg-dolt-server-list))))

(ert-deftest dorg-test-dolt-server-list-discovers-claim-in-fresh-process ()
  "A fresh process lists a claim published by an earlier process.
The fresh-process simulation clears every in-memory registry while retaining
only the durable store claim and discovery pointer.  Removing index
registration makes the list empty, so this is load-bearing for the real CLI
`start' then `list' contract."
  (dorg-test--with-dolt-server-runtime
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (let* ((repo (dorg-test--repo "cross-process-list"))
                 (canon (dorg-dolt-server--canonical-dir repo)))
            (dorg-dolt-server--publish-claim repo (cdr listener))
            ;; A new CLI Emacs has none of the starter's memory.
            (setq dorg-dolt-server--procs (make-hash-table :test 'equal)
                  dorg-dolt-server--attached-ports (make-hash-table :test 'equal)
                  dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
            (cl-letf (((symbol-function 'dorg-dolt-server--registry-store-dirs)
                       (lambda () nil)))
              (let ((entries (dorg-dolt-server-list)))
                (should (= 1 (length entries)))
                (should (equal canon (plist-get (car entries) :dolt-dir)))
                (should (= (cdr listener) (plist-get (car entries) :port))))))
        (delete-process (car listener))))))

(ert-deftest dorg-test-dolt-server-claim-delete-unregisters-discovery ()
  "Deleting the authoritative claim removes its discovery pointer too."
  (dorg-test--with-dolt-server-runtime
    (let ((repo (dorg-test--repo "index-release")))
      (dorg-dolt-server--publish-claim repo 4321)
      (should (file-exists-p (dorg-dolt-server--index-file repo)))
      (dorg-dolt-server--claim-delete-file repo)
      (should-not (file-exists-p (dorg-dolt-server--index-file repo)))
      (should-not (dorg-dolt-server--indexed-store-dirs)))))

(ert-deftest dorg-test-dolt-server-list-read-only ()
  "`dorg-dolt-server-list' is a pure reader: it starts nothing and writes nothing.
Listing a foreign claim must not auto-start a server or add claim files,
preserving the guardrail that inspection never triggers a
server start."
  (dorg-test--with-dolt-server-runtime
    (let* ((repo (dorg-test--repo "readonly-repo"))
           (canon (dorg-test--write-claim repo))
           (claim-file (dorg-dolt-server--claim-file canon))
           (before (and (file-exists-p claim-file)
                        (with-temp-buffer
                          (insert-file-contents claim-file)
                          (buffer-string)))))
      (dorg-dolt-server-list)
      (should (file-exists-p claim-file))
      (should (equal before
                     (with-temp-buffer
                       (insert-file-contents claim-file)
                       (buffer-string))))
      ;; No this-process server was started as a side effect.
      (should (= 0 (hash-table-count dorg-dolt-server--procs))))))

(ert-deftest dorg-test-dolt-server-list-classifies-dead-claim ()
  "A same-host claim with an absent owner pid and dead port lists as dead."
  (dorg-test--with-dolt-server-runtime
    (let ((canon (dorg-test--write-claim (dorg-test--repo "dead-repo")
                                         :owner-pid 999999 :port nil)))
      ;; Store-relative overlays are enumerated via the dorg registry of
      ;; client documents, not a central scan; register this store so the
      ;; list surfaces its overlay.
      (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                 (lambda (&optional _buf) canon)))
        (let* ((entries (dorg-dolt-server-list))
               (e (car entries)))
          (should (= 1 (length entries)))
          (should (equal (plist-get e :dolt-dir) canon))
          (should (eq (plist-get e :role) 'foreign))
          (should (eq (plist-get e :status) 'dead))
          (should (eq (plist-get e :policy) 'server)))))))

(ert-deftest dorg-test-dolt-server-list-classifies-foreign-live ()
  "A foreign claim with a live port lists as role foreign, status live.
\(With mysql.el unavailable the live-but-unattachable claim surfaces as
`incompatible'; either way it is a foreign, non-owned entry.)"
  (dorg-test--with-dolt-server-runtime
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (let ((canon (dorg-test--write-claim (dorg-test--repo "live-repo")
                                               :owner-pid 999999
                                               :port (cdr listener))))
            (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                       (lambda (&optional _buf) canon)))
              (let ((e (car (dorg-dolt-server-list))))
                (should (equal (plist-get e :dolt-dir) canon))
                (should (eq (plist-get e :role) 'foreign))
                (should (memq (plist-get e :status) '(live incompatible)))
                (should (= (plist-get e :port) (cdr listener))))))
        (delete-process (car listener))))))

(ert-deftest dorg-test-dolt-server-list-owned-process ()
  "A this-process owned live server lists as role owned, status live."
  (dorg-test--with-dolt-server-runtime
    (let* ((proc (start-process "dorg-test-sleep" nil "sleep" "30"))
           (canon (dorg-dolt-server--canonical-dir (dorg-test--repo "owned-repo"))))
      (unwind-protect
          (progn
            (puthash canon (cons proc 4321) dorg-dolt-server--procs)
            (let ((e (car (dorg-dolt-server-list))))
              (should (eq (plist-get e :role) 'owned))
              (should (eq (plist-get e :status) 'live))
              (should (= (plist-get e :port) 4321))
              (should (= (plist-get e :server-pid) (process-id proc)))))
        (delete-process proc)))))

(ert-deftest dorg-test-dolt-server-list-attached-port ()
  "A foreign live server this process attached to lists as role attached."
  (dorg-test--with-dolt-server-runtime
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (let ((canon (dorg-dolt-server--canonical-dir (dorg-test--repo "attached-repo"))))
            (dorg-test--write-claim (dorg-test--repo "attached-repo")
                                    :owner-pid 999999 :port (cdr listener))
            (puthash canon (cdr listener) dorg-dolt-server--attached-ports)
            (let ((e (cl-find canon (dorg-dolt-server-list)
                              :key (lambda (entry) (plist-get entry :dolt-dir))
                              :test #'equal)))
              (should e)
              (should (eq (plist-get e :role) 'attached))
              (should (= (plist-get e :port) (cdr listener)))))
        (delete-process (car listener))))))

(ert-deftest dorg-test-dolt-stop-refuses-foreign-live ()
  "`dorg-dolt-stop' refuses a live server owned by another DOrg process.
It signals `user-error' with owner pid/port and leaves the claim file
intact rather than killing another process's server."
  (dorg-test--with-dolt-server-runtime
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (progn
            (dorg-test--write-claim (dorg-test--repo "foreign-repo")
                                    :owner-pid 999999 :port (cdr listener))
            (let ((claim-file (dorg-dolt-server--claim-file (dorg-test--repo "foreign-repo"))))
              (should (file-exists-p claim-file))
              (should-error (dorg-dolt-stop (dorg-test--repo "foreign-repo"))
                            :type 'user-error)
              ;; The foreign owner's claim must survive the refusal.
              (should (file-exists-p claim-file))))
        (delete-process (car listener))))))

(ert-deftest dorg-test-dolt-stop-detaches-attached ()
  "`dorg-dolt-stop' on an attach-only client detaches and keeps the claim."
  (dorg-test--with-dolt-server-runtime
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (let ((canon (dorg-dolt-server--canonical-dir (dorg-test--repo "detach-repo")))
                (claim-file (dorg-dolt-server--claim-file (dorg-test--repo "detach-repo"))))
            (dorg-test--write-claim (dorg-test--repo "detach-repo")
                                    :owner-pid 999999 :port (cdr listener))
            (puthash canon (cdr listener) dorg-dolt-server--attached-ports)
            (should (dorg-dolt-stop (dorg-test--repo "detach-repo")))
            ;; Local attach state cleared, foreign owner's claim untouched.
            (should-not (gethash canon dorg-dolt-server--attached-ports))
            (should (file-exists-p claim-file)))
        (delete-process (car listener))))))

(ert-deftest dorg-test-dolt-stop-stops-owned ()
  "`dorg-dolt-stop' stops a this-process owned server and clears registry."
  (dorg-test--with-dolt-server-runtime
    (let* ((proc (start-process "dorg-test-sleep" nil "sleep" "30"))
           (canon (dorg-dolt-server--canonical-dir (dorg-test--repo "stopowned-repo"))))
      (unwind-protect
          (progn
            (puthash canon (cons proc 4321) dorg-dolt-server--procs)
            (should (dorg-dolt-stop (dorg-test--repo "stopowned-repo")))
            (should-not (gethash canon dorg-dolt-server--procs))
            (should-not (process-live-p proc)))
        (when (process-live-p proc) (delete-process proc))))))

(ert-deftest dorg-test-dolt-stop-all-counts-owned ()
  "`dorg-dolt-stop-all' stops only this-process servers and returns count."
  (dorg-test--with-dolt-server-runtime
    (let ((p1 (start-process "dorg-test-sleep" nil "sleep" "30"))
          (p2 (start-process "dorg-test-sleep" nil "sleep" "30")))
      (unwind-protect
          (progn
            (puthash (dorg-dolt-server--canonical-dir (dorg-test--repo "a"))
                     (cons p1 1111) dorg-dolt-server--procs)
            (puthash (dorg-dolt-server--canonical-dir (dorg-test--repo "b"))
                     (cons p2 2222) dorg-dolt-server--procs)
            (should (= 2 (dorg-dolt-stop-all)))
            (should (= 0 (hash-table-count dorg-dolt-server--procs))))
        (when (process-live-p p1) (delete-process p1))
        (when (process-live-p p2) (delete-process p2))))))

(ert-deftest dorg-test-dolt-start-uses-ensure-detached ()
  "`dorg-dolt-start' delegates to `dorg-dolt-server--ensure-detached', returns port.
This is the explicit interactive start path: it launches a DETACHED daemon
\(see `dorg-dolt-server--ensure-detached').  Previously this mocked the
session-scoped session selector, which the start path has not used
since the detached-daemon change; the stale mock let the real ensure run."
  (let (ensure-dir)
    (cl-letf (((symbol-function 'dorg-dolt-server--ensure-detached)
               (lambda (dolt-dir) (setq ensure-dir dolt-dir) 4242)))
      (should (= 4242 (dorg-dolt-start (dorg-test--repo "start-repo"))))
      (should (equal ensure-dir (dorg-test--repo "start-repo"))))))

(ert-deftest dorg-test-read-dolt-dir-prefers-registered ()
  "`dorg-dolt-server--read-dolt-dir' prefers the buffer's registered Dolt dir."
  (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
             (lambda (&optional _buffer) (dorg-test--repo "registered"))))
    (should (equal (dorg-dolt-server--read-dolt-dir "unused: ")
                   (dorg-test--repo "registered")))))

;;; ─── Wedged sql-server detection & recovery ─────────────────────────────
;;
;; A *wedged* dolt sql-server still owns its listen socket, so the OS
;; completes the TCP accept and `dorg-dolt-server--port-live-p' returns t even
;; though the server never answers SQL.  `dorg-test--listen-port' is
;; exactly such a fixture: it accepts connections and never writes a byte
;; back.  These specs require detection beyond TCP accept, an escalating
;; kill, and a reap/replace chokepoint — none of which exist yet, so they
;; are red until this detection/recovery is implemented.

(defmacro dorg-test--within-budget (seconds &rest body)
  "Run BODY and assert it returns or non-locally exits within SECONDS.
Proves a path does not block indefinitely: captures wall-clock around
BODY (even when BODY signals) and `should's the elapsed time under
SECONDS, then re-raises any signal so the caller can also assert on it."
  (declare (indent 1))
  `(let ((dorg-test--budget-start (float-time)))
     (unwind-protect
         (progn ,@body)
       (should (< (- (float-time) dorg-test--budget-start) ,seconds)))))

(ert-deftest dorg-test-port-healthy-p-rejects-wedged ()
  "A TCP-accepting server that never answers SQL is live by TCP, unhealthy by probe.
`port-live-p' must still see the listen socket, but the bounded SQL
health probe must return nil within `dorg-dolt-server-health-timeout' — not
hang."
  (if (not (dorg-sql--mysql-available-p))
      (ert-skip "mysql.el unavailable; health probe degrades to TCP verdict")
    (let ((listener (dorg-test--listen-port)))
      (unwind-protect
          (let ((dorg-dolt-server-health-timeout 0.75)
                (port (cdr listener)))
            (should (dorg-dolt-server--port-live-p port))
            (dorg-test--within-budget 3.0
              (should-not (dorg-dolt-server--port-healthy-p port))))
        (delete-process (car listener))))))

(ert-deftest dorg-test-port-healthy-p-accepts-real-server ()
  "The health probe returns non-nil for a real, answering sql-server.
Non-vacuity guard for `dorg-test-port-healthy-p-rejects-wedged': proves
the probe is not simply always-nil."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p)) (ert-skip "mysql.el unavailable"))
   (t
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                 (port (dorg-dolt-server--start dolt-dir)))
            (unwind-protect
                (should (dorg-dolt-server--port-healthy-p port))
              (dorg-dolt-server--stop dolt-dir)))
        (dorg-test--delete-owned-root temp-base))))))

(ert-deftest dorg-test-claim-classify-deep-flags-wedged ()
  "Deep classify returns `wedged' for a TCP-live-but-unhealthy claim.
Shallow classify keeps the historical `live' verdict so the hot path is
unaffected; only an explicit deep classify pays for the SQL probe."
  (if (not (dorg-sql--mysql-available-p))
      (ert-skip "mysql.el unavailable; deep classify degrades to TCP verdict")
    (dorg-test--with-dolt-server-runtime
      (let ((listener (dorg-test--listen-port)))
        (unwind-protect
            (let* ((dorg-dolt-server-health-timeout 0.75)
                   (claim (progn
                            (dorg-test--write-claim (dorg-test--repo "wedged-repo")
                                                    :owner-pid 999999
                                                    :port (cdr listener))
                            (dorg-dolt-server--claim-read (dorg-test--repo "wedged-repo")))))
              ;; Shallow: a live TCP port still wins.
              (should (eq (dorg-dolt-server--claim-classify claim) 'live))
              ;; Deep: the SQL probe fails -> wedged.
              (dorg-test--within-budget 3.0
                (should (eq (dorg-dolt-server--claim-classify claim t) 'wedged))))
          (delete-process (car listener)))))))

(ert-deftest dorg-test-kill-pid-escalates-to-sigkill ()
  "`dorg-dolt-server--kill-pid' escalates TERM -> SIGKILL for a TERM-ignoring pid.
The fixture traps and ignores SIGTERM, so only SIGKILL reaps it; the call
must confirm the pid is gone within the grace window."
  (let* ((proc (start-process "dorg-test-term-ignore" nil
                              "sh" "-c" "trap '' TERM; sleep 30"))
         (pid (process-id proc)))
    (unwind-protect
        (progn
          (should (dorg-dolt-server--pid-live-p pid))
          (dorg-test--within-budget 5.0
            (dorg-dolt-server--kill-pid pid 1.5))
          ;; Give the OS a beat to reap, then assert gone.
          (accept-process-output nil 0.2)
          (should-not (process-live-p proc)))
      (when (process-live-p proc) (delete-process proc)))))

(ert-deftest dorg-test-reap-wedged-owned-kills-and-clears ()
  "Reaping an owned wedged server kills the process and removes its claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((listener (dorg-test--listen-port))
           (canon (dorg-dolt-server--canonical-dir (dorg-test--repo "reap-owned"))))
      (unwind-protect
          (progn
            ;; Register the listener as an owned server + matching claim.
            (puthash canon (cons (car listener) (cdr listener))
                     dorg-dolt-server--procs)
            (dorg-test--write-claim (dorg-test--repo "reap-owned")
                                    :owner-pid (emacs-pid)
                                    :port (cdr listener))
            (should (file-exists-p (dorg-dolt-server--claim-file
                                    (dorg-test--repo "reap-owned"))))
            (dorg-dolt-server--reap-wedged (dorg-test--repo "reap-owned"))
            (should-not (process-live-p (car listener)))
            (should-not (gethash canon dorg-dolt-server--procs))
            (should-not (file-exists-p (dorg-dolt-server--claim-file
                                        (dorg-test--repo "reap-owned")))))
        (when (process-live-p (car listener)) (delete-process (car listener)))))))

;; `dorg-test-reap-wedged-foreign-removes-claim' (cross-process foreign reap)
;; was RETIRED together with the behavior it exercised (tracked-regression, OC
;; option C): a PID read from a claim is a transport handle, never authority,
;; and a wedged server cannot pass served-store validation, so a same-host
;; foreign wedged claim is NEVER signalled and its claim file is left
;; byte-identical.  Its replacement is
;; `dorg-test-srvid-reap-wedged-foreign-refuses-preserves'.

(ert-deftest dorg-test-ensure-strict-replaces-wedged-owned ()
  "Strict `server' transport reaps an owned wedged server and starts a replacement.
Never returns CLI; the replacement runs on a fresh port and answers the
health probe."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p)) (ert-skip "mysql.el unavailable"))
   (t
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests"))
            (listener (dorg-test--listen-port)))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (canon (dorg-dolt-server--canonical-dir dolt-dir))
                   (dorg-dolt-server-health-timeout 0.75))
              ;; Pretend an owned server exists but is wedged (listener port).
              (puthash canon (cons (car listener) (cdr listener))
                       dorg-dolt-server--procs)
              (dorg-test--write-claim dolt-dir
                                      :owner-pid (emacs-pid)
                                      :port (cdr listener))
              (let ((new-port (dorg-dolt-server--ensure-strict dolt-dir)))
                (unwind-protect
                    (progn
                      (should (integerp new-port))
                      (should-not (= new-port (cdr listener)))
                      (should-not (process-live-p (car listener)))
                      (should (dorg-dolt-server--port-healthy-p new-port)))
                  (dorg-dolt-server--stop dolt-dir))))
          (when (process-live-p (car listener)) (delete-process (car listener)))
          (dorg-test--delete-owned-root temp-base)))))))

;;; ─── Managed-Server Store Validation ───────────────────────────────────
;;
;; A DOrg-managed sql-server (or a foreign claim we are about to trust) must
;; prove it serves the EXPECTED store, not merely a database of the expected
;; name.  Two new gates, beyond the name-only served-set membership test
;; (`dorg-dolt-server--db-in-served-p' over a `--list-served-dbs' snapshot):
;;   1. DOrg schema marker: the served database must contain `dorg_files'.
;;   2. Bound-file identity (when the start/ensure is file-backed): the
;;      served `dorg_files' must contain a row for the canonical bound file
;;      (and matching root id) the registration expects.
;; Both gates apply at POST-START (before upgrading the provisional claim) and at
;; ATTACH-TIME (before routing reads/writes through a foreign claim).  On
;; failure the started process is killed and its claim removed after confirmed
;; death / the attach is refused, no ready claim is published or trusted, and a
;; bounded `dorg-sql-transport-error' is signaled — never a silent CLI fallback
;; against a claimed repo.

(ert-deftest dorg-test-dolt-server-start-fails-closed-on-missing-schema ()
  "A dolt-dir lacking the DOrg schema marker fails closed before any server.
Store validation runs PRE-START against the dolt-dir (one-shot CLI), so a
dir without the `dorg_files' marker is rejected before a server is launched:
DOrg signals a bounded `dorg-sql-transport-error' naming the missing marker,
starts NO server, publishes NO claim, and never falls back to CLI.
Non-vacuous: the post-start served-DB gate would pass; only the pre-start
schema-marker gate rejects.  Pre-start validation avoids the fresh-server
data-query deadlock by querying the dir directly."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (dorg-test--make-owned-temp-dir "dorg-noschema-" "dolt-server-tests"))
           (started nil)
           (published nil))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ;; The dolt-dir store lacks the `dorg_files' marker.
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) (list :reason 'no-schema
                                               :tables '("information_schema"))))
                      ;; Not file-backed for this case.
                      ((symbol-function 'dorg-dolt-server--expected-binding)
                       (lambda (&rest _) nil))
                      ;; A server must never be launched once validation fails.
                      ((symbol-function 'start-process)
                       (lambda (&rest _) (setq started t)
                         (error "start-process must not be called")))
                      ((symbol-function 'dorg-dolt-server--publish-claim)
                       (lambda (&rest _) (setq published t))))
              (let ((err (should-error (dorg-dolt-server--start temp-base)
                                       :type 'dorg-sql-transport-error)))
                ;; Bounded diagnostic naming the missing DOrg schema marker.
                (should (string-match-p "dorg_files" (cadr err)))
                (should (string-match-p "published no claim" (cadr err)))))
            ;; Started NO server ...
            (should-not started)
            ;; ... published NO ownership claim ...
            (should-not published)
            ;; ... left no claim file behind ...
            (should-not (dorg-dolt-server-claim temp-base))
            ;; ... and registered no owned server (no silent CLI fallback).
            (should-not (dorg-dolt-server-port temp-base)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-start-fails-closed-on-wrong-bound-file ()
  "A file-backed start whose dolt-dir lacks the bound-file row fails closed.
The dolt-dir carries the `dorg_files' marker, but `dorg_files' has no row
for the canonical bound file/root the registration expects — the exact
masquerade in the parent incident (a store that does not hold the bound
tracker file).  Pre-start validation rejects it: signal a bounded
diagnostic naming the bound file, start NO server, publish NO claim.
Non-vacuous: the schema gate passes; only the bound-file gate rejects."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (dorg-test--make-owned-temp-dir "dorg-wrongbound-" "dolt-server-tests"))
           (started nil)
           (published nil)
           (bound-file "/private/cosmic/dev/dorg/.tracker/beads.org"))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ;; File-backed start expecting this bound file ...
                      ((symbol-function 'dorg-dolt-server--expected-binding)
                       (lambda (&rest _) (list :bound-file bound-file :root-id 123)))
                      ;; ... schema present but no row for the bound file/root.
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) (list :reason 'wrong-bound-file
                                               :tables '("dorg_files" "headings")
                                               :bound-file bound-file)))
                      ((symbol-function 'start-process)
                       (lambda (&rest _) (setq started t)
                         (error "start-process must not be called")))
                      ((symbol-function 'dorg-dolt-server--publish-claim)
                       (lambda (&rest _) (setq published t))))
              (let ((err (should-error (dorg-dolt-server--start temp-base)
                                       :type 'dorg-sql-transport-error)))
                ;; Bounded diagnostic naming the unmatched bound file.
                (should (string-match-p "beads\\.org" (cadr err)))))
            (should-not started)
            (should-not published)
            (should-not (dorg-dolt-server-claim temp-base))
            (should-not (dorg-dolt-server-port temp-base)))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-start-publishes-on-valid-bound-store ()
  "A file-backed start whose store passes every gate publishes its claim.
DB name served, `dorg_files' marker present, and the bound-file row found:
validation must not over-reject — the claim is published and the started
process kept.  Guards the schema/bound-file gates against a false-negative
regression that would refuse a healthy bound store."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (dorg-test--make-owned-temp-dir "dorg-validbound-" "dolt-server-tests"))
           (orig-start (symbol-function 'start-process))
           (killed nil)
           (published nil)
           (bound-file "/private/cosmic/dev/dorg/.tracker/beads.org"))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ((symbol-function 'start-process)
                       (lambda (name buffer &rest _args)
                         (funcall orig-start name buffer "sleep" "30")))
                      ((symbol-function 'dorg-dolt-server--wait-ready)
                       (lambda (&rest _) t))
                      ;; Pre-start dir validation passes (schema + bound file) ...
                      ((symbol-function 'dorg-dolt-server--expected-binding)
                       (lambda (&rest _) (list :bound-file bound-file :root-id 123)))
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) t))
                      ;; ... and the launched server's served SNAPSHOT holds the
                      ;; DB our dolt-dir maps to, so the pure membership
                      ;; predicate passes the served-DB gate.
                      ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                       (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                      ((symbol-function 'dorg-dolt-server--list-served-dbs)
                       (lambda (&rest _)
                         (list "information_schema"
                               (cdr (dorg-dolt-server--data-dir-and-db
                                     temp-base)))))
                      ((symbol-function 'dorg-dolt-server--kill-process)
                       (lambda (_proc) (setq killed t)))
                      ((symbol-function 'dorg-dolt-server--publish-claim)
                       (lambda (&rest _) (setq published t))))
              (dorg-dolt-server--start temp-base))
            ;; Validation passed: claim published, process not killed.
            (should published)
            (should-not killed))
        (ignore-errors (dorg-dolt-server--stop temp-base))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-ensure-strict-rejects-invalid-store-claim ()
  "Strict transport refuses to attach to a live claim serving the wrong store.
A foreign claim classifies live and protocol-compatible, but its
sql-server carries no DOrg schema marker (wrong/empty store — the
masquerade in the parent incident).  Strict `server' transport must
validate the served store before attaching: signal
`dorg-sql-transport-error', register NO attached port, and never fall back
to CLI against the claimed repo.  Non-vacuous: classify and compatibility
both pass; only store validation rejects."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-invalid"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65111)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ;; The claim's server serves the expected DB (served SNAPSHOT
                ;; holds it) but its store carries no DOrg schema marker: the
                ;; masquerade must be refused at the SCHEMA gate, not wrong-db.
                ;; Tri-state seam (tracked-regression): lifecycle paths probe via
                ;; `--probe-served-dbs'; wrap the served snapshot as a CONFIRMED
                ;; result so this pre-tri-state stub still drives the same gate.
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                ((symbol-function 'dorg-dolt-server--list-served-dbs)
                 (lambda (&rest _)
                   (list (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))))
                ((symbol-function 'dorg-dolt-server--list-tables)
                 (lambda (&rest _) '("information_schema")))
                ((symbol-function 'dorg-dolt-server--expected-binding)
                 (lambda (&rest _) nil)))
        (let ((err (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                                 :type 'dorg-sql-transport-error)))
          ;; Refused at the schema gate — the served-DB gate already passed.
          (should (string-match-p "missing the DOrg schema marker" (cadr err))))
        ;; No attached port registered: nothing routes through the bad claim.
        (should-not (gethash canon dorg-dolt-server--attached-ports))))))

(ert-deftest dorg-test-ensure-strict-attaches-valid-store-claim ()
  "Strict transport attaches to a live claim whose served store validates.
DB served and `dorg_files' marker present: validation passes, so the
foreign port is attached and returned.  Guards against over-rejection of a
healthy foreign owner."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-valid"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (port 65222))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port port)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ;; Served SNAPSHOT holds the DB our dolt-dir maps to (pure
                ;; predicate passes the served-DB gate) and `dorg_files' is
                ;; present, so every gate validates and the foreign port attaches.
                ;; Tri-state seam (tracked-regression): lifecycle paths probe via
                ;; `--probe-served-dbs'; wrap the served snapshot as a CONFIRMED
                ;; result so this pre-tri-state stub still drives the same gate.
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                ((symbol-function 'dorg-dolt-server--list-served-dbs)
                 (lambda (&rest _)
                   (list (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))))
                ((symbol-function 'dorg-dolt-server--list-tables)
                 (lambda (&rest _) '("dorg_files" "headings")))
                ((symbol-function 'dorg-dolt-server--expected-binding)
                 (lambda (&rest _) nil)))
        (should (= (dorg-dolt-server--ensure-strict dolt-dir) port))
        (should (= (gethash canon dorg-dolt-server--attached-ports) port))))))

;;; ─── Public port adoption (tracked-case) ────────────────────────────────────

(ert-deftest dorg-test-adopt-port-registers-attached-client ()
  "`dorg-dolt-server-adopt-port' records a client attachment the hot path reuses.
The inherited port lands in `--attached-ports' (a CLIENT cache), never in the
`--procs' owner table, and strict `server' transport returns it with no
start/attach — exactly what the async worker needs when it inherits a
parent-resolved managed-server port."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "adopt-client"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (port 65111)
           (dorg-dolt-server-autostart nil))
      (should (= port (dorg-dolt-server-adopt-port dolt-dir port)))
      ;; Client cache, not owner table.
      (should (= port (gethash canon dorg-dolt-server--attached-ports)))
      (should (= 0 (hash-table-count dorg-dolt-server--procs)))
      (should-not (dorg-dolt-server-port dolt-dir))
      ;; Hot query path returns the adopted port directly — any fall-through to
      ;; attach/ensure signals so a lost registration cannot pass silently.
      (cl-letf (((symbol-function 'dorg-dolt-server--attach-only)
                 (lambda (&rest _) (error "must not attach: adopted port not reused")))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (error "must not ensure: adopted port not reused"))))
        (should (= port (dorg-dolt-server--strict-port dolt-dir)))))))

(ert-deftest dorg-test-adopt-port-rejects-nonpositive ()
  "`dorg-dolt-server-adopt-port' rejects an out-of-TCP-range / non-integer PORT.
Valid ports are integers in 1..65535.  A bogus port must never be cached, or
the hot path would route SQL nowhere (or to an impossible port)."
  (dorg-test--with-dolt-server-runtime
    (let ((dolt-dir (dorg-test--repo "adopt-bad")))
      (should-error (dorg-dolt-server-adopt-port dolt-dir 0)
                    :type 'wrong-type-argument)
      (should-error (dorg-dolt-server-adopt-port dolt-dir nil)
                    :type 'wrong-type-argument)
      ;; Above the TCP maximum (65535) — must be rejected, not cached, since
      ;; adoption places PORT directly on the no-probe hot path.
      (should-error (dorg-dolt-server-adopt-port dolt-dir 65536)
                    :type 'wrong-type-argument)
      (should (= 0 (hash-table-count dorg-dolt-server--attached-ports))))))

;;; ─── Served-Set Snapshot Consistency (tracked-case) ──────────────────────────
;;
;; The parent incident: a transport-policy refusal that was self-contradictory
;; — "does not serve the expected database \"dolt\" (served: dolt, ...)".  The
;; refusal named `dolt' in its OWN served-set.  Root cause: the membership
;; decision and the diagnostic were computed from DIFFERENT served-DB
;; snapshots.  The pre-fix decision sites fetched snapshot A via
;; `dorg-dolt-server--list-served-dbs' and tested membership (through the
;; now-removed `db-served-p' name-only wrapper); on FALSE, the
;; diagnostic re-fetched snapshot B.  A transient empty A followed by a healthy
;; B (a lane's server mid-restart) made the verdict `wrong-db' while the
;; message listed the DB as served.  Three sites shared this double-probe:
;; `dorg-dolt-server--store-valid-p', `dorg-dolt-server--start-child-unsafe'
;; (via `--start'), and `dorg-dolt-server--start-detached-unsafe'.
;;
;; The contract these tests pin: ONE served-list snapshot per decision, reused
;; for both the membership decision and its diagnostic, and a PURE
;; case/whitespace-insensitive membership predicate over that single snapshot.
;; RED before the narrow fix; GREEN after.

(defun dorg-test--served-source (&rest snapshots)
  "Return a stateful thunk yielding SNAPSHOTS in order (last repeats).
Models a served-DB probe source whose answer CHANGES between successive
calls — the transient the parent incident depended on — while counting how
many times it is consulted.  Returns a cons (THUNK . COUNTER-CELL); the
counter cell's car is the live call count."
  (let ((remaining snapshots)
        (counter (list 0)))
    (cons (lambda (&rest _)
            (setcar counter (1+ (car counter)))
            (if (cdr remaining) (pop remaining) (car remaining)))
          counter)))

(ert-deftest dorg-test-dolt-server-db-in-served-p-pure-normalized-membership ()
  "`dorg-dolt-server--db-in-served-p' is a PURE case/whitespace-insensitive
membership predicate over an EXPLICIT served snapshot: it issues no query of
its own, and it accepts case and surrounding-whitespace variants on BOTH the
served names and the wanted name.  This is the reusable predicate each
decision site applies to its single served-set snapshot so the
same immutable snapshot backs both the decision and the diagnostic.
Non-vacuous: undefined until the fix adds it, and a whitespace/case-blind or
query-issuing implementation fails."
  (cl-letf (((symbol-function 'dorg-dolt-server--list-served-dbs)
             (lambda (&rest _)
               (error "db-in-served-p must not issue its own served-DB query"))))
    ;; Present in the snapshot — exact, case, whitespace, and both-sides.
    (should (dorg-dolt-server--db-in-served-p '("dolt" "information_schema") "dolt"))
    (should (dorg-dolt-server--db-in-served-p '("DOLT") "dolt"))
    (should (dorg-dolt-server--db-in-served-p '("  dolt  ") "dolt"))
    (should (dorg-dolt-server--db-in-served-p '("dolt") "  DOLT "))
    ;; Genuinely absent, and the empty/nil snapshot.
    (should-not (dorg-dolt-server--db-in-served-p '("information_schema" "mysql") "dolt"))
    (should-not (dorg-dolt-server--db-in-served-p nil "dolt"))))

(ert-deftest dorg-test-dolt-server-store-valid-p-single-served-snapshot ()
  "`dorg-dolt-server--store-valid-p' fetches the served-DB list exactly ONCE
per decision and reports that SAME snapshot in its diagnostic.  With a probe
source that returns an empty snapshot first and a healthy one next, the
pre-fix double-probe consulted the source twice: it decided `wrong-db' on the
empty A yet reported the healthy B (containing \"dolt\") — the exact
self-contradiction.  The fix makes exactly one call, so a `wrong-db' verdict's
own `:served-dbs' can never contain the DB it just refused.
Non-vacuous: RED against the pre-fix double-probe (calls = 2, served-set holds
\"dolt\")."
  (let* ((src (dorg-test--served-source nil '("dolt" "information_schema" "mysql")))
         (counter (cdr src)))
    (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
               (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
              ((symbol-function 'dorg-dolt-server--list-served-dbs) (car src))
              ;; Isolate the served-DB gate; never reached on an empty snapshot.
              ((symbol-function 'dorg-dolt-server--schema-present-p)
               (lambda (&rest _) t)))
      (let ((verdict (dorg-dolt-server--store-valid-p 64009 "dolt")))
        ;; One snapshot consumed for this decision ...
        (should (= (car counter) 1))
        ;; ... the decision is `wrong-db' (the only snapshot was empty) ...
        (should (eq (plist-get verdict :reason) 'wrong-db))
        ;; ... and the diagnostic reports THAT snapshot — never listing the DB
        ;; it refused.
        (should-not (member "dolt" (plist-get verdict :served-dbs)))))))

(ert-deftest dorg-test-dolt-server-refusal-message-not-self-contradictory ()
  "A `wrong-db' refusal message never lists the expected DB in its own
`served:' clause.  Reproduces the parent-incident string shape: rendering
`dorg-dolt-server--store-invalid-message' for a verdict produced under a
changing probe source must not both claim database \"dolt\" is not served AND
print \"dolt\" in the served set.
Non-vacuous: RED against the pre-fix double-probe, whose message read
\"does not serve the expected database \\\"dolt\\\" (served: dolt, ...)\"."
  (let* ((src (dorg-test--served-source nil '("dolt" "information_schema" "mysql"))))
    (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
               (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
              ((symbol-function 'dorg-dolt-server--list-served-dbs) (car src))
              ((symbol-function 'dorg-dolt-server--schema-present-p)
               (lambda (&rest _) t)))
      (let* ((verdict (dorg-dolt-server--store-valid-p 64009 "dolt"))
             (msg (dorg-dolt-server--store-invalid-message
                   verdict "/private/cosmic/dev/dorg/.tracker/dolt/" "dolt" 64009)))
        (should (eq (plist-get verdict :reason) 'wrong-db))
        ;; The refusal must not name `dolt' in its served clause.
        (should-not (string-match-p "served:[^)]*\\bdolt\\b" msg))))))

(ert-deftest dorg-test-dolt-server-start-single-served-snapshot ()
  "The post-start served-DB gate (`dorg-dolt-server--start-child-unsafe' via
`dorg-dolt-server--start') consults the served list exactly ONCE in its
refusal arm and reports that same snapshot.  Same double-probe as
`store-valid-p': a changing probe source made the launched-server refusal name
`dolt' in its own served clause.  The started process is killed and its
provisional claim is removed after confirmed death; the refusal is not
self-contradictory.
Non-vacuous: RED against the pre-fix double-probe (calls = 2, message lists
\"dolt\")."
  (dorg-test--with-dolt-server-runtime
    (let* ((temp-base (dorg-test--make-owned-temp-dir "dorg-q42zl-start-" "dolt-server-tests"))
           (orig-start (symbol-function 'start-process))
           (src (dorg-test--served-source nil '("dolt" "information_schema" "mysql")))
           (counter (cdr src))
           ;; The exact child the (unstubbed) production refusal must reap; we
           ;; capture it so the unwind can guarantee no child is orphaned even
           ;; if an assertion aborts mid-flow.
           (child nil))
      (unwind-protect
          (let ((dorg-dolt--version-cache
                 (list :version dorg-dolt-minimum-version :raw "stubbed")))
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (&rest _) "/usr/bin/dolt"))
                      ;; A long-lived child so "no child remains" is a real
                      ;; proof the refusal reaped it (not a self-exit no-op).
                      ((symbol-function 'start-process)
                       (lambda (name buffer &rest _args)
                         (setq child (funcall orig-start name buffer "sleep" "30"))))
                      ((symbol-function 'dorg-dolt-server--wait-ready)
                       (lambda (&rest _) t))
                      ((symbol-function 'dorg-dolt-server--expected-binding)
                       (lambda (&rest _) nil))
                      ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                       (lambda (&rest _) t))
                      ;; Post-start served-DB probe changes between calls.
                      ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                       (lambda (port)
                         (cons 'served (dorg-dolt-server--list-served-dbs port))))
                      ((symbol-function 'dorg-dolt-server--list-served-dbs)
                       (car src)))
              ;; `kill-process' is NOT stubbed: the production refusal must
              ;; genuinely reap the child it started and remove the provisional
              ;; claim only after death is confirmed.
              (let ((err (should-error (dorg-dolt-server--start temp-base)
                                       :type 'dorg-sql-transport-error)))
                ;; One served-list snapshot in the refusal arm ...
                (should (= (car counter) 1))
                ;; ... refusal does not name `dolt' as served ...
                (should-not (string-match-p "served:[^)]*\\bdolt\\b" (cadr err)))))
            ;; ... the launched child was reaped by the production refusal
            ;; (no orphaned server process) and its provisional claim removed.
            (should child)
            (should-not (process-live-p child))
            (should-not (dorg-dolt-server-claim temp-base)))
        ;; Safety net: never leak the child, whatever failed above.
        (when (process-live-p child) (delete-process child))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-server-detached-start-single-served-snapshot ()
  "The detached-daemon served-DB gate
\(`dorg-dolt-server--start-detached-unsafe') consults the served list exactly
ONCE in its refusal arm and reports that same snapshot — the third site that
shared the double-probe.  A changing probe source made the daemon refusal name
`dolt' in its own served clause; the daemon is killed, its provisional claim is
removed after confirmed death, and the refusal is not self-contradictory.
Non-vacuous: RED against the pre-fix double-probe (calls = 2, message lists
\"dolt\")."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "q42zl-detached"))
           (src (dorg-test--served-source nil '("dolt" "information_schema" "mysql")))
           (counter (cdr src))
           (killed nil)
           (published-states nil)
           (orig-publish (symbol-function 'dorg-dolt-server--publish-claim)))
      (cl-letf (((symbol-function 'dorg-dolt-server--expected-binding)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--free-port)
                 (lambda (&rest _) 64010))
                ((symbol-function 'dorg-dolt-server--detached-launch)
                 (lambda (&rest _) 999999))
                ((symbol-function 'dorg-dolt-server--pid-live-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--port-live-p)
                 (lambda (&rest _) t))
                ;; Post-start served-DB probe changes between calls.
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (port)
                   (cons 'served (dorg-dolt-server--list-served-dbs port))))
                ((symbol-function 'dorg-dolt-server--list-served-dbs)
                 (car src))
                ((symbol-function 'dorg-dolt-server--kill-pid)
                 (lambda (&rest _) (setq killed t)))
                ((symbol-function 'dorg-dolt-server--publish-claim)
                 (lambda (&rest args)
                   (push (nth 8 args) published-states)
                   (apply orig-publish args))))
        (let ((err (should-error (dorg-dolt-server--start-detached-unsafe dolt-dir)
                                 :type 'dorg-sql-transport-error)))
          ;; One served-list snapshot in the refusal arm ...
          (should (= (car counter) 1))
          ;; ... refusal does not name `dolt' as served ...
          (should-not (string-match-p "served:[^)]*\\bdolt\\b" (cadr err))))
        ;; ... one provisional owner was published before the probe, then the
        ;; confirmed-dead daemon's claim was removed.
        (should killed)
        (should (equal published-states '(probe-inconclusive)))
        (should-not (dorg-dolt-server-claim dolt-dir))))))

;;; ─── Shared live-attach heart (tracked-case) ──────────────────────────────
;;
;; Characterization coverage for the ONE live/compatible attach core the three
;; attaching selectors delegate to (`dorg-dolt-server--attach-live' behind
;; `--ensure-strict', `--attach-only', `--ensure-detached').  These pin the
;; behavior the shared core must preserve BYTE-FOR-BYTE and guard it from both
;; sides:
;;   - the dolt-native UUID pre-check signals in BOTH fail-closed selectors
;;     (currently UNtested — no existing test exercises the "changed instance"
;;     signal), proving the shared `--assert-uuid-current' fires identically;
;;   - a wrong-store verdict fails closed in strict/attach-only (shared
;;     `--signal-store-invalid') vs breaks+replaces via the selector's own
;;     start primitive in `--ensure-detached' (shared `--attach-invalid-replace');
;;   - an inconclusive probe PRESERVES the live owner (no break, no start) in
;;     the tri-state selectors, with the selector-specific suffix.

(defun dorg-test--foreign-live-claim (dolt-dir port &rest overrides)
  "Build a live foreign-owner claim alist for DOLT-DIR on PORT.
OVERRIDES is a plist; :owner-kind defaults to `dolt' (the kind that arms the
sql-server.info UUID guard) and :uuid defaults nil."
  (list (cons :dolt-dir (dorg-dolt-server--canonical-dir dolt-dir))
        (cons :host (system-name))
        (cons :owner-pid 999999)
        (cons :server-pid 999999)
        (cons :owner-kind (or (plist-get overrides :owner-kind) 'dolt))
        (cons :port port)
        (cons :token "foreign-token")
        (cons :policy 'server)
        (cons :protocol dorg-dolt-server-claim-protocol)
        (cons :uuid (plist-get overrides :uuid))
        (cons :created-at (float-time))
        (cons :last-used (float-time))))

(defmacro dorg-test--with-live-claim (dolt-dir port &rest body)
  "Drive a live/compatible foreign owner for DOLT-DIR on PORT through BODY.
Stubs the claim discovery/classification seams so the selector under test
reaches its `live'/compatible attach arm deterministically without a real
server.  BODY adds the per-case verdict/identity stubs and assertions."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
             ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
             ((symbol-function 'dorg-dolt-server--effective-claim)
              (lambda (&rest _) (dorg-test--foreign-live-claim ,dolt-dir ,port)))
             ((symbol-function 'dorg-dolt-server--claim-classify) (lambda (&rest _) 'live))
             ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) nil))
             ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (&rest _) t))
             ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (&rest _) nil)))
     ,@body))

;; ── UUID pre-check: both fail-closed selectors signal when the dolt-native
;;    sql-server.info instance moved between discovery and attach. ──

(ert-deftest dorg-test-ensure-strict-signals-on-dolt-uuid-moved ()
  "Strict transport refuses to attach a dolt owner whose instance UUID moved.
A foreign `dolt' owner discovered via sql-server.info that is no longer the
current instance (pid/port reused) must not be silently trusted: attach
signals `dorg-sql-transport-error' (\"changed instance ... during attach\")
and memoizes nothing."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "uuid-moved-strict"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--with-live-claim dolt-dir 65301
        (cl-letf (((symbol-function 'dorg-dolt-server--dolt-uuid-current-p)
                   (lambda (&rest _) nil))
                  ;; store-valid-p must NOT be reached — the UUID guard precedes it.
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (error "store validation reached before UUID guard"))))
          (let ((err (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                                   :type 'dorg-sql-transport-error)))
            (should (string-match-p "changed instance" (cadr err))))
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

(ert-deftest dorg-test-attach-only-signals-on-dolt-uuid-moved ()
  "Attach-only transport signals identically when the dolt instance UUID moved.
Same guard as strict — proves the shared UUID pre-check fires in BOTH
fail-closed selectors, not just one."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "uuid-moved-attach"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--with-live-claim dolt-dir 65302
        (cl-letf (((symbol-function 'dorg-dolt-server--dolt-uuid-current-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (error "store validation reached before UUID guard"))))
          (let ((err (should-error (dorg-dolt-server--attach-only dolt-dir)
                                   :type 'dorg-sql-transport-error)))
            (should (string-match-p "changed instance" (cadr err))))
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

;; ── Wrong-store verdict: strict/attach-only FAIL CLOSED (signal, no start);
;;    `--ensure-detached' BREAKs + starts a replacement. ──

(ert-deftest dorg-test-ensure-strict-fails-closed-on-wrong-store ()
  "Strict transport refuses a live owner serving the WRONG store, no CLI fallback.
UUID is current but the owner does not serve the expected DB: strict signals
`dorg-sql-transport-error' (\"refusing to attach or fall back to CLI\"),
starts nothing, and memoizes nothing."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "wrong-store-strict"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--with-live-claim dolt-dir 65303
        (cl-letf (((symbol-function 'dorg-dolt-server--dolt-uuid-current-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (list :reason 'wrong-db :served-dbs '("other"))))
                  ((symbol-function 'dorg-dolt-server--start-unsafe)
                   (lambda (&rest _) (error "must not start over a live owner"))))
          (let ((err (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                                   :type 'dorg-sql-transport-error)))
            (should (string-match-p "refusing to attach or fall back to CLI" (cadr err))))
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

(ert-deftest dorg-test-attach-only-fails-closed-on-wrong-store ()
  "Attach-only transport refuses a live owner serving the WRONG store.
Same fail-closed disposition as strict — proves the shared store-invalid
signal (\"refusing to attach or fall back to CLI\") fires in both."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "wrong-store-attach"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--with-live-claim dolt-dir 65304
        (cl-letf (((symbol-function 'dorg-dolt-server--dolt-uuid-current-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (list :reason 'wrong-db :served-dbs '("other")))))
          (let ((err (should-error (dorg-dolt-server--attach-only dolt-dir)
                                   :type 'dorg-sql-transport-error)))
            (should (string-match-p "refusing to attach or fall back to CLI" (cadr err))))
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

(ert-deftest dorg-test-ensure-detached-replaces-on-confirmed-wrong-store ()
  "Detached ensure breaks a CONFIRMED wrong-store claim and starts a fresh daemon.
A live owner whose served DB is confirmed wrong is not the legitimate owner:
the (file-only) claim is broken and a replacement DETACHED server is started
via `--start-detached-unsafe' (never `--start-unsafe')."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "replace-detached"))
           (broke nil))
      (dorg-test--with-live-claim dolt-dir 65305
        (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (list :reason 'wrong-db :served-dbs '("other"))))
                  ((symbol-function 'dorg-dolt-server--break-claim)
                   (lambda (&rest _) (setq broke t)))
                  ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                   (lambda (&rest _) 55501))
                  ((symbol-function 'dorg-dolt-server--start-unsafe)
                   (lambda (&rest _) (error "detached path must not use --start-unsafe"))))
          (should (= (dorg-dolt-server--ensure-detached dolt-dir) 55501))
          (should broke))))))

;; ── Inconclusive probe: PRESERVE the live owner (no break, no start), raise
;;    with the selector-specific suffix. ──

(ert-deftest dorg-test-ensure-detached-preserves-owner-on-inconclusive ()
  "An inconclusive served-DB probe preserves the detached daemon and its claim.
A transient probe failure is NOT a confirmed wrong store: the live owner is
neither reaped nor replaced — the call signals `dorg-sql-transport-error'
(\"preserved the live detached daemon\") with zero break and zero start."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "inconclusive-detached"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (dorg-test--with-live-claim dolt-dir 65307
        (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (list :reason 'probe-inconclusive :cause "refused")))
                  ((symbol-function 'dorg-dolt-server--break-claim)
                   (lambda (&rest _) (error "must not break a live claim on inconclusive")))
                  ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                   (lambda (&rest _) (error "must not start over a preserved live owner"))))
          (let ((err (should-error (dorg-dolt-server--ensure-detached dolt-dir)
                                   :type 'dorg-sql-transport-error)))
            (should (string-match-p "preserved the live detached daemon" (cadr err))))
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

;;; ─── Project-Root vs Nested .tracker/dolt Split ─────────────────────────
;;
;; Regression coverage for the managed-server claim incident: a DOrg-managed sql-server
;; must claim and serve the NESTED `.tracker/dolt' store bound via
;; `#+DORG_DOLT_DIR', never the project-root `.dolt'; and a root repo with no
;; DOrg schema must fail before it can publish a claim or leave a live server.
;; These exercise the merged fix (explicit --data-dir + served-store
;; validation) end to end with real Dolt repos.

(ert-deftest dorg-test-dolt-server-split-claims-nested-tracker-dolt ()
  "A server for a `#+DORG_DOLT_DIR'-bound file claims the NESTED store.
Project with a root `.dolt' (raw `dolt init', no DOrg schema) and a nested
`.tracker/dolt' (full DOrg schema) backing `.tracker/beads.org' via a
`#+DORG_DOLT_DIR: dolt' preamble.  Starting a server for the bound file
must publish a claim for the canonical nested `.tracker/dolt/' — never the
project root — recording db `dolt' and data-dir the nested store itself
\(so sql-server artifacts stay inside it, not in `.tracker/'); the served
store exposes the `dorg_files' marker; `dorg-dolt-server-list' reports the
nested canonical path.  Non-vacuous: the nested canonical path is distinct
from both the root repo dir and the project dir, so a root-claiming
regression fails the equality."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let* (;; Real `dorg-dolt-server--start' cold start; give it generous
             ;; headroom so a loaded host does not trip the startup timeout.
             (dorg-dolt-server-startup-timeout 120)
             ;; `dorg-bind-dolt' below runs root-creation SQL against a
             ;; not-yet-served store; enable implicit autostart so that setup
             ;; SQL (not the claim-splitting behavior under test) succeeds.
             (dorg-dolt-server-autostart t)
             (proj (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-split-" "dolt-server-tests")))
             (tracker (file-name-as-directory (expand-file-name ".tracker" proj)))
             (beads (expand-file-name "beads.org" tracker))
             (root-repo (file-name-as-directory (expand-file-name "root" proj)))
             nested-dolt)
        (unwind-protect
            (let ((dorg--registry nil))
              (make-directory tracker t)
              ;; Project-root repo: a bare non-DOrg Dolt repo.
              (make-directory root-repo t)
              (let ((default-directory root-repo))
                (call-process "dolt" nil nil nil "init"
                              "--name" "t" "--email" "t@t"))
              ;; Nested DOrg store backing the tracker file.
              (setq nested-dolt (dorg-test--copy-repo tracker "dolt"))
              (with-temp-file beads
                (insert "#+DORG_DOLT_DIR: dolt\n* TODO bead\n"))
              ;; The store that will be served exposes the DOrg schema marker
              ;; (checked PRE-start against the dir, deadlock-free).
              (should (cl-some
                       (lambda (row)
                         (equal "dorg_files"
                                (downcase (format "%s" (cdar row)))))
                       (dorg-dolt-server--dir-sql-rows nested-dolt "SHOW TABLES")))
              (with-current-buffer (find-file-noselect beads)
                (unwind-protect
                    (let* ((bound (dorg--buffer-dolt-dir))
                           (canon-nested (dorg-dolt-server--canonical-dir bound)))
                      ;; The preamble binds the NESTED store, not the root.
                      (should (equal canon-nested
                                     (dorg-dolt-server--canonical-dir nested-dolt)))
                      (dorg-bind-dolt beads bound)
                      ;; Public `auto' may leave a detached daemon after the
                      ;; bind/root-creation SQL.  v0.1 lifecycle is manual, so
                      ;; stop it explicitly before this test drives the
                      ;; session-owned start path under inspection.
                      (dorg-dolt-stop bound)
                      (let ((port (dorg-dolt-server--start bound)))
                        (unwind-protect
                            (let ((claim (dorg-dolt-server-claim bound)))
                              (should (integerp port))
                              ;; Claim targets the nested canonical store ...
                              (should claim)
                              (should (equal (alist-get :dolt-dir claim)
                                             canon-nested))
                              ;; ... never the project root or its repo dir.
                              (should-not (equal (alist-get :dolt-dir claim)
                                                 (dorg-dolt-server--canonical-dir proj)))
                              (should-not (equal (alist-get :dolt-dir claim)
                                                 (dorg-dolt-server--canonical-dir root-repo)))
                              (should (equal (alist-get :db-name claim) "dolt"))
                              ;; data-dir is the nested store itself (artifacts
                              ;; stay inside it), never the parent `.tracker/'.
                              ;; Normalize trailing slashes before comparing.
                              (should (equal (file-truename
                                              (directory-file-name
                                               (alist-get :data-dir claim)))
                                             (file-truename
                                              (directory-file-name nested-dolt))))
                              (should-not (equal (file-truename
                                                  (directory-file-name
                                                   (alist-get :data-dir claim)))
                                                 (file-truename
                                                  (directory-file-name tracker))))
                              ;; The LIVE server exposes the DOrg schema over
                              ;; the wire: a post-start `SHOW TABLES FROM dolt'
                              ;; finds the `dorg_files' marker (which also
                              ;; proves db `dolt' is served).  A single probe
                              ;; only — stacking back-to-back one-shot CLI
                              ;; queries can wedge a freshly-started server.
                              (should (dorg-dolt-server--schema-present-p port "dolt"))
                              ;; dorg-dolt-server-list reports the nested canonical path.
                              (should (cl-find canon-nested (dorg-dolt-server-list)
                                               :key (lambda (e)
                                                      (plist-get e :dolt-dir))
                                               :test #'equal)))
                          (dorg-dolt-server--stop bound))))
                  (set-buffer-modified-p nil)
                  (kill-buffer))))
          (dorg-test--delete-owned-root proj))))))

(ert-deftest dorg-test-dolt-server-split-root-no-schema-fails-before-claim ()
  "Starting a server for a no-DOrg-schema root repo fails before any claim.
A bare `dolt init' repo (no `dorg_files') must make `dorg-dolt-server--start'
signal `dorg-sql-transport-error' from pre-start validation and leave NO
claim file, NO owned port, NO `.dolt/sql-server.info', and NO live process
— so future `auto' transport cannot mistake it for a valid DOrg server.
Executable non-vacuity: with `dorg-dolt-server--dir-store-valid-p' stubbed to t
(the pre-validation behavior), the SAME start instead launches a server and
publishes a claim — the old wrong-claim shape — proving the validation gate
is load-bearing."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let* ((proj (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-split-root-" "dolt-server-tests")))
             (root-repo (file-name-as-directory (expand-file-name "root" proj)))
             (canon (dorg-dolt-server--canonical-dir root-repo)))
        (unwind-protect
            (progn
              (make-directory root-repo t)
              (let ((default-directory root-repo))
                (call-process "dolt" nil nil nil "init"
                              "--name" "t" "--email" "t@t"))
              ;; Validated behavior: fails closed before publishing anything.
              (should-error (dorg-dolt-server--start root-repo)
                            :type 'dorg-sql-transport-error)
              (should-not (dorg-dolt-server-claim root-repo))
              (should-not (dorg-dolt-server-port root-repo))
              (should-not (gethash canon dorg-dolt-server--procs))
              (should-not (file-exists-p
                           (expand-file-name ".dolt/sql-server.info" root-repo)))
              ;; Non-vacuity: remove the validation -> the old wrong-claim
              ;; behavior reappears (server starts, claim published).
              (cl-letf (((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                         (lambda (&rest _) t)))
                (let ((port (dorg-dolt-server--start root-repo)))
                  (unwind-protect
                      (progn
                        (should (integerp port))
                        (should (dorg-dolt-server-claim root-repo)))
                    (dorg-dolt-server--stop root-repo)))))
          (dorg-test--delete-owned-root proj))))))

;;; ─── Detached Daemon Start (tracked-case) ────────────────────────────────
;;
;; `dorg dolt start` must launch a sql-server that OUTLIVES the starting
;; process and is discoverable cross-process, instead of an Emacs-child
;; server reaped the instant the CLI returns.

(ert-deftest dorg-test-dolt-server-publish-claim-daemon-fields ()
  "`dorg-dolt-server--publish-claim' records OWNER-KIND and an OWNER-PID override.
A detached daemon has no Emacs owner, so its claim must record
`:owner-kind daemon' and an owner-pid equal to the daemon's own pid (so
cross-process classify tracks the daemon, not a dead launcher)."
  (dorg-test--with-dolt-server-runtime
    (let* ((root (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-daemon-claim-" "dolt-server-tests")))
           (dolt (expand-file-name "dolt/" root)))
      (unwind-protect
          (progn
            (make-directory dolt t)
            (dorg-dolt-server--publish-claim dolt 4321 7777 nil nil 7777 'daemon)
            (let ((claim (dorg-dolt-server-claim dolt)))
              (should claim)
              (should (eq (alist-get :owner-kind claim) 'daemon))
              (should (equal (alist-get :owner-pid claim) 7777))
              (should (equal (alist-get :server-pid claim) 7777))
              (should (equal (alist-get :port claim) 4321)))
            ;; Default path stays Emacs-owned.
            (dorg-dolt-server--publish-claim dolt 4322)
            (let ((claim (dorg-dolt-server-claim dolt)))
              (should (equal (alist-get :owner-pid claim) (emacs-pid)))
              (should (memq (alist-get :owner-kind claim) '(emacs nil)))))
        (dorg-test--delete-owned-root root)))))

(ert-deftest dorg-test-dolt-server-detached-launch-argv ()
  "`dorg-dolt-server--detached-launch' detaches the io3e8 argv and returns the pid.
The launcher shell must `nohup' the `dolt sql-server' built from
`dorg-dolt--target-sql-server-args' (so --data-dir is the store and the
parent stays clean), background it, and write its pid to the pidfile —
which the function reads back and returns."
  (let* ((root (file-name-as-directory (dorg-test--make-owned-temp-dir "dorg-detach-argv-" "dolt-server-tests")))
         (dolt-dir (expand-file-name "dolt/" root))
         (pidfile (expand-file-name "srv.pid" root))
         (logfile (expand-file-name "srv.log" root))
         (captured nil))
    (unwind-protect
        (progn
          (make-directory dolt-dir t)
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program _infile _buffer _display &rest args)
                       (should (equal program "sh"))
                       (setq captured args)
                       ;; Emulate the shell writing the backgrounded pid.
                       (with-temp-file pidfile (insert "98765"))
                       0)))
            (let* ((target (dorg-dolt--target dolt-dir))
                   (pid (dorg-dolt-server--detached-launch target 51999 pidfile logfile)))
              (should (equal pid 98765))
              ;; sh -c "<script>"
              (should (equal (car captured) "-c"))
              (let ((script (cadr captured)))
                (should (string-match-p "nohup" script))
                (should (string-match-p "sql-server" script))
                (should (string-match-p "--port" script))
                (should (string-match-p "51999" script))
                ;; io3e8: data-dir is the store, doltcfg pinned inside it.
                (should (string-match-p
                         (regexp-quote (directory-file-name
                                        (plist-get target :server-data-dir)))
                         script))
                (should (string-match-p "--doltcfg-dir" script))
                ;; Backgrounded and pid captured to the pidfile.
                (should (string-match-p "&" script))
                (should (string-match-p (regexp-quote pidfile) script))))))
      (dorg-test--delete-owned-root root))))

;;; ─── Test-owned root reaper ───────────────────────────────────────────

(ert-deftest dorg-test-owned-root-reaper-never-signals-owner-pid ()
  "The test root reaper signals daemon :server-pid, never :owner-pid.
Regression for tracked-case: a daemon claim can carry an Emacs owner-pid.
The cleanup path confirm-reaps through EXACT identity (`--confirm-server-gone'):
it kills only the claim's :server-pid — matched against the live process's
dolt `sql-server' argv — never :owner-pid, or it can kill the test runner."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (killed nil))
    (dorg-test--with-dolt-server-runtime
      (let* ((owned-root (dorg-test--make-owned-temp-dir
                          "dorg-reap-owned-" "owner-pid-regression"))
             (foreign-root (dorg-test--make-owned-temp-dir
                            "dorg-reap-foreign-" "owner-pid-regression"))
             (owned-dir (expand-file-name "dolt/" owned-root))
             (foreign-dir (expand-file-name "dolt/" foreign-root))
             (owned-listener (dorg-test--listen-port))
             (foreign-listener (dorg-test--listen-port))
             (owned-canon (dorg-dolt-server--canonical-dir owned-dir)))
        ;; The foreign root exists to prove cleanup is registration-scoped.
        (dorg-test--unregister-owned-root foreign-root)
        (unwind-protect
            (progn
              (make-directory owned-dir t)
              (make-directory foreign-dir t)
              (dorg-test--write-claim owned-dir
                                      :owner-pid 11111
                                      :server-pid 22222
                                      :port (cdr owned-listener)
                                      :owner-kind 'daemon)
              (dorg-test--write-claim foreign-dir
                                      :owner-pid 33333
                                      :server-pid 44444
                                      :port (cdr foreign-listener)
                                      :owner-kind 'daemon)
              (cl-letf (;; :server-pid 22222 is a live dolt sql-server whose
                        ;; argv exactly matches the claim's port + data-dir,
                        ;; and dies once killed (post-kill confirm).
                        ((symbol-function 'dorg-dolt-server--pid-live-p)
                         (lambda (pid) (and (eql pid 22222)
                                            (not (memql pid killed)) t)))
                        ((symbol-function 'process-attributes)
                         (lambda (pid)
                           (when (eql pid 22222)
                             `((comm . "dolt")
                               (args . ,(format
                                         "dolt sql-server --port %d --data-dir %s"
                                         (cdr owned-listener) owned-canon))))))
                        ((symbol-function 'dorg-dolt-server--kill-pid)
                         (lambda (pid &rest _)
                           (push pid killed)
                           t)))
                (dorg-test--reap-owned-root owned-root)
                (should (equal killed '(22222)))
                (should-not (member 11111 killed))
                (should-not (member 33333 killed))
                (should-not (member 44444 killed))))
          (when (process-live-p (car owned-listener))
            (delete-process (car owned-listener)))
          (when (process-live-p (car foreign-listener))
            (delete-process (car foreign-listener)))
          (dorg-test--delete-owned-root owned-root)
          (when (file-directory-p foreign-root)
            (delete-directory foreign-root t)))))))

(ert-deftest dorg-test-owned-root-reaper-spares-shared-server ()
  "The test root reaper never signals the suite shared server.
Regression for tracked-case ACCEPT reject: a strict-transport test store under
an owned root can carry an on-disk claim whose :server-pid/:port belongs to
the suite shared server (`dorg-test--server-proc'/`-server-port').  The
metadata-scan reaper (`dorg-test--reap-owned-root' →
`dorg-test--reap-owned-store' → `dorg-test--kill-server-pid') must recognise
that pid/port as the shared server (`dorg-test--shared-server-record-p') and
refuse to signal it — that server is suite-owned, not test-root-owned."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (dorg-test--server-proc nil)
        (dorg-test--server-port nil)
        (shared-proc nil)
        (listener nil)
        (killed nil))
    (dorg-test--with-dolt-server-runtime
      (let* ((owned-root (dorg-test--make-owned-temp-dir
                          "dorg-reap-shared-" "shared-server-regression"))
             (owned-dir (expand-file-name "dolt/" owned-root)))
        (unwind-protect
            (progn
              (setq shared-proc
                    (start-process "dorg-test-shared-standin" nil "sleep" "30"))
              (setq listener (dorg-test--listen-port))
              (setq dorg-test--server-proc shared-proc
                    dorg-test--server-port (cdr listener))
              (should (integerp (process-id shared-proc)))
              (make-directory owned-dir t)
              ;; The owned store's on-disk claim points at the SHARED server.
              (dorg-test--write-claim owned-dir
                                      :owner-pid 11111
                                      :server-pid (process-id shared-proc)
                                      :port dorg-test--server-port
                                      :owner-kind 'daemon)
              (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                         (lambda (pid &rest _)
                           (push pid killed)
                           t)))
                (dorg-test--reap-owned-root owned-root)
                (should-not killed)
                (should (process-live-p shared-proc))))
          (when (and listener (process-live-p (car listener)))
            (delete-process (car listener)))
          (when (and shared-proc (process-live-p shared-proc))
            (delete-process shared-proc))
          (dorg-test--delete-owned-root owned-root))))))

(ert-deftest dorg-test-owned-root-reaper-real-detached-transformation ()
  "A real detached daemon under an owned root is reaped before deletion.
Non-vacuous transformation proof: before cleanup a real server-pid is
captured and the port is live; after root cleanup the port is dead.  If
the cleanup call is removed, the live server survives and the final
assertion fails."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p)) (ert-skip "mysql.el unavailable"))
   (t
    (let ((dorg-test--owned-roots (make-hash-table :test 'equal)))
      (dorg-test--with-dolt-server-runtime
        (let ((temp-base (dorg-test--make-owned-temp-dir
                          "dorg-reap-detached-" "real-detached-reap")))
          (unwind-protect
              (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                     (port (dorg-dolt-server--ensure-detached dolt-dir))
                     (claim (dorg-dolt-server-claim dolt-dir))
                     (server-pid (alist-get :server-pid claim)))
                (should (integerp port))
                (should (dorg-dolt-server--port-live-p port))
                (should (integerp server-pid))
                (should (> server-pid 0))
                (should (> (length (dorg-test--owned-store-dirs temp-base)) 0))
                (dorg-test--reap-owned-root temp-base)
                (let ((deadline (+ (float-time) 6)))
                  (while (and (dorg-dolt-server--port-live-p port)
                              (< (float-time) deadline))
                    (accept-process-output nil 0.1)))
                (should-not (dorg-dolt-server--port-live-p port)))
            (dorg-test--delete-owned-root temp-base))))))))

(ert-deftest dorg-test-owned-root-reaper-reaps-dead-daemon-pidfile-residue ()
  "A lone dead-daemon `.dorg/server.pid' residue is reaped, not stranded
\(tracked-case).  A real detached daemon stopped via `dorg-dolt-stop' leaves ONLY
its `.dorg/server.pid' (the claim is deleted and dolt removes sql-server.info),
holding the DEAD daemon's real pid.  `dorg-test--store-server-identities' must
recover that pid as a CONFIRMABLE `(:pid P :port nil)' identity — not the
always-blocking nil-pid — so `dorg-test--confirm-server-gone' proves it dead and
the reap returns `gone'.  Non-vacuous: reverting the residue-recovery branch
emits nil-pid and both reap assertions FAIL with `unknown'."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (proc nil))
    (dorg-test--with-dolt-server-runtime
      (let* ((owned-root (dorg-test--make-owned-temp-dir
                          "dorg-pidres-dead-" "pidfile-residue-reap"))
             (owned-dir (expand-file-name "dolt/" owned-root))
             (canon (dorg-dolt-server--canonical-dir owned-dir))
             (dorg-dir (dorg-dolt-server--ensure-dorg-dir owned-dir))
             (pidfile (expand-file-name "server.pid" dorg-dir)))
        (unwind-protect
            (let (dead-pid)
              (setq proc (start-process "dorg-test-dead" nil "sleep" "30"))
              (setq dead-pid (process-id proc))
              ;; Make the pid genuinely DEAD (a real, confirmed-dead identity).
              (delete-process proc)
              (let ((deadline (+ (float-time) 6)))
                (while (and (dorg-dolt-server--pid-live-p dead-pid)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should (null (dorg-dolt-server--pid-live-p dead-pid)))
              ;; The SOLE marker: server.pid holding the dead pid.  No claim,
              ;; no native sql-server.info.
              (with-temp-file pidfile (insert (number-to-string dead-pid)))
              (should-not (file-exists-p
                           (expand-file-name "server.claim" dorg-dir)))
              (should-not (file-exists-p
                           (expand-file-name ".dolt/sql-server.info" canon)))
              ;; Recovered CONFIRMABLE identity — the real pid, not nil.
              (should (equal (dorg-test--store-server-identities canon)
                             (list (list :pid dead-pid :port nil
                                         :dolt-dir canon))))
              ;; Dead pid confirms `gone' -> the root is reapable.
              (should (eq (dorg-test--reap-owned-store canon) 'gone))
              (should (eq (dorg-test--reap-owned-root owned-root) 'gone)))
          (when (and proc (process-live-p proc)) (delete-process proc))
          (ignore-errors (delete-file pidfile))
          (dorg-test--delete-owned-root owned-root))))))

(ert-deftest dorg-test-owned-root-reaper-blocks-live-pidfile-residue ()
  "A pidfile whose pid is LIVE blocks the reap fail-closed (tracked-case).
The residue-recovery path recovers the live pid as a `(:pid P :port nil)'
identity, but `dorg-test--confirm-server-gone' returns `unknown' (a nil `:port'
fails `dorg-test--registry-identity-match-p', so the pid is NEVER killed) and
the root is retained.  Guards the recovery from ever burying a live server."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (proc nil))
    (dorg-test--with-dolt-server-runtime
      (let* ((owned-root (dorg-test--make-owned-temp-dir
                          "dorg-pidres-live-" "pidfile-residue-live"))
             (owned-dir (expand-file-name "dolt/" owned-root))
             (canon (dorg-dolt-server--canonical-dir owned-dir))
             (dorg-dir (dorg-dolt-server--ensure-dorg-dir owned-dir))
             (pidfile (expand-file-name "server.pid" dorg-dir)))
        (unwind-protect
            (progn
              (setq proc (start-process "dorg-test-live" nil "sleep" "30"))
              (should (process-live-p proc))
              (with-temp-file pidfile
                (insert (number-to-string (process-id proc))))
              ;; Recovered as a real LIVE-pid identity (port nil).
              (should (equal (dorg-test--store-server-identities canon)
                             (list (list :pid (process-id proc) :port nil
                                         :dolt-dir canon))))
              ;; A live pid with no identity match blocks; never killed.
              (should-not (eq (dorg-test--reap-owned-store canon) 'gone))
              (should (process-live-p proc)))
          ;; Remove the marker so cleanup reaps deterministically (the live
          ;; proc is not a registered `--procs' entry, so a marker-free store
          ;; is `gone').
          (ignore-errors (delete-file pidfile))
          (when (and proc (process-live-p proc)) (delete-process proc))
          (dorg-test--delete-owned-root owned-root))))))

(ert-deftest dorg-test-owned-root-reaper-pidfile-residue-yields-to-unreadable-claim ()
  "Residue recovery is DISQUALIFIED when another marker is present (tracked-case).
A readable `server.pid' alongside a present-but-UNREADABLE `server.claim' must
still emit the ukfp6 nil-pid block — a distinct, possibly-live identity the
pidfile cannot certify — so the reap stays fail-closed `unknown'.  Proves the
recovery narrows ONLY the sole-pidfile shape and leaves ukfp6's corrupt/
incomplete-metadata protection intact."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal)))
    (dorg-test--with-dolt-server-runtime
      (let* ((owned-root (dorg-test--make-owned-temp-dir
                          "dorg-pidres-claim-" "pidfile-residue-unreadable"))
             (owned-dir (expand-file-name "dolt/" owned-root))
             (canon (dorg-dolt-server--canonical-dir owned-dir))
             (dorg-dir (dorg-dolt-server--ensure-dorg-dir owned-dir))
             (pidfile (expand-file-name "server.pid" dorg-dir))
             (claimfile (expand-file-name "server.claim" dorg-dir)))
        (unwind-protect
            (progn
              (with-temp-file pidfile (insert "999999"))
              ;; A present but UNREADABLE claim (not a `consp' form).
              (with-temp-file claimfile (insert "not-a-lisp-form {{{"))
              (should (null (dorg-dolt-server--claim-read owned-dir)))
              ;; Disqualified -> the nil-pid block, NOT the recovered pid.
              (should (equal (dorg-test--store-server-identities canon)
                             (list (list :pid nil :port nil :dolt-dir canon))))
              (should-not (eq (dorg-test--reap-owned-store canon) 'gone)))
          (ignore-errors (delete-file pidfile))
          (ignore-errors (delete-file claimfile))
          (dorg-test--delete-owned-root owned-root))))))

(ert-deftest dorg-test-dolt-server-ensure-detached-reuses-healthy ()
  "`dorg-dolt-server--ensure-detached' reuses a healthy server for the right store.
An idempotent `dorg dolt start' must not launch a second daemon when a
live claim already serves the expected store; it returns the existing
port and never calls the start primitive."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "ensure-detached-reuse"))
           (port 64321))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port port
                              :server-pid 999999)
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a second daemon"))))
        (should (= (dorg-dolt-server--ensure-detached dolt-dir) port))))))

(ert-deftest dorg-test-dolt-server-ensure-detached-starts-when-absent ()
  "`dorg-dolt-server--ensure-detached' starts a detached daemon when no claim exists.
The fresh daemon's port is memoized in `dorg-dolt-server--attached-ports'
so the hot query path (`dorg-dolt-server--strict-port') does not re-walk
claim discovery on every subsequent SQL call (tracked-regression)."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "ensure-detached-fresh"))
           (started nil))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (_dir) (setq started t) 55001)))
        (should (= (dorg-dolt-server--ensure-detached dolt-dir) 55001))
        (should started)
        (should (equal (gethash (dorg-dolt-server--canonical-dir dolt-dir)
                                dorg-dolt-server--attached-ports)
                       55001))))))

(ert-deftest dorg-test-dolt-server-implicit-autostart-disabled-errors-no-side-effects ()
  "The implicit hot-path (`dorg-dolt-server--strict-port') signals
`dorg-sql-transport-error' and starts NO real process when
`dorg-dolt-server-autostart' is nil (the default) and no usable server
exists.  Real, unmocked `dolt' coverage: after the error, no claim and no
native `sql-server.info' exist for the store.  The error text must name
the exact remedy (`dorg dolt start PATH') and the exact option
\(`dorg-dolt-server-autostart'), not just the signal type."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    ;; The attach-only selector hard-requires mysql.el and would signal the
    ;; mysql-unavailable error before reaching the no-autostart remedy this
    ;; test asserts, so a mysql-less host cannot exercise it meaningfully.
    (ert-skip "mysql.el unavailable — strict server path needs it"))
   (t
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir
                         "dorg-implicit-autostart-off-" "dolt-server-tests")))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (info-file (expand-file-name ".dolt/sql-server.info" dolt-dir))
                   (dorg-dolt-server-autostart nil)
                   (err (should-error (dorg-dolt-server--strict-port dolt-dir)
                                      :type 'dorg-sql-transport-error)))
              (should (string-match-p
                       (regexp-quote (format "dorg dolt start %s" dolt-dir))
                       (cadr err)))
              (should (string-match-p "dorg-dolt-server-autostart" (cadr err)))
              (should-not (dorg-dolt-server-port dolt-dir))
              (should-not (dorg-dolt-server-claim dolt-dir))
              (should-not (file-exists-p info-file)))
          (dorg-test--delete-owned-root temp-base)))))))

(ert-deftest dorg-test-dolt-server-implicit-autostart-enabled-starts-real-daemon ()
  "The implicit hot-path starts a real detached daemon when
`dorg-dolt-server-autostart' is enabled and no usable server exists.
Mirrors the disabled test above with the option flipped: same no-server
starting state, opposite outcome — a live, claimed, cross-process daemon."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir
                         "dorg-implicit-autostart-on-" "dolt-server-tests")))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (dorg-dolt-server-autostart t)
                   (port (dorg-dolt-server--strict-port dolt-dir)))
              (unwind-protect
                  (progn
                    (should (integerp port))
                    (should (dorg-dolt-server--port-live-p port))
                    ;; Detached: not an Emacs-child of this process.
                    (should-not (dorg-dolt-server-port dolt-dir))
                    (let ((claim (dorg-dolt-server-claim dolt-dir)))
                      (should claim)
                      (should (eq (alist-get :owner-kind claim) 'daemon))))
                (dorg-dolt-stop dolt-dir)))
          (dorg-test--delete-owned-root temp-base))))))

(ert-deftest dorg-test-dolt-server-implicit-autostart-disabled-attaches-live-daemon-claim ()
  "With `dorg-dolt-server-autostart' disabled, the implicit hot-path still
attaches to an ALREADY-RUNNING detached daemon instead of erroring — the
option only gates starting a fresh one.  Explicit `dorg-dolt-start' brings
the daemon up (unaffected by the option); dropping the in-memory attach
memo forces the hot path to re-walk real claim discovery (lock, claim
read, deep classify, store validation) rather than short-circuiting on the
cache, so this exercises `dorg-dolt-server--attach-only''s `daemon'
owner-kind branch against a genuine cross-process claim.  See
`dorg-test-attach-only-disabled-live-non-daemon-claim-attaches' for the
same proof against a live non-daemon owner, and
`dorg-test-dolt-server-implicit-autostart-fresh-subprocess-lifecycle' for
coverage from a genuinely separate Emacs process."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    ;; `dorg-dolt-server--attach-only' hard-requires mysql.el to attach.
    (ert-skip "mysql.el unavailable — strict server path needs it"))
   (t
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir
                         "dorg-implicit-autostart-attach-" "dolt-server-tests")))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (daemon-port (dorg-dolt-server--ensure-detached dolt-dir)))
              (unwind-protect
                  (let ((dorg-dolt-server-autostart nil)
                        (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
                    (should (= (dorg-dolt-server--strict-port dolt-dir) daemon-port)))
                (dorg-dolt-stop dolt-dir)))
          (dorg-test--delete-owned-root temp-base)))))))

(ert-deftest dorg-test-attach-only-disabled-live-non-daemon-claim-attaches ()
  "`dorg-dolt-server--attach-only' attaches to a live, validated,
NON-daemon claim (e.g. a foreign dolt-native or another Emacs session's
server) even when `dorg-dolt-server-autostart' is nil.  This is the case
the rejected first pass mishandled: `dorg-dolt-server--strict-port' used
to route every non-daemon claim straight to
`dorg-dolt-server--ensure-strict', bypassing the autostart gate entirely.
Non-vacuous: classify/compatible/store-valid all pass; only the owner-kind
(`dolt', not `daemon') distinguishes this from the daemon case."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-live-non-daemon"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (port 65444)
           (dorg-dolt-server-autostart nil))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port port
                              :owner-kind 'dolt)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--dolt-uuid-current-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should (= (dorg-dolt-server--attach-only dolt-dir) port))
        (should (= (gethash canon dorg-dolt-server--attached-ports) port))
        (should (= (dorg-dolt-server--strict-port dolt-dir) port))))))

(ert-deftest dorg-test-attach-only-disabled-absent-claim-errors-no-start ()
  "Driven through the public hot path `dorg-dolt-server--strict-port' (which
routes to `dorg-dolt-server--attach-only' when autostart is disabled): NO
claim visible signals `dorg-sql-transport-error' and never starts a daemon
or session server.  Non-vacuous: both start primitives are failing
sentinels."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-absent"))
           (dorg-dolt-server-autostart nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)))))

(ert-deftest dorg-test-attach-only-disabled-dead-claim-errors-no-start ()
  "Driven through `dorg-dolt-server--strict-port' with autostart disabled: a
`dead'-classified non-daemon claim signals `dorg-sql-transport-error' and
never breaks the claim or starts a replacement.  This is attach-only's
single fail-closed arm that
`dorg-dolt-server--ensure-strict' would instead break-and-restart, so this
proves the strict-port gate routes to attach-only (not ensure-strict).
Non-vacuous: `dorg-dolt-server--break-claim' and both start
primitives are failing sentinels."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-dead"))
           (dorg-dolt-server-autostart nil))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65445)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'dead))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (error "must not break the claim")))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)))))

(ert-deftest dorg-test-attach-only-disabled-wedged-claim-errors-no-reap-no-start ()
  "Driven through `dorg-dolt-server--strict-port' with autostart disabled: a
`wedged'-classified non-daemon claim signals `dorg-sql-transport-error'
and never reaps the wedged owner or starts a replacement.  This is exactly
the class of claim the rejected first pass could route into an
unconditional start (`dorg-dolt-server--ensure-strict' reaps-and-replaces
a wedged owner); it must fail closed under autostart-disabled.
Non-vacuous: `dorg-dolt-server--reap-wedged' and both start primitives are
failing sentinels."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-wedged"))
           (dorg-dolt-server-autostart nil))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65447)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'wedged))
                ((symbol-function 'dorg-dolt-server--reap-wedged)
                 (lambda (&rest _) (error "must not reap a wedged claim")))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)))))

(ert-deftest dorg-test-attach-only-disabled-owned-wedged-local-port-errors-no-reap ()
  "`dorg-dolt-server--attach-only' with a LOCALLY-owned but unhealthy
\(wedged) server signals `dorg-sql-transport-error' and never reaps or
restarts it.  `dorg-dolt-server--ensure-strict' reaps this exact case
unconditionally; attach-only must not, since that is a start/restart into
replacement.  Non-vacuous: `dorg-dolt-server--reap-wedged' and
`dorg-dolt-server--start-unsafe' are failing sentinels."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-owned-wedged"))
           (dorg-dolt-server-autostart nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) 65448))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--reap-wedged)
                 (lambda (&rest _) (error "must not reap the locally-owned server")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not restart the locally-owned server"))))
        (should-error (dorg-dolt-server--attach-only dolt-dir)
                      :type 'dorg-sql-transport-error)))))

(ert-deftest dorg-test-attach-only-disabled-live-claim-disappears-under-lock ()
  "Simulates a live claim vanishing between an outer observation and the
locked attach decision.  `dorg-dolt-server--attach-only' reads
`dorg-dolt-server--effective-claim' only once, while holding the claim
lock; this test both proves the lock is actually held at read time and
that a claim absent at that moment fails closed (no start) rather than
trusting a stale earlier observation.  Directly targets the rejected
pass's race: \"a live claim disappearing before the locked ensure\"."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-race"))
           (lock-file (concat (dorg-dolt-server--claim-file dolt-dir) ".lock"))
           (locked-during-read nil)
           (dorg-dolt-server-autostart nil))
      ;; A claim exists on disk (as an outer, pre-lock observer might see)
      ;; but the locked read below simulates it vanishing by the time the
      ;; lock-protected decision actually runs.
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65449)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _)
                   (setq locked-during-read (file-directory-p lock-file))
                   nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)
        (should locked-during-read)))))

(ert-deftest dorg-test-attach-only-disabled-preserves-wrong-store-error ()
  "Through the strict-port gate (`dorg-dolt-server--strict-port' →
`dorg-dolt-server--attach-only', autostart disabled), a live,
protocol-compatible claim serving the WRONG store still fails closed,
matching `dorg-dolt-server--ensure-strict''s existing behavior —
autostart-disabled does not relax store validation."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-wrong-store"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (dorg-dolt-server-autostart nil))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65450)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--claim-compatible-p)
                 (lambda (&rest _) t))
                ;; Served SNAPSHOT holds the expected DB (served-DB gate passes)
                ;; but the store has no DOrg schema marker: the wrong-store
                ;; refusal comes from the SCHEMA gate, not wrong-db.
                ;; Tri-state seam (tracked-regression): lifecycle paths probe via
                ;; `--probe-served-dbs'; wrap the served snapshot as a CONFIRMED
                ;; result so this pre-tri-state stub still drives the same gate.
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (port) (cons 'served (dorg-dolt-server--list-served-dbs port))))
                ((symbol-function 'dorg-dolt-server--list-served-dbs)
                 (lambda (&rest _)
                   (list (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))))
                ((symbol-function 'dorg-dolt-server--list-tables)
                 (lambda (&rest _) '("information_schema")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (&rest _) nil)))
        (let ((err (should-error (dorg-dolt-server--strict-port dolt-dir)
                                 :type 'dorg-sql-transport-error)))
          ;; Refused at the schema gate, matching ensure-strict's behavior.
          (should (string-match-p "missing the DOrg schema marker" (cadr err))))
        (should-not (gethash canon dorg-dolt-server--attached-ports))))))

(ert-deftest dorg-test-attach-only-disabled-preserves-unreachable-error ()
  "Through the strict-port gate (`dorg-dolt-server--strict-port' →
`dorg-dolt-server--attach-only', autostart disabled), an `unreachable'
live owner still fails closed, matching `dorg-dolt-server--ensure-strict''s
existing behavior — autostart-disabled does not turn an unreachable-owner
conflict into a fresh start."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "attach-only-unreachable"))
           (dorg-dolt-server-autostart nil))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port 65451)
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'unreachable))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a daemon")))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (error "must not start a session server"))))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)))))

;;; ─── Fresh-Subprocess Implicit-Autostart Lifecycle ──────────────────────

(defun dorg-test--dolt-server-fresh-emacs-eval (root expr)
  "Evaluate EXPR in a genuinely separate, freshly-started `--batch -Q' Emacs.
ROOT and its bundled `lib/mysql' are added to `load-path' so `require'
can find both the dorg sources and the hard mysql.el dependency (without
the mysql entry, loading `dorg-dolt' can fail the hard mysql.el load in a
bare `-Q' Emacs); no other state (including
`dorg-dolt-server--attached-ports' or any other in-process cache of this
test's Emacs) is visible to EXPR — this is real cross-process coverage,
not a same-process cache-clear simulation.  Returns (EXIT-CODE . OUTPUT-STRING)."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (mysql (expand-file-name "lib/mysql" root))
         (args (list "--batch" "-Q" "-L" root "-L" mysql
                     "--eval" (prin1-to-string expr)))
         (output-buf (generate-new-buffer " *dorg-fresh-emacs-probe*"))
         exit-code)
    (unwind-protect
        (progn
          (setq exit-code (apply #'call-process emacs nil output-buf nil args))
          (cons exit-code (with-current-buffer output-buf (buffer-string))))
      (kill-buffer output-buf))))

(ert-deftest dorg-test-dolt-server-implicit-autostart-fresh-subprocess-lifecycle ()
  "End-to-end implicit-autostart lifecycle, each stage a genuinely separate
`--batch -Q' Emacs subprocess (never the same process, never a cleared
cache): (1) disabled default against a no-server repo signals the exact
`dorg-sql-transport-error' remedy and leaves no process, claim, or native
`sql-server.info'; (2) explicit `dorg-dolt-start' creates a durable
daemon that outlives its starting subprocess; (3) a SEPARATE disabled
client subprocess executes a real SELECT through that daemon successfully
without stopping it; (4) explicit `dorg-dolt-stop' removes it, again from
a separate subprocess."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — strict server path needs it")))
  (let ((root (file-name-as-directory
               (or (locate-dominating-file default-directory "dorg-dolt.el")
                   default-directory)))
        (temp-base (dorg-test--make-owned-temp-dir
                    "dorg-fresh-subprocess-" "dolt-server-tests")))
    (unwind-protect
        (let* ((dolt-dir (dorg-test--copy-repo temp-base))
               (info-file (expand-file-name ".dolt/sql-server.info" dolt-dir)))
          ;; Stage 1: disabled default, no server — errors, no side effects.
          (let* ((expr `(progn
                          (require 'dorg-dolt-server)
                          (condition-case err
                              (progn (dorg-dolt-server--strict-port ,dolt-dir)
                                     (message "UNEXPECTED-SUCCESS")
                                     (kill-emacs 1))
                            (dorg-sql-transport-error
                             (princ (cadr err))
                             (kill-emacs 0))
                            (error
                             (message "WRONG-ERROR-TYPE: %S" err)
                             (kill-emacs 2)))))
                 (result (dorg-test--dolt-server-fresh-emacs-eval root expr)))
            (should (= (car result) 0))
            (should (string-match-p
                     (regexp-quote (format "dorg dolt start %s" dolt-dir))
                     (cdr result)))
            (should (string-match-p "dorg-dolt-server-autostart" (cdr result)))
            (should-not (dorg-dolt-server-claim dolt-dir))
            (should-not (file-exists-p info-file)))
          (unwind-protect
              (progn
                ;; Stage 2: explicit start creates a durable daemon that
                ;; outlives this subprocess.
                (let* ((expr `(progn
                                (require 'dorg-dolt-server)
                                (let ((port (dorg-dolt-start ,dolt-dir)))
                                  (if (integerp port)
                                      (kill-emacs 0)
                                    (progn (message "NO-PORT: %S" port)
                                           (kill-emacs 1))))))
                       (result (dorg-test--dolt-server-fresh-emacs-eval root expr)))
                  (should (= (car result) 0)))
                (let ((claim (dorg-dolt-server-claim dolt-dir)))
                  (should claim)
                  (should (eq (alist-get :owner-kind claim) 'daemon)))
                (should (file-exists-p info-file))
                ;; Stage 3: a separate disabled client attaches and SELECTs
                ;; without killing the daemon.
                (let* ((expr `(progn
                                (require 'dorg-dolt)
                                (require 'dorg-dolt-server)
                                (let ((rows (dorg-sql ,dolt-dir "SELECT 1 AS n")))
                                  (if (equal rows '(((n . 1))))
                                      (kill-emacs 0)
                                    (progn (message "UNEXPECTED-ROWS: %S" rows)
                                           (kill-emacs 1))))))
                       (result (dorg-test--dolt-server-fresh-emacs-eval root expr)))
                  (should (= (car result) 0)))
                (let ((claim (dorg-dolt-server-claim dolt-dir)))
                  (should claim)
                  (should (eq (alist-get :owner-kind claim) 'daemon)))
                (should (file-exists-p info-file)))
            ;; Stage 4: explicit stop removes it, from a separate subprocess.
            (let* ((expr `(progn
                            (require 'dorg-dolt-server)
                            (dorg-dolt-stop ,dolt-dir)
                            (kill-emacs 0)))
                   (result (dorg-test--dolt-server-fresh-emacs-eval root expr)))
              (should (= (car result) 0)))
            (should-not (dorg-dolt-server-claim dolt-dir))
            (should-not (file-exists-p info-file))))
      (dorg-test--delete-owned-root temp-base))))

(ert-deftest dorg-test-dolt-stop-reaps-daemon-despite-attach-memo ()
  "`dorg-dolt-stop' reaps a host-local daemon even when this process attached.
Since `dorg-dolt-server--ensure-detached' memoizes the daemon's port in
`dorg-dolt-server--attached-ports' (the tracked-case hot-path fix), the
attach-only \"detach and leave the owner's claim\" branch would shadow the
daemon-reap branch: `dorg dolt stop' would merely forget the attach and
leave the daemon running, breaking the manual v0.1 lifecycle (stop, then
`dorg-dolt-server--start' refuses on the live claim).  The daemon arm must
take precedence over attach-only, kill the recorded pid, and drop the
attach memo so a later resolve cannot serve the dead port."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "stop-daemon-attached"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (killed nil))
      (unwind-protect
          (progn
            ;; A VERIFIED ready daemon (tracked-regression): the overlay claim and
            ;; dolt-native `sql-server.info' agree on the full identity tuple.
            (dorg-test-srvid--write-daemon-claim dolt-dir 4243 port "u1")
            (dorg-test-srvid--write-native-info dolt-dir 4243 port "u1")
            ;; Simulate the post-ensure-detached state: this process attached.
            (puthash canon port dorg-dolt-server--attached-ports)
            (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'dorg-dolt-server--kill-pid)
                       (lambda (pid &rest _) (setq killed pid) t)))
              (should (dorg-dolt-stop dolt-dir))
              ;; The daemon was reaped, not just detached from.
              (should (equal killed 4243))
              ;; Claim gone and attach memo dropped: nothing can resurrect the port.
              (should-not (dorg-dolt-server-claim dolt-dir))
              (should-not (gethash canon dorg-dolt-server--attached-ports))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dolt-server-strict-port-memoizes-detached-attach ()
  "`dorg-dolt-server--strict-port' resolves a daemon claim ONCE, then short-circuits.
The strict-port docstring promises the hot query path reuses \"a
previously attached foreign port without re-probing\", but the detached
daemon reuse path never recorded its port in
`dorg-dolt-server--attached-ports' — so EVERY `dorg-sql' call re-took the
claim lock, re-read both claim files, and re-probed/validated the store
\(~60ms of ceremony per sub-millisecond query).  On a live-size tracker
that multiplied each tracker-cli command into 30s+ (tracked-regression).

After the first resolution attaches to a live daemon claim, the port
must be memoized and a second `dorg-dolt-server--strict-port' call must
perform NO further claim discovery."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "strict-port-memo"))
           (port 64322)
           (claim-reads 0)
           (real-claim (symbol-function 'dorg-dolt-server--effective-claim)))
      (dorg-test--write-claim dolt-dir :owner-pid 999999 :port port
                              :server-pid 999999 :owner-kind 'daemon)
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--claim-classify)
                 (lambda (&rest _) 'live))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (error "must not start a second daemon")))
                ((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (dir) (cl-incf claim-reads) (funcall real-claim dir))))
        ;; First call walks discovery (>=1 claim read) and memoizes the port.
        (should (= (dorg-dolt-server--strict-port dolt-dir) port))
        (should (equal (gethash (dorg-dolt-server--canonical-dir dolt-dir)
                                dorg-dolt-server--attached-ports)
                       port))
        (let ((first-reads claim-reads))
          (should (>= first-reads 1))
          ;; Second call must serve the memo: same port, ZERO new claim reads.
          (should (= (dorg-dolt-server--strict-port dolt-dir) port))
          (should (= claim-reads first-reads)))))))

(ert-deftest dorg-test-dolt-server-stop-daemon-cross-process ()
  "`dorg-dolt-stop' stops a same-host daemon by its server-pid + drops the claim.
A `:owner-kind daemon' claim has no live Emacs owner, so a separate
process's `dorg dolt stop' must reap the daemon (kill `:server-pid')
and delete the claim — NOT refuse it as a foreign Emacs-owned server.
An Emacs-owned live foreign claim is still refused (regression guard)."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "stop-daemon"))
           (killed nil))
      ;; Daemon claim: same host, live port, VERIFIED tuple across the overlay
      ;; claim and dolt-native `sql-server.info' (tracked-regression).
      (let ((lp (dorg-test--listen-port)))
        (unwind-protect
            (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'dorg-dolt-server--kill-pid)
                       (lambda (pid &rest _) (setq killed pid) t)))
              (dorg-test-srvid--write-daemon-claim dolt-dir 4242 (cdr lp) "u1")
              (dorg-test-srvid--write-native-info dolt-dir 4242 (cdr lp) "u1")
              (should (dorg-dolt-stop dolt-dir))
              (should (equal killed 4242))
              ;; Claim file removed so a later classify cannot resurrect it.
              (should-not (dorg-dolt-server-claim dolt-dir)))
          (delete-process (car lp))))
      ;; Regression: an Emacs-owned live foreign server is still refused.
      (let ((lp (dorg-test--listen-port)))
        (unwind-protect
            (progn
              (dorg-test--write-claim dolt-dir
                                      :owner-pid 999999 :server-pid 999999
                                      :port (cdr lp) :owner-kind 'emacs)
              (should-error (dorg-dolt-stop dolt-dir) :type 'user-error))
          (delete-process (car lp)))))))

(ert-deftest dorg-test-dolt-server-stop-daemon-port-down-pid-alive-refuses ()
  "`dorg-dolt-stop' refuses a daemon whose port is down but pid is alive.
A port-down daemon claim whose recorded pid is still live may be a server
mid-boot, or — worse — a pid the OS has reused for an unrelated process.
Signalling it would kill a bystander, so the stop branch must classify
`unreachable', refuse with `user-error', NEVER call `dorg-dolt-server--kill-pid',
and leave the claim intact for a later (port-up) reap."
  (dorg-test--with-dolt-server-runtime
    (let ((dolt-dir (dorg-test--repo "stop-daemon-down-alive"))
          (killed nil))
      ;; A port that is provably NOT listening: open then immediately close.
      (let* ((lp (dorg-test--listen-port))
             (dead-port (cdr lp)))
        (delete-process (car lp))
        (unwind-protect
            (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                       (lambda (pid &rest _) (setq killed pid) t)))
              ;; owner-pid = this live Emacs => classify `unreachable' (port down).
              (dorg-test--write-claim dolt-dir
                                      :owner-pid (emacs-pid) :server-pid (emacs-pid)
                                      :port dead-port :owner-kind 'daemon)
              (should-error (dorg-dolt-stop dolt-dir) :type 'user-error)
              ;; The possibly-reused pid was NEVER signalled.
              (should-not killed)
              ;; The claim is left intact, not orphaned.
              (should (dorg-dolt-server-claim dolt-dir)))
          ;; The claim this test intentionally leaves names THIS live Emacs pid
          ;; on a dead port — a synthetic identity the owned-root teardown reaper
          ;; can never `confirm-server-gone' (fail-closed: it refuses to kill our
          ;; own process), so it would strand this store's `dorg-test-store-'
          ;; root at the crash-net (tracked-case).  Delete the synthetic claim now
          ;; that the assertions have observed it, so the root reaps cleanly.
          (dorg-dolt-server--claim-delete-file dolt-dir))))))

(ert-deftest dorg-test-dolt-server-stop-daemon-dead-clears-without-signal ()
  "`dorg-dolt-stop' clears a dead daemon claim WITHOUT signalling its pid.
When the daemon's pid is enumerably gone and its port is down (classify
`dead'), the claim is stale: the recorded pid may now belong to an
unrelated process.  The stop branch must drop the claim and report success
WITHOUT calling `dorg-dolt-server--kill-pid' on the absent/reused pid."
  (dorg-test--with-dolt-server-runtime
    (let ((dolt-dir (dorg-test--repo "stop-daemon-dead"))
          (killed nil))
      (let* ((lp (dorg-test--listen-port))
             (dead-port (cdr lp)))
        (delete-process (car lp))
        (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                   (lambda (pid &rest _) (setq killed pid) t)))
          ;; owner-pid 999999 is absent => classify `dead' (pid gone, port down).
          (dorg-test--write-claim dolt-dir
                                  :owner-pid 999999 :server-pid 999999
                                  :port dead-port :owner-kind 'daemon)
          (should (dorg-dolt-stop dolt-dir))
          ;; No pid was signalled.
          (should-not killed)
          ;; Stale claim cleared.
          (should-not (dorg-dolt-server-claim dolt-dir)))))))

(ert-deftest dorg-test-dolt-server-stop-daemon-kill-unconfirmed-keeps-claim ()
  "`dorg-dolt-stop' keeps the claim when a live daemon's kill is unconfirmed.
For a `live' daemon claim the pid IS signalled, but if `dorg-dolt-server--kill-pid'
cannot confirm the process died (returns nil) the claim must NOT be deleted —
deleting it would orphan a still-running server from its discovery claim.
The call returns nil (stop not confirmed) and the claim survives."
  (dorg-test--with-dolt-server-runtime
    (let ((dolt-dir (dorg-test--repo "stop-daemon-killfail"))
          (killed nil))
      (let ((lp (dorg-test--listen-port)))
        (unwind-protect
            (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'dorg-dolt-server--kill-pid)
                       (lambda (pid &rest _) (setq killed pid) nil)))
              ;; VERIFIED ready daemon (tracked-regression): tuple agrees across the
              ;; overlay claim and dolt-native `sql-server.info'.
              (dorg-test-srvid--write-daemon-claim dolt-dir 4242 (cdr lp) "u1")
              (dorg-test-srvid--write-native-info dolt-dir 4242 (cdr lp) "u1")
              ;; Live, verified port => pid is signalled, but kill is unconfirmed.
              (should-not (dorg-dolt-stop dolt-dir))
              (should (equal killed 4242))
              ;; Claim survives so the live server stays discoverable.
              (should (dorg-dolt-server-claim dolt-dir)))
          (delete-process (car lp)))))))

(ert-deftest dorg-test-dolt-server-detached-persists-cross-process ()
  "A detached `dorg-dolt-server--ensure-detached' daemon outlives its launcher.
Drives the REAL detached path against an isolated `dorg-test--copy-repo'
store: after the call returns the server is still LISTENING, is NOT an
Emacs-child of this process (`dorg-dolt-server-port' nil), carries a durable
`:owner-kind daemon' claim that classifies `live', is visible to a
fresh-process client view (empty in-memory tables, store registered) via
the store-relative overlay reconciled with dolt-native info, leaves the
parent dir clean (io3e8), and is reapable via `dorg-dolt-stop'.
Non-vacuous: an Emacs-child start would register a `dorg-dolt-server-port' and
publish no daemon claim, failing these assertions."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-dolt-server-runtime
      (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-detach-live-" "dolt-server-tests")))
        (unwind-protect
            (let* ((dolt-dir (dorg-test--copy-repo temp-base))
                   (parent (file-name-as-directory (expand-file-name temp-base)))
                   (port (dorg-dolt-server--ensure-detached dolt-dir)))
              (unwind-protect
                  (progn
                    (should (integerp port))
                    ;; Detached, not an Emacs-child of this process.
                    (should-not (dorg-dolt-server-port dolt-dir))
                    ;; Really listening.
                    (should (dorg-dolt-server--port-live-p port))
                    ;; Durable daemon claim, classifies live.
                    (let ((claim (dorg-dolt-server-claim dolt-dir)))
                      (should claim)
                      (should (eq (alist-get :owner-kind claim) 'daemon))
                      (should (eq (dorg-dolt-server--claim-classify claim) 'live)))
                    ;; Fresh-process view (empty in-memory tables) that is a
                    ;; CLIENT of the store — i.e. has the document registered —
                    ;; discovers the daemon via the store-relative overlay +
                    ;; dolt-native info, reconciled to ONE live entry.
                    (let ((dorg-dolt-server--procs (make-hash-table :test 'equal))
                          (dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
                          (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
                      (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                                 (lambda (&optional _buf) dolt-dir)))
                        (let* ((listed (dorg-dolt-server-list))
                               (matches (cl-remove-if-not
                                         (lambda (e)
                                           (equal (plist-get e :dolt-dir)
                                                  (dorg-dolt-server--canonical-dir dolt-dir)))
                                         listed))
                               (entry (car matches)))
                          (should entry)
                          ;; Overlay + native info reconcile to ONE entry.
                          (should (= 1 (length matches)))
                          (should (eq (plist-get entry :status) 'live)))))
                    ;; io3e8: parent stays clean of dolt server artifacts.
                    (should-not
                     (seq-filter #'file-exists-p
                                 (list (expand-file-name ".dolt" parent)
                                       (expand-file-name ".doltcfg" parent)
                                       (expand-file-name "config.yaml" parent))))
                    ;; AC5: a stop reaps the daemon and drops the claim.
                    (dorg-dolt-stop dolt-dir)
                    (let ((deadline (+ (float-time) 6)))
                      (while (and (dorg-dolt-server--port-live-p port)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.1)))
                    (should-not (dorg-dolt-server--port-live-p port))
                    (should-not (dorg-dolt-server-claim dolt-dir)))
                ;; Safety net: reap the daemon even if an assertion threw.
                (let ((claim (dorg-dolt-server-claim dolt-dir)))
                  (when (and claim (integerp (alist-get :server-pid claim)))
                    (ignore-errors
                      (dorg-dolt-server--kill-pid (alist-get :server-pid claim) 2))))))
          (dorg-test--delete-owned-root temp-base))))))

;;; ─── Project-scoped (store-relative) discovery — tracked-case ─────────────
;;
;; The ownership overlay is store-relative (`<store>/.dorg/'), anchored to
;; dolt's native `<store>/.dolt/sql-server.info', so every client of a store
;; discovers the one server per store regardless of TMPDIR/environment.

(ert-deftest dorg-test-dolt-server-dorg-overlay-path-stable ()
  "The DOrg overlay claim path is store-relative and differs per store.
Lives under `<store>/.dorg/' (not a per-TMPDIR base)."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "stable-store"))
           (canon (dorg-dolt-server--canonical-dir store))
           (expected (expand-file-name ".dorg/server.claim" canon)))
      (should (equal (dorg-dolt-server--claim-file store) expected))
      (should (string-prefix-p canon (dorg-dolt-server--claim-file store)))
      (should-not (equal (dorg-dolt-server--claim-file store)
                         (dorg-dolt-server--claim-file (dorg-test--repo "other-store")))))))

(ert-deftest dorg-test-dolt-server-dolt-info-parse ()
  "`dorg-dolt-server--dolt-server-info' parses dolt's `PID:PORT:UUID' record.
Tolerates a trailing newline; a malformed record yields nil."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "info-store"))
           (canon (dorg-dolt-server--canonical-dir store))
           (dolt (expand-file-name ".dolt" canon))
           (info-file (expand-file-name "sql-server.info" dolt)))
      (make-directory dolt t)
      (with-temp-file info-file
        (insert "12345:47821:eaf0b36b-4003-4edd-b4eb-f1504002179e"))
      (let ((info (dorg-dolt-server--dolt-server-info store)))
        (should (= (plist-get info :pid) 12345))
        (should (= (plist-get info :port) 47821))
        (should (equal (plist-get info :uuid)
                       "eaf0b36b-4003-4edd-b4eb-f1504002179e")))
      ;; Trailing newline tolerated.
      (with-temp-file info-file (insert "9:1024:u\n"))
      (should (= (plist-get (dorg-dolt-server--dolt-server-info store) :port) 1024))
      ;; Malformed -> nil.
      (with-temp-file info-file (insert "garbage"))
      (should-not (dorg-dolt-server--dolt-server-info store))
      ;; Absent -> nil.
      (delete-file info-file)
      (should-not (dorg-dolt-server--dolt-server-info store)))))

(ert-deftest dorg-test-dolt-server-discovers-via-dolt-info ()
  "A server seen only via dolt-native info is foreign: attach-or-abandon.
With a live port recorded in `sql-server.info' and NO DOrg overlay, the
effective claim is a foreign `owner-kind' `dolt' claim that classifies
`live'; `dorg-dolt-stop' refuses it and NEVER signals its pid (AC7)."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "native-store"))
           (canon (dorg-dolt-server--canonical-dir store))
           (dolt (expand-file-name ".dolt" canon))
           (lp (dorg-test--listen-port))
           (killed nil))
      (make-directory dolt t)
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "sql-server.info" dolt)
              (insert (format "4242:%d:uuid-xyz" (cdr lp))))
            (should-not (dorg-dolt-server-claim store))   ; no overlay
            (let ((eff (dorg-dolt-server--effective-claim store)))
              (should eff)
              (should (eq (alist-get :owner-kind eff) 'dolt))
              (should (= (alist-get :port eff) (cdr lp)))
              (should (equal (alist-get :uuid eff) "uuid-xyz"))
              (should (eq (dorg-dolt-server--claim-classify eff) 'live)))
            (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                       (lambda (pid &rest _) (setq killed pid) t)))
              (should-error (dorg-dolt-stop store) :type 'user-error)
              (should-not killed)))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dolt-server-reap-spares-dolt-native ()
  "`dorg-dolt-server--reap-wedged' never signals a dolt-native info-file pid.
Non-vacuous: this fixture writes a native info file and intercepts pid killing;
the reaper must consult only retained `dorg-dolt-server--procs' ownership."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "reap-native"))
           (dolt (expand-file-name ".dolt" (dorg-dolt-server--canonical-dir store)))
           (lp (dorg-test--listen-port))
           (killed nil))
      (make-directory dolt t)
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (setq killed pid) t)))
            (with-temp-file (expand-file-name "sql-server.info" dolt)
              (insert (format "4242:%d:u" (cdr lp))))
            (dorg-dolt-server--reap-wedged store)
            (should-not killed))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dolt-server-claim-cross-tmpdir ()
  "A store-relative overlay is read through the canonical store path."
  (dorg-test--with-dolt-server-runtime
    (let ((store (dorg-test--repo "xtmp-store")))
      (dorg-test--write-claim store :owner-pid 7 :port 4321 :owner-kind 'daemon)
      (let ((claim (dorg-dolt-server-claim store)))
        (should claim)
        (should (= (alist-get :port claim) 4321))))))

(ert-deftest dorg-test-dolt-server-overlay-gitignored ()
  "Creating the overlay dir self-ignores it via a `*' .gitignore.
Keeps runtime state out of git even without a project ignore rule."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "ignored-store"))
           (dir (dorg-dolt-server--ensure-dorg-dir store))
           (gi (expand-file-name ".gitignore" dir)))
      (should (file-directory-p dir))
      (should (file-exists-p gi))
      (should (equal (string-trim
                      (with-temp-buffer (insert-file-contents gi) (buffer-string)))
                     "*")))))

(ert-deftest dorg-test-dolt-server-io3e8-parent-clean ()
  "The overlay lives inside the store; the store's parent stays clean (io3e8)."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "io3e8-store"))
           (canon (dorg-dolt-server--canonical-dir store))
           (parent (file-name-directory (directory-file-name canon))))
      (dorg-test--write-claim store :owner-pid 7 :port 4321)
      (should (file-exists-p (expand-file-name ".dorg/server.claim" canon)))
      (should-not (file-exists-p (expand-file-name ".dorg" parent)))
      (should-not (file-exists-p (expand-file-name ".dolt" parent)))
      (should-not (file-exists-p (expand-file-name "server.claim" parent))))))

(ert-deftest dorg-test-dolt-server-list-via-registry ()
  "`dorg-dolt-server-list' surfaces a registered store's server with NO central dir.
The overlay is store-relative, so enumeration draws candidate stores from
the dorg registry."
  (dorg-test--with-dolt-server-runtime
    (let ((lp (dorg-test--listen-port)))
      (unwind-protect
          (let ((canon (dorg-test--write-claim (dorg-test--repo "reg-store")
                                               :owner-pid 999999 :port (cdr lp))))
            (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                       (lambda (&optional _buf) canon)))
              (let ((e (car (dorg-dolt-server-list))))
                (should e)
                (should (equal (plist-get e :dolt-dir) canon))
                (should (memq (plist-get e :status) '(live incompatible))))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dolt-server-stale-overlay-masked-by-live-native ()
  "A STALE overlay must not mask a LIVE dolt-native server.
Overlay records a dead daemon on a now-down port; `sql-server.info' points
at a live port.  Reconciliation must resolve the effective claim to the
LIVE native server, not the stale (dead) overlay — else discovery would
treat the store as dead and start a second server over dolt's one-per-db
lock.  Non-vacuous: with overlay-first precedence the effective claim is
the dead daemon overlay and these assertions fail."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "stale-overlay"))
           (canon (dorg-dolt-server--canonical-dir store))
           (dolt (expand-file-name ".dolt" canon))
           (lp (dorg-test--listen-port)))
      (make-directory dolt t)
      (unwind-protect
          (progn
            ;; Stale overlay: a dead daemon on an unbindable (down) port.
            (dorg-test--write-claim store :owner-pid 999999 :server-pid 999999
                                    :port 1 :owner-kind 'daemon)
            ;; Live native server on the real listener port.
            (with-temp-file (expand-file-name "sql-server.info" dolt)
              (insert (format "4242:%d:live-uuid" (cdr lp))))
            ;; The overlay alone classifies dead (its port is down, pid gone).
            (should (eq (dorg-dolt-server--claim-classify (dorg-dolt-server-claim store))
                        'dead))
            ;; Reconciled effective claim = the LIVE native server.
            (let ((eff (dorg-dolt-server--effective-claim store)))
              (should (eq (alist-get :owner-kind eff) 'dolt))
              (should (= (alist-get :port eff) (cdr lp)))
              (should (equal (alist-get :uuid eff) "live-uuid"))
              (should (eq (dorg-dolt-server--claim-classify eff) 'live))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dolt-server-overlay-matching-native-wins ()
  "When the overlay and native record agree on the port, the overlay wins.
The DOrg-managed server keeps its ownership (owner-kind/token), not the
synthesized foreign `dolt' claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "agree-store"))
           (canon (dorg-dolt-server--canonical-dir store))
           (dolt (expand-file-name ".dolt" canon))
           (lp (dorg-test--listen-port)))
      (make-directory dolt t)
      (unwind-protect
          (progn
            (dorg-test--write-claim store :owner-pid 4242 :server-pid 4242
                                    :port (cdr lp) :owner-kind 'daemon)
            (with-temp-file (expand-file-name "sql-server.info" dolt)
              (insert (format "4242:%d:u" (cdr lp))))
            (let ((eff (dorg-dolt-server--effective-claim store)))
              (should (eq (alist-get :owner-kind eff) 'daemon))
              (should (= (alist-get :port eff) (cdr lp)))))
        (delete-process (car lp))))))

;;; ─── Compaction (dorg-dolt-compact) — tracked-case ─────────────────────────
;;
;; History-preserving `dolt gc` via CALL DOLT_GC().  The load-bearing
;; guarantee (tracked-case): gc removes only UNREACHABLE chunks, so the as-of/
;; undo window survives — an early commit's value is still readable AS OF
;; that commit after compaction, and every commit remains in dolt_log.

(defun dorg-test-dolt--commit-count (dolt-dir)
  "Return DOLT-DIR's commit count via the dolt_log system table."
  (let ((r (dorg-sql dolt-dir "SELECT COUNT(*) AS cnt FROM dolt_log")))
    (alist-get 'cnt (car r))))

(ert-deftest dorg-test-dolt-compact-preserves-asof-undo-window ()
  "`dorg-dolt-compact' gc's the store but PRESERVES the as-of/undo window.
Non-vacuous: an early commit's value is read AS OF that commit and must
STILL be readable — and identical — after compaction, with every commit
retained in dolt_log.  This is the tracked-case guarantee that gc never prunes
restorable history."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base))) ; standalone: no shared server
            (dorg-sql--with-cli-seam dolt-dir
              (dorg-sql dolt-dir "INSERT INTO headings (id,root_id,parent_id,sort_key,level,title) VALUES (1,1,NULL,X'01',0,'v1')")
              (dorg-commit dolt-dir "v1")
              (let ((early (dorg-head-commit dolt-dir)))
                (dotimes (i 5)
                  (dorg-sql dolt-dir (format "UPDATE headings SET title='v%d' WHERE id=1" (+ 2 i)))
                  (dorg-commit dolt-dir (format "v%d" (+ 2 i))))
                (let ((commits-before (dorg-test-dolt--commit-count dolt-dir)))
                  ;; Baseline: early as-of read returns 'v1' BEFORE gc.
                  (should (equal "v1" (alist-get 'value (car (dorg-sql dolt-dir "SELECT title AS value FROM headings AS OF '%s' WHERE id=1" early)))))
                  (let ((rep (dorg-dolt-compact dolt-dir)))
                    (should (integerp (plist-get rep :before)))
                    (should (integerp (plist-get rep :after))))
                  ;; UNDO WINDOW PRESERVED: same early as-of read still works post-gc.
                  (should (equal "v1" (alist-get 'value (car (dorg-sql dolt-dir "SELECT title AS value FROM headings AS OF '%s' WHERE id=1" early)))))
                  ;; Every commit retained.
                  (should (= commits-before (dorg-test-dolt--commit-count dolt-dir)))))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-compact-reports-size ()
  "`dorg-dolt-compact' returns before/after/reclaimed noms byte counts.
Honesty caveat: the command must REPORT size, never oversell
reclaim; the plist is the observable contract."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests")))
      (unwind-protect
          (let ((dolt-dir (dorg-test--copy-repo temp-base)))
            (dorg-sql--with-cli-seam dolt-dir
              (dorg-sql dolt-dir "INSERT INTO headings (id,root_id,parent_id,sort_key,level,title) VALUES (1,1,NULL,X'01',0,'x')")
              (dorg-commit dolt-dir "x")
              (let ((rep (dorg-dolt-compact dolt-dir)))
                (should (integerp (plist-get rep :before)))
                (should (integerp (plist-get rep :after)))
                (should (integerp (plist-get rep :reclaimed)))
                (should (= (plist-get rep :reclaimed)
                           (- (plist-get rep :before) (plist-get rep :after)))))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-dolt-dir-bytes ()
  "`dorg-dolt-server--dir-bytes' sums file sizes recursively."
  (let ((d (dorg-test--make-owned-temp-dir "dorg-bytes-" "dolt-server-tests")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a" d) (insert (make-string 100 ?x)))
          (make-directory (expand-file-name "sub" d))
          (with-temp-file (expand-file-name "sub/b" d) (insert (make-string 50 ?y)))
          (should (= 150 (dorg-dolt-server--dir-bytes d))))
      (dorg-test--delete-owned-root d))))

(ert-deftest dorg-test-dolt-compact-check-threshold-warns ()
  "`dorg-dolt-compact-check' warns only when noms size exceeds the threshold.
Non-vacuous: fires over, silent under (same mocked size, different bound)."
  (let ((warned nil))
    (cl-letf (((symbol-function 'dorg-dolt-server--noms-size) (lambda (&rest _) 1000))
              ((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (let ((dorg-dolt-compact-warn-threshold 500))
        (should (dorg-dolt-compact-check "/tmp/dorg-fake/"))
        (should warned))
      (setq warned nil)
      (let ((dorg-dolt-compact-warn-threshold 5000))
        (should-not (dorg-dolt-compact-check "/tmp/dorg-fake/"))
        (should-not warned)))))

(ert-deftest dorg-test-dolt-compact-issues-gc-call ()
  "`dorg-dolt-compact' actually issues CALL DOLT_GC(); prefix routes --shallow.
Non-vacuous for GC EXECUTION: a no-op that only returned before/after byte
counts would still pass the as-of/commit-count checks, so this spies the SQL
the command sends and pins the exact gc call (and the shallow variant)."
  (let (queries)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dir q &rest _) (push q queries) nil))
              ((symbol-function 'dorg-dolt-server--noms-size) (lambda (&rest _) 100)))
      (dorg-dolt-compact "/tmp/dorg-fake/")
      (should (member "CALL DOLT_GC()" queries))
      (setq queries nil)
      (dorg-dolt-compact "/tmp/dorg-fake/" t)
      (should (member "CALL DOLT_GC('--shallow')" queries))
      (should-not (member "CALL DOLT_GC()" queries)))))

(ert-deftest dorg-test-dolt-compact-check-wired-into-list ()
  "`dorg-dolt-list' runs the size check per store — a real production caller.
Non-vacuous: warns once over threshold, is silent under (same mocked size),
and never issues gc (the list path is read-only diagnostics)."
  (let ((warned 0) (gc-called nil))
    (cl-letf (((symbol-function 'dorg-dolt-server-list)
               (lambda () (list (list :dolt-dir "/tmp/dorg-fake-a/"))))
              ((symbol-function 'dorg-dolt-server--noms-size) (lambda (&rest _) 1000))
              ((symbol-function 'dorg-sql) (lambda (&rest _) (setq gc-called t) nil))
              ((symbol-function 'display-warning) (lambda (&rest _) (cl-incf warned)))
              ((symbol-function 'dorg-dolt-server-list-mode) (lambda (&rest _) nil))
              ((symbol-function 'tabulated-list-print) (lambda (&rest _) nil)))
      (let ((dorg-dolt-compact-warn-threshold 500))
        (dorg-dolt-list)
        (should (= warned 1))
        (should-not gc-called))
      (setq warned 0)
      (let ((dorg-dolt-compact-warn-threshold 5000))
        (dorg-dolt-list)
        (should (= warned 0))))))

(ert-deftest dorg-test-dolt-compact-preserves-history-restore ()
  "`dorg-restore-to-commit' succeeds AFTER compaction — the dorg-history undo
path genuinely survives gc.  Non-vacuous: had gc rewritten/pruned history the
early commit would not be an ancestor of HEAD and restore would `user-error',
and deleting the `dorg-restore-to-commit' call leaves the title at \"A2\".

Runs on a DEDICATED per-store sql-server for an ISOLATED COPY (never the shared
test server): `CALL DOLT_GC()' targets only this copy's data-dir chunk store,
and the typed restore write path needs the server-affine lease /
`GET_LOCK' linearization (tracked-case) that the one-shot CLI seam cannot serve.
The dedicated server is started via implicit autostart on an owned copy, and
stopped on unwind; `--owned-claims'/`--attached-ports' are isolated so the
shared fixture's registries are untouched.  This is NOT a shared-fixture cold
start (`dorg-dolt-fixture--cold-starts' advances only in the shared start), so
the suite cold-starts=1 invariant is preserved."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — server transport for gc+restore needs it")))
  (let ((temp-base (dorg-test--make-owned-temp-dir "dorg-test-" "dolt-server-tests"))
        (dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
        (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((dolt-dir (dorg-test--copy-repo temp-base))
              (dorg-sql-transport 'server)
              (dorg-dolt-server-autostart t))
          (unwind-protect
              (let* ((root (dorg-test-history--import dolt-dir "* A\n" 100 "c1"))
                     (aid (dorg-test-history--id-by-title dolt-dir "A"))
                     (early (dorg-head-commit dolt-dir)))
                (dorg-test-history--set-title dolt-dir aid "A2" "c2")
                (should (equal "A2" (dorg-test-history--title-of dolt-dir aid)))
                (dorg-dolt-compact dolt-dir)
                ;; Undo path AFTER gc: restore the whole root to the early commit.
                (dorg-restore-to-commit dolt-dir root nil early "restore to c1")
                (should (equal "A" (dorg-test-history--title-of dolt-dir aid))))
            (dorg-dolt-server--stop dolt-dir)))
      (dorg-test--delete-owned-root temp-base))))

;;; ─── cu9xm: cross-process server-index test isolation (tracked-regression) ────────
;;
;; Since 5d6de85 the cross-process discovery index `dorg-dolt-server-index-dir'
;; defaults to a STABLE per-user path, so TMPDIR isolation no longer scopes it:
;; a fresh CLI child reads the OPERATOR's real index and discovers the live
;; tracker server, reddening the full suite.  The fix scopes the index to a
;; per-run OWNED dir via `dorg-test--isolate-server-index', which sets BOTH the
;; in-process var and the DORG_DOLT_SERVER_INDEX_DIR env var (so children
;; inherit isolation) and is idempotent within one process.  It is wired into
;; the SUITE-RUNNER bootstrap `dorg-setup-test-environment' (tests/dorg-tests-
;; runner.el) — the single choke point every supported run funnels through
;; (`make test', the fast/medium/full gates, and focused `dorg-run-tests--batch'
;; runs) — so isolation is established before any test consumes inventory.  The
;; SUBPROCESS invoker `dorg-test-cli--invoke' is the second wiring point (it
;; injects the isolated dir into every spawned child; see dorg-cli-tests.el).
;; These are the RED regression seams that force those contracts.

(defun dorg-test-cu9xm--fresh-eval (expr)
  "Eval EXPR in a genuinely separate `--batch -Q' Emacs; return (CODE . OUTPUT).
Adds the repo root, tests/, and the bundled lib/mysql + lib/clime to
`load-path' so `require' of `dorg-test-helpers' / `dorg-tests-runner' (which
pull the CLI + Dolt suites) resolves in a fresh process.  No in-process
state of this test's Emacs is visible to EXPR — real cross-process coverage."
  (let* ((root (file-name-as-directory
                (or (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "dorg-dolt-server.el")
                    default-directory)))
         (emacs (expand-file-name invocation-name invocation-directory))
         (args (list "--batch" "-Q"
                     "-L" root
                     "-L" (expand-file-name "tests" root)
                     "-L" (expand-file-name "lib/mysql" root)
                     "-L" (expand-file-name "lib/clime" root)
                     "--eval" (prin1-to-string expr)))
         (buf (generate-new-buffer " *cu9xm-fresh-probe*")))
    (unwind-protect
        (cons (apply #'call-process emacs nil buf nil args)
              (with-current-buffer buf (buffer-string)))
      (kill-buffer buf))))

(defun dorg-test-cu9xm--extract-result (output)
  "Read the `RESULT…END'-delimited sexp emitted by a fresh-eval probe, or nil."
  (when (string-match "RESULT\\(.*\\)END" output)
    (car (read-from-string (match-string 1 output)))))

(ert-deftest dorg-test-cu9xm-isolate-server-index-scopes-owned-per-run-dir ()
  "`dorg-test--isolate-server-index' scopes the cross-process server index to a
fresh, EXISTING, test-OWNED per-run dir, setting BOTH `dorg-dolt-server-index-dir'
AND the `DORG_DOLT_SERVER_INDEX_DIR' env var (children inherit) — never the
operator's per-user default — and is idempotent within one process.  RED now
\(helper absent).

Guards the nav's `fixed-nonexistent-global-path' mutant: `file-directory-p'
rejects a bare path string that names no real dir, and `dorg-test--owned-root-p'
rejects any dir that is not under a reapable per-run test root.  (Per-run
DISTINCTNESS across independent runs is proved by
`dorg-test-cu9xm-isolate-server-index-distinct-across-processes'.)

No teardown: the isolated dir is the process-global run index (established here
or by the suite-runner bootstrap); deleting it would sabotage every later test.
It is owned/reapable, so suite teardown reclaims it."
  (let ((dir (dorg-test--isolate-server-index)))
    (let ((canon (file-name-as-directory (expand-file-name dir))))
      (should (stringp dir))
      ;; A real, existing directory — not a bare fixed path string.
      (should (file-directory-p dir))
      ;; Under a test-owned run root (durably registered, reapable), never a
      ;; stray global path the run does not own.
      (should (dorg-test--owned-root-p dir))
      ;; BOTH carriers point at it.
      (should (equal canon (file-name-as-directory
                            (expand-file-name dorg-dolt-server-index-dir))))
      (should (equal canon (file-name-as-directory
                            (expand-file-name
                             (getenv "DORG_DOLT_SERVER_INDEX_DIR")))))
      ;; NEVER the operator's per-user default.
      (should-not (equal canon (file-name-as-directory
                                (expand-file-name
                                 (locate-user-emacs-file "dorg/server-stores/")))))
      ;; Idempotent within one process: a second call returns the SAME dir and
      ;; leaves both carriers unchanged (forces process-global caching — a
      ;; fresh-dir-per-call impl returns a different dir here and fails).
      (should (equal canon (file-name-as-directory
                            (expand-file-name (dorg-test--isolate-server-index)))))
      (should (equal canon (file-name-as-directory
                            (expand-file-name dorg-dolt-server-index-dir))))
      (should (equal canon (file-name-as-directory
                            (expand-file-name
                             (getenv "DORG_DOLT_SERVER_INDEX_DIR"))))))))

(ert-deftest dorg-test-cu9xm-isolate-server-index-distinct-across-processes ()
  "Two genuinely separate runs each get a DISTINCT isolated index dir: the scope
is PER-RUN, not a shared fixed global path.  RED now (helper absent => the child
errors before printing, so no RESULT marker is emitted).

Guards the nav's `fixed-global-path' mutant, which satisfies the single-process
contract (`…-scopes-owned-per-run-dir') yet returns the SAME path in every run,
colliding across concurrent lanes — exactly the isolation the bead exists to
restore."
  (let* ((probe '(progn
                   (require 'dorg-test-helpers)
                   (let ((d (dorg-test--isolate-server-index)))
                     (princ (format "RESULT%sEND"
                                    (prin1-to-string
                                     (file-name-as-directory (expand-file-name d)))))
                     ;; Keep the durable owned-root registry clean across the probe.
                     (ignore-errors (dorg-test--delete-owned-root d))
                     (kill-emacs 0))))
         (r1 (dorg-test-cu9xm--fresh-eval probe))
         (r2 (dorg-test-cu9xm--fresh-eval probe)))
    (should (= 0 (car r1)))
    (should (= 0 (car r2)))
    (let ((d1 (dorg-test-cu9xm--extract-result (cdr r1)))
          (d2 (dorg-test-cu9xm--extract-result (cdr r2))))
      (should (stringp d1))
      (should (stringp d2))
      (should-not (equal d1 d2)))))

(ert-deftest dorg-test-cu9xm-runner-bootstrap-auto-isolates-index ()
  "The suite-runner bootstrap `dorg-setup-test-environment' AUTOMATICALLY
isolates the cross-process index before any test consumes inventory, and does so
by invoking the named seam `dorg-test--isolate-server-index'.  A fresh run that
starts with a foreign index A on BOTH carriers (and a store registered in A)
must, after the bootstrap runs:
  - carry a run-owned dir B (!= A) in BOTH `dorg-dolt-server-index-dir' and
    DORG_DOLT_SERVER_INDEX_DIR;
  - have called `dorg-test--isolate-server-index' at least once, whose return
    value IS that B in both carriers (proves the wiring runs through the named
    helper, not some parallel ad-hoc replacement);
  - hold a B that is a test-OWNED reapable root (`dorg-test--owned-root-p');
  - a consumer (`--indexed-store-dirs') must NO LONGER see A's store (reads B).
RED now (the bootstrap does not call the helper => both carriers stay A, no call
is recorded, A is not owned, and A's store is still visible).

This is the blocker-2 wiring proof.  Two guards close the nav v2 residual:
  * an invoke-only injection cannot satisfy it — it exercises the IN-PROCESS
    entry point in a genuinely separate Emacs;
  * a NON-owned global replacement mutant (e.g. advising the bootstrap to set
    both carriers to the repo root without calling the helper) is re-reddened by
    the `:owned' assertion AND by `:calls'>=1 / `:ret'=B — the repo root is not a
    test-owned run index and the helper is never invoked."
  (let* ((probe
          '(progn
             (require 'dorg-dolt-server)
             (require 'dorg-test-helpers)
             ;; Wrap the named seam with a call counter BEFORE the runner loads,
             ;; capturing every return value.  Guard on fboundp so the RED state
             ;; (helper absent) still prints a RESULT with :calls 0 rather than
             ;; erroring: the parent then fails on the :calls/:ret/:owned asserts.
             (defvar dorg-test-cu9xm--isolate-returns nil)
             (when (fboundp 'dorg-test--isolate-server-index)
               (advice-add 'dorg-test--isolate-server-index :filter-return
                           (lambda (ret)
                             (push (file-name-as-directory (expand-file-name ret))
                                   dorg-test-cu9xm--isolate-returns)
                             ret)
                           '((name . dorg-test-cu9xm--count))))
             (let* ((idx-a (file-name-as-directory (make-temp-file "cu9xm-boot-a-" t)))
                    (store (file-name-as-directory (make-temp-file "cu9xm-boot-store-" t)))
                    (canon (dorg-dolt-server--canonical-dir store)))
               ;; Foreign index A on BOTH carriers, with a store registered in it.
               (setq dorg-dolt-server-index-dir idx-a)
               (setenv "DORG_DOLT_SERVER_INDEX_DIR" idx-a)
               (dorg-dolt-server--index-register store)
               ;; Load the suite-runner entry point and run ONLY its bootstrap.
               (require 'dorg-tests-runner)
               (dorg-setup-test-environment)
               (princ (format "RESULT%sEND"
                              (prin1-to-string
                               (list :a (file-name-as-directory (expand-file-name idx-a))
                                     :var (file-name-as-directory
                                           (expand-file-name dorg-dolt-server-index-dir))
                                     :env (let ((e (getenv "DORG_DOLT_SERVER_INDEX_DIR")))
                                            (and e (file-name-as-directory
                                                    (expand-file-name e))))
                                     :exists (file-directory-p dorg-dolt-server-index-dir)
                                     :owned (and (dorg-test--owned-root-p
                                                  dorg-dolt-server-index-dir)
                                                 t)
                                     :calls (length dorg-test-cu9xm--isolate-returns)
                                     :ret (car dorg-test-cu9xm--isolate-returns)
                                     :sees-a (and (member canon
                                                          (dorg-dolt-server--indexed-store-dirs))
                                                  t)))))
               (kill-emacs 0))))
         (res (dorg-test-cu9xm--fresh-eval probe)))
    (should (= 0 (car res)))
    (let ((data (dorg-test-cu9xm--extract-result (cdr res))))
      (should data)
      (let ((a (plist-get data :a))
            (var (plist-get data :var))
            (env (plist-get data :env))
            (ret (plist-get data :ret)))
        ;; BOTH carriers replaced with a run-owned B distinct from foreign A.
        (should-not (equal var a))
        (should-not (equal env a))
        ;; BOTH carriers agree on B, and B exists.
        (should (equal var env))
        (should (plist-get data :exists))
        ;; B is a test-OWNED reapable root — not a stray global path (e.g. the
        ;; repo root) the run does not own.  Kills the non-owned mutant.
        (should (plist-get data :owned))
        ;; The bootstrap reached B THROUGH the named helper: it was called and
        ;; its return value is exactly B in both carriers.  Kills any parallel
        ;; ad-hoc replacement that never invokes the seam.
        (should (>= (or (plist-get data :calls) 0) 1))
        (should (equal ret var))
        (should (equal ret env))
        ;; The consumer now reads B: A's store is invisible post-bootstrap.
        (should-not (plist-get data :sees-a))))))

(ert-deftest dorg-test-cu9xm-indexed-store-dirs-respects-bound-index ()
  "In-process guard: `--indexed-store-dirs' reads the CURRENTLY BOUND
`dorg-dolt-server-index-dir', so a store registered into index dir A is invisible
while the var is bound to a fresh dir B.  Green now and after (proves in-process
isolation is intact and the fix does not regress it)."
  (let* ((idx-a (file-name-as-directory (make-temp-file "cu9xm-idx-a-" t)))
         (idx-b (file-name-as-directory (make-temp-file "cu9xm-idx-b-" t)))
         (store (file-name-as-directory (make-temp-file "cu9xm-store-" t)))
         (canon (dorg-dolt-server--canonical-dir store)))
    (unwind-protect
        (progn
          (let ((dorg-dolt-server-index-dir idx-a))
            (dorg-dolt-server--index-register store))
          (let ((dorg-dolt-server-index-dir idx-a))
            (should (member canon (dorg-dolt-server--indexed-store-dirs))))
          (let ((dorg-dolt-server-index-dir idx-b))
            (should-not (member canon (dorg-dolt-server--indexed-store-dirs)))))
      (ignore-errors (delete-directory idx-a t))
      (ignore-errors (delete-directory idx-b t))
      (ignore-errors (delete-directory store t)))))
;;; ─── dbprobe: tri-state served-DB validation with bounded retry (dorg-dbprobe)
;;
;; RED contract for the live tracker transport outage (2026-07-17): a transient
;; connection/query/parse failure in the served-DB probe currently collapses to
;; nil, and callers treat nil as an authoritative WRONG-DB — falsely refusing (or
;; killing/breaking) a healthy server.  The fix makes served validation
;; TRI-STATE: (a) confirmed served-set, (b) confirmed identity mismatch
;; (`wrong-db'), (c) INCONCLUSIVE transport/query/parse failure — retried under a
;; small explicit bound, and on exhaustion surfaced as `probe-inconclusive'
;; (never `wrong-db') while the server and claim are preserved.  These tests are
;; RED against current code (single probe, nil-collapse, no `probe-inconclusive'
;; reason, no `dorg-dolt-server--probe-served-dbs' primitive).

(ert-deftest dorg-test-dbprobe-probe-served-dbs-tristate ()
  "`dorg-dolt-server--probe-served-dbs' distinguishes served-set from inconclusive.
Returns (served . LIST) only on a confirmed exit-0 parse (LIST may be empty =
confirmed-empty), and (inconclusive . CAUSE) on any connection/query/parse
failure — never conflating the two as nil.  Not-yet-implemented => RED."
  (let ((mode 'ok))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile buffer _display &rest _args)
                 (pcase mode
                   ('ok (when (bufferp buffer)
                          (with-current-buffer buffer
                            (insert "{\"rows\":[{\"Database\":\"dolt\"},{\"Database\":\"mysql\"}]}")))
                        0)
                   ('badexit 1)
                   ('badjson (when (bufferp buffer)
                               (with-current-buffer buffer (insert "not json")))
                             0)))))
      ;; Confirmed served set.
      (setq mode 'ok)
      (let ((r (dorg-dolt-server--probe-served-dbs 3306)))
        (should (eq (car r) 'served))
        (should (dorg-dolt-server--db-in-served-p (cdr r) "dolt")))
      ;; Connection/exit failure => inconclusive (NOT an empty served set).
      (setq mode 'badexit)
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; Parse failure => inconclusive.
      (setq mode 'badjson)
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive)))))

(ert-deftest dorg-test-dbprobe-transient-then-healthy-succeeds ()
  "A transient first probe failure then a healthy served set validates (no refuse).
`dorg-dolt-server--store-valid-p' retries an inconclusive probe under the bound;
a single transient failure must NOT surface as wrong-db.  Transformation
control: if the retry is removed, the first inconclusive is fatal and this
fails.  RED now (no retry; nil-collapse => wrong-db)."
  (let ((calls 0))
    (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
               (lambda (_port)
                 (cl-incf calls)
                 (if (= calls 1)
                     (cons 'inconclusive "connection refused")
                   (cons 'served (list "db" "mysql")))))
              ;; Keep current-code path deterministic + fast, and isolate the
              ;; served-set gate from the deeper schema/bound gates.
              ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
              ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
      (should (eq (dorg-dolt-server--store-valid-p 3306 "db") t))
      (should (> calls 1)))))                 ; proves it retried past the transient

(ert-deftest dorg-test-dbprobe-persistent-inconclusive-not-wrong-db ()
  "Persistent probe failure reports `probe-inconclusive', never `wrong-db'.
Bound exhaustion must yield a distinct verdict carrying the original cause so
the caller preserves the server/claim and raises a probe-inconclusive transport
error instead of a false wrong-db refusal.  RED now (returns wrong-db)."
  (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
             (lambda (_port) (cons 'inconclusive "connection refused")))
            ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
            ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
            ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
    (let ((verdict (dorg-dolt-server--store-valid-p 3306 "db")))
      (should (eq (plist-get verdict :reason) 'probe-inconclusive))
      (should-not (eq (plist-get verdict :reason) 'wrong-db))
      (should (plist-get verdict :cause)))))

(ert-deftest dorg-test-dbprobe-confirmed-missing-is-wrong-db ()
  "A CONFIRMED served set lacking the expected DB still fails closed as wrong-db.
Tri-state must not weaken the fail-closed identity gate: a positive served set
that omits the DB is a real mismatch, not inconclusive.  Guard (green now and
after); fails if IMPL mis-maps confirmed-missing to inconclusive."
  (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
             (lambda (_port) (cons 'served (list "other" "mysql"))))
            ((symbol-function 'dorg-dolt-server--list-served-dbs)
             (lambda (_) (list "other" "mysql")))
            ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
            ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
    (let ((verdict (dorg-dolt-server--store-valid-p 3306 "db")))
      (should (eq (plist-get verdict :reason) 'wrong-db)))))

(ert-deftest dorg-test-dbprobe-retry-bound-is-explicit ()
  "The inconclusive-probe retry is bounded by `dorg-dolt-server-probe-attempts'.
Fewer failures than the bound succeed; failures exhausting the bound yield
`probe-inconclusive'.  Proves the bound is explicit (not unbounded, not
single-shot).  RED now (no bound var, no retry)."
  (should (boundp 'dorg-dolt-server-probe-attempts))
  (let ((dorg-dolt-server-probe-attempts 3))
    (cl-letf (((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
              ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
      ;; Fails (bound-1) times then succeeds => valid.
      (let ((n 0))
        (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
                   (lambda (_port) (cl-incf n)
                     (if (< n dorg-dolt-server-probe-attempts)
                         (cons 'inconclusive "transient")
                       (cons 'served (list "db"))))))
          (should (eq (dorg-dolt-server--store-valid-p 3306 "db") t))))
      ;; Fails on every attempt within the bound => inconclusive.
      (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (_port) (cons 'inconclusive "transient"))))
        (should (eq (plist-get (dorg-dolt-server--store-valid-p 3306 "db") :reason)
                    'probe-inconclusive))))))

;;; ─── dbprobe (expanded): caller-level preservation + pending-owner + controls
;;
;; Codex REVIEW @94ed00b: the unit RED tests above cover only the primitive and
;; `--store-valid-p'; they can all green while the SIX destructive served-DB
;; probe callers still kill/break/replace.  These tests drive EACH caller
;; through an INCONCLUSIVE probe and prove it preserves the server + claim
;; (never signals, breaks, or replaces), define the post-start non-orphan
;; pending-owner state, and add the parser / signalled-process / exact-attempt /
;; distinct-cause controls.

(defmacro dorg-test-dbprobe--with-destruction-spies (spyvar &rest body)
  "Bind SPYVAR to a fresh list and record every destructive primitive into it.
Records (:killed-proc P) (:killed-pid N) (:broke DIR) (:signalled P S)
(:started DIR) (:started-detached DIR).  A caller that PRESERVES on an
inconclusive probe leaves SPYVAR empty of destructive markers."
  (declare (indent 1))
  `(let ((,spyvar (list)))
     (cl-letf (((symbol-function 'dorg-dolt-server--kill-process)
                (lambda (proc &rest _) (push (cons :killed-proc proc) ,spyvar) t))
               ((symbol-function 'dorg-dolt-server--kill-pid)
                (lambda (pid &rest _) (push (cons :killed-pid pid) ,spyvar) t))
               ((symbol-function 'dorg-dolt-server--break-claim)
                (lambda (dir &rest _) (push (cons :broke dir) ,spyvar) nil))
               ((symbol-function 'signal-process)
                (lambda (pid sig &rest _) (push (list :signalled pid sig) ,spyvar) nil))
               ((symbol-function 'dorg-dolt-server--start-unsafe)
                (lambda (dir &rest _) (push (cons :started dir) ,spyvar) 59991))
               ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                (lambda (dir &rest _) (push (cons :started-detached dir) ,spyvar) 59992)))
       ,@body)))

(defmacro dorg-test-dbprobe--with-foreign-live (dolt-dir probe-fn &rest body)
  "Bind DOLT-DIR to an isolated store carrying a foreign live daemon claim.
PROBE-FN is installed as `--probe-served-dbs'.  The claim classifies `live'
and compatible; deeper gates are stubbed so BODY exercises only the
served-DB-probe branch of a caller."
  (declare (indent 2))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "dbprobe-caller"))
            (lp (dorg-test--listen-port)))
       (unwind-protect
           (progn
             (dorg-test--write-claim ,dolt-dir :owner-pid 424242 :server-pid 424242
                                     :port (cdr lp) :owner-kind 'daemon)
             (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                       ((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                       ((symbol-function 'dorg-dolt-server--claim-classify)
                        (lambda (_claim &optional _deep) 'live))
                       ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (_c) t))
                       ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                        (lambda (d) (cons (expand-file-name d) "db")))
                       ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                       ((symbol-function 'dorg-dolt-server--dolt-uuid-current-p) (lambda (&rest _) t))
                       ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
                       ((symbol-function 'dorg-dolt-server--probe-served-dbs) ,probe-fn))
               ,@body))
         (delete-process (car lp))))))

(defun dorg-test-dbprobe--inconclusive (&rest _) (cons 'inconclusive "conn refused"))

;; ---- Destructive REPLACE callers: ensure-detached (#5), ensure (#6) ---------

(ert-deftest dorg-test-dbprobe-caller-ensure-detached-preserves-on-inconclusive ()
  "`--ensure-detached' must NOT break the claim or start a replacement daemon on
an inconclusive served probe; it preserves the foreign server + claim and
raises probe-inconclusive.  RED now (verdict collapses to wrong-db => break +
start-detached)."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test-dbprobe--with-foreign-live dolt-dir #'dorg-test-dbprobe--inconclusive
      (should-error (dorg-dolt-server--ensure-detached dolt-dir)
                    :type 'dorg-sql-transport-error)
      (should-not (assq :broke spy))
      (should-not (assq :started-detached spy))
      (should (dorg-dolt-server-claim dolt-dir)))))

(ert-deftest dorg-test-dbprobe-caller-ensure-strict-preserves-on-inconclusive ()
  "Canonical `--ensure-strict' must NOT break the claim or start a replacement on
a PERSISTENT inconclusive served probe of a foreign live compatible owner: the
served store is unconfirmed, so it fails closed (`--signal-store-invalid') and
preserves the foreign server + claim.  Carries the inconclusive-preserve proof
onto the canonical selector now that the legacy non-strict `--ensure' twin is
retired; the `--ensure-detached'/`--attach-only' siblings execute different
selector state machines and do not prove the `--ensure-strict' acceptance branch."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test-dbprobe--with-foreign-live dolt-dir #'dorg-test-dbprobe--inconclusive
      (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                    :type 'dorg-sql-transport-error)
      (should-not (assq :broke spy))
      (should-not (assq :started spy))
      (should (dorg-dolt-server-claim dolt-dir)))))

;; ---- Refuse-only ATTACH callers: ensure-strict (#1), attach-only (#4) -------
;; The bug here is a FALSE refusal of a healthy server: a transient probe
;; failure must be retried, so a transient-then-healthy probe ATTACHES.

(ert-deftest dorg-test-dbprobe-caller-ensure-strict-attaches-after-transient ()
  "`--ensure-strict' attaches (returns a port) when a transient probe failure is
followed by a healthy served set — never a false wrong-db refusal.  RED now
(first inconclusive => wrong-db => signal, no retry)."
  (dorg-test-dbprobe--with-destruction-spies spy
    (let ((n 0))
      (dorg-test-dbprobe--with-foreign-live dolt-dir
          (lambda (_p) (cl-incf n)
            (if (= n 1) (cons 'inconclusive "transient") (cons 'served (list "db"))))
        (cl-letf (((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
          (should (integerp (dorg-dolt-server--ensure-strict dolt-dir)))
          (should-not (assq :broke spy)))))))

(ert-deftest dorg-test-dbprobe-caller-attach-only-attaches-after-transient ()
  "`--attach-only' attaches after a transient probe failure then a healthy set.
RED now (first inconclusive => wrong-db => signal)."
  (dorg-test-dbprobe--with-destruction-spies spy
    (let ((n 0))
      (dorg-test-dbprobe--with-foreign-live dolt-dir
          (lambda (_p) (cl-incf n)
            (if (= n 1) (cons 'inconclusive "transient") (cons 'served (list "db"))))
        (cl-letf (((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
          (should (integerp (dorg-dolt-server--attach-only dolt-dir)))
          (should-not (assq :broke spy)))))))

;; ---- POST-START launch callers + non-orphan pending-owner state -------------
;; start-child (#2) and detached-start (#3) probe a server they JUST launched.
;; An inconclusive probe must NOT kill it (it may be healthy) yet must NOT leave
;; it untracked: retain the process object (pending-owner) and a claim marked
;; `:state probe-inconclusive', and raise.  Recovery re-probes the retained
;; object.

(ert-deftest dorg-test-dbprobe-start-child-pending-owner-not-killed ()
  "Post-start inconclusive: the launched child is RETAINED (pending-owner), not
killed, and its claim is not broken; the call raises probe-inconclusive.  RED
now (wrong-db => kill-process + break-claim + no retention)."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "dbprobe-start-child"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (fakebuf (generate-new-buffer " *dbprobe-fake*"))
           (fake (start-process "dbprobe-fake-server" fakebuf "sleep" "60"))
           (killed nil) (broke nil))
      (set-process-query-on-exit-flag fake nil)
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon
                                        :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-args)
                     (lambda (&rest _) '("sql-server")))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () 59001))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'start-process) (lambda (&rest _) fake))
                    ((symbol-function 'dorg-dolt-server--wait-ready) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                     (lambda (_p) (cons 'inconclusive "post-start refused")))
                    ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
                    ((symbol-function 'dorg-dolt-server--kill-process)
                     (lambda (p &rest _) (setq killed p) t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d) (setq broke t) nil)))
            (should-error (dorg-dolt-server--start-child-unsafe dolt-dir t)
                          :type 'dorg-sql-transport-error)
            (should-not killed)                       ; healthy-or-unknown: never killed
            (should-not broke)                        ; claim preserved
            ;; Retained as a locally-owned pending server (killable on recovery).
            (should (eq (car (gethash canon dorg-dolt-server--procs)) fake))
            ;; Claim durably marks the unconfirmed state, never `ready', and
            ;; carries the EXACT launched pid/port so it is discoverable and
            ;; recoverable — a rewrite to arbitrary metadata must fail here.
            (let ((claim (dorg-dolt-server-claim dolt-dir)))
              (should (eq (alist-get :state claim) 'probe-inconclusive))
              (should (equal (alist-get :server-pid claim) (process-id fake)))
              (should (equal (alist-get :port claim) 59001))))
        (when (process-live-p fake) (delete-process fake))
        (kill-buffer fakebuf)))))

(defmacro dorg-test-dbprobe--with-pending-child (dolt-dir canon fake port &rest body)
  "Seed a pending CHILD (retained proc + `:state probe-inconclusive' claim).
Binds DOLT-DIR/CANON/FAKE and seeds a live proc on PORT plus a durable
pending claim owned here (token `pending-tok').  BODY runs inside the
runtime with FAKE cleaned up on exit."
  (declare (indent 4))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "dbprobe-recover-child"))
            (,canon (dorg-dolt-server--canonical-dir ,dolt-dir))
            (fakebuf (generate-new-buffer " *dbprobe-fake2*"))
            (,fake (start-process "dbprobe-fake-server2" fakebuf "sleep" "60")))
       (set-process-query-on-exit-flag ,fake nil)
       (unwind-protect
           (progn
             (puthash ,canon (cons ,fake ,port) dorg-dolt-server--procs)
             (dorg-dolt-server--claim-write
              ,dolt-dir (list (cons :dolt-dir ,canon) (cons :host (system-name))
                              (cons :owner-pid (emacs-pid)) (cons :owner-kind 'emacs)
                              (cons :server-pid (process-id ,fake)) (cons :port ,port)
                              (cons :state 'probe-inconclusive) (cons :token "pending-tok")
                              (cons :policy 'server)
                              (cons :protocol dorg-dolt-server-claim-protocol)
                              (cons :created-at (float-time)) (cons :last-used (float-time))))
             (puthash ,canon "pending-tok" dorg-dolt-server--owned-claims)
             ,@body)
         (when (process-live-p ,fake) (delete-process ,fake))
         (kill-buffer fakebuf)))))

(ert-deftest dorg-test-dbprobe-child-recovery-via-ensure-strict-finalizes ()
  "CHILD recovery through the REAL caller `--ensure-strict': a pre-seeded pending
child (retained proc + `:state probe-inconclusive' claim), on a later ensure
whose re-probe confirms the served set, finalizes the SAME claim to `:state
ready' reusing the exact object — zero new `start-process'.
Non-vacuity: readiness MUST cost exactly one bounded served-store revalidation
(`--store-valid-p'), so deleting that re-probe fails the count; and the finalized
claim MUST keep the pending claim's identity tuple byte-for-byte
(token/owner-kind/server-pid/port), so a rewrite to fresh metadata fails.  RED
now (no pending recovery; the fast path publishes no `:state ready' and does not
re-probe a `probe-inconclusive' claim)."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59002
    (let ((before (dorg-dolt-server-claim dolt-dir))
          (validations 0) (started nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ;; The bounded served-store revalidation seam.  Counted so a
                ;; finalize-without-re-probe cannot pass.
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations) t))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'start-process)
                 (lambda (&rest _) (setq started t) fake)))
        (dorg-dolt-server--ensure-strict dolt-dir)   ; REAL lifecycle entrypoint
        (should-not started)                          ; reused exact object
        (should (= validations 1))                    ; exactly one bounded re-probe
        (let ((after (dorg-dolt-server-claim dolt-dir)))
          (should (eq (alist-get :state after) 'ready))     ; only state advances
          (should (eq (car (gethash canon dorg-dolt-server--procs)) fake))
          ;; SAME claim: identity tuple is preserved byte-for-byte.
          (should (equal (alist-get :token after) (alist-get :token before)))
          (should (eq (alist-get :owner-kind after) (alist-get :owner-kind before)))
          (should (equal (alist-get :server-pid after) (alist-get :server-pid before)))
          (should (equal (alist-get :port after) (alist-get :port before))))))))

(ert-deftest dorg-test-dbprobe-child-recovery-inconclusive-stays-pending ()
  "CHILD recovery whose re-probe is STILL inconclusive must NOT advance the claim
to `:state ready': the same pending claim is preserved (proc retained, identity
intact) and the entrypoint raises `probe-inconclusive'.  This pins readiness to
a CONFIRMED served result — a blind `:state ready' rewrite (nav BF1) fails here.
RED now (the fast path publishes a claim with no `:state', losing the pending
marker, and never re-probes)."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59002
    (let ((validations 0) (started nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations)
                   (list :reason 'probe-inconclusive :cause "still refused")))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'start-process)
                 (lambda (&rest _) (setq started t) fake)))
        (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                      :type 'dorg-sql-transport-error)
        (should (= validations 1))                    ; re-probed once, still unconfirmed
        (should-not started)                          ; never replaced
        ;; Pending, not ready: the retained child and its pending claim survive.
        (should (eq (car (gethash canon dorg-dolt-server--procs)) fake))
        (should (eq (alist-get :state (dorg-dolt-server-claim dolt-dir))
                    'probe-inconclusive))))))

;; ---- HOT-PATH cache bypass: a pending child must be revalidated (nav BF1) ----
;; The recovery entrypoints above re-probe correctly, but the HOT query path
;; (`--strict-port', and its autostart-off selector `--attach-only') returned a
;; locally-owned pending child's cached `--procs' port with ZERO re-probe — an
;; unconfirmed served set served as ready.  These pin the hot path to the SAME
;; tri-state revalidation.

(ert-deftest dorg-test-dbprobe-strict-port-pending-child-revalidates ()
  "HOT PATH (`--strict-port', autostart nil): a locally-owned pending child is NOT
hot-served on its cached `--procs' port.  Its served DB is re-probed EXACTLY ONCE
and, on a confirmed served set, the SAME claim finalizes to `:state ready' and the
port is returned (zero relaunch).  RED now (strict-port returns
`dorg-dolt-server-port' directly: validations=0 and the claim stays
`probe-inconclusive')."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59005
    (let ((dorg-dolt-server-autostart nil)
          (validations 0) (started nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations) t))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'start-process)
                 (lambda (&rest _) (setq started t) fake)))
        (should (eq (dorg-dolt-server--strict-port dolt-dir) 59005))
        (should (= validations 1))            ; hot path re-probed exactly once
        (should-not started)                  ; reused exact object, no relaunch
        (should (eq (alist-get :state (dorg-dolt-server-claim dolt-dir)) 'ready))))))

(ert-deftest dorg-test-dbprobe-strict-port-pending-child-inconclusive-refuses ()
  "HOT PATH (`--strict-port', autostart nil): a pending child whose re-probe is
STILL inconclusive is NEVER hot-served — the path raises `probe-inconclusive' and
the claim stays `:state probe-inconclusive', neither silently served nor advanced
to ready.  RED now (strict-port returns the cached port with zero re-probe,
serving an unconfirmed child as if ready)."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59006
    (let ((dorg-dolt-server-autostart nil) (validations 0))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations)
                   (list :reason 'probe-inconclusive :cause "still refused")))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil)))
        (should-error (dorg-dolt-server--strict-port dolt-dir)
                      :type 'dorg-sql-transport-error)
        (should (= validations 1))
        (should (eq (alist-get :state (dorg-dolt-server-claim dolt-dir))
                    'probe-inconclusive))))))

(ert-deftest dorg-test-dbprobe-attach-only-pending-child-not-overwritten ()
  "`--attach-only' over a locally-owned pending child re-probes its served DB
rather than OVERWRITING the pending marker with a plain (stateless) claim and
attaching it as ready.  On a confirmed served set it finalizes the SAME claim to
`:state ready' with identity preserved.  RED now (the owned branch republishes a
stateless claim and returns the port with zero re-probe)."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59007
    (let ((before (dorg-dolt-server-claim dolt-dir)) (validations 0))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations) t))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil)))
        (should (eq (dorg-dolt-server--attach-only dolt-dir) 59007))
        (should (= validations 1))            ; re-probed, not blindly republished
        (let ((after (dorg-dolt-server-claim dolt-dir)))
          (should (eq (alist-get :state after) 'ready))
          (should (equal (alist-get :token after) (alist-get :token before))))))))

(ert-deftest dorg-test-dbprobe-child-recovery-wrong-store-replaces-via-ensure-strict ()
  "CHILD recovery through the REAL caller `--ensure-strict' over a locally-owned
pending child whose re-probe CONFIRMS the wrong store reaps the mis-serving child
\(`--kill-process' on the retained handle) and starts a replacement
\(`--start-unsafe') — proving strict pending revalidation is not unconditional
preservation.

With the legacy non-strict `--ensure' selector retired, the retained strict
pending tests otherwise cover only served/finalize and inconclusive/preserve.
This carries the ONLY caller-level proof that an owned pending child confirmed
wrong-store is killed+replaced onto the canonical selector so that disposition
survives the removal.  Non-vacuity: exactly one bounded `--store-valid-p', the
retained handle is the reaped process, and a replacement is started."
  (dorg-test-dbprobe--with-pending-child dolt-dir canon fake 59011
    (let ((validations 0) (started nil) (killed nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations)
                   (list :reason 'wrong-db :served-dbs '("other"))))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (p &rest _) (setq killed p) t))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) 59991)))
        (should (eq (dorg-dolt-server--ensure-strict dolt-dir) 59991))
        (should (= validations 1))
        (should (eq killed fake))             ; reaped the mis-serving child
        (should started)))))                  ; started a correct replacement

(defmacro dorg-test-dbprobe--with-pending-daemon (dolt-dir port &rest body)
  "Seed a locally-owned pending daemon claim (`:state probe-inconclusive').
Binds DOLT-DIR; opens a real listener so PORT classifies live (NO --procs
entry — detached daemons are unregistered); writes a durable daemon claim
with token `pend-d', registers that token in the isolated owner table, and
uses server-pid 424242.  BODY runs with the listener cleaned up on exit."
  (declare (indent 2))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "dbprobe-recover-detached"))
            (lp (dorg-test--listen-port)) (,port (cdr lp)))
       (unwind-protect
           (progn
             (dorg-dolt-server--claim-write
              ,dolt-dir (list (cons :dolt-dir (dorg-dolt-server--canonical-dir ,dolt-dir))
                              (cons :host (system-name)) (cons :owner-pid 424242)
                              (cons :owner-kind 'daemon) (cons :server-pid 424242)
                              (cons :port ,port) (cons :state 'probe-inconclusive)
                              (cons :token "pend-d") (cons :policy 'server)
                              (cons :protocol dorg-dolt-server-claim-protocol)
                              (cons :created-at (float-time)) (cons :last-used (float-time))))
             (puthash (dorg-dolt-server--canonical-dir ,dolt-dir) "pend-d"
                      dorg-dolt-server--owned-claims)
             ,@body)
         (remhash (dorg-dolt-server--canonical-dir ,dolt-dir)
                  dorg-dolt-server--owned-claims)
         (delete-process (car lp))))))

(ert-deftest dorg-test-dbprobe-detached-recovery-via-ensure-detached-finalizes ()
  "DETACHED recovery through the REAL caller `--ensure-detached': a pending
daemon claim (`:state probe-inconclusive', live port, NO --procs entry), on a
later ensure whose re-probe confirms the served set, finalizes the SAME claim to
`:state ready' with the daemon's tuple and starts NO replacement.
The REAL `--claim-classify' is used (NOT stubbed to `pending'), so production
classification of a live pending claim is exercised.  Non-vacuity: readiness
costs exactly one bounded `--store-valid-p'; the finalized claim keeps the
daemon's identity tuple byte-for-byte.  RED now (no pending recovery; the live
arm never writes `:state ready', so a blind rewrite or break+replace is the only
way through).

The pending daemon models a REAL detached `dolt sql-server', so its
dolt-native `sql-server.info' is present with the daemon's pid/port and a
non-empty instance UUID: since tracked-regression, finalizing a pending claim to
`:state ready' requires that verified native identity (`--native-uuid-for'),
so a `:state ready' claim is always re-verifiable by a later cross-process
stop (no misleading/unstoppable ready claim)."
  (dorg-test-dbprobe--with-pending-daemon dolt-dir port
    ;; The daemon is a live detached dolt server; its native identity file
    ;; exists (pid/port match the pending claim, non-empty UUID) so the
    ;; srvid native-uuid finalize gate can promote the SAME claim to ready.
    (dorg-test-srvid--write-native-info dolt-dir 424242 port "recover-uuid")
    (let ((before (dorg-dolt-server-claim dolt-dir))
          (validations 0) (relaunched nil))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                ;; A bare listener answers TCP but not SQL; pin health to t so the
                ;; REAL classifier yields `live' (absent the pending arm) rather
                ;; than the incidental `wedged' of an unhealthy probe — so this
                ;; test fails on the missing pending finalization, not on reap.
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations) t))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (setq relaunched t) port)))
        (should (eq (dorg-dolt-server--ensure-detached dolt-dir) port))
        (should-not relaunched)                         ; no replacement daemon
        (should (= validations 1))                      ; exactly one bounded re-probe
        (let ((after (dorg-dolt-server-claim dolt-dir)))
          (should (eq (alist-get :state after) 'ready))
          ;; SAME daemon identity survives finalization.
          (should (equal (alist-get :token after) "pend-d"))
          (should (eq (alist-get :owner-kind after) 'daemon))
          (should (equal (alist-get :server-pid after) (alist-get :server-pid before)))
          (should (equal (alist-get :port after) port)))))))

(ert-deftest dorg-test-dbprobe-detached-recovery-inconclusive-stays-pending ()
  "DETACHED recovery whose re-probe is STILL inconclusive must NOT advance to
`:state ready', must NOT break the claim, and must NOT start a replacement
daemon: the pending daemon claim is preserved and `probe-inconclusive' raised.
Pins readiness to a confirmed served result through the REAL classifier.  RED
now (the live arm breaks the claim and starts a replacement on a non-t verdict)."
  (dorg-test-dbprobe--with-pending-daemon dolt-dir port
    (let ((validations 0) (relaunched nil) (broke nil))
      (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (cl-incf validations)
                   (list :reason 'probe-inconclusive :cause "still refused")))
                ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                 (lambda (d) (cons (expand-file-name d) "db")))
                ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (_d) (setq broke t) nil))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (setq relaunched t) port)))
        (should-error (dorg-dolt-server--ensure-detached dolt-dir)
                      :type 'dorg-sql-transport-error)
        (should (= validations 1))
        (should-not relaunched)
        (should-not broke)
        (should (eq (alist-get :state (dorg-dolt-server-claim dolt-dir))
                    'probe-inconclusive))))))

;; ---- Controls (nav: parser / signalled-process / exact-attempt / cause) -----

(ert-deftest dorg-test-dbprobe-control-rows-shape-parse ()
  "`--probe-served-dbs' extracts served names across row shapes; a confirmed
empty set is `served' (not inconclusive); malformed JSON is `inconclusive'."
  (let ((json nil) (exit 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_p _i buffer _d &rest _a)
                 (when (and (bufferp buffer) json)
                   (with-current-buffer buffer (insert json)))
                 exit)))
      (setq json "{\"rows\":[{\"Database\":\"dolt\"},{\"Database\":\"mysql\"}]}" exit 0)
      (let ((r (dorg-dolt-server--probe-served-dbs 3306)))
        (should (eq (car r) 'served))
        (should (dorg-dolt-server--db-in-served-p (cdr r) "dolt")))
      (setq json "{\"rows\":[{\"database\":\"dolt\"}]}")
      (should (dorg-dolt-server--db-in-served-p
               (cdr (dorg-dolt-server--probe-served-dbs 3306)) "dolt"))
      (setq json "{\"rows\":[]}")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'served))
      (setq json "garbage-not-json")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive)))))

(ert-deftest dorg-test-dbprobe-caller-detached-start-preserves-on-inconclusive ()
  "`--start-detached-unsafe' post-start inconclusive: never signals the daemon
pid and never breaks the claim.  `--kill-pid' is the sole raw-signal wrapper,
so spying it (never a silent no-op) proves no process is signalled.  RED now
(wrong-db => kill-pid + break-claim)."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "dbprobe-detached"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (lp (dorg-test--listen-port))
           (killed-pid nil) (broke nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-command)
                     (lambda (&rest _) "dolt sql-server"))
                    ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () (cdr lp)))
                    ;; Launch a "daemon" whose pid is provably alive (this Emacs)
                    ;; on the live listener port, so readiness passes and the
                    ;; served-DB check is reached.  kill-pid is SPIED (no real
                    ;; signal to this process).
                    ((symbol-function 'dorg-dolt-server--detached-launch)
                     (lambda (&rest _) (emacs-pid)))
                    ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                     (lambda (_p) (cons 'inconclusive "post-start refused")))
                    ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (setq killed-pid pid) t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d) (setq broke t) nil)))
            (should-error (dorg-dolt-server--start-detached-unsafe dolt-dir)
                          :type 'dorg-sql-transport-error)
            (should-not killed-pid)      ; sole signal wrapper never invoked
            (should-not broke)           ; claim not broken
            ;; NON-ORPHAN: the live detached daemon is tracked by a durable
            ;; NON-ready claim carrying its exact pid/port, so it is
            ;; discoverable and recoverable and no peer replaces it.
            (let ((claim (dorg-dolt-server-claim dolt-dir)))
              (should claim)
              (should (eq (alist-get :state claim) 'probe-inconclusive))
              (should (eq (alist-get :owner-kind claim) 'daemon))
              (should (equal (alist-get :server-pid claim) (emacs-pid)))
              (should (equal (alist-get :port claim) (cdr lp)))))
        ;; The pending daemon claim records `:server-pid (emacs-pid)' — a LIVE
        ;; pid this fixture never launched a real server for.  Left on disk under
        ;; the owned root it makes the end-of-run reap unable to confirm the
        ;; "server" gone (`owned-root reap unconfirmed', runner exit 1).  Delete
        ;; the synthetic claim before unwinding so the root reaps cleanly.
        (dorg-dolt-server--claim-delete-file dolt-dir)
        (delete-process (car lp))))))

(ert-deftest dorg-test-dbprobe-control-exact-attempt-count ()
  "The inconclusive probe is retried EXACTLY `dorg-dolt-server-probe-attempts'
times before an inconclusive verdict; a served result short-circuits earlier.
RED now (no bound var / no retry)."
  (should (boundp 'dorg-dolt-server-probe-attempts))
  (let ((dorg-dolt-server-probe-attempts 3))
    (cl-letf (((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
              ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
      (let ((n 0))
        (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
                   (lambda (_p) (cl-incf n) (cons 'inconclusive "x"))))
          (dorg-dolt-server--store-valid-p 3306 "db")
          (should (= n dorg-dolt-server-probe-attempts)))) ; exhausts the bound
      (let ((n 0))
        (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
                   (lambda (_p) (cl-incf n)
                     (if (= n 2) (cons 'served (list "db")) (cons 'inconclusive "x")))))
          (should (eq (dorg-dolt-server--store-valid-p 3306 "db") t))
          (should (= n 2)))))))                        ; short-circuits on served

(ert-deftest dorg-test-dbprobe-control-distinct-original-cause ()
  "First-cause policy: with DISTINCT causes per attempt, the `probe-inconclusive'
verdict retains the ORIGINAL (first) probe cause, not the last.  RED now (no
:cause at all).  Non-vacuous vs a last-cause implementation: the asserted cause
is the FIRST, which differs from every later attempt's cause."
  (let ((dorg-dolt-server-probe-attempts 3) (n 0))
    (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
               (lambda (_p) (cl-incf n)
                 (cons 'inconclusive (format "cause-attempt-%d" n))))
              ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
              ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
      (let ((v (dorg-dolt-server--store-valid-p 3306 "db")))
        (should (eq (plist-get v :reason) 'probe-inconclusive))
        (should (equal (plist-get v :cause) "cause-attempt-1"))))))    ; FIRST cause

(ert-deftest dorg-test-dbprobe-control-legacy-cause-shape ()
  "The `probe-inconclusive' verdict carries a :cause (kept as a distinct control
that a cause is surfaced at all).  RED now (verdict is wrong-db, no :cause)."
  (cl-letf (((symbol-function 'dorg-dolt-server--probe-served-dbs)
             (lambda (_p) (cons 'inconclusive "ECONNREFUSED 127.0.0.1:50882")))
            ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) nil))
            ((symbol-function 'dorg-dolt-server--schema-present-p) (lambda (&rest _) t))
            ((symbol-function 'dorg-dolt-server--bound-file-served-p) (lambda (&rest _) t)))
    (let ((v (dorg-dolt-server--store-valid-p 3306 "db")))
      (should (eq (plist-get v :reason) 'probe-inconclusive))
      (should (equal (plist-get v :cause) "ECONNREFUSED 127.0.0.1:50882")))))

;;; ─── dbprobe v3: confirmed-mismatch controls, census, strict parser, diag ────
;;
;; Codex v2 REJECT (nav-vx1): the preserve tests can pass while confirmed-
;; mismatch cleanup is disabled (unconditional preserve), while a lifecycle fn
;; still consumes the nil-collapsing shim, and while the parser maps malformed
;; payloads to `served'.  These controls pin the OPPOSITE branch: a CONFIRMED
;; wrong-store served-set must STILL drive the caller's destructive action, no
;; lifecycle fn may call `--list-served-dbs' directly, and malformed probe
;; payloads are `inconclusive' (never confirmed).

(defun dorg-test-dbprobe--served-other (&rest _)
  "Probe result: a CONFIRMED served-set that LACKS the expected db `db'."
  (cons 'served (list "other" "mysql")))

(ert-deftest dorg-test-dbprobe-mismatch-ensure-detached-still-replaces ()
  "CONFIRMED wrong-store through `--ensure-detached' STILL breaks + replaces."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test-dbprobe--with-foreign-live dolt-dir #'dorg-test-dbprobe--served-other
      (ignore-errors (dorg-dolt-server--ensure-detached dolt-dir))
      (should (assq :broke spy))
      (should (assq :started-detached spy)))))

(ert-deftest dorg-test-dbprobe-mismatch-child-still-kills-breaks ()
  "CONFIRMED wrong-store through `--start-child-unsafe' post-start STILL kills the
launched child and breaks the claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "dbprobe-mismatch-child"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (fakebuf (generate-new-buffer " *dbprobe-mm-child*"))
           (fake (start-process "dbprobe-mm-child" fakebuf "sleep" "60"))
           (killed nil) (broke nil))
      (set-process-query-on-exit-flag fake nil)
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-args) (lambda (&rest _) '("sql-server")))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () 59021))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'start-process) (lambda (&rest _) fake))
                    ((symbol-function 'dorg-dolt-server--wait-ready) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--probe-served-dbs) #'dorg-test-dbprobe--served-other)
                    ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) (list "other")))
                    ((symbol-function 'dorg-dolt-server--kill-process)
                     (lambda (p &rest _)
                       (setq killed p)
                       (delete-process p)
                       (accept-process-output p 0.05)
                       t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d)
                       (setq broke t)
                       ;; Model the real break effect after confirmed child death.
                       (dorg-dolt-server--claim-delete-file dolt-dir)
                       (remhash canon dorg-dolt-server--owned-claims)
                       nil)))
            (should-error (dorg-dolt-server--start-child-unsafe dolt-dir t)
                          :type 'dorg-sql-transport-error)
            (should (eq killed fake))       ; confirmed wrong store => kill the child
            (should broke))                 ; and break its claim
        (when (process-live-p fake) (delete-process fake))
        (kill-buffer fakebuf)))))

(ert-deftest dorg-test-dbprobe-mismatch-detached-still-kills-breaks ()
  "CONFIRMED wrong-store through `--start-detached-unsafe' post-start STILL kills
the daemon pid and breaks the claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((dolt-dir (dorg-test--repo "dbprobe-mismatch-detached"))
           (canon (dorg-dolt-server--canonical-dir dolt-dir))
           (lp (dorg-test--listen-port)) (killed-pid nil) (broke nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-command) (lambda (&rest _) "dolt sql-server"))
                    ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () (cdr lp)))
                    ((symbol-function 'dorg-dolt-server--detached-launch) (lambda (&rest _) (emacs-pid)))
                    ((symbol-function 'dorg-dolt-server--probe-served-dbs) #'dorg-test-dbprobe--served-other)
                    ((symbol-function 'dorg-dolt-server--list-served-dbs) (lambda (_) (list "other")))
                    ((symbol-function 'dorg-dolt-server--kill-pid) (lambda (pid &rest _) (setq killed-pid pid) t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d)
                       (setq broke t)
                       ;; Model the real break effect: the synthetic daemon pid
                       ;; is this Emacs and must never escape into fixture reap.
                       (dorg-dolt-server--claim-delete-file dolt-dir)
                       (remhash canon dorg-dolt-server--owned-claims)
                       nil)))
            (should-error (dorg-dolt-server--start-detached-unsafe dolt-dir)
                          :type 'dorg-sql-transport-error)
            (should (equal killed-pid (emacs-pid)))   ; confirmed wrong store => kill
            (should broke))
        (delete-process (car lp))))))

(ert-deftest dorg-test-dbprobe-static-census-no-direct-list-served-dbs ()
  "STATIC CENSUS: no lifecycle function consumes the nil-collapsing
`--list-served-dbs' shim directly; every served-DB decision routes through the
tri-state `--probe-served-dbs'.  RED now (start-child/detached/attach-only call
it directly)."
  (require 'find-func)
  (let* ((file (find-library-name "dorg-dolt-server"))
         (allowed '("dorg-dolt-server--probe-served-dbs"
                    "dorg-dolt-server--served-dbs-or-inconclusive"))
         (offenders nil) (cur nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^(defun \\([^ \t\n()]+\\)") (setq cur (match-string 1)))
        (when (and cur (not (member cur allowed))
                   (save-excursion
                     (re-search-forward "(dorg-dolt-server--list-served-dbs\\b"
                                        (line-end-position) t)))
          (cl-pushnew cur offenders :test #'equal))
        (forward-line 1)))
    (should-not offenders)))

(ert-deftest dorg-test-dbprobe-parser-strict-shapes ()
  "`--probe-served-dbs' is strict: `{\"rows\":[]}' is confirmed-empty `served';
but missing `rows', scalar/object `rows', malformed row entries, a nonzero exit,
and a SIGNALLED `call-process' are all `inconclusive' — never mapped to
`served'/confirmed.  RED now (primitive absent)."
  (let ((payload nil) (rc 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_p _i buffer _d &rest _a)
                 (when (and (bufferp buffer) payload)
                   (with-current-buffer buffer (insert payload)))
                 rc)))
      ;; confirmed empty
      (setq payload "{\"rows\":[]}" rc 0)
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'served))
      ;; missing rows member => inconclusive
      (setq payload "{}")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; rows is a scalar => inconclusive
      (setq payload "{\"rows\":7}")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; rows is an object, not an array => inconclusive
      (setq payload "{\"rows\":{\"a\":1}}")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; malformed row entry (scalar in array) => inconclusive
      (setq payload "{\"rows\":[5]}")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; nonzero exit => inconclusive
      (setq payload "{\"rows\":[{\"Database\":\"db\"}]}" rc 1)
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive))
      ;; signalled call-process (string return) => inconclusive
      (setq rc "Terminated")
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306)) 'inconclusive)))))

(ert-deftest dorg-test-szd8j-probe-rejects-non-single-column-rows ()
  "Only proper one-cell row objects may produce a confirmed served set.

A proper multi-column row, a valid row followed by a multi-column row, and an
improper row whose first cell looks valid are all malformed.  Each whole probe
must be `inconclusive'; no partial served set or parser condition may escape.
Transformation witness: the old first-cell-only predicate maps all three cases
to `served'."
  (let ((payload nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile buffer _display &rest _args)
                 (with-current-buffer buffer
                   (insert payload))
                 0)))
      (dolist (json '("{\"rows\":[{\"Database\":\"dolt\",\"Extra\":\"x\"}]}"
                      "{\"rows\":[{\"Database\":\"dolt\"},{\"Database\":\"mysql\",\"Extra\":\"x\"}]}"))
        (setq payload json)
        (should (eq (car (dorg-dolt-server--probe-served-dbs 3306))
                    'inconclusive))))
    ;; JSON cannot encode an improper alist, but the probe owns the parsed-value
    ;; boundary and promises that every malformed row shape is inconclusive.
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _buffer _display &rest _args) 0))
              ((symbol-function 'dorg-sql--parse-json-or-nil)
               (lambda (_raw)
                 (list (cons 'rows
                             (list (cons (cons 'Database "dolt")
                                         'trailing)))))))
      (should (eq (car (dorg-dolt-server--probe-served-dbs 3306))
                  'inconclusive)))))

(ert-deftest dorg-test-dbprobe-diagnostic-inconclusive-reports-cause-invents-no-served-set ()
  "`--store-invalid-message' for a `probe-inconclusive' verdict names the exact
cause and INVENTS no served-set text (no `served: none').  RED now (no
probe-inconclusive arm)."
  (let ((msg (dorg-dolt-server--store-invalid-message
              (list :reason 'probe-inconclusive :cause "ECONNREFUSED 127.0.0.1:50882")
              "/tmp/store" "db" 50882)))
    (should (string-match-p "ECONNREFUSED 127.0.0.1:50882" msg))
    (should-not (string-match-p "served: *none" msg))
    ;; An inconclusive probe confirmed NO served set, so the diagnostic must not
    ;; enumerate one at all.
    (should-not (string-match-p "served:" msg))))

;;; ─── dbprobe v4: real pending classification, peer blocking, death cleanup ───
;;
;; Codex v3 REJECT (nav-vx1): the recovery REDs could green without any served-DB
;; re-probe (BF1), asserted SAME-claim identity in prose only (BF2), and never
;; exercised the REAL `--claim-classify' pending arm, peer blocking, or the
;; confirmed-death cleanup boundary (BF3).  BF1/BF2 are closed above (re-probe
;; counts + byte-stable identity tuples + negative recovery).  These v4 tests
;; close BF3: the real classifier must route a live `:state probe-inconclusive'
;; claim to a blocking `pending' verdict and a confirmed-dead one to `dead'; a
;; peer must not attach/start/reap/break a live pending claim; and bounded
;; cleanup must fire ONLY after confirmed death.

(ert-deftest dorg-test-dbprobe-classify-pending-when-live-dead-when-gone ()
  "The REAL `--claim-classify' routes a `:state probe-inconclusive' claim to a
blocking `pending' verdict while its owner is not confirmed dead, and to `dead'
only once the owner is confirmed gone (port down AND same-host pid absent), so
bounded cleanup is death-gated.  A `:state ready' control on the same live port
still classifies `live', proving the pending routing is state-specific — not a
blanket override.  RED now (no pending arm: a live pending claim classifies
`live' and would be attached-as-ready by every peer)."
  (let ((pending (list (cons :host (system-name)) (cons :owner-pid (emacs-pid))
                       (cons :server-pid (emacs-pid)) (cons :port 61001)
                       (cons :owner-kind 'daemon) (cons :state 'probe-inconclusive)))
        (ready (list (cons :host (system-name)) (cons :owner-pid (emacs-pid))
                     (cons :port 61001) (cons :owner-kind 'daemon)
                     (cons :state 'ready))))
    ;; Live port + pending state => pending (blocking), never `live'.
    (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
              ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t)))
      (should (eq (dorg-dolt-server--claim-classify pending t) 'pending))
      ;; Control: a CONFIRMED (`:state ready') claim on the same live port is live.
      (should (eq (dorg-dolt-server--claim-classify ready t) 'live)))
    ;; Owner confirmed dead: port down AND same-host pid absent => `dead'
    ;; (cleanup permitted), NOT a lingering pending block.
    (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) nil))
              ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) nil)))
      (should (eq (dorg-dolt-server--claim-classify pending) 'dead)))))

(ert-deftest dorg-test-dbprobe-classify-pending-dead-on-server-pid-not-owner ()
  "A pending child whose SERVER pid (`:server-pid') is gone but whose OWNING Emacs
\(`:owner-pid') is still alive classifies `dead', not a stranded `pending': the
death gate keys on the ACTUAL server/child pid, not the owner process, so a child
that dies while its parent Emacs lives is reclaimable.  RED now (the pending arm
checks `:owner-pid', so a live owner strands the claim `pending' forever and no
peer can ever break/replace it — nav-vx1 BF2)."
  (let ((claim (list (cons :host (system-name))
                     (cons :owner-pid (emacs-pid))   ; owning Emacs: ALIVE
                     (cons :server-pid 424242)       ; the child server: GONE
                     (cons :port 61050) (cons :owner-kind 'emacs)
                     (cons :state 'probe-inconclusive))))
    ;; Port down; owner (emacs-pid) alive; the child pid (424242) enumerably gone.
    (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) nil))
              ((symbol-function 'dorg-dolt-server--pid-live-p)
               (lambda (pid) (eq pid (emacs-pid)))))
      (should (eq (dorg-dolt-server--claim-classify claim) 'dead)))))

(ert-deftest dorg-test-dbprobe-peer-attach-only-blocked-by-live-pending ()
  "A PEER `--attach-only' must NOT attach-as-ready, probe, start, reap, or break a
foreign `:state probe-inconclusive' claim whose owner is still live: it raises
`dorg-sql-transport-error' and leaves zero attached port.  Uses the REAL
classifier (a stubbed classifier would beg the question).  RED now (a live
pending claim classifies `live', so attach-only validates and attaches it)."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test--with-dolt-server-runtime
      (let* ((dolt-dir (dorg-test--repo "dbprobe-peer-attach"))
             (canon (dorg-dolt-server--canonical-dir dolt-dir))
             (probed nil)
             (claim (list (cons :dolt-dir canon) (cons :host (system-name))
                          (cons :owner-pid 424242) (cons :server-pid 424242)
                          (cons :port 61002) (cons :owner-kind 'daemon)
                          (cons :state 'probe-inconclusive) (cons :token "peer-pend")
                          (cons :protocol dorg-dolt-server-claim-protocol))))
        (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                  ((symbol-function 'dorg-dolt-server--effective-claim) (lambda (_d) claim))
                  ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (_c) t))
                  ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                  ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                   (lambda (d) (cons (expand-file-name d) "db")))
                  ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (setq probed t) t)))
          (should-error (dorg-dolt-server--attach-only dolt-dir)
                        :type 'dorg-sql-transport-error)
          (should-not probed)                 ; blocked before any attach probe
          (should-not (gethash canon dorg-dolt-server--attached-ports))
          (should-not spy))))))              ; never starts/reaps/breaks

(ert-deftest dorg-test-dbprobe-peer-ensure-strict-blocked-by-live-pending ()
  "Canonical `--ensure-strict' must NOT start a replacement over, or break, a
foreign `:state probe-inconclusive' claim whose owner is still live: the claim
reaches strict's explicit `pending' arm, which raises without probing the served
store, launching a second server, breaking the claim, or writing the attach
cache (the outage class).  Uses the REAL classifier.  Carries the peer-blocking
proof onto the canonical selector now that the legacy non-strict `--ensure' twin
is retired."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test--with-dolt-server-runtime
      (let* ((dolt-dir (dorg-test--repo "dbprobe-peer-ensure-strict"))
             (canon (dorg-dolt-server--canonical-dir dolt-dir))
             (probed nil)
             (claim (list (cons :dolt-dir canon) (cons :host (system-name))
                          (cons :owner-pid 424242) (cons :server-pid 424242)
                          (cons :port 61013) (cons :owner-kind 'daemon)
                          (cons :state 'probe-inconclusive) (cons :token "peer-pend2s")
                          (cons :protocol dorg-dolt-server-claim-protocol))))
        (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim) (lambda (_d) claim))
                  ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                  ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                  ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                   (lambda (d) (cons (expand-file-name d) "db")))
                  ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                  ((symbol-function 'dorg-dolt-server--store-valid-p)
                   (lambda (&rest _) (setq probed t) t)))
          (should-error (dorg-dolt-server--ensure-strict dolt-dir)
                        :type 'dorg-sql-transport-error)
          (should-not probed)
          (should-not (assq :started spy))    ; no second server launched
          (should-not (assq :broke spy))      ; live pending claim preserved
          (should-not (gethash canon dorg-dolt-server--attached-ports)))))))

(ert-deftest dorg-test-dbprobe-pending-confirmed-dead-is-cleaned-up-via-ensure-strict ()
  "Canonical `--ensure-strict': a `:state probe-inconclusive' claim whose port is
down AND whose same-host owner pid is absent classifies `dead' (REAL classifier),
so strict's `dead' arm breaks the abandoned claim and starts a session
replacement.  Paired with the strict peer-blocking test (live pending => zero
break/start): together they prove cleanup is death-gated, not unconditional.
Carries the death-gated cleanup proof onto the canonical selector now that the
legacy non-strict `--ensure' twin is retired."
  (dorg-test-dbprobe--with-destruction-spies spy
    (dorg-test--with-dolt-server-runtime
      (let* ((dolt-dir (dorg-test--repo "dbprobe-pending-dead-strict"))
             (canon (dorg-dolt-server--canonical-dir dolt-dir))
             (claim (list (cons :dolt-dir canon) (cons :host (system-name))
                          (cons :owner-pid 424242) (cons :server-pid 424242)
                          (cons :port 61014) (cons :owner-kind 'daemon)
                          (cons :state 'probe-inconclusive) (cons :token "dead-pend-s")
                          (cons :protocol dorg-dolt-server-claim-protocol))))
        (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim) (lambda (_d) claim))
                  ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) nil))
                  ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) nil)))
          (ignore-errors (dorg-dolt-server--ensure-strict dolt-dir))
          (should (assq :broke spy))          ; confirmed-dead pending claim cleared
          (should (assq :started spy)))))))   ; and a replacement started

;;; ─── srvid: verify detached server identity before stop (tracked-regression) ─────
;;
;; OC option-C v2 Contract (2026-07-17).  A PID is a transport handle, never
;; authority.  A cross-process TERM/KILL is authorized ONLY inside the claim
;; lock, after re-reading BOTH the DOrg daemon claim AND dolt-native
;; `<store>/.dolt/sql-server.info' and confirming the 4-tuple
;; {canonical-store, server-pid, port, dolt-instance-UUID} agrees across both,
;; plus live + served-store validation.  Any absent/stale/inconclusive/mismatched
;; field REFUSES: zero signal, zero claim mutation, zero replacement.
;;
;; SCOPE BOUNDARY.  srvid owns the KILL-AUTHORITY identity gate: A (persist
;; :uuid/:state), B (`--verified-daemon-pid' — full tuple + EVERY single-field
;; mutant), C (lock-held stop critical section: verify reads + served-store
;; under ONE held lock adjacent to signal; mid-critical re-read mismatch
;; refuses), D (wedged/wrong-store stop disposition), E (`--reap-wedged' foreign
;; refuse + `--ensure-detached' wedged refuse), F (foreign attach-only refuses
;; stop AND real `--attach-only' attaches), H (lifecycle retention + REAL
;; start->stop round-trip through the actual entrypoint).
;;
;; srvid's residual G is NARROWED to a KILL-AUTHORITY invariant only: the gate
;; accepts ONLY `:state ready'.  It refuses EVERY non-ready claim — dbprobe's
;; actual `:state probe-inconclusive' marker, a bare `:state pending', an
;; arbitrary non-ready state, AND an absent `:state' (an unfinalized daemon is
;; never a finalized identity) — so a stop can never signal an unfinalized
;; daemon regardless of which marker the pending producer chose.  Because the
;; gate refuses ALL non-ready claims, the FULL pending-owner LIFECYCLE
;; (post-start pending publication + classification + recovery + peer blocking +
;; death-gated cleanup) is NOT a kill-authority concern: it is owned by
;; tracked-regression (see the `dorg-test-dbprobe-*' pending tests in this file).
;; TWO lifecycle obligations reviewer flagged — (4a) crash-safe publish BEFORE
;; spawn, and (4b) `--effective-claim' treating a pending overlay as
;; authoritative over a different-live-native-port record — are orthogonal to
;; srvid's kill gate (which fail-closed refuses any non-ready claim) and are
;; flagged to the controller/OC for an explicit dbprobe/health-epic owner; a
;; discovered bead tracks them so they are not silently dropped.
;;
;; Every refusal proof spies the raw `signal-process' seam as a RECORDER that
;; records (PID . SIG) and returns nil (a SAFE no-op — never a live re-signal,
;; never a `--kill-pid' stub); a regression that signals ANY pid is caught, and
;; a matching stop asserts the EXACT (pid,sig) once.

(defun dorg-test-srvid--write-native-info (store pid port uuid)
  "Write STORE's dolt-native `sql-server.info' as PID:PORT:UUID."
  (let ((dolt (expand-file-name ".dolt" (dorg-dolt-server--canonical-dir store))))
    (make-directory dolt t)
    (with-temp-file (expand-file-name "sql-server.info" dolt)
      (insert (format "%d:%d:%s" pid port uuid)))))

(cl-defun dorg-test-srvid--write-daemon-claim (store pid port uuid
                                                     &key (state 'ready))
  "Write a durable `:owner-kind daemon' overlay claim for STORE.
Models the post-readiness daemon claim (OC #3): canonical store, server-pid,
port, and the dolt instance UUID persisted together, plus STATE (default
`ready').  A `:state pending' claim carries no UUID (identity not finalized)."
  (dorg-dolt-server--claim-write
   store
   (append
    (list (cons :dolt-dir (dorg-dolt-server--canonical-dir store))
          (cons :host (system-name))
          (cons :owner-pid pid)
          (cons :owner-kind 'daemon)
          (cons :server-pid pid)
          (cons :port port)
          (cons :state state)
          (cons :token "srvid-daemon-token")
          (cons :policy 'server)
          (cons :protocol dorg-dolt-server-claim-protocol)
          (cons :created-at (float-time))
          (cons :last-used (float-time)))
    (and uuid (list (cons :uuid uuid))))))

(defun dorg-test-srvid--claim-bytes (store)
  "Return STORE's claim file contents, or nil when the file is absent."
  (let ((file (dorg-dolt-server--claim-file store)))
    (and (file-exists-p file)
         (with-temp-buffer (insert-file-contents file) (buffer-string)))))

;;; A. Identity tuple persisted (OC #3) ───────────────────────────────────────

(ert-deftest dorg-test-srvid-publish-claim-persists-uuid-and-state ()
  "`--publish-claim' persists the dolt instance UUID AND `:state ready' (OC #3).
After readiness the daemon claim atomically carries the instance UUID and a
finalized state so a later stop can re-verify identity.  RED now: the current
arity has no UUID/STATE parameters, so this call errors."
  (dorg-test--with-dolt-server-runtime
    (let ((store (dorg-test--repo "srvid-persist-uuid")))
      (make-directory store t)
      (dorg-dolt-server--publish-claim
       store 4242 4242 (expand-file-name store) "db" 4242 'daemon "u-persist" 'ready)
      (let ((claim (dorg-dolt-server-claim store)))
        (should (equal (alist-get :uuid claim) "u-persist"))
        (should (eq (alist-get :state claim) 'ready))))))

;;; B. Verification predicate — full match and EVERY single-field mutant ───────

(ert-deftest dorg-test-srvid-verified-daemon-pid-full-match ()
  "`--verified-daemon-pid' returns the server pid on a FULL identity match:
tuple agrees across claim + native, owner-kind daemon, host-local, port live,
served-store confirms this store.  RED now: the predicate does not exist."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-gate-match"))
           (lp (dorg-test--listen-port)) (port (cdr lp)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _) t)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (should (equal (dorg-dolt-server--verified-daemon-pid store) 4242)))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-verified-daemon-pid-refuses-every-mutant ()
  "`--verified-daemon-pid' returns nil (=> caller refuses) for EACH single-field
corruption of the identity tuple.  RED now: the predicate does not exist."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-gate-mutants"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (canon (dorg-dolt-server--canonical-dir store))
           (info (expand-file-name ".dolt/sql-server.info" canon)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _) t)))
            ;; uuid != native
            (dorg-test-srvid--write-daemon-claim store 4242 port "u-claim")
            (dorg-test-srvid--write-native-info store 4242 port "u-native")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; server-pid != native
            (dorg-test-srvid--write-native-info store 9999 port "u-claim")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; port != native
            (dorg-test-srvid--write-native-info store 4242 (1+ port) "u-claim")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; native absent
            (delete-file info)
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; native malformed
            (make-directory (file-name-directory info) t)
            (with-temp-file info (insert "not-a-tuple"))
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; uuid missing (nil) in the claim
            (dorg-test-srvid--write-daemon-claim store 4242 port nil)
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; uuid EMPTY ("") in BOTH claim and native — an empty UUID is not a
            ;; finalized identity even when the two empties "agree".
            (dorg-test-srvid--write-daemon-claim store 4242 port "")
            (dorg-test-srvid--write-native-info store 4242 port "")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; canonical-store MISMATCH: the claim's `:dolt-dir' names a DIFFERENT
            ;; store than the one being verified — the tuple's canonical-store
            ;; field disagrees, so the pid is meaningless for THIS store.
            (dorg-dolt-server--claim-write
             store (list (cons :dolt-dir (expand-file-name "elsewhere" canon))
                         (cons :host (system-name)) (cons :owner-pid 4242)
                         (cons :owner-kind 'daemon) (cons :server-pid 4242)
                         (cons :port port) (cons :uuid "u1") (cons :state 'ready)
                         (cons :token "t") (cons :policy 'server)
                         (cons :protocol dorg-dolt-server-claim-protocol)
                         (cons :created-at (float-time)) (cons :last-used (float-time))))
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; owner-kind not daemon (foreign emacs) — full tuple otherwise
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (dorg-test--write-claim store :owner-pid 4242 :server-pid 4242
                                    :port port :owner-kind 'emacs)
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; served-store confirms a DIFFERENT db (tuple agrees)
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                       (lambda (&rest _) (list :reason 'wrong-db :served-dbs '("other")))))
              (should-not (dorg-dolt-server--verified-daemon-pid store))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-verified-daemon-pid-refuses-non-host-local ()
  "`--verified-daemon-pid' refuses a claim written by another host: a remote
owner's pid is meaningless locally.  RED now: the predicate does not exist."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-gate-remote"))
           (lp (dorg-test--listen-port)) (port (cdr lp)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _) t)))
            (dorg-dolt-server--claim-write
             store (list (cons :dolt-dir (dorg-dolt-server--canonical-dir store))
                         (cons :host "some-other-host") (cons :owner-pid 4242)
                         (cons :owner-kind 'daemon) (cons :server-pid 4242)
                         (cons :port port) (cons :uuid "u1") (cons :state 'ready)
                         (cons :token "t") (cons :policy 'server)
                         (cons :protocol dorg-dolt-server-claim-protocol)
                         (cons :created-at (float-time)) (cons :last-used (float-time))))
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (should-not (dorg-dolt-server--verified-daemon-pid store)))
        (delete-process (car lp))))))

;;; C. Lock-held stop critical section (Blocker 1) ────────────────────────────

(ert-deftest dorg-test-srvid-stop-matching-under-lock-deletes-after-death ()
  "Matching stop: the AUTHORITATIVE claim-read AND native-info read, the
served-store check, and `signal-process' all observe ONE identical, uninterrupted
claim-lock acquisition, in the order claim-read -> native-read -> store-check ->
signal -> death-confirmed -> delete; the claim is deleted only AFTER confirmed
death.

v4-STRENGTHEN-2 (addresses Codex REJECT reviewer 2026-07-17 20:37, remaining
Blocker 1).  The prior version recorded only lock-directory PRESENCE
(`file-directory-p') at each seam.  That is a static boolean: an implementation
that acquires the lock, re-reads/compares claim + native, RELEASES, then
RE-ACQUIRES a fresh lock only around served-store + signal would report
`under-lock = t' at every seam and pass — even though the metadata was mutable in
the release/re-acquire window between verify and actuate.

This version instruments `--acquire-claim-lock' / `--release-claim-lock'
themselves to stamp a monotonic ACQUISITION GENERATION: each acquire bumps a
counter and records the held generation; each release clears it and logs a
`:release' event.  Every identity seam records `(TAG . GENERATION)' (nil when no
lock is held).  The gate is proven correct only if the LAST (authoritative)
claim-read, the LAST native-read, the served-store check, and the signal all
carry the SAME non-nil generation AND no `:release' occurs between the
authoritative claim-read and the signal — i.e. a single uninterrupted critical
section from verify through actuate.  Earlier routing/classify reads OUTSIDE the
lock (generation nil) or in a PRIOR acquisition (a lower generation) never match.
RED now: the daemon stop branch is not wrapped in `--with-claim-lock' at all, so
`--acquire-claim-lock' is never called during stop and every seam records
generation nil."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-match"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (orig-acquire (symbol-function 'dorg-dolt-server--acquire-claim-lock))
           (orig-release (symbol-function 'dorg-dolt-server--release-claim-lock))
           (orig-claim-read (symbol-function 'dorg-dolt-server--claim-read))
           (orig-info (symbol-function 'dorg-dolt-server--dolt-server-info))
           (lock-gen 0) (cur-gen nil)          ; monotonic counter + held generation
           (events nil) (signalled nil)
           (store-check-gen 'unset) (signal-gen 'unset)
           (alive t) (death-recorded nil))
      (unwind-protect
          (cl-letf (;; Stamp an ACQUISITION GENERATION on the real lock helpers:
                    ;; every acquire bumps the counter and becomes the held gen;
                    ;; every release clears it and logs a `:release' event.  A
                    ;; release+re-acquire between verify and signal therefore
                    ;; produces two DIFFERENT generations, which the assertions
                    ;; below reject.
                    ((symbol-function 'dorg-dolt-server--acquire-claim-lock)
                     (lambda (lock)
                       (prog1 (funcall orig-acquire lock)
                         (setq lock-gen (1+ lock-gen) cur-gen lock-gen))))
                    ((symbol-function 'dorg-dolt-server--release-claim-lock)
                     (lambda (lock)
                       (prog1 (funcall orig-release lock)
                         (push (cons :release cur-gen) events)
                         (setq cur-gen nil))))
                    ;; The identity seams record (TAG . GENERATION) on EVERY call
                    ;; and pass through to the real reader, so both the unlocked
                    ;; routing reads (gen nil) and the final authoritative
                    ;; under-lock reads are visible to the ordering assertion.
                    ((symbol-function 'dorg-dolt-server--claim-read)
                     (lambda (dolt-dir)
                       (push (cons :claim-read cur-gen) events)
                       (funcall orig-claim-read dolt-dir)))
                    ((symbol-function 'dorg-dolt-server--dolt-server-info)
                     (lambda (dolt-dir)
                       (push (cons :native-read cur-gen) events)
                       (funcall orig-info dolt-dir)))
                    ;; Served-store verification is part of the identity gate; it
                    ;; must run UNDER the same held acquisition, BEFORE the signal.
                    ((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _)
                       (setq store-check-gen cur-gen)
                       (push (cons :store-check cur-gen) events)
                       t))
                    ((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid)
                       (if alive t
                         (unless death-recorded
                           (setq death-recorded t) (push :death-confirmed events))
                         nil)))
                    ;; RECORDER captures (PID . SIG), not just the pid, so the
                    ;; exact signal is asserted and a regression signalling any
                    ;; other pid/sig is caught.
                    ((symbol-function 'signal-process)
                     (lambda (pid sig &rest _)
                       (push (cons pid sig) signalled)
                       (setq signal-gen cur-gen)
                       (setq alive nil)
                       (push (cons :signal cur-gen) events)
                       nil))
                    ((symbol-function 'dorg-dolt-server--claim-delete-file)
                     (lambda (_d) (push :delete events) nil)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (dorg-dolt-stop store)
            (should (equal signalled '((4242 . SIGTERM))))  ; one signal, exact (pid . sig)
            ;; The FINAL authoritative claim-read AND native-read each occur under
            ;; a held lock, then store-check, signal, death-confirm, and delete —
            ;; one strictly-increasing sequence.  `cl-position-if ... :from-end t'
            ;; picks the LAST (authoritative) under-lock read of each seam; reads
            ;; with generation nil (unlocked routing) never match.
            (let* ((chrono (reverse events))
                   (under-lock-read-p
                    (lambda (tag)
                      (lambda (e) (and (consp e) (eq (car e) tag) (integerp (cdr e))))))
                   (claim-idx
                    (cl-position-if (funcall under-lock-read-p :claim-read)
                                    chrono :from-end t))
                   (native-idx
                    (cl-position-if (funcall under-lock-read-p :native-read)
                                    chrono :from-end t))
                   (store-idx
                    (cl-position-if (lambda (e) (and (consp e) (eq (car e) :store-check)))
                                    chrono))
                   (signal-idx
                    (cl-position-if (lambda (e) (and (consp e) (eq (car e) :signal)))
                                    chrono))
                   (death-idx (cl-position :death-confirmed chrono))
                   (delete-idx (cl-position :delete chrono))
                   (claim-gen (and claim-idx (cdr (nth claim-idx chrono))))
                   (native-gen (and native-idx (cdr (nth native-idx chrono)))))
              (should claim-idx)    ; a claim-read happened UNDER the lock
              (should native-idx)   ; a native-read happened UNDER the lock
              (should store-idx)
              (should signal-idx)
              (should death-idx)
              (should delete-idx)
              (should (< claim-idx native-idx store-idx signal-idx
                         death-idx delete-idx))
              ;; ONE identical uninterrupted acquisition: the authoritative claim
              ;; read, native read, store-check, and signal all carry the SAME
              ;; non-nil generation.  A verify-then-release-then-reacquire impl
              ;; would stamp two different generations here and fail.
              (should (integerp claim-gen))
              (should (eql claim-gen native-gen))
              (should (eql claim-gen store-check-gen))
              (should (eql claim-gen signal-gen))
              ;; ...and no lock RELEASE occurs between that authoritative claim
              ;; read and the signal — the critical section is never dropped.
              (should-not
               (cl-find-if (lambda (e) (and (consp e) (eq (car e) :release)))
                           chrono :start claim-idx :end signal-idx))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-mid-critical-native-change-refuses ()
  "Mid-critical NATIVE re-read mismatch: the authoritative verify re-reads native
`sql-server.info' UNDER the held lock IMMEDIATELY before the signal; if the
recorded instance UUID no longer matches the claim (the daemon exited and
something else rewrote the info between the routing read and the under-lock
actuation read), the gate returns nil and the stop REFUSES — ZERO signal, ZERO
delete, claim byte-identical.  The mismatch is keyed on LOCK STATE, not a read
counter: native reads return the MATCHING uuid while the lock is NOT held
(routing) and a DIFFERENT uuid once the lock IS held (authoritative re-read).
So the test also proves the mismatching read is the UNDER-LOCK one — an
implementation that reads/compares native OUTSIDE the lock would see only the
matching uuid and wrongly signal.  RED now: the shallow live classify signals
the recorded pid without an under-lock re-verify."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-midcrit-native"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (lockdir (concat (dorg-dolt-server--claim-file store) ".lock"))
           (read-under-lock nil) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--dolt-server-info)
                     (lambda (&rest _)
                       (let ((locked (file-directory-p lockdir)))
                         (when locked (setq read-under-lock t))
                         (list :pid 4242 :port port
                               :uuid (if locked "u2-reused" "u1")))))
                    ((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-stop store) :type 'user-error)
              (should read-under-lock)                     ; the authoritative re-read was UNDER the lock
              (should-not signalled)                       ; zero signal on re-read mismatch
              (should (equal before (dorg-test-srvid--claim-bytes store)))))  ; claim byte-identical
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-mid-critical-claim-change-refuses ()
  "Mid-critical CLAIM re-read mismatch (paired with the native case): the
authoritative verify re-reads the DOrg overlay claim UNDER the held lock; if the
claim's recorded UUID no longer matches native (a competing process rewrote the
overlay between the routing read and the under-lock actuation read), the gate
returns nil and the stop REFUSES — ZERO signal, ZERO delete, claim byte-identical
on disk.  Like the native case the mismatch is keyed on LOCK STATE: the claim
seam returns the MATCHING uuid while the lock is NOT held and a DIFFERENT uuid
once the lock IS held, so an implementation that compares the claim OUTSIDE the
lock (then locks only around served-store + signal) sees only the matching uuid
and wrongly signals.  RED now: the daemon stop branch performs no under-lock
claim re-verify."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-midcrit-claim"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (lockdir (concat (dorg-dolt-server--claim-file store) ".lock"))
           (claim-file (dorg-dolt-server--claim-file store))
           (read-under-lock nil) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                    ;; Pass through to the real file read, then corrupt the
                    ;; claim's UUID ONLY while the lock is held — models the
                    ;; overlay being rewritten between routing and actuation.
                    ((symbol-function 'dorg-dolt-server--claim-read)
                     (lambda (_dolt-dir)
                       (let ((claim (dorg-dolt-server--read-claim-form claim-file))
                             (locked (file-directory-p lockdir)))
                         (when (and claim locked)
                           (setq read-under-lock t)
                           (setf (alist-get :uuid claim) "u2-reused"))
                         claim)))
                    ((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-stop store) :type 'user-error)
              (should read-under-lock)                     ; the authoritative claim re-read was UNDER the lock
              (should-not signalled)                       ; zero signal on re-read mismatch
              (should (equal before (dorg-test-srvid--claim-bytes store)))))  ; claim byte-identical
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-unconfirmed-death-preserves-claim ()
  "Preserved-behavior guard: a stop whose kill is NOT confirmed (pid outlives
SIGKILL) PRESERVES the claim byte-identical and reports failure — so the
verified+lock-held rewrite must keep the confirmed-death-only deletion
invariant.  Non-vacuous AND distinct from a refusal: the signal recorder must be
NON-EMPTY (a signal WAS attempted at the verified pid) — so a `refuse-without-
attempt' regression, which also preserves the claim, cannot pass this test."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-unconfirmed"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (signalled nil)
           (dorg-dolt-server-stop-term-grace 0.2))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                    ;; pid never dies => death cannot be confirmed
                    ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_pid) t))
                    ((symbol-function 'signal-process)
                     (lambda (pid sig &rest _) (push (cons pid sig) signalled) nil)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-not (dorg-dolt-stop store))          ; not confirmed
              ;; A signal WAS attempted at the verified server pid (this is an
              ;; unconfirmed KILL, not a refusal): distinguishes the two.
              (should (member (cons 4242 'SIGTERM) signalled))
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-unknown-liveness-preserves-claim ()
  "Regression (tracked-regression ACCEPT-REJECT reviewer 2026-07-17): an `unknown'
liveness verdict — the platform cannot enumerate processes, so death is
UNPROVEN — must NOT be treated as confirmed death.  A verified stop signals the
daemon pid, then `dorg-dolt-server--pid-live-p' returns `unknown' throughout; the
stop must report failure (not-confirmed) and leave the claim byte-identical, so a
possibly-live server is never orphaned from its claim.  Non-vacuous AND distinct
from a refusal: the signal recorder is NON-EMPTY (a signal WAS attempted at the
verified pid), separating this unconfirmed KILL from a refusal-without-attempt.
RED before the fix: `--kill-pid' ended `(not (eq (pid-live-p pid) t))', so
`unknown' returned t and the claim was deleted after only a recorded SIGTERM."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-unknown-liveness"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (signalled nil)
           (dorg-dolt-server-stop-term-grace 0.2))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                    ;; enumeration unsupported => liveness is `unknown', never
                    ;; confirmable as dead.
                    ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_pid) 'unknown))
                    ((symbol-function 'signal-process)
                     (lambda (pid sig &rest _) (push (cons pid sig) signalled) nil)))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-not (dorg-dolt-stop store))          ; unknown != confirmed
              ;; A signal WAS attempted at the verified server pid (unconfirmed
              ;; KILL, not a refusal): distinguishes the two.
              (should (member (cons 4242 'SIGTERM) signalled))
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-confirmed-dead-clears-without-signal ()
  "Preserved-behavior guard: a confirmed-dead daemon (pid enumerably absent, port
down) clears the stale claim with ZERO signal — death already proven, no
possibly-reused pid touched.  The verified+lock-held rewrite must keep this
disposition.  Non-vacuous: fails if a regression signals the recorded (possibly
reused) pid or strands the stale claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-confirmed-dead"))
           (signalled nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) nil))
                ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) nil))
                ((symbol-function 'signal-process)
                 (lambda (pid &rest _) (push pid signalled) nil)))
        ;; port 1 is down; owner pid absent => classify `dead'
        (dorg-test-srvid--write-daemon-claim store 424242 1 "u1")
        (dorg-test-srvid--write-native-info store 424242 1 "u1")
        (dorg-dolt-stop store)
        (should-not signalled)
        (should-not (dorg-dolt-server-claim store))))))

;;; D. Wedged / served-store disposition (Blocker 2) — three controls ─────────

(ert-deftest dorg-test-srvid-stop-wrong-store-refuses-zero-signal ()
  "A daemon whose 4-tuple agrees but whose served-store confirms a DIFFERENT db
is a wrong-store owner: stop REFUSES — zero signal, claim byte-identical.
RED now: shallow live classify signals the recorded pid regardless of store."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-wrongstore"))
           (lp (dorg-test--listen-port)) (port (cdr lp)) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil))
                    ((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _) (list :reason 'wrong-db :served-dbs '("other")))))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-stop store) :type 'user-error)
              (should-not signalled)
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-stop-wedged-refuses-zero-signal ()
  "A wedged/SQL-dead daemon (TCP up, tuple agrees, served-store INCONCLUSIVE) is
NEVER reaped cross-process: stop REFUSES — zero signal, claim byte-identical.
Cross-process wedged reap is retired.  RED now: shallow live classify signals."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-wedged"))
           (lp (dorg-test--listen-port)) (port (cdr lp)) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil))
                    ((symbol-function 'dorg-dolt-server--store-valid-p)
                     (lambda (&rest _) (list :reason 'probe-inconclusive :cause "wedged"))))
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1")
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-stop store) :type 'user-error)
              (should-not signalled)
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

;;; E. Non-verification preserves (Blocker 3) ─────────────────────────────────

(ert-deftest dorg-test-srvid-reap-foreign-defcustom-retired-absent ()
  "The retired foreign-wedged reap custom variable is absent.
Non-vacuous: before IMPL, the retired custom variable is still defined in
`dorg-dolt-server.el', so this guard fails."
  (should-not (boundp (intern (concat "dorg-dolt-server-reap-foreign-"
                                      "wedged")))))

(ert-deftest dorg-test-srvid-reap-wedged-foreign-refuses-preserves ()
  "`--reap-wedged' over a foreign daemon claim: zero signal, NO break-claim, the
claim file byte-identical.  Cross-process foreign reap is retired: a
claim-recorded pid is not authority, and without a retained local process
object the reaper must preserve the claim."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-reap-foreign"))
           (lp (dorg-test--listen-port))
           (port (cdr lp))
           (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil)))
            ;; foreign daemon (not owned here, host-local, integer server-pid)
            (dorg-test-srvid--write-daemon-claim store 424242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (dorg-dolt-server--reap-wedged store)
              (should-not signalled)
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-reap-wedged-owned-object-still-reaped ()
  "Ownership control: a LOCAL owned wedged server (a live `--procs' object) IS
reaped via `--kill-process' on its retained handle — the gate is OWNERSHIP, not
blanket refusal.  Green now and must stay green after IMPL retires the foreign
cross-process reap."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-reap-owned"))
           (canon (dorg-dolt-server--canonical-dir store))
           (killed nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (proc &rest _) (setq killed proc) nil)))
        (puthash canon (list 'fake-proc) dorg-dolt-server--procs)
        (dorg-dolt-server--reap-wedged store)
        (should (eq killed 'fake-proc))
        (should-not (gethash canon dorg-dolt-server--procs))))))

(ert-deftest dorg-test-srvid-ensure-detached-wedged-refuses ()
  "Caller-level identity refusal: `--ensure-detached' over a FOREIGN wedged daemon
\(TCP up, SQL dead) REFUSES — ZERO cross-process reap (no kill), ZERO break, ZERO
replacement start.  Cross-process wedged reap is retired: identity is a kill
precondition and a wedged server cannot pass served-store validation, so a peer
neither reaps nor replaces it.  RED now: the wedged arm reaps the recorded pid
and starts a replacement daemon."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-ensure-wedged"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (killed nil) (broke nil) (started nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (setq killed pid) t))
                    ((symbol-function 'dorg-dolt-server--kill-process)
                     (lambda (p &rest _) (setq killed p) t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d) (setq broke t) nil))
                    ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                     (lambda (&rest _) (setq started t) port)))
            ;; foreign daemon claim (not owned here), live port + unhealthy => wedged
            (dorg-test-srvid--write-daemon-claim store 424242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-server--ensure-detached store)
                            :type 'dorg-sql-transport-error)
              (should-not killed)          ; no cross-process reap
              (should-not broke)           ; claim not broken
              (should-not started)         ; no replacement daemon
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-ensure-strict-wedged-foreign-refuses-preserves ()
  "Caller-level identity refusal: `--ensure-strict' over a FOREIGN wedged owner
\(TCP up, SQL dead) REFUSES — ZERO reap (no kill), ZERO break, ZERO attach-cache
eviction, ZERO replacement start; the foreign claim stays byte-identical.  This
arm is reached only with NO retained local `--procs' object, so the wedged owner
is foreign by construction: its PID is a transport handle not authority and a
wedged server cannot pass served-store validation, so a peer neither reaps nor
replaces it (attach or abandon).  RED before fix (tracked-regression): the strict
`wedged' arm calls `--reap-wedged' (a no-op for a foreign claim) then
UNCONDITIONALLY `--start-unsafe', starting a SECOND server over the foreign
owner and returning a port instead of signalling.  Mirrors the detached analog
`dorg-test-srvid-ensure-detached-wedged-refuses'."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-strict-wedged"))
           (canon (dorg-dolt-server--canonical-dir store))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (killed nil) (broke nil) (started nil) (reaped nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (setq killed pid) t))
                    ((symbol-function 'dorg-dolt-server--kill-process)
                     (lambda (p &rest _) (setq killed p) t))
                    ((symbol-function 'dorg-dolt-server--break-claim)
                     (lambda (_d) (setq broke t) nil))
                    ((symbol-function 'dorg-dolt-server--reap-wedged)
                     (lambda (&rest _) (setq reaped t) nil))
                    ((symbol-function 'dorg-dolt-server--start-unsafe)
                     (lambda (&rest _) (setq started t) port)))
            ;; foreign owner claim (not owned here), live port + unhealthy => wedged;
            ;; no retained `--procs' entry, so local-port is nil and the deep-classify
            ;; wedged arm is reached.
            (dorg-test-srvid--write-daemon-claim store 424242 port "u1")
            (let ((before (dorg-test-srvid--claim-bytes store)))
              (should-error (dorg-dolt-server--ensure-strict store)
                            :type 'dorg-sql-transport-error)
              (should-not killed)          ; no cross-process reap
              (should-not reaped)          ; foreign arm must not even call --reap-wedged
              (should-not broke)           ; claim not broken
              (should-not started)         ; no replacement server
              (should-not (gethash canon dorg-dolt-server--attached-ports)) ; no attach-cache mutation
              (should (equal before (dorg-test-srvid--claim-bytes store)))))
        (delete-process (car lp))))))

;;; F. Foreign attach-only (OC #6) ────────────────────────────────────────────

(ert-deftest dorg-test-srvid-stop-foreign-dolt-native-refuses-zero-signal ()
  "A live raw/foreign dolt sql-server known only via dolt-native
`sql-server.info' (no DOrg overlay) is NEVER signalled: `dorg-dolt-stop'
refuses and leaves the native record intact.  Control proving foreign servers
are attach-or-abandon, never reaped."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-foreign-native"))
           (lp (dorg-test--listen-port)) (port (cdr lp)) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil)))
            ;; native info only, no DOrg overlay => synthesized `:owner-kind dolt'
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            (should-error (dorg-dolt-stop store) :type 'user-error)
            (should-not signalled)
            ;; native record untouched
            (should (equal (plist-get (dorg-dolt-server--dolt-server-info store) :uuid)
                           "u1")))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-attach-only-foreign-live-attaches ()
  "F success control: `--attach-only' ATTACHES a live foreign server (returns its
port and memoizes the attach).  The identity KILL gate governs STOP authority
only — never attach — so a foreign server DOrg cannot signal is still usable.
Pairs with the foreign-stop-refusal test above (attach-or-abandon).  Green now
and MUST stay green after IMPL: the kill gate must not break the attach path."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-attach-foreign"))
           (canon (dorg-dolt-server--canonical-dir store))
           (lp (dorg-test--listen-port)) (port (cdr lp)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
                    ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--port-healthy-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--claim-compatible-p) (lambda (_c) t))
                    ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                     (lambda (d) (cons (expand-file-name d) "db")))
                    ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                    ((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t)))
            ;; a live foreign daemon owned by another DOrg process (not owned here)
            (dorg-test-srvid--write-daemon-claim store 424242 port "u1")
            (should (eq (dorg-dolt-server--attach-only store) port))
            (should (eq (gethash canon dorg-dolt-server--attached-ports) port)))
        (delete-process (car lp))))))

;;; G. srvid residual: the gate accepts ONLY `:state ready' (marker-robust)

(ert-deftest dorg-test-srvid-verified-daemon-pid-accepts-only-ready ()
  "srvid residual kill-authority invariant: `--verified-daemon-pid' accepts ONLY a
`:state ready' claim.  With a FULL matching tuple (uuid==native, pid/port==native,
port live, served-store confirms this store) it returns nil — refusing to signal —
for EVERY non-ready state: dbprobe's ACTUAL `:state probe-inconclusive' marker, a
bare `:state pending', an ARBITRARY non-ready state, AND an ABSENT `:state' (an
unfinalized/legacy daemon is not a finalized identity).  So a stop can never
signal an unfinalized daemon regardless of which marker the pending producer
chose — an implementation special-casing only `pending' would wrongly signal a
dbprobe-owned `probe-inconclusive' daemon.  RED now: the predicate does not exist.
\(The full pending-owner LIFECYCLE — publish/classify/recovery/peer-block/cleanup
— is tracked-regression's; this is srvid's kill-gate residual only.)"
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-only-ready"))
           (lp (dorg-test--listen-port)) (port (cdr lp)) (signalled nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'signal-process)
                     (lambda (pid &rest _) (push pid signalled) nil)))
            (dorg-test-srvid--write-native-info store 4242 port "u1")
            ;; dbprobe's ACTUAL unfinalized marker, FULL matching tuple otherwise.
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1"
                                                 :state 'probe-inconclusive)
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; A bare `pending' marker (full tuple otherwise).
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1" :state 'pending)
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; An ARBITRARY non-ready state (full tuple otherwise).
            (dorg-test-srvid--write-daemon-claim store 4242 port "u1" :state 'frobnicate)
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            ;; ABSENT `:state' — a legacy/unfinalized claim is not a ready identity.
            (dorg-dolt-server--claim-write
             store (list (cons :dolt-dir (dorg-dolt-server--canonical-dir store))
                         (cons :host (system-name)) (cons :owner-pid 4242)
                         (cons :owner-kind 'daemon) (cons :server-pid 4242)
                         (cons :port port) (cons :uuid "u1")
                         (cons :token "t") (cons :policy 'server)
                         (cons :protocol dorg-dolt-server-claim-protocol)
                         (cons :created-at (float-time)) (cons :last-used (float-time))))
            (should-not (dorg-dolt-server--verified-daemon-pid store))
            (should-not signalled))       ; never signalled through any non-ready state
        (delete-process (car lp))))))

;;; H. Lifecycle preserved + CLI round-trip (OC #1/#7/#8) ─────────────────────

(ert-deftest dorg-test-srvid-lifecycle-surface-preserved ()
  "Retention guard (OC #7/#8): identity verification must NOT delete the detached
lifecycle, the autostart option, or the raw-pid transport primitive.  Fails if a
future edit removes any of them."
  (should (fboundp 'dorg-dolt-server--kill-pid))
  (should (fboundp 'dorg-dolt-server--kill-process))
  (should (fboundp 'dorg-dolt-server--detached-launch))
  (should (fboundp 'dorg-dolt-server--start-detached-unsafe))
  (should (fboundp 'dorg-dolt-server--ensure-detached))
  (should (fboundp 'dorg-dolt-server--attach-only))
  (should (boundp 'dorg-dolt-server-autostart)))

(ert-deftest dorg-test-srvid-real-start-persists-tuple-then-stop-round-trips ()
  "H — REAL start->stop round-trip through the ACTUAL entrypoint (NOT a fixture
`--publish-claim').  `dorg-dolt-server--start-detached-unsafe' — driven with
synthetic launch/readiness/served-DB seams and the daemon's native
`sql-server.info' — must persist a READY identity tuple carrying the native
instance :uuid AND `:state ready', proving the real start path (not a test shim)
wires identity through.  Then, with process-local state cleared to model a
SEPARATE process, `dorg-dolt-stop' verifies the matching tuple UNDER the lock,
signals the server pid exactly once (exact pid . sig), and releases the claim.
RED now: the real start publish has no :uuid/:state, so the persisted claim
carries no finalized identity to re-verify."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-real-rt"))
           (canon (dorg-dolt-server--canonical-dir store))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (lockdir (concat (dorg-dolt-server--claim-file store) ".lock"))
           (signalled nil) (lock-at-signal 'unset) (alive t))
      (make-directory store t)
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-command)
                     (lambda (&rest _) "dolt sql-server"))
                    ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () port))
                    ((symbol-function 'dorg-dolt-server--detached-launch)
                     (lambda (&rest _) 4242))
                    ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                    ;; The launched daemon pid is "alive" through readiness (the
                    ;; stop phase rebinds this to model death after the signal).
                    ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--served-dbs-or-inconclusive)
                     (lambda (_p) (cons 'served (list "db"))))
                    ((symbol-function 'dorg-dolt-server--db-in-served-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t)))
            ;; The daemon has written its native identity file (pid:port:uuid).
            (dorg-test-srvid--write-native-info store 4242 port "u-real")
            ;; REAL START: persists the READY tuple through the actual entrypoint.
            (should (eq (dorg-dolt-server--start-detached-unsafe store) port))
            (let ((claim (dorg-dolt-server-claim store)))
              (should (eq (alist-get :state claim) 'ready))
              (should (equal (alist-get :uuid claim) "u-real"))  ; native uuid wired through
              (should (equal (alist-get :server-pid claim) 4242))
              (should (eq (alist-get :owner-kind claim) 'daemon)))
            ;; Model a SEPARATE process: drop all in-memory ownership/attach state.
            (clrhash dorg-dolt-server--owned-claims)
            (clrhash dorg-dolt-server--attached-ports)
            (clrhash dorg-dolt-server--procs)
            (ignore canon)
            ;; REAL STOP: verified tuple, under the lock, exactly one (pid . sig).
            (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                       (lambda (_pid) (if alive t nil)))
                      ((symbol-function 'signal-process)
                       (lambda (pid sig &rest _)
                         (push (cons pid sig) signalled)
                         (setq lock-at-signal (file-directory-p lockdir))
                         (setq alive nil) nil)))
              (dorg-dolt-stop store)
              (should (equal signalled '((4242 . SIGTERM))))
              (should (eq lock-at-signal t))
              (should-not (dorg-dolt-server-claim store))))
        (delete-process (car lp))))))

;;; I. ACCEPT-REJECT corrections (reviewer 2026-07-17 22:03) ───────────────────

(ert-deftest dorg-test-srvid-stop-daemon-routing-revalidates-overlay-under-lock ()
  "Blocker 1: `dorg-dolt-stop' selects the host-local daemon branch from the
PRE-LOCK `--effective-claim', but the destructive stale-cleanup delete must run
only after re-confirming UNDER THE HELD LOCK that the CURRENT on-disk overlay is
still a host-local daemon.  Race: the unlocked routing read sees daemon claim A;
by the time the lock is held the overlay is a NEWER, DEAD, FOREIGN `:owner-kind
emacs' claim B (different token).  Daemon stale-cleanup must NOT delete B — zero
signal, B byte-identical, refuse.  Token AND owner-kind both differ from A, so a
UUID-only mutant cannot satisfy this.  RED now: the fallback deletes ANY
lock-held claim that classifies `dead', so it deletes the foreign B."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-routing-race"))
           (canon (dorg-dolt-server--canonical-dir store))
           (signalled nil)
           ;; Synthetic pre-lock daemon claim A: routes stop into the daemon branch.
           (daemon-a (list (cons :dolt-dir canon) (cons :host (system-name))
                           (cons :owner-pid 4242) (cons :owner-kind 'daemon)
                           (cons :server-pid 4242) (cons :port 65000)
                           (cons :uuid "u-a") (cons :state 'ready)
                           (cons :token "daemon-a-token") (cons :policy 'server)
                           (cons :protocol dorg-dolt-server-claim-protocol)
                           (cons :created-at (float-time)) (cons :last-used (float-time)))))
      (cl-letf (((symbol-function 'dorg-dolt-server--effective-claim)
                 (lambda (&rest _) daemon-a))
                ((symbol-function 'signal-process)
                 (lambda (pid &rest _) (push pid signalled) nil)))
        ;; The REAL on-disk overlay is a newer, DEAD, foreign Emacs claim B
        ;; (absent pid, port down => classifies `dead').
        (dorg-test--write-claim store :owner-pid 999999 :server-pid 999999
                                :port 1 :owner-kind 'emacs :token "foreign-b-token")
        (let ((before (dorg-test-srvid--claim-bytes store)))
          (should-error (dorg-dolt-stop store) :type 'user-error)
          (should-not signalled)                                    ; never signalled
          (should (equal before (dorg-test-srvid--claim-bytes store)))  ; B byte-identical
          ;; B still present and still the foreign emacs claim (not daemon-cleaned).
          (let ((cur (dorg-dolt-server-claim store)))
            (should (eq (alist-get :owner-kind cur) 'emacs))
            (should (equal (alist-get :token cur) "foreign-b-token"))))))))

(ert-deftest dorg-test-srvid-pending-recovery-persists-uuid-then-stops ()
  "Blocker 2: the REAL detached pending-recovery path (`--ensure-detached' ->
pending branch -> finalize) must transition to `:state ready' ONLY after wiring
the dolt-native instance UUID into the claim, preserving the original token and
created-at.  A SEPARATE-process `dorg-dolt-stop' then verifies the full tuple and
round-trips.  RED now: `--finalize-pending-claim' sets only :state, so the
recovered ready claim carries :uuid nil and `--verified-daemon-pid' rejects it
forever — the recovered daemon can never be stopped."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-pending-recover"))
           (lp (dorg-test--listen-port)) (port (cdr lp))
           (signalled nil) (alive t))
      (unwind-protect
          (progn
            ;; A pending detached daemon claim (probe-inconclusive, NO uuid) plus
            ;; its dolt-native identity file.
            (dorg-test-srvid--write-daemon-claim store 4242 port nil
                                                 :state 'probe-inconclusive)
            ;; This first ensure is the publishing process recovering its own
            ;; provisional owner.  The separate-process phase begins only after
            ;; finalization below, where the owner tables are explicitly cleared.
            (puthash (dorg-dolt-server--canonical-dir store)
                     "srvid-daemon-token" dorg-dolt-server--owned-claims)
            (dorg-test-srvid--write-native-info store 4242 port "u-recover")
            (let ((orig (dorg-dolt-server-claim store)))
              (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                        ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
                        ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                        ((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t)))
                ;; REAL recovery: pending -> ready, native uuid wired in.
                (should (eq (dorg-dolt-server--ensure-detached store) port))
                (let ((claim (dorg-dolt-server-claim store)))
                  (should (eq (alist-get :state claim) 'ready))
                  (should (equal (alist-get :uuid claim) "u-recover"))   ; native uuid persisted
                  (should (equal (alist-get :server-pid claim) 4242))
                  ;; identity preserved across finalize (NOT a fresh token/created-at)
                  (should (equal (alist-get :token claim) (alist-get :token orig)))
                  (should (equal (alist-get :created-at claim)
                                 (alist-get :created-at orig)))))
              ;; SEPARATE process: drop in-memory ownership/attach state, round-trip stop.
              (clrhash dorg-dolt-server--owned-claims)
              (clrhash dorg-dolt-server--attached-ports)
              (clrhash dorg-dolt-server--procs)
              (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p) (lambda (&rest _) t))
                        ((symbol-function 'dorg-dolt-server--pid-live-p)
                         (lambda (_p) (if alive t nil)))
                        ((symbol-function 'signal-process)
                         (lambda (pid sig &rest _)
                           (push (cons pid sig) signalled) (setq alive nil) nil)))
                (dorg-dolt-stop store)
                (should (equal signalled '((4242 . SIGTERM))))
                (should-not (dorg-dolt-server-claim store)))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-direct-start-without-native-refuses-ready ()
  "Blocker 2: `--start-detached-unsafe' must NOT publish `:state ready' when the
dolt-native `sql-server.info' identity is absent or does not match the launched
pid/port — a ready claim with a nil/wrong uuid is unstoppable and misleading.
It must retain a NON-ready (probe-inconclusive) blocking daemon claim and refuse.
RED now: the start path reads `plist-get :uuid' unconditionally and publishes
ready even when native is missing/mismatched."
  (dorg-test--with-dolt-server-runtime
    (let* ((store (dorg-test--repo "srvid-direct-no-native"))
           (canon (dorg-dolt-server--canonical-dir store))
           (lp (dorg-test--listen-port)) (port (cdr lp)))
      (make-directory store t)
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt--target)
                     (lambda (_d) (list :canonical-repo-dir canon :server-data-dir canon
                                        :repo-dir canon :db-name "db")))
                    ((symbol-function 'dorg-dolt--target-sql-server-command)
                     (lambda (&rest _) "dolt sql-server"))
                    ((symbol-function 'dorg-dolt-server--expected-binding) (lambda (_d) nil))
                    ((symbol-function 'dorg-dolt-server--dir-store-valid-p) (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--free-port) (lambda () port))
                    ((symbol-function 'dorg-dolt-server--detached-launch) (lambda (&rest _) 4242))
                    ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
                    ((symbol-function 'dorg-dolt-server--served-dbs-or-inconclusive)
                     (lambda (_p) (cons 'served (list "db"))))
                    ((symbol-function 'dorg-dolt-server--db-in-served-p) (lambda (&rest _) t)))
            ;; Phase 1: native ABSENT — refuse, retain a non-ready blocking claim.
            (should-error (dorg-dolt-server--start-detached-unsafe store)
                          :type 'dorg-sql-transport-error)
            (let ((claim (dorg-dolt-server-claim store)))
              (should claim)
              (should-not (eq (alist-get :state claim) 'ready))   ; NOT misleading-ready
              (should-not (alist-get :uuid claim)))
            ;; Phase 2: native present but pid MISMATCHED — same refusal.
            (dorg-test-srvid--write-native-info store 9999 port "u-x")
            (should-error (dorg-dolt-server--start-detached-unsafe store)
                          :type 'dorg-sql-transport-error)
            (let ((claim (dorg-dolt-server-claim store)))
              (should-not (eq (alist-get :state claim) 'ready))
              (should-not (alist-get :uuid claim))))
        (delete-process (car lp))))))

(ert-deftest dorg-test-srvid-no-new-supervisor-symbol ()
  "Source-scan guard (fatalex doctrine): identity verification introduces NO
supervisor / IPC-endpoint / process-start-time subsystem.  Fails if a future
edit defines such a symbol in the dolt-server module."
  (require 'find-func)
  (let ((file (find-library-name "dorg-dolt-server"))
        (forbidden "^(\\(?:cl-\\)?def\\(?:un\\|var\\|custom\\|macro\\)[ \t]+[^ \t\n()]*\\(?:supervisor\\|ipc-endpoint\\|process-start-time\\|proc-start-time\\)"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should-not (re-search-forward forbidden nil t)))))

;;; ─── maij2: provisional owner lifecycle + reconciliation authority ───
;;
;; A pending overlay is ownership state, not a stale discovery hint.  It must
;; survive a conflicting live dolt-native record and reach the real peer
;; selectors as a blocking claim.  Both launch paths must also publish that
;; state before their first readiness observation, then either upgrade the
;; exact claim in place or retain it until process death is confirmed.

(defmacro dorg-test-maij2--with-pending-over-live-native
    (dolt-dir canon native-port &rest body)
  "Bind a pending DOrg overlay plus a LIVE different-port native record.
DOLT-DIR and CANON name the isolated store; NATIVE-PORT names a real listener.
The pending overlay points at a different, down port but carries this Emacs's
live pid, so the real classifier returns `pending' if reconciliation preserves
the overlay.  BODY runs with no effective-claim/classifier stubs."
  (declare (indent 3))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "maij2-pending-native"))
            (,canon (dorg-dolt-server--canonical-dir ,dolt-dir))
            (native-listener (dorg-test--listen-port))
            (,native-port (cdr native-listener))
            (dolt-meta (expand-file-name ".dolt" ,canon)))
       (make-directory dolt-meta t)
       (unwind-protect
           (progn
             (dorg-dolt-server--claim-write
              ,dolt-dir
              (list (cons :dolt-dir ,canon)
                    (cons :data-dir ,canon)
                    (cons :db-name "db")
                    (cons :host (system-name))
                    (cons :owner-pid (emacs-pid))
                    (cons :owner-kind 'daemon)
                    (cons :server-pid (emacs-pid))
                    (cons :port 61081)
                    (cons :token "maij2-pending-token")
                    (cons :policy 'server)
                    (cons :protocol dorg-dolt-server-claim-protocol)
                    (cons :state 'probe-inconclusive)
                    (cons :created-at 100.0)
                    (cons :last-used 100.0)))
             (with-temp-file (expand-file-name "sql-server.info" dolt-meta)
               (insert (format "%d:%d:maij2-native-uuid"
                               (emacs-pid) ,native-port)))
             (cl-letf (((symbol-function 'dorg-dolt--ensure-available)
                        (lambda (&rest _) nil))
                       ;; Native listener is live; the pending overlay's port is
                       ;; intentionally down.  No classifier is stubbed.
                       ((symbol-function 'dorg-dolt-server--port-live-p)
                        (lambda (port) (= port ,native-port)))
                       ((symbol-function 'dorg-dolt-server--port-healthy-p)
                        (lambda (&rest _) t))
                       ((symbol-function 'dorg-dolt-server--data-dir-and-db)
                        (lambda (_d) (cons ,canon "db")))
                       ((symbol-function 'dorg-dolt-server--expected-binding)
                        (lambda (_d) nil)))
               ,@body))
         ;; RED assertions can fail while the synthetic claim still names this
         ;; live Emacs pid.  Remove test-owned state explicitly so the outer
         ;; owned-root reaper never mistakes the fixture for a real daemon.
         (dorg-dolt-server--claim-delete-file ,dolt-dir)
         (remhash ,canon dorg-dolt-server--owned-claims)
         (ignore-errors
           (delete-file (expand-file-name "sql-server.info" dolt-meta)))
         (when (process-live-p (car native-listener))
           (delete-process (car native-listener)))))))

(ert-deftest dorg-test-maij2-effective-claim-pending-overlay-beats-live-native ()
  "A pending overlay is authoritative over a live different-port native record.
Non-vacuity: the native listener is genuinely live and the overlay port is down;
the old native-first reconciliation returns owner-kind `dolt' here."
  (dorg-test-maij2--with-pending-over-live-native store canon native-port
    (let ((overlay (dorg-dolt-server-claim store))
          (effective (dorg-dolt-server--effective-claim store)))
      (should (equal effective overlay))
      (should (eq (alist-get :state effective) 'probe-inconclusive))
      (should (eq (dorg-dolt-server--claim-classify effective t) 'pending))
      (should-not (= (alist-get :port effective) native-port)))))

(ert-deftest dorg-test-maij2-attach-only-sees-pending-before-live-native ()
  "The real attach-only selector blocks the pending overlay before attachment."
  (dorg-test-maij2--with-pending-over-live-native store canon native-port
    (let ((probed nil) (started nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (setq probed t) t))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) native-port))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (let ((err (should-error (dorg-dolt-server--attach-only store)
                                 :type 'dorg-sql-transport-error)))
          (should (string-match-p "pending" (error-message-string err))))
        (should-not probed)
        (should-not started)
        (should-not broken)
        (should-not (gethash canon dorg-dolt-server--attached-ports))))))

(ert-deftest dorg-test-maij2-ensure-strict-sees-pending-before-live-native ()
  "The real strict selector blocks the pending overlay before replacement."
  (dorg-test-maij2--with-pending-over-live-native store canon native-port
    (let ((probed nil) (started nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (setq probed t) t))
                ((symbol-function 'dorg-dolt-server--start-unsafe)
                 (lambda (&rest _) (setq started t) native-port))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (let ((err (should-error (dorg-dolt-server--ensure-strict store)
                                 :type 'dorg-sql-transport-error)))
          (should (string-match-p "pending" (error-message-string err))))
        (should-not probed)
        (should-not started)
        (should-not broken)
        (should-not (gethash canon dorg-dolt-server--attached-ports))))))

(ert-deftest dorg-test-maij2-ensure-detached-sees-pending-before-live-native ()
  "The real detached selector blocks the pending overlay before replacement."
  (dorg-test-maij2--with-pending-over-live-native store canon native-port
    (let ((probed nil) (started nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--store-valid-p)
                 (lambda (&rest _) (setq probed t) t))
                ((symbol-function 'dorg-dolt-server--start-detached-unsafe)
                 (lambda (&rest _) (setq started t) native-port))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (let ((err (should-error (dorg-dolt-server--ensure-detached store)
                                 :type 'dorg-sql-transport-error)))
          (should (string-match-p "pending" (error-message-string err))))
        (should-not probed)
        (should-not started)
        (should-not broken)
        (should-not (gethash canon dorg-dolt-server--attached-ports))))))

(defmacro dorg-test-maij2--with-child (dolt-dir canon proc port &rest body)
  "Run BODY around a harmless child fixture for DOLT-DIR on PORT."
  (declare (indent 4))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "maij2-child"))
            (,canon (dorg-dolt-server--canonical-dir ,dolt-dir))
            (child-buffer (generate-new-buffer " *maij2-child*"))
            (,proc (start-process "maij2-child" child-buffer "sleep" "60"))
            (,port 61082)
            (launched-buffer nil))
       (set-process-query-on-exit-flag ,proc nil)
       (unwind-protect
           (cl-letf (((symbol-function 'dorg-dolt--target)
                      (lambda (_d) (list :canonical-repo-dir ,canon
                                         :server-data-dir ,canon
                                         :repo-dir ,canon :db-name "db")))
                     ((symbol-function 'dorg-dolt--target-sql-server-args)
                      (lambda (&rest _) '("sql-server")))
                     ((symbol-function 'dorg-dolt-server--free-port)
                      (lambda () ,port))
                     ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                      (lambda (&rest _) t))
                     ((symbol-function 'start-process)
                      (lambda (_name buffer &rest _)
                        (setq launched-buffer buffer)
                        ,proc)))
             ,@body)
         (when (process-live-p ,proc) (delete-process ,proc))
         (accept-process-output ,proc 0.05)
         (when (buffer-live-p child-buffer) (kill-buffer child-buffer))
         (when (buffer-live-p launched-buffer) (kill-buffer launched-buffer))))))

(ert-deftest dorg-test-maij2-child-publishes-before-ready-and-upgrades-in-place ()
  "Child startup publishes before readiness and finalizes the SAME claim."
  (dorg-test-maij2--with-child store canon child port
    (let ((before nil) (publish-count 0) (finalize-count 0)
          (real-publish (symbol-function 'dorg-dolt-server--publish-claim))
          (real-finalize (symbol-function 'dorg-dolt-server--finalize-pending-claim)))
      (cl-letf (((symbol-function 'dorg-dolt-server--publish-claim)
                 (lambda (&rest args)
                   (cl-incf publish-count)
                   (apply real-publish args)))
                ((symbol-function 'dorg-dolt-server--finalize-pending-claim)
                 (lambda (&rest args)
                   (cl-incf finalize-count)
                   (apply real-finalize args)))
                ((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _)
                   (setq before (copy-tree (dorg-dolt-server-claim store)))
                   nil))
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (_p) (cons 'served '("db")))))
        (should (= (dorg-dolt-server--start-child-unsafe store t) port))
        (let ((after (dorg-dolt-server-claim store)))
          (should (eq (alist-get :state before) 'probe-inconclusive))
          (should (eq (alist-get :state after) 'ready))
          (dolist (key '(:token :created-at :owner-kind :owner-pid
                         :server-pid :port :data-dir :db-name))
            (should (equal (alist-get key before) (alist-get key after))))
          (should (= publish-count 1))
          (should (= finalize-count 1)))))))

(ert-deftest dorg-test-maij2-child-early-death-sentinel-releases-provisional ()
  "The child death sentinel owns provisional claims before readiness begins."
  (dorg-test-maij2--with-child store canon child port
    (let ((seen nil) (after-death :unset))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _)
                   (setq seen (copy-tree (dorg-dolt-server-claim store)))
                   (delete-process child)
                   (accept-process-output child 0.1)
                   (setq after-death (dorg-dolt-server-claim store))
                   (error "injected early child death"))))
        (should-error (dorg-dolt-server--start-child-unsafe store t))
        (should (eq (alist-get :state seen) 'probe-inconclusive))
        (should-not after-death)
        (should-not (gethash canon dorg-dolt-server--procs))))))

(ert-deftest dorg-test-maij2-child-failed-ready-preserves-live-provisional ()
  "A readiness failure preserves the claim when child death is unconfirmed."
  (dorg-test-maij2--with-child store canon child port
    (let ((seen nil) (killed nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _)
                   (setq seen (copy-tree (dorg-dolt-server-claim store)))
                   (error "injected readiness failure")))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (proc &rest _) (setq killed proc) nil))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (should-error (dorg-dolt-server--start-child-unsafe store t))
        (should (eq (alist-get :state seen) 'probe-inconclusive))
        (should (eq killed child))
        (should (process-live-p child))
        (should-not broken)
        (should (eq (alist-get :state (dorg-dolt-server-claim store))
                    'probe-inconclusive))
        (should (eq (car (gethash canon dorg-dolt-server--procs)) child))))))

(defmacro dorg-test-maij2--with-detached (dolt-dir canon port &rest body)
  "Run BODY with a synthetic detached-launch target for DOLT-DIR and PORT."
  (declare (indent 3))
  `(dorg-test--with-dolt-server-runtime
     (let* ((,dolt-dir (dorg-test--repo "maij2-detached"))
            (,canon (dorg-dolt-server--canonical-dir ,dolt-dir))
            (,port 61083))
       (unwind-protect
           (cl-letf (((symbol-function 'dorg-dolt--target)
                      (lambda (_d) (list :canonical-repo-dir ,canon
                                         :server-data-dir ,canon
                                         :repo-dir ,canon :db-name "db")))
                     ((symbol-function 'dorg-dolt--target-sql-server-command)
                      (lambda (&rest _) "dolt sql-server"))
                     ((symbol-function 'dorg-dolt-server--expected-binding)
                      (lambda (_d) nil))
                     ((symbol-function 'dorg-dolt-server--dir-store-valid-p)
                      (lambda (&rest _) t))
                     ((symbol-function 'dorg-dolt-server--free-port)
                      (lambda () ,port))
                     ((symbol-function 'dorg-dolt-server--detached-launch)
                      (lambda (&rest _) (emacs-pid))))
             ,@body)
         (dorg-dolt-server--claim-delete-file ,dolt-dir)
         (remhash ,canon dorg-dolt-server--owned-claims)))))

(ert-deftest dorg-test-maij2-detached-publishes-before-ready-and-upgrades-in-place ()
  "Detached success finalizes the pre-readiness claim, never republishes it."
  (dorg-test-maij2--with-detached store canon port
    (let ((before nil) (publish-count 0) (finalize-count 0)
          (real-publish (symbol-function 'dorg-dolt-server--publish-claim))
          (real-finalize (symbol-function 'dorg-dolt-server--finalize-pending-claim)))
      (cl-letf (((symbol-function 'dorg-dolt-server--publish-claim)
                 (lambda (&rest args)
                   (cl-incf publish-count)
                   (apply real-publish args)))
                ((symbol-function 'dorg-dolt-server--finalize-pending-claim)
                 (lambda (&rest args)
                   (cl-incf finalize-count)
                   (apply real-finalize args)))
                ((symbol-function 'dorg-dolt-server--pid-live-p)
                 (lambda (_pid) t))
                ((symbol-function 'dorg-dolt-server--port-live-p)
                 (lambda (_port)
                   (setq before (or before
                                    (copy-tree (dorg-dolt-server-claim store))))
                   t))
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (_p) (cons 'served '("db"))))
                ((symbol-function 'dorg-dolt-server--native-uuid-for)
                 (lambda (&rest _) "maij2-ready-uuid")))
        (should (= (dorg-dolt-server--start-detached-unsafe store) port))
        (let ((after (dorg-dolt-server-claim store)))
          (should (eq (alist-get :state before) 'probe-inconclusive))
          (should-not (alist-get :uuid before))
          (should (eq (alist-get :state after) 'ready))
          (should (equal (alist-get :uuid after) "maij2-ready-uuid"))
          (dolist (key '(:token :created-at :owner-kind :owner-pid
                         :server-pid :port :data-dir :db-name))
            (should (equal (alist-get key before) (alist-get key after))))
          (should (= publish-count 1))
          (should (= finalize-count 1)))))))

(ert-deftest dorg-test-maij2-detached-timeout-preserves-unconfirmed-owner ()
  "Timeout keeps the provisional claim when raw-pid death is unconfirmed."
  (dorg-test-maij2--with-detached store canon port
    (let ((dorg-dolt-server-startup-timeout 0)
          (killed nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--kill-pid)
                 (lambda (pid &rest _) (setq killed pid) nil))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (should-error (dorg-dolt-server--start-detached-unsafe store)
                      :type 'dorg-sql-transport-error)
        (should (= killed (emacs-pid)))
        (should-not broken)
        (let ((claim (dorg-dolt-server-claim store)))
          (should (eq (alist-get :state claim) 'probe-inconclusive))
          (should (= (alist-get :server-pid claim) (emacs-pid)))
          (should (= (alist-get :port claim) port)))))))

(ert-deftest dorg-test-maij2-detached-wrong-store-preserves-unconfirmed-owner ()
  "Confirmed wrong store still preserves a claim when death is unconfirmed."
  (dorg-test-maij2--with-detached store canon port
    (let ((killed nil) (broken nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                 (lambda (_pid) t))
                ((symbol-function 'dorg-dolt-server--port-live-p)
                 (lambda (_port) t))
                ((symbol-function 'dorg-dolt-server--probe-served-dbs)
                 (lambda (_p) (cons 'served '("other"))))
                ((symbol-function 'dorg-dolt-server--kill-pid)
                 (lambda (pid &rest _) (setq killed pid) nil))
                ((symbol-function 'dorg-dolt-server--break-claim)
                 (lambda (&rest _) (setq broken t))))
        (should-error (dorg-dolt-server--start-detached-unsafe store)
                      :type 'dorg-sql-transport-error)
        (should (= killed (emacs-pid)))
        (should-not broken)
        (should (eq (alist-get :state (dorg-dolt-server-claim store))
                    'probe-inconclusive))))))

(provide 'dorg-dolt-server-tests)
;;; dorg-dolt-server-tests.el ends here
