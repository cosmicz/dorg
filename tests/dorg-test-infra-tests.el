;;; dorg-test-infra-tests.el --- Contract tests for server-required test infra -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Contract tests for the server-required test infrastructure (tracked-case,
;; server-only-transport epic tracked-case).  They pin the guarantee that the
;; test harness acquires and drives Dolt stores WITHOUT depending on the
;; public `auto'/`cli' values of `dorg-sql-transport' that tracked-case deletes:
;;
;;   AC1  The shared multi-DB test server round-trips under strict `server'
;;        transport alone — no `auto'/`cli' in the path.
;;   AC2  `dorg-sql--with-cli-seam' forces one-shot CLI for a standalone,
;;        server-less store even when `dorg-sql-transport' would otherwise
;;        start/attach a server — proving the seam bypasses policy.
;;   AC3  The seam IS exactly the `dorg-sql--pinned-transport' short-circuit
;;        (load-bearing / non-vacuity guard).
;;
;; See the tracked-case Design "Delete-scope contract" for what these lock in.

;;; Code:

(require 'ert)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-test-helpers)

(defconst dorg-test-infra--insert
  (concat "INSERT INTO headings"
          " (id, root_id, parent_id, sort_key, level_offset, commented, title, post_blank)"
          " VALUES (%d, %d, NULL, X'01', 0, FALSE, '%s', 0)")
  "Template INSERT of a single level-0 root heading with a title.")

(defmacro dorg-test-infra--with-standalone (dolt-var &rest body)
  "Bind DOLT-VAR to a fresh STANDALONE copy-repo store, no server holds it.
Distinct from `dorg-test--with-temp-db': this never routes through the
shared server, so the store is a genuine server-less Dolt repo — the
substrate the CLI seam exists for."
  (declare (indent 1) (debug (symbolp body)))
  (let ((temp (make-symbol "temp")))
    `(let ((,temp (make-temp-file "dorg-infra-" t)))
       (unwind-protect
           (let ((,dolt-var (dorg-test--copy-repo ,temp)))
             ,@body)
         (delete-directory ,temp t)))))

(defun dorg-test-infra--install-distinct-directory (path)
  "Replace the directory at PATH with a preallocated distinct object.
Move the original aside under a non-discoverable sibling name, then rename a
replacement allocated while the original still existed onto PATH.  Return a
plist containing both observed identities and the preserved-original path.
Callers assert the identities differ before exercising the real cleanup guard;
this helper never retries allocation and never stubs the guard."
  (let* ((dfn (directory-file-name path))
         (parent (file-name-directory dfn))
         (original-id (dorg-test--path-file-identity dfn))
         (replacement
          (make-temp-file (expand-file-name "identity-replacement-" parent) t))
         (replacement-id (dorg-test--path-file-identity replacement))
         (preserved
          (make-temp-name (expand-file-name "identity-original-" parent))))
    (rename-file dfn preserved)
    (rename-file replacement dfn)
    (list :original-id original-id
          :replacement-id replacement-id
          :preserved preserved)))

;;; AC1 — shared-server path is transport-policy-independent

(ert-deftest dorg-test-infra-shared-server-needs-no-cli-auto ()
  "The shared test server round-trips a write+read under strict `server' only.
Proves the bulk test path (`dorg-test--with-temp-db') needs neither `auto'
nor `cli' — it is already server-required."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let ((dorg-sql-transport 'server))
      (dorg-sql-batch dolt-dir
                      (list (format dorg-test-infra--insert 7001 7001 "infra-ac1"))
                      "ac1")
      (let ((rows (dorg-sql dolt-dir
                            "SELECT title FROM headings WHERE id = 7001")))
        (should (equal "infra-ac1" (alist-get 'title (car rows))))))))

;;; AC2 — the CLI seam bypasses policy on a server-less standalone store

(ert-deftest dorg-test-infra-cli-seam-bypasses-policy ()
  "`dorg-sql--with-cli-seam' drives a server-less store via one-shot CLI
even under `dorg-sql-transport' = `server' — WITHOUT starting a server.
The write+read round-trips, and no managed server is left owning the store,
which is the observable signature of the pin having short-circuited the
strict-`server' branch that would otherwise start one."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-infra--with-standalone dolt-dir
    (let ((dorg-sql-transport 'server))   ; would START a server without the seam
      (dorg-sql--with-cli-seam dolt-dir
        (dorg-sql-batch dolt-dir
                        (list (format dorg-test-infra--insert 7002 7002 "infra-ac2"))
                        "ac2")
        (let ((rows (dorg-sql dolt-dir
                              "SELECT title FROM headings WHERE id = 7002")))
          (should (equal "infra-ac2" (alist-get 'title (car rows))))))
      ;; Load-bearing: the seam bypassed the `server' branch, so this process
      ;; started NO server for the store.  If the seam were a no-op, strict
      ;; `server' would have started one and this port would be non-nil.
      (should (null (dorg-dolt-server-port dolt-dir))))))

;;; AC3 — the seam IS the pinned-transport short-circuit (non-vacuity)

(ert-deftest dorg-test-infra-cli-seam-is-the-pin ()
  "`dorg-sql--with-cli-seam' installs exactly the `(cli . nil)' transport pin
and disarms the CLI conflict check for its dir, and only for its dynamic
extent.  This is the mechanism tracked-case must preserve after deleting the
public `auto'/`cli' policy."
  (skip-unless dorg-test--dolt-available)
  (dorg-test-infra--with-standalone dolt-dir
    ;; Outside the seam: no implicit pin.
    (should (null (dorg-sql--pinned-transport-for dolt-dir)))
    (dorg-sql--with-cli-seam dolt-dir
      ;; Inside: the dir resolves to one-shot CLI via the pin, and the
      ;; standalone-store conflict check is disarmed.
      (should (equal '(cli . nil) (dorg-sql--pinned-transport-for dolt-dir)))
      (should (equal '(cli . nil) (dorg-sql--resolve-transport dolt-dir)))
      (should (null dorg-sql--cli-conflict-check)))
    ;; The pin unwinds with the macro.
    (should (null (dorg-sql--pinned-transport-for dolt-dir)))))

;;; AC4 — the harness FALLBACK path actually applies the seam

(ert-deftest dorg-test-infra-fallback-applies-cli-seam ()
  "The copy-repo fallback of `dorg-test--with-temp-db' pins its body to the
CLI seam, so a standalone store round-trips a write+read under strict
`server' policy WITHOUT starting a server.  This is the server-only-world
guarantee for tests that force a standalone store: their SQL routes through
the retained one-shot CLI even after fwb3z deletes implicit `auto'/`cli'.

Uses the REAL helper macro (not a hand-rolled fixture), and asserts a
non-nil `(cli . nil)' pin is live while the body executes — the exact gap
the REVIEW reject flagged.  `server' is bound INSIDE the body so the pin,
not policy, is what routes; the store is never server-backed."
  (skip-unless dorg-test--dolt-available)
  (let ((dorg-test--server-port nil))          ; force the copy-repo fallback
    (dorg-test--with-temp-db dolt-dir
      ;; The fallback wrapped this body in `dorg-sql--with-cli-seam': the pin
      ;; is live even though nothing here set it directly.
      (should (equal '(cli . nil) (dorg-sql--pinned-transport-for dolt-dir)))
      (let ((dorg-sql-transport 'server))      ; would need a server without the pin
        (dorg-sql-batch dolt-dir
                        (list (format dorg-test-infra--insert 7003 7003 "infra-fallback"))
                        "fb")
        (let ((rows (dorg-sql dolt-dir
                              "SELECT title FROM headings WHERE id = 7003")))
          (should (equal "infra-fallback" (alist-get 'title (car rows)))))
        ;; Load-bearing: routed via the seam pin, so no server was started.
        (should (null (dorg-dolt-server-port dolt-dir)))))))

(ert-deftest dorg-test-infra-fallback-and-dir-applies-cli-seam ()
  "As `dorg-test-infra-fallback-applies-cli-seam', for the two-dir variant
`dorg-test--with-temp-db-and-dir' — its copy-repo fallback body is seamed too."
  (skip-unless dorg-test--dolt-available)
  (let ((dorg-test--server-port nil))
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (ignore temp-base)
      (should (equal '(cli . nil) (dorg-sql--pinned-transport-for dolt-dir)))
      (let ((dorg-sql-transport 'server))
        (dorg-sql-batch dolt-dir
                        (list (format dorg-test-infra--insert 7004 7004 "infra-fallback-2"))
                        "fb2")
        (should (equal "infra-fallback-2"
                       (alist-get 'title
                                  (car (dorg-sql dolt-dir
                                                 "SELECT title FROM headings WHERE id = 7004")))))
        (should (null (dorg-dolt-server-port dolt-dir)))))))

(ert-deftest dorg-test-infra-fallback-seam-opt-out-leaves-no-pin ()
  "With `dorg-test--fallback-cli-seam' nil, the copy-repo fallback leaves its
body UNpinned — the escape hatch the server-lifecycle/policy tests bind so
the seam does not mask the transport under test.  This proves the default
seam wrap is load-bearing (the previous tests are not vacuous): flipping the
flag observably removes the pin the harness otherwise installs."
  (skip-unless dorg-test--dolt-available)
  (let ((dorg-test--server-port nil)
        (dorg-test--fallback-cli-seam nil))
    (dorg-test--with-temp-db dolt-dir
      (should (null (dorg-sql--pinned-transport-for dolt-dir))))))

;;; Batch runner cleanup contract (tracked-case zero-fatal-hook design)
;;
;; The runner's normal-context paths carry ALL cleanup responsibility:
;; stale-registry reaping BEFORE the shared server spawns, and a complete
;; unwind sequence (stop shared server, reap owned roots, stop-all managed
;; servers, template cleanup) on success, ERT abort, and the fast/no-server
;; path alike.  The old abnormal-exit hook test is gone with the hook:
;; `dorg-test-batch-kill-emacs-hook-free-of-dorg-cleanup' pins its absence.

(defmacro dorg-test-infra--with-runner-events (events &rest body)
  "Run BODY with the runner collaborators mocked into EVENTS collectors.
Mocks cover the zero-fatal-hook runner contract: `reap-registry',
`reap-roots', and `stop-all' events in addition to the original set.

The shared-server invariant guard (tracked-case) samples the REAL
`dorg-test--shared-server-pid' and `dorg-dolt-fixture-inventory'.  To make
the start path REPRESENTATIVE (an armed guard that genuinely runs its
liveness + cold-start comparison, not a vacuous one), the harness binds
`dorg-test--server-proc' to a REAL OS subprocess sentinel
\(`start-process ... \"sleep\" \"300\"' — the idiom already used at
dorg-dolt-server-tests.el:1008/1074/1087), so `dorg-test--shared-server-pid'
returns a stable INTEGER pid.  A network listener is NOT usable: `process-id'
on a server network process is nil, which would make the pid clause vacuous
\(the V2 REJECT).  `dorg-dolt-fixture-inventory' is stubbed to a stable
`(:servers 0 :stores 0 :cold-starts 1)' baseline (the guard reads only
`:cold-starts'; `:servers'/`:stores' stay 0 so the teardown's `inventory-zero'
contract still passes and the 8 pre-existing runner-control tests stay green).
The sentinel is deleted in the unwind with a guarded live-check so it is
idempotent when a test already `delete-process'd it to drive the
killed-sentinel violation; a test may also override the inventory stub to bump
`:cold-starts'."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((dorg-test--server-proc
          (start-process "dorg-test-shared-sentinel" nil "sleep" "300")))
     (unwind-protect
         (cl-letf (((symbol-function 'dorg-setup-test-environment)
                    (lambda () (push 'setup ,events)))
                   ((symbol-function 'dorg-run-tests--maybe-verbose)
                    (lambda () (push 'verbose ,events)))
                   ((symbol-function 'dorg-test--reap-stale-server-registry)
                    (lambda () (push 'reap-registry ,events) nil))
                   ((symbol-function 'dorg-test--start-shared-server)
                    (lambda () (push 'start ,events) 12345))
                   ((symbol-function 'dorg-dolt-fixture-inventory)
                    (lambda () '(:servers 0 :stores 0 :cold-starts 1)))
                   ((symbol-function 'ert-run-tests-batch-and-exit)
                    (lambda (&rest _) (push 'old-exit ,events)
                      (error "old ERT exit path used")))
                   ((symbol-function 'dorg-test--stop-shared-server)
                    (lambda () (push 'stop ,events)))
                   ((symbol-function 'dorg-sql--close-all-connections)
                    (lambda () (push 'close-connections ,events)))
                   ((symbol-function 'dorg-test--reap-owned-roots)
                    (lambda () (push 'reap-roots ,events) nil))
                   ((symbol-function 'dorg-dolt-server-stop-all)
                    (lambda () (push 'stop-all ,events)))
                   ((symbol-function 'dorg-test--cleanup-template)
                    (lambda () (push 'template ,events)))
                   ((symbol-function 'kill-emacs)
                    (lambda (status) (push (list 'exit status) ,events)
                      (throw 'dorg-test-infra-exit status))))
           ,@body)
       (when (process-live-p dorg-test--server-proc)
         (delete-process dorg-test--server-proc)))))

(ert-deftest dorg-test-infra-batch-cleanup-runs-before-exit ()
  "Server-gate run: stale-registry reap BEFORE spawn; full unwind cleanup.
RED until tracked-case IMPL adds reap-registry and explicit stop-all to the
runner."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_selector) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_stats) 0)))
        (should (= 0 (catch 'dorg-test-infra-exit
                       (dorg-run-tests--batch "selector" t))))
        (should (equal (nreverse events)
                       '(setup verbose reap-registry start ert
                         close-connections stop reap-roots stop-all
                         template (exit 0))))))))

(ert-deftest dorg-test-infra-batch-cleanup-ert-abort-exits-nonzero ()
  "Full unwind cleanup still runs when ERT itself aborts outside a test.
RED until tracked-case IMPL."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_selector) (push 'ert events) (error "boom")))
                ((symbol-function 'backtrace)
                 (lambda (&rest _) (push 'backtrace events))))
        (should (= 2 (catch 'dorg-test-infra-exit
                       (dorg-run-tests--batch "selector" t))))
        (should (equal (nreverse events)
                       '(setup verbose reap-registry start ert backtrace
                         close-connections stop reap-roots stop-all
                         template (exit 2))))))))

(ert-deftest dorg-test-9w3c4-runner-close-failure-continues-teardown ()
  "A pooled-connection close failure is visible but skips no later cleanup.
Both `error' and `quit' force a nonzero exit.  The close is attempted after
ERT and before the shared-server stop; drain, inventory, owned-root reap,
stop-all, and template cleanup still run.  This fails before tracked-case
because the runner has no independent pooled-close step."
  (dolist (condition '(error quit))
    (let (events)
      (dorg-test-infra--with-runner-events events
        (cl-letf (((symbol-function 'ert-run-tests-batch)
                   (lambda (_selector) (push 'ert events) 'stats))
                  ((symbol-function 'ert-stats-completed-unexpected)
                   (lambda (_stats) 0))
                  ((symbol-function 'dorg-sql--close-all-connections)
                   (lambda ()
                     (push 'close-connections events)
                     (if (eq condition 'error)
                         (error "spec: pooled close failed")
                       (signal 'quit nil))))
                  ((symbol-function 'dorg-dolt-fixture-drain-failed-start)
                   (lambda () (push 'drain events) nil))
                  ((symbol-function 'dorg-dolt-fixture-inventory)
                   (lambda ()
                     (push 'inventory events)
                     '(:servers 0 :stores 0 :cold-starts 1))))
          (should (= 1 (catch 'dorg-test-infra-exit
                         (dorg-run-tests--batch "selector" t))))
          (let ((ordered (nreverse events)))
            ;; Inventory is sampled by the armed runner guard before teardown
            ;; as well as by the teardown itself.  Pin the ordered UNIQUE seams
            ;; and require a later inventory sample instead of mistaking those
            ;; guard reads for cleanup events.
            (let ((ert-pos (seq-position ordered 'ert))
                  (close-pos (seq-position ordered 'close-connections))
                  (stop-pos (seq-position ordered 'stop))
                  (drain-pos (seq-position ordered 'drain))
                  (reap-pos (seq-position ordered 'reap-roots))
                  (stop-all-pos (seq-position ordered 'stop-all))
                  (template-pos (seq-position ordered 'template)))
              (should (< ert-pos close-pos stop-pos drain-pos
                         reap-pos stop-all-pos template-pos))
              (should (seq-position (nthcdr (1+ drain-pos) ordered)
                                    'inventory)))))))))

(ert-deftest dorg-test-infra-batch-cleanup-fast-gate-skips-server ()
  "Fast gate: no server start/stop — but stale-registry reaping still runs
\(bounded next-start recovery is owed at EVERY batch startup, whichever
gate runs next — OC audit 2026-07-16 22:16), and so does the rest of the
unwind cleanup (owned roots, stop-all, template).
RED until tracked-case IMPL."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_selector) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_stats) 1)))
        (should (= 1 (catch 'dorg-test-infra-exit
                       (dorg-run-tests--batch "selector" nil))))
        (should (equal (nreverse events)
                       '(setup verbose reap-registry ert
                         reap-roots stop-all template (exit 1))))))))

;; Blocker C (OC 02:44): a store DROP failure during teardown must force a
;; nonzero exit even when the shared close masks the leak to a zero inventory.
(ert-deftest dorg-test-infra-runner-forces-nonzero-on-store-drop-failure ()
  "Runner teardown independence + no-masking: with TWO outstanding stores where
ONE DROP fails and one succeeds, the runner still closes the shared server
\(the `stop' event) and runs the later crash-net/stop-all/template cleanup, and
forces a NONZERO exit — EVEN THOUGH the shared close forgets the leaked store
so the final inventory reads zero.  Non-vacuous against a runner that lets
`dorg-dolt-fixture-release-stores' swallow the DROP failure (exit 0) or that
infers success from the masked zero inventory.
The `release-stores' call is REAL here (two live leases, one DROP injected to
fail) so the structured-failure return is what surfaces the leak."
  (let (events
        (dorg-dolt-fixture--store-leases (make-hash-table :test 'equal))
        (dorg-dolt-fixture--conn 'dorg-test-fixture--fake-conn))
    (puthash "/tmp/dorg-drop-a/"
             (dorg-dolt-fixture--make-lease
              :kind 'store :dolt-dir "/tmp/dorg-drop-a/" :data-dir "/tmp/" :db-name "A")
             dorg-dolt-fixture--store-leases)
    (puthash "/tmp/dorg-drop-b/"
             (dorg-dolt-fixture--make-lease
              :kind 'store :dolt-dir "/tmp/dorg-drop-b/" :data-dir "/tmp/" :db-name "B")
             dorg-dolt-fixture--store-leases)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_selector) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_stats) 0))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql &rest _)
                   (when (string-match-p "DROP DATABASE IF EXISTS A" sql)
                     (error "spec: DROP A wedged"))
                   nil))
                ;; The shared close masks the leaked store: inventory reads zero.
                ((symbol-function 'dorg-dolt-fixture-inventory)
                 (lambda () '(:servers 0 :stores 0 :cold-starts 1))))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          ;; Nonzero despite the masked (zero) inventory.
          (should (= 1 status)))
        (let ((ev (nreverse events)))
          ;; The shared close AND all later cleanup still ran.
          (should (memq 'stop ev))
          (should (memq 'reap-roots ev))
          (should (memq 'stop-all ev))
          (should (memq 'template ev))
          (should (member '(exit 1) ev)))))))

;; OC 03:41 (9) + 03:36 (7): unreaped orphans + a shared start requested =>
;; numeric nonzero WITHOUT spawning a server or running ERT; cleanup still runs.
(ert-deftest dorg-test-infra-runner-stale-orphans-block-start-and-tests ()
  "With unreaped dead-owner orphans AND a shared start requested, the runner
does NOT spawn a shared server and does NOT run ERT — it returns a numeric
nonzero exit — then still runs the unwind cleanup.  Non-vacuous against a
runner that starts/tests over accumulating orphans, or returns a non-numeric
\(message-string) exit."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'dorg-test--reap-stale-server-registry)
                 (lambda () (push 'reap-registry events) '((f . server-unconfirmed))))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (integerp status))
          (should (= 1 status)))
        (let ((ev (nreverse events)))
          (should-not (memq 'start ev))    ; no fresh server spawned
          (should-not (memq 'ert ev))      ; no tests over a corrupt baseline
          ;; Unwind cleanup still runs.
          (should (memq 'reap-roots ev))
          (should (memq 'stop-all ev))
          (should (memq 'template ev)))))))

;; OC 03:41 (9): the healthy path calls reap -> start -> ERT and returns a
;; numeric 0 (no orphans, start-server).
(ert-deftest dorg-test-infra-runner-healthy-order-reap-start-ert ()
  "No orphans + a shared start requested: the runner reaps, THEN starts, THEN
runs ERT, and returns a NUMERIC 0 on a clean suite.  Non-vacuous against a
control flow that skips ERT after starting, or returns non-numeric."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_s) 0)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (integerp status))
          (should (= 0 status)))
        (let ((ev (nreverse events)))
          ;; Strict reap -> start -> ERT order.
          (should (equal (seq-filter (lambda (e) (memq e '(reap-registry start ert))) ev)
                         '(reap-registry start ert))))))))

;; OC 03:41 (10): a SIGNALLED start (infra failure, not the nil fallback)
;; propagates to the outer error path — ERT is never run, exit is nonzero.
(ert-deftest dorg-test-infra-runner-start-signal-propagates-no-tests ()
  "A SIGNALLED shared-server start (infrastructure failure, distinct from the
nil unsupported/mysql-absent fallback) propagates to the outer error path: ERT
is NEVER run, the exit is nonzero (2), and the unwind cleanup still runs.
Non-vacuous against a runner that catches the signal and continues tests as if
healthy (a green CLI fallback over a possibly-retained live partial)."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'dorg-test--start-shared-server)
                 (lambda () (push 'start events) (error "spec: infra start failed")))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats))
                ((symbol-function 'backtrace)
                 (lambda (&rest _) (push 'backtrace events))))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (= 2 status)))
        (let ((ev (nreverse events)))
          (should (memq 'start ev))
          (should-not (memq 'ert ev))      ; tests NOT run on infra failure
          (should (memq 'reap-roots ev))   ; cleanup still runs
          (should (memq 'template ev)))))))

;; OC 04:12 (1): the runner teardown DRIVES the failed-start drain and forces
;; nonzero when a partial cannot be reclaimed.
(ert-deftest dorg-test-infra-runner-drains-failed-start-and-forces-nonzero ()
  "The runner teardown DRIVES `dorg-dolt-fixture-drain-failed-start': a partial
that cannot be reclaimed (drain returns a failure) forces a nonzero exit while
the later crash-net/stop-all/template cleanup still runs.  Non-vacuous against
a runner that never drains the failed-start sentinel (relying on next-start
reaping, too late for the zero-after-unwind objective)."
  (let (events)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected) (lambda (_s) 0))
                ((symbol-function 'dorg-dolt-fixture-drain-failed-start)
                 (lambda () (push 'drain events) (cons 'failed-start-still-live 'p))))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (= 1 status)))
        (let ((ev (nreverse events)))
          (should (memq 'drain ev))
          (should (memq 'reap-roots ev))
          (should (memq 'stop-all ev))
          (should (memq 'template ev)))))))

;;; Shared-server invariant guard (tracked-case)
;;
;; A lifecycle/stop test that ran the global `dorg-dolt-server-stop-all'
;; against the shared fixture server killed it and cascaded ~360 no-usable-
;; server failures across the full suite.  The runner now guards the shared
;; server across ERT: `dorg-run-tests--shared-invariant-violation-p' is the
;; extracted, pure decision the runner folds into its exit status, converting
;; that silent cascade into an immediate, attributable gate failure.
;;
;; The matrix below calls the predicate DIRECTLY (non-vacuous: each
;; `should'/`should-not' fails if the corresponding clause is deleted); the
;; runner-level transformation tests then drive `dorg-run-tests--batch' through
;; the real-subprocess-sentinel harness end to end.

(ert-deftest dorg-test-infra-shared-invariant-not-armed-never-violates ()
  "Blocker-1 fallback: with ARMED nil the predicate is nil for ANY end state —
even a dead end-pid AND a bumped cold-start count.  Non-vacuous against a guard
that fires on the raw end state without first checking ARMED (the legitimate
nil-start CLI fallback must never false-fail)."
  (should-not (dorg-run-tests--shared-invariant-violation-p nil 4242 1 nil 2)))

(ert-deftest dorg-test-infra-shared-invariant-intact-is-clean ()
  "Armed + unchanged pid + equal cold-start count => no violation (the healthy
path across a clean run)."
  (should-not (dorg-run-tests--shared-invariant-violation-p t 4242 1 4242 1)))

(ert-deftest dorg-test-infra-shared-invariant-killed-pid-violates ()
  "Armed + baseline pid present + dead (nil) end-pid => violation: the shared
proc was killed (e.g. a stop-all against the global --procs)."
  (should (dorg-run-tests--shared-invariant-violation-p t 4242 1 nil 1)))

(ert-deftest dorg-test-infra-shared-invariant-replaced-pid-violates ()
  "Armed + baseline pid P + different end-pid Q => violation: the shared proc
was replaced by a different process."
  (should (dorg-run-tests--shared-invariant-violation-p t 4242 1 9999 1)))

(ert-deftest dorg-test-infra-shared-invariant-cold-start-bump-violates ()
  "Armed + pid unavailable at baseline + cold-starts 1->2 => violation: a
replace-with-cold-started-store is caught by the cold-start clause independently
of the pid clause."
  (should (dorg-run-tests--shared-invariant-violation-p t nil 1 nil 2)))

(ert-deftest dorg-test-infra-shared-invariant-equal-cold-no-pid-clean ()
  "Armed, no pid signal (baseline/end pid both nil), EQUAL cold-start count =>
no violation.  Non-vacuous against a cold-start clause using `/=' or `>='
instead of strict `>' (which would false-fire on an unchanged count)."
  (should-not (dorg-run-tests--shared-invariant-violation-p t nil 2 nil 2)))

;; Runner-level transformation coverage — drives `dorg-run-tests--batch' through
;; the real-subprocess-sentinel harness so `dorg-test--shared-server-pid'
;; returns a stable INTEGER pid (a network listener's `process-id' is nil — the
;; V2 REJECT).  Each test sets up the WRONG end-state and asserts the runner's
;; exit-status transformation (before != after).

(ert-deftest dorg-test-infra-runner-intact-shared-server-exit-zero ()
  "Intact shared server across a clean ERT run => exit 0 (the invariant clause
never trips) and NO INVARIANT-VIOLATED message.  Non-vacuous against a guard
that false-fires on the healthy path."
  (let (events messages)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format-message fmt args) messages) nil))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_s) 0)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (integerp (dorg-test--shared-server-pid))) ; sentinel intact
          (should (= 0 status))
          (should-not (seq-some (lambda (m) (string-match-p "INVARIANT VIOLATED" m))
                                messages)))))))

(ert-deftest dorg-test-infra-runner-killed-shared-server-forces-nonzero ()
  "A test in the block killed the shared server: the sentinel is SYNCHRONOUSLY
`delete-process'd inside the mocked ERT run so `process-live-p' is nil the
instant the mock returns (async `kill-process' can still read live — the V3
REJECT).  The test EXPLICITLY asserts an INTEGER baseline pid and a nil end pid
BEFORE asserting exit 1 + the INVARIANT-VIOLATED message; teardown still runs.
This is the transformation the V2 network-listener sentinel could not produce
\(nil pid at baseline made the pid clause vacuous)."
  (let (events messages baseline-pid)
    (dorg-test-infra--with-runner-events events
      (setq baseline-pid (process-id dorg-test--server-proc))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format-message fmt args) messages) nil))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s)
                   (push 'ert events)
                   ;; SYNCHRONOUS kill: process-live-p is nil once this returns.
                   (delete-process dorg-test--server-proc)
                   'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_s) 0)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          ;; Transformation preconditions: integer baseline pid -> nil end pid.
          (should (integerp baseline-pid))
          (should-not (dorg-test--shared-server-pid))
          ;; ...therefore the run is forced nonzero with the loud message.
          (should (= 1 status))
          (should (seq-some (lambda (m) (string-match-p "INVARIANT VIOLATED" m))
                            messages))
          ;; Teardown still ran (stop shared server, stop-all, template).
          (should (memq 'stop events))
          (should (memq 'stop-all events))
          (should (memq 'template events)))))))

(ert-deftest dorg-test-infra-runner-cold-start-bump-forces-nonzero ()
  "The shared server was replaced with a cold-started store during ERT: the
sentinel pid is INTACT but the fixture cold-start count advanced 1->2.  The
cold-start clause forces exit 1 independently of the pid clause.  Non-vacuous
against a guard that only checks pid liveness."
  (let (events (ran nil))
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'dorg-dolt-fixture-inventory)
                 ;; :servers/:stores 0 so the teardown `inventory-zero' contract
                 ;; passes — the forced exit 1 is then attributable PURELY to the
                 ;; cold-start clause, not a masked teardown failure.
                 (lambda () (list :servers 0 :stores 0
                                  :cold-starts (if ran 2 1))))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) (setq ran t) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_s) 0)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          ;; Pid intact => the pid clause is NOT the cause; the cold bump is.
          (should (integerp (dorg-test--shared-server-pid)))
          (should (= 1 status)))))))

(ert-deftest dorg-test-infra-runner-nil-start-fallback-disarms-guard ()
  "start-server=t but `dorg-test--start-shared-server' returns nil (the
supported mysql-absent CLI fallback): the guard is DISARMED (armed nil), ERT
still runs, and exit is driven ONLY by ERT status (0 on a clean suite) — no
false INVARIANT-VIOLATED even though there is no live shared proc to sample.
Non-vacuous against a guard that arms on start-server alone."
  (let (events messages)
    (dorg-test-infra--with-runner-events events
      (cl-letf (((symbol-function 'dorg-test--start-shared-server)
                 (lambda () (push 'start events) nil)) ; nil fallback
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format-message fmt args) messages) nil))
                ((symbol-function 'ert-run-tests-batch)
                 (lambda (_s) (push 'ert events) 'stats))
                ((symbol-function 'ert-stats-completed-unexpected)
                 (lambda (_s) 0)))
        (let ((status (catch 'dorg-test-infra-exit
                        (dorg-run-tests--batch "selector" t))))
          (should (memq 'ert events))   ; ERT ran despite the nil start
          (should (= 0 status))
          (should-not (seq-some (lambda (m) (string-match-p "INVARIANT VIOLATED" m))
                                messages)))))))

;;; Fatal-signal cleanup contract (tracked-case)
;;
;; SIGTERM can interrupt Emacs 30 inside `garbage_collect'; the handler runs
;; `kill-emacs-hook' Lisp while gc_in_progress is still true, and eval.c's
;; signal_or_quit calls emacs_abort() BEFORE condition handlers when
;; gc_in_progress — so ANY error signaled by ANY kill-emacs-hook function
;; there turns an ordinary SIGTERM into SIGABRT (incident F2C649B7).
;; `ignore-errors' cannot catch it.  The contract pinned here: the batch
;; test process runs ZERO DOrg/test cleanup Lisp at fatal shutdown —
;; nothing of ours on `kill-emacs-hook' at all; all cleanup lives in
;; normal-context unwind paths plus durable ownership records persisted
;; BEFORE server start and reaped at next test startup.
;;
;; The subprocess reproducer is OPERATOR-GATED (DORG_TEST_SIGTERM_GC_PROOF=1,
;; default-SKIP): while the fix is absent it necessarily dies by SIGABRT, so
;; it must never execute from routine focused/full selectors.

(defconst dorg-test-infra--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute path to the repository root (parent of tests/).")

(ert-deftest dorg-test-sigterm-during-gc-dies-by-sigterm ()
  "SIGTERM during GC in a batch test Emacs preserves original TERM semantics.
ONE bounded child (`--batch -Q') requires dorg-test-helpers, poisons
`dorg-test--owned-roots' with a non-string key (the malformed-data case of
AC#3), builds a multi-million-cons live heap, then loops `garbage-collect'
so gc_in_progress dominates wall time.  The parent SIGTERMs it mid-loop.
The child must die BY SIGTERM (wait status: signal 15, the incident's
healthy \"Terminated: 15\" semantics) — not the SIGABRT/emacs_abort crash
path, and with no wrong-type-argument or Fatal error output.
RED before tracked-case IMPL: the heavy cleanup hook reaches
`expand-file-name' on the poisoned key under gc_in_progress and aborts.
DEFAULT-SKIP (OC directive 2026-07-16): pre-fix this child necessarily
dies by SIGABRT, so the test must not execute in ANY routine selector,
focused or full, until the implementation lands.  It runs only when an
operator explicitly sets DORG_TEST_SIGTERM_GC_PROOF=1 for the one bounded
post-fix proof.  Pre-fix RED evidence is preserved durably: bead note
2026-07-16 20:27 (child status=signal code=6, \"abort trap: 6\") and
macOS incident F2C649B7-AC1A-4292-814C-6A65A705B897."
  (skip-unless (equal (getenv "DORG_TEST_SIGTERM_GC_PROOF") "1"))
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (expr '(progn
                  (require 'dorg-test-helpers)
                  ;; AC#3: malformed owned-root datum.
                  (puthash 42 t dorg-test--owned-roots)
                  ;; Live heap so each collection is long (~3M conses).
                  (defvar dorg-test-child--heap nil)
                  (dotimes (i 60)
                    (push (make-list 50000 i) dorg-test-child--heap))
                  ;; stderr is unbuffered in batch; stdout may be pipe-buffered.
                  (message "READY")
                  ;; Bounded: parent TERMs within seconds; this is a backstop.
                  (dotimes (_ 2000) (garbage-collect))
                  ;; TERM never arrived — distinctive failure exit.
                  (kill-emacs 99)))
         (buf (generate-new-buffer " *dorg-sigterm-gc-child*"))
         (proc (start-process "dorg-sigterm-gc-child" buf emacs
                              "--batch" "-Q"
                              "-L" dorg-test-infra--root
                              "-L" (expand-file-name "tests" dorg-test-infra--root)
                              "--eval" (prin1-to-string expr))))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc nil)
          (let ((deadline (+ (float-time) 15)))
            (while (and (< (float-time) deadline)
                        (not (with-current-buffer buf
                               (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "READY" nil t)))))
              (accept-process-output proc 0.1)))
          (should (with-current-buffer buf
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "READY" nil t))))
          ;; Let the child settle into the collect loop, then TERM it.
          (sleep-for 0.5)
          (signal-process proc 'SIGTERM)
          (let ((deadline (+ (float-time) 30)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1)))
          (should-not (process-live-p proc))
          (let ((status (process-status proc))
                (code (process-exit-status proc))
                (output (with-current-buffer buf (buffer-string))))
            (ert-info ((format "child status=%S code=%S output=%s"
                               status code output))
              ;; Not the emacs_abort/SIGABRT crash path.
              (should-not (and (eq status 'signal) (= code 6)))
              (should-not (string-match-p "wrong-type-argument" output))
              (should-not (string-match-p "Fatal error" output))
              ;; Original SIGTERM semantics: killed BY the signal.
              (should (eq status 'signal))
              (should (= code 15)))))
      (when (process-live-p proc)
        (signal-process proc 'SIGKILL))
      (kill-buffer buf))))

(ert-deftest dorg-test-batch-kill-emacs-hook-free-of-dorg-cleanup ()
  "The noninteractive test process contributes NOTHING to `kill-emacs-hook'.
Zero DOrg/test cleanup Lisp on the fatal path: no `dorg-'-prefixed entry
(this deletes the heavy helpers hook and removes the production
`dorg-dolt-server-stop-all' registration, whose work moves to the runner's
unwind cleanup) and no lazily-added `org-persist-' entry.  The scan covers
EVERY entry, not just named symbols: each entry's unbounded printed form is
searched, so a lambda/closure whose body, captured environment, or
byte-code constant vector references dorg-/org-persist- cleanup is an
offender exactly like a named function.  With an empty fatal path nothing
can signal, allocate, or wait during fatal shutdown, whatever C code the
signal interrupted.  RED before tracked-case IMPL.
Printed-form scanning cannot see inside opaque natively-compiled subrs;
`dorg-test-harness-sources-add-no-kill-emacs-hook' closes that gap
statically at the registration site."
  (skip-unless noninteractive)
  (let ((print-level nil)
        (print-length nil)
        offenders)
    (dolist (entry kill-emacs-hook)
      (when (string-match-p "\\_<\\(dorg-\\|org-persist-\\)"
                            (prin1-to-string entry))
        (push entry offenders)))
    (should (null offenders)))
  ;; The heavy hook is deleted outright, not just unregistered.
  (should-not (fboundp 'dorg-test--kill-emacs-cleanup-shared-server)))

(ert-deftest dorg-test-harness-sources-add-no-kill-emacs-hook ()
  "The test-harness sources REGISTER nothing on `kill-emacs-hook'.
Static complement to the runtime printed-form scan (which cannot introspect
natively-compiled anonymous functions): recursively read every top-level
form of dorg-test-helpers.el and dorg-tests-runner.el and reject ANY
\(add-hook \\='kill-emacs-hook ...) — symbol, lambda, and closure alike.
`remove-hook'/strip forms remain legal: the harness may only ever REMOVE
entries from the fatal path, never add them.  RED before tracked-case IMPL
(the helpers still register the heavy cleanup hook at load)."
  (let ((max-lisp-eval-depth 10000)
        offenders)
    (cl-labels ((walk (form file)
                  (when (consp form)
                    (when (and (eq (car-safe form) 'add-hook)
                               (equal (car-safe (cdr form)) ''kill-emacs-hook))
                      (push (cons file form) offenders))
                    (walk (car form) file)
                    (walk (cdr form) file))))
      (dolist (file '("dorg-test-helpers.el" "dorg-tests-runner.el"))
        (with-temp-buffer
          (insert-file-contents
           (expand-file-name (concat "tests/" file) dorg-test-infra--root))
          (condition-case nil
              (while t (walk (read (current-buffer)) file))
            (end-of-file nil)))))
    (should (null offenders))))

(defvar org-element-cache-persistent)   ; defined in org-element

(ert-deftest dorg-test-setup-environment-hardens-org-persist ()
  "`dorg-setup-test-environment' strips org-persist from the fatal path —
including entries org-persist registers when it loads AFTER setup ran.
org-persist joins `kill-emacs-hook' lazily when org-element cache
persistence engages mid-suite; dorg-app.el hardens batch CLI processes the
same way (dorg-app.el:107-120) and the test runner must mirror it.
The REAL pre-load order is simulated (org-persist is long since loaded in
the suite process): `features' is set to a copy WITHOUT `org-persist' so
`featurep' is false while setup runs — a `with-eval-after-load'-based
strip therefore REGISTERS instead of firing immediately — and
`after-load-alist' is set to a copy EXCLUDING all pre-existing
`org-persist'-keyed callbacks (dorg-app's own hardening among them), so
ONLY a callback registered by setup itself can do the stripping: the
assertion cannot pass vacuously.  Both variables are C-level storage that
`featurep'/`provide' read WITHOUT going through a Lisp let-binding
(verified empirically: a let-bound `features' copy is invisible to
`featurep'), so the isolation is setq + unwind-protect restore, not let.
Sequence: (1) setup runs while org-persist counts as not-yet-loaded and
must leave `org-element-cache-persistent' nil; (2) a synthetic
org-persist- entry is injected and the real org-persist library is loaded
— its load-time registrations join the (let-bound) hook, then `provide'
fires the after-load callback setup registered — after which EVERY
org-persist- entry, synthetic and genuine alike, must be gone.  A strip
that only runs inside setup itself (or on a repeat setup call) fails: the
load event happens after setup returned.  RED before tracked-case IMPL."
  (let ((orig-features features)
        (orig-after-load-alist after-load-alist)
        (kill-emacs-hook (copy-sequence kill-emacs-hook))
        (org-element-cache-persistent t))
    (cl-flet ((offenders ()
                (seq-filter
                 (lambda (e)
                   (and (symbolp e)
                        (string-prefix-p "org-persist-" (symbol-name e))))
                 kill-emacs-hook)))
      (unwind-protect
          (progn
            ;; Enter the simulated pre-load world.
            (setq features (delq 'org-persist (copy-sequence features)))
            (setq after-load-alist
                  (seq-remove (lambda (e) (eq (car-safe e) 'org-persist))
                              after-load-alist))
            (should-not (featurep 'org-persist))
            ;; (1) Setup BEFORE org-persist has (as far as it can tell)
            ;; loaded.
            (dorg-setup-test-environment)
            (should (null org-element-cache-persistent))
            ;; (2) The lazy-load event, after setup already returned:
            ;; inject a synthetic entry, then genuinely load org-persist —
            ;; its load-time registrations land, and `provide' fires
            ;; setup's after-load callback.
            (add-hook 'kill-emacs-hook 'org-persist-fake-lazy-cleanup)
            (should (offenders))        ; fixture is non-vacuous
            (load "org-persist" nil t)
            (should (featurep 'org-persist)) ; the load really happened
            (should (null (offenders))))
        (setq features orig-features
              after-load-alist orig-after-load-alist)))))

(ert-deftest dorg-test-registry-record-lifecycle ()
  "Durable ownership records: ATOMIC write/rewrite/delete of readable plists.
`dorg-test--registry-write' creates the registry dir on demand and lands
`<key>.eld' ATOMICALLY: the payload is written to a sibling temp file in
the SAME directory and renamed onto the final name (same-filesystem atomic
replace) — never an in-place write, which final-content checks alone
cannot distinguish.  The replace routes through the plain-Lisp seam
`dorg-test--registry-rename' (one rename of SRC onto DST): the test spies
and fails THAT seam, never `cl-letf's the native `rename-file' primitive
\(a known native-comp trampoline hazard in this repo).  A failure injected
at the seam leaves the PREVIOUS record fully readable and no temp litter
behind: a crash mid-write can never expose a partial or absent record to
next-start reaping.  The DEFAULT seam is also exercised unmocked to pin
its replace semantics.  Delete removes the file and no-ops on a missing
key.  Records are written BEFORE a server is spawned.
RED (void-function) before tracked-case IMPL."
  (let* ((base (make-temp-file "dorg-registry-test-" t))
         ;; Not yet existing — write must create it.
         (dorg-test--registry-dir (expand-file-name "sub" base))
         (renames nil))
    (unwind-protect
        (cl-flet ((read-record (f)
                    (with-temp-buffer
                      (insert-file-contents f)
                      (read (current-buffer)))))
          (let ((file (expand-file-name "k1.eld" dorg-test--registry-dir)))
            (cl-letf (((symbol-function 'dorg-test--registry-rename)
                       (lambda (src dst)
                         (push (cons src dst) renames)
                         (rename-file src dst t))))
              (dorg-test--registry-write
               "k1" '(:owner-pid 1 :port 3307
                      :root "/tmp/dorg-test-r/" :dolt-dir "/tmp/dorg-test-r/d/"))
              (should (file-exists-p file))
              (should (equal 3307 (plist-get (read-record file) :port)))
              ;; Atomicity evidence: the payload arrived by renaming a
              ;; sibling temp file from the SAME directory onto the final
              ;; name — not by writing the final name directly.
              (let ((hit (rassoc file renames)))
                (should hit)
                (should-not (equal (car hit) file))
                (should (equal (file-name-directory (car hit))
                               (file-name-as-directory
                                dorg-test--registry-dir)))
                ;; The rename consumed the temp.
                (should-not (file-exists-p (car hit))))
              ;; Rewrite (e.g. pid learned right after spawn) replaces the
              ;; content through the same rename boundary.
              (setq renames nil)
              (dorg-test--registry-write
               "k1" '(:owner-pid 1 :server-pid 4242 :port 3307
                      :root "/tmp/dorg-test-r/" :dolt-dir "/tmp/dorg-test-r/d/"))
              (should (equal 4242 (plist-get (read-record file) :server-pid)))
              (should (rassoc file renames)))
            ;; Injected pre-rename failure at the seam: the write signals,
            ;; the PREVIOUS record survives fully readable, and the temp
            ;; file is cleaned.
            (cl-letf (((symbol-function 'dorg-test--registry-rename)
                       (lambda (&rest _)
                         (error "tracked-case spec: injected pre-rename failure"))))
              (should-error (dorg-test--registry-write "k1" '(:owner-pid 9))))
            (should (equal 4242 (plist-get (read-record file) :server-pid)))
            (should (equal '("k1.eld")
                           (directory-files
                            dorg-test--registry-dir nil
                            directory-files-no-dot-files-regexp)))
            ;; The DEFAULT (unmocked) seam really replaces DST with SRC —
            ;; the seam contract the mocks above stand in for.
            (let ((a (expand-file-name "a.tmp" base))
                  (b (expand-file-name "b.eld" base)))
              (with-temp-file a (insert "payload"))
              (with-temp-file b (insert "old"))
              (dorg-test--registry-rename a b)
              (should-not (file-exists-p a))
              (should (equal "payload"
                             (with-temp-buffer
                               (insert-file-contents b)
                               (buffer-string)))))
            (dorg-test--registry-delete "k1")
            (should-not (file-exists-p file))
            ;; Missing key: bounded no-op.
            (dorg-test--registry-delete "k1")))
      (delete-directory base t))))

;; OC REFINE (correction 3): `dorg-test--registry-read' fails CLOSED on every
;; corrupt shape — only exactly one proper, even-length, keyword-keyed plist
;; spanning the file (whitespace to EOF) is a valid durable record.  A reader
;; that accepts any cons would consume a dotted/odd/non-plist/foreign or
;; half-overwritten record and let the reaper act on undeterminable ownership.
(ert-deftest dorg-test-registry-read-requires-proper-keyword-plist ()
  "`dorg-test--registry-read' accepts ONLY a non-empty, proper, even-length,
keyword-keyed plist followed by whitespace to EOF, and returns nil (fail
closed) for a dotted tail, an odd element count, a non-keyword key, a bare
atom, an empty list, or a trailing SECOND form.  Non-vacuous against a reader
that returns any `consp' datum: each mutant below is a cons (or, for the atom
case, is specifically rejected) that the old gate would have mis-accepted."
  (let ((dir (make-temp-file "dorg-regread-" t)))
    (unwind-protect
        (cl-flet ((write-raw (name text)
                    (let ((f (expand-file-name name dir)))
                      (with-temp-file f (insert text))
                      f))
                  (write-form (name form)
                    (let ((f (expand-file-name name dir)))
                      (with-temp-file f
                        (let ((print-level nil) (print-length nil))
                          (prin1 form (current-buffer))))
                      f)))
          ;; Valid: proper even-length keyword plist, trailing whitespace only.
          (should (equal '(:owner-pid 7 :root "/tmp/dorg-x/")
                         (dorg-test--registry-read
                          (write-raw "ok.eld"
                                     "(:owner-pid 7 :root \"/tmp/dorg-x/\")\n\n"))))
          ;; The pure predicate directly (positive + every negative).
          (should (dorg-test--proper-keyword-plist-p '(:a 1 :b 2)))
          (should-not (dorg-test--proper-keyword-plist-p '(:a . 1)))      ; dotted
          (should-not (dorg-test--proper-keyword-plist-p '(:a 1 :b)))     ; odd
          (should-not (dorg-test--proper-keyword-plist-p '("a" 1)))       ; non-kw key
          (should-not (dorg-test--proper-keyword-plist-p '(:a 1 "b" 2)))  ; kw then non-kw
          (should-not (dorg-test--proper-keyword-plist-p 42))             ; atom
          (should-not (dorg-test--proper-keyword-plist-p nil))            ; empty
          ;; Fail closed through the file reader on every corrupt shape.
          (should-not (dorg-test--registry-read
                       (write-form "dotted.eld" (cons :owner-pid 1))))
          (should-not (dorg-test--registry-read
                       (write-raw "odd.eld" "(:owner-pid 1 :root)")))
          (should-not (dorg-test--registry-read
                       (write-raw "nonkw.eld" "(\"owner-pid\" 1 \"root\" 2)")))
          (should-not (dorg-test--registry-read
                       (write-raw "atom.eld" "42")))
          (should-not (dorg-test--registry-read
                       (write-raw "empty.eld" "()")))
          ;; A valid first plist followed by a SECOND form is still rejected.
          (should-not (dorg-test--registry-read
                       (write-raw "trailing.eld"
                                  "(:owner-pid 1 :root \"/tmp/dorg-y/\") (:junk 2)"))))
      (delete-directory dir t))))

(ert-deftest dorg-test-shared-server-persists-ownership-before-spawn ()
  "The shared-server ownership record is durable BEFORE `start-process'.
Persist-before-start is the property that makes zero-fatal-hook shutdown
safe: no window exists in which a server process can outlive its Emacs
without a durable record for next-start reaping.  The spy on
`start-process' captures the registry state at the spawn moment and
aborts the start immediately after (via a failing wait-ready), so no
real server is launched.  RED before tracked-case IMPL.

ISOLATION (tracked-case): `dorg-dolt-fixture-shared-ensure' is idempotent —
in the full suite a live shared singleton already exists, so ensure would
REUSE it and never reach the spawn path, leaving the spy silent.  Every
mutable singleton global the spawn path reads or writes is therefore
dynamically SHADOWED here (never `setq'-nulled): the shared/failed-start
leases and conn are bound nil so ensure is forced down
`dorg-dolt-fixture--shared-start'; `--cold-starts' and `--procs' are bound
to private copies so the stubbed spawn's `cl-incf'/`puthash' cannot bump
the runner's shared-server invariant; and the legacy `dorg-test--server-*'
globals are bound nil so the wrapper publish and cleanup cannot clobber the
live full-suite fixture's proc/data-dir."
  (let* ((dir (make-temp-file "dorg-registry-test-" t))
         (dorg-test--registry-dir dir)
         (dorg-test--mysql-available t)
         ;; Force the spawn path and fully isolate the live singleton.
         (dorg-dolt-fixture--shared-lease nil)
         (dorg-dolt-fixture--failed-start-lease nil)
         (dorg-dolt-fixture--conn nil)
         (dorg-dolt-fixture--cold-starts dorg-dolt-fixture--cold-starts)
         (dorg-dolt-server--procs (make-hash-table :test 'equal))
         (dorg-test--server-proc nil)
         (dorg-test--server-data-dir nil)
         (dorg-test--server-port nil)
         (dorg-test--mysql-conn nil)
         (record-at-spawn 'not-captured))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dorg-dolt-supported-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'dorg-dolt-server--free-port)
                     (lambda () 3999))
                    ((symbol-function 'dorg-dolt-server--spawn-logged)
                     (lambda (&rest _)
                       (setq record-at-spawn
                             (let (hit)
                               (when (file-directory-p dir)
                                 (dolist (f (directory-files
                                             dir t "\\.eld\\'"))
                                   (let ((pl (with-temp-buffer
                                               (insert-file-contents f)
                                               (read (current-buffer)))))
                                     (when (equal 3999
                                                  (plist-get pl :port))
                                       (setq hit pl)))))
                               hit))
                       'fake-proc))
                    ((symbol-function 'set-process-query-on-exit-flag)
                     (lambda (&rest _) nil))
                    ((symbol-function 'process-live-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--wait-ready)
                     (lambda (&rest _)
                       (error "spec: abort start after spawn"))))
            (should-error (dorg-test--start-shared-server)))
          ;; The ownership record was already durable at the spawn moment.
          (should (consp record-at-spawn))
          (should (equal (emacs-pid)
                         (plist-get record-at-spawn :owner-pid))))
      ;; The singleton globals are dynamically shadowed above, so they restore
      ;; automatically — do NOT `setq' them nil or the live fixture is clobbered.
      (delete-directory dir t))))

(ert-deftest dorg-test-stale-registry-reap-kills-only-verified-dead-owner ()
  "Next-start reaping is lane-safe, identity-gated, CONFIRM-before-consume.
The record matrix pins every gate separately, under the confirm-before-
destroy contract (OC 02:51): a record is consumed ONLY when every server it
implicates is confirmed GONE.
- owner LIVE (555): skipped — record KEPT, REAL root KEPT, no identity
  inspection (a concurrent lane's processes are never even probed);
- owner liveness UNKNOWN (444): treated as live — skipped, record KEPT,
  REAL root KEPT, no identity inspection, never killed EVEN THOUGH its
  attributes would match perfectly (fail-safe when enumeration is
  unsupported);
- owner dead + full identity match on recorded fields (666): killed AND
  CONFIRMED dead (post-kill liveness flips), so the record is consumed;
- owner dead + live NON-dolt process (777): identity inspected, NOT a match
  → cannot be confirmed gone → record PRESERVED for retry (never a
  silent consume that would strand an untraceable server);
- owner dead + server pid DEAD (888): never signaled AND never
  identity-probed (liveness gates before inspection) — a dead pid IS
  confirmed absent, so the record is consumed;
- owner dead + live dolt NEAR-MISS (999): a `dolt sql-server' whose
  port/data-dir do NOT match the record — inspected, never signaled, and
  NOT confirmable → record PRESERVED.  Kills require matching the RECORDED
  identity fields, not merely comm=dolt.
The kept records (555/444) carry REAL prefix-valid roots: a buggy reaper that
treats live/unknown owners as dead WOULD delete them, so their survival is
asserted directly.  Every dead-owner root is an ELIGIBLE `dorg-*' dir under
the (bound) canonical `temporary-file-directory' with a CONTAINED dolt-dir, so
the explicit identity is legitimately probed (containment is exercised
separately).  A second run over the confirmed-gone records no-ops.  A missing
registry dir is a no-op."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-registry-base-" t))))
         (dir (concat base "registry/"))
         (dorg-test--registry-dir dir)
         (live-root (file-name-as-directory
                     (make-temp-file "dorg-test-keep-live-" t)))
         (unknown-root (file-name-as-directory
                        (make-temp-file "dorg-test-keep-unknown-" t)))
         (root-b (concat base "dorg-test-b/")) (dd-b (concat base "dorg-test-b/d/"))
         (root-c (concat base "dorg-test-c/")) (dd-c (concat base "dorg-test-c/d/"))
         (root-d (concat base "dorg-test-d/")) (dd-d (concat base "dorg-test-d/d/"))
         (root-f (concat base "dorg-test-f/")) (dd-f (concat base "dorg-test-f/d/"))
         (kills nil)
         (attr-queries nil))
    (make-directory dir t)
    (unwind-protect
        (progn
          (dolist (rec `((:owner-pid 111 :server-pid 555 :port 1
                          :root ,live-root :dolt-dir ,(concat live-root "d/"))
                         (:owner-pid 333 :server-pid 444 :port 5
                          :root ,unknown-root :dolt-dir ,(concat unknown-root "d/"))
                         (:owner-pid 222 :server-pid 666 :port 2
                          :root ,root-b :dolt-dir ,dd-b)
                         (:owner-pid 222 :server-pid 777 :port 3
                          :root ,root-c :dolt-dir ,dd-c)
                         (:owner-pid 222 :server-pid 888 :port 4
                          :root ,root-d :dolt-dir ,dd-d)
                         (:owner-pid 222 :server-pid 999 :port 6
                          :root ,root-f :dolt-dir ,dd-f)))
            (with-temp-file (expand-file-name
                             (format "s%d.eld" (plist-get rec :server-pid)) dir)
              (prin1 rec (current-buffer))))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid)
                       (cond ((eq pid 333) 'unknown)   ; owner: unsupported
                             ((memq pid kills) nil)     ; killed -> confirmed gone
                             ((memq pid '(111 444 666 777 999)) t)
                             (t nil))))                ; owners 222; server 888
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (push pid attr-queries)
                       (cond
                        ;; 444: PERFECT match — must still never be
                        ;; consulted or killed (unknown owner).
                        ((eq pid 444)
                         `((comm . "dolt")
                           (args . ,(format "dolt sql-server --port 5 --data-dir %sd/"
                                            unknown-root))))
                        ;; 666: matches the record's port AND data-dir.
                        ((eq pid 666)
                         `((comm . "dolt")
                           (args . ,(format "dolt sql-server --port 2 --data-dir %s" dd-b))))
                        ;; 888: DEAD pid whose attributes nevertheless
                        ;; present a PERFECT Dolt identity for its record —
                        ;; only the liveness gate can reject it.
                        ((eq pid 888)
                         `((comm . "dolt")
                           (args . ,(format "dolt sql-server --port 4 --data-dir %s" dd-d))))
                        ;; 999: dolt sql-server, but WRONG port/data-dir.
                        ((eq pid 999)
                         '((comm . "dolt")
                           (args . "dolt sql-server --port 9999 --data-dir /elsewhere/d/")))
                        (t '((comm . "unrelated") (args . "unrelated"))))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &optional _grace) (push pid kills) t)))
            (let ((temporary-file-directory base))
              (dorg-test--reap-stale-server-registry))
            (should (equal kills '(666)))
            ;; Skipped lanes keep their records AND their real roots.
            (should (file-exists-p (expand-file-name "s555.eld" dir)))
            (should (file-exists-p (expand-file-name "s444.eld" dir)))
            (should (file-directory-p live-root))
            (should (file-directory-p unknown-root))
            ;; Confirmed-gone servers (killed 666, already-dead 888) consumed.
            (should-not (file-exists-p (expand-file-name "s666.eld" dir)))
            (should-not (file-exists-p (expand-file-name "s888.eld" dir)))
            ;; Live but UNCONFIRMABLE servers (non-dolt 777, near-miss 999):
            ;; the record is PRESERVED so the server stays traceable.
            (should (file-exists-p (expand-file-name "s777.eld" dir)))
            (should (file-exists-p (expand-file-name "s999.eld" dir)))
            ;; Skipped lanes are never even identity-probed, and neither is
            ;; the DEAD server pid: liveness gating must PRECEDE identity
            ;; inspection (888's perfect Dolt attributes make any
            ;; inspect-first implementation kill it — and even a no-signal
            ;; inspect-first pass is probing a recycled/dead pid).
            (should-not (memq 555 attr-queries))
            (should-not (memq 444 attr-queries))
            (should-not (memq 888 attr-queries))
            ;; Idempotent second run: nothing new killed, kept/preserved
            ;; records and roots still present.
            (let ((temporary-file-directory base))
              (dorg-test--reap-stale-server-registry))
            (should (equal kills '(666)))
            (should (file-exists-p (expand-file-name "s555.eld" dir)))
            (should (file-exists-p (expand-file-name "s444.eld" dir)))
            (should (file-exists-p (expand-file-name "s777.eld" dir)))
            (should (file-exists-p (expand-file-name "s999.eld" dir)))
            (should (file-directory-p live-root))
            (should (file-directory-p unknown-root)))
          ;; Missing registry dir: bounded no-op.
          (let ((dorg-test--registry-dir
                 (expand-file-name "does-not-exist" dir)))
            (dorg-test--reap-stale-server-registry)))
      (ignore-errors (delete-directory live-root t))
      (ignore-errors (delete-directory unknown-root t))
      (delete-directory base t))))

(ert-deftest dorg-test-stale-registry-reap-recovers-pid-from-server-info ()
  "A pid-absent record (persist-before-start window) is still reapable.
Ownership is persisted BEFORE `start-process', so a fatal signal in the
window before the pid is learned leaves a record with no `:server-pid'.
The reaper recovers the pid from the store's dolt-native
`.dolt/sql-server.info', applies the same dolt-identity gate, kills, and
consumes the record + prefix-validated root.
RED (void-function) before tracked-case IMPL."
  (let* ((dir (make-temp-file "dorg-registry-test-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-root-" t)))
         (store (file-name-as-directory (expand-file-name "d" root)))
         (kills nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".dolt" store) t)
          (with-temp-file (expand-file-name ".dolt/sql-server.info" store)
            (insert "4321:3309:aaaa-bbbb-cccc"))
          (with-temp-file (expand-file-name "spending.eld" dir)
            (prin1 (list :owner-pid 222 :port 3309
                         :root root :dolt-dir store
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     ;; owner 222 dead; recovered server pid 4321 live until
                     ;; killed — the post-kill re-check must then confirm gone.
                     (lambda (pid)
                       (and (memq pid '(4321)) (not (memq pid kills)) t)))
                    ((symbol-function 'process-attributes)
                     ;; Full recorded identity in argv: the kill gate is
                     ;; fail-closed, so --port AND --data-dir must both be
                     ;; advertised and equal the record (OC audit
                     ;; 2026-07-16 22:16 — partial identity never kills).
                     (lambda (pid)
                       (when (= pid 4321)
                         `((comm . "dolt")
                           (args . ,(format
                                     "dolt sql-server --port 3309 --data-dir %s"
                                     store))))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &optional _grace) (push pid kills) t)))
            (dorg-test--reap-stale-server-registry))
          (should (equal kills '(4321)))
          (should-not (file-directory-p root))
          (should (null (directory-files dir nil "\\.eld\\'"))))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;; reviewer REJECT (297209c): the persist-before-spawn crash WINDOW between the
;; durable write (dorg-dolt-fixture.el:~1247) and `start-process' (:~1253) left a
;; :server-pid-absent record whose empty root has NO `.dolt/sql-server.info' at
;; all.  Pre-fix, `record-server-identities' emitted a BLOCKING nil-pid identity
;; merely because `:dolt-dir' was present, so the reaper returned
;; `server-unconfirmed', preserved the record/root, and the runner PERMANENTLY
;; refused every later shared start.  The fix: a pid-free, metadata-free,
;; port-dead eligible root implicates NO server and is safely recovered.
(ert-deftest dorg-test-stale-registry-recovers-pre-spawn-no-metadata ()
  "A dead-owner record from the persist-before-spawn window -- `:server-pid'
absent, its identity-intact eligible root EMPTY (no `.dolt/sql-server.info', no
`.dorg' overlay), and nothing listening on `:port' -- is SAFELY RECOVERED: the
reaper implicates no server, reaps the empty root, consumes the record, and
returns NO failures so a later shared start is not refused.  Non-vacuous against
the pre-fix reaper that emits a blocking nil-pid identity for the bare
`:dolt-dir': it would return `server-unconfirmed' and retain both the record and
the root."
  (let* ((dir (make-temp-file "dorg-registry-prespawn-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-preroot-" t)))
         (recfile (expand-file-name "shared-prespawn.eld" dir)))
    (unwind-protect
        (progn
          ;; EXACTLY the record shape written at shared-start:~1247 (root ==
          ;; dolt-dir, :root-id captured, NO :server-pid); the root is left
          ;; empty -- the crash preceded any server, so no metadata exists.
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :port 3311
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     ;; owner 222 dead -> a reap candidate.
                     (lambda (_pid) nil))
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     ;; nothing bound the recorded port (the pre-spawn window
                     ;; never reached `start-process').
                     (lambda (_port) nil))
                    ((symbol-function 'list-system-processes)
                     ;; NO live dolt child exists for this data-dir either -- a
                     ;; genuine pre-spawn crash left no process at all.
                     (lambda () nil)))
            ;; No failures: the record was consumed, so the runner's
            ;; refuse-on-orphans gate does NOT fire on a later start.
            (should (null (dorg-test--reap-stale-server-registry))))
          ;; The empty root was reaped and the durable record consumed.
          (should-not (file-directory-p root))
          (should-not (file-exists-p recfile))
          (should (null (directory-files dir nil "\\.eld\\'"))))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;; reviewer safety net (do NOT weaken confirmed-death): the SAME pid-free /
;; metadata-free record is PRESERVED when a live listener holds its `:port' -- a
;; crash in the post-spawn/pre-`sql-server.info' window orphans a live process
;; that has written no metadata yet, and the port is the only evidence left.
(ert-deftest dorg-test-stale-registry-pre-spawn-live-port-preserved ()
  "A dead-owner pid-free, metadata-free record whose recorded `:port' STILL has
a live listener is PRESERVED, not recovered: the reaper emits a blocking nil-pid
identity, returns `server-unconfirmed', and RETAINS the record and its root so a
post-spawn orphan is never deleted out from under a live server.  Non-vacuous
against a fix that recovers on pid/metadata absence ALONE (ignoring the port):
it would consume the record and reap the root over a live orphan."
  (let* ((dir (make-temp-file "dorg-registry-liveport-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-liveroot-" t)))
         (recfile (expand-file-name "shared-liveport.eld" dir)))
    (unwind-protect
        (progn
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :port 3312
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     ;; an orphan (or any process) holds the recorded port.
                     (lambda (_port) t)))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              ;; Reported as an unconfirmed server -> blocks a fresh start.
              (should (eq 'server-unconfirmed (cdr (assoc recfile failures))))))
          ;; Record AND root RETAINED for a later (safe) retry.
          (should (file-exists-p recfile))
          (should (file-directory-p root)))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;; reviewer RE-CERT #2 REJECT (09e9485): port-dead != process-dead.  A child
;; forked by `start-process' but NOT yet listening (pre-bind, pre-
;; `sql-server.info') is PORT-DEAD yet PROCESS-ALIVE and has written no metadata
;; -- its durable state is byte-identical to a pre-spawn crash record.  Recovery
;; on port/metadata absence ALONE would delete the root out from under a
;; legitimately-starting server.  The reaper must consult the OS process table.
(ert-deftest dorg-test-stale-registry-spawned-not-listening-retained ()
  "A dead-owner pid-free, metadata-free, port-DEAD record whose dolt-dir still
has a LIVE `dolt sql-server' process (a just-forked child not yet bound) is
PRESERVED, not recovered: `dorg-test--dolt-process-live-under-p' finds the
process in the OS table, the reaper emits a blocking nil-pid identity, returns
`server-unconfirmed', and RETAINS the record and root so a starting server is
never reaped out from under itself.  Non-vacuous against the port-only
over-reaping code (09e9485): with a dead port and no metadata it would consume
the record and delete the root despite the live child."
  (let* ((dir (make-temp-file "dorg-registry-starting-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-startroot-" t)))
         (recfile (expand-file-name "shared-starting.eld" dir)))
    (unwind-protect
        (progn
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :port 3313
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))          ; owner dead
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     (lambda (_port) nil))          ; child forked but NOT bound
                    ((symbol-function 'list-system-processes)
                     (lambda () '(90001)))
                    ((symbol-function 'process-attributes)
                     ;; A live `dolt sql-server' launched with this data-dir --
                     ;; the argv the fixture spawn passes.
                     (lambda (pid)
                       (when (= pid 90001)
                         `((comm . "dolt")
                           (args . ,(format
                                     "dolt sql-server --port 3313 --data-dir %s"
                                     root)))))))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              ;; Reported as an unconfirmed server -> blocks a fresh start.
              (should (eq 'server-unconfirmed (cdr (assoc recfile failures))))))
          ;; Record AND root RETAINED -- the starting server is untouched.
          (should (file-exists-p recfile))
          (should (file-directory-p root)))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;; reviewer re-cert #3 REJECT (8f19e51): the process scan must not FAIL OPEN.
;; `ignore-errors' around `list-system-processes' turned an enumeration failure
;; into nil, read as "confirmed no process" -> the root/record were deleted even
;; though a live child could not be ruled out.  A scan that cannot COMPLETE is
;; `unknown' and must PRESERVE.
(ert-deftest dorg-test-stale-registry-process-enumeration-error-preserves ()
  "When `list-system-processes' SIGNALS, process evidence is `unknown' -- the
pid-free, metadata-free, port-dead record is PRESERVED, not recovered: the reaper
returns `server-unconfirmed' and RETAINS the record and root, so an unexaminable
process table can never be misread as a confirmed absence.  Non-vacuous against
the fail-open scan (8f19e51) that swallows the enumeration error to nil and
reaps."
  (let* ((dir (make-temp-file "dorg-registry-enumerr-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-enumroot-" t)))
         (recfile (expand-file-name "shared-enumerr.eld" dir)))
    (unwind-protect
        (progn
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :port 3314
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     (lambda (_port) nil))
                    ((symbol-function 'list-system-processes)
                     (lambda () (error "process table unavailable"))))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should (eq 'server-unconfirmed (cdr (assoc recfile failures))))))
          (should (file-exists-p recfile))
          (should (file-directory-p root)))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

(ert-deftest dorg-test-stale-registry-process-attributes-error-preserves ()
  "When `process-attributes' SIGNALS for a live pid during the scan, that
process could not be examined, so evidence is `unknown' and the pid-free,
metadata-free, port-dead record is PRESERVED: the reaper returns
`server-unconfirmed' and RETAINS the record and root.  Non-vacuous against the
fail-open scan (8f19e51) that swallows the probe error, finds no match, and
reaps -- the unexamined process might have BEEN the child."
  (let* ((dir (make-temp-file "dorg-registry-attrerr-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-stale-attrroot-" t)))
         (recfile (expand-file-name "shared-attrerr.eld" dir)))
    (unwind-protect
        (progn
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :port 3315
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     (lambda (_port) nil))
                    ((symbol-function 'list-system-processes)
                     (lambda () '(90002)))
                    ((symbol-function 'process-attributes)
                     (lambda (_pid) (error "attributes unavailable"))))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should (eq 'server-unconfirmed (cdr (assoc recfile failures))))))
          (should (file-exists-p recfile))
          (should (file-directory-p root)))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

(ert-deftest dorg-test-stale-registry-reap-deletes-only-prefixed-roots ()
  "Dead-owner root records delete ONLY canonically-contained test roots.
A root is deletable iff its CANONICAL path (`file-truename', symlinks
resolved) lives under the canonical `temporary-file-directory' AND its
basename is `dorg-'-prefixed.  Lexical prefix/basename checks are
insufficient; the matrix proves each boundary separately:
- `dorg-*' directory under tmpdir: deleted;
- non-dorg basename under tmpdir: survives;
- `dorg-*' basename OUTSIDE tmpdir: survives — a basename-only
  implementation fails here;
- `dorg-*' SYMLINK under tmpdir whose target is OUTSIDE tmpdir (the
  escape): refused entirely — the link, the external target directory,
  and the target's contents all survive.  A lexical-containment or
  truename-then-delete implementation fails here.
Only the eligible good-root is deleted AND its record consumed.  The other
three roots EXIST but are ineligible, so under the confirm-before-destroy
contract (OC 03:56) their records are PRESERVED — never consumed over an
existing root we refuse to own.  The test pins its own canonical
`temporary-file-directory' (a `file-truename'd base) so the containment
boundary is exact."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-registry-esc-" t))))
         ;; Controlled canonical tmpdir for the reap call.
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (good-root (concat tmp "dorg-test-good-root/"))
         (bad-root (concat tmp "unrelated-stale-root/"))
         (outside-root (concat base "dorg-test-outside-root/"))
         (target (concat base "escape-target/"))
         (link (concat tmp "dorg-test-escape")))
    (unwind-protect
        (progn
          (dolist (d (list tmp dorg-test--registry-dir good-root
                           bad-root outside-root target))
            (make-directory d t))
          (with-temp-file (concat target "sentinel.txt") (insert "keep"))
          (make-symbolic-link (directory-file-name target) link)
          (let ((i 0))
            (dolist (root (list good-root bad-root outside-root
                                (file-name-as-directory link)))
              (with-temp-file (expand-file-name
                               (format "r%d.eld" (cl-incf i))
                               dorg-test--registry-dir)
                (prin1 (list :owner-pid 222 :root root
                             :root-id (dorg-test--path-file-identity root))
                       (current-buffer)))))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil)))
            (let ((temporary-file-directory tmp))
              (dorg-test--reap-stale-server-registry)))
          (should-not (file-directory-p good-root))
          (should (file-directory-p bad-root))
          (should (file-directory-p outside-root))
          ;; The escape symlink was refused entirely.
          (should (file-symlink-p link))
          (should (file-directory-p target))
          (should (file-exists-p (concat target "sentinel.txt")))
          ;; Only the eligible good-root's record (r1) is consumed; the three
          ;; existing-ineligible roots (r2/r3/r4) are PRESERVED.
          (should-not (file-exists-p
                       (expand-file-name "r1.eld" dorg-test--registry-dir)))
          (should (file-exists-p
                   (expand-file-name "r2.eld" dorg-test--registry-dir)))
          (should (file-exists-p
                   (expand-file-name "r3.eld" dorg-test--registry-dir)))
          (should (file-exists-p
                   (expand-file-name "r4.eld" dorg-test--registry-dir))))
      (delete-directory base t))))

(ert-deftest dorg-test-stale-registry-reap-missing-identity-evidence-fails-closed ()
  "A kill needs FULL recorded identity in evidence; anything less PRESERVES.
Fail-closed contract (OC audit 2026-07-16 22:16 + confirm-before-destroy
02:51): recorded :port and :dolt-dir must BOTH be present and equal the
--port/--data-dir the live process advertises.  Two missing-evidence shapes,
each a LIVE dolt sql-server with a matching port that a permissive matcher
would kill:
- argv omits --data-dir (near-anonymous dolt): never signaled;
- the RECORD lacks :dolt-dir while argv advertises one: never signaled.
Because neither server can be confirmed gone, BOTH dead-owner records are
PRESERVED for retry — never consumed, which would strand an untraceable live
server.  RED against a matcher that treats absent evidence as compatible (it
would kill) OR a reaper that consumes a record whose server is still live."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-registry-mie-" t))))
         (dir (concat base "registry/"))
         (dorg-test--registry-dir dir)
         (root-x (concat base "dorg-test-x/")) (dd-x (concat base "dorg-test-x/d/"))
         (root-y (concat base "dorg-test-y/"))
         (kills nil))
    (make-directory dir t)
    (unwind-protect
        (progn
          ;; Eligible roots + contained dolt-dirs so the identity is genuinely
          ;; PROBED (this pins the identity-mismatch gate, not mere ineligibility).
          (with-temp-file (expand-file-name "s1.eld" dir)
            (prin1 (list :owner-pid 222 :server-pid 1212 :port 7
                         :root root-x :dolt-dir dd-x)
                   (current-buffer)))
          (with-temp-file (expand-file-name "s2.eld" dir)
            (prin1 (list :owner-pid 222 :server-pid 1313 :port 8
                         :root root-y)
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (memq pid '(1212 1313)) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (cond ((= pid 1212)
                              ;; Port matches; --data-dir absent from argv.
                              '((comm . "dolt")
                                (args . "dolt sql-server --port 7")))
                             ((= pid 1313)
                              ;; Argv complete; the RECORD lacks :dolt-dir.
                              `((comm . "dolt")
                                (args . ,(format "dolt sql-server --port 8 --data-dir %sd/"
                                                 root-y)))))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &optional _grace) (push pid kills) t)))
            (let ((temporary-file-directory base))
              (dorg-test--reap-stale-server-registry)))
          (should (null kills))
          ;; Both records PRESERVED (their servers are live+unconfirmable).
          (should (file-exists-p (expand-file-name "s1.eld" dir)))
          (should (file-exists-p (expand-file-name "s2.eld" dir))))
      (delete-directory base t))))

(ert-deftest dorg-test-stale-registry-reap-servers-before-root-deletion ()
  "PAIRED crash records: servers are reaped before any root is deleted.
A real persist-before-start crash leaves TWO records for the shared
root — the generic owned-root record (`root-*') written by
`dorg-test--register-owned-root' and the shared-server record
\(`shared-*', no `:server-pid' yet) — and the registry listing orders
`root-*' first.  A record-by-record reaper deletes the store (and its
`.dolt/sql-server.info') off the root record BEFORE the shared record's
pid recovery can read it, stranding the live server (OC audit
2026-07-16 22:16).  The contract: across the WHOLE listing, server
reaping happens first, root deletion second, record consumption last —
the recovered pid is killed AND the root is deleted AND both records
are consumed.  RED against a record-at-a-time implementation."
  (let* ((dir (make-temp-file "dorg-registry-test-" t))
         (dorg-test--registry-dir dir)
         ;; Realistic shared root: the data-dir IS the store, named like
         ;; the real fixture and under the real tmpdir, so it is
         ;; legitimately deletable.
         ;; `file-truename' so the record's dolt-dir, the discovered store's
         ;; canonical dir, and the mocked argv all agree (no raw-vs-canon
         ;; path split creating a spurious extra identity tuple).
         (root (file-name-as-directory
                (file-truename (make-temp-file "dorg-test-data-" t))))
         (kills nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".dolt" root) t)
          (with-temp-file (expand-file-name ".dolt/sql-server.info" root)
            (insert "5150:3311:dddd-eeee-ffff"))
          ;; Exactly the paired records a crash in the spawn window
          ;; leaves; the root record's key sorts FIRST.
          (with-temp-file (expand-file-name "root-222-aaaa.eld" dir)
            (prin1 (list :owner-pid 222 :root root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (with-temp-file (expand-file-name "shared-222.eld" dir)
            (prin1 (list :owner-pid 222 :port 3311
                         :root root :dolt-dir root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     ;; owner 222 dead; recovered server pid 5150 live until
                     ;; killed, so the post-kill re-check confirms it gone.
                     (lambda (pid)
                       (and (memq pid '(5150)) (not (memq pid kills)) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (when (= pid 5150)
                         `((comm . "dolt")
                           (args . ,(format
                                     "dolt sql-server --port 3311 --data-dir %s"
                                     root))))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &optional _grace) (push pid kills) t)))
            (dorg-test--reap-stale-server-registry))
          ;; The pid was recovered from sql-server.info and killed — the
          ;; root record's deletion did not get to destroy it first.
          (should (equal kills '(5150)))
          (should-not (file-directory-p root))
          (should (null (directory-files dir nil "\\.eld\\'"))))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;;; ─── Crash-net confirm-before-destroy blockers (OC 02:51 / 03:08) ─────
;;
;; Removing the in-memory owned-server shadow put the whole next-start
;; recovery burden on the durable registry + on-disk metadata scan.  These
;; focused mutants pin the confirm-before-destroy contract: derive exact
;; identity, kill only verified servers, confirm gone, and PRESERVE
;; (never destroy metadata) on any unknown/failure.

;; OC 03:08 (1): the sole durable persist must FAIL LOUD and roll back.
(ert-deftest dorg-test-register-owned-root-fails-loud-on-persist-failure ()
  "`dorg-test--register-owned-root' FAILS LOUD when the durable persist fails
and ROLLS BACK the in-memory `--owned-roots' insertion — never accepting a
root with no crash-net now that the in-memory shadow is gone.  Non-vacuous
against an `ignore-errors' persist that swallows the failure and leaves an
in-memory-only, unrecoverable ownership."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'dorg-test--registry-write)
               (lambda (&rest _) (error "spec: persist failed"))))
      (should-error (dorg-test--register-owned-root "/tmp/dorg-test-regfail/"))
      (should (= 0 (hash-table-count dorg-test--owned-roots))))))

;; OC 03:08 (2): delete the exact root, THEN drop ownership; a delete failure
;; preserves root + record + in-memory ownership.
(ert-deftest dorg-test-delete-owned-root-preserves-on-delete-failure ()
  "`dorg-test--delete-owned-root' deletes the root FIRST and drops ownership
only AFTER: when servers are confirmed gone but `delete-directory' then FAILS,
the durable record AND the in-memory `--owned-roots' entry are BOTH retained —
the root is never stranded after ownership was consumed.  Non-vacuous against
the prior order (unregister before delete), which drops both registrations and
strands the root on a delete failure."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-delroot-reg-" t))
         (root (dorg-test--canonical-path
                (make-temp-file "dorg-test-delroot-" t) t))
         (reg-deletes 0))
    (unwind-protect
        (progn
          (puthash root (dorg-test--path-file-identity root)
                   dorg-test--owned-roots)
          (cl-letf (((symbol-function 'dorg-test--reap-owned-root)
                     (lambda (&rest _) 'gone))
                    ((symbol-function 'delete-directory)
                     (lambda (&rest _) (error "spec: rmdir wedged")))
                    ((symbol-function 'dorg-test--registry-delete)
                     (lambda (&rest _) (cl-incf reg-deletes) nil)))
            (should-error (dorg-test--delete-owned-root root))
            ;; Delete failed BEFORE ownership was dropped: both retained.
            (should (gethash root dorg-test--owned-roots))
            (should (= 0 reg-deletes))))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC 03:08 (2) non-vacuity: the runner's owned-root cleanup drives the FULL
;; delete path (deleting the root-delete call from the runner fails this).
(ert-deftest dorg-test-reap-owned-roots-drives-full-delete ()
  "`dorg-test--reap-owned-roots' drives the FULL delete path per root: confirm
servers gone, DELETE the exact directory, then drop ownership — returning nil
(no failures) on success.  Non-vacuous against a reap that unregisters without
deleting the root (the directory would survive) — i.e. deleting the root-delete
call from the runner's cleanup path fails this."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-ror-reg-" t))
         (root (dorg-test--canonical-path
                (make-temp-file "dorg-test-ror-" t) t)))
    (unwind-protect
        (progn
          (puthash root (dorg-test--path-file-identity root)
                   dorg-test--owned-roots)
          (cl-letf (((symbol-function 'dorg-test--reap-owned-root)
                     (lambda (&rest _) 'gone))
                    ((symbol-function 'dorg-test--registry-delete)
                     (lambda (&rest _) nil)))
            (should (file-directory-p root))
            (let ((failures (dorg-test--reap-owned-roots)))
              (should (null failures))
              ;; Full delete path ran: directory gone, ownership dropped.
              (should-not (file-directory-p root))
              (should (= 0 (hash-table-count dorg-test--owned-roots))))))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC REFINE (blocker 1 / narrow correction): registration FAILS LOUD when the
;; filesystem identity cannot be captured — an unreapable root must never be
;; registered, and `make-owned-temp-dir' rolls the fresh dir back.
(ert-deftest dorg-test-register-owned-root-fails-loud-on-nil-identity ()
  "`dorg-test--register-owned-root' FAILS LOUD BEFORE any `puthash'/durable
write when `dorg-test--path-file-identity' returns nil — a root with no captured
identity can never pass the reap identity gate, so `make-owned-temp-dir' rolls
the freshly-created dir back rather than leak an unreapable root.  Non-vacuous
against a register that stores a nil-identity ownership entry or writes a
nil-`:root-id' record."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (dorg-test--registry-dir (make-temp-file "dorg-nilid-reg-" t))
        (writes 0) (created nil))
    (unwind-protect
        (cl-letf* ((real-mtf (symbol-function 'make-temp-file))
                   ((symbol-function 'make-temp-file)
                    (lambda (&rest a)
                      (if (equal (car a) "dorg-nilid-")
                          (setq created (apply real-mtf a))
                        (apply real-mtf a))))
                   ((symbol-function 'dorg-test--path-file-identity)
                    (lambda (&rest _) nil))
                   ((symbol-function 'dorg-test--registry-write)
                    (lambda (&rest _) (cl-incf writes) nil)))
          (should-error (dorg-test--make-owned-temp-dir "dorg-nilid-"))
          ;; No durable write, no in-memory entry, and the dir was rolled back.
          (should (= 0 writes))
          (should (= 0 (hash-table-count dorg-test--owned-roots)))
          (should created)
          (should-not (file-exists-p created)))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC REFINE (blocker 1): register refuses a caller-supplied TERMINAL symlink
;; BEFORE canonicalizing (else it would capture the symlink TARGET's identity).
(ert-deftest dorg-test-register-owned-root-refuses-terminal-symlink ()
  "`dorg-test--register-owned-root' REFUSES a path whose terminal component is a
SYMLINK — `dorg-test--canonical-path' would `file-truename' it and capture the
symlink TARGET's identity, registering a link we do not own.  Non-vacuous
against a register that canonicalizes first and captures the resolved target's
identity (it would register the link)."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-symreg-" t))
         (base (file-name-as-directory
                (file-truename (make-temp-file "dorg-symreg-b-" t))))
         (target (concat base "target/"))
         (link (concat base "dorg-test-link")))
    (unwind-protect
        (progn
          (make-directory target t)
          (make-symbolic-link (directory-file-name target) link)
          (should-error (dorg-test--register-owned-root
                         (file-name-as-directory link)))
          (should (= 0 (hash-table-count dorg-test--owned-roots))))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-directory base t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC REFINE (blocker 1): normal owned-root cleanup refuses a SYMLINK swapped in
;; over the registered path — the outside target is never scanned or deleted.
(ert-deftest dorg-test-owned-root-cleanup-refuses-symlink-replacement ()
  "After a root is registered, replacing its exact path with a SYMLINK to an
OUTSIDE directory makes `dorg-test--delete-owned-root' REFUSE
\(`root-identity-mismatch'): the outside target and its contents survive, and
ownership + the durable record are RETAINED (never unregistered).  Non-vacuous
against cleanup that re-`file-truename's the path and scans/deletes the outside
target."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-symrepl-reg-" t))
         (outside (file-name-as-directory
                   (file-truename (make-temp-file "dorg-symrepl-out-" t))))
         (canon (dorg-test--make-owned-temp-dir "dorg-test-symrepl-"))
         (record (expand-file-name
                  (concat (dorg-test--registry-root-key canon) ".eld")
                  dorg-test--registry-dir)))
    (unwind-protect
        (progn
          (with-temp-file (concat outside "sentinel.txt") (insert "keep"))
          ;; Replace the registered real dir with a SYMLINK to the outside dir.
          (delete-directory (directory-file-name canon) t)
          (make-symbolic-link (directory-file-name outside)
                              (directory-file-name canon))
          (should (eq 'root-identity-mismatch
                      (dorg-test--delete-owned-root canon)))
          ;; The symlink, the outside target, and its content all survive.
          (should (file-symlink-p (directory-file-name canon)))
          (should (file-directory-p outside))
          (should (file-exists-p (concat outside "sentinel.txt")))
          ;; Ownership + durable record RETAINED for a later (safe) retry.
          (should (gethash canon dorg-test--owned-roots))
          (should (file-exists-p record)))
      (ignore-errors (delete-file (directory-file-name canon)))
      (ignore-errors (delete-directory outside t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; The guard refuses an OBSERVED identity mismatch at the same path.  The
;; fixture keeps the original allocated while creating the replacement, rather
;; than assuming delete+recreate cannot reuse a device+inode tuple.
(ert-deftest dorg-test-owned-root-cleanup-refuses-distinct-replacement ()
  "A distinct directory installed at a registered path makes cleanup REFUSE.
The fixture observes unequal identities before the real
`dorg-test--delete-owned-root' call.  The replacement and foreign bytes survive
without a store scan, reap, or recursive delete, while ownership and its
durable record remain.  This proves mismatch refusal, not historical identity."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-recreate-reg-" t))
         (canon (dorg-test--make-owned-temp-dir "dorg-test-recreate-"))
         (record (expand-file-name
                  (concat (dorg-test--registry-root-key canon) ".eld")
                  dorg-test--registry-dir))
         (registered-id (gethash canon dorg-test--owned-roots))
         (swap (dorg-test-infra--install-distinct-directory canon))
         (preserved (plist-get swap :preserved))
         (scans 0)
         (reaps 0)
         (deletes 0)
         (real-delete (symbol-function 'delete-directory)))
    (unwind-protect
        (progn
          (should (equal registered-id (plist-get swap :original-id)))
          (should-not (equal registered-id (plist-get swap :replacement-id)))
          (should (equal (plist-get swap :replacement-id)
                         (dorg-test--path-file-identity canon)))
          (with-temp-file (concat canon "foreign.txt") (insert "foreign"))
          (cl-letf (((symbol-function 'dorg-test--owned-store-dirs)
                     (lambda (&rest _) (cl-incf scans) nil))
                    ((symbol-function 'dorg-test--owned-procs-under)
                     (lambda (&rest _) (cl-incf scans) nil))
                    ((symbol-function 'dorg-test--reap-owned-store)
                     (lambda (&rest _) (cl-incf reaps) 'gone))
                    ((symbol-function 'delete-directory)
                     (lambda (&rest args)
                       (cl-incf deletes)
                       (apply real-delete args))))
            (should (eq 'root-identity-mismatch
                        (dorg-test--delete-owned-root canon))))
          ;; The distinct replacement + content and original object survive.
          (should (file-directory-p canon))
          (should (file-exists-p (concat canon "foreign.txt")))
          (should (file-directory-p preserved))
          (should (= 0 scans))
          (should (= 0 reaps))
          (should (= 0 deletes))
          (should (gethash canon dorg-test--owned-roots))
          (should (file-exists-p record)))
      (ignore-errors (delete-directory (directory-file-name canon) t))
      (ignore-errors (delete-directory preserved t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC REFINE 06:00 (scope guard): a root we own is ALWAYS a directory.
;; `dorg-test--path-file-identity' requires a real non-symlink directory, which
;; unifies both ownership gates. Entry-time: a REGULAR FILE captures a nil
;; identity, so registration FAILS LOUD before any tracking (a file registered
;; as a root would later make `delete-owned-root' skip `file-directory-p',
;; unregister, and falsely return `gone' over a present file).
(ert-deftest dorg-test-register-owned-root-refuses-regular-file ()
  "`dorg-test--register-owned-root' REFUSES a present path that is a REGULAR
FILE: `dorg-test--path-file-identity' returns nil for a non-directory, so the
fail-loud nil-identity guard signals BEFORE any `puthash'/durable write — no
file ever enters the ownership registry.  Non-vacuous against a
`path-file-identity' that returns a regular file's inode identity: registration
would succeed and track the file as an owned root."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-regfile-reg-" t))
         (base (file-name-as-directory
                (file-truename (make-temp-file "dorg-regfile-b-" t))))
         (file (concat base "a-regular-file")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not a directory"))
          (should (file-exists-p file))
          (should-not (file-directory-p file))
          (should-error (dorg-test--register-owned-root
                         (file-name-as-directory file)))
          ;; Nothing tracked, and the file is untouched (refused before write).
          (should (= 0 (hash-table-count dorg-test--owned-roots)))
          (should (file-exists-p file))
          (should-not (file-directory-p file)))
      (ignore-errors (delete-directory base t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC REFINE 06:03 (test audit): the substantive false-cleanup case is a
;; REGULAR FILE already present in `--owned-roots' carrying its OWN current
;; identity — a legacy/buggy registration (or an inode reused across a
;; kind swap) where the inode DID NOT change, so identity-intact-p cannot
;; catch it by inode alone. Pre-fix: reap scans, delete SKIPS on
;; `file-directory-p' nil, cleanup UNREGISTERS and returns `gone' over a
;; present file (false cleanup). Fixed: `path-file-identity' returns nil for
;; the non-directory, identity-intact-p fails closed, and cleanup PRESERVES.
(ert-deftest dorg-test-owned-root-cleanup-refuses-regular-file-replacement ()
  "A REGULAR FILE seeded into `--owned-roots' with its OWN current filesystem
identity (a legacy/buggy registration that captured a file's inode, or an inode
reused across a directory→file swap) makes `dorg-test--delete-owned-root'
REFUSE (`root-identity-mismatch'): `dorg-test--path-file-identity' returns nil
for the non-directory, so `identity-intact-p' fails closed — the file is neither
scanned nor deleted, and ownership + the durable record are RETAINED.
Non-vacuous against a pre-fix `path-file-identity' that returns the file's inode
identity: the gate would PASS, the delete would SKIP on `file-directory-p' nil,
and cleanup would UNREGISTER + return `gone' — falsely certifying the present
file as cleaned."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-regseed-reg-" t))
         (base (file-name-as-directory
                (file-truename (make-temp-file "dorg-regseed-b-" t))))
         (canon (file-name-as-directory (concat base "root")))
         (file (directory-file-name canon))
         ;; Compute the file's REAL identity WITHOUT the function under test, so
         ;; the seed faithfully models a pre-fix register that captured a
         ;; regular file's OWN inode identity (the inode does not change).
         (file-id nil)
         (record (expand-file-name
                  (concat (dorg-test--registry-root-key canon) ".eld")
                  dorg-test--registry-dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "a regular file posing as a root"))
          (setq file-id (file-attribute-file-identifier (file-attributes file)))
          (should file-id)              ; the file HAS a captured-able identity
          ;; Seed legacy/buggy ownership: in-memory hash VALUE + durable
          ;; :root-id BOTH equal the file's OWN current identity.
          (puthash canon file-id dorg-test--owned-roots)
          (dorg-test--registry-write
           (dorg-test--registry-root-key canon)
           (list :owner-pid (emacs-pid) :root canon :root-id file-id))
          (should (file-exists-p record))
          ;; The load-bearing invariant: a non-directory has NO stable identity.
          (should (null (dorg-test--path-file-identity canon)))
          ;; identity-intact-p fails closed even though the seeded EXPECTED id
          ;; equals the file's own inode (current identity is now nil).
          (should-not (dorg-test--identity-intact-p canon file-id))
          (should (eq 'root-identity-mismatch
                      (dorg-test--delete-owned-root canon)))
          ;; The file survives (never scanned or deleted) ...
          (should (file-exists-p file))
          (should-not (file-directory-p file))
          ;; ... and ownership + the durable record are RETAINED for a retry.
          (should (gethash canon dorg-test--owned-roots))
          (should (file-exists-p record)))
      (ignore-errors (delete-directory base t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC 03:08 (4): normal reap routes through exact identity + confirm; a
;; still-live server after the kill is NOT declared gone (port-down alone
;; never confirms).
(ert-deftest dorg-test-reap-owned-store-preserves-on-unconfirmed-kill ()
  "Normal `dorg-test--reap-owned-store' routes cleanup through EXACT identity
match + `dorg-test--confirm-server-gone': a recorded server whose pid stays
LIVE after the kill (still-live) is NOT declared gone — the claim and durable
store record are PRESERVED and a non-`gone' status returned, so no caller
deletes the root.  A port going down is NOT accepted as confirmation while the
pid may remain live.  Non-vacuous against a reap that declares `gone' from
port-down alone."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--server-proc nil)
         (dorg-test--server-port nil)
         (root (dorg-test--canonical-path "/tmp/dorg-test-reapstore/" t))
         (store (concat root "s/"))
         (store-canon (dorg-test--canonical-path store t))
         (claim-deleted nil) (kills nil))
    (puthash root t dorg-test--owned-roots)
    (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
               (lambda (d) (dorg-test--canonical-path d t)))
              ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--claim-read)
               (lambda (_c) '((:server-pid . 8181) (:port . 7070))))
              ((symbol-function 'dorg-dolt-server--dolt-server-info)
               (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
              ((symbol-function 'process-attributes)
               (lambda (_p) `((comm . "dolt")
                              (args . ,(format
                                        "dolt sql-server --port 7070 --data-dir %s"
                                        store-canon)))))
              ((symbol-function 'dorg-dolt-server--kill-pid)
               (lambda (pid &optional _g) (push pid kills) t))
              ((symbol-function 'dorg-dolt-server--claim-delete-file)
               (lambda (&rest _) (setq claim-deleted t)))
              ((symbol-function 'dorg-test--registry-delete) (lambda (&rest _) nil)))
      (let ((status (dorg-test--reap-owned-store store)))
        ;; Kill attempted on the exact match, but the pid stayed live -> not
        ;; confirmed gone -> claim + record PRESERVED, non-gone status.
        (should (equal kills '(8181)))
        (should-not (eq status 'gone))
        (should-not claim-deleted)))))

;; OC 03:08 (3): a discovered metadata-bearing store with an UNREADABLE pid
;; BLOCKS deletion (snapshot-once, never collapses to clear); a metadata-FREE
;; sibling root still cleans.
(ert-deftest dorg-test-stale-registry-metadata-store-unknown-pid-preserves ()
  "A dead-owner root record whose root has a metadata-bearing store with an
UNREADABLE pid BLOCKS deletion: the store can never be confirmed gone, so the
record + root are PRESERVED (never a silent consume that strands an orphan).
A sibling metadata-FREE root record in the same run still cleans and is
consumed.  The identities are snapshot ONCE before Phase 1, so an
unreadable-pid store never collapses to an empty/clear set.  Non-vacuous
against a reaper that drops a metadata store with no pid (treating the record
as clear) or that consumes on a re-read empty scan."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-cn-unknown-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (meta-root (concat tmp "dorg-test-meta/"))
         (store (concat meta-root "s/"))
         (free-root (concat tmp "dorg-test-free/")))
    (unwind-protect
        (progn
          (make-directory (concat store ".dolt/") t)
          (make-directory free-root t)
          (make-directory dorg-test--registry-dir t)
          ;; Metadata present but the pid is unreadable (garbage info file).
          (with-temp-file (concat store ".dolt/sql-server.info") (insert "not-a-pid"))
          (with-temp-file (expand-file-name "root-222-meta.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root meta-root
                         :root-id (dorg-test--path-file-identity meta-root))
                   (current-buffer)))
          (with-temp-file (expand-file-name "root-222-free.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root free-root
                         :root-id (dorg-test--path-file-identity free-root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil)))            ; owner 222 dead
            (let ((temporary-file-directory tmp))
              (dorg-test--reap-stale-server-registry)))
          ;; Metadata store, unknown pid -> record + root PRESERVED.
          (should (file-exists-p
                   (expand-file-name "root-222-meta.eld" dorg-test--registry-dir)))
          (should (file-directory-p meta-root))
          ;; Metadata-free root -> cleaned + consumed.
          (should-not (file-directory-p free-root))
          (should-not (file-exists-p
                       (expand-file-name "root-222-free.eld" dorg-test--registry-dir))))
      (delete-directory base t))))

;; OC 03:22 (2): a store discovered via `sql-server.info' with no claim is
;; still confirmed by exact identity — never declared gone over a live pid.
(ert-deftest dorg-test-reap-owned-store-info-only-blocks-over-live-pid ()
  "A store whose server identity comes ONLY from `.dolt/sql-server.info' (the
DOrg claim is absent/read-failed) is still confirmed by EXACT identity: a LIVE
pid that is not matched/killed leaves a non-`gone' status, so the root is never
deleted over it.  Non-vacuous against a reap that reads only the claim and,
finding none, declares the store `gone' over a live server."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--server-proc nil)
         (dorg-test--server-port nil)
         (root (dorg-test--canonical-path "/tmp/dorg-test-infoonly/" t))
         (store (concat root "s/"))
         (kills nil))
    (puthash root t dorg-test--owned-roots)
    (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
               (lambda (d) (dorg-test--canonical-path d t)))
              ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
              ;; No claim; the identity comes from `sql-server.info' alone.
              ((symbol-function 'dorg-dolt-server--claim-read) (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--dolt-server-info)
               (lambda (&rest _) '(:pid 9191 :port 5050)))
              ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
              ;; Live pid, but its identity does NOT match (non-dolt process).
              ((symbol-function 'process-attributes)
               (lambda (_p) '((comm . "unrelated") (args . "unrelated"))))
              ((symbol-function 'dorg-dolt-server--kill-pid)
               (lambda (pid &optional _g) (push pid kills) t)))
      (let ((status (dorg-test--reap-owned-store store)))
        ;; A live, unmatched server derived from info alone -> NOT gone.
        (should-not (eq status 'gone))
        (should (null kills))))))

;; OC 03:22 (3): durable record delete is checked BEFORE the in-memory drop;
;; a failure surfaces and retains in-memory ownership for retry.
(ert-deftest dorg-test-unregister-owned-root-surfaces-record-delete-failure ()
  "`dorg-test--unregister-owned-root' deletes the durable record FIRST
(checked) and drops in-memory ownership only AFTER: a `registry-delete'
failure PROPAGATES and RETAINS the in-memory `--owned-roots' entry for retry.
Non-vacuous against a remhash-first + `ignore-errors' delete that drops
in-memory ownership while the durable record leaks."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (root (dorg-test--canonical-path "/tmp/dorg-test-unreg/" t)))
    (puthash root t dorg-test--owned-roots)
    (cl-letf (((symbol-function 'dorg-test--registry-delete)
               (lambda (&rest _) (error "spec: record delete wedged"))))
      (should-error (dorg-test--unregister-owned-root root))
      ;; In-memory ownership retained for retry.
      (should (gethash root dorg-test--owned-roots)))))

;; OC 03:22 (3): a fail-loud persist must not leak the newly-created temp dir.
(ert-deftest dorg-test-make-owned-temp-dir-deletes-dir-on-registration-failure ()
  "`dorg-test--make-owned-temp-dir' deletes the EXACT temp directory it created
when durable registration fails, so a fail-loud persist never leaks an
untracked on-disk root.  Non-vacuous against a create-then-register that
leaves the directory behind on a persist failure."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (created nil))
    (cl-letf* ((real-mtf (symbol-function 'make-temp-file))
               ((symbol-function 'make-temp-file)
                (lambda (&rest args) (setq created (apply real-mtf args)) created))
               ((symbol-function 'dorg-test--registry-write)
                (lambda (&rest _) (error "spec: persist failed"))))
      (should-error (dorg-test--make-owned-temp-dir "dorg-test-mtf-leak-"))
      (should created)
      (should-not (file-exists-p created))
      (should (= 0 (hash-table-count dorg-test--owned-roots))))))

;; OC 02:51: the de-duplication leaves NO in-memory owned-server shadow and NO
;; publish advice behind.
(ert-deftest dorg-test-fixture-no-owned-server-shadow-or-publish-advice ()
  "The de-duplication leaves NO in-memory owned-server shadow and NO
publish-claim advice behind: the removed symbols are unbound (neither `fboundp'
nor `boundp').  Non-vacuous: reintroducing any shadow hash or publish advice
binds one of these and fails here."
  (dolist (sym '(dorg-test--owned-server-records
                 dorg-test--record-owned-server
                 dorg-test--reap-owned-root-records
                 dorg-test--publish-claim-advice))
    (should-not (fboundp sym))
    (should-not (boundp sym))))

;; OC 03:29 (1): reap snapshots the identity BEFORE `--stop', so a partial stop
;; that wipes metadata yet leaves the pid live cannot degrade to `no server'.
(ert-deftest dorg-test-reap-owned-store-snapshots-identity-before-stop ()
  "`dorg-test--reap-owned-store' reads the on-disk identity BEFORE calling
`dorg-dolt-server--stop': the claim read is ORDERED before the stop, and a
partial stop that wipes the metadata yet leaves the pid LIVE still BLOCKS
(the snapshotted live pid is confirmed and fails).  Non-vacuous against a reap
that reads the identity AFTER the stop (seeing nothing) and returns gone over a
live server."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-dolt-server--procs (make-hash-table :test 'equal))
         (dorg-test--server-proc nil)
         (dorg-test--server-port nil)
         (root (dorg-test--canonical-path "/tmp/dorg-test-snap/" t))
         (store (concat root "s/"))
         (store-canon (dorg-test--canonical-path store t))
         (metadata-present t) (order nil))
    (puthash root t dorg-test--owned-roots)
    (puthash store-canon (cons 'proc 6161) dorg-dolt-server--procs)
    (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
               (lambda (d) (dorg-test--canonical-path d t)))
              ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--stop)
               (lambda (&rest _) (push 'stop order) (setq metadata-present nil) nil))
              ((symbol-function 'dorg-dolt-server--claim-read)
               (lambda (&rest _)
                 (push 'read order)
                 (and metadata-present '((:server-pid . 6161) (:port . 4040)))))
              ((symbol-function 'dorg-dolt-server--dolt-server-info) (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) t))
              ((symbol-function 'process-attributes)
               (lambda (_p) '((comm . "unrelated") (args . "unrelated"))))
              ((symbol-function 'dorg-dolt-server--kill-pid)
               (lambda (pid &optional _g) pid t)))
      (let ((status (dorg-test--reap-owned-store store)))
        ;; Identity read BEFORE stop, and the live pid blocks.
        (should (equal (reverse order) '(read stop)))
        (should-not (eq status 'gone))))))

;; OC 03:29 (2): both on-disk identities are captured; a dead overlay + a live
;; mismatched native pid must PRESERVE (require ALL gone).
(ert-deftest dorg-test-reap-owned-store-claim-native-disagreement-preserves ()
  "When the DOrg claim (overlay) names a DEAD pid but `sql-server.info' names a
DIFFERENT LIVE pid/port, `reap-owned-store' captures BOTH identities and
requires ALL gone: the live native process blocks, so the claim/root are
PRESERVED.  Non-vacuous against a reap that confirms only the (dead) overlay
and deletes metadata over the live native process."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--server-proc nil)
         (dorg-test--server-port nil)
         ;; The Actions probe must carry the decision row without enabling the
         ;; side-effectful trace oracle for unrelated real cleanup paths.
         (dorg-test--reap-owned-store-trace t)
         (dorg-test--reap-owned-store-trace-stubs nil)
         (dorg-test--reap-owned-store-trace-current-test
          'dorg-test-reap-owned-store-claim-native-disagreement-preserves)
         (root (dorg-test--canonical-path "/tmp/dorg-test-disagree/" t))
         (store (concat root "s/"))
         (claim-deleted nil) (kills nil))
    (puthash root t dorg-test--owned-roots)
    (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--canonical-dir
                (lambda (d) (dorg-test--canonical-path d t))))
              ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
              ((symbol-function 'dorg-dolt-server--claim-read)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--claim-read
                (lambda (&rest _) '((:server-pid . 100) (:port . 4000)))))
              ((symbol-function 'dorg-dolt-server--dolt-server-info)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--dolt-server-info
                (lambda (&rest _) '(:pid 200 :port 4001))))
              ((symbol-function 'dorg-dolt-server--pid-live-p)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--pid-live-p
                (lambda (pid) (and (eql pid 200) t)))) ; 100 dead, 200 live
              ((symbol-function 'process-attributes)
               (dorg-test--reap-owned-store-mark-stub
                'process-attributes
                (lambda (_p) '((comm . "unrelated") (args . "unrelated")))))
              ((symbol-function 'dorg-dolt-server--kill-pid)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--kill-pid
                (lambda (pid &optional _g) (push pid kills) t)))
              ((symbol-function 'dorg-dolt-server--claim-delete-file)
               (dorg-test--reap-owned-store-mark-stub
                'dorg-dolt-server--claim-delete-file
                (lambda (&rest _) (setq claim-deleted t)))))
      (let ((status (dorg-test--reap-owned-store store)))
        (should-not (eq status 'gone))
        (should-not claim-deleted)))))

(defmacro dorg-test-infra--with-fake-shared-pid (pid var &rest body)
  "Run BODY with VAR bound to a live non-Dolt process whose pid reads as PID."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((,var (start-process "dorg-test-fake-shared" nil "sleep" "30")))
     (unwind-protect
         (cl-letf (((symbol-function 'process-id) (lambda (_p) ,pid)))
           ,@body)
       (ignore-errors (delete-process ,var)))))

(defun dorg-test-infra--run-ert-body (test)
  "Run TEST's ERT body directly under the caller's dynamic bindings."
  (funcall (ert-test-body (ert-get-test test))))

(ert-deftest dorg-test-reap-owned-store-claim-native-disagreement-isolated-from-shared-port ()
  "The real disagreement test survives injected shared-server port collision.
Mutation control: removing the test's inner shared-server nil bindings makes the
original `(should-not (eq status 'gone))' assertion fail as `(eq gone gone)'."
  (let ((dorg-test--server-port 4001))
    (dorg-test-infra--run-ert-body
     'dorg-test-reap-owned-store-claim-native-disagreement-preserves)))

(ert-deftest dorg-test-reap-owned-store-claim-native-disagreement-isolated-from-shared-pid ()
  "The real disagreement test survives injected shared-server pid collision."
  (dorg-test-infra--with-fake-shared-pid 200 fake
    (let ((dorg-test--server-proc fake))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-claim-native-disagreement-preserves))))

(ert-deftest dorg-test-reap-owned-store-claim-native-disagreement-noncolliding-shared-control ()
  "The real disagreement test still preserves under non-colliding shared state."
  (dorg-test-infra--with-fake-shared-pid 201 fake
    (let ((dorg-test--server-proc fake)
          (dorg-test--server-port 4002))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-claim-native-disagreement-preserves))))

(ert-deftest dorg-test-reap-owned-store-snapshot-identity-isolated-from-shared-port ()
  "The snapshot-before-stop sibling survives injected shared-server port collision."
  (let ((dorg-test--server-port 4040))
    (dorg-test-infra--run-ert-body
     'dorg-test-reap-owned-store-snapshots-identity-before-stop)))

(ert-deftest dorg-test-reap-owned-store-snapshot-identity-isolated-from-shared-pid ()
  "The snapshot-before-stop sibling survives injected shared-server pid collision."
  (dorg-test-infra--with-fake-shared-pid 6161 fake
    (let ((dorg-test--server-proc fake))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-snapshots-identity-before-stop))))

(ert-deftest dorg-test-reap-owned-store-snapshot-identity-noncolliding-shared-control ()
  "The snapshot-before-stop sibling still preserves under non-colliding shared state."
  (dorg-test-infra--with-fake-shared-pid 6162 fake
    (let ((dorg-test--server-proc fake)
          (dorg-test--server-port 4041))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-snapshots-identity-before-stop))))

(ert-deftest dorg-test-reap-owned-store-unconfirmed-kill-isolated-from-shared-port ()
  "The unconfirmed-kill synthetic test survives injected shared-server port collision."
  (let ((dorg-test--server-port 7070))
    (dorg-test-infra--run-ert-body
     'dorg-test-reap-owned-store-preserves-on-unconfirmed-kill)))

(ert-deftest dorg-test-reap-owned-store-unconfirmed-kill-isolated-from-shared-pid ()
  "The unconfirmed-kill synthetic test survives injected shared-server pid collision."
  (dorg-test-infra--with-fake-shared-pid 8181 fake
    (let ((dorg-test--server-proc fake))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-preserves-on-unconfirmed-kill))))

(ert-deftest dorg-test-reap-owned-store-unconfirmed-kill-noncolliding-shared-control ()
  "The unconfirmed-kill synthetic test still preserves under non-colliding shared state."
  (dorg-test-infra--with-fake-shared-pid 8182 fake
    (let ((dorg-test--server-proc fake)
          (dorg-test--server-port 7071))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-preserves-on-unconfirmed-kill))))

(ert-deftest dorg-test-reap-owned-store-info-only-isolated-from-shared-port ()
  "The info-only synthetic test survives injected shared-server port collision."
  (let ((dorg-test--server-port 5050))
    (dorg-test-infra--run-ert-body
     'dorg-test-reap-owned-store-info-only-blocks-over-live-pid)))

(ert-deftest dorg-test-reap-owned-store-info-only-isolated-from-shared-pid ()
  "The info-only synthetic test survives injected shared-server pid collision."
  (dorg-test-infra--with-fake-shared-pid 9191 fake
    (let ((dorg-test--server-proc fake))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-info-only-blocks-over-live-pid))))

(ert-deftest dorg-test-reap-owned-store-info-only-noncolliding-shared-control ()
  "The info-only synthetic test still preserves under non-colliding shared state."
  (dorg-test-infra--with-fake-shared-pid 9192 fake
    (let ((dorg-test--server-proc fake)
          (dorg-test--server-port 5051))
      (dorg-test-infra--run-ert-body
       'dorg-test-reap-owned-store-info-only-blocks-over-live-pid))))

;; OC 03:34 (A): raw overlay read + coalesce a provisional (nil-pid) overlay
;; into the native identity on a shared port; distinct ports stay separate.
(ert-deftest dorg-test-store-server-identities-coalesces-provisional-overlay ()
  "`dorg-test--store-server-identities' COALESCES a provisional overlay
\(`:server-pid' nil) into the native identity when they share a port — so
killing the native pid clears the store — while keeping overlay/native on
DIFFERENT ports DISTINCT (the incomplete overlay blocks on its own port).
Non-vacuous against a reader that leaves a nil-pid overlay identity that
permanently blocks even after the matching native pid is killed."
  (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
             (lambda (d) (dorg-test--canonical-path d t))))
    ;; Same port: provisional overlay (nil,7) coalesces into native (42,7).
    (cl-letf (((symbol-function 'dorg-dolt-server--claim-read)
               (lambda (&rest _) '((:server-pid . nil) (:port . 7))))
              ((symbol-function 'dorg-dolt-server--dolt-server-info)
               (lambda (&rest _) '(:pid 42 :port 7))))
      (let ((ids (dorg-test--store-server-identities "/tmp/dorg-coalesce/")))
        (should (= 1 (length ids)))
        (should (eql 42 (plist-get (car ids) :pid)))
        (should (eql 7 (plist-get (car ids) :port)))))
    ;; Different ports: both kept; the incomplete overlay (nil,8) blocks.
    (cl-letf (((symbol-function 'dorg-dolt-server--claim-read)
               (lambda (&rest _) '((:server-pid . nil) (:port . 8))))
              ((symbol-function 'dorg-dolt-server--dolt-server-info)
               (lambda (&rest _) '(:pid 42 :port 9))))
      (let ((ids (dorg-test--store-server-identities "/tmp/dorg-coalesce2/")))
        (should (= 2 (length ids)))
        (should (seq-find (lambda (id) (and (null (plist-get id :pid))
                                            (eql 8 (plist-get id :port))))
                          ids))
        (should (seq-find (lambda (id) (and (eql 42 (plist-get id :pid))
                                            (eql 9 (plist-get id :port))))
                          ids))))))

;; OC REFINE (correction 1): `dorg-test--store-server-identities' distinguishes
;; a store with NO metadata FILE (nil ids — genuinely serverless, cleanable)
;; from a store whose metadata FILE is present but unreadable/incomplete (ONE
;; blocking nil-pid identity).  Non-vacuous against a reader that always yields
;; a nil-pid identity for any canonical store (permanently blocking a
;; serverless dir) OR that collapses an unreadable metadata file to nil ids
;; (letting a metadata-bearing store clean).
(ert-deftest dorg-test-store-server-identities-distinguishes-metadata-file ()
  "No metadata file + nil readers -> nil ids (serverless).  A present-but-
unreadable metadata file (garbage `sql-server.info' or `server.claim') -> ONE
blocking nil-pid identity.  File presence is checked on disk, independent of
readability."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ssi-file-" t))))
         (bare (concat base "bare/"))         ; canonical dir, zero metadata
         (info-store (concat base "info/"))   ; unreadable sql-server.info
         (claim-store (concat base "claim/"))); unreadable .dorg/server.claim
    (unwind-protect
        (progn
          (make-directory bare t)
          (make-directory (concat info-store ".dolt/") t)
          (make-directory (concat claim-store ".dorg/") t)
          (with-temp-file (concat info-store ".dolt/sql-server.info")
            (insert "not-a-pid"))
          (with-temp-file (concat claim-store ".dorg/server.claim")
            (insert "{{ not a lisp form"))
          ;; No metadata file anywhere -> nil ids (serverless, cleanable).
          (should-not (dorg-test--store-server-identities bare))
          ;; Unreadable native info file present -> one blocking nil-pid id.
          (let ((ids (dorg-test--store-server-identities info-store)))
            (should (= 1 (length ids)))
            (should (null (plist-get (car ids) :pid)))
            (should (equal (dorg-test--canonical-path info-store t)
                           (plist-get (car ids) :dolt-dir))))
          ;; Unreadable DOrg claim overlay present -> one blocking nil-pid id.
          (let ((ids (dorg-test--store-server-identities claim-store)))
            (should (= 1 (length ids)))
            (should (null (plist-get (car ids) :pid)))))
      (delete-directory base t))))

;; OC REFINE (blocker 3): a DANGLING marker symlink is present malformed
;; metadata — `file-exists-p' follows it and reads it ABSENT, so presence must
;; OR in `file-symlink-p'.
(ert-deftest dorg-test-store-has-metadata-file-p-counts-dangling-symlink ()
  "A store whose only marker is a DANGLING `sql-server.info' symlink (target
absent) still counts as metadata-bearing: `dorg-test--store-has-metadata-file-p'
is non-nil and `dorg-test--store-server-identities' yields ONE blocking nil-pid
identity (never collapses to serverless).  Non-vacuous against a
`file-exists-p'-only probe that follows the dangling link and reads it absent."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-dangling-" t))))
         (store (concat base "s/"))
         (store-canon nil))
    (unwind-protect
        (progn
          (make-directory (concat store ".dolt/") t)
          ;; A DANGLING symlink at the info marker path (target does not exist).
          (make-symbolic-link (concat base "no-such-target")
                              (concat store ".dolt/sql-server.info"))
          (setq store-canon (dorg-test--canonical-path store t))
          (should (dorg-test--store-has-metadata-file-p store-canon))
          (let ((ids (dorg-test--store-server-identities store)))
            (should (= 1 (length ids)))
            (should (null (plist-get (car ids) :pid)))))
      (delete-directory base t))))

;; tracked-case: a SYNTHETIC/never-served overlay claim leaves a permanently
;; unreapable dead-owner record.  A `dorg-test--write-claim'-style provisional
;; overlay (`:server-pid' nil, `:port' nil) is a READABLE `.dorg/server.claim'
;; with no native `.dolt/sql-server.info' and no live process — pre-fix
;; `store-server-identities' emitted a BLOCKING nil-pid identity on metadata-file
;; presence alone, so the reaper returned `server-unconfirmed' forever.  The fix
;; relaxes ONLY this readable-provisional case, and ONLY when the process table
;; PROVES absence (`none'); every fail-closed guard below still blocks.
(defun dorg-test--ukfp6-write-provisional-claim (store)
  "Write a readable PROVISIONAL overlay claim (no pid/port) into STORE.
Mirrors the synthetic `dorg-test--write-claim' fixture: a parseable
`<store>/.dorg/server.claim' whose `:server-pid'/`:port' are nil and which has
NO accompanying native `.dolt/sql-server.info' or `.dorg/server.pid' marker.
Returns STORE's canonical dir."
  (let* ((canon (dorg-test--canonical-path store t))
         (dorg-dir (concat canon ".dorg/")))
    (make-directory dorg-dir t)
    (with-temp-file (concat dorg-dir "server.claim")
      (prin1 (list (cons :dolt-dir (directory-file-name canon))
                   (cons :host (system-name))
                   (cons :owner-pid 999999)
                   (cons :server-pid nil)
                   (cons :port nil)
                   (cons :owner-kind 'emacs)
                   (cons :token "foreign-token")
                   (cons :policy 'server))
             (current-buffer)))
    canon))

(ert-deftest dorg-test-store-server-identities-recovers-synthetic-provisional-claim ()
  "AC1: a store whose ONLY metadata is a READABLE provisional overlay claim
\(`:server-pid'/`:port' nil), with no native info / `server.pid' marker and NO
live `dolt' process under it (evidence `none'), returns NIL — serverless and
reapable.  Non-vacuous against the pre-fix reader that yields ONE blocking
nil-pid identity for any metadata-bearing store (permanently blocking reap)."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-ssi-" t))))
         (store (concat base "s/")))
    (unwind-protect
        (progn
          (dorg-test--ukfp6-write-provisional-claim store)
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (_dir) 'none)))
            (should (null (dorg-test--store-server-identities store)))))
      (delete-directory base t))))

(ert-deftest dorg-test-store-server-identities-synthetic-claim-live-process-blocks ()
  "AC2: the SAME readable provisional claim BLOCKS (one nil-pid identity) when
process evidence is `live' — a real (or just-forked) server may exist.
Non-vacuous against a fix that relaxes on the claim shape alone."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-live-" t))))
         (store (concat base "s/")))
    (unwind-protect
        (let ((canon (dorg-test--ukfp6-write-provisional-claim store)))
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (_dir) 'live)))
            (let ((ids (dorg-test--store-server-identities store)))
              (should (= 1 (length ids)))
              (should (null (plist-get (car ids) :pid)))
              (should (equal canon (plist-get (car ids) :dolt-dir))))))
      (delete-directory base t))))

(ert-deftest dorg-test-store-server-identities-synthetic-claim-unknown-scan-blocks ()
  "AC3: the SAME readable provisional claim BLOCKS when process evidence is
`unknown' (an inconclusive scan) — reviewer no-fail-open: an incomplete scan
must never read as absence.  Non-vacuous against relaxing on non-`live' alone."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-unk-" t))))
         (store (concat base "s/")))
    (unwind-protect
        (progn
          (dorg-test--ukfp6-write-provisional-claim store)
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (_dir) 'unknown)))
            (let ((ids (dorg-test--store-server-identities store)))
              (should (= 1 (length ids)))
              (should (null (plist-get (car ids) :pid))))))
      (delete-directory base t))))

(ert-deftest dorg-test-store-server-identities-synthetic-claim-native-marker-blocks ()
  "AC4/AC4b: a corroborating server marker keeps the block even with process
evidence `none': (a) provisional overlay PLUS a native `.dolt/sql-server.info'
marker present; (b) an UNREADABLE `.dorg/server.claim' (garbage, not a parsed
provisional overlay).  Non-vacuous against a relaxation that ignores the extra
marker / accepts an unreadable claim."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-marker-" t))))
         (with-native (concat base "a/"))
         (unreadable (concat base "b/")))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                   (lambda (_dir) 'none)))
          ;; (a) readable provisional overlay + native info MARKER present.
          (let ((canon (dorg-test--ukfp6-write-provisional-claim with-native)))
            (make-directory (concat canon ".dolt/") t)
            (with-temp-file (concat canon ".dolt/sql-server.info") (insert "garbage"))
            (let ((ids (dorg-test--store-server-identities with-native)))
              (should (= 1 (length ids)))
              (should (null (plist-get (car ids) :pid)))))
          ;; (b) UNREADABLE overlay claim (not a parsed provisional).
          (make-directory (concat unreadable ".dorg/") t)
          (with-temp-file (concat unreadable ".dorg/server.claim")
            (insert "{{ not a lisp form"))
          (let ((ids (dorg-test--store-server-identities unreadable)))
            (should (= 1 (length ids)))
            (should (null (plist-get (car ids) :pid)))))
      (delete-directory base t))))

(ert-deftest dorg-test-stale-registry-reaps-synthetic-claim-root ()
  "AC5 (the bug): a dead-owner ROOT record whose identity-intact eligible root
contains ONLY a synthetic provisional-claim store and NO live dolt process is
REAPED — `reap-stale-server-registry' returns NO failure, deletes the root, and
consumes the record.  Non-vacuous against the pre-fix reaper that returns
`server-unconfirmed' and retains both the root and the record forever."
  (let* ((dir (make-temp-file "dorg-ukfp6-registry-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-ukfp6-root-" t)))
         (recfile (expand-file-name "root-synthetic.eld" dir)))
    (unwind-protect
        (progn
          ;; Owned-root record shape (register-owned-root: no :dolt-dir), then a
          ;; synthetic overlay-claim store dropped UNDER the root (root inode
          ;; unchanged, so :root-id still matches).
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :root root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (dorg-test--ukfp6-write-provisional-claim (concat root "s/"))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))          ; owner 222 dead
                    ((symbol-function 'dorg-dolt-server--port-live-p)
                     (lambda (_port) nil))
                    ((symbol-function 'list-system-processes)
                     (lambda () nil)))             ; -> live evidence `none'
            (should (null (dorg-test--reap-stale-server-registry))))
          (should-not (file-directory-p root))
          (should-not (file-exists-p recfile))
          (should (null (directory-files dir nil "\\.eld\\'"))))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

(ert-deftest dorg-test-reap-owned-store-clears-synthetic-claim ()
  "AC6 (teardown crash-net): `reap-owned-store' on a synthetic provisional-claim
store under an owned root, with no `--procs' entry and process evidence `none',
returns `gone', deletes the claim, AND durably deletes the store's ownership
record — driving the \"owned-root-crash-net ... reap(s) unconfirmed\" false
failure to green.

Load-bearing durable-deletion assertion (reviewer reject fix): the store's
`server-<pid>-<md5>.eld' record is written to a REAL `dorg-test--registry-dir'
and its file MUST be gone after the reap.  `dorg-test--registry-delete' is NOT
stubbed to a no-op — it is spied while performing the REAL delete, so the test
asserts BOTH that it was called with the store key AND that the durable record
was actually removed from disk.

Non-vacuous against the pre-fix nil-pid block, which returns `unknown', never
reaches the `(eq status 'gone)' branch, preserves the claim, and NEVER calls
`registry-delete' — leaving `recfile' on disk so the durable-deletion assertion
FAILS."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-reap-" t))))
         (root (concat base "dorg-test-root/"))
         (store (concat root "s/"))
         (regdir (file-name-as-directory (make-temp-file "dorg-ukfp6-reg-" t)))
         (orig-registry-delete (symbol-function 'dorg-test--registry-delete)))
    (unwind-protect
        (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
               (dorg-dolt-server--procs (make-hash-table :test 'equal))
               (dorg-test--server-proc nil)
               (dorg-test--registry-dir regdir)
               (canon (dorg-test--ukfp6-write-provisional-claim store))
               (store-key (dorg-test--registry-store-key canon))
               (recfile (expand-file-name (concat store-key ".eld") regdir))
               (deleted nil)
               (delete-keys nil))
          (puthash (dorg-test--canonical-path root t) t dorg-test--owned-roots)
          ;; A REAL durable ownership record for this store on disk (mirrors the
          ;; crash-net debt a live-owner teardown must clear).
          (dorg-test--registry-write
           store-key (list :owner-pid (emacs-pid) :dolt-dir canon))
          (should (file-exists-p recfile))      ; precondition: record present
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (_dir) 'none))
                    ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
                    ((symbol-function 'dorg-dolt-server--claim-delete-file)
                     (lambda (&rest _) (setq deleted t)))
                    ;; Spy the REAL delete: record the key, then actually delete.
                    ((symbol-function 'dorg-test--registry-delete)
                     (lambda (key)
                       (push key delete-keys)
                       (funcall orig-registry-delete key))))
            (should (eq 'gone (dorg-test--reap-owned-store canon)))
            (should deleted)                          ; claim file deleted
            (should (member store-key delete-keys))   ; registry-delete CALLED w/ key
            (should-not (file-exists-p recfile))))    ; durable record actually GONE
      (delete-directory base t)
      (delete-directory regdir t))))

;; tracked-case REFINE (reviewer ACCEPT reject 47e4c20): the incident class ALSO
;; contains synthetic overlays with a nil server pid but a NON-nil ephemeral
;; `:port' (the 2026-07-19 roots dorg-test-store-MK4Y2A @61104 and .../fjvpBq
;; @61107).  A port-bearing overlay builds a READABLE `(nil . PORT)' identity, so
;; pre-fix `store-server-identities' lands in the `ids' branch and emits a
;; BLOCKING nil-pid identity that can never `confirm-server-gone' — the block the
;; port-less relaxation never touched.  The generalized predicate reaps a DEAD
;; port (no listener) while a LIVE port re-blocks.  Real TCP listeners exercise
;; the actual `dorg-dolt-server--port-live-p' probe (no stub), so the block/reap
;; transition is driven by real ground truth, not a mocked port verdict.
(defun dorg-test--ukfp6-write-provisional-claim-port (store port)
  "Write a readable PROVISIONAL overlay claim with nil pid but PORT into STORE.
Mirrors the port-bearing synthetic `dorg-test--write-claim' incident shape: a
parseable `<store>/.dorg/server.claim' with `:server-pid' nil and a non-nil
`:port', no accompanying native `.dolt/sql-server.info' or `.dorg/server.pid'
marker.  Returns STORE's canonical dir."
  (let* ((canon (dorg-test--canonical-path store t))
         (dorg-dir (concat canon ".dorg/")))
    (make-directory dorg-dir t)
    (with-temp-file (concat dorg-dir "server.claim")
      (prin1 (list (cons :dolt-dir (directory-file-name canon))
                   (cons :host (system-name))
                   (cons :owner-pid 999999)
                   (cons :server-pid nil)
                   (cons :port port)
                   (cons :owner-kind 'emacs)
                   (cons :token "foreign-token")
                   (cons :policy 'server))
             (current-buffer)))
    canon))

(defun dorg-test--ukfp6-open-ephemeral-listener ()
  "Open a real TCP server on 127.0.0.1 at an OS-assigned port.
Returns (PROC . PORT).  The caller must `delete-process' PROC.  Used to drive
`dorg-dolt-server--port-live-p' with a genuine live/dead port transition rather
than a stub."
  (let ((proc (make-network-process
               :name "dorg-ukfp6-listener" :server t :host 'local
               :service t :family 'ipv4 :noquery t)))
    (cons proc (process-contact proc :service))))

(ert-deftest dorg-test-store-server-identities-recovers-synthetic-claim-dead-port ()
  "AC1b (port-bearing bug): a store whose ONLY metadata is a READABLE provisional
overlay with `:server-pid' nil but a NON-nil DEAD `:port' (no TCP listener), no
native info / `server.pid' marker, and process evidence `none' RETURNS nil —
serverless and reapable.  Non-vacuous against the pre-fix reader, which builds a
readable `(nil . PORT)' overlay identity and returns ONE blocking nil-pid
identity bearing that port (permanently unreapable)."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-dport-" t))))
         (store (concat base "s/"))
         ;; An ephemeral port opened then IMMEDIATELY closed -> reliably DEAD but
         ;; recently valid, mirroring a listener whose owner has since died.
         (pp (dorg-test--ukfp6-open-ephemeral-listener))
         (port (cdr pp)))
    (delete-process (car pp))
    (unwind-protect
        (progn
          (should (integerp port))
          (should-not (dorg-dolt-server--port-live-p port)) ; precondition: dead
          (dorg-test--ukfp6-write-provisional-claim-port store port)
          (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                     (lambda (_dir) 'none)))
            (should (null (dorg-test--store-server-identities store)))))
      (delete-directory base t))))

(ert-deftest dorg-test-store-server-identities-synthetic-claim-live-port-blocks ()
  "AC2b (fail-closed): the SAME port-bearing provisional overlay BLOCKS (one
nil-pid identity carrying the port) while a REAL TCP listener is accepting on the
claimed port — a bound orphan may still be serving.  The block is driven by the
genuine `dorg-dolt-server--port-live-p' probe against a live socket, not a stub.
Non-vacuous against a fix that relaxes on the claim shape alone."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-ukfp6-lport-" t))))
         (store (concat base "s/"))
         (pp (dorg-test--ukfp6-open-ephemeral-listener))
         (port (cdr pp)))
    (unwind-protect
        (progn
          (should (dorg-dolt-server--port-live-p port)) ; precondition: LIVE
          (let ((canon (dorg-test--ukfp6-write-provisional-claim-port store port)))
            (cl-letf (((symbol-function 'dorg-test--live-dolt-process-evidence)
                       (lambda (_dir) 'none)))
              (let ((ids (dorg-test--store-server-identities store)))
                (should (= 1 (length ids)))
                (should (null (plist-get (car ids) :pid)))
                (should (= port (plist-get (car ids) :port)))
                (should (equal canon (plist-get (car ids) :dolt-dir)))))))
      (delete-process (car pp))
      (delete-directory base t))))

(ert-deftest dorg-test-stale-registry-reaps-synthetic-claim-dead-port-root ()
  "AC5b (the port-bearing bug, end-to-end through the reaper): a dead-owner ROOT
record whose identity-intact eligible root contains ONLY a synthetic provisional
claim with a NON-nil ephemeral `:port' is BLOCKED while a real listener is live
on that port, then REAPED once the listener is gone — proving the consumer
transformation flips on genuine port ground truth.

Phase LIVE: a real TCP server accepts on the claimed port -> the reaper returns a
`server-unconfirmed' failure for the record and RETAINS both the root and the
record (identical to the pre-fix behaviour, so this half also guards against
reaping a bound server).
Phase DEAD: the listener is closed -> the reaper returns NO failure, deletes the
root, and consumes the record.

Non-vacuous against 47e4c20: with the port-less-only relaxation the DEAD phase
STILL returns `(file . server-unconfirmed)' and leaves root+record on disk."
  (let* ((dir (make-temp-file "dorg-ukfp6-pregistry-" t))
         (dorg-test--registry-dir dir)
         (root (file-name-as-directory
                (make-temp-file "dorg-test-ukfp6-proot-" t)))
         (recfile (expand-file-name "root-synthetic-port.eld" dir))
         (pp (dorg-test--ukfp6-open-ephemeral-listener))
         (listener (car pp))
         (port (cdr pp)))
    (unwind-protect
        (progn
          (with-temp-file recfile
            (prin1 (list :owner-pid 222 :root root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (dorg-test--ukfp6-write-provisional-claim-port (concat root "s/") port)
          ;; Owner dead + no dolt process in the table (evidence `none'); the ONLY
          ;; live ground truth is the REAL port listener (port-live-p NOT stubbed).
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (_pid) nil))          ; owner 222 dead
                    ((symbol-function 'list-system-processes)
                     (lambda () nil)))             ; -> process evidence `none'
            ;; Phase LIVE: listener accepting -> record BLOCKS, root+record survive.
            (should (dorg-dolt-server--port-live-p port))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should (eq 'server-unconfirmed (cdr (assoc recfile failures)))))
            (should (file-directory-p root))
            (should (file-exists-p recfile))
            ;; Phase DEAD: close the listener -> record REAPED, root+record gone.
            (delete-process listener)
            (setq listener nil)
            (should-not (dorg-dolt-server--port-live-p port))
            (should (null (dorg-test--reap-stale-server-registry)))
            (should-not (file-directory-p root))
            (should-not (file-exists-p recfile))
            (should (null (directory-files dir nil "\\.eld\\'")))))
      (when (and listener (process-live-p listener)) (delete-process listener))
      (ignore-errors (delete-directory root t))
      (delete-directory dir t))))

;; OC REFINE (correction 2): in `dorg-test--reap-owned-store' a `--procs' entry
;; NEVER certifies an on-disk metadata identity, and the EXACT snapshotted proc
;; must be proven dead.  Three cases, all with a proc entry: (a) metadata-FREE
;; store + proc that dies -> gone; (b) metadata-FREE store + proc still live ->
;; blocks; (c) UNREADABLE-metadata store + proc that dies -> STILL blocks (the
;; nil-pid identity names a distinct server the proc cannot certify).
;; Non-vacuous against the removed `(if proc-entry gone unknown)' draft, which
;; declared (c) gone and could bury a distinct live server, and against a reap
;; that ignores a live snapshotted proc.
(ert-deftest dorg-test-reap-owned-store-proc-never-certifies-metadata ()
  "A `--procs' entry authorises clearing ONLY a metadata-FREE store, and ONLY
when its EXACT snapshotted proc is proven dead after the stop; an unreadable
metadata identity always blocks regardless of the proc."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-reap-proc-" t))))
         (root (concat base "dorg-test-root/"))
         (bare (concat root "bare/"))
         (meta (concat root "meta/")))
    (unwind-protect
        (progn
          (make-directory bare t)
          (make-directory (concat meta ".dolt/") t)
          (with-temp-file (concat meta ".dolt/sql-server.info") (insert "garbage"))
          (let ((bare-canon (dorg-test--canonical-path bare t))
                (meta-canon (dorg-test--canonical-path meta t)))
            ;; (a) metadata-free + proc that DIES after stop -> gone.
            (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
                   (dorg-dolt-server--procs (make-hash-table :test 'equal))
                   (dorg-test--server-proc nil)
                   (proc-alive t))
              (puthash root t dorg-test--owned-roots)
              (puthash bare-canon (cons 'the-proc 5555) dorg-dolt-server--procs)
              (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
                         (lambda (d) (dorg-test--canonical-path d t)))
                        ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
                        ((symbol-function 'dorg-dolt-server--stop)
                         (lambda (&rest _) (setq proc-alive nil) t))
                        ((symbol-function 'process-live-p)
                         (lambda (p) (and (eq p 'the-proc) proc-alive)))
                        ((symbol-function 'dorg-dolt-server--claim-delete-file) (lambda (&rest _) nil))
                        ((symbol-function 'dorg-test--registry-delete) (lambda (&rest _) nil)))
                (should (eq 'gone (dorg-test--reap-owned-store bare)))))
            ;; (b) metadata-free + proc STILL LIVE after stop -> blocks.
            (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
                   (dorg-dolt-server--procs (make-hash-table :test 'equal))
                   (dorg-test--server-proc nil))
              (puthash root t dorg-test--owned-roots)
              (puthash bare-canon (cons 'the-proc 5555) dorg-dolt-server--procs)
              (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
                         (lambda (d) (dorg-test--canonical-path d t)))
                        ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
                        ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) t))
                        ((symbol-function 'process-live-p) (lambda (p) (eq p 'the-proc))))
                (should (eq 'proc-still-live (dorg-test--reap-owned-store bare)))))
            ;; (c) UNREADABLE metadata + proc that DIES -> STILL blocks.
            (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
                   (dorg-dolt-server--procs (make-hash-table :test 'equal))
                   (dorg-test--server-proc nil)
                   (proc-alive t)
                   (claim-deleted nil))
              (puthash root t dorg-test--owned-roots)
              (puthash meta-canon (cons 'the-proc 6666) dorg-dolt-server--procs)
              (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir)
                         (lambda (d) (dorg-test--canonical-path d t)))
                        ((symbol-function 'dorg-sql--close-connection) (lambda (&rest _) nil))
                        ((symbol-function 'dorg-dolt-server--stop)
                         (lambda (&rest _) (setq proc-alive nil) t))
                        ((symbol-function 'process-live-p)
                         (lambda (p) (and (eq p 'the-proc) proc-alive)))
                        ((symbol-function 'dorg-dolt-server--claim-delete-file)
                         (lambda (&rest _) (setq claim-deleted t))))
                (should-not (eq 'gone (dorg-test--reap-owned-store meta)))
                ;; The blocking identity means metadata is PRESERVED.
                (should-not claim-deleted)))))
      (delete-directory base t))))

;; OC 03:36 (8): a symlink `:root' outside tmp is refused BEFORE any scan or
;; identity probe — recovery never inspects/kills a live process outside test
;; ownership.
(ert-deftest dorg-test-stale-registry-symlink-root-not-scanned ()
  "A dead-owner record whose `:root' is a `dorg-*' SYMLINK to an OUTSIDE store
with exact LIVE server metadata is NEVER scanned, identity-probed, or killed —
the pure eligibility boundary refuses it before `directory-files-recursively'/
`process-attributes' — and the link, its target, and the target's contents all
survive.  Non-vacuous against a reaper that scans `:root' before applying the
containment boundary (it would probe and kill the outside live process)."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-symlink-cn-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (outside-store (concat base "outside-store/"))
         (link (concat tmp "dorg-test-escape"))
         (attr-probes nil) (kills nil))
    (unwind-protect
        (progn
          (make-directory (concat outside-store ".dolt/") t)
          (make-directory tmp t)
          (make-directory dorg-test--registry-dir t)
          (with-temp-file (concat outside-store ".dolt/sql-server.info")
            (insert "7777:6060:uuu"))
          (make-symbolic-link (directory-file-name outside-store) link)
          (with-temp-file (expand-file-name "root-222.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root (file-name-as-directory link))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (eql pid 7777) (not (memql pid kills)) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid)
                       (push pid attr-probes)
                       (when (eql pid 7777)
                         `((comm . "dolt")
                           (args . ,(format
                                     "dolt sql-server --port 6060 --data-dir %s"
                                     (file-truename
                                      (directory-file-name outside-store))))))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (push pid kills) t)))
            (let ((temporary-file-directory tmp))
              (dorg-test--reap-stale-server-registry)))
          ;; Zero probing, zero kills; link + target + contents survive.
          (should (null attr-probes))
          (should (null kills))
          (should (file-symlink-p link))
          (should (file-directory-p outside-store))
          (should (file-exists-p (concat outside-store ".dolt/sql-server.info"))))
      (delete-directory base t))))

;; Next-start reaping is fail-closed on an OBSERVED root-identity mismatch and
;; on a legacy record with no captured identity.
(ert-deftest dorg-test-stale-registry-refuses-distinct-and-legacy-roots ()
  "A dead-owner record with a distinct replacement root is preserved.
The fixture establishes unequal identities at the same path before invoking
the real stale-registry reaper.  The replacement bytes and record survive with
no root scan, server reap, recursive delete, or record consumption.  A legacy
record with no `:root-id' remains protected as an independent branch."
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "dorg-nsident-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (recreate-root (concat tmp "dorg-test-recreate/"))
         (legacy-root (concat tmp "dorg-test-legacy/"))
         (scans 0)
         (reaps 0)
         (dir-deletes 0)
         (record-deletes 0)
         (real-delete-directory (symbol-function 'delete-directory))
         (real-delete-file (symbol-function 'delete-file)))
    (unwind-protect
        (progn
          (make-directory tmp t)
          (make-directory dorg-test--registry-dir t)
          (make-directory recreate-root t)
          (make-directory legacy-root t)
          (let* ((record (expand-file-name "root-222-recreate.eld"
                                           dorg-test--registry-dir))
                 (legacy-record (expand-file-name "root-222-legacy.eld"
                                                  dorg-test--registry-dir))
                 (original-id (dorg-test--path-file-identity recreate-root)))
            (with-temp-file record
              (prin1 (list :owner-pid 222 :root recreate-root
                           :root-id original-id)
                     (current-buffer)))
            (let* ((swap (dorg-test-infra--install-distinct-directory
                          recreate-root))
                   (preserved (plist-get swap :preserved)))
              (should (equal original-id (plist-get swap :original-id)))
              (should-not (equal original-id (plist-get swap :replacement-id)))
              (should (equal (plist-get swap :replacement-id)
                             (dorg-test--path-file-identity recreate-root)))
              (with-temp-file (concat recreate-root "foreign.txt")
                (insert "foreign"))
              ;; Legacy record: NO `:root-id' at all.
              (with-temp-file legacy-record
                (prin1 (list :owner-pid 222 :root legacy-root) (current-buffer)))
              (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                         (lambda (_p) nil)) ; owner 222 dead
                        ((symbol-function 'dorg-test--owned-store-dirs)
                         (lambda (&rest _) (cl-incf scans) nil))
                        ((symbol-function 'dorg-test--confirm-server-gone)
                         (lambda (&rest _) (cl-incf reaps) 'gone))
                        ((symbol-function 'delete-directory)
                         (lambda (&rest args)
                           (cl-incf dir-deletes)
                           (apply real-delete-directory args)))
                        ((symbol-function 'delete-file)
                         (lambda (&rest args)
                           (cl-incf record-deletes)
                           (apply real-delete-file args))))
                (let ((temporary-file-directory tmp))
                  (dorg-test--reap-stale-server-registry)))
              (should (file-directory-p recreate-root))
              (should (file-exists-p (concat recreate-root "foreign.txt")))
              (should (file-directory-p preserved))
              (should (file-exists-p record))
              (should (file-directory-p legacy-root))
              (should (file-exists-p legacy-record))
              (should (= 0 scans))
              (should (= 0 reaps))
              (should (= 0 dir-deletes))
              (should (= 0 record-deletes)))))
      (delete-directory base t))))

;; OC 03:36 (7): the reaper RETURNS a dead-owner failure when an exact pid
;; remains live after the kill (unconfirmed) so the runner can block a start.
(ert-deftest dorg-test-reap-stale-registry-returns-failure-on-unconfirmed ()
  "`dorg-test--reap-stale-server-registry' RETURNS a non-nil dead-owner failure
list when a dead-owner record's exact server pid remains LIVE after the kill
attempt (unconfirmed), and PRESERVES the record.  Non-vacuous against a reaper
that returns nil while silently preserving the orphan (letting the runner
spawn over it)."
  (let* ((dir (make-temp-file "dorg-registry-unconf-" t))
         (dorg-test--registry-dir dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "s7.eld" dir)
            (prin1 (list :owner-pid 222 :server-pid 7000 :port 5
                         :root "/tmp/dorg-test-unconf/"
                         :dolt-dir "/tmp/dorg-test-unconf/d/")
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (memq pid '(7000)) t)))  ; owner dead, 7000 stays live
                    ((symbol-function 'process-attributes)
                     (lambda (_p)
                       '((comm . "dolt")
                         (args . "dolt sql-server --port 5 --data-dir /tmp/dorg-test-unconf/d/"))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (&rest _) t)))    ; kill "succeeds" but pid stays live
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should failures)
              (should (= 1 (length failures)))))
          (should (file-exists-p (expand-file-name "s7.eld" dir))))
      (delete-directory dir t))))

;; OC 03:29 (3): reap-owned-root with UNREGISTER drops ownership WITHOUT
;; deleting the directory (persistent inspection artifact survives, un-owned).
(ert-deftest dorg-test-reap-owned-root-unregister-keeps-inspection-artifact ()
  "`(dorg-test--reap-owned-root root t)' reaps servers and DROPS ownership
WITHOUT deleting the directory: the inspection work dir SURVIVES on disk but is
no longer registered, so the runner's `dorg-test--reap-owned-roots' leaves it
untouched.  Non-vacuous against a refactor that ignores the unregister arg
\(leaving it registered so the runner deletes the artifact) or deletes it here."
  (let* ((dorg-test--owned-roots (make-hash-table :test 'equal))
         (dorg-test--registry-dir (make-temp-file "dorg-insp-reg-" t))
         (workdir (dorg-test--canonical-path
                   (make-temp-file "dorg-test-insp-" t) t)))
    (unwind-protect
        (progn
          (puthash workdir (dorg-test--path-file-identity workdir)
                   dorg-test--owned-roots)
          (cl-letf (((symbol-function 'dorg-test--registry-delete) (lambda (&rest _) nil)))
            (let ((status (dorg-test--reap-owned-root workdir t)))
              (should (eq status 'gone))
              (should-not (gethash workdir dorg-test--owned-roots))
              (should (file-directory-p workdir))
              ;; The runner's cleanup no longer sees it -> not deleted.
              (should (null (dorg-test--reap-owned-roots)))
              (should (file-directory-p workdir)))))
      (ignore-errors (delete-directory workdir t))
      (ignore-errors (delete-directory dorg-test--registry-dir t)))))

;; OC 03:29 (4): rollback delete failure retains ownership + compound error.
(ert-deftest dorg-test-make-owned-temp-dir-delete-failure-retains-and-compounds ()
  "When registration fails AND the rollback delete ALSO fails,
`dorg-test--make-owned-temp-dir' RETAINS in-memory ownership of the exact root
(so a later reap retries) and signals a COMPOUND `dorg-dolt-fixture-cleanup-
error' carrying BOTH failures — never reporting only the persist error while
leaking an untracked root.  Non-vacuous against an `ignore-errors' rollback
that abandons the dir and reports only the persist failure."
  (let ((dorg-test--owned-roots (make-hash-table :test 'equal))
        (created nil))
    (cl-letf* ((real-mtf (symbol-function 'make-temp-file))
               ((symbol-function 'make-temp-file)
                (lambda (&rest args) (setq created (apply real-mtf args)) created))
               ((symbol-function 'dorg-test--registry-write)
                (lambda (&rest _) (error "spec: persist failed")))
               ((symbol-function 'delete-directory)
                (lambda (&rest _) (error "spec: rollback delete failed"))))
      (should-error (dorg-test--make-owned-temp-dir "dorg-test-mtf-cf-")
                    :type 'dorg-dolt-fixture-cleanup-error)
      (should (gethash (dorg-test--canonical-path created t)
                       dorg-test--owned-roots)))
    (ignore-errors (delete-directory created t))))

;; OC 03:47 (3) / 04:00: unreadable/malformed/missing-root records BLOCK a
;; fresh start (preserve the file), independent of owner liveness.
(ert-deftest dorg-test-reap-stale-registry-blocks-on-malformed-record ()
  "An unreadable/malformed `.eld', and a record MISSING its required string
`:root' EVEN UNDER A LIVE OWNER, are reported as failures that BLOCK a fresh
start and are PRESERVED (ownership is undeterminable / unreapable — never
guess or delete, and a missing-root live-owner record must not pass as a
concurrent lane).  A well-formed live-owner record with a valid root is
skipped and does NOT block.  Non-vacuous against a reaper that `ignore-errors'
the read (dropping the file) or validates `:root' only after owner liveness."
  (let* ((dir (make-temp-file "dorg-registry-malformed-" t))
         (dorg-test--registry-dir dir)
         (probes nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.eld" dir) (insert "not-a-record"))
          ;; LIVE owner but NO string root -> must still block.
          (with-temp-file (expand-file-name "liveroot.eld" dir)
            (prin1 (list :owner-pid 111 :server-pid 5) (current-buffer)))
          ;; Well-formed live-owner record with a root -> skipped, no block.
          (with-temp-file (expand-file-name "ok.eld" dir)
            (prin1 (list :owner-pid 111 :root "/x/") (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (eql pid 111) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid) (push pid probes) nil)))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should (assoc (expand-file-name "bad.eld" dir) failures))
              (should (assoc (expand-file-name "liveroot.eld" dir) failures))
              (should-not (assoc (expand-file-name "ok.eld" dir) failures))))
          ;; No process probes at all; every file preserved.
          (should (null probes))
          (should (file-exists-p (expand-file-name "bad.eld" dir)))
          (should (file-exists-p (expand-file-name "liveroot.eld" dir)))
          (should (file-exists-p (expand-file-name "ok.eld" dir))))
      (delete-directory dir t))))

;; OC 03:47 (4): an ineligible (symlink) root with an explicit LIVE pid is
;; never probed/killed; the record is preserved.
(ert-deftest dorg-test-reap-stale-registry-ineligible-root-explicit-pid-no-probe ()
  "A dead-owner record whose `:root' is an EXISTING ineligible dir (symlink) and
which carries an explicit LIVE `:server-pid'/`:dolt-dir' is NEVER identity-
probed or killed — containment refuses it before any `process-attributes'/kill
— and the record is PRESERVED (a failure).  Non-vacuous against a reaper that
probes the record's explicit identity before the containment boundary."
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "dorg-inel-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (outside (concat base "outside/"))
         (link (concat tmp "dorg-test-link"))
         (attr-probes nil) (kills nil))
    (unwind-protect
        (progn
          (make-directory outside t) (make-directory tmp t)
          (make-directory dorg-test--registry-dir t)
          (make-symbolic-link (directory-file-name outside) link)
          (with-temp-file (expand-file-name "s.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :server-pid 5000 :port 3
                         :root (file-name-as-directory link)
                         :dolt-dir (concat outside "d/"))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (eql pid 5000) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid) (push pid attr-probes) nil))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (push pid kills) t)))
            (let ((temporary-file-directory tmp))
              (should (dorg-test--reap-stale-server-registry))))
          (should (null attr-probes))
          (should (null kills))
          (should (file-exists-p (expand-file-name "s.eld" dorg-test--registry-dir))))
      (delete-directory base t))))

;; OC 03:47 (4): an eligible root advertising a dolt-dir OUTSIDE it is not
;; probed/killed on that explicit identity.
(ert-deftest dorg-test-reap-stale-registry-eligible-root-outside-dolt-dir-no-probe ()
  "A dead-owner record with an ELIGIBLE `:root' but a `:dolt-dir' that resolves
OUTSIDE that root is NEVER probed/killed on its explicit identity — the
per-identity containment check refuses it — and the record is PRESERVED (the
eligible root is NOT deleted either).  Non-vacuous against a reaper that trusts
the record's dolt-dir without proving containment under the eligible root."
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "dorg-outdd-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (root (concat tmp "dorg-test-r/"))
         (attr-probes nil) (kills nil))
    (unwind-protect
        (progn
          (make-directory root t) (make-directory dorg-test--registry-dir t)
          (with-temp-file (expand-file-name "s.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :server-pid 5001 :port 3
                         :root root :dolt-dir (concat base "elsewhere/d/"))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (and (eql pid 5001) t)))
                    ((symbol-function 'process-attributes)
                     (lambda (pid) (push pid attr-probes) nil))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (push pid kills) t)))
            (let ((temporary-file-directory tmp))
              (should (dorg-test--reap-stale-server-registry))))
          (should (null attr-probes))
          (should (null kills))
          (should (file-directory-p root))
          (should (file-exists-p (expand-file-name "s.eld" dorg-test--registry-dir))))
      (delete-directory base t))))

;; OC 03:56: a root-only record on an EXISTING ineligible root is preserved
;; (never consumed over a root we refuse to own); an eligible-but-ABSENT root
;; is safely consumed.
(ert-deftest dorg-test-reap-stale-registry-ineligible-existing-root-only-preserves ()
  "A dead-owner ROOT-ONLY record (owner + root, no server fields) whose root is
an EXISTING ineligible dir yields zero identities yet is PRESERVED (a failure)
with zero probes — never consumed over an existing root we refuse to own.  An
eligible-but-ABSENT root is safely consumed.  Non-vacuous against a reaper that
consumes any zero-identity record whenever the root delete is merely skipped."
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "dorg-inelrec-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (bad-root (concat tmp "unrelated-existing/"))
         (absent-root (concat tmp "dorg-test-absent/"))
         (attr-probes nil))
    (unwind-protect
        (progn
          (make-directory bad-root t) (make-directory dorg-test--registry-dir t)
          (with-temp-file (expand-file-name "bad.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root bad-root) (current-buffer)))
          (with-temp-file (expand-file-name "absent.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root absent-root) (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p) (lambda (_p) nil))
                    ((symbol-function 'process-attributes)
                     (lambda (pid) (push pid attr-probes) nil)))
            (let ((temporary-file-directory tmp)
                  (failures nil))
              (setq failures (dorg-test--reap-stale-server-registry))
              ;; The existing-ineligible root record is a FAILURE that blocks a
              ;; fresh start — not silently preserved-and-ignored.
              (should (equal 'root-ineligible
                             (cdr (assoc (expand-file-name
                                          "bad.eld" dorg-test--registry-dir)
                                         failures))))))
          (should (null attr-probes))
          (should (file-exists-p (expand-file-name "bad.eld" dorg-test--registry-dir)))
          (should (file-directory-p bad-root))
          (should-not (file-exists-p
                       (expand-file-name "absent.eld" dorg-test--registry-dir))))
      (delete-directory base t))))

;; OC 04:00/04:04: schema validation (required string :root) runs BEFORE the
;; live-owner skip — a LIVE-owner record missing its root blocks with zero
;; liveness/attribute/root probing.
(ert-deftest dorg-test-reap-stale-registry-live-owner-missing-root-blocks ()
  "A record with a LIVE owner but a MISSING (non-string) `:root' is reported
`malformed' and PRESERVED — schema is validated BEFORE owner liveness, so it
never passes as a concurrent lane — and NO owner-liveness, `process-attributes',
or root function is called for it.  Non-vacuous against a reaper that checks
owner liveness first (skipping the record) or validates `:root' only later."
  (let* ((dir (make-temp-file "dorg-registry-noroot-" t))
         (dorg-test--registry-dir dir)
         (live-calls nil) (attr-probes nil) (root-calls nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "noroot.eld" dir)
            (prin1 (list :owner-pid 111 :server-pid 5) (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid) (push pid live-calls) t))
                    ((symbol-function 'process-attributes)
                     (lambda (pid) (push pid attr-probes) nil))
                    ((symbol-function 'dorg-test--reap-stale-root)
                     (lambda (root &rest _) (push root root-calls) 'deleted)))
            (let ((failures (dorg-test--reap-stale-server-registry)))
              (should (equal 'malformed-root
                             (cdr (assoc (expand-file-name "noroot.eld" dir)
                                         failures))))))
          ;; Schema validated before any liveness/attribute/root probing.
          (should (null live-calls))
          (should (null attr-probes))
          (should (null root-calls))
          (should (file-exists-p (expand-file-name "noroot.eld" dir))))
      (delete-directory dir t))))

;; OC 03:47 (4, last point): confirmation status keyed by exact tuple, not pid.
(ert-deftest dorg-test-reap-stale-registry-status-keyed-by-tuple-not-pid ()
  "Confirmation status is keyed by the EXACT identity tuple, never by pid.  Two
tuples share pid 900: the overlay tuple (port 10) is evaluated FIRST and its
pid reads DEAD -> `gone'; the native tuple (port 11) then reads the SAME pid
LIVE and matched, so it MUST be independently inspected and killed.  A
pid-keyed `gone' cache would SKIP the second tuple (its pid already `gone'),
leaving the live matched server UN-KILLED.  Exact-tuple keying kills it.
Non-vacuous: the old pid-keyed code rechecks after `unknown' but SKIPS after
`gone', so tuple 1 must be `gone' (not `unknown') to distinguish them; the
distinguishing observable is `kills' = (900)."
  (let* ((base (file-name-as-directory (file-truename (make-temp-file "dorg-tuple-" t))))
         (tmp (concat base "tmp/"))
         (dorg-test--registry-dir (concat base "registry/"))
         (root (concat tmp "dorg-test-t/"))
         (store (concat root "s/"))
         (store-canon nil)
         (n900 0)
         (kills nil))
    (unwind-protect
        (progn
          (make-directory (concat store ".dolt/") t)
          (make-directory dorg-test--registry-dir t)
          ;; A real metadata file so the store is DISCOVERED by the root scan
          ;; (claim-read/dolt-server-info are mocked for the identity itself).
          (with-temp-file (concat store ".dolt/sql-server.info") (insert "900:11:uuu"))
          (setq store-canon (dorg-dolt-server--canonical-dir store))
          (with-temp-file (expand-file-name "r.eld" dorg-test--registry-dir)
            (prin1 (list :owner-pid 222 :root root
                         :root-id (dorg-test--path-file-identity root))
                   (current-buffer)))
          (cl-letf (((symbol-function 'dorg-dolt-server--claim-read)
                     (lambda (_c) '((:server-pid . 900) (:port . 10))))   ; overlay P/10
                    ((symbol-function 'dorg-dolt-server--dolt-server-info)
                     (lambda (_c) '(:pid 900 :port 11)))                  ; native P/11
                    ((symbol-function 'dorg-dolt-server--pid-live-p)
                     (lambda (pid)
                       (cond ((not (eql pid 900)) nil)         ; owner 222 dead
                             ((memql 900 kills) nil)            ; post-kill: dead
                             ;; First confirm (overlay tuple) reads DEAD -> gone;
                             ;; the second tuple then reads LIVE.
                             ((= (cl-incf n900) 1) nil)
                             (t t))))
                    ((symbol-function 'process-attributes)
                     (lambda (_p) `((comm . "dolt")
                                    (args . ,(format
                                              "dolt sql-server --port 11 --data-dir %s"
                                              store-canon)))))
                    ((symbol-function 'dorg-dolt-server--kill-pid)
                     (lambda (pid &rest _) (push pid kills) t)))
            (let ((temporary-file-directory tmp))
              (dorg-test--reap-stale-server-registry)))
          ;; Exact-tuple keying independently inspected + killed the live native
          ;; tuple; a pid-keyed `gone' cache would have skipped it (kills nil).
          (should (equal kills '(900))))
      (delete-directory base t))))

;;; Opaque DOrg-managed Dolt fixture/lease API (tracked-case)
;;
;; SYNTHETIC RED specs for the supported fixture module (dorg-dolt-fixture.el)
;; that becomes the SOLE, OPAQUE owner of Dolt sql-server lifecycle for DOrg
;; test/consumer code.  No real server is spawned: the shared-server spawn
;; path is mocked to succeed WITHOUT a `dolt sql-server' via `cl-letf' spies,
;; so these run under the server-suppressed runner
;; \(`(dorg-run-tests--batch "dorg-test-fixture-" nil)').  Every spec
;; references the not-yet-existing API through `funcall'/`fboundp' so
;; byte-compile stays clean, gates on `(should (fboundp ...))' so it FAILS
;; \(not skips) while the API is absent, and maps to a tracked-case AC so it is
;; GREEN-able once IMPL lands.  The genuine cold-start/lifecycle tests
;; (dorg-dolt-server-tests.el) stay separate and keep spawning real servers.

(defvar dorg-test--mysql-available)     ; defined in dorg-test-helpers
(defvar dorg-test--server-proc)
(defvar dorg-test--server-data-dir)
(defvar dorg-test--server-port)
(defvar dorg-test--mysql-conn)

(defvar dorg-test-fixture--shared-proc-alive nil
  "Mutable box `(ALIVE)' modelling the mocked shared server process's liveness.
`dorg-test-fixture--with-spawn-mocked' binds it to `(t)' and wires the default
`process-live-p'/`dorg-dolt-stop' mocks through it so a successful PUBLIC stop
makes the fake process read DEAD — the realistic post-stop state the close
exact-death proof requires.  A test that overrides `dorg-dolt-stop' to succeed
must `(setcar dorg-test-fixture--shared-proc-alive nil)' to model the kill.")

(defmacro dorg-test-fixture--with-spawn-mocked (&rest body)
  "Run BODY with the shared-server spawn path mocked to succeed with NO server.
`start-process' returns a stub process, its pid/liveness/readiness are
faked so `dorg-dolt-server-list' can classify the fixture entry as a live
OWNED server, the mysql client \"connects\" to a stub, the durable-registry
writes/deletes are neutralized, and the public stop surface is a no-op.

The server-runtime tables (`--procs'/`--attached-ports'/`--owned-claims')
and the shared-server globals are let-bound to fresh empty state so a
GREEN fixture's registration is visible to the REAL `dorg-dolt-server-list'
in isolation and nothing leaks between tests; a throwaway registry dir is
bound and deleted.  Tests layer their own counting spies by nesting
`cl-letf' inside BODY; an inner rebinding of any of these symbols wins for
its dynamic extent."
  (declare (indent 0) (debug t))
  `(let ((dorg-test--registry-dir (make-temp-file "dorg-fixture-test-" t))
         (dorg-test--server-proc nil)
         (dorg-test--server-data-dir nil)
         (dorg-test--server-port nil)
         (dorg-test--mysql-conn nil)
         (dorg-test--mysql-available t)
         ;; Liveness box: the mocked shared proc starts alive; a successful
         ;; default `dorg-dolt-stop' flips it dead so the close exact-death
         ;; proof sees the realistic post-stop state.
         (dorg-test-fixture--shared-proc-alive (list t))
         ;; Fresh fixture state so each spec's server/store leases and counters
         ;; are isolated and nothing leaks between tests.
         (dorg-dolt-fixture--shared-lease nil)
         (dorg-dolt-fixture--failed-start-lease nil)
         (dorg-dolt-fixture--conn nil)
         (dorg-dolt-fixture--cold-starts 0)
         (dorg-dolt-fixture--db-counter 0)
         (dorg-dolt-fixture--store-leases (make-hash-table :test 'equal))
         (dorg-dolt-server--procs (make-hash-table :test 'equal))
         (dorg-dolt-server--attached-ports (make-hash-table :test 'equal))
         (dorg-dolt-server--owned-claims (make-hash-table :test 'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'dorg-dolt-supported-p) (lambda (&rest _) t))
                   ((symbol-function 'dorg-test--registry-write) (lambda (&rest _) nil))
                   ((symbol-function 'dorg-test--registry-delete) (lambda (&rest _) nil))
                   ((symbol-function 'dorg-dolt-server--free-port) (lambda () 3999))
                   ((symbol-function 'dorg-dolt-server--spawn-logged)
                    (lambda (&rest _) 'dorg-test-fixture--fake-proc))
                   ((symbol-function 'process-id) (lambda (&rest _) 4242))
                   ((symbol-function 'process-live-p)
                    (lambda (p) (if (eq p 'dorg-test-fixture--fake-proc)
                                    (car dorg-test-fixture--shared-proc-alive)
                                  t)))
                   ((symbol-function 'delete-process) (lambda (&rest _) nil))
                   ((symbol-function 'set-process-query-on-exit-flag) (lambda (&rest _) nil))
                   ((symbol-function 'dorg-dolt-server--port-live-p) (lambda (&rest _) t))
                   ((symbol-function 'dorg-dolt-server--wait-ready) (lambda (&rest _) t))
                   ((symbol-function 'mysql-connect)
                    (lambda (&rest _) 'dorg-test-fixture--fake-conn))
                   ((symbol-function 'mysql-disconnect) (lambda (&rest _) nil))
                   ;; The fixture uses a symbolic connection in this harness;
                   ;; real local resource disposal has its own process+buffer
                   ;; witness below.
                   ((symbol-function 'dorg-dolt-fixture--discard-connection-resources)
                    (lambda (&rest _) nil))
                   ;; Default no-op DDL seam (the fake conn is not a real
                   ;; `mysql-conn'); specs that assert CREATE/DROP override it.
                   ((symbol-function 'mysql-query) (lambda (&rest _) nil))
                   ((symbol-function 'dorg-dolt-server--publish-claim) (lambda (&rest _) "tok"))
                   ((symbol-function 'dorg-dolt-stop)
                    (lambda (&rest _)
                      (setcar dorg-test-fixture--shared-proc-alive nil) t))
                   ((symbol-function 'dorg-dolt-stop-all) (lambda (&rest _) 0)))
           ,@body)
       (ignore-errors (delete-directory dorg-test--registry-dir t)))))

(defun dorg-test-fixture--live-owned (entries)
  "Return ENTRIES that are a live server owned by this Emacs process.
The inventory `:servers' contract: a server counts as \"inside the
fixture\" iff `dorg-dolt-server-list' reports it `:status' `live' AND
`:role' `owned' — foreign/attached owners and dead entries are excluded."
  (seq-filter (lambda (e)
                (and (eq (plist-get e :status) 'live)
                     (eq (plist-get e :role) 'owned)))
              entries))

;; Spec 1 (AC: supported surface exists)
(ert-deftest dorg-test-fixture-api-surface-is-public ()
  "The fixture API is a SUPPORTED, PUBLIC surface: every entry point is
`fboundp' and carries no `--' private marker.  RED before IMPL (void)."
  (dolist (sym '(dorg-dolt-fixture-shared-ensure
                 dorg-dolt-fixture-store-acquire
                 dorg-dolt-fixture-store-release
                 dorg-dolt-fixture-close
                 dorg-dolt-fixture-inventory
                 dorg-dolt-fixture-with-store
                 dorg-dolt-fixture-call-with-store
                 dorg-dolt-fixture-lease-dolt-dir))
    (should (fboundp sym))
    (should-not (string-match-p "--" (symbol-name sym)))))

;; Spec 2 (AC: opacity — OC audit #6: real lease, reject list/plist exposure)
(ert-deftest dorg-test-fixture-lease-is-opaque ()
  "A REAL lease is OPAQUE.  `-shared-ensure' returns an object that is a
record (`cl-defstruct'), NOT a list/plist a caller could destructure to
reach `:port'/`:pid'/`:proc'/`:claim'.  `dorg-dolt-fixture-lease-dolt-dir'
is the ONLY public reader and returns a string; no public
pid/proc/claim/port accessor exists, so queries can only route through the
normal production transport.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-shared-ensure))
  (should (fboundp 'dorg-dolt-fixture-lease-dolt-dir))
  (dorg-test-fixture--with-spawn-mocked
    (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          (progn
            ;; Opaque object: a record, not a caller-destructurable list/plist.
            (should (recordp lease))
            (should-not (listp lease))
            ;; The one public reader works and yields a string.
            (should (stringp (funcall 'dorg-dolt-fixture-lease-dolt-dir lease)))
            ;; No public accessor exposes internals.
            (dolist (accessor '(dorg-dolt-fixture-lease-port
                                dorg-dolt-fixture-lease-pid
                                dorg-dolt-fixture-lease-server-pid
                                dorg-dolt-fixture-lease-proc
                                dorg-dolt-fixture-lease-process
                                dorg-dolt-fixture-lease-claim
                                dorg-dolt-fixture-lease-conn))
              (should-not (fboundp accessor))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))

;; Spec 3 (AC: w1asy persist-before-spawn — OC audit #5: EXACTLY one each + order)
(ert-deftest dorg-test-fixture-shared-ensure-persists-before-spawn ()
  "`dorg-dolt-fixture-shared-ensure' persists the durable ownership record
BEFORE it spawns the server — the persist-before-start invariant
\(tracked-case) that makes zero-fatal-hook shutdown safe: no window exists in
which a server outlives its Emacs unrecorded.  Over a COMPLETE (happy
path) ensure, the registry-write seam runs EXACTLY once and `start-process'
EXACTLY once, in that order — no duplicate hidden writes or spawns (the pid
is recovered from `sql-server.info' at reap time, so no post-spawn
re-write is needed).  RED before IMPL (void-function)."
  (should (fboundp 'dorg-dolt-fixture-shared-ensure))
  (dorg-test-fixture--with-spawn-mocked
    (let ((writes 0) (spawns 0) order)
      (cl-letf (((symbol-function 'dorg-test--registry-write)
                 (lambda (&rest _) (cl-incf writes) (push 'write order) nil))
                ((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _)
                   (cl-incf spawns) (push 'spawn order)
                   'dorg-test-fixture--fake-proc)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (progn
                (should (= 1 writes))
                (should (= 1 spawns))
                (should (equal (nreverse order) '(write spawn))))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))))

;; Spec 4 (AC: EXACTLY 1 live server)
(ert-deftest dorg-test-fixture-shared-ensure-idempotent ()
  "Two `-shared-ensure' calls reuse ONE live server: the spawn primitive
runs exactly once and the SAME lease object is returned.  This is the
`EXACTLY 1 live server' guarantee at the acquire layer.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-shared-ensure))
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc)))
        (let ((lease1 (funcall 'dorg-dolt-fixture-shared-ensure))
              (lease2 (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (progn
                (should (= 1 spawns))
                (should lease1)
                (should (eq lease1 lease2)))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease1))))))))

;; Spec 5 (AC: dorg-dolt-server-list sees the managed fixture —
;;          OC audit #1 + readiness-boundary directive 01:04/01:05:
;;          -shared-ensure is a REAL start+readiness+inventory boundary)
(ert-deftest dorg-test-fixture-visible-in-server-list ()
  "`-shared-ensure' is a READINESS BOUNDARY, not lazy context construction.
It registers a first-class DOrg-managed server and must return ONLY after
that server is actually started, readiness-probed, and visible in the
PUBLIC inventory — never deferring the start to first use.

Downstream evidence (Downstream tracker d0cd0da): `tracker-backend-dolt-context' is
LAZY — it returns while `dorg-dolt-server-list' is still nil and the first
BODY query starts the server later, so a consumer cannot sample a truthful
pre-body peak/budget.  This fixture must NOT behave that way: the moment
`-shared-ensure' returns — BEFORE any store-acquire/BODY query — the REAL
`dorg-dolt-server-list' must report EXACTLY ONE `:role' `owned' `:status'
`live' entry whose `:dolt-dir' is the lease's own store, and the public
`-inventory' `:servers' (the peak-sample point) must read 1.  A
context-only ensure (list still empty on return) fails here.  RED before
IMPL."
  (should (fboundp 'dorg-dolt-fixture-shared-ensure))
  (dorg-test-fixture--with-spawn-mocked
    (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          ;; Sample IMMEDIATELY after ensure, before ANY store-acquire/BODY:
          ;; the readiness boundary means the server is already live+visible.
          (let* ((dir (funcall 'dorg-dolt-fixture-lease-dolt-dir lease))
                 (canon (dorg-dolt-server--canonical-dir dir))
                 (live-owned (dorg-test-fixture--live-owned
                              (dorg-dolt-server-list))))
            (should (= 1 (length live-owned)))
            (should (equal canon
                           (dorg-dolt-server--canonical-dir
                            (plist-get (car live-owned) :dolt-dir))))
            ;; Same readiness is the consumer's peak/budget sample point,
            ;; observable via the public inventory with no BODY query run.
            (should (= 1 (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                    :servers))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))

;; Spec 6 (AC: 1→0, exact idempotent close — OC audit #2: EXACT call counts)
(ert-deftest dorg-test-fixture-close-is-exact-and-idempotent ()
  "`-close' on a server lease is EXACT: it calls the PUBLIC
`dorg-dolt-stop' exactly once with the lease's OWN dolt-dir, deletes the
durable record exactly once, and NEVER calls the owner-wide
`dorg-dolt-stop-all' (which could stop sibling servers).  A second close is
a bounded no-op: zero additional stops or deletes.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-close))
  (dorg-test-fixture--with-spawn-mocked
    (let (stops (deletes 0) (stop-alls 0))
      (cl-letf (((symbol-function 'dorg-dolt-stop)
                 (lambda (d &rest _) (push d stops)
                   (setcar dorg-test-fixture--shared-proc-alive nil) t))
                ((symbol-function 'dorg-dolt-stop-all)
                 (lambda (&rest _) (cl-incf stop-alls) 0))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let* ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
               (dir (funcall 'dorg-dolt-fixture-lease-dolt-dir lease)))
          (funcall 'dorg-dolt-fixture-close lease)
          ;; Exactly one public stop, for the lease's own dir; no stop-all.
          (should (= 1 (length stops)))
          (should (equal (dorg-dolt-server--canonical-dir (car stops))
                         (dorg-dolt-server--canonical-dir dir)))
          (should (= 1 deletes))
          (should (= 0 stop-alls))
          ;; Second close: bounded no-op.
          (funcall 'dorg-dolt-fixture-close lease)
          (should (= 1 (length stops)))
          (should (= 1 deletes))
          (should (= 0 stop-alls)))))))

;; Spec 7 (AC: foreign untouched)
(ert-deftest dorg-test-fixture-close-leaves-foreign-servers-untouched ()
  "`-close' never touches a foreign server: a foreign entry present in the
inventory is never handed to the public stop surface — only the fixture's
own store is stopped.  Non-vacuous against a close that naively iterates
`dorg-dolt-server-list' and stops every entry.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-close))
  (dorg-test-fixture--with-spawn-mocked
    (let ((foreign "/tmp/dorg-foreign-server/d/")
          stopped)
      (cl-letf (((symbol-function 'dorg-dolt-server-list)
                 (lambda () (list (list :dolt-dir "/tmp/dorg-foreign-server/d/"
                                        :port 4111 :status 'live :role 'foreign))))
                ((symbol-function 'dorg-dolt-stop)
                 (lambda (d &rest _) (push d stopped)
                   (setcar dorg-test-fixture--shared-proc-alive nil) t)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (funcall 'dorg-dolt-fixture-close lease)
          (should-not (member (file-name-as-directory foreign)
                              (mapcar #'file-name-as-directory
                                      (delq nil stopped)))))))))

;; Spec 8 (AC: supported surface — OC audit #2: same exactness for public stop)
(ert-deftest dorg-test-fixture-close-uses-only-public-stop ()
  "Normal `-close' reaps ONLY through the PUBLIC stop surface, EXACTLY: it
calls `dorg-dolt-stop' exactly once, never `dorg-dolt-stop-all', and never
the private crash-recovery kills (`--kill-pid'/`--kill-process') — those
belong solely to the w1asy next-start reaper.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-close))
  (dorg-test-fixture--with-spawn-mocked
    (let ((public-stops 0) (stop-alls 0) (private-kills 0))
      (cl-letf (((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _) (cl-incf public-stops)
                   (setcar dorg-test-fixture--shared-proc-alive nil) t))
                ((symbol-function 'dorg-dolt-stop-all)
                 (lambda (&rest _) (cl-incf stop-alls) 0))
                ((symbol-function 'dorg-dolt-server--kill-pid)
                 (lambda (&rest _) (cl-incf private-kills) t))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (cl-incf private-kills) t)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (funcall 'dorg-dolt-fixture-close lease)
          (should (= 1 public-stops))
          (should (= 0 stop-alls))
          (should (= 0 private-kills)))))))

;; Spec 9 (AC: record before/after cold-start COUNT —
;;          OC audit #3 / round-2 #1: :servers from >=2 DISTINCT public
;;          snapshots with different live+owned counts; public call asserted)
(ert-deftest dorg-test-fixture-inventory-metrics-shape ()
  "`-inventory' returns a structured metrics plist with integer `:servers',
`:stores', and `:cold-starts'.

The fixture is a SINGLETON, so `:servers' is 0 or 1 (OC 02:17 blocker 1):
it is 1 exactly when the CURRENT shared lease's OWN canonical dolt-dir
appears `:role' `owned' `:status' `live' in the PUBLIC `dorg-dolt-server-list',
and 0 otherwise.  Driven with the SAME open lease against distinct public
snapshots: the lease's own dir live+owned (alongside an UNRELATED live+owned
server that must be EXCLUDED) ⇒ 1; only the unrelated owned server ⇒ 0; the
lease's dir present but DEAD ⇒ 0; and with NO open lease ⇒ 0.  A `:servers'
that ignores the lease over-counts the unrelated owned server; inventory must
actually CALL the public list (asserted via a call counter).  `:cold-starts'
counts real server spawns and advances by exactly one ONLY when a spawn
happens; an idempotent second ensure does NOT bump it.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-inventory))
  (let* ((own (file-name-as-directory (make-temp-file "dorg-inv-own-" t)))
         (other (file-name-as-directory (make-temp-file "dorg-inv-other-" t)))
         (calls 0)
         (dorg-dolt-fixture--store-leases (make-hash-table :test 'equal))
         ;; A single OPEN shared lease whose own store is `own'.
         (dorg-dolt-fixture--shared-lease
          (dorg-dolt-fixture--make-lease
           :kind 'server :dolt-dir (dorg-dolt-server--canonical-dir own)
           :data-dir own)))
    (unwind-protect
        (cl-flet ((servers-for
                    (snapshot)
                    (cl-letf (((symbol-function 'dorg-dolt-server-list)
                               (lambda () (cl-incf calls) snapshot)))
                      (funcall 'dorg-dolt-fixture-inventory))))
          ;; Shape: all three metrics are integers.
          (let ((inv (servers-for nil)))
            (should (integerp (plist-get inv :servers)))
            (should (integerp (plist-get inv :stores)))
            (should (integerp (plist-get inv :cold-starts))))
          ;; The lease's own dir live+owned; the UNRELATED live+owned `other'
          ;; is EXCLUDED (singleton scope): expect 1.
          (should (= 1 (plist-get
                        (servers-for
                         (list (list :dolt-dir own :status 'live :role 'owned)
                               (list :dolt-dir other :status 'live :role 'owned)
                               (list :dolt-dir "/tmp/for/" :status 'live :role 'foreign)))
                        :servers)))
          ;; Only an unrelated owned server (not the lease's dir): expect 0.
          (should (= 0 (plist-get
                        (servers-for
                         (list (list :dolt-dir other :status 'live :role 'owned)))
                        :servers)))
          ;; The lease's dir present but DEAD: expect 0.
          (should (= 0 (plist-get
                        (servers-for
                         (list (list :dolt-dir own :status 'dead :role 'owned)))
                        :servers)))
          ;; Provenance: inventory consulted the PUBLIC list for every snapshot.
          (should (>= calls 4)))
      (ignore-errors (delete-directory own t))
      (ignore-errors (delete-directory other t))))
  ;; With NO open shared lease, :servers is 0 even when this Emacs owns a live
  ;; server for some other reason.
  (let ((dorg-dolt-fixture--shared-lease nil)
        (dorg-dolt-fixture--store-leases (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'dorg-dolt-server-list)
               (lambda () (list (list :dolt-dir "/tmp/own/" :status 'live :role 'owned)))))
      (should (= 0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :servers)))))
  ;; :cold-starts advances once per real spawn, never on idempotent reuse.
  (dorg-test-fixture--with-spawn-mocked
    (let ((cold0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :cold-starts))
          (spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (progn
                (should (= 1 spawns))
                (should (= (1+ cold0)
                           (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                      :cold-starts)))
                ;; Idempotent reuse: no spawn, no cold-start bump.
                (funcall 'dorg-dolt-fixture-shared-ensure)
                (should (= 1 spawns))
                (should (= (1+ cold0)
                           (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                      :cold-starts))))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))))

;; Spec 10 (AC: isolated store create/drop —
;;          OC audit #4 / round-2 #2: prove TWO distinct CREATE DATABASE, each
;;          matched by EXACTLY one DROP of its OWN token; no basename inference)
(defun dorg-test-fixture--ddl-token (text verb)
  "Return the database token named by a `VERB DATABASE' clause in TEXT, else nil.
VERB is \"CREATE\" or \"DROP\".  The token is read from the SQL itself, never
inferred from a directory basename."
  (when (and (stringp text)
             (string-match
              (concat verb
                      " DATABASE\\(?: IF\\(?: NOT\\)? EXISTS\\)? +`?\\([A-Za-z0-9_]+\\)")
              text))
    (match-string 1 text)))

(ert-deftest dorg-test-fixture-store-acquire-release-roundtrip ()
  "`-store-acquire' really CREATES a fresh isolated database and
`-store-release' DROPS exactly that one.  Two acquires issue TWO DISTINCT
`CREATE DATABASE' statements (a fake lease+counter that never creates a DB
cannot pass) and yield two different dolt-dirs; `:stores' rises by one per
acquire.  Releasing the first issues EXACTLY one `DROP DATABASE' of ITS OWN
created token and none of the other's; a second release of the same lease
is a bounded no-op; releasing the second drops exactly its token.  Tokens
are read from the SQL, not inferred from directory basenames.  `:stores'
returns to baseline (before ≠ after both ways).  RED before IMPL."
  ;; Self-check: the DDL token parser (a PURE function; no set/symbol-value)
  ;; is proven BEFORE the absent fixture API fails the test, so the capture
  ;; mechanism itself is never the reason for a red/green.
  (should (equal "test_1" (dorg-test-fixture--ddl-token
                           "CREATE DATABASE test_1" "CREATE")))
  (should (equal "test_2" (dorg-test-fixture--ddl-token
                           "DROP DATABASE IF EXISTS `test_2`" "DROP")))
  (should (null (dorg-test-fixture--ddl-token "SELECT 1" "CREATE")))
  (should (null (dorg-test-fixture--ddl-token
                 "CREATE DATABASE test_1" "DROP")))
  (should (fboundp 'dorg-dolt-fixture-store-acquire))
  (should (fboundp 'dorg-dolt-fixture-store-release))
  (dorg-test-fixture--with-spawn-mocked
    (let (creates drops)
      (cl-letf (((symbol-function 'mysql-query)
                 (lambda (_conn sql &rest _)
                   (let ((c (dorg-test-fixture--ddl-token sql "CREATE"))
                         (d (dorg-test-fixture--ddl-token sql "DROP")))
                     (when c (push c creates))
                     (when d (push d drops)))
                   nil))
                ;; Capture the CLI fallback path too, so the test is robust to
                ;; whichever create/drop seam the fixture uses.
                ((symbol-function 'call-process)
                 (lambda (_prog &optional _in _dest _disp &rest args)
                   (let* ((text (mapconcat (lambda (a) (format "%s" a)) args " "))
                          (c (dorg-test-fixture--ddl-token text "CREATE"))
                          (d (dorg-test-fixture--ddl-token text "DROP")))
                     (when c (push c creates))
                     (when d (push d drops)))
                   0)))
        ;; Capture the SHARED SERVER lease and close it on EVERY path
        ;; (OC isolation blocker): discarding it would leave fixture shared
        ;; state — a lease bound to these now-expiring dynamically-bound
        ;; runtime tables — to contaminate later tests or be reused stale.
        (let ((server-lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (let* ((stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores))
                     (la (funcall 'dorg-dolt-fixture-store-acquire))
                     (lb (funcall 'dorg-dolt-fixture-store-acquire))
                     (da (funcall 'dorg-dolt-fixture-lease-dolt-dir la))
                     (db (funcall 'dorg-dolt-fixture-lease-dolt-dir lb)))
                (setq creates (nreverse creates)) ; chronological: [la-tok lb-tok]
                (should (stringp da))
                (should (stringp db))
                ;; Distinct isolated stores AND two distinct real CREATEs.
                (should-not (equal (file-name-as-directory da)
                                   (file-name-as-directory db)))
                (should (= 2 (length creates)))
                (should-not (equal (nth 0 creates) (nth 1 creates)))
                ;; Acquire transformation: +2 stores.
                (should (= (+ stores0 2)
                           (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
                (let ((tok-a (nth 0 creates))   ; la acquired first
                      (tok-b (nth 1 creates)))
                  ;; Release A: exactly one DROP of A's OWN token, none of B.
                  (funcall 'dorg-dolt-fixture-store-release la)
                  (should (= 1 (cl-count tok-a drops :test #'equal)))
                  (should (= 0 (cl-count tok-b drops :test #'equal)))
                  (should (= (+ stores0 1)
                             (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
                  ;; Second release of A: bounded no-op (no extra DROP).
                  (funcall 'dorg-dolt-fixture-store-release la)
                  (should (= 1 (cl-count tok-a drops :test #'equal)))
                  ;; Release B: exactly one DROP of B's token; stores baseline.
                  (funcall 'dorg-dolt-fixture-store-release lb)
                  (should (= 1 (cl-count tok-b drops :test #'equal)))
                  (should (= stores0
                             (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                        :stores)))))
            (ignore-errors
              (funcall 'dorg-dolt-fixture-close server-lease))))))))

;; Spec 11 (AC: normal unwind)
(ert-deftest dorg-test-fixture-with-store-unwinds-on-error ()
  "`-call-with-store' releases the acquired store on BOTH normal exit and a
signalled error (unwind-protect scope).  Non-vacuous: the error path is
exercised explicitly and still fires exactly one release.  RED before IMPL."
  (should (fboundp 'dorg-dolt-fixture-call-with-store))
  (let ((releases 0))
    (cl-letf (((symbol-function 'dorg-dolt-fixture-store-acquire)
               (lambda (&rest _) 'dorg-test-fixture--store-lease))
              ((symbol-function 'dorg-dolt-fixture-store-release)
               (lambda (lease)
                 (when (eq lease 'dorg-test-fixture--store-lease)
                   (cl-incf releases))
                 nil))
              ((symbol-function 'dorg-dolt-fixture-lease-dolt-dir)
               (lambda (_lease) "/tmp/dorg-fixture-store/")))
      (funcall 'dorg-dolt-fixture-call-with-store (lambda (_dolt-dir) 'ok))
      (should (= 1 releases))
      (should-error
       (funcall 'dorg-dolt-fixture-call-with-store
                (lambda (_dolt-dir) (error "boom"))))
      (should (= 2 releases)))))

;; Spec 12 (AC#1: generic helper has ZERO direct spawn / kill / scan)
(ert-deftest dorg-test-fixture-generic-helper-free-of-spawn-and-scan ()
  "The generic helper `tests/dorg-test-helpers.el' owns ZERO Dolt lifecycle
after migration and DELEGATES to the fixture API only: no
`start-process'/`call-process' spawn; no `process-attributes' PID-metadata
scan; no process-teardown primitive (`delete-process'/`signal-process'/
`kill-process'/`interrupt-process'); no private lifecycle reach-in
\(`--kill-pid'/`--kill-process'/`--stop'/`--publish-claim'/
`dorg-dolt-server-stop-all'); no PUBLIC lifecycle entry point driven from
the helper (`dorg-dolt-start'/`-stop'/`-stop-all' — those are the fixture
module's to call); and no `dorg-test--owned-server-records' shadow
registry.  Forbidding the public entry points too (OC audit) prevents
lifecycle from merely hiding behind renamed symbols in the generic helper.
A reader-walk over every top-level form collects any offending SYMBOL
\(docstrings/comments are not read as symbols, so prose mentions are
ignored).  RED now (the helper still owns lifecycle); GREEN once IMPL
migrates it all into the opaque fixture module."
  (let ((forbidden '(;; direct spawn
                     start-process
                     make-process
                     dorg-dolt-server--spawn-logged
                     call-process
                     ;; PID-metadata scan
                     process-attributes
                     ;; process teardown primitives
                     delete-process
                     signal-process
                     kill-process
                     interrupt-process
                     ;; private lifecycle reach-ins
                     dorg-dolt-server--kill-pid
                     dorg-dolt-server--kill-process
                     dorg-dolt-server--stop
                     dorg-dolt-server--publish-claim
                     dorg-dolt-server-stop-all
                     ;; public lifecycle entry points — the helper must
                     ;; delegate to the fixture API, never drive these itself
                     dorg-dolt-start
                     dorg-dolt-stop
                     dorg-dolt-stop-all
                     ;; shadow ownership registry + its advice
                     dorg-test--publish-claim-advice
                     dorg-test--owned-server-records))
        (max-lisp-eval-depth 10000)
        offenders)
    (cl-labels ((walk (form)
                  (cond
                   ((and (symbolp form) (memq form forbidden))
                    (cl-pushnew form offenders))
                   ((consp form)
                    (walk (car form))
                    (walk (cdr form))))))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "tests/dorg-test-helpers.el" dorg-test-infra--root))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (should (null offenders))))

;;; ─── Fixture correctness failure-injection specs (OC 01:36 / 02:08) ───
;;
;; Focused mutants for the OC binding blockers on the collapsed fixture: they
;; run under the same server-suppressed `dorg-test-fixture--with-spawn-mocked'
;; harness (no real server, no full suite) and each is non-vacuous against the
;; specific pre-collapse bug it pins.

;; Blocker 1 / (E): cold-start counted only after `start-process' returns a
;; proc; every resource unwound on a spawn/readiness failure.
(ert-deftest dorg-test-fixture-shared-start-unwinds-and-guards-cold-start ()
  "A failed shared-server spawn advances NO cold-start when `start-process'
never returns a proc, and TRANSACTIONALLY unwinds every resource on a later
readiness failure (proc, `--procs' registration, connection, durable record)
— leaving no orphan server, registration, or singleton.
Non-vacuous against the prior bug (cold-start bumped before `start-process',
no unwind after the durable-record write)."
  ;; (a) start-process SIGNALS: cold-start not bumped, record undone, no
  ;;     registration, singleton/connection nil.
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (error "spec: spawn failed")))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should (= 0 dorg-dolt-fixture--cold-starts))
        (should (= 1 deletes))
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should-not dorg-dolt-fixture--shared-lease)
        (should-not dorg-dolt-fixture--conn))))
  ;; (b) readiness SIGNALS after a real proc: the spawn genuinely happened
  ;;     (cold-start counted once) yet every resource is unwound via the
  ;;     bounded TERM->KILL helper (proc proven gone) and no lease is cached.
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0) (killed 0) (killed-flag nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (error "spec: readiness failed")))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (cl-incf killed) (setq killed-flag t) nil))
                ((symbol-function 'process-live-p)
                 (lambda (&rest _) (not killed-flag)))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should (= 1 dorg-dolt-fixture--cold-starts))
        (should (= 1 killed))
        (should (= 1 deletes))
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should-not dorg-dolt-fixture--shared-lease)
        (should-not dorg-dolt-fixture--conn)))))

;; Blocker (3rd audit #4): an abort whose cleanup ALSO fails collects every
;; cleanup failure, attempts all steps, and signals a COMPOUND cleanup error.
(ert-deftest dorg-test-fixture-shared-start-abort-signals-compound-cleanup-error ()
  "When the spawn aborts (readiness fails), the proc is proven dead, the
data-dir removal SUCCEEDS, and the durable-record delete then FAILS,
`-shared-start' still attempts EVERY step and signals a COMPOUND
`dorg-dolt-fixture-cleanup-error' — the cleanup failure is reflected in the
SIGNAL, not just logs — while publishing NO lease and registering NO server.
Data-dir removal happens BEFORE the record delete (so a lost store never
outlives its record); here the data-dir succeeds, so the record delete is
reached and its failure surfaces in the compound signal.  Non-vacuous against
an abort that only logs cleanup failures and re-signals the primary error."
  (dorg-test-fixture--with-spawn-mocked
    (let ((dir-deletes 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (error "spec: readiness failed")))
                ((symbol-function 'process-live-p) (lambda (&rest _) nil))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (error "spec: registry-delete wedged")))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure)
                      :type 'dorg-dolt-fixture-cleanup-error)
        ;; Data-dir removal was attempted (and succeeded); the record delete
        ;; was then reached and its failure went into the compound signal.
        (should (= 1 dir-deletes))
        (should-not dorg-dolt-fixture--shared-lease)
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))))))

;; Blocker (final control-flow audit): an OUTER `unwind-protect' runs the full
;; abort cleanup on a QUIT (C-g) too, which a condition-case-only guard would
;; bypass.
(ert-deftest dorg-test-fixture-shared-start-cleans-up-on-quit ()
  "A `quit' (C-g) during the spawn still runs the FULL abort cleanup — the
exact process is terminated (proven gone), the durable record and data-dir are
removed, and NO lease/singleton/connection is published — because an OUTER
`unwind-protect' guards cleanup on every nonlocal exit, not only ordinary
errors.  Non-vacuous against a condition-case-only abort that a quit bypasses."
  (dorg-test-fixture--with-spawn-mocked
    (let ((killed 0) (killed-flag nil) (deletes 0) (dir-deletes 0) (got-quit nil))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (cl-incf killed) (setq killed-flag t) nil))
                ((symbol-function 'process-live-p)
                 (lambda (&rest _) (not killed-flag)))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil)))
        (condition-case nil
            (funcall 'dorg-dolt-fixture-shared-ensure)
          (quit (setq got-quit t)))
        (should got-quit)
        (should (= 1 killed))
        (should (= 1 deletes))
        (should (= 1 dir-deletes))
        (should-not dorg-dolt-fixture--shared-lease)
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should-not dorg-dolt-fixture--conn)))))

;; Blocker B (OC 02:44): the abort must PROVE the exact proc dead before
;; removing the `--procs' registration, data-dir, or durable record.  A kill
;; that leaves the proc still live RETAINS runtime + durable + dir.
(ert-deftest dorg-test-fixture-shared-start-abort-kill-still-live-retains ()
  "When the spawn aborts AFTER `--procs' registration (mysql-connect fails)
and the kill FAILS to terminate the process (it stays live), `-shared-start'
RETAINS the runtime registration, the durable record, and the data-dir —
reclaiming NOTHING — and signals a COMPOUND cleanup error naming the
still-live process.  Non-vacuous against an abort that removes the procs
registration / durable record / data-dir before proving the proc dead (which
would orphan a live server from its ownership metadata)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0) (dir-deletes 0))
      (cl-letf (((symbol-function 'mysql-connect)
                 (lambda (&rest _) (error "spec: connect failed")))
                ;; Kill cannot terminate it; the process stays live throughout.
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (error "spec: kill failed")))
                ((symbol-function 'process-live-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure)
                      :type 'dorg-dolt-fixture-cleanup-error)
        ;; Runtime registration RETAINED (proc still live): --procs still holds
        ;; the entry that was puthash'd before mysql-connect.
        (should (= 1 (hash-table-count dorg-dolt-server--procs)))
        ;; Durable record + data-dir RETAINED — nothing reclaimed.
        (should (= 0 deletes))
        (should (= 0 dir-deletes))
        (should-not dorg-dolt-fixture--shared-lease)))))

;; Blocker B (OC 02:44): once the proc is dead, the data-dir is deleted BEFORE
;; the record; a data-dir delete FAILURE RETAINS the record (the store stays
;; reapable).
(ert-deftest dorg-test-fixture-shared-start-abort-data-dir-failure-retains-record ()
  "When the spawn aborts, the proc is proven dead, and the data-dir delete
FAILS, `-shared-start' does NOT delete the durable record — the physical
store still exists, so its ownership record must survive to reap it — and
signals a COMPOUND cleanup error.  Non-vacuous against an abort that deletes
the record regardless of the data-dir outcome (stranding the store)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (error "spec: readiness failed")))
                ((symbol-function 'process-live-p) (lambda (&rest _) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (error "spec: data-dir removal wedged")))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure)
                      :type 'dorg-dolt-fixture-cleanup-error)
        ;; Data-dir delete failed -> the record was NOT deleted.
        (should (= 0 deletes))
        (should-not dorg-dolt-fixture--shared-lease)
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))))))

;; Blocker B (OC 02:44): the abort deletes ONLY a record THIS invocation
;; wrote; a failed registry-WRITE leaves a preexisting record untouched.
(ert-deftest dorg-test-fixture-shared-start-abort-preserves-prior-record ()
  "When the ownership WRITE itself fails, the abort must NOT delete the
durable record — the write never succeeded, so any preexisting record is not
this invocation's to consume.  A real preexisting record file for the shared
key survives, and the record delete is never invoked.  Non-vacuous against an
abort that unconditionally deletes the record key (destroying a prior run's
crash-net after its own write failed)."
  (dorg-test-fixture--with-spawn-mocked
    (let* ((deletes 0)
           (key (dorg-test--registry-shared-key))
           (prior (expand-file-name (concat key ".eld") dorg-test--registry-dir)))
      (with-temp-file prior (prin1 (list :owner-pid 4242 :root "/tmp/prior/")
                                   (current-buffer)))
      (cl-letf (((symbol-function 'dorg-test--registry-write)
                 (lambda (&rest _) (error "spec: registry write failed")))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (k) (cl-incf deletes)
                   (let ((f (expand-file-name (concat k ".eld")
                                              dorg-test--registry-dir)))
                     (when (file-exists-p f) (delete-file f))))))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        ;; The record delete was never invoked; the prior record survives.
        (should (= 0 deletes))
        (should (file-exists-p prior))
        (should-not dorg-dolt-fixture--shared-lease)))))

;; Blocker B (OC 02:44): the OUTER `unwind-protect' runs the full abort cleanup
;; on an arbitrary NONLOCAL EXIT (a `throw'), which a condition-case-only guard
;; catching only (error quit) would bypass entirely.
(ert-deftest dorg-test-fixture-shared-start-cleans-up-on-nonlocal-throw ()
  "A `throw' (neither error nor quit) during the spawn still runs the FULL
abort cleanup — the exact process is terminated (proven gone), the durable
record and data-dir are removed, no lease/singleton/connection is published —
and the throw propagates to the enclosing `catch'.  Non-vacuous against a
condition-case-only abort (matching only `(error quit)') that a bare `throw'
sails past, leaking the process, record, and data-dir."
  (dorg-test-fixture--with-spawn-mocked
    (let ((killed 0) (killed-flag nil) (deletes 0) (dir-deletes 0) caught)
      (cl-letf (((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (throw 'dorg-test-fixture--escape 'boom)))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (cl-incf killed) (setq killed-flag t) nil))
                ((symbol-function 'process-live-p)
                 (lambda (&rest _) (not killed-flag)))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil)))
        (setq caught (catch 'dorg-test-fixture--escape
                       (funcall 'dorg-dolt-fixture-shared-ensure)))
        (should (eq caught 'boom))
        (should (= 1 killed))
        (should (= 1 deletes))
        (should (= 1 dir-deletes))
        (should-not dorg-dolt-fixture--shared-lease)
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should-not dorg-dolt-fixture--conn)))))

;; OC REFINE (blocker 2): a failed start whose disconnect FAILS must RETAIN the
;; exact conn (the failed-start sentinel has no conn slot; the global is the
;; retryable handle) so a later drain/finalize retries it, while the remaining
;; proc/record/dir cleanup still runs and the sentinel is set.
(ert-deftest dorg-test-fixture-shared-start-failed-disconnect-retains-conn ()
  "When a start aborts AFTER the mysql connection is established and the cleanup
disconnect FAILS, `-shared-start' RETAINS the exact conn in
`dorg-dolt-fixture--conn' (reachable for retry), still runs the proc/record/
data-dir cleanup, and retains a sentinel; a later `drain-failed-start' RETRIES
the disconnect and clears the conn.  Non-vacuous against an unconditional
`(setq dorg-dolt-fixture--conn nil)' that drops the exact conn on a failed
disconnect (leaking an unreachable connection)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((disconnect-ok nil) (deletes 0) (dir-deletes 0))
      (cl-letf (;; Proc reads DEAD so cleanup proceeds to reclaim dir/record.
                ((symbol-function 'process-live-p)
                 (lambda (p) (not (eq p 'dorg-test-fixture--fake-proc))))
                ;; Inject a nonlocal exit AFTER --conn is set, at the ordinary
                ;; server-lease-construction seam (the inlined constructor is
                ;; not interceptable).
                ((symbol-function 'dorg-dolt-fixture--build-server-lease)
                 (lambda (&rest _) (error "spec: post-conn failure")))
                ((symbol-function 'mysql-disconnect)
                 (lambda (&rest _)
                   (unless disconnect-ok (error "spec: disconnect wedged"))))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure)
                      :type 'dorg-dolt-fixture-cleanup-error)
        ;; Exact conn RETAINED (reachable) despite the failed disconnect...
        (should (eq dorg-dolt-fixture--conn 'dorg-test-fixture--fake-conn))
        ;; ...and the rest of the cleanup STILL ran.
        (should (= 1 dir-deletes))
        (should (= 1 deletes))
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should dorg-dolt-fixture--failed-start-lease)
        ;; The drain retries the disconnect (now succeeding) and clears it.
        (setq disconnect-ok t)
        (should-not (funcall 'dorg-dolt-fixture-drain-failed-start))
        (should-not dorg-dolt-fixture--conn)
        (should-not dorg-dolt-fixture--failed-start-lease)))))

;; OC REFINE (blocker 2): a `quit' (C-g) mid-disconnect must not abort the rest
;; of the unwind — the cleanup steps are guarded for BOTH `error' and `quit'.
(ert-deftest dorg-test-fixture-shared-start-quit-during-disconnect-continues-cleanup ()
  "A `quit' during the failed-start disconnect is CAPTURED (guarded for
`(error quit)'), so the proc/record/data-dir cleanup still runs, the exact
conn is retained, and the sentinel is set.  Non-vacuous against a disconnect
guarded for `error' ONLY, which a `quit' sails past — skipping the remaining
cleanup and the sentinel."
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0) (dir-deletes 0))
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (p) (not (eq p 'dorg-test-fixture--fake-proc))))
                ((symbol-function 'dorg-dolt-fixture--build-server-lease)
                 (lambda (&rest _) (error "spec: post-conn failure")))
                ((symbol-function 'mysql-disconnect)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure)
                      :type 'dorg-dolt-fixture-cleanup-error)
        ;; The quit was captured; the remaining cleanup RAN and the conn is
        ;; retained (disconnect never confirmed).
        (should (= 1 dir-deletes))
        (should (= 1 deletes))
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))
        (should dorg-dolt-fixture--failed-start-lease)
        (should (eq dorg-dolt-fixture--conn 'dorg-test-fixture--fake-conn))))))

;; Blocker 2: a lease whose PROCESS IS DEAD is recovered/restarted, never
;; returned.  (A live-but-unlisted lease instead fails closed — see the
;; dedicated fail-closed mutant below.)
(ert-deftest dorg-test-fixture-shared-ensure-restarts-stale-lease ()
  "A cached shared lease is reused ONLY while its process is live AND owned in
the public inventory.  When the server PROCESS has actually DIED,
`-shared-ensure' RESTARTS — a second spawn, a NEW lease — and never returns
the stale handle; the stale lease is marked closed.  Non-vacuous against a
reuse-if-not-closed check that hands back the dead lease.
The staleness here is a PROVEN-DEAD process (`process-live-p' → nil), the
only condition that authorises recovery — distinct from the live-but-unlisted
inconsistency, which must fail closed."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc)))
        (let ((lease1 (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should (= 1 spawns))
          ;; The server PROCESS is actually dead (not merely absent from the
          ;; inventory): the reuse gate fails on liveness, so recovery is
          ;; legitimate and a restart follows.
          (cl-letf (((symbol-function 'process-live-p) (lambda (&rest _) nil)))
            (let ((lease2 (funcall 'dorg-dolt-fixture-shared-ensure)))
              (unwind-protect
                  (progn
                    (should (= 2 spawns))
                    (should-not (eq lease1 lease2))
                    (should (dorg-dolt-fixture--lease-closed lease1)))
                (ignore-errors (funcall 'dorg-dolt-fixture-close lease2))))))))))

;; Blocker 3a / (F): a failed store DROP keeps the lease open and registered.
(ert-deftest dorg-test-fixture-store-release-retryable-on-failed-drop ()
  "A store DROP that FAILS leaves the lease OPEN and registered — `:stores'
unchanged, the lease not marked closed — so the release is retryable; a
subsequent successful DROP then closes it and returns `:stores' to baseline.
Non-vacuous against a release that marks the lease closed before confirming
the DROP (which would leak the store and wrongly decrement)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure))
          (fail t))
      (unwind-protect
          (cl-letf (((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (when (and fail (string-match-p "DROP DATABASE" sql))
                         (error "spec: injected DROP failure"))
                       nil)))
            (let* ((lease (funcall 'dorg-dolt-fixture-store-acquire))
                   (stores1 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should (= 1 stores1))
              (should-error (funcall 'dorg-dolt-fixture-store-release lease))
              (should-not (dorg-dolt-fixture--lease-closed lease))
              (should (= 1 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (setq fail nil)
              (funcall 'dorg-dolt-fixture-store-release lease)
              (should (dorg-dolt-fixture--lease-closed lease))
              (should (= 0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; Blocker 3b / (F): shared close fails loud on a nil stop, ownership retained.
(ert-deftest dorg-test-fixture-shared-close-fails-loud-on-nil-stop ()
  "`-close' of the shared server FAILS LOUD when the PUBLIC `dorg-dolt-stop'
cannot confirm the stop (nil return): it signals, the singleton and durable
record are RETAINED, and the lease stays OPEN — the close is retryable.  A
later stop that confirms then closes it exactly once.  Non-vacuous against a
close that marks closed / clears the singleton / deletes the record before
confirming the stop."
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0) (stop-ok nil))
      (cl-letf (((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _)
                   (when stop-ok (setcar dorg-test-fixture--shared-proc-alive nil))
                   stop-ok))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          (should (eq lease dorg-dolt-fixture--shared-lease))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (= 0 deletes))
          (setq stop-ok t)
          (funcall 'dorg-dolt-fixture-close lease)
          (should (dorg-dolt-fixture--lease-closed lease))
          (should-not dorg-dolt-fixture--shared-lease)
          (should (= 1 deletes)))))))

(ert-deftest dorg-test-9w3c4-fixture-live-disconnect-precedes-stop ()
  "The fixture management connection closes while its exact server is live.
The healthy path orders disconnect before the public stop.  If disconnect
signals, public stop is never called and the live lease plus exact connection
remain retryable.  The old finalizer stops first and disconnects only after
the peer is dead, so both halves are transformational."
  ;; Healthy order.
  (dorg-test-fixture--with-spawn-mocked
    (let (events)
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (_conn) (push 'disconnect events)))
                ((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _)
                   (push 'stop events)
                   (setcar dorg-test-fixture--shared-proc-alive nil)
                   t)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (funcall 'dorg-dolt-fixture-close lease)
          (should (equal (nreverse events) '(disconnect stop)))))))
  ;; Refuse server death when the live disconnect is not confirmed.
  (dorg-test-fixture--with-spawn-mocked
    (let ((stops 0))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (_conn) (error "spec: disconnect failed")))
                ((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _)
                   (cl-incf stops)
                   (setcar dorg-test-fixture--shared-proc-alive nil)
                   t)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
              (conn dorg-dolt-fixture--conn))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          (should (= 0 stops))
          (should (car dorg-test-fixture--shared-proc-alive))
          (should (eq conn dorg-dolt-fixture--conn))
          (should (eq lease dorg-dolt-fixture--shared-lease))
          (should-not (dorg-dolt-fixture--lease-stopped lease))
          (should-not (dorg-dolt-fixture--lease-closed lease)))))))

(ert-deftest dorg-test-9w3c4-fixture-dead-connection-never-sends-quit ()
  "A proven-dead fixture peer uses local discard, never `mysql-disconnect'.
A discard failure retains the exact connection and stopped lease for retry;
the successful retry clears the connection and closes once without invoking
the public stop.  This fails before tracked-case because finalization sends
COM_QUIT through `mysql-disconnect' after peer death."
  (dorg-test-fixture--with-spawn-mocked
    (let ((disconnects 0) (discards 0) (stops 0) (discard-ok nil))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (&rest _) (cl-incf disconnects)))
                ((symbol-function 'dorg-dolt-fixture--discard-connection-resources)
                 (lambda (_conn)
                   (cl-incf discards)
                   (unless discard-ok (error "spec: local discard failed"))))
                ((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _) (cl-incf stops) t)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
              (conn dorg-dolt-fixture--conn))
          ;; Exact server death is independently proven before close starts.
          (setcar dorg-test-fixture--shared-proc-alive nil)
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          (should (= 0 disconnects))
          (should (= 1 discards))
          (should (= 0 stops))
          (should (eq conn dorg-dolt-fixture--conn))
          (should (dorg-dolt-fixture--lease-stopped lease))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          ;; The same stopped lease resumes finalization without a public stop.
          (setq discard-ok t)
          (funcall 'dorg-dolt-fixture-close lease)
          (should (= 0 disconnects))
          (should (= 2 discards))
          (should (= 0 stops))
          (should-not dorg-dolt-fixture--conn)
          (should (dorg-dolt-fixture--lease-closed lease)))))))

(ert-deftest dorg-test-9w3c4-local-discard-closes-process-and-buffer ()
  "The dead-peer discard seam closes local resources with zero protocol I/O."
  (should (fboundp 'dorg-dolt-fixture--discard-connection-resources))
  (let* ((buffer (generate-new-buffer " *dorg-9w3c4-discard*"))
         (proc (start-process "dorg-9w3c4-discard" buffer "sleep" "300"))
         (conn (make-mysql-conn :process proc :buf buffer)))
    (unwind-protect
        (progn
          (funcall 'dorg-dolt-fixture--discard-connection-resources conn)
          (should-not (process-live-p proc))
          (should-not (buffer-live-p buffer)))
      (when (process-live-p proc) (delete-process proc))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;; OC REFINE (blocker 3): a truthy `dorg-dolt-stop' is NOT proof of death.
;; `--shared-close' must require the lease's EXACT proc dead before finalizing;
;; a still-live proc PRESERVES the open lease, registry, and data-dir.
(ert-deftest dorg-test-fixture-shared-close-requires-exact-proc-death ()
  "When `dorg-dolt-stop' returns t but the lease's process stays LIVE (a stop
that released the `--procs' entry without a post-kill probe), `-close' SIGNALS
and reclaims NOTHING: the lease stays OPEN and not `stopped', the singleton is
retained, and the durable record + data-dir are untouched (finalize never
ran).  A later close once the proc is proven dead completes exactly once.
Non-vacuous against a close that treats truthy `dorg-dolt-stop' as death and
finalizes over a live server (deleting its record/data-dir and orphaning it)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0) (dir-deletes 0) (proc-live t))
      (cl-letf (((symbol-function 'dorg-dolt-stop) (lambda (&rest _) t))
                ;; Stop returns t but does NOT terminate the process.
                ((symbol-function 'process-live-p) (lambda (&rest _) proc-live))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf dir-deletes) nil)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          ;; Preserved: nothing finalized while the proc is live.
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should-not (dorg-dolt-fixture--lease-stopped lease))
          (should (eq lease dorg-dolt-fixture--shared-lease))
          (should (= 0 deletes))
          (should (= 0 dir-deletes))
          ;; Proc now proven dead -> the retry closes exactly once.
          (setq proc-live nil)
          (funcall 'dorg-dolt-fixture-close lease)
          (should (dorg-dolt-fixture--lease-closed lease))
          (should-not dorg-dolt-fixture--shared-lease)
          (should (= 1 deletes)))))))

;; OC REFINE (blocker 2): retry-safe against trunk public-stop semantics — a
;; first stop that returns t but leaves the proc LIVE (erasing state), then a
;; repeat stop that returns nil once the proc is DEAD, must still finalize.
(ert-deftest dorg-test-fixture-shared-close-retry-after-live-stop-completes ()
  "Call 1: `dorg-dolt-stop' returns t but the proc stays LIVE — close FAILS and
retains everything (no finalize).  Call 2: the proc is now DEAD and a repeat
stop returns nil (already released) — close must NOT require another truthy
stop; it finalizes off the proven-dead proc and completes exactly once.
Non-vacuous against a close that re-issues `dorg-dolt-stop' on retry and wedges
on its nil return (never finalizing)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((proc-live t) (stop-ret t) (deletes 0))
      (cl-letf (((symbol-function 'dorg-dolt-stop) (lambda (&rest _) stop-ret))
                ((symbol-function 'process-live-p) (lambda (&rest _) proc-live))
                ((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          ;; Call 1: truthy stop but proc live -> fail, retain everything.
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (= 0 deletes))
          ;; Proc has since died; a repeat stop now returns nil.
          (setq proc-live nil stop-ret nil)
          ;; Call 2: retry finalizes off the proven-dead proc.
          (funcall 'dorg-dolt-fixture-close lease)
          (should (dorg-dolt-fixture--lease-closed lease))
          (should-not dorg-dolt-fixture--shared-lease)
          (should (= 1 deletes)))))))

;; OC REFINE (blocker 1): the SHARED fixture root is identity-gated too — a
;; data-dir swapped to a symlink, observably distinct, or replaced by a
;; non-directory (dangling symlink / regular file) must NEVER be deleted, and
;; finalize must NOT erase store/registry accounting over it.
(ert-deftest dorg-test-fixture-shared-finalize-refuses-foreign-data-dir ()
  "Closing a shared lease whose data-dir was replaced REFUSES to finalize and
preserves everything, for THREE replacements: (a) a symlink to an outside dir
\(target + content survive); (b) an observably distinct directory with foreign
content; (c) a NON-DIRECTORY (a regular file at the path — `file-directory-p'
nil).  In every case close SIGNALS, the lease stays OPEN, the singleton is
retained, and the durable-record delete is NEVER reached.  Non-vacuous against a
finalize that deletes the foreign target, or that (guarding only on
`file-directory-p') skips the delete yet erases store/registry accounting."
  ;; (a) symlink-to-outside.
  (dorg-test-fixture--with-spawn-mocked
    (let ((outside (file-name-as-directory
                    (file-truename (make-temp-file "dorg-sharedout-a-" t))))
          (deletes 0))
      (cl-letf (((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let* ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
               (data-dir (dorg-dolt-fixture--lease-data-dir lease)))
          (unwind-protect
              (progn
                (with-temp-file (concat outside "sentinel.txt") (insert "keep"))
                (delete-directory (directory-file-name data-dir) t)
                (make-symbolic-link (directory-file-name outside)
                                    (directory-file-name data-dir))
                (should-error (funcall 'dorg-dolt-fixture-close lease))
                (should (file-directory-p outside))
                (should (file-exists-p (concat outside "sentinel.txt")))
                (should-not (dorg-dolt-fixture--lease-closed lease))
                (should (eq lease dorg-dolt-fixture--shared-lease))
                (should (= 0 deletes)))
            (ignore-errors (delete-file (directory-file-name data-dir)))
            (ignore-errors (delete-directory outside t)))))))
  ;; (b) preallocated distinct replacement with foreign content.
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0)
          (dir-deletes 0)
          (real-delete (symbol-function 'delete-directory)))
      (cl-letf (((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let* ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
               (data-dir (dorg-dolt-fixture--lease-data-dir lease))
               (recorded-id (dorg-dolt-fixture--lease-root-id lease))
               (swap (dorg-test-infra--install-distinct-directory data-dir))
               (preserved (plist-get swap :preserved)))
          (unwind-protect
              (progn
                (should (equal recorded-id (plist-get swap :original-id)))
                (should-not (equal recorded-id
                                   (plist-get swap :replacement-id)))
                (should (equal (plist-get swap :replacement-id)
                               (dorg-test--path-file-identity data-dir)))
                (with-temp-file (concat data-dir "foreign.txt")
                  (insert "foreign"))
                (cl-letf (((symbol-function 'delete-directory)
                           (lambda (&rest args)
                             (cl-incf dir-deletes)
                             (apply real-delete args))))
                  (should-error (funcall 'dorg-dolt-fixture-close lease)))
                (should (file-directory-p data-dir))
                (should (file-exists-p (concat data-dir "foreign.txt")))
                (should (file-directory-p preserved))
                (should-not (dorg-dolt-fixture--lease-closed lease))
                (should (eq lease dorg-dolt-fixture--shared-lease))
                (should (= 0 deletes))
                (should (= 0 dir-deletes)))
            (ignore-errors (delete-directory data-dir t))
            (ignore-errors (delete-directory preserved t)))))))
  ;; (c) non-directory replacement (a regular file at the data-dir path).
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0))
      (cl-letf (((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let* ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
               (data-dir (dorg-dolt-fixture--lease-data-dir lease)))
          (delete-directory (directory-file-name data-dir) t)
          (with-temp-file (directory-file-name data-dir) (insert "not-a-dir"))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          ;; The foreign FILE is preserved and accounting NOT erased.
          (should (file-exists-p (directory-file-name data-dir)))
          (should-not (file-directory-p data-dir))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (= 0 deletes))
          (ignore-errors (delete-file (directory-file-name data-dir)))))))
  ;; (d) a DANGLING terminal symlink (target absent): `file-exists-p' is nil, so
  ;; a `file-exists-p'-only presence check would skip the delete yet ERASE
  ;; store/registry accounting.  `file-symlink-p' catches it -> refuse+preserve.
  (dorg-test-fixture--with-spawn-mocked
    (let ((deletes 0))
      (cl-letf (((symbol-function 'dorg-test--registry-delete)
                 (lambda (&rest _) (cl-incf deletes) nil)))
        (let* ((lease (funcall 'dorg-dolt-fixture-shared-ensure))
               (data-dir (dorg-dolt-fixture--lease-data-dir lease)))
          (delete-directory (directory-file-name data-dir) t)
          (make-symbolic-link (concat data-dir "no-such-target")
                              (directory-file-name data-dir))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          ;; The dangling link survives and accounting is NOT erased.
          (should (file-symlink-p (directory-file-name data-dir)))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (= 0 deletes))
          (ignore-errors (delete-file (directory-file-name data-dir))))))))

;; OC REFINE (blocker 1): a nil captured identity is NO ownership proof — the
;; abort must PRESERVE the unverified fresh data-dir, never recursively delete
;; it (a fail-open a `(or (null root-id) ...)' gate would allow).
(ert-deftest dorg-test-fixture-shared-start-nil-identity-preserves-data-dir ()
  "When root identity cannot be captured (nil), `-shared-start' FAILS LOUD
before the durable write/spawn AND the abort does NOT delete the unverified
fresh data-dir — a nil identity is no ownership proof, so the path is PRESERVED
\(retained for a later reap) rather than recursively deleted.  Non-vacuous
against a `(or (null root-id) ...)' fail-open that deletes the unverified dir."
  (dorg-test-fixture--with-spawn-mocked
    (let ((created nil))
      (unwind-protect
          (cl-letf* ((real-mtf (symbol-function 'make-temp-file))
                     ((symbol-function 'make-temp-file)
                      (lambda (&rest a)
                        (if (equal (car a) "dorg-dolt-fixture-shared-")
                            (setq created (apply real-mtf a))
                          (apply real-mtf a))))
                     ((symbol-function 'dorg-test--path-file-identity)
                      (lambda (&rest _) nil)))
            (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
            ;; The fresh data-dir was NOT deleted (unverified path preserved).
            (should created)
            (should (file-directory-p created)))
        (ignore-errors (delete-directory created t))))))

;; Blocker C: the runner's teardown drains stores and verifies inventory zero;
;; a store that cannot drop stays registered so the zero-check catches it.
(ert-deftest dorg-test-fixture-runner-teardown-verifies-inventory-zero ()
  "The runner teardown contract: after `dorg-dolt-fixture-release-stores' and
closing the shared server, `dorg-dolt-fixture-inventory' is EXACTLY zero
servers and zero stores; and a store whose DROP fails stays registered so the
`:stores' zero-check CATCHES the leak (the runner then reports and exits
nonzero).  Non-vacuous: proves both the clean path reaches zero and a leaked
store is visible."
  ;; Clean path: acquire two stores, drain, close server -> inventory zero.
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure)))
      (funcall 'dorg-dolt-fixture-store-acquire)
      (funcall 'dorg-dolt-fixture-store-acquire)
      (should (= 2 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
      (funcall 'dorg-dolt-fixture-release-stores)
      (should (= 0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
      (funcall 'dorg-dolt-fixture-close server)
      (let ((inv (funcall 'dorg-dolt-fixture-inventory)))
        (should (= 0 (plist-get inv :servers)))
        (should (= 0 (plist-get inv :stores))))))
  ;; Leak path: a store whose DROP always fails survives the best-effort drain,
  ;; so the inventory :stores is non-zero and the runner's check would fire.
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          (cl-letf (((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (when (string-match-p "DROP DATABASE" sql)
                         (error "spec: DROP wedged"))
                       nil)))
            (funcall 'dorg-dolt-fixture-store-acquire)
            (funcall 'dorg-dolt-fixture-release-stores)
            (should (= 1 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; Blocker (2nd audit #5/#6): CREATE registers nothing on failure; create-test-db
;; releases the store if schema init fails.
(ert-deftest dorg-test-fixture-store-acquire-leaks-nothing-on-failed-create ()
  "`-store-acquire' registers a store lease ONLY after the CREATE succeeds: a
failed CREATE signals and leaves `:stores' untouched (no lease, no count).
Non-vacuous against an acquire that registers before confirming the CREATE."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          (cl-letf (((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (when (string-match-p "CREATE DATABASE" sql)
                         (error "spec: injected CREATE failure"))
                       nil)))
            (let ((stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should-error (funcall 'dorg-dolt-fixture-store-acquire))
              (should (= stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                            :stores)))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

(ert-deftest dorg-test-fixture-create-test-db-releases-store-on-init-failure ()
  "`dorg-test--create-test-db' RELEASES the freshly-acquired store when the
schema/commit initialization fails (one DROP of that store), so a failed seed
never leaks a store — `:stores' returns to baseline.  Non-vacuous against a
create-db that returns the dolt-dir without unwinding the store."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure))
          (drops 0))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-sql-batch)
                     (lambda (&rest _) (error "spec: schema seed failed")))
                    ((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (when (string-match-p "DROP DATABASE" sql) (cl-incf drops))
                       nil)))
            (let ((stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should-error (dorg-test--create-test-db))
              (should (= stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                            :stores)))
              (should (= 1 drops))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; Blocker (2nd audit #8): stale recovery forgets store leases under the dead
;; server's data-dir so `:stores' does not leak.
(ert-deftest dorg-test-fixture-stale-recovery-forgets-store-leases ()
  "A stale-lease recovery FORGETS every store lease under the dead server's
data-dir (so `:stores' does not leak) and then a fresh server starts.
Non-vacuous against a recovery that reclaims the data-dir but leaves the
store leases (and count) dangling."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc)))
        (funcall 'dorg-dolt-fixture-shared-ensure)
        (funcall 'dorg-dolt-fixture-store-acquire)
        (funcall 'dorg-dolt-fixture-store-acquire)
        (should (= 2 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
        ;; The server PROCESS actually dies -> the lease is legitimately stale
        ;; and recovery runs (a live-but-unlisted lease would fail closed).
        (cl-letf (((symbol-function 'process-live-p) (lambda (&rest _) nil)))
          (let ((lease2 (funcall 'dorg-dolt-fixture-shared-ensure)))
            (unwind-protect
                (progn
                  (should (= 2 spawns))
                  (should (= 0 (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                          :stores))))
              (ignore-errors (funcall 'dorg-dolt-fixture-close lease2)))))))))

;; Blocker (2nd audit #3): shared close is retryable across a partial
;; artifact-cleanup failure; a retry does not re-issue the stop.
(ert-deftest dorg-test-fixture-shared-close-retryable-after-artifact-failure ()
  "Shared close is RETRYABLE across a partial artifact-cleanup failure: after
the PUBLIC stop is confirmed the lease is marked `stopped'; if a later
artifact step (data-dir removal) then fails, the error propagates with the
lease still OPEN and STOPPED and the singleton retained — a retry does NOT
re-issue the stop and completes the teardown.  Non-vacuous against a close
that re-stops on retry or marks the lease closed before the artifacts are
reclaimed."
  (dorg-test-fixture--with-spawn-mocked
    (let ((stops 0) (fail-dir t))
      (cl-letf (((symbol-function 'dorg-dolt-stop)
                 (lambda (&rest _) (cl-incf stops)
                   (setcar dorg-test-fixture--shared-proc-alive nil) t))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _)
                   (when fail-dir (error "spec: data-dir removal wedged"))
                   nil)))
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should-error (funcall 'dorg-dolt-fixture-close lease))
          (should (dorg-dolt-fixture--lease-stopped lease))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (eq lease dorg-dolt-fixture--shared-lease))
          (should (= 1 stops))
          (setq fail-dir nil)
          (funcall 'dorg-dolt-fixture-close lease)
          (should (= 1 stops))
          (should (dorg-dolt-fixture--lease-closed lease))
          (should-not dorg-dolt-fixture--shared-lease))))))

(ert-deftest dorg-test-fixture-create-test-db-release-failure-is-loud ()
  "When `dorg-test--create-test-db' seed fails AND the compensating store
release ALSO fails, the error surfaces (fail loud) and the store lease is NOT
silently forgotten — it stays registered (`:stores' unchanged from the
acquire) and retryable.  Non-vacuous against a swallowed release that hides
the leak."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-sql-batch)
                     (lambda (&rest _) (error "spec: schema seed failed")))
                    ((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (if (string-match-p "DROP DATABASE" sql)
                           (error "spec: release DROP failed")
                         nil))))
            (let ((stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should-error (dorg-test--create-test-db))
              ;; The acquired store is NOT forgotten (its release failed).
              (should (= (1+ stores0)
                         (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; Blocker (3rd audit #1): stale recovery must fully finalize or FAIL — never
;; overwrite the singleton with a fresh server on top of a half-cleaned lease.
(ert-deftest dorg-test-fixture-stale-recovery-fails-loud-no-restart ()
  "When stale-lease recovery cannot finalize (an artifact step fails),
`-shared-ensure' FAILS and does NOT start/overwrite a new server on top of
the half-cleaned lease: the error propagates, no second spawn happens, and
the singleton still points at the stale lease (retryable).  Non-vacuous
against a recovery whose failure is swallowed before a fresh spawn overwrites
the singleton."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0) (fail-dir t))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _)
                   (when fail-dir (error "spec: recovery data-dir removal wedged"))
                   nil)))
        (let ((lease1 (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should (= 1 spawns))
          ;; The server PROCESS actually dies -> recovery is legitimately
          ;; attempted; its data-dir removal then fails.
          (cl-letf (((symbol-function 'process-live-p) (lambda (&rest _) nil)))
            ;; Recovery's data-dir removal fails -> ensure signals, no restart.
            (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
            (should (= 1 spawns))
            (should (eq lease1 dorg-dolt-fixture--shared-lease))
            (should-not (dorg-dolt-fixture--lease-closed lease1)))
          (setq fail-dir nil))))))

;; Blocker A (OC 02:44): a lease whose PROCESS IS LIVE but is NOT a live+owned
;; entry in the public inventory must FAIL CLOSED — recovery is only for a
;; process proven dead.
(ert-deftest dorg-test-fixture-shared-ensure-live-proc-unlisted-fails-closed ()
  "A shared lease whose process is LIVE but whose store is NOT a live+owned
entry in the public inventory (missing/incompatible) makes `-shared-ensure'
FAIL CLOSED: it SIGNALS rather than recovering a live server or spawning a
second.  The singleton, the live process, its `--procs' registration, the
connection, the data-dir, and any store leases are ALL preserved untouched,
and NO second spawn occurs.  Non-vacuous against a reuse gate that treats
`not listed' as `stale' and tears a LIVE server down (which would recover a
live process, orphaning it, and/or spawn a duplicate)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc)))
        (let ((lease1 (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should (= 1 spawns))
          (funcall 'dorg-dolt-fixture-store-acquire)
          (let ((procs-before (hash-table-count dorg-dolt-server--procs))
                (stores-before (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                          :stores))
                (conn-before dorg-dolt-fixture--conn)
                (data-dir (dorg-dolt-fixture--lease-data-dir lease1)))
            ;; Process stays LIVE (global `process-live-p' → t) but the public
            ;; inventory no longer lists this store as live+owned.
            (cl-letf (((symbol-function 'dorg-dolt-server-list) (lambda (&rest _) nil)))
              (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
              ;; No second spawn; everything preserved untouched.
              (should (= 1 spawns))
              (should (eq lease1 dorg-dolt-fixture--shared-lease))
              (should-not (dorg-dolt-fixture--lease-closed lease1))
              (should (= procs-before (hash-table-count dorg-dolt-server--procs)))
              (should (= stores-before
                         (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should (eq conn-before dorg-dolt-fixture--conn))
              (should (file-directory-p data-dir)))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease1))))))))

;; Blocker A (OC 02:44): the defensive assertion inside `--recover-stale'
;; refuses to reclaim a lease whose process is still live, even if reached
;; directly (defense in depth behind the fail-closed gate above).
(ert-deftest dorg-test-fixture-recover-stale-refuses-live-process ()
  "`dorg-dolt-fixture--recover-stale' SIGNALS when handed a lease whose server
process is still live, and reclaims NOTHING — no data-dir delete, no store
forget, no singleton clear.  Non-vacuous against a recovery that finalizes
(tears down) a live server's artifacts."
  (dorg-test-fixture--with-spawn-mocked
    (let ((dir-deletes 0))
      (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
        (funcall 'dorg-dolt-fixture-store-acquire)
        (cl-letf (((symbol-function 'delete-directory)
                   (lambda (&rest _) (cl-incf dir-deletes) nil)))
          ;; Process is live (global mock t): recovery must refuse.
          (should-error (dorg-dolt-fixture--recover-stale lease))
          (should (= 0 dir-deletes))
          (should-not (dorg-dolt-fixture--lease-closed lease))
          (should (eq lease dorg-dolt-fixture--shared-lease))
          (should (= 1 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))

;; OC 03:32 (5): a failed start leaving a LIVE un-killable proc blocks retries.
(ert-deftest dorg-test-fixture-failed-start-live-proc-blocks-retry ()
  "After a failed start leaves a LIVE proc that could not be killed, a retry
`-shared-ensure' FAILS CLOSED — zero additional spawns — until that exact proc
is proven dead; then a retry finalizes the orphan and starts fresh.
Non-vacuous against a retry that sees singleton=nil and spawns a duplicate over
the still-live original."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0) (proc-alive t) (connect-fail t))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (if connect-fail (error "spec: connect failed")
                                     'dorg-test-fixture--fake-conn)))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (error "spec: kill failed")))
                ((symbol-function 'process-live-p) (lambda (&rest _) proc-alive)))
        ;; First start: live proc, connect fails, kill fails -> sentinel set.
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should (= 1 spawns))
        (should dorg-dolt-fixture--failed-start-lease)
        (should-not dorg-dolt-fixture--shared-lease)
        ;; Retry while the orphan proc is still live: FAIL CLOSED, no new spawn.
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should (= 1 spawns))
        ;; Exact proc proven dead + connect works -> finalize orphan, start fresh.
        (setq proc-alive nil connect-fail nil)
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (progn
                (should (= 2 spawns))
                (should-not dorg-dolt-fixture--failed-start-lease)
                (should (eq lease dorg-dolt-fixture--shared-lease)))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))))

;; OC 03:51: a dead-proc abort with a retryable artifact leftover also retains a
;; (stopped) sentinel so a retry finalizes it before reusing the same key.
(ert-deftest dorg-test-fixture-failed-start-dead-proc-partial-blocks-retry ()
  "When a start aborts with the proc PROVEN DEAD but a data-dir reclaim FAILS (a
retryable partial), a `stopped' failed-start sentinel is retained so a retry
`-shared-ensure' FINALIZES that exact partial BEFORE reusing the same durable
key or spawning again — never overwriting the retained record and orphaning the
old dir.  Non-vacuous against an abort that sets the sentinel only for a live
proc (a retry would reuse shared-<pid> and overwrite the record)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((spawns 0) (fail-dir t))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (lambda (&rest _) (cl-incf spawns) 'dorg-test-fixture--fake-proc))
                ((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (if fail-dir (error "spec: readiness failed") t)))
                ((symbol-function 'process-live-p) (lambda (&rest _) nil))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (when fail-dir (error "spec: data-dir wedged")) nil)))
        ;; First start: proc dead, but data-dir delete fails -> partial sentinel.
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should (= 1 spawns))
        (should dorg-dolt-fixture--failed-start-lease)
        (should (dorg-dolt-fixture--lease-stopped
                 dorg-dolt-fixture--failed-start-lease))
        (should-not dorg-dolt-fixture--shared-lease)
        ;; The failures clear -> retry finalizes the partial, then starts fresh.
        (setq fail-dir nil)
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (unwind-protect
              (progn
                (should (= 2 spawns))
                (should-not dorg-dolt-fixture--failed-start-lease)
                (should (eq lease dorg-dolt-fixture--shared-lease)))
            (ignore-errors (funcall 'dorg-dolt-fixture-close lease))))))))

;; The shared server's log is a file the server writes itself, never an
;; Emacs process buffer.  A diag-dir log is evidence and survives every exit;
;; a sibling log is removed with the data-dir.

(defun dorg-test-fixture--logging-spawn (sink)
  "Return a mocked spawn that writes simulated Dolt output to its log.
It records (LOGFILE . DATA-DIR) through SINK, a one-argument function."
  (lambda (_name args logfile)
    (write-region "spec: dolt sql-server startup output\n" nil logfile nil 'silent)
    (funcall sink (cons logfile (file-name-as-directory
                                 (cadr (member "--data-dir" args)))))
    'dorg-test-fixture--fake-proc))

(ert-deftest dorg-test-dolt-fixture-failed-start-keeps-diag-log ()
  "A failed start keeps the server's log in the diagnostic directory.
The server writes <diag>/shared-server-<port>.log itself, so the evidence
needs no copy and survives the unwind that removes the data-dir."
  (dorg-test-fixture--with-spawn-mocked
    (let* ((diag (make-temp-file "dorg-fixture-diag-" t))
           (dorg-test-diag-dir diag)
           (seen nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                     (dorg-test-fixture--logging-spawn (lambda (x) (setq seen x))))
                    ((symbol-function 'dorg-dolt-server--wait-ready)
                     (lambda (&rest _) (error "spec: readiness failed")))
                    ((symbol-function 'process-live-p) (lambda (&rest _) nil)))
            (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
            (should (equal (car seen) (expand-file-name "shared-server-3999.log" diag)))
            (should (string-match-p "startup output"
                                    (with-temp-buffer
                                      (insert-file-contents (car seen))
                                      (buffer-string))))
            (should-not (file-exists-p (cdr seen)))
            (should-not (file-exists-p (dorg-dolt-fixture--sibling-log (cdr seen)))))
        (delete-directory diag t)))))

(ert-deftest dorg-test-dolt-fixture-no-diag-leaves-no-stray-log ()
  "Without a diagnostic dir, neither a failed start nor a close leaves a log.
The sibling log beside the data-dir goes after the process is dead and
before the data-dir, on the failure unwind and on a normal close alike.
The lease owns no process buffer."
  (dorg-test-fixture--with-spawn-mocked
    (let ((dorg-test-diag-dir nil) (seen nil) (fail t))
      (cl-letf (((symbol-function 'dorg-dolt-server--spawn-logged)
                 (dorg-test-fixture--logging-spawn (lambda (x) (setq seen x))))
                ((symbol-function 'dorg-dolt-server--wait-ready)
                 (lambda (&rest _) (if fail (error "spec: readiness failed") t))))
        (cl-letf (((symbol-function 'process-live-p) (lambda (&rest _) nil)))
          (should-error (funcall 'dorg-dolt-fixture-shared-ensure)))
        (should (equal (car seen) (dorg-dolt-fixture--sibling-log (cdr seen))))
        (should-not (file-exists-p (car seen)))
        (should-not (file-exists-p (cdr seen)))
        (setq fail nil seen nil)
        (let ((lease (funcall 'dorg-dolt-fixture-shared-ensure)))
          (should-not (dorg-dolt-fixture--lease-buffer lease))
          (should (file-exists-p (car seen)))
          (funcall 'dorg-dolt-fixture-close lease)
          (should-not (file-exists-p (car seen)))
          (should-not (file-exists-p (cdr seen))))))))

;; OC 03:47 (2): store-release removes by the stored key, never re-canonicalizes.
(ert-deftest dorg-test-fixture-store-release-removes-by-stored-key ()
  "`store-release' removes the lease by its EXACT stored key (`lease-dolt-dir'),
not by re-canonicalizing: a store whose canonicalization keeps FAILING is still
removed after its DROP (`:stores' returns to baseline, no permanently-closed
dangling entry).  Non-vacuous against a release that re-runs `--dir-key' for
the remhash and, on a repeated canonicalization failure, signals after the DROP
and leaves a closed entry forever."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure)))
      (unwind-protect
          (let ((lease (funcall 'dorg-dolt-fixture-store-acquire)))
            (should (= 1 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
            (cl-letf (((symbol-function 'dorg-dolt-fixture--dir-key)
                       (lambda (&rest _) (error "spec: canonicalization failed"))))
              (funcall 'dorg-dolt-fixture-store-release lease)
              (should (dorg-dolt-fixture--lease-closed lease))
              (should (= 0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; OC 03:32 (6): store-acquire is transactional past CREATE (DROP on failure).
(ert-deftest dorg-test-fixture-store-acquire-drops-on-post-create-failure ()
  "`store-acquire' is transactional past CREATE: if canonicalization fails AFTER
a successful CREATE, the new database is DROPped (exact db token) so no
untracked database leaks, and the error propagates — no lease registered.
Non-vacuous against an acquire that builds the lease outside any unwind and
leaks the created database on a post-CREATE signal."
  (dorg-test-fixture--with-spawn-mocked
    (let ((server (funcall 'dorg-dolt-fixture-shared-ensure))
          (creates nil) (drops nil))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-dolt-fixture--dir-key)
                     (lambda (&rest _) (error "spec: canonicalization failed")))
                    ((symbol-function 'mysql-query)
                     (lambda (_c sql &rest _)
                       (when (string-match "CREATE DATABASE \\(test_[0-9]+\\)" sql)
                         (push (match-string 1 sql) creates))
                       (when (string-match "DROP DATABASE IF EXISTS \\(test_[0-9]+\\)" sql)
                         (push (match-string 1 sql) drops))
                       nil)))
            (let ((stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory) :stores)))
              (should-error (funcall 'dorg-dolt-fixture-store-acquire))
              (should creates)
              (should (equal creates drops))
              (should (= stores0 (plist-get (funcall 'dorg-dolt-fixture-inventory)
                                            :stores)))))
        (ignore-errors (funcall 'dorg-dolt-fixture-close server))))))

;; OC 04:12 (1): the runner-unwind drain reclaims a LIVE partial in-process.
(ert-deftest dorg-test-fixture-drain-failed-start-live-partial-retry-reaches-zero ()
  "`dorg-dolt-fixture-drain-failed-start' reclaims a LIVE failed-start partial in
THIS process: it retries the exact bounded stop, and once the proc is proven
dead FINALIZES it (data-dir/record/`--procs' reclaimed) and clears the
sentinel, returning nil.  Non-vacuous against a drain that only closes the
singleton lease and leaves the partial (sentinel non-nil, proc registered)."
  (dorg-test-fixture--with-spawn-mocked
    (let ((proc-alive t) (killed 0))
      (cl-letf (((symbol-function 'mysql-connect)
                 (lambda (&rest _) (error "spec: connect failed")))
                ((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (error "spec: first kill fails")))
                ((symbol-function 'process-live-p) (lambda (&rest _) proc-alive)))
        ;; First start leaves a LIVE sentinel (connect fails, kill fails).
        (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
        (should dorg-dolt-fixture--failed-start-lease))
      ;; The unwind retry: this time the kill succeeds -> proc dead -> zero.
      (cl-letf (((symbol-function 'dorg-dolt-server--kill-process)
                 (lambda (&rest _) (cl-incf killed) (setq proc-alive nil) nil))
                ((symbol-function 'process-live-p) (lambda (&rest _) proc-alive)))
        (should-not (funcall 'dorg-dolt-fixture-drain-failed-start))
        (should (= 1 killed))
        (should-not dorg-dolt-fixture--failed-start-lease)
        (should (= 0 (hash-table-count dorg-dolt-server--procs)))))))

;; OC 04:12 (1): a persistently-live partial is preserved, drain returns failure.
(ert-deftest dorg-test-fixture-drain-failed-start-persistent-live-preserves ()
  "When a failed-start partial's process stays LIVE even after the drain retry,
`drain-failed-start' returns a `failed-start-still-live' failure and PRESERVES
the sentinel and its `--procs' registration — never finalizing/forgetting a
live server.  Non-vacuous against a drain that forgets/zeroes a still-live
partial."
  (dorg-test-fixture--with-spawn-mocked
    (cl-letf (((symbol-function 'mysql-connect)
               (lambda (&rest _) (error "spec: connect failed")))
              ((symbol-function 'dorg-dolt-server--kill-process)
               (lambda (&rest _) (error "spec: kill always fails")))
              ((symbol-function 'process-live-p) (lambda (&rest _) t)))
      (should-error (funcall 'dorg-dolt-fixture-shared-ensure))
      (should dorg-dolt-fixture--failed-start-lease)
      (let ((procs-before (hash-table-count dorg-dolt-server--procs)))
        (let ((failure (funcall 'dorg-dolt-fixture-drain-failed-start)))
          (should failure)
          (should (eq 'failed-start-still-live (car failure))))
        (should dorg-dolt-fixture--failed-start-lease)
        (should (= procs-before (hash-table-count dorg-dolt-server--procs)))))))
;;; Source-binding policy: stale .elc must not shadow checked-out source
;;
;; (tracked-case) Focused exact-SHA certification runs invoke the supported test
;; entry point (tests/dorg-tests-runner.el) directly, without the `clean-elc'
;; that `make test' performs.  Emacs' default `load-prefer-newer' is nil, so a
;; leftover dorg*.elc from a prior `make lint/compile' shadows the newer
;; checked-out source on `(require 'dorg ...)' — binding the verdict to stale
;; bytecode (false RED).  The runner establishes a source-binding policy BEFORE
;; the first project require: Mechanism A prefers newer source
;; (`load-prefer-newer' t); Mechanism B fails closed on any elc whose provenance
;; is unprovable (elc not strictly older than its source).
;;
;; RED until tracked-case IMPL defines `dorg-run-tests--enforce-source-binding'
;; and `dorg-run-tests--source-bound-elc-guard' in the runner.

(defun dorg-test-source-binding--write-probe (el value)
  "Write a synth module at EL whose `dorg-srcbind-probe' returns VALUE.
The feature name is derived from EL's base name so distinct modules do not
collide in the `features' cache."
  (let ((feature (file-name-base el)))
    (with-temp-file el
      (insert ";;; -*- lexical-binding: t; -*-\n"
              (format "(defun dorg-srcbind-probe () '%s)\n" value)
              (format "(provide '%s)\n" feature)))
    (intern feature)))

(ert-deftest dorg-test-source-binding-prefers-source-over-stale-elc ()
  "Newer checked-out source must execute even with an older .elc present.
Transformation (tracked-case): seed bytecode that returns \\='BYTECODE, then
rewrite the source to return \\='SOURCE with an mtime strictly newer than the
.elc.  A control module proves the default (unprotected) load path shadows
source with stale bytecode (BEFORE); after
`dorg-run-tests--enforce-source-binding' the SUPPORTED runner loads source
(AFTER).  before (\\='BYTECODE) != after (\\='SOURCE)."
  (let* ((dir (make-temp-file "dorg-srcbind-" t))
         ;; Control module exercised under the DEFAULT policy (load-prefer-newer
         ;; nil) to demonstrate the shadowing bug this policy removes.
         (ctrl-el (expand-file-name "dorg-srcbind-ctrl.el" dir))
         (ctrl-elc (concat ctrl-el "c"))
         (ctrl-feat (dorg-test-source-binding--write-probe ctrl-el 'BYTECODE))
         ;; Protected module exercised through the runner's policy.
         (mod-el (expand-file-name "dorg-srcbind-mod.el" dir))
         (mod-elc (concat mod-el "c"))
         mod-feat
         (load-prefer-newer nil))
    (unwind-protect
        (progn
          ;; ── BEFORE: default policy loads STALE bytecode over newer source ──
          (byte-compile-file ctrl-el)
          (should (file-exists-p ctrl-elc))
          (dorg-test-source-binding--write-probe ctrl-el 'SOURCE)
          (set-file-times
           ctrl-el (time-add (nth 5 (file-attributes ctrl-elc)) 10))
          (should (file-newer-than-file-p ctrl-el ctrl-elc))
          (let ((load-path (cons dir load-path)))
            (require ctrl-feat))
          ;; default load-prefer-newer nil => the older .elc wins => BYTECODE
          (should (eq (dorg-srcbind-probe) 'BYTECODE))

          ;; ── AFTER: the supported runner's policy binds to newer source ──
          (setq mod-feat
                (dorg-test-source-binding--write-probe mod-el 'BYTECODE))
          (byte-compile-file mod-el)
          (should (file-exists-p mod-elc))
          (dorg-test-source-binding--write-probe mod-el 'SOURCE)
          (set-file-times
           mod-el (time-add (nth 5 (file-attributes mod-elc)) 10))
          (should (file-newer-than-file-p mod-el mod-elc))
          (let ((load-path (cons dir load-path)))
            ;; Policy established before the (require ...) below, exactly as the
            ;; runner establishes it before (require 'dorg).
            (dorg-run-tests--enforce-source-binding (list dir))
            (require mod-feat))
          (should (eq (dorg-srcbind-probe) 'SOURCE)))
      (setq load-prefer-newer nil)
      (ignore-errors (unload-feature ctrl-feat t))
      (when mod-feat (ignore-errors (unload-feature mod-feat t)))
      (fmakunbound 'dorg-srcbind-probe)
      (delete-directory dir t))))

(ert-deftest dorg-test-source-binding-fails-closed-on-shadowing-elc ()
  "An .elc not strictly older than its source has unprovable provenance.
Fail closed (tracked-case AC-4): `dorg-run-tests--source-bound-elc-guard' must
signal rather than let a verdict bind to leftover bytecode.  Non-vacuity: an
.elc strictly OLDER than its source (the harmless stale case handled by
`load-prefer-newer') does NOT trip the guard."
  (let* ((dir (make-temp-file "dorg-srcbind-fc-" t))
         (el (expand-file-name "dorg-fc-mod.el" dir))
         (elc (concat el "c")))
    (unwind-protect
        (progn
          (with-temp-file el (insert "(provide 'dorg-fc-mod)\n"))
          (with-temp-file elc (insert ";; leftover bytecode\n"))
          ;; elc mtime >= el mtime => would be loaded under load-prefer-newer.
          (set-file-times
           elc (time-add (nth 5 (file-attributes el)) 10))
          (should-error (dorg-run-tests--source-bound-elc-guard (list dir))
                        :type 'error)
          ;; Control: source strictly newer than elc => harmless, no error.
          (set-file-times
           el (time-add (nth 5 (file-attributes elc)) 10))
          (should (file-newer-than-file-p el elc))
          (should-not (dorg-run-tests--source-bound-elc-guard (list dir))))
      (delete-directory dir t))))

;;; Integration guards: enforcement is automatic, load-ordered, default-scoped
;;
;; The two transformation specs above call the policy helpers DIRECTLY, after
;; the runner (and `dorg') is already loaded, and pass an explicit dir list.
;; They therefore prove the helpers WORK but not that IMPL WIRES them: they stay
;; green even if the load-time `(dorg-run-tests--enforce-source-binding)' call is
;; omitted or placed AFTER `(require 'dorg)', or if the no-argument/default scope
;; fails to cover the real worktree source dirs (broken `load-file-name' capture
;; or default-dir selection).  The two guards below close that gap independently
;; (tracked-case AC-3, reviewer REVIEW reject 2026-07-17):
;;
;;   1. A FRESH-PROCESS load of the SUPPORTED runner, reading back that
;;      enforcement ran AUTOMATICALLY and BEFORE `(require 'dorg)' (via the
;;      runner's own `dorg-run-tests--dorg-loaded-at-enforce' capture), and that
;;      the DEFAULT (no-argument) scope resolved to the exact worktree source
;;      directories (repo root + tests/).
;;   2. A STATIC source-order scan proving the enforcement call textually
;;      precedes the first `(require 'dorg)' — deterministically RED if the call
;;      is deleted or moved below the project require.
;;
;; Contract required of tracked-case IMPL (SPEC gate): the runner, at load time
;; and BEFORE `(require 'dorg)', must (a) define
;; `dorg-run-tests--dorg-loaded-at-enforce' (initial value NOT nil, e.g. a
;; sentinel), (b) define `dorg-run-tests--project-root' from `load-file-name'
;; and `dorg-run-tests--default-source-dirs', and (c) call
;; `(dorg-run-tests--enforce-source-binding)', whose FIRST action records
;; `(setq dorg-run-tests--dorg-loaded-at-enforce (featurep 'dorg))'.  A correct
;; wiring yields the flag nil (dorg not yet loaded); moving the call below the
;; require yields t; deleting it leaves the sentinel / unbound.

(ert-deftest dorg-test-source-binding-enforced-automatically-before-require ()
  "A fresh load of the runner enforces source-binding before loading `dorg'.
Integration guard (tracked-case AC-1/AC-3): spawn a clean `--batch -Q' Emacs,
`load' the SUPPORTED runner (`tests/dorg-tests-runner.el') exactly as an
exact-SHA certification run does, and read back three load-time facts the
helper-only specs cannot observe:

  * `dorg-run-tests--dorg-loaded-at-enforce' is nil — enforcement ran while
    `dorg' was NOT yet a feature, i.e. the load-time call sits BEFORE
    `(require (quote dorg))'.  Moving the call below the require makes this t;
    deleting it leaves the runner's non-nil sentinel (or the symbol unbound).
  * `dorg-run-tests--project-root' VALUE is `file-equal-p' to the worktree root
    even though the child ran from an UNRELATED temp cwd — `load-file-name'
    capture, not implicit `default-directory'.  A cwd-derived project-root would
    resolve to the temp dir and fail this (nav re-cert #2 M1).
  * `(dorg-run-tests--default-source-dirs)' — the NO-ARGUMENT scope — covers the
    exact worktree source directories (repo root AND tests/).

RED before IMPL: the current runner defines none of these symbols, so
`:flag-boundp' is nil.  The load is wrapped in `condition-case' in the child so
the read-back still runs even if the load-time guard legitimately fails closed
on stray bytecode (the flag and project-root are set before the guard runs)."
  (skip-unless noninteractive)
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (root dorg-test-infra--root)
         (expr `(let ((runner (expand-file-name
                               "tests/dorg-tests-runner.el" ,root)))
                  (condition-case err
                      (load runner nil t)
                    (error
                     (princ (format "DORGSRCBIND-LOADERR %s\n"
                                    (error-message-string err)))))
                  (princ
                   (format
                    "DORGSRCBIND-RESULT %S\n"
                    (list
                     :flag-boundp
                     (boundp 'dorg-run-tests--dorg-loaded-at-enforce)
                     :flag
                     (and (boundp 'dorg-run-tests--dorg-loaded-at-enforce)
                          dorg-run-tests--dorg-loaded-at-enforce)
                     :root-boundp
                     (boundp 'dorg-run-tests--project-root)
                     :project-root
                     (and (boundp 'dorg-run-tests--project-root)
                          dorg-run-tests--project-root)
                     :dirs
                     (and (fboundp 'dorg-run-tests--default-source-dirs)
                          (dorg-run-tests--default-source-dirs)))))))
         (buf (generate-new-buffer " *dorg-srcbind-child*"))
         ;; Run the child from an UNRELATED temp cwd so a project-root derived
         ;; from `default-directory' (implicit cwd) instead of `load-file-name'
         ;; resolves to this temp dir, NOT the worktree root — killing the
         ;; cwd-derived mutant when we assert the value below (nav re-cert #2 M1).
         (alien-cwd (file-name-as-directory
                     (make-temp-file "dorg-srcbind-alien-cwd" t))))
    (unwind-protect
        (let ((status
               (let ((default-directory alien-cwd))
                 (call-process
                  emacs nil buf nil
                  "--batch" "-Q"
                  "-L" root
                  "-L" (expand-file-name "tests" root)
                  "-L" (expand-file-name "lib/mysql" root)
                  "-L" (expand-file-name "lib/clime" root)
                  "--eval" (prin1-to-string expr))))
              result)
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "DORGSRCBIND-RESULT " nil t)
              (setq result (read (current-buffer)))))
          (ert-info ((format "child status=%S output=%s"
                             status (with-current-buffer buf (buffer-string))))
            (should result)
            ;; The runner defines the order-capture flag (call not deleted).
            (should (plist-get result :flag-boundp))
            ;; dorg was NOT yet loaded when enforcement ran => call precedes
            ;; (require 'dorg).  Moving it below the require makes this non-nil.
            (should (eq (plist-get result :flag) nil))
            ;; load-file-name-based project-root capture produced a value.
            (should (plist-get result :root-boundp))
            ;; The captured project-root's VALUE equals the worktree root even
            ;; though the child ran from an unrelated cwd — proving it derives
            ;; from `load-file-name', not implicit `default-directory' (M1).
            (let ((pr (plist-get result :project-root)))
              (should pr)
              (should (file-equal-p pr root))
              ;; And it is NOT the alien cwd we launched from.
              (should-not (file-equal-p pr alien-cwd)))
            ;; The DEFAULT (no-argument) scope covers the real worktree dirs.
            (let ((dirs (plist-get result :dirs)))
              (should dirs)
              (should (cl-some (lambda (d) (file-equal-p d root)) dirs))
              (should (cl-some (lambda (d)
                                 (file-equal-p
                                  d (expand-file-name "tests" root)))
                               dirs)))))
      (kill-buffer buf)
      (delete-directory alien-cwd t))))

(ert-deftest dorg-test-source-binding-runner-enforces-before-loading-dorg ()
  "Runner source calls enforcement textually BEFORE the first `(require (quote dorg))'.
Static order guard (tracked-case AC-3), complementing the runtime fresh-process
guard: read every top-level form of `tests/dorg-tests-runner.el' in order and
require that a `(dorg-run-tests--enforce-source-binding ...)' call is present AND
precedes the first `(require (quote dorg))'.  Deterministically RED if the call
is deleted (absent) or moved below the project require (index inversion).

The automatic call must also use the DEFAULT scope: its argument list must carry
no non-nil explicit dirs.  Passing an explicit list — e.g.
`(dorg-run-tests--enforce-source-binding (list \"/wrong\"))' — would let the
runner enforce a scope unrelated to the worktree while
`dorg-run-tests--default-source-dirs' still answers root+tests when queried
separately, so this assertion is what makes the default scope load-bearing (nav
re-cert #2 M2)."
  (let* ((runner (expand-file-name "tests/dorg-tests-runner.el"
                                   dorg-test-infra--root))
         (idx 0)
         enforce-idx enforce-form require-idx)
    (with-temp-buffer
      (insert-file-contents runner)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (null enforce-idx)
                         (eq (car-safe form)
                             'dorg-run-tests--enforce-source-binding))
                (setq enforce-idx idx
                      enforce-form form))
              (when (and (null require-idx)
                         (equal form '(require 'dorg)))
                (setq require-idx idx))
              (setq idx (1+ idx))))
        (end-of-file nil)))
    ;; The enforcement call exists (not deleted).
    (should enforce-idx)
    ;; Sanity: the project require the policy must precede is present.
    (should require-idx)
    ;; Enforcement is established before any project byte-code can load.
    (should (< enforce-idx require-idx))
    ;; The call uses the DEFAULT scope: no non-nil explicit dir arguments.  An
    ;; explicit wrong-scope list (e.g. (list "/wrong")) would satisfy the
    ;; car-only guard above yet bypass the worktree default scope — reject it.
    (should (cl-every #'null (cdr enforce-form)))))

(provide 'dorg-test-infra-tests)
;;; dorg-test-infra-tests.el ends here
