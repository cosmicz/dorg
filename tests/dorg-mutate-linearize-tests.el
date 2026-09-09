;;; dorg-mutate-linearize-tests.el --- Multi-writer linearization SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Executable RED/GREEN contract for tracked-regression — a DOrg-owned, narrow,
;; per-store mutation-linearization boundary for Dolt sources so typed-API
;; clients can safely remove the outer Downstream tracker `.tracker/.lock'.
;;
;; The acceptance criteria (A0..A16.8) live on the bead; each test names the
;; A-item it discharges and its non-vacuity (how it goes RED without the fix).
;; All real-Dolt tests run against an ISOLATED scratch store on the shared
;; per-store sql-server — never the live tracker, never the full suite — and
;; are gated on `(and dorg-test--dolt-available dorg-test--server-port)' because
;; the connection-affine gate needs a real persistent server session (the
;; one-shot CLI seam has none).
;;
;; Focused selector:  make test SELECT='dorg-test-lin-'
;;
;; This file covers the FOUNDATION and GATE/CAS/RE-PLAN layers:
;;   A16.8  reason-enum registration (pure; no Dolt)
;;   A0     GET_LOCK exclusion + canonical key derivation
;;   A16.5  non-circular generation-safe commit recovery (dolt_diff)
;;   A16.6  commit-date skew/ties do not misidentify the generation
;;   A16.7  prune + same-op-id reuse never returns the pruned/old hash
;;   A1     disjoint same-base race -> re-plan, both persist (linear)
;;   A3     same-field writers -> deterministic terminal overlap, no clobber
;;   A15    two inserts same parent -> stale-plan write-skew re-planned away
;;   A13    delta SQL is target-absolute (WHERE id=N / explicit allocated id)
;;   A11    phase timings; :gate-hold EXCLUDES the whole-root :plan
;;   A7     (static) both Dolt executors commit only through the gate seam
;; ...and the EXACTLY-ONCE / public-surface layer (Inc4):
;;   A16.0  allocated insert id is FIXED across re-plans (byte-identical replay)
;;   A16.1  the idempotency preflight is GATE-FIRST (held-gate -> contended)
;;   A16.2  process-death replay -> idempotent :ok, original commit, no reapply
;;   A16.3  op-id reuse with a different intent -> mismatch, ZERO SQL
;;   A16.4  transport loss -> terminal indeterminate; retention window reuse
;;   A14    public op-id echo + phase timings + terminal op-id via api-execute
;; The remaining integration layers (A2, A4-A6, A8-A10, A12) are added as those
;; increments land.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'find-func)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-mutate)
(require 'dorg-api)
(require 'dorg-envelope)
(require 'dorg-ext)
(require 'dorg-repo)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el
(defvar dorg-test--server-port)         ; defined in dorg-dolt-fixture.el

;;; ─── Helpers ──────────────────────────────────────────────────────────

(defun dorg-test-lin--raw-conn (dolt-dir)
  "Open and return a SECOND raw mysql session to DOLT-DIR's shared server.
Bypasses the lease guard entirely (it is not routed through `dorg-sql'), so it
is a genuinely independent session for cross-session GET_LOCK exclusion proof."
  (mysql-connect :host "127.0.0.1"
                 :port (dorg-dolt-server--strict-port dolt-dir)
                 :user "root"
                 :database (dorg-dolt-server--db-name dolt-dir)
                 :tls nil
                 :connect-timeout dorg-sql-connect-timeout
                 :read-idle-timeout dorg-sql-read-timeout))

(defun dorg-test-lin--raw-get-lock (conn key timeout)
  "Return the raw GET_LOCK(KEY,TIMEOUT) result on CONN normalized to 1/0/nil."
  (dorg-sql--lock-result
   (cdar (car (dorg-sql--mysql-exec
               (format "SELECT GET_LOCK(%s, %d) AS got" (dorg-sql-quote key) timeout)
               conn)))))

(defun dorg-test-lin--lease-head (lease)
  "Return HASHOF('HEAD') read on LEASE's exact session."
  (dorg-sql--lease-scalar lease "SELECT HASHOF('HEAD') AS h"))

(defun dorg-test-lin--commit-evidence (lease op-id gen-id digest ts)
  "Commit ONE evidence row (OP-ID GEN-ID DIGEST TS) on LEASE; return the new HEAD.
Ensures the table exists, then commits the INSERT so the row lands on HEAD; the
returned hash is the commit that ADDED the row."
  (dorg-sql--ensure-mutation-log lease)
  (dorg-sql--lease-batch
   lease (list (dorg-sql--mutation-log-insert-sql op-id gen-id digest ts))
   (format "evidence %s" op-id))
  (dorg-test-lin--lease-head lease))

(defun dorg-test-lin--delete-evidence (lease op-id)
  "Commit deletion of OP-ID's current-state evidence row on LEASE (prune model)."
  (dorg-sql--lease-batch
   lease (list (format "DELETE FROM dorg_mutation_log WHERE op_id = %s"
                       (dorg-sql-quote op-id)))
   (format "prune %s" op-id)))

(defun dorg-test-lin--raw-release (conn key)
  "Release GET_LOCK(KEY) on the raw CONN (independent session)."
  (dorg-sql--mysql-exec
   (format "SELECT RELEASE_LOCK(%s)" (dorg-sql-quote key)) conn))

(defun dorg-test-lin--api-apply (src form &rest opts)
  "Run FORM as an apply through the PUBLIC `dorg-api-execute' envelope on SRC.
OPTS are extra keyword args (e.g. :op-id, :base) forwarded verbatim."
  (apply #'dorg-api-execute form :mode 'apply :source src opts))

(defvar dorg-test-lin--lost-response-fired nil
  "Child-process flag set after the 21mx9 witness lets DOLT_COMMIT land.")

(defvar dorg-test-lin--lost-response-conn-live nil
  "Child-process fact: the leased connection remained live at response loss.")

(defvar dorg-test-lin--lost-response-rollbacks 0
  "Child-process count of ROLLBACK statements in the 21mx9 witness.")

(defun dorg-test-lin--lose-first-dolt-commit-response (orig sql conn)
  "Run ORIG SQL on CONN, then lose the first DOLT_COMMIT response.
The server has already accepted the commit when the typed timeout is signalled.
Its 1213-shaped text is deliberate: transport classification must use the
condition type and transaction phase, never feed this prose to the DB classifier."
  (let ((rows (funcall orig sql conn)))
    (if (and (not dorg-test-lin--lost-response-fired)
             (string-match-p "CALL DOLT_COMMIT" sql))
        (progn
          (setq dorg-test-lin--lost-response-fired t
                dorg-test-lin--lost-response-conn-live
                (process-live-p (mysql-conn-process conn)))
          (signal 'mysql-timeout
                  '("[1213] RAW-TRANSPORT response lost after commit")))
      rows)))

(defun dorg-test-lin--count-lost-response-rollback (orig conn sql)
  "Call ORIG on CONN with SQL, counting any forbidden ROLLBACK."
  (when (equal sql "ROLLBACK")
    (cl-incf dorg-test-lin--lost-response-rollbacks))
  (funcall orig conn sql))

;;; ─── Executor-level helpers (gate / CAS / re-plan behavioral tests) ─────

(defun dorg-test-lin--seed (dolt-dir contents &optional root-id)
  "Seed org CONTENTS into DOLT-DIR under ROOT-ID (default 100) and commit.
Returns the root id used."
  (let* ((rid (or root-id 100))
         (stmts (dorg-test--parse-to-sql contents rid)))
    (dorg-sql-batch dolt-dir stmts)
    (dorg-commit dolt-dir "seed")
    rid))

(defun dorg-test-lin--src (dolt-dir root)
  "Return a Dolt mutation source descriptor for DOLT-DIR / ROOT."
  (list :dolt-dir dolt-dir :root-id root))

(defun dorg-test-lin--id (dolt-dir title)
  "Return the heading id titled TITLE in DOLT-DIR (or nil)."
  (alist-get 'id (car (dorg-sql dolt-dir
                                (format "SELECT id FROM headings WHERE title = %s LIMIT 1"
                                        (dorg-sql-quote title))))))

(defun dorg-test-lin--prop (dolt-dir heading-id key)
  "Return the current value of property KEY on HEADING-ID (via the schema view)."
  (alist-get 'prop_value
             (car (dorg-sql dolt-dir
                            (format (concat "SELECT prop_value FROM heading_properties"
                                            " WHERE heading_id = %d AND prop_key = %s LIMIT 1")
                                    heading-id (dorg-sql-quote key))))))

(defun dorg-test-lin--parent-of (dolt-dir heading-id)
  "Return the parent_id of HEADING-ID in DOLT-DIR (an integer or nil)."
  (let ((v (alist-get 'parent_id
                      (car (dorg-sql dolt-dir
                                     (format "SELECT parent_id FROM headings WHERE id = %d"
                                             heading-id))))))
    (cond ((null v) nil) ((stringp v) (string-to-number v)) (t v))))

(defconst dorg-test-lin--repo-root
  (file-name-directory (find-library-name "dorg-mutate"))
  "Absolute path to the repo root (the directory holding the production .el).
Captured at LOAD time so the static call-site audit never depends on an implicit
`default-directory' or a run-time-nil `load-file-name'.")

(defun dorg-test-lin--read-top-forms (file)
  "Return the list of top-level forms in FILE, read with `read'.
Comments are dropped and docstrings kept.  An unreadable FILE yields nil; a
file that fails mid-parse contributes whatever it yielded up to the failure —
so this is an authoritative repository-wide source scan, not a loaded-closure
inspection."
  (let ((forms '()))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t (push (read (current-buffer)) forms))
          (error nil))))
    (nreverse forms)))

(defun dorg-test-lin--form-name (form)
  "Return a display-name string for top-level FORM, or nil.
Handles ANY top-level form, not a fixed set of definer heads: the name is the
second element, with a `(quote NAME)' wrapper (e.g. a `defalias' target)
unwrapped to the bare symbol.  A form with no usable name element falls back to
its head symbol, so an offending caller is always reportable."
  (let ((n (cadr form)))
    (cond ((and (consp n) (eq (car n) 'quote) (symbolp (cadr n)))
           (symbol-name (cadr n)))
          ((and n (symbolp n)) (symbol-name n))
          (n (prin1-to-string n))
          ((symbolp (car form)) (symbol-name (car form))))))

(defun dorg-test-lin--form-calls-p (form callee)
  "Non-nil if sexp FORM structurally contains a CALL to CALLEE (a symbol).
A call is any cons whose car is CALLEE.  Recurses through list structure —
including backquote/quote bodies, so a macro that expands to a call is caught —
but treats atoms, crucially STRINGS, as leaves.  Therefore a CALLEE mention
inside a string literal or docstring, a `#'CALLEE' funcall reference (car
`function'), and a `'CALLEE' datum (car `quote') are NOT counted as calls.
Detection is independent of the enclosing definer head, so there is no fixed
allowlist to evade (the reviewer A7 blocker)."
  (and (consp form)
       (if (eq (car form) callee)
           t
         (let ((walk form) (found nil))
           (while (and (consp walk) (not found))
             (when (dorg-test-lin--form-calls-p (car walk) callee)
               (setq found t))
             (setq walk (cdr walk)))
           found))))

(defun dorg-test-lin--source-defuns-with (needle files)
  "Return the sorted, de-duplicated names of top-level forms across FILES whose
PRINTED form contains NEEDLE (a literal substring).  Scans EVERY top-level
`consp' form with NO definer-head allowlist.  Substring matching is the right
tool for a LITERAL seam such as the `DOLT_COMMIT(' SQL a commit primitive
emits; to detect a FUNCTION CALL use `dorg-test-lin--source-callers', which
matches an actual call form and so ignores a mention inside a string literal."
  (let ((names '()))
    (dolist (f files)
      (dolist (form (dorg-test-lin--read-top-forms f))
        (when (and (consp form)
                   (string-match-p (regexp-quote needle) (prin1-to-string form)))
          (let ((name (dorg-test-lin--form-name form)))
            (when name (push name names))))))
    (sort (delete-dups names) #'string<)))

(defun dorg-test-lin--source-callers (callee files)
  "Return the sorted names of top-level forms across FILES that CALL CALLEE.
A CALL is an actual sub-form `(CALLEE ...)' detected STRUCTURALLY by
`dorg-test-lin--form-calls-p', so it fires regardless of the enclosing definer
head — `defun', `cl-defun', `cl-defmethod', `defmacro', `define-minor-mode',
`define-derived-mode', a `defalias' lambda, or any future function-defining
form — and does NOT count a `#''-quoted reference, a `'CALLEE' datum, or a
CALLEE mention inside a string/docstring.  There is NO fixed definer-head
allowlist to evade (the reviewer A7 blocker)."
  (let ((names '()))
    (dolist (f files)
      (dolist (form (dorg-test-lin--read-top-forms f))
        (when (and (consp form)
                   (dorg-test-lin--form-calls-p form callee))
          (let ((name (dorg-test-lin--form-name form)))
            (when name (push name names))))))
    (sort (delete-dups names) #'string<)))

(defun dorg-test-lin--production-el-files ()
  "Return the absolute paths of every PRODUCTION `.el' file at the repo root.
DISCOVERED from the repository (a non-recursive listing of `dorg-test-lin--repo-root'),
never a curated list — so a NEW production file is audited automatically.  The
tests/ tree is a subdirectory and is therefore not returned; package-bookkeeping
and any stray `-tests.el' at the root are filtered out."
  (seq-remove
   (lambda (f) (string-match-p "\\(-tests\\|-autoloads\\|-pkg\\)\\.el\\'" f))
   (directory-files dorg-test-lin--repo-root t "\\.el\\'")))

(defun dorg-test-lin--child-titles (dolt-dir parent-id)
  "Return the titles of PARENT-ID's direct children ordered by sort_key."
  (mapcar (lambda (r) (alist-get 'title r))
          (dorg-sql dolt-dir
                    (format (concat "SELECT title FROM headings WHERE parent_id = %d"
                                    " ORDER BY sort_key")
                            parent-id))))

(defun dorg-test-lin--commit-count (dolt-dir)
  "Return the number of commits reachable from HEAD in DOLT-DIR."
  (let ((n (alist-get 'n (car (dorg-sql dolt-dir "SELECT COUNT(*) AS n FROM dolt_log")))))
    (if (stringp n) (string-to-number n) n)))

(defun dorg-test-lin--parents-of-head (dolt-dir)
  "Return the number of parents of the current HEAD commit (1 = linear, 2 = merge)."
  (let* ((h (dorg-head-commit dolt-dir))
         (n (alist-get 'n
                       (car (dorg-sql dolt-dir
                                      (format (concat "SELECT COUNT(*) AS n FROM dolt_commit_ancestors"
                                                      " WHERE commit_hash = %s")
                                              (dorg-sql-quote h)))))))
    (if (stringp n) (string-to-number n) n)))

;;; ─── Cross-process writer harness (real OS-process concurrency) ─────────
;;
;; A2 / A4 / A7(dynamic) / A16 demand REAL separate OS processes racing on the
;; shared per-store sql-server, not in-process approximations.  Each child is a
;; fresh `emacs --batch' that ATTACHES to the already-running shared test server
;; as a pure SQL client: it pre-seeds `dorg-dolt-server--procs' with the shared
;; PORT so `dorg-sql' and the gate lease connect to 127.0.0.1:PORT and NEVER
;; start a server of their own — sidestepping the
;; documented CLI-subprocess/shared-server deadlock class (the child is a plain
;; SQL client, exactly like the A0/A4 raw sessions, just in another process).
;; The child runs a BODY sexp and emits its result between `@@LIN@@' markers on
;; stdout; the parent reads it back with `read-from-string'.

(defun dorg-test-lin--attach-shared-server (dolt-dir port)
  "CHILD-side: pin `server' transport and register PORT as DOLT-DIR's server so
`dorg-sql' and the gate lease attach to the already-running shared server
WITHOUT starting one.  A dummy live process keeps the
`dorg-dolt-server--procs' entry alive so
`dorg-dolt-server-port' returns PORT."
  (setq dorg-sql-transport 'server)
  ;; A short-lived dummy keeps the procs entry "live" for the child's DB work
  ;; (well under 60s); if the child dies abnormally the orphan self-reaps quickly.
  (let ((key (dorg-dolt-server--canonical-dir dolt-dir))
        (dummy (start-process "dorg-lin-dummy" nil "sleep" "60")))
    (set-process-query-on-exit-flag dummy nil)
    (puthash key (cons dummy port) dorg-dolt-server--procs)))

(defun dorg-test-lin--emit (value)
  "CHILD-side: print VALUE as one readable line delimited by `@@LIN@@' markers."
  (princ (format "\n@@LIN@@ %s @@LIN@@\n" (prin1-to-string value))))

(defun dorg-test-lin--writer-form (dolt-dir port root body)
  "Build the child `--eval' form: attach to PORT, bind `src' for DOLT-DIR/ROOT,
run BODY under a typed-error guard, and emit the result.  BODY is a sexp
evaluated in the child; it may reference `src' and return any readable value."
  `(progn
     (setq debug-on-error nil)
     (dorg-test-lin--attach-shared-server ,dolt-dir ,port)
     (let ((src (dorg-test-lin--src ,dolt-dir ,root)))
       (ignore src)
       (dorg-test-lin--emit
        (condition-case e ,body
          (dorg-mutate-error
           (list :error 'mutate
                 :reason (dorg-mutate-error-reason e)
                 :data (dorg-mutate-error-data e)))
          (error (list :error 'other :message (format "%S" e))))))))

(defun dorg-test-lin--spawn (label dolt-dir port root body)
  "Spawn an async child emacs running BODY against DOLT-DIR/ROOT on PORT.
Returns (LABEL PROC BUFFER).  The child inherits the parent load-path via
EMACSLOADPATH and loads the linearize suite for the `dorg-test-lin--' helpers."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (buf (generate-new-buffer (format " *lin-child-%s*" label)))
         (form (dorg-test-lin--writer-form dolt-dir port root body))
         (process-environment
          (cons (concat "EMACSLOADPATH=" (string-join load-path ":") ":")
                process-environment))
         (proc (make-process
                :name (format "lin-child-%s" label)
                :buffer buf
                :noquery t
                :command (list emacs "--batch" "-Q"
                               "-l" "dorg-mutate-linearize-tests"
                               "--eval" (prin1-to-string form)))))
    (list label proc buf)))

(defun dorg-test-lin--parse-child (buf)
  "Read the `@@LIN@@'-delimited result emitted into BUF, or an error plist."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (re-search-forward "@@LIN@@ \\(.*\\) @@LIN@@" nil t)
        (car (read-from-string (match-string 1)))
      (list :error 'no-result :raw (buffer-string)))))

(cl-defun dorg-test-lin--run-writers (specs timeout &key expected-exit after-spawn)
  "Run SPECS — a list of (LABEL DOLT-DIR PORT ROOT BODY) — as CONCURRENT child
processes; wait up to TIMEOUT seconds for all; return an alist (LABEL . RESULT).
Kills stragglers and reaps buffers on the way out.

EXPECTED-EXIT defaults to 0: every child MUST terminate with that exit status, or
the run `should'-fails — so a child that emits a success marker and then dies
during teardown is caught (a bare parse can no longer mask an abnormal exit).
Pass an integer (e.g. 43 for a deliberate `kill-emacs') to assert an intended
abnormal exit, or `:any' to skip the check entirely.

AFTER-SPAWN, when non-nil, is a nullary function invoked ONCE right after every
child is spawned and BEFORE the completion wait — the hook a cross-process barrier
uses to await all children at the rendezvous and release them simultaneously."
  (let* ((running (mapcar (lambda (s) (apply #'dorg-test-lin--spawn s)) specs))
         (deadline (+ (float-time) timeout))
         (expected (if (null expected-exit) 0 expected-exit)))
    (unwind-protect
        (progn
          (when after-spawn (funcall after-spawn))
          (while (and (cl-some (lambda (r) (process-live-p (nth 1 r))) running)
                      (< (float-time) deadline))
            (accept-process-output nil 0.2))
          (unless (eq expected :any)
            (dolist (r running)
              (should (eq 'exit (process-status (nth 1 r))))
              (should (= expected (process-exit-status (nth 1 r))))))
          (mapcar (lambda (r)
                    (cons (nth 0 r) (dorg-test-lin--parse-child (nth 2 r))))
                  running))
      (dolist (r running)
        (when (process-live-p (nth 1 r)) (delete-process (nth 1 r)))
        (when (buffer-live-p (nth 2 r)) (kill-buffer (nth 2 r)))))))

;;; ─── Cross-process rendezvous barrier (prove SIMULTANEOUS contention) ───
;;
;; File-based so it works across independent OS processes: each child, at a fixed
;; point in its execution, drops `PHASE-ready-LABEL' and spins until the parent
;; drops `PHASE-go'.  The parent awaits N ready markers (all children proven at
;; the same point) and then releases them together.  Placed AFTER the outside-gate
;; plan and BEFORE gate entry, it proves every writer resolved the source and
;; planned at the SAME base before any contends — so a green A2 cannot be an
;; artifact of OS-serialized startup.

(defun dorg-test-lin--barrier-signal-and-wait (barrier phase label timeout)
  "CHILD-side rendezvous: drop BARRIER/PHASE-ready-LABEL, then block until
BARRIER/PHASE-go appears (or TIMEOUT s elapse).  Reaching here proves this child
arrived at PHASE; the wait holds it until every sibling has too."
  (let ((ready (expand-file-name (format "%s-ready-%s" phase label) barrier))
        (go (expand-file-name (format "%s-go" phase) barrier))
        (deadline (+ (float-time) timeout)))
    (with-temp-file ready (insert "1"))
    (while (and (not (file-exists-p go)) (< (float-time) deadline))
      (sleep-for 0.02))))

(defun dorg-test-lin--barrier-release (barrier phase n timeout)
  "PARENT-side: await N `PHASE-ready-*' markers in BARRIER, then drop `PHASE-go'
to release every child at PHASE simultaneously.  Returns t iff all N arrived
within TIMEOUT (a straggler leaves the go-file unwritten and returns nil)."
  (let ((deadline (+ (float-time) timeout))
        (rx (format "\\`%s-ready-" (regexp-quote phase))))
    (cl-flet ((ready-count ()
                (length (ignore-errors (directory-files barrier nil rx)))))
      (while (and (< (ready-count) n) (< (float-time) deadline))
        (accept-process-output nil 0.05))
      (let ((ok (>= (ready-count) n)))
        (when ok
          (with-temp-file (expand-file-name (format "%s-go" phase) barrier)
            (insert "go")))
        ok))))

(defun dorg-test-lin--install-gate-barrier (barrier phase label timeout)
  "CHILD-side: rendezvous ONCE at BARRIER/PHASE just before the FIRST store-gate
entry (i.e. after the outside-gate plan, before `GET_LOCK').  One-shot, so re-plan
re-entries do not re-block."
  (let ((fired nil))
    (advice-add 'dorg-mutate-call-with-store-gate :before
                (lambda (&rest _)
                  (unless fired
                    (setq fired t)
                    (dorg-test-lin--barrier-signal-and-wait
                     barrier phase label timeout))))))

(defun dorg-test-lin--install-at-gate-marker (marker)
  "CHILD-side: drop MARKER the FIRST time the store gate is ENTERED — post-plan,
just before `GET_LOCK' — then proceed to block on the REAL gate.  Unlike the
rendezvous barrier this does NOT wait: it lets the parent prove the writer is
poised at / blocked on a gate the PARENT itself holds (the deterministic A2
contention proof), rather than inferring contention from an always-positive
acquire-elapsed timing."
  (let ((fired nil))
    (advice-add 'dorg-mutate-call-with-store-gate :before
                (lambda (&rest _)
                  (unless fired
                    (setq fired t)
                    (with-temp-file marker (insert "1")))))))

(defun dorg-test-lin--install-ungated-double-barrier (barrier phase label timeout)
  "CHILD-side A2 NON-VACUITY harness: DISABLE the store gate (no `GET_LOCK' held,
release a no-op) and rendezvous TWICE — at PHASEa just before the now-lock-free
gate entry, and at PHASEb just before the commit batch.  With mutual exclusion
removed, every sibling passes the HEAD==base CAS and then commits against the SAME
base together, forcing the qe9r overlap the gate exists to prevent."
  (let ((fired-a nil) (fired-b nil))
    (advice-add 'dorg-mutate--acquire-gate :override (lambda (&rest _) t))
    (advice-add 'dorg-sql--lease-release-lock :override (lambda (&rest _) t))
    (advice-add 'dorg-mutate-call-with-store-gate :before
                (lambda (&rest _)
                  (unless fired-a
                    (setq fired-a t)
                    (dorg-test-lin--barrier-signal-and-wait
                     barrier (concat phase "a") label timeout))))
    (advice-add 'dorg-sql--lease-batch :before
                (lambda (&rest _)
                  (unless fired-b
                    (setq fired-b t)
                    (dorg-test-lin--barrier-signal-and-wait
                     barrier (concat phase "b") label timeout))))))

(defun dorg-test-lin--wait-file (path timeout)
  "PARENT/CHILD: block until PATH exists or TIMEOUT s elapse; return t iff it exists."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (file-exists-p path)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (file-exists-p path)))

(defun dorg-test-lin--await-proc (proc timeout)
  "PARENT: block until PROC exits or TIMEOUT s elapse; return t iff it exited."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (process-live-p proc) (< (float-time) deadline))
      (accept-process-output proc 0.05))
    (not (process-live-p proc))))

(defun dorg-test-lin--disable-gate ()
  "CHILD-side: neuter the store gate so the idempotency preflight and CAS run
WITHOUT holding `GET_LOCK' — the gateless-preflight mutant for A16.1 non-vacuity."
  (advice-add 'dorg-mutate--acquire-gate :override (lambda (&rest _) t))
  (advice-add 'dorg-sql--lease-release-lock :override (lambda (&rest _) t)))

(defun dorg-test-lin--holder-body (apiform committing release op-id)
  "Build a child body that applies APIFORM under OP-ID but HOLDS the store gate
across the IN-FLIGHT commit: it advises `dorg-sql--lease-batch' to pause INSIDE
the gate, just BEFORE the real DOLT_COMMIT (so evidence is still absent and HEAD
still equals the base), drop the COMMITTING marker, and block until the RELEASE
marker appears — then run the real commit.  While it waits, the gate (GET_LOCK) is
held on its exact session, so a concurrent same-op-id retry must block on the gate
rather than observe the mid-settle window."
  `(progn
     (advice-add 'dorg-sql--lease-batch :around
                 (lambda (orig &rest a)
                   (with-temp-file ,committing (insert "1"))
                   (let ((deadline (+ (float-time) 90)))
                     (while (and (not (file-exists-p ,release))
                                 (< (float-time) deadline))
                       (sleep-for 0.02)))
                   (apply orig a)))
     (let ((r (dorg-api-execute ',apiform :mode 'apply :source src :op-id ,op-id)))
       (list :ok (and (plist-get r :ok) t) :commit (plist-get r :new-token)))))

(defun dorg-test-lin--holder-body-post-commit (apiform committing release op-id)
  "Build a child body that COMMITS under OP-ID, then models a LOST RESPONSE while
STILL HOLDING the gate — the faithful A16.1 post-send / response-loss seam
\(reviewer v3 blocker 1).  It advises `dorg-sql--lease-batch' to:
  1. run the REAL commit `(apply orig a)' — the server ACCEPTS `DOLT_COMMIT', so
     the evidence row and the new HEAD are durable (post-send / settled);
  2. drop the COMMITTING marker (the parent then observes HEAD has advanced);
  3. BLOCK until the RELEASE marker while the gate `GET_LOCK' is STILL HELD on this
     exact session (the settlement-delayed window: a concurrent same-op-id retry
     must WAIT OUT this session before it may read the settled evidence);
  4. signal `dorg-sql-lease-broken' — the client LOSES the response, so the
     holder's own `dorg-api-execute' returns terminal `:reason indeterminate'
     EVEN THOUGH the commit landed.
Unlike `dorg-test-lin--holder-body' (which pauses BEFORE the commit request is
sent), here the commit is genuinely accepted server-side before the response is
lost, so the retry proves gate-first RECOVERY of the ORIGINAL commit, not merely
blocking behind a paused pre-commit holder."
  `(progn
     (advice-add 'dorg-sql--lease-batch :around
                 (lambda (orig &rest a)
                   ;; (1) Real commit lands server-side.
                   (apply orig a)
                   ;; (2) Announce the settled commit.
                   (with-temp-file ,committing (insert "1"))
                   ;; (3) Hold the gate through the response-loss window.
                   (let ((deadline (+ (float-time) 90)))
                     (while (and (not (file-exists-p ,release))
                                 (< (float-time) deadline))
                       (sleep-for 0.02)))
                   ;; (4) Lose the response: fail closed AFTER the commit landed.
                   (signal 'dorg-sql-lease-broken
                           (list "dorg-test: response lost after settled commit"))))
     (let ((r (condition-case e
                  (dorg-api-execute ',apiform :mode 'apply :source src :op-id ,op-id)
                (dorg-mutate-error
                 (list :error 'mutate :reason (dorg-mutate-error-reason e))))))
       (list :reason (plist-get r :reason)
             :ok (and (plist-get r :ok) t)))))

;;; ─── Harness smoke — one real child attaches + commits through the gate ─

(ert-deftest dorg-test-lin-xproc-harness-smoke ()
  "Harness smoke: a SEPARATE OS process attaches to the shared server and
commits one gated mutation; the parent observes the committed value and the
child's echoed :commit.  Proves the cross-process attach + gated apply works
before A2/A4/A7/A16 rely on it."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (port dorg-test--server-port)
           (results (dorg-test-lin--run-writers
                     (list (list "smoke" dolt-dir port rid
                                 `(let ((r (dorg-test--apply-patches
                                            src (list (dorg-test--property-put-patch
                                                       (dorg-test--heading-address ,id-a)
                                                       "S" "child")))))
                                    (list :ok (and (plist-get r :ok) t)
                                          :commit (plist-get r :commit)))))
                     120))
           (res (cdr (assoc "smoke" results))))
      (should (plist-get res :ok))
      (should (stringp (plist-get res :commit)))
      (should (equal "child" (dorg-test-lin--prop dolt-dir id-a "S"))))))

;;; ─── A16.8 — reason enum registered (pure) ─────────────────────────────

(ert-deftest dorg-test-lin-reason-enum-registered ()
  "A16.8: the four terminal reasons this bead introduces are members of
`dorg-mutate-error-reasons', so `dorg-mutate--raise' emits them without
tripping its `cl-assert (memq reason dorg-mutate-error-reasons)'.
NON-VACUITY: an unregistered reason -> `cl-assert' failure, not a typed error."
  (dolist (reason '(overlap contended idempotency-key-mismatch indeterminate))
    (should (memq reason dorg-mutate-error-reasons))
    ;; Each raises a typed condition (not a cl-assertion-failed).
    (let ((err (should-error (dorg-mutate--raise reason "msg")
                             :type 'dorg-mutate-error)))
      (should (eq (dorg-mutate-error-reason err) reason))))
  ;; The public API surface inherits them (dorg-api-error-reasons appends).
  (dolist (reason '(overlap contended idempotency-key-mismatch indeterminate))
    (should (memq reason dorg-api-error-reasons))))

(ert-deftest dorg-test-lin-reason-enum-unregistered-asserts ()
  "A16.8 non-vacuity: an UNREGISTERED reason trips the raise assertion instead of
producing a typed error — proving the enum membership is load-bearing."
  (should-error (dorg-mutate--raise 'not-a-real-reason "msg")))

;;; ─── A0 — primitive excludes AND canonical key converges ───────────────

(ert-deftest dorg-test-lin-get-lock-excludes-across-sessions ()
  "A0 (exclusion): while one leased session holds GET_LOCK(K), a SECOND
independent session cannot acquire K; after RELEASE it can.
NON-VACUITY: a non-excluding primitive lets the second session acquire while the
first holds -> the `(should (eql 0 ...))' fails."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql--with-lease (lease dolt-dir)
      (let ((key (dorg-sql--mutate-lock-key lease))
            (conn2 (dorg-test-lin--raw-conn dolt-dir)))
        (unwind-protect
            (progn
              ;; Session 1 acquires via the leased seam.
              (should (dorg-sql--lease-get-lock lease key 5))
              ;; Session 2 is excluded while session 1 holds it.
              (should (eql 0 (dorg-test-lin--raw-get-lock conn2 key 0)))
              ;; Release on session 1; session 2 can then acquire.
              (dorg-sql--lease-release-lock lease key)
              (should (eql 1 (dorg-test-lin--raw-get-lock conn2 key 0)))
              ;; Re-acquire the lock on the lease before with-lease unwinds so
              ;; the leased session owns it again is NOT required; leave it on
              ;; conn2 and drop conn2.
              nil)
          (ignore-errors (mysql-disconnect conn2)))))))

(ert-deftest dorg-test-lin-release-lock-not-held-is-error ()
  "A0/A5 (release contract): RELEASE_LOCK on a name this session never held
returns 0/NULL and MUST be an error, not a silent success.
NON-VACUITY: treating 0/NULL as success would let the code drop the gate it
never owned."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql--with-lease (lease dolt-dir)
      (let ((key (dorg-sql--mutate-lock-key lease)))
        ;; Never acquired -> RELEASE_LOCK returns NULL -> ownership error.
        (should-error (dorg-sql--lease-release-lock lease key)
                      :type 'dorg-sql-lease-ownership-error)))))

(ert-deftest dorg-test-lin-canonical-key-derivation ()
  "A0 (key): the mutation lock key is derived from SERVER-authoritative inputs
\(DATABASE() + the parentless ROOT commit), is <=64 bytes, carries the namespace
prefix, is byte-STABLE for one store across path spellings, and DIFFERS for
distinct stores.
NON-VACUITY: a lexical-path key would differ across spellings of one store or
collide across distinct stores; an over-long key would exceed 64 bytes."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((k1 (dorg-sql--with-lease (l dolt-dir) (dorg-sql--mutate-lock-key l)))
           ;; A REAL alias spelling of the SAME store: a SYMLINK whose basename
           ;; matches the store's DB (so DB resolution is identical) but whose
           ;; client path is genuinely different — it resolves (truename) to the
           ;; same store.  The key reads server-authoritative DATABASE()+root, so
           ;; it must be byte-identical across this distinct spelling.
           (alias-parent (make-temp-file "dorg-lin-alias-" t))
           (alias (file-name-as-directory
                   (expand-file-name
                    (file-name-nondirectory (directory-file-name dolt-dir))
                    alias-parent)))
           (k-alias (progn
                      (make-symbolic-link (directory-file-name dolt-dir)
                                          (directory-file-name alias))
                      (dorg-sql--with-lease (l alias) (dorg-sql--mutate-lock-key l)))))
      (should (stringp k1))
      (should (string-prefix-p dorg-sql--mutate-lock-prefix k1))
      (should (<= (string-bytes k1) 64))
      (should (= (length k1) 64))
      ;; The alias is a genuinely different path spelling (a symlink), NOT a
      ;; normalized re-spelling of the same string.
      (should-not (equal (directory-file-name alias) (directory-file-name dolt-dir)))
      ;; Path-alias independence: same store, byte-identical key.
      (should (equal k1 k-alias))
      ;; The key does NOT embed the client path (neither spelling).
      (should-not (string-match-p (regexp-quote (directory-file-name dolt-dir)) k1))
      (should-not (string-match-p (regexp-quote (directory-file-name alias)) k1))
      ;; A DISTINCT store yields a DISTINCT key.
      (dorg-test--with-temp-db other-dir
        (let ((k-other (dorg-sql--with-lease (l other-dir)
                         (dorg-sql--mutate-lock-key l))))
          (should-not (equal k1 k-other)))))))

;;; ─── A16.5/.6/.7 — generation-safe commit recovery via dolt_diff ───────

(ert-deftest dorg-test-lin-recover-commit-non-circular ()
  "A16.5: after a normal committed evidence write, the creation commit is
recovered by the generation's own gen_id via the dolt_diff `added' transition —
NO commit_date ordering, NO self-hash written back into the tree.
NON-VACUITY (a): the invalid `dolt_history_*' query (naming to_commit/diff_type,
which that snapshot table lacks) fails to execute.  (b): gen_id NOT NULL UNIQUE
is enforced — a second row with a duplicate gen_id is rejected."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql--with-lease (lease dolt-dir)
      (let* ((gen (dorg-mutate--new-gen-id))
             (head (dorg-test-lin--commit-evidence lease "op-A" gen "digestA" 1000)))
        ;; Recovery returns the creation commit == the HEAD that added the row,
        ;; with no post-commit evidence write.
        (should (equal head (dorg-sql--mutation-log-recover-commit lease gen)))
        ;; Preflight lookup returns (gen . digest) for the live row.
        (should (equal (cons gen "digestA")
                       (dorg-sql--mutation-log-lookup lease "op-A")))
        ;; NON-VACUITY (a): the v5 dolt_history mutant cannot execute.
        (should-error
         (dorg-sql--lease-exec
          lease (concat "SELECT to_commit FROM dolt_history_dorg_mutation_log"
                        " WHERE diff_type = 'added' AND to_gen_id = "
                        (dorg-sql-quote gen))))
        ;; NON-VACUITY (b): gen_id NOT NULL UNIQUE is enforced.
        (should-error
         (dorg-sql--lease-batch
          lease (list (dorg-sql--mutation-log-insert-sql "op-B" gen "digestB" 1001))
          "dup gen"))))))

(ert-deftest dorg-test-lin-recover-commit-ignores-commit-date ()
  "A16.6: two generations of the SAME op_id (an original, then a reinsert after a
delete) each recover THEIR OWN creation commit, keyed on the unique gen_id — no
commit_date ordering is used, so skew/ties cannot misidentify the generation.
NON-VACUITY: an `ORDER BY commit_date ASC LIMIT 1' recovery would return one
fixed generation for both gen ids; here the two recovered hashes DIFFER and each
matches its own creation commit."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql--with-lease (lease dolt-dir)
      (let* ((gen1 (dorg-mutate--new-gen-id))
             (c1 (dorg-test-lin--commit-evidence lease "op-X" gen1 "d1" 5000)))
        (dorg-test-lin--delete-evidence lease "op-X")
        (let* ((gen2 (dorg-mutate--new-gen-id))
               (c2 (dorg-test-lin--commit-evidence lease "op-X" gen2 "d2" 5001)))
          (should-not (equal gen1 gen2))
          (should-not (equal c1 c2))
          ;; Each generation recovers its OWN creation commit by gen_id.
          (should (equal c1 (dorg-sql--mutation-log-recover-commit lease gen1)))
          (should (equal c2 (dorg-sql--mutation-log-recover-commit lease gen2))))))))

(ert-deftest dorg-test-lin-recover-commit-prune-reuse ()
  "A16.7: after generation-1 for op_id X is pruned (current-state row deleted)
and op_id X is REAPPLIED as generation-2 (fresh gen_id), recovery keyed on the
CURRENT row's gen_id returns generation-2's commit; the pruned generation-1
commit is NEVER returned; and a live-row lookup returns generation-2's evidence.
NON-VACUITY: a lookup keyed on op_id (not gen_id) would return the pruned
generation-1 hash despite current-state PK uniqueness."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-sql--with-lease (lease dolt-dir)
      (let* ((gen1 (dorg-mutate--new-gen-id))
             (c1 (dorg-test-lin--commit-evidence lease "op-X" gen1 "d1" 7000)))
        ;; Retention prunes generation-1's current-state row.
        (dorg-test-lin--delete-evidence lease "op-X")
        ;; After prune, no live evidence -> preflight treats X as not-committed.
        (should (null (dorg-sql--mutation-log-lookup lease "op-X")))
        ;; Reapply X as a fresh generation.
        (let* ((gen2 (dorg-mutate--new-gen-id))
               (c2 (dorg-test-lin--commit-evidence lease "op-X" gen2 "d2" 7001)))
          (should-not (equal c1 c2))
          ;; The live row is generation-2.
          (should (equal (cons gen2 "d2")
                         (dorg-sql--mutation-log-lookup lease "op-X")))
          ;; Recovery on the CURRENT gen returns generation-2's commit.
          (should (equal c2 (dorg-sql--mutation-log-recover-commit lease gen2)))
          ;; Generation-1's pruned commit is still recoverable BY ITS OWN gen id
          ;; (history retains it), and is DISTINCT from generation-2 — so a
          ;; gen-keyed lookup never conflates the two.
          (should (equal c1 (dorg-sql--mutation-log-recover-commit lease gen1))))))))

;;; ─── A1 — disjoint same-base race linearizes: both persist ─────────────

(ert-deftest dorg-test-lin-disjoint-stale-base-replans-both-persist ()
  "A1: two DISJOINT edits planned against the SAME base, applied through the gate,
BOTH durably persist as two clean single-parent commits — the second, whose base
is now stale, RE-PLANS onto the winner's HEAD (Design §2.4 3b) rather than
failing or overwriting.
NON-VACUITY (transformation): the second writer's base != HEAD after the first
commits; a gate that committed the stale plan would collide/merge, and the
pre-gate executor returned terminal `:reason stale' (no second commit).  Here we
assert the second SUCCEEDS, HEAD advances by exactly two SINGLE-parent commits,
and NEITHER field is lost."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n* Beta\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (id-b (dorg-test-lin--id dolt-dir "Beta"))
           (h0 (dorg-head-commit dolt-dir))
           (base-count (dorg-test-lin--commit-count dolt-dir)))
      ;; Writer 1: disjoint edit on Alpha, committed against the shared base.
      (let ((r1 (dorg-test--apply-patches
                 src (list (dorg-test--property-put-patch
                            (dorg-test--heading-address id-a) "S1" "one"))
                                :base-commit h0)))
        (should (plist-get r1 :ok)))
      (let ((h1 (dorg-head-commit dolt-dir)))
        (should-not (equal h0 h1))
        ;; Writer 2: disjoint edit on Beta, STILL carrying the now-stale base h0.
        (let ((r2 (dorg-test--apply-patches
                   src (list (dorg-test--property-put-patch
                              (dorg-test--heading-address id-b) "S2" "two"))
                                  :base-commit h0)))
          (should (plist-get r2 :ok))
          ;; It re-planned onto the winner HEAD, not h0.
          (should-not (equal h0 (plist-get r2 :commit))))
        ;; Both disjoint intents durably persist.
        (should (equal "one" (dorg-test-lin--prop dolt-dir id-a "S1")))
        (should (equal "two" (dorg-test-lin--prop dolt-dir id-b "S2")))
        ;; Exactly two linear single-parent commits advanced HEAD (no merge).
        (should (= (+ base-count 2) (dorg-test-lin--commit-count dolt-dir)))
        (should (= 1 (dorg-test-lin--parents-of-head dolt-dir)))))))

;;; ─── A3 — same-field writers: deterministic terminal overlap ───────────

(ert-deftest dorg-test-lin-same-field-overlap-terminal-no-clobber ()
  "A3: two writers targeting the SAME (heading, field) from the same base — the
first commits (FIRST-COMMITTER-WINS); the second sees its written heading was
changed since its base and returns TERMINAL `dorg-mutate-error' `:reason overlap'
with NO internal retry and NO clobber of the winner's value.
NON-VACUITY: with the overlap classification defeated (write-set forced empty),
the loser blindly RE-PLANS onto the winner HEAD and OVERWRITES the field
last-writer-wins — proving the overlap terminal is load-bearing."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (h0 (dorg-head-commit dolt-dir)))
      ;; Winner commits STATUS=win against the shared base.
      (should (plist-get (dorg-test--apply-patches
                          src (list (dorg-test--property-put-patch
                                     (dorg-test--heading-address id-a)
                                     "STATUS" "win"))
                                         :base-commit h0)
                         :ok))
      (should (equal "win" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
      ;; Loser carries the stale base and targets the SAME field.
      (let ((err (should-error
                  (dorg-test--apply-patches
                   src (list (dorg-test--property-put-patch
                              (dorg-test--heading-address id-a)
                              "STATUS" "lose"))
                                  :base-commit h0)
                  :type 'dorg-mutate-error)))
        (should (eq 'overlap (dorg-mutate-error-reason err)))
        ;; Overlap evidence carries the contended heading id.
        (should (member id-a (plist-get (dorg-mutate-error-data err) :overlap))))
      ;; First-committer-wins: the winner's value is intact, never clobbered.
      (should (equal "win" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
      ;; NON-VACUITY: defeat the overlap check -> loser re-plans and clobbers.
      (cl-letf (((symbol-function 'dorg-mutate--heading-write-set)
                 (lambda (&rest _) nil)))
        (should (plist-get (dorg-test--apply-patches
                            src (list (dorg-test--property-put-patch
                                       (dorg-test--heading-address id-a)
                                       "STATUS" "lose"))
                                           :base-commit h0)
                           :ok))
        (should (equal "lose" (dorg-test-lin--prop dolt-dir id-a "STATUS")))))))

;;; ─── A15 — stale-plan serializability / write-skew (two inserts) ───────

(ert-deftest dorg-test-lin-two-inserts-same-parent-serializable ()
  "A15: two writers each INSERT a distinct-id child under the SAME parent at the
SAME position from the SAME base (disjoint SQL write-sets, SHARED predicate
dependency on the parent's child ordering).  The outcome is serializable: exactly
one commits first, the second RE-PLANS onto the new HEAD (recomputing its
position AFTER the now-existing first child), and BOTH children persist under a
well-defined order with NO duplicate sort key.
NON-VACUITY: the loser's STALE plan (built against the empty parent) computes the
SAME sort key as the winner; committing it in place (the removed v2 write-set-only
shortcut) violates uq_heading_sibling_order — asserted here by applying the stale
plan directly at the moved HEAD and observing the constraint failure."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Parent\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (pid (dorg-test-lin--id dolt-dir "Parent"))
           (h0 (dorg-head-commit dolt-dir))
           ;; Capture the loser's STALE plan (built at h0, empty parent) BEFORE
           ;; the winner moves HEAD — the dry-run simulates at the current HEAD.
           (stale-plan (dorg-test--apply-patches
                        src (list (dorg-test--tree-insert-patch
                                   (dorg-test--heading-address pid)
                                   '(:title "C2")))
                                       :base-commit h0 :dry-run t))
           (stale-sql (plist-get stale-plan :db-ops)))
      (should stale-sql)
      ;; Winner inserts C1 under Parent; HEAD advances.
      (should (plist-get (dorg-test--apply-patches
                          src (list (dorg-test--tree-insert-patch
                                     (dorg-test--heading-address pid)
                                     '(:title "C1")))
                                         :base-commit h0)
                         :ok))
      (let ((h1 (dorg-head-commit dolt-dir)))
        (should-not (equal h0 h1))
        ;; NON-VACUITY: the loser's stale plan, committed in place at the moved
        ;; HEAD, collides on (parent_id, sort_key) — the exact write-skew the
        ;; HEAD-equality re-plan prevents.
        (dorg-sql--with-lease (lease dolt-dir)
          (should-error (dorg-sql--lease-batch lease stale-sql "stale in-place")))
        ;; The failed in-place attempt rolled back; HEAD is unchanged.
        (should (equal h1 (dorg-head-commit dolt-dir)))
        ;; GREEN: the loser applied through the gate RE-PLANS at h1 and commits
        ;; cleanly — both children persist under a well-defined order.
        (should (plist-get (dorg-test--apply-patches
                            src (list (dorg-test--tree-insert-patch
                                       (dorg-test--heading-address pid)
                                       '(:title "C2")))
                                           :base-commit h0)
                           :ok))
        (let ((kids (dorg-test-lin--child-titles dolt-dir pid)))
          (should (equal 2 (length kids)))
          (should (member "C1" kids))
          (should (member "C2" kids)))
        ;; No merge commit: the re-plan produced a linear single-parent advance.
        (should (= 1 (dorg-test-lin--parents-of-head dolt-dir)))))))

;;; ─── A13 — delta SQL is target-absolute ────────────────────────────────

(ert-deftest dorg-test-lin-delta-sql-target-absolute ()
  "A13: every generated mutating statement keys by ABSOLUTE identity (an explicit
`WHERE id = N' or a specific allocated id in VALUES), never by ordinal/position.
This is what lets the op-id idempotent replay (A16) and the re-plan re-apply the
intent against a MOVED HEAD without targeting the wrong row.
NON-VACUITY: a position/ordinal-relative statement would target a different row
after a concurrent insert shifts sibling order; here the UPDATE carries the EXACT
edited id and NOT its sibling's, the DELETE keys the doomed id, the INSERT carries
an explicit id, and no statement uses a positional predicate."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Parent\n** C1\n** C2\n"))
           (src (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id rid)))
           (pid (dorg-test-lin--id dolt-dir "Parent"))
           (c1 (dorg-test-lin--id dolt-dir "C1"))
           (c2 (dorg-test-lin--id dolt-dir "C2"))
           (sql (lambda (patches)
                  (plist-get (dorg-mutate--dolt-simulate
                              src (dorg-mutate--validate-patches patches) nil)
                             :sql))))
      ;; UPDATE keys by the exact edited id, never the sibling.
      (let* ((ops (funcall sql
                           (list (dorg-test--heading-update-patch
                                  c1 :title "C1x"))))
             (upd (seq-find (lambda (s) (string-match-p "UPDATE headings SET" s)) ops)))
        (should upd)
        (should (string-match-p (format "WHERE id = %d\\'" c1) upd))
        (should-not (string-match-p (format "id = %d\\b" c2) upd))
        ;; No positional/ordinal targeting anywhere in the batch.
        (dolist (s ops)
          (should-not (string-match-p "\\bLIMIT\\b\\|\\bOFFSET\\b\\|ORDER BY" s))))
      ;; DELETE keys the doomed row by absolute id.
      (let ((ops (funcall sql (list (dorg-test--delete-patch c2)))))
        (should (seq-some (lambda (s)
                            (and (string-match-p "DELETE FROM headings" s)
                                 (string-match-p (format "id = %d\\b" c2) s)))
                          ops)))
      ;; INSERT carries an explicit allocated id in VALUES (not positional).
      (let* ((ops (funcall sql
                           (list (dorg-test--tree-insert-patch
                                  (dorg-test--heading-address pid)
                                  '(:title "C3")))))
             (ins (seq-find (lambda (s) (string-match-p "INSERT INTO headings" s)) ops)))
        (should ins)
        (should (string-match-p "VALUES ([0-9]" ins))))))

;;; ─── A11 — phase timings split wait / hold / plan / sql / commit ───────

(ert-deftest dorg-test-lin-timings-gate-hold-excludes-plan ()
  "A11: a gated Dolt apply reports phase `:timings' with distinct `:gate-wait',
`:gate-hold', `:plan', `:sql', `:commit', surfaced through BOTH the executor
result and the public `dorg-api-execute'; `:gate-hold' EXCLUDES the whole-root
`:plan' because the load/simulate runs OUTSIDE the lock.
The two claims have separate deterministic witnesses.  Call-through observers
prove the real planner finishes before the real gate is acquired and never runs
while it is held.  The pure timing assembler then receives controlled phase
durations and must map plan 7.0 ONLY to `:plan', leaving `:gate-hold' exactly 2.
The latter proves the mapping layer, not how its caller measures the held window;
the structural witness carries the caller-side exclusion claim.  There is no
wall-clock comparison, baseline, scheduler threshold, retry, or global clock
replacement around live I/O."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (h0 (dorg-head-commit dolt-dir))
           (events nil)
           (plan-held-states nil)
           (gate-held nil)
           (plan-observer
            (lambda (orig &rest args)
              (push 'plan-enter events)
              (push gate-held plan-held-states)
              (unwind-protect
                  (apply orig args)
                (push gate-held plan-held-states)
                (push 'plan-exit events))))
           (acquire-observer
            (lambda (orig &rest args)
              (prog1 (apply orig args)
                ;; The flag changes only AFTER the real acquire succeeds.
                (setq gate-held t)
                (push 'gate-acquired events))))
           (release-observer
            (lambda (orig &rest args)
              (unwind-protect
                  (apply orig args)
                ;; The flag clears only AFTER the real release returns.
                (setq gate-held nil)
                (push 'gate-released events)))))
      (advice-add 'dorg-mutate--dolt-simulate :around plan-observer)
      (advice-add 'dorg-mutate--acquire-gate :around acquire-observer)
      (advice-add 'dorg-sql--lease-release-lock :around release-observer)
      (unwind-protect
          (let* ((r (dorg-api-execute
                     (list 'on (list 'id id-a) '(set (property "K") "V"))
                     :mode 'apply
                     :source (list :dolt-dir dolt-dir :root-id rid)
                     :base h0))
                 (tm (plist-get r :timings)))
            ;; The real public apply succeeds, moves HEAD, and carries all five
            ;; numeric phase values while the call-through observers are live.
            (should (plist-get r :ok))
            (should-not (equal h0 (dorg-head-commit dolt-dir)))
            (should tm)
            (dolist (k '(:gate-wait :gate-hold :plan :sql :commit))
              (should (numberp (plist-get tm k)))))
        (advice-remove 'dorg-sql--lease-release-lock release-observer)
        (advice-remove 'dorg-mutate--acquire-gate acquire-observer)
        (advice-remove 'dorg-mutate--dolt-simulate plan-observer))
      (setq events (nreverse events)
            plan-held-states (nreverse plan-held-states))
      ;; Counts make an unexpected retry legible rather than reducing it to an
      ;; opaque sequence mismatch.  No planner interval overlaps the held gate.
      (dolist (event '(plan-enter plan-exit gate-acquired gate-released))
        (should (= 1 (cl-count event events))))
      (should (equal '(plan-enter plan-exit gate-acquired gate-released) events))
      (should (equal '(nil nil) plan-held-states))
      (should-not gate-held)
      ;; Pure attribution seam: exact inputs, exact output.  PLAN is a separate
      ;; argument from [WAIT HOLD SQL COMMIT] and must not contaminate HOLD.
      (let* ((r (dorg-mutate--gated-result-with-timings
                 '(:ok t) 7.0 [1 2 3 4]))
             (tm (plist-get r :timings)))
        (should (equal 7.0 (plist-get tm :plan)))
        (should (equal 2 (plist-get tm :gate-hold)))
        (should (equal '(:gate-wait 1 :gate-hold 2 :plan 7.0
                         :sql 3 :commit 4)
                       tm))))))

(ert-deftest dorg-test-2xv2m-apply-simulates-once-at-equal-head ()
  "An equal-HEAD apply consumes the simulation already built for its plan.
The pre-change engine simulates once in `dorg-mutate--make-plan' and a second
time in the gated executor.  This transformation witness counts the complete
public apply boundary and requires exactly one full simulation.  Removing
attempt reuse must make it report two calls again."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Existing\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (insert (dorg-test--tree-insert-patch
                    :root '(:id 900001 :title "Created")))
           (orig (symbol-function 'dorg-mutate--dolt-simulate))
           (calls 0))
      (cl-letf (((symbol-function 'dorg-mutate--dolt-simulate)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply orig args))))
        (let ((result (dorg-test--apply-patches src (list insert))))
          (should (plist-get result :ok))
          (should (= 1 calls)))))))

(ert-deftest dorg-test-2xv2m-dry-run-simulates-once-and-does-not-commit ()
  "Dry-run remains a single full simulation and never persists its insert.
This passing pre-change control distinguishes attempt reuse from deleting the
planner or turning apply into an unplanned write."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Existing\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (dry (dorg-test--tree-insert-patch
                 :root '(:id 900002 :title "Dry")))
           (orig (symbol-function 'dorg-mutate--dolt-simulate))
           (head0 (dorg-head-commit dolt-dir))
           (calls 0))
      (cl-letf (((symbol-function 'dorg-mutate--dolt-simulate)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (apply orig args))))
        (let ((plan (dorg-mutate--plan-patches src (list dry))))
          (should (plist-get plan :supported))
          (should (= 1 calls))
          (should (equal head0 (dorg-head-commit dolt-dir)))
          (should-not (dorg-test-lin--id dolt-dir "Dry")))))))

;;; ─── A7 (static) — every public Dolt commit path routes through the gate ─

(ert-deftest dorg-test-lin-static-executors-route-through-gate ()
  "A7 (static call-site coverage): the same-root and cross-root Dolt executors
commit only through `dorg-mutate--dolt-gated-apply'; neither reaches the
leased SQL batch directly, so no public typed mutation bypasses the store gate.
Non-vacuity: routing either executor directly to `dorg-sql--lease-batch' makes
the corresponding `should-not' fail."
  (dolist (fn '(dorg-mutate--dolt-execute dorg-mutate--dolt-cross-root-execute))
    (let ((body (prin1-to-string (symbol-function fn))))
      ;; The executor delegates its commit to the gate seam...
      (should (string-match-p "dorg-mutate--dolt-gated-apply" body))
      ;; ...and never reaches the leased SQL batch itself.
      (should-not (string-match-p "dorg-sql--lease-batch" body)))))

(ert-deftest dorg-test-lin-static-no-ungated-typed-commit ()
  "A7 (static, REPOSITORY-WIDE audit): DISCOVERS every production `.el' at the repo
root (`dorg-test-lin--production-el-files' — not a curated list) and classifies
EVERY commit-producing seam, so a NEW ungated caller in a NEW file is caught, not
merely the two currently-named executors:

1. The direct `DOLT_COMMIT' issuers are EXACTLY the three low-level Dolt commit
   primitives; no typed engine path emits its own commit SQL.
2. The GATED commit seam `dorg-sql--lease-batch' has EXACTLY TWO production
   callers — `dorg-mutate--dolt-gated-apply' for typed writes and
   `dorg-transaction-commit' for exact-base administrative import.  Both acquire
   `dorg-mutate-call-with-store-gate' before reaching the lease batch.

NON-VACUITY: a new production defun issuing `DOLT_COMMIT(' directly, or an
executor reaching the leased batch outside the gate, makes a discovered set
differ from its allowlist and FAILS, naming the offending defun."
  (let* ((files (dorg-test-lin--production-el-files)))
    ;; The discovery is real, repo-wide, and includes the commit-bearing sources.
    (should (> (length files) 20))
    (dolist (core '("dorg-mutate.el" "dorg-dolt.el" "dorg-transaction.el"))
      (should (member (expand-file-name core dorg-test-lin--repo-root) files)))
    ;; (1) Direct DOLT_COMMIT issuers are exactly the low-level primitives.
    (should (equal '("dorg-commit" "dorg-sql--mysql-batch-on-conn" "dorg-sql-batch")
                   (dorg-test-lin--source-defuns-with "DOLT_COMMIT('" files)))
    ;; (2) The gated commit seam is reached only from the two gate owners.
    (should (equal '("dorg-mutate--dolt-gated-apply"
                     "dorg-transaction-commit")
                   (dorg-test-lin--source-callers 'dorg-sql--lease-batch files)))
    ;; The typed engine executors are not direct-commit issuers.
    (let ((direct (dorg-test-lin--source-defuns-with "DOLT_COMMIT('" files)))
      (dolist (ex '("dorg-mutate--dolt-execute" "dorg-mutate--dolt-cross-root-execute"))
        (should-not (member ex direct))))))

(ert-deftest dorg-test-lin-static-audit-traverses-cl-defun-callers ()
  "A7 (static, DEFINER COVERAGE — reviewer v3/v4 blocker): the repository-wide
call-site audit must classify a commit-seam call inside ANY call-bearing
top-level form, with NO fixed definer-head allowlist.  A curated allowlist is
evadable: the repo already defines functions with `cl-defun'
(`dorg-git-sync--result')
and `cl-defmethod' dispatch, and Emacs has many other function-defining forms
\(`define-minor-mode', `define-derived-mode', `defalias' lambdas, macros); a
scanner keyed on a fixed head set lets a NEW ungated caller defined with any
omitted head slip past while the audit stays green (the reviewer v4 blocker was
exactly `define-minor-mode'/`defalias' evading the head allowlist).

PROOF (synthetic): a temp source defines the ungated commit seam caller with
SEVEN different heads — `cl-defun', `cl-defmethod', `defmacro', `defun',
`define-minor-mode', `define-derived-mode', and a `defalias' lambda — plus a
top-level `defvar' whose STRING VALUE merely MENTIONS the call syntax and must
NOT be counted.  `dorg-test-lin--source-callers' returns EXACTLY the seven real
callers and excludes the string mention, because it detects an actual call FORM
structurally rather than by a definer head or a printed substring.

LOAD-BEARING (two mutant contrasts):
1. A fixed defun/cl-defun-head allowlist MISSES `bad-minor-mode',
   `bad-derived-mode', and `bad-alias' — so the head-allowlist approach the
   navigator rejected is provably unsound.
2. A naive printed-substring scan (`dorg-test-lin--source-defuns-with', correct
   only for literal SQL seams) WRONGLY counts the `defvar' string mention — so
   structural call detection is required.  The production audit helper does
   neither.

LOAD-BEARING (production anchor): the real `cl-defun'
`dorg-git-sync--result' is discovered by a needle present only in its body; a
`defun'-only scan would MISS it, so this membership fails without the
head-independent traversal."
  (let ((tmp (make-temp-file "dorg-lin-defmix-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert
             ";;; synthetic definer-coverage fixture\n"
             "(cl-defun bad-cl-caller (x) (dorg-test-unsafe-commit x))\n"
             "(cl-defmethod bad-method ((x integer)) (dorg-test-unsafe-commit x))\n"
             "(defmacro bad-macro (x) `(dorg-test-unsafe-commit ,x))\n"
             "(defun bad-plain-caller (x) (dorg-test-unsafe-commit x))\n"
             "(define-minor-mode bad-minor-mode \"doc\" :init-value nil"
             " (dorg-test-unsafe-commit 1))\n"
             "(define-derived-mode bad-derived-mode fundamental-mode \"Bad\""
             " (dorg-test-unsafe-commit 1))\n"
             "(defalias 'bad-alias (lambda () (dorg-test-unsafe-commit 1)))\n"
             ;; A NON-call top-level form: the seam appears only inside a STRING.
             "(defvar bad-not-a-caller \"(dorg-test-unsafe-commit noop)\")\n"))
          (let ((callers (dorg-test-lin--source-callers
                          'dorg-test-unsafe-commit (list tmp)))
                (expected '("bad-alias" "bad-cl-caller" "bad-derived-mode"
                            "bad-macro" "bad-method" "bad-minor-mode"
                            "bad-plain-caller")))
            ;; Every real caller — across all seven definer heads — is reported...
            (dolist (name expected)
              (should (member name callers)))
            ;; ...and the string-only mention is NOT counted as a caller.
            (should-not (member "bad-not-a-caller" callers))
            ;; Exactly the seven real callers, nothing more or less.
            (should (equal expected callers))
            ;; MUTANT 1 — a fixed defun/cl-defun head allowlist MISSES the
            ;; non-listed heads, proving the head-independent traversal is
            ;; load-bearing (this is the navigator's rejected approach).
            (let ((allow '(defun cl-defun)) (allowed '()))
              (dolist (form (dorg-test-lin--read-top-forms tmp))
                (when (and (consp form) (memq (car form) allow)
                           (dorg-test-lin--form-calls-p
                            form 'dorg-test-unsafe-commit))
                  (push (dorg-test-lin--form-name form) allowed)))
              (should-not (member "bad-minor-mode" allowed))
              (should-not (member "bad-derived-mode" allowed))
              (should-not (member "bad-alias" allowed)))
            ;; MUTANT 2 — a printed-substring scan WRONGLY counts the string
            ;; mention, proving structural call detection is load-bearing.
            (should (member "bad-not-a-caller"
                            (dorg-test-lin--source-defuns-with
                             "(dorg-test-unsafe-commit " (list tmp))))))
      (delete-file tmp)))
  ;; Production anchor: a real `cl-defun' IS traversed — a needle unique to
  ;; `dorg-git-sync--result's body finds it only because traversal is
  ;; head-independent (a `defun'-only audit would miss it).
  (let ((files (dorg-test-lin--production-el-files)))
    (should (member "dorg-git-sync--result"
                    (dorg-test-lin--source-defuns-with
                     "draining the warning accumulator" files)))))

;;; ─── A7 (dynamic) — cross-root moves linearize across OS processes ──────

(ert-deftest dorg-test-lin-cross-root-race-linearizes ()
  "A7 (dynamic cross-root coverage): two SEPARATE OS PROCESSES each apply a
DISJOINT typed cross-root move from the SAME base — Writer1 moves A under
destination parent DP1, Writer2 moves B under DP2 — through the cross-root
executor `dorg-mutate--dolt-cross-root-execute'.  The cross-root path linearizes
exactly like same-root: one commits, the other finds its base stale and re-plans
onto the new HEAD, so BOTH moves durably persist as TWO clean single-parent
commits — never a merge / second-parent HEAD.
NON-VACUITY (cross-process transformation): after the first commit the second
writer's base != HEAD, yet both cross-root moves land linearly; an un-gated
cross-root commit against the stale base would collide / merge (the qe9r race the
gate closes).  Paired with the static repository-wide audit above, which fails if
the cross-root executor ever commits outside the gate."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    ;; Source root 100: A(101 sub-tree) and B(102).  Dest root 500: DP1, DP2.
    (dorg-test-lin--seed dolt-dir "* A\n** A-kid\n* B\n" 100)
    (dorg-test-lin--seed dolt-dir "* DP1\n* DP2\n" 500)
    (let* ((port dorg-test--server-port)
           (id-a (dorg-test-lin--id dolt-dir "A"))
           (id-b (dorg-test-lin--id dolt-dir "B"))
           (id-dp1 (dorg-test-lin--id dolt-dir "DP1"))
           (id-dp2 (dorg-test-lin--id dolt-dir "DP2"))
           (h0 (dorg-head-commit dolt-dir))
           (base-count (dorg-test-lin--commit-count dolt-dir))
           (specs
            (list
             (list "cr1" dolt-dir port 100
                   `(let ((r (dorg-test--apply-patches
                              src (list (dorg-test--move-patch
                                         ,id-a (dorg-test--heading-address ,id-dp1)
                                         :to-source
                                         (dorg-mutate--resolve-source
                                          (list :dolt-dir ,dolt-dir :root-id 500))))
                              :base-commit ,h0)))
                      (list :ok (and (plist-get r :ok) t))))
             (list "cr2" dolt-dir port 100
                   `(let ((r (dorg-test--apply-patches
                              src (list (dorg-test--move-patch
                                         ,id-b (dorg-test--heading-address ,id-dp2)
                                         :to-source
                                         (dorg-mutate--resolve-source
                                          (list :dolt-dir ,dolt-dir :root-id 500))))
                              :base-commit ,h0)))
                      (list :ok (and (plist-get r :ok) t))))))
           (results (dorg-test-lin--run-writers specs 150)))
      ;; Both cross-root processes reported success (no error, no overlap here —
      ;; disjoint destinations, so both re-plan cleanly).
      (should (= 2 (length results)))
      (dolist (rc results)
        (should (plist-get (cdr rc) :ok))
        (should-not (eq :error (car (cdr rc)))))
      ;; Both moves durably landed under their destination parents.
      (should (eql id-dp1 (dorg-test-lin--parent-of dolt-dir id-a)))
      (should (eql id-dp2 (dorg-test-lin--parent-of dolt-dir id-b)))
      ;; Exactly two linear single-parent commits — no merge / second-parent HEAD.
      (should (= (+ base-count 2) (dorg-test-lin--commit-count dolt-dir)))
      (should (= 1 (dorg-test-lin--parents-of-head dolt-dir))))))

;;; ─── A16.2 — process death after commit: replay is idempotent, no reapply ─

(ert-deftest dorg-test-lin-op-id-replay-idempotent-no-reapply ()
  "A16.2: a logical mutation carrying `:op-id' commits in one OS PROCESS which
then DIES immediately after DOLT_COMMIT — before any response/reconciliation —
mirroring a client killed after the commit landed (server-accepted commit, lost
response).  A NEW process replaying the SAME op-id + intent hits the gate-first
evidence preflight and returns idempotent `:ok' with the ORIGINAL commit,
reapplying NO SQL.  Reconciliation is proven across a true process boundary, with
no in-memory error state to lean on.
NON-VACUITY: with the mandatory preflight defeated (lookup forced nil) a non-empty
replay reaches the CAS and its duplicate `op_id' evidence INSERT trips the PRIMARY
KEY backstop — it does NOT cleanly reconcile — proving the gate-first preflight is
what turns a replay into idempotent success."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (port dorg-test--server-port)
           (n0 (dorg-test-lin--commit-count dolt-dir))
           (apiform `(on (id ,id-a) (set (property "STATUS") "done"))))
      ;; Child A: commit under op-D, then DIE the instant DOLT_COMMIT returns
      ;; (the session drop auto-releases the gate server-side).
      (dorg-test-lin--run-writers
       (list (list "die" dolt-dir port rid
                   `(progn
                      (advice-add 'dorg-sql--lease-batch :around
                                  (lambda (orig &rest a)
                                    (apply orig a)
                                    (kill-emacs 43)))
                      (dorg-api-execute ',apiform :mode 'apply
                                        :source src :op-id "op-D"))))
       120 :expected-exit 43)
      (let ((c1 (dorg-head-commit dolt-dir))
            (n1 (dorg-test-lin--commit-count dolt-dir)))
        ;; Child A's commit landed exactly once (data + evidence in one commit).
        (should (= (1+ n0) n1))
        (should (equal "done" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
        ;; A FRESH process replays the SAME op-id -> idempotent original commit.
        (let* ((results (dorg-test-lin--run-writers
                         (list (list "replay" dolt-dir port rid
                                     `(let ((r (dorg-api-execute ',apiform :mode 'apply
                                                                 :source src :op-id "op-D")))
                                        (list :ok (and (plist-get r :ok) t)
                                              :idempotent (plist-get r :idempotent)
                                              ;; Public head token is `:new-token'
                                              ;; (= the Dolt commit hash), not :commit.
                                              :commit (plist-get r :new-token)))))
                         120))
               (res (cdr (assoc "replay" results))))
          (should (plist-get res :ok))
          (should (plist-get res :idempotent))
          (should (equal c1 (plist-get res :commit))))
        ;; No second commit; the original value stands.
        (should (= n1 (dorg-test-lin--commit-count dolt-dir)))
        (should (equal "done" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
        ;; NON-VACUITY: defeat the preflight -> a duplicate-op_id evidence INSERT
        ;; fails closed (no clean reconcile), proving the preflight is load-bearing.
        (cl-letf (((symbol-function 'dorg-sql--mutation-log-lookup) (lambda (&rest _) nil)))
          (should-error
           (dorg-test-lin--api-apply
            src `(on (id ,id-a) (set (property "STATUS") "again")) :op-id "op-D")
           :type 'dorg-mutate-error))
        (should (= n1 (dorg-test-lin--commit-count dolt-dir)))
        (should (equal "done" (dorg-test-lin--prop dolt-dir id-a "STATUS")))))))

;;; ─── A16.3 — op-id reuse with a different intent: mismatch, zero SQL ─────

(ert-deftest dorg-test-lin-op-id-mismatch-zero-sql ()
  "A16.3: an `:op-id' whose evidence records intent D1, replayed with a mutation
whose digest is D2 != D1, returns terminal `:reason idempotency-key-mismatch' and
executes ZERO SQL (never success for the WRONG intent).
NON-VACUITY: an identity that ignores the intent digest (constant digest) accepts
the different intent as an idempotent match — so the wrong intent is silently
returned `:ok' with no error — proving the intent digest is load-bearing."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha")))
      (should (plist-get (dorg-test-lin--api-apply
                          src `(on (id ,id-a) (set (property "STATUS") "v1")) :op-id "op-X")
                         :ok))
      (should (equal "v1" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
      (let ((n (dorg-test-lin--commit-count dolt-dir)))
        ;; Same op-id, DIFFERENT intent -> terminal mismatch, ZERO SQL.
        (let ((err (should-error
                    (dorg-test-lin--api-apply
                     src `(on (id ,id-a) (set (property "STATUS") "v2")) :op-id "op-X")
                    :type 'dorg-mutate-error)))
          (should (eq 'idempotency-key-mismatch (dorg-mutate-error-reason err)))
          (should (equal "op-X" (plist-get (dorg-mutate-error-data err) :op-id))))
        ;; Zero SQL: the winner value is intact and HEAD did not advance.
        (should (equal "v1" (dorg-test-lin--prop dolt-dir id-a "STATUS")))
        (should (= n (dorg-test-lin--commit-count dolt-dir))))
      ;; NON-VACUITY: a constant (intent-blind) digest accepts the different
      ;; intent as an idempotent match instead of rejecting it.
      (cl-letf (((symbol-function 'dorg-mutate--intent-digest) (lambda (&rest _) "CONST")))
        (should (plist-get (dorg-test-lin--api-apply
                            src `(on (id ,id-a) (set (property "K") "y1")) :op-id "op-Y")
                           :ok))
        (let ((r (dorg-test-lin--api-apply
                  src `(on (id ,id-a) (set (property "K") "y2")) :op-id "op-Y")))
          (should (plist-get r :ok))
          (should (plist-get r :idempotent)))          ; WRONG intent silently accepted
        (should (equal "y1" (dorg-test-lin--prop dolt-dir id-a "K")))))))

;;; ─── A16.1 — the idempotency preflight is GATE-FIRST ────────────────────

(ert-deftest dorg-test-lin-op-id-preflight-is-gate-first ()
  "A16.1 — server-accepted commit, LOST RESPONSE, FRESH-PROCESS retry (reviewer
v3 blocker 1).  A HOLDER process runs the REAL `DOLT_COMMIT' — the server ACCEPTS
it, so the evidence row and the new HEAD are DURABLE (post-send / settled) — and
then, still HOLDING the gate `GET_LOCK' on its exact session, LOSES the response
\(it fails closed with `:reason indeterminate' AFTER the commit landed).  While the
holder lingers in that response-loss window a SEPARATE-PROCESS retry with the SAME
op-id starts.  The retry FIRST acquires the same gate — which BLOCKS until the
holder's session releases — THEN its preflight finds the SETTLED evidence and
returns the ORIGINAL commit (recovered via the `dolt_diff' added-transition), with
NO second commit.  The parent proves gate-first directly: after the holder's
commit has LANDED (HEAD advanced) but the holder still holds the gate, the retry
has NOT returned and NO further commit appears — it is waiting out the prior
session rather than reading the already-settled evidence ahead of the gate.

This is the faithful post-send / response-loss seam: unlike a holder that pauses
BEFORE the commit request is sent (which would only prove blocking behind a paused
pre-commit holder), the commit here is genuinely accepted server-side before the
response is lost, so the retry proves safe RECOVERY of a landed commit.

NON-VACUITY (cross-process): a preflight that reads evidence/HEAD WITHOUT acquiring
the gate first observes evidence-absent + HEAD==base mid-settle, decides
retry-safe, and DUPLICATES the work — it returns a FRESH (`:idempotent' nil) apply
instead of reconciling, and the holder's own commit then collides / is lost.  With
the gate DISABLED in the retry, that duplication is exactly what happens -> RED."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  ;; ── Positive: server-accepted commit + lost response; gated retry BLOCKS out
  ;;    the holder's session, then RECOVERS the original commit ──
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (port dorg-test--server-port)
           (h0 (dorg-head-commit dolt-dir))
           (n0 (dorg-test-lin--commit-count dolt-dir))
           (apiform `(on (id ,id-a) (set (property "S") "one")))
           (bar (make-temp-file "dorg-lin-a161p-" t))
           (committing (expand-file-name "committing" bar))
           (release (expand-file-name "release" bar))
           (w2-started (expand-file-name "w2-started" bar))
           (procs '()))
      (unwind-protect
          (let ((holder (dorg-test-lin--spawn
                         "hold" dolt-dir port rid
                         (dorg-test-lin--holder-body-post-commit
                          apiform committing release "op-X"))))
            (push holder procs)
            ;; Holder has COMMITTED server-side and is now holding the gate in the
            ;; response-loss window: the commit HAS landed (HEAD advanced) even
            ;; though the client will report it as indeterminate.
            (should (dorg-test-lin--wait-file committing 60))
            (should (process-live-p (nth 1 holder)))
            (let ((c1 (dorg-head-commit dolt-dir)))
              (should-not (equal h0 c1))
              (should (= (1+ n0) (dorg-test-lin--commit-count dolt-dir)))
              (should (equal "one" (dorg-test-lin--prop dolt-dir id-a "S")))
              ;; Same-op-id retry in a fresh process, generous acquire deadline so it
              ;; BLOCKS on the held gate rather than timing out.
              (let ((replay (dorg-test-lin--spawn
                             "replay" dolt-dir port rid
                             `(progn
                                (with-temp-file ,w2-started (insert "1"))
                                (let* ((dorg-mutate-gate-acquire-timeout 60)
                                       (dorg-mutate-gate-deadline 60)
                                       (r (dorg-api-execute ',apiform :mode 'apply
                                                            :source src :op-id "op-X")))
                                  (list :ok (and (plist-get r :ok) t)
                                        :idempotent (plist-get r :idempotent)
                                        :commit (plist-get r :new-token)))))))
                (push replay procs)
                ;; The retry has started; give it time to plan and reach GET_LOCK,
                ;; then prove GATE-FIRST: it is STILL running (waiting out the
                ;; holder's session) and NO second commit has appeared — even though
                ;; the evidence is ALREADY settled.  A preflight-before-gate mutant
                ;; would read that settled evidence immediately and return here.
                (should (dorg-test-lin--wait-file w2-started 60))
                (accept-process-output nil 1.5)
                (should (process-live-p (nth 1 replay)))
                (should (equal c1 (dorg-head-commit dolt-dir)))
                (should (= (1+ n0) (dorg-test-lin--commit-count dolt-dir)))
                ;; Release the holder: it drops the gate; the retry then acquires,
                ;; finds the settled evidence, and recovers the ORIGINAL commit.
                (with-temp-file release (insert "go"))
                (should (dorg-test-lin--await-proc (nth 1 holder) 90))
                (should (dorg-test-lin--await-proc (nth 1 replay) 90))
                (should (= 0 (process-exit-status (nth 1 holder))))
                (should (= 0 (process-exit-status (nth 1 replay))))
                (let ((hres (dorg-test-lin--parse-child (nth 2 holder)))
                      (rres (dorg-test-lin--parse-child (nth 2 replay))))
                  ;; The holder LOST its response: terminal indeterminate, not :ok.
                  (should (eq 'indeterminate (plist-get hres :reason)))
                  ;; The retry reconciled to the ORIGINAL commit — idempotent, no re-apply.
                  (should (plist-get rres :ok))
                  (should (plist-get rres :idempotent))
                  (should (equal c1 (plist-get rres :commit))))
                ;; Exactly ONE commit landed across the whole in-flight race.
                (should (= (1+ n0) (dorg-test-lin--commit-count dolt-dir)))
                (should (equal "one" (dorg-test-lin--prop dolt-dir id-a "S"))))))
        (dolist (p procs)
          (when (process-live-p (nth 1 p)) (delete-process (nth 1 p)))
          (when (buffer-live-p (nth 2 p)) (kill-buffer (nth 2 p))))
        (delete-directory bar t))))
  ;; ── NON-VACUITY: a GATELESS retry duplicates instead of reconciling ──
  (dorg-test--with-temp-db other-dir
    (let* ((orid (dorg-test-lin--seed other-dir "* Alpha\n"))
           (oid (dorg-test-lin--id other-dir "Alpha"))
           (port dorg-test--server-port)
           (on0 (dorg-test-lin--commit-count other-dir))
           (apiform `(on (id ,oid) (set (property "S") "one")))
           (bar (make-temp-file "dorg-lin-a161n-" t))
           (committing (expand-file-name "committing" bar))
           (release (expand-file-name "release" bar))
           (procs '()))
      (unwind-protect
          (let ((holder (dorg-test-lin--spawn
                         "hold" other-dir port orid
                         (dorg-test-lin--holder-body apiform committing release "op-X"))))
            (push holder procs)
            (should (dorg-test-lin--wait-file committing 60))
            ;; GATELESS retry: no GET_LOCK, so its preflight reads the mid-settle
            ;; window (evidence absent, HEAD==base) and re-applies from scratch.
            (let ((replay (dorg-test-lin--spawn
                           "gateless" other-dir port orid
                           `(progn
                              (dorg-test-lin--disable-gate)
                              (let ((r (dorg-api-execute ',apiform :mode 'apply
                                                         :source src :op-id "op-X")))
                                (list :ok (and (plist-get r :ok) t)
                                      :idempotent (plist-get r :idempotent)))))))
              (push replay procs)
              ;; It does NOT block — it commits a duplicate and returns.
              (should (dorg-test-lin--await-proc (nth 1 replay) 90))
              (should (= 0 (process-exit-status (nth 1 replay))))
              ;; Now release the holder; its own commit collides on the op-id
              ;; evidence (or is lost to the moved HEAD).
              (with-temp-file release (insert "go"))
              (should (dorg-test-lin--await-proc (nth 1 holder) 90))
              (should (= 0 (process-exit-status (nth 1 holder))))
              (let ((w2 (dorg-test-lin--parse-child (nth 2 replay)))
                    (w1 (dorg-test-lin--parse-child (nth 2 holder)))
                    (final (dorg-test-lin--commit-count other-dir)))
                ;; The gateless retry did NOT reconcile — it re-applied (not idempotent).
                (should-not (plist-get w2 :idempotent))
                ;; Exactly-once is broken: either a duplicate commit landed, or the
                ;; holder's re-commit failed on the op-id collision.
                (should (or (not (plist-get w1 :ok))
                            (> final (1+ on0)))))))
        (dolist (p procs)
          (when (process-live-p (nth 1 p)) (delete-process (nth 1 p)))
          (when (buffer-live-p (nth 2 p)) (kill-buffer (nth 2 p))))
        (delete-directory bar t)))))

;;; ─── A16.0 — allocated insert id is FIXED across re-plans ───────────────

(ert-deftest dorg-test-lin-op-id-insert-id-stable-across-replan ()
  "A16.0: an insert's allocated heading id is PINNED at first plan and reused by
every re-plan simulate, so a replay commits BYTE-IDENTICAL rows.  Forcing a
re-plan (stale base vs a disjoint intervening commit) makes the simulator run
exactly twice across the whole apply boundary: once for the immutable initial
attempt and once for the forced fresh re-plan.  The `:created' id is identical
on both runs and equals the committed id.
NON-VACUITY: defeating the id pin makes each re-plan simulate allocate a FRESH
id, so the per-simulate created ids DIVERGE."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Parent\n* Other\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (pid (dorg-test-lin--id dolt-dir "Parent"))
           (oid (dorg-test-lin--id dolt-dir "Other"))
           (h0 (dorg-head-commit dolt-dir)))
      ;; Disjoint writer advances HEAD off h0 (touches Other, not Parent).
      (should (plist-get (dorg-test--apply-patches
                          src (list (dorg-test--property-put-patch
                                     (dorg-test--heading-address oid) "P" "x"))
                                         :base-commit h0)
                         :ok))
      (should-not (equal h0 (dorg-head-commit dolt-dir)))
      ;; Record every simulation across the public apply boundary.  The first
      ;; builds the immutable attempt before the gate; the stale caller base
      ;; forces exactly one fresh attempt after the gate is held.
      (let* ((created '())
             (spy (lambda (res)
                    (push (plist-get res :created) created)
                    res)))
        (advice-add 'dorg-mutate--dolt-simulate :filter-return spy)
        (unwind-protect
            ;; Insert C under Parent carrying the STALE base h0: the initial
            ;; current-HEAD attempt plus one disjoint-safe re-plan.
            (should (plist-get (dorg-test-lin--api-apply
                                src `(insert (heading (title "C"))
                                             (under (id ,pid)))
                                :op-id "op-C" :base h0)
                               :ok))
          (advice-remove 'dorg-mutate--dolt-simulate spy))
        ;; Exactly one fresh attempt followed the immutable initial attempt...
        (should (= (length created) 2))
        ;; ...and every run allocated the SAME id (pinned once at first plan).
        (should (cl-every (lambda (c) (equal c (car created))) created))
        ;; ...which is the committed heading's id.
        (should (equal (car (car created)) (dorg-test-lin--id dolt-dir "C"))))
      ;; NON-VACUITY: without the pin, each re-plan simulate re-allocates a fresh
      ;; id, so the gated-loop created ids DIVERGE.
      (let ((h1 (dorg-head-commit dolt-dir)))
        (should (plist-get (dorg-test--apply-patches
                            src (list (dorg-test--property-put-patch
                                       (dorg-test--heading-address oid) "P" "y"))
                                           :base-commit h1)
                           :ok))
        (let* ((created2 '())
               (spy2 (lambda (res)
                       (push (plist-get res :created) created2)
                       res)))
          (cl-letf (((symbol-function 'dorg-mutate--pin-insert-heading-ids)
                     (lambda (ops) ops)))
            (advice-add 'dorg-mutate--dolt-simulate :filter-return spy2)
            (unwind-protect
                (should (plist-get (dorg-test-lin--api-apply
                                    src `(insert (heading (title "D"))
                                                 (under (id ,pid)))
                                    :op-id "op-D2" :base h1)
                                   :ok))
              (advice-remove 'dorg-mutate--dolt-simulate spy2)))
          (should (= (length created2) 2))
          (should-not (cl-every (lambda (c) (equal c (car created2))) created2)))))))

(ert-deftest dorg-test-lin-scoped-load-recomputed-on-disjoint-head-replan ()
  "A scoped-load slice is recomputed at the NEW plan HEAD on disjoint re-plan.
This is a RED spec for tracked-case.  A stale caller base H0 with a disjoint
intervening commit moves HEAD to H1; the gated apply must compute scoped ids and
load ids at H1 for every re-plan attempt, never reuse H0's slice."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Target\n* Other\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (target (dorg-test-lin--id dolt-dir "Target"))
           (other (dorg-test-lin--id dolt-dir "Other"))
           (h0 (dorg-head-commit dolt-dir)))
      ;; Disjoint writer advances HEAD off the caller's optimistic base.
      (should (plist-get (dorg-test-lin--api-apply
                          src `(on (id ,other) (set (property "P") "x"))
                          :base h0)
                         :ok))
      (let* ((h1 (dorg-head-commit dolt-dir))
             (scope-calls nil)
             (load-heading-calls nil)
             (load-tree-calls nil)
             (real-load-headings (symbol-function 'dorg-model-load-headings))
             (real-load-tree (symbol-function 'dorg-model-load-tree)))
        (should-not (equal h0 h1))
        (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                   (lambda (source patches root-id head)
                     (push (list :source source :patches patches
                                 :root root-id :head head)
                           scope-calls)
                     (list rid target)))
                  ((symbol-function 'dorg-model-load-headings)
                   (lambda (dir root ids &optional as-of headline-only
                                query-fn)
                     (push (list :root root :ids ids
                                 :as-of as-of :headline-only headline-only)
                           load-heading-calls)
                     (funcall real-load-headings dir root ids
                              as-of headline-only query-fn)))
                  ((symbol-function 'dorg-model-load-tree)
                   (lambda (dir root &optional as-of)
                     (push (list :root root :as-of as-of) load-tree-calls)
                     (funcall real-load-tree dir root as-of))))
          (should (plist-get
                   (dorg-test-lin--api-apply
                    src `(on (id ,target) (set (property "S") "v"))
                    :base h0)
                   :ok)))
        (should (>= (length scope-calls) 2))
        (should-not load-tree-calls)
        (dolist (call scope-calls)
          (should (equal h1 (plist-get call :head)))
          (should-not (equal h0 (plist-get call :head))))
        ;; Planning loads (as-of-bound) must be recomputed at H1 over the
        ;; recomputed scope; the postimage validator's in-window readback
        ;; loads (tracked-regression) are as-of NIL — the pending transaction
        ;; state at the new head — and are exempt from the slice assertions.
        ;; NOTHING may load at the stale H0.
        (let ((planning (cl-remove-if-not
                         (lambda (call) (plist-get call :as-of))
                         load-heading-calls)))
          (should (>= (length planning) 2))
          (dolist (call planning)
            (should (member rid (plist-get call :ids)))
            (should (member target (plist-get call :ids)))
            (should (equal h1 (plist-get call :as-of))))
          (dolist (call load-heading-calls)
            (should-not (equal h0 (plist-get call :as-of)))))))))

;;; ─── A16.4(a) — transport loss at commit -> terminal indeterminate ──────

(ert-deftest dorg-test-lin-in-commit-lost-response-is-terminal ()
  "21mx9: a landed commit with a lost response is terminal indeterminate.
The leased connection remains LIVE: advice first runs the real DOLT_COMMIT,
then signals a typed timeout carrying deliberately 1213-shaped prose before
the returned hash reaches the batch.  Before the fix that prose is falsely
classified as a rolled-back serialization failure and the same call performs
an unauthorized internal re-plan, whose gate-first evidence check returns
idempotent success.  Correct behavior is a terminal commit-indeterminate result
with NO ROLLBACK.  A later same-op-id call is the separate reconciliation
control and returns the one original commit without a second insertion.
NON-VACUITY: deleting the phase-aware branch makes the first-call reason and
zero-ROLLBACK assertions fail while the exactly-once control stays green."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (port dorg-test--server-port)
           (h0 (dorg-head-commit dolt-dir))
           (n0 (dorg-test-lin--commit-count dolt-dir))
           (apiform `(on (id ,id-a)
                         (insert (keyword "LOSS") (after-last) "once")))
           (results
            (dorg-test-lin--run-writers
             (list
              (list
               "lost" dolt-dir port rid
               `(progn
                  (setq dorg-test-lin--lost-response-fired nil
                        dorg-test-lin--lost-response-conn-live nil
                        dorg-test-lin--lost-response-rollbacks 0)
                  (advice-add 'dorg-sql--mysql-exec :around
                              #'dorg-test-lin--lose-first-dolt-commit-response)
                  (advice-add 'mysql-query :around
                              #'dorg-test-lin--count-lost-response-rollback)
                  (unwind-protect
                      (let* ((first
                              (condition-case e
                                  (let ((r (dorg-api-execute
                                            ',apiform :mode 'apply :source src
                                            :base ,h0 :op-id "op-21mx9")))
                                    (list :ok (plist-get r :ok)
                                          :idempotent (plist-get r :idempotent)
                                          :commit (plist-get r :new-token)))
                                (dorg-mutate-error
                                 (list :error t
                                       :reason (dorg-mutate-error-reason e)
                                       :data (dorg-mutate-error-data e)))))
                             (second
                              (dorg-api-execute
                               ',apiform :mode 'apply :source src
                               :base ,h0 :op-id "op-21mx9")))
                        (list :first first
                              :second
                              (list :ok (plist-get second :ok)
                                    :idempotent (plist-get second :idempotent)
                                    :commit (plist-get second :new-token))
                              :lost dorg-test-lin--lost-response-fired
                              :conn-live dorg-test-lin--lost-response-conn-live
                              :rollbacks dorg-test-lin--lost-response-rollbacks))
                    (advice-remove
                     'mysql-query #'dorg-test-lin--count-lost-response-rollback)
                    (advice-remove
                     'dorg-sql--mysql-exec
                     #'dorg-test-lin--lose-first-dolt-commit-response)))))
             120))
           (res (cdr (assoc "lost" results)))
           (first (plist-get res :first))
           (second (plist-get res :second))
           (c1 (dorg-head-commit dolt-dir))
           (count-row
            (car
             (dorg-sql
              dolt-dir
              (format
               (concat "SELECT COUNT(*) AS n FROM heading_items hi "
                       "JOIN keyword_items ki ON ki.item_id = hi.id "
                       "WHERE hi.heading_id = %d AND ki.keyword = 'LOSS' "
                       "AND ki.value_raw = 'once'")
               id-a))))
           (item-count (alist-get 'n count-row)))
      (when (stringp item-count)
        (setq item-count (string-to-number item-count)))
      (should (plist-get res :lost))
      (should (plist-get res :conn-live))
      (should (plist-get first :error))
      (should (eq 'indeterminate (plist-get first :reason)))
      (let ((data (plist-get first :data)))
        (should (eq 'commit-indeterminate (plist-get data :detail)))
        (should (eq 'in-commit (plist-get data :transaction-phase)))
        (should (equal "op-21mx9" (plist-get data :op-id)))
        (should (equal h0 (plist-get data :base-commit)))
        (should (equal h0 (plist-get data :plan-base)))
        (should (equal h0 (plist-get data :observed-head)))
        (should (equal (list rid) (plist-get data :root-scope)))
        (should (equal (list (cons rid (list rid id-a)))
                       (plist-get data :identity-scope))))
      (should (= 0 (plist-get res :rollbacks)))
      (should (= (1+ n0) (dorg-test-lin--commit-count dolt-dir)))
      (should (= 1 item-count))
      (should (plist-get second :ok))
      (should (plist-get second :idempotent))
      (should (equal c1 (plist-get second :commit))))))

(ert-deftest dorg-test-lin-op-id-indeterminate-on-transport-loss ()
  "A16.4(a): a REAL PRE-COMMIT socket drop in one OS PROCESS under `:op-id' yields
terminal `indeterminate' (echoing the op-id) with NO commit landed — the
KNOWN-NOT-COMMITTED boundary, distinct from A16.2's landed-then-died case.  The
exact leased connection is torn down as the commit batch begins, so the next send
raises a bare Emacs `error' that `dorg-sql--with-broken-guard' classifies as
`dorg-sql-lease-broken' -> the gated-apply catch maps it to typed `indeterminate'
\(NOT `invariant').  A later retry with the SAME op-id in a fresh process finds
evidence truly ABSENT and does a clean fresh apply, so exactly ONE commit results
— no double-apply, no silent loss.
NON-VACUITY: the drop is the exact socket, not a stub of the whole batch — WITHOUT
the guard's dead-process classification the bare error is mis-typed `invariant'
and this asserts `indeterminate'; and the reconciling retry lands exactly one
commit."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (port dorg-test--server-port)
           (n0 (dorg-test-lin--commit-count dolt-dir))
           (apiform `(on (id ,id-a) (set (property "S") "v")))
           ;; Child A: drop the exact leased socket BEFORE any commit SQL runs.
           (results (dorg-test-lin--run-writers
                     (list (list "drop" dolt-dir port rid
                                 `(progn
                                    (advice-add 'dorg-sql--mysql-batch-on-conn :around
                                                (lambda (orig conn &rest args)
                                                  (delete-process (mysql-conn-process conn))
                                                  (apply orig conn args)))
                                    (dorg-api-execute ',apiform :mode 'apply
                                                      :source src :op-id "op-I"))))
                     120))
           (res (cdr (assoc "drop" results))))
      ;; Typed indeterminate, echoing the op-id; nothing committed.
      (should (eq :error (car res)))
      (should (eq 'indeterminate (plist-get res :reason)))
      (should (equal "op-I" (plist-get (plist-get res :data) :op-id)))
      (should (= n0 (dorg-test-lin--commit-count dolt-dir)))
      (should-not (dorg-test-lin--prop dolt-dir id-a "S"))
      ;; A fresh process reconciles with the SAME op-id: evidence truly absent ->
      ;; a clean fresh apply landing exactly one commit.
      (let* ((r2 (dorg-test-lin--run-writers
                  (list (list "recon" dolt-dir port rid
                              `(let ((r (dorg-api-execute ',apiform :mode 'apply
                                                          :source src :op-id "op-I")))
                                 (list :ok (and (plist-get r :ok) t)
                                       :idempotent (plist-get r :idempotent)))))
                  120))
             (res2 (cdr (assoc "recon" r2))))
        (should (plist-get res2 :ok))
        (should-not (plist-get res2 :idempotent)))
      (should (= (1+ n0) (dorg-test-lin--commit-count dolt-dir)))
      (should (equal "v" (dorg-test-lin--prop dolt-dir id-a "S"))))))

;;; ─── A16.4(b) — retention window: within = idempotent, after = fresh ────

(ert-deftest dorg-test-lin-op-id-retention-window-reuse ()
  "A16.4(b): exactly-once holds only within the declared retention window W.  A
replay while the evidence row is live is idempotent (original commit); once the
row is pruned (past W) the same op-id is OUTSIDE the promise and RE-APPLIES as a
fresh generation.
NON-VACUITY: pruning is correctness-visible — the pre-prune replay returns the
ORIGINAL commit idempotently while the post-prune reuse produces a NEW commit."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (r1 (dorg-test-lin--api-apply
                src `(on (id ,id-a) (set (property "S") "one")) :op-id "op-R"))
           (c1 (plist-get r1 :commit)))
      (should (plist-get r1 :ok))
      ;; WITHIN the window: replay is idempotent, returns the original commit.
      (let ((r2 (dorg-test-lin--api-apply
                 src `(on (id ,id-a) (set (property "S") "one")) :op-id "op-R")))
        (should (plist-get r2 :idempotent))
        (should (equal c1 (plist-get r2 :commit))))
      ;; Prune past W: delete the current-state evidence row.
      (dorg-sql--with-lease (lease dolt-dir)
        (dorg-test-lin--delete-evidence lease "op-R"))
      (let ((n (dorg-test-lin--commit-count dolt-dir)))
        ;; AFTER the window: the same op-id re-applies as a fresh generation.
        (let ((r3 (dorg-test-lin--api-apply
                   src `(on (id ,id-a) (set (property "S") "two")) :op-id "op-R")))
          (should (plist-get r3 :ok))
          (should-not (plist-get r3 :idempotent)))
        (should (= (1+ n) (dorg-test-lin--commit-count dolt-dir)))
        (should (equal "two" (dorg-test-lin--prop dolt-dir id-a "S")))))))

;;; ─── A14 — public surface: op-id + timings through dorg-api-execute ─────

(ert-deftest dorg-test-lin-public-surface-op-id-and-timings ()
  "A14: typed terminal outcomes, the echoed `:op-id', and phase `:timings' are
observable through the PUBLIC `dorg-api-execute' (not the internal
`dorg-test--apply-patches') — a successful gated apply echoes the op-id and carries the
five phase timings, and a terminal overlap through the public surface carries the
op-id.
NON-VACUITY: a caller supplying NO op-id sees the byte-identical pre-change shape
(no `:op-id' field)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha")))
      ;; Success: op-id echoed + the five phase timings present publicly.
      (let ((r (dorg-test-lin--api-apply
                src `(on (id ,id-a) (set (property "S") "one")) :op-id "op-A")))
        (should (plist-get r :ok))
        (should (equal "op-A" (plist-get r :op-id)))
        (let ((tm (plist-get r :timings)))
          (should tm)
          (dolist (k '(:gate-wait :gate-hold :plan :sql :commit))
            (should (plist-member tm k)))))
      ;; Terminal overlap through the PUBLIC surface carries the op-id.
      (let ((h1 (dorg-head-commit dolt-dir)))
        (dorg-test--apply-patches
         src (list (dorg-test--property-put-patch
                    (dorg-test--heading-address id-a) "S" "ext"))
         :base-commit h1)
        (let ((err (should-error
                    (dorg-test-lin--api-apply
                     src `(on (id ,id-a) (set (property "S") "mine")) :op-id "op-B" :base h1)
                    :type 'dorg-mutate-error)))
          (should (eq 'overlap (dorg-mutate-error-reason err)))
          (should (equal "op-B" (plist-get (dorg-mutate-error-data err) :op-id)))))
      ;; NON-VACUITY: omitting op-id keeps the byte-identical pre-change shape.
      (let ((r (dorg-test-lin--api-apply src `(on (id ,id-a) (set (property "T") "z")))))
        (should (plist-get r :ok))
        (should-not (plist-member r :op-id))))))

;;; ─── A9 — render / materialize / git-sync are OUTSIDE the held gate ─────

(defvar dorg-test-lin--gate-held nil
  "Bound t by the A9 harness only while inside `dorg-mutate-call-with-store-gate',
so the downstream-symbol detectors flag any render/materialize/git call that fires
while the per-store mutation gate is held.")

(ert-deftest dorg-test-lin-boundary-no-render-export-in-gate ()
  "A9: the gate-held interval is strictly HEAD re-read + preflight + delta SQL +
evidence + DOLT_COMMIT — NONE of the downstream render / materialize / git-sync
entry points runs while the per-store gate is held.  A full gated apply through
the public envelope invokes zero of them inside the gate window.
NON-VACUITY: injecting a render call into the in-gate commit path is DETECTED by
the SAME instrument — proving the boundary assertion is load-bearing (move any
render/sync call inside the gate window and it goes RED)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (require 'dorg-render nil t)
  (require 'dorg-git nil t)
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           ;; Representative render / materialize / git-sync entry points; only
           ;; those actually loaded are instrumented (an unloaded symbol cannot
           ;; be reached from a mutation anyway).
           (forbidden (seq-filter #'fboundp
                                  '(dorg-render
                                    dorg-materialize dorg-git--run)))
           (violations '())
           (gate-flag (lambda (orig &rest args)
                        (let ((dorg-test-lin--gate-held t)) (apply orig args))))
           (detectors (mapcar (lambda (sym)
                                (cons sym (lambda (&rest _)
                                            (when dorg-test-lin--gate-held
                                              (push sym violations)))))
                              forbidden)))
      (should forbidden)                     ; at least one entry point is loaded
      (advice-add 'dorg-mutate-call-with-store-gate :around gate-flag)
      (dolist (d detectors) (advice-add (car d) :before (cdr d)))
      (unwind-protect
          (progn
            (should (plist-get (dorg-test-lin--api-apply
                                src `(on (id ,id-a) (set (property "S") "one")) :op-id "op-A")
                               :ok))
            ;; No render / materialize / git call ran inside the gate window.
            (should (null violations)))
        (dolist (d detectors) (advice-remove (car d) (cdr d)))
        (advice-remove 'dorg-mutate-call-with-store-gate gate-flag))
      ;; NON-VACUITY: an injected in-gate render IS caught by the same instrument.
      (when (fboundp 'dorg-render)
       (let* ((viol2 '())
             (probe (lambda (&rest _) (when dorg-test-lin--gate-held (push 'dorg-render viol2))))
             (orig (symbol-function 'dorg-sql--lease-batch)))
        (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                   (lambda (&rest args) (ignore-errors (dorg-render dolt-dir)) (apply orig args))))
          (advice-add 'dorg-mutate-call-with-store-gate :around gate-flag)
          (advice-add 'dorg-render :before probe)
          (unwind-protect
              (progn
                (dorg-test-lin--api-apply
                 src `(on (id ,id-a) (set (property "S") "two")) :op-id "op-B")
                (should (member 'dorg-render viol2)))
            (advice-remove 'dorg-render probe)
            (advice-remove 'dorg-mutate-call-with-store-gate gate-flag))))))))

;;; ─── A6 — same-process non-reentrancy (consumer of the due6u lease) ─────

(ert-deftest dorg-test-lin-same-process-non-reentrancy ()
  "A6: within ONE process, while a logical mutation M1 is IN FLIGHT holding the
store gate, a SECOND concurrent logical mutation M2 against the SAME store —
attempted from inside M1's gate-held window — cannot proceed: acquiring a second
lease is refused (`dorg-sql-lease-contended') and ordinary non-lease SQL on the
leased store is refused (`dorg-sql-lease-busy'), so two logical mutations can
never share one session or interleave on the gate.  M1 itself still commits.  The
ownership PRIMITIVE is tracked-regression; this certifies the CONSUMER contract.
NON-VACUITY: multiplexing one cached session would let M2's acquire succeed and
let ordinary SQL touch the held session — both refusals would vanish (and the
`should' asserting they fired would fail)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (probe-results nil)
           ;; Fires during M1's in-gate HEAD re-read (gate held on M1's lease):
           ;; run M2's two entry seams and record how each is refused.  Runs once.
           (probe (lambda (&rest _)
                    (unless probe-results
                      (setq probe-results
                            (list :acquire
                                  (condition-case _e
                                      (progn (dorg-sql--acquire-lease dolt-dir) :NOT-REFUSED)
                                    (dorg-sql-lease-contended 'contended)
                                    (error 'other))
                                  :sql
                                  (condition-case _e
                                      (progn (dorg-sql dolt-dir "SELECT 1 AS one") :NOT-REFUSED)
                                    (dorg-sql-lease-busy 'busy)
                                    (error 'other)))))))
           (fired nil))
      (advice-add 'dorg-sql--lease-scalar :before probe)
      (unwind-protect
          (setq fired (plist-get (dorg-test--apply-patches
                                  src (list (dorg-test--property-put-patch
                                             (dorg-test--heading-address id-a)
                                             "S" "v")))
                                 :ok))
        (advice-remove 'dorg-sql--lease-scalar probe))
      ;; M1 committed...
      (should fired)
      (should (equal "v" (dorg-test-lin--prop dolt-dir id-a "S")))
      ;; ...and M2, attempted concurrently inside M1's gate, was refused on BOTH
      ;; seams: no second lease, no ordinary SQL on the held session.
      (should (eq 'contended (plist-get probe-results :acquire)))
      (should (eq 'busy (plist-get probe-results :sql))))))

;;; ─── A4 — crash / timeout auto-releases the gate ────────────────────────

(ert-deftest dorg-test-lin-crash-releases-gate ()
  "A4: the gate is SESSION-bound — a SEPARATE OS PROCESS acquires the store gate
on its leased session, then is KILLED (SIGKILL) mid-hold with NO release.  Its
session drops, so the server auto-releases the named lock and a second
independent session acquires the SAME gate within a bounded time, with NO manual
unlock and no leaked artifact.  Mirrors the live-swarm orphan (a 99%-CPU emacs
holding `.tracker/.lock' 60-90s), proven across a real process boundary.
NON-VACUITY: a FILE / non-session lock would stay held after the kill and block
the second acquirer past the bound."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-lin--seed dolt-dir "* Alpha\n")
    (let* ((port dorg-test--server-port)
           (key (dorg-sql--with-lease (l dolt-dir) (dorg-sql--mutate-lock-key l)))
           ;; Child acquires the gate on its OWN leased session, announces it,
           ;; then blocks holding it until killed.
           (holder (dorg-test-lin--spawn
                    "holder" dolt-dir port 100
                    '(dorg-sql--with-lease (lease (plist-get src :dolt-dir))
                       (let ((k (dorg-sql--mutate-lock-key lease)))
                         (if (dorg-sql--lease-get-lock lease k 5)
                             (progn (princ "\n@@ACQUIRED@@\n") (sleep-for 60))
                           (princ "\n@@NOLOCK@@\n"))))))
           (proc (nth 1 holder))
           (buf (nth 2 holder)))
      (unwind-protect
          (progn
            ;; Wait for the child to actually HOLD the gate.
            (let ((deadline (+ (float-time) 20)))
              (while (and (process-live-p proc)
                          (not (with-current-buffer buf
                                 (goto-char (point-min))
                                 (re-search-forward "@@ACQUIRED@@" nil t)))
                          (< (float-time) deadline))
                (accept-process-output proc 0.2)))
            (should (with-current-buffer buf
                      (goto-char (point-min))
                      (re-search-forward "@@ACQUIRED@@" nil t)))
            ;; KILL the holder mid-hold (SIGKILL, no chance to release).
            (delete-process proc)
            ;; A second independent session acquires the SAME gate promptly —
            ;; the killed session's lock auto-released server-side.
            (let ((conn2 (dorg-test-lin--raw-conn dolt-dir)))
              (unwind-protect
                  (should (eql 1 (dorg-test-lin--raw-get-lock conn2 key 10)))
                (ignore-errors (mysql-disconnect conn2)))))
        (when (process-live-p proc) (delete-process proc))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; ─── A5 — forced lease drop -> ZERO commit, fail-closed ─────────────────

(ert-deftest dorg-test-lin-forced-lease-drop-zero-commit ()
  "A5: a REAL transport drop between GET_LOCK and DOLT_COMMIT aborts fail-closed
on the LEASED session.  The exact leased connection's process is torn down the
instant the commit batch begins, so the next send hits a DEAD socket and raises a
bare Emacs `error' (\"... not running\"), which `dorg-sql--with-broken-guard' must
classify as the TYPED `dorg-sql-lease-broken'.  ZERO commit lands, HEAD is
unchanged, and the failure never reconnects to commit on a replacement session
\(no-reconnect lease contract).
NON-VACUITY: this is the exact-socket drop, not a stub of the whole batch —
WITHOUT the guard's dead-process classification the bare error escapes UNTYPED
\(the gated-apply catch would mis-type it `invariant'), so `should-error :type
dorg-sql-lease-broken' is RED; and routing the commit through the reconnecting
`dorg-sql--get-connection' seam would let a NEW unlocked session commit after the
drop — here HEAD does not advance at all."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (h0 (dorg-head-commit dolt-dir))
           (n (dorg-test-lin--commit-count dolt-dir))
           (orig (symbol-function 'dorg-sql--mysql-batch-on-conn)))
      ;; Real drop: delete the leased connection's process as the commit batch
      ;; starts, so the very first send inside the batch is on a dead socket.
      (cl-letf (((symbol-function 'dorg-sql--mysql-batch-on-conn)
                 (lambda (conn statements commit-message
                               &optional validate-function report-rollback)
                   (delete-process (mysql-conn-process conn))
                   (funcall orig conn statements commit-message
                            validate-function report-rollback))))
        ;; No op-id: the transport failure propagates fail-closed (never a
        ;; reconnect-and-commit on a replacement session).
        (should-error (dorg-test--apply-patches
                       src (list (dorg-test--property-put-patch
                                  (dorg-test--heading-address id-a) "S" "v")))
                      :type 'dorg-sql-lease-broken))
      ;; ZERO commit: HEAD unchanged, count unchanged, value unset.
      (should (equal h0 (dorg-head-commit dolt-dir)))
      (should (= n (dorg-test-lin--commit-count dolt-dir)))
      (should-not (dorg-test-lin--prop dolt-dir id-a "S")))))

;;; ─── A10 — the gate is DISTINCT from the sql-server claim lock ──────────

(ert-deftest dorg-test-lin-gate-distinct-from-claim-lock ()
  "A10: the per-store mutation gate (a `dorg/mutate/...' DB SESSION lock) is a
DIFFERENT lock from the sql-server lifecycle CLAIM lock (`<store>/.dorg/server.claim',
a FILE artifact).  The mutation key is a DB-derived string that never embeds the
claim path, and a gated mutation runs with the claim file present.
NON-VACUITY: if the mutation reused the claim artifact, its key would embed the
claim path / `server.claim' (it does not)."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (claim (dorg-dolt-server--claim-file dolt-dir))
           (key (dorg-sql--with-lease (l dolt-dir) (dorg-sql--mutate-lock-key l))))
      ;; The gate key is a DB session-lock name, NOT the claim FILE path.
      (should (string-prefix-p dorg-sql--mutate-lock-prefix key))
      (should-not (string-match-p (regexp-quote (directory-file-name claim)) key))
      (should-not (string-match-p "server\\.claim" key))
      ;; A gated mutation runs regardless of the claim overlay file.
      (should (plist-get (dorg-test--apply-patches
                          src (list (dorg-test--property-put-patch
                                     (dorg-test--heading-address id-a) "S" "v")))
                         :ok))
      (should (equal "v" (dorg-test-lin--prop dolt-dir id-a "S"))))))

;;; ─── A12 — fail-closed without a persistent server session ──────────────

(ert-deftest dorg-test-lin-fail-closed-without-server ()
  "A12: with the gate REQUIRED but NO persistent server session obtainable, a Dolt
mutation FAILS CLOSED — it never falls through to an unsynchronized one-shot CLI
DOLT_COMMIT.
NON-VACUITY: a fall-through-to-CLI-commit path would return :ok and advance HEAD
with no linearization; here a typed transport error is raised and HEAD is
unchanged."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (id-a (dorg-test-lin--id dolt-dir "Alpha"))
           (h0 (dorg-head-commit dolt-dir))
           (n (dorg-test-lin--commit-count dolt-dir)))
      ;; Simulate 'no persistent server': the gate cannot obtain a leased session.
      (cl-letf (((symbol-function 'dorg-sql--acquire-lease)
                 (lambda (&rest _)
                   (signal 'dorg-sql-transport-error (list "no persistent server")))))
        (should-error (dorg-test--apply-patches
                       src (list (dorg-test--property-put-patch
                                  (dorg-test--heading-address id-a) "S" "v")))
                      :type 'dorg-sql-transport-error))
      ;; Fail-closed: no unsynchronized CLI commit slipped through.
      (should (equal h0 (dorg-head-commit dolt-dir)))
      (should (= n (dorg-test-lin--commit-count dolt-dir))))))

;;; ─── A8 — core + participant rows are ONE atomic transaction ────────────

(defun dorg-test-lin--failing-project (ctx)
  "An index projector whose derived effect FAILS (NULL PRIMARY KEY) whenever a
heading is changed/created — used to prove the gate rolls core + derived back
together.  Pure fn of the `dorg-ext-index-context'; returns a typed row delta on
its OWNED table that passes manifest validation but, once DOrg generates the SQL,
violates a NOT-NULL primary-key constraint at execution."
  (when (or (dorg-ext-index-context-changed ctx)
            (dorg-ext-index-context-created ctx))
    (list (list :table "demo_fail_proj"
                :insert (list (cons 'heading_id nil) (cons 'title "boom"))))))

(ert-deftest dorg-test-lin-participant-failure-atomic-rollback ()
  "A8: a registered extension participant's projection SQL runs in the SAME Dolt
transaction as the core mutation.  When the participant SQL fails (a constraint
violation), NEITHER core NOR participant rows persist, HEAD is unchanged, and the
result is typed `:reason invariant' — the gate never splits the single txn.
NON-VACUITY: if the gate wrapped only the core commit and let participant SQL
commit separately, the core title change would persist after the participant
failure; here the core row is untouched and HEAD does not advance."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'failproj
       :schema '("CREATE TABLE demo_fail_proj (heading_id BIGINT PRIMARY KEY, title TEXT)")
       :version 1
       :capabilities '(dolt)
       :read-tables '((:name "demo_fail_proj" :key-column "heading_id"))
       :project #'dorg-test-lin--failing-project)
      (dorg-repo-ensure-extensions dolt-dir)
      (dorg-commit dolt-dir "install failproj")
      (let* ((rid (dorg-test-lin--seed dolt-dir "* Alpha\n"))
             (src (dorg-test-lin--src dolt-dir rid))
             (id-a (dorg-test-lin--id dolt-dir "Alpha"))
             (h0 (dorg-head-commit dolt-dir))
             (n (dorg-test-lin--commit-count dolt-dir))
             (title-of (lambda ()
                         (alist-get 'title
                                    (car (dorg-sql dolt-dir
                                                   (format "SELECT title FROM headings WHERE id = %d"
                                                           id-a)))))))
        ;; A core update marks Alpha :changed -> the participant runs its FAILING
        ;; projection SQL inside the committing transaction.
        (let ((err (should-error
                    (dorg-test--apply-patches
                     src (list (dorg-test--heading-update-patch
                                id-a :title "Beta")))
                    :type 'dorg-mutate-error)))
          (should (eq 'invariant (dorg-mutate-error-reason err))))
        ;; ATOMIC rollback: the core title change did NOT persist...
        (should (equal "Alpha" (funcall title-of)))
        ;; ...no participant row persisted...
        (should (= 0 (let ((v (alist-get 'n (car (dorg-sql dolt-dir
                                                           "SELECT COUNT(*) AS n FROM demo_fail_proj")))))
                       (if (stringp v) (string-to-number v) v))))
        ;; ...and HEAD did not advance.
        (should (equal h0 (dorg-head-commit dolt-dir)))
        (should (= n (dorg-test-lin--commit-count dolt-dir)))))))

;;; ─── A2 — six disjoint writers: no loss, linear, bounded progress ───────

(ert-deftest dorg-test-lin-six-disjoint-writers-bounded-progress ()
  "A2: six DISJOINT writers, each a SEPARATE OS PROCESS, each INSERTing a distinct
child under the SAME parent from the SAME base — disjoint SQL write-sets (six new
ids) that SHARE the parent's sibling-ordering predicate (A15 scaled to six).

DETERMINISTIC CONTENTION (reviewer v3 blocker 2): the PARENT itself acquires the
canonical store-gate `GET_LOCK' on an independent raw session and HOLDS it, then
launches all six writers.  Each writer plans OUTSIDE the gate, drops an `at-gate'
marker the instant it ENTERS `dorg-mutate-call-with-store-gate' (post-plan), and
then BLOCKS on `GET_LOCK' — which the parent owns.  With all six proven at the
gate the parent asserts the load-bearing contention fact: every writer is still
ALIVE and HEAD has NOT advanced — the held gate genuinely blocks the writers (a
non-excluding gate, or a writer bypassing it, would commit while the parent holds
the lock and move HEAD -> RED).  This replaces the prior vacuous `:gate-wait > 0'
check (production measures acquire ELAPSED wall time, which is positive even for
an instantly-acquired uncontended lock).  The parent then RELEASES the gate; the
six blocked writers contend for real, one commits and each stale-base loser
RE-PLANS its sort key onto the growing HEAD (Design §2.4 3b).  All six durably
persist as exactly SIX linear single-parent commits; none returns `contended'
within the sized deadline.  Progress is serialize + bounded re-plan, NOT GET_LOCK
fairness (undefined).  The Downstream tracker `.tracker/.lock' is NOT used — the DOrg per-store
gate is the sole linearizer, proven across true process boundaries.

NON-VACUITY (cross-process transformation, gate DISABLED): re-run a barrier
fan-out with the gate BYPASSED in every child (`GET_LOCK'/release no-ops) and a
SECOND rendezvous just before the commit batch, so all six pass the HEAD==base CAS
and commit their STALE plans against oh0 together.  Each stale plan computed its
sort key against the empty parent, so committing them without mutual exclusion
collides on `uq_heading_sibling_order' — the exact qe9r write-skew the gate closes.
The clean six-child / six-linear-commit outcome the ENABLED gate produced does NOT
hold -> RED.  (Deleting the gate makes the transformation fail; a zeroed retry
budget alone — which still serializes on the lock — would not.)"
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (let ((barrier (make-temp-file "dorg-lin-barrier-" t)))
    (unwind-protect
        (progn
          ;; ── Positive: parent HOLDS the gate, proves six writers block, then
          ;;    releases and proves bounded progress ──
          (dorg-test--with-temp-db dolt-dir
            (let* ((rid (dorg-test-lin--seed dolt-dir "* Parent\n"))
                   (port dorg-test--server-port)
                   (pid (dorg-test-lin--id dolt-dir "Parent"))
                   (h0 (dorg-head-commit dolt-dir))
                   (base-count (dorg-test-lin--commit-count dolt-dir))
                   ;; The canonical key the writers' gate will contend on.
                   (key (dorg-sql--with-lease (l dolt-dir)
                          (dorg-sql--mutate-lock-key l)))
                   ;; An independent raw session the PARENT holds the gate on.
                   (hold-conn (dorg-test-lin--raw-conn dolt-dir))
                   (marker (lambda (i) (expand-file-name (format "atgate-%d" i) barrier)))
                   (procs '()))
              (unwind-protect
                  (progn
                    ;; Parent acquires and HOLDS the store gate.
                    (should (eql 1 (dorg-test-lin--raw-get-lock hold-conn key 10)))
                    ;; Launch six real-process writers; each blocks on the held gate.
                    (setq procs
                          (cl-loop for i from 1 to 6 collect
                                   (dorg-test-lin--spawn
                                    (format "w%d" i) dolt-dir port rid
                                    `(progn
                                       (dorg-test-lin--install-at-gate-marker
                                        ,(funcall marker i))
                                       (let* ((dorg-mutate-gate-acquire-timeout 120)
                                              (dorg-mutate-gate-deadline 120)
                                              (r (dorg-test--apply-patches
                                                  src (list (dorg-test--tree-insert-patch
                                                             (dorg-test--heading-address ,pid)
                                                             (list :title ,(format "C%d" i))))
                                                  :base-commit ,h0)))
                                         (list :ok (and (plist-get r :ok) t)
                                               :reason (plist-get r :reason)))))))
                    ;; Every writer has planned and ENTERED the gate (post-plan).
                    (dolist (i '(1 2 3 4 5 6))
                      (should (dorg-test-lin--wait-file (funcall marker i) 90)))
                    ;; Let them all reach the GET_LOCK block, then prove DETERMINISTIC
                    ;; contention: all still alive+blocked, and NOTHING committed while
                    ;; the parent holds the gate (a non-excluding gate would move HEAD).
                    (accept-process-output nil 2.0)
                    (dolist (p procs) (should (process-live-p (nth 1 p))))
                    (should (equal h0 (dorg-head-commit dolt-dir)))
                    (should (= base-count (dorg-test-lin--commit-count dolt-dir)))
                    ;; Release the gate -> the six blocked writers contend for real
                    ;; and make bounded progress (serialize + re-plan).
                    (dorg-test-lin--raw-release hold-conn key)
                    (dolist (p procs)
                      (should (dorg-test-lin--await-proc (nth 1 p) 120)))
                    (dolist (p procs)
                      (should (eq 'exit (process-status (nth 1 p))))
                      (should (= 0 (process-exit-status (nth 1 p)))))
                    ;; All six reported clean success — no error, no contended.
                    (let ((results (mapcar (lambda (p) (dorg-test-lin--parse-child (nth 2 p)))
                                           procs)))
                      (dolist (res results)
                        (should (plist-get res :ok))
                        (should-not (eq :error (car res)))
                        (should-not (eq 'contended (plist-get res :reason)))))
                    ;; Every disjoint child durably persisted (stale-base losers
                    ;; re-planned a distinct sort key) under a well-defined order...
                    (let ((kids (dorg-test-lin--child-titles dolt-dir pid)))
                      (should (= 6 (length kids)))
                      (dolist (i '(1 2 3 4 5 6))
                        (should (member (format "C%d" i) kids))))
                    ;; ...as exactly six linear single-parent commits (no merge, no collision).
                    (should (= (+ base-count 6) (dorg-test-lin--commit-count dolt-dir)))
                    (should (= 1 (dorg-test-lin--parents-of-head dolt-dir))))
                (dolist (p procs)
                  (when (process-live-p (nth 1 p)) (delete-process (nth 1 p)))
                  (when (buffer-live-p (nth 2 p)) (kill-buffer (nth 2 p))))
                (ignore-errors (dorg-test-lin--raw-release hold-conn key))
                (ignore-errors (mysql-disconnect hold-conn)))))
          ;; ── NON-VACUITY: gate DISABLED, double rendezvous forces same-base overlap ──
          (dorg-test--with-temp-db other-dir
            (let* ((orid (dorg-test-lin--seed other-dir "* Parent\n"))
                   (port dorg-test--server-port)
                   (opid (dorg-test-lin--id other-dir "Parent"))
                   (oh0 (dorg-head-commit other-dir))
                   (obase (dorg-test-lin--commit-count other-dir))
                   (specs (cl-loop for i from 1 to 6 collect
                                   (list (format "r%d" i) other-dir port orid
                                         `(progn
                                            (dorg-test-lin--install-ungated-double-barrier
                                             ,barrier "u" ,(format "r%d" i) 120)
                                            (let ((r (dorg-test--apply-patches
                                                      src (list (dorg-test--tree-insert-patch
                                                                 (dorg-test--heading-address ,opid)
                                                                 (list :title ,(format "C%d" i))))
                                                      :base-commit ,oh0)))
                                              (list :ok (and (plist-get r :ok) t)))))))
                   (results (dorg-test-lin--run-writers
                             specs 180
                             :after-spawn
                             (lambda ()
                               ;; Release the pre-gate rendezvous, then (once every
                               ;; child has passed the HEAD==base CAS) the pre-commit
                               ;; rendezvous — so all six commit their stale plans
                               ;; against oh0 together.
                               (should (dorg-test-lin--barrier-release barrier "ua" 6 120))
                               (should (dorg-test-lin--barrier-release barrier "ub" 6 120)))))
                   (committed (seq-count (lambda (rc) (plist-get (cdr rc) :ok)) results))
                   (nkids (length (dorg-test-lin--child-titles other-dir opid)))
                   (final (dorg-test-lin--commit-count other-dir))
                   (parents (dorg-test-lin--parents-of-head other-dir)))
              (should (= 6 (length results)))
              ;; Gate removed -> linearization BROKEN: the stale sort keys collide,
              ;; so the clean six-child / six-linear-commit result the ENABLED gate
              ;; produced does NOT hold (a child was lost to a raw constraint conflict,
              ;; the commit count is off, or a merge second-parent HEAD appeared).
              (should-not (and (= committed 6)
                               (= 6 nkids)
                               (= (+ obase 6) final)
                               (= 1 parents))))))
      (delete-directory barrier t))))

;;; ─── Commit-failure classification + fresh replan (tracked-regression) ─────

(defun dorg-test-f4la4--source ()
  "Return a minimal resolved Dolt source for pure gated-apply specifications."
  (dorg-mutate--source-make
   :kind 'dolt :dolt-dir "/tmp/dorg-f4la4-store" :root-id 100))

(defun dorg-test-f4la4--attempt (base sql &optional result)
  "Return one explicit f4la4 plan attempt at BASE carrying SQL and RESULT."
  (list :caller-base "caller-base"
        :plan-base base
        :root-scope '(100)
        :identity-scope '((100 101 102))
        :write-set '(101)
        :sql (list sql)
        :make-result (lambda (head)
                       (append (or result '(:ok t)) (list :commit head)))
        :empty '(:ok t :empty t)))

(defmacro dorg-test-f4la4--with-pure-gate (head-fn batch-fn &rest body)
  "Run BODY with a pure store gate using HEAD-FN and BATCH-FN.
The idempotency preflight is empty; no filesystem, server, or SQL connection is
touched.  HEAD-FN replaces leased HEAD reads and BATCH-FN replaces the commit."
  (declare (indent 2) (debug (form form body)))
  `(cl-letf (((symbol-function 'dorg-head-commit) (lambda (&rest _) "implicit-head"))
             ((symbol-function 'dorg-mutate-call-with-store-gate)
              (lambda (_dir fn &rest _) (funcall fn 'lease "gate")))
             ((symbol-function 'dorg-sql--lease-scalar) ,head-fn)
             ((symbol-function 'dorg-sql--lease-batch) ,batch-fn)
             ((symbol-function 'dorg-sql--lease-changed-heading-ids)
              (lambda (&rest _) nil))
             ((symbol-function 'dorg-sql--ensure-mutation-log) (lambda (&rest _) nil))
             ((symbol-function 'dorg-sql--ensure-mutation-log-postimages)
              (lambda (&rest _) nil))
             ((symbol-function 'dorg-sql--mutation-log-lookup) (lambda (&rest _) nil))
             ((symbol-function 'dorg-sql--mutation-log-insert-sql)
              (lambda (&rest _) "EVIDENCE")))
     ,@body))

(ert-deftest dorg-test-f4la4-attempt-plan-base-controls-in-gate-cas ()
  "The gate compares HEAD to the attempt's actual plan base, never implicit state.
The implicit/caller base deliberately differs from `:plan-base'.  A correct gate
plans and commits once; the old implicit `h-plan' coupling needlessly plans twice."
  (let ((plans 0) (batches 0))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _) (if (> batches 0) "commit-head" "planned-head"))
        (lambda (&rest _) (cl-incf batches) "commit-head")
      (let ((result
             (dorg-mutate--dolt-gated-apply
              (dorg-test-f4la4--source) nil nil
              (lambda ()
                (cl-incf plans)
                (dorg-test-f4la4--attempt "planned-head" "SQL-ONE")))))
        (should (plist-get result :ok))
        (should (= plans 1))
        (should (= batches 1))))))

(ert-deftest dorg-test-f4la4-serialization-without-op-id-is-terminal-stale ()
  "A rollback-confirmed serialization failure is not retried without `:op-id'."
  (let ((plans 0) (batches 0))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _) "plan-1")
        (lambda (&rest _)
          (cl-incf batches)
          (signal 'dorg-sql-transaction-rolled-back
                  '("Dolt transaction rolled back"
                    (:rollback-confirmed t :class serialization :db-code 1213))))
      (let* ((err
              (should-error
               (dorg-mutate--dolt-gated-apply
                (dorg-test-f4la4--source) nil nil
                (lambda ()
                  (cl-incf plans)
                  (dorg-test-f4la4--attempt "plan-1" "POISON-ONE")))
               :type 'dorg-mutate-error))
             (data (dorg-mutate-error-data err)))
        (should (eq 'stale (dorg-mutate-error-reason err)))
        (should (eq 'serialization (plist-get data :detail)))
        (should (= plans 1))
        (should (= batches 1))))))

(ert-deftest dorg-test-f4la4-serialization-op-id-discards-sql-and-replans ()
  "An idempotent serialization retry discards attempt one and fully replans.
The poison first statement must never occur in the successful second batch."
  (let ((plans 0) (batches '()))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _)
          (pcase (length batches)
            (0 "plan-1")
            (1 "plan-2")
            (_ "commit-2")))
        (lambda (_lease statements &rest _)
          (push statements batches)
          (when (= (length batches) 1)
            (signal 'dorg-sql-transaction-rolled-back
                    '("Dolt transaction rolled back"
                      (:rollback-confirmed t :class serialization :db-code 1213))))
          "commit-2")
      (let ((result
             (dorg-mutate--dolt-gated-apply
              (dorg-test-f4la4--source)
              '(:op-id "op-replan" :intent-digest "digest") nil
              (lambda ()
                (cl-incf plans)
                (if (= plans 1)
                    (dorg-test-f4la4--attempt "plan-1" "POISON-ONE")
                  (dorg-test-f4la4--attempt "plan-2" "SAFE-TWO"))))))
        (should (plist-get result :ok))
        (should (= plans 2))
        (should (= (length batches) 2))
        (let ((successful (car batches)))
          (should (member "SAFE-TWO" successful))
          (should-not (member "POISON-ONE" successful)))))))

(ert-deftest dorg-test-f4la4-equal-head-1062-is-sanitized-planner-defect ()
  "Equal-HEAD sibling-order 1062 is terminal, auditable, and never raw/retried."
  (let ((plans 0) (batches 0))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _) "equal-head")
        (lambda (&rest _)
          (cl-incf batches)
          (signal 'dorg-sql-transaction-rolled-back
                  '("Dolt transaction rolled back"
                    (:rollback-confirmed t
                     :class sibling-order-duplicate
                     :db-code 1062
                     :constraint uq-heading-sibling-order))))
      (let* ((err
              (should-error
               (dorg-mutate--dolt-gated-apply
                (dorg-test-f4la4--source) nil nil
                (lambda ()
                  (cl-incf plans)
                  (dorg-test-f4la4--attempt "equal-head" "DUPLICATE")))
               :type 'dorg-mutate-error))
             (data (dorg-mutate-error-data err))
             (public (format "%s %S" (error-message-string err) data)))
        (should (eq 'invariant (dorg-mutate-error-reason err)))
        (should (eq 'sibling-order-planner-defect (plist-get data :detail)))
        (should (eq t (plist-get data :head-equal-at-failure)))
        (should (equal "equal-head" (plist-get data :plan-base)))
        (should (equal "equal-head" (plist-get data :observed-head)))
        (should (equal '(100) (plist-get data :root-scope)))
        (should (equal '((100 101 102)) (plist-get data :identity-scope)))
        (should (= 1062 (plist-get data :db-code)))
        (should (eq 'uq-heading-sibling-order (plist-get data :constraint)))
        (should-not (plist-member data :underlying))
        (should-not (string-match-p
                     (regexp-opt '("Duplicate" "DUPLICATE" "RAW")) public))
        (should (= plans 1))
        (should (= batches 1))))))

(ert-deftest dorg-test-f4la4-unproven-head-never-becomes-planner-defect ()
  "Sibling 1062 without an equal-HEAD fact is stale, never planner defect."
  (let* ((attempt (dorg-test-f4la4--attempt "plan" "DUPLICATE"))
         (receipt
          '(dorg-sql-transaction-rolled-back
            "Dolt transaction rolled back"
            (:rollback-confirmed t
             :class sibling-order-duplicate
             :db-code 1062
             :constraint uq-heading-sibling-order)))
         (err (should-error
               (dorg-mutate--rolled-back-decision
                receipt attempt "different" nil nil (+ (float-time) 10))
               :type 'dorg-mutate-error)))
    (should (eq 'stale (dorg-mutate-error-reason err)))
    (should (eq 'concurrent-state-unproven
                (plist-get (dorg-mutate-error-data err) :detail)))
    (should-not
     (eq 'sibling-order-planner-defect
         (plist-get (dorg-mutate-error-data err) :detail)))))

(ert-deftest dorg-test-f4la4-rollback-failure-never-retries ()
  "Even an op-id cannot authorize retry when rollback was not confirmed."
  (let ((plans 0) (batches 0))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _) "plan")
        (lambda (&rest _)
          (cl-incf batches)
          (signal 'dorg-sql-transaction-rollback-failed
                  '("Dolt transaction rollback failed"
                    (:rollback-confirmed nil
                     :class serialization :db-code 1213))))
      (let* ((err
              (should-error
               (dorg-mutate--dolt-gated-apply
                (dorg-test-f4la4--source)
                '(:op-id "op-no-rollback" :intent-digest "digest") nil
                (lambda ()
                  (cl-incf plans)
                  (dorg-test-f4la4--attempt "plan" "POISON")))
               :type 'dorg-mutate-error))
             (data (dorg-mutate-error-data err)))
        (should (eq 'invariant (dorg-mutate-error-reason err)))
        (should (eq 'rollback-failed (plist-get data :detail)))
        (should (= plans 1))
        (should (= batches 1))))))

(ert-deftest dorg-test-f4la4-serialization-deadline-exhaustion-is-stale ()
  "An op-id serialization receipt cannot outlive the existing gate deadline."
  (let ((plans 0) (batches 0)
        (dorg-mutate-gate-deadline -1))
    (dorg-test-f4la4--with-pure-gate
        (lambda (&rest _) "plan")
        (lambda (&rest _)
          (cl-incf batches)
          (signal 'dorg-sql-transaction-rolled-back
                  '("Dolt transaction rolled back"
                    (:rollback-confirmed t
                     :class serialization :db-code 1213))))
      (let* ((err
              (should-error
               (dorg-mutate--dolt-gated-apply
                (dorg-test-f4la4--source)
                '(:op-id "op-expired" :intent-digest "digest") nil
                (lambda ()
                  (cl-incf plans)
                  (dorg-test-f4la4--attempt "plan" "POISON")))
               :type 'dorg-mutate-error))
             (data (dorg-mutate-error-data err)))
        (should (eq 'stale (dorg-mutate-error-reason err)))
        (should (eq 'serialization (plist-get data :detail)))
        (should (= plans 1))
        (should (= batches 1))))))

(ert-deftest dorg-test-f4la4-cross-root-attempt-records-both-scopes ()
  "The cross-root executor records both roots and their exact bounded slices."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-lin--seed dolt-dir "* A\n** A-kid\n" 100)
    (dorg-test-lin--seed dolt-dir "* Destination\n" 500)
    (let* ((src (dorg-test-lin--src dolt-dir 100))
           (a (dorg-test-lin--id dolt-dir "A"))
           (destination (dorg-test-lin--id dolt-dir "Destination"))
           (head (dorg-head-commit dolt-dir))
           (patch
            (dorg-test--move-patch
             a (dorg-test--heading-address destination)
             :to-source
             (dorg-mutate--resolve-source
              (list :dolt-dir dolt-dir :root-id 500))))
           attempt)
      (cl-letf (((symbol-function 'dorg-mutate--dolt-gated-apply)
                 (lambda (_source _options _base plan-fn &optional _message)
                   (setq attempt (funcall plan-fn)))))
        (dorg-test--apply-patches src (list patch) :base-commit head))
      (should (equal head (plist-get attempt :caller-base)))
      (should (equal head (plist-get attempt :plan-base)))
      (should (equal '(100 500) (plist-get attempt :root-scope)))
      (should (equal '(100 500)
                     (mapcar #'car (plist-get attempt :identity-scope))))
      (dolist (entry (plist-get attempt :identity-scope))
        (should (listp (cdr entry)))
        (should (cl-every #'integerp (cdr entry))))
      (should (plist-get attempt :sql))
      (should (plist-get attempt :write-set)))))

(defun dorg-test-f4la4--core-snapshot (dolt-dir)
  "Return deterministic byte-sensitive rows for the three mutation core tables."
  (list
   :headings
   (dorg-sql
    dolt-dir
    (concat
     "SELECT id, root_id, parent_id, HEX(sort_key) AS sort_key, level, "
     "todo_keyword, priority, commented, title, post_blank, items_hash "
     "FROM headings ORDER BY id"))
   :tags
   (dorg-sql dolt-dir
             "SELECT heading_id, ordinal, tag FROM heading_tags ORDER BY heading_id, ordinal")
   :items
   (dorg-sql dolt-dir
             "SELECT * FROM heading_items ORDER BY heading_id, sort_key, id")))

(defun dorg-test-f4la4--constraint-violation-count (dolt-dir)
  "Return committed row count in Dolt's aggregate constraint-violation table."
  (let ((value
         (alist-get
          'n
          (car (dorg-sql dolt-dir
                         "SELECT COUNT(*) AS n FROM dolt_constraint_violations")))))
    (if (stringp value) (string-to-number value) value)))

(ert-deftest dorg-test-f4la4-real-sibling-1062-rolls-back-before-classifying ()
  "A real sibling-order 1062 rolls back an earlier statement and is sanitized.
The hand-built attempt enters through the production gated commit seam: its
first statement visibly changes an existing title, then its second statement
duplicates the enforced parent/sort slot.  Classification is allowed only after
the same-session rollback receipt; HEAD and all three core tables must be exact."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed
                 dolt-dir
                 "* Parent\n** Existing\n* Unrelated :keep:\n#+NOTE: untouched\nBody.\n"))
           (source (dorg-mutate--source-make
                    :kind 'dolt :dolt-dir dolt-dir :root-id rid))
           (parent (dorg-test-lin--id dolt-dir "Parent"))
           (existing (dorg-test-lin--id dolt-dir "Existing"))
           (slot (alist-get
                  'sort_key
                  (car (dorg-sql
                        dolt-dir
                        (format "SELECT HEX(sort_key) AS sort_key FROM headings WHERE id = %d"
                                existing)))))
           (head0 (dorg-head-commit dolt-dir))
           (rows0 (dorg-test-f4la4--core-snapshot dolt-dir))
           (attempts 0)
           (err
            (should-error
             (dorg-mutate--dolt-gated-apply
              source nil nil
              (lambda ()
                (cl-incf attempts)
                (list
                 :caller-base nil
                 :plan-base head0
                 :root-scope (list rid)
                 :identity-scope (list (cons rid (list rid parent existing)))
                 :write-set (list existing)
                 :sql
                 (list
                  (format "UPDATE headings SET title = 'TRANSIENT' WHERE id = %d"
                          existing)
                  (format
                   (concat
                    "INSERT INTO headings "
                    "(id, root_id, parent_id, sort_key, level, title) "
                    "VALUES (99999991, %d, %d, X'%s', 2, 'Collision')")
                   rid parent slot))
                 :make-result (lambda (head) (list :ok t :commit head))
                 :empty '(:ok t :empty t))))
             :type 'dorg-mutate-error))
           (data (dorg-mutate-error-data err))
           (envelope
            (json-encode
             (dorg-envelope-error-json
              (dorg-mutate-error-reason err) (cadr err) data)))
           (public (format "%s %S %s"
                           (error-message-string err) data envelope)))
      (should (= attempts 1))
      (should (eq 'invariant (dorg-mutate-error-reason err)))
      (should (eq 'sibling-order-planner-defect (plist-get data :detail)))
      (should (eq t (plist-get data :head-equal-at-failure)))
      (should (equal head0 (plist-get data :plan-base)))
      (should (equal head0 (plist-get data :observed-head)))
      (should (= 1062 (plist-get data :db-code)))
      (should (eq 'uq-heading-sibling-order (plist-get data :constraint)))
      (should-not (plist-member data :underlying))
      (should-not
       (string-match-p
        (regexp-opt (list "TRANSIENT" "Collision" "Duplicate entry"
                          "duplicate unique key given" "HY000" slot))
        public))
      ;; The transforming first statement and the collision both rolled back.
      (should (equal head0 (dorg-head-commit dolt-dir)))
      (should (equal rows0 (dorg-test-f4la4--core-snapshot dolt-dir)))
      (should (= 0 (dorg-test-f4la4--constraint-violation-count dolt-dir))))))

(defun dorg-test-f4la4--heading-snapshot (dolt-dir heading-id)
  "Return exact core/tag/item rows owned by HEADING-ID."
  (list
   :heading
   (dorg-sql
    dolt-dir
    (format
     (concat
      "SELECT id, root_id, parent_id, HEX(sort_key) AS sort_key, level, "
      "todo_keyword, priority, commented, title, post_blank, items_hash "
      "FROM headings WHERE id = %d")
     heading-id))
   :tags
   (dorg-sql dolt-dir
             (format (concat "SELECT heading_id, ordinal, tag FROM heading_tags "
                             "WHERE heading_id = %d ORDER BY ordinal")
                     heading-id))
   :items
   (dorg-sql dolt-dir
             (format (concat "SELECT id, heading_id, HEX(sort_key) AS sort_key, kind, post_blank "
                             "FROM heading_items WHERE heading_id = %d ORDER BY sort_key, id")
                     heading-id))))

(defun dorg-test-f4la4--mixed-writer
    (src parent-id title base marker whole-root)
  "CHILD-side mixed-scope writer used by the real f4la4 race.
WHOLE-ROOT forces the retained nil-IDS load fallback.  Return success, final
created ids, plan count, and every explicit identity-scope record."
  (let ((plans 0)
        (scopes nil)
        (orig-scope (symbol-function 'dorg-mutate--dolt-scope-ids)))
    (dorg-test-lin--install-at-gate-marker marker)
    (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
               (if whole-root (lambda (&rest _) nil) orig-scope)))
      (advice-add
       'dorg-mutate--dolt-gated-apply :around
       (lambda (orig source options caller-base plan-fn &optional message)
         (funcall
          orig source options caller-base
          (lambda ()
            (let ((attempt (funcall plan-fn)))
              (cl-incf plans)
              (push (plist-get attempt :identity-scope) scopes)
              attempt))
          message)))
      (let ((result
             (dorg-test--apply-patches
              src
              (list (dorg-test--tree-insert-patch
                     (dorg-test--heading-address parent-id)
                     (list :title title)))
              :base-commit base)))
        (list :ok (and (plist-get result :ok) t)
              :created (plist-get result :created)
              :plans plans
              :scopes (nreverse scopes))))))

(ert-deftest dorg-test-f4la4-mixed-scope-race-replans-and-preserves-unrelated ()
  "Bounded and whole-root typed writers linearize from one base without loss.
The parent holds the real gate until both child processes have planned, proving
the two attempts share a base and sibling-order predicate.  One writer uses the
normal bounded identity slice; one forces the retained nil-IDS whole-root load.
After release, exactly one loser replans, both generated identities persist as
two linear commits, constraints stay clean, and an unrelated heading's
core/tag/item rows remain byte-identical."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (let ((markers (make-temp-file "dorg-f4la4-mixed-" t)))
    (unwind-protect
        (dorg-test--with-temp-db dolt-dir
          (let* ((rid (dorg-test-lin--seed
                       dolt-dir
                       "* Parent\n** Existing\n* Unrelated :keep:\n#+NOTE: untouched\nBody.\n"))
                 (parent (dorg-test-lin--id dolt-dir "Parent"))
                 (unrelated (dorg-test-lin--id dolt-dir "Unrelated"))
                 (unrelated0
                  (dorg-test-f4la4--heading-snapshot dolt-dir unrelated))
                 (port dorg-test--server-port)
                 (head0 (dorg-head-commit dolt-dir))
                 (commits0 (dorg-test-lin--commit-count dolt-dir))
                 (key (dorg-sql--with-lease (lease dolt-dir)
                        (dorg-sql--mutate-lock-key lease)))
                 (holder (dorg-test-lin--raw-conn dolt-dir))
                 (bounded-marker (expand-file-name "bounded" markers))
                 (whole-marker (expand-file-name "whole" markers))
                 procs)
            (unwind-protect
                (progn
                  (should (eql 1 (dorg-test-lin--raw-get-lock holder key 10)))
                  (setq procs
                        (list
                         (dorg-test-lin--spawn
                          "bounded" dolt-dir port rid
                          `(dorg-test-f4la4--mixed-writer
                            src ,parent "Bounded" ,head0 ,bounded-marker nil))
                         (dorg-test-lin--spawn
                          "whole" dolt-dir port rid
                          `(dorg-test-f4la4--mixed-writer
                            src ,parent "Whole" ,head0 ,whole-marker t))))
                  ;; Both attempts finished their outside-gate plan at HEAD0.
                  (should (dorg-test-lin--wait-file bounded-marker 90))
                  (should (dorg-test-lin--wait-file whole-marker 90))
                  (should (equal head0 (dorg-head-commit dolt-dir)))
                  (dorg-test-lin--raw-release holder key)
                  (dolist (proc procs)
                    (should (dorg-test-lin--await-proc (nth 1 proc) 120)))
                  (let* ((results
                          (mapcar (lambda (proc)
                                    (cons (car proc)
                                          (dorg-test-lin--parse-child (nth 2 proc))))
                                  procs))
                         (bounded (cdr (assoc "bounded" results)))
                         (whole (cdr (assoc "whole" results)))
                         (plan-counts
                          (sort (list (plist-get bounded :plans)
                                      (plist-get whole :plans)) #'<))
                         (created
                          (append (plist-get bounded :created)
                                  (plist-get whole :created))))
                    (should (plist-get bounded :ok))
                    (should (plist-get whole :ok))
                    (should (equal '(1 2) plan-counts))
                    ;; Attempt one records the actual load shape; the loser may
                    ;; record the same shape again on its fresh plan.
                    (should (listp (cdar (car (plist-get bounded :scopes)))))
                    (should (eq :all (cdar (car (plist-get whole :scopes)))))
                    (should (= 2 (length created)))
                    (should-not (= (car created) (cadr created))))
                  (should (equal '("Bounded" "Existing" "Whole")
                                 (sort (dorg-test-lin--child-titles
                                        dolt-dir parent) #'string<)))
                  (should (= (+ commits0 2)
                             (dorg-test-lin--commit-count dolt-dir)))
                  (should (= 1 (dorg-test-lin--parents-of-head dolt-dir)))
                  (should (= 0 (dorg-test-f4la4--constraint-violation-count
                                dolt-dir)))
                  (should (equal unrelated0
                                 (dorg-test-f4la4--heading-snapshot
                                  dolt-dir unrelated))))
              (ignore-errors (dorg-test-lin--raw-release holder key))
              (ignore-errors (mysql-disconnect holder))
              (dolist (proc procs)
                (when (process-live-p (nth 1 proc)) (delete-process (nth 1 proc)))
                (when (buffer-live-p (nth 2 proc)) (kill-buffer (nth 2 proc)))))))
      (delete-directory markers t))))

(defun dorg-test-f4la4--capture-insert-attempt
    (src parent-id title whole-root)
  "Return a typed insert attempt without committing it."
  (let ((orig-scope (symbol-function 'dorg-mutate--dolt-scope-ids)))
    (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
               (if whole-root (lambda (&rest _) nil) orig-scope))
              ((symbol-function 'dorg-mutate--dolt-gated-apply)
               (lambda (_source _options _base plan-fn &optional _message)
                 (funcall plan-fn))))
      (dorg-test--apply-patches
       src
       (list (dorg-test--tree-insert-patch
              (dorg-test--heading-address parent-id)
              (list :title title)))))))

(ert-deftest dorg-test-f4la4-stale-mixed-scope-sql-negative-control ()
  "Replaying the whole-root writer's stale SQL really produces sibling 1062.
Two typed plans are captured at one untouched HEAD.  The bounded plan commits;
the whole-root plan is then deliberately replayed without the gate's replan and
must roll back as the sanitized sibling duplicate.  This is the non-vacuity
control for the enabled mixed-scope race."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test--with-temp-db dolt-dir
    (let* ((rid (dorg-test-lin--seed dolt-dir "* Parent\n** Existing\n"))
           (src (dorg-test-lin--src dolt-dir rid))
           (parent (dorg-test-lin--id dolt-dir "Parent"))
           (bounded
            (dorg-test-f4la4--capture-insert-attempt
             src parent "Bounded" nil))
           (whole
            (dorg-test-f4la4--capture-insert-attempt
             src parent "Whole" t)))
      (should (equal (plist-get bounded :plan-base)
                     (plist-get whole :plan-base)))
      (should (eq :all (cdar (plist-get whole :identity-scope))))
      (dorg-sql--with-lease (lease dolt-dir)
        (dorg-sql--lease-batch lease (plist-get bounded :sql) "bounded stale-control"))
      (let ((head1 (dorg-head-commit dolt-dir))
            (rows1 (dorg-test-f4la4--core-snapshot dolt-dir)))
        (dorg-sql--with-lease (lease dolt-dir)
          (let* ((err
                  (should-error
                   (dorg-sql--lease-batch
                    lease (plist-get whole :sql) "whole stale-control" nil t)
                   :type 'dorg-sql-transaction-rolled-back))
                 (facts (dorg-sql-transaction-rollback-data err)))
            (should (eq 'sibling-order-duplicate (plist-get facts :class)))
            (should (= 1062 (plist-get facts :db-code)))
            (should (eq 'uq-heading-sibling-order
                        (plist-get facts :constraint)))))
        (should (equal head1 (dorg-head-commit dolt-dir)))
        (should (equal rows1 (dorg-test-f4la4--core-snapshot dolt-dir)))
        (should (= 0 (dorg-test-f4la4--constraint-violation-count dolt-dir)))))))

(provide 'dorg-mutate-linearize-tests)
;;; dorg-mutate-linearize-tests.el ends here
