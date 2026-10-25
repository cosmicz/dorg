;;; dorg-dolt-server.el --- Dolt sql-server lifecycle for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manages `dolt sql-server' processes for dorg repositories.
;; Each dolt-dir can have at most one server.  When a server is
;; running, `dorg-sql' routes queries through the dolt client mode
;; (`dolt --host --port --use-db sql -q -r json') for ~4x speedup.
;;
;; Main entry points:
;; - `dorg-dolt-server--ensure-strict' — canonical session-owned acquire:
;;   reuse/probe a local server, attach a validated live owner, or start one
;; - `dorg-dolt-server--start' — start server, return port
;; - `dorg-dolt-server--stop' — stop server for a dolt-dir
;; - `dorg-dolt-server-stop-all' — stop all managed servers
;; - `dorg-dolt-server-port' — return port if server running, nil otherwise
;; - `dorg-dolt-server-adopt-port' — adopt an inherited server port as a
;;   client attachment (for async workers), never as an owner entry
;; - `dorg-sql-connect' — open interactive SQL shell via Emacs sql.el
;;
;; User-facing management commands (over the same machinery):
;; - `dorg-dolt-server-list' — structured inventory of managed servers/claims
;; - `dorg-dolt-list' — tabulated display of that inventory
;; - `dorg-dolt-start' — ensure a server for a repo (defaults to buffer)
;; - `dorg-dolt-stop' — owner-safe stop/detach for a repo
;; - `dorg-dolt-stop-all' — stop all servers this Emacs owns

;;; Code:

(require 'dorg-dolt)
(require 'dorg-registry)         ; dorg-registered-dolt-dir (no cycle: registry never requires server)
(require 'dorg-sexp-file)        ; atomic single-sexp file I/O (claim/index/lock payloads)

;; KEEP (external): sql.el's own variable, set here to register dorg connections.
(defvar sql-connection-alist nil
  "Connection definitions used by `sql-connect'.")

;; Emacs sql.el is loaded lazily via (require 'sql) inside the interactive
;; `dorg-sql-connect' (below), so heavy sql.el is not pulled in at top level.
(declare-function sql-connect "sql" (&optional connection buffer-name))
(autoload 'dorg-repo--relative-path-match "dorg-repo")

;;; ─── Internal State ────────────────────────────────────────────────────

(defcustom dorg-dolt-server-startup-timeout 30
  "Seconds to wait for dolt sql-server to accept connections."
  :type 'number
  :group 'dorg)

;; KEEP (mutable-state): runtime table of owned server procs, puthash-mutated across calls.
(defvar dorg-dolt-server--procs (make-hash-table :test 'equal)
  "Map of canonical dolt-dir to (process . port) cons cells.
Entries here are servers this Emacs process started and owns.")

(defcustom dorg-dolt-server-index-dir
  (file-name-as-directory
   (expand-file-name
    (or (getenv "DORG_DOLT_SERVER_INDEX_DIR")
        (locate-user-emacs-file "dorg/server-stores/"))))
  "Stable directory of non-authoritative managed-store discovery pointers.
Each DOrg claim remains authoritative and store-relative at
`<store>/.dorg/server.claim'.  This directory contains only canonical store
paths, one file per store, so a fresh CLI process can find those claims for
`dorg dolt list'.  It never grants ownership and is safe to rebuild.  The
`DORG_DOLT_SERVER_INDEX_DIR' environment variable overrides the default for
isolated tests and deployments."
  :type 'directory
  :group 'dorg)

(defcustom dorg-dolt-server-autostart nil
  "Non-nil to implicitly start a detached dolt sql-server when none exists.
Governs only the no-usable-server case of the hot batch/one-shot `server'
SQL path (`dorg-dolt-server--strict-port'): a locally owned server, a
previously attached port, and a live attachable claim are always reused
regardless of this option.  When nil (the default) and no such server is
present, that path signals `dorg-sql-transport-error' instead of starting a
process or writing claim/`sql-server.info' metadata; the error names the
remedies — run `dorg dolt start PATH' or enable this option.  Explicit
`dorg-dolt-start' (and `dorg dolt start') and `dorg-session' warm-session
ownership always start/attach a server and are not governed by this option."
  :type 'boolean
  :group 'dorg)

(defcustom dorg-dolt-server-health-timeout 2.0
  "Seconds bounding the trivial SQL round-trip used to health-check a server.
A wedged dolt sql-server keeps its listen socket, so a TCP-accept probe
\(`dorg-dolt-server--port-live-p') cannot tell it apart from a healthy one.
`dorg-dolt-server--port-healthy-p' opens a short-lived mysql.el connection and
runs `SELECT 1', treating no answer within this many seconds as unhealthy
\(wedged).  Kept small so an attach decision against a wedged server fails
fast instead of parking on mysql.el's default idle timeout."
  :type 'number
  :group 'dorg)

(defcustom dorg-dolt-server-stop-term-grace 5.0
  "Seconds to wait for a SIGTERM'd sql-server to exit before SIGKILL.
A wedged Dolt server can ignore SIGTERM (it required SIGKILL in the
2026-06-24 port-59000 incident), so the reaper escalates to SIGKILL once
this grace elapses.  Bounds `dorg-dolt-server--kill-process' and
`dorg-dolt-server--kill-pid'."
  :type 'number
  :group 'dorg)

(defcustom dorg-dolt-server-bound-org-globs '("*.org" "*/*.org")
  "Globs (relative to a positional server DIR) probed for DOrg-bound org files.
The `dorg dolt' misbound-root guard (`dorg-app--reject-misbound-dir')
uses these to discover documents under DIR whose `#+DORG_DOLT_DIR:'
binding points at a Dolt store other than DIR, so that
`dorg dolt start .' from a project root is refused instead of
publishing a managed claim for an accidental root `.dolt'.  The default
covers top-level org files and any one-level-nested org file — the
common layout for a tool that keeps its tracker document in a dedicated
subdirectory.  Each pattern is expanded with `file-expand-wildcards';
add patterns for deeper or differently-placed layouts, or set to
\\='(\"*.org\") to probe only the top level."
  :type '(repeat string)
  :group 'dorg)

(defconst dorg-dolt-server-claim-protocol 1
  "Version of the DOrg server-claim format and attach protocol.
A claim whose `:protocol' differs from this value is treated as
incompatible: strict `server' transport refuses to attach to it
rather than risk a mismatched wire/lifecycle contract.")

;; KEEP (mutable-state): runtime table of owned claim tokens, puthash-mutated across calls.
(defvar dorg-dolt-server--owned-claims (make-hash-table :test 'equal)
  "Map of canonical dolt-dir to the owner token for claims this process owns.
Used so claim removal is token-checked: a process only deletes a claim
file whose `:token' matches the token it recorded here, so it cannot
clobber a newer claim written by another process after a restart race.")

;; KEEP (mutable-state): runtime table of attached client ports, puthash-mutated across calls.
(defvar dorg-dolt-server--attached-ports (make-hash-table :test 'equal)
  "Map of canonical dolt-dir to a port this process attached to as a client.
Populated when strict `server' transport attaches to a sql-server owned
by another DOrg process; lets the hot query path reuse the port without
re-probing the claim on every call.  Attached entries are clients, not
owners — closing them releases only the mysql connection, never the
owner's claim.")

;;; ─── Helpers ───────────────────────────────────────────────────────────

(defun dorg-dolt-server--canonical-dir (dolt-dir)
  "Return canonical path for DOLT-DIR."
  (plist-get (dorg-dolt--target dolt-dir) :canonical-repo-dir))

(defun dorg-dolt-server--db-name (dolt-dir)
  "Extract database name from DOLT-DIR.
Dolt uses the directory name as the database name."
  (plist-get (dorg-dolt--target dolt-dir) :db-name))

(defun dorg-dolt-server--data-dir-and-db (dolt-dir)
  "Derive the (DATA-DIR . DB-NAME) `dolt sql-server' target for DOLT-DIR.
A `dolt sql-server --data-dir DATA-DIR' pointed AT a Dolt repo serves
that repo as a single database named after the data-dir's basename.
DOrg therefore points the data-dir at DOLT-DIR itself and selects the
database named after DOLT-DIR's basename — keeping every server-level
artifact (`.dolt/sql-server.info', `.doltcfg', `config.yaml') inside the
store rather than materializing them in DOLT-DIR's parent.

DOLT-DIR is canonicalized (symlinks resolved, trailing slash stripped)
before deriving so the result is stable regardless of how the caller
spelled the path.  DATA-DIR is returned slash-terminated.

Examples:
  /u/x/.dorg/dolt/    -> (\"/u/x/.dorg/dolt/\" . \"dolt\")
  /u/x/repo/          -> (\"/u/x/repo/\"       . \"repo\")  ; standalone repo"
  (let ((target (dorg-dolt--target dolt-dir)))
    (cons (plist-get target :server-data-dir)
          (plist-get target :db-name))))

(defun dorg-dolt-server--free-port ()
  "Find a free TCP port by binding to port 0 and reading the assignment."
  (let* ((proc (make-network-process
                :name "dorg-port-finder"
                :host "127.0.0.1"
                :service 0
                :server t
                :family 'ipv4))
         (port (process-contact proc :service)))
    (delete-process proc)
    port))

(defun dorg-dolt-server--wait-ready (port &optional timeout proc)
  "Wait for server to accept connections on PORT.
TIMEOUT is seconds to wait (default `dorg-dolt-server-startup-timeout').
When PROC is non-nil, abort early if that process dies rather than
waiting out the full timeout.  Signals an error on timeout or on
early process exit."
  (let ((deadline (+ (float-time) (or timeout dorg-dolt-server-startup-timeout)))
        (ready nil))
    (while (and (not ready)
                (< (float-time) deadline)
                (or (null proc) (process-live-p proc)))
      (condition-case nil
          (let ((probe (make-network-process
                        :name "dorg-probe"
                        :host "127.0.0.1"
                        :service port
                        :family 'ipv4
                        :nowait nil)))
            (delete-process probe)
            (setq ready t))
        (error (accept-process-output nil 0.1))))
    (unless ready
      (if (and proc (not (process-live-p proc)))
          (error "dorg-dolt-server: dolt sql-server exited before becoming ready on port %d" port)
        (error "dorg-dolt-server: timed out waiting for server on port %d" port)))))

(defun dorg-dolt-server--list-served-dbs (port)
  "Return the list of database names served by the sql-server on PORT.
Connects as a client over the dolt CLI's global `--host/--port' wire
\(no mysql.el dependency) and runs `SHOW DATABASES'.  Returns nil on any
connection or parse failure — callers treat that as \"expected DB not
confirmed served\" and fail closed rather than guessing."
  (ignore-errors
    (let ((buf (generate-new-buffer " *dorg-show-databases*")))
      (unwind-protect
          (let ((exit (call-process
                       "dolt" nil buf nil
                       "--host" "127.0.0.1"
                       "--port" (number-to-string port)
                       ;; root has an empty password; pass it explicitly so
                       ;; the dolt CLI does not block on an interactive
                       ;; credentials prompt under --batch.
                       "--user" "root" "--password" "" "--no-tls"
                       "sql" "-q" "SHOW DATABASES" "-r" "json")))
            (when (eq exit 0)
              (with-current-buffer buf
                (let* ((parsed (dorg-sql--parse-json-or-nil
                                (string-trim (buffer-string))))
                       (rows (and parsed (alist-get 'rows parsed))))
                  ;; `rows' is a vector of single-column alists; the column
                  ;; name varies by Dolt version (Database / database), so
                  ;; take each row's sole value rather than keying by name.
                  (mapcar (lambda (row)
                            (let ((cell (if (listp row) (car row) row)))
                              (format "%s" (if (consp cell) (cdr cell) cell))))
                          (append rows nil))))))
        (kill-buffer buf)))))

(defun dorg-dolt-server--normalize-db-name (name)
  "Return NAME normalized for served-DB membership: whitespace-trimmed, downcased.
Dolt lowercases/normalizes database identifiers, and a served name parsed
from wire output can carry surrounding whitespace.  Applied to BOTH sides of
a membership test so the decision is case- and whitespace-insensitive."
  (downcase (string-trim (format "%s" name))))

(defun dorg-dolt-server--db-in-served-p (served db-name)
  "Return non-nil when DB-NAME is a member of the SERVED snapshot.
PURE predicate over an already-fetched SERVED list (of names): it issues no
served-DB query of its own.  Membership is case- and whitespace-insensitive
on BOTH the served names and DB-NAME (see `dorg-dolt-server--normalize-db-name'),
matching how Dolt normalizes identifiers.  This is the single-snapshot
membership test each decision site reuses for both its verdict and its
diagnostic, so the two can never disagree."
  (let ((want (dorg-dolt-server--normalize-db-name db-name)))
    (cl-some (lambda (d) (equal (dorg-dolt-server--normalize-db-name d) want))
             served)))

(defcustom dorg-dolt-server-probe-attempts 3
  "TOTAL served-DB probe attempts before an inconclusive verdict.
A served-DB metadata probe (`SHOW DATABASES' over the dolt CLI wire) can fail
transiently — a connection refused against a warming server, a truncated read.
Collapsing that transient failure to `nil' let a healthy server be misreported
as serving the wrong database, killing or breaking a live server (the
2026-07-17 tracker outage).  This bounds a retry of an INCONCLUSIVE probe: the
first CONFIRMED served result short-circuits, and after this many TOTAL
attempts (>=1, NOT retries-after-first) the FIRST attempt's cause is retained
so the diagnostic names the original failure."
  :type 'integer
  :group 'dorg)

(defun dorg-dolt-server--probe-served-dbs (port)
  "Probe the databases served on PORT, tri-state.
Returns `(served . LIST)' on a CONFIRMED exit-0 JSON parse whose `rows' member
is an array of single-column row objects — LIST may be empty (a confirmed-empty
served set) — or `(inconclusive . CAUSE)', a human-readable cause string, on ANY
connection/exit/parse/shape failure.  This REPLACES the nil-collapsing
`dorg-dolt-server--list-served-dbs' shim on every lifecycle decision path: a
transient inability to probe is DISTINCT from a confirmed served set that lacks
the expected DB, so a caller never treats a transient failure as an
authoritative wrong-db.  Strict about shape: missing `rows', a scalar/object
`rows', a malformed row entry, a nonzero exit, and a SIGNALLED `call-process'
\(which returns a string) are ALL `inconclusive', never `served'."
  (let ((buf (generate-new-buffer " *dorg-show-databases*")))
    (unwind-protect
        (let ((exit (condition-case err
                        (call-process
                         "dolt" nil buf nil
                         "--host" "127.0.0.1"
                         "--port" (number-to-string port)
                         "--user" "root" "--password" "" "--no-tls"
                         "sql" "-q" "SHOW DATABASES" "-r" "json")
                      (error (error-message-string err)))))
          (if (not (eq exit 0))
              ;; Nonzero integer exit OR a signal string (both non-`eq' 0):
              ;; the probe could not complete => inconclusive, never confirmed.
              (cons 'inconclusive
                    (format "`SHOW DATABASES' on port %s did not complete (%s)"
                            port exit))
            (let* ((raw (with-current-buffer buf (string-trim (buffer-string))))
                   (parsed (dorg-sql--parse-json-or-nil raw))
                   (rows-cell (and (consp parsed) (assq 'rows parsed))))
              (cond
               ;; Unparseable output, or a parsed object with no `rows' member:
               ;; nothing was confirmed.
               ((null rows-cell)
                (cons 'inconclusive
                      (format "`SHOW DATABASES' on port %s returned no parseable `rows' (%s)"
                              port (if (> (length raw) 80)
                                       (concat (substring raw 0 80) "…")
                                     raw))))
               ;; `rows' present but not a JSON array (scalar/object => not a
               ;; list under `:array-type list', or a bare alist): malformed.
               ((not (listp (cdr rows-cell)))
                (cons 'inconclusive
                      (format "`SHOW DATABASES' on port %s returned a malformed `rows' member"
                              port)))
               (t
                ;; Extract each row's sole column value; a row that is not a
                ;; single-column object (e.g. a scalar in the array, or `rows'
                ;; that was actually a flat object) is malformed => inconclusive.
                (let ((names nil) (malformed nil))
                  (dolist (row (cdr rows-cell))
                    (if (and (consp row)
                             (null (cdr row))
                             (consp (car row)))
                        (push (format "%s" (cdar row)) names)
                      (setq malformed t)))
                  (if malformed
                      (cons 'inconclusive
                            (format "`SHOW DATABASES' on port %s returned malformed row entries"
                                    port))
                    (cons 'served (nreverse names)))))))))
      (kill-buffer buf))))

(defun dorg-dolt-server--served-dbs-or-inconclusive (port)
  "Probe served DBs on PORT with a bounded retry; tri-state result.
Returns `(served . LIST)' on the FIRST confirmed served result, or
`(inconclusive . FIRST-CAUSE)' after `dorg-dolt-server-probe-attempts' TOTAL
inconclusive attempts.  Retains the FIRST attempt's cause (first-cause policy)
so bound exhaustion reports the ORIGINAL failure, not a later one.  The single
retry seam every served-DB decision site routes through (with the primitive) —
no lifecycle function calls the nil-collapsing shim directly."
  (let ((attempts (max 1 dorg-dolt-server-probe-attempts))
        (i 0) (first-cause nil) (result nil))
    (while (and (< i attempts) (null result))
      (let ((probe (dorg-dolt-server--probe-served-dbs port)))
        (if (eq (car probe) 'served)
            (setq result probe)
          (unless first-cause (setq first-cause (cdr probe)))
          (setq i (1+ i)))))
    (or result (cons 'inconclusive first-cause))))

;;; ─── Same-Host Server Ownership Claims ──────────────────────────────────
;;
;; `dorg-dolt-server--procs' tracks only servers started by THIS Emacs process,
;; so it cannot tell whether another process already owns a sql-server for
;; the same Dolt repo.  A claim is a small file under the store-relative
;; DOrg overlay (`<store>/.dorg/server.claim'), recording the owner
;; pid/port/token/protocol/timestamps.  Strict `server' transport reads it to
;; attach to an existing server, break a stale one, or error rather than start
;; a second server or fall back to one-shot CLI against a repo a sql-server
;; already locks (the one-shot-CLI lock hazard).

(defun dorg-dolt-server--claim-key (dolt-dir)
  "Return a filesystem-safe key identifying DOLT-DIR's canonical path.
Used for diagnostics and compatibility with old hash-keyed test fixtures;
the live overlay is store-relative (`dorg-dolt-server--claim-file')."
  (md5 (dorg-dolt-server--canonical-dir dolt-dir)))

(defun dorg-dolt-server--store-dorg-dir (dolt-dir)
  "Return the store-relative DOrg overlay dir `<store>/.dorg/' for DOLT-DIR.
Co-locating DOrg's ownership overlay inside the Dolt store (rather than a
per-process `<TMPDIR>/dorg/server') makes every client — any TMPDIR or
environment — compute the same path, so the one managed server per store
is discoverable cross-environment.  Consistent with io3e8: artifacts stay
IN the store, never its parent."
  (file-name-as-directory
   (expand-file-name ".dorg" (dorg-dolt-server--canonical-dir dolt-dir))))

(defun dorg-dolt-server--ensure-dorg-dir (dolt-dir)
  "Create `<store>/.dorg/' (0700) with a self-ignoring `.gitignore'; return it.
The self-ignore (`*') keeps this volatile runtime state out of git even
when the project has not ignored the store, since the near-universal
`.dolt/' ignore pattern does not cover a `.dorg/' sibling."
  (let ((dir (dorg-dolt-server--store-dorg-dir dolt-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (ignore-errors (set-file-modes dir #o700))
      (ignore-errors
        (let ((gi (expand-file-name ".gitignore" dir)))
          (unless (file-exists-p gi)
            (with-temp-file gi (insert "*\n"))))))
    dir))

(defun dorg-dolt-server--claim-file (dolt-dir)
  "Return the store-relative DOrg ownership-overlay claim file for DOLT-DIR.
`<store>/.dorg/server.claim' — co-located with the Dolt repo so every
client computes the identical path regardless of TMPDIR/environment."
  (expand-file-name "server.claim" (dorg-dolt-server--store-dorg-dir dolt-dir)))

(defun dorg-dolt-server--index-file (dolt-dir)
  "Return the discovery-pointer filename for DOLT-DIR."
  (expand-file-name (concat (dorg-dolt-server--claim-key dolt-dir) ".store")
                    dorg-dolt-server-index-dir))

(defun dorg-dolt-server--index-register (dolt-dir)
  "Atomically register DOLT-DIR as a candidate managed store.
The pointer contains only the canonical path.  Store-relative claims remain
the authority for ownership, status, and lifecycle decisions."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (dir (file-name-as-directory
               (expand-file-name dorg-dolt-server-index-dir)))
         (file (dorg-dolt-server--index-file canon)))
    (make-directory dir t)
    (ignore-errors (set-file-modes dir #o700))
    (dorg-sexp-write-file-atomic file canon)))

(defun dorg-dolt-server--index-unregister (dolt-dir)
  "Remove DOLT-DIR's non-authoritative discovery pointer, if present."
  (let ((file (dorg-dolt-server--index-file dolt-dir)))
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(defun dorg-dolt-server--indexed-store-dirs ()
  "Return canonical store paths from the stable discovery index.
Malformed pointers and paths that no longer name directories are ignored.
This is a pure reader and never prunes or rewrites the index."
  (let ((dir (file-name-as-directory
              (expand-file-name dorg-dolt-server-index-dir)))
        stores)
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.store\\'" t))
        (let* ((form (dorg-sexp-read-file file))
               (path (and (stringp form) form)))
          (when (and path (file-directory-p path))
            (let ((canon (ignore-errors
                           (dorg-dolt-server--canonical-dir path))))
              (when canon (push canon stores)))))))
    (delete-dups stores)))

(defun dorg-dolt-server--make-token ()
  "Return a fresh random owner token string."
  (md5 (format "%s:%s:%s:%s"
               (system-name) (emacs-pid) (float-time) (random most-positive-fixnum))))

(defun dorg-dolt-server--read-claim-form (file)
  "Return the claim alist stored in FILE, or nil when absent/unreadable.
A malformed claim file is treated as no claim so a corrupt runtime file
never wedges transport selection or the server listing."
  (let ((form (dorg-sexp-read-file file)))
    (and (consp form) form)))

(defun dorg-dolt-server--claim-read (dolt-dir)
  "Return the claim alist for DOLT-DIR, or nil when absent/unreadable."
  (dorg-dolt-server--read-claim-form (dorg-dolt-server--claim-file dolt-dir)))

;;; ─── Store evidence: argv, then kernel ─────────────────────────────────
;;
;; A claim (overlay or dolt-native `sql-server.info') is bytes on disk, so a
;; copy made from another store names that store's pid and port.  Before any
;; endpoint contact, `dorg-dolt-server--claim-store-verdict' asks what the live
;; process demonstrably serves.  An explicit --data-dir in its argv answers
;; directly.  When argv is silent (a raw `dolt sql-server' serving its working
;; directory) or the recorded pid is dead or replaced while its port is still
;; served, the kernel answers instead: a running server holds an open
;; descriptor on `<store>/.dolt/noms/LOCK' for EACH database it serves, and a
;; byte copy cannot reproduce another process's descriptor.  Store UUID/HEAD
;; cannot distinguish a copy, which is why the evidence is a held descriptor
;; rather than content.  The process asked is always the current owner of the
;; claimed listening port: a claim records a pid but no start time, so a live
;; pid that still looks like Dolt may have been reused by another server.
;;
;; A server serves a SET of databases, so PROVEN means the requested store is
;; in that set and CONTRADICTORY means it is not.  The kernel probes are two
;; separate observations (socket owner, then held files), so a verdict rests
;; on one source-tagged process identity, re-validated after the last
;; observation.  GNU/Linux uses pid plus boot id plus /proc start ticks plus
;; argv; other platforms retain the existing pid plus Emacs start time plus
;; argv source.  Every probe is scoped to one pid or one port and nothing
;; dials the port.
;;
;; Compatibility choice: a probe that is missing, refused, fails, or answers
;; anything unparseable, ambiguous, empty, or torn leaves the evidence ABSENT,
;; which is exactly the behavior without these probes.  They may only ADD
;; refusals they prove, never turn a failed observation into a refusal.  The
;; residual is stated rather than hidden: a server that holds no lock yet, a
;; host without a working probe, and a platform whose process start time is
;; not stable between reads all stay absent.  Only attach-time classification
;; runs this; the validated-attachment cache on the query path never does.

(defun dorg-dolt-server--attrs-dolt-sql-server-p (attrs)
  "Return non-nil when process ATTRS identify a `dolt sql-server'."
  (let ((comm (cdr (assq 'comm attrs)))
        (args (cdr (assq 'args attrs))))
    (and (stringp args)
         ;; Darwin reports the interpreter in comm for a locally named
         ;; executable script, but preserves its invoked pathname in args.
         ;; Accept either OS representation of the executable name.
         (or (and (stringp comm)
                  (equal (file-name-nondirectory comm) "dolt"))
             (let ((argv0 (car (split-string args "[ \t]+" t))))
               (and argv0
                    (equal (file-name-nondirectory argv0) "dolt"))))
         (string-match-p "\\_<sql-server\\_>" args))))

(defun dorg-dolt-server--attrs-data-dir (attrs)
  "Return the canonical explicit --data-dir in process ATTRS' argv, or nil."
  (let* ((args (cdr (assq 'args attrs)))
         (linux-args (eq system-type 'gnu/linux))
         (data-dir-pattern
          (concat "--data-dir\\(?:=\\| \\)"
                  (if linux-args
                      "\\(\\(?:\\\\[[:space:]\\\\]\\|[^[:space:]\\\\]\\)+\\)\\(?: \\|\\'\\)"
                    "\\(.*?\\)\\(?: --\\|\\'\\)"))))
    ;; GNU/Linux escapes whitespace and backslashes in each argv element.
    ;; Darwin's display has raw spaces, so retain its next-option/EOL boundary.
    (when (and (stringp args) (string-match data-dir-pattern args))
      (dorg-dolt-server--canonical-dir
       (if linux-args
           (replace-regexp-in-string
            "\\\\\\([[:space:]\\\\]\\)" "\\1" (match-string 1 args) t)
         (string-trim (match-string 1 args)))))))


(defun dorg-dolt-server--linux-proc-stat-starttime (contents)
  "Return Linux /proc stat field 22 from CONTENTS, or nil when malformed.
The command field is parenthesized and may contain spaces or parens, so split
only after its final closing parenthesis."
  (when (string-match "\\`[0-9]+ (.*) \\(.*\\)\\(?:\n\\)?\\'" contents)
    (let ((value (nth 19 (split-string (match-string 1 contents) " " t))))
      (and value (string-match-p "\\`[0-9]+\\'" value) value))))

(defun dorg-dolt-server--linux-proc-boot-id (contents)
  "Return the canonical Linux boot ID from CONTENTS, or nil when malformed."
  (when (string-match-p
         "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\(?:\n\\)?\\'"
         contents)
    (string-trim-right contents)))

(defun dorg-dolt-server--linux-proc-birth (pid)
  "Return Linux PID's exact (BOOT-ID START-TICKS) birth tuple, or nil.
The proc reads are non-atomic.  A PID reuse between reads therefore produces a
different tuple at recheck and fails safe rather than supporting a refusal."
  (when (eq system-type 'gnu/linux)
    (condition-case nil
        (let ((boot-id
               (with-temp-buffer
                 (insert-file-contents-literally "/proc/sys/kernel/random/boot_id")
                 (dorg-dolt-server--linux-proc-boot-id (buffer-string))))
              (start-ticks
               (with-temp-buffer
                 (insert-file-contents-literally (format "/proc/%d/stat" pid))
                 (dorg-dolt-server--linux-proc-stat-starttime (buffer-string)))))
          (and boot-id start-ticks (list boot-id start-ticks)))
      (file-error nil)
      (error nil))))

(defun dorg-dolt-server--attrs-identity (pid attrs)
  "Return PID's source-tagged identity from process ATTRS, or nil.
On GNU/Linux the source is `linux-proc' and its birth tuple is exact boot ID
and /proc start ticks; there is no Emacs-start fallback.  Other platforms
retain the existing `emacs-start' source.  ARGS tells an exec apart."
  (let ((args (cdr (assq 'args attrs))))
    (if (eq system-type 'gnu/linux)
        (let ((birth (dorg-dolt-server--linux-proc-birth pid)))
          (and birth (list :pid pid :source 'linux-proc :birth birth :args args)))
      (let ((start (cdr (assq 'start attrs))))
        (and start (list :pid pid :source 'emacs-start :birth start :args args))))))

(defun dorg-dolt-server--identity-held-p (identity)
  "Return non-nil when IDENTITY's process is still the one observed.
Re-reads the process table.  Any failure, including a comparison that
signals, answers nil, so a torn observation can never support a refusal."
  (condition-case nil
      (let* ((pid (plist-get identity :pid))
             (source (plist-get identity :source))
             (now (and (integerp pid)
                       (memq source '(linux-proc emacs-start))
                       (dorg-dolt-server--attrs-identity
                        pid (process-attributes pid)))))
        (and now
             (eq source (plist-get now :source))
             (equal (plist-get identity :args) (plist-get now :args))
             (pcase source
               ('linux-proc
                (equal (plist-get identity :birth) (plist-get now :birth)))
               ('emacs-start
                (time-equal-p (plist-get identity :birth)
                              (plist-get now :birth))))))
    (error nil)))

(defun dorg-dolt-server--run-probe (program &rest args)
  "Run PROGRAM with ARGS; return its stdout lines on exit 0, else `unknown'.
The single seam through which kernel-evidence probes run.  Stderr is
discarded.  A missing PROGRAM, a nonzero or signalled exit, and any error
all answer `unknown'."
  (let ((exe (executable-find program)))
    (if (not exe)
        'unknown
      (condition-case nil
          (with-temp-buffer
            (if (eql 0 (apply #'call-process exe nil (list t nil) nil args))
                (split-string (buffer-string) "\n" t)
              'unknown))
        (error 'unknown)))))

(defun dorg-dolt-server--lock-store (path)
  "Return the store directory whose noms LOCK is PATH, or nil.
A lock inside a `.dolt' directory belongs to Dolt's internal databases
\(such as `.dolt/stats'), which are not served stores."
  (when (and (stringp path)
             (string-match "\\`\\(/.+\\)/\\.dolt/noms/LOCK\\'" path))
    (let ((store (match-string 1 path)))
      (unless (string-match-p "/\\.dolt\\(?:/\\|\\'\\)" store)
        store))))

(defun dorg-dolt-server--parse-lsof-files (pid lines)
  "Parse `lsof -Ffn' LINES for PID into (:locks PATHS :cwd DIR), or `unknown'.
The output must describe exactly PID once; any other process record, an
unexpected field, or a second working directory is ambiguous."
  (let (pids fd cwd locks bad)
    (dolist (line lines)
      (let ((value (substring line 1)))
        (pcase (aref line 0)
          (?p (push (string-to-number value) pids))
          (?f (setq fd value))
          (?n (cond ((equal fd "cwd")
                     (if cwd (setq bad t) (setq cwd value)))
                    ((dorg-dolt-server--lock-store value)
                     (push value locks))))
          (_ (setq bad t)))))
    (if (or bad (not (equal pids (list pid))))
        'unknown
      (list :locks (nreverse locks) :cwd cwd))))

(defun dorg-dolt-server--lsof-served-paths (pid)
  "Return PID's held noms LOCK paths and cwd via `lsof -p PID', or `unknown'."
  (let ((lines (dorg-dolt-server--run-probe
                "lsof" "-w" "-n" "-P" "-a" "-p" (number-to-string pid) "-Ffn")))
    (if (eq lines 'unknown)
        'unknown
      (dorg-dolt-server--parse-lsof-files pid lines))))

(defun dorg-dolt-server--proc-served-paths (pid proc-root)
  "Return PID's held noms LOCK paths and cwd read under PROC-ROOT, or `unknown'.
PROC-ROOT is a procfs mount such as \"/proc/\"; only PROC-ROOT/PID is read.
An unreadable descriptor table (another user's process) is `unknown'."
  (condition-case nil
      (let* ((dir (expand-file-name (number-to-string pid) proc-root))
             (fds (directory-files (expand-file-name "fd" dir) t "\\`[0-9]+\\'" t))
             (cwd (file-symlink-p (expand-file-name "cwd" dir)))
             locks)
        (dolist (fd fds)
          (let ((target (file-symlink-p fd)))
            (when (dorg-dolt-server--lock-store target)
              (push target locks))))
        (list :locks (nreverse locks) :cwd (and (stringp cwd) cwd)))
    (error 'unknown)))

(defun dorg-dolt-server--probe-served-paths (pid)
  "Return the noms LOCK paths PID holds open and its cwd, or `unknown'.
Answers (:locks PATHS :cwd DIR).  Scoped to PID: procfs where the platform
has it, else `lsof -p PID'."
  (if (file-directory-p "/proc/self/fd")
      (dorg-dolt-server--proc-served-paths pid "/proc/")
    (dorg-dolt-server--lsof-served-paths pid)))

(defun dorg-dolt-server--probe-socket-owner (port)
  "Return the one pid listening on TCP PORT, else `unknown'.
Scoped to PORT and read from the kernel socket table; nothing connects to
PORT.  lsof exits nonzero both when nothing listens and when it cannot see
the listener, so neither is distinguishable from failure; both answer
`unknown', as do several listening processes."
  (let ((lines (dorg-dolt-server--run-probe
                "lsof" "-w" "-n" "-P" (format "-iTCP:%d" port)
                "-sTCP:LISTEN" "-Fp")))
    (if (eq lines 'unknown)
        'unknown
      (let (pids bad)
        (dolist (line lines)
          (pcase (aref line 0)
            (?p (let ((pid (string-to-number (substring line 1))))
                  (unless (memql pid pids) (push pid pids))))
            (?f nil)
            (_ (setq bad t))))
        (if (and (not bad) (= (length pids) 1) (> (car pids) 0))
            (car pids)
          'unknown)))))

(defun dorg-dolt-server--parse-lsof-lock-holders (lock lines)
  "Return the PIDs in LINES that hold canonical LOCK, or `unknown'.
LINES are the `-Fpn' output from a file-scoped `lsof' query.  A PID record
must precede every matching name; any malformed field makes the observation
indeterminate rather than accidentally authorizing a launch."
  (let (pid holders bad)
    (dolist (line lines)
      (if (or (not (stringp line)) (zerop (length line)))
          (setq bad t)
        (pcase (aref line 0)
          (?p (let ((value (substring line 1)))
                (if (string-match-p "\\`[1-9][0-9]*\\'" value)
                    (setq pid (string-to-number value))
                  (setq bad t))))
          (?f nil)
          (?n (when (equal (substring line 1) lock)
                (if pid
                    (push pid holders)
                  (setq bad t))))
          (_ (setq bad t)))))
    (if bad 'unknown (delete-dups (nreverse holders)))))

(defconst dorg-dolt-server--admission-lsof-script
  "PATH=/usr/sbin:/usr/bin:/bin:$PATH\noutput=$(lsof -w -n -P -Fpn \"$1\" 2>&1); status=$?\nif [ \"$status\" -eq 0 ]; then\n  printf '%s\\n' \"$output\"\nelif [ \"$status\" -eq 1 ] && [ -z \"$output\" ]; then\n  exit 0\nelse\n  printf '%s\\n' \"$output\" >&2\n  exit \"$status\"\nfi"
  "Run file-scoped lsof while preserving a distinguishable empty result.

macOS lsof exits one for an empty file selection.  The adapter maps only its
silent no-selection result to zero; missing lsof and every diagnostic failure
remain nonzero so `dorg-dolt-server--run-probe' returns `unknown'.  The
standard system lsof directory is retained on scrubbed PATHs; LOCK is passed
as $1, never interpolated into the shell source.")

(defun dorg-dolt-server--admission-lock-holders (dolt-dir)
  "Return proven sql-server PIDs holding DOLT-DIR's noms LOCK, or `unknown'.
This is the strict, pre-spawn census only.  It deliberately does not alter
the permissive claim-verdict layer used by attachment and stop callers.

`lsof' normally exits one when a named file has no open holders.  Its fixed
shell adapter runs through the same probe seam and maps only that silent
no-selection case to usable empty output.  Parsing considers only the exact
canonical LOCK path.  Each lock hit must retain one stable process identity
and prove either its explicit canonical `--data-dir' or its cwd-served store
before it is an admission holder."
  (let* ((target (dorg-dolt--target dolt-dir))
         (store (plist-get target :canonical-repo-dir))
         (lock (expand-file-name ".dolt/noms/LOCK" store))
         ;; A freshly initialized store may have no noms lock until Dolt first
         ;; opens it.  That absence is conclusively clear, unlike a failed
         ;; probe of an existing lock (or a dangling lock symlink), which stays
         ;; indeterminate and blocks managed start.
         (missing-lock (and (not (file-exists-p lock))
                            (not (file-symlink-p lock)))))
    (if missing-lock
        nil
      (let ((lines (dorg-dolt-server--run-probe
                    "sh" "-c" dorg-dolt-server--admission-lsof-script
                    "dorg-admission-lsof" lock)))
        (if (eq lines 'unknown)
            'unknown
          (let ((pids (dorg-dolt-server--parse-lsof-lock-holders lock lines))
                holders)
            (if (eq pids 'unknown)
                'unknown
              (condition-case nil
                  (catch 'indeterminate
                    (dolist (pid pids)
                      (let* ((attrs (process-attributes pid))
                             (identity (and attrs
                                            (dorg-dolt-server--attrs-identity pid attrs)))
                             (data-dir (and attrs
                                            (dorg-dolt-server--attrs-data-dir attrs))))
                        (unless (and identity
                                     (dorg-dolt-server--attrs-dolt-sql-server-p attrs))
                          (throw 'indeterminate 'unknown))
                        (if data-dir
                            (unless (equal data-dir store)
                              (throw 'indeterminate 'unknown))
                          (let ((paths (dorg-dolt-server--probe-served-paths pid)))
                            (unless (and (not (eq paths 'unknown))
                                         (let ((served (dorg-dolt-server--served-set paths t)))
                                           (and served (member store (cdr served)))))
                              (throw 'indeterminate 'unknown))))
                        (unless (dorg-dolt-server--identity-held-p identity)
                          (throw 'indeterminate 'unknown))
                        (push pid holders)))
                    (nreverse holders))
                (error 'unknown)))))))))

(defun dorg-dolt-server--assert-admission-clear (dolt-dir)
  "Refuse a managed start unless DOLT-DIR's owner census is conclusively clear.
Call only while `dorg-dolt-server--with-claim-lock' is held and before any
child spawn, claim publication, or info rewrite."
  (pcase (dorg-dolt-server--admission-lock-holders dolt-dir)
    ('unknown
     (signal 'dorg-sql-transport-error
             (list (format (concat "dorg: owner census for %s is indeterminate; "
                                   "refusing managed sql-server start")
                           (dorg-dolt-server--canonical-dir dolt-dir)))))
    (`() t)
    (holders
     (signal 'dorg-sql-transport-conflict
             (list (format (concat "dorg: refusing managed sql-server start for %s; "
                                   "live owner census holds .dolt/noms/LOCK (pid%s %s)")
                           (dorg-dolt-server--canonical-dir dolt-dir)
                           (if (= (length holders) 1) "" "s")
                           (mapconcat #'number-to-string holders ", ")))))))

(defun dorg-dolt-server--served-set (paths cwd-allowed)
  "Return (BASIS . STORES): the canonical stores PATHS demonstrate, or nil.
Held noms locks come first, BASIS `locks'.  Only when no lock names a store
and CWD-ALLOWED (argv has no explicit --data-dir) does the working directory
answer, BASIS `cwd': a server without --data-dir serves its cwd if that is a
Dolt repo, and each immediate subdirectory that is one."
  (let (stores)
    (dolist (lock (plist-get paths :locks))
      (let ((store (dorg-dolt-server--lock-store lock)))
        (when store
          (push (dorg-dolt-server--canonical-dir store) stores))))
    (if stores
        (cons 'locks (delete-dups (nreverse stores)))
      (let ((cwd (plist-get paths :cwd)))
        (when (and cwd-allowed (stringp cwd) (file-directory-p cwd))
          (dolist (dir (cons cwd (directory-files
                                  cwd t directory-files-no-dot-files-regexp t)))
            (when (file-directory-p (expand-file-name ".dolt" dir))
              (push (dorg-dolt-server--canonical-dir dir) stores)))
          (when stores
            (cons 'cwd (delete-dups (nreverse stores)))))))))

(defun dorg-dolt-server--kernel-verdict (expected pid attrs source port recheck-port)
  "Classify EXPECTED against what PID demonstrably serves, from the kernel.
ATTRS is PID's already-observed process attributes, whose identity must
still hold after the files probe.  With RECHECK-PORT, PID must also still
own that listening socket.  SOURCE and PORT label the verdict.  Anything
short of a coherent, non-empty served set is `absent'."
  (let* ((identity (dorg-dolt-server--attrs-identity pid attrs))
         (paths (if identity
                    (dorg-dolt-server--probe-served-paths pid)
                  'unknown))
         (served (and (not (eq paths 'unknown))
                      (dorg-dolt-server--identity-held-p identity)
                      (or (null recheck-port)
                          (eql pid (dorg-dolt-server--probe-socket-owner
                                    recheck-port)))
                      (dorg-dolt-server--served-set
                       paths (null (dorg-dolt-server--attrs-data-dir attrs))))))
    (list :evidence (cond ((null served) 'absent)
                          ((member expected (cdr served)) 'proven)
                          (t 'contradictory))
          :source source :basis (car served) :served (cdr served)
          :pid pid :port port)))

(defun dorg-dolt-server--claim-store-verdict (dolt-dir claim)
  "Classify CLAIM's process evidence for DOLT-DIR without contacting its port.
Return a plist whose `:evidence' is `proven', `contradictory' or `absent',
with `:source' (`argv', `recorded-pid' or `socket-owner'), `:basis'
\(`locks' or `cwd' for kernel evidence), `:served', `:pid' and `:port'
describing what decided it.

- A live `dolt sql-server' at the recorded pid with an explicit --data-dir
  answers from argv: `proven' when it is DOLT-DIR, else `contradictory'.
- Otherwise the process that currently owns the claim's listening port
  answers from the stores it holds, when that is a `dolt sql-server'.  This
  covers a raw server without --data-dir, a dead recorded pid, and a
  recorded pid reused by any other process, including another raw Dolt
  server: a pid alone carries no start time in the claim, so only the
  port's current owner is bound to the endpoint.  `:source' is
  `recorded-pid' when that owner is the recorded pid, else `socket-owner'.
- Everything else, including no or malformed pid, no port, an unresolved
  port owner and every probe failure, is `absent'.

Absent keeps the established claim lifecycle (including dead-claim
recovery) intact; contradictory is the only safe basis for refusing a
copied claim before any endpoint contact."
  (let ((pid (alist-get :server-pid claim))
        (port (alist-get :port claim))
        (expected (dorg-dolt-server--canonical-dir dolt-dir)))
    (or (and (integerp pid) (> pid 0)
             (condition-case nil
                 (let* ((attrs (process-attributes pid))
                        (data-dir (and (dorg-dolt-server--attrs-dolt-sql-server-p attrs)
                                       (dorg-dolt-server--attrs-data-dir attrs))))
                   (if data-dir
                       (list :evidence (if (equal expected data-dir)
                                           'proven
                                         'contradictory)
                             :source 'argv :served (list data-dir)
                             :pid pid :port port)
                     ;; Argv is silent, the pid is dead, or it runs something
                     ;; else: a live pid that merely looks like Dolt may have
                     ;; been reused, so only the current owner of the claimed
                     ;; port can say what the endpoint serves.
                     (let* ((owner (and (integerp port)
                                        (dorg-dolt-server--probe-socket-owner port)))
                            (owner-attrs (cond ((not (integerp owner)) nil)
                                               ((eql owner pid) attrs)
                                               (t (process-attributes owner)))))
                       (and (dorg-dolt-server--attrs-dolt-sql-server-p owner-attrs)
                            (dorg-dolt-server--kernel-verdict
                             expected owner owner-attrs
                             (if (eql owner pid) 'recorded-pid 'socket-owner)
                             port port)))))
               (error nil)))
        (list :evidence 'absent :pid pid :port port))))

(defun dorg-dolt-server--claim-store-evidence (dolt-dir claim)
  "Return CLAIM's store evidence for DOLT-DIR.
The answer is `proven', `contradictory' or `absent'; see
`dorg-dolt-server--claim-store-verdict', which also says what decided it."
  (plist-get (dorg-dolt-server--claim-store-verdict dolt-dir claim) :evidence))

(defun dorg-dolt-server--foreign-store-message (dolt-dir verdict)
  "Return the refusal diagnostic for DOLT-DIR's contradictory VERDICT.
It names the requested store, what the live process demonstrably serves,
its pid and port, the deciding evidence, and a manual recovery route.
Recovery is manual by design: DOrg deletes, signals and repairs nothing."
  (let ((store (dorg-dolt-server--canonical-dir dolt-dir))
        (pid (plist-get verdict :pid)))
    (format "dorg: refusing server claim for %s: the live dolt sql-server (pid %s, port %s) demonstrably serves %s, not this store; evidence: %s.  Nothing was deleted, signalled or repaired.  Manual recovery: confirm what pid %s serves (for example with lsof -p %s); if %s or %s was copied from another store, remove the copied file by hand, then retry"
            store pid (plist-get verdict :port)
            (mapconcat #'identity (plist-get verdict :served) ", ")
            (pcase (list (plist-get verdict :source) (plist-get verdict :basis))
              (`(argv ,_) "the explicit --data-dir in its argv")
              (`(recorded-pid locks) "store locks held open by the claim's recorded pid, which owns the claimed port")
              (`(recorded-pid cwd) "the working directory of the claim's recorded pid, which owns the claimed port and has no --data-dir")
              (`(socket-owner locks) "store locks held open by the process listening on the claim's port; the recorded pid is dead or replaced")
              (`(socket-owner cwd) "the working directory of the process listening on the claim's port; the recorded pid is dead or replaced")
              (_ "process evidence"))
            pid pid
            (expand-file-name ".dolt/sql-server.info" store)
            (dorg-dolt-server--claim-file store))))

(defun dorg-dolt-server--assert-claim-not-foreign-store (dolt-dir claim)
  "Refuse a CLAIM whose live process conclusively serves another store.
This is intentionally an attach-selector guard, rather than part of the
total discovery reader.  It must run before claim classification, which may
contact CLAIM's recorded port."
  (let ((verdict (dorg-dolt-server--claim-store-verdict dolt-dir claim)))
    (when (eq (plist-get verdict :evidence) 'contradictory)
      (signal 'dorg-sql-transport-error
              (list (dorg-dolt-server--foreign-store-message dolt-dir verdict)))))
  claim)

(defun dorg-dolt-server--claim-write (dolt-dir claim)
  "Atomically write CLAIM (an alist) as DOLT-DIR's claim file.
Writes to a temp file in the runtime dir and renames it into place so a
concurrent reader never sees a half-written claim."
  (let ((file (dorg-dolt-server--claim-file dolt-dir)))
    (dorg-dolt-server--ensure-dorg-dir dolt-dir)
    (dorg-sexp-write-file-atomic file claim)
    (dorg-dolt-server--index-register dolt-dir)))

(defun dorg-dolt-server--claim-delete-file (dolt-dir)
  "Delete DOLT-DIR's claim file if present (no token check)."
  (let ((file (dorg-dolt-server--claim-file dolt-dir)))
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))
    (unless (file-exists-p file)
      (dorg-dolt-server--index-unregister dolt-dir))))

;;; ─── Dolt-native discovery (sql-server.info) ────────────────────────────
;;
;; A running `dolt sql-server' writes `<store>/.dolt/sql-server.info' — a
;; store-relative `PID:PORT:UUID' record that raw `dolt sql' clients already
;; consult.  Because it is store-relative it is TMPDIR/environment-independent,
;; so DOrg uses it as the PRIMARY cross-environment discovery anchor: one
;; server per store, shared by DOrg AND raw dolt clients.  The DOrg overlay
;; (`<store>/.dorg/server.claim') layers ownership/lifecycle on top; a server
;; seen ONLY via dolt-info (no overlay) is a foreign/raw server that DOrg
;; attaches to but never owns or reaps.

(defun dorg-dolt-server--dolt-server-info (dolt-dir)
  "Return dolt's native sql-server record for DOLT-DIR, or nil.
Reads `<store>/.dolt/sql-server.info' (format `PID:PORT:UUID', a single
line dolt writes while a server runs and removes on clean shutdown) and
returns a plist (:pid INT :port INT :uuid STR), or nil when the file is
absent or malformed.  Liveness is NOT implied — a crashed server leaves a
stale file, so callers must still probe the port."
  (let ((file (expand-file-name ".dolt/sql-server.info"
                                (dorg-dolt-server--canonical-dir dolt-dir))))
    (when (file-readable-p file)
      (ignore-errors
        (let* ((raw (with-temp-buffer
                      (insert-file-contents file)
                      (string-trim (buffer-string))))
               (parts (split-string raw ":")))
          (when (= (length parts) 3)
            (let ((pid (string-to-number (nth 0 parts)))
                  (port (string-to-number (nth 1 parts)))
                  (uuid (nth 2 parts)))
              (when (and (> pid 0) (> port 0) (not (string-empty-p uuid)))
                (list :pid pid :port port :uuid uuid)))))))))

(defun dorg-dolt-server--dolt-uuid-current-p (dolt-dir uuid)
  "Return non-nil if DOLT-DIR's current sql-server.info UUID equals UUID.
The UUID identifies a specific sql-server instance, so re-reading it just
before attaching detects a server that was replaced (PID/port reused)
between discovery and attach — the dolt-native auth check for a
foreign/native server."
  (let ((info (dorg-dolt-server--dolt-server-info dolt-dir)))
    (and info (equal (plist-get info :uuid) uuid))))

(defun dorg-dolt-server--synthesize-dolt-claim (dolt-dir)
  "Return a foreign claim synthesized from DOLT-DIR's dolt-native info, or nil.
When a `dolt sql-server' runs (per `dorg-dolt-server--dolt-server-info') but no
DOrg overlay exists, present it as a foreign, protocol-compatible,
same-host claim so the existing classify/attach machinery treats it as a
live foreign server: DOrg attaches (never starts a second) but, because
the claim carries no owner token and `:owner-kind' `dolt', never owns or
reaps it.  `:uuid' carries the dolt-native instance id for auth."
  (let ((info (dorg-dolt-server--dolt-server-info dolt-dir)))
    (when info
      (let ((now (float-time))
            (pid (plist-get info :pid)))
        (list (cons :dolt-dir (dorg-dolt-server--canonical-dir dolt-dir))
              (cons :host (system-name))
              (cons :owner-pid pid)
              (cons :owner-kind 'dolt)
              (cons :server-pid pid)
              (cons :port (plist-get info :port))
              (cons :uuid (plist-get info :uuid))
              (cons :policy 'server)
              (cons :protocol dorg-dolt-server-claim-protocol)
              (cons :origin 'dolt-native)
              (cons :created-at now)
              (cons :last-used now))))))

(defun dorg-dolt-server--effective-claim (dolt-dir)
  "Return the claim governing DOLT-DIR discovery, reconciled to ONE server.
Dolt's native `<store>/.dolt/sql-server.info' is the AUTHORITATIVE liveness
anchor; the store-relative DOrg overlay adds ownership.  Reconciliation
\(checked in order):
- a `:state probe-inconclusive' overlay => the pending DOrg owner.  Pending
  ownership is authoritative even when native metadata names a live different
  port: peers must see and block on the unfinalized owner until it is finalized
  or confirmed dead;
- overlay and native agree on `:port' => the DOrg-managed server; the
  overlay wins (it carries owner-kind/token/timestamps);
- a LIVE native server whose port a present overlay does NOT match (or with
  no overlay at all) => the native server.  A STALE overlay (e.g. a dead
  daemon recorded on a now-down port) must NEVER mask a live native server:
  returning the overlay would classify the store `dead' and wrongly start a
  second server over dolt's one-per-db lock;
- otherwise the overlay (native absent, or its port not live);
- otherwise a foreign claim synthesized from a stale-but-present native
  record.
Reconciling to a single claim also ensures a store with both an overlay and
native info is counted once."
  (let* ((overlay (dorg-dolt-server--claim-read dolt-dir))
         (native (dorg-dolt-server--dolt-server-info dolt-dir))
         (native-claim (and native
                            (dorg-dolt-server--synthesize-dolt-claim dolt-dir)))
         ;; Preserve a contradictory candidate without probing its port.  The
         ;; attach selector will make the terminal refusal; this reader stays
         ;; total so lifecycle recovery and diagnostics can still inspect dead
         ;; or otherwise unproven claims.
         (overlay-evidence (and overlay
                                (dorg-dolt-server--claim-store-evidence
                                 dolt-dir overlay)))
         (native-evidence (and native-claim
                               (dorg-dolt-server--claim-store-evidence
                                dolt-dir native-claim))))
    (cond
     ((eq overlay-evidence 'contradictory) overlay)
     ((eq native-evidence 'contradictory) native-claim)
     ((eq (alist-get :state overlay) 'probe-inconclusive)
      overlay)
     ((and overlay native
           (eql (alist-get :port overlay) (plist-get native :port)))
      overlay)
     ((and native-claim
           (dorg-dolt-server--port-live-p (plist-get native :port)))
      native-claim)
     (overlay overlay)
     (native-claim native-claim))))

(defcustom dorg-dolt-server-claim-lock-timeout 10.0
  "Seconds to wait to acquire the claim lock before attempting a stale reap.
A live holder that still owns the lock after this bound causes the
acquirer to signal `dorg-sql-transport-error' rather than run the
critical section unlocked."
  :type 'number
  :group 'dorg)

(defcustom dorg-dolt-server-claim-lock-stale-after 60.0
  "Seconds after which a held claim lock is treated as abandoned.
A lock whose same-host holder pid is enumerably gone is reaped
immediately; this age backstop additionally reaps a lock left by a
crashed holder whose pid cannot be proven dead (remote/unenumerable).
It must exceed the worst-case critical section (server startup) so a
slow-but-live holder is never reaped out from under itself."
  :type 'number
  :group 'dorg)

(defun dorg-dolt-server--lock-meta-file (lock)
  "Return the owner-metadata file path inside LOCK directory."
  (expand-file-name "owner" lock))

(defun dorg-dolt-server--lock-write-meta (lock)
  "Record this process (host/pid/time) as the holder of LOCK."
  (ignore-errors
    (dorg-sexp-write-file-atomic
     (dorg-dolt-server--lock-meta-file lock)
     (list (cons :host (system-name))
           (cons :pid (emacs-pid))
           (cons :acquired-at (float-time))))))

(defun dorg-dolt-server--lock-read-meta (lock)
  "Return LOCK's holder metadata alist, or nil when absent/unreadable."
  (let ((form (dorg-sexp-read-file (dorg-dolt-server--lock-meta-file lock))))
    (and (consp form) form)))

(defun dorg-dolt-server--lock-stale-p (lock)
  "Return non-nil when LOCK appears abandoned by a crashed holder.
Stale when its same-host holder pid is enumerably gone, or when the
lock (by holder timestamp, else directory mtime) is older than
`dorg-dolt-server-claim-lock-stale-after'.  A live or unprovable-but-recent
holder is never declared stale, so a live critical section is never
broken."
  (let* ((meta (dorg-dolt-server--lock-read-meta lock))
         (host (alist-get :host meta))
         (pid (alist-get :pid meta))
         (acquired (alist-get :acquired-at meta))
         (mtime (let ((attrs (file-attributes lock)))
                  (and attrs (float-time
                              (file-attribute-modification-time attrs)))))
         (age-base (or acquired mtime)))
    (cond
     ;; Same-host holder whose pid is enumerably absent: definitely crashed.
     ((and (equal host (system-name)) (integerp pid)
           (eq (dorg-dolt-server--pid-live-p pid) nil))
      t)
     ;; Age backstop for a holder whose liveness cannot be proven.
     ((and (numberp age-base)
           (> (- (float-time) age-base) dorg-dolt-server-claim-lock-stale-after))
      t)
     (t nil))))

(defun dorg-dolt-server--acquire-claim-lock (lock)
  "Acquire LOCK, an atomic `make-directory' mutex, or signal.
Spins up to `dorg-dolt-server-claim-lock-timeout' for an atomic acquisition.
If the lock is still held when the wait expires, reaps it only when
`dorg-dolt-server--lock-stale-p' proves the holder crashed, then retries
once.  Signals `dorg-sql-transport-error' when a live holder still owns
the lock — the caller must never run its critical section unlocked.
Returns non-nil on success and records this process as the holder."
  (make-directory (file-name-directory (directory-file-name lock)) t)
  (let ((deadline (+ (float-time) dorg-dolt-server-claim-lock-timeout))
        (acquired nil))
    (while (and (not acquired) (< (float-time) deadline))
      (condition-case nil
          (progn (make-directory lock) (setq acquired t))
        (file-already-exists (sleep-for 0.05))))
    (when (and (not acquired) (dorg-dolt-server--lock-stale-p lock))
      ;; Crashed holder: break the abandoned lock and try once more.
      (ignore-errors (delete-directory lock t))
      (condition-case nil
          (progn (make-directory lock) (setq acquired t))
        (file-already-exists nil)))   ; a competitor won the reaped lock
    (if acquired
        (progn (dorg-dolt-server--lock-write-meta lock) t)
      (signal 'dorg-sql-transport-error
              (list (format "dorg: could not acquire claim lock %s within %ss; a live DOrg process holds it — refusing to run claim selection unlocked"
                            lock dorg-dolt-server-claim-lock-timeout))))))

(defun dorg-dolt-server--release-claim-lock (lock)
  "Release LOCK if this process still holds it (or its meta is gone).
Token-checked by holder pid so a lock reaped and re-taken by another
process after a long critical section is not deleted out from under its
new holder."
  (let ((meta (dorg-dolt-server--lock-read-meta lock)))
    (when (or (null meta)
              (and (equal (alist-get :host meta) (system-name))
                   (eql (alist-get :pid meta) (emacs-pid))))
      (ignore-errors (delete-directory lock t)))))

(defmacro dorg-dolt-server--with-claim-lock (dolt-dir &rest body)
  "Run BODY holding an exclusive claim lock for DOLT-DIR.
Serializes the read-decide-start/break sequence across processes using
an atomic `make-directory' mutex.  BODY never runs without the lock: if
the lock cannot be acquired within `dorg-dolt-server-claim-lock-timeout', a
crashed holder's lock is reaped under the same primitive, and a still
live holder causes `dorg-dolt-server--acquire-claim-lock' to signal instead
of running BODY.  The lock is always released."
  (declare (indent 1))
  (let ((lock (make-symbol "lock")))
    `(let ((,lock (concat (dorg-dolt-server--claim-file ,dolt-dir) ".lock")))
       (dorg-dolt-server--acquire-claim-lock ,lock)
       (unwind-protect
           (progn ,@body)
         (dorg-dolt-server--release-claim-lock ,lock)))))

;;; ─── Claim Liveness ─────────────────────────────────────────────────────

(defun dorg-dolt-server--pid-live-p (pid)
  "Return non-nil if PID is a live process on this host.
Returns nil only when the platform can enumerate processes and PID is
absent; returns `unknown' when enumeration is unsupported."
  (let ((all (list-system-processes)))
    (cond ((null all) 'unknown)
          ((memq pid all) t)
          (t nil))))

(defun dorg-dolt-server--port-live-p (port)
  "Return non-nil if a TCP server is accepting connections on PORT."
  (and (integerp port)
       (condition-case nil
           (let ((probe (make-network-process
                         :name "dorg-claim-probe"
                         :host "127.0.0.1"
                         :service port
                         :family 'ipv4
                         :nowait nil)))
             (delete-process probe)
             t)
         (error nil))))

(defun dorg-dolt-server--port-healthy-p (port &optional timeout)
  "Return non-nil if a sql-server on PORT answers a trivial SQL round-trip.
TCP accept alone (`dorg-dolt-server--port-live-p') cannot distinguish a healthy
server from a wedged one that owns the listen socket but never speaks the
MySQL protocol.  This runs a bounded `SELECT 1' over a short-lived
mysql.el connection (see `dorg-sql--probe-healthy-p') and returns t only
on a real answer within TIMEOUT (default `dorg-dolt-server-health-timeout'),
nil on any timeout/refusal/error — it never raises."
  (when (integerp port)
    (dorg-sql--probe-healthy-p port (or timeout dorg-dolt-server-health-timeout))))

(defun dorg-dolt-server--claim-host-local-p (claim)
  "Return non-nil when CLAIM was written by a process on this host."
  (equal (alist-get :host claim) (system-name)))

(defun dorg-dolt-server--claim-classify (claim &optional deep)
  "Classify CLAIM as `live', `pending', `wedged', `dead', `unreachable'.
- `pending': an unfinalized post-start owner (`:state probe-inconclusive')
  whose served DB is unconfirmed — a BLOCKING verdict (no peer attach/replace)
  until a later positive re-probe finalizes it or it is confirmed dead;
- `live': the claimed sql-server port answers (attachable);
- `wedged': the port accepts TCP but fails the bounded SQL health probe —
  only ever returned when DEEP is non-nil;
- `dead': a same-host owner pid is definitively gone and the port is down;
- `unreachable': the owner may be alive (or is remote/unknowable) but its
  port does not answer.
A live port always wins, so a server that outlived a pid check still
attaches; a `dead' verdict requires a same-host, enumerable, absent pid
so a remote or unenumerable owner is never declared dead by mistake.

DEEP controls how a TCP-accepting port is judged.  Shallow (the default,
used on the hot path) trusts the TCP accept and returns `live'.  DEEP
additionally runs `dorg-dolt-server--port-healthy-p'; a port that accepts TCP
but fails the SQL probe classifies `wedged' instead of `live', so an
attach decision never commits to a server that never answers SQL.  Deep
classify pays for one bounded round-trip and is reserved for attach/commit
decision points, not per-query routing."
  (let* ((host-local (dorg-dolt-server--claim-host-local-p claim))
         (pid (alist-get :owner-pid claim))
         (port (alist-get :port claim))
         (pid-live (if (and host-local (integerp pid))
                       (dorg-dolt-server--pid-live-p pid) ; t / nil / `unknown'
                     'skip)))
    (cond
     ;; A post-start UNFINALIZED daemon/child (`:state probe-inconclusive',
     ;; tracked-regression): its served DB could not be confirmed, so it must not
     ;; be attached-as-ready or replaced by any peer.  It classifies `pending'
     ;; \(a BLOCKING verdict) while its owner is not confirmed dead, and `dead'
     ;; ONLY once the port is down AND a same-host owner pid is enumerably gone
     ;; — so bounded cleanup of a pending claim is DEATH-GATED.  A `:state
     ;; ready' claim on a live port is unaffected (it stays `live'), keeping the
     ;; pending routing state-specific rather than a blanket override.
     ((eq (alist-get :state claim) 'probe-inconclusive)
      ;; DEATH-GATED on the ACTUAL server/child pid (`:server-pid'), NOT the
      ;; owning process (`:owner-pid'): a session child is owned by its parent
      ;; Emacs (`:owner-pid' = that Emacs), so a child that dies while its owner
      ;; Emacs lives must classify `dead' — otherwise the pending claim strands
      ;; forever and no peer can ever replace it (tracked-regression nav-vx1 BF2).
      ;; For a detached daemon `:owner-pid' == `:server-pid', so keying the
      ;; death gate on `:server-pid' is a strict generalization.
      (let* ((server-pid (alist-get :server-pid claim))
             (server-live (if (and host-local (integerp server-pid))
                              (dorg-dolt-server--pid-live-p server-pid)
                            'skip)))
        (cond
         ((dorg-dolt-server--port-live-p port) 'pending)
         ((eq server-live nil) 'dead)     ; confirmed death: port down + server pid gone
         (t 'pending))))                  ; not confirmed dead => still blocking
     ((dorg-dolt-server--port-live-p port)
      (if (and deep (not (dorg-dolt-server--port-healthy-p port)))
          'wedged
        'live))
     ((eq pid-live nil) 'dead)            ; same-host, enumerated, pid absent
     (t 'unreachable))))                  ; pid live/unknown/remote, port down

(defun dorg-dolt-server--claim-compatible-p (claim)
  "Return non-nil if this process can attach to CLAIM's sql-server.
Requires a matching claim protocol.  mysql.el loadability is not re-checked
here: it is a HARD dependency loaded at `dorg-dolt' load time
\(`dorg-sql--require-mysql-hard'), so any running process already has it."
  (equal (alist-get :protocol claim) dorg-dolt-server-claim-protocol))

(defun dorg-dolt-server--claim-owned-here-p (claim)
  "Return non-nil if CLAIM's token matches one this process owns."
  (let ((token (gethash (alist-get :dolt-dir claim) dorg-dolt-server--owned-claims)))
    (and token (equal token (alist-get :token claim)))))

;;; ─── Claim Publication / Release ────────────────────────────────────────

(defun dorg-dolt-server--publish-claim (dolt-dir port &optional server-pid data-dir db-name owner-pid owner-kind uuid state)
  "Write or refresh this process's ownership claim for DOLT-DIR on PORT.
SERVER-PID, when given, records the sql-server process pid.  DATA-DIR and
DB-NAME record the normalized `dolt sql-server' target (the explicit
`--data-dir' and the served database name) so downstream readers can see
exactly which store this claim covers; when omitted
they are derived from DOLT-DIR via `dorg-dolt-server--data-dir-and-db' so a
plain refresh keeps the fields populated.

OWNER-PID and OWNER-KIND describe the claim's owner.  OWNER-KIND defaults
to `emacs' (an Emacs-process-owned server, reaped on that Emacs's exit)
and OWNER-PID to this Emacs's pid.  A detached daemon (see
`dorg-dolt-server--start-detached-unsafe') passes OWNER-KIND `daemon' and an
OWNER-PID equal to the daemon's own pid, so cross-process
`dorg-dolt-server--claim-classify' tracks the daemon — not a long-dead
launcher — when deciding `live'/`dead'.

UUID, when non-nil, records the dolt-native instance UUID (from
`<store>/.dolt/sql-server.info') so a later cross-process stop can re-verify
that the claim and native record name the SAME server instance before
signalling (tracked-regression `dorg-dolt-server--verified-daemon-pid').  A reused
PID with a different UUID therefore fails the identity match.  A nil UUID
writes no `:uuid' field, preserving the prior shape.

STATE, when non-nil, records the claim lifecycle state (tracked-regression): a
post-start owner whose served DB could not yet be confirmed publishes
`:state probe-inconclusive' so peers BLOCK on it (see
`dorg-dolt-server--claim-classify') until a later positive re-probe finalizes
it via `dorg-dolt-server--finalize-pending-claim'.  A finalized detached
daemon publishes `:state ready' with the full identity tuple.  A nil STATE
writes no `:state' field (an ordinary confirmed claim), preserving the prior
shape.

Reuses the existing owner token for DOLT-DIR so refreshes keep a stable
identity, and preserves the original `:created-at'.  Returns the owner
token."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (token (or (gethash canon dorg-dolt-server--owned-claims)
                    (dorg-dolt-server--make-token)))
         (now (float-time))
         (prev (dorg-dolt-server--claim-read dolt-dir))
         (derived (dorg-dolt-server--data-dir-and-db dolt-dir))
         (server-pid (or server-pid (and prev (alist-get :server-pid prev))))
         (data-dir (or data-dir (car derived)))
         (db-name (or db-name (cdr derived)))
         (claim (append
                 (list (cons :dolt-dir canon)
                      (cons :data-dir (expand-file-name data-dir))
                      (cons :db-name db-name)
                      (cons :host (system-name))
                      (cons :owner-pid (or owner-pid (emacs-pid)))
                      (cons :owner-kind (or owner-kind 'emacs))
                      (cons :server-pid server-pid)
                      (cons :port port)
                      (cons :token token)
                      (cons :policy 'server)
                      (cons :protocol dorg-dolt-server-claim-protocol))
                 (and uuid (list (cons :uuid uuid)))
                 (and state (list (cons :state state)))
                 (list
                      (cons :created-at (or (and prev (alist-get :created-at prev))
                                            now))
                      (cons :last-used now)))))
    (puthash canon token dorg-dolt-server--owned-claims)
    (dorg-dolt-server--claim-write dolt-dir claim)
    token))

(defun dorg-dolt-server--publish-provisional-claim
    (dolt-dir port server-pid data-dir db-name owner-pid owner-kind)
  "Publish the one pre-readiness pending owner for DOLT-DIR on PORT.
SERVER-PID is the launched child or daemon pid.  DATA-DIR, DB-NAME,
OWNER-PID, and OWNER-KIND carry the same normalized identity later retained by
`dorg-dolt-server--finalize-pending-claim'.  The claim has no UUID and records
`:state probe-inconclusive'; success must finalize this exact claim in place,
never break and republish it.  Callers hold the claim lock."
  (dorg-dolt-server--publish-claim
   dolt-dir port server-pid data-dir db-name owner-pid owner-kind
   nil 'probe-inconclusive))

(defun dorg-dolt-server--native-uuid-for (dolt-dir pid port)
  "Return DOLT-DIR's dolt-native instance UUID IFF native identity matches.
Reads `<store>/.dolt/sql-server.info' (`dorg-dolt-server--dolt-server-info')
and returns its `:uuid' ONLY when the native record is present, carries a
NON-EMPTY uuid, and its pid and port equal the expected PID and PORT of the
daemon being finalized/published.  Returns nil on any absent/malformed/
mismatched field, so a caller can REFUSE to publish a `:state ready' claim it
could not later re-verify — a ready claim with a nil/wrong uuid is unstoppable
and misleading (tracked-regression `dorg-dolt-server--verified-daemon-pid')."
  (let* ((native (dorg-dolt-server--dolt-server-info dolt-dir))
         (nuuid (plist-get native :uuid))
         (npid (plist-get native :pid))
         (nport (plist-get native :port)))
    (and native
         (stringp nuuid) (not (string-empty-p nuuid))
         (integerp pid) (eql pid npid)
         (integerp port) (eql port nport)
         nuuid)))

(defun dorg-dolt-server--finalize-pending-claim (dolt-dir state &optional uuid)
  "Rewrite DOLT-DIR's pending claim to STATE in place, preserving identity.
Recovery finalizes the SAME claim (tracked-regression): only `:state',
`:last-used', and — when UUID is non-nil — `:uuid' change; the rest of the
identity tuple (`:token', `:owner-kind', `:server-pid', `:port', `:created-at')
and every other field are byte-preserved, so a recovered pending server keeps
the exact ownership it published pre-finalize — NOT a freshly minted token.

UUID, when non-nil, persists the verified dolt-native instance UUID alongside
the `:state ready' transition so a later cross-process stop can re-verify the
4-tuple identity (tracked-regression): a pending daemon publishes NO uuid (identity
not yet finalized), so recovery to `ready' must wire the native uuid in here or
the recovered daemon could never be stopped.  Returns the updated claim, or nil
when no claim is present."
  (let ((claim (dorg-dolt-server--claim-read dolt-dir)))
    (when claim
      (setf (alist-get :state claim) state)
      (when uuid (setf (alist-get :uuid claim) uuid))
      (setf (alist-get :last-used claim) (float-time))
      (dorg-dolt-server--claim-write dolt-dir claim)
      claim)))

(defun dorg-dolt-server--owned-pending-p (dolt-dir)
  "Non-nil when DOLT-DIR's overlay claim is a `:state probe-inconclusive' owner.
A locally-owned pending child is retained in `dorg-dolt-server--procs' (so
`dorg-dolt-server-port' returns its port) but its served DB is UNCONFIRMED, so
every selector that would return that cached port — the hot path
\(`dorg-dolt-server--strict-port') and the ensure paths — must first revalidate
it (finalize-or-refuse via `dorg-dolt-server--recover-owned-pending'), never
hot-serve it as ready (tracked-regression nav-vx1 BF1)."
  (eq (alist-get :state (dorg-dolt-server--claim-read dolt-dir))
      'probe-inconclusive))

(cl-defun dorg-dolt-server--recover-owned-pending (dolt-dir local-port &key replace)
  "Revalidate a locally-owned `:state probe-inconclusive' child on LOCAL-PORT.
The SINGLE tri-state served-store revalidation both the hot path
\(`dorg-dolt-server--strict-port') and the recovery entrypoints
\(`dorg-dolt-server--ensure-strict', `dorg-dolt-server--attach-only') route a
pending child through, so a retained-but-unconfirmed child is NEVER hot-served
on its cached port without a fresh served-DB probe (tracked-regression nav-vx1
BF1).  Re-probes `dorg-dolt-server--store-valid-p' EXACTLY ONCE:
- CONFIRMED served => finalize the SAME claim to `:state ready' (identity tuple
  byte-preserved) and return LOCAL-PORT — the exact retained proc, zero relaunch;
- still INCONCLUSIVE => keep it pending and raise `dorg-sql-transport-error';
- CONFIRMED wrong store => when REPLACE is non-nil reap the mis-serving child and
  start a fresh server (`dorg-dolt-server--start-unsafe'); otherwise (attach-only,
  which never starts) raise fail-closed.
Must be called with the claim lock held."
  (let* ((db-name (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))
         (binding (dorg-dolt-server--expected-binding dolt-dir))
         (verdict (dorg-dolt-server--store-valid-p local-port db-name binding)))
    (cond
     ((eq verdict t)
      (dorg-dolt-server--finalize-pending-claim dolt-dir 'ready)
      local-port)
     ((eq (plist-get verdict :reason) 'probe-inconclusive)
      (signal 'dorg-sql-transport-error
              (list (concat (dorg-dolt-server--store-invalid-message
                             verdict dolt-dir db-name local-port)
                            "; kept the pending server claim"))))
     (replace
      (dorg-dolt-server--reap-wedged dolt-dir)
      (dorg-dolt-server--start-unsafe dolt-dir))
     (t
      (signal 'dorg-sql-transport-error
              (list (concat (dorg-dolt-server--store-invalid-message
                             verdict dolt-dir db-name local-port)
                            "; refusing to attach a pending server that confirmed the wrong store")))))))

(defun dorg-dolt-server--break-claim (dolt-dir)
  "Forcibly remove DOLT-DIR's claim file and forget any owned token.
Used under the claim lock once a claim has been classified dead."
  (remhash (dorg-dolt-server--canonical-dir dolt-dir) dorg-dolt-server--owned-claims)
  (remhash (dorg-dolt-server--canonical-dir dolt-dir) dorg-dolt-server--attached-ports)
  (dorg-dolt-server--claim-delete-file dolt-dir))

(defun dorg-dolt-server--evict-attached-port (dolt-dir port)
  "Compare-and-remove DOLT-DIR's attached-port hint iff it equals PORT.
The warm-connection self-heal (`dorg-sql--open-entry', tracked-case) calls this
when a connection to a cached-but-stale attached port fails: it drops ONLY that
exact memo, never a newer port another path just installed, so the attached-port
hint is no longer independently authoritative on the query hot path."
  (let ((canon (dorg-dolt-server--canonical-dir dolt-dir)))
    (when (eql (gethash canon dorg-dolt-server--attached-ports) port)
      (remhash canon dorg-dolt-server--attached-ports))))

(defun dorg-dolt-server--reap-wedged (dolt-dir)
  "Reap a LOCAL OWNED wedged sql-server for DOLT-DIR, then break its claim.
A wedged server accepts TCP but never answers SQL; left in place it holds
the repo's Dolt write lock and hangs every client.  Only a server THIS
process runs — a live `dorg-dolt-server--procs' entry, an object whose exact
process handle we retain — is reaped, via `dorg-dolt-server--kill-process'
\(SIGTERM->SIGKILL); its claim is then broken so a later classify cannot
resurrect it.

Cross-process FOREIGN wedged reap is RETIRED (tracked-regression, OC option C):
a PID read from a claim is a transport handle, never authority, and a wedged
server cannot pass served-store validation, so a same-host foreign wedged
claim is NEVER signalled and its claim file is left byte-identical (attach or
abandon, never reap).  Callers should hold the claim lock (see
`dorg-dolt-server--ensure-strict')."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash canon dorg-dolt-server--procs)))
    (when entry
      (dorg-dolt-server--kill-process (car entry))
      (remhash canon dorg-dolt-server--procs)
      (dorg-dolt-server--break-claim dolt-dir))))

(defun dorg-dolt-server--release-claim (dolt-dir)
  "Release DOLT-DIR's claim if and only if this process owns it.
Token-checked: a claim rewritten by another process (different token)
is left untouched, so an attach-only client or a restarted owner cannot
delete a newer owner's claim.  Always clears local owned/attached state."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (token (gethash canon dorg-dolt-server--owned-claims))
         (claim (dorg-dolt-server--claim-read dolt-dir)))
    (when (and token claim (equal token (alist-get :token claim)))
      (dorg-dolt-server--claim-delete-file dolt-dir))
    (remhash canon dorg-dolt-server--owned-claims)
    (remhash canon dorg-dolt-server--attached-ports)))

(defun dorg-dolt-server--scrub-vanished-registry-dirs ()
  "Forget process-local registry entries whose canonical dir no longer exists.
Returns the number of canonical dirs removed.  This is bookkeeping cleanup
only: it removes entries from `dorg-dolt-server--owned-claims' and
`dorg-dolt-server--attached-ports' without touching claim files or
re-canonicalizing vanished paths."
  (let ((stale (make-hash-table :test 'equal)))
    (dolist (table (list dorg-dolt-server--owned-claims
                         dorg-dolt-server--attached-ports))
      (maphash (lambda (canon _value)
                 (unless (and (stringp canon) (file-directory-p canon))
                   (puthash canon t stale)))
               table))
    (let ((count 0))
      (maphash (lambda (canon _value)
                 (remhash canon dorg-dolt-server--owned-claims)
                 (remhash canon dorg-dolt-server--attached-ports)
                 (setq count (1+ count)))
               stale)
      count)))

(defun dorg-dolt-server-claim (dolt-dir)
  "Return the current ownership claim alist for DOLT-DIR, or nil.
Public reader exposing claim/session identity (owner, port, token,
protocol, timestamps) for warm-session freshness checks
without switching transports."
  (dorg-dolt-server--claim-read dolt-dir))

;;; ─── Served-Store Validation ────────────────────────────────────────────
;;
;; A claim proving only that *some* server published a port for a canonical
;; dolt-dir is not enough: the listening server must serve the EXPECTED
;; store.  Beyond the name-only served-set membership test
;; (`dorg-dolt-server--db-in-served-p' over a `dorg-dolt-server--list-served-dbs'
;; snapshot), two further gates
;; — applied both post-start (before upgrading a provisional claim to ready)
;; and at attach-time (before trusting a foreign claim): the served database
;; must carry the DOrg
;; schema marker table `dorg_files', and — when the start/ensure is associated
;; with a file-backed DOrg buffer — `dorg_files' must hold a row for the
;; canonical bound file/root.  Every probe rides the dolt CLI wire (no
;; mysql.el) and fails closed (nil/failure) so a server that cannot prove
;; itself is never trusted.

(defun dorg-dolt-server--list-tables (port db-name)
  "Return the list of table names served in DB-NAME by the sql-server on PORT.
Connects over the dolt CLI's global `--host/--port' wire (no mysql.el) and
runs `SHOW TABLES FROM DB-NAME'.  Returns nil on any connection or parse
failure — callers treat that as \"schema not confirmed\" and fail closed."
  (ignore-errors
    (let ((buf (generate-new-buffer " *dorg-show-tables*")))
      (unwind-protect
          (let ((exit (call-process
                       "dolt" nil buf nil
                       "--host" "127.0.0.1"
                       "--port" (number-to-string port)
                       "--user" "root" "--password" "" "--no-tls"
                       "sql" "-q"
                       (format "SHOW TABLES FROM `%s`"
                               (replace-regexp-in-string "`" "``" db-name))
                       "-r" "json")))
            (when (eq exit 0)
              (with-current-buffer buf
                (let* ((parsed (dorg-sql--parse-json-or-nil
                                (string-trim (buffer-string))))
                       (rows (and parsed (alist-get 'rows parsed))))
                  ;; `rows' is a vector of single-column alists; the column
                  ;; name varies by Dolt version (Tables_in_<db>), so take
                  ;; each row's sole value rather than keying by name.
                  (mapcar (lambda (row)
                            (let ((cell (if (listp row) (car row) row)))
                              (format "%s" (if (consp cell) (cdr cell) cell))))
                          (append rows nil))))))
        (kill-buffer buf)))))

(defun dorg-dolt-server--schema-present-p (port db-name)
  "Return non-nil when DB-NAME on PORT carries the DOrg `dorg_files' marker.
The marker table identifies a database as a DOrg store; a database of the
right name without it (empty/accidental repo) is not a DOrg store."
  (let ((tables (dorg-dolt-server--list-tables port db-name)))
    (cl-some (lambda (tbl) (equal (downcase tbl) "dorg_files")) tables)))

(defun dorg-dolt-server--bound-file-served-p
    (port db-name dolt-dir abs-file &optional root-id)
  "Return non-nil when DB-NAME on PORT holds a `dorg_files' row for ABS-FILE.
Matches ABS-FILE against the canonical store-relative binding for DOLT-DIR.
When ROOT-ID is non-nil the row must also carry that root id.  Connects over
the dolt CLI wire (no mysql.el) and fails
closed (nil) on any connection or parse failure."
  (ignore-errors
    (let ((buf (generate-new-buffer " *dorg-bound-file*")))
      (unwind-protect
          (let* ((pred (dorg-repo--relative-path-match
                        "f.relative_path" dolt-dir abs-file))
                 (quoted-db (replace-regexp-in-string "`" "``" db-name))
                 (sql (format (concat "SELECT f.root_id FROM `%s`.dorg_files f "
                                      "JOIN `%s`.headings h ON h.id = f.root_id "
                                      "WHERE %s%s AND h.parent_id IS NULL "
                                      "AND h.level_offset = 0 "
                                      "AND h.root_id = f.root_id LIMIT 1")
                              quoted-db quoted-db pred
                              (if root-id
                                  (format " AND f.root_id = %d" root-id)
                                "")))
                 (exit (call-process
                        "dolt" nil buf nil
                        "--host" "127.0.0.1"
                        "--port" (number-to-string port)
                        "--user" "root" "--password" "" "--no-tls"
                        "sql" "-q" sql "-r" "json")))
            (when (eq exit 0)
              (with-current-buffer buf
                (let* ((parsed (dorg-sql--parse-json-or-nil
                                (string-trim (buffer-string))))
                       (rows (and parsed (alist-get 'rows parsed))))
                  (and rows (> (length rows) 0))))))
        (kill-buffer buf)))))

(defun dorg-dolt-server--expected-binding (dolt-dir)
  "Return the expected file binding for DOLT-DIR, or nil.
When the current buffer is a file-backed DOrg buffer already registered
against DOLT-DIR, returns a plist
(:bound-file ABS :dolt-dir CANONICAL-DIR :root-id N) so post-start and attach
validation can confirm the served store actually holds that file.
ABS is the `file-truename' of the buffer file (as stored in `dorg_files').

This deliberately reads `dorg--registry' directly instead of calling public
registration accessors.  The accessors are now pure classifiers, so a
keyword-declared buffer with no stored binding would classify to a transient
Dolt registration with a nil root_id — useless here, where validation needs
an already-bound concrete root_id.  Binding one on demand is also unsafe:
server start validation can run while claim selection already holds the repo
lock, so a strict ensure would recursively autostart the same repo and
deadlock.  Reading the stored binding directly avoids both.  If no explicit
registration is present, return nil and apply only the schema-marker gate."
  (when-let* ((file (buffer-file-name))
              (reg (cdr (assoc (file-truename file) dorg--registry)))
              (backend (dorg-registration-backend reg))
              (reg-dir (dorg-backend-dolt-dir backend)))
    (when (equal (dorg-dolt-server--canonical-dir reg-dir)
                 (dorg-dolt-server--canonical-dir dolt-dir))
      (list :bound-file (file-truename file)
            :dolt-dir (dorg-dolt-server--canonical-dir reg-dir)
            :root-id (dorg-backend-root-id backend)))))

(defun dorg-dolt-server--store-valid-p (port db-name &optional binding)
  "Return t when the sql-server on PORT serves a valid DOrg store for DB-NAME.
Validation layers: (1) DB-NAME is served; (2) it carries the DOrg schema
marker table `dorg_files'; (3) when BINDING is non-nil (a plist with
:bound-file and optional :root-id from a file-backed registration) the
served `dorg_files' holds a row for that file/root.  Returns t when every
applicable gate passes; otherwise a failure plist (:reason SYMBOL
:served-dbs LIST :tables LIST :bound-file STR) describing the first failed
gate, for a bounded diagnostic.  Fails closed: a probe that cannot confirm
a gate yields a failure plist.

TRI-STATE served-DB gate (tracked-regression): the served set is fetched via the
bounded-retry `dorg-dolt-server--served-dbs-or-inconclusive', which tells a
CONFIRMED served set apart from an INCONCLUSIVE probe (transient
connection/parse failure).  A confirmed set that lacks DB-NAME is `wrong-db'
\(fail-closed mismatch, unchanged); an inconclusive probe that exhausts the
retry bound yields `(:reason probe-inconclusive :cause FIRST-CAUSE)' — never a
false `wrong-db' — so the caller preserves the server/claim and raises rather
than reaping a healthy server on a transient failure."
  ;; Fetch the served-DB set ONCE (bounded retry) and reuse that single
  ;; immutable snapshot for both the membership decision and the diagnostic
  ;; `:served-dbs', so a `wrong-db' verdict can never report the very DB it just
  ;; refused (the parent incident's self-contradiction: decide on snapshot A,
  ;; report B).
  (let ((probe (dorg-dolt-server--served-dbs-or-inconclusive port)))
    (cond
     ((eq (car probe) 'inconclusive)
      ;; Could not confirm the served set within the bound: a transient
      ;; failure, NOT a confirmed mismatch.  Preserve-and-raise disposition.
      (list :reason 'probe-inconclusive :cause (cdr probe)))
     ((not (dorg-dolt-server--db-in-served-p (cdr probe) db-name))
      (list :reason 'wrong-db
            :served-dbs (cdr probe)))
     ((not (dorg-dolt-server--schema-present-p port db-name))
      (list :reason 'no-schema
            :served-dbs (cdr probe)
            :tables (dorg-dolt-server--list-tables port db-name)))
     ((and binding
           (plist-get binding :bound-file)
           (not (dorg-dolt-server--bound-file-served-p
                 port db-name (plist-get binding :dolt-dir)
                 (plist-get binding :bound-file)
                 (plist-get binding :root-id))))
      (list :reason 'wrong-bound-file
            :tables (dorg-dolt-server--list-tables port db-name)
            :bound-file (plist-get binding :bound-file)))
     (t t))))

(defun dorg-dolt-server--store-invalid-message (verdict dolt-dir db-name &optional port)
  "Build a bounded diagnostic for a failed store VERDICT.
Names the expected DOLT-DIR, expected DB-NAME, the failure reason, observed
databases/tables when available, and the claim file path.  Callers append
their own outcome clause (e.g. \"killed the server and published no
claim\")."
  (let ((reason (plist-get verdict :reason))
        (served (plist-get verdict :served-dbs))
        (tables (plist-get verdict :tables))
        (bound (plist-get verdict :bound-file)))
    (concat
     (format "dorg-dolt-server: sql-server%s for %s "
             (if port (format " on port %d" port) "")
             (expand-file-name dolt-dir))
     (pcase reason
       ('wrong-db
        (format "does not serve the expected database %S (served: %s)"
                db-name (if served (mapconcat #'identity served ", ") "none")))
       ('no-schema
        (format (concat "store for database %S is missing the DOrg schema "
                        "marker table dorg_files (tables: %s)")
                db-name (if tables (mapconcat #'identity tables ", ") "none")))
       ('wrong-bound-file
        (format "store for database %S has no dorg_files row for the bound file %s"
                db-name bound))
       ('probe-inconclusive
        ;; A transient/inconclusive probe confirmed NO served set, so the
        ;; diagnostic names the exact cause and INVENTS no served-set text
        ;; (never `served: none') — the parent incident's false-mismatch trap.
        (format "could not confirm the databases it serves for %S (probe inconclusive: %s)"
                db-name (or (plist-get verdict :cause) "unknown cause")))
       (_ (format "does not serve a valid DOrg store for database %S" db-name)))
     (format "; expected dolt-dir %s, claim file %s"
             (expand-file-name dolt-dir)
             (dorg-dolt-server--claim-file dolt-dir)))))

;; Pre-start validation (the start path) queries the dolt-dir DIRECTLY via the
;; bounded one-shot CLI primitive rather than probing the server we are about
;; to launch.
;; A data query (e.g. SELECT on `dorg_files') issued against a freshly-started
;; dolt sql-server — before it finishes initializing the user database — can
;; wedge that server and deadlock the client (the 2026-06-25 integration-test
;; hang).  On the start path we own the data-dir and no server holds it yet,
;; so a one-shot CLI read is both correct (it is the exact store the server
;; will serve) and deadlock-free.  Attach-time validation has no such dir
;; access (a live owner holds the Dolt lock) and probes the already-warm
;; foreign server over the wire instead.

(defun dorg-dolt-server--dir-sql-rows (dolt-dir sql)
  "Run SQL against DOLT-DIR through the bounded one-shot CLI primitive.
Return parsed rows, or nil on failure.  This private probe is used only on the
pre-start path, after the caller established that no managed server holds the
dir.  The direct primitive is therefore deadlock-free while still delegating
target normalization, timeout/reaping, and parsing to the single SQL boundary."
  (ignore-errors
    ;; The claim-lock start path has excluded a live owner, so the CLI conflict
    ;; guard does not apply.  This probe can also run inside lease acquisition,
    ;; so it must bypass the ordinary-query lease guard in `dorg-sql'.
    (let ((dorg-sql--cli-conflict-check nil))
      (dorg-sql--cli-exec sql (dorg-dolt--target dolt-dir)))))

(defun dorg-dolt-server--dir-store-valid-p (dolt-dir &optional binding)
  "Validate the DOLT-DIR store before starting a server.
Queries the dolt-dir directly (one-shot CLI): requires the `dorg_files'
schema marker and — when BINDING is file-backed — a `dorg_files' row for
the bound file (matching :root-id when given).  Returns t when every
applicable gate passes, else a failure plist (:reason no-schema |
wrong-bound-file :tables LIST :bound-file STR) for a bounded diagnostic.
Fails closed: an unreadable store yields a `no-schema' failure."
  (let* ((rows (dorg-dolt-server--dir-sql-rows dolt-dir "SHOW TABLES"))
         (tables (mapcar (lambda (row)
                           (let ((cell (if (listp row) (car row) row)))
                             (format "%s" (if (consp cell) (cdr cell) cell))))
                         rows)))
    (cond
     ((not (cl-some (lambda (tbl) (equal (downcase tbl) "dorg_files")) tables))
      (list :reason 'no-schema :tables tables))
     ((and binding
           (plist-get binding :bound-file)
           (not (dorg-dolt-server--dir-sql-rows
                 dolt-dir
                 (format (concat "SELECT f.root_id FROM dorg_files f "
                                 "JOIN headings h ON h.id = f.root_id "
                                 "WHERE %s%s AND h.parent_id IS NULL "
                                 "AND h.level_offset = 0 "
                                 "AND h.root_id = f.root_id LIMIT 1")
                         (dorg-repo--relative-path-match
                          "f.relative_path" dolt-dir
                          (plist-get binding :bound-file))
                         (if (plist-get binding :root-id)
                             (format " AND f.root_id = %d"
                                     (plist-get binding :root-id))
                           "")))))
      (list :reason 'wrong-bound-file
            :tables tables
            :bound-file (plist-get binding :bound-file)))
     (t t))))

;;; ─── Strict Server Ensure / Attach ──────────────────────────────────────

;; The three attaching selectors (`--ensure-strict', `--attach-only',
;; `--ensure-detached') share ONE live/compatible attach heart:
;; optional dolt-native UUID guard, the expected-store probe, a memoized
;; success, and a single invalid-verdict continuation.  Only that continuation
;; differs per selector, so it is passed in as `on-invalid'.  The divergent
;; non-live outcome arms (null / owned / pending / wedged / dead / unreachable)
;; stay legible at each selector — they do NOT reduce to one table.

(defun dorg-dolt-server--memo-attached-port (dolt-dir port)
  "Record PORT as DOLT-DIR's attached foreign server and return PORT.
Writes the process-local `dorg-dolt-server--attached-ports' entry that the
hot SQL transport path (`dorg-dolt-server--strict-port')
routes through without re-probing a claim.  `--break-claim' /
`--release-claim' drop the entry when the claim lifecycle invalidates it.
The single home for the attached-port memo, formerly pasted across the
attaching selectors and `dorg-dolt-server-adopt-port'."
  (puthash (dorg-dolt-server--canonical-dir dolt-dir)
           port dorg-dolt-server--attached-ports)
  port)

(defun dorg-dolt-server--assert-uuid-current (dolt-dir claim)
  "Signal unless CLAIM's dolt-native instance UUID is still current for DOLT-DIR.
For a foreign server discovered via `sql-server.info', confirm its UUID is
still current before attaching, so a server replaced (pid/port reused)
between discovery and attach is not silently trusted.  No-op for a
non-`dolt' owner kind (only dolt-native claims carry a verifiable UUID).
Signals `dorg-sql-transport-error' (\"changed instance ... during attach;
retry\") on mismatch, BEFORE any store probe."
  (when (and (eq (alist-get :owner-kind claim) 'dolt)
             (not (dorg-dolt-server--dolt-uuid-current-p
                   dolt-dir (alist-get :uuid claim))))
    (signal 'dorg-sql-transport-error
            (list (format "dorg: the dolt sql-server for %s changed instance (sql-server.info UUID mismatch) during attach; retry"
                          dolt-dir)))))

(defun dorg-dolt-server--signal-store-invalid (verdict dolt-dir db-name port)
  "Fail-closed `on-invalid' for `dorg-dolt-server--attach-live'.
A live, protocol-compatible owner serving the WRONG store must not be
attached.  Strict `server' transport cannot start a second server over a
live compatible owner, so a wrong store is a hard error here — no attach,
no CLI fallback.  Used by `--ensure-strict' and `--attach-only'."
  (signal 'dorg-sql-transport-error
          (list (concat (dorg-dolt-server--store-invalid-message
                         verdict dolt-dir db-name port)
                        "; refusing to attach or fall back to CLI"))))

(defun dorg-dolt-server--signal-foreign-wedged (claim dolt-dir)
  "Fail closed on a FOREIGN wedged owner of DOLT-DIR (tracked-regression).
A wedged server (TCP up, SQL dead) cannot pass served-store/identity
validation, so its recorded PID is a transport handle, never authority:
a peer must neither reap/replace it nor rewrite/delete its claim (attach
or abandon).  Both attaching lifecycle selectors that DEEP-classify —
`dorg-dolt-server--ensure-strict' and `dorg-dolt-server--ensure-detached'
— route their foreign `wedged' arm here, preserving the claim
byte-for-byte and supplying one shared operator message.  A LOCAL owned
wedged child (a retained `dorg-dolt-server--procs' handle) is NOT reached
here: it is reaped and replaced by `--ensure-strict's own local-port arm."
  (signal 'dorg-sql-transport-error
          (list (format "dorg: a DOrg sql-server (pid %s, port %s) owns %s but is wedged (TCP up, SQL dead); cross-process reap is retired — refusing to reap or replace a foreign wedged owner"
                        (alist-get :owner-pid claim)
                        (alist-get :port claim) dolt-dir))))

(defun dorg-dolt-server--attach-invalid-replace
    (verdict dolt-dir db-name port start-fn preserved-suffix)
  "Replace-on-confirmed `on-invalid' for `dorg-dolt-server--attach-live'.
Tri-state (tracked-regression): an INCONCLUSIVE served-DB probe is NOT a
confirmed wrong store — preserve the live owner and its claim (zero
break-claim, zero start) and raise with PRESERVED-SUFFIX so a retry
attaches.  A CONFIRMED wrong store stops trusting the (claim-file-only)
claim: break it and start a correct server via START-FN (called with
DOLT-DIR).  Used by `--ensure-detached' (with `--start-detached-unsafe')."
  (if (eq (plist-get verdict :reason) 'probe-inconclusive)
      (signal 'dorg-sql-transport-error
              (list (concat (dorg-dolt-server--store-invalid-message
                             verdict dolt-dir db-name port)
                            preserved-suffix)))
    (dorg-dolt-server--break-claim dolt-dir)
    (funcall start-fn dolt-dir)))

(cl-defun dorg-dolt-server--attach-live (dolt-dir claim &key check-uuid on-invalid)
  "Attach to the live, compatible CLAIM's server for DOLT-DIR; return its port.
The one shared live-attach core the three attaching selectors delegate to.
When CHECK-UUID, assert the dolt-native instance UUID is still current
BEFORE any store probe (`--assert-uuid-current').  Then validate the owner
serves the EXPECTED store — against the database name OUR DOLT-DIR maps to,
not the claim's self-reported `:db-name', so a wrong/foreign claim cannot
pick the name it is checked against.  On a valid store, memoize the port
and return it (the unified success path).  Otherwise call ON-INVALID with
\(VERDICT DOLT-DIR DB-NAME PORT) — the ONLY per-selector variation."
  (when check-uuid
    (dorg-dolt-server--assert-uuid-current dolt-dir claim))
  (let* ((port (alist-get :port claim))
         (db-name (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))
         (binding (dorg-dolt-server--expected-binding dolt-dir))
         (verdict (dorg-dolt-server--store-valid-p port db-name binding)))
    (if (eq verdict t)
        (dorg-dolt-server--memo-attached-port dolt-dir port)
      (funcall on-invalid verdict dolt-dir db-name port))))

(defun dorg-dolt-server--ensure-strict (dolt-dir)
  "Ensure strict `server' transport has a usable sql-server for DOLT-DIR.
Returns the port to connect to via mysql.el.  Under the claim lock it
either reuses a locally owned server, attaches to a live compatible
claim owned by another process, breaks and replaces a dead claim,
or signals `dorg-sql-transport-error' for an unreachable/incompatible
live owner — never starting a second server and never falling back to
one-shot CLI."
  (dorg-dolt-server--with-claim-lock dolt-dir
    (let ((local-port (dorg-dolt-server-port dolt-dir)))
      (cond
       ;; We already run a server for this repo: reuse it only if it still
       ;; answers SQL.  A server we own can wedge (accept TCP, never reply);
       ;; reuse-by-liveness would hand back the wedged port and hang.  Probe
       ;; once here (the cache-miss decision path, not the hot query path in
       ;; `dorg-dolt-server--strict-port'); if wedged, reap and start a fresh one.
       ((and local-port (dorg-dolt-server--port-healthy-p local-port))
        (let ((claim (dorg-dolt-server--claim-read dolt-dir)))
          (if (eq (alist-get :state claim) 'probe-inconclusive)
              ;; A pending child we retained after a post-start INCONCLUSIVE
              ;; probe (tracked-regression): route it through the SINGLE tri-state
              ;; revalidation — confirmed => finalize the SAME claim to `:state
              ;; ready' reusing the exact proc; still inconclusive => keep it
              ;; pending and raise; confirmed wrong store => reap + replace.
              (dorg-dolt-server--recover-owned-pending dolt-dir local-port :replace t)
            (dorg-dolt-server--publish-claim dolt-dir local-port)
            local-port)))
       (local-port
        (dorg-dolt-server--reap-wedged dolt-dir)
        (dorg-dolt-server--start-unsafe dolt-dir))
       (t
        ;; Effective claim reconciles the store-relative overlay with dolt's
        ;; native `sql-server.info' (and a read-only legacy claim): a server
        ;; started by raw dolt or another environment is discovered here, so
        ;; strict mode attaches to the one server rather than starting a second.
        (let ((claim (dorg-dolt-server--effective-claim dolt-dir)))
          (if (null claim)
              ;; Already under the claim lock: call the unlocked primitive
              ;; directly so we neither re-enter the mutex nor re-run the
              ;; public conflict check we have already decided here.
              (dorg-dolt-server--start-unsafe dolt-dir)
            (dorg-dolt-server--assert-claim-not-foreign-store dolt-dir claim)
            ;; Deep classify: this is the cache-miss attach decision, so pay
            ;; for one SQL probe to distinguish a healthy owner from a wedged
            ;; one (accepts TCP, never answers) before committing to attach.
            (pcase (dorg-dolt-server--claim-classify claim t)
              ('live
               (cond
                ;; Our token but no live local proc — local state is stale;
                ;; drop the claim and start a fresh owned server (new port).
                ((dorg-dolt-server--claim-owned-here-p claim)
                 (dorg-dolt-server--break-claim dolt-dir)
                 (dorg-dolt-server--start-unsafe dolt-dir))
                ((dorg-dolt-server--claim-compatible-p claim)
                 ;; Foreign live compatible owner: assert dolt-native UUID
                 ;; currency then validate the served store before routing
                 ;; through it.  Fail-closed on a wrong store — strict mode
                 ;; cannot start a second server over a live compatible owner.
                 (dorg-dolt-server--attach-live
                  dolt-dir claim :check-uuid t
                  :on-invalid #'dorg-dolt-server--signal-store-invalid))
                (t
                 (signal 'dorg-sql-transport-error
                         (list (format "dorg: a DOrg sql-server owns %s but its claim is incompatible (protocol %s); refusing to start a second server or use CLI"
                                       dolt-dir (alist-get :protocol claim)))))))
              ;; A FOREIGN wedged owner (TCP up, SQL dead) reached WITHOUT a
              ;; retained local proc (local-port is nil, so this is not our
              ;; owned child): its PID is a transport handle, never authority,
              ;; and a wedged server cannot pass served-store validation, so a
              ;; peer neither reaps nor replaces it — FAIL CLOSED (tracked-regression;
              ;; zero kill, zero break, zero attach-cache eviction, zero start).
              ;; A locally owned wedged child IS reaped and replaced by the
              ;; `local-port' arm above, which retains its process object.
              ('wedged
               (dorg-dolt-server--signal-foreign-wedged claim dolt-dir))
              ;; A foreign pending (unfinalized) owner reached without a local
              ;; proc: its served DB is unconfirmed, so a peer must not
              ;; attach-as-ready or replace it — block and raise
              ;; (tracked-regression; owned-here pending recovery is the fast path
              ;; above).
              ('pending
               (signal 'dorg-sql-transport-error
                       (list (format "dorg: the DOrg sql-server owning %s (pid %s, port %s) has not confirmed its served database (pending); refusing to attach-as-ready or replace it"
                                     dolt-dir (alist-get :owner-pid claim)
                                     (alist-get :port claim)))))
              ('dead
               (dorg-dolt-server--break-claim dolt-dir)
               (dorg-dolt-server--start-unsafe dolt-dir))
              ('unreachable
               (signal 'dorg-sql-transport-error
                       (list (format "dorg: the DOrg sql-server owning %s (pid %s, port %s) is unreachable; strict `server' transport refuses CLI fallback"
                                      dolt-dir (alist-get :owner-pid claim)
                                      (alist-get :port claim)))))))))))))

(defun dorg-dolt-server--strict-port (dolt-dir)
  "Return a ready strict-server port for DOLT-DIR for the hot query path.
Reuses a locally owned server or a previously attached foreign port
without re-probing.

When `dorg-dolt-server-autostart' is nil, every remaining case — regardless
of owner kind (daemon, dolt-native, or foreign Emacs session) or claim
health (absent, dead, stale, wedged, or a live claim that disappears
before the locked attach) — routes through the single locked attach-only
selector `dorg-dolt-server--attach-only', which may only attach a
validated live server and never starts, restarts, or reaps into a
replacement.

When `dorg-dolt-server-autostart' is non-nil, behavior is unchanged from
before the option existed: no visible claim, or a claim belonging to the
durable daemon lifecycle, delegates to `dorg-dolt-server--ensure-detached'
\(the server-only home for v0.1 batch/one-shot autostart); any other
visible claim falls back to the full `dorg-dolt-server--ensure-strict'
decision (lock, claim read, classify) so live-owner conflict and attach
checks stay strict."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (local-port (dorg-dolt-server-port dolt-dir)))
    (cond
     ;; A server this Emacs owns.  A READY owned child is hot-served on its
     ;; cached port directly.  A PENDING owned child (retained after a
     ;; post-start INCONCLUSIVE probe, tracked-regression) must NOT be hot-served:
     ;; its served DB is unconfirmed, so route it through the SAME tri-state
     ;; revalidation the recovery entrypoints use (finalize on served,
     ;; stay-pending + raise on inconclusive) rather than returning its cached
     ;; port unchecked (nav-vx1 BF1).  The `:state' read is one small claim-file
     ;; access, fires ONLY for a locally-owned server, and is never a SQL probe.
     (local-port
      (if (dorg-dolt-server--owned-pending-p dolt-dir)
          (if dorg-dolt-server-autostart
              (dorg-dolt-server--ensure-strict dolt-dir)
            (dorg-dolt-server--attach-only dolt-dir))
        local-port))
     ;; A validated attachment is a process-local cache.  Claim identity is
     ;; checked at its first attach decision; re-reading a claim per query
     ;; would turn this hot path into lock/probe/subprocess work.
     ((gethash canon dorg-dolt-server--attached-ports)
      (gethash canon dorg-dolt-server--attached-ports))
     ((not dorg-dolt-server-autostart)
      (dorg-dolt-server--attach-only dolt-dir))
     (t
      (let ((claim (dorg-dolt-server--effective-claim dolt-dir)))
        (if (and claim (not (eq (alist-get :owner-kind claim) 'daemon)))
            (dorg-dolt-server--ensure-strict dolt-dir)
          (dorg-dolt-server--ensure-detached dolt-dir)))))))

;;; ─── Lifecycle ─────────────────────────────────────────────────────────

(defun dorg-dolt-server--start (dolt-dir)
  "Start a dolt sql-server for DOLT-DIR, return the port number.
Routes through the claim protocol: under the claim lock it refuses to
start a second server when a live or unreachable DOrg claim owned by
another process already holds DOLT-DIR (which would overwrite that
owner's claim), signaling `dorg-sql-transport-conflict' instead — use
`dorg-dolt-server--ensure-strict' to attach to an existing owner.  A dead or stale
foreign claim is reaped first.  Signals an error if this process already
runs a server for DOLT-DIR."
  (dorg-dolt--ensure-available)
  (dorg-dolt-server--with-claim-lock dolt-dir
    (unless (dorg-dolt-server-port dolt-dir)   ; reuse handled by --start-unsafe
      (let ((claim (dorg-dolt-server--effective-claim dolt-dir)))
        (when (and claim (not (dorg-dolt-server--claim-owned-here-p claim)))
          (pcase (dorg-dolt-server--claim-classify claim)
            ((or 'live 'unreachable)
             (signal 'dorg-sql-transport-conflict
                     (list (format "dorg: refusing to start a second sql-server for %s; a DOrg server (pid %s, port %s) owns it — attach via `dorg-dolt-server--ensure-strict' instead"
                                   dolt-dir (alist-get :owner-pid claim)
                                   (alist-get :port claim)))))
            ('dead
             (dorg-dolt-server--break-claim dolt-dir))))))
    (dorg-dolt-server--start-unsafe dolt-dir)))

(defun dorg-dolt-server--start-bootstrap (dolt-dir)
  "Start a bootstrap sql-server for a just-initialized DOLT-DIR.
This is the narrow fresh-`dorg-init' entry point.  It follows the same
claim protocol as `dorg-dolt-server--start', but its low-level launch skips
only the pre-start DOrg schema/content validation that a brand-new repo
cannot satisfy yet.  Process registration, provisional publication, readiness,
served-database verification, and in-place claim upgrade stay identical to the
public start path: both publish an owner before readiness and mark that same
claim ready only after confirmation."
  (dorg-dolt--ensure-available)
  (dorg-dolt-server--with-claim-lock dolt-dir
    (unless (dorg-dolt-server-port dolt-dir)
      (let ((claim (dorg-dolt-server--effective-claim dolt-dir)))
        (when (and claim (not (dorg-dolt-server--claim-owned-here-p claim)))
          (pcase (dorg-dolt-server--claim-classify claim)
            ((or 'live 'unreachable)
             (signal 'dorg-sql-transport-conflict
                     (list (format "dorg: refusing bootstrap start for %s; a DOrg server (pid %s, port %s) owns it"
                                   dolt-dir (alist-get :owner-pid claim)
                                   (alist-get :port claim)))))
            ('dead
             (dorg-dolt-server--break-claim dolt-dir))))))
    (dorg-dolt-server--start-bootstrap-unsafe dolt-dir)))

(defconst dorg-dolt-server--spawn-trampoline
  "exec \"$0\" \"$@\" 3>&1 >\"$DORG_SERVER_LOG\" 2>&1"
  "Constant `sh -c' script that becomes the child `dolt sql-server'.
It replaces the shell with the dolt executable passed as $0 and its argv,
so the process keeps its pid and the kernel reports dolt's own argv and
cwd, while stdout and stderr go straight to the file named by
DORG_SERVER_LOG.  No path is ever interpolated into the script.
`3>&1' keeps the write end of Emacs's output pipe open, unused, on fd 3
for the child's lifetime: Emacs blocks inside `accept-process-output' once
a live child's output pipe reaches end of file, so closing it would hang
every later wait, including the mysql client's connect.")

(defun dorg-dolt-server--spawn-logged (name args logfile)
  "Start `dolt' with ARGS as process NAME, its output sunk into LOGFILE.
Return the process.  LOGFILE is truncated at start.  The kernel, not
Emacs, drains the output: the mysql client waits on its own socket only,
so output left for Emacs to read (a pty or pipe) fills up once Dolt has
logged a few failed statements, and the server then blocks mid-reply.
The process has no buffer; LOGFILE is recorded on it as `dorg-server-log'
for startup diagnostics."
  (let* ((dolt (or (executable-find "dolt")
                   (signal 'file-missing
                           (list "Searching for program" "No such file or directory" "dolt"))))
         (logfile (expand-file-name logfile))
         (process-environment (cons (concat "DORG_SERVER_LOG=" logfile)
                                    process-environment))
         (proc (make-process :name name
                             :buffer nil
                             :noquery t
                             :connection-type 'pipe
                             :command (append (list "/bin/sh" "-c"
                                                    dorg-dolt-server--spawn-trampoline
                                                    dolt)
                                              args))))
    (process-put proc 'dorg-server-log logfile)
    proc))

(defconst dorg-dolt-server--log-diagnostic-bytes 65536
  "Most bytes read from the end of a server log for a diagnostic.
The log grows for a server's whole lifetime, so a diagnostic reads only
its tail; the reported excerpt is trimmed further to its last lines.")

(defun dorg-dolt-server--read-log (logfile)
  "Return the trimmed tail of LOGFILE, or nil when it does not exist.
Reads at most `dorg-dolt-server--log-diagnostic-bytes' from the end.  Never
signals: a diagnostic must not decide lifecycle cleanup, so an unreadable
log yields a one-line note instead."
  (when logfile
    (condition-case err
        (when (file-exists-p logfile)
          (let* ((size (or (file-attribute-size (file-attributes logfile)) 0))
                 (beg (max 0 (- size dorg-dolt-server--log-diagnostic-bytes))))
            (with-temp-buffer
              (insert-file-contents logfile nil beg (max beg size))
              (dorg-dolt--trim-output (buffer-string)))))
      (error (format "(server log unreadable: %s)" (error-message-string err))))))

(defun dorg-dolt-server--child-exit-sentinel (proc _event)
  "Release PROC's registered child claim once the child is confirmed dead.
The sentinel is installed before readiness begins, so it owns provisional as
well as ready child claims.  Ownership cleanup comes first.  Only a child
that dies before readiness (`dorg-server-ready' unset) then has the tail of
its log snapshotted on PROC, so the synchronous start failure can still
report Dolt's diagnostic; a ready child's lifetime log is never read here."
  (unless (process-live-p proc)
    (let ((found-key nil))
      (maphash (lambda (key entry)
                 (when (eq (car entry) proc)
                   (setq found-key key)))
               dorg-dolt-server--procs)
      (when found-key
        (remhash found-key dorg-dolt-server--procs)
        (dorg-dolt-server--release-claim found-key)))
    (when (and (processp proc)
               (not (process-get proc 'dorg-server-ready))
               (not (process-get proc 'dorg-startup-output)))
      (process-put proc 'dorg-startup-output
                   (dorg-dolt-server--read-log
                    (process-get proc 'dorg-server-log))))
    (when (buffer-live-p (process-buffer proc))
      (kill-buffer (process-buffer proc)))))

(defun dorg-dolt-server--start-child-unsafe (dolt-dir validate-store)
  "Start a child sql-server for DOLT-DIR and publish its claim, return port.
Low-level primitive that performs no cross-process claim conflict check;
it must be called only after the caller has established ownership under
`dorg-dolt-server--with-claim-lock' (see `dorg-dolt-server--start' and
`dorg-dolt-server--ensure-strict').  When VALIDATE-STORE is non-nil,
validate that DOLT-DIR already carries DOrg schema/content before launch.
Signals an error if this process already runs a server for DOLT-DIR."
  (let* ((target (dorg-dolt--target dolt-dir))
         (key (plist-get target :canonical-repo-dir))
         (existing (gethash key dorg-dolt-server--procs)))
    (when (and existing (process-live-p (car existing)))
      (error "dorg-dolt-server: server already running for %s on port %d"
             dolt-dir (cdr existing)))
    ;; Clean stale entry if any
    (when existing
      (remhash key dorg-dolt-server--procs))
    ;; Unlike the permissive claim-verdict readers, a managed start needs a
    ;; conclusive store-owner census before it can create a second process or
    ;; publish/rewrite any ownership metadata.  Callers hold the claim lock.
    (dorg-dolt-server--assert-admission-clear dolt-dir)
    ;; Fail closed BEFORE launching a server: validate that the dolt-dir we
    ;; are about to serve actually carries the DOrg schema marker and — when
    ;; this start is file-backed — a `dorg_files' row for the bound file.  We
    ;; own the data-dir and no server holds it yet, so this one-shot CLI read
    ;; is deadlock-free (unlike a data query against the freshly-started
    ;; server, which can wedge it).  On failure, drop any stale claim we own
    ;; and signal without starting a server or publishing a claim.
    (when validate-store
      (let* ((db-name (plist-get target :db-name))
             (binding (dorg-dolt-server--expected-binding dolt-dir))
             (verdict (dorg-dolt-server--dir-store-valid-p dolt-dir binding)))
        (unless (eq verdict t)
          (dorg-dolt-server--break-claim dolt-dir)
          (signal 'dorg-sql-transport-error
                  (list (concat (dorg-dolt-server--store-invalid-message
                                 verdict dolt-dir db-name)
                                "; refused to start a server and published no claim"))))))
    (let* ((port (dorg-dolt-server--free-port))
           (data-dir (plist-get target :server-data-dir))
           (db-name (plist-get target :db-name))
           ;; Serve from an explicit data root rather than relying on the
           ;; process cwd: an accidental Dolt repo at `default-directory'
           ;; must never win over the bound store.
           ;; `default-directory' is still pinned to a real directory so the
           ;; subprocess inherits a sane cwd, but it no longer selects the repo.
           (default-directory (expand-file-name data-dir))
           (logfile (expand-file-name
                     "server.log" (dorg-dolt-server--ensure-dorg-dir dolt-dir)))
           (proc (dorg-dolt-server--spawn-logged
                  "dorg-sql-server"
                  (dorg-dolt--target-sql-server-args target port)
                  logfile)))
      (set-process-query-on-exit-flag proc nil)
      ;; Establish ownership before the first readiness observation.  Registry
      ;; and sentinel come first so an early child death releases the
      ;; provisional claim; the post-publish liveness check closes the narrow
      ;; case where the sentinel ran after registration but before publication.
      (puthash key (cons proc port) dorg-dolt-server--procs)
      (set-process-sentinel proc #'dorg-dolt-server--child-exit-sentinel)
      (dorg-dolt-server--publish-provisional-claim
       dolt-dir port (process-id proc) data-dir db-name (emacs-pid) 'emacs)
      (unless (process-live-p proc)
        (remhash key dorg-dolt-server--procs)
        (dorg-dolt-server--release-claim dolt-dir)
        (error "DOrg dolt-server: Dolt sql-server exited before readiness began on port %d"
               port))
      (condition-case err
          (progn
            (dorg-dolt-server--wait-ready port nil proc)
            ;; From here the child's death is ordinary, not a start failure,
            ;; so its sentinel no longer snapshots the lifetime log.
            (process-put proc 'dorg-server-ready t))
        (error
         (let ((status (process-status proc))
               (exit (process-exit-status proc))
               (output nil))
           (dorg-dolt-server--kill-process proc)
           ;; Claim deletion is death-gated.  A kill seam that cannot prove the
           ;; child gone leaves both the retained process entry and the
           ;; blocking provisional claim for peers to see.
           (unless (process-live-p proc)
             (remhash key dorg-dolt-server--procs)
             (dorg-dolt-server--break-claim dolt-dir))
           ;; Diagnostics only after the lifecycle is settled; the read is
           ;; bounded and never signals.
           (setq output (or (process-get proc 'dorg-startup-output)
                            (dorg-dolt-server--read-log logfile)))
           (error (concat "dorg-dolt-server: failed to start dolt sql-server "
                          "for %s on port %d (status %s, exit %s): %s\n"
                          "Command: %s\n"
                          "sql-server output: %s")
                  (expand-file-name dolt-dir) port status exit
                  (error-message-string err)
                  (dorg-dolt--target-sql-server-command target port)
                  (dorg-dolt--trim-output output)))))
      ;; Fail closed: confirm the expected database before upgrading the
      ;; provisional ownership claim to ready.  The store CONTENT (schema marker,
      ;; bound file) was already validated pre-start against the dolt-dir;
      ;; here we only confirm the server we launched actually serves DB-NAME
      ;; (a wrong data-dir, accidental sibling repo, or renamed store would
      ;; not).  `SHOW DATABASES' is metadata-only, so this probe does not
      ;; touch the user database and cannot wedge the freshly-started server.
      ;; On failure, kill the process we started, drop any stale claim we own,
      ;; and signal — never falling back to CLI.
      ;; ONE served-list snapshot backs both the membership decision and the
      ;; diagnostic, so the refusal can never name the DB it just refused.
      ;; Tri-state served-DB confirmation (tracked-regression).  A CONFIRMED wrong
      ;; store => attempt to kill the child, deleting the claim only after
      ;; confirmed death.  An INCONCLUSIVE probe (transient/unconfirmable) keeps
      ;; the already-published provisional claim and retained proc; a later
      ;; `--ensure-strict' re-probes and finalizes the SAME server.
      (let ((probe (dorg-dolt-server--served-dbs-or-inconclusive port)))
        (cond
         ((eq (car probe) 'inconclusive)
          (signal 'dorg-sql-transport-error
                  (list (format (concat "dorg-dolt-server: sql-server for %s on port %d could not "
                                         "confirm it serves the expected database %S (%s); retained "
                                         "the server as a pending owner and published no ready claim")
                                (expand-file-name dolt-dir) port db-name (cdr probe)))))
         ((not (dorg-dolt-server--db-in-served-p (cdr probe) db-name))
          (dorg-dolt-server--kill-process proc)
          (unless (process-live-p proc)
            (remhash key dorg-dolt-server--procs)
            (dorg-dolt-server--break-claim dolt-dir))
          (signal 'dorg-sql-transport-error
                  (list (format (concat "dorg-dolt-server: sql-server for %s came up on port %d "
                                         "but does not serve the expected database %S "
                                         "(data-dir %s; served: %s); stopped it only if death "
                                         "was confirmed, otherwise retained its pending claim")
                                (expand-file-name dolt-dir) port db-name
                                (expand-file-name data-dir)
                                (let ((served (cdr probe)))
                                  (if served (mapconcat #'identity served ", ") "none"))))))))
      ;; Success upgrades the exact provisional claim in place.  No break or
      ;; second publication can reopen an untracked ownership window.
      (dorg-dolt-server--finalize-pending-claim dolt-dir 'ready)
      port)))

(defun dorg-dolt-server--start-unsafe (dolt-dir)
  "Start a dolt sql-server for DOLT-DIR and publish its claim, return port.
Requires the pre-start DOrg schema/content validation used by public
server starts."
  (dorg-dolt-server--start-child-unsafe dolt-dir t))

(defun dorg-dolt-server--start-bootstrap-unsafe (dolt-dir)
  "Start a bootstrap sql-server for fresh DOLT-DIR and publish its claim.
Skips only the pre-start DOrg schema/content validation; all post-launch
checks and claim sequencing are shared with `dorg-dolt-server--start-unsafe'."
  (dorg-dolt-server--start-child-unsafe dolt-dir nil))

;;; ─── Detached (daemon) Start ─────────────────────────────────────────────
;;
;; `dorg-dolt-server--start-unsafe' launches an Emacs-CHILD server: it lives in
;; `dorg-dolt-server--procs', is reaped by `kill-emacs-hook', and releases its
;; claim via a death sentinel — correct for a session-scoped server an
;; interactive Emacs owns, but useless for the standalone `dorg dolt start'
;; command, whose Emacs exits the instant it returns.  The detached path
;; below launches the same (io3e8) sql-server argv via a `nohup ... &'
;; launcher shell that exits immediately, reparenting dolt away from this
;; Emacs so it SURVIVES.  No `dorg-dolt-server--procs' entry and no sentinel; a
;; durable `:owner-kind daemon' claim (owner-pid = the daemon's own pid)
;; carries persistence + cross-process discovery.

(defun dorg-dolt-server--detached-launch (target port pidfile logfile)
  "Launch a detached `dolt sql-server' for TARGET on PORT; return its pid.
Runs the sql-server built from `dorg-dolt--target-sql-server-args' (io3e8
argv: `--data-dir' the store, `--doltcfg-dir' pinned inside it, so the
parent stays clean) under `nohup', backgrounded, with output to LOGFILE
and stdin from /dev/null, then writes the backgrounded pid to PIDFILE.
The launcher shell exits at once, so the sql-server is reparented away
from this Emacs and outlives it.  Returns the pid read back from PIDFILE,
or nil when none was written (launch failure)."
  (let* ((dolt (or (executable-find "dolt") "dolt"))
         (cmd (mapconcat #'shell-quote-argument
                         (cons dolt (dorg-dolt--target-sql-server-args target port))
                         " "))
         (script (format "nohup %s >%s 2>&1 </dev/null & printf '%%s' \"$!\" >%s"
                         cmd
                         (shell-quote-argument logfile)
                         (shell-quote-argument pidfile))))
    (ignore-errors (delete-file pidfile))
    (call-process "sh" nil nil nil "-c" script)
    (when (file-exists-p pidfile)
      (with-temp-buffer
        (insert-file-contents pidfile)
        (let ((s (string-trim (buffer-string))))
          (and (string-match-p "\\`[0-9]+\\'" s) (string-to-number s)))))))

(defun dorg-dolt-server--start-detached-unsafe (dolt-dir)
  "Start a DETACHED `dolt sql-server' for DOLT-DIR; return its port.
Low-level primitive: call only under `dorg-dolt-server--with-claim-lock' after
establishing that no live server already owns DOLT-DIR.  Unlike
`dorg-dolt-server--start-unsafe' the server is detached (survives this
process), is NOT registered in `dorg-dolt-server--procs', and gets NO death
sentinel — so neither this process's exit nor `kill-emacs-hook' reaps it.
Persistence and cross-process discovery come from the durable daemon
claim it publishes.

Fails closed, with TWO distinct failure dispositions (tracked-regression,
tracked-regression):

- DESTRUCTIVE (confirmed failure): a pre-start store-invalid verdict, a
  launch that yields no pid, a daemon that never becomes ready, or a
  CONFIRMED wrong served store kills the daemon (if it came up), drops any
  stale owned claim (`--break-claim'), and signals `dorg-sql-transport-error'
  — no ready claim is ever published.

- RETAINED / BLOCKING (inconclusive or unfinalized identity): an
  INCONCLUSIVE served-DB probe, or a served DB confirmed but a dolt-native
  `sql-server.info' identity that is absent/malformed/mismatched, leaves the
  (possibly-healthy) detached daemon ALIVE and publishes a durable
  NON-ready `:state probe-inconclusive' daemon claim carrying its exact
  pid/port (no uuid), then signals `dorg-sql-transport-error'.  Such a claim
  BLOCKS peers (never attaches-as-ready or replaces) and is finalized to
  `:state ready' by a later `--ensure-detached' re-probe once served-DB and
  native identity both agree — reusing the SAME daemon, zero relaunch.

A `:state ready' claim is published ONLY when served-DB AND the native
4-tuple identity are confirmed, so the verified-identity stop gate can
always re-verify it — never a half-claimed or misleading/unstoppable
ready server."
  (let* ((target (dorg-dolt--target dolt-dir))
         (db-name (plist-get target :db-name))
         (binding (dorg-dolt-server--expected-binding dolt-dir))
         (verdict (dorg-dolt-server--dir-store-valid-p dolt-dir binding)))
    ;; This is the daemon counterpart of `--start-child-unsafe': both are
    ;; invoked under the claim lock and neither may spawn before the strict
    ;; store-scoped owner census succeeds.
    (dorg-dolt-server--assert-admission-clear dolt-dir)
    (unless (eq verdict t)
      (dorg-dolt-server--break-claim dolt-dir)
      (signal 'dorg-sql-transport-error
              (list (concat (dorg-dolt-server--store-invalid-message
                             verdict dolt-dir db-name)
                            "; refused to start a detached server and published no claim"))))
    (let* ((port (dorg-dolt-server--free-port))
           (data-dir (plist-get target :server-data-dir))
           (dorg-dir (dorg-dolt-server--ensure-dorg-dir dolt-dir))
           (pidfile (expand-file-name "server.pid" dorg-dir))
           (logfile (expand-file-name "server.log" dorg-dir))
           (read-log (lambda () (dorg-dolt-server--read-log logfile))))
      (let ((pid (dorg-dolt-server--detached-launch target port pidfile logfile)))
        (unless (and (integerp pid) (> pid 0))
          (dorg-dolt-server--break-claim dolt-dir)
          (signal 'dorg-sql-transport-error
                  (list (format "dorg-dolt-server: failed to launch a detached dolt sql-server for %s (no pid); see %s"
                                (expand-file-name dolt-dir) logfile))))
        ;; The detached child is now independently live-capable and has no
        ;; Emacs process object/sentinel.  Publish its blocking owner before the
        ;; first readiness or identity observation so a launcher crash cannot
        ;; leave an untracked daemon.
        (dorg-dolt-server--publish-provisional-claim
         dolt-dir port pid data-dir db-name pid 'daemon)
        ;; Wait until the detached server LISTENS, aborting early if its pid dies.
        (let ((deadline (+ (float-time) dorg-dolt-server-startup-timeout))
              (ready nil))
          (while (and (not ready)
                      (< (float-time) deadline)
                      (eq (dorg-dolt-server--pid-live-p pid) t))
            (if (dorg-dolt-server--port-live-p port)
                (setq ready t)
              (accept-process-output nil 0.1)))
          (unless ready
            (let ((output (funcall read-log)))
              (when (dorg-dolt-server--kill-pid
                     pid dorg-dolt-server-stop-term-grace)
                (dorg-dolt-server--break-claim dolt-dir))
              (signal 'dorg-sql-transport-error
                      (list (format (concat "dorg-dolt-server: detached dolt sql-server for %s did not "
                                            "become ready on port %d (pid %s); removed its claim only "
                                            "if death was confirmed, otherwise retained the pending "
                                            "owner.\nCommand: %s\nOutput: %s")
                                    (expand-file-name dolt-dir) port pid
                                    (dorg-dolt--target-sql-server-command target port)
                                    (dorg-dolt--trim-output output)))))))
        ;; Confirm the expected DB is served (metadata-only; cannot wedge it)
        ;; before upgrading the provisional claim to ready.
        ;; ONE served-list snapshot backs both the membership decision and the
        ;; diagnostic, so the refusal can never name the DB it just refused.
        ;; Tri-state served-DB confirmation (tracked-regression).  A CONFIRMED wrong
        ;; store => kill the daemon pid + break the claim after confirmed death.
        ;; An INCONCLUSIVE probe
        ;; (transient/unconfirmable) must NOT kill a possibly-healthy detached
        ;; daemon nor leave it untracked: a detached daemon is NOT in `--procs',
        ;; so retain the durable `:state probe-inconclusive' daemon claim already
        ;; carrying its exact pid/port, then raise — a later `--ensure-detached'
        ;; re-probes and finalizes the SAME daemon (no kill/relaunch).
        (let ((probe (dorg-dolt-server--served-dbs-or-inconclusive port)))
          (cond
           ((eq (car probe) 'inconclusive)
            (signal 'dorg-sql-transport-error
                    (list (format (concat "dorg-dolt-server: detached sql-server for %s on port %d could "
                                          "not confirm it serves the expected database %S (%s); retained "
                                          "it as a pending daemon owner and published no ready claim")
                                  (expand-file-name dolt-dir) port db-name (cdr probe)))))
           ((not (dorg-dolt-server--db-in-served-p (cdr probe) db-name))
            (when (dorg-dolt-server--kill-pid
                   pid dorg-dolt-server-stop-term-grace)
              (dorg-dolt-server--break-claim dolt-dir))
            (signal 'dorg-sql-transport-error
                    (list (format (concat "dorg-dolt-server: detached sql-server for %s came up on port %d "
                                          "but does not serve the expected database %S (served: %s); "
                                          "removed its claim only if death was confirmed, otherwise "
                                          "retained the pending owner")
                                  (expand-file-name dolt-dir) port db-name
                                  (let ((served (cdr probe)))
                                    (if served (mapconcat #'identity served ", ") "none"))))))
           (t
            ;; Confirmed served: publish the durable READY daemon claim — but
            ;; ONLY after the dolt-native `sql-server.info' identity is present
            ;; and its pid/port/uuid MATCH the launched daemon (tracked-regression
            ;; ACCEPT-REJECT reviewer 2026-07-17 Blocker 2).  owner-pid = the
            ;; daemon pid (so a separate process classifies by the daemon's own
            ;; liveness) and owner-kind `daemon' (so a cross-process `dorg dolt
            ;; stop' may reap it).  Persisting the native :uuid AND `:state
            ;; ready' lets a later cross-process stop re-verify the 4-tuple
            ;; identity before it signals; a reused PID with a fresh UUID then
            ;; fails the match and is never signalled.
            ;;
            ;; If native identity is absent/malformed/mismatched, a `:state
            ;; ready' claim would carry a nil/wrong uuid that
            ;; `--verified-daemon-pid' rejects forever — a misleading, UNSTOPPABLE
            ;; ready claim.  Fail closed instead: retain a NON-ready
            ;; (probe-inconclusive) blocking daemon claim carrying the exact
            ;; pid/port (so peers block, never attach-as-ready or replace) and
            ;; raise.  A later `--ensure-detached' re-probe re-attempts
            ;; finalization once native identity agrees.
            (let ((uuid (dorg-dolt-server--native-uuid-for dolt-dir pid port)))
              (if uuid
                  (progn
                    (dorg-dolt-server--finalize-pending-claim
                     dolt-dir 'ready uuid)
                    port)
                (signal 'dorg-sql-transport-error
                        (list (format (concat "dorg-dolt-server: detached sql-server for %s came up and serves "
                                              "%S on port %d, but its dolt-native sql-server.info identity "
                                              "(pid/port/uuid) is absent or does not match the launched daemon "
                                              "(pid %s); retained a pending daemon claim and published no ready "
                                              "claim so a later stop is never misled")
                                      (expand-file-name dolt-dir) db-name port pid))))))))))))

(defun dorg-dolt-server--signal-no-autostart (dolt-dir)
  "Signal `dorg-sql-transport-error' for DOLT-DIR: no server, autostart off.
Called only from the attach-only (`dorg-dolt-server--attach-only') path,
before any process is started, restarted, reaped, or claim/`sql-server.info'
metadata is written or broken."
  (signal 'dorg-sql-transport-error
          (list (format "dorg: no usable dolt sql-server for %s and implicit autostart is disabled; run `dorg dolt start %s' or enable `dorg-dolt-server-autostart'"
                        dolt-dir dolt-dir))))

(defun dorg-dolt-server--attach-only (dolt-dir)
  "Attach to an already-live, validated dolt sql-server for DOLT-DIR.
The sole hot-path selector used by `dorg-dolt-server--strict-port' when
`dorg-dolt-server-autostart' is nil, for every visible owner kind (daemon,
dolt-native, or another Emacs session's).  Runs under the claim lock so a
concurrent claim mutation cannot race the check-then-attach decision.  May
only return the port of a claim that classifies `live' and passes the same
protocol/UUID/store validation `dorg-dolt-server--ensure-strict' requires
before trusting a foreign server.  A live `daemon' or foreign owner is
attached even when this Emacs started the daemon (its `emacs'-independent
claim is not stale local state); every other outcome — no claim, a
session-owned (`emacs') claim whose local server is gone, a wedged, dead,
or stale claim, or an unreachable live owner — signals
`dorg-sql-transport-error' via `dorg-dolt-server--signal-no-autostart' (or,
for an unreachable owner, the existing fail-closed unreachable message).
Never starts, restarts, reaps, or breaks a claim."
  (dorg-dolt-server--with-claim-lock dolt-dir
    (let ((local-port (dorg-dolt-server-port dolt-dir)))
      (cond
       ;; A server this Emacs process owns is attach-eligible only while it
       ;; still answers SQL; a wedged owned server must not be reaped here.
       ((and local-port (dorg-dolt-server--port-healthy-p local-port))
        ;; A PENDING owned child (retained after a post-start INCONCLUSIVE
        ;; probe) is NOT attach-as-ready: route it through the SAME tri-state
        ;; revalidation (finalize on served, stay-pending + raise on
        ;; inconclusive) rather than overwriting its pending marker with a
        ;; plain claim and hot-serving it (tracked-regression nav-vx1 BF1).
        ;; attach-only never starts, so a confirmed wrong store fails closed.
        (if (dorg-dolt-server--owned-pending-p dolt-dir)
            (dorg-dolt-server--recover-owned-pending dolt-dir local-port)
          (dorg-dolt-server--publish-claim dolt-dir local-port)
          local-port))
       (local-port
        (dorg-dolt-server--signal-no-autostart dolt-dir))
       (t
        (let ((claim (dorg-dolt-server--effective-claim dolt-dir)))
          (if (null claim)
              (dorg-dolt-server--signal-no-autostart dolt-dir)
            (dorg-dolt-server--assert-claim-not-foreign-store dolt-dir claim)
            (pcase (dorg-dolt-server--claim-classify claim t)
              ('live
               (cond
                ;; Our token for a SESSION-scoped (`emacs') server, but no
                ;; live local proc (else `local-port' above would have caught
                ;; it): that owned server died and left a stale token; local
                ;; state is stale and attach-only never starts a replacement.
                ;; A `daemon' we started is an independent live process (never
                ;; in this Emacs's proc table, so `local-port' is nil), so it
                ;; is NOT stale local state — it falls through to the ordinary
                ;; live-compatible attach below, exactly as a foreign owner
                ;; would.  Gating this on `emacs' keeps session-owned stale
                ;; protection without rejecting an attachable daemon.
                ((and (dorg-dolt-server--claim-owned-here-p claim)
                      (eq (alist-get :owner-kind claim) 'emacs))
                 (dorg-dolt-server--signal-no-autostart dolt-dir))
                ((dorg-dolt-server--claim-compatible-p claim)
                 ;; Same fail-closed live-attach heart as `--ensure-strict':
                 ;; UUID guard, expected-store validation, memoized success,
                 ;; hard error (no attach, no CLI fallback) on a wrong store.
                 (dorg-dolt-server--attach-live
                  dolt-dir claim :check-uuid t
                  :on-invalid #'dorg-dolt-server--signal-store-invalid))
                (t
                 (signal 'dorg-sql-transport-error
                         (list (format "dorg: a DOrg sql-server owns %s but its claim is incompatible (protocol %s); refusing to start a second server or use CLI"
                                       dolt-dir (alist-get :protocol claim)))))))
              ;; A pending (unfinalized) owner: its served DB is not yet
              ;; confirmed, so a peer must NOT attach-as-ready, probe, start,
              ;; reap, or break it — block and raise (tracked-regression).
              ('pending
               (signal 'dorg-sql-transport-error
                       (list (format "dorg: the DOrg sql-server owning %s (pid %s, port %s) has not confirmed its served database (pending); refusing to attach-as-ready, replace, or reap it"
                                     dolt-dir (alist-get :owner-pid claim)
                                     (alist-get :port claim)))))
              ((or 'wedged 'dead)
               (dorg-dolt-server--signal-no-autostart dolt-dir))
              ('unreachable
               (signal 'dorg-sql-transport-error
                       (list (format "dorg: the DOrg sql-server owning %s (pid %s, port %s) is unreachable; strict `server' transport refuses CLI fallback"
                                     dolt-dir (alist-get :owner-pid claim)
                                     (alist-get :port claim)))))))))))))

(defun dorg-dolt-server--owned-child-authority-p (dolt-dir entry)
  "Return non-nil when the RAW lock-held claim proves ENTRY is THIS process's child.
ENTRY is a `dorg-dolt-server--procs' value, (PROC . PORT).

Every field must agree; an absent claim or any single mismatch is a refusal.
The raw overlay claim is read rather than the native-reconciled effective one,
because reconciliation can present a claim this process did not write."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (claim (dorg-dolt-server--claim-read dolt-dir))
         (proc (car entry))
         (port (cdr entry)))
    (and claim
         (eq (alist-get :owner-kind claim) 'emacs)
         (dorg-dolt-server--claim-host-local-p claim)
         (equal (alist-get :dolt-dir claim) canon)
         (eql (alist-get :owner-pid claim) (emacs-pid))
         (dorg-dolt-server--claim-owned-here-p claim)
         (eql (alist-get :server-pid claim) (process-id proc))
         (eql (alist-get :port claim) port)
         t)))

(defun dorg-dolt-server--retire-owned-child-unsafe (dolt-dir)
  "Retire this process's session-owned child for DOLT-DIR.
PRECONDITION: call ONLY with the claim lock held, and only for an EXPLICIT
persistent start.

A session-owned child does not satisfy detached lifetime: it dies with this
process, so returning its port from an explicit start promises a server that
outlives the caller and delivers one that does not.

Ordering is the whole contract here.  Death is CONFIRMED first; only then is the
exact `dorg-dolt-server--procs' entry retired and the old claim token released;
only then may a daemon be published.  `dorg-dolt-server--child-exit-sentinel'
keys on exact PROC membership, so retiring the entry first makes any delayed
callback inert — it finds no entry `eq' to that process and therefore cannot
release the DAEMON's claim under the same key.  Retiring AFTER publication, or
publishing without confirmed death, would reintroduce exactly that hazard.

Refuses, changing nothing, when another owner holds the store or when the child
cannot be proven gone; the retained entry and its blocking claim are left for
peers to see rather than replaced."
  (let* ((key (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash key dorg-dolt-server--procs)))
    (when entry
      ;; The ownership question is asked ONLY when there is a local entry to
      ;; retire.  With no child of ours there is nothing to retire and the
      ;; ordinary claim classification below owns the decision.
      ;;
      ;; Authority is the RAW lock-held claim, and it must prove the COMPLETE
      ;; tuple.  An earlier revision refused only when a claim existed and its
      ;; token disagreed, so an ABSENT claim proceeded to kill and release a
      ;; process it could not prove was ours.  Token
      ;; equality alone is also not identity: it says nothing about which store,
      ;; host, owner or process the claim describes.
      (unless (dorg-dolt-server--owned-child-authority-p dolt-dir entry)
        (signal 'dorg-sql-transport-conflict
                (list (format "dorg: the lock-held claim for %s does not prove this process owns the registered child; refusing to retire it (zero kill, release, start or memoization)"
                              key))))
      (let ((proc (car entry)))
        (dorg-dolt-server--kill-process proc)
        (when (process-live-p proc)
          (signal 'dorg-sql-transport-error
                  (list (format "dorg: could not confirm the session-owned sql-server for %s exited; kept its process entry and claim rather than publishing a daemon over a possibly-live server"
                                key))))
        (remhash key dorg-dolt-server--procs)
        (dorg-dolt-server--release-claim dolt-dir)))
    t))

(defun dorg-dolt-server--assert-verified-detached (dolt-dir port)
  "Return the VERIFIED detached port for DOLT-DIR, or signal.

Return authority for an explicit persistent start.  It reuses the single
ready-daemon verifier rather than keeping a second, weaker copy, so the same
contract that governs cross-process kill authority governs what an explicit
start is allowed to report: finalized daemon owner-kind at ready state, this
host and store, claim and dolt-native records agreeing on server-pid, port and
a non-empty uuid, the port live, and served-store validation confirming THIS
store.

PORT is whatever the lifecycle arm produced.  It must EQUAL the verified port:
returning a lifecycle port that the durable records do not corroborate would
hand the caller a number no second process can be expected to use, which is the
whole failure this bead exists to close."
  (let ((identity (dorg-dolt-server--verified-daemon-identity dolt-dir)))
    (unless identity
      (signal 'dorg-sql-transport-error
              (list (format "dorg: explicit persistent start for %s could not verify a ready detached daemon identity (owner-kind, state, host, store, pid/port/uuid agreement, live port and served-store validation); refusing to report a server a second process may be unable to attach"
                            (dorg-dolt-server--canonical-dir dolt-dir)))))
    (let ((verified (plist-get identity :port)))
      (unless (eql verified port)
        (signal 'dorg-sql-transport-error
                (list (format "dorg: explicit persistent start for %s produced port %s but the verified durable identity reports port %s; refusing to return or memoize an unverified port"
                              (dorg-dolt-server--canonical-dir dolt-dir)
                              port verified))))
      verified)))

(defun dorg-dolt-server--ensure-detached (dolt-dir &optional explicit)
  "Ensure a DETACHED, persistent `dolt sql-server' for DOLT-DIR; return port.
The explicit-start entry behind `dorg dolt start' / `dorg-dolt-start':
the server OUTLIVES the starting process, unlike the session-scoped
`dorg-dolt-server--ensure-strict'.  Under the claim lock it reuses a server this Emacs
already owns, reuses a healthy claim already serving the expected store
\(idempotent), reaps a wedged one, breaks a dead/stale/wrong claim, then
starts a fresh detached daemon.  Refuses to start a duplicate over an
unreachable (possibly still-booting) owner.  Returns only after the
server is confirmed listening and claimed.

With EXPLICIT non-nil the caller is asking for the documented DETACHED promise
rather than for any usable port.  A port this Emacs owns then does NOT satisfy
the request: the session-owned child is retired under the lock (see
`dorg-dolt-server--retire-owned-child-unsafe') before a daemon is published, and
the return is gated on `dorg-dolt-server--assert-verified-detached'.  EXPLICIT
defaults to nil, so the implicit autostart caller in
`dorg-dolt-server--strict-port' keeps its existing behaviour exactly, and a
future caller cannot acquire conversion authority by accident."
  (dorg-dolt--ensure-available)
  (or (and (not explicit) (dorg-dolt-server-port dolt-dir))
      (dorg-dolt-server--with-claim-lock dolt-dir
        (or (and (not explicit) (dorg-dolt-server-port dolt-dir))
            (let* ((_ (when explicit
                        (dorg-dolt-server--retire-owned-child-unsafe dolt-dir)))
                   (claim (dorg-dolt-server--effective-claim dolt-dir))
                   (port
                    (if (null claim)
                        (dorg-dolt-server--start-detached-unsafe dolt-dir)
                      (dorg-dolt-server--assert-claim-not-foreign-store dolt-dir claim)
                      (pcase (dorg-dolt-server--claim-classify claim t)
                        ('live
                         ;; Foreign live daemon (owned/pending/wedged owners are
                         ;; handled by their own arms).  Attach on a valid store;
                         ;; a CONFIRMED wrong store breaks the claim and starts a
                         ;; replacement DETACHED daemon, while an INCONCLUSIVE
                         ;; probe preserves the live daemon + claim and raises
                         ;; (tracked-regression — never break+replace on a transient
                         ;; failure).  The success port is memoized by the outer
                         ;; memo below; `--attach-live' also memoizes it (an
                         ;; idempotent same-key re-put).
                         (dorg-dolt-server--attach-live
                          dolt-dir claim
                          :on-invalid
                          (lambda (verdict dolt-dir db-name port)
                            (dorg-dolt-server--attach-invalid-replace
                             verdict dolt-dir db-name port
                             #'dorg-dolt-server--start-detached-unsafe
                             "; preserved the live detached daemon and its claim"))))
                        ;; A pending daemon is recoverable only by the process
                        ;; whose token published it.  A peer must block without
                        ;; probing or mutation; the owner re-probes exactly once
                        ;; and finalizes the SAME claim on confirmation.
                        ('pending
                         (if (not (dorg-dolt-server--claim-owned-here-p claim))
                             (signal 'dorg-sql-transport-error
                                     (list (format "dorg: the DOrg sql-server owning %s (pid %s, port %s) is pending under another process; refusing to probe, attach-as-ready, or replace it"
                                                   dolt-dir
                                                   (alist-get :owner-pid claim)
                                                   (alist-get :port claim))))
                           (let* ((port (alist-get :port claim))
                                  (server-pid (alist-get :server-pid claim))
                                  (db-name (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))
                                  (binding (dorg-dolt-server--expected-binding dolt-dir))
                                  (verdict (dorg-dolt-server--store-valid-p port db-name binding)))
                             (cond
                              ((eq verdict t)
                               ;; Served DB confirmed.  Finalize to `:state ready'
                               ;; ONLY after wiring the verified dolt-native instance
                               ;; UUID into the SAME claim (tracked-regression Blocker 2):
                               ;; a pending daemon carries no uuid, so a bare
                               ;; state-only finalize would leave a ready-but-nil-uuid
                               ;; claim that `--verified-daemon-pid' rejects forever.
                               (let ((uuid (dorg-dolt-server--native-uuid-for
                                            dolt-dir server-pid port)))
                                 (unless uuid
                                   (signal 'dorg-sql-transport-error
                                           (list (format (concat "dorg-dolt-server: served DB for %s confirmed on "
                                                                 "port %d, but the dolt-native sql-server.info "
                                                                 "identity (pid/port/uuid) is absent or does not "
                                                                 "match the pending daemon claim (pid %s); kept the "
                                                                 "pending claim and refused to finalize a ready "
                                                                 "claim a later stop could not verify")
                                                         (expand-file-name dolt-dir) port server-pid))))
                                 (dorg-dolt-server--finalize-pending-claim dolt-dir 'ready uuid)
                                 port))
                              ((eq (plist-get verdict :reason) 'probe-inconclusive)
                               (signal 'dorg-sql-transport-error
                                       (list (concat (dorg-dolt-server--store-invalid-message
                                                      verdict dolt-dir db-name port)
                                                     "; kept the pending detached daemon claim"))))
                              (t
                               (dorg-dolt-server--break-claim dolt-dir)
                               (dorg-dolt-server--start-detached-unsafe dolt-dir))))))
                        ;; A FOREIGN wedged daemon (TCP up, SQL dead) is NEVER
                        ;; reaped cross-process (tracked-regression): identity is a
                        ;; kill precondition and a wedged server cannot pass
                        ;; served-store validation, so a peer neither reaps nor
                        ;; replaces it — REFUSE (zero kill, zero break, zero
                        ;; replacement start).
                        ('wedged
                         (dorg-dolt-server--signal-foreign-wedged claim dolt-dir))
                        ('dead
                         (dorg-dolt-server--break-claim dolt-dir)
                         (dorg-dolt-server--start-detached-unsafe dolt-dir))
                        ('unreachable
                         (signal 'dorg-sql-transport-conflict
                                 (list (format "dorg: a DOrg sql-server (pid %s, port %s) owns %s but is unreachable; refusing to start a second server over a possibly-live owner"
                                               (alist-get :owner-pid claim)
                                               (alist-get :port claim) dolt-dir))))))))
              ;; Memoize the daemon's port so the hot query path
              ;; (`dorg-dolt-server--strict-port') stops re-walking claim
              ;; discovery — lock, claim reads, probe, store validation —
              ;; on EVERY `dorg-sql' call (tracked-regression).  The daemon is a
              ;; foreign detached server, so `--attached-ports' is its
              ;; correct home; `--break-claim' / `--release-claim' already
              ;; drop the entry when the claim lifecycle invalidates it.
              (dorg-dolt-server--memo-attached-port
               dolt-dir
               (if explicit
                   (dorg-dolt-server--assert-verified-detached dolt-dir port)
                 port)))))))

(defun dorg-dolt-server--kill-pid (pid &optional grace)
  "Escalate SIGTERM->SIGKILL on raw PID until it exits or GRACE elapses.
For a foreign sql-server reaped from a claim's `:server-pid' there is no
Emacs process object to drain, so liveness is polled via
`dorg-dolt-server--pid-live-p'.  GRACE defaults to `dorg-dolt-server-stop-term-grace'.
A TERM-ignoring server (as in the 2026-06-24 incident) survives the grace
window and is then SIGKILLed.  Returns t when the pid is confirmed gone,
nil if it somehow outlives SIGKILL or cannot be enumerated."
  (when (integerp pid)
    (ignore-errors (signal-process pid 'SIGTERM))
    (let ((deadline (+ (float-time) (or grace dorg-dolt-server-stop-term-grace))))
      (while (and (eq (dorg-dolt-server--pid-live-p pid) t)
                  (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (when (eq (dorg-dolt-server--pid-live-p pid) t)
      (ignore-errors (signal-process pid 'SIGKILL))
      (accept-process-output nil 0.2))
    ;; Confirmed death ONLY: return t iff liveness is exactly nil (pid
    ;; enumerably absent).  An `unknown' verdict (enumeration unsupported)
    ;; is NOT proof of death and must return nil so the stop path preserves
    ;; the claim instead of deleting it after an unconfirmed signal.
    (null (dorg-dolt-server--pid-live-p pid))))

(defun dorg-dolt-server--kill-process (proc &optional grace)
  "Kill PROC reliably: SIGTERM, drain output, then SIGKILL if needed.
GRACE seconds (default `dorg-dolt-server-stop-term-grace') bounds the wait for
a clean TERM exit before escalating to SIGKILL — a wedged Dolt server can
ignore SIGTERM.  A network process (no OS pid, e.g. a bare listener) has
no signal to send and is removed directly with `delete-process'."
  (when (process-live-p proc)
    (if (not (process-id proc))
        (delete-process proc)
      (signal-process proc 'SIGTERM)
      ;; Drain output so Emacs doesn't block on a full pipe
      (let ((deadline (+ (float-time) (or grace dorg-dolt-server-stop-term-grace))))
        (while (and (process-live-p proc)
                    (< (float-time) deadline))
          (accept-process-output proc 0.1 nil t)))
      (when (process-live-p proc)
        (signal-process proc 'SIGKILL)
        (accept-process-output proc 0.1 nil t))))
  (when (buffer-live-p (process-buffer proc))
    (kill-buffer (process-buffer proc))))

(defun dorg-dolt-server--stop (dolt-dir)
  "Stop the dolt sql-server for DOLT-DIR.
Also closes any persistent mysql connection for this directory and
releases this process's ownership claim (token-checked).  An attach-only
client — no managed process for DOLT-DIR — closes only its connection
and, because it owns no token, leaves the owner's claim intact."
  (dorg-sql--close-connection dolt-dir)
  (let* ((key (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash key dorg-dolt-server--procs)))
    (when entry
      (dorg-dolt-server--kill-process (car entry))
      (remhash key dorg-dolt-server--procs))
    (dorg-dolt-server--release-claim dolt-dir)))

(defun dorg-dolt-server-stop-all ()
  "Stop all managed dolt sql-server processes and connections.
Releases the ownership claim for every server this process owns."
  (dorg-sql--close-all-connections)
  (let ((dirs nil))
    (maphash (lambda (key entry)
               (push key dirs)
               (dorg-dolt-server--kill-process (car entry)))
             dorg-dolt-server--procs)
    (clrhash dorg-dolt-server--procs)
    (dolist (dir dirs)
      (dorg-dolt-server--release-claim dir)))
  (dorg-dolt-server--scrub-vanished-registry-dirs))

(defun dorg-dolt-server-port (dolt-dir)
  "Return the port for DOLT-DIR's server, or nil if not running.
Detects and cleans up dead processes."
  (let* ((key (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash key dorg-dolt-server--procs)))
    (when entry
      (if (process-live-p (car entry))
          (cdr entry)
        ;; Dead process — clean up
        (when (buffer-live-p (process-buffer (car entry)))
          (kill-buffer (process-buffer (car entry))))
        (remhash key dorg-dolt-server--procs)
        nil))))

(defun dorg-dolt-server-adopt-port (dolt-dir port)
  "Adopt an already-running server PORT for DOLT-DIR as a client attachment.
Registers PORT in the process-local attached-port cache
\(`dorg-dolt-server--attached-ports') so the hot SQL transport path
\(`dorg-dolt-server--strict-port') routes DOLT-DIR's queries to it
WITHOUT probing a claim or starting a server.

The caller does NOT own the server: adopting a port records only a CLIENT
attachment, never a `dorg-dolt-server--procs' owner entry, so process exit
closes only the mysql connection and never reaps the owner's server.

Intended for an async worker subprocess that inherits a parent-resolved
managed-server PORT and must route its own SQL through it, instead of forging
a private owner entry.  PORT must be an integer in the valid TCP port range
1..65535 (it is placed directly on the no-probe hot path, so an out-of-range
value is rejected rather than cached).  Returns PORT."
  (cl-check-type port (integer 1 65535))
  (dorg-dolt-server--memo-attached-port dolt-dir port))

;;; ─── Server Inventory (structured) ──────────────────────────────────────
;;
;; `dorg-dolt-server-list' is the public, read-only reporter over DOrg runtime
;; state: servers this Emacs owns (`dorg-dolt-server--procs'), foreign ports it
;; has attached to (`dorg-dolt-server--attached-ports'), stable discovery
;; pointers to store-relative DOrg overlays, and dolt-native
;; `sql-server.info'.  It deliberately does NOT grep arbitrary OS processes.
;; The interactive commands below render and act on this same data.

(defun dorg-dolt-server--entry-status (claim owned-live)
  "Classify a server entry from its CLAIM alist and OWNED-LIVE flag.
OWNED-LIVE is non-nil when this Emacs process runs a live server for the
repo.  Returns `live', `dead', `stale', `unreachable', `incompatible',
or nil when there is no claim to classify.  Reuses the existing claim
liveness/compatibility helpers rather than ad hoc probing."
  (cond
   (owned-live 'live)
   ((null claim) nil)
   (t (let ((class (dorg-dolt-server--claim-classify claim)))
        (if (and (eq class 'live)
                 (not (dorg-dolt-server--claim-compatible-p claim)))
            'incompatible
          class)))))

(defun dorg-dolt-server--entry-role (canon claim owned-live)
  "Return the ownership role for CANON given CLAIM and OWNED-LIVE.
One of `owned' (this Emacs runs or holds the claim token), `attached'
\(this Emacs is a client of a foreign owner), or `foreign' (another DOrg
process owns it)."
  (cond
   (owned-live 'owned)
   ((and claim (dorg-dolt-server--claim-owned-here-p claim)) 'owned)
   ((gethash canon dorg-dolt-server--attached-ports) 'attached)
   (t 'foreign)))

(defun dorg-dolt-server--describe (canon)
  "Return a plist describing the DOrg-managed server for CANON dolt-dir.
Merges this process's owned-proc state and attach state with the
store-relative overlay and dolt-native `sql-server.info' (via
`dorg-dolt-server--effective-claim').  See `dorg-dolt-server-list' for the key set."
  (let* ((claim (dorg-dolt-server--effective-claim canon))
         (proc-entry (gethash canon dorg-dolt-server--procs))
         (proc (car proc-entry))
         (owned-live (and proc-entry (process-live-p proc)))
         (attached-port (gethash canon dorg-dolt-server--attached-ports)))
    (list :dolt-dir canon
          :port (cond (owned-live (cdr proc-entry))
                      (attached-port attached-port)
                      (claim (alist-get :port claim)))
          :owner-pid (and claim (alist-get :owner-pid claim))
          :server-pid (cond (owned-live (process-id proc))
                            (claim (alist-get :server-pid claim)))
          :host (and claim (alist-get :host claim))
          :policy (and claim (alist-get :policy claim))
          :protocol (and claim (alist-get :protocol claim))
          :created-at (and claim (alist-get :created-at claim))
          :last-used (and claim (alist-get :last-used claim))
          :generation (dorg-sql--generation-fingerprint canon)
          :role (dorg-dolt-server--entry-role canon claim owned-live)
          :status (dorg-dolt-server--entry-status claim owned-live))))

(defun dorg-dolt-server--registry-store-dirs ()
  "Return canonical dolt-dirs known to the dorg registry (open documents).
Since the ownership overlay is now store-relative (no central scan dir),
whole-host enumeration draws the candidate stores from the dorg registry
of registered/open documents.  Defensive: returns nil when `dorg-registry'
is not loaded (e.g. pure CLI server ops), so `dorg-dolt-server-list' degrades
to this process's own and legacy state."
  (when (fboundp 'dorg-registered-dolt-dir)
    (let (dirs)
      (dolist (buf (buffer-list) dirs)
        (let ((dd (ignore-errors (dorg-registered-dolt-dir buf))))
          (when dd
            (push (dorg-dolt-server--canonical-dir dd) dirs)))))))

(defun dorg-dolt-server-list ()
  "Return structured info for all DOrg-managed sql-servers known here.
Enumerates candidate stores from the dorg registry of open documents
\(`dorg-dolt-server--registry-store-dirs'), the servers this Emacs owns
\(`dorg-dolt-server--procs'), the stable cross-process discovery index, and
ports it has attached
\(`dorg-dolt-server--attached-ports'); each store is described via
`dorg-dolt-server--describe', which reconciles the store-relative overlay with
dolt-native `sql-server.info'.  Each element is a plist with keys
:dolt-dir (canonical), :port, :owner-pid, :server-pid, :host, :policy,
:protocol, :created-at, :last-used, :generation, :role, and :status.

Purely a reader: it never starts, stops, or mutates any server or claim.
Stores with no running/claimed server (`:status' nil) are omitted."
  (let ((dirs (make-hash-table :test 'equal)))
    (dolist (d (dorg-dolt-server--registry-store-dirs)) (puthash d t dirs))
    (dolist (d (dorg-dolt-server--indexed-store-dirs)) (puthash d t dirs))
    (maphash (lambda (k _v) (puthash k t dirs)) dorg-dolt-server--procs)
    (maphash (lambda (k _v) (puthash k t dirs)) dorg-dolt-server--attached-ports)
    (let (result)
      (maphash (lambda (canon _)
                 (let ((entry (dorg-dolt-server--describe canon)))
                   ;; Omit a candidate store that has no server/claim at all.
                   (when (plist-get entry :status)
                     (push entry result))))
               dirs)
      (sort result (lambda (a b)
                     (string< (or (plist-get a :dolt-dir) "")
                              (or (plist-get b :dolt-dir) "")))))))

;;; ─── Interactive Server Management ───────────────────────────────────────
;;
;; Discoverable user commands over the lifecycle/claim machinery above.
;; Naming mirrors Emacs' own `buffer-list' (data) vs `list-buffers'
;; (command) split: `dorg-dolt-server-list' returns data, `dorg-dolt-list'
;; shows it.  These intentionally start a server when asked (an explicit
;; user action).

(defconst dorg-dolt-server-list-buffer-name "*DOrg Servers*"
  "Name of the buffer used by `dorg-dolt-list'.")

(defun dorg-dolt-server--read-dolt-dir (prompt)
  "Resolve a target Dolt dir: current buffer's registered repo, else prompt.
PROMPT is used when the current buffer has no registered Dolt backend."
  (or (and (fboundp 'dorg-registered-dolt-dir) (dorg-registered-dolt-dir))
      (read-directory-name prompt)))

(defun dorg-dolt-server--display-dir (dolt-dir)
  "Return a compact display string for DOLT-DIR."
  (abbreviate-file-name (directory-file-name (expand-file-name dolt-dir))))

(defun dorg-dolt-server--fmt (val)
  "Format VAL as a short table cell, using an em dash for nil."
  (cond ((null val) "—")
        ((stringp val) val)
        ((numberp val) (number-to-string val))
        ((symbolp val) (symbol-name val))
        (t (format "%s" val))))

(defun dorg-dolt-server--fmt-time (val)
  "Format a float-time VAL as a short timestamp, or an em dash for nil."
  (if (numberp val)
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time val))
    "—"))

(defun dorg-dolt-server--list-entries ()
  "Build `tabulated-list-entries' rows from `dorg-dolt-server-list'."
  (mapcar
   (lambda (e)
     (let ((dir (plist-get e :dolt-dir)))
       (list dir
             (vector (dorg-dolt-server--display-dir dir)
                     (dorg-dolt-server--fmt (plist-get e :port))
                     (dorg-dolt-server--fmt (plist-get e :role))
                     (dorg-dolt-server--fmt (plist-get e :status))
                     (dorg-dolt-server--fmt (plist-get e :policy))
                     (dorg-dolt-server--fmt (plist-get e :protocol))
                     (dorg-dolt-server--fmt (plist-get e :owner-pid))
                     (dorg-dolt-server--fmt (plist-get e :server-pid))
                     (dorg-dolt-server--fmt (plist-get e :generation))
                     (dorg-dolt-server--fmt-time (plist-get e :last-used))))))
   (dorg-dolt-server-list)))

(define-derived-mode dorg-dolt-server-list-mode tabulated-list-mode "DOrg-Servers"
  "Major mode listing DOrg-managed Dolt sql-servers."
  (setq tabulated-list-format
        [("Dolt Dir" 40 t) ("Port" 6 t) ("Role" 9 t) ("Status" 12 t)
         ("Policy" 7 t) ("Pr" 3 t) ("Owner" 7 t) ("Server" 7 t) ("Generation" 36 t)
         ("Last Used" 16 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'dorg-dolt-server--list-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun dorg-dolt-list ()
  "Display DOrg-managed Dolt sql-servers in a tabulated buffer.
Shows servers this Emacs owns, foreign ports it has attached to, and
same-host DOrg ownership claims classified live/dead/stale/unreachable/
incompatible.  External non-DOrg `dolt sql-server' processes are not
shown.  Returns the structured list from `dorg-dolt-server-list'."
  (interactive)
  (let ((data (dorg-dolt-server-list))
        (buf (get-buffer-create dorg-dolt-server-list-buffer-name)))
    (with-current-buffer buf
      (dorg-dolt-server-list-mode)
      (tabulated-list-print t))
    ;; Diagnostics: surface an oversized-store warning for each listed store on
    ;; this read-only path (never runs gc; honors `dorg-dolt-compact-warn-threshold').
    (dolist (entry data)
      (when-let ((dir (plist-get entry :dolt-dir)))
        (dorg-dolt-compact-check dir)))
    (when (called-interactively-p 'interactive)
      (if data
          (display-buffer buf)
        (message "dorg: no DOrg-managed sql-servers")))
    data))

;;;###autoload
(defun dorg-dolt-start (dolt-dir)
  "Ensure a DETACHED, persistent Dolt sql-server for DOLT-DIR; return its port.
Interactively defaults to the current buffer's registered Dolt repo and
otherwise prompts for a directory.  Uses `dorg-dolt-server--ensure-detached': an
EXPLICIT start launches a daemon that OUTLIVES the starting process (so a
standalone `dorg dolt start' actually leaves a warm server behind) and
is discoverable cross-process via its durable claim.  It reuses a healthy
server already serving the store rather than starting a second.  This is
distinct from the implicit, session-scoped `dorg-dolt-server--ensure-strict' used by
auto SQL transport, which is reaped when its Emacs exits."
  (interactive
   (list (dorg-dolt-server--read-dolt-dir "Start sql-server for Dolt dir: ")))
  (let ((port (dorg-dolt-server--ensure-detached dolt-dir t)))
    ;; Ensure an already-running daemon created before the discovery-index
    ;; contract is immediately visible to a subsequent CLI process too.
    (dorg-dolt-server--index-register dolt-dir)
    (when (called-interactively-p 'interactive)
      (message "dorg: sql-server for %s ready on port %d"
               (dorg-dolt-server--display-dir dolt-dir) port))
    port))

(defun dorg-dolt-server--verified-daemon-identity (dolt-dir)
  "Return the verified detached identity for DOLT-DIR as a plist, or nil.
The plist is (:server-pid INT :port INT :uuid STR), taken from ONE lock-held
read so a caller can return the VERIFIED port rather than whatever a lifecycle
arm happened to produce.

This is the single ready-daemon authority; `dorg-dolt-server--verified-daemon-pid'
is its pid projection.  It was factored out rather than copied because a
second, weaker verifier had drifted into accepting an `emacs'-owned,
probe-inconclusive claim as a detached identity.
The cross-process kill-authority gate: a PID is a transport handle, never
authority, so `dorg-dolt-stop' signals a detached daemon ONLY when this
predicate returns its pid.  PRECONDITION: call ONLY with the claim lock held
\(callers wrap in `dorg-dolt-server--with-claim-lock') so the reads below and
the caller's signal form ONE uninterrupted critical section — no unlocked
work between verify and actuate.

Reads the DOrg overlay claim (`dorg-dolt-server--claim-read', NOT the
native-reconciled effective claim) AND the dolt-native
`<store>/.dolt/sql-server.info' (`dorg-dolt-server--dolt-server-info') and
returns (:server-pid INT :port INT :uuid STR) IFF ALL hold:
- the claim is a finalized `:owner-kind daemon' with `:state ready' (a
  `pending'/`probe-inconclusive'/arbitrary/absent state is NOT a finalized
  identity and REFUSES, so an unfinalized daemon is never signalled);
- the claim was written by THIS host (`:host');
- the claim's `:dolt-dir' is THIS canonical store (canonical-store field);
- the native record is present and the full identity tuple agrees across
  both records — `:server-pid'==native pid, `:port'==native port, and a
  NON-EMPTY `:uuid'==native uuid (a reused PID with a fresh UUID fails);
- the claimed port is live AND served-store validation confirms THIS store.
Any absent/stale/inconclusive/mismatched/wedged field yields nil.  Pure
verification: it NEVER signals, breaks, or mutates a claim."
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (claim (dorg-dolt-server--claim-read dolt-dir))
         (native (dorg-dolt-server--dolt-server-info dolt-dir)))
    (and claim native
         (eq (alist-get :owner-kind claim) 'daemon)
         (eq (alist-get :state claim) 'ready)
         (dorg-dolt-server--claim-host-local-p claim)
         (equal (alist-get :dolt-dir claim) canon)
         (let ((cuuid (alist-get :uuid claim))
               (nuuid (plist-get native :uuid))
               (cpid (alist-get :server-pid claim))
               (npid (plist-get native :pid))
               (cport (alist-get :port claim))
               (nport (plist-get native :port)))
           (and (stringp cuuid) (not (string-empty-p cuuid))
                (equal cuuid nuuid)
                (integerp cpid) (eql cpid npid)
                (integerp cport) (eql cport nport)
                (dorg-dolt-server--port-live-p cport)
                (let ((db-name (cdr (dorg-dolt-server--data-dir-and-db dolt-dir)))
                      (binding (dorg-dolt-server--expected-binding dolt-dir)))
                  (eq (dorg-dolt-server--store-valid-p cport db-name binding) t))
                (list :server-pid cpid :port cport :uuid cuuid))))))

(defun dorg-dolt-server--verified-daemon-pid (dolt-dir)
  "Return the verified daemon server-pid for DOLT-DIR, or nil.
The pid projection of `dorg-dolt-server--verified-daemon-identity', which holds
the full contract: finalized `:owner-kind daemon' at `:state ready', this host,
this canonical store, claim and dolt-native records agreeing on server-pid,
port and a NON-EMPTY uuid, the port live, and served-store validation
confirming THIS store.  PRECONDITION unchanged: call ONLY with the claim lock
held.  Pure verification: it NEVER signals, breaks, or mutates a claim."
  (plist-get (dorg-dolt-server--verified-daemon-identity dolt-dir) :server-pid))

;;;###autoload
(defun dorg-dolt-stop (dolt-dir)
  "Stop or detach this Emacs's DOrg sql-server resources for DOLT-DIR.
Owner-safe:
- if this Emacs runs the server, stop the process and release this
  process's ownership claim (token-checked);
- if a host-local detached daemon holds DOLT-DIR, reap it ONLY through
  the verified-identity kill gate below — even when this process is also
  attached to it, since the hot query path memoizes daemon attaches
  (tracked-case) and an explicit stop must stop the daemon, not merely
  forget it;
- if this Emacs is only an attached client of a foreign Emacs-owned
  server, close the local mysql connection and forget the attach,
  leaving the owner's claim intact;
- if a live or unreachable sql-server owned by another DOrg process holds
  DOLT-DIR, refuse and report the owner pid/port rather than touching it.

Verified-identity kill gate (tracked-regression, OC option C).  A cross-process
TERM/KILL of a detached daemon is authorized ONLY inside ONE lock-held
critical section (`dorg-dolt-server--with-claim-lock') with NO unlocked
work between verify and actuate: under the held lock this re-reads BOTH
the DOrg daemon claim AND dolt-native `<store>/.dolt/sql-server.info',
requires the full 4-tuple {canonical-store, server-PID, port,
dolt-instance-UUID} to agree across both AND `:state ready', requires
served-store validation to confirm THIS db, then — still holding the lock
— signals, confirms death, and only then deletes the claim.  Any
absent/stale/inconclusive/mismatched field (wrong store, wedged/SQL-dead,
reused PID, non-`ready' `:state') REFUSES: zero signal, byte-identical
claim.  The claim is deleted ONLY after confirmed death; an unconfirmed
kill preserves it.

Accepted residual (v0.1, OC 2026-07-17): DOrg holds no persistent OS
handle on a detached daemon across CLI runs, so the sub-millisecond
window between the final under-lock `sql-server.info' read and `kill(2)'
cannot be closed without a supervisor/IPC layer (scoped out).  The
guarantee is therefore narrowed to \"never signal on stale/mismatched
claim/native METADATA\"; the metadata-fresh PID-reuse race in that window
is accepted for v0.1.

Interactively defaults to the current buffer's registered Dolt repo and
otherwise prompts.  Returns non-nil when local resources were released."
  (interactive
   (list (dorg-dolt-server--read-dolt-dir "Stop sql-server for Dolt dir: ")))
  (let* ((canon (dorg-dolt-server--canonical-dir dolt-dir))
         (owned-port (dorg-dolt-server-port dolt-dir))
         (attached-port (gethash canon dorg-dolt-server--attached-ports))
         (claim (dorg-dolt-server--effective-claim dolt-dir))
         (interactive-p (called-interactively-p 'interactive)))
    (cond
     ;; A live server this Emacs owns: stop it and release our claim.
     (owned-port
      (dorg-dolt-server--stop dolt-dir)
      (when interactive-p
        (message "dorg: stopped sql-server for %s (port %d); claim released"
                 (dorg-dolt-server--display-dir dolt-dir) owned-port))
      t)
     ;; A detached daemon (no live Emacs owner): an explicit `dorg dolt
     ;; stop' from ANY same-host process may reap it — but OWNER-SAFELY.  The
     ;; recorded `:server-pid' is signalled only when the claim is a live
     ;; server (its port is actually bound); for a port-down claim the pid may
     ;; have been reused by an unrelated process, so it is NEVER signalled.
     ;; This must precede BOTH the attach-only branch below — the hot query
     ;; path memoizes the daemon's port in `--attached-ports' (tracked-case),
     ;; and a mere detach would leave the daemon running against an explicit
     ;; stop — and the foreign-owner refusal, which would otherwise treat a
     ;; live daemon as an untouchable foreign Emacs server.
     ((and claim
           (eq (alist-get :owner-kind claim) 'daemon)
           (dorg-dolt-server--claim-host-local-p claim))
      (dorg-sql--close-connection dolt-dir)
      ;; Drop the attach memo in every daemon-stop outcome: it is a cache,
      ;; and after a reap (or refuse) the next resolve must re-walk discovery
      ;; rather than serve a possibly-dead port.
      (remhash canon dorg-dolt-server--attached-ports)
      ;; VERIFIED-IDENTITY kill (tracked-regression, OC option C).  The cross-process
      ;; TERM/KILL is authorized ONLY inside ONE lock-held critical section that
      ;; re-reads BOTH the DOrg daemon claim AND dolt-native `sql-server.info'
      ;; under the SAME held lock, verifies the full 4-tuple {canonical-store,
      ;; server-PID, port, dolt-instance-UUID} agrees across both AND that the
      ;; served store confirms this db, then — with NO unlocked work between
      ;; verify and actuate — signals, confirms death, and only then deletes the
      ;; claim.  Any absent/stale/inconclusive/mismatched field (wrong store,
      ;; wedged/SQL-dead, reused PID, unfinalized `:state') yields zero signal
      ;; and a byte-identical claim.  The residual OS PID-reuse window between
      ;; the final native read and `kill(2)' is not closable for a detached
      ;; process without a retained OS handle (scoped out, OC-accepted); the
      ;; guarantee is narrowed to "never signal on stale/mismatched metadata".
      (dorg-dolt-server--with-claim-lock dolt-dir
        (let ((pid (dorg-dolt-server--verified-daemon-pid dolt-dir)))
          (cond
           ;; Verified ready daemon: signal, confirm death, then delete the
           ;; claim.  Delete only once the kill is CONFIRMED, so a kill that
           ;; cannot be proven never orphans the claim from a still-live server.
           (pid
            (if (dorg-dolt-server--kill-pid pid dorg-dolt-server-stop-term-grace)
                (progn
                  (dorg-dolt-server--claim-delete-file dolt-dir)
                  (when interactive-p
                    (message "dorg: stopped detached sql-server for %s (pid %s); claim released"
                             (dorg-dolt-server--display-dir dolt-dir) pid))
                  t)
              (when interactive-p
                (message "dorg: could not confirm stop of detached sql-server for %s (pid %s); claim left intact"
                         (dorg-dolt-server--display-dir dolt-dir) pid))
              nil))
           ;; Identity NOT verified.  Distinguish a confirmed-dead daemon (port
           ;; down AND a same-host owner pid enumerably gone) — clear the stale
           ;; claim with ZERO signal, death already proven — from every other
           ;; unverified state (wrong-store, wedged, reused/mismatched PID,
           ;; unfinalized, port-down-but-pid-alive), which REFUSES with zero
           ;; signal and a byte-identical claim.
           (t
            (let ((cur (dorg-dolt-server--claim-read dolt-dir)))
              (cond
               ;; ROUTING REVALIDATION (tracked-regression ACCEPT-REJECT reviewer
               ;; 2026-07-17 Blocker 1).  The daemon branch was selected from the
               ;; PRE-LOCK `--effective-claim'; the claim lock protects only work
               ;; AFTER acquisition, not that stale routing decision.  The overlay
               ;; may have been replaced/removed between the routing read and this
               ;; lock-held read (e.g. by a newer foreign `emacs' claim).  Daemon
               ;; stale-cleanup must NEVER delete a claim that is no longer a
               ;; host-local daemon: preserve it BYTE-IDENTICALLY and refuse, so a
               ;; re-run routes on the now-current ownership.
               ((not (and cur
                          (eq (alist-get :owner-kind cur) 'daemon)
                          (dorg-dolt-server--claim-host-local-p cur)))
                (user-error
                 "dorg: refusing to stop detached sql-server for %s — the ownership claim changed under the lock (it is no longer a host-local daemon claim); left it intact, re-run `dorg dolt stop' to route on the current owner"
                 (dorg-dolt-server--display-dir dolt-dir)))
               ;; Still a host-local daemon AND confirmed dead: clear the stale claim.
               ((eq (dorg-dolt-server--claim-classify cur) 'dead)
                (dorg-dolt-server--claim-delete-file dolt-dir)
                (when interactive-p
                  (message "dorg: cleared stale detached-server claim for %s (daemon not running)"
                           (dorg-dolt-server--display-dir dolt-dir)))
                t)
               (t
                (user-error
                 "dorg: refusing to stop detached sql-server for %s (pid %s, port %s) — could not verify its identity under the claim lock (claim/native tuple mismatch, unconfirmed store, wedged, or a possibly-reused pid); left the claim intact"
                 (dorg-dolt-server--display-dir dolt-dir)
                 (alist-get :server-pid cur) (alist-get :port cur))))))))))
     ;; Attach-only client of a foreign Emacs-owned server: detach local
     ;; state, leave the owner's claim.  (A host-local daemon attach is
     ;; handled above — an explicit stop reaps the daemon, never detach-only.)
     (attached-port
      (dorg-dolt-server--stop dolt-dir)
      (when interactive-p
        (message "dorg: detached client for %s (port %d); owner claim left intact"
                 (dorg-dolt-server--display-dir dolt-dir) attached-port))
      t)
     ;; A raw/foreign dolt sql-server known only via dolt-native
     ;; `sql-server.info' (no DOrg overlay): DOrg did not start it and NEVER
     ;; signals it — attach-or-abandon only.  Refuse a live one explicitly so
     ;; the user stops it via dolt or its starter; a stale info file (port
     ;; down) falls through to best-effort cleanup.
     ((and claim
           (eq (alist-get :owner-kind claim) 'dolt)
           (memq (dorg-dolt-server--claim-classify claim) '(live unreachable)))
      (dorg-sql--close-connection dolt-dir)
      (user-error
       "dorg: refusing to stop sql-server for %s — it is a raw/foreign dolt sql-server (pid %s, port %s) DOrg does not own; stop it with `dolt' or the process that started it"
       (dorg-dolt-server--display-dir dolt-dir)
       (alist-get :owner-pid claim) (alist-get :port claim)))
     ;; A foreign DOrg process owns a live/unreachable server: never kill it.
     ((and claim
           (not (dorg-dolt-server--claim-owned-here-p claim))
           (memq (dorg-dolt-server--claim-classify claim) '(live unreachable)))
      (user-error
       "dorg: refusing to stop sql-server for %s — owned by another DOrg process (pid %s, port %s); stop it from that process"
       (dorg-dolt-server--display-dir dolt-dir)
       (alist-get :owner-pid claim) (alist-get :port claim)))
     ;; No managed server owned here (dead/stale/absent claim): best-effort
     ;; local cleanup of any lingering connection or attach state.
     (t
      (dorg-dolt-server--stop dolt-dir)
      (when interactive-p
        (message "dorg: no DOrg sql-server owned here for %s"
                 (dorg-dolt-server--display-dir dolt-dir)))
      nil))))

;;;###autoload
(defun dorg-dolt-stop-all ()
  "Stop every Dolt sql-server this Emacs process owns and release its claims.
Scoped to servers owned by the current Emacs process: foreign DOrg
servers owned by other processes are never touched.  Wraps
`dorg-dolt-server-stop-all' and returns the number of owned servers stopped."
  (interactive)
  (let ((n (hash-table-count dorg-dolt-server--procs)))
    (dorg-dolt-server-stop-all)
    (when (called-interactively-p 'interactive)
      (message "dorg: stopped %d owned sql-server%s" n (if (= n 1) "" "s")))
    n))

;;; ─── Store compaction (history-preserving gc) — tracked-case ───────────────
;;
;; A long-lived Dolt-backed tracker accumulates noms chunks: every resync
;; commit adds history and the append-only journal is never compacted until
;; gc.  `dorg-dolt-compact' runs `CALL DOLT_GC()' (server-coordinated when a
;; sql-server owns the store, else one-shot CLI), which removes ONLY chunks
;; unreachable from any commit and compacts the journal.  Every reachable
;; commit — and therefore the `dorg-history' as-of/undo window
;; — is preserved.  This means gc CANNOT bound growth that comes from
;; reachable historical states; it reclaims unreferenced churn only, so the
;; command REPORTS before/after size and never oversells reclaim.  Retention/
;; squash (which would shrink the undo window) is deliberately NOT here — it
;; is deferred to an explicit product undo-horizon decision.

(defcustom dorg-dolt-compact-warn-threshold (* 512 1024 1024)
  "Byte size above which `dorg-dolt-compact-check' warns about a Dolt store.
nil disables the warning.  Because compaction is history-preserving it
cannot cap growth from reachable history, so this is a maintenance prompt,
not a hard limit."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'dorg)

(defun dorg-dolt-server--dir-bytes (dir)
  "Return the total byte size of all files under DIR (recursive), or 0."
  (let ((total 0))
    (dolist (f (directory-files-recursively dir "" nil) total)
      (let ((sz (file-attribute-size (file-attributes f))))
        (when sz (setq total (+ total sz)))))))

(defun dorg-dolt-server--noms-size (dolt-dir)
  "Return the byte size of DOLT-DIR's Dolt chunk store, or nil.
Measures `<store>/.dolt/noms', where Dolt keeps its content-addressed
chunks and append-only journal."
  (let ((noms (expand-file-name ".dolt/noms"
                                (dorg-dolt-server--canonical-dir dolt-dir))))
    (when (file-directory-p noms)
      (dorg-dolt-server--dir-bytes noms))))

(defun dorg-dolt-server--format-bytes (n)
  "Human-readable size for N bytes; \"unknown\" when N is nil."
  (if (integerp n) (file-size-human-readable n) "unknown"))

;;;###autoload
(defun dorg-dolt-compact (dolt-dir &optional shallow)
  "Reclaim disk in DOLT-DIR's Dolt store via history-preserving gc.
Runs `CALL DOLT_GC()' through the resolved SQL transport: a live DOrg
sql-server runs it server-coordinated; otherwise a one-shot CLI runs it on
the standalone store; a same-host live-but-unreachable owner is refused by
`dorg-sql--resolve-transport' (which signals a transport conflict) rather
than contending on its lock.

gc removes ONLY chunks unreachable from any commit and compacts the noms
journal, so every reachable commit — and thus the `dorg-history' as-of/undo
window — is preserved.  It therefore CANNOT reduce growth
that comes from reachable historical states; it reclaims unreferenced churn
only.  With prefix arg SHALLOW, runs a faster newgen-only pass.

Returns a plist (:before :after :reclaimed) of `<store>/.dolt/noms' byte
sizes (values may be nil when the store is unreadable), and reports it when
called interactively."
  (interactive (list (dorg-dolt-server--read-dolt-dir "Compact Dolt dir: ")
                     current-prefix-arg))
  (let ((before (dorg-dolt-server--noms-size dolt-dir)))
    (dorg-sql dolt-dir (if shallow "CALL DOLT_GC('--shallow')" "CALL DOLT_GC()"))
    (let* ((after (dorg-dolt-server--noms-size dolt-dir))
           (reclaimed (and (integerp before) (integerp after) (- before after))))
      (when (called-interactively-p 'interactive)
        (message (concat "dorg: compacted %s — noms %s -> %s (reclaimed %s). "
                         "Reachable history / undo window preserved; gc cannot "
                         "shrink reachable history.")
                 (dorg-dolt-server--display-dir dolt-dir)
                 (dorg-dolt-server--format-bytes before)
                 (dorg-dolt-server--format-bytes after)
                 (dorg-dolt-server--format-bytes reclaimed)))
      (list :before before :after after :reclaimed reclaimed))))

;;;###autoload
(defun dorg-dolt-compact-check (dolt-dir)
  "Warn (non-blocking) when DOLT-DIR's Dolt store exceeds the size threshold.
Returns the store size in bytes when it warns, else nil.  Honors
`dorg-dolt-compact-warn-threshold' (nil disables).  Points the operator at
`dorg-dolt-compact'; never runs gc itself.  Called for each listed store by
`dorg-dolt-list' so the size warning surfaces on a real diagnostics path."
  (interactive (list (dorg-dolt-server--read-dolt-dir "Check Dolt dir size: ")))
  (let ((size (dorg-dolt-server--noms-size dolt-dir)))
    (when (and (integerp size)
               (integerp dorg-dolt-compact-warn-threshold)
               (> size dorg-dolt-compact-warn-threshold))
      (display-warning
       'dorg
       (format (concat "Dolt store %s is %s (> %s); run `M-x dorg-dolt-compact' "
                       "to reclaim unreferenced chunks (history-preserving).")
               (dorg-dolt-server--display-dir dolt-dir)
               (dorg-dolt-server--format-bytes size)
               (dorg-dolt-server--format-bytes dorg-dolt-compact-warn-threshold))
       :warning)
      size)))

;;; ─── kill-emacs integration ────────────────────────────────────────────
;;
;; Cleanup must be crash-safe (tracked-regression).  The confirmed Emacs 30.2
;; failure mode is generic: when SIGTERM interrupts garbage collection, any
;; error or allocation failure in a `kill-emacs-hook' Lisp handler aborts
;; before it can finish.  So NO DOrg cleanup Lisp runs on the fatal path.
;; Instead a NAMED `kill-emacs-query-functions' entry handles ORDINARY
;; interactive exit — that hook runs on `save-buffers-kill-emacs' but is
;; skipped entirely when a fatal signal calls `kill-emacs' directly.  It
;; reaps only process objects THIS Emacs retains and releases a managed
;; claim only after its exact retained object is confirmed dead; a foreign
;; or present-invalid claim (including apparently dead) stays fail-closed and
;; is left for explicit later recovery, never an automatic raw-PID reap.
;;
;; Registered at LATE depth 99: query functions run under
;; `run-hook-with-args-until-failure', so any function returning nil cancels
;; exit.  Reaping must therefore run only AFTER every other ordinary-exit
;; query has agreed — otherwise a later veto would strand servers we already
;; killed.  (Depth 99, not 100, by add-hook convention.)

(defun dorg-dolt-server--exit-cleanup ()
  "Reap this process's LOCAL managed servers on ordinary interactive exit.
Registered on `kill-emacs-query-functions', which runs on ordinary exit
but NOT when a fatal signal calls `kill-emacs' directly — so a
SIGTERM-during-GC runs no DOrg cleanup Lisp at all (tracked-regression).
Each independent seam is isolated against both `error' and `quit' so one
failure neither skips the rest nor vetoes exit.  A managed server's claim
is released (token-checked) only after its EXACT retained process object
is confirmed dead; a survivor or unknown verdict keeps both its registry
entry and its claim (fail-closed — never a raw-PID signal, never a
foreign-claim deletion).  Always returns t so it never blocks exit."
  (condition-case nil (dorg-sql--close-all-connections) ((error quit) nil))
  ;; Snapshot (dir . proc) pairs BEFORE any kill: `--kill-process' runs the
  ;; process sentinel, which can mutate `--procs' (e.g. clear a dead entry)
  ;; while we are still iterating.  Gathering an immutable snapshot first
  ;; keeps a mid-kill mutation from skipping or corrupting later entries.
  (let (snapshot dead-dirs)
    ;; The snapshot seam is as fallible as the kills: guard it against both
    ;; `error' and `quit'.  If the table cannot be walked, skip process
    ;; iteration entirely and still return t — never veto exit.
    (condition-case nil
        (maphash (lambda (key entry) (push (cons key (car entry)) snapshot))
                 dorg-dolt-server--procs)
      ((error quit) (setq snapshot nil)))
    (dolist (pair snapshot)
      (let ((proc (cdr pair)))
        (condition-case nil (dorg-dolt-server--kill-process proc)
          ((error quit) nil))
        (when (condition-case nil (not (process-live-p proc))
                ((error quit) nil))
          (push (car pair) dead-dirs))))
    (dolist (key dead-dirs)
      (condition-case nil (remhash key dorg-dolt-server--procs)
        ((error quit) nil))
      (condition-case nil (dorg-dolt-server--release-claim key)
        ((error quit) nil))))
  t)

(add-hook 'kill-emacs-query-functions #'dorg-dolt-server--exit-cleanup 99)

;;; ─── Interactive SQL Shell ─────────────────────────────────────────────

(defun dorg-sql-connect (dolt-dir)
  "Open an interactive SQL shell to the dorg database at DOLT-DIR.
Uses Emacs `sql-connect' with the mysql product to connect to the
running dolt sql-server.  Starts the server if not already running."
  (interactive
   (list (or (and (boundp 'dorg-dolt-dir) dorg-dolt-dir)
             (read-directory-name "Dolt directory: "))))
  (require 'sql)
  (let* ((port (dorg-dolt-server--ensure-strict dolt-dir))
         (db-name (dorg-dolt-server--db-name dolt-dir))
         (conn-name (intern (format "dorg-%s" db-name)))
         (sql-connection-alist
          (cons (list conn-name
                      '(sql-product 'mysql)
                      (list 'sql-server "127.0.0.1")
                      (list 'sql-port port)
                      (list 'sql-database db-name)
                      '(sql-user "root"))
                sql-connection-alist)))
    (sql-connect conn-name
                 (format "*dorg-sql:%s*" db-name))))

(provide 'dorg-dolt-server)
;;; dorg-dolt-server.el ends here
