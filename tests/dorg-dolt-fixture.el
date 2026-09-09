;;; dorg-dolt-fixture.el --- Opaque DOrg-owned Dolt fixture/lease API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module is the SOLE, OPAQUE owner of Dolt sql-server lifecycle for
;; DOrg test and consumer code (tracked-case).  It gathers every reach into
;; sql-server lifecycle — process spawn, readiness, PID-metadata scanning,
;; the durable ownership registry (tracked-case), and the shared warm test
;; server — behind a supported, opaque handle so that:
;;
;;   - callers touch a single choke point instead of duplicating lifecycle
;;     by hand as private reach-ins, direct spawns, and PID scans;
;;   - the fixture server is FIRST-CLASS in the public inventory
;;     (`dorg-dolt-server-list' sees it as a live, owned server); and
;;   - downstream consumers (Downstream tracker) route teardown through the PUBLIC
;;     `dorg-dolt-stop'/`-stop-all'/`-server-list' surface only, never PIDs,
;;     claims, or private registries.
;;
;; Layering (D3):
;;   consumers ─────────────► PUBLIC dorg-dolt-stop/-stop-all/-server-list
;;   generic helpers/macros ► OPAQUE dorg-dolt-fixture-* API (this file)
;;                                  │ owns
;;                                  ▼
;;             PUBLIC dorg-dolt-* + tracked-case durable registry
;;                                  │ (the ONLY layer that may)
;;                                  ▼
;;             private dorg-dolt-server--* primitives (spawn/kill/scan)
;;
;; The lifecycle machinery below (shared server, durable registry,
;; owned-root reaping) is the DOrg-owned substrate that the generic test
;; helper (`dorg-test-helpers.el') and the runner delegate to; it keeps its
;; `dorg-test--' names so existing consumers resolve unchanged.  The
;; supported public surface — the opaque lease API — is defined at the end.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dorg-schema)
(require 'dorg-dolt-server)

(defvar dorg-test--mysql-available (require 'mysql nil t)
  "Non-nil when mysql.el is available for native wire protocol.")

(define-error 'dorg-dolt-fixture-cleanup-error
  "DOrg fixture shared-server abort left cleanup failures")

;; Forward declarations: the opaque fixture state (defined with values in the
;; Fixture API section below) is referenced by the legacy thin wrappers that
;; precede that section, so declare it special here to keep the wrappers'
;; byte-compilation free of `free variable' warnings.
(defvar dorg-dolt-fixture--shared-lease)
(defvar dorg-dolt-fixture--failed-start-lease)
(defvar dorg-dolt-fixture--conn)
(defvar dorg-dolt-fixture--store-leases)

;;; ─── Shared Test Server ────────────────────────────────────────────────
;;
;; A single `dolt sql-server --data-dir' process shared across the
;; entire test suite.  Each test gets an isolated database via
;; CREATE DATABASE / DROP DATABASE — much faster than copy-directory
;; + CLI call-process per test.

(defvar dorg-test--server-data-dir nil
  "Temp directory used as --data-dir for the shared test server.")

(defvar dorg-test--server-port nil
  "Port of the shared test server, or nil if not running.")

(defvar dorg-test--server-proc nil
  "Process object for the shared test server.")

(defvar dorg-test--mysql-conn nil
  "Legacy mirror of the fixture's mysql.el connection to the shared server.
Published by `dorg-test--start-shared-server' from `dorg-dolt-fixture--conn'
for consumer suites that read it; the fixture owns the live connection.")

;;; ─── Owned Root / Server Cleanup ──────────────────────────────────────

(defvar dorg-test--owned-roots (make-hash-table :test 'equal)
  "Canonical temp roots owned by the current test run.")

;;; ─── Durable Ownership Registry (tracked-case) ──────────────────────────
;;
;; The batch test process contributes NOTHING to `kill-emacs-hook' (see the
;; zero-fatal-hook block below), so a fatal signal runs zero cleanup Lisp.
;; Deterministic recovery instead rests on durable ownership records:
;; readable `<key>.eld' plists written ATOMICALLY (sibling temp + rename)
;; BEFORE a server can exist, deleted in lockstep with normal-path reaping,
;; and consumed by bounded, identity-gated reaping at the NEXT test startup
;; (`dorg-test--reap-stale-server-registry').  Records carry
;; :owner-pid / :server-pid / :port / :root / :dolt-dir.

(defvar dorg-test--registry-dir
  (expand-file-name "dorg-test-server-registry" temporary-file-directory)
  "Directory holding durable test-ownership records (`<key>.eld' files).
Shared by concurrent lanes: records are keyed by owner Emacs pid, and
reaping is fail-safe — only records whose owner is provably dead are
ever touched.")

(defun dorg-test--registry-rename (src dst)
  "Atomically replace DST with SRC — the single plain-Lisp replace boundary.
Registry writes route every replace through this seam so tests can spy or
fail it without touching the native `rename-file' primitive."
  (rename-file src dst t))

(defun dorg-test--registry-validate (key plist)
  "Signal an error unless KEY and PLIST hold canonical registry data.
Only validated data is ever registered, so next-start reaping never meets
a malformed datum: paths must be strings, pids/ports integers, and KEY a
filename-safe token."
  (unless (and (stringp key)
               (string-match-p "\\`[A-Za-z0-9._-]+\\'" key))
    (error "dorg-test registry: invalid record key %S" key))
  (dolist (prop '(:root :dolt-dir))
    (when (and (plist-member plist prop)
               (not (stringp (plist-get plist prop))))
      (error "dorg-test registry: %s must be a string: %S"
             prop (plist-get plist prop))))
  (dolist (prop '(:owner-pid :server-pid :port))
    (when (and (plist-member plist prop)
               (not (integerp (plist-get plist prop))))
      (error "dorg-test registry: %s must be an integer: %S"
             prop (plist-get plist prop)))))

(defun dorg-test--registry-write (key plist)
  "Durably persist PLIST as ownership record KEY; return the record file.
Creates `dorg-test--registry-dir' on demand and lands `<KEY>.eld'
atomically: the payload goes to a sibling temp file in the SAME directory
and is renamed onto the final name through `dorg-test--registry-rename' —
never an in-place write, so a crash mid-write can never expose a partial
record to next-start reaping.  On failure the temp file is cleaned and a
previous record, if any, stays intact and readable."
  (dorg-test--registry-validate key plist)
  (make-directory dorg-test--registry-dir t)
  (let* ((file (expand-file-name (concat key ".eld") dorg-test--registry-dir))
         (temp (make-temp-file
                (expand-file-name (concat key ".tmp-")
                                  dorg-test--registry-dir))))
    (condition-case err
        (progn
          (with-temp-file temp
            (let ((print-level nil)
                  (print-length nil))
              (prin1 plist (current-buffer))))
          (dorg-test--registry-rename temp file))
      (error
       (ignore-errors (delete-file temp))
       (signal (car err) (cdr err))))
    file))

(defun dorg-test--registry-delete (key)
  "Delete ownership record KEY; bounded no-op when absent."
  (let ((file (expand-file-name (concat key ".eld") dorg-test--registry-dir)))
    (when (file-exists-p file)
      (delete-file file))))

(defun dorg-test--proper-keyword-plist-p (form)
  "Non-nil when FORM is a NON-EMPTY, proper, even-length keyword-keyed plist.
Fail-closed against every shape a half-written or foreign record can take: a
dotted tail or circular list (`proper-list-p' returns nil), an odd element
count, a bare atom, or ANY key position (0, 2, 4, …) that is not a keyword.
Only a record whose every key slot is a `:'-keyword and whose length is a
positive even number qualifies, so the reaper never destructures a corrupt
durable record as if it were valid ownership."
  (let ((len (proper-list-p form)))     ; nil for dotted/circular/non-list
    (and (integerp len)
         (> len 0)
         (= 0 (% len 2))
         (let ((ok t) (rest form))
           (while (and ok rest)
             (unless (keywordp (car rest)) (setq ok nil))
             (setq rest (cddr rest)))
           ok))))

(defun dorg-test--registry-read (file)
  "Return the record plist stored in FILE, or nil when unreadable/malformed.
Requires EXACTLY ONE proper Lisp form spanning FILE to EOF AND that the form
is a non-empty, even-length, keyword-keyed plist
\(`dorg-test--proper-keyword-plist-p').  A valid first plist followed by a
SECOND form or trailing non-whitespace junk, a dotted/odd/non-plist form, or a
form with a non-keyword key is a corrupted record and returns nil
\(fail-closed), so the reaper treats it as undeterminable ownership and blocks
— never consuming a half-overwritten record on the strength of its first form
or a foreign datum on the strength of being a cons."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        ;; Only trailing whitespace may follow the single form, and the form
        ;; must be a proper keyword plist.
        (skip-chars-forward " \t\n\r\f")
        (and (eobp)
             (dorg-test--proper-keyword-plist-p data)
             data)))))

(defun dorg-test--registry-shared-key ()
  "Registry key for this process's shared test-server record."
  (format "shared-%d" (emacs-pid)))

(defun dorg-test--registry-root-key (canon)
  "Registry key for the owned-root record of canonical path CANON."
  (format "root-%d-%s" (emacs-pid) (md5 canon)))

(defun dorg-test--registry-store-key (canon)
  "Registry key for the owned-server record of canonical dolt-dir CANON."
  (format "server-%d-%s" (emacs-pid) (md5 canon)))

(defun dorg-test--registry-recover-server-pid (record)
  "Recover RECORD's server pid from its store's `sql-server.info', or nil.
Covers the persist-before-start window: ownership is durable BEFORE
`start-process', so a fatal signal in that window leaves a record without
`:server-pid'.  The dolt-native `PID:PORT:UUID' file under the recorded
store is the bounded, normal-context source for the missing pid."
  (let ((store (plist-get record :dolt-dir)))
    (when (and (stringp store) (file-directory-p store))
      (plist-get (dorg-dolt-server--dolt-server-info store) :pid))))

(defun dorg-test--registry-identity-match-p (pid record)
  "Non-nil when live PID exactly matches the dolt identity RECORD records.
Fail-closed: the command must be dolt running `sql-server', AND the
recorded :port and :dolt-dir must BOTH be present and equal the --port
and --data-dir the command line advertises.  Missing evidence — a record
without the field, or an argv that does not advertise it — never
qualifies, so a near-anonymous live dolt can never be killed on partial
identity.  Callers must establish liveness first; this probes
`process-attributes'."
  (let* ((attrs (process-attributes pid))
         (comm (cdr (assq 'comm attrs)))
         (args (cdr (assq 'args attrs)))
         (rport (plist-get record :port))
         (rstore (plist-get record :dolt-dir)))
    (and (stringp comm)
         (string-match-p "\\(?:\\`\\|/\\)dolt\\'" comm)
         (stringp args)
         (string-match-p "sql-server" args)
         (integerp rport)
         (string-match "--port[= ]\\([0-9]+\\)" args)
         (= rport (string-to-number (match-string 1 args)))
         (stringp rstore)
         (string-match "--data-dir[= ]\\([^ \t\n]+\\)" args)
         (equal (match-string 1 args) rstore))))

(defun dorg-test--live-dolt-process-evidence (dolt-dir)
  "Return TRI-STATE process-table evidence for a live server under DOLT-DIR.
Scans the OS process table (`list-system-processes' + `process-attributes') for
a running `dolt' whose command line runs `sql-server' and whose --data-dir
argument canonically resolves (`file-truename') to DOLT-DIR.  Returns:
  `live'    -- a matching process exists -> RETAIN.
  `none'    -- the table was FULLY enumerated and examined and no match exists;
              the ONLY state that permits recovery.
  `unknown' -- the scan could not complete: DOLT-DIR is unusable, OR
              `list-system-processes' signalled, OR ANY `process-attributes'
              (or candidate-path canonicalization) signalled -- a process we
              could not examine might BE the child.  Evidence is inconclusive,
              so the caller must PRESERVE.

This is the ONLY evidence that survives the post-`start-process'/pre-bind/pre-
`sql-server.info' window: a just-forked child is process-ALIVE yet port-DEAD and
has written NO on-disk metadata, so neither `dorg-dolt-server--port-live-p' nor a
metadata probe can see it.  `tracked-case' persists the ownership record BEFORE the
spawn, so a crash in that window leaves a `:server-pid'-absent record that is
byte-identical to a genuine pre-spawn (no-child) record.

FAIL-CLOSED (reviewer re-cert #3): the scan must NOT fail open -- a swallowed
enumeration/probe error must never read as a confirmed absence, or recovery
would delete a data-dir out from under a live starting server.  A nil/empty scan
result is `none' ONLY when it was produced without any error; every error path
yields `unknown', which the caller treats as RETAIN."
  (if (not (stringp dolt-dir))
      'unknown
    (let ((target (condition-case nil
                      (file-name-as-directory
                       (file-truename (directory-file-name dolt-dir)))
                    (error 'error))))
      (if (eq target 'error)
          'unknown
        (let ((pids (condition-case nil (list-system-processes)
                      (error 'error))))
          (if (eq pids 'error)
              'unknown
            (catch 'done
              (let ((saw-error nil))
                (dolist (pid pids)
                  (let ((attrs (condition-case nil (process-attributes pid)
                                 (error 'error))))
                    (if (eq attrs 'error)
                        (setq saw-error t)
                      (let ((comm (cdr (assq 'comm attrs)))
                            (args (cdr (assq 'args attrs))))
                        (when (and (stringp comm)
                                   (string-match-p "\\(?:\\`\\|/\\)dolt\\'" comm)
                                   (stringp args)
                                   (string-match-p "sql-server" args)
                                   (string-match "--data-dir[= ]\\([^ \t\n]+\\)" args))
                          (let ((argdir (condition-case nil
                                            (file-name-as-directory
                                             (file-truename
                                              (directory-file-name
                                               (match-string 1 args))))
                                          (error 'error))))
                            (cond
                             ((eq argdir 'error) (setq saw-error t))
                             ((equal argdir target) (throw 'done 'live)))))))))
                ;; No match found: `none' ONLY if every process was examined
                ;; cleanly; any probe error leaves the child possibly unseen.
                (if saw-error 'unknown 'none)))))))))

(defun dorg-test--root-reap-eligible-p (root)
  "Non-nil when ROOT is a test-owned directory safe to SCAN and reap.
Pure boundary (no side effects): ROOT itself is not a symlink, its canonical
`file-truename' lies under the canonical `temporary-file-directory', AND its
basename is `dorg-'-prefixed.  A `dorg-*' SYMLINK targeting an outside store,
or a root outside tmpdir, is INELIGIBLE — so next-start recovery never scans,
`process-attributes'-probes, or kills a live process outside test ownership
that deletion would later refuse anyway."
  (and (stringp root)
       (not (file-symlink-p (directory-file-name root)))
       (let ((canon (ignore-errors (file-truename (directory-file-name root))))
             (tmp (file-name-as-directory
                   (file-truename temporary-file-directory))))
         (and (stringp canon)
              (string-prefix-p tmp (file-name-as-directory canon))
              (string-prefix-p "dorg-" (file-name-nondirectory canon))))))

(defun dorg-test--dir-contained-p (dir root)
  "Non-nil when DIR's canonical truename is contained under ROOT's canonical
truename.  Both are resolved with `file-truename' so an interior symlink that
escapes ROOT is NOT contained — a kill/probe target must live under the exact
eligible root, never at a dolt-dir the record advertises outside it."
  (and (stringp dir) (stringp root)
       (let ((d (ignore-errors
                  (file-name-as-directory (file-truename (directory-file-name dir)))))
             (r (ignore-errors
                  (file-name-as-directory (file-truename (directory-file-name root))))))
         (and d r (string-prefix-p r d)))))

(defun dorg-test--reap-stale-root (root expected-id)
  "Classify + delete recorded ROOT under identity + containment; return a symbol.
ROOT is the durable record's stored key and EXPECTED-ID its captured
`:root-id'.  The stored key is used DIRECTLY — NEVER re-`file-truename'd, so a
symlink swapped in over the path is not resolved to its outside target.
  `deleted'    — ROOT is an eligible test-owned dir whose current filesystem
                 identity matches the captured tuple; it was removed;
  `absent'     — ROOT names a path that does NOT exist (not even a symlink):
                 nothing to protect, so the record is SAFE to consume;
  `ineligible' — ROOT is missing/non-string, a symlink, a non-`dorg-'/outside
                 root, OR an EXISTING dir whose identity does NOT match
                 EXPECTED-ID (an observed distinct replacement, or a LEGACY
                 record with a nil `:root-id' that cannot authorize deletion):
                 PRESERVED, never consumed;
  or SIGNALS when an eligible delete itself fails (PRESERVES the record).
Callers destroy a root ONLY after every server it advertised is confirmed gone."
  (let ((dfn (and (stringp root) (directory-file-name root))))
    (cond
     ((not (stringp root)) 'ineligible)
     ;; A symlink swapped in over the path is FOREIGN — never resolve or delete.
     ((file-symlink-p dfn) 'ineligible)
     ;; Genuinely gone (no file at the path at all): nothing to protect.
     ((not (file-exists-p dfn)) 'absent)
     ;; Exists as a real dir: require eligibility AND a match with the captured
     ;; identity (a legacy nil `:root-id' or observed mismatch fails closed)
     ;; before the delete — validated on the stored path, never re-truenamed.
     ((and (dorg-test--root-reap-eligible-p root)
           (dorg-test--identity-intact-p root expected-id))
      (delete-directory dfn t)
      'deleted)
     (t 'ineligible))))

(defun dorg-test--marker-present-p (file)
  "Non-nil when FILE exists OR is a (possibly DANGLING) symlink.
`file-exists-p' FOLLOWS a symlink and returns nil for a dangling one, which
would misclassify a present-but-malformed metadata marker (a dangling
claim/pid/info symlink) as ABSENT.  A dangling marker symlink IS present
malformed metadata and must count, so existence is OR-ed with `file-symlink-p'."
  (or (file-exists-p file) (file-symlink-p file)))

(defun dorg-test--store-has-metadata-file-p (canon)
  "Non-nil when store dir CANON has a server-metadata FILE on disk.
Presence is checked INDEPENDENT of readability AND of symlink-target existence
\(`dorg-test--marker-present-p'), so a present-but-unreadable/incomplete or
DANGLING-symlink DOrg overlay (`<store>/.dorg/server.claim' or `server.pid') or
dolt-native `<store>/.dolt/sql-server.info' still counts and blocks — while a
store with NONE of these markers is genuinely serverless and may clean.
Mirrors the marker set `dorg-test--owned-store-dirs' scans for, so a store
discovered by that scan always registers here."
  (let ((dorg-dir (ignore-errors (dorg-dolt-server--store-dorg-dir canon))))
    (or (and dorg-dir (dorg-test--marker-present-p
                       (expand-file-name "server.claim" dorg-dir)))
        (and dorg-dir (dorg-test--marker-present-p
                       (expand-file-name "server.pid" dorg-dir)))
        (dorg-test--marker-present-p
         (expand-file-name ".dolt/sql-server.info" canon)))))

(defun dorg-test--synthetic-provisional-claim-p (canon claim info)
  "Non-nil when CANON's ONLY server metadata is a synthetic provisional claim.
Ground-truth relaxation of the metadata-file fallback in
`dorg-test--store-server-identities': a synthetic/never-served overlay
\(a test fixture that publishes a READABLE `.dorg/server.claim' with nil pid
and no accompanying native record, e.g. `dorg-test--write-claim') is confirmed
ABSENT by the SAME process-table + port ground truth the persist-before-spawn
recovery (tracked-case) already trusts, instead of emitting the permanent nil-pid
block that strands the root as an unreapable dead-owner record.  Returns t iff
ALL hold:
  1. CLAIM parsed (a READABLE overlay) with `:server-pid' nil;
  2. INFO nil (no readable native `.dolt/sql-server.info' identity);
  3. the claim's `:port' is EITHER nil OR a DEAD port — an integer with no TCP
     listener (`dorg-dolt-server--port-live-p' nil).  A LIVE port BLOCKS: a
     bound orphan may still be serving on it;
  4. the provisional overlay is the ONLY marker — no `.dorg/server.pid' file
     and no `.dolt/sql-server.info' file present under CANON
     (`dorg-test--marker-present-p'); and
  5. `dorg-test--live-dolt-process-evidence' for CANON is exactly `none' — a
     FULLY enumerated, error-free scan finding no `dolt sql-server --data-dir
     CANON'.
FAIL-CLOSED is preserved (reviewer no-fail-open): an UNREADABLE claim (nil),
a real pid, a LIVE claimed port, a corroborating `server.pid'/native-info
marker, or a `live'/`unknown' (inconclusive) process scan ALL leave the block
intact.  Only positive ground-truth absence relaxes.  Production provisional
overlays carry a PORT coalesced with a native pid, so the pid-less + (port-nil
OR dead-port) + native-free + process-`none' signature is test-only — and even
when a synthetic overlay records a dead ephemeral port (tracked-case: incident
roots at dead ports 61104/61107), the dead port + `none' process scan prove
nothing is alive.  A non-nil live port re-blocks, so no bound server is buried."
  (let ((dorg-dir (ignore-errors (dorg-dolt-server--store-dorg-dir canon))))
    (and claim
         (null (alist-get :server-pid claim))
         (null info)
         (let ((port (alist-get :port claim)))
           (or (null port)
               (and (integerp port)
                    (not (dorg-dolt-server--port-live-p port)))))
         (not (and dorg-dir
                   (dorg-test--marker-present-p
                    (expand-file-name "server.pid" dorg-dir))))
         (not (dorg-test--marker-present-p
               (expand-file-name ".dolt/sql-server.info" canon)))
         (eq 'none (dorg-test--live-dolt-process-evidence canon)))))

(defun dorg-test--read-pidfile-pid (pidfile)
  "Return the integer pid stored in PIDFILE, or nil when unreadable/malformed.
PIDFILE is `<store>/.dorg/server.pid' — a bare decimal pid line written by
`dorg-dolt-server--detached-launch'.  A dangling symlink, an unreadable file,
an empty file, or a non-numeric body yields nil."
  (and (file-readable-p pidfile)
       (ignore-errors
         (let ((s (with-temp-buffer
                    (insert-file-contents pidfile)
                    (string-trim (buffer-string)))))
           (when (string-match-p "\\`[0-9]+\\'" s)
             (string-to-number s))))))

(defun dorg-test--store-pidfile-only-identity (canon)
  "Return a one-identity list recovered from CANON's `.dorg/server.pid', or nil.
Residue recovery (tracked-case): a real detached daemon writes THREE artifacts —
`.dolt/sql-server.info', `.dorg/server.claim', and `.dorg/server.pid' (its bare
pid).  `dorg-dolt-stop' deletes the claim and dolt removes sql-server.info on
clean shutdown, but NOTHING removes server.pid (production only overwrites it on
the next launch).  The lone leftover server.pid holds the DEAD daemon's REAL,
readable pid, yet `dorg-test--store-server-identities' reads only the claim and
native info, so it would otherwise emit the always-blocking nil-pid identity and
strand the root.

Applies ONLY when server.pid is the SOLE server-metadata marker present — no
`server.claim' and no native `.dolt/sql-server.info' (`dorg-test--marker-
present-p', so a present-but-unreadable or dangling claim/info still counts and
DISQUALIFIES this path) — and it holds a readable pid.  Returns
`((:pid P :port nil :dolt-dir CANON))' so the pidfile pid is CONFIRMED by
`dorg-test--confirm-server-gone' (a DEAD pid -> `gone' -> reapable; a LIVE pid
-> `unknown', since `:port' nil fails `registry-identity-match-p', so it is
never killed and the root is retained — fail-closed).  Returns nil when the
pidfile is absent/dangling/unreadable OR any other marker is present, so the
caller falls through to the nil-pid block; ukfp6's protection for
corrupt/incomplete claim/info metadata is unchanged."
  (let ((dorg-dir (ignore-errors (dorg-dolt-server--store-dorg-dir canon))))
    (when dorg-dir
      (let ((pidfile (expand-file-name "server.pid" dorg-dir))
            (claimfile (expand-file-name "server.claim" dorg-dir))
            (infofile (expand-file-name ".dolt/sql-server.info" canon)))
        (when (and (not (dorg-test--marker-present-p claimfile))
                   (not (dorg-test--marker-present-p infofile))
                   (dorg-test--marker-present-p pidfile))
          (let ((pid (dorg-test--read-pidfile-pid pidfile)))
            (when (integerp pid)
              (list (list :pid pid :port nil :dolt-dir canon)))))))))

(defun dorg-test--store-server-identities (store)
  "Return the DISTINCT server identities STORE advertises on disk.
Captures BOTH the DOrg claim (overlay) identity AND the dolt-native
`.dolt/sql-server.info' identity — they can name DIFFERENT pids/ports (a stale
dead overlay vs a live native process not yet listed), and confirming only one
while the other is live would delete metadata over a running server.  Each is
`(:pid P :port PORT :dolt-dir CANON)', deduped by exact (pid . port).  CANON is
STORE's canonical dir (the `--data-dir' a matching argv advertises).

Metadata-FILE presence is distinguished from readable identity
\(`dorg-test--store-has-metadata-file-p'): a store with a metadata FILE on disk
but NO readable identity (an unreadable/incomplete claim or `sql-server.info')
still yields ONE nil-pid identity, so a metadata-bearing store can never
collapse to `no server' and always blocks — with TWO ground-truth exceptions.
RESIDUE RECOVERY (tracked-case, `dorg-test--store-pidfile-only-identity'): when
the SOLE marker is a readable `.dorg/server.pid' (claim AND native info both
absent) — the artifact a real detached daemon leaves after `dorg-dolt-stop'
drops the claim and dolt removes sql-server.info — its REAL pid is recovered as
a CONFIRMABLE `(:pid P :port nil)' identity (dead -> `gone', live -> blocks),
so dead-daemon roots are reapable instead of stranding on the nil-pid block.
The other exception
\(tracked-case, `dorg-test--synthetic-provisional-claim-p'): a synthetic/never-
served provisional overlay (readable pid-less claim whose `:port' is nil OR a
DEAD port, no `server.pid'/native-info marker) whose data-dir has process
evidence `none' collapses to NIL, so test-fixture claim roots — including the
port-bearing incident shape (`:server-pid' nil, dead ephemeral `:port') — are
reapable instead of stranding as permanent dead-owner records.  A LIVE claimed
port re-blocks.  A store with NO metadata file at all (nil
readers AND no file on disk) yields NIL — genuinely serverless, so a caller with
an independently-confirmed-dead contained process may clean it.  Returns nil
when STORE has no canonical form OR has no metadata file.

The overlay is read RAW (`dorg-dolt-server--claim-read'), not reconciled
\(`--effective-claim' can select away one on-disk source, defeating the
both-sources contract).  A PROVISIONAL overlay (`:server-pid' nil) is COALESCED
into the native identity when they share the same `:port' — the native pid is
the confirmable identity, so the provisional overlay must not permanently block
cleanup after that pid is killed.  Overlay and native on DIFFERENT ports stay
DISTINCT (fail-closed: the incomplete overlay blocks on its own port)."
  (let ((canon (ignore-errors (dorg-dolt-server--canonical-dir store))))
    (when canon
      (let* ((claim (ignore-errors (dorg-dolt-server--claim-read canon)))
             (info (ignore-errors (dorg-dolt-server--dolt-server-info canon)))
             (o-pid (and claim (alist-get :server-pid claim)))
             (o-port (and claim (alist-get :port claim)))
             (n-pid (and info (plist-get info :pid)))
             (n-port (and info (plist-get info :port)))
             (overlay (and (or o-pid o-port) (cons o-pid o-port)))
             (native (and (or n-pid n-port) (cons n-pid n-port)))
             pairs)
        (cond
         ;; Provisional overlay (nil pid) + native with a real pid on the SAME
         ;; port: one confirmable native identity (killing it clears both).
         ((and overlay native (null o-pid) (integerp n-pid)
               (equal o-port n-port))
          (setq pairs (list native)))
         ;; Otherwise keep every distinct on-disk identity (fail-closed).
         (t (setq pairs (delq nil (list overlay native)))))
        (let ((seen (make-hash-table :test 'equal)) ids)
          (dolist (pair pairs)
            (unless (gethash pair seen)
              (puthash pair t seen)
              (push (list :pid (car pair) :port (cdr pair) :dolt-dir canon) ids)))
          (cond
           ;; GROUND-TRUTH RELAXATION (tracked-case): a synthetic/never-served
           ;; provisional overlay — a readable pid-less claim whose `:port' is
           ;; nil OR a DEAD port, with no corroborating `server.pid'/native-info
           ;; marker and process evidence `none' — is confirmed ABSENT and
           ;; collapses to nil (reapable).  Checked FIRST so it suppresses BOTH
           ;; a port-bearing readable overlay identity `(nil . DEAD-PORT)' (which
           ;; would otherwise land in the `ids' branch below and block forever on
           ;; a nil pid that can never `confirm-server-gone') AND the port-less
           ;; metadata-file fallback.  Test-fixture claim roots therefore do not
           ;; strand as permanently unreapable dead-owner records.  The predicate
           ;; short-circuits on a real `:server-pid', a live native record, or a
           ;; LIVE port, so no readable real identity is ever suppressed.
           ((dorg-test--synthetic-provisional-claim-p canon claim info) nil)
           ;; Readable identities: exact, deduped.
           (ids (nreverse ids))
           ;; RESIDUE RECOVERY (tracked-case): the SOLE remaining marker is a
           ;; readable `.dorg/server.pid' (claim AND native sql-server.info both
           ;; absent) — the artifact a real detached daemon leaves after
           ;; `dorg-dolt-stop' drops the claim and dolt removes sql-server.info,
           ;; which nothing else deletes.  Recover its REAL pid as a CONFIRMABLE
           ;; identity (dead -> `gone', live -> blocks `unknown') instead of the
           ;; always-blocking nil-pid.  Checked BEFORE the metadata-file
           ;; fallback; disqualified (falls through) when any claim/info marker
           ;; is present or the pidfile is unreadable, so ukfp6's nil-pid block
           ;; for corrupt/incomplete claim/info metadata is untouched.
           ((dorg-test--store-pidfile-only-identity canon))
           ;; No readable identity, but a metadata FILE is present on disk
           ;; (unreadable/incomplete claim or `sql-server.info'): yield ONE
           ;; blocking nil-pid identity so a metadata-bearing store blocks
           ;; rather than cleans — it must be independently confirmed, never
           ;; certified by a `--procs' entry.
           ((dorg-test--store-has-metadata-file-p canon)
            (list (list :pid nil :port nil :dolt-dir canon)))
           ;; No metadata file at all: genuinely serverless -> nil.
           (t nil)))))))

(defun dorg-test--confirm-server-gone (identity)
  "Kill IDENTITY's server iff exact-verified live, and CONFIRM it is gone.
IDENTITY is `(:pid P :port PORT :dolt-dir DIR)'.  Fail-closed: a kill fires
ONLY for a pid that is provably live AND whose `process-attributes' identity
exactly matches the recorded dolt `sql-server' port + data-dir; after the
kill the pid is re-checked and MUST read dead.  Returns one of:
  `gone'        — confirmed absent (pid already dead, or killed and re-checked
                  dead): the ONLY status that authorises destroying the
                  server's on-disk metadata or its root;
  `unknown'     — cannot verify (no pid, `unknown' liveness, or a LIVE pid
                  whose identity does not exactly match);
  `still-live'  — an exact match was killed yet is still live;
  `error'       — the kill signalled.
Any non-`gone' status PRESERVES the server, its metadata, and the record so a
later start retries."
  (let ((pid (plist-get identity :pid))
        (record (list :port (plist-get identity :port)
                      :dolt-dir (plist-get identity :dolt-dir))))
    (if (not (integerp pid))
        'unknown
      (let ((live (dorg-dolt-server--pid-live-p pid)))
        (cond
         ((null live) 'gone)                    ; dead -> confirmed absent
         ((not (eq live t)) 'unknown)           ; `unknown' liveness -> preserve
         ((not (dorg-test--registry-identity-match-p pid record)) 'unknown)
         (t (condition-case _err
                (progn
                  (dorg-dolt-server--kill-pid pid dorg-dolt-server-stop-term-grace)
                  (if (null (dorg-dolt-server--pid-live-p pid)) 'gone 'still-live))
              (error 'error))))))))

(defun dorg-test--record-server-identities (record)
  "Return the server identities a dead-owner RECORD implicates.
Combines the record's OWN exact fields (`:server-pid', or a pid recovered
from its store's `sql-server.info', with `:port'/`:dolt-dir') with EVERY
server DISCOVERED by scanning the recorded `:root' for on-disk metadata — the
untraceable-orphan case a root-only record (`:root' + `:owner-pid') would
otherwise strand.

CONTAINMENT (fail-closed): a candidate is probed/killed ONLY when the record's
`:root' is eligible (`dorg-test--root-reap-eligible-p') AND the candidate's
canonical dolt-dir is contained under that eligible canonical root
\(`dorg-test--dir-contained-p').  Any candidate that is unprovable — an
ineligible root, or a dolt-dir the record advertises OUTSIDE the root
(including an interior symlink escaping it) — is emitted as a BLOCKING
\(`:pid' nil) identity so the record PRESERVES and appears as a failure, with
NO `process-attributes' probe or kill.  Discovered stores are contained by
construction; a metadata-bearing store with an unreadable pid still yields a
blocking identity.

PERSIST-BEFORE-SPAWN RECOVERY (tracked-case): the ownership record is written
BEFORE `start-process', so a fatal signal in that window leaves a `:server-pid'-
absent record whose eligible root has NO on-disk server metadata.  Such a record
implicates NO server ONLY when it also has no recoverable pid, nothing listening
on its recorded `:port', AND process-table evidence CONFIRMS no live server under
its dolt-dir (`dorg-test--live-dolt-process-evidence' returns `none') -- then the
empty root is reaped and the record consumed, so the shared lane is not
permanently refused.  On-disk metadata, a LIVE port, a live starting process
\(`live'), OR an INCONCLUSIVE scan (`unknown') each keeps the record blocking: a
post-`start-process'/pre-bind child is port-dead but PROCESS-ALIVE and has
written no metadata, so the process table is the only evidence (reviewer:
port-dead != process-dead), and a scan that could not complete must never read as
absence (reviewer re-cert #3: no fail-open).  So confirmed-death safety is never
weakened -- a metadata-free, pid-free, port-dead, CONFIRMED-process-none eligible
root is the only own-field case that produces no identities and may clean.

Identities are deduped by exact (pid . port . dolt-dir) tuple.  The caller
snapshots this list ONCE before Phase 1 and reuses that exact set, so metadata
disappearing (a paired record's delete) or a re-read cannot turn an unknown
into an empty/clear set."
  (let* ((ids nil)
         (seen (make-hash-table :test 'equal))
         (root (plist-get record :root))
         ;; `eligible' gates the record's OWN explicit identity + containment
         ;; (its `:server-pid'/`:dolt-dir' are the record's own data; the kill
         ;; is separately exact-match-gated).
         (eligible (and (stringp root) (dorg-test--root-reap-eligible-p root)))
         ;; `identity-ok' additionally requires the current filesystem tuple to
         ;; match the captured `:root-id' — the fail-closed gate BEFORE on-disk
         ;; read of the root's contents (the recursive store scan and the
         ;; recovery-pid read of `sql-server.info').  A symlink swap, a
         ;; observed mismatch, or a LEGACY record with a nil `:root-id' makes
         ;; `identity-ok' nil, so the root's on-disk contents are never scanned
         ;; or probed.  A matching reusable tuple is only best-effort evidence.
         (identity-ok (and eligible
                           (dorg-test--identity-intact-p
                            root (plist-get record :root-id)))))
    (cl-flet ((emit (id)
                (let ((key (list (plist-get id :pid)
                                 (plist-get id :port)
                                 (plist-get id :dolt-dir))))
                  (unless (gethash key seen)
                    (puthash key t seen)
                    (push id ids)))))
      ;; The record's own explicit identity — probed only when the root is
      ;; eligible AND its dolt-dir is exactly contained under that root; else a
      ;; BLOCKING (nil-pid) identity (never probed) so the record fails safe.
      ;; A recovery pid read from the store's `sql-server.info' is an ON-DISK
      ;; read, so it is gated by `identity-ok' (never a swapped/recreated root).
      (let* ((pid (or (plist-get record :server-pid)
                      (and identity-ok
                           (dorg-test--registry-recover-server-pid record))))
             (dolt-dir (plist-get record :dolt-dir))
             (port (plist-get record :port)))
        (cond
         ;; Exact, containable, real server identity -> probe/kill by its pid.
         ((and eligible (integerp pid) dolt-dir
               (dorg-test--dir-contained-p dolt-dir root))
          (emit (list :pid pid :port port :dolt-dir dolt-dir)))
         ;; Evidence of a server we cannot exactly identify -> BLOCKING nil-pid
         ;; identity so the record PRESERVES: an explicit/recovered pid we could
         ;; not contain (ineligible root, or a dolt-dir advertised outside it),
         ;; on-disk server metadata under the dolt-dir, a live listener on the
         ;; recorded port, OR process-table evidence that is not a confirmed
         ;; absence (`live' or `unknown').  The last two cover the post-
         ;; `start-process' window: a bound orphan is caught by the port; a
         ;; just-forked child not yet listening with no metadata is PORT-DEAD but
         ;; PROCESS-ALIVE, so only the process table sees it (reviewer: port-dead
         ;; != process-dead).  A scan that could not complete (`unknown') also
         ;; blocks -- an inconclusive probe must never read as no-server (reviewer
         ;; re-cert #3: no fail-open).  Confirmed-death/exact-identity safety is
         ;; intact -- nothing is killed, and any live/undetermined process keeps
         ;; the root.
         ((or (integerp pid)
              (and identity-ok dolt-dir
                   (dorg-test--store-has-metadata-file-p dolt-dir))
              (and (integerp port) (dorg-dolt-server--port-live-p port))
              (and dolt-dir
                   (not (eq 'none (dorg-test--live-dolt-process-evidence
                                   dolt-dir)))))
          (emit (list :pid nil :port port :dolt-dir dolt-dir)))
         ;; PERSIST-BEFORE-SPAWN RECOVERY (tracked-case): the record names a
         ;; dolt-dir but has NO recorded/recoverable pid, NO on-disk server
         ;; metadata, NOTHING listening on its port, AND process-table evidence
         ;; CONFIRMS `none' (no live dolt under it, scan completed clean) — the
         ;; crash landed AFTER the durable write but BEFORE (or with no
         ;; surviving) `start-process', so no server exists.
         ;; Implicate NO server: the identity-intact empty root is reaped and the
         ;; record consumed, so a later shared start is NOT permanently refused
         ;; (the defect reviewer rejected 297209c for).
         (t nil)))
      ;; Discovered stores under an identity-verified root (contained by
      ;; construction — `--owned-store-dirs' already prefix-filters canonical
      ;; store dirs).  The scan reads on-disk content, so it needs `identity-ok'.
      (when identity-ok
        (dolist (store (ignore-errors (dorg-test--owned-store-dirs root)))
          (dolist (id (dorg-test--store-server-identities store))
            (emit id)))))
    (nreverse ids)))

(defun dorg-test--reap-stale-server-registry ()
  "Reap ownership records left by dead test runs; confirm-before-destroy.
One non-recursive listing of `dorg-test--registry-dir' (a missing directory
is a no-op); every record is guarded so one bad datum never blocks the rest.
Only records whose owner Emacs is PROVABLY dead are considered — liveness t
or `unknown' keeps the record, its root, and its servers entirely untouched
(fail-safe for concurrent lanes).

The de-duplication that removed the in-memory owned-server shadow puts the
whole recovery burden here, so the destroy order is strict and confirmed:

  PHASE 1 — across the WHOLE listing, DISCOVER every server each dead-owner
  record implicates (its own exact fields AND a scan of its root's on-disk
  `.dolt/sql-server.info'/`server.claim' metadata) and confirm-reap each
  (`dorg-test--confirm-server-gone') BEFORE any root is deleted.  A paired
  crash (a root record plus the shared/server record for one root) therefore
  has its pid recovered from `sql-server.info' before that file can be
  destroyed, and a non-shared lifecycle-test server under a root record is
  discovered and killed rather than silently buried.

  PHASE 2 — a record's root is deleted, and only then the record consumed,
  ONLY when EVERY server it implicates is confirmed `gone' AND the root
  delete did not fail.  A discovered metadata-bearing store whose pid is
  unknown can never be `gone' and so BLOCKS its record; any live-unknown-
  identity, unknown-liveness, kill failure, still-live server, or root-delete
  failure PRESERVES the root AND the record (never destroying metadata) so a
  later start retries.  A second run over the same fully-reaped records
  no-ops.

Identities are SNAPSHOT ONCE (before Phase 1) and the SAME set is reused in
Phase 2 — so a paired record's Phase-2 deletion, or a metadata re-read, can
never turn an unknown store into an empty (clear) set.

RETURNS a list of `(file . status)' for every DEAD-OWNER record that could NOT
be fully reaped and consumed (a server left unconfirmed, or a root/record
delete failure) so the runner can refuse to start a fresh shared server on top
of accumulating orphans.  Live-owner / unknown-owner records (concurrent
lanes) are skipped and NEVER reported — they must not block a new start."
  (let (failures)
    (when (and (stringp dorg-test--registry-dir)
               (file-directory-p dorg-test--registry-dir))
      (let (stale)
        (dolist (file (directory-files dorg-test--registry-dir t "\\.eld\\'"))
          (let ((record (dorg-test--registry-read file)))
            (cond
             ;; Unreadable/malformed .eld: ownership cannot be determined — do
             ;; NOT guess or delete.  Preserve the file and report a hard
             ;; failure that BLOCKS a fresh shared start (unknown durable
             ;; ownership could be a live orphan).  Schema is validated BEFORE
             ;; owner liveness: a required string `:root' is load-bearing, so a
             ;; record missing it blocks even under a LIVE owner (it can never
             ;; be safely reaped and must not silently pass as a concurrent
             ;; lane).
             ((not (consp record)) (push (cons file 'unreadable) failures))
             ((not (integerp (plist-get record :owner-pid)))
              (push (cons file 'malformed) failures))
             ((not (stringp (plist-get record :root)))
              (push (cons file 'malformed-root) failures))
             ;; Dead owner -> a reap candidate.  Snapshot its implicated
             ;; identities ONCE, now, while all on-disk metadata is present.
             ((null (dorg-dolt-server--pid-live-p (plist-get record :owner-pid)))
              (let ((ids (condition-case _e
                             (dorg-test--record-server-identities record)
                           (error 'error))))
                (if (eq ids 'error)
                    (push (cons file 'identity-error) failures)
                  (push (list file record ids) stale))))
             ;; Live / unknown-liveness owner (concurrent lane): skip, never a
             ;; failure — it must not block a new start.
             (t nil))))
        (setq stale (nreverse stale))
        ;; PHASE 1: confirm-reap every implicated server before ANY deletion,
        ;; using the snapshot.  Status is keyed by the EXACT identity tuple
        ;; (pid . port . dolt-dir) — never by pid alone — so one pid's `gone'
        ;; verdict can NEVER certify a different port/dir tuple.
        (let ((status (make-hash-table :test 'equal)))
          (cl-flet ((idkey (id) (list (plist-get id :pid)
                                      (plist-get id :port)
                                      (plist-get id :dolt-dir))))
            (dolist (entry stale)
              (dolist (id (nth 2 entry))
                (let ((pid (plist-get id :pid))
                      (key (idkey id)))
                  (when (and (integerp pid) (not (gethash key status)))
                    (puthash key (dorg-test--confirm-server-gone id) status)))))
            ;; PHASE 2: delete root + consume record ONLY when the SNAPSHOT is
            ;; fully confirmed.  A nil-pid (blocking) identity can never be
            ;; `gone', so it blocks — clear requires EVERY identity to have an
            ;; integer pid confirmed `gone' at its exact tuple.  Anything
            ;; unreaped is collected as a dead-owner failure.
            (dolist (entry stale)
              (let* ((file (nth 0 entry))
                     (record (nth 1 entry))
                     (ids (nth 2 entry))
                     (clear (cl-every
                             (lambda (id)
                               (and (integerp (plist-get id :pid))
                                    (eq 'gone (gethash (idkey id) status))))
                             ids)))
                (if (not clear)
                    (push (cons file 'server-unconfirmed) failures)
                  (let ((root-status
                         (condition-case _e
                             (dorg-test--reap-stale-root
                              (plist-get record :root)
                              (plist-get record :root-id))
                           (error 'failed))))
                    (cond
                     ;; An eligible-delete failure: preserve for retry.
                     ((eq root-status 'failed)
                      (push (cons file 'root-delete-failed) failures))
                     ;; An EXISTING ineligible root (or missing/non-string
                     ;; root): preserve — never consume over a root we refuse
                     ;; to own, even with zero identities.
                     ((eq root-status 'ineligible)
                      (push (cons file 'root-ineligible) failures))
                     ;; `deleted' or `absent' -> nothing left to protect, so
                     ;; consume the durable record.
                     (t
                      (condition-case _e (delete-file file)
                        ((error quit)
                         (push (cons file 'record-delete-failed) failures)))))))))))))
    (nreverse failures)))

(defun dorg-test--canonical-path (path &optional directory)
  "Return a stable absolute path for PATH.
When DIRECTORY is non-nil, return a slash-terminated directory name."
  (let ((expanded (expand-file-name path)))
    (setq expanded (if (file-exists-p expanded)
                       (file-truename expanded)
                     (expand-file-name expanded)))
    (if directory
        (file-name-as-directory expanded)
      expanded)))

(defun dorg-test--path-file-identity (path)
  "Return PATH's observed filesystem tuple `(DEVICE . INODE)', or nil.
`file-attribute-file-identifier' distinguishes concurrently allocated directory
objects, but filesystems may reuse a tuple after deletion.  Callers therefore
treat a mismatch as proof of replacement; a match is only best-effort evidence.
A terminal SYMLINK is refused (nil): a symlink swapped in over the registered
path must never let its target's identity stand in for the original directory.

PATH must resolve to a REAL DIRECTORY — a non-directory (a regular file, a
fifo, or a missing path) is refused (nil).  A root we own is ALWAYS a directory,
and this single invariant unifies both ownership gates: an entry-time regular
file captures a nil identity, so `dorg-test--register-owned-root' fails LOUD
before tracking it; and a registered directory later replaced by a file yields a
nil current identity, so `dorg-test--identity-intact-p' fails closed and
`dorg-test--delete-owned-root' returns `root-identity-mismatch' (never skipping
the delete and then falsely unregistering + certifying a present file as
`gone')."
  (let ((dfn (directory-file-name path)))
    (and (not (file-symlink-p dfn))
         (file-directory-p dfn)
         (let ((attrs (file-attributes dfn)))
           (and attrs (file-attribute-file-identifier attrs))))))

(defun dorg-test--identity-intact-p (path expected-id)
  "Non-nil when PATH's current filesystem tuple matches EXPECTED-ID.
Fail-closed: a nil EXPECTED-ID (unregistered, or a LEGACY record with no
captured identity), a nil current identity (PATH missing, a symlink, or
unstattable), or an observed mismatch returns nil, so callers refuse before
scanning, probing, or deleting.  A matching device+inode tuple is best-effort:
filesystems may reuse it after deletion.  Callers validate before scanning and
again immediately before deletion to narrow, not close, the check/delete race."
  (and expected-id
       (let ((current (dorg-test--path-file-identity path)))
         (and current (equal current expected-id)))))

(defun dorg-test--register-owned-root (root &optional _reason)
  "Register ROOT as owned by this test run and return its canonical path.
Persists a durable ownership record so a fatal signal (which runs zero
cleanup Lisp) leaves the root reapable at the next test startup.  The durable
record is the SOLE crash-net now that the in-memory owned-server shadow is
gone, so the persist FAILS LOUD: a failed `dorg-test--registry-write' rolls
back the in-memory insertion and re-signals rather than accepting a root with
no crash-net (a swallowed failure would strand any server under ROOT
untraceably after a fatal signal).

The captured filesystem tuple (`file-attribute-file-identifier' — device +
inode) is the in-memory hash VALUE and the durable record's `:root-id'.  A later
reap refuses an observed mismatch before scanning or deleting and never
re-`file-truename's the stored path.  A matching tuple remains best-effort
because a filesystem may reuse it after deletion."
  ;; Reject a caller-supplied TERMINAL symlink BEFORE canonicalizing: `canonical-
  ;; path' `file-truename's it, so the post-canon identity would be the symlink
  ;; TARGET's — masking that the registered path is a link we do not own.
  (when (file-symlink-p (directory-file-name (expand-file-name root)))
    (error "dorg-test--register-owned-root: refusing to register a symlink root %s"
           root))
  (let* ((canon (dorg-test--canonical-path root t))
         (id (dorg-test--path-file-identity canon)))
    ;; A nil identity (canon missing, a symlink, or unstattable) would create an
    ;; ownership entry that can NEVER be safely reaped (`identity-intact-p'
    ;; fails closed on a nil expected-id).  Fail LOUD before any puthash/write
    ;; so `make-owned-temp-dir' rolls the freshly-created dir back rather than
    ;; leaking an unreapable root.
    (unless id
      (error "dorg-test--register-owned-root: cannot capture filesystem identity for %s"
             canon))
    (puthash canon id dorg-test--owned-roots)
    (condition-case err
        (dorg-test--registry-write (dorg-test--registry-root-key canon)
                                   (list :owner-pid (emacs-pid)
                                         :root canon
                                         :root-id id))
      ((error quit)
       (remhash canon dorg-test--owned-roots)
       (signal (car err) (cdr err))))
    canon))

(defun dorg-test--make-owned-temp-dir (prefix &optional reason)
  "Create and register a test-owned temp directory with PREFIX.
If durable registration FAILS (fail-loud persist), the freshly-created exact
temp directory is DELETED before the error propagates — a fail-loud persist
must not leak an untracked on-disk root.  If that rollback delete ALSO fails,
the directory cannot be silently abandoned: the run RETAINS in-memory
ownership of the exact root (so a later reap retries it) and signals a
COMPOUND `dorg-dolt-fixture-cleanup-error' carrying BOTH the registration and
the delete failure — never reporting only the persist error while leaking an
untracked root."
  (let ((dir (make-temp-file prefix t)))
    (condition-case reg-err
        (dorg-test--register-owned-root dir reason)
      ((error quit)
       ;; Capture the rollback-delete outcome in its OWN handler so the
       ;; re-signal of reg-err below is NOT swallowed as a delete failure.
       (let ((del-err (condition-case e (progn (delete-directory dir t) nil)
                        ((error quit) e))))
         (if (null del-err)
             ;; Rollback delete succeeded: re-signal the ORIGINAL registration
             ;; failure (in-memory ownership was already rolled back).
             (signal (car reg-err) (cdr reg-err))
           ;; Rollback delete ALSO failed: keep the root trackable for retry
           ;; (with its captured identity) and fail loud with BOTH failures.
           (let ((canon (dorg-test--canonical-path dir t)))
             (puthash canon (dorg-test--path-file-identity canon)
                      dorg-test--owned-roots))
           (signal 'dorg-dolt-fixture-cleanup-error
                   (list :registration reg-err :cleanup del-err :root dir))))))))

(defun dorg-test--unregister-owned-root (root)
  "Drop ROOT's ownership: durable record FIRST (checked), then in-memory.
The durable `dorg-test--registry-delete' is NOT swallowed: a failure
PROPAGATES and RETAINS the in-memory `--owned-roots' entry, so ownership stays
trackable for a retry — the in-memory drop happens ONLY after the durable
delete succeeds (the reverse order would strand a record with no in-memory
owner).

Uses the ORIGINAL registered key (a bare `expand-file-name' normalization,
NEVER re-`file-truename'd — the directory is typically already deleted here, and
a symlink swapped in over its path must not redirect the drop)."
  (let ((canon (file-name-as-directory (expand-file-name root))))
    (dorg-test--registry-delete (dorg-test--registry-root-key canon))
    (remhash canon dorg-test--owned-roots)))

(defun dorg-test--owned-root-for-path (path)
  "Return the registered owned root containing PATH, or nil."
  (let ((canon (dorg-test--canonical-path path nil))
        match)
    (maphash
     (lambda (root _)
       (when (and (not match)
                  (string-prefix-p root canon))
         (setq match root)))
     dorg-test--owned-roots)
    match))

(defun dorg-test--owned-root-p (path)
  "Return non-nil when PATH is below a root owned by this test run."
  (and (dorg-test--owned-root-for-path path) t))

(defun dorg-test--store-dir-from-server-metadata (file)
  "Derive the Dolt store dir from server metadata FILE."
  (let* ((base (file-name-nondirectory file))
         (parent (file-name-nondirectory
                  (directory-file-name (file-name-directory file)))))
    (cond
     ((and (equal parent ".dorg")
           (member base '("server.claim" "server.pid")))
      (file-name-directory
       (directory-file-name (file-name-directory file))))
     ((and (equal parent ".dolt")
           (equal base "sql-server.info"))
      (file-name-directory
       (directory-file-name (file-name-directory file))))
     (t nil))))

(defun dorg-test--owned-store-dirs (root)
  "Return canonical Dolt store dirs with server metadata under owned ROOT."
  (let ((root (dorg-test--canonical-path root t))
        stores)
    (when (file-directory-p root)
      (dolist (file (directory-files-recursively
                     root
                     "\\(?:server\\.claim\\|server\\.pid\\|sql-server\\.info\\)\\'"))
        (when-let ((store (dorg-test--store-dir-from-server-metadata file)))
          (let ((canon (ignore-errors
                         (dorg-dolt-server--canonical-dir store))))
            (when (and canon
                       (string-prefix-p root canon))
              (cl-pushnew canon stores :test #'equal))))))
    stores))

(defun dorg-test--owned-procs-under (root)
  "Return canonical dolt-dirs of `--procs' entries contained under owned ROOT.
The production `dorg-dolt-server--procs' registry is an ADDITIONAL discovery
source (NOT a second ownership shadow): a live owned server whose on-disk
claim/`sql-server.info' is already absent would otherwise be invisible to the
metadata scan, letting the root be deleted while its process is live.  Only
entries whose canonical dir is exactly CONTAINED under ROOT are returned, so a
sibling/outside `--procs' entry is never touched."
  (let ((root (dorg-test--canonical-path root t))
        dirs)
    (maphash
     (lambda (canon _entry)
       (when (and (stringp canon) (dorg-test--dir-contained-p canon root))
         (cl-pushnew canon dirs :test #'equal)))
     dorg-dolt-server--procs)
    dirs))

(defun dorg-test--shared-server-pid ()
  "Return the live shared test server pid, or nil."
  (and dorg-test--server-proc
       (process-live-p dorg-test--server-proc)
       (process-id dorg-test--server-proc)))

(defun dorg-test--shared-server-record-p (server-pid port)
  "Return non-nil when SERVER-PID or PORT belongs to the shared test server."
  (or (and (integerp server-pid)
           (equal server-pid (dorg-test--shared-server-pid)))
      (and (integerp port)
           (equal port dorg-test--server-port))))

;; NOTE (tracked-case de-duplication): the in-memory owned-server shadow
;; (`dorg-test--owned-server-records' + `dorg-test--record-owned-server' +
;; `dorg-test--reap-owned-root-records') and the `:around' advice on
;; `dorg-dolt-server--publish-claim' that auto-mirrored published claims into
;; it are REMOVED.  It duplicated the durable tracked-case registry (the fatal-
;; signal crash-net, reaped at next-start by
;; `dorg-test--reap-stale-server-registry') and the normal-context owned-root
;; reaper below (`dorg-test--owned-store-dirs' → `dorg-test--reap-owned-store',
;; which scans on-disk sql-server metadata).  Those two paths are the sole
;; owner-cleanup substrate now.

(defun dorg-test--reap-owned-store (dolt-dir)
  "Confirm-reap the sql-server(s) for test-owned DOLT-DIR; return a status symbol.
Snapshots EVERY on-disk identity (`dorg-test--store-server-identities' — both
the claim overlay AND the native `sql-server.info', distinct pids alike) BEFORE
any stop or metadata mutation, so a partial/failed `--stop' that removes the
claim yet leaves a pid live cannot degrade the snapshot to `no server'.  Then
closes the connection, stops an in-`--procs' entry, and confirms EACH
snapshot identity absent (`dorg-test--confirm-server-gone'; the suite shared
server is spared as not-ours).  A nil-pid identity (metadata FILE present, no
readable pid) blocks — a distinct server the proc entry cannot certify — EXCEPT
the ground-truth synthetic/never-served provisional overlay (tracked-case), which
`dorg-test--store-server-identities' collapses to no identity, so this reap
returns `gone' and clears the fixture claim/record.
A metadata-FREE store yields no identity and is cleared ONLY by the exact
snapshotted proc entry being proven dead after the stop.  Returns:
  `gone'  — EVERY identity confirmed absent (or none but the shared server) AND
            the snapshotted proc (if any) proven dead — and only then deletes
            the claim + durable store record;
  `unknown'/`still-live'/`error'/`proc-still-live' — ANY identity unconfirmed
            or the snapshotted proc still live: the claim, root, and durable
            records are PRESERVED so the caller refuses to delete the root.
A store not under an owned root is `gone' (not ours).  Confirming by pid — not
by port going down — refuses to declare a server gone while any recorded pid
may still be live."
  (let* ((canon (ignore-errors (dorg-dolt-server--canonical-dir dolt-dir)))
         (root (and canon (dorg-test--owned-root-for-path canon))))
    (if (not root)
        'gone
      ;; Snapshot BOTH the on-disk identities AND the in-memory `--procs' entry
      ;; BEFORE any stop/metadata mutation, so a partial/failed `--stop' that
      ;; removes metadata yet leaves a proc live cannot degrade either source.
      (let ((ids (dorg-test--store-server-identities canon))
            (proc-entry (and canon (gethash canon dorg-dolt-server--procs))))
        (when (fboundp 'dorg-sql--close-connection)
          (ignore-errors (dorg-sql--close-connection canon)))
        (when proc-entry
          (ignore-errors (dorg-dolt-server--stop canon)))
        ;; Confirm EVERY snapshot identity absent; the shared server is spared.
        (let ((status 'gone))
          (dolist (id ids)
            (let* ((pid (plist-get id :pid))
                   (port (plist-get id :port))
                   (s (cond
                       ((and (integerp pid)
                             (dorg-test--shared-server-record-p pid port))
                        'gone)
                       ;; A nil-pid on-disk identity (metadata FILE present but
                       ;; no readable pid) ALWAYS BLOCKS: it names a DISTINCT
                       ;; server the snapshotted proc entry cannot certify, so a
                       ;; proc-entry death must NEVER be allowed to clear it (a
                       ;; different, possibly-live server would be buried).
                       ;; `store-server-identities' only emits this identity
                       ;; when a metadata file actually exists — a metadata-FREE
                       ;; store yields no identity and is governed solely by the
                       ;; proc-entry-dead check below.
                       ((not (integerp pid)) 'unknown)
                       (t (dorg-test--confirm-server-gone id)))))
              (unless (eq s 'gone) (setq status s))))
          ;; The EXACT `--procs' proc snapshotted BEFORE the stop MUST be proven
          ;; dead: a live proc (stop failed/no-op) preserves root+records.  This
          ;; is the ONLY authority that clears a metadata-FREE store (empty
          ;; `ids') — a contained proc that dies after the stop makes it `gone';
          ;; a still-live proc blocks.
          (when proc-entry
            (let ((proc (car proc-entry)))
              (when (and proc (process-live-p proc))
                (setq status 'proc-still-live))))
          (when (eq status 'gone)
            (ignore-errors (dorg-dolt-server--claim-delete-file canon))
            (ignore-errors
              (dorg-test--registry-delete
               (dorg-test--registry-store-key canon))))
          status)))))

(defun dorg-test--reap-owned-root (root &optional unregister)
  "Confirm-reap every test-owned sql-server under ROOT; return a status symbol.
Servers are discovered from BOTH on-disk `dolt sql-server' metadata
\(`dorg-test--owned-store-dirs') AND the in-memory `--procs' registry
\(`dorg-test--owned-procs-under' — a live owned server with no on-disk metadata
would otherwise be invisible), and each is confirm-reaped through
`dorg-test--reap-owned-store'.  Returns `gone' only when EVERY discovered
server is confirmed absent, else the first blocking status.

Reaping NEVER deletes the directory.  When UNREGISTER is non-nil AND every
server is confirmed gone, it drops the root's ownership (in-memory + durable
record) WITHOUT deleting the directory — the persistent-inspection-artifact
contract (`tests/dorg-integration-tests.el' leaves its work dir on disk but
un-owned, so the runner's `dorg-test--reap-owned-roots' no longer deletes it).
An unconfirmed reap leaves ROOT, its metadata, AND its ownership intact for
retry regardless of UNREGISTER.  `dorg-test--delete-owned-root' is the only
path that deletes the directory.

IDENTITY GATE (fail-closed, BEFORE any scan): ROOT is normalized to its
registered key with a bare `expand-file-name' — NEVER re-`file-truename'd, so a
symlink swapped in over the path is not resolved to its outside target — and
the current filesystem tuple must match the captured value
\(`dorg-test--identity-intact-p' against the in-memory registration).  A symlink
swap, an observed distinct replacement, a missing registration, or a gone
directory returns `root-identity-mismatch' with ZERO store scans, probes, or
kills.  A matching reusable device+inode tuple is not historical proof."
  (let ((canon (file-name-as-directory (expand-file-name root))))
    (if (not (dorg-test--identity-intact-p
              canon (gethash canon dorg-test--owned-roots)))
        'root-identity-mismatch
      (let ((dirs (cl-union (dorg-test--owned-store-dirs canon)
                            (dorg-test--owned-procs-under canon)
                            :test #'equal))
            (status 'gone))
        (dolist (store dirs)
          (let ((s (dorg-test--reap-owned-store store)))
            (unless (eq s 'gone) (setq status s))))
        (when (and (eq status 'gone) unregister)
          (dorg-test--unregister-owned-root canon))
        status))))

(defun dorg-test--delete-owned-root (root)
  "Confirm-reap servers under ROOT, delete the exact root, THEN drop ownership.
Strict order (OC 02:51/03:08): confirm EVERY owned server gone
\(`dorg-test--reap-owned-root' → `gone'); delete the exact directory; and ONLY
after a successful delete unregister the root (in-memory `--owned-roots' +
durable record).  Any prior failure — an unconfirmed server reap or a
`delete-directory' signal — PRESERVES the directory, the durable record, AND
the in-memory ownership for retry (an unconfirmed reap returns its blocking
status; a delete signal propagates before either registration is dropped).
Returns `gone' on full success.

The root key is a bare `expand-file-name' normalization (NEVER re-
`file-truename'd).  `reap-owned-root' already fail-closed-gated the filesystem
tuple before any scan; this revalidates that tuple immediately before
`delete-directory' to narrow the check/delete race.  An observed mismatch
returns `root-identity-mismatch' and deletes nothing; tuple reuse remains a
documented limitation."
  (if (not root)
      'gone
    (let* ((canon (file-name-as-directory (expand-file-name root)))
           (status (dorg-test--reap-owned-root canon)))
      (if (not (eq status 'gone))
          status
        ;; Re-validate immediately before deletion to narrow the race: a symlink
        ;; or observed identity mismatch after the reap-time gate must refuse.
        (if (not (dorg-test--identity-intact-p
                  canon (gethash canon dorg-test--owned-roots)))
            'root-identity-mismatch
          ;; Servers confirmed gone AND the observed tuple still matches: delete
          ;; the root FIRST; a signal propagates with both registrations intact.
          (when (file-directory-p canon)
            (delete-directory (directory-file-name canon) t))
          ;; Only a successful delete drops ownership.
          (dorg-test--unregister-owned-root canon)
          'gone)))))

(defun dorg-test--reap-owned-roots ()
  "Drive the FULL delete path for every owned root; RETURN accumulated failures.
Calls `dorg-test--delete-owned-root' (confirm servers gone → delete the exact
root → drop ownership) for each root under an error/quit guard so one stuck
root never blocks the rest, but does NOT swallow failure: a root whose servers
cannot all be confirmed gone, or whose delete fails, is PRESERVED (directory,
durable record, and in-memory ownership intact) and collected as
`(root . status)'.  Returns the list of failures (nil when every root reaped
and deleted cleanly) so the runner forces a nonzero exit instead of masking a
stuck reap."
  (let (roots failures)
    (maphash (lambda (root _) (push root roots)) dorg-test--owned-roots)
    (dolist (root roots)
      (let ((status (condition-case err
                        (dorg-test--delete-owned-root root)
                      ((error quit) (cons 'error err)))))
        (unless (eq status 'gone)
          (push (cons root status) failures))))
    (nreverse failures)))

;; Zero-fatal-hook contract (tracked-case): the noninteractive test process
;; contributes NOTHING to `kill-emacs-hook'.  A SIGTERM can interrupt Emacs
;; mid-GC, and Emacs 30's signal_or_quit aborts (SIGABRT) on ANY Lisp error
;; while gc_in_progress — BEFORE condition handlers run, so `ignore-errors'
;; cannot help (incident F2C649B7).  With an empty fatal path nothing can
;; signal, allocate, or wait, whatever C code the signal interrupted, and
;; the process dies with original signal semantics.  All cleanup lives in
;; the runner's normal-context unwind paths plus the durable ownership
;; registry above; the production `dorg-dolt-server-stop-all' registration
;; is removed for the same reason — its coverage moves to the runner's
;; explicit unwind call.  Interactive sessions keep production behavior.
(when noninteractive
  (remove-hook 'kill-emacs-hook #'dorg-dolt-server-stop-all))

(defun dorg-test--server-port-advice (orig-fn dolt-dir)
  "Return shared test server port for databases under data-dir.
Falls through to ORIG-FN for other directories."
  (if (and dorg-test--server-port
           dorg-test--server-data-dir
           (string-prefix-p
            (file-truename dorg-test--server-data-dir)
            (file-truename (expand-file-name dolt-dir))))
      dorg-test--server-port
    (funcall orig-fn dolt-dir)))

;;; ─── Legacy Shared-Server Helpers (thin wrappers) ─────────────────────
;;
;; The suite-wide shared server and its per-test databases are owned by the
;; opaque fixture API defined below.  These `dorg-test--' names survive only
;; as thin adapters so the ~40 consumer suites and the runner keep resolving
;; the same symbols; each DELEGATES to the fixture — there is exactly ONE
;; spawn path (`dorg-dolt-fixture--shared-start') and ONE CREATE/DROP DATABASE
;; seam (`dorg-dolt-fixture--exec-ddl'), never a second copy.

(defun dorg-test--start-shared-server ()
  "Ensure the suite-wide shared dolt sql-server; return its port, or nil.
Thin wrapper over `dorg-dolt-fixture-shared-ensure': it publishes the
lease's port/data-dir/proc into the legacy `dorg-test--server-*' globals the
consumer suites read and installs the `dorg-dolt-server-port' shared-port
advice.  Returns nil (no error) ONLY when the shared path is legitimately unavailable —
dolt missing/unsupported or mysql.el absent — so the caller falls back to the
copy-repo/CLI-seam path.  A spawn/readiness/connect failure instead SIGNALS and
propagates unchanged: the runner treats a signal as an infrastructure failure
\(exit 2, cleanup run), never downgrading it to a green CLI fallback."
  (when (and (dorg-dolt-supported-p) dorg-test--mysql-available)
    (let ((lease (dorg-dolt-fixture-shared-ensure)))
      (setq dorg-test--server-data-dir (dorg-dolt-fixture--lease-data-dir lease)
            dorg-test--server-proc (dorg-dolt-fixture--lease-proc lease)
            dorg-test--server-port (dorg-dolt-fixture--lease-port lease)
            dorg-test--mysql-conn dorg-dolt-fixture--conn)
      (advice-add 'dorg-dolt-server-port :around
                  #'dorg-test--server-port-advice)
      dorg-test--server-port)))

(defun dorg-test--stop-shared-server ()
  "Stop the suite-wide shared server and clear the legacy globals.
Thin wrapper that DRIVES `dorg-dolt-fixture-close' on the singleton server
lease and FAILS LOUD: the shared-port advice and the `dorg-test--server-*'
globals are cleared ONLY after the fixture close succeeds.  A close failure
propagates (with the lease/registry left retryable) so the runner catches it,
reports, and forces a nonzero exit — the legacy globals are NOT cleared on a
failed close."
  (when dorg-dolt-fixture--shared-lease
    (dorg-dolt-fixture-close dorg-dolt-fixture--shared-lease))
  (ignore-errors
    (advice-remove 'dorg-dolt-server-port #'dorg-test--server-port-advice))
  (setq dorg-test--server-data-dir nil
        dorg-test--server-port nil
        dorg-test--server-proc nil
        dorg-test--mysql-conn nil))

(defun dorg-test--create-test-db ()
  "Create a fresh, schema-seeded test database on the shared server.
Thin wrapper: acquires an isolated store via `dorg-dolt-fixture-store-acquire'
\(the ONE CREATE DATABASE seam), then applies the core schema and an initial
commit under strict `server' transport — the same server-required path the
suite exercises, with no dependence on the `auto'/`cli' policy the
server-only collapse (tracked-case) deletes.  Returns the store's dolt-dir;
`dorg-test--destroy-test-db' releases it.  If the schema/commit
initialization fails, the freshly-acquired store lease is RELEASED before the
error propagates, so a failed seed never leaks a store.  The release is NOT
swallowed: if it too fails, that error surfaces (fail loud) and the store
lease stays registered (retryable) rather than being silently forgotten."
  (let* ((lease (dorg-dolt-fixture-store-acquire))
         (dolt-dir (dorg-dolt-fixture-lease-dolt-dir lease))
         (ok nil))
    (unwind-protect
        (progn
          (let ((dorg-sql-transport 'server))
            (dorg-sql-batch dolt-dir dorg-schema-ddl)
            (dorg-commit dolt-dir "init"))
          (setq ok t)
          dolt-dir)
      (unless ok
        (dorg-dolt-fixture-store-release lease)))))

(defun dorg-test--destroy-test-db (dolt-dir)
  "Release the shared-server test database at DOLT-DIR.
Thin wrapper: looks up the tracked store lease for DOLT-DIR and DROPs it
through `dorg-dolt-fixture-store-release' (the ONE DROP DATABASE seam).  A
dolt-dir with no tracked lease (e.g. a standalone copy-repo store) falls
back to directory deletion."
  (when dolt-dir
    (let* ((canon (ignore-errors (dorg-dolt-server--canonical-dir dolt-dir)))
           (lease (and canon (gethash canon dorg-dolt-fixture--store-leases))))
      (if lease
          (dorg-dolt-fixture-store-release lease)
        (when (file-directory-p dolt-dir)
          (ignore-errors (delete-directory dolt-dir t)))))))

;;; ─── Opaque Fixture / Lease API (tracked-case) ──────────────────────────
;;
;; The supported, public surface.  Callers receive an OPAQUE lease handle
;; — a `cl-defstruct' with a private constructor and a double-dash
;; `:conc-name' so NO pid/proc/claim/port accessor is generated — and the
;; only public reader is `dorg-dolt-fixture-lease-dolt-dir'.  The port is
;; never exposed: queries resolve it through the normal production
;; transport, keeping the handle opaque and on the supported path.

(cl-defstruct (dorg-dolt-fixture-lease
               (:constructor dorg-dolt-fixture--make-lease)
               (:conc-name dorg-dolt-fixture--lease-)
               (:predicate dorg-dolt-fixture--lease-p)
               (:copier nil))
  "Opaque handle to a fixture-managed Dolt resource.
KIND is `server' (the shared sql-server) or `store' (an isolated database
on it).  Internal slots carry no public accessor — the only supported
reader is `dorg-dolt-fixture-lease-dolt-dir'.  STOPPED records that a
server lease's process has been confirmed stopped, so a close whose artifact
cleanup partially failed can be retried WITHOUT re-issuing the stop.  ROOT-ID
is the captured filesystem tuple of the server's data-dir.  Finalize refuses
symlinks and observed mismatches before deletion; a matching tuple is
best-effort because filesystems may reuse device+inode after deletion."
  kind dolt-dir data-dir db-name proc port registry-key buffer stopped closed
  root-id)

(defvar dorg-test-diag-dir nil
  "Directory receiving test-only diagnostics, or nil.
Set by the batch runner from DORG_TEST_DIAG_DIR.  When non-nil the shared
server's output is tee'd from startup to <dir>/shared-server-<port>.log by a
process filter (see `dorg-dolt-fixture--diag-attach-sink').  Test/CI
evidence only; production code never reads this.")

(defun dorg-dolt-fixture--diag-attach-sink (proc port)
  "When `dorg-test-diag-dir' is set, tee PROC's output to a per-PORT log file.
Installs a process filter that appends every chunk of the shared server's
output to <dir>/shared-server-<port>.log before the default filter inserts it
into the process buffer, so the capture is complete whatever kills the
buffer later.  Test/CI evidence only; a sink failure never affects the
server or the tests."
  (when (and dorg-test-diag-dir (stringp dorg-test-diag-dir) (processp proc))
    (condition-case err
        (let ((file (expand-file-name (format "shared-server-%s.log" port)
                                      dorg-test-diag-dir)))
          (make-directory dorg-test-diag-dir t)
          (write-region "" nil file nil 'silent)
          (set-process-filter
           proc
           (lambda (p chunk)
             (condition-case nil
                 (write-region chunk nil file 'append 'silent)
               (error nil))
             (internal-default-process-filter p chunk)))
          (message "dorg-test: diag server sink -> %s" file))
      (error (message "dorg-test: diag server sink failed: %s"
                      (error-message-string err))))))

(defvar dorg-dolt-fixture--shared-lease nil
  "The singleton shared-server lease, or nil when none is live.
Cached so `dorg-dolt-fixture-shared-ensure' is idempotent: a second call
reuses ONE server and returns the SAME lease object.")

(defvar dorg-dolt-fixture--failed-start-lease nil
  "Sentinel lease for a start that aborted leaving a LIVE, un-killable process.
A `--shared-start' abort whose kill fails deliberately retains the live proc,
its `--procs' registration, durable record, and data-dir but publishes NO
singleton.  Recorded here so a retry `dorg-dolt-fixture-shared-ensure' FAILS
CLOSED (never spawns a duplicate) until that exact proc is proven dead and
finalized — at which point the sentinel is reclaimed and cleared.")

(defvar dorg-dolt-fixture--conn nil
  "The fixture's mysql.el connection to the shared server, or nil.
Used by `dorg-dolt-fixture-store-acquire'/`-store-release' to run
CREATE/DROP DATABASE against the warm shared server.")

(defvar dorg-dolt-fixture--cold-starts 0
  "Monotonic count of real shared-server `start-process' spawns.
The measurable before/after cold-start metric this bead is graded on;
advanced once the spawn's `start-process' returns a real process (the
cold-start cost is then paid), never before it and never on idempotent
reuse.  A spawn that returns a proc but then fails readiness/connect still
counts — the process genuinely started — and is unwound.")

(defvar dorg-dolt-fixture--store-leases (make-hash-table :test 'equal)
  "Live store leases keyed by their canonical dolt-dir (`file-truename').
`dorg-dolt-fixture-store-acquire' registers here on a confirmed CREATE and
`dorg-dolt-fixture-store-release' removes on a confirmed DROP, so the count
is the inventory `:stores' metric and `dorg-test--destroy-test-db' can
resolve the lease for a dolt-dir.  It is the SOLE store-ownership record —
there is no parallel count or shadow.")

(defvar dorg-dolt-fixture--db-counter 0
  "Counter for the fixture API's isolated store database names.")

(defun dorg-dolt-fixture-lease-dolt-dir (lease)
  "Return the canonical Dolt store directory LEASE manages.
The ONLY public reader of a lease: it exposes the store path and nothing
else, so callers can bind `default-directory'/route queries through the
normal production transport without reaching a port, pid, proc, or claim."
  (dorg-dolt-fixture--lease-dolt-dir lease))

(defun dorg-dolt-fixture--build-server-lease (&rest args)
  "Construct a server lease from ARGS through an ORDINARY (non-inlined) seam.
The `cl-defstruct' constructor `dorg-dolt-fixture--make-lease' is
compiler-macro-expanded at every call site, so rebinding that symbol cannot
intercept lease construction inside `--shared-start'.  This thin wrapper —
`apply' of the constructor, which the compiler macro does NOT rewrite — is the
explicit seam a test mocks to inject a post-connect (post-`--conn') failure and
exercise the failed-start conn-retention unwind."
  (apply #'dorg-dolt-fixture--make-lease args))

(defun dorg-dolt-fixture--shared-start ()
  "Start the shared server as a first-class DOrg-managed server; return a lease.
Persist-before-spawn (tracked-case): the durable ownership record lands
BEFORE `start-process', so no window exists in which a server outlives its
Emacs unrecorded.  On readiness the server is registered in
`dorg-dolt-server--procs' with its `(proc . port)' so the PUBLIC
`dorg-dolt-server-list' sees it live and owned (D4).

TRANSACTIONAL: EVERY allocation — the temp data-dir, the durable ownership
record, the process, its buffer, the `--procs' registration, and the mysql
connection — is created INSIDE the unwind scope and undone if ANY step
\(spawn, readiness probe, or mysql connect) fails, leaving no orphan server,
record, buffer, or directory.  The cold-start metric advances once, and
ONLY once `start-process' has returned a real process."
  (let ((key (dorg-test--registry-shared-key))
        (data-dir nil) (canon nil) (root-id nil) (port nil) (buf nil)
        (proc nil) (conn nil)
        (lease nil) (ok nil) (primary nil) (record-written nil))
    (unwind-protect
        ;; The condition-case only CAPTURES the primary error/quit (to enrich
        ;; the compound report) and re-signals it; the OUTER `unwind-protect'
        ;; is what guarantees cleanup on EVERY nonlocal exit — error, quit
        ;; (C-g), throw — not just ordinary errors.
        (condition-case err
            (progn
              (setq data-dir (file-name-as-directory
                              (make-temp-file "dorg-dolt-fixture-shared-" t))
                    canon (dorg-dolt-server--canonical-dir data-dir)
                    root-id (dorg-test--path-file-identity canon)
                    port (dorg-dolt-server--free-port)
                    buf (generate-new-buffer " *dorg-dolt-fixture-shared*"))
              ;; The captured root identity is REQUIRED before the durable write
              ;; or the spawn: a nil id would persist an unreapable record, so
              ;; fail closed here (cleanup unwinds the fresh data-dir/buffer).
              (unless root-id
                (error "dorg-dolt-fixture--shared-start: cannot capture root identity for %s"
                       canon))
              ;; Persist ownership BEFORE the server can exist — inside the
              ;; guarded scope, so a failure before readiness undoes it too.
              ;; `record-written' flips ONLY after the write succeeds, so the
              ;; abort deletes only a record THIS invocation actually wrote and
              ;; never a preexisting record left after a failed write.  The
              ;; EXACT captured `root-id' is persisted (not recaptured inline).
              (dorg-test--registry-write key (list :owner-pid (emacs-pid)
                                                   :port port
                                                   :root canon
                                                   :dolt-dir canon
                                                   :root-id root-id))
              (setq record-written t)
              (setq proc (start-process "dorg-dolt-fixture-shared" buf
                                        "dolt" "sql-server"
                                        "--host" "127.0.0.1"
                                        "--port" (number-to-string port)
                                        "--data-dir" data-dir))
              ;; A real process now exists — the cold-start cost is paid, so the
              ;; metric advances HERE (never before `start-process').  A
              ;; readiness or connect failure below still unwinds the process,
              ;; but the spawn genuinely happened and is counted.
              (when proc (cl-incf dorg-dolt-fixture--cold-starts))
              (dorg-dolt-fixture--diag-attach-sink proc port)
              (set-process-query-on-exit-flag proc nil)
              ;; Readiness boundary: return only once the server actually
              ;; accepts connections — never deferring the start to first use.
              (dorg-dolt-server--wait-ready port 30 proc)
              ;; First-class registration: the public inventory keys off --procs.
              (puthash canon (cons proc port) dorg-dolt-server--procs)
              (setq conn (mysql-connect :host "127.0.0.1" :port port :user "root"
                                        :password "" :tls nil :read-idle-timeout 5))
              (setq dorg-dolt-fixture--conn conn)
              ;; Construct through the ordinary wrapper seam (NOT the inlined
              ;; constructor) so a post-connect failure is injectable in tests.
              (setq lease (dorg-dolt-fixture--build-server-lease
                           :kind 'server :dolt-dir canon :data-dir data-dir
                           :proc proc :port port :registry-key key :buffer buf
                           :root-id root-id))
              ;; SUCCESS only after the lease exists.
              (setq ok t)
              lease)
          ((error quit)
           (setq primary err)
           (signal (car err) (cdr err))))
      ;; OUTER unwind — runs on success and on EVERY nonlocal exit.  On any
      ;; non-success, attempt EVERY cleanup step, COLLECTING failures so no
      ;; allocation's reclaim is skipped; persist-before-spawn means the
      ;; durable record and data-dir MUST be removed.  If any cleanup failed,
      ;; signal a COMPOUND `dorg-dolt-fixture-cleanup-error' carrying the
      ;; captured PRIMARY and the cleanup failures (the failure is in the
      ;; SIGNAL, not just logs); otherwise let the original nonlocal exit (or
      ;; the re-signaled primary) continue.  Either way no lease or singleton
      ;; is published.
      (unless ok
        (let (cleanup-errors proc-dead)
          ;; The client connection is ours to drop, but the drop must be
          ;; CONFIRMED: only clear the retryable global `dorg-dolt-fixture--conn'
          ;; AFTER a checked disconnect succeeds.  A disconnect failure — or a
          ;; `quit' (C-g) mid-disconnect — RETAINS the exact conn in the global
          ;; so a later drain/finalize retries it (the failed-start sentinel has
          ;; no conn slot; the global IS the retryable handle), and is guarded
          ;; for BOTH `error' and `quit' so the remaining proc/record/dir/buffer
          ;; cleanup and the sentinel still run.
          (when conn
            (condition-case e
                (progn (mysql-disconnect conn)
                       (setq dorg-dolt-fixture--conn nil))
              ((error quit) (push (cons 'mysql-disconnect e) cleanup-errors))))
          ;; PROVE the EXACT spawned process is dead BEFORE releasing any
          ;; runtime registration, the data-dir, or the durable record — a
          ;; live server must never be orphaned from its ownership metadata.
          ;; Every step is guarded for `error' AND `quit' so a C-g cannot abort
          ;; the unwind partway and skip the sentinel that keeps the partial
          ;; reapable.
          (cond
           ((or (null proc) (not (process-live-p proc))) (setq proc-dead t))
           (t
            (condition-case e
                (dorg-dolt-server--kill-process
                 proc dorg-dolt-server-stop-term-grace)
              ((error quit) (push (cons 'kill-process e) cleanup-errors)))
            (setq proc-dead (not (process-live-p proc)))))
          (if (not proc-dead)
              ;; Kill failed / still live: RETAIN runtime (`--procs'), the
              ;; durable record, the data-dir, AND the process buffer so a
              ;; later reap can finish — reclaim NOTHING — and report the live
              ;; process as a cleanup failure.
              (push (cons 'proc-still-live proc) cleanup-errors)
            ;; Proc proven dead — release runtime, then data-dir BEFORE the
            ;; record, then the buffer.
            (when canon (remhash canon dorg-dolt-server--procs))
            (let ((data-dir-ok t))
              (when data-dir
                (let ((dfn (directory-file-name data-dir)))
                  ;; PRESENCE is exists-OR-symlink: a dangling/non-dir swap is
                  ;; present even when `file-directory-p' is nil and must NOT be
                  ;; silently treated as cleaned.  Only a current real directory
                  ;; whose tuple matches the captured value is deleted; a
                  ;; foreign/non-dir replacement — or an uncapturable path
                  ;; (nil root-id) —
                  ;; retains the record for a later reap.
                  (when (or (file-exists-p dfn) (file-symlink-p dfn))
                    (condition-case e
                        (if (and (not (file-symlink-p dfn))
                                 (file-directory-p data-dir)
                                 (dorg-dolt-fixture--data-dir-deletable-p
                                  data-dir root-id))
                            (delete-directory data-dir t)
                          (setq data-dir-ok nil)
                          (push (cons 'data-dir-foreign data-dir) cleanup-errors))
                      ((error quit) (setq data-dir-ok nil)
                             (push (cons 'data-dir-delete e) cleanup-errors))))))
              ;; Delete ONLY a record this invocation wrote, and ONLY once the
              ;; physical store is gone — a failed data-dir delete RETAINS the
              ;; record so the store stays reapable at next start.
              (when (and record-written data-dir-ok)
                (condition-case e (dorg-test--registry-delete key)
                  ((error quit) (push (cons 'registry-delete e) cleanup-errors)))))
            (when (buffer-live-p buf)
              (condition-case e (kill-buffer buf)
                ((error quit) (push (cons 'buffer-kill e) cleanup-errors)))))
          (when cleanup-errors
            ;; ANY retryable leftover after spawn/record allocation — a live
            ;; proc OR a failed data-dir/record/buffer reclaim — retains a
            ;; failed-start SENTINEL (marked `stopped' iff the proc is proven
            ;; dead) so a retry `shared-ensure' FINALIZES this EXACT partial
            ;; before reusing the same durable key or spawning again — never
            ;; overwriting the retained record and orphaning the old dir.
            (setq dorg-dolt-fixture--failed-start-lease
                  (dorg-dolt-fixture--make-lease
                   :kind 'server :dolt-dir canon :data-dir data-dir
                   :proc proc :port port :registry-key key :buffer buf
                   :stopped proc-dead :root-id root-id))
            (signal 'dorg-dolt-fixture-cleanup-error
                    (list :primary primary
                          :cleanup (nreverse cleanup-errors)))))))))

(defun dorg-dolt-fixture--shared-listed-p ()
  "Non-nil when the singleton shared lease's own store is live+owned publicly.
The fixture manages at most ONE shared server, so this is the whole basis
for `:servers' (0 or 1) and for the reuse gate: the current
`dorg-dolt-fixture--shared-lease' must be open AND its EXACT canonical
dolt-dir must appear in `dorg-dolt-server-list' as a `:role' `owned',
`:status' `live' entry.  A live+owned server this Emacs runs for any other
reason has a different dir and is never matched."
  (let ((lease dorg-dolt-fixture--shared-lease))
    (and lease
         (not (dorg-dolt-fixture--lease-closed lease))
         (let ((canon (dorg-dolt-fixture--lease-dolt-dir lease)))
           (seq-some
            (lambda (e)
              (and (eq (plist-get e :status) 'live)
                   (eq (plist-get e :role) 'owned)
                   (equal (ignore-errors
                            (dorg-dolt-server--canonical-dir
                             (plist-get e :dolt-dir)))
                          canon)))
            (dorg-dolt-server-list))))))

(defun dorg-dolt-fixture-shared-ensure ()
  "Ensure the suite-wide shared sql-server exists; return its server lease.
A READINESS BOUNDARY, not lazy context construction: the moment this
returns, the server is started, readiness-probed, registered, and visible
in the PUBLIC `dorg-dolt-server-list' as exactly one live, owned server.
Idempotent — a second call reuses the one live server and returns the SAME
lease object; the spawn primitive runs at most once.

Reuse is GATED on liveness: a cached lease is returned only when its process
is live AND its own store is live+owned in the public inventory
\(`dorg-dolt-fixture--shared-listed-p').

Recovery may reclaim ONLY a process PROVEN DEAD.  Three cases:
  - live proc AND live+owned public entry ⇒ REUSE the lease;
  - live proc but MISSING/incompatible/non-live-owned public entry ⇒ FAIL
    CLOSED: signal, and preserve the singleton, process, `--procs'
    registration, durable record, data-dir, and store leases untouched — no
    second spawn.  Tearing down a live server, or spawning a second on top of
    it, is forbidden;
  - dead proc (or a closed/absent singleton) ⇒ a dead lease is recovered
    \(`dorg-dolt-fixture--recover-stale' forgets its store leases and reclaims
    its artifacts) BEFORE a fresh server starts.  Recovery must FULLY finalize
    or FAIL: its error propagates and NO new server is started on top of an
    incompletely cleaned stale lease, so the singleton is never lost.

A failed-start SENTINEL (a prior start left a live proc that could not be
killed) blocks ALL retries until that exact proc is proven dead: while it is
live this FAILS CLOSED (no duplicate spawn); once dead its artifacts are
finalized and the sentinel cleared before proceeding."
  ;; A prior failed start that left a live, un-killable proc blocks retries
  ;; until that exact proc is proven dead and finalized.
  (when dorg-dolt-fixture--failed-start-lease
    (let* ((fl dorg-dolt-fixture--failed-start-lease)
           (proc (dorg-dolt-fixture--lease-proc fl)))
      (if (and proc (process-live-p proc))
          (error (concat "dorg-dolt-fixture-shared-ensure: a prior failed start"
                         " left a live server that could not be killed; refusing"
                         " to spawn a duplicate (dolt-dir %s)")
                 (dorg-dolt-fixture--lease-dolt-dir fl))
        ;; The exact proc is now dead: finalize/reclaim its artifacts and clear
        ;; the sentinel before a fresh start.
        (setf (dorg-dolt-fixture--lease-stopped fl) t)
        (dorg-dolt-fixture--finalize-shared fl)
        (setq dorg-dolt-fixture--failed-start-lease nil))))
  (let* ((lease dorg-dolt-fixture--shared-lease)
         (open (and lease (not (dorg-dolt-fixture--lease-closed lease))))
         (proc (and open (dorg-dolt-fixture--lease-proc lease)))
         (proc-live (and proc (process-live-p proc) t)))
    (cond
     ;; Healthy: live process AND a live+owned public entry.
     ((and open proc-live (dorg-dolt-fixture--shared-listed-p)) lease)
     ;; Live process but NOT a live+owned public entry: fail closed.  Recovery
     ;; is only for a process proven dead; never reclaim or overwrite a live
     ;; server on inventory inconsistency alone.
     ((and open proc-live)
      (error (concat "dorg-dolt-fixture-shared-ensure: shared lease process is"
                     " live but not a live+owned entry in the public inventory;"
                     " refusing to recover a live server or spawn a second"
                     " (dolt-dir %s)")
             (dorg-dolt-fixture--lease-dolt-dir lease)))
     ;; Dead-process lease is reclaimed FIRST (recovery fully finalizes or
     ;; FAILS — its error propagates and the restart never runs), then a fresh
     ;; server starts.  A closed/absent singleton just starts fresh.
     (t
      (when open
        (dorg-dolt-fixture--recover-stale lease))
      (setq dorg-dolt-fixture--shared-lease
            (dorg-dolt-fixture--shared-start))))))

(defun dorg-dolt-fixture--exec-ddl (sql)
  "Run DDL SQL against the shared server through the ONE fixture DDL seam.
The shared server is server-required (started only with mysql.el), so the
sole seam is the native mysql.el connection.  A missing connection is a
programming error — there is no unchecked `call-process' fallback whose
nonzero exit would be mistaken for a successful CREATE/DROP."
  (unless dorg-dolt-fixture--conn
    (error "dorg-dolt-fixture: no shared-server connection for DDL: %s" sql))
  (mysql-query dorg-dolt-fixture--conn sql))

(defun dorg-dolt-fixture--dir-key (dir)
  "Return the ONE canonical key for store DIR used across the store map.
`file-truename' resolves the symlinked prefix (e.g. macOS `/var'→`/private/var')
on the existing parent data-dir while tolerating a not-yet-materialized leaf,
so acquire-time and destroy-time keys always agree."
  (file-name-as-directory (file-truename (directory-file-name dir))))

(defun dorg-dolt-fixture-store-acquire ()
  "Create a fresh isolated store on the shared server; return a store lease.
Issues a real `CREATE DATABASE' for a unique database, then — and ONLY after
the CREATE succeeds — canonicalizes the store dir and registers the lease in
`dorg-dolt-fixture--store-leases' (advancing the inventory `:stores' count).  A
failed CREATE propagates and leaks nothing.

TRANSACTIONAL past CREATE: because the CREATE has already materialized a
database, ANY later failure (canonicalization or lease registration) attempts
an exact `DROP DATABASE' of that token so no untracked database leaks.  If the
compensating DROP ALSO fails, the database is genuinely orphaned: it is
retained as a tracked lease (keyed by its best-known dir) so `:stores' reflects
it and a later release retries, and a COMPOUND
`dorg-dolt-fixture-cleanup-error' surfaces both failures.
`dorg-dolt-fixture-lease-dolt-dir' returns the canonical store dir."
  (let* ((server (dorg-dolt-fixture-shared-ensure))
         (data-dir (dorg-dolt-fixture--lease-data-dir server))
         (db-name (format "test_%d" (cl-incf dorg-dolt-fixture--db-counter)))
         (raw (file-name-as-directory (expand-file-name db-name data-dir))))
    ;; CREATE first; a signal here registers nothing.
    (dorg-dolt-fixture--exec-ddl (format "CREATE DATABASE %s" db-name))
    ;; Past CREATE: canonicalize + register under an unwind that DROPs on any
    ;; failure (or retains a tracked lease + compound error if the DROP fails).
    (condition-case err
        (let* ((canon (dorg-dolt-fixture--dir-key raw))
               (lease (dorg-dolt-fixture--make-lease
                       :kind 'store :dolt-dir canon :data-dir data-dir
                       :db-name db-name)))
          (puthash canon lease dorg-dolt-fixture--store-leases)
          lease)
      ((error quit)
       (let ((drop-err (condition-case e
                           (progn (dorg-dolt-fixture--exec-ddl
                                   (format "DROP DATABASE IF EXISTS %s" db-name))
                                  nil)
                         ((error quit) e))))
         (if (null drop-err)
             ;; Compensating DROP succeeded: no leak — re-signal the original.
             (signal (car err) (cdr err))
           ;; DROP also failed: the database leaked.  Retain it as a tracked,
           ;; retryable lease (best-known key) and surface both failures.
           (let ((key (or (ignore-errors (dorg-dolt-fixture--dir-key raw)) raw)))
             (puthash key (dorg-dolt-fixture--make-lease
                           :kind 'store :dolt-dir key :data-dir data-dir
                           :db-name db-name)
                      dorg-dolt-fixture--store-leases))
           (signal 'dorg-dolt-fixture-cleanup-error
                   (list :acquire err :drop drop-err :db-name db-name))))))))

(defun dorg-dolt-fixture-store-release (lease)
  "Drop the isolated store LEASE created; EXACT and idempotent.
Issues exactly one `DROP DATABASE' of LEASE's OWN database token, then — and
ONLY after the DROP succeeds — marks the lease closed and removes it from
`dorg-dolt-fixture--store-leases' (:stores--).  A failed DROP propagates
with the lease still OPEN and registered, so ownership is retryable and the
count is never wrongly decremented.  A second release of an already-closed
lease is a bounded no-op — no extra DROP, no double-decrement."
  (when (and (dorg-dolt-fixture--lease-p lease)
             (eq (dorg-dolt-fixture--lease-kind lease) 'store)
             (not (dorg-dolt-fixture--lease-closed lease)))
    ;; DROP first — a signal here leaves the lease open and registered.
    (dorg-dolt-fixture--exec-ddl
     (format "DROP DATABASE IF EXISTS %s" (dorg-dolt-fixture--lease-db-name lease)))
    (setf (dorg-dolt-fixture--lease-closed lease) t)
    ;; Remove by the EXACT key the lease was registered under — the map is
    ;; keyed by `lease-dolt-dir' in both normal and raw-key fallback
    ;; registration — so a store whose canonicalization keeps failing is still
    ;; removed after its DROP, never left as a permanently-closed dangling entry.
    (remhash (dorg-dolt-fixture--lease-dolt-dir lease)
             dorg-dolt-fixture--store-leases))
  nil)

(defun dorg-dolt-fixture--forget-stores-under (data-dir)
  "Forget every store lease whose store lives under shared DATA-DIR.
Those stores die physically with the shared server's data-dir, so their
leases are dropped from `dorg-dolt-fixture--store-leases' (:stores
decreases) without a per-store DROP — leaving no store shadow behind a
closed or recovered server."
  (let (keys)
    (maphash (lambda (k v)
               (when (equal (dorg-dolt-fixture--lease-data-dir v) data-dir)
                 (push k keys)))
             dorg-dolt-fixture--store-leases)
    (dolist (k keys) (remhash k dorg-dolt-fixture--store-leases))))

(defun dorg-dolt-fixture--data-dir-deletable-p (data-dir root-id)
  "Non-nil only when DATA-DIR's current tuple matches non-nil ROOT-ID.
Delegate to `dorg-test--identity-intact-p'.  A nil ROOT-ID, symlink, or observed
mismatch fails closed, so finalize/abort preserves the path and fails loud
rather than recursively deleting it.  A matching reusable tuple remains
best-effort evidence, not historical proof of ownership."
  (dorg-test--identity-intact-p data-dir root-id))

(defun dorg-dolt-fixture--discard-connection-resources (conn)
  "Discard CONN's local process and buffer without protocol output.
This seam is reserved for a fixture peer already proven dead.  In that state
`mysql-disconnect' is unsafe because it sends COM_QUIT before reclaiming the
same local resources.  Any cleanup failure propagates so the caller can retain
CONN and retry the remaining local cleanup."
  (let ((proc (mysql-conn-process conn))
        (buf (mysql-conn-buf conn)))
    (when (and (processp proc) (process-live-p proc))
      (delete-process proc))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun dorg-dolt-fixture--finalize-shared (lease)
  "Reclaim shared-server LEASE's artifacts after its stop is confirmed.
Precondition: LEASE is already `stopped' (a real stop was confirmed, or the
server is known dead).  Every step is CHECKED and the order is strict, so a
failure PROPAGATES with the lease still open+stopped and a retry resumes
here WITHOUT re-stopping:

  1. delete the data-dir FIRST — until the physical store is gone, ownership
     accounting must NOT be erased (a failed delete keeps the leases);
  2. only then forget the store leases under that data-dir;
  3. delete the durable ownership record;
  4. drop the `--procs' registration and discard any retained connection's
     LOCAL resources with zero protocol output (a cleanup failure leaves the
     conn set, so the close is retryable);
  5. and ONLY once every step above has SUCCEEDED, mark the lease closed and
     clear the singleton."
  (let ((data-dir (dorg-dolt-fixture--lease-data-dir lease))
        (canon (dorg-dolt-fixture--lease-dolt-dir lease)))
    ;; 1. Data-dir FIRST.  PRESENCE is `file-exists-p' OR `file-symlink-p' — a
    ;;    dangling symlink, a symlink-to-file, or a non-directory replacement is
    ;;    PRESENT even though `file-directory-p' is nil, and must NOT be treated
    ;;    as absent (that would skip the delete yet erase store/registry
    ;;    accounting as if cleanup succeeded).  A present path must be our EXACT
    ;;    captured directory — a real dir (not a symlink), of directory kind,
    ;;    with the identity still intact — or finalize SIGNALS and preserves
    ;;    (retryable), never erasing accounting over a foreign/non-dir target.
    ;;    Only a truly ABSENT path may no-op (a legitimate retry after a prior
    ;;    successful delete).
    (when data-dir
      (let ((dfn (directory-file-name data-dir)))
        (when (or (file-exists-p dfn) (file-symlink-p dfn))
          (unless (and (not (file-symlink-p dfn))
                       (file-directory-p data-dir)
                       (dorg-dolt-fixture--data-dir-deletable-p
                        data-dir (dorg-dolt-fixture--lease-root-id lease)))
            (error "dorg-dolt-fixture: shared data-dir foreign/non-directory replacement; refusing to finalize %s"
                   data-dir))
          (delete-directory data-dir t))))
    ;; 2. The stores died with the data-dir — now forget their leases.
    (dorg-dolt-fixture--forget-stores-under data-dir)
    ;; 3. Durable ownership record.
    (dorg-test--registry-delete (dorg-dolt-fixture--lease-registry-key lease))
    ;; 4. Runtime + connection — checked/retryable.  Exact server death is a
    ;; precondition of this finalizer, so COM_QUIT is forbidden here: discard
    ;; only the local process and buffer.
    (remhash canon dorg-dolt-server--procs)
    (when dorg-dolt-fixture--conn
      (dorg-dolt-fixture--discard-connection-resources
       dorg-dolt-fixture--conn)
      (setq dorg-dolt-fixture--conn nil))
    ;; 5. Kill the OWNED process buffer — the proc is confirmed dead here
    ;;    (finalize's precondition), so the deliberately retained failed-start
    ;;    buffer, and the normal shared buffer, are reclaimed rather than
    ;;    leaked; checked/retryable (a failure propagates before close).
    (let ((buf (dorg-dolt-fixture--lease-buffer lease)))
      (when (buffer-live-p buf) (kill-buffer buf)))
    ;; 6. Only now, after every cleanup step SUCCEEDED, is the lease closed.
    (setf (dorg-dolt-fixture--lease-closed lease) t)
    (when (eq lease dorg-dolt-fixture--shared-lease)
      (setq dorg-dolt-fixture--shared-lease nil))))

(defun dorg-dolt-fixture--recover-stale (lease)
  "Forget a stale (dead-server) shared LEASE and reclaim its artifacts.
DEFENSIVE PRECONDITION: recovery may reclaim ONLY a process proven dead.  If
LEASE's process is still live this SIGNALS rather than tearing down a running
server — reclaiming a live server's data-dir/record here would orphan it.
Callers (`dorg-dolt-fixture-shared-ensure') must fail closed on a live-but-
unlisted lease and never reach this path with a live process.

The server being gone, no stop is issued: the lease is marked `stopped' and
the SAME finalize path runs — forgetting the store leases under its data-dir
and reclaiming the record/dir — so a stale recovery never leaves store leases
or a record dangling."
  (let ((proc (dorg-dolt-fixture--lease-proc lease)))
    (when (and proc (process-live-p proc))
      (error (concat "dorg-dolt-fixture--recover-stale: refusing to recover a"
                     " lease whose server process is still live (dolt-dir %s)")
             (dorg-dolt-fixture--lease-dolt-dir lease))))
  (setf (dorg-dolt-fixture--lease-stopped lease) t)
  (dorg-dolt-fixture--finalize-shared lease))

(defun dorg-dolt-fixture--shared-close (lease)
  "Stop the shared-server LEASE through the PUBLIC stop surface, EXACTLY once.
Stops via the PUBLIC `dorg-dolt-stop' with the lease's OWN dolt-dir (never
`dorg-dolt-stop-all' — which could stop siblings — and never the private
crash-recovery kills).  A nil `dorg-dolt-stop' return FAILS LOUD and leaves
the lease OPEN and UNSTOPPED so the whole close is retryable.  Once the stop
is confirmed the lease is marked `stopped' and the artifacts are reclaimed
via `dorg-dolt-fixture--finalize-shared'; a retry after a partial
artifact-cleanup failure finds `stopped' set and SKIPS the stop.  A foreign
server is never touched — only the lease's own store reaches the stop.

EXACT-DEATH PROOF (retry-safe against trunk public-stop semantics): a truthy
`dorg-dolt-stop' is NOT proof the server died — the production stop can
remhash/release its `--procs' entry and return t WITHOUT a post-kill liveness
probe, and a REPEAT stop of an already-released server returns nil.  So the
authority is the lease's EXACT process, not the stop's return value:
  - proc ALREADY dead (a prior stop killed it but its artifact cleanup failed,
    or trunk erased state and the proc has since died) ⇒ do NOT require another
    truthy stop; mark `stopped' and finalize (a repeat nil stop must NOT wedge
    the retry);
  - proc still LIVE ⇒ issue the PUBLIC stop; a nil return FAILS LOUD (open,
    unstopped, retryable), and even a truthy return must be followed by the
    proc reading DEAD — while it is still live the close FAILS and PRESERVES
    the open lease, its durable record, and its data-dir (finalize never runs).
A retry after a partial artifact-cleanup failure finds `stopped' set and SKIPS
the stop.  A foreign server is never touched — only the lease's own store
reaches the stop."
  (unless (dorg-dolt-fixture--lease-stopped lease)
    (let ((proc (dorg-dolt-fixture--lease-proc lease)))
      (if (and proc (process-live-p proc))
          ;; Proc still live: close the protocol-writing management client
          ;; BEFORE the public stop.  A disconnect failure leaves both the
          ;; connection and the live lease available for an exact retry.
          (progn
            (when dorg-dolt-fixture--conn
              (mysql-disconnect dorg-dolt-fixture--conn)
              (setq dorg-dolt-fixture--conn nil))
            (unless (dorg-dolt-stop (dorg-dolt-fixture--lease-dolt-dir lease))
              (error "dorg-dolt-fixture: dorg-dolt-stop did not release %s; ownership retained"
                     (dorg-dolt-fixture--lease-dolt-dir lease)))
            (when (process-live-p proc)
              (error (concat "dorg-dolt-fixture: server process still live after"
                             " dorg-dolt-stop %s; ownership retained")
                     (dorg-dolt-fixture--lease-dolt-dir lease))))
        ;; Proc already proven dead: finalize off that proof, never requiring
        ;; another truthy stop that a repeat nil return would wedge.
        nil)
      (setf (dorg-dolt-fixture--lease-stopped lease) t)))
  (dorg-dolt-fixture--finalize-shared lease))

(defun dorg-dolt-fixture-close (lease)
  "Close LEASE exactly once; idempotent no-op on a second call.
A store lease drops its store; a server lease stops the server through the
PUBLIC `dorg-dolt-stop' surface and deletes its durable record, leaving zero
servers.  A store DROP or a server stop that fails propagates with the lease
still OPEN (retryable).  A foreign server is never touched."
  (when (and (dorg-dolt-fixture--lease-p lease)
             (not (dorg-dolt-fixture--lease-closed lease)))
    (pcase (dorg-dolt-fixture--lease-kind lease)
      ('store (dorg-dolt-fixture-store-release lease))
      ('server (dorg-dolt-fixture--shared-close lease))))
  nil)

(defun dorg-dolt-fixture-drain-failed-start ()
  "Reclaim a retained failed-start SENTINEL in THIS process; RETURN nil or a failure.
The runner drives this during its unwind so a partial start is reclaimed in the
SAME process — next-start registry reaping is too late for the zero-after-unwind
safety objective, and a pre-readiness live proc may not be in `--procs' (so
`dorg-dolt-server-stop-all' cannot be relied on).  A LIVE partial's EXACT
process is stopped with the bounded TERM->KILL helper and PROVEN dead, then
finalized; a STOPPED (or already-dead) partial is finalized directly.  On
complete cleanup the sentinel is cleared and nil returned.  If the process
cannot be confirmed dead, or finalize fails, the sentinel, its durable record,
and its data-dir are all PRESERVED and a `(status . detail)' failure is
returned so the runner forces a nonzero exit — never spawning or forgetting."
  (let ((fl dorg-dolt-fixture--failed-start-lease))
    (when fl
      (let ((proc (dorg-dolt-fixture--lease-proc fl)))
        ;; A live partial: retry the exact bounded stop and re-check.
        (when (and proc (process-live-p proc)
                   (not (dorg-dolt-fixture--lease-stopped fl)))
          (condition-case _e
              (dorg-dolt-server--kill-process proc dorg-dolt-server-stop-term-grace)
            (error nil))
          (unless (process-live-p proc)
            (setf (dorg-dolt-fixture--lease-stopped fl) t)))
        (if (and proc (process-live-p proc))
            ;; Still live after the retry: preserve everything; report failure.
            (cons 'failed-start-still-live proc)
          ;; Proc proven dead -> finalize; a finalize failure preserves it.
          (condition-case e
              (progn
                (setf (dorg-dolt-fixture--lease-stopped fl) t)
                (dorg-dolt-fixture--finalize-shared fl)
                (setq dorg-dolt-fixture--failed-start-lease nil)
                nil)
            ((error quit) (cons 'failed-start-finalize e))))))))

(defun dorg-dolt-fixture-release-stores ()
  "Release every currently-acquired store lease; attempt ALL, RETURN failures.
Used by the test runner to drain outstanding stores before closing the shared
server.  Best-effort per store — one stuck store never blocks the rest — but
failures are NOT swallowed: each `dorg-dolt-fixture-store-release' that
signals (error or quit) is CAPTURED as a `(lease . error)' pair and the list
of failures is RETURNED (nil when every release succeeded).  This matters
because the later `dorg-dolt-fixture--shared-close' forgets ALL store leases
under the shared data-dir WITHOUT a per-store DROP — so a swallowed DROP
failure here would be masked to a zero `:stores' inventory.  The runner
surfaces a non-nil return as a teardown failure and forces a nonzero exit."
  (let (leases failures)
    (maphash (lambda (_k v) (push v leases)) dorg-dolt-fixture--store-leases)
    (dolist (lease leases)
      (condition-case err
          (dorg-dolt-fixture-store-release lease)
        ((error quit) (push (cons lease err) failures))))
    (nreverse failures)))

(defun dorg-dolt-fixture-inventory ()
  "Return fixture metrics as a plist `(:servers N :stores M :cold-starts K)'.
The fixture is a SINGLETON, so `:servers' is 0 or 1: it is 1 exactly when
the current shared lease's OWN store is live+owned in the PUBLIC
`dorg-dolt-server-list' (`dorg-dolt-fixture--shared-listed-p'), and 0
otherwise — a live+owned server this Emacs runs for any other reason, or
foreign/attached/dead entries, is never counted.  `:stores' is the number of
store leases currently held (`dorg-dolt-fixture--store-leases'); `:cold-starts'
is the monotonic count of real shared-server spawns."
  (list :servers (if (dorg-dolt-fixture--shared-listed-p) 1 0)
        :stores (hash-table-count dorg-dolt-fixture--store-leases)
        :cold-starts dorg-dolt-fixture--cold-starts))

(defun dorg-dolt-fixture-call-with-store (fn)
  "Acquire an isolated store, call FN with its dolt-dir, release on unwind.
The store is released on BOTH normal exit and a signalled error
(`unwind-protect' scope).  Returns FN's value."
  (let ((lease (dorg-dolt-fixture-store-acquire)))
    (unwind-protect
        (funcall fn (dorg-dolt-fixture-lease-dolt-dir lease))
      (dorg-dolt-fixture-store-release lease))))

(defmacro dorg-dolt-fixture-with-store (dolt-var &rest body)
  "Run BODY with DOLT-VAR bound to a fresh isolated store's dolt-dir.
The store is acquired before BODY and released on both normal exit and
error — the normal-unwind scope over `dorg-dolt-fixture-call-with-store'."
  (declare (indent 1) (debug (symbolp body)))
  `(dorg-dolt-fixture-call-with-store (lambda (,dolt-var) ,@body)))

(provide 'dorg-dolt-fixture)
;;; dorg-dolt-fixture.el ends here
