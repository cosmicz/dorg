;;; dorg-session.el --- Warm read-only session layer for dorg-ql -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A DOrg-owned warm session primitive for read/query workflows.  A
;; session keeps a source's backing buffer/backend warm across repeated
;; `dorg-ql' queries and gates every served result behind an explicit
;; freshness token, so a future warm host (a DOrg-native worker or an
;; external process such as futur-server) can wrap this API without
;; changing the public surface.  This module is host-INDEPENDENT: it owns
;; no daemon, takes no production futur dependency, and mimics no specific
;; downstream consumer backend facade.  Downstream tools consume these
;; primitives; they do not
;; own the session lifecycle.
;;
;; This is the read-only MVP.  It
;; implements open/close/refresh/stale-p, warm `dorg-ql' reads with a
;; per-query result cache, freshness invalidation, host-safe projection,
;; and strict-server transport composition.  Mutation, sync, and
;; write-back are deliberately OUT of scope and are reserved as future
;; session operations (`dorg-session-mutate', `dorg-session-sync') so the
;; read-only surface does not paint the API into a corner.
;;
;; Public surface:
;;
;;   (dorg-session-open SOURCE &rest PLIST)   -> session
;;   (dorg-session-close SESSION)             -> nil
;;   (dorg-session-refresh SESSION &optional FORCE) -> session
;;   (dorg-session-stale-p SESSION)           -> boolean
;;   (dorg-session-freshness-token SESSION)   -> plist
;;   (dorg-session-query SESSION QUERY &rest OPTIONS) -> result list
;;   (dorg-session-project SESSION VALUE &rest OPTIONS) -> host-safe value
;;
;; FRESHNESS.  A session never silently serves stale cached query data.
;; Each query first recomputes the freshness token; if it differs from the
;; token captured at the last refresh the session refreshes (reloading a
;; cold/clean plain-org buffer from disk) and drops its result cache before
;; serving.  The token is a stable, comparable plist — not opaque internal
;; state — so a host process can inspect or persist it:
;;
;;   plain-org : (:backend plain-org :source TRUENAME :buffer-modified-p P
;;                :buffer-tick TICK :size N :mtime (HIGH LOW) :hash SHA1)
;;   dolt      : (:backend dolt :source TRUENAME :dolt-dir DIR :root-id ID
;;                :head HASHOF-HEAD :transport POLICY :server-role ROLE
;;                :claim-token TOKEN :size N :mtime (HIGH LOW))
;;
;; For plain-org the on-disk file signature (size/mtime/content hash) plus
;; the warm buffer's modified tick make both out-of-process edits and live
;; in-buffer edits invalidate.  For Dolt the decisive input is the Dolt
;; HEAD (HASHOF('HEAD')) together with the canonical dolt-dir and root id,
;; so a commit/reset on the backing repository invalidates a warm read.
;; When the Dolt HEAD cannot be read the session FAILS CLOSED: the token's
;; `:head' is `dorg-session--head-unreadable' (not nil), which is always
;; stale, so freshness that cannot be proven never serves the cache — the
;; query re-runs (or surfaces the transport error) instead.
;;
;; HOST-SAFE PROJECTION.  Plain-org `dorg-ql' results carry a LIVE buffer
;; `:marker'; Dolt/model results are markerless.  A live marker (like a
;; buffer, process, or mysql connection) is NOT read/print-safe and must
;; not cross a host boundary.  `dorg-session-query' therefore projects
;; results to the host-safe shape (:heading-id :title :properties) by
;; default.  The raw, marker-carrying result is available ONLY through the
;; explicit `:raw t' option and is documented as not host-safe (process
;; local callers only).
;;
;; STRICT TRANSPORT.  A Dolt session may select strict `server' transport
;; (`:transport server') for warm SQL reuse.  Open ensures/attaches a
;; same-host server through the landed claim protocol and
;; records whether this session OWNS the server it started or merely
;; ATTACHED to a foreign/pre-existing owner.  Every session query binds
;; `dorg-sql-transport' to the session's policy, so strict reads route
;; through the server and never fall back to one-shot CLI.  Close releases
;; only resources this session owns: an owner stops the server it started
;; (token-checked claim release); an attach-only session leaves the owner's
;; claim and the shared connection intact.
;;
;; A Dolt session that gets no explicit `:transport' is UNCONDITIONALLY
;; strict `server' (the session transport selector was collapsed in
;; tracked-case — every former knob value already resolved to `server', so
;; the selector carried no behavioral variation).  Warm strict reads are
;; ~70x faster and skip the per-query CLI freshness subprocess.  An
;; explicit `:transport' still wins and is fail-closed (server-only) via
;; `dorg-session--validate-transport'.

;;; Code:

(require 'cl-lib)
(require 'dorg-freshness)
(require 'dorg-backend)
(require 'dorg-registry)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-ql)

;;; ─── Errors ───────────────────────────────────────────────────────────

(define-error 'dorg-session-error "Invalid dorg-session operation")

(defun dorg-session--error (fmt &rest args)
  "Signal `dorg-session-error' with a message formatted from FMT and ARGS."
  (signal 'dorg-session-error (list (apply #'format fmt args))))

;;; ─── Session Struct ───────────────────────────────────────────────────

(cl-defstruct (dorg-session
               (:constructor dorg-session--make)
               (:copier nil)
               (:predicate dorg-session-p))
  "A warm, read-only DOrg query session.
Callers interact through the lifecycle and query functions, never by
setting slots directly.

SOURCE is the descriptor passed to `dorg-session-open' (a buffer or a
file path).  REGISTRATION is the resolved `dorg-registration'.  BUFFER is
the live org buffer the session keeps warm.  BACKEND-TYPE is the
effective dispatch identity (`plain-org' or `dolt').  TRANSPORT is the
session's `dorg-sql-transport' policy (Dolt only).  SERVER-ROLE is nil,
`owner' (this session started the strict server), or `attach' (it reused
a foreign/pre-existing owner).  SERVER-PORT is the strict-server port when
applicable.  CACHE maps a query key to its raw (marker-carrying) result
list.  TOKEN is the freshness plist captured at the last refresh.
OWNS-BUFFER records whether open created BUFFER (and so close should kill
it).  CLOSED is non-nil once the session is closed."
  source registration buffer backend-type
  transport server-role server-port
  cache token owns-buffer
  opened-at last-used-at closed)

(defun dorg-session--check-open (session)
  "Signal `dorg-session-error' when SESSION is closed or not a session."
  (unless (dorg-session-p session)
    (dorg-session--error "not a dorg-session: %S" session))
  (when (dorg-session-closed session)
    (dorg-session--error "session is closed")))

;;; ─── Source Resolution ────────────────────────────────────────────────

(defun dorg-session--resolve-buffer (source)
  "Resolve SOURCE to (BUFFER . OWNS-BUFFER).
SOURCE is a live buffer or a file path string.  OWNS-BUFFER is non-nil
when this call opened the buffer (no buffer was already visiting the
file), so close knows whether it may kill it.  A buffer SOURCE is never
owned — the caller retains responsibility for a buffer it passed in."
  (cond
   ((bufferp source)
    (unless (buffer-live-p source)
      (dorg-session--error "source buffer is not live: %S" source))
    (cons source nil))
   ((stringp source)
    (let ((file (expand-file-name source)))
      (unless (file-exists-p file)
        (dorg-session--error "source file does not exist: %s" file))
      (let ((existing (find-buffer-visiting file)))
        (if existing
            (cons existing nil)
          (cons (dorg--find-file-noselect file) t)))))
   (t (dorg-session--error
       "source must be a buffer or a file path string: %S" source))))

;;; ─── Freshness Token ──────────────────────────────────────────────────

(defun dorg-session--plain-token (session)
  "Compute the plain-org freshness token plist for SESSION.
Folds the on-disk file signature (size, mtime, content hash) together
with the warm buffer's modified tick and modified flag, so both
out-of-process file edits and live in-buffer edits invalidate."
  (let* ((buffer (dorg-session-buffer session))
         (file (buffer-file-name buffer))
         (truename (and file (file-truename file)))
         (attrs (and truename (file-attributes truename))))
    (list :backend 'plain-org
          :source truename
          :buffer-modified-p (and (buffer-modified-p buffer) t)
          :buffer-tick (buffer-chars-modified-tick buffer)
          :size (and attrs (file-attribute-size attrs))
          :mtime (and attrs (file-attribute-modification-time attrs))
          :hash (dorg-freshness-file-hash truename))))

(defconst dorg-session--head-unreadable :dorg-session-head-unreadable
  "Sentinel `:head' token value marking a Dolt HEAD that could not be read.
Unlike a plain nil, `dorg-session-stale-p' recognizes this value as NEVER
fresh: when freshness cannot be proven the session fails closed and re-runs
the query rather than serving a possibly-stale cached result.  It is a
printable keyword, so the freshness token stays a stable, `equal'-comparable,
read/print-safe plist.")

(defun dorg-session--dolt-token (session)
  "Compute the Dolt-backed freshness token plist for SESSION.
The decisive input is the Dolt HEAD (HASHOF('HEAD')) under the canonical
dolt-dir and root id; a commit/reset on the backing repository therefore
invalidates a warm read.  The materialized-view file signature and the
selected transport/claim identity are folded in as well.  HEAD is read
through the session's transport policy so a strict session never drops to
CLI to compute its own freshness.  When HEAD cannot be read the token's
`:head' is `dorg-session--head-unreadable' (not nil), which fails closed in
`dorg-session-stale-p' so two unreadable probes never compare as fresh."
  (let* ((reg (dorg-session-registration session))
         (backend (dorg-registration-backend reg))
         (dolt-dir (dorg-backend-dolt-dir backend))
         (root-id (dorg-backend-root-id backend))
         (file (buffer-file-name (dorg-session-buffer session)))
         (truename (and file (file-truename file)))
         (attrs (and truename (file-attributes truename)))
         (head (let ((dorg-sql-transport (dorg-session-transport session)))
                 (condition-case nil
                     (dorg-head-commit dolt-dir)
                   (error dorg-session--head-unreadable))))
         (claim (and (eq (dorg-session-transport session) 'server)
                     (ignore-errors (dorg-dolt-server-claim dolt-dir)))))
    (list :backend 'dolt
          :source truename
          :dolt-dir (and dolt-dir (file-truename dolt-dir))
          :root-id root-id
          :head head
          :transport (dorg-session-transport session)
          :server-role (dorg-session-server-role session)
          :claim-token (and claim (alist-get :token claim))
          :size (and attrs (file-attribute-size attrs))
          :mtime (and attrs (file-attribute-modification-time attrs)))))

(defun dorg-session--compute-token (session)
  "Compute SESSION's current freshness token plist for its backend."
  (pcase (dorg-session-backend-type session)
    ('plain-org (dorg-session--plain-token session))
    ('dolt      (dorg-session--dolt-token session))
    (other (dorg-session--error "unknown session backend: %S" other))))

(defun dorg-session-freshness-token (session)
  "Return SESSION's last captured freshness token plist.
The token is a stable, `equal'-comparable plist (see the module
commentary) that a host process can inspect or persist.  It reflects the
state captured at the last `dorg-session-refresh' / query, not a fresh
recomputation; use `dorg-session-stale-p' to test against current state."
  (dorg-session--check-open session)
  (dorg-session-token session))

(defun dorg-session-stale-p (session)
  "Return non-nil when SESSION's source has changed since the last refresh.
Recomputes the current freshness token and compares it with the token
captured at the last refresh.  A stale session must refresh before any
cached result is served (see `dorg-session-query').

Fails closed when freshness cannot be proven: a current token whose `:head'
is `dorg-session--head-unreadable' is always stale, so two unreadable Dolt
HEAD probes never compare as fresh and a cached result is never served while
HEAD stays unreadable."
  (dorg-session--check-open session)
  (let ((current (dorg-session--compute-token session)))
    (or (eq (plist-get current :head) dorg-session--head-unreadable)
        (not (equal (dorg-session-token session) current)))))

;;; ─── Refresh ──────────────────────────────────────────────────────────

(defun dorg-session--revert-if-needed (session)
  "Reload SESSION's plain-org buffer from disk when it is cold.
Called only from a STALE refresh, so a clean plain-org buffer is reloaded
unconditionally — not gated on the visited-file modtime, because a
sub-second out-of-process edit can change a file's contents (and so its
content-hash token) while leaving size and mtime unchanged.  A buffer with
live in-buffer edits is the source of truth and is left as is; its
modified tick already participates in the freshness token."
  (when (eq (dorg-session-backend-type session) 'plain-org)
    (let ((buffer (dorg-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (not (buffer-modified-p))
                     (buffer-file-name)
                     (file-exists-p (buffer-file-name)))
            (revert-buffer t t t)))))))

(defun dorg-session-refresh (session &optional force)
  "Refresh SESSION's warm state and recapture its freshness token.
Reloads a cold/clean plain-org buffer from disk when its file changed,
drops the result cache, and stores the recomputed token.  With FORCE
non-nil the cache is dropped and the buffer reload is attempted
unconditionally.  Returns SESSION."
  (dorg-session--check-open session)
  (when (or force (dorg-session-stale-p session))
    (dorg-session--revert-if-needed session)
    (clrhash (dorg-session-cache session))
    (setf (dorg-session-token session)
          (dorg-session--compute-token session)))
  session)

;;; ─── Host-Safe Projection ─────────────────────────────────────────────

(defun dorg-session--project-result (result)
  "Project one `dorg-ql' RESULT plist to its host-safe shape.
Drops the live `:marker' (and any non-readable object it would carry),
keeping the read/print-safe `:heading-id', `:title', and `:properties'.
RESULT keys other than these are dropped."
  (list :heading-id (plist-get result :heading-id)
        :title      (plist-get result :title)
        :properties (plist-get result :properties)))

(defun dorg-session-project (session value &rest options)
  "Return a host-safe projection of VALUE for SESSION.
VALUE is a single `dorg-ql' result plist or a list of them.  The default
projection strips the live `:marker' so the result is read/print-safe and
safe to return across a host boundary (see the module commentary).

OPTIONS:
  :verify NON-NIL  also assert the projection round-trips through
                   `prin1-to-string'/`read', signalling `dorg-session-error'
                   if any unreadable object leaked.

SESSION is accepted for API symmetry and a closed-session guard; the
projection itself is backend-independent."
  (dorg-session--check-open session)
  (let* ((projected
          (cond
           ;; A single result plist (its head is a keyword).
           ((and (consp value) (keywordp (car value)))
            (dorg-session--project-result value))
           ;; A list of result plists.
           ((listp value)
            (mapcar #'dorg-session--project-result value))
           (t (dorg-session--error
               "cannot project non-result value: %S" value)))))
    (when (plist-get options :verify)
      (condition-case _
          (read (prin1-to-string projected))
        (error (dorg-session--error
                "projection is not read/print-safe: %S" projected))))
    projected))

;;; ─── Query ────────────────────────────────────────────────────────────

(defun dorg-session--query-key (query)
  "Return a stable cache key for QUERY.
A fresh copy so a caller mutating its query form cannot corrupt the
cache; compared with `equal' by the hash table."
  (copy-tree query))

(defun dorg-session--run-query (session query)
  "Execute QUERY against SESSION's warm buffer, returning raw results.
Binds `dorg-sql-transport' to the session's policy so a strict-server
Dolt session routes through its server and never falls back to CLI."
  (let ((dorg-sql-transport (or (dorg-session-transport session)
                                dorg-sql-transport)))
    (dorg-ql-select query (dorg-session-buffer session))))

(defun dorg-session-query (session query &rest options)
  "Run dorg-ql QUERY through SESSION and return host-safe results.
Refreshes first when the source is stale, so cached data is never served
after an out-of-process file edit or a Dolt HEAD change.  Repeated
queries with an unchanged token reuse the warm result cache instead of
re-walking the buffer or re-issuing SQL.

QUERY is any form `dorg-ql-select' accepts.  Results are projected to the
host-safe shape (:heading-id :title :properties) by default.

OPTIONS:
  :raw NON-NIL  return the raw `dorg-ql' results, INCLUDING the live
                plain-org `:marker'.  Documented as NOT host-safe: only
                for process-local callers that explicitly ask for it.

Signals `dorg-session-error' when SESSION is closed; query-form errors
surface as `dorg-ql-error' from the underlying adapter."
  (dorg-session--check-open session)
  ;; Never serve stale: refresh-if-stale before consulting the cache.
  (dorg-session-refresh session)
  (let* ((cache (dorg-session-cache session))
         (key (dorg-session--query-key query))
         (raw (if (let ((probe (gethash key cache 'miss)))
                    (not (eq probe 'miss)))
                  (gethash key cache)
                (let ((results (dorg-session--run-query session query)))
                  (puthash key results cache)
                  results))))
    (setf (dorg-session-last-used-at session) (current-time))
    (if (plist-get options :raw)
        raw
      (dorg-session-project session raw))))

;;; ─── Strict Transport Lifecycle ───────────────────────────────────────

(defun dorg-session--open-strict-server (session dolt-dir)
  "Ensure strict-server transport for SESSION on DOLT-DIR and record its role.
Sets SERVER-ROLE to `owner' when THIS open started the server (no server
ran for the repo before and this process now owns the claim), or `attach'
when it reused a foreign or pre-existing owner.  Records the resolved
SERVER-PORT.  Never starts a second server and never falls back to CLI —
`dorg-dolt-server--ensure-strict' owns those guarantees."
  (let* ((pre-port (dorg-dolt-server-port dolt-dir))
         (port (dorg-dolt-server--ensure-strict dolt-dir))
         (post-port (dorg-dolt-server-port dolt-dir))
         (claim (dorg-dolt-server-claim dolt-dir))
         (owned-here (and claim (dorg-dolt-server--claim-owned-here-p claim)))
         ;; This session started the server iff none ran for the repo
         ;; before open, one runs now, and this process owns the claim.
         (owner (and (null pre-port) post-port owned-here t)))
    (setf (dorg-session-server-role session) (if owner 'owner 'attach)
          (dorg-session-server-port session) port)
    session))

(defun dorg-session--maybe-open-transport (session)
  "Perform any open-time transport setup for SESSION.
Only strict `server' transport on a Dolt backend does work here, ensuring
or attaching the server and recording the ownership role.  There is no
public session auto/CLI fallback."
  (when (and (eq (dorg-session-backend-type session) 'dolt)
             (eq (dorg-session-transport session) 'server))
    (let ((dolt-dir (dorg-backend-dolt-dir
                     (dorg-registration-backend
                      (dorg-session-registration session)))))
      (dorg-session--open-strict-server session dolt-dir))))

(defun dorg-session--close-transport (session)
  "Release SESSION's owned transport resources at close.
An `owner' session stops the strict server it started, which closes its
connection and releases the ownership claim (token-checked).  An `attach'
session leaves the owner's claim and the shared connection intact — it
released nothing it owns.  No-op for non-server sessions."
  (when (eq (dorg-session-server-role session) 'owner)
    (let ((dolt-dir (dorg-backend-dolt-dir
                     (dorg-registration-backend
                      (dorg-session-registration session)))))
      (when dolt-dir
        (ignore-errors (dorg-dolt-server--stop dolt-dir))))))

;;; ─── Session Transport Policy ─────────────────────────────────────────

(defun dorg-session--validate-transport (transport)
  "Return TRANSPORT when it is a public session SQL transport.
Signals `dorg-session-error' for removed or unknown public transports."
  (pcase transport
    ('server transport)
    ((or 'auto 'cli)
     (dorg-session--error
      "`%S' is not a public session transport; use strict `server'"
      transport))
    (other
     (dorg-session--error "invalid dorg session transport: %S" other))))

(defun dorg-session--default-transport (plist backend-type)
  "Choose the session transport for an open with PLIST and BACKEND-TYPE.
An explicit `:transport' in PLIST always wins (tested by key presence, so
a future valid value — including nil — is never silently overridden) and
is fail-closed through `dorg-session--validate-transport' (server-only).
Absent an explicit value, a Dolt backend is UNCONDITIONALLY strict
`server' — the session transport selector was collapsed (tracked-case), so
there is no knob to consult and no per-query recomputation.  Any other
backend keeps the historical global `dorg-sql-transport'."
  (cond
   ((plist-member plist :transport)
    (dorg-session--validate-transport (plist-get plist :transport)))
   ((eq backend-type 'dolt) 'server)
   (t dorg-sql-transport)))

;;; ─── Open / Close ─────────────────────────────────────────────────────

(defun dorg-session-open (source &rest plist)
  "Open a warm read-only session for SOURCE and return it.
SOURCE is a live buffer or a file path string.  The session resolves and
holds SOURCE's `dorg-registration', keeps its org buffer warm, captures an
initial freshness token, and (for strict Dolt transport) ensures/attaches
the server.

PLIST keys:
  :transport POLICY  override `dorg-sql-transport' for this session's Dolt
                     reads.  Only strict `server' is public.  When omitted,
                     a Dolt session is unconditionally strict `server'; any
                     other backend keeps the current `dorg-sql-transport'.
                     An explicit value always wins and is validated
                     server-only (`auto'/`cli'/unknown fail closed).
                     `server' ensures/attaches one and records owner vs
                     attach.

Signals `dorg-session-error' for a bad source or an unregisterable
buffer."
  (let* ((resolved (dorg-session--resolve-buffer source))
         (buffer (car resolved))
         (owns-buffer (cdr resolved))
         ;; Strict: a session operates on the backing store, so ensure a
         ;; concrete Dolt root_id rather than a pure classification.
         (reg (with-current-buffer buffer (dorg--ensure-registration))))
    (unless reg
      (when owns-buffer
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (dorg-session--error
       "source buffer %s has no registered backing storage"
       (buffer-name buffer)))
    (let* ((backend (dorg-registration-backend reg))
           (backend-type (dorg-ql--effective-backend-type backend))
           (transport (dorg-session--default-transport plist backend-type))
           (session (dorg-session--make
                     :source source
                     :registration reg
                     :buffer buffer
                     :backend-type backend-type
                     :transport transport
                     :cache (make-hash-table :test 'equal)
                     :owns-buffer owns-buffer
                     :opened-at (current-time)
                     :last-used-at (current-time)
                     :closed nil)))
      (unless backend-type
        (when owns-buffer (when (buffer-live-p buffer) (kill-buffer buffer)))
        (dorg-session--error
         "source buffer %s has an unknown backend" (buffer-name buffer)))
      (condition-case err
          (dorg-session--maybe-open-transport session)
        (error
         (when owns-buffer (when (buffer-live-p buffer) (kill-buffer buffer)))
         (signal (car err) (cdr err))))
      (setf (dorg-session-token session)
            (dorg-session--compute-token session))
      session)))

(defun dorg-session-close (session)
  "Close SESSION, releasing only the resources it owns.
Stops a strict server this session started (owner), but leaves a foreign
or pre-existing owner's claim untouched (attach-only).  Kills the warm
buffer only when `dorg-session-open' created it; a buffer passed in by the
caller is left open.  Drops the result cache and marks SESSION closed.
Idempotent: closing an already-closed session is a no-op.  Returns nil."
  (unless (dorg-session-p session)
    (dorg-session--error "not a dorg-session: %S" session))
  (unless (dorg-session-closed session)
    (dorg-session--close-transport session)
    (when (and (dorg-session-owns-buffer session)
               (buffer-live-p (dorg-session-buffer session)))
      (with-current-buffer (dorg-session-buffer session)
        (set-buffer-modified-p nil))
      (kill-buffer (dorg-session-buffer session)))
    (clrhash (dorg-session-cache session))
    (setf (dorg-session-closed session) t))
  nil)

(provide 'dorg-session)
;;; dorg-session.el ends here
