;;; dorg-ext-read.el --- Extension read-plane: grouped warm hydration -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The extension READ-PLANE: the read counterpart to the
;; mutation participant seam in `dorg-ext.el'.  It lets a
;; registered extension serve set-based/grouped reads from its own
;; projection tables inside a warm DOrg process, and consumes the
;; mutation-result invalidation scopes that the write side emits to keep a
;; warm read cache fresh.
;;
;; DOrg stays domain-neutral here: it knows nothing of bead/workflow/status
;; vocabulary.  An extension DECLARES read capability and returns a grouped
;; query PLAN; DOrg owns execution, caching, freshness, and host-safe
;; projection.  Downstream tools map the generic projected rows back into
;; their own domain models OUTSIDE DOrg.
;;
;; Downstream composition.  The motivating case is a downstream issue
;; tracker's list/ready/show: the downstream tool registers a projection
;; extension whose `:hydrate' returns a grouped query over its own
;; projection table, calls `dorg-ext-hydrate' against a warm cache, and maps
;; the generic rows to its own domain structs in the downstream adapter.
;; All issue/workflow/status/dependency/note/assignee/priority semantics
;; live in the downstream library, not DOrg — DOrg only sees generic rows,
;; roots, and a query-shape descriptor.  After a mutation, the downstream
;; tool feeds the typed mutation result's `:invalidation' plist straight
;; into `dorg-ext-read-invalidate' to keep the warm cache fresh without
;; reparsing a full materialized Org tracker.
;;
;; Contract mirrors the write side's purity model:
;;
;;   (dorg-register-extension 'NAME
;;     :schema ... :version ... :capabilities '(dolt)
;;     :read-tables '((:name "t" :key-column "id" :scope-column "root_id"))
;;     :hydrate #'FN)            ; pure: ctx -> grouped query PLAN
;;
;; A `:hydrate' callback receives a `dorg-ext-read-context' and returns a
;; PLAN: a plist (:sql (S1 S2 ...) :reduce FN) whose `:sql' is a BOUNDED,
;; set-based statement list (never one query per row/heading) and whose
;; optional `:reduce' assembles host-safe rows in Elisp.  A bare SQL string
;; or list of strings is sugar for (:sql ... :reduce nil).  DOrg executes
;; the plan via the single SQL chokepoint — `dorg-sql', or
;; `dorg-sql--at-revision' when a committed Dolt head was captured — so a
;; per-bead one-shot fallback is observable and rejected (see the
;; read-plane SPEC).  Revision-bound execution makes snapshot coherence a
;; STRUCTURAL property: plan statements read at the one captured token
;; (unqualified table references, nested subqueries and all) without the
;; participant appending any `AS OF' suffix.
;;
;; Cache invalidation has two complementary paths:
;;   - PULL / backstop: the freshness discriminator (Dolt HEAD, or plain
;;     file size/mtime/hash) is folded into the cache key, so an
;;     out-of-process change automatically misses the warm cache.
;;   - PUSH / same-process: `dorg-ext-read-invalidate' consumes a
;;     mutation-result :invalidation plist (:roots :ext-dirty :head :file)
;;     to evict warm entries for changed roots + extension dirty scopes.

;;; Code:

(require 'cl-lib)
(require 'dorg-freshness)
(require 'dorg-ext)
(require 'dorg-dolt)

(define-error 'dorg-ext-read-error "Invalid dorg extension read-plane operation")

(defun dorg-ext-read--error (fmt &rest args)
  "Signal `dorg-ext-read-error' with a message formatted from FMT and ARGS."
  (signal 'dorg-ext-read-error (list (apply #'format fmt args))))

;;; ─── Read Context ─────────────────────────────────────────────────────

(cl-defstruct (dorg-ext-read-context
               (:constructor dorg-ext-read-context--new)
               (:copier nil)
               (:predicate nil))
  "Read-only context DOrg passes to an extension `:hydrate' participant.
The read-plane counterpart context; all fields are
DOrg-owned.  ROOT-ID is the requested tree root (nil for a whole-file plain
source).  SOURCE-KIND is the read source kind (e.g. `dolt').  DOLT-DIR and
FILE locate the backing store.  HEAD is the Dolt HEAD freshness token (nil
for plain sources).  QUERY is the generic, domain-neutral query-shape
descriptor passed through from the caller; the participant maps it to SQL."
  root-id source-kind dolt-dir file head query)

;;; ─── Source Resolution + Freshness ────────────────────────────────────

(defun dorg-ext-read--resolve-source (source)
  "Resolve SOURCE to a plist (:source-kind :dolt-dir :root-id :file).
SOURCE is a Dolt descriptor plist (:dolt-dir D :root-id R), a plain-org
file path string, or a buffer visiting a file."
  (cond
   ((and (listp source) (plist-get source :dolt-dir))
    (list :source-kind 'dolt
          :dolt-dir (plist-get source :dolt-dir)
          :root-id (plist-get source :root-id)
          :file (let ((f (plist-get source :file)))
                  (and f (file-truename f)))))
   ((stringp source)
    (list :source-kind 'plain-org :dolt-dir nil :root-id nil
          :file (file-truename source)))
   ((bufferp source)
    (let ((f (buffer-file-name source)))
      (list :source-kind 'plain-org :dolt-dir nil :root-id nil
            :file (and f (file-truename f)))))
   (t (dorg-ext-read--error
       "source must be a Dolt descriptor plist, a file path, or a buffer: %S"
       source))))

(defconst dorg-ext-read--head-unreadable :dorg-ext-read-head-unreadable
  "Tag marking a Dolt HEAD that could not be read.
This is only the TAG half of an unreadable-HEAD token; the freshness
value actually folded into the cache key is `(TAG . N)' with a fresh
process-local counter N per probe (see `dorg-ext-read--head').  The cons
shape can never `equal' a real HEAD commit string nor a plain-source
\(:size :mtime :hash) signature, so an unreadable probe fails closed: each
failure yields a NON-EQUAL key, missing the warm cache and re-running
rather than serving rows it could not prove fresh.")

(defvar dorg-ext-read--head-unreadable-counter 0
  "Monotonic process-local counter for unreadable-HEAD freshness tokens.
Bumped on every failed HEAD probe so two failures for the same source
produce distinct cache keys.  Not reset across reads — only inequality of
successive tokens matters, never the absolute value.")

(defun dorg-ext-read--head (resolved)
  "Return the Dolt HEAD for a RESOLVED Dolt source, or nil.
Returns a fresh unreadable-HEAD token `(dorg-ext-read--head-unreadable . N)'
when HEAD cannot be read for a Dolt source — N is a per-probe counter so
two failed probes never collapse onto one cache key; nil for non-Dolt
sources."
  (if (eq (plist-get resolved :source-kind) 'dolt)
      (condition-case nil
          (dorg-head-commit (plist-get resolved :dolt-dir))
        (error (cons dorg-ext-read--head-unreadable
                     (cl-incf dorg-ext-read--head-unreadable-counter))))
    nil))

(defun dorg-ext-read--head-committed-p (head)
  "Return non-nil when HEAD is a committed Dolt hash usable as an AS-OF pin.
A real HEAD is the non-empty hash string `dorg-head-commit' returns; an
unreadable probe is the cons `(TAG . N)' (see `dorg-ext-read--head') and a
plain source is nil — neither can pin a read, so both answer nil."
  (and (stringp head) (not (string-empty-p head))))

(defun dorg-ext-read-as-of-suffix (head)
  "Return the SQL ` AS OF <HEAD>' clause pinning a read to committed HEAD.
HEAD is a `dorg-ext-read-context' `head' — a committed Dolt hash string for a
Dolt source.  Returns the empty string when HEAD is nil (a plain source) or
not a committed hash (an unreadable-HEAD token), so a participant may append
it UNCONDITIONALLY.

NOT FOR HYDRATE PLANS.  Snapshot binding is a DOrg-owned execution
property: `dorg-ext-read--hydrate-entry' runs every plan statement
revision-bound to the ONE captured committed token
\(`dorg-sql--at-revision'), and a revision-bound plan REJECTS any
`AS OF' clause — including one naming the captured hash — before its
first statement executes: the DOrg scope is the sole snapshot authority,
and a second caller-supplied pin only expands the error surface.

This helper remains for the SEPARATE pattern of a consumer explicitly
coordinating one snapshot across multiple ordinary `dorg-sql' calls
\(e.g. a downstream read-session protocol), where each statement runs
unscoped and the caller supplies the pin itself.  It is the same
SQL-text pin the render/model layer uses (`dorg-sql-gen--as-of-suffix')."
  (if (dorg-ext-read--head-committed-p head)
      (format " AS OF %s" (dorg-sql-quote head))
    ""))

(defun dorg-ext-read--freshness (resolved)
  "Return the freshness discriminator for a RESOLVED source.
For Dolt this is the HEAD commit (or a fresh unreadable-HEAD token, see
`dorg-ext-read--head'); for a plain or file-backed source it is the
\(size mtime hash) signature.  The value is `equal'-comparable and
read/print-safe."
  (pcase (plist-get resolved :source-kind)
    ('dolt (dorg-ext-read--head resolved))
    (_ (let* ((file (plist-get resolved :file))
              (attrs (and file (file-attributes file))))
         (list :size (and attrs (file-attribute-size attrs))
               :mtime (and attrs (file-attribute-modification-time attrs))
               :hash (dorg-freshness-file-hash file))))))

;;; ─── Cache Key ────────────────────────────────────────────────────────

(defun dorg-ext-read--cache-key (ext-name r query fresh)
  "Build the cache key from an already-resolved R and precomputed FRESH.
The hydrate path resolves the source and probes freshness ONCE, then threads
both here so the key's `:fresh', the extension context `head', the AS-OF pin,
and the returned snapshot token are all the SAME committed HEAD — no second
`dorg-head-commit' probe that could straddle a concurrent advance."
  (list :ext ext-name
        :kind (plist-get r :source-kind)
        :id (dorg-ext-read--identity (plist-get r :dolt-dir)
                                     (plist-get r :file))
        :root (plist-get r :root-id)
        :query (copy-tree query)
        :fresh fresh))

;;; ─── Cache ────────────────────────────────────────────────────────────

(cl-defstruct (dorg-ext-read-cache
               (:constructor dorg-ext-read-cache--new)
               (:copier nil)
               (:predicate dorg-ext-read-cache-p))
  "A warm extension read cache.
ENTRIES maps a private extension read cache key to a stored plist
\(:rows ROWS :head TOKEN :root ROOT :ext NAME :source-key SK), where TOKEN is
the committed Dolt HEAD the rows were read at (nil for a plain source).
Callers create one via `dorg-ext-read-cache-make' and never set slots
directly."
  entries)

(defun dorg-ext-read-cache-make ()
  "Return a fresh, empty `dorg-ext-read-cache'."
  (dorg-ext-read-cache--new
   :entries (make-hash-table :test 'equal)))

(defun dorg-ext-read--check-cache (cache)
  "Signal `dorg-ext-read-error' unless CACHE is a `dorg-ext-read-cache'."
  (unless (dorg-ext-read-cache-p cache)
    (dorg-ext-read--error "not a dorg-ext-read-cache: %S" cache)))

(defun dorg-ext-read--identity (dolt-dir file)
  "Return the canonical source-identity key for DOLT-DIR / FILE.
Canonical dolt-dir truename for a Dolt source, else the file truename, else
nil.  This is the same source-identity dimension the cache key folds into
`:id', so two Dolt repos that share a root id remain distinct cache sources."
  (or (and dolt-dir (file-truename dolt-dir))
      (and file (file-truename file))))

;;; ─── Host-Safe Projection ─────────────────────────────────────────────

(defun dorg-ext-read--project (value &rest options)
  "Return a host-safe projection of VALUE (a list of projected rows).
Rows produced by `dorg-sql' are already alists/plists of scalars — no live
markers, buffers, processes, or SQL connections — so projection is the
identity plus an optional read/print-safety assertion.  It is the single
normalization point where a richer extension result would be reduced to a
host-safe shape.

OPTIONS:
  :verify NON-NIL  assert VALUE round-trips through `prin1-to-string'/`read',
                   signalling `dorg-ext-read-error' if any unreadable object
                   leaked."
  (when (plist-get options :verify)
    (condition-case _
        (read (prin1-to-string value))
      (error (dorg-ext-read--error
              "read result is not read/print-safe: %S" value))))
  value)

;;; ─── Grouped Hydration ────────────────────────────────────────────────

(defun dorg-ext-read--normalize-plan (plan)
  "Normalize a `:hydrate' PLAN to a (:sql LIST :reduce/:next FN) plist.
PLAN is nil (no query), a SQL string, a list of SQL strings, or a plist
\(:sql SPEC :reduce FN :next FN).  :NEXT receives the current stage's row
lists and returns the next declarative plan.  DOrg, not the callback, validates
and executes every resulting statement under the captured revision.  A stage
uses either :REDUCE or :NEXT, never both.  Signals `dorg-ext-read-error'
otherwise."
  (cond
   ((null plan) (list :sql nil :reduce nil))
   ((stringp plan) (list :sql (list plan) :reduce nil))
   ((and (consp plan) (cl-every #'stringp plan))
    (list :sql plan :reduce nil))
   ((and (listp plan) (plist-member plan :sql))
    (let ((reduce (plist-get plan :reduce))
          (next (plist-get plan :next)))
      (unless (or (null reduce) (functionp reduce))
        (dorg-ext-read--error ":reduce must be a function, got %S" reduce))
      (unless (or (null next) (functionp next))
        (dorg-ext-read--error ":next must be a function, got %S" next))
      (when (and reduce next)
        (dorg-ext-read--error "a hydrate stage cannot combine :reduce and :next"))
      (list :sql (dorg-ext-normalize-sql-or-nil (plist-get plan :sql))
            :reduce reduce :next next)))
   (t (dorg-ext-read--error "invalid hydrate plan: %S" plan))))

(defun dorg-ext-read--execute (plan resolved &optional revision)
  "Execute a normalized PLAN against the RESOLVED source, returning rows.
Each `:sql' statement is run set-based via the SQL chokepoint —
`dorg-sql', or `dorg-sql--at-revision' when REVISION is non-nil — so a
per-row fallback is observable.  A non-nil REVISION is the ONE captured
committed Dolt HEAD: every statement then executes inside the DOrg-owned
revision-bound scope (a Dolt revision database), so every reference the
statement resolves through the session database — the default for
ordinary SQL, repeated and nested-subquery references included — reads
at REVISION regardless of concurrent HEAD advances.  The whole plan is
validated fail-closed before its first statement (see
`dorg-sql--revision-unsafe-reason'); statements that would resolve
outside the scope never execute, not even partially.  When PLAN has no
`:reduce', the per-statement row lists are appended; otherwise `:reduce'
is applied to the list of per-statement row lists to assemble host-safe
rows."
  (setq plan (dorg-ext-read--normalize-plan plan))
  (let* ((dolt-dir (plist-get resolved :dolt-dir))
         (reduce (plist-get plan :reduce))
         (next (plist-get plan :next))
         (stmts (plist-get plan :sql)))
    ;; Fail-closed plan validation: a revision-bound plan is checked WHOLE
    ;; before its FIRST statement executes, so an unsafe plan never runs
    ;; partially (see `dorg-sql--revision-unsafe-reason' for the rejected
    ;; forms — including ANY `AS OF': the scope is the sole pin).
    (when revision
      (let ((db (dorg-dolt-server--db-name dolt-dir))
            (index 0))
        (dolist (sql stmts)
          (setq index (1+ index))
          (let ((unsafe (dorg-sql--revision-unsafe-reason sql db)))
            (when unsafe
              (dorg-ext-read--error
               "snapshot plan statement %d is not revision-safe (%s): %s"
               index unsafe sql))))))
    (let ((per-stmt (mapcar (lambda (sql)
                              (if revision
                                  (dorg-sql--at-revision dolt-dir revision sql)
                                (dorg-sql dolt-dir sql)))
                            stmts)))
      (cond
       (next
        ;; NEXT returns only a declarative follow-up plan.  Re-entering this
        ;; executor means its SQL is snapshot-checked and revision-bound by
        ;; DOrg before any second-stage statement can run.
        (dorg-ext-read--execute (funcall next per-stmt) resolved revision))
       (reduce (funcall reduce per-stmt))
       (t (apply #'append per-stmt))))))

(defun dorg-ext-read--hydrate-entry (cache ext-name source query
                                           &optional require-committed-head)
  "Serve the full warm cache ENTRY for EXT-NAME hydrating SOURCE with QUERY.
Single-capture core shared by `dorg-ext-hydrate' (rows only) and
`dorg-ext-read-snapshot' (versioned).  Resolves the source ONCE and probes
freshness ONCE; that one value is the cache key's `:fresh', the extension
context `head', and — for a Dolt source — the committed snapshot token stored
on the entry as `:head'.  There is no second `dorg-head-commit' probe to
straddle a concurrent advance.

On a warm HIT the STORED entry is returned as-is — its rows AND the `:head'
token of the snapshot those rows came from — never a re-probed live HEAD.  On
a miss DOrg builds the `dorg-ext-read-context' (carrying the captured `head'),
calls the `:hydrate' participant for a grouped query PLAN, executes it
set-based, projects the rows host-safe, and caches the entry
`(:rows :head :root :ext :source-key)'.

Execution is REVISION-BOUND whenever the captured Dolt head is a
committed hash: every plan statement runs inside the DOrg-owned
revision scope at that token (`dorg-sql--at-revision'), for the rows-only
hydrate path as well as the snapshot path — both serve the SAME cache
entry, so an unbound hydrate could otherwise populate rows a later
snapshot warm hit returns tagged with the token.  Snapshot binding is a
structural property of this executor, never of participant SQL text.

When REQUIRE-COMMITTED-HEAD is non-nil and the source is a Dolt repo
whose HEAD is not a committed hash — unreadable, or a blank read-back —
signals `dorg-ext-read-error' BEFORE executing the plan rather than
serving rows tagged with a non-committed token (the snapshot fail-closed
contract).  Fails closed on capability too: signals when EXT-NAME
cannot serve the source kind (`dorg-ext-read-capability')."
  (dorg-ext-read--check-cache cache)
  (let* ((r (dorg-ext-read--resolve-source source))
         (kind (plist-get r :source-kind)))
    (unless (eq (dorg-ext-read-capability ext-name kind) 'supported)
      (dorg-ext-read--error
       "extension %S cannot serve %S reads (no :hydrate or capability)"
       ext-name kind))
    (let* ((fresh (dorg-ext-read--freshness r))
           ;; For a Dolt source the freshness discriminator IS the committed
           ;; HEAD (`dorg-ext-read--head'), so ONE probe gives the token used
           ;; for the key, the context head, the revision-bound execution
           ;; scope, and `:head'.
           (head (and (eq kind 'dolt) fresh))
           ;; The captured token doubles as the execution pin: a committed
           ;; hash revision-binds the whole plan; anything else (plain
           ;; source, unreadable or blank head) executes unbound and can
           ;; never satisfy a snapshot read.
           (revision (and (dorg-ext-read--head-committed-p head) head)))
      (when (and require-committed-head (eq kind 'dolt) (not revision))
        (dorg-ext-read--error
         "cannot serve snapshot read for %S: Dolt HEAD is not a committed token (unreadable or blank); refusing to execute unbound"
         ext-name))
      (let* ((key (dorg-ext-read--cache-key ext-name r query fresh))
             (entries (dorg-ext-read-cache-entries cache))
             (hit (gethash key entries 'miss)))
        (if (not (eq hit 'miss))
            hit
          (let* ((ext (dorg-get-extension ext-name))
                 (ctx (dorg-ext-read-context--new
                       :root-id (plist-get r :root-id)
                       :source-kind kind
                       :dolt-dir (plist-get r :dolt-dir)
                       :file (plist-get r :file)
                       :head head
                       :query query))
                 (plan (dorg-ext-read--normalize-plan
                        (funcall (dorg-extension-hydrate ext) ctx)))
                 (rows (dorg-ext-read--project
                        (dorg-ext-read--execute plan r revision)))
                 (entry (list :rows rows
                              :head head
                              :root (plist-get r :root-id)
                              :ext ext-name
                              :source-key (dorg-ext-read--identity
                                           (plist-get r :dolt-dir)
                                           (plist-get r :file)))))
            (puthash key entry entries)
            entry))))))

(defun dorg-ext-hydrate (cache ext-name source query)
  "Serve host-safe rows for EXT-NAME hydrating SOURCE with QUERY via CACHE.
On a warm cache hit (an unchanged freshness key) the cached rows are
returned without re-issuing the grouped query.  On a miss DOrg builds a
`dorg-ext-read-context', calls the extension's `:hydrate' participant for a
grouped query PLAN, executes it set-based — revision-bound to the captured
committed head for a Dolt source, since the entry it populates also serves
`dorg-ext-read-snapshot' warm hits — projects the rows host-safe, caches
them, and returns them.

Returns bare rows (the historical contract); snapshot-aware consumers that
need the committed token the rows were read at call `dorg-ext-read-snapshot'.

Fails closed: signals `dorg-ext-read-error' when EXT-NAME cannot serve the
source kind (`dorg-ext-read-capability')."
  (plist-get (dorg-ext-read--hydrate-entry cache ext-name source query) :rows))

(defun dorg-ext-read-snapshot (cache ext-name source query)
  "Serve a SNAPSHOT-COHERENT versioned read result for EXT-NAME.
Returns a plist `(:rows ROWS :head TOKEN :root RID)'.  TOKEN is the ONE
committed Dolt HEAD (`HASHOF(HEAD)') captured for this read, and the
binding of ROWS to TOKEN is DORG-OWNED: every statement of the
participant's plan executes inside a revision-bound scope at TOKEN
\(`dorg-sql--at-revision' — a Dolt revision database, deterministic
history), so the returned ROWS are provably SELECTED AT TOKEN.  A
concurrent HEAD advance between the token capture and the read — or
between two statements of the plan — cannot tear the result; repeated
and nested-subquery references are bound alike, with no `AS OF' in
participant SQL.  Plans are validated fail-closed BEFORE their first
statement: any form that would resolve outside the scope — database
qualification, statement smuggling, or any `AS OF' (including the
captured hash; the scope is the sole pin) — is rejected with a typed
error and zero plan SQL executed.

A warm cache HIT returns the STORED token and the stored rows from that same
earlier snapshot, never a re-probed live HEAD, so a cached read stays
internally coherent (its rows and its token name one commit).

Fails closed for a Dolt source whose HEAD is not a committed hash token
\(unreadable, or a blank read-back): signals `dorg-ext-read-error' before
any plan SQL executes rather than returning rows tagged with a
non-committed token.  For a plain (non-Dolt) source `:head' is nil — an
explicitly unversioned result, never a bogus token.

TOKEN is the raw committed-hash string, identical to the storage token an
AS-OF cross-check (`... AS OF '<hash>'') uses, so a downstream consumer can
compare read tokens across arms deterministically instead of inferring
coherence from HEAD-probe timing."
  (let ((entry (dorg-ext-read--hydrate-entry cache ext-name source query t)))
    (list :rows (plist-get entry :rows)
          :head (plist-get entry :head)
          :root (plist-get entry :root))))

;;; ─── Push Invalidation (consume mutation-result scopes) ───────────────

(defun dorg-ext-read-invalidate (cache invalidation)
  "Invalidate warm CACHE entries from a mutation-result INVALIDATION plist.
INVALIDATION is the `:invalidation' plist a typed mutation result carries
\(see `dorg-ext-collect-index-projection' / `dorg-mutate'):
  :roots     list of changed root ids
  :ext-dirty alist (EXT-NAME . SCOPE) of extension dirty scopes
  :head      the new Dolt HEAD after the mutation (optional, ignored here)
  :dolt-dir  the mutated Dolt repo dir (optional)
  :file      the source file (optional)

Evicts every cached entry whose root is in :roots OR whose extension is
named in :ext-dirty.  Freshness remains a pull-side cache-key discriminator,
so the next read probes the current source token and misses stale entries
without a write-only known-head table.  Returns the number of entries
evicted."
  (dorg-ext-read--check-cache cache)
  (let* ((roots (plist-get invalidation :roots))
         (ext-dirty (plist-get invalidation :ext-dirty))
         (dirty-exts (mapcar #'car ext-dirty))
         (entries (dorg-ext-read-cache-entries cache))
         (doomed nil))
    (maphash (lambda (key entry)
               (when (or (member (plist-get entry :root) roots)
                         (memq (plist-get entry :ext) dirty-exts))
                 (push key doomed)))
             entries)
    (dolist (key doomed) (remhash key entries))
    (length doomed)))

(provide 'dorg-ext-read)
;;; dorg-ext-read.el ends here
