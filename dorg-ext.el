;;; dorg-ext.el --- Extension API for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Extension API for dorg.  Lets domain libraries (a downstream issue
;; tracker, etc.) register schema extensions that are applied at
;; `dorg-init' time.
;;
;; Usage:
;;
;;   (dorg-register-extension 'myapp
;;     :schema myapp-dorg-schema-ddl   ; string or list of SQL strings
;;     :version 2
;;     :migrations '((1 . myapp-dorg--migration-1-to-2)))
;;
;; Extensions are applied after the core schema in `dorg-init'.  Existing
;; stores install or upgrade registered extensions only through the explicit
;; operator migration entry, `dorg-migrate-schema'.  Extension versions are
;; tracked in `dorg_meta' as `ext:<name>:version'.
;;
;; Schema evolution is a first-class migration contract, not silent
;; drift.  Each `:migrations' entry is keyed by a from-version integer
;; and migrates exactly N -> N+1.  A step is SQL (string or list of
;; strings) or a function of one argument (the dolt-dir) returning SQL;
;; function steps may inspect the repo but must not commit or touch
;; `dorg_meta' — dorg owns the version bump.  Dorg never infers schema
;; diffs and never downgrades; a missing migration step is an actionable
;; error before any write.

;;; Code:

(require 'cl-lib)
;; `dorg-heading-deep-copy' backs the immutable, host-safe index-context images
;; and entry snapshots (tracked-regression): no read into DOrg-owned simulation state.
(require 'dorg-model)
(require 'dorg-sql-quote)

;;; Extension Registry

(cl-defstruct dorg-extension
  "A registered dorg extension.
NAME is a symbol identifying the extension.
SCHEMA is a SQL string (or list of SQL strings) applied at init time.
VERSION is a positive integer tracking the extension's schema version.
MIGRATIONS is an alist of (FROM-VERSION . STEP) entries, each migrating
exactly FROM-VERSION -> FROM-VERSION+1.  STEP is SQL (string or list of
strings) or a function of one argument (the dolt-dir) returning SQL.

CAPABILITIES is a list of mutation/read source kinds (e.g. `dolt') the
extension's participants support; nil defaults to `(dolt)'.  PROJECT-SCOPE is
`bounded' (the default) or `whole-root': the latter is an explicit opt-in for
a projector whose deterministic aggregate requires every canonical heading in
the root, and causes DOrg to materialize a complete candidate image before
calling it.  An aggregate expressible as sums or xors (document frequencies,
corpus totals) is instead declared with `:accumulate' read tables and stays
bounded; see `dorg-ext-fold-accumulators'.
PROJECT and VALIDATE are the index-projector callbacks (see
`dorg-ext-collect-index-projection').  Each is a pure function of a single
`dorg-ext-index-context' argument.  PROJECT returns a list of TYPED ROW DELTAS —
`(:table NAME :insert ((COLUMN . VALUE) …))' or `(:table NAME :delete WHERE)' —
confined to the extension's declared `:read-tables'; it returns NO SQL text (DOrg
generates the SQL itself from the literal, candidate-derived values, so an effect
cannot read database state or reach another table).  VALIDATE may signal to abort
the mutation before commit.  Both must not commit, save buffers, or write
`dorg_meta'.  All default nil, leaving schema-only extensions unchanged.  The
former per-execution `:extension-inputs' caller-payload channel and the standalone
`:dirty-scope' callback are retired (tracked-regression): dirty scope is now derived
deterministically from the single projector result.

READ-TABLES is the read-plane manifest: a list of generic projection-table
descriptors (plists, e.g. (:name \"t\" :key-column \"id\")) describing the
extension-owned tables served by the read plane.  HYDRATE is the read-plane
participant: a pure function of a `dorg-ext-read-context' returning a
grouped query PLAN (see `dorg-ext-hydrate').  Both default nil for
extensions that do not serve grouped reads.

PLACEMENT-INVARIANT is the placement participant: a pure function of a
`dorg-ext-placement-context' returning a `dorg-ext-placement-verdict' (or
nil for no opinion).  It classifies whether a heading whose buffer-derived
star nesting would record it at a different lineage than its stored one is a
legitimate move or a corrupt promotion — knowledge only an extension that
OWNS the heading has (see `dorg-ext-classify-placement').  Defaults nil,
leaving generic DOrg with no opinion on any level-1 heading.

PRODUCER-RESOLVER is an optional policy-upgrade callback.  It resolves a
DOrg-owned request for a content-addressed producer into the installed
projector, immutable implementation/dependency identities, and canonical
policy input.  Ordinary mutation and root rebuild never consult it."
  name schema version migrations
  capabilities project project-scope validate
  read-tables hydrate
  fields query-context
  placement-invariant producer-resolver)

;; The old `dorg-ext-mutation-context' read model and its `:extension-inputs'
;; caller-payload seam are RETIRED (tracked-regression).  Extension participants are
;; now index projectors that consume the DOrg-owned `dorg-ext-index-context'
;; (see below); DOrg no longer constructs a mutation-context on the mutation
;; path.  Downstream consumers migrate to the index projector — the derivability
;; contract (tracked-regression), not a retained shadow mutation API.

;; ─── Owned-state vault: opaque per-record token -> private side table ──
;; A `dorg-ext-index-context' / `dorg-ext-index-entry' must be immutable to any
;; consumer: NO path reachable from the handed record — including introspection
;; of a value returned by a `cl-defstruct'-generated accessor — may recover or
;; mutate the DOrg-owned images, snapshots, patches, or entry lists.  Storing a
;; copy-on-read closure in a struct slot does NOT satisfy this: the generated slot
;; accessor returns the closure, and on an interpreted build `(aref CLOSURE 2)'
;; exposes its captured lexical environment, from which the owned image/heading is
;; recovered by reference (tracked-regression Navigator receipt raw-image-reached=t).
;; Instead each record stores ONLY an opaque per-instance token — a fresh
;; uninterned symbol carrying no data — and the owned state lives here, in a
;; module-private table keyed by that token.  Every generated accessor can then
;; yield only a scalar or the bare token; the owned state is not part of the
;; record's object graph.  Public readers copy OUT of the table, so consumers are
;; isolated from one another.  Weak keys reclaim an entry when its record is GC'd.
(defvar dorg-ext--index-vault-store (make-hash-table :test 'eq :weakness 'key)
  "Private token -> owned-state plist table for index records (tracked-regression).
Keyed by the opaque uninterned-symbol token each record stores; NOT reachable
through any record's generated accessor.  Weak keys collect with the record.")

(defun dorg-ext--index-vault-seal (payload)
  "Store owned-state PAYLOAD under a fresh opaque token and return the token.
The token is a bare uninterned symbol; it holds no reference to PAYLOAD, so a
record carrying it exposes nothing recoverable through a generated accessor."
  (let ((token (make-symbol "dorg-ext-index-vault")))
    (puthash token payload dorg-ext--index-vault-store)
    token))

(defun dorg-ext--index-vault-ref (token)
  "Return the owned-state plist sealed under TOKEN, or nil."
  (gethash token dorg-ext--index-vault-store))

(cl-defstruct (dorg-ext-index-entry
               (:constructor dorg-ext-index-entry--make)
               (:copier nil))
  "One affected heading in a `dorg-ext-index-context'.
KIND is `create', `change', `move', or `delete' — a disjoint partition of the
mutation's effect on this heading.  ID is the heading id.

BEFORE is the immutable S0 snapshot of the heading (its base state); it is nil
for a `create' (the heading did not exist in S0).  AFTER is the immutable
candidate snapshot (its post-simulation state); it is nil for a `delete', which
carries only the tombstone in BEFORE.  A `change' and a `move' carry BOTH: a
`move' is a lineage change (parent/sibling-order/level), a `change' is any other
in-place edit.  BEFORE/AFTER together are sufficient to derive the additions and
removals an index projector must apply, with no caller-authored semantics.

KIND and ID are read-only scalars.  BEFORE/AFTER are NOT struct slots: they are
sealed in the private vault (see the vault commentary) keyed by an opaque --TOKEN,
and read ONLY through `dorg-ext-index-entry-before' / `dorg-ext-index-entry-after',
which return a FRESH deep copy on every call.  The only non-scalar an accessor can
yield is the bare --TOKEN symbol, so no generated accessor can reach the stored
snapshot; a consumer can neither poison DOrg's simulation state nor what a later
consumer observes.  There is no `copier': the record cannot be duplicated into a
shallow-sharing twin."
  (kind nil :read-only t) (id nil :read-only t)
  (--token nil :read-only t))

(cl-defun dorg-ext-index-entry-create (&key kind id before after)
  "Construct an immutable `dorg-ext-index-entry'.
KIND/ID are stored read-only; BEFORE/AFTER are sealed in the private vault under
an opaque token so neither a generated accessor nor a consumer can reach or poison
the stored snapshot."
  (dorg-ext-index-entry--make
   :kind kind :id id
   :--token (dorg-ext--index-vault-seal (list :before before :after after))))

(defun dorg-ext-index-entry-before (entry)
  "Return a fresh deep copy of ENTRY's S0 (`before') snapshot, or nil.
The copy shares no structure with DOrg's simulation state or with any other
consumer's read (tracked-regression host-safe immutability)."
  (let ((h (plist-get (dorg-ext--index-vault-ref
                       (dorg-ext-index-entry---token entry))
                      :before)))
    (and h (dorg-heading-deep-copy h))))

(defun dorg-ext-index-entry-after (entry)
  "Return a fresh deep copy of ENTRY's candidate (`after') snapshot, or nil.
The copy shares no structure with DOrg's simulation state or with any other
consumer's read (tracked-regression host-safe immutability)."
  (let ((h (plist-get (dorg-ext--index-vault-ref
                       (dorg-ext-index-entry---token entry))
                      :after)))
    (and h (dorg-heading-deep-copy h))))

(cl-defstruct (dorg-ext-owner-partition
               (:constructor dorg-ext-owner-partition--make)
               (:copier nil))
  "One affected identity-owner partition in a `dorg-ext-index-context'.
ROOT-ID and OWNER-ID name the owner heading's tree and heading id.
PARENT-OWNER-ID is the nearest ancestor identity owner, or nil when absent.
Mutable/string/composite fields are sealed in the private vault and exposed only
through copy-on-read accessors."
  (root-id nil :read-only t)
  (owner-id nil :read-only t)
  (parent-owner-id nil :read-only t)
  (--token nil :read-only t))

(cl-defun dorg-ext-owner-partition-create
    (&key root-id owner-id owner-key owner-value parent-owner-id
          parent-owner-value affected-ids affected-paths
          base-image candidate-postimage)
  "Construct an immutable `dorg-ext-owner-partition'."
  (dorg-ext-owner-partition--make
   :root-id root-id
   :owner-id owner-id
   :parent-owner-id parent-owner-id
   :--token
   (dorg-ext--index-vault-seal
    (list :owner-key (and owner-key (copy-sequence owner-key))
          :owner-value (and owner-value (copy-sequence owner-value))
          :parent-owner-value (and parent-owner-value
                                   (copy-sequence parent-owner-value))
          :affected-ids (copy-sequence affected-ids)
          :affected-paths (copy-tree affected-paths)
          :base-image base-image
          :candidate-postimage candidate-postimage))))

(defun dorg-ext--owner-partition-field (partition field)
  "Return PARTITION's sealed owned FIELD by reference."
  (plist-get (dorg-ext--index-vault-ref
              (dorg-ext-owner-partition---token partition))
             field))

(defun dorg-ext-owner-partition-owner-key (partition)
  "Return a fresh copy of PARTITION's owner identity key string."
  (let ((s (dorg-ext--owner-partition-field partition :owner-key)))
    (and s (copy-sequence s))))

(defun dorg-ext-owner-partition-owner-value (partition)
  "Return a fresh copy of PARTITION's owner identity value string."
  (let ((s (dorg-ext--owner-partition-field partition :owner-value)))
    (and s (copy-sequence s))))

(defun dorg-ext-owner-partition-parent-owner-value (partition)
  "Return a fresh copy of PARTITION's parent owner identity value string, or nil."
  (let ((s (dorg-ext--owner-partition-field partition :parent-owner-value)))
    (and s (copy-sequence s))))

(defun dorg-ext-owner-partition-affected-ids (partition)
  "Return a fresh copy of PARTITION's affected heading ids."
  (copy-sequence (dorg-ext--owner-partition-field partition :affected-ids)))

(defun dorg-ext-owner-partition-affected-paths (partition)
  "Return a fresh copy of PARTITION's affected heading paths to the owner."
  (copy-tree (dorg-ext--owner-partition-field partition :affected-paths)))

(defun dorg-ext-owner-partition-base-image (partition)
  "Return a fresh deep copy of PARTITION's owner-local S0 image."
  (dorg-ext--copy-image
   (dorg-ext--owner-partition-field partition :base-image)))

(defun dorg-ext-owner-partition-candidate-postimage (partition)
  "Return a fresh deep copy of PARTITION's owner-local candidate image."
  (dorg-ext--copy-image
   (dorg-ext--owner-partition-field partition :candidate-postimage)))

;;; ─── Shared index context construction helpers ─────────────────────────

(defun dorg-ext--index-empty-image ()
  "Return an empty id -> heading image."
  (make-hash-table :test 'eql))

(defun dorg-ext--index-host-safe-image (headings)
  "Return an immutable id -> `dorg-heading' snapshot of HEADINGS.
HEADINGS is an id -> `dorg-heading' hash.  Every heading is deep-copied so the
snapshot shares no structure with a caller's mutable simulation/parse/load state.
A nil HEADINGS yields an empty map."
  (let ((image (dorg-ext--index-empty-image)))
    (when headings
      (maphash (lambda (id h) (puthash id (dorg-heading-deep-copy h) image))
               headings))
    image))

(defun dorg-ext--index-lineage-changed-p (before after)
  "Return non-nil when AFTER's lineage differs from BEFORE's.
Lineage is parent id, sibling sort key, and level offset: the persisted
placement facts that distinguish a move from an in-place content change.  A
derived effective level is not a persisted fact, so its change alone is not
a lineage change; a descendant whose ancestry moved is reported through the
explicit MOVED set of `dorg-ext--index-classify' instead."
  (not (and (eql (dorg-heading-parent-id before) (dorg-heading-parent-id after))
            (equal (dorg-heading-sort-key before) (dorg-heading-sort-key after))
            (eql (dorg-heading-level-offset before)
                 (dorg-heading-level-offset after)))))

(defun dorg-ext--index-classify (orig ps-headings created changed deleted
                                      &optional moved)
  "Partition candidate images into create/change/move/delete index entries.
ORIG and PS-HEADINGS are S0 and candidate id -> `dorg-heading' hashes.
CREATED/CHANGED/DELETED are touched-id accumulators, and MOVED lists ids whose
ancestry changed even where their persisted tuple did not.  Net membership
decides the disjoint partition, each id in exactly one: present only in the
candidate is a create, and present only in S0 is a delete.  Present in both,
a MOVED id is a move; otherwise a touched id is a move when its lineage
changed and a change when it did not.  Returns (:created ES :changed ES
:moved ES :deleted ES), each sorted by id."
  (let ((touched (make-hash-table :test 'eql))
        (moved-set (make-hash-table :test 'eql))
        (creates nil) (changes nil) (moves nil) (deletes nil)
        (by-id (lambda (a b) (< (dorg-ext-index-entry-id a)
                                (dorg-ext-index-entry-id b)))))
    (dolist (id (append created changed deleted moved)) (puthash id t touched))
    (dolist (id moved) (puthash id t moved-set))
    (when ps-headings
      (maphash
       (lambda (id after)
         (let ((before (gethash id orig)))
           (cond
            ((null before)
             (push (dorg-ext-index-entry-create
                    :kind 'create :id id :after (dorg-heading-deep-copy after))
                   creates))
            ((gethash id touched)
             (if (or (gethash id moved-set)
                     (dorg-ext--index-lineage-changed-p before after))
                 (push (dorg-ext-index-entry-create
                        :kind 'move :id id
                        :before (dorg-heading-deep-copy before)
                        :after (dorg-heading-deep-copy after))
                       moves)
               (push (dorg-ext-index-entry-create
                      :kind 'change :id id
                      :before (dorg-heading-deep-copy before)
                      :after (dorg-heading-deep-copy after))
                     changes))))))
       ps-headings))
    (when orig
      (maphash
       (lambda (id before)
         (unless (and ps-headings (gethash id ps-headings))
           (push (dorg-ext-index-entry-create
                  :kind 'delete :id id :before (dorg-heading-deep-copy before))
                 deletes)))
       orig))
    (list :created (sort creates by-id)
          :changed (sort changes by-id)
          :moved (sort moves by-id)
          :deleted (sort deletes by-id))))

(defun dorg-ext--index-heading-canonical (h)
  "Return a COMPLETE canonical tuple of `dorg-heading' H's persisted state.
The level fact is the persisted offset, never the derived effective level:
readback images are built from id slices that cannot derive it."
  (list (dorg-heading-id h)
        (dorg-heading-parent-id h)
        (dorg-heading-sort-key h)
        (dorg-heading-level-offset h)
        (dorg-heading-todo h)
        (dorg-heading-priority h)
        (dorg-heading-commented h)
        (dorg-heading-title h)
        (dorg-heading-post-blank h)
        (dorg-heading-tags h)
        (dorg-heading-items-hash h)))

(defun dorg-ext--index-image-canonical (image)
  "Return sorted canonical heading rows for IMAGE."
  (let (rows)
    (when image
      (maphash (lambda (_id h)
                 (push (dorg-ext--index-heading-canonical h) rows))
               image))
    (sort rows (lambda (a b) (< (car a) (car b))))))

(defun dorg-ext--index-owner-partition-canonical (partition)
  "Return a canonical digest row for owner PARTITION."
  (list
   :root-id (dorg-ext-owner-partition-root-id partition)
   :owner-id (dorg-ext-owner-partition-owner-id partition)
   :owner-key (dorg-ext-owner-partition-owner-key partition)
   :owner-value (dorg-ext-owner-partition-owner-value partition)
   :parent-owner-id (dorg-ext-owner-partition-parent-owner-id partition)
   :parent-owner-value (dorg-ext-owner-partition-parent-owner-value partition)
   :affected-ids (sort (copy-sequence
                        (dorg-ext-owner-partition-affected-ids partition))
                       #'<)
   :affected-paths (sort (copy-tree
                          (dorg-ext-owner-partition-affected-paths partition))
                         (lambda (a b)
                           (or (< (plist-get a :id) (plist-get b :id))
                               (and (= (plist-get a :id) (plist-get b :id))
                                    (string< (symbol-name (plist-get a :side))
                                             (symbol-name (plist-get b :side)))))))
   :base-image (dorg-ext--index-image-canonical
                (dorg-ext-owner-partition-base-image partition))
   :candidate-postimage (dorg-ext--index-image-canonical
                         (dorg-ext-owner-partition-candidate-postimage partition))))

(defun dorg-ext--index-owner-partition-canonical< (a b)
  "Non-nil when canonical owner partition row A sorts before B."
  (or (< (plist-get a :root-id) (plist-get b :root-id))
      (and (= (plist-get a :root-id) (plist-get b :root-id))
           (< (plist-get a :owner-id) (plist-get b :owner-id)))))

(defun dorg-ext--index-candidate-id (candidate-image &optional owner-partitions)
  "Return a deterministic transaction-scoped identity for CANDIDATE-IMAGE.
The digest is derived from the complete candidate heading image and affected
owner partitions.  It is not a Dolt commit token; it names the candidate state
before persistence."
  (secure-hash
   'sha1
   (prin1-to-string
    (list
     :candidate-postimage (dorg-ext--index-image-canonical candidate-image)
     :owner-partitions
     (sort (mapcar #'dorg-ext--index-owner-partition-canonical
                   owner-partitions)
           #'dorg-ext--index-owner-partition-canonical<)))))

(cl-defstruct (dorg-ext-index-context
               (:constructor dorg-ext-index-context--make)
               (:copier nil))
  "Immutable canonical post-mutation projection context (tracked-regression).
DOrg constructs exactly ONE of these AFTER typed core simulation and BEFORE
persistence, from its OWN simulated core state.  Registered index projectors
consume it (tracked-regression) INSTEAD of caller-authored semantic extension
payloads: every field is DOrg-derived and read-only.

SOURCE-KIND is the mutation source kind (e.g. `dolt', `plain-org').  ROOT-ID is
the mutated tree's root id; DOLT-DIR and FILE locate the backing store.  For a
cross-root move the subject spans two roots: FROM-ROOT-ID and TO-ROOT-ID name the
source and destination roots (ROOT-ID is the destination), and both are nil for
an ordinary same-root mutation — so a root-aggregate projector can attribute the
removal to FROM-ROOT-ID and the addition to TO-ROOT-ID.

BASE-COMMIT is the exact base/CAS token the plan is bound to — the immutable S0
identity.  CANDIDATE-ID is a transaction-scoped digest of the candidate
post-state — a DOrg-computed candidate-state identity, DISTINCT from BASE-COMMIT
and NOT a Dolt commit token.  The context NEVER exposes the future commit token:
`:new-token' exists only in the native execution result after core AND derived
effects commit atomically, so this struct has no such slot.

PATCHES is the canonical resolved typed patch batch (no semantic vocabulary).

CREATED, CHANGED, MOVED, and DELETED are lists of `dorg-ext-index-entry' — the
disjoint per-heading partition of the mutation's effect, each carrying the
before/after or tombstone data sufficient to derive additions and removals.

BASE-IMAGE and CANDIDATE-POSTIMAGE are host-safe id -> `dorg-heading' read views
of, respectively, the immutable S0 scope and the candidate post-state.  They are
read through `dorg-ext-index-context-base-image' /
`dorg-ext-index-context-candidate-postimage', which return a FRESH deep copy on
every call: the stored images are deep copies of DOrg's OWN simulated state, so a
consumer can neither reach the private mutable working model (`dorg-mutate--ps')
nor poison what a later consumer observes; they are NEVER a host-synthesized
reparse.  Binding BOTH lets a consumer compare S0 against the candidate (e.g.
prove `core remains N') BEFORE apply.  For a cross-root move both images UNION the
source and destination roots, so a root-aggregate projector sees the moved ids
under their source lineage in BASE-IMAGE and under their destination lineage in
CANDIDATE-POSTIMAGE.  For a backend that does not materialize a candidate tree
\(plain-org, which preserves core semantics without a durable shadow index) both
are nil; the Dolt backend supplies real images.

SCOPE: ordinary mutation and import images span the transaction's BOUNDED
WORKING SCOPE — the same id slice the core simulation loaded (root row, direct
targets, anchors, current parents, and affected-parent sibling fans), unless an
eligible registered projector explicitly declares `:project-scope whole-root'.
That explicit aggregate opt-in causes DOrg to materialize the whole root before
simulation; projectors still receive no store-read capability.  In either case,
every heading the mutation could have read or written is present and headings
outside a bounded scope are unchanged and not materialized.
`dorg-ext-rebuild-index' is a distinct
administrative, exact-revision operation: it supplies an empty base image and a
complete requested-root candidate image, with every entry classified `created'.
It does not fabricate mutation patches or relax this bounded-context contract.

Only the truly immutable identity scalars — SOURCE-KIND (a symbol) and the
integer ROOT-ID / FROM-ROOT-ID / TO-ROOT-ID — are `:read-only' struct slots.
Everything else is sealed in the private vault (see the vault commentary) keyed by
an opaque --TOKEN and read ONLY through the like-named accessors, each of which
returns a FRESH copy per call.  This deliberately INCLUDES the string-valued
identity/path fields (DOLT-DIR, FILE, BASE-COMMIT, CANDIDATE-ID): Emacs strings are
mutable objects, so a generated accessor returning a stored string by reference
would let one consumer `aset' the exact S0/candidate binding another consumer
observes.  The owned COMPOSITES — PATCHES and the CREATED/CHANGED/MOVED/DELETED
entry lists — and the BASE-IMAGE / CANDIDATE-POSTIMAGE hashes are likewise vaulted.
The only non-scalar any generated accessor can yield is the bare --TOKEN symbol, so
no generated accessor can reach the stored mutable state and the whole context is
immutable to any consumer once constructed: a first consumer that mutates a
composite spine, a nested patch string, an image, an identity string, or a heading
it reads cannot poison what a later consumer observes.  There is no `copier'."
  (source-kind nil :read-only t) (root-id nil :read-only t)
  (from-root-id nil :read-only t) (to-root-id nil :read-only t)
  (--token nil :read-only t))

(defun dorg-ext--copy-image (image)
  "Return an independent deep copy of host-safe IMAGE, or nil.
IMAGE is an id -> `dorg-heading' hash; every heading is deep-copied into a fresh
hash so the result shares no structure with IMAGE, DOrg's simulation state, or
any other consumer's read."
  (when image
    (let ((copy (make-hash-table :test 'eql :size (max 1 (hash-table-count image)))))
      (maphash (lambda (id h) (puthash id (dorg-heading-deep-copy h) copy)) image)
      copy)))

(defun dorg-ext--deep-copy (value)
  "Return a deep structural copy of VALUE sharing no MUTABLE leaf with it.
Recursively copies cons cells, vectors, and strings, so no consumer can `aset' a
string leaf or `setcar'/`aset' a shared cell to poison the stored context, a later
reader, or the source object.  Symbols, numbers, and other immutable atoms are
returned as-is.  Used for the typed patch IR, whose leaves include mutable title
and sort-key strings that `copy-tree' would share.  Cons spines are iterative so
batch width does not consume Lisp depth."
  (cond
   ((consp value)
    ;; Reject unsupported circular spines without formatting their payload.
    (condition-case nil
        (unless (cl-list-length value) (signal 'circular-list nil))
      (wrong-type-argument nil))
    (let* ((head (cons nil nil))
           (tail head))
      (while (consp value)
        (setcdr tail (cons (dorg-ext--deep-copy (car value)) nil))
        (setq tail (cdr tail)
              value (cdr value)))
      (setcdr tail (dorg-ext--deep-copy value))
      (cdr head)))
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (let ((v (copy-sequence value)))
      (dotimes (i (length v)) (aset v i (dorg-ext--deep-copy (aref v i))))
      v))
   (t value)))

(cl-defun dorg-ext-index-context-create
    (&key source-kind root-id from-root-id to-root-id dolt-dir file owner-request
          base-commit candidate-id patches created changed moved deleted
          base-image candidate-postimage owner-partitions)
  "Construct an immutable `dorg-ext-index-context'.
The immutable identity scalars SOURCE-KIND, ROOT-ID, FROM-ROOT-ID, and
TO-ROOT-ID are stored read-only.  Everything mutable is sealed in the private
vault under an opaque token, copied IN so the record owns it: the string-valued
DOLT-DIR, FILE, BASE-COMMIT, and CANDIDATE-ID are `copy-sequence'd; PATCHES is
deep-copied (copying nested string/vector leaves `copy-tree' shares); the
CREATED, CHANGED, MOVED, and DELETED entry lists and the BASE-IMAGE /
CANDIDATE-POSTIMAGE hashes are stored as owned.  With copy-on-read readers, no
generated accessor and no consumer can reach or poison DOrg's simulation state,
the source patches, or a later read."
  (dorg-ext-index-context--make
   :source-kind source-kind :root-id root-id
   :from-root-id from-root-id :to-root-id to-root-id
   :--token (dorg-ext--index-vault-seal
             (list :dolt-dir (and dolt-dir (copy-sequence dolt-dir))
                   :file (and file (copy-sequence file))
                   :base-commit (and base-commit (copy-sequence base-commit))
                   :candidate-id (and candidate-id (copy-sequence candidate-id))
                   :patches (dorg-ext--deep-copy patches)
                   :owner-request (dorg-ext--deep-copy owner-request)
                   :created created :changed changed
                   :moved moved :deleted deleted
                   :base-image base-image
                   :candidate-postimage candidate-postimage
                   :owner-partitions (copy-sequence owner-partitions)))))

(defun dorg-ext--index-context-field (context field)
  "Return CONTEXT's sealed owned FIELD by reference (internal, no copy).
FIELD is one of :dolt-dir :file :base-commit :candidate-id :patches :created
:changed :moved :deleted :base-image :candidate-postimage :owner-partitions.
Reachable only by naming this private function — not through any value a
generated accessor of CONTEXT returns."
  (plist-get (dorg-ext--index-vault-ref (dorg-ext-index-context---token context))
             field))

(defun dorg-ext-index-context-dolt-dir (context)
  "Return a fresh copy of CONTEXT's `dolt-dir' path string, or nil.
Copy-on-read: strings are mutable, so a fresh copy per call keeps the store
path a later consumer observes pristine (tracked-regression host-safe immutability)."
  (let ((s (dorg-ext--index-context-field context :dolt-dir)))
    (and s (copy-sequence s))))

(defun dorg-ext-index-context-file (context)
  "Return a fresh copy of CONTEXT's `file' path string, or nil.
Copy-on-read: see `dorg-ext-index-context-dolt-dir'."
  (let ((s (dorg-ext--index-context-field context :file)))
    (and s (copy-sequence s))))

(defun dorg-ext-index-context-base-commit (context)
  "Return a fresh copy of CONTEXT's exact base/CAS (S0) token string, or nil.
Copy-on-read: strings are mutable, so a fresh copy per call keeps the immutable
S0 binding a later consumer observes pristine (tracked-regression)."
  (let ((s (dorg-ext--index-context-field context :base-commit)))
    (and s (copy-sequence s))))

(defun dorg-ext-index-context-candidate-id (context)
  "Return a fresh copy of CONTEXT's candidate-state identity digest string, or nil.
Copy-on-read: strings are mutable, so a fresh copy per call keeps the candidate
identity a later consumer observes pristine (tracked-regression)."
  (let ((s (dorg-ext--index-context-field context :candidate-id)))
    (and s (copy-sequence s))))

(defun dorg-ext-index-context-patches (context)
  "Return a fresh deep copy of CONTEXT's canonical resolved typed patch batch.
Copy-on-read via `dorg-ext--deep-copy': the result shares neither spine nor a
mutable string/vector leaf with the stored batch, so a consumer cannot corrupt
what a later consumer reads (tracked-regression host-safe immutability)."
  (dorg-ext--deep-copy (dorg-ext--index-context-field context :patches)))

(defun dorg-ext-index-context-owner-request (context)
  "Return a detached immutable owner request for CONTEXT, when present.
This is planning data, never a capability and never a public mutation form."
  (dorg-ext--deep-copy (dorg-ext--index-context-field context :owner-request)))

(defun dorg-ext-index-context-created (context)
  "Return a fresh copy of CONTEXT's `create' `dorg-ext-index-entry' list.
Copy-on-read: the result is an independent spine over immutable entries, so a
consumer cannot corrupt what a later consumer reads (tracked-regression)."
  (copy-sequence (dorg-ext--index-context-field context :created)))

(defun dorg-ext-index-context-changed (context)
  "Return a fresh copy of CONTEXT's `change' `dorg-ext-index-entry' list.
Copy-on-read: the result is an independent spine over immutable entries, so a
consumer cannot corrupt what a later consumer reads (tracked-regression)."
  (copy-sequence (dorg-ext--index-context-field context :changed)))

(defun dorg-ext-index-context-moved (context)
  "Return a fresh copy of CONTEXT's `move' `dorg-ext-index-entry' list.
Copy-on-read: the result is an independent spine over immutable entries, so a
consumer cannot corrupt what a later consumer reads (tracked-regression)."
  (copy-sequence (dorg-ext--index-context-field context :moved)))

(defun dorg-ext-index-context-deleted (context)
  "Return a fresh copy of CONTEXT's `delete' `dorg-ext-index-entry' list.
Copy-on-read: the result is an independent spine over immutable entries, so a
consumer cannot corrupt what a later consumer reads (tracked-regression)."
  (copy-sequence (dorg-ext--index-context-field context :deleted)))

(defun dorg-ext-index-context-base-image (context)
  "Return a fresh deep copy of CONTEXT's S0 base image, or nil.
The image is an id -> `dorg-heading' hash.  Copy-on-read: the result shares no
structure with DOrg's simulation state or with any other consumer's read
\(tracked-regression host-safe immutability)."
  (dorg-ext--copy-image (dorg-ext--index-context-field context :base-image)))

(defun dorg-ext-index-context-candidate-postimage (context)
  "Return a fresh deep copy of CONTEXT's candidate postimage, or nil.
The image is an id -> `dorg-heading' hash.  Copy-on-read: the result shares no
structure with DOrg's simulation state or with any other consumer's read
\(tracked-regression host-safe immutability)."
  (dorg-ext--copy-image (dorg-ext--index-context-field context :candidate-postimage)))

(defun dorg-ext-index-context-owner-partitions (context)
  "Return a fresh list spine of CONTEXT's affected owner partitions."
  (copy-sequence (dorg-ext--index-context-field context :owner-partitions)))

;; KEEP (mutable-state): extension registry, setq/push-mutated; must persist across calls.
(defvar dorg--extensions nil
  "Alist of (NAME . `dorg-extension') for registered extensions.
Order of registration is preserved; extensions are applied at
init time in registration order.")

(defconst dorg-ext--name-re "\\`[a-zA-Z][a-zA-Z0-9_-]*\\'"
  "Regexp matching valid extension names.
Only ASCII letters, digits, hyphens, and underscores are allowed,
starting with a letter.  This prevents SQL injection when the name
is interpolated into metadata keys.")

(defconst dorg-ext--registration-keys
  '(:schema :version :migrations :capabilities :project :project-scope :validate
    :read-tables :hydrate :placement-invariant :fields :query-context
    :producer-resolver)
  "Accepted keyword arguments for `dorg-register-extension'.")

(defconst dorg-ext--read-table-keys
  '(:name :key-column :scope-column :scope-parent :memberships :accumulate :marker)
  "Closed keyword set for one `:read-tables' descriptor.")

(defconst dorg-ext--scope-parent-keys '(:table :key-column)
  "Closed keyword set for a one-edge rebuild scope-parent descriptor.")

(defconst dorg-ext--membership-keys
  '(:name :member-key :position-column :value-column :bridge-table
    :bridge-key :heading-column :root-column)
  "Closed keyword set for one extension query-membership descriptor.")

(defconst dorg-ext--field-keys
  '(:name :type :cardinality :dependencies :context :sql :eval)
  "Keys in a query field declaration owned by an extension.")

(defun dorg-ext--validate-fields (extension fields)
  "Validate EXTENSION's typed query FIELDS before changing the registry."
  (unless (proper-list-p fields)
    (error "dorg-register-extension: :fields must be a proper list"))
  (let ((prefix (concat (symbol-name extension) "/")) seen)
    (dolist (field fields)
      (dorg-ext--validate-plist-keys field dorg-ext--field-keys "query field")
      (let ((name (plist-get field :name)))
        (unless (and (symbolp name)
                     (string-prefix-p prefix (symbol-name name))
                     (string-match-p dorg-ext--name-re
                                     (substring (symbol-name name) (length prefix))))
          (error "dorg-register-extension: field %S must belong to %s" name prefix))
        (when (memq name seen)
          (error "dorg-register-extension: duplicate field %S" name))
        (push name seen))
      (unless (memq (plist-get field :type) '(string identity integer boolean timestamp))
        (error "dorg-register-extension: invalid field type %S" (plist-get field :type)))
      (unless (memq (plist-get field :cardinality) '(one many))
        (error "dorg-register-extension: invalid field cardinality"))
      (unless (and (proper-list-p (plist-get field :context))
                   (cl-every #'keywordp (plist-get field :context)))
        (error "dorg-register-extension: field context must list keyword dependencies"))
      (unless (proper-list-p (plist-get field :dependencies))
        (error "dorg-register-extension: field dependencies must form a list"))
      (dolist (dependency (plist-get field :dependencies))
        (pcase dependency
          (`(field ,(or 'id 'title 'todo 'priority 'commented 'level 'parent-id)))
          (`(property ,(pred stringp)))
          (_ (error "dorg-register-extension: invalid stored dependency %S" dependency))))
      (unless (or (plist-get field :sql) (plist-get field :eval))
        (error "dorg-register-extension: field needs at least one backend implementation"))
      (dolist (key '(:sql :eval))
        (when (and (plist-get field key) (not (functionp (plist-get field key))))
          (error "dorg-register-extension: field %s must be a function" key))))))

(defun dorg-ext--validate-plist-keys (plist allowed subject)
  "Validate PLIST as an even keyword plist confined to ALLOWED for SUBJECT."
  (let ((length (proper-list-p plist)))
    (unless (and length (zerop (% length 2)))
      (error "dorg-register-extension: %s must be a key/value plist" subject)))
  (let ((rest plist)
        seen)
    (while rest
      (let ((key (pop rest)))
        (unless (memq key allowed)
          (error "dorg-register-extension: unknown %s key %S" subject key))
        (when (memq key seen)
          (error "dorg-register-extension: duplicate %s key %S" subject key))
        (push key seen)
        (pop rest)))))

(defun dorg-ext--validate-read-tables (read-tables)
  "Validate READ-TABLES and every declared query-membership fact.
Returns READ-TABLES unchanged.  All identifier validation happens before an
extension enters the registry, because these values are later spliced into
DOrg-generated SQL."
  (unless (or (null read-tables)
              (and (listp read-tables) (cl-every #'listp read-tables)))
    (error "dorg-register-extension: :read-tables must be a list of table descriptor plists"))
  (let (relations)
    (dolist (table read-tables)
      (dorg-ext--validate-plist-keys
       table dorg-ext--read-table-keys ":read-tables descriptor")
      (unless (plist-member table :name)
        (error "dorg-register-extension: :read-tables descriptor needs :name"))
      (dolist (key '(:name :key-column :scope-column))
        (when (plist-member table key)
          (let ((identifier (plist-get table key)))
            (unless (dorg-ext--index-identifier-safe-p identifier)
              (error "dorg-register-extension: %s %S is not a safe identifier"
                     key identifier)))))
      (when-let ((parent (plist-get table :scope-parent)))
        (dorg-ext--validate-plist-keys
         parent dorg-ext--scope-parent-keys ":scope-parent descriptor")
        (dolist (key '(:table :key-column))
          (let ((identifier (plist-get parent key)))
            (unless (dorg-ext--index-identifier-safe-p identifier)
              (error "dorg-register-extension: :scope-parent %s %S is not a safe identifier"
                     key identifier)))))
      (let ((memberships (plist-get table :memberships)))
        (unless (or (null memberships)
                    (and (listp memberships) (cl-every #'listp memberships)))
          (error "dorg-register-extension: :memberships must be a list of descriptor plists"))
        (dolist (membership memberships)
          (dorg-ext--validate-plist-keys
           membership dorg-ext--membership-keys ":memberships descriptor")
          (let ((relation (plist-get membership :name)))
            (unless (and (symbolp relation)
                         (string-match-p dorg-ext--name-re (symbol-name relation)))
              (error "dorg-register-extension: membership :name %S is invalid"
                     relation))
            (when (memq relation relations)
              (error "dorg-register-extension: duplicate membership name %S"
                     relation))
            (push relation relations))
          (dolist (key '(:member-key :position-column :value-column
                         :bridge-table :bridge-key :heading-column :root-column))
            (let ((identifier (plist-get membership key)))
              (unless (dorg-ext--index-identifier-safe-p identifier)
                (error "dorg-register-extension: membership %s %S is not a safe identifier"
                       key identifier)))))))
    ;; Scope parents deliberately use their own declared join key.  The parent
    ;; descriptor's :key-column is read-plane metadata, not a uniqueness law.
    (dolist (table read-tables)
      (when-let* ((parent (plist-get table :scope-parent))
                  (parent-table (plist-get parent :table))
                  (parent-descriptor
                   (cl-find parent-table read-tables
                            :key (lambda (d) (plist-get d :name)) :test #'string=)))
        (when (or (string= parent-table (plist-get table :name))
                  (plist-get parent-descriptor :scope-parent)
                  (not (dorg-ext--index-identifier-safe-p
                        (plist-get parent-descriptor :scope-column))))
          (error "dorg-register-extension: :scope-parent must name a distinct directly scoped table")))
      (when (and (plist-get table :scope-parent)
                 (not (cl-find (plist-get (plist-get table :scope-parent) :table)
                               read-tables
                               :key (lambda (d) (plist-get d :name)) :test #'string=)))
        (error "dorg-register-extension: :scope-parent names no owned parent table")))
    (dorg-ext--validate-accumulator-declarations read-tables)
    read-tables))

(defun dorg-ext--validate-accumulator-declarations (read-tables)
  "Validate the accumulator and marker declarations of READ-TABLES.
An `:accumulate' table names its complete key columns, scope column included.
Whenever any table accumulates, exactly one accumulating table is the
completeness `:marker'.  Its value is t when the key is the scope column
alone, else the required non-scope key alists every built root must hold.
Violations signal `dorg-ext-index-error' before registration."
  (let (markers accumulating)
    (dolist (table read-tables)
      (let ((key (plist-get table :accumulate))
            (scope (and (plist-get table :scope-column)
                        (intern (plist-get table :scope-column)))))
        (when (plist-member table :accumulate)
          (push table accumulating)
          (unless (and key (proper-list-p key)
                       (cl-every #'dorg-ext--index-column-safe-p key)
                       (equal key (delete-dups (copy-sequence key)))
                       scope (memq scope key))
            (dorg-ext--index-raise
             'accumulator-shape
             "an :accumulate table names its complete key, scope column included"
             :table (plist-get table :name) :accumulate key)))
        (when (plist-member table :marker)
          (push table markers)
          (let ((required (plist-get table :marker))
                (rest (and key (remq scope key))))
            (unless (and key
                         (if (eq required t)
                             (null rest)
                           (and (consp required) (proper-list-p required)
                                (cl-every
                                 (lambda (alist)
                                   (and (proper-list-p alist)
                                        (equal (mapcar #'car alist) rest)
                                        (cl-every (lambda (cell)
                                                    (dorg-ext--index-literal-p (cdr cell)))
                                                  alist)))
                                 required)
                                (equal required (delete-dups (copy-sequence required))))))
              (dorg-ext--index-raise
               'marker-declaration
               "a :marker is an :accumulate table naming its required non-scope keys"
               :table (plist-get table :name) :marker required))))))
    (when (and accumulating (/= 1 (length markers)))
      (dorg-ext--index-raise
       'marker-declaration
       "accumulator tables need exactly one completeness :marker table"
       :markers (mapcar (lambda (table) (plist-get table :name)) markers)))))

(defun dorg-ext--sql-spec-p (spec)
  "Return non-nil if SPEC is a SQL string or a non-empty list of SQL strings."
  (or (stringp spec)
      (and (consp spec) (cl-every #'stringp spec))))

(defun dorg-ext-normalize-sql (spec)
  "Return SPEC as a list of SQL strings.
SPEC is a SQL string or a list of SQL strings.  Signals an error
otherwise."
  (cond
   ((stringp spec) (list spec))
   ((and (listp spec) (cl-every #'stringp spec)) spec)
   (t (error "dorg-ext: expected SQL string or list of strings, got %S" spec))))

(defun dorg-ext--migration-step-valid-p (step)
  "Return non-nil if STEP is a valid migration step.
A step is SQL (string or list of strings) or a function — a lambda,
a bound function, or a symbol naming a function that may be defined
later (validated lazily when the migration runs)."
  (cond
   ((functionp step) t)                              ; lambda / bound symbol
   ((stringp step) t)                                ; single SQL statement
   ((and (consp step) (cl-every #'stringp step)) t)  ; list of SQL statements
   ((and (symbolp step) step) t)                     ; deferred function name
   (t nil)))

(defun dorg-ext--validate-migrations (migrations)
  "Validate MIGRATIONS, an alist of (FROM-VERSION . STEP) entries.
Signals an error on a malformed alist, a non-positive-integer
from-version, or an invalid step.  Nil is allowed (no migrations)."
  (when migrations
    (unless (and (listp migrations) (cl-every #'consp migrations))
      (error "dorg-register-extension: :migrations must be an alist of (FROM-VERSION . STEP)"))
    (dolist (entry migrations)
      (let ((from (car entry))
            (step (cdr entry)))
        (unless (and (integerp from) (> from 0))
          (error "dorg-register-extension: migration from-version must be a positive integer, got %S"
                 from))
        (unless (dorg-ext--migration-step-valid-p step)
          (error "dorg-register-extension: migration step for v%S must be SQL string(s) or a function, got %S"
                 from step))))))

(defun dorg-register-extension (name &rest args)
  "Register extension NAME with schema, version, and optional migrations.
NAME must be a symbol whose name matches `dorg-ext--name-re'.
Keyword ARGS:
  :schema       SQL string or list of SQL strings applied at init (required)
  :version      Positive integer extension schema version (required)
  :migrations   Alist of (FROM-VERSION . STEP) migration steps (optional)
  :capabilities List of mutation source kinds the participant supports
                (optional; nil defaults to `(dolt)')
  :project      Index projector: pure fn of a `dorg-ext-index-context'
                returning a list of TYPED ROW DELTAS ((:table NAME :insert/:delete
                …), NOT SQL) confined to the extension's `:read-tables' (optional)
  :project-scope `bounded' (default) or `whole-root'.  A whole-root projector
                receives a complete canonical root candidate image so it can
                derive root-wide aggregate rows without an extension store read.
                Sum- or xor-shaped aggregates use `:accumulate' tables instead.
  :validate     Pure fn of a `dorg-ext-index-context'; signalling aborts the
                whole transaction before commit (optional)
  :read-tables  Read-plane manifest: list of generic projection-table
                descriptor plists (optional; see `dorg-extension').  A
                descriptor may add `:accumulate KEY-COLUMNS' (an aggregate
                table maintained by accumulator effects) and, on exactly one
                such table, `:marker' t or its required non-scope keys per
                root (see `dorg-ext--validate-accumulator-declarations')
  :hydrate      Read participant: pure fn of a `dorg-ext-read-context'
                returning a grouped query PLAN (optional; see
                `dorg-ext-hydrate')
  :fields       Typed query descriptors.  Each declares a namespaced name,
                scalar type, cardinality, dependencies, and `:eval' and/or
                `:sql' evaluator (optional)
  :query-context Pure fn of one invocation plist, called once when one of
                this extension's fields needs context.  The plist has source,
                root, captured revision, and a revision-bound `:read'
                capability.  Return `(:values PLIST ...)' of read/print-safe
                data; DOrg copies and digests it before field evaluation.
                (optional)
  :placement-invariant  Placement participant: pure fn of a
                `dorg-ext-placement-context' returning a
                `dorg-ext-placement-verdict' or nil (optional; see
                `dorg-ext-classify-placement')
  :producer-resolver Optional policy-upgrade resolver.  It receives one
                DOrg-owned request plist
                \(:producer IDENTITY :source SOURCE :revision H
                 :identity POLICY :scope (:root-id ROOT)), where IDENTITY is
                the requested (:implementation :dependencies :policy-digest)
                producer, SOURCE the normalized store directory and H the
                frozen base revision, and returns
                \(:project FN :implementation STRING :dependencies STRING
                 :policy STRING): the installed projector, the immutable
                implementation/dependency identities it actually binds and
                the canonical policy input.  DOrg computes the policy digest
                and refuses any mismatch.  Ordinary mutation and root rebuild
                do not call it.

Each migration step migrates exactly FROM-VERSION -> FROM-VERSION+1.
STEP is SQL (string or list of strings) or a function of one argument
\(the dolt-dir) returning SQL; function steps may inspect the repo but
must use `dorg-repo-migration-query' for repo reads during explicit migration
and must not commit or update `dorg_meta'.  Ordinary open never runs migration
steps; existing-store install/upgrade is owned by `dorg-migrate-schema'.

The participant callbacks are pure and must not commit Dolt, save buffers, or
write `dorg_meta'.  The `:project' index projector returns TYPED ROW DELTAS
\(DOrg generates the SQL); the `:hydrate' participant returns read-plane query
plans.  DOrg owns planning, ordering, stale checks, rollback, metadata, and
commit; it lowers the projector's typed effects to SQL and applies them inside
the SAME transaction as core mutation/import SQL (see
`dorg-ext-collect-index-projection').

Re-registering the same NAME replaces the prior entry while
preserving its position in the registration order."
  (unless (symbolp name)
    (error "dorg-register-extension: NAME must be a symbol, got %S" name))
  (unless (string-match-p dorg-ext--name-re (symbol-name name))
    (error "dorg-register-extension: invalid extension name %S (must match %s)"
           name dorg-ext--name-re))
  (let ((length (proper-list-p args)))
    (unless (and length (zerop (% length 2)))
      (error "dorg-register-extension: arguments must be a key/value list"))
    (let ((rest args))
      (while rest
        (let ((key (pop rest)))
          (unless (memq key dorg-ext--registration-keys)
            (error "dorg-register-extension: unknown extension option %S" key))
          (pop rest)))))
  (let ((schema (plist-get args :schema))
        (version (plist-get args :version))
        (migrations (plist-get args :migrations))
        (capabilities (plist-get args :capabilities))
        (project (plist-get args :project))
        (project-scope (or (plist-get args :project-scope) 'bounded))
        (validate (plist-get args :validate))
        (read-tables (plist-get args :read-tables))
        (hydrate (plist-get args :hydrate))
        (fields (plist-get args :fields))
        (query-context (plist-get args :query-context))
        (placement-invariant (plist-get args :placement-invariant))
        (producer-resolver (plist-get args :producer-resolver)))
    (unless (dorg-ext--sql-spec-p schema)
      (error "dorg-register-extension: :schema must be a SQL string or list of SQL strings"))
    (unless (and (integerp version) (> version 0))
      (error "dorg-register-extension: :version must be a positive integer"))
    (dorg-ext--validate-migrations migrations)
    (unless (or (null capabilities)
                (and (listp capabilities) (cl-every #'symbolp capabilities)))
      (error "dorg-register-extension: :capabilities must be a list of source-kind symbols"))
    (unless (memq project-scope '(bounded whole-root))
      (error "dorg-register-extension: :project-scope must be bounded or whole-root"))
    (when (and (eq project-scope 'whole-root) (not project))
      (error "dorg-register-extension: whole-root scope requires a :project callback"))
    (dolist (cb (list (cons :project project)
                      (cons :validate validate)
                      (cons :hydrate hydrate)
                      (cons :query-context query-context)
                      (cons :placement-invariant placement-invariant)
                      (cons :producer-resolver producer-resolver)))
      (unless (or (null (cdr cb)) (functionp (cdr cb)))
        (error "dorg-register-extension: %s must be a function, got %S"
               (car cb) (cdr cb))))
    (dorg-ext--validate-read-tables read-tables)
    (dorg-ext--validate-fields name fields)
    (let ((ext (make-dorg-extension :name name
                                    :schema schema
                                    :version version
                                    :migrations migrations
                                    :capabilities capabilities
                                    :project project
                                    :project-scope project-scope
                                    :validate validate
                                    :read-tables read-tables
                                    :hydrate hydrate
                                    :fields (copy-tree fields)
                                    :query-context query-context
                                    :placement-invariant placement-invariant
                                    :producer-resolver producer-resolver))
          (existing (assq name dorg--extensions)))
      (if existing
          (setcdr existing ext)
        (setq dorg--extensions
              (append dorg--extensions (list (cons name ext)))))
      ext)))

(defun dorg-unregister-extension (name)
  "Remove extension NAME from the registry.
Returns non-nil if an extension was removed."
  (let ((existing (assq name dorg--extensions)))
    (when existing
      (setq dorg--extensions (delq existing dorg--extensions))
      t)))

(defun dorg-get-extension (name)
  "Return the registered `dorg-extension' struct for NAME, or nil."
  (cdr (assq name dorg--extensions)))

(defun dorg-ext-query-field (name)
  "Return the registered descriptor for namespaced field NAME, or nil.
The copy includes :extension naming its owner; no second registry is used."
  (when (symbolp name)
    (let ((owner (car (split-string (symbol-name name) "/"))) result)
      (when-let ((extension (dorg-get-extension (intern owner))))
        (dolist (field (dorg-extension-fields extension))
          (when (eq name (plist-get field :name))
            (setq result (append (copy-tree field)
                                 (list :extension (dorg-extension-name extension)))))))
      result)))

(defun dorg-load-extension (name)
  "Load and register installed extension NAME without loading an application.
NAME is an ASCII extension name.  DOrg loads the conventional feature
`dorg-extension-NAME'; that small feature must register NAME and may depend
on its owning package, but DOrg never names or requires that package itself.
Returns the registered extension or signals a clear error when the feature
does not register the requested name."
  (unless (and (stringp name) (string-match-p dorg-ext--name-re name))
    (error "dorg-load-extension: invalid extension name %S" name))
  (let ((symbol (intern name)))
    (unless (dorg-get-extension symbol)
      (require (intern (format "dorg-extension-%s" name)))
      (unless (dorg-get-extension symbol)
        (error "dorg-load-extension: feature for %s did not register it" name)))
    (dorg-get-extension symbol)))

(defun dorg-ext-query-membership (extension relation)
  "Return EXTENSION's validated query descriptor for RELATION, or nil.
The returned plist is a fresh copy of the registered membership descriptor,
augmented with `:table' naming its containing read table.  Registration has
already validated every identifier, so the query planner may splice these
values into DOrg-owned SQL without accepting caller-authored identifiers."
  (let ((ext (dorg-get-extension extension))
        found)
    (when ext
      (dolist (table (dorg-extension-read-tables ext))
        (dolist (membership (plist-get table :memberships))
          (when (eq relation (plist-get membership :name))
            (setq found
                  (append (copy-sequence membership)
                          (list :table (plist-get table :name)))))))
      found)))

(defun dorg-registered-extensions ()
  "Return the list of currently-registered `dorg-extension' structs.
Returned in registration order."
  (mapcar #'cdr dorg--extensions))

;;; Mutation Participants

(defconst dorg-ext--default-capabilities '(dolt)
  "Default participant source kinds when an extension omits :capabilities.")

(defun dorg-ext--effective-capabilities (ext)
  "Return EXT's effective participant source kinds."
  (or (dorg-extension-capabilities ext) dorg-ext--default-capabilities))

(defun dorg-ext-index-projector-eligible-p (source-kind)
  "Return non-nil when some registered extension has an index projector eligible
for SOURCE-KIND — i.e. it declares a `:project' whose effective capabilities
include SOURCE-KIND.  DOrg uses this to fail closed on a backend that cannot run
derived-index projection (e.g. `plain-org'), so an eligible projector is never
silently dropped."
  (cl-some (lambda (ext)
             (and (dorg-extension-project ext)
                  (memq source-kind (dorg-ext--effective-capabilities ext))))
           (dorg-registered-extensions)))

(defun dorg-ext-index-projector-requires-whole-root-p
    (source-kind &optional installed-p)
  "Return non-nil when a SOURCE-KIND projector needs a complete root image.

This is an explicit registration property, not an inference from effect shape:
DOrg chooses the canonical simulation scope before invoking any projector, so
a root-aggregate projector cannot widen a bounded context after the fact.

When INSTALLED-P is non-nil, it receives each otherwise eligible extension and
must confirm that extension participates in the target store.  Registration is
process-global, but a store that never installed an extension must retain its
ordinary bounded mutation scope."
  (when (and installed-p (not (functionp installed-p)))
    (error "dorg-ext-index-projector-requires-whole-root-p: INSTALLED-P is not a function"))
  (cl-some (lambda (ext)
             (and (dorg-extension-project ext)
                  (eq (dorg-extension-project-scope ext) 'whole-root)
                  (memq source-kind (dorg-ext--effective-capabilities ext))
                  (or (null installed-p) (funcall installed-p ext))))
           (dorg-registered-extensions)))

(defvar dorg-ext--index-validation-phase nil
  "Dynamically names the current index-validator phase.
Private test/diagnostic value; validators must not branch domain behavior on it.")

(defun dorg-ext-run-index-validators
    (index-context &optional phase extension-names)
  "Run eligible index validators against INDEX-CONTEXT.
PHASE is bound to `dorg-ext--index-validation-phase' for diagnostics.  A
validator may accept or signal only; return values are ignored."
  (let ((kind (dorg-ext-index-context-source-kind index-context))
        (dorg-ext--index-validation-phase phase))
    (dolist (ext (dorg-registered-extensions))
      (when (and (dorg-ext--selected-index-extension-p ext extension-names)
                 (dorg-extension-validate ext)
                 (memq kind (dorg-ext--effective-capabilities ext)))
        (funcall (dorg-extension-validate ext) index-context))))
  nil)

(defun dorg-ext-read-capability (name source-kind)
  "Return `supported' if extension NAME can serve grouped reads for SOURCE-KIND.
Returns `unsupported' when NAME is not registered, declares no `:hydrate'
read participant, or does not declare SOURCE-KIND in its capabilities.  This
is the deterministic, fail-closed read-plane capability verdict."
  (let ((ext (dorg-get-extension name)))
    (if (and ext
             (dorg-extension-hydrate ext)
             (memq source-kind (dorg-ext--effective-capabilities ext)))
        'supported
      'unsupported)))

(defun dorg-ext-normalize-sql-or-nil (spec)
  "Return SPEC as a list of SQL strings, or nil when SPEC is nil.
Unlike `dorg-ext-normalize-sql', a nil SPEC is allowed (no projection)."
  (if (null spec) nil (dorg-ext-normalize-sql spec)))

;;; ─── Index projector: manifest / effect validation (tracked-regression) ─────
;; A registered index projector emits DERIVED index effects as TYPED ROW DELTAS
;; (not SQL); DOrg generates the SQL.  Before ANY effect is lowered into the core
;; Dolt transaction, DOrg confines it to the extension's OWN declared, unqualified,
;; identifier-safe index table — never a core table, a database-qualified/foreign
;; target, another extension's table, or a malformed/dual-op effect.
;; The projector cannot escape its manifest; validation is fail-closed and
;; pre-commit, so a rejected effect leaves both core and derived lanes unchanged.

(define-error 'dorg-ext-index-error
  "dorg derived index effect rejected" 'dorg-error)

(defconst dorg-ext-index-error-reasons
  '(not-an-effect no-table qualified bad-table bad-scope-column core-table unowned
    bad-op bad-column bad-value
    accumulator-shape accumulator-mixed accumulator-unbound negative-accumulator
    xor-width marker-declaration derived-index-inconsistent root-outside-context)
  "Closed set of `:reason' symbols a `dorg-ext-index-error' may carry.")

(defconst dorg-ext--index-identifier-re "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'"
  "A safe unqualified SQL identifier: DOrg splices table/column names raw, so a
name outside this closed grammar (delimiters, spaces, dots, `;') is rejected.")

(defun dorg-ext--index-identifier-safe-p (s)
  "Non-nil when string S is a safe unqualified SQL identifier."
  (and (stringp s) (string-match-p dorg-ext--index-identifier-re s)))

(defun dorg-ext--index-raise (reason message &rest data)
  "Signal `dorg-ext-index-error' with REASON, MESSAGE, and extra DATA plist.
Condition data is (MESSAGE PLIST) with PLIST beginning `:reason' REASON,
mirroring `dorg-mutate--raise' / `dorg-api--raise'."
  (cl-assert (memq reason dorg-ext-index-error-reasons))
  (signal 'dorg-ext-index-error
          (list message (apply #'list :reason reason data))))

(defun dorg-ext-index-error-reason (err)
  "Return the `:reason' symbol of a `dorg-ext-index-error' condition ERR."
  (plist-get (dorg-error-data err) :reason))

(defconst dorg-ext--index-core-tables
  '("headings" "heading_tags" "heading_items" "property_entries"
    "planning_items" "keyword_items" "drawer_items" "drawer_lines"
    "text_items" "dorg_meta" "dorg_files" "dorg_conflicts"
    "dorg_mutation_log")
  "Reserved core DOrg tables an extension index effect may NEVER target.
Owning a core table is forbidden even if an extension declares it, so the
materialized document lane cannot be written through the derived-index seam.")

;; ── Typed index effects (row deltas) ──────────────────────────────────
;; A projector returns a list of TYPED ROW DELTAS, never SQL text.  DOrg — not
;; the extension — generates the SQL, so a derived effect is PROVABLY a pure
;; function of the immutable candidate context: it cannot read another table,
;; join, subquery, or use a non-deterministic/session expression.  Each effect is
;; a plist naming exactly ONE owned target table and a single row-maintenance op:
;;   (:table NAME :insert ((COLUMN . VALUE) ...))  -- one row
;;   (:table NAME :delete ((COLUMN . VALUE) ...))  -- rows matching the equality
;;                                                    conjunction (nil = whole
;;                                                    owned table, e.g. a rebuild)
;; COLUMN is an identifier symbol; VALUE is a literal (string / number / nil).
;; An upsert or update is a `:delete' of the key followed by an `:insert'.

(defun dorg-ext--index-column-safe-p (col)
  "Non-nil when COL is a safe SQL identifier symbol for a derived column."
  (and (symbolp col) col (dorg-ext--index-identifier-safe-p (symbol-name col))))

(defun dorg-ext--index-literal-p (val)
  "Non-nil when VAL is a literal DOrg can emit as a bound SQL value."
  (or (null val) (stringp val) (numberp val)))

(defun dorg-ext--index-effect-op (effect)
  "Return (OP . PAYLOAD) for a typed index EFFECT, or nil if it has no op."
  (cond ((plist-member effect :insert) (cons 'insert (plist-get effect :insert)))
        ((plist-member effect :delete) (cons 'delete (plist-get effect :delete)))))

(defun dorg-ext--index-effect-keys (effect)
  "Return the ordered list of plist keys in EFFECT (every other element)."
  (let (keys)
    (while effect (push (car effect) keys) (setq effect (cddr effect)))
    (nreverse keys)))

(defun dorg-ext-validate-index-effect (effect owned &optional accumulate-specs)
  "Validate a typed index EFFECT against the extension's OWNED tables.
ACCUMULATE-SPECS, from `dorg-ext-accumulate-specs', names the extension's
accumulator tables: there an effect must be an accumulator (see
`dorg-ext--validate-accumulator-effect'), and an accumulator anywhere else is
rejected.  Without specs, an accumulator effect is rejected and DOrg-owned
folded or clearing effects validate as ordinary insert/delete.
Returns the touched table name on success; otherwise signals a typed
`dorg-ext-index-error' whose reason is one of `dorg-ext-index-error-reasons'
\(not-an-effect / no-table / qualified / core-table / unowned / bad-op /
bad-column / bad-value).  EFFECT is `(:table NAME :insert ROW)' or
`(:table NAME :delete WHERE)'.  It is confined to a single declared, unqualified,
owned (non-core) table with identifier-safe columns and literal values — so DOrg
can generate deterministic SQL that touches ONLY that table."
  (unless (and (listp effect) (plist-member effect :table))
    (dorg-ext--index-raise 'not-an-effect
                           "index effect is not a (:table … :insert/:delete …) plist"
                           :effect effect))
  (if (plist-member effect :accumulate)
      (dorg-ext--validate-accumulator-effect effect owned accumulate-specs)
    (prog1 (dorg-ext--validate-row-effect effect owned)
      (when (assoc (plist-get effect :table) accumulate-specs)
        (dorg-ext--index-raise
         'accumulator-mixed "an accumulator table takes only accumulator effects"
         :table (plist-get effect :table))))))

(defun dorg-ext--validate-row-effect (effect owned)
  "Validate an ordinary insert/delete EFFECT against OWNED; return its table."
  ;; CLOSED shape: only keys :table/:insert/:delete, no duplicates.
  (let ((keys (dorg-ext--index-effect-keys effect)))
    (when (or (cl-set-difference keys '(:table :insert :delete))
              (not (equal keys (delete-dups (copy-sequence keys)))))
      (dorg-ext--index-raise 'not-an-effect
                             "index effect has an unknown or duplicate key"
                             :effect effect :keys keys)))
  (let ((table (plist-get effect :table))
        (has-insert (plist-member effect :insert))
        (has-delete (plist-member effect :delete)))
    (unless (and (stringp table) (> (length table) 0))
      (dorg-ext--index-raise 'no-table "index effect names no target table"
                             :effect effect))
    (when (string-match-p "[.`]" table)
      (dorg-ext--index-raise 'qualified
                             "index effect target is database-qualified" :table table))
    ;; The table name is spliced RAW into DOrg-generated SQL, so it must be a safe
    ;; unqualified identifier (no delimiters/spaces/`;'); a manifest declaring an
    ;; unsafe name cannot smuggle a second statement or a foreign target.
    (unless (dorg-ext--index-identifier-safe-p table)
      (dorg-ext--index-raise 'bad-table
                             "index effect table name is not a safe identifier"
                             :table table))
    (when (cl-member table dorg-ext--index-core-tables
                     :test #'string-equal-ignore-case)
      (dorg-ext--index-raise 'core-table
                             "index effect targets a core DOrg table" :table table))
    (unless (cl-member table owned :test #'string-equal-ignore-case)
      (dorg-ext--index-raise 'unowned
                             "index effect targets an unowned table" :table table))
    ;; EXACTLY one op — never both, never neither (no silent op selection).
    (when (and has-insert has-delete)
      (dorg-ext--index-raise 'bad-op
                             "index effect declares both :insert and :delete"
                             :effect effect))
    (let* ((op (dorg-ext--index-effect-op effect)))
      (unless op
        (dorg-ext--index-raise 'bad-op
                               "index effect has no :insert or :delete op" :effect effect))
    (let ((payload (cdr op)))
      (unless (and (listp payload) (cl-every #'consp payload))
        (dorg-ext--index-raise 'bad-op
                               "index effect op payload is not a column alist"
                               :effect effect))
      (when (and (eq (car op) 'insert) (null payload))
        (dorg-ext--index-raise 'bad-op "index insert names no columns" :effect effect))
      (dolist (cell payload)
        (unless (dorg-ext--index-column-safe-p (car cell))
          (dorg-ext--index-raise 'bad-column
                                 "index effect column is not a safe identifier"
                                 :column (car cell)))
        (unless (dorg-ext--index-literal-p (cdr cell))
          (dorg-ext--index-raise 'bad-value
                                 "index effect value is not a literal"
                                 :column (car cell) :value (cdr cell)))))
    table)))

;; ── Accumulator effects (root aggregates) ────────────────────────────
;; An accumulator effect names one key of an `:accumulate' table and the
;; group-valued deltas to apply to it:
;;   (:table NAME :accumulate KEY-ALIST :add ((COL . INTEGER) ...)
;;    :xor ((COL . HEX64) ...) :initial ((COL . VALUE) ...) :prune-zero COL)
;; Integers under + and 256-bit digests under xor are commutative, associative
;; and invertible, so folding any sequence of deltas from the identity is
;; order-free: incremental maintenance equals a fresh rebuild.  Accumulators
;; never reach SQL.  DOrg folds them against prior rows it reads at the plan
;; base (`dorg-ext-fold-accumulators') into ordinary delete+insert effects.

(defconst dorg-ext--accumulator-keys
  '(:table :accumulate :add :xor :initial :prune-zero)
  "Closed keyword set of one accumulator effect.")

(defun dorg-ext-accumulate-specs (read-tables)
  "Return (NAME . KEY-COLUMNS) for each `:accumulate' table in READ-TABLES."
  (delq nil (mapcar (lambda (table)
                      (and (plist-get table :accumulate)
                           (cons (plist-get table :name) (plist-get table :accumulate))))
                    read-tables)))

(defun dorg-ext--hex64-p (value)
  "Non-nil when VALUE is a 64-character lowercase hexadecimal string."
  (and (stringp value) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(defun dorg-ext--validate-accumulator-effect (effect owned accumulate-specs)
  "Validate accumulator EFFECT against OWNED and ACCUMULATE-SPECS.
Return the touched table name."
  (let* ((keys (dorg-ext--index-effect-keys effect))
         (table (plist-get effect :table))
         (key (plist-get effect :accumulate))
         (spec (cdr (assoc table accumulate-specs)))
         (add (plist-get effect :add))
         (xor (plist-get effect :xor))
         (initial (plist-get effect :initial))
         (prune (plist-get effect :prune-zero)))
    (when (or (cl-set-difference keys dorg-ext--accumulator-keys)
              (not (equal keys (delete-dups (copy-sequence keys)))))
      (dorg-ext--index-raise 'not-an-effect
                             "accumulator effect has an unknown or duplicate key"
                             :effect effect :keys keys))
    ;; Table, ownership, key columns and literal key values: exactly the
    ;; ordinary checks, applied to the keyed row this accumulator addresses.
    (dorg-ext--validate-row-effect (list :table table :delete key) owned)
    (unless spec
      (dorg-ext--index-raise 'accumulator-mixed
                             "an ordinary table never takes an accumulator effect"
                             :table table))
    (dolist (payload (list add xor initial))
      (unless (and (proper-list-p payload) (cl-every #'consp payload)
                   (cl-every (lambda (cell) (dorg-ext--index-column-safe-p (car cell)))
                             payload))
        (dorg-ext--index-raise 'accumulator-shape
                               "accumulator payload is not a safe column alist"
                               :table table)))
    (let ((key-columns (mapcar #'car key))
          (counters (append (mapcar #'car add) (mapcar #'car xor)))
          (others (mapcar #'car initial)))
      (unless (and (= (length key-columns) (length spec))
                   (null (cl-set-difference key-columns spec))
                   (null (cl-set-difference spec key-columns)))
        (dorg-ext--index-raise 'accumulator-shape
                               "accumulator key is not the declared key"
                               :table table :key key-columns :declared spec))
      (unless (and counters
                   (equal counters (delete-dups (copy-sequence counters)))
                   (null (cl-intersection counters key-columns))
                   (null (cl-intersection others key-columns))
                   (null (cl-intersection others counters)))
        (dorg-ext--index-raise 'accumulator-shape
                               "accumulator columns overlap the key or each other"
                               :table table))
      (when (and prune (not (assq prune add)))
        (dorg-ext--index-raise 'accumulator-shape
                               "a prune column must be an :add column"
                               :table table :prune-zero prune)))
    (dolist (cell add)
      (unless (integerp (cdr cell))
        (dorg-ext--index-raise 'bad-value "accumulator :add value is not an integer"
                               :column (car cell) :value (cdr cell))))
    (dolist (cell xor)
      (unless (dorg-ext--hex64-p (cdr cell))
        (dorg-ext--index-raise 'xor-width
                               "accumulator :xor value is not 64 lowercase hex digits"
                               :column (car cell) :value (cdr cell))))
    (dolist (cell initial)
      (unless (dorg-ext--index-literal-p (cdr cell))
        (dorg-ext--index-raise 'bad-value "accumulator :initial value is not a literal"
                               :column (car cell) :value (cdr cell))))
    table))

(defun dorg-ext--accumulator-descriptor (read-tables table)
  "Return TABLE's descriptor from READ-TABLES."
  (cl-find table read-tables :key (lambda (d) (plist-get d :name)) :test #'string=))

(defun dorg-ext--accumulator-marker (read-tables)
  "Return the marker table descriptor of READ-TABLES, or nil."
  (cl-find-if (lambda (d) (plist-member d :marker)) read-tables))

(defun dorg-ext--effect-payload (effect)
  "Return the column alist that addresses EFFECT's row or rows."
  (or (plist-get effect :accumulate) (plist-get effect :insert) (plist-get effect :delete)))

(defun dorg-ext--effect-root (read-tables effect)
  "Return the root EFFECT addresses through its table's scope column.
An accumulator extension's every effect must name its root, so the root's
marker state can decide it; one that does not fails closed."
  (let* ((descriptor (dorg-ext--accumulator-descriptor read-tables (plist-get effect :table)))
         (scope (and descriptor (plist-get descriptor :scope-column)
                     (intern (plist-get descriptor :scope-column))))
         (cell (and scope (assq scope (dorg-ext--effect-payload effect)))))
    (unless (and cell (integerp (cdr cell)))
      (dorg-ext--index-raise 'accumulator-shape
                             "an accumulator extension's effect must name its root"
                             :table (plist-get effect :table)))
    (cdr cell)))

(defun dorg-ext--accumulator-canonical-key (key columns)
  "Return KEY's cells in declared COLUMNS order."
  (mapcar (lambda (column) (assq column key)) columns))

(defun dorg-ext--sort-keys (keys)
  "Return KEYS deduplicated and in a deterministic printed order."
  (sort (delete-dups (copy-sequence keys))
        (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b)))))

(defun dorg-ext--marker-required-keys (marker root)
  "Return the full required marker keys MARKER declares for ROOT."
  (let* ((columns (plist-get marker :accumulate))
         (scope (intern (plist-get marker :scope-column)))
         (required (plist-get marker :marker)))
    (mapcar (lambda (rest)
              (dorg-ext--accumulator-canonical-key (cons (cons scope root) rest) columns))
            (if (eq required t) '(nil) required))))

(defun dorg-ext-accumulator-prior-requests (read-tables effects)
  "Return the keyed prior reads folding EFFECTS needs, per READ-TABLES.
Each element is (TABLE KEY...) in declared table order.  An aggregate table
names exactly the keys its accumulators touch.  The marker table names each
touched root by its scope column alone, so every marker row of that root is
read and the root's completeness can be judged."
  (let ((marker (dorg-ext--accumulator-marker read-tables))
        (roots (delete-dups (mapcar (lambda (effect)
                                      (dorg-ext--effect-root read-tables effect))
                                    effects)))
        requests)
    (dolist (descriptor read-tables)
      (let* ((table (plist-get descriptor :name))
             (columns (plist-get descriptor :accumulate))
             (keys
              (cond
               ((eq descriptor marker)
                (let ((scope (intern (plist-get descriptor :scope-column))))
                  (mapcar (lambda (root) (list (cons scope root))) roots)))
               (columns
                (delq nil
                      (mapcar (lambda (effect)
                                (and (equal (plist-get effect :table) table)
                                     (plist-member effect :accumulate)
                                     (dorg-ext--accumulator-canonical-key
                                      (plist-get effect :accumulate) columns)))
                              effects))))))
        (when keys
          (push (cons table (dorg-ext--sort-keys keys)) requests))))
    (nreverse requests)))

(defun dorg-ext--row-matches-key-p (row key)
  "Non-nil when ROW holds every cell of KEY."
  (cl-every (lambda (cell) (equal (cdr (assq (car cell) row)) (cdr cell))) key))

(defun dorg-ext--prior-row (priors table key)
  "Return the prior row of TABLE whose key is KEY, or nil."
  (cdr (cl-find-if (lambda (prior)
                     (and (equal (car prior) table)
                          (dorg-ext--row-matches-key-p (cdr prior) key)))
                   priors)))

(defun dorg-ext--root-state (read-tables effects priors empty-roots root)
  "Return `fold' or `skip' for ROOT, or signal its inconsistent state.
Built (every required marker row present) folds.  Unbuilt (none present)
folds as a birth when ROOT had no headings at the base, which must write
every required marker row, and is otherwise skipped wholly.  Some but not
all required rows present is inconsistent and fails closed."
  (let* ((marker (dorg-ext--accumulator-marker read-tables))
         (table (plist-get marker :name))
         (required (dorg-ext--marker-required-keys marker root))
         (missing (cl-remove-if (lambda (key) (dorg-ext--prior-row priors table key))
                                required)))
    (cond
     ((null missing) 'fold)
     ((< (length missing) (length required))
      (dorg-ext--index-raise
       'derived-index-inconsistent
       (format (concat "derived index for root %d is partly built; "
                       "run dorg-ext-rebuild-index to repair it")
               root)
       :root root :missing missing))
     ((not (memq root empty-roots)) 'skip)
     (t
      (let ((born (delq nil
                        (mapcar (lambda (effect)
                                  (and (equal (plist-get effect :table) table)
                                       (plist-member effect :accumulate)
                                       (dorg-ext--accumulator-canonical-key
                                        (plist-get effect :accumulate)
                                        (plist-get marker :accumulate))))
                                effects))))
        (unless (cl-every (lambda (key) (member key born)) required)
          (dorg-ext--index-raise
           'accumulator-shape
           "a root's first index write must birth every required marker row"
           :root root :required required))
        'fold)))))

(defun dorg-ext--accumulator-number (value)
  "Return the stored integer VALUE, decoding a numeric string."
  (cond ((null value) 0)
        ((integerp value) value)
        ((stringp value) (string-to-number value))
        (t (dorg-ext--index-raise 'bad-value "stored accumulator is not an integer"
                                  :value value))))

(defun dorg-ext--accumulator-digest (value)
  "Return stored 256-bit digest VALUE as an integer; absent is zero."
  (cond ((null value) 0)
        ((dorg-ext--hex64-p value) (string-to-number value 16))
        (t (dorg-ext--index-raise 'xor-width "stored accumulator digest is not 64 hex"
                                  :value value))))

(defun dorg-ext--merge-accumulators (effects columns)
  "Merge accumulator EFFECTS on one key into one (KEY ADD XOR INITIAL PRUNE)."
  (let (add xor initial prune)
    (dolist (effect effects)
      (dolist (cell (plist-get effect :add))
        (setf (alist-get (car cell) add) (+ (alist-get (car cell) add 0) (cdr cell))))
      (dolist (cell (plist-get effect :xor))
        (setf (alist-get (car cell) xor)
              (logxor (alist-get (car cell) xor 0) (string-to-number (cdr cell) 16))))
      (dolist (cell (plist-get effect :initial))
        (unless (assq (car cell) initial) (push cell initial)))
      (setq prune (or prune (plist-get effect :prune-zero))))
    (list (dorg-ext--accumulator-canonical-key
           (plist-get (car effects) :accumulate) columns)
          (nreverse add) (nreverse xor) (nreverse initial) prune)))

(defun dorg-ext--fold-one (table merged prior)
  "Return the literal effects folding MERGED into PRIOR for TABLE."
  (pcase-let ((`(,key ,add ,xor ,initial ,prune) merged))
    (let* ((counters (append (mapcar #'car add) (mapcar #'car xor)))
           (post-counters
            (append
             (mapcar (lambda (cell)
                       (let ((value (+ (dorg-ext--accumulator-number
                                        (cdr (assq (car cell) prior)))
                                       (cdr cell))))
                         (when (< value 0)
                           (dorg-ext--index-raise
                            'negative-accumulator "an accumulator folds below zero"
                            :table table :key key :column (car cell) :value value))
                         (cons (car cell) value)))
                     add)
             (mapcar (lambda (cell)
                       (cons (car cell)
                             (format "%064x"
                                     (logxor (dorg-ext--accumulator-digest
                                              (cdr (assq (car cell) prior)))
                                             (cdr cell)))))
                     xor)))
           (others (cl-remove-if (lambda (cell)
                                   (or (assq (car cell) key) (memq (car cell) counters)))
                                 (or prior initial))))
      (cond
       ((and prune (eql 0 (cdr (assq prune post-counters))))
        (and prior (list (list :table table :delete key))))
       ((and prior
             (cl-every (lambda (cell)
                         (equal (cdr cell)
                                (let ((stored (cdr (assq (car cell) prior))))
                                  (if (assq (car cell) add)
                                      (dorg-ext--accumulator-number stored)
                                    stored))))
                       post-counters))
        nil)
       (t
        ;; A key with no stored row is born: there is nothing to delete.
        (append (and prior (list (list :table table :delete key)))
                (list (list :table table
                            :insert (append key
                                            (sort (copy-sequence others)
                                                  (lambda (a b) (string< (car a) (car b))))
                                            (sort (copy-sequence post-counters)
                                                  (lambda (a b) (string< (car a) (car b)))))))))))))

(defun dorg-ext-fold-accumulators (read-tables effects priors empty-roots)
  "Fold accumulator EFFECTS into literal insert/delete effects.
READ-TABLES are the extension's declarations.  PRIORS lists (TABLE . ROW)
for the rows `dorg-ext-accumulator-prior-requests' named, read at the plan
base.  EMPTY-ROOTS lists the touched roots that had no heading at the base.
Each touched root is judged by its marker rows (`dorg-ext--root-state'): a
skipped root receives no effect at all.  The result is the ordinary effects
in projector order, then per key, in declared table and printed key order,
nothing when the row is unchanged, a delete when a prune column reaches
zero, or a delete followed by an insert of the complete literal post row.
Without accumulator tables, EFFECTS are returned unchanged."
  (if (not (dorg-ext-accumulate-specs read-tables))
      effects
    (let* ((roots (delete-dups (mapcar (lambda (effect)
                                         (dorg-ext--effect-root read-tables effect))
                                       effects)))
           (skipped (cl-remove-if-not
                     (lambda (root)
                       (eq 'skip (dorg-ext--root-state
                                  read-tables effects priors empty-roots root)))
                     roots))
           (kept (cl-remove-if (lambda (effect)
                                 (memq (dorg-ext--effect-root read-tables effect) skipped))
                               effects))
           (ordinary (cl-remove-if (lambda (effect) (plist-member effect :accumulate)) kept))
           folded)
      (dolist (descriptor read-tables)
        (let* ((table (plist-get descriptor :name))
               (columns (plist-get descriptor :accumulate))
               (groups nil))
          (when columns
            (dolist (effect kept)
              (when (and (equal (plist-get effect :table) table)
                         (plist-member effect :accumulate))
                (let ((key (dorg-ext--accumulator-canonical-key
                            (plist-get effect :accumulate) columns)))
                  (push effect (alist-get key groups nil nil #'equal)))))
            (dolist (key (dorg-ext--sort-keys (mapcar #'car groups)))
              (setq folded
                    (nconc folded
                           (dorg-ext--fold-one
                            table
                            (dorg-ext--merge-accumulators
                             (reverse (alist-get key groups nil nil #'equal)) columns)
                            (dorg-ext--prior-row priors table key))))))))
      (append ordinary folded))))

(defun dorg-ext--index-quote (val)
  "Return the SQL literal for VAL (string via `dorg-sql-quote', number, or NULL)."
  (dorg-sql--literal-sql val))

(defun dorg-ext-index-effect-predicate-sql (payload)
  "Return the SQL predicate matching column alist PAYLOAD, or nil when empty.
A nil cell value lowers as `IS NULL' (SQL `= NULL' never matches), so a
NULL-keyed derived row is actually addressable.  A nil PAYLOAD (no cells) is
the distinct whole-table predicate and returns nil.  This delegates to the
single shared quoting-layer lowerer `dorg-sql--effect-predicate-sql'; the
delete statement generator and the postimage validator's transaction read port
\(tracked-regression) both consume that lowerer, so the committed predicate and the
verified predicate cannot drift."
  (dorg-sql--effect-predicate-sql payload))

(defun dorg-ext--index-effect-item (effect)
  "Return a VALIDATED typed index EFFECT as one lowering item.
An insert is a typed `dorg-sql-row'; a delete is its single DELETE statement.
DOrg owns generation: either form writes only the effect's single owned table
from literal, candidate-derived values -- never a read, join, or subquery.
Values stay in the effect literal domain (nil, string or number)."
  (let* ((table (plist-get effect :table))
         (op (dorg-ext--index-effect-op effect))
         (payload (cdr op)))
    (pcase (car op)
      ('insert
       (dolist (cell payload)
         (unless (dorg-ext--index-literal-p (cdr cell))
           (dorg-ext--index-raise 'bad-value "index effect value is not a literal"
                                  :column (car cell) :value (cdr cell))))
       (dorg-sql-row-make table (mapcar #'car payload) (mapcar #'cdr payload)))
      ('delete
       (let ((pred (dorg-ext-index-effect-predicate-sql payload)))
         (if pred
             (format "DELETE FROM %s WHERE %s" table pred)
           (format "DELETE FROM %s" table)))))))

(defun dorg-ext--index-effect-sql (effect)
  "Return the DOrg-generated SQL for a VALIDATED typed index EFFECT.
DOrg owns generation: the statement writes only the effect's single owned table
from literal, candidate-derived values -- never a read, join, or subquery."
  (let ((item (dorg-ext--index-effect-item effect)))
    (if (stringp item) item (dorg-sql-row-sql item))))

(defun dorg-ext--index-effects-sql (effects)
  "Return the lowered SQL for one extension's validated EFFECTS, in order.
Adjacent insert effects of one table and column list merge into multi-row
INSERTs; every delete stays a single statement in place.  DOrg knows no
extension foreign keys, so it never reorders an extension's effects."
  (dorg-sql-lower-statements (mapcar #'dorg-ext--index-effect-item effects)))

(defun dorg-ext--owned-tables (ext)
  "Return the list of table names EXT declares it owns via `:read-tables'."
  (delq nil (mapcar (lambda (d) (and (listp d) (plist-get d :name)))
                    (dorg-extension-read-tables ext))))

(defun dorg-ext-rebuild-scope-tables (ext)
  "Return EXT's validated rebuild table descriptors.
Every rebuildable table must declare a safe unqualified `:scope-column'.  The
column is spliced into DOrg-owned scoped DELETE SQL, so an absent, qualified, or
otherwise unsafe value is a typed refusal before any transaction opens.  The
generated scoped DELETE is also sent through `dorg-ext-validate-index-effect',
which rejects core, foreign, and malformed manifest targets by the shared guard."
  (let ((descriptors (dorg-extension-read-tables ext)))
    (unless descriptors
      (dorg-ext--index-raise
       'bad-scope-column
       "rebuild extension must declare at least one scoped table"
       :extension (dorg-extension-name ext)))
    (mapcar
     (lambda (descriptor)
       (let* ((table (plist-get descriptor :name))
              (scope-column (plist-get descriptor :scope-column))
              (scope-parent (plist-get descriptor :scope-parent))
              (parent-table (plist-get scope-parent :table))
              (parent-key (plist-get scope-parent :key-column))
              (parent (and scope-parent
                           (cl-find parent-table descriptors
                                    :key (lambda (d) (plist-get d :name))
                                    :test #'string=))))
         (unless (dorg-ext--index-identifier-safe-p scope-column)
           (dorg-ext--index-raise
            'bad-scope-column
            "rebuild table needs a safe unqualified :scope-column"
            :extension (dorg-extension-name ext)
            :table table :scope-column scope-column))
         (when scope-parent
           (unless (and parent
                        (not (string= table parent-table))
                        (not (plist-get parent :scope-parent))
                        (dorg-ext--index-identifier-safe-p parent-key)
                        (dorg-ext--index-identifier-safe-p
                         (plist-get parent :scope-column)))
             (dorg-ext--index-raise
              'bad-scope-column
              "rebuild scope-parent must name an owned directly scoped table"
              :extension (dorg-extension-name ext) :table table
              :scope-parent scope-parent)))
         ;; The generated direct-shaped predicate is deliberately validated even
         ;; for child tables; the rebuild compiler expands it per captured parent
         ;; key before SQL lowering.
         (dorg-ext-validate-index-effect
          (list :table table :delete (list (cons (intern scope-column) 0)))
          (dorg-ext--owned-tables ext))
         (list :name table :scope-column scope-column :scope-parent scope-parent
               :key-column (plist-get descriptor :key-column))))
     descriptors)))

(defun dorg-ext-rebuild--validate-effects (effects owned)
  "Return EFFECTS after applying the shared index-effect guard to each one.
This covers both DOrg-generated scoped clears and narrowed rebuild effects
before any readback or transaction can observe them."
  (dolist (effect effects)
    (dorg-ext-validate-index-effect effect owned))
  effects)

(defun dorg-ext--selected-index-extension-p (ext extension-names)
  "Return non-nil when EXT is selected by EXTENSION-NAMES.
The explicit symbol `:all' selects every registered extension.  Nil selects
none, so a store with no installed projectors never falls back to every
process-registered projector."
  (or (eq extension-names :all)
      (memq (dorg-extension-name ext) extension-names)))

(defun dorg-ext--index-context-roots (index-context)
  "Return the roots INDEX-CONTEXT establishes: its root, source and destination."
  (delete-dups (delq nil (list (dorg-ext-index-context-root-id index-context)
                               (dorg-ext-index-context-from-root-id index-context)
                               (dorg-ext-index-context-to-root-id index-context)))))

(defun dorg-ext--fold-through-port (name index-context read-tables effects
                                         fold-port)
  "Fold extension NAME's accumulator EFFECTS per READ-TABLES through FOLD-PORT.
Every effect, accumulator or ordinary row, must address a root INDEX-CONTEXT
establishes.  The check precedes every port read, so a projector cannot make
DOrg read or write another root's index."
  (unless (and (functionp (plist-get fold-port :priors))
               (functionp (plist-get fold-port :empty-roots)))
    (dorg-ext--index-raise
     'accumulator-unbound
     "this caller cannot bind accumulator effects to a commit base"
     :extension name))
  (let ((context-roots (dorg-ext--index-context-roots index-context))
        (roots (delete-dups (mapcar (lambda (effect)
                                      (dorg-ext--effect-root read-tables effect))
                                    effects))))
    (dolist (root roots)
      (unless (memql root context-roots)
        (dorg-ext--index-raise
         'root-outside-context
         "an accumulator extension's effect addresses a root outside its context"
         :extension name :root root :context-roots context-roots)))
    (dorg-ext-fold-accumulators
     read-tables effects
     (funcall (plist-get fold-port :priors)
              (dorg-ext-accumulator-prior-requests read-tables effects))
     (funcall (plist-get fold-port :empty-roots) roots))))

(defun dorg-ext-collect-index-projection (index-context &optional extension-names
                                                        fold-port)
  "Run selected DOrg-owned index projectors against INDEX-CONTEXT.
INDEX-CONTEXT is a `dorg-ext-index-context' (tracked-regression) — the canonical
candidate-state context DOrg produces after core simulation.  This is the SINGLE
projector path: it accepts NO caller payload, consumes NO implicit state, and runs
purely as a function of the DOrg-owned context.

EXTENSION-NAMES is an explicit list of registered names; nil selects none and
the symbol `:all' selects every eligible registered extension.

For each registered extension (in registration order) whose capabilities include
the context's source kind and which declares a `:project' index projector, DOrg
runs optional `:validate' (a signal aborts the whole mutation before commit), then
`:project' with the SAME context to collect a list of TYPED ROW DELTAS.  EVERY
effect is validated against the extension's declared `:read-tables' manifest via
`dorg-ext-validate-index-effect' (owned, unqualified, non-core table; safe
columns; literal values); a rejected effect signals `dorg-ext-index-error' and
aborts fail-closed before any persistence.  DOrg — not the extension — GENERATES
the SQL (`dorg-ext--index-effects-sql'), so no derived effect can read database
state or reach a table other than its own.

Returns a plist:
  :sql        flat list of DOrg-generated SQL strings, appended AFTER core SQL in
              the SAME Dolt transaction (one commit, atomic rollback).  Each
              extension's adjacent same-shape inserts are merged into
              multi-row INSERTs, in effect order;
  :effects    flat list of the VALIDATED typed row effects in execution order —
              the exact effect forms the `:sql' statements were generated from.
              The postimage validator (tracked-regression) compares in-transaction
              readback against THESE plan-time values, so retaining them here
              keeps the compared expectation the already-validated one rather
              than a recomputation;
  :extensions alist (NAME . (:statements N)) of participants that ran, N
              being the participant's post-fold effect count;
  :ext-dirty  alist (NAME . (:tables TABLES)) — the DOrg-owned dirty scope, i.e.
              the owned tables the validated effects actually touched.  This
              REPLACES the retired per-extension `:dirty-scope' callback: dirty
              scope is now derived deterministically from the one projector
              result, never supplied by a second participant.

FOLD-PORT binds accumulator effects to a commit base.  It is a plist
\(:priors FN :empty-roots FN): FN of `dorg-ext-accumulator-prior-requests'
requests returns (TABLE . ROW) priors read AS OF the caller's plan base, and
FN of a root list returns the roots that had no heading at that base.  An
extension declaring `:accumulate' tables is folded through it with
`dorg-ext-fold-accumulators' before SQL is generated, and the folded effects
are revalidated as ordinary row effects.  Before any port read, every effect
of such an extension must address a root the context establishes (its root,
source or destination), else `root-outside-context'.  A caller that passes no FOLD-PORT
cannot bind accumulators to a base, so any effect from such an extension
fails closed (`accumulator-unbound')."
  (let ((kind (dorg-ext-index-context-source-kind index-context))
        (sql nil) (all-effects nil) (extensions nil) (ext-dirty nil))
    (dorg-ext-run-index-validators index-context 'projection extension-names)
    (dolist (ext (dorg-registered-extensions))
      (let ((name (dorg-extension-name ext))
            (project (dorg-extension-project ext)))
        (when (and (dorg-ext--selected-index-extension-p ext extension-names)
                   project (memq kind (dorg-ext--effective-capabilities ext)))
          (let* ((owned (dorg-ext--owned-tables ext))
                 (read-tables (dorg-extension-read-tables ext))
                 (specs (dorg-ext-accumulate-specs read-tables))
                 (effects (funcall project index-context))
                 (_validated (mapc (lambda (e) (dorg-ext-validate-index-effect e owned specs))
                                   effects))
                 (effects (if (and specs effects)
                              (dorg-ext--fold-through-port
                               name index-context read-tables effects fold-port)
                            effects))
                 (tables (mapcar (lambda (e) (dorg-ext-validate-index-effect e owned))
                                 effects))
                 (stmts (dorg-ext--index-effects-sql effects)))
            (setq sql (nconc sql stmts))
            (setq all-effects (nconc all-effects (copy-sequence effects)))
            ;; The summary counts the post-fold effects, not the lowered
            ;; statements, so receipts do not depend on how rows merge.
            (push (cons name (list :statements (length effects))) extensions)
            (when tables
              (push (cons name (list :tables (delete-dups (copy-sequence tables))))
                    ext-dirty))))))
    (list :sql sql
          :effects all-effects
          :extensions (nreverse extensions)
          :ext-dirty (nreverse ext-dirty))))

;;; Placement Participants
;;
;; Generic DOrg stores a heading's `parent_id' and its parent-relative
;; `level_offset', with NO invariant tying a top-level heading (offset 1
;; under the root, effective level 1) to a genuine top-level identity.  The buffer star-count-authoritative write-back
;; (`dorg-parse-assign-ids') and the render path (`dorg-model-render-tree')
;; therefore cannot themselves tell a legitimate top-level heading — or an
;; intentional move-to-root — from a child section accidentally demoted to
;; column 1 and silently promoted to `parent_id=root'.  Only an extension
;; that OWNS the heading can classify that, from ITS class model.  The
;; placement participant is that classifier; with none registered the seam
;; is inert and behavior is unchanged.

(cl-defstruct dorg-ext-placement-context
  "Read-only context DOrg passes to a placement invariant.
All fields are owned and populated by DOrg; the participant consumes them
and returns a `dorg-ext-placement-verdict' or nil.

HEADING-ID and DORG-ID identify the heading (DORG-ID is its stable
`:DORG_ID:').  ROOT-ID is the tree's level-0 root id.  STORED-PARENT-ID is
the heading's parent as recorded in the backing store, and STORED-LEVEL its
EFFECTIVE absolute Org level there (nil when the heading is new or the level
is unknown).  The store persists only a parent-relative offset; a producer
must derive STORED-LEVEL from ancestor-closed data, never pass an offset.
BUFFER-PARENT-ID and BUFFER-LEVEL are the parent and effective level the
current buffer/render placement would record.  MOVE-INTENT is
non-nil when the mutation path carried an explicit reparent patch (mutate
DSL), signalling an intentional move rather than an accidental demotion.
SNAPSHOT is a domain-neutral alist of the heading's PROPERTIES (KEY . VALUE)
so the participant classifies by its model, not by star shape alone.
SOURCE-KIND is the mutation/read source kind (e.g. `dolt'), or nil."
  heading-id dorg-id root-id
  stored-parent-id stored-level
  buffer-parent-id buffer-level
  move-intent snapshot source-kind)

(cl-defstruct dorg-ext-placement-verdict
  "A placement invariant's classification of one heading.
CLASSIFICATION is a domain symbol naming the class (e.g. `promoted-orphan').
ACTION is one of `quarantine' (hold the promotion, preserve content, emit a
diagnostic), `allow' (a legitimate placement — no false positive), or
`re-resolve' (the extension will re-resolve the true parent).  REASON is a
human-readable explanation for the diagnostic."
  classification action reason)

(cl-defstruct dorg-placement-diagnostic
  "A classified placement diagnostic surfaced on a `quarantine' verdict.
Distinct from the generic `dorg-render--warn-unreachable' warning: the
promoted orphan IS root-reachable, so only the owning invariant can flag it.
Carries the CLASSIFICATION, the heading identity, and the stored-vs-buffer
lineage that diverged, plus the invariant's REASON."
  classification heading-id dorg-id
  stored-parent-id stored-level
  buffer-parent-id buffer-level
  reason)

(defun dorg-ext-placement-capability (name source-kind)
  "Return `supported' if extension NAME can classify placement for SOURCE-KIND.
Returns `unsupported' when NAME is not registered, declares no
`:placement-invariant', or declares capabilities excluding SOURCE-KIND.  A
nil SOURCE-KIND is always accepted (the classifier may be consulted outside
a specific mutation/read source, e.g. from the render guard)."
  (let ((ext (dorg-get-extension name)))
    (if (and ext
             (dorg-extension-placement-invariant ext)
             (or (null source-kind)
                 (memq source-kind (dorg-ext--effective-capabilities ext))))
        'supported
      'unsupported)))

(defun dorg-ext-classify-placement (context)
  "Classify CONTEXT by consulting registered placement invariants.
CONTEXT is a `dorg-ext-placement-context'.  Each registered extension (in
registration order) whose capabilities include the context's source kind
and which declares a `:placement-invariant' is called; the FIRST non-nil
`dorg-ext-placement-verdict' returned wins.  Returns that verdict, or nil
when no invariant has an opinion (the extension-neutral default: generic
DOrg holds no opinion on any placement)."
  (let ((kind (dorg-ext-placement-context-source-kind context)))
    (cl-dolist (ext (dorg-registered-extensions) nil)
      (let ((inv (dorg-extension-placement-invariant ext)))
        (when (and inv
                   (eq 'supported
                       (dorg-ext-placement-capability
                        (dorg-extension-name ext) kind)))
          (when-let ((verdict (funcall inv context)))
            (cl-return verdict)))))))

(defun dorg-placement-diagnostic-emit (diagnostic)
  "Surface DIAGNOSTIC to the operator via a classified `display-warning'.
Distinct from the generic unreachable-heading warning so a promoted-orphan
quarantine is legible as such.  Callers (write-back seam / render guard)
route every quarantine through here; tests rebind it to capture.  Returns
DIAGNOSTIC."
  (display-warning
   'dorg
   (format (concat "Placement quarantine [%s]: heading %s held at stored"
                   " parent %s/level %s (buffer placement would promote to"
                   " parent %s/level %s): %s")
           (dorg-placement-diagnostic-classification diagnostic)
           (dorg-placement-diagnostic-heading-id diagnostic)
           (dorg-placement-diagnostic-stored-parent-id diagnostic)
           (dorg-placement-diagnostic-stored-level diagnostic)
           (dorg-placement-diagnostic-buffer-parent-id diagnostic)
           (dorg-placement-diagnostic-buffer-level diagnostic)
           (dorg-placement-diagnostic-reason diagnostic))
   :warning)
  diagnostic)

(provide 'dorg-ext)
;;; dorg-ext.el ends here
