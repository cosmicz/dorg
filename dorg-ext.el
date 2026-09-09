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
;; Extensions are applied after the core schema in `dorg-init', and
;; their version is tracked in `dorg_meta' as `ext:<name>:version'.
;;
;; Schema evolution is a first-class migration contract, not silent
;; drift.  Each `:migrations' entry is keyed by a from-version integer
;; and migrates exactly N -> N+1.  A step is SQL (string or list of
;; strings) or a function of one argument (the dolt-dir) returning SQL;
;; function steps may inspect the repo but must not commit or touch
;; `dorg_meta' — dorg owns the version bump.  Dorg never infers schema
;; diffs and never downgrades; a missing migration step is an actionable
;; error, with manual SQL available only as an operator escape hatch (see
;; `dorg-repo-ensure-extensions').

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
extension's participants support; nil defaults to `(dolt)'.
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
leaving generic DOrg with no opinion on any level-1 heading."
  name schema version migrations
  capabilities project validate
  read-tables hydrate
  placement-invariant)

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
Lineage is parent id, sibling sort key, and level — the placement facts that
distinguish a move from an in-place content change."
  (not (and (eql (dorg-heading-parent-id before) (dorg-heading-parent-id after))
            (equal (dorg-heading-sort-key before) (dorg-heading-sort-key after))
            (eql (dorg-heading-level before) (dorg-heading-level after)))))

(defun dorg-ext--index-classify (orig ps-headings created changed deleted)
  "Partition candidate images into create/change/move/delete index entries.
ORIG and PS-HEADINGS are S0 and candidate id -> `dorg-heading' hashes.
CREATED/CHANGED/DELETED are touched-id accumulators.  Net membership decides the
disjoint partition: a heading present only in the candidate is a create, one
present only in S0 is a delete, and one present in both and touched is a move or
change.  Returns (:created ES :changed ES :moved ES :deleted ES), each sorted by
id."
  (let ((touched (make-hash-table :test 'eql))
        (creates nil) (changes nil) (moves nil) (deletes nil)
        (by-id (lambda (a b) (< (dorg-ext-index-entry-id a)
                                (dorg-ext-index-entry-id b)))))
    (dolist (id (append created changed deleted)) (puthash id t touched))
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
             (if (dorg-ext--index-lineage-changed-p before after)
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
  "Return a COMPLETE canonical tuple of `dorg-heading' H's persisted state."
  (list (dorg-heading-id h)
        (dorg-heading-parent-id h)
        (dorg-heading-sort-key h)
        (dorg-heading-level h)
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

SCOPE: ordinary mutation and import images span exactly the transaction's BOUNDED
WORKING SCOPE — the same id slice the core simulation loaded (root row, direct
targets, anchors, current parents, and affected-parent sibling fans), not the
whole root.  This is the canonical view an index projector derives from: every
heading the mutation could have read or written is present, so
removals/additions within scope are complete; headings outside the loaded scope
are unchanged and are not materialized.  `dorg-ext-rebuild-index' is a distinct
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
and sort-key strings that `copy-tree' would share."
  (cond
   ((consp value)
    (cons (dorg-ext--deep-copy (car value)) (dorg-ext--deep-copy (cdr value))))
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (let ((v (copy-sequence value)))
      (dotimes (i (length v)) (aset v i (dorg-ext--deep-copy (aref v i))))
      v))
   (t value)))

(cl-defun dorg-ext-index-context-create
    (&key source-kind root-id from-root-id to-root-id dolt-dir file
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
  '(:schema :version :migrations :capabilities :project :validate
    :read-tables :hydrate :placement-invariant)
  "Accepted keyword arguments for `dorg-register-extension'.")

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
  :validate     Pure fn of a `dorg-ext-index-context'; signalling aborts the
                whole transaction before commit (optional)
  :read-tables  Read-plane manifest: list of generic projection-table
                descriptor plists (optional; see `dorg-extension')
  :hydrate      Read participant: pure fn of a `dorg-ext-read-context'
                returning a grouped query PLAN (optional; see
                `dorg-ext-hydrate')
  :placement-invariant  Placement participant: pure fn of a
                `dorg-ext-placement-context' returning a
                `dorg-ext-placement-verdict' or nil (optional; see
                `dorg-ext-classify-placement')

Each migration step migrates exactly FROM-VERSION -> FROM-VERSION+1.
STEP is SQL (string or list of strings) or a function of one argument
\(the dolt-dir) returning SQL; function steps may inspect the repo but
must not commit or update `dorg_meta'.  See `dorg-repo-ensure-extensions'
for the install/upgrade contract.

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
        (validate (plist-get args :validate))
        (read-tables (plist-get args :read-tables))
        (hydrate (plist-get args :hydrate))
        (placement-invariant (plist-get args :placement-invariant)))
    (unless (dorg-ext--sql-spec-p schema)
      (error "dorg-register-extension: :schema must be a SQL string or list of SQL strings"))
    (unless (and (integerp version) (> version 0))
      (error "dorg-register-extension: :version must be a positive integer"))
    (dorg-ext--validate-migrations migrations)
    (unless (or (null capabilities)
                (and (listp capabilities) (cl-every #'symbolp capabilities)))
      (error "dorg-register-extension: :capabilities must be a list of source-kind symbols"))
    (dolist (cb (list (cons :project project)
                      (cons :validate validate)
                      (cons :hydrate hydrate)
                      (cons :placement-invariant placement-invariant)))
      (unless (or (null (cdr cb)) (functionp (cdr cb)))
        (error "dorg-register-extension: %s must be a function, got %S"
               (car cb) (cdr cb))))
    (unless (or (null read-tables)
                (and (listp read-tables) (cl-every #'listp read-tables)))
      (error "dorg-register-extension: :read-tables must be a list of table descriptor plists"))
    ;; A manifest table name is spliced raw into DOrg-generated index SQL, so it
    ;; must be a safe unqualified identifier — an unsafe name never registers.
    (dolist (d read-tables)
      (let ((n (plist-get d :name)))
        (unless (dorg-ext--index-identifier-safe-p n)
          (error "dorg-register-extension: :read-tables name %S is not a safe identifier"
                 n))))
    (let ((ext (make-dorg-extension :name name
                                    :schema schema
                                    :version version
                                    :migrations migrations
                                    :capabilities capabilities
                                    :project project
                                    :validate validate
                                    :read-tables read-tables
                                    :hydrate hydrate
                                    :placement-invariant placement-invariant))
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
    bad-op bad-column bad-value)
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

(defun dorg-ext-validate-index-effect (effect owned)
  "Validate a typed index EFFECT against the extension's OWNED tables.
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

(defun dorg-ext--index-effect-sql (effect)
  "Return the DOrg-generated SQL for a VALIDATED typed index EFFECT.
DOrg owns generation: the statement writes only the effect's single owned table
from literal, candidate-derived values — never a read, join, or subquery."
  (let* ((table (plist-get effect :table))
         (op (dorg-ext--index-effect-op effect))
         (payload (cdr op)))
    (pcase (car op)
      ('insert
       (format "INSERT INTO %s (%s) VALUES (%s)"
               table
               (mapconcat (lambda (c) (symbol-name (car c))) payload ", ")
               (mapconcat (lambda (c) (dorg-ext--index-quote (cdr c))) payload ", ")))
      ('delete
       (let ((pred (dorg-ext-index-effect-predicate-sql payload)))
         (if pred
             (format "DELETE FROM %s WHERE %s" table pred)
           (format "DELETE FROM %s" table)))))))

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
       (let ((table (plist-get descriptor :name))
             (scope-column (plist-get descriptor :scope-column)))
         (unless (dorg-ext--index-identifier-safe-p scope-column)
           (dorg-ext--index-raise
            'bad-scope-column
            "rebuild table needs a safe unqualified :scope-column"
            :extension (dorg-extension-name ext)
            :table table :scope-column scope-column))
         ;; The table itself and this DOrg-owned generated DELETE must pass the
         ;; same ownership/core-table confinement as projector effects.
         (dorg-ext-validate-index-effect
          (list :table table :delete
                (list (cons (intern scope-column) 0)))
          (dorg-ext--owned-tables ext))
         (list :name table :scope-column scope-column
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
  "Return non-nil when EXT is selected by optional EXTENSION-NAMES."
  (or (null extension-names)
      (memq (dorg-extension-name ext) extension-names)))

(defun dorg-ext-collect-index-projection (index-context &optional extension-names)
  "Run registered DOrg-owned index projectors against INDEX-CONTEXT.
INDEX-CONTEXT is a `dorg-ext-index-context' (tracked-regression) — the canonical
candidate-state context DOrg produces after core simulation.  This is the SINGLE
projector path: it accepts NO caller payload, consumes NO implicit state, and runs
purely as a function of the DOrg-owned context.

For each registered extension (in registration order) whose capabilities include
the context's source kind and which declares a `:project' index projector, DOrg
runs optional `:validate' (a signal aborts the whole mutation before commit), then
`:project' with the SAME context to collect a list of TYPED ROW DELTAS.  EVERY
effect is validated against the extension's declared `:read-tables' manifest via
`dorg-ext-validate-index-effect' (owned, unqualified, non-core table; safe
columns; literal values); a rejected effect signals `dorg-ext-index-error' and
aborts fail-closed before any persistence.  DOrg — not the extension — GENERATES
the SQL (`dorg-ext--index-effect-sql'), so no derived effect can read database
state or reach a table other than its own.

Returns a plist:
  :sql        flat list of DOrg-generated SQL strings, appended AFTER core SQL in
              the SAME Dolt transaction (one commit, atomic rollback);
  :effects    flat list of the VALIDATED typed row effects in execution order —
              the exact effect forms the `:sql' statements were generated from.
              The postimage validator (tracked-regression) compares in-transaction
              readback against THESE plan-time values, so retaining them here
              keeps the compared expectation the already-validated one rather
              than a recomputation;
  :extensions alist (NAME . (:statements N)) of participants that ran;
  :ext-dirty  alist (NAME . (:tables TABLES)) — the DOrg-owned dirty scope, i.e.
              the owned tables the validated effects actually touched.  This
              REPLACES the retired per-extension `:dirty-scope' callback: dirty
              scope is now derived deterministically from the one projector
              result, never supplied by a second participant."
  (let ((kind (dorg-ext-index-context-source-kind index-context))
        (sql nil) (all-effects nil) (extensions nil) (ext-dirty nil))
    (dorg-ext-run-index-validators index-context 'projection extension-names)
    (dolist (ext (dorg-registered-extensions))
      (let ((name (dorg-extension-name ext))
            (project (dorg-extension-project ext)))
        (when (and (dorg-ext--selected-index-extension-p ext extension-names)
                   project (memq kind (dorg-ext--effective-capabilities ext)))
          (let* ((owned (dorg-ext--owned-tables ext))
                 (effects (funcall project index-context))
                 (tables (mapcar (lambda (e) (dorg-ext-validate-index-effect e owned))
                                 effects))
                 (stmts (mapcar #'dorg-ext--index-effect-sql effects)))
            (setq sql (nconc sql stmts))
            (setq all-effects (nconc all-effects (copy-sequence effects)))
            (push (cons name (list :statements (length stmts))) extensions)
            (when tables
              (push (cons name (list :tables (delete-dups (copy-sequence tables))))
                    ext-dirty))))))
    (list :sql sql
          :effects all-effects
          :extensions (nreverse extensions)
          :ext-dirty (nreverse ext-dirty))))

;;; Placement Participants
;;
;; Generic DOrg stores heading `level' and `parent_id' as decoupled
;; columns with NO invariant tying a level-1 heading to a genuine
;; top-level identity.  The buffer star-count-authoritative write-back
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
`:DORG_ID:').  ROOT-ID is the tree's level-0 root id.  STORED-PARENT-ID and
STORED-LEVEL are the heading's lineage as recorded in the backing store
\(nil when the heading is new).  BUFFER-PARENT-ID and BUFFER-LEVEL are the
lineage the current buffer/render placement would record.  MOVE-INTENT is
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
