;;; dorg-mutate.el --- Source-generic mutation API for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The write-side companion to `dorg-ql'.  `dorg-mutate' defines the
;; PUBLIC, source-generic mutation contract over generic Org-shaped data:
;; headings, heading metadata, properties, planning, root-level file
;; keywords, ordered heading items (addressed by selector, not by item
;; id), and tree movement including explicit cross-root subtree moves.
;;
;; This is NOT downstream-domain CRUD.  The surface contains no downstream-domain
;; vocabulary (no claim/advance/note/dependency/status verbs).  Callers
;; such as a downstream domain library compose their own domain operations
;; out of these storage primitives.
;;
;; The public DSL belongs to `dorg-api'.  This module receives only the typed
;; patch IR produced at that boundary, validates it as a closed verb/target
;; matrix, plans it without mutation, and executes the same batch.  Unsupported
;; source/patch combinations are explicit plan data and typed refusals; no raw
;; operation dialect or compatibility executor survives here.
;;
;; This module owns the engine contract: error type and structured data, source
;; descriptor resolution, typed-patch validation, DORG metadata protection,
;; capability planning, and the source-neutral item/tree transforms.  Those
;; transforms preserve strict occurrence behavior, order, post-blank counts,
;; and items hashes.
;;
;; The pure item core is consumed by wired backend executors: the
;; plain-org executor (`dorg-mutate--plain-org-execute'), the Dolt
;; planner/executor (`dorg-mutate--dolt-execute'), and a bounded
;; same-Dolt-repo cross-root subtree move
;; (`dorg-mutate--dolt-cross-root-execute').  The private typed boundary applies
;; the batch and persists it; only a patch the resolved source genuinely
;; cannot run (e.g. a plain-org cross-source move, or a cross-repo /
;; multi-move cross-root Dolt batch) signals a deterministic
;; `unsupported' error instead of pretending to apply.
;;
;; Validation failures always signal because they are caller bugs.  Capability
;; gaps are returned by `dorg-mutate--plan-patches' and signalled by
;; `dorg-mutate--apply-patches'.  The authoritative typed matrix is declared by
;; `dorg-mutate--patch-fields'; public item IDs remain absent from the DSL.
;;
;; DOrg-owned metadata is protected from generic mutation: the
;; `DORG_ID' property is identity and is never a mutable property, and
;; any keyword whose name begins with the `DORG_' prefix (the identity,
;; hash, Dolt-dir, and future DOrg metadata keywords) is rejected by the
;; generic keyword and item operations.

;;; Code:

(require 'cl-lib)
(require 'dorg-error)
(require 'dorg-backend)
(require 'dorg-registry)
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-id)
(require 'dorg-sortkey)
(require 'dorg-sql-gen)                 ; pure SQL text only; no Dolt transport
(require 'dorg-projection)
(require 'dorg-ext)
;; B8/D6: `dorg-dolt' and `dorg-model-load' belong only to
;; the DOLT executor/planner.  They are loaded lazily at the actual kind=dolt
;; PLAN/APPLY DISPATCH (`dorg-mutate--ensure-dolt-backend', after kind=dolt is
;; determined and before any Dolt helper), so a plain-org `(require
;; \\='dorg-mutate)' + plain-org plan/apply stays Dolt-free.  Pure SQL text
;; generation is required normally above; only transport/hydration symbols need
;; forward declarations, supplied at runtime by the dispatch `require'.
(declare-function dorg-sql "dorg-dolt")
(declare-function dorg-head-commit "dorg-dolt")
(declare-function dorg-model-load-headings "dorg-model-load")
(declare-function dorg-model-load-tree "dorg-model-load")

;;; ─── Errors and Structured Data ───────────────────────────────────────

(define-error 'dorg-mutate-error "dorg mutation failed" 'dorg-error)

;; A `dorg-mutate-error' is signalled with data (MESSAGE DATA), where
;; DATA is a plist whose `:reason' is one of the symbols below.  The
;; structured plist lets callers branch on the failure class and inspect
;; the offending patch/source without parsing the message string.
;;
;;   `invalid-op'     the typed patch is malformed or its verb/target pair is
;;                    outside the closed engine matrix.
;;   `invalid-target' a patch target (heading address, occurrence, key,
;;                    or item value) is the wrong shape or type, or
;;                    names DOrg-protected metadata.
;;   `unsupported'    the resolved source cannot execute the operation:
;;                    either the contract does not cover this source/patch
;;                    pair, or no executor is implemented for the source
;;                    kind yet.
;;   `stale'          optimistic concurrency failure: the source's live
;;                    revision/commit no longer matches the caller's
;;                    `:base-revision' / `:base-commit'.  Raised by
;;                    executors.
;;   `invariant'      a tree/order/schema invariant would be violated
;;                    (e.g. moving a heading under itself).
;;   The multi-writer linearization boundary (tracked-regression) adds four
;;   terminal reasons carried through the same typed condition:
;;
;;   `overlap'        first-committer-wins: a field this intent writes was
;;                    changed by another committed writer since the plan's
;;                    `orig-base'.  Carries the overlapping (entity, field)
;;                    plus {base, their, our}.  TERMINAL (never re-planned,
;;                    which would silently last-writer-wins overwrite).
;;   `contended'      the per-store mutation gate could not be acquired
;;                    within the bounded acquire deadline (GET_LOCK
;;                    contention; waiter order is undefined).
;;   `idempotency-key-mismatch'
;;                    a caller-supplied `:op-id' was replayed with a
;;                    DIFFERENT intent digest than its recorded evidence;
;;                    ZERO SQL is executed (never success for the wrong
;;                    intent).
;;   `indeterminate'  a transport failure during/after DOLT_COMMIT left the
;;                    outcome unknown (the commit may or may not have
;;                    landed); the caller MUST reconcile with the same
;;                    `:op-id' and MUST NOT blind-retry a raw apply.

(defconst dorg-mutate-error-reasons
  '(invalid-op invalid-target unsupported stale invariant
    ;; tracked-regression multi-writer linearization terminal reasons.
    overlap contended idempotency-key-mismatch indeterminate
    ;; tracked-regression: a settled evidence row predating postimage receipts
    ;; cannot satisfy an evidence-requesting replay; typed operator
    ;; adjudication, never a verified success.
    legacy-evidence-unavailable)
  "The closed set of `:reason' symbols a `dorg-mutate-error' may carry.")

(defun dorg-mutate--raise (reason message &rest data)
  "Signal `dorg-mutate-error' with REASON, MESSAGE, and extra DATA plist.
REASON must be one of `dorg-mutate-error-reasons'.  The condition data
is (MESSAGE PLIST) where PLIST begins with `:reason' REASON followed by
DATA.  MESSAGE is also embedded so `error-message-string' is readable."
  (cl-assert (memq reason dorg-mutate-error-reasons))
  (signal 'dorg-mutate-error
          (list message (apply #'list :reason reason data))))

(defun dorg-mutate-error-data (err)
  "Return the structured data plist of a `dorg-mutate-error' condition ERR.
ERR is the object bound by `condition-case'.  Returns nil when ERR is
not a `dorg-mutate-error'.  Delegates to the generic `dorg-error-data'
(F2 consolidation) after pinning the leaf symbol, so the per-symbol
isolation is preserved while the plist extraction lives in one place."
  (and (eq (car-safe err) 'dorg-mutate-error)
       (dorg-error-data err)))

(defun dorg-mutate-error-reason (err)
  "Return the `:reason' symbol of a `dorg-mutate-error' condition ERR."
  (plist-get (dorg-mutate-error-data err) :reason))

;;; ─── Exactly-once identity: intent digest + generation nonce (rsuwn) ───
;; A caller-supplied `:op-id' is bound to an INTENT DIGEST so a replay carrying
;; the same op-id but a different intent is rejected (never succeeds for the
;; wrong mutation).  The digest is a pure function of the typed patches and the
;; store's STABLE root identity (never the client path), so it is identical
;; across re-plans and across path/symlink/worktree aliases of one store.

(defun dorg-mutate--canonical-intent (patches source)
  "Return a canonical, path-independent string form of the mutation INTENT.
PATCHES is typed patch IR v2 (deterministic from `dorg-api-normalize'); SOURCE
is the resolved source.  Keyed on the DB-derived root id and source kind — not
the client path — so equivalent source aliases serialize identically.  Carries
no HEAD/base, so it is stable across re-plans."
  (prin1-to-string (list :patches patches
                         :root-id (dorg-mutate-source-root-id source)
                         :kind (dorg-mutate-source-kind source))))

(defun dorg-mutate--intent-digest (patches source)
  "Return the sha256 hex digest binding an op-id to PATCHES on SOURCE.
See `dorg-mutate--canonical-intent'."
  (secure-hash 'sha256 (dorg-mutate--canonical-intent patches source)))

(defvar dorg-mutate--gen-id-counter 0
  "Monotonic within-process counter feeding `dorg-mutate--new-gen-id' uniqueness.")

(defun dorg-mutate--new-gen-id ()
  "Return a fresh per-generation nonce (<=64 chars) for one evidence generation.
Minted at first plan, PRESERVED across every re-plan of one attempt-series, and
FRESH for a genuinely new generation (e.g. a legitimate post-retention-window
reuse of the same op-id), so `dolt_diff' recovery keyed on it always resolves
the CURRENT generation's creation commit.  Uniqueness combines the pid, a
high-resolution timestamp, and a monotonic counter."
  (setq dorg-mutate--gen-id-counter (1+ dorg-mutate--gen-id-counter))
  (substring (secure-hash 'sha256 (format "%d:%s:%d"
                                          (emacs-pid) (float-time)
                                          dorg-mutate--gen-id-counter))
             0 48))

;;; ─── Source Descriptors and Resolution ────────────────────────────────

(cl-defstruct (dorg-mutate-source
               (:constructor dorg-mutate--source-make)
               (:copier nil))
  "A resolved, source-generic mutation target.
KIND is `plain-org' or `dolt'.  BUFFER is the live buffer when one
backs the source, else nil.  FILE is the absolute org file path when
known.  DOLT-DIR and ROOT-ID are populated only for the `dolt' kind.

IDENTITY is the binding's normalized identity POLICY — independent of
fidelity: `dorg' (DOrg-owned DORG_ID identity, addressed by heading id),
\(:property \"K\") (a domain PROPERTIES key K is the stable identity,
addressed by an extern key), or `none' (reads permitted, every mutation
refused).  A nil slot means DERIVE the default from the backend (both
`dolt' and `plain-org' default to `dorg'); read the policy through the
single resolver `dorg-mutate--source-identity', never the raw slot.
FIDELITY is BACKEND-DERIVED, not a slot: every plain-org edit is
source-preserving; there is no faithful flag.

The struct carries no downstream-domain vocabulary; it is purely a
storage binding."
  kind buffer file dolt-dir root-id identity)

(defun dorg-mutate--assert-source-kind (kind)
  "Return KIND when it belongs to the closed mutation source universe.
Every source ingress calls this before planning.  An unknown kind is a typed
caller-facing refusal here; downstream two-arm dispatch treats any impossible
kind as an internal invariant breach."
  (unless (memq kind '(plain-org dolt))
    (dorg-mutate--raise 'unsupported
                        (format "unknown source kind `%s'" kind)
                        :detail 'unknown-source-kind
                        :source-kind kind))
  kind)

;;; ─── Identity policy: the single resolver + predicates ────────────────

(defun dorg-mutate--normalize-identity (value)
  "Normalize a descriptor/registration :identity VALUE to a policy, or signal.
Accepts `dorg', `none', (:property \"K\"), or nil (nil => derive the default).
A malformed value signals `invalid-target' `:detail identity-policy-malformed'."
  (cond
   ((null value) nil)
   ((eq value 'dorg) 'dorg)
   ((eq value 'none) 'none)
   ((and (consp value) (eq (car value) :property)
         (stringp (cadr value)) (null (cddr value)))
    (list :property (cadr value)))
   (t (dorg-mutate--raise 'invalid-target
                          (format "invalid identity policy: %S" value)
                          :detail 'identity-policy-malformed :value value))))

(defun dorg-mutate--source-identity (source)
  "Return the normalized identity POLICY for resolved SOURCE.
The SINGLE interpretation of identity: `dorg' | (:property K) | `none'.
When the binding declares no policy, derive the default from the backend
\(both `dolt' and `plain-org' default to `dorg')."
  (or (dorg-mutate-source-identity source) 'dorg))

(defun dorg-mutate--identity-none-p (source)
  "Non-nil when SOURCE's identity policy is `none' (read-only)."
  (eq (dorg-mutate--source-identity source) 'none))

(defun dorg-mutate--identity-dorg-p (source)
  "Non-nil when SOURCE's identity policy is `dorg' (DORG_ID-owned)."
  (eq (dorg-mutate--source-identity source) 'dorg))

(defun dorg-mutate--identity-protected-key (source)
  "The identity PROPERTIES key to preserve out-of-band on SOURCE, or nil.
For a (:property K) policy the key is K; for a `dorg' policy it is \"DORG_ID\";
`none' protects nothing (it never writes)."
  (let ((p (dorg-mutate--source-identity source)))
    (cond ((and (consp p) (eq (car p) :property)) (cadr p))
          ((eq p 'dorg) dorg-id-property)
          (t nil))))

(defconst dorg-mutate--descriptor-keys
  '(:buffer :file :plain-org :dolt-dir :root-id :identity)
  "The recognized source-descriptor plist keys.")

(defun dorg-mutate--reject-unknown-descriptor-keys (desc)
  "Signal `invalid-target' `:detail unknown-descriptor-key' for any DESC key outside
`dorg-mutate--descriptor-keys'.  The removed legacy identity descriptor keys are
thereby rejected WITHOUT this module naming them (clean-break surface)."
  (let ((ks desc))
    (while ks
      (unless (memq (car ks) dorg-mutate--descriptor-keys)
        (dorg-mutate--raise 'invalid-target
                            (format "unrecognized source-descriptor key %S" (car ks))
                            :detail 'unknown-descriptor-key :key (car ks)))
      (setq ks (cddr ks)))))

(defun dorg-mutate--source-from-registration (reg buffer)
  "Build a `dorg-mutate-source' from registration REG and optional BUFFER."
  (let* ((backend (dorg-registration-backend reg))
         (type (dorg-backend-type backend)))
    (dorg-mutate--assert-source-kind type)
    (cl-ecase type
      (dolt
       (dorg-mutate--source-make
        :kind 'dolt
        :buffer buffer
        :file (dorg-registration-file reg)
        :dolt-dir (dorg-backend-dolt-dir backend)
        :root-id (dorg-backend-root-id backend)
        :identity (dorg-mutate--normalize-identity (dorg-registration-identity reg))))
      (plain-org
       (dorg-mutate--source-make
        :kind 'plain-org
        :buffer buffer
        :file (dorg-registration-file reg)
        :identity (dorg-mutate--normalize-identity
                   (dorg-registration-identity reg)))))))

(defun dorg-mutate--resolve-descriptor (desc)
  "Resolve an explicit source descriptor plist DESC to a `dorg-mutate-source'.
Recognized descriptor shapes (source-generic, no downstream vocabulary):

  (:buffer BUFFER)          a registered live buffer.
  (:file PATH)              an explicit plain-org file.
  (:plain-org PATH)         an explicit plain-org file (alias).
  (:dolt-dir DIR :root-id ID)  an explicit Dolt root."
  (dorg-mutate--reject-unknown-descriptor-keys desc)
  (cond
   ((plist-member desc :buffer)
    (let ((buf (plist-get desc :buffer)))
      (unless (buffer-live-p (get-buffer buf))
        (dorg-mutate--raise 'invalid-target
                            "source :buffer is not a live buffer"
                            :buffer buf))
      ;; Strict: a mutate operation writes the backing store, so a keyword
      ;; Dolt source buffer must be bound to a real root_id — ensure.
      (let ((reg (dorg--ensure-registration (get-buffer buf))))
        (unless reg
          (dorg-mutate--raise 'invalid-target
                              "source buffer has no backing storage"
                              :buffer buf))
        (let ((source (dorg-mutate--source-from-registration reg (get-buffer buf))))
          ;; D4 descriptor threading: an explicit descriptor `:identity' WINS over
          ;; the registration default (normalized here so a malformed value signals
          ;; `identity-policy-malformed' like every other descriptor branch).  When
          ;; the descriptor omits `:identity', the registration-derived policy stands.
          (when (plist-member desc :identity)
            (setf (dorg-mutate-source-identity source)
                  (dorg-mutate--normalize-identity (plist-get desc :identity))))
          source))))
   ((or (plist-member desc :file) (plist-member desc :plain-org))
    (let ((path (or (plist-get desc :file) (plist-get desc :plain-org))))
      (unless (stringp path)
        (dorg-mutate--raise 'invalid-target
                            "source file path must be a string"
                            :file path))
      (dorg-mutate--source-make
       :kind 'plain-org
       :file (expand-file-name path)
       :buffer (find-buffer-visiting (expand-file-name path))
       :identity (dorg-mutate--normalize-identity (plist-get desc :identity)))))
   ((plist-member desc :dolt-dir)
    (let ((dir (plist-get desc :dolt-dir))
          (root-id (plist-get desc :root-id)))
      (unless (stringp dir)
        (dorg-mutate--raise 'invalid-target
                            "source :dolt-dir must be a string"
                            :dolt-dir dir))
      (unless root-id
        (dorg-mutate--raise 'invalid-target
                            "Dolt source requires an explicit :root-id"
                            :dolt-dir dir))
      (unless (or (integerp root-id)
                  (and (stringp root-id)
                       (string-match-p "\\`[0-9]+\\'" root-id)))
        (dorg-mutate--raise 'invalid-target
                            (format "Dolt source :root-id must be a heading id: %S"
                                    root-id)
                            :dolt-dir dir :root-id root-id))
      (dorg-mutate--source-make
       :kind 'dolt
       :dolt-dir (file-name-as-directory (expand-file-name dir))
       :root-id (if (stringp root-id) (string-to-number root-id) root-id)
       :identity (dorg-mutate--normalize-identity (plist-get desc :identity)))))
   (t
    (dorg-mutate--raise 'invalid-target
                        "unrecognized source descriptor"
                        :descriptor desc))))

(defun dorg-mutate--resolve-source (source)
  "Resolve SOURCE to a `dorg-mutate-source' struct.
SOURCE may be nil (the current buffer), a buffer, or an explicit
descriptor plist (see `dorg-mutate--resolve-descriptor').  Signals
`dorg-mutate-error' with `:reason invalid-target' when SOURCE cannot be
resolved to a backing storage binding."
  (let ((resolved
         (cond
          ((dorg-mutate-source-p source) source)
          ((or (null source) (bufferp source)
               (and (stringp source) (get-buffer source)))
           (let* ((buffer (get-buffer (or source (current-buffer))))
                  ;; Strict: mutate writes the backing store — ensure a real root_id.
                  (reg (and buffer (dorg--ensure-registration buffer))))
             (unless reg
               (dorg-mutate--raise 'invalid-target
                                   "current/buffer source has no backing storage"
                                   :buffer (and buffer (buffer-name buffer))))
             (dorg-mutate--source-from-registration reg buffer)))
          ((and (consp source) (keywordp (car source)))
           (dorg-mutate--resolve-descriptor source))
          (t
           (dorg-mutate--raise 'invalid-target
                               "unrecognized source argument"
                               :source source)))))
    (dorg-mutate--assert-source-kind (dorg-mutate-source-kind resolved))
    resolved))

;;; ─── Target / Value Validation Primitives ─────────────────────────────

(defun dorg-mutate--typed-external-address-p (host)
  "Return non-nil when HOST is a resolved external-key address."
  (and (consp host) (eq (car host) :extern)
       (stringp (plist-get (cdr host) :key))
       (stringp (plist-get (cdr host) :value))))

(defun dorg-mutate--item-matches-target-p (item target)
  "Return non-nil when model ITEM belongs to typed TARGET's collection."
  (pcase (car target)
    ((or :text :texts) (cl-typep item 'dorg-text-item))
    ((or :keyword :keywords)
     (and (cl-typep item 'dorg-keyword-item)
          (equal (dorg-keyword-item-keyword item)
                 (dorg-mutate--patch-tget target :name))))))

(defun dorg-mutate--resolve-item-index (items target patch)
  "Return the 0-based index addressed by typed TARGET in ITEMS.
Typed occurrences are one-based; convert only at this list-selection boundary.
Single-occurrence targets refuse with `invalid-target'/`missing'."
  (let* ((matches (cl-loop for item in items for index from 0
                           when (dorg-mutate--item-matches-target-p item target)
                           collect index))
         (occurrence (when (memq (car target) '(:keyword :text))
                       (dorg-mutate--patch-tget target :occurrence)))
         ;; Named occurrences become list indexes only at this final step.
         (index (if occurrence (1- occurrence) 0)))
    (if (and (>= index 0) (< index (length matches)))
        (nth index matches)
      (dorg-mutate--raise
       'invalid-target
       (format "typed target matches no %s item at occurrence %s"
               (car target) (or occurrence 1))
       :operation (plist-get patch :op) :target target :detail 'missing))))

(defun dorg-mutate--item-from-value (value &optional post-blank)
  "Construct a model `dorg-heading-item' from a normalized VALUE plist.
VALUE is a normalized item value (see `dorg-mutate--normalize-item-value').
POST-BLANK is the item's trailing blank-line count; when nil, VALUE's own
`:post-blank' (the public grammar's optional tail, tracked-regression) is
preserved, defaulting to 0."
  (let ((pb (or post-blank (plist-get value :post-blank) 0)))
    (pcase (plist-get value :kind)
      ('text
       (dorg-text-item :post-blank pb :text (plist-get value :text)))
      ('keyword
       (dorg-keyword-item :post-blank pb
                          :keyword (plist-get value :keyword)
                          :value (plist-get value :value)))
      ('planning
       (dorg-planning-item :post-blank pb
                           :scheduled (plist-get value :scheduled)
                           :deadline (plist-get value :deadline)
                           :closed (plist-get value :closed)))
      ('drawer
       (dorg-drawer-item :post-blank pb
                         :name (plist-get value :name)
                         :lines (plist-get value :lines)
                         :properties (plist-get value :properties)))
      (kind
       (dorg-mutate--raise 'invalid-target
                           (format "cannot build item of kind %S" kind)
                           :value value)))))

(defun dorg-mutate--items-copy (items)
  "Return a deep copy of the ITEMS list with each item freshly allocated.
Output independence guarantees a mutation result can never alias the
caller's input items."
  (mapcar #'dorg-item-deep-copy items))

(defun dorg-mutate--list-insert-at (list idx elem)
  "Return LIST with ELEM inserted so that ELEM ends up at position IDX."
  (cond
   ((<= idx 0) (cons elem list))
   ((>= idx (length list)) (append list (list elem)))
   (t (append (cl-subseq list 0 idx) (list elem) (nthcdr idx list)))))

(defun dorg-mutate--item-batch-context ()
  "Return an explicit request-local mutation cursor context."
  (make-hash-table :test 'equal))

(defun dorg-mutate--item-insert-cursor-key (target position)
  "Return the request-local cursor key for TARGET/POSITION."
  (list (car target)
        (dorg-mutate--patch-tget target :host)
        (dorg-mutate--patch-tget target :name)
        (dorg-mutate--patch-tget target :occurrence)
        position))

(defun dorg-mutate--item-list-cursor-key-p (key)
  "Return non-nil when KEY addresses a top-level heading item cursor."
  (memq (car key) '(:texts :keywords)))

(defun dorg-mutate--item-batch-context-shift-items
    (context host index delta &optional except-key)
  "Shift top-level cursors in CONTEXT for HOST after an item-list edit.
INDEX is the edit position in the host item list.  DELTA is the item-count
change.  EXCEPT-KEY, when non-nil, is the cursor already updated by its own
insertion."
  (when context
    (maphash
     (lambda (key cursor)
       (when (and (dorg-mutate--item-list-cursor-key-p key)
                  (equal (nth 1 key) host)
                  (not (equal key except-key))
                  (> cursor index))
         (puthash key (+ cursor delta) context)))
     context)))

(defun dorg-mutate--item-batch-context-remove-items
    (context host indexes)
  "Shift top-level cursors in CONTEXT for HOST after removing INDEXES."
  (when context
    (maphash
     (lambda (key cursor)
       (when (and (dorg-mutate--item-list-cursor-key-p key)
                  (equal (nth 1 key) host))
         (let ((shift (cl-count-if (lambda (index) (< index cursor)) indexes)))
           (when (> shift 0)
             (puthash key (- cursor shift) context)))))
     context)))

(defun dorg-mutate--item-batch-context-clear-host (context host)
  "Drop every request-local cursor for HOST from CONTEXT."
  (when context
    (let (keys)
      (maphash
       (lambda (key _cursor)
         (when (equal (nth 1 key) host)
           (push key keys)))
       context)
      (dolist (key keys)
        (remhash key context)))))

(defun dorg-mutate--item-insert-index (items target position context)
  "Return the insertion index for TARGET/POSITION in ITEMS.
CONTEXT, when non-nil, records request-local cursors for left-addressed
positions so repeated same-batch insertions preserve declaration order."
  (let* ((matches (cl-loop for item in items for index from 0
                           when (dorg-mutate--item-matches-target-p item target)
                           collect index))
         (base (pcase position
                 (:before-first (if matches (car matches) (length items)))
                 (:after-last (if matches (1+ (car (last matches)))
                                (length items)))
                 (_ (dorg-mutate--raise
                     'invalid-op "unsupported item insertion position"
                     :operation 'insert :detail 'invalid-position
                     :position position)))))
    (if (and context (eq position :before-first))
        (let* ((key (dorg-mutate--item-insert-cursor-key target position))
               (cursor (gethash key context))
               (idx (or cursor base)))
          (puthash key (1+ idx) context)
          idx)
      base)))

(defun dorg-mutate--stabilize-text-item-boundaries (items index)
  "Ensure the local text run near INDEX keeps exact content after reparse."
  (let* ((start (if (and (> index 0)
                         (cl-typep (nth (1- index) items) 'dorg-text-item))
                    (1- index)
                  index))
         (cursor (nthcdr start items)))
    (while (and cursor (cdr cursor))
      (if (cl-typep (car cursor) 'dorg-text-item)
          (when (zerop (dorg-item-post-blank (car cursor)))
            (setf (dorg-item-post-blank (car cursor)) 1))
        (setq cursor nil))
      (when cursor
        (setq cursor (cdr cursor)))))
  items)

(defun dorg-mutate--apply-insert-item (items patch &optional batch-context)
  "Return a new item list with typed insert PATCH applied.
Text and keyword insertion land at the addressed collection's explicit
position, or at host end when the collection is empty."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (value (plist-get patch :value))
         (position (plist-get patch :position))
         (cursor-key (and batch-context
                          (eq position :before-first)
                          (dorg-mutate--item-insert-cursor-key
                           target position)))
         (new (dorg-mutate--item-from-value
               (pcase (car target)
                 (:texts (list :kind 'text :text value))
                 (:keywords (list :kind 'keyword
                                  :keyword (dorg-mutate--patch-tget target :name)
                                  :value value))))))
    (let* ((idx (dorg-mutate--item-insert-index
                 copy target position batch-context))
           (inserted (dorg-mutate--list-insert-at copy idx new)))
      (dorg-mutate--item-batch-context-shift-items
       batch-context (dorg-mutate--patch-tget target :host) idx 1 cursor-key)
      (if (eq (car target) :texts)
          (dorg-mutate--stabilize-text-item-boundaries inserted idx)
        inserted))))

(defun dorg-mutate--apply-replace-item (items patch)
  "Return a new item list with typed single-occurrence PATCH applied."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (index (dorg-mutate--resolve-item-index copy target patch))
         (existing (nth index copy))
         (value (plist-get patch :value))
         (new (dorg-mutate--item-from-value
               (pcase (car target)
                 (:text (list :kind 'text :text value))
                 (:keyword (list :kind 'keyword
                                 :keyword (dorg-mutate--patch-tget target :name)
                                 :value value)))
               (dorg-item-post-blank existing))))
    (setf (nth index copy) new)
    copy))

(defun dorg-mutate--apply-delete-item
    (items patch &optional batch-context)
  "Return a new item list with typed single-keyword PATCH removed."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (index (dorg-mutate--resolve-item-index
                 copy target patch)))
    (dorg-mutate--item-batch-context-remove-items
     batch-context (dorg-mutate--patch-tget target :host) (list index))
    (append (cl-subseq copy 0 index) (nthcdr (1+ index) copy))))

(defun dorg-mutate--apply-delete-keywords
    (items patch &optional batch-context)
  "Return ITEMS without any keyword in typed PATCH's addressed collection."
  (let* ((target (plist-get patch :target))
         (copy (dorg-mutate--items-copy items))
         (removed nil))
    (cl-loop for item in copy for index from 0
             when (dorg-mutate--item-matches-target-p item target)
             do (push index removed))
    (dorg-mutate--item-batch-context-remove-items
     batch-context (dorg-mutate--patch-tget target :host) (nreverse removed))
    (cl-remove-if (lambda (item)
                    (dorg-mutate--item-matches-target-p item target))
                  copy)))

(defun dorg-mutate--apply-replace-keywords
    (items patch &optional batch-context)
  "Replace a same-key keyword collection with PATCH's ordered values.
The replacement sequence occupies host end, preserving the established public
behavior while executing it as one typed patch."
  (let* ((target (plist-get patch :target))
         (copy (dorg-mutate--items-copy items))
         (removed nil)
         (kept nil)
         (key (dorg-mutate--patch-tget target :name)))
    (cl-loop for item in copy for index from 0
             if (dorg-mutate--item-matches-target-p item target)
             do (push index removed)
             else do (push item kept))
    (setq kept (nreverse kept))
    (dorg-mutate--item-batch-context-remove-items
     batch-context (dorg-mutate--patch-tget target :host) (nreverse removed))
    (append kept
            (mapcar (lambda (value)
                      (dorg-keyword-item :keyword key :value value :post-blank 0))
                    (plist-get patch :values)))))

(defun dorg-mutate--apply-replace-items (_items patch &optional batch-context)
  "Return a fresh list built from typed whole-items PATCH."
  (dorg-mutate--item-batch-context-clear-host
   batch-context
   (dorg-mutate--patch-tget (plist-get patch :target) :host))
  (mapcar #'dorg-mutate--item-from-value (plist-get patch :items)))

(defun dorg-mutate--drawer-line-create-index (items)
  "Return the canonical insertion index for a missing generic drawer in ITEMS.
It follows leading planning and the local property-typed PROPERTIES drawer, but
never treats a later/generic raw-lines PROPERTIES block as metadata."
  (let ((local (dorg-model--local-identity-drawer items)))
    (if local
        (1+ (cl-position local items :test #'eq))
      (cl-loop for item in items
               while (cl-typep item 'dorg-planning-item)
               count t))))

(defun dorg-mutate--drawer-line-index (lines target position context)
  "Return insertion index into LINES for drawer-line TARGET/POSITION."
  (pcase position
    (:before-first
     (if context
         (let* ((key (dorg-mutate--item-insert-cursor-key target position))
                (cursor (gethash key context))
                (idx (or cursor 0)))
           (puthash key (1+ idx) context)
           idx)
       0))
    (:after-last (length lines))
    (_ (dorg-mutate--raise 'invalid-op "unsupported drawer-line position"
                           :operation 'insert :detail 'invalid-position
                           :position position))))

(defun dorg-mutate--apply-insert-drawer-line
    (items patch &optional batch-context)
  "Return a new item list after typed drawer-line PATCH.
The first exact-name generic drawer is deep-copied and receives LINE at the
requested position.  If absent, `:missing :create' creates a raw-lines drawer
at the canonical metadata/body boundary.  All other items and the targeted
drawer's post-blank are preserved."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (name (dorg-mutate--patch-tget target :name))
         (position (plist-get patch :position))
         (line (plist-get patch :value))
         (drawer (cl-find-if
                  (lambda (item)
                    (and (cl-typep item 'dorg-drawer-item)
                         (null (dorg-drawer-item-properties item))
                         (equal name (dorg-drawer-item-name item))))
                  copy)))
    (if drawer
        (let* ((lines (dorg-drawer-item-lines drawer))
               (idx (dorg-mutate--drawer-line-index
                     lines target position batch-context)))
          (setf (dorg-drawer-item-lines drawer)
                (dorg-mutate--list-insert-at
                 lines idx (copy-sequence line))))
      (if (eq (plist-get patch :missing) :create)
          (let* ((idx (dorg-mutate--drawer-line-create-index copy))
                 (cursor-key (and batch-context
                                  (eq position :before-first)
                                  (dorg-mutate--item-insert-cursor-key
                                   target position))))
            (setq copy
                  (dorg-mutate--list-insert-at
                   copy idx
                   (dorg-drawer-item :name (copy-sequence name)
                                     :lines (list (copy-sequence line))
                                     :post-blank 0)))
            (dorg-mutate--item-batch-context-shift-items
             batch-context (dorg-mutate--patch-tget target :host) idx 1)
            (when cursor-key
              (puthash cursor-key 1 batch-context)))
        (dorg-mutate--raise
         'invalid-target "drawer-line target drawer is missing"
         :operation 'insert :target target :detail 'missing)))
    copy))

(defun dorg-mutate--apply-item-patch (items patch &optional batch-context)
  "Apply one typed item PATCH to ITEMS, returning a new item list.
ITEMS is never mutated.  Occurrence resolution errors signal
`dorg-mutate-error' before any
list is produced, so a failed patch makes no partial change."
  (pcase (dorg-mutate--patch-signature patch)
    (`(insert :texts)
     (dorg-mutate--apply-insert-item items patch batch-context))
    (`(insert :keywords)
     (dorg-mutate--apply-insert-item items patch batch-context))
    (`(replace :text)       (dorg-mutate--apply-replace-item items patch))
    (`(replace :keyword)    (dorg-mutate--apply-replace-item items patch))
    (`(delete :keyword)
     (dorg-mutate--apply-delete-item items patch batch-context))
    (`(delete :keywords)
     (dorg-mutate--apply-delete-keywords items patch batch-context))
    (`(replace :keywords)
     (dorg-mutate--apply-replace-keywords items patch batch-context))
    (`(replace :items)
     (dorg-mutate--apply-replace-items items patch batch-context))
    (`(insert :drawer-lines)
     (dorg-mutate--apply-insert-drawer-line items patch batch-context))
    (_ (dorg-mutate--raise 'invalid-op "not a typed item patch"
                           :operation (plist-get patch :op)
                           :target (plist-get patch :target)))))

(defun dorg-mutate--apply-item-patch-to-heading
    (heading patch &optional batch-context)
  "Return a deep copy of HEADING with item PATCH applied and hash refreshed.
HEADING may be a level-0 root heading or any concrete heading; the root
host is handled identically.  The copy's `dorg-heading-items' is the new
list from `dorg-mutate--apply-item-patch' and its items-hash is recomputed
so the result is render/parse stable.  HEADING is not mutated."
  (let ((copy (dorg-heading-deep-copy heading)))
    (setf (dorg-heading-items copy)
          (dorg-mutate--apply-item-patch
           (dorg-heading-items copy) patch batch-context))
    (dorg-heading-compute-items-hash copy)
    copy))

;;; ─── Capability / Support Classification ──────────────────────────────

(defun dorg-mutate--cross-source-p (source patch)
  "Return non-nil when move PATCH's destination is a distinct source.
A different Dolt repository, or a switch between plain-org and Dolt,
counts as cross-source; the same Dolt repository with a different root
does NOT."
  (when-let ((dest (plist-get patch :to-source)))
    (let ((skind (dorg-mutate-source-kind source))
          (dkind (dorg-mutate-source-kind dest)))
      (not (and (eq skind dkind)
                (if (eq skind 'dolt)
                    (equal (dorg-mutate-source-dolt-dir source)
                           (dorg-mutate-source-dolt-dir dest))
                  (equal (dorg-mutate-source-file source)
                         (dorg-mutate-source-file dest))))))))

(defun dorg-mutate--contract-support (source patch)
  "Classify whether the mutation contract covers PATCH on SOURCE.
This is independent of backend dispatch details.  Returns `supported' or a
cons (unsupported . REASON-STRING)."
  (let ((signature (dorg-mutate--patch-signature patch)))
    (cond
     ((and (equal signature '(move :subtree))
           (dorg-mutate--cross-source-p source patch)
           (not (plist-get patch :allow-nonatomic)))
      (cons 'unsupported
            "cross-source move requires :allow-nonatomic"))
     (t 'supported))))

(defun dorg-mutate--dolt-move-support (source patch)
  "Classify a typed Dolt subtree move PATCH as supported or not.
Same-root moves are supported: a `:to-source' that is omitted, or that
resolves to the SAME Dolt dir and the SAME root id, stays within the one
loaded root.  A same-Dolt-dir/DIFFERENT-root destination is the bounded
cross-root archive move: it is supported here when
ids are preserved.  Explicit `:preserve-ids nil' is rejected, since a
same-repo move keeps heading ids (a copy that regenerates ids is a
different, deferred operation).  A different Dolt repository, or a switch
to a non-Dolt backend, stays unsupported: there is no atomic
cross-repository/cross-backend move in this slice.  Returns `supported'
or a cons (unsupported . REASON-STRING).

This is a per-patch classification.  The batch-level bound — exactly one
cross-root move and no other patches in the batch — is enforced separately by
`dorg-mutate--dolt-cross-root-batch-reject', so a mixed or multi-move
batch is reported unsupported there even though each patch classifies here."
  (let ((dest (plist-get patch :to-source)))
    (cond
     ((null dest) 'supported)
     ((not (eq (dorg-mutate-source-kind dest) 'dolt))
      (cons 'unsupported
            "cross-backend typed subtree move is not supported"))
     ((not (equal (dorg-mutate-source-dolt-dir dest)
                  (dorg-mutate-source-dolt-dir source)))
      (cons 'unsupported
            "cross-repository typed subtree move is not supported"))
     ((eql (dorg-mutate-source-root-id dest)
           (dorg-mutate-source-root-id source))
      'supported)
     ;; Same Dolt dir, different root: the bounded cross-root move.
     ((and (plist-member patch :preserve-ids)
           (not (plist-get patch :preserve-ids)))
      (cons 'unsupported
            ":preserve-ids nil is not supported for a same-repo cross-root move (ids are preserved)"))
     (t 'supported))))

(defun dorg-mutate--executor-patch-support (source patch)
  "Classify whether SOURCE's backend can run PATCH.
Assumes the generic contract already covers PATCH.  Returns `supported', a
STRUCTURED plain-org
record =(:unsupported t :reason SYM :message STR :variant V)=, or the legacy
Dolt cons =(unsupported . REASON-STRING)=.

Plain-org is backend-derived source-preserving: it consults the single identity
resolver and the C3 cell, so a surgical verb is supported only at a heading
host.  One field-granular S1 exception supports an exact :commented boolean
heading update; its root and every other structural/heading/planning form refuse
with `no-source-preserving-editor' + the exact C3 `:variant', and a `none'
policy refuses every cell with `identity-none'.
The Dolt executor supports same-root typed subtree moves and a bounded
same-repo cross-root archive move (one move-only batch); a cross-repo
destination, a Dolt/plain-org mixed move, or a multi-move batch is
deferred \(see `dorg-mutate--dolt-move-support').  Classifying these here
keeps the plan/capability output honest: a dry-run never promises a patch
that a non-dry-run apply would reject at dispatch."
  (let ((kind (dorg-mutate-source-kind source))
        (signature (dorg-mutate--patch-signature patch)))
    (cond
     ;; Fidelity is backend-derived: EVERY plain-org edit is source-preserving.
     ;; The single identity resolver + the C3 cell classify each (verb, host)
     ;; cell — a surgical verb is supported only at a heading host, its root
     ;; variant and every structural/heading/planning whole-verb have no
     ;; source-preserving editor (NO whole-tree route), and a `none' policy is
     ;; read-only fail-closed.  Returns a STRUCTURED unsupported record
     ;; (`:unsupported' + `:reason' + `:message' + exact C3 `:variant') so the
     ;; plan/dry-run and the apply gate refuse in lockstep with capabilities.
     ((eq kind 'plain-org)
      (let ((policy (dorg-mutate--source-identity source)))
        (cond
         ((eq policy 'none)
          (list :unsupported t :reason 'identity-none
                :message "mutation is unavailable under identity=none"
                :variant nil))
         ((dorg-mutate--c3-refusal patch))
         (t 'supported))))
     ((and (eq kind 'dolt) (equal signature '(move :subtree)))
      (dorg-mutate--dolt-move-support source patch))
     (t 'supported))))

(defun dorg-mutate--classify-patch (source patch)
  "Return an execution-support classification plist for PATCH on SOURCE.
The plist has `:op', `:target-kind', `:supported', and — when unsupported —
`:reason' (a symbol), `:detail' (a string), and, for a plain-org C3 refusal,
the exact `:variant' the runtime error must carry."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (verb (car signature))
         (target-kind (cadr signature))
         (contract (dorg-mutate--contract-support source patch))
         (base (list :op verb :target-kind target-kind)))
    (cond
     ((consp contract)
      (append base (list :supported nil :reason 'contract :detail (cdr contract))))
     (t (let ((exec (dorg-mutate--executor-patch-support source patch)))
          (cond
           ((eq exec 'supported) (append base (list :supported t)))
           ;; Structured plain-org C3 record: carry its reason + exact variant.
           ((and (consp exec) (keywordp (car exec)))
            (append base (list :supported nil
                               :reason (plist-get exec :reason)
                               :detail (plist-get exec :message))
                    (when (plist-member exec :variant)
                      (list :variant (plist-get exec :variant)))))
           ;; Legacy Dolt cons (unsupported . REASON-STRING).
           ((consp exec)
            (append base (list :supported nil :reason 'executor
                               :detail (cdr exec))))
           (t (append base (list :supported t)))))))))

;;; ─── C3 variant-granular capability classifier ────────────────────────
;; Capability is classified per (verb, host-variant) CELL, not per bare verb.
;; A plain-org (source-preserving) landing supports the surgical verbs only at a
;; concrete heading host; the same verb at the level-0 root has no in-place
;; editor.  Structural and planning verbs have none at all.  Heading updates
;; have two field-granular exceptions: exact boolean COMMENTED on a concrete
;; heading, and an explicit-commented constructor under a concrete heading.
;; Dolt is a full backend and supports every cell.  ONE classifier
;; (`dorg-mutate--c3-refusal') projects the cell into the one complete refusal
;; record consumed by plan and apply, so both reject in lockstep (D4).

(defconst dorg-mutate--c3-surgical-patches
  '((put :property) (delete :property)
    (replace :keyword) (replace :keywords) (insert :keywords)
    (delete :keyword) (delete :keywords)
    (insert :texts) (replace :text) (replace :items)
    (insert :drawer-lines))
  "Typed patch signatures with a source-preserving heading editor.")

(defconst dorg-mutate--c3-whole-patch-slices
  '(((update :heading) . S1) ((update :planning) . S2)
    ((insert :tree) . S3) ((delete :heading) . S3)
    ((delete :subtree) . S3) ((move :subtree) . S3))
  "Typed patch signatures with no plain-org editor and their C3 slice.")

(defconst dorg-mutate--c3-field-cells
  '((((update :heading) heading (:commented))
     :status supported :slice S1
     :variant (:host heading :fields (:commented)))
    (((insert :tree) heading (:commented))
     :status supported :slice S3
     :variant (:host heading :constructor (:commented))))
  "Field-granular exceptions to whole-signature plain-org C3 cells.
Each key is (SIGNATURE HOST FIELD-KEYS); its plist is the complete cell fact.
The two exceptions are the ratified boolean COMMENTED intrinsic slot and the
explicit-commented, concrete-parent, unanchored public constructor.  General
heading-line/tree editing remains parked under tracked-regression, tracked-regression,
and tracked-regression")

(defun dorg-mutate--c3-public-property-items-p (items)
  "Non-nil when ITEMS is the public constructor's coalesced property drawer."
  (or (null items)
      (and (= (length items) 1)
           (let ((item (car items)))
             (and (eq (plist-get item :kind) 'drawer)
                  (equal (plist-get item :name) "PROPERTIES")
                  (listp (plist-get item :properties))
                  (null (plist-get item :lines)))))))

(defun dorg-mutate--c3-commented-constructor-p (patch)
  "Non-nil when PATCH is the bounded public COMMENTED constructor cell.
The clause must be explicitly present as a boolean.  The parent is classified
separately by the C3 host; anchors and raw typed payload extensions are outside
the ratified cell."
  (let* ((target (plist-get patch :target))
         (payload (plist-get patch :heading))
         (keys (cl-loop for (key _value) on payload by #'cddr collect key)))
    (and (equal (dorg-mutate--patch-signature patch) '(insert :tree))
         (not (plist-member (cdr target) :before))
         (not (plist-member (cdr target) :after))
         (plist-member payload :commented)
         (memq (plist-get payload :commented) '(nil t))
         (cl-every (lambda (key)
                     (memq key '(:title :todo :priority :tags :commented :items)))
                   keys)
         (dorg-mutate--c3-public-property-items-p
          (plist-get payload :items)))))

(defun dorg-mutate--c3-field-keys (patch)
  "Return PATCH's bounded C3 field-cell discriminator, or nil."
  (pcase (dorg-mutate--patch-signature patch)
    (`(update :heading)
     (cl-loop for (key _value) on (plist-get patch :fields) by #'cddr
              collect key))
    (`(insert :tree)
     (and (dorg-mutate--c3-commented-constructor-p patch) '(:commented)))))

(defun dorg-mutate--c3-patch-host (patch)
  "Return PATCH C3 host variant, either `heading' or `root'."
  (let ((signature (dorg-mutate--patch-signature patch)))
    (if (and (or (member signature dorg-mutate--c3-surgical-patches)
                 (equal signature '(update :heading))
                 (equal signature '(insert :tree)))
             (eq (dorg-mutate--source-preserving-patch-host patch) :root))
        'root
      'heading)))

(defun dorg-mutate--c3-field-cell (patch host)
  "Return field-granular C3 data for PATCH at HOST, or nil."
  (alist-get (list (dorg-mutate--patch-signature patch) host
                   (dorg-mutate--c3-field-keys patch))
             dorg-mutate--c3-field-cells nil nil #'equal))

(defun dorg-mutate--c3-cell (patch host)
  "Return the plain-org C3 (STATUS . SLICE) for PATCH at HOST."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (field-cell (dorg-mutate--c3-field-cell patch host)))
    (cond
     (field-cell (cons (plist-get field-cell :status)
                       (plist-get field-cell :slice)))
     ((not (member signature dorg-mutate--c3-surgical-patches))
      (cons 'unsupported
            (or (alist-get signature dorg-mutate--c3-whole-patch-slices
                           nil nil #'equal)
                'S3)))
     ((eq host 'root) (cons 'unsupported 'S4))
     (t (cons 'supported nil)))))

(defun dorg-mutate--c3-variant (patch host)
  "Return the authoritative C3 `:variant' for PATCH at HOST.
Whole-verb-collapsed patches carry the empty variant nil; a surgical verb carries
=(:host HOST)= so the heading/root cells stay distinguishable."
  (let ((signature (dorg-mutate--patch-signature patch)))
    (if-let ((field-cell (dorg-mutate--c3-field-cell patch host)))
        (plist-get field-cell :variant)
      (when (member signature dorg-mutate--c3-surgical-patches)
        (list :host host)))))

(defun dorg-mutate--c3-refusal (patch)
  "Return PATCH's complete plain-org C3 refusal record, or nil.
The record is the single source for plan classification and apply enforcement:
it carries the reason, rendered message, operation, target kind, and exact C3
variant.  A whole-verb refusal includes `:variant nil' so the empty variant is
distinguished from an omitted field."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (host (dorg-mutate--c3-patch-host patch)))
    (when (eq 'unsupported (car (dorg-mutate--c3-cell patch host)))
      (list :unsupported t
            :reason 'no-source-preserving-editor
            :message (format "no source-preserving editor for %S%s"
                             signature
                             (if (eq host 'root) " at the level-0 root" ""))
            :operation (car signature)
            :target-kind (cadr signature)
            :variant (dorg-mutate--c3-variant patch host)))))

;;; ─── Plain-Org Revision Token ─────────────────────────────────────────

(defun dorg-mutate--plain-org-revision (source)
  "Return an optimistic-concurrency revision token for plain-org SOURCE.
The token captures buffer/file identity, a stat change counter, AND an exact
raw-content `:fingerprint' (sha256 over the encoded on-disk bytes) so
typed apply can reject a stale write even when the stat counter coincides
\(an equal-size/equal-mtime replacement).  For a live buffer the counter is
`buffer-chars-modified-tick'; for a file with no live buffer it is the file
modification time; both additionally carry the content `:fingerprint'
\(`:absent' when the file does not yet exist).  Returns nil for non-plain-org
sources."
  (when (eq (dorg-mutate-source-kind source) 'plain-org)
    (let ((buf (dorg-mutate-source-buffer source))
          (file (dorg-mutate-source-file source)))
      (cond
       ((buffer-live-p buf)
        (list :kind 'buffer
              :buffer (buffer-name buf)
              :file file
              :tick (buffer-chars-modified-tick buf)
              :fingerprint (dorg-mutate--source-buffer-fingerprint buf)))
       ((and file (file-exists-p file))
        (list :kind 'file
              :file file
              :mtime (file-attribute-modification-time
                      (file-attributes file))
              :fingerprint (dorg-mutate--persist-file-fingerprint file)))
       (t (list :kind 'file :file file :mtime nil :fingerprint :absent))))))

(defun dorg-mutate--source-buffer-fingerprint (buffer)
  "Return the content fingerprint of BUFFER's encoded on-disk bytes.
Encodes with BUFFER's own coding system (DOrg `utf-8-unix' default) so the
hash matches what `dorg-mutate--plain-org-commit' writes and what
`dorg-mutate--persist-file-fingerprint' reads back."
  (with-current-buffer buffer
    (dorg-mutate--persist-fingerprint
     (dorg-mutate--persist-encode
      (buffer-substring-no-properties (point-min) (point-max))
      (or buffer-file-coding-system 'utf-8-unix)))))

(defun dorg-mutate--source-public-base (source)
  "Return the D5 PUBLIC pre-write freshness `:base' token for resolved SOURCE.
The token reflects the ACTUAL resolved source state (reviewer B2): when SOURCE has
a live visiting BUFFER it is the buffer token
\(:kind buffer :buffer NAME :file F :tick TICK :fingerprint FP) keyed off
`buffer-chars-modified-tick', so an unsaved buffer edit between a dry-run and its
apply is detected as `stale'/`base-revision' before any write; otherwise it is the
file token \(:kind file :file F :mtime M :fingerprint FP).  The `:fingerprint' is
an exact raw-content hash (sha256 over the encoded on-disk bytes) carried IN
ADDITION to the stat counter, so an equal-size/equal-mtime external replacement
across the dry-run -> apply flow is still caught (a stat-only token cannot see
it).  It shares the buffer-preferring shape of the internal optimistic-concurrency
token `dorg-mutate--plain-org-revision', but stays a SEPARATE public contract: it
is the freshness handle a dry-run hands back and `dorg-api-execute' validates at
the command layer, never forwarded to the engine as `:base-revision'.  Returns nil
for a source with no buffer and no file."
  (when (eq (dorg-mutate-source-kind source) 'plain-org)
    (let ((buf (dorg-mutate-source-buffer source))
          (file (dorg-mutate-source-file source)))
      (cond
       ((buffer-live-p buf)
        (list :kind 'buffer :buffer (buffer-name buf) :file file
              :tick (buffer-chars-modified-tick buf)
              :fingerprint (dorg-mutate--source-buffer-fingerprint buf)))
       (file
        (list :kind 'file :file file
              :mtime (and (file-exists-p file)
                          (file-attribute-modification-time
                           (file-attributes file)))
              :fingerprint (dorg-mutate--persist-file-fingerprint file)))
       (t nil)))))

(defun dorg-mutate--revision-stale-p (supplied current)
  "Return non-nil when SUPPLIED base-revision no longer matches CURRENT.
SUPPLIED and CURRENT are plain-org revision tokens (see
`dorg-mutate--plain-org-revision').  A nil SUPPLIED means the caller
requested no optimistic-concurrency check, so it is never stale.  A
change of token KIND (e.g. a file that gained a live buffer) is treated
as stale, as is a buffer tick or file mtime that has advanced.

The token is source-bound: a token captured from one buffer/file is
stale against a different source even when the change counters happen to
coincide.  Buffer tokens compare buffer name and visited file; file
tokens compare the file path.  This prevents replaying a token across
distinct plain-org sources that share a tick/mtime."
  (cond
   ((null supplied) nil)
   ((not (eq (plist-get supplied :kind) (plist-get current :kind))) t)
   ;; Content CAS (strengthening only): when BOTH tokens carry a fingerprint, a
   ;; differing one is stale even if the stat counters coincide — this catches
   ;; an equal-stat replacement.  Tokens without fingerprints fall through to
   ;; the stat comparison unchanged.
   ((and (plist-get supplied :fingerprint) (plist-get current :fingerprint)
         (not (equal (plist-get supplied :fingerprint)
                     (plist-get current :fingerprint))))
    t)
   ((eq (plist-get supplied :kind) 'buffer)
    (or (not (equal (plist-get supplied :buffer) (plist-get current :buffer)))
        (not (equal (plist-get supplied :file) (plist-get current :file)))
        (not (eql (plist-get supplied :tick) (plist-get current :tick)))))
   (t (or (not (equal (plist-get supplied :file) (plist-get current :file)))
          (not (equal (plist-get supplied :mtime) (plist-get current :mtime)))))))

;;; ─── Plain-Org Executor ───────────────────────────────────────────────

;; The plain-org executor is SURGICAL ONLY: it applies a validated typed batch
;; to a live buffer or an explicit org file by editing in place the exact
;; bytes each supported cell touches (see the source-preserving executor
;; below), byte-identical to an in-place `org-entry-put'.  There is NO
;; plain-org whole-tree model/render/replace route: only the surgically
;; supported cells (property / keyword / body / item patches on an identified
;; heading) are executable; heading-field, structural heading, and root-host
;; variants have no in-place editor and fail closed (`unsupported') before any
;; write.
;; Identity (`DORG_ID'), ordering, post-blank, preamble keywords, and every
;; untouched byte are preserved because untouched regions are never rewritten.
;;
;; The whole batch is validated + resolved BEFORE any write, so a missing
;; target or a mid-batch invariant violation signals and leaves the source
;; byte-for-byte unchanged (no partial write).  A supplied `:base-revision'
;; is checked against the live source first, so a stale optimistic-concurrency
;; token is rejected before any work.

;; The `dorg-mutate--ps' working model below is SOURCE-NEUTRAL and is retained
;; for the DOLT executor (`--dolt-load'/`--ps-apply-patch' diff the model into
;; database deltas); the plain-org path does NOT use it.
(cl-defstruct (dorg-mutate--ps
               (:constructor dorg-mutate--ps-create)
               (:copier nil))
  "Mutable source-neutral working model for one typed patch batch.
Retained for the Dolt executor (the surgical plain-org path does not use it).
HEADINGS is a hash id -> `dorg-heading' (every heading, including the
level-0 root).  ROOT-ID is the synthetic root's id.  CREATED/CHANGED/
DELETED accumulate affected heading ids for the result diagnostics."
  headings root-id created changed deleted)

(defun dorg-mutate--result-assert-unique-ids (result)
  "Signal `invariant' if parse RESULT carries two headings with the same id.
Delegates the one duplicate-id policy to
`dorg-parse-assert-unique-ids', translating its parse-domain condition
into the typed mutation error contract."
  (condition-case err
      (dorg-parse-assert-unique-ids result)
    (dorg-parse-duplicate-id
     (dorg-mutate--raise
      'invariant
      (format "plain-org source contains duplicate DORG_ID %s" (cadr err))
      :detail 'duplicate-heading-id
      :id (cadr err)
      :first-title (nth 2 err)
      :second-title (nth 3 err)))))

;;; Model navigation helpers

(defun dorg-mutate--heading-property (heading key)
  "Return HEADING's PROPERTIES value for KEY, or nil when absent."
  (let ((drawer (dorg-mutate--ps-find-properties heading)))
    (when drawer
      (cdr (assoc key (dorg-drawer-item-properties drawer))))))

(defun dorg-mutate--ps-resolve-extern (ps host patch)
  "Resolve external-key HOST against PS by matching a PROPERTIES value.
HOST is (:extern :key KEY :value VALUE).  Returns the unique matching
`dorg-heading'.  Resolution runs against the executor's OWN parse, so it
never depends on an ephemeral snowflake.  Zero matches signal
`invalid-target' `:detail missing'; more than one signal `invalid-target'
`:detail ambiguous' — a caller-named identity key must be unique."
  (let* ((key (plist-get (cdr host) :key))
         (value (plist-get (cdr host) :value))
         (matches nil))
    (maphash (lambda (_id h)
               (when (equal value (dorg-mutate--heading-property h key))
                 (push h matches)))
             (dorg-mutate--ps-headings ps))
    (cond
     ((null matches)
      (dorg-mutate--raise 'invalid-target
                          (format "no heading with %s=%S in source" key value)
                          :operation (plist-get patch :op)
                          :target host :detail 'missing))
     ((cdr matches)
      (dorg-mutate--raise 'invalid-target
                          (format "%d headings match %s=%S; identity key must be unique"
                                  (length matches) key value)
                          :operation (plist-get patch :op)
                          :target host :detail 'ambiguous))
     (t (car matches)))))

(defun dorg-mutate--ps-heading (ps host patch)
  "Return the `dorg-heading' addressed by HOST in PS, or signal missing.
HOST is a concrete id, the symbol `:root', or an external-key host
(:extern :key KEY :value VALUE) resolved against PS's own parse.  PATCH is the
typed patch the lookup is for, used only for error context."
  (if (dorg-mutate--typed-external-address-p host)
      (dorg-mutate--ps-resolve-extern ps host patch)
    (let ((id (if (eq host :root) (dorg-mutate--ps-root-id ps) host)))
      (or (gethash id (dorg-mutate--ps-headings ps))
          (dorg-mutate--raise 'invalid-target
                              (format "no heading with id %S in source" host)
                              :operation (plist-get patch :op)
                              :target host :detail 'missing)))))

(defun dorg-mutate--ps-children (ps parent-id)
  "Return PARENT-ID's child headings in PS, sorted by sort-key."
  (let (kids)
    (maphash (lambda (_id h)
               (when (eql (dorg-heading-parent-id h) parent-id)
                 (push h kids)))
             (dorg-mutate--ps-headings ps))
    (sort kids (lambda (a b) (string< (dorg-heading-sort-key a)
                                      (dorg-heading-sort-key b))))))

(defun dorg-mutate--ps-anchor-id (ps anchor patch)
  "Return ANCHOR as a concrete heading id in PS, resolving extern hosts."
  (cond
   ((null anchor) nil)
   ((dorg-mutate--typed-external-address-p anchor)
    (dorg-heading-id (dorg-mutate--ps-heading ps anchor patch)))
   (t anchor)))

(defun dorg-mutate--ps-after-cursor-key (parent-id declared-after-id)
  "Return the structural cursor key for PARENT-ID and DECLARED-AFTER-ID."
  (list :structural-after (list :parent parent-id)
        (list :after declared-after-id)))

(defun dorg-mutate--ps-effective-after-id
    (ps parent-id declared-after-id placed-id batch-context)
  "Return the effective after-anchor id for a structural placement.
DECLARED-AFTER-ID is the anchor from the patch.  When BATCH-CONTEXT carries a
valid prior cursor for the same destination parent and declared after-anchor,
use that cursor so repeated `:after X' placements keep request order.  A cursor
is valid only while its heading still exists under PARENT-ID and is not the
heading currently being placed; otherwise the cursor is discarded and the
declared anchor is used."
  (if (and batch-context declared-after-id)
      (let* ((key (dorg-mutate--ps-after-cursor-key parent-id declared-after-id))
             (cursor-id (gethash key batch-context))
             (cursor (and cursor-id
                          (not (eql cursor-id placed-id))
                          (gethash cursor-id (dorg-mutate--ps-headings ps)))))
        (if (and cursor (eql (dorg-heading-parent-id cursor) parent-id))
            cursor-id
          (when cursor-id
            (remhash key batch-context))
          declared-after-id))
    declared-after-id))

(defun dorg-mutate--ps-record-after-cursor
    (parent-id declared-after-id placed-id batch-context)
  "Record PLACED-ID as the request-local cursor after DECLARED-AFTER-ID."
  (when (and batch-context declared-after-id)
    (puthash (dorg-mutate--ps-after-cursor-key parent-id declared-after-id)
             placed-id batch-context)))

(defun dorg-mutate--ps-descendants (ps id)
  "Return all descendant headings of ID in PS (excluding ID)."
  (let ((out nil) (frontier (list id)))
    (while frontier
      (dolist (child (dorg-mutate--ps-children ps (pop frontier)))
        (push child out)
        (push (dorg-heading-id child) frontier)))
    out))

(defun dorg-mutate--ps-shift-levels (ps heading delta)
  "Shift HEADING's level and all descendants' levels by DELTA in PS.
Record each shifted descendant so Dolt persistence writes its new level."
  (unless (zerop delta)
    (cl-incf (dorg-heading-level heading) delta)
    (dolist (d (dorg-mutate--ps-descendants ps (dorg-heading-id heading)))
      (cl-incf (dorg-heading-level d) delta)
      (push (dorg-heading-id d) (dorg-mutate--ps-changed ps)))))

(defun dorg-mutate--ps-rebalance-order (ps ordered)
  "Assign fresh strictly-ordered, width-uniform sort keys to ORDERED in PS.
ORDERED is the intended final sibling sequence (`dorg-heading' objects) for
one parent, in document order.  Each heading's sort key is rewritten with
`dorg-sortkey-from-integer' at a width sized for the whole group, and its id
is recorded in the PS `changed' set so the net-membership diff lowers it to
an UPDATE (a freshly `created' id still routes to INSERT via the db-ops
union).  This is the complete-parent rebalance backstop reached only when a
bounded midpoint would exceed the VARBINARY(64) storage budget; it re-opens
even spacing for the whole group so an aged sibling list can absorb an
append/prepend/promotion instead of refusing at the storage cliff."
  (let ((w (dorg-sortkey-width-for-count (length ordered)))
        (n 0))
    (dolist (h ordered)
      (cl-incf n)
      (setf (dorg-heading-sort-key h) (dorg-sortkey-from-integer n w))
      (push (dorg-heading-id h) (dorg-mutate--ps-changed ps)))))

(defun dorg-mutate--ps-sibling-order-with (siblings node before-id after-id)
  "Return SIBLINGS with NODE spliced at its target slot, in document order.
BEFORE-ID/AFTER-ID (heading ids, or nil) anchor NODE relative to an existing
sibling exactly as `dorg-mutate--position-key' does; with neither, NODE is
appended.  SIBLINGS is the sorted sibling list excluding NODE.  Used to build
the final order handed to `dorg-mutate--ps-rebalance-order' when a bounded
placement exhausts."
  (cond
   (before-id
    (let (out placed)
      (dolist (s siblings)
        (when (and (not placed) (eql (dorg-heading-id s) before-id))
          (push node out) (setq placed t))
        (push s out))
      (unless placed (push node out))
      (nreverse out)))
   (after-id
    (let (out)
      (dolist (s siblings)
        (push s out)
        (when (eql (dorg-heading-id s) after-id)
          (push node out)))
      (nreverse out)))
   (t (append siblings (list node)))))

(defun dorg-mutate--position-key (siblings before-id after-id patch)
  "Return a sort key positioning a node among SIBLINGS.
SIBLINGS is the sorted sibling list excluding the node being placed.
BEFORE-ID/AFTER-ID, when non-nil, anchor the node relative to an
existing sibling; with neither, the node is appended after the last
sibling.  Signals `invalid-target' when an anchor is not a sibling.

Allocation is bounded: `dorg-sortkey-midpoint-limited' signals
`dorg-sortkey-exhausted' when the candidate key would reach the storage
cliff, so callers can complete-parent rebalance instead of writing a key the
SQL guard would refuse after the core rows commit.  Argument/order errors
propagate unchanged."
  (cl-flet ((anchor-index (id)
              (or (cl-position id siblings :key #'dorg-heading-id :test #'eql)
                  (dorg-mutate--raise
                   'invalid-target
                   (format "position anchor %S is not a sibling here" id)
                   :operation (plist-get patch :op) :target id :detail 'missing)))
            (key-at (i) (dorg-heading-sort-key (nth i siblings))))
    (cond
     (before-id
      (let ((idx (anchor-index before-id)))
        (dorg-sortkey-midpoint-limited (and (> idx 0) (key-at (1- idx))) (key-at idx))))
     (after-id
      (let ((idx (anchor-index after-id)))
        (dorg-sortkey-midpoint-limited
         (key-at idx)
         (and (< (1+ idx) (length siblings)) (key-at (1+ idx))))))
     (siblings
      (dorg-sortkey-midpoint-limited
       (dorg-heading-sort-key (car (last siblings))) nil))
     (t (dorg-sortkey-from-integer 1)))))

;;; Typed-patch model mutations

(defun dorg-mutate--patch-address (patch)
  "Return PATCH's addressed heading/root host token."
  (let ((target (plist-get patch :target)))
    (pcase (car target)
      ((or :heading :subtree) (dorg-mutate--patch-target-host target))
      ((or :property :keyword :keywords :planning :text :texts :items
           :drawer-lines)
       (dorg-mutate--patch-host (dorg-mutate--patch-tget target :host)))
      (:tree (dorg-mutate--patch-host
              (dorg-mutate--patch-tget target :under))))))

(defun dorg-mutate--patch-anchor-address (anchor)
  "Return typed ANCHOR as an engine heading address."
  (if (integerp anchor) anchor (dorg-mutate--patch-host anchor)))

(defun dorg-mutate--ps-insert-heading (ps patch &optional batch-context)
  "Apply typed tree-insert PATCH to PS."
  (let* ((target (plist-get patch :target))
         (payload (plist-get patch :heading))
         (parent (dorg-mutate--ps-heading
                  ps (dorg-mutate--patch-address patch) patch))
         (pid (dorg-heading-id parent))
         (id (or (plist-get payload :id) (dorg-id-generate))))
    (when (gethash id (dorg-mutate--ps-headings ps))
      (dorg-mutate--raise 'invalid-target
                          (format "inserted heading id %S already exists" id)
                          :operation 'insert :target id))
    (let* ((siblings (dorg-mutate--ps-children ps pid))
           (before-id (dorg-mutate--ps-anchor-id
                       ps (and (plist-member (cdr target) :before)
                               (dorg-mutate--patch-anchor-address
                                (dorg-mutate--patch-tget target :before))) patch))
           (after-id (dorg-mutate--ps-anchor-id
                      ps (and (plist-member (cdr target) :after)
                              (dorg-mutate--patch-anchor-address
                               (dorg-mutate--patch-tget target :after))) patch))
           (effective-after-id
            (dorg-mutate--ps-effective-after-id
             ps pid after-id id batch-context))
           ;; Bounded placement.  Unlike move/delete, the insert executor
           ;; deliberately hydrates only the parent plus the exact placement
           ;; neighbors (tracked-regression — never the full sibling fan), so a
           ;; complete-parent rebalance is not available here.  At the storage
           ;; cliff we therefore FAIL CLOSED before any core row is staged —
           ;; killing the core-without-projection orphan class the raw midpoint
           ;; produced when the SQL guard refused post-commit.  Restoring
           ;; head/tail insert SUCCESS onto an aged list is the deferred
           ;; asymmetric-allocator + lazy-normalization work (see Resolution).
           (key (condition-case nil
                    (dorg-mutate--position-key
                     siblings before-id effective-after-id patch)
                  (dorg-sortkey-exhausted
                   (dorg-mutate--raise
                    'invariant
                    (concat "destination sibling order is exhausted; a "
                            "prepend/append onto this aged group needs a "
                            "rebalance not available on the minimal-scope "
                            "insert path")
                    :operation 'insert :target pid
                    :detail 'sort-key-exhausted))))
           (h (make-dorg-heading
               :id id :parent-id pid :sort-key key
               :level (1+ (dorg-heading-level parent))
               :todo (plist-get payload :todo)
               :priority (plist-get payload :priority)
               :commented (and (plist-get payload :commented) t)
               :title (plist-get payload :title)
               :post-blank (or (plist-get payload :post-blank) 0)
               :tags (plist-get payload :tags)
               :items (mapcar (lambda (v) (dorg-mutate--item-from-value v 0))
                              (plist-get payload :items)))))
      (dorg-heading-compute-items-hash h)
      (puthash id h (dorg-mutate--ps-headings ps))
      (push id (dorg-mutate--ps-created ps))
      (unless before-id
        (dorg-mutate--ps-record-after-cursor
         pid after-id id batch-context))
      ;; Recursive children are part of this one typed constructor patch.  The
      ;; parent now exists in PS, so the existing insertion machinery can place
      ;; each child in order and recursively construct its descendants without
      ;; exposing an address for an uncommitted heading at the public boundary.
      (dolist (child (plist-get payload :children))
        (dorg-mutate--ps-insert-heading
         ps (list :op 'insert
                  :target (list :tree :under (list :heading :id id))
                  :heading child)
         batch-context)))))

(defun dorg-mutate--ps-update-heading (ps patch)
  "Apply typed heading-update PATCH to PS."
  (let* ((fields (plist-get patch :fields))
         (h (dorg-mutate--ps-heading ps (dorg-mutate--patch-address patch) patch)))
    (when (plist-member fields :title)
      (setf (dorg-heading-title h) (plist-get fields :title)))
    (when (plist-member fields :todo)
      (setf (dorg-heading-todo h) (plist-get fields :todo)))
    (when (plist-member fields :priority)
      (setf (dorg-heading-priority h) (plist-get fields :priority)))
    (when (plist-member fields :commented)
      (setf (dorg-heading-commented h) (and (plist-get fields :commented) t)))
    (when (plist-member fields :tags)
      (setf (dorg-heading-tags h) (plist-get fields :tags)))
    (when (plist-member fields :post-blank)
      (setf (dorg-heading-post-blank h) (plist-get fields :post-blank)))
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-move (ps patch &optional batch-context)
  "Apply typed subtree PATCH reparent+reposition.
In the tree model a subtree follows its node automatically (descendants
reference the node by id); descendant levels track the new depth."
  (let* ((to (plist-get patch :to))
         (h (dorg-mutate--ps-heading ps (dorg-mutate--patch-address patch) patch))
         (parent-host (dorg-mutate--patch-host (plist-get to :under))))
    (when parent-host
      (let ((np (dorg-mutate--ps-heading ps parent-host patch)))
        (when (or (eql (dorg-heading-id np) (dorg-heading-id h))
                  (cl-member (dorg-heading-id np)
                             (dorg-mutate--ps-descendants ps (dorg-heading-id h))
                             :key #'dorg-heading-id :test #'eql))
          (dorg-mutate--raise 'invariant
                              "cannot move a heading under itself or a descendant"
                              :operation (plist-get patch :op)
                              :heading (dorg-heading-id h)))
        (dorg-mutate--ps-shift-levels
         ps h (- (1+ (dorg-heading-level np)) (dorg-heading-level h)))
        (setf (dorg-heading-parent-id h) (dorg-heading-id np))))
    (when (or parent-host (plist-member to :before) (plist-member to :after))
      (let* ((siblings (cl-remove (dorg-heading-id h)
                                  (dorg-mutate--ps-children
                                   ps (dorg-heading-parent-id h))
                                  :key #'dorg-heading-id :test #'eql))
             (before-id (dorg-mutate--ps-anchor-id
                         ps (and (plist-member to :before)
                                 (dorg-mutate--patch-anchor-address
                                  (plist-get to :before))) patch))
             (after-id (dorg-mutate--ps-anchor-id
                        ps (and (plist-member to :after)
                                (dorg-mutate--patch-anchor-address
                                 (plist-get to :after))) patch))
             (effective-after-id
              (dorg-mutate--ps-effective-after-id
               ps (dorg-heading-parent-id h) after-id
               (dorg-heading-id h) batch-context)))
        ;; Bounded reposition; on storage-cliff exhaustion complete-parent
        ;; rebalance the destination group with the mover in its target slot.
        (condition-case nil
            (setf (dorg-heading-sort-key h)
                  (dorg-mutate--position-key
                   siblings before-id effective-after-id patch))
          (dorg-sortkey-exhausted
           (dorg-mutate--ps-rebalance-order
            ps (dorg-mutate--ps-sibling-order-with
                siblings h before-id effective-after-id))))
        (unless before-id
          (dorg-mutate--ps-record-after-cursor
           (dorg-heading-parent-id h) after-id
           (dorg-heading-id h) batch-context))))
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-delete-heading (ps patch)
  "Apply typed heading/subtree delete PATCH to PS.
A subtree target removes the heading and descendants.  A heading target
the heading is removed and its direct children are promoted to the
deleted heading's parent, keeping their order and adjusting levels."
  (let* ((h (dorg-mutate--ps-heading ps (dorg-mutate--patch-address patch) patch))
         (id (dorg-heading-id h))
         (hash (dorg-mutate--ps-headings ps)))
    (if (eq (car (plist-get patch :target)) :subtree)
        (progn
          (dolist (d (dorg-mutate--ps-descendants ps id))
            (remhash (dorg-heading-id d) hash)
            (push (dorg-heading-id d) (dorg-mutate--ps-deleted ps)))
          (remhash id hash)
          (push id (dorg-mutate--ps-deleted ps)))
      (let* ((gp (dorg-heading-parent-id h))
             (kids (dorg-mutate--ps-children ps id))
             (hkey (dorg-heading-sort-key h))
             (siblings (cl-remove id (dorg-mutate--ps-children ps gp)
                                  :key #'dorg-heading-id :test #'eql))
             (left nil) (right nil))
        (dolist (s siblings)
          (if (string< (dorg-heading-sort-key s) hkey)
              (setq left (dorg-heading-sort-key s))
            (unless right (setq right (dorg-heading-sort-key s)))))
        ;; Re-home each promoted child to the grandparent and adjust its level;
        ;; this is independent of key allocation and always applies.
        (dolist (k kids)
          (dorg-mutate--ps-shift-levels
           ps k (- (dorg-heading-level h) (dorg-heading-level k)))
          (setf (dorg-heading-parent-id k) gp)
          (push (dorg-heading-id k) (dorg-mutate--ps-changed ps)))
        ;; Allocate bounded keys in the deleted heading's gap.  Promoting many
        ;; children into a narrow gap is its own exhaustion vector, so on the
        ;; storage cliff complete-parent rebalance the merged sibling order
        ;; (left siblings ++ promoted kids ++ right siblings).
        (condition-case nil
            (let ((lo left))
              (dolist (k kids)
                (let ((nkey (dorg-sortkey-midpoint-limited lo right)))
                  (setf (dorg-heading-sort-key k) nkey)
                  (setq lo nkey))))
          (dorg-sortkey-exhausted
           (dorg-mutate--ps-rebalance-order
            ps (append
                (cl-remove-if-not
                 (lambda (s) (string< (dorg-heading-sort-key s) hkey)) siblings)
                kids
                (cl-remove-if
                 (lambda (s) (string< (dorg-heading-sort-key s) hkey)) siblings)))))
        (remhash id hash)
        (push id (dorg-mutate--ps-deleted ps))))))

(defun dorg-mutate--ps-find-properties (heading)
  "Return HEADING's local identity/property drawer, or nil.
Delegates to the one central resolver, `dorg-model--local-identity-drawer',
so property put/delete patches target the same
drawer org reads back as the heading's own — the property-typed PROPERTIES
drawer before any non-planning body item.  A generic body `:PROPERTIES:'
block (raw lines) is never returned, so property patches never rewrite it into
a key/value drawer and drop its lines."
  (dorg-model--local-identity-drawer (dorg-heading-items heading)))

(defun dorg-mutate--ps-leading-planning-count (items)
  "Return the count of leading `dorg-planning-item's in ITEMS."
  (let ((n 0))
    (catch 'done
      (dolist (it items)
        (if (cl-typep it 'dorg-planning-item) (cl-incf n) (throw 'done n))))
    n))

(defun dorg-mutate--ps-put-property (ps patch &optional batch-context)
  "Apply typed property-put PATCH to PS."
  (let* ((target (plist-get patch :target))
         (address (dorg-mutate--patch-address patch))
         (host (dorg-mutate--patch-tget target :host))
         (h (dorg-mutate--ps-heading ps address patch))
         (key (dorg-mutate--patch-tget target :name))
         (val (plist-get patch :value))
         (drawer (dorg-mutate--ps-find-properties h)))
    (if drawer
        (let ((cell (assoc key (dorg-drawer-item-properties drawer))))
          (if cell
              (setcdr cell val)
            (setf (dorg-drawer-item-properties drawer)
                  (append (dorg-drawer-item-properties drawer)
                          (list (cons key val))))))
      ;; No LOCAL property drawer yet: insert one after any leading planning
      ;; line, before other content, per Org convention.  A generic body
      ;; `:PROPERTIES:' block is NOT the local drawer, so it is left intact
      ;; here and a fresh identity/property drawer is created for the heading.
      (let* ((items (dorg-heading-items h))
             (idx (dorg-mutate--ps-leading-planning-count items))
             (new (dorg-drawer-item :name "PROPERTIES" :post-blank 0
                                     :properties (list (cons key val)))))
        (dorg-mutate--item-batch-context-shift-items
         batch-context host idx 1)
        (setf (dorg-heading-items h)
              (append (cl-subseq items 0 idx) (list new) (nthcdr idx items)))))
    (dorg-heading-compute-items-hash h)
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-delete-property (ps patch &optional batch-context)
  "Apply typed property-delete PATCH to PS (idempotent)."
  (let* ((target (plist-get patch :target))
         (address (dorg-mutate--patch-address patch))
         (host (dorg-mutate--patch-tget target :host))
         (h (dorg-mutate--ps-heading ps address patch))
         (key (dorg-mutate--patch-tget target :name))
         (drawer (dorg-mutate--ps-find-properties h)))
    (when drawer
      (setf (dorg-drawer-item-properties drawer)
            (cl-remove key (dorg-drawer-item-properties drawer)
                       :key #'car :test #'equal))
      (when (null (dorg-drawer-item-properties drawer))
        (let* ((items (dorg-heading-items h))
               (idx (cl-position drawer items :test #'eq)))
          (when idx
            (dorg-mutate--item-batch-context-remove-items
             batch-context host (list idx)))
          (setf (dorg-heading-items h)
                (cl-remove drawer items))))
      (dorg-heading-compute-items-hash h))
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-update-planning (ps patch &optional batch-context)
  "Apply typed planning-update PATCH to PS.
Replaces the heading's planning line with the supplied fields; an
all-nil planning request removes the planning line entirely."
  (let* ((target (plist-get patch :target))
         (address (dorg-mutate--patch-address patch))
         (host (dorg-mutate--patch-tget target :host))
         (h (dorg-mutate--ps-heading ps address patch))
         (s (plist-get patch :scheduled))
         (d (plist-get patch :deadline))
         (c (plist-get patch :closed))
         (planning (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item))
                               (dorg-heading-items h))))
    (cond
     ((and (null s) (null d) (null c))
      (when planning
        (let* ((items (dorg-heading-items h))
               (idx (cl-position planning items :test #'eq)))
          (when idx
            (dorg-mutate--item-batch-context-remove-items
             batch-context host (list idx)))
          (setf (dorg-heading-items h)
                (cl-remove planning items)))))
     (planning
      (setf (dorg-planning-item-scheduled planning) s)
      (setf (dorg-planning-item-deadline planning) d)
      (setf (dorg-planning-item-closed planning) c))
     (t
      ;; Planning is the first line of a heading's section.
      (dorg-mutate--item-batch-context-shift-items
       batch-context host 0 1)
      (setf (dorg-heading-items h)
            (cons (dorg-planning-item :post-blank 0 :scheduled s
                                      :deadline d :closed c)
                  (dorg-heading-items h)))))
    (dorg-heading-compute-items-hash h)
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--item-patch-host (patch)
  "Return the host heading address for typed item PATCH."
  (dorg-mutate--patch-address patch))

(defun dorg-mutate--ps-item-patch (ps patch &optional batch-context)
  "Apply typed item PATCH to its host heading in PS.
Delegates to the source-neutral pure item core, which deep-copies the
heading with items applied and items-hash refreshed."
  (let* ((host (dorg-mutate--item-patch-host patch))
         (h (dorg-mutate--ps-heading ps host patch))
         (new (dorg-mutate--apply-item-patch-to-heading
               h patch batch-context)))
    (puthash (dorg-heading-id h) new (dorg-mutate--ps-headings ps))
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-move-subtree (ps patch source &optional batch-context)
  "Apply typed subtree-move PATCH to PS.
A cross-source destination is out of scope for the plain-org executor
and signals `unsupported'; a same-source move reparents the subtree."
  (when (and (plist-get patch :to-source)
             (dorg-mutate--cross-source-p source patch))
    (dorg-mutate--raise 'unsupported
                        "cross-source typed move is not supported by plain-org"
                        :operation 'move :source-kind 'plain-org))
  (dorg-mutate--ps-move ps patch batch-context))

(defun dorg-mutate--ps-apply-patch (ps patch source &optional batch-context)
  "Apply one typed PATCH to working model PS for SOURCE."
  (pcase (dorg-mutate--patch-signature patch)
    (`(insert :tree)     (dorg-mutate--ps-insert-heading ps patch batch-context))
    (`(update :heading)  (dorg-mutate--ps-update-heading ps patch))
    (`(move :subtree)    (dorg-mutate--ps-move-subtree ps patch source batch-context))
    (`(delete :heading)  (dorg-mutate--ps-delete-heading ps patch))
    (`(delete :subtree)  (dorg-mutate--ps-delete-heading ps patch))
    (`(put :property)    (dorg-mutate--ps-put-property ps patch batch-context))
    (`(delete :property) (dorg-mutate--ps-delete-property ps patch batch-context))
    (`(update :planning) (dorg-mutate--ps-update-planning ps patch batch-context))
    ((or `(replace :keyword) `(replace :keywords) `(insert :keywords)
         `(delete :keyword) `(delete :keywords) `(insert :texts)
         `(replace :text) `(replace :items) `(insert :drawer-lines))
     (dorg-mutate--ps-item-patch ps patch batch-context))
    (_ (dorg-mutate--raise 'unsupported
                           "executor cannot apply typed patch"
                           :operation (plist-get patch :op)
                           :target-kind (car (plist-get patch :target))
                           :source-kind (dorg-mutate-source-kind source)))))

(defun dorg-mutate--check-base-revision (source options)
  "Signal `stale' when OPTIONS' `:base-revision' no longer matches SOURCE."
  (let ((supplied (plist-get options :base-revision)))
    (when supplied
      (let ((current (dorg-mutate--plain-org-revision source)))
        (when (dorg-mutate--revision-stale-p supplied current)
          (dorg-mutate--raise
           'stale
           "plain-org source changed since :base-revision was captured"
           :source-kind 'plain-org :base-revision supplied :revision current))))
    supplied))

;;; ─── Faithful source-preserving surgical executor ─────────────────────
;;
;; A FAITHFUL plain-org binding owns identity in the FILE and must round-trip
;; byte-for-byte with an in-place
;; `org-entry-put'.  So the faithful path does NOT re-render the tree; it
;; edits the buffer surgically with org's own primitives, touching only the
;; exact bytes each patch changes.  Every untouched byte — tag column, keyword
;; case, body blank runs, sibling headings — therefore stays original.
;;
;; Contract: (1) resolve every patch's external-key target against a fresh parse
;; of the buffer and validate every patch is supported BEFORE any
;; edit; (2) snapshot the buffer; (3) apply the surgical edits; (4) restore the
;; snapshot on any mid-batch failure — preserving the "missing target leaves the
;; source unchanged" atomicity guarantee.  Property and keyword/text/item
;; patches on an identified heading are supported surgically.  An exact boolean
;; COMMENTED update is the sole whole-heading exception and edits only the
;; literal `COMMENT ' span.  One explicit-commented constructor cell appends a
;; child under a concrete heading; omitted-commented, root, anchor, and broader
;; typed constructors remain refused.  General heading-line/tree editing stays
;; parked under tracked-regression, tracked-regression, and tracked-regression  All other
;; heading updates/deletes, subtree moves, planning updates, and root-host
;; variants have no in-place editor and FAIL CLOSED `unsupported' before any
;; write — there is no whole-tree fallback for plain-org.

(defun dorg-mutate--source-preserving-heading-key (key)
  "Return the local value of PROPERTIES KEY at point (a heading), or nil."
  (org-entry-get (point) key))

(defun dorg-mutate--source-preserving-find-markers (host)
  "Return markers at the heading(s) HOST addresses in the current org buffer.
HOST is an external-key host (:extern :key K :value V) resolved by scanning
local PROPERTIES; an integer id resolved via a local DORG_ID property.  The
buffer must be org-mode and accessible (widened by the caller)."
  (let ((markers nil))
    (cond
     ((dorg-mutate--typed-external-address-p host)
      (let ((key (plist-get (cdr host) :key))
            (value (plist-get (cdr host) :value)))
        (org-map-entries
         (lambda ()
           (when (equal value (dorg-mutate--source-preserving-heading-key key))
             (push (point-marker) markers))))))
     ((integerp host)
      (org-map-entries
       (lambda ()
         (when (equal (number-to-string host)
                      (dorg-mutate--source-preserving-heading-key dorg-id-property))
           (push (point-marker) markers)))))
     (t (dorg-mutate--raise
         'invalid-target
         "faithful apply addresses headings by external key, not :root"
         :target host)))
    (nreverse markers)))

(defun dorg-mutate--buffer-has-idless-heading-p ()
  "Non-nil when the current org buffer contains a heading with NO DORG_ID.
This is the idless/bootstrap boundary (A7): a `dorg' identity id target matching
no heading is a BOOTSTRAP attempt when the source contains an idless heading (an
id it cannot address), versus an ordinary MISSING on a source of only identified
headings."
  (catch 'idless
    (org-map-entries
     (lambda () (unless (org-entry-get (point) dorg-id-property) (throw 'idless t))))
    nil))

(defun dorg-mutate--source-preserving-resolve (host patch dorg-p)
  "Resolve HOST to the unique heading marker for PATCH, or signal deterministically.
Zero matches -> `invalid-target' `:detail missing' — EXCEPT, under a `dorg' identity
policy (DORG-P non-nil), an integer id HOST on a source that contains an idless
heading, which is a bootstrap attempt and signals `:detail id-bootstrap-unsupported'
\(A7; distinct from a plain miss on a source of only identified headings, and from a
`(:property K)' source where an integer miss is an ordinary `missing').  DORG-P is
threaded ONCE from `dorg-mutate--source-preserving-run' (the resolved source policy), never
re-derived per patch.  More than one -> `invalid-target' `:detail ambiguous'."
  (let ((markers (dorg-mutate--source-preserving-find-markers host)))
    (cond
     ((null markers)
      (dorg-mutate--raise 'invalid-target
                          (format "no heading addressed by %S in source" host)
                          :operation (plist-get patch :op) :target host
                          :detail (if (and dorg-p (integerp host)
                                           (dorg-mutate--buffer-has-idless-heading-p))
                                      'id-bootstrap-unsupported
                                    'missing)))
     ((cdr markers)
      (dorg-mutate--raise 'invalid-target
                          (format "%d headings match %S; identity key must be unique"
                                  (length markers) host)
                          :operation (plist-get patch :op) :target host :detail 'ambiguous))
     (t (car markers)))))

(defun dorg-mutate--source-preserving-validate-patch (patch)
  "Signal `unsupported'/`no-source-preserving-editor' unless PATCH has a
source-preserving editor at its C3 host variant.  The `:variant' carried on the
error is the authoritative C3 cell — nil for a whole-verb collapse, =(:host
root)= for a surgical verb addressed at the level-0 root.  Checked in Phase 1,
before any write, so plan and apply refuse in lockstep through the complete
record from `dorg-mutate--c3-refusal'."
  (when-let ((refusal (dorg-mutate--c3-refusal patch)))
    (dorg-mutate--raise
     'unsupported
     (plist-get refusal :message)
     :source-kind 'plain-org
     :operation (plist-get refusal :operation)
     :target-kind (plist-get refusal :target-kind)
     :detail (plist-get refusal :reason)
     :variant (plist-get refusal :variant))))

(defun dorg-mutate--source-preserving-patch-host (patch)
  "Return the external-key/id host a faithful PATCH addresses."
  (dorg-mutate--patch-address patch))

(defun dorg-mutate--source-preserving-patch-new-items
    (patch old-items &optional batch-context)
  "Return the post-PATCH item list from OLD-ITEMS via the pure core.
Selector/ordinal resolution errors signal before any buffer edit, so a
failed patch makes no change."
  (dorg-mutate--apply-item-patch old-items patch batch-context))

(defun dorg-mutate--item-content-equal (a b)
  "Return non-nil when items A and B have identical structured content.
Uses `dorg-item-content-string' (post-blank included), so a value change or a
post-blank change counts as different — the splice re-renders such an item."
  (equal (dorg-item-content-string a) (dorg-item-content-string b)))

(defun dorg-mutate--source-preserving-render-item-text (item)
  "Render ITEM to faithful source: its line(s) + terminating newline + its
post-blank blank run.  Never injects `:DORG_ID:'/synthetic PROPERTIES — a
faithful file owns identity in its own bytes."
  (let ((text (dorg-model--render-item item nil))
        (pb (dorg-item-post-blank item)))
    (concat text
            (if (string-suffix-p "\n" text) "" "\n")
            (make-string pb ?\n))))

(defun dorg-mutate--source-preserving-render-items (items)
  "Concatenate `dorg-mutate--source-preserving-render-item-text' over ITEMS."
  (mapconcat #'dorg-mutate--source-preserving-render-item-text items ""))

(defun dorg-mutate--source-preserving-constructor-property-values (patch key)
  "Return every public constructor property value for KEY in PATCH."
  (let (values)
    (dolist (item (plist-get (plist-get patch :heading) :items))
      (when (and (eq (plist-get item :kind) 'drawer)
                 (equal (plist-get item :name) "PROPERTIES"))
        (dolist (cell (plist-get item :properties))
          (when (equal (car cell) key)
            (push (cdr cell) values)))))
    (nreverse values)))

(defun dorg-mutate--source-preserving-constructor-identity-guard
    (patch source pending-identities)
  "Validate PATCH's new identity under SOURCE before any byte edit.
PENDING-IDENTITIES records property identities already claimed by earlier
constructors in this batch.  Return the new (KEY . VALUE) pair, or nil for a
`dorg' identity whose DORG_ID is minted only during phase 2."
  (let ((policy (dorg-mutate--source-identity source)))
    (when (and (consp policy) (eq (car policy) :property))
      (let* ((key (cadr policy))
             (values
              (dorg-mutate--source-preserving-constructor-property-values
               patch key)))
        (unless (= (length values) 1)
          (dorg-mutate--raise
           'invalid-target
           (format "faithful constructor needs exactly one identity property `%s'" key)
           :operation 'insert :target-kind :tree :key key
           :detail 'identity-carrier-required))
        (let* ((value (car values))
               (identity (cons key value))
               (matches
                (dorg-mutate--source-preserving-find-markers
                 (list :extern :key key :value value))))
          (when (or matches (gethash identity pending-identities))
            (dorg-mutate--raise
             'invalid-target
             (format "faithful constructor identity `%s=%s' already exists"
                     key value)
             :operation 'insert :target-kind :tree :key key :value value
             :detail 'duplicate-created-identity))
          identity)))))

(defun dorg-mutate--source-preserving-constructor-heading
    (patch level id)
  "Return a new heading model for public constructor PATCH at LEVEL and ID."
  (let ((payload (plist-get patch :heading)))
    (make-dorg-heading
     :id id :level level
     :todo (plist-get payload :todo)
     :priority (plist-get payload :priority)
     :commented (and (plist-get payload :commented) t)
     :title (plist-get payload :title)
     :post-blank 0
     :tags (plist-get payload :tags)
     :items (mapcar (lambda (value)
                      (dorg-mutate--item-from-value value 0))
                    (plist-get payload :items)))))

(defun dorg-mutate--source-preserving-insert-commented-child
    (patch parent-marker source)
  "Append one public COMMENTED constructor PATCH below PARENT-MARKER.
Return the physically written DORG_ID, or nil for a property-identity faithful
file.  The C3 cell and phase-1 identity guard have already accepted PATCH."
  (let* ((dorg-p (dorg-mutate--identity-dorg-p source))
         (id (and dorg-p (dorg-id-generate)))
         (parent-level (save-excursion
                         (goto-char parent-marker)
                         (org-outline-level)))
         (heading
          (dorg-mutate--source-preserving-constructor-heading
           patch (1+ parent-level) id))
         (text
          (if dorg-p
              ;; DOrg identity is injected into the same local PROPERTIES
              ;; drawer as any public property clauses.
              (dorg-model-render-heading-section heading)
            ;; A property-identity faithful file owns only its declared key;
            ;; never fabricate a DORG_ID merely because the model has an id slot.
            (concat (dorg-model--render-heading heading) "\n"
                    (dorg-mutate--source-preserving-render-items
                     (dorg-heading-items heading))))))
    (goto-char parent-marker)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert text)
    id))

(defun dorg-mutate--source-preserving-identity-guard (new-items _marker source patch)
  "Signal `invalid-target' if NEW-ITEMS would DIRECTLY change SOURCE's identity.
The resolved identity key (DORG_ID under `dorg', K under =(:property K)=) lives
in the heading's PROPERTIES drawer, which the executor preserves OUT-OF-BAND
across body/keyword/item splices (A8) — so a payload that simply OMITS the key
is fine (it is neither the carrier of the identity nor a request to remove it).
What is guarded is a DIRECT identity mutation smuggled through an item payload:
a PROPERTIES drawer item that sets the identity key to a DIFFERENT value, or
DUPLICATES it, fails closed before any edit.  `none' (no protected key, never
writes) passes."
  (let ((idkey (dorg-mutate--identity-protected-key source)))
    (when (and idkey
               ;; The heading's authoritative PROPERTIES drawer is preserved
               ;; OUT-OF-BAND, so a body payload must carry NO PROPERTIES drawer
               ;; at all — FIELD-INDEPENDENT.  One holding the identity key would
               ;; smuggle/duplicate identity; one holding only non-identity
               ;; fields would render a SECOND metadata drawer after the
               ;; preserved one.  Both fail closed.  Other non-metadata named
               ;; drawers are ordinary content and pass.
               (cl-some (lambda (it)
                          (and (cl-typep it 'dorg-drawer-item)
                               (equal (dorg-drawer-item-name it) "PROPERTIES")))
                        new-items))
      (dorg-mutate--raise
       'invalid-target
       (format "faithful body payload must not carry a PROPERTIES drawer; identity `%s' is preserved out-of-band"
               idkey)
       :operation (plist-get patch :op) :key idkey :detail 'identity-brick))))

(defun dorg-mutate--source-preserving-property-identity-guard (patch marker source)
  "Signal when property PATCH would brick SOURCE's identity key.
A faithful binding addressed by an EXTERNAL identity key (SOURCE's identity-key,
e.g. BEAD_ID) is byte-load-bearing: `delete-property' on it, or `set-property'
changing it to a DIFFERENT value, severs the heading's addressability.  Setting
it to its SAME value is a no-op and allowed; a non-identity property is
untouched (the guard is targeted).  Checked in Phase 1, before any write."
  (let* ((idkey (dorg-mutate--identity-protected-key source))
         (target (plist-get patch :target))
         (key (dorg-mutate--patch-tget target :name)))
    (when (and idkey (equal key idkey))
      (pcase (dorg-mutate--patch-signature patch)
        (`(delete :property)
         (dorg-mutate--raise
          'invalid-target
          (format "faithful write would delete identity key `%s'" idkey)
          :operation 'delete :target-kind :property
          :key idkey :detail 'identity-brick))
        (`(put :property)
         (let ((idval (save-excursion (goto-char marker)
                                      (org-entry-get (point) idkey))))
           (unless (equal (plist-get patch :value) idval)
             (dorg-mutate--raise
              'invalid-target
              (format "faithful write would change identity key `%s'" idkey)
              :operation 'put :target-kind :property
              :key idkey :detail 'identity-brick))))))))

(defun dorg-mutate--source-preserving-region-replace (items-begin section-end new-items)
  "Replace the whole items region [ITEMS-BEGIN, SECTION-END) with NEW-ITEMS.
Used for typed whole-items replacement — the caller's sequence is authoritative, so the
region is re-rendered wholesale (the heading line and every other heading stay
verbatim)."
  (delete-region items-begin section-end)
  (goto-char items-begin)
  (insert (dorg-mutate--source-preserving-render-items new-items)))

(defun dorg-mutate--source-preserving-diff-splice (spans new-items items-begin section-end)
  "Minimally splice OLD (SPANS) -> NEW-ITEMS, preserving unchanged items' bytes.
SPANS is `dorg-parse-section-item-spans' output aligned with the old items.
The longest content-equal prefix and suffix are left untouched (their original
author formatting survives byte-for-byte); only the differing middle is
replaced with freshly-rendered NEW-ITEMS.  ITEMS-BEGIN/SECTION-END bound an
empty-old or all-changed splice."
  (let* ((old-items (mapcar #'car spans))
         (o (length old-items))
         (n (length new-items))
         (p 0) (s 0))
    (while (and (< p o) (< p n)
                (dorg-mutate--item-content-equal (nth p old-items) (nth p new-items)))
      (setq p (1+ p)))
    (while (and (< s (- o p)) (< s (- n p))
                (dorg-mutate--item-content-equal (nth (- o 1 s) old-items)
                                                 (nth (- n 1 s) new-items)))
      (setq s (1+ s)))
    (let* ((mid-new (cl-subseq new-items p (- n s)))
           ;; spans are contiguous, so [prev-prefix.end, first-suffix.begin) is
           ;; exactly the changed middle's bytes.
           (region-begin (if (> p 0) (nth 3 (nth (1- p) spans)) items-begin))
           (region-end (if (> s 0) (nth 1 (nth (- o s) spans)) section-end)))
      (delete-region region-begin region-end)
      (goto-char region-begin)
      (insert (dorg-mutate--source-preserving-render-items mid-new)))))

(defconst dorg-mutate--source-preserving-item-patches
  (cl-remove-if (lambda (signature)
                  (eq (cadr signature) :property))
                dorg-mutate--c3-surgical-patches)
  "Faithful typed signatures applied by item-span splice.")

(defun dorg-mutate--source-preserving-scan (marker)
  "Return (ITEMS-BEGIN SECTION-END SPANS) for the heading at MARKER.
SPANS is `dorg-parse-section-item-spans' output; ITEMS-BEGIN is the position
after the heading line, SECTION-END the next heading (or point-max).  Positions
are buffer positions valid only until the next edit — recomputed per patch."
  (save-excursion
    (goto-char marker)
    (let* ((hbeg (point))
           (section-end (save-excursion (outline-next-heading) (point)))
           (items-begin (save-excursion (forward-line 1)
                                        (min (point) section-end)))
           (spans (save-restriction
                    (narrow-to-region hbeg section-end)
                    (let ((section (org-element-map (org-element-parse-buffer)
                                       'section #'identity nil t)))
                      (and section (dorg-parse-section-item-spans section))))))
      (list items-begin section-end spans))))

(defun dorg-mutate--source-preserving-body-scan (marker _source)
  "Return (BODY-BEGIN SECTION-END BODY-SPANS) for the heading at MARKER.
The heading's LEADING METADATA — any planning line(s) plus the PROPERTIES
drawer, which org fixes to the top of the section — is OUT-OF-BAND: it is
preserved verbatim in [items-begin, BODY-BEGIN) and never spliced.  BODY-BEGIN
is the position AFTER the property drawer's `:END:' line, located PHYSICALLY via
`org-get-property-block' so it is correct even when the drawer parses to NO item
span (a DORG_ID-only drawer under `dorg') and even when a planning line PRECEDES
the drawer (where a span-filter would leave a non-contiguous body and delete the
metadata between).  BODY-SPANS are the item spans at/after BODY-BEGIN.  With no
property drawer the pre-existing items-begin body boundary is kept."
  (let* ((scan (dorg-mutate--source-preserving-scan marker))
         (items-begin (nth 0 scan))
         (section-end (nth 1 scan))
         (body-begin (save-excursion
                       (goto-char marker)
                       (let ((pblock (org-get-property-block)))
                         (if pblock
                             (progn (goto-char (cdr pblock)) (forward-line 1) (point))
                           items-begin))))
         (body-spans (cl-remove-if (lambda (sp) (< (nth 1 sp) body-begin))
                                   (nth 2 scan))))
    (list body-begin section-end body-spans)))

(defun dorg-mutate--source-preserving-splice-patch
    (patch marker source &optional batch-context)
  "Apply a validated item PATCH at MARKER by byte-faithful splice.
Parses the heading's BODY items fresh (positions shift as earlier patches edit
the buffer) and splices only the body region AFTER the out-of-band identity
drawer — a whole body-region replace for typed items replacement, a minimal
content-diff middle splice otherwise.  The identity drawer's bytes are never
touched.  The guard and selector/ordinal resolution already ran in Phase 1."
  (let* ((bscan (dorg-mutate--source-preserving-body-scan marker source))
         (body-begin (nth 0 bscan))
         (section-end (nth 1 bscan))
         (body-spans (nth 2 bscan))
         (old-items (mapcar #'car body-spans))
         (new-items (dorg-mutate--source-preserving-patch-new-items
                     patch old-items batch-context)))
    (if (equal (dorg-mutate--patch-signature patch) '(replace :items))
        (dorg-mutate--source-preserving-region-replace body-begin section-end new-items)
      (dorg-mutate--source-preserving-diff-splice body-spans new-items body-begin section-end))))

(defun dorg-mutate--source-preserving-update-commented (patch)
  "Apply exact boolean COMMENTED PATCH at the current heading.
Only `org-toggle-comment' may edit the heading, and only when the current local
COMMENT state differs from the requested value.  Org inserts or removes the
literal `COMMENT ' span without realigning tags or rewriting other bytes."
  (let* ((desired (plist-get (plist-get patch :fields) :commented))
         (element (org-element-at-point))
         (current (and (eq (org-element-type element) 'headline)
                       (org-element-property :commentedp element)
                       t)))
    (unless (eq desired current)
      (org-toggle-comment))))

(defun dorg-mutate--source-preserving-apply-patch
    (patch marker source &optional batch-context)
  "Apply a validated faithful PATCH at heading MARKER.
Return a newly written DORG_ID for the bounded constructor, otherwise nil."
  (goto-char marker)
  (pcase (dorg-mutate--patch-signature patch)
    (`(put :property)
     (org-entry-put (point)
                    (dorg-mutate--patch-tget (plist-get patch :target) :name)
                    (plist-get patch :value))
     nil)
    (`(delete :property)
     (org-entry-delete
      (point) (dorg-mutate--patch-tget (plist-get patch :target) :name))
     nil)
    (`(update :heading)
     (dorg-mutate--source-preserving-update-commented patch)
     nil)
    (`(insert :tree)
     (dorg-mutate--source-preserving-insert-commented-child
      patch marker source))
    (_
     (dorg-mutate--source-preserving-splice-patch
      patch marker source batch-context)
     nil)))

(defun dorg-mutate--source-preserving-plan (patches source)
  "Validate, resolve, and simulate faithful PATCHES without writing.
Return a resolved plan of (PATCH . MARKER) entries.  Phase 1 validates the
ENTIRE batch before any write: each patch's kind is checked, its target
resolved, and item patches are simulated per heading through the shared pure
item transform."
  (save-restriction
    (widen)
    (let ((sim (make-hash-table :test 'eql))       ; heading position -> items
          (pending-identities (make-hash-table :test 'equal))
          (plan nil)
          ;; Resolve the identity policy ONCE (not per patch): only a `dorg' policy
          ;; makes an integer-id miss on an idless source a bootstrap attempt.
          (dorg-p (dorg-mutate--identity-dorg-p source)))
      ;; ── Phase 1: validate + resolve + simulate + guard, no writes ──
      (let ((phase1-context (dorg-mutate--item-batch-context)))
        (dolist (patch patches)
          (dorg-mutate--source-preserving-validate-patch patch)
          (let ((marker (dorg-mutate--source-preserving-resolve
                         (dorg-mutate--source-preserving-patch-host patch)
                         patch dorg-p)))
            (when (member (dorg-mutate--patch-signature patch)
                          dorg-mutate--source-preserving-item-patches)
              (let* ((key (marker-position marker))
                     ;; Sentinel default so a cached EMPTY item list (nil) is a
                     ;; valid state — never mistaken for "not yet cached", which
                     ;; would re-parse pre-batch bytes and desync the simulation.
                     (cached (gethash key sim 'dorg-mutate--uncached))
                     ;; BODY items only — the identity drawer is out-of-band, so
                     ;; the simulation and guard never see it and the body payload
                     ;; must introduce zero identity.
                     (cur (if (eq cached 'dorg-mutate--uncached)
                              (mapcar #'car (nth 2 (dorg-mutate--source-preserving-body-scan
                                                    marker source)))
                            cached))
                     ;; The pure transform raises occurrence errors here,
                     ;; before any write.
                     (new (dorg-mutate--source-preserving-patch-new-items
                           patch cur phase1-context)))
                (dorg-mutate--source-preserving-identity-guard
                 new marker source patch)
                (puthash key new sim)))
            ;; Property patches carry identity too: a faithful binding's external
            ;; key must not be changed/deleted via set/delete-property.
            (when (eq (cadr (dorg-mutate--patch-signature patch)) :property)
              (dorg-mutate--source-preserving-property-identity-guard
               patch marker source))
            (when (equal (dorg-mutate--patch-signature patch) '(insert :tree))
              (when-let ((identity
                          (dorg-mutate--source-preserving-constructor-identity-guard
                           patch source pending-identities)))
                (puthash identity t pending-identities)))
            (push (cons patch marker) plan))))
      (nreverse plan))))

(defun dorg-mutate--source-preserving-plan-changed (plan)
  "Return changed host tokens from resolved source-preserving PLAN."
  (delete-dups
   (mapcar (lambda (entry)
             (dorg-mutate--source-preserving-patch-host (car entry)))
           plan)))

(defun dorg-mutate--source-preserving-apply-plan (plan source)
  "Apply resolved source-preserving PLAN in the current buffer."
  (save-restriction
    (widen)
    ;; ── Phase 2: apply.  Markers stay valid across localized edits ──
    (let ((phase2-context (dorg-mutate--item-batch-context))
          created)
      (dolist (entry plan)
        (when-let ((id
                    (dorg-mutate--source-preserving-apply-patch
                     (car entry) (cdr entry) source phase2-context)))
          (push id created)))
      (list :created (nreverse created)
            :changed (dorg-mutate--source-preserving-plan-changed plan)))))

(defun dorg-mutate--source-preserving-run (patches source)
  "Validate, resolve, and apply faithful PATCHES in the current buffer.
Runs entirely inside the caller's snapshot guard.  Returns
`(:created IDS :changed HOSTS)', where IDS contains only physically written
DORG_ID values and HOSTS are the changed target host tokens."
  (dorg-mutate--source-preserving-apply-plan
   (dorg-mutate--source-preserving-plan patches source)
   source))

;;; ─── Dry-run projections (D5) ─────────────────────────────────────────
;; A dry-run previews, per target, the exact owned source region BEFORE and
;; AFTER the source-preserving edit.  Computed on an ISOLATED copy of the source
;; text run through the REAL faithful executor, so the preview is byte-identical
;; to what apply would write and the real source is never touched.

(defun dorg-mutate--plain-org-source-readable-p (source)
  "Non-nil when plain-org SOURCE has a live buffer or a readable file to preview."
  (let ((buf (dorg-mutate-source-buffer source))
        (file (dorg-mutate-source-file source)))
    (or (buffer-live-p buf) (and file (file-readable-p file)))))

(defun dorg-mutate--plain-org-source-text (source)
  "Return the current source text of plain-org SOURCE (its live buffer or file)."
  (let ((buf (dorg-mutate-source-buffer source))
        (file (dorg-mutate-source-file source)))
    (cond
     ((buffer-live-p buf)
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))
     ((and file (file-readable-p file))
      (with-temp-buffer (insert-file-contents file)
                        (buffer-substring-no-properties (point-min) (point-max))))
     (t (dorg-mutate--raise 'invalid-target
                            "plain-org source has no readable buffer or file"
                            :source-kind 'plain-org)))))

(defun dorg-mutate--source-assert-unique-ids (source)
  "Signal `invariant'/`duplicate-heading-id' if plain-org SOURCE has duplicate ids.
Parses SOURCE once — its live buffer when present, else a temp buffer seeded
from `dorg-mutate--plain-org-source-text' — and delegates to
`dorg-mutate--result-assert-unique-ids'.  The shared pre-write guard for the
surgical executor; run AFTER the identity=none refusal and the `:base-revision'
CAS gate so their refusal precedence is preserved.  Returns SOURCE."
  (let ((buf (dorg-mutate-source-buffer source)))
    (dorg-mutate--result-assert-unique-ids
     (if (buffer-live-p buf)
         ;; WIDEN before parsing: the surgical executor widens before it
         ;; mutates, so a narrowed accessible region must not hide a
         ;; collision from the guard.
         (with-current-buffer buf
           (save-restriction (widen) (dorg-parse-buffer buf nil)))
       (with-temp-buffer
         (insert (dorg-mutate--plain-org-source-text source))
         (let ((org-inhibit-startup t)) (org-mode))
         (dorg-parse-buffer (current-buffer) nil)))))
  source)

(defun dorg-mutate--source-has-idless-heading-p (source)
  "Non-nil when plain-org SOURCE contains a heading with NO DORG_ID.
The API-side idless/bootstrap boundary (A7) for direct `(key V)' under `dorg':
a key that resolves to no id-bearing heading is a BOOTSTRAP attempt when SOURCE
contains an idless heading, versus an ordinary missing on a source of only
identified headings.  See `dorg-mutate--buffer-has-idless-heading-p'."
  (let ((s (dorg-mutate--resolve-source source)))
    (and (eq (dorg-mutate-source-kind s) 'plain-org)
         (let ((buf (dorg-mutate-source-buffer s)))
           (if (buffer-live-p buf)
               (with-current-buffer buf (dorg-mutate--buffer-has-idless-heading-p))
             (with-temp-buffer
               (insert (dorg-mutate--plain-org-source-text s))
               (org-mode)
               (dorg-mutate--buffer-has-idless-heading-p)))))))

(defun dorg-mutate--source-preserving-region-string (host)
  "Return the owned source region string of HOST in the current org buffer.
The region runs from the heading line through the byte before the next heading
\(the full section: heading, leading metadata, and body).  Returns nil when HOST
addresses no heading."
  (let ((m (car (dorg-mutate--source-preserving-find-markers host))))
    (and m (save-excursion
             (goto-char m)
             (buffer-substring-no-properties
              (point) (save-excursion (outline-next-heading) (point)))))))

(defun dorg-mutate--projection-change-kind (host patches)
  "Return the D5 change kind for the first PATCH addressing HOST."
  (pcase (cl-loop for patch in patches
                  when (equal (dorg-mutate--source-preserving-patch-host patch)
                              host)
                  return (cadr (dorg-mutate--patch-signature patch)))
    (:property 'property)
    ((or :keyword :keywords) 'keyword)
    ((or :text :texts :items :drawer-lines) 'item)
    (_ 'body)))

(defun dorg-mutate--plain-org-projections (source patches)
  "Return an alist (HOST . PLIST) previewing typed PATCHES on plain-org
SOURCE, where PLIST is (:change-kind K :before STR :after STR :provision nil).
Computed on an ISOLATED copy of the source text run through the real faithful
executor, so the real SOURCE is never written.  HOSTS appear in first-seen PATCHES
order; the caller reorders/tokenizes.  Signals if any patch is unsupported (the
caller restricts this to supported plain-org dry-runs)."
  (setq patches (dorg-mutate--validate-patches patches))
  (let ((text (dorg-mutate--plain-org-source-text source))
        (hosts nil))
    (dolist (patch patches)
      (let ((h (dorg-mutate--source-preserving-patch-host patch)))
        (unless (member h hosts) (setq hosts (append hosts (list h))))))
    (with-temp-buffer
      (insert text)
      (org-mode)
      (widen)
      (let* ((proxy (dorg-mutate--source-make
                     :kind 'plain-org :buffer (current-buffer)
                     :identity (dorg-mutate-source-identity source)))
             (before (mapcar (lambda (h)
                               (cons h (dorg-mutate--source-preserving-region-string h)))
                             hosts)))
        (dorg-mutate--source-preserving-run patches proxy)
        (mapcar
         (lambda (h)
           (cons h (list :change-kind
                         (dorg-mutate--projection-change-kind h patches)
                         :before (cdr (assoc h before))
                         :after (dorg-mutate--source-preserving-region-string h)
                         :provision nil)))
         hosts)))))

;;; ─── Plain-Org Atomic Persist ─────────────────────────────────────────
;;
;; ONE DOrg-owned persistence primitive backs every plain-org apply that
;; reaches disk through `dorg-api-execute'.  It closes two holes in the
;; legacy `save-buffer' / direct `write-region' path:
;;
;;   CRASH SAFETY   An interrupted writer must never truncate or partially
;;                  replace the source.  The complete result is rendered to
;;                  an O_EXCL temp sibling on the SAME filesystem (so the
;;                  final swap is an atomic `rename'), fsync is requested
;;                  around the write, and on ANY pre-rename error/quit the
;;                  temp is removed and the destination is left byte-identical.
;;
;;   CONTENT CAS    Equal size+mtime+inode is NOT freshness: an equal-stat
;;                  external replacement must not be overwritten by a stale
;;                  in-memory source.  A content FINGERPRINT (sha256 over raw
;;                  bytes) is the compare-and-swap unit — carried on the
;;                  revision AND public `:base' tokens, established from the
;;                  exact bytes read at entry, and rechecked against the
;;                  destination immediately before the rename.
;;
;; Each step is a plain-Lisp seam so a deterministic fault-injection test can
;; `cl-letf' DOrg code (never a native subr — native-comp trampoline hazard).
;; No OS lock is taken; a writer landing between recheck and rename is not
;; excluded, but the destination is always a COMPLETE old-or-new byte image
;; (rename atomicity), never partial.  Cross-process serialization is a layer
;; above DOrg, which DOrg does not learn.

(defvar-local dorg-mutate--plain-org-baseline nil
  "Content fingerprint of the on-disk bytes this visiting buffer is trusted
to be a modification of.  Recorded on every DOrg apply ENTRY over a CLEAN
buffer (including `:save nil') and after every successful commit, so a
mutate-then-save sequence keeps a trustworthy compare-and-swap baseline.  A
MODIFIED buffer with this unset has no trustworthy baseline: its save is
refused `stale'/`baseline-untrusted' rather than adopting a late disk hash for
stale contents.  The symbol `:absent' records that no destination file exists.")

(defun dorg-mutate--persist-fingerprint (bytes)
  "Return the content fingerprint of unibyte BYTES (sha256 hex).
BYTES must be a unibyte (raw) string; callers encode buffer/work text with
`dorg-mutate--persist-encode' first so the hash is over on-disk bytes, not the
multibyte internal representation."
  (secure-hash 'sha256 bytes))

(defun dorg-mutate--persist-file-fingerprint (file)
  "Return the fingerprint of FILE's raw on-disk bytes, or `:absent' when FILE
does not exist.  Reads literally (no decoding) so the hash matches
`dorg-mutate--persist-fingerprint' of the bytes a commit writes."
  (if (file-exists-p file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (dorg-mutate--persist-fingerprint (buffer-string)))
    :absent))

(defun dorg-mutate--persist-coding (coding)
  "Normalize CODING to a concrete on-disk coding system for encode/decode.
A nil, `undecided', or `prefer-utf-8' CODING resolves to `utf-8-unix' — the
DOrg on-disk default — so nil never leaks into `encode-coding-string' as a
no-conversion request (which would mis-handle multibyte content)."
  (let ((cs (or coding 'utf-8-unix)))
    (if (memq (coding-system-base cs) '(undecided prefer-utf-8))
        'utf-8-unix
      cs)))

(defun dorg-mutate--persist-encode (text coding)
  "Encode multibyte TEXT to guaranteed-UNIBYTE raw bytes with a normalized
CODING (see `dorg-mutate--persist-coding').  The result is always unibyte so
`secure-hash' fingerprints the on-disk bytes (not the multibyte internal
representation, which would mis-hash non-ASCII content): `encode-coding-string'
returns the original multibyte string for pure-ASCII input, so an ASCII result
is converted down.  Non-raising: used for freshness tokens and fingerprints,
where a lossy coding must not abort token computation.  The executor's write
path uses `dorg-mutate--persist-encode-faithful' instead, so a coding that
cannot byte-round-trip fails closed BEFORE any write."
  (let ((bytes (encode-coding-string text (dorg-mutate--persist-coding coding))))
    (if (multibyte-string-p bytes) (string-to-unibyte bytes) bytes)))

(defun dorg-mutate--persist-encode-faithful (text coding)
  "Encode TEXT to unibyte raw bytes under a normalized CODING and PROVE the
bytes decode back to TEXT.  Returns the bytes when the coding byte-round-trips
TEXT; otherwise refuses `stale'/`stale-at-entry' — fidelity is unknowable, so
DOrg declines to write rather than silently rewrite unrelated bytes with a lossy
re-encode.  This is the encoder every disk-bound persist path uses."
  (let* ((cs (dorg-mutate--persist-coding coding))
         ;; A coding that cannot represent a char may either substitute it
         ;; (lossy) or signal; treat BOTH as unknowable fidelity.
         (bytes (condition-case nil (dorg-mutate--persist-encode text cs)
                  (error nil))))
    (unless (and bytes (equal text (decode-coding-string bytes cs)))
      (dorg-mutate--raise
       'stale
       "plain-org content is not byte-faithful under its coding system"
       :detail 'stale-at-entry :source-kind 'plain-org :coding cs))
    bytes))

(defun dorg-mutate--persist-file-read (file)
  "Return FILE's raw on-disk bytes from ONE literal read (unibyte string).
The single read that establishes BOTH the commit baseline fingerprint AND the
decoded work text, so no between-read external replacement can desync them (an
ABA replacement cannot commit a mutation derived from bytes B over an equal-
fingerprint A the executor never actually read)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun dorg-mutate--persist-decode-faithful (raw)
  "Decode RAW on-disk bytes to work text, PROVING the EXACT bytes round-trip.
Returns (TEXT . CODING) when `encode-coding-string' of the decoded TEXT under
the detected+normalized CODING reproduces RAW byte-for-byte.  Otherwise refuses
`stale'/`stale-at-entry': a coding whose re-encode drifts from RAW (BOM, mixed
or converted EOL, canonicalized form) makes a surgical re-encode rewrite bytes
outside the requested change, so DOrg declines rather than corrupt untouched
regions.  This is stronger than a text-level `decode(encode TEXT)==TEXT' check,
which a byte-drifting coding can still pass."
  (let* ((text (decode-coding-string raw 'undecided))
         (coding (dorg-mutate--persist-coding last-coding-system-used)))
    (unless (equal raw (dorg-mutate--persist-encode text coding))
      (dorg-mutate--raise
       'stale
       "plain-org file bytes are not byte-faithful under their coding system"
       :detail 'stale-at-entry :source-kind 'plain-org :coding coding))
    (cons text coding)))

(defun dorg-mutate--persist-create-temp (file)
  "Create and return an exclusive temp sibling of FILE for an atomic commit.
FILE is truename-resolved so a symlink destination places the temp in the
TARGET's directory (same filesystem — a `rename' there is atomic).  The temp
is created empty via `make-temp-file' (O_EXCL); consecutive calls for the same
destination return DISTINCT already-existing `.dorg-persist-' siblings, so no
fixed name is available for a concurrent writer to reuse or clobber."
  (let* ((target (file-truename file))
         (dir (file-name-directory target))
         (base (file-name-nondirectory target)))
    (make-temp-file (expand-file-name (concat base ".dorg-persist-") dir))))

(defun dorg-mutate--persist-write-temp (tmp bytes)
  "Write unibyte BYTES to TMP as raw bytes — the complete result, no conversion.
`write-region-inhibit-fsync' is bound around this seam by
`dorg-mutate--plain-org-commit', so the fsync request is observable here."
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil tmp nil 'silent)))

(defun dorg-mutate--persist-rename (tmp target)
  "Atomically rename TMP over TARGET (the destination truename)."
  (rename-file tmp target t))

(defun dorg-mutate--persist-post-rename ()
  "Seam fired inside `dorg-mutate--plain-org-commit' immediately AFTER the
commit point (rename + committed flag set), OUTSIDE `inhibit-quit'.  A no-op in
production; it is the single injectable point at which a deferred interrupt
\(a C-g that became visible after the atomic rename) can surface, so a fault
test can prove the caller reports the commit as landed rather than restoring the
pre-mutation state.")

(defun dorg-mutate--plain-org-commit (file bytes baseline committed)
  "Atomically commit unibyte BYTES to plain-org FILE under a content CAS.
FILE is truename-resolved: a symlink commits to its target and the link is
preserved.  BYTES are written to an O_EXCL temp sibling on the target's
filesystem with fsync requested, the temp inherits the destination's mode
\(the umask default when the destination is absent), the destination
fingerprint is rechecked against BASELINE, and the temp is atomically renamed
over the target under `inhibit-quit'.  On ANY pre-rename error/quit the temp is
removed and the destination is left BYTE-IDENTICAL.  BASELINE is the fingerprint
the caller read (`:absent' for a not-yet-existing file); a mismatch signals
`stale'/`destination-drift'.

COMMITTED is a one-element cell (a mutable list) whose car is set to t inside
the `inhibit-quit' region, IMMEDIATELY after the rename, with no interposing
form that can error or quit before it or before returning.  This is the EXPLICIT
commit point: callers branch on (car COMMITTED), never on a destination==bytes
fingerprint comparison (which a no-op/equal-content mutation would satisfy
BEFORE the rename, misclassifying a pre-rename failure as committed).  Returns
the committed fingerprint."
  (let* ((target (file-truename file))
         (tmp (dorg-mutate--persist-create-temp target))
         (renamed nil))
    (unwind-protect
        (progn
          ;; Complete, fsync'd write of the whole result to the sibling temp.
          (let ((write-region-inhibit-fsync nil))
            (dorg-mutate--persist-write-temp tmp bytes))
          ;; Carry the destination's permission bits across the swap so a
          ;; temp's 0600 never leaks through (umask default for a new file).
          (set-file-modes tmp (if (file-exists-p target)
                                  (file-modes target)
                                (default-file-modes)))
          ;; Content compare-and-swap immediately before the rename: an
          ;; equal-stat replacement or a writer racing the read is caught here.
          (let ((current (dorg-mutate--persist-file-fingerprint target)))
            (unless (equal current baseline)
              (dorg-mutate--raise
               'stale
               "plain-org destination changed under an in-flight commit"
               :detail 'destination-drift :source-kind 'plain-org
               :baseline baseline :current current)))
          ;; Commit point: rename then set the committed flag with NOTHING that
          ;; can error/quit in between, all under `inhibit-quit' so a real C-g is
          ;; deferred until after the flag is set.
          (let ((inhibit-quit t))
            (dorg-mutate--persist-rename tmp target)
            (setq renamed t)
            (setcar committed t))
          ;; Now OUTSIDE inhibit-quit: a deferred interrupt may surface here, but
          ;; the commit already landed and (car COMMITTED) is t.
          (dorg-mutate--persist-post-rename))
      (unless renamed
        (when (and (stringp tmp) (file-exists-p tmp))
          (ignore-errors (delete-file tmp)))))
    (dorg-mutate--persist-fingerprint bytes)))

(defun dorg-mutate--plain-org-buffer-baseline (entry-fp file)
  "Return the trusted commit baseline for the current visiting buffer.
ENTRY-FP is the fingerprint of the buffer's current encoded bytes.  A MODIFIED
buffer must already carry a recorded `dorg-mutate--plain-org-baseline'
\(refused `stale'/`baseline-untrusted' otherwise — a late disk hash is never
adopted for stale contents).  A CLEAN buffer's bytes must match the on-disk
FILE (else `stale'/`stale-at-entry', which catches an equal-stat external
replacement and a stale-at-entry divergence); the verified destination
fingerprint is recorded buffer-locally and returned."
  (if (buffer-modified-p)
      (or dorg-mutate--plain-org-baseline
          (dorg-mutate--raise
           'stale
           "modified plain-org buffer has no trustworthy base fingerprint"
           :detail 'baseline-untrusted :source-kind 'plain-org))
    (let ((disk-fp (if (and file (file-exists-p file))
                       (dorg-mutate--persist-file-fingerprint file)
                     :absent)))
      (when (and (not (eq disk-fp :absent))
                 (not (equal disk-fp entry-fp)))
        (dorg-mutate--raise
         'stale
         "plain-org buffer and destination diverge at entry"
         :detail 'stale-at-entry :source-kind 'plain-org
         :buffer-fingerprint entry-fp :destination-fingerprint disk-fp))
      (setq dorg-mutate--plain-org-baseline disk-fp)
      disk-fp)))

(defun dorg-mutate--plain-org-post-commit (buffer fp)
  "Post-commit coherence bookkeeping for BUFFER after a successful commit of
content fingerprint FP: sync the visited-file modtime, clear the modified flag,
and record FP as the next-save baseline.  Idempotent, so the caller can complete
it after a first attempt is interrupted by a deferred post-rename interrupt."
  (with-current-buffer buffer
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    (setq dorg-mutate--plain-org-baseline fp)))

(defun dorg-mutate--plain-org-index-context (source patches base-s0 created changed)
  "Build the logical `dorg-ext-index-context' for a plain-org mutation.
Plain-org preserves core semantics WITHOUT a durable shadow index, so it binds
source/root, the canonical PATCHES, and id-bearing CREATED/CHANGED entries — but
supplies no host-safe candidate images (both nil), no candidate digest, and no
future commit token.  This is the backend-parity construction of the same
context TYPE the Dolt path produces (tracked-regression).

BASE-S0 is the plain-org revision captured at execute ENTRY, BEFORE the surgical
mutation and any persistence, so BASE-COMMIT binds the exact S0/CAS identity —
not the post-mutation revision (S1).  The caller MUST pass the pre-mutation
token."
  (dorg-ext-index-context-create
   :source-kind 'plain-org
   :root-id (dorg-mutate-source-root-id source)
   :file (dorg-mutate-source-file source)
   :base-commit base-s0
   :patches patches
   :created (mapcar (lambda (id) (dorg-ext-index-entry-create :kind 'create :id id))
                    created)
   :changed (mapcar (lambda (id) (dorg-ext-index-entry-create :kind 'change :id id))
                    changed)))

(defun dorg-mutate--plain-org-source-preserving-execute (source patches options)
  "Surgically apply typed PATCHES to a source-preserving plain-org SOURCE.
Byte-identical to an in-place `org-entry-put'; see the section commentary.
Persists through the DOrg-owned atomic primitive `dorg-mutate--plain-org-commit'
\(no raw `save-buffer' / destination `write-region'): a content-fingerprint
baseline is established at entry, the complete result is rendered to an O_EXCL
temp sibling, and an atomic rename commits it.  Returns the apply result plist."
  (let ((supplied (dorg-mutate--check-base-revision source options))
        ;; Capture the S0/CAS identity BEFORE any mutation: the index context
        ;; must bind the base revision, not the post-mutation (S1) revision the
        ;; result reports (tracked-case).
        (base-s0 (dorg-mutate--plain-org-revision source))
        (buf (dorg-mutate-source-buffer source))
        (file (dorg-mutate-source-file source))
        (created nil)
        (changed nil)
        ;; Built AFTER core simulation and BEFORE persistence, so it is a valid
        ;; precommit projector input (tracked-case) — never observes S1.
        (index-context nil)
        (saved nil))
    ;; Fail closed on duplicate `:DORG_ID:' AFTER the CAS gate above and the
    ;; upstream identity=none refusal, BEFORE any surgical write.
    (dorg-mutate--source-assert-unique-ids source)
    (cond
     ((buffer-live-p buf)
      (with-current-buffer buf
        (let* ((coding (or buffer-file-coding-system 'utf-8-unix))
               (saving (plist-get options :save))
               ;; Entry trust is established ONLY when saving.  `:save t' hashes
               ;; the buffer through the FAITHFUL encoder (a coding that cannot
               ;; byte-round-trip refuses BEFORE any mutation) and runs the
               ;; destination trust check (equal-stat / stale-at-entry /
               ;; baseline-untrusted).  `:save nil' performs NO destination I/O:
               ;; it records a CLEAN buffer's own encoded fingerprint as the
               ;; future baseline and leaves a MODIFIED buffer's baseline as-is
               ;; (absent stays absent, so a later save fails closed).
               (baseline
                (if saving
                    (dorg-mutate--plain-org-buffer-baseline
                     (dorg-mutate--persist-fingerprint
                      (dorg-mutate--persist-encode-faithful (buffer-string) coding))
                     file)
                  (progn
                    (unless (buffer-modified-p)
                      (setq dorg-mutate--plain-org-baseline
                            (dorg-mutate--persist-fingerprint
                             (dorg-mutate--persist-encode (buffer-string) coding))))
                    nil))))
          (let ((plan (dorg-mutate--source-preserving-plan patches source)))
            (setq changed (dorg-mutate--source-preserving-plan-changed plan)
                  index-context
                  (dorg-mutate--plain-org-index-context
                   source patches base-s0 nil changed))
            (dorg-ext-run-index-validators index-context 'plain-prewrite)
            (dorg-projection-with-programmatic-changes
              (let ((snapshot (buffer-string))
                    (snap-modified (buffer-modified-p))
                    (committed (list nil))
                    (new-fp nil))
                (condition-case err
                    (progn
                      (let ((run (dorg-mutate--source-preserving-apply-plan
                                  plan source)))
                        (setq created (plist-get run :created)
                              changed (plist-get run :changed)))
                      ;; Construct the projection context BEFORE any persistence.
                      (setq index-context
                            (dorg-mutate--plain-org-index-context
                             source patches base-s0 created changed))
                      (when saving
                        (let ((new-bytes (dorg-mutate--persist-encode-faithful
                                          (buffer-string) coding)))
                          (setq new-fp (dorg-mutate--persist-fingerprint new-bytes))
                          ;; Explicit commit point: `dorg-mutate--plain-org-commit'
                          ;; sets (car COMMITTED) t immediately after the rename.
                          (dorg-mutate--plain-org-commit file new-bytes baseline committed)
                          (dorg-mutate--plain-org-post-commit buf new-fp)
                          (setq saved t))))
                  ((error quit)
                   (if (car committed)
                       ;; PAST the explicit commit point: the new image is on disk
                       ;; and the buffer already holds the new text (surgical run).
                       ;; Never restore-as-uncommitted and never re-signal.  A fault
                       ;; (error OR quit) here came from the post-commit helper;
                       ;; complete the three coherence operations through an
                       ;; INDEPENDENT path — the primitives directly, not the
                       ;; possibly-faulted helper — under `inhibit-quit', so the
                       ;; buffer ends coherent (text=new, modified=nil, modtime
                       ;; current, baseline=new-fp) before reporting success.
                       (let ((inhibit-quit t))
                         (set-visited-file-modtime)
                         (set-buffer-modified-p nil)
                         (setq dorg-mutate--plain-org-baseline new-fp
                               saved t))
                     ;; PRE-commit failure: restore the pre-mutation buffer snapshot
                     ;; (text AND modified flag) so buffer and destination are
                     ;; byte-identical, then re-signal.
                     (progn
                       (save-restriction
                         (widen)
                         (delete-region (point-min) (point-max))
                         (insert snapshot))
                       (set-buffer-modified-p snap-modified)
                       (signal (car err) (cdr err))))))))))))
     ((and file (not (string-empty-p file)))
      (unless (file-readable-p file)
        (dorg-mutate--raise 'invalid-target
                            "faithful plain-org source file is not readable"
                            :file file))
      ;; ONE literal read establishes BOTH the baseline AND the decoded work
      ;; text (no between-read desync / ABA): the baseline is the fingerprint of
      ;; the exact bytes decoded, and those bytes must round-trip exactly under
      ;; their coding (else stale-at-entry) so a surgical re-encode cannot drift
      ;; untouched regions.  A replacement racing the read flips the pre-rename
      ;; recheck (destination-drift).
      (let* ((raw (dorg-mutate--persist-file-read file))
             (baseline (dorg-mutate--persist-fingerprint raw))
             (decoded (dorg-mutate--persist-decode-faithful raw))
             (text (car decoded))
             (coding (cdr decoded))
             (committed (list nil)))
        (with-temp-buffer
          (insert text)
          (let ((org-inhibit-startup t)) (org-mode))
          ;; A temp buffer needs no snapshot: it is discarded on any error and
          ;; the destination is only renamed after a clean apply + recheck.
          (let ((run (dorg-mutate--source-preserving-run patches source)))
            (setq created (plist-get run :created)
                  changed (plist-get run :changed)))
          ;; Construct the projection context BEFORE any persistence (the commit
          ;; below), so it is a valid precommit projector input (tracked-case).
          (setq index-context
                (dorg-mutate--plain-org-index-context
                 source patches base-s0 created changed))
          (dorg-ext-run-index-validators index-context 'plain-prewrite)
          ;; FAITHFUL encode of the RESULT: a mutation that introduces text
          ;; unrepresentable/lossy under the file's coding fails closed before
          ;; any write (the entry raw round-trip only proved the PRE-mutation
          ;; bytes).
          (let ((new-bytes (dorg-mutate--persist-encode-faithful (buffer-string) coding)))
            (condition-case err
                (progn
                  (dorg-mutate--plain-org-commit file new-bytes baseline committed)
                  (setq saved t))
              ((error quit)
               ;; Explicit commit point: a quit visible AFTER the rename (with
               ;; (car COMMITTED) set) reports the commit; a pre-rename failure
               ;; (flag unset) re-signals with the destination byte-identical.
               (if (car committed)
                   (setq saved t)
                 (signal (car err) (cdr err)))))))))
     (t (dorg-mutate--raise 'invalid-target
                            "faithful plain-org source has no writable buffer or file"
                            :source-kind 'plain-org)))
    (list :ok t :dry-run nil :source-kind 'plain-org
          :root-id nil
          :base-revision supplied
          :revision (dorg-mutate--plain-org-revision source)
          :saved saved
          :patches patches
          :created created
          :changed changed
          :deleted nil
          ;; Constructed pre-persistence in each branch above (tracked-case).
          :index-context index-context)))

(defun dorg-mutate--plain-org-execute (source patches options)
  "Execute typed PATCHES against plain-org SOURCE with OPTIONS.

EVERY plain-org source is SOURCE-PRESERVING (backend-derived fidelity): the
surgical executor `dorg-mutate--plain-org-source-preserving-execute' applies the
covered patches byte-identically to an in-place `org-entry-put' and persists through
the DOrg-owned atomic primitive.  A `none' policy is read-only and refuses
before any write.  Returns the apply result plist."
  ;; Dispatch off the resolved identity policy directly: a `none' source is
  ;; read-only and fails closed before any write.
  (when (eq (dorg-mutate--source-identity source) 'none)
    (dorg-mutate--raise 'unsupported
                        "mutation is unavailable under identity=none"
                        :detail 'identity-none :source-kind 'plain-org))
  (dorg-mutate--plain-org-source-preserving-execute source patches options))

;;; ─── Dolt Planner / Transaction Executor ─────────────────────────────

;; The Dolt executor reuses the SOURCE-NEUTRAL working
;; model and `dorg-mutate--ps-*' mutators verbatim; it differs from the
;; plain-org path in exactly two seams:
;;
;;   LOAD     A scoped `dorg-model-load-headings' call (or the explicit
;;            whole-root fallback through `dorg-model-load-tree') decodes
;;            Dolt rows into the same `dorg-heading' hash (with unibyte sort
;;            keys) the mutators expect — there is no Dolt-specific mutation
;;            semantics.
;;   PERSIST  diff the working model against the pristine snapshot into
;;            semantic DB ops, lower them through
;;            `dorg-sql-gen-lower-heading-ops' in FK-safe phases, and commit
;;            through the per-store gated lease batch.
;;
;; Optimistic concurrency records the caller token separately from each
;; attempt's actual plan HEAD.  Commit-time database failures are classified
;; only after the leased session confirms rollback; callers receive stable
;; `stale' / `invariant' details, never raw MySQL text.

;; KEEP (test-seam): item-id generator indirection; tests let-bind it to force a PK collision.
(defvar dorg-mutate--dolt-item-id-generator #'dorg-id-generate
  "Function returning ids for `heading_items' rows during Dolt lowering.
Defaults to `dorg-id-generate'.  Exposed as a test seam so a rollback
test can force a generated item id to collide with an existing
`heading_items.id', producing a deterministic commit-time PRIMARY KEY
constraint failure that exercises gated transaction rollback.")

(defun dorg-mutate--dolt-load (source &optional as-of ids)
  "Load Dolt SOURCE's root into a pristine snapshot and working model.
Returns (ORIG . PS): ORIG is the snapshot hash from
`dorg-model-load-tree' or an IDS-restricted `dorg-model-load-headings'
load (NEVER mutated; kept for diffing), and PS is a `dorg-mutate--ps'
built from DEEP COPIES of ORIG's headings so applying patches cannot disturb
ORIG.  IDS nil preserves the whole-root fallback.  Signals
`dorg-mutate-error' with `:reason invalid-target' when the root is absent
\(missing / wrong source / unbound root)."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (root-id (dorg-mutate-source-root-id source))
         (orig (if ids
                   (dorg-model-load-headings
                    dolt-dir root-id (delete-dups (copy-sequence ids)) as-of)
                 (dorg-model-load-tree dolt-dir root-id as-of))))
    (unless (gethash root-id orig)
      (dorg-mutate--raise 'invalid-target
                          (format "root id %S is not present in Dolt source" root-id)
                          :dolt-dir dolt-dir :root-id root-id :detail 'missing))
    (let ((hash (make-hash-table :test 'eql)))
      (maphash (lambda (id h) (puthash id (dorg-heading-deep-copy h) hash)) orig)
      (cons orig
            (dorg-mutate--ps-create :headings hash :root-id root-id)))))

(defun dorg-mutate--dolt-parent-predicate (parent-id)
  "Return a SQL predicate matching PARENT-ID in headings.parent_id."
  (if parent-id
      (format "parent_id = %d" parent-id)
    "parent_id IS NULL"))

(defun dorg-mutate--dolt-heading-row (dolt-dir root-id id &optional as-of)
  "Return heading row ID in ROOT-ID at AS-OF, or nil when absent."
  (car (dorg-sql
        dolt-dir
        (format "SELECT id, parent_id FROM headings%s WHERE root_id = %d AND id = %d"
                (dorg-sql-gen--as-of-suffix as-of) root-id id))))

(defun dorg-mutate--dolt-parent-id (dolt-dir root-id id &optional as-of)
  "Return ID's parent id in ROOT-ID at AS-OF, or nil when absent/root."
  (alist-get 'parent_id
             (dorg-mutate--dolt-heading-row dolt-dir root-id id as-of)))

(defun dorg-mutate--dolt-child-ids (dolt-dir root-id parent-id &optional as-of)
  "Return direct child ids under PARENT-ID in ROOT-ID at AS-OF."
  (mapcar
   (lambda (row) (alist-get 'id row))
   (dorg-sql
    dolt-dir
    (format "SELECT id FROM headings%s WHERE root_id = %d AND %s"
            (dorg-sql-gen--as-of-suffix as-of)
            root-id
            (dorg-mutate--dolt-parent-predicate parent-id)))))

(defun dorg-mutate--dolt-placement-row
    (dolt-dir root-id heading-id &optional as-of)
  "Return HEADING-ID's parent and hex sort key in ROOT-ID at AS-OF.
The row comes from DOLT-DIR and is a structural placement fact, not a hydrated
heading model."
  (car
   (dorg-sql
    dolt-dir
    (format
     (concat "SELECT id, parent_id, HEX(sort_key) AS sort_key_hex "
             "FROM headings%s WHERE root_id = %d AND id = %d")
     (dorg-sql-gen--as-of-suffix as-of) root-id heading-id))))

(defun dorg-mutate--dolt-placement-neighbor-id
    (dolt-dir root-id parent-id sort-key-hex direction &optional as-of)
  "Return the immediate sibling next to SORT-KEY-HEX under PARENT-ID.
DIRECTION is `before' or `after'.  The lookup is an exact structural fact at
AS-OF in DOLT-DIR/ROOT-ID and never hydrates the sibling partition."
  (unless (memq direction '(before after))
    (error "Invalid placement-neighbor direction: %S" direction))
  (let ((op (if (eq direction 'before) "<" ">"))
        (order (if (eq direction 'before) "DESC" "ASC")))
    (alist-get
     'id
     (car
      (dorg-sql
       dolt-dir
       (format
        (concat "SELECT id FROM headings%s WHERE root_id = %d AND %s "
                "AND sort_key %s X'%s' ORDER BY sort_key %s, id %s LIMIT 1")
        (dorg-sql-gen--as-of-suffix as-of)
        root-id
        (dorg-mutate--dolt-parent-predicate parent-id)
        op sort-key-hex order order))))))

(defun dorg-mutate--dolt-last-child-id
    (dolt-dir root-id parent-id &optional as-of)
  "Return the last direct child under PARENT-ID in DOLT-DIR/ROOT-ID at AS-OF."
  (alist-get
   'id
   (car
    (dorg-sql
     dolt-dir
     (format
      (concat "SELECT id FROM headings%s WHERE root_id = %d AND %s "
              "ORDER BY sort_key DESC, id DESC LIMIT 1")
      (dorg-sql-gen--as-of-suffix as-of)
      root-id
      (dorg-mutate--dolt-parent-predicate parent-id))))))

(defun dorg-mutate--dolt-insert-placement-ids
    (dolt-dir root-id parent-id before-id after-id patch &optional as-of)
  "Return the exact existing heading ids needed to place insert PATCH.
Unanchored insertion needs only the last child.  BEFORE-ID needs the anchor and
its predecessor; AFTER-ID needs the anchor and its successor.  Anchor ownership
is checked from the same AS-OF snapshot in DOLT-DIR/ROOT-ID and fails with the
existing typed missing/non-sibling refusal instead of widening to the full
sibling fan."
  (if (or before-id after-id)
      (let* ((anchor-id (or before-id after-id))
             (row (dorg-mutate--dolt-placement-row
                   dolt-dir root-id anchor-id as-of)))
        (unless (and row (eql parent-id (alist-get 'parent_id row)))
          (dorg-mutate--raise
           'invalid-target
           (format "position anchor %S is not a sibling here" anchor-id)
           :operation (plist-get patch :op) :target anchor-id :detail 'missing))
        (let* ((direction (if before-id 'before 'after))
               (neighbor
                (dorg-mutate--dolt-placement-neighbor-id
                 dolt-dir root-id parent-id
                 (alist-get 'sort_key_hex row) direction as-of)))
          (delq nil (list anchor-id neighbor))))
    (let ((last (dorg-mutate--dolt-last-child-id
                 dolt-dir root-id parent-id as-of)))
      (and last (list last)))))

(defun dorg-mutate--dolt-subtree-ids (dolt-dir root-id node-id &optional as-of)
  "Return NODE-ID and descendant ids in ROOT-ID at AS-OF."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows (dorg-sql
                dolt-dir
                (format (concat
                         "WITH RECURSIVE sub AS ("
                         " SELECT id, parent_id FROM headings%s"
                         " WHERE id = %d AND root_id = %d"
                         " UNION ALL"
                         " SELECT h.id, h.parent_id FROM headings%s h"
                         " JOIN sub ON h.parent_id = sub.id"
                         " WHERE h.root_id = %d)"
                         " SELECT id FROM sub")
                        sfx node-id root-id sfx root-id))))
    (mapcar (lambda (row) (alist-get 'id row)) rows)))

(defun dorg-mutate--dolt-resolve-extern-id (dolt-dir root-id host patch &optional as-of)
  "Resolve external HOST to one heading id in ROOT-ID at AS-OF."
  (let* ((key (plist-get (cdr host) :key))
         (value (plist-get (cdr host) :value))
         (rows (dorg-sql
                dolt-dir
                (dorg-sql-gen--heading-ids-by-property
                 root-id key value as-of))))
    (cond
     ((null rows)
      (dorg-mutate--raise 'invalid-target
                          (format "no heading with %s=%S in source" key value)
                          :operation (plist-get patch :op)
                          :target host :detail 'missing))
     ((cdr rows)
      (dorg-mutate--raise 'invalid-target
                          (format "%d headings match %s=%S; identity key must be unique"
                                  (length rows) key value)
                          :operation (plist-get patch :op)
                          :target host :detail 'ambiguous))
     (t (alist-get 'id (car rows))))))

(defun dorg-mutate--dolt-resolve-host-id (source root-id host patch &optional as-of)
  "Resolve HOST for SOURCE/ROOT-ID at AS-OF to a concrete heading id."
  (cond
   ((eq host :root) root-id)
   ((integerp host) host)
   ((dorg-mutate--typed-external-address-p host)
    (dorg-mutate--dolt-resolve-extern-id
     (dorg-mutate-source-dolt-dir source) root-id host patch as-of))
   (t nil)))

(defun dorg-mutate--dolt-resolve-typed-host (source root-id host patch head)
  "Resolve typed HOST's external identity against pinned HEAD."
  (if (and (consp host) (memq (car host) '(:heading :subtree))
           (eq (cadr host) :extern))
      (list (car host) :id
            (dorg-mutate--dolt-resolve-host-id
             source root-id (dorg-mutate--patch-host host) patch head))
    host))

(defun dorg-mutate--dolt-resolve-patch-externs (source root-id patch head)
  "Return typed PATCH with same-root external hosts resolved at HEAD."
  (let* ((out (copy-tree patch))
         (target (plist-get out :target))
         (kind (car target)))
    (cond
     ((memq kind '(:heading :subtree))
      (setq out (plist-put out :target
                           (dorg-mutate--dolt-resolve-typed-host
                            source root-id target patch head))))
     ((memq kind '(:property :keyword :keywords :planning :text :texts :items
                    :drawer-lines))
      (let ((copy (copy-sequence (cdr target))))
        (setq copy
              (plist-put copy :host
                         (dorg-mutate--dolt-resolve-typed-host
                          source root-id (plist-get copy :host) patch head)))
        (setq out (plist-put out :target (cons kind copy)))))
     ((eq kind :tree)
      (let ((copy (copy-sequence (cdr target))))
        (dolist (field '(:under :before :after))
          (when (plist-member copy field)
            (let ((host (plist-get copy field)))
              (when (consp host)
                (setq copy
                      (plist-put copy field
                                 (dorg-mutate--dolt-resolve-typed-host
                                  source root-id host patch head)))))))
        (setq out (plist-put out :target (cons :tree copy))))))
    (when (plist-member out :to)
      (let ((to (copy-sequence (plist-get out :to))))
        (dolist (field '(:under :before :after))
          (when (and (plist-member to field) (consp (plist-get to field)))
            (setq to
                  (plist-put to field
                             (dorg-mutate--dolt-resolve-typed-host
                              source root-id (plist-get to field) patch head)))))
        (setq out (plist-put out :to to))))
    out))

(defun dorg-mutate--dolt-resolve-patches-externs (source root-id patches head)
  "Return typed PATCHES with external hosts resolved once at HEAD."
  (mapcar (lambda (patch)
            (dorg-mutate--dolt-resolve-patch-externs
             source root-id patch head))
          patches))

(defun dorg-mutate--dolt-patch-hosts (patch)
  "Return heading hosts read by PATCH, excluding parents and anchors."
  (unless (eq (car (plist-get patch :target)) :tree)
    (list (dorg-mutate--patch-address patch))))

(defun dorg-mutate--dolt-scope-ids (source patches root-id head)
  "Return a bounded id slice for same-root Dolt PATCHES at HEAD.
The slice is a conservative superset of every heading read by the
simulation/diff/lowering path: direct targets, root row, structural
subtrees/children, anchors, current parents, and affected-parent sibling
fans."
  (let ((dolt-dir (dorg-mutate-source-dolt-dir source))
        (ids nil)
        (affected-parents nil))
    (cl-labels
        ((add (id) (when (integerp id) (cl-pushnew id ids :test #'eql)))
         (affect (id) (when (integerp id)
                        (cl-pushnew id affected-parents :test #'eql)))
         (resolve (host patch)
           (dorg-mutate--dolt-resolve-host-id source root-id host patch head))
         (add-host (host patch)
           (add (resolve host patch)))
         (add-parent-siblings (parent-id)
           (when (integerp parent-id)
             (dolist (id (dorg-mutate--dolt-child-ids
                          dolt-dir root-id parent-id head))
               (add id)))))
      (add root-id)
      (dolist (patch patches)
        (dolist (host (dorg-mutate--dolt-patch-hosts patch))
          (add-host host patch))
        (pcase (dorg-mutate--patch-signature patch)
          (`(insert :tree)
           (let* ((target (plist-get patch :target))
                  (parent (resolve (dorg-mutate--patch-address patch) patch))
                  (before
                   (and (plist-member (cdr target) :before)
                        (resolve
                         (dorg-mutate--patch-anchor-address
                          (dorg-mutate--patch-tget target :before)) patch)))
                  (after
                   (and (plist-member (cdr target) :after)
                        (resolve
                         (dorg-mutate--patch-anchor-address
                          (dorg-mutate--patch-tget target :after)) patch))))
             (add parent)
             (dolist (id (dorg-mutate--dolt-insert-placement-ids
                          dolt-dir root-id parent before after patch head))
               (add id))))
          (`(move :subtree)
           (let* ((to (plist-get patch :to))
                  (id (resolve (dorg-mutate--patch-address patch) patch))
                  (old-parent (dorg-mutate--dolt-parent-id
                               dolt-dir root-id id head))
                  (new-parent (resolve
                               (dorg-mutate--patch-host (plist-get to :under))
                               patch)))
             (dolist (sid (dorg-mutate--dolt-subtree-ids
                           dolt-dir root-id id head))
               (add sid))
             (add old-parent)
             (add new-parent)
             (affect old-parent)
             (affect new-parent)
             (when (plist-member to :before)
               (add (resolve (dorg-mutate--patch-anchor-address
                              (plist-get to :before)) patch)))
             (when (plist-member to :after)
               (add (resolve (dorg-mutate--patch-anchor-address
                              (plist-get to :after)) patch)))))
          ((or `(delete :heading) `(delete :subtree))
           (let* ((id (resolve (dorg-mutate--patch-address patch) patch))
                  (parent (dorg-mutate--dolt-parent-id
                           dolt-dir root-id id head)))
             (add parent)
             (affect parent)
             (if (eq (cadr (dorg-mutate--patch-signature patch)) :subtree)
                 (dolist (sid (dorg-mutate--dolt-subtree-ids
                               dolt-dir root-id id head))
                   (add sid))
               (progn
                 (dolist (cid (dorg-mutate--dolt-child-ids
                               dolt-dir root-id id head))
                   (add cid))
                 (affect id)))))))
      (dolist (parent affected-parents)
        (add-parent-siblings parent))
      (delete-dups (nreverse ids)))))

(defun dorg-mutate--dolt-db-ops (orig ps root-id)
  "Diff working model PS against snapshot ORIG into tagged DB-OPS.
Net-membership diff (interleave-safe; a create-then-delete in one batch
nets to nothing): a heading present only in PS is an `:insert-heading',
one present only in ORIG is a `:delete-heading', and one present in both
AND touched (created/changed/deleted accumulators) is an
`:update-heading'.  Returns the tagged DB-OPS that
`dorg-sql-gen-lower-heading-ops' consumes."
  (let* ((cur (dorg-mutate--ps-headings ps))
         (touched (make-hash-table :test 'eql))
         (db-ops nil))
    (dolist (id (append (dorg-mutate--ps-created ps)
                        (dorg-mutate--ps-changed ps)
                        (dorg-mutate--ps-deleted ps)))
      (puthash id t touched))
    ;; Inserts and updates iterate the present headings.
    (maphash
     (lambda (id h)
       (cond
        ((not (gethash id orig))
         (push (list :insert-heading h root-id) db-ops))
        ((gethash id touched)
         (push (list :update-heading h) db-ops))))
     cur)
    ;; Deletes iterate the snapshot for ids no longer present.
    (maphash
     (lambda (id _h)
       (unless (gethash id cur)
         (push (list :delete-heading id) db-ops)))
     orig)
    (nreverse db-ops)))

(defun dorg-mutate--dolt-freed-slots (db-ops orig)
  "Return a hash of `(PARENT-ID . SORT-KEY)' slots freed by DB-OPS deletes.
For every `:delete-heading' op, the deleted heading's ORIGINAL sibling
slot (looked up in the ORIG snapshot) becomes free once that delete runs.
Keys are `equal'-compared conses; values are t.  An inserted or updated
heading whose FINAL slot matches a freed slot cannot occupy its final
`(parent_id, sort_key)' until the freeing delete has run, or it would
race the `uq_heading_sibling_order' unique key — those headings are
STAGED by `dorg-mutate--dolt-assemble-sql'."
  (let ((freed (make-hash-table :test 'equal)))
    (dolist (op db-ops)
      (when (eq (car op) :delete-heading)
        (when-let ((dh (gethash (cadr op) orig)))
          (puthash (cons (dorg-heading-parent-id dh)
                         (dorg-heading-sort-key dh))
                   t freed))))
    freed))

(defun dorg-mutate--dolt-staged-slot-p (heading freed)
  "Return non-nil when HEADING's final sibling slot is in FREED.
FREED is a `dorg-mutate--dolt-freed-slots' hash.  Such a heading needs
its final sort key staged until the freeing delete has run."
  (gethash (cons (dorg-heading-parent-id heading)
                 (dorg-heading-sort-key heading))
           freed))

(defun dorg-mutate--dolt-temp-sort-key (id)
  "Return the deterministic 8-byte id-derived staging sort key for ID.
Big-endian encoding of ID, identical to the bytes the core lowerer's
divergent staging writes via `UNHEX(LPAD(HEX(id), 16, ''0''))'.  Distinct
per ID, but NOT guaranteed disjoint from an existing sibling's ALLOCATED
sort key — a midpoint key may, in principle, equal a small id's
big-endian form.  Callers must therefore verify the slot is free for the
affected parent: `dorg-mutate--dolt-free-temp-key' searches for a free
key (used for staged inserts, where the temp key is unconstrained), while
a staged update is pinned to this value by the core lowerer's divergent
staging and so must fail closed when it collides."
  (apply #'unibyte-string
         (mapcar (lambda (shift) (logand (ash id shift) #xff))
                 '(-56 -48 -40 -32 -24 -16 -8 0))))

(defun dorg-mutate--dolt-sibling-keys (db-ops orig parent-id)
  "Return a hash set of sort keys coexisting under PARENT-ID mid-batch.
The set unions, for PARENT-ID, every ORIG row's sort key (survivors and
not-yet-run deletes sit at their original slot until the DELETE phase)
and every FINAL row's sort key carried by an insert/update DB op
(non-staged rows are written at their final slot in the insert/update
phases).  A staging temp key absent from this set is free under
PARENT-ID at every phase of `dorg-mutate--dolt-assemble-sql', so it never
races the `uq_heading_sibling_order' unique key while the batch is
in-flight.  Conservative by construction (a superset of the keys actually
live at any single phase)."
  (let ((keys (make-hash-table :test 'equal)))
    (maphash (lambda (_id h)
               (when (equal (dorg-heading-parent-id h) parent-id)
                 (puthash (dorg-heading-sort-key h) t keys)))
             orig)
    (dolist (op db-ops)
      (when (memq (car op) '(:insert-heading :update-heading))
        (let ((h (cadr op)))
          (when (equal (dorg-heading-parent-id h) parent-id)
            (puthash (dorg-heading-sort-key h) t keys)))))
    keys))

(defun dorg-mutate--dolt-sibling-key-conflict-p (db-ops orig parent-id key owner-id)
  "Return non-nil if a heading OTHER THAN OWNER-ID holds KEY under PARENT-ID.
Scans the same coexisting set as `dorg-mutate--dolt-sibling-keys' (every
ORIG row at its original slot ∪ every DB-OPS insert/update final slot) but is
OWNER-AWARE: the staged heading's own row is excluded.  A staged
`:update-heading' is pinned to its id-derived staging key
\(`dorg-mutate--dolt-temp-sort-key'); when that key equals the heading's
OWN current sort key the core lowerer's phase-1 staging merely rewrites
the same slot back to the same row, which never races the
`uq_heading_sibling_order' unique key.  Only a DISTINCT non-staged sibling
already occupying the staging slot is a real collision, so the
fail-closed decision must ignore the moving heading itself."
  (catch 'conflict
    (maphash (lambda (id h)
               (when (and (not (equal id owner-id))
                          (equal (dorg-heading-parent-id h) parent-id)
                          (equal (dorg-heading-sort-key h) key))
                 (throw 'conflict t)))
             orig)
    (dolist (op db-ops)
      (when (memq (car op) '(:insert-heading :update-heading))
        (let ((h (cadr op)))
          (when (and (not (equal (dorg-heading-id h) owner-id))
                     (equal (dorg-heading-parent-id h) parent-id)
                     (equal (dorg-heading-sort-key h) key))
            (throw 'conflict t)))))
    nil))

(defun dorg-mutate--dolt-free-temp-key (id occupied)
  "Return an id-derived staging sort key for ID free in OCCUPIED.
OCCUPIED is a `dorg-mutate--dolt-sibling-keys' hash for the staged
heading's parent.  Prefers the plain id-derived key
\(`dorg-mutate--dolt-temp-sort-key'), so a batch with no pre-existing
collision keeps byte-identical SQL; when an existing sibling already
holds that slot, a big-endian disambiguator is appended and retried until
a free key is found.  Terminates because OCCUPIED is finite and each
iteration yields a distinct, strictly longer key (bounded well under
`dorg-sortkey-max-bytes')."
  (let ((base (dorg-mutate--dolt-temp-sort-key id)))
    (if (not (gethash base occupied))
        base
      (let ((n 1) key)
        (while (progn
                 (setq key (concat base (dorg-mutate--dolt-temp-sort-key n)))
                 (gethash key occupied))
          (setq n (1+ n)))
        key))))

(defun dorg-mutate--dolt-stage-heading-copy (heading temp-key)
  "Return a shallow copy of HEADING carrying TEMP-KEY as its sort key.
Only the sort key is rewritten; parent/level/content/tags/items keep
their FINAL values, so the staged INSERT/UPDATE writes a fully consistent
row at the collision-free temporary slot TEMP-KEY.  The original
HEADING — and thus the `ps' model and result plan — is left untouched."
  (let ((copy (copy-dorg-heading heading)))
    (setf (dorg-heading-sort-key copy) temp-key)
    copy))

(defun dorg-mutate--dolt-finalize-stmt (heading)
  "Return the sort-key-only finalize UPDATE for a staged HEADING.
Run AFTER the deletes have freed the target slot, this rewrites the row
from its temporary staging key to HEADING's final sort key.  Only the
sort key changed during staging, so no other column is touched."
  (format "UPDATE headings SET sort_key = %s WHERE id = %d"
          (dorg-sql-gen--sort-key-hex (dorg-heading-sort-key heading))
          (dorg-heading-id heading)))

(defun dorg-mutate--dolt-assemble-sql (db-ops orig id-gen)
  "Lower tagged DB-OPS to SQL in dependency/collision-safe phases.
Reuses `dorg-sql-gen-lower-heading-ops' and the `dorg-sql-gen' helpers — this
is FK-/collision-safe phasing of the EXISTING lowerer, not a public
raw-SQL surface.  ORIG is the snapshot hash (for tag/item diffing);
ID-GEN allocates `heading_items' ids.

A heading is STAGED when its final `(parent_id, sort_key)' slot was freed
by a `:delete-heading' in the same batch (see
`dorg-mutate--dolt-freed-slots').  Staged inserts/updates are lowered
from a temp-key heading COPY so their final slot is not written until the
freeing delete has run.  Phases emitted in order:

  1. INSERTS  — new parent rows exist before an update reparents an
                existing child under them; a staged insert lands at a
                collision-free temp key chosen for its parent.
  2. UPDATES  — surviving headings (incl. `:delete-heading' child
                promotion moving children out from under a doomed parent,
                and the core lowerer's divergent-sort-key staging); a
                staged update lands at its id-derived temp key.
  3. DELETES  — removed rows, freeing their `(parent_id, sort_key)' slots.
  4. FINALIZE — staged headings move from their temp key to the now-free
                final sort key (sort-key-only UPDATE).

Staging temp keys are verified collision-free against the affected
parent's coexisting sibling set (`dorg-mutate--dolt-sibling-keys'), not
merely against other staged headings.  A staged INSERT is free to pick
any unoccupied key, so `dorg-mutate--dolt-free-temp-key' searches for
one.  A staged UPDATE is pinned to its id-derived key by the core
lowerer's divergent staging (`UNHEX(LPAD(HEX(id), 16, ''0''))'); when an
existing sibling already occupies that slot the batch FAILS CLOSED with a
deterministic `unsupported' error here — before any SQL — so
typed plan and apply agree (both run this assembler via
`dorg-mutate--dolt-simulate') instead of reporting the batch supported
and relying on transaction rollback."
  (let ((freed (dorg-mutate--dolt-freed-slots db-ops orig))
        (occupied (make-hash-table :test 'equal))
        inserts updates deletes finalize)
    (cl-labels ((occ (pid)
                  (or (gethash pid occupied)
                      (puthash pid (dorg-mutate--dolt-sibling-keys db-ops orig pid)
                               occupied))))
      (dolist (op db-ops)
        (pcase (car op)
          (:insert-heading
           (let ((h (cadr op)))
             (if (dorg-mutate--dolt-staged-slot-p h freed)
                 (let* ((keys (occ (dorg-heading-parent-id h)))
                        (temp (dorg-mutate--dolt-free-temp-key
                               (dorg-heading-id h) keys)))
                   (puthash temp t keys)
                   (push (list :insert-heading
                               (dorg-mutate--dolt-stage-heading-copy h temp)
                               (caddr op))
                         inserts)
                   (push h finalize))
               (push op inserts))))
          (:update-heading
           (let ((h (cadr op)))
             (if (dorg-mutate--dolt-staged-slot-p h freed)
                 (let* ((final-parent (dorg-heading-parent-id h))
                        (orig-h (gethash (dorg-heading-id h) orig))
                        (orig-parent (and orig-h
                                          (dorg-heading-parent-id orig-h)))
                        (keys (occ final-parent))
                        (temp (dorg-mutate--dolt-temp-sort-key
                               (dorg-heading-id h))))
                   ;; A staged UPDATE's transient key is forced to the
                   ;; id-derived value by the core lowerer's divergent
                   ;; staging, so it cannot be perturbed.  The core lowerer
                   ;; (`dorg-sql-gen-lower-heading-ops') parks the row at that
                   ;; key in a phase-1 pass that runs BEFORE parent_id is
                   ;; changed, so for a REPARENTING move the temp key is
                   ;; briefly live under the ORIGINAL parent and then under
                   ;; the FINAL parent until finalize.  It must therefore be
                   ;; free against the coexisting sibling set of BOTH
                   ;; parents; otherwise refuse before SQL so plan and apply
                   ;; agree instead of relying on transaction rollback.  A
                   ;; final-parent-only check misses the source-parent
                   ;; collision (reviewer repro).  The check is
                   ;; OWNER-AWARE: when the moving heading already sits at
                   ;; be8(id) the phase-1 staging rewrites its OWN slot back
                   ;; to itself — not a collision — so only a DISTINCT
                   ;; sibling at that key fails closed.
                   (when (or (dorg-mutate--dolt-sibling-key-conflict-p
                              db-ops orig final-parent temp (dorg-heading-id h))
                             (and orig-parent
                                  (not (equal orig-parent final-parent))
                                  (dorg-mutate--dolt-sibling-key-conflict-p
                                   db-ops orig orig-parent temp
                                   (dorg-heading-id h))))
                     (dorg-mutate--raise
                      'unsupported
                      (format "delete-freed staging collision: an existing sibling already occupies the id-derived staging slot for heading %d (checked both its original parent %s and final parent %s); moving it into the freed slot is unsupported"
                              (dorg-heading-id h)
                              orig-parent
                              final-parent)
                      :detail 'delete-freed-sort-key-reuse
                      :operation 'update
                      :target (dorg-heading-id h)))
                   (puthash temp t keys)
                   (push (list :update-heading
                               (dorg-mutate--dolt-stage-heading-copy h temp))
                         updates)
                   (push h finalize))
               (push op updates))))
          (:update-sort-key (push op updates))
          (:delete-heading (push op deletes)))))
    (append
     (dorg-sql-gen-lower-heading-ops (nreverse inserts) orig id-gen)
     (dorg-sql-gen-lower-heading-ops (nreverse updates) orig id-gen)
     (dorg-sql-gen-lower-heading-ops (nreverse deletes) orig id-gen)
     (mapcar #'dorg-mutate--dolt-finalize-stmt (nreverse finalize)))))

(defun dorg-mutate--dolt-simulate (source patches options)
  "Apply typed PATCHES to a fresh SOURCE snapshot and lower row diffs to SQL.
Returns a plist (:head-commit HEAD :scope-ids SCOPE :db-ops DB-OPS :sql SQL
:ps PS :orig ORIG :created IDS :changed IDS :deleted IDS).  Any missing-target / non-sibling
anchor / move-under-self error signals HERE, in memory, before any write.
A same-batch delete that frees a sibling `(parent_id, sort_key)' slot an
insert/update wants is handled by the staged-slot scheduler in
`dorg-mutate--dolt-assemble-sql' — it commits, it is no longer rejected.
OPTIONS is accepted for symmetry; the executor reads `:base-commit'
itself."
  (ignore options)
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (root-id (dorg-mutate-source-root-id source))
         (head (dorg-head-commit dolt-dir))
         (resolved-patches (dorg-mutate--dolt-resolve-patches-externs
                            source root-id patches head))
         (scope-ids (dorg-mutate--dolt-scope-ids
                     source resolved-patches root-id head))
         (loaded (dorg-mutate--dolt-load source head scope-ids))
         (orig (car loaded))
         (ps (cdr loaded)))
    (let ((batch-context (dorg-mutate--item-batch-context)))
      (dolist (patch resolved-patches)
        (dorg-mutate--ps-apply-patch ps patch source batch-context)))
    (let* ((db-ops (dorg-mutate--dolt-db-ops orig ps root-id))
           (sql (dorg-mutate--dolt-assemble-sql
                 db-ops orig dorg-mutate--dolt-item-id-generator))
           (sim (list :head-commit head
                      :scope-ids (and scope-ids
                                      (sort (delete-dups (copy-sequence scope-ids))
                                            #'<))
                      :patches resolved-patches
                      :db-ops db-ops
                      :sql sql
                      :ps ps
                      :orig orig
                      :created (nreverse (delete-dups (dorg-mutate--ps-created ps)))
                      :changed (nreverse (delete-dups (dorg-mutate--ps-changed ps)))
                      :deleted (nreverse (delete-dups (dorg-mutate--ps-deleted ps))))))
      ;; Expose the immutable post-mutation projection context built from this
      ;; simulation, after core simulation and before persistence (tracked-case).
      (nconc sim
             (list :index-context
                   (dorg-mutate--dolt-index-context
                    source resolved-patches sim head))))))

;;; ─── Immutable post-mutation projection context (tracked-case) ──────────
;; DOrg builds ONE immutable candidate-state context after typed core
;; simulation and before persistence, from its OWN simulated state.  Index
;; projectors consume it (tracked-regression) instead of caller-authored semantic
;; payloads.  It binds the exact base/CAS (S0) identity and a candidate digest
;; but NEVER a future commit token, and exposes host-safe S0/candidate images
;; (copies) rather than the private mutable `dorg-mutate--ps'.

(defun dorg-mutate--index-host-safe-image (headings)
  "Return an immutable id -> `dorg-heading' snapshot of HEADINGS."
  (dorg-ext--index-host-safe-image headings))

(defun dorg-mutate--index-classify (orig ps-headings created changed deleted)
  "Partition a mutation into create/change/move/delete `dorg-ext-index-entry's."
  (dorg-ext--index-classify orig ps-headings created changed deleted))

(defun dorg-mutate--index-heading-canonical (h)
  "Return a COMPLETE canonical tuple of `dorg-heading' H's persisted state."
  (dorg-ext--index-heading-canonical h))

(defun dorg-mutate--index-owner-partition-canonical (partition)
  "Return a canonical digest row for owner PARTITION."
  (dorg-ext--index-owner-partition-canonical partition))

(defun dorg-mutate--index-owner-partition-canonical< (a b)
  "Non-nil when canonical owner partition row A sorts before B."
  (dorg-ext--index-owner-partition-canonical< a b))

(defun dorg-mutate--index-candidate-id (candidate-image &optional owner-partitions)
  "Return a deterministic transaction-scoped identity for CANDIDATE-IMAGE."
  (dorg-ext--index-candidate-id candidate-image owner-partitions))

(defun dorg-mutate--index-owner-key (source)
  "Return SOURCE's identity owner property key, or nil for identity=none."
  (dorg-mutate--identity-protected-key source))

(defun dorg-mutate--index-heading-owner-value (source heading)
  "Return HEADING's owner identity value under SOURCE's identity policy."
  (pcase (dorg-mutate--source-identity source)
    ('dorg (and (dorg-heading-id heading)
                (number-to-string (dorg-heading-id heading))))
    (`(:property ,key)
     (let ((v (cdr (assoc key (dorg-model-heading-property-snapshot heading)))))
       (and (stringp v) v)))
    (_ nil)))

(defun dorg-mutate--index-empty-image ()
  "Return an empty id -> heading image."
  (dorg-ext--index-empty-image))

(defun dorg-mutate--index-load-image (source root-id ids as-of)
  "Load IDS under ROOT-ID at AS-OF as a host-safe image.
IDS must be explicit; nil is never passed through to the whole-root loader."
  (if ids
      (dorg-mutate--index-host-safe-image
       (dorg-model-load-headings
        (dorg-mutate-source-dolt-dir source)
        root-id
        (sort (delete-dups (copy-sequence ids)) #'<)
        as-of))
    (dorg-mutate--index-empty-image)))

(defun dorg-mutate--index-image-slice (image ids)
  "Return a host-safe image slice for IDS already present in IMAGE."
  (let ((slice (dorg-mutate--index-empty-image)))
    (dolist (id ids slice)
      (when-let ((h (and image (gethash id image))))
        (puthash id (dorg-heading-deep-copy h) slice)))))

(defun dorg-mutate--index-image-missing-ids (image ids)
  "Return ids from IDS that are not present in IMAGE."
  (cl-remove-if (lambda (id) (and image (gethash id image))) ids))

(defun dorg-mutate--index-owner-base-image (source root-id ids base-image base)
  "Return owner-local base image, reusing BASE-IMAGE before bounded loads."
  (dorg-mutate--index-merge-images
   (dorg-mutate--index-image-slice base-image ids)
   (dorg-mutate--index-load-image
    source root-id
    (dorg-mutate--index-image-missing-ids base-image ids)
    base)))

(defun dorg-mutate--dolt-ancestor-ids (dolt-dir root-id start-id as-of)
  "Return START-ID and its ancestor ids in ROOT-ID at AS-OF, nearest first."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows
          (dorg-sql
           dolt-dir
           (format
            (concat
             "WITH RECURSIVE ancestors(id, parent_id, depth) AS ("
             " SELECT h.id, h.parent_id, 0 FROM headings%s h"
             " WHERE h.root_id = %d AND h.id = %d"
             " UNION ALL"
             " SELECT p.id, p.parent_id, ancestors.depth + 1"
             " FROM headings%s p"
             " JOIN ancestors ON p.id = ancestors.parent_id"
             " WHERE p.root_id = %d)"
             " SELECT id FROM ancestors ORDER BY depth")
            sfx root-id start-id sfx root-id))))
    (mapcar (lambda (row) (alist-get 'id row)) rows)))

(defun dorg-mutate--index-image-ancestor-prefix (image start-id)
  "Return (IDS . MISSING-PARENT) for START-ID's ancestor prefix in IMAGE."
  (let ((id start-id)
        ids
        seen
        missing-parent)
    (while (and (integerp id) (not missing-parent))
      (when (memq id seen)
        (dorg-mutate--raise 'invariant "cycle in owner ancestor chain"
                            :heading id))
      (push id seen)
      (let ((h (and image (gethash id image))))
        (if h
            (let ((parent (dorg-heading-parent-id h)))
              (push id ids)
              (setq id parent)
              (when (and (integerp parent)
                         (not (and image (gethash parent image))))
                (setq missing-parent parent)))
          (setq missing-parent id))))
    (cons (nreverse ids) missing-parent)))

(defun dorg-mutate--index-ancestor-image (source root-id image start-id as-of)
  "Return a host-safe image containing START-ID and its ancestor chain."
  (let* ((prefix (dorg-mutate--index-image-ancestor-prefix image start-id))
         (db-start (cdr prefix))
         (ids (append (car prefix)
                      (and (integerp db-start)
                           (dorg-mutate--dolt-ancestor-ids
                            (dorg-mutate-source-dolt-dir source)
                            root-id db-start as-of)))))
    (dorg-mutate--index-merge-images
     (dorg-mutate--index-image-slice image ids)
     (dorg-mutate--index-load-image
      source root-id (dorg-mutate--index-image-missing-ids image ids) as-of))))

(defun dorg-mutate--index-nearest-owner (source root-id image start-id as-of)
  "Return plist for START-ID's nearest identity owner under SOURCE, or nil."
  (let ((key (dorg-mutate--index-owner-key source))
        path
        seen)
    (when key
      (if (eq (dorg-mutate--source-identity source) 'dorg)
          (and (integerp start-id)
               (list :root-id root-id :owner-id start-id
                     :owner-key key :owner-value (number-to-string start-id)
                     :path (list start-id)))
        (let ((chain-image (dorg-mutate--index-ancestor-image
                            source root-id image start-id as-of))
              (id start-id))
          (catch 'owner
            (while (integerp id)
              (when (memq id seen)
                (dorg-mutate--raise 'invariant "cycle in owner ancestor chain"
                                    :root-id root-id :heading id))
              (push id seen)
              (let ((h (gethash id chain-image)))
                (unless h
                  (dorg-mutate--raise
                   'invalid-target
                   "missing heading while resolving owner partition"
                   :root-id root-id :heading id :detail 'missing))
                (push (dorg-heading-id h) path)
                (let ((value (dorg-mutate--index-heading-owner-value source h)))
                  (when value
                    (throw 'owner
                           (list :root-id root-id :owner-id (dorg-heading-id h)
                                 :owner-key key :owner-value value
                                 :path (nreverse path))))
                  (setq id (dorg-heading-parent-id h)))))))))))

(defun dorg-mutate--index-parent-owner (source root-id image owner-id as-of)
  "Return OWNER-ID's nearest ancestor identity owner under SOURCE, or nil."
  (if (eq (dorg-mutate--source-identity source) 'dorg)
      (when-let* ((owner (or (and image (gethash owner-id image))
                             (gethash owner-id
                                      (dorg-mutate--index-load-image
                                       source root-id (list owner-id) as-of))))
                  (parent-id (dorg-heading-parent-id owner)))
        (list :root-id root-id :owner-id parent-id
              :owner-key (dorg-mutate--index-owner-key source)
              :owner-value (number-to-string parent-id)
              :path (list parent-id)))
    (when-let* ((owner-image (dorg-mutate--index-ancestor-image
                              source root-id image owner-id as-of))
                (owner (gethash owner-id owner-image))
                (parent-id (dorg-heading-parent-id owner)))
      (dorg-mutate--index-nearest-owner source root-id owner-image parent-id as-of))))

(defun dorg-mutate--dolt-owner-local-ids (dolt-dir root-id owner-id key as-of
                                                   &optional query-fn)
  "Return OWNER-ID plus non-nested-owner descendants for identity property KEY.
QUERY-FN, when non-nil, replaces the implicit `dorg-sql' read path — the
settled-replay reconstruction (tracked-regression) runs under the held store
gate, where ordinary SQL is lease-refused, so it reads on the leased
session instead."
  (let* ((sfx (dorg-sql-gen--as-of-suffix as-of))
         (rows
          (funcall
           (or query-fn (lambda (sql) (dorg-sql dolt-dir sql)))
           (format
            (concat
             "WITH RECURSIVE owner_local(id, nested_owner) AS ("
             " SELECT h.id, 0 FROM headings%s h"
             " WHERE h.root_id = %d AND h.id = %d"
             " UNION ALL"
             " SELECT c.id,"
             " CASE WHEN EXISTS ("
             " SELECT 1 FROM heading_items%s hi"
             " JOIN property_entries%s pe ON pe.drawer_item_id = hi.id"
             " WHERE hi.heading_id = c.id AND pe.prop_key = %s"
             " ) THEN 1 ELSE 0 END"
             " FROM headings%s c"
             " JOIN owner_local ol ON c.parent_id = ol.id"
             " WHERE c.root_id = %d AND ol.nested_owner = 0)"
             " SELECT id FROM owner_local"
             " WHERE nested_owner = 0 ORDER BY id")
            sfx root-id owner-id
            sfx sfx (dorg-sql-quote key)
            sfx root-id))))
    (mapcar (lambda (row) (alist-get 'id row)) rows)))

(defun dorg-mutate--index-owner-local-ids (source root-id owner-id as-of
                                                  &optional query-fn)
  "Return OWNER-ID plus owner-local descendant ids, stopping before nested owners.
QUERY-FN optionally replaces the implicit read path (see
`dorg-mutate--dolt-owner-local-ids')."
  (pcase (dorg-mutate--source-identity source)
    ('dorg (list owner-id))
    (`(:property ,key)
     (dorg-mutate--dolt-owner-local-ids
      (dorg-mutate-source-dolt-dir source) root-id owner-id key as-of
      query-fn))
    (_ nil)))

(defun dorg-mutate--index-all-entries (parts)
  "Return every index entry in PARTS."
  (append (plist-get parts :created)
          (plist-get parts :changed)
          (plist-get parts :moved)
          (plist-get parts :deleted)))

(defun dorg-mutate--index-entry-owner (source root-id image entry side as-of)
  "Return ENTRY's owner plist for SIDE (`before' or `after'), or nil."
  (let ((heading (if (eq side 'before)
                     (dorg-ext-index-entry-before entry)
                   (dorg-ext-index-entry-after entry))))
    (when heading
      (dorg-mutate--index-nearest-owner
       source root-id image (dorg-heading-id heading) as-of))))

(defun dorg-mutate--index-put-owner (owners owner affected-id side)
  "Record OWNER/AFFECTED-ID/SIDE into OWNERS hash."
  (when owner
    (let* ((key (list (plist-get owner :root-id)
                      (plist-get owner :owner-id)
                      (plist-get owner :owner-value)))
           (path (list :id affected-id :side side
                       :path (copy-sequence (plist-get owner :path))))
           (cell (gethash key owners)))
      (if cell
          (progn
            (cl-pushnew affected-id (plist-get cell :affected-ids) :test #'eql)
            (cl-pushnew path (plist-get cell :affected-paths) :test #'equal))
        (puthash key (append owner (list :affected-ids (list affected-id)
                                         :affected-paths (list path)))
                 owners)))))

(defun dorg-mutate--index-overlay-owner-candidate
    (image entries owner source root-id candidate-postimage base sides)
  "Overlay affected ENTRIES for OWNER onto owner-local IMAGE."
  (let ((owner-id (plist-get owner :owner-id)))
    (dolist (entry entries)
      (let ((after (dorg-ext-index-entry-after entry))
            (before (dorg-ext-index-entry-before entry)))
        (when (and before (memq 'before sides))
          (when-let ((before-owner
                      (dorg-mutate--index-nearest-owner
                       source root-id image (dorg-heading-id before) base)))
            (when (eql owner-id (plist-get before-owner :owner-id))
              (remhash (dorg-heading-id before) image))))
        (when (and after (memq 'after sides))
          (when-let ((after-owner
                      (dorg-mutate--index-nearest-owner
                       source root-id candidate-postimage
                       (dorg-heading-id after) base)))
            (when (eql owner-id (plist-get after-owner :owner-id))
              (puthash (dorg-heading-id after)
                       (dorg-heading-deep-copy after)
                       image))))))))

(defun dorg-mutate--index-owner-partitions
    (source root-id parts base-image candidate-postimage base &optional sides)
  "Return affected owner partitions for SOURCE/ROOT-ID and index PARTS."
  (let ((sides (or sides '(before after)))
        (owners (make-hash-table :test 'equal))
        (entries (dorg-mutate--index-all-entries parts)))
    (when (dorg-mutate--index-owner-key source)
      (dolist (entry entries)
        (when (memq 'before sides)
          (dorg-mutate--index-put-owner
           owners
           (dorg-mutate--index-entry-owner source root-id base-image entry 'before base)
           (dorg-ext-index-entry-id entry)
           'before))
        (when (memq 'after sides)
          (dorg-mutate--index-put-owner
           owners
           (dorg-mutate--index-entry-owner
            source root-id candidate-postimage entry 'after base)
           (dorg-ext-index-entry-id entry)
           'after))))
    (let (partitions)
      (maphash
       (lambda (_key owner)
         (let* ((owner-id (plist-get owner :owner-id))
                (parent-image (if (and (memq 'before sides)
                                       (not (memq 'after sides)))
                                  base-image
                                candidate-postimage))
                (parent (dorg-mutate--index-parent-owner
                         source root-id parent-image owner-id base))
                (ids (dorg-mutate--index-owner-local-ids
                      source root-id owner-id base))
                (base-owner-image
                 (dorg-mutate--index-owner-base-image
                  source root-id ids base-image base))
                (candidate-owner-image
                 (dorg-ext--copy-image base-owner-image)))
           (dorg-mutate--index-overlay-owner-candidate
            candidate-owner-image entries owner source root-id
            candidate-postimage base sides)
           (push
            (dorg-ext-owner-partition-create
             :root-id root-id
             :owner-id owner-id
             :owner-key (plist-get owner :owner-key)
             :owner-value (plist-get owner :owner-value)
             :parent-owner-id (plist-get parent :owner-id)
             :parent-owner-value (plist-get parent :owner-value)
             :affected-ids (sort (plist-get owner :affected-ids) #'<)
             :affected-paths (plist-get owner :affected-paths)
             :base-image base-owner-image
             :candidate-postimage candidate-owner-image)
            partitions)))
       owners)
      (sort partitions
            (lambda (a b)
              (< (dorg-ext-owner-partition-owner-id a)
                 (dorg-ext-owner-partition-owner-id b)))))))

(defun dorg-mutate--dolt-index-context (source patches sim base)
  "Build the immutable `dorg-ext-index-context' for a Dolt mutation.
SOURCE, resolved PATCHES, the `dorg-mutate--dolt-simulate' SIM, and the exact
plan-base CAS token BASE (the immutable S0 identity) supply the DOrg-owned
fields.  The context is produced from DOrg's OWN simulated core state after
simulation and before persistence; it exposes host-safe S0 and candidate images
and never a future commit token."
  (let* ((orig (plist-get sim :orig))
         (ps (plist-get sim :ps))
         (ps-headings (and ps (dorg-mutate--ps-headings ps)))
         (base-image (dorg-mutate--index-host-safe-image orig))
         (candidate-postimage (dorg-mutate--index-host-safe-image ps-headings))
         (parts (dorg-mutate--index-classify
                 orig ps-headings
                 (plist-get sim :created)
                 (plist-get sim :changed)
                 (plist-get sim :deleted)))
         (owner-partitions
          (dorg-mutate--index-owner-partitions
           source (dorg-mutate-source-root-id source)
           parts base-image candidate-postimage base)))
    (dorg-ext-index-context-create
     :source-kind 'dolt
     :root-id (dorg-mutate-source-root-id source)
     :dolt-dir (dorg-mutate-source-dolt-dir source)
     :file (dorg-mutate-source-file source)
     :base-commit base
     :candidate-id (dorg-mutate--index-candidate-id
                    candidate-postimage owner-partitions)
     :patches patches
     :created (plist-get parts :created)
     :changed (plist-get parts :changed)
     :moved (plist-get parts :moved)
     :deleted (plist-get parts :deleted)
     :base-image base-image
     :candidate-postimage candidate-postimage
     :owner-partitions owner-partitions)))

(defun dorg-mutate--index-merge-images (a b)
  "Return a new host-safe image hash merging images A and B.
Heading ids are globally unique primary keys across roots, so the two bounded
root scopes never truly collide; the merge unions them for a cross-root context.
B wins on the impossible-collision case."
  (let ((merged (make-hash-table :test 'eql)))
    (when a (maphash (lambda (id h) (puthash id h merged)) a))
    (when b (maphash (lambda (id h) (puthash id h merged)) b))
    merged))

(defun dorg-mutate--dolt-cross-root-index-context (source patch sim base)
  "Build the immutable cross-root `dorg-ext-index-context' for an archive move.
It AGGREGATES both roots: BASE-IMAGE unions the source and destination S0
scopes and CANDIDATE-POSTIMAGE unions the re-homed candidate scopes, so a
root-aggregate projector sees each moved id leave FROM-ROOT-ID (source lineage
in BASE-IMAGE) and arrive at TO-ROOT-ID (destination lineage in
CANDIDATE-POSTIMAGE).  SIM is the `dorg-mutate--dolt-cross-root-simulate'
result; BASE is the plan-base CAS (the immutable S0 identity).  The moved ids
are `move' entries whose BEFORE is the pristine source-root heading and whose
AFTER is the re-homed destination heading."
  (let* ((src-orig (plist-get sim :src-orig))
         (src-ps (plist-get sim :src-ps))
         (dst-orig (plist-get sim :dst-orig))
         (dst-ps (plist-get sim :dst-ps))
         (src-cand (and src-ps (dorg-mutate--ps-headings src-ps)))
         (base-image (dorg-mutate--index-merge-images
                      (dorg-mutate--index-host-safe-image src-orig)
                      (dorg-mutate--index-host-safe-image dst-orig)))
         (candidate-postimage
          (dorg-mutate--index-merge-images
           (dorg-mutate--index-host-safe-image src-cand)
           (dorg-mutate--index-host-safe-image
            (and dst-ps (dorg-mutate--ps-headings dst-ps)))))
         (moves
          (mapcar
           (lambda (id)
             (let ((before (and src-orig (gethash id src-orig)))
                   (after (and src-cand (gethash id src-cand))))
               (dorg-ext-index-entry-create
                :kind 'move :id id
                :before (and before (dorg-heading-deep-copy before))
                :after (and after (dorg-heading-deep-copy after)))))
           (plist-get sim :moved)))
         (parts (list :created nil :changed nil :moved moves :deleted nil))
         (owner-partitions
          (append
           (dorg-mutate--index-owner-partitions
            source (plist-get sim :from-root-id)
            parts
            (dorg-mutate--index-host-safe-image src-orig)
            (dorg-mutate--index-host-safe-image src-cand)
            base '(before))
           (when-let ((dest (plist-get patch :to-source)))
             (dorg-mutate--index-owner-partitions
              dest (plist-get sim :to-root-id)
              parts
              (dorg-mutate--index-host-safe-image dst-orig)
              candidate-postimage
              base '(after))))))
    (dorg-ext-index-context-create
     :source-kind 'dolt
     :root-id (plist-get sim :to-root-id)
     :from-root-id (plist-get sim :from-root-id)
     :to-root-id (plist-get sim :to-root-id)
     :dolt-dir (dorg-mutate-source-dolt-dir source)
     :file (dorg-mutate-source-file source)
     :base-commit base
     :candidate-id (dorg-mutate--index-candidate-id
                    candidate-postimage owner-partitions)
     :patches (list patch)
     :created nil :changed nil :moved moves :deleted nil
     :base-image base-image
     :candidate-postimage candidate-postimage
     :owner-partitions owner-partitions)))

;;; ─── Canonical postimage receipt + pre-commit transaction proof (2ij34) ────
;; Contract C (tracked-regression): a committed apply carries one versioned receipt
;; entry per affected owner partition, and the commit is guarded by an
;; in-transaction proof in the existing typed transaction-read validate window
;; (post-statements, pre-DOLT_COMMIT):
;;   assert (1) readback core rows == the candidate canonical, and
;;   assert (2) readback extension rows == the validated typed effects.
;; Both abort pre-commit (rollback, zero durable writes).  Effects are
;; project(candidate), so (1) makes (2) transitively rows == project(core S1)
;; — the generic parity invariant — while each leg compares READBACK against
;; an independent pre-commit value (neither is a recomputation).  The receipt
;; digest is computed PRE-transaction from the candidate canonical (the
;; mutation-log INSERT precedes the validate window) and is ENTITLED by
;; assert (1): an aborted transaction rolls the digest row back, so no
;; unentitled digest survives.

(defun dorg-mutate--postimage-digest (partition)
  "Return the sha256 receipt digest of owner PARTITION's canonical form.
The digest is over the EXISTING full persisted canonical representation
\(`dorg-mutate--index-owner-partition-canonical') — the contract wires the
one canon, it invents no second one.  Printer depth/length limits are bound
off so the printed canonical is total and deterministic."
  (let ((print-level nil) (print-length nil) (print-circle nil))
    (secure-hash 'sha256
                 (prin1-to-string
                  (dorg-mutate--index-owner-partition-canonical partition)))))

(defun dorg-mutate--postimage-entry (partition candidate-id)
  "Return the versioned `:postimages' receipt entry for PARTITION.
CANDIDATE-ID is the apply's transaction-scoped candidate-state identity.
`:deleted' is t when this apply removes the owner heading itself — the owner
id no longer appears in the partition's candidate image."
  (let ((owner-id (dorg-ext-owner-partition-owner-id partition))
        (cand (dorg-ext-owner-partition-candidate-postimage partition)))
    (list :version 1
          :root (dorg-ext-owner-partition-root-id partition)
          :owner-heading owner-id
          :parent-owner-heading (dorg-ext-owner-partition-parent-owner-id partition)
          :candidate-id candidate-id
          :digest (dorg-mutate--postimage-digest partition)
          :deleted (not (and cand (gethash owner-id cand))))))

(defun dorg-mutate--postimage-log-payload (entries)
  "Return the serialized `postimages_v1' evidence payload for ENTRIES, or nil.
The operational row stores positional (ROOT OWNER-HEADING PARENT-OWNER
CANDIDATE-ID DIGEST DELETED) tuples — scope and digests only, no caller
payload; the envelope's own `:version' field (inside each receipt entry)
governs compatibility."
  (when entries
    (let ((print-level nil) (print-length nil) (print-circle nil))
      (prin1-to-string
       (mapcar (lambda (e)
                 (list (plist-get e :root)
                       (plist-get e :owner-heading)
                       (plist-get e :parent-owner-heading)
                       (plist-get e :candidate-id)
                       (plist-get e :digest)
                       (plist-get e :deleted)))
               entries)))))

(defun dorg-mutate--postimage-expectations (context partitions)
  "Return (EXPECTED-HT . ABSENT-IDS) for CONTEXT and its PARTITIONS.
EXPECTED-HT maps each heading id the transaction proof must find to its
candidate canonical tuple; ABSENT-IDS are the ids the apply deletes, whose
rows must be GONE at readback.  Coverage is the union of every partition's
candidate image (so each receipt digest's rows sit inside assert (1)) and
every per-heading effect entry — the latter also covers identity-less
sources, which materialize no owner partitions."
  (let ((cand (dorg-ext-index-context-candidate-postimage context))
        (expected (make-hash-table :test 'eql))
        (absent nil))
    (dolist (p partitions)
      (let ((img (dorg-ext-owner-partition-candidate-postimage p)))
        (when img
          (maphash (lambda (id h)
                     (puthash id (dorg-mutate--index-heading-canonical h)
                              expected))
                   img))))
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)
                           (dorg-ext-index-context-moved context)
                           (dorg-ext-index-context-deleted context)))
      (let* ((id (dorg-ext-index-entry-id entry))
             (h (and cand (gethash id cand))))
        (cond ((null h) (push id absent))
              ((not (gethash id expected))
               (puthash id (dorg-mutate--index-heading-canonical h) expected)))))
    (cons expected
          (sort (cl-delete-if (lambda (id) (gethash id expected))
                              (delete-dups absent))
                #'<))))

(defun dorg-mutate--postimage-row-matches-p (row where)
  "Non-nil when effect ROW satisfies delete predicate WHERE (column alists).
Mirrors the typed equality semantics lowered by
`dorg-sql--effect-predicate-sql': nil matches only nil.  A WHERE column ABSENT
from ROW does not match — the validator never guesses a database default
value."
  (cl-every (lambda (cell)
              (let ((hit (assq (car cell) row)))
                (and hit (equal (cdr hit) (cdr cell)))))
            where))

(defun dorg-mutate--postimage-row-checks (effects)
  "Return net-effect readback checks for the validated typed EFFECTS.
Walks EFFECTS in execution order per table, maintaining the surviving insert
rows (an insert is removed when a LATER delete predicate matches it), so the
checks describe the transaction's FINAL expected state, not any transient
one — a projector's delete-then-rebuild pattern nets to its inserts.
Returns (:present ((TABLE ROW COUNT)…) :absent ((TABLE WHERE COUNT)…)):
each surviving ROW must match at least COUNT readback rows; each delete
WHERE scope must hold exactly COUNT readback rows (its surviving matching
inserts)."
  (let (survivors deletes)
    (dolist (effect effects)
      (let ((table (plist-get effect :table)))
        (cond
         ((plist-member effect :insert)
          (push (cons table (plist-get effect :insert)) survivors))
         ((plist-member effect :delete)
          (let ((where (plist-get effect :delete)))
            (setq survivors
                  (cl-delete-if (lambda (pr)
                                  (and (equal (car pr) table)
                                       (dorg-mutate--postimage-row-matches-p
                                        (cdr pr) where)))
                                survivors))
            (push (cons table where) deletes))))))
    (setq survivors (nreverse survivors)
          deletes (nreverse deletes))
    (let (present)
      (dolist (pr survivors)
        (let ((hit (cl-find-if (lambda (c)
                                 (and (equal (nth 0 c) (car pr))
                                      (equal (nth 1 c) (cdr pr))))
                               present)))
          (if hit (cl-incf (nth 2 hit))
            (push (list (car pr) (cdr pr) 1) present))))
      (list :present (nreverse present)
            :absent
            (mapcar (lambda (d)
                      (list (car d) (cdr d)
                            (cl-count-if
                             (lambda (pr)
                               (and (equal (car pr) (car d))
                                    (dorg-mutate--postimage-row-matches-p
                                     (cdr pr) (cdr d))))
                             survivors)))
                    deletes)))))

(defun dorg-mutate--postimage-abort (plane message &rest facts)
  "Signal the typed in-transaction parity abort for PLANE with MESSAGE.
Rides the ordinary batch rollback path (`dorg-sql-postimage-parity' →
confirmed ROLLBACK → sanitized receipt → terminal typed refusal), so the
transaction — data, derived rows, and evidence row — rolls back with zero
durable writes.  FACTS carry only closed values (ids, table names)."
  (signal 'dorg-sql-postimage-parity
          (list message (append (list :plane plane) facts))))

(defun dorg-mutate--postimage-assert-core (read expected absent-ids
                                                 &optional union-root)
  "Assert (1): in-transaction readback core rows equal the candidate canonical.
READ is the typed transaction-local read port.  EXPECTED maps
heading id → candidate canonical tuple; ABSENT-IDS must have no readback
row.  Readback rows are normalized through the SAME `dorg-sql--coerce-types'
+ `dorg-model-from-db-row' path every loader uses, then canonicalized by the
SAME `dorg-mutate--index-heading-canonical' — so the two compared values
share one construction convention and one canon.  UNION-ROOT, when non-nil
\(every same-root apply), root-scopes the readback SELECT: a row re-homed
out of the mutated root is MISSING at readback and mismatches.  A
cross-root aggregate passes nil here and binds root placement per
partition instead (K#025 B1)."
  (let (ids)
    (maphash (lambda (id _c) (push id ids)) expected)
    (setq ids (sort ids #'<))
    (when ids
      (let* ((h-rows (funcall read
                              (list :version 1 :op 'core-heading-rows
                                    :ids ids :root-id union-root)))
             (tag-rows (funcall read
                                (list :version 1 :op 'core-heading-tags
                                      :ids ids)))
             (tags-ht (make-hash-table :test 'eql))
             (readback (make-hash-table :test 'eql))
             mismatched)
        (dolist (tr tag-rows)
          (let ((hid (alist-get 'heading_id tr)))
            (puthash hid (append (gethash hid tags-ht)
                                 (list (alist-get 'tag tr)))
                     tags-ht)))
        (dolist (row h-rows)
          (let ((id (alist-get 'id row)))
            (puthash id
                     (dorg-mutate--index-heading-canonical
                      (dorg-model-from-db-row row (gethash id tags-ht)))
                     readback)))
        (dolist (id ids)
          (unless (equal (gethash id expected) (gethash id readback))
            (push id mismatched)))
        (when mismatched
          (dorg-mutate--postimage-abort
           'core
           "readback core rows diverge from the validated candidate"
           :ids (sort mismatched #'<)))))
    (when absent-ids
      (let ((rows (funcall read (list :version 1 :op 'core-heading-ids
                                      :ids absent-ids))))
        (when rows
          (dorg-mutate--postimage-abort
           'core
           "rows the validated candidate deletes survive at readback"
           :ids (sort (mapcar (lambda (r)
                                (let ((id (alist-get 'id r)))
                                  (if (stringp id) (string-to-number id) id)))
                              rows)
                      #'<)))))))

(defun dorg-mutate--postimage-readback-count (read table pred)
  "Return the readback row count in TABLE matching predicate PRED (or all)."
  (funcall read (list :version 1 :op 'extension-row-count
                      :table table :where pred)))

(defun dorg-mutate--postimage-assert-rows (read checks)
  "Assert (2): in-transaction readback extension rows equal the typed effects.
READ is the typed transaction-local read port; CHECKS is the
net-effect expectation from `dorg-mutate--postimage-row-checks'.  Row
predicates remain typed equality alists here; DOrg lowers them inside the
transaction read port, so the proof path receives no SQL predicate text."
  (dolist (check (plist-get checks :present))
    (pcase-let ((`(,table ,row ,count) check))
      (when (< (dorg-mutate--postimage-readback-count
                read table row)
               count)
        (dorg-mutate--postimage-abort
         'rows
         "a validated derived row is missing or altered at readback"
         :table table))))
  (dolist (check (plist-get checks :absent))
    (pcase-let ((`(,table ,where ,count) check))
      (when (/= (dorg-mutate--postimage-readback-count
                 read table where)
                count)
        (dorg-mutate--postimage-abort
         'rows
         "a deleted derived scope holds unexpected rows at readback"
         :table table)))))

(defun dorg-mutate--postimage-wire-item (item)
  "Serialize model ITEM to its public-constructor typed form (wire v1).
The head vocabulary is EXACTLY `dorg-api--item-value's input grammar —
\(text S), (keyword K V), (planning :scheduled/:deadline/:closed …) with
present-only fields in fixed order, (drawer NAME (K V)…) for property-form
drawers and (drawer NAME :lines (L…)) for line-form ones (the loader's own
detail-table discriminator) — and every form carries an ALWAYS-PRESENT
`:post-blank' tail (K#025 B2): item-level post-blank is persisted and
participates in the items digest, so the unchanged-complement checker must
see it on the wire."
  (let ((pb (list :post-blank (dorg-item-post-blank item))))
    (cond
     ((cl-typep item 'dorg-text-item)
      (append (list 'text (dorg-text-item-text item)) pb))
     ((cl-typep item 'dorg-keyword-item)
      (append (list 'keyword (dorg-keyword-item-keyword item)
                    (dorg-keyword-item-value item))
              pb))
     ((cl-typep item 'dorg-planning-item)
      (append '(planning)
              (when-let ((s (dorg-planning-item-scheduled item)))
                (list :scheduled s))
              (when-let ((d (dorg-planning-item-deadline item)))
                (list :deadline d))
              (when-let ((c (dorg-planning-item-closed item)))
                (list :closed c))
              pb))
     ((cl-typep item 'dorg-drawer-item)
      (let ((name (dorg-drawer-item-name item))
            (props (dorg-drawer-item-properties item)))
        (if props
            (append (list 'drawer name)
                    (mapcar (lambda (kv) (list (car kv) (cdr kv))) props)
                    pb)
          (append (list 'drawer name :lines
                        (copy-sequence (dorg-drawer-item-lines item)))
                  pb))))
     (t (dorg-mutate--raise
         'invariant "wire serializer met an unknown item kind"
         :detail 'postimage-wire :item-type (type-of item))))))

(defun dorg-mutate--postimage-wire-node (heading children)
  "Serialize HEADING with serialized CHILDREN into the v1 wire NODE.
THE single wire serializer: every persisted heading field the delta checker
consumes crosses here (level derives from tree position and is omitted by
contract).  Properties are sorted by name; items keep model order."
  (list 'heading
        :id (dorg-heading-id heading)
        :title (dorg-heading-title heading)
        :todo (dorg-heading-todo heading)
        :priority (dorg-heading-priority heading)
        :tags (copy-sequence (dorg-heading-tags heading))
        :post-blank (dorg-heading-post-blank heading)
        :commented (and (dorg-heading-commented heading) t)
        :properties (sort (copy-sequence
                           (dorg-model-heading-property-snapshot heading))
                          (lambda (a b) (string< (car a) (car b))))
        :items (mapcar #'dorg-mutate--postimage-wire-item
                       (dorg-heading-items heading))
        :children children))

(defun dorg-mutate--postimage-wire-tree (image owner-id)
  "Serialize owner OWNER-ID's subtree from owner-local IMAGE to a wire NODE.
IMAGE is an id → `dorg-heading' hash of the owner-local slice; children
order is semantic (sort-key), matching every render/model path."
  (let ((kids (make-hash-table :test 'eql)))
    (maphash (lambda (id h)
               (unless (eql id owner-id)
                 (push h (gethash (dorg-heading-parent-id h) kids))))
             image)
    (letrec ((build
              (lambda (h)
                (dorg-mutate--postimage-wire-node
                 h
                 (mapcar build
                         (sort (copy-sequence
                                (gethash (dorg-heading-id h) kids))
                               (lambda (a b)
                                 (string< (dorg-heading-sort-key a)
                                          (dorg-heading-sort-key b)))))))))
      (funcall build (gethash owner-id image)))))

(defun dorg-mutate--postimage-assert-partitions
    (read owner-specs want-models models-store)
  "Assert (1) in FULL for every affected owner partition, unconditionally.
OWNER-SPECS is a list of (ROOT-ID OWNER-ID IDS EXPECTED-HT DELETED) — one
per receipt entry, receipt-ordered.  For each present partition the
committed rows are rebuilt THROUGH THE ONE LOADER over the typed transaction
read port; the loader's root-scoped id filter binds every expected row to the
partition's root, so a row deleted, misplaced, or re-homed to the wrong
root is MISSING at readback and aborts.  For every expected id: the row
must exist, the digest recomputed from its committed item rows must equal
the stored items_hash (item-row corruption with an intact hash column
aborts — K#025 B1), and its canonical must equal the partition's candidate
canonical.  Receipt strength never depends on the model-return opt-in;
WANT-MODELS only additionally serializes each verified image into
MODELS-STORE, keyed (ROOT . OWNER), for the wire.  Deleted owners store a
nil model (their row absence is asserted by the union scope check)."
  (let (built)
    (pcase-dolist (`(,root-id ,owner-id ,ids ,expected ,deleted) owner-specs)
      (if deleted
          (when want-models
            (push (cons (cons root-id owner-id) nil) built))
        (let ((image (funcall read
                              (list :version 1 :op 'owner-model
                                    :root-id root-id :ids ids))))
          (dolist (id ids)
            (let ((h (gethash id image)))
              (unless h
                (dorg-mutate--postimage-abort
                 'core
                 "a partition row is missing or misplaced at readback"
                 :ids (list id) :root root-id))
              (let ((stored (dorg-heading-items-hash h)))
                (dorg-heading-compute-items-hash h)
                (unless (equal stored (dorg-heading-items-hash h))
                  (dorg-mutate--postimage-abort
                   'core
                   "committed item rows diverge from their stored digest"
                   :ids (list id))))
              (unless (equal (gethash id expected)
                             (dorg-mutate--index-heading-canonical h))
                (dorg-mutate--postimage-abort
                 'core
                 "readback model diverges from the validated candidate"
                 :ids (list id)))))
          (when want-models
            (push (cons (cons root-id owner-id)
                        (dorg-mutate--postimage-wire-tree image owner-id))
                  built)))))
    (when want-models (setcar models-store built))))

(defun dorg-mutate--postimage-models (entries models-store)
  "Return the `:candidate-models' wire list for receipt ENTRIES.
MODELS-STORE holds the (ROOT . OWNER) → NODE alist the validator built from
the verified committed readback; a `:deleted' entry carries `:model nil'."
  (mapcar (lambda (e)
            (list :version 1
                  :root (plist-get e :root)
                  :owner-heading (plist-get e :owner-heading)
                  :parent-owner-heading (plist-get e :parent-owner-heading)
                  :model (cdr (assoc (cons (plist-get e :root)
                                           (plist-get e :owner-heading))
                                     (car models-store)))))
          entries))

(defun dorg-mutate--postimage-tuple-entry (tu)
  "Return the receipt entry for a well-typed stored tuple TU, else nil.
The closed field grammar (K#025 B3): integer root and owner-heading,
integer-or-nil parent-owner, 40-lowercase-hex candidate-id, 64-lowercase-hex
digest, boolean deleted.  Any other shape is malformed evidence."
  (and (proper-list-p tu) (= (length tu) 6)
       (integerp (nth 0 tu))
       (integerp (nth 1 tu))
       (or (null (nth 2 tu)) (integerp (nth 2 tu)))
       (stringp (nth 3 tu))
       (string-match-p "\\`[0-9a-f]\\{40\\}\\'" (nth 3 tu))
       (stringp (nth 4 tu))
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (nth 4 tu))
       (memq (nth 5 tu) '(nil t))
       (list :version 1
             :root (nth 0 tu)
             :owner-heading (nth 1 tu)
             :parent-owner-heading (nth 2 tu)
             :candidate-id (nth 3 tu)
             :digest (nth 4 tu)
             :deleted (nth 5 tu))))

(defun dorg-mutate--postimage-entries-from-payload (payload)
  "Parse a stored `postimages_v1' PAYLOAD back into receipt entries, or nil.
nil (malformed, mistyped, or unreadable) makes the row behave as LEGACY
evidence rather than fabricating receipt entries — fail closed.  Three
fences make this one of the two vetted raw reader seams the CLI
reader-census test pins: (1) the closed tuple grammar contains only
integers, lowercase-hex strings, nil, and t, so a `#' ANYWHERE in the
payload is malformed before the reader runs — this bans read-time
evaluation (`#.'), object/bytecode syntax, and cyclic labels wholesale
without binding reader state; (2) exactly ONE form is consumed — trailing
bytes after it are malformed; (3) every tuple passes the closed field-type
validation of `dorg-mutate--postimage-tuple-entry'."
  (when (and (stringp payload)
             (not (string-match-p "#" payload)))
    (pcase (condition-case nil (read-from-string payload) (error nil))
      (`(,tuples . ,end)
       (when (and (string-match-p "\\`[[:space:]]*\\'" (substring payload end))
                  (proper-list-p tuples)
                  tuples)
         (let ((entries (mapcar #'dorg-mutate--postimage-tuple-entry tuples)))
           (unless (memq nil entries) entries)))))))

(defun dorg-mutate--postimage-reconstruct-models (source commit entries
                                                         &optional query-fn)
  "Rebuild `:candidate-models' AS OF settled COMMIT over ENTRIES' owner scope.
The settled-replay reconstruction (tracked-regression C2): legitimate evidence
because assert (1) proved committed == candidate at apply time, so the
committed rows AT THAT COMMIT are the candidate models.  Reads are bound to
the exact recovered commit — never current state — and flow through the one
loader and the one wire serializer.  QUERY-FN routes the reads (the replay
seam runs under the held store gate, where ordinary SQL is lease-refused,
so it supplies the leased session's read function)."
  (let ((dolt-dir (dorg-mutate-source-dolt-dir source)))
    (mapcar
     (lambda (e)
       (let ((root (plist-get e :root))
             (owner (plist-get e :owner-heading)))
         (list :version 1
               :root root
               :owner-heading owner
               :parent-owner-heading (plist-get e :parent-owner-heading)
               :model
               (unless (plist-get e :deleted)
                 (let* ((ids (dorg-mutate--index-owner-local-ids
                              source root owner commit query-fn))
                        (image (dorg-model-load-headings
                                dolt-dir root ids commit nil query-fn)))
                   (dorg-mutate--postimage-wire-tree image owner))))))
     entries)))

(defun dorg-mutate--postimage-plan (index-context effects &optional want-models)
  "Return the receipt entries and transaction-proof validator for one attempt.
INDEX-CONTEXT is the attempt's immutable candidate-state context; EFFECTS are
the validated typed row effects its projection produced.  Returns
\(:entries RECEIPT-ENTRIES :validate VALIDATE-FN [:models-fn MODELS-FN]):
ENTRIES is the versioned `:postimages' receipt (one entry per affected owner
partition — nil for an identity-less source, which materializes none), and
VALIDATE-FN is the typed transaction-read-port wrapper running assert (1) then
assert (2).  Both are computed HERE, from the same single reads of the vault-copied
context, so the digested candidate rows and the asserted candidate rows
cannot drift.

The per-partition proof (`dorg-mutate--postimage-assert-partitions') is
UNCONDITIONAL — root-bound row presence, item-rows digest recomputation,
and canonical equality run for every affected partition on every apply
\(K#025 B1).  WANT-MODELS (tracked-regression AC4) only additionally serializes
the verified readback into MODELS-FN (nullary, called only after the batch
returns) for the wire `:candidate-models'."
  (let* ((partitions (dorg-ext-index-context-owner-partitions index-context))
         (candidate-id (dorg-ext-index-context-candidate-id index-context))
         (entries (mapcar (lambda (p)
                            (dorg-mutate--postimage-entry p candidate-id))
                          partitions))
         (expectations (dorg-mutate--postimage-expectations
                        index-context partitions))
         (row-checks (dorg-mutate--postimage-row-checks effects))
         (extension-tables
          (delete-dups
           (delq nil (mapcar (lambda (effect)
                               (plist-get effect :table))
                             effects))))
         ;; For an ordinary same-root apply every union-scope row must sit
         ;; under the context root; a cross-root aggregate spans two roots,
         ;; where root placement is bound per partition by the loader's
         ;; root-scoped filter instead.
         (union-root (unless (dorg-ext-index-context-from-root-id
                              index-context)
                       (dorg-ext-index-context-root-id index-context)))
         (owner-specs
          (cl-mapcar
           (lambda (p e)
             (let ((img (dorg-ext-owner-partition-candidate-postimage p))
                   (expected (make-hash-table :test 'eql))
                   (ids nil))
               (when img
                 (maphash (lambda (id h)
                            (push id ids)
                            (puthash id (dorg-mutate--index-heading-canonical h)
                                     expected))
                          img))
               (list (plist-get e :root)
                     (plist-get e :owner-heading)
                     (sort ids #'<)
                     expected
                     (plist-get e :deleted))))
           partitions entries))
         (models-store (list nil)))
    (append
     (list :entries entries
           :validate
           (list
            :mode 'transaction-read-port
            :extension-tables extension-tables
            :function
            (lambda (read)
              (dorg-ext-run-index-validators index-context 'transaction)
              (dorg-mutate--postimage-assert-core
               read (car expectations) (cdr expectations) union-root)
              (dorg-mutate--postimage-assert-partitions
               read owner-specs want-models models-store)
              (dorg-mutate--postimage-assert-rows read row-checks))))
     (when want-models
       (list :models-fn
             (lambda ()
               (dorg-mutate--postimage-models entries models-store)))))))


;;; ─── Per-store mutation gate + HEAD-equality CAS + bounded re-plan (rsuwn) ─
;; The DOrg-owned narrow mutation-linearization boundary.  Both Dolt executors
;; route their commit through ONE connection-affine gate (a Dolt-server named
;; lock on a tracked-regression lease) so [ HEAD re-read -> commit-decision ->
;; DOLT_COMMIT -> post-HEAD ] is indivisible across cooperating processes and
;; auto-releases if the writer dies.  The whole-root LOAD + simulate stays
;; OUTSIDE the gate (off the critical path).  A commit occurs ONLY while the
;; gate is held AND the in-gate HEAD still equals the plan base, so no stale
;; plan is ever committed (serializable).  A HEAD that moved since planning
;; either returns terminal `:reason overlap' (a heading this intent writes was
;; changed — first-committer-wins) or re-plans at the new HEAD under a bounded
;; deadline.  This lets typed-API clients drop any outer coarse lock.

(defcustom dorg-mutate-gate-acquire-timeout 30
  "Seconds to wait for the per-store mutation gate before signalling `contended'.
Bounds a single `GET_LOCK' acquire on the canonical store lock; on expiry the
mutation returns terminal `:reason contended' (GET_LOCK waiter order is
undefined, so this is a liveness bound, not a fairness guarantee)."
  :type 'integer :group 'dorg)

(defcustom dorg-mutate-gate-deadline 60
  "Wall-clock seconds bounding the re-plan loop before terminal `stale'.
A mutation that keeps losing its base to concurrent committers re-plans at each
new HEAD until this deadline.  The same budget bounds an op-id-authorized full
replan after a rollback-confirmed serialization failure; old SQL is discarded.
Expiry returns `:reason stale' rather than livelocking."
  :type 'number :group 'dorg)

(defvar dorg-mutate--gate-timings nil
  "When non-nil, a 4-slot float vector [GATE-WAIT GATE-HOLD SQL COMMIT] that the
gate primitives accumulate seconds into for one `dorg-mutate--dolt-gated-apply'
call.  Bound per apply so re-plans accumulate; nil disables measurement.  Slot 0
is the `GET_LOCK' acquire wait, slot 1 the whole gate-held window (acquire
success through release, EXCLUDING the outside-gate plan), slot 2 the commit SQL
batch, slot 3 the post-commit HEAD read.")

(defun dorg-mutate--acquire-gate (lease key op-id dolt-dir deadline)
  "Acquire `GET_LOCK' on KEY for LEASE, bounded by DEADLINE (wall-clock `float-time').
Contention retries acquisition under ONE shared wall-clock DEADLINE with bounded
exponential backoff (§2.7) — the SAME deadline the re-plan loop uses, so a
mutation's total gate wait + re-plan cost is bounded by one budget rather than
two independent timeouts.  A non-blocking `GET_LOCK(0)' probe is attempted every
iteration FIRST, so an uncontended lock is taken immediately even at/after the
deadline (a zero deadline still admits an uncontended writer; only genuine
contention yields terminal `contended').  DEADLINE nil defaults to
`dorg-mutate-gate-acquire-timeout' from now (the direct/legacy single-acquire
bound).  Signals typed `:reason contended' (echoing OP-ID) on deadline expiry."
  (let ((dl (or deadline (+ (float-time) dorg-mutate-gate-acquire-timeout)))
        (backoff 0.05))
    (catch 'dorg-mutate--gate-acquired
      (while t
        ;; Immediate non-blocking probe: an available lock is always taken.
        (when (dorg-sql--lease-get-lock lease key 0)
          (throw 'dorg-mutate--gate-acquired t))
        (when (<= (- dl (float-time)) 0)
          (apply #'dorg-mutate--raise
                 'contended
                 (format "could not acquire the mutation gate for %s within the deadline"
                         dolt-dir)
                 :source-kind 'dolt
                 (when op-id (list :op-id op-id))))
        ;; Block up to the per-try cap, bounded by the remaining deadline, then
        ;; back off and retry (GET_LOCK integer-second granularity).
        (let ((budget (max 1 (floor (min dorg-mutate-gate-acquire-timeout
                                         (- dl (float-time)))))))
          (when (dorg-sql--lease-get-lock lease key budget)
            (throw 'dorg-mutate--gate-acquired t)))
        (sleep-for (min backoff (max 0.0 (- dl (float-time)))))
        (setq backoff (min 1.0 (* backoff 2)))))))

(defun dorg-mutate-call-with-store-gate (dolt-dir fn &optional op-id deadline)
  "Acquire an exclusive lease + `GET_LOCK' on DOLT-DIR's store; call FN with (LEASE KEY).
The gate pins the whole critical section to ONE leased session (tracked-regression):
no cache relookup or reconnect, and a mid-lease transport failure aborts
fail-closed.  `RELEASE_LOCK' runs on the same session on unwind; a broken
session has already auto-released server-side and is not re-released (which
would mask the original error).  Acquisition retries under DEADLINE (wall-clock
`float-time', shared with the re-plan loop, §2.7); on expiry it signals typed
`:reason contended' (echoing OP-ID when supplied).  DEADLINE nil uses a single
`dorg-mutate-gate-acquire-timeout' bound.  When `dorg-mutate--gate-timings' is
bound it records the acquire wait and the gate-held window into that vector."
  (dorg-sql--with-lease (lease dolt-dir)
    (let ((key (dorg-sql--mutate-lock-key lease))
          (wait0 (float-time)))
      (dorg-mutate--acquire-gate lease key op-id dolt-dir deadline)
      (when dorg-mutate--gate-timings
        (cl-incf (aref dorg-mutate--gate-timings 0) (- (float-time) wait0)))
      (let ((hold0 (float-time)))
        (unwind-protect
            (funcall fn lease key)
          (condition-case _e
              (dorg-sql--lease-release-lock lease key)
            (dorg-sql-lease-broken nil))
          (when dorg-mutate--gate-timings
            (cl-incf (aref dorg-mutate--gate-timings 1) (- (float-time) hold0))))))))

(defun dorg-mutate--pin-insert-heading-ids (patches)
  "Return PATCHES with a stable id assigned to every tree insert lacking one.
The id is allocated ONCE here (`dorg-id-generate', identical to the per-simulate
allocation `dorg-mutate--ps-insert-heading' would otherwise do) so that a
gated-apply re-plan loop re-simulating the SAME patches reuses the SAME id — a
replay commits BYTE-IDENTICAL heading rows (A16.0 id-stability).  Caller-pinned
ids are preserved at their nodes, while unpinned descendants are still pinned
recursively.  Input patches are never mutated.  Generated values are injected
raw, exactly as the simulate-time fallback would produce them, so pinning
changes only WHEN each id is minted, never its shape."
  (cl-labels
      ((pin-heading
        (heading)
        (let ((copy (copy-tree heading)))
          (unless (plist-member copy :id)
            (setq copy (plist-put copy :id (dorg-id-generate))))
          (when (plist-member copy :children)
            (setq copy
                  (plist-put copy :children
                             (mapcar #'pin-heading
                                     (plist-get copy :children)))))
          copy)))
    (mapcar
     (lambda (patch)
       (if (equal (dorg-mutate--patch-signature patch) '(insert :tree))
           (let ((copy (copy-tree patch)))
             (plist-put copy :heading
                        (pin-heading (plist-get copy :heading))))
         patch))
     patches)))

(defun dorg-mutate--heading-write-set (sim)
  "Return the immutable heading-id write-set of a simulated mutation SIM.
The union of created/changed/deleted/moved heading ids from the typed-IR
simulation — the intent's write-set for the HEAD-equality overlap check.  It is
derived from the IMMUTABLE intent, so it is stable across re-plans."
  (delete-dups
   (append (plist-get sim :created)
           (plist-get sim :changed)
           (plist-get sim :deleted)
           (plist-get sim :moved))))

(defun dorg-mutate--attempt-identity-scope (root-id-scopes)
  "Return canonical per-root identity scope for ROOT-ID-SCOPES pairs.
Each member is (ROOT . IDS), where IDS is a bounded list or nil for the retained
whole-root fallback.  The result sorts roots and bounded ids; nil is recorded as
the explicit symbol `:all' so an attempt never silently loses its read scope."
  (mapcar
   (lambda (entry)
     (let ((root (car entry))
           (ids (cdr entry)))
       (cons root (if ids
                      (sort (delete-dups (copy-sequence ids)) #'<)
                    :all))))
   (sort (copy-sequence root-id-scopes)
         (lambda (a b) (< (car a) (car b))))))

(defun dorg-mutate--attempt-failure-data
    (plan observed-head facts &optional head-equal)
  "Return sanitized public failure facts for PLAN at OBSERVED-HEAD.
FACTS is the closed plist from `dorg-dolt--classify-db-error'; only stable code
and normalized constraint cross this boundary.  HEAD-EQUAL, when non-nil, is
recorded explicitly for the auditable equal-HEAD planner-defect verdict."
  (append
   (list :base-commit (plist-get plan :caller-base)
         :commit observed-head
         :plan-base (plist-get plan :plan-base)
         :observed-head observed-head
         :root-scope (plist-get plan :root-scope)
         :identity-scope (plist-get plan :identity-scope))
   (when (plist-member facts :transaction-phase)
     (list :transaction-phase (plist-get facts :transaction-phase)))
   (when head-equal (list :head-equal-at-failure t))
   (when (plist-member facts :db-code)
     (list :db-code (plist-get facts :db-code)))
   (when (plist-member facts :constraint)
     (list :constraint (plist-get facts :constraint)))))

(defun dorg-mutate--rolled-back-decision
    (receipt plan observed-head head-equal op-id deadline)
  "Return `replan' or signal a sanitized typed refusal for RECEIPT.
PLAN is the exact failed attempt.  OBSERVED-HEAD and HEAD-EQUAL are facts
recorded while the store gate was held.  OP-ID is the sole post-SQL retry
authorization, and DEADLINE is the existing mutation deadline.  Raw database
conditions never enter this function: RECEIPT carries classifier facts only."
  (let* ((facts (dorg-sql-transaction-rollback-data receipt))
         (rollback-confirmed (eq t (plist-get facts :rollback-confirmed)))
         (class (plist-get facts :class))
         (data (dorg-mutate--attempt-failure-data
                plan observed-head facts head-equal)))
    (cond
     ((not rollback-confirmed)
      (apply #'dorg-mutate--raise
             'invariant
             "Dolt transaction failed and rollback could not be confirmed"
             (append (list :source-kind 'dolt :detail 'rollback-failed)
                     data (when op-id (list :op-id op-id)))))
     ((eq class 'serialization)
      (if (and op-id (< (float-time) deadline))
          'replan
        (apply #'dorg-mutate--raise
               'stale
               "Dolt transaction was rolled back after a serialization conflict"
               (append (list :source-kind 'dolt :detail 'serialization)
                       data (when op-id (list :op-id op-id))))))
     ((eq class 'sibling-order-duplicate)
      (cond
       (head-equal
        (apply #'dorg-mutate--raise
               'invariant
               "planned sibling order violated its enforced unique constraint"
               (append (list :source-kind 'dolt
                             :detail 'sibling-order-planner-defect)
                       data (when op-id (list :op-id op-id)))))
       ((and op-id (< (float-time) deadline)) 'replan)
       (t
        (apply #'dorg-mutate--raise
               'stale
               "sibling-order collision occurred without proven equal HEAD"
               (append (list :source-kind 'dolt
                             :detail 'concurrent-state-unproven)
                       data (when op-id (list :op-id op-id)))))))
     ((eq class 'deterministic-constraint)
      (apply #'dorg-mutate--raise
             'invariant
             "Dolt transaction violated an enforced database constraint"
             (append (list :source-kind 'dolt :detail 'constraint)
                     data (when op-id (list :op-id op-id)))))
     ;; The postimage transaction proof refused the commit (tracked-regression):
     ;; readback diverged from the validated candidate inside the pre-commit
     ;; window.  Determinate zero-write abort — NEVER a re-plan: the divergence
     ;; is corruption evidence, not a serialization conflict, and a retry
     ;; would either re-fail or mask it.
     ((eq class 'postimage-parity)
      (apply #'dorg-mutate--raise
             'invariant
             "committed state diverged from the validated candidate; aborted pre-commit"
             (append (list :source-kind 'dolt :detail 'postimage-parity)
                     (when (plist-member facts :parity-plane)
                       (list :parity-plane (plist-get facts :parity-plane)))
                     data (when op-id (list :op-id op-id)))))
     ((eq class 'mutation-validator)
      (apply #'dorg-mutate--raise
             (or (plist-get facts :mutation-reason) 'invariant)
             "Dolt transaction validator vetoed the candidate"
             (append (list :source-kind 'dolt
                           :detail (or (plist-get facts :mutation-detail)
                                       'candidate-validator-veto))
                     data (when op-id (list :op-id op-id)))))
     (t
      (apply #'dorg-mutate--raise
             'invariant
             "Dolt transaction failed"
             (append (list :source-kind 'dolt :detail 'database-failure)
                     data (when op-id (list :op-id op-id))))))))

(defun dorg-mutate--gated-result-with-timings (result plan-secs vec)
  "Attach a `:timings' plist to a gated-apply RESULT.
PLAN-SECS is the accumulated outside-gate plan time; VEC is the
`dorg-mutate--gate-timings' accumulator [GATE-WAIT GATE-HOLD SQL COMMIT].  The
phases are distinct and `:gate-hold' EXCLUDES the whole-root `:plan' (the load
is OUTSIDE the lock), so the read plane can attribute latency (A11)."
  (if (and result (plistp result))
      (plist-put result :timings
                 (list :gate-wait (aref vec 0)
                       :gate-hold (aref vec 1)
                       :plan plan-secs
                       :sql (aref vec 2)
                       :commit (aref vec 3)))
    result))

(defun dorg-mutate--dolt-idempotent-result
    (lease gen-id op-id orig-base root-id &optional source want-models)
  "Build the idempotent :ok result for an already-committed OP-ID under LEASE.
GEN-ID is the current evidence row's generation nonce; the original creation
commit is recovered generation-safely (§2.6.3) so a replay returns the SAME
commit it first produced, WITHOUT re-applying any SQL.  The affected-id lists
are nil: a settled replay reconstructs no core deltas — its load-bearing
outputs are the recovered `:commit', the echoed `:op-id', the `:idempotent'
flag, and the stored postimage evidence (tracked-regression).

Evidence law: a row carrying `postimages_v1' re-emits its `:postimages'
verbatim, and WANT-MODELS additionally reconstructs `:candidate-models' AS
OF the recovered commit over the stored owner scope (SOURCE supplies the
identity policy).  A LEGACY row (no stored payload) with WANT-MODELS is the
typed `legacy-evidence-unavailable' refusal carrying the settled commit and
op identity — never a verified success; without WANT-MODELS the old
idempotent result is returned unchanged."
  (let* ((commit (dorg-sql--mutation-log-recover-commit lease gen-id))
         (stored (dorg-sql--mutation-log-postimages lease op-id))
         (entries (and stored
                       (dorg-mutate--postimage-entries-from-payload stored))))
    (when (and want-models (null entries))
      (dorg-mutate--raise
       'legacy-evidence-unavailable
       "settled evidence predates postimage receipts; adjudicate via the settled commit"
       :source-kind 'dolt :op-id op-id :commit commit
       :root-id root-id :base-commit orig-base))
    (append
     (list :ok t :dry-run nil :source-kind 'dolt :root-id root-id
           :base-commit orig-base
           :commit commit
           :op-id op-id :idempotent t
           :created nil :changed nil :deleted nil
           :extensions nil :invalidation nil)
     (when entries (list :postimages entries))
     (when (and want-models entries source)
       (list :candidate-models
             (dorg-mutate--postimage-reconstruct-models
              source commit entries
              ;; Under the held gate ordinary SQL is lease-refused; the
              ;; AS-OF reconstruction reads on the leased session.
              (lambda (sql)
                (dorg-sql--coerce-types
                 (dorg-sql--lease-exec lease sql)))))))))

(defun dorg-mutate--dolt-op-id-settled (source options op-id intent-digest orig-base)
  "Return an idempotent :ok result if OP-ID's matching evidence is settled, else nil.
The planning-failure reconciliation seam: acquires the store gate on a fresh
lease (so any in-flight prior commit under this op-id has settled), ensures the
log (and its postimage evidence column), and looks up OP-ID.  Present +
matching INTENT-DIGEST -> idempotent result (recovered original commit, with
the stored postimage evidence law of `dorg-mutate--dolt-idempotent-result');
present with a DIFFERENT digest -> terminal `idempotency-key-mismatch';
absent -> nil (the caller re-raises its genuine planning failure).  SOURCE and
OPTIONS carry the identity policy and the caller's evidence request."
  (dorg-mutate-call-with-store-gate
   (dorg-mutate-source-dolt-dir source)
   (lambda (lease _key)
     (dorg-sql--ensure-mutation-log lease)
     (dorg-sql--ensure-mutation-log-postimages lease)
     (let ((ev (dorg-sql--mutation-log-lookup lease op-id)))
       (cond
        ((null ev) nil)
        ((equal (cdr ev) intent-digest)
         (dorg-mutate--dolt-idempotent-result
          lease (car ev) op-id orig-base
          (dorg-mutate-source-root-id source)
          source (plist-get options :return-candidate-models)))
        (t (dorg-mutate--raise
            'idempotency-key-mismatch
            "op-id replayed with a different intent digest than its evidence"
            :source-kind 'dolt :op-id op-id)))))
   op-id))

(defun dorg-mutate--dolt-gated-apply (source options orig-base plan-fn
                                             &optional default-message)
  "Linearize a Dolt mutation under the per-store gate; return the apply result.
ORIG-BASE is the caller's immutable optimistic base commit (`:base-commit', or
nil for a fresh apply).  DEFAULT-MESSAGE is the commit message when OPTIONS
carries no `:message'.  OPTIONS may carry a public `:op-id' (caller-generated
once, reused verbatim across retries/restarts) and the DOrg-computed
`:intent-digest' binding it to the immutable intent; when present they engage
the restart-safe exactly-once protocol (§2.6).  PLAN-FN is a nullary closure run
OUTSIDE the gate that LOADS the current tree, simulates, collects participant
projection, and returns an immutable attempt plist:
  :caller-base  immutable caller optimistic token (or nil)
  :plan-base    actual HEAD used by this load/simulate/lower pass
  :root-scope   sorted roots read or written by this attempt
  :identity-scope per-root bounded ids, or explicit :all fallback
  :sql          full core+participant SQL list (nil = empty net effect)
  :write-set    heading ids the intent writes (from `dorg-mutate--heading-write-set')
  :make-result  (lambda (NEW-HEAD) -> apply result plist)
  :empty        result plist for the empty net-effect case (no gate, no commit)
The core commits ONLY when in-gate HEAD equals this attempt's `:plan-base'.  A
moved caller/plan base returns terminal `:reason overlap' when a written heading
changed, else releases the gate and builds a fresh attempt under the existing
deadline.  A rollback-confirmed serialization failure may take the same full
replan path only when OP-ID authorizes idempotent post-SQL retry; generated SQL
is never replayed.  Commit failures are sanitized through the single database
classifier before a typed refusal reaches callers.

EXACTLY-ONCE (op-id present): every attempt runs a GATE-FIRST idempotency
preflight — settled matching evidence short-circuits to an idempotent `:ok'
carrying the ORIGINAL commit (no re-apply); a different intent digest is terminal
`idempotency-key-mismatch' with zero SQL.  The evidence row is committed in the
SAME transaction as the data.  A transport failure during/after DOLT_COMMIT is
terminal `:reason indeterminate' (the caller reconciles with the same op-id).  A
typed transport failure while the commit request is in flight is classified by
condition type plus transaction phase before any rollback or database-message
classification; it never sends ROLLBACK and carries
`:detail commit-indeterminate'.  A planning failure whose target was already
consumed by this op-id's own prior commit is reconciled to idempotent `:ok'
rather than surfaced.  `:op-id' is echoed on every result and terminal
condition."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (root-id (dorg-mutate-source-root-id source))
         (msg (or (plist-get options :message) default-message "dorg-mutate"))
         (op-id (plist-get options :op-id))
         (intent-digest (plist-get options :intent-digest))
         (deadline (+ (float-time) dorg-mutate-gate-deadline))
         (dorg-mutate--gate-timings (make-vector 4 0.0))
         (t-plan 0.0)
         ;; ONE generation nonce per attempt-series, minted at first plan and
         ;; PRESERVED across every re-plan (§2.6.2), so the dolt_diff recovery
         ;; keyed on it always resolves this generation's creation commit.
         (gen-id (and op-id (dorg-mutate--new-gen-id)))
         ;; The caller token is checked once, separately from the actual plan
         ;; base.  Conflating these was the pre-f4la4 implicit h-plan defect.
         (caller-base-checked (null orig-base))
         (last-observed orig-base))
    (dorg-mutate--gated-result-with-timings
     (catch 'dorg-mutate--gate-done
       (while t
         (let* ((plan0 (float-time))
                (plan (condition-case perr
                          (let ((built (funcall plan-fn)))
                            ;; The first attempt may have been built by the
                            ;; public plan phase before this gate call.  Keep
                            ;; its measured load/simulate time in the existing
                            ;; :plan bucket instead of making the optimization
                            ;; erase the dominant timing evidence.
                            (cl-incf
                             t-plan
                             (+ (- (float-time) plan0)
                                (or (plist-get built
                                               :precomputed-plan-seconds)
                                    0.0)))
                            built)
                        (dorg-mutate-error
                         (cl-incf t-plan (- (float-time) plan0))
                         ;; A planning failure MAY be an idempotent replay whose
                         ;; target was already consumed by this op-id's OWN prior
                         ;; commit.  Consult settled evidence before surfacing it.
                         (if (and op-id
                                  (memq (dorg-mutate-error-reason perr)
                                        '(invalid-target invariant)))
                             (let ((hit (dorg-mutate--dolt-op-id-settled
                                         source options op-id intent-digest
                                         orig-base)))
                               (if hit (throw 'dorg-mutate--gate-done hit)
                                 (signal (car perr) (cdr perr))))
                           (signal (car perr) (cdr perr))))))
                (plan-base (plist-get plan :plan-base)))
          (unless (stringp plan-base)
            (dorg-mutate--raise
             'invariant "Dolt planner did not record an actual plan HEAD"
             :source-kind 'dolt :detail 'invalid-plan-attempt))
          ;; No op-id + empty net effect: no gate, no commit (unchanged path).
          (when (and (null op-id) (null (plist-get plan :sql)))
            (throw 'dorg-mutate--gate-done (plist-get plan :empty)))
          (let
              ((action
                (catch 'dorg-mutate--retry-after-rollback
                  (dorg-mutate-call-with-store-gate
                   dolt-dir
                   (lambda (lease _key)
                     ;; ── Gate-first idempotency preflight (§2.6.4) ──
                     (when op-id
                       (dorg-sql--ensure-mutation-log lease)
                       ;; Additive postimage evidence column (tracked-regression);
                       ;; working-set DDL outside the transaction, persisted
                       ;; by this mutation's own DOLT_COMMIT.
                       (dorg-sql--ensure-mutation-log-postimages lease)
                       (let ((ev (dorg-sql--mutation-log-lookup lease op-id)))
                         (when ev
                           (if (equal (cdr ev) intent-digest)
                               (throw 'dorg-mutate--gate-done
                                      (dorg-mutate--dolt-idempotent-result
                                       lease (car ev) op-id orig-base root-id
                                       source
                                       (plist-get options
                                                  :return-candidate-models)))
                             (dorg-mutate--raise
                              'idempotency-key-mismatch
                              "op-id replayed with a different intent digest than its evidence"
                              :source-kind 'dolt :op-id op-id)))))
                     (when (null (plist-get plan :sql))
                       (let ((empty (copy-sequence (plist-get plan :empty))))
                         (throw 'dorg-mutate--gate-done
                                (if op-id (plist-put empty :op-id op-id) empty))))
                     (let ((h-now (dorg-sql--lease-scalar
                                   lease "SELECT HASHOF('HEAD') AS h")))
                       (setq last-observed h-now)
                       ;; First validate the immutable caller token.  Even though
                       ;; PLAN-FN loaded current HEAD, a stale caller must still
                       ;; receive first-committer-wins overlap semantics.  A
                       ;; disjoint change releases the gate and plans again.
                       (if (and (not caller-base-checked)
                                (not (equal orig-base h-now)))
                           (let* ((changed
                                   (dorg-sql--lease-changed-heading-ids
                                    lease orig-base h-now))
                                  (overlap
                                   (cl-intersection
                                    (plist-get plan :write-set) changed :test #'eql)))
                             (setq caller-base-checked t)
                             (if overlap
                                 (apply #'dorg-mutate--raise
                                        'overlap
                                        "a heading this mutation writes was changed since its base commit"
                                        :source-kind 'dolt
                                        :base-commit orig-base :commit h-now
                                        :overlap overlap
                                        (when op-id (list :op-id op-id)))
                               'replan))
                         (setq caller-base-checked t)
                         (if (not (equal h-now plan-base))
                             ;; The attempt itself went stale after planning.
                             (let* ((changed
                                     (dorg-sql--lease-changed-heading-ids
                                      lease plan-base h-now))
                                    (overlap
                                     (cl-intersection
                                      (plist-get plan :write-set) changed :test #'eql)))
                               (if overlap
                                   (apply #'dorg-mutate--raise
                                          'overlap
                                          "a heading this mutation writes was changed since its plan"
                                          :source-kind 'dolt
                                          :base-commit plan-base :commit h-now
                                          :overlap overlap
                                          (when op-id (list :op-id op-id)))
                                 'replan))
                           ;; Equal attempt HEAD under the held gate: this is the
                           ;; only branch allowed to execute generated SQL.
                           (let* ((sql0 (float-time))
                                  (batch
                                   (if op-id
                                       (append
                                        (plist-get plan :sql)
                                        (list
                                         (dorg-sql--mutation-log-insert-sql
                                          op-id gen-id intent-digest
                                          (truncate (float-time))
                                          ;; Candidate-canonical digests, in
                                          ;; the SAME transaction as the data;
                                          ;; entitled by the validator's
                                          ;; assert (1) below (tracked-regression).
                                          (dorg-mutate--postimage-log-payload
                                           (plist-get plan :postimages)))))
                                     (plist-get plan :sql))))
                             (unwind-protect
                                 (condition-case e
                                     ;; Opt into the sanitized rollback receipt.
                                     ;; The attempt's postimage validator runs
                                     ;; in the typed transaction-read window
                                     ;; after all statements and before
                                     ;; DOLT_COMMIT.
                                     (dorg-sql--lease-batch
                                      lease batch msg
                                      (plist-get plan :validate) t)
                                   (dorg-sql-transaction-rolled-back
                                    (when (eq 'replan
                                              (dorg-mutate--rolled-back-decision
                                               e plan h-now t op-id deadline))
                                      (throw 'dorg-mutate--retry-after-rollback
                                             'replan)))
                                   (dorg-sql-transaction-rollback-failed
                                    (dorg-mutate--rolled-back-decision
                                     e plan h-now t op-id deadline))
                                   ;; DOLT_COMMIT may have landed even when its
                                   ;; response hash was lost.  This exact child
                                   ;; condition must precede `lease-broken': it
                                   ;; is terminal, never a rollback-authorized
                                   ;; re-plan, and carries only closed facts.
                                   (dorg-sql-commit-indeterminate
                                    (if op-id
                                        (let ((facts
                                               (dorg-sql-commit-indeterminate-data
                                                e)))
                                          (apply
                                           #'dorg-mutate--raise
                                           'indeterminate
                                           (concat
                                            "commit outcome is indeterminate; "
                                            "reconcile with the same op-id")
                                           :source-kind 'dolt
                                           :detail 'commit-indeterminate
                                           :op-id op-id
                                           :base-commit orig-base
                                           :plan-base plan-base
                                           :observed-head h-now
                                           :root-scope
                                           (plist-get plan :root-scope)
                                           :identity-scope
                                           (plist-get plan :identity-scope)
                                           (when
                                               (plist-member
                                                facts :transaction-phase)
                                             (list
                                              :transaction-phase
                                              (plist-get
                                               facts :transaction-phase)))))
                                      (signal (car e) (cdr e))))
                                   ;; A broken session may have committed.  Keep
                                   ;; the existing op-id reconciliation contract.
                                   (dorg-sql-lease-broken
                                    (if op-id
                                        (dorg-mutate--raise
                                         'indeterminate
                                         "transport failure during/after commit; reconcile with the same op-id"
                                         :source-kind 'dolt :op-id op-id
                                         :base-commit orig-base)
                                      (signal (car e) (cdr e))))
                                   (dorg-mutate-error
                                    (signal (car e) (cdr e)))
                                   ;; Defensive fallback: never copy a raw SQL
                                   ;; condition into the public refusal.
                                   (error
                                    (dorg-mutate--raise
                                     'invariant "Dolt transaction failed"
                                     :source-kind 'dolt
                                     :detail 'database-failure
                                     :base-commit orig-base
                                     :plan-base plan-base
                                     :observed-head h-now)))
                               (cl-incf (aref dorg-mutate--gate-timings 2)
                                        (- (float-time) sql0))))
                           (let* ((commit0 (float-time))
                                  (new-head
                                   (dorg-sql--lease-scalar
                                    lease "SELECT HASHOF('HEAD') AS h"))
                                  (result
                                   (funcall (plist-get plan :make-result)
                                            new-head)))
                             (cl-incf (aref dorg-mutate--gate-timings 3)
                                      (- (float-time) commit0))
                             (throw 'dorg-mutate--gate-done
                                    (if op-id
                                        (plist-put result :op-id op-id)
                                      result)))))))
                   op-id deadline))))
            ;; Reached only after the gate released through a safe full-replan
            ;; decision (moved HEAD or rollback-confirmed serialization).
            (when (and (eq action 'replan) (> (float-time) deadline))
              (apply #'dorg-mutate--raise
                     'stale
                     "re-plan deadline exhausted under sustained concurrent writers"
                     :source-kind 'dolt :base-commit orig-base
                     :commit last-observed
                     (when op-id (list :op-id op-id))))))))
     t-plan dorg-mutate--gate-timings)))

;;; ─── Operation-qualified pre-apply consistency bar (tracked-regression) ─────
;; Immutable S0, the candidate postimage, and the DECLARED typed effects must be
;; mutually consistent BEFORE apply.  DOrg refuses a candidate ONLY when the
;; typed request declared a child identity PRESERVED (no delete/subtree-delete
;; targets it) yet the candidate drops it — the tracker-cli-id8i6 incident.  It is NOT a
;; blanket cardinality veto: an explicit typed deletion (single or subtree) of any
;; number of children is consistent.  Refusal is pre-apply within one execution,
;; never a rollback of an earlier committed request.

(defun dorg-mutate--image-descendants (image id)
  "Return the transitive descendant ids of ID within IMAGE (id -> heading hash)."
  (let (direct)
    (maphash (lambda (cid h)
               (when (eql (dorg-heading-parent-id h) id) (push cid direct)))
             image)
    (append direct
            (apply #'append
                   (mapcar (lambda (c) (dorg-mutate--image-descendants image c))
                           direct)))))

(defun dorg-mutate--declared-removed-ids (patches image)
  "Return the set of ids the typed PATCHES explicitly declare removed.
A `(delete :heading)' declares its target id; a `(delete :subtree)' declares the
target plus all its descendants in IMAGE (the S0 scope).  This is DOrg's
expected-effect normalization of the public typed request — never a caller
payload or hint channel."
  (let (ids)
    (dolist (patch patches)
      (let ((sig (dorg-mutate--patch-signature patch)))
        (when (eq (car sig) 'delete)
          (let ((id (dorg-mutate--patch-address patch)))
            (push id ids)
            (when (eq (cadr sig) :subtree)
              (setq ids (nconc (dorg-mutate--image-descendants image id) ids)))))))
    (delete-dups ids)))

(defun dorg-mutate--candidate-drop-violations (orig candidate declared)
  "Return the ids ORIG holds that CANDIDATE dropped WITHOUT a DECLARED removal.
ORIG/CANDIDATE are id -> heading hashes (S0 scope and candidate post-state);
DECLARED is the set of ids the typed request declared removed.  A non-empty result
is an operation-qualified consistency violation: the request implicitly PRESERVED
those identities yet the candidate dropped them.  An explicitly declared deletion
is never a violation, so a large declared deletion is accepted (no cardinality
veto)."
  (let (violations)
    (maphash (lambda (id _h)
               (when (and (not (gethash id candidate))
                          (not (memql id declared)))
                 (push id violations)))
             orig)
    violations))

(defun dorg-mutate--assert-candidate-consistency (orig candidate patches)
  "Signal a typed `invariant' error when CANDIDATE drops an S0 child ORIG holds
that PATCHES did not declare removed (tracked-regression operation-qualified bar).
ORIG/CANDIDATE are the S0 and candidate id -> heading hashes; PATCHES is the typed
request.  Returns nil when consistent.  Refusal is pre-apply within one execution,
never a rollback of an earlier committed request."
  (let ((violations (dorg-mutate--candidate-drop-violations
                     orig candidate
                     (dorg-mutate--declared-removed-ids patches orig))))
    (when violations
      (dorg-mutate--raise
       'invariant
       "candidate drops child identities the typed request preserved"
       :detail 'undeclared-child-drop
       :dropped (sort (copy-sequence violations) #'<)))))

(defun dorg-mutate--cross-root-assert-consistency (sim patch)
  "Assert the cross-root candidate SIM is consistent with the AGGREGATE S0 and the
declared PATCH (tracked-regression #5).  Unions both roots' S0 (`:src-orig'/`:dst-orig')
and candidate (`:src-ps'/`:dst-ps') images and refuses a candidate that drops a
preserved identity.  This ONE helper is shared by the cross-root dry-run plan-data
and apply executor, so plan and apply validate identically (no dry-run/apply
divergence)."
  (dorg-mutate--assert-candidate-consistency
   (dorg-mutate--index-merge-images
    (plist-get sim :src-orig) (plist-get sim :dst-orig))
   (dorg-mutate--index-merge-images
    (and (plist-get sim :src-ps) (dorg-mutate--ps-headings (plist-get sim :src-ps)))
    (and (plist-get sim :dst-ps) (dorg-mutate--ps-headings (plist-get sim :dst-ps))))
   (list patch)))

(defun dorg-mutate--dolt-attempt-from-sim
    (source patches options sim)
  "Build one immutable same-root execution attempt for SOURCE/PATCHES.
OPTIONS supplies freshness/idempotency policy and SIM is the sole full
load/apply/diff/lower result for the attempt HEAD.
Participant validation/projection runs exactly once here and its SQL is folded
into the same transaction.  The returned private attempt is consumed only when
the held gate proves `:plan-base' still current; a retry builds a fresh SIM and
attempt rather than replaying this SQL."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (root-id (dorg-mutate-source-root-id source))
         (base (plist-get options :base-commit))
         (sql (plist-get sim :sql)))
    (if (null sql)
        ;; No net effect (e.g. a create-then-delete that nets to nothing): no
        ;; participants and no commit.  Empty attempts are not threaded from
        ;; plan to apply because the no-gate short circuit cannot revalidate a
        ;; precomputed HEAD; apply builds its own current empty attempt.
        (list :sql nil
              :caller-base base
              :plan-base (plist-get sim :head-commit)
              :root-scope (list root-id)
              :identity-scope
              (dorg-mutate--attempt-identity-scope
               (list (cons root-id (plist-get sim :scope-ids))))
              :sim sim
              :projection nil
              :empty (list :ok t :dry-run nil :source-kind 'dolt
                           :root-id root-id
                           :base-commit base
                           :commit (dorg-head-commit dolt-dir)
                           :patches patches
                           :created nil :changed nil :deleted nil
                           :extensions nil :invalidation nil))
      ;; Operation-qualified pre-apply consistency bar: immutable S0, the
      ;; candidate postimage, and the declared typed effects must agree before any
      ;; write — refuse a candidate that drops a child the request preserved
      ;; (tracked-regression; propagates before the gate, aborting fail-closed).
      (dorg-mutate--assert-candidate-consistency
       (plist-get sim :orig)
       (and (plist-get sim :ps) (dorg-mutate--ps-headings (plist-get sim :ps)))
       (or (plist-get sim :patches) patches))
      ;; The index projector consumes the immutable candidate-state context
      ;; (tracked-regression) DOrg attached to SIM after core simulation — NOT a caller
      ;; payload.  Its validated derived effects are folded into the SAME
      ;; transaction; a manifest/validate refusal propagates before the gate and
      ;; before any write.  EXT-INPUTS is retired (superseded, fail-closed at the
      ;; API boundary) and no longer participates.
      (let* ((proj (dorg-ext-collect-index-projection
                    (plist-get sim :index-context)))
             (full-sql (append sql (plist-get proj :sql)))
             ;; Postimage receipt + pre-commit transaction proof (tracked-regression):
             ;; entries and validator derive from the SAME immutable context and
             ;; the SAME validated effects this attempt commits.
             (postimage (dorg-mutate--postimage-plan
                         (plist-get sim :index-context)
                         (plist-get proj :effects)
                         (plist-get options :return-candidate-models))))
        (list :sql full-sql
              :caller-base base
              :plan-base (plist-get sim :head-commit)
              :root-scope (list root-id)
              :identity-scope
              (dorg-mutate--attempt-identity-scope
               (list (cons root-id (plist-get sim :scope-ids))))
              :write-set (dorg-mutate--heading-write-set sim)
              :sim sim
              :projection proj
              :postimages (plist-get postimage :entries)
              :validate (plist-get postimage :validate)
              :make-result
              (lambda (new-head)
                (append
                 (list :ok t :dry-run nil :source-kind 'dolt :root-id root-id
                       :base-commit base :commit new-head
                       :patches patches
                       :created (plist-get sim :created)
                       :changed (plist-get sim :changed)
                       :deleted (plist-get sim :deleted))
                 ;; A receipt entry attests a NEW commit's postimage.  Under
                 ;; the held gate HEAD advanced iff this apply committed; a
                 ;; value-identical batch Dolt answers "nothing to commit"
                 ;; mints no commit and must emit no postimages (the
                 ;; zero-effect law, tracked-regression).
                 (when (and (plist-get postimage :entries)
                            (not (equal new-head
                                        (plist-get sim :head-commit))))
                   (append
                    (list :postimages (plist-get postimage :entries))
                    ;; Wire v1 (AC4): serialized from the exact structs the
                    ;; validator read back and asserted in this transaction.
                    (when-let ((models-fn (plist-get postimage :models-fn)))
                      (list :candidate-models (funcall models-fn)))))
                 (list :extensions (plist-get proj :extensions)
                       :invalidation (list :roots (list root-id)
                                           :ext-dirty (plist-get proj :ext-dirty)
                                           :head new-head
                                           :dolt-dir dolt-dir
                                           :file
                                           (dorg-mutate-source-file source))))))))))

(defun dorg-mutate--dolt-build-attempt (source patches options)
  "Build one attempt for SOURCE/PATCHES with OPTIONS.
The attempt is based on exactly one simulation."
  (dorg-mutate--dolt-attempt-from-sim
   source patches options
   (dorg-mutate--dolt-simulate source patches options)))

(defun dorg-mutate--dolt-attempt-plan-data (attempt)
  "Return dry-run semantic fields derived from private ATTEMPT.
An invalidation preview is built whenever an index projector ran (`:extensions'
present), so a dry-run exposes the same stable invalidation fields as apply.  The
private attempt itself is included only for a non-empty apply to consume; the
public API reshaper never exposes it."
  (let* ((sim (plist-get attempt :sim))
         (proj (plist-get attempt :projection))
         (root-id (car (plist-get attempt :root-scope)))
         (base-plan (list :head-commit (plist-get sim :head-commit)
                          :db-ops (plist-get sim :db-ops)
                          :created (plist-get sim :created)
                          :changed (plist-get sim :changed)
                          :deleted (plist-get sim :deleted))))
    (if (null (plist-get attempt :sql))
        base-plan
      (append
       (list :dorg-execution-attempt attempt
             :head-commit (plist-get sim :head-commit)
             :db-ops (append (plist-get sim :db-ops)
                             (mapcar (lambda (s) (list :ext-sql s))
                                     (plist-get proj :sql)))
             :extensions (plist-get proj :extensions)
             :ext-dirty (plist-get proj :ext-dirty)
             :created (plist-get sim :created)
             :changed (plist-get sim :changed)
             :deleted (plist-get sim :deleted))
       (when (plist-get proj :extensions)
         (list :invalidation
               (list :roots (list root-id)
                     :ext-dirty (plist-get proj :ext-dirty))))))))

(defun dorg-mutate--dolt-execute
    (source patches options &optional initial-attempt)
  "Execute typed PATCHES against Dolt SOURCE, linearized per store.
Loads + simulates the batch in memory OUTSIDE the per-store mutation gate (so a
missing target / invariant violation signals before any write, and the scoped
load stays off the critical path), runs the registered index projector
(`dorg-ext-collect-index-projection') against the candidate-state context to
gather validated derived SQL,
then commits the core SQL followed by the participant SQL in ONE Dolt
transaction on the gate's LEASED session via `dorg-mutate--dolt-gated-apply'
\(so core + participant effects are atomic and roll back together).  The commit
lands only when the in-gate HEAD still equals the plan base; a concurrent
committer yields terminal `:reason overlap' (a written heading changed) or a
bounded re-plan (`:base-commit' honors the optimistic token).  An empty net
effect short-circuits with no commit.  A participant `:validate' signal aborts
before any commit.  A commit-time database failure is classified only after a
confirmed rollback and returns a sanitized `stale' or `invariant' detail.
Returns the apply result plist,
including `:extensions' and `:invalidation' scopes for the read plane.
INITIAL-ATTEMPT is the private non-empty attempt already built by plan-data.
It is returned once to the gate closure; every subsequent call is an existing
safe full-replan decision and builds a fresh attempt from the new HEAD."
  (let ((base (plist-get options :base-commit))
        (initial initial-attempt))
    (dorg-mutate--dolt-gated-apply
     source options base
     (lambda ()
       (if initial
           (prog1 initial (setq initial nil))
         (dorg-mutate--dolt-build-attempt
          source patches options))))))

;;; ─── Dolt Cross-Root (Archive) Move ──────────────────────────────────

;; The bounded cross-root slice makes a SAME-Dolt-repo,
;; DIFFERENT-root typed subtree move a supported, ID-preserving, single
;; atomic Dolt commit.  It reuses the same snapshot LOAD seam as the
;; same-root executor (`dorg-mutate--dolt-load') but loads BOTH roots from
;; one captured HEAD, validates all source/destination targets in memory,
;; and PERSISTS only `headings' row updates (root_id + parent/sort/level)
;; for the moved subtree.  Tags and heading items hang off `heading_id',
;; which does not change, so they are not deleted/reinserted: the move
;; preserves heading identity and content wholesale.
;;
;; The slice is intentionally bounded to exactly one cross-root move per
;; batch with no other patches; a mixed or multi-move batch is reported
;; unsupported (a multi-root batch scheduler is later follow-up work).  Different
;; Dolt repositories, Dolt/plain-org mixed moves, and plain-org
;; different-file moves remain unsupported even with `:allow-nonatomic t'.

(defun dorg-mutate--dolt-cross-root-patch-p (source patch)
  "Return non-nil when PATCH is a same-Dolt-dir/different-root move.
SOURCE is resolved; PATCH is typed and its private
`:to-source' (when present) is a resolved `dorg-mutate-source'.  This is
the structural cross-root shape, independent of `:preserve-ids' validity
\(which is classified by `dorg-mutate--dolt-move-support')."
  (and (equal (dorg-mutate--patch-signature patch) '(move :subtree))
       (eq (dorg-mutate-source-kind source) 'dolt)
       (let ((dest (plist-get patch :to-source)))
         (and dest
              (eq (dorg-mutate-source-kind dest) 'dolt)
              (equal (dorg-mutate-source-dolt-dir dest)
                     (dorg-mutate-source-dolt-dir source))
              (not (eql (dorg-mutate-source-root-id dest)
                        (dorg-mutate-source-root-id source)))))))

(defun dorg-mutate--dolt-cross-root-batch-patch (source patches)
  "Return the sole cross-root move of bounded PATCHES.
Returns the patch only when PATCHES has exactly one member and it is a
same-Dolt-dir/different-root move (see `dorg-mutate--dolt-cross-root-patch-p');
otherwise nil.  This is the dispatch predicate for the cross-root
executor."
  (when (and (= (length patches) 1)
             (dorg-mutate--dolt-cross-root-patch-p source (car patches)))
    (car patches)))

(defun dorg-mutate--dolt-cross-root-batch-reject (source patches)
  "Return a batch-level unsupported classification, or nil.
The fh4s slice supports exactly ONE cross-root typed subtree move per batch
with no other patches.  When PATCHES contains a cross-root move that is not the
sole patch (a mixed batch or multiple cross-root moves), the batch is
unsupported until a multi-root scheduler exists.  Each individual patch may
still classify as supported, so this batch-level gate is what keeps the
plan honest."
  (let ((cross (cl-remove-if-not
                (lambda (patch)
                  (dorg-mutate--dolt-cross-root-patch-p source patch))
                patches)))
    (when (and cross (or (> (length cross) 1) (> (length patches) 1)))
      (list :op 'move :target-kind :subtree :supported nil :reason 'executor
            :detail (if (> (length cross) 1)
                        "a cross-root batch may carry only one move"
                      "a cross-root move must be the only patch in its batch")))))

(defun dorg-mutate--dolt-cross-root-heading-update (heading root-id)
  "Return SQL UPDATE for HEADING that also sets `root_id' = ROOT-ID.
`dorg-sql-gen-heading-update' deliberately updates only same-root mutable
attributes; the cross-root archive move additionally re-homes the moved
rows into the destination root.  All other mutable fields are written
from HEADING unchanged so the row is fully consistent after the move."
  (format "UPDATE headings SET root_id = %d, parent_id = %s, sort_key = %s, level = %d, todo_keyword = %s, priority = %s, commented = %s, title = %s, post_blank = %d, items_hash = %s WHERE id = %d"
          root-id
          (if (dorg-heading-parent-id heading)
              (number-to-string (dorg-heading-parent-id heading))
            "NULL")
          (dorg-sql-gen--sort-key-hex (dorg-heading-sort-key heading))
          (dorg-heading-level heading)
          (dorg-sql-quote (dorg-heading-todo heading))
          (dorg-sql-quote (dorg-heading-priority heading))
          (if (dorg-heading-commented heading) "TRUE" "FALSE")
          (dorg-sql-quote (dorg-heading-title heading))
          (or (dorg-heading-post-blank heading) 0)
          (dorg-sql-quote (dorg-heading-items-hash heading))
          (dorg-heading-id heading)))

(defun dorg-mutate--dolt-cross-root-scope-ids (source dest patch head)
  "Return (SRC-IDS . DST-IDS) for cross-root PATCH at HEAD.
Each side is an independent bounded load slice.  Nil on either side is
reserved for that root's whole-root fallback."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (src-root (dorg-mutate-source-root-id source))
         (dst-root (dorg-mutate-source-root-id dest))
         (src-ids nil)
         (dst-ids nil))
    (cl-labels
        ((src-add (id) (when (integerp id) (cl-pushnew id src-ids :test #'eql)))
         (dst-add (id) (when (integerp id) (cl-pushnew id dst-ids :test #'eql)))
         (src-siblings (parent-id)
           (when (integerp parent-id)
             (dolist (id (dorg-mutate--dolt-child-ids
                          dolt-dir src-root parent-id head))
               (src-add id))))
         (dst-siblings (parent-id)
           (when (integerp parent-id)
             (dolist (id (dorg-mutate--dolt-child-ids
                          dolt-dir dst-root parent-id head))
               (dst-add id)))))
      (let* ((to (plist-get patch :to))
             (moving (dorg-mutate--dolt-resolve-host-id
                      source src-root (dorg-mutate--patch-address patch)
                      patch head))
             (src-parent (dorg-mutate--dolt-parent-id
                          dolt-dir src-root moving head))
             (dst-parent (dorg-mutate--dolt-resolve-host-id
                          dest dst-root
                          (dorg-mutate--patch-host (plist-get to :under))
                          patch head)))
        (src-add src-root)
        (dolist (id (dorg-mutate--dolt-subtree-ids
                     dolt-dir src-root moving head))
          (src-add id))
        (src-add moving)
        (src-add src-parent)
        (src-siblings src-parent)
        (dst-add dst-root)
        (dst-add dst-parent)
        (when (plist-member to :before)
          (dst-add
           (dorg-mutate--dolt-resolve-host-id
            dest dst-root
            (dorg-mutate--patch-anchor-address (plist-get to :before))
            patch head)))
        (when (plist-member to :after)
          (dst-add
           (dorg-mutate--dolt-resolve-host-id
            dest dst-root
            (dorg-mutate--patch-anchor-address (plist-get to :after))
            patch head)))
        (dst-siblings dst-parent)
        (cons (delete-dups (nreverse src-ids))
              (delete-dups (nreverse dst-ids)))))))

(defun dorg-mutate--dolt-cross-root-simulate (source patch options)
  "Simulate a bounded cross-root move PATCH on Dolt SOURCE.
Loads the source and destination roots from ONE captured HEAD, validates
every source/destination target in memory, computes the moved heading row
updates (root_id, parent, sort, level), and returns plan data WITHOUT
writing.  Any missing source heading, source-root move, missing/invalid
destination parent or anchor, or detected id/parent overlap signals
`dorg-mutate-error' HERE, before any SQL is produced, so a failed move
makes no partial change.  Returns a plist:

  (:head-commit HEAD :from-scope-ids IDS :to-scope-ids IDS
   :sql SQL :db-ops OPS :moved IDS :changed IDS
   :from-root-id SRC :to-root-id DST)

OPTIONS is accepted for symmetry; the executor reads `:base-commit'."
  (ignore options)
  (let* ((dest (plist-get patch :to-source))
         (to (plist-get patch :to))
         (dolt-dir (dorg-mutate-source-dolt-dir source))
         (src-root (dorg-mutate-source-root-id source))
         (dst-root (dorg-mutate-source-root-id dest))
         (head (dorg-head-commit dolt-dir))
         (scopes (dorg-mutate--dolt-cross-root-scope-ids
                  source dest patch head))
         ;; Both roots loaded from the SAME HEAD: one two-root snapshot.  The
         ;; pristine ORIG halves (never mutated) are retained as the S0 base
         ;; image for the cross-root index context (tracked-case).
         (src-loaded (dorg-mutate--dolt-load source head (car scopes)))
         (src-orig (car src-loaded))
         (src-ps (cdr src-loaded))
         (dst-loaded (dorg-mutate--dolt-load dest head (cdr scopes)))
         (dst-orig (car dst-loaded))
         (dst-ps (cdr dst-loaded))
         (moved-root (dorg-mutate--ps-heading
                      src-ps (dorg-mutate--patch-address patch) patch)))
    ;; The source level-0 root heading is structural, not movable.
    (when (= (dorg-heading-level moved-root) 0)
      (dorg-mutate--raise 'invariant
                          "cannot move the source root heading across roots"
                          :operation 'move :target-kind :subtree
                          :heading (dorg-heading-id moved-root)))
    (let* ((moved (cons moved-root
                        (dorg-mutate--ps-descendants
                         src-ps (dorg-heading-id moved-root))))
           (moved-ids (mapcar #'dorg-heading-id moved))
           (moved-set (let ((h (make-hash-table :test 'eql)))
                        (dolist (id moved-ids) (puthash id t h)) h))
           (dst-parent (dorg-mutate--ps-heading
                        dst-ps (dorg-mutate--patch-host (plist-get to :under))
                        patch))
           (dst-parent-id (dorg-heading-id dst-parent))
           (before (dorg-mutate--ps-anchor-id
                    dst-ps
                    (and (plist-member to :before)
                         (dorg-mutate--patch-anchor-address
                          (plist-get to :before))) patch))
           (after (dorg-mutate--ps-anchor-id
                   dst-ps
                   (and (plist-member to :after)
                        (dorg-mutate--patch-anchor-address
                         (plist-get to :after))) patch)))
      ;; Fail closed on impossible/corrupt overlap before any write.  In a
      ;; valid single-repo DB heading ids are globally unique primary keys,
      ;; so these cannot trip; they guard against a corrupt snapshot.
      (when (gethash dst-parent-id moved-set)
        (dorg-mutate--raise 'invariant
                            "destination parent is inside the moved subtree"
                            :operation 'move :target-kind :subtree
                            :heading dst-parent-id))
      (dolist (id moved-ids)
        (when (gethash id dst-orig)
          (dorg-mutate--raise
           'invariant
           (format "moved heading id %S already exists in the destination root" id)
           :operation 'move :target-kind :subtree
           :heading id :detail 'overlap)))
      (let* ((siblings (dorg-mutate--ps-children dst-ps dst-parent-id))
             ;; Anchors are validated as destination siblings here; an
             ;; anchor in another root or inside the moved set is not a
             ;; sibling and signals `invalid-target' before any write.
             ;; Cross-root placement is bounded like the same-root seam, but the
             ;; cross-root SQL/db-ops are built by mapping over the MOVED rows
             ;; only (not a net-membership diff), so a destination
             ;; complete-parent rebalance would need to emit destination-sibling
             ;; UPDATEs it currently does not.  Rather than orphan or silently
             ;; truncate, fail closed BEFORE any write with a structured
             ;; invariant — the whole move is simulated outside the gate, so no
             ;; core row is committed.  Full cross-root destination rebalance is
             ;; the follow-up (see tracked-regression Resolution).
             (key (condition-case nil
                      (dorg-mutate--position-key siblings before after patch)
                    (dorg-sortkey-exhausted
                     (dorg-mutate--raise
                      'invariant
                      (concat "destination sibling order is exhausted; a "
                              "cross-root move into this aged group needs a "
                              "rebalance not yet supported on the cross-root path")
                      :operation 'move :target-kind :subtree
                      :heading dst-parent-id :detail 'sort-key-exhausted))))
             (delta (- (1+ (dorg-heading-level dst-parent))
                       (dorg-heading-level moved-root))))
        ;; Place the moved subtree in memory: the moved root takes the
        ;; destination parent/sort/level; descendants keep their
        ;; parent/sort but shift level by the same delta.  Every moved row
        ;; is re-homed into the destination root.
        (setf (dorg-heading-parent-id moved-root) dst-parent-id)
        (setf (dorg-heading-sort-key moved-root) key)
        (cl-incf (dorg-heading-level moved-root) delta)
        (dolist (d (cdr moved))
          (cl-incf (dorg-heading-level d) delta))
        (let ((sim (list :head-commit head
                         :from-scope-ids (and (car scopes)
                                              (sort (delete-dups
                                                     (copy-sequence (car scopes)))
                                                    #'<))
                         :to-scope-ids (and (cdr scopes)
                                            (sort (delete-dups
                                                   (copy-sequence (cdr scopes)))
                                                  #'<))
                         :sql (mapcar
                               (lambda (h)
                                 (dorg-mutate--dolt-cross-root-heading-update
                                  h dst-root))
                               moved)
                         :db-ops (mapcar (lambda (h) (list :update-heading h)) moved)
                         :moved moved-ids
                         :changed moved-ids
                         :from-root-id src-root
                         :to-root-id dst-root
                         ;; Two-root S0/candidate halves for the cross-root index
                         ;; context: ORIG holds the moved rows under their SOURCE
                         ;; lineage; the re-homed PS holds them under DESTINATION.
                         :src-orig src-orig
                         :src-ps src-ps
                         :dst-orig dst-orig
                         :dst-ps dst-ps)))
          ;; Expose the immutable cross-root projection context aggregating both
          ;; roots, after simulation and before persistence (tracked-case).
          (nconc sim
                 (list :index-context
                       (dorg-mutate--dolt-cross-root-index-context
                        source patch sim head))))))))


(defun dorg-mutate--dolt-cross-root-execute (source patch options)
  "Execute a bounded cross-root move PATCH on Dolt SOURCE.
Loads + simulates the move in memory OUTSIDE the per-store mutation gate (so any
invalid target / invariant / overlap signals before any write, and the
two-root load stays off the critical path), runs the registered index projector
(`dorg-ext-collect-index-projection') against the cross-root candidate-state
context, then commits the moved heading row updates FOLLOWED BY the validated
derived SQL in ONE Dolt transaction on the gate's LEASED session via
`dorg-mutate--dolt-gated-apply' (core + participant effects are atomic and roll
back together).  Like the same-root executor, the commit is linearized: it
lands only when the in-gate HEAD equals the plan base, else yields terminal
`:reason overlap' or a bounded re-plan.  A participant `:validate' signal aborts
before any commit.  A commit-time database failure is classified only after a
confirmed rollback and returns a sanitized `stale' or `invariant' detail.
Returns the apply result plist,
including `:extensions' and `:invalidation' naming BOTH the source and
destination roots so the read plane drops the warm cache for each.

Because the bounded slice PRESERVES heading ids, the moved rows re-home
under the destination root_id and a heading-keyed `:project' re-projects
them in place from the single destination-root context (see
`dorg-mutate--dolt-cross-root-mutation-context' for the documented
root-aggregate limitation)."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (src-root (dorg-mutate-source-root-id source))
         (dst-root (dorg-mutate-source-root-id (plist-get patch :to-source)))
         (base (plist-get options :base-commit)))
    (dorg-mutate--dolt-gated-apply
     source options base
     (lambda ()
       (let* ((sim (dorg-mutate--dolt-cross-root-simulate
                    source patch options))
              (sql (plist-get sim :sql))
              ;; Operation-qualified consistency bar over the cross-root AGGREGATE
              ;; S0/candidate — the SAME helper the dry-run plan-data uses, so a
              ;; corrupt candidate is refused identically in plan and apply.
              (_ (dorg-mutate--cross-root-assert-consistency sim patch))
              ;; The index projector runs against the cross-root candidate-state
              ;; context (both roots aggregated, moved ids re-homed); `:validate'
              ;; may signal here, before any commit, aborting fail-closed.
              (proj (dorg-ext-collect-index-projection
                     (plist-get sim :index-context)))
              (full-sql (append sql (plist-get proj :sql)))
              ;; Postimage receipt + transaction proof (tracked-regression): the
              ;; cross-root context aggregates both roots, so the receipt
              ;; carries one entry per affected owner on EACH side.
              (postimage (dorg-mutate--postimage-plan
                          (plist-get sim :index-context)
                          (plist-get proj :effects)
                          (plist-get options :return-candidate-models))))
         ;; A bounded cross-root move always yields non-empty core SQL (the
         ;; moved heading row updates), so there is no empty-net-effect branch.
         (list :sql full-sql
               :caller-base base
               :plan-base (plist-get sim :head-commit)
               :root-scope (sort (list src-root dst-root) #'<)
               :identity-scope
               (dorg-mutate--attempt-identity-scope
                (list (cons src-root (plist-get sim :from-scope-ids))
                      (cons dst-root (plist-get sim :to-scope-ids))))
               :write-set (dorg-mutate--heading-write-set sim)
               :postimages (plist-get postimage :entries)
               :validate (plist-get postimage :validate)
               :make-result
               (lambda (new-head)
                 (append
                  (list :ok t :dry-run nil :source-kind 'dolt
                        :root-id src-root
                        :from-root-id src-root
                        :to-root-id dst-root
                        :base-commit base
                        :commit new-head
                        :patches (list patch)
                        :moved (plist-get sim :moved)
                        :created nil
                        :changed (plist-get sim :changed)
                        :deleted nil)
                  ;; Same entitlement law as the same-root builder: entries
                  ;; attach only when a NEW commit landed (tracked-regression).
                  (when (and (plist-get postimage :entries)
                             (not (equal new-head
                                         (plist-get sim :head-commit))))
                    (append
                     (list :postimages (plist-get postimage :entries))
                     (when-let ((models-fn (plist-get postimage :models-fn)))
                       (list :candidate-models (funcall models-fn)))))
                  (list :extensions (plist-get proj :extensions)
                        :invalidation (list :roots (list src-root dst-root)
                                            :ext-dirty (plist-get proj :ext-dirty)
                                            :head new-head
                                            :dolt-dir dolt-dir
                                            :file (dorg-mutate-source-file source))))))))
     "dorg-mutate: cross-root move")))

(defun dorg-mutate--dolt-cross-root-plan-data (source patch options)
  "Return cross-root typed plan data, or nil on repo error.
Runs `dorg-mutate--dolt-cross-root-simulate' (load + validate + compute,
no write) so a dry-run reports `:head-commit', `:db-ops', moved ids, and
the source/destination root ids without advancing HEAD.  A validation /
invariant `dorg-mutate-error' PROPAGATES (a caller bug surfaces in plan
and apply alike); a repo-access `error' is tolerated as nil so the plan
stays classification-only.

The index projector runs OUTSIDE the repo-access-tolerant catch, so a
`:validate'/`:project'/manifest failure PROPAGATES in dry-run exactly as in apply
\(only a genuine repo-access/simulate `error' stays classification-only nil).
When an index projector ran (`:extensions' present) the dry-run also constructs an
invalidation PREVIEW (`:roots' + `:ext-dirty', NO advancing head token) so the
command layer can compare the STABLE invalidation fields against apply."
  (let ((sim (condition-case e
                 (dorg-mutate--dolt-cross-root-simulate source patch options)
               (dorg-mutate-error (signal (car e) (cdr e)))
               (error nil))))
    (when sim
      ;; Same operation-qualified consistency bar as apply (shared helper), so a
      ;; corrupt cross-root candidate is refused in dry-run exactly as in apply.
      (dorg-mutate--cross-root-assert-consistency sim patch)
      ;; Mirror apply: collect the participant projection (no commit) so the
      ;; dry-run plan reports the same extension effects and dirty scope the
      ;; apply will persist.  This runs OUTSIDE the tolerant catch above so a
      ;; participant failure surfaces (D1a).  Projection SQL is surfaced as
      ;; engine-internal `:ext-sql' db-ops; reshape/D2 strips it from PUBLIC.
      (let* ((proj (dorg-ext-collect-index-projection
                    (plist-get sim :index-context))))
        (append
         (list :head-commit (plist-get sim :head-commit)
               :db-ops (append (plist-get sim :db-ops)
                               (mapcar (lambda (s) (list :ext-sql s))
                                       (plist-get proj :sql)))
               :extensions (plist-get proj :extensions)
               :ext-dirty (plist-get proj :ext-dirty)
               :moved (plist-get sim :moved)
               :from-root-id (plist-get sim :from-root-id)
               :to-root-id (plist-get sim :to-root-id)
               :created nil
               :changed (plist-get sim :changed)
               :deleted nil)
         (when (plist-get proj :extensions)
           (list :invalidation
                 (list :roots (list (plist-get sim :from-root-id)
                                    (plist-get sim :to-root-id))
                       :ext-dirty (plist-get proj :ext-dirty)))))))))

(defun dorg-mutate--ensure-dolt-backend ()
  "Load the Dolt executor backend (`dorg-dolt' + hydration), idempotent.
Called at every Dolt plan/apply DISPATCH after kind=dolt is determined and BEFORE
any Dolt helper — so an ALREADY-RESOLVED Dolt `dorg-mutate-source' (D4 idempotent
resolution / B5 synthesized bindings) that never passes through descriptor or
registration construction still loads the stack, while plain-org never does
\(B8/D6 plain-file bundle isolation).  `require' is a no-op once loaded."
  (require 'dorg-dolt)
  (require 'dorg-model-load))            ; hydration loaders + as-of-suffix chain (tracked-case)

(defun dorg-mutate--dolt-plan-data (source patches options unsupported)
  "Return semantic Dolt plan data for SOURCE, or nil when unavailable.
Runs a simulate (load + apply + diff + lower) so a typed plan on a
Dolt source returns `:head-commit'/`:db-ops'/`:created'/`:changed'/
`:deleted' WITHOUT committing (AC1).  Skipped when any patch is UNSUPPORTED
\(a cross-root move must classify before snapshot load) or when the Dolt
dir is not a readable repository directory (so the no-repo unit/plan
tests stay classification-only and never spawn a failing CLI call).

A delete-freed sibling sort-key reuse is no longer a capability gap — the
staged-slot scheduler (`dorg-mutate--dolt-assemble-sql') simulates and
plans it as a supported batch.  A validation / invariant
`dorg-mutate-error' PROPAGATES (those are caller bugs AC1 requires to
surface in plan and apply alike), while a repo-access `error' is
tolerated, leaving the plan classification-only (the eventual apply
re-simulates and surfaces any real backend failure)."
  (when (and (eq (dorg-mutate-source-kind source) 'dolt)
             (null unsupported)
             (let ((dir (dorg-mutate-source-dolt-dir source)))
               (and dir (file-directory-p dir))))
    (dorg-mutate--ensure-dolt-backend)          ; kind=dolt dispatch: load backend
    (let ((cross (dorg-mutate--dolt-cross-root-batch-patch source patches)))
      (if cross
          ;; A bounded cross-root archive move plans via the cross-root
          ;; simulator (two-root load), not the one-root same-root path.
          (dorg-mutate--dolt-cross-root-plan-data source cross options)
        (dorg-mutate--dolt-plan-data--same-root
         source patches options)))))

(defun dorg-mutate--dolt-plan-data--same-root
    (source patches options)
  "Same-root Dolt plan data (the one-root simulate path).
Extracted so `dorg-mutate--dolt-plan-data' can dispatch the bounded
cross-root case to its own two-root simulator without disturbing this
behavior.  See `dorg-mutate--dolt-plan-data' for the propagate/tolerate
error policy this preserves.

D1 (BOUND parity): unlike the pre-change same-root dry-run, this now RUNS
the index-projector `:validate'/`:project' collection against the SIMULATED
new-core state — exactly as apply does, and mirroring the cross-root plan-data —
surfacing engine-internal `:extensions'/`:ext-dirty' and the derived SQL as
`:ext-sql' db-ops, WITHOUT executing or committing
any SQL.  D1a: the collection runs OUTSIDE the repo-access-tolerant catch so
a participant failure PROPAGATES in dry-run (only a genuine simulate/repo
`error' stays classification-only nil).  D1b (zero-net-core preserved): when
the simulated core `:sql' is nil, NO participant runs and NO commit is
reported — extension input alone never forces a commit or plan/apply
divergence.  When EXT-INPUTS is non-empty a dry-run invalidation PREVIEW
\(`:roots' + `:ext-dirty', NO advancing head token) is built so the command
layer can compare the STABLE invalidation fields against apply; omitted/empty
inputs keep the pre-change plan shape."
  (let ((sim (condition-case e
                 (dorg-mutate--dolt-simulate source patches options)
               (dorg-mutate-error (signal (car e) (cdr e)))
               (error nil))))
    (when sim
      ;; Participant collection stays OUTSIDE the tolerant simulate catch: a
      ;; participant refusal propagates in plan and apply.  The resulting
      ;; immutable attempt is both the dry-run evidence source and, for a
      ;; non-empty apply, the one attempt consumed under equal HEAD.
      (dorg-mutate--dolt-attempt-plan-data
       (dorg-mutate--dolt-attempt-from-sim
        source patches options sim)))))

;;; ─── Typed patch accessors and validation ────────────────────────────

(defun dorg-mutate--patch-tget (tagged key)
  "Return KEY's value from a TAGGED target/host list (TAG :k v …) — i.e. the
plist after the leading type tag."
  (plist-get (cdr tagged) key))

(defun dorg-mutate--patch-host (host)
  "Return the engine address carried by typed HOST.
An id heading/subtree yields its integer id; an external address yields
\(:extern :key K :value V) for late resolution; `:root' passes through."
  (cond
   ((and (consp host) (memq (car host) '(:heading :subtree))
         (eq (cadr host) :extern))
    (list :extern :key (dorg-mutate--patch-tget host :key)
          :value (dorg-mutate--patch-tget host :value)))
   ((and (consp host) (memq (car host) '(:heading :subtree)))
    (dorg-mutate--patch-tget host :id))
   (t host)))

(defun dorg-mutate--patch-target-host (target)
  "Return the engine address carried by typed TARGET.
Heading/subtree targets carry either `:id' or inline `:extern' identity."
  (dorg-mutate--patch-host target))

;;; ─── Native typed-patch validation ─────────────────────────────────

(defun dorg-mutate--patch-signature (patch)
  "Return PATCH's (VERB TARGET-KIND) discriminator."
  (let ((target (plist-get patch :target)))
    (list (plist-get patch :op) (and (consp target) (car target)))))

(defun dorg-mutate--patch-check-keys (plist allowed required context)
  "Validate PLIST as a closed paired plist for CONTEXT.
ALLOWED and REQUIRED are keyword lists.  Unknown, duplicated, dangling, and
missing fields signal `invalid-op'."
  (unless (listp plist)
    (dorg-mutate--raise 'invalid-op "typed patch must be a proper list"
                        :operation context))
  (let ((tail plist) seen)
    (while tail
      (let ((key (car tail)))
        (unless (and (keywordp key) (cdr tail))
          (dorg-mutate--raise 'invalid-op "malformed typed patch plist"
                              :operation context :field key))
        (unless (memq key allowed)
          (dorg-mutate--raise 'invalid-op "unknown typed patch field"
                              :operation context :field key))
        (when (memq key seen)
          (dorg-mutate--raise 'invalid-op "duplicate typed patch field"
                              :operation context :field key))
        (push key seen)
        (setq tail (cddr tail))))
    (dolist (key required)
      (unless (memq key seen)
        (dorg-mutate--raise 'invalid-op "missing typed patch field"
                            :operation context :field key)))))

(defun dorg-mutate--patch-validate-host (host context &optional non-root)
  "Validate a typed HOST for CONTEXT and return it.
When NON-ROOT is non-nil, the level-0 `:root' address is rejected."
  (cond
   ((eq host :root)
    (when non-root
      (dorg-mutate--raise 'invalid-target "operation requires a heading host"
                          :operation context :target host)))
   ((and (consp host) (memq (car host) '(:heading :subtree)))
    (dorg-mutate--patch-check-keys
     (cdr host) '(:id :extern :key :value)
     (if (eq (cadr host) :extern) '(:extern :key :value) '(:id)) context)
    (if (eq (cadr host) :extern)
        (unless (and (eq t (dorg-mutate--patch-tget host :extern))
                     (stringp (dorg-mutate--patch-tget host :key))
                     (stringp (dorg-mutate--patch-tget host :value)))
          (dorg-mutate--raise 'invalid-target "invalid external-key host"
                              :operation context :target host))
      (unless (integerp (dorg-mutate--patch-tget host :id))
        (dorg-mutate--raise 'invalid-target "heading id must be an integer"
                            :operation context :target host))))
   (t (dorg-mutate--raise 'invalid-target "invalid typed heading host"
                          :operation context :target host)))
  host)

(defun dorg-mutate--patch-validate-target (target context)
  "Validate typed TARGET for CONTEXT and return it."
  (unless (consp target)
    (dorg-mutate--raise 'invalid-target "typed patch requires a tagged target"
                        :operation context :target target))
  (pcase (car target)
    ((or :heading :subtree)
     (dorg-mutate--patch-validate-host target context
                                      (eq (car target) :subtree)))
    (:property
     (dorg-mutate--patch-check-keys (cdr target) '(:host :name)
                                    '(:host :name) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context)
     (unless (stringp (dorg-mutate--patch-tget target :name))
       (dorg-mutate--raise 'invalid-target "property name must be a string"
                           :operation context :target target))
     (when (equal (dorg-mutate--patch-tget target :name) dorg-id-property)
       (dorg-mutate--raise 'invalid-target
                           "DORG_ID is identity and not a mutable property"
                           :operation context :target target)))
    ((or :keyword :keywords)
     (dorg-mutate--patch-check-keys
      (cdr target) '(:host :name :occurrence)
      (append '(:host :name) (when (eq (car target) :keyword) '(:occurrence)))
      context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context)
     (unless (stringp (dorg-mutate--patch-tget target :name))
       (dorg-mutate--raise 'invalid-target "keyword name must be a string"
                           :operation context :target target))
     (when (dorg-protected-keyword-p (dorg-mutate--patch-tget target :name))
       (dorg-mutate--raise 'invalid-target "protected keyword target"
                           :operation context :target target))
     (when (eq (car target) :keyword)
       (unless (and (integerp (dorg-mutate--patch-tget target :occurrence))
                    (>= (dorg-mutate--patch-tget target :occurrence) 1))
         (dorg-mutate--raise 'invalid-target "keyword occurrence must be positive"
                             :operation context :target target))))
    (:planning
     (dorg-mutate--patch-check-keys (cdr target) '(:host) '(:host) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context))
    (:tree
     (dorg-mutate--patch-check-keys (cdr target) '(:under :before :after)
                                    '(:under) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :under) context)
     (dolist (field '(:before :after))
       (when (plist-member (cdr target) field)
         (dorg-mutate--patch-validate-host
          (dorg-mutate--patch-tget target field) context t)))
     (when (and (plist-member (cdr target) :before)
                (plist-member (cdr target) :after))
       (dorg-mutate--raise 'invalid-op "tree target has contradictory anchors"
                           :operation context :target target)))
    ((or :texts :items)
     (dorg-mutate--patch-check-keys (cdr target) '(:host) '(:host) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context))
    (:text
     (dorg-mutate--patch-check-keys (cdr target) '(:host :occurrence)
                                    '(:host :occurrence) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context)
     (unless (eql 1 (dorg-mutate--patch-tget target :occurrence))
       (dorg-mutate--raise
        'invalid-target "text target currently supports only occurrence 1"
        :operation context :target target)))
    (:drawer-lines
     (dorg-mutate--patch-check-keys (cdr target) '(:host :name :occurrence)
                                    '(:host :name :occurrence) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context t)
     (unless (and (stringp (dorg-mutate--patch-tget target :name))
                  (eql 1 (dorg-mutate--patch-tget target :occurrence)))
       (dorg-mutate--raise 'invalid-target "invalid drawer-line target"
                           :operation context :target target)))
    (_ (dorg-mutate--raise 'invalid-op "unknown typed target kind"
                           :operation context :target target)))
  target)

(defun dorg-mutate--patch-validate-item-value (item context)
  "Validate one canonical typed ITEM value for CONTEXT.
Every kind admits the optional `:post-blank' — the engine-boundary
re-validation of the public grammar's trailing tail (tracked-regression).
Strictness matches the public boundary (K#027): a PRESENT `:post-blank'
must be a non-negative integer — an explicitly nil value is a
noncanonical typed value and is refused; only ABSENCE is the valid
default."
  (unless (listp item)
    (dorg-mutate--raise 'invalid-target "item value must be a plist"
                        :operation context :value item))
  (when (plist-member item :post-blank)
    (let ((pb (plist-get item :post-blank)))
      (unless (and (integerp pb) (>= pb 0))
        (dorg-mutate--raise 'invalid-target
                            "item :post-blank must be a non-negative integer"
                            :operation context :value item))))
  (let ((kind (plist-get item :kind)))
    (pcase kind
      ('text
       (dorg-mutate--patch-check-keys item '(:kind :text :post-blank)
                                      '(:kind :text) context)
       (unless (stringp (plist-get item :text))
         (dorg-mutate--raise 'invalid-target "text item requires string :text"
                             :operation context :value item)))
      ('keyword
       (dorg-mutate--patch-check-keys item '(:kind :keyword :value :post-blank)
                                      '(:kind :keyword :value) context)
       (unless (and (stringp (plist-get item :keyword))
                    (stringp (plist-get item :value)))
         (dorg-mutate--raise 'invalid-target "keyword item requires strings"
                             :operation context :value item))
       (when (dorg-protected-keyword-p (plist-get item :keyword))
         (dorg-mutate--raise 'invalid-target "protected keyword item"
                             :operation context :value item)))
      ('planning
       (dorg-mutate--patch-check-keys
        item '(:kind :scheduled :deadline :closed :post-blank) '(:kind) context)
       (dolist (field '(:scheduled :deadline :closed))
         (unless (or (null (plist-get item field))
                     (stringp (plist-get item field)))
           (dorg-mutate--raise 'invalid-target "invalid planning item field"
                               :operation context :field field))))
      ('drawer
       (dorg-mutate--patch-check-keys
        item '(:kind :name :properties :lines :post-blank) '(:kind :name) context)
       (unless (stringp (plist-get item :name))
         (dorg-mutate--raise 'invalid-target "drawer name must be a string"
                             :operation context :value item))
       (when (and (plist-get item :properties) (plist-get item :lines))
         (dorg-mutate--raise 'invalid-target
                             "drawer cannot mix properties and raw lines"
                             :operation context :value item))
       (dolist (cell (plist-get item :properties))
         (unless (and (consp cell) (stringp (car cell)) (stringp (cdr cell)))
           (dorg-mutate--raise 'invalid-target "invalid drawer property"
                               :operation context :value cell)))
       (dolist (line (plist-get item :lines))
         (unless (stringp line)
           (dorg-mutate--raise 'invalid-target "invalid drawer line"
                               :operation context :value line))))
      (_ (dorg-mutate--raise 'invalid-target "unknown typed item kind"
                             :operation context :value item))))
  item)

(defun dorg-mutate--patch-validate-heading-payload (payload context allowed)
  "Validate a typed heading PAYLOAD for CONTEXT against ALLOWED fields."
  (dorg-mutate--patch-check-keys
   payload allowed nil context)
  (when (plist-member payload :id)
    (unless (integerp (plist-get payload :id))
      (dorg-mutate--raise 'invalid-target "heading :id must be an integer"
                          :operation context)))
  (dolist (field '(:title :todo :priority))
    (when (plist-member payload field)
      (unless (or (null (plist-get payload field))
                  (stringp (plist-get payload field)))
        (dorg-mutate--raise 'invalid-target "invalid heading string field"
                            :operation context :field field))))
  (when (plist-member payload :tags)
    (unless (and (listp (plist-get payload :tags))
                 (cl-every #'stringp (plist-get payload :tags)))
      (dorg-mutate--raise 'invalid-target "heading tags must be strings"
                          :operation context)))
  (when (plist-member payload :commented)
    (unless (memq (plist-get payload :commented) '(nil t))
      (dorg-mutate--raise 'invalid-target "heading :commented must be boolean"
                          :operation context)))
  (when (plist-member payload :post-blank)
    (unless (and (integerp (plist-get payload :post-blank))
                 (>= (plist-get payload :post-blank) 0))
      (dorg-mutate--raise 'invalid-target
                          "heading :post-blank must be non-negative"
                          :operation context)))
  (when (plist-member payload :items)
    (unless (listp (plist-get payload :items))
      (dorg-mutate--raise 'invalid-target "heading :items must be a list"
                          :operation context))
    (dolist (item (plist-get payload :items))
      (dorg-mutate--patch-validate-item-value item context)))
  (when (plist-member payload :children)
    (unless (listp (plist-get payload :children))
      (dorg-mutate--raise 'invalid-target "heading :children must be a list"
                          :operation context))
    (dolist (child (plist-get payload :children))
      (dorg-mutate--patch-validate-heading-payload
       child context
       '(:id :title :todo :priority :tags :commented :post-blank
             :items :children))))
  payload)

(defconst dorg-mutate--patch-fields
  '(((put :property)      (:op :target :value) (:op :target :value))
    ((delete :property)   (:op :target) (:op :target))
    ((replace :keyword)   (:op :target :value :missing) (:op :target :value))
    ((replace :keywords)  (:op :target :values) (:op :target :values))
    ((insert :keywords)   (:op :target :value :position)
                           (:op :target :value :position))
    ((delete :keyword)    (:op :target :missing) (:op :target))
    ((delete :keywords)   (:op :target) (:op :target))
    ((update :heading)    (:op :target :fields) (:op :target :fields))
    ((update :planning)   (:op :target :scheduled :deadline :closed)
                           (:op :target))
    ((insert :tree)       (:op :target :heading) (:op :target :heading))
    ((move :subtree)      (:op :target :to :to-source :preserve-ids
                           :atomic :allow-nonatomic) (:op :target :to))
    ((delete :heading)    (:op :target) (:op :target))
    ((delete :subtree)    (:op :target) (:op :target))
    ((insert :drawer-lines) (:op :target :position :missing :value)
                             (:op :target :position :missing :value))
    ((insert :texts)      (:op :target :value :position)
                           (:op :target :value :position))
    ((replace :text)      (:op :target :value :missing)
                           (:op :target :value :missing))
    ((replace :items)     (:op :target :items) (:op :target :items)))
  "Closed top-level fields by typed (verb target-kind) pair.")

(defun dorg-mutate--raise-structural-payload (operation value)
  "Signal a typed structural payload refusal for OPERATION/VALUE."
  (dorg-mutate--raise
   'invalid-target "payload would change item structure"
   :operation operation :detail 'structural-payload-boundary :value value))

(defun dorg-mutate--single-line-payload (value operation)
  "Validate VALUE for a single-line item grammar."
  (when (string-match-p "[\0\r\n]" value)
    (dorg-mutate--raise-structural-payload operation value))
  value)

(defun dorg-mutate--structural-text-line-p (line)
  "Return non-nil when LINE starts an Org structural item."
  (or (string-match-p "\\`[ \t]*\\*+\\(?:[ \t]\\|\\'\\)" line)
      (string-match-p "\\`[ \t]*#\\+[[:alnum:]_@#%:-]+:" line)
      (string-match-p "\\`[ \t]*:[[:alnum:]_@#%]+:[ \t]*\\'" line)
      (string-match-p "\\`[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):" line)))

(defun dorg-mutate--validate-text-payload (value operation)
  "Validate VALUE as exactly one text item payload for OPERATION."
  (when (string-match-p "[\0\r]" value)
    (dorg-mutate--raise-structural-payload operation value))
  (dolist (line (split-string value "\n"))
    (when (dorg-mutate--structural-text-line-p line)
      (dorg-mutate--raise-structural-payload operation value)))
  (with-temp-buffer
    (insert "* DOrg payload\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n" value "\n")
    (let ((org-inhibit-startup t)) (org-mode))
    (let* ((result (dorg-parse-buffer (current-buffer)))
           (heading (cl-find 1 (dorg-parse-result-headings result)
                             :key #'dorg-heading-id :test #'eql))
           (items (and heading (dorg-heading-items heading))))
      (unless (and (= (length items) 1)
                   (cl-typep (car items) 'dorg-text-item)
                   (let ((text (dorg-text-item-text (car items))))
                     (or (equal text value)
                         (equal text (concat value "\n")))))
        (dorg-mutate--raise-structural-payload operation value))))
  value)

(defun dorg-mutate--validate-patch (patch index)
  "Validate typed PATCH at INDEX and return it unchanged."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (row (assoc signature dorg-mutate--patch-fields))
         (op (car signature))
         (target (plist-get patch :target)))
    (unless row
      (dorg-mutate--raise 'invalid-op "unsupported typed verb/target pair"
                          :operation op :target-kind (cadr signature)
                          :op-index index))
    (dorg-mutate--patch-check-keys patch (nth 1 row) (nth 2 row) op)
    (dorg-mutate--patch-validate-target target op)
    (pcase signature
      ((or `(put :property) `(replace :keyword))
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value))))
      (`(replace :keywords)
       (unless (and (listp (plist-get patch :values))
                    (cl-every #'stringp (plist-get patch :values)))
         (dorg-mutate--raise 'invalid-target "keyword values must be strings"
                             :operation op)))
      ((or `(replace :keyword) `(delete :keyword))
       (when (and (plist-member patch :missing)
                  (not (eq (plist-get patch :missing) :error)))
         (dorg-mutate--raise 'invalid-op
                             "single keyword mutation requires :missing :error"
                             :operation op :detail 'invalid-missing-policy)))
      (`(insert :keywords)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (dorg-mutate--single-line-payload (plist-get patch :value) op)
       (unless (memq (plist-get patch :position) '(:before-first :after-last))
         (dorg-mutate--raise
          'invalid-op "keyword insert requires :before-first or :after-last"
          :operation op :detail 'invalid-position)))
      (`(insert :texts)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (dorg-mutate--validate-text-payload (plist-get patch :value) op)
       (unless (memq (plist-get patch :position) '(:before-first :after-last))
         (dorg-mutate--raise
          'invalid-op "text insert requires :before-first or :after-last"
          :operation op :detail 'invalid-position)))
      (`(replace :text)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (unless (eq (plist-get patch :missing) :error)
         (dorg-mutate--raise
          'invalid-op "single text mutation requires :missing :error"
          :operation op :detail 'invalid-missing-policy)))
      (`(insert :drawer-lines)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (dorg-mutate--single-line-payload (plist-get patch :value) op)
       (unless (and (memq (plist-get patch :position)
                          '(:before-first :after-last))
                    (memq (plist-get patch :missing) '(:create :error)))
         (dorg-mutate--raise 'invalid-target "invalid drawer-line placement"
                             :operation op)))
      (`(update :planning)
       (dolist (field '(:scheduled :deadline :closed))
         (when (and (plist-member patch field)
                    (not (or (null (plist-get patch field))
                             (stringp (plist-get patch field)))))
           (dorg-mutate--raise 'invalid-target "invalid planning field"
                               :operation op :field field))))
      (`(replace :items)
       (unless (listp (plist-get patch :items))
         (dorg-mutate--raise 'invalid-target "items replacement must be a list"
                             :operation op))
       (dolist (item (plist-get patch :items))
         (dorg-mutate--patch-validate-item-value item op)))
      (`(insert :tree)
       (dorg-mutate--patch-validate-heading-payload
        (plist-get patch :heading) op
        '(:id :title :todo :priority :tags :commented :post-blank
              :items :children)))
      (`(update :heading)
       (dorg-mutate--patch-validate-heading-payload
        (plist-get patch :fields) op
        '(:title :todo :priority :tags :commented :post-blank)))
      (`(move :subtree)
       (let ((to (plist-get patch :to)))
         (dorg-mutate--patch-check-keys to '(:under :before :after)
                                        '(:under) op)
         (dorg-mutate--patch-validate-host (plist-get to :under) op)
         (dolist (field '(:before :after))
           (when (plist-member to field)
             (dorg-mutate--patch-validate-host (plist-get to field) op t)))
         (when (and (plist-member to :before) (plist-member to :after))
           (dorg-mutate--raise 'invalid-op "move has contradictory anchors"
                               :operation op))
         (when (and (plist-member patch :to-source)
                    (not (dorg-mutate-source-p (plist-get patch :to-source))))
           (dorg-mutate--raise 'invalid-target
                               "move :to-source must be a resolved source"
                               :operation op :target (plist-get patch :to-source)))
         (when (plist-member patch :to-source)
           (dorg-mutate--assert-source-kind
            (dorg-mutate-source-kind (plist-get patch :to-source))))
         (dolist (field '(:preserve-ids :atomic :allow-nonatomic))
           (when (and (plist-member patch field)
                      (not (memq (plist-get patch field) '(nil t))))
             (dorg-mutate--raise 'invalid-target
                                 "move policy fields must be boolean"
                                 :operation op :field field))))))
    patch))

(defun dorg-mutate--validate-patches (patches)
  "Validate PATCHES as one typed batch and return it unchanged."
  (unless (listp patches)
    (dorg-mutate--raise 'invalid-op "typed patch batch must be a list"
                        :patches patches))
  (cl-loop for patch in patches for index from 0
           collect (dorg-mutate--validate-patch patch index)))

(defun dorg-mutate--make-plan (source patches options)
  "Build a typed PATCHES plan for resolved SOURCE with OPTIONS."
  (let* ((kind (dorg-mutate-source-kind source)))
    ;; Explicit unsupported, never a silent drop (tracked-regression): a backend that
    ;; cannot run derived-index projection (only `dolt' does) must fail closed
    ;; when an eligible index projector is registered — in BOTH plan and apply,
    ;; before any persistence.  Core-only work on such a backend is unchanged.
    (when (and (not (eq kind 'dolt))
               (dorg-ext-index-projector-eligible-p kind))
      (dorg-mutate--raise
       'unsupported
       (format "derived-index projection is not supported on a %s source" kind)
       :detail 'derived-index-unsupported-source :source-kind kind)))
  (let* ((kind (dorg-mutate-source-kind source))
         (classified (mapcar (lambda (patch)
                               (dorg-mutate--classify-patch source patch))
                             patches))
         ;; A cross-root move may classify as supported per patch
         ;; yet violate the bounded fh4s batch shape (sole op, one move).
         ;; That batch-level gap is appended to `:unsupported' here.
         (batch-reject
          (dorg-mutate--dolt-cross-root-batch-reject source patches))
         (unsupported (append
                       (cl-remove-if (lambda (c) (plist-get c :supported)) classified)
                       (and batch-reject (list batch-reject))))
         (plan (list :ok t
                     :dry-run t
                     :source-kind kind
                     :root-id (dorg-mutate-source-root-id source)
                     :base-commit (plist-get options :base-commit)
                     :base-revision (or (plist-get options :base-revision)
                                        (dorg-mutate--plain-org-revision source))
                     :patches patches
                     :patch-support classified
                     :unsupported unsupported
                     :supported (null unsupported)))
         (dolt0 (float-time))
         (dolt-data (dorg-mutate--dolt-plan-data
                     source patches options unsupported)))
    (when-let ((attempt (plist-get dolt-data :dorg-execution-attempt)))
      (plist-put attempt :precomputed-plan-seconds
                 (- (float-time) dolt0)))
    ;; DOLT-DATA is prepended so its keys (semantic plan data, and any
    ;; batch-level `:supported'/`:unsupported' override discovered during
    ;; simulation) take precedence over the static classification.
    (if dolt-data (append dolt-data plan) plan)))

(defun dorg-mutate--gate-identity-none (src patches)
  "Fail closed when SRC has identity `none' and PATCHES is non-empty.
A `none' identity is READ-ONLY: a mutation SIGNALS the exact pair `unsupported' /
`:detail identity-none' here — before any plan/render/write — rather than being
reported as a `:supported nil' plan.  An EMPTY batch is a no-op and stays valid
\(not reclassified as a mutation).  Both typed planning and apply call this
after resolution and validation, before `dorg-mutate--make-plan'."
  (when (and patches (dorg-mutate--identity-none-p src))
    (dorg-mutate--raise 'unsupported
                        "mutation is unavailable under identity=none"
                        :detail 'identity-none
                        :source-kind (dorg-mutate-source-kind src))))

(defun dorg-mutate--execute-patches (src patches options plan)
  "Execute resolved SRC's typed PATCHES given precomputed PLAN.
The shared apply core: honors `:dry-run' (returns PLAN), surfaces the first
unsupported classification as a deterministic `unsupported' error, else
dispatches to the plain-org / Dolt (incl. bounded cross-root) executor.  PLAN
is `dorg-mutate--make-plan' output."
  (cond
   ((plist-get options :dry-run) plan)
   ((plist-get plan :unsupported)
    ;; At least one patch cannot be executed by the resolved source.
    ;; Surface the first concrete reason deterministically.
    (let* ((first (car (plist-get plan :unsupported)))
           (reason (plist-get first :reason))
           (detail (plist-get first :detail)))
      (apply #'dorg-mutate--raise
             'unsupported
             (format "typed mutation refused: %s" detail)
             :source-kind (dorg-mutate-source-kind src)
             :operation (plist-get first :op)
             ;; The raised condition `:detail' is the classification reason;
             ;; a plain-org C3 refusal also carries its exact `:variant'.
             :detail reason
             :plan plan
             (when (plist-member first :variant)
               (list :variant (plist-get first :variant))))))
   (t
    (cl-ecase (dorg-mutate-source-kind src)
      (plain-org (dorg-mutate--plain-org-execute src patches options))
      (dolt
       (dorg-mutate--ensure-dolt-backend)     ; kind=dolt dispatch: load backend
       ;; The bounded same-Dolt-dir/different-root archive move dispatches
       ;; to the cross-root executor BEFORE the one-root executor; every
       ;; other (same-root) Dolt batch stays on the y5k6 path.
       (let ((cross (dorg-mutate--dolt-cross-root-batch-patch src patches)))
         (if cross
             (dorg-mutate--dolt-cross-root-execute src cross options)
           (dorg-mutate--dolt-execute
            src patches options
            (plist-get plan :dorg-execution-attempt)))))))))

;;; ─── Native typed-patch engine boundary ───────────────────────────────
;; These are the only semantic engine entries.  `dorg-api' passes the exact
;; typed patches it normalized; the engine validates and executes that shape
;; directly, with no compatibility lowering or second mutation IR.

(defun dorg-mutate--reject-extension-inputs (ext-inputs)
  "Fail closed when EXT-INPUTS is non-nil — the channel is retired (tracked-regression).
Index projectors run automatically from the DOrg-owned candidate-state context and
accept no caller payload; a supplied `:extension-inputs' is a superseded API, not a
silently-ignored hint."
  (when ext-inputs
    (dorg-mutate--raise
     'unsupported
     "the :extension-inputs channel is retired; index projectors run \
automatically from candidate state and accept no caller payload"
     :detail 'extension-inputs-retired)))

(defun dorg-mutate--plan-patches (source patches &optional ext-inputs &rest options)
  "Validate typed PATCHES for SOURCE and return a non-mutating plan.

EXT-INPUTS is the RETIRED per-execution `:extension-inputs' channel
\(tracked-regression): index projectors now run automatically from the candidate-state
context and accept no caller payload.  The positional parameter is kept for
signature stability but FAILS CLOSED — a non-nil value signals `unsupported'."
  (dorg-mutate--reject-extension-inputs ext-inputs)
  (let* ((src (dorg-mutate--resolve-source source))
         (patches (dorg-mutate--validate-patches patches)))
    (dorg-mutate--gate-identity-none src patches)
    (dorg-mutate--make-plan src patches options)))

(defun dorg-mutate--apply-patches (source patches &optional ext-inputs &rest options)
  "Validate and execute typed PATCHES against SOURCE, or signal.

EXT-INPUTS is the RETIRED `:extension-inputs' channel (tracked-regression): index
projectors run automatically from the candidate-state context and accept no
caller payload.  The positional parameter is kept for signature stability but
FAILS CLOSED — a non-nil value signals `unsupported'."
  (dorg-mutate--reject-extension-inputs ext-inputs)
  (let* ((src (dorg-mutate--resolve-source source))
         (patches (dorg-mutate--validate-patches patches))
         ;; Pin generated ids BEFORE the one reusable plan attempt is built.
         ;; Every safe replan then receives the same immutable intent/id pair;
         ;; without an op-id, a discarded uncommitted attempt may allocate a
         ;; fresh id exactly as before.
         (patches (if (and (eq (dorg-mutate-source-kind src) 'dolt)
                           (plist-get options :op-id))
                      (dorg-mutate--pin-insert-heading-ids patches)
                    patches))
         (_ (dorg-mutate--gate-identity-none src patches))
         (plan (dorg-mutate--make-plan src patches options)))
    (dorg-mutate--execute-patches src patches options plan)))

(provide 'dorg-mutate)
;;; dorg-mutate.el ends here
