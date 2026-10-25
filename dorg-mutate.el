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
(cl-defstruct (dorg-mutate--owner-capability
               (:constructor dorg-mutate--owner-capability--make)
               (:copier nil))
  "Private authority minted for one registered-family execution.
This is an in-process boundary, not a security boundary against trusted Elisp."
  family execution-id request)

(defun dorg-mutate--planning-owner-request (plan)
  "Return the sole request from explicit verified planning PLAN, or nil."
  (let (requests)
    (dolist (cap (plist-get plan :owner-capabilities))
      (unless (and (cl-typep cap 'dorg-mutate--owner-capability)
                   (memq (dorg-mutate--owner-capability-execution-id cap)
                         (plist-get plan :owner-execution-ids)))
        (dorg-mutate--raise 'invalid-target
                            "owner planning capability is not verified"
                            :detail 'capability-forged))
      (when-let ((request (dorg-mutate--owner-capability-request cap)))
        (push request requests)))
    (when (cdr requests)
      (dorg-mutate--raise 'invalid-target
                          "multiple owner planning requests are ambiguous"
                          :detail 'multiple-owner-requests))
    (car requests)))
(require 'dorg-error)
(require 'dorg-file-lock)
(require 'dorg-mutate-grammar)
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
    overlap contended idempotency-key-mismatch indeterminate)
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

(defconst dorg-mutate--settled-planning-error-reasons
  '(invalid-target invariant)
  "Planning failures eligible for digest-bound settled replay recovery.")

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
    (let ((key (cadr value)))
      ;; A protected key is DOrg-owned metadata, not an addressable user
      ;; identity.  Registered families remain explicit so this holds if their
      ;; storage namespace ever stops being a subset of the DORG_ rule.
      (when (or (dorg-protected-keyword-p key)
                (dorg-owned-metadata-key-p key))
        (dorg-mutate--raise 'invalid-target
                            "protected metadata cannot be an identity policy"
                            :detail 'identity-policy-protected :key key))
      (list :property key)))
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
    (:item t)
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
         (occurrence (when (memq (car target) '(:keyword :text :item))
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

(defconst dorg-mutate--identity-context-key
  '(:dorg-mutate-context :source-identity-property)
  "Private key for a source identity property in request-local context.")

(defun dorg-mutate--item-batch-context (&optional identity-key)
  "Return an explicit request-local mutation cursor context.

IDENTITY-KEY is the source's protected property identity, when configured."
  (let ((context (make-hash-table :test 'equal)))
    (when identity-key
      (puthash dorg-mutate--identity-context-key identity-key context))
    context))

(defun dorg-mutate--item-insert-cursor-key (target position)
  "Return the request-local cursor key for TARGET/POSITION."
  (list (car target)
        (dorg-mutate--patch-tget target :host)
        (dorg-mutate--patch-tget target :name)
        (dorg-mutate--patch-tget target :occurrence)
        position))

(defun dorg-mutate--item-list-cursor-key-p (key)
  "Return non-nil when KEY addresses a top-level heading item cursor."
  (memq (car key) '(:texts :keywords :item)))

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

(defun dorg-mutate--item-batch-context-shift-drawer-lines
    (context host name index &optional except-key)
  "Shift held drawer-line cursors after an insertion at INDEX.
The context key includes target host, drawer name, selector and position.
Different positions in the same drawer therefore need the same slot-following
update as top-level item cursors; otherwise an insertion ahead of a held slot
would make the next repeated insertion reverse declaration order."
  (when context
    (maphash
     (lambda (key cursor)
       (when (and (eq (car key) :drawer-lines)
                  (equal (nth 1 key) host)
                  (equal (nth 2 key) name)
                  (not (equal key except-key))
                  (> cursor index))
         (puthash key (1+ cursor) context)))
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
                 (`(:before ,n)
                  (if (and (integerp n) (<= 1 n) (<= n (length matches)))
                      (nth (1- n) matches)
                    (dorg-mutate--raise
                     'invalid-target "insertion anchor is missing"
                     :operation 'insert :detail 'missing :position position)))
                 (`(:after ,n)
                  (if (and (integerp n) (<= 1 n) (<= n (length matches)))
                      (1+ (nth (1- n) matches))
                    (dorg-mutate--raise
                     'invalid-target "insertion anchor is missing"
                     :operation 'insert :detail 'missing :position position)))
                 (_ (dorg-mutate--raise
                     'invalid-op "unsupported item insertion position"
                     :operation 'insert :detail 'invalid-position
                     :position position)))))
    (if (and context (not (eq position :after-last)))
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
                          (not (eq position :after-last))
                          (dorg-mutate--item-insert-cursor-key
                           target position)))
         (new (dorg-mutate--item-from-value
               (pcase (car target)
                 (:texts (list :kind 'text :text value))
                 (:keywords (list :kind 'keyword
                                  :keyword (dorg-mutate--patch-tget target :name)
                                  :value value))
                 (:item (plist-get patch :item))))))
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
                                 :value value))
                 (:item (plist-get patch :item)))
               (dorg-item-post-blank existing))))
    (when (and (eq (car target) :item)
               (cl-typep existing 'dorg-keyword-item)
               (dorg-protected-keyword-p
                (dorg-keyword-item-keyword existing)))
      (dorg-mutate--raise 'invalid-target "selected item is a protected keyword"
                          :operation 'replace :detail 'protected-keyword
                          :target target))
    (setf (nth index copy) new)
    copy))

(defun dorg-mutate--body-literal-refuse (count patch)
  "Refuse a body-literal PATCH with capped match COUNT before mutation."
  (dorg-mutate--raise 'invalid-target
                      "body literal must match exactly once"
                      :operation (plist-get patch :op)
                      :target (plist-get patch :target)
                      :detail 'body-literal-count :count count))

(defun dorg-mutate--body-literal-replacement (heading patch)
  "Return HEADING's canonical body view after its exact literal PATCH.

Matching is deliberately performed on the one model-owned body view before
either backend considers source mapping.  The result is a string so the plain
adapter can prove a byte mapping independently of the Dolt model adapter."
  (let* ((literal (dorg-mutate--patch-tget (plist-get patch :target) :literal))
         (view (dorg-model-body-view heading))
         (match (dorg-model-body-view-literal-match view literal)))
    (pcase (plist-get match :count)
      (0 (dorg-mutate--body-literal-refuse 0 patch))
      (1 (concat (substring view 0 (plist-get match :begin))
                 (plist-get patch :value)
                 (substring view (plist-get match :end))))
      (_ (dorg-mutate--body-literal-refuse 2 patch)))))

(defun dorg-mutate--body-leading-metadata (items)
  "Return the leading planning/local-PROPERTIES prefix of ITEMS.
This is the part excluded by `dorg-model-body-view' and therefore immutable to
a body-literal replacement."
  (let ((rest items) prefix)
    (while (and rest (cl-typep (car rest) 'dorg-planning-item))
      (push (car rest) prefix)
      (setq rest (cdr rest)))
    (when (and rest
               (cl-typep (car rest) 'dorg-drawer-item)
               (equal "PROPERTIES" (dorg-drawer-item-name (car rest))))
      (push (car rest) prefix))
    (nreverse prefix)))

(defun dorg-mutate--body-view-protected-metadata (view &optional identity-key)
  "Return protected keyword/property signatures physically present in VIEW."
  (let (out drawer)
    (dolist (line (split-string view "\n"))
      (cond
       ((string-match "\\`[ \t]*#\\+\\([^:\n]+\\):" line)
        (let ((key (match-string 1 line)))
          (when (dorg-protected-keyword-p key)
            (push (list :keyword key line) out))))
       ((string-match "\\`[ \t]*:\\([[:alnum:]_@#%:-]+\\):[ \t]*\\'" line)
        (setq drawer (match-string 1 line))
        (when (equal drawer "END") (setq drawer nil)))
       ((and drawer
             (string-match "\\`[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*\\)\\'" line))
        (let ((key (match-string 1 line)))
          (when (or (and identity-key
                         (string-equal (upcase key) (upcase identity-key)))
                    (dorg-mutate--protected-body-property-p key))
            (push (list :drawer drawer key (match-string 2 line)) out))))))
    (nreverse out)))

(defun dorg-mutate--protected-body-property-p (key)
  "Return non-nil when body drawer KEY is DOrg-owned metadata.

The body feature is based on the existing DOrg ownership predicate: today
registered families are all in the case-insensitive DORG_ namespace, and the
local identity key is named separately for clarity."
  (or (equal key dorg-id-property)
      (dorg-protected-keyword-p key)
      ;; Keep the registered-family seam explicit even while every current
      ;; family lives beneath DORG_.  A body postimage must remain protected
      ;; if the generic namespace rule ever narrows.
      (dorg-owned-metadata-key-p key)))

(defun dorg-mutate--parse-body-view-items-raw (view patch)
  "Parse canonical body VIEW using DOrg's item grammar.

PATCH supplies only typed diagnostic context.  Callers decide which semantic
postimage invariant to compare with the parsed result."
  (with-temp-buffer
    ;; DOrg's model parser intentionally admits only identity-bearing headings.
    ;; Give the temporary constructor an identity, then remove that synthetic
    ;; leading metadata from the returned body items.
    (insert "* DOrg literal body\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n" view)
    (let ((org-inhibit-startup t)) (org-mode))
    (let* ((org-headings (dorg-mutate--org-heading-shape))
           (parsed (dorg-parse-buffer (current-buffer)))
           (headings (dorg-parse-result-headings parsed))
           (heading (cl-find "DOrg literal body" headings
                             :key #'dorg-heading-title :test #'equal)))
      (unless (and heading (= (length org-headings) 1))
        (dorg-mutate--raise 'invalid-target
                            "body replacement is not representable"
                            :operation (plist-get patch :op)
                            :detail 'structural-payload-boundary
                            :value (plist-get patch :value)))
      (let* ((items (dorg-heading-items heading))
             (prefix (dorg-mutate--body-leading-metadata items)))
        (nthcdr (length prefix) items)))))

(defun dorg-mutate--parse-body-view-items
    (view patch &optional before-items before-view identity-key)
  "Parse canonical body VIEW after validating PATCH's literal-body boundary.
The temporary heading delegates the item grammar to DOrg's parser instead of
maintaining a second body parser in the mutation engine."
  (unless (equal (dorg-mutate--body-view-protected-metadata before-view identity-key)
                 (dorg-mutate--body-view-protected-metadata view identity-key))
    (dorg-mutate--raise 'invalid-target
                        "body replacement changes protected metadata"
                        :operation (plist-get patch :op)
                        :detail 'structural-payload-boundary))
  (let ((items (dorg-mutate--parse-body-view-items-raw view patch)))
    (dorg-mutate--validate-body-postimage items before-items patch identity-key)
    items))

(defun dorg-mutate--body-nontext-shape (items)
  "Return the ordered constructor shape of non-text body ITEMS."
  (cl-loop for item in items
           unless (cl-typep item 'dorg-text-item)
           collect (cond ((cl-typep item 'dorg-keyword-item)
                          (list :keyword (dorg-keyword-item-keyword item)))
                         ((cl-typep item 'dorg-drawer-item)
                          (list :drawer (dorg-drawer-item-name item)))
                         (t (list (type-of item))))))

(defun dorg-mutate--body-protected-metadata (items &optional identity-key)
  "Return the protected body metadata carried by ITEMS, in source order.

The value is deliberately a full signature, not merely a set of names: an
edit may not rewrite a protected value just because the corresponding item
already existed in the preimage."
  (cl-loop for item in items append
           (cond
            ((and (cl-typep item 'dorg-keyword-item)
                  (dorg-protected-keyword-p
                   (dorg-keyword-item-keyword item)))
             (list (list :keyword
                         (dorg-keyword-item-keyword item)
                         (dorg-keyword-item-value item))))
            ((cl-typep item 'dorg-drawer-item)
             (cl-loop for cell in (dorg-drawer-item-properties item)
                      when (or (and identity-key
                                    (string-equal (upcase (car cell))
                                                  (upcase identity-key)))
                               (dorg-mutate--protected-body-property-p (car cell)))
                      collect (list :drawer
                                    (dorg-drawer-item-name item)
                                    (car cell) (cdr cell)))))))

(defun dorg-mutate--validate-body-postimage
    (items before-items patch &optional identity-key)
  "Refuse unsafe constructor or protected-metadata changes in body ITEMS.

This is the shared composed-postimage guard for the literal-body and
positional-text doors.  Literal body edits add the non-text shape rule;
positional text edits retain their existing payload rule and use the common
heading/protected-metadata portion after the physical splice."
  (when (and before-items
             (not (equal (dorg-mutate--body-nontext-shape before-items)
                         (dorg-mutate--body-nontext-shape items))))
    (dorg-mutate--raise 'invalid-target "body replacement changes constructors"
                        :operation (plist-get patch :op)
                        :detail 'structural-payload-boundary))
  (unless (equal (dorg-mutate--body-protected-metadata before-items identity-key)
                 (dorg-mutate--body-protected-metadata items identity-key))
    (dorg-mutate--raise 'invalid-target
                        "postimage changes protected body metadata"
                        :operation (plist-get patch :op)
                        :detail 'structural-payload-boundary)))

(defun dorg-mutate--validate-item-postimage
    (items before-items patch &optional batch-context)
  "Refuse ITEM PATCH when its rendered body changes protected metadata.

Unlike the model-only item transform, this reparses the serialized body.  The
declared item model must survive that reparse: a text payload that becomes a
keyword/drawer, or a drawer line that becomes a heading, refuses.  In contrast
a declared ordinary keyword or drawer remains a legal item-door payload.

Protected metadata is compared over the complete item sequences.  In contrast,
the render/reparse constructor check operates on each image's own body: a
leading planning/local-PROPERTIES prefix is not body grammar.  Keeping these
boundaries distinct makes dropping or forging such a prefix visible to the
protected-signature comparison."
  (let* ((after-prefix (dorg-mutate--body-leading-metadata items))
         (after-body (nthcdr (length after-prefix) items))
         (before-full-view (dorg-mutate--body-items-view before-items))
         (after-full-view (dorg-mutate--body-items-view items))
         (after-view (dorg-mutate--body-items-view after-body)))
    (let ((identity-key (and batch-context
                             (gethash dorg-mutate--identity-context-key
                                      batch-context))))
      (unless (equal (dorg-mutate--body-view-protected-metadata
                      before-full-view identity-key)
                     (dorg-mutate--body-view-protected-metadata
                      after-full-view identity-key))
      (dorg-mutate--raise 'invalid-target
                          "item postimage changes protected body metadata"
                          :operation (plist-get patch :op)
                          :detail 'structural-payload-boundary))
      (let ((reparsed (dorg-mutate--parse-body-view-items-raw after-view patch)))
        (unless (equal (dorg-mutate--body-nontext-shape after-body)
                       (dorg-mutate--body-nontext-shape reparsed))
          (dorg-mutate--raise 'invalid-target
                              "item postimage changes constructors"
                              :operation (plist-get patch :op)
                              :detail 'structural-payload-boundary))))))

(defun dorg-mutate--org-heading-shape ()
  "Return the complete Org headline shape of the current buffer.

Unlike DOrg's persistence model, this includes an injected idless headline.
It is therefore suitable for proving constructor-boundary preservation."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (list (org-element-property :level headline)
            (org-element-property :raw-value headline)))))

(defun dorg-mutate--body-items-view (items)
  "Return the canonical view for ITEMS already known to be body-only."
  (mapconcat #'dorg-model--body-view-item items ""))

(defun dorg-mutate--apply-body-literal-to-items (items patch &optional batch-context)
  "Apply PATCH to body-only ITEMS, returning parsed replacement items."
  (let* ((view (dorg-mutate--body-items-view items))
         (literal (dorg-mutate--patch-tget (plist-get patch :target) :literal))
         (match (dorg-model-body-view-literal-match view literal)))
    (pcase (plist-get match :count)
      (0 (dorg-mutate--body-literal-refuse 0 patch))
      (1 (dorg-mutate--parse-body-view-items
          (concat (substring view 0 (plist-get match :begin))
                  (plist-get patch :value)
                  (substring view (plist-get match :end)))
          patch items view
          (and batch-context
               (gethash dorg-mutate--identity-context-key batch-context))))
      (_ (dorg-mutate--body-literal-refuse 2 patch)))))

(defun dorg-mutate--apply-body-literal-to-items-for-source-plan
    (items patch &optional batch-context)
  "Simulate a body literal PATCH for an ordered plain-source plan.

Use the parsed postimage, not a synthetic text placeholder: later ordinary
item selectors must see the same item sequence that phase two will apply."
  (let* ((view (dorg-mutate--body-items-view items))
         (literal (dorg-mutate--patch-tget (plist-get patch :target) :literal))
         (match (dorg-model-body-view-literal-match view literal)))
    (pcase (plist-get match :count)
      (0 (dorg-mutate--body-literal-refuse 0 patch))
      (1 (dorg-mutate--parse-body-view-items
          (concat (substring view 0 (plist-get match :begin))
                  (plist-get patch :value)
                  (substring view (plist-get match :end)))
          patch items view
          (and batch-context
               (gethash dorg-mutate--identity-context-key batch-context))))
      (_ (dorg-mutate--body-literal-refuse 2 patch)))))

(defun dorg-mutate--apply-body-literal-to-heading
    (heading patch &optional batch-context)
  "Return a deep copy of HEADING with one exact body literal replaced.
Leading planning and the immediate local PROPERTIES drawer remain verbatim in
the model.  The body is reparsed by the existing DOrg parser, so structural
payload is refused at the same boundary as other text mutations."
  (let* ((replacement (dorg-mutate--body-literal-replacement heading patch))
         (prefix (dorg-mutate--body-leading-metadata (dorg-heading-items heading)))
         (copy (dorg-heading-deep-copy heading)))
    (setf (dorg-heading-items copy)
          (append prefix
                  (dorg-mutate--parse-body-view-items
                   replacement patch
                   (nthcdr (length prefix) (dorg-heading-items heading))
                   (dorg-model-body-view heading)
                   (and batch-context
                        (gethash dorg-mutate--identity-context-key batch-context)))))
    (dorg-heading-compute-items-hash copy)
    copy))

(defun dorg-mutate--apply-delete-item
    (items patch &optional batch-context)
  "Return a new item list with typed single-keyword PATCH removed."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (index (dorg-mutate--resolve-item-index
                 copy target patch))
         (existing (nth index copy)))
    (when (and (eq (car target) :item)
               (cl-typep existing 'dorg-keyword-item)
               (dorg-protected-keyword-p
                (dorg-keyword-item-keyword existing)))
      (dorg-mutate--raise 'invalid-target "selected item is a protected keyword"
                          :operation 'delete :detail 'protected-keyword
                          :target target))
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
The replacement sequence occupies the first matching member's slot.  The
unmatched sequence keeps its order; an absent key materializes at host end."
  (let* ((target (plist-get patch :target))
         (copy (dorg-mutate--items-copy items))
         (removed nil)
         (kept nil)
         (first-index nil)
         (key (dorg-mutate--patch-tget target :name)))
    (cl-loop for item in copy for index from 0
             if (dorg-mutate--item-matches-target-p item target)
             do (progn
                  (unless first-index (setq first-index index))
                  (push index removed))
             else do (push item kept))
    (setq kept (nreverse kept))
    (dorg-mutate--item-batch-context-remove-items
     batch-context (dorg-mutate--patch-tget target :host) (nreverse removed))
    (let ((replacement
           (mapcar (lambda (value)
                     (dorg-keyword-item :keyword key :value value :post-blank 0))
                   (plist-get patch :values))))
      (if first-index
          ;; FIRST-INDEX is in COPY; subtract prior removed members to obtain
          ;; its index in KEPT, then fill that slot with the new run.
          (let ((kept-index
                 (cl-count-if (lambda (i) (< i first-index)) removed)))
            (setq kept-index (- first-index kept-index))
            (append (cl-subseq kept 0 kept-index)
                    replacement
                    (nthcdr kept-index kept)))
        (append kept replacement)))))

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
    ((or `(:before ,n) `(:after ,n))
     (let ((base (if (and (integerp n) (<= 1 n) (<= n (length lines)))
                     (+ (1- n) (if (eq (car position) :after) 1 0))
                   (dorg-mutate--raise
                    'invalid-target "drawer-line insertion anchor is missing"
                    :operation 'insert :detail 'missing :position position))))
       (if context
           (let* ((key (dorg-mutate--item-insert-cursor-key target position))
                  (cursor (gethash key context))
                  (index (or cursor base)))
             (puthash key (1+ index) context)
             index)
         base)))
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
               (cursor-key (and batch-context
                                (not (eq position :after-last))
                                (dorg-mutate--item-insert-cursor-key target position)))
               (idx (dorg-mutate--drawer-line-index lines target position batch-context)))
          (setf (dorg-drawer-item-lines drawer)
                (dorg-mutate--list-insert-at
                 lines idx (copy-sequence line)))
          (dorg-mutate--item-batch-context-shift-drawer-lines
           batch-context (dorg-mutate--patch-tget target :host) name idx cursor-key))
      (if (and (eq (plist-get patch :missing) :create)
               (memq position '(:before-first :after-last)))
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

(defun dorg-mutate--apply-drawer-line-selection (items patch)
  "Apply a private replace/delete drawer-line PATCH to ITEMS.
An absent drawer is materialized only for an all-lines replacement; an all
delete is a no-op.  Occurrence operations require an existing selected line."
  (let* ((copy (dorg-mutate--items-copy items))
         (target (plist-get patch :target))
         (name (dorg-mutate--patch-tget target :name))
         (occurrence (dorg-mutate--patch-tget target :occurrence))
         (op (plist-get patch :op))
         (drawer (cl-find-if
                  (lambda (item)
                    (and (cl-typep item 'dorg-drawer-item)
                         (null (dorg-drawer-item-properties item))
                         (equal name (dorg-drawer-item-name item))))
                  copy)))
    (cond
     ((and occurrence (null drawer))
      (dorg-mutate--raise 'invalid-target "drawer-line target drawer is missing"
                          :operation op :target target :detail 'missing))
     ((and occurrence (> occurrence (length (dorg-drawer-item-lines drawer))))
      (dorg-mutate--raise 'invalid-target "drawer-line occurrence is missing"
                          :operation op :target target :detail 'missing))
     (occurrence
      (let* ((index (1- occurrence))
             (lines (dorg-drawer-item-lines drawer)))
        (setf (dorg-drawer-item-lines drawer)
              (if (eq op 'delete)
                  (append (cl-subseq lines 0 index) (nthcdr (1+ index) lines))
                (append (cl-subseq lines 0 index)
                        (list (copy-sequence (plist-get patch :value)))
                        (nthcdr (1+ index) lines))))))
     ((eq op 'delete) copy)
     (drawer
      (setf (dorg-drawer-item-lines drawer)
            (mapcar #'copy-sequence (plist-get patch :values))))
     (t
      (let ((index (dorg-mutate--drawer-line-create-index copy)))
        (setq copy
              (dorg-mutate--list-insert-at
               copy index
               (dorg-drawer-item :name (copy-sequence name)
                                 :lines (mapcar #'copy-sequence
                                                (plist-get patch :values))
                                 :post-blank 0))))))
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
    (`(delete :text)
     (dorg-mutate--apply-delete-item items patch batch-context))
    (`(replace :keyword)    (dorg-mutate--apply-replace-item items patch))
    (`(delete :keyword)
     (dorg-mutate--apply-delete-item items patch batch-context))
    (`(replace :item)      (dorg-mutate--apply-replace-item items patch))
    (`(delete :item)
     (dorg-mutate--apply-delete-item items patch batch-context))
    (`(insert :item)
     (dorg-mutate--apply-insert-item items patch batch-context))
    (`(delete :keywords)
     (dorg-mutate--apply-delete-keywords items patch batch-context))
    (`(replace :keywords)
     (dorg-mutate--item-batch-context-clear-host
      batch-context (dorg-mutate--patch-tget (plist-get patch :target) :host))
     (dorg-mutate--apply-replace-keywords items patch batch-context))
    (`(replace :items)
     (dorg-mutate--apply-replace-items items patch batch-context))
    (`(replace :drawer-lines)
     (dorg-mutate--item-batch-context-clear-host
      batch-context (dorg-mutate--patch-tget (plist-get patch :target) :host))
     (dorg-mutate--apply-drawer-line-selection items patch))
    (`(delete :drawer-lines)
     (dorg-mutate--apply-drawer-line-selection items patch))
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
    (when (memq (cadr (dorg-mutate--patch-signature patch))
                '(:text :texts :item :items :drawer-lines))
      (dorg-mutate--validate-item-postimage
       (dorg-heading-items copy) (dorg-heading-items heading) patch batch-context))
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
host.  The field-granular S1 heading-line family supports nonempty subsets of
:title, :todo, :priority, :tags and :commented at that host; the S2 planning
editor is likewise concrete-heading only.  Its root and every other
structural/heading form refuses with `no-source-preserving-editor'
+ the exact C3 `:variant', and a `none' policy refuses every cell with
`identity-none'.
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
     ;; variant and every structural/heading whole-verb have no
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
;; editor.  Structural verbs have none at all; planning updates have their
;; own concrete-heading S2 editor.  Heading updates
;; admit nonempty subsets of the five intrinsic line fields at a concrete
;; heading; the separate constructor admits one concrete-parent child whose
;; section can be rendered and reparsed without ambiguity.
;; Dolt is a full backend and supports every cell.  ONE classifier
;; (`dorg-mutate--c3-refusal') projects the cell into the one complete refusal
;; record consumed by plan and apply, so both reject in lockstep (D4).

(defconst dorg-mutate--c3-surgical-patches
  '((put :property) (delete :property)
    (replace :keyword) (replace :keywords) (insert :keywords)
    (delete :keyword) (delete :keywords)
    (insert :texts) (replace :text) (replace :body) (delete :text) (replace :items)
    (replace :item) (delete :item) (insert :item)
    (insert :drawer-lines) (replace :drawer-lines) (delete :drawer-lines))
  "Typed patch signatures with a source-preserving heading editor.")

(defconst dorg-mutate--c3-whole-patch-slices
  '(((update :heading) . S1)
    ((insert :tree) . S3) ((delete :heading) . S3)
    ((delete :subtree) . S3) ((move :subtree) . S3))
  "Typed patch signatures with no plain-org editor and their C3 slice.")

(defconst dorg-mutate--c3-field-cells
  '((((update :heading) heading (:commented))
     :status supported :slice S1
     :variant (:host heading :fields (:commented)))
    (((insert :tree) heading (:constructor))
     :status supported :slice S3
     :variant (:host heading :constructor :plain-child)))
  "Field-granular exceptions to whole-signature plain-org C3 cells.
Each key is (SIGNATURE HOST FIELD-KEYS); its plist is the complete cell fact.
The complete nonempty five-field heading-line family is recognized by
`dorg-mutate--c3-field-cell'; the concrete-parent, unanchored public
constructor is the other field-granular cell.  Tree editing remains outside
this table.")

(defconst dorg-mutate--c3-heading-line-fields
  '(:title :todo :priority :tags :commented)
  "Intrinsic headline slots with a source-preserving plain-org editor.")

(defun dorg-mutate--c3-plain-constructor-p (patch)
  "Non-nil when PATCH is the bounded public plain-child constructor cell.
The parent is classified separately by the C3 host.  Anchors, recursive
children, and raw typed payload extensions stay outside this ratified cell;
section representability is checked separately during phase one."
  (let* ((target (plist-get patch :target))
         (payload (plist-get patch :heading))
         (keys (cl-loop for (key _value) on payload by #'cddr collect key)))
    (and (equal (dorg-mutate--patch-signature patch) '(insert :tree))
         (not (plist-member (cdr target) :before))
         (not (plist-member (cdr target) :after))
         (not (plist-member (cdr target) :offset))
         (or (not (plist-member payload :commented))
             (memq (plist-get payload :commented) '(nil t)))
         (cl-every (lambda (key)
                     (memq key '(:title :todo :priority :tags :commented :items)))
                   keys))))

(defun dorg-mutate--c3-field-keys (patch)
  "Return PATCH's bounded C3 field-cell discriminator, or nil."
  (pcase (dorg-mutate--patch-signature patch)
    (`(update :heading)
     (cl-loop for (key _value) on (plist-get patch :fields) by #'cddr
              collect key))
    (`(insert :tree)
     (and (dorg-mutate--c3-plain-constructor-p patch) '(:constructor)))))

(defun dorg-mutate--c3-patch-host (patch)
  "Return PATCH C3 host variant, either `heading' or `root'."
  (let ((signature (dorg-mutate--patch-signature patch)))
    (if (and (or (member signature dorg-mutate--c3-surgical-patches)
                 (equal signature '(update :heading))
                 (equal signature '(update :planning))
                 (equal signature '(insert :tree)))
             (eq (dorg-mutate--source-preserving-patch-host patch) :root))
        'root
      'heading)))

(defun dorg-mutate--c3-field-cell (patch host)
  "Return field-granular C3 data for PATCH at HOST, or nil."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (fields (dorg-mutate--c3-field-keys patch)))
    (or (alist-get (list signature host fields)
                  dorg-mutate--c3-field-cells nil nil #'equal)
        (when (and (equal signature '(update :heading))
                   (eq host 'heading)
                   fields
                   (cl-every (lambda (field)
                               (memq field dorg-mutate--c3-heading-line-fields))
                             fields))
          (list :status 'supported :slice 'S1
                :variant (list :host 'heading :fields fields))))))

(defun dorg-mutate--c3-cell (patch host)
  "Return the plain-org C3 (STATUS . SLICE) for PATCH at HOST."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (field-cell (dorg-mutate--c3-field-cell patch host)))
    (cond
     ((equal signature '(update :planning))
      (if (eq host 'heading) (cons 'supported 'S2) (cons 'unsupported 'S4)))
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
      (when (or (member signature dorg-mutate--c3-surgical-patches)
                (equal signature '(update :planning)))
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
DELETED accumulate affected heading ids for the result diagnostics.
ORDER-FACTS-FN, when the load was sparse, returns a parent's complete ordered
\(ID . SORT-KEY) child facts from the store at the attempt HEAD, keys only;
nil means HEADINGS is complete.  FACTS-CACHE memoizes those reads per parent.
REKEYS maps a heading id to (PARENT-ID . SORT-KEY) for rows this batch rekeyed
without otherwise touching them; they lower to one key-only statement per
parent and never enter CREATED/CHANGED.  VACATED maps a parent id to the
hash set of sort keys this batch freed under it (deleted, moved or rekeyed
rows): the store still holds those keys until commit, so no placement under
that parent may reuse them.  ORIG is the pristine snapshot hash the headings
were copied from (nil for an in-memory model); ordering derives the rows this
batch changed from it (`dorg-mutate--ps-order-delta').
MOVED is the hash set of ids whose ancestry this batch changed: a moved
subtree and each promoted child's subtree.  Their stored offsets may be
unchanged, so index classification takes them from here, not from a level
diff.  FACT-OFFSETS maps an id the ordering facts reported to its stored
level offset, so a sparse candidate can check representability against a
neighbour it never loaded.  PLACEMENT-NEIGHBOURS is (LEFT-ID . RIGHT-ID), the
immediate siblings the latest single-row key allocation placed between (nil
where there is none), so the check reuses that allocation's reads."
  headings root-id created changed deleted
  order-facts-fn orig
  (rekeys (make-hash-table :test 'eql))
  (vacated (make-hash-table :test 'eql))
  (moved (make-hash-table :test 'eql))
  (fact-offsets (make-hash-table :test 'eql))
  placement-neighbours)

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

(defun dorg-mutate--ps-reparent-closure (ps heading)
  "Record HEADING's subtree as MOVED in PS and re-derive its effective levels.
Called after HEADING's parent or offset changed.  No stored offset below
HEADING changes, so nothing here is written; each loaded level is derived
top-down as its parent's level plus its own offset (nil when the parent's
level is unknown), never patched by arithmetic on a previous value."
  (let ((moved (dorg-mutate--ps-moved ps))
        (parent (gethash (dorg-heading-parent-id heading)
                         (dorg-mutate--ps-headings ps))))
    (dolist (h (dorg-mutate--derive-closure
                heading (and parent (dorg-heading-level parent))
                (lambda (h) (dorg-mutate--ps-children ps (dorg-heading-id h)))))
      (puthash (dorg-heading-id h) t moved))))

(defun dorg-mutate--derive-closure (heading base children-fn)
  "Derive HEADING's subtree levels via `dorg-model-derive-closure'.
BASE and CHILDREN-FN are as there.  A heading in the closure with an
invalid stored offset refuses the mutation as an `invariant' failure, so a
move can neither resurrect a numeric level through it nor commit over it."
  (condition-case err
      (dorg-model-derive-closure heading base children-fn)
    (dorg-model-invalid-offset
     (dorg-mutate--raise 'invariant
                         (format "heading %S has invalid level offset %S"
                                 (nth 1 err) (nth 2 err))
                         :heading (nth 1 err) :detail 'invalid-level-offset))))

(defun dorg-mutate--ps-sibling-offset (ps id)
  "Return sibling ID's stored level offset in PS, or nil when ID is nil.
A loaded sibling answers from the candidate; an unloaded one from the
offsets its ordering facts reported.  An offset that is unknown signals
rather than being assumed."
  (when id
    (let* ((h (gethash id (dorg-mutate--ps-headings ps)))
           (offset (if h
                       (dorg-heading-level-offset h)
                     (gethash id (dorg-mutate--ps-fact-offsets ps)))))
      (unless (integerp offset)
        (dorg-mutate--raise 'invariant
                            (format "sibling %S has no known level offset" id)
                            :detail 'unknown-neighbour-offset :heading id))
      offset)))

(defun dorg-mutate--ps-assert-representable (ps heading patch)
  "Refuse HEADING's final placement in PS unless Org can render it faithfully.
Direct-sibling offsets must not increase in render order, or Org re-parents
the later heading.  The neighbours are the two rows the placement's key
allocation already found (`dorg-mutate--ps-placement-neighbours'), so the
check adds no read.  The diagnostic names both neighbours' offsets and, for a
too-small offset, the smallest valid explicit one; nothing is auto-picked."
  (let* ((id (dorg-heading-id heading))
         (pid (dorg-heading-parent-id heading))
         (offset (dorg-heading-level-offset heading))
         (neighbours (dorg-mutate--ps-placement-neighbours ps))
         (pred (dorg-mutate--ps-sibling-offset ps (car neighbours)))
         (succ (dorg-mutate--ps-sibling-offset ps (cdr neighbours))))
    (unless (dorg-model-placement-representable-p offset pred succ)
      (dorg-mutate--raise
       'invariant
       (format (concat "placing heading %S at level offset %d under %S would "
                       "re-parent a sibling when rendered as Org (previous "
                       "sibling offset %S, next sibling offset %S)%s")
               id offset pid pred succ
               (if (and succ (< offset succ))
                   (format "; supply (offset N) with N >= %d" succ)
                 ""))
       :operation (plist-get patch :op) :heading id
       :detail 'nonrepresentable-sibling-order
       :offset offset :previous-offset pred :next-offset succ))))

;;; Sibling placement
;;
;; One ordering path with no correctness-bearing cache.  For a parent P at
;; the attempt snapshot S, let T be the ids whose stored ordering this batch
;; replaces or removes and D the current candidate rows under P.  The
;; effective order is (S \ T) ∪ D, and both sets are derived on demand from
;; records the PS already keeps: the pristine snapshot, the headings, the
;; deleted ids and the pending rekeys.  A neighbour of a candidate key is the
;; better of the indexed store answer (T excluded by id) and the D answer.
;; No sibling body, tag or extension row is ever hydrated for ordering.

(defconst dorg-mutate-rebalance-neighbourhood 64
  "Rows read on each side of an exhausted gap before a local rebalance.
A tunable, not a correctness condition: a cluster that does not fit in this
neighbourhood falls back to the whole sibling group through the same
rebalance function.")

(defun dorg-mutate--ps-vacate (ps parent-id key)
  "Record KEY as freed under PARENT-ID by this batch; the store holds it until commit."
  (when key
    (let ((table (dorg-mutate--ps-vacated ps)))
      (puthash key t (or (gethash parent-id table)
                         (puthash parent-id (make-hash-table :test 'equal) table))))))

(defun dorg-mutate--ps-vacated-under (ps parent-id)
  "Return the hash set of keys this batch vacated under PARENT-ID, or nil."
  (gethash parent-id (dorg-mutate--ps-vacated ps)))

(defun dorg-mutate--ps-order-delta (ps parent-id &optional exclude-ids)
  "Return (T . D) for PARENT-ID: excluded stored ids and current candidate rows.
T lists ids whose stored ordering under PARENT-ID this batch replaced or
removed: deleted rows and repositioned or departed rows (their snapshot
parent or key differs from their candidate), pending rekeys under
PARENT-ID, and EXCLUDE-IDS.  D lists (ID . KEY) for every row currently
under PARENT-ID in the candidate: loaded headings with their current keys
and pending rekeys of unloaded rows, minus EXCLUDE-IDS, deduplicated by id.
Unchanged loaded rows appear in both sources with the same key, which is
harmless.  Derived on demand from the PS; nothing is cached."
  (let ((orig (dorg-mutate--ps-orig ps))
        (excluded (make-hash-table :test 'eql))
        (rows (make-hash-table :test 'eql)))
    (dolist (id exclude-ids) (puthash id t excluded))
    (maphash (lambda (id cell)
               (when (eql (car cell) parent-id)
                 (puthash id t excluded)
                 (puthash id (cdr cell) rows)))
             (dorg-mutate--ps-rekeys ps))
    ;; A deleted row is excluded from S and never a pending row of D.
    (dolist (id (dorg-mutate--ps-deleted ps))
      (puthash id t excluded)
      (remhash id rows))
    (maphash (lambda (id h)
               (let ((o (and orig (gethash id orig))))
                 (when (and o
                            (eql (dorg-heading-parent-id o) parent-id)
                            (not (and (eql (dorg-heading-parent-id h) parent-id)
                                      (equal (dorg-heading-sort-key h)
                                             (dorg-heading-sort-key o)))))
                   (puthash id t excluded)))
               (if (eql (dorg-heading-parent-id h) parent-id)
                   (puthash id (dorg-heading-sort-key h) rows)
                 (remhash id rows)))
             (dorg-mutate--ps-headings ps))
    (dolist (id exclude-ids) (remhash id rows))
    (let (ids cells)
      (maphash (lambda (id _) (push id ids)) excluded)
      (maphash (lambda (id key) (push (cons id key) cells)) rows)
      (cons ids cells))))

(defun dorg-mutate--ps-dedupe-rows (rows)
  "Return ROWS ((ID . KEY) ...) with each id once, first occurrence kept, in linear time."
  (let ((seen (make-hash-table :test 'eql))
        out)
    (dolist (cell rows (nreverse out))
      (unless (gethash (car cell) seen)
        (puthash (car cell) t seen)
        (push cell out)))))

(defun dorg-mutate--ps-neighbours (ps parent-id bound side limit &optional exclude-ids)
  "Return up to LIMIT (ID . KEY) rows under PARENT-ID on SIDE of BOUND, nearest first.
SIDE is `before' (keys strictly below BOUND, greatest first) or `after' (keys
strictly above, smallest first); a nil BOUND means the fan end on that side.
The store answers from the attempt snapshot through the indexed sibling
order with T excluded by id; the pending rows D answer from the PS; the two
ordered lists are merged.  Ordinary placement transfers at most LIMIT rows
from the store regardless of how many siblings the parent has."
  (let* ((delta (and (> limit 0)
                     (dorg-mutate--ps-order-delta ps parent-id exclude-ids)))
         (fn (dorg-mutate--ps-order-facts-fn ps))
         (less (if (eq side 'before)
                   (lambda (a b) (string> a b))
                 (lambda (a b) (string< a b))))
         (inside (lambda (key)
                   (or (null bound)
                       (if (eq side 'before)
                           (string< key bound)
                         (string< bound key)))))
         (pending (cl-remove-if-not (lambda (cell) (funcall inside (cdr cell)))
                                    (cdr delta)))
         (stored (and fn (> limit 0)
                      (funcall fn parent-id bound side limit (car delta)))))
    ;; An unchanged loaded row answers from both sources with the same key;
    ;; keep it once so a rebalance window never carries a row twice.
    (seq-take (sort (dorg-mutate--ps-dedupe-rows (append stored pending))
                    (lambda (a b) (funcall less (cdr a) (cdr b))))
              limit)))

(defun dorg-mutate--ps-group (ps parent-id &optional exclude-ids)
  "Return PARENT-ID's whole effective order as an ascending (ID . KEY) vector.
Used only by the whole-group rebalance fallback; never for ordinary placement."
  (let* ((delta (dorg-mutate--ps-order-delta ps parent-id exclude-ids))
         (fn (dorg-mutate--ps-order-facts-fn ps))
         (stored (and fn (funcall fn parent-id nil nil nil (car delta)))))
    (apply #'vector
           (sort (dorg-mutate--ps-dedupe-rows (append stored (cdr delta)))
                 (lambda (a b) (string< (cdr a) (cdr b)))))))

(defun dorg-mutate--ps-retire-rekey (ps id)
  "Drop ID's pending rekey: a later semantic change now owns its final key.
The row's own row op (or its deletion) carries the final state; an
intermediate rekey must not survive into lowering or projector images."
  (remhash id (dorg-mutate--ps-rekeys ps)))

(defun dorg-mutate--ps-apply-rekeys (ps parent-id old-keys rekeys)
  "Record REKEYS ((ID . KEY) ...) under PARENT-ID without touching content.
OLD-KEYS maps each rekeyed id to the key it held before, which is vacated.  A
loaded heading takes its new key; every rekey is recorded for the key-only
lowering.  Rekeyed rows are not marked changed: their order is preserved, so
no projection needs to see them."
  (dolist (cell rekeys)
    (let* ((id (car cell))
           (h (gethash id (dorg-mutate--ps-headings ps))))
      (dorg-mutate--ps-vacate ps parent-id (gethash id old-keys))
      (when h
        (setf (dorg-heading-sort-key h) (cdr cell)))
      (puthash id (cons parent-id (cdr cell)) (dorg-mutate--ps-rekeys ps)))))

(defun dorg-mutate--ps-rebalance (ps parent-id rows index count left-bound right-bound)
  "Rebalance ROWS to place COUNT keys after INDEX; return the new keys.
ROWS is an ascending (ID . KEY) vector; LEFT-BOUND and RIGHT-BOUND are the
unchanged keys just outside it (nil at a fan end).  Signals
`dorg-sortkey-exhausted' when ROWS cannot fit between the bounds."
  (let* ((plan (dorg-model-plan-window
                rows index count (dorg-mutate--ps-vacated-under ps parent-id)
                left-bound right-bound))
         (old (make-hash-table :test 'eql)))
    (cl-loop for cell across rows do (puthash (car cell) (cdr cell) old))
    (dorg-mutate--ps-apply-rekeys ps parent-id old (plist-get plan :rekeys))
    (plist-get plan :keys)))

(defun dorg-mutate--ps-allocate-keys (ps parent-id slot ids &optional exclude-ids)
  "Return fresh keys placing IDS under PARENT-ID at SLOT, in order.
SLOT is (:after ID . KEY), (:before ID . KEY), `:head' or `:tail'; ID is the
anchor row itself, or nil when KEY is only a bound (a slot a deleted row
vacated).  The immediate neighbours come from `dorg-mutate--ps-neighbours'
\(one row per side) and the pure allocator places between them.  On
exhaustion, a neighbourhood of `dorg-mutate-rebalance-neighbourhood' rows per
side with the next rows as explicit outside bounds is rebalanced; if it cannot
fit, the whole sibling group is, through the same function.  Every key is
fresh for the parent: distinct from every live key and from every key this
batch vacated under it, so no statement order can violate the sibling unique
index.  EXCLUDE-IDS are rows being placed that still carry stale keys."
  (pcase-let* ((count (length ids))
               (`(,kind ,anchor-id ,key)
                (pcase slot
                  (`(,kind ,id . ,key) (list kind id key))
                  (:head (list :head nil nil))
                  (:tail (list :tail nil nil))
                  (_ (error "Unknown placement slot %S" slot))))
               (anchor (and anchor-id (cons anchor-id key)))
               (vacated (dorg-mutate--ps-vacated-under ps parent-id)))
    (cl-flet ((below (n)
                ;; Rows strictly below the slot, nearest first.
                (cond ((eq kind :head) nil)
                      ((and (eq kind :after) anchor)
                       (cons anchor (dorg-mutate--ps-neighbours
                                     ps parent-id key 'before (1- n) exclude-ids)))
                      (t (dorg-mutate--ps-neighbours
                          ps parent-id key 'before n exclude-ids))))
              (above (n)
                (cond ((eq kind :tail) nil)
                      ((and (eq kind :before) anchor)
                       (cons anchor (dorg-mutate--ps-neighbours
                                     ps parent-id key 'after (1- n) exclude-ids)))
                      (t (dorg-mutate--ps-neighbours
                          ps parent-id key 'after n exclude-ids)))))
      (let ((left (car (below 1)))
            (right (car (above 1))))
        ;; The same two rows bound the placed heading in render order, so the
        ;; representability check reuses them instead of reading again.
        (setf (dorg-mutate--ps-placement-neighbours ps)
              (cons (car left) (car right)))
        (condition-case nil
            (dorg-model-plan-gap (cdr left) (cdr right) count vacated)
          (dorg-sortkey-exhausted
           (let* ((n dorg-mutate-rebalance-neighbourhood)
                  (lows (below (1+ n)))
                  (highs (above (1+ n)))
                  (left-bound (and (> (length lows) n) (cdr (nth n lows))))
                  (right-bound (and (> (length highs) n) (cdr (nth n highs))))
                  (low-part (nreverse (seq-take lows n)))
                  (window (vconcat low-part (seq-take highs n))))
             (condition-case nil
                 (dorg-mutate--ps-rebalance
                  ps parent-id window (1- (length low-part)) count
                  left-bound right-bound)
               (dorg-sortkey-exhausted
                (let* ((group (dorg-mutate--ps-group ps parent-id exclude-ids))
                       (gindex (cond ((eq kind :head) -1)
                                     ((eq kind :tail) (1- (length group)))
                                     (t (dorg-mutate--ps-group-index
                                         group key (eq kind :after))))))
                  (dorg-mutate--ps-rebalance
                   ps parent-id group gindex count nil nil)))))))))))

(defun dorg-mutate--ps-group-index (group key inclusive)
  "Return the index of the last GROUP entry below KEY (at or below with INCLUSIVE), or -1."
  (let ((lo 0) (hi (length group)))
    (while (< lo hi)
      (let* ((mid (/ (+ lo hi) 2))
             (cell-key (cdr (aref group mid))))
        (if (if inclusive (not (string< key cell-key)) (string< cell-key key))
            (setq lo (1+ mid))
          (setq hi mid))))
    (1- lo)))

(defun dorg-mutate--ps-slot (ps parent-id before-id after-id patch)
  "Return the placement slot under PARENT-ID for BEFORE-ID / AFTER-ID.
The slot carries the anchor id and its key; with neither anchor it is `:tail'.  Signals `invalid-target' when an
anchor is not a loaded sibling under PARENT-ID."
  (cl-flet ((anchor-key (id)
              (let ((h (gethash id (dorg-mutate--ps-headings ps))))
                (unless (and h (eql (dorg-heading-parent-id h) parent-id))
                  (dorg-mutate--raise
                   'invalid-target
                   (format "position anchor %S is not a sibling here" id)
                   :operation (plist-get patch :op) :target id
                   :detail 'missing))
                (dorg-heading-sort-key h))))
    (cond
     (before-id (cons :before (cons before-id (anchor-key before-id))))
     (after-id (cons :after (cons after-id (anchor-key after-id))))
     (t :tail))))

(defun dorg-mutate--ps-place (ps parent-id id before-id after-id patch
                                 &optional exclude-ids)
  "Return one fresh key placing ID under PARENT-ID at the anchored slot."
  (car (dorg-mutate--ps-allocate-keys
        ps parent-id (dorg-mutate--ps-slot ps parent-id before-id after-id patch)
        (list id) exclude-ids)))

;;; Typed-patch model mutations

(defun dorg-mutate--patch-address (patch)
  "Return PATCH's addressed heading/root host token."
  (let ((target (plist-get patch :target)))
    (pcase (car target)
      ((or :heading :subtree) (dorg-mutate--patch-target-host target))
    ((or :property :keyword :keywords :planning :body :text :texts :items :item
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
    (let* ((before-id (dorg-mutate--ps-anchor-id
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
           (offset (or (dorg-mutate--patch-tget target :offset) 1))
           (h (make-dorg-heading
               :id id :parent-id pid
               ;; A new heading sits OFFSET levels below its parent (1 unless
               ;; history restores a skipped level); its effective level is
               ;; derived from the parent's.
               :level (dorg-model-offset-level (dorg-heading-level parent)
                                               offset)
               :level-offset offset
               :todo (plist-get payload :todo)
               :priority (plist-get payload :priority)
               :commented (and (plist-get payload :commented) t)
               :title (plist-get payload :title)
               :post-blank (or (plist-get payload :post-blank) 0)
               :tags (plist-get payload :tags)
               :items (mapcar (lambda (v) (dorg-mutate--item-from-value v))
                               (plist-get payload :items)))))
      ;; Placement in the parent's fan (keys only, never hydrated content);
      ;; a gap without room rekeys the smallest window with room.
      (setf (dorg-heading-sort-key h)
            (dorg-mutate--ps-place
             ps pid id before-id effective-after-id patch))
      (dorg-heading-compute-items-hash h)
      (puthash id h (dorg-mutate--ps-headings ps))
      (dorg-mutate--ps-assert-representable ps h patch)
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
reference the node by id).  The moved root's level offset becomes the
patch's :offset, or 1 when omitted, even when the parent is unchanged; no
descendant offset changes.  The final placement must be representable as
Org, and the moved closure's effective levels are then re-derived."
  (let* ((to (plist-get patch :to))
         (h (dorg-mutate--ps-heading ps (dorg-mutate--patch-address patch) patch))
         (parent-host (dorg-mutate--patch-host (plist-get to :under)))
         (old-parent-id (dorg-heading-parent-id h))
         (old-key (dorg-heading-sort-key h)))
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
        (setf (dorg-heading-level-offset h) (or (plist-get to :offset) 1))
        (setf (dorg-heading-parent-id h) (dorg-heading-id np))))
    (when (or parent-host (plist-member to :before) (plist-member to :after))
      (let* ((before-id (dorg-mutate--ps-anchor-id
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
        ;; The mover leaves its old slot (its key stays vacated until commit)
        ;; and is placed under the destination, excluded from that parent's
        ;; pending rows since it still carries its old key.  Its final key is
        ;; now this move's; an earlier rekey of it is retired.
        (dorg-mutate--ps-vacate ps old-parent-id old-key)
        (dorg-mutate--ps-retire-rekey ps (dorg-heading-id h))
        (setf (dorg-heading-sort-key h)
              (dorg-mutate--ps-place
               ps (dorg-heading-parent-id h) (dorg-heading-id h) before-id
               effective-after-id patch (list (dorg-heading-id h))))
        (unless before-id
          (dorg-mutate--ps-record-after-cursor
           (dorg-heading-parent-id h) after-id
           (dorg-heading-id h) batch-context))))
    (dorg-mutate--ps-assert-representable ps h patch)
    (dorg-mutate--ps-reparent-closure ps h)
    (push (dorg-heading-id h) (dorg-mutate--ps-changed ps))))

(defun dorg-mutate--ps-delete-heading (ps patch)
  "Apply typed heading/subtree delete PATCH to PS.
A subtree target removes the heading and descendants.  A heading target
the heading is removed and its direct children are promoted to the
deleted heading's parent, keeping their order; each takes the removed
heading's level offset and its subtree is recorded as MOVED."
  (let* ((h (dorg-mutate--ps-heading ps (dorg-mutate--patch-address patch) patch))
         (id (dorg-heading-id h))
         (hash (dorg-mutate--ps-headings ps)))
    (if (eq (car (plist-get patch :target)) :subtree)
        (progn
          (dolist (d (dorg-mutate--ps-descendants ps id))
            (dorg-mutate--ps-retire-rekey ps (dorg-heading-id d))
            (remhash (dorg-heading-id d) hash)
            (push (dorg-heading-id d) (dorg-mutate--ps-deleted ps)))
          (dorg-mutate--ps-vacate
           ps (dorg-heading-parent-id h) (dorg-heading-sort-key h))
          (dorg-mutate--ps-retire-rekey ps id)
          (remhash id hash)
          (push id (dorg-mutate--ps-deleted ps)))
      (let* ((gp (dorg-heading-parent-id h))
             (kids (dorg-mutate--ps-children ps id))
             (hkey (dorg-heading-sort-key h)))
        ;; The deleted heading's key becomes a bare bound (it is excluded
        ;; from the effective order); the children are placed at that slot
        ;; BEFORE they are re-homed, so no stale key enters the pending rows.
        (dorg-mutate--ps-vacate ps gp hkey)
        (dorg-mutate--ps-retire-rekey ps id)
        (let ((keys (dorg-mutate--ps-allocate-keys
                     ps gp (cons :before (cons nil hkey))
                     (mapcar #'dorg-heading-id kids)
                     (list id))))
          (dolist (k kids)
            ;; A promoted child takes the removed heading's place, and so
            ;; its offset; its own descendants keep theirs.  The promoted run
            ;; o, o, ... sits between neighbours that already bracketed o, so
            ;; it is always representable.
            (setf (dorg-heading-level-offset k) (dorg-heading-level-offset h))
            (setf (dorg-heading-parent-id k) gp)
            (setf (dorg-heading-sort-key k) (pop keys))
            (dorg-mutate--ps-retire-rekey ps (dorg-heading-id k))
            (dorg-mutate--ps-reparent-closure ps k)
            (push (dorg-heading-id k) (dorg-mutate--ps-changed ps))))
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
Updates only the planning fields present in PATCH.  An explicit clear removes
the planning item only when all three resulting fields are nil."
  (let* ((target (plist-get patch :target))
         (address (dorg-mutate--patch-address patch))
         (host (dorg-mutate--patch-tget target :host))
         (h (dorg-mutate--ps-heading ps address patch))
         (has-s (plist-member patch :scheduled))
         (has-d (plist-member patch :deadline))
         (has-c (plist-member patch :closed))
         (planning (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item))
                               (dorg-heading-items h))))
    (cond
     (planning
      (when has-s
        (setf (dorg-planning-item-scheduled planning) (plist-get patch :scheduled)))
      (when has-d
        (setf (dorg-planning-item-deadline planning) (plist-get patch :deadline)))
      (when has-c
        (setf (dorg-planning-item-closed planning) (plist-get patch :closed)))
      (when (and (null (dorg-planning-item-scheduled planning))
                 (null (dorg-planning-item-deadline planning))
                 (null (dorg-planning-item-closed planning)))
        (let* ((items (dorg-heading-items h))
               (idx (cl-position planning items :test #'eq)))
          (when idx
            (dorg-mutate--item-batch-context-remove-items
             batch-context host (list idx)))
          (setf (dorg-heading-items h)
                (cl-remove planning items)))))
     (t
      (when (or (plist-get patch :scheduled) (plist-get patch :deadline)
                (plist-get patch :closed))
        ;; Planning is the first line of a heading's section.
        (dorg-mutate--item-batch-context-shift-items
         batch-context host 0 1)
        (setf (dorg-heading-items h)
              (cons (dorg-planning-item :post-blank 0
                                        :scheduled (plist-get patch :scheduled)
                                        :deadline (plist-get patch :deadline)
                                        :closed (plist-get patch :closed))
                    (dorg-heading-items h))))))
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
         `(replace :text) `(replace :items) `(replace :item) `(delete :item)
         `(insert :item) `(insert :drawer-lines) `(replace :drawer-lines)
         `(delete :drawer-lines))
     (dorg-mutate--ps-item-patch ps patch batch-context))
    (`(replace :body)
     (let* ((host (dorg-mutate--item-patch-host patch))
            (heading (dorg-mutate--ps-heading ps host patch))
            (new (dorg-mutate--apply-body-literal-to-heading
                  heading patch batch-context)))
       (puthash (dorg-heading-id heading) new (dorg-mutate--ps-headings ps))
       (push (dorg-heading-id heading) (dorg-mutate--ps-changed ps))))
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
;; patches on an identified heading are supported surgically.  A nonempty
;; subset of the five intrinsic heading-line fields is the sole whole-heading
;; exception and edits only its affected token spans.  One explicit-commented
;; constructor cell appends a child under a concrete heading; omitted-commented,
;; root, anchor, and broader typed constructors remain refused.  Heading
;; deletion, subtree moves, and root-host variants have no
;; in-place editor and FAIL CLOSED `unsupported' before any write — there is no
;; whole-tree fallback for plain-org.

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
     :id id :level level :level-offset 1
     :todo (plist-get payload :todo)
     :priority (plist-get payload :priority)
     :commented (and (plist-get payload :commented) t)
     :title (plist-get payload :title)
     :post-blank 0
     :tags (plist-get payload :tags)
     :items (mapcar (lambda (value)
                      (dorg-mutate--item-from-value value))
                    (plist-get payload :items)))))

(defun dorg-mutate--source-preserving-constructor-refuse (value)
  "Refuse a constructor section VALUE which does not round-trip through Org."
  (dorg-mutate--raise 'invalid-target
                      "constructor section cannot represent requested content"
                      :value value :detail 'constructor-section-unrepresentable))

(defun dorg-mutate--source-preserving-heading-grammar-apply (grammar)
  "Install captured source headline GRAMMAR in the current temporary buffer."
  (setq-local org-todo-regexp (plist-get grammar :todo-regexp)
              org-todo-keywords-1 (plist-get grammar :todo-keywords)
              org-done-keywords (plist-get grammar :done-keywords)
              org-odd-levels-only (plist-get grammar :odd-levels-only)))

(defun dorg-mutate--source-preserving-constructor-line-fields (patch)
  "Return PATCH's headline slots as a complete line-validator plist."
  (let ((payload (plist-get patch :heading)))
    (list :title (or (plist-get payload :title) "")
          :todo (plist-get payload :todo)
          :priority (plist-get payload :priority)
          :tags (plist-get payload :tags)
          :commented (and (plist-get payload :commented) t))))

(defun dorg-mutate--source-preserving-constructor-item-contents (items)
  "Return the independent ordered item-content expectation for ITEMS.
`dorg-parse-buffer' retains the section's terminal newline in a final text
item.  Account for that documented parser convention without deriving the
expected side from the private reparse."
  (let ((contents (mapcar #'dorg-item-content-string items))
        (last (car (last items))))
    (when (and (cl-typep last 'dorg-text-item)
               (not (string-suffix-p "\n" (dorg-text-item-text last))))
      (setf (car (last contents))
            (concat (car (last contents)) "\n")))
    contents))

(defun dorg-mutate--source-preserving-constructor-section-check
    (patch level grammar)
  "Validate PATCH's whole new section at LEVEL under captured GRAMMAR.
The headline goes through the shared 1qdtb editor's field guard first.  A
fresh render is then reparsed in a private buffer and must yield exactly one
heading at LEVEL with the independently constructed item sequence."
  (let* ((heading (dorg-mutate--source-preserving-constructor-heading
                   patch level nil))
         (fields (dorg-mutate--source-preserving-constructor-line-fields patch))
         (line (concat (make-string level ?*) " placeholder"))
         (expected-items
          (dorg-mutate--source-preserving-constructor-item-contents
           (dorg-heading-items heading)))
         ;; Phase one validates source grammar, not the allocator's future
         ;; DORG_ID.  Physical injection remains exclusively in phase two.
         (rendered (concat (dorg-model--render-heading heading) "\n"
                           (dorg-mutate--source-preserving-render-items
                            (dorg-heading-items heading))))
         ;; The model reader cannot represent an Org-legal skipped level in
         ;; an odd-level file.  Re-render the same requested section at level
         ;; one under ordinary grammar for item validation; this is deliberately
         ;; independent of the source-grammar org-element facet check below.
         (item-rendered
          (let ((item-heading
                 (dorg-mutate--source-preserving-constructor-heading
                  patch 1 nil)))
            (concat (dorg-model--render-heading item-heading) "\n"
                    (dorg-mutate--source-preserving-render-items
                     (dorg-heading-items item-heading)))))
         ;; `dorg-parse-buffer' needs a valid ancestor chain for a standalone
         ;; child.  These private parent lines supply parser context only;
         ;; validation below selects exactly the one requested child.
         (parse-prefix
          (let ((parent-level 1)
                (odd (plist-get grammar :odd-levels-only))
                lines)
            (while (< parent-level level)
              (push (concat (make-string parent-level ?*) " validator parent\n")
                    lines)
              (setq parent-level (+ parent-level (if odd 2 1))))
            (apply #'concat (nreverse lines))))
         (parse-text (concat parse-prefix rendered)))
    ;; This uses the shared editor's check and semantic facet assertion rather
    ;; than a constructor-only approximation of headline grammar.
    (dorg-mutate--source-preserving-heading-line-simulate line fields grammar)
    (with-temp-buffer
      (org-mode)
      (dorg-mutate--source-preserving-heading-grammar-apply grammar)
      (org-set-regexps-and-options)
      (insert parse-text)
      (goto-char (point-min))
      (let* ((org-headings
              (org-element-map (org-element-parse-buffer) 'headline #'identity))
             (org-new
              (cl-remove-if-not
               (lambda (candidate)
                 ;; `org-element' reports a logical level under
                 ;; `org-odd-levels-only'; the constructor's level is its
                 ;; physical star count.  Count physical headings so an
                 ;; injected sibling cannot hide behind that remapping.
                 (save-excursion
                   (goto-char (org-element-property :begin candidate))
                   (= (org-outline-level) level)))
               org-headings))
             (org-actual (car org-new))
             (org-valid
              (and (= (length org-new) 1)
                   (equal (org-element-property :raw-value org-actual)
                          (dorg-heading-title heading))
                   (equal (org-element-property :todo-keyword org-actual)
                          (dorg-heading-todo heading))
                   (equal (when-let ((priority
                                      (org-element-property :priority org-actual)))
                            (char-to-string priority))
                          (dorg-heading-priority heading))
                   (equal (org-element-property :tags org-actual)
                          (dorg-heading-tags heading))
                   (equal (and (org-element-property :commentedp org-actual) t)
                          (dorg-heading-commented heading))))
             (parsed
              (with-temp-buffer
                (org-mode)
                (insert item-rendered)
                (org-set-regexps-and-options)
                (goto-char (point-min))
                (dorg-parse-buffer (current-buffer))))
             (headings
              (cl-remove-if-not
               (lambda (candidate) (= (dorg-heading-level candidate) 1))
               (dorg-parse-result-headings parsed)))
             (actual (car headings))
             (actual-items
              (and actual
                   (dorg-mutate--source-preserving-constructor-item-contents
                    (dorg-heading-items actual))))
             (valid
              (and org-valid
                   (= (length headings) 1)
                   (= (dorg-heading-level actual) 1)
                   (equal actual-items expected-items))))
        (unless valid
          (dorg-mutate--raise
           'invalid-target "constructor section cannot represent requested content"
           :value patch :detail 'constructor-section-unrepresentable
           :expected expected-items :actual actual-items :rendered rendered
           :org-levels (mapcar (lambda (candidate)
                                 (org-element-property :level candidate))
                               org-headings)))))))

(defun dorg-mutate--source-preserving-insert-commented-child
    (patch parent-marker source)
  "Append one public constructor PATCH below PARENT-MARKER.
Return the physically written DORG_ID, or nil for a property-identity faithful
file.  The C3 cell and phase-1 identity guard have already accepted PATCH."
  (let* ((dorg-p (dorg-mutate--identity-dorg-p source))
         (id (and dorg-p (dorg-id-generate)))
         (parent-level (save-excursion
                         (goto-char parent-marker)
                         (org-outline-level)))
         (child-level
          (let ((org-odd-levels-only org-odd-levels-only))
            (org-get-valid-level parent-level 1)))
         (heading
          (dorg-mutate--source-preserving-constructor-heading
           patch child-level id))
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
               ;; Literal-body validation compares the composed pre/postimage,
               ;; so it can distinguish an unchanged ordinary body PROPERTIES
               ;; drawer from a forged protected key.  The older generic item
               ;; guard cannot make that distinction.
               (not (equal (dorg-mutate--patch-signature patch) '(replace :body)))
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
                  (memq (cadr signature) '(:property :body)))
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
property drawer, leading planning item spans still advance BODY-BEGIN."
  (let* ((scan (dorg-mutate--source-preserving-scan marker))
         (items-begin (nth 0 scan))
         (section-end (nth 1 scan))
         (spans (nth 2 scan))
         (planning-end
          (let ((rest spans) end)
            (while (and rest (cl-typep (caar rest) 'dorg-planning-item))
              (setq end (nth 3 (car rest))
                    rest (cdr rest)))
            end))
         (body-begin
          (max (or planning-end items-begin)
               (save-excursion
                 (goto-char marker)
                 (let ((pblock (org-get-property-block)))
                   (if pblock
                       (progn (goto-char (cdr pblock)) (forward-line 1) (point))
                     items-begin)))))
         (body-spans (cl-remove-if (lambda (sp) (< (nth 1 sp) body-begin))
                                   spans)))
    (list body-begin section-end body-spans)))

(defun dorg-mutate--source-preserving-model-body-view (marker source)
  "Return the canonical model body view for the heading at MARKER.

The source scanner has one extra duty: it must prove that its body-item view
is the same match surface seen by direct-model and Dolt callers."
  (save-excursion
    (goto-char marker)
    ;; The byte-span helper is also a lower-level test seam, where SOURCE is
    ;; nil because there is no registered backing store to resolve.  Nil has
    ;; the same declared default policy as every resolved source: `dorg'.
    ;; This chooses the identity key before lookup; a missing matching heading
    ;; still refuses below and is never recovered through another key.
    (let* ((policy (if (dorg-mutate-source-p source)
                       (dorg-mutate--source-identity source)
                     'dorg))
           (key (pcase policy
                  (`(:property ,property) property)
                  ('dorg dorg-id-property)
                  (_ nil)))
           (value (and key (org-entry-get (point) key)))
           (parsed (dorg-parse-buffer (current-buffer)))
           (heading
            (and key value
                 (cl-find-if
                  (lambda (candidate)
                    (pcase policy
                      ('dorg
                       ;; The canonical DOrg identity is typed on the model,
                       ;; rather than retained in its generic property alist.
                       (when-let ((id (dorg-heading-id candidate)))
                         (equal value (number-to-string id))))
                      (`(:property ,property)
                       (equal value
                              (dorg-mutate--heading-property candidate property)))
                      (_ nil)))
                  (dorg-parse-result-headings parsed)))))
      (unless heading
        (dorg-mutate--raise 'invalid-target
                            "body literal host has no canonical model heading"
                            :detail 'body-literal-unmappable))
      (dorg-model-body-view heading))))

(defun dorg-mutate--source-preserving-body-literal-span (patch marker source)
  "Return PATCH's verified plain source range at MARKER, or refuse.

The match always happens first on DOrg's canonical model view.  Plain Org maps
only the selected model range: every intersected item's model serialization
must be byte-identical to its source span and the participating spans must be
contiguous.  Thus an unrelated lossy block does not reject a safe replacement,
but escaped or whitespace-collapsed selected text refuses rather than falling
back to a second source-language.
Return (BEGIN END REPLACEMENT), with positions valid until the next edit."
  (let* ((scan (dorg-mutate--source-preserving-body-scan marker source))
         (spans (nth 2 scan))
         (items (mapcar #'car spans))
         ;; BODY-SCAN has already removed the physical leading planning and
         ;; local drawer.  Do not run `dorg-model-body-view' again here: an
         ;; ordinary body lookalike PROPERTIES drawer is body content.
         (view (dorg-mutate--body-items-view items))
         (literal (dorg-mutate--patch-tget (plist-get patch :target) :literal))
         (match (dorg-model-body-view-literal-match view literal)))
    (unless (equal view (dorg-mutate--source-preserving-model-body-view marker source))
      (dorg-mutate--raise 'invalid-target "body source and model views disagree"
                          :operation (plist-get patch :op)
                          :target (plist-get patch :target)
                          :detail 'body-literal-unmappable))
    (pcase (plist-get match :count)
      (0 (dorg-mutate--body-literal-refuse 0 patch))
      ((pred (lambda (n) (> n 1)))
       (dorg-mutate--body-literal-refuse 2 patch)))
    (let ((model-pos 0)
          (match-begin (plist-get match :begin))
          (match-end (plist-get match :end))
          source-begin source-end previous-end)
      (dolist (span spans)
        (let* ((item (car span))
               (segment (dorg-model--body-view-item item))
               (segment-begin model-pos)
               (segment-end (+ model-pos (length segment)))
               (source-beginning (nth 1 span))
               (source-ending (nth 3 span)))
          (when (and (< segment-begin match-end) (> segment-end match-begin))
            (unless (and (equal segment
                                (buffer-substring-no-properties
                                 source-beginning source-ending))
                         (or (null previous-end) (= previous-end source-beginning)))
              (dorg-mutate--raise 'invalid-target "body literal cannot map to source"
                                  :operation (plist-get patch :op)
                                  :target (plist-get patch :target)
                                  :detail 'body-literal-unmappable))
            (unless source-begin
              (setq source-begin (+ source-beginning
                                    (- match-begin segment-begin))))
            (setq source-end (+ source-beginning
                                (- (min match-end segment-end) segment-begin))
                  previous-end source-ending))
          (setq model-pos segment-end)))
      (unless (and source-begin source-end)
        (dorg-mutate--raise 'invalid-target "body literal cannot map to source"
                            :operation (plist-get patch :op)
                            :target (plist-get patch :target)
                            :detail 'body-literal-unmappable))
      (list source-begin source-end (plist-get patch :value)))))

(defun dorg-mutate--source-preserving-body-literal-postimage-check
    (patch marker source)
  "Refuse a source BODY PATCH that changes the document headline shape.

This is phase-one evidence for dry-run as well as apply.  It uses the same
mapped physical span as phase two, but in a disposable copy of the complete
source so a splice boundary at the beginning of a line cannot manufacture an
idless Org headline."
  (pcase-let ((`(,begin ,end ,replacement)
               (dorg-mutate--source-preserving-body-literal-span
                patch marker source))
              (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (let ((org-inhibit-startup t)) (org-mode))
      (let ((before-shape (dorg-mutate--org-heading-shape)))
        (goto-char begin)
        (delete-region begin end)
        (insert replacement)
        (org-element-cache-reset)
        (unless (equal before-shape (dorg-mutate--org-heading-shape))
          (dorg-mutate--raise 'invalid-target
                              "body replacement is not representable"
                              :operation (plist-get patch :op)
                              :target (plist-get patch :target)
                              :detail 'structural-payload-boundary
                              :value (plist-get patch :value)))))))

(defun dorg-mutate--source-preserving-apply-body-literal (patch marker source)
  "Apply one verified body literal PATCH at MARKER without re-rendering source."
  ;; The byte span is only a faithful transport for the model operation.  It is
  ;; not itself sufficient proof that Org will retain the same constructor
  ;; boundary: a payload can turn body text into a heading.  Reparse the whole
  ;; postimage after the splice and compare the document heading shape and
  ;; canonical body view.  The comparison is against the canonical view before
  ;; this physical splice, not against a reparsed synthetic source: unrelated
  ;; escaped Org bytes outside the selected range remain intentionally lossy.
  (let* ((before-shape (dorg-mutate--source-preserving-heading-shape))
         (before-items
          (mapcar #'car
                  (nth 2 (dorg-mutate--source-preserving-body-scan
                          marker source))))
         (before-view (dorg-mutate--body-items-view before-items))
         (literal (dorg-mutate--patch-tget (plist-get patch :target) :literal))
         (match (dorg-model-body-view-literal-match before-view literal))
         (expected-view
          (concat (substring before-view 0 (plist-get match :begin))
                  (plist-get patch :value)
                  (substring before-view (plist-get match :end)))))
    (pcase-let ((`(,begin ,end ,replacement)
                 (dorg-mutate--source-preserving-body-literal-span
                  patch marker source)))
      (goto-char begin)
      (delete-region begin end)
      (insert replacement)
      (org-element-cache-reset))
    (unless (equal before-shape (dorg-mutate--source-preserving-heading-shape))
      (dorg-mutate--raise 'invalid-target
                          "body replacement is not representable"
                          :operation (plist-get patch :op)
                          :target (plist-get patch :target)
                          :detail 'structural-payload-boundary
                          :value (plist-get patch :value)))
    (unless (equal expected-view
                   (dorg-mutate--body-items-view
                    (mapcar #'car
                            (nth 2 (dorg-mutate--source-preserving-body-scan
                                    marker source)))))
      (dorg-mutate--raise 'invalid-target
                          "body literal postimage does not match its model view"
                          :operation (plist-get patch :op)
                          :target (plist-get patch :target)
                          :detail 'body-literal-unmappable
                          :value (plist-get patch :value)))))

(defun dorg-mutate--source-preserving-heading-shape ()
  "Return the complete parsed heading identity/shape of the current buffer.

This is intentionally whole-document evidence: a plain-source body splice is
valid only when it leaves every constructor boundary intact, not merely when
the selected source range looks plausible in isolation."
  (dorg-mutate--org-heading-shape))

(defun dorg-mutate--source-preserving-splice-patch
    (patch marker source &optional batch-context)
  "Apply a validated item PATCH at MARKER by byte-faithful splice.
Parses the heading's BODY items fresh (positions shift as earlier patches edit
the buffer) and splices only the body region AFTER the out-of-band identity
drawer — a whole body-region replace for typed items replacement, a minimal
content-diff middle splice otherwise.  The identity drawer's bytes are never
touched.  The guard and selector/ordinal resolution already ran in Phase 1."
  (if (equal (dorg-mutate--patch-signature patch) '(replace :body))
      (dorg-mutate--source-preserving-apply-body-literal patch marker source)
    (let* ((bscan (dorg-mutate--source-preserving-body-scan marker source))
         (body-begin (nth 0 bscan))
         (section-end (nth 1 bscan))
         (body-spans (nth 2 bscan))
         (old-items (mapcar #'car body-spans))
         (text-p (equal (dorg-mutate--patch-signature patch) '(replace :text)))
         (before-shape (and text-p (dorg-mutate--source-preserving-heading-shape)))
         (new-items (dorg-mutate--source-preserving-patch-new-items
                     patch old-items batch-context)))
      (if (equal (dorg-mutate--patch-signature patch) '(replace :items))
          (dorg-mutate--source-preserving-region-replace body-begin section-end new-items)
        (dorg-mutate--source-preserving-diff-splice body-spans new-items body-begin section-end))
      (when text-p
        (org-element-cache-reset)
        (unless (equal before-shape (dorg-mutate--source-preserving-heading-shape))
          (dorg-mutate--raise-structural-payload 'replace (plist-get patch :value)))
        (dorg-mutate--validate-body-postimage
         (mapcar #'car (nth 2 (dorg-mutate--source-preserving-body-scan marker source)))
         old-items patch
         (and batch-context
              (gethash dorg-mutate--identity-context-key batch-context)))))))

(defun dorg-mutate--source-preserving-heading-line-facets ()
  "Return Org's semantic intrinsic facets for the heading at point.
The byte editor uses this before and after each local splice: requested fields
must acquire their requested value and every omitted field must retain this
exact parsed value."
  (save-excursion
    (org-back-to-heading t)
    (let ((element (org-element-at-point)))
      (unless (eq (org-element-type element) 'headline)
        (dorg-mutate--source-preserving-heading-line-refuse 'heading nil))
      (list :title (or (org-element-property :raw-value element) "")
            :todo (org-element-property :todo-keyword element)
            :priority (when-let ((priority (org-element-property :priority element)))
                        (char-to-string priority))
            :tags (org-element-property :tags element)
            :commented (and (org-element-property :commentedp element) t)))))

(defun dorg-mutate--source-preserving-heading-line-spans ()
  "Return parser-grammar spans for the heading line at point.
The returned plist names the intrinsic heading tokens and their exact buffer
ranges.  It deliberately follows `org-element--headline-parse-title': Org's
configured TODO regexp and its COMMENT/tag grammar decide what is a token;
the editor only owns the returned ranges and their minimal separators.  The
COMMENT regexp is Org's pinned private parser constant; the semantic
cross-check below makes that dependency fail closed if it ever diverges."
  (save-excursion
    (org-back-to-heading t)
    (let* ((line-end (line-end-position))
           (star-begin (point))
           (star-end (progn (skip-chars-forward "*") (point)))
           (content-begin (progn (skip-chars-forward " \t") (point)))
           todo-begin todo-end priority-begin priority-end
           comment-begin comment-end tags-begin tags-end
           title-begin title-end)
      (when (and org-todo-regexp
                 (let ((case-fold-search nil))
                   (looking-at (concat org-todo-regexp "\\(?: \\|$\\)"))))
        (setq todo-begin (match-beginning 1) todo-end (match-end 1))
        (goto-char (match-end 0))
        (skip-chars-forward " \t"))
      (when (looking-at "\\[#.\\][ \t]*")
        (setq priority-begin (point) priority-end (+ (point) 4))
        (goto-char (match-end 0)))
      (when (let ((case-fold-search nil))
              (looking-at org-element--headline-comment-re))
        (setq comment-begin (point) comment-end (+ (point) (length "COMMENT")))
        (goto-char (match-end 0))
        (skip-chars-forward " \t"))
      (setq title-begin (point) title-end line-end)
      (when (re-search-forward "\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" line-end 'move)
        (setq tags-begin (match-beginning 1) tags-end (match-end 1)
              title-end tags-begin))
      (let ((raw-begin (save-excursion (goto-char title-begin)
                                        (skip-chars-forward " \t" title-end) (point)))
            (raw-end (save-excursion (goto-char title-end)
                                      (skip-chars-backward " \t" title-begin) (point))))
        (let* ((spans (list :star-begin star-begin :star-end star-end
                            :content-begin content-begin
                            :todo-begin todo-begin :todo-end todo-end
                            :priority-begin priority-begin :priority-end priority-end
                            :comment-begin comment-begin :comment-end comment-end
                            :title-begin title-begin :title-end title-end
                            :raw-begin raw-begin :raw-end raw-end
                            :tags-begin tags-begin :tags-end tags-end))
               (facets (dorg-mutate--source-preserving-heading-line-facets))
               (span-text (lambda (begin end)
                            (and begin end
                                 (buffer-substring-no-properties begin end))))
               (same-or-absent
                (lambda (begin end expected)
                  (if expected
                      (equal (funcall span-text begin end) expected)
                    (and (null begin) (null end))))))
          ;; Positions are derived from Org's grammar but never trusted on
          ;; their own: a disagreement with Org's semantic parser fails closed
          ;; before this editor changes any byte.
          (unless
              (and (funcall same-or-absent todo-begin todo-end
                            (plist-get facets :todo))
                   (funcall same-or-absent priority-begin priority-end
                            (when-let ((priority (plist-get facets :priority)))
                              (format "[#%s]" priority)))
                   (funcall same-or-absent comment-begin comment-end
                            (and (plist-get facets :commented) "COMMENT"))
                   (equal (funcall span-text raw-begin raw-end)
                          (plist-get facets :title))
                   (funcall same-or-absent tags-begin tags-end
                            (when-let ((tags (plist-get facets :tags)))
                              (concat ":" (mapconcat #'identity tags ":") ":"))))
            (dorg-mutate--source-preserving-heading-line-refuse 'heading facets))
          spans)))))

(defun dorg-mutate--source-preserving-heading-line-refuse (field value)
  "Raise the stable representability refusal for FIELD and VALUE."
  (dorg-mutate--raise 'invalid-target "heading line cannot represent requested field"
                      :field field :value value :detail 'heading-line-unrepresentable))

(defun dorg-mutate--source-preserving-heading-line-check-fields (fields)
  "Refuse heading FIELDS which cannot round-trip through one Org headline.
The bounded plain-org editor deliberately supports alphabetic A-Z priorities;
numeric Org priority syntax remains outside this source-preserving contract."
  (dolist (field '(:title :todo :priority :tags))
    (when (plist-member fields field)
      (let ((value (plist-get fields field))
            (bad nil))
        (pcase field
          (:title
           (setq bad (or (not (stringp value))
                         (string-match-p "[\r\n]" value)
                         (string-match-p "\\`[ \t]\\|[ \t]\\'" value)
                         (string-match-p "\\`\\(?:COMMENT\\|\\[#.\\]\\)\\(?:[ \t]\\|$\\)" value)
                         (string-match-p "[ \t]:[[:alnum:]_@#%:]+:\\'" value))))
          (:todo
           (setq bad (and (not (null value)) (not (equal value ""))
                          (or (not (stringp value))
                              (not (member value org-todo-keywords-1))))))
          (:priority
           (setq bad (and value (not (and (stringp value)
                                           (= (length value) 1)
                                           (>= (aref value 0) ?A)
                                           (<= (aref value 0) ?Z))))))
          (:tags
           (setq bad (or (not (listp value))
                         (not (cl-every (lambda (tag)
                                          (and (stringp tag)
                                               (string-match-p "\\`[[:alnum:]_@#%]+\\'" tag)))
                                        value))))))
        (when bad
          (dorg-mutate--source-preserving-heading-line-refuse field value))))))

(defun dorg-mutate--source-preserving-heading-line-delete (begin end)
  "Delete owned token BEGIN END and one following ASCII space, if present."
  (goto-char begin)
  (delete-region begin end)
  (when (eq (char-after begin) ?\s) (delete-char 1)))

(defun dorg-mutate--source-preserving-heading-line-apply-fields (fields)
  "Apply validated intrinsic heading FIELDS at point.
This is deliberately the one line-editing primitive for both a private
phase-one copy and phase two.  It recomputes Org-derived spans after each
field, since an earlier token edit can move every later span."
  (dorg-mutate--source-preserving-heading-line-check-fields fields)
  (let ((before (dorg-mutate--source-preserving-heading-line-facets)))
    (dolist (field '(:todo :priority :commented :title :tags))
      (when (plist-member fields field)
        (let* ((value (plist-get fields field))
               (spans (dorg-mutate--source-preserving-heading-line-spans))
               (span-field (if (eq field :commented) :comment field))
               (begin (plist-get spans (intern (format "%s-begin" span-field))))
               (end (plist-get spans (intern (format "%s-end" span-field)))))
          (cond
           ((eq field :todo)
            (if (or (null value) (equal value ""))
                (when begin
                  (dorg-mutate--source-preserving-heading-line-delete begin end))
              (if begin
                  (replace-region-contents begin end (lambda () value))
                (goto-char (plist-get spans :content-begin))
                (insert value " "))))
           ((eq field :priority)
            (if (null value)
                (when begin
                  (dorg-mutate--source-preserving-heading-line-delete begin end))
              (if begin
                  (replace-region-contents begin end
                                           (lambda () (format "[#%s]" value)))
                (goto-char (or (plist-get spans :todo-end)
                               (plist-get spans :content-begin)))
                (skip-chars-forward " \t")
                (insert (format "[#%s] " value)))))
           ((eq field :commented)
            (if value
                (unless begin
                  (goto-char (or (plist-get spans :priority-end)
                                 (plist-get spans :todo-end)
                                 (plist-get spans :content-begin)))
                  (skip-chars-forward " \t")
                  (insert "COMMENT "))
              (when begin
                (dorg-mutate--source-preserving-heading-line-delete begin end))))
           ((eq field :title)
            (if (> (plist-get spans :raw-end) (plist-get spans :raw-begin))
                (replace-region-contents (plist-get spans :raw-begin)
                                         (plist-get spans :raw-end)
                                         (lambda () value))
              (goto-char (plist-get spans :title-begin))
              (when (and (plist-get spans :tags-begin) (eq (char-before) ?\s))
                (delete-char -1))
              (insert value (if (plist-get spans :tags-begin) " " ""))))
           ((eq field :tags)
            (let ((rendered (concat ":" (mapconcat #'identity value ":") ":")))
              (cond
               (begin
                (if value
                    (replace-region-contents begin end (lambda () rendered))
                  (delete-region (if (eq (char-before begin) ?\s)
                                     (1- begin)
                                   begin)
                                 end)))
               (value
                (goto-char (plist-get spans :raw-end))
                (insert " " rendered)))))))))
    (goto-char (line-beginning-position))
    (dorg-mutate--source-preserving-heading-line-assert-fields fields before)))

(defun dorg-mutate--source-preserving-heading-line-fields-match-p (fields before)
  "Return non-nil when the heading at point preserves BEFORE under FIELDS.
This is the semantic half of the faithful editor: a byte splice is accepted
only if Org still reads every requested field as requested and each omitted
field exactly as it read before the splice."
  (let ((after (dorg-mutate--source-preserving-heading-line-facets)))
    (cl-every
     (lambda (field)
       (let ((expected
              (if (plist-member fields field)
                  (pcase field
                    (:todo (let ((value (plist-get fields field)))
                             (and (stringp value) (not (equal value "")) value)))
                    (:priority (when-let ((value (plist-get fields field))) value))
                    (_ (plist-get fields field)))
                (plist-get before field))))
         (equal (plist-get after field) expected)))
     dorg-mutate--c3-heading-line-fields)))

(defun dorg-mutate--source-preserving-heading-line-assert-fields (fields before)
  "Refuse FIELDS when the edited heading does not round-trip from BEFORE."
  (unless (dorg-mutate--source-preserving-heading-line-fields-match-p fields before)
    (dorg-mutate--source-preserving-heading-line-refuse
     'heading fields)))

(defun dorg-mutate--source-preserving-heading-line-grammar ()
  "Capture the current buffer's effective Org headline parse grammar.
These are the buffer-local values consumed by Org's headline parser after it
has read any #+TODO, #+SEQ_TODO or #+TYP_TODO source directives."
  (list :todo-regexp org-todo-regexp
        :todo-keywords org-todo-keywords-1
        :done-keywords org-done-keywords
        :odd-levels-only org-odd-levels-only))

(defun dorg-mutate--source-preserving-heading-line-simulate (line fields grammar)
  "Return LINE after private faithful FIELDS edit under GRAMMAR.
No part of the live source buffer is touched.  The final parse assertion
rejects title text which would become a TODO, COMMENT, priority, or tag token."
  (with-temp-buffer
    (org-mode)
    (dorg-mutate--source-preserving-heading-grammar-apply grammar)
    (insert line)
    (goto-char (point-min))
    (dorg-mutate--source-preserving-heading-line-apply-fields fields)
    (buffer-string)))

(defun dorg-mutate--source-preserving-update-heading-line (patch)
  "Apply source-preserving intrinsic heading PATCH at point.
Each operation recomputes Org-derived spans after the preceding one, so mixed
field updates never reuse shifted positions."
  (let ((fields (plist-get patch :fields)))
    (dorg-mutate--source-preserving-heading-line-apply-fields fields)))

(defconst dorg-mutate--source-preserving-planning-fields
  '(:scheduled :deadline :closed)
  "Planning fields in the stable physical insertion order.")

(defun dorg-mutate--source-preserving-planning-refuse (detail &optional field value)
  "Raise a typed refusal for an unrepresentable plain planning edit."
  (dorg-mutate--raise
   'invalid-target "planning line cannot represent requested field"
   :detail detail :field field :value value))

(defun dorg-mutate--source-preserving-planning-line-state ()
  "Return the validated immediate planning-line state at the current heading.
The line is either absent or entirely a run of uppercase Org planning tokens
and horizontal separators.  Returning byte positions makes the caller's edits
surgical; malformed, duplicate, or unterminated candidates fail before a byte
is changed."
  (org-back-to-heading t)
  (unless (eq (char-before (point-max)) ?\n)
    (dorg-mutate--source-preserving-planning-refuse 'planning-line-unterminated))
  (let ((section-end (save-excursion (outline-next-heading) (point))))
    (forward-line 1)
    (let ((line-begin (point))
          (line-end (line-end-position)))
      (cond
       ((or (>= line-begin section-end) (org-at-heading-p))
        (list :present nil :insert-at line-begin))
       ;; A non-planning first body line is not a planning line.  It remains
       ;; body text and insertion happens before it.
       ((not (save-excursion
               (skip-chars-forward " \t" line-end)
               (let ((case-fold-search t))
                 (looking-at "\\(?:scheduled\\|deadline\\|closed\\):"))))
        (list :present nil :insert-at line-begin))
       (t
        ;; Org's planning element retains only the last occurrence of a
        ;; repeated keyword.  Detect repetition in the physical line first so
        ;; it remains an ambiguity instead of looking like an unparsed token.
        (let ((keywords nil))
          (save-excursion
            (goto-char line-begin)
            (let ((case-fold-search nil))
              (while (re-search-forward "\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" line-end t)
                (let ((field (intern (concat ":" (downcase (match-string-no-properties 1))))) )
                  (when (memq field keywords)
                    (dorg-mutate--source-preserving-planning-refuse
                     'planning-line-ambiguous field))
                  (push field keywords))))))
        (let ((element (org-element-context))
              (fields nil))
          (goto-char line-begin)
          (while (< (point) line-end)
            (skip-chars-forward " \t" line-end)
            (unless (or (= (point) line-end)
                        (looking-at "\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):[ \t]*"))
              (dorg-mutate--source-preserving-planning-refuse
               'planning-line-malformed))
            (unless (= (point) line-end)
              (let* ((token-begin (point))
                     (field (intern (concat ":" (downcase (match-string-no-properties 1)))) )
                     (timestamp (org-element-property field element)))
                (goto-char (match-end 0))
                (let ((raw-end (and timestamp
                                    (+ (point)
                                       (length (org-element-property :raw-value timestamp))))))
                  (unless (and timestamp
                               (= (point) (org-element-property :begin timestamp))
                               (<= raw-end line-end))
                  (dorg-mutate--source-preserving-planning-refuse
                   'planning-line-malformed field))
                  (push (list :field field :token-begin token-begin
                              :value-begin (point) :value-end raw-end
                              :raw (org-element-property :raw-value timestamp))
                        fields)
                  (goto-char raw-end)))))
          (list :present t :line-begin line-begin :line-end line-end
                :line-after (min (point-max) (1+ line-end))
                :fields (nreverse fields))))))))

(defun dorg-mutate--source-preserving-planning-field (state field)
  "Return FIELD's physical planning token from STATE, or nil."
  (cl-find field (plist-get state :fields)
           :key (lambda (entry) (plist-get entry :field))))

(defun dorg-mutate--source-preserving-planning-value (state field)
  "Return FIELD's raw planning value in STATE, or nil."
  (when-let ((entry (dorg-mutate--source-preserving-planning-field state field)))
    (plist-get entry :raw)))

(defun dorg-mutate--source-preserving-planning-assert (fields before)
  "Assert the requested FIELDS and all omitted planning facets after an edit."
  (let ((after
         (condition-case _err
             (dorg-mutate--source-preserving-planning-line-state)
           (dorg-mutate-error
            (let ((field (cl-find-if (lambda (candidate)
                                       (plist-get fields candidate))
                                     dorg-mutate--source-preserving-planning-fields)))
              (dorg-mutate--source-preserving-planning-refuse
               'planning-line-unrepresentable field (plist-get fields field)))))))
    (dolist (field dorg-mutate--source-preserving-planning-fields)
      (let ((expected (if (plist-member fields field)
                          (plist-get fields field)
                        (dorg-mutate--source-preserving-planning-value before field))))
        (unless (equal expected
                       (dorg-mutate--source-preserving-planning-value after field))
          (dorg-mutate--source-preserving-planning-refuse
           'planning-line-unrepresentable field expected))))))

(defun dorg-mutate--source-preserving-planning-assert-value (field value)
  "Refuse requested FIELD VALUE unless it is exactly one Org timestamp.

This is deliberately a candidate-value check, before any splice: malformed is
reserved for existing source bytes, while an unrepresentable request is not a
source defect.  Org decides the timestamp kind; inactive and diary raw forms
are accepted when their raw value is exactly VALUE."
  (when value
    (condition-case _err
        (with-temp-buffer
          (insert "* Probe\n" (upcase (substring (symbol-name field) 1))
                  ": " value "\n")
          (let ((org-inhibit-startup t)) (org-mode))
          (goto-char (point-min))
          (let ((state (dorg-mutate--source-preserving-planning-line-state)))
            (unless (equal value
                           (dorg-mutate--source-preserving-planning-value
                            state field))
              (dorg-mutate--source-preserving-planning-refuse
               'planning-line-unrepresentable field value))))
      (dorg-mutate-error
       (dorg-mutate--source-preserving-planning-refuse
        'planning-line-unrepresentable field value)))))

(defun dorg-mutate--source-preserving-planning-apply-fields (fields)
  "Apply raw planning FIELDS at the current heading by minimal byte splices."
  (dolist (field dorg-mutate--source-preserving-planning-fields)
    (when (plist-member fields field)
      (dorg-mutate--source-preserving-planning-assert-value
       field (plist-get fields field))))
  (let ((before (dorg-mutate--source-preserving-planning-line-state)))
    (dolist (field dorg-mutate--source-preserving-planning-fields)
      (when (plist-member fields field)
        (let* ((value (plist-get fields field))
               (state (dorg-mutate--source-preserving-planning-line-state))
               (entry (dorg-mutate--source-preserving-planning-field state field)))
          (cond
           ((not (plist-get state :present))
            (when value
              (goto-char (plist-get state :insert-at))
              (insert (upcase (substring (symbol-name field) 1)) ": " value "\n")))
           (entry
            (if value
                (replace-region-contents
                 (plist-get entry :value-begin) (plist-get entry :value-end)
                 (lambda () value))
              (let* ((all (plist-get state :fields))
                     (tail (cdr (memq entry all)))
                     (previous (car (last (cl-subseq all 0 (cl-position entry all))))) )
                (cond
                 ((= (length all) 1)
                  (delete-region (plist-get state :line-begin)
                                 (plist-get state :line-after)))
                 (tail
                  ;; First and middle tokens own the whole following run.
                  (delete-region (plist-get entry :token-begin)
                                 (plist-get (car tail) :token-begin)))
                 (t
                  ;; The final token owns the whole preceding run.
                  (delete-region (plist-get previous :value-end)
                                 (plist-get entry :value-end)))))))
           (value
            ;; Append before the existing trailing horizontal whitespace.
            (goto-char (plist-get state :line-end))
            (skip-chars-backward " \t" (plist-get state :line-begin))
            (insert " " (upcase (substring (symbol-name field) 1)) ": " value))))))
    (dorg-mutate--source-preserving-planning-assert fields before)))

(defun dorg-mutate--source-preserving-planning-section-string (marker)
  "Return MARKER's complete heading section for no-write planning simulation."
  (save-excursion
    (goto-char marker)
    (let ((begin (point)) (end (save-excursion (outline-next-heading) (point))))
      (buffer-substring-no-properties begin end))))

(defun dorg-mutate--source-preserving-planning-simulate (section patch)
  "Return SECTION after applying PATCH through the real planning editor."
  (with-temp-buffer
    (insert section)
    (org-mode)
    (goto-char (point-min))
    (dorg-mutate--source-preserving-planning-apply-fields
     (cl-loop for field in dorg-mutate--source-preserving-planning-fields
              when (plist-member patch field)
              append (list field (plist-get patch field))))
    (buffer-string)))

(defun dorg-mutate--source-preserving-update-planning-line (patch)
  "Apply one validated plain-Org planning PATCH at the current heading."
  (dorg-mutate--source-preserving-planning-apply-fields
   (cl-loop for field in dorg-mutate--source-preserving-planning-fields
            when (plist-member patch field)
            append (list field (plist-get patch field)))))

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
     (dorg-mutate--source-preserving-update-heading-line patch)
     nil)
    (`(update :planning)
     (dorg-mutate--source-preserving-update-planning-line patch)
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
          (line-sim (make-hash-table :test 'eql))  ; heading position -> line bytes
          (planning-sim (make-hash-table :test 'eql)) ; heading position -> section bytes
          (heading-grammar (dorg-mutate--source-preserving-heading-line-grammar))
          (pending-identities (make-hash-table :test 'equal))
          (plan nil)
          ;; Resolve the identity policy ONCE (not per patch): only a `dorg' policy
          ;; makes an integer-id miss on an idless source a bootstrap attempt.
          (dorg-p (dorg-mutate--identity-dorg-p source)))
      ;; ── Phase 1: validate + resolve + simulate + guard, no writes ──
      (let ((phase1-context
             (dorg-mutate--item-batch-context
              (dorg-mutate--identity-protected-key source))))
        (dolist (patch patches)
          (dorg-mutate--source-preserving-validate-patch patch)
          (let ((marker (dorg-mutate--source-preserving-resolve
                         (dorg-mutate--source-preserving-patch-host patch)
                         patch dorg-p)))
            ;; Heading-line representability deliberately follows target and
            ;; identity resolution: an identity-none/missing/ambiguous refusal
            ;; remains authoritative, but a later invalid line field prevents
            ;; every patch in this batch from entering phase two.
            (when (equal (dorg-mutate--patch-signature patch) '(update :heading))
              ;; Simulate each heading line in its own temporary buffer.  A
              ;; cached result is the input to a later patch at the same
              ;; marker, so validation covers the whole ordered batch while
              ;; phase one leaves the source buffer byte-for-byte untouched.
              (let* ((key (marker-position marker))
                     (cached (gethash key line-sim 'dorg-mutate--uncached))
                     (line (if (eq cached 'dorg-mutate--uncached)
                               (save-excursion
                                 (goto-char marker)
                                 (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position)))
                             cached)))
                (puthash key
                         (dorg-mutate--source-preserving-heading-line-simulate
                          line (plist-get patch :fields) heading-grammar)
                         line-sim)))
            (when (equal (dorg-mutate--patch-signature patch) '(update :planning))
              ;; Planning uses the same no-write editor as phase two.  Cache
              ;; the complete section because a prior planning splice can
              ;; shift the immediate line while a later patch targets it.
              (let* ((key (marker-position marker))
                     (cached (gethash key planning-sim 'dorg-mutate--uncached))
                     (section (if (eq cached 'dorg-mutate--uncached)
                                  (dorg-mutate--source-preserving-planning-section-string marker)
                                cached)))
                (puthash key
                         (dorg-mutate--source-preserving-planning-simulate section patch)
                         planning-sim)))
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
                (when (memq (cadr (dorg-mutate--patch-signature patch))
                            '(:text :texts :item :items :drawer-lines))
                  (dorg-mutate--validate-item-postimage
                   new cur patch phase1-context))
                (puthash key new sim)))
            ;; Body literals have their own source-to-model mapping proof.  It
            ;; runs in phase one so an unmappable/missing/ambiguous match
            ;; refuses the whole batch before phase two writes anything.
            (when (equal (dorg-mutate--patch-signature patch) '(replace :body))
              ;; Simulate against the per-heading body cache, never the live
              ;; buffer: later body/text actions must observe earlier actions
              ;; while phase one remains byte-for-byte non-mutating.
              (let* ((key (marker-position marker))
                     (cached (gethash key sim 'dorg-mutate--uncached))
                     ;; The first body edit at this heading still maps the
                     ;; live source to a disposable whole-document postimage.
                     ;; Later edits use the ordered simulation; their source
                     ;; bytes do not yet contain the earlier simulated literal.
                     (_postimage
                      (when (eq cached 'dorg-mutate--uncached)
                        (dorg-mutate--source-preserving-body-literal-postimage-check
                         patch marker source)))
                     (cur (if (eq cached 'dorg-mutate--uncached)
                              (mapcar #'car
                                      (nth 2 (dorg-mutate--source-preserving-body-scan
                                              marker source)))
                            cached))
                     (new (dorg-mutate--apply-body-literal-to-items-for-source-plan
                           cur patch phase1-context)))
                (dorg-mutate--source-preserving-identity-guard
                 new marker source patch)
                (puthash key new sim)))
            ;; Property patches carry identity too: a faithful binding's external
            ;; key must not be changed/deleted via set/delete-property.
            (when (eq (cadr (dorg-mutate--patch-signature patch)) :property)
              (dorg-mutate--source-preserving-property-identity-guard
               patch marker source))
            (when (equal (dorg-mutate--patch-signature patch) '(insert :tree))
              (let* ((parent-level (save-excursion
                                     (goto-char marker)
                                     (org-outline-level)))
                     (child-level
                      (let ((org-odd-levels-only
                             (plist-get heading-grammar :odd-levels-only)))
                        (org-get-valid-level parent-level 1))))
                (dorg-mutate--source-preserving-constructor-section-check
                 patch child-level heading-grammar))
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
    (let ((phase2-context
          (dorg-mutate--item-batch-context
           (dorg-mutate--identity-protected-key source)))
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

(defun dorg-mutate--source-preserving-preflight (patches source)
  "Run the no-write faithful source planner for plain-org PATCHES.

Generic planning classifies backend support, but body-literal dry runs must
also validate the actual source mapping and the whole simulated postimage.
This helper deliberately discards the plan: its only effects are typed
refusals, and it never edits a live buffer or a file."
  (let ((buffer (dorg-mutate-source-buffer source))
        (file (dorg-mutate-source-file source)))
    (cond
     ((buffer-live-p buffer)
      (with-current-buffer buffer
        (dorg-mutate--source-preserving-plan patches source)))
     ((and (stringp file) (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((org-inhibit-startup t)) (org-mode))
        (dorg-mutate--source-preserving-plan patches source)))
     (t
      (dorg-mutate--raise 'invalid-target
                          "faithful plain-org source is not readable"
                          :source-kind 'plain-org :file file)))))

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
    ((or :text :texts :items :item :drawer-lines) 'item)
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
;; Saved local plain-Org applies hold DOrg's per-canonical-file gate across
;; fresh read, validation, splice, fingerprint check, rename, and buffer
;; coherence.  External editors that do not use the gate remain subject to
;; the content-fingerprint check immediately before rename.

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
A nil or plain `undecided' CODING resolves to `utf-8-unix', DOrg's on-disk
default.  A detected `undecided-<eol>' preserves its EOL conversion while
choosing UTF-8, so nil never leaks into `encode-coding-string' as a
no-conversion request (which would mis-handle multibyte content)."
  (let ((cs (or coding 'utf-8-unix)))
    (if (memq (coding-system-base cs) '(undecided prefer-utf-8))
        ;; Detection supplies an `undecided-<eol>' coding system for a raw
        ;; existing file.  Preserve that EOL conversion while selecting the
        ;; concrete UTF-8 encoder: otherwise a byte-faithful CRLF source is
        ;; incorrectly re-encoded as LF before the fidelity gate can compare it.
        (coding-system-change-eol-conversion
         'utf-8 (or (coding-system-eol-type cs) 0))
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
  (let* ((detected (or (car (detect-coding-string raw)) 'utf-8-unix))
         (text (decode-coding-string raw detected))
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
          (dorg-mutate--assert-held-plain-file-gate)
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

(defun dorg-mutate--plain-org-index-identity (source heading)
  "Return HEADING's immutable identity value under plain SOURCE.

The ordinary DOrg policy has numeric ids already.  A property-identity source
has no fabricated DORG_ID, so its external property is the stable key used
only to bind this bounded candidate context together."
  (pcase (dorg-mutate--source-identity source)
    ('dorg (dorg-heading-id heading))
    (`(:property ,key)
     (cdr (assoc-string key
                        (dorg-model-heading-property-snapshot heading) t)))))

(defun dorg-mutate--plain-org-index-id-map (source before after)
  "Return SOURCE's candidate-local numeric map for property identities.

`dorg-ext-index-context' entries carry numeric ids.  For a property-bound
plain source these are opaque, deterministic keys local to this simulation;
the source's declared external identity remains in every heading snapshot.
Duplicate property identities are not representable as one immutable entry,
so refuse instead of silently collapsing one candidate heading into another."
  (when (pcase (dorg-mutate--source-identity source)
          (`(:property ,_) t))
    (let (values)
      (dolist (parsed (list before after))
        (let (image-values)
          (dolist (heading (dorg-parse-result-headings parsed))
            (when-let ((value (dorg-mutate--plain-org-index-identity source heading)))
              (push value image-values)))
          (let ((unique-image
                 (delete-dups (sort (copy-sequence image-values) #'string<))))
            (unless (= (length image-values) (length unique-image))
              (dorg-mutate--raise
               'invalid-target "property identity is ambiguous in candidate"
               :detail 'ambiguous :identity (dorg-mutate--source-identity source)))
            (setq values (nconc unique-image values)))))
      (let ((unique (delete-dups (sort values #'string<))))
        (let ((map (make-hash-table :test 'equal))
              (next 1))
          (dolist (value unique map)
            (puthash value next map)
            (setq next (1+ next))))))))

(defun dorg-mutate--plain-org-index-image (source parsed id-map)
  "Return plain SOURCE's immutable image from already PARSED candidate bytes.

ID-MAP is required only for a property identity; it maps the real external
identity to a candidate-local numeric entry id without adding a DORG_ID to the
source or exposing a synthetic id as its public identity."
  (let ((image (make-hash-table :test 'eql)))
    (dorg-parse-assert-unique-ids parsed)
    (dolist (heading (dorg-parse-result-headings parsed))
      (when-let* ((identity (dorg-mutate--plain-org-index-identity source heading))
                  (id (if id-map (gethash identity id-map) identity)))
        (puthash id (dorg-heading-deep-copy heading) image)))
    image))

(defun dorg-mutate--plain-org-index-touched-ids (source hosts id-map)
  "Map source-preserving HOSTS to immutable candidate ids for SOURCE."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (host)
            (pcase (dorg-mutate--source-identity source)
              ('dorg (and (integerp host) host))
              (`(:property ,key)
               (and (dorg-mutate--typed-external-address-p host)
                    (equal key (plist-get (cdr host) :key))
                    (gethash (plist-get (cdr host) :value) id-map)))))
          hosts))))

(defun dorg-mutate--plain-org-index-context-from-parsed
    (source patches base-s0 before-parsed after-parsed created changed)
  "Build immutable owner context from plain before/after parse results.

CREATED and CHANGED are the physical result of the one authoritative source
plan.  Keeping image construction separate from planning lets apply validate
the already-spliced candidate rather than invoking the splice seam a second
time."
  (let* ((id-map (dorg-mutate--plain-org-index-id-map
                  source before-parsed after-parsed))
         (before (dorg-mutate--plain-org-index-image source before-parsed id-map))
         (after (dorg-mutate--plain-org-index-image source after-parsed id-map))
         (parts (dorg-mutate--index-classify
                 before after
                 (dorg-mutate--plain-org-index-touched-ids source created id-map)
                 (dorg-mutate--plain-org-index-touched-ids source changed id-map)
                 nil nil)))
    (dorg-ext-index-context-create
     :source-kind 'plain-org
     :root-id (dorg-mutate-source-root-id source)
     :file (dorg-mutate-source-file source)
     :base-commit base-s0
     :patches patches
     :created (plist-get parts :created)
     :changed (plist-get parts :changed)
     :moved (plist-get parts :moved)
     :deleted (plist-get parts :deleted)
     :base-image (dorg-mutate--index-host-safe-image before)
     :candidate-postimage (dorg-mutate--index-host-safe-image after))))

(defun dorg-mutate--plain-org-parse-text (text)
  "Parse plain Org TEXT without borrowing the caller's live source buffer."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t)) (org-mode))
    (dorg-parse-buffer (current-buffer) nil)))

(defun dorg-mutate--plain-org-planned-index-context (source patches base-s0)
  "Simulate plain PATCHES into the immutable owner-validation context.

This is the non-mutating planning path.  Apply builds the same context from
its one authoritative splice instead, avoiding a second trip through the
public splice seam."
  (with-temp-buffer
    (insert (dorg-mutate--plain-org-source-text source))
    (let ((org-inhibit-startup t)) (org-mode))
    (widen)
    (let* ((proxy (dorg-mutate--source-make
                   :kind 'plain-org :buffer (current-buffer)
                   :file (dorg-mutate-source-file source)
                   :root-id (dorg-mutate-source-root-id source)
                   :identity (dorg-mutate-source-identity source)))
           (before-parsed (dorg-parse-buffer (current-buffer) nil))
           (run (dorg-mutate--source-preserving-run patches proxy))
           (after-parsed (dorg-parse-buffer (current-buffer) nil)))
      (dorg-mutate--plain-org-index-context-from-parsed
       source patches base-s0 before-parsed after-parsed
       (plist-get run :created) (plist-get run :changed)))))

(defun dorg-mutate--plain-org-applied-index-context
    (source patches base-s0 before-text created changed)
  "Build owner context after one faithful splice, before its persistence.

BEFORE-TEXT is the exact S0 text whose plan has just changed in the current
buffer.  A validator refusal is still precommit: the caller restores the live
buffer or discards the temporary file buffer before re-signalling."
  (let ((before-parsed (dorg-mutate--plain-org-parse-text before-text)))
    (save-restriction
      (widen)
      (dorg-mutate--plain-org-index-context-from-parsed
       source patches base-s0 before-parsed
       (dorg-parse-buffer (current-buffer) nil) created changed))))

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
            (dorg-ext-run-index-validators index-context 'plain-prewrite :all)
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
                      ;; Validate the immutable owner view built from this
                      ;; single physical splice.  A refusal remains precommit
                      ;; and therefore falls through the snapshot restore.
                      (dorg-mutate--validate-owned-metadata-context
                       (dorg-mutate--plain-org-applied-index-context
                        source patches base-s0 snapshot created changed))
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
          ;; This temporary buffer contains the one authoritative splice.
          ;; Owner validation precedes encoding and the atomic rename, so a
          ;; refusal leaves the destination untouched.
          (dorg-mutate--validate-owned-metadata-context
           (dorg-mutate--plain-org-applied-index-context
            source patches base-s0 text created changed))
          ;; Construct the projection context BEFORE any persistence (the commit
          ;; below), so it is a valid precommit projector input (tracked-case).
          (setq index-context
                (dorg-mutate--plain-org-index-context
                 source patches base-s0 created changed))
          (dorg-ext-run-index-validators index-context 'plain-prewrite :all)
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
    (let ((hash (make-hash-table :test 'eql))
          (fact-offsets (make-hash-table :test 'eql)))
      (maphash (lambda (id h) (puthash id (dorg-heading-deep-copy h) hash)) orig)
      (cons orig
            (dorg-mutate--ps-create
             :headings hash :root-id root-id :orig orig
             :fact-offsets fact-offsets
             ;; A sparse load answers order questions with keys-only facts
             ;; read at the same AS-OF through the indexed sibling order; a
             ;; whole-root load is complete and needs none.  The same read
             ;; records each reported sibling's stored offset.
             :order-facts-fn
             (and ids
                  (lambda (parent-id &optional bound side limit exclude-ids)
                    (dorg-mutate--dolt-order-facts
                     dolt-dir root-id parent-id as-of
                     bound side limit exclude-ids fact-offsets))))))))

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

(defun dorg-mutate--dolt-order-facts (dolt-dir root-id parent-id &optional as-of
                                                bound side limit exclude-ids
                                                offsets-sink)
  "Return (ID . SORT-KEY) facts for PARENT-ID's children in ROOT-ID at AS-OF.
Keys only, never content.  Without BOUND, SIDE and LIMIT this is the complete
ascending sibling order.  With SIDE `before' (keys strictly below BOUND,
greatest first) or `after' (keys strictly above, smallest first), at most
LIMIT rows nearest the bound come back; a nil BOUND means the fan end on that
side.  EXCLUDE-IDS are left out by id, so a candidate key is a valid bound
against the remaining unchanged rows.  The predicate and ordering match the
composite sibling index (parent_id, sort_key).  OFFSETS-SINK, when a hash, also
receives each returned id's stored level offset from the same statement, so a
sparse candidate can check representability without another read."
  (let* ((desc (eq side 'before))
         (sql (concat
               (format "SELECT id, HEX(sort_key) AS sort_key_hex, level_offset FROM headings%s WHERE root_id = %d AND %s"
                       (dorg-sql-gen--as-of-suffix as-of)
                       root-id
                       (dorg-mutate--dolt-parent-predicate parent-id))
               (when (and bound side)
                 (format " AND sort_key %s %s" (if desc "<" ">")
                         (dorg-sql-gen--sort-key-hex bound)))
               (when exclude-ids
                 (format " AND id NOT IN (%s)"
                         (mapconcat #'number-to-string exclude-ids ", ")))
               (if desc
                   " ORDER BY sort_key DESC, id DESC"
                 " ORDER BY sort_key, id")
               (when limit (format " LIMIT %d" limit)))))
    (mapcar
     (lambda (row)
       (when offsets-sink
         (puthash (alist-get 'id row) (alist-get 'level_offset row) offsets-sink))
       (cons (alist-get 'id row)
             (dorg-sortkey-from-hex (alist-get 'sort_key_hex row))))
     (dorg-sql dolt-dir sql))))

(defun dorg-mutate--dolt-child-ids (dolt-dir root-id parent-id &optional as-of)
  "Return direct child ids under PARENT-ID in ROOT-ID at AS-OF."
  (mapcar #'car (dorg-mutate--dolt-order-facts dolt-dir root-id parent-id as-of)))

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
     ((memq kind '(:property :keyword :keywords :planning :text :texts :items :item
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
subtrees/children, anchors and current parents.  A parent's sibling fan is
never hydrated: placement reads its complete order as keys only, once per
batch (`dorg-mutate--ps-neighbours')."
  (let ((dolt-dir (dorg-mutate-source-dolt-dir source))
        (ids nil))
    (cl-labels
        ((add (id) (when (integerp id) (cl-pushnew id ids :test #'eql)))
         (resolve (host patch)
           (dorg-mutate--dolt-resolve-host-id source root-id host patch head))
         (add-host (host patch)
           (add (resolve host patch))))
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
             (add before)
             (add after)))
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
             ;; Both delete forms need the deleted heading and every descendant:
             ;; a heading delete promotes its direct children and shifts each
             ;; loaded subtree through the existing planner.  One recursive
             ;; snapshot read is therefore the bounded shared scope.
             (dolist (sid (dorg-mutate--dolt-subtree-ids
                           dolt-dir root-id id head))
               (add sid))))))
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

(defun dorg-mutate--ps-rekey-ops (ps orig)
  "Return (:rekey PARENT-ID ((ID . SORT-KEY) ...)) ops for PS's pending rekeys.
One op per parent, ids ascending.  A row this batch created, changed or
deleted carries its key in its own row op and is left out; so is a row whose
pending key equals its ORIG key."
  (let ((touched (make-hash-table :test 'eql))
        (by-parent nil))
    (dolist (id (append (dorg-mutate--ps-created ps)
                        (dorg-mutate--ps-changed ps)
                        (dorg-mutate--ps-deleted ps)))
      (puthash id t touched))
    (maphash
     (lambda (id cell)
       (let ((o (gethash id orig)))
         (unless (or (gethash id touched)
                     (and o (equal (dorg-heading-sort-key o) (cdr cell))))
           (push (cons id (cdr cell))
                 (alist-get (car cell) by-parent nil nil #'eql)))))
     (dorg-mutate--ps-rekeys ps))
    (mapcar (lambda (entry)
              (list :rekey (car entry)
                    (sort (cdr entry) (lambda (a b) (< (car a) (car b))))))
            (sort by-parent (lambda (a b) (< (or (car a) 0) (or (car b) 0)))))))

(defun dorg-mutate--dolt-assemble-sql (db-ops orig id-gen)
  "Lower tagged DB-OPS to SQL in FK-safe phases.
Phases: INSERTS (new parent rows exist before an update reparents a child
under them), UPDATES including key-only `:rekey' ops, then DELETES.  Every key
any op writes is fresh for its parent (`dorg-mutate--ps-allocate-keys'), so no
phase order can violate the sibling unique index and no temporary keys exist.
ORIG is the snapshot hash (for tag/item diffing); ID-GEN allocates
`heading_items' ids."
  (let (inserts updates deletes)
    (dolist (op db-ops)
      (pcase (car op)
        (:insert-heading (push op inserts))
        ((or :update-heading :rekey) (push op updates))
        (:delete-heading (push op deletes))))
    (append
     (dorg-sql-gen-lower-heading-ops (nreverse inserts) orig id-gen)
     (dorg-sql-gen-lower-heading-ops (nreverse updates) orig id-gen)
     (dorg-sql-gen-lower-heading-ops (nreverse deletes) orig id-gen))))

;; Accumulator effects are folded at plan time against derived rows read AS
;; OF the plan base.  The commit gate lands an attempt only while the in-gate
;; HEAD still equals that base, and otherwise replans with fresh priors, so a
;; folded literal row is exactly the base row plus the plan's own deltas.
;; Priors are never read from the live head or the working set.

(defun dorg-mutate--accumulator-request-sql (table keys)
  "Return one keyed SELECT of TABLE's rows for KEYS (column alists).
Keys sharing every column but the last are grouped into one IN list."
  (let (groups)
    (dolist (key keys)
      (let ((prefix (butlast key)) (last (car (last key))))
        (push (cdr last) (alist-get (cons prefix (car last)) groups nil nil #'equal))))
    (format "SELECT * FROM %s WHERE %s" table
            (mapconcat
             (lambda (group)
               (let* ((prefix (car (car group))) (column (cdr (car group)))
                      (values (reverse (cdr group))))
                 (format "(%s)"
                         (mapconcat
                          #'identity
                          (append (mapcar (lambda (cell)
                                            (format "%s = %s" (car cell)
                                                    (dorg-sql--literal-sql (cdr cell))))
                                          prefix)
                                  (list (format "%s IN (%s)" column
                                                (mapconcat #'dorg-sql--literal-sql
                                                           values ", "))))
                          " AND "))))
             (reverse groups) " OR "))))

(defun dorg-mutate--read-accumulator-priors (dolt-dir revision requests)
  "Return (TABLE . ROW) priors for REQUESTS in DOLT-DIR AS OF REVISION.
REQUESTS come from `dorg-ext-accumulator-prior-requests'; each table is one
keyed revision read."
  (unless (and (stringp revision) (not (string-empty-p revision)))
    (dorg-mutate--raise 'unsupported
                        "accumulator priors need an exact plan-base revision"))
  (let (priors)
    (dolist (request requests)
      (dolist (row (dorg-sql--at-revision
                    dolt-dir revision
                    (dorg-mutate--accumulator-request-sql (car request) (cdr request))))
        (push (cons (car request) row) priors)))
    (nreverse priors)))

(defun dorg-mutate--roots-empty-at-base (dolt-dir revision roots)
  "Return the ROOTS that had no non-root heading in DOLT-DIR AS OF REVISION."
  (cl-remove-if
   (lambda (root)
     (dorg-sql--at-revision
      dolt-dir revision
      (format "SELECT 1 AS present FROM headings WHERE root_id = %d AND id <> %d LIMIT 1"
              root root)))
   roots))

(defun dorg-mutate--accumulator-fold-port (dolt-dir revision)
  "Return the fold port binding accumulator priors to plan base REVISION."
  (list :priors (lambda (requests)
                  (dorg-mutate--read-accumulator-priors dolt-dir revision requests))
        :empty-roots (lambda (roots)
                       (dorg-mutate--roots-empty-at-base dolt-dir revision roots))))

(defun dorg-mutate--dolt-extension-installed-p (dolt-dir extension)
  "Return non-nil when EXTENSION is installed in DOLT-DIR's metadata.

The extension registry is process-global while schema installation is per
store.  This small pre-simulation metadata read prevents a registered aggregate
projector from widening or participating in an unrelated store."
  (let ((key (format "ext:%s:version" (dorg-extension-name extension))))
    (car (dorg-sql dolt-dir
                   (format "SELECT value FROM dorg_meta WHERE `key` = %s"
                           (dorg-sql-quote key))))))

(defun dorg-mutate--dolt-installed-index-extension-names (dolt-dir)
  "Return registered projectors installed in DOLT-DIR for Dolt mutation."
  (delq nil
        (mapcar (lambda (extension)
                  (and (dorg-extension-project extension)
                       (memq 'dolt (dorg-ext--effective-capabilities extension))
                       (dorg-mutate--dolt-extension-installed-p dolt-dir extension)
                       (dorg-extension-name extension)))
                (dorg-registered-extensions))))

(defun dorg-mutate--dolt-simulate (source patches _options &optional owner-plan)
  "Apply typed PATCHES to a fresh SOURCE snapshot and lower row diffs to SQL.
Returns a plist (:head-commit HEAD :scope-ids SCOPE :db-ops DB-OPS :sql SQL
:ps PS :orig ORIG :created IDS :changed IDS :deleted IDS).  Any missing-target /
non-sibling anchor / move-under-self error signals HERE, in memory, before
any write.  Placement never widens the load: a gap without room rekeys a
window of the parent's key facts (keys only), lowered as `:rekey' ops.
The owner request comes only from a verified private capability."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (root-id (dorg-mutate-source-root-id source))
         (owner-request (dorg-mutate--planning-owner-request owner-plan))
         (head (dorg-head-commit dolt-dir))
         (resolved-patches (dorg-mutate--dolt-resolve-patches-externs
                            source root-id patches head))
         (bounded-scope-ids
          (unless (eq (plist-get owner-request :action) 'restore)
            (delete-dups
             (append
              (dorg-mutate--dolt-scope-ids
               source resolved-patches root-id head)
              (when owner-request
                (list (string-to-number
                       (plist-get owner-request :scope-id))))))))
         ;; Most projectors receive the normal bounded candidate image.  A
         ;; registered root-aggregate projector (BM25 df/N, for example)
         ;; declares that requirement before simulation, so DOrg loads the
         ;; complete root itself rather than letting a projector read a store.
         (scope-ids (unless (or (eq (plist-get owner-request :action) 'restore)
                                (dorg-ext-index-projector-requires-whole-root-p
                                 'dolt
                                 (lambda (extension)
                                   (dorg-mutate--dolt-extension-installed-p
                                    dolt-dir extension))))
                      bounded-scope-ids))
         (loaded (dorg-mutate--dolt-load source head scope-ids))
         (orig (car loaded))
         (ps (cdr loaded))
         (batch-context (dorg-mutate--item-batch-context
                         (dorg-mutate--identity-protected-key source))))
    (dolist (patch resolved-patches)
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (let* ((db-ops (append (dorg-mutate--dolt-db-ops orig ps root-id)
                           (dorg-mutate--ps-rekey-ops ps orig)))
           (sql (dorg-mutate--dolt-assemble-sql
                 db-ops orig dorg-mutate--dolt-item-id-generator))
           (sim (list
                 :head-commit head
                 :scope-ids
                 (and scope-ids
                      (sort (delete-dups (copy-sequence scope-ids)) #'<))
                 :patches resolved-patches
                 :db-ops db-ops
                 :sql sql
                 :ps ps
                 :orig orig
                 :created (nreverse (delete-dups (dorg-mutate--ps-created ps)))
                 :changed (nreverse (delete-dups (dorg-mutate--ps-changed ps)))
                 :deleted (nreverse (delete-dups (dorg-mutate--ps-deleted ps)))
                 ;; Ancestry this batch changed (moved subtrees and promoted
                 ;; children's subtrees): their stored offsets may be
                 ;; unchanged, so index classification takes them from here.
                 :moved (let (ids)
                          (maphash (lambda (id _) (push id ids))
                                   (dorg-mutate--ps-moved ps))
                          (sort ids #'<)))))
      ;; Expose the immutable post-mutation projection context built from
      ;; this simulation, after core simulation and before persistence
      ;; (tracked-case).
      (nconc sim
             (list :index-context
                   (dorg-mutate--dolt-index-context
                    source resolved-patches sim head owner-request))))))

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

(defun dorg-mutate--index-classify (orig ps-headings created changed deleted
                                          &optional moved)
  "Partition a mutation into create/change/move/delete `dorg-ext-index-entry's.
MOVED lists ids whose ancestry changed; see `dorg-ext--index-classify'."
  (dorg-ext--index-classify orig ps-headings created changed deleted moved))

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

(defun dorg-mutate--index-image-children (image)
  "Return IMAGE parent id -> child id lists without loading or copying rows."
  (let ((children (make-hash-table :test 'eql)))
    (maphash
     (lambda (id heading)
       (when-let ((parent-id (dorg-heading-parent-id heading)))
         (push id (gethash parent-id children))))
     image)
    children))

(defun dorg-mutate--index-image-owner-local-ids (source image children root-id)
  "Return ROOT-ID and its owner-local descendants already in IMAGE.
Property identity traversal is transitive but stops before a descendant carrying
the identity property: that heading owns its own partition and neither it nor
its descendants belong in the outer partition.  Under `dorg' identity every
heading is its own owner, so the result is ROOT-ID alone.  IMAGE is an
already-materialized simulation image; this helper never loads headings or
queries the store."
  (if (eq (dorg-mutate--source-identity source) 'dorg)
      (and (gethash root-id image) (list root-id))
    (let ((owner-key (dorg-mutate--index-owner-key source))
          (seen (make-hash-table :test 'eql))
          (queue (list root-id))
          ids)
      (while queue
        (let ((id (pop queue)))
          (unless (gethash id seen)
            (puthash id t seen)
            (when (gethash id image)
              (push id ids)
              (dolist (child-id (gethash id children))
                (let ((child (gethash child-id image)))
                  (unless (and child
                               (assoc owner-key
                                      (dorg-model-heading-property-snapshot child)))
                    (push child-id queue))))))))
      (sort ids #'<))))

(defun dorg-mutate--index-overlay-owner-candidate
    (image source owner base-image base-children candidate-image candidate-children)
  "Overlay OWNER's already-resolved entry sides onto owner-local IMAGE.
Membership was resolved against the complete before/after images while
collecting OWNER's affected paths.  Resolving it again against each partial,
changing partition both repeats unrelated SQL reads and can misclassify a moved
descendant.  BASE-IMAGE and CANDIDATE-IMAGE are complete already-materialized
simulation images; BASE-CHILDREN and CANDIDATE-CHILDREN index each image once.
SOURCE supplies the identity boundary for each closure."
  (let ((paths (plist-get owner :affected-paths)))
    ;; Remove the old membership before installing any new membership; order
    ;; of hash traversal and ancestor/descendant entries must not affect this.
    (dolist (path paths)
      (when (eq 'before (plist-get path :side))
        (dolist (id (dorg-mutate--index-image-owner-local-ids
                     source base-image base-children (plist-get path :id)))
          (remhash id image))))
    (dolist (path paths)
      (when (eq 'after (plist-get path :side))
        (dolist (id (dorg-mutate--index-image-owner-local-ids
                     source candidate-image candidate-children (plist-get path :id)))
          (when-let ((after (gethash id candidate-image)))
            (puthash id (dorg-heading-deep-copy after) image)))))))

(defun dorg-mutate--index-owner-partitions
    (source root-id parts base-image candidate-postimage base
            &optional sides rekeys)
  "Return affected owner partitions for SOURCE/ROOT-ID and index PARTS.
REKEYS, when non-nil, is the batch's pending rekey table (id -> (PARENT-ID .
SORT-KEY)); an owner-local row hydrated from the store carries its final key
in the candidate image, so projections and the readback assert see the
candidate order, not the base order."
  (let ((sides (or sides '(before after)))
        (owner-key (dorg-mutate--index-owner-key source))
        (owners (make-hash-table :test 'equal))
        (entries-by-id (make-hash-table :test 'eql))
        (entries (dorg-mutate--index-all-entries parts)))
    (when owner-key
      (dolist (entry entries)
        (puthash (dorg-ext-index-entry-id entry) entry entries-by-id)
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
    (let ((base-children (and (memq 'before sides)
                              (dorg-mutate--index-image-children base-image)))
          (candidate-children (and (memq 'after sides)
                                   (dorg-mutate--index-image-children
                                    candidate-postimage)))
          partitions)
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
            candidate-owner-image source owner
            base-image base-children candidate-postimage candidate-children)
           ;; Pending rekeys reach rows the semantic entries did not touch;
           ;; an entry's final key always wins over an earlier rekey.
           (when rekeys
             (maphash (lambda (id cell)
                        (unless (gethash id entries-by-id)
                          (when-let ((h (gethash id candidate-owner-image)))
                            (setf (dorg-heading-sort-key h) (cdr cell)))))
                      rekeys))
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

(defun dorg-mutate--dolt-index-context (source patches sim base &optional owner-request)
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
                 (plist-get sim :deleted)
                 (plist-get sim :moved)))
         (owner-partitions
          (dorg-mutate--index-owner-partitions
           source (dorg-mutate-source-root-id source)
           parts base-image candidate-postimage base nil
           (and ps (dorg-mutate--ps-rekeys ps)))))
    (let ((context (dorg-ext-index-context-create
     :source-kind 'dolt
     :root-id (dorg-mutate-source-root-id source)
     :dolt-dir (dorg-mutate-source-dolt-dir source)
     :file (dorg-mutate-source-file source)
     :base-commit base
     :owner-request owner-request
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
      (dorg-mutate--assert-structural-owned-metadata-available context)
      (dorg-mutate--validate-owned-metadata-context context)
      context)))

(defun dorg-mutate--structural-patch-p (patch)
  "Non-nil when PATCH can remove or relocate existing heading membership."
  (member (dorg-mutate--patch-signature patch)
          '((delete :heading) (delete :subtree) (move :subtree))))

(defun dorg-mutate--heading-owned-metadata-families (heading)
  "Return registered metadata prefixes stored by HEADING's drawer properties."
  (let (families)
    (dolist (cell (dorg-model-heading-property-snapshot heading))
      (when-let ((family (dorg-owned-metadata-family (car cell))))
        (cl-pushnew (car family) families :test #'equal)))
    (sort families #'string<)))

(defun dorg-mutate--index-context-owned-metadata-families (context)
  "Return every registered metadata family touched by immutable CONTEXT.

Both sides of every classified entry participate.  A removal therefore remains
visible through its S0 tombstone, and an ordinary edit to an already-managed
heading cannot bypass its owner merely because the request did not name a
family key."
  (let (families)
    (dolist (entry
             (append (dorg-ext-index-context-created context)
                     (dorg-ext-index-context-changed context)
                     (dorg-ext-index-context-moved context)
                     (dorg-ext-index-context-deleted context)))
      (dolist (heading (list (dorg-ext-index-entry-before entry)
                             (dorg-ext-index-entry-after entry)))
        (when heading
          (dolist (family
                   (dorg-mutate--heading-owned-metadata-families heading))
            (cl-pushnew family families :test #'equal)))))
    (sort families #'string<)))

(defun dorg-mutate--validate-owned-metadata-context (context)
  "Run each touched metadata owner's validator once against CONTEXT.

The owner sees only DOrg's sealed immutable index context.  An absent owner or
validator fails closed as `owner-unavailable'; any owner signal is normalized
to the public `owner-refused' diagnostic without retaining a condition object
or exposing a mutable simulation handle."
  (unless (and (hash-table-p (dorg-ext-index-context-base-image context))
               (hash-table-p
                (dorg-ext-index-context-candidate-postimage context)))
    (dorg-mutate--raise
     'invalid-target "owned metadata validation requires both immutable images"
     :detail 'owner-context-images-missing))
  (dolist (family (dorg-mutate--index-context-owned-metadata-families context))
    (let* ((participants
            (condition-case err
                (dorg-owned-metadata-participants family)
              (error
               (apply #'dorg-mutate--raise
                      'invalid-target "owned metadata owner refused the candidate"
                      :family family
                      (dorg-owned-metadata-normalize-owner-signal
                       family nil err)))))
           (validate (and (listp participants)
                          (plist-get participants :validate))))
      (unless (functionp validate)
        (dorg-mutate--raise 'invalid-target
                            "owned metadata owner is unavailable"
                            :family family :detail 'owner-unavailable))
      (condition-case err
          (funcall validate context)
        (error
         (apply #'dorg-mutate--raise
                'invalid-target "owned metadata owner refused the candidate"
                :family family
                (dorg-owned-metadata-normalize-owner-signal
                 family participants err)))))))

(defun dorg-mutate--assert-structural-owned-metadata-available (context)
  "Refuse structural CONTEXT changes involving an unavailable metadata owner.

Only classified entries are inspected.  The simulator may load an unchanged
ancestor for placement, but that read must not turn an unrelated child edit
into a metadata operation.  Before and candidate entry sides cover deletion,
promotion, and same-store reparenting without consulting a mutable live view."
  (when (cl-some #'dorg-mutate--structural-patch-p
                 (dorg-ext-index-context-patches context))
    (let (families)
      (dolist (entry
               (append (dorg-ext-index-context-created context)
                       (dorg-ext-index-context-changed context)
                       (dorg-ext-index-context-moved context)
                       (dorg-ext-index-context-deleted context)))
        (dolist (heading (list (dorg-ext-index-entry-before entry)
                               (dorg-ext-index-entry-after entry)))
          (when heading
            (dolist (family
                     (dorg-mutate--heading-owned-metadata-families heading))
              (cl-pushnew family families :test #'equal)))))
      (when families
        (setq families (sort families #'string<))
        (dorg-mutate--raise
         'invalid-target
         "structural mutation requires the registered metadata owner"
         :operation 'structural :detail 'owner-unavailable
         :family (car families) :families families)))))

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
              base '(after)
              (and dst-ps (dorg-mutate--ps-rekeys dst-ps)))))))
    (let ((context (dorg-ext-index-context-create
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
      (dorg-mutate--assert-structural-owned-metadata-available context)
      (dorg-mutate--validate-owned-metadata-context context)
      context)))

;;; ─── Full pre-commit transaction proof ────────────────────────────────────
;; The commit is guarded by an in-transaction proof in the existing typed
;; transaction-read validate window
;; (post-statements, pre-DOLT_COMMIT):
;;   assert (1) readback core rows == the candidate canonical, and
;;   assert (2) readback extension rows == the validated typed effects.
;; Both abort pre-commit (rollback, zero durable writes).  Effects are
;; project(candidate), so (1) makes (2) transitively rows == project(core S1)
;; — the generic parity invariant — while each leg compares READBACK against
;; an independent pre-commit value (neither is a recomputation).  Full owner
;; parity and persisted item identity are checked independently of the compact
;; operation receipt; no derived per-owner evidence is persisted.

(defun dorg-mutate--postimage-expectations (context partitions)
  "Return (EXPECTED-HT . ABSENT-IDS) for CONTEXT and its PARTITIONS.
EXPECTED-HT maps each heading id the transaction proof must find to its
candidate canonical tuple; ABSENT-IDS are the ids the apply deletes, whose
rows must be GONE at readback.  Coverage is the union of every partition's
candidate image (so every owner's rows sit inside assert (1)) and
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

(defun dorg-mutate--postimage-scope-index (entries table columns)
  "Return a fresh scope index of the live ENTRIES of TABLE for COLUMNS.
An index maps the values of COLUMNS, in that order, to the entries whose
rows hold every one of those columns; see `dorg-mutate--postimage-row-checks'."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (when (and (aref entry 2) (equal (aref entry 0) table))
        (dorg-mutate--postimage-scope-add index columns entry)))
    index))

(defun dorg-mutate--postimage-scope-add (index columns entry)
  "Add ENTRY to scope INDEX over COLUMNS when its row holds every column."
  (let* ((row (aref entry 1))
         (cells (mapcar (lambda (column) (assq column row)) columns)))
    (unless (memq nil cells)
      (push entry (gethash (mapcar #'cdr cells) index)))))

(defun dorg-mutate--postimage-row-checks (effects)
  "Return net-effect readback checks for the validated typed EFFECTS.
Walks EFFECTS in execution order per table, maintaining the surviving insert
rows (an insert is removed when a LATER delete predicate matches it), so the
checks describe the transaction's FINAL expected state, not any transient
one — a projector's delete-then-rebuild pattern nets to its inserts.
Returns (:present ((TABLE ROW COUNT)…) :absent ((TABLE WHERE COUNT)…)):
each surviving ROW must match at least COUNT readback rows; each delete
WHERE scope must hold exactly COUNT readback rows (its surviving matching
inserts).

The walk is linear in EFFECTS times the number of distinct delete column
lists per table.  Each insert becomes an entry [TABLE ROW LIVE].  A delete's
column list, in its own order, names a scope index from those columns'
values to the entries holding all of them, built from the live entries the
first time that column list appears and extended by every later insert.  A
delete kills the live entries of its bucket and empties it, so a bucket
holds exactly the live entries matching its predicate that arrived after
its last delete.  Every candidate is still confirmed with
`dorg-mutate--postimage-row-matches-p', the one definition of a match."
  (let ((entries nil)
        (scopes (make-hash-table :test 'equal))
        (deletes nil))
    (dolist (effect effects)
      (let ((table (plist-get effect :table)))
        (cond
         ((plist-member effect :insert)
          (let ((entry (vector table (plist-get effect :insert) t)))
            (push entry entries)
            (maphash (lambda (scope index)
                       (when (equal (car scope) table)
                         (dorg-mutate--postimage-scope-add index (cdr scope) entry)))
                     scopes)))
         ((plist-member effect :delete)
          (let* ((where (plist-get effect :delete))
                 (scope (cons table (mapcar #'car where)))
                 (index (or (gethash scope scopes)
                            (puthash scope (dorg-mutate--postimage-scope-index
                                            entries table (cdr scope))
                                     scopes)))
                 (key (mapcar #'cdr where)))
            (dolist (entry (gethash key index))
              (when (and (aref entry 2)
                         (dorg-mutate--postimage-row-matches-p (aref entry 1) where))
                (aset entry 2 nil)))
            (remhash key index)
            (push (list table where scope key) deletes))))))
    (let ((survivors (cl-remove-if-not (lambda (entry) (aref entry 2))
                                       (nreverse entries)))
          (counts (make-hash-table :test 'equal))
          (absent-counts (make-hash-table :test 'equal))
          present)
      (dolist (entry survivors)
        (let* ((identity (cons (aref entry 0) (aref entry 1)))
               (hit (gethash identity counts)))
          (if hit
              (cl-incf (nth 2 hit))
            (let ((check (list (aref entry 0) (aref entry 1) 1)))
              (puthash identity check counts)
              (push check present)))))
      (list :present (nreverse present)
            :absent
            (mapcar (pcase-lambda (`(,table ,where ,scope ,key))
                      (list table where
                            (let ((memo (cons scope key)))
                              (or (gethash memo absent-counts)
                                  (puthash memo
                                           (cl-count-if
                                            (lambda (entry)
                                              (and (aref entry 2)
                                                   (dorg-mutate--postimage-row-matches-p
                                                    (aref entry 1) where)))
                                            (gethash key (gethash scope scopes)))
                                           absent-counts)))))
                    (nreverse deletes))))))

(defun dorg-mutate--postimage-read-batched (read)
  "Return a read function over READ that submits oversized id sets in chunks.
The transaction read port refuses any single request whose `:ids' exceed
`dorg-sql-transaction-read-port-max-ids'; that cap is transport policy and
stays.  A complete candidate proof can legitimately name more ids than the
cap (a bootstrap import, a large delete), so this wrapper, applied once at the
proof hand-off, splits ONLY the `:ids' of the four id-bearing ops into
consecutive chunks of at most the cap, issues the same request with each
chunk inside the caller's transaction and snapshot, and merges the results
by the op's shape: row ops keep input order; `owner-model' folds every
chunk's image into one table.  No request grammar lives here: a request the
port would refuse still reaches the port, either untouched (any other op, an
empty or non-integer id set) or as its first chunk, so the port's own
refusal governs and nothing becomes accepted by chunking.  Linear in the id
count."
  (lambda (request)
    (let ((op (plist-get request :op))
          (ids (plist-get request :ids))
          (cap dorg-sql-transaction-read-port-max-ids))
      (if (and (memq op '(core-heading-rows core-heading-tags
                          core-heading-ids owner-model))
               (consp ids)
               (> (length ids) cap)
               (cl-every (lambda (id) (and (integerp id) (> id 0))) ids))
          (let ((rest ids)
                (results nil))
            (while rest
              (let ((chunk nil)
                    (k 0))
                (while (and rest (< k cap))
                  (push (pop rest) chunk)
                  (setq k (1+ k)))
                (push (funcall read (plist-put (copy-sequence request)
                                               :ids (nreverse chunk)))
                      results)))
            (setq results (nreverse results))
            (if (eq op 'owner-model)
                (let ((merged (make-hash-table :test 'eql)))
                  (dolist (image results)
                    (when (hash-table-p image)
                      (maphash (lambda (id h) (puthash id h merged)) image)))
                  merged)
              (apply #'append (mapcar #'copy-sequence results))))
        (funcall read request)))))

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
row.  Readback rows keep the decoder's types and go through the SAME
`dorg-model-from-db-row' path every loader uses, then canonicalized by the
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

(defun dorg-mutate--postimage-assert-rows (read checks)
  "Assert (2): in-transaction readback extension rows equal the typed effects.
READ is the typed transaction-local read port; CHECKS is the
net-effect expectation from `dorg-mutate--postimage-row-checks'.  Row
predicates remain typed equality alists here; DOrg lowers them inside the
transaction read port, so the proof path receives no SQL predicate text.

The present checks and then the absent checks form one ordered list, read
in consecutive chunks of at most `dorg-sql-transaction-read-port-max-checks'
through one batched count each.  Each chunk is judged in order, and the
first failing check aborts before any later chunk is read, so a successful
readback reports exactly the failure a check-by-check proof would."
  (let ((ordered (append (mapcar (lambda (check) (cons 'present check))
                                 (plist-get checks :present))
                         (mapcar (lambda (check) (cons 'absent check))
                                 (plist-get checks :absent))))
        ;; The chunk size is the read port's own cap (dorg-dolt.el), loaded
        ;; with the transport before any proof runs.
        (cap (with-suppressed-warnings
                 ((free-vars dorg-sql-transaction-read-port-max-checks))
               dorg-sql-transaction-read-port-max-checks)))
    (while ordered
      (let* ((chunk (seq-take ordered cap))
             (counts (funcall read
                              (list :version 1 :op 'extension-row-counts
                                    :checks (mapcar (lambda (check)
                                                      (cons (nth 1 check) (nth 2 check)))
                                                    chunk)))))
        (setq ordered (nthcdr cap ordered))
        (cl-loop
         for (kind table _where count) in chunk
         for n in counts
         do (if (eq kind 'present)
                (when (< n count)
                  (dorg-mutate--postimage-abort
                   'rows
                   "a validated derived row is missing or altered at readback"
                   :table table))
              (when (/= n count)
                (dorg-mutate--postimage-abort
                 'rows
                 "a deleted derived scope holds unexpected rows at readback"
                 :table table))))))))

(defun dorg-mutate--postimage-item-row-canonical (heading)
  "Return HEADING's persisted item row identity vector.
The heading-row core proof sees only `headings' columns.  The partition proof
reloads complete model rows and therefore also verifies item row ids and final
sort keys, which `items_hash' intentionally does not cover."
  (mapcar (lambda (item)
            (list (dorg-item-id item)
                  (dorg-item-sort-key item)))
          (dorg-heading-items heading)))

(defun dorg-mutate--postimage-partition-unions (owner-specs)
  "Return ((ROOT-ID . IDS) ...) for the present partitions of OWNER-SPECS.
Roots keep their first appearance and each root's ids their first
appearance, each id once.  Root ids are BIGINT values that may be equal
bignums without being `eq', so roots are grouped by an `eql' hash, as ids
are.  One pass, linear in the specs' id occurrences.  OWNER-SPECS is read,
never mutated."
  (let ((by-root (make-hash-table :test 'eql))
        (roots nil))
    (pcase-dolist (`(,root-id ,_owner-id ,ids ,_expected ,deleted) owner-specs)
      (unless deleted
        ;; ENTRY is (SEEN-HASH . IDS-NEWEST-FIRST).
        (let ((entry (gethash root-id by-root)))
          (unless entry
            (setq entry (list (make-hash-table :test 'eql)))
            (puthash root-id entry by-root)
            (push root-id roots))
          (dolist (id ids)
            (unless (gethash id (car entry))
              (puthash id t (car entry))
              (setcdr entry (cons id (cdr entry))))))))
    (mapcar (lambda (root-id) (cons root-id (reverse (cdr (gethash root-id by-root)))))
            (nreverse roots))))

(defun dorg-mutate--postimage-assert-partitions (read owner-specs)
  "Assert full parity for every affected owner partition, unconditionally.
OWNER-SPECS contains (ROOT-ID OWNER-ID IDS EXPECTED-HT DELETED).  For every
present partition, root-bound readback must contain every expected row;
recomputed item digests, candidate canonical and persisted item identity
must all match.  Deleted owners are covered by the union absence proof.

Each root is read once, as the union of its present partitions' ids
\(`dorg-mutate--postimage-partition-unions'), through READ, which chunks a
large union.  An owner image is a per-row load, so a heading's image does
not depend on which other ids are read with it.  The partitions are then
checked in their own order, and each one's ids in theirs, so a successful
read reports the first failure a partition-by-partition proof would; a
failed read raises before any check."
  (let ((images (make-hash-table :test 'eql)))
    (dolist (union (dorg-mutate--postimage-partition-unions owner-specs))
      (puthash (car union)
               (funcall read (list :version 1 :op 'owner-model
                                   :root-id (car union) :ids (cdr union)))
               images))
    (pcase-dolist (`(,root-id ,_owner-id ,ids ,expected ,deleted) owner-specs)
      (unless deleted
        (let ((image (gethash root-id images)))
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
              (let ((expected-row (gethash id expected)))
                (unless (equal (car expected-row)
                               (dorg-mutate--index-heading-canonical h))
                  (dorg-mutate--postimage-abort
                   'core
                   "readback model diverges from the validated candidate"
                   :ids (list id)))
                (unless (equal (cdr expected-row)
                               (dorg-mutate--postimage-item-row-canonical h))
                  (dorg-mutate--postimage-abort
                   'core
                   "committed item row identity diverges from the validated candidate"
                   :ids (list id)))))))))))

(defun dorg-mutate--postimage-plan (index-context effects)
  "Return the transaction-proof validator for one immutable attempt.
INDEX-CONTEXT supplies the full candidate and affected owner partitions;
EFFECTS supplies independently validated row expectations.  Every core,
owner, item-identity and extension proof remains unconditional.  No derived
receipt or wire model is built or persisted."
  (let* ((partitions (dorg-ext-index-context-owner-partitions index-context))
         (expectations (dorg-mutate--postimage-expectations
                        index-context partitions))
         (row-checks (dorg-mutate--postimage-row-checks effects))
         (extension-tables
          (delete-dups
           (delq nil (mapcar (lambda (effect)
                              (plist-get effect :table))
                            effects))))
         ;; A cross-root aggregate binds placement per partition rather than
         ;; forcing its union into the ordinary same-root context root.
         (union-root (unless (dorg-ext-index-context-from-root-id
                              index-context)
                       (dorg-ext-index-context-root-id index-context)))
         (owner-specs
          (mapcar
           (lambda (p)
             (let ((img (dorg-ext-owner-partition-candidate-postimage p))
                   (owner-id (dorg-ext-owner-partition-owner-id p))
                   (expected (make-hash-table :test 'eql))
                   (ids nil))
               (when img
                 (maphash (lambda (id h)
                            (push id ids)
                            (puthash id
                                     (cons
                                      (dorg-mutate--index-heading-canonical h)
                                      (dorg-mutate--postimage-item-row-canonical h))
                                     expected))
                          img))
               (list (dorg-ext-owner-partition-root-id p)
                     owner-id (sort ids #'<) expected
                     (not (and img (gethash owner-id img))))))
           partitions)))
    (list :validate
          (list
           :mode 'transaction-read-port
           :extension-tables extension-tables
           :function
           (lambda (read)
             ;; The existing bounded hand-off applies to EVERY proof read.
             (let ((read (dorg-mutate--postimage-read-batched read)))
               (dorg-ext-run-index-validators index-context 'transaction :all)
               (dorg-mutate--postimage-assert-core
                read (car expectations) (cdr expectations) union-root)
               (dorg-mutate--postimage-assert-partitions read owner-specs)
               (dorg-mutate--postimage-assert-rows read row-checks)))))))

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
     (list :constraint (plist-get facts :constraint)))
   ;; Closed diagnostic correlation from the batch executor:
   ;; the failed statement's execution ordinal, its closed verb/table class,
   ;; and the private-capture status/id.  All are value-free by construction
   ;; (`dorg-sql--statement-class' and the capture id carry no prose or SQL),
   ;; so they cross the same sanitized boundary as the classifier facts.
   (when (plist-member facts :statement-index)
     (list :statement-index (plist-get facts :statement-index)))
   (when (plist-member facts :statement-class)
     (list :statement-class (plist-get facts :statement-class)))
   (when (plist-member facts :capture)
     (list :capture (plist-get facts :capture)))
   (when (plist-member facts :capture-id)
     (list :capture-id (plist-get facts :capture-id)))))

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
             "in-transaction readback diverged; rolled back pre-commit"
             (append (list :source-kind 'dolt :detail 'postimage-parity)
                     (when (plist-member facts :parity-plane)
                       (list :parity-plane (plist-get facts :parity-plane)))
                     ;; Parity IDs are classifier-validated closed facts.  They
                     ;; are projected only by this confirmed parity branch, so
                     ;; rollback-failed and every non-parity refusal remain
                     ;; byte-identical diagnostic contracts.
                     (when (plist-member facts :parity-ids)
                       (list :parity-ids (plist-get facts :parity-ids)))
                     (when (plist-member facts :parity-id-count)
                       (list :parity-id-count (plist-get facts :parity-id-count)))
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
    (lease gen-id op-id orig-base root-id)
  "Return the compact settled result, recovering the original commit on LEASE.
GEN-ID identifies the immutable added transition, so recovery remains
generation-safe and fail-closed.  No data is replayed, no payload is read,
and historical derived evidence does not affect the response."
  (let ((commit (dorg-sql--mutation-log-recover-commit lease gen-id)))
    (list :ok t :dry-run nil :source-kind 'dolt :root-id root-id
          :base-commit orig-base :commit commit
          :op-id op-id :idempotent t
          :created nil :changed nil :deleted nil
          :extensions nil :invalidation nil)))

(defun dorg-mutate--dolt-op-id-settled (source _options op-id intent-digest orig-base)
  "Return an idempotent :ok result if OP-ID's matching evidence is settled, else nil.
The planning-failure reconciliation seam: acquires the store gate on a fresh
lease (so any in-flight prior commit under this op-id has settled), verifies the
versioned receipt schema read-only, and looks up OP-ID.  Present +
matching INTENT-DIGEST -> compact idempotent result (recovered original commit);
present with a DIFFERENT digest -> terminal `idempotency-key-mismatch';
absent -> nil (the caller re-raises its genuine planning failure)."
  (dorg-mutate-call-with-store-gate
   (dorg-mutate-source-dolt-dir source)
   (lambda (lease _key)
     (dorg-sql--require-mutation-log-schema lease)
     (let ((ev (dorg-sql--mutation-log-lookup lease op-id)))
       (cond
        ((null ev) nil)
        ((equal (cdr ev) intent-digest)
         (dorg-mutate--dolt-idempotent-result
          lease (car ev) op-id orig-base
          (dorg-mutate-source-root-id source)))
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
                                        dorg-mutate--settled-planning-error-reasons))
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
                       ;; Receipt storage is core schema v7.  Ordinary apply
                       ;; only verifies it and never mutates the working set.
                       (dorg-sql--require-mutation-log-schema lease)
                       (let ((ev (dorg-sql--mutation-log-lookup lease op-id)))
                         (when ev
                           (if (equal (cdr ev) intent-digest)
                               (throw 'dorg-mutate--gate-done
                                      (dorg-mutate--dolt-idempotent-result
                                       lease (car ev) op-id orig-base root-id))
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
                                   ;; Compact operation identity lands atomically
                                   ;; with the data; owner proofs stay below.
                                   (if op-id
                                       (append
                                        (plist-get plan :sql)
                                        (list
                                         (dorg-sql--mutation-log-insert-sql
                                          op-id gen-id intent-digest
                                          (truncate (float-time)))))
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
      (let* ((installed-extensions
              (dorg-mutate--dolt-installed-index-extension-names dolt-dir))
             (proj (dorg-ext-collect-index-projection
                    (plist-get sim :index-context) installed-extensions
                    (dorg-mutate--accumulator-fold-port
                     dolt-dir (plist-get sim :head-commit))))
             (full-sql (append sql (plist-get proj :sql)))
             ;; Full pre-commit proof from the immutable candidate context and
             ;; independently validated effects this attempt commits.
             (postimage (dorg-mutate--postimage-plan
                         (plist-get sim :index-context)
                         (plist-get proj :effects))))
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
              :validate (plist-get postimage :validate)
              :make-result
              (lambda (new-head)
                (append
                 (list :ok t :dry-run nil :source-kind 'dolt :root-id root-id
                       :base-commit base :commit new-head
                       :patches patches
                       :created (plist-get sim :created)
                       :changed (plist-get sim :changed)
                       :deleted (plist-get sim :deleted)
                       :moved (plist-get sim :moved))
                 (list :extensions (plist-get proj :extensions)
                       :invalidation (list :roots (list root-id)
                                           :ext-dirty (plist-get proj :ext-dirty)
                                           :head new-head
                                           :dolt-dir dolt-dir
                                           :file
                                           (dorg-mutate-source-file source))))))))))

(defun dorg-mutate--dolt-build-attempt (source patches options &optional owner-plan)
  "Build one attempt for SOURCE/PATCHES with OPTIONS.
The attempt is based on exactly one simulation."
  (dorg-mutate--dolt-attempt-from-sim
   source patches options
   (dorg-mutate--dolt-simulate source patches options owner-plan)))

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
                          :deleted (plist-get sim :deleted)
                          :moved (plist-get sim :moved))))
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
             :deleted (plist-get sim :deleted)
             :moved (plist-get sim :moved))
       (when (plist-get proj :extensions)
         (list :invalidation
               (list :roots (list root-id)
                     :ext-dirty (plist-get proj :ext-dirty))))))))

(defun dorg-mutate--dolt-execute
    (source patches options &optional initial-attempt owner-plan)
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
          source patches options owner-plan))))))

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
  (format "UPDATE headings SET root_id = %d, parent_id = %s, sort_key = %s, level_offset = %d, todo_keyword = %s, priority = %s, commented = %s, title = %s, post_blank = %d, items_hash = %s WHERE id = %d"
          root-id
          (if (dorg-heading-parent-id heading)
              (number-to-string (dorg-heading-parent-id heading))
            "NULL")
          (dorg-sql-gen--sort-key-hex (dorg-heading-sort-key heading))
          (dorg-sql-gen--level-offset heading)
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
reserved for that root's whole-root fallback.  An installed root-aggregate
projector requires the complete image of BOTH participating roots: its source
corpus loses moved rows while its destination corpus gains them.  Process-global
registration alone never widens an unrelated store."
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (src-root (dorg-mutate-source-root-id source))
         (dst-root (dorg-mutate-source-root-id dest))
         (src-ids nil)
         (dst-ids nil))
    (if (dorg-ext-index-projector-requires-whole-root-p
         'dolt
         (lambda (extension)
           (dorg-mutate--dolt-extension-installed-p dolt-dir extension)))
        (cons nil nil)
      (cl-labels
        ((src-add (id) (when (integerp id) (cl-pushnew id src-ids :test #'eql)))
         (dst-add (id) (when (integerp id) (cl-pushnew id dst-ids :test #'eql))))
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
        (dst-add dst-root)
        (dst-add dst-parent)
        ;; Destination placement locates its slot in the destination parent's
        ;; fan (keys only); the anchors themselves are hydrated to validate.
        (when (plist-member to :before)
          (dst-add (dorg-mutate--dolt-resolve-host-id
                    dest dst-root
                    (dorg-mutate--patch-anchor-address (plist-get to :before))
                    patch head)))
        (when (plist-member to :after)
          (dst-add (dorg-mutate--dolt-resolve-host-id
                    dest dst-root
                    (dorg-mutate--patch-anchor-address (plist-get to :after))
                    patch head)))
        (cons (delete-dups (nreverse src-ids))
              (delete-dups (nreverse dst-ids))))))))

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
    (when (dorg-model-root-p moved-root)
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
      (let* (;; Anchors are validated as destination siblings here; an
             ;; anchor in another root or inside the moved set is not a
             ;; sibling and signals `invalid-target' before any write.  A
             ;; destination gap without room rekeys a window of the
             ;; destination parent's fan on DST-PS; those rekeys lower
             ;; below together with the moved rows.
             (key (dorg-mutate--ps-place
                   dst-ps dst-parent-id (dorg-heading-id moved-root)
                   before after patch)))
        ;; Place the moved subtree in memory: the moved root takes the
        ;; destination parent and sort key and the patch's offset, or 1;
        ;; descendants keep their parent, sort key and offset.  Every moved
        ;; row is re-homed into the destination root, and the subtree's
        ;; effective levels are re-derived from the destination parent.
        (setf (dorg-heading-parent-id moved-root) dst-parent-id)
        (setf (dorg-heading-sort-key moved-root) key)
        (setf (dorg-heading-level-offset moved-root) (or (plist-get to :offset) 1))
        (dorg-mutate--ps-assert-representable dst-ps moved-root patch)
        (let ((by-parent (make-hash-table :test 'eql)))
          (dolist (d (cdr moved))
            (push d (gethash (dorg-heading-parent-id d) by-parent)))
          (dorg-mutate--derive-closure
           moved-root (dorg-heading-level dst-parent)
           (lambda (h) (gethash (dorg-heading-id h) by-parent))))
        (let ((sim (list :head-commit head
                         :from-scope-ids (and (car scopes)
                                              (sort (delete-dups
                                                     (copy-sequence (car scopes)))
                                                    #'<))
                         :to-scope-ids (and (cdr scopes)
                                            (sort (delete-dups
                                                   (copy-sequence (cdr scopes)))
                                                  #'<))
                         :sql (append
                               (mapcar
                                (lambda (h)
                                  (dorg-mutate--dolt-cross-root-heading-update
                                   h dst-root))
                                moved)
                               (dorg-sql-gen-lower-heading-ops
                                (dorg-mutate--ps-rekey-ops dst-ps dst-orig)
                                dst-orig dorg-mutate--dolt-item-id-generator))
                         :db-ops (append
                                  (mapcar (lambda (h) (list :update-heading h)) moved)
                                  (dorg-mutate--ps-rekey-ops dst-ps dst-orig))
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
              (installed-extensions
               (dorg-mutate--dolt-installed-index-extension-names dolt-dir))
              (proj (dorg-ext-collect-index-projection
                     (plist-get sim :index-context) installed-extensions
                     (dorg-mutate--accumulator-fold-port
                      dolt-dir (plist-get sim :head-commit))))
              (full-sql (append sql (plist-get proj :sql)))
              ;; The cross-root proof covers affected owners on BOTH roots.
              (postimage (dorg-mutate--postimage-plan
                          (plist-get sim :index-context)
                          (plist-get proj :effects))))
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
  (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
         (sim (condition-case e
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
      (let* ((installed-extensions
              (dorg-mutate--dolt-installed-index-extension-names dolt-dir))
             (proj (dorg-ext-collect-index-projection
                    (plist-get sim :index-context) installed-extensions
                    (dorg-mutate--accumulator-fold-port
                     dolt-dir (plist-get sim :head-commit)))))
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

(defun dorg-mutate--dolt-plan-data (source patches options unsupported
                                          &optional owner-plan)
  "Return semantic Dolt plan data for SOURCE, or nil when unavailable.
Runs a simulate (load + apply + diff + lower) so a typed plan on a
Dolt source returns `:head-commit'/`:db-ops'/`:created'/`:changed'/
`:deleted' WITHOUT committing (AC1).  Skipped when any patch is UNSUPPORTED
\(a cross-root move must classify before snapshot load) or when the Dolt
dir is not a readable repository directory (so the no-repo unit/plan
tests stay classification-only and never spawn a failing CLI call).

A same-batch delete that frees a sibling slot never collides with a later
placement: freed keys stay vacated until commit.  A validation / invariant
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
         source patches options owner-plan)))))

(defun dorg-mutate--dolt-plan-data--same-root
    (source patches options &optional owner-plan)
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
                 (dorg-mutate--dolt-simulate source patches options owner-plan)
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

(defun dorg-mutate--owner-plan-capability (plan family)
  "Return PLAN's private capability for FAMILY or signal a typed refusal.
This is an in-process authority boundary only: public and IR inputs never
carry PLAN, while trusted in-process Elisp can construct arbitrary records."
  (unless plan
    (dorg-mutate--raise 'invalid-target "owned metadata requires its typed owner"
                        :detail 'owner-unavailable :family family))
  (let* ((caps (append (when-let ((cap (plist-get plan :owner-capability)))
                        (list cap))
                      (plist-get plan :owner-capabilities)))
         (execution-ids
          (append (when-let ((id (plist-get plan :owner-execution-id)))
                    (list id))
                  (plist-get plan :owner-execution-ids))))
    ;; Reject crafted values before reading struct accessors: callers must get
    ;; the closed typed diagnostic, never an EIEIO/cl-defstruct type error.
    (dolist (candidate caps)
      (unless (cl-typep candidate 'dorg-mutate--owner-capability)
        (dorg-mutate--raise 'invalid-target "owner capability is forged"
                            :detail 'capability-forged :family family)))
    ;; A trusted plan that omits its claimed private authority is as forged as
    ;; one that supplies an arbitrary plist.  Keep this distinct from a wholly
    ;; unavailable owner (`PLAN' nil above), which has no trusted plan at all.
    (unless caps
      (dorg-mutate--raise 'invalid-target "owner capability is forged"
                          :detail 'capability-forged :family family))
    (let ((cap (cl-find family caps
                        :key #'dorg-mutate--owner-capability-family
                        :test #'equal)))
    (unless (cl-typep cap 'dorg-mutate--owner-capability)
      (dorg-mutate--raise 'invalid-target "owner capability names another family"
                          :detail 'capability-family-mismatch :family family))
    (unless (memq (dorg-mutate--owner-capability-execution-id cap) execution-ids)
      (dorg-mutate--raise 'invalid-target "owner capability belongs to another execution"
                          :detail 'capability-forged :family family))
      cap)))

(defun dorg-mutate--patch-validate-target (target context &optional owner-plan)
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
     (let* ((name (dorg-mutate--patch-tget target :name))
            (family (dorg-owned-metadata-family name)))
       (if family
           (progn
             (unless (dorg-owned-metadata-canonical-key-p name)
               (dorg-mutate--raise 'invalid-target
                                   "owned metadata key is not canonical"
                                   :detail 'owned-metadata-noncanonical
                                   :target target :key name))
             (dorg-mutate--owner-plan-capability owner-plan (car family)))
         (when (dorg-protected-keyword-p name)
           (dorg-mutate--raise 'invalid-target
                               "DORG_-prefixed properties are protected"
                               :operation context :target target)))))
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
    (:body
     (dorg-mutate--patch-check-keys (cdr target) '(:host :literal)
                                    '(:host :literal) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context t)
     (unless (and (stringp (dorg-mutate--patch-tget target :literal))
                  (not (string-empty-p
                        (dorg-mutate--patch-tget target :literal))))
       (dorg-mutate--raise 'invalid-target
                           "body literal must be a nonempty string"
                           :operation context :target target)))
    (:tree
     ;; :offset is internal (history restore of a skipped level); the public
     ;; insert form never lowers one.
     (dorg-mutate--patch-check-keys (cdr target) '(:under :before :after :offset)
                                    '(:under) context)
     (when (plist-member (cdr target) :offset)
       (let ((offset (dorg-mutate--patch-tget target :offset)))
         (unless (and (fixnump offset) (> offset 0))
           (dorg-mutate--raise 'invalid-op
                               "insert offset must be a positive integer"
                               :operation context :offset offset))))
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
    (:item
     (dorg-mutate--patch-check-keys (cdr target) '(:host :occurrence)
                                    '(:host :occurrence) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context t)
     (unless (and (integerp (dorg-mutate--patch-tget target :occurrence))
                  (>= (dorg-mutate--patch-tget target :occurrence) 1))
       (dorg-mutate--raise 'invalid-target "item occurrence must be positive"
                           :operation context :target target)))
    (:text
     (dorg-mutate--patch-check-keys (cdr target) '(:host :occurrence)
                                    '(:host :occurrence) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context)
     (unless (and (integerp (dorg-mutate--patch-tget target :occurrence))
                  (>= (dorg-mutate--patch-tget target :occurrence) 1))
       (dorg-mutate--raise
        'invalid-target "text occurrence must be positive"
        :operation context :target target)))
    (:drawer-lines
     (dorg-mutate--patch-check-keys (cdr target) '(:host :name :occurrence)
                                    '(:host :name) context)
     (dorg-mutate--patch-validate-host
      (dorg-mutate--patch-tget target :host) context t)
     (unless (stringp (dorg-mutate--patch-tget target :name))
       (dorg-mutate--raise 'invalid-target "invalid drawer-line target"
                           :operation context :target target))
     (when (plist-member (cdr target) :occurrence)
       (unless (and (integerp (dorg-mutate--patch-tget target :occurrence))
                    (>= (dorg-mutate--patch-tget target :occurrence) 1))
         (dorg-mutate--raise 'invalid-target "drawer-line occurrence must be positive"
                             :operation context :target target))))
    (_ (dorg-mutate--raise 'invalid-op "unknown typed target kind"
                           :operation context :target target)))
  target)

(defun dorg-mutate--patch-validate-item-value (item context &optional owner-plan)
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
                               :operation context :value cell))
         (let ((family (dorg-owned-metadata-family (car cell))))
           ;; A private family capability authorizes its own registered keys,
           ;; never system metadata carried incidentally by a whole drawer.
           (when (and owner-plan (dorg-protected-keyword-p (car cell))
                      (not family))
             (dorg-mutate--raise 'invalid-target
                                 "owner capability cannot write system metadata"
                                 :operation context :value cell))
           (when family
             (unless (dorg-owned-metadata-canonical-key-p (car cell))
               (dorg-mutate--raise 'invalid-target
                                   "owned metadata key is not canonical"
                                   :detail 'owned-metadata-noncanonical
                                   :operation context :value cell))
             (dorg-mutate--owner-plan-capability owner-plan (car family)))))
       (dolist (line (plist-get item :lines))
         (unless (stringp line)
           (dorg-mutate--raise 'invalid-target "invalid drawer line"
                               :operation context :value line))))
      (_ (dorg-mutate--raise 'invalid-target "unknown typed item kind"
                             :operation context :value item))))
  item)

(defun dorg-mutate--patch-validate-heading-payload
    (payload context allowed &optional owner-plan)
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
      (dorg-mutate--patch-validate-item-value item context owner-plan)))
  (when (plist-member payload :children)
    (unless (listp (plist-get payload :children))
      (dorg-mutate--raise 'invalid-target "heading :children must be a list"
                          :operation context))
    (dolist (child (plist-get payload :children))
      (dorg-mutate--patch-validate-heading-payload
       child context
       '(:id :title :todo :priority :tags :commented :post-blank
             :items :children)
       owner-plan)))
  payload)

(defconst dorg-mutate--patch-fields
  '(((put :property)      (:op :target :value) (:op :target :value))
    ((delete :property)   (:op :target) (:op :target))
    ((replace :keyword)   (:op :target :value :missing) (:op :target :value))
    ((replace :keywords)  (:op :target :values) (:op :target :values))
    ((insert :keywords)   (:op :target :value :position)
                           (:op :target :value :position))
    ((insert :item)       (:op :target :item :position)
                           (:op :target :item :position))
    ((replace :item)      (:op :target :item :missing)
                           (:op :target :item))
    ((delete :item)       (:op :target :missing) (:op :target))
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
    ((replace :drawer-lines) (:op :target :value :values)
                              (:op :target))
    ((delete :drawer-lines) (:op :target) (:op :target))
    ((insert :texts)      (:op :target :value :position)
                           (:op :target :value :position))
    ((replace :text)      (:op :target :value :missing)
                           (:op :target :value :missing))
    ((replace :body)      (:op :target :value) (:op :target :value))
    ((delete :text)       (:op :target :missing) (:op :target :missing))
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

(defun dorg-mutate--validate-engine-patch (patch index &optional owner-plan)
  "Validate executor-private PATCH at INDEX and return it unchanged.
The public typed-patch boundary is `dorg-mutate--validate-patch'; this helper
accepts only the executor representation produced after that boundary."
  (let* ((signature (dorg-mutate--patch-signature patch))
         (row (assoc signature dorg-mutate--patch-fields))
         (op (car signature))
         (target (plist-get patch :target)))
    (unless row
      (dorg-mutate--raise 'invalid-op "unsupported typed verb/target pair"
                          :operation op :target-kind (cadr signature)
                          :op-index index))
    (dorg-mutate--patch-check-keys patch (nth 1 row) (nth 2 row) op)
    (dorg-mutate--patch-validate-target target op owner-plan)
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
       (unless (or (memq (plist-get patch :position) '(:before-first :after-last))
                   (pcase (plist-get patch :position)
                     (`(:before ,n) (and (integerp n) (>= n 1)))
                     (`(:after ,n) (and (integerp n) (>= n 1)))))
         (dorg-mutate--raise
          'invalid-op "keyword insert requires :before-first or :after-last"
          :operation op :detail 'invalid-position)))
      (`(insert :texts)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (dorg-mutate--validate-text-payload (plist-get patch :value) op)
       (unless (or (memq (plist-get patch :position) '(:before-first :after-last))
                   (pcase (plist-get patch :position)
                     (`(:before ,n) (and (integerp n) (>= n 1)))
                     (`(:after ,n) (and (integerp n) (>= n 1)))))
         (dorg-mutate--raise
          'invalid-op "text insert requires :before-first or :after-last"
          :operation op :detail 'invalid-position)))
      (`(insert :item)
       (dorg-mutate--patch-validate-item-value
        (plist-get patch :item) op owner-plan)
       (unless (or (memq (plist-get patch :position) '(:before-first :after-last))
                   (pcase (plist-get patch :position)
                     (`(:before ,n) (and (integerp n) (>= n 1)))
                     (`(:after ,n) (and (integerp n) (>= n 1)))))
         (dorg-mutate--raise 'invalid-op "item insert requires a sequence position"
                             :operation op :detail 'invalid-position)))
      (`(replace :item)
       (dorg-mutate--patch-validate-item-value
        (plist-get patch :item) op owner-plan)
       (unless (eq (plist-get patch :missing) :error)
         (dorg-mutate--raise 'invalid-op "single item mutation requires :missing :error"
                             :operation op :detail 'invalid-missing-policy)))
      (`(delete :item)
       (unless (eq (plist-get patch :missing) :error)
         (dorg-mutate--raise 'invalid-op "single item mutation requires :missing :error"
                             :operation op :detail 'invalid-missing-policy)))
      (`(replace :text)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (unless (eq (plist-get patch :missing) :error)
         (dorg-mutate--raise
          'invalid-op "single text mutation requires :missing :error"
          :operation op :detail 'invalid-missing-policy)))
      (`(replace :body)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value))))
      (`(insert :drawer-lines)
       (unless (stringp (plist-get patch :value))
         (dorg-mutate--raise 'invalid-target "typed patch value must be a string"
                             :operation op :value (plist-get patch :value)))
       (dorg-mutate--single-line-payload (plist-get patch :value) op)
       (unless (and (or (memq (plist-get patch :position)
                            '(:before-first :after-last))
                       (pcase (plist-get patch :position)
                         (`(:before ,n) (and (integerp n) (>= n 1)))
                         (`(:after ,n) (and (integerp n) (>= n 1)))))
                    (memq (plist-get patch :missing) '(:create :error)))
         (dorg-mutate--raise 'invalid-target "invalid drawer-line placement"
                             :operation op)))
      (`(replace :drawer-lines)
       (if (plist-member (cdr (plist-get patch :target)) :occurrence)
           (unless (and (plist-member patch :value)
                        (stringp (plist-get patch :value))
                        (not (plist-member patch :values)))
             (dorg-mutate--raise 'invalid-target "drawer-line occurrence replacement needs one line"
                                 :operation op))
         (unless (and (consp (plist-get patch :values))
                      (not (plist-member patch :value))
                      (cl-every #'stringp (plist-get patch :values)))
           (dorg-mutate--raise 'invalid-target "drawer-line all replacement needs lines"
                               :operation op))))
      (`(delete :drawer-lines)
       (when (or (plist-member patch :value) (plist-member patch :values))
         (dorg-mutate--raise 'invalid-target "drawer-line delete has no payload"
                             :operation op)))
      (`(update :planning)
       (unless (or (plist-member patch :scheduled)
                   (plist-member patch :deadline)
                   (plist-member patch :closed))
         (dorg-mutate--raise 'invalid-target
                             "planning update requires at least one field"
                             :operation op :target target))
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
         (dorg-mutate--patch-validate-item-value item op owner-plan)))
      (`(insert :tree)
       (dorg-mutate--patch-validate-heading-payload
        (plist-get patch :heading) op
        '(:id :title :todo :priority :tags :commented :post-blank
              :items :children)
        owner-plan))
      (`(update :heading)
       (dorg-mutate--patch-validate-heading-payload
        (plist-get patch :fields) op
        '(:title :todo :priority :tags :commented :post-blank)
        owner-plan))
      (`(move :subtree)
       (let ((to (plist-get patch :to)))
         (dorg-mutate--patch-check-keys to '(:under :before :after :offset)
                                        '(:under) op)
         (when (plist-member to :offset)
           (let ((offset (plist-get to :offset)))
             (unless (and (fixnump offset) (> offset 0))
               (dorg-mutate--raise 'invalid-op
                                   "move offset must be a positive integer"
                                   :operation op :offset offset))))
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

(defconst dorg-mutate--retired-sequence-target-kinds
  '(:keywords :texts :items :drawer-lines)
  "Pre-v0.2 sequence target tags rejected at the public engine boundary.")

(defun dorg-mutate--sequence-select (target required context)
  "Validate TARGET's optional v2 selector and return it.
REQUIRED names the selector law for CONTEXT: `all-or-occurrence' or nil."
  (let ((select (dorg-mutate--patch-tget target :select)))
    (if required
        (progn
          (unless (consp select)
            (dorg-mutate--raise 'invalid-target "sequence selector is required"
                                :operation context :target target))
          (pcase select
            (`(:all) select)
            (`(:occurrence ,n)
             (unless (and (integerp n) (>= n 1))
               (dorg-mutate--raise 'invalid-target
                                   "sequence occurrence must be positive"
                                   :operation context :target target))
             select)
            (_ (dorg-mutate--raise 'invalid-target "invalid sequence selector"
                                   :operation context :target target))))
      (when select
        (dorg-mutate--raise 'invalid-target "insert target has a selector"
                            :operation context :target target)))))

(defun dorg-mutate--validate-sequence-v2-patch (patch index)
  "Validate one closed v2 sequence PATCH at INDEX and return PATCH.
The executor sees a private lowering only after this function has rejected all
retired target tags, `:occurrence' fields, and top-level `:missing' policies."
  (let* ((target (plist-get patch :target))
         (kind (car-safe target))
         (op (plist-get patch :op))
         (context (or op 'unknown)))
    (unless (and (listp patch) (memq kind '(:keyword :text :item :drawer-line)))
      (dorg-mutate--raise 'invalid-op "invalid v2 sequence patch"
                          :operation context :op-index index))
    (when (plist-member patch :missing)
      (dorg-mutate--raise 'invalid-target "v2 sequence rows do not carry :missing"
                          :operation context :detail 'retired-ir :patch patch))
    ;; This must precede the closed target-key check below.  Otherwise an
    ;; isolated retired `:occurrence' key leaks out as the generic `invalid-op'
    ;; unknown-field diagnostic instead of the v0.2 hard-cut refusal.
    (when (plist-member (cdr target) :occurrence)
      (dorg-mutate--raise 'invalid-target "v2 sequence target uses retired :occurrence"
                          :operation context :detail 'retired-ir :target target))
    (pcase kind
      (:keyword
       (dorg-mutate--patch-check-keys (cdr target) '(:host :name :select)
                                      '(:host :name) context)
       (dorg-mutate--patch-validate-host (dorg-mutate--patch-tget target :host) context)
       (let ((name (dorg-mutate--patch-tget target :name)))
         (unless (stringp name)
           (dorg-mutate--raise 'invalid-target "keyword name must be a string"
                               :operation context :target target))
         (when (dorg-protected-keyword-p name)
           (dorg-mutate--raise 'invalid-target "protected keyword target"
                               :operation context :target target))))
      (:text
       (dorg-mutate--patch-check-keys (cdr target) '(:host :select) '(:host) context)
       (dorg-mutate--patch-validate-host (dorg-mutate--patch-tget target :host) context))
      (:item
       (dorg-mutate--patch-check-keys (cdr target) '(:host :select) '(:host) context)
       (dorg-mutate--patch-validate-host (dorg-mutate--patch-tget target :host) context))
      (:drawer-line
       (dorg-mutate--patch-check-keys (cdr target) '(:host :name :select)
                                      '(:host :name) context)
       (dorg-mutate--patch-validate-host (dorg-mutate--patch-tget target :host) context)
       (unless (stringp (dorg-mutate--patch-tget target :name))
         (dorg-mutate--raise 'invalid-target "drawer name must be a string"
                             :operation context :target target))))
    (let* ((select (dorg-mutate--sequence-select
                    target (not (eq op 'insert)) context))
           (cell (dorg-mutate-sequence-cell kind op select))
           (host (dorg-mutate--patch-tget target :host)))
      (unless (and cell (plist-get cell :legal))
        (dorg-mutate--raise 'invalid-target "unsupported sequence cell"
                            :operation context :detail 'unsupported-selector
                            :target target))
      (when (and (eq host :root) (not (plist-get cell :root)))
        (dorg-mutate--raise 'invalid-target "operation requires a heading host"
                            :operation context :target host))
      (pcase (list op kind select)
      (`(replace :keyword (:all))
       (dorg-mutate--patch-check-keys patch '(:op :target :values) '(:op :target :values) context)
       (unless (and (consp (plist-get patch :values))
                    (cl-every #'stringp (plist-get patch :values)))
         (dorg-mutate--raise 'invalid-target "keyword all replacement needs strings"
                             :operation context)))
      (`(delete :keyword (:all))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(replace :keyword (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target :value) '(:op :target :value) context))
      (`(delete :keyword (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(insert :keyword nil)
       (dorg-mutate--patch-check-keys patch '(:op :target :value :position)
                                      '(:op :target :value :position) context))
      (`(replace :text (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target :value) '(:op :target :value) context))
      (`(delete :text (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(insert :text nil)
       (dorg-mutate--patch-check-keys patch '(:op :target :value :position)
                                      '(:op :target :value :position) context))
      (`(replace :item (:all))
       (dorg-mutate--patch-check-keys patch '(:op :target :items) '(:op :target :items) context)
       (unless (consp (plist-get patch :items))
         (dorg-mutate--raise 'invalid-target "item all replacement needs items"
                             :operation context)))
      (`(replace :item (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target :item) '(:op :target :item) context))
      (`(delete :item (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(insert :item nil)
       (dorg-mutate--patch-check-keys patch '(:op :target :item :position)
                                      '(:op :target :item :position) context))
      (`(replace :drawer-line (:all))
       (dorg-mutate--patch-check-keys patch '(:op :target :values) '(:op :target :values) context))
      (`(delete :drawer-line (:all))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(replace :drawer-line (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target :value) '(:op :target :value) context))
      (`(delete :drawer-line (:occurrence ,_))
       (dorg-mutate--patch-check-keys patch '(:op :target) '(:op :target) context))
      (`(insert :drawer-line nil)
       (dorg-mutate--patch-check-keys patch '(:op :target :value :position)
                                      '(:op :target :value :position) context))
      (_ (dorg-mutate--raise 'invalid-target "unsupported sequence cell"
                             :operation context :detail 'unsupported-selector
                             :target target))))
    patch))

(defun dorg-mutate--sequence-patch->engine (patch)
  "Lower a validated v2 sequence PATCH to the existing item executor form."
  (let* ((target (plist-get patch :target))
         (kind (car-safe target))
         (host (dorg-mutate--patch-tget target :host))
         (name (dorg-mutate--patch-tget target :name))
         (select (dorg-mutate--patch-tget target :select))
         (op (plist-get patch :op)))
    (pcase (list op kind select)
      (`(replace :keyword (:all))
       (list :op 'replace :target (list :keywords :host host :name name)
             :values (plist-get patch :values)))
      (`(delete :keyword (:all))
       (list :op 'delete :target (list :keywords :host host :name name)))
      (`(,operation :keyword (:occurrence ,n))
       (append (list :op operation :target (list :keyword :host host :name name :occurrence n))
               (when (plist-member patch :value) (list :value (plist-get patch :value)))
               (list :missing :error)))
      (`(insert :keyword nil)
       (list :op 'insert :target (list :keywords :host host :name name)
             :value (plist-get patch :value) :position (plist-get patch :position)))
      (`(,operation :text (:occurrence ,n))
       (append (list :op operation :target (list :text :host host :occurrence n))
               (when (plist-member patch :value) (list :value (plist-get patch :value)))
               (list :missing :error)))
      (`(insert :text nil)
       (list :op 'insert :target (list :texts :host host)
             :value (plist-get patch :value) :position (plist-get patch :position)))
      (`(replace :item (:all))
       (list :op 'replace :target (list :items :host host)
             :items (plist-get patch :items)))
      (`(,operation :item (:occurrence ,n))
       (append (list :op operation :target (list :item :host host :occurrence n))
               (when (plist-member patch :item) (list :item (plist-get patch :item)))
               (list :missing :error)))
      (`(insert :item nil)
       (list :op 'insert :target (list :item :host host :occurrence 1)
             :item (plist-get patch :item) :position (plist-get patch :position)))
      (`(insert :drawer-line nil)
       (list :op 'insert :target (list :drawer-lines :host host :name name :occurrence 1)
             :position (plist-get patch :position) :missing :create
             :value (plist-get patch :value)))
      (`(,operation :drawer-line (:all))
       (append (list :op operation :target (list :drawer-lines :host host :name name))
               (when (plist-member patch :values) (list :values (plist-get patch :values)))))
      (`(,operation :drawer-line (:occurrence ,n))
       (append (list :op operation
                     :target (list :drawer-lines :host host :name name :occurrence n))
               (when (plist-member patch :value) (list :value (plist-get patch :value)))))
      (_ patch))))

(defun dorg-mutate--validate-patch (patch index &optional owner-plan)
  "Validate public PATCH at INDEX, refusing every retired sequence IR shape."
  (let ((target (plist-get patch :target)))
    (cond
     ((memq (car-safe target) '(:keyword :text :item :drawer-line))
      (dorg-mutate--validate-sequence-v2-patch patch index))
     ((memq (car-safe target) dorg-mutate--retired-sequence-target-kinds)
      (dorg-mutate--raise 'invalid-target "retired typed sequence target"
                          :operation (plist-get patch :op) :detail 'retired-ir
                          :target target))
     ((and (memq (car-safe target) '(:keyword :text))
           (plist-member (cdr target) :occurrence))
      (dorg-mutate--raise 'invalid-target "retired typed occurrence field"
                          :operation (plist-get patch :op) :detail 'retired-ir
                          :target target))
     (t (dorg-mutate--validate-engine-patch patch index owner-plan)))))

(defun dorg-mutate--validate-patches (patches &optional owner-plan)
  "Validate public PATCHES and lower their closed v2 sequence rows for execution."
  (unless (listp patches)
    (dorg-mutate--raise 'invalid-op "typed patch batch must be a list"
                        :patches patches))
  (cl-loop for patch in patches for index from 0
           collect
           (let ((public (dorg-mutate--validate-patch patch index owner-plan)))
              (dorg-mutate--validate-engine-patch
              (dorg-mutate--sequence-patch->engine public) index owner-plan))))

(defun dorg-mutate--make-plan (source patches options &optional owner-plan)
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
                     source patches options unsupported owner-plan)))
    ;; Plain body literals have source-specific representability obligations
    ;; (byte mapping and structural postimage) that static capability
    ;; classification cannot see.  Make dry-run enter the same no-write
    ;; planner as apply, so it cannot advertise an unexecutable mutation.
    (when (and (eq kind 'plain-org)
               (cl-some (lambda (patch)
                          (equal (dorg-mutate--patch-signature patch)
                                 '(replace :body)))
                        patches))
      (dorg-mutate--source-preserving-preflight patches source))
    ;; A dry-run has no applied candidate from which to construct the owner
    ;; context, so its one isolated simulation remains the authoritative
    ;; no-write gate.  Apply validates after its single real splice instead.
    (when (and (eq kind 'plain-org) (plist-get options :dry-run)
               patches (null unsupported))
      (dorg-mutate--validate-owned-metadata-context
       (dorg-mutate--plain-org-planned-index-context
        source patches (dorg-mutate--plain-org-revision source))))
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

(defun dorg-mutate--execute-patches (src patches options plan &optional owner-plan)
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
            (plist-get plan :dorg-execution-attempt) owner-plan))))))))

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

(defun dorg-mutate--reject-owner-request-option (options)
  "Refuse even nil :owner-request in caller OPTIONS; capability owns it."
  (when (plist-member options :owner-request)
    (dorg-mutate--raise 'invalid-target
                        "owner request cannot be passed as an option"
                        :detail 'owner-request-forged)))

(defun dorg-mutate--plan-patches (source patches &optional ext-inputs &rest options)
  "Validate typed PATCHES for SOURCE and return a non-mutating plan.

EXT-INPUTS is the RETIRED per-execution `:extension-inputs' channel
\(tracked-regression): index projectors now run automatically from the candidate-state
context and accept no caller payload.  The positional parameter is kept for
signature stability but FAILS CLOSED — a non-nil value signals `unsupported'."
  (dorg-mutate--reject-extension-inputs ext-inputs)
  (dorg-mutate--reject-owner-request-option options)
  (let* ((src (dorg-mutate--resolve-source source))
         (patches (dorg-mutate--validate-patches patches)))
    (dorg-mutate--gate-identity-none src patches)
    (let ((plan (dorg-mutate--make-plan src patches options)))
      ;; Planning remains a no-write owner gate.  Apply builds this same
      ;; context from its one actual splice after the established CAS and
      ;; invariant guards, so ordinary public execution neither changes
      ;; refusal precedence nor invokes the splice seam twice.
      (when (and (eq (dorg-mutate-source-kind src) 'plain-org)
                 patches (not (plist-get plan :unsupported)))
        (dorg-mutate--validate-owned-metadata-context
         (dorg-mutate--plain-org-planned-index-context
          src patches (dorg-mutate--plain-org-revision src))))
      plan)))

(defvar dorg-mutate--held-plain-file-lock nil
  "Dynamically held (canonical file, owner, alias, raise function).
This one dynamic binding carries the token across API-to-engine re-entry.")

(defun dorg-mutate--plain-gate-raise (raise reason message &rest data)
  "Call RAISE with REASON, MESSAGE and DATA for a writer-gate refusal."
  (apply (or raise #'dorg-mutate--raise) reason message data))

(defun dorg-mutate--assert-held-plain-file-gate ()
  "Require the saved plain-file gate and verify its alias and owner."
  (unless dorg-mutate--held-plain-file-lock
    (dorg-mutate--raise
     'invariant "saved plain-org commit has no held file gate"
     :detail 'file-gate-not-held))
  (let* ((key (car dorg-mutate--held-plain-file-lock))
         (record (cadr dorg-mutate--held-plain-file-lock))
         (alias (nth 2 dorg-mutate--held-plain-file-lock))
         (raise (nth 3 dorg-mutate--held-plain-file-lock)))
    (unless (equal key (file-truename alias))
      (dorg-mutate--plain-gate-raise
       raise 'stale "plain-org source link changed" :detail 'source-retargeted))
    (unless (equal record
                   (file-symlink-p
                    (dorg-file-lock--path-from-canonical key)))
      (dorg-mutate--plain-gate-raise
       raise 'invariant "plain-org file gate ownership was lost"
       :detail 'file-lock-lost))))

(defun dorg-mutate--canonical-locked-source (source)
  "Return SOURCE bound to the held canonical file for an atomic saved commit."
  (if (and dorg-mutate--held-plain-file-lock
           (eq (dorg-mutate-source-kind source) 'plain-org))
      (dorg-mutate--source-make
       :kind 'plain-org
       :buffer (dorg-mutate-source-buffer source)
       :file (car dorg-mutate--held-plain-file-lock)
       :root-id (dorg-mutate-source-root-id source)
       :identity (dorg-mutate-source-identity source))
    source))

(defun dorg-mutate--known-dolt-source-p (source)
  "Return non-nil when SOURCE is known Dolt without strict resolution."
  (or (and (dorg-mutate-source-p source)
           (eq (dorg-mutate-source-kind source) 'dolt))
      (and (consp source) (keywordp (car source))
           (plist-member source :dolt-dir)
           (not (plist-member source :buffer))
           (not (plist-member source :file))
           (not (plist-member source :plain-org)))
      (let ((buffer (cond ((null source) (current-buffer))
                          ((bufferp source) source)
                          ((stringp source) (get-buffer source))
                          ((and (consp source) (keywordp (car source))
                                (plist-member source :buffer))
                           (get-buffer (plist-get source :buffer))))))
        (when (buffer-live-p buffer)
          (let ((registration (dorg--classify-registration buffer)))
            (and registration
                 (eq (dorg-backend-type (dorg-registration-backend registration))
                     'dolt)))))))

(defun dorg-mutate--with-saved-plain-file-gate (source mode options fn &optional raise)
  "Call FN under SOURCE's local saved plain-Org gate when MODE is apply.
OPTIONS supplies :save and the optional finite :lock-timeout in seconds.
RAISE, when supplied, maps gate refusals to the public entry condition."
  (let ((timeout (if (plist-member options :lock-timeout)
                     (plist-get options :lock-timeout)
                   dorg-file-lock-default-timeout)))
    (unless (dorg-file-lock--valid-timeout-p timeout)
      (dorg-mutate--plain-gate-raise
       raise 'invalid-target "invalid plain-org lock timeout" :detail 'lock-timeout))
    (if (or (not (eq mode 'apply))
            (dorg-mutate--known-dolt-source-p source))
        (funcall fn)
      (let* ((src (dorg-mutate--resolve-source source))
             (file (dorg-mutate-source-file src))
             (buffer (dorg-mutate-source-buffer src))
             (saving (or (not (buffer-live-p buffer))
                         (plist-get options :save))))
        (if (not (and (eq (dorg-mutate-source-kind src) 'plain-org)
                      saving file))
            (funcall fn)
          (when (file-remote-p file)
            (dorg-mutate--plain-gate-raise
             raise 'unsupported "saved remote plain-org writes are unsupported"
             :detail 'remote-file-gate))
          (let* ((key (file-truename file))
                 (lock (dorg-file-lock--path-from-canonical key)))
            (if dorg-mutate--held-plain-file-lock
                (if (and (equal key (car dorg-mutate--held-plain-file-lock))
                         (equal (file-symlink-p lock)
                                (cadr dorg-mutate--held-plain-file-lock)))
                    (funcall fn)
                  (dorg-mutate--plain-gate-raise
                   (nth 3 dorg-mutate--held-plain-file-lock)
                   'invariant "nested saved plain-org writes target different files"
                   :detail (if (equal key (car dorg-mutate--held-plain-file-lock))
                               'file-lock-lost 'nested-file-gate)))
              (let ((result
                     (dorg-file-lock-call
                      lock timeout
                      (lambda (record)
                        (let ((dorg-mutate--held-plain-file-lock
                               (list key record file raise)))
                          (unless (equal key (file-truename file))
                            (dorg-mutate--plain-gate-raise
                             raise 'stale "plain-org source link changed"
                             :detail 'source-retargeted))
                          (funcall fn))))))
                (if (eq (plist-get result :status) :acquired)
                    (plist-get result :value)
                  (let ((status (plist-get result :status)))
                    (dorg-mutate--plain-gate-raise
                     raise (if (eq status :unsupported) 'unsupported 'contended)
                     (if (eq status :unsupported)
                         "local filesystem cannot publish a file lock"
                       "saved plain-org writer gate is held")
                     :detail (if (eq status :unsupported) 'file-lock-unavailable
                               (intern (substring (symbol-name status) 1))))))))))))))

(defun dorg-mutate--apply-patches-internal
    (source patches ext-inputs owner-authority &rest options)
  "Apply PATCHES, acquiring the saved plain-Org file gate before planning."
  (dorg-mutate--reject-extension-inputs ext-inputs)
  (dorg-mutate--reject-owner-request-option options)
  (dorg-mutate--with-saved-plain-file-gate
   source 'apply options
   (lambda ()
     (apply #'dorg-mutate--apply-patches-internal-locked
            (dorg-mutate--canonical-locked-source
             (dorg-mutate--resolve-source source))
            patches ext-inputs owner-authority options))))

(defun dorg-mutate--apply-patches-internal-locked
    (source patches ext-inputs owner-authority &rest options)
  "Validate and execute typed PATCHES against SOURCE, or signal.

EXT-INPUTS is the RETIRED `:extension-inputs' channel (tracked-regression): index
projectors run automatically from the candidate-state context and accept no
caller payload.  The positional parameter is kept for signature stability but
FAILS CLOSED — a non-nil value signals `unsupported'.
An eligible initial Dolt planning failure may be a settled op-id replay.
After validation, recover only digest-matching evidence on a fresh gated lease;
an evidence miss preserves the original failure.  Execution is never retried."
  (dorg-mutate--reject-extension-inputs ext-inputs)
  (dorg-mutate--reject-owner-request-option options)
  (let* ((src (dorg-mutate--resolve-source source))
         (owner-authorities
          (cond ((null owner-authority) nil)
                ((cl-typep owner-authority 'dorg-mutate--owner-capability)
                 (list owner-authority))
                ((and (listp owner-authority)
                      (cl-every (lambda (cap)
                                  (cl-typep cap 'dorg-mutate--owner-capability))
                                owner-authority))
                 owner-authority)
                (t (dorg-mutate--raise 'invalid-target "owner capability is forged"
                                       :detail 'capability-forged))))
         (owner-plan
          (and owner-authorities
               (list :owner-capabilities owner-authorities
                     :owner-execution-ids
                     (mapcar #'dorg-mutate--owner-capability-execution-id
                             owner-authorities))))
         (patches (dorg-mutate--validate-patches patches owner-plan))
         ;; Pin generated ids BEFORE the one reusable plan attempt is built.
         ;; Every safe replan then receives the same immutable intent/id pair;
         ;; without an op-id, a discarded uncommitted attempt may allocate a
         ;; fresh id exactly as before.
         (patches (if (and (eq (dorg-mutate-source-kind src) 'dolt)
                           (plist-get options :op-id))
                      (dorg-mutate--pin-insert-heading-ids patches)
                    patches))
         (_ (dorg-mutate--gate-identity-none src patches)))
    (catch 'dorg-mutate--initial-plan-settled
      (let ((plan
             (condition-case perr
                 (let ((plan (dorg-mutate--make-plan src patches options owner-plan)))
                   (when owner-authorities
                     (setq plan (plist-put plan :owner-capabilities owner-authorities))
                     (setq plan (plist-put plan :owner-execution-ids
                                           (mapcar #'dorg-mutate--owner-capability-execution-id
                                                   owner-authorities))))
                   plan)
               (dorg-mutate-error
                ;; The reusable initial attempt is built BEFORE gated-apply;
                ;; its failure cannot reach that seam's re-plan handler.
                ;; Validation and the public intent digest already precede us.
                (let ((op-id (plist-get options :op-id))
                      (digest (plist-get options :intent-digest)))
                  (when (and (eq (dorg-mutate-source-kind src) 'dolt)
                             (not (plist-get options :dry-run))
                             op-id (stringp digest) (not (equal digest ""))
                             (memq (dorg-mutate-error-reason perr)
                                   dorg-mutate--settled-planning-error-reasons))
                    (when-let ((hit (dorg-mutate--dolt-op-id-settled
                                    src options op-id digest
                                    (plist-get options :base-commit))))
                      (throw 'dorg-mutate--initial-plan-settled hit)))
                  (signal (car perr) (cdr perr)))))))
        ;; Only the initial plan is handled above, never execution/commit.
        (dorg-mutate--execute-patches src patches options plan owner-plan)))))

(defun dorg-mutate--apply-patches (source patches &optional ext-inputs &rest options)
  "Validate and execute public typed PATCHES against SOURCE, or signal."
  (apply #'dorg-mutate--apply-patches-internal
         source patches ext-inputs nil options))

(defun dorg-mutate--owner-lowering-execute (source family request &rest options)
  "Lower owner REQUEST under the saved plain-Org gate when it will persist.
The public API enters that gate itself and calls the -locked seam directly."
  (dorg-mutate--reject-owner-request-option options)
  (dorg-mutate--with-saved-plain-file-gate
   source 'apply options
   (lambda ()
     (apply #'dorg-mutate--owner-lowering-execute-locked
            source family request options))))

(defun dorg-mutate--owner-lowering-execute-locked
    (source family request &rest options)
  "Run FAMILY's private typed lowering for REQUEST with fresh plan authority.
The owner receives only REQUEST and returns `(:patches PATCHES)'; the registry
mints its uninterned capability after lowering and never exposes it publicly."
  (dorg-mutate--reject-owner-request-option options)
  (let* ((participants
          (condition-case err
              (dorg-owned-metadata-participants family)
            (error
             (apply #'dorg-mutate--raise
                    'invalid-target "owned metadata owner refused lowering"
                    :family family
                    (dorg-owned-metadata-normalize-owner-signal
                     family nil err)))))
         (lower (and (listp participants) (plist-get participants :lower))))
    (unless (functionp lower)
      (dorg-mutate--raise 'invalid-target "owned metadata owner is unavailable"
                          :family family :detail 'owner-unavailable))
    (let* ((lowered
            (condition-case err
                (funcall lower request)
              (error
               (apply #'dorg-mutate--raise
                      'invalid-target "owned metadata owner refused lowering"
                      :family family
                      (dorg-owned-metadata-normalize-owner-signal
                       family participants err)))))
           (patches (plist-get lowered :patches))
           (capability
            (dorg-mutate--owner-capability--make
             :family family
             :request (dorg-ext--deep-copy request)
             :execution-id (make-symbol "dorg-owned-metadata-execution-"))))
      (unless (listp patches)
        (dorg-mutate--raise 'invalid-target "owner lowering returned no patches"
                            :family family :detail 'owner-unavailable))
      (apply #'dorg-mutate--apply-patches-internal-locked
             source patches nil capability options))))

(provide 'dorg-mutate)
;;; dorg-mutate.el ends here
