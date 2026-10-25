;;; dorg-ext-rebuild.el --- Exact-revision derived-index rebuild -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Administrative rebuild is deliberately narrower than mutation and import.
;; It reads one complete root at one committed revision, invokes the existing
;; index projector with a create-only full snapshot, audits the extension's
;; declared root scope, and optionally repairs drift at that exact HEAD.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dorg-dolt)
(require 'dorg-ext)
(require 'dorg-import)
(require 'dorg-model-load)
(require 'dorg-mutate)
(require 'dorg-sql-gen)
(require 'dorg-transaction)

(define-error 'dorg-ext-rebuild-error
  "DOrg derived-index rebuild failed" 'dorg-error)

(defconst dorg-ext-rebuild--option-keys '(:dolt-dir :revision :mode :identity)
  "Accepted keyword arguments for `dorg-ext-rebuild-index'.")

(defun dorg-ext-rebuild--raise (message &rest data)
  "Signal a typed rebuild error with MESSAGE and DATA plist."
  (signal 'dorg-ext-rebuild-error (list message data)))

(defun dorg-ext-rebuild--options (options)
  "Validate and return rebuild OPTIONS."
  (let ((original options))
    (unless (and (proper-list-p options) (zerop (% (length options) 2)))
      (dorg-ext-rebuild--raise
       "rebuild options must be a proper key/value list"))
    (let (seen)
      (while options
        (let ((key (pop options)))
          (unless (memq key dorg-ext-rebuild--option-keys)
            (dorg-ext-rebuild--raise "unknown rebuild option" :option key))
          (when (memq key seen)
            (dorg-ext-rebuild--raise "duplicate rebuild option" :option key))
          (push key seen)
          (pop options))))
    original))

(defun dorg-ext-rebuild--audit-options (options)
  "Validate the closed public option set for the whole-store audit."
  (dorg-ext-rebuild--options options)
  (dolist (key '(:dolt-dir :revision :identity))
    (unless (plist-member options key)
      (dorg-ext-rebuild--raise "whole-store audit option is required"
                               :reason 'option-missing :option key)))
  (when (plist-member options :mode)
    (dorg-ext-rebuild--raise "whole-store audit has no mode option"
                             :reason 'option-unsupported :option :mode))
  options)

(defun dorg-ext-rebuild--audit-revision (revision)
  "Return REVISION after refusing absent, empty, or non-commit tokens."
  (cond
   ((not (stringp revision))
    (dorg-ext-rebuild--raise "whole-store audit revision is required"
                             :reason 'revision-missing))
   ((string-empty-p revision)
    (dorg-ext-rebuild--raise "whole-store audit revision is empty"
                             :reason 'revision-empty))
   ;; Dolt commit hashes use a base32-like alphabet, not hexadecimal.
   ((not (string-match-p "\\`[[:alnum:]]+\\'" revision))
    (dorg-ext-rebuild--raise "whole-store audit revision is malformed"
                             :reason 'revision-malformed :revision revision))
   (t revision)))

(defun dorg-ext-rebuild--policy-upgrade-scope (scope)
  "Validate and return the closed policy-upgrade SCOPE codec.
The only v1 form is `(:root-id POSITIVE-INTEGER)'.  This is deliberately
stricter than a generic plist: duplicate, unknown, malformed, and child-alias
forms refuse before any projection or write can be attempted."
  (unless (and (proper-list-p scope) (zerop (% (length scope) 2)))
    (dorg-ext-rebuild--raise "policy upgrade scope is malformed"
                             :reason 'scope-malformed :scope scope))
  (let ((keys (cl-loop for (key _value) on scope by #'cddr collect key)))
    (unless (equal keys '(:root-id))
      (dorg-ext-rebuild--raise "policy upgrade scope must contain only :root-id"
                               :reason 'scope-malformed :scope scope))
    (let ((root-id (plist-get scope :root-id)))
      (unless (and (integerp root-id) (> root-id 0))
        (dorg-ext-rebuild--raise "policy upgrade scope root must be positive"
                                 :reason 'scope-root-invalid :scope scope))
      (list :root-id root-id))))

(defconst dorg-ext-rebuild--policy-upgrade-request-keys
  '(:dolt-dir :revision :identity :scope :old-producer :new-producer)
  "Closed public request keys for a policy-upgrade plan.")

(defun dorg-ext-rebuild--policy-upgrade-request (request)
  "Normalize the closed public policy-upgrade REQUEST before any store read."
  (unless (and (proper-list-p request) (zerop (% (length request) 2)))
    (dorg-ext-rebuild--raise "policy upgrade request is malformed"
                             :reason 'request-malformed))
  (let ((keys (cl-loop for (key _value) on request by #'cddr collect key)))
    (unless (and (= (length keys) (length dorg-ext-rebuild--policy-upgrade-request-keys))
                 (cl-every (lambda (key)
                             (= (cl-count key keys) 1))
                           dorg-ext-rebuild--policy-upgrade-request-keys)
                 (cl-every (lambda (key)
                             (memq key dorg-ext-rebuild--policy-upgrade-request-keys))
                           keys))
      (dorg-ext-rebuild--raise "policy upgrade request has invalid keys"
                               :reason 'request-malformed)))
  (let ((dolt-dir (plist-get request :dolt-dir))
        (revision (dorg-ext-rebuild--audit-revision (plist-get request :revision)))
        (identity (dorg-ext-rebuild--identity
                   (list :identity (plist-get request :identity))) )
        (scope (dorg-ext-rebuild--policy-upgrade-scope
                (plist-get request :scope)))
        (old (plist-get request :old-producer))
        (new (plist-get request :new-producer)))
    (unless (and (stringp dolt-dir) (not (string-empty-p dolt-dir)))
      (dorg-ext-rebuild--raise "policy upgrade request Dolt directory is invalid"
                               :reason 'request-malformed))
    (dolist (producer (list old new))
      (unless (dorg-ext-rebuild--policy-producer-identity-p producer)
        (dorg-ext-rebuild--raise "policy upgrade producer is malformed"
                                 :reason 'producer-malformed :producer producer)))
    (list :dolt-dir dolt-dir :revision revision :identity identity :scope scope
          :old-producer (copy-tree old) :new-producer (copy-tree new))))

(defun dorg-ext-rebuild--full-context (dolt-dir root-id revision identity &optional query-fn)
  "Build the create-only full-root rebuild context AS OF REVISION.
This is the documented administrative exception to mutation's bounded context:
there is no fabricated before image or replayed patch history."
  (let* ((candidate
          (dorg-ext--index-host-safe-image
           (dorg-model-load-headings dolt-dir root-id nil revision nil query-fn)))
         (ids (sort (hash-table-keys candidate) #'<))
         (base-image (dorg-ext--index-empty-image))
         (parts (dorg-ext--index-classify base-image candidate ids nil nil))
         ;; Import owns the canonical complete-image owner walk.  Rebuild has
         ;; the same complete S0/S1 shape, so calling it is a reuse, not a
         ;; second interpretation of nested property identity.
         (owner-partitions
          (dorg-import--index-owner-partitions
           identity root-id parts base-image candidate)))
    (unless (gethash root-id candidate)
      (dorg-ext-rebuild--raise "root is absent at requested revision"
                               :root-id root-id :revision revision))
    (dorg-ext-index-context-create
     :source-kind 'dolt :root-id root-id :dolt-dir dolt-dir
     :base-commit revision
     :candidate-id (dorg-ext--index-candidate-id candidate owner-partitions)
     :patches nil
     :created (plist-get parts :created)
     :changed nil :moved nil :deleted nil
     :base-image base-image
     :candidate-postimage candidate
     :owner-partitions owner-partitions)))

(defun dorg-ext-rebuild--identity (options)
  "Return required normalized rebuild identity from OPTIONS, or refuse early."
  (unless (and (plist-member options :identity)
               (plist-get options :identity))
    (dorg-ext-rebuild--raise "rebuild identity is required" :reason 'identity-missing))
  (let ((identity
         (condition-case err
             (dorg-mutate--normalize-identity (plist-get options :identity))
           (error
            (dorg-ext-rebuild--raise
             "invalid rebuild identity" :reason 'identity-malformed
             :underlying (error-message-string err))))))
    (when (or (null identity) (eq identity 'none))
      (dorg-ext-rebuild--raise "rebuild identity is unsupported" :reason 'identity-none))
    identity))

(defconst dorg-ext-rebuild--policy-classes
  '(unchanged upgrade already-new missing-new extra unexplained)
  "Closed complete-row policy-upgrade classes.")

(defun dorg-ext-rebuild--policy-producer-identity-p (identity)
  "Return non-nil when IDENTITY is one closed content-addressed producer id."
  (and (listp identity)
       (equal (sort (copy-sequence (cl-loop for (key _value) on identity by #'cddr
                                             collect key))
                    (lambda (left right)
                      (string< (symbol-name left) (symbol-name right))))
              '(:dependencies :implementation :policy-digest))
       (cl-every (lambda (key)
                   (let ((value (plist-get identity key)))
                     (and (stringp value) (not (string-empty-p value)))))
                 '(:implementation :dependencies :policy-digest))))

(defun dorg-ext-rebuild--resolve-policy-producer (ext request)
  "Resolve REQUEST through EXT's installed policy producer, or refuse.
REQUEST is DOrg-owned and must carry a closed content-addressed =:producer=.
The resolver returns only operation-local code/data.  DOrg copies the result,
computes the policy digest, and rejects any identity mismatch before an upgrade
can construct effects or open a transaction."
  (let ((requested (plist-get request :producer))
        (resolver (dorg-extension-producer-resolver ext)))
    (unless (dorg-ext-rebuild--policy-producer-identity-p requested)
      (dorg-ext-rebuild--raise "policy producer identity is malformed"
                               :reason 'producer-malformed))
    (unless resolver
      (dorg-ext-rebuild--raise "extension has no policy producer resolver"
                               :reason 'producer-resolver-missing
                               :extension (dorg-extension-name ext)))
    (let ((resolved
           (condition-case err
               (funcall resolver (copy-tree request))
             (error
              (dorg-ext-rebuild--raise "policy producer resolver failed"
                                       :reason 'producer-resolution-failed
                                       :underlying (error-message-string err))))))
      (unless (and (listp resolved)
                   (functionp (plist-get resolved :project))
                   (stringp (plist-get resolved :implementation))
                   (not (string-empty-p (plist-get resolved :implementation)))
                   (stringp (plist-get resolved :dependencies))
                   (not (string-empty-p (plist-get resolved :dependencies)))
                   (stringp (plist-get resolved :policy)))
        (dorg-ext-rebuild--raise "policy producer resolver returned malformed data"
                                 :reason 'producer-resolution-malformed))
      (let ((resolved-identity
             (list :implementation (plist-get resolved :implementation)
                   :dependencies (plist-get resolved :dependencies)
                   :policy-digest (secure-hash 'sha256 (plist-get resolved :policy)))))
        (unless (equal requested resolved-identity)
          (dorg-ext-rebuild--raise "policy producer identity does not match resolver"
                                   :reason 'producer-identity-mismatch
                                   :requested requested :resolved resolved-identity))
        (list :identity (copy-tree resolved-identity)
              :project (plist-get resolved :project)
              :policy (copy-sequence (plist-get resolved :policy)))))))

(defun dorg-ext-rebuild--policy-projection (ext producer context)
  "Run PRODUCER's installed projector over CONTEXT as complete inserted rows.
Policy producers describe a desired complete derived image, never caller SQL or
incremental deletes.  The shared manifest validator remains the single table
authority before row multisets reach policy classification."
  (let ((owned (dorg-ext--owned-tables ext))
        (rows nil)
        (effects
         (condition-case err
             (funcall (plist-get producer :project) context)
           (error
            (dorg-ext-rebuild--raise "policy producer projector failed"
                                     :reason 'producer-projection-failed
                                     :underlying (error-message-string err))))))
    (unless (listp effects)
      (dorg-ext-rebuild--raise "policy producer projector returned malformed effects"
                               :reason 'producer-projection-malformed))
    (dolist (effect effects)
      (unless (and (plist-member effect :insert)
                   (not (plist-member effect :delete)))
        (dorg-ext-rebuild--raise "policy producer must emit complete insert rows"
                                 :reason 'producer-projection-malformed
                                 :effect effect))
      (let ((table
             (condition-case err
                 (dorg-ext-validate-index-effect effect owned)
               (dorg-ext-index-error
                (dorg-ext-rebuild--raise
                 "policy producer emitted an invalid effect"
                 :reason 'producer-projection-malformed
                 :effect effect :underlying (dorg-error-data err))))))
        (push (cons table (plist-get effect :insert)) rows)))
    (nreverse rows)))

(defun dorg-ext-rebuild--policy-upgrade-canonical-root
    (dolt-dir root-id revision)
  "Require ROOT-ID to be a canonical root at frozen REVISION.
An in-tree child alias is never widened into a root upgrade scope."
  (let ((rows
         (dorg-sql
          dolt-dir
          (format (concat "SELECT id FROM headings%s WHERE id = %d "
                          "AND root_id = id AND parent_id IS NULL "
                          "AND level_offset = 0")
                  (dorg-sql-gen--as-of-suffix revision) root-id))))
    (unless (= (length rows) 1)
      (dorg-ext-rebuild--raise "policy upgrade scope root is not canonical at revision"
                               :reason 'scope-root-not-canonical
                               :root-id root-id :revision revision))
    root-id))

(defun dorg-ext-rebuild--policy-row-supported-p (row)
  "Return non-nil when stored ROW is a lossless complete row policy can compare.
ROW must be a proper non-empty list of cells with unique, identifier-safe
symbol columns whose values are SQL NULL (nil), an integer or a string.  Any
other value type (a float, a vector, ...) has no exact byte/NULL comparison
here and is unsupported."
  (and (consp row) (proper-list-p row)
       (cl-every (lambda (cell)
                   (and (consp cell) (car cell) (symbolp (car cell))
                        (dorg-ext--index-identifier-safe-p (symbol-name (car cell)))
                        (let ((value (cdr cell)))
                          (or (null value) (integerp value) (stringp value)))))
                 row)
       (= (length row) (length (delete-dups (mapcar #'car row))))))

(defun dorg-ext-rebuild--policy-image-row (row)
  "Return stored ROW as a canonical image row, or `(:unsupported . ROW)'.
An unsupported row is never canonicalized or compared; classification
reports it as unexplained, so its plan cannot be applicable."
  (if (dorg-ext-rebuild--policy-row-supported-p row)
      (dorg-ext-rebuild--canonical-row row)
    (cons :unsupported (copy-tree row))))

(defun dorg-ext-rebuild--policy-rows-by-table (projection descriptors)
  "Canonicalize complete PROJECTION rows into one alist entry per descriptor.
Missing tables become explicit empty multisets, so later classification cannot
mistake absent scope data for an omitted table declaration."
  (mapcar
   (lambda (descriptor)
     (let ((table (plist-get descriptor :name)))
       (cons table
             (mapcar #'dorg-ext-rebuild--policy-image-row
                     (mapcar #'cdr
                             (cl-remove-if-not
                              (lambda (entry) (string= table (car entry)))
                              projection))))))
   descriptors))

(defun dorg-ext-rebuild--policy-current-rows
    (dolt-dir descriptors root-id revision memberships)
  "Read and canonicalize each declared table in one frozen policy scope."
  (mapcar
   (lambda (descriptor)
     (cons (plist-get descriptor :name)
           (mapcar #'dorg-ext-rebuild--policy-image-row
                   (dorg-ext-rebuild--table-rows
                    dolt-dir descriptor root-id revision memberships))))
   descriptors))

(defconst dorg-ext-rebuild--policy-example-cap 8
  "Most diagnostic example occurrences a public table report keeps per class.
Counts, effects and digests always cover every occurrence; only the
human-facing examples are bounded, in canonical row order.")

(defun dorg-ext-rebuild--policy-table-report (table delta)
  "Return TABLE's bounded public report of its complete classification DELTA."
  (list :table table
        :applicable (plist-get delta :applicable)
        :class-counts (plist-get delta :class-counts)
        :delete-count (plist-get delta :delete-count)
        :insert-count (plist-get delta :insert-count)
        :examples (mapcar (lambda (entry)
                            (cons (car entry)
                                  (seq-take (cdr entry)
                                            dorg-ext-rebuild--policy-example-cap)))
                          (plist-get delta :classes))))

(defun dorg-ext-rebuild--policy-deltas
    (descriptors old-by-table new-by-table current-by-table)
  "Classify complete policy images for every descriptor as one transition.
An unexplained row in any declared table makes the entire transition
inapplicable and discards all otherwise valid effects.  `:tables' holds the
bounded public reports; `:write-count' counts complete-row occurrences
written (counted deletes plus inserts), not statements."
  (let (tables effects (occurrences 0))
    (dolist (descriptor descriptors)
      (let* ((table (plist-get descriptor :name))
             (delta (dorg-ext-rebuild--policy-delta
                     table (cdr (assoc table old-by-table))
                     (cdr (assoc table new-by-table))
                     (cdr (assoc table current-by-table)))))
        (push (dorg-ext-rebuild--policy-table-report table delta) tables)
        (when (plist-get delta :applicable)
          (setq effects (nconc effects (plist-get delta :deletes)
                               (plist-get delta :inserts))
                occurrences (+ occurrences (plist-get delta :delete-count)
                               (plist-get delta :insert-count))))))
    (setq tables (nreverse tables))
    (let ((applicable (cl-every (lambda (entry) (plist-get entry :applicable)) tables)))
      (list :tables tables :applicable applicable
            :effects (and applicable effects)
            :write-count (if applicable occurrences 0)))))

(defun dorg-ext-rebuild--policy-projection-effects (projection)
  "Reify complete PROJECTION entries as ordinary validated insert effects."
  (mapcar (lambda (entry) (list :table (car entry) :insert (cdr entry))) projection))

(defun dorg-ext-rebuild--policy-check-scope (effects descriptors root-id parent-keys)
  "Require every producer image row in EFFECTS to lie in ROOT-ID's scope.
This is ordinary rebuild's declared root/parent scope guard
\(`dorg-ext-rebuild--scope-effect') applied to both complete producer images.
Table ownership alone does not prove root ownership, and a complete image is
never silently filtered: an out-of-scope row refuses the whole plan."
  (dolist (effect effects t)
    (condition-case err
        (dorg-ext-rebuild--scope-effect effect descriptors root-id parent-keys)
      (dorg-ext-rebuild-error
       (dorg-ext-rebuild--raise "policy producer row is outside the requested scope"
                                :reason 'producer-scope-violation
                                :root-id root-id :effect effect
                                :underlying (dorg-error-data err))))))

(defun dorg-ext-rebuild--policy-class-counts (tables)
  "Aggregate closed per-table class counts from policy TABLES."
  (mapcar
   (lambda (class)
     (cons class
           (apply #'+ 0
                  (mapcar (lambda (table)
                            (or (cdr (assq class (plist-get table :class-counts))) 0))
                          tables))))
   dorg-ext-rebuild--policy-classes))

(defun dorg-ext-rebuild--policy-serialize (object)
  "Return OBJECT's complete printed form, independent of surrounding print settings.
Digests and canonical ordering must not change, or collapse unequal values,
because a caller bound `print-level', `print-length' or an escape option."
  (let ((print-level nil) (print-length nil) (print-circle nil)
        (print-escape-newlines nil) (print-escape-control-characters nil)
        (print-escape-nonascii nil) (print-escape-multibyte nil)
        (print-quoted t) (print-gensym nil) (print-integers-as-characters nil)
        (float-output-format nil))
    (prin1-to-string object)))

(defun dorg-ext-rebuild--policy-sort-rows (rows)
  "Return complete ROWS in canonical order, keeping every duplicate occurrence."
  (mapcar #'cdr
          (sort (mapcar (lambda (row)
                          (cons (dorg-ext-rebuild--policy-serialize row) row))
                        rows)
                (lambda (left right) (string< (car left) (car right))))))

(defun dorg-ext-rebuild--policy-digest (object)
  "Return the SHA-256 of OBJECT's complete, print-context-free serialization."
  (secure-hash 'sha256 (encode-coding-string
                        (dorg-ext-rebuild--policy-serialize object) 'utf-8-emacs t)))

(defun dorg-ext-rebuild--policy-image-digest (images)
  "Return a digest of complete-row IMAGES that depends only on their multisets.
IMAGES is an alist of table name to canonical rows.  Table and row order are
canonicalized here, so neither emit nor SELECT order can change the digest;
duplicate occurrences, every byte and SQL NULL versus empty content do."
  (dorg-ext-rebuild--policy-digest
   (mapcar (lambda (entry)
             (cons (car entry) (dorg-ext-rebuild--policy-sort-rows (cdr entry))))
           (sort (copy-sequence images)
                 (lambda (left right) (string< (car left) (car right)))))))

(defun dorg-ext-rebuild--policy-source (dolt-dir)
  "Return DOLT-DIR's normalized existing store identity, or refuse.
The identity is the canonical repository directory DOrg already uses for its
store targets.  It is bound into every plan, digest and receipt."
  (unless (and (stringp dolt-dir) (not (string-empty-p dolt-dir))
               (file-directory-p dolt-dir))
    (dorg-ext-rebuild--raise "policy upgrade source is not an existing store directory"
                             :reason 'source-invalid :source dolt-dir))
  (plist-get (dorg-dolt--target dolt-dir) :canonical-repo-dir))

(defun dorg-ext-rebuild--policy-plan-state (extension request)
  "Derive EXTENSION's policy-upgrade state for REQUEST, read-only, AS OF H.
Returns a plist: `:plan' (the public plan) plus the operation-local images
and scope facts that apply and inverse re-check against: `:old-images',
`:new-images', `:current-images', `:memberships', `:descriptors', `:root-id'
and `:source'.  Producer callbacks are resolved anew on every call and are
never part of the result."
  (let* ((request (dorg-ext-rebuild--policy-upgrade-request request))
         (source (dorg-ext-rebuild--policy-source (plist-get request :dolt-dir)))
         (revision (plist-get request :revision))
         (identity (plist-get request :identity))
         (scope (plist-get request :scope))
         (root-id (plist-get scope :root-id))
         (ext (dorg-get-extension extension)))
    (unless (and (symbolp extension) ext)
      (dorg-ext-rebuild--raise "policy upgrade extension is not registered"
                               :extension extension))
    (unless (memq 'dolt (dorg-ext--effective-capabilities ext))
      (dorg-ext-rebuild--raise "extension does not support Dolt policy upgrades"
                               :extension extension))
    (unless (dorg-commit-ancestor-p source revision)
      (dorg-ext-rebuild--raise "policy upgrade revision is not reachable from HEAD"
                               :reason 'revision-unreachable :revision revision))
    (dorg-ext-rebuild--policy-upgrade-canonical-root source root-id revision)
    (let* ((descriptors (dorg-ext-rebuild-scope-tables ext))
           (context (dorg-ext-rebuild--full-context
                     source root-id revision identity))
           (old-producer
            (dorg-ext-rebuild--resolve-policy-producer
             ext (list :producer (plist-get request :old-producer)
                       :source source :revision revision :identity identity
                       :scope scope)))
           (new-producer
            (dorg-ext-rebuild--resolve-policy-producer
             ext (list :producer (plist-get request :new-producer)
                       :source source :revision revision :identity identity
                       :scope scope)))
           (old-projection (dorg-ext-rebuild--policy-projection ext old-producer context))
           (new-projection (dorg-ext-rebuild--policy-projection ext new-producer context))
           (old-effects (dorg-ext-rebuild--policy-projection-effects old-projection))
           (new-effects (dorg-ext-rebuild--policy-projection-effects new-projection))
           ;; Each complete image must lie in the root on its OWN projected
           ;; parents: one producer's parent row never authorizes the other
           ;; producer's child.  Only after both checks does membership
           ;; discovery use the explicit union of projected parent keys.
           (_old-scoped
            (dorg-ext-rebuild--policy-check-scope
             old-effects descriptors root-id
             (dorg-ext-rebuild--parent-keys old-effects descriptors root-id)))
           (_new-scoped
            (dorg-ext-rebuild--policy-check-scope
             new-effects descriptors root-id
             (dorg-ext-rebuild--parent-keys new-effects descriptors root-id)))
           (parent-keys
            (dorg-ext-rebuild--parent-keys
             (append old-effects new-effects) descriptors root-id))
           (memberships
            (condition-case err
                (dorg-ext-rebuild--scope-memberships
                 source revision descriptors root-id parent-keys)
              (dorg-ext-rebuild-error
               (dorg-ext-rebuild--raise
                "policy upgrade parent scope is ambiguous or unreadable"
                :reason 'scope-parent-ambiguous
                :underlying (dorg-error-data err)))))
           (old-images (dorg-ext-rebuild--policy-rows-by-table old-projection descriptors))
           (new-images (dorg-ext-rebuild--policy-rows-by-table new-projection descriptors))
           (current-images
            (dorg-ext-rebuild--policy-current-rows
             source descriptors root-id revision memberships))
           (delta (dorg-ext-rebuild--policy-deltas
                   descriptors old-images new-images current-images))
           (current-digest (dorg-ext-rebuild--policy-image-digest current-images))
           (class-counts (dorg-ext-rebuild--policy-class-counts
                          (plist-get delta :tables)))
           (plan
            (list :ok t :kind 'policy-upgrade-plan :source source :extension extension
                  :base revision :scope scope
                  :identity identity :tables (plist-get delta :tables)
                  :old-producer (plist-get old-producer :identity)
                  :new-producer (plist-get new-producer :identity)
                  :class-counts class-counts :applicable (plist-get delta :applicable)
                  :effects (plist-get delta :effects)
                  :write-count (plist-get delta :write-count)
                  :current-digest current-digest)))
      (setq plan
            (append plan
                    (list :plan-digest
                          (dorg-ext-rebuild--policy-digest
                           (list :source source :extension extension
                                 :base revision :scope scope
                                 :identity identity
                                 :old-producer (plist-get old-producer :identity)
                                 :new-producer (plist-get new-producer :identity)
                                 :current-digest current-digest
                                 :tables (plist-get delta :tables)
                                 :effects (plist-get delta :effects))))))
      (list :plan plan :source source :descriptors descriptors :root-id root-id
            :memberships memberships :old-images old-images :new-images new-images
            :current-images current-images))))

;;;###autoload
(defun dorg-ext-rebuild-plan-policy-upgrade (extension request)
  "Plan EXTENSION's closed, content-addressed policy upgrade REQUEST.
Every projection and derived row image is frozen at REQUEST's revision.  This
is a read-only planner: an unexplained table makes the entire returned plan
inapplicable and contributes no effects.  The plan binds the normalized store
identity `:source', which apply re-derives against."
  (plist-get (dorg-ext-rebuild--policy-plan-state extension request) :plan))

(defun dorg-ext-rebuild--scope-column-symbol (descriptor)
  "Return DESCRIPTOR's validated scope column as a symbol."
  (intern (plist-get descriptor :scope-column)))

(defun dorg-ext-rebuild--scope-effect (effect descriptors root-id &optional parent-keys)
  "Return rebuild-safe EFFECT narrowed to ROOT-ID's declared scope.
The extension emitted EFFECT through the normal validated projector path.  This
administrative adapter adds the root conjunct to deletes and requires the same
root in inserts; it never changes mutation/import effect lowering."
  (let* ((table (plist-get effect :table))
         (descriptor (cl-find table descriptors
                              :key (lambda (d) (plist-get d :name))
                              :test #'string=))
         (column (and descriptor
                      (dorg-ext-rebuild--scope-column-symbol descriptor)))
         (parent (plist-get descriptor :scope-parent))
         (allowed (and parent parent-keys (gethash table parent-keys)))
         (op (dorg-ext--index-effect-op effect))
         (payload (copy-tree (cdr op)))
         (existing (and column (assq column payload))))
    (unless descriptor
      (dorg-ext-rebuild--raise "projector effect table is outside rebuild scope"
                               :table table))
    (pcase (car op)
      ('insert
       (unless (and existing
                    (if parent (member (cdr existing) allowed)
                      (equal (cdr existing) root-id)))
         (dorg-ext-rebuild--raise
          "rebuild projector insert must carry the requested scope"
          :table table :scope-column column :root-id root-id :effect effect))
       effect)
      ('delete
       (when (and existing (not (if parent (member (cdr existing) allowed)
                                  (equal (cdr existing) root-id))))
         (dorg-ext-rebuild--raise
          "rebuild projector delete names another scope"
          :table table :scope-column column :root-id root-id :effect effect))
       (when (and parent (not existing))
         (dorg-ext-rebuild--raise "rebuild projector child delete needs scope key"
                                  :table table :scope-column column :effect effect))
       (unless existing
         (setq payload (append payload (list (cons column root-id)))))
       (list :table table :delete payload))
      (_ (dorg-ext-rebuild--raise "invalid rebuild projector effect" :effect effect)))))

(defun dorg-ext-rebuild--clear-effects (descriptors root-id &optional parent-keys)
  "Return DOrg-owned declared-scope clears for DESCRIPTORS at ROOT-ID."
  (apply #'append
         (mapcar
          (lambda (descriptor)
            (let ((table (plist-get descriptor :name))
                  (column (dorg-ext-rebuild--scope-column-symbol descriptor)))
              (if (plist-get descriptor :scope-parent)
                  (mapcar (lambda (key) (list :table table :delete (list (cons column key))))
                          (and parent-keys (gethash table parent-keys)))
                (list (list :table table :delete (list (cons column root-id)))))))
          (sort (copy-sequence descriptors)
                (lambda (left right)
                  (and (plist-get left :scope-parent)
                       (not (plist-get right :scope-parent))))))))

(defun dorg-ext-rebuild--canonical-row (row)
  "Return ROW as a stable canonical column/value alist."
  (sort
   (mapcar (lambda (cell)
             (cons (symbol-name (car cell)) (cdr cell)))
           row)
   (lambda (a b) (string< (car a) (car b)))))

(defun dorg-ext-rebuild--row-multiset (rows)
  "Return the complete canonical ROWS as a multiset hash table.
Manifest `:key-column' names read-plane capability metadata, not a uniqueness
contract.  Rebuild comparison consequently uses every canonical column/value
pair and preserves duplicate complete rows by count."
  (let ((rows-by-value (make-hash-table :test 'equal)))
    (dolist (row rows)
      (puthash row (1+ (gethash row rows-by-value 0)) rows-by-value))
    rows-by-value))

(defconst dorg-ext-rebuild--audit-classes
  '(owned-canonical-root owned-stored-only-root orphan-child ambiguous undeclared-scope)
  "Closed whole-store ownership classes.")

(defun dorg-ext-rebuild--audit-empty-classes ()
  "Return a complete zero/empty class alist for an unread audit table."
  (mapcar (lambda (class) (cons class nil)) dorg-ext-rebuild--audit-classes))

(defun dorg-ext-rebuild--audit-empty-counts ()
  "Return a complete zero count alist for an unread audit table."
  (mapcar (lambda (class) (cons class 0)) dorg-ext-rebuild--audit-classes))

(defun dorg-ext-rebuild--audit-row-value (row column)
  "Return COLUMN's value from full schema ROW without collapsing nil."
  (cdr (assq (intern column) row)))

(defun dorg-ext-rebuild--audit-records (entries)
  "Return full-row multiplicity records from classified ENTRIES.
Each entry is a cons of canonical row and its resolved owner list."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((key (list (car entry) (cdr entry)))
             (old (gethash key counts)))
        (puthash key (1+ (or old 0)) counts)))
    (let (records)
      (maphash
       (lambda (key count)
         (push (list :row (car key) :multiplicity count :owners (cadr key)) records))
       counts)
      (sort records (lambda (left right)
                      (string< (prin1-to-string (plist-get left :row))
                               (prin1-to-string (plist-get right :row))))))))

(defun dorg-ext-rebuild--audit-parent-owners (descriptor descriptors rows)
  "Return child parent-key -> resolved root owners for DESCRIPTOR.
ROWS is an alist mapping declared table names to complete source row lists."
  (when-let* ((parent (plist-get descriptor :scope-parent))
              (table (plist-get parent :table))
              (key (plist-get parent :key-column))
              (parent-desc (cl-find table descriptors :key (lambda (d) (plist-get d :name))
                                    :test #'string=)))
    (let ((owners (make-hash-table :test 'equal))
          (root-column (plist-get parent-desc :scope-column)))
      (dolist (row (cdr (assoc table rows)))
        (let ((parent-key (dorg-ext-rebuild--audit-row-value row key))
              (root (dorg-ext-rebuild--audit-row-value row root-column)))
          (when (and parent-key root)
            (puthash parent-key
                     (delete-dups (cons root (gethash parent-key owners))) owners))))
      owners)))

(defun dorg-ext-rebuild--audit-classify-rows (descriptors rows canonical-roots &optional errors)
  "Classify complete declared ROWS under DESCRIPTORS and CANONICAL-ROOTS.
This pure seam performs no SQL.  It preserves complete row values and duplicate
occurrences so callers can independently account source rows before reporting."
  (let ((table-reports nil)
        (all-clean (null errors))
        (complete (null errors))
        (errors (copy-tree errors)))
    (dolist (descriptor descriptors)
      (let* ((table (plist-get descriptor :name))
             (scope (plist-get descriptor :scope-column))
             (parent (plist-get descriptor :scope-parent))
             (source-entry (assoc table rows))
             (source (cdr source-entry))
             (parent-unread
              (and parent
                   (not (assoc (plist-get parent :table) rows))))
             (by-class (mapcar (lambda (class) (cons class nil))
                                dorg-ext-rebuild--audit-classes))
             (parent-owners (and parent
                                 (dorg-ext-rebuild--audit-parent-owners
                                  descriptor descriptors rows))))
        (cond
         ((not source-entry)
            (progn
              (setq complete nil all-clean nil)
              (push (list :table table :reason 'unread) errors)
              (push (list :table table :rows-read nil :accounted-count 0
                          :class-counts (dorg-ext-rebuild--audit-empty-counts)
                          :classes (dorg-ext-rebuild--audit-empty-classes))
                    table-reports)))
         ;; A read child whose direct parent table was unread is retained with
         ;; its independently captured count, but it cannot honestly become an
         ;; orphan finding: its parent membership is unknown rather than empty.
         (parent-unread
          (setq complete nil all-clean nil)
          (push (list :table table :reason 'parent-unread
                      :parent-table (plist-get parent :table)) errors)
          (push (list :table table :rows-read (length source) :accounted-count 0
                      :class-counts (dorg-ext-rebuild--audit-empty-counts)
                      :classes (dorg-ext-rebuild--audit-empty-classes))
                table-reports))
         (t
          (dolist (row source)
            (let* ((value (dorg-ext-rebuild--audit-row-value row scope))
                   (owners
                    (if parent (and value (gethash value parent-owners))
                      (and value (list value))))
                   (class
                    (cond
                     ((null value) 'undeclared-scope)
                     ((and parent (null owners)) 'orphan-child)
                     ((and parent (> (length owners) 1)) 'ambiguous)
                     ((member (car owners) canonical-roots) 'owned-canonical-root)
                     (t 'owned-stored-only-root)))
                   (canonical (dorg-ext-rebuild--canonical-row row)))
              (push (cons canonical (sort (copy-sequence owners) #'<))
                    (alist-get class by-class))))
          (let* ((classes
                  (mapcar (lambda (entry)
                            (cons (car entry)
                                  (dorg-ext-rebuild--audit-records (cdr entry))))
                          by-class))
                 (counts (mapcar (lambda (entry)
                                   (cons (car entry)
                                         (apply #'+ (mapcar (lambda (record)
                                                             (plist-get record :multiplicity))
                                                           (cdr entry)))))
                                 classes))
                 (read (length source))
                 (accounted (apply #'+ (mapcar #'cdr counts))))
            (unless (= read accounted)
              (setq complete nil all-clean nil)
              (push (list :table table :reason 'accounting-mismatch) errors))
            (unless (and (zerop (alist-get 'owned-stored-only-root counts))
                         (zerop (alist-get 'orphan-child counts))
                         (zerop (alist-get 'ambiguous counts))
                         (zerop (alist-get 'undeclared-scope counts)))
              (setq all-clean nil))
            (push (list :table table :rows-read read :accounted-count accounted
                        :class-counts counts :classes classes)
                  table-reports))))))
    (list :ok t :complete complete :outcome (if complete 'complete 'incomplete)
          :clean (and complete all-clean) :errors (nreverse errors)
          :tables (nreverse table-reports))))

(defun dorg-ext-rebuild--audit-canonical-roots (query revision)
  "Return the canonical root ids at REVISION through QUERY."
  (mapcar (lambda (row) (alist-get 'id row))
          (funcall query
                   (format (concat "SELECT id FROM headings%s "
                                   "WHERE id = root_id AND parent_id IS NULL"
                                   " AND level_offset = 0 ORDER BY id")
                           (dorg-sql-gen--as-of-suffix revision)))))

(defun dorg-ext-rebuild--audit-table-rows (query descriptor revision)
  "Read every declared row for DESCRIPTOR at REVISION through QUERY."
  (funcall query
           (format "SELECT * FROM %s%s" (plist-get descriptor :name)
                   (dorg-sql-gen--as-of-suffix revision))))

(defun dorg-ext-rebuild--policy-group (rows)
  "Return complete ROW -> ROWS, preserving duplicate occurrences.

The complete typed row is the multiset identity.  Descriptor `:key-column'
metadata is useful for read grouping only; it must never collapse distinct
rows while planning a policy transition."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (row rows groups)
      (puthash row (cons row (gethash row groups)) groups))))

(defun dorg-ext-rebuild--policy-classify (old new current)
  "Classify complete typed-row multisets OLD, NEW, and CURRENT.

For every identical full row, conceptual occurrence ranks run from one through
the largest O/N/C multiplicity.  A rank is present on each side precisely when
it is at most that side's count.  This records the six policy classes without
inventing a physical row identity for identical duplicates.  An unexplained or
extra row refuses the complete transition; otherwise its write delta is C→N.
Nil row absence remains distinct from a present row containing SQL NULL."
  (let ((old-groups (dorg-ext-rebuild--policy-group old))
        (new-groups (dorg-ext-rebuild--policy-group new))
        (current-groups (dorg-ext-rebuild--policy-group current))
        (classes (mapcar (lambda (class) (cons class nil))
                         dorg-ext-rebuild--policy-classes))
        keys (delete-count 0) (insert-count 0) applicable)
    (dolist (groups (list old-groups new-groups current-groups))
      (maphash (lambda (key _rows) (cl-pushnew key keys :test #'equal)) groups))
    ;; Hash iteration follows insertion, i.e. emit/SELECT order; class records
    ;; feed the plan digest, so they are produced in canonical row order.
    (setq keys (dorg-ext-rebuild--policy-sort-rows keys))
    (dolist (key keys)
      (let* ((old-count (length (gethash key old-groups)))
             (new-count (length (gethash key new-groups)))
             (current-count (length (gethash key current-groups)))
             (projection-max (max old-count new-count)))
        (cond
         ;; A row in neither projection is always extra, independently of its
         ;; multiplicity.  It cannot be attributed to this policy transition.
         ;; An unsupported row has no exact comparison, whichever image (O, N
         ;; or C) holds it: every occurrence is unexplained.
         ((eq (car-safe key) :unsupported)
          (dotimes (_ (max old-count new-count current-count))
            (push (list :row (cdr key) :reason 'unsupported-row)
                  (alist-get 'unexplained classes))))
         ((zerop projection-max)
          (dotimes (rank current-count)
            (push (list :row key :rank (1+ rank) :old-count old-count
                        :new-count new-count :current-count current-count)
                  (alist-get 'extra classes))))
         ;; Current copies beyond both projections are unexplained.  Keep the
         ;; in-range ranks below, so diagnostics show the exact excess.
         (t
          (dotimes (rank projection-max)
            (let* ((occurrence (1+ rank))
                   (old-present (<= occurrence old-count))
                   (new-present (<= occurrence new-count))
                   (current-present (<= occurrence current-count))
                   (class
                    (cond
                     ((and old-present new-present current-present) 'unchanged)
                     ((and old-present current-present (not new-present)) 'upgrade)
                     ((and new-present current-present (not old-present)) 'already-new)
                     (new-present 'missing-new)
                     ((and old-present (not new-present) (not current-present))
                      'already-new)
                     ;; All remaining in-range combinations violate the
                     ;; complete-row rank algebra and cannot produce effects.
                     (t 'unexplained))))
              (push (list :row key :rank occurrence :old-count old-count
                          :new-count new-count :current-count current-count)
                    (alist-get class classes))))
          (dotimes (rank (max 0 (- current-count projection-max)))
            (push (list :row key :rank (+ projection-max 1 rank)
                        :old-count old-count :new-count new-count
                        :current-count current-count)
                  (alist-get 'unexplained classes)))))))
    (let ((records
           (mapcar (lambda (entry) (cons (car entry) (nreverse (cdr entry)))) classes)))
      (setq applicable (and (null (alist-get 'extra records))
                            (null (alist-get 'unexplained records))))
      (when applicable
        (dolist (key keys)
          (let ((new-count (length (gethash key new-groups)))
                (current-count (length (gethash key current-groups))))
            (setq delete-count (+ delete-count (max 0 (- current-count new-count)))
                  insert-count (+ insert-count (max 0 (- new-count current-count)))))))
      (list :classes records :applicable applicable
            :delete-count delete-count :insert-count insert-count
            :class-counts (mapcar (lambda (entry) (cons (car entry) (length (cdr entry))))
                                  records)))))

(defun dorg-ext-rebuild--policy-effect-row (row)
  "Return canonical image ROW as a typed effect row with symbol columns.
This is the single boundary between the two internal row forms: complete-row
images and their classification use string column names (see
`dorg-ext-rebuild--canonical-row'), while every effect handed to the closed
validator and lowerer uses symbol columns.  The validator stays unrelaxed and
rejects any unsafe column name produced here."
  (mapcar (lambda (cell)
            (unless (stringp (car cell))
              (dorg-ext-rebuild--raise "policy image row is not canonical"
                                       :reason 'policy-row-not-canonical :row row))
            (cons (intern (car cell)) (cdr cell)))
          row))

(defun dorg-ext-rebuild--policy-delta (table old new current)
  "Return TABLE's applicable counted transition from CURRENT to NEW.
OLD, NEW and CURRENT are canonical image rows (string columns); the returned
deletes and inserts are typed effects (symbol columns).

OLD is deliberately retained for explainability and diagnostics, not as the
write source.  Deletes carry an exact positive :count for the private counted
lowerer; inserts retain one typed complete row per missing occurrence."
  (let* ((report (dorg-ext-rebuild--policy-classify old new current))
         (old-groups (dorg-ext-rebuild--policy-group old))
         (new-groups (dorg-ext-rebuild--policy-group new))
         (current-groups (dorg-ext-rebuild--policy-group current))
         (keys nil) deletes inserts)
    (dolist (groups (list old-groups new-groups current-groups))
      (maphash (lambda (key _rows) (cl-pushnew key keys :test #'equal)) groups))
    (when (plist-get report :applicable)
      (dolist (key (dorg-ext-rebuild--policy-sort-rows keys))
        (let ((new-count (length (gethash key new-groups)))
              (current-count (length (gethash key current-groups)))
              (row (dorg-ext-rebuild--policy-effect-row key)))
          (when (> current-count new-count)
            (push (list :table table :delete row :count (- current-count new-count))
                  deletes))
          (dotimes (_ (max 0 (- new-count current-count)))
            (push (list :table table :insert (copy-tree row)) inserts)))))
    (append report (list :deletes (nreverse deletes) :inserts (nreverse inserts)))))

(defun dorg-ext-rebuild--policy-delete-predicate (row)
  "Return ROW's complete, NULL-safe, byte-exact SQL predicate.

ROW has already passed the closed typed-effect validator.  String cells use
the binary operator on both operands so collation cannot merge a case, trailing
space, or distinct byte sequence.  Numbers retain numeric comparison; nil is
SQL NULL and therefore needs the NULL-safe equality operator as well."
  (mapconcat
   (lambda (cell)
     (let ((column (symbol-name (car cell)))
           (value (cdr cell)))
       (cond
        ((null value) (format "%s <=> NULL" column))
        ((stringp value)
         (format "BINARY %s <=> BINARY %s" column (dorg-ext--index-quote value)))
        (t (format "%s <=> %s" column (dorg-ext--index-quote value))))))
   row " AND "))

(defun dorg-ext-rebuild--policy-delete-sql (effect owned count)
  "Lower a counted complete-row delete EFFECT over OWNED tables.

COUNT must be positive.  The policy transition needs this private lowerer
because the ordinary `:delete' effect intentionally deletes every matching
row; using it for duplicate rows would silently over-delete."
  (unless (and (integerp count) (> count 0))
    (dorg-ext-rebuild--raise "policy delete count must be positive"
                             :reason 'policy-delete-count :count count))
  (let ((table (dorg-ext-validate-index-effect effect owned))
        (row (plist-get effect :delete)))
    (unless row
      (dorg-ext-rebuild--raise "policy delete requires a complete row predicate"
                               :reason 'policy-delete-whole-table :effect effect))
    (format "DELETE FROM %s WHERE %s LIMIT %d"
            table (dorg-ext-rebuild--policy-delete-predicate row) count)))

(defun dorg-ext-rebuild--policy-delta-sql (delta owned)
  "Lower applicable policy DELTA through DOrg-owned typed-effect guards.

DELTA is the private result of `dorg-ext-rebuild--policy-delta'.  Its deletes
must retain their positive multiplicity and therefore cannot use the ordinary
mass-delete lowerer; its inserts are ordinary validated typed effects."
  (append
   (mapcar (lambda (effect)
             (dorg-ext-rebuild--policy-delete-sql
              (list :table (plist-get effect :table)
                    :delete (plist-get effect :delete))
              owned (plist-get effect :count)))
           (plist-get delta :deletes))
   (mapcar (lambda (effect)
             (dorg-ext-validate-index-effect effect owned)
             (dorg-ext--index-effect-sql effect))
           (plist-get delta :inserts))))

(defun dorg-ext-rebuild--table-rows-sql (descriptor root-id revision &optional memberships)
  "Return the SELECT for DESCRIPTOR's ROOT-ID scope AS OF REVISION, or nil.
A nil REVISION reads the current working set (inside a transaction).  nil is
returned for a parent-scoped table whose membership is empty."
  (let ((table (plist-get descriptor :name))
        (scope (plist-get descriptor :scope-column))
        (parent (plist-get descriptor :scope-parent)))
    (if (not parent)
        (format "SELECT * FROM %s%s WHERE %s = %d"
                table (dorg-sql-gen--as-of-suffix revision) scope root-id)
      (let ((keys (and memberships (gethash table memberships))))
        (and keys
             (format "SELECT * FROM %s%s WHERE %s IN (%s)" table
                     (dorg-sql-gen--as-of-suffix revision) scope
                     (mapconcat #'dorg-ext--index-quote keys ",")))))))

(defun dorg-ext-rebuild--table-rows (dolt-dir descriptor root-id revision &optional memberships)
  "Read DESCRIPTOR's declared ROOT-ID scope AS OF REVISION."
  (let ((sql (dorg-ext-rebuild--table-rows-sql descriptor root-id revision memberships)))
    (and sql (dorg-sql dolt-dir sql))))

(defun dorg-ext-rebuild--parent-keys (effects descriptors root-id)
  "Return expected directly scoped parent keys keyed by child table name."
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (descriptor descriptors keys)
      (when-let ((parent (plist-get descriptor :scope-parent)))
        (let* ((values nil) (parent-table (plist-get parent :table))
               (parent-key (intern (plist-get parent :key-column)))
               (parent-descriptor
                (cl-find parent-table descriptors
                         :key (lambda (d) (plist-get d :name)) :test #'string=))
               (parent-root (intern (plist-get parent-descriptor :scope-column))))
          (dolist (effect effects)
            (when (and (string= (plist-get effect :table) parent-table)
                       (plist-member effect :insert)
                       (equal (cdr (assq parent-root (plist-get effect :insert))) root-id))
              (push (cdr (assq parent-key (plist-get effect :insert))) values)))
          (puthash (plist-get descriptor :name) (delete-dups values) keys))))))

(defun dorg-ext-rebuild--scope-memberships (dolt-dir revision descriptors root-id expected)
  "Return child-table M sets: stored root keys union projected root keys.
Reject a key which names a parent under another root before audit or repair."
  (let ((memberships (make-hash-table :test 'equal))
        (parent-memberships (make-hash-table :test 'equal))
        (seen-parents (make-hash-table :test 'equal))
        (suffix (dorg-sql-gen--as-of-suffix revision)))
    (dolist (descriptor descriptors memberships)
      (when-let* ((parent (plist-get descriptor :scope-parent))
                  (parent-desc (cl-find (plist-get parent :table) descriptors
                                        :key (lambda (d) (plist-get d :name)) :test #'string=)))
        (let* ((table (plist-get parent :table))
               (key (plist-get parent :key-column))
               (all (gethash table parent-memberships)))
          (unless (gethash table seen-parents)
            (let* ((root-column (plist-get parent-desc :scope-column))
                   (stored (mapcar (lambda (row) (alist-get (intern key) row))
                                   (dorg-sql dolt-dir
                                             (format "SELECT %s FROM %s%s WHERE %s = %d"
                                                     key table suffix root-column root-id))))
                   (projected (gethash (plist-get descriptor :name) expected)))
              ;; `delete-dups' is destructive.  PROJECTED is also the allowlist
              ;; used by `scope-effect', so its tail must not be shared here.
              (setq all (delete-dups (append stored (copy-sequence projected))))
              ;; One ambiguity query per parent table.  Parent keys are often
              ;; shared by many child tables, so checking them per child/key
              ;; becomes needlessly serial at tracker scale.
              (when (and all
                         (dorg-sql dolt-dir
                                   (format "SELECT %s FROM %s%s WHERE %s IN (%s) AND %s != %d"
                                           key table suffix key
                                           (mapconcat #'dorg-ext--index-quote all ",")
                                           root-column root-id)))
                (dorg-ext-rebuild--raise "rebuild parent scope key is ambiguous"
                                         :table table :keys all :root-id root-id))
              (puthash table all parent-memberships)))
          (puthash table t seen-parents)
          (puthash (plist-get descriptor :name) all memberships))))))

(defun dorg-ext-rebuild--expected-rows (effects table)
  "Return canonical insert rows for TABLE from full-snapshot EFFECTS."
  (mapcar #'dorg-ext-rebuild--canonical-row
          (mapcar (lambda (effect) (plist-get effect :insert))
                  (cl-remove-if-not
                   (lambda (effect)
                     (and (string= table (plist-get effect :table))
                          (plist-member effect :insert)))
                   effects))))

(defun dorg-ext-rebuild--drift (dolt-dir descriptors root-id revision effects &optional memberships)
  "Return bounded full-row multiset drift for DESCRIPTORS at ROOT-ID.
EFFECTS are already validated and narrowed rebuild effects."
  (let ((missing 0) (extra 0) (different 0) (examples nil))
    (dolist (descriptor descriptors)
      (let* ((table (plist-get descriptor :name))
             (expected (dorg-ext-rebuild--expected-rows effects table))
             (current (mapcar #'dorg-ext-rebuild--canonical-row
                              (dorg-ext-rebuild--table-rows
                               dolt-dir descriptor root-id revision memberships)))
             (current-by-row (dorg-ext-rebuild--row-multiset current)))
        (dolist (row expected)
          (let ((count (gethash row current-by-row 0)))
            (if (> count 0)
                (puthash row (1- count) current-by-row)
              (cl-incf missing)
              (when (< (length examples) 12)
                (push (list :table table :kind 'missing :row row) examples)))))
        (maphash
         (lambda (row count)
           (dotimes (_ count)
             (cl-incf extra)
             (when (< (length examples) 12)
               (push (list :table table :kind 'extra :row row) examples))))
         current-by-row)))
    (append (list :missing-count missing :extra-count extra
                  :differing-count different)
            (when examples (list :examples (nreverse examples))))))

(defun dorg-ext-rebuild--drift-empty-p (drift)
  "Return non-nil when DRIFT satisfies the existing zero-effect law."
  (and (zerop (plist-get drift :missing-count))
       (zerop (plist-get drift :extra-count))
       (zerop (plist-get drift :differing-count))))

(defun dorg-ext-rebuild--repair-safe-p (drift)
  "Return non-nil only when DRIFT's full-row comparison explains every
current row occurrence.
`dorg-ext-rebuild--drift' computes a total canonical-row multiset comparison;
an extra or different current row would be removed by scope replacement without
projector proof, so repair refuses it.  Missing expected rows are safe to add."
  (and (zerop (plist-get drift :extra-count))
       (zerop (plist-get drift :differing-count))))

(defun dorg-ext-rebuild--audit-store-with-query
    (extension ext revision query-function)
  "Audit validated EXTENSION state through explicit QUERY-FUNCTION.
The public boundary owns request and history validation.  This private worker
accepts the read port only so focused controls can observe every audit SELECT
without a global dynamic variable."
  (let* ((descriptors (dorg-ext-rebuild-scope-tables ext))
           (statement-count 0)
           (query (lambda (sql)
                    (cl-incf statement-count)
                    (funcall query-function sql)))
           (canonical-roots nil)
           (rows nil)
           (errors nil))
      (condition-case err
          (setq canonical-roots
                (dorg-ext-rebuild--audit-canonical-roots query revision))
        (error
         (setq errors (list (list :table 'core :reason 'canonical-root-read
                                  :underlying (error-message-string err))))))
      ;; This is an ownership census: canonical root identity, with import's
      ;; level-offset rule, is its complete canonical owner partition.  It
      ;; deliberately does not load projected heading images.
      (dolist (descriptor descriptors)
        (condition-case err
            (push (cons (plist-get descriptor :name)
                        (dorg-ext-rebuild--audit-table-rows query descriptor revision))
                  rows)
          (error
           (push (list :table (plist-get descriptor :name) :reason 'table-read
                       :underlying (error-message-string err))
                 errors))))
      (let ((report (dorg-ext-rebuild--audit-classify-rows
                     descriptors rows canonical-roots errors)))
        (append (list :extension extension :mode 'audit-store
                      :observed-core-revision revision :scope descriptors
                      :read-only t :statement-count statement-count)
                report))))

;;;###autoload
(defun dorg-ext-rebuild-audit-store (extension &rest options)
  "Audit every declared derived row for EXTENSION at an exact revision.
OPTIONS require `:dolt-dir', `:revision', and `:identity'.  This is a
read-only ownership census, not a projection-drift check or repair operation.
It returns an incomplete report when an audit data read fails; invalid inputs
and an invalid revision remain typed refusals before any derived-table read."
  (let* ((options (dorg-ext-rebuild--audit-options options))
         (dolt-dir (plist-get options :dolt-dir))
         (revision (dorg-ext-rebuild--audit-revision
                    (plist-get options :revision)))
         (ext (dorg-get-extension extension)))
    (dorg-ext-rebuild--identity options)
    (unless (and (symbolp extension) ext (dorg-extension-project ext))
      (dorg-ext-rebuild--raise "extension has no registered index projector"
                               :extension extension))
    (unless (and (stringp dolt-dir) (not (string-empty-p dolt-dir)))
      (dorg-ext-rebuild--raise "invalid whole-store audit request"
                               :extension extension :reason 'request-malformed))
    (unless (memq 'dolt (dorg-ext--effective-capabilities ext))
      (dorg-ext-rebuild--raise "extension does not support Dolt rebuild"
                               :extension extension))
    ;; This public boundary deliberately validates H before opening the audit
    ;; read port.  `dorg-commit-ancestor-p' is a history validation probe, not
    ;; an audit data read and is therefore excluded from :statement-count.
    (unless (dorg-commit-ancestor-p dolt-dir revision)
      (dorg-ext-rebuild--raise "revision is not reachable from HEAD"
                               :revision revision :reason 'revision-unreachable))
    (dorg-ext-rebuild--audit-store-with-query
     extension ext revision
     (lambda (sql) (dorg-sql dolt-dir sql)))))

;;; ─── Policy upgrade: apply, reconciliation and exact inverse ───────────

(defconst dorg-ext-rebuild--policy-plan-keys
  '(:ok :kind :source :extension :base :scope :identity :tables :old-producer
    :new-producer :class-counts :applicable :effects :write-count
    :current-digest :plan-digest)
  "Closed key order of a policy-upgrade plan.")

(defconst dorg-ext-rebuild--policy-receipt-keys
  '(:ok :kind :source :extension :base :post-head :scope :tables :identity
    :old-producer :new-producer :preimage-digest :postimage-digest :plan-digest
    :write-count)
  "Closed key order of a forward policy-upgrade receipt (`:no-op' may follow).")

(defun dorg-ext-rebuild--policy-keys (record)
  "Return RECORD's plist keys in order, or nil when RECORD is not a plist."
  (and (proper-list-p record) (zerop (% (length record) 2))
       (cl-loop for (key _value) on record by #'cddr collect key)))

(defun dorg-ext-rebuild--policy-request-of (record)
  "Return the planning request bound by plan or receipt RECORD."
  (list :dolt-dir (plist-get record :source) :revision (plist-get record :base)
        :identity (plist-get record :identity) :scope (plist-get record :scope)
        :old-producer (plist-get record :old-producer)
        :new-producer (plist-get record :new-producer)))

(defun dorg-ext-rebuild--policy-gate-check (source base)
  "Under SOURCE's store gate require HEAD = BASE and a clean working set.
The same canonical lock as `dorg-transaction-commit', so a no-op or a refusal
observes HEAD and the working set atomically and opens no transaction."
  (dorg-mutate-call-with-store-gate
   source
   (lambda (lease _key)
     (let ((head (dorg-sql--lease-scalar lease "SELECT HASHOF('HEAD') AS h")))
       (unless (equal head base)
         (dorg-ext-rebuild--raise "policy upgrade base is not the current HEAD"
                                  :reason 'stale-head :base base :head head)))
     (when (dorg-sql--lease-exec lease "SELECT table_name FROM dolt_status LIMIT 1")
       (dorg-ext-rebuild--raise "policy upgrade refuses a dirty working set"
                                :reason 'dirty-working-set :source source)))))

(defun dorg-ext-rebuild--policy-read-images (query descriptors root-id memberships
                                                   &optional revision)
  "Read DESCRIPTORS' ROOT-ID scope through QUERY as canonical images.
QUERY is a function of one SQL string.  REVISION nil reads the working set."
  (mapcar (lambda (descriptor)
            (let ((sql (dorg-ext-rebuild--table-rows-sql
                        descriptor root-id revision memberships)))
              (cons (plist-get descriptor :name)
                    (and sql (mapcar #'dorg-ext-rebuild--policy-image-row
                                     (funcall query sql))))))
          descriptors))

(defun dorg-ext-rebuild--policy-unscoped-digest (query descriptors scoped revision)
  "Digest every DESCRIPTORS row outside the SCOPED images, read via QUERY.
Each table is read whole AS OF REVISION (nil: working set) and the scoped
complete-row multiset is removed, so unrelated roots must match byte for
byte."
  (dorg-ext-rebuild--policy-image-digest
   (mapcar
    (lambda (descriptor)
      (let* ((table (plist-get descriptor :name))
             (all (mapcar #'dorg-ext-rebuild--policy-image-row
                          (funcall query (format "SELECT * FROM %s%s" table
                                                 (dorg-sql-gen--as-of-suffix revision)))))
             (counts (dorg-ext-rebuild--row-multiset (cdr (assoc table scoped))))
             rest)
        (dolist (row all)
          (if (> (gethash row counts 0) 0)
              (puthash row (1- (gethash row counts)) counts)
            (push row rest)))
        (cons table rest)))
    descriptors)))

(defun dorg-ext-rebuild--policy-sql-false-p (value)
  "Return non-nil only when VALUE is an explicit SQL boolean false.
A missing, NULL or unrecognized value is not false: the proof fails closed."
  (or (eql value 0) (equal value "0") (eq value :false) (eq value :json-false)
      (equal value "false")))

(defun dorg-ext-rebuild--policy-commit-proven-p (state commit post-images)
  "Return non-nil when COMMIT is provably the authorized transition of STATE.
Read-only: COMMIT's only parent is the base, its diff touches only the scope
tables with no schema change in any of them, its scoped derived images equal
POST-IMAGES and every row outside the scope is unchanged from the base.  Any
failed read is no proof."
  (condition-case nil
      (let* ((source (plist-get state :source))
             (plan (plist-get state :plan))
             (base (plist-get plan :base))
             (descriptors (plist-get state :descriptors))
             (tables (mapcar (lambda (d) (plist-get d :name)) descriptors))
             (query (lambda (sql) (dorg-sql source sql)))
             (parents (mapcar (lambda (row) (alist-get 'parent_hash row))
                              (dorg-sql source
                                        (format "SELECT parent_hash FROM dolt_commit_ancestors WHERE commit_hash = %s"
                                                (dorg-sql-quote commit)))))
             (diff (dorg-sql source
                             (format "SELECT table_name, schema_change FROM dolt_diff WHERE commit_hash = %s"
                                     (dorg-sql-quote commit))))
             (changed (mapcar (lambda (row) (alist-get 'table_name row)) diff)))
        (and (equal parents (list base))
             changed
             (cl-every (lambda (table) (member table tables)) changed)
             ;; Identical rows are not enough: the schema of every changed
             ;; table must be untouched.  Only an explicit SQL false passes.
             (cl-every (lambda (row)
                         (dorg-ext-rebuild--policy-sql-false-p (alist-get 'schema_change row)))
                       diff)
             (equal (dorg-ext-rebuild--policy-image-digest post-images)
                    (dorg-ext-rebuild--policy-image-digest
                     (dorg-ext-rebuild--policy-read-images
                      query descriptors (plist-get state :root-id)
                      (plist-get state :memberships) commit)))
             (equal (dorg-ext-rebuild--policy-unscoped-digest
                     query descriptors (plist-get state :current-images) base)
                    (dorg-ext-rebuild--policy-unscoped-digest
                     query descriptors post-images commit))))
    (error nil)))

(defun dorg-ext-rebuild--policy-commit (state statements message post-images)
  "Commit STATEMENTS at STATE's base, validating POST-IMAGES in the transaction.
The validator re-reads the scoped working set and every row outside the scope,
so an incomplete or overreaching write rolls the whole transaction back.
Returns the new HEAD, or reconciles a lost reply (see
`dorg-ext-rebuild--policy-reconcile')."
  (let* ((source (plist-get state :source))
         (base (plist-get (plist-get state :plan) :base))
         (descriptors (plist-get state :descriptors))
         (expected (dorg-ext-rebuild--policy-image-digest post-images))
         (outside (dorg-ext-rebuild--policy-unscoped-digest
                   (lambda (sql) (dorg-sql source sql)) descriptors
                   (plist-get state :current-images) base))
         (validate
          (lambda (query)
            (let ((post (dorg-ext-rebuild--policy-read-images
                         query descriptors (plist-get state :root-id)
                         (plist-get state :memberships))))
              (unless (and (equal expected (dorg-ext-rebuild--policy-image-digest post))
                           (equal outside (dorg-ext-rebuild--policy-unscoped-digest
                                           query descriptors post nil)))
                (dorg-ext-rebuild--raise
                 "policy upgrade postimage does not match its plan"
                 :reason 'postimage-mismatch))))))
    (condition-case err
        (dorg-transaction-commit source base statements message validate)
      (dorg-transaction-indeterminate
       (dorg-ext-rebuild--policy-reconcile state err post-images)))))

(defun dorg-ext-rebuild--policy-reconcile (state err post-images)
  "Reconcile the indeterminate transaction ERR for STATE, read-only.
A commit that `dorg-ext-rebuild--policy-commit-proven-p' proves is returned as
the new HEAD.  HEAD still at the base with no reported commit stays
indeterminate as `not-committed' with a re-derived `:next-plan'; anything else
stays indeterminate as `ambiguous'.  Nothing is retried or written."
  (let* ((source (plist-get state :source))
         (plan (plist-get state :plan))
         (base (plist-get plan :base))
         (facts (cddr err))
         (head (condition-case nil (dorg-head-commit source) (error nil)))
         (candidate (or (plist-get facts :commit)
                        (and head (not (equal head base)) head))))
    (cond
     ((and candidate (equal candidate head)
           (dorg-ext-rebuild--policy-commit-proven-p state candidate post-images))
      candidate)
     ((and (null candidate) (equal head base))
      (signal 'dorg-transaction-indeterminate
              (append (list (cadr err)) facts
                      (list :reconciled 'not-committed
                            :next-plan
                            (condition-case nil
                                (dorg-ext-rebuild-plan-policy-upgrade
                                 (plist-get plan :extension)
                                 (dorg-ext-rebuild--policy-request-of plan))
                              (error nil))))))
     (t
      (signal 'dorg-transaction-indeterminate
              (append (list (cadr err)) facts
                      (list :reconciled 'ambiguous :head head)))))))

(defun dorg-ext-rebuild--policy-split-effects (effects)
  "Return EFFECTS as the counted delta plist the lowerer consumes."
  (list :deletes (cl-remove-if-not (lambda (e) (plist-member e :delete)) effects)
        :inserts (cl-remove-if-not (lambda (e) (plist-member e :insert)) effects)))

;;;###autoload
(defun dorg-ext-rebuild-apply-policy-upgrade (extension plan)
  "Apply EXTENSION's policy-upgrade PLAN exactly, or refuse before writing.
PLAN must be a complete plan from `dorg-ext-rebuild-plan-policy-upgrade'.  It
is re-derived at its base against its bound `:source', with producers resolved
anew; any difference (edited effects even with a recomputed digest, changed
producer identity, drifted current rows) refuses.  An all-NEW plan returns an
explicit no-op receipt without a transaction.  Otherwise the C-to-N delta is
committed at the base in one validated transaction and the self-contained
forward receipt is returned."
  (unless (equal dorg-ext-rebuild--policy-plan-keys
                 (dorg-ext-rebuild--policy-keys plan))
    (dorg-ext-rebuild--raise "policy upgrade plan is malformed" :reason 'plan-malformed))
  (unless (and (eq t (plist-get plan :ok))
               (eq 'policy-upgrade-plan (plist-get plan :kind))
               (eq extension (plist-get plan :extension)))
    (dorg-ext-rebuild--raise "policy upgrade plan is malformed" :reason 'plan-malformed))
  (let* ((state (dorg-ext-rebuild--policy-plan-state
                 extension (dorg-ext-rebuild--policy-request-of plan)))
         (fresh (plist-get state :plan)))
    (unless (equal fresh plan)
      (dorg-ext-rebuild--raise "policy upgrade plan does not match its re-derivation"
                               :reason 'plan-mismatch
                               :plan-digest (plist-get plan :plan-digest)
                               :rederived-digest (plist-get fresh :plan-digest)))
    (unless (plist-get fresh :applicable)
      (dorg-ext-rebuild--raise "policy upgrade plan is not applicable"
                               :reason 'plan-inapplicable))
    (let* ((source (plist-get state :source))
           (base (plist-get fresh :base))
           (post-images (plist-get state :new-images))
           (owned (dorg-ext--owned-tables (dorg-get-extension extension)))
           (no-op (zerop (plist-get fresh :write-count))))
      (dorg-ext-rebuild--policy-gate-check source base)
      (let ((post-head
             (if no-op base
               (dorg-ext-rebuild--policy-commit
                state
                (dorg-ext-rebuild--policy-delta-sql
                 (dorg-ext-rebuild--policy-split-effects (plist-get fresh :effects))
                 owned)
                (format "dorg: policy upgrade %s root %d"
                        extension (plist-get state :root-id))
                post-images))))
        (append
         (list :ok t :kind 'policy-upgrade-forward :source source :extension extension
               :base base :post-head post-head :scope (plist-get fresh :scope)
               :tables (mapcar (lambda (d) (plist-get d :name))
                               (plist-get state :descriptors))
               :identity (plist-get fresh :identity)
               :old-producer (plist-get fresh :old-producer)
               :new-producer (plist-get fresh :new-producer)
               :preimage-digest (plist-get fresh :current-digest)
               :postimage-digest (dorg-ext-rebuild--policy-image-digest post-images)
               :plan-digest (plist-get fresh :plan-digest)
               :write-count (plist-get fresh :write-count))
         (and no-op (list :no-op t)))))))

(defun dorg-ext-rebuild--policy-digest-p (value)
  "Return non-nil when VALUE is a lowercase SHA-256 hex digest."
  (and (stringp value) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value)))

(defun dorg-ext-rebuild--policy-receipt-input (extension receipt)
  "Validate the closed forward RECEIPT shape for EXTENSION, or refuse."
  (let ((keys (dorg-ext-rebuild--policy-keys receipt))
        (malformed (lambda ()
                     (dorg-ext-rebuild--raise "policy upgrade receipt is malformed"
                                              :reason 'receipt-malformed))))
    (unless (or (equal keys dorg-ext-rebuild--policy-receipt-keys)
                (equal keys (append dorg-ext-rebuild--policy-receipt-keys '(:no-op))))
      (funcall malformed))
    (unless (and (eq t (plist-get receipt :ok))
                 (eq 'policy-upgrade-forward (plist-get receipt :kind))
                 (eq extension (plist-get receipt :extension))
                 (stringp (plist-get receipt :source))
                 (cl-every (lambda (key)
                             (let ((value (plist-get receipt key)))
                               (and (stringp value) (not (string-empty-p value)))))
                           '(:base :post-head))
                 (cl-every #'dorg-ext-rebuild--policy-digest-p
                           (mapcar (lambda (key) (plist-get receipt key))
                                   '(:preimage-digest :postimage-digest :plan-digest)))
                 (proper-list-p (plist-get receipt :tables))
                 (plist-get receipt :tables)
                 (cl-every #'stringp (plist-get receipt :tables))
                 (natnump (plist-get receipt :write-count))
                 (dorg-ext-rebuild--policy-producer-identity-p
                  (plist-get receipt :old-producer))
                 (dorg-ext-rebuild--policy-producer-identity-p
                  (plist-get receipt :new-producer))
                 (if (plist-member receipt :no-op)
                     (and (eq t (plist-get receipt :no-op))
                          (zerop (plist-get receipt :write-count))
                          (equal (plist-get receipt :base) (plist-get receipt :post-head)))
                   (> (plist-get receipt :write-count) 0)))
      (funcall malformed))
    (condition-case nil
        (progn (dorg-ext-rebuild--policy-upgrade-scope (plist-get receipt :scope))
               (dorg-ext-rebuild--identity (list :identity (plist-get receipt :identity))))
      (dorg-ext-rebuild-error (funcall malformed)))
    receipt))

;;;###autoload
(defun dorg-ext-rebuild-invert-policy-upgrade (extension receipt)
  "Invert EXTENSION's successful forward policy-upgrade RECEIPT exactly.
The store at the receipt's `:source' must have HEAD equal to its post-head F,
a clean working set, and F must be the authorized transition: its only parent
is the base H, its diff is wholly inside the scope tables and rows outside
the scope are unchanged.  The plan is re-derived at H with producers resolved
at H and must match the receipt's preimage, postimage and plan digests.  The
inverse then restores the exact C-at-H preimage, multiplicity included, in
one validated transaction at base F.  A no-op receipt inverts with no
transaction.  No force override exists."
  (dorg-ext-rebuild--policy-receipt-input extension receipt)
  (let* ((source (dorg-ext-rebuild--policy-source (plist-get receipt :source)))
         (post-head (plist-get receipt :post-head)))
    (unless (and (equal source (plist-get receipt :source))
                 (dorg-commit-ancestor-p source post-head))
      (dorg-ext-rebuild--raise "policy upgrade receipt does not belong to this store"
                               :reason 'source-mismatch :source source))
    (dorg-ext-rebuild--policy-gate-check source post-head)
    (let* ((state (dorg-ext-rebuild--policy-plan-state
                   extension (dorg-ext-rebuild--policy-request-of receipt)))
           (plan (plist-get state :plan))
           (descriptors (plist-get state :descriptors))
           (preimage (plist-get state :current-images))
           (no-op (plist-get receipt :no-op)))
      (unless (and (equal (plist-get plan :current-digest) (plist-get receipt :preimage-digest))
                   (equal (dorg-ext-rebuild--policy-image-digest (plist-get state :new-images))
                          (plist-get receipt :postimage-digest))
                   (equal (plist-get plan :plan-digest) (plist-get receipt :plan-digest))
                   (equal (plist-get plan :write-count) (plist-get receipt :write-count))
                   (equal (mapcar (lambda (d) (plist-get d :name)) descriptors)
                          (plist-get receipt :tables)))
        (dorg-ext-rebuild--raise "policy upgrade receipt does not match its re-derivation"
                                 :reason 'receipt-mismatch))
      (let ((inverse
             (list :ok t :kind 'policy-upgrade-inverse :base post-head
                   :new-head post-head :forward-receipt (copy-tree receipt)
                   :restored-digest (plist-get receipt :preimage-digest))))
        (if no-op
            (append inverse (list :no-op t))
          (let* ((query (lambda (sql) (dorg-sql source sql)))
                 (current (dorg-ext-rebuild--policy-read-images
                           query descriptors (plist-get state :root-id)
                           (plist-get state :memberships) post-head)))
            (unless (equal (dorg-ext-rebuild--policy-image-digest current)
                           (plist-get receipt :postimage-digest))
              (dorg-ext-rebuild--raise "derived rows at the post-head drifted from the receipt"
                                       :reason 'derived-drift))
            (unless (dorg-ext-rebuild--policy-commit-proven-p state post-head current)
              (dorg-ext-rebuild--raise
               "the forward commit is not wholly inside the authorized derived scope"
               :reason 'non-derived-diff :post-head post-head))
            ;; Restore C, never OLD: C->N was the forward delta, so N->C over
            ;; the same scope reverses exactly that diff.
            (let* ((effects
                    (apply #'append
                           (mapcar
                            (lambda (descriptor)
                              (let* ((table (plist-get descriptor :name))
                                     (now (cdr (assoc table current)))
                                     (delta (dorg-ext-rebuild--policy-delta
                                             table now (cdr (assoc table preimage)) now)))
                                (append (plist-get delta :deletes)
                                        (plist-get delta :inserts))))
                            descriptors)))
                   (inverse-state
                    (append (list :plan (append (list :base post-head) plan)
                                  :current-images current)
                            state))
                   (new-head
                    (dorg-ext-rebuild--policy-commit
                     inverse-state
                     (dorg-ext-rebuild--policy-delta-sql
                      (dorg-ext-rebuild--policy-split-effects effects)
                      (dorg-ext--owned-tables (dorg-get-extension extension)))
                     (format "dorg: invert policy upgrade %s root %d"
                             extension (plist-get state :root-id))
                     preimage)))
              (plist-put inverse :new-head new-head))))))))

;;;###autoload
(defun dorg-ext-rebuild-index (extension root-id &rest options)
  "Audit or repair EXTENSION's ROOT-ID index at an exact core revision.
OPTIONS require `:dolt-dir', `:revision', `:identity', and `:mode' (`audit'
or `repair').  IDENTITY is `dorg' or `(:property KEY)'; rebuild refuses a
missing, malformed, or `none' policy before opening a database operation.
Audit is read-only.  Repair is permitted only when REVISION is current HEAD and
commits DOrg-owned scope clears plus the selected canonical projector output in
one exact-base transaction."
  (let* ((options (dorg-ext-rebuild--options options))
         (dolt-dir (plist-get options :dolt-dir))
         (revision (plist-get options :revision))
         (mode (plist-get options :mode))
         (identity (dorg-ext-rebuild--identity options))
         (ext (dorg-get-extension extension)))
    (unless (and (symbolp extension) ext (dorg-extension-project ext))
      (dorg-ext-rebuild--raise "extension has no registered index projector"
                               :extension extension))
    (unless (and (stringp dolt-dir) (not (string-empty-p dolt-dir))
                 (stringp revision) (not (string-empty-p revision))
                 (integerp root-id) (memq mode '(audit repair)))
      (dorg-ext-rebuild--raise "invalid rebuild request" :extension extension))
    (unless (memq 'dolt (dorg-ext--effective-capabilities ext))
      (dorg-ext-rebuild--raise "extension does not support Dolt rebuild"
                               :extension extension))
    (unless (dorg-commit-ancestor-p dolt-dir revision)
      (dorg-ext-rebuild--raise "revision is not reachable from HEAD"
                               :revision revision))
    (let* ((descriptors (dorg-ext-rebuild-scope-tables ext))
           (owned (dorg-ext--owned-tables ext))
           (context (dorg-ext-rebuild--full-context dolt-dir root-id revision identity))
           (projection
            (condition-case err
                ;; DOrg-owned clears precede these effects, so accumulators
                ;; fold from absent and every root's marker rows are born.
                (dorg-ext-collect-index-projection
                 context (list extension)
                 (list :priors (lambda (_requests) nil) :empty-roots #'identity))
              (dorg-ext-index-error (signal (car err) (cdr err)))
              (error (dorg-ext-rebuild--raise
                      "selected index projector failed"
                      :extension extension :underlying (error-message-string err)))))
           (expected-parent-keys
            (dorg-ext-rebuild--parent-keys
             (plist-get projection :effects) descriptors root-id))
           (memberships
            (dorg-ext-rebuild--scope-memberships
             dolt-dir revision descriptors root-id expected-parent-keys))
           (effects
            (mapcar (lambda (effect)
                      (dorg-ext-rebuild--scope-effect effect descriptors root-id expected-parent-keys))
                    (plist-get projection :effects)))
           (clear-effects (dorg-ext-rebuild--clear-effects descriptors root-id memberships))
           (write-effects
            (dorg-ext-rebuild--validate-effects
             (append clear-effects effects) owned))
           (drift (dorg-ext-rebuild--drift
                   dolt-dir descriptors root-id revision effects memberships))
           (head (dorg-head-commit dolt-dir)))
      (when (and (eq mode 'repair) (not (equal revision head)))
        (dorg-ext-rebuild--raise
         "historical rebuild repair is refused; audit the historical revision instead"
         :revision revision :head head))
      (when (and (eq mode 'repair)
                 (not (dorg-ext-rebuild--repair-safe-p drift)))
        (dorg-ext-rebuild--raise
         "rebuild repair refuses to erase a row not reproduced by its projector"
         :extension extension :root-id root-id :drift drift))
      (let ((receipt (list :ok t :extension extension :root-id root-id
                           :scope descriptors :mode mode
                           :observed-core-revision revision :drift drift
                           :new-token nil)))
        (if (or (eq mode 'audit) (dorg-ext-rebuild--drift-empty-p drift))
            receipt
          (let* ((sql (mapcar #'dorg-ext--index-effect-sql write-effects))
                 (new-token
                  (dorg-transaction-commit
                   dolt-dir revision sql
                   (format "dorg: rebuild %s root %d" extension root-id))))
            (plist-put receipt :new-token new-token)))))))

(provide 'dorg-ext-rebuild)
;;; dorg-ext-rebuild.el ends here
