;;; dorg-import.el --- Explicit full-document Dolt import -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Administrative bootstrap/recovery ingest for one complete declared Org
;; source.  This is intentionally not a synchronization facade: it has no
;; save hook, mode-enable hook, refresh path, merge plan, or implicit-buffer
;; fallback.  Ordinary authoring goes through typed mutation.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'dorg-backend)
(require 'dorg-dolt)
(require 'dorg-ext)
(require 'dorg-id)
(require 'dorg-model)
(require 'dorg-model-load)
(require 'dorg-parse)
(require 'dorg-registry)
(require 'dorg-repo)
(require 'dorg-sql-gen)
(require 'dorg-sql-quote)
(require 'dorg-transaction)

(declare-function dorg-materialized-view-scope "dorg-mode"
                  (&optional buffer))
(declare-function dorg-mutate--postimage-plan "dorg-mutate")

(define-error 'dorg-import-error
              "DOrg document import failed")

(define-error 'dorg-import-request-error
              "DOrg document import request is invalid"
              'dorg-import-error)

(define-error 'dorg-import-source-error
              "DOrg document import source is unsafe or incomplete"
              'dorg-import-error)

(define-error 'dorg-import-target-error
              "DOrg document import target is incompatible with the requested mode"
              'dorg-import-error)

(define-error 'dorg-import-postcondition-error
              "DOrg committed import failed exact-revision observation"
              'dorg-import-error)

(defconst dorg-import--option-keys
  '(:dolt-dir :target-file :mode :base :message)
  "Accepted keyword arguments for `dorg-import-document'.")

(defun dorg-import--options (options)
  "Validate and return import OPTIONS as a plist."
  (let ((length (proper-list-p options))
        seen)
    (unless (and length (zerop (% length 2)))
      (signal 'dorg-import-request-error
              (list "import options must be a proper key/value list")))
    (let ((rest options))
      (while rest
        (let ((key (pop rest)))
          (unless (memq key dorg-import--option-keys)
            (signal 'dorg-import-request-error
                    (list (format "unknown import option %S" key))))
          (when (memq key seen)
            (signal 'dorg-import-request-error
                    (list (format "duplicate import option %S" key))))
          (push key seen)
          (pop rest))))
    options))

(defun dorg-import--canonical-file (path)
  "Return PATH as an absolute file identity without requiring it to exist."
  (let ((expanded (expand-file-name path)))
    (if (file-exists-p expanded)
        (file-truename expanded)
      expanded)))

(defun dorg-import--projection-metadata-p (buffer)
  "Return non-nil when BUFFER declares canonical projection metadata."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search t)
              found)
          (while
              (and
               (not found)
               (re-search-forward
                "^[ \t]*#\\+DORG_\\(?:HASH\\|WINDOW\\):[ \t]*"
                nil t))
            (let ((begin (match-beginning 0))
                  (next (match-end 0)))
              (goto-char begin)
              ;; Body/example text can legitimately discuss this syntax.  Only
              ;; an actual Org keyword marks the document as a projection.
              (let ((element (org-element-context)))
                (when (and (eq (org-element-type element) 'keyword)
                           (member (upcase (org-element-property :key element))
                                   (list dorg-hash-keyword "DORG_WINDOW")))
                  (setq found t)))
              (goto-char next)))
          found)))))

(defun dorg-import--assert-source-buffer (buffer)
  "Reject BUFFER unless it is a complete, non-projection Org source."
  (unless (buffer-live-p buffer)
    (signal 'dorg-import-source-error
            (list "source buffer is not live")))
  (with-current-buffer buffer
    (unless (buffer-file-name)
      (signal 'dorg-import-source-error
              (list "source buffer must have an explicit file identity")))
    (when (buffer-base-buffer)
      (signal 'dorg-import-source-error
              (list "indirect source buffers are partial and cannot be imported")))
    (when (buffer-narrowed-p)
      (signal 'dorg-import-source-error
              (list "narrowed source buffers cannot be imported")))
    (when (and (fboundp 'dorg-materialized-view-scope)
               (dorg-materialized-view-scope buffer))
      (signal 'dorg-import-source-error
              (list "a live DOrg materialized projection cannot be imported")))
    (when (dorg-import--projection-metadata-p buffer)
      (signal 'dorg-import-source-error
              (list "DORG_HASH/DORG_WINDOW marks the source as an output projection")))))

(defun dorg-import--snapshot-buffer (text source-file)
  "Return a read-only Org buffer containing immutable TEXT for SOURCE-FILE."
  (let ((buffer (generate-new-buffer " *dorg-import-source*")))
    (with-current-buffer buffer
      (insert text)
      (setq buffer-file-name source-file)
      ;; Parsing must not run project hooks that could bind, enable a mode,
      ;; start a server, or otherwise turn source capture into a write path.
      (let ((org-mode-hook nil))
        (org-mode))
      ;; SOURCE-FILE rides separately in the import context.  The ephemeral
      ;; snapshot is deliberately not file-visiting, so an ill-behaved
      ;; participant cannot turn `save-buffer' into source-file writeback.
      (setq buffer-file-name nil)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))
    buffer))

(defun dorg-import--capture-source (source)
  "Capture explicit SOURCE into an immutable snapshot descriptor.
SOURCE is a live file-visiting buffer or a readable file pathname.
Returns (:file FILE :string TEXT :buffer SNAPSHOT)."
  (cond
   ((bufferp source)
    (dorg-import--assert-source-buffer source)
    (with-current-buffer source
      (let* ((file (dorg-import--canonical-file (buffer-file-name)))
             (text (buffer-substring-no-properties (point-min) (point-max))))
        (list :file file :string text
              :buffer (dorg-import--snapshot-buffer text file)))))
   ((stringp source)
    (let ((file (dorg-import--canonical-file source)))
      (unless (and (file-regular-p file) (file-readable-p file))
        (signal 'dorg-import-source-error
                (list (format "source is not a readable regular file: %s"
                              file))))
      (let ((text
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string))))
        (let ((snapshot (dorg-import--snapshot-buffer text file)))
          (condition-case err
              (progn
                (when (dorg-import--projection-metadata-p snapshot)
                  (signal
                   'dorg-import-source-error
                   (list
                    "DORG_HASH/DORG_WINDOW marks the source as an output projection")))
                (list :file file :string text :buffer snapshot))
            (error
             (kill-buffer snapshot)
             (signal (car err) (cdr err))))))))
   (t
    (signal 'dorg-import-source-error
            (list "source must be a live file-visiting buffer or file pathname")))))

(defun dorg-import--assert-snapshot-unchanged (source-buffer source-tick)
  "Reject mutation of SOURCE-BUFFER since SOURCE-TICK."
  (unless (= (with-current-buffer source-buffer
               (buffer-chars-modified-tick))
             source-tick)
    (signal 'dorg-import-source-error
            (list "the import source snapshot changed before commit"))))

(defun dorg-import--ensure-repository (dolt-dir)
  "Initialize or validate DOLT-DIR and return its canonical directory."
  (let ((expanded (file-name-as-directory (expand-file-name dolt-dir))))
    (unless (file-exists-p (expand-file-name ".dolt" expanded))
      (dorg-init expanded))
    (dorg-repo-ensure-schema expanded)
    (file-name-as-directory (file-truename expanded))))

(defun dorg-import--target-state (dolt-dir target-file)
  "Return the current import state for TARGET-FILE in DOLT-DIR."
  (let ((root-id (dorg-repo-root-id dolt-dir target-file)))
    (if (null root-id)
        (list :root-id nil :ids nil :imported nil :empty-synthetic nil)
      (let ((rows
             (dorg-sql
              dolt-dir
              (format
               (concat
                "SELECT h.id, h.parent_id, h.level, HEX(h.sort_key) AS sort_key_hex,"
                " h.todo_keyword, h.priority, h.commented, h.title,"
                " h.post_blank, h.items_hash,"
                " (SELECT COUNT(*) FROM heading_tags t WHERE t.heading_id = h.id) AS tag_count,"
                " (SELECT COUNT(*) FROM heading_items i WHERE i.heading_id = h.id) AS item_count"
                " FROM headings h WHERE h.root_id = %d ORDER BY h.id")
               root-id)))
            (imported
             (dorg-sql
              dolt-dir
              (format "SELECT value FROM dorg_meta WHERE `key` = %s"
                      (dorg-sql-quote
                       (format "document-import:%d" root-id))))))
        (unless rows
          (signal 'dorg-import-target-error
                  (list (format "target root %s has no headings row" root-id))))
        (let* ((root-row
                (cl-find root-id rows
                         :key (lambda (row) (alist-get 'id row))
                         :test #'eql))
               (empty
                (and (null imported)
                     (= (length rows) 1)
                     root-row
                     (null (alist-get 'parent_id root-row))
                     (eql (alist-get 'level root-row) 0)
                     (equal (alist-get 'sort_key_hex root-row) "01")
                     (null (alist-get 'todo_keyword root-row))
                     (null (alist-get 'priority root-row))
                     (zerop (or (alist-get 'commented root-row) 0))
                     (null (alist-get 'title root-row))
                     (zerop (or (alist-get 'post_blank root-row) 0))
                     (null (alist-get 'items_hash root-row))
                     (zerop (or (alist-get 'tag_count root-row) 0))
                     (zerop (or (alist-get 'item_count root-row) 0)))))
          (list :root-id root-id
                :ids (mapcar (lambda (row) (alist-get 'id row)) rows)
                :imported (and imported t)
                :empty-synthetic empty))))))

(defun dorg-import--preflight-target (mode base head state)
  "Validate MODE/BASE against HEAD and target STATE."
  (pcase mode
    ('bootstrap
     (when base
       (signal 'dorg-import-request-error
               (list "bootstrap does not accept :base")))
     (when (and (plist-get state :root-id)
                (not (plist-get state :empty-synthetic)))
       (signal 'dorg-import-target-error
               (list "bootstrap refuses a populated or previously imported target"))))
    ('replace
     (unless (and (stringp base) (not (string-empty-p base)))
       (signal 'dorg-import-request-error
               (list "replace requires an exact :base revision")))
     (unless (equal base head)
       (signal 'dorg-transaction-stale
               (list (format "replace base %s is stale; current HEAD is %s"
                             base head)
                     :base base :head head)))
     (unless (plist-get state :root-id)
       (signal 'dorg-import-target-error
               (list "replace requires an existing target root"))))
    (_
     (signal 'dorg-import-request-error
             (list ":mode must be bootstrap or replace")))))

(defun dorg-import--chunks (values size)
  "Return VALUES partitioned into lists of at most SIZE elements."
  (let (chunks)
    (while values
      (let ((chunk nil)
            (n 0))
        (while (and values (< n size))
          (push (pop values) chunk)
          (setq n (1+ n)))
        (push (nreverse chunk) chunks)))
    (nreverse chunks)))

(defun dorg-import--assert-no-foreign-ids
    (dolt-dir target-root heading-ids)
  "Reject HEADING-IDS already owned by a root other than TARGET-ROOT."
  (dolist (chunk (dorg-import--chunks (copy-sequence heading-ids) 500))
    (let ((rows
           (dorg-sql
            dolt-dir
            (format "SELECT id, root_id FROM headings WHERE id IN (%s)"
                    (mapconcat #'number-to-string chunk ", ")))))
      (dolist (row rows)
        (let ((owner (alist-get 'root_id row))
              (id (alist-get 'id row)))
          (unless (and target-root (eql owner target-root))
            (signal 'dorg-import-target-error
                    (list (format "DORG_ID %s already belongs to root %s"
                                  id owner)
                          :id id :root-id owner))))))))

(defun dorg-import--root-reconcile-sql (root id-generator)
  "Return SQL replacing existing ROOT content while preserving its id."
  (let ((id (dorg-heading-id root)))
    (append
     (list (dorg-sql-gen-heading-update root)
           (format "DELETE FROM heading_tags WHERE heading_id = %d" id))
     (dorg-sql-gen-tag-inserts id (dorg-heading-tags root))
     (list (format "DELETE FROM heading_items WHERE heading_id = %d" id))
     (dorg-sql-gen-heading-items-sql root id-generator))))

(defun dorg-import--core-sql
    (dolt-dir parsed target-file existing-root mode id-generator)
  "Return complete core SQL for PARSED import into TARGET-FILE in DOLT-DIR.
EXISTING-ROOT is nil for a new target.  MODE is `bootstrap' or
`replace'.  ID-GENERATOR allocates item ids."
  (let* ((root-id (dorg-parse-result-root-id parsed))
         (headings (dorg-parse-result-headings parsed))
         sql)
    (when (eq mode 'replace)
      (push (format "DELETE FROM headings WHERE parent_id = %d" root-id) sql))
    (if existing-root
        (progn
          (setq sql
                (nconc (nreverse sql)
                       (dorg-import--root-reconcile-sql
                        (car headings) id-generator)))
          (dolist (heading (cdr headings))
            (setq sql
                  (nconc sql
                         (dorg-sql-gen-heading-full-sql
                          heading root-id id-generator)))))
      (setq sql (nreverse sql))
      (dolist (heading headings)
        (setq sql
              (nconc sql
                     (dorg-sql-gen-heading-full-sql
                      heading root-id id-generator)))))
    (nconc
     sql
     (list
      (dorg-repo-file-row-statement dolt-dir root-id target-file)
      (format
       (concat "INSERT INTO dorg_meta (`key`, value) VALUES (%s, '1') "
               "ON DUPLICATE KEY UPDATE value = '1'")
       (dorg-sql-quote (format "document-import:%d" root-id)))))))

(defun dorg-import--same-path-p (left right)
  "Return non-nil when file identities LEFT and RIGHT name one path."
  (let ((left (dorg-repo--canonical-local-path left "Org file"))
        (right (dorg-repo--canonical-local-path right "Org file")))
    (if (or (file-name-case-insensitive-p left)
            (file-name-case-insensitive-p right))
        (string-equal (downcase left) (downcase right))
      (string-equal left right))))

(defun dorg-import--validate-state
    (query dolt-dir root-id target-file expected-ids &optional revision)
  "Validate core import state through QUERY, optionally AS OF REVISION."
  (let ((suffix (dorg-sql-gen--as-of-suffix revision)))
    (let* ((rows
            (funcall
             query
             (format "SELECT id FROM headings%s WHERE root_id = %%d ORDER BY id"
                     suffix)
             root-id))
           (actual (sort (mapcar (lambda (row) (alist-get 'id row)) rows) #'<))
           (expected (sort (copy-sequence expected-ids) #'<)))
      (unless (equal actual expected)
        (error "dorg-import: pending id set mismatch (expected %S, stored %S)"
               expected actual)))
    (let ((root
           (funcall
            query
            (format
             (concat "SELECT id FROM headings%s"
                     " WHERE id = %%d AND root_id = %%d"
                     " AND parent_id IS NULL AND level = 0")
             suffix)
            root-id root-id)))
      (unless (= (length root) 1)
        (error "dorg-import: pending root identity %s is invalid" root-id)))
    (let ((orphans
           (funcall
            query
            (format
             (concat
              "SELECT h.id FROM headings%s h"
              " LEFT JOIN headings%s p ON h.parent_id = p.id"
              " WHERE h.root_id = %%d AND h.id <> %%d"
              " AND (p.id IS NULL OR p.root_id <> %%d)")
             suffix suffix)
            root-id root-id root-id)))
      (when orphans
        (error "dorg-import: pending parent closure failed for ids %S"
               (mapcar (lambda (row) (alist-get 'id row)) orphans))))
    (let* ((files
            (funcall
             query
             (format "SELECT relative_path FROM dorg_files%s WHERE root_id = %%d"
                     suffix)
             root-id))
           (stored (and (= (length files) 1)
                        (alist-get 'relative_path (car files)))))
      (unless (and stored
                   (dorg-import--same-path-p
                    (dorg-repo-decode-relative-path dolt-dir stored)
                    target-file))
        (error "dorg-import: pending file binding mismatch for root %s"
               root-id)))
    (let ((marker
           (funcall
            query
            (format "SELECT value FROM dorg_meta%s WHERE `key` = %%s"
                    suffix)
            (dorg-sql-quote (format "document-import:%d" root-id)))))
      (unless (= (length marker) 1)
        (error "dorg-import: pending import marker is missing for root %s"
               root-id)))))

(defun dorg-import--validate-pending
    (query dolt-dir root-id target-file expected-ids)
  "Validate pending core import state through read-only QUERY."
  (dorg-import--validate-state
   query dolt-dir root-id target-file expected-ids nil))

(defun dorg-import--validate-committed
    (dolt-dir root-id target-file expected-ids revision)
  "Validate committed core import state in DOLT-DIR exactly at REVISION."
  (dorg-import--validate-state
   (lambda (query &rest args)
     (dorg-sql dolt-dir (if args (apply #'format query args) query)))
   dolt-dir root-id target-file expected-ids revision))

(defun dorg-import--parsed-image (headings)
  "Return a host-safe id -> heading image for parsed HEADINGS."
  (let ((image (dorg-ext--index-empty-image)))
    (dolist (heading headings image)
      (puthash (dorg-heading-id heading) (dorg-heading-deep-copy heading) image))))

(defun dorg-import--image-ids (image)
  "Return sorted heading ids in IMAGE."
  (let (ids)
    (maphash (lambda (id _heading) (push id ids)) image)
    (sort ids #'<)))

(defun dorg-import--image-changed-ids (base-image candidate-image)
  "Return common ids whose persisted canonical image changed."
  (let (ids)
    (maphash
     (lambda (id after)
       (when-let ((before (gethash id base-image)))
         (unless (equal (dorg-ext--index-heading-canonical before)
                        (dorg-ext--index-heading-canonical after))
           (push id ids))))
     candidate-image)
    (sort ids #'<)))

(defun dorg-import--target-registration (target-file)
  "Return explicit registry binding for TARGET-FILE, or nil.
Import uses only an already stored target binding for identity policy.  Transient
keyword classification is intentionally ignored here: an unbound import target
falls back to the shared default policy."
  (cl-loop for (file . registration) in dorg--registry
           when (dorg-import--same-path-p file target-file)
           return registration))

(defun dorg-import--identity-policy (target-file)
  "Return TARGET-FILE's explicit identity policy, or the shared default `dorg'."
  (or (and-let* ((registration (dorg-import--target-registration target-file)))
        (dorg-registration-identity registration))
      'dorg))

(defun dorg-import--owner-key (identity)
  "Return identity owner key for IDENTITY, or nil for identity=none."
  (pcase identity
    ('dorg dorg-id-property)
    (`(:property ,key) key)
    (_ nil)))

(defun dorg-import--heading-owner-value (identity heading)
  "Return HEADING's owner identity value under IDENTITY."
  (pcase identity
    ('dorg (and (dorg-heading-id heading)
                (number-to-string (dorg-heading-id heading))))
    (`(:property ,key)
     (let ((value (cdr (assoc key (dorg-model-heading-property-snapshot heading)))))
       (and (stringp value) value)))
    (_ nil)))

(defun dorg-import--nearest-owner (identity root-id image start-id)
  "Return START-ID's nearest identity owner in complete IMAGE, or nil."
  (let ((key (dorg-import--owner-key identity))
        path
        seen
        (id start-id))
    (when key
      (if (eq identity 'dorg)
          (and (integerp start-id)
               (list :root-id root-id :owner-id start-id
                     :owner-key key :owner-value (number-to-string start-id)
                     :path (list start-id)))
        (catch 'owner
          (while (integerp id)
            (when (memq id seen)
              (signal 'dorg-import-target-error
                      (list "cycle in owner ancestor chain"
                            :root-id root-id :heading id)))
            (push id seen)
            (let ((heading (gethash id image)))
              (unless heading
                (signal 'dorg-import-target-error
                        (list "missing heading while resolving owner partition"
                              :root-id root-id :heading id)))
              (push (dorg-heading-id heading) path)
              (let ((value (dorg-import--heading-owner-value identity heading)))
                (when value
                  (throw 'owner
                         (list :root-id root-id
                               :owner-id (dorg-heading-id heading)
                               :owner-key key
                               :owner-value value
                               :path (nreverse path))))
                (setq id (dorg-heading-parent-id heading))))))))))

(defun dorg-import--parent-owner (identity root-id image owner-id)
  "Return OWNER-ID's nearest ancestor identity owner in complete IMAGE, or nil."
  (when-let* ((owner (gethash owner-id image))
              (parent-id (dorg-heading-parent-id owner)))
    (if (eq identity 'dorg)
        (list :root-id root-id :owner-id parent-id
              :owner-key (dorg-import--owner-key identity)
              :owner-value (number-to-string parent-id)
              :path (list parent-id))
      (dorg-import--nearest-owner identity root-id image parent-id))))

(defun dorg-import--children-index (image)
  "Return parent-id -> child-id list index for complete IMAGE."
  (let ((children (make-hash-table :test 'eql)))
    (maphash
     (lambda (id heading)
       (when-let ((parent-id (dorg-heading-parent-id heading)))
         (puthash parent-id (cons id (gethash parent-id children)) children)))
     image)
    (maphash (lambda (parent ids) (puthash parent (sort ids #'<) children))
             children)
    children))

(defun dorg-import--owner-local-ids (identity image owner-id)
  "Return OWNER-ID plus non-nested-owner descendants in complete IMAGE."
  (pcase identity
    ('dorg (list owner-id))
    (`(:property ,key)
     (let ((children (dorg-import--children-index image))
           ids
           seen)
       (cl-labels
           ((walk
              (id root-p)
              (when (memq id seen)
                (signal 'dorg-import-target-error
                        (list "cycle in owner descendant chain" :heading id)))
              (push id seen)
              (when-let ((heading (gethash id image)))
                (unless (and (not root-p)
                             (cdr (assoc key
                                         (dorg-model-heading-property-snapshot
                                          heading))))
                  (push id ids)
                  (dolist (child (gethash id children))
                    (walk child nil))))))
         (walk owner-id t))
       (sort ids #'<)))
    (_ nil)))

(defun dorg-import--image-slice (image ids)
  "Return a host-safe slice of IMAGE restricted to IDS."
  (let ((slice (dorg-ext--index-empty-image)))
    (dolist (id ids slice)
      (when-let ((heading (gethash id image)))
        (puthash id (dorg-heading-deep-copy heading) slice)))))

(defun dorg-import--all-index-entries (parts)
  "Return every index entry in PARTS."
  (append (plist-get parts :created)
          (plist-get parts :changed)
          (plist-get parts :moved)
          (plist-get parts :deleted)))

(defun dorg-import--put-owner (owners owner affected-id side)
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

(defun dorg-import--index-owner-partitions
    (identity root-id parts base-image candidate-postimage)
  "Return affected owner partitions for complete import images.
Unlike mutation's bounded image path, import owns COMPLETE S0 and candidate
images for the full document.  Owner-local images are therefore derived by a pure
walk over those complete images; the bounded-load fallbacks mutation needs never
fire, so the owner partition semantics coincide without import-side SQL reads."
  (let ((owners (make-hash-table :test 'equal))
        (entries (dorg-import--all-index-entries parts)))
    (when (dorg-import--owner-key identity)
      (dolist (entry entries)
        (when-let ((before (dorg-ext-index-entry-before entry)))
          (dorg-import--put-owner
           owners
           (dorg-import--nearest-owner
            identity root-id base-image (dorg-heading-id before))
           (dorg-ext-index-entry-id entry)
           'before))
        (when-let ((after (dorg-ext-index-entry-after entry)))
          (dorg-import--put-owner
           owners
           (dorg-import--nearest-owner
            identity root-id candidate-postimage (dorg-heading-id after))
           (dorg-ext-index-entry-id entry)
           'after))))
    (let (partitions)
      (maphash
       (lambda (_key owner)
         (let* ((owner-id (plist-get owner :owner-id))
                (parent-image (if (gethash owner-id candidate-postimage)
                                  candidate-postimage
                                base-image))
                (parent (dorg-import--parent-owner
                         identity root-id parent-image owner-id))
                (base-ids
                 (and (gethash owner-id base-image)
                      (dorg-import--owner-local-ids identity base-image owner-id)))
                (candidate-ids
                 (and (gethash owner-id candidate-postimage)
                      (dorg-import--owner-local-ids
                       identity candidate-postimage owner-id))))
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
             :base-image (dorg-import--image-slice base-image base-ids)
             :candidate-postimage
             (dorg-import--image-slice candidate-postimage candidate-ids))
            partitions)))
       owners)
      (sort partitions
            (lambda (a b)
              (< (dorg-ext-owner-partition-owner-id a)
                 (dorg-ext-owner-partition-owner-id b)))))))

(defun dorg-import--index-context
    (dolt-dir target-file root-id base candidate-image mode)
  "Build import's canonical `dorg-ext-index-context'."
  (let* ((base-image
          (if (eq mode 'replace)
              (dorg-ext--index-host-safe-image
               (dorg-model-load-headings dolt-dir root-id nil base))
            (dorg-ext--index-empty-image)))
         (created (dorg-import--set-difference
                   (dorg-import--image-ids candidate-image)
                   (dorg-import--image-ids base-image)))
         (deleted (dorg-import--set-difference
                   (dorg-import--image-ids base-image)
                   (dorg-import--image-ids candidate-image)))
         ;; Replace is a full snapshot import, so every surviving heading is
         ;; reprojected from canonical core even when its core tuple is unchanged.
         (changed (if (eq mode 'replace)
                      (dorg-import--set-difference
                       (dorg-import--image-ids candidate-image) created)
                    (dorg-import--image-changed-ids
                     base-image candidate-image)))
         (parts (dorg-ext--index-classify
                 base-image candidate-image created changed deleted))
         (identity (dorg-import--identity-policy target-file))
         (owner-partitions
          (dorg-import--index-owner-partitions
           identity root-id parts base-image candidate-image)))
    (dorg-ext-index-context-create
     :source-kind 'dolt
     :root-id root-id
     :dolt-dir dolt-dir
     :file target-file
     :base-commit base
     :candidate-id (dorg-ext--index-candidate-id
                    candidate-image owner-partitions)
     :patches nil
     :created (plist-get parts :created)
     :changed (plist-get parts :changed)
     :moved (plist-get parts :moved)
     :deleted (plist-get parts :deleted)
     :base-image base-image
     :candidate-postimage candidate-image
     :owner-partitions owner-partitions)))

(defun dorg-import--set-difference (left right)
  "Return LEFT values absent from RIGHT, preserving LEFT order."
  (cl-remove-if (lambda (value) (memql value right)) left))

(defun dorg-import--receipt
    (mode source-file target-file dolt-dir root-id base commit
          old-ids new-ids extensions ext-dirty)
  "Build the public import receipt."
  (list :ok t
        :mode mode
        :source source-file
        :target target-file
        :dolt-dir dolt-dir
        :root-id root-id
        :base-commit base
        :commit commit
        :created (dorg-import--set-difference new-ids old-ids)
        :changed (cl-remove-if-not (lambda (id) (memql id old-ids)) new-ids)
        :deleted (dorg-import--set-difference old-ids new-ids)
        :extensions extensions
        :invalidation
        (list :roots (list root-id)
              :extensions (mapcar #'car extensions)
              :ext-dirty ext-dirty
              :scope 'whole)))

(defun dorg-import-document (source &rest options)
  "Import one complete declared Org SOURCE into a declared Dolt target.
SOURCE is a live file-visiting buffer or readable file pathname.

Required OPTIONS are:
  :dolt-dir DIR       target Dolt repository
  :target-file FILE   target file/root identity
  :mode MODE          `bootstrap' or `replace'

Replace additionally requires :base with the exact current revision and
a source file distinct from TARGET-FILE.  :message overrides the
administrative commit message.

Bootstrap accepts only an unbound target or the exact empty synthetic
root created by legacy binding.  Replace preserves the existing root and
file identities.  Source capture, parse, id completion, collision checks,
and derived index planning happen before the gated transaction.  Core and
derived extension SQL then execute together; exact id/file/parent checks and
derived-row postimage checks run inside the pending transaction before one
commit.

The function never writes IDs or metadata to SOURCE and never
materializes TARGET-FILE.  Use `dorg-materialize' with the returned
:commit token as the explicit output step."
  (let* ((opts (dorg-import--options options))
         (dolt-option (plist-get opts :dolt-dir))
         (target-option (plist-get opts :target-file))
         (mode (plist-get opts :mode))
         (base-option (plist-get opts :base))
         (message (plist-get opts :message)))
    (unless (and (stringp dolt-option) (not (string-empty-p dolt-option)))
      (signal 'dorg-import-request-error
              (list ":dolt-dir is required")))
    (unless (and (stringp target-option) (not (string-empty-p target-option)))
      (signal 'dorg-import-request-error
              (list ":target-file is required")))
    (unless (memq mode '(bootstrap replace))
      (signal 'dorg-import-request-error
              (list ":mode must be bootstrap or replace")))
    (when (and (eq mode 'bootstrap) (plist-member opts :base))
      (signal 'dorg-import-request-error
              (list "bootstrap does not accept :base")))
    (when (and (eq mode 'replace)
               (not (and (stringp base-option)
                         (not (string-empty-p base-option)))))
      (signal 'dorg-import-request-error
              (list "replace requires an exact :base revision")))
    (when (and (plist-member opts :message)
               (not (and (stringp message)
                         (not (string-empty-p message)))))
      (signal 'dorg-import-request-error
              (list ":message must be a non-empty string")))
    ;; Source preflight precedes repository initialization/schema writes.
    (let* ((source-desc (dorg-import--capture-source source))
           (source-file (plist-get source-desc :file))
           (source-buffer (plist-get source-desc :buffer))
           (target-file (dorg-import--canonical-file target-option)))
      (unwind-protect
          (progn
            (when (file-directory-p target-file)
              (signal 'dorg-import-target-error
                      (list "target file identity names a directory")))
            (when (and (eq mode 'replace)
                       (dorg-import--same-path-p source-file target-file))
              (signal 'dorg-import-source-error
                      (list "replace requires a recovery source distinct from the target")))
            (let* ((dolt-dir (dorg-import--ensure-repository dolt-option))
                   (head (dorg-head-commit dolt-dir))
                   (state (dorg-import--target-state dolt-dir target-file)))
              (when (and (eq mode 'bootstrap)
                         (dorg-import--same-path-p source-file target-file)
                         (plist-get state :root-id))
                (signal 'dorg-import-source-error
                        (list
                         (concat
                          "same-file bootstrap requires an unbound target; "
                          "use a distinct declared source for an empty binding"))))
              (dorg-import--preflight-target mode base-option head state)
              (let* ((existing-root (plist-get state :root-id))
                     (parsed
                      (dorg-parse-buffer source-buffer existing-root)))
                (dorg-parse-assign-ids parsed)
                (let* ((root-id (dorg-parse-result-root-id parsed))
                       (headings (dorg-parse-result-headings parsed))
                       (new-ids (mapcar #'dorg-heading-id headings))
                       (old-ids (plist-get state :ids))
                       (source-tick
                        (with-current-buffer source-buffer
                          (buffer-chars-modified-tick)))
                       (core-sql
                        (dorg-import--core-sql
                         dolt-dir parsed target-file existing-root mode
                         #'dorg-id-generate)))
                  (dorg-import--assert-no-foreign-ids
                   dolt-dir existing-root new-ids)
                  (let* ((candidate-image
                          (dorg-import--parsed-image headings))
                         (index-context
                          (dorg-import--index-context
                           dolt-dir target-file root-id head
                           candidate-image mode))
                         (projection
                          (dorg-ext-collect-index-projection
                           index-context))
                         (extensions (plist-get projection :extensions))
                         (ext-dirty (plist-get projection :ext-dirty))
                         (postimage
                          (dorg-mutate--postimage-plan
                           index-context (plist-get projection :effects)))
                         (validate-function
                          (append
                           (plist-get postimage :validate)
                           (list
                            :query-function
                            (lambda (query)
                              ;; Validate core last so drift from any
                              ;; generated extension SQL is still caught.
                              (dorg-import--validate-pending
                               query dolt-dir root-id target-file new-ids)
                              (dorg-import--assert-snapshot-unchanged
                               source-buffer source-tick)))))
                         (statements
                          (append core-sql
                                  (plist-get projection :sql))))
                    (dorg-import--assert-snapshot-unchanged
                     source-buffer source-tick)
                    (let* ((commit
                            (dorg-transaction-commit
                             dolt-dir head statements
                             (or message
                                 (format "dorg: %s %s"
                                         mode
                                         (file-name-nondirectory target-file)))
                             validate-function))
                           (receipt
                            (dorg-import--receipt
                             mode source-file target-file dolt-dir root-id
                             head commit old-ids new-ids extensions
                             ext-dirty)))
                      (condition-case err
                          (dorg-import--validate-committed
                           dolt-dir root-id target-file new-ids commit)
                        (error
                         (signal
                          'dorg-import-postcondition-error
                          (list
                           (format
                            (concat
                             "import committed as %s, but exact-revision "
                             "validation failed: %s")
                            commit (error-message-string err))
                           :commit commit
                           :underlying (error-message-string err)))))
                      receipt))))))
        (when (buffer-live-p source-buffer)
          (kill-buffer source-buffer))))))

(provide 'dorg-import)
;;; dorg-import.el ends here
