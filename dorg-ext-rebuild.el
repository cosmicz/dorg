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
(require 'dorg-model-load)
(require 'dorg-sql-gen)
(require 'dorg-transaction)

(define-error 'dorg-ext-rebuild-error
  "DOrg derived-index rebuild failed" 'dorg-error)

(defconst dorg-ext-rebuild--option-keys '(:dolt-dir :revision :mode)
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

(defun dorg-ext-rebuild--full-context (dolt-dir root-id revision)
  "Build the create-only full-root rebuild context AS OF REVISION.
This is the documented administrative exception to mutation's bounded context:
there is no fabricated before image or replayed patch history."
  (let* ((candidate
          (dorg-ext--index-host-safe-image
           (dorg-model-load-headings dolt-dir root-id nil revision)))
         (ids (sort (hash-table-keys candidate) #'<))
         (parts (dorg-ext--index-classify
                 (dorg-ext--index-empty-image) candidate ids nil nil)))
    (unless (gethash root-id candidate)
      (dorg-ext-rebuild--raise "root is absent at requested revision"
                               :root-id root-id :revision revision))
    (dorg-ext-index-context-create
     :source-kind 'dolt :root-id root-id :dolt-dir dolt-dir
     :base-commit revision
     :candidate-id (dorg-ext--index-candidate-id candidate)
     :patches nil
     :created (plist-get parts :created)
     :changed nil :moved nil :deleted nil
     :base-image (dorg-ext--index-empty-image)
     :candidate-postimage candidate
     :owner-partitions nil)))

(defun dorg-ext-rebuild--scope-column-symbol (descriptor)
  "Return DESCRIPTOR's validated scope column as a symbol."
  (intern (plist-get descriptor :scope-column)))

(defun dorg-ext-rebuild--scope-effect (effect descriptors root-id)
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
         (op (dorg-ext--index-effect-op effect))
         (payload (copy-tree (cdr op)))
         (existing (and column (assq column payload))))
    (unless descriptor
      (dorg-ext-rebuild--raise "projector effect table is outside rebuild scope"
                               :table table))
    (pcase (car op)
      ('insert
       (unless (and existing (equal (cdr existing) root-id))
         (dorg-ext-rebuild--raise
          "rebuild projector insert must carry the requested scope"
          :table table :scope-column column :root-id root-id :effect effect))
       effect)
      ('delete
       (when (and existing (not (equal (cdr existing) root-id)))
         (dorg-ext-rebuild--raise
          "rebuild projector delete names another scope"
          :table table :scope-column column :root-id root-id :effect effect))
       (unless existing
         (setq payload (append payload (list (cons column root-id)))))
       (list :table table :delete payload))
      (_ (dorg-ext-rebuild--raise "invalid rebuild projector effect" :effect effect)))))

(defun dorg-ext-rebuild--clear-effects (descriptors root-id)
  "Return DOrg-owned declared-scope clears for DESCRIPTORS at ROOT-ID."
  (mapcar (lambda (descriptor)
            (list :table (plist-get descriptor :name)
                  :delete
                  (list (cons (dorg-ext-rebuild--scope-column-symbol descriptor)
                              root-id))))
          descriptors))

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

(defun dorg-ext-rebuild--table-rows (dolt-dir descriptor root-id revision)
  "Read DESCRIPTOR's declared ROOT-ID scope AS OF REVISION."
  (let ((table (plist-get descriptor :name))
        (scope (plist-get descriptor :scope-column)))
    (dorg-sql
     dolt-dir
     (format "SELECT * FROM %s%s WHERE %s = %d"
             table (dorg-sql-gen--as-of-suffix revision) scope root-id))))

(defun dorg-ext-rebuild--expected-rows (effects table)
  "Return canonical insert rows for TABLE from full-snapshot EFFECTS."
  (mapcar #'dorg-ext-rebuild--canonical-row
          (mapcar (lambda (effect) (plist-get effect :insert))
                  (cl-remove-if-not
                   (lambda (effect)
                     (and (string= table (plist-get effect :table))
                          (plist-member effect :insert)))
                   effects))))

(defun dorg-ext-rebuild--drift (dolt-dir descriptors root-id revision effects)
  "Return bounded full-row multiset drift for DESCRIPTORS at ROOT-ID.
EFFECTS are already validated and narrowed rebuild effects."
  (let ((missing 0) (extra 0) (different 0) (examples nil))
    (dolist (descriptor descriptors)
      (let* ((table (plist-get descriptor :name))
             (expected (dorg-ext-rebuild--expected-rows effects table))
             (current (mapcar #'dorg-ext-rebuild--canonical-row
                              (dorg-ext-rebuild--table-rows
                               dolt-dir descriptor root-id revision)))
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

;;;###autoload
(defun dorg-ext-rebuild-index (extension root-id &rest options)
  "Audit or repair EXTENSION's ROOT-ID index at an exact core revision.
OPTIONS require `:dolt-dir', `:revision', and `:mode' (`audit' or `repair').
Audit is read-only.  Repair is permitted only when REVISION is current HEAD and
commits DOrg-owned scope clears plus the selected canonical projector output in
one exact-base transaction."
  (let* ((options (dorg-ext-rebuild--options options))
         (dolt-dir (plist-get options :dolt-dir))
         (revision (plist-get options :revision))
         (mode (plist-get options :mode))
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
           (context (dorg-ext-rebuild--full-context dolt-dir root-id revision))
           (projection
            (condition-case err
                (dorg-ext-collect-index-projection context (list extension))
              (dorg-ext-index-error (signal (car err) (cdr err)))
              (error (dorg-ext-rebuild--raise
                      "selected index projector failed"
                      :extension extension :underlying (error-message-string err)))))
           (effects
            (mapcar (lambda (effect)
                      (dorg-ext-rebuild--scope-effect effect descriptors root-id))
                    (plist-get projection :effects)))
           (clear-effects (dorg-ext-rebuild--clear-effects descriptors root-id))
           (write-effects
            (dorg-ext-rebuild--validate-effects
             (append clear-effects effects) owned))
           (drift (dorg-ext-rebuild--drift
                   dolt-dir descriptors root-id revision effects))
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
