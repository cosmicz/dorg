;;; extension.el --- A foreign-key test extension for insert lowering -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A third-party index extension whose two tables carry foreign keys: each
;; child row references its own extension parent row and a core heading.
;; DOrg knows none of these keys, so it may only merge the extension's
;; adjacent same-shape inserts and must never reorder its effects.
;;
;; Per affected heading the projector emits, in this order:
;; - a child delete, then a parent delete (when the heading existed);
;; - a parent insert (when the heading exists afterwards).
;; Then it emits every child insert as one run.  Deletes and inserts
;; interleave across both tables, and the child inserts form one adjacent
;; run that DOrg can merge.
;;
;; A fault cell can place one invalid child row in the MIDDLE of that run:
;; - `duplicate' repeats the first child's primary key;
;; - `dangling' references an extension parent that does not exist.

;;; Code:

(require 'cl-lib)
(require 'dorg-ext)
(require 'dorg-model)

(defconst dorg-ibx-schema
  '("CREATE TABLE ib_parent (id BIGINT PRIMARY KEY, root_id BIGINT NOT NULL, label TEXT NULL)"
    "CREATE TABLE ib_child (id BIGINT PRIMARY KEY, parent_id BIGINT NOT NULL, heading_id BIGINT NOT NULL, root_id BIGINT NOT NULL, title TEXT NULL, FOREIGN KEY (parent_id) REFERENCES ib_parent(id) ON DELETE CASCADE, FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE)")
  "The test extension's tables.")

(defconst dorg-ibx-tables '("ib_parent" "ib_child")
  "The test extension's owned tables.")

(defconst dorg-ibx-dump-tables
  '(("ib_parent" . "id") ("ib_child" . "id"))
  "Order keys for dumping the test extension's tables.")

(defun dorg-ibx--entries (context)
  "Return CONTEXT's affected non-root entries in ascending id order."
  (let ((roots (delq nil (list (dorg-ext-index-context-root-id context)
                               (dorg-ext-index-context-from-root-id context)
                               (dorg-ext-index-context-to-root-id context)))))
    (sort (cl-remove-if (lambda (entry) (memql (dorg-ext-index-entry-id entry) roots))
                        (append (dorg-ext-index-context-created context)
                                (dorg-ext-index-context-changed context)
                                (dorg-ext-index-context-moved context)
                                (dorg-ext-index-context-deleted context)))
          (lambda (a b) (< (dorg-ext-index-entry-id a) (dorg-ext-index-entry-id b))))))

(defun dorg-ibx-project (context fault)
  "Project CONTEXT for the test extension, with FAULT nil, `duplicate' or `dangling'."
  (let ((root (or (dorg-ext-index-context-to-root-id context)
                  (dorg-ext-index-context-root-id context)))
        effects children)
    (dolist (entry (dorg-ibx--entries context))
      (let* ((id (dorg-ext-index-entry-id entry))
             (kind (dorg-ext-index-entry-kind entry))
             (after (and (memq kind '(create change move))
                         (dorg-ext-index-entry-after entry))))
        (when (memq kind '(change move delete))
          (push `(:table "ib_child" :delete ((heading_id . ,id))) effects)
          (push `(:table "ib_parent" :delete ((id . ,id))) effects))
        (when after
          (push `(:table "ib_parent" :insert ((id . ,id) (root_id . ,root)
                                              (label . ,(and (cl-oddp id) "odd"))))
                effects)
          (push `(:table "ib_child" :insert ((id . ,(- id)) (parent_id . ,id)
                                             (heading_id . ,id) (root_id . ,root)
                                             (title . ,(dorg-heading-title after))))
                children))))
    (setq children (nreverse children))
    (when (and fault (cdr children))
      ;; The bad row keeps the run's exact column list, so it lowers into
      ;; the same multi-row statement as its neighbours.
      (let* ((payload (plist-get (car children) :insert))
             (bad (if (eq fault 'duplicate)
                      (copy-tree payload)
                    (mapcar (lambda (cell)
                              (pcase (car cell)
                                ('id (cons 'id 1))
                                ('parent_id (cons 'parent_id 2))
                                (_ (cons (car cell) (cdr cell)))))
                            payload)))
             (middle (/ (length children) 2)))
        (setq children (append (cl-subseq children 0 middle)
                               (list (list :table "ib_child" :insert bad))
                               (nthcdr middle children)))))
    (append (nreverse effects) children)))

(defun dorg-ibx-register (fault-cell)
  "Register the test extension; FAULT-CELL's car selects the injected fault."
  (dorg-register-extension
   'ib-fk
   :schema dorg-ibx-schema
   :version 1
   :read-tables '((:name "ib_parent" :key-column "id")
                  (:name "ib_child" :key-column "id"))
   :project (lambda (context) (dorg-ibx-project context (car fault-cell)))))

(provide 'dorg-insert-batching-extension)
;;; extension.el ends here
