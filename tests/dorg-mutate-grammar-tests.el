;;; dorg-mutate-grammar-tests.el --- Semantic mutate grammar inventory tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Drift guards for the public mutation grammar.  The inventory is data, not a
;; second parser: every concrete example is normalized by the real public API
;; and checked against its declared typed-IR operation and target kind.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-app)
(require 'dorg-mutate-grammar)

(defconst dorg-test-mutate-grammar--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute repository root for grammar drift checks.")

(defconst dorg-test-mutate-grammar--production-ids
  '(set-property
    delete-property
    replace-keywords
    delete-keywords
    replace-keyword
    delete-keyword
    insert-keyword
    insert-drawer-line
    set-heading
    set-planning
    insert-text
    replace-text
    replace-items
    move
    delete-heading
    delete-subtree
    insert-heading)
  "The ratified post-g0vnp public production set.")

(defun dorg-test-mutate-grammar--read (relative)
  "Return the contents of RELATIVE under the repository root."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative dorg-test-mutate-grammar--root))
    (buffer-string)))

(defun dorg-test-mutate-grammar--count (regexp text)
  "Return the number of non-overlapping REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(ert-deftest dorg-test-mutate-grammar-exact-production-set-and-schema ()
  "The semantic inventory is exactly the ratified 17-production grammar."
  (let ((ids (mapcar (lambda (production) (plist-get production :id))
                     dorg-mutate-grammar-productions))
        (required '(:id :family :verb :template :example :ir
                        :collection-address :cardinality :missing
                        :positions :clearing :safety)))
    (should (equal dorg-test-mutate-grammar--production-ids ids))
    (should (= (length ids) (length (delete-dups (copy-sequence ids)))))
    (dolist (production dorg-mutate-grammar-productions)
      (ert-info ((format "%S" (plist-get production :id)))
        (dolist (key required)
          (should (plist-member production key))
          (should (plist-get production key)))
        (should (= (length (plist-get production :collection-address))
                   (dorg-mutate-grammar-address-depth production)))))))

(ert-deftest dorg-test-mutate-grammar-every-example-normalizes-to-declared-ir ()
  "Every inventory example is accepted by the real normalizer with declared IR."
  (dolist (production dorg-mutate-grammar-productions)
    (ert-info ((format "%S" (plist-get production :id)))
      (let* ((patches (dorg-api-normalize (plist-get production :example)))
             (patch (car patches))
             (declared (plist-get production :ir))
             (target (plist-get patch :target)))
        (should (= 1 (length patches)))
        (should (eq (plist-get declared :op) (plist-get patch :op)))
        (should (eq (plist-get declared :target-kind) (car target)))))))

(ert-deftest dorg-test-9diah-mutate-grammar-insert-positions-match-api ()
  "The inventory advertises every accepted scoped item insertion position."
  (dolist (case '((insert-keyword
                   (on document (insert (keyword "K") POSITION "v")))
                  (insert-drawer-line
                   (on (id 100) (insert (drawer-line "AUDIT" (occurrence 1))
                                        POSITION "- line")))
                  (insert-text
                   (on (id 100) (insert (text) POSITION "body")))))
    (pcase-let* ((`(,id ,template) case)
                 (production (cl-find id dorg-mutate-grammar-productions
                                      :key (lambda (p) (plist-get p :id))))
                 (advertised (plist-get production :positions))
                 (accepted nil))
      (dolist (position '((before-first) (after-last)))
        (let ((form (cl-subst position 'POSITION template)))
          (when (condition-case nil
                    (progn (dorg-api-normalize form) t)
                  (dorg-api-error nil))
            (push (intern (format ":%s" (car position))) accepted))))
      (should (equal (sort (copy-sequence advertised) #'string<)
                     (sort (copy-sequence accepted) #'string<))))))

(ert-deftest dorg-test-mutate-grammar-target-and-clearing-laws-are-semantic ()
  "Target safety and per-kind clearing rules are machine-readable facts."
  (let ((key (alist-get 'key dorg-mutate-grammar-targets))
        (where (alist-get 'where dorg-mutate-grammar-targets))
        (base (alist-get 'base dorg-mutate-grammar-safety-laws))
        (items (alist-get 'items dorg-mutate-grammar-clearing-laws)))
    (should (eq 'exactly-one (plist-get key :cardinality)))
    (should (eq 'identity-violation (plist-get key :duplicates)))
    (should (eq 'required-on-apply (plist-get where :expected-targets)))
    (should (eq 'checked-when-supplied (plist-get base :cas)))
    (should (eq 'unsupported-public (plist-get items :clearing)))
    (should-not
     (cl-find-if (lambda (production)
                   (memq (plist-get production :id)
                         '(delete-items replace-empty-items)))
                 dorg-mutate-grammar-productions))))

(ert-deftest dorg-test-c8ynt-heading-field-domain-is-semantic-and-normalizes ()
  "The intrinsic heading FIELD domain is one exact, executable inventory."
  (let ((expected
         '((title string singleton empty-string headings.title "Inbox")
           (todo token-string-or-nil singleton nil-or-empty-string
                 headings.todo_keyword "DONE")
           (priority priority-string-or-nil singleton nil headings.priority "A")
           (tags tag-list singleton empty-list heading_tags ("work"))
           (commented boolean-nil-or-t singleton nil headings.commented t))))
    (should
     (equal expected
            (mapcar
             (lambda (entry)
               (let ((data (cdr entry)))
                 (list (car entry)
                       (plist-get data :value-domain)
                       (plist-get data :cardinality)
                       (plist-get data :clearing)
                       (plist-get data :projection)
                       (plist-get data :example))))
             dorg-mutate-grammar-heading-fields)))
    (should (equal (mapcar #'car dorg-mutate-grammar-heading-fields)
                   dorg-api--heading-fields))
    (dolist (entry dorg-mutate-grammar-heading-fields)
      (let* ((field (car entry))
             (example (plist-get (cdr entry) :example))
             (patch (car (dorg-api-normalize
                          `(on (id 100)
                               (set (heading ,field) ,example))))))
        (should (equal (list (intern (format ":%s" field)) example)
                       (plist-get patch :fields)))))))

(ert-deftest dorg-test-ltxz7-constructor-clause-domain-is-semantic ()
  "The insert-heading clause domain is exact data, including COMMENTED."
  (let ((expected
         '((title string-or-nil singleton :title headings.title
                  (title "Inbox"))
           (todo token-string-or-nil singleton :todo headings.todo_keyword
                 (todo "TODO"))
           (priority priority-string-or-nil singleton :priority
                     headings.priority (priority "A"))
           (tags tag-list singleton :tags heading_tags (tags "work"))
           (property property-key-and-string repeatable :items property_entries
                     (property "BEAD_ID" "task-1"))
           (commented boolean-nil-or-t singleton :commented
                      headings.commented (commented t))
           (items ordered-item-forms singleton :items heading-items
                  (items (text "Body")))
           (children recursive-heading-constructors singleton :children
                     headings (children (heading (title "Child")))))))
    (should
     (equal expected
            (mapcar
             (lambda (entry)
               (let ((data (cdr entry)))
                 (list (car entry)
                       (plist-get data :value-domain)
                       (plist-get data :cardinality)
                       (plist-get data :ir-field)
                       (plist-get data :projection)
                       (plist-get data :example))))
             dorg-mutate-grammar-insert-heading-clauses)))
    (dolist (entry dorg-mutate-grammar-insert-heading-clauses)
      (let* ((clause (car entry))
             (example (plist-get (cdr entry) :example))
             (patch (car (dorg-api-normalize
                          `(insert (heading ,example) (under document)))))
             (heading (plist-get patch :heading)))
        (ert-info ((format "%S" clause))
          (should (eq 'insert (plist-get patch :op)))
          (should (plist-member heading
                                (plist-get (cdr entry) :ir-field))))))))

(ert-deftest dorg-test-ltxz7-constructor-clause-table-is-generated ()
  "Generated grammar exposes exact constructor clause metadata."
  (let ((rendered (dorg-mutate-grammar-render-org)))
    (should (string-match-p
             (regexp-quote
              "| Clause | Value domain | Cardinality | IR field | Projection |")
             rendered))
    (should (string-match-p
             (regexp-quote
              "| commented | boolean-nil-or-t | singleton | commented | headings.commented |")
             rendered))))

(ert-deftest dorg-test-c8ynt-heading-field-table-is-generated ()
  "Generated reference exposes the exact heading-field domain and projections."
  (let ((rendered (dorg-mutate-grammar-render-org)))
    (should (string-match-p
             (regexp-quote
              "| Field | Value domain | Cardinality | Clearing | Projection |")
             rendered))
    (dolist (projection '(headings.title headings.todo_keyword
                          headings.priority heading_tags headings.commented))
      (should (string-match-p (regexp-quote (symbol-name projection))
                              rendered)))
    (should (string-match-p
             (regexp-quote "| commented | boolean-nil-or-t | singleton | nil | headings.commented |")
             rendered))))

(ert-deftest dorg-test-51gsn-target-projection-law-is-semantic-and-rendered ()
  "Every target declares projection; generated docs expose the where fallback."
  (dolist (entry dorg-mutate-grammar-targets)
    (should (plist-get (cdr entry) :projection)))
  (should
   (eq 'policy-key-when-present-else-existing-heading-id
       (plist-get (alist-get 'where dorg-mutate-grammar-targets) :projection)))
  (let ((rendered (dorg-mutate-grammar-render-org)))
    (should (string-match-p
             (regexp-quote
              "| Target | Cardinality | Missing | Duplicates | Projection | Confirmation |")
             rendered))
    (should (string-match-p
             "policy-key-when-present-else-existing-heading-id" rendered))))

(ert-deftest dorg-test-mutate-grammar-generated-reference-is-byte-exact ()
  "The checked-in Org include is exactly the deterministic inventory rendering."
  (let ((checked-in (dorg-test-mutate-grammar--read
                     "docs/generated/mutate-grammar.org"))
        emitted)
    (should (equal (dorg-mutate-grammar-render-org) checked-in))
    (cl-letf (((symbol-function 'princ)
               (lambda (object &optional _stream)
                 (setq emitted (format "%s" object)))))
      (load (expand-file-name "tools/generate-mutate-grammar.el"
                              dorg-test-mutate-grammar--root)
            nil t))
    (should (equal checked-in emitted)))
  (let ((mutate (dorg-test-mutate-grammar--read "docs/mutate.org")))
    (should (= 1 (dorg-test-mutate-grammar--count
                  "^#\\+INCLUDE: \"generated/mutate-grammar\\.org\"$"
                  mutate)))
    (should-not (string-match-p "^\\*\\* Actions$" mutate))))

(ert-deftest dorg-test-mutate-grammar-cli-node-is-inventory-derived ()
  "The actual Clime mutate node obtains its help and epilog from the inventory."
  (let* ((node (cdr (assoc "mutate" (clime-group-children dorg-cli))))
         (formatted (clime-format-help node '("dorg" "mutate"))))
    (should node)
    (should (equal (dorg-mutate-grammar-cli-help) (clime-node-help node)))
    (should (equal (dorg-mutate-grammar-cli-epilog) (clime-node-epilog node)))
    (dolist (production dorg-mutate-grammar-productions)
      (should (string-match-p
               (regexp-quote (plist-get production :template)) formatted)))
    (should (string-match-p "Keyword sequence" formatted))
    (should (string-match-p "Keyword occurrence" formatted))))

(ert-deftest dorg-test-mutate-grammar-completion-table-is-standard-and-complete ()
  "The public completion table exposes exactly the inventory templates."
  (let* ((candidates (dorg-mutate-grammar-completion-candidates))
         (all (all-completions "" #'dorg-mutate-grammar-completion-table))
         (first (car candidates)))
    (should (= 17 (length candidates)))
    (should (equal candidates all))
    (should (equal t (try-completion first
                                     #'dorg-mutate-grammar-completion-table)))
    (should (equal '(metadata (category . dorg-mutate-production))
                   (funcall #'dorg-mutate-grammar-completion-table
                            "" nil 'metadata)))
    (should (cl-find-if (lambda (s) (string-match-p "(keywords KEY)" s))
                        candidates))
    (should (cl-find-if
             (lambda (s) (string-match-p "(keyword KEY (occurrence N))" s))
             candidates))))

(ert-deftest dorg-test-mutate-grammar-module-stays-a-pure-leaf ()
  "The inventory has no upward API, app, storage, or executor dependency."
  (let ((source (dorg-test-mutate-grammar--read "dorg-mutate-grammar.el")))
    (dolist (forbidden '("dorg-api" "dorg-mutate" "dorg-app" "dorg-dolt"
                         "dorg-backend" "dorg-repo" "dorg-transport"))
      (should-not
       (string-match-p
        (format "(require[ \\t\\n]+['\"]%s" (regexp-quote forbidden))
        source)))))

(provide 'dorg-mutate-grammar-tests)
;;; dorg-mutate-grammar-tests.el ends here
