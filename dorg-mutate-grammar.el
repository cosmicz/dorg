;;; dorg-mutate-grammar.el --- Semantic public mutation grammar -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One pure inventory for the public mutate grammar.  Documentation, CLI help,
;; and Emacs completion render this data; the real parser remains
;; `dorg-api-normalize'.  Tests pass every concrete example through that parser
;; so inventory drift cannot create a second accepted language.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst dorg-mutate-grammar-targets
  '((id :syntax "(id N)" :cardinality exactly-one
        :missing invalid-target :duplicates impossible
        :durability durable-identity :projection numeric-heading-id
        :legal-actions all)
    (key :syntax "(key V)" :cardinality exactly-one
         :missing invalid-target :duplicates identity-violation
         :durability source-identity :projection exact-source-identity
         :legal-actions all)
    (document :syntax "document" :cardinality exactly-one
              :missing impossible :duplicates impossible
              :durability document-host :projection document-host
              :legal-actions (property keyword keywords planning insert-heading-under))
    (where :syntax "(where QUERY)" :cardinality zero-many
           :missing empty-selection :duplicates deduplicated
           :durability query-resolved
           :projection policy-key-when-present-else-existing-heading-id
           :expected-targets required-on-apply
           :legal-actions non-structural-on-actions))
  "Semantic metadata for public mutation targets.")

(defconst dorg-mutate-grammar-heading-fields
  '((title :value-domain string :cardinality singleton
           :clearing empty-string :projection headings.title
           :example "Inbox")
    (todo :value-domain token-string-or-nil :cardinality singleton
          :clearing nil-or-empty-string :projection headings.todo_keyword
          :example "DONE")
    (priority :value-domain priority-string-or-nil :cardinality singleton
              :clearing nil :projection headings.priority
              :example "A")
    (tags :value-domain tag-list :cardinality singleton
          :clearing empty-list :projection heading_tags
          :example ("work"))
    (commented :value-domain boolean-nil-or-t :cardinality singleton
               :clearing nil :projection headings.commented
               :example t))
  "Semantic metadata for intrinsic public heading FIELD values.")

(defconst dorg-mutate-grammar-insert-heading-clauses
  '((title :value-domain string-or-nil :cardinality singleton
           :ir-field :title :projection headings.title
           :example (title "Inbox"))
    (todo :value-domain token-string-or-nil :cardinality singleton
          :ir-field :todo :projection headings.todo_keyword
          :example (todo "TODO"))
    (priority :value-domain priority-string-or-nil :cardinality singleton
              :ir-field :priority :projection headings.priority
              :example (priority "A"))
    (tags :value-domain tag-list :cardinality singleton
          :ir-field :tags :projection heading_tags
          :example (tags "work"))
    (property :value-domain property-key-and-string :cardinality repeatable
              :ir-field :items :projection property_entries
              :example (property "BEAD_ID" "task-1"))
    (commented :value-domain boolean-nil-or-t :cardinality singleton
               :ir-field :commented :projection headings.commented
               :example (commented t))
    (items :value-domain ordered-item-forms :cardinality singleton
           :ir-field :items :projection heading-items
           :example (items (text "Body")))
    (children :value-domain recursive-heading-constructors
              :cardinality singleton :ir-field :children
              :projection headings
              :example (children (heading (title "Child")))))
  "Semantic metadata for public insert-heading constructor clauses.")

(defconst dorg-mutate-grammar-safety-laws
  '((base :cas checked-when-supplied :missing-base unchecked)
    (query-scope :expected-targets required-on-apply)
    (broad-write :threshold independently-gated)
    (dry-run :writes never :preview byte-exact))
  "Safety rules that compose with every public production.")

(defconst dorg-mutate-grammar-verb-laws
  '((set :summary "Assign an intrinsic slot or upsert a unique addressed cell; it may create the cell when absent.")
    (replace-element :summary "Strictly rewrite an addressed existing occurrence; a missing occurrence is an error.")
    (replace-sequence :summary "Make the complete sequence equal the supplied non-empty sequence; keyword sequences may materialize when absent.")
    (insert :summary "Create one element at an explicit position; drawer-line insertion may create its missing first drawer.")
    (delete :summary "Remove the addressed cell, occurrence, sequence, heading, or subtree."))
  "Public mutation verb laws.")

(defconst dorg-mutate-grammar-clearing-laws
  '((property :clearing delete :spelling "(delete (property KEY))")
    (keyword-occurrence :clearing delete
                        :spelling "(delete (keyword KEY (occurrence N)))")
    (keyword-sequence :clearing delete
                      :spelling "(delete (keywords KEY))")
    (heading :clearing empty-assignment
             :spelling "documented nil, empty string, or empty list by FIELD")
    (planning :clearing empty-assignment
              :spelling "nil")
    (items :clearing unsupported-public
           :spelling "no public clear; replace requires at least one item")
    (text :clearing unsupported-public :spelling "no public clear")
    (drawer-line :clearing unsupported-public :spelling "no public clear"))
  "Canonical clearing spelling per exposed public target kind.")

(defconst dorg-mutate-grammar--common-safety
  '(:base when-supplied :expected-targets when-where
    :broad-write threshold-gated)
  "Safety metadata shared by non-structural public productions.")

(defconst dorg-mutate-grammar-productions
  `((:id set-property :family property :verb set
     :template "(set (property KEY) VALUE)"
     :example (on (id 100) (set (property "STATUS") "DONE"))
     :ir (:op put :target-kind :property)
     :collection-address [KEY] :cardinality singleton
     :missing create :positions (:none) :clearing delete
     :safety ,dorg-mutate-grammar--common-safety)
    (:id delete-property :family property :verb delete
     :template "(delete (property KEY))"
     :example (on (id 100) (delete (property "STATUS")))
     :ir (:op delete :target-kind :property)
     :collection-address [KEY] :cardinality singleton
     :missing no-op :positions (:none) :clearing canonical
     :safety ,dorg-mutate-grammar--common-safety)
    (:id replace-keywords :family keyword-sequence :verb replace-sequence
     :template "(replace (keywords KEY) VALUE...)"
     :example (on document (replace (keywords "SEQ_TODO") "TODO" "DONE"))
     :ir (:op replace :target-kind :keywords)
     :collection-address [KEY] :cardinality whole-sequence
     :missing materialize :positions (:none) :clearing delete-only
     :safety ,dorg-mutate-grammar--common-safety)
    (:id delete-keywords :family keyword-sequence :verb delete
     :template "(delete (keywords KEY))"
     :example (on document (delete (keywords "SEQ_TODO")))
     :ir (:op delete :target-kind :keywords)
     :collection-address [KEY] :cardinality whole-sequence
     :missing no-op :positions (:none) :clearing canonical
     :safety ,dorg-mutate-grammar--common-safety)
    (:id replace-keyword :family keyword-occurrence :verb replace-element
     :template "(replace (keyword KEY (occurrence N)) VALUE)"
     :example (on document
                  (replace (keyword "REVIEWERS" (occurrence 2)) "a,b"))
     :ir (:op replace :target-kind :keyword)
     :collection-address [KEY] :cardinality one-occurrence
     :missing error :positions (:none) :clearing delete-only
     :safety ,dorg-mutate-grammar--common-safety)
    (:id delete-keyword :family keyword-occurrence :verb delete
     :template "(delete (keyword KEY (occurrence N)))"
     :example (on document (delete (keyword "REVIEWERS" (occurrence 1))))
     :ir (:op delete :target-kind :keyword)
     :collection-address [KEY] :cardinality one-occurrence
     :missing error :positions (:none) :clearing canonical
     :safety ,dorg-mutate-grammar--common-safety)
    (:id insert-keyword :family keyword-occurrence :verb insert
     :template "(insert (keyword KEY) POSITION VALUE)"
     :example (on document
                  (insert (keyword "REVIEWERS") (before-first) "carol"))
     :ir (:op insert :target-kind :keywords)
     :collection-address [KEY] :cardinality one-new-occurrence
     :missing create-at-host-end :positions (:before-first :after-last)
     :clearing unsupported :safety ,dorg-mutate-grammar--common-safety)
    (:id insert-drawer-line :family drawer-line :verb insert
     :template "(insert (drawer-line NAME (occurrence 1)) POSITION RAW-LINE)"
     :example (on (id 100)
                  (insert (drawer-line "AUDIT" (occurrence 1))
                          (before-first) "- State DONE"))
     :ir (:op insert :target-kind :drawer-lines)
     :collection-address [NAME "(occurrence 1)"]
     :cardinality one-new-line :missing create-or-error
     :positions (:before-first :after-last) :clearing unsupported
     :safety ,dorg-mutate-grammar--common-safety)
    (:id set-heading :family heading-field :verb set
     :template "(set (heading FIELD) VALUE)"
     :example (on (id 100) (set (heading todo) "DONE"))
     :ir (:op update :target-kind :heading)
     :collection-address [FIELD] :cardinality singleton
     :missing error :positions (:none) :clearing empty-assignment-by-field
     :safety ,dorg-mutate-grammar--common-safety)
    (:id set-planning :family planning-field :verb set
     :template "(set (planning FIELD) VALUE)"
     :example (on (id 100) (set (planning scheduled) "<2026-07-01>"))
     :ir (:op update :target-kind :planning)
     :collection-address [FIELD] :cardinality singleton
     :missing error :positions (:none) :clearing nil-assignment
     :safety ,dorg-mutate-grammar--common-safety)
    (:id insert-text :family text :verb insert
     :template "(insert (text) POSITION TEXT)"
     :example (on (id 100) (insert (text) (after-last) "new"))
     :ir (:op insert :target-kind :texts)
     :collection-address [] :cardinality one-new-element
     :missing create-at-host-end :positions (:before-first :after-last)
     :clearing unsupported :safety ,dorg-mutate-grammar--common-safety)
    (:id replace-text :family text :verb replace-element
     :template "(replace (text (occurrence 1)) TEXT)"
     :example (on (id 100) (replace (text (occurrence 1)) "new"))
     :ir (:op replace :target-kind :text)
     :collection-address [] :cardinality first-occurrence-only
     :missing error :positions (:none) :clearing unsupported
     :safety ,dorg-mutate-grammar--common-safety)
    (:id replace-items :family items :verb replace-sequence
     :template "(replace (items) ITEM...)"
     :example (on (id 100) (replace (items) (text "new")))
     :ir (:op replace :target-kind :items)
     :collection-address [] :cardinality whole-non-empty-sequence
     :missing error :positions (:none) :clearing unsupported-public
     :safety (:base when-supplied :expected-targets when-where
              :broad-write full-ordered-item-sequence))
    (:id move :family tree :verb move
     :template "(move (subtree) (under TGT [ANCHOR]))"
     :example (on (id 42)
                  (move (subtree) (under (id 7) (after (id 9)))))
     :ir (:op move :target-kind :subtree)
     :collection-address [] :cardinality one-subtree
     :missing error :positions (:under :before :after)
     :clearing not-applicable :safety ,dorg-mutate-grammar--common-safety)
    (:id delete-heading :family tree :verb delete
     :template "(delete (heading))"
     :example (on (id 100) (delete (heading)))
     :ir (:op delete :target-kind :heading)
     :collection-address [] :cardinality one-heading-promote-children
     :missing error :positions (:none) :clearing destructive
     :safety ,dorg-mutate-grammar--common-safety)
    (:id delete-subtree :family tree :verb delete
     :template "(delete (subtree))"
     :example (on (id 100) (delete (subtree)))
     :ir (:op delete :target-kind :subtree)
     :collection-address [] :cardinality one-subtree
     :missing error :positions (:none) :clearing destructive
     :safety ,dorg-mutate-grammar--common-safety)
    (:id insert-heading :family tree :verb insert
     :template "(insert (heading CLAUSE...) (under TGT [ANCHOR]))"
     :example (insert (heading (title "Inbox") (todo "TODO"))
                      (under document))
     :ir (:op insert :target-kind :tree)
     :collection-address [] :cardinality one-new-subtree
     :missing error :positions (:under :before :after)
     :clearing not-applicable
     :safety (:base when-supplied :expected-targets not-applicable
              :broad-write bounded-constructor-subtree)))
  "Canonical semantic inventory of public mutation productions.")

(defconst dorg-mutate-grammar--family-labels
  '((property . "Property")
    (keyword-sequence . "Keyword sequence")
    (keyword-occurrence . "Keyword occurrence")
    (drawer-line . "Drawer line")
    (heading-field . "Heading field")
    (planning-field . "Planning field")
    (text . "Text")
    (items . "Full item sequence")
    (tree . "Tree structure"))
  "Human labels for grammar production families.")

(defun dorg-mutate-grammar-address-depth (production)
  "Return the address depth declared by PRODUCTION.
Selectors refine the collection addressed by these components; they do not
pad a production whose collection address has depth zero."
  (length (plist-get production :collection-address)))

(defun dorg-mutate-grammar-completion-candidates ()
  "Return public mutation production templates in canonical order."
  (mapcar (lambda (production) (plist-get production :template))
          dorg-mutate-grammar-productions))

(defun dorg-mutate-grammar-completion-table (string predicate action)
  "Complete STRING over public mutation templates.
PREDICATE and ACTION implement the standard Emacs completion-table protocol."
  (if (eq action 'metadata)
      '(metadata (category . dorg-mutate-production))
    (complete-with-action action
                          (dorg-mutate-grammar-completion-candidates)
                          string predicate)))

(defun dorg-mutate-grammar-cli-help ()
  "Return the inventory-derived one-line help for the mutate command."
  (format "Apply one public mutate DSL form (%d semantic productions); requires exactly one of --dry-run / --apply."
          (length dorg-mutate-grammar-productions)))

(defun dorg-mutate-grammar-cli-epilog ()
  "Return the inventory-derived mutate grammar for CLI help."
  (let ((families nil))
    (dolist (production dorg-mutate-grammar-productions)
      (let ((family (plist-get production :family)))
        (unless (memq family families)
          (setq families (append families (list family))))))
    (concat
     "Targets: (id N), (key V), document, or (where QUERY).\n"
     "Wrap non-structural actions in (on TARGET ACTION...).\n\n"
     (mapconcat
      (lambda (family)
        (concat
         (alist-get family dorg-mutate-grammar--family-labels) ":\n  "
         (mapconcat
          (lambda (production) (plist-get production :template))
          (cl-remove-if-not
           (lambda (production) (eq family (plist-get production :family)))
           dorg-mutate-grammar-productions)
          "\n  ")))
      families "\n\n"))))

(defun dorg-mutate-grammar--display (value)
  "Return a compact deterministic display string for metadata VALUE."
  (cond
   ((null value) "none")
   ((symbolp value) (string-remove-prefix ":" (symbol-name value)))
   ((vectorp value)
    (if (= 0 (length value))
        "0"
      (mapconcat #'dorg-mutate-grammar--display (append value nil) ", ")))
   ((listp value)
    (mapconcat #'dorg-mutate-grammar--display value ", "))
   (t (format "%s" value))))

(defun dorg-mutate-grammar--display-safety (safety)
  "Return SAFETY plist as deterministic key=value text."
  (let (parts)
    (while safety
      (setq parts
            (append parts
                    (list (format "%s=%s"
                                  (dorg-mutate-grammar--display (pop safety))
                                  (dorg-mutate-grammar--display (pop safety)))))))
    (string-join parts ", ")))

(defun dorg-mutate-grammar--render-target-table ()
  "Render target metadata as an Org table."
  (concat
   "| Target | Cardinality | Missing | Duplicates | Projection | Confirmation |\n"
   "|--------+-------------+---------+------------+------------+--------------|\n"
   (mapconcat
    (lambda (entry)
      (let ((name (car entry))
            (data (cdr entry)))
        (format "| ~%s~ | %s | %s | %s | %s | %s |"
                (plist-get data :syntax)
                (dorg-mutate-grammar--display (plist-get data :cardinality))
                (dorg-mutate-grammar--display (plist-get data :missing))
                (dorg-mutate-grammar--display (plist-get data :duplicates))
                (dorg-mutate-grammar--display (plist-get data :projection))
                (if (eq name 'where) "expected targets on apply" "none"))))
    dorg-mutate-grammar-targets "\n")
   "\n\n"))

(defun dorg-mutate-grammar--render-law-list (laws)
  "Render semantic LAWS as an Org list."
  (mapconcat
   (lambda (entry)
     (format "- *%s*: %s"
             (dorg-mutate-grammar--display (car entry))
             (plist-get (cdr entry) :summary)))
   laws "\n"))

(defun dorg-mutate-grammar--render-clearing-table ()
  "Render per-kind clearing metadata as an Org table."
  (concat
   "| Kind | Clearing | Canonical spelling |\n"
   "|------+----------+--------------------|\n"
   (mapconcat
    (lambda (entry)
      (let ((data (cdr entry)))
        (format "| %s | %s | ~%s~ |"
                (dorg-mutate-grammar--display (car entry))
                (dorg-mutate-grammar--display (plist-get data :clearing))
                (plist-get data :spelling))))
   dorg-mutate-grammar-clearing-laws "\n")
   "\n\n"))

(defun dorg-mutate-grammar--render-heading-field-table ()
  "Render intrinsic heading-field metadata as an Org table."
  (concat
   "| Field | Value domain | Cardinality | Clearing | Projection |\n"
   "|-------+--------------+-------------+----------+------------|\n"
   (mapconcat
    (lambda (entry)
      (let ((data (cdr entry)))
        (format "| %s | %s | %s | %s | %s |"
                (dorg-mutate-grammar--display (car entry))
                (dorg-mutate-grammar--display
                 (plist-get data :value-domain))
                (dorg-mutate-grammar--display
                 (plist-get data :cardinality))
                (if (and (plist-member data :clearing)
                         (null (plist-get data :clearing)))
                    "nil"
                  (dorg-mutate-grammar--display
                   (plist-get data :clearing)))
                (dorg-mutate-grammar--display
                 (plist-get data :projection)))))
    dorg-mutate-grammar-heading-fields "\n")
   "\n\n"))

(defun dorg-mutate-grammar--render-constructor-clause-table ()
  "Render insert-heading constructor-clause metadata as an Org table."
  (concat
   "| Clause | Value domain | Cardinality | IR field | Projection |\n"
   "|--------+--------------+-------------+----------+------------|\n"
   (mapconcat
    (lambda (entry)
      (let ((data (cdr entry)))
        (format "| %s | %s | %s | %s | %s |"
                (dorg-mutate-grammar--display (car entry))
                (dorg-mutate-grammar--display
                 (plist-get data :value-domain))
                (dorg-mutate-grammar--display
                 (plist-get data :cardinality))
                (dorg-mutate-grammar--display
                 (plist-get data :ir-field))
                (dorg-mutate-grammar--display
                 (plist-get data :projection)))))
    dorg-mutate-grammar-insert-heading-clauses "\n")
   "\n\n"))

(defun dorg-mutate-grammar--render-production-table ()
  "Render the complete semantic production inventory as an Org table."
  (concat
   "| Family | Production | Address depth | Cardinality | Missing | Position | Clearing | Safety |\n"
   "|--------+------------+---------------+-------------+---------+----------+----------+--------|\n"
   (mapconcat
    (lambda (production)
      (format "| %s | ~%s~ | %d | %s | %s | %s | %s | %s |"
              (alist-get (plist-get production :family)
                         dorg-mutate-grammar--family-labels)
              (plist-get production :template)
              (dorg-mutate-grammar-address-depth production)
              (dorg-mutate-grammar--display
               (plist-get production :cardinality))
              (dorg-mutate-grammar--display (plist-get production :missing))
              (dorg-mutate-grammar--display (plist-get production :positions))
              (dorg-mutate-grammar--display (plist-get production :clearing))
              (dorg-mutate-grammar--display-safety
               (plist-get production :safety))))
    dorg-mutate-grammar-productions "\n")
   "\n"))

(defun dorg-mutate-grammar-render-org ()
  "Render the complete generated public mutation grammar as Org text."
  (concat
   "# This file is generated from dorg-mutate-grammar.el.  Do not edit.\n"
   "** Semantic mutation grammar\n\n"
   "The inventory below is the single semantic source for reference prose, "
   "CLI help, and Emacs completion.  The accepted language remains "
   "=dorg-api-normalize=; every inventory example is checked against it.\n\n"
   "*** Targets and safety\n\n"
   (dorg-mutate-grammar--render-target-table)
   "A supplied base revision is checked by CAS; omitting it does not imply a "
   "check.  Query-scoped apply requires the exact targets resolved by dry-run. "
   "Broad-write confirmation is an independent safety gate.\n\n"
   "*** Address-refinement law\n\n"
   "Collection address components refine left-to-right.  An =occurrence= "
   "selector chooses an element of that collection; a position token chooses "
   "an insertion slot.  Arity follows the declared address depth and is never "
   "padded with a selector that has no referent.\n\n"
   "*** Verb laws\n\n"
   (dorg-mutate-grammar--render-law-list dorg-mutate-grammar-verb-laws)
   "\n\nThere is no universal rule that insert is the only creator: property set, "
   "keyword-sequence replace, and bounded drawer-line insertion have the "
   "creation behavior stated above.\n\n"
   "*** Heading fields\n\n"
   "Heading fields are intrinsic singleton slots.  Their value domains, "
   "clearing assignments, and relational projections are explicit below.\n\n"
   (dorg-mutate-grammar--render-heading-field-table)
   "*** Insert-heading constructor clauses\n\n"
   "Constructor clauses initialize one bounded heading subtree in the same mutation. "
   "The =commented= clause is an exact singleton boolean; explicit =nil= is "
   "a value, not omission.  Empty =items= and =children= clauses likewise "
   "remain explicit empty singleton values.  Literal descendants are finite "
   "constructor payload, not expanded targets: their created-set size does "
   "not feed =dorg-api-broad-write-threshold=; the resolved target count "
   "remains independently gated.\n\n"
   (dorg-mutate-grammar--render-constructor-clause-table)
   "*** Clearing law\n\n"
   "Clearing is defined per exposed target kind, with one canonical spelling "
   "for each supported intent.\n\n"
   (dorg-mutate-grammar--render-clearing-table)
   "The full items replacement is a broad hammer: it assigns the complete "
   "ordered item sequence and requires at least one item.  The public grammar "
   "has no clear-items operation.\n\n"
   "*** Productions\n\n"
   (dorg-mutate-grammar--render-production-table)))

(provide 'dorg-mutate-grammar)
;;; dorg-mutate-grammar.el ends here
