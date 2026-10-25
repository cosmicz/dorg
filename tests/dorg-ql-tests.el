;;; dorg-ql-tests.el --- Tests for the dorg-ql core query API -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-ql-select': query validation, the model adapter over
;; `dorg-heading' structs, the plain-org buffer adapter, and the Dolt
;; SQL adapter.  Negative cases cover unknown predicates, wrong
;; arity, and wrong argument types.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-ql)
(require 'dorg-command)
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-registry)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

(defun dorg-test-ql--ids (results)
  "Return the sorted `:heading-id' set of query RESULTS for set comparison."
  (sort (mapcar (lambda (r) (plist-get r :heading-id)) results) #'<))

(defun dorg-test-ql--ids-in-order (results)
  "Return `:heading-id' values from query RESULTS preserving result order."
  (mapcar (lambda (r) (plist-get r :heading-id)) results))

(defun dorg-test-ql--model-from-org (contents)
  "Parse org CONTENTS into a list of non-root `dorg-heading' model structs.
Used to run the model adapter over the SAME text the plain-org and Dolt
backends see, so cross-backend equality is checked against one source."
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (cl-remove-if (lambda (h) (= 0 (dorg-heading-level h)))
                  (dorg-parse-result-headings (dorg-parse-buffer)))))

;;; ─── Validation ───────────────────────────────────────────────────────

(ert-deftest dorg-test-ql-selected-values-render-with-navigation ()
  "Interactive rows display selected values and retain their target identity."
  (let* ((row '(:heading-id 101 :marker nil
               :values (("label" . "Visible") ("missing") ("flag" . :json-false))))
         (buffer (cl-letf (((symbol-function 'display-buffer) #'ignore))
                   (dorg-command--show (list row) '(pipe (select (field title)))
                                       nil))))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "Visible\tnull\tfalse" nil t))
          (should (equal (get-text-property (line-beginning-position)
                                            'dorg-command-result)
                         row)))
      (kill-buffer buffer))))

(ert-deftest dorg-test-ql-dolt-select-title-does-not-read-properties ()
  "A scalar projection uses one revision-bound query and no property read."
  (let* ((reg (make-dorg-registration
               :file "/unused.org"
               :backend (dorg-backend-dolt "/unused" 17)))
         statements)
    (cl-letf (((symbol-function 'dorg-ql--dolt-query)
               (lambda (_dir revision sql &rest _)
                 (should (equal revision "captured"))
                 (push sql statements)
                 '(((id . 23) (c0 . "Chosen"))))))
      (let ((rows (dorg-ql--dolt-eval
                   (dorg-ql-normalize
                    '(pipe (where (id 23)) (select (field title))))
                   reg "captured")))
        (should (equal (plist-get (car rows) :values)
                       '(("title" . "Chosen"))))
        (should (= (length statements) 1))
        (should (string-match-p "h.title AS c0" (car statements)))
        (should-not (string-match-p "heading_properties\\|text_items"
                                    (car statements)))))))

(ert-deftest dorg-test-ql-dolt-extension-field-shares-lowering-and-joins ()
  "Predicate and returned columns use one declared scalar and shared join."
  (let ((dorg--extensions nil) (lowerings 0) statements)
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields
     (list (list :name 'demo/name :type 'string :cardinality 'one
                 :dependencies '((field title))
                 :eval (lambda (_ctx read) (funcall read '(field title)))
                 :sql (lambda (_ctx _aliases)
                        (cl-incf lowerings)
                        '(:expression "demo_q.title"
                          :joins ((demo_q . "LEFT JOIN headings demo_q ON demo_q.id = h.id")))))))
    (cl-letf (((symbol-function 'dorg-ql--dolt-query)
               (lambda (_dir revision sql &rest _)
                 (should (equal revision "captured"))
                 (push sql statements)
                 '(((id . 23) (c0 . "Chosen") (c1 . "Chosen"))))))
      (let ((rows (dorg-ql--dolt-eval
                   (dorg-ql-normalize
                    '(pipe (where (= (field demo/name) "Chosen"))
                           (select (as (field demo/name) label)
                                   (as (field demo/name) again))))
                   (make-dorg-registration
                    :backend (dorg-backend-dolt "/unused" 17)) "captured")))
        (should (equal (plist-get (car rows) :values)
                       '(("label" . "Chosen") ("again" . "Chosen"))))
        (should (= lowerings 1))
        (should (= (length statements) 1))
        (should (= (length (split-string (car statements) "LEFT JOIN headings demo_q")) 2))
        (should (string-match-p "demo_q.title AS c0" (car statements)))
        (should (string-match-p "BINARY demo_q.title = BINARY 'Chosen'"
                                (car statements)))))))

(ert-deftest dorg-test-ql-extension-context-is-captured-once-and-enveloped ()
  "One invocation uses one immutable extension context and reports its digest."
  (let ((dorg--extensions nil) (calls 0)
        (backend (dorg-backend-dolt "/unused" 17)))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :query-context (lambda (invocation)
                      (cl-incf calls)
                      (should (equal (plist-get invocation :revision) "captured"))
                      (should (functionp (plist-get invocation :read)))
                      '(:values (:pause-states ("WAIT")) :provenance "fixture"))
     :fields (list (list :name 'demo/holder :type 'string :cardinality 'one
                         :context '(:pause-states) :dependencies nil
                         :sql (lambda (_context _aliases) '(:expression "NULL")))))
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (_ast _reg revision)
                 (should (equal revision "captured"))
                 (should (equal (plist-get (cdr (assq 'demo dorg-ql--extension-contexts))
                                            :values)
                                '(:pause-states ("WAIT"))))
                 nil)))
      (let ((result (dorg-ql-select-snapshot
                     '(pipe (select (field demo/holder)))
                     (make-dorg-registration :backend backend))))
        (should (= calls 1))
        (should (equal (plist-get result :rows) nil))
        (should (equal (plist-get result :snapshot)
                       '(:kind dolt :head "captured")))
        (should (stringp (plist-get result :context-digest)))))))

(ert-deftest dorg-test-ql-snapshot-public-keeps-column-names-and-hides-markers ()
  "The public snapshot data is shared, JSON-ready, and keeps string names opaque."
  (let* ((data
          (dorg-ql-snapshot-public
           '(:source (:kind plain-org :file "/tmp/tracker.org")
             :root nil :snapshot (:kind plain-org :content-sha256 "contents")
             :context-digest "context"
             :rows ((:heading-id 9 :marker captured-marker
                     :values (("matched_seat" . "Alpha") ("BEAD_ID" . "x-1")))
                    (:heading-id 10 :marker captured-marker :title "Beta"
                     :properties (("BEAD_ID" . "x-2")))))))
         (rows (plist-get data :results)))
    (should (equal (plist-get data :source)
                   '(:kind plain-org :file "/tmp/tracker.org")))
    (should (equal (plist-get data :snapshot)
                   '(:kind plain-org :content-sha256 "contents")))
    (should (equal (aref rows 0)
                   '(("matched_seat" . "Alpha") ("BEAD_ID" . "x-1"))))
    (should (equal (plist-get (aref rows 1) :properties)
                   '(("BEAD_ID" . "x-2"))))
    (should-not (plist-member (aref rows 1) :marker))))

(ert-deftest dorg-test-ql-extension-context-copies-mutable-values ()
  "Resolver-owned strings and vectors cannot change a captured invocation."
  (let ((dorg--extensions nil)
        (original (list :label (copy-sequence "queued") :ids [1 2])))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :query-context (lambda (_invocation) (list :values original))
     :fields (list (list :name 'demo/holder :type 'string :cardinality 'one
                         :context '(:label :ids) :dependencies nil
                         :eval (lambda (_context _read) nil))))
    (let* ((context (dorg-ql--resolve-extension-contexts
                     (dorg-ql-normalize '(pipe (select (field demo/holder))))
                     '(:source fixture)))
           (values (plist-get (cdr (assq 'demo (plist-get context :contexts)))
                              :values)))
      (aset (plist-get original :ids) 0 99)
      (aset (plist-get original :label) 0 ?X)
      (should (equal (plist-get values :label) "queued"))
      (should (equal (plist-get values :ids) [1 2])))))

;;; ─── Invocation-local extension descriptor plans ───────────────────────

(defun dorg-test-ql--descriptor-plan-lookups (heading-count &optional mutation)
  "Run an extension predicate over HEADING-COUNT models and return lookups.
When MUTATION is non-nil, bypass the invocation plan.  That is the RED
control: descriptor lookup must then grow with the number of rows, whereas
the real public invocation is bounded by the one declared extension field."
  (let ((dorg--extensions nil)
        (lookups 0)
        (headings (cl-loop for id from 1 to heading-count
                           collect (let ((heading (dorg-test-ql--heading
                                                   id (format "H%d" id))))
                                     (setf (dorg-heading-todo heading) "TODO")
                                     heading)))
        (resolver (symbol-function 'dorg-ext-query-field)))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields (list (list :name 'demo/open :type 'boolean :cardinality 'one
                         :dependencies '((field todo))
                         :eval (lambda (_context read)
                                 (equal (funcall read '(field todo)) "TODO")))))
    (cl-letf (((symbol-function 'dorg-ext-query-field)
               (lambda (field)
                 (cl-incf lookups)
                 (funcall resolver field))))
      (if mutation
          (cl-letf (((symbol-function 'dorg-ql--query-field)
                     (lambda (field) (dorg-ext-query-field field))))
            (should (= heading-count
                       (length
                        (dorg-ql-select
                         '(pipe (where (= (field demo/open) t))
                                (select (field title) (property "K")))
                         headings)))))
        (should (= heading-count
                   (length
                    (dorg-ql-select
                     '(pipe (where (= (field demo/open) t))
                            (select (field title) (property "K")))
                     headings)))))
      lookups)))

(ert-deftest dorg-test-ql-descriptor-plan-is-field-bounded-not-row-bounded ()
  "A public predicate plus built-in/property projection resolves per field.
Ten and one thousand model headings exercise the same query shape.  The
mutation bypasses the plan and demonstrates that this control would become
row-bound if the invocation binding were removed."
  (let ((small (dorg-test-ql--descriptor-plan-lookups 10))
        (large (dorg-test-ql--descriptor-plan-lookups 1000))
        (mutated-small (dorg-test-ql--descriptor-plan-lookups 10 t))
        (mutated-large (dorg-test-ql--descriptor-plan-lookups 1000 t)))
    ;; One extension field may be consulted during normalization, plan capture,
    ;; and adapter setup, but never once per selected heading.
    (should (= small large))
    (should (<= large 8))
    (should (> mutated-large mutated-small))
    (should (> mutated-large large))))

(ert-deftest dorg-test-ql-descriptor-plan-observes-extension-replacement ()
  "Each public invocation captures the currently registered descriptor only."
  (let ((dorg--extensions nil)
        (headings (list (dorg-test-ql--heading 1 "Alpha"))))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields (list (list :name 'demo/value :type 'string :cardinality 'one
                         :dependencies nil
                         :eval (lambda (_context _read) "open"))))
    (should (equal (dorg-ql-select
                    '(pipe (where (= (field demo/value) "open"))
                           (select (field title)))
                    headings)
                   '((:heading-id 1 :marker nil :values (("title" . "Alpha"))))))
    ;; A changed type and cardinality must affect the very next invocation;
    ;; a process-global plan would incorrectly retain the first descriptor.
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 2
     :fields (list (list :name 'demo/value :type 'boolean :cardinality 'many
                         :dependencies nil
                         :eval (lambda (_context _read) (list t :json-false)))))
    (should-error
     (dorg-ql-select '(pipe (where (= (field demo/value) "open"))) headings)
     :type 'dorg-ql-error)
    (should (equal (plist-get (car (dorg-ql-select
                                    '(pipe (select (field demo/value))) headings))
                              :values)
                   '(("demo/value" . [t :json-false]))))))

(defun dorg-test-ql--dolt-descriptor-plan-lookups (heading-count)
  "Return registry lookups for one real-Dolt query over HEADING-COUNT rows."
  (let ((dorg--extensions nil)
        (lookups 0)
        (resolver (symbol-function 'dorg-ext-query-field)))
    (dorg-test-ql--with-dolt-synced
        (mapconcat (lambda (id) (format "* H%d\n" id))
                   (number-sequence 1 heading-count) "")
        (:buffer buffer)
      (dorg-register-extension
       'demo :schema "SELECT 1" :version 1
       :fields (list (list :name 'demo/open :type 'boolean :cardinality 'one
                           :dependencies nil
                           :sql (lambda (_context _aliases) '(:expression "TRUE")))))
      (cl-letf (((symbol-function 'dorg-ext-query-field)
                 (lambda (field)
                   (cl-incf lookups)
                   (funcall resolver field))))
        ;; Built-in title plus a property keep the projection extension-free.
        ;; The extension predicate drives SQL planning while row decoding
        ;; still consults cardinality/type metadata through the invocation
        ;; plan; the property also proves its nil field marker is not looked up.
        (should (= heading-count
                   (length
                    (dorg-ql-select
                     '(pipe (where (= (field demo/open) t))
                            (select (field title) (property "K")))
                     buffer))))))
    lookups))

(ert-deftest dorg-test-ql-dolt-descriptor-plan-reaches-row-decoder ()
  "The real Dolt planner and row decoder retain the same field-bound plan."
  (let ((small (dorg-test-ql--dolt-descriptor-plan-lookups 10))
        (large (dorg-test-ql--dolt-descriptor-plan-lookups 1000)))
    (should (= small large))
    (should (<= large 8))))

(defun dorg-test-ql--plain-dependency-plan-lookups (heading-count)
  "Return resolver lookups for a plain extension projection with a dependency."
  (let ((dorg--extensions nil)
        (lookups 0)
        (resolver (symbol-function 'dorg-ext-query-field)))
    (dorg-test-ql--with-plain-org-file
        (mapconcat (lambda (id) (format "* TODO H%d\n" id))
                   (number-sequence 1 heading-count) "")
        (:buffer buffer)
      (dorg-register-extension
       'demo :schema "SELECT 1" :version 1
       :fields (list (list :name 'demo/open :type 'boolean :cardinality 'one
                           :dependencies '((field todo))
                           :eval (lambda (_context read)
                                   (equal (funcall read '(field todo)) "TODO")))))
      (cl-letf (((symbol-function 'dorg-ext-query-field)
                 (lambda (field)
                   (cl-incf lookups)
                   (funcall resolver field))))
        ;; TODO is deliberately absent from the query: only the declared
        ;; evaluator dependency can put its built-in negative entry in plan.
        (should (= heading-count
                   (length (dorg-ql-select
                            '(pipe (select (field demo/open))) buffer))))))
    lookups))

(ert-deftest dorg-test-ql-plain-dependency-plan-is-field-bounded ()
  "An unselected stored dependency is planned once for plain-org evaluation."
  (let ((small (dorg-test-ql--plain-dependency-plan-lookups 10))
        (large (dorg-test-ql--plain-dependency-plan-lookups 1000)))
    (should (= small large))
    (should (<= large 8))))

(ert-deftest dorg-test-ql-nested-public-query-captures-its-own-field-plan ()
  "A nested public invocation never evaluates under its caller's plan."
  (let ((dorg--extensions nil)
        (headings (list (dorg-test-ql--heading 1 "Alpha"))))
    (dorg-register-extension
     'inner :schema "SELECT 1" :version 1
     :fields (list (list :name 'inner/value :type 'string :cardinality 'one
                         :dependencies nil
                         :eval
                         (lambda (_context _read)
                           ;; The registry is unavailable at evaluation time;
                           ;; success proves nested execution bound its plan.
                           (cl-letf (((symbol-function 'dorg-ext-query-field)
                                      (lambda (&rest _)
                                        (ert-fail "nested plan fell through to registry"))))
                             (should (eq (plist-get (dorg-ql--query-field 'inner/value)
                                                    :extension)
                                         'inner))
                             "inner")))))
    (dorg-register-extension
     'outer :schema "SELECT 1" :version 1
     :fields (list (list :name 'outer/value :type 'string :cardinality 'one
                         :dependencies nil
                         :eval
                         (lambda (_context _read)
                           (cdr (assoc "inner/value"
                                       (plist-get
                                        (car (dorg-ql-select
                                              '(pipe (select (field inner/value)))
                                              headings))
                                        :values)))))))
    (should (equal (plist-get
                    (car (dorg-ql-select '(pipe (select (field outer/value)))
                                         headings))
                    :values)
                   '(("outer/value" . "inner"))))))

(defun dorg-test-ql--v1-identity-key (value)
  "Return the superseded v1 canon-slot key for focused regression witnesses."
  (let* ((table (char-table-extra-slot (standard-case-table) 1))
         (canonical (apply #'string
                           (mapcar (lambda (char) (or (aref table char) char))
                                   value))))
    (concat "v1:"
            (mapconcat (lambda (byte) (format "%02x" byte))
                       (encode-coding-string canonical 'utf-8 t) ""))))

(ert-deftest dorg-test-ql-identity-key-includes-codec-version ()
  "Persisted identity comparison keys have an explicit migration version."
  (should (equal dorg-query-identity-key-version 2))
  (should (equal (dorg-query-identity-key "A") "v2:41")))

(ert-deftest dorg-test-ql-identity-key-matches-standard-case-equivalence ()
  "V2 is pinned characterwise casing, never v1 canonical merging or expansion."
  (cl-labels ((standard-equal-p (a b)
                (with-case-table (standard-case-table)
                  (string-equal-ignore-case a b))))
    ;; These equal classes include final sigma and sharp-s, but never expand a
    ;; ligature or sharp-s into two characters.
    (dolist (pair '(("Σ" "ς") ("σ" "Σ") ("ß" "ẞ")
                    ("straße" "STRAẞE")))
      (should (standard-equal-p (car pair) (cadr pair)))
      (should (equal (dorg-query-identity-key (car pair))
                     (dorg-query-identity-key (cadr pair)))))
    (dolist (pair '(("ß" "SS") ("ﬀ" "FF") ("straße" "STRASSE")
                    ("Ω" "ω") ("Å" "å") ("K" "k") ("ϴ" "θ")))
      (should-not (standard-equal-p (car pair) (cadr pair)))
      (should-not (equal (dorg-query-identity-key (car pair))
                         (dorg-query-identity-key (cadr pair)))))
    ;; This is the nonvacuous v1 witness: its canon slot merged these three
    ;; distinct standard-case classes.  Kelvin was already distinct in v1.
    (dolist (pair '(("Ω" "ω") ("Å" "å") ("ϴ" "θ")))
      (should (equal (dorg-test-ql--v1-identity-key (car pair))
                     (dorg-test-ql--v1-identity-key (cadr pair)))))
    ;; Buffer-local case tables must not alter the persisted codec.
    (let ((key (dorg-query-identity-key "i"))
          (table (copy-case-table (standard-case-table))))
      (set-case-syntax-pair ?İ ?i table)
      (with-temp-buffer
        (set-case-table table)
        (should (string-equal-ignore-case "i" "İ"))
        (should (equal key (dorg-query-identity-key "i")))))
    ;; ASCII unibyte text has the documented case semantics; non-ASCII bytes
    ;; and raw-byte characters must not collide with Unicode UTF-8 text.
    (should (equal (dorg-query-identity-key (unibyte-string ?a))
                   (dorg-query-identity-key "A")))
    (should-error (dorg-query-identity-key (unibyte-string #xc3 #xa9))
                  :type 'dorg-ql-error)
    (should-error
     (dorg-query-identity-key
      (string (unibyte-char-to-multibyte #xc3)
              (unibyte-char-to-multibyte #xa9)))
     :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-identity-codec-is-shared-by-model-and-sql ()
  "Identity evaluation and SQL lowering use the same versioned equivalence."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields
     (list (list :name 'demo/identity :type 'identity :cardinality 'one
                 :dependencies '((field title))
                 :eval (lambda (_context read) (funcall read '(field title)))
                 :sql (lambda (_context _aliases)
                        '(:expression "x.display_value" :key-expression "x.identity_key")))))
    ;; Standard-case Unicode pairs are part of the persisted codec contract,
    ;; not adapter-specific string comparison behavior.
    (dolist (pair '(("Σ" "ς") ("ß" "ẞ")))
      (should (equal (dorg-query-identity-key (car pair))
                     (dorg-query-identity-key (cadr pair)))))
    (let ((headings (list (make-dorg-heading :id 1 :level 1 :level-offset 1
                                              :title "ς"))))
      (should (= 1 (length
                    (dorg-ql-select
                     '(pipe (where (= (field demo/identity) "Σ"))
                            (select (field demo/identity)))
                     headings)))))
    (require 'dorg-ql-dolt)
    (let ((dorg-ql--dolt-field-plans nil)
          (key (dorg-query-identity-key "Σ")))
      (should (string-match-p (regexp-quote key)
                              (dorg-ql--dolt-field-comparison
                               'demo/identity '= "Σ" 1))))))

(ert-deftest dorg-test-ql-context-digest-ignores-caller-print-limits ()
  "Context identity is independent of caller printing and object sharing."
  (let ((print-length 2))
    (should-not
     (equal (dorg-ql--context-digest '(:values (:states ("a" "b" "c"))))
            (dorg-ql--context-digest '(:values (:states ("a" "b" "d")))))))
  (let ((shared (copy-sequence "x")))
    (should (equal (dorg-ql--context-digest (list :p shared :q shared))
                   (dorg-ql--context-digest
                    (list :p (copy-sequence "x") :q (copy-sequence "x"))))))
  (should-error (dorg-ql--copy-context-value (make-hash-table))
                :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-projection-sql-is-fresh-across-real-queries ()
  "Sequential real-Dolt projections cannot retain a prior select-list spine."
  (dorg-test-ql--with-dolt-synced
      "* TODO [#A] Alpha\n** Child\n" (:buffer buffer)
    (dorg-ql-select '(pipe (select (field level) (field title))) buffer)
    (let ((rows (dorg-ql-select
                 '(pipe (select (field title) (field level) (field id))) buffer)))
      (let ((values (plist-get (car rows) :values)))
        (should (equal (butlast values)
                       '(("title" . "Alpha") ("level" . 1))))
        (should (integerp (cdr (assoc "id" values))))))))

(ert-deftest dorg-test-ql-dolt-boolean-extension-projects-wire-values ()
  "Dolt 0/1 booleans normalize to the same values as the evaluator."
  (let* ((contents "* TODO Open\n* Plain\n")
         (model (dorg-test-ql--model-from-org contents))
         (dorg--extensions nil))
    (dorg-test-ql--with-dolt-synced
        contents (:buffer buffer)
      (dorg-register-extension
       'demo :schema "SELECT 1" :version 1
       :fields (list (list :name 'demo/open :type 'boolean :cardinality 'one
                           :dependencies '((field todo))
                           :eval (lambda (_context read)
                                   (if (funcall read '(field todo)) :json-false t))
                           :sql (lambda (_context _aliases)
                                  '(:expression "(h.todo_keyword IS NULL)")))))
      (let ((values (mapcar (lambda (row) (plist-get row :values))
                            (dorg-ql-select
                             '(pipe (select (field title) (field demo/open))) buffer))))
        (should (equal values
                       '((("title" . "Open") ("demo/open" . :json-false))
                         (("title" . "Plain") ("demo/open" . t)))))
        (should (equal values
                       (mapcar (lambda (row) (plist-get row :values))
                               (dorg-ql-select
                                '(pipe (select (field title) (field demo/open)))
                                model))))))))

(ert-deftest dorg-test-ql-dolt-extension-projection-accepts-one-and-many-where ()
  "The r2 one-wrapper and combined SQL where shape project extension fields."
  (let ((dorg--extensions nil))
    (dorg-test-ql--with-dolt-synced "* TODO Alpha\n* DONE Beta\n" (:buffer buffer)
      (dorg-register-extension
       'demo :schema "SELECT 1" :version 1
       :fields
       (list (list :name 'demo/name :type 'string :cardinality 'one
                   :dependencies '((field title))
                   :sql (lambda (_context _aliases) '(:expression "h.title")))))
      (let* ((alpha-id (plist-get (car (dorg-ql-select '(title "Alpha") buffer))
                                   :heading-id))
             (one (dorg-ql-select
                   `(one (pipe (where (id ,alpha-id)) (select (field demo/name))))
                   buffer))
             (many (dorg-ql-select
                    '(pipe (where (= (field todo) "TODO"))
                           (where (= (field demo/name) "Alpha"))
                           (select (field demo/name)))
                    buffer)))
        (should (equal (mapcar (lambda (row) (plist-get row :values)) one)
                       '((("demo/name" . "Alpha")))))
        (should (equal (mapcar (lambda (row) (plist-get row :values)) many)
                       '((("demo/name" . "Alpha")))))))))

(ert-deftest dorg-test-ql-projection-parity-covers-pushdown-and-residual ()
  "Built-in projection values agree for SQL pushdown, residual, and plain-org."
  (let* ((contents "* Alpha\n** Child\n* Beta\n")
         (projection '(pipe (select (field title) (field level))))
         ;; `contains' deliberately remains residual, while the equivalent
         ;; projection-only form uses the bounded SQL path.
         (residual '(pipe (where (not (contains (field title) "zzz")))
                          (select (field title) (field level)))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let ((sql-values (mapcar (lambda (row) (plist-get row :values))
                                (dorg-ql-select projection dbuf)))
            (residual-values (mapcar (lambda (row) (plist-get row :values))
                                     (dorg-ql-select residual dbuf))))
        (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
          (should (equal sql-values
                         (mapcar (lambda (row) (plist-get row :values))
                                 (dorg-ql-select projection pbuf))))
          (should (equal residual-values
                         (mapcar (lambda (row) (plist-get row :values))
                                 (dorg-ql-select residual pbuf)))))))))

(ert-deftest dorg-test-ql-dolt-collection-projection-is-keyed-and-ordered ()
  "A many field hydrates selected keys once and never multiplies heading rows."
  (let ((dorg--extensions nil)
        (reg (make-dorg-registration :backend (dorg-backend-dolt "/unused" 17))))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields (list (list :name 'demo/tags :type 'string :cardinality 'many
                         :dependencies nil
                         :sql (lambda (_context _aliases)
                                (list :batch (lambda (ids)
                                               (should (equal ids '(23)))
                                               "SELECT collection rows"))))))
    (cl-letf (((symbol-function 'dorg-ql--dolt-query)
               (lambda (_dir revision sql &rest _)
                 (should (equal revision "captured"))
                 (if (string-match-p "collection rows" sql)
                     '(((heading_id . 23) (ordinal . 2) (value . "second"))
                       ((heading_id . 23) (ordinal . 1) (value . "first")))
                   '(((id . 23)))))))
      (let ((rows (dorg-ql--dolt-eval
                   (dorg-ql-normalize '(pipe (where (id 23))
                                             (select (field demo/tags))))
                   reg "captured")))
        (should (equal (plist-get (car rows) :values)
                       '(("demo/tags" . ["first" "second"]))))))))

(ert-deftest dorg-test-ql-dolt-collection-projection-real-store ()
  "A real Dolt batch preserves one row per heading and collection order."
  (let ((dorg--extensions nil))
    (dorg-test-ql--with-dolt-synced
        "* First\n* Second\n" (:dolt-dir dolt-dir :reg reg :buffer buffer)
      (let ((root (dorg-backend-root-id (dorg-registration-backend reg))))
        (dorg-register-extension
         'demo
         :schema (concat "CREATE TABLE demo_tags ("
                         "heading_id BIGINT, root_id BIGINT, ordinal INT, value TEXT, "
                         "PRIMARY KEY (heading_id, ordinal))")
         :version 1
         :fields
         (list
          (list :name 'demo/tags :type 'string :cardinality 'many :dependencies nil
                :sql
                (lambda (_context aliases)
                  (let ((field-root (plist-get aliases :root)))
                    (list :batch
                          (lambda (ids)
                            (format
                             (concat "SELECT heading_id, ordinal, value FROM demo_tags"
                                     " WHERE root_id = %d AND heading_id IN (%s)"
                                     " ORDER BY heading_id, ordinal")
                             field-root
                             (mapconcat #'number-to-string ids ", ")))))))))
        (dorg-migrate-schema dolt-dir)
        (let ((ids (mapcar (lambda (row) (plist-get row :heading-id))
                           (dorg-ql-select '(pipe (select (field id))) buffer))))
          (dorg-sql dolt-dir
                    "INSERT INTO demo_tags (heading_id, root_id, ordinal, value) VALUES (%d, %d, 2, 'second')"
                    (car ids) root)
          (dorg-sql dolt-dir
                    "INSERT INTO demo_tags (heading_id, root_id, ordinal, value) VALUES (%d, %d, 1, 'first')"
                    (car ids) root)
          (dorg-commit dolt-dir "fixture: collection rows")
          (let ((rows (dorg-ql-select
                       '(pipe (select (field demo/tags))) buffer)))
            (should (= (length rows) 2))
            (should (equal (plist-get (car rows) :values)
                           '(("demo/tags" . ["first" "second"]))))
            (should (equal (plist-get (cadr rows) :values)
                           '(("demo/tags" . [])))))
          ;; A store commit after heading selection but before collection
          ;; hydration must not leak into this invocation's captured revision.
          (let ((advanced nil)
                (original (symbol-function 'dorg-ql--dolt-query)))
            (cl-letf (((symbol-function 'dorg-ql--dolt-query)
                       (lambda (dir revision sql &rest args)
                         (let ((rows (apply original dir revision sql args)))
                           (when (and (not advanced)
                                      (string-match-p "FROM qtree q JOIN headings h" sql))
                             (setq advanced t)
                             (dorg-sql
                              dolt-dir
                              (concat "UPDATE demo_tags SET value = 'changed' "
                                      "WHERE heading_id = %d AND root_id = %d AND ordinal = 1")
                              (car ids) root)
                             (dorg-commit dolt-dir "fixture: advance during query"))
                           rows))))
              (let ((rows (dorg-ql-select
                           '(pipe (select (field demo/tags))) buffer)))
                (should advanced)
                (should (equal (plist-get (car rows) :values)
                               '(("demo/tags" . ["first" "second"]))))))
            ;; A later invocation observes the write.  The prior assertion
            ;; therefore witnesses revision pinning, not a no-op update.
            (should (equal (plist-get (car (dorg-ql-select
                                            '(pipe (select (field demo/tags))) buffer))
                                      :values)
                           '(("demo/tags" . ["changed" "second"]))))))))))

(ert-deftest dorg-test-ql-dolt-select-property-is-key-and-id-bounded ()
  "Only requested keys for selected IDs are read at the captured revision."
  (let* ((reg (make-dorg-registration
               :file "/unused.org"
               :backend (dorg-backend-dolt "/unused" 17)))
         statements)
    (cl-letf (((symbol-function 'dorg-ql--dolt-query)
               (lambda (_dir revision sql &rest args)
                 (should (equal revision "captured"))
                 (setq sql (apply #'format sql args))
                 (push sql statements)
                 (if (string-match-p "heading_properties" sql)
                     '(((heading_id . 23) (prop_key . "BEAD_ID")
                        (prop_value . "example-one")))
                   '(((id . 23)))))))
      (let ((rows (dorg-ql--dolt-eval
                   (dorg-ql-normalize
                    '(pipe (where (id 23))
                           (select (as (property "BEAD_ID") id))))
                   reg "captured")))
        (should (equal (plist-get (car rows) :values)
                       '(("id" . "example-one"))))
        (should (= (length statements) 2))
        (should (string-match-p "heading_id IN (23)" (car statements)))
        (should (string-match-p "prop_key IN ('BEAD_ID')" (car statements)))))))

(ert-deftest dorg-test-ql-public-reason-set-is-closed ()
  "The public query condition has exactly two top-level reason classes."
  (should (equal '(invalid-query unsupported) dorg-ql--error-reasons)))

(ert-deftest dorg-test-ql-invalid-query-carries-typed-data ()
  "Grammar and value failures carry the `invalid-query' reason."
  (dolist (query '((between 1 2) id (id "not-numeric")))
    (let ((err (should-error (dorg-ql-select query nil)
                             :type 'dorg-ql-error)))
      (should (eq 'invalid-query (dorg-error-reason err)))
      (should (equal '(:reason invalid-query) (dorg-error-data err))))))

(ert-deftest dorg-test-ql-validate-unknown-predicate ()
  "An unknown predicate signals typed `invalid-query'."
  (should-error (dorg-ql-select '(between 1 2) nil)
                :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-validate-non-list-form ()
  "A non-list query form signals `dorg-ql-error'."
  (should-error (dorg-ql-select 'id nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select 42 nil) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-validate-wrong-arity ()
  "Wrong arity for each predicate signals `dorg-ql-error'."
  (should-error (dorg-ql-select '(id) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(id 1 2) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property "K") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property "K" "V" "X") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(title) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(tag) nil) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-validate-wrong-types ()
  "Wrong argument types signal `dorg-ql-error'."
  (should-error (dorg-ql-select '(id "not-numeric") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property foo "v") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property "k" 5) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(title 5) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(tag 5) nil) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-validate-id-string-normalized ()
  "A numeric string id is accepted (normalized to integer)."
  (let ((headings (list (make-dorg-heading :id 7 :title "Seven" :level 1 :level-offset 1))))
    (let ((results (dorg-ql-select '(id "7") headings)))
      (should (= 1 (length results)))
      (should (= 7 (plist-get (car results) :heading-id))))))

(ert-deftest dorg-test-ql-model-list-v2-filter-public-facade ()
  "Public `dorg-ql-select' runs filter-only v2 forms on model lists.
This is non-vacuous against the old deferred contract: the same call used
to signal `dorg-ql-error'.  The source is an ordered model-list forest and
the public result order is the input order."
  (let ((headings (list (dorg-test-ql--heading 1 "Alpha" '(("FOO" . "bar"))
                                               '("work"))
                        (dorg-test-ql--heading 2 "Beta" '(("FOO" . "baz")))
                        (dorg-test-ql--heading 3 "Gamma" nil '("home")))))
    (should (equal '(1 2)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(and (contains (field title) "a")
                           (exists (property "FOO")))
                     headings))))
    (should (equal '(1)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(and (tag "work") (prefix (property "FOO") "ba"))
                     headings))))))

(ert-deftest dorg-test-ql-model-list-v2-keyword-and-timestamp-public-facade ()
  "Model-list v2 facade supports keyword and timestamp filters."
  (let ((headings
         (list
          (make-dorg-heading
           :id 1 :parent-id nil :level 1 :level-offset 1 :title "Exported"
           :items (list (dorg-keyword-item :keyword "EXPORT_FILE_NAME"
                                           :value "exported")
                        (dorg-planning-item :deadline "<2026-06-03 Wed>")))
          (make-dorg-heading
           :id 2 :parent-id nil :level 1 :level-offset 1 :title "Plain"
           :items (list (dorg-planning-item
                         :deadline "<2026-07-03 Fri>"))))))
    (should (equal '(1)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(and (keyword "EXPORT_FILE_NAME" "exported")
                           (deadline "2026-06-01" "2026-06-30"))
                     headings))))
    (should (equal '(2)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(missing (keyword "EXPORT_FILE_NAME"))
                     headings))))))

(ert-deftest dorg-test-ql-model-list-v2-filter-allows-bad-identity ()
  "Filter-only model-list v2 forms do not require traversal identity.
Nil and duplicate ids are preserved in public results when no traversal
or unique step needs identity."
  (let ((headings (list (make-dorg-heading :id nil :level 1 :level-offset 1 :title "No Id")
                        (make-dorg-heading :id 7 :level 1 :level-offset 1 :title "First")
                        (make-dorg-heading :id 7 :level 1 :level-offset 1 :title "Second"))))
    (should (equal '(nil 7 7)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select '(>= (field level) 1) headings))))
    ;; Nil id is a non-match for the identity predicate, not a raw type error.
    (should (equal '(7 7)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select '(id >= 7) headings))))))

(ert-deftest dorg-test-ql-model-list-v2-traversal-partial-forest ()
  "Model-list traversal operates over only the supplied ordered forest.
Missing parents are absent roots; traversal never manufactures headings
outside the model list."
  (let ((headings (list (make-dorg-heading :id 2 :parent-id 1 :level 2 :level-offset 1
                                           :title "Child")
                        (make-dorg-heading :id 3 :parent-id 2 :level 3 :level-offset 1
                                           :title "Grandchild")
                        (make-dorg-heading :id 4 :parent-id 99 :level 2 :level-offset 1
                                           :title "Orphan"))))
    ;; Parent 1 and parent 99 are absent from the supplied partial forest.
    (should (null (dorg-ql-select
                   '(pipe (where (id 2)) (traverse parent))
                   headings)))
    (should (equal '(3)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(pipe (where (id 2)) (traverse children))
                     headings))))
    (should (equal '(2)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(pipe (where (id 3)) (traverse ancestors))
                     headings))))))

(ert-deftest dorg-test-ql-model-list-v2-identity-required-for-traversal-and-unique ()
  "Traversal/unique require unique non-nil ids in model-list sources.
These assertions are non-vacuous: replacing the duplicate/nil sources
with the GOOD source makes the same pipeline succeed."
  (let ((good (list (make-dorg-heading :id 1 :parent-id nil :level 1 :level-offset 1
                                       :title "Parent")
                    (make-dorg-heading :id 2 :parent-id 1 :level 2 :level-offset 1
                                       :title "Child")))
        (nil-id (list (make-dorg-heading :id nil :level 1 :level-offset 1 :title "No Id")
                      (make-dorg-heading :id 2 :parent-id nil :level 1 :level-offset 1
                                         :title "Other")))
        (duplicate (list (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "One")
                         (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "Again"))))
    (should (equal '(2)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select
                     '(pipe (where (id 1)) (traverse children))
                     good))))
    (should-error (dorg-ql-select
                   '(pipe (where (>= (field level) 1)) (traverse children))
                   nil-id)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select
                   '(pipe (where (>= (field level) 1)) (unique))
                   nil-id)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select
                   '(pipe (where (>= (field level) 1)) (traverse children))
                   duplicate)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select
                   '(pipe (where (>= (field level) 1)) (unique))
                   duplicate)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-model-list-v2-public-facade-matches-model-eval ()
  "Public model-list v2 facade returns the internal evaluator baseline."
  (let* ((headings (dorg-test-ql--model-from-org
                    "* Alpha\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n** Alpha Child\n:PROPERTIES:\n:DORG_ID: 2\n:END:\n* Beta\n:PROPERTIES:\n:DORG_ID: 3\n:END:\n"))
         (query '(pipe (where (prefix (field title) "Alpha"))
                       (traverse self children)
                       (unique))))
    (should (equal (dorg-ql--model-eval (dorg-ql-normalize query) headings)
                   (dorg-ql-select query headings)))))

;;; ─── Timestamp Predicate Metadata (single source of truth) ────────────

(ert-deftest dorg-test-ql-metadata-each-predicate-once ()
  "Every timestamp predicate is described exactly once in the metadata.
Guards against a predicate landing in the predicate set or validation
without a metadata entry — or appearing twice."
  (let ((keys (mapcar #'car dorg-ql--timestamp-predicate-metadata)))
    ;; No duplicate keys.
    (should (equal (sort (copy-sequence keys) #'string<)
                   (sort (delete-dups (copy-sequence keys)) #'string<)))
    ;; The derived predicate set is exactly the metadata keys.
    (should (equal (sort (copy-sequence dorg-ql--timestamp-predicates) #'string<)
                   (sort (copy-sequence keys) #'string<)))
    ;; Arity is the only metadata field consumed by production normalization.
    (dolist (entry dorg-ql--timestamp-predicate-metadata)
      (let ((meta (cdr entry)))
        (should (memq (plist-get meta :arity) '(window window-name)))
        (should (= (length meta) 2))))))

(ert-deftest dorg-test-ql-consumerless-capability-gate-removed ()
  "The all-backend timestamp gate and its derivation are absent before v0.1."
  (should-not (boundp 'dorg-ql--backend-timestamp-caps))
  (should-not (fboundp 'dorg-ql--timestamp-supported-p))
  (should-not (fboundp 'dorg-ql--timestamp-node-predicate))
  (should-not (fboundp 'dorg-ql--gate-timestamp-caps)))

(defun dorg-test-ql--ts-filter-node (query)
  "Normalize timestamp alias QUERY and return its single `:timestamp' node.
Migrates the former `dorg-ql--validate'-based window unit tests onto the
surviving `dorg-ql-normalize' path: a well-formed timestamp alias lowers to
`(:pipe (:source :all) (:filter (:timestamp SOURCE :overlaps SPAN)))', and
this returns that inner `(:timestamp ...)' node."
  (pcase-let ((`(:pipe (:source :all) (:filter ,node)) (dorg-ql-normalize query)))
    node))

(defun dorg-test-ql--alias-window (query)
  "Return the historical validated shape of timestamp alias QUERY.
Reconstructs `(HEAD FROM TO [NAME])' from the normalized `:timestamp' node
so the migrated window tests can compare spans without the retired
`dorg-ql--validate'.  A `(:property KEY|*)' source maps back to the
`property-ts' HEAD with a trailing NAME (nil for `*')."
  (pcase-let ((`(:timestamp ,source ,_rel (,from . ,to))
               (dorg-test-ql--ts-filter-node query)))
    (if (and (consp source) (eq (car source) :property))
        (let ((name (nth 1 source)))
          (list 'property-ts from to (unless (eq name '*) name)))
      (list source from to))))

(ert-deftest dorg-test-ql-metadata-validation-covers-every-predicate ()
  "AC-11: normalization accepts every metadata predicate at its declared arity.
Proves the metadata-driven normalizer (`dorg-ql-normalize', which replaced
the retired atomic validator) is driven by the metadata: a `window'
predicate lowers a two-arg form, a `window-name' predicate lowers the
optional-NAME form, and each yields the expected `:timestamp' node.  Adding
a metadata predicate the normalizer does not handle, or removing one, turns
this RED."
  (dolist (entry dorg-ql--timestamp-predicate-metadata)
    (let ((pred (car entry))
          (arity (plist-get (cdr entry) :arity)))
      (pcase arity
        ('window
         (should (equal (dorg-test-ql--ts-filter-node
                         (list pred "2026-01-01" "2026-12-31"))
                        (list :timestamp pred :overlaps
                              '((2026 1 1) . (2026 12 31))))))
        ('window-name
         ;; The sole window-name predicate lowers onto a (:property KEY|*)
         ;; source; NAME nil selects the wildcard, a string selects the key.
         (should (equal (dorg-test-ql--ts-filter-node
                         (list pred "2026-01-01" nil))
                        (list :timestamp '(:property *) :overlaps
                              '((2026 1 1) . nil))))
         (should (equal (dorg-test-ql--ts-filter-node
                         (list pred nil "2026-12-31" "DUE"))
                        (list :timestamp '(:property "DUE") :overlaps
                              '(nil . (2026 12 31))))))))))

;;; ─── Model Adapter ────────────────────────────────────────────────────

(defun dorg-test-ql--heading (id title &optional props tags)
  "Build a `dorg-heading' with ID, TITLE, PROPS alist, and TAGS list.
PROPS, when non-nil, becomes a PROPERTIES drawer item."
  (make-dorg-heading
   :id id
   :title title
   :level 1 :level-offset 1
   :tags tags
   :items (when props
            (list (dorg-drawer-item :name "PROPERTIES" :properties props)))))

(ert-deftest dorg-test-ql-model-by-id ()
  "Model adapter matches by id and returns the find result shape."
  (let ((headings (list (dorg-test-ql--heading 100 "Alpha")
                        (dorg-test-ql--heading 200 "Beta"))))
    (let* ((results (dorg-ql-select '(id 200) headings))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 200 (plist-get r :heading-id)))
      (should (equal "Beta" (plist-get r :title)))
      (should (null (plist-get r :marker)))
      ;; Result plist carries exactly the four keys.
      (should (equal '(:heading-id :marker :title :properties)
                     (cl-loop for (k _v) on r by #'cddr collect k))))))

(ert-deftest dorg-test-ql-model-by-property ()
  "Model adapter matches by exact PROPERTIES drawer key/value."
  (let ((headings (list (dorg-test-ql--heading 1 "Red" '(("CATEGORY" . "red")))
                        (dorg-test-ql--heading 2 "Blue" '(("CATEGORY" . "blue")))
                        (dorg-test-ql--heading 3 "Red2" '(("CATEGORY" . "red"))))))
    (let* ((results (dorg-ql-select '(property "CATEGORY" "red") headings))
           (titles (mapcar (lambda (r) (plist-get r :title)) results)))
      (should (= 2 (length results)))
      (should (member "Red" titles))
      (should (member "Red2" titles))
      (should-not (member "Blue" titles))
      ;; Properties alist is surfaced on the result.
      (should (equal '(("CATEGORY" . "red"))
                     (plist-get (car results) :properties))))))

(ert-deftest dorg-test-ql-model-by-property-miss ()
  "Model property lookup returns nil when no heading matches."
  (let ((headings (list (dorg-test-ql--heading 1 "H" '(("K" . "v"))))))
    (should (null (dorg-ql-select '(property "K" "other") headings)))))

(ert-deftest dorg-test-ql-model-by-title ()
  "Model adapter matches by exact title."
  (let ((headings (list (dorg-test-ql--heading 1 "Exact")
                        (dorg-test-ql--heading 2 "Exact other"))))
    (let ((results (dorg-ql-select '(title "Exact") headings)))
      (should (= 1 (length results)))
      (should (= 1 (plist-get (car results) :heading-id))))))

(ert-deftest dorg-test-ql-model-by-tag ()
  "Model adapter matches by local tag membership."
  (let ((headings (list (dorg-test-ql--heading 1 "Tagged" nil '("work" "urgent"))
                        (dorg-test-ql--heading 2 "Untagged" nil '("home")))))
    (let ((results (dorg-ql-select '(tag "urgent") headings)))
      (should (= 1 (length results)))
      (should (= 1 (plist-get (car results) :heading-id))))))

(ert-deftest dorg-test-ql-model-id-not-a-property ()
  "DORG_ID identity: the id predicate owns id, not the property predicate.
Model properties never include DORG_ID, so a property query for it misses."
  (let ((headings (list (dorg-test-ql--heading 42 "H" '(("OTHER" . "x"))))))
    (should (null (dorg-ql-select '(property "DORG_ID" "42") headings)))
    (should (= 1 (length (dorg-ql-select '(id 42) headings))))))

;;; ─── Plain-Org Adapter ────────────────────────────────────────────────

(defmacro dorg-test-ql--with-plain-org-file (contents bindings &rest body)
  "Write CONTENTS to a temp file, open it plain-org, then run BODY.
BINDINGS is a plist (:file FVAR :buffer BVAR); either key may be omitted."
  (declare (indent 2))
  (let ((file-var (or (plist-get bindings :file) (gensym "file")))
        (buf-var (or (plist-get bindings :buffer) (gensym "buf"))))
    `(let ((dorg--registry nil)
           (temp-base (make-temp-file "dorg-ql-test-" t)))
       (unwind-protect
           (let ((,file-var (expand-file-name "doc.org" temp-base)))
             (with-temp-file ,file-var (insert ,contents))
             (let ((,buf-var (find-file-noselect ,file-var)))
               (unwind-protect
                   (progn ,@body)
                 (when (buffer-live-p ,buf-var)
                   (with-current-buffer ,buf-var
                     (set-buffer-modified-p nil))
                   (kill-buffer ,buf-var)))))
         (delete-directory temp-base t)))))

(ert-deftest dorg-test-ql-plain-index-matches-parser-model-on-phantom-corpus ()
  "One DORG_ID corpus gives the parser/model oracle for the plain index.

It distinguishes each verbatim rule: one terminated sample is absent, its
real level-three child splices to Parent, two headings abandon a would-be
bridge, and an unterminated block leaves its following heading real."
  (let ((contents
         "* TODOX [#B] COMMENT Parent :local:\n:PROPERTIES:\n:DORG_ID: 100\n:FOO: parent\n:END:\n#+begin_example\n** Sample phantom\n#+end_example\n*** Spliced child\n:PROPERTIES:\n:DORG_ID: 101\n:BAR: child\n:END:\n#+begin_example\n** First after abandoned begin\n:PROPERTIES:\n:DORG_ID: 102\n:END:\n** Second after abandoned begin\n:PROPERTIES:\n:DORG_ID: 103\n:END:\n#+end_example\n* COMMENT\n:PROPERTIES:\n:DORG_ID: 106\n:END:\n* Before unterminated\n:PROPERTIES:\n:DORG_ID: 104\n:END:\n#+begin_quote\n** After unterminated begin\n:PROPERTIES:\n:DORG_ID: 105\n:END:\n"))
    (let ((org-todo-keywords '((sequence "TODOX" "DONE"))))
      (dorg-test-ql--with-plain-org-file contents (:file file :buffer buf)
        (let* ((model (dorg-test-ql--model-from-org contents))
               (query '(pipe (where (>= (field level) 1))))
               (ast (dorg-ql-normalize query))
               (model-rows (dorg-ql--model-eval ast model))
               (plain-rows (dorg-ql--plain-eval
                            ast (make-dorg-registration :file file) buf))
               (projection-ast
                (dorg-ql-normalize
                 '(pipe (where (>= (field level) 1))
                        (select (field id) (field parent-id) (field level)
                                (field title) (field todo) (field priority)
                                (field commented)))))
               (model-projection (dorg-ql--model-eval projection-ast model))
               (plain-projection
                (dorg-ql--plain-eval projection-ast
                                     (make-dorg-registration :file file) buf))
               (shape (lambda (row)
                        (list (plist-get row :heading-id)
                              (plist-get row :title)
                              (plist-get row :properties))))
               (index (with-current-buffer buf (dorg-ql--plain-build-index)))
               (real-id (plist-get index :real-id))
               (props (plist-get index :props))
               (by-real (make-hash-table :test 'eql))
               (model-by-real (make-hash-table :test 'eql)))
          ;; The identical normalized query runs through both public backend
          ;; evaluators; a marker is intentionally adapter-local.
          (should (equal (mapcar shape model-rows) (mapcar shape plain-rows)))
          ;; The same selected fields must also agree through both evaluators,
          ;; not only through the adapter's internal index representation.
          (should (equal (mapcar (lambda (row) (plist-get row :values))
                                 model-projection)
                         (mapcar (lambda (row) (plist-get row :values))
                                 plain-projection)))
          (dolist (heading (plist-get index :ordered))
            (puthash (gethash (dorg-heading-id heading) real-id) heading by-real))
          (dolist (heading model)
            (puthash (dorg-heading-id heading) heading model-by-real))
          ;; The index owns adapter ordinals, so compare its structural links
          ;; after translating them through the unchanged public ID reader.
          (maphash
           (lambda (id plain)
             (let* ((model-heading (gethash id model-by-real))
                    (parent-ord (dorg-heading-parent-id plain))
                    (plain-parent (and parent-ord (gethash parent-ord real-id)))
                    (plain-priority (dorg-heading-priority plain))
                    (model-priority (dorg-heading-priority model-heading)))
               (should model-heading)
               (should (equal (list plain-parent
                                    (dorg-heading-level plain)
                                    (dorg-heading-title plain)
                                    (dorg-heading-todo plain)
                                    plain-priority
                                    (dorg-heading-tags plain)
                                    (dorg-heading-commented plain)
                                    (gethash (dorg-heading-id plain) props))
                              (list (dorg-heading-parent-id model-heading)
                                    (dorg-heading-level model-heading)
                                    (dorg-heading-title model-heading)
                                    (dorg-heading-todo model-heading)
                                    (if (integerp model-priority)
                                        (char-to-string model-priority)
                                      model-priority)
                                    (dorg-heading-tags model-heading)
                                    (dorg-heading-commented model-heading)
                                    (dorg-ql--model-properties model-heading))))))
           by-real)
          (should-not (gethash nil by-real))
          (should-not (cl-find "Sample phantom" (plist-get index :ordered)
                               :key #'dorg-heading-title :test #'equal))
          (let ((parent (gethash 100 by-real))
                (spliced (gethash 101 by-real))
                (model-spliced (gethash 101 model-by-real))
                (bare-comment (gethash 106 by-real)))
            (should parent)
            (should spliced)
            (should model-spliced)
            (should (= (dorg-heading-id parent)
                       (dorg-heading-parent-id spliced)))
            (should (= (dorg-heading-level-offset model-spliced)
                       (dorg-heading-level-offset spliced)))
            (should (= 2 (dorg-heading-level-offset spliced)))
            (should (equal "" (dorg-heading-title bare-comment)))
            (should (dorg-heading-commented bare-comment)))
          (dolist (id '(102 103 105))
            (should (gethash id by-real))))))))

(defun dorg-test-ql--plain-index-inline-child (load-inline)
  "Run one isolated inline-task parity probe, loading it when LOAD-INLINE."
  (let* ((root (file-name-directory
                (or (locate-library "dorg-ql")
                    (error "dorg-ql is not on load-path"))))
         (emacs (expand-file-name invocation-name invocation-directory))
         (fixture "* Parent\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n*************** Task\n:PROPERTIES:\n:DORG_ID: 2\n:END:\n*************** END\n:PROPERTIES:\n:DORG_ID: 3\n:END:\n")
         (expr
          `(progn
             (require 'dorg-ql)
             (require 'dorg-parse)
             ,@(when load-inline '((require 'org-inlinetask)))
             (unless (eq (featurep 'org-inlinetask) ,load-inline)
               (error "inline-task feature state wrong: %S" (featurep 'org-inlinetask)))
             (with-temp-buffer
               (org-mode)
               (insert ,fixture)
               (let* ((ast (dorg-ql-normalize '(pipe (where (>= (field level) 1)))))
                      (model (cl-remove-if (lambda (h) (= 0 (dorg-heading-level h)))
                                           (dorg-parse-result-headings (dorg-parse-buffer))))
                      (model-ids (mapcar (lambda (r) (plist-get r :heading-id))
                                         (dorg-ql--model-eval ast model)))
                      (model-parents
                       (mapcar (lambda (h)
                                 (cons (dorg-heading-id h)
                                       (dorg-heading-parent-id h)))
                               model))
                      (calls 0)
                      (real-element (symbol-function 'org-element-at-point))
                      plain-ids)
                 (cl-letf (((symbol-function 'org-element-at-point)
                            (lambda (&rest args)
                              (setq calls (1+ calls))
                              (apply real-element args))))
                   (setq plain-ids
                         (mapcar (lambda (r) (plist-get r :heading-id))
                                 (dorg-ql--plain-eval
                                  ast (make-dorg-registration :file "/inline.org")
                                  (current-buffer)))))
                 (let ((expected (if ,load-inline '(1) '(1 2 3))))
                   (unless (and (equal expected model-ids)
                                (equal expected plain-ids)
                                (= calls 3))
                     (error "inline mismatch expected=%S model=%S plain=%S decodes=%S"
                            expected model-ids plain-ids calls))
                   (let* ((index (dorg-ql--plain-build-index))
                          (real-id (plist-get index :real-id))
                          (plain-parents
                           (mapcar
                            (lambda (h)
                              (cons (gethash (dorg-heading-id h) real-id)
                                    (let ((parent (dorg-heading-parent-id h)))
                                      (and parent (gethash parent real-id)))))
                            (plist-get index :ordered)))
                          (expected-parents
                           (if ,load-inline '((1 . nil))
                             '((1 . nil) (2 . 1) (3 . 1)))))
                     (unless (and (equal expected-parents model-parents)
                                  (equal expected-parents plain-parents))
                       (error "inline parent mismatch expected=%S model=%S plain=%S"
                              expected-parents model-parents plain-parents)))))
             (princ "INLINE-OK"))))
    (with-temp-buffer
      (let ((status (call-process emacs nil (current-buffer) nil
                                  "--batch" "-Q" "-L" root
                                  "--eval" (prin1-to-string expr))))
        (ert-info ((buffer-string))
          (should (= 0 status))
          (should (string-match-p "INLINE-OK" (buffer-string)))))))))

(ert-deftest dorg-test-ql-plain-index-inline-task-unloaded-fresh-child ()
  "Without org-inlinetask, its task and END lines stay ordinary headings."
  (dorg-test-ql--plain-index-inline-child nil))

(ert-deftest dorg-test-ql-plain-index-inline-task-loaded-fresh-child ()
  "With org-inlinetask, task and END are non-headlines and decode once each."
  (dorg-test-ql--plain-index-inline-child t))

(ert-deftest dorg-test-ql-plain-snapshot-refuses-a-source-changed-during-read ()
  "A plain content hash is never returned for rows read before a live edit."
  (dorg-test-ql--with-plain-org-file "* Alpha\n" (:buffer buffer)
    (let ((original (symbol-function 'dorg-ql--plain-eval)))
      (cl-letf (((symbol-function 'dorg-ql--plain-eval)
                 (lambda (&rest args)
                   (prog1 (apply original args)
                     (with-current-buffer buffer
                       (goto-char (point-max))
                       (insert "* Changed during evaluation\n"))))))
        (should-error (dorg-ql-select-snapshot '(title "Alpha") buffer)
                      :type 'dorg-ql-error)))))

(ert-deftest dorg-test-ql-plain-select-real-identity-without-body-read ()
  "Projection uses real IDs, preserves markers, and reads no body items."
  (dorg-test-ql--with-plain-org-file
      "* Parent\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n** Child\n:PROPERTIES:\n:DORG_ID: 200\n:FOO: bar\n:END:\nA body that must not be parsed.\n"
      (:buffer buffer)
    (cl-letf (((symbol-function 'dorg-parse-section-items-at-heading)
               (lambda (&rest _) (ert-fail "projection read the body"))))
      (let ((row (car (dorg-ql-select
                       '(pipe (where (id 200))
                              (select (field id) (field parent-id)
                                      (field title) (property "FOO")))
                       buffer))))
        (should (equal (plist-get row :values)
                       '(("id" . 200) ("parent-id" . 100)
                         ("title" . "Child") ("FOO" . "bar"))))
        (should (markerp (plist-get row :marker)))
        (should (= (plist-get row :heading-id) 200))))))

(ert-deftest dorg-test-ql-plain-by-id ()
  "Plain-org adapter matches by DORG_ID and returns a live marker."
  (dorg-test-ql--with-plain-org-file
      "* Alpha
:PROPERTIES:
:DORG_ID: 100
:END:
* Beta
:PROPERTIES:
:DORG_ID: 200
:END:
"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(id 200) buf))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 200 (plist-get r :heading-id)))
      (should (equal "Beta" (plist-get r :title)))
      (should (markerp (plist-get r :marker))))))

(ert-deftest dorg-test-ql-plain-by-id-miss ()
  "Plain-org id lookup returns nil when no heading matches."
  (dorg-test-ql--with-plain-org-file
      "* Only\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(id 9999) buf)))))

(ert-deftest dorg-test-ql-key-public-entry-loads-resolver-fresh-child ()
  "The public QL entry owns (key V) without loading the Dolt transport."
  (let* ((root (file-name-directory
                (or (locate-library "dorg-ql")
                    (error "dorg-ql is not on load-path"))))
         (emacs (expand-file-name invocation-name invocation-directory))
         (process-environment
          (cl-remove-if (lambda (entry)
                          (string-match-p "\\`\\(EMACSLOADPATH\\|EMACSLIB\\)=" entry))
                        process-environment))
         (expr
          '(let ((load-prefer-newer t)
                 (file (make-temp-file "dorg-ql-key-" nil ".org")))
             (unwind-protect
                 (progn
                   (require 'dorg-ql)
                   (unless (and (featurep 'dorg-mutate)
                                (fboundp 'dorg-mutate--source-identity)
                                (fboundp 'dorg-mutate--resolve-source)
                                (fboundp 'dorg-mutate--source-from-registration))
                     (error "public QL entry left key resolver unloaded"))
                   (when (or (featurep 'dorg-dolt)
                             (featurep 'dorg-ql-dolt)
                             (featurep 'mysql))
                     (error "public QL entry loaded Dolt transport"))
                   (with-temp-file file
                     (insert "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"))
                   (let ((buffer (find-file-noselect file)))
                     (unwind-protect
                         (let ((rows (dorg-ql-select '(key "100") buffer)))
                           (unless (and (= 1 (length rows))
                                        (equal "Alpha" (plist-get (car rows) :title))
                                        (= 100 (plist-get (car rows) :heading-id)))
                             (error "key query returned wrong rows: %S" rows))
                           (when (or (featurep 'dorg-dolt)
                                     (featurep 'dorg-ql-dolt)
                                     (featurep 'mysql))
                             (error "plain key query loaded Dolt transport")))
                       (kill-buffer buffer)))
                   (princ "PUBLIC-KEY-OK"))
               (delete-file file)))))
    (with-temp-buffer
      (let ((status (call-process emacs nil (current-buffer) nil
                                  "--batch" "-Q" "-L" root
                                  "--eval" (prin1-to-string expr))))
        (ert-info ((buffer-string))
          (should (= 0 status))
          (should (string-match-p "PUBLIC-KEY-OK" (buffer-string))))))))

(ert-deftest dorg-test-ql-plain-key-idbearing-lowers-to-id ()
  "AC-4 (tracked-case): on an id-bearing binding, (key V) resolves via the
DEFAULT identity-key DORG_ID — it matches the heading whose identity is V,
never a fallback property scan."
  (dorg-test-ql--with-plain-org-file
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Beta\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n"
      (:buffer buf)
    (let ((r (dorg-ql-select '(key "200") buf)))
      (should (= 1 (length r)))
      (should (= 200 (plist-get (car r) :heading-id))))
    ;; a value matching no identity misses (0 rows), never scans properties
    (should (null (dorg-ql-select '(key "999") buf)))))

(ert-deftest dorg-test-ql-key-model-list-unsupported ()
  "AC-4: (key V) on a model-list source has no binding identity-key -> a loud
dorg-ql-error (UNSUPPORTED), never a fallback scan or silent empty."
  (let* ((headings (list (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "A")))
         (err (should-error (dorg-ql-select '(key "1") headings)
                            :type 'dorg-ql-error)))
    (should (eq 'unsupported (dorg-error-reason err)))))

(ert-deftest dorg-test-ql-plain-one-cardinality ()
  "AC-2 (tracked-case): the (one QUERY) wrapper enforces exactly-one on the
plain-org backend identically to the model backend."
  (dorg-test-ql--with-plain-org-file
      "* Alpha
:PROPERTIES:
:DORG_ID: 100
:STATUS: OPEN
:END:
* Beta
:PROPERTIES:
:DORG_ID: 200
:STATUS: OPEN
:END:
"
      (:buffer buf)
    ;; exactly one by id -> the single heading
    (should (= 1 (length (dorg-ql-select '(one (id 100)) buf))))
    ;; non-vacuity: the bare inner query really returns two OPEN headings
    (should (= 2 (length (dorg-ql-select '(property "STATUS" "OPEN") buf))))
    ;; two matches -> cardinality error
    (should-error (dorg-ql-select '(one (property "STATUS" "OPEN")) buf)
                  :type 'dorg-ql-error)
    ;; zero matches -> cardinality error
    (should-error (dorg-ql-select '(one (id 9999)) buf) :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-plain-by-property ()
  "Plain-org adapter filters by exact property KEY = VALUE."
  (dorg-test-ql--with-plain-org-file
      "* Match
:PROPERTIES:
:CATEGORY: red
:END:
* Skip
:PROPERTIES:
:CATEGORY: blue
:END:
"
      (:buffer buf)
    (let ((titles (mapcar (lambda (r) (plist-get r :title))
                          (dorg-ql-select '(property "CATEGORY" "red") buf))))
      (should (member "Match" titles))
      (should-not (member "Skip" titles)))))

(ert-deftest dorg-test-ql-plain-by-property-not-inherited ()
  "Plain-org property match is local: an inherited parent property must
not match a child with no local drawer entry.  Mirrors model/Dolt, which
store only local custom properties."
  (dorg-test-ql--with-plain-org-file
      "* Parent
:PROPERTIES:
:CATEGORY: red
:END:
** Child
"
      (:buffer buf)
    (let ((titles (mapcar (lambda (r) (plist-get r :title))
                          (dorg-ql-select '(property "CATEGORY" "red") buf))))
      ;; The parent carries CATEGORY locally; the child only inherits it
      ;; and must not be returned by a local-property query.
      (should (equal '("Parent") titles)))))

(ert-deftest dorg-test-ql-plain-property-not-dorg-id ()
  "DORG_ID is identity-only in plain-org too: a (property \"DORG_ID\" ...)
query must miss, matching the model/Dolt adapters.  Identity is owned by
the (id ID) predicate."
  (dorg-test-ql--with-plain-org-file
      "* Only\n:PROPERTIES:\n:DORG_ID: 42\n:END:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(property "DORG_ID" "42") buf)))
    ;; The id predicate still owns identity lookup.
    (should (= 1 (length (dorg-ql-select '(id 42) buf))))))

(ert-deftest dorg-test-ql-plain-by-title ()
  "Plain-org adapter matches by exact heading title."
  (dorg-test-ql--with-plain-org-file
      "* First\n* Second\n"
      (:buffer buf)
    (let ((results (dorg-ql-select '(title "Second") buf)))
      (should (= 1 (length results)))
      (should (equal "Second" (plist-get (car results) :title)))
      (should (markerp (plist-get (car results) :marker))))))

(ert-deftest dorg-test-ql-plain-by-tag-local-not-inherited ()
  "Plain-org tag match is local: an inherited parent tag must not match."
  (dorg-test-ql--with-plain-org-file
      "* Parent :proj:
** Child
"
      (:buffer buf)
    (let ((results (dorg-ql-select '(tag "proj") buf)))
      ;; Only the parent carries the tag locally; the child inherits it
      ;; but must not be returned by a local-tag query.
      (should (= 1 (length results)))
      (should (equal "Parent" (plist-get (car results) :title))))))

(ert-deftest dorg-test-ql-plain-by-tag-miss ()
  "Plain-org tag lookup returns nil when no heading carries the tag."
  (dorg-test-ql--with-plain-org-file
      "* H :other:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(tag "missing") buf)))))

(ert-deftest dorg-test-ql-plain-result-properties-local-only ()
  "Plain-org result `:properties' carry only local PROPERTIES drawer pairs.
A child with local FOO=bar under a parent with local CATEGORY must surface
only its own FOO — never the inherited CATEGORY, never its own DORG_ID."
  (dorg-test-ql--with-plain-org-file
      "* Parent
:PROPERTIES:
:CATEGORY: red
:DORG_ID: 1
:END:
** Child
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(property "FOO" "bar") buf))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 2 (plist-get r :heading-id)))
      (should (equal '(("FOO" . "bar")) (plist-get r :properties))))))

(ert-deftest dorg-test-ql-plain-id-result-properties-exclude-dorg-id ()
  "An (id N) plain-org result excludes inherited CATEGORY and own DORG_ID."
  (dorg-test-ql--with-plain-org-file
      "* Parent
:PROPERTIES:
:CATEGORY: red
:DORG_ID: 1
:END:
** Child
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(id 2) buf))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 2 (plist-get r :heading-id)))
      (should (equal '(("FOO" . "bar")) (plist-get r :properties))))))

(ert-deftest dorg-test-ql-plain-result-properties-empty-when-only-dorg-id ()
  "A heading whose only drawer line is DORG_ID yields `:properties' nil."
  (dorg-test-ql--with-plain-org-file
      "* Only\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
      (:buffer buf)
    (let ((r (car (dorg-ql-select '(id 7) buf))))
      (should (= 7 (plist-get r :heading-id)))
      (should (null (plist-get r :properties))))))

(ert-deftest dorg-test-ql-properties-parity-plain-vs-model ()
  "Model and plain-org return `equal' :heading-id and :properties.
Same fixture text fed to both backends for (property \"FOO\" \"bar\"); the
plain-org result must converge with the model's local-drawer set."
  (let ((contents "* Parent
:PROPERTIES:
:CATEGORY: red
:DORG_ID: 1
:END:
** Child
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
"))
    (let* ((model (dorg-test-ql--model-from-org contents))
           (mr (car (dorg-ql-select '(property "FOO" "bar") model))))
      (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
        (let ((pr (car (dorg-ql-select '(property "FOO" "bar") pbuf))))
          (should (equal (plist-get mr :heading-id) (plist-get pr :heading-id)))
          (should (equal (plist-get mr :properties) (plist-get pr :properties))))))))

(ert-deftest dorg-test-ql-properties-parity-plain-vs-dolt ()
  "Dolt and plain-org return `equal' :heading-id and :properties.
Covers a child with inherited CATEGORY and a DORG_ID; skips without Dolt."
  (let ((contents "* Parent
:PROPERTIES:
:CATEGORY: red
:DORG_ID: 1
:END:
** Child
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
"))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let ((dr (car (dorg-ql-select '(property "FOO" "bar") dbuf))))
        (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
          (let ((pr (car (dorg-ql-select '(property "FOO" "bar") pbuf))))
            (should (equal (plist-get dr :heading-id) (plist-get pr :heading-id)))
            (should (equal (plist-get dr :properties) (plist-get pr :properties)))))))))

(ert-deftest dorg-test-ql-unregistered-errors ()
  "A query on a buffer with no backing file signals a plain error."
  (let ((dorg--registry nil))
    (with-temp-buffer
      (insert "* H\n")
      (should-error (dorg-ql-select '(id 1) (current-buffer))))))

;;; ─── Dolt Adapter ─────────────────────────────────────────────────────

(defun dorg-test-ql--assert-core-schema-only ()
  "Reject QL fixture callers that require extension schema.
`dorg-test--create-test-db' seeds the core schema only, so a registered
extension must not silently receive a store missing its schema."
  (should (null dorg--extensions)))

(defun dorg-test-ql--assert-shared-fixture-restored (inventory-before pid-before)
  "Assert normal shared QL teardown returned its store and kept its server.
The caller runs this after the fixture seam returns, so a primary body error
keeps its original signal while the seam still releases its store on unwind."
  (let ((inventory-after (dorg-dolt-fixture-inventory))
        (pid-after (dorg-test--shared-server-pid)))
    (should (equal pid-before pid-after))
    (should (= (plist-get inventory-before :cold-starts)
               (plist-get inventory-after :cold-starts)))
    (should (= (plist-get inventory-before :stores)
               (plist-get inventory-after :stores)))))

(defun dorg-test-ql--seed-document (dolt-dir)
  "Seed the current Org buffer into fresh core-schema DOLT-DIR.
Return its registration.  Query/mutation fixtures need committed data, not
administrative import, schema migration, or binding validation.  Those
contracts have their own lifecycle tests and a real-import parity witness.
The caller owns the database and dynamically isolates `dorg--registry'."
  (dorg-test-ql--assert-core-schema-only)
  (let* ((parsed (dorg-parse-buffer))
         (_ (dorg-parse-assign-ids parsed))
         (root (dorg-parse-result-root-id parsed))
         (file (file-truename (buffer-file-name)))
         statements)
    (dolist (heading (dorg-parse-result-headings parsed))
      (setq statements
            (nconc statements
                   (dorg-sql-gen-heading-full-sql heading root #'dorg-id-generate))))
    (dorg-sql-batch
     dolt-dir
     (nconc statements
            ;; Both paths belong to this fresh fixture and have known casing.
            ;; Production binding discovers casing by scanning every ancestor;
            ;; its import/bind contracts exercise that work separately.
            (list (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (%d, %s)"
                          root (dorg-sql-quote (file-relative-name file dolt-dir)))
                  (format "INSERT INTO dorg_meta (`key`, value) VALUES (%s, '1')"
                          (dorg-sql-quote (format "document-import:%d" root)))))
     "fixture: seed document")
    (let ((reg (make-dorg-registration
                :backend (dorg-backend-dolt dolt-dir root) :file file)))
      (push (cons file reg) dorg--registry)
      reg)))

(defmacro dorg-test-ql--with-dolt-synced (contents bindings &rest body)
  "Seed CONTENTS, register its Dolt-backed fixture, and run BODY.
BINDINGS plist: (:dolt-dir DDVAR :reg RVAR :buffer BVAR).
Skips if Dolt is unavailable."
  (declare (indent 2))
  (let ((dolt-dir (or (plist-get bindings :dolt-dir) (gensym "dd")))
        (reg (or (plist-get bindings :reg) (gensym "reg")))
        (buf (or (plist-get bindings :buffer) (gensym "buf"))))
    `(if (not dorg-test--dolt-available)
         (ert-skip "dolt not installed")
       (let ((inventory-before (and dorg-test--server-port
                                    (dorg-dolt-fixture-inventory)))
             (pid-before (dorg-test--shared-server-pid)))
         (dorg-test-ql--assert-core-schema-only)
         (dorg-test--with-temp-db-and-dir temp-base ,dolt-dir
           (let ((dorg--registry nil))
             (with-temp-buffer
               (insert ,contents)
               (org-mode)
               (set-visited-file-name (expand-file-name "doc.org" temp-base) t)
               (set-buffer-modified-p nil)
               (let ((,reg (dorg-test-ql--seed-document ,dolt-dir))
                     (,buf (current-buffer)))
                 (ignore ,reg)
                 ,@body))))
         (when inventory-before
           (dorg-test-ql--assert-shared-fixture-restored
            inventory-before pid-before))))))

(ert-deftest dorg-test-ql-shared-fixture-refuses-extension-schema-drift ()
  "A registered extension fails before a core-only shared store is used."
  (let ((dorg--extensions nil))
    (dorg-register-extension 'demo :schema "SELECT 1" :version 1)
    (should-error (dorg-test-ql--assert-core-schema-only)
                  :type 'ert-test-failed)))

(ert-deftest dorg-test-ql-shared-fixture-release-control-is-discriminating ()
  "The post-fixture assertion rejects an unreleased simulated store.
This unit control proves only the assertion's discrimination; the actual
fixture wiring is covered separately by its simulated lifecycle test."
  (cl-letf (((symbol-function 'dorg-dolt-fixture-inventory)
             (lambda () '(:servers 1 :stores 1 :cold-starts 1)))
            ((symbol-function 'dorg-test--shared-server-pid)
             (lambda () 12345)))
    (should-error
     (dorg-test-ql--assert-shared-fixture-restored
      '(:servers 1 :stores 0 :cold-starts 1) 12345)
     :type 'ert-test-failed)))

(ert-deftest dorg-test-ql-shared-fixture-restoration-accepts-clean-release ()
  "A normal shared-store release preserves the fixture inventory."
  (cl-letf (((symbol-function 'dorg-dolt-fixture-inventory)
             (lambda () '(:servers 1 :stores 0 :cold-starts 1)))
            ((symbol-function 'dorg-test--shared-server-pid)
             (lambda () 12345)))
    (should (dorg-test-ql--assert-shared-fixture-restored
             '(:servers 1 :stores 0 :cold-starts 1) 12345))))

(ert-deftest dorg-test-ql-shared-fixture-asserts-after-simulated-release ()
  "The actual outer macro checks restoration after its seam releases the store.
Moving the postcondition back inside `dorg-test--with-temp-db-and-dir' makes
this test RED: the simulated inventory is then one store above its baseline."
  (let ((stores 0)
        (body-ran nil)
        (dorg-test--dolt-available t)
        (dorg-test--server-port 12345)
        (dorg--extensions nil))
    (cl-letf (((symbol-function 'dorg-dolt-fixture-inventory)
               (lambda () (list :servers 1 :stores stores :cold-starts 1)))
              ((symbol-function 'dorg-test--shared-server-pid)
               (lambda () 12345))
              ((symbol-function 'dorg-test-ql--seed-document)
               (lambda (&rest _) 'simulated-registration)))
      (cl-macrolet
          ((dorg-test--with-temp-db-and-dir (temp-var dolt-var &rest body)
             `(let ((,temp-var "/tmp/dorg-ql-simulated")
                    (,dolt-var "/tmp/dorg-ql-simulated/dolt"))
                (cl-incf stores)
                (unwind-protect
                    (progn ,@body)
                  (cl-decf stores)))))
        (dorg-test-ql--with-dolt-synced "* H" (:buffer buffer)
          (setq body-ran (buffer-live-p buffer))))
      (should body-ran)
      (should (= 0 stores)))))

(ert-deftest dorg-test-ql-shared-fixture-simulated-body-error-keeps-signal ()
  "A primary BODY error survives while the simulated seam releases its store."
  (let ((stores 0)
        (dorg-test--dolt-available t)
        (dorg-test--server-port 12345)
        (dorg--extensions nil))
    (cl-letf (((symbol-function 'dorg-dolt-fixture-inventory)
               (lambda () (list :servers 1 :stores stores :cold-starts 1)))
              ((symbol-function 'dorg-test--shared-server-pid)
               (lambda () 12345))
              ((symbol-function 'dorg-test-ql--seed-document)
               (lambda (&rest _) 'simulated-registration)))
      (cl-macrolet
          ((dorg-test--with-temp-db-and-dir (temp-var dolt-var &rest body)
             `(let ((,temp-var "/tmp/dorg-ql-simulated")
                    (,dolt-var "/tmp/dorg-ql-simulated/dolt"))
                (cl-incf stores)
                (unwind-protect
                    (progn ,@body)
                  (cl-decf stores)))))
        (let ((err (should-error
                    (dorg-test-ql--with-dolt-synced "* H" (:buffer buffer)
                      (error "primary fixture body failure"))
                    :type 'error)))
          (should (string-match-p "primary fixture body failure"
                                  (error-message-string err)))))
      (should (= 0 stores)))))

(ert-deftest dorg-test-ql-fixture-seeds-without-administrative-bootstrap ()
  "Query setup writes committed data without invoking import or binding."
  (cl-letf (((symbol-function 'dorg-import-document)
             (lambda (&rest _) (ert-fail "fixture invoked administrative import")))
            ((symbol-function 'dorg-bind-dolt)
             (lambda (&rest _) (ert-fail "fixture invoked strict binding")))
            ((symbol-function 'dorg-repo-ensure-schema)
             (lambda (&rest _) (ert-fail "fixture revalidated its fresh schema")))
            ((symbol-function 'dorg-repo-file-row-statement)
             (lambda (&rest _) (ert-fail "fixture rediscovered its known file path"))))
    (dorg-test-ql--with-dolt-synced
        "* Seeded\n:PROPERTIES:\n:DORG_ID: 55\n:END:\n" (:reg reg :dolt-dir dd :buffer buf)
      (should (equal "Seeded" (plist-get (car (dorg-ql-select '(id 55) buf)) :title)))
      (let ((root (dorg-backend-root-id (dorg-registration-backend reg))))
        (should (= root (dorg-repo-root-id dd (buffer-file-name buf))))
        (let ((relative (alist-get 'relative_path
                                   (car (dorg-sql dd "SELECT relative_path FROM dorg_files")))))
          (should-not (file-name-absolute-p relative))
          (should (equal (file-truename (buffer-file-name buf))
                         (expand-file-name relative dd))))
        (should (gethash 55 (dorg-model-load-headings dd root nil (dorg-head-commit dd)))))
      (should (equal "* Seeded\n:PROPERTIES:\n:DORG_ID: 55\n:END:\n"
                     (buffer-string))))))

(ert-deftest dorg-test-ql-fixture-matches-real-import ()
  "The cheap builder and real bootstrap store the same committed model."
  (let ((contents "#+TITLE: Fixture\nPreamble\n* TODO Parent :tag:\nSCHEDULED: <2026-09-24 Thu>\n:PROPERTIES:\n:DORG_ID: 55\n:COLOR: blue\n:END:\nBody <2026-09-25 Fri>\n** Child\n:LOGBOOK:\n- State \"DONE\" from \"TODO\" [2026-09-24 Thu 12:00]\n:END:\n")
        (counter 10000)
        expected)
    (cl-letf (((symbol-function 'dorg-id-generate) (lambda () (cl-incf counter))))
      (dorg-test-ql--with-dolt-synced contents (:reg reg :dolt-dir dd)
        (let ((root (dorg-backend-root-id (dorg-registration-backend reg))))
          (setq expected
                (cl-sort (hash-table-values
                          (dorg-model-load-headings dd root nil (dorg-head-commit dd)))
                         #'< :key #'dorg-heading-id))
          (should (equal "1" (alist-get 'value
                                        (car (dorg-sql dd "SELECT value FROM dorg_meta WHERE `key` = 'document-import:10001'")))))))
      (setq counter 10000)
      (dorg-test--with-temp-db-and-dir base dd
        (let ((dorg--registry nil))
          (with-temp-buffer
            (insert contents)
            (org-mode)
            (set-visited-file-name (expand-file-name "doc.org" base) t)
            (set-buffer-modified-p nil)
            (dorg-import-document (current-buffer) :dolt-dir dd
                                  :target-file (buffer-file-name) :mode 'bootstrap)
            (let* ((reg (dorg-bind-dolt (buffer-file-name) dd))
                   (root (dorg-backend-root-id (dorg-registration-backend reg))))
              (should (equal expected
                             (cl-sort (hash-table-values
                                       (dorg-model-load-headings
                                        dd root nil (dorg-head-commit dd)))
                                      #'< :key #'dorg-heading-id))))))))))

(ert-deftest dorg-test-ql-fixture-isolates-writes-and-unwinds-errors ()
  "A failed nested case cannot contaminate its sibling or retain a store."
  (let ((before (dorg-dolt-fixture-inventory))
        (contents "* Original\n:PROPERTIES:\n:DORG_ID: 55\n:END:\n"))
    (dorg-test-ql--with-dolt-synced contents (:dolt-dir outer :reg outer-reg)
      (let ((registry dorg--registry))
        (should
         (equal '(error "fixture body failed")
                (should-error
                 (dorg-test-ql--with-dolt-synced contents (:dolt-dir inner)
                   (should-not (equal outer inner))
                   (dorg-sql inner "UPDATE headings SET title = 'Changed' WHERE id = 55")
                   (should (equal "Changed" (alist-get 'title (car (dorg-sql inner "SELECT title FROM headings WHERE id = 55")))))
                   (error "fixture body failed")))))
        (should (equal registry dorg--registry))
        (should (equal "Original" (plist-get (car (dorg-ql-select '(id 55) outer-reg)) :title)))))
    (should (equal before (dorg-dolt-fixture-inventory)))))

(ert-deftest dorg-test-ql-dolt-by-id ()
  "Dolt adapter matches by id and returns markerless results."
  (dorg-test-ql--with-dolt-synced
      "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n* Other\n"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(id 555) buf))
           (r (car results)))
      (should (= 1 (length results)))
      (should (= 555 (plist-get r :heading-id)))
      (should (equal "Find me" (plist-get r :title)))
      (should (null (plist-get r :marker))))))

(ert-deftest dorg-test-ql-dolt-by-id-miss ()
  "Dolt id lookup returns nil when no row matches."
  (dorg-test-ql--with-dolt-synced
      "* Just one\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(id 9999) buf)))))

(ert-deftest dorg-test-ql-dolt-by-property ()
  "Dolt adapter returns rows matching exact property KEY = VALUE."
  (dorg-test-ql--with-dolt-synced
      "* Red one
:PROPERTIES:
:CATEGORY: red
:END:
* Blue
:PROPERTIES:
:CATEGORY: blue
:END:
* Red two
:PROPERTIES:
:CATEGORY: red
:END:
"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(property "CATEGORY" "red") buf))
           (titles (mapcar (lambda (r) (plist-get r :title)) results)))
      (should (= 2 (length results)))
      (should (member "Red one" titles))
      (should (member "Red two" titles))
      (should-not (member "Blue" titles)))))

(ert-deftest dorg-test-ql-dolt-one-cardinality ()
  "AC-2 (tracked-case): the (one QUERY) wrapper enforces exactly-one on the
Dolt backend identically to the model/plain backends."
  (dorg-test-ql--with-dolt-synced
      "* Red one
:PROPERTIES:
:CATEGORY: red
:END:
* Blue
:PROPERTIES:
:CATEGORY: blue
:END:
* Red two
:PROPERTIES:
:CATEGORY: red
:END:
"
      (:buffer buf)
    ;; exactly one blue row -> the single heading
    (should (= 1 (length (dorg-ql-select '(one (property "CATEGORY" "blue")) buf))))
    ;; non-vacuity: two red rows really match the bare inner query
    (should (= 2 (length (dorg-ql-select '(property "CATEGORY" "red") buf))))
    ;; two matches -> cardinality error
    (should-error (dorg-ql-select '(one (property "CATEGORY" "red")) buf)
                  :type 'dorg-ql-error)
    ;; zero matches -> cardinality error
    (should-error (dorg-ql-select '(one (property "CATEGORY" "green")) buf)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-dolt-by-property-miss ()
  "Dolt property lookup returns nil when no row matches."
  (dorg-test-ql--with-dolt-synced
      "* H\n:PROPERTIES:\n:K: v\n:END:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(property "K" "other") buf)))))

(ert-deftest dorg-test-ql-dolt-by-title ()
  "Dolt adapter matches by exact title."
  (dorg-test-ql--with-dolt-synced
      "* First\n:PROPERTIES:\n:DORG_ID: 11\n:END:\n* Second\n:PROPERTIES:\n:DORG_ID: 22\n:END:\n"
      (:buffer buf)
    (let ((results (dorg-ql-select '(title "Second") buf)))
      (should (= 1 (length results)))
      (should (= 22 (plist-get (car results) :heading-id)))
      (should (null (plist-get (car results) :marker))))))

(ert-deftest dorg-test-ql-dolt-by-tag ()
  "Dolt adapter matches by local tag; an inherited parent tag must not match."
  (dorg-test-ql--with-dolt-synced
      "* Parent :proj:
:PROPERTIES:
:DORG_ID: 33
:END:
** Child
:PROPERTIES:
:DORG_ID: 44
:END:
"
      (:buffer buf)
    (let ((results (dorg-ql-select '(tag "proj") buf)))
      (should (= 1 (length results)))
      (should (= 33 (plist-get (car results) :heading-id))))))

(ert-deftest dorg-test-ql-dolt-by-tag-miss ()
  "Dolt tag lookup returns nil when no row carries the tag."
  (dorg-test-ql--with-dolt-synced
      "* H :other:\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (null (dorg-ql-select '(tag "missing") buf)))))

;;; ─── Dolt Batched Property Load ───────────────────────────────────────

(defmacro dorg-test-ql--counting-dolt-sql (hp-var in-var &rest body)
  "Run BODY counting revision-bound queries by SQL text.
HP-VAR is bound to the number of executed queries whose template
references `heading_properties'; IN-VAR to those matching the batched
`WHERE heading_id IN (' load.  Both names refer to mutable counters that
BODY may read after the wrapped calls run.  Wraps both SQL chokepoints so
the real implementation still executes and pinned reads remain observable."
  (declare (indent 2))
  `(let ((,hp-var 0)
         (,in-var 0)
         (dorg-test-ql--orig-live-sql (symbol-function 'dorg-sql))
         (dorg-test-ql--orig-pinned-sql
          (symbol-function 'dorg-sql--at-revision)))
     (cl-letf (((symbol-function 'dorg-sql)
                (lambda (dolt-dir query &rest args)
                  (let ((statement (if args (apply #'format query args) query)))
                    (when (string-match-p "heading_properties" statement)
                      (setq ,hp-var (1+ ,hp-var)))
                    (when (string-match-p "WHERE heading_id IN (" statement)
                      (setq ,in-var (1+ ,in-var))))
                  (apply dorg-test-ql--orig-live-sql dolt-dir query args)))
               ((symbol-function 'dorg-sql--at-revision)
                (lambda (dolt-dir revision query &rest args)
                  (let ((statement (if args (apply #'format query args) query)))
                    (when (string-match-p "heading_properties" statement)
                      (setq ,hp-var (1+ ,hp-var)))
                    (when (string-match-p "WHERE heading_id IN (" statement)
                      (setq ,in-var (1+ ,in-var))))
                  (apply dorg-test-ql--orig-pinned-sql
                         dolt-dir revision query args))))
       ,@body)))

(ert-deftest dorg-test-ql-dolt-batches-property-load ()
  "A multi-row tag query loads properties with one batched query, not N.
Uses a TAG selector so the rows query never touches
`heading_properties', making the property-load count unambiguous.  The
old per-row path issued one such query per result, so this fails without
the batch."
  (dorg-test-ql--with-dolt-synced
      "* Alpha :grp:
:PROPERTIES:
:KEY: a
:END:
* Beta :grp:
:PROPERTIES:
:KEY: b
:END:
* Gamma :grp:
:PROPERTIES:
:KEY: g
:END:
"
      (:buffer buf)
    (dorg-test-ql--counting-dolt-sql hp-count in-count
      (let* ((results (dorg-ql-select '(tag "grp") buf))
             (by-title (lambda (title)
                         (cl-find title results
                                  :key (lambda (r) (plist-get r :title))
                                  :test #'equal))))
        ;; Exactly one heading_properties query (the batch), not three.
        (should (= 1 hp-count))
        (should (= 1 in-count))
        (should (= 3 (length results)))
        (should (equal '(("KEY" . "a"))
                       (plist-get (funcall by-title "Alpha") :properties)))
        (should (equal '(("KEY" . "b"))
                       (plist-get (funcall by-title "Beta") :properties)))
        (should (equal '(("KEY" . "g"))
                       (plist-get (funcall by-title "Gamma") :properties)))))))

(ert-deftest dorg-test-ql-dolt-batch-preserves-ordinal-order ()
  "Batched property load preserves PROPERTIES drawer (ordinal) order.
Uses a NON-alphabetical source order (ZED, ALPHA, MID) so a naive sort of the
property keys would reorder it — the assertion is non-vacuous."
  (dorg-test-ql--with-dolt-synced
      "* One :grp:
:PROPERTIES:
:ZED: 1
:ALPHA: 2
:MID: 3
:END:
"
      (:buffer buf)
    (let* ((results (dorg-ql-select '(tag "grp") buf))
           (props (plist-get (car results) :properties)))
      (should (= 1 (length results)))
      (should (equal '(("ZED" . "1") ("ALPHA" . "2") ("MID" . "3")) props))
      ;; Non-vacuity guard: source order is NOT the alphabetical order, so a
      ;; reorder-by-sort regression would change this list.
      (should-not (equal (sort (copy-sequence props)
                               (lambda (a b) (string< (car a) (car b))))
                         props)))))

(ert-deftest dorg-test-ql-dolt-result-shape-and-order ()
  "Dolt results keep the exact plist shape and `dorg-ql--dolt-rows' order."
  (dorg-test-ql--with-dolt-synced
      "* Alpha :grp:
:PROPERTIES:
:KEY: a
:END:
* Beta :grp:
:PROPERTIES:
:KEY: b
:END:
* Gamma :grp:
:PROPERTIES:
:KEY: g
:END:
"
      (:buffer buf :reg rvar)
    (let* ((query '(tag "grp"))
           (results (dorg-ql-select query buf))
           ;; Order/ids straight from the rows query the select maps over.
           (rows (dorg-ql--dolt-rows query rvar))
           (row-ids (mapcar (lambda (r) (alist-get 'id r)) rows)))
      (dolist (r results)
        ;; Exact key set, in order, with a nil marker.
        (should (equal '(:heading-id :marker :title :properties)
                       (cl-loop for (k _v) on r by #'cddr collect k)))
        (should (null (plist-get r :marker))))
      (should (equal row-ids
                     (mapcar (lambda (r) (plist-get r :heading-id)) results))))))

(ert-deftest dorg-test-ql-dolt-empty-result-no-property-query ()
  "A no-match selector returns nil and issues zero property queries."
  (dorg-test-ql--with-dolt-synced
      "* H :other:\n:PROPERTIES:\n:K: v\n:END:\n"
      (:buffer buf)
    (dorg-test-ql--counting-dolt-sql hp-count in-count
      (should (null (dorg-ql-select '(tag "missing") buf)))
      ;; No batch query: avoids invalid `IN ()' SQL.
      (should (= 0 hp-count))
      (should (= 0 in-count)))))

(ert-deftest dorg-test-ql-dolt-batch-root-scoped ()
  "Batched load keys only on root-scoped ids; sibling roots do not leak.
Two roots share one dolt-dir and the queried tag; root A's results must
exclude root B's heading and must not carry root B's properties."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (dorg-test--make-owned-temp-dir
                       "dorg-ql-dolt-roots-" "ql-dolt-roots"))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (file-a (expand-file-name "a.org" temp-base))
           (file-b (expand-file-name "b.org" temp-base)))
      (unwind-protect
          (let ((dorg--registry nil))
            (dorg-init dolt-dir)
            (with-temp-buffer
              (insert "* RootB hit :shared:\n:PROPERTIES:\n:OWNER: b\n:END:\n")
              (org-mode)
              (set-visited-file-name file-b t)
              (set-buffer-modified-p nil)
              (dorg-import-document
               (current-buffer) :dolt-dir dolt-dir
               :target-file file-b :mode 'bootstrap))
            (with-temp-buffer
              (insert "* RootA hit :shared:\n:PROPERTIES:\n:OWNER: a\n:END:\n")
              (org-mode)
              (set-visited-file-name file-a t)
              (set-buffer-modified-p nil)
              (dorg-import-document
               (current-buffer) :dolt-dir dolt-dir
               :target-file file-a :mode 'bootstrap)
              (let ((reg-a (dorg-bind-dolt file-a dolt-dir))
                    (buf-a (current-buffer)))
                (ignore reg-a)
                (let* ((results (dorg-ql-select '(tag "shared") buf-a))
                       (titles (mapcar (lambda (r) (plist-get r :title))
                                       results)))
                  (should (equal '("RootA hit") titles))
                  (should (equal '(("OWNER" . "a"))
                                 (plist-get (car results) :properties)))))))
        (dorg-test--delete-owned-root temp-base)))))

;;; ─── Timestamp / LOGBOOK / Property-timestamp Predicates ──────────────

(defun dorg-test-ql--ts-heading (id title items)
  "Build a level-1 `dorg-heading' with ID, TITLE, and ITEMS list."
  (make-dorg-heading :id id :title title :level 1 :level-offset 1 :items items))

(defun dorg-test-ql--planning (&rest plist)
  "Build a `dorg-planning-item' from PLIST (:scheduled/:deadline/:closed)."
  (apply #'dorg-planning-item plist))

(ert-deftest dorg-test-ql-scheduled-in-window ()
  "(scheduled FROM TO) matches a SCHEDULED timestamp inside the window."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Soon"
                (list (dorg-test-ql--planning :scheduled "<2026-05-15 Fri>")))
               (dorg-test-ql--ts-heading
                2 "Later"
                (list (dorg-test-ql--planning :scheduled "<2026-09-01 Tue>"))))))
    (let ((results (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") headings)))
      (should (= 1 (length results)))
      (should (= 1 (plist-get (car results) :heading-id))))
    ;; Out-of-window: no match.
    (should (null (dorg-ql-select '(scheduled "2026-06-01" "2026-06-30") headings)))))

(ert-deftest dorg-test-ql-scheduled-open-bounds ()
  "Open (nil) window bounds are unbounded on that side."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Mid"
                (list (dorg-test-ql--planning :scheduled "<2026-05-15 Fri>"))))))
    ;; FROM open: anything on or before TO matches.
    (should (= 1 (length (dorg-ql-select '(scheduled nil "2026-05-31") headings))))
    ;; TO open: anything on or after FROM matches.
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-05-01" nil) headings))))
    ;; Both open: matches any heading that has a SCHEDULED timestamp.
    (should (= 1 (length (dorg-ql-select '(scheduled nil nil) headings))))
    ;; TO open but FROM after the date: no match.
    (should (null (dorg-ql-select '(scheduled "2026-06-01" nil) headings)))))

(ert-deftest dorg-test-ql-deadline-and-closed ()
  "(deadline ...) and (closed ...) project the matching planning field."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "H"
                (list (dorg-test-ql--planning
                       :deadline "<2026-05-20 Wed>"
                       :closed "[2026-05-25 Mon]"))))))
    (should (= 1 (length (dorg-ql-select '(deadline "2026-05-01" "2026-05-31") headings))))
    (should (null (dorg-ql-select '(deadline "2026-06-01" "2026-06-30") headings)))
    ;; CLOSED is an inactive timestamp; still windowed by its date.
    (should (= 1 (length (dorg-ql-select '(closed "2026-05-24" "2026-05-26") headings))))
    (should (null (dorg-ql-select '(closed "2026-05-01" "2026-05-10") headings)))
    ;; A heading with no planning field of that kind never matches.
    (should (null (dorg-ql-select '(scheduled nil nil) headings)))))

(ert-deftest dorg-test-ql-active-body-timestamps ()
  "(active-ts ...) matches active body timestamps but ignores inactive ones."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Body"
                (list (dorg-text-item
                       :text "Meeting <2026-06-10 Wed> and note [2026-01-01 Thu]."))))))
    (should (= 1 (length (dorg-ql-select '(active-ts "2026-06-01" "2026-06-30") headings))))
    ;; The inactive [2026-01-01] is not an active body timestamp.
    (should (null (dorg-ql-select '(active-ts "2025-12-15" "2026-01-15") headings)))
    (should (null (dorg-ql-select '(active-ts "2026-07-01" "2026-07-31") headings)))))

(ert-deftest dorg-test-ql-active-range-overlap ()
  "A date-range active timestamp matches a window strictly inside the range."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Span"
                (list (dorg-text-item
                       :text "Trip <2026-06-01 Mon>--<2026-06-30 Tue>."))))))
    ;; Window fully inside the range overlaps via end-date, not just start.
    (should (= 1 (length (dorg-ql-select '(active-ts "2026-06-15" "2026-06-20") headings))))
    ;; A window entirely before the range start does not overlap.
    (should (null (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") headings)))))

(ert-deftest dorg-test-ql-logbook-window ()
  "(logbook ...) matches LOGBOOK entry timestamps by window."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Worked"
                (list (dorg-drawer-item
                       :name "LOGBOOK"
                       :lines '("- State \"DONE\" from \"TODO\" [2026-05-20 Wed 10:00]"
                                "- Note taken on [2026-05-21 Thu 09:00] \\\\")))))))
    (should (= 1 (length (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") headings))))
    (should (null (dorg-ql-select '(logbook "2026-06-01" "2026-06-30") headings)))))

(ert-deftest dorg-test-ql-property-timestamps ()
  "(property-ts ...) matches any property timestamp; NAME restricts the key."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Props"
                (list (dorg-drawer-item
                       :name "PROPERTIES"
                       :properties '(("DUE" . "<2026-07-01 Wed>")
                                     ("REVIEW" . "<2026-08-01 Sat>")
                                     ("CATEGORY" . "work"))))))))
    ;; Unrestricted: DUE falls in the window.
    (should (= 1 (length (dorg-ql-select '(property-ts "2026-06-15" "2026-07-15") headings))))
    ;; NAME restriction: only DUE is consulted.
    (should (= 1 (length (dorg-ql-select '(property-ts nil nil "DUE") headings))))
    (should (null (dorg-ql-select '(property-ts nil nil "MISSING") headings)))
    ;; REVIEW (08-01) is in window, but restricting to DUE (07-01) misses it.
    (should (null (dorg-ql-select '(property-ts "2026-07-20" "2026-08-15" "DUE") headings)))
    (should (= 1 (length (dorg-ql-select '(property-ts "2026-07-20" "2026-08-15" "REVIEW") headings))))
    ;; A non-timestamp property value (CATEGORY) never matches.
    (should (null (dorg-ql-select '(property-ts nil nil "CATEGORY") headings)))))

(ert-deftest dorg-test-ql-repeater-base-occurrence-only ()
  "Repeaters match their base occurrence only; no calendar expansion."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Weekly"
                (list (dorg-test-ql--planning :scheduled "<2026-05-15 Fri +1w>"))))))
    ;; Base occurrence matches.
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-05-14" "2026-05-16") headings))))
    ;; A future repeat week is NOT projected, so it does not match.
    (should (null (dorg-ql-select '(scheduled "2026-05-22" "2026-05-23") headings)))))

(ert-deftest dorg-test-ql-timestamp-validation ()
  "Timestamp predicates reject bad dates, arity, and NAME types."
  ;; Malformed date strings (must be zero-padded YYYY-MM-DD).
  (should-error (dorg-ql-select '(scheduled "2026-5-1" nil) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(deadline "not-a-date" nil) nil) :type 'dorg-ql-error)
  ;; Wrong arity for fixed two-arg windows.
  (should-error (dorg-ql-select '(scheduled "2026-01-01") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(closed "a" "b" "c") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(active-ts) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(logbook "2026-01-01") nil) :type 'dorg-ql-error)
  ;; property-ts: 2 or 3 args only; NAME must be a string.
  (should-error (dorg-ql-select '(property-ts "2026-01-01") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property-ts "a" "b" "c" "d") nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(property-ts nil nil 5) nil) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-timestamp-all-lowered-on-plain-org ()
  "Every timestamp predicate answers on the plain-org backend.
After Slice 4 the last predicate (`active-ts') is lowered too, so
`scheduled'/`deadline'/`closed'/`property-ts'/`logbook'/`active-ts' all
answer (returning nil rather than signalling when nothing matches)."
  (dorg-test-ql--with-plain-org-file
      "* H
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 1
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-05-16 Sat]
:END:
Body meeting <2026-05-17 Sun> here.
"
      (:buffer buf)
    ;; Lowered: planning predicates no longer reject (no DEADLINE/CLOSED
    ;; here, so those answer nil rather than signalling).
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(deadline nil nil) buf)))
    (should (null (dorg-ql-select '(closed nil nil) buf)))
    ;; Lowered: property-ts no longer rejects.  The only drawer entry is
    ;; DORG_ID (excluded, and not a timestamp), so it answers nil.
    (should (null (dorg-ql-select '(property-ts nil nil) buf)))
    ;; Lowered: logbook no longer rejects; the LOGBOOK state-change
    ;; timestamp is windowed.
    (should (= 1 (length (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(logbook "2026-06-01" "2026-06-30") buf)))
    ;; Lowered: active-ts no longer rejects; the active body timestamp is
    ;; windowed, while the SCHEDULED active planning timestamp (not body)
    ;; is never counted.
    (should (= 1 (length (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(active-ts "2026-06-01" "2026-06-30") buf)))))

(ert-deftest dorg-test-ql-timestamp-all-lowered-on-dolt ()
  "Every timestamp predicate answers on the Dolt backend.
After Slice 4 the last predicate (`active-ts') is lowered too, so
`scheduled'/`deadline'/`closed'/`property-ts'/`logbook'/`active-ts' all
answer (returning nil rather than signalling when nothing matches)."
  (dorg-test-ql--with-dolt-synced
      "* H
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 1
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-05-16 Sat]
:END:
Body meeting <2026-05-17 Sun> here.
"
      (:buffer buf)
    ;; Lowered: planning predicates no longer reject.
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(deadline nil nil) buf)))
    ;; Lowered: property-ts no longer rejects (DORG_ID is not a property
    ;; timestamp candidate, so it answers nil).
    (should (null (dorg-ql-select '(property-ts nil nil) buf)))
    ;; Lowered: logbook no longer rejects; the LOGBOOK timestamp windows.
    (should (= 1 (length (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(logbook "2026-06-01" "2026-06-30") buf)))
    ;; Lowered: active-ts no longer rejects; the active body timestamp is
    ;; windowed, while the SCHEDULED active planning timestamp (not body)
    ;; is never counted.
    (should (= 1 (length (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(active-ts "2026-06-01" "2026-06-30") buf)))))

;;; ─── Planning Predicate Lowering (Slice 1: scheduled/deadline/closed) ──

(ert-deftest dorg-test-ql-plain-org-scheduled-window ()
  "(scheduled ...) on plain-org windows the entry's SCHEDULED planning line."
  (dorg-test-ql--with-plain-org-file
      "* Soon
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 1
:END:
* Later
SCHEDULED: <2026-09-01 Tue>
:PROPERTIES:
:DORG_ID: 2
:END:
"
      (:buffer buf)
    (let ((results (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf)))
      (should (= 1 (length results)))
      (should (= 1 (plist-get (car results) :heading-id)))
      ;; Plain-org results carry a live marker.
      (should (markerp (plist-get (car results) :marker))))
    ;; Out-of-window: no match.
    (should (null (dorg-ql-select '(scheduled "2026-06-01" "2026-06-30") buf)))
    ;; Open bounds select any heading with a SCHEDULED timestamp.
    (should (= 2 (length (dorg-ql-select '(scheduled nil nil) buf))))
    (should (= 1 (length (dorg-ql-select '(scheduled nil "2026-05-31") buf))))
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-06-01" nil) buf))))))

(ert-deftest dorg-test-ql-plain-org-deadline-and-closed ()
  "(deadline ...)/(closed ...) project the matching plain-org planning field."
  (dorg-test-ql--with-plain-org-file
      "* H
DEADLINE: <2026-05-20 Wed> CLOSED: [2026-05-25 Mon]
:PROPERTIES:
:DORG_ID: 7
:END:
"
      (:buffer buf)
    (should (= 1 (length (dorg-ql-select '(deadline "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(deadline "2026-06-01" "2026-06-30") buf)))
    ;; CLOSED is an inactive timestamp; still windowed by its date.
    (should (= 1 (length (dorg-ql-select '(closed "2026-05-24" "2026-05-26") buf))))
    (should (null (dorg-ql-select '(closed "2026-05-01" "2026-05-10") buf)))
    ;; This heading has no SCHEDULED planning field.
    (should (null (dorg-ql-select '(scheduled nil nil) buf)))))

(ert-deftest dorg-test-ql-plain-org-scheduled-repeater-base-only ()
  "Plain-org repeaters match their base occurrence only; no projection."
  (dorg-test-ql--with-plain-org-file
      "* Weekly
SCHEDULED: <2026-05-15 Fri +1w>
:PROPERTIES:
:DORG_ID: 1
:END:
"
      (:buffer buf)
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-05-14" "2026-05-16") buf))))
    (should (null (dorg-ql-select '(scheduled "2026-05-22" "2026-05-23") buf)))))

(ert-deftest dorg-test-ql-dolt-scheduled-window ()
  "(scheduled ...) on Dolt windows the planning_items SCHEDULED candidate."
  (dorg-test-ql--with-dolt-synced
      "* Soon
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 1
:END:
* Later
SCHEDULED: <2026-09-01 Tue>
:PROPERTIES:
:DORG_ID: 2
:END:
"
      (:buffer buf)
    (let ((results (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf)))
      (should (= 1 (length results)))
      (should (= 1 (plist-get (car results) :heading-id)))
      ;; Dolt results carry a nil marker.
      (should (null (plist-get (car results) :marker))))
    (should (null (dorg-ql-select '(scheduled "2026-06-01" "2026-06-30") buf)))
    ;; Open bounds select any heading with a SCHEDULED timestamp.
    (should (= 2 (length (dorg-ql-select '(scheduled nil nil) buf))))
    (should (= 1 (length (dorg-ql-select '(scheduled nil "2026-05-31") buf))))
    (should (= 1 (length (dorg-ql-select '(scheduled "2026-06-01" nil) buf))))))

(defun dorg-test-ql--assert-timestamp-fold-policy
    (scanner scanner-args rows expected wrong-decide wrong-expected)
  "Assert SCANNER delegates its ordered ROWS through the timestamp fold.
SCANNER-ARGS are applied to SCANNER.  EXPECTED is the real policy result;
WRONG-DECIDE must instead produce WRONG-EXPECTED, proving the fixture
distinguishes the policies."
  (should (fboundp 'dorg-ql--dolt-fold-timestamp-rows))
  (let* ((original-fold
          (symbol-function 'dorg-ql--dolt-fold-timestamp-rows))
         (sql-count 0)
         (fold-count 0)
         actual)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (&rest _args)
                 (setq sql-count (1+ sql-count))
                 rows))
              ((symbol-function 'dorg-ql--dolt-fold-timestamp-rows)
               (lambda (got-rows decide)
                 (setq fold-count (1+ fold-count))
                 (funcall original-fold got-rows decide))))
      (setq actual (apply scanner scanner-args)))
    (should (= 1 sql-count))
    (should (= 1 fold-count))
    (should (equal expected actual))
    (let ((wrong (funcall original-fold rows wrong-decide)))
      (should (equal wrong-expected wrong))
      (should-not (equal wrong actual)))))

(ert-deftest dorg-test-ql-dolt-timestamp-fold-property-scans-until-match ()
  "Property timestamp rows keep scanning past an out-of-window candidate."
  (let ((from '(2026 7 1))
        (to '(2026 7 31)))
    (dorg-test-ql--assert-timestamp-fold-policy
     #'dorg-ql--dolt-property-ts-rows
     (list "/unused" 1 from to nil)
     '(((id . 7) (title . "Policy") (raw . "<2026-01-01 Thu>"))
       ((id . 7) (title . "Policy") (raw . "<2026-07-15 Wed>")))
     '(((id . 7) (title . "Policy")))
     (lambda (row)
       (let ((ts (dorg-timestamp-parse (alist-get 'raw row))))
         (cond
          ((null ts) 'no-decision)
          ((dorg-ql--timestamp-in-window-p ts from to) 'decided/match)
          (t 'decided/no-match))))
     nil)))

(ert-deftest dorg-test-ql-dolt-timestamp-fold-planning-first-parseable-decides ()
  "Planning stops after its first parseable out-of-window candidate."
  (let ((from '(2026 7 1))
        (to '(2026 7 31)))
    (dorg-test-ql--assert-timestamp-fold-policy
     #'dorg-ql--dolt-planning-rows
     (list "/unused" 1 'scheduled from to)
     '(((id . 8) (title . "Planning") (raw . "<2026-01-01 Thu>"))
       ((id . 8) (title . "Planning") (raw . "<2026-07-15 Wed>")))
     nil
     (lambda (row)
       (let ((ts (dorg-timestamp-parse (alist-get 'raw row))))
         (if (and ts (dorg-ql--timestamp-in-window-p ts from to))
             'decided/match
           'no-decision)))
     '(((id . 8) (title . "Planning"))))))

(ert-deftest dorg-test-ql-dolt-timestamp-fold-planning-unparseable-falls-through ()
  "Planning skips an unparseable row before its first parseable candidate."
  (let ((from '(2026 7 1))
        (to '(2026 7 31)))
    (dorg-test-ql--assert-timestamp-fold-policy
     #'dorg-ql--dolt-planning-rows
     (list "/unused" 1 'scheduled from to)
     '(((id . 9) (title . "Fallthrough") (raw . "not a timestamp"))
       ((id . 9) (title . "Fallthrough") (raw . "<2026-07-15 Wed>")))
     '(((id . 9) (title . "Fallthrough")))
     (lambda (row)
       (let ((ts (dorg-timestamp-parse (alist-get 'raw row))))
         (cond
          ((null ts) 'decided/no-match)
          ((dorg-ql--timestamp-in-window-p ts from to) 'decided/match)
          (t 'decided/no-match))))
     nil)))

(ert-deftest dorg-test-ql-dolt-timestamp-fold-rejects-unknown-decision ()
  "The private fold fails closed when a scanner returns an unknown token."
  (should-error
   (dorg-ql--dolt-fold-timestamp-rows
    '(((id . 10) (title . "Unknown")))
    (lambda (_row) 'unknown-decision))
   :type 'error))

(ert-deftest dorg-test-ql-dolt-deadline-and-closed ()
  "(deadline ...)/(closed ...) project the matching Dolt planning column."
  (dorg-test-ql--with-dolt-synced
      "* H
DEADLINE: <2026-05-20 Wed> CLOSED: [2026-05-25 Mon]
:PROPERTIES:
:DORG_ID: 9
:END:
"
      (:buffer buf)
    (should (= 1 (length (dorg-ql-select '(deadline "2026-05-01" "2026-05-31") buf))))
    (should (null (dorg-ql-select '(deadline "2026-06-01" "2026-06-30") buf)))
    (should (= 1 (length (dorg-ql-select '(closed "2026-05-24" "2026-05-26") buf))))
    (should (null (dorg-ql-select '(closed "2026-05-01" "2026-05-10") buf)))
    (should (null (dorg-ql-select '(scheduled nil nil) buf)))))

(ert-deftest dorg-test-ql-dolt-planning-no-whole-tree-load ()
  "Dolt planning lowering never materializes the whole tree.
Post-y9nvv, an atomic timestamp alias routes onto the unified v2 planner,
which may load a HEADLINE-ONLY adjacency map (headings + tags) — but never a
full body/item load.  Assert no non-headline-only `dorg-model-load-headings'
call occurs; a regression to a whole-tree hydration turns it RED."
  (dorg-test-ql--with-dolt-synced
      "* H\nSCHEDULED: <2026-05-15 Fri>\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (let ((full-load 0)
          (orig (symbol-function 'dorg-model-load-headings)))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (dolt-dir root-id ids
                          &optional as-of headline-only query-fn levels)
                   (should (functionp query-fn))
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only query-fn levels))))
        (let ((results (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf)))
          (should (= 1 (length results)))
          (should (= 0 full-load)))))))

(ert-deftest dorg-test-ql-dolt-planning-constant-query-count ()
  "Dolt planning runs a constant query count (selector + one property batch).
The selector touches `planning_items', so the only `heading_properties'
query is the single batched property load — independent of result count."
  (dorg-test-ql--with-dolt-synced
      "* Alpha
SCHEDULED: <2026-05-10 Sun>
:PROPERTIES:
:KEY: a
:END:
* Beta
SCHEDULED: <2026-05-20 Wed>
:PROPERTIES:
:KEY: b
:END:
"
      (:buffer buf)
    (dorg-test-ql--counting-dolt-sql hp-count in-count
      (let ((results (dorg-ql-select '(scheduled "2026-05-01" "2026-05-31") buf)))
        (should (= 2 (length results)))
        ;; Exactly one heading_properties query (the batch), not one per row.
        (should (= 1 hp-count))
        (should (= 1 in-count))))))

(ert-deftest dorg-test-ql-planning-cross-backend-equality ()
  "Model, plain-org, and Dolt return the SAME heading-ids for planning queries.
Set equality over identical content; the model adapter is the baseline."
  (let ((contents "* Alpha
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 1
:END:
* Beta
DEADLINE: <2026-05-20 Wed>
:PROPERTIES:
:DORG_ID: 2
:END:
* Gamma
CLOSED: [2026-05-25 Mon]
:PROPERTIES:
:DORG_ID: 3
:END:
* Delta
SCHEDULED: <2026-09-01 Tue>
:PROPERTIES:
:DORG_ID: 4
:END:
")
        (queries '((scheduled "2026-05-01" "2026-05-31")
                   (scheduled nil nil)
                   (deadline "2026-05-01" "2026-05-31")
                   (deadline nil nil)
                   (closed nil nil)
                   (closed "2026-05-24" "2026-05-26")
                   (scheduled "2026-06-01" "2026-06-30"))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let* ((model (dorg-test-ql--model-from-org contents))
             ;; Capture plain-org ids inside their own registry scope.
             (pids-list
              (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
                (mapcar (lambda (q) (dorg-test-ql--ids (dorg-ql-select q pbuf)))
                        queries))))
        (cl-loop for q in queries for pids in pids-list do
                 (let ((mids (dorg-test-ql--ids (dorg-ql-select q model)))
                       (dids (dorg-test-ql--ids (dorg-ql-select q dbuf))))
                   (should (equal mids dids))
                   (should (equal mids pids))))))))

;;; ─── property-ts Lowering (Slice 2) ───────────────────────────────────

(defconst dorg-test-ql--property-ts-content
  "* Alpha
:PROPERTIES:
:DORG_ID: 1
:DUE: <2026-07-01 Wed>
:END:
* Beta
:PROPERTIES:
:DORG_ID: 2
:REVIEW: [2026-08-01 Sat]
:END:
* Gamma
:PROPERTIES:
:DORG_ID: 3
:NOTE: not a timestamp
:END:
* Delta
:PROPERTIES:
:DORG_ID: 4
:DUE: <2026-07-05 Sun>
:REVIEW: [2026-07-06 Mon]
:END:
"
  "Shared fixture for property-ts lowering tests.
Alpha/Delta carry a DUE timestamp, Beta/Delta a REVIEW timestamp, Gamma
only a non-timestamp NOTE.  DORG_ID is present everywhere but must never
match (identity-only, and numeric values do not parse as timestamps).")

(ert-deftest dorg-test-ql-plain-org-property-ts-window ()
  "(property-ts ...) on plain-org windows PROPERTIES-drawer timestamps.
Covers positive, negative, and open-bound cases, a live marker, and
dedup of a heading carrying two matching property timestamps."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--property-ts-content (:buffer buf)
    ;; Positive: Alpha's DUE 2026-07-01 falls in window; Delta's
    ;; 07-05/07-06 also fall in.  Two headings, Delta counted once.
    (let ((results (dorg-ql-select '(property-ts "2026-07-01" "2026-07-31") buf)))
      (should (equal '(1 4) (dorg-test-ql--ids results)))
      ;; Plain-org results carry a live marker.
      (should (markerp (plist-get (car results) :marker))))
    ;; Negative: window before any property timestamp.
    (should (null (dorg-ql-select '(property-ts "2026-01-01" "2026-06-30") buf)))
    ;; Open bounds: every heading with a parseable property timestamp.
    ;; Gamma (NOTE) does not parse, so it is excluded.
    (should (equal '(1 2 4) (dorg-test-ql--ids
                             (dorg-ql-select '(property-ts nil nil) buf))))
    (should (equal '(1 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil "2026-07-31") buf))))
    (should (equal '(2) (dorg-test-ql--ids
                         (dorg-ql-select '(property-ts "2026-08-01" nil) buf))))))

(ert-deftest dorg-test-ql-plain-org-property-ts-name ()
  "(property-ts ... NAME) on plain-org restricts to one property key.
DORG_ID is never a candidate and an absent key yields no match."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--property-ts-content (:buffer buf)
    ;; NAME restricts to the named key only.
    (should (equal '(1 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil nil "DUE") buf))))
    (should (equal '(2 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil nil "REVIEW") buf))))
    ;; NAME + window: only Delta's REVIEW 2026-07-06 is in range.
    (should (equal '(4) (dorg-test-ql--ids
                         (dorg-ql-select '(property-ts "2026-07-01" "2026-07-31" "REVIEW") buf))))
    ;; Absent key: no match.
    (should (null (dorg-ql-select '(property-ts nil nil "MISSING") buf)))
    ;; DORG_ID is identity-only: never a property-ts candidate.
    (should (null (dorg-ql-select '(property-ts nil nil "DORG_ID") buf)))))

(ert-deftest dorg-test-ql-dolt-property-ts-window ()
  "(property-ts ...) on Dolt windows `heading_properties' candidates.
Covers positive, negative, and open-bound cases, a nil marker, and
dedup of a heading with two matching property timestamps."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--property-ts-content (:buffer buf)
    (let ((results (dorg-ql-select '(property-ts "2026-07-01" "2026-07-31") buf)))
      (should (equal '(1 4) (dorg-test-ql--ids results)))
      ;; Dolt results carry a nil marker.
      (should (null (plist-get (car results) :marker))))
    (should (null (dorg-ql-select '(property-ts "2026-01-01" "2026-06-30") buf)))
    (should (equal '(1 2 4) (dorg-test-ql--ids
                             (dorg-ql-select '(property-ts nil nil) buf))))
    (should (equal '(1 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil "2026-07-31") buf))))
    (should (equal '(2) (dorg-test-ql--ids
                         (dorg-ql-select '(property-ts "2026-08-01" nil) buf))))))

(ert-deftest dorg-test-ql-dolt-property-ts-name ()
  "(property-ts ... NAME) on Dolt restricts to one `prop_key'.
DORG_ID is never a candidate and an absent key yields no match."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--property-ts-content (:buffer buf)
    (should (equal '(1 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil nil "DUE") buf))))
    (should (equal '(2 4) (dorg-test-ql--ids
                           (dorg-ql-select '(property-ts nil nil "REVIEW") buf))))
    (should (equal '(4) (dorg-test-ql--ids
                         (dorg-ql-select '(property-ts "2026-07-01" "2026-07-31" "REVIEW") buf))))
    (should (null (dorg-ql-select '(property-ts nil nil "MISSING") buf)))
    (should (null (dorg-ql-select '(property-ts nil nil "DORG_ID") buf)))))

(ert-deftest dorg-test-ql-dolt-property-ts-no-whole-tree-load ()
  "Dolt property-ts lowering never materializes the whole tree.
The unified v2 planner may load a HEADLINE-ONLY adjacency map but never a
full body/item load (see `dorg-test-ql-dolt-planning-no-whole-tree-load')."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--property-ts-content (:buffer buf)
    (let ((full-load 0)
          (orig (symbol-function 'dorg-model-load-headings)))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (dolt-dir root-id ids
                          &optional as-of headline-only query-fn levels)
                   (should (functionp query-fn))
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only query-fn levels))))
        (should (equal '(1 2 4) (dorg-test-ql--ids
                                 (dorg-ql-select '(property-ts nil nil) buf))))
        (should (= 0 full-load))))))

(ert-deftest dorg-test-ql-dolt-property-ts-batched-property-load ()
  "Dolt property-ts loads result properties with one batched query.
The selector reads `heading_properties' once; the only `heading_id IN ('
load is the single property batch, independent of the result count
(here two matching headings)."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--property-ts-content (:buffer buf)
    (dorg-test-ql--counting-dolt-sql hp-count in-count
      (let ((results (dorg-ql-select '(property-ts "2026-07-01" "2026-07-31") buf)))
        (should (= 2 (length results)))
        ;; One batched property load, not one per result row.
        (should (= 1 in-count))
        (ignore hp-count)))))

(ert-deftest dorg-test-ql-property-ts-cross-backend-equality ()
  "Model, plain-org, and Dolt return the SAME heading-ids for property-ts.
Set equality over identical content; the model adapter is the baseline.
Exercises no-NAME, NAME, open/closed bounds, dedup, the unparseable
NOTE, an absent key, and DORG_ID exclusion."
  (let ((contents dorg-test-ql--property-ts-content)
        (queries '((property-ts nil nil)
                   (property-ts "2026-07-01" "2026-07-31")
                   (property-ts nil nil "DUE")
                   (property-ts nil nil "REVIEW")
                   (property-ts "2026-07-01" "2026-07-31" "REVIEW")
                   (property-ts "2026-08-15" nil)
                   (property-ts nil nil "MISSING")
                   (property-ts nil nil "DORG_ID"))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let* ((model (dorg-test-ql--model-from-org contents))
             (pids-list
              (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
                (mapcar (lambda (q) (dorg-test-ql--ids (dorg-ql-select q pbuf)))
                        queries))))
        (cl-loop for q in queries for pids in pids-list do
                 (let ((mids (dorg-test-ql--ids (dorg-ql-select q model)))
                       (dids (dorg-test-ql--ids (dorg-ql-select q dbuf))))
                   (should (equal mids dids))
                   (should (equal mids pids))))))))

;;; ─── logbook Lowering (Slice 3) ───────────────────────────────────────

(defconst dorg-test-ql--logbook-content
  "* Alpha
:PROPERTIES:
:DORG_ID: 1
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-05-10 Sun]
- Note taken on [2026-08-20 Thu]
:END:
* Beta
:PROPERTIES:
:DORG_ID: 2
:END:
:LOGBOOK:
- Rescheduled, was [2026-05-01 Fri]--[2026-05-31 Sun]
:END:
* Gamma
:PROPERTIES:
:DORG_ID: 3
:END:
* Delta
CLOSED: [2026-05-12 Tue]
:PROPERTIES:
:DORG_ID: 4
:DUE: [2026-05-13 Wed]
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" <2026-05-14 Thu>
:END:
Body mentions [2026-05-15 Fri] inactive here.
:CUSTOM:
- buried [2026-05-16 Sat]
:END:
"
  "Shared fixture for logbook lowering tests.
Alpha carries two inactive LOGBOOK timestamps (2026-05-10 and the
out-of-range 2026-08-20); Beta a single inactive `--' range spanning
all of May; Gamma no LOGBOOK at all.  Delta is the edge case: its only
LOGBOOK timestamp is ACTIVE (`<...>') so it never matches `logbook',
while its in-window INACTIVE timestamps live outside LOGBOOK — a CLOSED
planning line, a DUE property, body text, and a generic CUSTOM drawer —
none of which `logbook' may scan.")

(defconst dorg-test-ql--logbook-block-content
  "* Real
:PROPERTIES:
:DORG_ID: 1
:END:
:LOGBOOK:
- Note taken on [2026-05-10 Sun]
:END:
* Fake
:PROPERTIES:
:DORG_ID: 2
:END:
#+begin_example
:LOGBOOK:
- fake [2026-05-10 Sun]
:END:
#+end_example
* SrcFake
:PROPERTIES:
:DORG_ID: 3
:END:
#+begin_src text
:LOGBOOK:
- src fake [2026-05-10 Sun]
:END:
#+end_src
* QuoteFake
:PROPERTIES:
:DORG_ID: 4
:END:
#+begin_quote
:LOGBOOK:
- quote fake [2026-05-10 Sun]
:END:
#+end_quote
* ListFake
:PROPERTIES:
:DORG_ID: 5
:END:
- item
  :LOGBOOK:
  - list fake [2026-05-10 Sun]
  :END:
"
  "Fixture proving plain-org `logbook' scans only genuine drawer nodes.
Real (id 1) has a real LOGBOOK drawer with an in-window inactive
timestamp.  Fake (id 2) and SrcFake (id 3) each contain a
`:LOGBOOK:'/`:END:' pair INSIDE an example or source block -- block text,
not a drawer node.  QuoteFake (id 4) and ListFake (id 5) each contain a
genuine drawer node, but nested inside a quote block / list item, so it
is a drawer of that container, not of the entry's section.  The model
parser (which walks section children) and Dolt `drawer_items' ignore all
four, and plain-org must agree.")

(ert-deftest dorg-test-ql-plain-org-logbook-ignores-block-text ()
  "Plain-org `logbook' ignores fake LOGBOOK drawers outside the section.
A `:LOGBOOK:'/`:END:' pair inside an example or source block is block
content (not a drawer node), and one nested inside a quote block or list
item is a drawer of that container (not a direct child of the entry's
section), so only the genuine section-level LOGBOOK drawer (id 1) may
match.  Regression for the org-drawer-regexp text scan that matched block
text and for the recursive `org-element-map' that matched nested drawers."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--logbook-block-content (:buffer buf)
    (should (equal '(1)
                   (dorg-test-ql--ids
                    (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf))))
    (should (equal '(1)
                   (dorg-test-ql--ids
                    (dorg-ql-select '(logbook nil nil) buf))))))

(ert-deftest dorg-test-ql-logbook-block-text-cross-backend-equality ()
  "Model, plain-org, and Dolt agree non-section LOGBOOK pairs are not drawers.
Only the heading with a genuine section-level LOGBOOK drawer (id 1)
matches; the example/source-block fakes and the quote-block/list-item
nested drawers are ignored identically by all three backends (set
equality, model adapter as baseline)."
  (let ((contents dorg-test-ql--logbook-block-content)
        (queries '((logbook nil nil)
                   (logbook "2026-05-01" "2026-05-31"))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let* ((model (dorg-test-ql--model-from-org contents))
             (pids-list
              (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
                (mapcar (lambda (q) (dorg-test-ql--ids (dorg-ql-select q pbuf)))
                        queries))))
        (cl-loop for q in queries for pids in pids-list do
                 (let ((mids (dorg-test-ql--ids (dorg-ql-select q model)))
                       (dids (dorg-test-ql--ids (dorg-ql-select q dbuf))))
                   (should (equal mids dids))
                   (should (equal mids pids))))))))


(ert-deftest dorg-test-ql-plain-org-logbook-window ()
  "(logbook ...) on plain-org windows the entry's own LOGBOOK drawer lines.
Covers positive, negative, open-bound, distinct-window, range, dedup,
and the non-LOGBOOK / active-timestamp exclusions of Delta."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--logbook-content (:buffer buf)
    ;; Positive: Alpha's 2026-05-10 and Beta's May range fall in window.
    ;; Delta is excluded (its only LOGBOOK ts is active; its inactive
    ;; timestamps are all outside LOGBOOK).
    (let ((results (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf)))
      (should (equal '(1 2) (dorg-test-ql--ids results)))
      ;; Plain-org results carry a live marker.
      (should (markerp (plist-get (car results) :marker))))
    ;; Negative: a window touching no LOGBOOK timestamp.
    (should (null (dorg-ql-select '(logbook "2026-06-01" "2026-06-30") buf)))
    ;; Distinct window: only Alpha's August note is in range.
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(logbook "2026-08-01" "2026-08-31") buf))))
    ;; Open bounds: every heading with any inactive LOGBOOK timestamp.
    ;; Alpha carries two; it is still counted once (dedup).
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(logbook nil nil) buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(logbook nil "2026-08-31") buf))))
    (should (null (dorg-ql-select '(logbook "2026-09-01" nil) buf)))))

(ert-deftest dorg-test-ql-plain-org-logbook-ignores-non-logbook-timestamps ()
  "Plain-org `logbook' ignores inactive timestamps outside the LOGBOOK drawer.
Delta's CLOSED planning line, DUE property, body text, and generic
CUSTOM drawer all carry in-window inactive timestamps, and its only
LOGBOOK timestamp is active — so Delta never matches `logbook'."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--logbook-content (:buffer buf)
    ;; Delta (id 4) is never in any logbook result, narrow or open.
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(logbook nil nil) buf))))
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf))))))

(ert-deftest dorg-test-ql-dolt-logbook-window ()
  "(logbook ...) on Dolt windows `LOGBOOK' `drawer_lines' candidates.
Covers positive, negative, open-bound, distinct-window, range, dedup,
a nil marker, and the non-LOGBOOK / active-timestamp exclusions."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--logbook-content (:buffer buf)
    (let ((results (dorg-ql-select '(logbook "2026-05-01" "2026-05-31") buf)))
      (should (equal '(1 2) (dorg-test-ql--ids results)))
      ;; Dolt results carry a nil marker.
      (should (null (plist-get (car results) :marker))))
    (should (null (dorg-ql-select '(logbook "2026-06-01" "2026-06-30") buf)))
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(logbook "2026-08-01" "2026-08-31") buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(logbook nil nil) buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(logbook nil "2026-08-31") buf))))
    (should (null (dorg-ql-select '(logbook "2026-09-01" nil) buf)))
    ;; Delta's non-LOGBOOK / active timestamps never match.
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(logbook nil nil) buf))))))

(ert-deftest dorg-test-ql-dolt-logbook-no-whole-tree-load ()
  "Dolt logbook lowering never materializes the whole tree.
The unified v2 planner may load a HEADLINE-ONLY adjacency map but never a
full body/item load (see `dorg-test-ql-dolt-planning-no-whole-tree-load')."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--logbook-content (:buffer buf)
    (let ((full-load 0)
          (orig (symbol-function 'dorg-model-load-headings)))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (dolt-dir root-id ids
                          &optional as-of headline-only query-fn levels)
                   (should (functionp query-fn))
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only query-fn levels))))
        (should (equal '(1 2) (dorg-test-ql--ids
                               (dorg-ql-select '(logbook nil nil) buf))))
        (should (= 0 full-load))))))

(ert-deftest dorg-test-ql-logbook-cross-backend-equality ()
  "Model, plain-org, and Dolt return the SAME heading-ids for logbook.
Set equality over identical content; the model adapter is the baseline.
Exercises open/closed bounds, the May range, dedup, a distinct window,
and the non-LOGBOOK / active-timestamp exclusions of Delta."
  (let ((contents dorg-test-ql--logbook-content)
        (queries '((logbook nil nil)
                   (logbook "2026-05-01" "2026-05-31")
                   (logbook "2026-08-01" "2026-08-31")
                   (logbook "2026-06-01" "2026-06-30")
                   (logbook "2026-09-01" nil)
                   (logbook nil "2026-08-31"))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let* ((model (dorg-test-ql--model-from-org contents))
             (pids-list
              (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
                (mapcar (lambda (q) (dorg-test-ql--ids (dorg-ql-select q pbuf)))
                        queries))))
        (cl-loop for q in queries for pids in pids-list do
                 (let ((mids (dorg-test-ql--ids (dorg-ql-select q model)))
                       (dids (dorg-test-ql--ids (dorg-ql-select q dbuf))))
                   (should (equal mids dids))
                   (should (equal mids pids))))))))

;;; ─── Active-TS Predicate Lowering (Slice 4: active-ts) ─────────────────

(defconst dorg-test-ql--active-content
  "* Alpha
:PROPERTIES:
:DORG_ID: 1
:END:
Meeting <2026-05-10 Sun> and a later <2026-08-20 Thu> too.
* Beta
:PROPERTIES:
:DORG_ID: 2
:END:
Trip <2026-05-01 Fri>--<2026-05-31 Sun>.
* Gamma
:PROPERTIES:
:DORG_ID: 3
:END:
:CUSTOM:
active <2026-05-20 Wed> buried in a generic drawer
:END:
* Delta
SCHEDULED: <2026-05-14 Thu>
:PROPERTIES:
:DORG_ID: 4
:DUE: <2026-05-13 Wed>
:END:
:LOGBOOK:
- State \"DONE\" from \"TODO\" <2026-05-12 Tue>
:END:
Body mentions [2026-05-15 Fri] inactive only.
"
  "Shared fixture for active-ts lowering tests.
Alpha carries two active body timestamps (the in-range 2026-05-10 and the
out-of-range 2026-08-20); Beta a single active `--' range spanning all of
May.  Gamma's only active timestamp lives in a generic CUSTOM drawer, not
body text.  Delta is the edge case: every one of its active timestamps is
OUTSIDE the body -- a SCHEDULED active planning line, an active DUE
property, and an active LOGBOOK state-change -- while its only body
timestamp is INACTIVE.  So `active-ts' may match only Alpha and Beta; a
`dorg-text-item' is the sole source any backend scans.")

(ert-deftest dorg-test-ql-plain-org-active-ts-window ()
  "(active-ts ...) on plain-org windows the entry's own body timestamps.
Covers positive, negative, open-bound, distinct-window, range, dedup, and
the non-body / inactive exclusions of Gamma and Delta."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--active-content (:buffer buf)
    ;; Positive: Alpha's 2026-05-10 and Beta's May range fall in window.
    (let ((results (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") buf)))
      (should (equal '(1 2) (dorg-test-ql--ids results)))
      ;; Plain-org results carry a live marker.
      (should (markerp (plist-get (car results) :marker))))
    ;; Negative: a window touching no active body timestamp.
    (should (null (dorg-ql-select '(active-ts "2026-06-01" "2026-06-30") buf)))
    ;; Distinct window: only Alpha's August timestamp is in range.
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts "2026-08-01" "2026-08-31") buf))))
    ;; Open bounds: every heading with any active body timestamp.  Alpha
    ;; carries two; it is still counted once (dedup).
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(active-ts nil nil) buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(active-ts nil "2026-08-31") buf))))
    (should (null (dorg-ql-select '(active-ts "2026-09-01" nil) buf)))))

(ert-deftest dorg-test-ql-plain-org-active-ts-ignores-non-body-timestamps ()
  "Plain-org `active-ts' ignores active timestamps outside body text.
Gamma's only active timestamp is in a generic CUSTOM drawer; Delta's
active timestamps are a SCHEDULED planning line, a DUE property, and a
LOGBOOK line, and its only body timestamp is inactive -- so neither
heading ever matches `active-ts'."
  (dorg-test-ql--with-plain-org-file
      dorg-test-ql--active-content (:buffer buf)
    ;; Gamma (3) and Delta (4) are never in any active-ts result.
    (should-not (memq 3 (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts nil nil) buf))))
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts nil nil) buf))))
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") buf))))))

(ert-deftest dorg-test-ql-dolt-active-ts-window ()
  "(active-ts ...) on Dolt windows `text_items.text_raw' candidates.
Covers positive, negative, open-bound, distinct-window, range, dedup, a
nil marker, and the non-body / inactive exclusions."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--active-content (:buffer buf)
    (let ((results (dorg-ql-select '(active-ts "2026-05-01" "2026-05-31") buf)))
      (should (equal '(1 2) (dorg-test-ql--ids results)))
      ;; Dolt results carry a nil marker.
      (should (null (plist-get (car results) :marker))))
    (should (null (dorg-ql-select '(active-ts "2026-06-01" "2026-06-30") buf)))
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts "2026-08-01" "2026-08-31") buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(active-ts nil nil) buf))))
    (should (equal '(1 2) (dorg-test-ql--ids
                           (dorg-ql-select '(active-ts nil "2026-08-31") buf))))
    (should (null (dorg-ql-select '(active-ts "2026-09-01" nil) buf)))
    ;; Gamma's drawer / Delta's non-body active timestamps never match.
    (should-not (memq 3 (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts nil nil) buf))))
    (should-not (memq 4 (dorg-test-ql--ids
                         (dorg-ql-select '(active-ts nil nil) buf))))))

(ert-deftest dorg-test-ql-dolt-active-ts-no-whole-tree-load ()
  "Dolt active-ts lowering never materializes the whole tree.
The candidate selector touches only `text_items' (root-scoped, LIKE
prefiltered); the unified v2 planner may additionally load a HEADLINE-ONLY
adjacency map but never a full body/item load."
  (dorg-test-ql--with-dolt-synced
      dorg-test-ql--active-content (:buffer buf)
    (let ((full-load 0)
          (orig (symbol-function 'dorg-model-load-headings)))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (dolt-dir root-id ids
                          &optional as-of headline-only query-fn levels)
                   (should (functionp query-fn))
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only query-fn levels))))
        (should (equal '(1 2) (dorg-test-ql--ids
                               (dorg-ql-select '(active-ts nil nil) buf))))
        (should (= 0 full-load))))))

(ert-deftest dorg-test-ql-active-ts-cross-backend-equality ()
  "Model, plain-org, and Dolt return the SAME heading-ids for active-ts.
Set equality over identical content; the model adapter is the baseline.
Exercises open/closed bounds, the May range, dedup, a distinct window,
and the non-body / inactive exclusions of Gamma and Delta."
  (let ((contents dorg-test-ql--active-content)
        (queries '((active-ts nil nil)
                   (active-ts "2026-05-01" "2026-05-31")
                   (active-ts "2026-08-01" "2026-08-31")
                   (active-ts "2026-06-01" "2026-06-30")
                   (active-ts "2026-09-01" nil)
                   (active-ts nil "2026-08-31"))))
    (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
      (let* ((model (dorg-test-ql--model-from-org contents))
             (pids-list
              (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
                (mapcar (lambda (q) (dorg-test-ql--ids (dorg-ql-select q pbuf)))
                        queries))))
        (cl-loop for q in queries for pids in pids-list do
                 (let ((mids (dorg-test-ql--ids (dorg-ql-select q model)))
                       (dids (dorg-test-ql--ids (dorg-ql-select q dbuf))))
                   (should (equal mids dids))
                   (should (equal mids pids))))))))

;;; ─── Window Bound & Form Normalization ────────────────────────────────

;; Unit tests at the normalization layer: `dorg-ql-normalize' lowers a
;; timestamp alias's window bounds and convenience window forms into a
;; `(:timestamp SOURCE :overlaps (START . END))' node; `dorg-test-ql--alias-window'
;; reconstructs the historical `(HEAD FROM-TRIPLE TO-TRIPLE [NAME])' shape
;; from that node so these span tests survive the atomic-validator retirement.
;; A bound contributes its
;; span start to FROM and its span end to TO, so a date-range bound widens
;; the window; clock times never reach the date bound.

(ert-deftest dorg-test-ql-window-bound-raw-org-timestamps ()
  "Window bounds accept raw active/inactive Org timestamp strings.
Active vs. inactive delimiters do not affect the normalized bound, and a
trailing clock time is ignored for the date span."
  (should (equal (dorg-test-ql--alias-window '(scheduled "<2026-05-15 Fri>" "<2026-05-20 Wed>"))
                 '(scheduled (2026 5 15) (2026 5 20))))
  (should (equal (dorg-test-ql--alias-window '(scheduled "[2026-05-15 Fri]" "[2026-05-20 Wed]"))
                 '(scheduled (2026 5 15) (2026 5 20))))
  (should (equal (dorg-test-ql--alias-window '(scheduled "<2026-05-15 Fri 09:30>" nil))
                 '(scheduled (2026 5 15) nil))))

(ert-deftest dorg-test-ql-window-bound-range-uses-span-ends ()
  "A date-range bound contributes its start to FROM and its end to TO."
  (should (equal (dorg-test-ql--alias-window
                  '(scheduled "2026-05-01"
                              "<2026-06-01 Mon>--<2026-06-30 Tue>"))
                 '(scheduled (2026 5 1) (2026 6 30))))
  ;; As FROM, the range's start date is used.
  (should (equal (dorg-test-ql--alias-window
                  '(scheduled "<2026-06-01 Mon>--<2026-06-30 Tue>" nil))
                 '(scheduled (2026 6 1) nil))))

(ert-deftest dorg-test-ql-window-bound-dorg-timestamp-struct ()
  "Window bounds accept `dorg-timestamp' structs, including ranges."
  (let ((ts (dorg-timestamp-parse "<2026-05-15 Fri>"))
        (range (dorg-timestamp-parse "<2026-06-01 Mon>--<2026-06-30 Tue>")))
    (should (equal (dorg-test-ql--alias-window (list 'scheduled ts nil))
                   '(scheduled (2026 5 15) nil)))
    ;; A struct range contributes start to FROM, end to TO.
    (should (equal (dorg-test-ql--alias-window (list 'scheduled nil range))
                   '(scheduled nil (2026 6 30))))))

(ert-deftest dorg-test-ql-window-form-between-on-before-after ()
  "Convenience window forms expand to `(FROM TO)' bound pairs.
`on' a single date is a one-day window and `on' a range spans the range;
`before' is on-or-before (open FROM) and `after' on-or-after (open TO)."
  (should (equal (dorg-test-ql--alias-window '(scheduled (between "2026-05-01" "2026-05-31")))
                 '(scheduled (2026 5 1) (2026 5 31))))
  ;; between honors open bounds.
  (should (equal (dorg-test-ql--alias-window '(scheduled (between nil "2026-05-31")))
                 '(scheduled nil (2026 5 31))))
  (should (equal (dorg-test-ql--alias-window '(scheduled (on "2026-05-15")))
                 '(scheduled (2026 5 15) (2026 5 15))))
  (should (equal (dorg-test-ql--alias-window '(scheduled (on "<2026-06-01 Mon>--<2026-06-30 Tue>")))
                 '(scheduled (2026 6 1) (2026 6 30))))
  (should (equal (dorg-test-ql--alias-window '(scheduled (before "2026-05-31")))
                 '(scheduled nil (2026 5 31))))
  (should (equal (dorg-test-ql--alias-window '(scheduled (after "2026-05-01")))
                 '(scheduled (2026 5 1) nil))))

(ert-deftest dorg-test-ql-window-form-with-name ()
  "A window-name predicate accepts a window form plus an optional NAME."
  (should (equal (dorg-test-ql--alias-window '(property-ts (on "2026-07-01")))
                 '(property-ts (2026 7 1) (2026 7 1) nil)))
  (should (equal (dorg-test-ql--alias-window '(property-ts (between "2026-07-01" "2026-07-31") "DUE"))
                 '(property-ts (2026 7 1) (2026 7 31) "DUE")))
  ;; NAME must still be a string when present.
  (should-error (dorg-test-ql--alias-window '(property-ts (today) 5)) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-window-form-relative-deterministic ()
  "`today'/`this-week' read `dorg-ql-current-time' for deterministic windows.
Changing `dorg-ql-week-start-day' shifts the resolved week (transformation
check: the two week-start choices produce different windows)."
  (let ((dorg-ql-current-time (encode-time (list 0 0 12 15 5 2026 nil -1 nil))))
    (should (equal (dorg-test-ql--alias-window '(scheduled (today)))
                   '(scheduled (2026 5 15) (2026 5 15))))
    ;; 2026-05-15 is a Friday.  Monday-start week is 05-11..05-17.
    (let ((dorg-ql-week-start-day 1))
      (should (equal (dorg-test-ql--alias-window '(scheduled (this-week)))
                     '(scheduled (2026 5 11) (2026 5 17)))))
    ;; Sunday-start week is the distinct 05-10..05-16.
    (let ((dorg-ql-week-start-day 0))
      (should (equal (dorg-test-ql--alias-window '(scheduled (this-week)))
                     '(scheduled (2026 5 10) (2026 5 16)))))))

(ert-deftest dorg-test-ql-window-bound-errors ()
  "Invalid window bounds and malformed window forms signal `dorg-ql-error'."
  ;; Unparseable string bound.
  (should-error (dorg-ql-select '(scheduled "not-a-timestamp" nil) nil)
                :type 'dorg-ql-error)
  ;; Non-zero-padded date is neither a bare date nor a parseable timestamp.
  (should-error (dorg-ql-select '(scheduled "2026-5-1" nil) nil)
                :type 'dorg-ql-error)
  ;; A non-string, non-struct bound.
  (should-error (dorg-ql-select '(scheduled 20260515 nil) nil)
                :type 'dorg-ql-error)
  ;; Window forms with bad arity.
  (should-error (dorg-ql-select '(scheduled (between "2026-05-01")) nil)
                :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(scheduled (on)) nil) :type 'dorg-ql-error)
  (should-error (dorg-ql-select '(scheduled (today 5)) nil) :type 'dorg-ql-error)
  ;; on/before/after require a non-open date.
  (should-error (dorg-ql-select '(scheduled (on nil)) nil) :type 'dorg-ql-error)
  ;; A single non-window-form argument is not a valid window.
  (should-error (dorg-ql-select '(scheduled "2026-05-01") nil)
                :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-window-bound-warning-cookie ()
  "A deadline warning cookie is accepted as a window bound.
`dorg-timestamp-parse' now models warning cookies (e.g. the `-3d' in
`<2026-05-20 Wed -3d>'), so such a string is a parseable bound.  The
warning offset is advisory and never shifts the date span: the bound
normalizes to its literal start/end date, exactly like a repeater base
occurrence."
  ;; Relative and absolute warning forms both normalize to the literal date.
  (should (equal (dorg-test-ql--alias-window
                  '(deadline "<2026-05-20 Wed -3d>" "<2026-05-25 Mon --1w>"))
                 '(deadline (2026 5 20) (2026 5 25))))
  ;; Warning offset does not widen the window via convenience forms.
  (should (equal (dorg-test-ql--alias-window '(deadline (on "<2026-05-20 Wed -3d>")))
                 '(deadline (2026 5 20) (2026 5 20))))
  (should (equal (dorg-test-ql--alias-window '(deadline (before "<2026-05-31 Sun -2d>")))
                 '(deadline nil (2026 5 31))))
  (should (equal (dorg-test-ql--alias-window '(deadline (after "<2026-05-01 Fri -1m>")))
                 '(deadline (2026 5 1) nil)))
  ;; A warning cookie may coexist with a repeater on the same bound.
  (should (equal (dorg-test-ql--alias-window '(deadline "<2026-05-20 Wed +1w -3d>" nil))
                 '(deadline (2026 5 20) nil))))

(ert-deftest dorg-test-ql-window-form-selects-via-model ()
  "Window forms and timestamp bounds drive real selection unchanged.
The model adapter consumes the normalized `(FROM TO)' triples, so source,
clock-time, and repeater base-occurrence semantics are preserved."
  (let ((headings
         (list (dorg-test-ql--ts-heading
                1 "Mid"
                (list (dorg-test-ql--planning :scheduled "<2026-05-15 Fri>")))
               (dorg-test-ql--ts-heading
                2 "Late"
                (list (dorg-test-ql--planning :scheduled "<2026-09-01 Tue>"))))))
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select
                          '(scheduled (between "2026-05-01" "2026-05-31"))
                          headings))))
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(scheduled (on "2026-05-15")) headings))))
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select '(scheduled (before "2026-06-01")) headings))))
    (should (equal '(2) (dorg-test-ql--ids
                         (dorg-ql-select '(scheduled (after "2026-06-01")) headings))))
    ;; Relative today selects the heading scheduled on the reference day.
    (let ((dorg-ql-current-time (encode-time (list 0 0 12 15 5 2026 nil -1 nil))))
      (should (equal '(1) (dorg-test-ql--ids
                           (dorg-ql-select '(scheduled (today)) headings)))))
    ;; Raw Org timestamp string bounds also drive selection.
    (should (equal '(1) (dorg-test-ql--ids
                         (dorg-ql-select
                          '(scheduled "<2026-05-01 Fri>" "<2026-05-31 Sun>")
                          headings))))))

;;; ─── dorg-find Compatibility ──────────────────────────────────────────

(ert-deftest dorg-test-ql-find-shape-compatible ()
  "`dorg-find-heading-by-id' (routed through dorg-ql) keeps its shape."
  (dorg-test-ql--with-plain-org-file
      "* H\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
      (:buffer buf)
    (let* ((via-find (dorg-find-heading-by-id 7 buf))
           (via-ql (dorg-ql-select '(id 7) buf)))
      (should (equal (plist-get (car via-find) :heading-id)
                     (plist-get (car via-ql) :heading-id)))
      (should (equal (plist-get (car via-find) :title)
                     (plist-get (car via-ql) :title))))))

;;; ─── Dolt v2 Normalized-AST Planner ───────────────────────────────────

;; The v2 planner lowers a normalized `(:pipe (:source :all) STEP...)' AST
;; over a single Dolt root, reusing the model evaluator's matching/traversal
;; semantics as the baseline.  These tests pin the covered surface (boolean
;; composition, headline/property/tag/id predicates, timestamp windows,
;; traversal, de-dup/order), atomic-alias compatibility, the fail-closed
;; behavior for negated content predicates, and the no-body-load discipline.

(defconst dorg-test-ql--v2-doc
  "#+EXPORT_FILE_NAME: root-file
* [#A] Alpha :work:
DEADLINE: <2026-06-03 Wed>
:PROPERTIES:
:DORG_ID: 1
:END:
** TODO [#B] Alpha One
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
#+EXPORT_FILE_NAME: alpha-one
** DONE [#C] COMMENT Alpha Two
:PROPERTIES:
:DORG_ID: 3
:END:
* Beta :home:
:PROPERTIES:
:DORG_ID: 4
:END:
** [#A] Beta One
:PROPERTIES:
:DORG_ID: 5
:END:
*** Beta One a
:PROPERTIES:
:DORG_ID: 6
:END:
"
  "Document-order tree mirroring the model evaluator fixture, sans root.
Ids 1..6: Alpha(1 #A :work: DEADLINE 2026-06-03), Alpha One(2 #B TODO
FOO=bar, EXPORT_FILE_NAME=alpha-one), Alpha Two(3 #C DONE COMMENT),
Beta(4 :home:), Beta One(5 #A), Beta One a(6).")

(defun dorg-test-ql--titles (results)
  "Return the ordered `:title' list of query RESULTS.
Titles are stable across id allocation, so order assertions do not depend
on the snowflake heading ids."
  (mapcar (lambda (r) (plist-get r :title)) results))

(defun dorg-test-ql--v2-titles (query buf)
  "Run v2 QUERY against BUF and return matching titles in result order."
  (dorg-test-ql--titles (dorg-ql-select query buf)))

(ert-deftest dorg-test-ql-dolt-v2-source-all-document-order ()
  "A bare v2 source pipe returns every non-root heading in document order."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha" "Alpha One" "Alpha Two" "Beta" "Beta One"
                     "Beta One a")
                   (dorg-test-ql--v2-titles '(pipe (where (>= (field level) 1)))
                                            buf)))))

(ert-deftest dorg-test-ql-dolt-v2-field-predicates ()
  "Headline field predicates lower with model string/ordered semantics."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha One" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles '(contains (field title) "One") buf)))
    (should (equal '("Alpha" "Alpha One" "Alpha Two")
                   (dorg-test-ql--v2-titles '(prefix (field title) "Alpha") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(field todo "TODO") buf)))
    (should (equal '("Alpha One" "Alpha Two" "Beta One")
                   (dorg-test-ql--v2-titles '(field level 2) buf)))
    ;; commented is a real boolean: only the COMMENT heading is t.
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles '(field commented t) buf)))))

(ert-deftest dorg-test-ql-dolt-v2-priority-org-order ()
  "Priority orders by Org rank (A highest), not raw string comparison."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha" "Alpha One" "Beta One")
                   (dorg-test-ql--v2-titles '(>= (field priority) "B")
                                            buf)))
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles '(< (field priority) "B") buf)))
    (should (equal '("Alpha" "Beta One")
                   (dorg-test-ql--v2-titles '(field priority "A") buf)))))

(ert-deftest dorg-test-ql-dolt-v2-id-property-tag-keyword ()
  "id/property/tag/keyword predicates lower with heading-scoped rules."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(field id 2) buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(property "FOO" "bar") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(exists (property "FOO")) buf)))
    ;; Only `missing' matches an absent key.
    (should (equal '("Alpha" "Alpha Two" "Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles '(missing (property "FOO")) buf)))
    ;; DORG_ID is never a model/Dolt property: identity is owned by `id'.
    (should (null (dorg-test-ql--v2-titles '(property "DORG_ID" "1") buf)))
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles '(tag "work") buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles '(exists (tag)) buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(keyword "EXPORT_FILE_NAME" "alpha-one") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(contains (keyword "EXPORT_FILE_NAME") "alpha") buf)))
    (should (null (dorg-test-ql--v2-titles
                   '(keyword "EXPORT_FILE_NAME" "root-file") buf)))
    (should (equal '("Alpha" "Alpha Two" "Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(missing (keyword "EXPORT_FILE_NAME")) buf)))))

(ert-deftest dorg-test-ql-dolt-v2-boolean-composition ()
  "and/or/not compose bounded candidate sets in document order."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(and (field level 2)
                          (>= (field priority) "B")
                          (exists (property "FOO")))
                    buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles '(or (tag "work") (tag "home"))
                                            buf)))
    ;; not over a headline predicate is a sound root-scoped complement.
    (should (equal '("Alpha One" "Alpha Two" "Beta" "Beta One a")
                   (dorg-test-ql--v2-titles '(not (field priority "A"))
                                            buf)))))

(ert-deftest dorg-test-ql-dolt-v2-pipe-and-unique ()
  "Interleaved where filters apply in order; unique is a no-op on a clean set."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Beta One")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field priority "A"))
                           (where (field level 2)))
                    buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field level 1)) (unique)) buf)))))

(ert-deftest dorg-test-ql-dolt-v2-traversal ()
  "Traversal walks parent_id adjacency, deduped and in document order.
The synthetic level-0 root is excluded, so a top-level heading's parent
resolves to nothing — matching the plain-org/model baseline."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha One" "Alpha Two")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 1)) (traverse children)) buf)))
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 2)) (traverse parent)) buf)))
    ;; Parent of a top-level heading is the absent root -> empty.
    (should (null (dorg-test-ql--v2-titles
                   '(pipe (where (id 1)) (traverse parent)) buf)))
    ;; Ancestors of "Beta One a" climb its real spine in document order.
    (should (equal '("Beta" "Beta One")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 6)) (traverse ancestors)) buf)))
    (should (equal '("Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 4)) (traverse descendants)) buf)))
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 2)) (traverse siblings)) buf)))
    (should (equal '("Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 4)) (traverse self descendants)) buf)))))

(ert-deftest dorg-test-ql-dolt-v2-command-facade-traversal ()
  "The command/CLI facade routes a composable v2 form to the Dolt planner.
`dorg-command-run-query' is the UI-free entry that the `dorg-app.el' CLI and
the HTTP serve path both drive.  A `(pipe (where ...) (traverse children))'
form on a Dolt-backed buffer must return the planner's hits, NOT signal the
historic fail-closed \"composable v2 queries are not supported on the dolt
backend\" error (which `dorg-command-run-query' would re-raise as a
`user-error').  Regression guard: docs/CLI help had claimed
the query surface was atomic-only, while the README flagship example is a
composable pipeline that must run on Dolt end-to-end."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha One" "Alpha Two")
                   (dorg-test-ql--titles
                    (dorg-command-run-query
                     '(pipe (where (id 1)) (traverse children)) buf))))))

(ert-deftest dorg-test-ql-dolt-v2-traverse-dedup-shared-parent ()
  "Traversing to a shared parent yields that parent exactly once."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field parent-id 1)) (traverse parent))
                    buf)))))

(ert-deftest dorg-test-ql-dolt-v2-timestamp-window ()
  "A v2 timestamp filter windows the DEADLINE projection via the scanner."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(timestamp deadline :overlaps (between "2026-06-01"
                                                            "2026-06-30"))
                    buf)))
    (should (null (dorg-test-ql--v2-titles
                   '(timestamp deadline :overlaps (between "2026-07-01"
                                                           "2026-07-31"))
                   buf)))))

(ert-deftest dorg-test-ql-dolt-v2-due-this-week-high-priority-children ()
  "High-priority children of headings due this week compose cleanly.
Alpha (id 1) is due 2026-06-03; with the week anchored there its only
priority >= B child is Alpha One."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      (should (equal '("Alpha One")
                     (dorg-test-ql--v2-titles
                      '(pipe (where (deadline (this-week)))
                             (traverse children)
                             (where (>= (field priority) "B")))
                      buf))))))

(ert-deftest dorg-test-ql-dolt-v2-due-this-week-ancestors ()
  "Ancestors of headings due this week climb to the top-level heading.
Alpha (id 1) is due this week; being top-level its only ancestor (the
synthetic root) is excluded, so the ancestor set is empty — while a deeper
due heading would climb its real spine."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      (should (null (dorg-test-ql--v2-titles
                     '(pipe (where (deadline (this-week)))
                            (traverse ancestors))
                     buf))))))

(ert-deftest dorg-test-ql-dolt-v2-matches-model-baseline ()
  "Dolt v2 results equal the model evaluator over the same id-tree.
Builds a hand model tree (no synthetic root, ids 1..6) and asserts the
Dolt planner reproduces the model's id order for a composed query."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let* ((model
            (list
             (make-dorg-heading :id 1 :parent-id nil :level 1 :level-offset 1 :title "Alpha"
                                :priority "A" :tags '("work"))
             (make-dorg-heading :id 2 :parent-id 1 :level 2 :level-offset 1 :title "Alpha One"
                                :priority "B" :todo "TODO")
             (make-dorg-heading :id 3 :parent-id 1 :level 2 :level-offset 1 :title "Alpha Two"
                                :priority "C" :todo "DONE" :commented t)
             (make-dorg-heading :id 4 :parent-id nil :level 1 :level-offset 1 :title "Beta"
                                :tags '("home"))
             (make-dorg-heading :id 5 :parent-id 4 :level 2 :level-offset 1 :title "Beta One"
                                :priority "A")
             (make-dorg-heading :id 6 :parent-id 5 :level 3 :level-offset 1
                                :title "Beta One a")))
           (query '(pipe (where (field level 1)) (traverse descendants)))
           (model-ids (mapcar (lambda (r) (plist-get r :heading-id))
                              (dorg-ql--model-eval (dorg-ql-normalize query)
                                                   model)))
           (dolt-ids (mapcar (lambda (r) (plist-get r :heading-id))
                             (dorg-ql-select query buf))))
      (should (equal model-ids dolt-ids))
      (should (equal '(2 3 5 6) dolt-ids)))))

(ert-deftest dorg-test-ql-dolt-v2-result-shape ()
  "v2 results keep the exact four-key markerless plist with property alist."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((r (car (dorg-ql-select '(id 2) buf))))
      (should (equal '(:heading-id :marker :title :properties)
                     (cl-loop for (k _v) on r by #'cddr collect k)))
      (should (= 2 (plist-get r :heading-id)))
      (should (equal "Alpha One" (plist-get r :title)))
      (should (null (plist-get r :marker)))
      (should (equal '(("FOO" . "bar")) (plist-get r :properties))))))

(ert-deftest dorg-test-ql-dolt-v2-root-scoped ()
  "v2 candidate sets and traversal never leak across roots."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((temp-base (dorg-test--make-owned-temp-dir
                       "dorg-ql-dolt-v2-roots-" "ql-dolt-v2-roots"))
           (dolt-dir (expand-file-name "dolt/" temp-base))
           (file-a (expand-file-name "a.org" temp-base))
           (file-b (expand-file-name "b.org" temp-base)))
      (unwind-protect
          (let ((dorg--registry nil))
            (dorg-init dolt-dir)
            (with-temp-buffer
              (insert "* RootB :shared:\n:PROPERTIES:\n:DORG_ID: 91\n:END:\n")
              (org-mode)
              (set-visited-file-name file-b t)
              (set-buffer-modified-p nil)
              (dorg-import-document
               (current-buffer) :dolt-dir dolt-dir
               :target-file file-b :mode 'bootstrap))
            (with-temp-buffer
              (insert "* RootA :shared:\n:PROPERTIES:\n:DORG_ID: 90\n:END:\n")
              (org-mode)
              (set-visited-file-name file-a t)
              (set-buffer-modified-p nil)
              (dorg-import-document
               (current-buffer) :dolt-dir dolt-dir
               :target-file file-a :mode 'bootstrap)
              (let ((reg-a (dorg-bind-dolt file-a dolt-dir))
                    (buf-a (current-buffer)))
                (ignore reg-a)
                (should (equal '("RootA")
                               (dorg-test-ql--v2-titles
                                '(pipe (where (tag "shared"))) buf-a))))))
        (dorg-test--delete-owned-root temp-base)))))

(ert-deftest dorg-test-ql-dolt-v2-no-body-load-for-headline-query ()
  "A v2 headline/traversal query touches no body/item tables.
Only headline-only rows + tags (and, for properties, the
`heading_properties' view) may load; `text_items'/`planning_items'/
`drawer_lines' must not be queried."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((body-touched 0)
          (orig-live (symbol-function 'dorg-sql))
          (orig-pinned (symbol-function 'dorg-sql--at-revision)))
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (dolt-dir query &rest args)
                   (let ((statement (if args (apply #'format query args) query)))
                     (when (string-match-p
                            "text_items\\|planning_items\\|drawer_lines\\|keyword_items"
                            statement)
                       (setq body-touched (1+ body-touched))))
                   (apply orig-live dolt-dir query args)))
                ((symbol-function 'dorg-sql--at-revision)
                 (lambda (dolt-dir revision query &rest args)
                   (let ((statement (if args (apply #'format query args) query)))
                     (when (string-match-p
                            "text_items\\|planning_items\\|drawer_lines\\|keyword_items"
                            statement)
                       (setq body-touched (1+ body-touched))))
                   (apply orig-pinned dolt-dir revision query args))))
        (let ((results (dorg-ql-select
                        '(pipe (where (field level 1)) (traverse children))
                        buf)))
          (should (equal '("Alpha One" "Alpha Two" "Beta One")
                         (dorg-test-ql--titles results)))
          (should (= 0 body-touched)))))))

(ert-deftest dorg-test-ql-dolt-v2-fail-closed-negated-timestamp ()
  "Negating a timestamp predicate fails closed rather than scanning bodies."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (should-error (dorg-ql-select
                   '(not (timestamp deadline :overlaps (between "2026-06-01"
                                                                "2026-06-30")))
                   buf)
                  :type 'dorg-ql-error)
    ;; Even nested under a boolean the negated timestamp is unlowerable.
    (should-error (dorg-ql-select
                   '(and (field level 1)
                         (not (deadline "2026-06-01" "2026-06-30")))
                   buf)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-plain-v2-now-supported ()
  "Composable v2 queries now execute on a plain-org backend (was deferred).
A well-formed pipeline returns results; only genuinely malformed forms or
unsupported shapes still signal `dorg-ql-error'."
  (dorg-test-ql--with-plain-org-file
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (should (equal '("H")
                   (dorg-test-ql--v2-titles '(pipe (where (field level 1)))
                                            buf)))
    (should (equal '("H")
                   (dorg-test-ql--v2-titles '(and (id 1) (field level 1))
                                            buf)))
    ;; A malformed v2 form still fails closed.
    (should-error (dorg-ql-select '(pipe (where (= (field bogus) 1))) buf)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select '(pipe (traverse nonsense)) buf)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-dolt-v2-atomic-alias-compatibility ()
  "Atomic aliases run unchanged even though v2 lowers them too.
The atomic path still answers (id ...)/(property ...)/(tag ...)/(title ...)
with the existing shape, so v2 wiring does not regress atomic Dolt."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((r (car (dorg-ql-select '(id 1) buf))))
      (should (= 1 (plist-get r :heading-id)))
      (should (equal "Alpha" (plist-get r :title)))
      (should (null (plist-get r :marker))))
    (should (equal '("Alpha One")
                   (dorg-test-ql--titles
                    (dorg-ql-select '(property "FOO" "bar") buf))))
    (should (equal '("Alpha")
                   (dorg-test-ql--titles (dorg-ql-select '(tag "work") buf))))))

;;; ─── Plain-org v2 AST adapter ─────────────────────────────────────────

;; The plain-org v2 executor runs a normalized AST over a registered plain
;; Org buffer.  These tests author clean Org text, run composed v2 queries
;; through `dorg-ql-select', and cross-check against the model evaluator over
;; the SAME text (`dorg-test-ql--model-from-org') so plain-org cannot drift
;; from the accepted model baseline.  The tree mirrors the model eval tree
;; (ids 1..6, no synthetic root):
;;
;;   * [#A] Alpha :work:   DEADLINE <2026-06-03 Wed>   (1)
;;   ** TODO [#B] Alpha One            FOO=bar          (2)
;;   ** DONE [#C] COMMENT Alpha Two                     (3)
;;   * Beta :home:                                      (4)
;;   ** [#A] Beta One                                   (5)
;;   *** Beta One a                                     (6)

(defvar dorg-test-ql--v2-plain-doc
  "#+EXPORT_FILE_NAME: root-file
* [#A] Alpha :work:
DEADLINE: <2026-06-03 Wed>
:PROPERTIES:
:DORG_ID: 1
:END:
** TODO [#B] Alpha One
:PROPERTIES:
:DORG_ID: 2
:FOO: bar
:END:
#+EXPORT_FILE_NAME: alpha-one
** DONE [#C] COMMENT Alpha Two
:PROPERTIES:
:DORG_ID: 3
:END:
* Beta :home:
:PROPERTIES:
:DORG_ID: 4
:END:
** [#A] Beta One
:PROPERTIES:
:DORG_ID: 5
:END:
*** Beta One a
:PROPERTIES:
:DORG_ID: 6
:END:
"
  "Clean plain-org text backing the plain-org v2 adapter tests.")

(defun dorg-test-ql--v2-ids (query buf)
  "Run v2 QUERY against BUF and return matching heading-ids in result order."
  (mapcar (lambda (r) (plist-get r :heading-id)) (dorg-ql-select query buf)))

(defun dorg-test-ql--model-v2-titles (query contents)
  "Run v2 QUERY through the model evaluator over CONTENTS' parsed tree.
Used as the convergence baseline for the plain-org executor."
  (mapcar (lambda (r) (plist-get r :title))
          (dorg-ql--model-eval (dorg-ql-normalize query)
                               (dorg-test-ql--model-from-org contents))))

(defun dorg-test-ql--plain-v2-titles-and-projection-count (query buf)
  "Run v2 QUERY against BUF and return (TITLES . PROJECTION-COUNT)."
  (let ((calls 0)
        (orig (symbol-function 'dorg-parse-section-items-at-heading)))
    (cl-letf (((symbol-function 'dorg-parse-section-items-at-heading)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply orig args))))
      (cons (dorg-test-ql--v2-titles query buf) calls))))

(ert-deftest dorg-test-ql-plain-v2-source-all-document-order ()
  "A bare v2 source pipe returns every heading in document order."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha" "Alpha One" "Alpha Two" "Beta" "Beta One"
                     "Beta One a")
                   (dorg-test-ql--v2-titles '(pipe (where (>= (field level) 1)))
                                            buf)))))

(ert-deftest dorg-test-ql-plain-v2-field-predicates ()
  "Headline field predicates lower with model string/ordered/boolean semantics."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha One" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles '(contains (field title) "One") buf)))
    (should (equal '("Alpha" "Alpha One" "Alpha Two")
                   (dorg-test-ql--v2-titles '(prefix (field title) "Alpha") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(field todo "TODO") buf)))
    (should (equal '("Alpha One" "Alpha Two" "Beta One")
                   (dorg-test-ql--v2-titles '(field level 2) buf)))
    ;; commented is a real boolean: only the COMMENT heading is t.
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles '(field commented t) buf)))))

(ert-deftest dorg-test-ql-plain-v2-priority-org-order ()
  "Priority orders by Org rank (A highest), not raw string comparison."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha" "Alpha One" "Beta One")
                   (dorg-test-ql--v2-titles '(>= (field priority) "B")
                                            buf)))
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles '(< (field priority) "B") buf)))
    (should (equal '("Alpha" "Beta One")
                   (dorg-test-ql--v2-titles '(field priority "A") buf)))))

(ert-deftest dorg-test-ql-plain-v2-id-property-tag-keyword-local ()
  "id/property/tag/keyword lower with heading-scoped, local rules."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(field id 2) buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(property "FOO" "bar") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles '(exists (property "FOO")) buf)))
    ;; Only `missing' matches an absent key.
    (should (equal '("Alpha" "Alpha Two" "Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles '(missing (property "FOO")) buf)))
    ;; DORG_ID is identity, never a user property: it is owned by `id'.
    (should (null (dorg-test-ql--v2-titles '(property "DORG_ID" "1") buf)))
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles '(tag "work") buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles '(exists (tag)) buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(keyword "EXPORT_FILE_NAME" "alpha-one") buf)))
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(contains (keyword "EXPORT_FILE_NAME") "alpha") buf)))
    ;; The file-level root owns EXPORT_FILE_NAME=root-file.  Query keyword
    ;; predicates are heading-scoped and must not select that root host.
    (should (null (dorg-test-ql--v2-titles
                   '(keyword "EXPORT_FILE_NAME" "root-file") buf)))
    (should (equal '("Alpha" "Alpha Two" "Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(missing (keyword "EXPORT_FILE_NAME")) buf)))))

(ert-deftest dorg-test-ql-plain-v2-boolean-composition ()
  "and/or/not compose the candidate set in document order."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha One")
                   (dorg-test-ql--v2-titles
                    '(and (field level 2)
                          (>= (field priority) "B")
                          (exists (property "FOO")))
                    buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles '(or (tag "work") (tag "home"))
                                            buf)))
    (should (equal '("Alpha One" "Alpha Two" "Beta" "Beta One a")
                   (dorg-test-ql--v2-titles '(not (field priority "A"))
                                            buf)))))

(ert-deftest dorg-test-ql-plain-v2-pipe-and-unique ()
  "Interleaved where filters apply in order; unique is a no-op on a clean set."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Beta One")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field priority "A"))
                           (where (field level 2)))
                    buf)))
    (should (equal '("Alpha" "Beta")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field level 1)) (unique)) buf)))))

(ert-deftest dorg-test-ql-plain-v2-traversal ()
  "Traversal walks the buffer's structural tree, deduped and in document order.
A plain-org file has no synthetic root, so a top-level heading's parent
resolves to nothing — matching the model/Dolt baseline."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha One" "Alpha Two")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 1)) (traverse children)) buf)))
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 2)) (traverse parent)) buf)))
    ;; Parent of a top-level heading is the absent root -> empty.
    (should (null (dorg-test-ql--v2-titles
                   '(pipe (where (id 1)) (traverse parent)) buf)))
    (should (equal '("Beta" "Beta One")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 6)) (traverse ancestors)) buf)))
    (should (equal '("Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 4)) (traverse descendants)) buf)))
    (should (equal '("Alpha Two")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 2)) (traverse siblings)) buf)))
    (should (equal '("Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (id 4)) (traverse self descendants)) buf)))))

(ert-deftest dorg-test-ql-plain-v2-traverse-dedup-shared-parent ()
  "Traversing to a shared parent yields that parent exactly once."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field parent-id 1)) (traverse parent))
                    buf)))))

(ert-deftest dorg-test-ql-plain-v2-timestamp-window ()
  "A v2 timestamp filter windows the entry's DEADLINE planning projection."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha")
                   (dorg-test-ql--v2-titles
                    '(timestamp deadline :overlaps (between "2026-06-01"
                                                            "2026-06-30"))
                    buf)))
    (should (null (dorg-test-ql--v2-titles
                   '(timestamp deadline :overlaps (between "2026-07-01"
                                                           "2026-07-31"))
                   buf)))))

(ert-deftest dorg-test-ql-plain-v2-negated-timestamp-is-sound ()
  "Plain-org negates a timestamp predicate soundly (exact item projection).
Unlike the Dolt planner's conservative scanners, plain-org projects full
parser-equivalent items, so `not' over a timestamp has a sound complement
and must NOT fail closed."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should (equal '("Alpha One" "Alpha Two" "Beta" "Beta One" "Beta One a")
                   (dorg-test-ql--v2-titles
                    '(not (deadline "2026-06-01" "2026-06-30")) buf)))))

(ert-deftest dorg-test-ql-plain-v2-due-this-week-high-priority-children ()
  "High-priority children of headings due this week (planning) compose cleanly.
Alpha is due 2026-06-03; with the week anchored there its only
priority >= B child is Alpha One."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      (should (equal '("Alpha One")
                     (dorg-test-ql--v2-titles
                      '(pipe (where (deadline (this-week)))
                             (traverse children)
                             (where (>= (field priority) "B")))
                      buf))))))

(ert-deftest dorg-test-ql-plain-v2-due-this-week-ancestors ()
  "Ancestors of headings due this week (planning) climb the real spine.
Alpha is due this week; being top-level its only ancestor (the absent
root) is excluded, so the ancestor set is empty."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      (should (null (dorg-test-ql--v2-titles
                     '(pipe (where (deadline (this-week)))
                            (traverse ancestors))
                     buf))))))

(defvar dorg-test-ql--v2-plain-due-doc
  "* Parent
:PROPERTIES:
:DORG_ID: 10
:END:
** Task
:PROPERTIES:
:DORG_ID: 11
:DUE: <2026-06-03 Wed>
:END:
*** [#A] Sub High
:PROPERTIES:
:DORG_ID: 12
:END:
*** [#C] Sub Low
:PROPERTIES:
:DORG_ID: 13
:END:
"
  "Plain-org text whose due date lives in a PROPERTIES `:DUE:' value.")

(ert-deftest dorg-test-ql-plain-v2-due-this-week-property-backed ()
  "Property-backed due dates compose like planning ones, both directions.
Task (id 11) carries `:DUE:' this week; its high-priority child is Sub High
and its ancestor is Parent."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-due-doc (:buffer buf)
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      ;; High-priority children of the property-due heading.
      (should (equal '("Sub High")
                     (dorg-test-ql--v2-titles
                      '(pipe (where (property-ts (this-week) "DUE"))
                             (traverse children)
                             (where (>= (field priority) "B")))
                      buf)))
      ;; Ancestors of the property-due heading climb to its parent.
      (should (equal '("Parent")
                     (dorg-test-ql--v2-titles
                      '(pipe (where (property-ts (this-week) "DUE"))
                             (traverse ancestors))
                      buf))))))

(ert-deftest dorg-test-ql-plain-v2-result-shape-and-live-markers ()
  "v2 results keep the four-key plist, carry a LIVE marker, and local props."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (let ((r (car (dorg-ql-select '(id 2) buf))))
      (should (equal '(:heading-id :marker :title :properties)
                     (cl-loop for (k _v) on r by #'cddr collect k)))
      (should (= 2 (plist-get r :heading-id)))
      (should (equal "Alpha One" (plist-get r :title)))
      ;; Plain-org v2 results carry a live marker, unlike model/Dolt.
      (should (markerp (plist-get r :marker)))
      ;; Local PROPERTIES only, DORG_ID excluded.
      (should (equal '(("FOO" . "bar")) (plist-get r :properties))))
    ;; A traversal result also carries a live marker into the buffer.
    (let ((r (car (dorg-ql-select
                   '(pipe (where (id 1)) (traverse children)) buf))))
      (should (markerp (plist-get r :marker)))
      (should (= 2 (plist-get r :heading-id))))))

(ert-deftest dorg-test-ql-plain-v2-matches-model-baseline ()
  "Plain-org v2 results equal the model evaluator over the SAME parsed text.
Proves convergence with the accepted reference semantics across booleans,
traversal, timestamps, and order/de-duplication."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (dolist (query '((pipe (where (field level 1)) (traverse descendants))
                     (and (>= (field priority) "B") (field level 2))
                     (or (tag "work") (tag "home"))
                     (keyword "EXPORT_FILE_NAME" "alpha-one")
                     (not (field priority "A"))
                     (pipe (where (id 4)) (traverse self descendants))
                     (contains (field title) "One")))
      (should (equal (dorg-test-ql--model-v2-titles query
                                                    dorg-test-ql--v2-plain-doc)
                     (dorg-test-ql--v2-titles query buf))))
    ;; Timestamp parity needs a deterministic reference week.
    (let ((dorg-ql-current-time (encode-time 0 0 12 3 6 2026)))
      (let ((query '(pipe (where (deadline (this-week))) (traverse children))))
        (should (equal (dorg-test-ql--model-v2-titles
                        query dorg-test-ql--v2-plain-doc)
                       (dorg-test-ql--v2-titles query buf)))))))

(ert-deftest dorg-test-ql-plain-v2-item-projection-counts-by-query-kind ()
  "Plain-org v2 projects items once per heading only when item leaves exist.
The fixture has six indexed headings.  Headline-only queries project no
items; timestamp-only, keyword-only, and mixed timestamp+keyword queries
must each call `dorg-parse-section-items-at-heading' exactly once per
heading, preserving their result semantics."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (dolist (case
             '(((pipe (where (field level 1)) (traverse children))
                ("Alpha One" "Alpha Two" "Beta One")
                0)
               ((deadline "2026-06-01" "2026-06-30")
                ("Alpha")
                6)
               ((missing (keyword "EXPORT_FILE_NAME"))
                ("Alpha" "Alpha Two" "Beta" "Beta One" "Beta One a")
                6)
               ((and (deadline "2026-06-01" "2026-06-30")
                     (missing (keyword "EXPORT_FILE_NAME")))
                ("Alpha")
                6)))
      (pcase-let ((`(,query ,expected-titles ,expected-count) case))
        (let ((actual (dorg-test-ql--plain-v2-titles-and-projection-count
                       query buf)))
          (should (equal expected-titles (car actual)))
          (should (= expected-count (cdr actual))))))))

(ert-deftest dorg-test-ql-plain-v2-fail-closed-malformed ()
  "Malformed or unsupported v2 shapes signal `dorg-ql-error', never widen."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (should-error (dorg-ql-select '(pipe (where (= (field bogus) 1))) buf)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select '(pipe (traverse nope)) buf)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select '(field priority bad-op "A") buf)
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-select '(timestamp deadline :bogus-rel (today)) buf)
                  :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-plain-v2-atomic-compat-unchanged ()
  "Atomic plain-org queries keep their existing path, shape, and live markers.
The v2 wiring must not regress the atomic dispatch: (id ...)/(title ...)/
(tag ...) still answer with the historical result shape."
  (dorg-test-ql--with-plain-org-file dorg-test-ql--v2-plain-doc (:buffer buf)
    (let ((r (car (dorg-ql-select '(id 1) buf))))
      (should (= 1 (plist-get r :heading-id)))
      (should (equal "Alpha" (plist-get r :title)))
      (should (markerp (plist-get r :marker))))
    (should (equal '("Alpha One")
                   (dorg-test-ql--titles
                    (dorg-ql-select '(property "FOO" "bar") buf))))
    (should (equal '("Alpha")
                   (dorg-test-ql--titles (dorg-ql-select '(tag "work") buf))))))

(ert-deftest dorg-test-ql-plain-v2-id-matches-real-dorg-id-not-surrogate ()
  "The `id' predicate matches the real DORG_ID, not an adapter-local ordinal.
Headings without a DORG_ID still take part in traversal/ordering but carry a
nil `:heading-id' and are never matched by an `id' predicate."
  (dorg-test-ql--with-plain-org-file
      "* No Id
* [#A] Has Id
:PROPERTIES:
:DORG_ID: 42
:END:
** Child No Id
"
      (:buffer buf)
    ;; id 42 matches the real DORG_ID (the 2nd heading), not ordinal 1 or 2.
    (should (equal '("Has Id")
                   (dorg-test-ql--v2-titles '(id 42) buf)))
    ;; id 1 (a plausible ordinal) matches nothing: no heading has DORG_ID 1.
    (should (null (dorg-test-ql--v2-titles '(id 1) buf)))
    ;; The no-id headings still appear in a source-all sweep, with nil ids.
    (should (equal '("No Id" "Has Id" "Child No Id")
                   (dorg-test-ql--v2-titles '(pipe (where (>= (field level) 1)))
                                            buf)))
    (should (equal '(nil 42 nil)
                   (dorg-test-ql--v2-ids '(pipe (where (>= (field level) 1)))
                                         buf)))
    ;; Traversal still works structurally across the id-less spine.
    (should (equal '("Child No Id")
                   (dorg-test-ql--v2-titles
                    '(pipe (where (field title "Has Id"))
                           (traverse children))
                    buf)))))

;;; ─── B5 Option A: dorg-registration as a dorg-ql-select SOURCE ─────────────
;; `dorg-ql-select' now accepts a `dorg-registration' — an internal, possibly
;; UNREGISTERED binding — dispatched via its own backend with no buffer/registry
;; lookup.  This is the seam that makes a no-buffer Dolt source queryable.

(ert-deftest dorg-test-ql-registration-source-atomic-and-v2 ()
  "A synthesized (unregistered) `dorg-registration' works as a `dorg-ql-select'
SOURCE for both an atomic (title) query and a composable v2 (pipe) query — each
returns the exact root-scoped Dolt row, with no buffer or `dorg--registry' lookup."
  (dorg-test-ql--with-dolt-synced
      "* Keep\n:PROPERTIES:\n:DORG_ID: 300\n:END:\n* Drop\n:PROPERTIES:\n:DORG_ID: 301\n:END:\n"
      (:reg reg)
    (let* ((backend (dorg-registration-backend reg))
           (src (list :dolt-dir (dorg-backend-dolt-dir backend)
                      :root-id (dorg-backend-root-id backend)))
           (qlreg (dorg-api--ql-source (dorg-mutate--resolve-source src))))
      (should (dorg-registration-p qlreg))
      ;; atomic
      (let ((r (dorg-ql-select '(title "Keep") qlreg)))
        (should (= 1 (length r)))
        (should (= 300 (plist-get (car r) :heading-id))))
      ;; composable v2 (filter-only pipeline) — same root-scoped result
      (let ((r (dorg-ql-select '(pipe (where (title "Keep"))) qlreg)))
        (should (= 1 (length r)))
        (should (= 300 (plist-get (car r) :heading-id)))))))

(ert-deftest dorg-test-ql-registration-key-policy-table ()
  "`(key V)' resolves against a `dorg-registration' SOURCE through the SAME single
resolver: `dorg' -> (id V); (:property K) -> (property K V); `none' -> `dorg-ql-error'
carrying reason unsupported / detail identity-none — never a fallback scan."
  (let* ((dorg--registry nil)
         (mk (lambda (identity)
               (make-dorg-registration
                :backend (dorg-backend-dolt "/tmp/x/.dolt/" 1)
                :identity identity))))
    (should (equal '(id "5")
                   (dorg-ql--resolve-key-refs '(key "5") (funcall mk 'dorg))))
    (should (equal '(property "BEAD_ID" "a-1")
                   (dorg-ql--resolve-key-refs '(key "a-1") (funcall mk '(:property "BEAD_ID")))))
    (let ((outcome (condition-case err
                       (progn (dorg-ql--resolve-key-refs '(key "a-1") (funcall mk 'none))
                              :no-error)
                     (dorg-ql-error
                      (let ((data (nth 2 err)))
                        (cons (plist-get data :reason) (plist-get data :detail)))))))
      (should (equal '(unsupported . identity-none) outcome)))))

;;; DORG_* protected keyword — QL READ-path exclusion (tracked-regression)
;;
;; BLOCKER 1 result-level control: SQL generation NEVER emits a protected
;; keyword row, so a SQL-gen assertion cannot prove the three dorg-ql-dolt
;; read filters exclude one.  These tests SEED a lowercase `dorg_id' row
;; DIRECTLY into keyword_items (bypassing SQL gen so the protected row
;; genuinely reaches the DB) and assert every read path drops it, while a
;; sibling ordinary keyword row on the same heading proves the seed landed
;; and the filters are specifically case-insensitively excluding DORG_*.

(ert-deftest dorg-test-ql-dolt-lowercase-protected-keyword-excluded-from-reads ()
  "A lowercase #+dorg_id: keyword_items row seeded directly into the DB is
excluded by all three Dolt read filters — the batch loader and both
`(keyword …)' selectors — while an ordinary sibling keyword is returned.
Reverting the batch seam to `(string-prefix-p \"DORG_\" key)' or dropping
`UPPER(ki.keyword)' from the selector SQL turns the corresponding
assertion RED (the seeded lowercase row would surface)."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Beta\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n"
      (:reg reg :dolt-dir dd)
    ;; Seed directly: a protected lowercase `dorg_id' and an ordinary
    ;; `notdorg' keyword item, both on heading 100.  High item ids and
    ;; sort_keys avoid colliding with the synced items.
    (dorg-sql-batch
     dd
     (list
      "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (999000001, 100, X'FE', 'keyword', 0)"
      "INSERT INTO keyword_items (item_id, keyword, value_raw) VALUES (999000001, 'dorg_id', 'leak')"
      "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (999000002, 100, X'FD', 'keyword', 0)"
      "INSERT INTO keyword_items (item_id, keyword, value_raw) VALUES (999000002, 'notdorg', 'visible')"))
    ;; (1) Batch loader: protected omitted, ordinary present (non-vacuity).
    (let* ((table (dorg-ql--dolt-keywords-batch dd (list 100)))
           (kws (gethash 100 table)))
      (should (assoc "notdorg" kws))
      (should-not (assoc "dorg_id" kws)))
    ;; (2) `(keyword KEY)' selector: protected returns nothing; the control
    ;;     ordinary keyword returns heading 100 (seed reached the DB).
    (should (null (dorg-ql--dolt-select '(keyword "dorg_id") reg)))
    (let ((hit (dorg-ql--dolt-select '(keyword "notdorg") reg)))
      (should (= 1 (length hit)))
      (should (= 100 (plist-get (car hit) :heading-id))))
    ;; (3) `(keyword KEY VALUE)' selector: protected excluded even with its
    ;;     exact value; the control ordinary keyword/value returns heading 100.
    (should (null (dorg-ql--dolt-select '(keyword "dorg_id" "leak") reg)))
    (let ((hit (dorg-ql--dolt-select '(keyword "notdorg" "visible") reg)))
      (should (= 1 (length hit)))
      (should (= 100 (plist-get (car hit) :heading-id))))))

;;; ─── Atomic-executor retirement (tracked-case) ──────────────────────────

;; These tests pin the retirement of the historical atomic executors onto
;; the single v2 AST path.  The atomic aliases keep answering (proved by the
;; existing model/plain/Dolt atomic corpus above, which routes through
;; `dorg-ql-select' and now flows through `dorg-ql-normalize'); these add the
;; structural and dispatch-contract assertions the collapse introduces.

(ert-deftest dorg-test-ql-retired-atomic-executors-are-gone ()
  "AC-1: every retired atomic executor/validator symbol is unbound after load.
Non-vacuous: all of these are `fboundp'/`boundp' before the retirement, so
the test can only pass once the deletions land."
  (dolist (sym '(;; executors + atomic matcher / buffer dispatch
                 dorg-ql--model-select
                 dorg-ql--model-match-p
                 dorg-ql--plain-org-select
                 dorg-ql--plain-org-predicate
                 dorg-ql--plain-org-timestamp-predicate
                 dorg-ql--collect-plain-org
                 dorg-ql--buffer-select
                 ;; atomic validator + its now-orphaned helpers
                 dorg-ql--validate
                 dorg-ql--validate-window
                 dorg-ql--validate-window-name
                 dorg-ql--timestamp-query-p))
    (should-not (fboundp sym)))
  ;; The validator's sole-use predicate-name constant is gone too.
  (should-not (boundp 'dorg-ql--predicates)))

(ert-deftest dorg-test-ql-model-by-keyword ()
  "AC-2: keyword presence and value atomic forms answer through the unified
v2 path (model list), local (heading-scoped) with input order preserved."
  (let ((headings
         (list (make-dorg-heading
                :id 1 :level 1 :level-offset 1 :title "Work"
                :items (list (dorg-keyword-item :keyword "CATEGORY"
                                                :value "work")))
               (make-dorg-heading
                :id 2 :level 1 :level-offset 1 :title "Home"
                :items (list (dorg-keyword-item :keyword "CATEGORY"
                                                :value "home")))
               (make-dorg-heading :id 3 :level 1 :level-offset 1 :title "None"))))
    ;; presence
    (should (equal '(1 2)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select '(keyword "CATEGORY") headings))))
    ;; exact value
    (should (equal '(1)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select '(keyword "CATEGORY" "work") headings))))
    (should (null (dorg-ql-select '(keyword "CATEGORY" "missing") headings)))))

(ert-deftest dorg-test-ql-plain-by-keyword ()
  "AC-3: keyword presence and value atomic forms answer through the unified
v2 path (plain-org buffer) with a live marker and heading-scoped matching."
  (dorg-test-ql--with-plain-org-file
      "* Exported
:PROPERTIES:
:DORG_ID: 1
:END:
#+EXPORT_FILE_NAME: exported
* Plain
:PROPERTIES:
:DORG_ID: 2
:END:
"
      (:buffer buf)
    (let ((r (car (dorg-ql-select '(keyword "EXPORT_FILE_NAME") buf))))
      (should (= 1 (plist-get r :heading-id)))
      (should (markerp (plist-get r :marker))))
    (should (equal '(1)
                   (dorg-test-ql--ids-in-order
                    (dorg-ql-select '(keyword "EXPORT_FILE_NAME" "exported") buf))))
    (should (null (dorg-ql-select '(keyword "EXPORT_FILE_NAME" "other") buf)))))

(ert-deftest dorg-test-ql-dolt-ast->point-form-recognizes-six-forms ()
  "AC-9 (translator half): the point-form recognizer maps exactly the six
single-filter equality/presence ASTs back to their atomic form and rejects
every other shape — ranges, negations, traversals, and all six timestamp
aliases (which must route to the general planner)."
  (require 'dorg-ql-dolt)
  ;; The six recognized point leaves round-trip to their atomic form.
  (should (equal '(id 5)
                 (dorg-ql--dolt-ast->point-form (dorg-ql-normalize '(id 5)))))
  (should (equal '(property "K" "V")
                 (dorg-ql--dolt-ast->point-form
                  (dorg-ql-normalize '(property "K" "V")))))
  (should (equal '(title "T")
                 (dorg-ql--dolt-ast->point-form (dorg-ql-normalize '(title "T")))))
  (should (equal '(tag "G")
                 (dorg-ql--dolt-ast->point-form (dorg-ql-normalize '(tag "G")))))
  (should (equal '(keyword "K")
                 (dorg-ql--dolt-ast->point-form (dorg-ql-normalize '(keyword "K")))))
  (should (equal '(keyword "K" "V")
                 (dorg-ql--dolt-ast->point-form
                  (dorg-ql-normalize '(keyword "K" "V")))))
  ;; Timestamp aliases are never point forms.
  (dolist (q '((scheduled "2026-01-01" "2026-12-31")
               (deadline "2026-01-01" "2026-12-31")
               (closed "2026-01-01" "2026-12-31")
               (active-ts "2026-01-01" "2026-12-31")
               (logbook "2026-01-01" "2026-12-31")
               (property-ts "2026-01-01" "2026-12-31" "DUE")))
    (should (null (dorg-ql--dolt-ast->point-form (dorg-ql-normalize q)))))
  ;; Non-equality / composite shapes are not point forms.
  (dolist (q '((id >= 5)
               (contains (property "K") "v")
               (pipe (where (id 5)) (traverse children))
               (and (id 5) (tag "G"))
               (not (id 5))))
    (should (null (dorg-ql--dolt-ast->point-form (dorg-ql-normalize q))))))

(ert-deftest dorg-test-ql-dolt-point-fast-path-taken ()
  "AC-4: the six exact point-lookup forms answer via the bounded-SQL fast
path and never load the headline adjacency map, whereas a compound query
takes the general planner (which does).  Deleting the fast-path branch makes
the point-lookup load counter non-zero (RED)."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (cl-flet ((run (query)
                (let ((n 0)
                      (orig (symbol-function 'dorg-model-load-headings)))
                  (cl-letf (((symbol-function 'dorg-model-load-headings)
                             (lambda (&rest a) (setq n (1+ n)) (apply orig a))))
                    (cons (dorg-test-ql--titles (dorg-ql-select query buf)) n)))))
      (dolist (case '(((id 2) "Alpha One")
                      ((property "FOO" "bar") "Alpha One")
                      ((tag "work") "Alpha")
                      ((title "Beta") "Beta")
                      ((keyword "EXPORT_FILE_NAME" "alpha-one") "Alpha One")
                      ((keyword "EXPORT_FILE_NAME") "Alpha One")))
        (let ((r (run (car case))))
          (should (equal (cdr case) (car r)))
          ;; Fast path: no adjacency load.
          (should (= 0 (cdr r)))))
      ;; A compound (multi-filter) query is not a point form -> general planner.
      (let ((r (run '(pipe (where (and (tag "work") (field level 1)))))))
        (should (equal '("Alpha") (car r)))
        (should (= 1 (cdr r)))))))

(ert-deftest dorg-test-xeq5v-insensitive-point-residual-route ()
  "An insensitive point-shaped leaf preserves mode and bypasses point SQL.
This names the existing sensitive bounded-cost control
`dorg-test-ql-dolt-point-fast-path-taken': it continues to prove the
sensitive point route has zero headline-map loads."
  (dorg-test-ql--with-dolt-synced
      "* Alpha
:PROPERTIES:
:DORG_ID: 101
:END:
* alpha
:PROPERTIES:
:DORG_ID: 102
:END:
"
      (:buffer buf)
    (let ((point-calls 0)
          (general-calls 0)
          (point (symbol-function 'dorg-ql--dolt-select))
          (general (symbol-function 'dorg-ql--dolt-eval-general)))
      (cl-letf (((symbol-function 'dorg-ql--dolt-select)
                 (lambda (&rest args)
                   (cl-incf point-calls)
                   (apply point args)))
                ((symbol-function 'dorg-ql--dolt-eval-general)
                 (lambda (ast &rest args)
                   (cl-incf general-calls)
                   (should (equal ast
                                  '(:pipe (:source :all)
                                          (:filter (:cmp title = "ALPHA"
                                                         :case insensitive)))))
                   (should-not (dorg-ql--dolt-ast->point-form ast))
                   (apply general ast args))))
        (should (equal '(101 102)
                       (dorg-test-ql--ids
                        (dorg-ql-select '(title "ALPHA" :case insensitive) buf))))
        (should (= 0 point-calls))
        (should (= 1 general-calls))))))

(ert-deftest dorg-test-xeq5v-case-refusals-precede-source-access ()
  "Every malformed/inapplicable case tail refuses before loader or SQL use."
  (with-temp-buffer
    (let ((source-calls 0)
          (sql-calls 0))
      (cl-letf (((symbol-function 'dorg-ql--registration)
                 (lambda (&rest _)
                   (cl-incf source-calls)
                   (error "source access must not occur")))
                ((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest _)
                   (cl-incf sql-calls)
                   (error "SQL access must not occur"))))
        (dolist (query '((title "x" :case folded)
                         (title "x" :case insensitive :case sensitive)
                         (field id 1 :case insensitive)
                         (field priority "A" :case insensitive)
                         (exists (property "K") :case insensitive)))
          (should-error (dorg-ql-select query (current-buffer))
                        :type 'dorg-ql-error))
        (should (= 0 source-calls))
        (should (= 0 sql-calls))))))

(ert-deftest dorg-test-xeq5v-case-parity-plain-model-dolt ()
  "Every case row has literal ids, then agrees across all three backends.

The rows intentionally span title, TODO, property values, keyword values,
and tags.  Their expected ids are the
oracle: backend agreement alone would not detect a shared wrong matcher."
  (let ((contents
         "* TODO Work :MixEd:work:\n:PROPERTIES:\n:DORG_ID: 101\n:COLOR: RedApple\n:END:\n#+CAPTION: Draft-One\n* DONE work :mixed:other:\n:PROPERTIES:\n:DORG_ID: 102\n:COLOR: redapple\n:END:\n#+CAPTION: draft-two\n* TODO Other :MiXeD:\n:PROPERTIES:\n:DORG_ID: 103\n:COLOR: Blue\n:END:\n#+CAPTION: Note\n* DONE Missing\n:PROPERTIES:\n:DORG_ID: 104\n:END:\n"))
    (let ((dorg--extensions nil)
          (rows '(((title "Work") (101))
                  ((title "WORK" :case insensitive) (101 102))
                  ((field todo "todo" :case insensitive) (101 103))
                  ((!= (field todo) "todo" :case insensitive) (102 104))
                  ((contains (property "COLOR") "APPLE" :case insensitive)
                   (101 102))
                  ((prefix (keyword "CAPTION") "DRAFT" :case insensitive)
                   (101 102))
                  ((suffix (tag) "XED" :case insensitive) (101 102 103))
                  ((!=(tag) "MIXED" :case insensitive) (104)))))
      (let ((model (dorg-test-ql--model-from-org contents)))
        ;; The parser model intentionally has no store identity; supply the
        ;; same fixture identities used by the plain/Dolt document.
        (cl-loop for heading in model for id from 101
                 do (setf (dorg-heading-id heading) id))
        (dolist (row rows)
          (pcase-let ((`(,query ,expected) row))
            (should (equal expected
                           (dorg-test-ql--ids (dorg-ql-select query model)))))))
      (dorg-test-ql--with-plain-org-file contents (:buffer pbuf)
        (dolist (row rows)
          (pcase-let ((`(,query ,expected) row))
            (should (equal expected
                           (dorg-test-ql--ids (dorg-ql-select query pbuf)))))))
      (dorg-test-ql--with-dolt-synced contents (:buffer dbuf)
        (dolist (row rows)
          (pcase-let ((`(,query ,expected) row))
            (should (equal expected
                           (dorg-test-ql--ids (dorg-ql-select query dbuf))))))))))

(ert-deftest dorg-test-xeq5v-case-route-audit-expected-ids ()
  "Point, pushed compound, and residual forms retain each mode's ids.

The literal ids make this a route-equivalence audit rather than a mere
comparison of two backend outputs.  The insensitive forms deliberately use
the residual plan; the sensitive point form remains covered by the named
bounded-cost control `dorg-test-ql-dolt-point-fast-path-taken'."
  (dorg-test-ql--with-dolt-synced
      "* Work :keep:\n:PROPERTIES:\n:DORG_ID: 101\n:COLOR: Apple\n:END:\n* work :keep:\n:PROPERTIES:\n:DORG_ID: 102\n:COLOR: apple\n:END:\n* Other\n:PROPERTIES:\n:DORG_ID: 103\n:COLOR: Blue\n:END:\n"
      (:buffer buf)
    (dolist (row
             '(((title "Work") (101))
               ((and (title "Work") (field level 1)) (101))
               ((and (title "Work") (contains (property "COLOR") "pp")) (101))
               ((title "WORK" :case insensitive) (101 102))
               ((and (title "WORK" :case insensitive) (field level 1)) (101 102))
               ((and (title "WORK" :case insensitive) (tag "KEEP" :case insensitive))
                (101 102))))
      (pcase-let ((`(,query ,expected) row))
        (should (equal expected
                       (dorg-test-ql--ids (dorg-ql-select query buf))))))))

(ert-deftest dorg-test-xeq5v-insensitive-residual-cost-3000 ()
  "The 3000-heading insensitive title read loads no heading models or bodies.

An insensitive title leaf is evaluated over a root-scoped id/title scan, so
neither the headline adjacency map nor any body table is read.  This is
deliberately a structural cost control, not a duration threshold.  The
elapsed value is emitted in the test result for the acceptance receipt."
  (let ((contents (mapconcat
                   (lambda (id)
                     (format "* %s%04d :tag%04d:\n:PROPERTIES:\n:DORG_ID: %d\n:END:\n"
                             (if (= id 1731) "Needle" "Heading") id id id))
                   (number-sequence 1 3000) "")))
    (dorg-test-ql--with-dolt-synced contents (:buffer buf)
      (let ((body-statements 0)
            (headline-only-loads 0)
            (started (float-time))
            (orig-live (symbol-function 'dorg-sql))
            (orig-pinned (symbol-function 'dorg-sql--at-revision))
            (orig-load (symbol-function 'dorg-model-load-headings)))
        (cl-labels ((body-sql-p (statement)
                      (string-match-p
                       "text_items\\|planning_items\\|drawer_lines\\|keyword_items"
                       statement)))
          (cl-letf (((symbol-function 'dorg-sql)
                     (lambda (dolt-dir query &rest args)
                       (let ((statement (if args (apply #'format query args) query)))
                         (when (body-sql-p statement) (cl-incf body-statements)))
                       (apply orig-live dolt-dir query args)))
                    ((symbol-function 'dorg-sql--at-revision)
                     (lambda (dolt-dir revision query &rest args)
                       (let ((statement (if args (apply #'format query args) query)))
                         (when (body-sql-p statement) (cl-incf body-statements)))
                       (apply orig-pinned dolt-dir revision query args)))
                    ((symbol-function 'dorg-model-load-headings)
                     (lambda (dolt-dir root-id ids
                              &optional as-of headline-only query-fn levels)
                       (should headline-only)
                       (should (functionp query-fn))
                       (cl-incf headline-only-loads)
                       (funcall orig-load dolt-dir root-id ids as-of headline-only
                                query-fn levels))))
            (should (equal '(1731)
                           (dorg-test-ql--ids
                            (dorg-ql-select
                             '(title "needle1731" :case insensitive) buf))))
            ;; A headings-row leaf beside the insensitive one is a scan flag.
            (should (equal '(1731)
                           (dorg-test-ql--ids
                            (dorg-ql-select
                             '(and (contains (field title) "NEEDLE" :case insensitive)
                                   (not (field todo "DONE")))
                             buf))))
            (message "xeq5v 3000-heading residual fixture elapsed=%.6fs"
                     (- (float-time) started))
            (should (= 0 body-statements))
            (should (= 0 headline-only-loads))))))))

(ert-deftest dorg-test-ql-dolt-timestamp-uses-general-planner ()
  "AC-9 (execution half): a Dolt timestamp alias answers correctly AND loads
the adjacency/scanner path (the mirror of AC-4's point-form bypass)."
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:buffer buf)
    (let ((n 0)
          (orig (symbol-function 'dorg-model-load-headings)))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (&rest a) (setq n (1+ n)) (apply orig a))))
        ;; Alpha carries DEADLINE <2026-06-03>.
        (should (equal '("Alpha")
                       (dorg-test-ql--titles
                        (dorg-ql-select '(deadline "2026-06-01" "2026-06-30")
                                        buf))))
        (should (= 1 n))))))

(ert-deftest dorg-test-ql-dolt-rows-catch-all-signals ()
  "AC-7: `dorg-ql--dolt-rows' signals `dorg-ql-error' for any non-point form
— an unknown head AND each of the four now-deleted timestamp arms — instead
of silently returning nil (unknown) or scanning rows (timestamps)."
  (require 'dorg-ql-dolt)
  (dorg-test-ql--with-dolt-synced dorg-test-ql--v2-doc (:reg reg)
    (should-error (dorg-ql--dolt-rows '(bogus "x") reg) :type 'dorg-ql-error)
    (dolist (q '((scheduled (2026 1 1) (2026 12 31))
                 (property-ts (2026 1 1) (2026 12 31) nil)
                 (logbook (2026 1 1) (2026 12 31))
                 (active-ts (2026 1 1) (2026 12 31))))
      (should-error (dorg-ql--dolt-rows q reg) :type 'dorg-ql-error))))

(ert-deftest dorg-test-ql-unknown-backend-typed-error ()
  "AC-10 (ratified): a registration whose effective backend is neither
plain-org nor dolt signals the typed `dorg-ql-error' for BOTH an atomic and
a v2 form — the single fail-closed contract after the collapse.  Pre-refactor
the atomic form yielded a generic `error' via `--buffer-select', so asserting
`dorg-ql-error' on it is RED before the change."
  (let ((reg (make-dorg-registration
              :backend (dorg-backend--make :type 'mystery
                                           :options nil))))
    (dolist (query '((id 1) (pipe (where (field level 1)))))
      (let ((err (should-error (dorg-ql-select query reg)
                               :type 'dorg-ql-error)))
        (should (eq 'unsupported (dorg-error-reason err)))))))

;;; ─── Declared extension membership pushdown ──────────────────────────────

(defconst dorg-test-85e9w--membership-tables
  '((:name "assignment_rows"
     :key-column "subject_id"
     :scope-column "subject_id"
     :memberships
     ((:name assignee-at
       :member-key "subject_id"
       :position-column "ordinal"
       :value-column "assignee"
       :bridge-table "subjects"
       :bridge-key "subject_id"
       :heading-column "heading_id"
       :root-column "root_id")))
    (:name "subjects" :key-column "subject_id" :scope-column "root_id"))
  "Literal membership declaration for the planner REDs.")

(defmacro dorg-test-85e9w--with-membership-extension (&rest body)
  "Run BODY with only the fixture-free demo membership extension registered."
  (declare (indent 0))
  `(let ((dorg--extensions nil))
     (dorg-register-extension
      'demo :schema "SELECT 1" :version 1
      :read-tables dorg-test-85e9w--membership-tables)
     ,@body))

(ert-deftest dorg-test-85e9w-composed-assignment-filter-is-fully-pushable ()
  "Member, status, property-existence and booleans lower as one SQL filter."
  (require 'dorg-ql-dolt)
  (dorg-test-85e9w--with-membership-extension
    (let* ((query
            '(and
              (field todo "IMPL")
              (missing (property "EMBEAD_STATIC"))
              (missing (property "BEAD_ASSIGNABLE"))
              (missing (property "EMBEAD_FORM"))
              (missing (property "EMBEAD_WORKFLOW_DEF"))
              (or
               (extension-member demo assignee-at 1 ("alice"))
               (and
                (field todo "WAIT")
                (extension-member demo assignee-at 1 (""))
                (extension-member demo assignee-at 2 ("alice"))))))
           (expr (cadr (nth 2 (dorg-ql-normalize query))))
           (sql (dorg-ql--dolt-pushdown-select-sql expr 17)))
      (should (dorg-ql--dolt-pushable-filter-p expr))
      (should (string-match-p "WITH RECURSIVE" sql))
      (should (string-match-p "CAST(HEX(sort_key) AS CHAR(4096))" sql))
      (should (string-match-p "ORDER BY BINARY q.path" sql))
      (should (string-match-p "assignment_rows" sql))
      (should (string-match-p "subjects" sql))
      (should (string-match-p "root_id = 17" sql))
      (should (string-match-p "todo_keyword" sql))
      (should (string-match-p "heading_properties" sql))
      (should (string-match-p "ordinal = 1" sql))
      (should (string-match-p "ordinal = 2" sql))
      ;; Explicit lower-then-binary comparison: no inherited ext3 collation.
      (should (string-match-p "BINARY LOWER" sql))
      (should (string-match-p "'alice'" sql))
      (should (string-match-p "''" sql)))))

(ert-deftest dorg-test-85e9w-unregistered-membership-refuses-before-sql ()
  "Unknown extension/relation never degrades to an empty or broad selection."
  (require 'dorg-ql-dolt)
  (let ((dorg--extensions nil)
        (sql-calls 0))
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (&rest _) (cl-incf sql-calls) nil)))
      (should-error
       (dorg-ql--dolt-pushdown-select-sql
        '(:extension-member absent assignee-at 1 ("alice")) 17)
       :type 'dorg-ql-error)
      (should (= 0 sql-calls)))))

(ert-deftest dorg-test-85e9w-public-unregistered-membership-refuses-before-head ()
  "The public boundary rejects an unknown declaration before snapshot SQL."
  (let* ((dorg--extensions nil)
         (backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         (head-calls 0))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (&rest _) (cl-incf head-calls) "unexpected")))
      (should-error
       (dorg-ql-select
        '(extension-member absent assignee-at 1 ("alice")) reg)
       :type 'dorg-ql-error)
      (should (= 0 head-calls)))))

(ert-deftest dorg-test-85e9w-unsupported-filter-keeps-residual-classification ()
  "An unsupported leaf rejects all-or-pushdown and stays on the old evaluator."
  (require 'dorg-ql-dolt)
  (dorg-test-85e9w--with-membership-extension
    (should-not
     (dorg-ql--dolt-pushable-filter-p
      '(:and (:extension-member demo assignee-at 1 ("alice"))
             (:tag contains "work"))))))

(ert-deftest dorg-test-85e9w-negated-todo-is-two-valued-for-keywordless-heading ()
  "SQL pushdown preserves residual truth for nullable TODO under NOT."
  (require 'dorg-ql-dolt)
  (let ((heading (make-dorg-heading :id 9 :title "No keyword")))
    (dolist (expr '((:not (:cmp todo = "DONE" :case sensitive))
                    (:not (:cmp todo != "DONE" :case sensitive))))
      ;; Residual/model truth is the contract: a nil TODO never matches the
      ;; leaf, so negating either leaf includes the keyword-less heading.
      (should (dorg-ql--expr-match-p expr heading))
      (let ((sql (dorg-ql--dolt-filter-sql expr 17)))
        (should (string-match-p "COALESCE" sql))
        (should (string-match-p "FALSE" sql))))))

(ert-deftest dorg-test-85e9w-registered-membership-with-residual-refuses-before-head ()
  "A registered extension leaf cannot fall into the residual evaluator."
  (let* ((backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         (head-calls 0))
    (dorg-test-85e9w--with-membership-extension
      (cl-letf (((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) (cl-incf head-calls) "unexpected"))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (&rest _) (ert-fail "hydrated before refusal"))))
        (let ((err
               (should-error
                (dorg-ql-select
                 '(and
                   (extension-member demo assignee-at 1 ("nav"))
                   (deadline nil nil))
                 reg)
                :type 'dorg-ql-error)))
          (should (= 0 head-calls))
          (should (string-match-p
                   "cannot compose with residual"
                   (error-message-string err)))
          (should (string-match-p
                   ":timestamp"
                   (error-message-string err))))))))

(ert-deftest dorg-test-85e9w-revision-query-uses-only-pinned-chokepoint ()
  "A captured revision reaches the existing revision-bound SQL chokepoint."
  (require 'dorg-ql-dolt)
  (let (seen)
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (&rest _) (ert-fail "used live-head dorg-sql")))
              ((symbol-function 'dorg-sql--at-revision)
               (lambda (dir revision sql &rest args)
                 (setq seen (list dir revision (apply #'format sql args)))
                 '((ok . 1)))))
      (should (equal '((ok . 1))
                     (dorg-ql--dolt-query "/tmp/demo" "abc123" "SELECT %d" 1)))
      (should (equal '("/tmp/demo" "abc123" "SELECT 1") seen)))))

(ert-deftest dorg-test-85e9w-rows-and-snapshot-entry-share-one-dolt-pipeline ()
  "Rows-only and snapshot entry points differ only in returned metadata."
  (let* ((backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         (rows '((:heading-id 9 :marker nil :title "Selected" :properties nil)))
         revisions)
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "abc123"))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (_ast _reg revision)
                 (push revision revisions)
                 rows)))
      (should (equal rows (dorg-ql-select '(id 9) reg)))
      (let ((snapshot (dorg-ql-select-snapshot '(id 9) reg)))
        (should (equal rows (plist-get snapshot :rows)))
        (should (equal '(:kind dolt :head "abc123")
                       (plist-get snapshot :snapshot)))
        (should (= 17 (plist-get snapshot :root)))
        (should (equal 'dolt (plist-get (plist-get snapshot :source) :kind)))
        (should (equal (file-truename "/tmp/demo/")
                       (plist-get (plist-get snapshot :source) :store))))
      (should (equal '("abc123" "abc123") revisions)))))

(ert-deftest dorg-test-query-source-nil-root-resolves-at-captured-head ()
  "A classified nil-root Dolt source resolves once and snapshots that root."
  (let* ((backend (dorg-backend-dolt "/tmp/query-source-store" nil))
         (reg (make-dorg-registration :file "/tmp/query-source.org" :backend backend))
         (root-calls 0)
         seen)
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (dir file revision)
                 (cl-incf root-calls)
                 (should (equal revision "captured"))
                 (should (equal file "/tmp/query-source.org"))
                 (should (equal dir (dorg-backend-dolt-dir backend)))
                 47))
              ((symbol-function 'dorg-ql--make-invocation)
               (lambda (source revision)
                 (setq seen (list source revision))
                 (list :source source :revision revision :root
                       (dorg-backend-root-id
                        (dorg-registration-backend source)))))
              ((symbol-function 'dorg-ql--resolve-extension-contexts)
               (lambda (&rest _) '(:contexts nil :digest "fixture")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (_ast source revision)
                 (should (= 47 (dorg-backend-root-id
                                (dorg-registration-backend source))))
                 (should (equal revision "captured"))
                 '((:heading-id 47 :marker nil :title "Resolved" :properties nil)))))
      (let ((snapshot (dorg-ql-select-snapshot '(id 47) reg)))
        (should (= 1 root-calls))
        (should (= 47 (dorg-backend-root-id
                       (dorg-registration-backend (car seen)))))
        (should (equal "captured" (cadr seen)))
        (should (= 47 (plist-get snapshot :root)))
        (should (equal (file-truename "/tmp/query-source.org")
                       (plist-get (plist-get snapshot :source) :file)))))))

(ert-deftest dorg-test-query-source-rooted-source-skips-readonly-lookup ()
  "A supplied positive root remains authoritative at the captured revision."
  (let* ((backend (dorg-backend-dolt "/tmp/query-source-store" 71))
         (reg (make-dorg-registration :file "/tmp/query-source.org" :backend backend)))
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (&rest _) (ert-fail "looked up positive root")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (_ast source _revision)
                 (should (= 71 (dorg-backend-root-id
                                (dorg-registration-backend source))))
                 nil)))
      (let ((snapshot (dorg-ql-select-snapshot '(id 71) reg)))
        (should (= 71 (plist-get snapshot :root)))))))

(ert-deftest dorg-test-query-source-nil-root-refuses-before-evaluation ()
  "A nil root at H is a typed bind recovery failure, never empty rows."
  (let* ((backend (dorg-backend-dolt "/tmp/query-source-store" nil))
         (reg (make-dorg-registration :file "/tmp/query-source.org" :backend backend))
         (eval-calls 0))
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
              ((symbol-function 'dorg-repo-root-id) (lambda (&rest _) nil))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "query bound a missing root")))
              ((symbol-function 'dorg--ensure-registration)
               (lambda (&rest _) (ert-fail "query ensured a missing root")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "query initialized a missing root")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "query created a missing root")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "query allocated a missing root")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (&rest _) (cl-incf eval-calls))))
      (let ((err (should-error (dorg-ql-select '(id 47) reg)
                               :type 'dorg-binding-error)))
        (should (eq 'missing-root (dorg-binding-error-reason err)))
        (should (equal "/tmp/query-source-store/"
                       (plist-get (dorg-binding-error-data err) :dolt-dir)))
        (should (string-match-p "dorg-bind-dolt" (error-message-string err)))
        (should-not (string-match-p (concat "em" "bd dolt")
                                    (error-message-string err))))
      (should (= 0 eval-calls)))))

(ert-deftest dorg-test-query-source-prepared-invocation-survives-root-resolution ()
  "A warm session's captured invocation is not rebuilt while its root resolves."
  (let* ((reg (make-dorg-registration
               :file "/tmp/query-source-session.org"
               :backend (dorg-backend-dolt "/tmp/query-source-store" nil)))
         (invocation '(:session-invocation t :revision "session-h"))
         (prepared (list :ast (dorg-ql-normalize '(id 47))
                         :revision "session-h" :invocation invocation))
         seen)
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (&rest _) (ert-fail "warm session recaptured HEAD")))
              ((symbol-function 'dorg-repo-root-id)
               (lambda (_dir _file revision)
                 (should (equal "session-h" revision))
                 47))
              ((symbol-function 'dorg-ql--resolve-extension-contexts)
               (lambda (_ast actual-invocation)
                 (setq seen actual-invocation)
                 '(:contexts nil :digest "session")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (_ast resolved revision)
                 (should (= 47 (dorg-backend-root-id
                                (dorg-registration-backend resolved))))
                 (should (equal "session-h" revision))
                 nil)))
      (should-not (dorg-ql-select '(id 47) reg prepared))
      (should (eq invocation seen)))))

(ert-deftest dorg-test-query-source-file-registration-composes-through-real-ql ()
  "A classified file flows through H/root resolution and the real SQL planner."
  (require 'dorg-repo)
  (require 'dorg-ql-dolt)
  (let ((dorg--registry nil)
        (temp-base (make-temp-file "query-source-compose-" t)))
    (unwind-protect
        (let* ((org-file (expand-file-name "doc.org" temp-base))
               (dolt-dir (expand-file-name "store" temp-base))
               relative)
          (make-directory dolt-dir)
          (with-temp-file org-file
            (insert "#+DORG_DOLT_DIR: store\n* Queryable\n"))
          (setq relative (dorg-repo-encode-relative-path dolt-dir org-file))
          (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
                    ((symbol-function 'dorg-bind-dolt)
                     (lambda (&rest _) (ert-fail "query bound file source")))
                    ((symbol-function 'dorg--ensure-registration)
                     (lambda (&rest _) (ert-fail "query ensured file source")))
                    ;; The registration resolver and planner are real.  The
                    ;; two SQL transport entries alone are replaced here.
                    ((symbol-function 'dorg-sql)
                     (lambda (_dir sql &rest _)
                       (should (string-match-p "dorg_files AS OF 'captured'" sql))
                       `(((root_id . 47) (relative_path . ,relative)
                          (heading_id . 47) (heading_root_id . 47)
                          (parent_id . nil) (level_offset . 0)))))
                    ((symbol-function 'dorg-sql--at-revision)
                     (lambda (_dir revision sql &rest _)
                       (should (equal "captured" revision))
                       (if (string-match-p "FROM headings" sql)
                           '(((id . 47) (title . "Queryable")))
                         nil))))
            (let ((snapshot (dorg-ql-select-snapshot
                             '(id 47) (dorg-current-registration org-file))))
              (should (= 47 (plist-get snapshot :root)))
              (should (equal "Queryable"
                             (plist-get (car (plist-get snapshot :rows)) :title))))))
      (delete-directory temp-base t))))

(ert-deftest dorg-test-query-source-root-query-error-never-falls-back-to-rows ()
  "A missing historical table remains its raw read failure, before evaluation."
  (require 'dorg-repo)
  (require 'dorg-ql-dolt)
  (let* ((reg (make-dorg-registration
               :file "/tmp/query-source-absent-table.org"
               :backend (dorg-backend-dolt "/tmp/query-source-store" nil)))
         (eval-calls 0))
    (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "captured"))
              ((symbol-function 'dorg-bind-dolt)
               (lambda (&rest _) (ert-fail "raw resolver error bound a source")))
              ((symbol-function 'dorg--ensure-registration)
               (lambda (&rest _) (ert-fail "raw resolver error ensured a source")))
              ((symbol-function 'dorg-init)
               (lambda (&rest _) (ert-fail "raw resolver error initialized a store")))
              ((symbol-function 'dorg-repo-ensure-root)
               (lambda (&rest _) (ert-fail "raw resolver error created a root")))
              ((symbol-function 'dorg--create-root)
               (lambda (&rest _) (ert-fail "raw resolver error allocated a root")))
              ;; `dorg-repo-root-id' is real.  This is the server boundary
              ;; produced by a store without dorg_files at the captured head.
              ((symbol-function 'dorg-sql)
               (lambda (&rest _) (error "Table 'dorg_files' doesn't exist")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (&rest _) (cl-incf eval-calls))))
      (let ((err (should-error (dorg-ql-select '(id 47) reg))))
        ;; If root resolution is wrapped in `ignore-errors', the seam changes
        ;; this into its typed missing-root refusal.  Preserve the original
        ;; read condition and message instead.
        (should-not (eq 'dorg-binding-error (car err)))
        (should (string-match-p "dorg_files" (error-message-string err))))
      (should (= 0 eval-calls)))))

(ert-deftest dorg-test-85e9w-unreadable-head-refuses-before-dolt-eval ()
  "A Dolt query whose committed token cannot be captured issues no data read."
  (let* ((backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         (eval-calls 0))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (_) (error "unreadable head")))
              ((symbol-function 'dorg-ql--dolt-eval)
               (lambda (&rest _) (cl-incf eval-calls))))
      (should-error (dorg-ql-select '(id 9) reg))
      (should (= 0 eval-calls)))))

(ert-deftest dorg-test-85e9w-public-membership-read-is-pinned-and-never-hydrates-root ()
  "The public assignment filter selects ids in SQL without whole-root loading."
  (let* ((backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         calls)
    (dorg-test-85e9w--with-membership-extension
      (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "abc123"))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (&rest _) (ert-fail "hydrated the whole root")))
                ((symbol-function 'dorg-sql)
                 (lambda (&rest _) (ert-fail "used live-head dorg-sql")))
                ((symbol-function 'dorg-sql--at-revision)
                 (lambda (_dir revision sql &rest args)
                   (let ((statement (apply #'format sql args)))
                     (push (cons revision statement) calls)
                     (cond
                      ((string-match-p "WITH RECURSIVE" statement)
                       '(((id . 31) (title . "Assigned"))))
                      ((string-match-p "FROM heading_properties" statement) nil)
                      (t (ert-fail "unexpected pinned SQL: %s" statement)))))))
        (should
         (equal '((:heading-id 31 :marker nil :title "Assigned" :properties nil))
                (dorg-ql-select
                 '(and
                   (field todo "IMPL")
                   (missing (property "EMBEAD_STATIC"))
                   (extension-member demo assignee-at 1 ("alice")))
                 reg)))
        (should (= 2 (length calls)))
        (should (cl-every (lambda (entry) (equal "abc123" (car entry))) calls))
        (should (= 1 (cl-count-if
                      (lambda (entry)
                        (string-match-p "WITH RECURSIVE" (cdr entry)))
                      calls)))
        (should (cl-some
                 (lambda (entry)
                   (string-match-p "heading_id IN (31)" (cdr entry)))
                 calls))))))

(ert-deftest dorg-test-0l5rj-lease-data-shape-and-unlabelled-failure ()
  "The annotation is ADDITIVE, and absent outside an annotated statement.
The per-kind positive is the next control.  This one pins the two halves it
cannot: that a lease failure raised outside any annotated statement carries
NO `:statement-kind' at all, and that annotating never disturbs the data a
consumer already read -- the historical message stays first and
`:cause-class' stays immediately after it.

This replaces a control that observed the old dynamic BINDING.  Reading the
binding could only ever prove the variable was let-bound; it could not prove
the failure a caller actually receives is labelled, which is the contract."
  (require 'dorg-ql-dolt)
  (let ((lease (dorg-sql--make-lease :dolt-dir "/private/store"
                                     :conn 'fixture-connection
                                     :port 3307
                                     :phase :active)))
    ;; (a) Outside any annotated statement: message, :cause-class, nothing else.
    (let* ((err (condition-case caught
                    (progn (dorg-sql--raise-lease-broken
                            lease '(mysql-timeout "private transport prose"))
                           (ert-fail "no lease failure raised"))
                  (dorg-sql-lease-broken caught)))
           (data (cdr err)))
      (should (stringp (nth 0 data)))
      (should (eq :cause-class (nth 1 data)))
      (should (eq 'mysql-timeout (nth 2 data)))
      (should-not (plist-member (cdr data) :statement-kind))
      (should (= 3 (length data))))
    ;; (b) Inside one: the same prefix, with the label appended after it.
    (let* ((err (condition-case caught
                    (progn
                      (dorg-ql--dolt-with-statement-kind pushdown-select
                        (dorg-sql--raise-lease-broken
                         lease '(mysql-timeout "private transport prose")))
                      (ert-fail "no lease failure raised"))
                  (dorg-sql-lease-broken caught)))
           (data (cdr err)))
      (should (stringp (nth 0 data)))
      (should (eq :cause-class (nth 1 data)))
      (should (eq 'mysql-timeout (nth 2 data)))
      (should (eq 'pushdown-select (plist-get (cdr data) :statement-kind)))
      (should (= 5 (length data))))
    ;; (c) An error that is not a broken lease passes through untouched.
    (should-error
     (dorg-ql--dolt-with-statement-kind pushdown-select
       (error "unrelated failure"))
     :type 'error)))

(ert-deftest dorg-test-0l5rj-selector-kind-reaches-broken-condition-data ()
  "Each selector binding reaches the emitted lease-broken condition data."
  (require 'dorg-ql-dolt)
  (let* ((backend (dorg-backend-dolt "/tmp/demo" 17))
         (reg (make-dorg-registration :backend backend))
         (lease (dorg-sql--make-lease :dolt-dir "/private/store"
                                       :conn 'fixture-connection
                                       :port 3307
                                       :phase :active)))
    (cl-labels
        ((raise-broken ()
           (dorg-sql--raise-lease-broken
            lease '(mysql-timeout "private transport prose")))
         (catch-kind (thunk)
           (let ((err (condition-case caught
                          (progn (funcall thunk)
                                 (ert-fail "selector did not raise"))
                        (dorg-sql-lease-broken caught))))
             (plist-get (cddr err) :statement-kind))))
      (cl-letf (((symbol-function 'dorg-ql--dolt-pushdown-filter)
                 (lambda (_ast) 'fixture-pushdown))
                ((symbol-function 'dorg-ql--dolt-pushdown-select-sql)
                 (lambda (&rest _) "SELECT fixture"))
                ((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest _) (raise-broken))))
        (should
         (eq 'pushdown-select
             (catch-kind
              (lambda ()
                (dorg-ql--dolt-eval-general
                 '(:pipe (:source :all) (:filter fixture)) reg "rev"))))))
      (cl-letf (((symbol-function 'dorg-ql--dolt-pushdown-filter)
                 (lambda (_ast) 'fixture-pushdown))
                ((symbol-function 'dorg-ql--dolt-pushdown-select-sql)
                 (lambda (&rest _) "SELECT fixture"))
                ((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest _)
                   '(((id . 31) (title . "Assigned")))))
                ((symbol-function 'dorg-ql--dolt-properties-batch)
                 (lambda (&rest _) (raise-broken))))
        (should
         (eq 'pushdown-properties
             (catch-kind
              (lambda ()
                (dorg-ql--dolt-eval-general
                 '(:pipe (:source :all) (:filter fixture)) reg "rev"))))))
      (cl-letf (((symbol-function 'dorg-ql--dolt-pushdown-filter)
                 (lambda (_ast) nil))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (_dir _root _ids _as-of _headline-only query-fn &optional _levels)
                   (funcall query-fn "SELECT fixture")))
                ((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest _) (raise-broken))))
        (should
         (eq 'by-id-materialization
             (catch-kind
              (lambda ()
                (dorg-ql--dolt-eval-general
                 '(:pipe (:source :all) (:filter fixture)) reg "rev")))))))))

(ert-deftest dorg-test-85e9w-membership-leaf-fails-closed-off-dolt ()
  "Extension membership never acquires divergent model/plain semantics."
  (dorg-test-85e9w--with-membership-extension
    (let ((heading (make-dorg-heading :id 1 :title "Local")))
      (should-error
       (dorg-ql-select
        '(extension-member demo assignee-at 1 ("alice")) (list heading))
       :type 'dorg-ql-error))))

;;; ─── 8qtfg: property and membership filters lower to UNCORRELATED sets ──
;;
;; Both leaves used to lower to a correlated EXISTS whose subquery referenced
;; the outer row (`hp.heading_id = h.id', `eb.heading_id = h.id').  A
;; correlated subquery is re-evaluated per joined row and cannot be cached,
;; which is the producer mechanism behind roster read-idle timeouts.  Lowering
;; each to an independent set that the planner may evaluate once and cache is
;; the whole change.
;;
;; Scope note, per architect direction: the existing root/ordinal/value
;; controls above stay GREEN and are not restated here.  Only the CHANGED
;; shape and the NULL guards are new.  These are SHAPE controls — they prove
;; the emitted SQL is uncorrelated.  They are not execution proof: the pure
;; matcher controls are the semantic oracle, and only the consumer witness
;; shows the rewritten SQL computes the same rows.

(defconst dorg-test-8qtfg--outer-reference-re "= *h\\."
  "Matches a right-hand reference to the outer heading row.
Correlation is exactly that: a subquery term comparing something to the outer
`h'.  The uncorrelated form still MENTIONS `h.id', on the left of IN, so a
bare search for \"h.id\" would match the correct shape too and prove nothing.")

(ert-deftest dorg-test-8qtfg-property-exists-lowers-to-uncorrelated-set ()
  "Property existence lowers to `h.id IN (<independent set>)'."
  (require 'dorg-ql-dolt)
  (let ((sql (dorg-ql--dolt-filter-sql '(:property "BEAD_ID" exists nil) 17)))
    (should (string-match-p "h\\.id IN (" sql))
    (should-not (string-match-p dorg-test-8qtfg--outer-reference-re sql))
    (should-not (string-match-p "EXISTS" sql))
    ;; The set still selects the property view's heading ids on the same key.
    (should (string-match-p "SELECT hp\\.heading_id" sql))
    (should (string-match-p "heading_properties" sql))
    (should (string-match-p "hp\\.prop_key = 'BEAD_ID'" sql))
    ;; The UNKNOWN-collapsing wrapper is preserved.
    (should (string-match-p "COALESCE" sql))
    (should (string-match-p "FALSE" sql))))

(ert-deftest dorg-test-8qtfg-property-missing-lowers-to-uncorrelated-not-in ()
  "Property absence lowers to `h.id NOT IN (<independent set>)'."
  (require 'dorg-ql-dolt)
  (let ((sql (dorg-ql--dolt-filter-sql '(:property "BEAD_ID" missing nil) 17)))
    (should (string-match-p "h\\.id NOT IN (" sql))
    (should-not (string-match-p dorg-test-8qtfg--outer-reference-re sql))
    (should-not (string-match-p "EXISTS" sql))
    (should (string-match-p "COALESCE" sql))))

(ert-deftest dorg-test-8qtfg-property-set-excludes-null-heading-ids ()
  "Both property sets exclude NULL heading ids explicitly.
This is the guard that keeps NOT IN safe BY CONSTRUCTION rather than by a
schema invariant reachable only through a view definition two joins deep.
Its absence is silent and total: one NULL row makes NOT IN UNKNOWN for every
row, the enclosing COALESCE collapses that to FALSE, and a `missing' query
returns zero rows with no error."
  (require 'dorg-ql-dolt)
  (dolist (op '(exists missing))
    (let ((sql (dorg-ql--dolt-filter-sql
                (list :property "BEAD_ID" op nil) 17)))
      (should (string-match-p "hp\\.heading_id IS NOT NULL" sql)))))

(ert-deftest dorg-test-8qtfg-membership-lowers-to-uncorrelated-set ()
  "Membership lowers to `h.id IN (<set of descriptor heading ids>)'.
The existing root, ordinal, folded-value and quoting assertions are restated
here only to prove the shape change carried them along unchanged; they are
covered as standing contracts by the 85e9w controls above."
  (require 'dorg-ql-dolt)
  (dorg-test-85e9w--with-membership-extension
    (let ((sql (dorg-ql--dolt-filter-sql
                '(:extension-member demo assignee-at 1 ("alice")) 17)))
      (should (string-match-p "h\\.id IN (" sql))
      (should-not (string-match-p dorg-test-8qtfg--outer-reference-re sql))
      (should-not (string-match-p "EXISTS" sql))
      ;; The set projects the descriptor's heading column from the same join.
      (should (string-match-p "SELECT eb\\.heading_id" sql))
      (should (string-match-p "assignment_rows em" sql))
      (should (string-match-p "JOIN subjects eb ON eb\\.subject_id = em\\.subject_id" sql))
      ;; Carried through unchanged: root, position, folded value, quoting.
      (should (string-match-p "eb\\.root_id = 17" sql))
      (should (string-match-p "em\\.ordinal = 1" sql))
      (should (string-match-p "BINARY LOWER(em\\.assignee)" sql))
      (should (string-match-p "BINARY LOWER('alice')" sql))
      (should (string-match-p "COALESCE" sql)))))

(ert-deftest dorg-test-8qtfg-membership-set-excludes-null-heading-ids ()
  "The membership set excludes NULL heading ids explicitly.
On this leg the guard is defence in depth rather than load-bearing: the
caller's COALESCE already collapses UNKNOWN before any enclosing NOT sees it.
It is asserted so the shape stops depending on that wrapper."
  (require 'dorg-ql-dolt)
  (dorg-test-85e9w--with-membership-extension
    (let ((sql (dorg-ql--dolt-filter-sql
                '(:extension-member demo assignee-at 1 ("alice")) 17)))
      (should (string-match-p "eb\\.heading_id IS NOT NULL" sql)))))

(ert-deftest dorg-test-8qtfg-composed-statement-has-no-correlated-leaf ()
  "A composed AND/OR/NOT pushdown emits no correlated subquery anywhere,
and the selector around the leaves is untouched."
  (require 'dorg-ql-dolt)
  (dorg-test-85e9w--with-membership-extension
    (let* ((query '(and (field todo "IMPL")
                        (missing (property "EMBEAD_STATIC"))
                        (or (extension-member demo assignee-at 1 ("alice"))
                            ;; Negated property leaf: the QL surface spells
                            ;; existence as `missing', so NOT of it is how a
                            ;; composed query reaches the other polarity.
                            (not (missing (property "BEAD_ID"))))))
           (expr (cadr (nth 2 (dorg-ql-normalize query))))
           (sql (dorg-ql--dolt-pushdown-select-sql expr 17)))
      (should (dorg-ql--dolt-pushable-filter-p expr))
      ;; No leaf anywhere in the composed statement correlates.
      (should-not (string-match-p dorg-test-8qtfg--outer-reference-re sql))
      (should-not (string-match-p "EXISTS" sql))
      ;; The recursive selector itself is unchanged.
      (should (string-match-p "WITH RECURSIVE" sql))
      (should (string-match-p "CAST(HEX(sort_key) AS CHAR(4096))" sql))
      (should (string-match-p "ORDER BY BINARY q.path" sql))
      (should (string-match-p "root_id = 17" sql))
      (should (string-match-p "h.id <> 17" sql)))))

(ert-deftest dorg-test-level-offset-dolt-ql-level-predicate-effective ()
  "A Dolt `level' predicate matches the effective level after a deeper move.
B moves under A with offset 1, so its child C goes from level 2 to level 3
while C's own offset stays 1; `(>= (field level) 3)' must then match C."
  (dorg-test-ql--with-dolt-synced "* A\n* B\n** C\n" (:reg reg :dolt-dir dd :buffer buf)
    (let* ((root (dorg-backend-root-id (dorg-registration-backend reg)))
           (id-of (lambda (title)
                    (alist-get 'id (car (dorg-sql dd (format "SELECT id FROM headings WHERE title = %s"
                                                            (dorg-sql-quote title)))))))
           (titles (lambda ()
                     (mapcar (lambda (r) (plist-get r :title))
                             (dorg-ql-select '(>= (field level) 3) buf)))))
      (should-not (funcall titles))
      (dorg-test--apply-patches
       (list :dolt-dir dd :root-id root)
       (list (dorg-test--move-patch (funcall id-of "B")
                                    (dorg-test--heading-address (funcall id-of "A"))))
       :message "move B under A")
      (should (equal '("C") (funcall titles))))))

;;; ─── Multi-term title search execution ──────────────────────────────────
;;
;; The reported read was a long OR of title `contains' leaves guarded by a
;; property and projected through an extension identity column.  Title
;; leaves must execute without loading the root's heading models: sensitive
;; leaves lower to exact SQL, insensitive leaves are evaluated in Elisp over a
;; root-scoped id/title scan, and the selected ids are projected at the same
;; captured revision.  These controls pin the answers, the mechanism and the
;; snapshot.

(defconst dorg-test-ql-title-search--doc
  (concat "* Ontology work\n:PROPERTIES:\n:DORG_ID: 301\n:ITEM_ID: I-1\n:END:\n"
          "** ontology section\n:PROPERTIES:\n:DORG_ID: 302\n:END:\n"
          "* researcher notes\n:PROPERTIES:\n:DORG_ID: 303\n:ITEM_ID: I-2\n:END:\n"
          "* Roles and ROLES\n:PROPERTIES:\n:DORG_ID: 304\n:ITEM_ID: I-3\n:END:\n"
          "* Other item\n:PROPERTIES:\n:DORG_ID: 305\n:ITEM_ID: I-4\n:END:\n"
          "** roles section\n:PROPERTIES:\n:DORG_ID: 306\n:END:\n"
          "* work on ontology\n:PROPERTIES:\n:DORG_ID: 307\n:ITEM_ID: I-5\n:END:\n"
          "* no match here\n:PROPERTIES:\n:DORG_ID: 308\n:ITEM_ID: I-6\n:END:\n")
  "Tracker-shaped items with ITEM_ID, and nested sections without it.")

(defun dorg-test-ql-title-search--register-extension ()
  "Register a string column projecting ITEM_ID, standing in for a consumer id."
  (dorg-register-extension
   'demo :schema "SELECT 1" :version 1
   :fields
   (list (list :name 'demo/item-id :type 'string :cardinality 'one
               :dependencies '((property "ITEM_ID"))
               :eval (lambda (_context read) (funcall read '(property "ITEM_ID")))
               :sql (lambda (_context _aliases)
                      '(:expression "demo_item.prop_value"
                        :joins ((demo_item
                                 . "LEFT JOIN heading_properties demo_item ON demo_item.heading_id = h.id AND demo_item.prop_key = 'ITEM_ID'"))))))))

(defun dorg-test-ql-title-search--forms (mode projection)
  "Return the long OR form and the `contains-any' form under MODE.
Each guards on ITEM_ID and selects PROJECTION.  A nil MODE omits `:case'."
  (let ((needles '("ontology" "researcher" "roles"))
        (tail (and mode (list :case mode))))
    (list
     `(pipe (where (and (exists (property "ITEM_ID"))
                        (or ,@(mapcar (lambda (needle)
                                        `(contains (field title) ,needle ,@tail))
                                      needles))))
            (select ,@projection))
     `(pipe (where (and (exists (property "ITEM_ID"))
                        (contains-any (field title) ,needles ,@tail)))
            (select ,@projection)))))

(defmacro dorg-test-ql-title-search--without-hydration (&rest body)
  "Run BODY failing if a heading model load or body read is attempted."
  (declare (indent 0))
  `(let ((orig-live (symbol-function 'dorg-sql))
         (orig-pinned (symbol-function 'dorg-sql--at-revision)))
     (cl-flet ((guard (statement)
                 (when (string-match-p
                        "text_items\\|planning_items\\|drawer_lines\\|keyword_items\\|heading_tags"
                        statement)
                   (ert-fail (list "body or tag read during a title search" statement)))))
       (cl-letf (((symbol-function 'dorg-model-load-headings)
                  (lambda (&rest _) (ert-fail "title search loaded heading models")))
                 ((symbol-function 'dorg-sql)
                  (lambda (dolt-dir query &rest args)
                    (guard (if args (apply #'format query args) query))
                    (apply orig-live dolt-dir query args)))
                 ((symbol-function 'dorg-sql--at-revision)
                  (lambda (dolt-dir revision query &rest args)
                    (guard (if args (apply #'format query args) query))
                    (apply orig-pinned dolt-dir revision query args))))
         ,@body))))

(ert-deftest dorg-test-ql-title-search-extension-projection-at-one-snapshot ()
  "Long OR and `contains-any' title searches return compact projected rows.

The extension column and a property guard compose with title leaves in both
case modes; sections without ITEM_ID are excluded; rows keep document order;
the snapshot names the head captured before the read; and the requested
property key is the only property read."
  (dorg-test-ql--with-dolt-synced dorg-test-ql-title-search--doc
      (:buffer buf :dolt-dir dd)
    (let ((dorg--extensions nil)
          (expected
           '((sensitive
              ((("id" . "I-2") ("title" . "researcher notes"))
               (("id" . "I-5") ("title" . "work on ontology"))))
             (insensitive
              ((("id" . "I-1") ("title" . "Ontology work"))
               (("id" . "I-2") ("title" . "researcher notes"))
               (("id" . "I-3") ("title" . "Roles and ROLES"))
               (("id" . "I-5") ("title" . "work on ontology"))))))
          (head (dorg-head-commit dd))
          property-keys)
      (dorg-test-ql-title-search--register-extension)
      (dolist (mode '(nil sensitive insensitive))
        (dolist (query (dorg-test-ql-title-search--forms
                        mode '((as (field demo/item-id) id) (field title))))
          (let ((orig-batch (symbol-function 'dorg-ql--dolt-properties-batch)))
            (cl-letf (((symbol-function 'dorg-ql--dolt-properties-batch)
                       (lambda (dolt-dir ids &optional revision keys)
                         (push keys property-keys)
                         (funcall orig-batch dolt-dir ids revision keys))))
              (dorg-test-ql-title-search--without-hydration
                (let ((snapshot (dorg-ql-select-snapshot query buf)))
                  (should (equal (plist-get (plist-get snapshot :snapshot) :head) head))
                  (should (equal (mapcar (lambda (row) (plist-get row :values))
                                         (plist-get snapshot :rows))
                                 (cadr (assq (or mode 'sensitive) expected))))))))))
      ;; Only fields were selected, so no property rows were read at all.
      (should-not property-keys)
      ;; Selecting the property reads exactly that key.
      (let ((orig-batch (symbol-function 'dorg-ql--dolt-properties-batch)))
        (cl-letf (((symbol-function 'dorg-ql--dolt-properties-batch)
                   (lambda (dolt-dir ids &optional revision keys)
                     (push keys property-keys)
                     (funcall orig-batch dolt-dir ids revision keys))))
          (dorg-test-ql-title-search--without-hydration
            (should (equal (mapcar (lambda (row) (plist-get row :values))
                                   (dorg-ql-select
                                    (car (dorg-test-ql-title-search--forms
                                          'insensitive
                                          '((as (property "ITEM_ID") id) (field title))))
                                    buf))
                           (cadr (assq 'insensitive expected)))))))
      (should (equal property-keys '(("ITEM_ID")))))))

(ert-deftest dorg-test-ql-title-search-backends-agree-on-public-forms ()
  "Model, plain-org and Dolt agree on rows and order for both forms and modes."
  (let ((dorg--extensions nil)
        (queries (apply #'append
                        (mapcar (lambda (mode)
                                  (dorg-test-ql-title-search--forms
                                   mode '((as (field demo/item-id) id) (field title))))
                                '(nil sensitive insensitive))))
        (values (lambda (rows) (mapcar (lambda (row) (plist-get row :values)) rows)))
        model plain)
    (dorg-test-ql-title-search--register-extension)
    (let ((headings (dorg-test-ql--model-from-org dorg-test-ql-title-search--doc)))
      (setq model (mapcar (lambda (query) (funcall values (dorg-ql-select query headings)))
                          queries)))
    (dorg-test-ql--with-plain-org-file dorg-test-ql-title-search--doc (:buffer pbuf)
      (setq plain (mapcar (lambda (query) (funcall values (dorg-ql-select query pbuf)))
                          queries)))
    (should (equal model plain))
    ;; The fixture separates the modes, so agreement is not trivially empty.
    (should (= 2 (length (nth 0 model))))
    (should (= 4 (length (nth 4 model))))
    (let ((dorg--extensions nil))
      (dorg-test-ql--with-dolt-synced dorg-test-ql-title-search--doc (:buffer dbuf)
        (dorg-test-ql-title-search--register-extension)
        (dorg-test-ql-title-search--without-hydration
          (should (equal model
                         (mapcar (lambda (query)
                                   (funcall values (dorg-ql-select query dbuf)))
                                 queries))))))))

(ert-deftest dorg-test-ql-title-search-empty-candidates-skip-projection ()
  "A title search with no match issues no projection read and still checks one."
  (dorg-test-ql--with-dolt-synced dorg-test-ql-title-search--doc (:buffer buf)
    (let ((dorg--extensions nil) (statements 0)
          (orig (symbol-function 'dorg-ql--dolt-query)))
      (dorg-test-ql-title-search--register-extension)
      (cl-letf (((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest args) (cl-incf statements) (apply orig args))))
        (dorg-test-ql-title-search--without-hydration
          (should-not
           (dorg-ql-select
            '(pipe (where (contains (field title) "zz-absent" :case insensitive))
                   (select (field demo/item-id)))
            buf))
          (should (= 1 statements))
          (should-error
           (dorg-ql-select
            '(one (pipe (where (contains (field title) "zz-absent" :case insensitive))
                        (select (field demo/item-id))))
            buf)
           :type 'dorg-ql-error)
          (should (= 2 statements))
          (should (equal '((("id" . "I-2")))
                         (mapcar (lambda (row) (plist-get row :values))
                                 (dorg-ql-select
                                  '(one (pipe (where (contains (field title) "RESEARCHER"
                                                               :case insensitive))
                                              (select (as (field demo/item-id) id))))
                                  buf))))
          ;; Two matches still fail the cardinality check after selection.
          (should-error
           (dorg-ql-select
            '(one (pipe (where (contains (field title) "ROLES" :case insensitive))
                        (select (field demo/item-id))))
            buf)
           :type 'dorg-ql-error))))))

(defconst dorg-test-ql-title-search--matrix-titles
  (list "plain ascii" "Plain ASCII" "Ünïcöd\u00e9 ß" "STRASSE ẞ" "e\u0301clair" "\u00e9clair"
        "rocket \U0001F680 launch" "100% _under_ back\\slash" "it's \"quoted\""
        "pad " "pad" "" nil "ΑΣ σ" "\u212A kelvin" "ǆungla Ǆ" "ſtar ıtem"
        "bad \uFFFD byte" "a!b" "\u2126 ohm" "final ς")
  "Stored title classes for the parity matrix; nil stores SQL NULL.")

(defconst dorg-test-ql-title-search--matrix-needles
  (list "a" "A" "ß" "ẞ" "e\u0301" "\u00e9" "\U0001F680" "%" "_" "\\" "'" "\"" ""
        "pad" "pad " "σ" "Σ" "k" "K" "\u212A" "ǆ" "Ǆ" "s" "S" "ı" "i" "\uFFFD" "!"
        "plain" "slash" "ascii" "strasse" "ς" "ω" "Ω")
  "Needle classes for the parity matrix.
The final sigma and Ohm sign rows separate the standard-case upcase rule
from a lowercase comparison: ς upcases to Σ but does not lowercase to σ, and
the Ohm sign lowercases to ω while ω upcases to the Greek capital, not to it.")

(defun dorg-test-ql-title-search--seed-matrix (dolt-dir root-id)
  "Store the matrix titles verbatim over the seeded headings of ROOT-ID."
  (let ((ids (mapcar (lambda (row) (alist-get 'id row))
                     (dorg-sql dolt-dir
                               (format "SELECT id FROM headings WHERE root_id = %d AND id <> %d ORDER BY id"
                                       root-id root-id)))))
    (should (= (length ids) (length dorg-test-ql-title-search--matrix-titles)))
    (dorg-sql-batch
     dolt-dir
     (cl-mapcar (lambda (id title)
                  (format "UPDATE headings SET title = %s WHERE id = %d"
                          (if title (dorg-sql-quote title) "NULL") id))
                ids dorg-test-ql-title-search--matrix-titles)
     "fixture: matrix titles")))

(defun dorg-test-ql-title-search--matrix-doc ()
  "Return one plain heading per matrix title; titles are replaced in SQL."
  (mapconcat (lambda (i) (format "* T%d\n:PROPERTIES:\n:DORG_ID: %d\n:END:\n" i (+ 400 i)))
             (number-sequence 1 (length dorg-test-ql-title-search--matrix-titles)) ""))

(defun dorg-test-ql-title-search--matrix-queries (mode)
  "Return title leaf, negated and disjunctive queries under case MODE."
  (let (queries)
    (dolist (op '(= != contains prefix suffix))
      (dolist (needle dorg-test-ql-title-search--matrix-needles)
        (let ((leaf `(,op (field title) ,needle :case ,mode)))
          (push leaf queries)
          (push `(not ,leaf) queries))))
    (dolist (pair '(("a" "ß") ("%" "_") ("" "pad") ("K" "\u212A") ("ẞ" "σ")))
      (push `(or (contains (field title) ,(car pair) :case ,mode)
                 (prefix (field title) ,(cadr pair) :case ,mode))
            queries)
      (push `(not (or (suffix (field title) ,(car pair) :case ,mode)
                      (= (field title) ,(cadr pair) :case ,mode)))
            queries))
    (nreverse queries)))

(defun dorg-test-ql-title-search--assert-matrix (mode)
  "Assert Dolt title results equal the shared Elisp evaluator under MODE.
Expected ids come from the model adapter over headline models loaded from
the same revision, before the hydration poison is installed."
  (dorg-test-ql--with-dolt-synced (dorg-test-ql-title-search--matrix-doc)
      (:buffer buf :dolt-dir dd :reg reg)
    (let ((root (dorg-backend-root-id (dorg-registration-backend reg))))
      (dorg-test-ql-title-search--seed-matrix dd root)
      (let* ((models (dorg-ql--dolt-ordered-headings
                      (dorg-model-load-headings dd root nil nil t) root))
             (queries (dorg-test-ql-title-search--matrix-queries mode))
             (expected (mapcar (lambda (query)
                                 (dorg-test-ql--ids-in-order (dorg-ql-select query models)))
                               queries))
             (mismatches nil))
        ;; The NULL row really is NULL, and the fixture is non-vacuous.
        (should (cl-some (lambda (h) (null (dorg-heading-title h))) models))
        (should (cl-some #'identity expected))
        (dorg-test-ql-title-search--without-hydration
          (cl-mapc (lambda (query want)
                     (let ((got (dorg-test-ql--ids-in-order (dorg-ql-select query buf))))
                       (unless (equal got want)
                         (push (list query :want want :got got) mismatches))))
                   queries expected))
        (should-not mismatches)))))

(ert-deftest dorg-test-ql-title-search-sensitive-sql-matches-elisp-matrix ()
  "Sensitive title leaves in SQL agree with the shared evaluator everywhere.
Rows cover ASCII, multibyte, combining marks, astral, wildcard and quote
characters, a trailing space, empty and NULL titles and a replacement
character, under every operator, NOT and OR."
  (dorg-test-ql-title-search--assert-matrix 'sensitive))

(ert-deftest dorg-test-ql-title-search-insensitive-scan-matches-elisp-matrix ()
  "Insensitive title leaves agree with the shared evaluator everywhere.
The matrix includes the standard-case-table edges ß/ẞ, ǆ/Ǆ, ſ, ı, final
sigma, the Ohm sign and the Kelvin sign that SQL UPPER/LOWER are not known to
reproduce."
  (dorg-test-ql-title-search--assert-matrix 'insensitive))

(ert-deftest dorg-test-ql-title-search-store-refuses-invalid-utf8-title ()
  "Invalid UTF-8 cannot be stored as a title, so byte and code-point tests agree.
This records the boundary of the parity matrix: the replacement character is
storable and covered there; malformed bytes are refused by the store."
  (dorg-test-ql--with-dolt-synced (dorg-test-ql-title-search--matrix-doc)
      (:dolt-dir dd)
    (let ((err (should-error
                (dorg-sql dd "UPDATE headings SET title = UNHEX('C328FF') WHERE id = 401"))))
      (should (string-match-p "Incorrect string value" (error-message-string err))))
    ;; The same statement shape stores a valid replacement character.
    (dorg-sql dd "UPDATE headings SET title = UNHEX('EFBFBD') WHERE id = 401")
    (should (equal "\uFFFD"
                   (alist-get 'title (car (dorg-sql dd "SELECT title FROM headings WHERE id = 401")))))))

(ert-deftest dorg-test-ql-title-search-both-stages-read-the-captured-revision ()
  "A commit landing during an insensitive title search is invisible to it.
One write lands after snapshot capture and before the candidate scan; a
second lands between the scan and the projection.  The rows reflect the
captured head exactly, and both writes demonstrably moved HEAD."
  (dorg-test-ql--with-dolt-synced dorg-test-ql-title-search--doc
      (:buffer buf :dolt-dir dd)
    (let* ((dorg--extensions nil)
           (calls 0)
           (head (dorg-head-commit dd))
           (after-first nil)
           (orig (symbol-function 'dorg-ql--dolt-query))
           (query '(pipe (where (and (exists (property "ITEM_ID"))
                                     (contains-any (field title)
                                                   ("ontology" "researcher")
                                                   :case insensitive)))
                         (select (as (field demo/item-id) id) (field title))))
           (values (lambda (rows) (mapcar (lambda (row) (plist-get row :values)) rows))))
      (dorg-test-ql-title-search--register-extension)
      (cl-letf (((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (when (= calls 1)
                     ;; Before the candidate scan: a non-match starts matching.
                     (dorg-sql-batch
                      dd '("UPDATE headings SET title = 'late ontology' WHERE id = 308")
                      "concurrent write before scan")
                     (setq after-first (dorg-head-commit dd)))
                   (prog1 (apply orig args)
                     (when (= calls 1)
                       ;; Between scan and projection: a match is renamed.
                       (dorg-sql-batch
                        dd '("UPDATE headings SET title = 'renamed' WHERE id = 303")
                        "concurrent write before projection"))))))
        (let ((snapshot (dorg-ql-select-snapshot query buf)))
          (should (= calls 2))
          (should (equal (plist-get (plist-get snapshot :snapshot) :head) head))
          (should (equal (funcall values (plist-get snapshot :rows))
                         '((("id" . "I-1") ("title" . "Ontology work"))
                           (("id" . "I-2") ("title" . "researcher notes"))
                           (("id" . "I-5") ("title" . "work on ontology")))))))
      ;; Each write moved HEAD and each changes the answer at the live head,
      ;; so the snapshot assertions above are not vacuous.
      (should-not (equal after-first head))
      (should-not (equal (dorg-head-commit dd) after-first))
      (should (equal (funcall values (dorg-ql-select query buf))
                     '((("id" . "I-1") ("title" . "Ontology work"))
                       (("id" . "I-5") ("title" . "work on ontology"))
                       (("id" . "I-6") ("title" . "late ontology")))))
      (should (equal (funcall values
                              (dorg-ql-select
                               '(pipe (where (= (field title) "renamed"))
                                      (select (as (field demo/item-id) id)))
                               buf))
                     '((("id" . "I-2"))))))))

(ert-deftest dorg-test-ql-title-search-rank-prefilter-lowers-sensitive-title ()
  "A ranked read's sensitive title prefilter uses the exact title lowering.
An insensitive title prefilter remains a typed residual refusal."
  (require 'dorg-ql-dolt)
  (dolist (row '((contains "INSTR(BINARY h.title, BINARY 'ab') > 0")
                 (prefix "BINARY LEFT(h.title, 2) = BINARY 'ab'")
                 (suffix "BINARY RIGHT(h.title, 2) = BINARY 'ab'")
                 (!= "BINARY h.title <> BINARY 'ab'")))
    (let ((filter-sql
           (plist-get (dorg-ql--dolt-bm25-rank-components
                       (dorg-ql-normalize
                        `(pipe (where (,(car row) (field title) "ab"))
                               (rank-bm25 "q" :profile sensitive :top-k 3)))
                       17)
                      :filter-sql)))
      (should (string-match-p (regexp-quote (cadr row)) filter-sql))
      (should-not (string-match-p "LOCATE\\|LIKE\\| contains \\| prefix \\| suffix " filter-sql))))
  (should-error
   (dorg-ql--dolt-bm25-rank-components
    (dorg-ql-normalize
     '(pipe (where (contains (field title) "ab" :case insensitive))
            (rank-bm25 "q" :profile sensitive :top-k 3)))
    17)
   :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-title-search-extension-filter-keeps-residual-refusal ()
  "An insensitive title leaf beside an extension filter is not scanned.
The candidate scan reads the headings row plus the property-existence guard
only.  A tree that also filters on an extension field or a declared
membership keeps the established refusal, message included, and issues no
candidate scan or other data statement."
  (dorg-test-ql--with-dolt-synced dorg-test-ql-title-search--doc
      (:buffer buf)
    (let ((dorg--extensions nil) (statements 0)
          (orig (symbol-function 'dorg-ql--dolt-query)))
      (dorg-test-ql-title-search--register-extension)
      (dorg-register-extension
       'members :schema "SELECT 1" :version 1
       :read-tables dorg-test-85e9w--membership-tables)
      (cl-letf (((symbol-function 'dorg-ql--dolt-query)
                 (lambda (&rest args) (cl-incf statements) (apply orig args))))
        (dorg-test-ql-title-search--without-hydration
          (dolist (row
                   '(((pipe (where (and (contains (field title) "ontology" :case insensitive)
                                        (= (field demo/item-id) "I-1")))
                            (select (field title)))
                      "extension fields cannot use this residual plan: (demo/item-id)")
                     ((pipe (where (and (exists (property "ITEM_ID"))
                                        (contains (field title) "ontology" :case insensitive)
                                        (extension-member members assignee-at 1 ("alice"))))
                            (select (field title)))
                      "extension membership cannot compose with residual filter: (:cmp title contains \"ontology\" :case insensitive)")))
            (let ((err (should-error (dorg-ql-select (car row) buf)
                                     :type 'dorg-ql-error)))
              (should (eq 'unsupported (dorg-error-reason err)))
              (should (equal (cadr row) (nth 1 err)))))))
      (should (= 0 statements)))))

(ert-deftest dorg-test-ql-title-search-revision-plans-are-root-scoped-3000 ()
  "Title-search statements plan as root-scoped, keyed reads at the revision.
Over 3000 tracker items with sections, EXPLAIN PLAN in the revision
database shows: the sensitive pushdown walks the tree by the root/parent
index; the candidate scan reads headings by the root index, with no join
and no table but the property view inside the one cacheable existence
subquery; the projection resolves candidate ids as primary-key point
lookups.  No plan has a cross join."
  (let ((contents
         (mapconcat
          (lambda (i)
            (concat (format "* %s item %04d\n:PROPERTIES:\n:DORG_ID: %d\n:ITEM_ID: I-%d\n:END:\n"
                            (cond ((= 0 (mod i 97)) "Ontology")
                                  ((= 0 (mod i 89)) "roles")
                                  (t "plain"))
                            i (+ 10000 i) i)
                    (if (= 0 (mod i 10))
                        (format "** ontology section %d\n:PROPERTIES:\n:DORG_ID: %d\n:END:\n"
                                i (+ 20000 i))
                      "")))
          (number-sequence 1 3000) "")))
    (dorg-test-ql--with-dolt-synced contents (:buffer buf :dolt-dir dd)
      (let ((orig (symbol-function 'dorg-ql--dolt-query))
            statements)
        (cl-labels ((plan (revision sql)
                      (mapconcat (lambda (row) (format "%s" (alist-get 'plan row)))
                                 (dorg-sql--at-revision dd revision (concat "EXPLAIN PLAN " sql))
                                 "\n"))
                    (run (query)
                      (setq statements nil)
                      (cl-letf (((symbol-function 'dorg-ql--dolt-query)
                                 (lambda (dir revision sql &rest args)
                                   (push (cons revision (if args (apply #'format sql args) sql))
                                         statements)
                                   (apply orig dir revision sql args))))
                        (dorg-test-ql-title-search--without-hydration
                          (prog1 (dorg-ql-select query buf)
                            (setq statements (nreverse statements)))))))
          ;; Stage A: one sensitive pushdown statement.
          (should (= 63 (length (run '(pipe (where (and (exists (property "ITEM_ID"))
                                                        (or (contains (field title) "Ontology")
                                                            (contains (field title) "roles"))))
                                            (select (field title)))))))
          (should (= 1 (length statements)))
          (let ((a (plan (caar statements) (cdar statements))))
            (should (string-match-p "index: \\[headings.root_id,headings.parent_id\\]" a))
            (should-not (string-match-p "Cross" a)))
          ;; Stage B: candidate scan, then keyed projection.
          (should (= 63 (length (run '(pipe (where (and (exists (property "ITEM_ID"))
                                                        (contains-any (field title)
                                                                      ("ONTOLOGY" "Roles")
                                                                      :case insensitive)))
                                            (select (field title)))))))
          (should (= 2 (length statements)))
          (pcase-let ((`((,rev1 . ,scan) (,rev2 . ,projection)) statements))
            (should (equal rev1 rev2))
            (should (string-match-p
                     "\\`SELECT h\\.id AS id, h\\.title AS title, .* FROM headings h +WHERE h\\.root_id = "
                     scan))
            (should-not (string-match-p "JOIN" scan))
            (should (equal '("heading_properties")
                           (let ((start 0) tables)
                             (while (string-match "FROM \\([a-z_]+\\)" scan start)
                               (push (match-string 1 scan) tables)
                               (setq start (match-end 0)))
                             (delete-dups (delete "headings" tables)))))
            (let ((b1 (plan rev1 scan))
                  (b2 (plan rev2 projection))
                  (first-id (and (string-match "h\\.id IN (\\([0-9]+\\)" projection)
                                 (match-string 1 projection))))
              (should (string-match-p
                       "IndexedTableAccess(headings)\n[^\n]*index: \\[headings.root_id\\]" b1))
              (should (string-match-p "cacheable: true" b1))
              (should-not (string-match-p "Cross" b1))
              (should first-id)
              (should (string-match-p
                       (concat "index: \\[headings.id\\]\n[^\n]*filters: \\[{\\["
                               first-id ", " first-id "\\]}")
                       b2))
              (should-not (string-match-p "Cross" b2)))))))))

;;; ─── Extension string-field lowering ────────────────────────────────────
;;
;; A string extension field compared in SQL must answer exactly what the
;; shared evaluator answers on the model and plain-org sources.  The field
;; here reads the stored title column through a declared join, so the title
;; parity matrix doubles as an extension-field matrix.

(defun dorg-test-ql-extension-string--register ()
  "Register a string field whose SQL is the stored title through a join."
  (dorg-register-extension
   'demo :schema "SELECT 1" :version 1
   :fields
   (list (list :name 'demo/label :type 'string :cardinality 'one
               :dependencies '((field title))
               :eval (lambda (_context read) (funcall read '(field title)))
               :sql (lambda (_context _aliases)
                      '(:expression "demo_label.title"
                        :joins ((demo_label
                                 . "LEFT JOIN headings demo_label ON demo_label.id = h.id"))))))))

(ert-deftest dorg-test-ql-extension-string-sql-matches-elisp-matrix ()
  "Sensitive extension string comparisons in SQL agree with the evaluator.
Every operator, NOT and OR run over the stored title classes of the title
matrix (case, multibyte, combining, astral, wildcard, quote, trailing-space,
empty, NULL and replacement-character values), against the model adapter on
headline models of the same revision."
  (dorg-test-ql--with-dolt-synced (dorg-test-ql-title-search--matrix-doc)
      (:buffer buf :dolt-dir dd :reg reg)
    (let ((root (dorg-backend-root-id (dorg-registration-backend reg)))
          (dorg--extensions nil)
          (mismatches nil))
      (dorg-test-ql-title-search--seed-matrix dd root)
      (dorg-test-ql-extension-string--register)
      (let* ((models (dorg-ql--dolt-ordered-headings
                      (dorg-model-load-headings dd root nil nil t) root))
             (queries
              (let (queries)
                (dolist (op '(= != contains prefix suffix))
                  (dolist (needle dorg-test-ql-title-search--matrix-needles)
                    (push `(,op (field demo/label) ,needle) queries)
                    (push `(not (,op (field demo/label) ,needle)) queries)))
                (push '(or (contains (field demo/label) "a")
                           (prefix (field demo/label) "ß"))
                      queries)
                (nreverse queries)))
             (expected (mapcar (lambda (query)
                                 (dorg-test-ql--ids-in-order (dorg-ql-select query models)))
                               queries)))
        (should (cl-some #'identity expected))
        (cl-mapc (lambda (query want)
                   (let ((got (dorg-test-ql--ids-in-order (dorg-ql-select query buf))))
                     (unless (equal got want)
                       (push (list query :want want :got got) mismatches))))
                 queries expected)
        (should-not mismatches)))))

(ert-deftest dorg-test-ql-extension-string-sql-shape-of-other-operators ()
  "Only string containment, prefix and suffix use the exact helper.
Equality and relational string/timestamp comparisons, integer, boolean and
identity lowerings keep their established SQL text."
  (require 'dorg-ql-dolt)
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'demo :schema "SELECT 1" :version 1
     :fields
     (list (list :name 'demo/s :type 'string :cardinality 'one
                 :sql (lambda (_c _a) '(:expression "x.s")))
           (list :name 'demo/ts :type 'timestamp :cardinality 'one
                 :sql (lambda (_c _a) '(:expression "x.ts")))
           (list :name 'demo/n :type 'integer :cardinality 'one
                 :sql (lambda (_c _a) '(:expression "x.n")))
           (list :name 'demo/b :type 'boolean :cardinality 'one
                 :sql (lambda (_c _a) '(:expression "x.b")))
           (list :name 'demo/id :type 'identity :cardinality 'one
                 :sql (lambda (_c _a) '(:expression "x.d" :key-expression "x.k")))))
    (dolist (row '(((demo/s = "x") "COALESCE((BINARY x.s = BINARY 'x'), FALSE)")
                   ((demo/s != "x") "COALESCE((BINARY x.s <> BINARY 'x'), FALSE)")
                   ((demo/s < "x") "COALESCE((BINARY x.s < BINARY 'x'), FALSE)")
                   ((demo/s contains "ß") "COALESCE((INSTR(BINARY x.s, BINARY 'ß') > 0), FALSE)")
                   ((demo/s prefix "ß") "COALESCE((BINARY LEFT(x.s, 1) = BINARY 'ß'), FALSE)")
                   ((demo/s suffix "ß") "COALESCE((BINARY RIGHT(x.s, 1) = BINARY 'ß'), FALSE)")
                   ((demo/ts = "2026-01-01")
                    "COALESCE((BINARY x.ts = BINARY '2026-01-01'), FALSE)")
                   ((demo/ts >= "2026-01-01")
                    "COALESCE((BINARY x.ts >= BINARY '2026-01-01'), FALSE)")
                   ((demo/n > 3) "COALESCE((x.n > 3), FALSE)")
                   ((demo/b = t) "COALESCE((x.b = TRUE), FALSE)")
                   ((demo/b != :json-false) "COALESCE((x.b <> FALSE), FALSE)")
                   ((demo/id = "A") "COALESCE((BINARY x.k = BINARY 'v2:41'), FALSE)")))
      (let ((dorg-ql--dolt-field-plans nil))
        (should (equal (cadr row)
                       (apply #'dorg-ql--dolt-field-comparison
                              (append (car row) (list 17)))))))))

(provide 'dorg-ql-tests)
;;; dorg-ql-tests.el ends here
