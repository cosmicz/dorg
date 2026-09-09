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
  (let ((headings (list (make-dorg-heading :id 7 :title "Seven" :level 1))))
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
           :id 1 :parent-id nil :level 1 :title "Exported"
           :items (list (dorg-keyword-item :keyword "EXPORT_FILE_NAME"
                                           :value "exported")
                        (dorg-planning-item :deadline "<2026-06-03 Wed>")))
          (make-dorg-heading
           :id 2 :parent-id nil :level 1 :title "Plain"
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
  (let ((headings (list (make-dorg-heading :id nil :level 1 :title "No Id")
                        (make-dorg-heading :id 7 :level 1 :title "First")
                        (make-dorg-heading :id 7 :level 1 :title "Second"))))
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
  (let ((headings (list (make-dorg-heading :id 2 :parent-id 1 :level 2
                                           :title "Child")
                        (make-dorg-heading :id 3 :parent-id 2 :level 3
                                           :title "Grandchild")
                        (make-dorg-heading :id 4 :parent-id 99 :level 2
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
  (let ((good (list (make-dorg-heading :id 1 :parent-id nil :level 1
                                       :title "Parent")
                    (make-dorg-heading :id 2 :parent-id 1 :level 2
                                       :title "Child")))
        (nil-id (list (make-dorg-heading :id nil :level 1 :title "No Id")
                      (make-dorg-heading :id 2 :parent-id nil :level 1
                                         :title "Other")))
        (duplicate (list (make-dorg-heading :id 1 :level 1 :title "One")
                         (make-dorg-heading :id 1 :level 1 :title "Again"))))
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
   :level 1
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
  (let* ((headings (list (make-dorg-heading :id 1 :level 1 :title "A")))
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

(defmacro dorg-test-ql--with-dolt-synced (contents bindings &rest body)
  "Import CONTENTS, bind its Dolt-backed registration, and run BODY.
BINDINGS plist: (:dolt-dir DDVAR :reg RVAR :buffer BVAR).
Skips if Dolt is unavailable."
  (declare (indent 2))
  (let ((dolt-dir (or (plist-get bindings :dolt-dir) (gensym "dd")))
        (reg (or (plist-get bindings :reg) (gensym "reg")))
        (buf (or (plist-get bindings :buffer) (gensym "buf"))))
    `(if (not dorg-test--dolt-available)
         (ert-skip "dolt not installed")
       (let* ((temp-base (dorg-test--make-owned-temp-dir
                          "dorg-ql-dolt-" "ql-dolt-synced"))
              (,dolt-dir (expand-file-name "dolt/" temp-base))
              (org-file (expand-file-name "doc.org" temp-base)))
         (unwind-protect
             (let ((dorg--registry nil))
               (dorg-init ,dolt-dir)
               (with-temp-buffer
                 (insert ,contents)
                 (org-mode)
                 (set-visited-file-name org-file t)
                 (set-buffer-modified-p nil)
                 (dorg-import-document
                  (current-buffer)
                  :dolt-dir ,dolt-dir
                  :target-file org-file
                  :mode 'bootstrap)
                 (let ((,reg (dorg-bind-dolt org-file ,dolt-dir))
                       (,buf (current-buffer)))
                   (ignore ,reg)
                   ,@body)))
           (dorg-test--delete-owned-root temp-base))))))

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
  "Run BODY counting `dorg-sql' queries by SQL text.
HP-VAR is bound to the number of executed queries whose template
references `heading_properties'; IN-VAR to those matching the batched
`WHERE heading_id IN (' load.  Both names refer to mutable counters that
BODY may read after the wrapped calls run.  Wraps `dorg-sql' so the real
implementation still executes."
  (declare (indent 2))
  `(let ((,hp-var 0)
         (,in-var 0)
         (dorg-test-ql--orig-sql (symbol-function 'dorg-sql)))
     (cl-letf (((symbol-function 'dorg-sql)
                (lambda (dolt-dir query &rest args)
                  (when (string-match-p "heading_properties" query)
                    (setq ,hp-var (1+ ,hp-var)))
                  (when (string-match-p "WHERE heading_id IN (" query)
                    (setq ,in-var (1+ ,in-var)))
                  (apply dorg-test-ql--orig-sql dolt-dir query args))))
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
  (make-dorg-heading :id id :title title :level 1 :items items))

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
                 (lambda (dolt-dir root-id ids &optional as-of headline-only)
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only))))
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
                 (lambda (dolt-dir root-id ids &optional as-of headline-only)
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only))))
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
                 (lambda (dolt-dir root-id ids &optional as-of headline-only)
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only))))
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
                 (lambda (dolt-dir root-id ids &optional as-of headline-only)
                   (unless headline-only (setq full-load (1+ full-load)))
                   (funcall orig dolt-dir root-id ids as-of headline-only))))
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
             (make-dorg-heading :id 1 :parent-id nil :level 1 :title "Alpha"
                                :priority "A" :tags '("work"))
             (make-dorg-heading :id 2 :parent-id 1 :level 2 :title "Alpha One"
                                :priority "B" :todo "TODO")
             (make-dorg-heading :id 3 :parent-id 1 :level 2 :title "Alpha Two"
                                :priority "C" :todo "DONE" :commented t)
             (make-dorg-heading :id 4 :parent-id nil :level 1 :title "Beta"
                                :tags '("home"))
             (make-dorg-heading :id 5 :parent-id 4 :level 2 :title "Beta One"
                                :priority "A")
             (make-dorg-heading :id 6 :parent-id 5 :level 3
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
          (orig (symbol-function 'dorg-sql)))
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (dolt-dir query &rest args)
                   (when (string-match-p
                          "text_items\\|planning_items\\|drawer_lines\\|keyword_items"
                          query)
                     (setq body-touched (1+ body-touched)))
                   (apply orig dolt-dir query args))))
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
                :id 1 :level 1 :title "Work"
                :items (list (dorg-keyword-item :keyword "CATEGORY"
                                                :value "work")))
               (make-dorg-heading
                :id 2 :level 1 :title "Home"
                :items (list (dorg-keyword-item :keyword "CATEGORY"
                                                :value "home")))
               (make-dorg-heading :id 3 :level 1 :title "None"))))
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

(provide 'dorg-ql-tests)
;;; dorg-ql-tests.el ends here
