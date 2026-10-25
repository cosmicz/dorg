;;; dorg-ql-ast-tests.el --- Tests for the dorg-ql v2 normalizer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-ql-normalize': the central v2 AST normalizer/validator.
;; Covers compatibility-alias normalization, the composable v2 reader forms
;; (pipe/where/traverse/unique, and/or/not, field/property/tag/id/timestamp),
;; boolean arity, field/operator validation, the normalized AST shape, and
;; the `dorg-ql-error' behavior for malformed forms.  Normalization is pure
;; structure: it never evaluates against a backend, so these tests need no
;; org buffers or Dolt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-ql-ast)

;;; ─── Helpers ──────────────────────────────────────────────────────────

(defun dorg-test-ql-ast--filter (query)
  "Normalize QUERY and return the single filter EXPR it wraps.
Asserts the canonical pipeline envelope `(:pipe (:source :all) (:filter EXPR))'."
  (let ((ast (dorg-ql-normalize query)))
    (should (eq (nth 0 ast) :pipe))
    (should (equal (nth 1 ast) '(:source :all)))
    (should (eq (car (nth 2 ast)) :filter))
    (cadr (nth 2 ast))))

;;; ─── AST envelope ─────────────────────────────────────────────────────

(ert-deftest dorg-test-ql-ast-select-columns ()
  "Projection keeps expression nodes and explicit output names."
  (should
   (equal (dorg-ql-normalize
           '(pipe (where (= (field todo) "TODO"))
                  (select (field title) (as (property "BEAD_ID") id))))
          '(:pipe (:source :all) (:filter (:cmp todo = "TODO" :case sensitive))
                  (:select (:column "title" (:field title))
                           (:column "id" (:property "BEAD_ID")))))))

(ert-deftest dorg-test-ql-ast-select-rejects-ambiguous-or-invalid-columns ()
  "Bad projection cannot silently overwrite columns or widen selection."
  (dolist (query '((pipe (select))
                   (pipe (select (field unknown)))
                   (pipe (select (field title extra)))
                   (pipe (select (as (field title))))
                   (pipe (select (as (field title) nil)))
                   (pipe (select (as (field title) . bad)))
                   (pipe (select (field title) (as (field todo) title)))
                   (pipe (select (property "x") (as (field title) "x")))
                   (pipe (select (field title)) (where (id 1)))
                   (pipe (select (field title)) (select (field id)))
                   (pipe (where (select (field title))))))
    (should-error (dorg-ql-normalize query) :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-ast-one-before-select ()
  "The cardinality wrapper asserts heading count before final projection."
  (should
   (equal (dorg-ql-normalize '(one (pipe (where (id 7))
                                       (select (field title)))))
          '(:pipe (:source :all) (:filter (:id = 7)) (:one)
                  (:select (:column "title" (:field title)))))))

(ert-deftest dorg-test-ql-ast-top-level-predicate-wraps-in-pipe ()
  "A top-level predicate normalizes to a pipe over the :all source."
  (should (equal (dorg-ql-normalize '(id 7))
                 '(:pipe (:source :all) (:filter (:id = 7))))))

(ert-deftest dorg-test-ql-ast-pipe-preserves-step-order ()
  "A pipe keeps its steps ordered after the synthesized source step."
  (should (equal (dorg-ql-normalize
                  '(pipe (where (tag "x")) (traverse children ancestors) (unique)))
                 '(:pipe (:source :all)
                         (:filter (:tag = "x" :case sensitive))
                         (:traverse children ancestors)
                         (:unique)))))

(ert-deftest dorg-test-ql-ast-pipe-allows-interleaved-filters ()
  "Filters and traversals interleave in order; not bucketed into slots."
  (should (equal (dorg-ql-normalize
                  '(pipe (where (id 1)) (traverse parent) (where (tag "t"))))
                 '(:pipe (:source :all)
                         (:filter (:id = 1))
                         (:traverse parent)
                         (:filter (:tag = "t" :case sensitive))))))

;;; ─── (one QUERY) cardinality wrapper ──────────────────────────────────
;; AC-2 (tracked-case): the PUBLIC surface of `(one QUERY)' is a top-level
;; query WRAPPER.  It normalizes to the inner query's pipe with a terminal
;; `(:one)' assertion step appended — a single spelling shared by read and
;; mutate.  It is NOT a bare pipeline step and NOT a where-predicate.

(ert-deftest dorg-test-ql-ast-one-wraps-predicate ()
  "(one PRED) normalizes to the inner pipe with a terminal (:one) step."
  (should (equal (dorg-ql-normalize '(one (id 7)))
                 '(:pipe (:source :all) (:filter (:id = 7)) (:one)))))

(ert-deftest dorg-test-ql-ast-one-wraps-pipe ()
  "(one (pipe ...)) appends (:one) after every inner step, order preserved."
  (should (equal (dorg-ql-normalize
                  '(one (pipe (where (tag "x")) (traverse children))))
                 '(:pipe (:source :all)
                         (:filter (:tag = "x" :case sensitive))
                         (:traverse children)
                         (:one)))))

(ert-deftest dorg-test-ql-ast-one-arity ()
  "(one QUERY) takes exactly one inner query."
  (should-error (dorg-ql-normalize '(one)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(one (id 1) (id 2))) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-one-not-a-bare-pipeline-step ()
  "Bare `one' inside (pipe ...) is NOT public grammar (single spelling)."
  (should-error (dorg-ql-normalize '(pipe (where (id 1)) (one)))
                :type 'dorg-ql-error))

;;; ─── Compatibility alias normalization ────────────────────────────────

(ert-deftest dorg-test-ql-ast-alias-id ()
  "(id ID) lowers to the identity node; numeric strings normalize."
  (should (equal (dorg-test-ql-ast--filter '(id 7)) '(:id = 7)))
  (should (equal (dorg-test-ql-ast--filter '(id "42")) '(:id = 42))))

(ert-deftest dorg-test-ql-ast-alias-property ()
  "(property KEY VALUE) lowers to an equality property node."
  (should (equal (dorg-test-ql-ast--filter '(property "CATEGORY" "red"))
                 '(:property "CATEGORY" = "red" :case sensitive))))

(ert-deftest dorg-test-ql-ast-alias-title ()
  "(title TITLE) lowers to a title field comparison."
  (should (equal (dorg-test-ql-ast--filter '(title "Hello"))
                 '(:cmp title = "Hello" :case sensitive))))

(ert-deftest dorg-test-ql-ast-alias-tag ()
  "(tag TAG) lowers to a tag membership node."
  (should (equal (dorg-test-ql-ast--filter '(tag "work"))
                 '(:tag = "work" :case sensitive))))

(ert-deftest dorg-test-85e9w-ql-ast-extension-member-is-one-typed-leaf ()
  "The bounded extension membership reader has one exact normalized shape."
  (should
   (equal
    (dorg-test-ql-ast--filter
     '(extension-member demo assignee-at 1 ("alice" "")))
    '(:extension-member demo assignee-at 1 ("alice" "")))))

(ert-deftest dorg-test-85e9w-ql-ast-extension-member-composes-in-booleans ()
  "Membership is an ordinary boolean leaf, not a top-level execution branch."
  (should
   (equal
    (dorg-test-ql-ast--filter
     '(or (extension-member demo assignee-at 1 ("alice"))
          (and (exists (property "PAUSED"))
               (extension-member demo assignee-at 1 (""))
               (extension-member demo assignee-at 2 ("alice")))))
    '(:or
      (:extension-member demo assignee-at 1 ("alice"))
      (:and (:property "PAUSED" exists nil)
            (:extension-member demo assignee-at 1 (""))
            (:extension-member demo assignee-at 2 ("alice")))))))

(ert-deftest dorg-test-85e9w-ql-ast-extension-member-refuses-malformed-forms ()
  "Malformed membership never normalizes into a broad or empty query."
  (dolist (query '((extension-member demo assignee-at 0 ("alice"))
                   (extension-member demo assignee-at 1 ())
                   (extension-member demo assignee-at 1 ("alice" 7))
                   (extension-member "demo" assignee-at 1 ("alice"))
                   (extension-member demo "assignee-at" 1 ("alice"))
                   (extension-member demo assignee-at 1 "alice")
                   (extension-member demo assignee-at 1 ("alice") :extra)))
    (should-error (dorg-ql-normalize query) :type 'dorg-ql-error)))

(ert-deftest dorg-test-ql-ast-alias-timestamp-windows ()
  "Planning/body/logbook aliases lower to :timestamp :overlaps nodes."
  (should (equal (dorg-test-ql-ast--filter '(scheduled "2026-05-01" "2026-05-31"))
                 '(:timestamp scheduled :overlaps ((2026 5 1) . (2026 5 31)))))
  (should (equal (dorg-test-ql-ast--filter '(deadline nil "2026-12-31"))
                 '(:timestamp deadline :overlaps (nil . (2026 12 31)))))
  (should (equal (dorg-test-ql-ast--filter '(closed "2026-01-01" nil))
                 '(:timestamp closed :overlaps ((2026 1 1)))))
  (should (equal (dorg-test-ql-ast--filter '(active-ts nil nil))
                 '(:timestamp active-ts :overlaps (nil))))
  (should (equal (dorg-test-ql-ast--filter '(logbook "2026-05-01" "2026-05-31"))
                 '(:timestamp logbook :overlaps ((2026 5 1) . (2026 5 31))))))

(ert-deftest dorg-test-ql-ast-alias-property-ts ()
  "(property-ts FROM TO [NAME]) lowers to a (:property KEY|*) timestamp node."
  (should (equal (dorg-test-ql-ast--filter '(property-ts "2026-07-01" "2026-07-31"))
                 '(:timestamp (:property *) :overlaps ((2026 7 1) . (2026 7 31)))))
  (should (equal (dorg-test-ql-ast--filter '(property-ts nil nil "DUE"))
                 '(:timestamp (:property "DUE") :overlaps (nil)))))

(ert-deftest dorg-test-ql-ast-alias-window-forms ()
  "Timestamp aliases accept a single window form via the shared normalizer.
The atomic aliases must lower the same window syntax core's window
normalizer (`dorg-ql--normalize-window-spec') accepts --- `(HEAD
WINDOW-FORM)' and `(property-ts WINDOW-FORM [NAME])' --- not only explicit
`(HEAD FROM TO)' bounds (regression)."
  (let ((dorg-ql-current-time (encode-time (list 0 0 12 3 6 2026 nil -1 nil))))
    (should (equal (dorg-test-ql-ast--filter '(scheduled (today)))
                   '(:timestamp scheduled :overlaps ((2026 6 3) . (2026 6 3)))))
    (should (equal (dorg-test-ql-ast--filter '(deadline (this-week)))
                   '(:timestamp deadline :overlaps ((2026 6 1) . (2026 6 7))))))
  (should (equal (dorg-test-ql-ast--filter '(closed (on "2026-07-01")))
                 '(:timestamp closed :overlaps ((2026 7 1) . (2026 7 1)))))
  (should (equal (dorg-test-ql-ast--filter '(active-ts (before "2026-06-30")))
                 '(:timestamp active-ts :overlaps (nil . (2026 6 30)))))
  (should (equal (dorg-test-ql-ast--filter '(logbook (after "2026-06-01")))
                 '(:timestamp logbook :overlaps ((2026 6 1)))))
  ;; property-ts: window form alone, and window form with a trailing NAME.
  (should (equal (dorg-test-ql-ast--filter '(property-ts (on "2026-07-01")))
                 '(:timestamp (:property *) :overlaps ((2026 7 1) . (2026 7 1)))))
  (should (equal (dorg-test-ql-ast--filter
                  '(property-ts (between "2026-07-01" "2026-07-31") "DUE"))
                 '(:timestamp (:property "DUE") :overlaps ((2026 7 1) . (2026 7 31))))))

(ert-deftest dorg-test-ql-ast-alias-window-equiv-v2 ()
  "An alias window form and its explicit v2 `timestamp' spelling agree."
  (should (equal (dorg-ql-normalize '(scheduled (on "2026-07-01")))
                 (dorg-ql-normalize
                  '(timestamp scheduled :overlaps (on "2026-07-01")))))
  (should (equal (dorg-ql-normalize '(property-ts (on "2026-07-01") "DUE"))
                 (dorg-ql-normalize
                  '(timestamp (:property "DUE") :overlaps (on "2026-07-01"))))))

(ert-deftest dorg-test-ql-ast-alias-equiv-v2 ()
  "An alias and its explicit v2 spelling normalize identically."
  (should (equal (dorg-ql-normalize '(id 7))
                 (dorg-ql-normalize '(id = 7))))
  (should (equal (dorg-ql-normalize '(id 7))
                 (dorg-ql-normalize '(field id 7))))
  (should (equal (dorg-ql-normalize '(property "K" "v"))
                 (dorg-ql-normalize '(= (property "K") "v"))))
  (should (equal (dorg-ql-normalize '(tag "work"))
                 (dorg-ql-normalize '(= (tag) "work")))))

;;; ─── v2 predicate normalization ───────────────────────────────────────

(ert-deftest dorg-test-ql-ast-id-operators ()
  "(id OP VALUE) keeps the operator and normalizes VALUE to an integer."
  (should (equal (dorg-test-ql-ast--filter '(id < "10")) '(:id < 10)))
  (should (equal (dorg-test-ql-ast--filter '(id >= 3)) '(:id >= 3))))

(ert-deftest dorg-test-ql-ast-field-comparisons ()
  "Prefix field operators and equality atoms normalize value types."
  (should (equal (dorg-test-ql-ast--filter '(contains (field title) "foo"))
                 '(:cmp title contains "foo" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(> (field level) "2"))
                 '(:cmp level > 2)))
  (should (equal (dorg-test-ql-ast--filter '(field commented t))
                 '(:cmp commented = t)))
  (should (equal (dorg-test-ql-ast--filter '(>= (field priority) "B"))
                 '(:cmp priority >= "B")))
  (should (equal (dorg-test-ql-ast--filter `(>= (field priority) ,?B))
                 '(:cmp priority >= "B"))))

(ert-deftest dorg-test-ql-ast-field-id-folds-to-identity ()
  "(field id ...) folds into the dedicated :id node, not a :cmp node."
  (should (equal (dorg-test-ql-ast--filter '(field id 5)) '(:id = 5))))

(ert-deftest dorg-test-ql-ast-property-operators ()
  "Prefix property forms cover equality, string ops, and existence."
  (should (equal (dorg-test-ql-ast--filter '(= (property "K") "v"))
                 '(:property "K" = "v" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(prefix (property "K") "v"))
                 '(:property "K" prefix "v" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(exists (property "K")))
                 '(:property "K" exists nil)))
  (should (equal (dorg-test-ql-ast--filter '(missing (property "K")))
                 '(:property "K" missing nil))))

(ert-deftest dorg-test-ql-ast-tag-operators ()
  "Prefix tag forms cover operators and existence."
  (should (equal (dorg-test-ql-ast--filter '(= (tag) "work"))
                 '(:tag = "work" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(suffix (tag) "x"))
                 '(:tag suffix "x" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(exists (tag)))
                 '(:tag exists nil))))

(ert-deftest dorg-test-ql-ast-keyword-operators ()
  "Keyword predicates are normalized as heading-scoped keyword leaves."
  (should (equal (dorg-test-ql-ast--filter '(keyword "EXPORT_FILE_NAME"))
                 '(:keyword "EXPORT_FILE_NAME" exists nil)))
  (should (equal (dorg-test-ql-ast--filter '(keyword "EXPORT_FILE_NAME" "alpha"))
                 '(:keyword "EXPORT_FILE_NAME" = "alpha" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(= (keyword "EXPORT_FILE_NAME") "alpha"))
                 '(:keyword "EXPORT_FILE_NAME" = "alpha" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(contains (keyword "EXPORT_FILE_NAME") "alp"))
                 '(:keyword "EXPORT_FILE_NAME" contains "alp" :case sensitive)))
  (should (equal (dorg-test-ql-ast--filter '(missing (keyword "EXPORT_FILE_NAME")))
                 '(:keyword "EXPORT_FILE_NAME" missing nil))))

(ert-deftest dorg-test-ql-ast-timestamp-form ()
  "(timestamp SOURCE REL WINDOW) normalizes source, relation, and span."
  (should (equal (dorg-test-ql-ast--filter
                  '(timestamp scheduled :overlaps (between "2026-01-01" "2026-12-31")))
                 '(:timestamp scheduled :overlaps ((2026 1 1) . (2026 12 31)))))
  (should (equal (dorg-test-ql-ast--filter
                  '(timestamp (:property "DUE") :overlaps (on "2026-07-01")))
                 '(:timestamp (:property "DUE") :overlaps ((2026 7 1) . (2026 7 1)))))
  (should (equal (dorg-test-ql-ast--filter
                  '(timestamp deadline :overlaps (before "2026-06-30")))
                 '(:timestamp deadline :overlaps (nil . (2026 6 30)))))
  (should (equal (dorg-test-ql-ast--filter
                  '(timestamp logbook :overlaps (after "2026-06-01")))
                 '(:timestamp logbook :overlaps ((2026 6 1))))))

;;; ─── Boolean combinators ──────────────────────────────────────────────

(ert-deftest dorg-test-ql-ast-boolean-nodes ()
  "and/or/not lower to their AST nodes with normalized operands."
  (should (equal (dorg-test-ql-ast--filter '(and (tag "a") (tag "b")))
                 '(:and (:tag = "a" :case sensitive) (:tag = "b" :case sensitive))))
  (should (equal (dorg-test-ql-ast--filter '(or (id 1) (id 2)))
                 '(:or (:id = 1) (:id = 2))))
  (should (equal (dorg-test-ql-ast--filter '(not (tag "x")))
                 '(:not (:tag = "x" :case sensitive))))
  ;; Nesting recurses through the predicate normalizer.
  (should (equal (dorg-test-ql-ast--filter
                  '(and (tag "u") (not (property "DONE" "t"))))
                 '(:and (:tag = "u" :case sensitive)
                         (:not (:property "DONE" = "t" :case sensitive))))))

(ert-deftest dorg-test-ql-ast-boolean-arity ()
  "Boolean combinators enforce their arity."
  (should-error (dorg-ql-normalize '(and)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(or)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(not)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(not (id 1) (id 2))) :type 'dorg-ql-error)
  ;; A single operand is valid for and/or.
  (should (equal (dorg-test-ql-ast--filter '(and (id 1))) '(:and (:id = 1))))
  (should (equal (dorg-test-ql-ast--filter '(or (id 1))) '(:or (:id = 1)))))

;;; ─── Field / operator validation ──────────────────────────────────────

(ert-deftest dorg-test-ql-ast-unknown-field-errors ()
  "An unknown field name signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(= (field bogus) "x")) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-bad-operator-errors ()
  "An operator outside a predicate's allowed set signals `dorg-ql-error'."
  ;; `<' is not valid for a string title field.
  (should-error (dorg-ql-normalize '(< (field title) "x")) :type 'dorg-ql-error)
  ;; Legacy priority-specific operators are removed.
  (should-error (dorg-ql-normalize '(:at-or-above (field priority) "B"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(:below (field priority) "B"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(property "K" bogus "v")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(tag bogus "v")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(id bogus 1)) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-bad-value-types-error ()
  "Wrong value types for a field signal `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(field id "not-numeric")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(field commented "x")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(field title 5)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(contains (property "K") 5)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(keyword "K" 5)) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-old-middle-operator-forms-error ()
  "Removed middle-operator forms fail instead of staying compatibility aliases."
  (should-error (dorg-ql-normalize '(field title contains "foo"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(field level > "2"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(property "K" prefix "v"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(property "K" exists))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(tag suffix "x"))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(tag exists))
                :type 'dorg-ql-error))

;;; ─── Arity / shape errors ─────────────────────────────────────────────

(ert-deftest dorg-test-ql-ast-non-list-errors ()
  "A non-list query form signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize 'id) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize 42) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-unknown-predicate-errors ()
  "An unknown predicate head signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(between 1 2)) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-arity-errors ()
  "Wrong arity for predicates and aliases signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(id 1 2 3)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(field title)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(property "K")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(tag)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(keyword)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(field title "x" "extra")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(timestamp scheduled)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(title)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(scheduled "2026-01-01")) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(property-ts "a" "b" "c" "d")) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-bad-window-errors ()
  "Malformed window bounds signal `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(scheduled "2026-5-1" nil)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize
                 '(timestamp scheduled :overlaps (between "bad" nil)))
                :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-unknown-timestamp-source-and-rel ()
  "Unknown timestamp source or relation signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(timestamp bogus :overlaps (on "2026-01-01")))
                :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(timestamp scheduled :within (on "2026-01-01")))
                :type 'dorg-ql-error))

;;; ─── Pipeline step validation ─────────────────────────────────────────

(ert-deftest dorg-test-ql-ast-pipeline-step-at-top-errors ()
  "A bare pipeline step at top level signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(where (id 1))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(traverse children)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(unique)) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-pipe-validation ()
  "Pipe steps and their arity are validated."
  (should-error (dorg-ql-normalize '(pipe)) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (id 1))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (where))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (where (id 1) (id 2)))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (unique x))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (traverse))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(pipe (traverse cousins))) :type 'dorg-ql-error))

(ert-deftest dorg-test-ql-ast-pipeline-step-not-a-predicate ()
  "A pipeline step used as a predicate operand signals `dorg-ql-error'."
  (should-error (dorg-ql-normalize '(and (where (id 1)))) :type 'dorg-ql-error)
  (should-error (dorg-ql-normalize '(not (unique))) :type 'dorg-ql-error))

;;; ─── Window-span delegation to core ───────────────────────────────────

(ert-deftest dorg-test-ql-ast-relative-window-delegates-to-core ()
  "Relative/convenience windows resolve via the dorg-ql-core normalizer.
The AST no longer owns a window seam: `today'/`this-week' expand through
`dorg-ql--normalize-window-form', made deterministic here by binding
`dorg-ql-current-time' (2026-06-03 is a Wednesday; the Monday-start week
is 2026-06-01..2026-06-07)."
  (let ((dorg-ql-current-time (encode-time (list 0 0 12 3 6 2026 nil -1 nil))))
    (should (equal (dorg-test-ql-ast--filter
                    '(timestamp scheduled :overlaps (today)))
                   '(:timestamp scheduled :overlaps ((2026 6 3) . (2026 6 3)))))
    (should (equal (dorg-test-ql-ast--filter
                    '(timestamp deadline :overlaps (this-week)))
                   '(:timestamp deadline :overlaps ((2026 6 1) . (2026 6 7))))))
  ;; An unrecognized window form still errors --- normalization never widens.
  (should-error (dorg-ql-normalize '(timestamp scheduled :overlaps (bogus 1)))
                :type 'dorg-ql-error))

;;; ─── Explicit literal case mode (xeq5v) ───────────────────────────────

(ert-deftest dorg-test-xeq5v-case-normalizes-once ()
  "Every eligible string leaf carries one canonical `:case' mode.
Omission and explicit `sensitive' are deliberately byte-identical; this is
the AST/cache identity contract, not merely an evaluator default."
  (dolist (entry
           '(((title "Work")
              (:cmp title = "Work" :case sensitive))
             ((title "Work" :case sensitive)
              (:cmp title = "Work" :case sensitive))
             ((contains (field title) "work" :case insensitive)
              (:cmp title contains "work" :case insensitive))
             ((= (property "CATEGORY") "Work" :case insensitive)
              (:property "CATEGORY" = "Work" :case insensitive))
             ((prefix (keyword "CAPTION") "Draft" :case insensitive)
              (:keyword "CAPTION" prefix "Draft" :case insensitive))
             ((!=(tag) "ARCHIVE" :case insensitive)
              (:tag != "ARCHIVE" :case insensitive))))
    (should (equal (dorg-test-ql-ast--filter (car entry)) (cadr entry)))))

(ert-deftest dorg-test-xeq5v-case-option-refusals ()
  "Malformed or inapplicable `:case' tails refuse during normalization."
  (dolist (query
           '((title "x" :case folded)
             (title "x" :case insensitive :case sensitive)
             (title "x" :case)
             (field id 1 :case insensitive)
             (field priority "A" :case insensitive)
             (property "K" "V" :case insensitive extra)
             (exists (property "K") :case insensitive)
             (timestamp scheduled :overlaps (on "2026-01-01") :case insensitive)))
    (should-error (dorg-ql-normalize query) :type 'dorg-ql-error)))

;;; ─── contains-any reader sugar ──────────────────────────────────────

(ert-deftest dorg-test-ql-ast-contains-any-is-or-of-contains ()
  "`contains-any' normalizes to exactly the long OR of `contains' leaves.
The sugar adds no AST operator: the filter is `equal' to the long form's,
needle order and duplicates included, for both explicit case modes."
  (dolist (mode '(sensitive insensitive))
    (should (equal (dorg-test-ql-ast--filter
                    `(contains-any (field title) ("ontology" "researcher" "roles" "roles")
                                   :case ,mode))
                   (dorg-test-ql-ast--filter
                    `(or (contains (field title) "ontology" :case ,mode)
                         (contains (field title) "researcher" :case ,mode)
                         (contains (field title) "roles" :case ,mode)
                         (contains (field title) "roles" :case ,mode)))))
    (should (equal (dorg-test-ql-ast--filter
                    `(contains-any (field title) ("b" "a") :case ,mode))
                   `(:or (:cmp title contains "b" :case ,mode)
                         (:cmp title contains "a" :case ,mode))))))

(ert-deftest dorg-test-ql-ast-contains-any-default-case-is-contains-default ()
  "Without `:case', every leaf takes exactly the plain `contains' default."
  (let ((default (dorg-test-ql-ast--filter '(contains (field title) "x"))))
    (should (equal (dorg-test-ql-ast--filter '(contains-any (field title) ("x")))
                   (list :or default)))
    (should (equal (dorg-test-ql-ast--filter '(contains-any (field title) ("x" "y")))
                   (dorg-test-ql-ast--filter '(or (contains (field title) "x")
                                                  (contains (field title) "y")))))))

(ert-deftest dorg-test-ql-ast-contains-any-empty-needle-is-contains-empty ()
  "An empty needle is legal and means what (contains EXPR \"\") means."
  (should (equal (dorg-test-ql-ast--filter '(contains-any (field title) ("")))
                 (list :or (dorg-test-ql-ast--filter '(contains (field title) ""))))))

(ert-deftest dorg-test-ql-ast-contains-any-shares-contains-operands ()
  "Every operand `contains' accepts is accepted, with the same leaf shape."
  (dolist (operand '((property "COLOR") (keyword "AUTHOR") (tag)))
    (should (equal (dorg-test-ql-ast--filter
                    `(contains-any ,operand ("a" "b") :case insensitive))
                   (dorg-test-ql-ast--filter
                    `(or (contains ,operand "a" :case insensitive)
                         (contains ,operand "b" :case insensitive)))))))

(ert-deftest dorg-test-ql-ast-contains-any-empty-list-refused-like-or ()
  "An empty needle list is the same typed invalid query as an empty (or)."
  (let ((empty-or (should-error (dorg-ql-normalize '(or)) :type 'dorg-ql-error))
        (empty-any (should-error (dorg-ql-normalize '(contains-any (field title) ()))
                                 :type 'dorg-ql-error)))
    (should (eq 'invalid-query (dorg-error-reason empty-or)))
    (should (eq (dorg-error-reason empty-or) (dorg-error-reason empty-any)))))

(ert-deftest dorg-test-ql-ast-contains-any-refuses-malformed-forms ()
  "Arity, needle-list shape, needle type and case tail fail before any source."
  (dolist (query '((contains-any (field title))
                   (contains-any (field title) "ontology")
                   (contains-any (field title) ("a" . "b"))
                   (contains-any (field title) ("a" 1))
                   (contains-any (field title) ("a") :case folded)
                   (contains-any (field title) ("a") :case insensitive :case sensitive)
                   (contains-any (field level) ("1"))
                   (contains-any (field bogus) ("a"))))
    (should-error (dorg-ql-normalize query) :type 'dorg-ql-error))
  ;; The insensitive raw-byte literal refusal is the plain contains refusal.
  (let ((raw (unibyte-string #xc3)))
    (should-error (dorg-ql-normalize `(contains (field title) ,raw :case insensitive))
                  :type 'dorg-ql-error)
    (should-error (dorg-ql-normalize `(contains-any (field title) (,raw) :case insensitive))
                  :type 'dorg-ql-error)))

(provide 'dorg-ql-ast-tests)
;;; dorg-ql-ast-tests.el ends here
