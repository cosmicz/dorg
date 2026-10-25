;;; dorg-api-tests.el --- Public mutate API (dorg-api.el) SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED SPEC for the v0.1 public mutate surface: the
;; `dorg-api.el' layer that reads the public DSL, normalizes it into the
;; canonical typed patch IR, and wraps the native typed engine in the
;; mode/safety command envelope.
;;
;; SOURCE OF TRUTH is the ratified docs — docs/mutate.org, docs/api.org,
;; docs/query.org — NOT the (stale) bead Design section.  In particular the
;; keyword surface here is the corrected one: one-based `(occurrence N)',
;; `(after-last)', `(replace|delete keywords KEY …)' whole-sequence ops, and
;; `(replace|delete keyword KEY (occurrence N))' strict in-place ops; there is
;; NO public `(set keyword …)'.
;;
;; Tests use boundary doubles where a live store is irrelevant so the API-layer
;; contract (normalize, guard, reshape) stays pinned in isolation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-api nil t)                ; RED: module not implemented yet

;; API-only tests that double the query and apply engine still take the real
;; writer gate.  Each call gets a nonexistent name under the test temp root;
;; its parent can host a lock without a load-time resource or exit hook.
(defun dorg-test-api--unit-file ()
  "Return a fresh nonexistent source in a writable temp directory."
  (concat (make-temp-name
           (expand-file-name (format "dorg-test-api-unit-%s-" (emacs-pid))
                             temporary-file-directory))
          ".org"))

(defmacro dorg-test-api--with-unit-files (&rest body)
  "Run BODY and check that its synthetic source locks are reclaimed."
  (declare (indent 0))
  `(let (unit-files body-completed)
     (cl-flet ((dorg-test-api--unit-file ()
                 (let ((file (dorg-test-api--unit-file)))
                   (push file unit-files)
                   file)))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq body-completed t))
         (let (leftovers)
           (dolist (file unit-files)
             (let ((lock (dorg-file-lock-path file)))
               (dolist (path (list lock (concat lock ".reclaim")))
                 (when (file-attributes path 'string)
                   (push path leftovers)
                   (ignore-errors (delete-file path))))))
           (when body-completed
             (should-not leftovers)))))))

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun dorg-test-api--patches (form &optional source)
  "Normalize FORM (against SOURCE) and return its typed patch IR list."
  (dorg-api-normalize form source))

(defun dorg-test-api--1 (form &optional source)
  "Normalize FORM to exactly one typed patch and return it."
  (let ((ps (dorg-test-api--patches form source)))
    (should (= 1 (length ps)))
    (car ps)))

(defun dorg-test-api--reason (thunk)
  "Call THUNK, expect a `dorg-api-error', return its :reason symbol."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-api-error (dorg-api-error-reason err))))

(defmacro dorg-test-api--should-reason (reason &rest body)
  "Assert BODY signals `dorg-api-error' with :reason REASON."
  (declare (indent 1))
  `(should (eq ,reason (dorg-test-api--reason (lambda () ,@body)))))

(defconst dorg-test-api--extern-host-a
  '(:heading :extern t :key "BEAD_ID" :value "task-a")
  "Canonical external host for API identity-key tests.")

(defconst dorg-test-api--extern-host-parent
  '(:heading :extern t :key "BEAD_ID" :value "parent")
  "Canonical external parent host for API identity-key tests.")

(defconst dorg-test-api--extern-host-anchor
  '(:heading :extern t :key "BEAD_ID" :value "anchor")
  "Canonical external sibling-anchor host for API identity-key tests.")

(defun dorg-test-api--extern-source (&optional file)
  "Return a source descriptor using BEAD_ID as its public identity key."
  (list :file (or file "/tmp/dorg-api-extern-key.org")
        :identity '(:property "BEAD_ID")))

(defmacro dorg-test-api--with-extern-key-hits (&rest body)
  "Run BODY with BEAD_ID key queries resolved to deterministic extern hits."
  (declare (indent 0))
  `(let ((source (dorg-test-api--extern-source)))
     (cl-letf (((symbol-function 'dorg-ql-select)
                (lambda (query &rest _)
                  (pcase query
                    (`(property "BEAD_ID" "task-a")
                     '((:heading-id 101
                        :properties (("BEAD_ID" . "task-a")))))
                    (`(property "BEAD_ID" "parent")
                     '((:heading-id 201
                        :properties (("BEAD_ID" . "parent")))))
                    (`(property "BEAD_ID" "anchor")
                     '((:heading-id 202
                        :properties (("BEAD_ID" . "anchor")))))
                    (`(property "BEAD_ID" "dup")
                     '((:heading-id 301
                        :properties (("BEAD_ID" . "dup")))
                       (:heading-id 302
                        :properties (("BEAD_ID" . "dup")))))
                    (_ nil)))))
       ,@body)))

(defun dorg-test-api--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun dorg-test-api--heading-property (heading key)
  "Return HEADING's PROPERTIES drawer value for KEY."
  (cl-loop for item in (dorg-heading-items heading)
           when (and (cl-typep item 'dorg-drawer-item)
                     (equal (dorg-drawer-item-name item) "PROPERTIES"))
           return (cdr (assoc key (dorg-drawer-item-properties item)))))

(defun dorg-test-api--heading-by-property (headings key value)
  "Return the unique heading in HEADINGS with PROPERTIES KEY equal to VALUE."
  (let (matches)
    (maphash (lambda (_id h)
               (when (equal value (dorg-test-api--heading-property h key))
                 (push h matches)))
             headings)
    (should (= 1 (length matches)))
    (car matches)))

(defun dorg-test-api--child-titles (headings parent-id)
  "Return direct child titles of PARENT-ID from HEADINGS in sibling order."
  (let (children)
    (maphash (lambda (_id heading)
               (when (equal parent-id (dorg-heading-parent-id heading))
                 (push heading children)))
             headings)
    (mapcar #'dorg-heading-title
            (sort children
                  (lambda (a b)
                    (string< (dorg-heading-sort-key a)
                             (dorg-heading-sort-key b)))))))

(defun dorg-test-api--mutation-reason+detail (thunk)
  "Call THUNK, returning (REASON . DETAIL) for API or mutate errors."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-api-error
     (cons (dorg-api-error-reason err)
           (plist-get (dorg-api-error-data err) :detail)))
    (dorg-mutate-error
     (cons (dorg-mutate-error-reason err)
           (plist-get (dorg-mutate-error-data err) :detail)))))

(defun dorg-test-api--dolt-commit-count (dolt-dir)
  "Return the number of commits reachable from DOLT-DIR's HEAD."
  (alist-get 'cnt
             (car (dorg-sql dolt-dir
                             "SELECT COUNT(*) AS cnt FROM dolt_log"))))

;;; ─── Shared public capability contract ───────────────────────────────────

;; This is deliberately data, not a second dispatcher.  It records which
;; existing public-door controls may be credited for a backend/source cell.
;; Keep pending feature routing out of this source: a pending row describes the
;; public capability and its typed detail, never the work item that will land it.
(defconst dorg-test-api--capability-matrix
  '((:key keyword-heading-existing-buffer
     :form (on (id 1001) (insert (keyword "K") (after-last) "c"))
     :source-kind buffer :identity-policy dorg :status supported
     :oracle-kind transitive-independent
     :expected ((keyword "K" "a") (keyword "OTHER" "middle")
                (keyword "K" "b") (keyword "K" "c") (text "Tail."))
     :selector "^dorg-test-mutate-public-keyword-insert-.*"
     :mutant (:executor plain-splice
              :selector "^dorg-test-api-capability-plain-splice-noop-mutant-is-observable$"
              :test dorg-test-api-capability-plain-splice-noop-mutant-is-observable)
     :tests (dorg-test-mutate-public-keyword-insert-backend-parity
             dorg-test-mutate-public-keyword-insert-plain-heading-placement))
    (:key keyword-heading-missing-buffer
     :form (on (id 1002) (insert (keyword "K") (after-last) "c"))
     :source-kind buffer :identity-policy dorg :status supported
     :oracle-kind transitive-independent
     :expected ((keyword "OTHER" "middle") (text "Tail.") (keyword "K" "c"))
     :selector "^dorg-test-mutate-public-keyword-insert-.*"
     :tests (dorg-test-mutate-public-keyword-insert-backend-parity
             dorg-test-mutate-public-keyword-insert-plain-heading-placement))
    (:key keyword-heading-existing-dolt
     ;; The real fixture allocates these IDs by title.  200/201 distinguish
     ;; the existing/missing logical hosts; they are not fixture literals.
     :form (on (id 200) (insert (keyword "K") (after-last) "c"))
     :source-kind dolt :identity-policy dorg :status supported
     :oracle-kind independent
     :expected ((keyword "K" "a") (keyword "OTHER" "middle")
                (keyword "K" "b") (keyword "K" "c") (text "Tail."))
     :selector "^dorg-test-mutate-public-keyword-insert-backend-parity$"
     :mutant (:executor dolt-write
              :selector "^dorg-test-api-capability-dolt-write-noop-mutant-is-observable$"
              :test dorg-test-api-capability-dolt-write-noop-mutant-is-observable)
     :tests (dorg-test-mutate-public-keyword-insert-backend-parity))
    (:key keyword-heading-missing-dolt
     :form (on (id 201) (insert (keyword "K") (after-last) "c"))
     :source-kind dolt :identity-policy dorg :status supported
     :oracle-kind independent
     :expected ((keyword "OTHER" "middle") (text "Tail.") (keyword "K" "c"))
     :selector "^dorg-test-mutate-public-keyword-insert-backend-parity$"
     :tests (dorg-test-mutate-public-keyword-insert-backend-parity))
    (:key keyword-document-dolt
     :form (on document (insert (keyword "K") (after-last) "root-c"))
     :source-kind dolt :identity-policy dorg :status supported
     :oracle-kind independent
     :expected ((keyword "K" "root-a") (keyword "OTHER" "root-middle")
                (keyword "K" "root-b") (keyword "K" "root-c")
                (text "Root tail."))
     :selector "^dorg-test-mutate-public-keyword-insert-backend-parity$"
     :tests (dorg-test-mutate-public-keyword-insert-backend-parity))
    (:key keyword-document-buffer
     :form (on document (insert (keyword "K") (after-last) "root-c"))
     :source-kind buffer :identity-policy dorg :status refused
     :refusal (unsupported . no-source-preserving-editor)
     :selector "^dorg-test-mutate-public-keyword-insert-plain-heading-placement$"
     :tests (dorg-test-mutate-public-keyword-insert-plain-heading-placement))
    (:key property-extern-file
     :form (on (where (property "BEAD_ID" "a-1"))
               (set (property "STATUS") "DONE"))
     :source-kind file :identity-policy (:property "BEAD_ID") :status supported
     :oracle-kind independent
     :expected dorg-test-idp--prop-fixture-after
     :selector "^dorg-test-idp-dry-run-apply-same-intent$"
     :tests (dorg-test-idp-dry-run-apply-same-intent))
    (:key text-insert-buffer
     :form (on (id 1001) (insert (text) (after-last) "Appended note."))
     :source-kind buffer :identity-policy dorg :status pending
     :oracle-kind substring :pending-detail independent-oracle-required
     :selector "^dorg-test-2rx9n-api-exec-insert-text-plain-org-preserves$"
     :tests (dorg-test-2rx9n-api-exec-insert-text-plain-org-preserves))
    (:key text-replace-dolt
     :form (on (id 3001) (replace (text (occurrence 1)) "Rewritten via API."))
     :source-kind dolt :identity-policy dorg :status pending
     :oracle-kind substring :pending-detail independent-oracle-required
     :selector "^dorg-test-2rx9n-api-exec-replace-text-dolt-preserves$"
     :tests (dorg-test-2rx9n-api-exec-replace-text-dolt-preserves))
    (:key identity-none-file
     :form (on (key "a-1") (set (property "STATUS") "X"))
     :source-kind file :identity-policy none :status refused
     :refusal (unsupported . identity-none)
     :selector "^dorg-test-idp-parity-none-uniform-refusal$"
     :tests (dorg-test-idp-parity-none-uniform-refusal))
    (:key heading-title-buffer
     :form (on (key "line-a") (set (heading title) "A much longer λ title"))
     :source-kind buffer :identity-policy dorg :status supported
     :oracle-kind independent
     :expected "*  TODO   [#B]  COMMENT A much longer λ title\t  :alpha:beta:  \n"
     :selector "^dorg-test-1qdtb-title$" :tests (dorg-test-1qdtb-title))
    (:key heading-title-file
     :form (on (key "line-a") (set (heading title) "A much longer λ title"))
     :source-kind file :identity-policy dorg :status supported
     :oracle-kind independent
     :expected "*  TODO   [#B]  COMMENT A much longer λ title\t  :alpha:beta:  \n"
     :selector "^dorg-test-1qdtb-title$" :tests (dorg-test-1qdtb-title))
    (:key heading-line-unrepresentable-buffer
     :form (on (key "line-a") (set (heading priority) "AA"))
     :source-kind buffer :identity-policy dorg :status refused
     :refusal (invalid-target . heading-line-unrepresentable)
     :selector "^dorg-test-1qdtb-unrepresentable-fields-refuse-before-apply$"
     :tests (dorg-test-1qdtb-unrepresentable-fields-refuse-before-apply))
    ;; These tests are retained in the inventory but have no literal oracle;
    ;; they are not backend-creditable until one is added.
    (:key query-property-plain
     :form (query (where (property "FOO" "bar")))
     :source-kind buffer :identity-policy dorg :status pending
     :oracle-kind backend-vs-model :pending-detail independent-oracle-required
     :selector "^dorg-test-ql-properties-parity-plain-vs-model$"
     :tests (dorg-test-ql-properties-parity-plain-vs-model))
    (:key query-property-dolt
     :form (query (where (property "FOO" "bar")))
     :source-kind dolt :identity-policy dorg :status pending
     :oracle-kind backend-vs-backend :pending-detail independent-oracle-required
     :selector "^dorg-test-ql-properties-parity-plain-vs-dolt$"
     :tests (dorg-test-ql-properties-parity-plain-vs-dolt))
    (:key planning-extern-file
     :form (on (where (property "BEAD_ID" "a-1"))
               (set (planning scheduled) "<2026-10-15 Thu>"))
     :source-kind file :identity-policy (:property "BEAD_ID") :status supported
     :oracle-kind independent :expected dorg-test-idp--planning-fixture-after
     :selector "^dorg-test-idp-update-planning-supported$"
     :tests (dorg-test-idp-update-planning-supported))
    (:key capture-heading-file
     :form (insert (heading (title "Captured")) (under (id 1)))
     :source-kind file :identity-policy dorg :status pending
     :pending-detail no-source-preserving-editor :selector nil :tests nil))
  "Single test-side public capability contract for plain Org and Dolt.")

(defun dorg-test-api--capability-test-names (selector)
  "Return ERT names selected by SELECTOR, without running those tests."
  (mapcar #'ert-test-name (ert-select-tests selector t)))

(defun dorg-test-api--capability-expected-p (expected)
  "Return non-nil when EXPECTED is a literal or a bound literal constant."
  (if (and (symbolp expected) (boundp expected))
      (symbol-value expected)
    expected))

(ert-deftest dorg-test-api-capability-matrix-is-well-formed ()
  "The public capability matrix has usable, non-creditable pending cells.
This deliberately server-free test verifies metadata only; it never invokes a
named control or creates a Dolt fixture."
  (let (keys mutant-executors)
    (dolist (cell dorg-test-api--capability-matrix)
      (let ((key (plist-get cell :key))
            (status (plist-get cell :status))
            (tests (plist-get cell :tests))
            (selector (plist-get cell :selector)))
        (dolist (field '(:key :form :source-kind :identity-policy :status))
          (should (plist-get cell field)))
        (should (memq (plist-get cell :source-kind) '(buffer file dolt)))
        (should (memq status '(supported refused pending)))
        (should-not (memq key keys))
        (push key keys)
        (dolist (test tests)
          (should (ert-test-boundp test))
          (should selector)
          (should (memq test (dorg-test-api--capability-test-names selector))))
        (when-let ((mutant (plist-get cell :mutant)))
          (let ((executor (plist-get mutant :executor))
                (test (plist-get mutant :test))
                (mutant-selector (plist-get mutant :selector)))
            (should (memq executor '(plain-splice dolt-write)))
            (should (ert-test-boundp test))
            (should mutant-selector)
            (should (memq test
                          (dorg-test-api--capability-test-names mutant-selector)))
            (push executor mutant-executors)))
        (pcase status
          ('supported
           (should tests)
           (should (plist-member cell :expected))
           (should (dorg-test-api--capability-expected-p
                    (plist-get cell :expected)))
           (should (memq (plist-get cell :oracle-kind)
                         '(independent transitive-independent))))
          ('refused
           (should tests)
           (let ((refusal (plist-get cell :refusal)))
             (should (consp refusal))
             (should (and (car refusal) (symbolp (car refusal))))
             (should (and (cdr refusal) (symbolp (cdr refusal))))))
          ('pending
           (should (plist-get cell :pending-detail))
           (should-not (memq (plist-get cell :oracle-kind)
                             '(independent transitive-independent)))))))
    (should (equal '(dolt-write plain-splice)
                   (cl-sort mutant-executors #'string< :key #'symbol-name)))))

(ert-deftest dorg-test-api-capability-plain-splice-noop-mutant-is-observable ()
  "A no-op at the real plain splice boundary makes the public postimage RED."
  (let ((form '(on (id 1001) (insert (text) (after-last) "Mutant text."))))
    ;; Establish that the exact public intent has a real green leg first.
    (dorg-test-mutate--with-content real-buf dorg-test-mutate--keyword-insert-sample
      (dorg-test-mutate--assert-dorg-policy real-buf)
      (should (plist-get (dorg-api-execute form :mode 'apply :source real-buf) :ok))
      (should (string-match-p "Mutant text\\." (buffer-string))))
    (dorg-test-mutate--with-content buf dorg-test-mutate--keyword-insert-sample
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((before (buffer-string))
            (splice-calls 0))
        (cl-letf (((symbol-function 'dorg-mutate--source-preserving-splice-patch)
                   (lambda (&rest _) (cl-incf splice-calls) nil)))
          (should (plist-get (dorg-api-execute form :mode 'apply :source buf) :ok))
          (should (= 1 splice-calls))
          ;; The same positive oracle is RED under a removed executor; the
          ;; complete source stays unchanged, including wrong member/spans.
          (should-error
           (should (string-match-p "Mutant text\\." (buffer-string)))
           :type 'ert-test-failed)
          (should (equal before (buffer-string))))))))

(ert-deftest dorg-test-api-capability-dolt-write-noop-mutant-is-observable ()
  "A no-op at the Dolt write gate makes public durable-HEAD evidence RED."
  (let ((fixture "* Old\n"))
    ;; A real non-root heading update proves this intent is independently green.
    (dorg-test-mutate--with-dolt
      (let* ((root (dorg-test-mutate--seed dolt-dir fixture))
             (target (dorg-test-mutate--id-by-title dolt-dir "Old"))
             (source (list :dolt-dir dolt-dir :root-id root))
             (head-before (dorg-head-commit dolt-dir)))
        (should target)
        (should (plist-get
                 (dorg-api-execute `(on (id ,target) (set (heading title) "New"))
                                   :mode 'apply :source source)
                 :ok))
        (should-not (equal head-before (dorg-head-commit dolt-dir)))))
    (dorg-test-mutate--with-dolt
      (let* ((root (dorg-test-mutate--seed dolt-dir fixture))
             (target (dorg-test-mutate--id-by-title dolt-dir "Old"))
             (source (list :dolt-dir dolt-dir :root-id root))
             (head-before (dorg-head-commit dolt-dir))
             (write-calls 0))
        (should target)
        (cl-letf (((symbol-function 'dorg-mutate--dolt-gated-apply)
                   (lambda (_source _options _base _plan-fn &rest _)
                     (cl-incf write-calls)
                     (list :ok t :dry-run nil :source-kind 'dolt :root-id root
                           :commit head-before :created nil :changed nil :deleted nil))))
          (should (plist-get
                   (dorg-api-execute `(on (id ,target) (set (heading title) "New"))
                                     :mode 'apply :source source)
                   :ok))
          (should (= 1 write-calls))
          ;; The durable readback oracle is RED only after its real green leg.
          (should-error
           (should-not (equal head-before (dorg-head-commit dolt-dir)))
           :type 'ert-test-failed)
          (should (equal head-before (dorg-head-commit dolt-dir))))))))

;;; ─── Intrinsic COMMENTED heading field (tracked-regression) ────────────────

(ert-deftest dorg-test-c8ynt-commented-normalization-preserves-nil ()
  "Public COMMENTED lowering carries both booleans, including present nil."
  (dolist (value '(t nil))
    (let* ((intent `(on (id 100) (set (heading commented) ,value)))
           (patch (dorg-test-api--1 intent))
           (fields (plist-get patch :fields)))
      (should (equal '(:heading :id 100) (plist-get patch :target)))
      (should (eq 'update (plist-get patch :op)))
      (should (plist-member fields :commented))
      (should (eq value (plist-get fields :commented)))
      (should (equal (list :commented value) fields)))))

(ert-deftest dorg-test-c8ynt-commented-dry-run-is-wire-honest ()
  "Dry-run echoes exact COMMENTED intent and typed patches without a write."
  (dorg-test-api--with-unit-files
   (let (seen-patches)
     (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
		(lambda (_source patches _ext-inputs &rest _)
                  (setq seen-patches patches)
                  '(:ok t :dry-run t :source-kind plain-org :root-id 1
			:base-revision "tok" :supported t :unsupported nil))))
	      (dolist (value '(t nil))
		(let* ((intent `(on (id 100) (set (heading commented) ,value)))
		       (response (dorg-api-execute intent :mode 'dry-run
						   :source (list :file (dorg-test-api--unit-file))))
		       (patches (plist-get response :patches))
		       (fields (plist-get (car patches) :fields)))
		  (should (equal (list intent) (plist-get response :intent)))
		  (should (equal patches seen-patches))
		  (should (plist-member fields :commented))
		  (should (eq value (plist-get fields :commented)))))))))

;;; ─── Atomic COMMENTED heading construction (tracked-regression) ───────────

(ert-deftest dorg-test-ltxz7-commented-constructor-normalizes-booleans ()
  "Constructor COMMENTED preserves exact t/nil presence in typed IR."
  (dolist (value '(t nil))
    (let* ((patch
            (dorg-test-api--1
             `(insert (heading (title "Resolution") (commented ,value))
                      (under (id 100)))))
           (heading (plist-get patch :heading)))
      (should (eq 'insert (plist-get patch :op)))
      (should (equal '(:tree :under (:heading :id 100))
                     (plist-get patch :target)))
      (should (plist-member heading :commented))
      (should (eq value (plist-get heading :commented)))
      (should (equal "Resolution" (plist-get heading :title))))))

(ert-deftest dorg-test-ltxz7-commented-constructor-refuses-duplicates-and-types ()
  "Constructor COMMENTED is one exact boolean, never last-wins or coerced."
  (dolist
      (form
       '((insert (heading (title "Resolution")
                          (commented t) (commented nil))
                 (under (id 100)))
         (insert (heading (title "Resolution") (commented bogus))
                 (under (id 100)))
         (insert (heading (title "Resolution") (commented "yes"))
                 (under (id 100)))))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize form))))

;;; ─── Compound mutation grammar (tracked-regression) ────────────────────────

(ert-deftest dorg-test-g0vnp-compound-grammar-exact-ir-table ()
  "Every final compound production lowers to the existing typed IR shape."
  (dolist
      (case
       '(((on (id 100) (set (property "STATUS") "DONE"))
          (:op put :target (:property :host (:heading :id 100) :name "STATUS")
           :value "DONE"))
         ((on (id 100) (delete (property "STATUS")))
          (:op delete :target
           (:property :host (:heading :id 100) :name "STATUS")))
         ((on document (replace (keyword "SEQ_TODO" (all)) "TODO" "DONE"))
          (:op replace :target (:keyword :host :root :name "SEQ_TODO" :select (:all))
           :values ("TODO" "DONE")))
         ((on document (delete (keyword "SEQ_TODO" (all))))
          (:op delete :target (:keyword :host :root :name "SEQ_TODO" :select (:all))))
         ((on document
              (replace (keyword "REVIEWERS" (occurrence 2)) "a,b"))
          (:op replace
           :target (:keyword :host :root :name "REVIEWERS" :select (:occurrence 2))
           :value "a,b"))
         ((on document (delete (keyword "REVIEWERS" (occurrence 1))))
          (:op delete
           :target (:keyword :host :root :name "REVIEWERS" :select (:occurrence 1))))
         ((on document
              (insert (keyword "REVIEWERS") (after-last) "nav-new"))
          (:op insert :target (:keyword :host :root :name "REVIEWERS")
           :value "nav-new" :position :after-last))
         ((on (id 100)
              (insert (drawer-line "LOGBOOK")
                      (before-first) "- State \"DONE\""))
          (:op insert
           :target (:drawer-line :host (:heading :id 100) :name "LOGBOOK")
           :position :before-first :value "- State \"DONE\""))
         ((on (id 100) (set (heading todo) "DONE"))
          (:op update :target (:heading :id 100) :fields (:todo "DONE")))
         ((on (id 100) (set (planning scheduled) "<2026-07-01>"))
          (:op update :target (:planning :host (:heading :id 100))
           :scheduled "<2026-07-01>"))
         ((on (id 42)
              (move (subtree) (under (id 7) (after (id 9)))))
          (:op move :target (:subtree :id 42)
           :to (:under (:heading :id 7) :after (:heading :id 9))))
         ((on (id 100) (delete (heading)))
          (:op delete :target (:heading :id 100)))
         ((on (id 100) (delete (subtree)))
          (:op delete :target (:subtree :id 100)))
         ((on (id 100) (insert (text) (after-last) "new"))
          (:op insert :target (:text :host (:heading :id 100))
           :position :after-last :value "new"))
         ((on (id 100) (replace (text (occurrence 1)) "new"))
          (:op replace
           :target (:text :host (:heading :id 100) :select (:occurrence 1))
           :value "new"))
         ((on (id 100) (replace (item (all)) (text "new")))
          (:op replace :target (:item :host (:heading :id 100) :select (:all))
           :items ((:kind text :text "new" :post-blank 0))))
         ((insert (heading (title "Inbox") (todo "TODO"))
                  (under document))
          (:op insert :target (:tree :under :root)
           :heading (:title "Inbox" :todo "TODO")))))
    (pcase-let ((`(,form ,expected) case))
      (ert-info ((format "%S" form))
        (should (equal expected (dorg-test-api--1 form)))))))

(ert-deftest dorg-test-g0vnp-direct-key-is-canonical-and-wrapper-composes ()
  "Direct key and query-composed key share one exactly-one projection seam."
  (dorg-test-api--with-extern-key-hits
    (let* ((direct '(on (key "task-a")
                        (set (property "STATUS") "DONE")))
           (wrapped '(on (where (key "task-a"))
                         (set (property "STATUS") "DONE")))
           (direct-patch (dorg-test-api--1 direct source))
           (wrapped-patch (dorg-test-api--1 wrapped source)))
      (should (equal direct-patch wrapped-patch))
      (should (equal dorg-test-api--extern-host-a
                     (dorg-api--tget (plist-get direct-patch :target) :host)))
      (should-not (dorg-api--query-scoped-p direct))
      (should (dorg-api--selector-bounded-p direct))
      (should (dorg-api--query-scoped-p wrapped))
      (should (dorg-api--selector-bounded-p wrapped)))))

(ert-deftest dorg-test-g0vnp-direct-key-cardinality-and-value-refuse ()
  "Direct identities distinguish missing, duplicate, and malformed values."
  (dorg-test-api--with-extern-key-hits
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize
       '(on (key "missing") (set (property "STATUS") "DONE")) source))
    (dorg-test-api--should-reason 'identity-violation
      (dorg-api-normalize
       '(on (key "dup") (set (property "STATUS") "DONE")) source))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize
       '(on (key 42) (set (property "STATUS") "DONE")) source))))

(ert-deftest dorg-test-g0vnp-structural-key-operands-resolve-exactly-once ()
  "Move and insert resolve every direct-key subject, parent, and anchor."
  (dorg-test-api--with-extern-key-hits
    (let ((move (dorg-test-api--1
                 '(on (key "task-a")
                      (move (subtree)
                            (under (key "parent") (after (key "anchor")))))
                 source))
          (insert (dorg-test-api--1
                   '(insert (heading (title "Created")
                                     (property "BEAD_ID" "created"))
                            (under (key "parent") (before (key "anchor"))))
                   source)))
      (should (equal '(:subtree :extern t :key "BEAD_ID" :value "task-a")
                     (plist-get move :target)))
      (should (equal `(:under ,dorg-test-api--extern-host-parent
                       :after ,dorg-test-api--extern-host-anchor)
                     (plist-get move :to)))
      (should (equal `(:tree :under ,dorg-test-api--extern-host-parent
                       :before ,dorg-test-api--extern-host-anchor)
                     (plist-get insert :target))))
    (dorg-test-api--should-reason 'identity-violation
      (dorg-api-normalize
       '(on (key "task-a")
            (move (subtree) (under (key "parent") (after (key "dup")))))
       source))))

(ert-deftest dorg-test-g0vnp-retired-flat-addresses-hard-cut ()
  "Every pre-compound address spelling is a typed invalid-target refusal."
  (dolist
      (form
       '((on (id 100) (set property "K" "V"))
         (on (id 100) (delete property "K"))
         (on (id 100) (replace keywords "K" "V"))
         (on (id 100) (delete keywords "K"))
         (on (id 100) (replace keyword "K" (occurrence 1) "V"))
         (on (id 100) (delete keyword "K" (occurrence 1)))
         (on (id 100) (insert keyword "K" (after-last) "V"))
         (on (id 100)
             (insert drawer-line "LOGBOOK" (occurrence 1)
                     (before-first) "line"))
         (on (id 100) (set heading title "x"))
         (on (id 100) (set planning scheduled nil))
         (on (id 100) (move (under (id 7)) (after (id 9))))
         (on (id 100) (delete heading))
         (on (id 100) (delete subtree))
         (on (id 100) (replace items (text "x")))
         (insert heading (under (id 100)) (title "x"))))
    (ert-info ((format "%S" form))
      (dorg-test-api--should-reason 'invalid-target
        (dorg-api-normalize form))))
  ;; `append' is not a verb in the final algebra; malformed `replace' is.
  (dorg-test-api--should-reason 'invalid-op
    (dorg-api-normalize '(on (id 100) (append body "x"))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on (id 100) (replace body "x"))))
  ;; Improper action/anchor tails are typed grammar refusals, not raw Lisp
  ;; sequence errors from `mapcar' or `length'.
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on (id 100) . malformed)))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize
     '(on (id 100) (move (subtree) (under document . malformed)))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(batch . malformed))))

(ert-deftest dorg-test-g0vnp-root-and-old-structural-shapes-hard-cut ()
  "The public document host and grouped structural position have one spelling."
  (dolist
      (form
       '((on root (delete (keyword "K" (all))))
         (insert (heading (title "x")) (under root))
         (on (id 100)
             (move (subtree) (under (id 7)) (after (id 9))))
         (insert (heading (title "x"))
                 (under (id 7)) (after (id 9)))
         (on (id 100)
             (move (subtree)
                   (under (where (key "parent")))))))
    (ert-info ((format "%S" form))
      (dorg-test-api--should-reason 'invalid-target
        (dorg-api-normalize form (dorg-test-api--extern-source))))))

(ert-deftest dorg-test-g0vnp-dry-run-shows-public-vs-ir-boundary ()
  "Dry-run keeps the canonical intent while exposing unchanged internal :root."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
              (lambda (&rest _)
		'(:ok t :dry-run t :source-kind plain-org :root-id 1
                      :base-revision "tok" :supported t :unsupported nil))))
	    (let* ((form '(on document
			      (replace (keyword "SEQ_TODO" (all)) "TODO" "DONE")))
		   (response (dorg-api-execute form :mode 'dry-run
					       :source (list :file (dorg-test-api--unit-file)))))
	      (should (equal (list form) (plist-get response :intent)))
	      (should
	       (equal '((:op replace
			     :target (:keyword :host :root :name "SEQ_TODO" :select (:all))
			     :values ("TODO" "DONE")))
		      (plist-get response :patches)))))))

;;; ─── AC2: typed patch IR v2 — op×target coverage ─────────────────────────
;; Each public action lowers to the exact canonical typed patch (docs/mutate.org
;; "DSL → typed IR mapping").  HOST = (:heading :id N) from (id N), or :root.

(ert-deftest dorg-test-api-ir-put-property ()
  (let ((p (dorg-test-api--1 '(on (id 100) (set (property "STATUS") "DONE")))))
    (should (eq 'put (plist-get p :op)))
    (should (equal '(:property :host (:heading :id 100) :name "STATUS")
                   (plist-get p :target)))
    ;; AC-1 non-vacuity: the OLD :key field is gone from the typed IR target.
    (should-not (memq :key (plist-get p :target)))
    (should (equal "DONE" (plist-get p :value)))))

(ert-deftest dorg-test-api-ir-delete-property ()
  (let ((p (dorg-test-api--1 '(on (id 100) (delete (property "STATUS"))))))
    (should (eq 'delete (plist-get p :op)))
    (should (equal '(:property :host (:heading :id 100) :name "STATUS")
                   (plist-get p :target)))))

(ert-deftest dorg-test-api-ir-replace-keyword-all-sequence ()
  (let ((p (dorg-test-api--1 '(on document (replace (keyword "SEQ_TODO" (all)) "TODO" "NEXT" "DONE")))))
    (should (eq 'replace (plist-get p :op)))
    (should (equal '(:keyword :host :root :name "SEQ_TODO" :select (:all))
                   (plist-get p :target)))
    (should (equal '("TODO" "NEXT" "DONE") (plist-get p :values)))))

(ert-deftest dorg-test-api-ir-delete-keyword-all-sequence ()
  (let ((p (dorg-test-api--1 '(on document (delete (keyword "SEQ_TODO" (all)))))))
    (should (eq 'delete (plist-get p :op)))
    (should (equal '(:keyword :host :root :name "SEQ_TODO" :select (:all))
                   (plist-get p :target)))))

(ert-deftest dorg-test-api-ir-replace-keyword-occurrence-preserves-base ()
  "Public one-based (occurrence 2) stays a selector in the typed IR."
  (let ((p (dorg-test-api--1
            '(on document (replace (keyword "REVIEWERS" (occurrence 2)) "a,b")))))
    (should (eq 'replace (plist-get p :op)))
    (should (equal '(:keyword :host :root :name "REVIEWERS" :select (:occurrence 2))
                   (plist-get p :target)))
    (should (equal "a,b" (plist-get p :value)))))

(ert-deftest dorg-test-api-ir-delete-keyword-occurrence ()
  (let ((p (dorg-test-api--1 '(on document (delete (keyword "REVIEWERS" (occurrence 1)))))))
    (should (eq 'delete (plist-get p :op)))
    (should (equal '(:keyword :host :root :name "REVIEWERS" :select (:occurrence 1))
                   (plist-get p :target)))))

(ert-deftest dorg-test-api-ir-insert-keyword-after-last ()
  (let ((p (dorg-test-api--1 '(on document (insert (keyword "REVIEWERS") (after-last) "nav-new")))))
    (should (eq 'insert (plist-get p :op)))
    (should (equal '(:keyword :host :root :name "REVIEWERS") (plist-get p :target)))
    (should (equal "nav-new" (plist-get p :value)))
    (should (eq :after-last (plist-get p :position)))))

(ert-deftest dorg-test-api-ir-update-heading-fields ()
  (let ((p (dorg-test-api--1 '(on (id 100) (set (heading todo) "DONE")))))
    (should (eq 'update (plist-get p :op)))
    (should (equal '(:heading :id 100) (plist-get p :target)))
    (should (equal '(:todo "DONE") (plist-get p :fields)))))

(ert-deftest dorg-test-api-ir-update-planning ()
  (let ((p (dorg-test-api--1 '(on (id 100) (set (planning scheduled) "<2026-07-01>")))))
    (should (eq 'update (plist-get p :op)))
    (should (equal '(:planning :host (:heading :id 100)) (plist-get p :target)))
    (should (equal "<2026-07-01>" (plist-get p :scheduled)))))

(ert-deftest dorg-test-api-ir-insert-heading ()
  (let ((p (dorg-test-api--1
            '(insert (heading (title "Inbox") (todo "TODO"))
                     (under document)))))
    (should (eq 'insert (plist-get p :op)))
    (should (equal '(:tree :under :root) (plist-get p :target)))
    (should (equal "Inbox" (plist-get (plist-get p :heading) :title)))
    (should (equal "TODO" (plist-get (plist-get p :heading) :todo)))))

(ert-deftest dorg-test-api-ir-insert-heading-properties-become-items ()
  "Public insert-heading property clauses normalize to one PROPERTIES drawer."
  (let* ((p (dorg-test-api--1
             '(insert (heading (title "Created")
                               (property "K" "v")
                               (property "OWNER" "cosmic"))
                      (under document))))
         (heading (plist-get p :heading))
         (items (plist-get heading :items))
         (expected '((:kind drawer :name "PROPERTIES"
                      :properties (("K" . "v") ("OWNER" . "cosmic"))
                      :lines nil))))
    (should (eq 'insert (plist-get p :op)))
    (should (equal expected items))
    (should-not (plist-member heading :properties))))

(ert-deftest dorg-test-api-ir-move-subtree ()
  (let ((p (dorg-test-api--1
            '(on (id 42)
                 (move (subtree) (under (id 7) (after (id 9))))))))
    (should (eq 'move (plist-get p :op)))
    (should (equal '(:subtree :id 42) (plist-get p :target)))
    (should (equal '(:under (:heading :id 7) :after (:heading :id 9))
                   (plist-get p :to)))))

(ert-deftest dorg-test-kixox-numeric-insert-anchor-normalizes-to-typed-host ()
  "A numeric sibling anchor stays typed through public insert normalization."
  (let ((p (dorg-test-api--1
            '(insert (heading (title "Created"))
                     (under (id 7) (before (id 9)))))))
    (should (eq 'insert (plist-get p :op)))
    (should (equal '(:tree :under (:heading :id 7)
                           :before (:heading :id 9))
                   (plist-get p :target)))))

(ert-deftest dorg-test-kixox-dry-run-exposes-typed-numeric-anchors ()
  "Dry-run sends and exposes typed numeric insert and move anchors verbatim."
  (dorg-test-api--with-unit-files
   (let (seen)
     (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
		(lambda (_source patches _ext-inputs &rest _)
                  (setq seen patches)
                  '(:ok t :dry-run t :source-kind plain-org :root-id 1
			:base-revision "tok" :supported t :unsupported nil))))
	      (let* ((intent
		      '(batch
			(insert (heading (title "Created"))
				(under (id 7) (before (id 9))))
			(on (id 42)
			    (move (subtree) (under (id 7) (after (id 9)))))))
		     (response (dorg-api-execute intent :mode 'dry-run
						 :source (list :file (dorg-test-api--unit-file))))
		     (patches (plist-get response :patches)))
		(should (equal patches seen))
		(should (equal '(:tree :under (:heading :id 7)
				       :before (:heading :id 9))
			       (plist-get (car patches) :target)))
		(should (equal '(:under (:heading :id 7)
					:after (:heading :id 9))
			       (plist-get (cadr patches) :to))))))))

(ert-deftest dorg-test-api-ir-delete-heading ()
  (let ((p (dorg-test-api--1 '(on (id 100) (delete (heading))))))
    (should (eq 'delete (plist-get p :op)))
    (should (equal '(:heading :id 100) (plist-get p :target)))))

(ert-deftest dorg-test-api-ir-delete-subtree ()
  (let ((p (dorg-test-api--1 '(on (id 100) (delete (subtree))))))
    (should (eq 'delete (plist-get p :op)))
    (should (equal '(:subtree :id 100) (plist-get p :target)))))

(ert-deftest dorg-test-api-extern-key-heading-subjects-preserve-hosts ()
  "A bounded identity-key subject stays an extern host for heading/tree actions.
This is the public-boundary RED spec for tracked-regression: the current
`dorg-api--host-id' path collapses these targets to `(:heading :id nil)' /
`(:subtree :id nil)' instead of preserving the host for executor-owned
same-snapshot resolution."
  (dorg-test-api--with-extern-key-hits
    (dolist (case
             `(((on (key "task-a")
                    (set (heading title) "Renamed"))
                update ,dorg-test-api--extern-host-a)
               ((on (key "task-a")
                    (set (heading todo) "DONE"))
                update ,dorg-test-api--extern-host-a)
               ((on (key "task-a")
                    (set (heading priority) "A"))
                update ,dorg-test-api--extern-host-a)
               ((on (key "task-a")
                    (set (heading tags) ("work" "urgent")))
                update ,dorg-test-api--extern-host-a)
               ((on (key "task-a") (delete (heading)))
                delete ,dorg-test-api--extern-host-a)
               ((on (key "task-a") (delete (subtree)))
                delete (:subtree :extern t :key "BEAD_ID" :value "task-a"))
               ((on (key "task-a")
                    (move (subtree) (under document)))
                move (:subtree :extern t :key "BEAD_ID" :value "task-a"))))
      (pcase-let ((`(,form ,op ,target) case))
        (let ((p (dorg-test-api--1 form source)))
          (ert-info ((format "%S" form))
            (should (eq op (plist-get p :op)))
            (should (equal target (plist-get p :target)))
            (should-not (equal '(:heading :id nil) (plist-get p :target)))
            (should-not (equal '(:subtree :id nil) (plist-get p :target)))))))))

(ert-deftest dorg-test-api-extern-key-structural-operands-preserve-hosts ()
  "Move and insert preserve direct-key parent and sibling hosts.
Nested structural operands use the same canonical identity vocabulary as
top-level subjects; callers never pre-resolve numeric DORG_ID values."
  (dorg-test-api--with-extern-key-hits
    (let ((move (dorg-test-api--1
                 '(on (key "task-a")
                      (move (subtree)
                            (under (key "parent")
                                   (after (key "anchor")))))
                 source))
          (insert (dorg-test-api--1
                   '(insert (heading (title "Created")
                                     (property "BEAD_ID" "created"))
                            (under (key "parent")
                                   (before (key "anchor"))))
                   source)))
      (should (equal '(:subtree :extern t :key "BEAD_ID" :value "task-a")
                     (plist-get move :target)))
      (should (equal `(:under ,dorg-test-api--extern-host-parent
                       :after ,dorg-test-api--extern-host-anchor)
                     (plist-get move :to)))
      (should (equal `(:tree :under ,dorg-test-api--extern-host-parent
                       :before ,dorg-test-api--extern-host-anchor)
                     (plist-get insert :target)))
      (should (equal "created"
                     (cdr (assoc "BEAD_ID"
                                 (plist-get
                                  (car (plist-get (plist-get insert :heading) :items))
                                  :properties))))))))

(ert-deftest dorg-test-api-extern-key-structural-forms-prequery ()
  "Every direct key resolves through the shared exactly-one query seam."
  (let ((seen nil))
    (dorg-test-api--with-extern-key-hits
      (let ((original (symbol-function 'dorg-ql-select)))
        (cl-letf (((symbol-function 'dorg-ql-select)
                   (lambda (query &rest args)
                     (push query seen)
                     (apply original query args))))
          (dorg-test-api--1
           '(on (key "task-a")
                (move (subtree)
                      (under (key "parent") (after (key "anchor")))))
           source)
          (dorg-test-api--1
           '(on (key "task-a") (set (heading title) "Renamed")) source)
          (dorg-test-api--1
           '(insert (heading (title "Created"))
                    (under (key "parent") (before (key "anchor"))))
           source)
          (should
           (equal '((property "BEAD_ID" "anchor")
                    (property "BEAD_ID" "parent")
                    (property "BEAD_ID" "task-a")
                    (property "BEAD_ID" "anchor")
                    (property "BEAD_ID" "parent")
                    (property "BEAD_ID" "task-a"))
                  seen)))))))

(ert-deftest dorg-test-api-extern-key-surgical-containing-forms-prequery ()
  "A direct key containing mixed actions resolves through one shared query."
  (let ((source (dorg-test-api--extern-source))
        (seen nil))
    (cl-letf (((symbol-function 'dorg-ql-select)
               (lambda (query &rest _)
                 (setq seen query)
                 '((:heading-id 101 :properties (("BEAD_ID" . "task-a")))))))
      (let ((ps (dorg-api-normalize
                 '(on (key "task-a")
                      (set (property "STATUS") "DONE")
                      (set (heading title) "Renamed"))
                 source)))
        (should (equal '(property "BEAD_ID" "task-a") seen))
        (should (= 2 (length ps)))
        (should (equal '(:property
                         :host (:heading :extern t :key "BEAD_ID" :value "task-a")
                         :name "STATUS")
                       (plist-get (car ps) :target)))
        (should (equal dorg-test-api--extern-host-a
                       (plist-get (cadr ps) :target)))))))

(ert-deftest dorg-test-api-extern-key-missing-and-duplicate-fail-before-write ()
  "Missing identity stays invalid-target; duplicate identity stays identity-violation.
The failed public apply leaves the source bytes unchanged, proving the error path
does not partially write earlier batch members."
  (dorg-test-api--with-extern-key-hits
    (dorg-test-api--should-reason 'identity-violation
      (dorg-api-normalize
       '(on (key "dup") (set (property "STATUS") "ambiguous"))
       source)))
  (let* ((initial "* TODO Alpha\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n")
         (file (make-temp-file "dorg-api-extern-missing-" nil ".org" initial))
         (source (dorg-test-api--extern-source file)))
    (unwind-protect
        (let ((reason (condition-case err
                          (progn
                            (dorg-api-execute
                             '(batch
                               (on (key "task-a")
                                   (set (property "STATUS") "Changed"))
                               (on (key "missing")
                                   (set (property "STATUS") "DONE")))
                             :mode 'apply :source source :save t)
                            :no-error)
                        (dorg-api-error (dorg-api-error-reason err))
                        (dorg-mutate-error (dorg-mutate-error-reason err)))))
          (should (eq 'invalid-target reason))
          (should (equal initial (dorg-test-api--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-api-extern-key-heading-apply-plain-org ()
  "Plain-org title updates through an external key use the public editor."
  (let* ((initial "* TODO Alpha\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n* TODO Beta\n:PROPERTIES:\n:BEAD_ID: task-b\n:END:\n")
         (file (make-temp-file "dorg-api-extern-heading-" nil ".org" initial))
         (source (dorg-test-api--extern-source file)))
    (unwind-protect
        (should (plist-get
                 (dorg-api-execute
                  '(on (key "task-a")
                       (set (heading title) "Renamed Alpha"))
                  :mode 'apply :source source :save t)
                 :ok))
        (should (equal
                 "* TODO Renamed Alpha\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n* TODO Beta\n:PROPERTIES:\n:BEAD_ID: task-b\n:END:\n"
                 (dorg-test-api--slurp file)))
      (delete-file file))))

(ert-deftest dorg-test-api-extern-key-heading-apply-dolt ()
  "A live Dolt property-identity source resolves key-addressed heading/tree hosts.
This pins the executor-owned path: extern subjects and structural anchors are
resolved under the same source snapshot/transaction, while the public API target
set remains the canonical extern identity set."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:BEAD_ID: task-a\n:END:\n* Parent\n:PROPERTIES:\n:DORG_ID: 200\n:BEAD_ID: parent\n:END:\n** Anchor\n:PROPERTIES:\n:DORG_ID: 201\n:BEAD_ID: anchor\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id
                      :identity '(:property "BEAD_ID")))
           (commits-before (dorg-test-api--dolt-commit-count dd))
           (resp (dorg-api-execute
                  '(batch
                    (on (key "task-a")
                        (set (heading title) "Renamed Alpha")
                        (set (heading todo) "DONE")
                        (set (heading priority) "A")
                        (set (heading tags) ("work" "urgent")))
                    (insert (heading (title "Created")
                                     (property "BEAD_ID" "created"))
                            (under (key "parent")
                                   (before (key "anchor"))))
                    (on (id 200)
                        (set (property "MIXED") "yes"))
                    (on (key "task-a")
                        (move (subtree)
                              (under (key "parent")
                                     (before (key "anchor"))))))
                  :mode 'apply :source src)))
      (should (plist-get resp :ok))
      (should (= (1+ commits-before)
                 (dorg-test-api--dolt-commit-count dd)))
      (should (member '(:extern :key "BEAD_ID" :value "task-a")
                      (plist-get resp :targets)))
      (should (member 200 (plist-get resp :targets)))
      (let ((headings (dorg-model-load-headings dd root-id nil)))
        (let ((task (gethash 100 headings))
              (parent (gethash 200 headings))
              (anchor (gethash 201 headings))
              (created (dorg-test-api--heading-by-property headings "BEAD_ID" "created")))
          (should (equal "Renamed Alpha" (dorg-heading-title task)))
          (should (equal "DONE" (dorg-heading-todo task)))
          (should (equal "A" (dorg-heading-priority task)))
          (should (equal '("work" "urgent") (dorg-heading-tags task)))
          (should (= 200 (dorg-heading-parent-id task)))
          (should (= 200 (dorg-heading-parent-id anchor)))
          (should (= 200 (dorg-heading-parent-id created)))
          (should (equal "yes" (dorg-test-api--heading-property parent "MIXED")))
          (should (string< (dorg-heading-sort-key created)
                           (dorg-heading-sort-key task)))
          (should (string< (dorg-heading-sort-key task)
                           (dorg-heading-sort-key anchor)))
          (should parent))))))

(ert-deftest dorg-test-api-extern-key-delete-heading-and-subtree-dolt ()
  "External-key delete preserves existing promotion vs subtree semantics."
  (dorg-test-ql--with-dolt-synced
      "* Promote\n:PROPERTIES:\n:DORG_ID: 300\n:BEAD_ID: promote\n:END:\n** Child\n:PROPERTIES:\n:DORG_ID: 301\n:BEAD_ID: child\n:END:\n* Gone\n:PROPERTIES:\n:DORG_ID: 400\n:BEAD_ID: gone\n:END:\n** Gone child\n:PROPERTIES:\n:DORG_ID: 401\n:BEAD_ID: gone-child\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id
                      :identity '(:property "BEAD_ID")))
           (base (dorg-head-commit dd))
           (resp (dorg-api-execute
                  '(batch
                    (on (key "promote") (delete (heading)))
                    (on (key "gone") (delete (subtree))))
                  :mode 'apply :source src)))
      (should (plist-get resp :ok))
      (should-not (equal base (dorg-head-commit dd)))
      (let ((headings (dorg-model-load-headings dd root-id nil)))
        (should-not (gethash 300 headings))
        (should-not (gethash 400 headings))
        (should-not (gethash 401 headings))
        (let ((child (gethash 301 headings)))
          (should child)
          (should (= root-id (dorg-heading-parent-id child)))
          (should (= 1 (dorg-heading-level child))))))))

(ert-deftest dorg-test-api-extern-key-anchor-parent-mismatch-fails-before-write ()
  "An external-key sibling anchor must belong to the chosen external-key parent."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:BEAD_ID: task-a\n:END:\n* Parent\n:PROPERTIES:\n:DORG_ID: 200\n:BEAD_ID: parent\n:END:\n* Anchor\n:PROPERTIES:\n:DORG_ID: 201\n:BEAD_ID: anchor\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id
                      :identity '(:property "BEAD_ID")))
           (base (dorg-head-commit dd))
           (outcome (dorg-test-api--mutation-reason+detail
                     (lambda ()
                       (dorg-api-execute
                        '(on (key "task-a")
                             (move (subtree)
                                   (under (key "parent")
                                          (before (key "anchor")))))
                        :mode 'apply :source src)))))
      (should (equal '(invalid-target . missing) outcome))
      (should (equal base (dorg-head-commit dd)))
      (let ((headings (dorg-model-load-headings dd root-id nil)))
        (should (= root-id (dorg-heading-parent-id (gethash 100 headings))))
        (should (= root-id (dorg-heading-parent-id (gethash 201 headings))))))))

(ert-deftest dorg-test-kixox-numeric-anchor-parent-mismatch-fails-before-write ()
  "A numeric sibling anchor outside its chosen parent is missing, not untyped."
  (dorg-test-ql--with-dolt-synced
      "* Subject\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Parent\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n* Anchor\n:PROPERTIES:\n:DORG_ID: 201\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (base (dorg-head-commit dd))
           (outcome
            (dorg-test-api--mutation-reason+detail
             (lambda ()
               (dorg-api-execute
                '(on (id 100)
                     (move (subtree)
                           (under (id 200) (before (id 201)))))
                :mode 'apply :source source)))))
      (should (equal '(invalid-target . missing) outcome))
      (should (equal base (dorg-head-commit dd)))
      (let ((headings (dorg-model-load-headings dd root-id nil)))
        (should (= root-id (dorg-heading-parent-id (gethash 100 headings))))
        (should (= root-id (dorg-heading-parent-id (gethash 201 headings))))))))

(ert-deftest dorg-test-kixox-plain-numeric-anchor-reaches-c3-refusal ()
  "A typed numeric anchor reaches the existing plain structural refusal.
The source bytes remain unchanged; this bead does not add a plain editor."
  (let* ((initial
          "* Parent\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n** Existing\n:PROPERTIES:\n:DORG_ID: 101\n:END:\n")
         (file (make-temp-file "dorg-kixox-plain-" nil ".org" initial)))
    (unwind-protect
        (let ((outcome
               (dorg-test-api--mutation-reason+detail
                (lambda ()
                  (dorg-api-execute
                   '(insert (heading (title "Created"))
                            (under (id 100) (before (id 101))))
                   :mode 'apply :source (list :file file) :save t)))))
          (should (equal '(unsupported . no-source-preserving-editor) outcome))
          (should (equal initial (dorg-test-api--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-kixox-anchored-operations-match-tail-move-workaround ()
  "Typed numeric anchors match the original anchor-free 90ing workaround.
One public batch transforms four sibling sets atomically.  The anchored insert
and move reach exactly the orders produced by their tail-move control sets."
  (dorg-test-ql--with-dolt-synced
      (concat
       "* Anchored insert\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
       "** Insert start\n:PROPERTIES:\n:DORG_ID: 101\n:END:\n"
       "** Insert end\n:PROPERTIES:\n:DORG_ID: 102\n:END:\n"
       "* Control insert\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n"
       "** Insert start\n:PROPERTIES:\n:DORG_ID: 201\n:END:\n"
       "** Insert end\n:PROPERTIES:\n:DORG_ID: 202\n:END:\n"
       "* Anchored move\n:PROPERTIES:\n:DORG_ID: 300\n:END:\n"
       "** Move one\n:PROPERTIES:\n:DORG_ID: 301\n:END:\n"
       "** Move two\n:PROPERTIES:\n:DORG_ID: 302\n:END:\n"
       "** Move three\n:PROPERTIES:\n:DORG_ID: 303\n:END:\n"
       "* Control move\n:PROPERTIES:\n:DORG_ID: 400\n:END:\n"
       "** Move one\n:PROPERTIES:\n:DORG_ID: 401\n:END:\n"
       "** Move two\n:PROPERTIES:\n:DORG_ID: 402\n:END:\n"
       "** Move three\n:PROPERTIES:\n:DORG_ID: 403\n:END:\n")
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (before-headings (dorg-model-load-headings dd root-id nil))
           (insert-before (dorg-test-api--child-titles before-headings 100))
           (move-before (dorg-test-api--child-titles before-headings 300))
           (commits-before (dorg-test-api--dolt-commit-count dd))
           (response
            (dorg-api-execute
             '(batch
               (insert (heading (title "Inserted"))
                       (under (id 100) (before (id 102))))
               (insert (heading (title "Inserted")) (under (id 200)))
               (on (id 202) (move (subtree) (under (id 200))))
               (on (id 301)
                   (move (subtree) (under (id 300) (after (id 302)))))
               (on (id 402) (move (subtree) (under (id 400))))
               (on (id 401) (move (subtree) (under (id 400))))
               (on (id 403) (move (subtree) (under (id 400)))))
             :mode 'apply :source source)))
      (should (plist-get response :ok))
      (should (= (1+ commits-before) (dorg-test-api--dolt-commit-count dd)))
      (let* ((headings (dorg-model-load-headings dd root-id nil))
             (anchored-insert (dorg-test-api--child-titles headings 100))
             (control-insert (dorg-test-api--child-titles headings 200))
             (anchored-move (dorg-test-api--child-titles headings 300))
             (control-move (dorg-test-api--child-titles headings 400))
             (violations
              (alist-get 'n
                         (car (dorg-sql
                               dd
                               "SELECT COUNT(*) AS n FROM dolt_constraint_violations")))))
        (should-not (equal insert-before anchored-insert))
        (should-not (equal move-before anchored-move))
        (should (equal '("Insert start" "Inserted" "Insert end")
                       anchored-insert))
        (should (equal anchored-insert control-insert))
        (should (equal '("Move two" "Move one" "Move three")
                       anchored-move))
        (should (equal anchored-move control-move))
        (should (= 0 violations))))))

(ert-deftest dorg-test-bejl7-api-dolt-buffer-source-insert-after-anchor-order ()
  "A Dolt-bound buffer source preserves repeated after-anchor insert order.
This covers the buffer-mediated API path used by interactive commands; there
is no direct interactive heading insert/move command in `dorg-mode' today."
  (dorg-test-ql--with-dolt-synced
      "* X\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Tail\n:PROPERTIES:\n:DORG_ID: 101\n:END:\n"
      (:reg reg :dolt-dir dd :buffer buf)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (before-headings (dorg-model-load-headings dd root-id nil))
           (commits-before (dorg-test-api--dolt-commit-count dd)))
      (should (equal (dorg-test-api--child-titles before-headings root-id)
                     '("X" "Tail")))
      (let ((response
             (dorg-api-execute
              '(batch
                (insert (heading (title "A"))
                        (under document (after (id 100))))
                (insert (heading (title "B"))
                        (under document (after (id 100)))))
              :mode 'apply :source buf)))
        (should (plist-get response :ok)))
      (should (= (1+ commits-before) (dorg-test-api--dolt-commit-count dd)))
      (should (equal (dorg-test-api--child-titles
                      (dorg-model-load-headings dd root-id nil) root-id)
                     '("X" "A" "B" "Tail"))))))

(ert-deftest dorg-test-api-extern-key-wrong-root-is-missing-before-write ()
  "An external key present only under another root is scoped out as missing."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:BEAD_ID: task-a\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id
                      :identity '(:property "BEAD_ID"))))
      (dorg-sql-batch
       dd
       (list
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, todo_keyword, priority, commented, title, post_blank, items_hash) VALUES (9000, 9000, NULL, X'01', 0, NULL, NULL, FALSE, NULL, 0, NULL)"
        "INSERT INTO dorg_files (root_id, relative_path) VALUES (9000, 'foreign.org')"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, todo_keyword, priority, commented, title, post_blank, items_hash) VALUES (9001, 9000, 9000, X'01', 1, NULL, NULL, FALSE, 'Foreign', 0, NULL)"
        "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (9100, 9001, X'01', 'drawer', 0)"
        "INSERT INTO drawer_items (item_id, name) VALUES (9100, 'PROPERTIES')"
        "INSERT INTO property_entries (drawer_item_id, ordinal, prop_key, prop_value, raw_line) VALUES (9100, 1, 'BEAD_ID', 'foreign', ':BEAD_ID: foreign')"))
      (dorg-commit dd "seed foreign root")
      (let ((base (dorg-head-commit dd)))
        (should (equal '(invalid-target . missing)
                       (dorg-test-api--mutation-reason+detail
                        (lambda ()
                          (dorg-api-execute
                           '(on (key "foreign")
                                (set (heading title) "Should not write"))
                           :mode 'apply :source src)))))
        (should (equal base (dorg-head-commit dd)))
        (should (equal "Foreign"
                       (alist-get 'title
                                  (car (dorg-sql dd
                                                  "SELECT title FROM headings WHERE id = 9001")))))))))

(ert-deftest dorg-test-api-extern-key-executor-time-missing-and-duplicate ()
  "Executor-owned external-key resolution classifies missing and duplicate identities.
The API query is mocked to return one canonical target, so failures happen during
executor resolution against the owned source snapshot, before any write."
  (let* ((missing-initial "* Alpha\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n")
         (missing-file (make-temp-file "dorg-api-extern-exec-missing-" nil ".org"
                                       missing-initial))
         (missing-src (dorg-test-api--extern-source missing-file))
         (dup-initial "* A\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n* B\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n")
         (dup-file (make-temp-file "dorg-api-extern-exec-dup-" nil ".org"
                                   dup-initial))
         (dup-src (dorg-test-api--extern-source dup-file)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-ql-select)
                   (lambda (query &rest _)
                     (pcase query
                       (`(property "BEAD_ID" ,value)
                        `((:heading-id 999 :properties (("BEAD_ID" . ,value)))))
                       (_ nil)))))
          (should (equal '(invalid-target . missing)
                         (dorg-test-api--mutation-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             '(on (key "ghost")
                                  (set (property "X") "1"))
                             :mode 'apply :source missing-src :save t)))))
          (should (equal missing-initial (dorg-test-api--slurp missing-file)))
          (should (equal '(identity-violation . ambiguous)
                         (dorg-test-api--mutation-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             '(on (key "dup")
                                  (set (property "X") "1"))
                             :mode 'apply :source dup-src :save t)))))
          (should (equal dup-initial (dorg-test-api--slurp dup-file))))
      (delete-file missing-file)
      (delete-file dup-file))))

(ert-deftest dorg-test-api-extern-key-stale-base-rejects-before-write ()
  "A stale public :base rejects an external-key heading apply before writing."
  (let* ((initial "* TODO Alpha\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n")
         (external "* TODO Alpha external\n:PROPERTIES:\n:BEAD_ID: task-a\n:END:\n")
         (file (make-temp-file "dorg-api-extern-stale-" nil ".org" initial))
         (source (dorg-test-api--extern-source file))
         (form '(on (key "task-a") (set (heading title) "Renamed"))))
    (unwind-protect
        (let* ((dry (dorg-api-execute form :mode 'dry-run :source source))
               (base (plist-get dry :base)))
          (with-temp-file file (insert external))
          (set-file-times file (time-add (current-time) 10))
          (should (equal '(stale . base-revision)
                         (dorg-test-api--mutation-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             form :mode 'apply :source source :base base
                             :expected-targets (plist-get dry :targets)
                             :save t)))))
          (should (equal external (dorg-test-api--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-api-ir-batch-yields-per-form-patches ()
  "A (batch …) form normalizes to the ordered patches of its member forms."
  (let ((ps (dorg-test-api--patches
             '(batch (on (id 1) (set (property "A") "1"))
                     (on (id 2) (set (property "A") "2"))))))
    ;; two put patches, one per member (batch atomicity is an envelope concern).
    (let ((puts (cl-remove-if-not (lambda (p) (eq 'put (plist-get p :op)))
                                  (flatten-list ps))))
      (should (cl-find "1" ps :key (lambda (p) (plist-get p :value)) :test #'equal))
      (should (cl-find "2" ps :key (lambda (p) (plist-get p :value)) :test #'equal))
      (ignore puts))))

;;; ─── AC1: public DSL normalizes to typed patch IR ───────────────────────

(ert-deftest dorg-test-api-normalize-is-typed-ir-not-legacy ()
  "Normalization yields typed patch plists with :op and :target fields."
  (let ((p (dorg-test-api--1 '(on (id 100) (set (property "STATUS") "DONE")))))
    (should (plist-member p :op))
    (should (plist-member p :target))
    (should (symbolp (plist-get p :op)))))

;;; ─── Text collection/occurrence selectors (tracked-regression) ──────────────
;; Text creation and occurrence replacement use the same singular target;
;; selection lives in :select and missing laws belong to its IR row.

(ert-deftest dorg-test-2rx9n-api-ir-insert-text-exact ()
  "Text insertion preserves the explicit target-relative position in IR."
  (should
   (equal '(:op insert :target (:text :host (:heading :id 100))
                 :position :after-last :value "a note")
          (dorg-test-api--1
           '(on (id 100) (insert (text) (after-last) "a note"))))))

(ert-deftest dorg-test-9diah-api-ir-insert-text-before-first ()
  "Text insertion also accepts the target-relative before-first slot."
  (should
   (equal '(:op insert :target (:text :host (:heading :id 100))
                 :position :before-first :value "a note")
          (dorg-test-api--1
           '(on (id 100) (insert (text) (before-first) "a note"))))))

(ert-deftest dorg-test-2rx9n-api-ir-replace-text-exact ()
  "Text replacement preserves its one-based occurrence and strict policy."
  (should
   (equal '(:op replace
                 :target (:text :host (:heading :id 100) :select (:occurrence 1))
                 :value "new prose")
          (dorg-test-api--1
           '(on (id 100) (replace (text (occurrence 1)) "new prose"))))))

(ert-deftest dorg-test-q60zv-api-ir-replace-body-literal-exact ()
  "A body literal precondition remains explicit in the typed intent."
  (should
   (equal '(:op replace
                 :target (:body :host (:heading :id 100) :literal "old words")
                 :value "new prose")
          (dorg-test-api--1
           '(on (id 100) (replace (body (literal "old words")) "new prose"))))))

(ert-deftest dorg-test-q60zv-api-body-literal-refusals ()
  "Body replacement accepts one nonempty literal and no positional alias."
  (dolist (form '((on (id 100) (replace (body (literal "")) "new"))
                  (on (id 100) (replace (body (literal 4)) "new"))
                  (on (id 100) (replace (body (literal "old")) 4))
                  (on (id 100) (replace (body (occurrence 1)) "new"))
                  (on (id 100) (replace (body) "new"))))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize form))))

(ert-deftest dorg-test-2rx9n-api-text-selector-refusals ()
  "The text capability has closed selector and value shape."
  (dolist (form '((on (id 100) (insert (text) (after-last)))
                  (on (id 100) (insert (text) after-last "x"))
                  (on (id 100) (insert (text extra) (after-last) "x"))
                  (on (id 100) (replace (text (occurrence 0)) "x"))
                  (on (id 100) (replace (text (occurrence -1)) "x"))
                  (on (id 100) (replace (text (occurrence 1.5)) "x"))
                  (on (id 100) (replace (text (occurrence "1")) "x"))
                  (on (id 100) (replace (text) "x"))
                  (on (id 100) (replace (text (occurrence 1) extra) "x"))))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize form))))

(ert-deftest dorg-test-2rx9n-api-text-later-occurrence-normalizes ()
  "A positive ordinal lowers without guessing whether the source has it."
  (should (equal '(:text :host (:heading :id 100) :select (:occurrence 2))
                 (plist-get (dorg-test-api--1
                             '(on (id 100) (replace (text (occurrence 2)) "x")))
                            :target))))

(ert-deftest dorg-test-2rx9n-api-text-rejects-root ()
  "Root is not a public prose-text host."
  (dolist (form '((on document (insert (text) (after-last) "x"))
                  (on document (replace (text (occurrence 1)) "x"))))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize form))))

(ert-deftest dorg-test-2rx9n-api-text-rejects-non-string ()
  "Text values are never coerced."
  (dolist (form '((on (id 1) (insert (text) (after-last) 42))
                  (on (id 1) (replace (text (occurrence 1)) nil))))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize form))))

(ert-deftest dorg-test-2rx9n-api-old-body-grammar-is-hard-cut ()
  "The two retired body productions have no compatibility normalization."
  (dorg-test-api--should-reason 'invalid-op
    (dorg-api-normalize '(on (id 100) (append body "x"))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on (id 100) (replace body "x")))))

;;; ─── Drawer-line front insertion (tracked-regression) ───────────────────

(ert-deftest dorg-test-bnsmz-api-ir-insert-drawer-line-exact ()
  "The singular drawer-line target preserves a valid lowercase NAME exactly."
  (should
   (equal
    '(:op insert
      :target (:drawer-line :host (:heading :id 100) :name "logbook")
      :position :before-first
      :value "- State \"DONE\" [2026-08-14 Fri]")
    (dorg-test-api--1
     '(on (id 100)
          (insert (drawer-line "logbook") (before-first)
                  "- State \"DONE\" [2026-08-14 Fri]"))))))

(ert-deftest dorg-test-9diah-api-ir-insert-drawer-line-after-last ()
  "Drawer-line insertion also preserves the explicit after-last position."
  (should
   (equal
    '(:op insert
      :target (:drawer-line :host (:heading :id 100) :name "audit")
      :position :after-last
      :value "- appended")
    (dorg-test-api--1
     '(on (id 100)
          (insert (drawer-line "audit") (after-last)
                  "- appended"))))))

(ert-deftest dorg-test-bnsmz-api-drawer-line-validation ()
  "Drawer names and raw lines fail closed before executor planning."
  (dolist (name '("" " LOGBOOK" "LOG BOOK" "LOG:BOOK"
                  "PROPERTIES" "properties" "END" "end"))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize
       `(on (id 100)
            (insert (drawer-line ,name) (before-first) "safe")))))
  (dolist (line (list 42 "two\nlines" "two\rlines"
                      (concat "nul" (string 0) "byte")
                      ":END:" "  :end:\t" ":OTHER:"))
    (dorg-test-api--should-reason 'invalid-target
      (dorg-api-normalize
       `(on (id 100)
            (insert (drawer-line "LOGBOOK") (before-first)
                    ,line)))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize
     '(on document
          (insert (drawer-line "LOGBOOK") (before-first)
                  "safe")))))

(ert-deftest dorg-test-bnsmz-api-drawer-line-selector-refusals ()
  "Drawer insertion is bare and accepts only the declared position grammar."
  (let ((forms
         '((on (id 100)
               (insert (drawer-line "LOGBOOK" (occurrence 0)) (before-first) "x"))
           (on (id 100)
               (insert (drawer-line "LOGBOOK" (occurrence -1)) (before-first) "x"))
           (on (id 100)
               (insert (drawer-line "LOGBOOK" (occurrence "1")) (before-first) "x"))
           (on (id 100)
               (insert (drawer-line "LOGBOOK" (occurrence 2)) (before-first) "x"))
           (on (id 100)
               (insert (drawer-line "LOGBOOK") (middle) "x"))
           (on (id 100)
               (insert drawer-line "LOGBOOK" (before-first) (occurrence 1) "x")))))
    (should
     (equal (make-list (length forms) 'invalid-target)
            (mapcar (lambda (form)
                      (dorg-test-api--reason
                       (lambda () (dorg-api-normalize form))))
                    forms)))))

;;; ─── Validation / error taxonomy (docs/mutate.org "Edge cases") ──────────

(ert-deftest dorg-test-32mp9-api-refuses-owned-property-prefixes ()
  "Every case-insensitive DORG_ property is refused before lowering.

The lowercase identity rows are regressions: before this guard used the
literal uppercase DORG_ID spelling, allowing the Org-equivalent `dorg_id' to
replace or delete the real identity line during execution.  Each form must
retain the key in its typed diagnostic so this proves the public property
door, rather than a later engine refusal."
  (dolist (key '("DORG_ID" "dorg_id" "Dorg_Id" "DORG_HASH"
                 "DORG_CLOCK_START" "dorg_hash" "DORG_" "DORG_NEW"))
    (dolist (form
             (list `(on (id 100) (set (property ,key) "x"))
                   `(on (id 100) (delete (property ,key)))
                   `(insert (heading (title "Created")
                                     (property ,key "x"))
                            (under document))))
      (let ((err (should-error (dorg-api-normalize form) :type 'dorg-api-error)))
        (should (eq 'invalid-target (dorg-api-error-reason err)))
        (should (equal key (plist-get (dorg-api-error-data err) :key))))))
  ;; Prefix ownership is not a substring rule: unowned ordinary names lower.
  (dolist (key '("STATUS" "XDORG_ID"))
    (should (equal key
                   (dorg-api--tget
                    (plist-get
                     (dorg-test-api--1 `(on (id 100) (set (property ,key) "x")))
                     :target)
                    :name)))))

(ert-deftest dorg-test-api-reject-insert-heading-invalid-property ()
  "Insert property clauses validate at the public normalization boundary."
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(insert (heading (title "Created")
                                           (property 42 "v"))
                                (under document))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(insert (heading (title "Created")
                                           (property "K" 42))
                                (under document))))
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(insert (heading (title "Created")
                                           (property "DORG_ID" "x"))
                                (under document)))))

(ert-deftest dorg-test-api-reject-dorg-prefixed-keyword ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (replace (keyword "DORG_HASH" (all)) "x")))))

(ert-deftest dorg-test-api-reject-lowercase-dorg-keyword ()
  "A LOWERCASE dorg_* keyword is rejected identically to DORG_* (tracked regression).
Org keywords are case-insensitive, so `dorg-api--check-keyword-key' must
reject #+dorg_hash: as protected metadata.  The pre-existing uppercase-only
test (`dorg-test-api-reject-dorg-prefixed-keyword') cannot catch a missed
repoint of the API check to the case-insensitive `dorg-protected-keyword-p';
this one turns RED if the API validator reverts to a case-sensitive check."
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (replace (keyword "dorg_hash" (all)) "x")))))

(ert-deftest dorg-test-api-reject-root-on-heading-field ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (set (heading todo) "DONE")))))

(ert-deftest dorg-test-api-reject-root-on-delete-heading ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (delete (heading))))))

(ert-deftest dorg-test-api-reject-occurrence-zero ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (delete (keyword "R" (occurrence 0)))))))

(ert-deftest dorg-test-api-reject-empty-replace-keywords ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (replace (keyword "R" (all)))))))

(ert-deftest dorg-test-api-reject-move-both-before-and-after ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize
     '(on (id 1)
          (move (subtree)
                (under (id 2) (before (id 3)) (after (id 4))))))))

(ert-deftest dorg-test-api-reject-no-public-set-keyword ()
  "A recognized verb with the retired flat keyword address is invalid-target."
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on document (set keyword "K" "V")))))

(ert-deftest dorg-test-api-reject-unknown-form ()
  (dorg-test-api--should-reason 'invalid-op
    (dorg-api-normalize '(frobnicate (id 1)))))

;;; ─── AC1/AC6: query-scoped expansion (shared normalization path) ─────────

(ert-deftest dorg-test-api-where-refuses-display-projection ()
  "Aliases cannot become mutation target identities."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _)
		(ert-fail "projected query reached target selection"))))
	    (dolist (query '((pipe (where (id 101)) (select (as (field title) id)))
			     (one (pipe (where (id 101)) (select (field id))))))
	      (dorg-test-api--should-reason 'invalid-target
					    (dorg-api-normalize
					     `(on (where ,query) (set (property "STATUS") "DONE"))
					     (list :file (dorg-test-api--unit-file))))))))

(ert-deftest dorg-test-api-where-expands-to-per-id-patches ()
  "(on (where Q) ACTION) expands via dorg-ql-select to one patch per id."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _) '((:heading-id 101) (:heading-id 102)))))
	    (let ((ps (dorg-test-api--patches
		       '(on (where (property "STATUS" "OPEN")) (set (property "STATUS") "DONE"))
		       (list :file (dorg-test-api--unit-file)))))
	      (should (= 2 (length ps)))
	      (should (equal '(:property :host (:heading :id 101) :name "STATUS")
			     (plist-get (nth 0 ps) :target)))
	      (should (equal '(:property :host (:heading :id 102) :name "STATUS")
			     (plist-get (nth 1 ps) :target)))))))

(ert-deftest dorg-test-api-explicit-descriptor-never-expands-from-current-buffer ()
  "An explicit source descriptor must not fall back to implicit current-buffer.
The current buffer intentionally has a matching heading.  A bug that maps an
explicit descriptor to nil will let `dorg-ql-select' read that implicit buffer
and produce a patch for DORG_ID 777."
  (let ((file (make-temp-file "dorg-api-implicit-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Implicit match\n:PROPERTIES:\n:DORG_ID: 777\n:STATUS: OPEN\n:END:\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (org-mode)
                  (should-error
                   (dorg-api-normalize
                    '(on (where (property "STATUS" "OPEN"))
                         (set (property "STATUS") "DONE"))
                    '(:dolt-dir "/definitely/not/a/dolt/repo" :root-id 1)))
                  (should-error
                   (dorg-api-normalize
                    '(on (where (property "STATUS" "OPEN"))
                         (set (property "STATUS") "DONE"))
                    '(:file "/definitely/not/the/current-buffer.org"))))
              (when (buffer-live-p buf) (kill-buffer buf)))))
      (delete-file file))))

(ert-deftest dorg-test-api-where-one-resolves-single-host ()
  "AC-3 (tracked-case): (on (where (one Q)) ...) drives the REAL `(one)'
cardinality wrapper through the mutate selector — exactly-one resolves to a
single patch on that host; a >1 match surfaces the same `dorg-ql-error' from
selector expansion (no partial apply).  The mock delegates to the real model
evaluator so the assertion actually runs."
  ;; A8/source-optional migration: query-scoped targeting resolves SOURCE (for the
  ;; policy/none check), so drive it through an EXPLICIT file descriptor under
  ;; identity=dorg rather than an implicit nil source (which would resolve to the
  ;; bufferless batch buffer).  The mock still delegates to the real model
  ;; evaluator so the cardinality wrapper genuinely runs.
  (let* ((file (make-temp-file "dorg-api-where-one" nil ".org"
                               "* A\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"))
         (src (list :file file :identity 'dorg))
         (headings
          (list (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "A" :priority "A")
                (make-dorg-heading :id 2 :level 1 :level-offset 1 :title "B" :priority "A")
                (make-dorg-heading :id 5 :level 1 :level-offset 1 :title "C" :priority "B"))))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-ql-select)
                   (lambda (q &rest _)
                     (dorg-ql--model-eval (dorg-ql-normalize q) headings))))
          ;; exactly one (id 2) -> one patch on that host, no expected-targets ceremony
          (let ((ps (dorg-test-api--patches
                     '(on (where (one (id 2))) (set (property "P") "1")) src)))
            (should (= 1 (length ps)))
            (should (equal '(:heading :id 2)
                           (dorg-api--tget (plist-get (car ps) :target) :host))))
          ;; two priority-A headings -> cardinality error propagates unwrapped
          (should-error (dorg-test-api--patches
                         '(on (where (one (field priority "A"))) (set (property "P") "1")) src)
                        :type 'dorg-ql-error))
      (delete-file file))))

(ert-deftest dorg-test-api-key-idbearing-projects-heading-id ()
  "AC-4 (tracked-case): direct (key V) on an id-bearing source rewrites
(key V) to (id V) through the shared seam and projects to the heading id."
  (dorg-test-api--with-unit-files
   (let ((seen nil))
     (cl-letf (((symbol-function 'dorg-ql-select)
		(lambda (q &rest _) (setq seen q) '((:heading-id 42)))))
	      (let ((ps (dorg-api-normalize
			 '(on (key "42") (set (property "S") "X"))
			 (list :file (dorg-test-api--unit-file)))))
		(should (equal '(id "42") seen))
		(should (= 1 (length ps)))
		(should (equal '(:heading :id 42)
			       (dorg-api--tget (plist-get (car ps) :target) :host))))))))

(ert-deftest dorg-test-api-key-no-binding-is-invalid-target ()
  "AC-4 (tracked-case): direct mutate (key V) on a source that resolves to NO
binding identity (nil / unrecognized descriptor) PROPAGATES a structured
`dorg-mutate-error'/`invalid-target' straight from source resolution — NEVER a
leaked `dorg-ql-error', never degraded to an (id V) guess, and never collapsed
to a stale dorg-api-layer reason (the resolver call is un-caught).  Non-vacuity:
before tri-state resolution this leaked `dorg-ql-error: id must be integer'."
  (cl-flet ((reason+detail (src)
              (condition-case err
                  (progn (dorg-api-normalize
                          '(on (key "x") (set (property "P") "1")) src)
                         :no-error)
                (dorg-mutate-error
                 (cons (dorg-mutate-error-reason err)
                       (plist-get (dorg-mutate-error-data err) :detail))))))
    ;; nil source -> current buffer has no backing storage -> propagated
    ;; dorg-mutate-error/invalid-target (buffer name is context-specific).
    (should (eq 'invalid-target (car (reason+detail nil))))
    ;; unrecognized descriptor -> propagated invalid-target with the EXACT
    ;; `unknown-descriptor-key' detail (structured, no collapse).
    (should (equal '(invalid-target . unknown-descriptor-key)
                   (reason+detail '(:bogus t))))))

;; Regression (dorg-api-normalize source-optional): pure IR normalization needs
;; NO source.  A structural (id N) form lowers to the exact typed patch when
;; SOURCE is nil — the identity-key write-guard (which resolves SOURCE) is a
;; write-time concern and is not run for a source-less normalize.
(ert-deftest dorg-test-api-normalize-nil-source-yields-typed-ir ()
  (let ((ps (dorg-api-normalize '(on (id 100) (set (property "P") "1")) nil)))
    (should (= 1 (length ps)))
    (should (equal '(:op put :target (:property :host (:heading :id 100) :name "P") :value "1")
                   (car ps)))))

;; Regression (no-collapse): an EXPLICIT nonnil source is resolved by the guard
;; and a malformed descriptor's structured `dorg-mutate-error' PROPAGATES
;; uncaught — exact reason `invalid-target' + detail `unknown-descriptor-key',
;; never swallowed to a default.  Uses an (id N) target so the guard's resolve is
;; the SOLE source resolution (proving the guard fires for a nonnil source).
(ert-deftest dorg-test-api-normalize-malformed-source-propagates-structured ()
  (should (equal '(invalid-target . unknown-descriptor-key)
                 (condition-case err
                     (progn (dorg-api-normalize
                             '(on (id 100) (set (property "P") "1")) '(:bogus t))
                            :no-error)
                   (dorg-mutate-error
                    (cons (dorg-mutate-error-reason err)
                          (plist-get (dorg-mutate-error-data err) :detail)))))))

;;; ─── AC5: dry-run canonical response (reshape adds :intent + :patches) ────

(ert-deftest dorg-test-api-dry-run-response-shape ()
  "mutate --dry-run returns the canonical response: :ok, :command mutate,
:mode dry-run, :intent (the forms), :patches (typed IR), :targets, and a
base token.  The retired lowered `:ops' echo is absent."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
              (lambda (&rest _)
		'(:ok t :dry-run t :source-kind plain-org :root-id 1
                      :base-revision "tok" :supported t :unsupported nil))))
	    (let ((r (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				       :mode 'dry-run :source (list :file (dorg-test-api--unit-file)))))
	      (should (eq t (plist-get r :ok)))
	      (should (eq 'mutate (plist-get r :command)))
	      (should (eq 'dry-run (plist-get r :mode)))
	      (should (equal '((on (id 100) (set (property "STATUS") "DONE")))
			     (plist-get r :intent)))
	      (should (plist-get r :patches))
	      (should-not (plist-member r :ops))
	      (should (member 100 (plist-get r :targets)))
	      (should (equal "tok" (plist-get r :base-revision)))))))

(ert-deftest dorg-test-api-dry-run-occurrence-base-is-wire-stable ()
  "A dry-run response exposes the same one-based occurrence as its request."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
              (lambda (&rest _)
		'(:ok t :dry-run t :source-kind plain-org :root-id 1
                      :base-revision "tok" :supported t :unsupported nil))))
	    (let* ((r (dorg-api-execute
		       '(on (id 100)
			    (replace (keyword "REVIEWERS" (occurrence 1)) "alice"))
		       :mode 'dry-run :source (list :file (dorg-test-api--unit-file))))
		   (patch (car (plist-get r :patches))))
	      (should
	       (equal '(:keyword :host (:heading :id 100)
				 :name "REVIEWERS" :select (:occurrence 1))
		      (plist-get patch :target)))
	      (should (equal (plist-get r :patches)
			     (dorg-api-normalize
			      '(on (id 100)
				   (replace (keyword "REVIEWERS" (occurrence 1))
					    "alice")))))))))

(ert-deftest dorg-test-2rx9n-api-dry-run-text-selectors-are-wire-stable ()
  "A dry-run response echoes text collection and occurrence selectors intact."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
              (lambda (&rest _)
		'(:ok t :dry-run t :source-kind plain-org :root-id 1
                      :base-revision "tok" :supported t :unsupported nil))))
	    (let* ((intent '(batch
			     (on (id 100) (insert (text) (after-last) "new"))
			     (on (id 100)
				 (replace (text (occurrence 1)) "rewritten"))))
		   (response (dorg-api-execute intent :mode 'dry-run
					       :source (list :file (dorg-test-api--unit-file))))
		   (patches (plist-get response :patches)))
	      (should
	       (equal '((:op insert :target (:text :host (:heading :id 100))
			     :position :after-last :value "new")
			(:op replace
			     :target (:text :host (:heading :id 100) :select (:occurrence 1))
			     :value "rewritten"))
		      patches))
	      (should (equal patches (dorg-api-normalize intent)))))))

;;; ─── Native typed engine boundary ───────────────────────────────────────
;; These tests spy on the private boundary and assert that the public API sends
;; op/target-bearing typed patches without another semantic representation.

(ert-deftest dorg-test-api-execute-dry-run-speaks-typed-ir-to-engine ()
  "mutate --dry-run calls the IR-native boundary `dorg-mutate--plan-patches'
with typed PATCHES (op/target-bearing plists)."
  (dorg-test-api--with-unit-files
   (let (seen-patches)
     (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
		(lambda (_source patches _ext-inputs &rest _)
                  (setq seen-patches patches)
                  '(:ok t :dry-run t :source-kind plain-org :root-id 1
			:base-revision "tok" :supported t :unsupported nil))))
	      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				:mode 'dry-run :source (list :file (dorg-test-api--unit-file)))
	      (should seen-patches)
	      (should (plist-member (car seen-patches) :op))
	      (should (plist-member (car seen-patches) :target))))))

(ert-deftest dorg-test-api-execute-apply-speaks-typed-ir-to-engine ()
  "mutate --apply calls the IR-native boundary `dorg-mutate--apply-patches'
with typed PATCHES."
  (dorg-test-api--with-unit-files
   (let (seen-patches)
     (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
		(lambda (_source patches _ext-inputs &rest _)
                  (setq seen-patches patches)
                  '(:ok t :dry-run nil :source-kind plain-org :root-id 1
			:revision "tok2" :created nil :changed (100) :deleted nil))))
	      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				:mode 'apply :source (list :file (dorg-test-api--unit-file)))
	      (should seen-patches)
	      (should (plist-member (car seen-patches) :op))
	      (should (plist-member (car seen-patches) :target))))))

;;; ─── API execute option/response forwarding ─────────────────────────────

(ert-deftest dorg-test-api-execute-apply-forwards-save-option ()
  "An apply call preserves the public :save option for the mutation engine."
  (dorg-test-api--with-unit-files
   (let (seen-options)
     (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
		(lambda (_source _patches _ext-inputs &rest options)
                  (setq seen-options options)
                  '(:ok t :dry-run nil :source-kind plain-org :root-id 1
			:revision "tok2" :created nil :changed (100) :deleted nil))))
	      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				:mode 'apply :source (list :file (dorg-test-api--unit-file)) :save t)
	      (should (eq t (plist-get seen-options :save)))))))

(ert-deftest dorg-test-api-execute-apply-omits-save-when-unsupplied ()
  "Omitting :save does not synthesize a mutation-engine :save option."
  (dorg-test-api--with-unit-files
   (let (seen-options)
     (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
		(lambda (_source _patches _ext-inputs &rest options)
                  (setq seen-options options)
                  '(:ok t :dry-run nil :source-kind plain-org :root-id 1
			:revision "tok2" :created nil :changed (100) :deleted nil))))
	      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				:mode 'apply :source (list :file (dorg-test-api--unit-file)))
	      (should-not (plist-member seen-options :save))))))

(ert-deftest dorg-test-api-execute-apply-forwards-message-option ()
  "An apply call preserves the public :message option for the mutation engine."
  (let (seen-options)
    (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
               (lambda (_source _patches _ext-inputs &rest options)
                 (setq seen-options options)
                 '(:ok t :dry-run nil :source-kind dolt :root-id 1
                   :commit "dolt-tok2" :created nil :changed (100) :deleted nil))))
      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
                        :mode 'apply :source '(:dolt-dir "/repo/.dolt" :root-id 1)
                        :message "api commit message")
      (should (equal "api commit message" (plist-get seen-options :message))))))

(ert-deftest dorg-test-api-execute-apply-omits-message-when-unsupplied ()
  "Omitting :message lets the mutation engine retain its backend default."
  (let (seen-options)
    (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
               (lambda (_source _patches _ext-inputs &rest options)
                 (setq seen-options options)
                 '(:ok t :dry-run nil :source-kind dolt :root-id 1
                   :commit "dolt-tok2" :created nil :changed (100) :deleted nil))))
      (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
                        :mode 'apply :source '(:dolt-dir "/repo/.dolt" :root-id 1))
      (should-not (plist-member seen-options :message)))))

(ert-deftest dorg-test-api-execute-preserves-native-invalidation ()
  "The public response preserves the engine's native invalidation payload."
  (let ((native-invalidation
         '(:roots (1) :head "dolt-tok2" :dolt-dir "/repo/.dolt" :file nil)))
    (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (list :ok t :dry-run nil :source-kind 'dolt :root-id 1
                       :commit "dolt-tok2" :created nil :changed '(100) :deleted nil :invalidation native-invalidation))))
      (let ((r (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
                                 :mode 'apply
                                 :source '(:dolt-dir "/repo/.dolt" :root-id 1))))
        (should (equal native-invalidation (plist-get r :invalidation)))
        (should (equal "dolt-tok2" (plist-get r :new-token)))))))

(ert-deftest dorg-test-api-execute-omits-invalidation-when-engine-omits-it ()
  "The API response does not synthesize :invalidation for engines without it."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
              (lambda (&rest _)
		'(:ok t :dry-run nil :source-kind plain-org :root-id 1
                      :revision "tok2" :created nil :changed (100) :deleted nil))))
	    (let ((r (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
				       :mode 'apply :source (list :file (dorg-test-api--unit-file)))))
	      (should-not (plist-member r :invalidation))
	      (should (equal "tok2" (plist-get r :new-token)))))))

(ert-deftest dorg-test-api-execute-dry-run-does-not-apply-with-save-message ()
  "Supplying apply-only options to dry-run still invokes only the plan path."
  (dorg-test-api--with-unit-files
   (let (planned)
     (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
		(lambda (&rest _)
                  (setq planned t)
                  '(:ok t :dry-run t :source-kind plain-org :root-id 1
			:base-revision "tok" :supported t :unsupported nil
			:patches ((:op put
				       :target (:property :host (:heading :id 100)
							  :name "STATUS")
				       :value "DONE")))))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _) (error "must not reach apply engine"))))
	      (let ((r (dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
					 :mode 'dry-run :source (list :file (dorg-test-api--unit-file))
					 :save t :message "ignored for dry-run")))
		(should planned)
		(should (eq 'dry-run (plist-get r :mode))))))))

;;; ─── AC4: mode exclusivity (exactly one of dry-run / apply) ──────────────

(ert-deftest dorg-test-api-mode-neither-is-mode-error ()
  "A request without an execution mode is rejected before the writer gate."
  (dorg-test-api--with-unit-files
    (dorg-test-api--should-reason 'mode-error
      (dorg-api-execute '(on (id 1) (set (property "A") "1"))
                        :source (list :file (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-mode-both-is-mode-error ()
  "A request for both execution modes is rejected before the writer gate."
  (dorg-test-api--with-unit-files
    (dorg-test-api--should-reason 'mode-error
      (dorg-api-execute '(on (id 1) (set (property "A") "1"))
                        :mode 'both
                        :source (list :file (dorg-test-api--unit-file))))))

;;; ─── AC6: query-scoped apply safety gates ────────────────────────────────

(ert-deftest dorg-test-api-apply-query-without-expected-targets-errors ()
  "A query-scoped apply with no :expected-targets is expected-targets-required."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _) '((:heading-id 101))))
             ((symbol-function 'dorg-mutate--apply-patches)
              (lambda (&rest _) (error "must not reach the engine"))))
	    (dorg-test-api--should-reason 'expected-targets-required
					  (dorg-api-execute '(on (where (property "S" "O")) (set (property "S") "X"))
							    :mode 'apply :source (list :file (dorg-test-api--unit-file)))))))

(ert-deftest dorg-test-api-apply-query-with-save-message-still-requires-expected-targets ()
  "Forwarded apply options do not weaken query-scoped safety gates."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _) '((:heading-id 101))))
             ((symbol-function 'dorg-mutate--apply-patches)
              (lambda (&rest _) (error "must not reach the engine"))))
	    (dorg-test-api--should-reason 'expected-targets-required
					  (dorg-api-execute '(on (where (property "S" "O")) (set (property "S") "X"))
							    :mode 'apply :source (list :file (dorg-test-api--unit-file))
							    :save t :message "must not bypass guard")))))

(ert-deftest dorg-test-api-apply-query-target-set-changed ()
  "Apply re-expands the query; a set differing from :expected-targets (by
membership, order ignored) is target-set-changed."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _) '((:heading-id 101) (:heading-id 103))))
             ((symbol-function 'dorg-mutate--apply-patches)
              (lambda (&rest _) (error "must not reach the engine"))))
	    (dorg-test-api--should-reason 'target-set-changed
					  (dorg-api-execute '(on (where (property "S" "O")) (set (property "S") "X"))
							    :mode 'apply :expected-targets '(101 102)
							    :source (list :file (dorg-test-api--unit-file)))))))

(ert-deftest dorg-test-api-query-target-expansion-runs-under-saved-gate ()
  "A saved query's target set is resolved only after the file gate is held."
  (dorg-test-api--with-unit-files
   (let ((inside nil)
         (expanded nil))
     (cl-letf (((symbol-function 'dorg-file-lock-call)
		(lambda (_lock _timeout fn)
                  (setq inside t)
                  (unwind-protect
                      (list :status :acquired :value (funcall fn "unit-record"))
                    (setq inside nil))))
               ((symbol-function 'dorg-ql-select)
		(lambda (&rest _)
                  (should inside)
                  (setq expanded t)
                  '((:heading-id 101))))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _)
                  '(:ok t :dry-run nil :source-kind plain-org :root-id 1
			:revision "t2" :created nil :changed (101) :deleted nil))))
	      (should (plist-get
		       (dorg-api-execute
			'(on (where (property "S" "O")) (set (property "S") "X"))
			:mode 'apply :expected-targets '(101)
			:source (list :file (dorg-test-api--unit-file)))
		       :ok))
	      (should expanded)))))

(ert-deftest dorg-test-api-apply-query-expected-set-membership-order-ignored ()
  "A confirmed set matching by membership (different order) is NOT changed:
the guard passes and reaches the engine (folds tracked-case canonicalization)."
  (dorg-test-api--with-unit-files
   (let ((reached nil))
     (cl-letf (((symbol-function 'dorg-ql-select)
		(lambda (&rest _) '((:heading-id 101) (:heading-id 102))))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _)
                  (setq reached t)
                  '(:ok t :dry-run nil :source-kind plain-org :root-id 1
			:revision "t2" :created nil :changed (101 102) :deleted nil))))
	      (dorg-api-execute '(on (where (property "S" "O")) (set (property "S") "X"))
				:mode 'apply :expected-targets '(102 101) ; reversed
				:source (list :file (dorg-test-api--unit-file)))
	      (should reached)))))

(ert-deftest dorg-test-api-apply-one-selector-exempt-from-expected-targets ()
  "AC-6 (tracked-case): a bounded (where (one …)) apply asserts cardinality <= 1
and is EXEMPT from the :expected-targets ceremony — it reaches the engine
with no :expected-targets supplied."
  (dorg-test-api--with-unit-files
   (let ((reached nil))
     (cl-letf (((symbol-function 'dorg-ql-select)
		(lambda (&rest _) '((:heading-id 101))))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _)
                  (setq reached t)
                  '(:ok t :revision "t" :created nil :changed (101) :deleted nil))))
	      (dorg-api-execute '(on (where (one (property "S" "O"))) (set (property "S") "X"))
				:mode 'apply :source (list :file (dorg-test-api--unit-file)))
	      (should reached)))))

(ert-deftest dorg-test-api-apply-one-selector-exempt-nonvacuous ()
  "Non-vacuity for AC-6: with the bounded-selector predicate forced off, the
same (one …) apply demands :expected-targets — proving the exemption is what
lets it through."
  (dorg-test-api--with-unit-files
   (cl-letf (((symbol-function 'dorg-ql-select)
              (lambda (&rest _) '((:heading-id 101))))
             ((symbol-function 'dorg-api--selector-bounded-p) (lambda (&rest _) nil))
             ((symbol-function 'dorg-mutate--apply-patches)
              (lambda (&rest _) (error "must not reach the engine"))))
	    (dorg-test-api--should-reason 'expected-targets-required
					  (dorg-api-execute '(on (where (one (property "S" "O"))) (set (property "S") "X"))
							    :mode 'apply :source (list :file (dorg-test-api--unit-file)))))))

(ert-deftest dorg-test-api-apply-key-selector-exempt-from-expected-targets ()
  "AC-6 (tracked-case): a bounded direct (key V) apply asserts cardinality <= 1
and reaches the engine with no :expected-targets supplied."
  (dorg-test-api--with-unit-files
   (let ((reached nil))
     (cl-letf (((symbol-function 'dorg-ql-select)
		(lambda (&rest _) '((:heading-id 42))))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _)
                  (setq reached t)
                  '(:ok t :revision "t" :created nil :changed (42) :deleted nil))))
	      (dorg-api-execute '(on (key "42") (set (property "S") "X"))
				:mode 'apply :source (list :file (dorg-test-api--unit-file)))
	      (should reached)))))

(ert-deftest dorg-test-api-identity-violation-reason-registered ()
  "AC-8 (tracked-case): identity-violation is in the closed error-reason set and
round-trips through the accessor."
  (should (memq 'identity-violation dorg-api-error-reasons))
  (let ((err (should-error (dorg-api--raise 'identity-violation "dup" :n 2)
                           :type 'dorg-api-error)))
    (should (eq 'identity-violation (dorg-api-error-reason err)))))

(ert-deftest dorg-test-api-apply-broad-write-refused-over-threshold ()
  "An apply over more than `dorg-api-broad-write-threshold' confirmed targets
without :allow-broad is broad-write-refused."
  (dorg-test-api--with-unit-files
   (let* ((n (1+ dorg-api-broad-write-threshold))
          (ids (number-sequence 1 n))
          (hits (mapcar (lambda (i) (list :heading-id i)) ids)))
     (cl-letf (((symbol-function 'dorg-ql-select) (lambda (&rest _) hits))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _) (error "must not reach the engine"))))
	      (dorg-test-api--should-reason 'broad-write-refused
					    (dorg-api-execute '(on (where (property "S" "O")) (set (property "S") "X"))
							      :mode 'apply :expected-targets ids
							      :source (list :file (dorg-test-api--unit-file))))))))

;;; ─── tracked-case: deterministic expected-target set comparison ────────────
;; The query-scoped apply guard (dorg-api.el) compares the re-expanded target
;; set against :EXPECTED-TARGETS by SET MEMBERSHIP — order-independent and
;; duplicate-independent.  Both sides are canonicalized to a sorted unique id
;; set (`dorg-api--patch-target-ids' for resolved targets; `delete-dups' + sort
;; for the expected input), so the guard cannot be order- or multiplicity-flaky.
;; These tests pin that determinism: duplicate/reordered/unchanged confirmations
;; pass to the engine, while missing/extra/changed sets are `target-set-changed'.

(defun dorg-test-api--guard-scenario (resolved expected file)
  "Run a query-scoped apply resolving to RESOLVED heading ids.
Use EXPECTED as :expected-targets and FILE as source.  Return `reached'
when the guard passes and the engine runs, else the signalled
`dorg-api-error' :reason symbol.  The engine is stubbed to a sentinel."
  (let ((reached nil))
    (cl-letf (((symbol-function 'dorg-ql-select)
               (lambda (&rest _)
                 (mapcar (lambda (i) (list :heading-id i)) resolved)))
              ((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (setq reached t)
                 (list :ok t :dry-run nil :source-kind 'plain-org :root-id 1
                       :revision "t2" :created nil :changed resolved :deleted nil))))
      (let ((reason (dorg-test-api--reason
                     (lambda ()
                       (dorg-api-execute
                        '(on (where (property "S" "O")) (set (property "S") "X"))
                        :mode 'apply :expected-targets expected
                        :source (list :file file))))))
        (if reached 'reached reason)))))

(ert-deftest dorg-test-api-apply-expected-duplicate-input-deduped ()
  "Duplicate ids in :expected-targets are stably de-duplicated before the
comparison; a set equal by membership still passes to the engine (AC2)."
  (dorg-test-api--with-unit-files
    (should (eq 'reached
                (dorg-test-api--guard-scenario
                 '(101 102) '(101 101 102) (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-apply-expected-set-not-multiset ()
  "The comparison is set membership, not multiset: repeated expected ids that
reduce to the resolved set pass (AC1/AC2)."
  (dorg-test-api--with-unit-files
    (should (eq 'reached
                (dorg-test-api--guard-scenario
                 '(101 102) '(101 102 102) (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-apply-expected-dedupe-does-not-mask-mismatch ()
  "De-duplicating duplicate expected input must not hide a real membership
difference: (101 101 103) vs resolved (101 102) is still target-set-changed."
  (dorg-test-api--with-unit-files
    (should (eq 'target-set-changed
                (dorg-test-api--guard-scenario
                 '(101 102) '(101 101 103) (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-apply-missing-target-is-changed ()
  "A resolved set MISSING an expected id (resolved is a proper subset of
expected) is target-set-changed (AC3)."
  (dorg-test-api--with-unit-files
    (should (eq 'target-set-changed
                (dorg-test-api--guard-scenario
                 '(101) '(101 102) (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-apply-extra-target-is-changed ()
  "A resolved set with an EXTRA id beyond expected (resolved is a proper
superset of expected) is target-set-changed (AC3)."
  (dorg-test-api--with-unit-files
    (should (eq 'target-set-changed
                (dorg-test-api--guard-scenario
                 '(101 102 103) '(101 102) (dorg-test-api--unit-file))))))

(ert-deftest dorg-test-api-apply-expected-exact-unchanged-passes ()
  "An exact same-order membership match passes to the engine (unchanged set);
complements the reversed-order pass to pin order-independence in both
directions (AC1)."
  (dorg-test-api--with-unit-files
    (should (eq 'reached
                (dorg-test-api--guard-scenario
                 '(101 102) '(101 102) (dorg-test-api--unit-file))))))

;;; ─── L2: public ordered item-sequence verb (tracked-regression) ───────────────
;;
;; The public `(replace (item (all)) ITEM…)` verb normalizes to one typed patch whose
;; item sequence preserves interleaved text/keyword/planning/drawer order.
;;
;; Item forms:  (text "S") | (keyword "K" "V")
;;            | (planning :scheduled S? :deadline D? :closed C?)
;;            | (drawer "NAME" (K V)…) | (drawer "NAME" :lines ("L"…))

(defun dorg-test-api--replace-items-patch (form)
  "Normalize FORM to exactly one typed item replacement patch and return it."
  (let ((patches (dorg-api-normalize form nil)))
    (should (= 1 (length patches)))
    (car patches)))

(defun dorg-test-api--canon-items (patch)
  "Return PATCH's canonical typed item values."
  (plist-get patch :items))

;; L2-a — the public verb normalizes to one `replace :item` patch whose items
;; are canonical drawer/text/keyword values.
(ert-deftest dorg-test-api-replace-items-normalizes-to-one-patch ()
  (let* ((form '(on (id 42)
                    (replace (item (all))
                             (drawer "PROPERTIES" ("BEAD_ID" "a-1"))
                             (text "Body.")
                             (keyword "FOO" "solo"))))
         (patch (dorg-test-api--replace-items-patch form)))
    (should (eq 'replace (plist-get patch :op)))
    (should (equal '(:item :host (:heading :id 42) :select (:all))
                   (plist-get patch :target)))
    (should (equal
             '((:kind drawer :name "PROPERTIES" :properties (("BEAD_ID" . "a-1"))
                :post-blank 0)
               (:kind text :text "Body." :post-blank 0)
               (:kind keyword :keyword "FOO" :value "solo" :post-blank 0))
             (dorg-test-api--canon-items patch)))))

;; L2-b — planning + text items round-trip through the ordered sequence in
;; document order (the sequence is ordered, not set-like).
(ert-deftest dorg-test-api-replace-items-ordered-planning-text ()
  (let* ((form '(on (id 7)
                    (replace (item (all))
                             (planning :scheduled "<2026-07-13 Mon>")
                             (text "After planning."))))
         (patch (dorg-test-api--replace-items-patch form)))
    (should (equal
             '((:kind planning :scheduled "<2026-07-13 Mon>" :post-blank 0)
               (:kind text :text "After planning." :post-blank 0))
             (dorg-test-api--canon-items patch)))))

;; L2-c — an empty sequence and a protected keyword item are invalid targets.
;; Public items clearing is unsupported: replacing requires at least one item.
(ert-deftest dorg-test-cyvpd-empty-replace-items-is-invalid-target ()
  "A recognized item target with its required value missing is invalid-target."
  (let ((err (should-error
              (dorg-api-normalize '(on (id 1) (replace (item (all)))) nil)
              :type 'dorg-api-error)))
    (should (eq 'invalid-target (dorg-api-error-reason err)))
    (should (equal '(:reason invalid-target :action (replace (item (all))))
                   (dorg-api-error-data err)))))

(ert-deftest dorg-test-cyvpd-empty-replace-items-refuses-before-engine ()
  "Empty item selection refuses before either public execution mode reaches the engine."
  (dorg-test-api--with-unit-files
   (let (called)
     (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
		(lambda (&rest _)
                  (setq called t)
                  (error "must not plan")))
               ((symbol-function 'dorg-mutate--apply-patches)
		(lambda (&rest _)
                  (setq called t)
                  (error "must not apply"))))
	      (dolist (mode '(dry-run apply))
		(dorg-test-api--should-reason 'invalid-target
					      (dorg-api-execute '(on (id 1) (replace (item (all))))
								:mode mode :source (list :file (dorg-test-api--unit-file)))))
	      (should-not called)))))

(ert-deftest dorg-test-api-replace-items-rejects-protected-keyword ()
  (dorg-test-api--should-reason 'invalid-target
    (dorg-api-normalize '(on (id 1) (replace (item (all)) (keyword "DORG_ID" "5"))) nil)))

(ert-deftest dorg-test-api-owned-metadata-refuses-every-generic-property-form ()
  "No public generic form reaches the unavailable clock-owner boundary."
  (dolist (form
           '((on (id 1) (set (property "DORG_CLOCK_START") "2026-10-14T08:00:00Z"))
             (on (id 1) (delete (property "dorg_clock_start")))
             (on (id 1) (set (property "DORG_CLOCK_START+") "accumulate"))
             (insert (heading (title "Clock")
                              (property "DORG_CLOCK_START" "2026-10-14T08:00:00Z"))
                     (under (id 1)))))
    (let ((err (should-error (dorg-api-normalize form nil) :type 'dorg-api-error)))
      (should (eq 'invalid-target (dorg-api-error-reason err)))
      (should-not (plist-member (dorg-api-error-data err) :detail)))))

;;; ─── B5 Option A: no-buffer Dolt binding is queryable for target expansion ──
;; A resolved (:dolt-dir … :root-id …) binding carries no buffer/file, and
;; `dorg-ql-select' reaches Dolt only through a registration.  `dorg-api--ql-source'
;; synthesizes an UNREGISTERED `dorg-registration' from the SAME resolved source and
;; threads it in, so query-scoped mutation targeting a no-buffer Dolt source expands
;; through the real backend instead of raising a raw type error (the pre-fix defect).

(ert-deftest dorg-test-api-dolt-no-buffer-where-target-expansion ()
  "Public dry-run over an explicit NO-BUFFER Dolt descriptor with a (where (title …))
target expands through the synthesized registration to the exact heading id — exact
patch/target/plan, no raw type error — and mutates nothing (registry unchanged)."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Target Heading\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (registry-before (copy-sequence dorg--registry))
           (src (list :dolt-dir dd :root-id root-id))      ; NO :buffer
           (resp (dorg-api-execute
                  '(on (where (title "Target Heading")) (set (property "STATUS") "DONE"))
                  :mode 'dry-run :source src)))
      ;; Exact target expansion + canonical target + supported plan, no raw error.
      (should (plist-get resp :ok))
      (should (eq 'dry-run (plist-get resp :mode)))
      (should (equal '(200) (plist-get resp :targets)))
      (let ((ps (plist-get resp :patches)))
        (should (= 1 (length ps)))
        (should (equal '(:property :host (:heading :id 200) :name "STATUS")
                       (plist-get (car ps) :target)))
        (should (equal "DONE" (plist-get (car ps) :value))))
      (should (eq t (plist-get resp :supported)))
      ;; No registry mutation: the synthesized registration is unregistered.
      (should (equal registry-before dorg--registry)))))

(ert-deftest dorg-test-api-dolt-ql-source-synthesizes-unregistered-registration ()
  "`dorg-api--ql-source' turns a resolved no-buffer Dolt source into an UNREGISTERED
`dorg-registration' carrying the same dolt-dir/root-id and the RESOLVED identity —
without a file/buffer and without mutating `dorg--registry'."
  (let* ((dorg--registry nil)
         (src (list :dolt-dir "/tmp/nonexistent-dolt/.dolt/" :root-id 7
                    :identity '(:property "BEAD_ID")))
         (resolved (dorg-mutate--resolve-source src))
         (ql (dorg-api--ql-source resolved)))
    (should (dorg-registration-p ql))
    (should (null dorg--registry))                        ; registry untouched
    (should (null (dorg-registration-file ql)))           ; no file/buffer
    (let ((backend (dorg-registration-backend ql)))
      (should (eq 'dolt (dorg-backend-type backend)))
      (should (equal (file-name-as-directory
                      (expand-file-name "/tmp/nonexistent-dolt/.dolt"))
                     (dorg-backend-dolt-dir backend)))
      (should (equal 7 (dorg-backend-root-id backend))))
    ;; Resolved identity is carried onto the synthesized registration.
    (should (equal '(:property "BEAD_ID")
                   (dorg-mutate--source-identity
                    (dorg-mutate--source-from-registration ql nil))))))

(ert-deftest dorg-test-api-dolt-no-buffer-where-apply-persists ()
  "PUBLIC apply over an explicit NO-BUFFER Dolt descriptor with a (where (title …))
target and :expected-targets CAS persists the exact value: wrong initial STATUS,
DONE after reload, before!=after, HEAD advances, canonical target pinned, with NO new
visiting buffer and NO registry mutation around the public call.  This makes the
expansion non-vacuous (a real write reaches the store, not just a dry-run plan)."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n* Target Heading\n:PROPERTIES:\n:DORG_ID: 200\n:STATUS: TODO\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id))       ; NO :buffer
           (qlsrc (dorg-api--ql-source (dorg-mutate--resolve-source src))))
      ;; Wrong initial state: STATUS=DONE selects nothing yet (before).
      (should (null (dorg-ql-select '(property "STATUS" "DONE") qlsrc)))
      (let ((head-before (dorg-head-commit dd))
            (registry-before (copy-sequence dorg--registry))
            (buffers-before (buffer-list))
            (resp (dorg-api-execute
                   '(on (where (title "Target Heading")) (set (property "STATUS") "DONE"))
                   :mode 'apply :source src :expected-targets '(200))))
        (should (plist-get resp :ok))
        (should (eq 'apply (plist-get resp :mode)))
        (should (equal '(200) (plist-get resp :targets)))   ; canonical target pinned
        (should (member 200 (plist-get (plist-get resp :affected) :changed)))
        ;; No new visiting buffer + no registry mutation around the public call.
        (should (equal buffers-before (buffer-list)))
        (should (equal registry-before dorg--registry))
        ;; HEAD advanced (the write was committed).
        (should-not (equal head-before (dorg-head-commit dd))))
      ;; Reload from the store: exactly heading 200 now has STATUS=DONE (after).
      (let ((hits (dorg-ql-select '(property "STATUS" "DONE") qlsrc)))
        (should (= 1 (length hits)))
        (should (= 200 (plist-get (car hits) :heading-id)))))))

(ert-deftest dorg-test-api-dolt-property-identity-through-public-api ()
  "An explicit Dolt descriptor's (:property K) identity survives THROUGH the public
API: a direct `(key V)' target on a real Dolt store rewrites to the property key
K, selects the matching heading, and projects to the EXACT canonical extern target
and identity — only reachable if the descriptor identity is not lost to the derived
`dorg' default (which would rewrite (key V) to an id lookup and miss)."
  (dorg-test-ql--with-dolt-synced
      "* First\n:PROPERTIES:\n:DORG_ID: 400\n:BEAD_ID: a-1\n:END:\n* Second\n:PROPERTIES:\n:DORG_ID: 401\n:BEAD_ID: a-2\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id :identity '(:property "BEAD_ID")))
           (resp (dorg-api-execute
                  '(on (key "a-1") (set (property "STATUS") "DONE"))
                  :mode 'dry-run :source src)))
      (should (plist-get resp :ok))
      (let ((ps (plist-get resp :patches)))
        (should (= 1 (length ps)))
        (should (equal '(:property
                         :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                         :name "STATUS")
                       (plist-get (car ps) :target))))
      ;; Canonical extern identity is pinned (property policy survived the API).
      (should (equal '((:extern :key "BEAD_ID" :value "a-1"))
                     (plist-get resp :targets))))))

(ert-deftest dorg-test-api-51gsn-dolt-keyless-id-projection-applies ()
  "Dolt matches plain-org: descriptive keyless hits project and apply by ID."
  (dorg-test-ql--with-dolt-synced
      "* Notes\n:PROPERTIES:\n:DORG_ID: 410\n:STATUS: OPEN\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root-id
                      :identity '(:property "BEAD_ID")))
           (form '(on (where (property "STATUS" "OPEN"))
                      (set (property "STATUS") "DONE")))
           (base (dorg-head-commit dd))
           (dry (dorg-api-execute form :mode 'dry-run :source src)))
      (should (equal '(410) (plist-get dry :targets)))
      (let ((resp
             (dorg-api-execute form :mode 'apply :source src
                               :base (plist-get dry :base)
                               :expected-targets (plist-get dry :targets))))
        (should (plist-get resp :ok))
        (should (equal '(410) (plist-get resp :targets)))
        (should-not (equal base (dorg-head-commit dd))))
      (let ((heading (gethash 410 (dorg-model-load-headings dd root-id nil))))
        (should heading)
        (should (equal "DONE"
                       (dorg-test-api--heading-property heading "STATUS")))
        (should-not (dorg-test-api--heading-property heading "BEAD_ID"))))))

(ert-deftest dorg-test-api-dolt-descriptor-identity-survives ()
  "An explicit Dolt descriptor's :identity policy survives resolution (item 4):
`(:dolt-dir … :root-id … :identity (:property K))' reports (:property K) through the
single resolver; a nil :identity derives `dorg'."
  (should (equal '(:property "BEAD_ID")
                 (dorg-mutate--source-identity
                  (dorg-mutate--resolve-source
                   '(:dolt-dir "/tmp/x/.dolt/" :root-id 1 :identity (:property "BEAD_ID"))))))
  (should (eq 'dorg
             (dorg-mutate--source-identity
              (dorg-mutate--resolve-source '(:dolt-dir "/tmp/x/.dolt/" :root-id 1))))))

(ert-deftest dorg-test-api-dolt-descriptor-malformed-identity ()
  "A malformed explicit :identity on a Dolt descriptor signals the EXACT structured
error at resolution — reason invalid-target, detail identity-policy-malformed."
  (let ((outcome (condition-case err
                     (progn (dorg-mutate--resolve-source
                             '(:dolt-dir "/tmp/x/.dolt/" :root-id 1 :identity bogus))
                            :no-error)
                   (dorg-mutate-error
                    (cons (dorg-mutate-error-reason err)
                          (plist-get (dorg-mutate-error-data err) :detail))))))
    (should (equal '(invalid-target . identity-policy-malformed) outcome))))

(ert-deftest dorg-test-api-dolt-public-base-commit-round-trip ()
  "Backend-aware public :base for Dolt: a dry-run returns the engine commit token
and NO :base-revision; after an EXTERNAL HEAD advance that changes the SAME
heading this apply writes, an apply carrying the OLD token forwards it ONLY as
:base-commit and fails-closed terminal `overlap' BEFORE any write (tracked-regression
§2.4 first-committer-wins; the old blanket `stale' is replaced by the per-store
gate's overlap/re-plan contract), leaving the external HEAD retained.  Public API
+ Dolt infra only (no private token plumbing)."
  (dorg-test-ql--with-dolt-synced
      "* Target\n:PROPERTIES:\n:DORG_ID: 500\n:STATUS: OPEN\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root (dorg-backend-root-id (dorg-registration-backend reg)))
           (src (list :dolt-dir dd :root-id root))
           (form '(on (id 500) (set (property "STATUS") "DONE")))
           ;; EXACT prewrite HEAD captured via test Dolt infra (not the API),
           ;; so the dry-run token is pinned to a value a constant-bogus public
           ;; base could not satisfy.
           (head0 (dorg-head-commit dd))
           (dry (dorg-api-execute form :mode 'dry-run :source src))
           (base0 (plist-get dry :base)))
      (should base0)                                   ; dry-run commit token present
      (should (equal head0 base0))                     ; == the EXACT prewrite HEAD
      ;; A fresh dry-run pins the legacy/internal response fields nil: the public
      ;; :base carries the token, not :base-commit / :base-revision.
      (should (null (plist-get dry :base-commit)))     ; no legacy :base-commit
      (should (null (plist-get dry :base-revision)))   ; no :base-revision for Dolt
      ;; EXTERNAL HEAD advance (not via the API under test).
      (dorg-sql-batch dd (list "UPDATE headings SET title = 'Bumped' WHERE id = 500"))
      (dorg-commit dd "external bump")
      (let ((head1 (dorg-head-commit dd))
            (orig (symbol-function 'dorg-mutate--apply-patches))
            (seen-opts 'unset))
        (should-not (equal base0 head1))               ; HEAD advanced past the token
        ;; SPY that DELEGATES to the real engine: capture the exact options the
        ;; apply forwards, while the genuine Dolt base-commit CAS still runs.
        (cl-letf (((symbol-function 'dorg-mutate--apply-patches)
                   (lambda (s o &optional ei &rest options)
                     (setq seen-opts options)
                     (apply orig s o ei options))))
          (let ((err (condition-case e
                         (progn (dorg-api-execute form :mode 'apply :source src :base base0)
                                nil)
                       (dorg-mutate-error e))))
            ;; EXACT structured engine gate error (not reason alone): reason
            ;; `overlap' (the external bump changed heading 500, which this intent
            ;; writes), the supplied OLD token as :base-commit, the current HEAD as
            ;; :commit, and heading 500 named in the :overlap evidence.
            (should err)
            (should (eq 'overlap (dorg-mutate-error-reason err)))
            (let ((data (dorg-mutate-error-data err)))
              (should (equal base0 (plist-get data :base-commit)))
              (should (equal head1 (plist-get data :commit)))
              (should (member 500 (plist-get data :overlap))))))
        ;; The apply forwarded the OLD token EXACTLY as :base-commit and NO
        ;; :base-revision (Dolt never uses the plain-org revision token).
        (should (equal base0 (plist-get seen-opts :base-commit)))
        (should-not (plist-member seen-opts :base-revision))
        ;; external HEAD retained: the rejected apply committed nothing.
        (should (equal head1 (dorg-head-commit dd)))))))

(ert-deftest dorg-test-api-public-base-stale-cas ()
  "The NEW D5 public :base CAS is load-bearing: a dry-run captures the literal file
token; the bound file is then externally changed (mtime forced distinct); apply with
the STALE :base fails-closed with EXACTLY `stale' / detail `base-revision' (the D5
table binding) BEFORE any write, and the file is left byte-identical (no
partial/extra write).  Fails if the public-base validation block is removed."
  (let* ((file (make-temp-file "dorg-api-base-cas-" nil ".org"))
         (initial "* TODO Alpha\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nBody.\n")
         (external "* TODO Alpha EXTERNALLY EDITED\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nNew body.\n")
         (form '(on (where (property "BEAD_ID" "a-1")) (set (property "STATUS") "DONE")))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert initial))
          (let* ((dry (dorg-api-execute form :mode 'dry-run :source src))
                 (stale-base (plist-get dry :base)))
            (should (eq 'file (plist-get stale-base :kind)))
            ;; External change + a FORCED distinct mtime so the current public
            ;; token provably differs regardless of filesystem mtime resolution.
            (with-temp-file file (insert external))
            (set-file-times file (time-add (current-time) 10))
            ;; apply with the STALE :base must fail-closed with exact classification.
            (let ((outcome (condition-case err
                               (progn (dorg-api-execute
                                       form :mode 'apply :source src
                                       :base stale-base :save t
                                       :expected-targets (plist-get dry :targets))
                                      :no-error)
                             (dorg-api-error
                              (cons (dorg-api-error-reason err)
                                    (plist-get (dorg-api-error-data err) :detail))))))
              (should (equal '(stale . base-revision) outcome)))
            ;; NO write: the file is exactly the externally-changed bytes.
            (should (equal external (with-temp-buffer (insert-file-contents file)
                                                      (buffer-string))))))
      (delete-file file))))

;;; ─── First-class immutable mutation intent (tracked-regression) ─────────────

(ert-deftest dorg-test-fhhwd-intent-copy-refuses-cyclic-cons-spines ()
  "Unsupported circular forms/options refuse without formatting cyclic data."
  (let* ((max-lisp-eval-depth 1000)
         (form (list 'batch))
         (options (list :message "cyclic")))
    (setcdr form form)
    (setcdr (cdr options) options)
    (should-error (dorg-api-make-mutation-intent form) :type 'circular-list)
    (should-error (dorg-api-make-mutation-intent '(batch) :base options)
                  :type 'circular-list)))

(ert-deftest dorg-test-fhhwd-wide-intent-constructor-and-accessor-own-form ()
  "A wide valid public batch is copied at the runner's unchanged depth bound."
  (let* ((max-lisp-eval-depth 1000)
         (form (cons 'batch
                     (cl-loop for id from 1 to 512
                              collect (list 'on (list 'id id)
                                            (list 'set
                                                  (list 'property
                                                        (copy-sequence "K"))
                                                  (copy-sequence "V"))))))
         (intent (dorg-api-make-mutation-intent form))
         (inspection (dorg-api-mutation-intent-form intent)))
    (should (equal form inspection))
    (should (= 512 (length (cdr inspection))))
    ;; Mutating both the original spine and its last element must be isolated.
    (aset (nth 2 (nth 2 (car (last form)))) 0 ?X)
    (setcar (cdr form) '(on (id 999) (set (property "K") "X")))
    (should (equal '(on (id 1) (set (property "K") "V"))
                   (cadr (dorg-api-mutation-intent-form intent))))
    (should (equal "V"
                   (nth 2 (nth 2
                               (car (last
                                     (dorg-api-mutation-intent-form intent)))))))
    ;; Accessor copies have the same spine and element ownership law.
    (aset (nth 2 (nth 2 (car (last inspection)))) 0 ?Y)
    (setcdr inspection nil)
    (let ((again (dorg-api-mutation-intent-form intent)))
      (should (= 512 (length (cdr again))))
      (should (equal "V" (nth 2 (nth 2 (car (last again)))))))))

(ert-deftest dorg-test-fhhwd-intent-copy-preserves-dotted-vector-ownership ()
  "Public option copies detach dotted tails and vectors but retain handles."
  (with-temp-buffer
    (let* ((handle (current-buffer))
           (head (copy-sequence "head"))
           (tail (copy-sequence "tail"))
           (vector (vector (cons head tail) handle
                           (vector (copy-sequence "nested"))))
           (intent (dorg-api-make-mutation-intent
                    '(on (id 1) (set (property "K") "V"))
                    :source handle :base vector))
           (options (dorg-api-mutation-intent-options intent))
           (copy (plist-get options :base)))
      (should (equal vector copy))
      (should-not (eq vector copy))
      (should-not (eq (aref vector 0) (aref copy 0)))
      (should (eq handle (plist-get options :source)))
      (should (eq handle (aref copy 1)))
      (aset head 0 ?X)
      (aset tail 0 ?X)
      (aset (aref (aref vector 2) 0) 0 ?X)
      (aset vector 1 nil)
      (should (equal '("head" . "tail") (aref copy 0)))
      (should (equal "nested" (aref (aref copy 2) 0)))
      (aset (cdr (aref copy 0)) 0 ?Y)
      (setcar (aref copy 0) "replaced")
      (aset (aref copy 2) 0 "replaced")
      (let* ((derived (dorg-api-mutation-intent-with intent :message "new"))
             (again (plist-get (dorg-api-mutation-intent-options derived)
                              :base)))
        (should (equal '("head" . "tail") (aref again 0)))
        (should (equal "nested" (aref (aref again 2) 0)))
        (should (eq handle (aref again 1)))
        (should-not (plist-member (dorg-api-mutation-intent-options intent)
                                  :message))))))

(ert-deftest dorg-test-nwlkl-intent-deep-owns-input-and-inspection ()
  "Mutation intent owns mutable input and returns detached inspection values."
  (let* ((key (copy-sequence "STATUS"))
         (value (copy-sequence "DONE"))
         (file (copy-sequence "/tmp/nwlkl.org"))
         (message (copy-sequence "nwlkl message"))
         (fingerprint (copy-sequence "base-fingerprint"))
         (extern-value (copy-sequence "bead-1"))
         (form (list 'on (list 'id 100)
                     (list 'set (list 'property key) value)))
         (source (list :file file :identity '(:property "BEAD_ID")))
         (base (list :kind 'file :fingerprint fingerprint))
         (targets (list (list :extern :key "BEAD_ID" :value extern-value)))
         (intent (dorg-api-make-mutation-intent
                  form :source source :base base :expected-targets targets
                  :save nil :message message :op-id "nwlkl-op")))
    (should (dorg-api-mutation-intent-p intent))
    ;; Mutate every caller-owned string after construction.
    (aset key 0 ?X)
    (aset value 0 ?X)
    (aset file 0 ?X)
    (aset message 0 ?X)
    (aset fingerprint 0 ?X)
    (aset extern-value 0 ?X)
    (should
     (equal '(on (id 100) (set (property "STATUS") "DONE"))
            (dorg-api-mutation-intent-form intent)))
    (let ((options (dorg-api-mutation-intent-options intent)))
      (should (equal '(:file "/tmp/nwlkl.org"
                       :identity (:property "BEAD_ID"))
                     (plist-get options :source)))
      (should (equal '(:kind file :fingerprint "base-fingerprint")
                     (plist-get options :base)))
      (should (equal '((:extern :key "BEAD_ID" :value "bead-1"))
                     (plist-get options :expected-targets)))
      (should (plist-member options :save))
      (should-not (plist-get options :save))
      (should (equal "nwlkl message" (plist-get options :message)))
      ;; Mutating inspection copies cannot reach the stored value either.
      (aset (plist-get (plist-get options :source) :file) 0 ?Y)
      (aset (nth 2 (nth 2 (dorg-api-mutation-intent-form intent))) 0 ?Y))
    (should (equal "/tmp/nwlkl.org"
                   (plist-get
                    (plist-get (dorg-api-mutation-intent-options intent)
                               :source)
                    :file)))
    (should (equal "DONE"
                   (nth 2 (nth 2
                               (dorg-api-mutation-intent-form intent)))))))

(ert-deftest dorg-test-nwlkl-intent-retains-live-buffer-identity ()
  "A buffer source is an external identity handle, not cloned mutable data."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (intent
            (dorg-api-make-mutation-intent
             '(on (id 1) (set (property "K") "V")) :source buf)))
      (should (eq buf
                  (plist-get (dorg-api-mutation-intent-options intent)
                             :source))))))

(ert-deftest dorg-test-nwlkl-intent-constructor-refuses-foreign-authority ()
  "The intent option domain is closed before source resolution or engine work."
  (let ((resolve-calls 0)
        (engine-calls 0)
        (form '(on (id 1) (set (property "K") "V"))))
    (cl-letf (((symbol-function 'dorg-mutate--resolve-source)
               (lambda (&rest _)
                 (cl-incf resolve-calls)
                 (ert-fail "intent construction resolved storage")))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 (cl-incf engine-calls)
                 (ert-fail "intent construction entered the engine"))))
      (dolist (option+value
               '((:mode dry-run)
                 (:extension-inputs ((probe . payload)))
                 (:read-set (:kind beads))
                 (:scope (:ids (1)))
                 (:candidate-state hidden)
                 (:sql "SELECT 1")
                 (:unknown t)))
        (dorg-test-api--should-reason 'malformed
          (apply #'dorg-api-make-mutation-intent
                 form option+value))))
    (should (= 0 resolve-calls))
    (should (= 0 engine-calls))))

(ert-deftest dorg-test-nwlkl-intent-with-is-functional-and-presence-safe ()
  "Functional derivation changes a new value and preserves option presence."
  (let* ((intent
          (dorg-api-make-mutation-intent
           '(on (id 1) (set (property "K") "V"))
           :source '(:file "/tmp/nwlkl.org") :save nil :message nil))
         (base (list :kind 'file :fingerprint (copy-sequence "approved")))
         (targets (list 1))
         (approved (dorg-api-mutation-intent-with
                    intent :base base :expected-targets targets
                    :message "approved message"))
         (original-options (dorg-api-mutation-intent-options intent))
         (approved-options (dorg-api-mutation-intent-options approved)))
    (should (dorg-api-mutation-intent-p approved))
    (should-not (eq intent approved))
    (should-not (plist-member original-options :base))
    (should-not (plist-member original-options :expected-targets))
    (should (plist-member original-options :save))
    (should (plist-member original-options :message))
    (should-not (plist-get original-options :message))
    (should (equal base (plist-get approved-options :base)))
    (should (equal targets (plist-get approved-options :expected-targets)))
    (should (equal "approved message" (plist-get approved-options :message)))
    (should (= 1 (cl-count :message approved-options)))
    ;; Functional updates deep-own their replacement values too.
    (aset (plist-get base :fingerprint) 0 ?X)
    (setcar targets 99)
    (should (equal "approved"
                   (plist-get
                    (plist-get (dorg-api-mutation-intent-options approved)
                               :base)
                    :fingerprint)))
    (should (equal '(1)
                   (plist-get (dorg-api-mutation-intent-options approved)
                              :expected-targets)))))

(ert-deftest dorg-test-nwlkl-intent-execute-refuses-option-overrides ()
  "An existing intent accepts only :mode; variants require functional derivation."
  (let ((source-calls 0)
        (engine-calls 0)
        (intent
         (dorg-api-make-mutation-intent
          '(on (id 1) (set (property "K") "V"))
          :source '(:file "/tmp/nwlkl.org"))))
    (cl-letf (((symbol-function 'dorg-mutate--resolve-source)
               (lambda (&rest _)
                 (cl-incf source-calls)
                 (ert-fail "override refusal resolved storage")))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 (cl-incf engine-calls)
                 (ert-fail "override refusal entered the engine"))))
      (dolist (option+value
               '((:source (:file "/other.org"))
                 (:base "other")
                 (:expected-targets (1))
                 (:allow-broad t)
                 (:save t)
                 (:message "other")
                 (:op-id "other")
                 (:extension-inputs ((probe . payload)))))
        (dorg-test-api--should-reason 'malformed
          (apply #'dorg-api-execute intent :mode 'dry-run option+value))))
    (should (= 0 source-calls))
    (should (= 0 engine-calls))))

(ert-deftest dorg-test-nwlkl-form-and-value-converge-on-one-core ()
  "Raw forms and immutable values use one core and preserve the response wire."
  (let* ((form '(on (id 100) (set (property "STATUS") "DONE")))
         (source '(:file "/tmp/nwlkl.org"))
         (intent (dorg-api-make-mutation-intent form :source source
                                                 :op-id "nwlkl-op"))
         (core-calls nil)
         (real-core (symbol-function 'dorg-api--execute-intent)))
    (cl-letf (((symbol-function 'dorg-api--execute-intent)
               (lambda (value mode)
                 (push (list value mode) core-calls)
                 (funcall real-core value mode)))
              ((symbol-function 'dorg-mutate--source-public-base)
               (lambda (_) '(:kind file :fingerprint "base")))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (_source _patches _extension-inputs &rest _)
                 '(:ok t :dry-run t :source-kind plain-org :root-id nil
                   :base-revision "base" :supported t :unsupported nil))))
      (let ((raw (dorg-api-execute form :mode 'dry-run :source source
                                   :op-id "nwlkl-op"))
            (first-class (dorg-api-execute intent :mode 'dry-run)))
        (should (equal raw first-class))
        (should (equal (list form) (plist-get first-class :intent)))
        (should-not (dorg-api-mutation-intent-p
                     (car (plist-get first-class :intent))))))
    ;; Both routes converge on the ONE 2-arg core (no caller-payload channel).
    (should (= 2 (length core-calls)))
    (dolist (call core-calls)
      (should (dorg-api-mutation-intent-p (car call)))
      (should (eq 'dry-run (nth 1 call))))))

(ert-deftest dorg-test-nwlkl-raw-extension-input-remains-outside-intent ()
  "The retired `:extension-inputs' channel is refused on BOTH surfaces
(tracked-regression): index projectors run automatically from the DOrg-owned
candidate-state context and accept no caller payload.  The raw route fails
closed with typed `unsupported' BEFORE any engine/plan work, and the intent
constructor still refuses the option as foreign authority."
  (let* ((form '(on (id 100) (set (property "STATUS") "DONE")))
         (source '(:dolt-dir "/tmp/nwlkl-dolt" :root-id 1))
         (payload '((probe . (:record 1))))
         (reached-engine nil))
    ;; Raw route: fail closed at the API boundary — the engine is never reached.
    (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _) (setq reached-engine t) nil)))
      (dorg-test-api--should-reason 'unsupported
        (dorg-api-execute form :mode 'dry-run :source source
                          :extension-inputs payload)))
    (should-not reached-engine)
    ;; And the intent constructor still refuses it as foreign authority.
    (dorg-test-api--should-reason 'malformed
      (dorg-api-make-mutation-intent form :source source
                                     :extension-inputs payload))))

(ert-deftest dorg-test-nwlkl-extension-inputs-nil-fails-closed-by-presence ()
  "The retired =:extension-inputs= key fails closed by PRESENCE (tracked-regression):
an explicitly supplied =:extension-inputs nil= is rejected before any engine work,
not silently accepted as \"no inputs\"."
  (let ((form '(on (id 100) (set (property "STATUS") "DONE")))
        (source '(:dolt-dir "/tmp/nwlkl-dolt" :root-id 1))
        (reached nil))
    (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _) (setq reached t) nil)))
      (dorg-test-api--should-reason 'unsupported
        (dorg-api-execute form :mode 'dry-run :source source
                          :extension-inputs nil)))
    (should-not reached)))

(ert-deftest dorg-test-nwlkl-intent-reexpands-targets-at-each-mode ()
  "Apply re-expands the stored form and refuses a changed approved target set."
  (let ((hits '((:heading-id 1)))
        (apply-calls 0))
    (cl-letf (((symbol-function 'dorg-api--ql-select)
               (lambda (&rest _) hits))
              ((symbol-function 'dorg-mutate--source-public-base)
               (lambda (_) '(:kind file :fingerprint "same")))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 '(:ok t :dry-run t :source-kind plain-org :root-id nil
                   :base-revision "same" :supported t :unsupported nil)))
              ((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (cl-incf apply-calls)
                 (ert-fail "changed target set entered apply"))))
      (let* ((intent
              (dorg-api-make-mutation-intent
               '(on (where (property "STATUS" "OPEN"))
                    (set (property "STATUS") "DONE"))
               :source '(:file "/tmp/nwlkl.org")))
             (preview (dorg-api-execute intent :mode 'dry-run))
             (approved
              (dorg-api-mutation-intent-with
               intent :base (plist-get preview :base)
               :expected-targets (plist-get preview :targets))))
        (should (equal '(1) (plist-get preview :targets)))
        (setq hits '((:heading-id 2)))
        (dorg-test-api--should-reason 'target-set-changed
          (dorg-api-execute approved :mode 'apply))))
    (should (= 0 apply-calls))))

(ert-deftest dorg-test-nwlkl-intent-promotion-keeps-base-cas-load-bearing ()
  "A functionally promoted intent carries the preview base into apply CAS."
  (let ((current-base '(:kind file :fingerprint "before"))
        (apply-calls 0))
    (cl-letf (((symbol-function 'dorg-mutate--source-public-base)
               (lambda (_) current-base))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 '(:ok t :dry-run t :source-kind plain-org :root-id nil
                   :base-revision "before" :supported t :unsupported nil)))
              ((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (cl-incf apply-calls)
                 (ert-fail "stale promoted intent entered apply"))))
      (let* ((intent
              (dorg-api-make-mutation-intent
               '(on (id 1) (set (property "K") "V"))
               :source '(:file "/tmp/nwlkl.org")))
             (preview (dorg-api-execute intent :mode 'dry-run))
             (approved (dorg-api-mutation-intent-with
                        intent :base (plist-get preview :base))))
        (setq current-base '(:kind file :fingerprint "after"))
        (should (equal '(stale . base-revision)
                       (dorg-test-api--mutation-reason+detail
                        (lambda ()
                          (dorg-api-execute approved :mode 'apply)))))))
    (should (= 0 apply-calls))))

(ert-deftest dorg-test-nwlkl-plain-org-raw-and-value-parity ()
  "Plain Org produces byte-equal responses through raw and value routes."
  (let* ((file (make-temp-file "dorg-nwlkl-plain-" nil ".org"
                               "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:STATUS: OPEN\n:END:\n"))
         (form '(on (id 100) (set (property "STATUS") "DONE")))
         (source (list :file file))
         (intent (dorg-api-make-mutation-intent
                  form :source source :op-id "nwlkl-plain")))
    (unwind-protect
        (let ((raw (dorg-api-execute form :mode 'dry-run
                                     :source source :op-id "nwlkl-plain"))
              (value (dorg-api-execute intent :mode 'dry-run)))
          (should (equal raw value))
          (should (equal (list form) (plist-get value :intent)))
          (should (equal "nwlkl-plain" (plist-get value :op-id))))
      (delete-file file))))

(ert-deftest dorg-test-nwlkl-dolt-raw-and-value-parity ()
  "Dolt produces byte-equal responses through raw and value routes."
  (dorg-test-ql--with-dolt-synced
      "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:STATUS: OPEN\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root))
           (form '(on (id 100) (set (property "STATUS") "DONE")))
           (intent (dorg-api-make-mutation-intent
                    form :source source :op-id "nwlkl-dolt"))
           (raw (dorg-api-execute form :mode 'dry-run
                                  :source source :op-id "nwlkl-dolt"))
           (value (dorg-api-execute intent :mode 'dry-run)))
      (should (equal raw value))
      (should (equal (list form) (plist-get value :intent)))
      (should (equal "nwlkl-dolt" (plist-get value :op-id))))))

;;; 39omw — initial public planning failure, not the downstream re-plan seam

(defvar dorg-test-39omw--events nil)
(defvar dorg-test-39omw--missing-condition nil)
(defvar dorg-test-39omw--recovery-rows '(((c . "39omw-original-commit"))))
(defconst dorg-test-39omw--descriptor
  '(:dolt-dir "/nonexistent/public-replay/" :root-id 100))
(defconst dorg-test-39omw--delete '(on (id 101) (delete (heading))))

(defun dorg-test-39omw--missing-plan
    (source patches _options unsupported &optional _owner-plan)
  "Apply PATCHES to an in-memory post-delete state, raising the real error."
  (should-not unsupported)
  (let ((ps (dorg-mutate--ps-create
             :root-id 100 :headings (make-hash-table :test 'eql))))
    (condition-case err
        (dolist (patch patches)
          (dorg-mutate--ps-apply-patch ps patch source))
      (dorg-mutate-error
       (setq dorg-test-39omw--missing-condition err)
       (push 'missing-target dorg-test-39omw--events)
       (signal (car err) (cdr err))))))

(defun dorg-test-39omw--execute (form &optional opaque mode)
  "Execute FORM through the real raw or OPAQUE public boundary in MODE."
  (if opaque
      (dorg-api-execute
       (dorg-api-make-mutation-intent
        form :source dorg-test-39omw--descriptor :op-id "39omw-op")
       :mode (or mode 'apply))
    (dorg-api-execute form :mode (or mode 'apply)
                      :source dorg-test-39omw--descriptor :op-id "39omw-op")))

(defun dorg-test-39omw--receipt-rows (receipt lease query &rest args)
  "Supply SQL ROWS only; retain real receipt query/extraction/recovery laws."
  (should (eq lease '39omw-lease))
  (let ((sql (apply #'format query args))
        (op-id (dorg-sql-quote "39omw-op")))
    (cond
     ((equal sql (format (concat "SELECT gen_id, intent_digest FROM dorg_mutation_log"
                                 " WHERE op_id = %s") op-id))
      (should (eq (car dorg-test-39omw--events) 'schema))
      (push 'lookup dorg-test-39omw--events)
      (when receipt
        (list (list (cons 'gen_id (car receipt))
                    (cons 'intent_digest (cdr receipt))))))
     ((equal sql (format (concat "SELECT to_commit AS c FROM dolt_diff_dorg_mutation_log"
                                 " WHERE diff_type = 'added' AND to_gen_id = %s")
                         (dorg-sql-quote (car receipt))))
      ;; Exact equality rejects op-id/date ordering or unquoted generation IDs.
      (should (string-match-p "diff_type = 'added' AND to_gen_id =" sql))
      (push 'recover dorg-test-39omw--events)
      dorg-test-39omw--recovery-rows)
     (t (ert-fail (format "unexpected receipt SQL: %s" sql))))))

(defmacro dorg-test-39omw--with-receipt (evidence &rest body)
  "Keep real public/planner/settled/result behavior; double receipt transport.
EVIDENCE is (GEN-ID . DIGEST) or nil.  No fixture or server is started."
  (declare (indent 1) (debug t))
  `(progn
     (require 'dorg-dolt)
     (let ((dorg-test-39omw--events nil)
           (dorg-test-39omw--missing-condition nil)
           (receipt ,evidence)
           (planner (symbol-function 'dorg-mutate--make-plan))
           (settled (symbol-function 'dorg-mutate--dolt-op-id-settled)))
       (cl-letf
           (((symbol-function 'dorg-mutate--dolt-plan-data)
             #'dorg-test-39omw--missing-plan)
            ((symbol-function 'dorg-mutate--make-plan)
             (lambda (source patches options &optional owner-plan)
               (push 'plan-enter dorg-test-39omw--events)
               (prog1 (funcall planner source patches options owner-plan)
                 (push 'plan-exit dorg-test-39omw--events))))
            ((symbol-function 'dorg-mutate--dolt-op-id-settled)
             (lambda (source options op-id digest base)
               (should (dorg-mutate-source-p source))
               (should (eq 'dolt (dorg-mutate-source-kind source)))
               (should (= 100 (dorg-mutate-source-root-id source)))
               (should (equal op-id (plist-get options :op-id)))
               (should (equal digest (plist-get options :intent-digest)))
               (should (equal base (plist-get options :base-commit)))
               (push 'settled dorg-test-39omw--events)
               (funcall settled source options op-id digest base)))
            ((symbol-function 'dorg-mutate-call-with-store-gate)
             (lambda (dir fn &optional op-id _deadline)
               (should (equal dir "/nonexistent/public-replay/"))
               (should (equal op-id "39omw-op"))
               (push 'gate dorg-test-39omw--events)
               (funcall fn '39omw-lease '39omw-key)))
            ((symbol-function 'dorg-sql--require-mutation-log-schema)
             (lambda (lease)
               (should (eq lease '39omw-lease))
               (push 'schema dorg-test-39omw--events)))
            ((symbol-function 'dorg-mutate--execute-patches)
             (lambda (&rest _) (ert-fail "replay reached execute-patches")))
            ((symbol-function 'dorg-mutate--dolt-gated-apply)
             (lambda (&rest _) (ert-fail "replay reached downstream gate")))
            ((symbol-function 'dorg-sql--lease-exec)
             (lambda (lease query &rest args)
               (apply #'dorg-test-39omw--receipt-rows receipt lease query args)))
            ((symbol-function 'dorg-sql--lease-batch)
             (lambda (&rest _) (ert-fail "unexpected commit batch"))))
         ,@body))))

(defun dorg-test-39omw--delete-evidence ()
  "Return a receipt binding the original normalized delete, not a fake digest."
  (cons "39omw-generation"
        (dorg-mutate--intent-digest
         (dorg-api-normalize dorg-test-39omw--delete)
         (dorg-mutate--resolve-source dorg-test-39omw--descriptor))))

(ert-deftest dorg-test-39omw-public-consumed-target-replay-recovers-settled ()
  "Raw and opaque public replay recover at the INITIAL plan failure only."
  (dolist (opaque '(nil t))
    (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
      (let ((result (dorg-test-39omw--execute dorg-test-39omw--delete opaque)))
        (should (plist-get result :ok))
        (should (plist-get result :idempotent))
        (should (equal "39omw-op" (plist-get result :op-id)))
        (should (equal "39omw-original-commit" (plist-get result :new-token)))
        (should (equal (list dorg-test-39omw--delete) (plist-get result :intent)))
        (should (equal '(plan-enter missing-target settled gate schema lookup
                                   recover)
                       (reverse dorg-test-39omw--events)))))))

(ert-deftest dorg-test-39omw-public-consumed-target-mismatched-intent-refuses ()
  "Digest mismatch wins over missing target, on both raw and opaque routes."
  (dolist (opaque '(nil t))
    (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
      (let* ((different '(on (id 101) (delete (subtree))))
             (digest (dorg-mutate--intent-digest
                      (dorg-api-normalize different)
                      (dorg-mutate--resolve-source dorg-test-39omw--descriptor)))
             (err (should-error (dorg-test-39omw--execute different opaque)
                                :type 'dorg-mutate-error)))
        (should-not (equal digest (cdr receipt)))
        (should (eq 'idempotency-key-mismatch (dorg-mutate-error-reason err)))
        (should (equal "39omw-op" (plist-get (dorg-mutate-error-data err) :op-id)))
        (should (equal '(plan-enter missing-target settled gate schema lookup)
                       (reverse dorg-test-39omw--events)))))))

(ert-deftest dorg-test-39omw-public-consumed-target-without-evidence-retains-error ()
  "A lookup miss re-signals the identical real initial-plan condition/data."
  (dolist (opaque '(nil t))
    (dorg-test-39omw--with-receipt nil
      (let ((err (should-error
                  (dorg-test-39omw--execute dorg-test-39omw--delete opaque)
                  :type 'dorg-mutate-error)))
        (should (equal err dorg-test-39omw--missing-condition))
        (should (equal '(plan-enter missing-target settled gate schema lookup)
                       (reverse dorg-test-39omw--events)))))))

(ert-deftest dorg-test-39omw-no-op-id-dry-run-and-missing-digest-retain-error ()
  "Recovery is APPLY-only and needs both op-id and nonempty string digest."
  (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
    (dolist (options '(nil (:op-id "39omw-op")
                          (:op-id "39omw-op" :intent-digest "")
                          (:op-id "39omw-op" :intent-digest 1)
                          (:op-id "39omw-op" :intent-digest "digest" :dry-run t)))
      (setq dorg-test-39omw--events nil)
      (let ((err (should-error
                  (apply #'dorg-mutate--apply-patches
                         dorg-test-39omw--descriptor
                         (dorg-api-normalize dorg-test-39omw--delete) nil options)
                  :type 'dorg-mutate-error)))
        (should (equal err dorg-test-39omw--missing-condition))
        (should (equal '(missing-target plan-enter) dorg-test-39omw--events))))
    (setq dorg-test-39omw--events nil)
    (should-error (dorg-test-39omw--execute dorg-test-39omw--delete nil 'dry-run)
                  :type 'dorg-mutate-error)
    (should (equal '(missing-target plan-enter) dorg-test-39omw--events))))

(ert-deftest dorg-test-39omw-invalid-input-never-consults-receipt ()
  "Raw/source/identity/typed validation failures do not inherit recovery."
  (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
    (should-error (dorg-test-39omw--execute '(on (id nope) (delete (heading))))
                  :type 'dorg-api-error)
    (should-error (dorg-api-execute dorg-test-39omw--delete :mode 'apply
                                   :source '(:dolt-dir 7 :root-id 100)
                                   :op-id "39omw-op")
                  :type 'dorg-mutate-error)
    (should-error (dorg-test-39omw--execute
                   '(on (id 101) (set (property "DORG_ID") "other")))
                  :type 'dorg-api-error)
    (should-error (dorg-api-execute dorg-test-39omw--delete :mode 'apply
                                   :source '(:dolt-dir "/nonexistent/public-replay/"
                                             :root-id 100 :identity none)
                                   :op-id "39omw-op")
                  :type 'dorg-mutate-error)
    (should-error (dorg-mutate--apply-patches
                   dorg-test-39omw--descriptor
                   '((:op delete :target (:heading :id "not-integer"))) nil
                   :op-id "39omw-op" :intent-digest "digest")
                  :type 'dorg-mutate-error)
    (should-not dorg-test-39omw--events)))

(ert-deftest dorg-test-39omw-history-direct-apply-without-op-id-retains-error ()
  "Exercise the actual history direct caller with unchanged no-op-id law."
  (require 'dorg-history)
  (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
    (cl-letf (((symbol-function 'dorg-commit-ancestor-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-model-load-tree)
               (lambda (&rest _) (make-hash-table :test 'eql)))
              ((symbol-function 'dorg-head-commit) (lambda (&rest _) "head"))
              ((symbol-function 'dorg-restore--patches)
               (lambda (&rest _) (dorg-api-normalize dorg-test-39omw--delete))))
      (let ((err (should-error
                  (dorg-restore-to-commit "/nonexistent/public-replay/" 100 nil
                                          "ancestor" "history control")
                  :type 'dorg-mutate-error)))
        (should (equal err dorg-test-39omw--missing-condition))
        (should (equal '(missing-target plan-enter) dorg-test-39omw--events))))))

(ert-deftest dorg-test-39omw-plain-org-and-ineligible-errors-are-not-reconciled ()
  "No Dolt recovery for plain Org or for an unsupported planning condition."
  (dorg-test-api--with-unit-files
   (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
				  ;; The Dolt missing-state double is not a plain-Org classifier.  Inject a
				  ;; typed planning failure here without changing plain-Org patch support.
				  (cl-letf (((symbol-function 'dorg-mutate--make-plan)
					     (lambda (&rest _)
					       (dorg-mutate--raise 'invariant "plain planning refusal"))))
					   (let ((err (should-error
						       (dorg-mutate--apply-patches
							(list :file (dorg-test-api--unit-file))
							(dorg-api-normalize dorg-test-39omw--delete) nil
							:op-id "39omw-op" :intent-digest "digest")
						       :type 'dorg-mutate-error)))
					     (should (equal "plain planning refusal" (cadr err)))
					     (should-not dorg-test-39omw--events)))
				  (setq dorg-test-39omw--events nil)
				  (cl-letf (((symbol-function 'dorg-mutate--dolt-plan-data)
					     (lambda (&rest _)
					       (dorg-mutate--raise 'unsupported "ineligible fixture condition"))))
					   (let ((err (should-error (dorg-test-39omw--execute dorg-test-39omw--delete)
								    :type 'dorg-mutate-error)))
					     (should (eq 'unsupported (dorg-mutate-error-reason err)))
					     (should (equal '(plan-enter) dorg-test-39omw--events)))))))

(ert-deftest dorg-test-39omw-execution-failure-is-not-a-planning-retry ()
  "An eligible-looking EXECUTION error must propagate without receipt lookup."
  (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
    (cl-letf (((symbol-function 'dorg-mutate--dolt-plan-data) (lambda (&rest _) nil))
              ((symbol-function 'dorg-mutate--execute-patches)
               (lambda (&rest _)
                 (push 'execute dorg-test-39omw--events)
                 (dorg-mutate--raise 'invariant "execution refusal"))))
      (let ((err (should-error (dorg-test-39omw--execute dorg-test-39omw--delete)
                               :type 'dorg-mutate-error)))
        (should (eq 'invariant (dorg-mutate-error-reason err)))
        (should (equal '(execute plan-exit plan-enter) dorg-test-39omw--events))))))

(ert-deftest dorg-test-39omw-successful-op-id-plans-before-receipt-lookup ()
  "Retain the legitimate post-plan gate-first preflight on a successful plan."
  (require 'dorg-dolt)
  (let ((executor (symbol-function 'dorg-mutate--execute-patches))
        (gated (symbol-function 'dorg-mutate--dolt-gated-apply)))
    (dorg-test-39omw--with-receipt nil
      (cl-letf (((symbol-function 'dorg-mutate--dolt-plan-data)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-mutate--execute-patches) executor)
                ((symbol-function 'dorg-mutate--dolt-gated-apply) gated)
                ((symbol-function 'dorg-mutate--dolt-build-attempt)
                 (lambda (&rest _)
                   (list :plan-base "head" :sql nil :empty '(:ok t)))))
        (should (plist-get (dorg-test-39omw--execute dorg-test-39omw--delete) :ok))
        (should (equal '(plan-enter plan-exit gate schema lookup)
                       (reverse dorg-test-39omw--events)))))))

(ert-deftest dorg-test-39omw-bounded-cross-root-initial-error-laws ()
  "Existing cross-root typed planning has the same hit/mismatch/miss laws."
  (let* ((source (dorg-mutate--resolve-source dorg-test-39omw--descriptor))
         (destination (dorg-mutate--resolve-source
                       '(:dolt-dir "/nonexistent/public-replay/" :root-id 500)))
         (patch (list :op 'move :target '(:subtree :id 101)
                      :to '(:under (:heading :id 501))
                      :to-source destination :preserve-ids t))
         (digest (dorg-mutate--intent-digest (list patch) source)))
    (dolist (law '(hit mismatch miss))
      (dorg-test-39omw--with-receipt
          (and (not (eq law 'miss))
               (cons "39omw-generation" (if (eq law 'hit) digest "other-digest")))
        (cl-letf (((symbol-function 'dorg-mutate--dolt-plan-data)
                   (lambda (src patches options unsupported &optional _owner-plan)
                     (should-not unsupported)
                     (let ((cross (dorg-mutate--dolt-cross-root-batch-patch src patches)))
                       (should cross)
                       (dorg-mutate--dolt-cross-root-plan-data src cross options))))
                  ((symbol-function 'dorg-mutate--dolt-cross-root-plan-data)
                   (lambda (src cross options)
                     (push 'cross-root-plan dorg-test-39omw--events)
                     (dorg-test-39omw--missing-plan src (list cross) options nil))))
          (if (eq law 'hit)
              (let ((result (dorg-mutate--apply-patches
                             source (list patch) nil
                             :op-id "39omw-op" :intent-digest digest)))
                (should (plist-get result :idempotent))
                (should (equal "39omw-original-commit" (plist-get result :commit))))
            (let ((err (should-error
                        (dorg-mutate--apply-patches
                         source (list patch) nil :op-id "39omw-op" :intent-digest digest)
                        :type 'dorg-mutate-error)))
              (if (eq law 'mismatch)
                  (should (eq 'idempotency-key-mismatch (dorg-mutate-error-reason err)))
                (should (equal err dorg-test-39omw--missing-condition)))))
          (should (= 1 (cl-count 'cross-root-plan dorg-test-39omw--events)))
          (should (= 1 (cl-count 'settled dorg-test-39omw--events)))
          (should (= 1 (cl-count 'lookup dorg-test-39omw--events))))))))

(ert-deftest dorg-test-39omw-key-query-normalization-refusals-stay-before-lookup ()
  "Failed key/query resolution and expected-target gates are not replay recovery."
  (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
    (cl-letf (((symbol-function 'dorg-api--ql-select)
               (lambda (&rest _)
                 (dorg-api--raise 'invalid-target "selector missing" :detail 'missing))))
      (dolist (target '((key "consumed") (where (property "K" "V"))))
        (let ((err (should-error
                    (dorg-test-39omw--execute `(on ,target (delete (heading))))
                    :type 'dorg-api-error)))
          (should (eq 'invalid-target (dorg-api-error-reason err))))))
    (cl-letf (((symbol-function 'dorg-api--ql-select) (lambda (&rest _) nil)))
      (let ((err (should-error
                  (dorg-test-39omw--execute
                   '(on (where (property "K" "V")) (delete (heading))))
                  :type 'dorg-api-error)))
        (should (eq 'expected-targets-required (dorg-api-error-reason err)))))
    (should-not dorg-test-39omw--events)))

(ert-deftest dorg-test-39omw-shared-eligibility-and-generation-recovery-law ()
  "Both sites share eligibility; real generation recovery is quoted and unambiguous."
  (should (equal '(invalid-target invariant)
                 dorg-mutate--settled-planning-error-reasons))
  (require 'dorg-dolt)
  (let ((gated (symbol-function 'dorg-mutate--dolt-gated-apply)))
    (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
      (let ((dorg-mutate--settled-planning-error-reasons nil))
        (should-error (dorg-test-39omw--execute dorg-test-39omw--delete)
                      :type 'dorg-mutate-error)
        (should-not (memq 'settled dorg-test-39omw--events))
        (setq dorg-test-39omw--events nil)
        (should-error
         (funcall gated
                  (dorg-mutate--resolve-source dorg-test-39omw--descriptor)
                  (list :op-id "39omw-op" :intent-digest (cdr receipt)) nil
                  (lambda () (dorg-mutate--raise 'invariant "re-plan failure")))
         :type 'dorg-mutate-error)
        (should-not dorg-test-39omw--events))))
  ;; Drive the PUBLIC hit through the real recovery helper.  Both ambiguous
  ;; directions must fail closed; a single immutable-generation transition wins.
  (dolist (rows '(nil (((c . "39omw-original-commit")))
                     (((c . "old")) ((c . "new")))))
    (let ((dorg-test-39omw--recovery-rows rows))
      (dorg-test-39omw--with-receipt (dorg-test-39omw--delete-evidence)
        (setcar receipt "39omw-gen'quoted")
        (if (= 1 (length rows))
            (should (equal "39omw-original-commit"
                           (plist-get (dorg-test-39omw--execute dorg-test-39omw--delete)
                                      :new-token)))
          (should-error (dorg-test-39omw--execute dorg-test-39omw--delete)
                        :type 'dorg-sql-lease-broken))
        (should (= 1 (cl-count 'recover dorg-test-39omw--events)))))))

;;; Compact operation receipts; derived postimage response retirement.

(ert-deftest dorg-test-fhhwd-live-intent-options-and-unrelated-raw-options-unchanged ()
  "Retirement must not change compact operation options or unrelated raw behavior."
  (let* ((options '(:op-id "compact-op" :save nil :message "compact"))
         (intent (apply #'dorg-api-make-mutation-intent '(batch) options)))
    (should (equal options (dorg-api-mutation-intent-options intent)))
    ;; The historical raw projection is domain-ordered, not caller-ordered.
    (should (equal '(:save nil :message "compact" :op-id "compact-op")
                   (dorg-api--raw-intent-options
                    (append '(:unrelated-unknown t :mode apply) options
                            '(:op-id "ignored-second-value")))))))

(ert-deftest dorg-test-fhhwd-intent-constructor-refuses-retired-model-option ()
  "Presence, including nil, refuses through the existing closed option domain."
  (dolist (value '(nil t))
    (let ((err (should-error
                (dorg-api-make-mutation-intent
                 '(batch) :return-candidate-models value)
                :type 'dorg-api-error)))
      (should (eq 'malformed (dorg-api-error-reason err)))
      (should (eq :return-candidate-models
                  (plist-get (dorg-api-error-data err) :option))))))

(ert-deftest dorg-test-fhhwd-intent-replacement-refuses-retired-model-option ()
  "Functional replacement cannot reintroduce the retired option or mutate intent."
  (let* ((intent (dorg-api-make-mutation-intent '(batch) :op-id "compact-op"))
         (before (dorg-api-mutation-intent-options intent)))
    (dolist (value '(nil t))
      (let ((err (should-error
                  (dorg-api-mutation-intent-with
                   intent :return-candidate-models value)
                  :type 'dorg-api-error)))
        (should (eq 'malformed (dorg-api-error-reason err)))
        (should (eq :return-candidate-models
                    (plist-get (dorg-api-error-data err) :option)))))
    (should (equal before (dorg-api-mutation-intent-options intent)))))

(ert-deftest dorg-test-fhhwd-raw-execute-refuses-retired-option-before-planning ()
  "Raw execute must not silently filter the formerly supported option away."
  (cl-letf (((symbol-function 'dorg-api-normalize)
             (lambda (&rest _) (ert-fail "retired option reached normalization")))
            ((symbol-function 'dorg-mutate--resolve-source)
             (lambda (&rest _) (ert-fail "retired option resolved a source")))
            ((symbol-function 'dorg-mutate--apply-patches)
             (lambda (&rest _) (ert-fail "retired option entered the engine"))))
    (dolist (mode '(dry-run apply))
      (dolist (value '(nil t))
        (let ((err (should-error
                    (dorg-api-execute '(batch) :mode mode
                                      :return-candidate-models value)
                    :type 'dorg-api-error)))
          (should (eq 'malformed (dorg-api-error-reason err)))
          (should (eq :return-candidate-models
                      (plist-get (dorg-api-error-data err) :option))))))))

(ert-deftest dorg-test-fhhwd-compact-insert-has-no-payload-argument ()
  "A new receipt is exactly four fields; legacy nullable storage is omitted."
  (require 'dorg-dolt)
  (should (equal '(4 . 4) (func-arity #'dorg-sql--mutation-log-insert-sql)))
  (should
   (equal (format (concat "INSERT INTO dorg_mutation_log"
                          " (op_id, gen_id, intent_digest, ts)"
                          " VALUES (%s, %s, %s, %d)")
                  (dorg-sql-quote "op'quoted")
                  (dorg-sql-quote "gen'quoted")
                  (dorg-sql-quote "intent") 123)
          (dorg-sql--mutation-log-insert-sql
           "op'quoted" "gen'quoted" "intent" 123))))

(ert-deftest dorg-test-fhhwd-settled-result-recovers-without-payload-read ()
  "Real generation recovery returns the compact result without reading old payload."
  (require 'dorg-dolt)
  (let ((reads nil))
    (cl-letf (((symbol-function 'dorg-sql--lease-exec)
               (lambda (lease query &rest args)
                 (should (eq 'compact-lease lease))
                 (let ((sql (apply #'format query args)))
                   (push sql reads)
                   (should
                    (equal (format
                            (concat "SELECT to_commit AS c"
                                    " FROM dolt_diff_dorg_mutation_log"
                                    " WHERE diff_type = 'added' AND to_gen_id = %s")
                            (dorg-sql-quote "gen'quoted"))
                           sql))
                   '(((c . "original-commit")))))))
      (let ((result (dorg-mutate--dolt-idempotent-result
                     'compact-lease "gen'quoted" "compact-op" "base" 100)))
        (should (plist-get result :ok))
        (should (plist-get result :idempotent))
        (should (equal "original-commit" (plist-get result :commit)))
        (should (equal "compact-op" (plist-get result :op-id)))
        (should-not (plist-member result :postimages))
        (should-not (plist-member result :candidate-models))
        (should (= 1 (length reads)))))))

(provide 'dorg-api-tests)
;;; dorg-api-tests.el ends here
