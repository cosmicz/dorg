;;; dorg-qgr4z-golden-tests.el --- qgr4z 0.1 byte goldens -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Byte fixtures captured from the 0.1 public mutation spelling before qgr4z
;; replaces the sequence-addressing grammar.  Subsequent controls must execute
;; the 0.2 spelling and compare against these bytes, except where the accepted
;; 0.2 in-place law deliberately changes the result.

;;; Code:

(require 'ert)
(require 'dorg-api)

(defconst dorg-test-qgr4z--fixture-directory
  (expand-file-name "fixtures/qgr4z/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding qgr4z baseline byte fixtures.")

(defconst dorg-test-qgr4z--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root used by qgr4z documentation controls.")

(defun dorg-test-qgr4z--fixture (name)
  "Return the contents of qgr4z fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name dorg-test-qgr4z--fixture-directory))
    (buffer-string)))

(defmacro dorg-test-qgr4z--with-input (name buffer &rest body)
  "Visit fixture NAME as BUFFER while evaluating BODY."
  (declare (indent 2) (debug (form symbolp body)))
  `(let* ((file (make-temp-file "dorg-qgr4z" nil ".org"
                                (dorg-test-qgr4z--fixture ,name)))
          (,buffer (find-file-noselect file)))
     (unwind-protect
         (with-current-buffer ,buffer
           (org-mode)
           ,@body)
       (with-current-buffer ,buffer (set-buffer-modified-p nil))
       (kill-buffer ,buffer)
       (delete-file file))))

(defun dorg-test-qgr4z--retired-error (form)
  "Return the `dorg-api-error' condition signalled by retired FORM."
  (condition-case err
      (progn (dorg-api-read form) (ert-fail "expected a retired-spelling error"))
    (dorg-api-error err)))

(ert-deftest dorg-test-qgr4z-retired-spellings-refuse-with-exact-diagnostic ()
  "Every retired spelling reports its mechanical 0.2 replacement exactly."
  (dolist
      (case
       '(((on (id 1) (replace (keywords "K") "value"))
          (replace (keywords "K") "value")
          (replace (keyword "K" (all)) "value"))
         ((on document (delete (keywords "K")))
          (delete (keywords "K"))
          (delete (keyword "K" (all))))
         ((on (id 1)
              (insert (drawer-line "LOGBOOK" (occurrence 1))
                      (before (occurrence 65)) "- state"))
         (insert (drawer-line "LOGBOOK" (occurrence 1))
                  (before (occurrence 65)) "- state")
          (insert (drawer-line "LOGBOOK") (before (occurrence 65)) "- state"))
         ((on (id 1) (replace (items) (text "new")))
          (replace (items) (text "new"))
          (replace (item (all)) (text "new")))
         ;; The hard-cut matcher owns verb plus address shape, not payload
         ;; validity.  These replacements are mechanical diagnostics only.
         ((on (id 1) (replace (keywords "K")))
          (replace (keywords "K"))
          (replace (keyword "K" (all))))
         ((on (id 1) (replace (keywords "DORG_ID") "x"))
          (replace (keywords "DORG_ID") "x")
          (replace (keyword "DORG_ID" (all)) "x"))
         ((on (id 1) (replace (items)))
          (replace (items))
          (replace (item (all))))))
    (pcase-let* ((`(,form ,action ,replacement) case)
                 (err (let ((print-length 1) (print-level 1)
                            (print-integers-as-characters t) (print-quoted nil))
                        (dorg-test-qgr4z--retired-error form)))
                 (data (dorg-api-error-data err)))
      (should (eq 'invalid-target (dorg-api-error-reason err)))
      (should (eq 'retired-spelling (plist-get data :detail)))
      (should (equal action (plist-get data :action)))
      (should (equal replacement (plist-get data :replacement)))
      (let ((print-length nil) (print-level nil)
            (print-integers-as-characters nil) (print-quoted t)
            (float-output-format nil) (print-escape-newlines t)
            (print-escape-control-characters t) (print-escape-nonascii nil)
            (print-circle nil))
        (should (equal (cadr err)
                       (format "dorg: retired mutation spelling; use %S"
                               replacement)))))))

(ert-deftest dorg-test-qgr4z-retired-spellings-preflight-before-target-resolution ()
  "Retired spellings beat QL/key resolution, including nested batch order."
  (let (hosts ql)
    (cl-letf (((symbol-function 'dorg-api--target-hosts)
               (lambda (&rest _) (setq hosts t) (ert-fail "must not resolve hosts")))
              ((symbol-function 'dorg-api--ql-select)
               (lambda (&rest _) (setq ql t) (ert-fail "must not run QL"))))
      (dolist
          (case
           '(((on (where (title "unreachable")) (replace (keywords "K") "v"))
              (replace (keywords "K") "v"))
             ((on (key "unresolvable") (delete (keywords "K")))
              (delete (keywords "K")))
             ((batch (on (id 1) (delete (keywords "FIRST")))
                     (on (id 2) (replace (items) (text "later"))))
              (delete (keywords "FIRST")))))
        (pcase-let* ((`(,form ,action) case)
                     (err (should-error
                           (dorg-api-normalize form '(:file "/definitely-not-used.org"))
                           :type 'dorg-api-error))
                     (data (dorg-api-error-data err)))
          (should (eq 'invalid-target (dorg-api-error-reason err)))
          (should (eq 'retired-spelling (plist-get data :detail)))
          (should (equal action (plist-get data :action)))
          (should (plist-get data :replacement))))
      (should-not hosts)
      (should-not ql))))

(ert-deftest dorg-test-qgr4z-retired-spellings-never-enter-engine ()
  "Every retired form refuses before both engine modes and preserves source.
The engine spies signal, rather than merely setting flags, so this fails if a
preflight regression reaches plan or apply before its typed refusal."
  (dolist
      (action '((replace (keywords "K") "v")
                (delete (keywords "K"))
                (insert (drawer-line "LOGBOOK" (occurrence 1))
                        (before-first) "- state")
                (replace (items) (text "replacement"))))
    (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
      (let ((before (with-current-buffer buffer (buffer-string))))
        (cl-letf (((symbol-function 'dorg-mutate--plan-patches)
                   (lambda (&rest _) (ert-fail "retired form reached plan")))
                  ((symbol-function 'dorg-mutate--apply-patches)
                   (lambda (&rest _) (ert-fail "retired form reached apply"))))
          (should-error
           (dorg-api-make-mutation-intent `(on (id 1001) ,action))
           :type 'dorg-api-error)
          (dolist (mode '(dry-run apply))
            (should-error
             (dorg-api-execute `(on (id 1001) ,action) :mode mode :source buffer)
             :type 'dorg-api-error)))
        (should (equal before (with-current-buffer buffer (buffer-string))))))))

(ert-deftest dorg-test-qgr4z-unchanged-address-spellings-stay-accepted ()
  "Only the four mapped spellings retire; neighbours lower to their v2 IR."
  (dolist
      (case
       '(((on (id 1) (replace (keyword "K" (occurrence 2)) "v"))
          (:op replace :target (:keyword :host (:heading :id 1) :name "K"
                                           :select (:occurrence 2)) :value "v"))
         ((on (id 1) (delete (keyword "K" (occurrence 1))))
          (:op delete :target (:keyword :host (:heading :id 1) :name "K"
                                          :select (:occurrence 1))))
         ((on (id 1) (insert (keyword "K") (after-last) "v"))
          (:op insert :target (:keyword :host (:heading :id 1) :name "K")
           :value "v" :position :after-last))
         ((on (id 1) (insert (text) (before-first) "body"))
          (:op insert :target (:text :host (:heading :id 1))
           :position :before-first :value "body"))
         ((on (id 1) (replace (text (occurrence 1)) "body"))
          (:op replace :target (:text :host (:heading :id 1)
                                       :select (:occurrence 1)) :value "body"))
         ;; These three rows share the old drawer address but not its retired
         ;; insert verb.  Comparing lowered IR catches an over-broad matcher.
         ((on (id 1) (replace (drawer-line "LOGBOOK" (occurrence 1)) "line"))
          (:op replace :target (:drawer-line :host (:heading :id 1) :name "LOGBOOK"
                                              :select (:occurrence 1)) :value "line"))
         ((on (id 1) (delete (drawer-line "LOGBOOK" (occurrence 1))))
          (:op delete :target (:drawer-line :host (:heading :id 1) :name "LOGBOOK"
                                             :select (:occurrence 1))))
         ((on (id 1) (insert (drawer-line "LOGBOOK") (before-first) "line"))
          (:op insert :target (:drawer-line :host (:heading :id 1) :name "LOGBOOK")
           :position :before-first :value "line"))))
    (pcase-let ((`(,form ,expected) case))
      (should (equal expected (car (dorg-api-normalize form)))))))

(ert-deftest dorg-test-qgr4z-current-sequence-forms-lower-to-v2-ir ()
  "Current public sequence forms expose only singular v2 targets."
  (dolist
      (case
       '(((on (id 1) (replace (keyword "K" (all)) "v"))
          (:op replace :target (:keyword :host (:heading :id 1) :name "K" :select (:all))
           :values ("v")))
         ((on (id 1) (insert (drawer-line "LOGBOOK") (before-first) "- state"))
          (:op insert :target (:drawer-line :host (:heading :id 1) :name "LOGBOOK")
           :position :before-first :value "- state"))
         ((on (id 1) (replace (item (all)) (text "replacement")))
         (:op replace :target (:item :host (:heading :id 1) :select (:all))
           :items ((:kind text :text "replacement" :post-blank 0))))
         ((on (id 1) (replace (item (occurrence 2)) (text "replacement")))
          (:op replace :target (:item :host (:heading :id 1)
                               :select (:occurrence 2))
           :item (:kind text :text "replacement" :post-blank 0)))
         ((on (id 1) (insert (item) (after (occurrence 2)) (text "new")))
          (:op insert :target (:item :host (:heading :id 1))
           :position (:after 2) :item (:kind text :text "new" :post-blank 0)))
         ((on (id 1) (replace (drawer-line "LOGBOOK" (all)) "one" "two"))
          (:op replace :target (:drawer-line :host (:heading :id 1) :name "LOGBOOK"
                                             :select (:all))
           :values ("one" "two")))
         ((on (id 1) (delete (text (occurrence 2))))
          (:op delete :target (:text :host (:heading :id 1)
                               :select (:occurrence 2))))))
    (pcase-let ((`(,form ,expected) case))
      (should (equal expected (car (dorg-api-normalize form)))))))

(ert-deftest dorg-test-qgr4z-no-sequence-cells-refuse-with-declared-detail ()
  "The table's three NO cells are typed selector refusals, not parser drift."
  (dolist (form '((on (id 1) (replace (text (all)) "x"))
                  (on (id 1) (delete (text (all))))
                  (on (id 1) (delete (item (all))))))
    (let ((err (should-error (dorg-api-normalize form) :type 'dorg-api-error)))
      (should (eq 'invalid-target (dorg-api-error-reason err)))
      (should (eq 'unsupported-selector
                  (plist-get (dorg-api-error-data err) :detail))))))

(ert-deftest dorg-test-qgr4z-table-is-single-source ()
  "Legal grammar rows are projections of the same cells API and IR query.
This is deliberately a reason check, not a source-text census: changing a
cell's id, operation, noun, or selector breaks the grammar projection, and
wrapping the selector lookup proves both runtime consumers call the table."
  (dolist (cell dorg-mutate-sequence-cells)
    (when (plist-get cell :legal)
      (let ((production (cl-find (plist-get cell :id)
                                 dorg-mutate-grammar-productions
                                 :key (lambda (row) (plist-get row :id)))))
        (should production)
        (should (eq (plist-get cell :op) (plist-get (plist-get production :ir) :op)))
        (should (eq (plist-get cell :noun)
                    (plist-get (plist-get production :ir) :target-kind)))
        (should (equal (plist-get cell :cardinality)
                       (plist-get production :cardinality)))))
  (let ((real (symbol-function 'dorg-mutate-sequence-cell))
        calls)
    (cl-letf (((symbol-function 'dorg-mutate-sequence-cell)
               (lambda (&rest args)
                 (push args calls)
                 (apply real args))))
      (let ((patch (car (dorg-api-normalize
                         '(on (id 1) (replace (keyword "K" (all)) "v"))))))
        (dorg-mutate--validate-patch patch 0)))
    (should (>= (length calls) 2)))))

(ert-deftest dorg-test-qgr4z-every-cell-normalizes-to-ir ()
  "Every legal table cell's public example reaches its declared v2 row.
The selector comparison preserves the distinction between the all and
occurrence families while accepting the concrete positive example number."
  (dolist (cell dorg-mutate-sequence-cells)
    (if (plist-get cell :legal)
        (let* ((production (cl-find (plist-get cell :id)
                                    dorg-mutate-grammar-productions
                                    :key (lambda (row) (plist-get row :id))))
               (patch (car (dorg-api-normalize (plist-get production :example))))
               (target (plist-get patch :target))
               (select (plist-get (cdr target) :select)))
          (should production)
          (should (eq (plist-get cell :op) (plist-get patch :op)))
          (should (eq (plist-get cell :noun) (car target)))
          (should (if (equal (plist-get cell :select) '(:all))
                      (equal '(:all) select)
                    (if (equal (plist-get cell :select) '(:occurrence N))
                        (and (eq :occurrence (car-safe select))
                             (integerp (cadr select)) (> (cadr select) 0))
                      (null select))))
          (when-let ((payload (plist-get cell :payload)))
            (should (plist-member patch payload))))
      ;; Each explicitly declared NO cell must be refused by the public
      ;; lowering boundary with the table's diagnostic, not parser fallout.
      (let* ((noun (plist-get cell :noun))
             (op (plist-get cell :op))
             (form (pcase (list op noun)
                     (`(replace :text) '(on (id 1) (replace (text (all)) "x")))
                     (`(delete :text) '(on (id 1) (delete (text (all)))))
                     (`(delete :item) '(on (id 1) (delete (item (all))))))))
        (let ((err (should-error (dorg-api-normalize form) :type 'dorg-api-error)))
          (should (eq 'unsupported-selector
                      (plist-get (dorg-api-error-data err) :detail))))))))

(ert-deftest dorg-test-qgr4z-closed-rows-fields-and-laws ()
  "Every v2 row rejects foreign fields and the retired missing policy.
Examples are taken from the grammar projection, so this ranges over every
legal cell rather than a hand-maintained subset.  The execution-law controls
below pin its materialize/no-op/error result on plain org."
  (dolist (cell dorg-mutate-sequence-cells)
    (when (plist-get cell :legal)
      (let* ((production (cl-find (plist-get cell :id)
                                  dorg-mutate-grammar-productions
                                  :key (lambda (row) (plist-get row :id))))
             (patch (car (dorg-api-normalize (plist-get production :example))))
             (payload (plist-get cell :payload))
             (with-missing (append patch '(:missing :error))))
        (should-error (dorg-mutate--validate-patch with-missing 0)
                      :type 'dorg-mutate-error)
        ;; Test every foreign sequence payload, not merely one made-up key.
        ;; In particular this kills accepting :value on keyword/all, whose
        ;; declared payload is :values.
        (dolist (field '(:value :values :item :items))
          (unless (eq field payload)
            (should-error
             (dorg-mutate--validate-patch
              (append patch
                      (list field
                            (pcase field
                              (:value "foreign")
                              (:values '("foreign"))
                              (:item '(:kind text :text "foreign"))
                              (:items '((:kind text :text "foreign"))))))
              0)
             :type 'dorg-mutate-error)))))
  ;; NO cells must be refused by the IR validator itself, rather than only by
  ;; public lowering.  This kills a validator that silently trusts its caller.
  (dolist (cell dorg-mutate-sequence-cells)
    (unless (plist-get cell :legal)
      (let* ((noun (plist-get cell :noun))
             (op (plist-get cell :op))
             (select (plist-get cell :select))
             (payload (plist-get cell :payload))
             (target (append (list noun :host '(:heading :id 1))
                             (when select (list :select select))))
             (patch (append (list :op op :target target)
                            (pcase payload
                              (:value '(:value "value"))
                              (:values '(:values ("value")))
                              (:item '(:item (:kind text :text "value")))
                              (:items '(:items ((:kind text :text "value"))))))))
        (should-error (dorg-mutate--validate-patch patch 0)
                      :type 'dorg-mutate-error))))))

(ert-deftest dorg-test-qgr4z-generated-reference-byte-exact ()
  "The qgr selector table remains a rendered, not hand-edited, reference."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "docs/generated/mutate-grammar.org"
                                            dorg-test-qgr4z--root))
    (should (equal (dorg-mutate-grammar-render-org) (buffer-string)))))

(ert-deftest dorg-test-qgr4z-doc-drift-fixed ()
  "The v2 prose retains every corrected sequence-algebra statement."
  (let ((mutate (with-temp-buffer
                  (insert-file-contents (expand-file-name "docs/mutate.org"
                                                          dorg-test-qgr4z--root))
                  (buffer-string)))
        (cli (with-temp-buffer
               (insert-file-contents (expand-file-name "docs/cli.org"
                                                       dorg-test-qgr4z--root))
               (buffer-string))))
    ;; These cover the text, drawer-line, IR, host-axis, ordering and CLI
    ;; corrections from the qgr census; retired plural addresses are absent.
    (dolist (text '("Any positive occurrence is valid"
                    "Insert accepts =before-first=, =after-last="
                    "Every drawer-line operation addresses the *first exact-spelling*"
                    "(:text :host HOST :select SEL)"
                    "(:item :host HOST :select SEL)"
                    "(:drawer-line :host HOST :name NAME :select SEL)"
                    "~:root~ is rejected"))
      (should (string-match-p (regexp-quote text) mutate)))
    (should (string-match-p
             (regexp-quote "(replace (keyword \"SEQ_TODO\" (all))") cli))
    (should-not (string-match-p "(keywords [A-Z]" mutate))
    (should-not (string-match-p "DOrg 0\\.1\\.0 mutation surface" mutate))
    (should-not (string-match-p ":front" mutate))))

(ert-deftest dorg-test-qgr4z-retired-near-misses-remain-generic ()
  "Only exact retired productions carry migration data."
  (dolist
      (form
       '((on (id 1) (insert (drawer-line "LOGBOOK" (occurrence 2))
                             (before-first) "- state"))
         (on (id 1) (insert (keywords "K") (before-first) "v"))
         (on (id 1) (delete (items)))
         (on (id 1) (replace (texts) "v"))
         (on (id 1) (replace body "v"))))
    (let ((err (condition-case caught
                   (progn (dorg-api-normalize form) nil)
                 (dorg-api-error caught))))
      (should err)
      (should-not (eq 'retired-spelling
                      (plist-get (dorg-api-error-data err) :detail)))
      (should-not (plist-member (dorg-api-error-data err) :replacement)))))

(ert-deftest dorg-test-qgr4z-retirement-has-no-warning-or-fallback ()
  "Every retired spelling signals without a warning, message, or lowerer retry."
  (let (warnings lowered)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warnings t)))
              ((symbol-function 'message)
               (lambda (&rest _) (ert-fail "retired spelling emitted a message")))
              ((symbol-function 'dorg-api--action->patch)
               (lambda (&rest _) (setq lowered t))))
      (dolist
          (action '((replace (keywords "K") "v")
                    (delete (keywords "K"))
                    (insert (drawer-line "LOGBOOK" (occurrence 1))
                            (before-first) "- state")
                    (replace (items) (text "replacement"))))
        (should-error (dorg-api-read `(on (id 1) ,action))
                      :type 'dorg-api-error))
      (should-not warnings)
      (should-not lowered))))

(ert-deftest dorg-test-qgr4z-retired-ir-shapes-refuse ()
  "The private sequence boundary rejects each retired IR feature in isolation."
  (dolist
      (patch
       '((:op replace :target (:keywords :host (:heading :id 1) :name "K")
          :values ("v"))
         (:op insert :target (:texts :host (:heading :id 1))
          :position :before-first :value "v")
         (:op replace :target (:items :host (:heading :id 1))
          :items ((:kind text :text "v")))
         (:op insert :target (:drawer-lines :host (:heading :id 1) :name "L")
          :position :before-first :value "- v")
         (:op replace :target (:keyword :host (:heading :id 1) :name "K"
                                             :select (:occurrence 1)
                                             :occurrence 1)
          :value "v")
         (:op replace :target (:keyword :host (:heading :id 1) :name "K"
                                             :select (:occurrence 1))
          :missing :error :value "v")))
    (let ((err (should-error (dorg-mutate--validate-patch patch 0)
                             :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should-not (plist-member (dorg-mutate-error-data err) :replacement))
      (should-not (eq 'retired-spelling
                      (plist-get (dorg-mutate-error-data err) :detail))))))

(ert-deftest dorg-test-qgr4z-ir-root-target-legality ()
  "Only the keyword cells admit the source-independent root host."
  (should (equal
           '(:op replace :target (:keyword :host :root :name "K" :select (:all))
             :values ("v"))
           (dorg-mutate--validate-patch
            '(:op replace :target (:keyword :host :root :name "K" :select (:all))
              :values ("v")) 0)))
  (dolist
      (patch
       '((:op replace :target (:text :host :root :select (:occurrence 1)) :value "v")
         (:op replace :target (:item :host :root :select (:all))
          :items ((:kind text :text "v")))
         (:op replace :target (:drawer-line :host :root :name "LOGBOOK" :select (:all))
          :values ("v"))))
    (let ((err (should-error (dorg-mutate--validate-patch patch 0)
                             :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err))))))

(ert-deftest dorg-test-qgr4z-root-keyword-source-capability-plain ()
  "The table, not a sequence parser arm, owns root capability decisions.
Root keywords lower through the API.  Plain-org planning and application both
reach the established source-capability refusal without changing bytes; the
named Dolt root controls cover the writable source success case."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (let ((before (with-current-buffer buffer (buffer-string))))
      (cl-letf (((symbol-function 'dorg-api--host-id)
                 (lambda (&rest _) (ert-fail "sequence arm checked root host"))))
        (should (equal
                 '((:op insert :target (:keyword :host :root :name "K")
                    :value "root" :position :after-last))
                 (dorg-api-normalize
                  '(on document (insert (keyword "K") (after-last) "root")))))
        (dolist (action '((insert (text) (after-last) "x")
                          (replace (item (all)) (text "x"))
                          (insert (drawer-line "LOGBOOK") (after-last) "x")))
          (let ((err (should-error (dorg-api-normalize `(on document ,action))
                                   :type 'dorg-api-error)))
            (should (eq 'invalid-target (dorg-api-error-reason err)))))
        (let ((preview (dorg-api-execute
                        '(on document (insert (keyword "K") (after-last) "root"))
                        :mode 'dry-run :source buffer)))
          (should-not (plist-get preview :supported))
          (should (eq 'no-source-preserving-editor
                      (plist-get (car (plist-get preview :unsupported)) :reason))))
        (let ((err (should-error
                    (dorg-api-execute
                     '(on document (insert (keyword "K") (after-last) "root"))
                     :mode 'apply :source buffer)
                    :type 'dorg-mutate-error)))
          (should (eq 'unsupported (plist-get (dorg-mutate-error-data err) :reason)))
          (should (eq 'no-source-preserving-editor
                      (plist-get (dorg-mutate-error-data err) :detail)))
          (should (equal before (with-current-buffer buffer (buffer-string)))))))))

(ert-deftest dorg-test-qgr4z-root-keyword-source-capability ()
  "A document keyword reaches the Dolt source capability through the table."
  (dorg-test-ql--with-dolt-synced
      "#+OPTIONS: x\n* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (before (dorg-head-commit dd)))
      (cl-letf (((symbol-function 'dorg-api--host-id)
                 (lambda (&rest _) (ert-fail "sequence arm checked root host"))))
        (should (plist-get
                 (dorg-api-execute
                  '(on document (insert (keyword "AUTHOR") (after-last) "OC"))
                  :mode 'apply :source source)
                 :ok))
        (should-not (equal before (dorg-head-commit dd)))))))

(ert-deftest dorg-test-qgr4z-keyword-all-replace-in-place ()
  "The 0.2 spelling is in-place, the one ruled change from the 0.1 golden."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (dorg-api-execute
     '(on (id 1001) (replace (keyword "K" (all)) "new-1" "new-2"))
     :mode 'apply :source buffer)
    (should (equal (buffer-string)
                   (dorg-test-qgr4z--fixture "keyword-all-0.2-golden.org")))))

(ert-deftest dorg-test-qgr4z-keyword-all-in-place-noncontiguous ()
  "All-replace uses the first matching slot and preserves other item order."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nA\n#+K: old-1\nB\n#+K: old-2\nC\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001) (replace (keyword "K" (all)) "new-1" "new-2"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nA\n#+K: new-1\n#+K: new-2\nB\nC\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-keyword-before-after-occurrence-byte-exact ()
  "Keyword occurrence anchors execute at the exact selected slots."
  (dolist
      (case
       '(((insert (keyword "K") (before (occurrence 2)) "before")
          "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\nA\n#+K: before\n#+K: second\nB\n")
         ((insert (keyword "K") (after (occurrence 1)) "after")
          "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\n#+K: after\nA\n#+K: second\nB\n")))
    (pcase-let ((`(,action ,expected) case))
      (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
        (dorg-api-execute `(on (id 1001) ,action) :mode 'apply :source buffer)
        (should (equal expected (with-current-buffer buffer (buffer-string))))))))

(ert-deftest dorg-test-qgr4z-retired-replacements-are-equivalent ()
  "Each non-ruled v0.2 replacement reproduces its captured v0.1 postimage.
The first lane commit captured the three baseline files on trunk `3cc7513d'.
Keyword all-replace is excluded because its v0.2 in-place law deliberately
differs from its 0.1 append-at-end postimage."
  (dolist
      (case
       '(((delete (keyword "K" (all))) "keyword-delete-0.1-golden.org")
         ((insert (drawer-line "LOGBOOK") (before-first) "- state")
          "drawer-insert-0.1-golden.org")
         ((replace (item (all)) (text "replacement"))
          "items-replace-0.1-golden.org")))
    (pcase-let ((`(,action ,golden) case))
      (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
        (dorg-api-execute `(on (id 1001) ,action) :mode 'apply :source buffer)
        (should (equal (with-current-buffer buffer (buffer-string))
                       (dorg-test-qgr4z--fixture golden)))))))

(ert-deftest dorg-test-qgr4z-text-delete-occurrence ()
  "Text occurrence two is a real v2 selection, not the retired fixed first cell."
  (dorg-test-qgr4z--with-input
      "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nsecond\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001) (delete (text (occurrence 2))))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-text-insert-before-occurrence ()
  "A v2 before-occurrence insert lands at the selected text member."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nsecond\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001)
                           (insert (text) (before (occurrence 2)) "inserted"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\ninserted\n\nsecond\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-text-replace-occurrence-n ()
  "Text occurrence N replaces that selected member, not a fixed first cell."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nsecond\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001) (replace (text (occurrence 2)) "changed"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nchanged\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-occurrence-reresolves ()
  "Occurrence selectors resolve against the current working copy per patch."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nsecond\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (text) (before (occurrence 2)) "X"))
       (on (id 1001) (replace (text (occurrence 2)) "changed")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nfirst\n#+X: keep\nchanged\n\nsecond\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-item-replace-occurrence ()
  "An item occurrence replacement reaches the pure executor with no legacy IR."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (dorg-api-execute '(on (id 1001)
                           (replace (item (occurrence 2)) (text "changed")))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\nchanged\n#+K: second\nB\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-guard-protected-keyword-item-occurrence ()
  "A selected DORG_ item is refused before a v2 occurrence write."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+DORG_HASH: keep\nbody\n")
      (save-buffer))
    (let ((before (with-current-buffer buffer (buffer-string)))
          (err (should-error
                (dorg-api-execute '(on (id 1001)
                                       (replace (item (occurrence 1)) (text "changed")))
                                  :mode 'apply :source buffer)
                :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should (eq 'protected-keyword
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal before (with-current-buffer buffer (buffer-string)))))))

(ert-deftest dorg-test-qgr4z-guard-item-all-materializes-empty-heading ()
  "The legal item-all replacement materializes an empty heading body."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001) (replace (item (all)) (text "materialized")))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nmaterialized\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-item-delete-and-insert-execute ()
  "The remaining item occurrence and insertion cells change the right slots."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (dorg-api-execute
     '(batch
       (on (id 1001) (delete (item (occurrence 2))))
       (on (id 1001) (insert (item) (before (occurrence 2)) (text "inserted"))))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\ninserted\n#+K: second\nB\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-drawer-line-replace-all ()
  "A drawer-line all replacement creates only the missing named drawer."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (dorg-api-execute '(on (id 1001)
                           (replace (drawer-line "LOGBOOK" (all)) "first" "second"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nfirst\nsecond\n:END:\n#+K: first\nA\n#+K: second\nB\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-drawer-line-all-in-place ()
  "All-replace keeps its drawer and surrounding body items in their slots."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nbefore\n:LOGBOOK:\nold-1\nold-2\n:END:\nafter\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001)
                           (replace (drawer-line "LOGBOOK" (all)) "new-1" "new-2"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nbefore\n:LOGBOOK:\nnew-1\nnew-2\n:END:\nafter\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-guard-drawer-first-of-duplicates ()
  "A drawer-line target addresses the first exact-name generic drawer only."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nfirst\n:END:\n:LOGBOOK:\nsecond\n:END:\n")
      (save-buffer))
    (dorg-api-execute '(on (id 1001) (replace (drawer-line "LOGBOOK" (all)) "changed"))
                      :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nchanged\n:END:\n:LOGBOOK:\nsecond\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-repeated-before-n-keeps-order ()
  "Repeated v2 before-occurrence inserts retain declaration order."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 2)) "A"))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 2)) "B")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\nA\nB\ntwo\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-repeated-after-n ()
  "Repeated after-occurrence inserts retain declaration order at one held slot."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (drawer-line "LOGBOOK") (after (occurrence 1)) "A"))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (after (occurrence 1)) "B")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\nA\nB\ntwo\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-anchor-deleted-keeps-slot ()
  "A held insertion cursor survives deletion of the member that named it."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 2)) "A"))
       (on (id 1001) (delete (drawer-line "LOGBOOK" (occurrence 3))))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 2)) "B")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\nA\nB\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-insert-ahead-shifts-cursor ()
  "An earlier insertion shifts a held slot without retargeting its anchor."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\nthree\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 3)) "A"))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 1)) "X"))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 3)) "B")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nX\none\ntwo\nA\nB\nthree\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-batch-replace-all-clears-cursors ()
  "An all-replace discards held insertion slots for its host before reuse."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\nthree\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 3)) "A"))
       (on (id 1001) (replace (drawer-line "LOGBOOK" (all)) "R1" "R2" "R3"))
       (on (id 1001) (insert (drawer-line "LOGBOOK") (before (occurrence 3)) "B")))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nR1\nR2\nB\nR3\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-drawer-line-delete-occurrence ()
  "Drawer occurrence replace/delete and all delete obey their distinct laws."
  (dorg-test-qgr4z--with-input "keyword-all-input.org" buffer
    (with-current-buffer buffer
      (erase-buffer)
      (insert "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\n:END:\n")
      (save-buffer))
    (dorg-api-execute
     '(batch
       (on (id 1001) (replace (drawer-line "LOGBOOK" (occurrence 2)) "TWO"))
       (on (id 1001) (delete (drawer-line "LOGBOOK" (occurrence 1))))
       (on (id 1001) (delete (drawer-line "MISSING" (all)))))
     :mode 'apply :source buffer)
    (should (equal "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nTWO\n:END:\n"
                   (with-current-buffer buffer (buffer-string))))))

(ert-deftest dorg-test-qgr4z-drawer-line-all-dolt-parity ()
  "The v2 drawer all row persists the same replacement through Dolt."
  (dorg-test-ql--with-dolt-synced
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\nold\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (response (dorg-api-execute
                      '(on (id 1001)
                           (replace (drawer-line "LOGBOOK" (all)) "first" "second"))
                      :mode 'apply :source source))
           (heading (gethash 1001 (dorg-model-load-headings dd root-id nil)))
           (drawer (cl-find-if
                    (lambda (item)
                      (and (cl-typep item 'dorg-drawer-item)
                           (equal "LOGBOOK" (dorg-drawer-item-name item))))
                    (dorg-heading-items heading))))
      (should (plist-get response :ok))
      (should (equal '("first" "second") (dorg-drawer-item-lines drawer))))))

(ert-deftest dorg-test-qgr4z-item-occurrence-dolt-parity ()
  "The v2 item occurrence row persists through the Dolt executor."
  (dorg-test-ql--with-dolt-synced
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\nold\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (response (dorg-api-execute
                      '(on (id 1001)
                           (replace (item (occurrence 2)) (text "changed")))
                      :mode 'apply :source source))
           (heading (gethash 1001 (dorg-model-load-headings dd root-id nil)))
           (item (nth 1 (dorg-heading-items heading))))
      (should (plist-get response :ok))
      (should (cl-typep item 'dorg-text-item))
      (should (equal "changed" (dorg-text-item-text item))))))

(ert-deftest dorg-test-qgr4z-item-delete-insert-dolt-parity ()
  "The remaining v2 item cells preserve their order through Dolt."
  (dorg-test-ql--with-dolt-synced
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n#+K: first\nold\n#+K: second\ntail\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (response (dorg-api-execute
                      '(batch
                        (on (id 1001) (delete (item (occurrence 2))))
                        (on (id 1001) (insert (item) (before (occurrence 2))
                                               (text "inserted"))))
                      :mode 'apply :source source))
           (heading (gethash 1001 (dorg-model-load-headings dd root-id nil))))
      (should (plist-get response :ok))
      (should (equal '(keyword text keyword text)
                     (mapcar (lambda (item)
                               (cond ((cl-typep item 'dorg-keyword-item) 'keyword)
                                     ((cl-typep item 'dorg-text-item) 'text)))
                             (dorg-heading-items heading))))
      (should (equal "inserted"
                     (dorg-text-item-text (nth 1 (dorg-heading-items heading))))))))

(ert-deftest dorg-test-qgr4z-drawer-anchor-cursor-dolt-parity ()
  "Repeated drawer anchors keep declaration order through the Dolt executor."
  (dorg-test-ql--with-dolt-synced
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n:LOGBOOK:\none\ntwo\n:END:\n"
      (:reg reg :dolt-dir dd)
    (let* ((root-id (dorg-backend-root-id (dorg-registration-backend reg)))
           (source (list :dolt-dir dd :root-id root-id))
           (response (dorg-api-execute
                      '(batch
                        (on (id 1001) (insert (drawer-line "LOGBOOK")
                                               (before (occurrence 2)) "A"))
                        (on (id 1001) (insert (drawer-line "LOGBOOK")
                                               (before (occurrence 2)) "B")))
                      :mode 'apply :source source))
           (heading (gethash 1001 (dorg-model-load-headings dd root-id nil)))
           (drawer (cl-find-if (lambda (item)
                                 (and (cl-typep item 'dorg-drawer-item)
                                      (equal "LOGBOOK" (dorg-drawer-item-name item))))
                               (dorg-heading-items heading))))
      (should (plist-get response :ok))
      (should (equal '("one" "A" "B" "two") (dorg-drawer-item-lines drawer))))))

(provide 'dorg-qgr4z-golden-tests)
;;; dorg-qgr4z-golden-tests.el ends here
