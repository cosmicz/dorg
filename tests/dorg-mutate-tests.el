;;; dorg-mutate-tests.el --- Tests for the dorg-mutate API foundation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the source-generic typed mutation engine (`dorg-mutate'):
;; structured errors, source resolution, the closed typed-patch matrix, DOrg
;; metadata protection, deterministic capability refusals, source-preserving
;; plain-org execution, Dolt transactions, and engine-private cross-root moves.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-mutate)
(require 'dorg-api)                      ; public body verbs
(require 'dorg-model)
(require 'dorg-parse)
(require 'dorg-dolt)
(require 'dorg-sql-gen)
(require 'dorg-ext-read)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Helpers ──────────────────────────────────────────────────────────

(defmacro dorg-test-mutate--reason (form)
  "Evaluate FORM, expecting a `dorg-mutate-error', and return its reason."
  `(condition-case err
       (progn ,form (ert-fail "expected dorg-mutate-error, none signalled"))
     (dorg-mutate-error (dorg-mutate-error-reason err))))

(defmacro dorg-test-mutate--error-data (form)
  "Evaluate FORM, expecting a `dorg-mutate-error', and return its data plist.
Used by the C3 clean-break refusals to pin the exact reason/detail/variant."
  `(condition-case err
       (progn ,form (ert-fail "expected dorg-mutate-error, none signalled"))
     (dorg-mutate-error (dorg-mutate-error-data err))))

(defun dorg-test-mutate--assert-dorg-policy (source)
  "Assert SOURCE resolves to EXACTLY the `dorg' identity policy.
The C3 invariant recasts drive a supported property-put cell, so this
load-bearing assertion pins that the vehicle exercises the resolved `dorg'
policy (DORG_ID targeting) — legacy policy-ignoring heading targeting would
otherwise false-green.  Returns SOURCE."
  (should (eq 'dorg (dorg-mutate--source-identity
                     (dorg-mutate--resolve-source source))))
  source)

(defconst dorg-test-mutate--dolt-source
  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 100)
  "An explicit Dolt source descriptor that needs no live repository.")

(declare-function dorg-ext-read--resolve-source "dorg-ext-read")
(declare-function dorg-ext-read--freshness "dorg-ext-read")
(declare-function dorg-ext-read--cache-key "dorg-ext-read")

(defun dorg-test-mutate--ext-read-cache-key (ext-name source query)
  "Return the private extension read cache key for EXT-NAME/SOURCE/QUERY."
  (let ((resolved (dorg-ext-read--resolve-source source)))
    (dorg-ext-read--cache-key
     ext-name resolved query (dorg-ext-read--freshness resolved))))

;;; ─── Error Structure ──────────────────────────────────────────────────

(ert-deftest dorg-test-mutate-error-structured-data ()
  "`dorg-mutate-error' carries a structured plist with :reason and fields."
  (condition-case err
      (dorg-mutate--raise 'invalid-target "bad" :operation 'put :key "K")
    (dorg-mutate-error
     (should (eq (dorg-mutate-error-reason err) 'invalid-target))
     (let ((data (dorg-mutate-error-data err)))
       (should (eq (plist-get data :reason) 'invalid-target))
       (should (eq (plist-get data :operation) 'put))
       (should (equal (plist-get data :key) "K"))))))

(ert-deftest dorg-test-mutate-error-reasons-closed-set ()
  "Each documented reason is signallable and reportable."
  (dolist (reason dorg-mutate-error-reasons)
    (should (eq reason
                (condition-case err
                    (dorg-mutate--raise reason "x")
                  (dorg-mutate-error (dorg-mutate-error-reason err)))))))

;;; ─── Source Resolution ────────────────────────────────────────────────

(ert-deftest dorg-test-mutate-resolve-dolt-descriptor ()
  "An explicit Dolt descriptor resolves without a live repository."
  (let ((src (dorg-mutate--resolve-source dorg-test-mutate--dolt-source)))
    (should (eq (dorg-mutate-source-kind src) 'dolt))
    (should (equal (dorg-mutate-source-root-id src) 100))
    (should (string-suffix-p "/" (dorg-mutate-source-dolt-dir src)))))

(ert-deftest dorg-test-mutate-resolve-plain-org-file ()
  "An explicit plain-org file descriptor resolves to a plain-org source."
  (let ((src (dorg-mutate--resolve-source '(:file "/tmp/dorg-test.org"))))
    (should (eq (dorg-mutate-source-kind src) 'plain-org))
    (should (string-suffix-p "dorg-test.org" (dorg-mutate-source-file src)))))

(ert-deftest dorg-test-mutate-resolve-registered-buffer ()
  "A live registered buffer resolves via the registry to a plain-org source."
  (let ((buf (generate-new-buffer "dorg-mutate-test.org")))
    (unwind-protect
        (with-current-buffer buf
          (set-visited-file-name "/tmp/dorg-mutate-buf-test.org" t)
          (set-buffer-modified-p nil)
          (let ((src (dorg-mutate--resolve-source buf)))
            (should (eq (dorg-mutate-source-kind src) 'plain-org))
            (should (eq (dorg-mutate-source-buffer src) buf))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(ert-deftest dorg-test-mutate-resolve-dolt-requires-root-id ()
  "A Dolt descriptor without :root-id is an invalid target."
  (should (eq 'invalid-target
              (dorg-test-mutate--reason
               (dorg-mutate--resolve-source '(:dolt-dir "/tmp/x"))))))

(ert-deftest dorg-test-mutate-resolve-dolt-rejects-non-id-root ()
  "A Dolt descriptor with a non-ID :root-id is an invalid target."
  (should (eq 'invalid-target
              (dorg-test-mutate--reason
               (dorg-mutate--resolve-source
                '(:dolt-dir "/tmp/x" :root-id "not-a-number"))))))

(ert-deftest dorg-test-mutate-resolve-dolt-normalizes-numeric-string-root ()
  "A numeric-string :root-id resolves and normalizes to an integer."
  (let ((src (dorg-mutate--resolve-source
              '(:dolt-dir "/tmp/x" :root-id "42"))))
    (should (eq (dorg-mutate-source-kind src) 'dolt))
    (should (equal (dorg-mutate-source-root-id src) 42))))

(ert-deftest dorg-test-mutate-resolve-unknown-descriptor ()
  "An unrecognized source descriptor is an invalid target."
  (should (eq 'invalid-target
              (dorg-test-mutate--reason
               (dorg-mutate--resolve-source '(:bogus "x"))))))

;;; ─── Native typed-patch validation ────────────────────────────────────

(defun dorg-test-mutate--heading-address (id)
  "Return the canonical typed heading address for ID."
  (list :heading :id id))

(defun dorg-test-mutate--external-address (kind key value)
  "Return a canonical typed external KEY/VALUE address of KIND."
  (list kind :extern t :key key :value value))

(defun dorg-test-mutate--property-put-patch (id key value)
  "Return a canonical property-put patch for heading ID."
  (list :op 'put
        :target (list :property :host (dorg-test-mutate--heading-address id)
                      :name key)
        :value value))

(defun dorg-test-mutate--property-delete-patch (id key)
  "Return a canonical property-delete patch for heading ID."
  (list :op 'delete
        :target (list :property :host (dorg-test-mutate--heading-address id)
                      :name key)))

(defun dorg-test-mutate--planning-patch (id &rest fields)
  "Return a canonical planning update patch for heading ID and FIELDS."
  (append (list :op 'update
                :target (list :planning :host
                              (dorg-test-mutate--heading-address id)))
          fields))

(defun dorg-test-mutate--heading-update-patch (id &rest fields)
  "Return a canonical heading update patch for ID and FIELDS."
  (list :op 'update :target (dorg-test-mutate--heading-address id)
        :fields fields))

(defun dorg-test-mutate--tree-insert-patch (under heading &rest anchors)
  "Return a canonical tree insert below UNDER with HEADING and ANCHORS."
  (list :op 'insert :target (append (list :tree :under under) anchors)
        :heading heading))

(defun dorg-test-mutate--delete-patch (id &optional subtree)
  "Return a canonical heading or SUBTREE delete patch for ID."
  (list :op 'delete
        :target (list (if subtree :subtree :heading) :id id)))

(defun dorg-test-mutate--move-patch (id under &rest fields)
  "Return a canonical subtree move patch for ID beneath UNDER.
FIELDS are private top-level move fields such as :to-source."
  (append (list :op 'move
                :target (if (consp id) id (list :subtree :id id))
                :to (list :under under))
          fields))

(defun dorg-test-mutate--move-before-patch (id under before &rest fields)
  "Return a canonical subtree move of ID below UNDER and before BEFORE."
  (append (list :op 'move
                :target (if (consp id) id (list :subtree :id id))
                :to (list :under under :before before))
          fields))

(defun dorg-test-mutate--keyword-patch (verb host key &rest fields)
  "Return a canonical typed keyword collection patch.
VERB is a typed verb, HOST a typed host, KEY the keyword name, and FIELDS the
top-level payload."
  (append (list :op verb :target (list :keywords :host host :name key)) fields))

(defun dorg-test-mutate--keyword-occurrence-patch
    (verb host key occurrence &rest fields)
  "Return a canonical typed single-keyword occurrence patch."
  (append (list :op verb
                :target (list :keyword :host host :name key
                              :occurrence occurrence))
          fields))

(defun dorg-test-mutate--plan-patch-batch (source patches &rest options)
  "Plan typed PATCHES for SOURCE with engine OPTIONS."
  (apply #'dorg-mutate--plan-patches source patches nil options))

(defun dorg-test-mutate--apply-patch-batch (source patches &rest options)
  "Apply typed PATCHES to SOURCE with engine OPTIONS."
  (apply #'dorg-mutate--apply-patches source patches nil options))

(ert-deftest dorg-test-mutate-native-validator-accepts-closed-matrix-cell ()
  "A closed typed property-put patch survives validation byte-for-byte."
  (let ((patch (dorg-test-mutate--property-put-patch 3 "K" "V")))
    (should (equal (list patch) (dorg-mutate--validate-patches (list patch))))))

(ert-deftest dorg-test-mutate-native-validator-protects-dorg-id ()
  "The engine refuses DORG_ID even when a caller bypasses the public API."
  (should (eq 'invalid-target
              (dorg-test-mutate--reason
               (dorg-mutate--validate-patches
                (list (dorg-test-mutate--property-put-patch 3 "DORG_ID" "9")))))))

(ert-deftest dorg-test-mutate-native-validator-protects-dorg-keywords ()
  "Every DORG_* keyword is protected at the engine boundary."
  (dolist (key '("DORG_ID" "DORG_HASH" "DORG_DOLT_DIR" "DORG_FUTURE"))
    (should
     (eq 'invalid-target
         (dorg-test-mutate--reason
          (dorg-mutate--validate-patches
           `((:op insert :target (:keywords :host :root :name ,key)
                  :position :after-last :value "x"))))))))

(ert-deftest dorg-test-mutate-native-validator-rejects-restore-fields-on-update ()
  "Restore-only :id/:items fields never enter a heading update."
  (dolist (fields '((:id 9) (:items ())))
    (should
     (eq 'invalid-op
         (dorg-test-mutate--reason
          (dorg-mutate--validate-patches
           `((:op update :target (:heading :id 3) :fields ,fields))))))))

(ert-deftest dorg-test-mutate-native-validator-validates-tree-anchors ()
  "Tree anchors are typed non-root heading addresses, not unchecked payloads."
  (should
   (eq 'invalid-target
       (dorg-test-mutate--reason
        (dorg-mutate--validate-patches
         '((:op insert :target (:tree :under :root :before :root)
                :heading (:title "T"))))))))

(ert-deftest dorg-test-kixox-native-validator-rejects-raw-numeric-insert-anchor ()
  "The typed IR boundary rejects the retired bare-number insert anchor."
  (should
   (eq 'invalid-target
       (dorg-test-mutate--reason
        (dorg-mutate--validate-patches
         '((:op insert
            :target (:tree :under (:heading :id 7) :before 9)
            :heading (:title "T"))))))))

(ert-deftest dorg-test-kixox-native-validator-rejects-raw-numeric-move-anchor ()
  "The typed IR boundary rejects the retired bare-number move anchor."
  (should
   (eq 'invalid-target
       (dorg-test-mutate--reason
        (dorg-mutate--validate-patches
         '((:op move :target (:subtree :id 42)
            :to (:under (:heading :id 7) :after 9))))))))

(ert-deftest dorg-test-mutate-native-validator-validates-move-policy ()
  "Private move policy fields are booleans and destination sources are resolved."
  (dolist (patch
           '((:op move :target (:subtree :id 8)
                  :to (:under (:heading :id 3)) :atomic yes)
             (:op move :target (:subtree :id 8)
                  :to (:under (:heading :id 3))
                  :to-source (:dolt-dir "/tmp/repo-a" :root-id 2))))
    (should (eq 'invalid-target
                (dorg-test-mutate--reason
                 (dorg-mutate--validate-patches (list patch)))))))

;;; ─── Source-Bound Revision Tokens ─────────────────────────────────────

;; The stale check must bind a revision token to the source it was
;; captured from.  A token replayed against a DIFFERENT plain-org source
;; that happens to share the same change counter (buffer tick / file
;; mtime) must read as stale, not be silently accepted.  These exercise
;; the predicate with coinciding counters so only source identity differs
;; — reverting the source-binding code makes them fail.

(ert-deftest dorg-test-mutate-revision-stale-on-cross-buffer-token ()
  "A buffer token is stale against a different buffer source at the same tick.
Source identity (buffer name and visited file) is part of the token, so a
matching `:tick' alone does not make a foreign token current."
  (let ((token '(:kind buffer :buffer "a.org" :file "/tmp/a.org" :tick 7)))
    ;; Different buffer name, identical tick -> stale.
    (should (dorg-mutate--revision-stale-p
             token '(:kind buffer :buffer "b.org" :file "/tmp/a.org" :tick 7)))
    ;; Same name but different visited file, identical tick -> stale.
    (should (dorg-mutate--revision-stale-p
             token '(:kind buffer :buffer "a.org" :file "/tmp/b.org" :tick 7)))
    ;; Control: identical source and tick is NOT stale.
    (should-not (dorg-mutate--revision-stale-p token (copy-sequence token)))))

(ert-deftest dorg-test-mutate-revision-stale-on-cross-file-token ()
  "A file token is stale against a different file source at the same mtime.
The file path is part of the token, so a coinciding `:mtime' does not make
a token captured from another file current."
  (let ((token '(:kind file :file "/tmp/a.org" :mtime (25000 0))))
    ;; Different path, identical mtime -> stale.
    (should (dorg-mutate--revision-stale-p
             token '(:kind file :file "/tmp/b.org" :mtime (25000 0))))
    ;; Control: identical path and mtime is NOT stale.
    (should-not (dorg-mutate--revision-stale-p token (copy-sequence token)))))

;;; ─── Selector-Based Item Resolution and Application ───────────────────

;; These exercise the source-neutral pure item core: resolving a
;; normalized selector against a model item list and producing an
;; updated item list.  No backend persistence is involved.

(defun dorg-test-mutate--kw (key value &optional pb)
  "A keyword item with KEY/VALUE and optional post-blank PB."
  (dorg-keyword-item :keyword key :value value :post-blank (or pb 0)))

(defun dorg-test-mutate--text (text &optional pb)
  "A text item with TEXT and optional post-blank PB."
  (dorg-text-item :text text :post-blank (or pb 0)))

(defun dorg-test-mutate--drawer (name lines &optional pb)
  "A raw-lines drawer item with NAME, LINES, and optional post-blank PB."
  (dorg-drawer-item :name name :lines lines :post-blank (or pb 0)))

(defun dorg-test-mutate--item-shape (item)
  "Return ITEM's kind and semantic payload for order assertions."
  (cond
   ((cl-typep item 'dorg-keyword-item)
    (list 'keyword (dorg-keyword-item-keyword item)
          (dorg-keyword-item-value item)))
   ((cl-typep item 'dorg-text-item)
    (list 'text (dorg-text-item-text item)))
   ((cl-typep item 'dorg-drawer-item)
    (list 'drawer (dorg-drawer-item-name item)))
   ((cl-typep item 'dorg-planning-item)
    (list 'planning (dorg-planning-item-scheduled item)
          (dorg-planning-item-deadline item)
          (dorg-planning-item-closed item)))
   (t (ert-fail (format "unknown item kind: %S" item)))))

(defun dorg-test-mutate--items-shape (items)
  "Return semantic shape projections for ITEMS in document order."
  (mapcar #'dorg-test-mutate--item-shape items))

;;; ─── Typed item transforms ────────────────────────────────────────────

(ert-deftest dorg-test-mutate-typed-keyword-occurrence-is-strict ()
  "Single-keyword replacement addresses exactly one one-based occurrence."
  (let* ((items (list (dorg-test-mutate--kw "A" "1")
                      (dorg-test-mutate--kw "B" "2")
                      (dorg-test-mutate--kw "A" "3")))
         (patch '(:op replace
                  :target (:keyword :host (:heading :id 5)
                                    :name "A" :occurrence 2)
                  :value "changed" :missing :error))
         (out (dorg-mutate--apply-item-patch items patch)))
    (should-not (equal (dorg-test-mutate--items-shape items)
                       (dorg-test-mutate--items-shape out)))
    (should (equal '("1" "2" "changed")
                   (mapcar #'dorg-keyword-item-value out)))
    (should (equal '("1" "2" "3")
                   (mapcar #'dorg-keyword-item-value items)))))

(ert-deftest dorg-test-mutate-typed-keyword-missing-refuses ()
  "A missing single occurrence refuses with typed missing detail."
  (let ((patch '(:op delete
                 :target (:keyword :host (:heading :id 5)
                                   :name "A" :occurrence 3)
                 :missing :error)))
    (condition-case err
        (progn
          (dorg-mutate--apply-item-patch
           (list (dorg-test-mutate--kw "A" "1")) patch)
          (ert-fail "expected dorg-mutate-error"))
      (dorg-mutate-error
       (should (eq 'invalid-target (dorg-mutate-error-reason err)))
       (should (eq 'missing
                   (plist-get (dorg-mutate-error-data err) :detail)))))))

(ert-deftest dorg-test-mutate-typed-keyword-occurrence-must-be-positive ()
  "Typed keyword occurrence zero, negative, and non-integer refuse prewrite."
  (dolist (occurrence '(0 -1 1.5 "1"))
    (dorg-test-mutate--with-buffer buf
      (let* ((before (buffer-string))
             (patch (dorg-test-mutate--keyword-occurrence-patch
                     'replace (dorg-test-mutate--heading-address 1001)
                     "FOO" occurrence :value "changed" :missing :error))
             (data (dorg-test-mutate--error-data
                    (dorg-test-mutate--apply-patch-batch buf (list patch)))))
        (should (eq 'invalid-target (plist-get data :reason)))
        ;; Validation failures carry no executor-level missing detail.  Under
        ;; the old >=0 contract, occurrence zero reached item resolution and
        ;; falsely returned `missing', so reason alone is not discriminating.
        (should-not (plist-member data :detail))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-mutate-typed-keyword-after-last-is-target-relative ()
  "After-last inserts after the final same-key item, before a trailing body."
  (let* ((items (list (dorg-test-mutate--kw "K" "a")
                      (dorg-test-mutate--kw "OTHER" "middle")
                      (dorg-test-mutate--kw "K" "b")
                      (dorg-test-mutate--text "tail")))
         (patch '(:op insert
                  :target (:keywords :host (:heading :id 5) :name "K")
                  :position :after-last :value "c"))
         (out (dorg-mutate--apply-item-patch items patch)))
    (should
     (equal '((keyword "K" "a")
              (keyword "OTHER" "middle")
              (keyword "K" "b")
              (keyword "K" "c")
              (text "tail"))
            (dorg-test-mutate--items-shape out)))
    (should (= 4 (length items)))))

(ert-deftest dorg-test-mutate-typed-keyword-after-last-missing-uses-host-end ()
  "When the key is absent, after-last inserts at the whole host-sequence end."
  (dolist (items (list (list (dorg-test-mutate--kw "OTHER" "middle")
                             (dorg-test-mutate--text "tail"))
                       nil))
    (let* ((before (dorg-test-mutate--items-shape items))
           (patch '(:op insert
                    :target (:keywords :host (:heading :id 5) :name "K")
                    :position :after-last :value "c"))
           (out (dorg-mutate--apply-item-patch items patch)))
      (should (equal (append before '((keyword "K" "c")))
                     (dorg-test-mutate--items-shape out))))))

(ert-deftest dorg-test-9diah-typed-keyword-before-first-is-target-relative ()
  "Before-first inserts before the first same-key item, after prior content."
  (let* ((items (list (dorg-test-mutate--text "lead")
                      (dorg-test-mutate--kw "K" "old-a")
                      (dorg-test-mutate--kw "OTHER" "middle")
                      (dorg-test-mutate--kw "K" "old-b")))
         (patch '(:op insert
                  :target (:keywords :host (:heading :id 5) :name "K")
                  :position :before-first :value "new"))
         (out (dorg-mutate--apply-item-patch items patch)))
    (should (equal '((text "lead")
                     (keyword "K" "new")
                     (keyword "K" "old-a")
                     (keyword "OTHER" "middle")
                     (keyword "K" "old-b"))
                   (dorg-test-mutate--items-shape out)))))

(ert-deftest dorg-test-2rx9n-typed-text-after-last-is-target-relative ()
  "Text insertion lands before a trailing drawer, not at whole-host end."
  (let* ((drawer (dorg-test-mutate--drawer "LOGBOOK" '("- old") 2))
         (items (list (dorg-test-mutate--text "old" 1) drawer))
         (before (dorg-test-mutate--items-shape items))
         (patch '(:op insert :target (:texts :host (:heading :id 5))
                  :position :after-last :value "new"))
         (out (dorg-mutate--apply-item-patch items patch)))
    ;; Transformation witness: deleting the call leaves BEFORE and fails this.
    (should-not (equal before (dorg-test-mutate--items-shape out)))
    (should (equal '((text "old") (text "new") (drawer "LOGBOOK"))
                   (dorg-test-mutate--items-shape out)))
    (should (equal '((text "old") (drawer "LOGBOOK")) before))
    (should (equal '("- old") (dorg-drawer-item-lines (nth 2 out))))
    (should (= 2 (dorg-item-post-blank (nth 2 out))))))

(ert-deftest dorg-test-2rx9n-typed-text-after-last-empty-uses-host-end ()
  "An empty text collection uses host end, including an empty host."
  (dolist (items (list (list (dorg-test-mutate--drawer
                              "LOGBOOK" '("- old") 1))
                       nil))
    (let* ((before (dorg-test-mutate--items-shape items))
           (patch '(:op insert :target (:texts :host (:heading :id 5))
                    :position :after-last :value "new"))
           (out (dorg-mutate--apply-item-patch items patch)))
      (should-not (equal before (dorg-test-mutate--items-shape out)))
      (should (equal (append before '((text "new")))
                     (dorg-test-mutate--items-shape out))))))

(ert-deftest dorg-test-9diah-typed-text-before-first-batch-preserves-order ()
  "Public declared before-first batch order is A,B,old, not B,A,old."
  (dorg-test-mutate--with-content
      buf (concat "* Task\n"
                  ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                  "old\n"
                  ":AUDIT:\n- line\n:END:\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string)))
      (dorg-api-execute
       '(batch
         (on (id 1001) (insert (text) (before-first) "A"))
         (on (id 1001) (insert (text) (before-first) "B")))
       :mode 'apply :source buf)
      ;; Transformation witness: deleting either insertion leaves the expected
      ;; three-text prefix absent or incomplete.
      (should-not (equal before (buffer-string)))
      (should (equal '((text "A") (text "B") (text "old") (drawer "AUDIT"))
                     (dorg-test-mutate--items-shape
                      (dorg-heading-items
                       (dorg-test-mutate--parsed-heading buf 1001))))))))

(ert-deftest dorg-test-9diah-typed-text-after-last-batch-preserves-order ()
  "Public declared after-last batch order is old,A,B."
  (dorg-test-mutate--with-content
      buf (concat "* Task\n"
                  ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                  "old\n"
                  ":AUDIT:\n- line\n:END:\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string)))
      (dorg-api-execute
       '(batch
         (on (id 1001) (insert (text) (after-last) "A"))
         (on (id 1001) (insert (text) (after-last) "B")))
       :mode 'apply :source buf)
      ;; Transformation witness: deleting either insertion leaves the expected
      ;; three-text prefix absent or incomplete.
      (should-not (equal before (buffer-string)))
      (should (equal '((text "old") (text "A") (text "B") (drawer "AUDIT"))
                     (dorg-test-mutate--items-shape
                      (dorg-heading-items
                       (dorg-test-mutate--parsed-heading buf 1001))))))))

(ert-deftest dorg-test-9diah-text-before-first-survives-delete-interleave ()
  "A delete between same-slot inserts cannot stale the before-first cursor."
  (dorg-test-mutate--with-content
      buf (concat "* Task\n"
                  ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                  "#+K: old-key\n"
                  "old\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string)))
      (dorg-api-execute
       '(batch
         (on (id 1001) (insert (text) (before-first) "A"))
         (on (id 1001) (delete (keyword "K" (occurrence 1))))
         (on (id 1001) (insert (text) (before-first) "B")))
       :mode 'apply :source buf)
      (should-not (equal before (buffer-string)))
      (should (equal '((text "A") (text "B") (text "old\n"))
                     (dorg-test-mutate--items-shape
                      (dorg-heading-items
                       (dorg-test-mutate--parsed-heading buf 1001))))))))

(ert-deftest dorg-test-9diah-text-before-first-survives-keyword-insert-interleave ()
  "Cross-collection insertion cannot stale a same-slot text cursor."
  (dorg-test-mutate--with-content
      buf (concat "* Task\n"
                  ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                  "#+K: old-key\n"
                  "old\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string)))
      (dorg-api-execute
       '(batch
         (on (id 1001) (insert (text) (before-first) "A"))
         (on (id 1001) (insert (keyword "K") (before-first) "new-key"))
         (on (id 1001) (insert (text) (before-first) "B")))
       :mode 'apply :source buf)
      (should-not (equal before (buffer-string)))
      (should (equal '((keyword "K" "new-key") (keyword "K" "old-key")
                       (text "A") (text "B") (text "old\n"))
                     (dorg-test-mutate--items-shape
                      (dorg-heading-items
                       (dorg-test-mutate--parsed-heading buf 1001))))))))

(ert-deftest dorg-test-9diah-text-before-first-restarts-after-replace-items ()
  "Whole item replacement drops prior host cursors before a later insertion."
  (dorg-test-mutate--with-content
      buf (concat "* Task\n"
                  ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                  "old\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string)))
      (dorg-api-execute
       '(batch
         (on (id 1001) (insert (text) (before-first) "A"))
         (on (id 1001) (replace (items) (text "old")))
         (on (id 1001) (insert (text) (before-first) "B")))
       :mode 'apply :source buf)
      (should-not (equal before (buffer-string)))
      (should (equal '((text "B") (text "old\n"))
                     (dorg-test-mutate--items-shape
                      (dorg-heading-items
                       (dorg-test-mutate--parsed-heading buf 1001))))))))

(ert-deftest dorg-test-9diah-empty-keyword-text-collections-coincide ()
  "Before-first and after-last coincide when the addressed collection is empty."
  (let* ((keyword-items (list (dorg-test-mutate--kw "OTHER" "1")
                              (dorg-test-mutate--text "tail")))
         (keyword-before '(:op insert
                           :target (:keywords :host (:heading :id 5) :name "K")
                           :position :before-first :value "v"))
         (keyword-after (copy-sequence keyword-before))
         (text-items (list (dorg-test-mutate--kw "OTHER" "1")))
         (text-before '(:op insert
                        :target (:texts :host (:heading :id 5))
                        :position :before-first :value "body"))
         (text-after (copy-sequence text-before)))
    (plist-put keyword-after :position :after-last)
    (plist-put text-after :position :after-last)
    (should
     (equal (dorg-test-mutate--items-shape
             (dorg-mutate--apply-item-patch keyword-items keyword-before))
            (dorg-test-mutate--items-shape
             (dorg-mutate--apply-item-patch keyword-items keyword-after))))
    (should
     (equal (dorg-test-mutate--items-shape
             (dorg-mutate--apply-item-patch text-items text-before))
            (dorg-test-mutate--items-shape
             (dorg-mutate--apply-item-patch text-items text-after))))))

(ert-deftest dorg-test-2rx9n-typed-text-replace-preserves-post-blank ()
  "Text replacement preserves and addresses the first prose item only."
  (let* ((items (list (dorg-test-mutate--text "old" 3)
                      (dorg-test-mutate--kw "K" "v")
                      (dorg-test-mutate--text "later" 1)))
         (patch '(:op replace
                  :target (:text :host (:heading :id 5) :occurrence 1)
                  :missing :error :value "new"))
         (out (dorg-mutate--apply-item-patch items patch)))
    (should-not (equal (dorg-test-mutate--items-shape items)
                       (dorg-test-mutate--items-shape out)))
    (should (equal "new" (dorg-text-item-text (car out))))
    (should (= 3 (dorg-item-post-blank (car out))))
    (should (equal '((text "new") (keyword "K" "v") (text "later"))
                   (dorg-test-mutate--items-shape out)))
    (should (equal '((text "old") (keyword "K" "v") (text "later"))
                   (dorg-test-mutate--items-shape items)))))

(ert-deftest dorg-test-2rx9n-typed-text-selector-refusals-are-prewrite ()
  "The bounded typed capability rejects selectors without touching bytes."
  (dolist
      (case
       '((invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 0)
               :missing :error :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 2)
               :missing :error :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 1.5)
               :missing :error :value "x"))
         (invalid-op invalid-position
          (:op insert :target (:texts :host (:heading :id 1001))
               :position :front :value "x"))
         (invalid-op nil
          (:op insert :target (:texts :host (:heading :id 1001))
               :value "x"))
         (invalid-op nil
          (:op insert :target (:texts :host (:heading :id 1001)
                                      :occurrence 1)
               :position :after-last :value "x"))
         (invalid-target nil
          (:op insert :target (:texts :host (:heading :id 1001))
               :position :after-last :value 42))
         (invalid-target structural-payload-boundary
          (:op insert :target (:texts :host (:heading :id 1001))
               :position :after-last :value "* fused"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:texts :host (:heading :id 1001))
               :position :after-last :value "first\n\nsecond"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:keywords :host (:heading :id 1001) :name "K")
               :position :after-last :value "two\nlines"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:drawer-lines :host (:heading :id 1001)
                                             :name "AUDIT" :occurrence 1)
               :position :before-first :missing :create :value "two\nlines"))
         (invalid-target missing
          (:op insert :target (:drawer-lines :host (:heading :id 1001)
                                             :name "AUDIT" :occurrence 1)
               :position :before-first :missing :error :value "- refused"))
         (invalid-target missing
          (:op insert :target (:drawer-lines :host (:heading :id 1001)
                                             :name "AUDIT" :occurrence 1)
               :position :after-last :missing :error :value "- refused"))
         (invalid-op invalid-missing-policy
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 1)
               :missing :create :value "x"))
         (invalid-op nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 1)
               :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :occurrence 1)
               :missing :error :value nil))))
    (dorg-test-mutate--with-buffer buf
      (let* ((before (buffer-string))
             (data (dorg-test-mutate--error-data
                    (dorg-test-mutate--apply-patch-batch
                     buf (list (nth 2 case))))))
        (should (eq (car case) (plist-get data :reason)))
        (when (cadr case)
          (should (eq (cadr case) (plist-get data :detail))))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-2rx9n-typed-text-missing-is-strict-and-prewrite ()
  "Occurrence one on a heading with no prose reports missing without a list."
  (let* ((items (list (dorg-test-mutate--drawer "LOGBOOK" '("- old") 1)))
         (before (dorg-test-mutate--items-shape items))
         (patch '(:op replace
                  :target (:text :host (:heading :id 5) :occurrence 1)
                  :missing :error :value "x")))
    (condition-case err
        (progn
          (dorg-mutate--apply-item-patch items patch)
          (ert-fail "expected strict missing refusal"))
      (dorg-mutate-error
       (should (eq 'invalid-target (dorg-mutate-error-reason err)))
       (should (eq 'missing
                   (plist-get (dorg-mutate-error-data err) :detail)))))
    (should (equal before (dorg-test-mutate--items-shape items)))))

(ert-deftest dorg-test-mutate-typed-whole-items-replacement-is-independent ()
  "Whole-items replacement discards the old list and allocates fresh values."
  (let* ((items (list (dorg-test-mutate--kw "OLD" "x")))
         (patch '(:op replace :target (:items :host (:heading :id 5))
                  :items ((:kind text :text "a")
                          (:kind keyword :keyword "K" :value "v"))))
         (out (dorg-mutate--apply-item-patch items patch)))
    (should (equal '((text "a") (keyword "K" "v"))
                   (dorg-test-mutate--items-shape out)))
    (should (equal '((keyword "OLD" "x"))
                   (dorg-test-mutate--items-shape items)))))

(ert-deftest dorg-test-mutate-typed-item-heading-copy-refreshes-hash ()
  "Applying an item patch copies its heading and refreshes the items hash."
  (let* ((heading (make-dorg-heading
                   :id 7 :parent-id nil :level 1 :post-blank 0
                   :items (list (dorg-test-mutate--text "old" 1))))
         (patch '(:op replace
                  :target (:text :host (:heading :id 7) :occurrence 1)
                  :missing :error :value "new"))
         (out (dorg-mutate--apply-item-patch-to-heading heading patch)))
    (should-not (eq out heading))
    (should (equal "old" (dorg-text-item-text
                           (car (dorg-heading-items heading)))))
    (should (equal "new" (dorg-text-item-text
                           (car (dorg-heading-items out)))))
    (should (equal (dorg-heading-items-hash out)
                   (let ((copy (dorg-heading-deep-copy out)))
                     (dorg-heading-compute-items-hash copy))))))

;;; ─── Plain-Org Executor ───────────────────────────────────────────────

;; These exercise the wired plain-org executor end to end: parse a live
;; buffer / explicit file, apply a typed batch, render back, and
;; assert the source text and structured result.  They cover base-revision
;; planning and stale rejection, dry-run no-edit, no-partial-write
;; rollback, save semantics, root keyword / property / planning mutation,
;; occurrence item replacement, heading insert/update/delete/move, and
;; render/parse stability.

(defconst dorg-test-mutate--sample
  "#+TITLE: Project

* First
:PROPERTIES:
:DORG_ID: 1001
:END:
First body.

* Second
:PROPERTIES:
:DORG_ID: 1002
:END:
Second body.
"
  "A small dorg-managed org buffer with two DORG_ID'd headings.")

(defconst dorg-test-mutate--repeated-sample
  "#+TITLE: Project
#+SEQ_TODO: TODO NEXT | DONE
#+SEQ_TODO: BUG | FIXED

* First
:PROPERTIES:
:DORG_ID: 1001
:END:
First body.
"
  "A root preamble carrying two same-key `#+SEQ_TODO:' keyword items.
Used to pin repeated-keyword cardinality/order under typed occurrence patches.")

(defmacro dorg-test-mutate--with-buffer (var &rest body)
  "Bind VAR to a live org buffer visiting a temp `dorg-test-mutate--sample'.
The buffer has a backing file (so it resolves to a plain-org source) but
BODY exercises the live-buffer write path.  Evaluate BODY with VAR
current, then kill the buffer and delete the temp file."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((dorg-test-mutate--tmp
           (make-temp-file "dorg-mutate-exec" nil ".org"
                           dorg-test-mutate--sample))
          (,var (find-file-noselect dorg-test-mutate--tmp)))
     (unwind-protect
         (with-current-buffer ,var
           (let ((org-inhibit-startup t)) (org-mode))
           (goto-char (point-min))
           ,@body)
       (with-current-buffer ,var (set-buffer-modified-p nil))
       (kill-buffer ,var)
       (delete-file dorg-test-mutate--tmp))))

(defmacro dorg-test-mutate--with-content (var content &rest body)
  "Bind VAR to a live org buffer visiting a temp file seeded with CONTENT.
Like `dorg-test-mutate--with-buffer' but for sources other than
`dorg-test-mutate--sample' — CONTENT is any org string."
  (declare (indent 2) (debug (symbolp form body)))
  `(let* ((dorg-test-mutate--tmp
           (make-temp-file "dorg-mutate-exec" nil ".org" ,content))
          (,var (find-file-noselect dorg-test-mutate--tmp)))
     (unwind-protect
         (with-current-buffer ,var
           (let ((org-inhibit-startup t)) (org-mode))
           (goto-char (point-min))
           ,@body)
       (with-current-buffer ,var (set-buffer-modified-p nil))
       (kill-buffer ,var)
       (delete-file dorg-test-mutate--tmp))))

(defun dorg-test-mutate--heading-titles (buffer)
  "Return the heading titles in BUFFER in document order."
  (with-current-buffer buffer
    (let (titles)
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (h) (push (org-element-property :raw-value h) titles)))
      (nreverse titles))))

(defun dorg-test-mutate--keyword-values (buffer key)
  "Return, in document order, the values of every `#+KEY:' line in BUFFER.
KEY is matched case-sensitively; cardinality is preserved so repeated
keywords each appear, letting tests assert order + count."
  (with-current-buffer buffer
    (let ((re (format "^#\\+%s: \\(.*\\)$" (regexp-quote key)))
          (case-fold-search nil)
          vals)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (push (match-string-no-properties 1) vals)))
      (nreverse vals))))

(defconst dorg-test-mutate--keyword-insert-sample
  "#+K: root-a
#+OTHER: root-middle
#+K: root-b
Root tail.

* Existing
:PROPERTIES:
:DORG_ID: 1001
:END:
#+K: a
#+OTHER: middle
#+K: b
Tail.

* Missing
:PROPERTIES:
:DORG_ID: 1002
:END:
#+OTHER: middle
Tail.

* After
:PROPERTIES:
:DORG_ID: 1003
:END:
After body.
"
  "Public keyword-insert fixture with interleaved and missing-key hosts.")

(defun dorg-test-mutate--parsed-heading (buffer id)
  "Return the parsed heading with ID from BUFFER."
  (let* ((parsed (dorg-parse-buffer buffer))
         (heading (cl-find id (dorg-parse-result-headings parsed)
                           :key #'dorg-heading-id :test #'eql)))
    (should heading)
    heading))

(ert-deftest dorg-test-mutate-public-keyword-insert-plain-heading-placement ()
  "Public plain-org insertion creates at target-relative heading positions.
The existing-key arm proves occurrence one is preserved and cardinality grows;
the missing-key arm proves host-end fallback before the following heading."
  (dorg-test-mutate--with-content
      buf dorg-test-mutate--keyword-insert-sample
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string))
          (res (dorg-api-execute
                '(batch
                  (on (id 1001) (insert (keyword "K") (after-last) "c"))
                  (on (id 1002) (insert (keyword "K") (after-last) "c")))
                :mode 'apply :source buf)))
      (should (plist-get res :ok))
      (should-not (equal before (buffer-string)))
      (should
       (equal '((keyword "K" "a")
                (keyword "OTHER" "middle")
                (keyword "K" "b")
                (keyword "K" "c")
                (text "Tail."))
              (dorg-test-mutate--items-shape
               (dorg-heading-items
                (dorg-test-mutate--parsed-heading buf 1001)))))
      (should
       (equal '((keyword "OTHER" "middle")
                (text "Tail.")
                (keyword "K" "c"))
              (dorg-test-mutate--items-shape
               (dorg-heading-items
                (dorg-test-mutate--parsed-heading buf 1002))))))
    ;; The root C3 cell stays fail-closed on plain-org, with no partial bytes.
    (let* ((before-root (buffer-string))
           (data (dorg-test-mutate--error-data
                  (dorg-api-execute
                   '(on document (insert (keyword "K") (after-last) "root-c"))
                   :mode 'apply :source buf))))
      (should (eq 'unsupported (plist-get data :reason)))
      (should (eq 'no-source-preserving-editor (plist-get data :detail)))
      (should (equal '(:host root) (plist-get data :variant)))
      (should (equal before-root (buffer-string))))))

;;; Base-revision planning + stale rejection ---------------------------

(ert-deftest dorg-test-mutate-exec-plan-buffer-revision ()
  "Planning a live-buffer source yields a buffer-kind revision token."
  (dorg-test-mutate--with-buffer buf
    (let* ((plan (dorg-test-mutate--plan-patch-batch
                  buf (list (dorg-test-mutate--property-put-patch
                             1001 "CATEGORY" "work"))))
           (rev (plist-get plan :base-revision)))
      (should (eq (plist-get rev :kind) 'buffer))
      (should (integerp (plist-get rev :tick))))))

(ert-deftest dorg-test-mutate-exec-stale-rejected-before-edit ()
  "A stale `:base-revision' is rejected and leaves the buffer unchanged.
Uses the supported source-preserving property-put cell on the buffer's
DORG_ID heading (identity `dorg') as the CAS vehicle; the stale check is
unchanged."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((patch (dorg-test-mutate--property-put-patch
                   1001 "CATEGORY" "work"))
           (rev (plist-get (dorg-test-mutate--plan-patch-batch buf (list patch))
                           :base-revision))
          (before (buffer-string)))
      ;; Mutate the buffer out from under the captured token.
      (goto-char (point-max))
      (insert "\n* Third\n")
      (let ((after-edit (buffer-string)))
        (should (eq 'stale
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--apply-patch-batch
                      buf (list patch) :base-revision rev))))
        ;; The apply must not have edited further: only our manual edit
        ;; is present.
        (should (equal (buffer-string) after-edit))
        (should-not (equal (buffer-string) before))
        ;; the CAS-rejected property was never written
        (should-not (string-match-p ":CATEGORY:" (buffer-string)))))))

(ert-deftest dorg-test-9diah-plain-validator-veto-is-zero-write ()
  "A plain-org validator veto leaves the live buffer byte-identical."
  (let ((dorg--extensions nil))
    (dorg-register-extension
     'plain-veto
     :schema "CREATE TABLE plain_veto_ix (id INT)"
     :version 1
     :capabilities '(plain-org)
     :validate (lambda (ctx)
                 (should (eq 'plain-org
                             (dorg-ext-index-context-source-kind ctx)))
                 (dorg-mutate--raise
                  'invariant "validator veto"
                  :detail 'candidate-validator-veto)))
    (dorg-test-mutate--with-buffer buf
      (dorg-test-mutate--assert-dorg-policy buf)
      (let* ((before (buffer-string))
             (patch (dorg-test-mutate--property-put-patch
                     1001 "CATEGORY" "work"))
             (data (dorg-test-mutate--error-data
                    (dorg-test-mutate--apply-patch-batch buf (list patch)))))
        (should (eq 'invariant (plist-get data :reason)))
        (should (eq 'candidate-validator-veto (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-9diah-live-buffer-validator-sees-candidate-effects-prewrite ()
  "Live-buffer validators receive changed entries after simulation, pre-byte."
  (let ((dorg--extensions nil))
    (dorg-test-mutate--with-content
        buf "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((before (buffer-string))
            (seen nil))
        (dorg-register-extension
         'plain-effect-probe
         :schema "CREATE TABLE plain_effect_probe_ix (id INT)"
         :version 1
         :capabilities '(plain-org)
         :validate (lambda (ctx)
                     (let ((changed (dorg-ext-index-context-changed ctx)))
                       (setq seen changed)
                       (should (cl-find 1001 changed
                                        :key #'dorg-ext-index-entry-id
                                        :test #'eql))
                       (should (equal before
                                      (with-current-buffer buf
                                        (buffer-string)))))))
        (dorg-api-execute
         '(on (id 1001) (insert (text) (after-last) "New."))
         :mode 'apply :source buf)
        (should (cl-find 1001 seen :key #'dorg-ext-index-entry-id :test #'eql))
        (should-not (equal before (buffer-string)))))))

(ert-deftest dorg-test-mutate-exec-fresh-revision-accepted ()
  "A current `:base-revision' is accepted and the patch applies.  Uses the supported
property-put cell on the DORG_ID heading (identity `dorg'); wrong initial
state (no CATEGORY) -> corrected (property written), before != after."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((before (buffer-string))
           (patch (dorg-test-mutate--property-put-patch
                   1001 "CATEGORY" "work"))
           (rev (plist-get (dorg-test-mutate--plan-patch-batch buf (list patch))
                           :base-revision)))
      (should-not (string-match-p ":CATEGORY:" before))
      (let ((result (dorg-test-mutate--apply-patch-batch
                     buf (list patch) :base-revision rev)))
        (should (plist-get result :ok))
        (should-not (plist-get result :dry-run))
        (should (string-match-p ":CATEGORY: +work" (buffer-string)))
        (should-not (equal (buffer-string) before))))))

;;; Dry-run -------------------------------------------------------------

(ert-deftest dorg-test-mutate-exec-dry-run-no-edit ()
  "Dry-run returns a plan and never touches the buffer."
  (dorg-test-mutate--with-buffer buf
    (let ((before (buffer-string))
          (plan (dorg-test-mutate--plan-patch-batch
                 buf (list (dorg-test-mutate--property-put-patch
                            1001 "CATEGORY" "work")))))
      (should (plist-get plan :dry-run))
      (should (equal (buffer-string) before)))))

;;; Root keyword mutation ----------------------------------------------

(ert-deftest dorg-test-mutate-exec-replace-keywords-root-refuses ()
  "AC (clean-break C3): a root-host keyword replacement has no source-preserving
editor on plain-org and is refused pre-write with the exact
`unsupported'/`no-source-preserving-editor' + variant =(:host root)=, leaving
the buffer byte-identical.  The root preamble-keyword semantics are retained on
Dolt by `dorg-test-mutate-dolt-root-keyword-repeated'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--keyword-patch
                           'replace :root "AUTHOR" :values '("OC")))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (equal '(:host root) (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-delete-keywords-root-refuses ()
  "AC (clean-break C3): a root-host keyword collection delete has no source-preserving
editor on plain-org and is refused pre-write with the exact
`unsupported'/`no-source-preserving-editor' + variant =(:host root)=, buffer
byte-identical.  Root keyword semantics retained on Dolt by
`dorg-test-mutate-dolt-root-keyword-repeated'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--keyword-patch
                           'delete :root "TITLE"))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (equal '(:host root) (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

;;; Property + planning mutation ---------------------------------------

(ert-deftest dorg-test-mutate-exec-put-property ()
  "A property put adds a non-DORG_ID property to the heading drawer."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--apply-patch-batch
     buf (list (dorg-test-mutate--property-put-patch 1001 "CATEGORY" "work")))
    (should (string-match-p ":CATEGORY: work" (buffer-string)))
    ;; identity is preserved
    (should (string-match-p ":DORG_ID: 1001" (buffer-string)))))

(ert-deftest dorg-test-mutate-exec-update-planning-refuses ()
  "AC (clean-break C3): a planning update has no source-preserving editor on
plain-org (whole-verb collapse, C3 slice S2) and is refused pre-write with the
exact `unsupported'/`no-source-preserving-editor' + empty variant nil, buffer
byte-identical.  Planning writes are retained on Dolt by
`dorg-test-mutate-dolt-property-and-planning'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--planning-patch
                           1002 :scheduled "<2026-05-29 Fri>"))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

;; Local-identity resolver: property ops target the ONE property drawer org
;; reads back as the heading's own, never a generic body :PROPERTIES: block.
;; The fixtures below give a heading whose only PROPERTIES-shaped drawer is a
;; generic body block (a :PROPERTIES: appearing after body content, which org
;; parses as a raw-lines drawer).  The identity-only leading drawer is dropped
;; by the parser, so `dorg-mutate--ps-find-properties' must NOT latch onto the
;; body block by name.

(defconst dorg-test-mutate--body-properties-sample
  "* H
:PROPERTIES:
:DORG_ID: 1001
:END:
Body line.
:PROPERTIES:
:FOO: bar
:END:
"
  "A heading whose only PROPERTIES-shaped drawer is a generic body block.
The leading identity drawer holds only :DORG_ID: (dropped from the model by
the parser), and a second :PROPERTIES: block sits AFTER `Body line.', which
org parses as a generic raw-lines drawer named PROPERTIES.")

(ert-deftest dorg-test-mutate-exec-put-property-preserves-body-drawer ()
  "Property put creates a local drawer and preserves a generic body block.
Transformation regression (P0): the heading's only PROPERTIES-shaped drawer
is a generic body :PROPERTIES: block (raw lines).  Property put must NOT
rewrite that block into a key/value drawer — doing so drops its lines.  A
fresh local identity/property drawer is created after the heading, carrying
the new key and the heading id, while `:FOO: bar' survives in place."
  (dorg-test-mutate--with-content buf dorg-test-mutate--body-properties-sample
    (dorg-test-mutate--apply-patch-batch
     buf (list (dorg-test-mutate--property-put-patch 1001 "K" "V")))
    (let ((text (buffer-string)))
      ;; The generic body block keeps its own line (regression: was dropped).
      (should (string-match-p ":FOO: bar" text))
      ;; The new property lands in the heading's local drawer, with identity.
      ;; Property put is byte-identical to an in-place `org-entry-put',
      ;; whose `org-property-format' ("%-10s %s") right-pads the key column, so
      ;; the drawer line is ":K:        V", not ":K: V".  Accept Org's own
      ;; alignment whitespace; still load-bearing — value V must land under K
      ;; (drop the write and ":K:[[:space:]]+V" no longer matches).
      (should (string-match-p ":K:[[:space:]]+V" text))
      (should (string-match-p ":DORG_ID: 1001" text))
      ;; Placement: identity+K precede the body, whose :FOO: block follows it.
      (let ((idpos  (string-match ":DORG_ID: 1001" text))
            (bodypos (string-match "Body line\\." text))
            (foopos (string-match ":FOO: bar" text)))
        (should (and idpos bodypos foopos))
        (should (< idpos bodypos))
        (should (< bodypos foopos))))))

(ert-deftest dorg-test-mutate-exec-delete-property-spares-body-drawer ()
  "`:delete-property' of a non-property key does not nuke a generic body block.
Transformation regression (P0): deleting a key absent from the heading's
local drawer must be a no-op.  The old resolver latched onto the generic
body :PROPERTIES: block by name; removing the (missing) key then found the
block's `properties' empty and DELETED the whole block, destroying its raw
lines.  The block and its `:FOO: bar' line must survive untouched."
  (dorg-test-mutate--with-content buf dorg-test-mutate--body-properties-sample
    (dorg-test-mutate--apply-patch-batch
     buf (list (dorg-test-mutate--property-delete-patch 1001 "FOO")))
    (let ((text (buffer-string)))
      ;; The generic body block is spared (regression: was removed entirely).
      (should (string-match-p ":FOO: bar" text))
      ;; The heading keeps its identity.
      (should (string-match-p ":DORG_ID: 1001" text)))))

;;; Heading metadata + structure ---------------------------------------

(ert-deftest dorg-test-mutate-exec-update-heading-refuses ()
  "AC (clean-break C3): heading update has no source-preserving editor on
plain-org (whole-verb collapse, C3 slice S1) and is refused pre-write with the
exact `unsupported'/`no-source-preserving-editor' + empty variant nil, buffer
byte-identical.  Heading-title updates are retained on Dolt by
`dorg-test-mutate-dolt-commit-heading-update' /
`dorg-test-mutate-result-token-affected-dolt'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--heading-update-patch
                           1001 :title "Renamed" :todo "TODO"))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-insert-tree-allocates-id-refuses ()
  "AC (clean-break C3): tree insert has no source-preserving editor on
plain-org (whole-verb collapse, C3 slice S3) and is refused pre-write with the
exact `unsupported'/`no-source-preserving-editor' + empty variant nil, buffer
byte-identical.  Insert-with-id-allocation is retained on Dolt by
`dorg-test-mutate-dolt-insert-heading-allocates-id'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--tree-insert-patch
                           :root '(:title "Third")))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-insert-tree-before-anchor-refuses ()
  "AC (clean-break C3): an anchored tree insert has no source-preserving
editor on plain-org (whole-verb collapse, C3 slice S3) and is refused pre-write
with the exact `unsupported'/`no-source-preserving-editor' + empty variant nil,
buffer byte-identical.  Anchored insertion is retained on Dolt by
`dorg-test-mutate-dolt-delete-freed-insert-success' (inserts C `:before' B)."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--tree-insert-patch
                           :root '(:title "Between")
                           :before (dorg-test-mutate--heading-address 1002)))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-delete-subtree-refuses ()
  "AC (clean-break C3): subtree deletion has no source-preserving
editor on plain-org (whole-verb collapse, C3 slice S3) and is refused pre-write
with the exact `unsupported'/`no-source-preserving-editor' + empty variant nil,
buffer byte-identical.  Subtree deletion is retained on Dolt by
`dorg-test-mutate-dolt-delete-heading-subtree-with-child'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--delete-patch 1001 t))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-delete-heading-refuses ()
  "AC (clean-break C3): heading deletion with child promotion has no
source-preserving editor on plain-org (whole-verb collapse, C3 slice S3) and is
refused pre-write with the exact `unsupported'/`no-source-preserving-editor' +
empty variant nil, buffer byte-identical.  Child-promotion delete is retained on
Dolt by `dorg-test-mutate-dolt-delete-freed-child-promotion'."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--delete-patch 1001))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

(ert-deftest dorg-test-mutate-exec-move-subtree-refuses ()
  "AC (clean-break C3): subtree move has no source-preserving editor on
plain-org (whole-verb collapse, C3 slice S3) and is refused pre-write with the
exact `unsupported'/`no-source-preserving-editor' + empty variant nil, buffer
byte-identical.  Sibling reorder is retained on Dolt by
`dorg-test-mutate-dolt-insert-move-delete' (moves C2 `:before' C1)."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf `((:op move :target (:subtree :id 1002)
                       :to (:under :root
                            :before ,(dorg-test-mutate--heading-address 1001))))))))
      (should (eq 'unsupported (plist-get d :reason)))
      (should (eq 'no-source-preserving-editor (plist-get d :detail)))
      (should (null (plist-get d :variant)))
      (should (equal (buffer-string) before)))))

;;; Text occurrence replacement ---------------------------------------

(ert-deftest dorg-test-mutate-exec-replace-text-occurrence ()
  "A typed text replacement rewrites the addressed prose item in place."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--apply-patch-batch
     buf '((:op replace
            :target (:text :host (:heading :id 1001) :occurrence 1)
            :missing :error :value "Rewritten body.")))
    (should (string-match-p "Rewritten body\\." (buffer-string)))
    (should-not (string-match-p "First body\\." (buffer-string)))))

;;; No-partial-write rollback ------------------------------------------

(ert-deftest dorg-test-mutate-exec-rollback-on-bad-later-op ()
  "A mid-batch invalid target aborts the WHOLE apply with no buffer edit.  The
SUPPORTED first patch (property put on the existing DORG_ID heading, resolved
`dorg' policy) must never partially apply when a later op targets a missing
heading: exact (invalid-target . missing), buffer byte-identical, CATEGORY
absent."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((before (buffer-string))
           ;; First patch is valid; the second targets a missing heading.
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--property-put-patch
                           1001 "CATEGORY" "work")
                          (dorg-test-mutate--property-put-patch
                           999999 "K" "V"))))))
      (should (eq 'invalid-target (plist-get d :reason)))
      (should (eq 'missing (plist-get d :detail)))
      ;; the valid first patch never partially applied
      (should (equal (buffer-string) before))
      (should-not (string-match-p ":CATEGORY:" (buffer-string))))))

;;; Save semantics ------------------------------------------------------

(ert-deftest dorg-test-mutate-exec-buffer-no-save-by-default ()
  "Editing a file-visiting buffer does not save unless `:save' is set.  Uses the
supported property-put cell on the DORG_ID heading (identity `dorg')."
  (let* ((tmp (make-temp-file "dorg-mutate-save" nil ".org"
                              dorg-test-mutate--sample))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-test-mutate--assert-dorg-policy buf)
          (let ((on-disk-before
                 (with-temp-buffer (insert-file-contents tmp) (buffer-string))))
            (dorg-test-mutate--apply-patch-batch
             buf (list (dorg-test-mutate--property-put-patch
                        1001 "CATEGORY" "work")))
            ;; Buffer edited (before != after), disk untouched.
            (should (string-match-p ":CATEGORY: +work" (buffer-string)))
            (should-not (equal (buffer-string) on-disk-before))
            (should (buffer-modified-p))
            (should (equal (with-temp-buffer (insert-file-contents tmp)
                                             (buffer-string))
                           on-disk-before))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-mutate-exec-buffer-save-when-requested ()
  "With `:save t' a file-visiting buffer is written through to disk.  Uses the
supported property-put cell on the DORG_ID heading (identity `dorg')."
  (let* ((tmp (make-temp-file "dorg-mutate-save" nil ".org"
                              dorg-test-mutate--sample))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-test-mutate--assert-dorg-policy buf)
          (let ((on-disk-before
                 (with-temp-buffer (insert-file-contents tmp) (buffer-string)))
                (result (dorg-test-mutate--apply-patch-batch
                         buf (list (dorg-test-mutate--property-put-patch
                                    1001 "CATEGORY" "work"))
                         :save t)))
            (should (plist-get result :saved))
            (should-not (buffer-modified-p))
            (let ((on-disk-after (with-temp-buffer (insert-file-contents tmp)
                                                   (buffer-string))))
              (should (string-match-p ":CATEGORY: +work" on-disk-after))
              (should-not (equal on-disk-after on-disk-before)))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

;;; Explicit file source (no live buffer) ------------------------------

(ert-deftest dorg-test-mutate-exec-file-source-writes-directly ()
  "An explicit file source with no live buffer is written on apply.  Uses the
supported property-put cell on the DORG_ID heading (identity `dorg')."
  (let ((tmp (make-temp-file "dorg-mutate-file" nil ".org"
                             dorg-test-mutate--sample)))
    (unwind-protect
        (let ((on-disk-before (with-temp-buffer (insert-file-contents tmp)
                                                (buffer-string))))
          ;; No buffer visits TMP, so this is the pure file path.
          (should-not (find-buffer-visiting tmp))
          (dorg-test-mutate--assert-dorg-policy `(:file ,tmp))
          (let ((result (dorg-test-mutate--apply-patch-batch
                         `(:file ,tmp)
                         (list (dorg-test-mutate--property-put-patch
                                1001 "CATEGORY" "work")))))
            (should (plist-get result :saved))
            (let ((on-disk-after (with-temp-buffer (insert-file-contents tmp)
                                                   (buffer-string))))
              (should (string-match-p ":CATEGORY: +work" on-disk-after))
              (should-not (equal on-disk-after on-disk-before)))))
      (delete-file tmp))))

;;; Duplicate DORG_ID guard --------------------------------------------

(defconst dorg-test-mutate--duplicate-dorg-id-sample
  "#+TITLE: Project

* First
:PROPERTIES:
:DORG_ID: 1001
:END:
First body.

* Second
:PROPERTIES:
:DORG_ID: 1001
:END:
Second body.
"
  "Plain-org source whose two headings share `:DORG_ID: 1001'.")

(ert-deftest dorg-test-mutate-plain-org-duplicate-dorg-id-fails-closed ()
  "Two headings sharing a `:DORG_ID:' fail closed on BOTH plain-org write paths.
Before the shared uniqueness guard the surgical resolver reports only an opaque
`invalid-target'/`ambiguous', and the dormant legacy id-keyed hash would
silently overwrite the first heading with the second (dropping its content on
re-render).  The guard must instead raise `invariant', detail
`duplicate-heading-id', name the offending id, and leave the source
byte-for-byte unchanged — verified on the live-buffer AND file-backed paths."
  ;; Live-buffer path.
  (dorg-test-mutate--with-content buf dorg-test-mutate--duplicate-dorg-id-sample
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--property-put-patch
                           1001 "CATEGORY" "x"))))))
      (should (eq 'invariant (plist-get d :reason)))
      (should (eq 'duplicate-heading-id (plist-get d :detail)))
      (should (eql 1001 (plist-get d :id)))
      ;; The diagnostic names BOTH colliding headings, not just the id.
      (should (equal "First" (plist-get d :first-title)))
      (should (equal "Second" (plist-get d :second-title)))
      ;; Fail-closed: no partial write, no byte loss.
      (should (equal (buffer-string) before))))
  ;; File-backed path (no live buffer).
  (let ((tmp (make-temp-file "dorg-mutate-dupid" nil ".org"
                             dorg-test-mutate--duplicate-dorg-id-sample)))
    (unwind-protect
        (let ((on-disk-before (with-temp-buffer (insert-file-contents tmp)
                                                (buffer-string))))
          (should-not (find-buffer-visiting tmp))
          (dorg-test-mutate--assert-dorg-policy `(:file ,tmp))
          (let ((d (dorg-test-mutate--error-data
                    (dorg-test-mutate--apply-patch-batch
                     `(:file ,tmp)
                     (list (dorg-test-mutate--property-put-patch
                            1001 "CATEGORY" "x"))))))
            (should (eq 'invariant (plist-get d :reason)))
            (should (eq 'duplicate-heading-id (plist-get d :detail)))
            (should (eql 1001 (plist-get d :id))))
          ;; Fail-closed: the on-disk bytes are untouched.
          (should (equal (with-temp-buffer (insert-file-contents tmp)
                                           (buffer-string))
                         on-disk-before)))
      (delete-file tmp))))

(defconst dorg-test-mutate--narrowed-hidden-duplicate-sample
  "#+TITLE: Project

* Target
:PROPERTIES:
:DORG_ID: 1001
:END:
Target body.

* HiddenA
:PROPERTIES:
:DORG_ID: 2002
:END:
Hidden A body.

* HiddenB
:PROPERTIES:
:DORG_ID: 2002
:END:
Hidden B body.
"
  "Source whose accessible heading (id 1001) is UNIQUE, with an UNRELATED
duplicate id 2002 across two headings a narrowing can hide.  Lets a narrowed
test drive an unambiguous op on the accessible target: without a widening guard
the mutation genuinely succeeds and writes, so the test pins the guard rather
than the surgical resolver's ambiguity refusal.")

(ert-deftest dorg-test-mutate-plain-org-duplicate-dorg-id-narrowed-buffer-fails-closed ()
  "A duplicate `:DORG_ID:' HIDDEN outside a narrowed live buffer still fails closed.
The op targets the UNIQUE accessible id 1001, so without a widening guard the
surgical executor (which widens) resolves it unambiguously and WRITES — the
duplicate 2002 lives only in the hidden region.  The guard must widen, see the
hidden 2002 collision, raise `invariant'/`duplicate-heading-id' naming 2002,
leave the full buffer byte-for-byte unchanged (no write), AND restore the
narrowing it widened past (a permanent widen must not survive)."
  (dorg-test-mutate--with-content buf dorg-test-mutate--narrowed-hidden-duplicate-sample
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((full-before (save-restriction (widen) (buffer-string))))
      ;; Narrow to everything BEFORE the hidden headings; only id 1001 is visible.
      (goto-char (point-min))
      (re-search-forward "^\\* HiddenA")
      (narrow-to-region (point-min) (match-beginning 0))
      ;; Sanity: the accessible region carries the unique target and hides 2002.
      (should (string-match-p ":DORG_ID: 1001" (buffer-string)))
      (should-not (string-match-p "2002" (buffer-string)))
      (should-not (string-match-p "Hidden" (buffer-string)))
      (let ((bounds (cons (point-min) (point-max))))
        ;; The op addresses the UNIQUE accessible id — it would succeed if the
        ;; guard failed to widen; the HIDDEN duplicate is what must stop it.
        (let ((d (dorg-test-mutate--error-data
                  (dorg-test-mutate--apply-patch-batch
                   buf (list (dorg-test-mutate--property-put-patch
                              1001 "CATEGORY" "x"))))))
          (should (eq 'invariant (plist-get d :reason)))
          (should (eq 'duplicate-heading-id (plist-get d :detail)))
          (should (eql 2002 (plist-get d :id))))
        ;; Restoration: the guard widened under `save-restriction', so the
        ;; caller's narrowing is intact (a permanent widen fails here).
        (should (equal bounds (cons (point-min) (point-max)))))
      ;; No write: the full buffer — every heading — is byte-for-byte unchanged.
      (should (equal (save-restriction (widen) (buffer-string)) full-before)))))

(ert-deftest dorg-test-mutate-source-assert-unique-ids-widens-past-narrowing ()
  "`dorg-mutate--source-assert-unique-ids' widens before parsing a live buffer,
then restores.  This is the surviving pre-write duplicate-id guard (the surgical
executor runs it before any mutation): a hidden duplicate id 2002 outside a
narrowed restriction must still raise `invariant'/`duplicate-heading-id', and the
caller's narrowing must survive the widen."
  (dorg-test-mutate--with-content buf dorg-test-mutate--narrowed-hidden-duplicate-sample
    (goto-char (point-min))
    (re-search-forward "^\\* HiddenA")
    (narrow-to-region (point-min) (match-beginning 0))
    (should-not (string-match-p "2002" (buffer-string)))
    (let ((bounds (cons (point-min) (point-max)))
          (src (dorg-mutate--resolve-source buf)))
      (let ((d (dorg-test-mutate--error-data (dorg-mutate--source-assert-unique-ids src))))
        (should (eq 'invariant (plist-get d :reason)))
        (should (eq 'duplicate-heading-id (plist-get d :detail)))
        (should (eql 2002 (plist-get d :id))))
      ;; Restoration: the guard widened under `save-restriction'.
      (should (equal bounds (cons (point-min) (point-max)))))))

(ert-deftest dorg-test-mutate-exec-file-source-stale-rejected ()
  "A stale file `:base-revision' (mtime) is rejected before writing.  Uses the
supported property-put cell on the DORG_ID heading (identity `dorg'); the
mtime CAS check is unchanged."
  (let ((tmp (make-temp-file "dorg-mutate-file" nil ".org"
                             dorg-test-mutate--sample)))
    (unwind-protect
        (let* ((patch (dorg-test-mutate--property-put-patch
                       1001 "CATEGORY" "work"))
               (rev (progn
                      (dorg-test-mutate--assert-dorg-policy `(:file ,tmp))
                      (plist-get (dorg-test-mutate--plan-patch-batch
                                  `(:file ,tmp) (list patch))
                                 :base-revision)))
              (before (with-temp-buffer (insert-file-contents tmp)
                                        (buffer-string))))
          (should (eq (plist-get rev :kind) 'file))
          ;; Advance the file mtime past the captured token.
          (set-file-times tmp (time-add (current-time) 5))
          (should (eq 'stale
                      (dorg-test-mutate--reason
                       (dorg-test-mutate--apply-patch-batch
                        `(:file ,tmp) (list patch) :base-revision rev))))
          (should (equal (with-temp-buffer (insert-file-contents tmp)
                                           (buffer-string))
                         before))
          ;; the CAS-rejected property was never written to disk
          (should-not (string-match-p ":CATEGORY:"
                                      (with-temp-buffer (insert-file-contents tmp)
                                                        (buffer-string)))))
      (delete-file tmp))))

(ert-deftest dorg-test-mutate-exec-cross-source-token-rejected-before-edit ()
  "A token captured from one plain-org file is stale against another file.
Both files share an mtime so only the source path differs: an end-to-end
apply of file A's token against file B must reject as `stale' before any
edit, leaving B byte-for-byte unchanged."
  (let ((tmp-a (make-temp-file "dorg-mutate-xsrc-a" nil ".org"
                               dorg-test-mutate--sample))
        (tmp-b (make-temp-file "dorg-mutate-xsrc-b" nil ".org"
                               dorg-test-mutate--sample)))
    (unwind-protect
        (progn
          ;; Pin both files to the same mtime so the only difference the
          ;; stale check can see is the source identity (path).
          (set-file-times tmp-a '(25000 0))
          (set-file-times tmp-b '(25000 0))
          (dorg-test-mutate--assert-dorg-policy `(:file ,tmp-a))
          (dorg-test-mutate--assert-dorg-policy `(:file ,tmp-b))
          (let* ((patch (dorg-test-mutate--property-put-patch
                         1001 "CATEGORY" "work"))
                 (rev-a (plist-get
                         (dorg-test-mutate--plan-patch-batch
                          `(:file ,tmp-a) (list patch))
                         :base-revision))
                (before-b (with-temp-buffer (insert-file-contents tmp-b)
                                            (buffer-string))))
            (should (equal (plist-get rev-a :file) tmp-a))
            (should (eq 'stale
                        (dorg-test-mutate--reason
                         (dorg-test-mutate--apply-patch-batch
                          `(:file ,tmp-b) (list patch)
                          :base-revision rev-a))))
            (should (equal (with-temp-buffer (insert-file-contents tmp-b)
                                             (buffer-string))
                           before-b))
            ;; the cross-source-stale-rejected property was never written to B
            (should-not (string-match-p ":CATEGORY:"
                                        (with-temp-buffer (insert-file-contents tmp-b)
                                                          (buffer-string))))))
      (delete-file tmp-a)
      (delete-file tmp-b))))

;;; Engine-private cross-source move: plain-org unsupported ------------

(ert-deftest dorg-test-mutate-exec-plain-org-cross-source-move-plan-unsupported ()
  "A private plain-org cross-source move plans unsupported.
The contract gate clears once `:allow-nonatomic' is supplied, but under the
clean-break C3 contract plain-org has no source-preserving editor for
subtree moves, so the plan reports the patch unsupported with the exact
`:reason no-source-preserving-editor' (not `contract')."
  (let* ((dest (dorg-mutate--resolve-source
                '(:file "/tmp/dorg-xmove-b.org")))
         (patch (dorg-test-mutate--move-patch
                 1001 (dorg-test-mutate--heading-address 1002)
                 :allow-nonatomic t :to-source dest))
         (plan (dorg-test-mutate--plan-patch-batch
                '(:file "/tmp/dorg-xmove-a.org") (list patch)))
         (unsupported (plist-get plan :unsupported)))
    (should (plist-get plan :ok))
    (should-not (plist-get plan :supported))
    (should (= (length unsupported) 1))
    (should (eq (plist-get (car unsupported) :op) 'move))
    (should (eq (plist-get (car unsupported) :reason) 'no-source-preserving-editor))))

(ert-deftest dorg-test-mutate-exec-plain-org-cross-source-move-apply-signals ()
  "Applying a private plain-org cross-source move signals.
The typed engine raises a structured `dorg-mutate-error' (reason
`unsupported', detail `no-source-preserving-editor') BEFORE loading or writing
either source, so the source file is left untouched."
  (let ((tmp-a (make-temp-file "dorg-mutate-xmove-a" nil ".org"
                               dorg-test-mutate--sample))
        (tmp-b (make-temp-file "dorg-mutate-xmove-b" nil ".org"
                               dorg-test-mutate--sample)))
    (unwind-protect
        (let ((before-a (with-temp-buffer (insert-file-contents tmp-a)
                                          (buffer-string)))
              (before-b (with-temp-buffer (insert-file-contents tmp-b)
                                          (buffer-string))))
          (condition-case err
              (progn
                (let ((patch (dorg-test-mutate--move-patch
                              1001 (dorg-test-mutate--heading-address 1002)
                              :allow-nonatomic t
                              :to-source (dorg-mutate--resolve-source
                                          `(:file ,tmp-b)))))
                  (dorg-test-mutate--apply-patch-batch
                   `(:file ,tmp-a) (list patch)))
                (ert-fail "expected dorg-mutate-error, none signalled"))
            (dorg-mutate-error
             (should (eq (dorg-mutate-error-reason err) 'unsupported))
             (let ((data (dorg-mutate-error-data err)))
               (should (eq (plist-get data :operation) 'move))
               (should (eq (plist-get data :detail) 'no-source-preserving-editor)))))
          ;; Signalled before any load/write: BOTH source files are unchanged.
          (should (equal (with-temp-buffer (insert-file-contents tmp-a)
                                           (buffer-string))
                         before-a))
          (should (equal (with-temp-buffer (insert-file-contents tmp-b)
                                           (buffer-string))
                         before-b)))
      (delete-file tmp-a)
      (delete-file tmp-b))))

;;; ─── Plain-org typed-batch invariants ─────────────────────────────────

(ert-deftest dorg-test-mutate-empty-typed-batch-is-byte-no-op ()
  "An empty typed batch succeeds and leaves plain-org bytes unchanged."
  (dorg-test-mutate--with-buffer buf
    (let ((before (buffer-string)))
      (should (plist-get (dorg-test-mutate--apply-patch-batch buf nil) :ok))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-mutate-property-put-preserves-identity-and-order ()
  "A property put preserves every heading's DORG_ID and sibling order."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--apply-patch-batch
     buf (list (dorg-test-mutate--property-put-patch 1002 "CATEGORY" "x")))
    (should (string-match-p ":DORG_ID: 1001" (buffer-string)))
    (should (string-match-p ":DORG_ID: 1002" (buffer-string)))
    (should (equal '("First" "Second")
                   (dorg-test-mutate--heading-titles buf)))))

;;; ─── Dolt Executor Integration (gated on a live Dolt repo) ────────────

;; These exercise the Dolt planner/executor against a real
;; temporary Dolt repository.  Each seeds a root + headings, captures the
;; base commit, and verifies the read path via `dorg-model-load-tree' and
;; the commit advance via `dorg-head-commit'.

(defmacro dorg-test-mutate--with-dolt (&rest body)
  "Run BODY with `dolt-dir' bound to a fresh temp Dolt repo, or skip.
Mirrors `dorg-test-with-dolt' without a cross-file macro dependency."
  (declare (indent 0))
  `(if (not (bound-and-true-p dorg-test--dolt-available))
       (ert-skip "dolt not installed or unsupported")
     (dorg-test--with-temp-db dolt-dir ,@body)))

(defun dorg-test-mutate--seed (dolt-dir contents &optional root-id)
  "Seed CONTENTS (org text) into DOLT-DIR under ROOT-ID and commit.
Returns ROOT-ID (default 100)."
  (let* ((rid (or root-id 100))
         (stmts (dorg-test--parse-to-sql contents rid)))
    (dorg-sql-batch dolt-dir stmts)
    (dorg-commit dolt-dir "seed")
    rid))

(defun dorg-test-mutate--id-by-title (dolt-dir title)
  "Return the heading id whose title is TITLE in DOLT-DIR, or nil."
  (alist-get 'id (car (dorg-sql dolt-dir
                        (format "SELECT id FROM headings WHERE title = %s LIMIT 1"
                                (dorg-sql-quote title))))))

(defun dorg-test-mutate--title (tree id)
  "Return the title of heading ID in loaded TREE hash."
  (let ((h (gethash id tree))) (and h (dorg-heading-title h))))

(defun dorg-test-mutate--child-titles (headings parent-id)
  "Return direct child titles of PARENT-ID from HEADINGS in sibling order."
  (mapcar #'dorg-heading-title
          (sort
           (cl-loop for heading being the hash-values of headings
                    when (equal parent-id (dorg-heading-parent-id heading))
                    collect heading)
           (lambda (a b)
             (string< (dorg-heading-sort-key a)
                      (dorg-heading-sort-key b))))))

(defun dorg-test-mutate--dolt-keyword-values (tree id key)
  "Return the ordered list of KEY keyword-item values for heading ID in TREE."
  (let ((h (gethash id tree)) (out nil))
    (dolist (it (and h (dorg-heading-items h)) (nreverse out))
      (when (and (cl-typep it 'dorg-keyword-item)
                 (equal (dorg-keyword-item-keyword it) key))
        (push (dorg-keyword-item-value it) out)))))

(ert-deftest dorg-test-mutate-public-keyword-insert-backend-parity ()
  "Public keyword insertion has one heading meaning on plain-org and Dolt.
Dolt additionally executes the root-host cell that C3 intentionally refuses
on plain-org.  All three transformations are non-vacuous."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir dorg-test-mutate--keyword-insert-sample))
           (existing (dorg-test-mutate--id-by-title dolt-dir "Existing"))
           (missing (dorg-test-mutate--id-by-title dolt-dir "Missing"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (tree-before (dorg-model-load-tree dolt-dir root))
           (root-before (dorg-test-mutate--items-shape
                         (dorg-heading-items (gethash root tree-before))))
           (existing-before (dorg-test-mutate--items-shape
                             (dorg-heading-items
                              (gethash existing tree-before))))
           (missing-before (dorg-test-mutate--items-shape
                            (dorg-heading-items
                             (gethash missing tree-before))))
           (head-before (dorg-head-commit dolt-dir))
           (res (dorg-api-execute
                 `(batch
                   (on (id ,existing)
                       (insert (keyword "K") (after-last) "c"))
                   (on (id ,missing)
                       (insert (keyword "K") (after-last) "c"))
                   (on document
                       (insert (keyword "K") (after-last) "root-c")))
                 :mode 'apply :source source))
           (tree-after (dorg-model-load-tree dolt-dir root))
           (root-after (dorg-test-mutate--items-shape
                        (dorg-heading-items (gethash root tree-after))))
           (existing-after (dorg-test-mutate--items-shape
                            (dorg-heading-items
                             (gethash existing tree-after))))
           (missing-after (dorg-test-mutate--items-shape
                           (dorg-heading-items
                            (gethash missing tree-after))))
           plain-existing plain-missing)
      (should (plist-get res :ok))
      (should-not (equal head-before (dorg-head-commit dolt-dir)))
      (should-not (equal root-before root-after))
      (should-not (equal existing-before existing-after))
      (should-not (equal missing-before missing-after))
      (should
       (equal '((keyword "K" "root-a")
                (keyword "OTHER" "root-middle")
                (keyword "K" "root-b")
                (keyword "K" "root-c")
                (text "Root tail."))
              root-after))
      ;; Run the two supported heading cells through plain-org and compare the
      ;; entire item projections, not merely the same-key values.
      (dorg-test-mutate--with-content
          buf dorg-test-mutate--keyword-insert-sample
        (dorg-api-execute
         '(batch
           (on (id 1001) (insert (keyword "K") (after-last) "c"))
           (on (id 1002) (insert (keyword "K") (after-last) "c")))
         :mode 'apply :source buf)
        (setq plain-existing
              (dorg-test-mutate--items-shape
               (dorg-heading-items
                (dorg-test-mutate--parsed-heading buf 1001))))
        (setq plain-missing
              (dorg-test-mutate--items-shape
               (dorg-heading-items
                (dorg-test-mutate--parsed-heading buf 1002)))))
      (should (equal plain-existing existing-after))
      (should (equal plain-missing missing-after))
      (should (equal '((keyword "K" "a")
                       (keyword "OTHER" "middle")
                       (keyword "K" "b")
                       (keyword "K" "c")
                       (text "Tail."))
                     existing-after))
      (should (equal '((keyword "OTHER" "middle")
                       (text "Tail.")
                       (keyword "K" "c"))
                     missing-after)))))

(ert-deftest dorg-test-mutate-dolt-commit-heading-update ()
  "A typed heading update persists a new title/todo and advances HEAD.
Transformation test: the loaded title differs before and after."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Old\n"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "Old"))
           (base (dorg-head-commit dolt-dir))
           (before (dorg-test-mutate--title
                    (dorg-model-load-tree dolt-dir root) cid))
           (res (dorg-test-mutate--apply-patch-batch
                 (list :dolt-dir dolt-dir :root-id root)
                 (list (dorg-test-mutate--heading-update-patch
                        cid :title "New" :todo "TODO")))))
      (should (plist-get res :ok))
      (should (member cid (plist-get res :changed)))
      (should-not (equal base (dorg-head-commit dolt-dir)))
      (let* ((tree (dorg-model-load-tree dolt-dir root))
             (after (dorg-test-mutate--title tree cid)))
        (should (equal before "Old"))
        (should (equal after "New"))
        (should-not (equal before after))
        (should (equal (dorg-heading-todo (gethash cid tree)) "TODO"))))))

(ert-deftest dorg-test-mutate-dolt-resolved-struct-plan-apply ()
  "An ALREADY-RESOLVED Dolt `dorg-mutate-source' struct (bypassing descriptor/
registration construction, as D4 idempotent threading / B5 synthesized bindings
produce) plans AND applies correctly through the kind=dolt dispatch: the update
persists and HEAD advances (B8 lazy-load boundary is at dispatch, not construction)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Old\n"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "Old"))
           (struct (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id root)))
           (base (dorg-head-commit dolt-dir)))
      (should (dorg-mutate-source-p struct))
      (let ((patch (dorg-test-mutate--heading-update-patch cid :title "New")))
        (should (plist-get (dorg-test-mutate--plan-patch-batch
                            struct (list patch)) :ok))
        (let ((res (dorg-test-mutate--apply-patch-batch struct (list patch))))
        (should (plist-get res :ok))
          (should (member cid (plist-get res :changed)))))
      (should-not (equal base (dorg-head-commit dolt-dir)))
      (should (equal "New" (dorg-test-mutate--title
                            (dorg-model-load-tree dolt-dir root) cid))))))

(ert-deftest dorg-test-mutate-dolt-insert-move-delete ()
  "Insert two children, reorder them, and delete a leaf in one transaction."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* L\n"))
           (lid (dorg-test-mutate--id-by-title dolt-dir "L"))
           (res (dorg-test-mutate--apply-patch-batch
                 (list :dolt-dir dolt-dir :root-id root)
                 (list (dorg-test-mutate--tree-insert-patch
                        :root '(:id 200 :title "C1"))
                       (dorg-test-mutate--tree-insert-patch
                        :root '(:id 201 :title "C2"))
                       `(:op move :target (:subtree :id 201)
                         :to (:under :root
                              :before ,(dorg-test-mutate--heading-address 200)))
                       (dorg-test-mutate--delete-patch lid)))))
      (should (plist-get res :ok))
      (should (member 200 (plist-get res :created)))
      (should (member 201 (plist-get res :created)))
      (should (member lid (plist-get res :deleted)))
      (let ((tree (dorg-model-load-tree dolt-dir root)))
        (should-not (gethash lid tree))
        (should (gethash 200 tree))
        (should (gethash 201 tree))
        ;; C2 (201) was moved before C1 (200).
        (should (string< (dorg-heading-sort-key (gethash 201 tree))
                         (dorg-heading-sort-key (gethash 200 tree))))))))

(ert-deftest dorg-test-mutate-dolt-property-and-planning ()
  "Property put and planning update persist; all-nil planning removes it."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--property-put-patch hid "K" "V")))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--planning-patch
                  hid :scheduled "<2026-05-29 Fri>")))
      (let* ((tree (dorg-model-load-tree dolt-dir root))
             (items (dorg-heading-items (gethash hid tree)))
             (drawer (cl-find-if (lambda (it)
                                   (and (cl-typep it 'dorg-drawer-item)
                                        (equal (dorg-drawer-item-name it) "PROPERTIES")))
                                 items))
             (planning (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item)) items)))
        (should drawer)
        (should (equal (cdr (assoc "K" (dorg-drawer-item-properties drawer))) "V"))
        (should planning)
        (should (equal (dorg-planning-item-scheduled planning) "<2026-05-29 Fri>")))
      ;; All-nil planning removes the planning line.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--planning-patch hid)))
      (let* ((tree (dorg-model-load-tree dolt-dir root))
             (planning (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item))
                                   (dorg-heading-items (gethash hid tree)))))
        (should-not planning)))))

(ert-deftest dorg-test-mutate-dolt-put-property-preserves-body-drawer ()
  "Dolt property put spares a generic body :PROPERTIES: block end-to-end.
Transformation regression (P0) exercising the loader (item #1) and mutator
(item #3) together: a heading seeded with a generic body :PROPERTIES: block
stores it as `drawer_lines'.  After property put, a reload must show BOTH
the untouched body block (still raw lines with `:FOO: bar') AND a new local
property drawer carrying K=V — never the body block rewritten to K=V with
its lines lost."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* H\nbody\n:PROPERTIES:\n:FOO: bar\n:END:\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      ;; Control: the seeded body block is stored as raw lines, not properties.
      (let* ((items0 (dorg-heading-items (gethash hid (dorg-model-load-tree
                                                       dolt-dir root))))
             (body0 (cl-find-if (lambda (it) (cl-typep it 'dorg-drawer-item))
                                items0)))
        (should body0)
        (should (member ":FOO: bar" (dorg-drawer-item-lines body0)))
        (should (null (dorg-drawer-item-properties body0))))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--property-put-patch hid "K" "V")))
      (let* ((items (dorg-heading-items (gethash hid (dorg-model-load-tree
                                                      dolt-dir root))))
             (drawers (cl-remove-if-not
                       (lambda (it) (cl-typep it 'dorg-drawer-item)) items))
             (local (cl-find-if #'dorg-drawer-item-properties drawers))
             (body (cl-find-if #'dorg-drawer-item-lines drawers)))
        ;; The generic body block survived as raw lines with its own content.
        (should body)
        (should (member ":FOO: bar" (dorg-drawer-item-lines body)))
        (should (null (dorg-drawer-item-properties body)))
        ;; A distinct local property drawer now carries K=V.
        (should local)
        (should (not (eq local body)))
        (should (equal (cdr (assoc "K" (dorg-drawer-item-properties local)))
                       "V"))
        ;; And it is free of the body block's raw line (they are not merged).
        (should (null (dorg-drawer-item-lines local)))))))

(ert-deftest dorg-test-mutate-dolt-item-replacement ()
  "Typed keyword occurrence and whole-items replacement persist in order."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n#+FOO: a\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-occurrence-patch
                  'replace (dorg-test-mutate--heading-address hid)
                  "FOO" 1 :value "b" :missing :error)))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) hid "FOO")
                     '("b")))
      (dorg-test-mutate--apply-patch-batch
       src `((:op replace :target (:items :host (:heading :id ,hid))
               :items ((:kind keyword :keyword "BAR" :value "1")
                       (:kind keyword :keyword "BAR" :value "2")))))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) hid "BAR")
                     '("1" "2"))))))

(ert-deftest dorg-test-mutate-dolt-root-keyword-repeated ()
  "Repeated root keywords replace/insert/delete by occurrence, preserving order."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "#+OPTIONS: x\n#+OPTIONS: y\n* H\n"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) root "OPTIONS")
                     '("x" "y")))
      ;; Replace only the second occurrence.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-occurrence-patch
                  'replace :root "OPTIONS" 2 :value "y2" :missing :error)))
      ;; Insert a third after the last same-key occurrence.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-patch
                  'insert :root "OPTIONS" :position :after-last :value "z3")))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) root "OPTIONS")
                     '("x" "y2" "z3")))
      ;; Delete only the first occurrence.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-occurrence-patch
                  'delete :root "OPTIONS" 1 :missing :error)))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) root "OPTIONS")
                     '("y2" "z3"))))))

(ert-deftest dorg-test-mutate-dolt-root-insert-keyword ()
  "Dolt retains root keyword insertion.  A root
typed keyword insert of a non-DORG keyword through the engine
adds the preamble keyword: the reloaded tree shows the exact root keyword
value AND preserves the pre-existing keyword's order, and HEAD advances (the
source is persisted).  Wrong initial state -> corrected state (before != after)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "#+OPTIONS: x\n* H\n"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (before (dorg-head-commit dolt-dir)))
      ;; precondition (wrong/initial state): no AUTHOR keyword yet
      (should (null (dorg-test-mutate--dolt-keyword-values
                     (dorg-model-load-tree dolt-dir root) root "AUTHOR")))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-patch
                  'insert :root "AUTHOR" :position :after-last :value "OC")))
      ;; reloaded tree: exact root keyword value, pre-existing OPTIONS order intact
      (let ((tree (dorg-model-load-tree dolt-dir root)))
        (should (equal (dorg-test-mutate--dolt-keyword-values tree root "AUTHOR") '("OC")))
        (should (equal (dorg-test-mutate--dolt-keyword-values tree root "OPTIONS") '("x"))))
      ;; commit advanced: the source was persisted
      (should-not (equal (dorg-head-commit dolt-dir) before)))))

(ert-deftest dorg-test-mutate-dolt-insert-tree-allocates-id ()
  "Dolt tree insertion with no explicit :id allocates and persists a fresh DORG_ID
\(the load-bearing counterpart for the retired plain-org
`dorg-test-mutate-exec-insert-heading-allocates-id').  Wrong initial state (no
Third) -> corrected: `:created' reports a fresh integer id, the reloaded tree
contains that id titled Third, and HEAD advances (source persisted)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* First\n* Second\n"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (before (dorg-head-commit dolt-dir))
           (fid (dorg-test-mutate--id-by-title dolt-dir "First"))
           (sid (dorg-test-mutate--id-by-title dolt-dir "Second"))
           ;; complete pre-apply id set (proves the fresh id is genuinely new)
           (pre-ids (let (ids)
                      (maphash (lambda (id _h) (push id ids))
                               (dorg-model-load-tree dolt-dir root))
                      ids)))
      ;; precondition (wrong/initial state): no Third; First/Second present
      (should-not (dorg-test-mutate--id-by-title dolt-dir "Third"))
      (should (member fid pre-ids))
      (should (member sid pre-ids))
      (let* ((res (dorg-test-mutate--apply-patch-batch
                   src (list (dorg-test-mutate--tree-insert-patch
                              :root '(:title "Third")))))
             (new-id (car (plist-get res :created))))
        (should (plist-get res :ok))
        (should (= 1 (length (plist-get res :created))))
        (should (integerp new-id))
        ;; NON-VACUITY: the allocated id was ABSENT from the pre-apply tree
        (should-not (member new-id pre-ids))
        ;; reloaded: Third persisted at new-id, AND First/Second unchanged
        ;; (their original ids still map to their original titles)
        (let ((tree (dorg-model-load-tree dolt-dir root)))
          (should (equal "Third" (dorg-heading-title (gethash new-id tree))))
          (should (equal "First" (dorg-heading-title (gethash fid tree))))
          (should (equal "Second" (dorg-heading-title (gethash sid tree)))))
        ;; HEAD advanced: the allocation was committed
        (should-not (equal (dorg-head-commit dolt-dir) before))))))

(ert-deftest dorg-test-mutate-dolt-delete-subtree-with-child ()
  "Dolt subtree deletion removes a heading and its child (the
load-bearing counterpart for the retired plain-org
`dorg-test-mutate-exec-delete-heading-subtree').  Seeds a parent with a child
and an unaffected sibling; after the subtree delete the reloaded tree proves the
parent AND child ABSENT, the sibling PRESENT, and HEAD advanced."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Parent\n** Child\n* Sibling\n"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (pid (dorg-test-mutate--id-by-title dolt-dir "Parent"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "Child"))
           (sid (dorg-test-mutate--id-by-title dolt-dir "Sibling"))
           (before (dorg-head-commit dolt-dir)))
      ;; precondition: parent, child, and sibling all present
      (let ((tree (dorg-model-load-tree dolt-dir root)))
        (should (gethash pid tree))
        (should (gethash cid tree))
        (should (gethash sid tree)))
      (let ((res (dorg-test-mutate--apply-patch-batch
                  src (list (dorg-test-mutate--delete-patch pid t)))))
        (should (plist-get res :ok))
        (should (member pid (plist-get res :deleted))))
      ;; reloaded: parent AND child gone, sibling untouched
      (let ((tree (dorg-model-load-tree dolt-dir root)))
        (should-not (gethash pid tree))
        (should-not (gethash cid tree))
        (should (gethash sid tree)))
      ;; HEAD advanced: the subtree delete was committed
      (should-not (equal (dorg-head-commit dolt-dir) before)))))

(ert-deftest dorg-test-mutate-dolt-dorg-protection ()
  "A DORG_* root keyword is rejected; a benign edit writes no DORG_* rows."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "#+OPTIONS: x\n* H\n"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src (list (dorg-test-mutate--keyword-patch
                               'insert :root "DORG_ID"
                               :position :after-last :value "9"))))))
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--keyword-occurrence-patch
                  'replace :root "OPTIONS" 1 :value "x2" :missing :error)))
      ;; No DORG_* keyword content rows exist for the root.
      (let ((rows (dorg-sql dolt-dir
                    (format "SELECT COUNT(*) AS n FROM heading_items hi JOIN keyword_items ki ON ki.item_id = hi.id WHERE hi.heading_id = %d AND ki.keyword LIKE 'DORG_%%'"
                            root))))
        (should (= (alist-get 'n (car rows)) 0))))))

(ert-deftest dorg-test-mutate-dolt-base-commit-stale ()
  "A stale `:base-commit' whose intent OVERLAPS an intervening same-heading commit
is rejected terminal `overlap' (first-committer-wins, tracked-regression §2.4) and
leaves the DB unchanged.  Under the per-store linearization gate a stale base no
longer blanket-fails `stale'; a same-heading conflict is `overlap' (a disjoint
one re-plans and commits, see the linearize suite)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (base (dorg-head-commit dolt-dir)))
      ;; Intervening commit advances HEAD past BASE, changing the SAME heading.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--heading-update-patch hid :title "Mid")))
      (let ((head (dorg-head-commit dolt-dir)))
        (condition-case err
            (progn
              (dorg-test-mutate--apply-patch-batch
               src (list (dorg-test-mutate--heading-update-patch hid :title "Late"))
               :base-commit base)
              (ert-fail "expected overlap error"))
          (dorg-mutate-error
           (should (eq (dorg-mutate-error-reason err) 'overlap))
           (let ((data (dorg-mutate-error-data err)))
             (should (equal (plist-get data :base-commit) base))
             (should (equal (plist-get data :commit) head))
             (should (member hid (plist-get data :overlap))))))
        ;; The failed call did not write.
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (equal (dorg-test-mutate--title
                        (dorg-model-load-tree dolt-dir root) hid)
                       "Mid"))))))

(ert-deftest dorg-test-mutate-dolt-invariant-no-write ()
  "An in-memory invariant violation signals before any write (HEAD unchanged)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* P\n** C\n"))
           (pid (dorg-test-mutate--id-by-title dolt-dir "P"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "C"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (base (dorg-head-commit dolt-dir)))
      ;; A benign update precedes the illegal move-under-descendant.
      (should (eq 'invariant
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src (list (dorg-test-mutate--heading-update-patch
                               pid :title "x")
                              (dorg-test-mutate--move-patch
                               pid (dorg-test-mutate--heading-address cid)))))))
      (should (equal base (dorg-head-commit dolt-dir)))
      (should (equal (dorg-test-mutate--title
                      (dorg-model-load-tree dolt-dir root) pid)
                     "P")))))

(ert-deftest dorg-test-mutate-dolt-rollback-constraint ()
  "A commit-time constraint failure rolls back and wraps as invariant/constraint.
Forces a generated `heading_items.id' to collide with an existing row's
id when a keyword item is appended (item-level diffing reuses ids for
in-place edits, so the collision is forced via an inserted item): the
in-memory simulation succeeds, the DB rejects the duplicate PRIMARY KEY
inside the gated lease transaction, and the whole transaction is rolled
back (HEAD unchanged, no partial rows)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H1\n#+FOO: a\n* H2\n#+BAR: b\n"))
           (h1 (dorg-test-mutate--id-by-title dolt-dir "H1"))
           (h2 (dorg-test-mutate--id-by-title dolt-dir "H2"))
           (src (list :dolt-dir dolt-dir :root-id root))
           ;; An existing heading_items.id belonging to H2 (NOT deleted by
           ;; the batch), so inserting it under H1 violates the PK.
           (collide (alist-get 'id (car (dorg-sql dolt-dir
                      (format "SELECT id FROM heading_items WHERE heading_id = %d LIMIT 1"
                              h2)))))
           (base (dorg-head-commit dolt-dir)))
      (should collide)
      (let ((dorg-mutate--dolt-item-id-generator (lambda () collide)))
        (condition-case err
            (progn
              ;; Typed after-last appends a second FOO item (forces an INSERT,
              ;; whose generated id is the forced collision).
              (dorg-test-mutate--apply-patch-batch
               src (list (dorg-test-mutate--keyword-patch
                          'insert (dorg-test-mutate--heading-address h1) "FOO"
                          :position :after-last :value "z")))
              (ert-fail "expected constraint error"))
          (dorg-mutate-error
           (should (eq (dorg-mutate-error-reason err) 'invariant))
           (should (eq (plist-get (dorg-mutate-error-data err) :detail) 'constraint)))))
      ;; Rolled back: HEAD unchanged, H1's FOO still "a", no partial rows.
      (should (equal base (dorg-head-commit dolt-dir)))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) h1 "FOO")
                     '("a"))))))

(ert-deftest dorg-test-mutate-dolt-dry-run-no-write ()
  "`:dry-run t' returns the plan with :db-ops and leaves HEAD/model unchanged."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (base (dorg-head-commit dolt-dir))
           (plan (dorg-test-mutate--plan-patch-batch
                  src (list (dorg-test-mutate--property-put-patch hid "K" "V")))))
      (should (plist-get plan :dry-run))
      (should (plist-get plan :db-ops))
      (should (equal base (dorg-head-commit dolt-dir)))
      ;; Model unchanged: no PROPERTIES drawer written.
      (let ((items (dorg-heading-items
                    (gethash hid (dorg-model-load-tree dolt-dir root)))))
        (should-not (cl-find-if (lambda (it) (cl-typep it 'dorg-drawer-item)) items))))))

(ert-deftest dorg-test-mutate-dolt-net-create-delete-no-commit ()
  "A create-then-delete batch nets to nothing: :ok with HEAD unchanged."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (base (dorg-head-commit dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--tree-insert-patch
                            :root '(:id 700 :title "T"))
                           (dorg-test-mutate--delete-patch 700)))))
      (should (plist-get res :ok))
      (should (equal base (dorg-head-commit dolt-dir)))
      (should-not (gethash 700 (dorg-model-load-tree dolt-dir root))))))

(ert-deftest dorg-test-mutate-dolt-plan-db-ops-no-commit ()
  "A native Dolt plan returns persistence db-ops without committing."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (base (dorg-head-commit dolt-dir))
           (plan (dorg-test-mutate--plan-patch-batch
                  (list :dolt-dir dolt-dir :root-id root)
                  (list (dorg-test-mutate--heading-update-patch hid :title "X")))))
      (should (plist-get plan :supported))
      (should (equal (plist-get plan :head-commit) base))
      (should (member hid (plist-get plan :changed)))
      (should (cl-find-if (lambda (op) (eq (car op) :update-heading))
                          (plist-get plan :db-ops)))
      ;; No commit happened.
      (should (equal base (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-mutate-dolt-insert-parent-move-child ()
  "Insert a fresh parent and move an existing child under it in one commit.
Proves FK-safe phased lowering: the new parent row is inserted before the
existing child's UPDATE reparents it."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* X\n"))
           (xid (dorg-test-mutate--id-by-title dolt-dir "X"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (res (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--tree-insert-patch
                            :root '(:id 500 :title "P"))
                           (dorg-test-mutate--move-patch
                            xid (dorg-test-mutate--heading-address 500))))))
      (should (plist-get res :ok))
      (should (member 500 (plist-get res :created)))
      (should (member xid (plist-get res :changed)))
      (let ((tree (dorg-model-load-tree dolt-dir root)))
        (should (gethash 500 tree))
        (should (eql (dorg-heading-parent-id (gethash xid tree)) 500))))))

(ert-deftest dorg-test-mutate-dolt-public-move-persists-descendant-levels ()
  "A public typed move persists every shifted descendant level.
The fixture has a section-bearing descendant at depth two below the moved
heading.  Moving it below the deeper Host changes every subtree depth.  The
assertion reloads Dolt and derives expected levels from stored parent links,
so a correct simulated model cannot mask omitted descendant UPDATEs."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* Source\n"
                          "** Moved\n"
                          "*** Notes\n"
                          "Section-bearing descendant.\n"
                          "**** Leaf\n"
                          "Depth-two section body.\n"
                          "* Destination\n"
                          "** Host\n")))
           ;; `dorg-test--parse-to-sql' assigns root 100 and these successive
           ;; heading ids.  Moved begins at depth 2; Host is at depth 2.
           (moved 102)
           (descendants '(103 104))
           (source (list :dolt-dir dolt-dir :root-id root))
           (result (dorg-api-execute
                    '(on (id 102) (move (subtree) (under (id 106))))
                    :mode 'apply :source source)))
      (should (plist-get result :ok))
      (let ((reloaded (dorg-model-load-tree dolt-dir root)))
        (cl-labels ((depth (heading)
                      (if-let ((parent-id (dorg-heading-parent-id heading)))
                          (1+ (depth (gethash parent-id reloaded)))
                        0)))
          (should (= (dorg-heading-level (gethash moved reloaded))
                     (depth (gethash moved reloaded))))
          (dolist (id descendants)
            (let ((heading (gethash id reloaded)))
              (should heading)
              (should (= (dorg-heading-level heading) (depth heading)))))))
      ;; The receipt names every row that the persistence diff rewrites.
      (should (equal '(102 103 104)
                     (sort (copy-sequence
                            (plist-get (plist-get result :affected) :changed))
                           #'<))))))

(ert-deftest dorg-test-bejl7-ps-insert-after-anchor-batch-preserves-order ()
  "Repeated same-anchor typed heading inserts preserve declared batch order."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :title "Tail"
                                  :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context)))
    (dolist (patch
             (list
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 20 :title "A"))
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 21 :title "B"))))
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "A" "B" "Tail")))))

(ert-deftest dorg-test-bejl7-ps-move-after-anchor-batch-preserves-order ()
  "Repeated same-anchor typed subtree moves preserve declared batch order."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x20)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :title "Tail"
                                  :sort-key (unibyte-string #x80)))
         (holding (make-dorg-heading :id 12 :parent-id 0 :level 1
                                     :title "Holding"
                                     :sort-key (unibyte-string #xe0)))
         (a (make-dorg-heading :id 20 :parent-id 12 :level 2 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 21 :parent-id 12 :level 2 :title "B"
                               :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail holding a b))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context)))
    (dolist (patch
             (list
              '(:op move :target (:subtree :id 20)
                    :to (:under :root :after (:heading :id 10)))
              '(:op move :target (:subtree :id 21)
                    :to (:under :root :after (:heading :id 10)))))
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "A" "B" "Tail" "Holding")))))

(ert-deftest dorg-test-bejl7-ps-move-after-anchor-exhaustion-keeps-cursor-order ()
  "The move rebalance fallback uses the same effective anchor as position-key."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x20)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :title "Tail"
                                  :sort-key (unibyte-string #x80)))
         (holding (make-dorg-heading :id 12 :parent-id 0 :level 1
                                     :title "Holding"
                                     :sort-key (unibyte-string #xe0)))
         (a (make-dorg-heading :id 20 :parent-id 12 :level 2 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 21 :parent-id 12 :level 2 :title "B"
                               :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail holding a b))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context))
         (calls 0)
         (orig (symbol-function 'dorg-sortkey-midpoint-limited)))
    (cl-letf (((symbol-function 'dorg-sortkey-midpoint-limited)
               (lambda (lo hi)
                 (cl-incf calls)
                 (if (= calls 2)
                     (signal 'dorg-sortkey-exhausted nil)
                   (funcall orig lo hi)))))
      (dolist (patch
               (list
                '(:op move :target (:subtree :id 20)
                      :to (:under :root :after (:heading :id 10)))
                '(:op move :target (:subtree :id 21)
                      :to (:under :root :after (:heading :id 10)))))
        (dorg-mutate--ps-apply-patch ps patch source batch-context)))
    (should (= 2 calls))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "A" "B" "Tail" "Holding")))))

(ert-deftest dorg-test-bejl7-ps-after-cursor-deleted-falls-back-to-declared-anchor ()
  "A deleted after-cursor does not make a later same-anchor insert invalid."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :title "Tail"
                                  :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context)))
    (dolist (patch
             (list
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 20 :title "A"))
              '(:op delete :target (:heading :id 20))
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 21 :title "B"))))
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "B" "Tail")))))

(ert-deftest dorg-test-bejl7-ps-insert-before-anchor-batch-stays-declared-order ()
  "Before-anchor heading inserts stay order-correct and untouched by cursors."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x80)))
         (ps (dorg-test-mutate--ps-with 0 root x))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context)))
    (dolist (patch
             (list
              '(:op insert :target (:tree :under :root :before (:heading :id 10))
                    :heading (:id 20 :title "A"))
              '(:op insert :target (:tree :under :root :before (:heading :id 10))
                    :heading (:id 21 :title "B"))))
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("A" "B" "X")))))

(ert-deftest dorg-test-bejl7-ps-chained-mixed-after-anchor-order-is-pinned ()
  "Pin chained mixed order: A after X, B after A, C after X => X,A,C,B.
C follows the request-local after-X cursor A and therefore lands before B,
which was explicitly anchored to A on a separate after-A chain."
  (let* ((root (make-dorg-heading :id 0 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :title "Tail"
                                  :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context)))
    (dolist (patch
             (list
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 20 :title "A"))
              '(:op insert :target (:tree :under :root :after (:heading :id 20))
                    :heading (:id 21 :title "B"))
              '(:op insert :target (:tree :under :root :after (:heading :id 10))
                    :heading (:id 22 :title "C"))))
      (dorg-mutate--ps-apply-patch ps patch source batch-context))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "A" "C" "B" "Tail")))))

(ert-deftest dorg-test-bejl7-dolt-insert-after-anchor-batch-preserves-order ()
  "Dolt apply preserves declared order for repeated same-anchor inserts."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* X\n* Tail\n"))
           (xid (dorg-test-mutate--id-by-title dolt-dir "X"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (patches
            (list
             (dorg-test-mutate--tree-insert-patch
              :root '(:id 300 :title "A")
              :after (dorg-test-mutate--heading-address xid))
             (dorg-test-mutate--tree-insert-patch
              :root '(:id 301 :title "B")
              :after (dorg-test-mutate--heading-address xid)))))
      (should (equal (dorg-test-mutate--child-titles
                      (dorg-model-load-headings dolt-dir root nil) root)
                     '("X" "Tail")))
      (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
        (should (plist-get res :ok))
        (should (member 300 (plist-get res :created)))
        (should (member 301 (plist-get res :created))))
      (should (equal (dorg-test-mutate--child-titles
                      (dorg-model-load-headings dolt-dir root nil) root)
                     '("X" "A" "B" "Tail"))))))

(ert-deftest dorg-test-bejl7-dolt-move-after-anchor-batch-preserves-order ()
  "Dolt apply preserves declared order for repeated same-anchor moves."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* X\n* Tail\n* Holding\n** A\n** B\n"))
           (xid (dorg-test-mutate--id-by-title dolt-dir "X"))
           (aid (dorg-test-mutate--id-by-title dolt-dir "A"))
           (bid (dorg-test-mutate--id-by-title dolt-dir "B"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (patches
            (list
             (list :op 'move
                   :target (list :subtree :id aid)
                   :to (list :under :root
                             :after (dorg-test-mutate--heading-address xid)))
             (list :op 'move
                   :target (list :subtree :id bid)
                   :to (list :under :root
                             :after (dorg-test-mutate--heading-address xid))))))
      (should (equal (dorg-test-mutate--child-titles
                      (dorg-model-load-headings dolt-dir root nil) root)
                     '("X" "Tail" "Holding")))
      (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
        (should (plist-get res :ok))
        (should (member aid (plist-get res :changed)))
        (should (member bid (plist-get res :changed))))
      (should (equal (dorg-test-mutate--child-titles
                      (dorg-model-load-headings dolt-dir root nil) root)
                     '("X" "A" "B" "Tail" "Holding"))))))

;; The staged-slot scheduler in
;; `dorg-mutate--dolt-assemble-sql' replaces the old fail-closed
;; delete-freed guard.  A same-batch delete that frees a sibling
;; `(parent_id, sort_key)' slot an insert/move/promotion wants now
;; commits atomically: the writer lands at an id-derived temp key, the
;; delete frees the slot, and a sort-key-only finalize moves the writer
;; into place.

(ert-deftest dorg-test-mutate-dolt-delete-freed-insert-success ()
  "Delete a sibling and INSERT into its freed slot: commits via staging.
Seeds A at X'01' and B at X'02'; a batch deletes A and inserts C :before
B, landing on A's freed slot X'01'.  The scheduler inserts C at a temp
key, deletes A, then finalizes C's sort key — committing atomically
instead of racing `uq_heading_sibling_order'."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string 2))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "C")
                     :before (dorg-test-mutate--heading-address 102)))))
        ;; Plan now reports the batch supported (no delete-freed gap).
        (should (plist-get
                 (dorg-test-mutate--plan-patch-batch src patches) :supported))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :created)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (gethash 300 tree))
          ;; C occupies A's freed slot X'01' and orders before B.
          (should (equal (dorg-heading-sort-key (gethash 300 tree))
                         (unibyte-string 1)))
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-success ()
  "Delete a sibling and MOVE an existing heading into its freed slot.
Seeds A X'01', B X'02', D X'03'; deletes A and moves D :before B, landing
on A's freed slot X'01'.  No `delete-freed-sort-key-reuse' classification:
the scheduler updates D to a temp key, deletes A, then finalizes D's sort
key."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string 2)))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "D"
                                 :sort-key (unibyte-string 3))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert d 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--move-before-patch
                     103 :root (dorg-test-mutate--heading-address 102)))))
        (let ((plan (dorg-test-mutate--plan-patch-batch src patches)))
          (should (plist-get plan :supported))
          (should-not (plist-get plan :unsupported)))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 103 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (equal (dorg-heading-sort-key (gethash 103 tree))
                         (unibyte-string 1)))
          (should (string< (dorg-heading-sort-key (gethash 103 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-insert-parent-move-child ()
  "Combined FK + collision case commits atomically.
Deletes sibling S, inserts a new parent P into S's freed slot via staging,
and moves existing child X under that new parent — all in one commit.  P's
row exists (at a temp key) before X's UPDATE references it, the delete
frees the slot, and P's sort key is finalized last."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (s (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "S"
                                 :sort-key (unibyte-string 1)))
           (x (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "X"
                                 :sort-key (unibyte-string 2))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert s 100)
                            (dorg-sql-gen-heading-insert x 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "P")
                     :before (dorg-test-mutate--heading-address 102))
                    (dorg-test-mutate--move-patch
                     102 (dorg-test-mutate--heading-address 300)))))
        (should (plist-get
                 (dorg-test-mutate--plan-patch-batch src patches) :supported))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :created)))
          (should (member 102 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (gethash 300 tree))
          ;; New parent P took S's freed slot X'01' at the root.
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (should (equal (dorg-heading-sort-key (gethash 300 tree))
                         (unibyte-string 1)))
          ;; X is now a child of the freshly inserted parent.
          (should (eql (dorg-heading-parent-id (gethash 102 tree)) 300)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-child-promotion ()
  "Child promotion into the freed parent slot stays FK/cascade safe.
Deletes parent P (no :subtree) whose sole child C is promoted to the root
and lands on P's freed sibling slot X'40'.  The scheduler reparents C to a
temp key BEFORE deleting P, so ON DELETE CASCADE cannot remove the
surviving child, then finalizes C's sort key."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (p (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "P"
                                 :sort-key (unibyte-string #x40)))
           (q (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "Q"
                                 :sort-key (unibyte-string #x80)))
           (c (make-dorg-heading :id 102 :parent-id 101 :level 2 :title "C"
                                 :sort-key (unibyte-string #x80))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert p 100)
                            (dorg-sql-gen-heading-insert q 100)
                            (dorg-sql-gen-heading-insert c 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches (list (dorg-test-mutate--delete-patch 101))))
        (should (plist-get
                 (dorg-test-mutate--plan-patch-batch src patches) :supported))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 102 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          ;; C survived, promoted to the root at P's freed slot X'40'.
          (should (gethash 102 tree))
          (should (eql (dorg-heading-parent-id (gethash 102 tree)) 100))
          (should (eql (dorg-heading-level (gethash 102 tree)) 1))
          (should (equal (dorg-heading-sort-key (gethash 102 tree))
                         (unibyte-string #x40)))
          ;; Q untouched.
          (should (equal (dorg-heading-sort-key (gethash 103 tree))
                         (unibyte-string #x80))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-rollback ()
  "A staged delete-freed batch that fails at commit rolls back wholesale.
Seeds A X'01', B X'02' (#+FOO a), D X'03' (#+BAZ q).  The batch deletes A,
inserts C into A's freed slot (staged), and appends a keyword item to B; a
forced `heading_items.id' generator collides the new item id with D's
surviving BAZ row, so the whole transaction — staged insert, delete, and
item append — is rolled back.  HEAD is unchanged, A keeps its slot, C is
never finalized, and B's FOO is still \"a\"."
  (dorg-test-mutate--with-dolt
    (let* ((idg (let ((n 9000)) (lambda () (cl-incf n))))
           (root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string 2)
                                 :items (list (dorg-keyword-item
                                               :keyword "FOO" :value "a"))))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "D"
                                 :sort-key (unibyte-string 3)
                                 :items (list (dorg-keyword-item
                                               :keyword "BAZ" :value "q")))))
      (dorg-heading-compute-items-hash b)
      (dorg-heading-compute-items-hash d)
      (dorg-sql-batch dolt-dir
                      (append (list (dorg-sql-gen-heading-insert root-h 100)
                                    (dorg-sql-gen-heading-insert a 100))
                              (dorg-sql-gen-heading-full-sql b 100 idg)
                              (dorg-sql-gen-heading-full-sql d 100 idg)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             ;; A surviving (untouched) heading_items.id under D, so the
             ;; rewritten FOO row re-inserting it violates the PK.
             (collide (alist-get 'id (car (dorg-sql dolt-dir
                        "SELECT id FROM heading_items WHERE heading_id = 103 LIMIT 1"))))
             ;; Inserting after-last appends a second FOO item to B (forces an
             ;; INSERT whose generated id is the forced collision; an
             ;; in-place edit would reuse the existing id and not collide).
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "C")
                     :before (dorg-test-mutate--heading-address 102))
                    (dorg-test-mutate--keyword-patch
                     'insert (dorg-test-mutate--heading-address 102) "FOO"
                     :position :after-last :value "z"))))
        (should collide)
        (let ((dorg-mutate--dolt-item-id-generator (lambda () collide)))
          (should
           (eq 'invariant
               (dorg-test-mutate--reason
                (dorg-test-mutate--apply-patch-batch src patches)))))
        ;; Rolled back: HEAD unchanged, A in its slot, C absent, FOO still "a".
        (should (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should (gethash 101 tree))
          (should (equal (dorg-heading-sort-key (gethash 101 tree))
                         (unibyte-string 1)))
          (should-not (gethash 300 tree))
          (should (equal (dorg-test-mutate--dolt-keyword-values tree 102 "FOO")
                         '("a"))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-insert-key-collision ()
  "Staged insert whose id-derived temp key is already taken still commits.
Mirrors the acceptance blocker: an existing sibling D (103)
is seeded at the id-derived staging key for the inserted id 300, A (101)
sits at the midpoint between D and B (so an insert :before B lands on A's
freed slot), and B (102) is at X'80'.  Deleting A and inserting C (300)
:before B is a valid final model whose naive id-derived staging key would
collide with D on `uq_heading_sibling_order'.  The scheduler must detect
the occupied slot, pick a free temp key, and commit — plan agreeing,
without relying on rollback."
  (dorg-test-mutate--with-dolt
    (let* ((d-key (dorg-mutate--dolt-temp-sort-key 300))
           (b-key (unibyte-string #x80))
           (a-key (dorg-sortkey-midpoint d-key b-key))
           (root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key a-key))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key b-key))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "D"
                                 :sort-key d-key)))
      ;; Sanity: D really holds the slot the naive temp key would reuse.
      (should (equal d-key (dorg-mutate--dolt-temp-sort-key 300)))
      (should (string< d-key a-key))
      (should (string< a-key b-key))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert d 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "C")
                     :before (dorg-test-mutate--heading-address 102)))))
        (let ((plan (dorg-test-mutate--plan-patch-batch src patches)))
          (should (plist-get plan :supported))
          (should-not (plist-get plan :unsupported)))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :created)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          ;; C took A's freed slot and orders between D and B.
          (should (equal (dorg-heading-sort-key (gethash 300 tree)) a-key))
          (should (string< (dorg-heading-sort-key (gethash 103 tree))
                           (dorg-heading-sort-key (gethash 300 tree))))
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree))))
          ;; D is untouched at its (colliding-looking) seeded slot.
          (should (equal (dorg-heading-sort-key (gethash 103 tree)) d-key)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-update-key-collision-fails-closed ()
  "Staged update pinned to an occupied id-derived slot fails closed.
A staged UPDATE's transient staging key is forced to the id-derived value
by the core lowerer's divergent staging, so it cannot be perturbed.  Here
an existing sibling D (103) holds the id-derived staging key for the
moved heading M (300); deleting A (101) and moving M onto A's freed slot
is a valid final model, but the staging key is unavailable.  The
scheduler must refuse deterministically BEFORE SQL — plan and apply
signalling the same `unsupported' reason — rather than committing a
half-applied batch or leaning on rollback."
  (dorg-test-mutate--with-dolt
    (let* ((m-key (dorg-mutate--dolt-temp-sort-key 300))
           (b-key (unibyte-string #x80))
           (a-key (dorg-sortkey-midpoint m-key b-key))
           (root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key a-key))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key b-key))
           ;; D holds the id-derived staging slot M would need.
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "D"
                                 :sort-key m-key))
           ;; M is parked far to the right; moving it :before B lands it on
           ;; A's freed slot, requiring staging.
           (m (make-dorg-heading :id 300 :parent-id 100 :level 1 :title "M"
                                 :sort-key (unibyte-string #xc0))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert d 100)
                            (dorg-sql-gen-heading-insert m 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--move-before-patch
                     300 :root (dorg-test-mutate--heading-address 102)))))
        ;; Plan fails closed with the same unsupported reason apply raises.
        (should (eq 'unsupported
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--plan-patch-batch src patches))))
        (should (eq 'unsupported
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--apply-patch-batch src patches))))
        ;; No write: HEAD unchanged, A still in its slot.
        (should (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should (gethash 101 tree))
          (should (equal (dorg-heading-sort-key (gethash 101 tree)) a-key)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-source-parent-collision-fails-closed ()
  "Reparenting staged move fails closed when be8(id) collides in the SOURCE parent.
The database-op lowerer (`dorg-sql-gen-lower-heading-ops') parks every staged
`:update-heading' at the forced be8(id) staging key in a phase-1 pass
that runs BEFORE parent_id is changed, so for a REPARENTING move that
key is briefly live under the heading's ORIGINAL parent before it
becomes live under the destination parent.

Here the collision exists ONLY in the source parent: sibling X (301)
under source parent P (200) holds be8(300), the forced staging key of
the moved heading M (300).  No sibling of M's destination parent (root
100) holds that key.  A collision check that inspects only the final
parent therefore MISSES the source-parent collision, lets planning report the
batch supported, and lets apply reach SQL where it
fails on the unique (parent_id, sort_key) order under P and only rolls
back — i.e. plan and apply disagree.  This is the reviewer
repro.  The scheduler must instead detect the source-parent collision
and fail closed BEFORE SQL with reason `unsupported', so plan and apply
agree.

Layout: under root 100, A id 101 @ X'01' (deleted, frees slot X'01'),
B id 102 @ X'02', and survivor source parent P id 200 @ X'03'.  Under P:
M id 300 @ X'10' and X id 301 @ be8(300).  The batch deletes A and moves
M to root :before B, landing M on A's freed slot X'01' (so M is staged);
M's forced staging key be8(300) duplicates X under the original parent P."
  (dorg-test-mutate--with-dolt
    (let* ((m-temp (dorg-mutate--dolt-temp-sort-key 300))
           (root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string 2)))
           (p (make-dorg-heading :id 200 :parent-id 100 :level 1 :title "P"
                                 :sort-key (unibyte-string 3)))
           ;; M is the heading that will reparent from P to root.
           (m (make-dorg-heading :id 300 :parent-id 200 :level 2 :title "M"
                                 :sort-key (unibyte-string #x10)))
           ;; X sits under the SOURCE parent P at M's forced staging key;
           ;; root (the destination) has no sibling at this key.
           (x (make-dorg-heading :id 301 :parent-id 200 :level 2 :title "X"
                                 :sort-key m-temp)))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert p 100)
                            (dorg-sql-gen-heading-insert m 100)
                            (dorg-sql-gen-heading-insert x 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--move-before-patch
                     300 :root (dorg-test-mutate--heading-address 102)))))
        ;; Plan must fail closed (NOT report supported): the source-parent
        ;; collision is caught before SQL with `unsupported'.
        (should (eq 'unsupported
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--plan-patch-batch src patches))))
        ;; Apply must signal the SAME deterministic pre-SQL reason — not
        ;; the `invariant' constraint/rollback reason the pre-fix
        ;; final-parent-only check produced.  Asserting `unsupported' here
        ;; is what makes this regression non-vacuous: with the fix removed
        ;; the collision is missed, plan reports supported, and apply
        ;; signals `invariant' from the rolled-back unique-key violation.
        (should (eq 'unsupported
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--apply-patch-batch src patches))))
        ;; No write: HEAD unchanged, M still under its original parent P
        ;; at its original key, X untouched.
        (should (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should (gethash 101 tree))
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 200))
          (should (equal (dorg-heading-sort-key (gethash 300 tree))
                         (unibyte-string #x10)))
          (should (equal (dorg-heading-sort-key (gethash 301 tree))
                         m-temp)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-own-temp-key-commits ()
  "Staged move whose OWN current key is be8(id) commits (no false collision).
The forced staging key for a staged `:update-heading' is be8(id).  When
the moving heading ALREADY sits at be8(id) — and no DISTINCT sibling does
— the core lowerer's phase-1 staging merely rewrites the row's own slot
back to itself; this never races the `uq_heading_sibling_order' unique
key.  An owner-blind check counts the heading's own original row as an
occupied sibling and over-rejects the valid final model with
`delete-freed-sort-key-reuse'.  The check must be OWNER-AWARE.

Layout (reviewer repro): under root 100, A id 101 @ X'40'
\(deleted, frees slot X'40'), B id 102 @ X'80', and M id 300 already
parked at its own forced staging key be8(300).  The batch deletes A and
moves M :before B, landing M on A's freed slot X'40' (so M is staged); M's
forced staging key be8(300) is occupied ONLY by M itself, so the move must
commit."
  (dorg-test-mutate--with-dolt
    (let* ((m-temp (dorg-mutate--dolt-temp-sort-key 300))
           (root-h (make-dorg-heading :id 100 :level 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string #x40)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string #x80)))
           ;; M already sits at its OWN forced staging key be8(300); no
           ;; other sibling occupies that slot.
           (m (make-dorg-heading :id 300 :parent-id 100 :level 1 :title "M"
                                 :sort-key m-temp)))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert m 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (list :dolt-dir dolt-dir :root-id 100))
             (base (dorg-head-commit dolt-dir))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--move-before-patch
                     300 :root (dorg-test-mutate--heading-address 102)))))
        ;; Valid final model: plan supported, no delete-freed-sort-key-reuse.
        (let ((plan (dorg-test-mutate--plan-patch-batch src patches)))
          (should (plist-get plan :supported))
          (should-not (plist-get plan :unsupported)))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        ;; HEAD advanced once; A gone, M landed on A's freed slot before B.
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (should (equal (dorg-heading-sort-key (gethash 300 tree))
                         (unibyte-string #x40)))
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

;;; ─── Dolt Scoped Command-Write Specs (tracked-case RED) ────────────────

(defun dorg-test-mutate--scope-tree (&rest headings)
  "Return a heading hash containing HEADINGS."
  (let ((tree (make-hash-table :test 'eql)))
    (dolist (h headings tree)
      (puthash (dorg-heading-id h) h tree))))

(defun dorg-test-mutate--scope-child-ids (parent-id headings)
  "Return direct child ids of PARENT-ID in HEADINGS."
  (mapcar #'dorg-heading-id
          (cl-remove-if-not
           (lambda (h) (eql parent-id (dorg-heading-parent-id h)))
           headings)))

(defun dorg-test-mutate--scoped-parity-id-gen ()
  "Return a deterministic item-id generator for scoped parity simulations."
  (let ((next 900000))
    (lambda () (prog1 next (cl-incf next)))))

(defun dorg-test-mutate--sim-projection (sim)
  "Return the scoped-write parity fields from SIM."
  (mapcar (lambda (key) (cons key (plist-get sim key)))
          '(:db-ops :sql :created :changed :deleted)))

(defun dorg-test-mutate--assert-scoped-sim-parity (source patches ids)
  "Assert forced scoped simulation matches whole-root simulation for PATCHES.
IDS is the bounded load slice forced through the future scope resolver."
  (let* ((src (dorg-mutate--resolve-source source))
         (whole (let ((dorg-mutate--dolt-item-id-generator
                       (dorg-test-mutate--scoped-parity-id-gen)))
                  (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                             (lambda (&rest _) nil)))
                    (dorg-mutate--dolt-simulate src patches nil))))
         (scope-calls nil)
         (load-calls nil)
         (real-load-headings (symbol-function 'dorg-model-load-headings))
         (scoped (let ((dorg-mutate--dolt-item-id-generator
                        (dorg-test-mutate--scoped-parity-id-gen)))
                   (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                              (lambda (got-source got-patches root-id head)
                                (push (list got-source got-patches root-id head)
                                      scope-calls)
                                ids))
                             ((symbol-function 'dorg-model-load-tree)
                              (lambda (&rest _)
                                (ert-fail
                                 "scoped parity run must not call whole-root load")))
                             ((symbol-function 'dorg-model-load-headings)
                              (lambda (dolt-dir root-id got-ids
                                                &optional as-of headline-only)
                                (push (list root-id got-ids as-of headline-only)
                                      load-calls)
                                (funcall real-load-headings
                                         dolt-dir root-id got-ids
                                         as-of headline-only))))
                     (dorg-mutate--dolt-simulate src patches nil)))))
    (should scope-calls)
    (should load-calls)
    (should (equal (dorg-test-mutate--sim-projection scoped)
                   (dorg-test-mutate--sim-projection whole)))))

(defun dorg-test-mutate--assert-production-sparse-parity (source patches)
  "Assert production sparse simulation matches forced whole-root simulation.
Both arms use the same deterministic item-id stream.  The sparse arm is not
stubbed: it exercises the production structural-fact queries and bounded model
load.  The control forces only the old whole-root loading strategy."
  (let* ((src (dorg-mutate--resolve-source source))
         (sparse (let ((dorg-mutate--dolt-item-id-generator
                        (dorg-test-mutate--scoped-parity-id-gen)))
                   (dorg-mutate--dolt-simulate src patches nil)))
         (whole (let ((dorg-mutate--dolt-item-id-generator
                       (dorg-test-mutate--scoped-parity-id-gen)))
                  (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                             (lambda (&rest _) nil)))
                    (dorg-mutate--dolt-simulate src patches nil)))))
    (should (equal (dorg-test-mutate--sim-projection sparse)
                   (dorg-test-mutate--sim-projection whole)))))

(ert-deftest dorg-test-mutate-dolt-scope-load-envelope-uses-ids-and-plan-head ()
  "A bounded same-root command-write loads an id slice at the plan HEAD.
This is a RED spec for tracked-case.  Non-vacuity: the current executor still
calls `dorg-model-load-tree'; this test traps that whole-root path.  The fix
must compute scoped ids once for the captured plan HEAD, include the root row,
and pass the same HEAD to the id-restricted loader."
  (let* ((src (dorg-mutate--resolve-source '(:dolt-dir "/tmp/dorg-scope/" :root-id 100)))
         (patches (list (dorg-test-mutate--heading-update-patch
                         101 :title "A*")))
         (root (make-dorg-heading :id 100 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                               :sort-key (unibyte-string #x80)))
         (scope-calls nil)
         (load-calls nil))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (_dolt-dir) "H-plan"))
              ((symbol-function 'dorg-mutate--dolt-scope-ids)
               (lambda (source got-patches root-id head)
                 (push (list source got-patches root-id head) scope-calls)
                 '(100 101 102)))
              ((symbol-function 'dorg-mutate--dolt-child-ids)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of)
                 (dorg-test-mutate--scope-child-ids parent-id
                                                     (list root a b))))
              ((symbol-function 'dorg-model-load-tree)
               (lambda (&rest _)
                 (ert-fail "bounded mutation must not call dorg-model-load-tree")))
              ((symbol-function 'dorg-model-load-headings)
               (lambda (_dolt-dir root-id ids &optional as-of headline-only)
                 (push (list root-id ids as-of headline-only) load-calls)
                 (dorg-test-mutate--scope-tree root a b))))
      (let ((sim (dorg-mutate--dolt-simulate src patches nil)))
        (should (equal "H-plan" (plist-get sim :head-commit)))
        (should (equal '((100 101 102)) (mapcar #'cadr load-calls)))
        (should (equal '("H-plan") (mapcar #'caddr load-calls)))
        (should (member 100 (cadar load-calls)))
        (should (equal (list src patches 100 "H-plan") (car scope-calls)))))))

(ert-deftest dorg-test-mutate-dolt-scope-includes-affected-sibling-fan ()
  "Same-root scope includes root + affected siblings and excludes unrelated descendants.
For a delete-freed insert under the root, the executor reads all siblings of the
affected parent to prove staging keys are collision-free.  It does not need an
unrelated descendant under an unaffected sibling."
  (dorg-test-mutate--with-dolt
    (let* ((root (make-dorg-heading :id 100 :level 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                                 :sort-key (unibyte-string #x20)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :title "B"
                                 :sort-key (unibyte-string #x40)))
           (p (make-dorg-heading :id 103 :parent-id 100 :level 1 :title "P"
                                 :sort-key (unibyte-string #x60)))
           (far (make-dorg-heading :id 104 :parent-id 103 :level 2 :title "Far"
                                   :sort-key (unibyte-string #x40))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert p 100)
                            (dorg-sql-gen-heading-insert far 100)))
      (dorg-commit dolt-dir "seed")
      (let* ((src (dorg-mutate--resolve-source
                   (list :dolt-dir dolt-dir :root-id 100)))
             (patches
              (list (dorg-test-mutate--delete-patch 101)
                    (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "C")
                     :before (dorg-test-mutate--heading-address 102))))
             (ids (sort (copy-sequence
                         (dorg-mutate--dolt-scope-ids
                          src patches 100 (dorg-head-commit dolt-dir)))
                        #'<)))
        (dolist (id '(100 101 102 103))
          (should (member id ids)))
        (should-not (member 104 ids))))))

(ert-deftest dorg-test-2xv2m-insert-scope-is-local-to-placement-facts ()
  "Pure insert scope contains only the parent and exact placement neighbors.
The fixture has four direct root children.  Tail insertion needs only the last
child; insertion before B needs A+B; insertion after B needs B+C.  The old
affected-parent fan returns every child and therefore fails each exact set.
This applies only to pure insertion: move/delete tests continue to pin their
complete affected sibling fan independently."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* A\n* B\n* C\n* D\n" 100))
           (a (dorg-test-mutate--id-by-title dolt-dir "A"))
           (b (dorg-test-mutate--id-by-title dolt-dir "B"))
           (c (dorg-test-mutate--id-by-title dolt-dir "C"))
           (d (dorg-test-mutate--id-by-title dolt-dir "D"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id root)))
           (head (dorg-head-commit dolt-dir))
           (scope
            (lambda (patch)
              (sort (dorg-mutate--dolt-scope-ids
                     source (list patch) root head) #'<))))
      (should (equal (sort (list root d) #'<)
                     (funcall scope
                              (dorg-test-mutate--tree-insert-patch
                               :root '(:id 300 :title "Tail")))))
      (should (equal (sort (list root a b) #'<)
                     (funcall scope
                              (dorg-test-mutate--tree-insert-patch
                               :root '(:id 301 :title "Before B")
                               :before (dorg-test-mutate--heading-address b)))))
      (should (equal (sort (list root b c) #'<)
                     (funcall scope
                              (dorg-test-mutate--tree-insert-patch
                               :root '(:id 302 :title "After B")
                               :after (dorg-test-mutate--heading-address b))))))))

(ert-deftest dorg-test-2xv2m-insert-simulation-never-hydrates-sibling-fan ()
  "A pure root insert hydrates at most parent plus two local placement rows.
The SQL fact query may inspect ordering metadata, but the working-model loader
must not decode all sibling models.  The accepted performance fixture has 819
direct root siblings, so this witness uses the same cardinality: the old
implementation passes 820 ids (root + every child)."
  (dorg-test-mutate--with-dolt
    (let* ((contents
            (mapconcat (lambda (n) (format "* S%03d\n" n))
                       (number-sequence 1 819) ""))
           (root (dorg-test-mutate--seed dolt-dir contents 100))
           (middle (dorg-test-mutate--id-by-title dolt-dir "S410"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id root)))
           (real-load (symbol-function 'dorg-model-load-headings))
           (loaded nil))
      (cl-letf (((symbol-function 'dorg-model-load-headings)
                 (lambda (dir rid ids &optional as-of headline-only)
                   (push (copy-sequence ids) loaded)
                   (funcall real-load dir rid ids as-of headline-only))))
        (dorg-mutate--dolt-simulate
         source
         (list (dorg-test-mutate--tree-insert-patch
                :root '(:id 303 :title "Before middle")
                :before (dorg-test-mutate--heading-address middle)))
         nil))
      (should loaded)
      (dolist (ids loaded)
        (should ids)
        (should (<= (length ids) 3)))
      (should (cl-some (lambda (ids)
                         (and (member root ids)
                              (member middle ids)))
                       loaded)))))

(ert-deftest dorg-test-2xv2m-sparse-insert-placement-parity-matrix ()
  "Sparse insert placement is byte-equivalent to whole-root simulation.
The matrix covers empty/head/middle/tail placement, before/after anchors, and
both document-root and keyed-child sibling partitions.  Each case compares the
production sparse structural-fact path against a forced whole-root control at
the same revision."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* A\n"
                          "** A1\n"
                          "** A2\n"
                          "** A3\n"
                          "* B\n"
                          "* C\n"
                          "* D\n")
                  100))
           (a (dorg-test-mutate--id-by-title dolt-dir "A"))
           (a1 (dorg-test-mutate--id-by-title dolt-dir "A1"))
           (a2 (dorg-test-mutate--id-by-title dolt-dir "A2"))
           (a3 (dorg-test-mutate--id-by-title dolt-dir "A3"))
           (b (dorg-test-mutate--id-by-title dolt-dir "B"))
           (c (dorg-test-mutate--id-by-title dolt-dir "C"))
           (d (dorg-test-mutate--id-by-title dolt-dir "D"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (cases
            `((root-head
               ,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 301 :title "Root head")
                 :before (dorg-test-mutate--heading-address a)))
              (root-middle-before
               ,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 302 :title "Root middle before")
                 :before (dorg-test-mutate--heading-address c)))
              (root-middle-after
               ,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 303 :title "Root middle after")
                 :after (dorg-test-mutate--heading-address b)))
              (root-tail-after
               ,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 304 :title "Root tail after")
                 :after (dorg-test-mutate--heading-address d)))
              (root-tail-unanchored
               ,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 305 :title "Root tail")))
              (child-empty
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address b)
                 '(:id 306 :title "First child")))
              (child-head
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address a)
                 '(:id 307 :title "Child head")
                 :before (dorg-test-mutate--heading-address a1)))
              (child-middle-before
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address a)
                 '(:id 308 :title "Child middle before")
                 :before (dorg-test-mutate--heading-address a3)))
              (child-middle-after
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address a)
                 '(:id 309 :title "Child middle after")
                 :after (dorg-test-mutate--heading-address a2)))
              (child-tail-after
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address a)
                 '(:id 310 :title "Child tail after")
                 :after (dorg-test-mutate--heading-address a3)))
              (child-tail-unanchored
               ,(dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address a)
                 '(:id 311 :title "Child tail"))))))
      (dolist (case cases)
        (ert-info ((format "placement case %s" (car case)))
          (dorg-test-mutate--assert-production-sparse-parity
           source (list (cadr case))))))))

(ert-deftest dorg-test-mutate-dolt-scope-resolves-extern-structural-hosts ()
  "Extern structural targets resolve before parent/subtree SQL expansion.
This pins the ACCEPT-gate regression where structural typed patches passed an
unresolved external host into integer SQL helpers."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* A\n"
                          ":PROPERTIES:\n:EXT: alpha\n:END:\n"
                          "** A kid\n"
                          "* B\n"
                          ":PROPERTIES:\n:EXT: beta\n:END:\n"
                          "* C\n"
                          "** C kid\n")))
           (a (dorg-test-mutate--id-by-title dolt-dir "A"))
           (a-kid (dorg-test-mutate--id-by-title dolt-dir "A kid"))
           (b (dorg-test-mutate--id-by-title dolt-dir "B"))
           (c (dorg-test-mutate--id-by-title dolt-dir "C"))
           (c-kid (dorg-test-mutate--id-by-title dolt-dir "C kid"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id root)))
           (head (dorg-head-commit dolt-dir)))
      (dolist
          (case
           `(("delete-subtree"
              (,(list :op 'delete
                      :target (dorg-test-mutate--external-address
                               :subtree "EXT" "alpha")))
              (,root ,a ,a-kid ,b ,c)
              (,c-kid))
             ("move-heading"
              (,(dorg-test-mutate--move-before-patch
                 (dorg-test-mutate--external-address
                  :subtree "EXT" "beta")
                 :root (dorg-test-mutate--heading-address a)))
              (,root ,a ,b ,c)
              (,a-kid ,c-kid))
             ("move-subtree"
              (,(list :op 'move
                      :target (dorg-test-mutate--external-address
                               :subtree "EXT" "alpha")
                      :to (list :under :root
                                :after (dorg-test-mutate--heading-address b))))
              (,root ,a ,a-kid ,b ,c)
              (,c-kid))))
        (let* ((name (nth 0 case))
               (patches (nth 1 case))
               (ids (dorg-mutate--dolt-scope-ids source patches root head)))
          (ert-info ((format "case %s" name))
            (should ids)
            (should (cl-every #'integerp ids))
            (dolist (id (nth 2 case))
              (should (member id ids)))
            (dolist (id (nth 3 case))
              (should-not (member id ids)))))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-scope-resolves-extern-moving-host ()
  "Cross-root scope resolves an extern moving subtree before source SQL expansion."
  (dorg-test-mutate--with-dolt
    (let* ((src-root (dorg-test-mutate--seed
                      dolt-dir
                      (concat "* SrcMovable\n"
                              ":PROPERTIES:\n:EXT: alpha\n:END:\n"
                              "** SrcKid\n"
                              "* SrcSibling\n")
                      100))
           (dst-root (dorg-test-mutate--seed
                      dolt-dir
                      "* DstParent\n** DstChild\n* DstRootSibling\n"
                      500))
           (movable (dorg-test-mutate--id-by-title dolt-dir "SrcMovable"))
           (kid (dorg-test-mutate--id-by-title dolt-dir "SrcKid"))
           (src-sibling (dorg-test-mutate--id-by-title dolt-dir "SrcSibling"))
           (dst-parent (dorg-test-mutate--id-by-title dolt-dir "DstParent"))
           (dst-child (dorg-test-mutate--id-by-title dolt-dir "DstChild"))
           (dst-root-sibling (dorg-test-mutate--id-by-title
                              dolt-dir "DstRootSibling"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id src-root)))
           (dest (dorg-mutate--resolve-source
                  (list :dolt-dir dolt-dir :root-id dst-root)))
           (patch (dorg-test-mutate--move-patch
                   (dorg-test-mutate--external-address
                    :subtree "EXT" "alpha")
                   (dorg-test-mutate--heading-address dst-parent)
                   :to-source dest))
           (scopes (dorg-mutate--dolt-cross-root-scope-ids
                    source dest patch (dorg-head-commit dolt-dir)))
           (src-ids (car scopes))
           (dst-ids (cdr scopes)))
      (should src-ids)
      (should dst-ids)
      (should (cl-every #'integerp src-ids))
      (should (cl-every #'integerp dst-ids))
      (dolist (id (list src-root movable kid src-sibling))
        (should (member id src-ids)))
      (dolist (id (list dst-root dst-parent dst-child))
        (should (member id dst-ids)))
      (should-not (member dst-root-sibling dst-ids)))))

(ert-deftest dorg-test-mutate-dolt-scope-parity-matrix ()
  "Scoped simulation matches whole-root simulation over the typed-patch matrix.
This is a RED spec for tracked-case.  Non-vacuity: the scoped arm traps
`dorg-model-load-tree', so the current whole-root-only executor fails before
it can accidentally compare two whole-root runs."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "#+OPTIONS: old\n"
                          "* A\n"
                          "SCHEDULED: <2026-07-01 Wed>\n"
                          ":PROPERTIES:\n:EXT: alpha\n:END:\n"
                          "#+FOO: old\n"
                          "Body A.\n"
                          "* B\n"
                          ":PROPERTIES:\n:EXT: beta\n:END:\n"
                          "* Parent\n"
                          "** Child\n"
                          "* MoveMe\n"
                          "* Far\n"
                          "** Far kid\n")))
           (a (dorg-test-mutate--id-by-title dolt-dir "A"))
           (b (dorg-test-mutate--id-by-title dolt-dir "B"))
           (parent (dorg-test-mutate--id-by-title dolt-dir "Parent"))
           (child (dorg-test-mutate--id-by-title dolt-dir "Child"))
           (move (dorg-test-mutate--id-by-title dolt-dir "MoveMe"))
           (far (dorg-test-mutate--id-by-title dolt-dir "Far"))
           (far-kid (dorg-test-mutate--id-by-title dolt-dir "Far kid"))
           (root-fan (list root a b parent move far))
           (source (list :dolt-dir dolt-dir :root-id root
                         :identity '(:property "EXT"))))
      ;; Force the root sibling slots so deleting A then moving MoveMe before B
      ;; reuses A's freed X'40' slot and exercises the staged-slot scheduler.
      (dorg-sql-batch
       dolt-dir
       (list (format "UPDATE headings SET sort_key = %s WHERE id = %d"
                     (dorg-sql-gen--sort-key-hex (unibyte-string #x40)) a)
             (format "UPDATE headings SET sort_key = %s WHERE id = %d"
                     (dorg-sql-gen--sort-key-hex (unibyte-string #x80)) b)
             (format "UPDATE headings SET sort_key = %s WHERE id = %d"
                     (dorg-sql-gen--sort-key-hex (unibyte-string #xc0)) parent)
             (format "UPDATE headings SET sort_key = %s WHERE id = %d"
                     (dorg-sql-gen--sort-key-hex (unibyte-string #xe0)) move)
             (format "UPDATE headings SET sort_key = %s WHERE id = %d"
                     (dorg-sql-gen--sort-key-hex (unibyte-string #xf0)) far)))
      (dorg-commit dolt-dir "shape scoped parity fixture")
      (dolist
          (case
           `(("update/property/planning/keyword/item"
              (,(dorg-test-mutate--heading-update-patch a :title "A*")
               ,(dorg-test-mutate--property-put-patch a "K" "V")
               ,(dorg-test-mutate--planning-patch
                 a :deadline "<2026-08-01 Sat>")
               ,(dorg-test-mutate--keyword-occurrence-patch
                 'replace :root "OPTIONS" 1 :value "new" :missing :error)
               ,(dorg-test-mutate--keyword-occurrence-patch
                 'replace (dorg-test-mutate--heading-address a)
                 "FOO" 1 :value "new" :missing :error))
              ,(list root a))
             ("insert-anchor"
              (,(dorg-test-mutate--tree-insert-patch
                 :root '(:id 300 :title "Inserted")
                 :before (dorg-test-mutate--heading-address b)))
              ,root-fan)
             ("delete-subtree"
              (,(dorg-test-mutate--delete-patch parent t))
              ,(append root-fan (list child)))
             ("delete-promotes-children"
              (,(dorg-test-mutate--delete-patch parent))
              ,(append root-fan (list child)))
             ("move-within-root"
              (,(dorg-test-mutate--move-before-patch
                 move :root (dorg-test-mutate--heading-address a)))
              ,root-fan)
             ("delete-freed-slot-reuse"
              (,(dorg-test-mutate--delete-patch a)
               ,(dorg-test-mutate--move-before-patch
                 move :root (dorg-test-mutate--heading-address b)))
              ,root-fan)
             ("extern-host"
              (,(list :op 'put
                      :target (list
                               :property
                               :host (dorg-test-mutate--external-address
                                      :heading "EXT" "beta")
                               :name "K")
                      :value "V"))
              ,(list root b))
             ("unrelated-descendant-excluded-control"
              (,(dorg-test-mutate--property-put-patch a "K2" "V2"))
              ,(list root a))))
        (let ((name (nth 0 case))
              (patches (nth 1 case))
              (ids (delete-dups (copy-sequence (nth 2 case)))))
          (should-not (member far-kid ids))
          (ert-info ((format "case %s" name))
            (dorg-test-mutate--assert-scoped-sim-parity
             source patches ids)))))))

(ert-deftest dorg-test-mutate-dolt-scope-nil-falls-back-to-whole-root-at-plan-head ()
  "A nil scope is an explicit whole-root fallback loaded at the plan HEAD.
This is a RED spec for tracked-case.  The current executor whole-root-loads, but
it does not call the scope resolver and does not pin that fallback to the
captured plan HEAD."
  (let* ((src (dorg-mutate--resolve-source '(:dolt-dir "/tmp/dorg-scope/"
                                             :root-id 100)))
         (patches (list (dorg-test-mutate--heading-update-patch
                         101 :title "A*")))
         (root (make-dorg-heading :id 100 :level 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (scope-calls nil)
         (load-tree-calls nil)
         (load-heading-calls nil))
    (cl-letf (((symbol-function 'dorg-head-commit)
               (lambda (_dolt-dir) "H-plan"))
              ((symbol-function 'dorg-mutate--dolt-scope-ids)
               (lambda (source got-patches root-id head)
                 (push (list source got-patches root-id head) scope-calls)
                 nil))
              ((symbol-function 'dorg-mutate--dolt-child-ids)
               (lambda (_dolt-dir _root-id parent-id &optional _as-of)
                 (dorg-test-mutate--scope-child-ids parent-id
                                                     (list root a))))
              ((symbol-function 'dorg-model-load-tree)
               (lambda (_dolt-dir root-id &optional as-of)
                 (push (list root-id as-of) load-tree-calls)
                 (dorg-test-mutate--scope-tree root a)))
              ((symbol-function 'dorg-model-load-headings)
               (lambda (_dolt-dir root-id ids &optional as-of headline-only)
                 (push (list root-id ids as-of headline-only) load-heading-calls)
                 (ert-fail "nil scope fallback must not use id-restricted load"))))
      (let ((sim (dorg-mutate--dolt-simulate src patches nil)))
        (should (equal "H-plan" (plist-get sim :head-commit)))
        (should (equal (list src patches 100 "H-plan") (car scope-calls)))
        (should (equal '((100 "H-plan")) load-tree-calls))
        (should-not load-heading-calls)))))

;;; ─── Dolt Cross-Root (Archive) Move Integration (gated) ───────────────

;; These exercise the bounded cross-root move against a real
;; temporary Dolt repository holding TWO roots in one repo.  Ids are
;; allocated sequentially from the seed root id (see
;; `dorg-test--parse-to-sql'): seeding root 100 gives root=100, first
;; real heading 101, ...; seeding root 500 gives root=500, 501, ....  The
;; two id ranges are kept far apart so no global PRIMARY KEY collides.

(defun dorg-test-mutate--cross-root-seed (dolt-dir)
  "Seed DOLT-DIR with a source root (100) and a destination root (500).
Source root 100: a movable subtree `Movable' (101) > `Kid' (102) plus a
sibling `SrcSib' (103).  Destination root 500: `DestParent' (501) and
`DestSib' (502).  Returns the plist of named ids."
  (dorg-test-mutate--seed dolt-dir "* Movable\n** Kid\n* SrcSib\n" 100)
  (dorg-test-mutate--seed dolt-dir "* DestParent\n* DestSib\n" 500)
  (list :src-root 100 :dst-root 500
        :movable 101 :kid 102 :src-sib 103
        :dst-parent 501 :dst-sib 502))

(defun dorg-test-mutate--cross-root-move-patch
    (id under dolt-dir root-id &rest fields)
  "Return a private typed cross-root move patch for ID below UNDER."
  (apply #'dorg-test-mutate--move-patch
         id under :to-source
         (dorg-mutate--resolve-source
          (list :dolt-dir dolt-dir :root-id root-id))
         fields))

(ert-deftest dorg-test-mutate-dolt-cross-root-move-success ()
  "A same-repo cross-root move re-homes the subtree in one Dolt commit.
The moved subtree leaves the source root and appears under the destination
parent with adjusted levels; HEAD advances exactly once."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir))
           (res (dorg-test-mutate--apply-patch-batch
                 src
                 (list
                  (dorg-test-mutate--cross-root-move-patch
                   (plist-get ids :movable)
                   (dorg-test-mutate--heading-address
                    (plist-get ids :dst-parent))
                   dolt-dir (plist-get ids :dst-root))))))
      (should (plist-get res :ok))
      (should (member (plist-get ids :movable) (plist-get res :changed)))
      (should (member (plist-get ids :movable) (plist-get res :moved)))
      (should-not (plist-get res :created))
      (should-not (plist-get res :deleted))
      (should (equal (plist-get res :from-root-id) (plist-get ids :src-root)))
      (should (equal (plist-get res :to-root-id) (plist-get ids :dst-root)))
      ;; HEAD advanced exactly once.
      (should-not (equal base (dorg-head-commit dolt-dir)))
      ;; Source root no longer renders the moved subtree; its sibling stays.
      (let ((src-tree (dorg-model-load-tree dolt-dir (plist-get ids :src-root))))
        (should-not (gethash (plist-get ids :movable) src-tree))
        (should-not (gethash (plist-get ids :kid) src-tree))
        (should (gethash (plist-get ids :src-sib) src-tree)))
      ;; Destination root now holds the moved subtree with preserved ids,
      ;; correct parent linkage, and shifted levels.
      (let* ((dst-tree (dorg-model-load-tree dolt-dir (plist-get ids :dst-root)))
             (mv (gethash (plist-get ids :movable) dst-tree))
             (kid (gethash (plist-get ids :kid) dst-tree)))
        (should mv)
        (should kid)
        (should (eql (dorg-heading-parent-id mv) (plist-get ids :dst-parent)))
        (should (eql (dorg-heading-parent-id kid) (plist-get ids :movable)))
        ;; DestParent is level 1, so Movable is level 2 and Kid level 3.
        (should (= (dorg-heading-level mv) 2))
        (should (= (dorg-heading-level kid) 3))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-preserves-id-and-content ()
  "A cross-root move preserves heading id, todo/priority/tags, props, items.
Tags and items hang off `heading_id', which does not change, so no content
is deleted/reinserted by the move."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed
     dolt-dir
     (concat "* TODO [#A] Rich :work:urgent:\n"
             "SCHEDULED: <2026-05-29 Fri>\n"
             ":PROPERTIES:\n:K: V\n:END:\n"
             "Body line.\n"
             "** Leaf\n")
     100)
    (dorg-test-mutate--seed dolt-dir "* DestParent\n" 500)
    (let* ((rich 101) (leaf 102) (dst-parent 501)
           (src (list :dolt-dir dolt-dir :root-id 100))
           ;; Snapshot the items hash before the move for equality.
           (before (dorg-heading-items-hash
                    (gethash rich (dorg-model-load-tree dolt-dir 100))))
           (res (dorg-test-mutate--apply-patch-batch
                 src
                 (list
                  (dorg-test-mutate--cross-root-move-patch
                   rich (dorg-test-mutate--heading-address dst-parent)
                   dolt-dir 500)))))
      (should (plist-get res :ok))
      (let* ((dst-tree (dorg-model-load-tree dolt-dir 500))
             (h (gethash rich dst-tree))
             (items (dorg-heading-items h)))
        (should h)
        (should (eql (dorg-heading-id h) rich))
        (should (equal (dorg-heading-todo h) "TODO"))
        (should (equal (dorg-heading-priority h) "A"))
        (should (equal (dorg-heading-tags h) '("work" "urgent")))
        ;; Items hash unchanged: body/planning/properties survived intact.
        (should (equal (dorg-heading-items-hash h) before))
        (should (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item)) items))
        (should (cl-find-if
                 (lambda (it) (and (cl-typep it 'dorg-drawer-item)
                                   (equal (dorg-drawer-item-name it) "PROPERTIES")))
                 items))
        ;; The leaf child moved with its parent and kept its id.
        (should (gethash leaf dst-tree))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-destination-ordering ()
  "Typed anchors order the moved subtree among destination siblings."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed dolt-dir "* Movable\n" 100)
    (dorg-test-mutate--seed dolt-dir "* AA\n* BB\n" 500)
    (let* ((movable 101) (aa 501) (bb 502)
           (src (list :dolt-dir dolt-dir :root-id 100)))
      ;; Move under the destination ROOT, positioned before BB.
      (dorg-test-mutate--apply-patch-batch
       src
       (list
        (append (dorg-test-mutate--move-before-patch
                 movable :root (dorg-test-mutate--heading-address bb))
                (list :to-source
                      (dorg-mutate--resolve-source
                       (list :dolt-dir dolt-dir :root-id 500))))))
      (let* ((dst-tree (dorg-model-load-tree dolt-dir 500))
             (kaa (dorg-heading-sort-key (gethash aa dst-tree)))
             (kmv (dorg-heading-sort-key (gethash movable dst-tree)))
             (kbb (dorg-heading-sort-key (gethash bb dst-tree))))
        ;; Order AA < Movable < BB.
        (should (string< kaa kmv))
        (should (string< kmv kbb))
        (should (eql (dorg-heading-parent-id (gethash movable dst-tree)) 500))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-source-root-rejected ()
  "Moving the source level-0 root heading signals `invariant', writes nothing."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir)))
      (should (eq 'invariant
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src
                    (list
                     (dorg-test-mutate--cross-root-move-patch
                      (plist-get ids :src-root)
                      (dorg-test-mutate--heading-address
                       (plist-get ids :dst-parent))
                      dolt-dir (plist-get ids :dst-root)))))))
      (should (equal base (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-missing-dest-parent-no-partial ()
  "A missing destination parent fails before any write: no partial move.
The source subtree stays intact, the destination gains no copied rows, and
HEAD is unchanged — proving no partial source delete / destination copy."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir)))
      ;; 999 is not a heading in the destination root.
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src
                    (list
                     (dorg-test-mutate--cross-root-move-patch
                      (plist-get ids :movable)
                      (dorg-test-mutate--heading-address 999)
                      dolt-dir (plist-get ids :dst-root)))))))
      (should (equal base (dorg-head-commit dolt-dir)))
      ;; Source subtree untouched.
      (let ((src-tree (dorg-model-load-tree dolt-dir (plist-get ids :src-root))))
        (should (gethash (plist-get ids :movable) src-tree))
        (should (gethash (plist-get ids :kid) src-tree)))
      ;; Destination has no copy of the moved ids.
      (let ((dst-tree (dorg-model-load-tree dolt-dir (plist-get ids :dst-root))))
        (should-not (gethash (plist-get ids :movable) dst-tree))
        (should-not (gethash (plist-get ids :kid) dst-tree))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-stale-base-rejected ()
  "A stale `:base-commit' whose cross-root move OVERLAPS an intervening change to
the moved heading is rejected terminal `overlap' before any write (tracked-regression
§2.4) — the cross-root executor is gated identically to the same-root one.  A
DISJOINT intervening change would instead re-plan and commit (see the linearize
suite); here the intervening commit touches the MOVED heading to force overlap."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (stale (dorg-head-commit dolt-dir)))
      ;; Advance HEAD past STALE with a commit on the MOVED heading, so the
      ;; pending move's write-set overlaps the intervening change.
      (dorg-test-mutate--apply-patch-batch
       src
       (list (dorg-test-mutate--heading-update-patch
              (plist-get ids :movable) :title "Touched")))
      (let ((head (dorg-head-commit dolt-dir)))
        (should (eq 'overlap
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--apply-patch-batch
                      src
                      (list
                       (dorg-test-mutate--cross-root-move-patch
                        (plist-get ids :movable)
                        (dorg-test-mutate--heading-address
                         (plist-get ids :dst-parent))
                        dolt-dir (plist-get ids :dst-root)))
                      :base-commit stale))))
        ;; The rejected move wrote nothing; the source subtree stays put.
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (gethash (plist-get ids :movable)
                         (dorg-model-load-tree dolt-dir (plist-get ids :src-root))))
        (should-not (gethash (plist-get ids :movable)
                             (dorg-model-load-tree dolt-dir (plist-get ids :dst-root))))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-dry-run-no-write ()
  "Dry-run plan returns semantic cross-root data and advances no HEAD.
Reports source/destination root ids, moved ids, and the head commit; the
source and destination roots are left untouched."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir))
           (plan
            (dorg-test-mutate--apply-patch-batch
             src
             (list
              (dorg-test-mutate--cross-root-move-patch
               (plist-get ids :movable)
               (dorg-test-mutate--heading-address
                (plist-get ids :dst-parent))
               dolt-dir (plist-get ids :dst-root)))
             :dry-run t)))
      (should (plist-get plan :dry-run))
      (should (plist-get plan :supported))
      (should (equal (plist-get plan :head-commit) base))
      (should (equal (plist-get plan :from-root-id) (plist-get ids :src-root)))
      (should (equal (plist-get plan :to-root-id) (plist-get ids :dst-root)))
      (should (member (plist-get ids :movable) (plist-get plan :moved)))
      (should (plist-get plan :db-ops))
      ;; No commit; both roots unchanged.
      (should (equal base (dorg-head-commit dolt-dir)))
      (should (gethash (plist-get ids :movable)
                       (dorg-model-load-tree dolt-dir (plist-get ids :src-root))))
      (should-not (gethash (plist-get ids :movable)
                           (dorg-model-load-tree dolt-dir (plist-get ids :dst-root)))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-preserve-ids-nil-live-no-write ()
  "Explicit `:preserve-ids nil' is rejected as unsupported, writing nothing."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir)))
      (should (eq 'unsupported
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src
                    (list
                     (dorg-test-mutate--cross-root-move-patch
                      (plist-get ids :movable)
                      (dorg-test-mutate--heading-address
                       (plist-get ids :dst-parent))
                      dolt-dir (plist-get ids :dst-root)
                      :preserve-ids nil))))))
      (should (equal base (dorg-head-commit dolt-dir)))
      (should (gethash (plist-get ids :movable)
                       (dorg-model-load-tree dolt-dir (plist-get ids :src-root))))
      (should-not (gethash (plist-get ids :movable)
                           (dorg-model-load-tree dolt-dir (plist-get ids :dst-root)))))))

(ert-deftest dorg-test-mutate-dolt-move-stays-root-scoped ()
  "A move without `:to-source' is root-scoped and writes no commit on failure.
The patch loads only the source root, so a
parent id that exists only in a different Dolt root resolves as a missing
target — it never silently reparents across roots."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (base (dorg-head-commit dolt-dir)))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    src
                    (list
                     (dorg-test-mutate--move-patch
                      (plist-get ids :movable)
                      (dorg-test-mutate--heading-address
                       (plist-get ids :dst-parent))))))))
      (should (equal base (dorg-head-commit dolt-dir)))
      (should (gethash (plist-get ids :movable)
                       (dorg-model-load-tree dolt-dir (plist-get ids :src-root)))))))

;;; ─── B1 Patch-instance parity and divergence ─────────────────────

(defun dorg-test-mutate--patch-support1 (source patch)
  "Return the sole `:patch-support' classification for PATCH."
  (car (plist-get
        (dorg-test-mutate--plan-patch-batch source (list patch))
        :patch-support)))

(ert-deftest dorg-test-mutate-patch-instance-move-parity-and-divergence ()
  "AC-B1b: concrete typed move patches classify per operand.
Dolt classifies PER OPERAND — a same-root move
\(no/same-dir `:to-source') is supported, a cross-repo/cross-backend move is
unsupported.  PLAIN-ORG has NO source-preserving editor for subtree moves at
ANY operand, so every plain-org instance is unsupported (verb-level, not
operand-level) — the clean-break divergence from the old same-source parity.
Root-local move semantics are retained on Dolt by
`dorg-test-mutate-dolt-same-root-move-supported' /
`dorg-test-mutate-dolt-cross-root-move-success'."
  (let ((plain '(:file "/tmp/tracked-case.org"))
        (dolt '(:dolt-dir "/tmp/dorg-b1b-a" :root-id 1)))
    ;; Dolt same-source move (no :to-source): supported.
    (should (plist-get (dorg-test-mutate--patch-support1
                        dolt (dorg-test-mutate--move-patch
                              8 (dorg-test-mutate--heading-address 3)))
                       :supported))
    ;; Plain-org same-source move: no source-preserving editor -> unsupported.
    (should-not (plist-get (dorg-test-mutate--patch-support1
                            plain (dorg-test-mutate--move-patch
                                   8 (dorg-test-mutate--heading-address 3)))
                           :supported))
    ;; Plain-org cross-source move (other file, :allow-nonatomic): unsupported.
    (should-not (plist-get (dorg-test-mutate--patch-support1
                            plain
                            (dorg-test-mutate--move-patch
                             8 (dorg-test-mutate--heading-address 3)
                             :allow-nonatomic t
                             :to-source
                             (dorg-mutate--resolve-source
                              '(:file "/tmp/dorg-b1b-other.org"))))
                           :supported))
    ;; Dolt same-dir / different-root move (ids preserved): supported.
    (should (plist-get (dorg-test-mutate--patch-support1
                        dolt
                        (dorg-test-mutate--move-patch
                         8 (dorg-test-mutate--heading-address 3)
                         :to-source
                         (dorg-mutate--resolve-source
                          '(:dolt-dir "/tmp/dorg-b1b-a" :root-id 2))))
                       :supported))
    ;; Dolt cross-repo (different dolt-dir) and cross-backend: unsupported.
    (should-not (plist-get (dorg-test-mutate--patch-support1
                            dolt
                            (dorg-test-mutate--move-patch
                             8 (dorg-test-mutate--heading-address 3)
                             :allow-nonatomic t
                             :to-source
                             (dorg-mutate--resolve-source
                              '(:dolt-dir "/tmp/dorg-b1b-b" :root-id 2))))
                           :supported))
    (should-not (plist-get (dorg-test-mutate--patch-support1
                            dolt
                            (dorg-test-mutate--move-patch
                             8 (dorg-test-mutate--heading-address 3)
                             :allow-nonatomic t
                             :to-source
                             (dorg-mutate--resolve-source
                              '(:file "/tmp/tracked-case.org"))))
                           :supported))
    ;; Divergence (non-vacuity): dropping :to-source flips DOLT to supported,
    ;; while plain-org stays unsupported for the SAME operand — the verb has no
    ;; plain source-preserving editor, so the operand cannot rescue it.
    (let ((patch (dorg-test-mutate--move-patch
                  8 (dorg-test-mutate--heading-address 3))))
      (should (plist-get (dorg-test-mutate--patch-support1 dolt patch)
                         :supported))
      (should-not (plist-get (dorg-test-mutate--patch-support1 plain patch)
                             :supported)))))

;;; ─── B2 Direct result plists ──────────────────────────────────────────

(ert-deftest dorg-test-mutate-result-plist-plain-org ()
  "AC-B2: a plain-org result exposes `:revision' and changed ids directly.
Uses the supported property-put cell on the DORG_ID heading
\(identity `dorg'); the heading structural update is retained on Dolt by
`dorg-test-mutate-result-plist-dolt'."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((before (buffer-string))
           (res (dorg-test-mutate--apply-patch-batch
                 buf
                 (list (dorg-test-mutate--property-put-patch
                        1001 "CATEGORY" "work")))))
      (should (plist-get res :ok))
      (should (plist-get res :revision))
      (should (member 1001 (plist-get res :changed)))
      ;; user transformation (non-vacuity): before != after, exact value written
      (should-not (equal (buffer-string) before))
      (should (string-match-p ":CATEGORY: +work" (buffer-string))))))

(ert-deftest dorg-test-mutate-result-plist-dolt ()
  "AC-B2: a Dolt same-root result exposes `:commit' and changed ids directly."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Old\n"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "Old"))
           (res (dorg-test-mutate--apply-patch-batch
                 (list :dolt-dir dolt-dir :root-id root)
                 (list (dorg-test-mutate--heading-update-patch
                        cid :title "New")))))
      (should (plist-get res :commit))
      (should (member cid (plist-get res :changed))))))

(ert-deftest dorg-test-mutate-result-plist-cross-root-includes-moved ()
  "AC-B2: a cross-root result exposes the moved ids and new commit directly."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root)))
           (res (dorg-test-mutate--apply-patch-batch
                 src
                 (list
                  (dorg-test-mutate--cross-root-move-patch
                   (plist-get ids :movable)
                   (dorg-test-mutate--heading-address
                    (plist-get ids :dst-parent))
                   dolt-dir (plist-get ids :dst-root))))))
      (should (plist-get res :commit))
      (should (member (plist-get ids :movable) (plist-get res :moved))))))

;;; ─── B3 plain-org PULL-by-freshness (needs no explicit invalidation) ──

(ert-deftest dorg-test-mutate-plain-org-cache-key-flips-on-content-change ()
  "AC-B3: a content-changing plain-org mutation flips the read cache key
\(the freshness discriminator), so the next read misses the warm cache with
NO explicit invalidate — proving plain-org needs no `:invalidation'.  Pure
private cache-key coverage; does NOT touch `dorg-ext-hydrate'."
  (dorg-test-mutate--with-content buf "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((file (buffer-file-name buf))
           (query '(:by-tag "x"))
           (before (with-temp-buffer (insert-file-contents file) (buffer-string)))
           (k1 (dorg-test-mutate--ext-read-cache-key 'demo file query)))
      ;; Non-vacuity control: recomputing with NO mutation is stable.
      (should (equal k1 (dorg-test-mutate--ext-read-cache-key 'demo file query)))
      ;; A content-changing, saved supported property mutation.
      (dorg-test-mutate--apply-patch-batch
       buf (list (dorg-test-mutate--property-put-patch 1 "CATEGORY" "work"))
       :save t)
      ;; user transformation (non-vacuity): the on-disk source actually changed
      ;; with the exact intended value — the write, not just the key, is real.
      (let ((after (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should-not (equal after before))
        (should (string-match-p ":CATEGORY: +work" after)))
      ;; and the freshness key flips with NO explicit invalidate.
      (should-not (equal k1 (dorg-test-mutate--ext-read-cache-key 'demo file query))))))

;;; ─── B4 Backend-neutral error classification parity ───────────────────

(ert-deftest dorg-test-mutate-error-reason-parity-unsupported ()
  "AC-B4: an unsupported patch classifies to `unsupported' on both backends."
  ;; Plain-org cross-source move (no atomic cross-source move).
  (should (eq 'unsupported
              (dorg-test-mutate--reason
               (dorg-test-mutate--apply-patch-batch
                '(:file "/tmp/dorg-b4-a.org")
                (list
                 (dorg-test-mutate--move-patch
                  8 (dorg-test-mutate--heading-address 3)
                  :allow-nonatomic t
                  :to-source
                  (dorg-mutate--resolve-source
                   '(:file "/tmp/dorg-b4-b.org"))))))))
  ;; Dolt cross-repo move.
  (should (eq 'unsupported
              (dorg-test-mutate--reason
               (dorg-test-mutate--apply-patch-batch
                '(:dolt-dir "/tmp/dorg-b4-a" :root-id 1)
                (list
                 (dorg-test-mutate--move-patch
                  8 (dorg-test-mutate--heading-address 3)
                  :allow-nonatomic t
                  :to-source
                  (dorg-mutate--resolve-source
                   '(:dolt-dir "/tmp/dorg-b4-b" :root-id 2)))))))))

(ert-deftest dorg-test-mutate-error-reason-parity-stale ()
  "AC-B4 + tracked-regression: a conflicting stale optimistic token is REJECTED before
any partial write on BOTH backends, but the reason now DIVERGES by backend design.
Plain-org keeps strict-mtime optimistic CAS -> `stale'.  Dolt's per-store
linearization gate returns `overlap' for a same-heading conflict (first-committer-
wins); only re-plan-deadline exhaustion yields Dolt `stale', and a DISJOINT stale
Dolt write re-plans and COMMITS rather than failing (see the linearize suite)."
  ;; Plain-org: a stale :base-revision (supported property-put vehicle).
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((patches
            (list (dorg-test-mutate--property-put-patch
                   1001 "CATEGORY" "work")))
           (rev (plist-get
                 (dorg-test-mutate--plan-patch-batch buf patches)
                 :base-revision)))
      (goto-char (point-max))
      (insert "\n* Extra\n")
      (let ((after-edit (buffer-string)))
        (should (eq 'stale
                    (dorg-test-mutate--reason
                     (dorg-test-mutate--apply-patch-batch
                      buf patches :base-revision rev))))
        ;; the stale-rejected apply left the buffer byte-identical to the
        ;; post-manual-edit state (no partial write) ...
        (should (equal (buffer-string) after-edit))
        ;; ... and specifically never wrote the CATEGORY property.
        (should-not (string-match-p ":CATEGORY:" (buffer-string))))))
  ;; Dolt: a stale :base-commit whose intent OVERLAPS the intervening same-heading
  ;; change -> terminal `overlap' (the gate's first-committer-wins), not `stale'.
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Old\n"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "Old"))
           (stale-base (dorg-head-commit dolt-dir)))
      (dorg-test-mutate--apply-patch-batch
       (list :dolt-dir dolt-dir :root-id root)
       (list (dorg-test-mutate--heading-update-patch cid :title "Mid")))
      (should (eq 'overlap
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    (list :dolt-dir dolt-dir :root-id root)
                    (list (dorg-test-mutate--heading-update-patch
                           cid :title "New"))
                    :base-commit stale-base)))))))

(ert-deftest dorg-test-mutate-error-reason-parity-invalid-target ()
  "AC-B4: a missing heading target classifies to `invalid-target' on both
backends.  The plain-org arm uses the SUPPORTED `:set-property' cell under the
load-bearing resolved `dorg' policy (exact `missing' detail); the Dolt arm keeps
its structural `:update-heading' and still proves `invalid-target'."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((d (dorg-test-mutate--error-data
              (dorg-test-mutate--apply-patch-batch
               buf
               (list (dorg-test-mutate--property-put-patch
                      999999 "K" "V"))))))
      (should (eq 'invalid-target (plist-get d :reason)))
      (should (eq 'missing (plist-get d :detail)))))
  (dorg-test-mutate--with-dolt
    (let ((root (dorg-test-mutate--seed dolt-dir "* Old\n")))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-test-mutate--apply-patch-batch
                    (list :dolt-dir dolt-dir :root-id root)
                    (list (dorg-test-mutate--heading-update-patch
                           999999 :title "x")))))))))

;;; ─── Public text selectors end-to-end (tracked-regression) ────────────────
;; Compound text selectors driven through the PUBLIC command
;; envelope `dorg-api-execute', executed on the real plain-org and Dolt
;; backends.  These pin the AC: the prose lands AND every non-text item
;; (planning, the user PROPERTIES drawer holding DORG_ID, other drawers,
;; keywords) plus sibling headings are PRESERVED — the whole reason the
;; execution is a collection insert or strict first-text replacement and NEVER
;; whole `replace :items' (which would discard the drawer and break identity).

(ert-deftest dorg-test-2rx9n-api-exec-insert-text-plain-org-preserves ()
  "Text insertion preserves prior prose, the DORG_ID
PROPERTIES drawer, and the sibling heading."
  (dorg-test-mutate--with-buffer buf
    (dorg-api-execute '(on (id 1001)
                            (insert (text) (after-last) "Appended note."))
                      :mode 'apply :source buf)
    (let ((text (buffer-string)))
      (should (string-match-p "First body\\." text))     ; prior prose kept
      (should (string-match-p "Appended note\\." text))  ; new prose appended
      (should (string-match-p ":DORG_ID: 1001" text))    ; identity drawer kept
      (should (string-match-p "\\* Second" text))        ; sibling kept
      (should (string-match-p "Second body\\." text)))))

(ert-deftest dorg-test-2rx9n-api-exec-replace-text-plain-org-preserves ()
  "Text occurrence replacement changes the first prose item IN PLACE, preserving a
planning line, a USER property (CATEGORY), and the sibling heading.
NON-VACUOUS: the CATEGORY property and SCHEDULED planning are dropped by
whole `replace :items' but kept by strict text replacement — DORG_ID alone is
render-injected from the heading id and would survive either way, so it is
NOT a sufficient witness."
  (let* ((tmp (make-temp-file
               "dorg-body-preserve" nil ".org"
               (concat "* Task\n"
                       "SCHEDULED: <2026-07-01 Wed>\n"
                       ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                       "First body.\n\n"
                       "* Sibling\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\n"
                       "Sibling body.\n")))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute '(on (id 1001)
                                  (replace (text (occurrence 1))
                                           "Rewritten body."))
                            :mode 'apply :source buf)
          (let ((text (buffer-string)))
            (should (string-match-p "Rewritten body\\." text))   ; new prose present
            (should-not (string-match-p "First body\\." text))   ; old prose replaced
            (should (string-match-p ":CATEGORY: work" text))     ; USER prop PRESERVED
            (should (string-match-p "SCHEDULED: <2026-07-01" text)) ; planning PRESERVED
            (should (string-match-p ":DORG_ID: 1001" text))      ; identity intact
            (should (string-match-p "\\* Sibling" text))         ; sibling PRESERVED
            (should (string-match-p "Sibling body\\." text))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-2rx9n-api-exec-replace-text-no-prose-is-missing ()
  "Text occurrence replacement is strict when a heading has only metadata.
The missing occurrence signals `invalid-target' and leaves bytes untouched; use
explicit text insertion to create prose.
Preservation: no metadata is touched on the error path."
  (let* ((tmp (make-temp-file "dorg-noprose" nil ".org"
                              "* Only\n:PROPERTIES:\n:DORG_ID: 2001\n:END:\n"))
         (b (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer b
          (let ((org-inhibit-startup t)) (org-mode))
          (let ((before (buffer-string)))
            (should (eq 'invalid-target
                        (condition-case err
                            (progn (dorg-api-execute
                                    '(on (id 2001)
                                         (replace (text (occurrence 1)) "x"))
                                    :mode 'apply :source b)
                                   :no-error)
                          (dorg-api-error (dorg-api-error-reason err))
                          (dorg-mutate-error (dorg-mutate-error-reason err)))))
            (should (equal before (buffer-string)))))
      (with-current-buffer b (set-buffer-modified-p nil))
      (kill-buffer b)
      (delete-file tmp))))

(defun dorg-test-api--dolt-seed-rich (dolt-dir)
  "Seed DOLT-DIR (root 1000) with a Task heading (id 3001) carrying a planning
line, a user CATEGORY property, and prose — the rich preservation fixture."
  (dorg-sql-batch dolt-dir
                  (dorg-test--parse-to-sql
                   (concat "* Task\n"
                           "SCHEDULED: <2026-07-01 Wed>\n"
                           ":PROPERTIES:\n:DORG_ID: 3001\n:CATEGORY: work\n:END:\n"
                           "Original body.\n"
                           ":LOGBOOK:\n- old\n:END:\n")
                   1000)
                  "seed")
  (dorg-commit dolt-dir "seed"))

(defun dorg-test-api--heading-texts (h)
  "Return the list of text strings of H's `dorg-text-item' items."
  (mapcar #'dorg-text-item-text
          (cl-remove-if-not (lambda (i) (cl-typep i 'dorg-text-item))
                            (dorg-heading-items h))))

(defun dorg-test-api--heading-category (h)
  "Return H's user CATEGORY property from its PROPERTIES drawer, or nil."
  (cl-some (lambda (i)
             (and (cl-typep i 'dorg-drawer-item)
                  (equal (dorg-drawer-item-name i) "PROPERTIES")
                  (cdr (assoc "CATEGORY" (dorg-drawer-item-properties i)))))
           (dorg-heading-items h)))

(defun dorg-test-api--heading-scheduled-p (h)
  "Return non-nil when H has a planning item carrying SCHEDULED."
  (cl-some (lambda (i)
             (and (cl-typep i 'dorg-planning-item)
                  (dorg-planning-item-scheduled i)))
           (dorg-heading-items h)))

(ert-deftest dorg-test-2rx9n-api-exec-replace-text-dolt-preserves ()
  "Text occurrence replacement on Dolt preserves metadata and trailing drawer.
user CATEGORY property, the planning line, and identity — plain-org/Dolt
parity.  NON-VACUOUS: CATEGORY/planning are dropped by whole `replace :items'."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (dorg-api-execute '(on (id 3001)
                            (replace (text (occurrence 1))
                                     "Rewritten via API."))
                      :mode 'apply :source (list :dolt-dir dolt-dir :root-id 1000))
    (let* ((h (gethash 3001 (dorg-model-load-headings dolt-dir 1000 nil)))
           (texts (dorg-test-api--heading-texts h)))
      (should (cl-some (lambda (s) (string-match-p "Rewritten via API" s)) texts))
      (should-not (cl-some (lambda (s) (string-match-p "Original body" s)) texts))
      (should (equal "work" (dorg-test-api--heading-category h)))   ; USER prop kept
      (should (dorg-test-api--heading-scheduled-p h))               ; planning kept
      (should (cl-find-if (lambda (item)
                            (and (cl-typep item 'dorg-drawer-item)
                                 (equal "LOGBOOK"
                                        (dorg-drawer-item-name item))))
                          (dorg-heading-items h)))
      (should (= 3001 (dorg-heading-id h))))))                      ; identity kept

(ert-deftest dorg-test-2rx9n-api-exec-insert-text-dolt-target-relative ()
  "Dolt inserts after the last text and before a trailing LOGBOOK drawer."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (dorg-api-execute '(on (id 3001)
                            (insert (text) (after-last) "Appended via API."))
                      :mode 'apply :source (list :dolt-dir dolt-dir :root-id 1000))
    (let* ((h (gethash 3001 (dorg-model-load-headings dolt-dir 1000 nil)))
           (items (dorg-heading-items h))
           (texts (dorg-test-api--heading-texts h))
           (new-index (cl-position-if
                       (lambda (item)
                         (and (cl-typep item 'dorg-text-item)
                              (string-match-p "Appended via API"
                                              (dorg-text-item-text item))))
                       items))
           (logbook-index (cl-position-if
                           (lambda (item)
                             (and (cl-typep item 'dorg-drawer-item)
                                  (equal "LOGBOOK"
                                         (dorg-drawer-item-name item))))
                           items)))
      (should (cl-some (lambda (s) (string-match-p "Original body" s)) texts)) ; prior kept
      (should (cl-some (lambda (s) (string-match-p "Appended via API" s)) texts)) ; appended
      (should (integerp new-index))
      (should (integerp logbook-index))
      (should (< new-index logbook-index))
      (should (equal "work" (dorg-test-api--heading-category h)))   ; USER prop kept
      (should (dorg-test-api--heading-scheduled-p h))               ; planning kept
      (should (= 3001 (dorg-heading-id h))))))

(ert-deftest dorg-test-2rx9n-api-exec-insert-text-plain-org-target-relative ()
  "Plain Org transforms text+LOGBOOK to text+new-text+LOGBOOK byte-surgically."
  (let* ((before (concat "* Task\n"
                         ":PROPERTIES:\n:DORG_ID: 4101\n:CATEGORY: work\n:END:\n"
                         "Old prose.\n"
                         ":LOGBOOK:\n- old\n:END:\n"
                         "* Sibling\n:PROPERTIES:\n:DORG_ID: 4102\n:END:\n"
                         "Sibling prose.\n"))
         (file (make-temp-file "dorg-text-target-relative" nil ".org" before))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(on (id 4101) (insert (text) (after-last) "New prose."))
           :mode 'apply :source buf)
          (let ((after (buffer-string)))
            (should-not (equal before after))
            (should (string-match-p
                     "Old prose\\.\n\nNew prose\\.\n\n:LOGBOOK:\n- old\n:END:"
                     after))
            (should (string-match-p ":CATEGORY: work" after))
            (should (string-match-p
                     "\\* Sibling\n:PROPERTIES:\n:DORG_ID: 4102\n:END:\nSibling prose\\."
                     after))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file file))))

;;; ─── Public drawer-line front insertion (tracked-regression) ───────────

(ert-deftest dorg-test-bnsmz-exec-insert-existing-first-drawer ()
  "Sequential front insertions target occurrence zero and preserve all bytes.
NON-VACUOUS: deleting either public action leaves the expected A/B prefix
absent or incomplete."
  (let* ((before (concat "* Task\n"
                         "SCHEDULED: <2026-08-14 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                         ":AUDIT:\n- old first\n:END:\n"
                         "Body.\n"
                         ":AUDIT:\n- old second\n:END:\n"))
         (after (concat "* Task\n"
                        "SCHEDULED: <2026-08-14 Fri>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                        ":AUDIT:\n- A\n- B\n- old first\n:END:\n"
                        "Body.\n"
                        ":AUDIT:\n- old second\n:END:\n"))
         (tmp (make-temp-file "dorg-bnsmz-existing" nil ".org" before))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(batch
             (on (id 1001)
                 (insert (drawer-line "AUDIT" (occurrence 1)) (before-first)
                         "- A"))
             (on (id 1001)
                 (insert (drawer-line "AUDIT" (occurrence 1)) (before-first)
                         "- B")))
           :mode 'apply :source buf)
          (should (equal after (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-bnsmz-exec-identical-inserts-are-not-deduplicated ()
  "Two identical front insertions add two adjacent identical lines."
  (let* ((before (concat "* Task\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                         ":LOGBOOK:\n- old\n:END:\n"))
         (after (concat "* Task\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                        ":LOGBOOK:\n- x\n- x\n- old\n:END:\n"))
         (tmp (make-temp-file "dorg-bnsmz-duplicates" nil ".org" before))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(batch
             (on (id 1001)
                 (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first)
                         "- x"))
             (on (id 1001)
                 (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first)
                         "- x")))
           :mode 'apply :source buf)
          (should (equal after (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-9diah-exec-insert-existing-drawer-after-last ()
  "Sequential after-last insertions append to the first existing drawer."
  (let* ((before (concat "* Task\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                         ":AUDIT:\n- old first\n:END:\n"
                         "Body.\n"
                         ":AUDIT:\n- old second\n:END:\n"))
         (after (concat "* Task\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                        ":AUDIT:\n- old first\n- A\n- B\n:END:\n"
                        "Body.\n"
                        ":AUDIT:\n- old second\n:END:\n"))
         (tmp (make-temp-file "dorg-9diah-after-last" nil ".org" before))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(batch
             (on (id 1001)
                 (insert (drawer-line "AUDIT" (occurrence 1)) (after-last)
                         "- A"))
             (on (id 1001)
                 (insert (drawer-line "AUDIT" (occurrence 1)) (after-last)
                         "- B")))
           :mode 'apply :source buf)
          (should (equal after (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-bnsmz-exec-insert-creates-after-metadata ()
  "A missing drawer is created after planning/PROPERTIES and before body."
  (let* ((before (concat "* Task\n"
                         "SCHEDULED: <2026-08-14 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                         "Body.\n"))
         (after (concat "* Task\n"
                        "SCHEDULED: <2026-08-14 Fri>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                        ":LOGBOOK:\n- created\n:END:\n"
                        "Body.\n"))
         (tmp (make-temp-file "dorg-bnsmz-missing" nil ".org" before))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(on (id 1001)
                (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first)
                        "- created"))
           :mode 'apply :source buf)
          (should (equal after (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-9diah-exec-insert-missing-drawer-after-last-creates ()
  "After-last on a missing drawer creates the first drawer with one line."
  (let* ((before (concat "* Task\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                         "Body.\n"))
         (after (concat "* Task\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                        ":AUDIT:\n- created\n:END:\n"
                        "Body.\n"))
         (tmp (make-temp-file "dorg-9diah-missing-after" nil ".org" before))
         (buf (find-file-noselect tmp)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-inhibit-startup t)) (org-mode))
          (dorg-api-execute
           '(on (id 1001)
                (insert (drawer-line "AUDIT" (occurrence 1)) (after-last)
                        "- created"))
           :mode 'apply :source buf)
          (should (equal after (buffer-string))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file tmp))))

(ert-deftest dorg-test-9diah-exec-missing-drawer-positions-create-identical-bytes ()
  "Missing-drawer before-first and after-last both create identical bytes."
  (let* ((before (concat "* Task\n"
                         "SCHEDULED: <2026-08-14 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                         "Body.\n"))
         (expected (concat "* Task\n"
                           "SCHEDULED: <2026-08-14 Fri>\n"
                           ":PROPERTIES:\n:DORG_ID: 1001\n:CATEGORY: work\n:END:\n"
                           ":AUDIT:\n- created\n:END:\n"
                           "Body.\n")))
    (cl-labels
        ((mutated (position)
           (dorg-test-mutate--with-content buf before
             (dorg-test-mutate--assert-dorg-policy buf)
             (dorg-api-execute
              `(on (id 1001)
                   (insert (drawer-line "AUDIT" (occurrence 1))
                           ,(list position)
                           "- created"))
              :mode 'apply :source buf)
             (buffer-string))))
      (let ((front (mutated 'before-first))
            (back (mutated 'after-last)))
        (should (equal expected front))
        (should (equal expected back))
        (should (equal front back))))))

(defun dorg-test-bnsmz--drawer-lines (heading name)
  "Return raw lines from HEADING's first exact-name drawer NAME, or nil."
  (when-let ((drawer
              (cl-find-if
               (lambda (item)
                 (and (cl-typep item 'dorg-drawer-item)
                      (null (dorg-drawer-item-properties item))
                      (equal name (dorg-drawer-item-name item))))
               (dorg-heading-items heading))))
    (dorg-drawer-item-lines drawer)))

(defun dorg-test-9diah--render-loaded-tree (dolt-dir root)
  "Render loaded DOLT-DIR ROOT model bytes without revision metadata."
  (let (headings)
    (maphash (lambda (_ h) (push h headings))
             (dorg-model-load-tree dolt-dir root))
    (dorg-model-render-tree headings)))

(defun dorg-test-9diah--dolt-missing-drawer-create-render (position)
  "Apply missing-drawer create at POSITION and return rendered model bytes."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* Task\n"
                          "SCHEDULED: <2026-08-14 Fri>\n"
                          ":PROPERTIES:\n:DORG_ID: 3001\n:CATEGORY: work\n:END:\n"
                          "Body.\n")
                  1000))
           (source (list :dolt-dir dolt-dir :root-id root)))
      (dorg-api-execute
       `(on (id 3001)
            (insert (drawer-line "AUDIT" (occurrence 1))
                    ,(list position)
                    "- created"))
       :mode 'apply :source source)
      (dorg-test-9diah--render-loaded-tree dolt-dir root))))

(ert-deftest dorg-test-9diah-dolt-missing-drawer-positions-create-identical-bytes ()
  "Dolt missing-drawer before-first and after-last create identical bytes."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (let* ((expected (concat "* Task\n"
                           "SCHEDULED: <2026-08-14 Fri>\n"
                           ":PROPERTIES:\n:DORG_ID: 3001\n:CATEGORY: work\n:END:\n"
                           ":AUDIT:\n- created\n:END:\n"
                           "Body.\n"))
         (front (dorg-test-9diah--dolt-missing-drawer-create-render
                 'before-first))
         (back (dorg-test-9diah--dolt-missing-drawer-create-render
                'after-last)))
    (should (equal expected front))
    (should (equal expected back))
    (should (equal front back))))

(ert-deftest dorg-test-9diah-dolt-before-first-survives-planning-interleave ()
  "Dolt planning insertion cannot stale a same-slot text cursor."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  "* Task\nold\n"
                  1000))
           (id (dorg-test-mutate--id-by-title dolt-dir "Task"))
           (source (list :dolt-dir dolt-dir :root-id root)))
      (dorg-api-execute
       `(batch
         (on (id ,id) (insert (text) (before-first) "A"))
         (on (id ,id) (set (planning scheduled) "<2026-09-02 Wed>"))
         (on (id ,id) (insert (text) (before-first) "B")))
       :mode 'apply :source source)
      (should
       (equal '((planning "<2026-09-02 Wed>" nil nil)
                (text "A") (text "B") (text "old\n"))
              (dorg-test-mutate--items-shape
               (dorg-heading-items
                (gethash id (dorg-model-load-tree dolt-dir root)))))))))

(ert-deftest dorg-test-9diah-dolt-before-first-survives-property-drawer-create ()
  "Dolt property drawer creation cannot stale a same-slot text cursor."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  "* Task\nSCHEDULED: <2026-09-01 Tue>\nold\n"
                  1000))
           (id (dorg-test-mutate--id-by-title dolt-dir "Task"))
           (source (list :dolt-dir dolt-dir :root-id root)))
      (dorg-api-execute
       `(batch
         (on (id ,id) (insert (text) (before-first) "A"))
         (on (id ,id) (set (property "CATEGORY") "work"))
         (on (id ,id) (insert (text) (before-first) "B")))
       :mode 'apply :source source)
      (let* ((items (dorg-heading-items
                     (gethash id (dorg-model-load-tree dolt-dir root))))
             (shape (dorg-test-mutate--items-shape items))
             (property-index
              (cl-position-if
               (lambda (item)
                 (and (cl-typep item 'dorg-drawer-item)
                      (equal "PROPERTIES" (dorg-drawer-item-name item))))
               items))
             (first-text-index
              (cl-position-if
               (lambda (item) (cl-typep item 'dorg-text-item))
               items)))
        (should (integerp property-index))
        (should (integerp first-text-index))
        (should (< property-index first-text-index))
        (should
         (equal '((planning "<2026-09-01 Tue>" nil nil)
                  (drawer "PROPERTIES")
                  (text "A") (text "B") (text "old\n"))
                shape))))))

(ert-deftest dorg-test-bnsmz-dolt-dry-run-cas-apply-replay ()
  "Dolt dry-run is read-only, stale CAS refuses, fresh apply updates the drawer,
and same-op-id replay is idempotent.  Planning and user properties survive."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* Task\n"
                          "SCHEDULED: <2026-08-14 Fri>\n"
                          ":PROPERTIES:\n:DORG_ID: 3001\n:CATEGORY: work\n:END:\n"
                          "Body.\n")
                  1000))
           (source (list :dolt-dir dolt-dir :root-id root))
           (form '(on (id 3001)
                      (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first) "- created")))
           (head0 (dorg-head-commit dolt-dir))
           (preview0 (dorg-api-execute form :mode 'dry-run :source source)))
      (should (equal head0 (plist-get preview0 :base)))
      (should (equal head0 (dorg-head-commit dolt-dir)))
      ;; Advance HEAD independently, then prove the captured drawer plan cannot
      ;; write through stale CAS.
      (dorg-api-execute '(on (id 3001) (set (property "DRIFT") "yes"))
                        :mode 'apply :source source)
      (let ((head1 (dorg-head-commit dolt-dir)))
        (should-not (equal head0 head1))
        (should
         (eq 'overlap
             (condition-case err
                 (progn
                   (dorg-api-execute form :mode 'apply :source source :base head0)
                   :no-error)
               (dorg-mutate-error (dorg-mutate-error-reason err)))))
        (should (equal head1 (dorg-head-commit dolt-dir)))
        (should
         (null
          (dorg-test-bnsmz--drawer-lines
           (gethash 3001 (dorg-model-load-tree dolt-dir root)) "LOGBOOK")))
        (let* ((preview1 (dorg-api-execute form :mode 'dry-run :source source))
               (result (dorg-api-execute form :mode 'apply :source source
                                         :base (plist-get preview1 :base)
                                         :op-id "bnsmz-create"))
               (commit (dorg-head-commit dolt-dir))
               (heading (gethash 3001 (dorg-model-load-tree dolt-dir root))))
          (should (plist-get result :ok))
          (should (member 3001 (plist-get (plist-get result :affected) :changed)))
          (should (equal '("- created")
                         (dorg-test-bnsmz--drawer-lines heading "LOGBOOK")))
          (should (equal "work" (dorg-test-api--heading-category heading)))
          (should (dorg-test-api--heading-scheduled-p heading))
          (let ((replay (dorg-api-execute form :mode 'apply :source source
                                          :op-id "bnsmz-create")))
            (should (plist-get replay :idempotent))
            (should (equal commit (dorg-head-commit dolt-dir)))
            (should
             (equal '("- created")
                    (dorg-test-bnsmz--drawer-lines
                     (gethash 3001 (dorg-model-load-tree dolt-dir root))
                     "LOGBOOK"))))
          ;; Reusing the evidence key for different drawer bytes is a terminal
          ;; mismatch, not an idempotent success or a second insertion.
          (should
           (eq 'idempotency-key-mismatch
               (condition-case err
                   (progn
                     (dorg-api-execute
                      '(on (id 3001)
                           (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first) "- different"))
                      :mode 'apply :source source :op-id "bnsmz-create")
                     :no-error)
                 (dorg-api-error (dorg-api-error-reason err))
                 (dorg-mutate-error (dorg-mutate-error-reason err)))))
          (should (equal commit (dorg-head-commit dolt-dir)))
          (should
           (equal '("- created")
                  (dorg-test-bnsmz--drawer-lines
                   (gethash 3001 (dorg-model-load-tree dolt-dir root))
                   "LOGBOOK"))))))))

(ert-deftest dorg-test-bnsmz-dolt-batch-fault-rolls-back ()
  "A valid drawer insertion before a missing-target op leaves HEAD and rows
unchanged; the batch is validated/simulated atomically before SQL."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Task\n"))
           (id (dorg-test-mutate--id-by-title dolt-dir "Task"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (head (dorg-head-commit dolt-dir))
           (form `(batch
                   (on (id ,id)
                       (insert (drawer-line "AUDIT" (occurrence 1)) (before-first) "- must roll back"))
                   (on (id 999999) (set (property "K") "V")))))
      (should
       (eq 'invalid-target
           (condition-case err
               (progn (dorg-api-execute form :mode 'apply :source source)
                      :no-error)
             (dorg-api-error (dorg-api-error-reason err))
             (dorg-mutate-error (dorg-mutate-error-reason err)))))
      (should (equal head (dorg-head-commit dolt-dir)))
      (should-not
       (member "- must roll back"
               (dorg-test-bnsmz--drawer-lines
                (gethash id (dorg-model-load-tree dolt-dir root)) "AUDIT"))))))

;;; ─── Native typed-patch engine boundary ───────────────────────────────
;; Typed patches are the engine's sole semantic mutation interface.  These
;; tests cover the private boundary directly where the behavior is engine-only.

(ert-deftest dorg-test-mutate-apply-patches-plain-org ()
  "A typed property-put patch applies directly to a live plain-org buffer."
  (dorg-test-mutate--with-buffer buf
    (dorg-test-mutate--assert-dorg-policy buf)
    (should-not (string-match-p ":CATEGORY:" (buffer-string)))   ; absent before
    (let* ((patches (dorg-api-normalize
                     '(on (id 1001) (set (property "CATEGORY") "work")) buf))
           (res (dorg-mutate--apply-patches buf patches)))
      (should (plist-get res :ok))
      (should-not (plist-get res :dry-run))
      (should (member 1001 (plist-get res :changed)))
      (should (string-match-p ":CATEGORY: +work" (buffer-string))))))  ; present after

(ert-deftest dorg-test-mutate-plan-patches-plain-org ()
  "`dorg-mutate--plan-patches' is IR-native and NON-mutating: it returns a
supported plan carrying the exact typed patches and leaves the buffer
byte-identical."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (patches (dorg-api-normalize
                     '(on (id 1001) (set (property "CATEGORY") "work")) buf))
           (plan (dorg-mutate--plan-patches buf patches)))
      (should (plist-get plan :ok))
      (should (plist-get plan :dry-run))
      (should (plist-get plan :supported))
      (should (equal (plist-get plan :patches) patches))
      (should (plist-member plan :patch-support))
      (should-not (plist-member plan :ops))
      (should (equal (buffer-string) before)))))              ; dry-run mutated nothing

(ert-deftest dorg-test-mutate-apply-patches-gate-identity-none ()
  "The IR-native boundary enforces the SAME read-only identity gate as the
engine path: a non-empty typed batch against a `none'-identity source signals
`unsupported' / `:detail identity-none' before any write (parity with
`dorg-mutate--gate-identity-none').

The `none' policy is carried by an EXPLICIT source descriptor `(:buffer BUF
:identity none)' — a bare buffer resolves to the default `dorg' policy, so the
descriptor is what actually exercises the read-only gate.  The SAME descriptor
drives normalization and application, and the source buffer stays byte-identical
because the gate fires before any write."
  (dorg-test-mutate--with-content buf "* Plain\nNo identity.\n"
    (let ((src (list :buffer buf :identity 'none))
          (before (buffer-string)))
      ;; precondition: the descriptor genuinely resolves to the `none' policy.
      (should (eq 'none (dorg-mutate--source-identity
                         (dorg-mutate--resolve-source src))))
      (let* ((patches (dorg-api-normalize
                       '(on (id 1) (set (property "P") "1")) src))
             (data (dorg-test-mutate--error-data
                    (dorg-mutate--apply-patches src patches))))
        (should (eq 'unsupported (plist-get data :reason)))
        (should (eq 'identity-none (plist-get data :detail)))
        (should (equal (buffer-string) before))))))       ; source unchanged

(ert-deftest dorg-test-mutate-boundary-parity-plain-org ()
  "Private typed execution and the public API yield byte-identical plain Org.
The sample carries no CATEGORY, so before differs from after on both paths."
  (let ((form '(on (id 1001) (set (property "CATEGORY") "work"))))
    (should-not (string-match-p ":CATEGORY:" dorg-test-mutate--sample)) ; absent before
    (dorg-test-mutate--with-buffer buf-ir
      (dorg-mutate--apply-patches buf-ir (dorg-api-normalize form buf-ir))
      (let ((ir-after (buffer-string)))
        (dorg-test-mutate--with-buffer buf-public
          (dorg-api-execute form :mode 'apply :source buf-public)
          (should (string-match-p ":CATEGORY: +work" ir-after))       ; change real
          (should (equal ir-after (buffer-string))))))))              ; private == public

(defun dorg-test-mutate--ir-surface (patches)
  "Return the backend-INDEPENDENT semantic surface of PATCHES.
Strips the heading-host id (the backend-only identity anchor: a plain-org
DORG_ID vs a Dolt snowflake) so two normalizations that address the SAME logical
heading on different backends compare `equal'.  The surface keeps :op, the
target KIND, the target :name, and :value — the semantic content of the patch."
  (mapcar (lambda (p)
            (let ((tg (plist-get p :target)))
              (list :op (plist-get p :op)
                    :target-kind (car tg)
                    :name (dorg-api--tget tg :name)
                    :value (plist-get p :value))))
          patches))

(ert-deftest dorg-test-mutate-boundary-parity-dolt ()
  "AC-3 parity: the SAME typed patch IR applied through the IR-native boundary
yields the SAME semantic heading outcome on the Dolt executor and on the
plain-org executor.  A `put :property' (CATEGORY) is normalized+applied on both a
Dolt source and a plain-org buffer; after normalizing backend-only metadata (the
id anchor, via `dorg-test-mutate--ir-surface') the two typed IR surfaces are
`equal', and the resulting heading model (title + user CATEGORY) matches across
backends.  Transformation test: CATEGORY is absent before and present after on
BOTH paths.  Requires Dolt; skipped under bounded static evidence, run at ACCEPT."
  (dorg-test-mutate--with-dolt
    ;; ── plain-org arm: same put on a live plain-org buffer ──
    (dorg-test-mutate--with-content pbuf
        "* Task\n:PROPERTIES:\n:DORG_ID: 4242\n:END:\n"
      (let* ((pbefore (dorg-test-api--heading-category
                       (cl-find "Task" (dorg-parse-result-headings
                                        (dorg-parse-buffer pbuf))
                                :key #'dorg-heading-title :test #'equal)))
             (ppatches (dorg-api-normalize
                        '(on (id 4242) (set (property "CATEGORY") "work")) pbuf))
             (pres (dorg-mutate--apply-patches pbuf ppatches))
             (pheading (cl-find "Task" (dorg-parse-result-headings
                                        (dorg-parse-buffer pbuf))
                                :key #'dorg-heading-title :test #'equal))
             ;; ── Dolt arm: the SAME put on a Dolt source ──
             (root (dorg-test-mutate--seed dolt-dir "* Task\n"))
             (cid (dorg-test-mutate--id-by-title dolt-dir "Task"))
             (base (dorg-head-commit dolt-dir))
             (src (list :dolt-dir dolt-dir :root-id root))
             (dbefore (dorg-test-api--heading-category
                       (gethash cid (dorg-model-load-tree dolt-dir root))))
             (dpatches (dorg-api-normalize
                        (list 'on (list 'id cid) '(set (property "CATEGORY") "work")) src))
             (dres (dorg-mutate--apply-patches src dpatches))
             (dheading (gethash cid (dorg-model-load-tree dolt-dir root))))
        ;; both boundary calls succeeded on their respective backends
        (should (plist-get pres :ok))
        (should (plist-get dres :ok))
        (should (member 4242 (plist-get pres :changed)))
        (should (member cid (plist-get dres :changed)))
        (should-not (equal base (dorg-head-commit dolt-dir)))        ; Dolt HEAD advanced
        ;; SAME typed IR surface once the backend id anchor is normalized away
        (should (equal (dorg-test-mutate--ir-surface ppatches)
                       (dorg-test-mutate--ir-surface dpatches)))
        ;; transformation: CATEGORY absent before, present after, on BOTH paths
        (should (null pbefore))
        (should (null dbefore))
        ;; SAME semantic outcome across backends: (title . CATEGORY)
        (should (equal (cons (dorg-heading-title pheading)
                             (dorg-test-api--heading-category pheading))
                       (cons (dorg-heading-title dheading)
                             (dorg-test-api--heading-category dheading))))
        (should (equal "work" (dorg-test-api--heading-category pheading)))))))

;;; ─── Intrinsic COMMENTED heading field (tracked-regression) ────────────────

(defconst dorg-test-c8ynt--rich-heading
  (concat
   "* TODO [#A] Task  :alpha:beta:\n"
   "SCHEDULED: <2026-08-19 Wed>\n"
   ":PROPERTIES:\n:DORG_ID: 9101\n:BEAD_ID: task-9101\n:CATEGORY: work\n:END:\n"
   "Original body.\n"
   ":LOGBOOK:\n- State TODO\n:END:\n"
   "** Child\n:PROPERTIES:\n:DORG_ID: 9102\n:END:\nChild body.\n"
   "* Sibling\n:PROPERTIES:\n:DORG_ID: 9103\n:END:\nSibling body.\n")
  "Rich COMMENTED fixture whose non-comment state must stay unchanged.")

(defun dorg-test-c8ynt--heading-state (heading)
  "Return every semantic HEADING facet except the commented boolean."
  (list :id (dorg-heading-id heading)
        :parent-id (dorg-heading-parent-id heading)
        :sort-key (dorg-heading-sort-key heading)
        :level (dorg-heading-level heading)
        :todo (dorg-heading-todo heading)
        :priority (dorg-heading-priority heading)
        :title (dorg-heading-title heading)
        :post-blank (dorg-heading-post-blank heading)
        :tags (dorg-heading-tags heading)
        :items-hash (dorg-heading-items-hash heading)
        :items (dorg-test-mutate--items-shape
                (dorg-heading-items heading))))

(defun dorg-test-c8ynt--tree-state (tree)
  "Return TREE as sorted id/state pairs, excluding only commented booleans."
  (let (out)
    (maphash (lambda (id heading)
               (push (cons id (dorg-test-c8ynt--heading-state heading)) out))
             tree)
    (sort out (lambda (a b) (< (car a) (car b))))))

(defun dorg-test-c8ynt--error (thunk)
  "Call THUNK and return its mutate error reason and message."
  (condition-case err
      (progn (funcall thunk) :no-error)
    (dorg-mutate-error
     (list (dorg-mutate-error-reason err) (cadr err)))))

(ert-deftest dorg-test-3yo52-c3-refusal-shape-is-single-source ()
  "The shared C3 builder owns whole-verb and root-host refusal shapes."
  (let ((planning (dorg-test-mutate--planning-patch
                   1001 :scheduled "<2026-08-19 Wed>"))
        (root-property '(:op put
                         :target (:property :host :root :name "K")
                         :value "V")))
    (should
     (equal '(:unsupported t
              :reason no-source-preserving-editor
              :message "no source-preserving editor for (update :planning)"
              :operation update
              :target-kind :planning
              :variant nil)
            (dorg-mutate--c3-refusal planning)))
    (should
     (equal '(:unsupported t
              :reason no-source-preserving-editor
              :message "no source-preserving editor for (put :property) at the level-0 root"
              :operation put
              :target-kind :property
              :variant (:host root))
            (dorg-mutate--c3-refusal root-property)))))

(ert-deftest dorg-test-3yo52-c3-refusal-builder-drives-plan-and-apply ()
  "Plan and apply consume the same complete C3 refusal record."
  (let* ((source (dorg-mutate--source-make
                  :kind 'plain-org :file "/tmp/tracked-case.org"
                  :identity 'dorg))
         (patch (dorg-test-mutate--planning-patch
                 1001 :scheduled "<2026-08-19 Wed>"))
         (sentinel '(:unsupported t
                     :reason sentinel-reason
                     :message "sentinel shared refusal"
                     :operation sentinel-operation
                     :target-kind :sentinel-target
                     :variant (:sentinel t)))
         (calls 0))
    (cl-letf (((symbol-function 'dorg-mutate--c3-refusal)
               (lambda (actual)
                 (should (eq actual patch))
                 (setq calls (1+ calls))
                 sentinel)))
      (should (equal sentinel
                     (dorg-mutate--executor-patch-support source patch)))
      (should (= calls 1))
      (let* ((err (condition-case actual
                      (progn
                        (dorg-mutate--source-preserving-validate-patch patch)
                        (ert-fail "expected dorg-mutate-error, none signalled"))
                    (dorg-mutate-error actual)))
             (data (dorg-mutate-error-data err)))
        (should (= calls 2))
        (should (eq 'unsupported (plist-get data :reason)))
        (should (eq 'sentinel-reason (plist-get data :detail)))
        (should (eq 'sentinel-operation (plist-get data :operation)))
        (should (eq :sentinel-target (plist-get data :target-kind)))
        (should (equal '(:sentinel t) (plist-get data :variant)))
        (should (string-match-p "sentinel shared refusal"
                                (cadr err)))))))

(defun dorg-test-5oc7v--assert-kind-refusal (form)
  "Assert that evaluating FORM refuses one unknown source kind."
  (let ((data (dorg-test-mutate--error-data (funcall form))))
    (should (eq 'unsupported (plist-get data :reason)))
    (should (eq 'unknown-source-kind (plist-get data :detail)))
    (should (eq 'bogus (plist-get data :source-kind)))))

(ert-deftest dorg-test-5oc7v-all-source-ingresses-share-one-kind-guard ()
  "Registration, resolved sources, and move destinations share one guard."
  (let* ((reg (make-dorg-registration
               :backend (dorg-backend-plain-org)
               :file "/tmp/tracked-case.org"))
         (bogus (dorg-mutate--source-make :kind 'bogus :identity 'dorg))
         (move (dorg-test-mutate--move-patch
                1 :root :to-source bogus))
         seen)
    (cl-letf (((symbol-function 'dorg-mutate--assert-source-kind)
               (lambda (kind)
                 (push kind seen)
                 kind)))
      (dorg-mutate--source-from-registration reg nil)
      (should (eq bogus (dorg-mutate--resolve-source bogus)))
      (should (eq move (dorg-mutate--validate-patch move 0))))
    (should (equal '(plain-org bogus bogus) (nreverse seen)))))

(ert-deftest dorg-test-5oc7v-unknown-registration-kind-refuses-at-ingress ()
  "An unknown registration backend is one typed source-kind refusal."
  (let ((reg (make-dorg-registration
              :backend (dorg-backend--make :type 'bogus :options nil))))
    (dorg-test-5oc7v--assert-kind-refusal
     (lambda () (dorg-mutate--source-from-registration reg nil)))))

(ert-deftest dorg-test-5oc7v-resolved-empty-batch-refuses-at-ingress ()
  "A bogus resolved source cannot produce a vacuously supported empty plan."
  (let ((bogus (dorg-mutate--source-make :kind 'bogus :identity 'dorg)))
    (dorg-test-5oc7v--assert-kind-refusal
     (lambda ()
       (dorg-mutate--plan-patches
        bogus nil nil :base-revision :synthetic)))))

(ert-deftest dorg-test-5oc7v-unknown-move-destination-refuses-at-ingress ()
  "A resolved move destination must belong to the closed source universe."
  (let* ((bogus (dorg-mutate--source-make :kind 'bogus :identity 'dorg))
         (move (dorg-test-mutate--move-patch
                1 :root :to-source bogus)))
    (dorg-test-5oc7v--assert-kind-refusal
     (lambda () (dorg-mutate--validate-patch move 0)))))

(ert-deftest dorg-test-5oc7v-private-dispatch-bypass-fails-as-invariant ()
  "An impossible kind at private dispatch errors; it never nil-returns."
  (let ((bogus (dorg-mutate--source-make :kind 'bogus :identity 'dorg))
        (plan '(:ok t :dry-run t :supported t :unsupported nil)))
    (condition-case err
        (progn
          (dorg-mutate--execute-patches bogus nil nil plan)
          (ert-fail "impossible source kind returned from private dispatch"))
      (error
       (should (eq 'error (car err)))
       (should (string-match-p "cl-ecase failed" (cadr err)))))))

(ert-deftest dorg-test-5oc7v-plan-omits-obsolete-executor-capability ()
  "Plans expose support facts, not an always-true executor capability bit."
  (let* ((source (dorg-mutate--source-make
                  :kind 'plain-org :file "/tmp/tracked-case.org"
                  :identity 'dorg))
         (plan (dorg-mutate--plan-patches
                source nil nil :base-revision :synthetic))
         (executor-symbol
          (intern (concat "dorg-mutate--executor-" "implemented-p")))
         (kinds-symbol
          (intern (concat "dorg-mutate--source-" "kinds"))))
    (should (plist-get plan :supported))
    (should-not (plist-member plan (intern (concat ":executor-" "implemented"))))
    (should-not (fboundp executor-symbol))
    (should-not (boundp kinds-symbol))))

(ert-deftest dorg-test-c8ynt-c3-commented-cell-is-the-sole-s1-exception ()
  "C3 data supports only exact COMMENTED fields; general S1 remains refused."
  (should
   (equal '((((update :heading) heading (:commented))
             :status supported :slice S1
             :variant (:host heading :fields (:commented))))
          (cl-remove-if-not
           (lambda (cell) (eq 'S1 (plist-get (cdr cell) :slice)))
           dorg-mutate--c3-field-cells)))
  (let ((commented '(:op update :target (:heading :id 1)
                     :fields (:commented nil)))
        (mixed '(:op update :target (:heading :id 1)
                 :fields (:commented t :title "Renamed")))
        (root '(:op update :target (:heading :root)
                :fields (:commented t))))
    (should (equal '(supported . S1)
                   (dorg-mutate--c3-cell commented 'heading)))
    (should (equal '(unsupported . S1)
                   (dorg-mutate--c3-cell mixed 'heading)))
    (should (equal '(unsupported . S1)
                   (dorg-mutate--c3-cell root 'root)))))

(ert-deftest dorg-test-c8ynt-commented-plain-org-round-trip-and-refusal ()
  "Plain-org adds/removes COMMENT and refuses non-booleans before byte writes."
  (dorg-test-mutate--with-content buf dorg-test-c8ynt--rich-heading
    (let* ((bytes0 (buffer-string))
           (tree0 (dorg-parse-buffer buf))
           (target0 (cl-find 9101 (dorg-parse-result-headings tree0)
                             :key #'dorg-heading-id :test #'eql))
           (target-state0 (dorg-test-c8ynt--heading-state target0)))
      (should-not (dorg-heading-commented target0))
      (let* ((patches (dorg-api-normalize
                       '(on (id 9101) (set (heading commented) t)) buf))
             (plan (dorg-mutate--plan-patches buf patches)))
        (should (plist-get plan :supported))
        (should (plist-get (car (plist-get plan :patch-support)) :supported)))
      ;; A mixed S1 payload remains refused in phase 1, before COMMENT can land.
      (let* ((before (buffer-string))
             (data (dorg-test-mutate--error-data
                    (dorg-mutate--apply-patches
                     buf '((:op update :target (:heading :id 9101)
                                  :fields (:commented t :title "Renamed")))))))
        (should (eq 'unsupported (plist-get data :reason)))
        (should (eq 'no-source-preserving-editor (plist-get data :detail)))
        (should (equal before (buffer-string))))
      (dorg-api-execute '(on (id 9101) (set (heading commented) t))
                        :mode 'apply :source buf)
      (let* ((bytes1 (buffer-string))
             (target1 (dorg-test-mutate--parsed-heading buf 9101))
             (expected1
              (replace-regexp-in-string
               (regexp-quote "* TODO [#A] Task  :alpha:beta:\n")
               "* TODO [#A] COMMENT Task  :alpha:beta:\n"
               bytes0 t t)))
        (should-not (equal bytes0 bytes1))
        ;; Byte-minimality: the exact COMMENT+space insertion is the whole diff.
        (should (equal expected1 bytes1))
        (should (dorg-heading-commented target1))
        (should (equal target-state0
                       (dorg-test-c8ynt--heading-state target1)))
        (should (string-match-p
                 "^\\* TODO \\[#A\\] COMMENT Task  :alpha:beta:$" bytes1))
        ;; Same-value assignment is a byte no-op; do not toggle twice.
        (dorg-api-execute '(on (id 9101) (set (heading commented) t))
                          :mode 'apply :source buf)
        (should (equal bytes1 (buffer-string)))
        (dorg-api-execute '(on (key "task-9101")
                                (set (heading commented) nil))
                          :mode 'apply
                          :source (list :buffer buf
                                        :identity '(:property "BEAD_ID")))
        (let ((target2 (dorg-test-mutate--parsed-heading buf 9101)))
          (should-not (dorg-heading-commented target2))
          (should (equal target-state0
                         (dorg-test-c8ynt--heading-state target2)))
          (should (equal bytes0 (buffer-string)))))
      (dolist (bad '(bogus "yes"))
        (let ((before (buffer-string)))
          (should
           (equal '(invalid-target "heading :commented must be boolean")
                  (dorg-test-c8ynt--error
                   (lambda ()
                     (dorg-api-execute
                      `(on (id 9101) (set (heading commented) ,bad))
                      :mode 'apply :source buf)))))
          (should (equal before (buffer-string))))))))

(ert-deftest dorg-test-c8ynt-commented-dolt-round-trip-and-refusal ()
  "Dolt adds/removes COMMENT with one commit each and rejects without a commit."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir dorg-test-c8ynt--rich-heading 9000))
           (source (list :dolt-dir dolt-dir :root-id root))
           (tree0 (dorg-model-load-tree dolt-dir root))
           (state0 (dorg-test-c8ynt--tree-state tree0))
           (commits0 (alist-get
                      'cnt (car (dorg-sql
                                 dolt-dir
                                 "SELECT COUNT(*) AS cnt FROM dolt_log")))))
      (should-not (dorg-heading-commented (gethash 9101 tree0)))
      (dorg-api-execute '(on (id 9101) (set (heading commented) t))
                        :mode 'apply :source source)
      (let* ((tree1 (dorg-model-load-tree dolt-dir root))
             (head1 (dorg-head-commit dolt-dir))
             (commits1 (alist-get
                        'cnt (car (dorg-sql
                                   dolt-dir
                                   "SELECT COUNT(*) AS cnt FROM dolt_log")))))
        (should (dorg-heading-commented (gethash 9101 tree1)))
        (should (equal state0 (dorg-test-c8ynt--tree-state tree1)))
        (should (= (1+ commits0) commits1))
        (dorg-api-execute '(on (key "task-9101")
                                (set (heading commented) nil))
                          :mode 'apply
                          :source (append source
                                          '(:identity (:property "BEAD_ID"))))
        (let* ((tree2 (dorg-model-load-tree dolt-dir root))
               (head2 (dorg-head-commit dolt-dir))
               (commits2 (alist-get
                          'cnt (car (dorg-sql
                                     dolt-dir
                                     "SELECT COUNT(*) AS cnt FROM dolt_log")))))
          (should-not (equal head1 head2))
          (should-not (dorg-heading-commented (gethash 9101 tree2)))
          (should (equal state0 (dorg-test-c8ynt--tree-state tree2)))
          (should (= (1+ commits1) commits2))
          (dolist (bad '(bogus "yes"))
            (should
             (equal '(invalid-target "heading :commented must be boolean")
                    (dorg-test-c8ynt--error
                     (lambda ()
                       (dorg-api-execute
                        `(on (id 9101) (set (heading commented) ,bad))
                        :mode 'apply :source source)))))
            (should (equal head2 (dorg-head-commit dolt-dir)))
            (should (= commits2
                       (alist-get
                        'cnt (car (dorg-sql
                                   dolt-dir
                                   "SELECT COUNT(*) AS cnt FROM dolt_log")))))
            (should (equal state0
                           (dorg-test-c8ynt--tree-state
                           (dorg-model-load-tree dolt-dir root))))))))))

;;; ─── Atomic COMMENTED heading construction (tracked-regression) ───────────

(defconst dorg-test-ltxz7--fixture
  (concat
   "* Parent\n"
   ":PROPERTIES:\n:DORG_ID: 9201\n:BEAD_ID: parent\n:END:\n"
   "Parent body.\n"
   "** Existing child\n"
   ":PROPERTIES:\n:DORG_ID: 9202\n:BEAD_ID: existing\n:END:\n"
   "Existing body.\n"
   "* Sibling\n"
   ":PROPERTIES:\n:DORG_ID: 9203\n:BEAD_ID: sibling\n:END:\n")
  "Constructor fixture with a non-empty parent subtree and following sibling.")

(defun dorg-test-ltxz7--insert-patch (&optional target heading)
  "Return one representative typed insert patch with TARGET and HEADING overrides."
  (list :op 'insert
        :target (or target '(:tree :under (:heading :id 9201)))
        :heading (or heading '(:title "Resolution" :commented t))))

(defun dorg-test-ltxz7--patch-support (source patch)
  "Return PATCH's sole support record under SOURCE."
  (car (plist-get (dorg-mutate--plan-patches source (list patch))
                  :patch-support)))

(ert-deftest dorg-test-ltxz7-commented-constructor-is-one-c3-data-cell ()
  "C3 data, not a dispatch bypass, owns the explicit-commented constructor."
  (should
   (equal
    '((((update :heading) heading (:commented))
       :status supported :slice S1
       :variant (:host heading :fields (:commented)))
      (((insert :tree) heading (:commented))
       :status supported :slice S3
       :variant (:host heading :constructor (:commented))))
    dorg-mutate--c3-field-cells))
  (dolist (value '(t nil))
    (let ((patch (dorg-test-ltxz7--insert-patch
                  nil (list :title "Resolution" :commented value))))
      (should (equal '(supported . S3)
                     (dorg-mutate--c3-cell patch 'heading)))))
  ;; Transformation: deleting the new data cell must remove plain support.
  (let ((dorg-mutate--c3-field-cells
         (list (car dorg-mutate--c3-field-cells))))
    (should (equal '(unsupported . S3)
                   (dorg-mutate--c3-cell
                    (dorg-test-ltxz7--insert-patch) 'heading)))))

(ert-deftest dorg-test-ltxz7-plain-c3-refusal-matrix-is-exact ()
  "Every non-ratified plain constructor shape stays typed and byte-unchanged."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (let* ((source (dorg-mutate--resolve-source buf))
           (omitted (dorg-test-ltxz7--insert-patch
                     nil '(:title "Visible")))
           (root (dorg-test-ltxz7--insert-patch
                  '(:tree :under :root)))
           (anchored (dorg-test-ltxz7--insert-patch
                      '(:tree :under (:heading :id 9201)
                              :before (:heading :id 9202))))
           (broader (dorg-test-ltxz7--insert-patch
                     nil '(:title "Raw" :commented t :post-blank 1)))
           (before (buffer-string)))
      (dolist (case `((omitted . ,omitted)
                      (root . ,root)
                      (anchored . ,anchored)
                      (broader . ,broader)))
        (ert-info ((symbol-name (car case)))
          (let* ((patch (cdr case))
                 (support (dorg-test-ltxz7--patch-support source patch))
                 (data (dorg-test-mutate--error-data
                        (dorg-mutate--apply-patches source (list patch)))))
            (should-not (plist-get support :supported))
            (should (eq 'no-source-preserving-editor
                        (plist-get support :reason)))
            (should (eq 'unsupported (plist-get data :reason)))
            (should (eq 'no-source-preserving-editor
                        (plist-get data :detail)))
            (should (equal before (buffer-string)))))))))

(ert-deftest dorg-test-ltxz7-dolt-c3-keeps-full-constructor-surface ()
  "Plain's new cell does not narrow any established Dolt constructor shape."
  (let* ((source (dorg-mutate--source-make
                  :kind 'dolt :dolt-dir "/nonexistent/tracked-case/"
                  :root-id 9200 :identity 'dorg))
         (patches
          (list
           (dorg-test-ltxz7--insert-patch nil '(:title "Omitted"))
           (dorg-test-ltxz7--insert-patch '(:tree :under :root))
           (dorg-test-ltxz7--insert-patch
            '(:tree :under (:heading :id 9201) :before (:heading :id 9202)))
           (dorg-test-ltxz7--insert-patch
            nil '(:title "Raw" :commented t :post-blank 1)))))
    (dolist (patch patches)
      (let ((support (dorg-test-ltxz7--patch-support source patch)))
        (should (plist-get support :supported))
        (should-not (plist-get support :reason))))))

(ert-deftest dorg-test-ltxz7-plain-dorg-heading-is-hidden-from-first-bytes ()
  "A dorg-bound plain child is born COMMENTED with one truthful created id."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((before (buffer-string))
           (response
            (dorg-api-execute
             '(insert (heading (title "Resolution")
                               (todo "TODO")
                               (priority "A")
                               (tags "workflow")
                               (commented t))
                      (under (id 9201)))
             :mode 'apply :source buf))
           (created (plist-get (plist-get response :affected) :created))
           (id (car created))
           (heading (and id (dorg-test-mutate--parsed-heading buf id))))
      (should (plist-get response :ok))
      (should (= 1 (length created)))
      (should (integerp id))
      (should heading)
      (should (dorg-heading-commented heading))
      (should (equal "Resolution" (dorg-heading-title heading)))
      (should (equal 9201 (dorg-heading-parent-id heading)))
      (should (string-match-p
               (regexp-quote
                (format "** TODO [#A] COMMENT Resolution  :workflow:\n:PROPERTIES:\n:DORG_ID: %d\n:END:"
                        id))
               (buffer-string)))
      (should-not (equal before (buffer-string)))
      ;; Birth-hidden is literal: the first and only inserted heading line has
      ;; COMMENT; no visible preimage was ever committed to the buffer.
      (should-not (string-match-p "^\\*\\* TODO \\[#A\\] Resolution"
                                  (buffer-string))))))

(ert-deftest dorg-test-ltxz7-property-identity-creates-no-fabricated-id ()
  "A DORG_ID-less faithful child carries its key and reports no numeric id."
  (let ((fixture
         (concat "* Parent\n:PROPERTIES:\n:BEAD_ID: parent\n:END:\n"
                 "* Sibling\n:PROPERTIES:\n:BEAD_ID: sibling\n:END:\n")))
    (dorg-test-mutate--with-content buf fixture
      (let* ((source (list :buffer buf :identity '(:property "BEAD_ID")))
             (before (buffer-string)))
        ;; Missing and duplicate carriers both fail before bytes move.
        (dolist
            (form
             '((insert (heading (title "No key") (commented t))
                       (under (key "parent")))
               (insert (heading (title "Two keys")
                                (property "BEAD_ID" "one")
                                (property "BEAD_ID" "two")
                                (commented t))
                       (under (key "parent")))))
          (should (eq 'invalid-target
                      (dorg-test-mutate--reason
                       (dorg-api-execute form :mode 'apply :source source))))
          (should (equal before (buffer-string))))
        (let* ((response
                (dorg-api-execute
                 '(insert (heading (title "Resolution")
                                   (property "BEAD_ID" "resolution-1")
                                   (commented t))
                          (under (key "parent")))
                 :mode 'apply :source source))
               (created (plist-get (plist-get response :affected) :created)))
          (should (plist-get response :ok))
          (should-not created)
          (should (string-match-p "^\\*\\* COMMENT Resolution$"
                                  (buffer-string)))
          (should (string-match-p ":BEAD_ID: resolution-1"
                                  (buffer-string)))
          (should-not (string-match-p ":DORG_ID:" (buffer-string))))))))

(ert-deftest dorg-test-ltxz7-plain-constructor-batch-failure-restores-preimage ()
  "A later phase-one miss leaves no hidden child or other partial bytes."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (let ((before (buffer-string)))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-api-execute
                    '(batch
                      (insert (heading (title "Resolution") (commented t))
                              (under (id 9201)))
                      (on (id 999999) (set (property "K") "V")))
                    :mode 'apply :source buf))))
      (should (equal before (buffer-string)))
      (should-not (string-match-p "Resolution" (buffer-string))))))

(ert-deftest dorg-test-ltxz7-plain-dry-run-is-deterministic-and-no-write ()
  "Dry-run preserves explicit COMMENTED and has no source effect."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (let* ((form '(insert (heading (title "Resolution") (commented nil))
                          (under (id 9201))))
           (before (buffer-string))
           (one (dorg-api-execute form :mode 'dry-run :source buf))
           (two (dorg-api-execute form :mode 'dry-run :source buf))
           (heading (plist-get (car (plist-get one :patches)) :heading)))
      (should (equal one two))
      (should (plist-get one :supported))
      (should (plist-member heading :commented))
      (should-not (plist-get heading :commented))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-ltxz7-dolt-heading-is-hidden-in-first-commit ()
  "Dolt inserts COMMENTED in one commit and round-trips constructor state."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir dorg-test-ltxz7--fixture 9200))
           (source (list :dolt-dir dolt-dir :root-id root))
           (commits0 (alist-get
                      'cnt (car (dorg-sql
                                 dolt-dir
                                 "SELECT COUNT(*) AS cnt FROM dolt_log"))))
           (response
            (dorg-api-execute
             '(insert (heading (title "Resolution")
                               (todo "TODO")
                               (priority "A")
                               (tags "workflow")
                               (property "BEAD_ID" "resolution-dolt")
                               (commented t))
                      (under (id 9201)))
             :mode 'apply :source source))
           (created (plist-get (plist-get response :affected) :created))
           (id (car created))
           (row (car (dorg-sql
                      dolt-dir
                      (format
                       "SELECT commented, title FROM headings WHERE id = %d" id))))
           (heading (gethash id (dorg-model-load-tree dolt-dir root)))
           (commits1 (alist-get
                      'cnt (car (dorg-sql
                                 dolt-dir
                                 "SELECT COUNT(*) AS cnt FROM dolt_log")))))
      (should (plist-get response :ok))
      (should (= 1 (length created)))
      (should (= (1+ commits0) commits1))
      (should (= 1 (alist-get 'commented row)))
      (should (equal "Resolution" (alist-get 'title row)))
      (should (dorg-heading-commented heading))
      (should (equal "TODO" (dorg-heading-todo heading)))
      (should (equal "A" (dorg-heading-priority heading)))
      (should (equal '("workflow") (dorg-heading-tags heading)))
      (should (equal 9201 (dorg-heading-parent-id heading))))))

;;; ─── Legacy mutation vocabulary retirement (tracked-regression) ───────────

(ert-deftest dorg-test-mutate-legacy-entry-seams-are-absent ()
  "The typed engine has no callable raw-operation compatibility entry.
This is an absence contract, not a deprecation contract: aliases and private
lowering shims would keep the second mutation language alive."
  (dolist (symbol '(dorg-plan-ops
                    dorg-apply-ops
                    dorg-api--lower-patches
                    dorg-mutate--patch->ops
                    dorg-mutate--patches->ops
                    dorg-mutate--normalize-op
                    dorg-mutate--normalize-ops))
    (should-not (fboundp symbol))))

(ert-deftest dorg-test-mutate-definition-only-remnants-removed ()
  "Five never-called private remnants are gone and their neighbours are not.
Each of the five was defined and referenced nowhere else in DOrg, its tests,
its docs, or any non-vendored downstream — dead text carried into the first
public tag.  Two of them were pass-throughs and two were exact aliases, so
the risk of deleting them is not that a caller breaks (there is none) but
that the deletion is widened into the LIVE helper standing next to each one.
The positive assertions below are that guard: they fail if the C3 surgical
signature set, its field-cell exceptions, the external host-id resolver, or
either owning `dorg-ext' index primitive is removed alongside the remnants.
Re-adding any remnant turns the `should-not' RED."
  (dolist (symbol '(dorg-mutate--ps-keyword-indices
                    dorg-mutate--dolt-resolve-extern-host
                    dorg-mutate--index-lineage-changed-p
                    dorg-mutate--index-image-canonical))
    (should-not (fboundp symbol)))
  (should-not (boundp 'dorg-mutate--source-preserving-supported-patches))
  ;; Live neighbours the deletions must not reach.
  (should (boundp 'dorg-mutate--c3-surgical-patches))
  (should (boundp 'dorg-mutate--c3-field-cells))
  (should (fboundp 'dorg-mutate--dolt-resolve-host-id))
  (should (fboundp 'dorg-ext--index-lineage-changed-p))
  (should (fboundp 'dorg-ext--index-image-canonical))
  ;; The remaining mutate-side index pass-throughs are out of scope and stay.
  (dolist (symbol '(dorg-mutate--index-host-safe-image
                    dorg-mutate--index-classify
                    dorg-mutate--index-heading-canonical
                    dorg-mutate--index-owner-partition-canonical))
    (should (fboundp symbol))))

(ert-deftest dorg-test-mutate-native-plan-carries-patches-only ()
  "A native plan carries the exact typed batch and typed support vocabulary.
No lowered operation echo may survive in either the plan or its capability
classifications."
  (dorg-test-mutate--with-buffer buf
    (let* ((patches (dorg-api-normalize
                     '(on (id 1001) (set (property "CATEGORY") "work")) buf))
           (before (buffer-string))
           (plan (dorg-mutate--plan-patches buf patches)))
      (should (equal patches (plist-get plan :patches)))
      (should (plist-member plan :patch-support))
      (should-not (plist-member plan :ops))
      (should-not (plist-member plan :op-support))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-mutate-native-validator-never-drops-unknown-fields ()
  "An unknown typed-patch field refuses before mutation instead of vanishing.
The source is byte-pinned so silently ignoring `:bogus' cannot false-green."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (patch (car (dorg-api-normalize
                        '(on (id 1001) (set (property "CATEGORY") "work")) buf)))
           (malformed (append patch '(:bogus t)))
           (data (dorg-test-mutate--error-data
                  (dorg-mutate--apply-patches buf (list malformed)))))
      (should (eq 'invalid-op (plist-get data :reason)))
      (should (equal before (buffer-string))))))

;;; ─── Sortkey exhaustion / bounded-midpoint adoption (tracked-regression) ────
;;
;; The typed-mutation seams (`dorg-mutate--position-key' for insert/move and
;; the `dorg-mutate--ps-delete-heading' child-promotion loop) historically
;; allocated with the raw `dorg-sortkey-midpoint', which grows the key length
;; without bound.  On an aged sibling list a single placement can reach the
;; VARBINARY(64) storage cliff; the SQL guard then refuses AFTER the core rows
;; commit, orphaning them.  These tests pin the restorative repair: bounded
;; allocation (`dorg-sortkey-midpoint-limited') plus complete-parent rebalance,
;; so the placement succeeds with strictly-ordered, sub-cliff keys and the SQL
;; guard is never the failing path.

(defun dorg-test-mutate--aged-key (n)
  "Return a 63-byte sort key: 62 null bytes terminated by byte N.
Two such keys with adjacent N have no room for a bounded midpoint — a
bisection between them (or a before-first insert below one) reaches the
64-byte rebalance threshold, reproducing the incident shape."
  (concat (make-string 62 0) (unibyte-string n)))

(defun dorg-test-mutate--ps-with (root-id &rest headings)
  "Build a `dorg-mutate--ps' seeded with HEADINGS under ROOT-ID.
Each HEADING is inserted into a fresh id->heading hash.  Returns the PS."
  (let ((h (make-hash-table :test 'eql)))
    (dolist (heading headings)
      (puthash (dorg-heading-id heading) heading h))
    (dorg-mutate--ps-create :headings h :root-id root-id)))

(defun dorg-test-mutate--child-keys (ps parent-id)
  "Return PARENT-ID's child sort keys in PS, in sorted sibling order."
  (mapcar #'dorg-heading-sort-key (dorg-mutate--ps-children ps parent-id)))

(defun dorg-test-mutate--keys-bounded-ordered-p (keys)
  "Return non-nil if KEYS are strictly ordered and every key is sub-cliff.
Sub-cliff means shorter than `dorg-sortkey-max-bytes' (64) — the fix keeps
allocations strictly below the storage limit, never at or beyond it."
  (and (dorg-sortkey-ordered-p keys)
       (cl-every (lambda (k) (< (length k) dorg-sortkey-max-bytes)) keys)))

(ert-deftest dorg-test-mutate-insert-prepend-refuses-cleanly-on-exhaustion ()
  "A before-first insert onto an aged list refuses before any core commit (AC1).
The first sibling key is a 63-byte aged key, so a before-first midpoint reaches
the storage cliff.  The insert executor hydrates only the parent plus placement
neighbors (tracked-regression), so it cannot complete-parent rebalance; it must fail
closed with a structured `invariant'/`sort-key-exhausted' refusal and leave the
working model untouched — no orphaned core heading.  (The raw midpoint instead
staged a 64-byte cliff key, the class this repair eliminates.)"
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1
                                :sort-key (dorg-test-mutate--aged-key 1)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1
                                :sort-key (dorg-test-mutate--aged-key 2)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root s1 s2))
         (data (dorg-test-mutate--error-data
                (dorg-mutate--ps-insert-heading
                 ps '(:op insert
                      :target (:tree :under (:heading :id 0)
                               :before (:heading :id 10))
                      :heading (:title "prepended"))))))
    ;; Structured refusal, not a raw signal or a silent cliff key.
    (should (eq 'invariant (plist-get data :reason)))
    (should (eq 'sort-key-exhausted (plist-get data :detail)))
    ;; No partial state: the new heading was never staged and siblings are intact.
    (should (null (dorg-mutate--ps-created ps)))
    (should-not (cl-find "prepended" (dorg-mutate--ps-children ps 0)
                         :key #'dorg-heading-title :test #'equal))
    (should (equal (dorg-heading-sort-key s1) (dorg-test-mutate--aged-key 1)))
    (should (equal (dorg-heading-sort-key s2) (dorg-test-mutate--aged-key 2)))))

(ert-deftest dorg-test-mutate-position-key-no-rebalance-when-roomy ()
  "A roomy typed insert does NOT rebalance and matches the raw midpoint (AC8).
When the bounded midpoint does not signal, `position-key' returns exactly the
key the raw midpoint would, and no sibling key is rewritten — proving the
rebalance is boundary-triggered, not unconditional."
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1
                                :sort-key (unibyte-string #x40)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1
                                :sort-key (unibyte-string #xc0)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root s1 s2))
         (expected (dorg-sortkey-midpoint (unibyte-string #x40)
                                          (unibyte-string #xc0))))
    (dorg-mutate--ps-insert-heading
     ps '(:op insert
          :target (:tree :under (:heading :id 0) :after (:heading :id 10))
          :heading (:title "between")))
    (let ((new (cl-find "between" (dorg-mutate--ps-children ps 0)
                        :key #'dorg-heading-title :test #'equal)))
      (should (equal (dorg-heading-sort-key new) expected))
      ;; No rebalance: pre-existing siblings were not rewritten.
      (should-not (memq 10 (dorg-mutate--ps-changed ps)))
      (should-not (memq 11 (dorg-mutate--ps-changed ps)))
      (should (equal (dorg-heading-sort-key s1) (unibyte-string #x40)))
      (should (equal (dorg-heading-sort-key s2) (unibyte-string #xc0))))))

(ert-deftest dorg-test-mutate-move-into-aged-siblings-rebalances ()
  "A same-root move into an aged gap rebalances instead of allocating a
cliff key (AC5).  The moved node lands ahead of the aged first sibling with
strictly-ordered, sub-cliff keys."
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1
                                :sort-key (dorg-test-mutate--aged-key 1)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1
                                :sort-key (dorg-test-mutate--aged-key 2)))
         ;; Mover currently under a different parent so the move re-homes it.
         (mover (make-dorg-heading :id 20 :parent-id 5 :level 2
                                   :sort-key (unibyte-string #x80)))
         (other (make-dorg-heading :id 5 :parent-id 0 :level 1
                                   :sort-key (dorg-test-mutate--aged-key 9)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root s1 s2 mover other)))
    (dorg-mutate--ps-move
     ps '(:op move
          :target (:subtree :id 20)
          :to (:under (:heading :id 0) :before (:heading :id 10))))
    (let ((keys (dorg-test-mutate--child-keys ps 0)))
      (should (dorg-test-mutate--keys-bounded-ordered-p keys))
      ;; No key can reach the VARBINARY(64) SQL guard (AC2).
      (dolist (k keys) (should (dorg-sql-gen--sort-key-hex k)))
      ;; The mover now leads its new siblings.
      (should (eql (dorg-heading-parent-id mover) 0))
      (should (string< (dorg-heading-sort-key mover)
                       (dorg-heading-sort-key s1))))))

(ert-deftest dorg-test-mutate-delete-promotion-rebalances-on-exhaustion ()
  "Promoting children into an aged destination gap rebalances (AC4).
Deleting a heading promotes its children to the grandparent between two
aged siblings; the raw loop would exhaust on the second child, so the repair
rebalances the merged sibling order to strictly-ordered, sub-cliff keys."
  (let* ((left (make-dorg-heading :id 30 :parent-id 0 :level 1
                                  :sort-key (dorg-test-mutate--aged-key 1)))
         (mid (make-dorg-heading :id 31 :parent-id 0 :level 1
                                 :sort-key (dorg-test-mutate--aged-key 2)))
         (right (make-dorg-heading :id 32 :parent-id 0 :level 1
                                   :sort-key (dorg-test-mutate--aged-key 3)))
         (k1 (make-dorg-heading :id 40 :parent-id 31 :level 2
                                :sort-key (unibyte-string #x40)))
         (k2 (make-dorg-heading :id 41 :parent-id 31 :level 2
                                :sort-key (unibyte-string #xc0)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root left mid right k1 k2)))
    (dorg-mutate--ps-delete-heading
     ps '(:op delete :target (:heading :id 31)))
    (let* ((kids (dorg-mutate--ps-children ps 0))
           (ids (mapcar #'dorg-heading-id kids))
           (keys (mapcar #'dorg-heading-sort-key kids)))
      ;; Exact merged placement: left(30) ++ promoted kids(40 41) ++ right(32).
      ;; ps-children returns key order, so this pins that the rebalance assigned
      ;; keys in the promised left++kids++right sequence — not merely that keys
      ;; are sorted (a tautology) nor only child-relative order (k1<k2).
      (should (equal ids '(30 40 41 32)))
      ;; Every survivor key is strictly sub-cliff and clears the SQL guard (AC2).
      (should (cl-every (lambda (k) (< (length k) dorg-sortkey-max-bytes)) keys))
      (dolist (k keys) (should (dorg-sql-gen--sort-key-hex k)))
      ;; Promoted kids re-homed to the grandparent.
      (should (eql (dorg-heading-parent-id k1) 0))
      (should (eql (dorg-heading-parent-id k2) 0)))))

(ert-deftest dorg-test-mutate-dolt-move-into-aged-siblings-persists-rebalance ()
  "End-to-end: a same-root move into an aged destination persists a bounded,
strictly-ordered rebalance (AC5 + AC3).  Crafted 63-byte destination sibling
keys force the before-first placement to the storage cliff; the move loads the
complete destination fan (affected-parent scope), complete-parent rebalances,
and the executor emits the sibling UPDATEs.  Read back from Dolt, every child
key is sub-cliff and the mover leads — proving the rebalance is not just an
in-memory artifact but a committed transformation (raw midpoint instead left a
64-byte cliff key and stale 63-byte siblings)."
  (dorg-test-mutate--with-dolt
    (let* ((root (make-dorg-heading :id 100 :level 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
           (c1 (make-dorg-heading :id 201 :parent-id 100 :level 1 :title "C1"
                                  :sort-key (dorg-test-mutate--aged-key 1)))
           (c2 (make-dorg-heading :id 202 :parent-id 100 :level 1 :title "C2"
                                  :sort-key (dorg-test-mutate--aged-key 2)))
           (g (make-dorg-heading :id 210 :parent-id 100 :level 1 :title "G"
                                 :sort-key (dorg-test-mutate--aged-key 5)))
           (m (make-dorg-heading :id 300 :parent-id 210 :level 2 :title "M"
                                 :sort-key (unibyte-string #x80)))
           (src (list :dolt-dir dolt-dir :root-id 100)))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root 100)
                            (dorg-sql-gen-heading-insert c1 100)
                            (dorg-sql-gen-heading-insert c2 100)
                            (dorg-sql-gen-heading-insert g 100)
                            (dorg-sql-gen-heading-insert m 100)))
      (dorg-commit dolt-dir "seed aged fan")
      ;; Move M from under G to under Root, before C1 — a before-first placement
      ;; that bisects below a 63-byte key and must rebalance.
      (let ((res (dorg-test-mutate--apply-patch-batch
                  src (list (dorg-test-mutate--move-before-patch
                             300
                             (dorg-test-mutate--heading-address 100)
                             (dorg-test-mutate--heading-address 201))))))
        (should (plist-get res :ok)))
      ;; Read the committed destination fan back from Dolt.
      (let ((rows (dorg-sql dolt-dir
                    (concat "SELECT id, LENGTH(sort_key) AS n FROM headings"
                            " WHERE parent_id = 100 ORDER BY sort_key"))))
        ;; All four siblings re-homed under Root, every key strictly sub-cliff.
        (should (equal (mapcar (lambda (r) (alist-get 'id r)) rows)
                       '(300 201 202 210)))
        (dolist (r rows)
          (should (< (alist-get 'n r) dorg-sortkey-max-bytes)))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-into-aged-dest-refuses-no-write ()
  "A cross-root move into an aged destination fails closed with no write (AC-X).
The destination parent carries a crafted 63-byte aged sibling fan, so a
before-first cross-root placement bisects to the storage cliff.  The cross-root
executor emits SQL by mapping over the moved rows only (not a net-membership
diff), so it cannot complete-parent rebalance the destination; it must raise a
structured `invariant'/`sort-key-exhausted' refusal BEFORE any write.  The whole
move is simulated outside the gate, so HEAD does not advance and neither root's
membership changes — no core row is committed without its projection."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed dolt-dir "* Movable\n** Kid\n* SrcSib\n" 100)
    (dorg-test-mutate--seed dolt-dir "* DestParent\n" 500)
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           ;; DestParent (501) is level 1; craft two aged level-2 children.
           (da1 (make-dorg-heading :id 600 :parent-id 501 :level 2 :title "DA1"
                                   :sort-key (dorg-test-mutate--aged-key 1)))
           (da2 (make-dorg-heading :id 601 :parent-id 501 :level 2 :title "DA2"
                                   :sort-key (dorg-test-mutate--aged-key 2))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert da1 500)
                            (dorg-sql-gen-heading-insert da2 500)))
      (dorg-commit dolt-dir "seed aged dest fan")
      (let* ((base (dorg-head-commit dolt-dir))
             (patch (list :op 'move
                          :target (list :subtree :id 101)
                          :to (list :under (dorg-test-mutate--heading-address 501)
                                    :before (dorg-test-mutate--heading-address 600))
                          :to-source (dorg-mutate--resolve-source
                                      (list :dolt-dir dolt-dir :root-id 500))))
             (data (dorg-test-mutate--error-data
                    (dorg-test-mutate--apply-patch-batch src (list patch)))))
        ;; Structured refusal — not a raw signal, not a silent cliff key.
        (should (eq 'invariant (plist-get data :reason)))
        (should (eq 'sort-key-exhausted (plist-get data :detail)))
        ;; No write: HEAD did not advance.
        (should (equal base (dorg-head-commit dolt-dir)))
        ;; Source membership intact — the subtree never left.
        (let ((src-tree (dorg-model-load-tree dolt-dir 100)))
          (should (gethash 101 src-tree))
          (should (gethash 102 src-tree)))
        ;; Destination membership intact — nothing was homed in, aged fan kept.
        (let ((dst-tree (dorg-model-load-tree dolt-dir 500)))
          (should-not (gethash 101 dst-tree))
          (should (gethash 600 dst-tree))
          (should (gethash 601 dst-tree)))))))

(provide 'dorg-mutate-tests)
;;; dorg-mutate-tests.el ends here
