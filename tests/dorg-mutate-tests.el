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

(ert-deftest dorg-test-ay3q8-identity-policy-refuses-protected-metadata ()
  "No system or registered DORG_ key can become an addressing identity."
  (dolist (key '("DORG_CLOCK_START" "dorg_clock_start" "DORG_ID" "DORG_HASH"))
    (let ((err (should-error
                (dorg-mutate--normalize-identity (list :property key))
                :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should (eq 'identity-policy-protected
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal key (plist-get (dorg-mutate-error-data err) :key)))))
  (should (equal '(:property "CAPTURE_ID")
                 (dorg-mutate--normalize-identity '(:property "CAPTURE_ID")))))

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

(ert-deftest dorg-test-mutate-planning-update-preserves-absent-fields ()
  "A planning patch preserves absent fields and removes only after a clear-last."
  (let* ((planning (dorg-planning-item :scheduled "S" :deadline "D" :closed "C"))
         (heading (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H"
                                     :items (list planning)))
         (headings (make-hash-table :test 'eql))
         (ps nil))
    (puthash 1 heading headings)
    (setq ps (dorg-mutate--ps-create :headings headings :root-id 1))
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :scheduled "S2"))
    (should (equal "S2" (dorg-planning-item-scheduled planning)))
    (should (equal "D" (dorg-planning-item-deadline planning)))
    (should (equal "C" (dorg-planning-item-closed planning)))
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :scheduled nil))
    (should-not (dorg-planning-item-scheduled planning))
    (should (equal "D" (dorg-planning-item-deadline planning)))
    (should (equal "C" (dorg-planning-item-closed planning)))
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :deadline nil :closed nil))
    (should-not (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item))
                            (dorg-heading-items heading)))
    (let ((err (should-error
                (dorg-mutate--validate-patches
                 '((:op update :target (:planning :host (:heading :id 1)))))
                :type 'dorg-mutate-error)))
      (should (equal "planning update requires at least one field" (cadr err)))
      (should (eq 'invalid-target (plist-get (caddr err) :reason))))))

(ert-deftest dorg-test-mutate-planning-update-clear-absent-is-a-noop ()
  "An explicit clear does not create a planning line on an unplanned heading."
  (let* ((heading (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H"))
         (headings (make-hash-table :test 'eql))
         (ps nil))
    (puthash 1 heading headings)
    (setq ps (dorg-mutate--ps-create :headings headings :root-id 1))
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :deadline nil))
    (should-not (dorg-heading-items heading))))

(ert-deftest dorg-test-mutate-planning-update-batch-composes-fields ()
  "Successive planning fields compose without erasing an existing sibling."
  (let* ((planning (dorg-planning-item :closed "C"))
         (heading (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "H"
                                     :items (list planning)))
         (headings (make-hash-table :test 'eql))
         (ps nil)
         (context (dorg-mutate--item-batch-context)))
    (puthash 1 heading headings)
    (setq ps (dorg-mutate--ps-create :headings headings :root-id 1))
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :scheduled "S") context)
    (dorg-mutate--ps-update-planning
     ps '(:op update :target (:planning :host 1) :deadline "D") context)
    (should (equal "S" (dorg-planning-item-scheduled planning)))
    (should (equal "D" (dorg-planning-item-deadline planning)))
    (should (equal "C" (dorg-planning-item-closed planning)))))

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
  (append (list :op verb :target (append (list :keyword :host host :name key)
                                       (unless (eq verb 'insert)
                                         '(:select (:all))))) fields))

(defun dorg-test-mutate--keyword-occurrence-patch
    (verb host key occurrence &rest fields)
  "Return a canonical typed single-keyword occurrence patch."
  (append (list :op verb
                :target (list :keyword :host host :name key
                              :select (list :occurrence occurrence)))
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

(ert-deftest dorg-test-32mp9-native-validator-protects-owned-properties ()
  "Typed property put/delete delegates to the shared DORG_ prefix rule.

These are direct-IR rows: they deliberately bypass the public normalizer, so
they turn RED if the engine returns to literal DORG_ID comparison."
  (dolist (key '("DORG_ID" "dorg_id" "DORG_HASH"))
    (dolist (patch (list (dorg-test-mutate--property-put-patch 3 key "9")
                         (dorg-test-mutate--property-delete-patch 3 key)))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-mutate--validate-patches (list patch)))))))
  (let ((patch (dorg-test-mutate--property-put-patch 3 "XDORG_ID" "9")))
    (should (equal (list patch) (dorg-mutate--validate-patches (list patch))))))

(ert-deftest dorg-test-mutate-native-validator-refuses-owned-metadata-in-every-property-door ()
  "Typed IR cannot bypass an unavailable owner's registered family.

The direct property target and the typed drawer-item validator are distinct
engine entry doors; both must reject before planning."
  (dolist (thunk
           (list
            (lambda ()
              (dorg-mutate--validate-patches
               (list (dorg-test-mutate--property-put-patch
                      3 "DORG_CLOCK_START" "2026-10-14T08:00:00Z"))))
            (lambda ()
              (dorg-mutate--patch-validate-item-value
               '(:kind drawer :name "PROPERTIES"
                 :properties (("DORG_CLOCK_START" . "2026-10-14T08:00:00Z")))
               'replace))))
    (let ((err (should-error (funcall thunk) :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should (eq 'owner-unavailable
                  (plist-get (dorg-mutate-error-data err) :detail))))))

(ert-deftest dorg-test-mutate-owner-lowering-writes-only-its-family ()
  "A test-only lazy owner can write its own registered family through lowering."
  (dorg-test-mutate--with-content buf
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
    (let ((patch (dorg-test-mutate--property-put-patch
                  1001 "DORG_CLOCK_START" "2026-10-14T08:00:00Z")))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request) (list :patches (list patch)))
                         :validate (lambda (_context) nil)))))
        (dorg-mutate--owner-lowering-execute buf "DORG_CLOCK_" '(:start t))
        (should (string-match-p
                 ":DORG_CLOCK_START: 2026-10-14T08:00:00Z" (buffer-string)))))))

(ert-deftest dorg-test-mutate-saved-owner-lowering-holds-gate-before-lower ()
  "A saved owner lowerer runs only after acquiring the canonical file gate."
  (dorg-test-mutate--with-content buf
      "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
    (let ((patch (dorg-test-mutate--property-put-patch
                  1001 "DORG_CLOCK_START" "2026-10-14T08:00:00Z"))
          (lock (dorg-file-lock-path dorg-test-mutate--tmp))
          (saw-lock nil))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request)
                                  (setq saw-lock (file-symlink-p lock))
                                  (list :patches (list patch)))
                         :validate (lambda (_context) nil)))))
        (let ((result (dorg-mutate--owner-lowering-execute
                       buf "DORG_CLOCK_" '(:start t) :save t)))
          (should (plist-get result :saved))))
      (should (string-prefix-p "dorg-lock-v1:" saw-lock))
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-mutate-owner-lowering-refuses-system-key ()
  "Family authority cannot mutate DORG_ID through the private executor."
  (dorg-test-mutate--with-buffer buf
    (let ((patch (dorg-test-mutate--property-put-patch 1001 "DORG_ID" "9")))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request) (list :patches (list patch)))
                         :validate (lambda (_context) nil)))))
        (let ((err (should-error
                    (dorg-mutate--owner-lowering-execute buf "DORG_CLOCK_" '(:start t))
                    :type 'dorg-mutate-error)))
          (should (eq 'invalid-target (dorg-mutate-error-reason err))))))))

(ert-deftest dorg-test-mutate-owner-lowering-refuses-foreign-family ()
  "DORG_CLOCK_ authority cannot write a different registered family."
  (dorg-test-mutate--with-buffer buf
    (let ((dorg-owned-metadata-families
           '(("DORG_CLOCK_" dorg-clock-metadata-owner 1 dorg-clock)
             ("DORG_OTHER_" dorg-other-metadata-owner 1 nil)))
          (patch (dorg-test-mutate--property-put-patch 1001 "DORG_OTHER_X" "x")))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request) (list :patches (list patch)))
                         :validate (lambda (_context) nil)))))
        (let ((err (should-error
                    (dorg-mutate--owner-lowering-execute buf "DORG_CLOCK_" '(:start t))
                    :type 'dorg-mutate-error)))
          (should (eq 'invalid-target (dorg-mutate-error-reason err)))
          (should (eq 'capability-family-mismatch
                      (plist-get (dorg-mutate-error-data err) :detail))))))))

(ert-deftest dorg-test-mutate-owner-lowering-refuses-unavailable-owner ()
  "A registered family with no loaded participant cannot enter lowering."
  (dorg-test-mutate--with-buffer buf
    (cl-letf (((symbol-function 'dorg-clock-metadata-owner) nil))
      (let ((err (should-error
                  (dorg-mutate--owner-lowering-execute buf "DORG_CLOCK_" '(:start t))
                  :type 'dorg-mutate-error)))
        (should (eq 'invalid-target (dorg-mutate-error-reason err)))
        (should (eq 'owner-unavailable
                    (plist-get (dorg-mutate-error-data err) :detail)))))))

(ert-deftest dorg-test-mutate-owner-validation-sees-immutable-plain-candidate ()
  "A managed plain dry-run validates against immutable candidate images.

The refusal must see a complete before/after context and leave the source
untouched.  Removing the shared validator call makes this control green,
which is the intended RED mutation."
  (dorg-test-mutate--with-content buf
      "* Before\n:PROPERTIES:\n:DORG_ID: 1001\n:DORG_CLOCK_START: 2026-10-14T08:00:00Z\n:END:\n"
    (let ((before (buffer-string))
          seen)
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list
                    :lower
                    (lambda (_request)
                      (list :patches
                            (list (dorg-test-mutate--heading-update-patch
                                   1001 :title "After"))))
                    :validate
                    (lambda (context)
                      (setq seen context)
                      (error "clock owner rejects candidate"))))))
        (let ((err (should-error
                    (dorg-mutate--owner-lowering-execute
                     buf "DORG_CLOCK_" '(:update t) :dry-run t)
                    :type 'dorg-mutate-error)))
          (should (eq 'invalid-target (dorg-mutate-error-reason err)))
          (should (eq 'owner-refused
                      (plist-get (dorg-mutate-error-data err) :detail)))
          (should (equal "DORG_CLOCK_"
                         (plist-get (dorg-mutate-error-data err) :family)))))
      (should (cl-typep seen 'dorg-ext-index-context))
      (should (gethash 1001 (dorg-ext-index-context-base-image seen)))
      (should (gethash 1001 (dorg-ext-index-context-candidate-postimage seen)))
      (let ((changed (dorg-ext-index-context-changed seen)))
        (should (= 1 (length changed)))
        (should (dorg-ext-index-entry-before (car changed)))
        (should (dorg-ext-index-entry-after (car changed))))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-mutate-owner-validation-sees-property-identity-candidate ()
  "A property-identity plain candidate cannot bypass owner validation.

The source intentionally has no DORG_ID.  The validator must nevertheless
receive a changed before/after entry derived from CAPTURE_ID, then refuse the
dry run without changing source bytes."
  (dorg-test-mutate--with-content buf
      "* Before\n:PROPERTIES:\n:CAPTURE_ID: capture-1\n:DORG_CLOCK_START: 2026-10-14T08:00:00Z\n:END:\n"
    (let ((before (buffer-string))
          seen
          (patch '(:op update
                   :target (:heading :extern t :key "CAPTURE_ID" :value "capture-1")
                   :fields (:title "After"))))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request) (list :patches (list patch)))
                         :validate (lambda (context)
                                     (setq seen context)
                                     (error "clock owner rejects property candidate"))))))
        (let ((err (should-error
                    (dorg-mutate--owner-lowering-execute
                     (list :buffer buf :identity '(:property "CAPTURE_ID"))
                     "DORG_CLOCK_" '(:update t) :dry-run t)
                    :type 'dorg-mutate-error)))
          (should (eq 'invalid-target (dorg-mutate-error-reason err)))
          (should (eq 'owner-refused
                      (plist-get (dorg-mutate-error-data err) :detail)))))
      (should (cl-typep seen 'dorg-ext-index-context))
      (should (= 1 (length (dorg-ext-index-context-changed seen))))
      (let ((entry (car (dorg-ext-index-context-changed seen))))
        (should (dorg-ext-index-entry-before entry))
        (should (dorg-ext-index-entry-after entry)))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-mutate-owner-validation-sees-last-key-removal ()
  "A before-only family member remains visible to its plain owner validator."
  (dorg-test-mutate--with-content buf
      "* Before\n:PROPERTIES:\n:DORG_ID: 1001\n:DORG_CLOCK_START: 2026-10-14T08:00:00Z\n:END:\n"
    (let ((before (buffer-string))
          seen
          (patch '(:op delete
                   :target (:property :host (:heading :id 1001)
                              :name "DORG_CLOCK_START"))))
      (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
                 (lambda ()
                   (list :lower (lambda (_request) (list :patches (list patch)))
                         :validate (lambda (context)
                                     (setq seen context)
                                     (error "clock owner rejects removal"))))))
        (let ((err (should-error
                    (dorg-mutate--owner-lowering-execute
                     buf "DORG_CLOCK_" '(:delete t) :dry-run t)
                    :type 'dorg-mutate-error)))
          (should (eq 'owner-refused
                      (plist-get (dorg-mutate-error-data err) :detail)))))
      (let ((entry (car (dorg-ext-index-context-changed seen))))
        (should entry)
        (should (dorg-ext-index-entry-before entry))
        (should (dorg-ext-index-entry-after entry))
        (should (member "DORG_CLOCK_"
                        (dorg-mutate--heading-owned-metadata-families
                         (dorg-ext-index-entry-before entry))))
        (should-not (dorg-mutate--heading-owned-metadata-families
                     (dorg-ext-index-entry-after entry))))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-mutate-owner-validation-normalizes-lazy-owner-failure ()
  "A lazy owner load error is the same typed candidate refusal as validation."
  (let* ((managed
          (make-dorg-heading
           :id 2 :items
           (list (dorg-drawer-item
                  :name "PROPERTIES"
                  :properties
                  '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))))
         (context
          (dorg-ext-index-context-create
           :changed
           (list (dorg-ext-index-entry-create
                  :kind 'change :id 2 :before managed :after managed))
           :base-image (let ((image (make-hash-table :test 'eql)))
                         (puthash 2 managed image)
                         image)
           :candidate-postimage (let ((image (make-hash-table :test 'eql)))
                                  (puthash 2 managed image)
                                  image))))
    (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
               (lambda () (error "lazy owner failed"))))
      (let ((err (should-error
                  (dorg-mutate--validate-owned-metadata-context context)
                  :type 'dorg-mutate-error)))
        (should (eq 'owner-refused
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal "lazy owner failed"
                       (plist-get (dorg-mutate-error-data err) :owner-message)))))))

(ert-deftest dorg-test-ay3q8-owner-validation-refuses-nil-images-before-owner ()
  "Owner validation never receives a context with a missing immutable image.

The test owner returns normally, so deleting the image preflight turns this
control RED by allowing the malformed context to pass."
  (let* ((managed
          (make-dorg-heading
           :id 2 :items
           (list (dorg-drawer-item
                  :name "PROPERTIES"
                  :properties
                  '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))))
         (context
          (dorg-ext-index-context-create
           :changed (list (dorg-ext-index-entry-create
                           :kind 'change :id 2 :before managed :after managed))
           :base-image nil :candidate-postimage nil))
         called)
    (cl-letf (((symbol-function 'dorg-clock-metadata-owner)
               (lambda ()
                 (list :validate (lambda (_context) (setq called t))))))
      (let ((err (should-error
                  (dorg-mutate--validate-owned-metadata-context context)
                  :type 'dorg-mutate-error)))
        (should (eq 'invalid-target (dorg-mutate-error-reason err)))
        (should (eq 'owner-context-images-missing
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should-not called)))))

(ert-deftest dorg-test-mutate-owner-plan-capability-refuses-crafted-values ()
  "The shared plan validator refuses forged, foreign, stale, and nil authority."
  (let* ((family "DORG_CLOCK_")
         (execution (make-symbol "execution-"))
         (matching (dorg-mutate--owner-capability--make
                    :family family :execution-id execution))
         (foreign (dorg-mutate--owner-capability--make
                   :family "DORG_OTHER_" :execution-id execution)))
    (dolist (case
             (list
              (cons (list :owner-capability '(:family "DORG_CLOCK_")
                          :owner-execution-id execution)
                    'capability-forged)
              (cons (list :owner-capability foreign :owner-execution-id execution)
                    'capability-family-mismatch)
              (cons (list :owner-capability matching
                          :owner-execution-id (make-symbol "other-execution-"))
                    'capability-forged)
              (cons (list :owner-capability nil :owner-execution-id execution)
                    'capability-forged)))
      (let ((err (should-error (dorg-mutate--owner-plan-capability
                                (car case) family)
                               :type 'dorg-mutate-error)))
        (should (eq (cdr case) (plist-get (dorg-mutate-error-data err) :detail)))))
    (should (eq matching
                (dorg-mutate--owner-plan-capability
                 (list :owner-capability matching :owner-execution-id execution)
                 family)))))

(ert-deftest dorg-test-mutate-owner-plan-capability-authorizes-drawer-item ()
  "The whole-item engine validator accepts matching private plan authority."
  (let* ((family "DORG_CLOCK_")
         (execution (make-symbol "execution-"))
         (capability (dorg-mutate--owner-capability--make
                      :family family :execution-id execution))
         (plan (list :owner-capability capability :owner-execution-id execution))
         (item '(:kind drawer :name "PROPERTIES"
                 :properties (("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))
         (patch `(:op replace :target (:item :host (:heading :id 3) :select (:all))
                   :items (,item))))
    (should (eq item
                (dorg-mutate--patch-validate-item-value item 'replace plan)))
    (should (= 1 (length (dorg-mutate--validate-patches (list patch) plan))))))

(ert-deftest dorg-test-mutate-owner-plan-capability-refuses-system-drawer-key ()
  "Private family authority cannot smuggle DORG_ID through a drawer item."
  (let* ((family "DORG_CLOCK_")
         (execution (make-symbol "execution-"))
         (capability (dorg-mutate--owner-capability--make
                      :family family :execution-id execution))
         (plan (list :owner-capability capability :owner-execution-id execution))
         (item '(:kind drawer :name "PROPERTIES"
                 :properties (("DORG_ID" . "9"))))
         (patch `(:op replace :target (:item :host (:heading :id 3) :select (:all))
                   :items (,item))))
    (let ((err (should-error
                (dorg-mutate--validate-patches (list patch) plan)
                :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err))))))

(ert-deftest dorg-test-ay3q8-owner-plan-refuses-noncanonical-family-keys ()
  "Private lowering cannot bypass family case or KEY+ storage rules."
  (let* ((family "DORG_CLOCK_")
         (execution (make-symbol "execution-"))
         (capability (dorg-mutate--owner-capability--make
                      :family family :execution-id execution))
         (plan (list :owner-capability capability :owner-execution-id execution)))
    (dolist (key '("dorg_clock_start" "DORG_CLOCK_START+"))
      (let ((err (should-error
                  (dorg-mutate--validate-patches
                   (list (dorg-test-mutate--property-put-patch 3 key "x")) plan)
                  :type 'dorg-mutate-error)))
        (should (eq 'invalid-target (dorg-mutate-error-reason err)))
        (should (eq 'owned-metadata-noncanonical
                    (plist-get (dorg-mutate-error-data err) :detail)))))
    (let* ((item '(:kind drawer :name "PROPERTIES"
                  :properties (("dorg_clock_start" . "x"))))
           (patch `(:op replace :target (:item :host (:heading :id 3) :select (:all))
                     :items (,item)))
           (err (should-error (dorg-mutate--validate-patches (list patch) plan)
                              :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should (eq 'owned-metadata-noncanonical
                  (plist-get (dorg-mutate-error-data err) :detail))))))

(ert-deftest dorg-test-mutate-structural-owned-metadata-refuses-unavailable-owner ()
  "A structural deletion cannot silently discard a registered family value.

The control is deliberately pure: the immutable before/candidate images are
the mutation simulator's boundary immediately before projection or persistence.
Removing the preflight makes this assertion red, so it proves the preflight
rather than a later Dolt or fixture failure."
  (let* ((before (make-hash-table :test 'eql))
         (after (make-hash-table :test 'eql))
         (managed
          (make-dorg-heading
           :id 2 :parent-id 1 :items
           (list (dorg-drawer-item
                  :name "PROPERTIES"
                  :properties
                  '(("OWNER" . "me")
                    ("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))))
         (context
          (dorg-ext-index-context-create
           :patches (list (dorg-test-mutate--delete-patch 2 t))
           :deleted
           (list (dorg-ext-index-entry-create
                  :kind 'delete :id 2 :before managed :after nil))
           :base-image (progn (puthash 2 managed before) before)
           :candidate-postimage after)))
    (let ((err (should-error
                (dorg-mutate--assert-structural-owned-metadata-available context)
                :type 'dorg-mutate-error)))
      (should (eq 'invalid-target (dorg-mutate-error-reason err)))
      (should (eq 'owner-unavailable
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal "DORG_CLOCK_"
                     (plist-get (dorg-mutate-error-data err) :family))))))

(ert-deftest dorg-test-mutate-structural-demotion-refuses-unavailable-owner ()
  "A real same-store demotion cannot move family data around an absent owner.

The managed heading starts at the root and becomes a child of peer 9, the
parent/level change that represents a heading demotion rather than reparenting
between two existing parents."
  (let* ((before (make-dorg-heading
                  :id 2 :parent-id nil :level 1 :level-offset 1 :items
                  (list (dorg-drawer-item
                         :name "PROPERTIES"
                         :properties
                         '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))))
         (after (dorg-heading-deep-copy before))
         (context
          (dorg-ext-index-context-create
           :patches (list (dorg-test-mutate--move-patch
                           2 (dorg-test-mutate--heading-address 9)))
           :moved
           (list (dorg-ext-index-entry-create
                  :kind 'move :id 2 :before before :after after)))))
    (setf (dorg-heading-parent-id after) 9
          (dorg-heading-level after) 2)
    (let ((err (should-error
                (dorg-mutate--assert-structural-owned-metadata-available context)
                :type 'dorg-mutate-error)))
      (should (eq 'owner-unavailable
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal "DORG_CLOCK_"
                     (plist-get (dorg-mutate-error-data err) :family))))))

(ert-deftest dorg-test-mutate-structural-ancestor-delete-promotion-sees-managed-child ()
  "Deleting an unmarked ancestor still guards its promoted managed child.

The parent itself has no family key.  The only family observation is the
child's immutable before/after entry, which changes parent from 10 to 1 as a
heading delete promotes it.  Omitting that changed entry makes the control
vacuously green, so it proves the classified promotion boundary."
  (let* ((parent (make-dorg-heading :id 10 :parent-id 1 :items nil))
         (before-child
          (make-dorg-heading
           :id 11 :parent-id 10 :items
           (list (dorg-drawer-item
                  :name "PROPERTIES"
                  :properties
                  '(("DORG_CLOCK_START" . "2026-10-14T08:00:00Z"))))))
         (after-child (dorg-heading-deep-copy before-child))
         (context
          (dorg-ext-index-context-create
           :patches (list (dorg-test-mutate--delete-patch 10 nil))
           :deleted (list (dorg-ext-index-entry-create
                           :kind 'delete :id 10 :before parent :after nil))
           :changed (list (dorg-ext-index-entry-create
                           :kind 'change :id 11
                           :before before-child :after after-child)))))
    (setf (dorg-heading-parent-id after-child) 1)
    (let ((err (should-error
                (dorg-mutate--assert-structural-owned-metadata-available context)
                :type 'dorg-mutate-error)))
      (should (eq 'owner-unavailable
                  (plist-get (dorg-mutate-error-data err) :detail)))
      (should (equal "DORG_CLOCK_"
                     (plist-get (dorg-mutate-error-data err) :family))))))

(ert-deftest dorg-test-mutate-native-validator-protects-dorg-keywords ()
  "Every DORG_* keyword is protected at the engine boundary."
  (dolist (key '("DORG_ID" "DORG_HASH" "DORG_DOLT_DIR" "DORG_FUTURE"))
    (should
     (eq 'invalid-target
         (dorg-test-mutate--reason
          (dorg-mutate--validate-patches
           `((:op insert :target (:keyword :host :root :name ,key)
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
                     "FOO" occurrence :value "changed"))
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
         (on (id 1001) (replace (item (all)) (text "old")))
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
                                      :select (:occurrence 0)) :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :select (:occurrence 2)) :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :select (:occurrence 1.5)) :value "x"))
         (invalid-op invalid-position
          (:op insert :target (:text :host (:heading :id 1001))
               :position :front :value "x"))
         (invalid-op nil
          (:op insert :target (:text :host (:heading :id 1001))
               :value "x"))
         (invalid-target nil
          (:op insert :target (:text :host (:heading :id 1001)
                                      :select (:occurrence 1))
               :position :after-last :value "x"))
         (invalid-target nil
          (:op insert :target (:text :host (:heading :id 1001))
               :position :after-last :value 42))
         (invalid-target structural-payload-boundary
          (:op insert :target (:text :host (:heading :id 1001))
               :position :after-last :value "* fused"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:text :host (:heading :id 1001))
               :position :after-last :value "first\n\nsecond"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:keyword :host (:heading :id 1001) :name "K")
               :position :after-last :value "two\nlines"))
         (invalid-target structural-payload-boundary
          (:op insert :target (:drawer-line :host (:heading :id 1001)
                                             :name "AUDIT")
               :position :before-first :value "two\nlines"))
         (invalid-target missing
          (:op replace :target (:drawer-line :host (:heading :id 1001)
                                             :name "AUDIT" :select (:occurrence 1))
               :value "- refused"))
         (invalid-target missing
          (:op delete :target (:drawer-line :host (:heading :id 1001)
                                             :name "AUDIT" :select (:occurrence 1))))
         (invalid-target retired-ir
          (:op replace :target (:text :host (:heading :id 1001)
                                      :select (:occurrence 1)) :missing :create :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001))
               :value "x"))
         (invalid-target nil
          (:op replace :target (:text :host (:heading :id 1001)
                                      :select (:occurrence 1)) :value nil))))
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
                   :id 7 :parent-id nil :level 1 :level-offset 1 :post-blank 0
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

(ert-deftest dorg-test-hiyva-plain-planning-replaces-one-raw-field ()
  "Plain planning edits replace only the selected raw field bytes.
The deadline, property drawer and body make this fail if the editor renders a
whole planning section rather than splicing the selected raw timestamp."
  (let* ((before (concat "* Task\n"
                         "SCHEDULED: <2026-01-02 Fri> DEADLINE: <2026-01-03 Sat>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:KEEP: yes\n:END:\n"
                         "Body.\nSCHEDULED: <2099-01-01 Thu>\n"))
         (after (concat "* Task\n"
                        "SCHEDULED: <2026-02-02 Mon> DEADLINE: <2026-01-03 Sat>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:KEEP: yes\n:END:\n"
                        "Body.\nSCHEDULED: <2099-01-01 Thu>\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-api-execute
       '(on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
       :mode 'apply :source buf)
      (should-not (equal before after))
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-replaces-no-space-timestamp ()
  "A legal planning token keeps its zero-space colon geometry on replacement."
  (let* ((before (concat "* Task\nSCHEDULED:<2026-01-02 Fri> DEADLINE: <2026-01-03 Sat>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n"))
         (after (concat "* Task\nSCHEDULED:<2026-02-02 Mon> DEADLINE: <2026-01-03 Sat>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-api-execute
       '(on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
       :mode 'apply :source buf)
      (should-not (equal before after))
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-persists-explicit-file-source ()
  "A planning apply through `:file' writes the exact projected bytes."
  (let* ((before (concat "* Task\nSCHEDULED: <2026-01-02 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
         (after (concat "* Task\nSCHEDULED: <2026-02-02 Mon>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
         (file (make-temp-file "dorg-hiyva-file" nil ".org" before)))
    (unwind-protect
        (progn
          (dorg-api-execute
           '(on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
           :mode 'apply :source (list :file file))
          (should (equal after
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (buffer-string)))))
      (delete-file file))))

(ert-deftest dorg-test-hiyva-plain-planning-owns-maximal-separators ()
  "Planning clear owns only its declared maximal horizontal separator run.

The literal rows cover a tab after SCHEDULED, two spaces after SCHEDULED,
two spaces before a last DEADLINE, and an indented clear-last line."
  (dolist
      (row
       `(("* Task\nSCHEDULED: <2026-02-02 Mon>\tDEADLINE: <2026-01-03 Sat>\n"
          (:scheduled nil)
          "* Task\nDEADLINE: <2026-01-03 Sat>\n")
         ("* Task\nSCHEDULED: <2026-02-02 Mon>  DEADLINE: <2026-01-03 Sat>\n"
          (:scheduled nil)
          "* Task\nDEADLINE: <2026-01-03 Sat>\n")
         ("* Task\nSCHEDULED: <2026-02-02 Mon>  DEADLINE: <2026-01-03 Sat>\n"
          (:deadline nil)
          "* Task\nSCHEDULED: <2026-02-02 Mon>\n")
         ("* Task\n  SCHEDULED: <2026-01-02 Fri>\n"
          (:scheduled nil)
          "* Task\n")))
    (pcase-let ((`(,before ,fields ,after) row))
      (setq before (concat before ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
      (setq after (concat after ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
      (dorg-test-mutate--with-content buf before
        (dorg-test-mutate--apply-patch-batch
         buf (list (apply #'dorg-test-mutate--planning-patch 1001 fields)))
        (should-not (equal before after))
        (should (equal after (buffer-string)))))))

(ert-deftest dorg-test-hiyva-plain-planning-inserts-before-properties ()
  "An absent planning line is inserted immediately below its heading."
  (let* ((before (concat "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:KEEP: yes\n:END:\nBody.\n"))
         (after (concat "* Task\nSCHEDULED: <2026-02-02 Mon>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:KEEP: yes\n:END:\nBody.\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--apply-patch-batch
       buf (list (dorg-test-mutate--planning-patch
                  1001 :scheduled "<2026-02-02 Mon>")))
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-dry-run-matches-apply ()
  "Planning dry-run uses the real editor and predicts apply bytes exactly."
  (let* ((before (concat "* Task\nSCHEDULED: <2026-01-02 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
         (form '(on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))))
    (dorg-test-mutate--with-content buf before
      (let* ((preview (dorg-api-execute form :mode 'dry-run :source buf))
             (projected (plist-get (car (plist-get preview :projections)) :after)))
        (should (equal before (buffer-string)))
        (dorg-api-execute form :mode 'apply :source buf)
        (should-not (equal before (buffer-string)))
        (should (equal projected (buffer-string)))))))

(ert-deftest dorg-test-hiyva-plain-planning-same-heading-batch-composes ()
  "Ordered planning patches share phase-one simulation and shifted spans."
  (let* ((before (concat "* Task\nSCHEDULED: <2026-01-02 Fri> DEADLINE: <2026-01-03 Sat>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
         (after (concat "* Task\nSCHEDULED: <2026-02-02 Mon> DEADLINE: <2026-02-03 Tue>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
         (form '(batch
                 (on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
                 (on (id 1001) (set (planning deadline) "<2026-02-03 Tue>")))))
    (dorg-test-mutate--with-content buf before
      (dorg-api-execute form :mode 'apply :source buf)
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-two-heading-batch-lockstep ()
  "A two-heading planning batch has matching dry-run projections and apply bytes."
  (let* ((before (concat "* One\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                         "* Two\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\n"))
         (after (concat "* One\nSCHEDULED: <2026-02-02 Mon>\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                        "* Two\nSCHEDULED: <2026-02-03 Tue>\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\n"))
         (form '(batch
                 (on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
                 (on (id 1002) (set (planning scheduled) "<2026-02-03 Tue>")))))
    (dorg-test-mutate--with-content buf before
      (let ((preview (dorg-api-execute form :mode 'dry-run :source buf)))
        (should (equal before (buffer-string)))
        (should (= 2 (length (plist-get preview :projections))))
        (should (string-match-p "SCHEDULED: <2026-02-02 Mon>"
                                (plist-get (nth 0 (plist-get preview :projections)) :after)))
        (should (string-match-p "SCHEDULED: <2026-02-03 Tue>"
                                (plist-get (nth 1 (plist-get preview :projections)) :after))))
      (dorg-api-execute form :mode 'apply :source buf)
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-later-invalid-batch-is-atomic ()
  "A later unrepresentable planning request leaves the earlier heading intact."
  (let ((before (concat "* One\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\n"
                        "* Two\nSCHEDULED: <2026-01-02 Fri>\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (let ((err (should-error
                  (dorg-api-execute
                   '(batch
                     (on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
                     (on (id 1002) (set (planning scheduled) "tomorrow")))
                  :mode 'apply :source buf)
                  :type 'dorg-mutate-error)))
        (should (eq 'invalid-target (dorg-mutate-error-reason err)))
        (should (eq 'planning-line-unrepresentable
                    (plist-get (dorg-mutate-error-data err) :detail))))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-preserves-unrelated-metadata ()
  "A literal planning edit cannot alter recurrence metadata or completion data."
  (let* ((before (concat "* TODO Task\n"
                         "SCHEDULED: <2026-01-02 Fri> DEADLINE: <2026-01-03 Sat> CLOSED: [2026-01-04 Sun]\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:DORG_SCHEDULED_RULE: +1w\n:DORG_DEADLINE_RULE: +2d\n:END:\n"
                         ":LOGBOOK:\n- State TODO\n:END:\nBody.\n** Child\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\nChild.\n"))
         (after (concat "* TODO Task\n"
                        "SCHEDULED: <2026-02-02 Mon> DEADLINE: <2026-01-03 Sat> CLOSED: [2026-01-04 Sun]\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:DORG_SCHEDULED_RULE: +1w\n:DORG_DEADLINE_RULE: +2d\n:END:\n"
                        ":LOGBOOK:\n- State TODO\n:END:\nBody.\n** Child\n:PROPERTIES:\n:DORG_ID: 1002\n:END:\nChild.\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-api-execute
       '(on (id 1001) (set (planning scheduled) "<2026-02-02 Mon>"))
       :mode 'apply :source buf)
      (should (equal after (buffer-string))))))

(ert-deftest dorg-test-hiyva-plain-planning-refuses-unrepresentable-input ()
  "Malformed and injected planning values refuse without a byte change."
  (dolist (row `(("SCHEDULED: tomorrow\n" planning-line-malformed)
                 ("scheduled: <2026-02-02 Mon>\n" planning-line-malformed)
                 ("SCHEDULED: <2026-02-02 Mon> junk DEADLINE: <2026-01-03 Sat>\n"
                  planning-line-malformed)))
    (pcase-let ((`(,line ,detail) row))
      (let ((before (concat "* Task\n" line ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
        (dorg-test-mutate--with-content buf before
          (let ((data (dorg-test-mutate--error-data
                       (dorg-test-mutate--apply-patch-batch
                        buf (list (dorg-test-mutate--planning-patch
                                   1001 :scheduled "<2026-03-02 Mon>"))))))
            (should (eq 'invalid-target (plist-get data :reason)))
            (should (eq detail (plist-get data :detail)))
            (should (equal before (buffer-string)))))))
  (let ((before (concat "* Task\nSCHEDULED: <2026-01-02 Fri>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list (dorg-test-mutate--planning-patch
                               1001 :scheduled
                               "<2026-02-02 Mon> DEADLINE: <2027-01-01 Fri>"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'planning-line-unrepresentable (plist-get data :detail)))
        (should (equal before (buffer-string)))))))
  (let ((before (concat "* Task\nSCHEDULED: <2026-01-02 Fri>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list (dorg-test-mutate--planning-patch
                               1001 :scheduled "tomorrow"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'planning-line-unrepresentable (plist-get data :detail)))
        (should (equal "tomorrow" (plist-get data :value)))
        (should (equal before (buffer-string))))))
  (let ((before (concat "* Task\n"
                        "SCHEDULED: <2026-01-02 Fri> SCHEDULED: <2026-02-02 Mon>\n"
                        ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list (dorg-test-mutate--planning-patch
                               1001 :scheduled "<2026-03-02 Mon>"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'planning-line-ambiguous (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-hiyva-plain-planning-refuses-unterminated-source ()
  "A missing final newline has its own fail-closed planning diagnostic."
  (let ((before "* Task\nSCHEDULED: <2026-01-02 Fri>\n:PROPERTIES:\n:DORG_ID: 1001\n:END:"))
    (dorg-test-mutate--with-content buf before
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list (dorg-test-mutate--planning-patch
                               1001 :scheduled "<2026-02-02 Mon>"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'planning-line-unterminated (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-hiyva-plain-planning-preserves-representable-raw-values ()
  "Plain planning stores representable raw Org forms without normalization."
  (dolist (value '("<2026-02-02 Mon 10:00-11:00 +1w -2d>"
                   "<2026-02-02 Mon>--<2026-02-03 Tue>"
                   "<%%(diary-float t 4 2)>"
                   "[2026-02-02 Mon]"))
    (let ((before "* Task\n:PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))
      (dorg-test-mutate--with-content buf before
        (dorg-test-mutate--apply-patch-batch
         buf (list (dorg-test-mutate--planning-patch 1001 :scheduled value)))
        (should (equal (concat "* Task\nSCHEDULED: " value "\n"
                               ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n")
                       (buffer-string)))))))

(ert-deftest dorg-test-hiyva-plain-planning-refuses-diary-sexp-keyword-without-evaluation ()
  "An unrepresentable diary sexp refuses in both modes without evaluation.
The sexp intentionally contains `SCHEDULED:' to pin that scanner-looking text
does not execute while representability is checked."
  (let* ((counter 0)
         (value "<%%(dorg-test-hiyva--diary-trap \"SCHEDULED:\")>")
         (before (concat "* Task\nSCHEDULED: <2026-01-02 Fri>\n"
                         ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
    (cl-letf (((symbol-function 'dorg-test-hiyva--diary-trap)
               (lambda (&rest _args) (setq counter (1+ counter)) "ignored")))
      (dorg-test-mutate--with-content buf before
      (dolist (mode '(dry-run apply))
        (condition-case err
            (progn
              (dorg-api-execute
               `(on (id 1001) (set (planning scheduled) ,value))
               :mode mode :source buf)
              (ert-fail "diary sexp request unexpectedly succeeded"))
          ((dorg-api-error dorg-mutate-error)
           (let ((data (dorg-error-data err)))
             (should (eq 'invalid-target (plist-get data :reason)))
             (should (eq 'planning-line-unrepresentable (plist-get data :detail)))
             (should (equal value (plist-get data :value))))))
        (should (= 0 counter))
        (should (equal before (buffer-string))))
    ;; The same malformed diary text in source is never evaluated or written.
    ;; The current whole-line duplicate guard reports it before splice.
    (let ((source (concat "* Task\nSCHEDULED: " value "\n"
                          ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
      (dorg-test-mutate--with-content buf source
        (let ((data (dorg-test-mutate--error-data
                     (dorg-test-mutate--apply-patch-batch
                      buf (list (dorg-test-mutate--planning-patch
                                 1001 :deadline "<2026-03-02 Mon>"))))))
          (should (eq 'invalid-target (plist-get data :reason)))
          (should (equal source (buffer-string)))
          (should (= 0 counter)))))
    ;; A real second field is still the unambiguous duplicate witness.
    (let ((duplicate (concat "* Task\nSCHEDULED: <2026-01-02 Fri> "
                             "SCHEDULED: <2026-02-02 Mon>\n"
                             ":PROPERTIES:\n:DORG_ID: 1001\n:END:\n")))
      (dorg-test-mutate--with-content buf duplicate
        (let ((data (dorg-test-mutate--error-data
                     (dorg-test-mutate--apply-patch-batch
                      buf (list (dorg-test-mutate--planning-patch
                                 1001 :deadline "<2026-03-02 Mon>"))))))
          (should (eq 'invalid-target (plist-get data :reason)))
          (should (eq 'planning-line-ambiguous (plist-get data :detail)))
          (should (equal duplicate (buffer-string))))))))))

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

(ert-deftest dorg-test-mutate-exec-update-heading-post-blank-refuses ()
  "Post-blank remains outside the plain heading-line editor and refuses pre-write.
The ordinary title/TODO/priority/tags/COMMENTED cells are covered by 1qdtb's
literal transformation tests; this retains the whole-field C3 protection."
  (dorg-test-mutate--with-buffer buf
    (let* ((before (buffer-string))
           (d (dorg-test-mutate--error-data
               (dorg-test-mutate--apply-patch-batch
                buf (list (dorg-test-mutate--heading-update-patch
                           1001 :post-blank 2))))))
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
            :target (:text :host (:heading :id 1001) :select (:occurrence 1)) :value "Rewritten body.")))
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

;;; ─── Dolt order-fact result coercion ──────────────────────────────────

(ert-deftest dorg-test-mutate-dolt-order-facts-preserve-hex-after-json-parse ()
  "Order facts preserve numeric-only hex text from the real JSON parser.
The private `dorg-sql' response seam avoids a SQL process, but the test uses
the real `dorg-sql--parse-result' and `dorg-mutate--dolt-order-facts' path.
IDs arrive as JSON numbers and HEX sort keys as JSON strings; the parser keeps
both, so numeric-only hex stays text until decoded."
  (let ((rows (with-temp-buffer
                (insert (concat "{\"rows\":["
                                "{\"id\":107,\"sort_key_hex\":\"01\"},"
                                "{\"id\":108,\"sort_key_hex\":\"00\"},"
                                "{\"id\":109,\"sort_key_hex\":\"AF\"}]}"))
                (dorg-sql--parse-result (current-buffer) 0 "order facts"))))
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dolt-dir sql &rest _)
                 (should (string-match-p "HEX(sort_key) AS sort_key_hex" sql))
                 rows)))
      (should (equal (list (cons 107 (unibyte-string 1))
                           (cons 108 (unibyte-string 0))
                           (cons 109 (unibyte-string #xaf)))
                     (dorg-mutate--dolt-order-facts "/tmp/0cjqb" 100 101))))))

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

(defun dorg-test-mutate--tree-child-keys (tree parent-id)
  "Return direct child sort keys of PARENT-ID from TREE in sibling order."
  (mapcar #'dorg-heading-sort-key
          (sort
           (cl-loop for heading being the hash-values of tree
                    when (equal parent-id (dorg-heading-parent-id heading))
                    collect heading)
           (lambda (a b)
             (string< (dorg-heading-sort-key a)
                      (dorg-heading-sort-key b))))))

(defun dorg-test-mutate--tree-children (tree parent-id)
  "Return direct children of PARENT-ID from TREE in sibling order."
  (sort
   (cl-loop for heading being the hash-values of tree
            when (equal parent-id (dorg-heading-parent-id heading))
            collect heading)
   (lambda (a b)
     (string< (dorg-heading-sort-key a)
              (dorg-heading-sort-key b)))))

(defun dorg-test-mutate--assert-tree-sibling-order (tree parent-id)
  "Assert PARENT-ID's direct child keys in TREE are unique and sub-cliff."
  (let ((keys (dorg-test-mutate--tree-child-keys tree parent-id)))
    (should (cl-loop for (a b) on keys
                     while b
                     always (string< a b)))
    (should (cl-every (lambda (key) (< (length key) dorg-sortkey-max-bytes))
                      keys))))

(defun dorg-test-mutate--dolt-keyword-values (tree id key)
  "Return the ordered list of KEY keyword-item values for heading ID in TREE."
  (let ((h (gethash id tree)) (out nil))
    (dolist (it (and h (dorg-heading-items h)) (nreverse out))
      (when (and (cl-typep it 'dorg-keyword-item)
                 (equal (dorg-keyword-item-keyword it) key))
        (push (dorg-keyword-item-value it) out)))))

(defun dorg-test-mutate--be16 (n)
  "Return the fixed-width item boundary key for integer N."
  (dorg-sortkey-from-integer n dorg-sortkey-boundary-width))

(defun dorg-test-mutate--be4 (n)
  "Return the four-byte suffix key for integer N."
  (dorg-sortkey-from-integer n 4))

(defun dorg-test-mutate--ac4-text-rows ()
  "Return aged AC4 text rows whose 16-byte prefix is saturated.
The first row is exactly sixteen #xff bytes and the next twelve carry be4(N)
suffixes, so no append can find room after the last key at the fixed width:
any append renormalizes the complete list, rewriting every reused row's key
to a fresh one that equals none of the old keys."
  (let ((base (apply #'unibyte-string (make-list 16 #xff))))
    (cl-loop for id from 42 to 54
             for key = (if (= id 42)
                           base
                         (concat base (dorg-test-mutate--be4 (- id 42))))
             collect (dorg-text-item :id id
                                     :sort-key key
                                     :text (format "item-%d" id)))))

(defun dorg-test-mutate--seed-text-heading (dolt-dir rows)
  "Seed DOLT-DIR with root 100, text heading 200, and DB-bound ROWS."
  (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
         (h (make-dorg-heading :id 200 :parent-id 100 :level 1 :level-offset 1 :title "H"
                               :sort-key (unibyte-string #x80)
                               :items rows)))
    (dorg-heading-compute-items-hash h)
    (dorg-sql-batch
     dolt-dir
     (append
      (list (dorg-sql-gen-heading-insert root-h 100)
            (dorg-sql-gen-heading-insert h 100))
      (cl-loop for row in rows
               append (dorg-item-to-sql row
                                        (dorg-item-id row)
                                        200
                                        (dorg-item-sort-key row)))))
    (dorg-commit dolt-dir "seed")
    200))

(defun dorg-test-mutate--dolt-text-items (dolt-dir root-id heading-id)
  "Return loaded text items for HEADING-ID from DOLT-DIR/ROOT-ID."
  (let* ((tree (dorg-model-load-tree dolt-dir root-id))
         (heading (gethash heading-id tree)))
    (should heading)
    (cl-remove-if-not (lambda (item) (cl-typep item 'dorg-text-item))
                      (dorg-heading-items heading))))

(defun dorg-test-mutate--text-append-patch (heading-id text)
  "Return a typed text append patch adding TEXT under HEADING-ID."
  `(:op insert
    :target (:text :host (:heading :id ,heading-id))
    :position :after-last
    :value ,text))

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
  "Property put and planning update persist; an explicit clear removes it."
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
      ;; Clearing the last stored field removes the planning line.
      (dorg-test-mutate--apply-patch-batch
       src (list (dorg-test-mutate--planning-patch hid :scheduled nil)))
      (let* ((tree (dorg-model-load-tree dolt-dir root))
             (planning (cl-find-if (lambda (it) (cl-typep it 'dorg-planning-item))
                                   (dorg-heading-items (gethash hid tree)))))
        (should-not planning)))))

(defun dorg-test-mutate--planning-raw-row (dolt-dir heading-id)
  "Return the first planning row's raw fields for HEADING-ID in DOLT-DIR."
  (car (dorg-sql
        dolt-dir
        (format (concat "SELECT pi.scheduled_raw, pi.deadline_raw, pi.closed_raw "
                        "FROM planning_items pi "
                        "JOIN heading_items hi ON hi.id = pi.item_id "
                        "WHERE hi.heading_id = %d ORDER BY hi.sort_key LIMIT 1")
                heading-id))))

(ert-deftest dorg-test-mutate-dolt-public-planning-preserves-sibling-fields ()
  "Public planning set preserves raw siblings and neighboring body items.
This is the native AC4/D3 control: reverting key-presence semantics erases
DEADLINE/CLOSED and fails after reload."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* H\n"
                          "SCHEDULED: <2026-05-29 Thu> "
                          "DEADLINE: <2026-05-30 Fri> "
                          "CLOSED: [2026-05-31 Sat]\n"
                          "Neighbour body.\n")))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (before (dorg-test-mutate--planning-raw-row dolt-dir hid))
           (deadline (alist-get 'deadline_raw before))
           (closed (alist-get 'closed_raw before)))
      (should deadline)
      (should closed)
      (should (plist-get
               (dorg-api-execute
                `(on (id ,hid) (set (planning scheduled) "<2026-06-01 Mon>"))
                :mode 'apply :source src)
               :ok))
      (let* ((after (dorg-test-mutate--planning-raw-row dolt-dir hid))
             (heading (gethash hid (dorg-model-load-tree dolt-dir root))))
        (should (equal "<2026-06-01 Mon>" (alist-get 'scheduled_raw after)))
        (should (equal deadline (alist-get 'deadline_raw after)))
        (should (equal closed (alist-get 'closed_raw after)))
        (should (equal '((planning "<2026-06-01 Mon>" "<2026-05-30 Fri>"
                                   "[2026-05-31 Sat]")
                         (text "Neighbour body.\n"))
                       (dorg-test-mutate--items-shape
                        (dorg-heading-items heading)))))
      (should (plist-get
               (dorg-api-execute
                `(on (id ,hid)
                     (set (planning scheduled) "<2026-06-02 Tue>")
                     (set (planning deadline) "<2026-06-03 Wed>"))
                :mode 'apply :source src)
               :ok))
      (let* ((after-batch (dorg-test-mutate--planning-raw-row dolt-dir hid))
             (heading (gethash hid (dorg-model-load-tree dolt-dir root))))
        (should (equal "<2026-06-02 Tue>"
                       (alist-get 'scheduled_raw after-batch)))
        (should (equal "<2026-06-03 Wed>"
                       (alist-get 'deadline_raw after-batch)))
        (should (equal closed (alist-get 'closed_raw after-batch)))
        (should (equal '((planning "<2026-06-02 Tue>" "<2026-06-03 Wed>"
                                   "[2026-05-31 Sat]")
                         (text "Neighbour body.\n"))
                       (dorg-test-mutate--items-shape
                        (dorg-heading-items heading)))))
      (should (plist-get
               (dorg-api-execute
                `(on (id ,hid) (set (planning scheduled) nil))
                :mode 'apply :source src)
               :ok))
      (let* ((after-clear (dorg-test-mutate--planning-raw-row dolt-dir hid))
             (heading (gethash hid (dorg-model-load-tree dolt-dir root))))
        (should-not (alist-get 'scheduled_raw after-clear))
        (should (equal "<2026-06-03 Wed>"
                       (alist-get 'deadline_raw after-clear)))
        (should (equal closed (alist-get 'closed_raw after-clear)))
        (should (equal '((planning nil "<2026-06-03 Wed>" "[2026-05-31 Sat]")
                         (text "Neighbour body.\n"))
                       (dorg-test-mutate--items-shape
                        (dorg-heading-items heading))))))))

(ert-deftest dorg-test-hiyva-dolt-planning-stores-tomorrow-literally ()
  "Dolt keeps the raw planning domain where plain Org rejects `tomorrow'."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\n"))
           (hid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root)))
      (should (plist-get
               (dorg-api-execute
                `(on (id ,hid) (set (planning scheduled) "tomorrow"))
                :mode 'apply :source src)
               :ok))
      (should (equal "tomorrow"
                     (alist-get 'scheduled_raw
                                (dorg-test-mutate--planning-raw-row dolt-dir hid)))))))

(defun dorg-test-hiyva--planning-source (fields)
  "Return an exact plain fixture with planning FIELDS and identity 1001.
FIELDS is an alist whose keys are the public field symbols and whose values are
raw timestamp strings.  The helper deliberately fixes the non-planning suffix
so every plain parity row proves the complete resulting bytes."
  (concat "* Task\n"
          (when fields
            (concat
             (mapconcat
              (lambda (entry)
                (format "%s: %s" (upcase (symbol-name (car entry))) (cdr entry)))
              fields " ")
             "\n"))
          ":PROPERTIES:\n:DORG_ID: 1001\n:END:\nBody.\n"))

(defun dorg-test-hiyva--planning-intent (id fields)
  "Return one public planning intent for ID and FIELD alist."
  (append (list 'on (list 'id id))
          (mapcar (lambda (entry)
                    (list 'set (list 'planning (car entry)) (cdr entry)))
                  fields)))

(defconst dorg-test-hiyva--planning-parity-rows
  '((scheduled-set
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((scheduled . "<2026-02-02 Mon>"))
     ((scheduled . "<2026-02-02 Mon>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]")))
    (scheduled-clear
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((scheduled))
     ((deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]")))
    (scheduled-clear-last ((scheduled . "<2026-01-02 Fri>")) ((scheduled)) nil)
    (scheduled-unchanged ((scheduled . "<2026-01-02 Fri>")) ((scheduled . "<2026-01-02 Fri>")) ((scheduled . "<2026-01-02 Fri>")))
    (deadline-set
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((deadline . "<2026-02-03 Tue>"))
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-02-03 Tue>") (closed . "[2026-01-04 Sun]")))
    (deadline-clear
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((deadline))
     ((scheduled . "<2026-01-02 Fri>") (closed . "[2026-01-04 Sun]")))
    (deadline-clear-last ((deadline . "<2026-01-03 Sat>")) ((deadline)) nil)
    (deadline-unchanged ((deadline . "<2026-01-03 Sat>")) ((deadline . "<2026-01-03 Sat>")) ((deadline . "<2026-01-03 Sat>")))
    (closed-set
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((closed . "[2026-02-04 Wed]"))
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-02-04 Wed]")))
    (closed-clear
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((closed))
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>")))
    (closed-clear-last ((closed . "[2026-01-04 Sun]")) ((closed)) nil)
    (closed-unchanged ((closed . "[2026-01-04 Sun]")) ((closed . "[2026-01-04 Sun]")) ((closed . "[2026-01-04 Sun]")))
    (scheduled-deadline-two-field-batch
     ((scheduled . "<2026-01-02 Fri>") (deadline . "<2026-01-03 Sat>") (closed . "[2026-01-04 Sun]"))
     ((scheduled . "<2026-02-02 Mon>") (deadline . "<2026-02-03 Tue>"))
     ((scheduled . "<2026-02-02 Mon>") (deadline . "<2026-02-03 Tue>") (closed . "[2026-01-04 Sun]"))))
  "Named shared planning operation rows required by hiyva R5.")

(ert-deftest dorg-test-hiyva-plain-planning-parity-rows ()
  "Every named R5 operation has an exact plain byte witness."
  (dolist (row dorg-test-hiyva--planning-parity-rows)
    (pcase-let ((`(,name ,before-fields ,patches ,after-fields) row))
      (let ((before (dorg-test-hiyva--planning-source before-fields))
            (after (dorg-test-hiyva--planning-source after-fields)))
        (dorg-test-mutate--with-content buf before
          (should (plist-get
                   (dorg-api-execute
                    (dorg-test-hiyva--planning-intent 1001 patches)
                    :mode 'apply :source buf)
                   :ok))
          (should (equal after (buffer-string)))
          ;; Keep NAME in the failure context if a row becomes non-discriminating.
          (should (or (not (equal before after))
                      (string-suffix-p "unchanged" (symbol-name name)))))))))

(ert-deftest dorg-test-hiyva-dolt-planning-parity-rows ()
  "Every named R5 operation has the same resulting Dolt planning values."
  (dorg-test-mutate--with-dolt
    (let ((root-id 100))
      (dolist (row dorg-test-hiyva--planning-parity-rows)
      (pcase-let ((`(,name ,before-fields ,patches ,after-fields) row))
        (let* ((identity (1+ root-id))
               (contents (replace-regexp-in-string
                          ":DORG_ID: 1001" (format ":DORG_ID: %d" identity)
                          (dorg-test-hiyva--planning-source before-fields)
                          t t))
               (title (symbol-name name))
               (root (dorg-test-mutate--seed
                      dolt-dir
                      (concat "* " title "\n" (substring contents (length "* Task\n")))
                      root-id))
               (hid (dorg-test-mutate--id-by-title dolt-dir title))
               (source (list :dolt-dir dolt-dir :root-id root)))
          (should hid)
          (should (plist-get
                   (dorg-api-execute
                    (dorg-test-hiyva--planning-intent hid patches)
                    :mode 'apply :source source)
                   :ok))
          (let ((actual (dorg-test-mutate--planning-raw-row dolt-dir hid)))
            (dolist (field '((scheduled . scheduled_raw)
                             (deadline . deadline_raw)
                             (closed . closed_raw)))
              (should (equal (alist-get (car field) after-fields)
                             (alist-get (cdr field) actual)))))
          ;; NAME is a named matrix cell, even when it is semantically a no-op.
          (should (symbolp name)))
        (setq root-id (+ root-id 100)))))))

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
                  "FOO" 1 :value "b")))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) hid "FOO")
                     '("b")))
      (dorg-test-mutate--apply-patch-batch
       src `((:op replace :target (:item :host (:heading :id ,hid) :select (:all))
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
       src (list '(:op replace
                   :target (:keyword :host :root :name "OPTIONS"
                                     :select (:occurrence 2))
                   :value "y2")))
      ;; Insert a third after the last same-key occurrence.
      (dorg-test-mutate--apply-patch-batch
       src (list '(:op insert :target (:keyword :host :root :name "OPTIONS")
                   :position :after-last :value "z3")))
      (should (equal (dorg-test-mutate--dolt-keyword-values
                      (dorg-model-load-tree dolt-dir root) root "OPTIONS")
                     '("x" "y2" "z3")))
      ;; Delete only the first occurrence.
      (dorg-test-mutate--apply-patch-batch
       src (list '(:op delete
                   :target (:keyword :host :root :name "OPTIONS"
                                     :select (:occurrence 1)))))
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
       src (list '(:op insert :target (:keyword :host :root :name "AUTHOR")
                   :position :after-last :value "OC")))
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
                  'replace :root "OPTIONS" 1 :value "x2")))
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

(ert-deftest dorg-test-mutate-postimage-validates-all-owner-rows-without-wire ()
  "Every row of every present partition validates; deleted scope stays independent."
  (let ((validated nil))
    (cl-labels
        ((heading (id)
           (let ((h (make-dorg-heading :id id)))
             (dorg-heading-compute-items-hash h)
             h))
         (image (ids)
           (let ((out (make-hash-table :test 'eql)))
             (dolist (id ids) (puthash id (heading id) out))
             out))
         (expected (ids)
           (let ((out (make-hash-table :test 'eql)))
             (dolist (id ids)
               (let ((h (heading id)))
                 (puthash id
                          (cons (dorg-mutate--index-heading-canonical h)
                                (dorg-mutate--postimage-item-row-canonical h))
                          out)))
             out)))
      (let ((specs (list (list 100 1 '(1 2 3) (expected '(1 2 3)) nil)
                         (list 100 10 '(10 11) (expected '(10 11)) nil)
                         (list 100 20 nil nil t)))
            (canonical (symbol-function 'dorg-mutate--postimage-item-row-canonical)))
        (cl-letf (((symbol-function 'dorg-mutate--postimage-item-row-canonical)
                   (lambda (h)
                     (push (dorg-heading-id h) validated)
                     (funcall canonical h))))
          (dorg-mutate--postimage-assert-partitions
           (lambda (request) (image (plist-get request :ids))) specs))
        (should (equal '(1 2 3 10 11) (nreverse validated)))))))

(ert-deftest dorg-test-mutate-dolt-ac4-aged-item-append-commits-atomically ()
  "AC4: aged item normalization and the requested append persist atomically.
The Dolt seed saturates the 16-byte prefix, so the append renormalizes every
reused row to a fresh 16-byte key that equals none of the old keys; no temporary
key is needed.  The committed image preserves row ids, content, and order."
  (dorg-test-mutate--with-dolt
    (let* ((rows (dorg-test-mutate--ac4-text-rows))
           (old-keys (mapcar #'dorg-item-sort-key rows))
           (hid (dorg-test-mutate--seed-text-heading dolt-dir rows))
           (src (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir)))
      (let ((dorg-mutate--dolt-item-id-generator (lambda () 55)))
        (let ((res (dorg-test-mutate--apply-patch-batch
                    src (list (dorg-test-mutate--text-append-patch
                               hid "appended")))))
          (should (plist-get res :ok))
          (should (member hid (plist-get res :changed)))))
      (should-not (equal base (dorg-head-commit dolt-dir)))
      (let* ((items (dorg-test-mutate--dolt-text-items dolt-dir 100 hid))
             (ids (mapcar #'dorg-item-id items))
             (texts (mapcar #'dorg-text-item-text items))
             (keys (mapcar #'dorg-item-sort-key items)))
        (should (equal ids (number-sequence 42 55)))
        (should (equal texts
                       (append (cl-loop for id from 42 to 54
                                        collect (format "item-%d" id))
                               '("appended"))))
        (should-not (equal old-keys keys))
        (should (dorg-sortkey-ordered-p keys))
        (should (cl-every (lambda (key) (= (length key) 16)) keys))))))

(ert-deftest dorg-test-mutate-dolt-ac4-rollback-keeps-aged-image ()
  "AC4: a forced write failure rolls back both normalization and append.
The generated appended text id is forced to collide with an existing item row,
after the item plan has computed normalized keys.  The transaction abort leaves
HEAD and the aged DB image unchanged."
  (dorg-test-mutate--with-dolt
    (let* ((rows (dorg-test-mutate--ac4-text-rows))
           (old-keys (mapcar #'dorg-item-sort-key rows))
           (hid (dorg-test-mutate--seed-text-heading dolt-dir rows))
           (src (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir)))
      (let ((dorg-mutate--dolt-item-id-generator (lambda () 42)))
        (condition-case err
            (progn
              (dorg-test-mutate--apply-patch-batch
               src (list (dorg-test-mutate--text-append-patch hid "appended")))
              (ert-fail "expected constraint error"))
          (dorg-mutate-error
           (should (eq (dorg-mutate-error-reason err) 'invariant))
           (should (eq (plist-get (dorg-mutate-error-data err) :detail)
                       'constraint)))))
      (should (equal base (dorg-head-commit dolt-dir)))
      (let* ((items (dorg-test-mutate--dolt-text-items dolt-dir 100 hid))
             (ids (mapcar #'dorg-item-id items))
             (texts (mapcar #'dorg-text-item-text items))
             (keys (mapcar #'dorg-item-sort-key items)))
        (should (equal ids (number-sequence 42 54)))
        (should (equal texts
                       (cl-loop for id from 42 to 54
                                collect (format "item-%d" id))))
        (should (equal keys old-keys))))))

(ert-deftest dorg-test-mutate-dolt-ac4-lost-rekey-caught-by-postimage ()
  "AC4: database-side postimage parity catches a lost persisted rekey.
The test mutates the generated item SQL by removing id 42's rekey.  SQL
constraints still allow that image, so the postimage verifier must reject the
committed rows and roll back."
  (dorg-test-mutate--with-dolt
    (let* ((rows (dorg-test-mutate--ac4-text-rows))
           (old-keys (mapcar #'dorg-item-sort-key rows))
           (hid (dorg-test-mutate--seed-text-heading dolt-dir rows))
           (src (list :dolt-dir dolt-dir :root-id 100))
           (base (dorg-head-commit dolt-dir))
           (real-lower (symbol-function 'dorg-sql-gen--lower-item-plan)))
      (cl-letf (((symbol-function 'dorg-sql-gen--lower-item-plan)
                 (lambda (new-heading db-heading item-id-gen)
                   (cl-remove-if
                    (lambda (stmt)
                      (and (string-match-p
                            "UPDATE heading_items SET sort_key = .*post_blank"
                            stmt)
                           (string-match-p "WHERE id = 42\\'" stmt)))
                    (funcall real-lower new-heading db-heading item-id-gen)))))
        (let ((dorg-mutate--dolt-item-id-generator (lambda () 55)))
          (condition-case err
              (progn
                (dorg-test-mutate--apply-patch-batch
                 src (list (dorg-test-mutate--text-append-patch
                            hid "appended")))
                (ert-fail "expected postimage parity error"))
            (dorg-mutate-error
             (should (eq (dorg-mutate-error-reason err) 'invariant))
             (should (eq (plist-get (dorg-mutate-error-data err) :detail)
                         'postimage-parity))))))
      (should (equal base (dorg-head-commit dolt-dir)))
      (let* ((items (dorg-test-mutate--dolt-text-items dolt-dir 100 hid))
             (ids (mapcar #'dorg-item-id items))
             (keys (mapcar #'dorg-item-sort-key items)))
        (should (equal ids (number-sequence 42 54)))
        (should (equal keys old-keys))))))

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
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1 :title "Tail"
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
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
                               :sort-key (unibyte-string #x20)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1 :title "Tail"
                                  :sort-key (unibyte-string #x80)))
         (holding (make-dorg-heading :id 12 :parent-id 0 :level 1 :level-offset 1
                                     :title "Holding"
                                     :sort-key (unibyte-string #xe0)))
         (a (make-dorg-heading :id 20 :parent-id 12 :level 2 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 21 :parent-id 12 :level 2 :level-offset 1 :title "B"
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
  "The window fallback of a move uses the same effective anchor as the fast path."
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
                               :sort-key (unibyte-string #x20)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1 :title "Tail"
                                  :sort-key (unibyte-string #x80)))
         (holding (make-dorg-heading :id 12 :parent-id 0 :level 1 :level-offset 1
                                     :title "Holding"
                                     :sort-key (unibyte-string #xe0)))
         (a (make-dorg-heading :id 20 :parent-id 12 :level 2 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 21 :parent-id 12 :level 2 :level-offset 1 :title "B"
                               :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 0 root x tail holding a b))
         (source (dorg-mutate--resolve-source
                  '(:dolt-dir "/tmp/dorg-test-repo" :root-id 0)))
         (batch-context (dorg-mutate--item-batch-context))
         (calls 0)
         (orig (symbol-function 'dorg-model-plan-gap)))
    (cl-letf (((symbol-function 'dorg-model-plan-gap)
               (lambda (&rest args)
                 (cl-incf calls)
                 (if (= calls 2)
                     (signal 'dorg-sortkey-exhausted nil)
                   (apply orig args)))))
      (dolist (patch
               (list
                '(:op move :target (:subtree :id 20)
                      :to (:under :root :after (:heading :id 10)))
                '(:op move :target (:subtree :id 21)
                      :to (:under :root :after (:heading :id 10)))))
        (dorg-mutate--ps-apply-patch ps patch source batch-context)))
    ;; The forced exhaustion (call 2) sends the move through the facts path,
    ;; whose empty window allocates through the gap planner again.
    (should (>= calls 2))
    (should (equal (dorg-test-mutate--child-titles
                    (dorg-mutate--ps-headings ps) 0)
                   '("X" "A" "B" "Tail" "Holding")))))

(ert-deftest dorg-test-bejl7-ps-after-cursor-deleted-falls-back-to-declared-anchor ()
  "A deleted after-cursor does not make a later same-anchor insert invalid."
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1 :title "Tail"
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
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
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
  (let* ((root (make-dorg-heading :id 0 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (x (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1 :title "X"
                               :sort-key (unibyte-string #x40)))
         (tail (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1 :title "Tail"
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
  "Delete a sibling and INSERT before its successor: commits atomically.
Seeds A at X'01' and B at X'02'; a batch deletes A and inserts C :before B.
The fixed-width boundary allocator gives C a final key strictly before B,
without depending on the legacy exact byte that A used to occupy."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
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
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-success ()
  "Delete a sibling and MOVE an existing heading before its successor.
Seeds A X'01', B X'02', D X'03'; deletes A and moves D :before B.  The
observable contract is D strictly before B with a valid sibling sequence,
not reuse of A's legacy one-byte key."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key (unibyte-string 2)))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "D"
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
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should (string< (dorg-heading-sort-key (gethash 103 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-insert-parent-move-child ()
  "Combined FK + collision case commits atomically.
Deletes sibling S, inserts a new parent P before existing child X, and moves
X under that new parent — all in one commit.  The test pins the final tree
shape and sibling-order invariants, not the allocator's incidental bytes."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (s (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "S"
                                 :sort-key (unibyte-string 1)))
           (x (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "X"
                                 :sort-key (unibyte-string 2)))
           (q (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "Q"
                                 :sort-key (unibyte-string 3))))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert s 100)
                            (dorg-sql-gen-heading-insert x 100)
                            (dorg-sql-gen-heading-insert q 100)))
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
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 103 tree))))
          ;; X is now a child of the freshly inserted parent.
          (should (eql (dorg-heading-parent-id (gethash 102 tree)) 300))
          (dorg-test-mutate--assert-tree-sibling-order tree 300))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-child-promotion ()
  "Child promotion into the freed parent slot stays FK/cascade safe.
Deletes parent P (no :subtree) whose sole child C is promoted to the root
before Q.  The scheduler reparents C before deleting P, so ON DELETE CASCADE
cannot remove the surviving child; the observable contract is C before Q in
a strictly ordered root sibling sequence."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (p (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "P"
                                 :sort-key (unibyte-string #x40)))
           (q (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "Q"
                                 :sort-key (unibyte-string #x80)))
           (c (make-dorg-heading :id 102 :parent-id 101 :level 2 :level-offset 1 :title "C"
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
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should (string< (dorg-heading-sort-key (gethash 102 tree))
                           (dorg-heading-sort-key (gethash 103 tree))))
          ;; Q untouched.
          (should (equal (dorg-heading-sort-key (gethash 103 tree))
                         (unibyte-string #x80))))))))

(ert-deftest dorg-test-mutate-dolt-delete-promotion-scope-is-recursive-once ()
  "Both delete forms load the deleted heading's complete subtree once.
This is a source-only scope test: the production scope resolver and recursive
subtree helper run against a pinned SQL response seam.  The target 101 has a
promoted child 102, deep descendants 103/104, and an unrelated branch 105/106.
The scope must include the target, its parent, and every promoted descendant,
but never the unrelated branch or a per-child direct-order query."
  (let* ((source (dorg-mutate--resolve-source
                  (list :dolt-dir "/tmp/dorg-delete-promotion/" :root-id 100)))
         (head "hh12c-head")
         (recursive-queries nil)
         (direct-child-queries nil)
         (heading-patch (dorg-test-mutate--delete-patch 101))
         (subtree-patch (dorg-test-mutate--delete-patch 101 t)))
    (cl-letf (((symbol-function 'dorg-sql)
               (lambda (_dolt-dir sql)
                 (cond
                  ((string-match-p "WITH RECURSIVE sub" sql)
                   (push sql recursive-queries)
                   '(((id . 101)) ((id . 102)) ((id . 103)) ((id . 104))))
                  ((string-match-p "SELECT id, parent_id FROM headings" sql)
                   '(((id . 101) (parent_id . 100))))
                  ((string-match-p "HEX(sort_key) AS sort_key_hex" sql)
                   (push sql direct-child-queries)
                   '(((id . 102) (sort_key_hex . "80"))))
                  (t (ert-fail (format "unexpected hh12c scope SQL: %s" sql)))))))
      (let ((heading-scope
             (sort (dorg-mutate--dolt-scope-ids
                    source (list heading-patch) 100 head) #'<))
            (subtree-scope
             (sort (dorg-mutate--dolt-scope-ids
                    source (list subtree-patch) 100 head) #'<)))
        ;; The heading form is the pre-fix RED: it currently sees only 102.
        (should (equal '(100 101 102 103 104) heading-scope))
        (should (equal heading-scope subtree-scope))
        (should-not (member 105 heading-scope))
        (should-not (member 106 heading-scope))
        ;; One recursive request per patch, both at the same captured HEAD;
        ;; the old direct-child order query has no place in either form.
        (should (= 2 (length recursive-queries)))
        (should (cl-every (lambda (sql) (string-match-p head sql))
                          recursive-queries))
        (should-not direct-child-queries)))))

(ert-deftest dorg-test-mutate-delete-promotion-preserves-gapped-levels ()
  "Promotion keeps every descendant's offset without normalizing gaps.
Deleting B/102 from A(1) > B(2) > D(3) > E(5) promotes D to level 2, and E
follows at level 4 through its unchanged offset.  The retained two-level D/E
gap is an Org-value contract, not inferred from parent links.  Only D's row
changes; E is recorded as moved for invalidation but is not rewritten."
  (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x80)))
         (b (make-dorg-heading :id 102 :parent-id 101 :level 2 :level-offset 1 :title "B"
                               :sort-key (unibyte-string #x80)))
         (d (make-dorg-heading :id 103 :parent-id 102 :level 3 :level-offset 1 :title "D"
                               :sort-key (unibyte-string #x80)))
         (e (make-dorg-heading :id 104 :parent-id 103 :level 5 :level-offset 2 :title "E"
                               :sort-key (unibyte-string #x80)))
         (other (make-dorg-heading :id 105 :parent-id 100 :level 1 :level-offset 1 :title "Other"
                                   :sort-key (unibyte-string #xc0)))
         (ps (dorg-test-mutate--ps-with 100 root a b d e other)))
    ;; Key allocation is outside this level contract; retain the production
    ;; delete/promotion planner while making its one placement deterministic.
    (cl-letf (((symbol-function 'dorg-mutate--ps-allocate-keys)
               (lambda (&rest _) (list (unibyte-string #x40)))))
      (dorg-mutate--ps-delete-heading ps
                                       (dorg-test-mutate--delete-patch 102)))
    (should-not (gethash 102 (dorg-mutate--ps-headings ps)))
    (should (= 101 (dorg-heading-parent-id (gethash 103 (dorg-mutate--ps-headings ps)))))
    (should (= 103 (dorg-heading-parent-id (gethash 104 (dorg-mutate--ps-headings ps)))))
    (should (= 2 (dorg-heading-level (gethash 103 (dorg-mutate--ps-headings ps)))))
    (should (= 4 (dorg-heading-level (gethash 104 (dorg-mutate--ps-headings ps)))))
    (should (= 2 (- (dorg-heading-level (gethash 104 (dorg-mutate--ps-headings ps)))
                    (dorg-heading-level (gethash 103 (dorg-mutate--ps-headings ps))))))
    (should (= 1 (dorg-heading-level (gethash 105 (dorg-mutate--ps-headings ps)))))
    (should (member 102 (dorg-mutate--ps-deleted ps)))
    (should (member 103 (dorg-mutate--ps-changed ps)))
    (should-not (member 104 (dorg-mutate--ps-changed ps)))
    (should (gethash 104 (dorg-mutate--ps-moved ps)))))

(ert-deftest dorg-test-mutate-dolt-public-delete-promotion-persists-deep-levels ()
  "Public heading deletion persists the complete promoted subtree after reload.
Parent -> Child -> Grandchild -> Great-grandchild is a contiguous depth-three
survivor chain; an unrelated branch guards bounded scope and conservation."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* Parent\n"
                          "** Child\nChild payload.\n"
                          "*** Grandchild\nGrandchild payload.\n"
                          "**** Great-grandchild\nGreat payload.\n"
                          "* Unrelated\n"
                          "** Unrelated child\nUnrelated payload.\n")
                  100))
           (parent (dorg-test-mutate--id-by-title dolt-dir "Parent"))
           (child (dorg-test-mutate--id-by-title dolt-dir "Child"))
           (grandchild (dorg-test-mutate--id-by-title dolt-dir "Grandchild"))
           (great-grandchild
            (dorg-test-mutate--id-by-title dolt-dir "Great-grandchild"))
           (unrelated (dorg-test-mutate--id-by-title dolt-dir "Unrelated"))
           (unrelated-child
            (dorg-test-mutate--id-by-title dolt-dir "Unrelated child"))
           (survivors (list child grandchild great-grandchild))
           (source (list :dolt-dir dolt-dir :root-id root))
           (before-head (dorg-head-commit dolt-dir))
           (before-tree (dorg-model-load-tree dolt-dir root))
           (before-content
            (mapcar (lambda (id)
                      (let ((heading (gethash id before-tree)))
                        (list id (dorg-heading-title heading)
                              (dorg-heading-items heading))))
                    survivors))
           (before-unrelated
            (let ((heading (gethash unrelated before-tree)))
              (list (dorg-heading-parent-id heading)
                    (dorg-heading-level heading)
                    (dorg-heading-sort-key heading)
                    (dorg-heading-title heading)
                    (dorg-heading-items heading))))
           (result (dorg-api-execute
                    `(on (id ,parent) (delete (heading)))
                    :mode 'apply :source source)))
      ;; This is a transformation, not a reload-only assertion.
      (should (plist-get result :ok))
      (should-not (equal before-head (dorg-head-commit dolt-dir)))
      (should (member parent (plist-get (plist-get result :affected) :deleted)))
      (dolist (id survivors)
        (should (member id (plist-get (plist-get result :affected) :changed))))
      (let ((after-tree (dorg-model-load-tree dolt-dir root)))
        (should-not (gethash parent after-tree))
        ;; Persisted topology and the contiguous uniform decrement.
        (should (= root (dorg-heading-parent-id (gethash child after-tree))))
        (should (= child (dorg-heading-parent-id (gethash grandchild after-tree))))
        (should (= grandchild
                   (dorg-heading-parent-id (gethash great-grandchild after-tree))))
        (should (= 1 (dorg-heading-level (gethash child after-tree))))
        (should (= 2 (dorg-heading-level (gethash grandchild after-tree))))
        (should (= 3 (dorg-heading-level (gethash great-grandchild after-tree))))
        (should (equal before-content
                       (mapcar (lambda (id)
                                 (let ((heading (gethash id after-tree)))
                                   (list id (dorg-heading-title heading)
                                         (dorg-heading-items heading))))
                               survivors)))
        (should (equal (list child unrelated)
                       (mapcar #'dorg-heading-id
                               (dorg-test-mutate--tree-children after-tree root))))
        (let ((after-unrelated (gethash unrelated after-tree)))
          (should (equal before-unrelated
                         (list (dorg-heading-parent-id after-unrelated)
                               (dorg-heading-level after-unrelated)
                               (dorg-heading-sort-key after-unrelated)
                               (dorg-heading-title after-unrelated)
                               (dorg-heading-items after-unrelated)))))
        (should (= unrelated
                   (dorg-heading-parent-id (gethash unrelated-child after-tree))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-rollback ()
  "A delete-freed batch that fails at commit rolls back wholesale.
Seeds A one step before B, B at X'80' (#+FOO a), and D at X'c0' (#+BAZ q).
The batch deletes A, inserts C into A's freed gap, and appends a keyword item
to B; a forced `heading_items.id' generator collides the new item id with D's
surviving BAZ row, so the whole transaction — insert, delete, and item append
— is rolled back.  HEAD is unchanged, A keeps its slot, C is absent, and B's
FOO is still \"a\"."
  (dorg-test-mutate--with-dolt
    (let* ((idg (let ((n 9000)) (lambda () (cl-incf n))))
           (b-key (unibyte-string #x80))
           (a-key (dorg-sortkey-step-before b-key))
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key a-key))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key b-key
                                 :items (list (dorg-keyword-item
                                               :keyword "FOO" :value "a"))))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "D"
                                 :sort-key (unibyte-string #xc0)
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
                         a-key))
          (should-not (gethash 300 tree))
          (should (equal (dorg-test-mutate--dolt-keyword-values tree 102 "FOO")
                         '("a"))))))))

(defun dorg-test-mutate--legacy-be8-key (id)
  "Return the retired be8(id) staging key used by legacy fixtures."
  (apply #'unibyte-string
         (mapcar (lambda (shift) (logand (ash id shift) #xff))
                 '(-56 -48 -40 -32 -24 -16 -8 0))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-insert-takes-a-fresh-key ()
  "W7: an insert into a slot a same-batch delete frees never reuses the freed key.
D (103) is seeded at the retired be8(300) value, A (101) between D and B, and
B (102) at X'80'.  Deleting A and inserting C (300) :before B commits in one
transaction: C lands strictly between D and B on a key that is neither A's
freed key nor any live key, so no statement order can collide on
`uq_heading_sibling_order' and nothing is staged."
  (dorg-test-mutate--with-dolt
    (let* ((d-key (dorg-test-mutate--legacy-be8-key 300))
           (b-key (unibyte-string #x80))
           (a-key (unibyte-string #x40))
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key a-key))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key b-key))
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "D"
                                 :sort-key d-key)))
      ;; Sanity: D really holds the slot the naive temp key would reuse.
      (should (equal d-key (dorg-test-mutate--legacy-be8-key 300)))
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
          ;; C orders between D and B on a fresh key, not on A's freed key.
          (should-not (equal (dorg-heading-sort-key (gethash 300 tree)) a-key))
          (should (string< (dorg-heading-sort-key (gethash 103 tree))
                           (dorg-heading-sort-key (gethash 300 tree))))
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree))))
          ;; D is untouched at its (colliding-looking) seeded slot.
          (should (equal (dorg-heading-sort-key (gethash 103 tree)) d-key)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-takes-a-fresh-key ()
  "W7: a move into a slot a same-batch delete frees never reuses the freed key.
D sits at the retired be8(300) value; moving M :before B lands it between D
and B on a fresh key, so nothing is staged and the batch commits."
  (dorg-test-mutate--with-dolt
    (let* ((m-key (dorg-test-mutate--legacy-be8-key 300))
           (b-key (unibyte-string #x80))
           (a-key (unibyte-string #x40))
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key a-key))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key b-key))
           ;; D holds the id-derived staging slot M would need.
           (d (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "D"
                                 :sort-key m-key))
           ;; M is parked far to the right; moving it :before B lands it on
           ;; A's freed slot, requiring staging.
           (m (make-dorg-heading :id 300 :parent-id 100 :level 1 :level-offset 1 :title "M"
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
        (let ((plan (dorg-test-mutate--plan-patch-batch src patches)))
          (should (plist-get plan :supported))
          (should-not (plist-get plan :unsupported)))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should-not (equal (dorg-heading-sort-key (gethash 300 tree)) a-key))
          (should (string< (dorg-heading-sort-key (gethash 103 tree))
                           (dorg-heading-sort-key (gethash 300 tree))))
          (should (string< (dorg-heading-sort-key (gethash 300 tree))
                           (dorg-heading-sort-key (gethash 102 tree)))))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-source-parent-collision-commits ()
  "A legacy source-parent be8 collision no longer blocks reparenting.
The planner chooses a temp from the shared bounded namespace against the
complete affected source/destination sibling facts."
  (dorg-test-mutate--with-dolt
    (let* ((m-temp (dorg-test-mutate--legacy-be8-key 300))
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (unibyte-string 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key (unibyte-string 2)))
           (p (make-dorg-heading :id 200 :parent-id 100 :level 1 :level-offset 1 :title "P"
                                 :sort-key (unibyte-string 3)))
           (m (make-dorg-heading :id 300 :parent-id 200 :level 2 :level-offset 1 :title "M"
                                 :sort-key (unibyte-string #x10)))
           (x (make-dorg-heading :id 301 :parent-id 200 :level 2 :level-offset 1 :title "X"
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
        (let ((plan (dorg-test-mutate--plan-patch-batch src patches)))
          (should (plist-get plan :supported))
          (should-not (plist-get plan :unsupported)))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :changed)))
          (should (member 101 (plist-get res :deleted))))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
          (should (equal (dorg-heading-sort-key (gethash 301 tree))
                         m-temp)))))))

(ert-deftest dorg-test-mutate-dolt-delete-freed-move-own-temp-key-commits ()
  "A move whose own current key looks like a legacy temp key commits.
Nothing about a key's shape matters any more: a mover simply receives a fresh
key between its new neighbours, and its old key stays vacated until commit.

Layout: under root 100, A id 101 @ X'40', B id 102 @ X'80', and M id 300
already parked at its own forced staging key be8(300).  The batch deletes A
and moves M :before B.  M's own temp-key-shaped current key must not cause a
false collision, and the final model must order M strictly before B."
  (dorg-test-mutate--with-dolt
    (let* ((m-temp (dorg-test-mutate--legacy-be8-key 300))
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (unibyte-string #x40)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key (unibyte-string #x80)))
           ;; M already sits at its OWN forced staging key be8(300); no
           ;; other sibling occupies that slot.
           (m (make-dorg-heading :id 300 :parent-id 100 :level 1 :level-offset 1 :title "M"
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
        ;; HEAD advanced once; A gone, M orders before B.
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 tree))
          (should (eql (dorg-heading-parent-id (gethash 300 tree)) 100))
          (dorg-test-mutate--assert-tree-sibling-order tree 100)
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
                                                &optional as-of headline-only
                                                query-fn levels)
                                (push (list root-id got-ids as-of headline-only)
                                      load-calls)
                                (funcall real-load-headings
                                         dolt-dir root-id got-ids
                                         as-of headline-only query-fn levels))))
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
         (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                               :sort-key (unibyte-string #x40)))
         (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
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
               (lambda (_dolt-dir root-id ids &optional as-of headline-only &rest _)
                 (push (list root-id ids as-of headline-only) load-calls)
                 (dorg-test-mutate--scope-tree root a b))))
      (let ((sim (dorg-mutate--dolt-simulate src patches nil)))
        (should (equal "H-plan" (plist-get sim :head-commit)))
        (should (equal '((100 101 102)) (mapcar #'cadr load-calls)))
        (should (equal '("H-plan") (mapcar #'caddr load-calls)))
        (should (member 100 (cadar load-calls)))
        (should (equal (list src patches 100 "H-plan") (car scope-calls)))))))

(ert-deftest dorg-test-mutate-dolt-scope-hydrates-targets-and-anchors-only ()
  "W4: same-root scope hydrates the root, the targets and the anchors only.
A delete-freed insert under the root loads A (deleted) and B (anchor); it
never hydrates the unrelated sibling P or P's descendant, because placement
reads the parent's order as keys, never as headings."
  (dorg-test-mutate--with-dolt
    (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (unibyte-string #x20)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key (unibyte-string #x40)))
           (p (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "P"
                                 :sort-key (unibyte-string #x60)))
           (far (make-dorg-heading :id 104 :parent-id 103 :level 2 :level-offset 1 :title "Far"
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
        (dolist (id '(100 101 102))
          (should (member id ids)))
        (should-not (member 103 ids))
        (should-not (member 104 ids))))))

(ert-deftest dorg-test-2xv2m-insert-scope-is-local-to-placement-facts ()
  "Pure insert scope contains only the parent and the anchor.
The fixture has four direct root children.  Placement reads the parent's
order as keys, so no sibling is hydrated for it: tail insertion hydrates only
the root; insertion before or after B hydrates the root and B.  The old
affected-parent fan returned every child and therefore fails each exact set."
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
      (ignore a c d)
      (should (equal (list root)
                     (funcall scope
                              (dorg-test-mutate--tree-insert-patch
                               :root '(:id 300 :title "Tail")))))
      (should (equal (sort (list root b) #'<)
                     (funcall scope
                              (dorg-test-mutate--tree-insert-patch
                               :root '(:id 301 :title "Before B")
                               :before (dorg-test-mutate--heading-address b)))))
      (should (equal (sort (list root b) #'<)
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
                 (lambda (dir rid ids &optional as-of headline-only query-fn levels)
                   (push (copy-sequence ids) loaded)
                   (funcall real-load dir rid ids as-of headline-only
                            query-fn levels))))
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
              (,root ,a ,a-kid)
              (,b ,c ,c-kid))
             ("move-heading"
              (,(dorg-test-mutate--move-before-patch
                 (dorg-test-mutate--external-address
                  :subtree "EXT" "beta")
                 :root (dorg-test-mutate--heading-address a)))
              (,root ,a ,b)
              (,c ,a-kid ,c-kid))
             ("move-subtree"
              (,(list :op 'move
                      :target (dorg-test-mutate--external-address
                               :subtree "EXT" "alpha")
                      :to (list :under :root
                                :after (dorg-test-mutate--heading-address b))))
              (,root ,a ,a-kid ,b)
              (,c ,c-kid))))
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
      (dolist (id (list src-root movable kid))
        (should (member id src-ids)))
      ;; No placement happens in the source: its sibling fan is not hydrated.
      (should-not (member src-sibling src-ids))
      (dolist (id (list dst-root dst-parent))
        (should (member id dst-ids)))
      ;; Destination order is read as keys; no destination child is hydrated.
      (should-not (member dst-child dst-ids))
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
                 'replace :root "OPTIONS" 1 :value "new")
               ,(dorg-test-mutate--keyword-occurrence-patch
                 'replace (dorg-test-mutate--heading-address a)
                 "FOO" 1 :value "new"))
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
         (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
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
               (lambda (_dolt-dir root-id ids &optional as-of headline-only &rest _)
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

(defun dorg-test-level-offset--cross-root-fixture (dolt-dir)
  "Seed a cross-root move with an inner gap; return the ids as a plist.
Source root 100: Movable (offset 1) > Kid (offset 3), then SrcSib.
Destination root 500: DstParent at level 1."
  (dorg-test-mutate--seed dolt-dir "* Movable\n**** Kid\n* SrcSib\n" 100)
  (dorg-test-mutate--seed dolt-dir "* DstParent\n" 500)
  (list :movable (dorg-test-mutate--id-by-title dolt-dir "Movable")
        :kid (dorg-test-mutate--id-by-title dolt-dir "Kid")
        :dst-parent (dorg-test-mutate--id-by-title dolt-dir "DstParent")))

(defun dorg-test-level-offset--cross-root-apply (dolt-dir ids)
  "Move the fixture's Movable under DstParent in root 500; return the result."
  (dorg-test-mutate--apply-patch-batch
   (list :dolt-dir dolt-dir :root-id 100)
   (list (dorg-test-mutate--cross-root-move-patch
          (plist-get ids :movable)
          (dorg-test-mutate--heading-address (plist-get ids :dst-parent))
          dolt-dir 500))))

(ert-deftest dorg-test-level-offset-dolt-cross-root-refuses-corrupt-descendant ()
  "A cross-root move over a zero descendant offset is refused before writing.
The destination candidate derives the moved closure through the shared
fail-closed rule, so Kid's invalid offset cannot yield a numeric level."
  (dorg-test-mutate--with-dolt
    (let ((ids (dorg-test-level-offset--cross-root-fixture dolt-dir)))
      (dorg-sql dolt-dir (format "UPDATE headings SET level_offset = 0 WHERE id = %d"
                                 (plist-get ids :kid)))
      (dorg-commit dolt-dir "corrupt Kid")
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error (dorg-test-level-offset--cross-root-apply dolt-dir ids)
                                :type 'dorg-mutate-error)))
        (should (eq 'invalid-level-offset
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (eql 100 (alist-get 'root_id
                                    (car (dorg-sql dolt-dir (format "SELECT root_id FROM headings WHERE id = %d"
                                                                    (plist-get ids :kid)))))))))))

(ert-deftest dorg-test-level-offset-dolt-cross-root-move-root-id ()
  "GUARD: a cross-root move re-homes every moved row into the destination root."
  (dorg-test-mutate--with-dolt
    (let ((ids (dorg-test-level-offset--cross-root-fixture dolt-dir)))
      (should (plist-get (dorg-test-level-offset--cross-root-apply dolt-dir ids) :ok))
      (dolist (id (list (plist-get ids :movable) (plist-get ids :kid)))
        (should (eql 500 (alist-get 'root_id
                                    (car (dorg-sql dolt-dir (format "SELECT root_id FROM headings WHERE id = %d" id))))))))))

(ert-deftest dorg-test-level-offset-dolt-cross-root-move-no-descendant-offset ()
  "A cross-root move resets only the moved root's offset; descendants keep theirs."
  (dorg-test-mutate--with-dolt
    (let ((ids (dorg-test-level-offset--cross-root-fixture dolt-dir)))
      (should (plist-get (dorg-test-level-offset--cross-root-apply dolt-dir ids) :ok))
      (let ((offset (lambda (id)
                      (alist-get 'level_offset
                                 (car (dorg-sql dolt-dir (format "SELECT level_offset FROM headings WHERE id = %d" id))))))
            (tree (dorg-model-load-tree dolt-dir 500)))
        (should (eql 1 (funcall offset (plist-get ids :movable))))
        (should (eql 3 (funcall offset (plist-get ids :kid))))
        (should (eql 2 (dorg-heading-level (gethash (plist-get ids :movable) tree))))
        (should (eql 5 (dorg-heading-level (gethash (plist-get ids :kid) tree))))))))

(ert-deftest dorg-test-level-offset-dolt-cross-root-candidate-derives-descendant-levels ()
  "The cross-root candidate hands consumers the descendants' derived levels.
Extension images read these in-memory levels, so they must follow the new
ancestry from the stored offsets, not only after a reload."
  (dorg-test-mutate--with-dolt
    (let* ((ids (dorg-test-level-offset--cross-root-fixture dolt-dir))
           (sim (dorg-mutate--dolt-cross-root-simulate
                 (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id 100))
                 (car (dorg-mutate--validate-patches
                       (list (dorg-test-mutate--cross-root-move-patch
                              (plist-get ids :movable)
                              (dorg-test-mutate--heading-address (plist-get ids :dst-parent))
                              dolt-dir 500))))
                 nil))
           (level (lambda (id)
                    (dorg-heading-level
                     (cadr (cl-find-if (lambda (op) (eql id (dorg-heading-id (cadr op))))
                                       (plist-get sim :db-ops)))))))
      (should (eql 2 (funcall level (plist-get ids :movable))))
      (should (eql 5 (funcall level (plist-get ids :kid)))))))

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
      ;; correct parent linkage, and levels derived from the new ancestry.
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

(defun dorg-test-32mp9--assert-owned-property-refusal (form key source before current)
  "Assert FORM cannot change owned KEY through SOURCE in either public mode.

CURRENT reads the source preimage.  The shared public normalizer must reject
before either planning or apply, preserving the exact preimage in both modes."
  (dolist (mode '(dry-run apply))
    (let ((data (condition-case err
                    (progn
                      (dorg-api-execute form :mode mode :source source)
                      (ert-fail "expected owned-property refusal"))
                  (dorg-api-error (dorg-api-error-data err))
                  (dorg-mutate-error (dorg-mutate-error-data err)))))
      (should (eq 'invalid-target (plist-get data :reason)))
      (should (equal key (plist-get data :key)))
      (should (equal before (funcall current))))))

(ert-deftest dorg-test-32mp9-public-property-prefix-preserves-preimages ()
  "Case variants of owned properties cannot forge or erase DORG_ID.

The old public guard compared only the literal uppercase spelling.  These
rows exercise the actual property door in dry-run and apply on buffer, file,
and Dolt sources; the rich Dolt fixture additionally proves planning and an
ordinary user property survive the refusal."
  (let ((content (concat "* Host\nSCHEDULED: <2026-07-01 Wed>\n"
                         ":PROPERTIES:\n:DORG_ID: 7\n:CATEGORY: work\n:END:\nbody\n")))
    (dorg-test-mutate--with-content buf content
      (dolist (form '((on (id 7) (set (property "dorg_id") "9"))
                      (on (id 7) (delete (property "dorg_id")))))
        (dorg-test-32mp9--assert-owned-property-refusal
         form "dorg_id" (list :buffer buf :identity 'dorg) content
         (lambda () (buffer-string)))))
    (let ((file (make-temp-file "dorg-32mp9-" nil ".org" content)))
      (unwind-protect
          (dolist (form '((on (id 7) (set (property "Dorg_Id") "9"))
                          (on (id 7) (delete (property "Dorg_Id")))))
            (dorg-test-32mp9--assert-owned-property-refusal
             form "Dorg_Id" (list :file file :identity 'dorg) content
             (lambda () (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (when-let ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-file file))))
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (let* ((source (list :dolt-dir dolt-dir :root-id 1000))
           (head (dorg-head-commit dolt-dir)))
      (dolist (form '((on (id 3001) (set (property "dorg_id") "9"))
                      (on (id 3001) (delete (property "dorg_id")))))
        (dorg-test-32mp9--assert-owned-property-refusal
         form "dorg_id" source head (lambda () (dorg-head-commit dolt-dir))))
      (let ((heading (gethash 3001 (dorg-model-load-headings dolt-dir 1000 nil))))
        (should (= 3001 (dorg-heading-id heading)))
        (should (dorg-test-api--heading-scheduled-p heading))
        (should (equal "work" (dorg-test-api--heading-category heading)))))))

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
                 (insert (drawer-line "AUDIT") (before-first)
                         "- A"))
             (on (id 1001)
                 (insert (drawer-line "AUDIT") (before-first)
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
                 (insert (drawer-line "LOGBOOK") (before-first)
                         "- x"))
             (on (id 1001)
                 (insert (drawer-line "LOGBOOK") (before-first)
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
                 (insert (drawer-line "AUDIT") (after-last)
                         "- A"))
             (on (id 1001)
                 (insert (drawer-line "AUDIT") (after-last)
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
                (insert (drawer-line "LOGBOOK") (before-first)
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
                (insert (drawer-line "AUDIT") (after-last)
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
                   (insert (drawer-line "AUDIT")
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
            (insert (drawer-line "AUDIT")
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
                      (insert (drawer-line "LOGBOOK") (before-first) "- created")))
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
                           (insert (drawer-line "LOGBOOK") (before-first) "- different"))
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
                       (insert (drawer-line "AUDIT") (before-first) "- must roll back"))
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
  "The shared C3 builder admits concrete planning and refuses its root cell."
  (let ((planning (dorg-test-mutate--planning-patch
                   1001 :scheduled "<2026-08-19 Wed>"))
        (root-planning '(:op update :target (:planning :host :root)
                         :scheduled "<2026-08-19 Wed>"))
        (root-property '(:op put :target (:property :host :root :name "K")
                         :value "V")))
    (should-not (dorg-mutate--c3-refusal planning))
    (should (equal '(:unsupported t
                     :reason no-source-preserving-editor
                     :message "no source-preserving editor for (update :planning) at the level-0 root"
                     :operation update :target-kind :planning :variant (:host root))
                   (dorg-mutate--c3-refusal root-planning)))
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

(ert-deftest dorg-test-c8ynt-c3-heading-line-cells-are-supported ()
  "C3 admits every nonempty intrinsic heading-line field subset at a heading."
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
    (should (equal '(supported . S1)
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
      ;; A mixed S1 payload shares the heading-line editor: both changed
      ;; fields land, while all omitted heading facets retain their bytes.
      (let* ((before (buffer-string))
             (expected (replace-regexp-in-string
                        (regexp-quote "* TODO [#A] Task  :alpha:beta:\n")
                        "* TODO [#A] COMMENT Renamed  :alpha:beta:\n"
                        before t t)))
        (dorg-mutate--apply-patches
         buf '((:op update :target (:heading :id 9101)
                      :fields (:commented t :title "Renamed"))))
        (should-not (equal before (buffer-string)))
        (should (equal expected (buffer-string)))
        ;; Keep the historical COMMENT-only public-path control below on its
        ;; original source image.
        (erase-buffer)
        (insert before))
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

(ert-deftest dorg-test-ltxz7-plain-constructor-is-one-c3-data-cell ()
  "C3 data, not a dispatch bypass, owns the bounded plain constructor."
  (should
   (equal
    '((((update :heading) heading (:commented))
       :status supported :slice S1
       :variant (:host heading :fields (:commented)))
      (((insert :tree) heading (:constructor))
       :status supported :slice S3
       :variant (:host heading :constructor :plain-child)))
    dorg-mutate--c3-field-cells))
  (dolist (value '(t nil omitted))
    (let ((patch (dorg-test-ltxz7--insert-patch
                  nil (append (list :title "Resolution")
                              (unless (eq value 'omitted)
                                (list :commented value))))))
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
           (root (dorg-test-ltxz7--insert-patch
                  '(:tree :under :root)))
           (anchored (dorg-test-ltxz7--insert-patch
                      '(:tree :under (:heading :id 9201)
                              :before (:heading :id 9202))))
           (broader (dorg-test-ltxz7--insert-patch
                     nil '(:title "Raw" :commented t :post-blank 1)))
           (before (buffer-string)))
      (dolist (case `((root . ,root)
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
                               (property "BEAD_ID" "resolution-1")
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
                (format "** TODO [#A] COMMENT Resolution  :workflow:\n:PROPERTIES:\n:DORG_ID: %d\n:BEAD_ID: resolution-1\n:END:"
                        id))
               (buffer-string)))
      (should-not (equal before (buffer-string)))
      ;; Birth-hidden is literal: the first and only inserted heading line has
      ;; COMMENT; no visible preimage was ever committed to the buffer.
      (should-not (string-match-p "^\\*\\* TODO \\[#A\\] Resolution"
                                  (buffer-string))))))

;;; ─── Recursive constructor item spacing ──────────────────────────────

(defun dorg-test-7dyd0--constructor-patch ()
  "Return one public recursive constructor patch with declared item spacing."
  (car
   (dorg-api-normalize
    '(insert
      (heading
       (title "Parent")
       (items (text "Para one." :post-blank 1)
              (text "Para two."))
       (children
        (heading
         (title "Child")
         (items (text "Child one." :post-blank 1)
                (text "Child two.")))))
      (under (id 0))))))

(defun dorg-test-7dyd0--heading-by-title (ps title)
  "Return the heading named TITLE from constructor postimage PS."
  (let (found)
    (maphash (lambda (_id heading)
               (when (equal title (dorg-heading-title heading))
                 (setq found heading)))
             (dorg-mutate--ps-headings ps))
    (should found)
    found))

(defun dorg-test-7dyd0--text-items (heading)
  "Return HEADING's text items in document order."
  (cl-remove-if-not (lambda (item) (cl-typep item 'dorg-text-item))
                    (dorg-heading-items heading)))

(ert-deftest dorg-test-7dyd0-ps-constructor-preserves-item-post-blank-recursively ()
  "Public constructor spacing survives PS construction, rendering, and reparse.

This is the no-Dolt real PS-apply witness.  Its child covers recursive
construction; the source-preserving constructor has a separate pure-model
witness because C3 deliberately refuses this rich payload before persistence."
  (let* ((root (make-dorg-heading :id 0 :parent-id nil :level 0 :level-offset 0
                                  :sort-key (unibyte-string 0)))
         (ps (dorg-test-mutate--ps-with 0 root))
         (patch (dorg-test-7dyd0--constructor-patch)))
    (dorg-mutate--ps-insert-heading ps patch)
    (dolist (title '("Parent" "Child"))
      (let* ((heading (dorg-test-7dyd0--heading-by-title ps title))
             (items (dorg-test-7dyd0--text-items heading))
             (prefix (if (equal title "Parent") "Para" "Child")))
        (should (equal '(1 0) (mapcar #'dorg-item-post-blank items)))
        (let ((rendered (dorg-model-render-heading-section heading)))
          (should (string-match-p
                   (regexp-quote
                    (format "%s one.\n\n%s two.\n" prefix prefix))
                   rendered))
          (with-temp-buffer
            (insert rendered)
            (org-mode)
            (let* ((parsed (dorg-parse-buffer (current-buffer)))
                   (round-trip
                    (cl-find title (dorg-parse-result-headings parsed)
                             :key #'dorg-heading-title :test #'equal))
                   (round-items (dorg-test-7dyd0--text-items round-trip)))
              (should (= 2 (length round-items)))
              ;; The parser retains the final item's terminal newline; the
              ;; first item's blank separator is structural, not text payload.
              (should (equal (list (format "%s one." prefix)
                                   (format "%s two.\n" prefix))
                             (mapcar #'dorg-text-item-text round-items))))))))))

(ert-deftest dorg-test-7dyd0-source-preserving-constructor-preserves-item-post-blank ()
  "The restricted source-preserving constructor keeps normalized item spacing.

This is deliberately a pure construction witness: the public C3 source path
admits only its commented/properties cell, so it refuses the rich two-text
payload above before persistence.  The existing C3 refusal matrix remains the
write-path admission witness."
  (let* ((heading (dorg-mutate--source-preserving-constructor-heading
                   (dorg-test-7dyd0--constructor-patch) 2 7001))
         (items (dorg-test-7dyd0--text-items heading)))
    (should (equal '(1 0) (mapcar #'dorg-item-post-blank items)))
    ;; Normalized forms with no explicit tail stay at the established zero.
    (should (= 0 (dorg-item-post-blank (nth 1 items))))))

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

(ert-deftest dorg-test-jgc67-plain-constructor-admits-visible-child-with-body ()
  "A property-bound parent admits one visible public child at subtree end."
  (let ((fixture
         (concat "* Inbox\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n"
                 "Existing body.\n\n"
                 "* Sibling\n:PROPERTIES:\n:CAPTURE_ID: sibling\n:END:\n")))
    (dorg-test-mutate--with-content buf fixture
      (let* ((source (list :buffer buf :identity '(:property "CAPTURE_ID")))
             (response
              (dorg-api-execute
               '(insert (heading (title "Captured note")
                                 (property "CAPTURE_ID" "capture-1")
                                 (items (text "First paragraph." :post-blank 1)
                                        (text "Second paragraph.")))
                        (under (key "inbox")))
               :mode 'apply :source source)))
        (should (plist-get response :ok))
        (should-not (plist-get (plist-get response :affected) :created))
        (should
         (equal
          (concat "* Inbox\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n"
                  "Existing body.\n\n"
                  "** Captured note\n"
                  ":PROPERTIES:\n:CAPTURE_ID: capture-1\n:END:\n"
                  "First paragraph.\n\nSecond paragraph.\n"
                  "* Sibling\n:PROPERTIES:\n:CAPTURE_ID: sibling\n:END:\n")
          (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-keeps-descendants-blanks-and-sibling ()
  "The literal insertion boundary is after all old subtree bytes, before S."
  (let* ((prefix (concat "* Parent\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n"
                         "** Old child\nold\n\n\n"))
         (suffix "* Next\n")
         (new (concat "** TODO Captured note\n:PROPERTIES:\n:CAPTURE_ID: capture-1\n:END:\n"
                      "First paragraph.\n\nSecond paragraph.\n")))
    (dorg-test-mutate--with-content buf (concat prefix suffix)
      (let ((response
             (dorg-api-execute
              '(insert (heading (title "Captured note") (todo "TODO")
                                (property "CAPTURE_ID" "capture-1")
                                (items (text "First paragraph." :post-blank 1)
                                       (text "Second paragraph.")))
                       (under (key "inbox")))
              :mode 'apply
              :source (list :buffer buf :identity '(:property "CAPTURE_ID")))))
        (should (plist-get response :ok))
        (should (equal (concat prefix new suffix) (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-handles-both-eof-newline-cases ()
  "An existing EOF LF is retained; a missing LF becomes only the new separator."
  (let ((new (concat "** TODO Captured note\n:PROPERTIES:\n:CAPTURE_ID: capture-1\n:END:\n"
                     "First paragraph.\n\nSecond paragraph.\n")))
    (dolist (case '(("body\n" . "") ("body" . "\n")))
      (let ((prefix (concat "* Parent\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n"
                            (car case))))
        (dorg-test-mutate--with-content buf prefix
          (let ((response
                 (dorg-api-execute
                  '(insert (heading (title "Captured note") (todo "TODO")
                                    (property "CAPTURE_ID" "capture-1")
                                    (items (text "First paragraph." :post-blank 1)
                                           (text "Second paragraph.")))
                           (under (key "inbox")))
                  :mode 'apply
                  :source (list :buffer buf :identity '(:property "CAPTURE_ID")))))
            (should (plist-get response :ok))
            (should (equal (concat prefix (cdr case) new) (buffer-string)))))))))

(ert-deftest dorg-test-jgc67-plain-constructor-preserves-crlf-bytes ()
  "A file source receives CRLF constructor bytes without changing raw P/S."
  (let* ((prefix (concat "* Parent\r\n:PROPERTIES:\r\n:CAPTURE_ID: inbox\r\n:END:\r\n"))
         (suffix "* Next\r\n")
         (new (concat "** TODO Captured note\r\n:PROPERTIES:\r\n:CAPTURE_ID: capture-1\r\n:END:\r\n"
                      "First paragraph.\r\n\r\nSecond paragraph.\r\n"))
         (file (make-temp-file "dorg-jgc67-crlf" nil ".org")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region (concat prefix suffix) nil file nil 'silent))
          (let ((response
                 (dorg-api-execute
                  '(insert (heading (title "Captured note") (todo "TODO")
                                    (property "CAPTURE_ID" "capture-1")
                                    (items (text "First paragraph." :post-blank 1)
                                           (text "Second paragraph.")))
                           (under (key "inbox")))
                  :mode 'apply :source (list :file file :identity '(:property "CAPTURE_ID")))))
            (should (plist-get response :ok))
            (should
             (equal (concat prefix new suffix)
                    (with-temp-buffer
                      (insert-file-contents-literally file)
                      (buffer-string))))))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest dorg-test-jgc67-plain-constructor-preserves-all-item-order ()
  "Planning, drawer, keyword, and text stay in the declared constructor order."
  (let* ((prefix "* Parent\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n")
         (new (concat "** TODO Capture α\n"
                      "SCHEDULED: <2026-10-20 Tue>\n"
                      ":PROPERTIES:\n:CAPTURE_ID: capture-1\n:END:\n"
                      "#+CAPTURE_KIND: transcript\n"
                      "First paragraph.\n\nSecond paragraph.\n")))
    (dorg-test-mutate--with-content buf prefix
      (let ((response
             (dorg-api-execute
              '(insert (heading (title "Capture α") (todo "TODO")
                                (items (planning :scheduled "<2026-10-20 Tue>")
                                       (drawer "PROPERTIES" ("CAPTURE_ID" "capture-1"))
                                       (keyword "CAPTURE_KIND" "transcript")
                                       (text "First paragraph." :post-blank 1)
                                       (text "Second paragraph.")))
                       (under (key "inbox")))
              :mode 'apply
              :source (list :buffer buf :identity '(:property "CAPTURE_ID")))))
        (should (plist-get response :ok))
        (should (equal (concat prefix new) (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-refuses-planning-after-drawer ()
  "A constructor never silently reorders a planning line around its drawer."
  (let ((fixture "* Parent\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n"))
    (dorg-test-mutate--with-content buf fixture
      (let ((before (buffer-string))
            (err (should-error
                  (dorg-api-execute
                   '(insert (heading (title "Capture")
                                     (items (drawer "PROPERTIES" ("CAPTURE_ID" "capture-1"))
                                            (planning :scheduled "<2026-10-20 Tue>")))
                            (under (key "inbox")))
                   :mode 'apply
                   :source (list :buffer buf :identity '(:property "CAPTURE_ID")))
                  :type 'dorg-mutate-error)))
        ;; This is the heading/section constructor door, not a body/text
        ;; payload splice; keep its established constructor-specific detail.
        (should (eq 'constructor-section-unrepresentable
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-jgc67-property-identity-child-keeps-body-lookalike ()
  "Only the local drawer carries property identity; body text remains ordinary."
  (let ((prefix "* Parent\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\n")
        (new (concat "** Capture\n:PROPERTIES:\n:CAPTURE_ID: capture-1\n:END:\n"
                     "Body says :PROPERTIES: :CAPTURE_ID: decoy.\n")))
    (dorg-test-mutate--with-content buf prefix
      (let ((response
             (dorg-api-execute
              '(insert (heading (title "Capture")
                                (property "CAPTURE_ID" "capture-1")
                                (items (text "Body says :PROPERTIES: :CAPTURE_ID: decoy.")))
                       (under (key "inbox")))
              :mode 'apply
              :source (list :buffer buf :identity '(:property "CAPTURE_ID")))))
        (should (plist-get response :ok))
        (should-not (plist-get (plist-get response :affected) :created))
        (should (equal (concat prefix new) (buffer-string)))
        (should-not (string-match-p ":DORG_ID:" (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-captures-odd-level-grammar ()
  "A source-local odd-level grammar chooses Org's valid child star count."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (setq-local org-odd-levels-only t)
    (let ((response
           (dorg-api-execute
            '(insert (heading (title "Odd child")) (under (id 9201)))
            :mode 'apply :source buf)))
      (should (plist-get response :ok))
      (should (string-match-p "^\\*\\*\\* Odd child$" (buffer-string))))))

(ert-deftest dorg-test-jgc67-plain-constructor-refuses-injected-heading-in-both-grammars ()
  "A body heading is never hidden by either source headline grammar.
The old odd-level bypass accepted this exact form because it skipped both the
source org-element check and item validation."
  (dolist (odd '(nil t))
    (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
      (setq-local org-odd-levels-only odd)
      (let ((before (buffer-string))
            (err (should-error
                  (dorg-api-execute
                   '(insert (heading (title "Captured")
                                     (items (text "First.\n*** Injected\n")))
                            (under (id 9201)))
                   :mode 'apply :source buf)
                  :type 'dorg-mutate-error)))
        ;; This is the heading/section constructor door, not a body/text
        ;; payload splice; keep its established constructor-specific detail.
        (should (eq 'constructor-section-unrepresentable
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-mutate-refusal-detail-is-specific-to-its-door ()
  "Constructor and payload guards retain their distinct typed details.

The public constructor and body controls above exercise each physical route;
this compact table pins the two shared rejection owners so an unrelated
diagnostic rename cannot make one door silently inherit the other's class."
  (dolist (case
           `((constructor-section-unrepresentable
              ,(lambda () (dorg-mutate--source-preserving-constructor-refuse
                           "* injected")))
             (structural-payload-boundary
              ,(lambda () (dorg-mutate--raise-structural-payload
                           'replace "* injected")))))
    (let ((err (should-error (funcall (cadr case)) :type 'dorg-mutate-error)))
      (should (eq (car case) (plist-get (dorg-mutate-error-data err) :detail))))))

(ert-deftest dorg-test-jgc67-plain-constructor-reuses-heading-line-refusals ()
  "Constructor titles use the shared 1qdtb grammar guard before any edit."
  (dolist (title '("TODO Capture" "COMMENT Capture" "[#A] Capture"
                   "Capture :tag:" "Captured\nInjected"))
    (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
      (let ((before (buffer-string))
            (err (should-error
                  (dorg-api-execute
                   `(insert (heading (title ,title)) (under (id 9201)))
                   :mode 'apply :source buf)
                  :type 'dorg-mutate-error)))
        (should (eq 'heading-line-unrepresentable
                    (plist-get (dorg-mutate-error-data err) :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-refuses-unrepresentable-items ()
  "Section reparse rejects item bytes that would change Org structure."
  (dolist (item '((drawer "LOGBOOK" :lines ("alpha" ":END:" "omega"))
                  (keyword "CAPTURE_KIND" "one\ntwo")
                  (keyword "DORG_CLOCK_START" "2026-10-14T08:00:00Z")))
    (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
      (let ((before (buffer-string))
            (err (should-error
                  (dorg-api-execute
                   `(insert (heading (title "Captured") (items ,item))
                            (under (id 9201)))
                   :mode 'apply :source buf))))
        (let ((detail (plist-get (if (eq (car-safe err) 'dorg-api-error)
                                     (dorg-api-error-data err)
                                   (dorg-mutate-error-data err))
                                  :detail)))
          ;; Constructor item rendering has the constructor-door diagnostic;
          ;; this does not broaden the text/body payload detail contract.
          (should (or (null detail)
                      (memq detail '(constructor-section-unrepresentable
                                     protected-keyword)))))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-jgc67-plain-constructor-uses-valid-level-from-even-parent ()
  "Odd-level grammar maps an even physical parent to Org's valid child level."
  (dorg-test-mutate--with-content
      buf (concat "* Context\n** Parent\n:PROPERTIES:\n:DORG_ID: 9201\n:END:\n")
    (setq-local org-odd-levels-only t)
    (let ((response
           (dorg-api-execute
            '(insert (heading (title "Even parent child")) (under (id 9201)))
            :mode 'apply :source buf)))
      (should (plist-get response :ok))
      (should (string-match-p "^\\*\\*\\* Even parent child$" (buffer-string))))))

(ert-deftest dorg-test-ltxz7-plain-constructor-batch-failure-restores-preimage ()
  "A later phase-one miss leaves buffer and file bytes untouched."
  (dorg-test-mutate--with-content buf dorg-test-ltxz7--fixture
    (let ((before (buffer-string))
          (file-before (with-temp-buffer
                         (insert-file-contents-literally dorg-test-mutate--tmp)
                         (buffer-string)))
          (mtime (file-attribute-modification-time
                  (file-attributes dorg-test-mutate--tmp))))
      (should (eq 'invalid-target
                  (dorg-test-mutate--reason
                   (dorg-api-execute
                    '(batch
                      (insert (heading (title "Resolution") (commented t))
                              (under (id 9201)))
                      (on (id 999999) (set (property "K") "V")))
                    :mode 'apply :source buf))))
      (should (equal before (buffer-string)))
      (should (equal file-before
                     (with-temp-buffer
                       (insert-file-contents-literally dorg-test-mutate--tmp)
                       (buffer-string))))
      (should (equal mtime (file-attribute-modification-time
                            (file-attributes dorg-test-mutate--tmp))))
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

;;; ─── Sibling placement: fixed-width keys and bounded windows ───────────────
;;
;; Every placement site (insert, move, child promotion, cross-root move) goes
;; through `dorg-mutate--ps-allocate-keys': a fast path from the two loaded
;; neighbours, and when that gap has no room, a window rebalance over the
;; parent's key facts (keys only, never hydrated content) lowered as one
;; key-only `:rekey' statement.  Fresh keys never equal a live or vacated key,
;; so no statement order can collide and nothing is staged.  These tests pin
;; that contract on aged fixtures whose keys leave no room at the fixed width.

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

(defun dorg-test-mutate--note-prepend-patch (parent-id before-id id)
  "Return a typed heading prepend patch under PARENT-ID before BEFORE-ID."
  (dorg-test-mutate--tree-insert-patch
   (dorg-test-mutate--heading-address parent-id)
   (list :id id :title (format "Note %d" id))
   :before (dorg-test-mutate--heading-address before-id)))

(defun dorg-test-mutate--raw-prepend-keys (count)
  "Return COUNT keys shaped like the retired before-first bisection chain.
Level 0 halves a single byte from X'80' down to X'01'; every deeper level
prefixes one more zero byte and halves from X'7f'.  The list is newest-first
document order, matching repeated prepends, strictly increasing, and its
longest key has (COUNT-8)/7 + 2 bytes."
  (let ((bytes '(#x80 #x40 #x20 #x10 #x08 #x04 #x02 #x01))
        (deeper '(#x7f #x3f #x1f #x0f #x07 #x03 #x01))
        keys)
    (dotimes (n count)
      (push (if (< n 8)
                (unibyte-string (nth n bytes))
              (let* ((level (1+ (/ (- n 8) 7)))
                     (byte (nth (% (- n 8) 7) deeper)))
                (concat (make-string level 0) (unibyte-string byte))))
            keys))
    keys))

(defun dorg-test-mutate--heading-insert-clears-sort-key-guard (heading)
  "Return non-nil when HEADING lowers through the heading sort-key guard."
  (dorg-sql-gen-heading-insert heading 1)
  t)

(defun dorg-test-mutate--heading-property (heading key)
  "Return HEADING's local property value for KEY, or nil."
  (cl-loop for item in (and heading (dorg-heading-items heading))
           when (and (cl-typep item 'dorg-drawer-item)
                     (dorg-drawer-item-properties item)
                     (assoc key (dorg-drawer-item-properties item)))
           return (cdr (assoc key (dorg-drawer-item-properties item)))))

(defun dorg-test-mutate--heading-text (heading)
  "Return HEADING's text-item body, joined in item order."
  (mapconcat
   #'identity
   (cl-loop for item in (and heading (dorg-heading-items heading))
            when (cl-typep item 'dorg-text-item)
            collect (dorg-text-item-text item))
   "\n"))

(defun dorg-test-mutate--make-prepend-ps (&optional raw-count)
  "Return a PS with a parent and RAW-COUNT prepended note children.
RAW-COUNT, when non-nil, seeds children with the raw prepend chain."
  (let* ((parent-id 10)
         (root (make-dorg-heading :id 1 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (parent (make-dorg-heading :id parent-id :parent-id 1 :level 1 :level-offset 1
                                    :title "Notes"
                                    :sort-key (unibyte-string #x80)))
         (children
          (cl-loop for key in (dorg-test-mutate--raw-prepend-keys
                               (or raw-count 1))
                   for id from 100
                   collect (make-dorg-heading
                            :id id :parent-id parent-id :level 2 :level-offset 1
                            :title (format "Note %d" id)
                            :sort-key key))))
    (apply #'dorg-test-mutate--ps-with 1 root parent children)))

(defun dorg-test-mutate--dolt-simulate-counting-heading-fan-reads
    (patches &optional aged)
  "Run PATCHES through the Dolt simulator and count keys-only ordering reads.
FAN-PARENTS lists whole-group reads (no LIMIT); ORDER-QUERIES and ORDER-ROWS
count every ordering query and the rows it returned.  When AGED is non-nil,
parent 101's children use storage-cliff keys so a before-first insert has no
room and must rekey a local window.  The fixture also carries an unrelated
loaded title-update child under parent 201 so eager scans are visible."
  (let* ((source (dorg-mutate--source-make
                  :kind 'dolt :dolt-dir "/not-a-real-store/" :root-id 100))
         (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (parent (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1
                                    :title "Parent"
                                    :sort-key (unibyte-string #x40)))
         (a-key (if aged
                    (dorg-test-mutate--aged-key 1)
                  (unibyte-string #x80)))
         (a (make-dorg-heading :id 102 :parent-id 101 :level 2 :level-offset 1
                               :title "A" :sort-key a-key))
         (b (make-dorg-heading :id 103 :parent-id 101 :level 2 :level-offset 1
                               :title "B"
                               :sort-key (if aged
                                             (dorg-test-mutate--aged-key 2)
                                           (unibyte-string #xc0))))
         (c (make-dorg-heading :id 104 :parent-id 101 :level 2 :level-offset 1
                               :title "C"
                               :sort-key (if aged
                                             (dorg-test-mutate--aged-key 3)
                                           (unibyte-string #xe0))))
         (other (make-dorg-heading :id 202 :parent-id 201 :level 2 :level-offset 1
                                   :title "Other"
                                   :sort-key (unibyte-string #x60)))
         (table (make-hash-table :test 'eql))
         fan-parents
         (order-queries 0)
         (order-rows 0)
         sql-log)
    (dolist (heading (list root parent a b c other))
      (puthash (dorg-heading-id heading) heading table))
    (cl-labels
        ((rows-for (ids)
           (let ((out (make-hash-table :test 'eql)))
             (dolist (id ids out)
               (when-let ((heading (gethash id table)))
                 (puthash id (dorg-heading-deep-copy heading) out)))))
         (order-rows-for (parent-id sql)
           ;; Serve the ranged ordering read the way the store would.
           (let* ((bound (and (string-match "sort_key \\([<>]\\) X'\\([0-9a-f]+\\)'" sql)
                              (cons (match-string 1 sql)
                                    (dorg-sortkey-from-hex (match-string 2 sql)))))
                  (excluded (and (string-match "NOT IN (\\([0-9, ]+\\))" sql)
                                 (mapcar #'string-to-number
                                         (split-string (match-string 1 sql) ", "))))
                  (limit (and (string-match "LIMIT \\([0-9]+\\)" sql)
                              (string-to-number (match-string 1 sql))))
                  (desc (string-match-p "ORDER BY sort_key DESC" sql))
                  (ids (pcase parent-id (101 '(102 103 104)) (201 '(202)) (_ nil)))
                  (rows (cl-loop for id in ids
                                 for key = (dorg-heading-sort-key (gethash id table))
                                 unless (or (memql id excluded)
                                            (and bound
                                                 (if (equal (car bound) "<")
                                                     (not (string< key (cdr bound)))
                                                   (not (string< (cdr bound) key)))))
                                 collect (cons id key)))
                  (sorted (sort rows (if desc
                                         (lambda (a b) (string> (cdr a) (cdr b)))
                                       (lambda (a b) (string< (cdr a) (cdr b)))))))
             (mapcar (lambda (cell)
                       (list (cons 'id (car cell))
                             (cons 'sort_key_hex (dorg-sql-gen--sort-key-hex (cdr cell)))
                             ;; Like the store read, report the stored offset.
                             (cons 'level_offset
                                   (dorg-heading-level-offset (gethash (car cell) table)))))
                     (if limit (seq-take sorted limit) sorted)))))
      (cl-letf (((symbol-function 'dorg-head-commit) (lambda (_) "fixed-head"))
                ((symbol-function 'dorg-model-load-headings)
                 (lambda (_dir _root ids as-of &rest _)
                   (unless (equal as-of "fixed-head")
                     (error "unexpected AS-OF %S" as-of))
                   (rows-for ids)))
                ((symbol-function 'dorg-sql)
                 (lambda (_dir sql &rest _)
                   (push sql sql-log)
                   (cond
                    ((string-match
                      (concat "SELECT id, HEX(sort_key) AS sort_key_hex, level_offset FROM "
                              "headings.*parent_id = \\([0-9]+\\)")
                      sql)
                     (let ((parent-id (string-to-number (match-string 1 sql)))
                           (rows nil))
                       (unless (string-match-p " LIMIT " sql)
                         (push parent-id fan-parents))
                       (setq rows (order-rows-for parent-id sql))
                       (cl-incf order-queries)
                       (cl-incf order-rows (length rows))
                       rows))
                    (t (error "unexpected SQL %s" sql)))))
                ((symbol-function 'dorg-mutate--dolt-index-context)
                 (lambda (&rest _) nil)))
        (let ((sim (dorg-mutate--dolt-simulate source patches nil)))
          (list :sim sim
                :fan-parents (nreverse fan-parents)
                :order-queries order-queries
                :order-rows order-rows
                :sql (nreverse sql-log)))))))

(ert-deftest dorg-test-mutate-dolt-title-update-does-not-read-complete-fan ()
  "A non-ordering sparse update does not prove complete sibling facts."
  (let* ((patch (dorg-test-mutate--heading-update-patch 102 :title "Changed"))
         (res (dorg-test-mutate--dolt-simulate-counting-heading-fan-reads
               (list patch)))
         (sim (plist-get res :sim))
         (after (gethash 102 (dorg-mutate--ps-headings (plist-get sim :ps)))))
    (should (equal nil (plist-get res :fan-parents)))
    (should (equal '(100 102) (plist-get sim :scope-ids)))
    (should (equal '(102) (plist-get sim :changed)))
    (should (equal "Changed" (dorg-heading-title after)))))

(ert-deftest dorg-test-mutate-dolt-roomy-insert-reads-one-neighbour-row ()
  "W-bounded: a roomy anchored insert reads at most one ordering row per side.
Insertion before B needs B's predecessor from the store; B itself is the
loaded anchor.  No whole-group read happens."
  (let* ((patch (dorg-test-mutate--tree-insert-patch
                 (dorg-test-mutate--heading-address 101)
                 '(:id 300 :title "Inserted")
                 :before (dorg-test-mutate--heading-address 102)))
         (res (dorg-test-mutate--dolt-simulate-counting-heading-fan-reads
               (list patch)))
         (sim (plist-get res :sim))
         (after (gethash 300 (dorg-mutate--ps-headings (plist-get sim :ps)))))
    (should (null (plist-get res :fan-parents)))
    (should (<= (plist-get res :order-queries) 2))
    (should (<= (plist-get res :order-rows) 2))
    (should (member 300 (plist-get sim :created)))
    (should (string< (dorg-heading-sort-key after)
                     (dorg-heading-sort-key
                      (gethash 102 (dorg-mutate--ps-headings
                                    (plist-get sim :ps))))))))

(ert-deftest dorg-test-mutate-dolt-exhausted-insert-reads-only-the-requested-parent ()
  "W4: an exhausted insert reads ordering rows for its own parent only.
An unrelated touched child under another parent causes no ordering read
there, and the exhausted parent's unloaded siblings stay unloaded: they are
rekeyed as pending rekeys and appear in the effective order, not in the PS."
  (let* ((insert (dorg-test-mutate--tree-insert-patch
                  (dorg-test-mutate--heading-address 101)
                  '(:id 300 :title "Inserted")
                  :before (dorg-test-mutate--heading-address 102)))
         (unrelated (dorg-test-mutate--heading-update-patch 202 :title "Touched"))
         (res (dorg-test-mutate--dolt-simulate-counting-heading-fan-reads
               (list insert unrelated) t))
         (sim (plist-get res :sim))
         (ps (plist-get sim :ps))
         (kids (dorg-mutate--ps-children ps 101))
         (facts (sort (cdr (dorg-mutate--ps-order-delta ps 101))
                      (lambda (a b) (string< (cdr a) (cdr b)))))
         (other (gethash 202 (dorg-mutate--ps-headings ps))))
    (should (null (plist-get res :fan-parents)))
    (should (cl-every (lambda (sql) (not (string-match-p "parent_id = 201" sql)))
                      (cl-remove-if-not (lambda (sql) (string-match-p "HEX(sort_key)" sql))
                                        (plist-get res :sql))))
    (should (equal '(300 102) (mapcar #'dorg-heading-id kids)))
    (should (equal '(300 102 103 104) (mapcar #'car facts)))
    (should (dorg-test-mutate--keys-bounded-ordered-p (mapcar #'cdr facts)))
    (should (equal "Touched" (dorg-heading-title other)))
    (should-not (member 201 (plist-get res :fan-parents)))))

(ert-deftest dorg-test-mutate-dolt-exhausted-insert-rekeys-a-local-window ()
  "W-local: a before-first insert with no room rekeys a local neighbourhood.
The aged fan leaves no room below its first key, so the simulator reads a
bounded neighbourhood (never the whole group), hydrates nothing beyond the
placement slice, rekeys the window through one `:rekey' op, marks no sibling
changed, and lowers exactly one key-only UPDATE."
  (let* ((insert (dorg-test-mutate--tree-insert-patch
                  (dorg-test-mutate--heading-address 101)
                  '(:id 300 :title "Inserted")
                  :before (dorg-test-mutate--heading-address 102)))
         (res (dorg-test-mutate--dolt-simulate-counting-heading-fan-reads
               (list insert) t))
         (sim (plist-get res :sim))
         (ps (plist-get sim :ps))
         (rekey-ops (cl-remove-if-not (lambda (op) (eq (car op) :rekey))
                                      (plist-get sim :db-ops)))
         (rekey-stmts (cl-remove-if-not
                       (lambda (stmt)
                         (string-match-p "UPDATE headings h JOIN (VALUES " stmt))
                       (plist-get sim :sql))))
    (should (null (plist-get res :fan-parents)))
    (should (<= (plist-get res :order-rows) (* 2 (1+ dorg-mutate-rebalance-neighbourhood))))
    (should (equal '(100 101 102) (plist-get sim :scope-ids)))
    (should (equal '(300) (plist-get sim :created)))
    (should (null (plist-get sim :changed)))
    (should (= 1 (length rekey-ops)))
    (should (= 1 (length rekey-stmts)))
    ;; The loaded neighbour was rekeyed in place; the unloaded ones only pend.
    (should (gethash 102 (dorg-mutate--ps-rekeys ps)))
    (should-not (gethash 103 (dorg-mutate--ps-headings ps)))
    ;; Every stored row was rekeyed, so the pending delta alone carries the
    ;; candidate order (the store stub is out of scope here).
    (let* ((facts (sort (cdr (dorg-mutate--ps-order-delta ps 101))
                        (lambda (a b) (string< (cdr a) (cdr b)))))
           (ids (mapcar #'car facts))
           (keys (mapcar #'cdr facts)))
      (should (equal ids '(300 102 103 104)))
      (should (dorg-sortkey-ordered-p keys))
      (should (cl-every (lambda (k) (= (length k) dorg-sortkey-boundary-width))
                        keys)))))

(ert-deftest dorg-test-mutate-prepend-heading-legacy-fan-rekeys-a-bounded-window ()
  "W2: a prepend onto a 449-key legacy fan rekeys a window, never the whole fan.
The deepest legacy keys share sixteen leading zero bytes, so the window expands
over them until an outside key leaves room; shallow keys keep their bytes.
Order is preserved, no sibling is marked changed, and the rekeys lower to one
statement."
  (let* ((parent-id 10)
         (new-id 999)
         (ps (dorg-test-mutate--make-prepend-ps 449))
         (before-kids (dorg-mutate--ps-children ps parent-id))
         (old-ids (mapcar #'dorg-heading-id before-kids))
         (before-keys (mapcar #'dorg-heading-sort-key before-kids))
         (patch (dorg-test-mutate--note-prepend-patch
                 parent-id (car old-ids) new-id)))
    (dorg-mutate--ps-insert-heading ps patch)
    (let* ((kids (dorg-mutate--ps-children ps parent-id))
           (ids (mapcar #'dorg-heading-id kids))
           (keys (mapcar #'dorg-heading-sort-key kids))
           (rekeys (dorg-mutate--ps-rekeys ps))
           (ops (dorg-mutate--ps-rekey-ops ps (make-hash-table :test 'eql)))
           (stmts (dorg-sql-gen-lower-heading-ops
                   ops (make-hash-table :test 'eql)
                   (lambda () (ert-fail "heading rekey lowering allocates no item ids")))))
      (should (equal ids (cons new-id old-ids)))
      (should (= 450 (length keys)))
      (should (dorg-sortkey-ordered-p keys))
      (dolist (key keys) (should (dorg-sql-gen--sort-key-hex key)))
      (should (null (dorg-mutate--ps-changed ps)))
      (should (> (hash-table-count rekeys) 0))
      (should (< (hash-table-count rekeys) 449))
      (should (= 1 (length ops)))
      (should (= 1 (length stmts)))
      ;; Rekeyed rows are 16 bytes; untouched rows keep their legacy bytes.
      (cl-loop for kid in (cdr kids)
               for old in before-keys
               do (if (gethash (dorg-heading-id kid) rekeys)
                      (should (= dorg-sortkey-boundary-width
                                 (length (dorg-heading-sort-key kid))))
                    (should (equal old (dorg-heading-sort-key kid))))))))

(defun dorg-test-mutate--facts-fn (facts &optional counter)
  "Return an ordering reader over FACTS ((ID . KEY) ...) with store semantics.
COUNTER, a cons, has its car incremented and its cdr accumulates rows served."
  (lambda (_parent-id &optional bound side limit exclude-ids)
    (let* ((rows (cl-remove-if
                  (lambda (cell)
                    (or (memql (car cell) exclude-ids)
                        (and bound (if (eq side 'before)
                                       (not (string< (cdr cell) bound))
                                     (not (string< bound (cdr cell)))))))
                  facts))
           (sorted (sort (copy-sequence rows)
                         (if (eq side 'before)
                             (lambda (a b) (string> (cdr a) (cdr b)))
                           (lambda (a b) (string< (cdr a) (cdr b))))))
           (served (if limit (seq-take sorted limit) sorted)))
      (when counter
        (cl-incf (car counter))
        (cl-incf (cdr counter) (length served)))
      served)))

(ert-deftest dorg-test-mutate-prepend-heading-sparse-fan-reads-keys-only ()
  "W4: a sparse prepend reads ordering rows only and hydrates nothing.
The PS holds only the root, the parent and the first child; the store reader
answers ranged reads over the complete 449-key order.  The legacy cluster is
wider than the neighbourhood, so the group is read once (keys only); the
window rekeys unloaded rows as pending rekeys without adding a single heading
to the PS."
  (let* ((parent-id 10)
         (full (dorg-test-mutate--make-prepend-ps 449))
         (kids (dorg-mutate--ps-children full parent-id))
         (facts (mapcar (lambda (h) (cons (dorg-heading-id h)
                                          (dorg-heading-sort-key h)))
                        kids))
         (counter (cons 0 0))
         (ps (dorg-test-mutate--ps-with
              1 (gethash 1 (dorg-mutate--ps-headings full))
              (gethash parent-id (dorg-mutate--ps-headings full))
              (car kids)))
         (patch (dorg-test-mutate--note-prepend-patch
                 parent-id (dorg-heading-id (car kids)) 999)))
    (setf (dorg-mutate--ps-order-facts-fn ps)
          (dorg-test-mutate--facts-fn facts counter))
    (dorg-mutate--ps-insert-heading ps patch)
    ;; The zero-prefixed legacy cluster is wider than the neighbourhood, so
    ;; after the bounded reads the whole 449-row group is read once.
    (should (<= (cdr counter) (+ 449 (* 2 (1+ dorg-mutate-rebalance-neighbourhood)))))
    (should (= 4 (hash-table-count (dorg-mutate--ps-headings ps))))
    (should (> (hash-table-count (dorg-mutate--ps-rekeys ps)) 0))
    (should (< (hash-table-count (dorg-mutate--ps-rekeys ps)) 449))
    (let* ((after (dorg-mutate--ps-group ps parent-id))
           (ids (mapcar #'car (append after nil)))
           (keys (mapcar #'cdr (append after nil))))
      (should (equal ids (cons 999 (mapcar #'car facts))))
      (should (dorg-sortkey-ordered-p keys)))))

(ert-deftest dorg-test-mutate-dolt-note-prepend-legacy-fan-public-path ()
  "Public note-style Dolt apply rekeys a window of an aged note fan end to end."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir
                  (concat "* Task\n"
                          ":PROPERTIES:\n:BEAD_ID: task-r3\n:END:\n"
                          "** Notes\n")
                  1000))
           (owner-id (dorg-test-mutate--id-by-title dolt-dir "Task"))
           (notes-id (dorg-test-mutate--id-by-title dolt-dir "Notes"))
           (old-ids (number-sequence 10000 10448))
           (old-keys (dorg-test-mutate--raw-prepend-keys (length old-ids)))
           (source (list :dolt-dir dolt-dir :root-id root
                         :identity '(:property "BEAD_ID")))
           (title "[2026-09-12 Sat 09:20]")
           (body "Public note body.")
           (actor "driver")
           (updated-at "[2026-09-12 Sat 09:20]")
           (form
            `(batch
              (insert
               (heading
                (title ,title)
                (items
                 (drawer "PROPERTIES" ("BEAD_ACTOR" ,actor))
                 (text ,body)))
               (under (id ,notes-id) (before (id ,(car old-ids)))))
              (on (key "task-r3")
                  (set (property "BEAD_UPDATED_AT") ,updated-at))))
           fact-reads
           scopes
           (real-scope (symbol-function 'dorg-mutate--dolt-scope-ids))
           (real-facts (symbol-function 'dorg-mutate--dolt-order-facts)))
      (should owner-id)
      (should notes-id)
      (dorg-sql-batch
       dolt-dir
       (cl-loop for id in old-ids
                for key in old-keys
                collect
                (dorg-sql-gen-heading-insert
                 (make-dorg-heading
                  :id id :parent-id notes-id :level 3 :level-offset 1
                  :title (format "Old %d" id)
                  :sort-key key)
                 root)))
      (dorg-commit dolt-dir "seed aged note fan")
      (let ((head0 (dorg-head-commit dolt-dir)))
        (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                   (lambda (&rest args)
                     (let ((ids (apply real-scope args)))
                       (push (sort (copy-sequence ids) #'<) scopes)
                       ids)))
                  ((symbol-function 'dorg-mutate--dolt-order-facts)
                   (lambda (dir root parent-id &rest rest)
                     (push parent-id fact-reads)
                     (apply real-facts dir root parent-id rest))))
          (let* ((result (dorg-api-execute
                          form :mode 'apply :source source :base head0
                          :op-id "r3-note-prepend"))
                 (head1 (dorg-head-commit dolt-dir))
                 (tree (dorg-model-load-tree dolt-dir root))
                 (owner (gethash owner-id tree))
                 (kids (dorg-test-mutate--tree-children tree notes-id))
                 (ids (mapcar #'dorg-heading-id kids))
                 (keys (mapcar #'dorg-heading-sort-key kids))
                 (new-note (car kids)))
            (should (plist-get result :ok))
            (should-not (equal head0 head1))
            (should (equal head1 (plist-get result :new-token)))
            ;; Only the notes parent is read for ordering: bounded reads
            ;; and, because the legacy cluster is wider than the
            ;; neighbourhood, one whole-group read; no scope hydrated the fan.
            (should (cl-every (lambda (p) (= p notes-id)) fact-reads))
            (should (<= (length fact-reads) 6))
            (should (cl-notany (lambda (scope)
                                 (cl-every (lambda (id) (member id scope))
                                           old-ids))
                               scopes))
            (should (= 450 (length kids)))
            (should (equal (cdr ids) old-ids))
            (should (equal title (dorg-heading-title new-note)))
            (should (equal actor
                           (dorg-test-mutate--heading-property
                            new-note "BEAD_ACTOR")))
            (should (string-match-p (regexp-quote body)
                                    (dorg-test-mutate--heading-text new-note)))
            (should (equal updated-at
                           (dorg-test-mutate--heading-property
                            owner "BEAD_UPDATED_AT")))
            (should (dorg-sortkey-ordered-p keys))
            (should (= dorg-sortkey-boundary-width
                       (length (dorg-heading-sort-key new-note))))
            (should (cl-every (lambda (key)
                                (<= (length key) dorg-sortkey-max-bytes))
                              keys))))))))

(ert-deftest dorg-test-mutate-insert-prepend-normalizes-on-exhaustion ()
  "A before-first insert onto a complete aged PS rekeys a window (AC3).
The aged keys leave no room at the fixed width, so the placement rekeys the
siblings it must pass; rekeyed rows are recorded for key-only lowering and are
not marked changed."
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (dorg-test-mutate--aged-key 1)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (dorg-test-mutate--aged-key 2)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0 :level-offset 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root s1 s2)))
    (dorg-mutate--ps-insert-heading
     ps '(:op insert
          :target (:tree :under (:heading :id 0)
                   :before (:heading :id 10))
          :heading (:id 12 :title "prepended")))
    (let* ((kids (dorg-mutate--ps-children ps 0))
           (ids (mapcar #'dorg-heading-id kids))
           (keys (mapcar #'dorg-heading-sort-key kids)))
      (should (equal ids '(12 10 11)))
      (should (dorg-sortkey-ordered-p keys))
      (should (cl-every (lambda (key)
                          (= (length key) dorg-sortkey-boundary-width))
                        keys))
      (should (member 12 (dorg-mutate--ps-created ps)))
      (should (null (dorg-mutate--ps-changed ps)))
      (should (gethash 10 (dorg-mutate--ps-rekeys ps)))
      (should (gethash 11 (dorg-mutate--ps-rekeys ps))))))

(ert-deftest dorg-test-mutate-roomy-insert-rekeys-nothing ()
  "A roomy typed insert takes the fast-path key and rewrites no sibling (AC8).
The key equals what the gap planner returns for the two neighbours, no rekey
is recorded, and no sibling is marked changed — the window is room-triggered,
not unconditional."
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (unibyte-string #x40)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (unibyte-string #xc0)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0 :level-offset 0
                                  :sort-key (unibyte-string 1)))
         (ps (dorg-test-mutate--ps-with 0 root s1 s2))
         (expected (car (dorg-model-plan-gap (unibyte-string #x40)
                                             (unibyte-string #xc0) 1))))
    (dorg-mutate--ps-insert-heading
     ps '(:op insert
          :target (:tree :under (:heading :id 0) :after (:heading :id 10))
          :heading (:title "between")))
    (let ((new (cl-find "between" (dorg-mutate--ps-children ps 0)
                        :key #'dorg-heading-title :test #'equal)))
      (should (equal (dorg-heading-sort-key new) expected))
      ;; No rebalance: pre-existing siblings were not rewritten.
      (should (zerop (hash-table-count (dorg-mutate--ps-rekeys ps))))
      (should-not (memq 10 (dorg-mutate--ps-changed ps)))
      (should-not (memq 11 (dorg-mutate--ps-changed ps)))
      (should (equal (dorg-heading-sort-key s1) (unibyte-string #x40)))
      (should (equal (dorg-heading-sort-key s2) (unibyte-string #xc0))))))

(ert-deftest dorg-test-mutate-move-into-aged-siblings-rebalances ()
  "A same-root move into an aged gap rebalances instead of allocating a
cliff key (AC5).  The moved node lands ahead of the aged first sibling with
strictly-ordered, sub-cliff keys."
  (let* ((s1 (make-dorg-heading :id 10 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (dorg-test-mutate--aged-key 1)))
         (s2 (make-dorg-heading :id 11 :parent-id 0 :level 1 :level-offset 1
                                :sort-key (dorg-test-mutate--aged-key 2)))
         ;; Mover currently under a different parent so the move re-homes it.
         (mover (make-dorg-heading :id 20 :parent-id 5 :level 2 :level-offset 1
                                   :sort-key (unibyte-string #x80)))
         (other (make-dorg-heading :id 5 :parent-id 0 :level 1 :level-offset 1
                                   :sort-key (dorg-test-mutate--aged-key 9)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0 :level-offset 0
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
  (let* ((left (make-dorg-heading :id 30 :parent-id 0 :level 1 :level-offset 1
                                  :sort-key (dorg-test-mutate--aged-key 1)))
         (mid (make-dorg-heading :id 31 :parent-id 0 :level 1 :level-offset 1
                                 :sort-key (dorg-test-mutate--aged-key 2)))
         (right (make-dorg-heading :id 32 :parent-id 0 :level 1 :level-offset 1
                                   :sort-key (dorg-test-mutate--aged-key 3)))
         (k1 (make-dorg-heading :id 40 :parent-id 31 :level 2 :level-offset 1
                                :sort-key (unibyte-string #x40)))
         (k2 (make-dorg-heading :id 41 :parent-id 31 :level 2 :level-offset 1
                                :sort-key (unibyte-string #xc0)))
         (root (make-dorg-heading :id 0 :parent-id nil :level 0 :level-offset 0
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
keys force the before-first placement to the storage cliff; the move reads
the destination fan's keys, rekeys a window, and the executor emits one
key-only UPDATE for it.  Read back from Dolt, every child
key is sub-cliff and the mover leads — proving the rebalance is not just an
in-memory artifact but a committed transformation (raw midpoint instead left a
64-byte cliff key and stale 63-byte siblings)."
  (dorg-test-mutate--with-dolt
    (let* ((root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
           (c1 (make-dorg-heading :id 201 :parent-id 100 :level 1 :level-offset 1 :title "C1"
                                  :sort-key (dorg-test-mutate--aged-key 1)))
           (c2 (make-dorg-heading :id 202 :parent-id 100 :level 1 :level-offset 1 :title "C2"
                                  :sort-key (dorg-test-mutate--aged-key 2)))
           (g (make-dorg-heading :id 210 :parent-id 100 :level 1 :level-offset 1 :title "G"
                                 :sort-key (dorg-test-mutate--aged-key 5)))
           (m (make-dorg-heading :id 300 :parent-id 210 :level 2 :level-offset 1 :title "M"
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

(ert-deftest dorg-test-mutate-dolt-insert-aged-boundary-rekeys-in-one-simulation ()
  "AC3: a sparse insert with no room rekeys a window inside one simulation.
The scope stays the minimal placement slice (root plus the first aged sibling);
the exhausted placement reads the parent's key facts and never replays with a
hydrated fan.  The committed result preserves identity/content order and writes
fixed-width keys for the rekeyed window."
  (dorg-test-mutate--with-dolt
    (let* ((root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (a (make-dorg-heading :id 101 :parent-id 100 :level 1 :level-offset 1 :title "A"
                                 :sort-key (dorg-test-mutate--aged-key 1)))
           (b (make-dorg-heading :id 102 :parent-id 100 :level 1 :level-offset 1 :title "B"
                                 :sort-key (dorg-test-mutate--aged-key 2)))
           (c (make-dorg-heading :id 103 :parent-id 100 :level 1 :level-offset 1 :title "C"
                                 :sort-key (dorg-test-mutate--aged-key 3)))
           (src (list :dolt-dir dolt-dir :root-id 100))
           (scopes nil)
           (real-scope (symbol-function 'dorg-mutate--dolt-scope-ids)))
      (dorg-sql-batch dolt-dir
                      (list (dorg-sql-gen-heading-insert root-h 100)
                            (dorg-sql-gen-heading-insert a 100)
                            (dorg-sql-gen-heading-insert b 100)
                            (dorg-sql-gen-heading-insert c 100)))
      (dorg-commit dolt-dir "seed aged insert fan")
      (let* ((patches
              (list (dorg-test-mutate--tree-insert-patch
                     :root '(:id 300 :title "Prepended")
                     :before (dorg-test-mutate--heading-address 101))))
             (base (dorg-head-commit dolt-dir)))
        (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                   (lambda (&rest args)
                     (let ((ids (apply real-scope args)))
                       (push (sort (copy-sequence ids) #'<) scopes)
                       ids))))
          (dorg-mutate--dolt-simulate
           (dorg-mutate--resolve-source src) patches nil))
        (let ((res (dorg-test-mutate--apply-patch-batch src patches)))
          (should (plist-get res :ok))
          (should (member 300 (plist-get res :created))))
        (should-not (equal base (dorg-head-commit dolt-dir))))
      ;; Exactly one simulation with the minimal slice; no hydrated replay.
      (should (equal scopes '((100 101))))
      (let* ((tree (dorg-model-load-tree dolt-dir 100))
             (kids (dorg-test-mutate--tree-children tree 100))
             (ids (mapcar #'dorg-heading-id kids))
             (titles (mapcar #'dorg-heading-title kids))
             (keys (mapcar #'dorg-heading-sort-key kids)))
        (should (equal ids '(300 101 102 103)))
        (should (equal titles '("Prepended" "A" "B" "C")))
        (should (dorg-sortkey-ordered-p keys))
        (should (cl-every (lambda (key)
                            (= (length key) dorg-sortkey-boundary-width))
                          keys))))))

(ert-deftest dorg-test-mutate-dolt-cross-root-into-aged-dest-rekeys-destination ()
  "A cross-root move into an aged destination rekeys the destination window.
The destination parent carries a crafted 63-byte aged sibling fan, so a
before-first cross-root placement has no room.  The destination PS rekeys the
window it must pass and the executor lowers those rekeys with the moved rows
in the same transaction: the subtree arrives, leads the fan, every key is
sub-cliff, and both roots read back consistently."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed dolt-dir "* Movable\n** Kid\n* SrcSib\n" 100)
    (dorg-test-mutate--seed dolt-dir "* DestParent\n" 500)
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (da1 (make-dorg-heading :id 600 :parent-id 501 :level 2 :level-offset 1 :title "DA1"
                                   :sort-key (dorg-test-mutate--aged-key 1)))
           (da2 (make-dorg-heading :id 601 :parent-id 501 :level 2 :level-offset 1 :title "DA2"
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
             (res (dorg-test-mutate--apply-patch-batch src (list patch))))
        (should (plist-get res :ok))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let ((src-tree (dorg-model-load-tree dolt-dir 100)))
          (should-not (gethash 101 src-tree))
          (should-not (gethash 102 src-tree)))
        (let* ((dst-tree (dorg-model-load-tree dolt-dir 500))
               (kids (dorg-test-mutate--tree-children dst-tree 501))
               (keys (mapcar #'dorg-heading-sort-key kids)))
          (should (equal (mapcar #'dorg-heading-id kids) '(101 600 601)))
          (should (eql (dorg-heading-parent-id (gethash 102 dst-tree)) 101))
          (should (dorg-sortkey-ordered-p keys))
          (should (cl-every (lambda (k) (< (length k) dorg-sortkey-max-bytes))
                            keys)))))))

;;; ─── Shared heading ordering owner retirement guards ─────────────────────

(defun dorg-test-mutate--old-symbol (suffix)
  "Return an old private mutate symbol assembled from SUFFIX."
  (intern (concat "dorg-mutate--" suffix)))

(defun dorg-test-mutate--old-source-hit-p (needle)
  "Return non-nil if NEEDLE appears in dorg-mutate.el or dorg-sql-gen.el."
  (let ((hit nil)
        (root (file-name-as-directory
               (or (locate-dominating-file
                    (or load-file-name buffer-file-name default-directory)
                    "dorg.el")
                   default-directory))))
    (dolist (file '("dorg-mutate.el" "dorg-sql-gen.el"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file root))
        (goto-char (point-min))
        (when (search-forward needle nil t)
          (setq hit t))))
    hit))

(defun dorg-test-mutate--sql-index (stmts pattern)
  "Return the zero-based index of the first statement matching PATTERN."
  (cl-position-if (lambda (stmt) (string-match-p pattern stmt)) stmts))

(ert-deftest dorg-test-mutate-dolt-assembler-phases-and-single-rekey-statement ()
  "W3: the assembler lowers inserts, then updates and rekeys, then deletes.
A rekey op lowers to exactly one VALUES-join UPDATE; no temporary key and no
finalize statement exist anywhere in the output."
  (let* ((orig (make-hash-table :test 'eql))
         (new (make-dorg-heading :id 300 :parent-id 100 :level 1 :level-offset 1 :title "New"
                                 :sort-key (dorg-sortkey-encode 5000)))
         (pairs (list (cons 201 (dorg-sortkey-encode 1000))
                      (cons 202 (dorg-sortkey-encode 2000))))
         (sql (dorg-mutate--dolt-assemble-sql
               (list (list :delete-heading 203)
                     (list :rekey 100 pairs)
                     (list :insert-heading new 100))
               orig (lambda () 9000)))
         (kinds (mapcar (lambda (stmt)
                          (cond ((string-prefix-p "INSERT INTO headings" stmt) 'insert)
                                ((string-prefix-p "UPDATE headings h JOIN (VALUES " stmt) 'rekey)
                                ((string-prefix-p "DELETE FROM headings" stmt) 'delete)
                                (t 'other)))
                        sql)))
    (should (equal kinds '(insert rekey delete)))
    (should (= 1 (cl-count 'rekey kinds)))
    (should-not (cl-some (lambda (stmt) (string-match-p "WHERE id = 20[12]\\'" stmt)) sql))))


(ert-deftest dorg-test-mutate-dolt-bench-10k-fan-rekey ()
  "W6: a 10,000-sibling fan with no room anywhere renormalises in seconds.
Every seeded key shares sixteen leading zero bytes, so a before-first prepend
finds no room until the window covers the whole fan: the largest possible
rekey.  The apply runs the complete public path — planning, one key-only
UPDATE, validation, audit row, commit — and the wall time, statement count and
rekey count are reported.  Skipped unless DORG_BENCH is set."
  (unless (getenv "DORG_BENCH")
    (ert-skip "set DORG_BENCH=1 to run the 10k rekey benchmark"))
  (dorg-test-mutate--with-dolt
    (let* ((n 10000)
           (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                    :sort-key (unibyte-string #x80)))
           (prefix (make-string 16 0))
           (rows (cl-loop for i from 1 to n
                          collect (format "(%d, 100, 100, %s, 1, NULL, NULL, FALSE, %s, 0, NULL)"
                                          (+ 1000 i)
                                          (dorg-sql-gen--sort-key-hex
                                           (concat prefix (dorg-sortkey-from-integer i 2)))
                                          (dorg-sql-quote (format "N%d" i)))))
           (chunks (cl-loop for start from 0 below n by 500
                            collect (format (concat "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset,"
                                                    " todo_keyword, priority, commented, title, post_blank,"
                                                    " items_hash) VALUES %s")
                                            (mapconcat #'identity
                                                       (cl-subseq rows start (min n (+ start 500)))
                                                       ", "))))
           (src (list :dolt-dir dolt-dir :root-id 100))
           (statements 0)
           (real-batch (symbol-function 'dorg-sql--mysql-batch-on-conn)))
      (dorg-sql-batch dolt-dir (cons (dorg-sql-gen-heading-insert root 100) chunks))
      (dorg-commit dolt-dir "seed 10k aged fan")
      (let* ((base (dorg-head-commit dolt-dir))
             (t0 (float-time))
             ;; The public execute path: planning, the key-only UPDATE,
             ;; validation, audit row and commit, with an op-id.
             (res (cl-letf (((symbol-function 'dorg-sql--mysql-batch-on-conn)
                             (lambda (conn stmts &rest rest)
                               (setq statements (length stmts))
                               (apply real-batch conn stmts rest))))
                    (dorg-api-execute
                     '(insert (heading (title "Prepended"))
                              (under (id 100) (before (id 1001))))
                     :mode 'apply :source src :base base
                     :op-id "bench-10k-rekey")))
             (wall (- (float-time) t0)))
        (should (plist-get res :ok))
        (should-not (equal base (dorg-head-commit dolt-dir)))
        (let* ((facts (dorg-sql dolt-dir
                                (concat "SELECT id, title, LENGTH(sort_key) AS n FROM headings"
                                        " WHERE parent_id = 100 ORDER BY sort_key, id")))
               (ids (mapcar (lambda (r) (alist-get 'id r)) facts)))
          (should (= (1+ n) (length ids)))
          (should (equal "Prepended" (alist-get 'title (car facts))))
          (should (equal (cdr ids) (number-sequence 1001 (+ 1000 n))))
          (should (cl-every (lambda (r) (<= (alist-get 'n r) dorg-sortkey-max-bytes))
                            facts))
          (message "BENCH 10k rekey: wall=%.2fs statements=%d rekeyed-rows=%d"
                   wall statements
                   (cl-count-if (lambda (r) (= (alist-get 'n r) dorg-sortkey-boundary-width))
                                facts)))
        (should (< wall 60.0))))))

(ert-deftest dorg-test-mutate-dolt-delete-neighbour-then-insert-keeps-live-adjacency ()
  "F1: deleting a loaded neighbour never turns an unloaded live sibling into a collision.
Siblings A/101, B/102, C/103, D/104 hold keys 1..4 times 2^64.  One batch
deletes subtree B and inserts 200 before C.  The production scope hydrates
root, B and C only; placement locates the slot in the parent's fan (keys
only, read once), so the insert lands strictly between the still-live A and C
— the same key the whole-root simulation chooses (parity).  Control: without
the delete, the roomy insert also reads the fan once and lands between B and
C."
  (dorg-test-mutate--with-dolt
    (let* ((step dorg-sortkey-boundary-step)
           (root-h (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                      :sort-key (unibyte-string #x80)))
           (rows (cl-loop for id from 101 to 104
                          for n from 1
                          collect (make-dorg-heading
                                   :id id :parent-id 100 :level 1 :level-offset 1
                                   :title (format "S%d" id)
                                   :sort-key (dorg-sortkey-encode (* n step)))))
           (src (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id 100)))
           (a-key (dorg-sortkey-encode step))
           (c-key (dorg-sortkey-encode (* 3 step)))
           (insert (dorg-test-mutate--tree-insert-patch
                    :root '(:id 200 :title "X")
                    :before (dorg-test-mutate--heading-address 103)))
           (batch (list (list :op 'delete :target (list :subtree :id 102)) insert))
           (real-scope (symbol-function 'dorg-mutate--dolt-scope-ids))
           (real-facts (symbol-function 'dorg-mutate--dolt-order-facts))
           fact-reads scope)
      (dorg-sql-batch dolt-dir
                      (cons (dorg-sql-gen-heading-insert root-h 100)
                            (mapcar (lambda (h) (dorg-sql-gen-heading-insert h 100)) rows)))
      (dorg-commit dolt-dir "seed")
      (cl-flet ((new-key (sim)
                  (dorg-heading-sort-key
                   (gethash 200 (dorg-mutate--ps-headings (plist-get sim :ps))))))
        ;; Sparse production path.
        (let ((sparse
               (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                          (lambda (&rest args)
                            (setq scope (sort (copy-sequence (apply real-scope args)) #'<))
                            scope))
                         ((symbol-function 'dorg-mutate--dolt-order-facts)
                          (lambda (dir root parent-id &rest rest)
                            (push parent-id fact-reads)
                            (apply real-facts dir root parent-id rest))))
                 (new-key (dorg-mutate--dolt-simulate src batch nil))))
              ;; Whole-root parity.
              (whole (cl-letf (((symbol-function 'dorg-mutate--dolt-scope-ids)
                                (lambda (&rest _) nil)))
                       (new-key (dorg-mutate--dolt-simulate src batch nil)))))
          (should (equal scope '(100 102 103)))
          (should (equal fact-reads '(100)))   ; one predecessor read
          (should-not (equal sparse a-key))
          (should (string< a-key sparse))
          (should (string< sparse c-key))
          (should (equal sparse whole)))
        ;; Control: no removal, no facts read, key between B and C.
        (setq fact-reads nil)
        (let ((roomy (cl-letf (((symbol-function 'dorg-mutate--dolt-order-facts)
                                (lambda (dir root parent-id &rest rest)
                                  (push parent-id fact-reads)
                                  (apply real-facts dir root parent-id rest))))
                       (new-key (dorg-mutate--dolt-simulate src (list insert) nil)))))
          (should (equal fact-reads '(100)))
          (should (string< (dorg-sortkey-encode (* 2 step)) roomy))
          (should (string< roomy c-key))))
      ;; The batch commits and reads back as A, X, C, D.
      (let ((res (dorg-test-mutate--apply-patch-batch src batch)))
        (should (plist-get res :ok)))
      (let ((rows (dorg-sql dolt-dir
                            "SELECT id FROM headings WHERE parent_id = 100 ORDER BY sort_key, id")))
        (should (equal (mapcar (lambda (r) (alist-get 'id r)) rows) '(101 200 103 104)))))))

(ert-deftest dorg-test-mutate-window-work-is-linear ()
  "F2: a forced n-row window plans and applies with O(n) hash writes and no scans.
Every key shares sixteen leading zero bytes, so a before-first placement must
absorb the whole fan.  Hash insertions across planning and application stay
within a small constant times n, and application performs no linear search."
  (let* ((n 4000)
         (root (make-dorg-heading :id 0 :level 0 :level-offset 0 :sort-key (unibyte-string 1)))
         (kids (cl-loop for i from 1 to n
                        collect (make-dorg-heading
                                 :id i :parent-id 0 :level 1 :level-offset 1
                                 :sort-key (concat (make-string 16 0)
                                                   (dorg-sortkey-from-integer i 2)))))
         (ps (apply #'dorg-test-mutate--ps-with 0 root kids))
         (puthashes 0)
         (scans 0)
         (real-puthash (symbol-function 'puthash))
         (real-find (symbol-function 'cl-find))
         (real-position (symbol-function 'cl-position)))
    (cl-letf (((symbol-function 'puthash)
               (lambda (&rest args) (cl-incf puthashes) (apply real-puthash args))))
      (dorg-mutate--ps-insert-heading
       ps '(:op insert
            :target (:tree :under (:heading :id 0) :before (:heading :id 1))
            :heading (:id 9999 :title "prepended"))))
    (should (= (1+ n) (length (dorg-mutate--ps-children ps 0))))
    (should (= n (hash-table-count (dorg-mutate--ps-rekeys ps))))
    ;; An in-memory model IS its own pending set, so each ordering step is
    ;; proportional to n; a handful of such steps stays a small multiple.
    (should (< puthashes (* 16 n)))
    ;; Applying rekeys performs no linear search over the group.
    (let ((old (make-hash-table :test 'eql)))
      (cl-loop for cell across (dorg-mutate--ps-group ps 0)
               do (puthash (car cell) (cdr cell) old))
      (cl-letf (((symbol-function 'cl-find)
                 (lambda (&rest args) (cl-incf scans) (apply real-find args)))
                ((symbol-function 'cl-position)
                 (lambda (&rest args) (cl-incf scans) (apply real-position args))))
        (dorg-mutate--ps-apply-rekeys
         ps 0 old (list (cons 1 (dorg-sortkey-encode 12345)))))
      (should (zerop scans)))))

(defun dorg-test-mutate--bench-seed-10k-fan (dolt-dir)
  "Seed root 100 with 10,000 aged siblings (shared 16-byte zero prefix) and commit."
  (let* ((n 10000)
         (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (prefix (make-string 16 0))
         (rows (cl-loop for i from 1 to n
                        collect (format "(%d, 100, 100, %s, 1, NULL, NULL, FALSE, %s, 0, NULL)"
                                        (+ 1000 i)
                                        (dorg-sql-gen--sort-key-hex
                                         (concat prefix (dorg-sortkey-from-integer i 2)))
                                        (dorg-sql-quote (format "N%d" i)))))
         (chunks (cl-loop for start from 0 below n by 500
                          collect (format (concat "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset,"
                                                  " todo_keyword, priority, commented, title, post_blank,"
                                                  " items_hash) VALUES %s")
                                          (mapconcat #'identity
                                                     (cl-subseq rows start (min n (+ start 500)))
                                                     ", ")))))
    (dorg-sql-batch dolt-dir (cons (dorg-sql-gen-heading-insert root 100) chunks))
    (dorg-commit dolt-dir "seed 10k aged fan")
    n))

(defun dorg-test-mutate--bench-measure (label dolt-dir form)
  "Apply FORM through the public execute path on DOLT-DIR; report and return facts.
Counts store queries (with rows returned) and transaction statements, and
reports LABEL with the wall time."
  (let* ((src (list :dolt-dir dolt-dir :root-id 100))
         (base (dorg-head-commit dolt-dir))
         (queries 0) (rows 0) (statements 0)
         (order-queries 0) (order-rows 0) (group-reads 0)
         (real-sql (symbol-function 'dorg-sql))
         (real-facts (symbol-function 'dorg-mutate--dolt-order-facts))
         (real-batch (symbol-function 'dorg-sql--mysql-batch-on-conn))
         (t0 (float-time))
         (res (cl-letf (((symbol-function 'dorg-sql)
                         (lambda (&rest args)
                           (let ((out (apply real-sql args)))
                             (cl-incf queries)
                             (cl-incf rows (length out))
                             out)))
                        ((symbol-function 'dorg-mutate--dolt-order-facts)
                         (lambda (dir root parent-id &optional as-of bound side limit exclude-ids
                                 offsets-sink)
                           (let ((out (funcall real-facts dir root parent-id as-of
                                               bound side limit exclude-ids offsets-sink)))
                             (cl-incf order-queries)
                             (cl-incf order-rows (length out))
                             (unless limit (cl-incf group-reads))
                             out)))
                        ((symbol-function 'dorg-sql--mysql-batch-on-conn)
                         (lambda (conn stmts &rest rest)
                           (setq statements (length stmts))
                           (apply real-batch conn stmts rest))))
                (dorg-api-execute form :mode 'apply :source src :base base
                                  :op-id (format "bench-%s" label))))
         (wall (- (float-time) t0)))
    (should (plist-get res :ok))
    (message "BENCH %s: wall=%.3fs queries=%d rows=%d ordering-queries=%d ordering-rows=%d group-reads=%d statements=%d"
             label wall queries rows order-queries order-rows group-reads statements)
    (list :wall wall :queries queries :rows rows :statements statements
          :order-queries order-queries :order-rows order-rows :group-reads group-reads)))

(ert-deftest dorg-test-mutate-dolt-bench-10k-ordinary-append ()
  "M1: an ordinary append into a 10,000-sibling fan reads its keys once and rekeys nothing.
Skipped unless DORG_BENCH is set."
  (unless (getenv "DORG_BENCH")
    (ert-skip "set DORG_BENCH=1 to run the benchmarks"))
  (dorg-test-mutate--with-dolt
    (let* ((n (dorg-test-mutate--bench-seed-10k-fan dolt-dir))
           (m (dorg-test-mutate--bench-measure
               "10k-append" dolt-dir
               '(insert (heading (title "Appended")) (under (id 100))))))
      ;; The insert and the audit row; no rekey statement.  Ordering reads
      ;; transferred at most one row per side, never the fan.
      (should (= 2 (plist-get m :statements)))
      (should (<= (plist-get m :order-rows) 2))
      (should (zerop (plist-get m :group-reads)))
      (let ((rows (dorg-sql dolt-dir
                            (concat "SELECT title, LENGTH(sort_key) AS n FROM headings"
                                    " WHERE parent_id = 100 ORDER BY sort_key, id"))))
        (should (= (1+ n) (length rows)))
        (should (equal "Appended" (alist-get 'title (car (last rows)))))
        ;; No sibling was rekeyed: only the appended row is 16 bytes wide.
        (should (= 1 (cl-count-if (lambda (r) (= (alist-get 'n r) dorg-sortkey-boundary-width))
                                  rows)))))))

(ert-deftest dorg-test-mutate-dolt-bench-10k-multi-op-batch ()
  "M2: fifty appends in one batch read one ordering row each and commit together.
Skipped unless DORG_BENCH is set."
  (unless (getenv "DORG_BENCH")
    (ert-skip "set DORG_BENCH=1 to run the benchmarks"))
  (dorg-test-mutate--with-dolt
    (let* ((n (dorg-test-mutate--bench-seed-10k-fan dolt-dir))
           (m (dorg-test-mutate--bench-measure
               "10k-batch-50" dolt-dir
               (cons 'batch
                     (cl-loop for i from 1 to 50
                              collect `(insert (heading (title ,(format "B%d" i)))
                                               (under (id 100))))))))
      (should (zerop (plist-get m :group-reads)))
      (should (<= (plist-get m :order-rows) 50))
      (let ((rows (dorg-sql dolt-dir
                            "SELECT title FROM headings WHERE parent_id = 100 ORDER BY sort_key, id")))
        (should (= (+ n 50) (length rows)))
        (should (equal (mapcar (lambda (r) (alist-get 'title r)) (last rows 50))
                       (cl-loop for i from 1 to 50 collect (format "B%d" i))))))))

(ert-deftest dorg-test-mutate-dolt-bench-10k-large-t-placement ()
  "M3 / W-large-T: after a whole-group rekey, a further placement in the same
batch stays correct and its exclusion query stays cheap.  Skipped unless
DORG_BENCH is set."
  (unless (getenv "DORG_BENCH")
    (ert-skip "set DORG_BENCH=1 to run the benchmarks"))
  (dorg-test-mutate--with-dolt
    (let* ((n (dorg-test-mutate--bench-seed-10k-fan dolt-dir))
           (excluded-max 0)
           (elapsed 0.0)
           (real-facts (symbol-function 'dorg-mutate--dolt-order-facts))
           (m (cl-letf (((symbol-function 'dorg-mutate--dolt-order-facts)
                         (lambda (dir root parent-id &optional as-of bound side limit exclude-ids
                                 offsets-sink)
                           (let ((t0 (float-time)))
                             (prog1 (funcall real-facts dir root parent-id as-of
                                             bound side limit exclude-ids offsets-sink)
                               (when (> (length exclude-ids) excluded-max)
                                 (setq excluded-max (length exclude-ids)
                                       elapsed (- (float-time) t0))))))))
                (dorg-test-mutate--bench-measure
                 "10k-rekey-then-append" dolt-dir
                 '(batch (insert (heading (title "Prepended"))
                                 (under (id 100) (before (id 1001))))
                         (insert (heading (title "Appended")) (under (id 100))))))))
      (message "BENCH large-T: largest exclusion list=%d ids, that query=%.3fs"
               excluded-max elapsed)
      (should (>= excluded-max n))
      (let ((rows (dorg-sql dolt-dir
                            "SELECT title FROM headings WHERE parent_id = 100 ORDER BY sort_key, id")))
        (should (= (+ n 2) (length rows)))
        (should (equal "Prepended" (alist-get 'title (car rows))))
        (should (equal "Appended" (alist-get 'title (car (last rows)))))
        (ignore m)))))

(defun dorg-test-mutate--seed-fan (dolt-dir keys &optional parent-id start-id)
  "Seed DOLT-DIR with root 100 and siblings holding KEYS under PARENT-ID.
Ids run from START-ID (default 101); titles are S<id>.  Returns the id list."
  (let* ((parent (or parent-id 100))
         (root (make-dorg-heading :id 100 :level 0 :level-offset 0 :title "Root"
                                  :sort-key (unibyte-string #x80)))
         (ids (cl-loop for _ in keys for id from (or start-id 101) collect id))
         (rows (cl-loop for key in keys for id in ids
                        collect (make-dorg-heading
                                 :id id :parent-id parent :level 1 :level-offset 1
                                 :title (format "S%d" id) :sort-key key))))
    (dorg-sql-batch dolt-dir
                    (append (unless parent-id (list (dorg-sql-gen-heading-insert root 100)))
                            (mapcar (lambda (h) (dorg-sql-gen-heading-insert h 100)) rows)))
    (dorg-commit dolt-dir "seed fan")
    ids))

(defun dorg-test-mutate--fan-order (dolt-dir &optional parent-id)
  "Return (ID . KEY) rows under PARENT-ID (default 100) in stored order."
  (mapcar (lambda (r) (cons (alist-get 'id r)
                            (dorg-sortkey-from-hex (alist-get 'k r))))
          (dorg-sql dolt-dir
                    (format "SELECT id, HEX(sort_key) AS k FROM headings WHERE parent_id = %d ORDER BY sort_key, id"
                            (or parent-id 100)))))

(defun dorg-test-mutate--assert-fan (dolt-dir expected-ids &optional parent-id)
  "Assert the stored fan under PARENT-ID is EXPECTED-IDS with distinct ordered keys."
  (let ((rows (dorg-test-mutate--fan-order dolt-dir parent-id)))
    (should (equal (mapcar #'car rows) expected-ids))
    (should (dorg-sortkey-ordered-p (mapcar #'cdr rows)))
    (should (= (length rows)
               (length (cl-remove-duplicates (mapcar #'cdr rows) :test #'equal))))))

(defmacro dorg-test-mutate--with-ordering-spy (var &rest body)
  "Run BODY with VAR bound to a spy record of ordering reads.
VAR is a plist (:queries N :rows N :group-reads N :max-excluded N) updated
by every call of the store ordering reader."
  (declare (indent 1))
  `(let ((,var (list :queries 0 :rows 0 :group-reads 0 :max-excluded 0))
         (real (symbol-function 'dorg-mutate--dolt-order-facts)))
     (cl-letf (((symbol-function 'dorg-mutate--dolt-order-facts)
                (lambda (dir root parent-id &optional as-of bound side limit exclude-ids
                        offsets-sink)
                  (let ((out (funcall real dir root parent-id as-of
                                      bound side limit exclude-ids offsets-sink)))
                    (cl-incf (plist-get ,var :queries))
                    (cl-incf (plist-get ,var :rows) (length out))
                    (unless limit (cl-incf (plist-get ,var :group-reads)))
                    (setf (plist-get ,var :max-excluded)
                          (max (plist-get ,var :max-excluded) (length exclude-ids)))
                    out))))
       ,@body)))

(ert-deftest dorg-test-mutate-dolt-w-rekey-then-insert ()
  "W-rekey-then-insert: a placement after a same-batch rekey sees the new keys.
A and B are aged (no room), C is far.  Inserting X before B rekeys a window;
inserting Y after X in the same batch must land between X and the REKEYED B:
the store answer excludes the rekeyed ids and the pending rows supply them."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (list (dorg-test-mutate--aged-key 1)
                                               (dorg-test-mutate--aged-key 2)
                                               (dorg-sortkey-encode (ash 1 100))))
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "X")
                           :before (dorg-test-mutate--heading-address 102))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 301 :title "Y")
                           :after (dorg-test-mutate--heading-address 300))))
           spy)
      (dorg-test-mutate--with-ordering-spy s
        (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
        (setq spy s))
      (should (>= (plist-get spy :max-excluded) 1))
      (should (zerop (plist-get spy :group-reads)))
      (dorg-test-mutate--assert-fan dolt-dir '(101 300 301 102 103)))))

(ert-deftest dorg-test-mutate-dolt-w-created-anchor ()
  "W-created-anchor: anchors created earlier in the batch are ordinary pending rows."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (list (dorg-sortkey-encode (ash 1 100))
                                               (dorg-sortkey-encode (ash 1 101))))
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "X")
                           :after (dorg-test-mutate--heading-address 101))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 301 :title "Y")
                           :after (dorg-test-mutate--heading-address 300))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 302 :title "Z")
                           :before (dorg-test-mutate--heading-address 300)))))
      (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
      (dorg-test-mutate--assert-fan dolt-dir '(101 302 300 301 102)))))

(ert-deftest dorg-test-mutate-dolt-w-moved-anchor ()
  "W-moved-anchor: a mover entering the parent is a pending row; a placement after it sees the stored successor."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (list (dorg-sortkey-encode (ash 1 100))
                                               (dorg-sortkey-encode (ash 1 101))
                                               (dorg-sortkey-encode (ash 1 102))))
    ;; M lives under sibling 101 and moves before 103.
    (dorg-test-mutate--seed-fan dolt-dir (list (dorg-sortkey-encode (ash 1 100))) 101 200)
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--move-before-patch
                           200 :root (dorg-test-mutate--heading-address 103))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "N")
                           :after (dorg-test-mutate--heading-address 200)))))
      (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
      (dorg-test-mutate--assert-fan dolt-dir '(101 102 200 300 103))
      (should (null (dorg-test-mutate--fan-order dolt-dir 101))))))

(ert-deftest dorg-test-mutate-dolt-w-delete-chain ()
  "W-delete-chain: three deleted rows are skipped by one exclusion query.
Deleting A, B, C then inserting before D finds Z, the row before the chain,
with a single predecessor read whose exclusion list names the chain."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (cl-loop for n from 1 to 5
                                                  collect (dorg-sortkey-encode (* n (ash 1 100)))))
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (list :op 'delete :target (list :subtree :id 102))
                          (list :op 'delete :target (list :subtree :id 103))
                          (list :op 'delete :target (list :subtree :id 104))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "X")
                           :before (dorg-test-mutate--heading-address 105))))
           spy)
      (dorg-test-mutate--with-ordering-spy s
        (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
        (setq spy s))
      (should (= 1 (plist-get spy :queries)))
      (should (>= (plist-get spy :max-excluded) 3))
      (dorg-test-mutate--assert-fan dolt-dir '(101 300 105)))))

(ert-deftest dorg-test-mutate-dolt-w-distant ()
  "W-distant: a placement far from an earlier one finds its stored successor."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (cl-loop for n from 1 to 20
                                                  collect (dorg-sortkey-encode (* n (ash 1 100)))))
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "Tail"))
                          (dorg-test-mutate--tree-insert-patch
                           :root '(:id 301 :title "X")
                           :after (dorg-test-mutate--heading-address 105))))
           spy)
      (dorg-test-mutate--with-ordering-spy s
        (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
        (setq spy s))
      (should (<= (plist-get spy :rows) 2))
      (should (zerop (plist-get spy :group-reads)))
      (dorg-test-mutate--assert-fan
       dolt-dir (append (number-sequence 101 105) '(301) (number-sequence 106 120) '(300))))))

(ert-deftest dorg-test-mutate-dolt-w-local-then-full ()
  "W-local / W-full: a small aged cluster rekeys locally; a wide one falls back to the group.
Both go through the same rebalance function and lower one bulk statement."
  (dorg-test-mutate--with-dolt
    ;; Local: 60 far rows, a 3-row aged cluster, 60 far rows.
    (dorg-test-mutate--seed-fan
     dolt-dir (append (cl-loop for n from 1 to 60 collect (dorg-sortkey-encode (* n (ash 1 64))))
                      (list (dorg-test-mutate--aged-key 1) (dorg-test-mutate--aged-key 2)
                            (dorg-test-mutate--aged-key 3))
                      (cl-loop for n from 1 to 60 collect (dorg-sortkey-encode (+ (ash 1 120) (* n (ash 1 64)))))))
    ;; Aged keys sort first (zero prefix), so the cluster is ids 161..163 at the head.
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "X")
                           :before (dorg-test-mutate--heading-address 162))))
           (statements 0)
           (real-batch (symbol-function 'dorg-sql--mysql-batch-on-conn))
           spy)
      (dorg-test-mutate--with-ordering-spy s
        (cl-letf (((symbol-function 'dorg-sql--mysql-batch-on-conn)
                   (lambda (conn stmts &rest rest)
                     (setq statements (cl-count-if
                                       (lambda (st) (string-match-p "JOIN (VALUES" st)) stmts))
                     (apply real-batch conn stmts rest))))
          (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok)))
        (setq spy s))
      (should (zerop (plist-get spy :group-reads)))
      (should (= 1 statements))
      (let ((rows (dorg-test-mutate--fan-order dolt-dir)))
        (should (= 124 (length rows)))
        (should (dorg-sortkey-ordered-p (mapcar #'cdr rows)))
        ;; Only the cluster (and possibly its nearest neighbours) changed width.
        (should (<= (cl-count-if (lambda (r) (= 16 (length (cdr r)))) rows) 124))
        (should (>= (cl-count-if (lambda (r) (> (length (cdr r)) 16)) rows) 0))))
    ;; Full: 200 aged rows, no room anywhere; the neighbourhood cannot fit.
    (dorg-sql-batch dolt-dir '("DELETE FROM headings WHERE parent_id = 100"))
    (dorg-commit dolt-dir "clear")
    (dorg-test-mutate--seed-fan dolt-dir (cl-loop for n from 1 to 200
                                                  collect (concat (make-string 16 0)
                                                                  (dorg-sortkey-from-integer n 2)))
                                100 1001)
    (let* ((src (list :dolt-dir dolt-dir :root-id 100))
           (patches (list (dorg-test-mutate--tree-insert-patch
                           :root '(:id 300 :title "X")
                           :before (dorg-test-mutate--heading-address 1001))))
           spy)
      (dorg-test-mutate--with-ordering-spy s
        (should (plist-get (dorg-test-mutate--apply-patch-batch src patches) :ok))
        (setq spy s))
      (should (= 1 (plist-get spy :group-reads)))
      (let ((rows (dorg-test-mutate--fan-order dolt-dir)))
        (should (= 201 (length rows)))
        (should (= 300 (car (car rows))))
        (should (cl-every (lambda (r) (= 16 (length (cdr r)))) rows))))))

(ert-deftest dorg-test-mutate-dolt-w-index-use ()
  "W-index-use: the neighbour query is an indexed range scan with a hash exclusion."
  (dorg-test-mutate--with-dolt
    (dorg-test-mutate--seed-fan dolt-dir (list (dorg-sortkey-encode (ash 1 100))
                                               (dorg-sortkey-encode (ash 1 101))))
    (let* ((sql (concat "EXPLAIN PLAN SELECT id, HEX(sort_key) AS sort_key_hex, level_offset FROM headings"
                        " WHERE root_id = 100 AND parent_id = 100 AND sort_key < "
                        (dorg-sql-gen--sort-key-hex (dorg-sortkey-encode (ash 1 101)))
                        " AND id NOT IN (101, 102) ORDER BY sort_key DESC, id DESC LIMIT 1"))
           (plan (mapconcat (lambda (r) (format "%s" (alist-get 'plan r)))
                            (dorg-sql dolt-dir sql) "\n")))
      (should (string-match-p "IndexedTableAccess" plan))
      (should (string-match-p "index: \\[headings.parent_id,headings.sort_key\\]" plan))
      (should (string-match-p "HASH IN" plan))
      (should (string-match-p "Limit(1)\\|TopN" plan)))))

(ert-deftest dorg-test-mutate-retired-ordering-machinery-is-gone ()
  "D1: temp-key staging, finalize and complete-parent replay no longer exist."
  (dolist (sym '(dorg-mutate--ps-rebalance-order
                 dorg-mutate--ps-sibling-order-with
                 dorg-mutate--ps-parent-complete-p
                 dorg-mutate--ps-require-complete-parent
                 dorg-mutate--position-key
                 dorg-mutate--dolt-finalize-stmt
                 dorg-mutate--dolt-heading-with-sort-key
                 dorg-mutate--dolt-heading-staging-parent-ids
                 dorg-mutate--dolt-heading-order-facts
                 dorg-mutate--dolt-heading-order-facts-from-orig
                 dorg-mutate--ordering-facts-error-data
                 dorg-mutate--dolt-placement-row
                 dorg-mutate--dolt-placement-neighbor-id
                 dorg-mutate--dolt-last-child-id
                 dorg-mutate--dolt-insert-placement-ids
                 dorg-mutate--ps-order-facts
                 dorg-mutate--ps-remove-under
                 dorg-mutate--ps-slot-index
                 dorg-mutate--ps-fan
                 dorg-mutate--ps-fan-index
                 dorg-mutate--ps-fan-insert
                 dorg-mutate--ps-fan-remove
                 dorg-mutate--ps-fan-rekey
                 dorg-model-plan-heading-staging
                 dorg-model-plan-item-staging
                 dorg-model-plan-item-ordering
                 dorg-model-plan-heading-position
                 dorg-model-plan-heading-gap-keys
                 dorg-model-plan-heading-rebalance
                 dorg-model--ordering-staging-key
                 dorg-model--validate-heading-order-facts
                 dorg-sortkey-midpoint
                 dorg-sortkey-midpoint-limited
                 dorg-sortkey-normalized-keys
                 dorg-sortkey-width-for-count))
    (should-not (fboundp sym)))
  (dolist (sym '(dorg-mutate--ps-complete-parent-ids
                 dorg-mutate--dolt-complete-parent-ids
                 dorg-mutate--dolt-complete-parent-child-ids
                 dorg-sortkey-rebalance-threshold))
    (should-not (boundp sym))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-freed-slots ()
  "Retires the old freed-slot collector as a key-scheduling owner."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-freed-slots"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-staged-slot-p ()
  "Retires the old staged-slot predicate; planner owns the decision."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-staged-slot-p"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-temp-sort-key ()
  "Retires the old be8(id) heading temp-key function."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-temp-sort-key"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-sibling-keys ()
  "Retires the old mutate-side sibling-key census helper."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-sibling-keys"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-sibling-key-conflict-p ()
  "Retires the old distinct-sibling temp-key collision predicate."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-sibling-key-conflict-p"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-free-temp-key ()
  "Retires the old mutate-side temp search in favor of the shared model owner."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-free-temp-key"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-raise-staging-collision ()
  "Retires the old be8 collision raiser; planner either schedules or exhausts."
  (should-not (fboundp (dorg-test-mutate--old-symbol "dolt-raise-staging-collision"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-divergent-temp-guard ()
  "Retires the ordinary divergent-update source-parent be8 guard."
  (should-not (fboundp (dorg-test-mutate--old-symbol
                        "dolt-check-divergent-update-temp-key"))))

(ert-deftest dorg-test-mutate-dolt-heading-structural-retire-staged-slot-scheduling ()
  "Retires the old staged-slot scheduling branch inside the assembler."
  (should-not (dorg-test-mutate--old-source-hit-p "dorg-mutate--dolt-staged-slot-p")))

(ert-deftest dorg-test-mutate-refusal-carries-closed-diagnostic-facts ()
  "A database-failure refusal surfaces the executor's closed correlation facts.
The rolled-back receipt's `:statement-index', `:statement-class' and capture
status reach `dorg-mutate--rolled-back-decision's public refusal unchanged,
while nothing raw (server prose, SQL, values) crosses.  The receipt here is
built by hand to pin the passthrough alone; the real-executor control lives
beside the capture witnesses in the dolt tests."
  (let* ((receipt
          (list 'dorg-sql-transaction-rolled-back
                "Dolt transaction rolled back"
                (list :rollback-confirmed t
                      :transaction-phase 'statements
                      :class 'database-failure
                      :db-code 1105
                      :statement-index 2
                      :statement-class '(insert "headings")
                      :capture 'written
                      :capture-id "cap-RAW-NEVER-abc123")))
         (plan (list :caller-base "b0" :plan-base "b0"
                     :root-scope '(1) :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision receipt plan "h1" nil nil 0)
               :type 'dorg-mutate-error))
         (data (dorg-mutate-error-data err))
         (printed (format "%s %S" (error-message-string err) data)))
    (should (eq 'invariant (plist-get data :reason)))
    (should (eq 'database-failure (plist-get data :detail)))
    (should (= 2 (plist-get data :statement-index)))
    (should (equal '(insert "headings") (plist-get data :statement-class)))
    (should (eq 'written (plist-get data :capture)))
    (should (equal "cap-RAW-NEVER-abc123" (plist-get data :capture-id)))
    ;; The capture id is an opaque file stem; no server prose or SQL is present.
    (should-not (string-match-p "RAW-NEVER-[^a]" printed))
    (should-not (plist-member data :underlying))
    (should-not (plist-member data :message))))

(ert-deftest dorg-test-mutate-postimage-refusal-projects-classified-ids-only ()
  "The confirmed parity refusal exposes classifier IDs but never replans."
  (let* ((parity (condition-case caught
                     (signal 'dorg-sql-postimage-parity
                             (list "in-transaction readback diverged"
                                   '(:plane core :ids (7 11))))
                   (dorg-sql-postimage-parity caught)))
         (facts (dorg-dolt--classify-db-error parity))
         (receipt (list 'dorg-sql-transaction-rolled-back "rolled back"
                        (append (list :rollback-confirmed t
                                      :transaction-phase 'validate) facts)))
         (plan (list :caller-base "b0" :plan-base "b0"
                     :root-scope '(1) :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision receipt plan "h1" t "op" (+ (float-time) 60))
               :type 'dorg-mutate-error))
         (data (dorg-mutate-error-data err)))
    (should (eq 'invariant (dorg-mutate-error-reason err)))
    (should (eq 'postimage-parity (plist-get data :detail)))
    (should (equal '(7 11) (plist-get data :parity-ids)))
    (should (= 2 (plist-get data :parity-id-count)))
    (should (string-match-p "in-transaction readback diverged; rolled back pre-commit"
                            (error-message-string err))))
  (let* ((facts '(:class serialization :db-code 1213))
         (receipt (list 'dorg-sql-transaction-rolled-back "rolled back"
                        (append (list :rollback-confirmed t) facts)))
         (plan (list :caller-base "b0" :plan-base "b0" :root-scope '(1)
                     :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision receipt plan "h1" nil nil 0)
               :type 'dorg-mutate-error)))
    (should-not (plist-member (dorg-mutate-error-data err) :parity-ids))))

(ert-deftest dorg-test-mutate-postimage-validator-rollback-receipt-keeps-ids ()
  "The real validator, rollback builder, and refusal retain closed mismatch IDs."
  (let (queries parity receipt)
    (condition-case caught
        (dorg-mutate--postimage-assert-core
         (lambda (request)
           (when (eq (plist-get request :op) 'core-heading-ids)
             '(((id . 42)))))
         (make-hash-table :test 'eql) '(42))
      (dorg-sql-postimage-parity (setq parity caught)))
    (cl-letf (((symbol-function 'mysql-query)
               (lambda (_conn sql) (push sql queries) nil)))
      (condition-case caught
          (dorg-sql--raise-batch-rollback-result
           'conn parity 'validate nil t)
        (dorg-sql-transaction-rolled-back (setq receipt caught))))
    (let* ((plan (list :caller-base "b0" :plan-base "b0" :root-scope '(1)
                       :identity-scope '((1 1 1))))
           (err (should-error
                 (dorg-mutate--rolled-back-decision receipt plan "h1" t nil 0)
                 :type 'dorg-mutate-error))
           (data (dorg-mutate-error-data err)))
      (should (equal '(42) (plist-get data :parity-ids)))
      (should (= 1 (plist-get data :parity-id-count)))
      (should (equal '("ROLLBACK") (nreverse queries))))))

(ert-deftest dorg-test-mutate-unconfirmed-rollback-omits-parity-ids ()
  "A rollback-unconfirmed refusal never claims parity diagnostics or rollback."
  (let* ((receipt
          '(dorg-sql-transaction-rollback-failed "rollback failed"
            (:rollback-confirmed nil :class postimage-parity :parity-plane core
             :parity-ids (42) :parity-id-count 1)))
         (plan (list :caller-base "b0" :plan-base "b0" :root-scope '(1)
                     :identity-scope '((1 1 1))))
         (err (should-error
               (dorg-mutate--rolled-back-decision receipt plan "h1" t nil 0)
               :type 'dorg-mutate-error))
         (data (dorg-mutate-error-data err)))
    (should (eq 'rollback-failed (plist-get data :detail)))
    (should-not (plist-member data :parity-ids))
    (should-not (plist-member data :parity-id-count))
    (should-not (string-match-p "rolled back" (error-message-string err)))))

(ert-deftest dorg-test-q60zv-body-literal-engine-replaces-unique-cross-paragraph-range ()
  "One model-view literal may cross a normal paragraph boundary.
The changed postimage and preserved planning/property prefix make this a real
body operation rather than an occurrence-item substitute."
  (let* ((planning (dorg-planning-item :scheduled "<2026-10-16 Thu>"))
         (properties (dorg-drawer-item :name "PROPERTIES"
                                       :properties '(("DORG_ID" . "7"))))
         (heading (make-dorg-heading
                   :id 7 :level 1 :level-offset 1 :title "H"
                   :items (list planning properties
                                (dorg-text-item :text "First" :post-blank 1)
                                (dorg-text-item :text "Second" :post-blank 0))))
         (patch '(:op replace
                   :target (:body :host (:heading :id 7)
                                  :literal "First\n\nSecond")
                   :value "Joined"))
         (new (dorg-mutate--apply-body-literal-to-heading heading patch)))
    (should (equal "First\n\nSecond\n" (dorg-model-body-view heading)))
    (should (equal "Joined\n" (dorg-model-body-view new)))
    (let ((new-planning (car (dorg-heading-items new)))
          (new-drawer (cadr (dorg-heading-items new))))
      (should (equal "<2026-10-16 Thu>"
                     (dorg-planning-item-scheduled new-planning)))
      (should (equal "7" (cdr (assoc "DORG_ID"
                                     (dorg-drawer-item-properties new-drawer))))))))

(ert-deftest dorg-test-q60zv-body-literal-engine-refuses-zero-and-overlap ()
  "Zero and overlapping repeated literals refuse before a heading is copied."
  (let ((heading (make-dorg-heading
                  :id 7 :level 1 :level-offset 1 :title "H"
                  :items (list (dorg-text-item :text "aaa" :post-blank 0)))))
    (dolist (case '(("missing" 0)
                    ("aa" 2)))
      (let* ((literal (car case))
             (count (cadr case))
             (data (dorg-test-mutate--error-data
                   (dorg-mutate--apply-body-literal-to-heading
                    heading `(:op replace :target (:body :host (:heading :id 7)
                                                           :literal ,literal)
                                           :value "new")))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'body-literal-count (plist-get data :detail)))
        (should (= count (plist-get data :count)))))))

(ert-deftest dorg-test-q60zv-body-literal-plain-replaces-mapped-cross-paragraph-range ()
  "The plain adapter changes only a mapped normal-body literal range."
  (let ((before (concat "* Host\n"
                        "SCHEDULED: <2026-10-16 Thu>\n"
                        ":PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                        "First\n\nSecond\n"
                        "** Child\nChild OLD stays.\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((patch '(:op replace
                     :target (:body :host (:heading :id 7)
                                    :literal "First\n\nSecond")
                     :value "Joined")))
        (should (plist-get (dorg-test-mutate--apply-patch-batch buf (list patch)) :ok))
        (should (equal (concat "* Host\n"
                               "SCHEDULED: <2026-10-16 Thu>\n"
                               ":PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                               "Joined\n"
                               "** Child\nChild OLD stays.\n")
                       (buffer-string)))))))

(ert-deftest dorg-test-q60zv-body-literal-plain-refuses-lossy-model-mapping ()
  "A comma-escaped verbatim model view never falls back to source matching."
  (let ((before (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                        "#+begin_example\n,* OLD\n#+end_example\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list '(:op replace
                                :target (:body :host (:heading :id 7) :literal "* OLD")
                                :value "new"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'body-literal-unmappable (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-q60zv-body-literal-byte-equality-guards-the-source-span ()
  "The span mapper itself refuses a model/source byte mismatch.

This pins the mapping guard independently of the later whole-postimage check:
removing the segment equality makes this control RED before any mutation can
mask the missing proof."
  (dorg-test-mutate--with-content
      buf (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                  "#+begin_example\n,* OLD\n#+end_example\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (goto-char (point-min))
    (let ((data (dorg-test-mutate--error-data
                 (dorg-mutate--source-preserving-body-literal-span
                  '(:op replace
                    :target (:body :host (:heading :id 7) :literal "* OLD")
                    :value "new")
                  (point-marker) nil))))
      (should (eq 'invalid-target (plist-get data :reason)))
      (should (eq 'body-literal-unmappable (plist-get data :detail))))))

(ert-deftest dorg-test-q60zv-body-literal-plain-refuses-collapsed-whitespace ()
  "A whitespace-only paragraph separator cannot be mapped as canonical bytes."
  (let ((before (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                        "Start.\n   \nEnd.\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list '(:op replace
                                :target (:body :host (:heading :id 7)
                                               :literal "Start.\n\nEnd")
                                :value "Joined"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'body-literal-unmappable (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-q60zv-body-literal-plain-refuses-view-mismatch ()
  "Plain refuses the f2 local-drawer ambiguity before matching source bytes."
  (let ((before (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                        ":PROPERTIES:\n:KEY: OLD\n:END:\n"
                        "Body.\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list '(:op replace
                                :target (:body :host (:heading :id 7) :literal "OLD")
                                :value "NEW"))))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'body-literal-unmappable
                    (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-q60zv-public-body-literal-uses-the-mutation-door ()
  "The public form reaches the same mapped body operation, not item replace."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nPublic OLD.\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-api-execute
              '(on (id 7) (replace (body (literal "Public OLD")) "Public NEW"))
              :mode 'apply :source buf)
             :ok))
    (should (equal "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nPublic NEW.\n"
                   (buffer-string)))))

(ert-deftest dorg-test-q60zv-dolt-replaces-the-same-model-body-range ()
  "Dolt uses model replacement and pins replay/mismatch at its committed HEAD."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Host\nFirst\n\nSecond\n"))
           (id (dorg-test-mutate--id-by-title dolt-dir "Host"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (form `(on (id ,id)
                      (replace (body (literal "First\n\nSecond")) "Joined")))
           (result (dorg-api-execute form :mode 'apply :source source
                                     :op-id "q60zv-body-replay"))
           (head (dorg-head-commit dolt-dir))
           (heading (gethash id (dorg-model-load-tree dolt-dir root))))
      (should (plist-get result :ok))
      (should (equal "Joined\n" (dorg-model-body-view heading)))
      (let ((replay (dorg-api-execute form :mode 'apply :source source
                                      :op-id "q60zv-body-replay")))
        (should (plist-get replay :idempotent))
        (should (equal head (dorg-head-commit dolt-dir))))
      (should
       (eq 'idempotency-key-mismatch
           (condition-case err
               (progn
                 (dorg-api-execute
                  `(on (id ,id) (replace (body (literal "Joined")) "Different"))
                  :mode 'apply :source source :op-id "q60zv-body-replay")
                 :no-error)
             (dorg-api-error (dorg-api-error-reason err))
             (dorg-mutate-error (dorg-mutate-error-reason err)))))
      (should (equal head (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-q60zv-dolt-refuses-boundary-heading-injection ()
  "Dolt rejects a deletion that would expose a new headline in the body."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* Host\nx* Injected\n"))
           (id (dorg-test-mutate--id-by-title dolt-dir "Host"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (head (dorg-head-commit dolt-dir))
           (data (dorg-test-mutate--error-data
                  (dorg-api-execute
                   `(on (id ,id) (replace (body (literal "x")) ""))
                   :mode 'apply :source source))))
      (should (eq 'invalid-target (plist-get data :reason)))
      (should (eq 'structural-payload-boundary (plist-get data :detail)))
      (should (equal head (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-q60zv-body-literal-noop-still-checks-one-match ()
  "OLD equal to NEW is unchanged only after the literal guard ran."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nSame.\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string))
          (calls 0)
          (real (symbol-function 'dorg-model-body-view-literal-match)))
      (cl-letf (((symbol-function 'dorg-model-body-view-literal-match)
                 (lambda (&rest args) (setq calls (1+ calls)) (apply real args))))
        (should (plist-get
                 (dorg-api-execute
                  '(on (id 7) (replace (body (literal "Same")) "Same"))
                  :mode 'apply :source buf)
                 :ok)))
      ;; One plan check plus the actual source mapping check: deleting the
      ;; operation's guard therefore cannot make this no-op control pass.
      (should (>= calls 2))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-q60zv-late-body-literal-refusal-is-prewrite-atomic ()
  "A later body precondition refusal leaves earlier batch requests unwritten."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nExisting.\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let ((before (buffer-string))
          (data (dorg-test-mutate--error-data
                 (dorg-test-mutate--apply-patch-batch
                  buf '((:op put :target (:property :host (:heading :id 7) :name "K")
                                :value "V")
                        (:op replace :target (:body :host (:heading :id 7)
                                                       :literal "absent")
                                     :value "new"))))))
      (should (eq 'invalid-target (plist-get data :reason)))
      (should (eq 'body-literal-count (plist-get data :detail)))
      (should (= 0 (plist-get data :count)))
      (should (equal before (buffer-string))))))

(ert-deftest dorg-test-q60zv-body-scan-excludes-leading-planning-without-drawer ()
  "Leading planning is outside body matching even without local PROPERTIES."
  (with-temp-buffer
    (insert "* Host\nSCHEDULED: <2026-10-16 Thu>\nBody OLD.\n")
    (let ((org-inhibit-startup t)) (org-mode))
    (goto-char (point-min))
    (let* ((scan (dorg-mutate--source-preserving-body-scan (point-marker) nil))
           (body (buffer-substring-no-properties (nth 0 scan) (nth 1 scan))))
      (should (equal "Body OLD.\n" body)))))

(ert-deftest dorg-test-q60zv-body-literal-keeps-a-body-properties-lookalike ()
  "A body drawer after text is part of the common model/source view."
  (let ((before (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                        "Before.\n:PROPERTIES:\n:K: OLD\n:END:\n")))
    (dorg-test-mutate--with-content buf before
      (dorg-test-mutate--assert-dorg-policy buf)
      (should (plist-get
               (dorg-test-mutate--apply-patch-batch
                buf (list '(:op replace
                            :target (:body :host (:heading :id 7) :literal "OLD")
                            :value "NEW")))
               :ok))
      (should (equal (replace-regexp-in-string "OLD" "NEW" before t t)
                     (buffer-string))))))

(ert-deftest dorg-test-q60zv-body-literal-simulates-an-ordered-chain ()
  "Later body patches plan against the preceding postimage, not live bytes."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nStart\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf '((:op replace
                         :target (:body :host (:heading :id 7) :literal "Start")
                         :value "Begin")
                    (:op replace
                         :target (:body :host (:heading :id 7) :literal "Begin")
                         :value "Go")))
             :ok))
    (should (string-suffix-p "Go\n" (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-simulates-a-prior-text-patch ()
  "A body literal sees an earlier ordinary text replacement in its batch."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nStart\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf '((:op replace
                         :target (:text :host (:heading :id 7)
                                        :select (:occurrence 1))
                         :value "Begin")
                    (:op replace
                         :target (:body :host (:heading :id 7) :literal "Begin")
                         :value "Go")))
             :ok))
    (should (string-suffix-p "Go\n" (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-simulates-before-a-later-text-patch ()
  "An ordinary text patch observes the body literal's simulated postimage."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nStart\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf '((:op replace
                         :target (:body :host (:heading :id 7) :literal "Start")
                         :value "Begin")
                    (:op replace
                         :target (:text :host (:heading :id 7)
                                        :select (:occurrence 1))
                         :value "Go")))
             :ok))
    (should (string-suffix-p "Go\n" (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-does-not-count-a-sibling ()
  "The host body's literal count excludes sibling body text."
  (dorg-test-mutate--with-content
      buf (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nHost OLD\n"
                  "* Sibling\n:PROPERTIES:\n:DORG_ID: 8\n:END:\nSibling OLD\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf (list '(:op replace
                          :target (:body :host (:heading :id 7) :literal "OLD")
                          :value "NEW")))
             :ok))
    (should (equal (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nHost NEW\n"
                           "* Sibling\n:PROPERTIES:\n:DORG_ID: 8\n:END:\nSibling OLD\n")
                   (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-stale-base-refuses-before-write ()
  "A dry-run base token cannot write through a later source revision."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nOLD\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (let* ((patch '(:op replace
                   :target (:body :host (:heading :id 7) :literal "OLD")
                   :value "NEW"))
           (base (plist-get (dorg-mutate--plan-patches buf (list patch))
                            :base-revision)))
      (goto-char (point-max))
      (insert "Drift.\n")
      (let ((before (buffer-string))
            (data (dorg-test-mutate--error-data
                   (dorg-test-mutate--apply-patch-batch
                    buf (list patch) :base-revision base))))
        (should (eq 'stale (plist-get data :reason)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-q60zv-body-view-three-items-appends-one-final-lf ()
  "Three body items retain both paragraph gaps and the renderer's final LF."
  (let ((heading (make-dorg-heading
                  :id 7 :level 1 :level-offset 1 :title "H"
                  :items (list (dorg-text-item :text "Start." :post-blank 1)
                               (dorg-text-item :text "End one." :post-blank 1)
                               (dorg-text-item :text "Next para." :post-blank 0)))))
    (should (equal "Start.\n\nEnd one.\n\nNext para.\n"
                   (dorg-model-body-view heading)))))

(ert-deftest dorg-test-q60zv-body-literal-permits-paragraph-splitting ()
  "A representable NEW may introduce a normal paragraph boundary."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nOLD\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf (list '(:op replace
                          :target (:body :host (:heading :id 7) :literal "OLD")
                          :value "A.\n\nB.")))
             :ok))
    (should (string-suffix-p "A.\n\nB.\n" (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-refuses-heading-injection ()
  "Boundary deletion/insertion refuses in planning and apply without a write."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nx* Injected\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (dolist (new '("" "* Split\n"))
      (let ((before (buffer-string))
            (patch `(:op replace
                       :target (:body :host (:heading :id 7) :literal "x")
                       :value ,new)))
        (dolist (run (list (lambda () (dorg-mutate--plan-patches buf (list patch)))
                           (lambda () (dorg-test-mutate--apply-patch-batch
                                       buf (list patch)))))
          (let ((data (dorg-test-mutate--error-data (funcall run))))
            (should (eq 'invalid-target (plist-get data :reason)))
            (should (eq 'structural-payload-boundary
                        (plist-get data :detail)))
            (should (equal before (buffer-string)))))))))

(ert-deftest dorg-test-q60zv-body-literal-refuses-structured-and-protected-postimages ()
  "Only paragraph geometry may change through the body literal door.

Each value is parsed in its composed body context.  The before/after witness
makes a deleted validator fail rather than merely exercising an invalid DSL
form."
  (dolist (value '("#+DORG_HASH: forged"
                   ":PROPERTIES:\n:DORG_ID: 9\n:END:"
                   "#+KEY: value"
                   ":NOTE:\n- created\n:END:"))
    (dorg-test-mutate--with-content
        buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nOLD\n"
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((before (buffer-string))
            (patch `(:op replace
                       :target (:body :host (:heading :id 7) :literal "OLD")
                       :value ,value)))
        (dolist (run (list (lambda () (dorg-mutate--plan-patches buf (list patch)))
                           (lambda () (dorg-test-mutate--apply-patch-batch
                                       buf (list patch)))))
          (let ((data (dorg-test-mutate--error-data (funcall run))))
            (should (eq 'invalid-target (plist-get data :reason)))
            (should (eq 'structural-payload-boundary
                        (plist-get data :detail)))
            (should (equal before (buffer-string)))))))))

(ert-deftest dorg-test-q60zv-body-literal-simulates-paragraph-split-before-text ()
  "A later positional occurrence sees every parsed item from a body split."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nStart\n\nTail\n"
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-test-mutate--apply-patch-batch
              buf '((:op replace
                         :target (:body :host (:heading :id 7) :literal "Start")
                         :value "A.\n\nB.")
                    (:op replace
                         :target (:text :host (:heading :id 7)
                                        :select (:occurrence 3))
                         :value "Z.")))
             :ok))
    (should (string-suffix-p "A.\n\nB.\n\nZ.\n" (buffer-string)))))

(ert-deftest dorg-test-q60zv-body-literal-file-simulates-paragraph-split-before-text ()
  "The file adapter gives a later positional patch the parsed body postimage."
  (let* ((before "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nStart\n\nTail\n")
         (file (make-temp-file "dorg-q60zv-batch-file" nil ".org" before)))
    (unwind-protect
        (progn
          (should (plist-get
                   (dorg-api-execute
                    '(batch
                      (on (id 7) (replace (body (literal "Start")) "A.\n\nB."))
                      (on (id 7) (replace (text (occurrence 3)) "Z.")))
                    :mode 'apply :source (list :file file :identity 'dorg))
                   :ok))
          (should (equal "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nA.\n\nB.\n\nZ.\n"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (when-let ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest dorg-test-q60zv-body-literal-file-source-is-byte-surgical ()
  "A file descriptor uses the same mapped body range as a live buffer."
  (let* ((before (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                         "OLD body.\n* Sibling\n:PROPERTIES:\n:DORG_ID: 8\n:END:\n"
                         "OLD sibling.\n"))
         (file (make-temp-file "dorg-q60zv-file" nil ".org" before)))
    (unwind-protect
        (let ((result
               (dorg-api-execute
                '(on (id 7) (replace (body (literal "OLD body")) "NEW body"))
                :mode 'apply :source (list :file file :identity 'dorg))))
          (should (plist-get result :ok))
          (should (equal (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                                 "NEW body.\n* Sibling\n:PROPERTIES:\n:DORG_ID: 8\n:END:\n"
                                 "OLD sibling.\n")
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (when-let ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest dorg-test-q60zv-body-literal-property-identity-buffer-and-file ()
  "Literal bodies resolve a property-identity host for preview and apply."
  (let ((before "* Host\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\nOLD\n"))
    (dorg-test-mutate--with-content buf before
      (dolist (mode '(dry-run apply))
        (let ((result (dorg-api-execute
                       '(on (key "inbox")
                            (replace (body (literal "OLD")) "NEW"))
                       :mode mode :source (list :buffer buf
                                                :identity '(:property "CAPTURE_ID")))))
          (should (plist-get result :ok))
          (if (eq mode 'dry-run)
              (should (equal before (buffer-string)))
            (should (string-suffix-p "NEW\n" (buffer-string)))))))
    (let ((file (make-temp-file "dorg-q60zv-property" nil ".org" before)))
      (unwind-protect
          (dolist (mode '(dry-run apply))
            (let ((result (dorg-api-execute
                           '(on (key "inbox")
                                (replace (body (literal "OLD")) "NEW"))
                           :mode mode :source (list :file file
                                                    :identity '(:property "CAPTURE_ID")))))
              (should (plist-get result :ok))
              (if (eq mode 'dry-run)
                  (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string))))
                (should (equal "* Host\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\nNEW\n"
                               (with-temp-buffer (insert-file-contents file) (buffer-string)))))))
        (when-let ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
        (delete-file file)))))

(ert-deftest dorg-test-q60zv-property-identity-body-refuses-forged-dorg-id ()
  "A property-bound body cannot manufacture a DORG_ID drawer."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:CAPTURE_ID: inbox\n:END:\nOLD\n"
    (let ((before (buffer-string))
          (data (dorg-test-mutate--error-data
                 (dorg-api-execute
                  '(on (key "inbox")
                       (replace (body (literal "OLD")) ":PROPERTIES:\n:DORG_ID: 9\n:END:"))
                  :mode 'apply :source (list :buffer buf :identity '(:property "CAPTURE_ID"))))))
      (should (eq 'structural-payload-boundary (plist-get data :detail)))
      (should (equal before (buffer-string))))))

(defun dorg-test-7isy6--assert-refusal-in-each-mode (form source before current)
  "Assert FORM refuses before SOURCE's bytes change in both public modes.

BEFORE is the exact byte string expected from the zero-argument CURRENT
reader.  Keeping this assertion at the public API boundary makes the control
fail if planning, application, or either refusal datum regresses."
  (dolist (mode '(dry-run apply))
    (let ((data (dorg-test-mutate--error-data
                 (dorg-api-execute form :mode mode :source source))))
      (should (eq 'invalid-target (plist-get data :reason)))
      (should (eq 'structural-payload-boundary (plist-get data :detail)))
      (should (equal before (funcall current))))))

(ert-deftest dorg-test-7isy6-item-and-drawer-line-refuse-protected-postimages ()
  "Item and drawer-line payloads cannot manufacture owned metadata.

The pre-fix item and drawer-line paths accept these forms, unlike the text
path.  Every refusal is checked at both plan and apply, before the backing
bytes change."
  (dolist (form '((on (id 7)
                      (replace (item (occurrence 1))
                               (text "#+DORG_HASH: forged")))
                  (on (id 7)
                      (replace (item (occurrence 1))
                               (text "  #+dorg_hash: forged")))
                  (on (id 7)
                      (replace (item (occurrence 1))
                               (text "#+KEY: ordinary structural payload")))
                  (on (id 7)
                      (replace (item (all))
                               (text ":PROPERTIES:\n:DORG_ID: 9\n:END:")))
                  (on (id 7)
                      (insert (drawer-line "LOGBOOK") (before-first)
                              ":DORG_ID: 9"))
                  (on (id 7)
                      (replace (drawer-line "LOGBOOK" (all))
                               ":DORG_CLOCK_START: x"))))
    (dorg-test-mutate--with-content
        buf (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                    "old\n:LOGBOOK:\nentry\n:END:\n")
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((before (buffer-string)))
        (dolist (run (list (lambda () (dorg-api-execute form :mode 'dry-run
                                                        :source (list :buffer buf :identity 'dorg)))
                           (lambda () (dorg-api-execute form :mode 'apply
                                                        :source (list :buffer buf :identity 'dorg)))))
          (let ((data (dorg-test-mutate--error-data (funcall run))))
            (should (eq 'invalid-target (plist-get data :reason)))
            (should (eq 'structural-payload-boundary (plist-get data :detail)))
            (should (equal before (buffer-string)))))))))

(ert-deftest dorg-test-ay3q8-body-property-guard-consults-owned-family-seam ()
  "Registered-family recognition is a load-bearing body-postimage disjunct.

The outer mutation rows establish the public refusal.  This narrow spy keeps
the explicit owned-family call non-vacuous while the current DORG_ prefix
happens to cover the same literal key."
  (let ((calls 0)
        (real (symbol-function 'dorg-owned-metadata-key-p)))
    (cl-letf (((symbol-function 'dorg-protected-keyword-p)
               (lambda (_key) nil))
              ((symbol-function 'dorg-owned-metadata-key-p)
               (lambda (key)
                 (cl-incf calls)
                 (funcall real key))))
      (should (dorg-mutate--protected-body-property-p "DORG_CLOCK_START"))
      (should (= 1 calls)))))

(ert-deftest dorg-test-7isy6-ordinary-item-and-drawer-line-remain-legal ()
  "The protected-postimage guard does not reject ordinary payloads."
  (dorg-test-mutate--with-content
      buf (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                  "old\n:LOGBOOK:\nentry\n:END:\n")
    (dorg-test-mutate--assert-dorg-policy buf)
    (should (plist-get
             (dorg-api-execute '(on (id 7)
                                    (replace (item (occurrence 1)) (text "new")))
                               :mode 'apply :source (list :buffer buf :identity 'dorg))
             :ok))
    (should (plist-get
             (dorg-api-execute '(on (id 7)
                                    (replace (drawer-line "LOGBOOK" (all)) "ordinary"))
                               :mode 'apply :source (list :buffer buf :identity 'dorg))
             :ok))))

(ert-deftest dorg-test-7isy6-structured-item-postimages-remain-legal ()
  "The item door accepts declared ordinary keyword and drawer constructors."
  (dolist (form '((on (id 7)
                       (replace (item (occurrence 1)) (keyword "KEY" "v")))
                  (on (id 7)
                      (insert (item) (after-last) (keyword "KEY" "v")))
                  (on (id 7)
                      (insert (item) (after-last)
                              (drawer "NOTE" :lines ("ordinary"))))))
    (dorg-test-mutate--with-content
        buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nold\n"
      (dorg-test-mutate--assert-dorg-policy buf)
      (should (plist-get (dorg-api-execute form :mode 'apply
                                           :source (list :buffer buf :identity 'dorg))
                         :ok)))))

(ert-deftest dorg-test-7isy6-structured-item-postimages-remain-legal-on-file ()
  "The same ordinary structured item forms persist through the public file path."
  (dolist (form '((on (id 7)
                       (replace (item (occurrence 1)) (keyword "KEY" "v")))
                  (on (id 7)
                      (insert (item) (after-last) (keyword "KEY" "v")))
                  (on (id 7)
                      (insert (item) (after-last)
                              (drawer "NOTE" :lines ("ordinary"))))))
    (let ((file (make-temp-file "dorg-7isy6-structured" nil ".org"
                                "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nold\n")))
      (unwind-protect
          (let ((result (dorg-api-execute form :mode 'apply
                                           :source (list :file file :identity 'dorg))))
            (should (plist-get result :ok))
            (should (file-exists-p file)))
        (delete-file file)))))

(ert-deftest dorg-test-7isy6-refuses-item-postimage-heading-and-owned-removal ()
  "Item and drawer-line edits cannot inject headings or remove owned metadata."
  (dolist (form '((on (id 7)
                       (insert (drawer-line "LOGBOOK") (before-first) "* Injected"))
                  (on (id 7)
                      (replace (drawer-line "LOGBOOK" (all)) "** Sub"))
                  (on (id 7) (replace (item (all)) (text "Only.")))))
    (dorg-test-mutate--with-content
        buf (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                    "#+DORG_HASH: keep\nold\n:LOGBOOK:\nentry\n:END:\n")
      (dorg-test-mutate--assert-dorg-policy buf)
      (let ((before (buffer-string))
            (data (dorg-test-mutate--error-data
                   (dorg-api-execute form :mode 'apply
                                     :source (list :buffer buf :identity 'dorg)))))
        (should (eq 'invalid-target (plist-get data :reason)))
        (should (eq 'structural-payload-boundary (plist-get data :detail)))
        (should (equal before (buffer-string)))))))

(ert-deftest dorg-test-7isy6-property-identity-forgery-refuses-at-body-doors ()
  "A source-configured property identity is protected without a literal key list."
  (dolist (form '((on (key "host")
                       (replace (body (literal "old"))
                                ":LOGBOOK:\n:CAPTURE_ID: forged\n:END:"))
                  (on (key "host")
                       (replace (text (occurrence 1))
                                ":LOGBOOK:\n:CAPTURE_ID: forged\n:END:"))
                  (on (key "host")
                       (replace (item (occurrence 1))
                                (text ":LOGBOOK:\n:CAPTURE_ID: forged\n:END:")))
                  (on (key "host")
                      (insert (drawer-line "LOGBOOK") (before-first)
                              ":CAPTURE_ID: forged"))))
    (dorg-test-mutate--with-content
        buf (concat "* Host\n:PROPERTIES:\n:CAPTURE_ID: host\n:END:\n"
                    "old\n:LOGBOOK:\nentry\n:END:\n")
      (let ((before (buffer-string))
            (source (list :buffer buf :identity '(:property "CAPTURE_ID"))))
        (let ((data (dorg-test-mutate--error-data
                     (dorg-api-execute form :mode 'apply :source source))))
          (should (eq 'invalid-target (plist-get data :reason)))
          (should (eq 'structural-payload-boundary (plist-get data :detail)))
          (should (equal before (buffer-string))))))))

(ert-deftest dorg-test-7isy6-protected-keyword-is-case-insensitive ()
  "Lowercase ownership remains protected before Org canonicalizes a payload."
  (should (dorg-protected-keyword-p "dorg_hash")))

(ert-deftest dorg-test-7isy6-dolt-forgery-preserves-head ()
  "Dolt rejects item and drawer-line forgeries in both modes before a commit."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* Host\nold\n:LOGBOOK:\nentry\n:END:\n"))
           (id (dorg-test-mutate--id-by-title dolt-dir "Host"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (head (dorg-head-commit dolt-dir)))
      (dolist (form `((on (id ,id)
                           (replace (item (occurrence 1))
                                    (text "#+DORG_HASH: forged")))
                      (on (id ,id)
                          (replace (drawer-line "LOGBOOK" (all))
                                   ":DORG_CLOCK_START: x"))))
        (dolist (mode '(dry-run apply))
          (let ((data (dorg-test-mutate--error-data
                       (dorg-api-execute form :mode mode :source source))))
            (should (eq 'invalid-target (plist-get data :reason)))
            (should (eq 'structural-payload-boundary (plist-get data :detail)))
            (should (equal head (dorg-head-commit dolt-dir)))))))))

(ert-deftest dorg-test-7isy6-dolt-body-doors-preserve-leading-metadata ()
  "Dolt body doors preserve leading planning and ordinary PROPERTIES metadata.
The reparse guard operates on the body after that fixed leading prefix, while
readback proves the prefix and each edited body constructor persist."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (let ((source (list :dolt-dir dolt-dir :root-id 1000)))
      (dolist (form
               '((on (id 3001)
                     (replace (text (occurrence 1)) "Rewritten by 7isy6."))
                 (on (id 3001)
                     (insert (text) (after-last) "Appended by 7isy6."))
                 (on (id 3001)
                     (insert (item) (after-last) (keyword "KEY" "value")))
                 (on (id 3001)
                     (insert (drawer-line "LOGBOOK") (before-first)
                             "- 7isy6 audit"))))
        (should (plist-get (dorg-api-execute form :mode 'apply :source source)
                           :ok)))
      (let* ((heading (gethash 3001 (dorg-model-load-headings dolt-dir 1000 nil)))
             (texts (dorg-test-api--heading-texts heading)))
        (should (cl-some (lambda (text) (string-match-p "Rewritten by 7isy6" text))
                         texts))
        (should (cl-some (lambda (text) (string-match-p "Appended by 7isy6" text))
                         texts))
        (should (dorg-test-api--heading-scheduled-p heading))
        (should (equal "work" (dorg-test-api--heading-category heading)))
        (should (cl-find-if
                 (lambda (item)
                   (and (cl-typep item 'dorg-keyword-item)
                        (equal "KEY" (dorg-keyword-item-keyword item))
                        (equal "value" (dorg-keyword-item-value item))))
                 (dorg-heading-items heading)))
        (should (member "- 7isy6 audit"
                        (dorg-test-bnsmz--drawer-lines heading "LOGBOOK")))))))

(ert-deftest dorg-test-7isy6-dolt-rich-leading-metadata-forgeries-refuse ()
  "Dolt sees forged metadata even when a rich heading loses its leading prefix."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (let ((source (list :dolt-dir dolt-dir :root-id 1000))
          (head (dorg-head-commit dolt-dir)))
      (dolist (form
               '((on (id 3001)
                     (replace (item (all))
                              (text "#+DORG_HASH: f\n") (text "y\n")))
                 (on (id 3001)
                     (replace (item (all))
                              (drawer "NOTE" :lines (":DORG_ID: 9"))
                              (text "y\n")))
                 (on (id 3001)
                     (insert (item) (before (occurrence 1))
                             (text "#+DORG_HASH: f")))))
        (dorg-test-7isy6--assert-refusal-in-each-mode
         form source head (lambda () (dorg-head-commit dolt-dir)))))))

(ert-deftest dorg-test-7isy6-dolt-item-before-leading-planning-refuses ()
  "Dolt refuses an item insertion that would render before leading planning.
The sequence would put a keyword before SCHEDULED, where Org no longer
recognizes the planning line as heading metadata."
  (skip-unless dorg-test--dolt-available)
  (dorg-test--with-temp-db dolt-dir
    (dorg-test-api--dolt-seed-rich dolt-dir)
    (let ((source (list :dolt-dir dolt-dir :root-id 1000))
          (head (dorg-head-commit dolt-dir)))
      (dorg-test-7isy6--assert-refusal-in-each-mode
       '(on (id 3001)
            (insert (item) (before (occurrence 1)) (keyword "KEY" "v")))
       source head (lambda () (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-7isy6-composed-and-lowercase-drawer-lines-refuse ()
  "Composed item batches and lowercase body properties cannot forge ownership."
  (dolist (case
           '(((batch
               (on (id 7) (insert (item) (after-last) (text ":NOTE:")))
               (on (id 7) (insert (item) (after-last) (text ":DORG_ID: 9")))
               (on (id 7) (insert (item) (after-last) (text ":END:"))))
              dorg)
             ((batch
               (on (id 7) (insert (drawer-line "LOGBOOK") (before-first)
                                  ":DORG_ID: 9"))
               (on (id 7) (replace (text (occurrence 1)) "new")))
              dorg)
             ((on (id 7) (insert (drawer-line "LOGBOOK") (before-first)
                                 ":dorg_id: 9"))
              dorg)))
    (pcase-let ((`(,form ,identity) case))
      (dorg-test-mutate--with-content
          buf (concat "* Host\n:PROPERTIES:\n:"
                      (if (equal identity 'dorg) "DORG_ID: 7" "CAPTURE_ID: host")
                      "\n:END:\nold\n:LOGBOOK:\nentry\n:END:\n")
        (let ((before (buffer-string))
              (source (list :buffer buf :identity identity)))
          (dorg-test-7isy6--assert-refusal-in-each-mode
           form source before (lambda () (buffer-string))))))))

(ert-deftest dorg-test-7isy6-lowercase-property-identity-drawer-line-refuses ()
  "The configured property identity is case-insensitive in a body drawer."
  (dorg-test-mutate--with-content
      buf (concat "* Host\n:PROPERTIES:\n:CAPTURE_ID: host\n:END:\n"
                  "old\n:LOGBOOK:\nentry\n:END:\n")
    (let ((before (buffer-string))
          (source (list :buffer buf :identity '(:property "CAPTURE_ID"))))
      (dorg-test-7isy6--assert-refusal-in-each-mode
       '(on (key "host")
            (insert (drawer-line "LOGBOOK") (before-first) ":capture_id: c2"))
       source before (lambda () (buffer-string))))))

(ert-deftest dorg-test-7isy6-drawer-line-occurrence-and-file-forgery-refuse ()
  "Occurrence and unvisited file drawer-line forgery paths retain exact bytes."
  (let ((content (concat "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\n"
                         "old\n:LOGBOOK:\nentry\n:END:\n")))
    (dorg-test-mutate--with-content buf content
      (dorg-test-7isy6--assert-refusal-in-each-mode
       '(on (id 7)
            (replace (drawer-line "LOGBOOK" (occurrence 1))
                     ":DORG_CLOCK_START: x"))
       (list :buffer buf :identity 'dorg) content (lambda () (buffer-string)))))
  (let ((file (make-temp-file "dorg-7isy6-forgery" nil ".org"
                              "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nold\n")))
    (unwind-protect
        (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (dorg-test-7isy6--assert-refusal-in-each-mode
           '(on (id 7) (replace (item (occurrence 1))
                                 (text "#+DORG_HASH: forged")))
           (list :file file :identity 'dorg) before
           (lambda ()
             (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (delete-file file))))

(ert-deftest dorg-test-7isy6-keyword-replace-and-ordinary-drawer-remain-legal ()
  "A declared ordinary keyword and ordinary drawer content remain editable."
  (dorg-test-mutate--with-content
      buf "* Host\n:PROPERTIES:\n:DORG_ID: 7\n:END:\nold\n:NOTE:\n:OTHER: inside\n:END:\n"
    (let ((source (list :buffer buf :identity 'dorg)))
      (should (plist-get
               (dorg-api-execute '(on (id 7)
                                      (replace (item (occurrence 1))
                                               (keyword "KEY" "v")))
                                 :mode 'apply :source source)
               :ok))
      (should (plist-get
               (dorg-api-execute '(on (id 7)
                                      (replace (drawer-line "NOTE" (occurrence 1))
                                               ":OTHER: changed"))
                                 :mode 'apply :source source)
               :ok))
      (should (string-match-p "#\\+KEY: v" (buffer-string)))
      (should (string-match-p (regexp-quote ":NOTE:\n:OTHER: changed\n:END:")
                              (buffer-string))))))

(ert-deftest dorg-test-7isy6-dolt-heading-forgery-preserves-head ()
  "Dolt rejects heading-shaped drawer lines before dry-run or apply commits."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed
                  dolt-dir "* Host\nold\n:LOGBOOK:\nentry\n:END:\n"))
           (id (dorg-test-mutate--id-by-title dolt-dir "Host"))
           (source (list :dolt-dir dolt-dir :root-id root))
           (head (dorg-head-commit dolt-dir))
           (form `(on (id ,id)
                      (replace (drawer-line "LOGBOOK" (all)) "* Injected"))))
      (dorg-test-7isy6--assert-refusal-in-each-mode
       form source head (lambda () (dorg-head-commit dolt-dir))))))

(ert-deftest dorg-test-q60zv-direct-body-intent-is-closed-and-nonroot ()
  "Direct engine callers get the same literal/non-root boundary as the DSL."
  (let ((good '(:op replace :target (:body :host (:heading :id 7) :literal "old")
                    :value "new")))
    (should (equal (list good) (dorg-mutate--validate-patches (list good))))
    (should (eq 'invalid-target
                (dorg-test-mutate--reason
                 (dorg-mutate--validate-patches
                  '((:op replace :target (:body :host :root :literal "old")
                     :value "new"))))))))

(provide 'dorg-mutate-tests)
;;; dorg-mutate-tests.el ends here

;;; ─── Real-Dolt identity witness for equal-content replace ──────────────

(ert-deftest dorg-test-mutate-dolt-6x6b6-equal-items-replace-keeps-identity-and-commits ()
  "A public whole-items replacement equal to the persisted items passes the
in-transaction partition proof: the request is :ok, every item row keeps its
persisted id and key, content and order are unchanged.  A true no-op is not
required to add a commit.  Before the identity-binding repair this request
aborted with postimage-parity on the core plane (item-row identity expected nil)."
  (dorg-test-mutate--with-dolt
    (let* ((root (dorg-test-mutate--seed dolt-dir "* H\nseed\n"))
           (cid (dorg-test-mutate--id-by-title dolt-dir "H"))
           (src (list :dolt-dir dolt-dir :root-id root))
           (form `(batch (on (id ,cid) (replace (item (all)) (text "A") (text "B")))))
           (first (dorg-api-execute form :mode 'apply :source src))
           (ids-before nil) (keys-before nil) (texts-before nil))
      (should (plist-get first :ok))
      (let ((h (gethash cid (dorg-model-load-tree dolt-dir root))))
        (setq ids-before (mapcar #'dorg-item-id (dorg-heading-items h))
              keys-before (mapcar #'dorg-item-sort-key (dorg-heading-items h))
              texts-before (mapcar #'dorg-text-item-text (dorg-heading-items h))))
      (should (equal texts-before '("A" "B")))
      (should (cl-every #'integerp ids-before))
      (let* ((head-before (dorg-head-commit dolt-dir))
             (again (dorg-api-execute form :mode 'apply :source src)))
        (should (plist-get again :ok))
        (let ((h (gethash cid (dorg-model-load-tree dolt-dir root))))
          (should (equal (mapcar #'dorg-item-id (dorg-heading-items h)) ids-before))
          (should (equal (mapcar #'dorg-item-sort-key (dorg-heading-items h)) keys-before))
          (should (equal (mapcar #'dorg-text-item-text (dorg-heading-items h)) texts-before)))
        (ignore head-before)))))
