;;; dorg-faithful-tests.el --- Downstream tracker-faithful plain-org apply/render SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED SPEC for tracked-regression — tracker-faithful plain-org apply/render:
;; external-key targeting, DORG_ID suppression, and property alignment on a
;; DORG_ID-less (Downstream tracker-shaped) source.  Design v2 (cross-provider approved)
;; settles three decisions this file pins:
;;
;;   B1  `(where QUERY)` keeps its ratified multi-target per-id expansion; a
;;       FAITHFUL binding only changes the IDENTITY PROJECTION of the matched
;;       set (integer id -> extern token from the binding's identity-key).
;;   B2  a faithful apply is SURGICAL (org-entry-put), so every untouched byte
;;       INSIDE the target heading (tag column, keyword case, body blank runs)
;;       and every sibling heading stays byte-identical to an in-place edit.
;;   B3  extern identity round-trips through the dry-run/apply safety envelope
;;       as one canonical transport token.
;;
;; The IMPL does not exist yet, so every assertion is RED until it lands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-mutate nil t)
(require 'dorg-api nil t)
(require 'dorg-app nil t)

;;; ─── Downstream tracker-shaped fixture ───────────────────────────────────────────────
;; Deliberately NONCANONICAL on the TARGET heading (B2): lowercase `#+title:`,
;; author-chosen tag spacing, org-property-format-aligned drawer, a body with a
;; blank-line run, and ZERO :DORG_ID:.  A whole-tree re-render would rewrite all
;; of these; a surgical org-entry-put touches only the STATUS value.

(defconst dorg-test-faithful--fixture
  "#+title: My Tasks
#+CATEGORY: work

* TODO First bead     :work:urgent:
:PROPERTIES:
:BEAD_ID:  bead-001
:STATUS:   OPEN
:END:

Body line one.

Body line two after a blank.
* TODO Second bead   :misc:
:PROPERTIES:
:BEAD_ID:  bead-002
:STATUS:   OPEN
:END:
"
  "An Downstream tracker-shaped plain-org buffer with no :DORG_ID: drawers.")

(defun dorg-test-faithful--tmpfile (contents)
  "Write CONTENTS to a fresh temp .org file and return its path."
  (let ((f (make-temp-file "dorg-faithful-" nil ".org")))
    (with-temp-file f (insert contents))
    f))

(defun dorg-test-faithful--source (file)
  "Explicit faithful plain-org source descriptor for FILE (identity-key BEAD_ID)."
  (list :file file :identity '(:property "BEAD_ID")))

(defun dorg-test-faithful--kill-visiting (file)
  "Kill FILE's visiting buffer when one exists."
  (let ((buf (find-buffer-visiting file)))
    (when buf (kill-buffer buf))))

(defun dorg-test-faithful--org-entry-put-baseline (contents bead-id key value)
  "Return CONTENTS after an in-place `org-entry-put' KEY=VALUE on the heading
whose BEAD_ID property equals BEAD-ID.  This is the byte-identity baseline the
faithful apply must match exactly."
  (with-temp-buffer
    (let ((org-inhibit-startup t)) (insert contents) (org-mode))
    (goto-char (point-min))
    (let (found)
      (org-map-entries
       (lambda ()
         (when (and (not found)
                    (equal bead-id (org-entry-get (point) "BEAD_ID")))
           (setq found t)
           (org-entry-put (point) key value))))
      (unless found (error "baseline: no heading BEAD_ID=%s" bead-id)))
    (buffer-string)))

(defun dorg-test-faithful--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun dorg-test-faithful--external (value &optional kind)
  "Return a typed BEAD_ID external address for VALUE and KIND."
  (dorg-test--external-address (or kind :heading) "BEAD_ID" value))

(defun dorg-test-faithful--put (value key property-value)
  "Return a typed property-put patch for external BEAD_ID VALUE."
  (dorg-test--property-put-patch
   (dorg-test-faithful--external value) key property-value))

(defun dorg-test-faithful--delete-property (value key)
  "Return a typed property-delete patch for external BEAD_ID VALUE."
  (dorg-test--property-delete-patch
   (dorg-test-faithful--external value) key))

(defun dorg-test-faithful--apply (source patches &rest options)
  "Apply typed PATCHES to faithful SOURCE with OPTIONS."
  (apply #'dorg-test--apply-patches source patches options))

;;; ─── A: external-key targeting resolves + applies ────────────────────────

(ert-deftest dorg-test-faithful-extern-target-applies ()
  "A faithful apply addressing a heading by (:extern BEAD_ID V) resolves and
writes, with no DORG_ID injected."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (let ((res (dorg-test-faithful--apply
                    src
                    (list (dorg-test-faithful--put
                           "bead-001" "STATUS" "DONE")))))
          (should (plist-get res :ok))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p "^:STATUS:  +DONE$" out))
            (should-not (string-match-p "DORG_ID" out))))
      (delete-file file))))

;;; ─── B: byte-identity, set an EXISTING property ──────────────────────────

(ert-deftest dorg-test-faithful-byte-identical-set-existing ()
  "Faithful apply of an existing-property change is byte-for-byte identical to
`org-entry-put', and changes the file (non-vacuous)."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file))
         (expected (dorg-test-faithful--org-entry-put-baseline
                    dorg-test-faithful--fixture "bead-001" "STATUS" "DONE")))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src (list (dorg-test-faithful--put
                      "bead-001" "STATUS" "DONE")))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal expected out))
            (should-not (equal dorg-test-faithful--fixture out))))
      (delete-file file))))

;;; ─── C: byte-identity, ADD a new property ────────────────────────────────

(ert-deftest dorg-test-faithful-byte-identical-add-new ()
  "Faithful apply that ADDS a property is byte-identical to `org-entry-put'."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file))
         (expected (dorg-test-faithful--org-entry-put-baseline
                    dorg-test-faithful--fixture "bead-001" "OWNER" "alice")))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src (list (dorg-test-faithful--put
                      "bead-001" "OWNER" "alice")))
          (should (equal expected (dorg-test-faithful--slurp file))))
      (delete-file file))))

;;; ─── D: untouched bytes preserved (target heading + siblings) ────────────

(ert-deftest dorg-test-faithful-preserves-untouched-bytes ()
  "A faithful property edit leaves every byte it did not change intact: the
lowercase `#+title:', the target heading's author tag spacing, its body blank
run, and the entire sibling heading."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src (list (dorg-test-faithful--put
                      "bead-001" "STATUS" "DONE")))
          (let ((out (dorg-test-faithful--slurp file)))
            ;; keyword case preserved (not upcased to #+TITLE:)
            (should (string-match-p "^#\\+title: My Tasks$" out))
            ;; target heading tag column preserved verbatim
            (should (string-match-p "^\\* TODO First bead     :work:urgent:$" out))
            ;; body blank run preserved
            (should (string-match-p "Body line one\\.\n\nBody line two" out))
            ;; sibling heading fully intact
            (should (string-match-p "^\\* TODO Second bead   :misc:$" out))
            (should (string-match-p "^:BEAD_ID:  bead-002$" out))))
      (delete-file file))))

;;; ─── E: deterministic missing / ambiguous ────────────────────────────────

(defun dorg-test-faithful--reason (thunk)
  "Run THUNK; return the `dorg-mutate-error' :reason symbol, or :no-error."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-mutate-error (dorg-mutate-error-reason err))))

(defun dorg-test-faithful--mutate-pair (thunk)
  "Run THUNK and return its mutate (REASON . DETAIL), or `:no-error'."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-mutate-error
     (cons (dorg-mutate-error-reason err)
           (plist-get (dorg-mutate-error-data err) :detail)))))

(defun dorg-test-faithful--api-reason (thunk)
  "Run THUNK; return the `dorg-api-error' :reason symbol, or :no-error."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-api-error (dorg-api-error-reason err))))

(ert-deftest dorg-test-faithful-extern-missing-is-invalid-target ()
  "An extern key with no matching heading is a deterministic invalid-target."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (should (eq 'invalid-target
                    (dorg-test-faithful--reason
                     (lambda ()
                       (dorg-test-faithful--apply
                        src (list (dorg-test-faithful--put
                                   "nope" "STATUS" "DONE")))))))
      (delete-file file))))

(ert-deftest dorg-test-faithful-extern-ambiguous-is-invalid-target ()
  "A duplicated extern key is a deterministic invalid-target (ambiguous), not a
silent first-match."
  (let* ((dupe (concat dorg-test-faithful--fixture
                       "* TODO Dupe bead\n:PROPERTIES:\n:BEAD_ID:  bead-001\n:END:\n"))
         (file (dorg-test-faithful--tmpfile dupe))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (should (eq 'invalid-target
                    (dorg-test-faithful--reason
                     (lambda ()
                       (dorg-test-faithful--apply
                        src (list (dorg-test-faithful--put
                                   "bead-001" "STATUS" "DONE")))))))
      (delete-file file))))

;;; ─── B2 out-of-scope: structural ops fail BEFORE any write ───────────────

(ert-deftest dorg-test-faithful-structural-op-unsupported-no-write ()
  "A structural op (delete-heading) on a faithful binding signals `unsupported'
and leaves the source byte-for-byte unchanged (no partial write/render)."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should (eq 'unsupported
                      (dorg-test-faithful--reason
                       (lambda ()
                         (dorg-test-faithful--apply
                          src
                          (list
                           (list :op 'delete
                                 :target
                                 (dorg-test-faithful--external
                                  "bead-001" :heading))))))))
          ;; unchanged
          (should (equal dorg-test-faithful--fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;;; ─── B1: identity projection is SOURCE-SPECIFIC, where stays multi-target ─

(ert-deftest dorg-test-faithful-where-projects-to-extern-multi ()
  "On a faithful source, `(where (property STATUS OPEN))' still expands to ALL
matches (multi-target), each projected to an extern host keyed by the binding's
identity-key (BEAD_ID), NOT collapsed to a single lookup."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id nil :properties (("BEAD_ID" . "bead-001")
                                               ("STATUS" . "OPEN")))
                 (:heading-id nil :properties (("BEAD_ID" . "bead-002")
                                               ("STATUS" . "OPEN")))))))
    (let ((ps (dorg-api-normalize
               '(on (where (property "STATUS" "OPEN")) (set (property "STATUS") "DONE"))
               (dorg-test-faithful--source "/does-not-matter.org"))))
      (should (= 2 (length ps)))
      (should (equal '(:property :host (:heading :extern t :key "BEAD_ID" :value "bead-001")
                       :name "STATUS")
                     (plist-get (nth 0 ps) :target)))
      (should (equal '(:property :host (:heading :extern t :key "BEAD_ID" :value "bead-002")
                       :name "STATUS")
                     (plist-get (nth 1 ps) :target))))))

(ert-deftest dorg-test-faithful-key-projects-extern ()
  "AC-4/AC-5 (tracked-case): (on (key V) ...) on a faithful source
rewrites (key V) to the declared identity-key via the shared seam, and a
single hit projects to the extern host."
  (let ((seen nil))
    (cl-letf (((symbol-function 'dorg-ql-select)
               (lambda (q &rest _)
                 (setq seen q)
                 '((:heading-id nil
                    :properties (("BEAD_ID" . "bead-001") ("STATUS" . "OPEN")))))))
      (let ((ps (dorg-api-normalize
                 '(on (key "bead-001") (set (property "STATUS") "DONE"))
                 (dorg-test-faithful--source "/does-not-matter.org"))))
        ;; the shared seam rewrote (key V) -> (property BEAD_ID V)
        (should (equal '(property "BEAD_ID" "bead-001") seen))
        (should (= 1 (length ps)))
        (should (equal '(:heading :extern t :key "BEAD_ID" :value "bead-001")
                       (dorg-api--tget (plist-get (car ps) :target) :host)))))))

(ert-deftest dorg-test-faithful-key-zero-is-invalid-target ()
  "AC-5: (key V) matching no heading is a hard invalid-target."
  (cl-letf (((symbol-function 'dorg-ql-select) (lambda (&rest _) nil)))
    (should (eq 'invalid-target
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize
                    '(on (key "nope") (set (property "S") "X"))
                    (dorg-test-faithful--source "/x.org"))))))))

(ert-deftest dorg-test-faithful-key-multi-is-identity-violation ()
  "AC-5: (key V) matching >1 heading is identity-violation (store corruption),
distinct from a cardinality dorg-ql-error and from target-set-changed."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id nil :properties (("BEAD_ID" . "dup")))
                 (:heading-id nil :properties (("BEAD_ID" . "dup")))))))
    (should (eq 'identity-violation
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize
                    '(on (key "dup") (set (property "S") "X"))
                    (dorg-test-faithful--source "/x.org"))))))))

(ert-deftest dorg-test-faithful-identity-key-write-protected ()
  "AC-7 (tracked-case): (set/delete property IDKEY) on a binding declaring IDKEY
is invalid-target — the declared identity-key is engine-load-bearing.  A
non-identity property on the same binding still normalizes (non-vacuity: the
guard is targeted, not a blanket refusal)."
  (let ((src (dorg-test-faithful--source "/x.org")))
    (should (eq 'invalid-target
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize '(on document (set (property "BEAD_ID") "x")) src)))))
    (should (eq 'invalid-target
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize '(on document (delete (property "BEAD_ID"))) src)))))
    (should (eq :no-error
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize '(on document (set (property "STATUS") "DONE")) src)))))))

(ert-deftest dorg-test-faithful-where-idbearing-unchanged ()
  "B1 regression: on a NON-faithful (id-bearing) source, `(where (property …))'
still expands to per-id patches exactly as the ratified contract requires."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _) '((:heading-id 101) (:heading-id 102)))))
    (let ((ps (dorg-api-normalize
               '(on (where (property "STATUS" "OPEN")) (set (property "STATUS") "DONE"))
               '(:file "/x.org"))))
      (should (= 2 (length ps)))
      (should (equal '(:property :host (:heading :id 101) :name "STATUS")
                     (plist-get (nth 0 ps) :target)))
      (should (equal '(:property :host (:heading :id 102) :name "STATUS")
                     (plist-get (nth 1 ps) :target))))))

(ert-deftest dorg-test-51gsn-where-keyless-id-projects-numeric ()
  "A descriptive where hit lacking K projects by its existing numeric ID."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id 101 :properties (("STATUS" . "OPEN")))))))
    (let ((patches
           (dorg-api-normalize
            '(on (where (property "STATUS" "OPEN"))
                 (set (property "STATUS") "DONE"))
            (dorg-test-faithful--source "/x.org"))))
      (should (= 1 (length patches)))
      (should
       (equal '(:property :host (:heading :id 101) :name "STATUS")
              (plist-get (car patches) :target))))))

(ert-deftest dorg-test-51gsn-where-keyless-idless-still-errors ()
  "A descriptive hit with neither K nor an ID remains a hard invalid-target."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id nil :properties (("STATUS" . "OPEN")))))))
    (should (eq 'invalid-target
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize
                    '(on (where (property "STATUS" "OPEN"))
                         (set (property "STATUS") "DONE"))
                    (dorg-test-faithful--source "/x.org"))))))))

(ert-deftest dorg-test-51gsn-where-malformed-present-key-never-falls-back ()
  "A present non-string K refuses even when the hit carries a valid ID."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id 101
                  :properties (("BEAD_ID" . 42) ("STATUS" . "OPEN")))))))
    (should (eq 'invalid-target
                (dorg-test-faithful--api-reason
                 (lambda ()
                   (dorg-api-normalize
                    '(on (where (property "STATUS" "OPEN"))
                         (set (property "STATUS") "DONE"))
                    (dorg-test-faithful--source "/x.org"))))))))

(ert-deftest dorg-test-51gsn-exact-key-path-never-falls-back-to-id ()
  "Direct/key-wrapper selectors refuse a keyless hit carrying a numeric ID."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id 101 :properties (("STATUS" . "OPEN")))))))
    (dolist (target '((key "bead-001") (where (key "bead-001"))))
      (should
       (eq 'invalid-target
           (dorg-test-faithful--api-reason
            (lambda ()
              (dorg-api-normalize
               `(on ,target (set (property "STATUS") "DONE"))
               (dorg-test-faithful--source "/x.org")))))))))

(ert-deftest dorg-test-51gsn-mixed-target-dry-run-apply-roundtrip ()
  "Mixed extern+ID query targets survive the expected-target safety envelope."
  (let ((applied 0)
        (hits '((:heading-id 202
                 :properties (("BEAD_ID" . "bead-002") ("STATUS" . "OPEN")))
                (:heading-id 101 :properties (("STATUS" . "OPEN"))))))
    (cl-letf (((symbol-function 'dorg-ql-select) (lambda (&rest _) hits))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 '(:ok t :dry-run t :source-kind plain-org :root-id 1
                   :base-revision "t" :supported t :patches nil)))
              ((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (setq applied (1+ applied))
                 '(:ok t :dry-run nil :source-kind plain-org :root-id 1
                   :revision "t2" :created nil :changed (101 202) :deleted nil
                   :patches nil))))
      (let* ((src (dorg-test-faithful--source "/x.org"))
             (form '(on (where (property "STATUS" "OPEN"))
                        (set (property "STATUS") "DONE")))
             (dry (dorg-api-execute form :mode 'dry-run :source src))
             (targets (plist-get dry :targets)))
        (should
         (equal '(101 (:extern :key "BEAD_ID" :value "bead-002")) targets))
        (should (equal targets
                       (mapcar (lambda (token)
                                 (dorg-api-parse-target-token
                                  (dorg-api-target-token token)))
                               targets)))
        (dorg-api-execute form :mode 'apply :source src
                          :expected-targets targets)
        (should (= 1 applied))
        (should
         (eq 'target-set-changed
             (dorg-test-faithful--api-reason
              (lambda ()
                (dorg-api-execute form :mode 'apply :source src
                                  :expected-targets '(101))))))
        (should (= 1 applied))))))

(ert-deftest dorg-test-51gsn-identity-write-guard-covers-id-projected-hit ()
  "The declared K stays write-protected after descriptive ID projection."
  (cl-letf (((symbol-function 'dorg-ql-select)
             (lambda (&rest _)
               '((:heading-id 101 :properties (("STATUS" . "OPEN")))))))
    (dolist (action '((set (property "BEAD_ID") "new")
                      (delete (property "BEAD_ID"))))
      (let ((err
             (should-error
              (dorg-api-normalize
               `(on (where (property "STATUS" "OPEN")) ,action)
               (dorg-test-faithful--source "/x.org"))
              :type 'dorg-api-error)))
        (should (eq 'invalid-target (dorg-api-error-reason err)))
        (should (string-match-p "write-protected"
                                (error-message-string err)))))
    (let ((patch
           (car
            (dorg-api-normalize
             '(on (where (property "STATUS" "OPEN"))
                  (set (property "OWNER") "alice"))
             (dorg-test-faithful--source "/x.org")))))
      (should
       (equal '(:property :host (:heading :id 101) :name "OWNER")
              (plist-get patch :target))))))

(ert-deftest dorg-test-51gsn-plain-org-idbearing-where-transforms ()
  "Public apply mutates a keyless ID-bearing plain heading without provisioning."
  (let* ((before
          "* Notes\n:PROPERTIES:\n:DORG_ID: 101\n:STATUS: OPEN\n:END:\n")
         (file (dorg-test-faithful--tmpfile before))
         (src (dorg-test-faithful--source file))
         (form '(on (where (property "STATUS" "OPEN"))
                    (set (property "STATUS") "DONE"))))
    (unwind-protect
        (let* ((dry (dorg-api-execute form :mode 'dry-run :source src))
               (targets (plist-get dry :targets)))
          (should (equal '(101) targets))
          (dorg-api-execute form :mode 'apply :source src
                            :expected-targets targets :save t)
          (let ((after (dorg-test-faithful--slurp file)))
            (should-not (equal before after))
            (should (string-match-p "^:DORG_ID: +101$" after))
            (should (string-match-p "^:STATUS: +DONE$" after))
            (should-not (string-match-p "BEAD_ID" after))))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

;;; ─── B3: canonical transport token round-trip ────────────────────────────

(ert-deftest dorg-test-faithful-token-roundtrip ()
  "The extern identity token serializes to a transport-safe wire string and
parses back, including reserved characters."
  (let ((tok '(:extern :key "BEAD_ID" :value "b,1:2=3%x")))
    (let ((wire (dorg-api-target-token tok)))
      (should (stringp wire))
      (should (string-prefix-p "key:" wire))
      ;; reserved chars are percent-encoded, so a comma-split is safe
      (should-not (string-match-p ",[^%]" (substring wire 4)))
      (should (equal tok (dorg-api-parse-target-token wire))))))

(ert-deftest dorg-test-faithful-token-integer-id-passthrough ()
  "An integer id target canonicalizes to its decimal wire string and back."
  (should (equal "101" (dorg-api-target-token 101)))
  (should (equal 101 (dorg-api-parse-target-token "101"))))

(ert-deftest dorg-test-faithful-cli-parse-expect-targets-mixed ()
  "`dorg-app--parse-expect-targets' accepts a MIXED comma list of integer ids
and extern tokens."
  (let ((parsed (dorg-app--parse-expect-targets "101,key:BEAD_ID=bead-002")))
    (should (equal 101 (nth 0 parsed)))
    (should (equal '(:extern :key "BEAD_ID" :value "bead-002") (nth 1 parsed)))))

(ert-deftest dorg-test-faithful-dry-run-apply-roundtrip-extern ()
  "A query-scoped dry-run on a faithful DORG_ID-less source returns extern
tokens in :targets; feeding the identical set back as :expected-targets passes
the safety gate and reaches apply (B3 handshake)."
  (let ((reached nil))
    (cl-letf (((symbol-function 'dorg-ql-select)
               (lambda (&rest _)
                 '((:heading-id nil :properties (("BEAD_ID" . "bead-001")
                                                 ("STATUS" . "OPEN"))))))
              ((symbol-function 'dorg-mutate--plan-patches)
               (lambda (&rest _)
                 '(:ok t :dry-run t :source-kind plain-org :root-id 1
                   :base-revision "t" :supported t :patches nil)))
              ((symbol-function 'dorg-mutate--apply-patches)
               (lambda (&rest _)
                 (setq reached t)
                 '(:ok t :dry-run nil :source-kind plain-org :root-id 1
                   :revision "t2" :created nil :changed nil :deleted nil
                   :patches nil))))
      (let* ((src (dorg-test-faithful--source "/x.org"))
             (plan (dorg-api-execute
                    '(on (where (property "BEAD_ID" "bead-001"))
                         (set (property "STATUS") "DONE"))
                    :mode 'dry-run :source src))
             (targets (plist-get plan :targets)))
        (should (member '(:extern :key "BEAD_ID" :value "bead-001") targets))
        (dorg-api-execute
         '(on (where (property "BEAD_ID" "bead-001")) (set (property "STATUS") "DONE"))
         :mode 'apply :expected-targets targets :source src)
        (should reached)))))

;;; ─── F: faithful surgical keyword/body/item ops (tracked-regression) ───────────
;;
;; RED SPEC for the item-span-splice deliverable.  A FAITHFUL binding owns
;; identity in the file (no :DORG_ID:), so a keyword/body/item op must edit
;; ONLY the bytes it changes: the changed item is re-rendered, but every
;; SIBLING item keeps its original author formatting verbatim (e.g. the
;; two-space `:BEAD_ID:  a-1' and `#+FOO:  second'), the target heading line
;; and every other heading stay byte-identical, and NO :DORG_ID: / synthetic
;; PROPERTIES drawer is ever injected.  These ops are `unsupported' on the
;; faithful path until tracked-case lands, so every assertion is RED for now.

(defconst dorg-test-faithful--item-fixture
  "#+title: my tasks

* TODO Alpha  :work:
:PROPERTIES:
:BEAD_ID:  a-1
:END:
#+FOO:  first
#+FOO:  second
Alpha body one.

Alpha body two.
* TODO Beta  :misc:
:PROPERTIES:
:BEAD_ID:  b-2
:END:
Beta body.
"
  "A faithful (DORG_ID-less) fixture whose Alpha heading carries a user
PROPERTIES drawer, two same-key keyword items with author two-space spacing,
and a two-paragraph body with a blank run — all sibling bytes a surgical op
must preserve.  Beta is the untouched sibling heading.")

(defconst dorg-test-faithful--alpha-host
  '(:heading :extern t :key "BEAD_ID" :value "a-1")
  "Extern-key host addressing the Alpha heading.")

(defun dorg-test-faithful--beta-verbatim-p (out)
  "Return non-nil when the Beta sibling heading is byte-identical in OUT."
  (string-match-p
   (regexp-quote "* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:END:\nBeta body.\n")
   out))

;; F1 — set-keyword replaces one occurrence; the OTHER occurrence's author
;; two-space spacing survives verbatim (only the changed line is re-rendered).
(ert-deftest dorg-test-faithful-set-keyword-replaces-occurrence ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should
           (plist-get
            (dorg-test-faithful--apply
             src
             (list (dorg-test--keyword-occurrence-patch
                    'replace dorg-test-faithful--alpha-host "FOO" 1
                    :value "changed" :missing :error)))
            :ok))
          (let ((out (dorg-test-faithful--slurp file)))
            ;; changed occurrence re-rendered; sibling occurrence verbatim
            (should (string-match-p "#\\+FOO: changed\n#\\+FOO:  second\n" out))
            (should-not (string-match-p "first" out))
            ;; identity drawer + body + preamble + sibling heading untouched
            (should (string-match-p "^:BEAD_ID:  a-1$" out))
            (should (string-match-p "Alpha body one\\.\n\nAlpha body two\\." out))
            (should (string-match-p "^#\\+title: my tasks$" out))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (string-match-p "DORG_ID" out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F2 — set-keyword with no matching key APPENDS a new keyword line at the end
;; of the section's items region; all prior items stay verbatim.
(ert-deftest dorg-test-faithful-set-keyword-appends-when-absent ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (dorg-test--keyword-patch
                  'insert dorg-test-faithful--alpha-host "BAR"
                  :position :after-last :value "new")))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p "Alpha body two\\.\n#\\+BAR: new\n\\* TODO Beta" out))
            (should (string-match-p "#\\+FOO:  first\n#\\+FOO:  second\n" out))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F3 — delete-keyword by ordinal removes exactly that occurrence; the sibling
;; occurrence keeps its author spacing; missing ordinal is invalid-target.
(ert-deftest dorg-test-faithful-delete-keyword-ordinal ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (dorg-test--keyword-occurrence-patch
                  'delete dorg-test-faithful--alpha-host "FOO" 1
                  :missing :error)))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p ":END:\n#\\+FOO:  second\nAlpha body one\\." out))
            ;; exactly one #+FOO line remains
            (should (= 1 (length (seq-filter
                                  (lambda (l) (string-prefix-p "#+FOO" l))
                                  (split-string out "\n")))))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F3b — delete-keyword with a missing ordinal is invalid-target, no write.
(ert-deftest dorg-test-faithful-delete-keyword-missing-no-write ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should (eq 'invalid-target
                      (dorg-test-faithful--reason
                       (lambda ()
                         (dorg-test-faithful--apply
                          src
                          (list (dorg-test--keyword-occurrence-patch
                                 'delete dorg-test-faithful--alpha-host
                                 "FOO" 6 :missing :error)))))))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F4 — delete-keyword :all removes every occurrence; non-matching items stay.
(ert-deftest dorg-test-faithful-delete-keyword-all ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (dorg-test--keyword-patch
                  'delete dorg-test-faithful--alpha-host "FOO")))
          (let ((out (dorg-test-faithful--slurp file)))
            (should-not (string-match-p "#\\+FOO" out))
            (should (string-match-p ":END:\nAlpha body one\\." out))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F5 — target-relative text insertion adds after the last prose item.
(ert-deftest dorg-test-faithful-insert-text-after-last ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (list :op 'insert
                       :target (list :texts :host dorg-test-faithful--alpha-host)
                       :position :after-last
                       :value "Appended para.")))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p
                     "Alpha body two\\.\n\nAppended para\\.\n\\* TODO Beta" out))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F6 — singular text replacement rewrites only occurrence one.
(ert-deftest dorg-test-faithful-replace-text-occurrence ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (list :op 'replace
                       :target (list :text :host dorg-test-faithful--alpha-host
                                     :occurrence 1)
                       :missing :error
                       :value "First body, rewritten.")))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p "First body, rewritten\\." out))
            (should-not (string-match-p "Alpha body one\\." out))
            ;; second text item + keyword siblings verbatim
            (should (string-match-p "Alpha body two\\." out))
            (should (string-match-p "#\\+FOO:  first\n#\\+FOO:  second\n" out))
            (should (dorg-test-faithful--beta-verbatim-p out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F8 — replace-items replaces the WHOLE BODY region with an ordered mix
;; (text + keyword).  Under A8 the caller payload OMITS the identity drawer
;; (submitting it would smuggle/duplicate identity, F9c); the authoritative
;; BEAD_ID drawer is preserved verbatim OUT-OF-BAND, no :DORG_ID: injected, and
;; the heading line + Beta sibling stay byte-identical.  Full-string
;; transformation assertion.
(ert-deftest dorg-test-faithful-replace-items-ordered-sequence ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (expected
          (concat
           "#+title: my tasks\n"
           "\n"
           "* TODO Alpha  :work:\n"
           ":PROPERTIES:\n"
           ":BEAD_ID:  a-1\n"
           ":END:\n"
           "New body.\n"
           "#+FOO: solo\n"
           "* TODO Beta  :misc:\n"
           ":PROPERTIES:\n"
           ":BEAD_ID:  b-2\n"
           ":END:\n"
           "Beta body.\n")))
    (unwind-protect
        (progn
          (dorg-test-faithful--apply
           src
           (list (dorg-test--items-replace-patch
                  dorg-test-faithful--alpha-host
                  '((:kind text :text "New body.")
                    (:kind keyword :keyword "FOO" :value "solo")))))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal expected out))
            (should-not (string-match-p "DORG_ID" out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; ── Identity immutability invariant (A8, ratified) ────────────────────────
;; A faithful binding's configured external identity key (here BEAD_ID) lives in
;; the PROPERTIES drawer, which the executor preserves verbatim OUT-OF-BAND
;; across body/item edits.  A caller payload therefore must NOT carry the key at
;; all: OMISSION is SAFE (the drawer is preserved), while any payload occurrence
;; — a CHANGE, a DUPLICATION, or an item-addressed DELETE of the drawer — is
;; refused BEFORE any write, leaving the source byte-identical.  These supersede
;; the earlier "payload must retain the key" doctrine (A8).

;; F9a — OMISSION is SAFE: replace-items whose body payload omits the identity
;; drawer succeeds; the BEAD_ID drawer is preserved verbatim out-of-band.
(ert-deftest dorg-test-faithful-replace-items-omit-identity-preserves-drawer ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (expected
          (concat
           "#+title: my tasks\n" "\n"
           "* TODO Alpha  :work:\n" ":PROPERTIES:\n" ":BEAD_ID:  a-1\n" ":END:\n"
           "New body.\n" "#+FOO: solo\n"
           "* TODO Beta  :misc:\n" ":PROPERTIES:\n" ":BEAD_ID:  b-2\n" ":END:\n"
           "Beta body.\n")))
    (unwind-protect
        (progn
          (should
           (plist-get
            (dorg-test-faithful--apply
             src
             (list (dorg-test--items-replace-patch
                    dorg-test-faithful--alpha-host
                    '((:kind text :text "New body.")
                      (:kind keyword :keyword "FOO" :value "solo")))))
            :ok))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal expected out))
            ;; the drawer survived verbatim out-of-band, exactly once, with its
            ;; author two-space spacing intact (source preservation, not re-render)
            (should (= 1 (cl-count ":BEAD_ID:  a-1" (split-string out "\n")
                                   :test #'equal)))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F9b — CHANGE: a payload drawer that alters the identity value is a direct
;; identity mutation smuggled through the body -> invalid-target/identity-brick,
;; no write.
(ert-deftest dorg-test-faithful-replace-items-change-identity-is-invalid ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should (equal '(invalid-target . identity-brick)
                      (dorg-test-faithful--mutate-pair
                       (lambda ()
                         (dorg-test-faithful--apply
                          src
                          (list
                           (dorg-test--items-replace-patch
                            dorg-test-faithful--alpha-host
                            '((:kind drawer :name "PROPERTIES"
                               :properties (("BEAD_ID" . "MUTATED")))
                              (:kind text :text "New body.")))))))))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F9c — DUPLICATION: a payload with the identity key present twice smuggles a
;; duplicate identity -> invalid-target/identity-brick, no write.
(ert-deftest dorg-test-faithful-replace-items-duplicate-identity-is-invalid ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should (equal '(invalid-target . identity-brick)
                      (dorg-test-faithful--mutate-pair
                       (lambda ()
                         (dorg-test-faithful--apply
                          src
                          (list
                           (dorg-test--items-replace-patch
                            dorg-test-faithful--alpha-host
                            '((:kind drawer :name "PROPERTIES"
                               :properties (("BEAD_ID" . "a-1")))
                              (:kind drawer :name "PROPERTIES"
                               :properties (("BEAD_ID" . "a-1")))
                              (:kind text :text "New body.")))))))))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F9e — EMPTY is a positive BODY-CLEAR: an empty replace-items clears the
;; heading's BODY items while the out-of-band BEAD_ID drawer is preserved
;; verbatim (identity is never in the body, so clearing it cannot brick).
(ert-deftest dorg-test-faithful-replace-items-empty-clears-body-preserves-drawer ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (expected
          (concat
           "#+title: my tasks\n" "\n"
           "* TODO Alpha  :work:\n" ":PROPERTIES:\n" ":BEAD_ID:  a-1\n" ":END:\n"
           "* TODO Beta  :misc:\n" ":PROPERTIES:\n" ":BEAD_ID:  b-2\n" ":END:\n"
           "Beta body.\n")))
    (unwind-protect
        (progn
          (should
           (plist-get
            (dorg-test-faithful--apply
             src
             (list (dorg-test--items-replace-patch
                    dorg-test-faithful--alpha-host nil)))
            :ok))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal expected out))
            ;; the drawer (verbatim two-space spacing) survived; body is gone
            (should (string-match-p ":BEAD_ID:  a-1" out))
            (should-not (string-match-p "Alpha body" out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (delete-file file))))

;; F10 — atomicity: a batch mixing a supported op with a missing-target op
;; leaves the source byte-identical (snapshot restore) — no partial write.
(ert-deftest dorg-test-faithful-item-batch-atomic-on-missing ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should (eq 'invalid-target
                      (dorg-test-faithful--reason
                       (lambda ()
                         (dorg-test-faithful--apply
                          src
                          (list
                           (dorg-test--keyword-occurrence-patch
                            'replace dorg-test-faithful--alpha-host "FOO" 1
                            :value "changed" :missing :error)
                           (dorg-test--keyword-occurrence-patch
                            'replace (dorg-test-faithful--external "nope")
                            "FOO" 1 :value "x" :missing :error)))))))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F12 — END-TO-END (the authoritative acceptance): the public ordered
;; item-sequence verb driven through `dorg-api-execute' (apply) onto a FAITHFUL
;; binding replaces the heading's items byte-faithfully — no :DORG_ID: or
;; synthetic PROPERTIES injected, heading line + sibling heading verbatim.
;; Direct `(key …)' is bounded, so apply needs no expected-targets
;; handshake; `dorg-ql-select' is mocked only to PROJECT the host, while the
;; real write re-resolves the extern key against the file's own parse.
(ert-deftest dorg-test-faithful-e2e-public-replace-items ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (expected
          (concat
           "#+title: my tasks\n"
           "\n"
           "* TODO Alpha  :work:\n"
           ":PROPERTIES:\n"
           ":BEAD_ID:  a-1\n"
           ":END:\n"
           "New body.\n"
           "#+FOO: solo\n"
           "* TODO Beta  :misc:\n"
           ":PROPERTIES:\n"
           ":BEAD_ID:  b-2\n"
           ":END:\n"
           "Beta body.\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-ql-select)
                   (lambda (&rest _)
                     '((:heading-id nil :properties (("BEAD_ID" . "a-1")))))))
          (let ((res (dorg-api-execute
                      ;; A8: the payload OMITS the identity drawer (submitting it
                      ;; would smuggle identity); the BEAD_ID drawer is preserved
                      ;; verbatim out-of-band.
                      '(on (key "a-1")
                           (replace (items)
                                    (text "New body.")
                                    (keyword "FOO" "solo")))
                      :mode 'apply :source src)))
            (should (plist-get res :ok))
            (let ((out (dorg-test-faithful--slurp file)))
              (should (equal expected out))
              (should-not (string-match-p "DORG_ID" out))
              (should-not (equal dorg-test-faithful--item-fixture out)))))
      (delete-file file))))

;; F13 — END-TO-END A8 preservation: the public verb through `dorg-api-execute'
;; (apply) with a body payload that OMITS the identity key SUCCEEDS — the BEAD_ID
;; drawer is preserved verbatim out-of-band (omission is safe, not a brick).
(defun dorg-test-faithful--any-reason (thunk)
  "Run THUNK; return the mutate/api error :reason, or :no-error."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-mutate-error (dorg-mutate-error-reason err))
    (dorg-api-error (dorg-api-error-reason err))))

(ert-deftest dorg-test-faithful-e2e-public-omit-identity-preserves-drawer ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (cl-letf (((symbol-function 'dorg-ql-select)
                   (lambda (&rest _)
                     '((:heading-id nil :properties (("BEAD_ID" . "a-1")))))))
          (should (plist-get
                   (dorg-api-execute
                    '(on (key "a-1")
                         (replace (items)
                                  (text "New body.")
                                  (keyword "FOO" "solo")))
                    :mode 'apply :source src)
                   :ok))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal dorg-test-faithful--public-replace-alpha-expected out))
            ;; identity drawer preserved verbatim (two-space) out-of-band
            (should (= 1 (cl-count ":BEAD_ID:  a-1" (split-string out "\n")
                                   :test #'equal)))
            (should-not (string-match-p "DORG_ID" out))))
      (delete-file file))))

;;; ─── F13b: real file-source query expansion (tracked-regression) ─────────────

(defconst dorg-test-faithful--public-replace-alpha-expected
  (concat
   "#+title: my tasks\n"
   "\n"
   "* TODO Alpha  :work:\n"
   ":PROPERTIES:\n"
   ":BEAD_ID:  a-1\n"
   ":END:\n"
   "New body.\n"
   "#+FOO: solo\n"
   "* TODO Beta  :misc:\n"
   ":PROPERTIES:\n"
   ":BEAD_ID:  b-2\n"
   ":END:\n"
   "Beta body.\n")
  "Expected bytes after the public faithful Alpha item replacement.")

(defun dorg-test-faithful--public-replace-alpha-form ()
  "Return the public API form that replaces Alpha's BODY item sequence.
A8: the payload OMITS the identity drawer (submitting it would smuggle
identity); the BEAD_ID drawer is preserved verbatim out-of-band."
  '(on (key "a-1")
       (replace (items)
                (text "New body.")
                (keyword "FOO" "solo"))))

(ert-deftest dorg-test-faithful-api-file-source-no-visiting-buffer ()
  "A faithful :file source with no visiting buffer resolves query targets itself.

This is the tracked-case regression: callers must not create a same-named buffer
shim before `(key ...)' can run."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--kill-visiting file)
          (should-not (find-buffer-visiting file))
          (let ((res (dorg-api-execute
                      (dorg-test-faithful--public-replace-alpha-form)
                      :mode 'apply :source src :save t)))
            (should (plist-get res :ok))
            (should (member '(:extern :key "BEAD_ID" :value "a-1")
                            (plist-get res :targets)))
            (should-not (find-buffer-visiting file)))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (equal dorg-test-faithful--public-replace-alpha-expected out))
            (should-not (string-match-p "DORG_ID" out))
            (should-not (equal dorg-test-faithful--item-fixture out))))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

(ert-deftest dorg-test-faithful-api-file-source-already-visiting-buffer ()
  "A faithful :file source also works when its file is already visiting a buffer."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer buf (org-mode))
          (should (eq buf (find-buffer-visiting file)))
          (let ((res (dorg-api-execute
                      (dorg-test-faithful--public-replace-alpha-form)
                      :mode 'apply :source src :save t)))
            (should (plist-get res :ok))
            (should (member '(:extern :key "BEAD_ID" :value "a-1")
                            (plist-get res :targets)))
            (should (buffer-live-p buf))
            (should (eq buf (find-buffer-visiting file))))
          (should (equal dorg-test-faithful--public-replace-alpha-expected
                         (dorg-test-faithful--slurp file)))
          (should (buffer-live-p buf))
          (should (eq buf (find-buffer-visiting file))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

(ert-deftest dorg-test-faithful-api-file-source-ignores-same-named-buffer ()
  "A same-named non-file buffer is not an authoritative query source."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (shim (get-buffer-create file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--kill-visiting file)
          (with-current-buffer shim
            (erase-buffer)
            (insert "* Wrong in-memory document\n:PROPERTIES:\n:BEAD_ID: shim\n:END:\n")
            (org-mode))
          (should-not (buffer-file-name shim))
          (let ((res (dorg-api-execute
                      (dorg-test-faithful--public-replace-alpha-form)
                      :mode 'apply :source src :save t)))
            (should (plist-get res :ok))
            (should (member '(:extern :key "BEAD_ID" :value "a-1")
                            (plist-get res :targets))))
          (should-not (find-buffer-visiting file))
          (should (equal dorg-test-faithful--public-replace-alpha-expected
                         (dorg-test-faithful--slurp file))))
      (when (buffer-live-p shim) (kill-buffer shim))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

(ert-deftest dorg-test-faithful-api-file-source-query-error-kills-owned-buffer ()
  "A failing no-buffer file query must not leave a visiting buffer behind."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (dorg-test-faithful--kill-visiting file)
          (should-not (find-buffer-visiting file))
          (should-error
           (dorg-api-execute
            '(on (where (property "STATUS"))
                 (set (property "STATUS") "DONE"))
            :mode 'apply :source src :save t)
           :type 'dorg-ql-error)
          (should-not (find-buffer-visiting file))
          (should (equal dorg-test-faithful--fixture
                         (dorg-test-faithful--slurp file))))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

(ert-deftest dorg-test-faithful-api-file-source-query-scoped-apply ()
  "Unbounded query-scoped apply on a faithful :file source uses extern targets."
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--fixture))
         (src (dorg-test-faithful--source file))
         (expected-targets '((:extern :key "BEAD_ID" :value "bead-001")
                             (:extern :key "BEAD_ID" :value "bead-002"))))
    (unwind-protect
        (progn
          (dorg-test-faithful--kill-visiting file)
          (should (eq 'expected-targets-required
                      (dorg-test-faithful--any-reason
                       (lambda ()
                         (dorg-api-execute
                          '(on (where (property "STATUS" "OPEN"))
                               (set (property "STATUS") "DONE"))
                          :mode 'apply :source src)))))
          (should-not (find-buffer-visiting file))
          (let ((res (dorg-api-execute
                      '(on (where (property "STATUS" "OPEN"))
                           (set (property "STATUS") "DONE"))
                      :mode 'apply
                      :expected-targets expected-targets
                      :source src
                      :save t)))
            (should (plist-get res :ok))
            (should (equal expected-targets (plist-get res :targets))))
          (should-not (find-buffer-visiting file))
          (let ((out (dorg-test-faithful--slurp file)))
            (should (string-match-p "^:STATUS:  +DONE$" out))
            (should-not (string-match-p "^:STATUS:  +OPEN$" out))
            (should-not (string-match-p "DORG_ID" out))))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

;; F14 — Phase-1 atomicity: a batch whose LATER op is a DIRECT identity mutation
;; (delete-property on the configured identity key) is rejected DURING plan
;; validation, before ANY op is applied.  The safe earlier op's apply is never
;; reached (snapshot rollback is NOT the contract — the first write must never
;; execute), and a real run leaves the file byte-identical.  Under A8 an item
;; payload that merely omits the key is SAFE (F9a), so the batch's hazard is a
;; DIRECT delete of the identity property, which fails closed prewrite.
;; Non-vacuous: with the guard in Phase 2 the earlier set-keyword would apply
;; before the delete-property guard fires, so `applied' would be non-nil.
(ert-deftest dorg-test-faithful-batch-antibrick-rejected-in-phase1 ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file))
         (batch
          (list (dorg-test--keyword-occurrence-patch
                 'replace dorg-test-faithful--alpha-host "FOO" 1
                 :value "changed" :missing :error)
                (dorg-test--property-delete-patch
                 dorg-test-faithful--alpha-host "BEAD_ID"))))
    (unwind-protect
        (progn
          ;; (a) Phase-1 proof: the apply step is never reached for either op.
          (let ((applied nil))
            (cl-letf (((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                       (lambda (&rest _) (push t applied))))
              (should (equal '(invalid-target . identity-brick)
                             (dorg-test-faithful--mutate-pair
                              (lambda ()
                                (dorg-test-faithful--apply src batch)))))
              (should (null applied)))
            ;; the safe first op was NOT written under the mock either
            (should (equal dorg-test-faithful--item-fixture
                           (dorg-test-faithful--slurp file))))
          ;; (b) atomicity: a real run also leaves the file byte-identical.
          (should (eq 'invalid-target
                      (dorg-test-faithful--reason
                       (lambda () (dorg-test-faithful--apply src batch)))))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F16 — property-op anti-brick: the identity guard must also cover the
;; byte-identity property path.  A faithful binding addressed by an EXTERNAL
;; identity key (BEAD_ID) can otherwise `set-property'/`delete-property' its own
;; configured key and brick its addressability.  Rejected in Phase 1 (apply
;; never reached, bytes unchanged); a same-VALUE set stays allowed.

;; F16a — set the configured identity key to a DIFFERENT value -> invalid-target.
(ert-deftest dorg-test-faithful-set-identity-property-is-invalid ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))   ; identity-key BEAD_ID
    (unwind-protect
        (let ((applied nil))
          (cl-letf (((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                     (lambda (&rest _) (push t applied))))
            (should (eq 'invalid-target
                        (dorg-test-faithful--reason
                         (lambda ()
                           (dorg-test-faithful--apply
                            src
                            (list (dorg-test--property-put-patch
                                   dorg-test-faithful--alpha-host
                                   "BEAD_ID" "MUTATED")))))))
            (should (null applied)))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F16b — delete the configured identity key -> invalid-target.
(ert-deftest dorg-test-faithful-delete-identity-property-is-invalid ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (let ((applied nil))
          (cl-letf (((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                     (lambda (&rest _) (push t applied))))
            (should (eq 'invalid-target
                        (dorg-test-faithful--reason
                         (lambda ()
                           (dorg-test-faithful--apply
                            src
                            (list (dorg-test--property-delete-patch
                                   dorg-test-faithful--alpha-host
                                   "BEAD_ID")))))))
            (should (null applied)))
          (should (equal dorg-test-faithful--item-fixture
                         (dorg-test-faithful--slurp file))))
      (delete-file file))))

;; F16c — non-vacuity: setting the identity key to its SAME value is allowed
;; (a no-op change of the load-bearing key does not brick), and a non-identity
;; property still writes — the guard is targeted, not a blanket refusal.
(ert-deftest dorg-test-faithful-set-identity-same-value-allowed ()
  (let* ((file (dorg-test-faithful--tmpfile dorg-test-faithful--item-fixture))
         (src (dorg-test-faithful--source file)))
    (unwind-protect
        (progn
          (should
           (plist-get
            (dorg-test-faithful--apply
             src
             (list (dorg-test--property-put-patch
                    dorg-test-faithful--alpha-host "BEAD_ID" "a-1")))
            :ok))
          (should
           (plist-get
            (dorg-test-faithful--apply
             src
             (list (dorg-test--property-put-patch
                    dorg-test-faithful--alpha-host "OWNER" "alice")))
            :ok))
          ;; org-entry-put aligns property columns, so allow the alignment run.
          (should (string-match-p "^:OWNER: +alice$" (dorg-test-faithful--slurp file))))
      (delete-file file))))

;;; ══════════════════════════════════════════════════════════════════════════
;;; Identity-policy + backend-fidelity SPEC (tracked-regression)
;;; ══════════════════════════════════════════════════════════════════════════
;; RED SPEC for the clean-break identity-policy contract.  Folded into this
;; (mapped) module rather than a new file so no DEVELOPMENT.org Tests-map edit is
;; needed.  Design authority: bead Design AUTHORITATIVE DECISIONS v2 (D1-D7) +
;; CORRECTIONS v3 (C2-C4).  Two ORTHOGONAL axes:
;;   FIDELITY  is BACKEND-DERIVED (never a public flag); the =:faithful= descriptor
;;             key + struct/registration slots + `dorg-mutate-source-faithful' are
;;             REMOVED (clean break); every plain-org edit is source-preserving.
;;   IDENTITY  is an INDEPENDENT policy normalized to `dorg' | (:property K) | `none'
;;             via ONE resolver `dorg-mutate--source-identity'.
;; Each test is RED until IMPL lands and fails for the RIGHT cause.

;; Captured at LOAD time (load-file-name is nil once an ert body runs).
(defconst dorg-test-idp--this-file (or load-file-name buffer-file-name)
  "Absolute path of this test file, captured at load time.")

(defconst dorg-test-idp--repo-root
  (directory-file-name
   (or (and dorg-test-idp--this-file
            (file-name-directory (directory-file-name
                                  (file-name-directory dorg-test-idp--this-file))))
       ;; fallback: dominate up to the directory containing dorg-api.el
       (locate-dominating-file default-directory "dorg-api.el")
       default-directory))
  "Repository root (dir containing dorg-api.el), captured at load time.")

(defun dorg-test-idp--tmpfile (contents)
  "Write CONTENTS to a fresh temp .org file and return its path."
  (let ((f (make-temp-file "dorg-idp-" nil ".org")))
    (with-temp-file f (insert contents))
    f))

(defun dorg-test-idp--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun dorg-test-idp--reason+detail (thunk)
  "Run THUNK; return (REASON . DETAIL) of a `dorg-mutate-error', or :no-error."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-mutate-error
     (cons (dorg-mutate-error-reason err)
           (plist-get (dorg-mutate-error-data err) :detail)))))

(defun dorg-test-idp--reason (thunk)
  "Reason symbol of a `dorg-mutate-error', or :no-error."
  (let ((r (dorg-test-idp--reason+detail thunk)))
    (if (consp r) (car r) r)))

;; Downstream tracker-shaped file whose identity is a DOMAIN property BEAD_ID (no :DORG_ID:).
(defconst dorg-test-idp--prop-fixture
  "#+title: tasks\n\n* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nBody.\n* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:END:\nBeta body.\n"
  "A plain-org file whose stable identity is the domain property BEAD_ID.")

;; An idless heading (no DORG_ID, addressed only by a domain property).
(defconst dorg-test-idp--idless-fixture
  "* Alone\n:PROPERTIES:\n:TAG: t1\n:END:\nOnly body.\n"
  "A plain-org heading with no DORG_ID (a `dorg' policy would need S0 to provision).")

;;; ─── A1: descriptor normalization + legacy-key rejection (D1/D4/D5) ───────

(ert-deftest dorg-test-idp-descriptor-property-policy ()
  "=:identity (:property \"K\")= resolves to the normalized (:property \"K\") policy."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (dorg-mutate--resolve-source (list :file file :identity '(:property "BEAD_ID")))))
    (unwind-protect
        (should (equal '(:property "BEAD_ID") (dorg-mutate--source-identity src)))
      (delete-file file))))

(ert-deftest dorg-test-idp-descriptor-none-policy ()
  "=:identity none= resolves to the normalized `none' policy."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (dorg-mutate--resolve-source (list :file file :identity 'none))))
    (unwind-protect
        (should (eq 'none (dorg-mutate--source-identity src)))
      (delete-file file))))

(ert-deftest dorg-test-idp-plain-org-default-is-dorg ()
  "A plain-org source with no explicit =:identity= derives the `dorg' policy."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (dorg-mutate--resolve-source (list :file file))))
    (unwind-protect
        (should (eq 'dorg (dorg-mutate--source-identity src)))
      (delete-file file))))

(ert-deftest dorg-test-idp-malformed-identity-is-structured ()
  "A malformed =:identity= value signals `invalid-target'/`identity-policy-malformed'."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture)))
    (unwind-protect
        (should (equal '(invalid-target . identity-policy-malformed)
                       (dorg-test-idp--reason+detail
                        (lambda () (dorg-mutate--resolve-source
                                    (list :file file :identity 'bogus))))))
      (delete-file file))))

(ert-deftest dorg-test-idp-legacy-keys-rejected ()
  "Legacy =:faithful=/=:identity-key= descriptor keys are REJECTED (clean break),
signalling `invalid-target'/`unknown-descriptor-key' — never silently tolerated."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture)))
    (unwind-protect
        (progn
          (should (equal '(invalid-target . unknown-descriptor-key)
                         (dorg-test-idp--reason+detail
                          (lambda () (dorg-mutate--resolve-source
                                      (list :file file :faithful t))))))
          (should (equal '(invalid-target . unknown-descriptor-key)
                         (dorg-test-idp--reason+detail
                          (lambda () (dorg-mutate--resolve-source
                                      (list :file file :identity-key "BEAD_ID")))))))
      (delete-file file))))

(ert-deftest dorg-test-idp-model-descriptor-rejected ()
  "S-B6 fail-closed: the D4/C4 =(:model HEADING-LIST)= source kind is OWNED WHOLLY
by the S0 CHILD (Ctl-Cdx 2026-07-15 item-6 caller audit: no parent runtime caller
exists).  At THIS parent landing a =:model= descriptor is NOT a live source kind:
it is REJECTED `invalid-target'/`unknown-descriptor-key' before any resolution,
adding no unconsumed model runtime surface.  Guards against a regression that adds
=:model= to `dorg-mutate--descriptor-keys' as a live kind."
  (should-not (memq :model dorg-mutate--descriptor-keys))
  (should (equal '(invalid-target . unknown-descriptor-key)
                 (dorg-test-idp--reason+detail
                  (lambda () (dorg-mutate--resolve-source
                              (list :model '(("h" . "heading")))))))))

(ert-deftest dorg-test-idp-buffer-descriptor-identity-threaded ()
  "B3/D4 descriptor threading: an explicit =:identity= on a =:buffer= descriptor is
HONORED — the descriptor policy WINS over the registration default, never silently
collapsed to derived `dorg'.  Covers (:property K), none, malformed, and the
omitted case (which correctly derives `dorg' from the registration)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          ;; explicit (:property "BEAD_ID") threaded through, not collapsed to dorg
          (should (equal '(:property "BEAD_ID")
                         (dorg-mutate--source-identity
                          (dorg-mutate--resolve-source
                           (list :buffer buf :identity '(:property "BEAD_ID"))))))
          ;; explicit none threaded through
          (should (eq 'none
                      (dorg-mutate--source-identity
                       (dorg-mutate--resolve-source
                        (list :buffer buf :identity 'none)))))
          ;; malformed fails structured before any use
          (should (equal '(invalid-target . identity-policy-malformed)
                         (dorg-test-idp--reason+detail
                          (lambda () (dorg-mutate--resolve-source
                                      (list :buffer buf :identity 'bogus))))))
          ;; omitted => registration/derived dorg (unchanged default)
          (should (eq 'dorg
                      (dorg-mutate--source-identity
                       (dorg-mutate--resolve-source (list :buffer buf))))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

(ert-deftest dorg-test-idp-registration-derived-defaults ()
  "A1: a source resolved from a REGISTRATION (not a descriptor) derives the default
identity policy `dorg' for BOTH backends: plain-org ⇒ dorg and dolt ⇒ dorg."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (plain-reg (make-dorg-registration
                     :backend (dorg-backend-plain-org) :file file))
         (dolt-reg (make-dorg-registration
                    :backend (dorg-backend-dolt "/tmp/does-not-matter" 1) :file file)))
    (unwind-protect
        (progn
          (should (eq 'dorg (dorg-mutate--source-identity
                             (dorg-mutate--source-from-registration plain-reg nil))))
          (should (eq 'dorg (dorg-mutate--source-identity
                             (dorg-mutate--source-from-registration dolt-reg nil)))))
      (delete-file file))))

;;; ─── A2: clean break — forbidden legacy symbols absent from production ─────

;; Read a production file's text once (repo root via locate-dominating-file).
(defun dorg-test-idp--prod-root ()
  (let ((root (directory-file-name
               (or (locate-dominating-file default-directory "dorg-api.el")
                   dorg-test-idp--repo-root))))
    (should (file-exists-p (expand-file-name "dorg-api.el" root)))
    root))

(defun dorg-test-idp--prod-text (basename)
  (with-temp-buffer
    (insert-file-contents (expand-file-name basename (dorg-test-idp--prod-root)))
    (buffer-string)))

;; Is ROOT-RELATIVE path REL a production DOrg source file?  Exclude any path
;; with a `tests/' component (test code) or a `lib/' component (vendored
;; third-party Elisp — lib/clime, lib/mysql, lib/clime/lib/web-server).  Also
;; exclude private orchestration trees so the directory-walk
;; fallback below cannot readmit their event logs.  Matching is on the RELATIVE
;; path only: this worktree's own absolute path may contain one, so filtering
;; an absolute path would wrongly drop the entire tree.
(defconst dorg-test-idp--nonprod-path-re
  (concat "\\(?:\\`\\|/\\)\\(?:tests\\|lib\\|\\.me" "ld\\|\\.em"
          "bead\\)/")
  "Regexp matching a root-relative path OUTSIDE the DOrg production surface.")

(defun dorg-test-idp--prod-file-p (rel)
  "Non-nil when ROOT-RELATIVE path REL is a DOrg production .el source file."
  (and (string-suffix-p ".el" rel)
       (not (string-match-p dorg-test-idp--nonprod-path-re rel))))

;; The single source of truth for the "production surface" both clean-break
;; structural tests scan.  Selection = git-TRACKED *.el under prod-root, so
;; UNTRACKED runtime artifacts never enter the surface: coordinator/tracker event logs
;; (.coordinator/events/*.log.el, .tracker/events/*.el) carry a .el extension and quote
;; :faithful / :identity-key / dorg-api--source-faithful-idkey in prose, and a
;; live swarm writing them from the repo root must NOT turn this release gate
;; red.  `git ls-files' alone is insufficient because the vendored libs under
;; lib/ ARE tracked; `dorg-test-idp--prod-file-p' drops those and tests/.  A
;; directory-walk fallback (same predicate) covers a non-git checkout so the
;; helper never errors.
(defun dorg-test-idp--prod-files ()
  "Absolute paths of DOrg production .el source files (never tests/, lib/, or
untracked orchestration event logs)."
  (let* ((root (dorg-test-idp--prod-root))
         (tracked (let ((default-directory (file-name-as-directory root)))
                    (ignore-errors (process-lines "git" "ls-files" "--" "*.el"))))
         (rel (or tracked
                  (mapcar (lambda (f) (file-relative-name f root))
                          (directory-files-recursively root "\\.el\\'")))))
    (seq-filter
     #'file-exists-p
     (mapcar (lambda (f) (expand-file-name f root))
             (seq-filter #'dorg-test-idp--prod-file-p rel)))))

;; Non-vacuity for the production-surface PREDICATE (named outside ^dorg-test-idp-
;; and ^dorg-test-faithful- so the 76-contract selector and faithful baseline are
;; unperturbed): DOrg modules are IN; tests/, vendored lib/ (incl. nested
;; lib/clime/lib/web-server), and untracked orchestration event trees are OUT;
;; non-.el is never production.
(ert-deftest dorg-test-prod-files-selection-selfcheck ()
  (cl-flet ((p (f) (dorg-test-idp--prod-file-p f)))
    (should (p "dorg-api.el"))
    (should (p "dorg-mutate.el"))
    (should-not (p "tests/dorg-faithful-tests.el"))
    (should-not (p "lib/clime/clime-core.el"))
    (should-not (p "lib/clime/lib/web-server/web-server.el"))
    (should-not (p (concat ".me" "ld/events/operator-messages.log.el")))
    (should-not (p (concat ".em" "bead/events/logbook.log.el")))
    (should-not (p "dorg-api.org"))))

;; Non-vacuity for `dorg-test-idp--prod-files' file SELECTION, in a throwaway git
;; repo (named outside the 76 selector + faithful baseline).  Proves BOTH
;; directions the bead requires: (A) with the ONLY forbidden tokens living in an
;; UNTRACKED .coordinator/events log and a TRACKED-but-vendored lib/ file, the scan the
;; structural test runs is CLEAN — a live swarm's event logs cannot redden the
;; gate; (B) once a forbidden token lands in a TRACKED DOrg module, the same scan
;; TRIPS — a real regression is still caught.
(ert-deftest dorg-test-prod-files-untracked-and-tracked-selfcheck ()
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (make-temp-file "dorg-prodfiles-" t)))
         (default-directory root)
         (process-environment
          (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                    "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                  process-environment)))
    (cl-flet ((emit (rel text)
                (let ((f (expand-file-name rel root)))
                  (make-directory (file-name-directory f) t)
                  (with-temp-file f (insert text))))
              (trips ()
                ;; the exact legacy-keyword scan `…-no-legacy-surface' runs.
                (seq-some
                 (lambda (f)
                   (with-temp-buffer
                     (insert-file-contents f)
                     (goto-char (point-min))
                     (re-search-forward dorg-test-idp--legacy-keyword-re nil t)))
                 (dorg-test-idp--prod-files))))
      (unwind-protect
          (progn
            (emit "dorg-api.el" ";; clean anchor\n")          ; prod-root anchor, clean
            (emit (concat ".me" "ld/events/x.log.el")
                  "(list :faithful t)\n") ; UNTRACKED forbidden
            (emit "lib/clime/clime-core.el" "(list :faithful t)\n") ; vendored forbidden
            (should (zerop (call-process "git" nil nil nil "init" "-q")))
            (call-process "git" nil nil nil "add" "dorg-api.el"
                          "lib/clime/clime-core.el")
            ;; (A) forbidden tokens only in untracked log + vendored lib ⇒ clean
            (let ((prod (mapcar (lambda (f) (file-relative-name f root))
                                (dorg-test-idp--prod-files))))
              (should (member "dorg-api.el" prod))
              (should-not (member (concat ".me" "ld/events/x.log.el") prod))
              (should-not (member "lib/clime/clime-core.el" prod)))
            (should-not (trips))
            ;; (B) forbidden token in a TRACKED DOrg module ⇒ scan trips
            (emit "dorg-foo.el" "(list :faithful t)\n")
            (call-process "git" nil nil nil "add" "dorg-foo.el")
            (should (member "dorg-foo.el"
                            (mapcar (lambda (f) (file-relative-name f root))
                                    (dorg-test-idp--prod-files))))
            (should (trips)))
        (delete-directory (directory-file-name root) t)))))

;; Source span of a top-level (cl-)defun/defmethod NAME in TEXT (handles cl-defun,
;; e.g. `dorg-mutate--plain-org-execute'), else nil.
(defun dorg-test-idp--defun-span (text name)
  (let ((start (string-match
                (concat "^(\\(?:cl-\\)?def\\(?:un\\|method\\)[ \t]+"
                        (regexp-quote name) "[ \t\n(]")
                text)))
    (when start
      (let ((next (string-match "^(\\(?:cl-\\)?def" text (1+ start))))
        (substring text start (or next (length text)))))))

;; Count REGEXP matches in TEXT that lie in CODE — outside strings and comments —
;; using a real emacs-lisp-mode buffer + syntax-ppss, so a docstring/comment that
;; textually contains `(dorg-mutate--source-identity ...)' cannot false-pass.
(defun dorg-test-idp--code-occurrences (text regexp &optional depth)
  "Count REGEXP matches in TEXT that lie in CODE (outside strings/comments).  When
DEPTH is non-nil, count only matches whose paren depth equals DEPTH (0 = top level)."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (emacs-lisp-mode))
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward regexp nil t)
        ;; `syntax-ppss' can rewind point to its POS arg; without restoring point
        ;; past the match, `re-search-forward' would re-match the same spot forever.
        (let* ((end (point))
               (ppss (save-excursion (syntax-ppss (match-beginning 0)))))
          (goto-char end)
          (when (and (not (nth 3 ppss)) (not (nth 4 ppss)) ; not string/comment
                     (or (null depth) (= (nth 0 ppss) depth)))
            (setq n (1+ n)))))
      n)))

;; ANCESTOR-AWARE no-silent-collapse.  Walk the seam's READ defun form and count
;; EXECUTABLE `(dorg-mutate--source-identity …)' CALLS as (TOTAL . CAUGHT):
;;  - CAUGHT increments when the call is lexically inside the PROTECTED region of an
;;    `ignore-errors' / `condition-case' / `condition-case-unless-debug' (its failure
;;    would be caught/replaced by nil/default/non-nil).  Handler bodies inherit the
;;    implicit caught state; they are NOT themselves a protected region, so unrelated
;;    legitimate error handling elsewhere is allowed.
;;  - `quote'/`function' subtrees are DATA/function-literals, not executable calls,
;;    and are skipped (a quoted `dorg-mutate--source-identity' does not count).
;; The seam gate requires TOTAL>0 (a real call) AND CAUGHT=0 (none collapses).
(defun dorg-test-idp--resolver-call-counts (form &optional caught acc)
  (setq acc (or acc (cons 0 0)))
  (cond
   ((not (consp form)) acc)
   ((memq (car form) '(quote function)) acc)      ; data / function literal
   ((eq (car form) 'dorg-mutate--source-identity)
    (setq acc (cons (1+ (car acc)) (if caught (1+ (cdr acc)) (cdr acc))))
    (dolist (sub (cdr form)) (setq acc (dorg-test-idp--resolver-call-counts sub caught acc)))
    acc)
   ((eq (car form) 'ignore-errors)
    (dolist (sub (cdr form)) (setq acc (dorg-test-idp--resolver-call-counts sub t acc)))
    acc)
   ((memq (car form) '(condition-case condition-case-unless-debug))
    (setq acc (dorg-test-idp--resolver-call-counts (nth 2 form) t acc)) ; protected
    (dolist (h (nthcdr 3 form))                                          ; handlers
      (setq acc (dorg-test-idp--resolver-call-counts h caught acc)))
    acc)
   (t (let ((f form))
        (while (consp f)
          (setq acc (dorg-test-idp--resolver-call-counts (car f) caught acc))
          (setq f (cdr f))))
      acc)))

;; Non-vacuity SELF-TESTS for the ancestor-aware detector: prove it counts an
;; uncaught call, flags every collapse spelling as caught, allows unrelated handling,
;; and ignores quoted/function/docstring occurrences.  (These PASS today — they test
;; the test harness, not the production contract.)
;; Named OUTSIDE ^dorg-test-idp- and ^dorg-test-faithful- so it does not perturb the
;; 76/76 contract-RED selector or the faithful baseline; run explicitly.
(ert-deftest dorg-test-resolver-counts-detector-selfcheck ()
  (cl-flet ((counts (s) (dorg-test-idp--resolver-call-counts (car (read-from-string s)))))
    ;; plain uncaught call: total 1, caught 0
    (should (equal '(1 . 0) (counts "(defun f () (dorg-mutate--source-identity x))")))
    ;; ignore-errors body: caught
    (should (equal '(1 . 1) (counts "(defun f () (ignore-errors (dorg-mutate--source-identity x)))")))
    ;; condition-case protected with nil / default-symbol / non-nil fallbacks: caught
    (should (equal '(1 . 1) (counts "(defun f () (condition-case nil (dorg-mutate--source-identity x) (error nil)))")))
    (should (equal '(1 . 1) (counts "(defun f () (condition-case e (dorg-mutate--source-identity x) (error 'id-bearing)))")))
    (should (equal '(1 . 1) (counts "(defun f () (condition-case-unless-debug e (dorg-mutate--source-identity x) (error 42)))")))
    ;; unrelated condition-case elsewhere + a separate uncaught resolver call: caught 0
    (should (equal '(1 . 0) (counts "(defun f () (dorg-mutate--source-identity x) (condition-case nil (foo) (error nil)))")))
    ;; MIXED (existential-masking bug OC flagged): one uncaught call PLUS one caught
    ;; call -> total 2, caught 1 (so caught != 0 fails the seam gate; a bare
    ;; existence-of-any-uncaught test would wrongly pass).
    (should (equal '(2 . 1) (counts "(defun f () (dorg-mutate--source-identity a) (ignore-errors (dorg-mutate--source-identity b)))")))
    (should (equal '(2 . 1) (counts "(defun f () (dorg-mutate--source-identity a) (condition-case nil (dorg-mutate--source-identity b) (error nil)))")))
    ;; resolver call INSIDE a handler (implicit uncaught) still propagates: caught 0
    (should (equal '(1 . 0) (counts "(defun f () (condition-case e (foo) (error (dorg-mutate--source-identity x))))")))
    ;; quoted data / function literal / docstring do NOT count as executable calls
    (should (equal '(0 . 0) (counts "(defun f () '(dorg-mutate--source-identity x))")))
    (should (equal '(0 . 0) (counts "(defun f () #'dorg-mutate--source-identity)")))
    (should (equal '(0 . 0) (counts "(defun f () \"see dorg-mutate--source-identity\" nil)")))))

;; Regression for the `syntax-ppss'-rewinds-point loop: prove the code-occurrence
;; scanner TERMINATES and counts EXACTLY on one and multiple real code matches, and
;; excludes comment/string matches.  Named outside the 76 selector and faithful base.
(ert-deftest dorg-test-code-occurrences-terminates-selfcheck ()
  (cl-flet ((co (s rx &optional d) (dorg-test-idp--code-occurrences s rx d)))
    (let ((rx "(dorg-mutate--source-identity\\_>"))
      ;; one real code match -> terminates, counts 1
      (should (= 1 (co "(foo (bar) (dorg-mutate--source-identity x))" rx)))
      ;; MULTIPLE real code matches -> terminates, counts exactly 2
      (should (= 2 (co "(f (dorg-mutate--source-identity a) (g (dorg-mutate--source-identity b)))" rx)))
      ;; comment match excluded
      (should (= 0 (co ";; (dorg-mutate--source-identity x)\n(foo)" rx)))
      ;; string match excluded
      (should (= 0 (co "(f \"(dorg-mutate--source-identity x)\")" rx)))
      ;; depth-0 filter: a BOL (defun at paren-depth 1 is excluded at depth 0
      (should (= 0 (co "(progn\n(defun f ()))" "^(defun\\_>" 0)))
      (should (= 1 (co "(progn\n(defun f ()))" "^(defun\\_>"))))))

;; A standalone plist keyword `:faithful' / `:identity-key' — delimiter (or BOL)
;; before the colon, delimiter (or EOL) after the keyword, so substrings like
;; `:identity-keyword', `:faithfulness', or `foo:faithful' do NOT match.
(defconst dorg-test-idp--legacy-keyword-re
  "\\(?:^\\|[^[:alnum:]:_-]\\):\\(?:faithful\\|identity-key\\)\\(?:[^[:alnum:]:_-]\\|$\\)")

(ert-deftest dorg-test-legacy-keyword-matcher-selfcheck ()
  "Non-vacuity for the legacy-keyword matcher (named outside the 76 selector):
it CATCHES standalone `:faithful'/`:identity-key' and EXCLUDES substrings."
  (cl-flet ((hit (s) (with-temp-buffer (insert s) (goto-char (point-min))
                       (and (re-search-forward dorg-test-idp--legacy-keyword-re nil t) t))))
    ;; catches standalone forbidden keywords in representative positions
    (should (hit "(list :faithful t)"))
    (should (hit "(list :identity-key \"K\")"))
    (should (hit "   :faithful"))
    (should (hit ";; a comment mentioning :identity-key here"))
    ;; excludes substrings and non-keyword colon uses
    (should-not (hit "(list :identity-keyword t)"))
    (should-not (hit ":faithfulness"))
    (should-not (hit "foo:faithful"))
    (should-not (hit "identity-key"))))

(ert-deftest dorg-test-idp-structural-no-legacy-surface ()
  "Clean break: production .el (never tests/) contains NO forbidden legacy symbol,
NO literal =:faithful=/=:identity-key= KEYWORD token, and NO
`dorg-api--source-faithful-idkey' (DELETED, not rebased).  (dorg-git-sync
`identity-keywords' is a distinct subsystem and is not a :faithful/:identity-key
keyword token, so it is not matched.)"
  (let* ((prod (dorg-test-idp--prod-files))
         (forbidden-syms '("dorg-mutate-source-faithful"
                           "dorg-registration-faithful"
                           "dorg-registration-identity-key"
                           "dorg-mutate-source-identity-key"
                           "dorg-api--source-faithful-idkey"))
         (hits nil))
    (dolist (f prod)
      (with-temp-buffer
        (insert-file-contents f)
        (dolist (sym forbidden-syms)
          (goto-char (point-min))
          (when (search-forward sym nil t)
            (push (format "%s in %s" sym (file-name-nondirectory f)) hits)))
        ;; standalone plist keyword tokens (NOT substrings).  `\\_<' does not work
        ;; before `:' in a raw (fundamental-mode) buffer, so require a delimiter
        ;; before the colon and after the keyword.
        (goto-char (point-min))
        (when (re-search-forward dorg-test-idp--legacy-keyword-re nil t)
          (push (format "legacy keyword in %s" (file-name-nondirectory f)) hits))))
    (should (null hits))))

(ert-deftest dorg-test-idp-structural-resolver-defined-once ()
  "The resolver `dorg-mutate--source-identity' is defined EXACTLY once as a
TOP-LEVEL (syntax depth 0) CODE definition in dorg-mutate.el — not text inside a
string/comment, not merely >=1."
  (should (= 1 (dorg-test-idp--code-occurrences
                (dorg-test-idp--prod-text "dorg-mutate.el")
                "^(\\(?:cl-\\)?defun[ \t]+dorg-mutate--source-identity\\_>"
                0))))

(ert-deftest dorg-test-idp-structural-source-preserving-executor ()
  "B3 clean break (Design §3 / Plan B3): the internal surgical executor + helper
family is RENAMED off the `faithful' vocabulary onto `source-preserving'.
Production .el (never tests/) contains NO `dorg-mutate--source-preserving-' symbol anywhere
(defun, caller, or docstring), and dorg-mutate.el DEFINES exactly once AND CALLS the
renamed executor `dorg-mutate--plain-org-source-preserving-execute'.  This is the
proof the earlier `dorg-test-idp-structural-no-legacy-surface' (public accessor
slots only) could not give, so B3 no longer false-passes."
  (let* ((prod (dorg-test-idp--prod-files))
         (hits nil))
    (dolist (f prod)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        ;; both the `--faithful-*' helper family AND the
        ;; `--plain-org-faithful-execute' executor must be gone.
        (when (re-search-forward "dorg-mutate--\\(?:plain-org-\\)?faithful" nil t)
          (push (file-name-nondirectory f) hits))))
    (should (null hits))
    (let ((text (dorg-test-idp--prod-text "dorg-mutate.el")))
      ;; DEFINED exactly once at top level …
      (should (= 1 (dorg-test-idp--code-occurrences
                    text
                    "^(\\(?:cl-\\)?defun[ \t]+dorg-mutate--plain-org-source-preserving-execute\\_>"
                    0)))
      ;; … and CALLED (an open-paren-led call form, not the defun header).
      (should (>= (dorg-test-idp--code-occurrences
                   text "(dorg-mutate--plain-org-source-preserving-execute\\_>")
                  1)))))

;; One INDEPENDENTLY NAMED syntax-aware test per designated identity consumer seam
;; (so each seam's RED executes on its own, never masked by an earlier abort).  Each
;; asserts the seam's defun span (1) contains a resolver CALL FORM in CODE
;; (whitespace-robust; syntax-ppss excludes docstring/comment mentions) and (2) the
;; resolver call is UNCAUGHT — ANCESTOR-AWARE: not lexically inside an
;; `ignore-errors' / `condition-case' / `condition-case-unless-debug' protected
;; region that would catch/replace its failure (a resolver failure must propagate).
;; Unrelated legitimate error handling elsewhere in the seam is NOT banned.  Applied
;; to ALL 10 seams.  LABEL FILE CONSUMER.
(defmacro dorg-test-idp--def-seam-tests (&rest seams)
  (cons 'progn
        (mapcar
         (lambda (s)
           (let ((name (intern (format "dorg-test-idp-seam-%s" (nth 0 s)))))
             `(ert-deftest ,name ()
                (let* ((text (dorg-test-idp--prod-text ,(nth 1 s)))
                       (span (dorg-test-idp--defun-span text ,(nth 2 s))))
                  (should span)
                  ;; (1) resolver CALL FORM in code
                  (should (> (dorg-test-idp--code-occurrences
                              span "([[:space:]]*dorg-mutate--source-identity\\_>")
                             0))
                  ;; (2) ancestor-aware: >=1 executable resolver call AND none caught
                  (let ((counts (dorg-test-idp--resolver-call-counts
                                 (car (read-from-string span)))))
                    (should (> (car counts) 0))
                    (should (= 0 (cdr counts))))))))
         seams)))

(dorg-test-idp--def-seam-tests
 ("ql-identity-key-spec"      "dorg-ql.el"     "dorg-ql--identity-key-spec")
 ("api-key-identity-spec"     "dorg-api.el"    "dorg-api--key-identity-spec")
 ("api-key-target-hosts"      "dorg-api.el"    "dorg-api--key-target-hosts")
 ("api-target-hosts"          "dorg-api.el"    "dorg-api--target-hosts")
 ("api-project-hit"           "dorg-api.el"    "dorg-api--project-hit")
 ("api-guard-identity-writes" "dorg-api.el"    "dorg-api--guard-identity-key-writes")
 ("api-execute-expected-target" "dorg-api.el"  "dorg-api--execute-intent")
 ("executor-patch-support"    "dorg-mutate.el" "dorg-mutate--executor-patch-support")
 ("plain-org-execute"         "dorg-mutate.el" "dorg-mutate--plain-org-execute"))

(ert-deftest dorg-test-idp-identity-survives-api-expansion ()
  "AC/D4: an explicit `:identity' is NOT lost when the API converts a descriptor to
a file/buffer for query expansion — a (:property K) source's `(key V)'
still projects to the extern key (property interpretation preserved), NOT collapsed
to an id-bearing default."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        ;; API call INSIDE unwind-protect so intended-RED errors still clean up.
        (let ((resp (dorg-api-execute
                     '(on (key "a-1") (set (property "STATUS") "DONE"))
                     :mode 'dry-run :source src)))
          (should (equal "key:BEAD_ID=a-1"
                         (dorg-api-target-token (car (plist-get resp :targets))))))
      (delete-file file))))

;;; ─── A4: (:property K) addressing + write-protection (D1/D5) ──────────────

(ert-deftest dorg-test-idp-property-extern-apply ()
  "A =(:property \"BEAD_ID\")= source addresses a heading by its extern key and
applies source-preservingly (no DORG_ID injected), WITHOUT any faithful flag."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (let ((res (dorg-test-faithful--apply
                    src (list (dorg-test-faithful--put
                               "a-1" "STATUS" "DONE")))))
          (should (plist-get res :ok))
          (let ((out (dorg-test-idp--slurp file)))
            (should (string-match-p "^:STATUS:  +DONE$" out))
            (should-not (string-match-p "DORG_ID" out))
            (should (string-match-p "^:BEAD_ID:  b-2$" out))))
      (delete-file file))))

(ert-deftest dorg-test-idp-property-key-write-protected ()
  "The declared identity property K is write-protected: mutating it via the engine
is `invalid-target'."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (should (eq 'invalid-target
                    (dorg-test-idp--reason
                     (lambda ()
                       (dorg-test-faithful--apply
                        src (list (dorg-test-faithful--put
                                   "a-1" "BEAD_ID" "mutated")))))))
      (delete-file file))))

;;; ─── A5: none is fail-closed (reads ok, every mutation refused) (D5) ───────

(ert-deftest dorg-test-idp-none-mutation-fails-closed ()
  "Under =:identity none= every mutation SIGNALS `unsupported'/`identity-none'
before any write; the source stays byte-unchanged."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none)))
    (unwind-protect
        (progn
          (should (equal '(unsupported . identity-none)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test-faithful--apply
                             src (list (dorg-test-faithful--put
                                        "a-1" "STATUS" "DONE")))))))
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

;;; ─── A6: variant-granular capability entries (C3 schema) ───────────────────

(ert-deftest dorg-test-idp-unsupported-variant-exact-error ()
  "A typed heading update with no source-preserving editor refuses
with the exact `unsupported'/`no-source-preserving-editor' contract, byte-unchanged."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          (should (equal '(unsupported . no-source-preserving-editor)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test-faithful--apply
                             src (list (list :op 'update
                                             :target (dorg-test-faithful--external "a-1")
                                             :fields '(:title "New"))))))))
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

;;; ─── A7: S0 interim — dorg missing-id is unsupported; no id bootstrap ─────

(ert-deftest dorg-test-idp-dorg-missing-id-provisioning-unavailable ()
  "Under `dorg' policy, addressing an idless heading via the D2-permitted PUBLIC
bounded selector `(where (one (property K V)))' is EXACTLY
`unsupported'/`identity-provisioning-unavailable' (S0 not landed here): the refusal
happens BEFORE any render or write (spy) and the source stays byte-unchanged."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--idless-fixture))
         ;; explicitly visit the file so the dorg source has a live query buffer
         (buf (find-file-noselect file))
         (src (list :file file :identity 'dorg))
         (touched nil))
    (unwind-protect
        ;; The whole-tree model render is the tell that a write path was entered
        ;; (the surgical executor never renders the tree); spy it to prove the
        ;; refusal precedes any render/write.
        (cl-letf (((symbol-function 'dorg-model-render-tree)
                   (lambda (&rest _) (setq touched 'rendered) "")))
          (should (equal '(unsupported . identity-provisioning-unavailable)
                         (dorg-test-idp--any-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             '(on (where (one (property "TAG" "t1")))
                                  (set (property "STATUS") "DONE"))
                             :mode 'apply :source src)))))
          (should (null touched))
          (should (equal dorg-test-idp--idless-fixture (dorg-test-idp--slurp file))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

;;; ─── A11: CLI source construction defaults to derived `dorg' ───────────────

(ert-deftest dorg-test-idp-cli-source-defaults-dorg ()
  "The CLI mutate source (dorg-app--mutate-source) is programmatic-`:identity'-free:
a plain-org file source it builds resolves to the derived `dorg' policy."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (source+buf (dorg-app--mutate-source file nil nil))
         (desc (car source+buf))
         (src (dorg-mutate--resolve-source desc)))
    (unwind-protect
        (progn
          (should-not (plist-member desc :identity))
          (should (eq 'dorg (dorg-mutate--source-identity src))))
      (delete-file file)
      (let ((buf (cdr source+buf))) (when (buffer-live-p buf) (kill-buffer buf))))))

;;; ─── A2 (cont.): behavioral parity — one interpretation across layers ─────

(ert-deftest dorg-test-idp-parity-none-uniform-refusal ()
  "One interpretation: the SAME resolved `none' policy refuses across the API
`(key V)' projection path (which threads the policy into the ql key rewrite),
and the executor apply path — both keyed off the single resolver."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none)))
    (unwind-protect
        (progn
          ;; API + ql-threading: (key V) rewrite/select under none refuses.
          (should (equal '(unsupported . identity-none)
                         (dorg-test-idp--any-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             '(on (key "a-1") (set (property "STATUS") "X"))
                             :mode 'dry-run :source src)))))
          ;; Executor: a direct extern apply refuses with the same pair.
          (should (equal '(unsupported . identity-none)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test--apply-patches
                             src (list (dorg-test-faithful--put
                                        "a-1" "STATUS" "X"))))))))
      (delete-file file))))

(ert-deftest dorg-test-idp-parity-property-extern-across-layers ()
  "One interpretation: the SAME resolved (:property K) policy drives the API
`(key V)' projection (dry-run targets the extern key) and the executor apply
(extern-key write succeeds) — proving one identity interpretation, not two."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          (should (equal '(:property "BEAD_ID")
                         (dorg-mutate--source-identity (dorg-mutate--resolve-source src))))
          ;; API projection: (key "a-1") dry-run resolves (no error) and
          ;; projects to the a-1 extern target.
          (let* ((resp (dorg-api-execute
                        '(on (key "a-1") (set (property "STATUS") "DONE"))
                        :mode 'dry-run :source src))
                 (projs (plist-get resp :projections)))
            (should projs)
            (should (cl-some
                     (lambda (p) (let ((tg (plist-get p :target)))
                                   (and tg (string-match-p "a-1" (format "%S" tg)))))
                     projs)))
          ;; Executor: the same extern key applies.
          (should (plist-get
                   (dorg-test--apply-patches
                    src (list (dorg-test-faithful--put
                               "a-1" "STATUS" "DONE")))
                   :ok)))
      (delete-file file))))

(ert-deftest dorg-test-idp-parity-ql-direct-consumer ()
  "DIRECT QL path: the designated consumer `dorg-ql--resolve-key-refs' resolves
`(key V)' using the SAME identity policy the API/executor use — `(property K V)'
for (:property K), `(id V)' for `dorg', and a `dorg-ql-error' (identity-none) for
`none'.  This proves QL receives/uses the resolver result, not API-only threading."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture)))
    (unwind-protect
        (progn
          (should (equal '(property "BEAD_ID" "a-1")
                         (dorg-ql--resolve-key-refs
                          '(key "a-1") (list :file file :identity '(:property "BEAD_ID")))))
          (should (equal '(id "100")
                         (dorg-ql--resolve-key-refs
                          '(key "100") (list :file file :identity 'dorg))))
          ;; `none': signals `dorg-ql-error' carrying the EXACT structured payload
          ;; reason `unsupported' + detail `identity-none' (asserted from the error
          ;; DATA plist, not the message text), and NEVER falls through to a scan.
          (let ((outcome (condition-case err
                             (progn (dorg-ql--resolve-key-refs
                                     '(key "a-1") (list :file file :identity 'none))
                                    :no-error)
                           (dorg-ql-error
                            ;; MESSAGE+PLIST convention (as mutate/API): the
                            ;; structured data plist is (nth 2 ERR).
                            (let ((data (nth 2 err)))
                              (cons (plist-get data :reason)
                                    (plist-get data :detail))))
                           (error :wrong-error-type))))
            (should (equal '(unsupported . identity-none) outcome))))
      (delete-file file))))

;;; ─── A3: backend-derived fidelity — dorg id-addressed is source-preserving ──

(defconst dorg-test-idp--dorgid-fixture
  "#+title: My Tasks\n\n* TODO First     :work:urgent:\n:PROPERTIES:\n:DORG_ID:  100\n:STATUS:   OPEN\n:END:\n\nBody one.\n* TODO Second   :misc:\n:PROPERTIES:\n:DORG_ID:  200\n:END:\n"
  "A DORG_ID-bearing plain-org file with NONCANONICAL spacing (a whole-tree
re-render would normalize it; a source-preserving edit must not).")

(defconst dorg-test-idp--dorgid-expected
  "#+title: My Tasks\n\n* TODO First     :work:urgent:\n:PROPERTIES:\n:DORG_ID:  100\n:STATUS:   DONE\n:END:\n\nBody one.\n* TODO Second   :misc:\n:PROPERTIES:\n:DORG_ID:  200\n:END:\n"
  "Exact byte-level result of a source-preserving STATUS=DONE on DORG_ID 100:
ONLY the STATUS value changes; every other byte (title keyword case, both heading
lines' author spacing, the blank run, the sibling drawer) is identical.")

(ert-deftest dorg-test-idp-dorg-id-apply-source-preserving ()
  "Under `dorg' policy, applying a property op to a DORG_ID-bearing heading is
source-preserving: the WHOLE file is byte-identical to the fixture except the one
changed value; no whole-tree re-render, no DORG_* added to the unaffected sibling."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dorgid-fixture))
         (src (list :file file :identity 'dorg))
         ;; exact unaffected sibling block (from its heading line to EOF)
         (sibling "* TODO Second   :misc:\n:PROPERTIES:\n:DORG_ID:  200\n:END:\n"))
    (unwind-protect
        (let ((res (dorg-test--apply-patches
                    src (list (dorg-test--property-put-patch
                               (dorg-test--heading-address 100)
                               "STATUS" "DONE")))))
          (should (plist-get res :ok))
          (let ((out (dorg-test-idp--slurp file)))
            ;; whole-file byte equality to the precise expected output
            (should (equal dorg-test-idp--dorgid-expected out))
            ;; non-vacuity: the edit actually changed the file
            (should-not (equal dorg-test-idp--dorgid-fixture out))
            ;; the unaffected sibling heading region is byte-identical (no re-render,
            ;; no injected DORG_*)
            (should (string-suffix-p sibling out))))
      (delete-file file))))

;; A3 (cont.): every surviving source-preserving typed family under DORG policy,
;; each an exact source-preserving whole-file transformation with the DORG_ID
;; drawer preserved OUT-OF-BAND.  Per-op NAMED tests (one failing cell never masks
;; the rest).  LABEL FIXTURE OP EXPECTED-WHOLE-FILE.
(defmacro dorg-test-idp--def-supported-dorg (&rest cases)
  (cons 'progn
        (mapcar
         (lambda (c)
           (let ((name (intern (format "dorg-test-idp-dorg-supported-%s" (nth 0 c)))))
             `(ert-deftest ,name ()
                (let* ((file (dorg-test-idp--tmpfile ,(nth 1 c)))
                       (src (list :file file :identity 'dorg)))
                  (unwind-protect
                      (progn
                        ;; policy-distinguishing: the source must resolve to `dorg'
                        ;; (else the edit merely reflects legacy DORG_ID targeting,
                        ;; not the dorg identity policy).
                        (should (eq 'dorg (dorg-mutate--source-identity
                                           (dorg-mutate--resolve-source src))))
                        (should (plist-get
                                 (dorg-test--apply-patches src (list ',(nth 2 c)))
                                 :ok))
                        (let ((out (dorg-test-idp--slurp file)))
                          (should-not (equal ,(nth 1 c) out))
                          (should (equal ,(nth 3 c) out))
                          ;; DORG_ID drawer preserved exactly once, out-of-band
                          (should (= 1 (cl-count-if
                                        (lambda (l) (string-match-p "DORG_ID" l))
                                        (split-string out "\n"))))))
                    (delete-file file))))))
         cases)))

(dorg-test-idp--def-supported-dorg
 ("set-property"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n"
  (:op put :target (:property :host (:heading :id 100) :name "OWNER")
       :value "alice")
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    alice\n:END:\nbody\n")
 ("delete-property"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:STATUS: OPEN\n:END:\n"
  (:op delete :target (:property :host (:heading :id 100) :name "STATUS"))
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n")
 ("set-keyword"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n"
  (:op insert :target (:keywords :host (:heading :id 100) :name "FOO")
       :position :after-last :value "bar")
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n#+FOO: bar\n")
 ("delete-keyword"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n#+FOO: bar\nbody\n"
  (:op delete :target (:keywords :host (:heading :id 100) :name "FOO"))
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n")
 ("insert-item"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n"
  (:op insert :target (:texts :host (:heading :id 100))
       :position :after-last :value "EXTRA")
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nbody\n\nEXTRA\n")
 ("replace-item"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nold\n"
  (:op replace :target (:text :host (:heading :id 100) :occurrence 1)
       :missing :error :value "new")
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nnew\n")
 ;; MIXED drawer (identity DORG_ID + non-identity OWNER with distinctive 4-space
 ;; alignment): the exact whole-file pin proves the ENTIRE drawer is preserved
 ;; verbatim out-of-band, not reconstructed from the identity line alone.
 ("replace-items"
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nold body\n"
  (:op replace :target (:items :host (:heading :id 100))
       :items ((:kind text :text "fresh")))
  "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nfresh\n"))

;;; ─── A4 (cont.): duplicate identity K — exact API vs engine layer split ────

(defconst dorg-test-idp--dup-fixture
  "* One\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n* Two\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n"
  "Two headings share BEAD_ID=dup (an identity violation on a (:property K) source).")

(ert-deftest dorg-test-idp-duplicate-key-engine-layer ()
  "ENGINE layer: a duplicate external K on a (:property K) source resolving an
extern target is `dorg-mutate-error'/`invalid-target' :detail `ambiguous'."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dup-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          ;; policy-distinguishing: the source resolves to (:property "BEAD_ID")
          (should (equal '(:property "BEAD_ID")
                         (dorg-mutate--source-identity (dorg-mutate--resolve-source src))))
          (should (equal '(invalid-target . ambiguous)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test--apply-patches
                             src (list (dorg-test-faithful--put
                                        "dup" "STATUS" "X"))))))))
      (delete-file file))))

(ert-deftest dorg-test-idp-duplicate-key-api-layer ()
  "API layer: a duplicate external K via the public `(key V)' selector is
`dorg-api-error'/`identity-violation' (a distinct layer contract from the engine)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dup-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (should (eq 'identity-violation
                    (dorg-test-faithful--api-reason
                     (lambda ()
                       (dorg-api-execute
                        '(on (key "dup") (set (property "STATUS") "X"))
                        :mode 'apply :source src)))))
      (delete-file file))))

(ert-deftest dorg-test-idp-missing-key-engine-layer ()
  "A4 new-policy contract: a (:property K) source addressing a K value that matches
NO heading is EXACTLY `dorg-mutate-error'/`invalid-target' :detail `missing' at the
ENGINE layer (not a legacy deferred contract); no write."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          ;; policy-distinguishing: the source resolves to (:property "BEAD_ID")
          (should (equal '(:property "BEAD_ID")
                         (dorg-mutate--source-identity (dorg-mutate--resolve-source src))))
          (should (equal '(invalid-target . missing)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test--apply-patches
                             src (list (dorg-test-faithful--put
                                        "no-such" "STATUS" "X")))))))
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-missing-key-api-layer ()
  "A4 new-policy contract: a missing K via the public `(key V)' selector is
EXACTLY `invalid-target' at the API layer; no write."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          (should (equal '(invalid-target . missing)
                         (dorg-test-idp--any-reason+detail
                          (lambda ()
                            (dorg-api-execute
                             '(on (key "no-such") (set (property "STATUS") "X"))
                             :mode 'apply :source src)))))
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

;;; ─── A5 (cont.): none across plan + dry-run + reads ────────────────────────

(ert-deftest dorg-test-idp-none-plan-signals ()
  "`dorg-test--plan-patches' on a `none' source signals `unsupported'/`identity-none' (it does
NOT return an unsupported plan)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none)))
    (unwind-protect
        (should (equal '(unsupported . identity-none)
                       (dorg-test-idp--reason+detail
                        (lambda ()
                          (dorg-test--plan-patches
                           src (list (dorg-test-faithful--put
                                      "a-1" "STATUS" "X")))))))
      (delete-file file))))

(defun dorg-test-idp--any-reason+detail (thunk)
  "Return (REASON . DETAIL) for a `dorg-api-error' OR `dorg-mutate-error', else :no-error.
Captures ONE error object so reason and detail come from the same signal."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-api-error (cons (dorg-api-error-reason err)
                          (plist-get (dorg-api-error-data err) :detail)))
    (dorg-mutate-error (cons (dorg-mutate-error-reason err)
                             (plist-get (dorg-mutate-error-data err) :detail)))))

(ert-deftest dorg-test-idp-none-dry-run-signals ()
  "A `none' source dry-run through the public envelope refuses with the EXACT PAIR
reason `unsupported' AND detail `identity-none' (one captured signal), before any
plan."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none)))
    (unwind-protect
        (should (equal '(unsupported . identity-none)
                       (dorg-test-idp--any-reason+detail
                        (lambda ()
                          (dorg-api-execute
                           '(on (key "a-1") (set (property "STATUS") "X"))
                           :mode 'dry-run :source src)))))
      (delete-file file))))

(ert-deftest dorg-test-idp-none-reads-succeed ()
  "A `none' source is READ-ONLY, not read-blocked: for a resolved
`(:file F :identity none)' source, every non-identity read selector (id, property,
title, tag) over that source's file returns the heading, WHILE a mutation on the
SAME none source is refused `unsupported'/`identity-none'.  Ties reads-succeed to
the none policy (not a bare unrelated buffer)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dorgid-fixture))
         (desc (list :file file :identity 'none)))
    (unwind-protect
        (progn
          ;; the source under test is demonstrably none
          (should (eq 'none (dorg-mutate--source-identity
                             (dorg-mutate--resolve-source desc))))
          ;; reads run through the POLICY-AWARE descriptor path (dorg-api--ql-select
          ;; resolves the identity=none source), and succeed for every selector,
          ;; each returning the SAME heading (id 100 — pinned, not mere truthiness,
          ;; so an unrelated match cannot false-green).  NOTE (tests-only vehicle
          ;; correction, disclosed to nav): the property selector is an ORDINARY
          ;; unique property STATUS=OPEN — DORG_ID stays excluded from generic
          ;; property lookup (metadata suppression preserved; see
          ;; dorg-backend-tests.el:433).
          (should (= 100 (plist-get (car (dorg-api--ql-select '(id 100) desc)) :heading-id)))
          (should (= 100 (plist-get (car (dorg-api--ql-select '(property "STATUS" "OPEN") desc)) :heading-id)))
          (should (= 100 (plist-get (car (dorg-api--ql-select '(title "First") desc)) :heading-id)))
          (should (= 100 (plist-get (car (dorg-api--ql-select '(tag "work") desc)) :heading-id)))
          ;; contrast: a mutation on the SAME none source is fail-closed
          (should (equal '(unsupported . identity-none)
                         (dorg-test-idp--reason+detail
                          (lambda ()
                            (dorg-test--apply-patches
                             desc (list (dorg-test--property-put-patch
                                         (dorg-test--heading-address 100)
                                         "STATUS" "X"))))))))
      (delete-file file))))

(ert-deftest dorg-test-idp-none-apply-dry-run-signals ()
  "The RAW engine dry-run path `(dorg-test--apply-patches SRC OPS :dry-run t)' on a `none'
source SIGNALS the exact pair (unsupported . identity-none) via the SHARED gate —
it does NOT return a `:supported nil' plan — BEFORE any render or write, leaving the
source byte-unchanged (the whole-tree renderer never fires)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none))
         (rendered nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dorg-model-render-tree)
                     (lambda (&rest _) (setq rendered t) "")))
            (should (equal '(unsupported . identity-none)
                           (dorg-test-idp--reason+detail
                            (lambda ()
                              (dorg-test--apply-patches
                               src (list (dorg-test-faithful--put
                                          "a-1" "STATUS" "X"))
                               :dry-run t))))))
          (should-not rendered)
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-none-empty-ops-valid-plan ()
  "An EMPTY batch is a NO-OP, not a mutation: `(dorg-test--plan-patches SRC nil)' on a `none'
source returns a VALID plan (`:ok t', `:supported t', no ops) — the shared none gate
is scoped to a NON-EMPTY normalized batch, so an empty plan is never reclassified as
a refused mutation."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity 'none)))
    (unwind-protect
        (let ((plan (dorg-test--plan-patches src nil)))
          (should (plist-get plan :ok))
          (should (eq t (plist-get plan :supported)))
          (should (null (plist-get plan :patches)))
          (should (null (plist-get plan :unsupported))))
      (delete-file file))))

;;; ─── A6 (cont.): EVERY authoritative C3 inventory row ──────────────────────

;; Per-cell SUPPORTED typed transformation tests.  Each is a SEPARATELY NAMED ert test
;; (so one failing cell never masks the other seven) asserting fixture != output
;; AND the exact whole-file output (computed once from the source-preserving
;; executor).  LABEL FIXTURE PATCH EXPECTED-WHOLE-FILE.
(defmacro dorg-test-idp--def-supported (&rest cases)
  (cons 'progn
        (mapcar
         (lambda (c)
           (let ((name (intern (format "dorg-test-idp-supported-%s" (nth 0 c)))))
             `(ert-deftest ,name ()
                (let* ((file (dorg-test-idp--tmpfile ,(nth 1 c)))
                       (src (list :file file :identity '(:property "BEAD_ID"))))
                  (unwind-protect
                      (progn
                        (should (plist-get (dorg-test--apply-patches src (list ',(nth 2 c))) :ok))
                        (let ((out (dorg-test-idp--slurp file)))
                          (should-not (equal ,(nth 1 c) out))
                          (should (equal ,(nth 3 c) out))))
                    (delete-file file))))))
         cases)))

(dorg-test-idp--def-supported
 ("set-property"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n"
  (:op put :target (:property :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                              :name "OWNER")
       :value "alice")
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:OWNER:    alice\n:END:\nbody\n")
 ("delete-property"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:STATUS: OPEN\n:END:\n"
  (:op delete :target (:property :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                                 :name "STATUS"))
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\n")
 ("set-keyword"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n"
  (:op insert :target (:keywords :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                                :name "FOO")
       :position :after-last :value "bar")
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n#+FOO: bar\n")
 ("delete-keyword"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\n#+FOO: bar\nbody\n"
  (:op delete :target (:keywords :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                                :name "FOO"))
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n")
 ("insert-item"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n"
  (:op insert :target (:texts :host (:heading :extern t :key "BEAD_ID" :value "a-1"))
       :position :after-last :value "EXTRA")
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nbody\n\nEXTRA\n")
 ("replace-item"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nold\n"
  (:op replace :target (:text :host (:heading :extern t :key "BEAD_ID" :value "a-1")
                             :occurrence 1)
       :missing :error :value "new")
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nnew\n")
 ;; replace-items: caller payload has ONLY the non-identity item; the executor
 ;; preserves the WHOLE identity drawer OUT-OF-BAND (A8, not the legacy brick).
 ;; The drawer is MIXED (identity + a non-identity OWNER with author 4-space
 ;; alignment) and the exact whole-file pin proves every drawer byte — not just
 ;; the identity line — survives verbatim (no reconstruct/reformat).
 ("replace-items"
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:OWNER:    bob\n:END:\nold body\n"
  (:op replace :target (:items :host (:heading :extern t :key "BEAD_ID" :value "a-1"))
       :items ((:kind text :text "fresh")))
  "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:OWNER:    bob\n:END:\nfresh\n"))

;; Per-row UNSUPPORTED refusal tests.  Each is a SEPARATELY NAMED ert test (so one
;; row never masks the others) asserting the exact `unsupported'/
;; `no-source-preserving-editor' + the EXACT authoritative C3 `:variant'
;; (=(:host root)= for root-host rows; the empty variant nil for collapsed
;; whole-verb rows), with a render/write SPY proving refusal BEFORE any render/write
;; and the source byte-identical.  LABEL PATCH EXPECTED-VARIANT.
(defmacro dorg-test-idp--def-unsupported (&rest rows)
  (cons 'progn
        (mapcar
         (lambda (r)
           (let ((name (intern (format "dorg-test-idp-unsupported-%s" (nth 0 r)))))
             `(ert-deftest ,name ()
                (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
                       (src (list :file file :identity '(:property "BEAD_ID")))
                       (touched nil))
                  (unwind-protect
                      (cl-letf (((symbol-function 'dorg-model-render-tree)
                                 (lambda (&rest _) (setq touched 'rendered) ""))
                                ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                                 (lambda (&rest _) (setq touched 'applied))))
                        (condition-case err
                            (progn (dorg-test--apply-patches src (list ',(nth 1 r)))
                                   (should (eq :must-have-refused ',(nth 0 r))))
                          (dorg-mutate-error
                           (let ((data (dorg-mutate-error-data err)))
                             (should (eq 'unsupported (dorg-mutate-error-reason err)))
                             (should (eq 'no-source-preserving-editor (plist-get data :detail)))
                             (should (equal ',(nth 2 r) (plist-get data :variant))))
                           (should (null touched))
                           (should (equal dorg-test-idp--prop-fixture
                                          (dorg-test-idp--slurp file))))))
                    (delete-file file))))))
         rows)))

(dorg-test-idp--def-unsupported
 ;; root-host rows -> variant (:host root)
 ("put-property-root"
  (:op put :target (:property :host :root :name "K") :value "V")
  (:host root))
 ("delete-property-root"
  (:op delete :target (:property :host :root :name "K"))
  (:host root))
 ("insert-keyword-root"
  (:op insert :target (:keywords :host :root :name "K")
       :position :after-last :value "V")
  (:host root))
 ("delete-keyword-root"
  (:op delete :target (:keywords :host :root :name "K"))
  (:host root))
 ("insert-texts-root"
  (:op insert :target (:texts :host :root) :position :after-last :value "x")
  (:host root))
 ("replace-text-root"
  (:op replace :target (:text :host :root :occurrence 1)
       :missing :error :value "x")
  (:host root))
 ("replace-items-root"
  (:op replace :target (:items :host :root)
       :items ((:kind text :text "x")))
  (:host root))
 ;; Structural rows -> collapsed empty variant nil.
 ("update-heading"
  (:op update :target (:heading :extern t :key "BEAD_ID" :value "a-1")
       :fields (:title "New")) nil)
 ("update-planning"
  (:op update
       :target (:planning :host (:heading :extern t :key "BEAD_ID" :value "a-1"))
       :scheduled "<2026-01-01 Wed>") nil)
 ("insert-heading"
  (:op insert :target (:tree :under :root) :heading (:title "New")) nil)
 ("delete-heading"
  (:op delete :target (:heading :extern t :key "BEAD_ID" :value "a-1")) nil)
 ("delete-subtree"
  (:op delete :target (:subtree :extern t :key "BEAD_ID" :value "a-1")) nil)
 ("move-subtree"
  (:op move :target (:subtree :extern t :key "BEAD_ID" :value "a-1")
       :to (:under :root)) nil))

;;; ─── A7 (cont.): BOTH negative bootstrap selectors under dorg ──────────────

(ert-deftest dorg-test-idp-key-cannot-bootstrap-idless ()
  "`(key V)' under `dorg' cannot provision an idless heading — the key
rewrites to an id lookup that cannot match on a source CONTAINING an idless
heading; EXACT (invalid-target . id-bootstrap-unsupported) BEFORE any
render/apply/write (independent spies never fire), source byte-identical."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--idless-fixture))
         (src (list :file file :identity 'dorg))
         (rendered nil) (applied nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dorg-model-render-tree)
                     (lambda (&rest _) (setq rendered t) ""))
                    ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                     (lambda (&rest _) (setq applied t))))
            (should (equal '(invalid-target . id-bootstrap-unsupported)
                           (dorg-test-idp--any-reason+detail
                            (lambda ()
                              (dorg-api-execute
                               '(on (key "t1") (set (property "STATUS") "X"))
                               :mode 'apply :source src))))))
          (should-not rendered) (should-not applied)
          (should (equal dorg-test-idp--idless-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-id-cannot-bootstrap-idless ()
  "A direct `(id N)' cannot bootstrap an idless target under `dorg': an id selector
matching zero headings on a source CONTAINING an idless heading is EXACTLY
`invalid-target'/`id-bootstrap-unsupported' (distinct from a non-identity selector's
`missing', and from a plain miss on an only-identified source) — BEFORE any
render/apply/write (independent spies never fire), source byte-identical."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--idless-fixture))
         (src (list :file file :identity 'dorg))
         (rendered nil) (applied nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dorg-model-render-tree)
                     (lambda (&rest _) (setq rendered t) ""))
                    ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                     (lambda (&rest _) (setq applied t))))
            (should (equal '(invalid-target . id-bootstrap-unsupported)
                           (dorg-test-idp--reason+detail
                            (lambda ()
                              (dorg-test--apply-patches
                               src (list (dorg-test--property-put-patch
                                          (dorg-test--heading-address 999)
                                          "STATUS" "X"))))))))
          (should-not rendered) (should-not applied)
          (should (equal dorg-test-idp--idless-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-property-policy-integer-miss-is-missing ()
  "A7 property-policy leakage guard: id-bootstrap-unsupported is `dorg'-ONLY.  Under a
`(:property K)' policy, a raw integer-id op matching NOTHING on a source with NO
DORG_ID (but a BEAD_ID) is an ORDINARY `(invalid-target . missing)' — the idless
heading must NOT leak the bootstrap classification into a non-`dorg' policy — signalled
BEFORE any render/apply/write (independent spies never fire), source byte-identical."
  (let* ((fixture "* Alpha\n:PROPERTIES:\n:BEAD_ID:  a-1\n:END:\nBody.\n")
         (file (dorg-test-idp--tmpfile fixture))
         (src (list :file file :identity '(:property "BEAD_ID")))
         (rendered nil) (applied nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dorg-model-render-tree)
                     (lambda (&rest _) (setq rendered t) ""))
                    ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                     (lambda (&rest _) (setq applied t))))
            (should (equal '(invalid-target . missing)
                           (dorg-test-idp--reason+detail
                            (lambda ()
                              (dorg-test--apply-patches
                               src (list (dorg-test--property-put-patch
                                          (dorg-test--heading-address 999)
                                          "STATUS" "X"))))))))
          (should-not rendered) (should-not applied)
          (should (equal fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(defun dorg-test-idp--api-detail (thunk)
  "Return the `:detail' of a `dorg-api-error'/`dorg-mutate-error', or :no-error."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-api-error (plist-get (dorg-api-error-data err) :detail))
    (dorg-mutate-error (plist-get (dorg-mutate-error-data err) :detail))))

;;; ─── A8: replace-items under dorg preserves the drawer DORG_ID ─────────────

(defconst dorg-test-idp--dorgid-item-fixture
  "* TODO Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nOld body.\n"
  "A DORG_ID-bearing heading with a MIXED drawer (identity + a non-identity
OWNER at author 4-space alignment) for replace-items drawer-preservation: the
whole drawer, not just the identity line, must survive verbatim out-of-band.")

(defun dorg-test-idp--items-mention-dorg-id-p (items)
  "Non-nil if any ITEMS entry references a DORG_ID (drawer key or keyword name)."
  (cl-some
   (lambda (it)
     (or (and (equal (plist-get it :kind) 'keyword)
              (let ((k (plist-get it :keyword))) (and k (string-prefix-p "DORG_" k))))
         (and (equal (plist-get it :kind) 'drawer)
              (cl-some (lambda (kv) (string-prefix-p "DORG_" (car kv)))
                       (plist-get it :properties)))))
   items))

(ert-deftest dorg-test-idp-replace-items-preserves-dorg-id ()
  "Under `dorg', a replace-items that rewrites the body items preserves the drawer
DORG_ID verbatim (identity lives OUT-OF-BAND); AND the caller-submitted payload
itself contains NO DORG_ID item (A8 addendum)."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dorgid-item-fixture))
         (src (list :file file :identity 'dorg))
         (items '((:kind text :text "New body."))))
    (unwind-protect
        (progn
          ;; policy-distinguishing: source resolves to `dorg' (not legacy fallthrough)
          (should (eq 'dorg (dorg-mutate--source-identity
                             (dorg-mutate--resolve-source src))))
          ;; A8 addendum: the submitted payload carries no DORG_ID item.
          (should-not (dorg-test-idp--items-mention-dorg-id-p items))
          (let ((res (dorg-test--apply-patches
                      src (list (dorg-test--items-replace-patch
                                 (dorg-test--heading-address 100) items)))))
            (should (plist-get res :ok))
            (let ((out (dorg-test-idp--slurp file)))
              ;; EXACT whole-file: only the body item changes; the WHOLE mixed
              ;; drawer (DORG_ID + OWNER's 4-space alignment) is preserved verbatim
              ;; out-of-band (the caller never submitted it).
              (should (equal "* TODO Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nNew body.\n"
                             out))
              ;; supplemental transformation checks
              (should-not (equal dorg-test-idp--dorgid-item-fixture out))
              (should-not (string-match-p "Old body\\." out))
              (should (= 1 (cl-count-if
                            (lambda (l) (string-match-p "DORG_ID" l))
                            (split-string out "\n")))))))
      (delete-file file))))

;;; ─── A8 (cont.): body payload must never carry a PROPERTIES drawer ─────────
;; The heading's authoritative PROPERTIES drawer is preserved OUT-OF-BAND, so a
;; body/item payload must carry NO PROPERTIES drawer — FIELD-INDEPENDENT.  A
;; payload PROPERTIES drawer holding the identity key smuggles/duplicates
;; identity (F9b/F9c); one holding ONLY non-identity fields would splice a SECOND
;; metadata drawer after the preserved one — both fail closed with the exact
;; pair (invalid-target . identity-brick), before any apply/write, bytes intact.
;; OTHER named drawers (a body LOGBOOK, etc.) are ordinary content and pass.

(ert-deftest dorg-test-idp-payload-properties-drawer-dorg-is-invalid ()
  "Under `dorg', a replace-items payload carrying a PROPERTIES drawer with ONLY
non-identity fields (no DORG_ID) is refused prewrite — never spliced as a second
metadata drawer.  Exact (invalid-target . identity-brick); apply never reached;
source byte-identical."
  (let* ((file (dorg-test-idp--tmpfile
                "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nold\n"))
         (src (list :file file :identity 'dorg))
         (applied nil) (wrote nil))
    (unwind-protect
        (progn
          ;; Independent apply AND write spies: apply-not-reached does NOT imply
          ;; write-not-reached, so both are asserted.  The faithful file path
          ;; writes via `write-region'; spying it makes the assertion non-vacuous.
          (cl-letf* ((wr (symbol-function 'write-region))
                     ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                      (lambda (&rest _) (push t applied)))
                     ((symbol-function 'write-region)
                      (lambda (&rest a) (setq wrote 'write-region) (apply wr a))))
            (should (equal '(invalid-target . identity-brick)
                           (dorg-test-idp--reason+detail
                            (lambda ()
                              (dorg-test--apply-patches
                               src (list (dorg-test--items-replace-patch
                                          (dorg-test--heading-address 100)
                                          '((:kind drawer :name "PROPERTIES"
                                                               :properties (("OWNER" . "bob")))
                                                        (:kind text :text "new")))))))))
            (should (null applied))
            (should (null wrote)))
          (should (equal "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:END:\nold\n"
                         (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-payload-properties-drawer-property-is-invalid ()
  "Under `(:property K)', a replace-items payload carrying a PROPERTIES drawer
with ONLY non-identity fields (no BEAD_ID) is refused prewrite — same
field-independent contract.  Exact (invalid-target . identity-brick); apply
never reached; source byte-identical."
  (let* ((file (dorg-test-idp--tmpfile
                "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nold\n"))
         (src (list :file file :identity '(:property "BEAD_ID")))
         (applied nil) (wrote nil))
    (unwind-protect
        (progn
          ;; Independent apply AND write spies (write-region is the faithful file
          ;; write path).
          (cl-letf* ((wr (symbol-function 'write-region))
                     ((symbol-function 'dorg-mutate--source-preserving-apply-patch)
                      (lambda (&rest _) (push t applied)))
                     ((symbol-function 'write-region)
                      (lambda (&rest a) (setq wrote 'write-region) (apply wr a))))
            (should (equal '(invalid-target . identity-brick)
                           (dorg-test-idp--reason+detail
                            (lambda ()
                              (dorg-test--apply-patches
                               src (list (dorg-test--items-replace-patch
                                          (dorg-test-faithful--external "a-1")
                                          '((:kind drawer :name "PROPERTIES"
                                                               :properties (("OWNER" . "bob")))
                                                        (:kind text :text "new")))))))))
            (should (null applied))
            (should (null wrote)))
          (should (equal "* TODO A\n:PROPERTIES:\n:BEAD_ID: a-1\n:END:\nold\n"
                         (dorg-test-idp--slurp file))))
      (delete-file file))))

(ert-deftest dorg-test-idp-payload-nonproperties-drawer-legal ()
  "A NON-PROPERTIES named drawer (LOGBOOK) in a replace-items body payload is
ordinary content and is spliced in legally, WHILE the authoritative MIXED
PROPERTIES drawer (DORG_ID + a non-identity OWNER at 4-space alignment) is
preserved verbatim out-of-band.  Wrong initial state -> exact whole-file result,
before != after."
  (let* ((fixture "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nold body\n")
         (file (dorg-test-idp--tmpfile fixture))
         (src (list :file file :identity 'dorg)))
    (unwind-protect
        (progn
          (should (plist-get
                   (dorg-test--apply-patches
                    src (list (dorg-test--items-replace-patch
                               (dorg-test--heading-address 100)
                               '((:kind drawer :name "LOGBOOK" :lines ("- note"))
                                 (:kind text :text "new")))))
                   :ok))
          (let ((out (dorg-test-idp--slurp file)))
            ;; exact whole-file: mixed PROPERTIES drawer bytes verbatim, LOGBOOK
            ;; spliced into the body, old body gone
            (should (equal "* TODO A\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\n:LOGBOOK:\n- note\n:END:\nnew\n"
                           out))
            (should-not (equal fixture out))
            (should (string-match-p "^:LOGBOOK:$" out))
            ;; the authoritative drawer survived exactly once, verbatim
            (should (= 1 (cl-count ":OWNER:    bob" (split-string out "\n")
                                   :test #'equal)))))
      (delete-file file))))

;;; ─── A8 (cont.): planning BEFORE the PROPERTIES drawer — leading metadata ──
;; Org fixes a planning line ABOVE the PROPERTIES drawer, so the leading metadata
;; block is (planning + drawer).  A body/item splice must preserve that WHOLE
;; contiguous block verbatim (locating the body boundary PHYSICALLY, so a
;; DORG_ID-only drawer that parses to no item span is still preserved and a
;; planning line never shifts the body region over the drawer).

(ert-deftest dorg-test-idp-planning-before-drawer-dorg-text-replace ()
  "Under `dorg', a targeted text replace on a heading whose MIXED PROPERTIES
drawer (DORG_ID + OWNER at 4-space alignment) is preceded by a SCHEDULED line
preserves BOTH the planning line and the WHOLE drawer verbatim out-of-band; only
the addressed body text changes.  Exact whole-file, before != after."
  (let* ((fixture "* TODO A\nSCHEDULED: <2026-01-01 Thu>\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nold body\n")
         (file (dorg-test-idp--tmpfile fixture))
         (src (list :file file :identity 'dorg)))
    (unwind-protect
        (progn
          (should (plist-get
                   (dorg-test--apply-patches
                    src (list (list :op 'replace
                                    :target (list :text :host
                                                  (dorg-test--heading-address 100)
                                                  :occurrence 1)
                                    :missing :error
                                    :value "new body")))
                   :ok))
          (let ((out (dorg-test-idp--slurp file)))
            (should (equal "* TODO A\nSCHEDULED: <2026-01-01 Thu>\n:PROPERTIES:\n:DORG_ID: 100\n:OWNER:    bob\n:END:\nnew body\n"
                           out))
            (should-not (equal fixture out))
            (should (string-match-p "^SCHEDULED: <2026-01-01 Thu>$" out))
            (should (= 1 (cl-count ":OWNER:    bob" (split-string out "\n") :test #'equal)))
            (should (= 1 (cl-count ":DORG_ID: 100" (split-string out "\n") :test #'equal)))))
      (delete-file file))))

(ert-deftest dorg-test-idp-planning-before-drawer-property-replace-items ()
  "Under `(:property K)', a replace-items SEQUENCE on a heading whose MIXED
PROPERTIES drawer (BEAD_ID + OWNER at 4-space alignment) is preceded by a
SCHEDULED line preserves BOTH the planning line and the WHOLE drawer verbatim
out-of-band; only the body sequence is replaced.  Exact whole-file, before != after."
  (let* ((fixture "* TODO A\nSCHEDULED: <2026-01-01 Thu>\n:PROPERTIES:\n:BEAD_ID: a-1\n:OWNER:    bob\n:END:\nold body\n")
         (file (dorg-test-idp--tmpfile fixture))
         (src (list :file file :identity '(:property "BEAD_ID"))))
    (unwind-protect
        (progn
          (should (plist-get
                   (dorg-test--apply-patches
                    src (list (dorg-test--items-replace-patch
                               (dorg-test-faithful--external "a-1")
                               '((:kind text :text "fresh")
                                 (:kind keyword :keyword "FOO" :value "bar")))))
                   :ok))
          (let ((out (dorg-test-idp--slurp file)))
            (should (equal "* TODO A\nSCHEDULED: <2026-01-01 Thu>\n:PROPERTIES:\n:BEAD_ID: a-1\n:OWNER:    bob\n:END:\nfresh\n#+FOO: bar\n"
                           out))
            (should-not (equal fixture out))
            (should (string-match-p "^SCHEDULED: <2026-01-01 Thu>$" out))
            (should (= 1 (cl-count ":OWNER:    bob" (split-string out "\n") :test #'equal)))
            (should (= 1 (cl-count ":BEAD_ID: a-1" (split-string out "\n") :test #'equal)))))
      (delete-file file))))

;;; ─── A9: dry-run projection schema + same-intent equality ──────────────────

;; The projection contract this SPEC pins (test-first): for a (:property K) set on
;; the a-1 heading, the single projection's owned region is the FULL heading section
;; (heading line through the byte before the next heading); :before is the source
;; bytes, :after the source-preserving result, :change-kind is `property', :provision
;; is nil, and :target is the canonical `dorg-api-target-token' of the projected host.
(defconst dorg-test-idp--a-1-section
  "* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nBody.\n"
  "Exact owned source region of the a-1 heading in `dorg-test-idp--prop-fixture'.")

(defconst dorg-test-idp--a-1-section-after
  "* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   DONE\n:END:\nBody.\n"
  "The a-1 section after a source-preserving STATUS=DONE (only the value changes).")

(ert-deftest dorg-test-idp-dry-run-projections-schema ()
  "A public dry-run returns exactly one `:projections' entry with LITERAL exact
values: canonical `:target' token, full owned `:before'/`:after' byte regions that
differ, `:provision' exactly nil, `:change-kind' exactly `property'; the response
carries the D5 public `:base' freshness token; and no write occurs."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID")))
         ;; independent expected :base built BEFORE dry-run (D5 public schema);
         ;; carries the exact content :fingerprint (tracked-case content CAS).
         (exp-base (list :kind 'file :file file
                         :mtime (file-attribute-modification-time (file-attributes file))
                         :fingerprint (dorg-mutate--persist-file-fingerprint file))))
    (unwind-protect
        ;; API call INSIDE unwind-protect so intended-RED errors still clean up.
        (let* ((resp (dorg-api-execute
                      '(on (key "a-1") (set (property "STATUS") "DONE"))
                      :mode 'dry-run :source src))
               (projs (plist-get resp :projections)))
          (should (= 1 (length projs)))
          ;; LITERAL whole-plist in the EXACT D5 field order (order-sensitive).
          (should (equal (list :target "key:BEAD_ID=a-1" :change-kind 'property
                               :before dorg-test-idp--a-1-section
                               :after dorg-test-idp--a-1-section-after
                               :provision nil)
                         (car projs)))
          (let ((p (car projs)))
            ;; EXACT plist schema: precisely these keys, no extras.
            (should (equal '(:after :before :change-kind :provision :target)
                           (dorg-test-idp--plist-keys p)))
            ;; LITERAL canonical token (pinned, not derived through code-under-test)
            (should (equal "key:BEAD_ID=a-1" (plist-get p :target)))
            (should (equal dorg-test-idp--a-1-section (plist-get p :before)))
            (should (equal dorg-test-idp--a-1-section-after (plist-get p :after)))
            (should-not (equal (plist-get p :before) (plist-get p :after)))
            (should (eq nil (plist-get p :provision)))
            (should (eq 'property (plist-get p :change-kind))))
          ;; D5 public freshness token `:base' equals the independently-built value
          (should (equal exp-base (plist-get resp :base)))
          ;; dry-run performed no write
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

(defun dorg-test-idp--plist-keys (pl)
  "Return PL's keys sorted by symbol-name (to compare an exact key set)."
  (let (ks) (while pl (push (car pl) ks) (setq pl (cddr pl)))
       (sort ks (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

;; NONCANONICAL source order: b-2 appears BEFORE a-1 in the file, so canonical
;; projection ordering (a-1 then b-2) is a real transformation of source order.
(defconst dorg-test-idp--reversed-fixture
  "#+title: tasks\n\n* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:STATUS:   OPEN\n:END:\nBeta body.\n* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nBody.\n"
  "Two BEAD_ID headings in NONCANONICAL source order (b-2 first, a-1 second).")

(ert-deftest dorg-test-idp-dry-run-projections-multi-target ()
  "A9 multi-target: for a >=2-match dry-run over a NONCANONICAL-order fixture, the
ENTIRE ordered `:projections' list equals two LITERAL plists in CANONICAL token
order (a-1 then b-2) — pinning ordering, exact :target/:before/:after/:provision/
:change-kind values, AND rejecting any extra key — plus exact ordered `:targets'."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--reversed-fixture))
         (buf (find-file-noselect file))       ; live visiting buffer (query-ready)
         (src (list :file file :identity '(:property "BEAD_ID")))
         ;; EXACT D5 field order: (:target :change-kind :before :after :provision).
         (expected
          (list
           (list :target "key:BEAD_ID=a-1" :change-kind 'property
                 :before "* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   OPEN\n:END:\nBody.\n"
                 :after  "* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   DONE\n:END:\nBody.\n"
                 :provision nil)
           (list :target "key:BEAD_ID=b-2" :change-kind 'property
                 :before "* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:STATUS:   OPEN\n:END:\nBeta body.\n"
                 :after  "* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:STATUS:   DONE\n:END:\nBeta body.\n"
                 :provision nil))))
    (unwind-protect
        ;; API call INSIDE unwind-protect so an (intentionally-RED) error still runs
        ;; buffer/file cleanup.
        (let ((resp (dorg-api-execute
                     '(on (where (or (property "BEAD_ID" "a-1") (property "BEAD_ID" "b-2")))
                          (set (property "STATUS") "DONE"))
                     :mode 'dry-run :source src)))
          ;; entire ordered projections list equals the two literal plists
          (should (equal expected (plist-get resp :projections)))
          ;; exact ordered :targets as LITERAL shapes (canonical order, independent
          ;; of tokenizer behavior); projection :target already pins canonical tokens
          (should (equal '((:extern :key "BEAD_ID" :value "a-1")
                           (:extern :key "BEAD_ID" :value "b-2"))
                         (plist-get resp :targets)))
          (should (equal dorg-test-idp--reversed-fixture (dorg-test-idp--slurp file))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

(defconst dorg-test-idp--prop-fixture-after
  "#+title: tasks\n\n* TODO Alpha  :work:\n:PROPERTIES:\n:BEAD_ID:  a-1\n:STATUS:   DONE\n:END:\nBody.\n* TODO Beta  :misc:\n:PROPERTIES:\n:BEAD_ID:  b-2\n:END:\nBeta body.\n"
  "Exact whole-file result of the source-preserving STATUS=DONE on a-1 (only the
STATUS value changes; every other byte identical to `dorg-test-idp--prop-fixture').")

(ert-deftest dorg-test-idp-dry-run-apply-same-intent ()
  "Same normalized intent, pinned to LITERAL approved-IR values (not derived through
the normalizer under test): dry-run carries the exact typed patches,
target set, canonical token, and the D5 public `:base' freshness token; those are
forwarded to apply as `:expected-targets' + `:base' (CAS), and apply writes the
EXACT whole-file expected bytes.  Unbounded `(where PRED)' selector."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (buf (find-file-noselect file))       ; live visiting buffer (query-ready)
         (form '(on (where (property "BEAD_ID" "a-1")) (set (property "STATUS") "DONE")))
         (src (list :file file :identity '(:property "BEAD_ID")))
         ;; LITERAL approved expectations (Ctl-Cdx A9 LITERAL FIXTURES)
         (exp-patches '((:op put :target (:property :host (:heading :extern t :key "BEAD_ID" :value "a-1") :name "STATUS") :value "DONE")))
         (exp-targets '((:extern :key "BEAD_ID" :value "a-1")))
         ;; independent expected :base built BEFORE dry-run (no derivation from resp).
         ;; A live buffer visits the source, so the PUBLIC base is the buffer token
         ;; (B2: public base reflects actual resolved source state), NOT file-mtime.
         (exp-base (list :kind 'buffer :buffer (buffer-name buf) :file file
                         :tick (buffer-chars-modified-tick buf)
                         :fingerprint (dorg-mutate--source-buffer-fingerprint buf))))
    (unwind-protect
        ;; API calls INSIDE unwind-protect so an (intentionally-RED) error still runs
        ;; buffer/file cleanup.
        (let ((dry (dorg-api-execute form :mode 'dry-run :source src)))
          ;; dry-run carries the exact literal intent + D5 :base
          (should (equal exp-patches (plist-get dry :patches)))
          (should (equal exp-targets (plist-get dry :targets)))
          (should (equal "key:BEAD_ID=a-1"
                         (dorg-api-target-token (car (plist-get dry :targets)))))
          (should (equal exp-base (plist-get dry :base)))
          ;; forward :base + :expected-targets to apply (CAS path)
          (let ((app (dorg-api-execute form :mode 'apply :source src
                                       :base exp-base :save t
                                       :expected-targets (plist-get dry :targets))))
            (should (plist-get app :ok))
            (should (equal exp-base (plist-get app :base)))
            (should (equal (plist-get dry :base) (plist-get app :base)))
            (should (equal exp-patches (plist-get app :patches)))
            (should (equal exp-targets (plist-get app :targets)))
            ;; apply wrote the EXACT whole-file expected bytes
            (should (equal dorg-test-idp--prop-fixture-after (dorg-test-idp--slurp file)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file file))))

(ert-deftest dorg-test-idp-apply-refuses-stale-unsaved-buffer-edit ()
  "B2 base freshness: the PUBLIC `:base' token reflects the ACTUAL resolved source
state, INCLUDING a live visiting buffer.  When an unsaved buffer edit intervenes
between dry-run and apply, apply REFUSES with the EXACT `stale'/`base-revision'
BEFORE any write — the file is byte-unchanged and the unrelated unsaved edit is NOT
persisted.  (At f0c1bec the public base was file-mtime-only, so apply false-applied
and `:save t' persisted the sneaky edit — this is the regression guard.)"
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--dorgid-fixture))
         (buf (find-file-noselect file))
         (form '(on (id 100) (set (property "STATUS") "DONE")))
         (src (list :file file :identity 'dorg)))
    (unwind-protect
        (let ((dry (dorg-api-execute form :mode 'dry-run :source src)))
          (should (plist-get dry :ok))
          ;; user makes an UNSAVED edit to the visiting buffer (unrelated bytes)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "* Sneaky unsaved heading\n"))
          ;; apply with the now-stale dry-run base + :save t must REFUSE, exactly
          (should (equal '(stale . base-revision)
                         (dorg-test-idp--any-reason+detail
                          (lambda ()
                            (dorg-api-execute form :mode 'apply :source src
                                              :base (plist-get dry :base) :save t)))))
          ;; nothing written to disk: file byte-identical to the fixture
          (should (equal dorg-test-idp--dorgid-fixture (dorg-test-idp--slurp file)))
          ;; the unsaved buffer edit was NOT persisted (buffer still modified)
          (should (buffer-modified-p buf)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file file))))

(ert-deftest dorg-test-idp-dry-run-projections-zero-match-present-nil ()
  "An ELIGIBLE (supported, readable plain-org) dry-run that matches ZERO targets
carries a PRESENT `:projections' field whose value is nil — distinct from an
INAPPLICABLE preview, which omits the field — alongside nil `:targets', the public
`:base' file token, and a byte-unchanged source."
  (let* ((file (dorg-test-idp--tmpfile dorg-test-idp--prop-fixture))
         (src (list :file file :identity '(:property "BEAD_ID")))
         ;; full public file token built BEFORE the dry-run (source is unchanged);
         ;; carries the exact content :fingerprint (tracked-case content CAS).
         (exp-base (list :kind 'file :file file
                         :mtime (file-attribute-modification-time
                                 (file-attributes file))
                         :fingerprint (dorg-mutate--persist-file-fingerprint file))))
    (unwind-protect
        (let ((resp (dorg-api-execute
                     '(on (where (property "BEAD_ID" "no-such-value"))
                          (set (property "STATUS") "DONE"))
                     :mode 'dry-run :source src)))
          (should (plist-get resp :ok))
          (should (null (plist-get resp :targets)))
          ;; PRESENT field, value nil (NOT omitted).
          (should (plist-member resp :projections))
          (should (null (plist-get resp :projections)))
          ;; FULL public base token pinned (shape + exact file + exact mtime).
          (should (equal exp-base (plist-get resp :base)))
          ;; source bytes unchanged.
          (should (equal dorg-test-idp--prop-fixture (dorg-test-idp--slurp file))))
      (delete-file file))))

;;; ─── A10: package bundle — plain-file entry loads Dolt-free (fresh process) ─

(defun dorg-test-idp--bundle-isolation-output (force-interpreted &optional trailing-form)
  "Spawn a FRESH Emacs (-Q -L ROOT), require `dorg-api', run a plain-org dry-run +
apply, and return the child stdout reporting any DIRTY forbidden feature plus a
RANBOTH marker.  When FORCE-INTERPRETED, the child must load DOrg from
UNCOMPILED source — `eval-when-compile'-style eager pulls cannot then hide
behind a compiled-file no-op — and PROVES it: after the operations it reports
the origin of every loaded dorg feature (`SRC:' lines) and emits `BADSRC:'
for any that did not come from a .el file.  Source loading is asserted, not
forced through `load-suffixes': a global suffix pin also forces Emacs's own
Lisp through source-only loading, which on installations shipping only
.el.gz + .elc (Nix) recurses into jka-compr.  `load-prefer-newer' still
biases toward a newer .el when both files exist.  The child must exit 0:
its status is asserted with the captured output retained, so markers alone
cannot certify a child that died after printing them.  TRAILING-FORM, when
given, is appended after the markers; it exists only so a test can prove
that a nonzero exit is rejected."
  (let* ((root (directory-file-name
                (or (locate-dominating-file default-directory "dorg-api.el")
                    dorg-test-idp--repo-root)))
         (script (make-temp-file "dorg-idp-bundle-" nil ".el"))
         (target (make-temp-file "dorg-idp-bundle-src-" nil ".org")))
    (should (file-exists-p (expand-file-name "dorg-api.el" root)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "* H\n:PROPERTIES:\n:DORG_ID: 5\n:END:\nbody\n"))
          (with-temp-file script
            (insert (format "%S"
                            `(progn
                               ,@(when force-interpreted
                                   '((setq load-prefer-newer t)))
                               (require 'dorg-api)
                               (let ((src (list :file ,target :identity 'dorg)))
                                 (dorg-api-execute '(on (id 5) (set (property "K") "V"))
                                                   :mode 'dry-run :source src)
                                 (dorg-api-execute '(on (id 5) (set (property "K") "V"))
                                                   :mode 'apply :source src))
                               ;; Compute the forbidden set AFTER dry-run+apply so a
                               ;; feature lazily loaded by those ops is still caught.
                               (let ((dirty
                                      (mapconcat
                                       (lambda (f) (if (featurep f) (format "DIRTY:%s " f) ""))
                                       '(dorg-dolt dorg-repo dorg-dolt-server
                                         dorg-git-sync dorg-ql-dolt) "")))
                                 (princ (concat dirty "|RANBOTH")))
                               ,@(when force-interpreted
                                   '((dolist (f features)
                                       (when (string-prefix-p "dorg" (symbol-name f))
                                         (let ((file (symbol-file f 'provide)))
                                           (princ (format " %s:%s=%s"
                                                          (if (and file (string-suffix-p ".el" file))
                                                              "SRC" "BADSRC")
                                                          f (and file (file-name-nondirectory file)))))))))
                               ,@(when trailing-form (list trailing-form))))))
          (with-temp-buffer
            (let* ((status (call-process "emacs" nil t nil
                                         "-Q" "--batch" "-L" root "-l" script))
                   (out (buffer-string)))
              (ert-info ((format "child exit status %S; output: %s" status out))
                (should (eql 0 status)))
              out)))
      (delete-file script)
      (delete-file target))))

(defun dorg-test-idp--bundle-compiled-output ()
  "Like `dorg-test-idp--bundle-isolation-output' but GUARANTEES a genuine COMPILED
\(.elc) origin for `dorg-api' that survives `make test' clean-elc.

`make test' runs `clean-elc' first (it deletes every repo `*.elc'), so a child
launched with `-L ROOT' would silently `require' the .el SOURCE — making the
\"compiled\" variant a duplicate of the forced-.el sibling.  Here we instead
byte-compile the whole repo Elisp surface into an ISOLATED build dir (a temp copy,
untouched by the repo `clean-elc'), spawn a fresh Emacs whose ONLY load-path entry
is that dir, and — BEFORE `require' and the plain-org dry-run/apply — EMIT the
`locate-library' resolution and (after require) the `symbol-file' origin of
`dorg-api-execute'.  The caller asserts both end in .elc; if the isolated compile
step were removed the build dir would hold only .el and both premises would fail."
  (let* ((root (directory-file-name
                (or (locate-dominating-file default-directory "dorg-api.el")
                    dorg-test-idp--repo-root)))
         (builddir (make-temp-file "dorg-idp-bundle-build-" t))
         (script (make-temp-file "dorg-idp-bundle-c-" nil ".el"))
         (target (make-temp-file "dorg-idp-bundle-csrc-" nil ".org")))
    (should (file-exists-p (expand-file-name "dorg-api.el" root)))
    (unwind-protect
        (progn
          ;; Isolated compiled tree: copy every repo top-level .el into BUILDDIR
          ;; and byte-compile THE WHOLE SET there, in a fresh child, so the
          ;; artifacts are compiled and the repo `clean-elc' cannot revert the
          ;; child's sources to .el.  The compile is FAIL-CLOSED: it runs with the
          ;; available vendored Clime and mysql.el dependencies on the load path (the
          ;; full top-level set needs both — `make compile' puts only clime on the
          ;; path, but mysql.el is required to compile the Dolt/sql modules)
          ;; so the full set compiles cleanly, and we assert the child's exit
          ;; status is exactly 0 — a partially compiled tree is NOT accepted.
          (dolist (el (directory-files root t "\\.el\\'"))
            (copy-file el (expand-file-name (file-name-nondirectory el) builddir) t))
          (let* ((clime (expand-file-name "lib/clime" root))
                 (mysql (expand-file-name "lib/mysql" root))
                 (lpaths (append (list "-L" builddir)
                                 (when (file-exists-p (expand-file-name "clime.el" clime))
                                   (list "-L" clime))
                                 (when (file-exists-p (expand-file-name "mysql.el" mysql))
                                   (list "-L" mysql))))
                 (status (apply #'call-process "emacs" nil nil nil "-Q" "--batch"
                                (append lpaths
                                        (list "--eval" "(setq byte-compile-warnings nil)"
                                              "-f" "batch-byte-compile")
                                        (directory-files builddir t "\\.el\\'")))))
            ;; Fail closed on the chosen compilation set: the isolated bundle must
            ;; compile CLEANLY (exit 0), not lean on a tolerated partial tree.
            (should (equal status 0)))
          (should (file-exists-p (expand-file-name "dorg-api.elc" builddir)))
          (with-temp-file target
            (insert "* H\n:PROPERTIES:\n:DORG_ID: 5\n:END:\nbody\n"))
          (with-temp-file script
            (insert (format "%S"
                            `(progn
                               ;; PREMISE, part 1: resolution BEFORE require must
                               ;; pick .elc (compiled preferred over .el).
                               (princ (format "PREMISE-LOCATE:%s "
                                              (locate-library "dorg-api")))
                               (require 'dorg-api)
                               ;; PREMISE, part 2: what actually got LOADED — the
                               ;; recorded defining file of the executor entry —
                               ;; must be the .elc, not the .el.
                               (princ (format "PREMISE-SYMFILE:%s "
                                              (symbol-file 'dorg-api-execute 'defun)))
                               (let ((src (list :file ,target :identity 'dorg)))
                                 (dorg-api-execute '(on (id 5) (set (property "K") "V"))
                                                   :mode 'dry-run :source src)
                                 (dorg-api-execute '(on (id 5) (set (property "K") "V"))
                                                   :mode 'apply :source src))
                               ;; Compute the forbidden set AFTER dry-run+apply so a
                               ;; feature lazily loaded by those ops is still caught.
                               (let ((dirty
                                      (mapconcat
                                       (lambda (f) (if (featurep f) (format "DIRTY:%s " f) ""))
                                       '(dorg-dolt dorg-repo dorg-dolt-server
                                         dorg-git-sync dorg-ql-dolt) "")))
                                 (princ (concat dirty "|RANBOTH")))))))
          (with-temp-buffer
            (call-process "emacs" nil t nil "-Q" "--batch" "-L" builddir "-l" script)
            (buffer-string)))
      (delete-file script)
      (delete-file target)
      (delete-directory builddir t))))

(ert-deftest dorg-test-idp-bundle-plain-file-dolt-free ()
  "COMPILED fresh process: (require 'dorg-api) + plain-org dry-run/apply loads the
plain-file surface from an ISOLATED .elc bundle WITHOUT pulling any
Dolt/sync/server feature.  Self-contained under normal `make test': the compiled
artifacts are built in a temp dir that `clean-elc' does not touch, so the
compiled-origin premise is real, not silently downgraded to .el.  The premise is
asserted explicitly — `locate-library' and the loaded `symbol-file' of
`dorg-api-execute' must both end in .elc — so removing the isolated-compile step
fails this test (it would leave only .el)."
  (let ((out (dorg-test-idp--bundle-compiled-output)))
    (should (string-match-p "PREMISE-LOCATE:[^ ]*\\.elc " out))
    (should (string-match-p "PREMISE-SYMFILE:[^ ]*\\.elc " out))
    (should (string-match-p "|RANBOTH" out))
    (should-not (string-match-p "DIRTY:" out))))

(ert-deftest dorg-test-idp-bundle-plain-file-source-load ()
  "INTERPRETED (uncompiled source) fresh process — the SAME isolation, forcing .el
loading so the acceptance gate holds on an uncompiled checkout, not only .elc (an
`eval-when-compile' require would DIRTY this even while the compiled variant stays
clean)."
  (let ((out (dorg-test-idp--bundle-isolation-output t)))
    (should (string-match-p "|RANBOTH" out))
    (should-not (string-match-p "DIRTY:" out))
    ;; Source origin is asserted, not assumed: every loaded dorg feature must
    ;; report a .el origin, and at least the entry feature must be present.
    (should (string-match-p " SRC:dorg-api=dorg-api\\.el" out))
    (should-not (string-match-p "BADSRC:" out))))

(ert-deftest dorg-test-idp-bundle-child-nonzero-exit-rejected ()
  "A child that prints every expected marker and then exits nonzero is REJECTED.
Proves the exit-status assertion has teeth: markers alone never certify a
child that died after printing them."
  (should-error
   (dorg-test-idp--bundle-isolation-output t '(kill-emacs 3))
   :type 'ert-test-failed))

(defun dorg-test-idp--resolved-struct-lazy-output (dolt-dir dispatch)
  "Fresh child (-Q, parent load-path): require `dorg-api', resolve an already-built
Dolt `dorg-mutate-source' at DOLT-DIR (bypassing construction), run DISPATCH
\(`plan' or `apply') on the STRUCT, and return `PRE:<f> AFTER-RESOLVE:<f>
AFTER:<dolt>,<model-load> |OK'.  The probed pair is the dispatch-time lazy
backend set (`dorg-dolt' + `dorg-model-load').  The patch
is `ignore-errors'-wrapped so a missing/empty repo cannot mask the pre-helper
lazy load the DISPATCH ensure performs."
  (let* ((script (make-temp-file "dorg-idp-struct-" nil ".el"))
         (lp load-path)
         (call (if (eq dispatch 'plan)
                   'dorg-mutate--plan-patches
                 'dorg-mutate--apply-patches)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert (format "%S"
                            `(progn
                               (require 'dorg-api)
                               (princ (format "PRE:%s " (featurep 'dorg-dolt)))
                               (let ((s (dorg-mutate--resolve-source
                                         (list :dolt-dir ,dolt-dir :root-id 1))))
                                 (princ (format "AFTER-RESOLVE:%s " (featurep 'dorg-dolt)))
                                 (ignore-errors
                                   (,call s
                                          '((:op put
                                             :target (:property
                                                      :host (:heading :id 1)
                                                      :name "K")
                                             :value "V"))
                                          nil))
                                 (princ (format "AFTER:%s,%s |OK"
                                                (featurep 'dorg-dolt) (featurep 'dorg-model-load))))))))
          (with-temp-buffer
            (apply #'call-process "emacs" nil t nil "-Q" "--batch"
                   (append (cl-loop for d in lp append (list "-L" d))
                           (list "-l" script)))
            (buffer-string)))
      (delete-file script))))

(ert-deftest dorg-test-idp-bundle-resolved-struct-dolt-plan-lazy-load ()
  "PLAN dispatch is INDEPENDENTLY load-bearing: with an EXISTING dolt-dir (so
`dorg-mutate--dolt-plan-data' passes its directory guard and reaches
`dorg-mutate--ensure-dolt-backend'), a resolved-STRUCT typed plan loads the
Dolt backend.  Fresh process: no Dolt after require/resolve, loaded after PLAN
alone — deleting the plan-dispatch ensure fails this test."
  (let ((dir (make-temp-file "dorg-b8-plandir-" t)))
    (unwind-protect
        (let ((out (dorg-test-idp--resolved-struct-lazy-output dir 'plan)))
          (should (string-match-p "PRE:nil AFTER-RESOLVE:nil " out))
          (should (string-match-p "AFTER:t,t |OK" out)))
      (delete-directory dir t))))

(ert-deftest dorg-test-idp-bundle-resolved-struct-dolt-apply-lazy-load ()
  "APPLY dispatch is INDEPENDENTLY load-bearing: with a NONEXISTENT dolt-dir (so the
plan guard returns BEFORE its ensure and cannot load), a resolved-STRUCT
typed apply still loads the Dolt backend via the apply dolt-branch ensure.
Fresh process: no Dolt after require/resolve, loaded after APPLY alone — deleting
the apply-dispatch ensure fails this test."
  (let ((out (dorg-test-idp--resolved-struct-lazy-output "/nonexistent-b8/.dolt/" 'apply)))
    (should (string-match-p "PRE:nil AFTER-RESOLVE:nil " out))
    (should (string-match-p "AFTER:t,t |OK" out))))

;;; ─── Public drawer-line insertion (tracked-regression) ───────────────────

(ert-deftest dorg-test-bnsmz-faithful-dry-run-apply-byte-exact ()
  "External-key dry-run previews the exact surgical drawer edit, then apply
writes those bytes and nothing else.  The heading/tag spacing, planning line,
aligned identity property, prior drawer line, blank run, and prose are witnesses
against a whole-section render."
  (let* ((before (concat "* TODO Alpha     :work:\n"
                         "SCHEDULED: <2026-08-14 Fri>\n"
                         ":PROPERTIES:\n:BEAD_ID:  bead-001\n:END:\n"
                         ":LOGBOOK:\n- old\n:END:\n\n"
                         "Body.\n"))
         (after (concat "* TODO Alpha     :work:\n"
                        "SCHEDULED: <2026-08-14 Fri>\n"
                        ":PROPERTIES:\n:BEAD_ID:  bead-001\n:END:\n"
                        ":LOGBOOK:\n- new\n- old\n:END:\n\n"
                        "Body.\n"))
         (file (dorg-test-faithful--tmpfile before))
         (source (dorg-test-faithful--source file))
         (form '(on (key "bead-001")
                    (insert (drawer-line "LOGBOOK" (occurrence 1)) (before-first) "- new"))))
    (unwind-protect
        (let* ((preview (dorg-api-execute form :mode 'dry-run :source source))
               (projection (car (plist-get preview :projections))))
          (should (equal "key:BEAD_ID=bead-001" (plist-get projection :target)))
          (should (eq 'item (plist-get projection :change-kind)))
          (should (equal before (plist-get projection :before)))
          (should (equal after (plist-get projection :after)))
          (should (equal before (dorg-test-faithful--slurp file)))
          (dorg-api-execute form :mode 'apply :source source)
          (should (equal after (dorg-test-faithful--slurp file))))
      (dorg-test-faithful--kill-visiting file)
      (delete-file file))))

(provide 'dorg-faithful-tests)
;;; dorg-faithful-tests.el ends here
