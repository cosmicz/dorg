;;; dorg-materialize-lifecycle-tests.el --- Materialize lifecycle SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; SPEC (RED-first) for the atomic exact-revision + window materialize
;; lifecycle contract (tracked-regression).  These tests define the ONE public
;; buffer-mutating lifecycle command
;;
;;   (dorg-materialize [&optional buffer] [:source source]
;;                     [:revision revision] [:window window])
;;
;; and the ONE public observation seam
;;
;;   (dorg-materialized-view-scope BUFFER) -> nil | whole | partial
;;
;; The original twelve tests were authored RED-first.  Implementation-stage
;; obligations extend them with descriptor, keyword-first, negative-path, and
;; post-replacement rollback coverage.
;;
;; Non-vacuity discipline (see Acceptance Criteria on the bead):
;;   - EXACT-REVISION tests advance HEAD to a DIFFERENT snapshot (B/Y) and
;;     materialize an EARLIER revision (A/X).  Substituting current HEAD for
;;     the supplied revision makes the buffer/hash carry B/Y and FAILS them.
;;   - Deleting the materialization call leaves the destination buffer empty
;;     of the expected snapshot and FAILS them.
;;   - The window test folds the tracked-regression defect: the current renderer
;;     stamps DORG_HASH from HEAD while reading rows AS OF the argument, so a
;;     window materialized AT A while HEAD is B must label A, not B.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-parse)
(require 'dorg-render)
(require 'dorg-dolt)
(require 'dorg-projection)
(require 'dorg-test-helpers)
(require 'dorg-mode)

(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el

;;; ─── Fixtures ────────────────────────────────────────────────────────

(defun dorg-mlt--id (dolt-dir title)
  "Return the heading id with TITLE in DOLT-DIR."
  (alist-get 'id (car (dorg-sql dolt-dir
                                "SELECT id FROM headings WHERE title = %s"
                                (dorg-sql-quote title)))))

(defmacro dorg-mlt--with-registered (temp-var dolt-var file-var reg-var root-var
                                             &rest body)
  "Bind a fresh Dolt store to a visited org file and run BODY.
TEMP-VAR/DOLT-VAR come from `dorg-test--with-temp-db-and-dir'.  FILE-VAR is a
doc.org path under TEMP-VAR, REG-VAR its `dorg-bind-dolt' registration, and
ROOT-VAR the registered root id.  The registry is isolated per test."
  (declare (indent 5) (debug (symbolp symbolp symbolp symbolp symbolp body)))
  `(dorg-test--with-temp-db-and-dir ,temp-var ,dolt-var
     (let* ((,file-var (expand-file-name "doc.org" ,temp-var))
            (dorg--registry nil)
            (,reg-var (dorg-bind-dolt ,file-var ,dolt-var))
            (,root-var (dorg-backend-root-id
                        (dorg-registration-backend ,reg-var))))
       (ignore ,reg-var ,root-var)
       ,@body)))

(defun dorg-mlt--seed (dolt-dir root-id contents message)
  "Seed CONTENTS (org text) under ROOT-ID in DOLT-DIR and commit MESSAGE.
Returns the resulting HEAD commit token."
  (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql contents root-id t))
  (dorg-commit dolt-dir message)
  (dorg-head-commit dolt-dir))

(defun dorg-mlt--view-values (view)
  "Return an inert copy of VIEW as test evidence."
  (copy-tree view))

(defun dorg-mlt--lifecycle-snapshot (buffer)
  "Capture BUFFER bytes and every materialize lifecycle field."
  (with-current-buffer buffer
    (list :bytes (buffer-substring-no-properties (point-min) (point-max))
          :point (point)
          :modified (buffer-modified-p)
          :read-only buffer-read-only
          :programmatic-p (dorg-projection-programmatic-change-p)
          :view-values
          (dorg-mlt--view-values (dorg-projection-view-context buffer)))))

(defun dorg-mlt--should-match-lifecycle-snapshot (buffer snapshot)
  "Assert BUFFER exactly matches lifecycle SNAPSHOT."
  (with-current-buffer buffer
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   (plist-get snapshot :bytes)))
    (should (= (point) (plist-get snapshot :point)))
    (should (eq (buffer-modified-p) (plist-get snapshot :modified)))
    (should (eq buffer-read-only (plist-get snapshot :read-only)))
    (should (eq (dorg-projection-programmatic-change-p)
                (plist-get snapshot :programmatic-p)))
    (should (equal (dorg-mlt--view-values
                    (dorg-projection-view-context buffer))
                   (plist-get snapshot :view-values)))))

;;; ─── AC1 — one compatible public entry ───────────────────────────────

(ert-deftest dorg-test-mlt-materialize-keyword-arity ()
  "`dorg-materialize' admits optional BUFFER plus :source/:revision/:window.
Calling with each keyword must NOT raise `wrong-number-of-arguments' — the
legacy single-argument signature would.  Any OTHER error (an unregistered temp
buffer) proves the keyword surface parsed.  Legacy zero/one-positional calls
remain the same public entry."
  (should (fboundp 'dorg-materialize))
  (cl-flet ((admits (&rest args)
              ;; t when the lambda-list accepted ARGS (whatever downstream
              ;; error follows); nil only on an arity rejection.
              (condition-case _
                  (progn (apply #'dorg-materialize args) t)
                (wrong-number-of-arguments nil)
                (error t))))
    (with-temp-buffer
      (should (admits))                                 ; legacy zero-arg
      (should (admits (current-buffer)))                ; legacy one-arg
      (should (admits (current-buffer) :source (current-buffer)))
      (should (admits (current-buffer) :revision "deadbeef"))
      (should (admits (current-buffer) :window '(:focus (1))))
      (should (admits :revision "deadbeef")))))          ; keyword-first

;;; ─── AC3 — exact whole snapshot, not HEAD ────────────────────────────

(ert-deftest dorg-test-mlt-exact-whole-not-head ()
  "Materializing revision A shows A/X even though HEAD advanced to B/Y.
Transformation proof: HEAD carries Beta; asserting Alpha present AND Beta
absent fails if the impl substitutes current HEAD for the supplied revision."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed dolt-dir root-id "* Alpha\nalpha body\n" "A")))
        ;; Advance HEAD to B/Y.
        (dorg-sql-batch dolt-dir
          (list "UPDATE headings SET title = 'Beta' WHERE title = 'Alpha'"))
        (dorg-commit dolt-dir "B")
        (should-not (equal token-a (dorg-head-commit dolt-dir)))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((result (dorg-materialize (current-buffer) :revision token-a)))
            (should (eq (plist-get result :scope) 'whole))
            (should (equal (plist-get result :revision) token-a))
            (should (= (plist-get result :root-id) root-id))
            (should-not (plist-get result :window))
            (should (eq (plist-get result :buffer) (current-buffer))))
          (should (string-match-p "^\\* Alpha" (buffer-string)))
          (should-not (string-match-p "Beta" (buffer-string)))
          ;; Exact hash + descriptor + scope describe A.
          (should (string-match-p
                   (format "^#\\+DORG_HASH: %s" (regexp-quote token-a))
                   (buffer-string)))
          (should (eq (dorg-materialized-view-scope (current-buffer)) 'whole)))))))

(ert-deftest dorg-test-mlt-supplied-revision-zero-head-reads ()
  "Supplying a revision performs ZERO HEAD reads after entry.
Any post-entry `dorg-head-commit' means the impl re-derived HEAD instead of
threading the supplied token."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed dolt-dir root-id "* Alpha\n" "A")))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let* ((calls 0)
                 (real (symbol-function 'dorg-head-commit)))
            (cl-letf (((symbol-function 'dorg-head-commit)
                       (lambda (&rest args) (cl-incf calls) (apply real args))))
              (dorg-materialize (current-buffer) :revision token-a))
            (should (= calls 0))))))))

;;; ─── AC2 — registered source, separate destination ───────────────────

(ert-deftest dorg-test-mlt-source-into-fresh-destination ()
  "A registered :source resolves store/root; a fresh destination receives bytes.
The source buffer is not mutated; the store/root come only from :source."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed dolt-dir root-id "* Alpha\n" "A")))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((source (current-buffer)))
            (with-temp-buffer
              (let ((dest (current-buffer)))
                (org-mode)
                ;; Keyword-first form must default destination to current
                ;; buffer while resolving storage only from SOURCE.
                (let ((result
                       (dorg-materialize :source source :revision token-a)))
                  (should (eq (plist-get result :buffer) dest)))
                ;; Destination got the view.
                (should (string-match-p "^\\* Alpha" (buffer-string)))
                (should (eq (dorg-materialized-view-scope dest) 'whole))
                (let ((context (dorg-projection-view-context dest)))
                  (should (eq (plist-get context :source) source))
                  (should (= (plist-get context :root-id) root-id))
                  (should (equal (plist-get context :revision) token-a)))
                ;; Source buffer bytes untouched (still empty/unrendered).
                (with-current-buffer source
                  (should-not (string-match-p "Alpha" (buffer-string))))))))))))

;;; ─── AC4 — exact bounded window, not split snapshots ─────────────────

(ert-deftest dorg-test-mlt-window-exact-revision-not-head ()
  "A bounded :window materialized AT A labels A, not HEAD B (tracked-regression).
Focus B: ancestor A headline + B/C subtree present, peers D/E absent, and the
durable window hash identifies A even though HEAD advanced to B."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed
                      dolt-dir root-id
                      "* A\nA ancestor body.\n** B\n*** C\n** D\n* E\n" "A")))
        (let ((b (dorg-mlt--id dolt-dir "B")))
          ;; Advance HEAD with an unrelated change so HEAD != token-a.
          (dorg-sql-batch dolt-dir
            (list "UPDATE headings SET title = 'E2' WHERE title = 'E'"))
          (dorg-commit dolt-dir "B-commit")
          (should-not (equal token-a (dorg-head-commit dolt-dir)))
          (with-temp-buffer
            (org-mode)
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (let* ((window (list :focus (list b)))
                   (result
                    (dorg-materialize (current-buffer)
                                      :revision token-a :window window)))
              (should (eq (plist-get result :scope) 'partial))
              (should (equal (plist-get result :revision) token-a))
              (should (= (plist-get result :root-id) root-id))
              (should (equal (plist-get result :window) window))
              (should (eq (plist-get result :buffer) (current-buffer)))
              (let ((context (dorg-projection-view-context)))
                (should (equal (plist-get context :revision) token-a))
                (should (equal (plist-get context :window) (list b)))))
            (let ((out (buffer-string)))
              ;; Bounded projection shape.
              (should (string-match-p "^\\* A$" out))
              (should-not (string-match-p "A ancestor body" out))
              (should (string-match-p "^\\*\\* B" out))
              (should (string-match-p "^\\*\\*\\* C" out))
              (should-not (string-match-p "^\\*\\* D" out))
              (should-not (string-match-p "^\\* E" out))
              ;; Durable window metadata identifies the EXACT revision A.
              (should (string-match-p "^#\\+DORG_WINDOW:" out))
              (should (string-match-p
                       (format "^#\\+DORG_HASH: %s" (regexp-quote token-a))
                       out)))
            (should (eq (dorg-materialized-view-scope (current-buffer))
                        'partial))))))))

(ert-deftest dorg-test-mlt-window-extern-identity-resolves-at-exact-revision ()
  "A public external identity focus resolves AS OF the requested revision.
At commit A, BEAD_ID=focus names the Focus subtree.  HEAD then changes that
property to focus-at-head.  Materializing A through the canonical public
external identity must still render Focus/Child, preserve the external target
in the descriptor and private view record, and encode only the resolved numeric
id in the durable private marker.  Resolving the identity at HEAD makes this
non-vacuous test fail as missing."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let* ((token-a
              (dorg-mlt--seed
               dolt-dir root-id
               (concat "* Parent\n"
                       ":PROPERTIES:\n:BEAD_ID: parent\n:END:\n"
                       "** Focus\n"
                       ":PROPERTIES:\n:BEAD_ID: focus\n:END:\n"
                       "*** Child\n"
                       "** Peer\n"
                       ":PROPERTIES:\n:BEAD_ID: peer\n:END:\n")
               "identity A"))
             (focus-id (dorg-mlt--id dolt-dir "Focus"))
             (property-row
              (car
               (dorg-sql
                dolt-dir
                (concat
                 "SELECT pe.drawer_item_id FROM property_entries pe"
                 " JOIN heading_items hi ON pe.drawer_item_id = hi.id"
                 " JOIN headings h ON hi.heading_id = h.id"
                 " WHERE h.root_id = %d AND h.id = %d"
                 " AND pe.prop_key = 'BEAD_ID'")
                root-id focus-id)))
             (drawer-id (alist-get 'drawer_item_id property-row))
             (target '(:extern :key "BEAD_ID" :value "focus"))
             (window (list :focus (list target))))
        (should drawer-id)
        (dorg-sql-batch
         dolt-dir
         (list
          (format
           (concat "UPDATE property_entries SET prop_value = 'focus-at-head'"
                   " WHERE drawer_item_id = %d AND prop_key = 'BEAD_ID'")
           drawer-id)))
        (dorg-commit dolt-dir "identity HEAD")
        (should-not (equal token-a (dorg-head-commit dolt-dir)))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((source (current-buffer)))
            (with-temp-buffer
              (org-mode)
              (let ((result
                     (dorg-materialize
                      (current-buffer) :source source
                      :revision token-a :window window)))
                (should (equal (plist-get result :window) window))
                (should (equal
                         (plist-get (dorg-projection-view-context) :window)
                         (list target)))
                (should (string-match-p "^\\*\\* Focus" (buffer-string)))
                (should (string-match-p "^\\*\\*\\* Child" (buffer-string)))
                (should-not (string-match-p "^\\*\\* Peer" (buffer-string)))
                (should
                 (string-match-p
                  (format "^#\\+DORG_WINDOW: .*foci:%d .*head:%s"
                          focus-id (regexp-quote token-a))
                  (buffer-string)))))))))))

(ert-deftest dorg-test-mlt-window-extern-missing-duplicate-no-mutation ()
  "Missing and duplicate public external foci fail before lifecycle mutation.
The error classification must identify the property lookup outcome, rather than
rejecting every external identity at the window-shape boundary."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a
             (dorg-mlt--seed
              dolt-dir root-id
              (concat "* One\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n"
                      "* Two\n:PROPERTIES:\n:BEAD_ID: dup\n:END:\n")
              "identity duplicates")))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((source (current-buffer)))
            (with-temp-buffer
              (org-mode)
              (insert "SENTINEL external identity\n")
              (set-buffer-modified-p t)
              (let ((dest (current-buffer))
                    (before (dorg-mlt--lifecycle-snapshot (current-buffer))))
                (dolist
                    (case
                     '(((:extern :key "BEAD_ID" :value "missing")
                        . "no heading with BEAD_ID")
                       ((:extern :key "BEAD_ID" :value "dup")
                        . "2 headings match BEAD_ID")))
                  (let ((err
                         (should-error
                          (dorg-materialize
                           dest :source source :revision token-a
                           :window (list :focus (list (car case)))))))
                    (should (string-match-p
                             (regexp-quote (cdr case))
                             (error-message-string err)))
                    (dorg-mlt--should-match-lifecycle-snapshot
                     dest before)))))))))))

;;; ─── AC5 — stable explicit-buffer scope query ────────────────────────

(ert-deftest dorg-test-mlt-scope-transitions ()
  "`dorg-materialized-view-scope' returns nil -> whole -> partial by lifecycle.
It reads the SUPPLIED buffer and yields only the three stable symbols, never
an internal marker plist / record."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed
                      dolt-dir root-id "* A\n** B\n*** C\n** D\n* E\n" "A")))
        (let ((b (dorg-mlt--id dolt-dir "B")))
          (with-temp-buffer
            (org-mode)
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            ;; Before any materialization.
            (should (null (dorg-materialized-view-scope (current-buffer))))
            ;; Whole.
            (dorg-materialize (current-buffer) :revision token-a)
            (should (eq (dorg-materialized-view-scope (current-buffer)) 'whole))
            ;; Whole -> partial.
            (dorg-materialize (current-buffer)
                              :revision token-a :window (list :focus (list b)))
            (let ((scope (dorg-materialized-view-scope (current-buffer))))
              (should (eq scope 'partial))
              ;; Never a raw representation.
              (should (memq scope '(nil whole partial))))))))))

(ert-deftest dorg-test-mlt-raw-partial-view-retired ()
  "The raw `dorg-render-partial-view' variable is retired from the public API.
Consumers classify via `dorg-materialized-view-scope', not a marker plist."
  (should (fboundp 'dorg-materialized-view-scope))
  (should-not (boundp 'dorg-render-partial-view)))

;;; ─── AC6 — validate before mutation ──────────────────────────────────

(ert-deftest dorg-test-mlt-invalid-revision-no-mutation ()
  "An invalid revision signals BEFORE any destination byte changes.
A sentinel pre-fill survives and no materialized-view scope is installed."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (dorg-mlt--seed dolt-dir root-id "* Alpha\n" "A")
      (with-temp-buffer
        (org-mode)
        ;; Seed pre-existing wrong bytes before this buffer acquires the
        ;; registered Dolt file identity and therefore projection policy.
        (insert "SENTINEL not-a-view\n")
        (set-visited-file-name org-file t)
        (set-buffer-modified-p nil)
        (should-error
         (dorg-materialize (current-buffer) :revision "0000000000000000000000000000000000"))
        ;; Bytes untouched; no false view established.
        (should (string-match-p "SENTINEL not-a-view" (buffer-string)))
        (should (null (dorg-materialized-view-scope (current-buffer))))))))

(ert-deftest dorg-test-mlt-invalid-source-window-and-focus-no-mutation ()
  "Invalid source/window/focus inputs fail before changing destination state."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a
             (dorg-mlt--seed dolt-dir root-id "* Alpha\n" "A")))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((source (current-buffer)))
            (with-temp-buffer
              (org-mode)
              (insert "SENTINEL lifecycle\n")
              (set-buffer-modified-p t)
              (let ((dest (current-buffer))
                    (before (dorg-mlt--lifecycle-snapshot (current-buffer))))
                ;; An unregistered source has no storage identity.
                (with-temp-buffer
                  (org-mode)
                  (should-error
                   (dorg-materialize dest :source (current-buffer)
                                     :revision token-a)))
                (dorg-mlt--should-match-lifecycle-snapshot dest before)
                ;; A malformed window contract is rejected before rendering.
                (should-error
                 (dorg-materialize dest :source source
                                   :revision token-a :window '(:focus)))
                (dorg-mlt--should-match-lifecycle-snapshot dest before)
                ;; A well-shaped window whose focus does not exist is also a no-op.
                (should-error
                 (dorg-materialize dest :source source
                                   :revision token-a
                                   :window '(:focus (999999999))))
                (dorg-mlt--should-match-lifecycle-snapshot
                 dest before)))))))))

(ert-deftest dorg-test-mlt-plain-org-revision-window-refused-no-mutation ()
  "Plain-org sources refuse revision/window lifecycle without touching DEST."
  (let ((dorg--registry nil)
        (plain-file (make-temp-file "dorg-mlt-plain-" nil ".org" "* Plain\n")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name plain-file t)
          (set-buffer-modified-p nil)
          (let ((source (current-buffer)))
            (with-temp-buffer
              (org-mode)
              (insert "SENTINEL plain destination\n")
              (set-buffer-modified-p t)
              (let ((dest (current-buffer))
                    (before (dorg-mlt--lifecycle-snapshot (current-buffer))))
                (should-error
                 (dorg-materialize dest :source source :revision "token"))
                (dorg-mlt--should-match-lifecycle-snapshot dest before)
                (should-error
                 (dorg-materialize dest :source source
                                   :window '(:focus (1))))
                (dorg-mlt--should-match-lifecycle-snapshot dest before)))))
      (delete-file plain-file))))

;;; ─── AC7 — exact projection publication invariant ────────────────────

(ert-deftest dorg-test-mlt-whole-publishes-only-projection-state ()
  "Whole materialize replaces stale state and seals the exact projection.
The destination begins in the wrong state: stale bytes, modified, and
writable.  Deleting the materialize call or its lifecycle seal leaves at least
one postcondition false."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed dolt-dir root-id "* Alpha\n" "A")))
        (with-temp-buffer
          (org-mode)
          ;; Dirty/no-base buffer forces the full-render publish path.
          (insert "* stale local\n")
          (set-visited-file-name org-file t)
          (set-buffer-modified-p t)
          (dorg-projection-install-view
           (current-buffer) (current-buffer) root-id
           "stale-revision" 'partial '(99))
          (should (buffer-modified-p))
          (should-not buffer-read-only)
          (let ((before-bytes (buffer-string)))
            (dorg-materialize (current-buffer) :revision token-a)
            (should-not (equal before-bytes (buffer-string)))
            (should (string-match-p "^\\* Alpha" (buffer-string)))
            (should-not (buffer-modified-p))
            (should buffer-read-only)
            (let ((view (dorg-projection-view-context)))
              (should (equal (plist-get view :revision) token-a))
              (should (eq (plist-get view :scope) 'whole))
              (should (null (plist-get view :window))))))))))

;;; ─── AC8 — partial safety invariant ──────────────────────────────────

(ert-deftest dorg-test-mlt-partial-replaces-whole-projection-context ()
  "Whole -> partial replaces the established view with an exact bounded scope."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let ((token-a (dorg-mlt--seed
                      dolt-dir root-id "* A\n** B\n*** C\n** D\n* E\n" "A")))
        (let ((b (dorg-mlt--id dolt-dir "B")))
          (with-temp-buffer
            (org-mode)
            (set-visited-file-name org-file t)
            (set-buffer-modified-p nil)
            (dorg-materialize (current-buffer) :revision token-a)
            (let ((whole (dorg-projection-view-context)))
              (dorg-materialize (current-buffer)
                                :revision token-a :window (list :focus (list b)))
              (let ((partial (dorg-projection-view-context)))
                (should-not (equal whole partial))
                (should (eq (plist-get partial :scope) 'partial))
                (should (equal (plist-get partial :revision) token-a))
                (should (equal (plist-get partial :window) (list b)))))))))))

;;; ─── AC9 — post-replacement failure rolls back everything ───────────

(ert-deftest dorg-test-mlt-publication-failure-restores-whole-lifecycle ()
  "A failure after byte replacement restores bytes and projection lifecycle.
The injected failure runs after the new materialized-view record is installed,
which is after exact byte replacement.  Both whole and window paths must
restore the prior bytes, point, modification state, and projection view."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-mlt--with-registered temp dolt-dir org-file reg root-id
      (let* ((token-a
              (dorg-mlt--seed dolt-dir root-id "* Alpha\nalpha body\n" "A"))
             (alpha-id (dorg-mlt--id dolt-dir "Alpha")))
        (with-temp-buffer
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (dorg-materialize (current-buffer) :revision token-a)
          ;; Advance the target snapshot so replacement would materially
          ;; change the bytes and revision if rollback were absent.
          (dorg-sql-batch
           dolt-dir
           (list "UPDATE headings SET title = 'Beta' WHERE title = 'Alpha'"))
          (dorg-commit dolt-dir "B")
          (let ((token-b (dorg-head-commit dolt-dir)))
            (should-not (equal token-a token-b))
            (goto-char (min 7 (point-max)))
            (set-buffer-modified-p t)
            (let ((before (dorg-mlt--lifecycle-snapshot (current-buffer)))
                  (real-install
                   (symbol-function
                    'dorg-projection-install-view)))
              (dolist (window (list nil (list :focus (list alpha-id))))
                (cl-letf
                    (((symbol-function
                       'dorg-projection-install-view)
                      (lambda (&rest args)
                        (apply real-install args)
                        (error "injected lifecycle publication failure"))))
                  (should-error
                   (if window
                       (dorg-materialize (current-buffer)
                                         :revision token-b :window window)
                     (dorg-materialize (current-buffer)
                                       :revision token-b))))
                (dorg-mlt--should-match-lifecycle-snapshot
                 (current-buffer) before)))))))))

;;; ─── AC10/AC11 — retained pure readers; retired lifecycle API ─────────

(ert-deftest dorg-test-mlt-pure-string-readers-retained ()
  "`dorg-render' / `dorg-render-island' keep their pure string-read contract."
  (should (fboundp 'dorg-render))
  (should (fboundp 'dorg-render-island))
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (dorg-test--with-temp-db dolt-dir
      (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql "* Hello\n"))
      (dorg-commit dolt-dir "init")
      (should (string-match-p "^\\* Hello" (dorg-render dolt-dir))))))

(ert-deftest dorg-test-mlt-retired-buffer-lifecycle-api-absent ()
  "The duplicate public buffer-lifecycle/render-to-buffer surface is retired.
Only `dorg-materialize' mutates buffers; no aliases for the retired names."
  (dolist (sym '(dorg-render-to-buffer
                 dorg-render-island-to-buffer
                 dorg-render-window
                 dorg-render-window-to-buffer
                 dorg-render-window-restore-marker
                 dorg-sync-finalize-snapshot))
    (should-not (fboundp sym))))

(provide 'dorg-materialize-lifecycle-tests)
;;; dorg-materialize-lifecycle-tests.el ends here
