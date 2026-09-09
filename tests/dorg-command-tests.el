;;; dorg-command-tests.el --- Tests for dorg interactive commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-command': query commands and the `*dorg-query*'
;; results buffer, marker / id-search navigation, and the
;; typed `dorg-set-property' update (DORG_ID rejection, one API call,
;; exact-token Dolt refresh, and ordinary plain-Org modified-buffer behavior).
;;
;; Also covers the stable public noninteractive facade: the
;; `dorg-command-run-query' query helper (result shape, typed failures, and no
;; `*dorg-query*' side effect), plus the absence of the retired whole-buffer
;; persistence facade.  Interactive commands adapt typed failures to clean,
;; message-only `user-error' conditions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-command)
(require 'dorg-ql)
(require 'dorg-registry)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; ─── Helpers ──────────────────────────────────────────────────────────

(defmacro dorg-test-command--with-plain-org-file (contents bindings &rest body)
  "Write CONTENTS to a temp file, open it plain-org, then run BODY.
BINDINGS is a plist (:file FVAR :buffer BVAR); either key may be omitted."
  (declare (indent 2))
  (let ((file-var (or (plist-get bindings :file) (gensym "file")))
        (buf-var (or (plist-get bindings :buffer) (gensym "buf"))))
    `(let ((dorg--registry nil)
           (temp-base (make-temp-file "dorg-command-test-" t)))
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

(defun dorg-test-command--result-titles (buf)
  "Return the titles of the result lines in results buffer BUF, in order."
  (with-current-buffer buf
    (let (titles)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((r (get-text-property (point) 'dorg-command-result)))
            (push (plist-get r :title) titles))
          (forward-line 1)))
      (nreverse titles))))

(defun dorg-test-command--kill-results ()
  "Kill the `*dorg-query*' buffer if it exists."
  (when-let ((b (get-buffer "*dorg-query*")))
    (kill-buffer b)))

;;; ─── Query Commands ───────────────────────────────────────────────────

(ert-deftest dorg-test-command-query-property ()
  "`dorg-query-property' shows exactly the local-property matches."
  (dorg-test-command--with-plain-org-file
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
    (unwind-protect
        (with-current-buffer buf
          (let ((out (dorg-query-property "CATEGORY" "red")))
            (let ((titles (dorg-test-command--result-titles out)))
              (should (equal '("Match") titles)))))
      (dorg-test-command--kill-results))))

(ert-deftest dorg-test-command-query-tag ()
  "`dorg-query-tag' shows headings carrying the local tag."
  (dorg-test-command--with-plain-org-file
      "* Tagged :work:\n* Plain\n"
      (:buffer buf)
    (unwind-protect
        (with-current-buffer buf
          (let* ((out (dorg-query-tag "work"))
                 (titles (dorg-test-command--result-titles out)))
            (should (equal '("Tagged") titles))))
      (dorg-test-command--kill-results))))

(ert-deftest dorg-test-command-query-title ()
  "`dorg-query-title' shows the heading with the exact title."
  (dorg-test-command--with-plain-org-file
      "* First\n* Second\n"
      (:buffer buf)
    (unwind-protect
        (with-current-buffer buf
          (let* ((out (dorg-query-title "Second"))
                 (titles (dorg-test-command--result-titles out)))
            (should (equal '("Second") titles))))
      (dorg-test-command--kill-results))))

(ert-deftest dorg-test-command-query-malformed-user-error ()
  "An interactive malformed query raises a message-only `user-error'."
  (dorg-test-command--with-plain-org-file
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      (dolist (query '((between 1 2) id))
        (let ((err (should-error (dorg-query query) :type 'user-error)))
          (should-not (string-match-p "(:reason"
                                      (error-message-string err))))))))

(ert-deftest dorg-test-command-query-empty-shows-no-matches ()
  "An empty result set renders an explicit \"No matches.\" line."
  (dorg-test-command--with-plain-org-file
      "* Only\n:PROPERTIES:\n:CATEGORY: red\n:END:\n"
      (:buffer buf)
    (unwind-protect
        (with-current-buffer buf
          (let ((out (dorg-query-property "CATEGORY" "green")))
            (should (null (dorg-test-command--result-titles out)))
            (with-current-buffer out
              (should (string-match-p "No matches\\." (buffer-string))))))
      (dorg-test-command--kill-results))))

;;; ─── Public query facade (`dorg-command-run-query') ──────────────────

(ert-deftest dorg-test-command-run-query-matches-ql-select ()
  "`dorg-command-run-query' returns the same result plists as `dorg-ql-select'."
  (dorg-test-command--with-plain-org-file
      "* Match
:PROPERTIES:
:DORG_ID: 100
:CATEGORY: red
:END:
* Skip
:PROPERTIES:
:DORG_ID: 200
:CATEGORY: blue
:END:
"
      (:buffer buf)
    (with-current-buffer buf
      (let* ((query '(property "CATEGORY" "red"))
             (facade (dorg-command-run-query query buf))
             (direct (dorg-ql-select query buf)))
        (should (= 1 (length facade)))
        (should (equal (mapcar (lambda (r) (plist-get r :heading-id)) facade)
                       (mapcar (lambda (r) (plist-get r :heading-id)) direct)))
        (should (equal (mapcar (lambda (r) (plist-get r :title)) facade)
                       (mapcar (lambda (r) (plist-get r :title)) direct)))
        (should (equal 100 (plist-get (car facade) :heading-id)))
        (should (equal "Match" (plist-get (car facade) :title)))))))

(ert-deftest dorg-test-command-run-query-preserves-typed-error ()
  "`dorg-command-run-query' preserves the query condition and its plist."
  (dorg-test-command--with-plain-org-file
      "* H\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      (dolist (query '((between 1 2) id))
        (let ((err (should-error (dorg-command-run-query query buf)
                                 :type 'dorg-ql-error)))
          (should (eq 'invalid-query (dorg-error-reason err)))
          (should (equal '(:reason invalid-query)
                         (dorg-error-data err))))))))

(ert-deftest dorg-test-command-run-query-no-results-buffer ()
  "`dorg-command-run-query' must not create or display `*dorg-query*'."
  (dorg-test-command--with-plain-org-file
      "* Match\n:PROPERTIES:\n:CATEGORY: red\n:END:\n"
      (:buffer buf)
    (dorg-test-command--kill-results)
    (with-current-buffer buf
      (should (dorg-command-run-query '(property "CATEGORY" "red") buf))
      (should (null (get-buffer "*dorg-query*")))
      ;; An empty result set is just as silent.
      (should (null (dorg-command-run-query '(property "CATEGORY" "green") buf)))
      (should (null (get-buffer "*dorg-query*"))))))

(ert-deftest dorg-test-command-private-query-wrapper-removed ()
  "The private query compatibility wrapper is gone; both real seams remain.
`dorg-command--run-query' existed solely to give downstream callers of the
old private name one release cycle to migrate.  The census found that
population empty, and 0.1 is the FIRST public tag \u2014 there is no earlier
release anything could be migrating from \u2014 so the grace period had no
beneficiary.  What must survive is the pair it sat between: the public
machine facade, which propagates typed `dorg-ql-error' conditions, and the
private interactive adapter, which converts them to message-only
`user-error'.  Re-adding the wrapper turns the `should-not' RED."
  (should-not (fboundp 'dorg-command--run-query))
  (should (fboundp 'dorg-command-run-query))
  (should (fboundp 'dorg-command--run-query-for-user))
  (dorg-test-command--with-plain-org-file
      "* Match\n:PROPERTIES:\n:DORG_ID: 100\n:CATEGORY: red\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      ;; The facade still answers, and the adapter still agrees with it.
      (should (equal (dorg-command--run-query-for-user
                      '(property "CATEGORY" "red") buf)
                     (dorg-command-run-query
                      '(property "CATEGORY" "red") buf)))
      ;; The typed/interactive split the wrapper blurred is intact: the
      ;; facade signals `dorg-ql-error', the adapter signals `user-error'.
      (should-error (dorg-command-run-query '(bogus-predicate "x") buf)
                    :type 'dorg-ql-error)
      (should-error (dorg-command--run-query-for-user '(bogus-predicate "x") buf)
                    :type 'user-error))))

;;; ─── Navigation ───────────────────────────────────────────────────────

(ert-deftest dorg-test-command-goto-id ()
  "`dorg-goto-id' moves point to the heading with that DORG_ID."
  (dorg-test-command--with-plain-org-file
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
    (with-current-buffer buf
      (goto-char (point-min))
      (dorg-goto-id 200)
      (should (eq (current-buffer) buf))
      (should (equal "200" (org-entry-get (point) "DORG_ID")))
      (should (equal "Beta" (org-get-heading t t t t))))))

(ert-deftest dorg-test-command-goto-result-nil-marker-id-search ()
  "The locate helper resolves a nil-marker result by in-buffer id search."
  (dorg-test-command--with-plain-org-file
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
    (with-current-buffer buf
      (goto-char (point-min))
      ;; A markerless result, as Dolt/model adapters return.
      (dorg-command--goto-result
       (list :heading-id 200 :marker nil :title "Beta" :properties nil)
       buf)
      (should (equal "200" (org-entry-get (point) "DORG_ID"))))))

(ert-deftest dorg-test-command-goto-id-absent-user-error ()
  "Navigating to an id absent from the buffer signals `user-error'."
  (dorg-test-command--with-plain-org-file
      "* Only\n:PROPERTIES:\n:DORG_ID: 1\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      (should-error (dorg-goto-id 9999) :type 'user-error)
      (should-error (dorg-command--goto-id-in-buffer 9999 buf)
                    :type 'user-error))))

;;; ─── Update: buffer-mediated transformation ──────────────────────────

(ert-deftest dorg-test-command-set-property-transforms ()
  "`dorg-set-property' adds the property and marks the buffer modified.
Non-vacuous: the property is absent before and present after."
  (dorg-test-command--with-plain-org-file
      "* Item
:PROPERTIES:
:DORG_ID: 100
:END:
"
      (:buffer buf)
    (with-current-buffer buf
      (let ((marker (dorg-command--goto-id-in-buffer 100 buf)))
        ;; before: no STATUS
        (goto-char marker)
        (should (null (org-entry-get (point) "STATUS"))))
      (set-buffer-modified-p nil)
      (dorg-set-property "STATUS" "done" 100 buf)
      ;; after: STATUS present and the buffer is dirty
      (dorg-command--goto-id-in-buffer 100 buf)
      (should (equal "done" (org-entry-get (point) "STATUS")))
      (should (buffer-modified-p buf)))))

(ert-deftest dorg-test-command-set-property-uses-one-typed-api-call ()
  "The interactive setter delegates one canonical mutation and returns it."
  (dorg-test-command--with-plain-org-file
      "* Item\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
      (:buffer buf)
    (let (calls)
      (cl-letf (((symbol-function 'dorg-api-execute)
                 (lambda (form &rest options)
                   (push (cons form options) calls)
                   '(:ok t :new-token "plain-2"))))
        (should (equal '(:ok t :new-token "plain-2")
                       (dorg-set-property "STATUS" "done" 100 buf))))
      (should (= 1 (length calls)))
      (pcase-let ((`(,form . ,options) (car calls)))
        (should (equal form
                       '(on (id 100) (set (property "STATUS") "done"))))
        (should (eq (plist-get options :mode) 'apply))
        (should (eq (plist-get options :source) buf))
        (should-not (plist-member options :save))))))

(ert-deftest dorg-test-command-set-property-needs-no-legacy-authoring-state ()
  "Typed property mutation is independent of retired reconciliation state."
  (dorg-test-command--with-plain-org-file
      "* Item\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      (should-not (boundp 'dorg--local-authoring-p))
      (should-not (boundp 'dorg--dirty-ids))
      (cl-letf (((symbol-function 'dorg-api-execute)
                 (lambda (&rest _)
                   '(:ok t :new-token "plain-2"))))
        (dorg-set-property "STATUS" "done" 100 buf))
      (should-not (boundp 'dorg--local-authoring-p))
      (should-not (boundp 'dorg--dirty-ids)))))

(ert-deftest dorg-test-command-dolt-set-property-preflights-and-refreshes-token ()
  "Dolt mutation validates its view before one commit and refreshes exact token."
  (with-temp-buffer
    (org-mode)
    (insert "* Item\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n")
    (let ((buf (current-buffer))
          events)
      (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                 (lambda (&optional _buffer) "/tmp/fake/"))
                ((symbol-function 'dorg--refresh-materialized-view-context)
                 (lambda (buffer)
                   (push (list 'preflight buffer) events)
                   '(:scope whole)))
                ((symbol-function 'dorg-api-execute)
                 (lambda (form &rest options)
                   (push (list 'apply form options) events)
                   '(:ok t :new-token "commit-2")))
                ((symbol-function 'dorg-refresh-materialized-view)
                 (lambda (buffer revision)
                   (push (list 'refresh buffer revision) events)
                   '(:scope whole :revision "commit-2"))))
        (should (equal '(:ok t :new-token "commit-2")
                       (dorg-set-property "STATUS" "done" 100 buf))))
      (setq events (nreverse events))
      (should (equal (mapcar #'car events) '(preflight apply refresh)))
      (should (eq (nth 1 (nth 0 events)) buf))
      (should (equal (nth 1 (nth 1 events))
                     '(on (id 100) (set (property "STATUS") "done"))))
      (should (eq (plist-get (nth 2 (nth 1 events)) :source) buf))
      (should (equal (nth 2 (nth 2 events)) "commit-2")))))

(ert-deftest dorg-test-command-dolt-set-property-refuses-before-api-without-view ()
  "A missing/stale Dolt view cannot commit and hope to refresh afterwards."
  (with-temp-buffer
    (org-mode)
    (insert "* Item\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n")
    (let ((api-calls 0))
      (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                 (lambda (&optional _buffer) "/tmp/fake/"))
                ((symbol-function 'dorg--refresh-materialized-view-context)
                 (lambda (&rest _) (user-error "dorg: no established view")))
                ((symbol-function 'dorg-api-execute)
                 (lambda (&rest _)
                   (cl-incf api-calls)
                   '(:ok t :new-token "must-not-commit"))))
        (should-error (dorg-set-property "STATUS" "done" 100 (current-buffer))
                      :type 'user-error))
      (should (= 0 api-calls)))))

(defun dorg-test-command--assert-post-commit-refresh-fault (fault)
  "Assert that post-commit refresh FAULT reports its token without replay."
  (with-temp-buffer
    (org-mode)
    (insert "* Item\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n")
    (let ((api-calls 0)
          caught)
      (cl-letf (((symbol-function 'dorg-registered-dolt-dir)
                 (lambda (&optional _buffer) "/tmp/fake/"))
                ((symbol-function 'dorg--refresh-materialized-view-context)
                 (lambda (&rest _) '(:scope whole)))
                ((symbol-function 'dorg-api-execute)
                 (lambda (&rest _)
                   (cl-incf api-calls)
                   '(:ok t :new-token "commit-2")))
                ((symbol-function 'dorg-refresh-materialized-view)
                 (lambda (&rest _) (funcall fault))))
        (condition-case err
            (dorg-set-property "STATUS" "done" 100 (current-buffer))
          (dorg-error (setq caught err))))
      (should caught)
      (should (eq (dorg-error-reason caught) 'refresh-after-commit-failed))
      (should (equal (plist-get (dorg-error-data caught) :new-token)
                     "commit-2"))
      (should (= 1 api-calls)))))

(ert-deftest dorg-test-command-post-commit-refresh-error-carries-token-no-retry ()
  "A refresh error reports the committed token and does not replay mutation."
  (dorg-test-command--assert-post-commit-refresh-fault
   (lambda () (error "render failed"))))

(ert-deftest dorg-test-command-post-commit-refresh-quit-carries-token-no-retry ()
  "A refresh quit reports the committed token and does not replay mutation."
  (dorg-test-command--assert-post-commit-refresh-fault
   (lambda () (signal 'quit nil))))

(ert-deftest dorg-test-command-refresh-boundary-is-lazy ()
  "`dorg-command' declares mode refresh functions without requiring the mode."
  (with-temp-buffer
    (insert-file-contents (locate-library "dorg-command"))
    (should-not (re-search-forward "^(require 'dorg-mode)" nil t))
    (goto-char (point-min))
    (should (re-search-forward
             "^(declare-function dorg-refresh-materialized-view " nil t))))

(ert-deftest dorg-test-command-persist-boundary-is-removed ()
  "No compatibility save/sync facade remains after typed command cutover."
  (should-not (fboundp 'dorg-persist-buffer)))

;;; ─── Update: DORG_ID identity rejection ──────────────────────────────

(ert-deftest dorg-test-command-set-property-rejects-dorg-id ()
  "`dorg-set-property' refuses to set DORG_ID (case-insensitively)."
  (dorg-test-command--with-plain-org-file
      "* Item
:PROPERTIES:
:DORG_ID: 100
:END:
"
      (:buffer buf)
    (with-current-buffer buf
      (should-error (dorg-set-property "DORG_ID" "9" 100 buf) :type 'user-error)
      (should-error (dorg-set-property "dorg_id" "9" 100 buf) :type 'user-error)
      ;; identity unchanged
      (dorg-command--goto-id-in-buffer 100 buf)
      (should (equal "100" (org-entry-get (point) "DORG_ID"))))))

;;; ─── Dolt arm (gated) ─────────────────────────────────────────────────

(defmacro dorg-test-command--with-dolt-synced (contents bindings &rest body)
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
                          "dorg-command-dolt-" "command-dolt-synced"))
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

(ert-deftest dorg-test-command-dolt-goto-id-nil-marker ()
  "On a Dolt buffer, `dorg-goto-id' locates by id-search despite nil markers."
  (dorg-test-command--with-dolt-synced
      "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n* Other\n:PROPERTIES:\n:DORG_ID: 556\n:END:\n"
      (:buffer buf)
    (with-current-buffer buf
      ;; The ql Dolt result carries a nil marker; navigation must fall back
      ;; to the in-buffer DORG_ID search.
      (should (null (plist-get (car (dorg-ql-select '(id 555) buf)) :marker)))
      (goto-char (point-min))
      (dorg-goto-id 555)
      (should (equal "555" (org-entry-get (point) "DORG_ID"))))))

(ert-deftest dorg-test-command-dolt-set-property-commits-and-refreshes-view ()
  "Dolt set-property commits directly, then refreshes the established view."
  (let ((dorg-dolt-auto-server nil))
    (dorg-test-command--with-dolt-synced
        "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n"
      (:buffer buf)
      (with-current-buffer buf
        (dorg-materialize buf :source buf)
        (dorg-mode 1)
        (should (null (dorg-ql-select '(property "STATUS" "done") buf)))
        (let ((response (dorg-set-property "STATUS" "done" 555 buf)))
          (should (stringp (plist-get response :new-token)))
          (should (equal (plist-get response :new-token)
                         (plist-get (dorg-projection-view-context buf)
                                    :revision))))
        (let ((res (dorg-ql-select '(property "STATUS" "done") buf)))
          (should (= 1 (length res)))
          (should (= 555 (plist-get (car res) :heading-id))))))))

(provide 'dorg-command-tests)
;;; dorg-command-tests.el ends here
