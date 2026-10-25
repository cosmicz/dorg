;;; dorg-ql-door-tests.el --- Public query door controls -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; These controls keep raw query validation ahead of content reads and pin the
;; forms advertised in docs/query.org to the public load boundary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-ql)
(require 'dorg-ql-dolt)
(require 'dorg-model)
(require 'dorg-registry)

(defconst dorg-test-ql-door--invalid-cases
  '((key-free (bogus 1) invalid-query invalid-query invalid-query)
    (key-empty (key) invalid-query invalid-query invalid-query)
    (key-extra (key "100" "extra") invalid-query invalid-query invalid-query)
    (key-number (key 100) invalid-query invalid-query invalid-query)
    (key-under-invalid (pipe (where (bogus (key "100"))))
                       invalid-query invalid-query unsupported)
    (key-dotted (pipe (where (key "100")) . bad)
                invalid-query invalid-query invalid-query))
  "Name, raw query, and exact plain/Dolt/model reasons, respectively.")

(defun dorg-test-ql-door--reason (query source)
  "Return the typed reason from QUERY on SOURCE, failing on a raw Lisp error."
  (let ((err (should-error (dorg-ql-select query source)
                           :type 'dorg-ql-error)))
    (dorg-error-reason err)))

(ert-deftest dorg-test-ql-door-refuses-before-content-read ()
  "Malformed raw queries cannot read plain or Dolt content before validation."
  (let* ((dorg--registry nil)
         (dir (make-temp-file "dorg-ql-door-" t))
         (file (expand-file-name "doc.org" dir))
         (plain nil)
         (dolt (make-dorg-registration
                :file file :backend (dorg-backend-dolt dir 1)
                :identity 'dorg))
         (model (list (make-dorg-heading :id 100 :level 1
                                         :level-offset 1 :title "Alpha"))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"))
          (setq plain (find-file-noselect file))
          (dolist (source (list plain dolt model))
            (let ((plain-reads 0)
                  (head-reads 0)
                  (row-reads 0)
                  (transport-calls 0)
                  (server-starts 0)
                  (poison-head t)
                  (identity-lookups 0)
                  (real-identity (symbol-function 'dorg-mutate--source-identity)))
              (cl-letf (((symbol-function 'dorg-ql--plain-eval)
                         (lambda (&rest _)
                           (cl-incf plain-reads)
                           (error "door plain-content poison")))
                        ((symbol-function 'dorg-head-commit)
                         (lambda (&rest _)
                           (if poison-head
                               (progn
                                 (cl-incf head-reads)
                                 (error "door Dolt HEAD poison"))
                             "captured")))
                        ((symbol-function 'dorg-ql--dolt-eval)
                         (lambda (&rest _)
                           (cl-incf row-reads)
                           (error "door Dolt row poison")))
                        ((symbol-function 'dorg-ql--dolt-query)
                         (lambda (&rest _)
                           (cl-incf transport-calls)
                           (error "door transport poison")))
                        ((symbol-function 'dorg-sql)
                         (lambda (&rest _)
                           (cl-incf transport-calls)
                           (error "door SQL poison")))
                        ((symbol-function 'dorg-sql--get-connection)
                         (lambda (&rest _)
                           (cl-incf transport-calls)
                           (error "door connection poison")))
                        ((symbol-function 'dorg-dolt-server--start)
                         (lambda (&rest _)
                           (cl-incf server-starts)
                           (error "door server-start poison")))
                        ((symbol-function 'dorg-dolt-server--ensure-strict)
                         (lambda (&rest _)
                           (cl-incf server-starts)
                           (error "door server-ensure poison")))
                        ((symbol-function 'dorg-mutate--source-identity)
                         (lambda (&rest args)
                           (cl-incf identity-lookups)
                           (apply real-identity args))))
                (dolist (case dorg-test-ql-door--invalid-cases)
                  (let ((before identity-lookups))
                    (should (eq (nth (cond ((eq source plain) 2)
                                           ((eq source dolt) 3)
                                           (t 4)) case)
                                (dorg-test-ql-door--reason (nth 1 case) source)))
                    (when (eq (car case) 'key-free)
                      (should (= identity-lookups before)))
                    (should (= plain-reads 0))
                    (should (= head-reads 0))
                    (should (= row-reads 0))
                    (should (= transport-calls 0))
                    (should (= server-starts 0))))
                ;; A valid query must hit the poisoned content seam on each
                ;; backend, so the zero-read assertions above are not vacuous.
                (unless (eq source model)
                  (let* ((before identity-lookups)
                         (err (should-error
                               (dorg-ql-select '(key "100") source))))
                    (should (string-match-p "door .* poison" (cadr err)))
                    (should (> identity-lookups before))
                    (if (eq source plain)
                        (should (= plain-reads 1))
                      (should (= head-reads 1))
                      ;; The second valid query proves the row seam is live.
                      (setq poison-head nil)
                      (let ((row-err (should-error
                                      (dorg-ql-select '(key "100") source))))
                        (should (equal (cadr row-err) "door Dolt row poison"))
                        (should (= row-reads 1))))
                    (when (eq source plain) (should (= row-reads 0)))
                    (should (= transport-calls 0))
                    (should (= server-starts 0))))))))
      (when (buffer-live-p plain)
        (with-current-buffer plain (set-buffer-modified-p nil))
        (kill-buffer plain))
      (delete-directory dir t))))

(ert-deftest dorg-test-ql-door-key-binding-refusals-are-specific ()
  "Unavailable identity and absent registration have fixed unsupported reasons."
  (let* ((dorg--registry nil)
         (no-identity (make-dorg-registration
                       :file "/unused.org"
                       :backend (dorg-backend-dolt "/unused" 1)
                       :identity 'none)))
    (should (eq 'unsupported
                (dorg-test-ql-door--reason '(key "100") no-identity)))
    (with-temp-buffer
      (should (eq 'unsupported
                  (dorg-test-ql-door--reason '(key "100")
                                              (current-buffer)))))))

(defconst dorg-test-ql-door--public-cases
  '((id (id 100) ids (100))
    (field (field todo "TODO") ids (100))
    (property (property "FOO" "bar") ids (100))
    (keyword (keyword "EXPORT_FILE_NAME" "exported") ids (100))
    (title (title "Alpha") ids (100))
    (tag (tag "work") ids (100))
    (scheduled (scheduled "2026-05-15" "2026-05-15") ids (100))
    (deadline (deadline "2026-05-17" "2026-05-17") ids (200))
    (closed (closed "2026-05-16" "2026-05-16") ids (200))
    (active-ts (active-ts "2026-05-17" "2026-05-17") ids (100))
    (logbook (logbook "2026-05-16" "2026-05-16") ids (100))
    (property-ts (property-ts "2026-05-15" "2026-05-15" "DUE") ids (100))
    (pipe (pipe (where (id 100))) ids (100))
    (where (pipe (where (id 100))) ids (100))
    (traverse (pipe (where (id 100)) (traverse children)) ids (200))
    (unique (pipe (where (id 100)) (unique)) ids (100))
    (one (one (id 100)) ids (100))
    (and (and (id 100) (title "Alpha")) ids (100))
    (or (or (id 100) (id 999)) ids (100))
    (not (not (id 200)) ids (100))
    (key (key "100") ids (100))
    (exists (exists (property "FOO")) ids (100))
    (missing (missing (property "FOO")) ids (200))
    (timestamp (timestamp scheduled :overlaps (on "2026-05-15")) ids (100))
    (between (scheduled (between "2026-05-15" "2026-05-15")) ids (100))
    (on (scheduled (on "2026-05-15")) ids (100))
    (before (scheduled (before "2026-05-15")) ids (100))
    (after (scheduled (after "2026-05-15")) ids (100))
    (today (scheduled (today)) ids (100))
    (this-week (scheduled (this-week)) ids (100))
    (= (= (field title) "Alpha") ids (100))
    (!= (!= (field title) "Beta") ids (100))
    (< (< (field id) 200) ids (100))
    (<= (<= (field id) 100) ids (100))
    (> (> (field id) 100) ids (200))
    (>= (>= (field id) 200) ids (200))
    (contains (contains (field title) "lph") ids (100))
    (contains-any (contains-any (field title) ("pha" "xyz")) ids (100))
    (contains-any (contains-any (field title) ("ALP" "xyz") :case insensitive)
                  ids (100))
    (= (= (field title) "ALPHA" :case insensitive) ids (100))
    (prefix (prefix (field title) "Al") ids (100))
    (suffix (suffix (field title) "pha") ids (100))
    (select (pipe (where (id 100)) (select (field title)))
            values (("title" . "Alpha")))
    (as (pipe (where (id 100)) (select (as (field title) label)))
        values (("label" . "Alpha")))
    (rank-bm25 (pipe (rank-bm25 "Alpha" :profile sensitive :top-k 1))
               reason unsupported))
  "One concrete public-entry assertion per documented executable form head.")

(defconst dorg-test-ql-door--grammar-blocks
  '("dorg-query-atomic-forms" "dorg-query-composable-forms"
    "dorg-query-case-forms" "dorg-query-contains-any-case-form"
    "dorg-query-value-forms" "dorg-query-window-forms"
    "dorg-query-projection-forms")
  "Names of canonical executable grammar blocks in docs/query.org.")

(defun dorg-test-ql-door--block (document name)
  "Return the body of named grammar block NAME from DOCUMENT."
  (with-temp-buffer
    (insert document)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^#\\+name: " (regexp-quote name) "$"))
      (error "Missing documented grammar block %s" name))
    (forward-line 1)
    (unless (looking-at "#\\+begin_\\(example\\|src\\)")
      (error "Grammar block %s has no body" name))
    (forward-line 1)
    (let ((start (point)))
      (unless (re-search-forward "^#\\+end_\\(example\\|src\\)" nil t)
        (error "Unclosed grammar block %s" name))
      (buffer-substring-no-properties start (line-beginning-position)))))

(defun dorg-test-ql-door--heads-in-text (text)
  "Return all parenthesized heads in TEXT, including nested forms."
  (let (heads (at 0))
    (while (string-match "([[:space:]]*\\([^()[:space:]]+\\)" text at)
      (push (intern (match-string 1 text)) heads)
      (setq at (match-end 0)))
    heads))

(defun dorg-test-ql-door--documented-heads (document)
  "Derive executable head symbols from canonical blocks in DOCUMENT."
  (let (heads)
    (dolist (name dorg-test-ql-door--grammar-blocks)
      (let ((block (dorg-test-ql-door--block document name)))
        (dolist (line (split-string block "\n" t))
          (when (string-match "^([[:space:]]*\\([^()[:space:]]+\\)" line)
            (push (intern (match-string 1 line)) heads))
          (when (and (equal name "dorg-query-composable-forms")
                     (string-match "STEP = \\(.*\\)" line))
            (setq heads (append (dorg-test-ql-door--heads-in-text
                                 (match-string 1 line)) heads)))
          (when (equal name "dorg-query-projection-forms")
            (setq heads (append (dorg-test-ql-door--heads-in-text line)
                                heads)))
          (when (string-match "OP ∈ \\(.*\\)" line)
            (setq heads (append (mapcar #'intern
                                        (split-string (match-string 1 line)))
                                heads))))))
    ;; OP is a documented metavariable expanded from its adjacent vocabulary.
    (sort (delete-dups (delq 'OP heads))
          (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun dorg-test-ql-door--assert-inventory (document cases)
  "Assert exact two-way agreement between DOCUMENT grammar and CASES."
  (let* ((documented (dorg-test-ql-door--documented-heads document))
         (covered (sort (delete-dups (mapcar #'car cases))
                        (lambda (a b) (string< (symbol-name a)
                                                (symbol-name b))))))
    (unless (equal documented covered)
      (ert-fail (list :missing-cases (cl-set-difference documented covered)
                      :undocumented-cases (cl-set-difference covered documented))))))

(ert-deftest dorg-test-ql-door-public-inventory-is-bidirectional ()
  "Both documentation drift and invented case heads fail the inventory gate."
  (let* ((root (file-name-directory (or (locate-library "dorg-ql")
                                        (error "dorg-ql not on load-path"))))
         (document (with-temp-buffer
                     (insert-file-contents (expand-file-name "docs/query.org" root))
                     (buffer-string))))
    (dorg-test-ql-door--assert-inventory document
                                         dorg-test-ql-door--public-cases)
    (should-error
     (dorg-test-ql-door--assert-inventory
      (replace-regexp-in-string "^(id ID)[^\n]*\n" "" document)
      dorg-test-ql-door--public-cases)
     :type 'ert-test-failed)
    (should-error
     (dorg-test-ql-door--assert-inventory
      document (cons '(extra-form nil ids nil)
                     dorg-test-ql-door--public-cases))
     :type 'ert-test-failed)))

(defconst dorg-test-ql-door--child-org
  "#+CATEGORY: work
* TODO Alpha :work:
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:DORG_ID: 100
:FOO: bar
:DUE: <2026-05-15 Fri>
:END:
#+EXPORT_FILE_NAME: exported
:LOGBOOK:
- State \"DONE\" from \"TODO\" [2026-05-16 Sat]
:END:
Body meeting <2026-05-17 Sun> here.
** DONE Beta :team:
DEADLINE: <2026-05-17 Sun> CLOSED: [2026-05-16 Sat]
:PROPERTIES:
:DORG_ID: 200
:END:
"
  "Small real plain-Org source for every documented query form.")

(defun dorg-test-ql-door--child-expression (cases)
  "Return self-contained child expression exercising CASES through public QL."
  `(let ((load-prefer-newer t))
     (dolist (feature features)
       (let ((name (symbol-name feature)))
         (when (or (equal name "dorg")
                   (string-prefix-p "dorg-" name))
           (error "DOrg feature loaded before public require: %S" feature))))
     (dolist (path load-path)
       (when (and (stringp path)
                  (string-match-p "/tests/?\\'" path))
         (error "tests directory leaked onto child load-path: %S" path)))
     (dolist (entry process-environment)
       (when (string-match-p "\\`\\(MELD_\\|EMBEAD_\\)" entry)
         (error "MelD/Embead variable leaked into child")))
     (require 'dorg-ql)
     (let ((dorg-ql-current-time (encode-time 0 0 12 15 5 2026))
           (file (make-temp-file "dorg-ql-public-" nil ".org")))
       (unwind-protect
           (progn
             (with-temp-file file (insert ,dorg-test-ql-door--child-org))
             (let ((buffer (find-file-noselect file)))
               (unwind-protect
                   (dolist (case ',cases)
                     (condition-case case-error
                         (let ((head (nth 0 case))
                           (query (nth 1 case))
                           (kind (nth 2 case))
                           (expected (nth 3 case)))
                       (if (eq kind 'reason)
                           (let ((actual
                                  (condition-case err
                                      (progn (dorg-ql-search-snapshot query buffer)
                                             :no-error)
                                    (dorg-ql-error (dorg-error-reason err)))))
                             (unless (eq expected actual)
                               (error "%S reason: expected %S, got %S"
                                      head expected actual)))
                         (let* ((rows (dorg-ql-select query buffer))
                                (actual (pcase kind
                                          ('ids (mapcar (lambda (row)
                                                          (plist-get row :heading-id))
                                                        rows))
                                          ('values (plist-get (car rows) :values))
                                          (_ (error "Unknown assertion %S" kind)))))
                           (unless (equal expected actual)
                             (error "%S: expected %S, got %S; rows %S"
                                    head expected actual rows)))))
                       (error
                        (princ (format "CASE-FAIL %S %S\n"
                                       (car case) case-error))
                        (kill-emacs 23))))
                 (kill-buffer buffer)))
             (princ (format "PUBLIC-FORMS-OK %d" (length ',cases))))
         (delete-file file)))))

(defun dorg-test-ql-door--run-child (root cases)
  "Run isolated Emacs child for CASES from ROOT with a reaped 120s deadline."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (process-environment
          (cl-remove-if
           (lambda (entry)
             (string-match-p "\\`\\(MELD_\\|EMBEAD_\\|EMACSLOADPATH=\\)" entry))
           process-environment))
         (output (generate-new-buffer " *dorg-query-public-forms*"))
         (command (list "/bin/sh" "-c" "exec \"$@\" </dev/null" "sh"
                        emacs "-Q" "--batch"
                        "--eval" "(setq native-comp-jit-compilation nil native-comp-enable-subr-trampolines nil load-prefer-newer t)"
                        "-L" root
                        "--eval" (prin1-to-string
                                  (dorg-test-ql-door--child-expression cases))))
         (proc nil))
    (unwind-protect
        (progn
          (setq proc (make-process :name "dorg-query-public-forms"
                                   :buffer output :stderr output
                                   :command command :connection-type 'pipe
                                   :noquery t))
          (let ((deadline (+ (float-time) 120)))
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (when (process-live-p proc)
              (kill-process proc)
              (let ((reap-deadline (+ (float-time) 5)))
                (while (and (process-live-p proc)
                            (< (float-time) reap-deadline))
                  (accept-process-output proc 0.1)))
              (unless (not (process-live-p proc))
                (error "Timed-out public-form child was not reaped"))
              (error "Public-form child exceeded 120 seconds")))
          (with-current-buffer output
            (let ((text (buffer-string)))
              (unless (and (= 0 (process-exit-status proc))
                           (string-match-p
                            (format "PUBLIC-FORMS-OK %d" (length cases)) text))
                (ert-fail (format "Public-form child failed: exit=%s output=%s"
                                  (process-exit-status proc) text)))))
          t)
      (when (and proc (process-live-p proc))
        (kill-process proc)
        (while (process-live-p proc) (accept-process-output proc 0.1)))
      (kill-buffer output))))

(ert-deftest dorg-test-ql-door-public-forms-load-in-fresh-child ()
  "Only public dorg-ql loading admits every documented executable form."
  (let* ((root (file-name-directory (or (locate-library "dorg-ql")
                                        (error "dorg-ql not on load-path"))))
         (document (with-temp-buffer
                     (insert-file-contents (expand-file-name "docs/query.org" root))
                     (buffer-string))))
    (dorg-test-ql-door--assert-inventory document
                                         dorg-test-ql-door--public-cases)
    (should (dorg-test-ql-door--run-child root
                                          dorg-test-ql-door--public-cases))))

(provide 'dorg-ql-door-tests)
;;; dorg-ql-door-tests.el ends here
