;;; dorg-roundtrip-corpus-tests.el --- Semantic round-trip corpus gate -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Release certificate for semantic, not byte-for-byte, round trips.
;;
;; The oracle compares normalized `dorg-parse-buffer' domain projections.
;; It deliberately does not compare rendered bytes.  Managed fields excluded
;; from the projection are DORG_ID, DORG_DOLT_DIR, DORG_HASH, numeric IDs,
;; sort keys, DB-local item ids, marker data, and items-hash.
;;
;; Documented renderer/parser canonicalizations covered by projection
;; equality, not by byte-diff exceptions:
;; - keyword key case normalization, e.g. #+title: renders as #+TITLE:;
;; - keyword/property/tag token spacing normalization;
;; - planning field order normalization to SCHEDULED, DEADLINE, CLOSED;
;; - property drawers left empty after managed DORG_ID exclusion may vanish;
;; - body item trailing post-blank trim/re-add behavior;
;; - verbatim block comma escaping/unescaping round-trip.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'dorg-dolt)
(require 'dorg-parse)
(require 'dorg-render)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)

(defconst dorg-test-roundtrip-corpus--fixtures
  '(("mixed" . "tests/fixtures/semantic-roundtrip/mixed.org")
    ("properties-logbook" . "tests/fixtures/semantic-roundtrip/properties-logbook.org")
    ("body-elements" . "tests/fixtures/semantic-roundtrip/body-elements.org")
    ("tracker-like" . "tests/fixtures/semantic-roundtrip/tracker-like.org"))
  "Fixed semantic round-trip corpus fixtures.")

(defun dorg-test-roundtrip-corpus--repo-root ()
  "Return repository root for corpus fixtures."
  (or (locate-dominating-file default-directory "dorg.el")
      (when-let ((library (locate-library "dorg")))
        (file-name-directory library))
      (error "Cannot locate dorg repository root")))

(defun dorg-test-roundtrip-corpus--fixture-path (relative)
  "Return absolute fixture path for RELATIVE."
  (expand-file-name relative (dorg-test-roundtrip-corpus--repo-root)))

(defun dorg-test-roundtrip-corpus--read-file (file)
  "Read FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun dorg-test-roundtrip-corpus--parse (text)
  "Parse TEXT and assign deterministic ids."
  (with-temp-buffer
    (insert text)
    (org-mode)
    (goto-char (point-min))
    (let* ((result (dorg-parse-buffer nil nil))
           (counter 0))
      (dorg-parse-assign-ids result (lambda () (cl-incf counter)))
      result)))

(defun dorg-test-roundtrip-corpus--managed-keyword-p (keyword)
  "Return non-nil when KEYWORD is managed DOrg metadata."
  (string-prefix-p "DORG_" (upcase keyword)))

(defun dorg-test-roundtrip-corpus--path-for-level (level stacks counts)
  "Return a semantic path for LEVEL, mutating STACKS and COUNTS."
  (if (= level 0)
      (progn
        (puthash 0 '(0) stacks)
        '(0))
    (let* ((parent (or (gethash (1- level) stacks) '(0)))
           (key (prin1-to-string parent))
           (next (1+ (or (gethash key counts) 0)))
           (path (append parent (list next))))
      (puthash key next counts)
      (puthash level path stacks)
      path)))

(defun dorg-test-roundtrip-corpus--item-projection (item)
  "Return semantic projection for ITEM, or nil for managed metadata."
  (cond
   ((cl-typep item 'dorg-text-item)
    `(:text ,(dorg-text-item-text item)
            :post-blank ,(dorg-item-post-blank item)))
   ((cl-typep item 'dorg-keyword-item)
    (unless (dorg-test-roundtrip-corpus--managed-keyword-p
             (dorg-keyword-item-keyword item))
      `(:keyword ,(upcase (dorg-keyword-item-keyword item))
                 ,(dorg-keyword-item-value item)
                 :post-blank ,(dorg-item-post-blank item))))
   ((cl-typep item 'dorg-planning-item)
    `(:planning :scheduled ,(dorg-planning-item-scheduled item)
                :deadline ,(dorg-planning-item-deadline item)
                :closed ,(dorg-planning-item-closed item)
                :post-blank ,(dorg-item-post-blank item)))
   ((cl-typep item 'dorg-drawer-item)
    (let ((name (upcase (dorg-drawer-item-name item))))
      (if (string= name "PROPERTIES")
          (let ((props (cl-remove-if
                        (lambda (kv) (string= (upcase (car kv)) "DORG_ID"))
                        (dorg-drawer-item-properties item))))
            (when props
              `(:properties ,props
                            :post-blank ,(dorg-item-post-blank item))))
        `(:drawer ,name
                  :lines ,(dorg-drawer-item-lines item)
                  :post-blank ,(dorg-item-post-blank item)))))
   (t `(:unknown ,(format "%S" item)))))

(defun dorg-test-roundtrip-corpus--semantic-snapshot (text)
  "Return semantic projection for Org TEXT."
  (let* ((result (dorg-test-roundtrip-corpus--parse text))
         (stacks (make-hash-table :test 'eql))
         (counts (make-hash-table :test 'equal))
         (snapshot nil))
    (dolist (heading (dorg-parse-result-headings result))
      (let* ((level (dorg-heading-level heading))
             (path (dorg-test-roundtrip-corpus--path-for-level level stacks counts))
             (items (delq nil (mapcar #'dorg-test-roundtrip-corpus--item-projection
                                       (dorg-heading-items heading)))))
        (push `(:heading
                :path ,path
                :parent ,(butlast path)
                :level ,level
                :todo ,(dorg-heading-todo heading)
                :priority ,(dorg-heading-priority heading)
                :commented ,(dorg-heading-commented heading)
                :title ,(dorg-heading-title heading)
                :tags ,(dorg-heading-tags heading)
                :post-blank ,(dorg-heading-post-blank heading)
                :items ,items)
              snapshot)))
    (nreverse snapshot)))

(defun dorg-test-roundtrip-corpus--assert-files-equivalent (expected actual)
  "Assert EXPECTED and ACTUAL files are semantically equivalent."
  (let ((expected-snapshot
         (dorg-test-roundtrip-corpus--semantic-snapshot
          (dorg-test-roundtrip-corpus--read-file expected)))
        (actual-snapshot
         (dorg-test-roundtrip-corpus--semantic-snapshot
          (dorg-test-roundtrip-corpus--read-file actual))))
    (unless (equal expected-snapshot actual-snapshot)
      (error "semantic mismatch between %s and %s" expected actual))))

(defun dorg-test-roundtrip-corpus-batch-assert-files (expected actual)
  "Batch entry point: assert EXPECTED and ACTUAL files are semantically equal."
  (condition-case err
      (progn
        (dorg-test-roundtrip-corpus--assert-files-equivalent expected actual)
        (message "semantic-roundtrip: PASS %s %s" expected actual)
        (kill-emacs 0))
    (error
     (message "semantic-roundtrip: FAIL %s" (error-message-string err))
     (kill-emacs 1))))

(defun dorg-test-roundtrip-corpus--import-render (text &optional after-import)
  "Import TEXT into a temp Dolt store and return rendered text.
AFTER-IMPORT, when non-nil, is called with the temp dolt dir after the first
commit and before rendering.  Returns (RENDERED COMMIT DOLT-DIR)."
  (unless dorg-test--dolt-available
    (ert-skip "dolt not installed"))
  (dorg-test--with-temp-db dolt-dir
    (let ((stmts (dorg-test--parse-to-sql text)))
      (when stmts
        (dorg-sql-batch dolt-dir stmts "semantic corpus import")))
    (let ((commit (dorg-head-commit dolt-dir)))
      (when after-import
        (funcall after-import dolt-dir))
      (list (dorg-render dolt-dir) commit dolt-dir))))

(defun dorg-test-roundtrip-corpus--fixture-contents (name)
  "Return fixture contents for NAME."
  (let ((relative (alist-get name dorg-test-roundtrip-corpus--fixtures nil nil #'string=)))
    (unless relative
      (error "unknown semantic corpus fixture %s" name))
    (dorg-test-roundtrip-corpus--read-file
     (dorg-test-roundtrip-corpus--fixture-path relative))))

(defconst dorg-test-roundtrip-corpus--mutation-base
  (concat "#+title: Mutation Base\n"
          "#+startup: showall\n"
          "#+startup: logdone\n"
          "\n"
          "* TODO [#A] Alpha :one:two:\n"
          "SCHEDULED: <2026-07-10 Fri> DEADLINE: <2026-07-11 Sat>\n"
          ":PROPERTIES:\n"
          ":ZED: 1\n"
          ":ALPHA: 2\n"
          ":END:\n"
          ":LOGBOOK:\n"
          "line one\n"
          "line two\n"
          ":END:\n"
          "Paragraph A.\n"
          "\n"
          "- first\n"
          "- second\n"
          "\n"
          "#+begin_example\n"
          "payload line\n"
          "#+end_example\n"
          "\n"
          "** DONE Child\n"
          "Child body.\n"))

(ert-deftest dorg-test-roundtrip-corpus-oracle-detects-semantic-mutations ()
  "Projection changes when semantic fields change."
  (let ((base (dorg-test-roundtrip-corpus--semantic-snapshot
               dorg-test-roundtrip-corpus--mutation-base)))
    (dolist
        (mutated
         (list
          (replace-regexp-in-string "^\\* TODO \\[#A\\] Alpha :one:two:\n" ""
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "Alpha :one:two:" "Alpha changed :one:two:"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "TODO \\[#A\\]" "DONE [#B]"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string ":one:two:" ":two:one:"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "#\\+startup: showall\n#\\+startup: logdone"
                                    "#+startup: logdone\n#+startup: showall"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string ":ALPHA: 2" ":ALPHA: changed"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string ":ZED: 1\n:ALPHA: 2" ":ALPHA: 2\n:ZED: 1"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "<2026-07-10 Fri>" "<2026-07-12 Sun>"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "line two" "line changed"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "payload line" "payload changed"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "- first\n- second" "- second\n- first"
                                    dorg-test-roundtrip-corpus--mutation-base)
          (replace-regexp-in-string "Paragraph A\\." "Paragraph changed."
                                    dorg-test-roundtrip-corpus--mutation-base)))
      (should-not (equal base
                         (dorg-test-roundtrip-corpus--semantic-snapshot mutated))))))

(ert-deftest dorg-test-roundtrip-corpus-direct-dorg ()
  "Fixed corpus survives parse -> Dolt -> render -> parse semantically."
  (dolist (fixture dorg-test-roundtrip-corpus--fixtures)
    (let* ((text (dorg-test-roundtrip-corpus--read-file
                  (dorg-test-roundtrip-corpus--fixture-path (cdr fixture))))
           (expected (dorg-test-roundtrip-corpus--semantic-snapshot text))
           (rendered+commit (dorg-test-roundtrip-corpus--import-render text))
           (rendered (car rendered+commit))
           (commit (cadr rendered+commit)))
      (should (stringp commit))
      (should-not (string-empty-p commit))
      ;; This is a read-source smoke check, not the semantic oracle.
      (should-not (equal rendered text))
      (should (equal expected
                     (dorg-test-roundtrip-corpus--semantic-snapshot rendered))))))

(ert-deftest dorg-test-roundtrip-corpus-render-reads-dolt-store ()
  "Out-of-band Dolt mutation proves render reads storage, not input text."
  (let* ((text (dorg-test-roundtrip-corpus--fixture-contents "mixed"))
         (original (dorg-test-roundtrip-corpus--semantic-snapshot text))
         (rendered+commit
          (dorg-test-roundtrip-corpus--import-render
           text
           (lambda (dolt-dir)
             (dorg-sql-batch
              dolt-dir
              (list "UPDATE headings SET title = 'Project Alpha From Store' WHERE title = 'Project Alpha'")
              "semantic corpus out-of-band mutation"))))
         (rendered (car rendered+commit))
         (after (dorg-test-roundtrip-corpus--semantic-snapshot rendered)))
    (should-not (equal original after))
    (should (cl-some (lambda (heading)
                       (equal (plist-get (cdr heading) :title)
                              "Project Alpha From Store"))
                     after))))

(provide 'dorg-roundtrip-corpus-tests)
;;; dorg-roundtrip-corpus-tests.el ends here
