;;; dorg-sql-gen-tests.el --- Tests for dorg-sql-gen -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dedicated tests for the SQL generation module.  Created by tracked-regression;
;; until the relocation lands this file carries the Family D relocation
;; contract guard, and the lowering tests move here with the implementation.
;;
;; The guard assembles the retired names from fragments so a post-relocation
;; repository grep for the full old names is zero-hit, including this file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-sortkey)
(require 'dorg-sql-gen)

(defconst dorg-sql-gen-tests--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-sql-gen-tests--name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-sql-gen-tests--repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-sql-gen-relocation-contract ()
  "tracked-regression Family D: the lowering cluster lives in dorg-sql-gen.el.
RED at SPEC (new names undefined, retired names still defined and
referenced); GREEN after IMPL (lowering under the sql-gen prefix, the
sync-prefixed originals deleted repository-wide)."
  (let ((pairs '(("-lower-core-ops"      . dorg-sql-gen-lower-heading-ops)
                 ("-lower-item-plan"     . dorg-sql-gen--lower-item-plan)
                 ("-item-plan-possible-p" . dorg-sql-gen--item-plan-possible-p)
                 ("-sort-insert-groups"  . dorg-sql-gen--sort-insert-groups)))
        (old-prefix (concat "dorg-sync" "-")))
    (dolist (p pairs)
      (let ((old (intern-soft (concat old-prefix (car p)))))
        (should (fboundp (cdr p)))
        (should-not (and old (fboundp old)))))
    (should-not (dorg-sql-gen-tests--name-hits
                 (mapcar (lambda (p) (concat old-prefix (car p))) pairs)))))

;;; ─── Direct lowerer tests (moved by tracked-regression) ─────────────────────

(ert-deftest dorg-test-sql-gen-lower-item-plan-no-whole-heading-delete ()
  "Changed-heading lowering with loadable item ids emits no whole-heading
item delete (AC11).  When the DB heading carries item objects with ids, an
item-level plan replaces the DELETE FROM heading_items WHERE heading_id."
  (let* ((db-h (make-dorg-heading
                :id 1 :level 1 :title "H"
                :items (list (dorg-text-item :id 100 :sort-key (dorg-sortkey-from-integer 1)
                                             :text "A")
                             (dorg-text-item :id 101 :sort-key (dorg-sortkey-from-integer 2)
                                             :text "B"))
                :items-hash "OLDHASH"))
         (new-h (make-dorg-heading
                 :id 1 :level 1 :title "H"
                 :items (list (dorg-text-item :text "A")
                              (dorg-text-item :text "B-edited"))
                 :items-hash "NEWHASH"))
         (db-headings (make-hash-table :test 'eql))
         (counter 5000)
         (id-gen (lambda () (cl-incf counter))))
    (puthash 1 db-h db-headings)
    (let* ((stmts (dorg-sql-gen-lower-heading-ops
                   (list (list :update-heading new-h)) db-headings id-gen))
           (sql (mapconcat #'identity stmts "\n")))
      (should-not (string-match-p "DELETE FROM heading_items WHERE heading_id" sql))
      ;; The reused item id 101 receives a same-kind text update.
      (should (string-match-p "UPDATE text_items SET text_raw = .*WHERE item_id = 101" sql)))))

(ert-deftest dorg-test-sql-gen-lower-item-plan-fallback-when-no-ids ()
  "Lowering falls back to whole-heading delete when DB items are absent (AC11).
A legacy DB heading with nil items cannot be planned, so the safe
delete-all/reinsert path is kept."
  (let* ((db-h (make-dorg-heading :id 1 :level 1 :title "H"
                                  :items nil :items-hash "OLDHASH"))
         (new-h (make-dorg-heading
                 :id 1 :level 1 :title "H"
                 :items (list (dorg-text-item :text "A"))
                 :items-hash "NEWHASH"))
         (db-headings (make-hash-table :test 'eql))
         (counter 5000)
         (id-gen (lambda () (cl-incf counter))))
    (puthash 1 db-h db-headings)
    (let* ((stmts (dorg-sql-gen-lower-heading-ops
                   (list (list :update-heading new-h)) db-headings id-gen))
           (sql (mapconcat #'identity stmts "\n")))
      (should (string-match-p "DELETE FROM heading_items WHERE heading_id = 1" sql)))))

(provide 'dorg-sql-gen-tests)
;;; dorg-sql-gen-tests.el ends here
