;;; dorg-doctor-tests.el --- Tests for dorg-doctor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the dorg-doctor consistency checker.

;;; Code:

(require 'ert)
(require 'dorg-doctor)
(require 'dorg-dolt)
(require 'dorg-repo)
(require 'dorg-id)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; Helpers

(defmacro dorg-test-with-doctor-repo (&rest body)
  "Create a dolt repo with test data and execute BODY.
Binds `dolt-dir'.  Seeds one valid file root with a child heading.
Skips if dolt is not available."
  (declare (indent 0))
  `(if (not dorg-test--dolt-available)
       (ert-skip "dolt not installed")
     (dorg-test--with-temp-db dolt-dir
       ;; Seed a valid root + child + dorg_files entry
       (let ((root-id 1000)
             (child-id 1001))
         (dorg-sql-batch dolt-dir
           (list
            (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (%d, %d, NULL, X'01', 0, FALSE, NULL, 0)"
                    root-id root-id)
            (format "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (%d, %d, %d, X'01', 1, FALSE, 'Child', 0)"
                    child-id root-id root-id)
            (format "INSERT INTO dorg_files (root_id, relative_path) VALUES (%d, 'doc.org')"
                    root-id)))
         (dorg-commit dolt-dir "seed"))
       ,@body)))

;;; Tests — Clean repo

(ert-deftest dorg-test-doctor-clean-repo ()
  "Doctor reports no issues on a consistent repo."
  (dorg-test-with-doctor-repo
    (let ((findings (dorg-doctor dolt-dir)))
      (should (null findings)))))

;;; Tests — Orphan roots

(ert-deftest dorg-test-doctor-orphan-root ()
  "Doctor detects root headings with no dorg_files entry."
  (dorg-test-with-doctor-repo
    ;; Insert an orphan root (no dorg_files row)
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let* ((findings (dorg-doctor dolt-dir))
           (orphans (cl-remove-if-not
                     (lambda (f) (equal (plist-get f :check) "orphan-root"))
                     findings)))
      (should (= (length orphans) 1))
      (should (plist-get (car orphans) :fix-sql)))))

(ert-deftest dorg-test-doctor-orphan-root-fix ()
  "Doctor fixes orphan roots by deleting them."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (dorg-doctor dolt-dir t)
    ;; Orphan should be gone
    (let ((rows (dorg-sql dolt-dir "SELECT id FROM headings WHERE id = 9900")))
      (should (null rows)))
    ;; Original data intact
    (let ((rows (dorg-sql dolt-dir "SELECT id FROM headings WHERE id = 1000")))
      (should (= (length rows) 1)))))

;;; Tests — Root invariant violations

(ert-deftest dorg-test-doctor-root-invariant ()
  "Doctor detects root headings with wrong level or parent_id."
  (dorg-test-with-doctor-repo
    ;; Break the valid root: set level=1
    (dorg-sql dolt-dir "UPDATE headings SET level = 1 WHERE id = 1000")
    (dorg-commit dolt-dir "break root")
    (let* ((findings (dorg-doctor dolt-dir))
           (violations (cl-remove-if-not
                        (lambda (f) (equal (plist-get f :check) "root-invariant"))
                        findings)))
      (should (= (length violations) 1))
      (should (plist-get (car violations) :fix-sql)))))

(ert-deftest dorg-test-doctor-root-invariant-fix ()
  "Doctor fixes root invariant violations."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir "UPDATE headings SET level = 3, parent_id = 1001 WHERE id = 1000")
    (dorg-commit dolt-dir "break root")
    (dorg-doctor dolt-dir t)
    (let* ((rows (dorg-sql dolt-dir "SELECT level, parent_id FROM headings WHERE id = 1000"))
           (row (car rows)))
      (should (= (alist-get 'level row) 0))
      (should (null (alist-get 'parent_id row))))))

;;; Tests — Dangling parent_id

(ert-deftest dorg-test-doctor-dangling-parent ()
  "Doctor detects headings referencing non-existent parents."
  (dorg-test-with-doctor-repo
    ;; Point child at non-existent parent (disable FK checks in same batch)
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "UPDATE headings SET parent_id = 99999 WHERE id = 1001"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "break parent")
    (let* ((findings (dorg-doctor dolt-dir))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-parent"))
                      findings)))
      (should (= (length dangling) 1))
      ;; Not fixable
      (should (null (plist-get (car dangling) :fix-sql))))))

(ert-deftest dorg-test-doctor-dangling-parent-not-double-reported ()
  "A pure dangling parent is reported once, not also as unreachable.
The reachability check must defer to `dangling-parent' for chains that
hit a missing parent, preserving existing diagnostic behavior."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "UPDATE headings SET parent_id = 99999 WHERE id = 1001"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "break parent")
    (let* ((findings (dorg-doctor dolt-dir))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-parent"))
                      findings))
           (unreachable (cl-remove-if-not
                         (lambda (f) (equal (plist-get f :check) "unreachable-heading"))
                         findings)))
      (should (= (length dangling) 1))
      (should (null unreachable)))))

;;; Tests — Unreachable headings (cycles / disconnected islands)

(ert-deftest dorg-test-doctor-disconnected-island ()
  "Doctor reports an island whose internal parents exist but never reach root."
  (dorg-test-with-doctor-repo
    ;; 2000 -> 2001 -> NULL.  Both parents exist; chain never reaches
    ;; the declared root 1000, so both are non-root-reachable.
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (2000, 1000, 2001, X'02', 1, FALSE, 'IslandA', 0)"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (2001, 1000, NULL, X'03', 2, FALSE, 'IslandB', 0)"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add island")
    (let* ((findings (dorg-doctor dolt-dir))
           (unreachable (cl-remove-if-not
                         (lambda (f) (equal (plist-get f :check) "unreachable-heading"))
                         findings))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-parent"))
                      findings)))
      (should (= (length unreachable) 2))
      (should (null dangling))
      (dolist (f unreachable)
        (should (eq (plist-get f :severity) 'error))
        (should (null (plist-get f :fix-sql)))
        (should (string-match-p "disconnected" (plist-get f :message)))))))

(ert-deftest dorg-test-doctor-cross-root-chain ()
  "Doctor reports a heading whose parent chain reaches a different root."
  (dorg-test-with-doctor-repo
    ;; Second valid root 3000; heading 2002 in root 1000 points at it.
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (3000, 3000, NULL, X'01', 0, FALSE, NULL, 0)"
        "INSERT INTO dorg_files (root_id, relative_path) VALUES (3000, 'doc2.org')"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (2002, 1000, 3000, X'04', 1, FALSE, 'CrossRoot', 0)"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add cross-root child")
    (let* ((findings (dorg-doctor dolt-dir))
           (unreachable (cl-remove-if-not
                         (lambda (f) (equal (plist-get f :check) "unreachable-heading"))
                         findings)))
      (should (= (length unreachable) 1))
      (let ((f (car unreachable)))
        (should (string-match-p "2002" (plist-get f :message)))
        (should (string-match-p "disconnected" (plist-get f :message)))
        (should (null (plist-get f :fix-sql)))))))

(ert-deftest dorg-test-doctor-parent-cycle ()
  "Doctor reports a parent cycle even when every immediate parent exists."
  (dorg-test-with-doctor-repo
    ;; 2000 <-> 2001 cycle, both within root 1000.
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (2000, 1000, 2001, X'02', 1, FALSE, 'CycA', 0)"
        "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (2001, 1000, 2000, X'03', 1, FALSE, 'CycB', 0)"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add cycle")
    (let* ((findings (dorg-doctor dolt-dir))
           (unreachable (cl-remove-if-not
                         (lambda (f) (equal (plist-get f :check) "unreachable-heading"))
                         findings))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-parent"))
                      findings)))
      (should (= (length unreachable) 2))
      (should (null dangling))
      (dolist (f unreachable)
        (should (string-match-p "cycle" (plist-get f :message)))
        (should (null (plist-get f :fix-sql)))))))

;;; Tests — Dangling heading_items

(ert-deftest dorg-test-doctor-dangling-items ()
  "Doctor detects heading_items referencing non-existent headings."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (5000, 88888, X'01', 'text', 0)"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add dangling item")
    (let* ((findings (dorg-doctor dolt-dir))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-item"))
                      findings)))
      (should (= (length dangling) 1))
      (should (plist-get (car dangling) :fix-sql)))))

(ert-deftest dorg-test-doctor-dangling-items-fix ()
  "Doctor fixes dangling heading_items by deleting them."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO heading_items (id, heading_id, sort_key, kind, post_blank) VALUES (5000, 88888, X'01', 'text', 0)"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add dangling item")
    (dorg-doctor dolt-dir t)
    (let ((rows (dorg-sql dolt-dir "SELECT id FROM heading_items WHERE id = 5000")))
      (should (null rows)))))

;;; Tests — Dangling detail rows

(ert-deftest dorg-test-doctor-dangling-detail ()
  "Doctor detects detail rows referencing non-existent items."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO text_items (item_id, text_raw) VALUES (77777, 'orphan text')"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add dangling detail")
    (let* ((findings (dorg-doctor dolt-dir))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-detail"))
                      findings)))
      (should (= (length dangling) 1))
      (should (plist-get (car dangling) :fix-sql)))))

;;; Tests — Dangling drawer children

(ert-deftest dorg-test-doctor-dangling-drawer-children ()
  "Doctor detects drawer_lines/property_entries with no parent drawer."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "INSERT INTO drawer_lines (drawer_item_id, ordinal, line_raw) VALUES (66666, 1, 'orphan line')"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "add dangling drawer child")
    (let* ((findings (dorg-doctor dolt-dir))
           (dangling (cl-remove-if-not
                      (lambda (f) (equal (plist-get f :check) "dangling-drawer-child"))
                      findings)))
      (should (= (length dangling) 1))
      (should (plist-get (car dangling) :fix-sql)))))

;;; Tests — Case-duplicate dorg_files

(ert-deftest dorg-test-doctor-case-duplicate-files ()
  "Doctor flags dorg_files rows whose paths are case-insensitive duplicates.
The seed root 1000 maps /test/doc.org; a blank case-variant root 1500 maps
/TEST/Doc.org for the same physical file."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      (list
       "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (1500, 1500, NULL, X'01', 0, FALSE, NULL, 0)"
       "INSERT INTO dorg_files (root_id, relative_path) VALUES (1500, 'Doc.org')"))
    (dorg-commit dolt-dir "add case-variant dup")
    (let* ((findings (dorg-doctor dolt-dir))
           (dups (cl-remove-if-not
                  (lambda (f) (equal (plist-get f :check) "case-duplicate-file"))
                  findings)))
      (should (= (length dups) 1))
      (let ((msg (plist-get (car dups) :message)))
        (should (string-match-p "1000" msg))
        (should (string-match-p "1500" msg))))))

(ert-deftest dorg-test-doctor-no-case-duplicate-when-distinct ()
  "Doctor reports no case-duplicate finding when paths are genuinely distinct."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      (list
       "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (1500, 1500, NULL, X'01', 0, FALSE, NULL, 0)"
       "INSERT INTO dorg_files (root_id, relative_path) VALUES (1500, 'other.org')"))
    (dorg-commit dolt-dir "add distinct file")
    (let* ((findings (dorg-doctor dolt-dir))
           (dups (cl-remove-if-not
                  (lambda (f) (equal (plist-get f :check) "case-duplicate-file"))
                  findings)))
      (should (null dups)))))

(ert-deftest dorg-test-doctor-reports-malformed-store-relative-binding ()
  "Doctor routes persisted locator validation through the repository decoder."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
              "UPDATE dorg_files SET relative_path = 'x/../doc.org' WHERE root_id = 1000")
    (dorg-commit dolt-dir "seed malformed relative binding")
    (let* ((findings (dorg-doctor dolt-dir))
           (invalid
            (cl-remove-if-not
             (lambda (finding)
               (equal (plist-get finding :check) "invalid-file-binding"))
             findings)))
      (should (= 1 (length invalid)))
      (should (string-match-p "x/\\.\\./doc\\.org"
                              (plist-get (car invalid) :message))))))

;;; Tests — Report format

(ert-deftest dorg-test-doctor-report-clean ()
  "Clean repo report says no issues."
  (dorg-test-with-doctor-repo
    (dorg-doctor dolt-dir)
    (with-current-buffer "*dorg-doctor*"
      (should (string-match-p "No issues found" (buffer-string))))))

(ert-deftest dorg-test-doctor-report-issues ()
  "Report lists findings with severity."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (dorg-doctor dolt-dir)
    (with-current-buffer "*dorg-doctor*"
      (let ((text (buffer-string)))
        (should (string-match-p "1 issue" text))
        (should (string-match-p "\\[WARN\\]" text))
        (should (string-match-p "fixable" text))))))

;;; Tests — shared run/fix/re-check core (tracked-case)

(ert-deftest dorg-test-doctor-apply-returns-postfix-findings ()
  "Interactive `dorg-doctor' with APPLY returns the POST-fix findings.
The only issue is a fixable orphan root; after applying, the re-checked
repo is clean, so the return value must be nil — not the pre-fix orphan
finding.  Non-vacuity: fails against the pre-qwhnl code, which bound
`findings' once and returned the stale pre-fix list."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    ;; Sanity: the orphan is present and fixable before the fix.
    (should (cl-some (lambda (f) (equal (plist-get f :check) "orphan-root"))
                     (dorg-doctor dolt-dir)))
    ;; After APPLY the return reflects the re-checked (clean) state.
    (should (null (dorg-doctor dolt-dir t)))))

(ert-deftest dorg-test-doctor-apply-buffer-reflects-postfix ()
  "The *dorg-doctor* buffer reflects POST-fix state after APPLY.
It must show the clean re-check report (\"No issues found\") plus the
\"Applied 1 fix\" note — not the pre-fix orphan listing.  Non-vacuity:
the pre-qwhnl code rendered the pre-fix report and only appended the note."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (dorg-doctor dolt-dir t)
    (with-current-buffer "*dorg-doctor*"
      (let ((text (buffer-string)))
        (should (string-match-p "No issues found" text))
        (should (string-match-p "Applied 1 fix" text))
        (should-not (string-match-p "orphan-root\\|no dorg_files" text))))))

(ert-deftest dorg-test-doctor-run-fix-recheck-applies ()
  "`dorg-doctor--run-fix-recheck' applies + re-checks when SHOULD-APPLY is t.
Returns (:applied 1) and POST-fix findings with the orphan gone; the DB
row is actually deleted."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let* ((seen 'unset)
           (result (dorg-doctor--run-fix-recheck
                    dolt-dir (lambda (findings) (setq seen findings) t))))
      ;; AC4: the seam received the INITIAL fixable findings (not nil / not
      ;; the post-fix list) so a caller may render them before deciding.
      (should (cl-some (lambda (f) (equal (plist-get f :check) "orphan-root"))
                       seen))
      (should (= 1 (plist-get result :applied)))
      (should-not (cl-some (lambda (f) (equal (plist-get f :check) "orphan-root"))
                           (plist-get result :findings)))
      (should (null (dorg-sql dolt-dir
                      "SELECT id FROM headings WHERE id = 9900"))))))

(ert-deftest dorg-test-doctor-run-fix-recheck-declined ()
  "`dorg-doctor--run-fix-recheck' leaves state untouched when SHOULD-APPLY is nil.
Returns (:applied 0) and the pre-fix findings (orphan still present); the
DB row survives."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let ((result (dorg-doctor--run-fix-recheck dolt-dir (lambda (_f) nil))))
      (should (= 0 (plist-get result :applied)))
      (should (cl-some (lambda (f) (equal (plist-get f :check) "orphan-root"))
                       (plist-get result :findings)))
      (should (dorg-sql dolt-dir
                "SELECT id FROM headings WHERE id = 9900")))))

(ert-deftest dorg-test-doctor-run-fix-recheck-no-fixable-skips-callback ()
  "`dorg-doctor--run-fix-recheck' never calls SHOULD-APPLY with no fixable finding.
A dangling-parent is a non-fixable (`:fix-sql' nil) error, so there is
nothing to confirm: the decision seam must not fire, and :applied is 0."
  (dorg-test-with-doctor-repo
    (dorg-sql-batch dolt-dir
      '("SET FOREIGN_KEY_CHECKS=0"
        "UPDATE headings SET parent_id = 99999 WHERE id = 1001"
        "SET FOREIGN_KEY_CHECKS=1"))
    (dorg-commit dolt-dir "break parent")
    (let* ((called nil)
           (result (dorg-doctor--run-fix-recheck
                    dolt-dir (lambda (_f) (setq called t) t))))
      (should-not called)
      (should (= 0 (plist-get result :applied)))
      (should (cl-some (lambda (f) (equal (plist-get f :check) "dangling-parent"))
                       (plist-get result :findings))))))

(ert-deftest dorg-test-doctor-run-fix-recheck-passes-initial-findings ()
  "AC4: the core hands SHOULD-APPLY the INITIAL pre-fix findings verbatim.
The interactive pre-prompt rendering depends on this value, so the seam must
see exactly what `dorg-doctor--run-checks' produced before any fix — not nil
and not the post-fix list.  Declines (SHOULD-APPLY => nil) to keep the DB
pristine so the captured findings can be compared against a fresh pre-fix
run.  Non-vacuity: void-function pre-impl; an impl passing nil or the
re-checked list fails the `equal'."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let ((seen 'unset)
          (expected (dorg-doctor--run-checks dolt-dir)))
      (dorg-doctor--run-fix-recheck
       dolt-dir (lambda (findings) (setq seen findings) nil))
      (should (equal seen expected))
      (should (cl-some (lambda (f) (equal (plist-get f :check) "orphan-root"))
                       seen)))))

;;; Tests — interactive call-interactively regression (AC1/AC2)

(ert-deftest dorg-test-doctor-call-interactively-applies-on-yes ()
  "Through `call-interactively', `dorg-doctor' reaches `yes-or-no-p' and a
`yes' answer applies the fix.  Regression guard: `called-interactively-p' is
t in `dorg-doctor' but nil inside a closure the shared core invokes (verified
outer=t / closure=nil), so interactivity MUST be captured at entry.  An impl
that tests interactivity inside the decision callback silently stops prompting
and never applies — this test then fails (orphan survives)."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) dolt-dir))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq prompted t) t)))
        (call-interactively #'dorg-doctor))
      ;; The prompt was reached...
      (should prompted)
      ;; ...and answering `yes' applied the fix (orphan row gone).
      (should (null (dorg-sql dolt-dir
                      "SELECT id FROM headings WHERE id = 9900"))))))

(ert-deftest dorg-test-doctor-call-interactively-declines-on-no ()
  "The interactive prompt answer CONTROLS application: a `no' answer leaves
the DB untouched.  Paired with the `yes' test above this is a before/after
transformation — same fixture, opposite answers, opposite DB state — proving
the prompt result drives the fix rather than being ignored."
  (dorg-test-with-doctor-repo
    (dorg-sql dolt-dir
      "INSERT INTO headings (id, root_id, parent_id, sort_key, level, commented, title, post_blank) VALUES (9900, 9900, NULL, X'01', 0, FALSE, NULL, 0)")
    (dorg-commit dolt-dir "add orphan")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) dolt-dir))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq prompted t) nil)))
        (call-interactively #'dorg-doctor))
      (should prompted)
      ;; Declined → orphan survives.
      (should (dorg-sql dolt-dir
                "SELECT id FROM headings WHERE id = 9900")))))

;;; Tests — mysql.el hard-dependency check (tracked-case)

(ert-deftest dorg-test-doctor-mysql-unavailable-finding ()
  "`dorg-doctor--mysql-available' returns an ERROR finding when mysql.el is
not loadable — the k5cp4 must-error contract surfaced in the doctor."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () nil)))
    (let ((findings (dorg-doctor--mysql-available "/ignored")))
      (should (= 1 (length findings)))
      (should (equal "mysql-unavailable" (plist-get (car findings) :check)))
      (should (eq 'error (plist-get (car findings) :severity)))
      (should (string-match-p "mysql\\.el" (plist-get (car findings) :message))))))

(ert-deftest dorg-test-doctor-mysql-available-clean ()
  "`dorg-doctor--mysql-available' returns no finding when mysql.el is loadable."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t)))
    (should (null (dorg-doctor--mysql-available "/ignored")))))

(ert-deftest dorg-test-doctor-mysql-check-registered ()
  "The mysql availability check is registered in `dorg-doctor--checks', so
`dorg-doctor--run-checks' runs it on every doctor invocation."
  (should (memq 'dorg-doctor--mysql-available dorg-doctor--checks)))

(ert-deftest dorg-test-doctor-report-shows-mysql-present ()
  "The clean doctor report affirmatively states mysql.el presence, so
`dorg doctor' reports presence and not only problems."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t)))
    (let ((report (dorg-doctor--format-report nil "/some/dolt")))
      (should (string-match-p "mysql\\.el: present" report))
      (should (string-match-p "No issues found" report)))))

(ert-deftest dorg-test-doctor-report-shows-mysql-with-findings ()
  "The mysql.el presence line is a header/environment line — it appears in the
HAS-FINDINGS report too, not only the clean one (REVIEW-1 blocker)."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t)))
    (let ((report (dorg-doctor--format-report
                   (list (list :check "orphan-root" :severity 'warn
                               :message "orphan" :fix-sql nil))
                   "/some/dolt")))
      (should (string-match-p "mysql\\.el: present" report))
      (should (string-match-p "1 issue" report))
      (should (string-match-p "\\[WARN\\]" report)))))

;;; Tests — install-story remediation + env verification (tracked-case, RED SPEC)
;; The mysql.el HARD-dependency failure path must NAME the installer as the
;; remediation, and `dorg doctor' must offer an env-only verification (no repo)
;; so the installer can finish with `dorg doctor'.

(ert-deftest dorg-test-doctor-mysql-finding-names-installer ()
  "The mysql-unavailable doctor finding names the installer as remediation.
Non-vacuous: the old message (\"install mysql.el or restore lib/mysql\") does
NOT reference the installer, so this fails until the message is updated."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () nil)))
    (let ((msg (plist-get (car (dorg-doctor--mysql-available "/ignored")) :message)))
      (should (string-match-p "install\\.sh\\|make install" msg)))))

(ert-deftest dorg-test-doctor-mysql-hard-error-message-names-installer ()
  "The fatal-at-load mysql.el error message (dorg-sql--require-mysql-hard) names
the installer and keeps the HARD-dependency / lib/mysql context."
  (let ((msg (dorg-sql--mysql-hard-error-message "/x/lib/mysql" "boom")))
    (should (string-match-p "mysql\\.el" msg))
    (should (string-match-p "lib/mysql" msg))
    (should (string-match-p "install\\.sh\\|make install" msg))
    (should (string-match-p "boom" msg))))

(ert-deftest dorg-test-doctor-env-findings-clean-when-mysql-present ()
  "`dorg-doctor--env-findings' has NO error finding when mysql.el is present
\(dolt presence is a warn at most) — the healthy post-install case."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t)))
    (let ((findings (dorg-doctor--env-findings)))
      (should-not (cl-some (lambda (f) (eq 'error (plist-get f :severity)))
                           findings)))))

(ert-deftest dorg-test-doctor-env-findings-error-when-mysql-absent ()
  "`dorg-doctor--env-findings' surfaces the mysql-unavailable ERROR (naming the
installer) with no repo — the install-integrity failure the installer reports."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () nil)))
    (let* ((findings (dorg-doctor--env-findings))
           (err (cl-find "mysql-unavailable" findings
                         :key (lambda (f) (plist-get f :check)) :test #'equal)))
      (should err)
      (should (eq 'error (plist-get err :severity)))
      (should (string-match-p "install\\.sh\\|make install" (plist-get err :message))))))

(ert-deftest dorg-test-doctor-env-findings-warn-when-dolt-absent ()
  "With mysql.el present but no supported dolt, env-findings has a WARN (not an
error) — the Dolt backend is optional; plain-org needs no dolt."
  (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda () t))
            ((symbol-function 'dorg-dolt-available-p) (lambda () nil))
            ((symbol-function 'dorg-dolt-supported-p) (lambda () nil)))
    (let ((findings (dorg-doctor--env-findings)))
      (should-not (cl-some (lambda (f) (eq 'error (plist-get f :severity))) findings))
      (should (cl-some (lambda (f) (and (eq 'warn (plist-get f :severity))
                                        (string-match-p "[Dd]olt"
                                                        (or (plist-get f :message) ""))))
                       findings)))))

(provide 'dorg-doctor-tests)
;;; dorg-doctor-tests.el ends here
