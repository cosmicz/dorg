;;; dorg-doctor.el --- Diagnose and fix dorg DB consistency -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interactive command `dorg-doctor' inspects a dorg database for
;; consistency problems and optionally applies fixes.
;;
;; Each check function returns a list of finding plists:
;;   (:check NAME :severity warn|error :message STRING :fix-sql STRING|nil)
;;
;; Fixable findings have a non-nil :fix-sql.  Fixes are applied in a
;; single transaction after user confirmation.

;;; Code:

(require 'cl-lib)
(require 'dorg-backend)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-model)
(require 'dorg-repo)
(require 'dorg-registry)

;;; ─── Individual Checks ──────────────────────────────────────────────

(defun dorg-doctor--orphan-roots (dolt-dir)
  "Find root headings with no dorg_files entry in DOLT-DIR."
  (let ((rows (dorg-sql dolt-dir
                "SELECT h.id FROM headings h \
WHERE h.id = h.root_id AND h.level_offset = 0 \
AND h.id NOT IN (SELECT root_id FROM dorg_files)")))
    (mapcar (lambda (row)
              (let ((id (alist-get 'id row)))
                (list :check "orphan-root"
                      :severity 'warn
                      :message (format "Root heading %d has no dorg_files entry" id)
                      :fix-sql (format "DELETE FROM headings WHERE id = %d" id))))
            rows)))

(defun dorg-doctor--case-duplicate-files (dolt-dir)
  "Find case-equivalent relative bindings in DOLT-DIR.
On a case-insensitive filesystem two letter-case variants of one physical
file can be stored as distinct rows — the `relative_path' column uses a
binary, case-sensitive collation — producing a blank phantom root alongside
the populated one.  Strict binding refuses the ambiguity; this check surfaces
it.  Reported as a warning with no automated fix because choosing an identity
is an operator decision."
  (let ((rows (dorg-sql dolt-dir
                "SELECT a.root_id AS root_id, a.relative_path AS relative_path, \
b.root_id AS other_root, b.relative_path AS other_relative_path \
FROM dorg_files a JOIN dorg_files b \
ON a.root_id < b.root_id \
AND LOWER(a.relative_path) = LOWER(b.relative_path)")))
    (mapcar (lambda (row)
              (let ((ra (alist-get 'root_id row))
                    (pa (alist-get 'relative_path row))
                    (rb (alist-get 'other_root row))
                    (pb (alist-get 'other_relative_path row)))
                (list :check "case-duplicate-file"
                      :severity 'warn
                      :message (format "Case-duplicate dorg_files relative paths: root %s (%s) and root %s (%s) map the same physical file"
                                       ra pa rb pb)
                      :fix-sql nil)))
            rows)))

(defun dorg-doctor--invalid-file-bindings (dolt-dir)
  "Find malformed locators or invalid bound roots in DOLT-DIR.
The same repository decoder used by bind/import validates persisted bytes;
doctor reports the typed failure and never guesses a repair."
  (let ((rows
         (dorg-sql
          dolt-dir
          (concat "SELECT f.root_id AS root_id, "
                  "f.relative_path AS relative_path, h.id AS heading_id, "
                  "h.root_id AS heading_root_id, "
                  "h.parent_id AS parent_id, h.level_offset AS level_offset "
                  "FROM dorg_files f LEFT JOIN headings h "
                  "ON h.id = f.root_id ORDER BY f.root_id")))
        findings)
    (dolist (row rows (nreverse findings))
      (let ((root-id (alist-get 'root_id row))
            (relative (alist-get 'relative_path row)))
        (cond
         ((not (and (integerp root-id)
                    (> root-id 0)
                    (eql root-id (alist-get 'heading_id row))
                    (eql root-id (alist-get 'heading_root_id row))
                    (null (alist-get 'parent_id row))
                    (zerop (or (alist-get 'level_offset row) -1))))
          (push (list :check "invalid-file-binding"
                      :severity 'error
                      :message (format "dorg_files root %S is not a valid level-0 root"
                                       root-id)
                      :fix-sql nil)
                findings))
         (t
          (condition-case err
              (dorg-repo-decode-relative-path dolt-dir relative)
            (dorg-binding-error
             (push (list :check "invalid-file-binding"
                         :severity 'error
                         :message (format "Root %s has invalid relative binding %S: %s"
                                          root-id relative
                                          (error-message-string err))
                         :fix-sql nil)
                   findings)))))))))

(defun dorg-doctor--root-invariants (dolt-dir)
  "Find root headings violating level_offset=0 / parent_id=NULL in DOLT-DIR."
  (let ((rows (dorg-sql dolt-dir
                "SELECT id, level_offset, parent_id FROM headings \
WHERE id = root_id AND (level_offset != 0 OR parent_id IS NOT NULL)")))
    (mapcar (lambda (row)
              (let ((id (alist-get 'id row))
                    (level (alist-get 'level_offset row))
                    (pid (alist-get 'parent_id row)))
                (list :check "root-invariant"
                      :severity 'error
                      :message (format "Root %d: level_offset=%d parent_id=%s (expected 0/NULL)"
                                       id level (if pid (number-to-string pid) "NULL"))
                      :fix-sql (format "UPDATE headings SET level_offset = 0, parent_id = NULL WHERE id = %d"
                                       id))))
            rows)))

(defun dorg-doctor--nonpositive-offsets (dolt-dir)
  "Find non-root headings whose level offset is not positive in DOLT-DIR.
Every real heading sits at least one level below its parent; a zero or
negative offset cannot render as Org and is reported, never repaired."
  (mapcar (lambda (row)
            (list :check "nonpositive-level-offset"
                  :severity 'error
                  :message (format "Heading %d: level_offset=%s under parent %s (expected > 0)"
                                   (alist-get 'id row)
                                   (alist-get 'level_offset row)
                                   (alist-get 'parent_id row))
                  :fix-sql nil))
          (dorg-sql dolt-dir
                    "SELECT id, level_offset, parent_id FROM headings \
WHERE parent_id IS NOT NULL AND level_offset <= 0")))

(defun dorg-doctor--dangling-parent (dolt-dir)
  "Find headings whose parent_id references a non-existent heading."
  (let ((rows (dorg-sql dolt-dir
                "SELECT h.id, h.parent_id FROM headings h \
WHERE h.parent_id IS NOT NULL \
AND h.parent_id NOT IN (SELECT id FROM headings)")))
    (mapcar (lambda (row)
              (let ((id (alist-get 'id row))
                    (pid (alist-get 'parent_id row)))
                (list :check "dangling-parent"
                      :severity 'error
                      :message (format "Heading %d references non-existent parent %d" id pid)
                      :fix-sql nil)))
            rows)))

(defun dorg-doctor--dangling-items (dolt-dir)
  "Find heading_items whose heading_id has no matching headings row."
  (let ((rows (dorg-sql dolt-dir
                "SELECT hi.id, hi.heading_id FROM heading_items hi \
WHERE hi.heading_id NOT IN (SELECT id FROM headings)")))
    (mapcar (lambda (row)
              (let ((id (alist-get 'id row))
                    (hid (alist-get 'heading_id row)))
                (list :check "dangling-item"
                      :severity 'warn
                      :message (format "Item %d references non-existent heading %d" id hid)
                      :fix-sql (format "DELETE FROM heading_items WHERE id = %d" id))))
            rows)))

(defun dorg-doctor--dangling-detail-rows (dolt-dir)
  "Find detail rows whose item_id has no matching heading_items row."
  (let ((findings nil))
    (dolist (table '("text_items" "keyword_items" "planning_items" "drawer_items"))
      (let ((rows (dorg-sql dolt-dir
                    (format "SELECT t.item_id FROM %s t \
WHERE t.item_id NOT IN (SELECT id FROM heading_items)" table))))
        (dolist (row rows)
          (let ((iid (alist-get 'item_id row)))
            (push (list :check "dangling-detail"
                        :severity 'warn
                        :message (format "%s row %d references non-existent item" table iid)
                        :fix-sql (format "DELETE FROM %s WHERE item_id = %d" table iid))
                  findings)))))
    (nreverse findings)))

(defun dorg-doctor--dangling-drawer-children (dolt-dir)
  "Find drawer_lines/property_entries with no matching drawer_items row."
  (let ((findings nil))
    (dolist (table '("drawer_lines" "property_entries"))
      (let ((rows (dorg-sql dolt-dir
                    (format "SELECT t.id, t.drawer_item_id FROM %s t \
WHERE t.drawer_item_id NOT IN (SELECT item_id FROM drawer_items)" table))))
        (dolist (row rows)
          (let ((id (alist-get 'id row))
                (did (alist-get 'drawer_item_id row)))
            (push (list :check "dangling-drawer-child"
                        :severity 'warn
                        :message (format "%s row %d references non-existent drawer %d"
                                         table id did)
                        :fix-sql (format "DELETE FROM %s WHERE id = %d" table id))
                  findings)))))
    (nreverse findings)))

(defun dorg-doctor--unreachable-headings (dolt-dir)
  "Find headings whose parent chain does not reach their declared root.
Detects parent cycles and disconnected/cross-root chains even when every
immediate parent id exists — cases the per-row `dangling-parent' check
cannot see.  Pure dangling-parent rows are reported by
`dorg-doctor--dangling-parent' and are not duplicated here.

Diagnostic only: corrupted islands and cycles have no safe automated
rewiring, so findings carry no :fix-sql."
  (let* ((rows (dorg-sql dolt-dir
                "SELECT id, parent_id, root_id FROM headings"))
         (parents (make-hash-table :test 'eql))
         (roots (make-hash-table :test 'eql))
         (findings nil))
    (dolist (row rows)
      (let ((id (alist-get 'id row)))
        (puthash id (dorg-model--json-nil (alist-get 'parent_id row)) parents)
        (puthash id (dorg-model--json-nil (alist-get 'root_id row)) roots)))
    (maphash
     (lambda (id _pid)
       (pcase (dorg-model-heading-reachability id parents (gethash id roots))
         ((and (or 'cycle 'disconnected) class)
          (push (list :check "unreachable-heading"
                      :severity 'error
                      :message (format "Heading %d in root %s is not root-reachable (%s)"
                                       id
                                       (let ((r (gethash id roots)))
                                         (if r (number-to-string r) "NULL"))
                                       class)
                      :fix-sql nil
                      :id id)
                findings))))
     parents)
    ;; Deterministic ordering for stable reports/tests.
    (sort findings (lambda (a b) (< (plist-get a :id) (plist-get b :id))))))

;;; ─── Runner ─────────────────────────────────────────────────────────

(defun dorg-doctor--mysql-available (_dolt-dir)
  "Report standalone mysql.el availability as a doctor finding.
mysql.el (the sql-server wire client) is a HARD dependency: the managed
sql-server is dorg's only supported SQL transport, so an unavailable
mysql.el is an ERROR, never a silent CLI fallback (tracked-case / k5cp4).
Returns a single error finding when mysql.el is not loadable, else no
findings.  Ignores _DOLT-DIR — this is an environment check, not a per-repo
data check."
  (unless (dorg-sql--mysql-available-p)
    (list (list :check "mysql-unavailable"
                :severity 'error
                :message (concat "standalone mysql.el is not loadable; dorg's "
                                 "managed sql-server transport cannot run. "
                                 "Restore it with the DOrg installer (install.sh) "
                                 "or `make install', which verify/restore the "
                                 "bundled lib/mysql")
                :fix-sql nil))))

(defun dorg-doctor--dolt-available (&optional _dolt-dir)
  "Report Dolt binary presence/version as a doctor finding.
Dolt is OPTIONAL — only the Dolt backend needs it; the plain-org backend
has no external dependency.  So an absent or too-old `dolt' is a WARN (not
an error) that names the version floor and the installer, never a hard
failure.  Ignores _DOLT-DIR — an environment check."
  (cond
   ((not (dorg-dolt-available-p))
    (list (list :check "dolt-unavailable"
                :severity 'warn
                :message (concat "no `dolt' binary on PATH; the Dolt backend is "
                                 "unavailable (the plain-org backend still works). "
                                 "Install Dolt " (dorg-dolt-minimum-version-string)
                                 "+ (see the DOrg installer / README) to enable it")
                :fix-sql nil)))
   ((not (dorg-dolt-supported-p))
    (list (list :check "dolt-too-old"
                :severity 'warn
                :message (format (concat "the `dolt' binary is older than the "
                                         "supported floor %s+; upgrade Dolt to use "
                                         "the Dolt backend")
                                 (dorg-dolt-minimum-version-string))
                :fix-sql nil)))))

(defun dorg-doctor--env-findings ()
  "Return the ENVIRONMENT (install-integrity) findings — no repo required.
Runs the environment checks only (mysql.el availability + Dolt presence/
version), so the installer and `dorg doctor' can verify an install without a
Dolt repository.  mysql.el absence is an ERROR (the hard dependency); Dolt
absence/age is a WARN (optional backend)."
  (append (dorg-doctor--mysql-available nil)
          (dorg-doctor--dolt-available nil)))

(defun dorg-doctor--duplicate-live-owners (dolt-dir)
  "Report duplicate or indeterminate sql-server owners for DOLT-DIR.
The census is read-only: doctor neither starts, stops, signals, rewrites nor
repairs a server.  An indeterminate observation is surfaced as a warning
rather than a false clean result; one proven holder is normal, while multiple
holders are an error naming every proven PID."
  (pcase (dorg-dolt-server--admission-lock-holders dolt-dir)
    ('unknown
     (list (list :check "dolt-owner-census-indeterminate"
                 :severity 'warn
                 :message (format "Cannot determine live sql-server owners for %s"
                                  (dorg-dolt-server--canonical-dir dolt-dir))
                 :fix-sql nil)))
    (`() nil)
    (holders
     (when (> (length holders) 1)
       (list (list :check "duplicate-dolt-server-owners"
                   :severity 'error
                   :message (format "Multiple live sql-server owners hold %s: pid%s %s"
                                    (dorg-dolt-server--canonical-dir dolt-dir)
                                    (if (= (length holders) 1) "" "s")
                                    (mapconcat #'number-to-string holders ", "))
                   :fix-sql nil))))))

(defconst dorg-doctor--checks
  '(dorg-doctor--mysql-available
    dorg-doctor--duplicate-live-owners
    dorg-doctor--orphan-roots
    dorg-doctor--invalid-file-bindings
    dorg-doctor--case-duplicate-files
    dorg-doctor--root-invariants
    dorg-doctor--nonpositive-offsets
    dorg-doctor--dangling-parent
    dorg-doctor--unreachable-headings
    dorg-doctor--dangling-items
    dorg-doctor--dangling-detail-rows
    dorg-doctor--dangling-drawer-children)
  "Ordered list of doctor check functions.
Each takes a dolt-dir and returns a list of finding plists.
`dorg-doctor--mysql-available' is an environment check that ignores its
argument; the rest are per-repo data-consistency checks.")

(defun dorg-doctor--run-checks (dolt-dir)
  "Run all consistency checks on DOLT-DIR.  Return list of findings."
  (cl-mapcan (lambda (fn) (funcall fn dolt-dir)) dorg-doctor--checks))

;;; ─── Report ─────────────────────────────────────────────────────────

(defun dorg-doctor--format-report (findings dolt-dir)
  "Format FINDINGS into an org-mode report string for DOLT-DIR."
  ;; The mysql.el status is an environment/header line, reported in BOTH the
  ;; clean and the has-findings report so `dorg doctor' always reports presence.
  (let ((mysql-line (format "mysql.el: %s."
                            (if (dorg-sql--mysql-available-p) "present" "MISSING"))))
   (if (null findings)
      (format "* dorg-doctor: %s\n\n%s\n\nNo issues found.\n" dolt-dir mysql-line)
    (let ((fixable (cl-count-if (lambda (f) (plist-get f :fix-sql)) findings)))
      (with-temp-buffer
        (insert (format "* dorg-doctor: %s\n\n" dolt-dir))
        (insert (format "%s\n\n" mysql-line))
        (insert (format "%d issue%s found (%d fixable).\n\n"
                        (length findings)
                        (if (= (length findings) 1) "" "s")
                        fixable))
        (dolist (f findings)
          (let ((sev (plist-get f :severity))
                (msg (plist-get f :message))
                (fix (plist-get f :fix-sql)))
            (insert (format "- [%s] %s%s\n"
                            (if (eq sev 'error) "ERROR" "WARN")
                            msg
                            (if fix " (fixable)" "")))))
        (buffer-string))))))

;;; ─── Fix Application ────────────────────────────────────────────────

(defun dorg-doctor--apply-fixes (dolt-dir findings)
  "Apply fixable FINDINGS to DOLT-DIR in a single transaction."
  (let ((fix-stmts (cl-remove-if #'null
                     (mapcar (lambda (f) (plist-get f :fix-sql)) findings))))
    (when fix-stmts
      (dorg-sql-batch dolt-dir
                      (append (list "BEGIN")
                              fix-stmts
                              (list "COMMIT")))
      (dorg-commit dolt-dir
                   (format "dorg-doctor: applied %d fix%s"
                           (length fix-stmts)
                           (if (= (length fix-stmts) 1) "" "es")))
      (length fix-stmts))))

;;; ─── Run / fix / re-check core ──────────────────────────────────────

(defun dorg-doctor--run-fix-recheck (dolt-dir should-apply)
  "Run checks on DOLT-DIR, optionally apply fixes, and re-check.

Runs `dorg-doctor--run-checks'.  When any finding is fixable AND
\(funcall SHOULD-APPLY initial-findings) returns non-nil, applies the
fixes via `dorg-doctor--apply-fixes' and re-runs the checks so the
result reflects POST-fix state.

SHOULD-APPLY is the caller-specific decision seam.  It is invoked ONLY
when a fixable finding exists (there is nothing to confirm otherwise),
and receives the INITIAL pre-fix findings so a caller may render them
before deciding.

Returns a plist (:findings FINDINGS :applied N): FINDINGS is the
post-fix findings when fixes were applied, else the initial findings; N
is the number of fixes applied (0 when none)."
  (let* ((findings (dorg-doctor--run-checks dolt-dir))
         (fixable (cl-some (lambda (f) (plist-get f :fix-sql)) findings))
         (applied 0))
    (when (and fixable (funcall should-apply findings))
      (setq applied (or (dorg-doctor--apply-fixes dolt-dir findings) 0))
      (setq findings (dorg-doctor--run-checks dolt-dir)))
    (list :findings findings :applied applied)))

;;; ─── Public API ─────────────────────────────────────────────────────

(defun dorg-doctor (dolt-dir &optional apply)
  "Diagnose consistency problems in the dorg database at DOLT-DIR.
Runs all checks, displays a report in the *dorg-doctor* buffer.
When called interactively, prompts for DOLT-DIR and offers to
apply fixes if any are found.  Non-interactively, applies fixes
only if APPLY is non-nil.

Returns the POST-fix findings when fixes were applied (the report and
return value reflect the re-checked state), else the current findings."
  (interactive
   (list (read-directory-name "Dolt directory: "
                              (when-let ((reg (ignore-errors
                                               (dorg--classify-registration))))
                                (dorg-backend-dolt-dir
                                 (dorg-registration-backend reg))))))
  ;; Capture interactivity at command ENTRY: `called-interactively-p' is
  ;; nil inside the `should-apply' closure the core invokes, so the prompt
  ;; decision must be threaded through this lexical flag.
  (let* ((interactive-p (called-interactively-p 'any))
         (dolt-dir (file-name-as-directory (expand-file-name dolt-dir)))
         (buf (get-buffer-create "*dorg-doctor*"))
         (render
          (lambda (findings &optional applied)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (dorg-doctor--format-report findings dolt-dir))
                (when (and applied (> applied 0))
                  (goto-char (point-max))
                  (insert (format "\nApplied %d fix%s.\n"
                                  applied (if (= applied 1) "" "es"))))
                (org-mode)
                (goto-char (point-min))))
            (display-buffer buf)))
         ;; Decision seam: render the pre-fix report (so the buffer stays
         ;; visible during the prompt) then decide — interactive prompt vs
         ;; the APPLY flag.
         (should-apply
          (lambda (findings)
            (funcall render findings)
            (if interactive-p
                (let ((fixable (cl-remove-if-not
                                (lambda (f) (plist-get f :fix-sql)) findings)))
                  (yes-or-no-p (format "Apply %d fix%s? "
                                       (length fixable)
                                       (if (= (length fixable) 1) "" "es"))))
              apply)))
         (result (dorg-doctor--run-fix-recheck dolt-dir should-apply))
         (findings (plist-get result :findings))
         (applied (plist-get result :applied)))
    ;; Render the FINAL (post-fix) findings + any "Applied N" note.
    (funcall render findings applied)
    (when (> applied 0)
      (message "dorg-doctor: applied %d fix%s" applied
               (if (= applied 1) "" "es")))
    findings))

(provide 'dorg-doctor)
;;; dorg-doctor.el ends here
