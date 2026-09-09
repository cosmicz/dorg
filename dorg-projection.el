;;; dorg-projection.el --- Projection-buffer utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Neutral state and utilities for operating on a rendered Org projection
;; buffer: the owned-write capability, established-view metadata, heading
;; region calculation, bounded region replacement, and DORG_ID marker
;; scanning/refresh.  Extracted from whole-file reconciliation by
;; tracked-regression; its surviving owner is the one-way read-only projection
;; path.

;;; Code:

(require 'org)
(require 'dorg-id)

;;; Owned projection state

(defvar-local dorg--projection-change-depth 0
  "Nesting depth of DOrg-owned projection mutations.")

(defvar-local dorg--projection-view nil
  "Private established-view context for the current projection buffer.")

(put 'dorg--projection-view 'permanent-local t)

(defun dorg-projection-call-with-programmatic-changes (function)
  "Call FUNCTION with permission to mutate the current projection buffer."
  (let ((inhibit-read-only t))
    (unwind-protect
        (progn
          (setq dorg--projection-change-depth
                (1+ dorg--projection-change-depth))
          (funcall function))
      (setq dorg--projection-change-depth
            (1- dorg--projection-change-depth)))))

(defmacro dorg-projection-with-programmatic-changes (&rest body)
  "Execute BODY with permission to mutate the current projection buffer."
  (declare (indent 0) (debug t))
  `(dorg-projection-call-with-programmatic-changes
    (lambda () ,@body)))

(defun dorg-projection-programmatic-change-p ()
  "Return non-nil inside a DOrg-owned projection mutation."
  (> dorg--projection-change-depth 0))

(defun dorg-projection-install-view (buffer source root-id revision scope window)
  "Install BUFFER's established projection-view context.
SOURCE, ROOT-ID, REVISION, SCOPE, and WINDOW are lifecycle-owned values.
SCOPE must be `whole' or `partial'.  Mutable inputs are copied so callers
cannot change the installed context through a retained reference."
  (unless (memq scope '(whole partial))
    (error "dorg: invalid materialized-view scope %S" scope))
  (with-current-buffer buffer
    (setq dorg--projection-view
          (list :source source
                :root-id root-id
                :revision revision
                :scope scope
                :window (copy-tree window)))))

(defun dorg-projection-clear-view (&optional buffer)
  "Clear BUFFER's established projection-view context."
  (with-current-buffer (or buffer (current-buffer))
    (setq dorg--projection-view nil)))

(defun dorg-projection-view-context (&optional buffer)
  "Return an isolated copy of BUFFER's established view context, or nil."
  (copy-tree
   (buffer-local-value 'dorg--projection-view
                       (or buffer (current-buffer)))))

(defun dorg-projection-state-snapshot (&optional buffer)
  "Capture BUFFER's established projection state for atomic rollback."
  (dorg-projection-view-context buffer))

(defun dorg-projection-state-restore (snapshot &optional buffer)
  "Restore BUFFER's established projection state from SNAPSHOT."
  (with-current-buffer (or buffer (current-buffer))
    (setq dorg--projection-view (copy-tree snapshot))))

(defun dorg-materialized-view-scope (&optional buffer)
  "Return BUFFER's materialized-view scope: nil, `whole', or `partial'.
Nil means no DOrg materialization is established in BUFFER.  This is the
only public classification of a materialized view; internal context remains
private to the projection lifecycle."
  (plist-get (buffer-local-value 'dorg--projection-view
                                 (or buffer (current-buffer)))
             :scope))

;;; Heading regions

(defun dorg-projection-section-region ()
  "Return (BEG . END) for the section of heading at point.
Section = heading line through all content up to the next heading
\(any level) or buffer end.  Point must be on a heading line."
  (save-excursion
    (let ((beg (line-beginning-position)))
      (forward-line 1)
      (cons beg (if (re-search-forward org-heading-regexp nil t)
                    (line-beginning-position)
                  (point-max))))))

(defun dorg-projection-subtree-region ()
  "Return (BEG . END) for the full subtree at point.
Includes heading, all descendants, and trailing blank lines up to
the next heading at the same or higher level, or buffer end.
Point must be on a heading line."
  (save-excursion
    (let ((beg (line-beginning-position))
          (level (org-outline-level)))
      (forward-line 1)
      (cons beg (if (re-search-forward
                     (format "^\\*\\{1,%d\\} " level) nil t)
                    (line-beginning-position)
                  (point-max))))))

(defun dorg-projection-replace-region (beg end text)
  "Replace region BEG..END in current buffer with TEXT.
Uses `replace-buffer-contents' under narrowing to minimize edits.
BEG and END may be markers; they are left unchanged."
  (save-restriction
    (narrow-to-region beg end)
    (let ((source (generate-new-buffer " *dorg-scoped*")))
      (unwind-protect
          (progn
            (with-current-buffer source (insert text))
            (replace-buffer-contents source))
        (kill-buffer source)))))

;;; DORG_ID marker scanning

(defun dorg-projection-refresh-region-markers (beg end heading-markers)
  "Update HEADING-MARKERS for headings in buffer region BEG..END.
For each heading with a `:DORG_ID:' property, puthashes a fresh
marker at the heading's line-beginning position.  BEG and END may
be markers or integer positions.

Reads DORG_ID via `dorg-id-at-point-plain' — a single plain-text pass
over each heading's identity drawer under the one malformed-value policy
\(dorg-id.el), avoiding a cold `org-element-at-point' / `org-entry-get'
parse per heading (tracked-case seam)."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward org-heading-regexp end t)
      (let ((heading-begin (line-beginning-position))
            (id (dorg-id-at-point-plain)))
        (when id
          (puthash id (copy-marker heading-begin) heading-markers))))))

(defun dorg-projection-scan-markers (&optional buffer)
  "Build id->marker hash by scanning BUFFER for :DORG_ID: properties.
Cheaper than a full `org-element-parse-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (let ((markers (make-hash-table :test 'eql)))
      (save-excursion
        (save-restriction
          (widen)
          (dorg-projection-refresh-region-markers
           (point-min) (point-max) markers)))
      markers)))

;;; Placement

(defun dorg-projection-insert-point (parent-id heading-markers)
  "Return buffer position where children of PARENT-ID would begin.
For non-root: end of parent's section region.
For root (no marker): first heading line or point-max."
  (let ((m (and parent-id (gethash parent-id heading-markers))))
    (if m
        (save-excursion
          (goto-char (marker-position m))
          (cdr (dorg-projection-section-region)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward org-heading-regexp nil t)
            (line-beginning-position)
          (point-max))))))

(provide 'dorg-projection)
;;; dorg-projection.el ends here
