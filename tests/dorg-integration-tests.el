;;; dorg-integration-tests.el --- End-to-end integration test for dorg-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; End-to-end integration test covering the full dorg-mode lifecycle:
;; explicit setup/import -> exact projection -> typed mutation + exact refresh
;; (id stability) -> materialize into a fresh buffer.
;;
;; This test runs in a fresh per-run OWNED workdir under the canonical
;; TMPDIR (allocated via
;; `dorg-test--make-owned-temp-dir').  On exit the workdir is reaped +
;; UN-owned but NOT deleted, so the artifacts (org file, Dolt repo) remain
;; on disk for inspection while the run holds no stale ownership.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg)
(require 'dorg-dolt)
(require 'dorg-registry)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;; The workdir is a per-run TMPDIR-canonical OWNED temp dir (allocated inside
;; the test via `dorg-test--make-owned-temp-dir'), NOT a hard-coded shared
;; path.  A fixed `/tmp/dorg-integration-work' both collided across concurrent
;; lanes and — under a canonical `/private/tmp' TMPDIR where it names no real
;; directory yet — could not have its filesystem identity captured by the
;; fail-loud `dorg-test--register-owned-root', reddening the isolated suite.

(defconst dorg-test-integration-org-content
  (concat "#+TITLE: Integration Test\n"
          "#+AUTHOR: dorg\n"
          "\n"
          "Preamble text before first heading.\n"
          "\n"
          "* TODO [#A] First task                                     :work:urgent:\n"
          "SCHEDULED: <2026-04-10 Fri>\n"
          ":PROPERTIES:\n"
          ":EFFORT:   2h\n"
          ":END:\n"
          "Body text for first task.\n"
          "\n"
          "With multiple paragraphs.\n"
          "\n"
          "** DONE Subtask                                            :done:\n"
          "CLOSED: [2026-04-09 Thu 14:00]\n"
          "A completed subtask.\n"
          "\n"
          "* Second heading\n"
          "Just a body, no metadata.\n")
  "Non-trivial Org content exercising headings, tags, properties,
planning, nesting, preamble and keywords.")

(defun dorg-test-integration--count-matches (re string)
  "Count non-overlapping matches of RE in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match re string start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(ert-deftest dorg-test-integration-end-to-end ()
  "Exercise the projection-only lifecycle in an owned workdir.
Proves explicit setup/import, exact DORG_ID publication, typed mutation,
exact refresh, id stability, and fresh-buffer materialization.  Leaves the
reaped and un-owned workdir on disk for inspection."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((workdir (dorg-test--make-owned-temp-dir
                     "dorg-integration-" "integration-end-to-end"))
           (dolt-dir (expand-file-name "db/" workdir))
           (org-file (expand-file-name "tasks.org" workdir))
           (fresh-file (expand-file-name "fresh.org" workdir)))
      (make-directory dolt-dir t)
      (unwind-protect
          (progn
            (with-temp-file org-file
              (insert dorg-test-integration-org-content))
            (should (file-exists-p org-file))

            (let ((dorg--registry nil)
                  (buf (find-file-noselect org-file)))
              (unwind-protect
                  (progn
                    ;; Setup is the only first-ingest path: initialize/import,
                    ;; bind, materialize and save the exact receipt, enable mode.
                    (with-current-buffer buf
                      (dorg-setup dolt-dir)
                      (should dorg-mode)
                      (should buffer-read-only))
                    (let* ((reg (dorg--classify-registration buf))
                           (root-id
                            (dorg-backend-root-id
                             (dorg-registration-backend reg))))
                      (should (dorg-registration-p reg))
                      (should (file-exists-p (expand-file-name ".dolt" dolt-dir)))
                      (should (integerp root-id))

                      ;; DB has root + 3 content headings.
                      (let ((headings
                             (dorg-sql
                              dolt-dir
                              (concat
                               "SELECT level_offset, title FROM headings WHERE root_id = %d "
                               "ORDER BY CASE WHEN level_offset = 0 THEN 0 ELSE 1 END, sort_key")
                              root-id)))
                        (should (= (length headings) 4))
                        (should (equal
                                 (mapcar (lambda (h) (alist-get 'title h))
                                         headings)
                                 '(nil "First task" "Subtask" "Second heading"))))

                      ;; Exact setup publication saved stable IDs without a
                      ;; save-hook write-back path.
                      (with-current-buffer buf
                        (should (= 3 (dorg-test-integration--count-matches
                                      ":DORG_ID:" (buffer-string)))))
                      (let ((disk (with-temp-buffer
                                    (insert-file-contents org-file)
                                    (buffer-string))))
                        (should (= 3 (dorg-test-integration--count-matches
                                      ":DORG_ID:" disk))))

                      ;; Typed title mutation + exact-token refresh preserves
                      ;; IDs and updates both authority and projection.
                      (let* ((rows-before
                              (dorg-sql
                               dolt-dir
                               "SELECT id, title FROM headings WHERE root_id = %d AND level_offset > 0 ORDER BY sort_key"
                               root-id))
                             (ids-before
                              (mapcar (lambda (r) (alist-get 'id r)) rows-before))
                             (first-id (alist-get 'id (car rows-before)))
                             (response
                              (dorg-api-execute
                               (list 'on (list 'id first-id)
                                     (list 'set (list 'heading 'title)
                                           "First task RENAMED"))
                               :mode 'apply :source buf)))
                        (dorg-refresh-materialized-view
                         buf (plist-get response :new-token))
                        (let* ((rows
                                (dorg-sql
                                 dolt-dir
                                 "SELECT id, title FROM headings WHERE root_id = %d AND level_offset > 0 ORDER BY sort_key"
                                 root-id))
                               (ids-after
                                (mapcar (lambda (r) (alist-get 'id r)) rows))
                               (titles
                                (mapcar (lambda (r) (alist-get 'title r)) rows)))
                          (should (equal ids-before ids-after))
                          (should (member "First task RENAMED" titles)))
                        (with-current-buffer buf
                          (should (string-match-p "First task RENAMED"
                                                  (buffer-string)))))

                      ;; A new buffer sees the same committed authority.
                      (with-temp-file fresh-file (insert ""))
                      (let ((fresh-buf (find-file-noselect fresh-file)))
                        (unwind-protect
                            (with-current-buffer fresh-buf
                              (push (cons (file-truename fresh-file)
                                          (make-dorg-registration
                                           :backend (dorg-backend-dolt
                                                     dolt-dir root-id)))
                                    dorg--registry)
                              (dorg-materialize)
                              (let ((content (buffer-string)))
                                (should (string-match-p "First task RENAMED" content))
                                (should (string-match-p ":DORG_ID:" content))
                                (should (string-match-p "Preamble text" content))
                                (should (string-match-p "Subtask" content))
                                (should (string-match-p ":work:urgent:" content))))
                          (when (buffer-live-p fresh-buf)
                            (with-current-buffer fresh-buf
                              (set-buffer-modified-p nil))
                            (kill-buffer fresh-buf))))))
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (set-buffer-modified-p nil))
                  (kill-buffer buf))))

            ;; Artifacts deliberately remain for inspection.
            (should (file-exists-p org-file))
            (should (file-exists-p (expand-file-name ".dolt" dolt-dir))))
        (dorg-test--reap-owned-root workdir t)))))

(provide 'dorg-integration-tests)
;;; dorg-integration-tests.el ends here
