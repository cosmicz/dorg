;;; dorg-perf-tests.el --- Performance tests for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Performance benchmarks for dorg parse, model load, and rendering.
;; DB tests require a sql-server (skipped in CLI-only mode).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-parse)
(require 'dorg-model)
(require 'dorg-render)
(require 'dorg-import)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)  ;; defined in dorg-dolt-tests.el

;;; Generator

(defun dorg-perf--generate-org (n-top children-per depth body-lines)
  "Generate org content with N-TOP top-level headings.
Each heading gets DEPTH additional levels of nesting with CHILDREN-PER
children at each level, and BODY-LINES lines of body text per heading.
Total headings = N-TOP * (1 + c + c^2 + ... + c^depth).
Returns the generated string."
  (let ((parts nil)
        (heading-count 0))
    (cl-labels
        ((gen-subtree (level remaining-depth)
           (dotimes (_i children-per)
             (let ((stars (make-string level ?*))
                   (idx (cl-incf heading-count)))
               (push (format "%s TODO Heading %d\n" stars idx) parts)
               (dotimes (bl body-lines)
                 (push (format "Line %d of heading %d. Sed do eiusmod tempor.\n"
                               bl idx)
                       parts))
               (push "\n" parts)
               (when (> remaining-depth 0)
                 (gen-subtree (1+ level) (1- remaining-depth)))))))
      (dotimes (_i n-top)
        (let ((idx (cl-incf heading-count)))
          (push (format "* TODO Heading %d  :project:work:\n" idx) parts)
          (dotimes (bl body-lines)
            (push (format "Line %d of heading %d. Sed do eiusmod tempor.\n"
                         bl idx)
                  parts))
          (push "\n" parts)
          (gen-subtree 2 (1- depth)))))
    (apply #'concat (nreverse parts))))

(defun dorg-perf--time-call (fn)
  "Call FN, return (TIME-SECONDS . RESULT)."
  (let ((start (float-time)))
    (let ((result (funcall fn)))
      (cons (- (float-time) start) result))))

;;; Parse Benchmark

(ert-deftest dorg-perf-parse-2000-headings ()
  "Benchmark: parse a ~1MB org buffer with 2000+ headings, 5 nesting levels.
Measures `dorg-parse-buffer' and `org-element-parse-buffer' separately."
  ;; 70 top * (1 + 2 + 4 + 8 + 16) c=2,depth=4 = 70*31 = 2170 headings
  ;; 10 body lines per heading → ~1.0 MB total
  (let* ((content (dorg-perf--generate-org 70 2 4 10))
         (content-size (length content))
         (heading-count 0)
         org-parse-time
         dorg-parse-time)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (cl-incf heading-count)))
    (ert-info ((format "Content: %d bytes, %d headings"
                       content-size heading-count))
      (should (>= content-size 500000))
      (should (>= heading-count 2000)))
    (with-temp-buffer
      (insert content)
      (org-mode)
      (let ((timing (dorg-perf--time-call
                     (lambda () (org-element-parse-buffer)))))
        (setq org-parse-time (car timing))
        (ert-info ((format "org-element-parse-buffer: %.3fs" org-parse-time))
          (should t))))
    (with-temp-buffer
      (insert content)
      (org-mode)
      (let ((timing (dorg-perf--time-call
                     (lambda () (dorg-parse-buffer (current-buffer) 1)))))
        (setq dorg-parse-time (car timing))
        (ert-info ((format "dorg-parse-buffer: %.3fs (%d headings parsed)"
                           dorg-parse-time
                           (length (dorg-parse-result-headings (cdr timing)))))
          (should t))))
    (let ((overhead (- dorg-parse-time org-parse-time)))
      (ert-info ((format "dorg overhead over org-element: %.3fs (%.0f%%)"
                         overhead
                         (* 100 (/ overhead (max org-parse-time 0.001)))))
        (should t)))
    (ert-info ("dorg-parse-buffer should complete in under 5s")
      (should (< dorg-parse-time 5.0)))))

;;; Model-load Benchmark (AS OF query)

(ert-deftest dorg-perf-model-load-tree-2000-headings ()
  "Benchmark: load 2000+ headings from DB model via AS OF vs cache read."
  (unless dorg-test--dolt-available (ert-skip "dolt not installed"))
  (unless dorg-test--server-port (ert-skip "requires sql-server"))
  (let* ((content (dorg-perf--generate-org 70 2 4 10)))
    (dorg-test--with-temp-db-and-dir temp-base dolt-dir
      (let* ((org-file (expand-file-name "perf.org" temp-base))
             (dorg--registry nil))
        (with-temp-buffer
          (insert content)
          (org-mode)
          (set-visited-file-name org-file t)
          (set-buffer-modified-p nil)
          (let ((receipt (dorg-import-document
                          (current-buffer)
                          :dolt-dir dolt-dir
                          :target-file org-file
                          :mode 'bootstrap)))
            (let* ((root-id (plist-get receipt :root-id))
                   (commit (dorg-head-commit dolt-dir))
                   (as-of-timing
                    (dorg-perf--time-call
                     (lambda ()
                       (dorg-model-load-tree dolt-dir root-id commit))))
                   (as-of-time (car as-of-timing))
                   (db-state (cdr as-of-timing))
                   (copy-timing
                    (dorg-perf--time-call
                     (lambda () (copy-hash-table db-state))))
                   (copy-time (car copy-timing)))
              (ert-info ((format "AS OF query: %.3fs (%d headings)"
                                 as-of-time (hash-table-count db-state)))
                (should (>= (hash-table-count db-state) 2000)))
              (ert-info ((format "hash-table copy: %.6fs" copy-time))
                (should t))
              (ert-info ((format "speedup: %.0fx"
                                 (/ as-of-time (max copy-time 0.000001))))
                (should (> (/ as-of-time (max copy-time 0.000001)) 10))))))))))

;;; Bounded-window vs full-tree render benchmark (tracked-case / fepa2)

(defun dorg-perf--count-headlines (text)
  "Count Org headlines (lines starting with one or more `*') in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((c 0)) (while (re-search-forward "^\\*+ " nil t) (cl-incf c)) c)))

(defun dorg-perf--seed-flat-children (dolt-dir n)
  "Seed DOLT-DIR with a root of N flat children (each a body); commit.
Gives a materially large full tree (thousands of headings) to window
against with a single focus."
  (let ((org (with-temp-buffer
               (dotimes (i n)
                 (insert (format "* Child-%d\nbody line for child %d\n"
                                 (1+ i) (1+ i))))
               (buffer-string))))
    (dorg-sql-batch dolt-dir (dorg-test--parse-to-sql org))
    (dorg-commit dolt-dir "window fixture")))

(ert-deftest dorg-perf-render-window-vs-full ()
  "Benchmark: bounded-window render vs full-tree render on a large store.
Measures the fepa2 `dorg-render--window' primitive (one focus) against
`dorg-render' (whole tree) across growing N.  The window is
O(focus-subtree + breadcrumb) and its materialized output is ~constant,
while the full render is O(tree) and grows linearly.  Requires the warm
sql-server transport — the live dashboard/capture path where each query
is a cheap round-trip rather than a fresh dolt subprocess."
  (unless dorg-test--dolt-available (ert-skip "dolt not installed"))
  (unless dorg-test--server-port (ert-skip "requires sql-server"))
  (dolist (n '(1000 6000))
    (dorg-test--with-temp-db dolt-dir
      (dorg-perf--seed-flat-children dolt-dir n)
      (let* ((focus (alist-get 'id (car (dorg-sql
                                         dolt-dir
                                         "SELECT id FROM headings WHERE title = %s"
                                         (dorg-sql-quote (format "Child-%d" n))))))
             ;; Warm the process once before timing.
             (_ (dorg-render dolt-dir))
             (full (dorg-perf--time-call (lambda () (dorg-render dolt-dir))))
             (win (dorg-perf--time-call
                   (lambda () (dorg-render--window dolt-dir (list focus)))))
             (full-txt (cdr full))
             (win-txt (cdr win))
             (full-heads (dorg-perf--count-headlines full-txt))
             (win-heads (dorg-perf--count-headlines win-txt)))
        ;; `message' (not `ert-info') so the numbers surface on PASS.
        (message (concat "[w08rt] N=%d  full=%.3fs (%dh, %.1fKB)  "
                         "window=%.3fs (%dh, %.1fKB)  speedup=%.1fx")
                 n (car full) full-heads (/ (string-bytes full-txt) 1024.0)
                 (car win) win-heads (/ (string-bytes win-txt) 1024.0)
                 (/ (car full) (max (car win) 0.000001)))
        ;; Window is a single-focus projection; full is the whole tree.
        (should (= win-heads 1))
        (should (= full-heads n))
        ;; Materialized window output is a small fraction of the full tree.
        (should (< (string-bytes win-txt) (/ (string-bytes full-txt) 10)))
        ;; Wall-clock: bounded-window materialization beats full-tree render.
        (should (< (car win) (car full)))))))

(provide 'dorg-perf-tests)
;;; dorg-perf-tests.el ends here
