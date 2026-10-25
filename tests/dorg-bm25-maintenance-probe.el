;;; dorg-bm25-maintenance-probe.el --- Phase probe for BM25 edit maintenance -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An opt-in measurement harness, not a test.  It seeds a deterministic
;; corpus through the public import door, installs and rebuilds the BM25
;; read model, then applies ordinary edits through the public mutate door
;; and splits each into phases: bounded or whole-root load, simulation,
;; index-context construction, projection, the commit-time proof parts, and
;; the commit batch.  It also counts fetched headings and tokenizer calls,
;; the scaling quantities that matter for bounded maintenance, and times the
;; keyed revision reads a plan-time accumulator fold would issue.
;;
;; Only public facades and long-standing internal phase functions are named,
;; so the same file measures a baseline checkout beside a candidate.  Every
;; advised function is checked with `fboundp' first.  It asserts nothing
;; about time; callers record the printed plists.
;;
;; Usage, from a checkout root, one fresh process per step:
;;
;;   emacs --batch -Q -L . -L lib/mysql -l tests/dorg-bm25-maintenance-probe.el \
;;     --eval '(dorg-bm25-maintenance-probe-seed "STORE" 3000)'
;;   ... --eval '(dorg-bm25-maintenance-probe-serve "STORE")'
;;   ... --eval "(dorg-bm25-maintenance-probe-edits \"STORE\" 'title 5 START)"
;;   ... --eval '(dorg-bm25-maintenance-probe-prior-reads "STORE")'
;;   ... --eval '(dorg-bm25-maintenance-probe-stop "STORE")'
;;
;; EDIT is `title', `body' or `delete'.  Each `edits' run is a fresh process
;; against an already-running server; its first edit is the fresh sample.

;;; Code:

(require 'cl-lib)

(defvar dorg--extensions)
(declare-function dorg-api-execute "dorg-api" (form-or-intent &rest options))
(declare-function dorg-bm25-register-extension "dorg-bm25" ())
(declare-function dorg-commit "dorg-dolt" (dolt-dir message))
(declare-function dorg-dolt-start "dorg-dolt-server" (dolt-dir))
(declare-function dorg-dolt-stop "dorg-dolt-server" (dolt-dir))
(declare-function dorg-ext-rebuild-index "dorg-ext-rebuild" (extension root-id &rest options))
(declare-function dorg-head-commit "dorg-dolt" (dolt-dir))
(declare-function dorg-import-document "dorg-import" (source &rest options))
(declare-function dorg-migrate-schema "dorg-schema" (dolt-dir))
(declare-function dorg-sql "dorg-dolt" (dolt-dir query &rest args))
(declare-function dorg-sql--at-revision "dorg-dolt" (dolt-dir revision query &rest args))

(defconst dorg-bm25-maintenance-probe--words
  '("orchid" "lattice" "harbor" "meridian" "cobalt" "quartz" "saffron" "tundra"
    "vellum" "zephyr" "ember" "fjord" "glacier" "hollow" "inlet" "juniper"
    "kestrel" "lagoon" "marrow" "nectar" "obsidian" "pylon" "quiver" "russet")
  "Filler vocabulary for generated documents.")

(defun dorg-bm25-maintenance-probe--text (i n)
  "Return N deterministic words for document I, with sparse rare tokens."
  (let* ((words dorg-bm25-maintenance-probe--words)
         (count (length words)))
    (mapconcat (lambda (j) (nth (mod (+ (* i 31) (* j 7) (/ i 13)) count) words))
               (number-sequence 1 n) " ")))

(defun dorg-bm25-maintenance-probe-content (documents)
  "Return Org text for DOCUMENTS headings, each with a title and a body."
  (mapconcat
   (lambda (i)
     (format "* Doc-%05d %s\n:PROPERTIES:\n:DORG_ID: %d\n:END:\n%s rare%05d\n"
             i (dorg-bm25-maintenance-probe--text i 3) (+ 100000 i)
             (dorg-bm25-maintenance-probe--text (* 3 i) 12) i))
   (number-sequence 1 documents) ""))

(defun dorg-bm25-maintenance-probe--paths (store)
  "Return (DOLT-DIR SOURCE-FILE TARGET-FILE META-FILE) under STORE."
  (let ((store (file-name-as-directory (expand-file-name store))))
    (list (expand-file-name "dolt" store)
          (expand-file-name "source.org" store)
          (expand-file-name "corpus.org" store)
          (expand-file-name "probe-meta.eld" store))))

(defun dorg-bm25-maintenance-probe--meta (store)
  "Return the seed metadata plist recorded for STORE."
  (with-temp-buffer
    (insert-file-contents (nth 3 (dorg-bm25-maintenance-probe--paths store)))
    (read (current-buffer))))

(defun dorg-bm25-maintenance-probe-seed (store documents)
  "Import DOCUMENTS headings into a new STORE, install BM25 and rebuild it."
  (require 'dorg)
  (require 'dorg-import)
  (require 'dorg-ext-rebuild)
  (require 'dorg-bm25)
  (pcase-let ((`(,dolt-dir ,source ,target ,meta)
               (dorg-bm25-maintenance-probe--paths store)))
    (when (file-exists-p store)
      (error "Probe store already exists: %s" store))
    (make-directory dolt-dir t)
    (with-temp-file source
      (insert (dorg-bm25-maintenance-probe-content documents)))
    (let* ((started (float-time))
           (_ (dorg-import-document source :dolt-dir dolt-dir
                                    :target-file target :mode 'bootstrap))
           (imported (- (float-time) started))
           (root (alist-get 'root_id
                            (car (dorg-sql dolt-dir "SELECT root_id FROM dorg_files"))))
           (dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "probe: install lexical extension")
      (let* ((build-started (float-time))
             (receipt (dorg-ext-rebuild-index
                       'bm25 root :dolt-dir dolt-dir
                       :revision (dorg-head-commit dolt-dir)
                       :identity 'dorg :mode 'repair))
             (built (- (float-time) build-started)))
        (with-temp-file meta
          (prin1 (list :documents documents :root-id root
                       :head (dorg-head-commit dolt-dir)
                       :import-seconds imported :rebuild-seconds built
                       :rebuild-ok (plist-get receipt :ok))
                 (current-buffer)))
        (princ (format "seeded store=%s root=%S import=%.3fs rebuild=%.3fs\n"
                       store root imported built))))))

(defun dorg-bm25-maintenance-probe-serve (store)
  "Start STORE's detached sql-server."
  (require 'dorg)
  (princ (format "served store=%s port=%S\n" store
                 (dorg-dolt-start (car (dorg-bm25-maintenance-probe--paths store))))))

(defun dorg-bm25-maintenance-probe-stop (store)
  "Stop STORE's detached sql-server."
  (require 'dorg)
  (princ (format "stopped store=%s result=%S\n" store
                 (dorg-dolt-stop (car (dorg-bm25-maintenance-probe--paths store))))))

;; Every sample is (KIND SECONDS COUNT).  COUNT is the number of headings a
;; load returned, or nil.  Phase functions are timed only at their outermost
;; call, so a nested call is part of its caller's phase.

(defvar dorg-bm25-maintenance-probe--trace nil
  "Samples for the edit in flight, newest first.")

(defvar dorg-bm25-maintenance-probe--depth nil
  "Alist of phase kind to current nesting depth.")

(defvar dorg-bm25-maintenance-probe--tokenize-calls 0
  "Tokenizer calls during the edit in flight.")

(defconst dorg-bm25-maintenance-probe--phases
  '((load . dorg-mutate--dolt-load)
    (simulate . dorg-mutate--dolt-simulate)
    (context . dorg-mutate--dolt-index-context)
    (project . dorg-ext-collect-index-projection)
    (proof-core . dorg-mutate--postimage-assert-core)
    (proof-partitions . dorg-mutate--postimage-assert-partitions)
    (proof-rows . dorg-mutate--postimage-assert-rows)
    (commit-batch . dorg-sql--lease-batch))
  "Mutation phase functions present in both baseline and candidate checkouts.")

(defconst dorg-bm25-maintenance-probe--loaders
  '(dorg-model-load-tree dorg-model-load-headings)
  "Heading loaders whose returned heading count is the fetched-heading count.")

(defun dorg-bm25-maintenance-probe--loaded-count (value)
  "Return the number of headings in loader result VALUE, or nil."
  (cond ((hash-table-p value) (hash-table-count value))
        ((proper-list-p value) (length value))))

(defun dorg-bm25-maintenance-probe--timed (kind count-p)
  "Return :around advice recording KIND; COUNT-P counts loaded headings."
  (lambda (orig &rest args)
    (let ((depth (alist-get kind dorg-bm25-maintenance-probe--depth 0)))
      (if (> depth 0)
          (apply orig args)
        (let ((started (float-time)) value)
          (setf (alist-get kind dorg-bm25-maintenance-probe--depth) 1)
          (unwind-protect
              (setq value (apply orig args))
            (setf (alist-get kind dorg-bm25-maintenance-probe--depth) 0)
            (push (list kind (- (float-time) started)
                        (and count-p (dorg-bm25-maintenance-probe--loaded-count value)))
                  dorg-bm25-maintenance-probe--trace))
          value)))))

(defun dorg-bm25-maintenance-probe--instrument ()
  "Advise the phase functions, loaders and tokenizer this checkout defines."
  (dolist (entry dorg-bm25-maintenance-probe--phases)
    (when (fboundp (cdr entry))
      (advice-add (cdr entry) :around
                  (dorg-bm25-maintenance-probe--timed (car entry) nil))))
  (dolist (loader dorg-bm25-maintenance-probe--loaders)
    (when (fboundp loader)
      (advice-add loader :around (dorg-bm25-maintenance-probe--timed loader t))))
  (advice-add 'dorg-bm25-tokenize :before
              (lambda (&rest _)
                (cl-incf dorg-bm25-maintenance-probe--tokenize-calls))))

(defun dorg-bm25-maintenance-probe--sum (kind)
  "Return summed seconds of KIND samples."
  (cl-loop for sample in dorg-bm25-maintenance-probe--trace
           when (eq (car sample) kind) sum (nth 1 sample)))

(defun dorg-bm25-maintenance-probe--fetched ()
  "Return the total heading count returned by every loader call."
  (cl-loop for sample in dorg-bm25-maintenance-probe--trace
           when (memq (car sample) dorg-bm25-maintenance-probe--loaders)
           sum (or (nth 2 sample) 0)))

(defun dorg-bm25-maintenance-probe--form (edit id i)
  "Return the public mutate form for EDIT on heading ID, sample I."
  (pcase edit
    ('title `(on (id ,id) (set (heading title) ,(format "Doc revised %d orchid%d" i i))))
    ('body `(on (id ,id) (insert (text) (after-last) ,(format "appended zephyr%d" i))))
    ('delete `(on (id ,id) (delete (heading))))
    (_ (error "Unknown probe edit: %S" edit))))

(defun dorg-bm25-maintenance-probe-edits (store edit count &optional start)
  "Apply COUNT EDIT mutations to STORE in this process and print phase plists.
START (default 0) offsets the target headings, so successive processes
never repeat a target; a delete needs a heading that still exists."
  (let ((load-started (float-time)))
    (require 'dorg)
    (require 'dorg-api)
    (require 'dorg-bm25)
    (require 'mysql nil t)
    (let* ((load-seconds (- (float-time) load-started))
           (dolt-dir (car (dorg-bm25-maintenance-probe--paths store)))
           (meta (dorg-bm25-maintenance-probe--meta store))
           (root (plist-get meta :root-id))
           (offset (pcase edit ('title 1) ('body 1001) ('delete 2001)))
           (dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-bm25-maintenance-probe--instrument)
      (prin1 (list :probe 'process :edit edit :module-load load-seconds
                   :source-dir (expand-file-name default-directory)
                   :fixture meta))
      (terpri)
      (dotimes (i count)
        (setq dorg-bm25-maintenance-probe--trace nil
              dorg-bm25-maintenance-probe--tokenize-calls 0)
        (let* ((id (+ 100000 offset (* 7 (+ i (or start 0)))))
               (started (float-time))
               (result (condition-case err
                           (dorg-api-execute
                            (dorg-bm25-maintenance-probe--form edit id i)
                            :mode 'apply
                            :source (list :dolt-dir dolt-dir :root-id root))
                         (error (list :ok nil :error (error-message-string err)))))
               (total (- (float-time) started)))
          (prin1 (list :phase (if (= i 0) 'fresh 'warm) :edit edit :id id
                       :ok (plist-get result :ok) :error (plist-get result :error)
                       :total total
                       :phases (mapcar (lambda (entry)
                                         (cons (car entry)
                                               (dorg-bm25-maintenance-probe--sum (car entry))))
                                       dorg-bm25-maintenance-probe--phases)
                       :fetched-headings (dorg-bm25-maintenance-probe--fetched)
                       :tokenize-calls dorg-bm25-maintenance-probe--tokenize-calls))
          (terpri))))))

(defun dorg-bm25-maintenance-probe-prior-reads (store &optional repeats)
  "Time keyed revision reads of BM25 aggregate rows in STORE at its HEAD.
Reads the manifest by (root, profile) and term rows by (root, profile,
field, term IN K terms) for K = 5, 50 and 500 real terms, REPEATS times each
\(default 5), and prints each statement's EXPLAIN PLAN once."
  (require 'dorg)
  (let* ((dolt-dir (car (dorg-bm25-maintenance-probe--paths store)))
         (root (plist-get (dorg-bm25-maintenance-probe--meta store) :root-id))
         (head (dorg-head-commit dolt-dir))
         (repeats (or repeats 5))
         (terms (mapcar (lambda (row) (alist-get 'term row))
                        (dorg-sql--at-revision
                         dolt-dir head
                         (format (concat "SELECT term FROM dorg_bm25_terms WHERE root_id = %d"
                                         " AND profile = 'sensitive' AND field_name = 'body'"
                                         " ORDER BY term LIMIT 500")
                                 root))))
         (statements
          (cons (cons 'manifest
                      (format (concat "SELECT * FROM dorg_bm25_manifest WHERE root_id = %d"
                                      " AND profile = 'sensitive'")
                              root))
                (mapcar (lambda (k)
                          (cons (intern (format "terms-%d" k))
                                (format (concat "SELECT * FROM dorg_bm25_terms WHERE root_id = %d"
                                                " AND profile = 'sensitive' AND field_name = 'body'"
                                                " AND term IN (%s)")
                                        root
                                        (mapconcat (lambda (term) (format "'%s'" term))
                                                   (cl-subseq terms 0 (min k (length terms)))
                                                   ", "))))
                        '(5 50 500)))))
    (prin1 (list :probe 'prior-reads :head head :available-terms (length terms)))
    (terpri)
    (dolist (statement statements)
      (let (samples rows)
        (dotimes (_ repeats)
          (let ((started (float-time)))
            (setq rows (length (dorg-sql--at-revision dolt-dir head (cdr statement))))
            (push (- (float-time) started) samples)))
        (prin1 (list :read (car statement) :rows rows
                     :seconds (sort samples #'<)
                     :plan (mapconcat (lambda (row) (format "%s" (alist-get 'plan row)))
                                      (dorg-sql--at-revision
                                       dolt-dir head (concat "EXPLAIN PLAN " (cdr statement)))
                                      "\n")))
        (terpri)))))

(provide 'dorg-bm25-maintenance-probe)

;;; dorg-bm25-maintenance-probe.el ends here
