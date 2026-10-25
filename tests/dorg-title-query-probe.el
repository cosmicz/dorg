;;; dorg-title-query-probe.el --- Phase probe for title queries -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An opt-in measurement harness, not a test.  It seeds a declared
;; tracker-shaped store and runs multi-term title queries through the public
;; snapshot door in a fresh Emacs process, splitting each read into phases:
;; module load, snapshot capture, every SQL statement (wire and row decode
;; separately), projection and JSON output.
;;
;; The harness deliberately uses only the public import and select facades
;; plus a test extension field, so the same file can be loaded against an
;; older checkout to produce a baseline row beside the candidate's row.  It
;; asserts nothing about time; callers record the printed plists.
;;
;; Fixture shape: BEADS tracker items, each a level-1 heading with a
;; BEAD_ID property, followed by two level-2 sections without BEAD_ID.  A
;; sparse, deterministic subset of item titles and section titles contains
;; the probe needles in mixed case, so the BEAD_ID guard and the case mode
;; both change the answer.
;;
;; Usage, from a checkout root (the store directory must not exist yet):
;;
;;   emacs --batch -Q -L . -l tests/dorg-title-query-probe.el \
;;     --eval '(dorg-title-query-probe-seed "STORE" 3000)'
;;   emacs --batch -Q -L . -l tests/dorg-title-query-probe.el \
;;     --eval '(dorg-title-query-probe-serve "STORE")'
;;   emacs --batch -Q -L . -l tests/dorg-title-query-probe.el \
;;     --eval "(dorg-title-query-probe-run \"STORE\" 5 '(LABEL))"
;;   emacs --batch -Q -L . -l tests/dorg-title-query-probe.el \
;;     --eval '(dorg-title-query-probe-stop "STORE")'
;;
;; Each `run' is a fresh process against an already-running server, given
;; one LABEL from `dorg-title-query-probe-queries' so that label's first read
;; opens its own connection and context: that read is the fresh sample and
;; the repeats are warm.

;;; Code:

(require 'cl-lib)
(require 'json)

;; DOrg itself loads inside each entry point so the run can time it.
(defvar dorg--extensions)
(declare-function dorg-backend-dolt "dorg-backend" (dolt-dir &optional root-id))
(declare-function make-dorg-registration "dorg-registry" (&rest slots))
(declare-function dorg-ql-select-snapshot "dorg-ql" (query &optional source))
(declare-function dorg-register-extension "dorg-ext" (name &rest args))
(declare-function dorg-dolt-start "dorg-dolt-server" (dolt-dir))
(declare-function dorg-dolt-stop "dorg-dolt-server" (dolt-dir))
(declare-function dorg-sql "dorg-dolt" (dolt-dir query &rest args))
(declare-function dorg-head-commit "dorg-dolt" (dolt-dir))
(declare-function dorg-import-document "dorg-import" (source &rest options))

(defconst dorg-title-query-probe--words
  '("parser" "index" "cache" "renderer" "session" "queue" "lease" "import"
    "export" "schema" "budget" "audit" "planner" "history" "server" "mutate")
  "Filler words for generated titles.")

(defun dorg-title-query-probe--title (i)
  "Return the deterministic title of tracker item I."
  (let* ((n (length dorg-title-query-probe--words))
         (base (format "%s %s item %d"
                       (nth (mod i n) dorg-title-query-probe--words)
                       (nth (mod (* 7 i) n) dorg-title-query-probe--words)
                       i)))
    (cond ((= 0 (mod i 97)) (concat base " ontology"))
          ((= 0 (mod i 89)) (concat "Ontology " base))
          ((= 0 (mod i 151)) (concat base " researcher"))
          ((= 0 (mod i 61)) (concat base " roles"))
          ((= 0 (mod i 67)) (concat base " ROLES"))
          (t base))))

(defun dorg-title-query-probe-content (beads)
  "Return the tracker-shaped Org text for BEADS items."
  (mapconcat
   (lambda (i)
     (concat (format "* TODO %s\n:PROPERTIES:\n:BEAD_ID: trk-%05d\n:END:\n"
                     (dorg-title-query-probe--title i) i)
             (if (= 0 (mod i 53))
                 "** Design ontology roles\nSection text.\n"
               "** Design\nSection text.\n")
             "** Notes\nNote text.\n"))
   (number-sequence 1 beads) ""))

(defun dorg-title-query-probe--paths (store)
  "Return (DOLT-DIR SOURCE-FILE TARGET-FILE META-FILE) under STORE."
  (let ((store (file-name-as-directory (expand-file-name store))))
    (list (expand-file-name "dolt" store)
          (expand-file-name "source.org" store)
          (expand-file-name "tracker.org" store)
          (expand-file-name "probe-meta.eld" store))))

(defun dorg-title-query-probe-seed (store beads)
  "Import a BEADS-item tracker fixture into a new STORE directory."
  (require 'dorg)
  (require 'dorg-import)
  (pcase-let ((`(,dolt-dir ,source ,target ,meta)
               (dorg-title-query-probe--paths store)))
    (when (file-exists-p store)
      (error "Probe store already exists: %s" store))
    (make-directory dolt-dir t)
    (with-temp-file source
      (insert (dorg-title-query-probe-content beads)))
    (let* ((started (float-time))
           (result (dorg-import-document source :dolt-dir dolt-dir
                                         :target-file target
                                         :mode 'bootstrap))
           (head (dorg-head-commit dolt-dir))
           (root (caar (mapcar (lambda (row) (list (alist-get 'root_id row)))
                               (dorg-sql dolt-dir
                                         "SELECT root_id FROM dorg_files")))))
      (with-temp-file meta
        (prin1 (list :beads beads :root-id root :head head
                     :import-seconds (- (float-time) started)
                     :import-result result)
               (current-buffer)))
      (princ (format "seeded store=%s root=%S head=%s seconds=%.3f\n"
                     store root head (- (float-time) started))))))

(defun dorg-title-query-probe-serve (store)
  "Start STORE's detached sql-server, as a deployed consumer would find it."
  (require 'dorg)
  (let ((started (float-time))
        (port (dorg-dolt-start (car (dorg-title-query-probe--paths store)))))
    (princ (format "served store=%s port=%S seconds=%.3f\n"
                   store port (- (float-time) started)))))

(defun dorg-title-query-probe-stop (store)
  "Stop STORE's detached sql-server."
  (require 'dorg)
  (princ (format "stopped store=%s result=%S\n" store
                 (dorg-dolt-stop (car (dorg-title-query-probe--paths store))))))

(defun dorg-title-query-probe--register-extension ()
  "Register the test identity column standing in for a consumer's bead id."
  (dorg-register-extension
   'probe :schema "SELECT 1" :version 1
   :fields
   (list (list :name 'probe/bead-id :type 'string :cardinality 'one
               :dependencies '((property "BEAD_ID"))
               :eval (lambda (_context read) (funcall read '(property "BEAD_ID")))
               :sql (lambda (_context _aliases)
                      '(:expression "probe_bead.prop_value"
                        :joins ((probe_bead
                                 . "LEFT JOIN heading_properties probe_bead ON probe_bead.heading_id = h.id AND probe_bead.prop_key = 'BEAD_ID'"))))))))

(defun dorg-title-query-probe--needles ()
  "Return the probe's literal needles, as in the reported query."
  '("ontology" "researcher" "roles"))

(defun dorg-title-query-probe--long-form (case-mode projection)
  "Return the long OR query under CASE-MODE with PROJECTION columns."
  (let ((leaves (mapcar (lambda (needle)
                          (if case-mode
                              `(contains (field title) ,needle :case ,case-mode)
                            `(contains (field title) ,needle)))
                        (dorg-title-query-probe--needles))))
    `(pipe (where (and (exists (property "BEAD_ID")) (or ,@leaves)))
           (select ,@projection))))

(defun dorg-title-query-probe-queries ()
  "Return labelled probe queries.
The first two are the baseline-supported reported shapes; the rest need the
candidate reader and are recorded as refusals elsewhere."
  (let ((by-property '((as (property "BEAD_ID") id) (field title)))
        (by-extension '((as (field probe/bead-id) id) (field title))))
    `((long-property-sensitive
       . ,(dorg-title-query-probe--long-form nil by-property))
      (long-extension-sensitive
       . ,(dorg-title-query-probe--long-form nil by-extension))
      (long-extension-insensitive
       . ,(dorg-title-query-probe--long-form 'insensitive by-extension))
      (any-extension-insensitive
       . (pipe (where (and (exists (property "BEAD_ID"))
                           (contains-any (field title)
                                         ,(dorg-title-query-probe--needles)
                                         :case insensitive)))
               (select ,@by-extension)))
      (sparse-extension-insensitive
       . (pipe (where (and (exists (property "BEAD_ID"))
                           (contains (field title) "zz-absent"
                                     :case insensitive)))
               (select ,@by-extension))))))

;; Every sample is (KIND SECONDS DETAIL OUTCOME PHASE).  OUTCOME is `ok' or
;; the signalled error symbol, so a failed statement keeps its time; PHASE is
;; the enclosing pipeline phase, so SQL is attributed to the phase it ran in.

(defvar dorg-title-query-probe--trace nil
  "Samples collected for the read in flight, newest first.")

(defvar dorg-title-query-probe--phase nil
  "The pipeline phase currently running, bound by the phase advice.")

(defconst dorg-title-query-probe--phases
  '((prepare . dorg-ql--prepare-query)
    (validate . dorg-ql--dolt-validate-extension-memberships)
    (head . dorg-head-commit)
    (context . dorg-ql--resolved-dolt-registration)
    (context . dorg-ql--make-invocation)
    (context . dorg-ql--resolve-extension-contexts)
    (eval . dorg-ql--dolt-eval))
  "Public-pipeline functions timed as phases, present in old and new checkouts.")

(defconst dorg-title-query-probe--sql-kinds
  '((statement . dorg-sql--at-revision)
    (live-statement . dorg-sql)
    (wire . mysql-query)
    (decode . dorg-sql--mysql-rows-to-alists))
  "Transport functions timed as SQL samples.")

(defun dorg-title-query-probe--detail (kind args)
  "Return a short statement label for KIND called with ARGS, or nil."
  (let ((sql (pcase kind
               ('statement (nth 2 args))
               ('live-statement (nth 1 args)))))
    (and (stringp sql) (substring sql 0 (min 72 (length sql))))))

(defun dorg-title-query-probe--timed (kind phase-p)
  "Return :around advice recording KIND samples; PHASE-P binds the phase."
  (lambda (orig &rest args)
    (if (and phase-p dorg-title-query-probe--phase)
        ;; A phase function called inside another phase is that phase's work.
        (apply orig args)
      (let ((started (float-time)) (outcome 'ok))
        (unwind-protect
            (condition-case err
                (if phase-p
                    (let ((dorg-title-query-probe--phase kind)) (apply orig args))
                  (apply orig args))
              (t (setq outcome (car err))
                 (signal (car err) (cdr err))))
          (push (list kind (- (float-time) started)
                      (dorg-title-query-probe--detail kind args)
                      outcome (if phase-p kind dorg-title-query-probe--phase))
                dorg-title-query-probe--trace))))))

(defun dorg-title-query-probe--instrument ()
  "Advise the phase and transport functions this checkout defines."
  (dolist (entry dorg-title-query-probe--phases)
    (when (fboundp (cdr entry))
      (advice-add (cdr entry) :around
                  (dorg-title-query-probe--timed (car entry) t))))
  (dolist (entry dorg-title-query-probe--sql-kinds)
    (when (fboundp (cdr entry))
      (advice-add (cdr entry) :around
                  (dorg-title-query-probe--timed (car entry) nil)))))

(defun dorg-title-query-probe--sum (kind &optional phase)
  "Return summed seconds of KIND samples, only those in PHASE when non-nil.
Phase samples are recorded only at the outermost call, so they never nest."
  (cl-loop for sample in dorg-title-query-probe--trace
           when (and (eq (car sample) kind)
                     (or (null phase) (eq (nth 4 sample) phase)))
           sum (nth 1 sample)))

(defun dorg-title-query-probe--sql (&optional phase)
  "Return summed statement seconds, only those in PHASE when non-nil."
  (+ (dorg-title-query-probe--sum 'statement phase)
     (dorg-title-query-probe--sum 'live-statement phase)))

(defun dorg-title-query-probe--outcome (err)
  "Classify ERR as a typed QL refusal or an execution failure."
  (if (memq 'dorg-ql-error (get (car err) 'error-conditions))
      (list :status 'refused
            :reason (and (fboundp 'dorg-error-reason) (dorg-error-reason err)))
    (list :status 'failed :reason (car err))))

(defun dorg-title-query-probe--read (label query reg)
  "Run QUERY at REG once and return a phase plist labelled LABEL."
  (setq dorg-title-query-probe--trace nil)
  (let ((started (float-time)) rows outcome error-message output-seconds)
    (condition-case err
        (let ((snapshot (dorg-ql-select-snapshot query reg)))
          (setq rows (plist-get snapshot :rows))
          (let ((output-started (float-time)))
            (json-encode (mapcar (lambda (row) (plist-get row :values)) rows))
            (setq output-seconds (- (float-time) output-started))))
      (error (setq outcome (dorg-title-query-probe--outcome err)
                   error-message (error-message-string err))))
    (let* ((total (- (float-time) started))
           (phases (mapcar (lambda (phase)
                             (list phase (dorg-title-query-probe--sum phase)
                                   (dorg-title-query-probe--sql phase)))
                           '(prepare validate head context eval)))
           (eval (dorg-title-query-probe--sum 'eval)))
      (append
       (list :label label)
       (or outcome (list :status 'ok :reason nil))
       (list :error error-message
             :rows (length rows)
             :total total
             :sql (dorg-title-query-probe--sql)
             :wire (dorg-title-query-probe--sum 'wire)
             :decode (dorg-title-query-probe--sum 'decode)
             ;; Each phase is (NAME SECONDS SQL-SECONDS-INSIDE-IT).
             :phases phases
             ;; Elisp work inside the executor: selection evaluation and
             ;; row projection, without its statements.
             :projection (- eval (dorg-title-query-probe--sql 'eval))
             :output output-seconds
             ;; Read time outside every timed phase and the output step.
             :unattributed (- total (apply #'+ (mapcar #'cadr phases))
                              (or output-seconds 0))
             :statements
             (cl-loop for sample in (reverse dorg-title-query-probe--trace)
                      when (memq (car sample) '(statement live-statement))
                      collect (list (nth 1 sample) (nth 4 sample)
                                    (nth 3 sample) (nth 2 sample))))))))

(defun dorg-title-query-probe-run (store warm-repeats &optional labels)
  "Read STORE with each probe query: one fresh read then WARM-REPEATS warm.
LABELS restricts the run to those query labels; give one label per process
so every label's fresh read opens its own connection and context.  Prints
one plist per read."
  (let ((load-started (float-time)))
    (require 'dorg)
    (require 'dorg-ql)
    (require 'dorg-ql-dolt)
    (require 'mysql nil t)
    (let ((load-seconds (- (float-time) load-started)))
      (pcase-let* ((`(,dolt-dir ,_source ,target ,meta)
                    (dorg-title-query-probe--paths store))
                   (plist (with-temp-buffer
                            (insert-file-contents meta)
                            (read (current-buffer))))
                   (reg (make-dorg-registration
                         :backend (dorg-backend-dolt
                                   (file-name-as-directory dolt-dir)
                                   (plist-get plist :root-id))
                         :file target))
                   (dorg--extensions nil))
        (dorg-title-query-probe--register-extension)
        (dorg-title-query-probe--instrument)
        (prin1 (list :probe 'process
                     :emacs emacs-version
                     :source-dir (expand-file-name default-directory)
                     :startup-to-probe (- load-started
                                          (float-time before-init-time))
                     :module-load load-seconds
                     :labels labels
                     :store store :fixture plist))
        (terpri)
        (dolist (entry (dorg-title-query-probe-queries))
          (when (or (null labels) (memq (car entry) labels))
            (dotimes (i (1+ warm-repeats))
              (prin1 (append (list :phase (if (= i 0) 'fresh 'warm))
                             (dorg-title-query-probe--read
                              (car entry) (cdr entry) reg)))
              (terpri))))))))

(provide 'dorg-title-query-probe)

;;; dorg-title-query-probe.el ends here
