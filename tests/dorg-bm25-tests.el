;;; dorg-bm25-tests.el --- Q1 lexical profile qualification -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-bm25)
(require 'dorg-dolt)
(require 'dorg-dolt-tests)
(require 'dorg-history)
(require 'dorg-mutate-tests)
(require 'dorg-ql-ast)
(require 'dorg-ql)
(require 'dorg-ql-dolt)

(defun dorg-test-bm25--image (&rest headings)
  "Return a candidate postimage containing HEADINGS by numeric identity."
  (let ((image (make-hash-table :test 'eql)))
    (dolist (heading headings image)
      (puthash (dorg-heading-id heading) heading image))))

(defun dorg-test-bm25--row (rows key value)
  "Return the first row in ROWS whose KEY cell equals VALUE."
  (cl-find value rows :key (lambda (row) (alist-get key row)) :test #'equal))

(defun dorg-test-bm25--scalar (dolt-dir sql)
  "Return SQL's only scalar cell in DOLT-DIR, or nil when no row exists."
  (when-let ((row (car (dorg-sql dolt-dir sql))))
    (cdar row)))

(defun dorg-test-bm25--schema-present-p (dolt-dir)
  "Return non-nil when DOLT-DIR has the BM25 derived schema."
  (dorg-sql dolt-dir "SHOW TABLES LIKE 'dorg_bm25_documents'"))

(defun dorg-test-bm25--other-project (context)
  "Project changed headings into the small installed-other control table.

This is deliberately a heading-keyed bounded projector: the mixed-install
control must distinguish its selected effects from an uninstalled whole-root
BM25 projector, not merely observe that the core mutation succeeded."
  (let ((root-id (dorg-ext-index-context-root-id context))
        effects)
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)
                           (dorg-ext-index-context-moved context)))
      (let ((id (dorg-ext-index-entry-id entry))
            (heading (dorg-ext-index-entry-after entry)))
        (push (list :table "dorg_bm25_other_control"
                    :delete (list (cons 'heading_id id)))
              effects)
        (when heading
          (push (list :table "dorg_bm25_other_control"
                      :insert (list (cons 'root_id root-id)
                                    (cons 'heading_id id)
                                    (cons 'title (dorg-heading-title heading))))
                effects))))
    (nreverse effects)))

(defun dorg-test-bm25--oracle (tf dl avgdl df n k1 b)
  "Independent literal BM25 formula for Q1 controls."
  (if (zerop tf)
      0.0
    (let* ((idf (log (+ 1.0 (/ (+ (- n df) 0.5) (+ df 0.5)))))
           (normalizer (+ 1.0 (- b) (* b (/ (float dl) avgdl)))))
      (* idf (/ (* tf (+ k1 1.0))
                (+ tf (* k1 normalizer)))))))

(defun dorg-test-bm25--close-p (actual expected)
  "Return non-nil when ACTUAL and EXPECTED meet the Q1 score tolerance."
  (<= (abs (- actual expected))
      (max 1e-12 (* 1e-9 (max (abs actual) (abs expected))))))

(defconst dorg-test-bm25--public-corpus-license "CC0-1.0"
  "License for the deterministic synthetic lexical measurement corpus.")

(defun dorg-test-bm25--public-corpus (partition count)
  "Return COUNT public-safe synthetic Org documents for PARTITION.

The three committed generator partitions make the named 3,000-document
measurement corpus without copying private tracker or filesystem material into
the repository.  Its repeated, rare, identifier-like, punctuation, and Unicode
tokens are deliberate lexical cases, not sampled user content."
  (concat
   (format "#+TITLE: Synthetic lexical corpus %d\n#+LICENSE: %s\n"
           partition dorg-test-bm25--public-corpus-license)
   (mapconcat
    (lambda (index)
      (format "* Atlas-%d-%04d\n%s\n"
              partition index
              (cond ((zerop (% index 97)) "orchid orchid, A_42 café")
                    ((zerop (% index 29)) "atlas atlas punctuation: river-stone")
                    ((zerop (% index 11)) "common atlas cedar")
                    (t "common cedar river"))))
    (number-sequence 1 count) "")))

(ert-deftest dorg-test-search-bm25-fixture-public-safe ()
  "The committed 3,000-document corpus is deterministic and public-safe."
  (let ((corpus (mapconcat (lambda (partition)
                             (dorg-test-bm25--public-corpus partition 1000))
                           '(1 2 3) "")))
    (with-temp-buffer
      (insert corpus)
      (should (= 3000 (how-many "^\\* " (point-min) (point-max)))))
    (should (string-match-p "#\\+LICENSE: CC0-1.0" corpus))
    (dolist (forbidden (list (concat "/Us" "ers/")
                             (concat "\\bd" "org-[a-z0-9]\\{3,5\\}\\b")
                         "\\bseat-[[:alnum:]-]+\\b"))
      (should-not (string-match-p forbidden corpus)))))

(defun dorg-test-bm25--time-call (function)
  "Return (SECONDS . VALUE) from one real FUNCTION invocation."
  (let ((start (float-time))
        value)
    (setq value (funcall function))
    (cons (- (float-time) start) value)))

(defun dorg-test-bm25--recall-at (ranked relevant k)
  "Return Recall@K for RANKED identities and judged RELEVANT identities."
  (if (null relevant) 0.0
    (/ (float (length (cl-intersection (cl-subseq ranked 0 (min k (length ranked)))
                                        relevant :test #'equal)))
       (length relevant))))

(defun dorg-test-bm25--mrr (ranked relevant)
  "Return reciprocal rank of the first RELEVANT identity in RANKED."
  (let ((position (cl-position-if (lambda (id) (member id relevant)) ranked)))
    (if position (/ 1.0 (1+ position)) 0.0)))

(defun dorg-test-bm25--ndcg-at (ranked gains k)
  "Return nDCG@K for RANKED identities and alist GAINS."
  (let* ((actual (cl-loop for id in (cl-subseq ranked 0 (min k (length ranked)))
                          for position from 1
                          sum (/ (float (or (cdr (assoc id gains)) 0))
                                 (log (1+ position) 2))))
         (ideal-gains (sort (mapcar #'cdr gains) #'>))
         (ideal (cl-loop for gain in (cl-subseq ideal-gains 0 (min k (length ideal-gains)))
                         for position from 1
                         sum (/ (float gain) (log (1+ position) 2)))))
    (if (zerop ideal) 0.0 (/ actual ideal))))

(ert-deftest dorg-test-search-bm25-held-out-quality-metrics ()
  "Held-out judgments keep BM25 tuning metrics explicit and reproducible."
  ;; The fixture labels are synthetic corpus titles, not user material.  Atlas
  ;; is the baseline query; Orchid is held out and has three graded judgments.
  (let ((ranked '("Atlas-1-0029" "Atlas-1-0001" "Atlas-1-0097"))
        (relevant '("Atlas-1-0029" "Atlas-1-0097"))
        (gains '(("Atlas-1-0029" . 3) ("Atlas-1-0097" . 2))))
    (should (= 1.0 (dorg-test-bm25--recall-at ranked relevant 3)))
    (should (= 1.0 (dorg-test-bm25--mrr ranked relevant)))
    (should (dorg-test-bm25--close-p
             (dorg-test-bm25--ndcg-at ranked gains 3)
             0.9385574520455129))))

(ert-deftest dorg-perf-search-bm25-public-corpus-observation ()
  "Record BM25 costs on the named public 3,000-document corpus.

This is deliberately opt-in: timings are host observations, never an ERT
threshold.  Functional assertions only prove the measured route returned the
complete expected rare-token result set."
  ;; The full suite enables generic DORG_BENCH benchmarks, but this corpus
  ;; import exceeds the shared fixture lease on some hosts.  It is an
  ;; observational receipt, so require an explicit operator measurement run.
  (skip-unless (getenv "DORG_BM25_CORPUS_BENCH"))
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install lexical extension")
      (let* ((root-id (dorg-test-mutate--seed
                       dolt-dir (dorg-test-bm25--public-corpus 1 3000) 100))
             (source (make-dorg-registration
                      :backend (dorg-backend-dolt dolt-dir root-id)))
             (build (dorg-test-bm25--time-call
                     (lambda ()
                       (dorg-ext-rebuild-index
                        'bm25 root-id :dolt-dir dolt-dir
                        :revision (dorg-head-commit dolt-dir)
                        :identity 'dorg :mode 'repair))))
             (cold (dorg-test-bm25--time-call
                    (lambda ()
                      (dorg-ql-search-snapshot
                       '(pipe (rank-bm25 "orchid" :profile sensitive :top-k 100)) source))))
             (warm (dorg-test-bm25--time-call
                    (lambda ()
                      (dorg-ql-search-snapshot
                       '(pipe (rank-bm25 "orchid" :profile sensitive :top-k 100)) source))))
             (incremental (dorg-test-bm25--time-call
                           (lambda ()
                             (dorg-test-mutate--apply-patch-batch
                              (dorg-mutate--resolve-source
                               (list :dolt-dir dolt-dir :root-id root-id))
                              (list
                               (dorg-test-mutate--heading-update-patch
                                (dorg-test-mutate--id-by-title
                                 dolt-dir "Atlas-1-0001")
                                :title "Atlas-1-0001 revised"))))))
             (matches (plist-get (cdr warm) :matches))
             (bytes (dorg-test-bm25--scalar
                     dolt-dir
                     (format (concat "SELECT SUM(CHAR_LENGTH(term) + CHAR_LENGTH(field_name)"
                                     " + 24) FROM dorg_bm25_postings"
                                     " WHERE root_id = %d")
                             root-id))))
        (should (= 30 (length matches)))
        (should (cl-every (lambda (match)
                            (numberp (plist-get match :score)))
                          matches))
        (should (assq 'bm25 (plist-get (cdr incremental) :extensions)))
        (message (concat "[bm25-public-corpus] documents=3000 profile=%s "
                         "logical-posting-bytes=%s build=%.6fs cold=%.6fs warm=%.6fs "
                         "incremental-title-edit=%.6fs")
                 dorg-bm25-profile-version bytes (car build) (car cold) (car warm)
                 (car incremental))))))

(defun dorg-test-bm25--run-mixed-lifecycle (dolt-dir &optional delete-fn)
  "Apply the required mixed canonical lifecycle in DOLT-DIR.

DELETE-FN, when non-nil, receives SOURCE and the typed delete patch.  The
normal control uses the public mutation engine; the divergence control binds
only that delete's projector path while preserving the shared rebuild path.
Return the final root id and exact revision after the history restore."
  (let* ((root-id (dorg-test-mutate--seed
                   dolt-dir
                   "* Parent A\nalpha body\n** Move Me\nmove body\n* Parent B\nbeta body\n* Delete Me\nobsolete body\n* Delete Final\nfinal obsolete body\n"
                   100))
         (parent-a (dorg-test-mutate--id-by-title dolt-dir "Parent A"))
         (move-me (dorg-test-mutate--id-by-title dolt-dir "Move Me"))
         (parent-b (dorg-test-mutate--id-by-title dolt-dir "Parent B"))
         (delete-me (dorg-test-mutate--id-by-title dolt-dir "Delete Me"))
         (delete-final (dorg-test-mutate--id-by-title dolt-dir "Delete Final"))
         (source (dorg-mutate--resolve-source
                  (list :dolt-dir dolt-dir :root-id root-id))))
    ;; The seed is canonical data created before the optional extension gets
    ;; a mutation opportunity.  Establish the explicit derived baseline;
    ;; subsequent public mutations must maintain only their row delta.
    (dorg-ext-rebuild-index
     'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
     :identity 'dorg :mode 'repair)
    ;; Insert, then edit title and directly-owned body through public patches.
    (dorg-test-mutate--apply-patch-batch
     source
     (list (dorg-test-mutate--tree-insert-patch :root
                                                 '(:id 900 :title "Inserted"))))
    (dorg-test-bm25--assert-complete-audit
     dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir)))
    (dorg-test-mutate--apply-patch-batch
     source (list (dorg-test-mutate--heading-update-patch
                   parent-a :title "Parent A edited")))
    (dorg-test-bm25--assert-complete-audit
     dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir)))
    (dorg-test-mutate--apply-patch-batch
     source (list (dorg-test-mutate--text-append-patch parent-a "edited body")))
    (dorg-test-bm25--assert-complete-audit
     dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir)))
    ;; Delete before the restore checkpoint: its removed terms must decrement
    ;; df/N in both profiles, whether the operation is normal or mutated.
    (funcall (or delete-fn #'dorg-test-mutate--apply-patch-batch)
             source (list (dorg-test-mutate--delete-patch delete-me)))
    (unless delete-fn
      (dorg-test-bm25--assert-complete-audit
       dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir))))
    ;; Reparent within the same root, then make and restore an ordinary
    ;; heading revision.  The move remains part of the final canonical image;
    ;; the history restore is an additional DOrg mutation that must project
    ;; the same image as an exact rebuild.
    (dorg-test-mutate--apply-patch-batch
     source (list (dorg-test-mutate--move-patch
                   move-me (dorg-test-mutate--heading-address parent-b))))
    (should (= parent-b
               (dorg-heading-parent-id
                (gethash move-me (dorg-model-load-tree dolt-dir root-id)))))
    (unless delete-fn
      (dorg-test-bm25--assert-complete-audit
       dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir))))
    (let ((post-move (dorg-head-commit dolt-dir)))
      (dorg-test-mutate--apply-patch-batch
       source (list (dorg-test-mutate--heading-update-patch
                     parent-b :title "Parent B later")))
      (dorg-restore-to-commit dolt-dir root-id parent-b post-move)
      (should (equal "Parent B"
                     (dorg-test-bm25--scalar
                      dolt-dir
                      (format "SELECT title FROM headings WHERE id = %d" parent-b)))))
    ;; A final delete prevents a deliberately broken delete projection from
    ;; being healed by a later ordinary full-image projector call.
    (funcall (or delete-fn #'dorg-test-mutate--apply-patch-batch)
             source (list (dorg-test-mutate--delete-patch delete-final)))
    (unless delete-fn
      (dorg-test-bm25--assert-complete-audit
       dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir))))
    (list :root-id root-id :revision (dorg-head-commit dolt-dir)
          :parent-a parent-a :parent-b parent-b :move-me move-me
          :deleted (list delete-me delete-final))))

(defun dorg-test-bm25--assert-complete-audit (dolt-dir lifecycle)
  "Assert LIFECYCLE's exact-revision audit covers all profiles and tables."
  (let* ((root-id (plist-get lifecycle :root-id))
         (audit (dorg-ext-rebuild-index
                 'bm25 root-id :dolt-dir dolt-dir
                 :revision (plist-get lifecycle :revision)
                 :identity 'dorg :mode 'audit))
         (drift (plist-get audit :drift))
         (scope (mapcar (lambda (descriptor) (plist-get descriptor :name))
                        (plist-get audit :scope))))
    (should (plist-get audit :ok))
    (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                   drift))
    (should (equal '("dorg_bm25_documents" "dorg_bm25_postings"
                     "dorg_bm25_terms" "dorg_bm25_manifest")
                   scope))
    (dolist (table scope)
      (should (= 2 (string-to-number
                    (format "%s"
                            (dorg-test-bm25--scalar
                             dolt-dir
                             (format (concat "SELECT COUNT(DISTINCT profile) FROM %s "
                                             "WHERE root_id = %d")
                                     table root-id)))))))
    audit))

(ert-deftest dorg-test-search-bm25-route-b-oracle ()
  "Q1: profile tokenization and DOrg BM25 agree with an independent oracle."
  (should (equal (dorg-bm25-tokenize "Weekly_MEETING, 42!" 'sensitive)
                 '("Weekly_MEETING" "42")))
  (should (equal (dorg-bm25-tokenize "Weekly_MEETING, 42!" 'insensitive)
                 '("WEEKLY_MEETING" "42")))
  (should (equal (dorg-bm25-tokenize "Σigma--café" 'sensitive)
                 '("Σigma" "café")))
  ;; V1 intentionally does no Unicode normalization.  The token grammar
  ;; retains the combining mark, so these literal codepoint sequences differ.
  (should (equal (dorg-bm25-tokenize "café" 'sensitive) '("café")))
  (should (equal (dorg-bm25-tokenize "café" 'sensitive) '("café")))
  (should-not (equal (dorg-bm25-tokenize "café" 'sensitive)
                     (dorg-bm25-tokenize "café" 'sensitive)))
  (let ((overlong (make-string (1+ dorg-bm25-max-token-characters) ?a)))
    (should-not (member overlong (dorg-bm25-tokenize overlong 'sensitive)))
    (should (equal '("keep")
                   (dorg-bm25-tokenize (concat "keep " overlong) 'sensitive))))
  (let* ((term "alpha")
         (documents '("alpha alpha beta"
                      "alpha gamma"
                      "beta beta beta"
                      "gamma delta"
                      "delta"))
         ;; These are the route-B fixture's in-memory derived postings.
         ;; Schema work must not begin until this derivation and the oracle
         ;; agree on the profile's tokenization and score.
         (postings (mapcar (lambda (document)
                             (dorg-bm25-tokenize document 'sensitive))
                           documents))
         (n (length postings))
         (tf (cl-count term (car postings) :test #'string=))
         (dl (length (car postings)))
         (avgdl (/ (float (apply #'+ (mapcar #'length postings))) n))
         (df (cl-count-if (lambda (posting) (member term posting)) postings))
         (actual (dorg-bm25-score-term tf dl avgdl df n))
         (oracle (dorg-test-bm25--oracle tf dl avgdl df n 1.2 0.75))
         ;; This independently calculated literal is pinned so the control
         ;; is not satisfied merely by transcribing the same formula twice.
         (expected-score 1.0920795589672359))
    (should (= tf 2))
    (should (= dl 3))
    (should (= df 2))
    (should (= n 5))
    (should (dorg-test-bm25--close-p actual expected-score))
    (should (dorg-test-bm25--close-p actual oracle))
    (message "Q1 route-b profile=%s k1=%.1f b=%.2f score=%.17g oracle=%.17g"
             dorg-bm25-profile-version 1.2 0.75 actual oracle)
    ;; Saturation and normalization must differ from raw term count.
    (should (< (dorg-bm25-score-term 6 dl avgdl df n) (* 2 actual)))
    (should (> actual (dorg-bm25-score-term tf 18 avgdl df n)))
    ;; An OOV query term has no postings and therefore contributes zero.
    (should (= 0.0 (dorg-bm25-score-term tf dl avgdl 0 n)))))

(ert-deftest dorg-test-search-bm25-native-fulltext-observation ()
  "Q1: native FULLTEXT is observable but is not the DOrg scorer."
  (dorg-test-with-dolt
    (dorg-sql dolt-dir "CREATE TABLE bm25_q1 (id INT PRIMARY KEY, body TEXT)")
    (dorg-sql dolt-dir "INSERT INTO bm25_q1 VALUES (1, 'alpha alpha'), (2, 'beta')")
    (dorg-sql dolt-dir "CREATE FULLTEXT INDEX bm25_q1_body ON bm25_q1(body)")
    (let ((rows (dorg-sql dolt-dir
                          "SELECT id, MATCH(body) AGAINST ('alpha') AS score FROM bm25_q1 WHERE MATCH(body) AGAINST ('alpha') ORDER BY score DESC, id ASC")))
      (should (= 1 (length rows)))
      (should (= 1 (alist-get 'id (car rows))))
      (let ((score (alist-get 'score (car rows))))
        (should (numberp score))
        (should (> score 0))
        (message "Q1 native-fulltext score=%.17g (observation only)" score)))))

(ert-deftest dorg-test-search-bm25-q4-literal-saturation-and-length-scores ()
  "Q4 literals distinguish saturation and long-document normalization.

The expected values were independently calculated from the published v1
formula with N=4, df=1, k1=1.2, and b=.75; they are deliberately not derived
through the test oracle helper."
  (let ((saturated (dorg-bm25-score-term 4 4 3 1 4))
        (long-document (dorg-bm25-score-term 1 20 5 1 4))
        (single-short (dorg-bm25-score-term 1 1 3 1 4)))
    (should (dorg-test-bm25--close-p saturated 1.926356486921498))
    (should (dorg-test-bm25--close-p long-document 0.5405592182687876))
    (should (> saturated single-short))
    (should (< long-document single-short))))

(ert-deftest dorg-test-search-bm25-ranked-lowering-bounds-query-postings ()
  "The first ranked stage names only query postings, never a root scan."
  (let ((sql (dorg-bm25--postings-sql 17 'sensitive '("needle" "rare_id"))))
    (should (string-match-p "term IN ('needle', 'rare_id')" sql))
    (should (string-match-p "dorg_bm25_postings" sql))
    (should-not (string-match-p "dorg_bm25_documents\\|dorg_bm25_terms\\|headings" sql))
    (should-not (string-match-p "text_items\\|heading_items\\|org_files" sql)))
  (let ((query (concat "(pipe (rank-bm25 \""
                       (mapconcat (lambda (n) (format "term%d" n))
                                  (number-sequence 1 33) " ")
                       "\" :profile sensitive :top-k 1))")))
    (should-error
     (dorg-ql--dolt-bm25-rank-components (dorg-ql-normalize (read query)) 17)
     :type 'dorg-ql-error)))

(defun dorg-test-bm25--rank-plan (dolt-dir &optional revision)
  "Return the physical postings plan in DOLT-DIR, optionally at REVISION."
  (mapconcat (lambda (row) (format "%s" (alist-get 'plan row)))
             (if revision
                 (dorg-sql--at-revision
                  dolt-dir revision
                  (concat "EXPLAIN PLAN "
                          (dorg-bm25--postings-sql
                           100 'sensitive '("orchid"))))
               (dorg-sql
                dolt-dir
                (concat "EXPLAIN PLAN "
                        (dorg-bm25--postings-sql
                         100 'sensitive '("orchid")))))
             "\n"))

(defun dorg-test-bm25--seed-rank-plan-fixture (dolt-dir)
  "Materialize enough distinct postings for DOLT-DIR's physical-plan choice."
  (dorg-test-mutate--seed
   dolt-dir
   (concat "* Orchid\norchid orchid\n"
           "* Cedar\ncedar cedar\n"
           "* River\nriver river\n")
   100)
  (dorg-ext-rebuild-index
   'bm25 100 :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
   :identity 'dorg :mode 'repair))

(defun dorg-test-bm25--assert-rank-plan-uses-term-index (dolt-dir &optional revision)
  "Assert DOLT-DIR bounds ranked postings through the term-leading index."
  (let ((plan (dorg-test-bm25--rank-plan dolt-dir revision)))
    (should (string-match-p "IndexedTableAccess" plan))
    (should
     (string-match-p
      (regexp-quote
       "index: [dorg_bm25_postings.root_id,dorg_bm25_postings.profile,dorg_bm25_postings.term,dorg_bm25_postings.field_name,dorg_bm25_postings.heading_id]")
      plan))
    (should (string-match-p
             (regexp-quote
              "filters: [{[100, 100], [sensitive, sensitive], [orchid, orchid]")
             plan))))

(ert-deftest dorg-test-search-bm25-ranked-plan-uses-term-index ()
  "R6: ranked postings use a term-leading physical range, not a root scan."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      ;; The planner estimates the real derived cardinalities, not an empty
      ;; schema.  Seed both matching and non-matching postings so selecting
      ;; the term-leading key is a meaningful physical choice.
      (dorg-test-bm25--seed-rank-plan-fixture dolt-dir)
      (dorg-test-bm25--assert-rank-plan-uses-term-index dolt-dir)
      ;; This is the R6 mutation witness: the identical rank SQL must no
      ;; longer satisfy the physical-plan assertion after its lookup index is
      ;; removed.  It prevents a textual SQL capture from standing in for an
      ;; actual term-bounded access plan.
      (dorg-sql dolt-dir
                (format "DROP INDEX %s ON dorg_bm25_postings"
                        dorg-bm25-postings-term-index))
      (should-error
       (dorg-test-bm25--assert-rank-plan-uses-term-index dolt-dir)
       :type 'ert-test-failed))))

(ert-deftest dorg-test-search-bm25-ranked-revision-plan-uses-term-index ()
  "R6: revision-bound ranked postings retain their physical term range at scale."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install lexical extension")
      (dorg-test-mutate--seed
       dolt-dir (dorg-test-bm25--public-corpus 1 1000) 100)
      (dorg-ext-rebuild-index
       'bm25 100 :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
       :identity 'dorg :mode 'repair)
      (dorg-test-bm25--assert-rank-plan-uses-term-index
       dolt-dir (dorg-head-commit dolt-dir)))))

(ert-deftest dorg-test-search-bm25-postings-term-index-migrates-v1 ()
  "Existing v1 BM25 stores gain the ranked-postings index on migration."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((dorg--extensions nil)
           (old-schema (copy-sequence dorg-bm25-schema))
           (old-postings (nth 1 old-schema)))
      ;; Model a store installed by the prior schema rather than merely
      ;; creating a new v2 table.  The direct plan assertion below proves
      ;; the registered migration adds the usable physical access path.
      (setf (nth 1 old-schema)
            (replace-regexp-in-string
             (regexp-quote
              (format ", KEY %s (root_id, profile, term, field_name, heading_id)"
                      dorg-bm25-postings-term-index))
             "" old-postings t t))
      ;; Versions 1 and 2 also stored the index generation.
      (setf (nth 3 old-schema)
            (replace-regexp-in-string
             (regexp-quote "corpus_generation VARCHAR(128) NOT NULL,")
             "corpus_generation VARCHAR(128) NOT NULL, index_generation VARCHAR(255) NOT NULL,"
             (nth 3 old-schema) t t))
      (let ((dorg-bm25-schema-version 1)
            (dorg-bm25-schema old-schema))
        (dorg-bm25-register-extension)
        (dorg-migrate-schema dolt-dir))
      (dorg-commit dolt-dir "install BM25 v1 fixture")
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (should (equal "3"
                     (dorg-test-bm25--scalar
                      dolt-dir
                      "SELECT value FROM dorg_meta WHERE `key` = 'ext:bm25:version'")))
      (dorg-test-bm25--seed-rank-plan-fixture dolt-dir)
      (dorg-test-bm25--assert-rank-plan-uses-term-index dolt-dir))))

(ert-deftest dorg-test-search-bm25-normalizes-terminal-ranked-step ()
  "Rank BM25 has one canonical terminal AST form before any source read."
  (should
   (equal
    (dorg-ql-normalize
     '(pipe (where (tag "public"))
            (rank-bm25 "Alpha_42" :profile sensitive :top-k 7)
            (select (field title))))
    '(:pipe (:source :all)
            (:filter (:tag = "public" :case sensitive))
            (:rank-bm25 "Alpha_42" :profile sensitive :top-k 7)
            (:select (:column "title" (:field title)))))))

(ert-deftest dorg-test-search-bm25-ranked-step-refuses-nonterminal-or-invalid-forms ()
  "Malformed or nonterminal BM25 rank steps fail during pure normalization."
  (dolist
      (query '((pipe (rank-bm25 "" :profile sensitive :top-k 1))
               (pipe (rank-bm25 "alpha" :profile sensitive :top-k 0))
               (pipe (rank-bm25 "alpha" :profile sensitive :top-k 101))
               (pipe (rank-bm25 "alpha" :profile folded :top-k 1))
               (pipe (rank-bm25 "alpha" :profile sensitive :top-k 1 :extra t))
               (pipe (rank-bm25 "alpha" :profile sensitive :profile insensitive :top-k 1))
               (pipe (rank-bm25 "alpha" :profile sensitive :top-k 1)
                     (where (tag "late")))
               (pipe (traverse children)
                     (rank-bm25 "alpha" :profile sensitive :top-k 1))
               (pipe (unique)
                     (rank-bm25 "alpha" :profile sensitive :top-k 1))
               (pipe (rank-bm25 "alpha" :profile sensitive :top-k 1)
                     (rank-bm25 "beta" :profile sensitive :top-k 1))
               (one (pipe (rank-bm25 "alpha" :profile sensitive :top-k 1)))
               (and (rank-bm25 "alpha" :profile sensitive :top-k 1)
                    (id 1))))
    (should-error (dorg-ql-normalize query) :type 'dorg-ql-error)))

(ert-deftest dorg-test-search-bm25-public-envelope-is-snapshot-bound ()
  "The rank route has a provenance envelope without changing select rows."
  (let* ((dorg--extensions nil)
         (backend (dorg-backend-dolt "/unused" 17))
         (registration (make-dorg-registration :backend backend))
         (loaded 0))
    (cl-letf (((symbol-function 'dorg-load-extension)
               (lambda (name)
                 (should (equal name "bm25"))
                 (cl-incf loaded)
                 'bm25))
              ((symbol-function 'dorg-head-commit) (lambda (_dir) "captured"))
              ((symbol-function 'dorg-ql--dolt-bm25-search)
               (lambda (ast reg revision)
                 (should (equal revision "captured"))
                 (should (eq reg registration))
                 (should (equal (cadr (cl-find-if
                                       (lambda (step) (eq (car-safe step) :rank-bm25))
                                       ast))
                                "Alpha"))
                 '(:matches ((:heading-id 23 :score 1.25 :title "Alpha"))
                   :profile sensitive :index-generation "idx" :corpus-generation "corpus"))))
      (let ((envelope
             (dorg-ql-search-snapshot
              '(pipe (where (title "Alpha"))
                     (rank-bm25 "Alpha" :profile sensitive :top-k 3))
              registration)))
        (should (= loaded 1))
        (should (eq (plist-get envelope :command) 'search))
        (should (equal (plist-get envelope :snapshot) '(:kind dolt :head "captured")))
        (should (equal (plist-get envelope :matches)
                       '((:heading-id 23 :score 1.25 :title "Alpha"))))
        (should (equal (plist-get envelope :profile) 'sensitive))
        (should (equal (plist-get envelope :index-generation) "idx"))
        (should (equal (plist-get envelope :corpus-generation) "corpus"))))))

(ert-deftest dorg-test-search-bm25-public-route-refuses-non-dolt-before-ranking ()
  "Plain and model sources cannot silently substitute a ranking algorithm."
  (let ((dorg--extensions nil))
    (should-error
     (dorg-ql-search-snapshot
      '(pipe (rank-bm25 "Alpha" :profile sensitive :top-k 1))
      (list (make-dorg-heading :id 1 :level 1 :level-offset 1 :title "Alpha")))
     :type 'dorg-ql-error)))

(ert-deftest dorg-test-search-bm25-derivation-owns-document-boundaries ()
  "The shared derivation indexes titles plus direct text, never structure.

This is the pure step-8 control.  Registration and SQL lowering must consume
these candidate-derived rows rather than reconstructing document content."
  (let* ((root (make-dorg-heading :id 1 :parent-id nil :title "Root"))
         (parent (make-dorg-heading
                  :id 2 :parent-id 1 :title "Alpha title"
                  :items (list (dorg-text-item :text "Alpha body")
                               (dorg-planning-item :scheduled "[2026-01-01]")
                               (dorg-drawer-item :name "PROPERTIES"
                                                 :properties '(("SECRET" . "alpha")))
                               (dorg-keyword-item :keyword "DORG_HASH" :value "alpha"))))
         (child (make-dorg-heading :id 3 :parent-id 2 :title "Beta"
                                   :items (list (dorg-text-item :text "alpha alpha"))))
         (image (dorg-test-bm25--image root parent child))
         (derived (dorg-bm25-derive-image 1 image))
         (documents (plist-get derived :documents))
         (postings (plist-get derived :postings))
         (terms (plist-get derived :terms))
         (manifests (plist-get derived :manifests)))
    (should (= 4 (length documents))) ; two headings, two case profiles
    (should-not (dorg-test-bm25--row documents 'heading_id 1))
    (let ((parent-doc
           (cl-find-if (lambda (row)
                         (and (= 2 (alist-get 'heading_id row))
                              (equal "sensitive" (alist-get 'profile row))))
                       documents)))
      (should (equal "sensitive" (alist-get 'profile parent-doc)))
      (should (= 2 (alist-get 'title_length parent-doc)))
      (should (= 2 (alist-get 'body_length parent-doc))))
    (should (dorg-test-bm25--row postings 'term "Alpha"))
    (should-not (dorg-test-bm25--row postings 'term "SECRET"))
    (should-not (dorg-test-bm25--row postings 'term "DORG_HASH"))
    (let ((alpha (dorg-test-bm25--row terms 'term "alpha")))
      (should (equal "sensitive" (alist-get 'profile alpha)))
      (should (= 1 (alist-get 'document_frequency alpha))))
    ;; The binary schema collation agrees with the sensitive profile: these
    ;; are distinct terms, not a case-insensitive lookup alias.
    (should (dorg-test-bm25--row terms 'term "Alpha"))
    (should-not (equal (dorg-test-bm25--row terms 'term "Alpha")
                       (dorg-test-bm25--row terms 'term "alpha")))
    (let ((manifest (dorg-test-bm25--row manifests 'profile "sensitive")))
      (should (equal dorg-bm25-tokenizer-version
                     (alist-get 'tokenizer_version manifest)))
      (should (= dorg-bm25-schema-version (alist-get 'schema_version manifest)))
      (should (equal (dorg-bm25--corpus-generation 1 image)
                     (alist-get 'corpus_generation manifest)))
      ;; The generation is the xor of the two document digests; the index
      ;; generation is derived from it and is no longer stored.
      (should (equal (format "%064x"
                             (logxor (string-to-number
                                      (dorg-bm25-document-digest 2 "Alpha title" "Alpha body")
                                      16)
                                     (string-to-number
                                      (dorg-bm25-document-digest 3 "Beta" "alpha alpha")
                                      16)))
                     (alist-get 'corpus_generation manifest)))
      (should-not (assq 'index_generation manifest)))
    (should (= 2 (length manifests)))))

(ert-deftest dorg-test-search-bm25-projector-registers-derived-tables ()
  "The registered projector emits only DOrg-derived rows in owned tables."
  (let* ((root (make-dorg-heading :id 10 :parent-id nil :title "Root"))
         (heading (make-dorg-heading :id 11 :parent-id 10 :title "Alpha"
                                     :items (list (dorg-text-item :text "alpha beta"))))
         (context (dorg-ext-index-context-create
                   :source-kind 'dolt :root-id 10
                   :candidate-id "fixture-candidate-v1"
                   :created (list (dorg-ext-index-entry-create
                                   :kind 'create :id 11 :after heading))
                   :candidate-postimage (dorg-test-bm25--image root heading)))
         (dorg--extensions nil))
    (dorg-bm25-register-extension)
    (let* ((extension (dorg-get-extension 'bm25))
           (projection (dorg-ext-collect-index-projection
                        context '(bm25)
                        (list :priors (lambda (_requests) nil) :empty-roots #'identity)))
           (effects (plist-get projection :effects))
           (tables (delete-dups (mapcar (lambda (effect) (plist-get effect :table))
                                        effects))))
      (should extension)
      (should (= dorg-bm25-schema-version (dorg-extension-version extension)))
      (should (eq 'bounded (dorg-extension-project-scope extension)))
      (should (equal '(("dorg_bm25_terms" root_id profile field_name term)
                       ("dorg_bm25_manifest" root_id profile))
                     (dorg-ext-accumulate-specs (dorg-extension-read-tables extension))))
      ;; Without a fold port, accumulators cannot be bound to a base.
      (should (eq 'accumulator-unbound
                  (dorg-ext-index-error-reason
                   (should-error (dorg-ext-collect-index-projection context '(bm25))
                                 :type 'dorg-ext-index-error))))
      (should (equal '("dorg_bm25_documents" "dorg_bm25_manifest"
                       "dorg_bm25_postings" "dorg_bm25_terms")
                     (sort tables #'string<)))
      (should (cl-every (lambda (effect)
                          (member (plist-get effect :table)
                                  '("dorg_bm25_documents" "dorg_bm25_manifest"
                                    "dorg_bm25_postings" "dorg_bm25_terms")))
                        effects))
      (should (cl-some (lambda (effect)
                         (equal "alpha"
                                (alist-get 'term (plist-get effect :insert))))
                       effects)))))

(ert-deftest dorg-test-search-bm25-projector-emits-only-the-edited-document ()
  "One title edit among 1000 documents emits effects for that document only.
Heading-local rows name only the edited heading; the aggregates are one
document-frequency accumulator per touched term and one manifest
accumulator per profile.  No other document is tokenized."
  (let* ((root-id 100)
         (base (make-hash-table :test 'eql))
         (candidate (make-hash-table :test 'eql))
         (root (make-dorg-heading :id root-id :parent-id nil :title "Root"))
         (changed-id 101)
         (tokenized 0)
         (orig-tokenize (symbol-function 'dorg-bm25-tokenize)))
    (puthash root-id root base)
    (puthash root-id (dorg-heading-deep-copy root) candidate)
    (dotimes (offset 1000)
      (let* ((id (+ changed-id offset))
             (title (if (= id changed-id) "Alpha" "Common"))
             (heading (make-dorg-heading
                       :id id :parent-id root-id :title title
                       :items (list (dorg-text-item :text "shared body")))))
        (puthash id heading base)
        (puthash id (dorg-heading-deep-copy heading) candidate)))
    (setf (dorg-heading-title (gethash changed-id candidate)) "Omega")
    (let* ((context (dorg-ext-index-context-create
                     :source-kind 'dolt :root-id root-id
                     :changed (list (dorg-ext-index-entry-create
                                     :kind 'change :id changed-id
                                     :before (gethash changed-id base)
                                     :after (gethash changed-id candidate)))
                     :base-image base :candidate-postimage candidate))
           (effects (cl-letf (((symbol-function 'dorg-bm25-tokenize)
                               (lambda (&rest args)
                                 (cl-incf tokenized) (apply orig-tokenize args))))
                      (dorg-bm25-project context)))
           (rows (cl-remove-if (lambda (effect) (plist-member effect :accumulate)) effects))
           (terms (cl-remove-if-not (lambda (effect)
                                      (equal "dorg_bm25_terms" (plist-get effect :table)))
                                    effects))
           (manifests (cl-remove-if-not (lambda (effect)
                                          (equal "dorg_bm25_manifest" (plist-get effect :table)))
                                        effects)))
      (should (= 8 tokenized))
      (should (cl-every (lambda (effect)
                          (equal changed-id
                                 (alist-get 'heading_id (or (plist-get effect :insert)
                                                            (plist-get effect :delete)))))
                        rows))
      (should (equal (sort (mapcar (lambda (effect)
                                     (let ((key (plist-get effect :accumulate)))
                                       (list (alist-get 'profile key) (alist-get 'term key)
                                             (alist-get 'document_frequency
                                                        (plist-get effect :add)))))
                                   terms)
                           (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))
                     '(("insensitive" "ALPHA" -1) ("insensitive" "OMEGA" 1)
                       ("sensitive" "Alpha" -1) ("sensitive" "Omega" 1))))
      (should (= 2 (length manifests)))
      (should (cl-every (lambda (effect)
                          (equal '((corpus_size . 0) (title_length . 0) (body_length . 0))
                                 (plist-get effect :add)))
                        manifests)))))

(ert-deftest dorg-test-search-bm25-registration-keeps-uninstalled-store-bounded ()
  "An uninstalled BM25 projector selects no mutation participant in its store.

Exercise the same-root dry-run and apply paths, rather than only inspecting the
simulation scope: both must keep the BM25 schema absent and report no derived
extension effects while the ordinary core title change remains real."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((dorg--extensions nil)
           (_extension (dorg-bm25-register-extension))
           (root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n* Beta\n" 100))
           (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id root-id)))
           (patches (list (dorg-test-mutate--heading-update-patch
                           alpha :title "Alpha*")))
           (head (dorg-head-commit dolt-dir))
           (bounded (sort (copy-sequence
                           (dorg-mutate--dolt-scope-ids
                            source patches root-id head))
                          #'<))
           (sim (dorg-mutate--dolt-simulate source patches nil))
           (dry-run (dorg-test-mutate--apply-patch-batch
                     source patches :dry-run t))
           (applied (dorg-test-mutate--apply-patch-batch source patches)))
      (should-not (dorg-mutate--dolt-installed-index-extension-names dolt-dir))
      (should (equal bounded (plist-get sim :scope-ids)))
      (should (plist-get dry-run :dry-run))
      (should-not (plist-get dry-run :extensions))
      (should-not (plist-get applied :extensions))
      (should-not (dorg-test-bm25--schema-present-p dolt-dir))
      (should (equal "Alpha*"
                     (dorg-test-bm25--scalar
                      dolt-dir (format "SELECT title FROM headings WHERE id = %d" alpha)))))))

(ert-deftest dorg-test-search-bm25-uninstalled-projector-cross-root-move-is-core-only ()
  "A globally registered but uninstalled BM25 projector cannot poison cross-root paths.

The dry-run and the actual move share the cross-root projection seam.  Both
must select no BM25 effects; the move itself proves no missing derived table
was reached during either phase."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((dorg--extensions nil)
           (_extension (dorg-bm25-register-extension))
           (ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (source (dorg-mutate--resolve-source
                    (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root))))
           (patch (dorg-test-mutate--cross-root-move-patch
                   (plist-get ids :movable)
                   (dorg-test-mutate--heading-address (plist-get ids :dst-parent))
                   dolt-dir (plist-get ids :dst-root)))
           (dry-run (dorg-test-mutate--apply-patch-batch source (list patch) :dry-run t))
           (applied (dorg-test-mutate--apply-patch-batch source (list patch))))
      (should (plist-get dry-run :dry-run))
      (should-not (plist-get dry-run :extensions))
      (should-not (plist-get applied :extensions))
      (should-not (dorg-test-bm25--schema-present-p dolt-dir))
      (should (gethash (plist-get ids :movable)
                       (dorg-model-load-tree dolt-dir (plist-get ids :dst-root)))))))

(ert-deftest dorg-test-search-bm25-uninstalled-projector-does-not-shadow-installed-other ()
  "Only an installed projector participates when BM25 is merely registered.

This guards the inverse of the empty-selection case: selection must not be
implemented by disabling all projectors whenever one registered projector is
missing from the target store."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-register-extension
       'other
       :schema "CREATE TABLE dorg_bm25_other_control (root_id BIGINT, heading_id BIGINT PRIMARY KEY, title TEXT)"
       :version 1 :capabilities '(dolt)
       :read-tables '((:name "dorg_bm25_other_control" :key-column "heading_id"))
       :project #'dorg-test-bm25--other-project)
      ;; Install only OTHER, then register BM25 in the process after migration.
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install other projector")
      (dorg-bm25-register-extension)
      (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (alpha (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root-id)))
             (result (dorg-test-mutate--apply-patch-batch
                      source (list (dorg-test-mutate--heading-update-patch
                                    alpha :title "Alpha*")))))
        (should (equal '(other)
                       (dorg-mutate--dolt-installed-index-extension-names dolt-dir)))
        (should (assq 'other (plist-get result :extensions)))
        (should-not (assq 'bm25 (plist-get result :extensions)))
        (should-not (dorg-test-bm25--schema-present-p dolt-dir))
        (should (equal "Alpha*"
                       (dorg-test-bm25--scalar
                        dolt-dir (format (concat "SELECT title FROM dorg_bm25_other_control "
                                                "WHERE heading_id = %d") alpha))))))))

(ert-deftest dorg-test-search-bm25-cross-root-derived-rows-equal-fresh-rebuild ()
  "A BM25 cross-root move has the same rows as a fresh rebuild of BOTH roots.

The mutation context is intentionally an aggregate so generic validation can
observe the relocation.  This test prevents that aggregate from being treated
as one destination corpus: each root's maintained delta must equal the shared
pure derivation at the new exact revision."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install BM25 projector")
      (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id (plist-get ids :src-root))))
             (patch (dorg-test-mutate--cross-root-move-patch
                     (plist-get ids :movable)
                     (dorg-test-mutate--heading-address (plist-get ids :dst-parent))
                     dolt-dir (plist-get ids :dst-root))))
        ;; Establish the two root-local derived baselines before the move.
        (dolist (root-id (list (plist-get ids :src-root) (plist-get ids :dst-root)))
          (dorg-ext-rebuild-index 'bm25 root-id :dolt-dir dolt-dir
                                  :revision (dorg-head-commit dolt-dir)
                                  :identity 'dorg :mode 'repair))
        (let ((result (dorg-test-mutate--apply-patch-batch source (list patch))))
          (should (assq 'bm25 (plist-get result :extensions)))
          (dolist (root-id (list (plist-get ids :src-root) (plist-get ids :dst-root)))
            (let ((audit (dorg-ext-rebuild-index
                          'bm25 root-id :dolt-dir dolt-dir
                          :revision (dorg-head-commit dolt-dir)
                          :identity 'dorg :mode 'audit)))
              (should (plist-get audit :ok))
              (should (equal '(:missing-count 0 :extra-count 0 :differing-count 0)
                             (plist-get audit :drift))))))))))

(ert-deftest dorg-test-search-bm25-explicit-registration-migrates-schema ()
  "The optional lexical extension installs through the normal migration door."
  (dorg-test-with-dolt
    (dorg-test-with-clean-extensions
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dolist (table '("dorg_bm25_documents" "dorg_bm25_postings"
                       "dorg_bm25_terms" "dorg_bm25_manifest"))
        (should (dorg-sql dolt-dir (format "SHOW TABLES LIKE '%s'" table))))
      (should (equal (number-to-string dorg-bm25-schema-version)
                     (format "%s"
                             (alist-get 'value
                                        (car (dorg-sql
                                              dolt-dir
                                              "SELECT value FROM dorg_meta WHERE `key` = 'ext:bm25:version'")))))))))

(ert-deftest dorg-test-search-bm25-mutation-commits-derived-image-atomically ()
  "One core mutation commits the candidate-derived BM25 image in the same Dolt commit."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (heading-id (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root-id)))
             (commit-count (lambda ()
                             (string-to-number
                              (format "%s" (dorg-test-bm25--scalar
                                             dolt-dir "SELECT COUNT(*) FROM dolt_log")))))
             (_baseline (dorg-ext-rebuild-index
                          'bm25 root-id :dolt-dir dolt-dir
                          :revision (dorg-head-commit dolt-dir)
                          :identity 'dorg :mode 'repair))
             (before (funcall commit-count))
             (receipt (dorg-test-mutate--apply-patch-batch
                       source
                       (list (dorg-test-mutate--heading-update-patch
                              heading-id :title "Beta")))))
        (should (= (1+ before) (funcall commit-count)))
        (should (equal "Beta"
                       (dorg-test-bm25--scalar
                        dolt-dir
                        (format "SELECT title FROM headings WHERE id = %d" heading-id))))
        (should (= 2 (string-to-number
                      (format "%s" (dorg-test-bm25--scalar
                                     dolt-dir
                                     (format "SELECT COUNT(*) FROM dorg_bm25_documents WHERE root_id = %d"
                                             root-id))))))
        (should (equal "Beta"
                       (dorg-test-bm25--scalar
                        dolt-dir
                        (format (concat "SELECT term FROM dorg_bm25_postings "
                                        "WHERE root_id = %d AND heading_id = %d "
                                        "AND profile = 'sensitive' AND field_name = 'title'")
                                root-id heading-id))))
        (should (dorg-test-bm25--scalar
                 dolt-dir
                 (format "SELECT corpus_generation FROM dorg_bm25_manifest WHERE root_id = %d"
                         root-id)))
        (should (assq 'bm25 (plist-get receipt :extensions)))))))

(ert-deftest dorg-test-search-bm25-ranked-read-is-profiled-and-root-scoped ()
  "The public rank envelope reads bounded postings at one Dolt revision."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed
                       dolt-dir (concat "* Alpha\n:PROPERTIES:\n:K: retained\n:END:\n"
                                        "alpha alpha\n* Beta\nalpha\n") 100))
             (source (make-dorg-registration
                      :backend (dorg-backend-dolt dolt-dir root-id))))
        (dorg-ext-rebuild-index
         'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
         :identity 'dorg :mode 'repair)
        (let ((count (dorg-test-bm25--scalar
                      dolt-dir
                      (format (concat "SELECT COUNT(*) FROM dorg_bm25_manifest "
                                      "WHERE root_id = %d AND profile = 'sensitive'")
                              root-id))))
          (should (= 1 (string-to-number (format "%s" count)))))
        (let* ((result (dorg-ql-search-snapshot
                        '(pipe (rank-bm25 "alpha" :profile sensitive :top-k 3))
                        source))
               (matches (plist-get result :matches)))
          (should (eq (plist-get result :command) 'search))
          (should (equal (plist-get result :root) root-id))
          (should (equal (plist-get result :profile) 'sensitive))
          (should (equal (mapcar (lambda (match) (plist-get match :title)) matches)
                         '("Alpha" "Beta")))
          (should (> (plist-get (car matches) :score)
                     (plist-get (cadr matches) :score)))
          (should (stringp (plist-get result :index-generation)))
          (should (stringp (plist-get result :corpus-generation)))
          (should (equal (plist-get (plist-get result :snapshot) :head)
                         (dorg-head-commit dolt-dir)))
          ;; The title predicate is SQL-lowered into the postings query, so
          ;; candidate authorization/filtering happens before score/top-k.
          (let ((filtered (dorg-ql-search-snapshot
                           '(pipe (where (title "Beta"))
                                  (rank-bm25 "alpha" :profile sensitive :top-k 3))
                           source)))
            (should (equal (mapcar (lambda (match) (plist-get match :title))
                                    (plist-get filtered :matches))
                           '("Beta"))))
          ;; A final select projects only the already ranked, bounded ids; it
          ;; does not route the request through the ordinary unranked reader.
          (let ((projected (dorg-ql-search-snapshot
                            '(pipe (rank-bm25 "alpha" :profile sensitive :top-k 3)
                                   (select (field title) (property "K") (field level)))
                            source)))
            (should (equal '(("Alpha" "retained" 1) ("Beta" nil 1))
                           (mapcar (lambda (match)
                                     (mapcar #'cdr (plist-get match :values)))
                                   (plist-get projected :matches)))))
          ;; Profile identity is visible at the public route: literal
          ;; sensitive codepoints do not match the lower-case postings, while
          ;; the insensitive index reuses DOrg's standard-case canonicalizer.
          (should-not
           (plist-get
            (dorg-ql-search-snapshot
             '(pipe (rank-bm25 "ALPHA" :profile sensitive :top-k 3)) source)
            :matches))
          (should (equal (mapcar (lambda (match) (plist-get match :title))
                                 (plist-get
                                  (dorg-ql-search-snapshot
                                   '(pipe (rank-bm25 "ALPHA" :profile insensitive :top-k 3))
                                   source)
                                  :matches))
                         '("Alpha" "Beta")))
          ;; An all-OOV query still reads the manifest at a real snapshot;
          ;; it returns no documents rather than degrading to a broad scan,
          ;; even when a final select asks for a bounded hydration.
          (let ((oov (dorg-ql-search-snapshot
                      '(pipe (rank-bm25 "not-in-this-corpus" :profile sensitive :top-k 3)
                             (select (field title)))
                      source)))
            (should-not (plist-get oov :matches))
            (should (equal (plist-get oov :index-generation)
                           (plist-get result :index-generation)))
            (should (equal (plist-get oov :corpus-generation)
                           (plist-get result :corpus-generation)))))))))

(ert-deftest dorg-test-search-bm25-missing-derived-index-is-typed-unavailable ()
  "A missing lexical schema cannot fall back to canonical-content scanning."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
           (source (make-dorg-registration
                    :backend (dorg-backend-dolt dolt-dir root-id)))
           caught)
      (let ((dorg--extensions nil))
        (condition-case err
            (dorg-ql-search-snapshot
             '(pipe (rank-bm25 "Alpha" :profile sensitive :top-k 1)) source)
          (dorg-ql-error (setq caught err))))
      (should caught)
      (should (eq (dorg-error-reason caught) 'unsupported))
      (should (eq (plist-get (dorg-error-data caught) :detail)
                  'search-index-unavailable)))))

(ert-deftest dorg-test-search-bm25-ties-sort-by-canonical-heading-id ()
  "Equal rounded scores use heading id, never storage or collation order."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed
                       dolt-dir "* First\nneedle\n* Second\nneedle\n" 100))
             (source (make-dorg-registration
                      :backend (dorg-backend-dolt dolt-dir root-id))))
        (dorg-ext-rebuild-index
         'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
         :identity 'dorg :mode 'repair)
        (let ((matches (plist-get
                        (dorg-ql-search-snapshot
                         '(pipe (rank-bm25 "needle" :profile sensitive :top-k 2)) source)
                        :matches)))
          (should (= (plist-get (car matches) :score)
                     (plist-get (cadr matches) :score)))
          (should (apply #'< (mapcar (lambda (match) (plist-get match :heading-id))
                                     matches))))))))

(ert-deftest dorg-test-search-bm25-public-read-observes-delete-generation ()
  "A public ranked snapshot drops a deleted document at its new generation."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed
                       dolt-dir "* Keep\nneedle\n* Remove\nneedle\n" 100))
             (remove-id (dorg-test-mutate--id-by-title dolt-dir "Remove"))
             (source (make-dorg-registration
                      :backend (dorg-backend-dolt dolt-dir root-id))))
        (dorg-ext-rebuild-index
         'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
         :identity 'dorg :mode 'repair)
        (let ((before (dorg-ql-search-snapshot
                       '(pipe (rank-bm25 "needle" :profile sensitive :top-k 3)) source)))
          (should (equal '("Keep" "Remove")
                         (mapcar (lambda (match) (plist-get match :title))
                                 (plist-get before :matches))))
          (dorg-test-mutate--apply-patch-batch
           (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id root-id))
           (list (dorg-test-mutate--delete-patch remove-id)))
          (let ((after (dorg-ql-search-snapshot
                        '(pipe (rank-bm25 "needle" :profile sensitive :top-k 3)) source)))
            (should (equal '("Keep")
                           (mapcar (lambda (match) (plist-get match :title))
                                   (plist-get after :matches))))
            (should-not (equal (plist-get (plist-get before :snapshot) :head)
                               (plist-get (plist-get after :snapshot) :head)))
            (should-not (equal (plist-get before :corpus-generation)
                               (plist-get after :corpus-generation)))))))))

(ert-deftest dorg-test-search-bm25-rebuild-audits-shared-derived-image ()
  "Rebuild audit reproduces the mutation-maintained BM25 image exactly."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (heading-id (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root-id))))
        (dorg-ext-rebuild-index
         'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
         :identity 'dorg :mode 'repair)
        (dorg-test-mutate--apply-patch-batch
         source (list (dorg-test-mutate--heading-update-patch
                       heading-id :title "Beta")))
        (let ((audit (dorg-ext-rebuild-index
                      'bm25 root-id :dolt-dir dolt-dir
                      :revision (dorg-head-commit dolt-dir)
                      :identity 'dorg :mode 'audit)))
          (should (plist-get audit :ok))
          (let ((drift (plist-get audit :drift)))
            (should (= 0 (plist-get drift :missing-count)))
            (should (= 0 (plist-get drift :extra-count)))
            (should (= 0 (plist-get drift :differing-count)))))))))

(ert-deftest dorg-test-search-bm25-rebuild-audit-detects-corrupt-maintained-row ()
  "A derived-row deletion is visible to the shared exact-revision audit."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Alpha\n" 100))
             (heading-id (dorg-test-mutate--id-by-title dolt-dir "Alpha"))
             (source (dorg-mutate--resolve-source
                      (list :dolt-dir dolt-dir :root-id root-id))))
        (dorg-ext-rebuild-index
         'bm25 root-id :dolt-dir dolt-dir :revision (dorg-head-commit dolt-dir)
         :identity 'dorg :mode 'repair)
        (dorg-test-mutate--apply-patch-batch
         source (list (dorg-test-mutate--heading-update-patch
                       heading-id :title "Beta")))
        ;; This direct delete is the mutant: core content remains Beta, but
        ;; its required sensitive title posting is no longer materialized.
        (dorg-sql
         dolt-dir
         (format (concat "DELETE FROM dorg_bm25_postings WHERE root_id = %d "
                         "AND heading_id = %d AND profile = 'sensitive' "
                         "AND field_name = 'title' AND term = 'Beta'")
                 root-id heading-id))
        (dorg-commit dolt-dir "mutant delete derived BM25 posting")
        (let* ((audit (dorg-ext-rebuild-index
                       'bm25 root-id :dolt-dir dolt-dir
                       :revision (dorg-head-commit dolt-dir)
                       :identity 'dorg :mode 'audit))
               (drift (plist-get audit :drift)))
          (should (plist-get audit :ok))
          (should (= 1 (plist-get drift :missing-count)))
          (should (= 0 (plist-get drift :extra-count)))
          (should (= 0 (plist-get drift :differing-count)))
          (should (equal "Beta"
                         (dorg-test-bm25--scalar
                          dolt-dir
                          (format "SELECT title FROM headings WHERE id = %d"
                                  heading-id)))))))))

(ert-deftest dorg-test-search-bm25-derived-equivalence-mixed-lifecycle ()
  "One derivation agrees after insert/edit/delete/move/restore in real Dolt."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let ((lifecycle (dorg-test-bm25--run-mixed-lifecycle dolt-dir)))
        (dorg-test-bm25--assert-complete-audit dolt-dir lifecycle)
        (should-not (dorg-test-mutate--id-by-title dolt-dir "Delete Me"))
        (should-not (dorg-test-mutate--id-by-title dolt-dir "Delete Final"))
        (should (equal "Parent A edited"
                       (dorg-test-bm25--scalar
                        dolt-dir
                        (format "SELECT title FROM headings WHERE id = %d"
                                (plist-get lifecycle :parent-a)))))))))

(ert-deftest dorg-test-search-bm25-derived-equivalence-rejects-delete-delta-divergence ()
  "A delete projector that leaves df rows stale makes the mixed audit RED."
  (skip-unless (and dorg-test--dolt-available dorg-test--server-port))
  (dorg-test-mutate--with-dolt
    (let ((dorg--extensions nil))
      (dorg-bm25-register-extension)
      (dorg-migrate-schema dolt-dir)
      (dorg-commit dolt-dir "install bm25 extension")
      (let ((real-project (symbol-function 'dorg-bm25-project)))
        (let* ((lifecycle
                (dorg-test-bm25--run-mixed-lifecycle
                 dolt-dir
                 (lambda (source patches)
                   ;; The mutant changes only the mutation-time projection:
                   ;; on delete it retains the old term/df image.  Rebuild
                   ;; still invokes REAL-PROJECT, so the audit compares two
                   ;; genuinely different derivation paths.
                   (cl-letf
                       (((symbol-function 'dorg-bm25-project)
                         (lambda (context)
                           (let ((effects (funcall real-project context)))
                             (if (dorg-ext-index-context-deleted context)
                                 (cl-remove-if
                                  (lambda (effect)
                                    (equal "dorg_bm25_terms"
                                           (plist-get effect :table)))
                                  effects)
                               effects)))))
                     (dorg-test-mutate--apply-patch-batch source patches)))))
               (root-id (plist-get lifecycle :root-id))
               (audit (dorg-ext-rebuild-index
                       'bm25 root-id :dolt-dir dolt-dir
                       :revision (plist-get lifecycle :revision)
                       :identity 'dorg :mode 'audit))
               (drift (plist-get audit :drift)))
          (should (plist-get audit :ok))
          (should (> (plist-get drift :extra-count) 0))
          ;; `final' belongs only to the final deleted heading.  Its retained
          ;; sensitive-body and insensitive title/body rows show the failure
          ;; is a stale df/N delta, not setup refusal.
          (should (= 3 (string-to-number
                        (format "%s"
                                (dorg-test-bm25--scalar
                                 dolt-dir
                                 (format (concat "SELECT COUNT(*) FROM dorg_bm25_terms "
                                                 "WHERE root_id = %d "
                                                 "AND term IN ('final', 'FINAL')")
                                         root-id)))))))))))

;;; ─── Bounded maintenance ────────────────────────────────────────────────
;;
;; An ordinary edit maintains the lexical index from the changed headings
;; only: heading-local rows are diffed, and document frequencies, corpus
;; totals and the corpus generation are folded from keyed priors read at the
;; plan base.  These specs pin the scaling, the unbuilt-root behaviour, the
;; revision-pinned priors and rebuild equality.

(defmacro dorg-test-bm25--with-installed (&rest body)
  "Run BODY in a fresh Dolt store with the BM25 extension installed."
  (declare (indent 0))
  `(progn
     (unless (and dorg-test--dolt-available dorg-test--server-port)
       (ert-skip "dolt sql-server unavailable"))
     (dorg-test-mutate--with-dolt
       (let ((dorg--extensions nil))
         (dorg-bm25-register-extension)
         (dorg-migrate-schema dolt-dir)
         (dorg-commit dolt-dir "install bm25 extension")
         ,@body))))

(defun dorg-test-bm25--corpus (count)
  "Return COUNT titled documents with bodies, as Org text."
  (mapconcat (lambda (i) (format "* Doc %d meridian\nbody %d harbor quartz\n" i i))
             (number-sequence 1 count) ""))

(defun dorg-test-bm25--build (dolt-dir root-id)
  "Rebuild ROOT-ID's lexical index at HEAD of DOLT-DIR."
  (dorg-ext-rebuild-index 'bm25 root-id :dolt-dir dolt-dir
                          :revision (dorg-head-commit dolt-dir)
                          :identity 'dorg :mode 'repair))

(defun dorg-test-bm25--source (dolt-dir root-id)
  "Return the mutation source for ROOT-ID in DOLT-DIR."
  (dorg-mutate--resolve-source (list :dolt-dir dolt-dir :root-id root-id)))

(defun dorg-test-bm25--audit-clean (dolt-dir root-id)
  "Assert ROOT-ID's maintained index equals a rebuild at HEAD."
  (dorg-test-bm25--assert-complete-audit
   dolt-dir (list :root-id root-id :revision (dorg-head-commit dolt-dir))))

(defun dorg-test-bm25--root-rows (dolt-dir root-id)
  "Return ROOT-ID's row counts in the four derived tables."
  (mapcar (lambda (table)
            (string-to-number
             (format "%s" (dorg-test-bm25--scalar
                           dolt-dir (format "SELECT COUNT(*) FROM %s WHERE root_id = %d"
                                            table root-id)))))
          '("dorg_bm25_documents" "dorg_bm25_postings" "dorg_bm25_terms"
            "dorg_bm25_manifest")))

(defun dorg-test-bm25--edit-cost (count)
  "Return (TOKENIZE-CALLS FETCHED WHOLE-ROOT-LOADS) for one title edit.
The store holds COUNT documents with a built index."
  (let (result)
    (dorg-test-bm25--with-installed
      (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus count) 100))
             (target (dorg-test-mutate--id-by-title dolt-dir "Doc 1 meridian"))
             (tokenize 0) (fetched 0) (whole 0)
             (orig-tokenize (symbol-function 'dorg-bm25-tokenize))
             (orig-load (symbol-function 'dorg-model-load-headings))
             (orig-tree (symbol-function 'dorg-model-load-tree)))
        (dorg-test-bm25--build dolt-dir root-id)
        (cl-letf (((symbol-function 'dorg-bm25-tokenize)
                   (lambda (&rest args) (cl-incf tokenize) (apply orig-tokenize args)))
                  ((symbol-function 'dorg-model-load-headings)
                   (lambda (&rest args)
                     (let ((value (apply orig-load args)))
                       (cl-incf fetched (hash-table-count value))
                       value)))
                  ((symbol-function 'dorg-model-load-tree)
                   (lambda (&rest args) (cl-incf whole) (apply orig-tree args))))
          (dorg-test-mutate--apply-patch-batch
           (dorg-test-bm25--source dolt-dir root-id)
           (list (dorg-test-mutate--heading-update-patch target :title "Doc 1 revised"))))
        (dorg-test-bm25--audit-clean dolt-dir root-id)
        (setq result (list tokenize fetched whole))))
    result))

(ert-deftest dorg-test-search-bm25-title-edit-cost-is-independent-of-corpus ()
  "A title edit tokenizes only the edited document and fetches a bounded slice.
The old and new document are each tokenized once per profile and field:
eight calls at 20 or 80 documents alike.  The heading fetch is the same small
slice at both sizes, and no whole-root tree is loaded; the result still
equals a rebuild."
  (let ((small (dorg-test-bm25--edit-cost 20))
        (large (dorg-test-bm25--edit-cost 80)))
    (should (equal small large))
    (should (= 8 (nth 0 small)))
    (should (= 0 (nth 2 small)))
    (should (< (nth 1 small) 10))))

(ert-deftest dorg-test-search-bm25-unbuilt-root-edits-keep-the-index-absent ()
  "An installed but never built root accepts edits and keeps no partial index.
Search names the rebuild instead of returning empty results; after a rebuild
the next edit is maintained and equals a rebuild."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 2 meridian"))
           (registration (make-dorg-registration
                          :backend (dorg-backend-dolt dolt-dir root-id)))
           (search (lambda ()
                     (dorg-ql-search-snapshot
                      '(pipe (rank-bm25 "meridian" :profile sensitive :top-k 5))
                      registration))))
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--heading-update-patch target :title "Doc 2 lattice")))
      (should (equal "Doc 2 lattice"
                     (dorg-test-bm25--scalar
                      dolt-dir (format "SELECT title FROM headings WHERE id = %d" target))))
      (should (equal '(0 0 0 0) (dorg-test-bm25--root-rows dolt-dir root-id)))
      (let ((err (should-error (funcall search) :type 'dorg-ql-error)))
        (should (eq (plist-get (dorg-error-data err) :detail) 'derived-index-unbuilt))
        (should (string-match-p "dorg-ext-rebuild-index" (error-message-string err))))
      (dorg-test-bm25--build dolt-dir root-id)
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--heading-update-patch target :title "Doc 2 vellum")))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      ;; "Doc 1 meridian" and "Doc 3 meridian" remain.
      (should (= 2 (length (plist-get (funcall search) :matches)))))))

(ert-deftest dorg-test-search-bm25-empty-root-first-edit-births-the-index ()
  "A root with no headings at its base births its index on the first edit."
  (dorg-test-bm25--with-installed
    (let ((root-id (dorg-test-mutate--seed dolt-dir "" 100)))
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--tree-insert-patch :root '(:id 900 :title "First orchid"))))
      (should (equal '(2 4 4 2) (dorg-test-bm25--root-rows dolt-dir root-id)))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-new-term-births-and-last-use-prunes ()
  "A first occurrence on a built root births its df row; the last use prunes it."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 3 meridian"))
           (df (lambda (term)
                 (dorg-test-bm25--scalar
                  dolt-dir
                  (format (concat "SELECT document_frequency FROM dorg_bm25_terms"
                                  " WHERE root_id = %d AND profile = 'sensitive'"
                                  " AND field_name = 'title' AND term = %s")
                          root-id (dorg-sql-quote term))))))
      (dorg-test-bm25--build dolt-dir root-id)
      (should-not (funcall df "zebra"))
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--heading-update-patch target :title "Doc 3 zebra")))
      (should (equal 1 (funcall df "zebra")))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--heading-update-patch target :title "Doc 3 meridian")))
      (should-not (funcall df "zebra"))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(defun dorg-test-bm25--collected-effects (thunk)
  "Run THUNK; return every effect the index projections collected, in order."
  (let ((orig (symbol-function 'dorg-ext-collect-index-projection))
        collected)
    (cl-letf (((symbol-function 'dorg-ext-collect-index-projection)
               (lambda (&rest args)
                 (let ((result (apply orig args)))
                   (setq collected (append collected (plist-get result :effects)))
                   result))))
      (funcall thunk))
    collected))

(defun dorg-test-bm25--effect-term (effect)
  "Return (PROFILE FIELD TERM SHAPE) of a terms EFFECT; SHAPE is its kind."
  (let ((row (or (plist-get effect :insert) (plist-get effect :delete))))
    (list (alist-get 'profile row) (alist-get 'field_name row) (alist-get 'term row)
          (if (plist-member effect :insert) :insert :delete))))

(ert-deftest dorg-test-search-bm25-new-aggregate-keys-fold-to-inserts ()
  "A key with no stored aggregate row folds to one insert, never a delete.
A bootstrap import births every term and manifest row, so it collects no
delete on either aggregate table and one term insert per stored term row.
A maintained edit that introduces a term and drops one use of a shared
term collects a lone insert for the new key and a delete plus an insert
for the shared one.  The index equals a rebuild after each step, and after
a replace import."
  (dorg-test-bm25--with-installed
    (let* ((directory (make-temp-file "dorg-bm25-shape-" t))
           (text (concat "* Doc A\n:PROPERTIES:\n:DORG_ID: 7101\n:END:\nalpha harbor\n"
                         "* Doc B\n:PROPERTIES:\n:DORG_ID: 7102\n:END:\nharbor quartz\n"))
           (aggregate-p (lambda (effect)
                          (member (plist-get effect :table)
                                  '("dorg_bm25_terms" "dorg_bm25_manifest"))))
           root-id)
      (let ((boot (dorg-test-bm25--collected-effects
                   (lambda ()
                     (setq root-id (dorg-test-bm25--import dolt-dir directory text 'bootstrap))))))
        (should (cl-some aggregate-p boot))
        (should-not (cl-remove-if-not (lambda (e) (and (funcall aggregate-p e)
                                                       (plist-member e :delete)))
                                      boot))
        (should (= (cl-count-if (lambda (e) (and (equal "dorg_bm25_terms" (plist-get e :table))
                                                 (plist-member e :insert)))
                                boot)
                   (string-to-number
                    (format "%s" (dorg-test-bm25--scalar
                                  dolt-dir (format "SELECT COUNT(*) FROM dorg_bm25_terms WHERE root_id = %d"
                                                   root-id)))))))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      (let* ((edit (dorg-test-bm25--collected-effects
                    (lambda ()
                      (dorg-test-mutate--apply-patch-batch
                       (dorg-test-bm25--source dolt-dir root-id)
                       (list (dorg-test-mutate--heading-update-patch 7101 :title "A zebra"))))))
             (shapes (lambda (term)
                       (mapcar #'cadddr
                               (cl-remove-if-not
                                (lambda (cell) (equal (butlast cell) (list "sensitive" "title" term)))
                                (mapcar #'dorg-test-bm25--effect-term
                                        (cl-remove-if-not (lambda (e) (equal "dorg_bm25_terms"
                                                                             (plist-get e :table)))
                                                          edit)))))))
        (should (equal '(:insert) (funcall shapes "zebra")))
        (should (equal '(:delete :insert) (funcall shapes "Doc")))
        (should-not (funcall shapes "A")))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      (dorg-test-bm25--import
       dolt-dir directory
       (concat "* Doc A zebra\n:PROPERTIES:\n:DORG_ID: 7101\n:END:\nalpha lantern\n"
               "* Doc C\n:PROPERTIES:\n:DORG_ID: 7103\n:END:\nquartz\n")
       'replace (dorg-head-commit dolt-dir))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-missing-prior-of-a-stored-row-fails-closed ()
  "A prior read that misses a stored aggregate row cannot overwrite it.
With the stored row of an existing title term dropped from the priors, the
fold sees that key as new and births it as an insert, which collides with
the stored row on the table's key.  The intended collision fails the edit
as a typed constraint violation: HEAD, the canonical heading and every
derived row are unchanged.  The same edit with the true priors then
succeeds, so the dropped prior is the cause."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed
                     dolt-dir "* Alpha shared\nx\n* Beta\ny\n* Gamma shared\nz\n" 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Beta"))
           (orig (symbol-function 'dorg-mutate--read-accumulator-priors))
           (dropped 0))
      (dorg-test-bm25--build dolt-dir root-id)
      (let ((head (dorg-head-commit dolt-dir))
            (rows (dorg-test-bm25--root-rows dolt-dir root-id))
            (stored (lambda ()
                      (dorg-test-bm25--scalar
                       dolt-dir
                       (format (concat "SELECT document_frequency FROM dorg_bm25_terms"
                                       " WHERE root_id = %d AND profile = 'sensitive'"
                                       " AND field_name = 'title' AND term = 'shared'")
                               root-id)))))
        (should (equal 2 (funcall stored)))
        (cl-letf (((symbol-function 'dorg-mutate--read-accumulator-priors)
                   (lambda (&rest args)
                     (cl-remove-if (lambda (prior)
                                     (let ((row (cdr prior)))
                                       (and (equal (car prior) "dorg_bm25_terms")
                                            (equal (alist-get 'profile row) "sensitive")
                                            (equal (alist-get 'field_name row) "title")
                                            (equal (alist-get 'term row) "shared")
                                            (cl-incf dropped))))
                                   (apply orig args)))))
          (let ((err (should-error
                      (dorg-test-mutate--apply-patch-batch
                       (dorg-test-bm25--source dolt-dir root-id)
                       (list (dorg-test-mutate--heading-update-patch target :title "Beta shared"))))))
            (should (eq 'invariant (plist-get (dorg-error-data err) :reason)))
            (should (eq 'constraint (plist-get (dorg-error-data err) :detail)))))
        (should (> dropped 0))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (dorg-test-mutate--id-by-title dolt-dir "Beta"))
        (should (equal rows (dorg-test-bm25--root-rows dolt-dir root-id)))
        (should (equal 2 (funcall stored)))
        (dorg-test-bm25--audit-clean dolt-dir root-id)
        (dorg-test-mutate--apply-patch-batch
         (dorg-test-bm25--source dolt-dir root-id)
         (list (dorg-test-mutate--heading-update-patch target :title "Beta shared")))
        (should (equal 3 (funcall stored)))
        (dorg-test-bm25--audit-clean dolt-dir root-id)))))

(ert-deftest dorg-test-search-bm25-priors-are-read-at-the-plan-base ()
  "Aggregate priors are keyed revision reads at the plan base, never live reads."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 4) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 4 meridian"))
           (orig-live (symbol-function 'dorg-sql))
           (orig-pinned (symbol-function 'dorg-sql--at-revision))
           pinned live base)
      (dorg-test-bm25--build dolt-dir root-id)
      (setq base (dorg-head-commit dolt-dir))
      (cl-letf (((symbol-function 'dorg-sql)
                 (lambda (dir query &rest args)
                   (let ((sql (if args (apply #'format query args) query)))
                     (when (string-match-p "FROM dorg_bm25_\\(terms\\|manifest\\)" sql)
                       (push sql live)))
                   (apply orig-live dir query args)))
                ((symbol-function 'dorg-sql--at-revision)
                 (lambda (dir revision query &rest args)
                   (let ((sql (if args (apply #'format query args) query)))
                     (when (string-match-p "FROM dorg_bm25_\\(terms\\|manifest\\)" sql)
                       (push (cons revision sql) pinned)))
                   (apply orig-pinned dir revision query args))))
        (dorg-test-mutate--apply-patch-batch
         (dorg-test-bm25--source dolt-dir root-id)
         (list (dorg-test-mutate--heading-update-patch target :title "Doc 4 cobalt"))))
      (should-not live)
      (should pinned)
      (should (cl-every (lambda (read) (equal (car read) base)) pinned))
      ;; Keyed: the term read names the touched terms, never the whole table.
      (should (cl-every (lambda (read)
                          (string-match-p "root_id = \\|root_id IN" (cdr read)))
                        pinned))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-stale-prior-replans-under-the-commit-guard ()
  "A commit landing between the prior read and the commit forces a replan.
The injected commit edits another document's terms, so a plan committed
with its stale priors would leave aggregates that differ from a rebuild."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 4) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 1 meridian"))
           (other (dorg-test-mutate--id-by-title dolt-dir "Doc 2 meridian"))
           (orig (symbol-function 'dorg-mutate--read-accumulator-priors))
           (injected 0))
      (dorg-test-bm25--build dolt-dir root-id)
      (cl-letf (((symbol-function 'dorg-mutate--read-accumulator-priors)
                 (lambda (&rest args)
                   (prog1 (apply orig args)
                     (when (= 0 injected)
                       (cl-incf injected)
                       ;; An ordinary maintained edit of another document
                       ;; commits while this plan holds base-B priors.
                       (dorg-test-mutate--apply-patch-batch
                        (dorg-test-bm25--source dolt-dir root-id)
                        (list (dorg-test-mutate--heading-update-patch
                               other :title "Doc 2 cobalt"))))))))
        (dorg-test-mutate--apply-patch-batch
         (dorg-test-bm25--source dolt-dir root-id)
         (list (dorg-test-mutate--heading-update-patch target :title "Doc 1 cobalt"))))
      (should (= 1 injected))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-generation-is-order-free-content-identity ()
  "The corpus generation is the xor of per-document digests.
It equals the rebuild value, ignores a pure in-root move, and changes on a
create, an edit and a delete."
  (should (equal (dorg-bm25-document-digest 7 "T" "B")
                 (secure-hash 'sha256 "dorg-bm25-corpus-v3\0\x37\0\x31:T1:B")))
  (should (equal (dorg-bm25-document-digest 7 nil "")
                 (secure-hash 'sha256 "dorg-bm25-corpus-v3\0\x37\0\x30:0:")))
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed
                     dolt-dir "* Parent A\nalpha\n** Child\nchild body\n* Parent B\nbeta\n" 100))
           (child (dorg-test-mutate--id-by-title dolt-dir "Child"))
           (parent-b (dorg-test-mutate--id-by-title dolt-dir "Parent B"))
           (source (dorg-test-bm25--source dolt-dir root-id))
           (generation (lambda ()
                         (dorg-test-bm25--scalar
                          dolt-dir (format (concat "SELECT corpus_generation FROM dorg_bm25_manifest"
                                                   " WHERE root_id = %d AND profile = 'sensitive'")
                                           root-id))))
           seen)
      (dorg-test-bm25--build dolt-dir root-id)
      (push (funcall generation) seen)
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (car seen)))
      (dorg-test-mutate--apply-patch-batch
       source (list (dorg-test-mutate--move-patch
                     child (dorg-test-mutate--heading-address parent-b))))
      (should (equal (car seen) (funcall generation)))
      (dolist (patch (list (dorg-test-mutate--tree-insert-patch :root '(:id 901 :title "New"))
                           (dorg-test-mutate--heading-update-patch child :title "Child edited")
                           (dorg-test-mutate--delete-patch 901)))
        (dorg-test-mutate--apply-patch-batch source (list patch))
        (should-not (member (funcall generation) seen))
        (push (funcall generation) seen)
        (dorg-test-bm25--audit-clean dolt-dir root-id)))))

(ert-deftest dorg-test-search-bm25-v2-index-migrates-to-unbuilt ()
  "Migrating a populated version-2 index removes its derived rows.
Edits then proceed with the index absent, search names the rebuild, and the
rebuild restores an index equal to a fresh derivation."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 1 meridian"))
           (registration (make-dorg-registration
                          :backend (dorg-backend-dolt dolt-dir root-id))))
      (dorg-test-bm25--build dolt-dir root-id)
      ;; Recreate the version-2 shape over the populated index.
      (dorg-sql-batch
       dolt-dir
       (list "ALTER TABLE dorg_bm25_manifest ADD COLUMN index_generation VARCHAR(255) NOT NULL DEFAULT 'v2'"
             "UPDATE dorg_meta SET value = '2' WHERE `key` = 'ext:bm25:version'"))
      (dorg-commit dolt-dir "simulate version 2")
      (dorg-migrate-schema dolt-dir)
      (should (equal "3" (format "%s" (dorg-test-bm25--scalar
                                       dolt-dir "SELECT value FROM dorg_meta WHERE `key` = 'ext:bm25:version'"))))
      (should-not (dorg-test-bm25--scalar
                   dolt-dir (concat "SELECT column_name FROM information_schema.columns"
                                    " WHERE table_name = 'dorg_bm25_manifest'"
                                    " AND column_name = 'index_generation'")))
      (should (equal '(0 0 0 0) (dorg-test-bm25--root-rows dolt-dir root-id)))
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--heading-update-patch target :title "Doc 1 cobalt")))
      (should (equal '(0 0 0 0) (dorg-test-bm25--root-rows dolt-dir root-id)))
      (should (eq 'derived-index-unbuilt
                  (plist-get (dorg-error-data
                              (should-error
                               (dorg-ql-search-snapshot
                                '(pipe (rank-bm25 "cobalt" :profile sensitive :top-k 5))
                                registration)
                               :type 'dorg-ql-error))
                             :detail)))
      (dorg-test-bm25--build dolt-dir root-id)
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-dry-run-shows-the-folded-effects ()
  "A dry-run plan carries the literal folded SQL an apply then commits."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 1 meridian"))
           (patches (list (dorg-test-mutate--heading-update-patch
                           target :title "Doc 1 cobalt")))
           (orig (symbol-function 'dorg-sql--lease-batch))
           committed)
      (dorg-test-bm25--build dolt-dir root-id)
      (let* ((plan (dorg-mutate--plan-patches (dorg-test-bm25--source dolt-dir root-id)
                                              patches))
             (planned (delq nil (mapcar (lambda (op) (plist-get op :ext-sql))
                                        (plist-get plan :db-ops)))))
        (should planned)
        (should (cl-some (lambda (sql)
                           (string-match-p "INSERT INTO dorg_bm25_manifest" sql))
                         planned))
        (should-not (cl-some (lambda (sql) (string-match-p "ON DUPLICATE\\|+ *[0-9]" sql))
                             planned))
        (cl-letf (((symbol-function 'dorg-sql--lease-batch)
                   (lambda (lease statements &rest args)
                     (setq committed statements)
                     (apply orig lease statements args))))
          (dorg-test-mutate--apply-patch-batch
           (dorg-test-bm25--source dolt-dir root-id) patches))
        (should (cl-every (lambda (sql) (member sql committed)) planned))
        (dorg-test-bm25--audit-clean dolt-dir root-id)))))

(ert-deftest dorg-test-search-bm25-fold-failure-rolls-the-edit-back ()
  "A fold that would drive a count below zero aborts the whole mutation.
A committed df of zero is impossible in a maintained index.  Planting one
makes deleting a document fold below zero: the edit fails closed, and HEAD,
the canonical heading and the derived rows are all unchanged."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 2 meridian")))
      (dorg-test-bm25--build dolt-dir root-id)
      (dorg-sql-batch
       dolt-dir
       (list (format (concat "UPDATE dorg_bm25_terms SET document_frequency = 0"
                             " WHERE root_id = %d AND profile = 'sensitive'"
                             " AND field_name = 'title' AND term = 'meridian'")
                     root-id))
       "plant an impossible count")
      (let ((head (dorg-head-commit dolt-dir))
            (rows (dorg-test-bm25--root-rows dolt-dir root-id)))
        (should-error
         (dorg-test-mutate--apply-patch-batch
          (dorg-test-bm25--source dolt-dir root-id)
          (list (dorg-test-mutate--delete-patch target))))
        (should (equal head (dorg-head-commit dolt-dir)))
        (should (dorg-test-mutate--id-by-title dolt-dir "Doc 2 meridian"))
        (should (equal rows (dorg-test-bm25--root-rows dolt-dir root-id)))))))

(ert-deftest dorg-test-search-bm25-partly-built-root-refuses-until-rebuilt ()
  "A root holding only some of its required manifest rows refuses edits.
The refusal names the rebuild; after it the same edit is maintained and
equals a rebuild."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 3) 100))
           (target (dorg-test-mutate--id-by-title dolt-dir "Doc 1 meridian"))
           (edit (lambda ()
                   (dorg-test-mutate--apply-patch-batch
                    (dorg-test-bm25--source dolt-dir root-id)
                    (list (dorg-test-mutate--heading-update-patch
                           target :title "Doc 1 cobalt"))))))
      (dorg-test-bm25--build dolt-dir root-id)
      (dorg-sql-batch
       dolt-dir
       (list (format "DELETE FROM dorg_bm25_manifest WHERE root_id = %d AND profile = 'insensitive'"
                     root-id))
       "lose one profile's manifest")
      (let* ((head (dorg-head-commit dolt-dir))
             (err (should-error (funcall edit))))
        (should (string-match-p "partly built" (error-message-string err)))
        (should (string-match-p "dorg-ext-rebuild-index" (error-message-string err)))
        (should (equal head (dorg-head-commit dolt-dir))))
      (dorg-test-bm25--build dolt-dir root-id)
      (funcall edit)
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-cross-root-stale-prior-replans ()
  "A cross-root move whose priors go stale before commit replans.
An ordinary maintained edit of the source root commits after the move read
its priors; both roots still equal a rebuild afterwards."
  (dorg-test-bm25--with-installed
    (let* ((ids (dorg-test-mutate--cross-root-seed dolt-dir))
           (src (plist-get ids :src-root))
           (dst (plist-get ids :dst-root))
           (orig (symbol-function 'dorg-mutate--read-accumulator-priors))
           (injected 0))
      (dolist (root (list src dst)) (dorg-test-bm25--build dolt-dir root))
      (cl-letf (((symbol-function 'dorg-mutate--read-accumulator-priors)
                 (lambda (&rest args)
                   (prog1 (apply orig args)
                     (when (= 0 injected)
                       (cl-incf injected)
                       (dorg-test-mutate--apply-patch-batch
                        (dorg-test-bm25--source dolt-dir src)
                        (list (dorg-test-mutate--heading-update-patch
                               (plist-get ids :src-sib) :title "SrcSib harbor"))))))))
        (dorg-test-mutate--apply-patch-batch
         (dorg-test-bm25--source dolt-dir src)
         (list (dorg-test-mutate--cross-root-move-patch
                (plist-get ids :movable)
                (dorg-test-mutate--heading-address (plist-get ids :dst-parent))
                dolt-dir dst))))
      (should (= 1 injected))
      (dolist (root (list src dst)) (dorg-test-bm25--audit-clean dolt-dir root)))))

(defun dorg-test-bm25--import (dolt-dir directory text mode &optional base)
  "Import TEXT as DIRECTORY's corpus.org into DOLT-DIR in MODE; return the root."
  (let ((source (expand-file-name (format "%s-source.org" mode) directory))
        (target (expand-file-name "corpus.org" directory)))
    (with-temp-file source (insert text))
    (plist-get (apply #'dorg-import-document source :dolt-dir dolt-dir
                      :target-file target :mode mode
                      (and base (list :base base)))
               :root-id)))

(ert-deftest dorg-test-search-bm25-import-replace-maintains-and-guards-priors ()
  "Import replace maintains the index, and a stale prior refuses the import.
The replacement folds against the index at its base and equals a rebuild.
When an ordinary maintained edit commits after the import read its priors,
the import's HEAD = base commit refuses, and the store still equals a rebuild."
  (dorg-test-bm25--with-installed
    (let* ((directory (make-temp-file "dorg-bm25-import-" t))
           (root-id (dorg-test-bm25--import
                     dolt-dir directory
                     "* Doc A\n:PROPERTIES:\n:DORG_ID: 7001\n:END:\nalpha harbor\n* Doc B\n:PROPERTIES:\n:DORG_ID: 7002\n:END:\nbeta\n"
                     'bootstrap)))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      (dorg-test-bm25--import
       dolt-dir directory
       "* Doc A revised\n:PROPERTIES:\n:DORG_ID: 7001\n:END:\nalpha quartz\n* Doc C\n:PROPERTIES:\n:DORG_ID: 7003\n:END:\ngamma\n"
       'replace (dorg-head-commit dolt-dir))
      (dorg-test-bm25--audit-clean dolt-dir root-id)
      (let ((orig (symbol-function 'dorg-mutate--read-accumulator-priors))
            (injected 0))
        (cl-letf (((symbol-function 'dorg-mutate--read-accumulator-priors)
                   (lambda (&rest args)
                     (prog1 (apply orig args)
                       (when (= 0 injected)
                         (cl-incf injected)
                         (dorg-test-mutate--apply-patch-batch
                          (dorg-test-bm25--source dolt-dir root-id)
                          (list (dorg-test-mutate--heading-update-patch
                                 7003 :title "Doc C edited"))))))))
          (should-error
           (dorg-test-bm25--import
            dolt-dir directory
            "* Doc A again\n:PROPERTIES:\n:DORG_ID: 7001\n:END:\nalpha\n* Doc C\n:PROPERTIES:\n:DORG_ID: 7003\n:END:\ngamma\n"
            'replace (dorg-head-commit dolt-dir))))
        (should (= 1 injected)))
      (dorg-test-bm25--audit-clean dolt-dir root-id))))

(ert-deftest dorg-test-search-bm25-policy-upgrade-refuses-accumulators ()
  "A policy-upgrade producer cannot emit an accumulator effect.
Policy producers describe complete inserted rows; an accumulator is refused
before any row validation."
  (let ((dorg--extensions nil))
    (dorg-bm25-register-extension)
    (let ((err (should-error
                (dorg-ext-rebuild--policy-projection
                 (dorg-get-extension 'bm25)
                 (list :project
                       (lambda (_context)
                         '((:table "dorg_bm25_terms"
                            :accumulate ((root_id . 1) (profile . "sensitive")
                                         (field_name . "title") (term . "a"))
                            :add ((document_frequency . 1))))))
                 nil)
                :type 'dorg-ext-rebuild-error)))
      (should (eq 'producer-projection-malformed
                  (plist-get (dorg-error-data err) :reason))))))

(ert-deftest dorg-test-search-bm25-random-edit-sequence-equals-rebuild ()
  "A seeded random sequence of edits keeps the index equal to a rebuild.
Titles, bodies, inserts, deletes and moves draw on a small vocabulary so terms
appear, recur and vanish; the audit runs after every committed edit."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir (dorg-test-bm25--corpus 6) 100))
           (source (dorg-test-bm25--source dolt-dir root-id))
           (words ["orchid" "harbor" "quartz" "meridian" "ember" "zephyr"])
           (state 7)
           (next-id 9100)
           (applied 0)
           (live (lambda ()
                   (mapcar (lambda (row) (alist-get 'id row))
                           (dorg-sql dolt-dir
                                     (format (concat "SELECT id FROM headings WHERE root_id = %d"
                                                     " AND id <> %d ORDER BY id")
                                             root-id root-id))))))
      (cl-flet ((pick (n) (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
                  (mod (/ state 65536) n)))
        (dorg-test-bm25--build dolt-dir root-id)
        (dotimes (_step 12)
          (let* ((ids (funcall live))
                 (id (nth (pick (length ids)) ids))
                 (w (lambda () (aref words (pick (length words)))))
                 (patch
                  (pcase (pick 5)
                    (0 (dorg-test-mutate--heading-update-patch
                        id :title (format "%s %s" (funcall w) (funcall w))))
                    (1 (dorg-test-mutate--text-append-patch id (funcall w)))
                    (2 (dorg-test-mutate--tree-insert-patch
                        :root (list :id (cl-incf next-id)
                                    :title (format "new %s" (funcall w)))))
                    (3 (if (> (length ids) 3)
                           (dorg-test-mutate--delete-patch id)
                         (dorg-test-mutate--text-append-patch id (funcall w))))
                    (_ (let ((under (nth (pick (length ids)) ids)))
                         (if (eql under id)
                             (dorg-test-mutate--text-append-patch id (funcall w))
                           (dorg-test-mutate--move-patch
                            id (dorg-test-mutate--heading-address under))))))))
            (condition-case nil
                (progn (dorg-test-mutate--apply-patch-batch source (list patch))
                       (cl-incf applied))
              ;; A move into its own subtree is refused by the core; skip it.
              (dorg-mutate-error nil))
            (dorg-test-bm25--audit-clean dolt-dir root-id)))
        ;; The sequence is not vacuous: most of its edits committed.
        (should (>= applied 8))))))

(ert-deftest dorg-test-search-bm25-unbuilt-root-emptiness-is-a-base-fact ()
  "Deleting the last heading of an unbuilt root keeps its index absent.
The root had a heading at the plan base, so it is unbuilt rather than empty:
nothing is born even though the candidate holds no document."
  (dorg-test-bm25--with-installed
    (let* ((root-id (dorg-test-mutate--seed dolt-dir "* Only orchid\nbody\n" 100))
           (only (dorg-test-mutate--id-by-title dolt-dir "Only orchid")))
      (dorg-test-mutate--apply-patch-batch
       (dorg-test-bm25--source dolt-dir root-id)
       (list (dorg-test-mutate--delete-patch only)))
      (should-not (dorg-test-mutate--id-by-title dolt-dir "Only orchid"))
      (should (equal '(0 0 0 0) (dorg-test-bm25--root-rows dolt-dir root-id))))))

;;; ─── Multi-row insert lowering: BM25 order and mutation batches ──────────

(defconst dorg-test-bm25--ib-dir
  (expand-file-name "fixtures/insert-batching/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The insert-batching base oracle and reference model.")

(require 'dorg-insert-batching-capture
         (expand-file-name "capture" dorg-test-bm25--ib-dir))
(require 'dorg-insert-batching-reference
         (expand-file-name "reference" dorg-test-bm25--ib-dir))

(defun dorg-test-bm25--ib-candidate ()
  "Return the candidate's normalized observation of every oracle scenario."
  (unless (and dorg-test--dolt-available dorg-test--server-port)
    (ert-skip "dolt sql-server unavailable"))
  (dorg-test--with-temp-db dolt-dir
    (let ((dir (make-temp-file "dorg-ib-" t)))
      (unwind-protect (dorg-ib-scenarios dolt-dir dir)
        (delete-directory dir t)))))

(defun dorg-test-bm25--ib-golden (key)
  "Return the frozen base observation of scenario KEY."
  (cdr (assq key (plist-get (dorg-ib-read-golden) :scenarios))))

(defun dorg-test-bm25--ib-partitioned (effects)
  "Return base EFFECTS in the frozen BM25 order.
Document and posting deletes keep their order, then document inserts, then
posting inserts, then every folded accumulator effect in its order."
  (let* ((block-p (lambda (e) (member (plist-get e :table)
                                      '("dorg_bm25_documents" "dorg_bm25_postings"))))
         (insert-on (lambda (table)
                      (cl-remove-if-not (lambda (e) (and (equal table (plist-get e :table))
                                                         (plist-member e :insert)))
                                        effects)))
         (block-end (or (cl-position-if-not block-p effects) (length effects))))
    ;; The base projector emits one block of document/posting effects first.
    (should-not (cl-position-if block-p effects :start block-end))
    (append (cl-remove-if-not (lambda (e) (and (funcall block-p e)
                                               (plist-member e :delete)))
                              effects)
            (funcall insert-on "dorg_bm25_documents")
            (funcall insert-on "dorg_bm25_postings")
            (nthcdr block-end effects))))

(ert-deftest dorg-test-search-bm25-projection-partitions-inserts-by-table ()
  "BM25 emits its deletes, then all document inserts, then all posting inserts.
For every import scenario the effect list equals the frozen base list with
the document/posting inserts stably partitioned by table.  Every other effect
keeps its content and order, including the folded accumulators.  The base
interleaves documents and postings per heading and fails."
  (let ((candidate (dorg-test-bm25--ib-candidate)))
    (dolist (key '(:bootstrap :replace :second-bootstrap))
      (let ((golden (car (plist-get (dorg-test-bm25--ib-golden key) :projections)))
            (actual (car (plist-get (cdr (assq key candidate)) :projections))))
        (should (equal (dorg-test-bm25--ib-partitioned (plist-get golden :effects))
                       (plist-get actual :effects)))
        (should (equal (plist-get golden :extensions) (plist-get actual :extensions)))))))

(ert-deftest dorg-test-search-bm25-mutation-batches-equal-reference-model ()
  "Mutations keep scalar core SQL and send the lowered BM25 block, exactly.
For an ordinary edit, an in-root move and a cross-root move:
- the applied batch equals the reference model built from the frozen base
  batch: core statements byte-identical and unmoved, the BM25 block
  partitioned and lowered;
- the dry-run plan equals the base in every field, except that its
  extension SQL is that same lowered block, which the apply then commits;
- the generated ids are the base's."
  (let ((candidate (dorg-test-bm25--ib-candidate)))
    (dolist (key '(:mutate-ordinary :mutate-move :mutate-cross-root))
      (let* ((golden (dorg-test-bm25--ib-golden key))
             (actual (cdr (assq key candidate)))
             (base-batch (car (plist-get (plist-get golden :apply) :lease-batches)))
             (expected (dorg-ibr-expected-mutation base-batch))
             (gplan (plist-get (plist-get golden :plan) :value))
             (aplan (plist-get (plist-get actual :plan) :value))
             (ext-of (lambda (plan)
                       (delq nil (mapcar (lambda (op) (plist-get op :ext-sql))
                                         (plist-get plan :db-ops)))))
             (core-of (lambda (plan)
                        (cl-remove-if (lambda (op) (plist-get op :ext-sql))
                                      (plist-get plan :db-ops)))))
        (should (equal (list expected)
                       (plist-get (plist-get actual :apply) :lease-batches)))
        (should (equal (funcall core-of gplan) (funcall core-of aplan)))
        (should (equal (cl-remove-if-not #'dorg-ibr-ext-p expected)
                       (funcall ext-of aplan)))
        (should (equal (plist-put (copy-sequence gplan) :db-ops nil)
                       (plist-put (copy-sequence aplan) :db-ops nil)))
        (should (equal (plist-get golden :id-calls) (plist-get actual :id-calls)))))))

(provide 'dorg-bm25-tests)
;;; dorg-bm25-tests.el ends here
