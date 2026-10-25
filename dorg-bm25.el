;;; dorg-bm25.el --- Versioned lexical profile and BM25 derivation -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Q1 qualification substrate for ranked lexical retrieval.  It owns no
;; schema, index, extension registration, or query grammar.  Its only job is
;; to make the tokenizer/profile and BM25 arithmetic inspectable before those
;; layers are allowed to depend on them.

;;; Code:

(require 'cl-lib)
(require 'dorg-ext)
(require 'dorg-ql-eval)

(declare-function dorg-ext-read-context-root-id "dorg-ext-read")
(declare-function dorg-ext-read-context-query "dorg-ext-read")
(declare-function dorg-sql-quote "dorg-dolt")

(defconst dorg-bm25-profile-version "dorg-bm25-v1"
  "Version of the DOrg-owned lexical profile.")

(defconst dorg-bm25-default-k1 1.2
  "Default BM25 term-frequency saturation parameter.")

(defconst dorg-bm25-default-b 0.75
  "Default BM25 document-length normalization parameter.")

(defconst dorg-bm25-default-title-weight 2.0
  "Default independent BM25 weight for a heading title.")

(defconst dorg-bm25-default-body-weight 1.0
  "Default independent BM25 weight for directly owned body text.")

(defconst dorg-bm25-tokenizer-version "dorg-bm25-tokenizer-v1"
  "Version of the DOrg-owned token-boundary implementation.")

(defconst dorg-bm25-max-token-characters 256
  "Maximum character length of a v1 indexed token.

Longer tokens are dropped before postings are derived.  This profile rule keeps
the 4-byte UTF-8 term component within 1024 bytes, well below the installed
Dolt composite-key limit exercised by the schema control.")

(defconst dorg-bm25-max-query-terms 32
  "Maximum v1 tokenizer terms accepted in one ranked request.")

(defconst dorg-bm25-extension-name 'bm25
  "Registered extension name for DOrg's derived lexical read model.")

(defconst dorg-bm25-schema-version 3
  "Schema version of DOrg's derived lexical read model.
Version 3 maintains the root aggregates incrementally and no longer stores
the index generation, which is derived from the corpus generation.")

(defconst dorg-bm25-postings-term-index
  "dorg_bm25_postings_root_profile_term"
  "Name of the postings index used by bounded ranked term lookups.")

(defconst dorg-bm25--profiles '(sensitive insensitive)
  "The two version-one lexical profiles materialized from canonical content.")

(defconst dorg-bm25-schema
  '("CREATE TABLE dorg_bm25_documents (root_id BIGINT NOT NULL, heading_id BIGINT NOT NULL, profile VARCHAR(32) NOT NULL, title_length BIGINT NOT NULL, body_length BIGINT NOT NULL, PRIMARY KEY (root_id, heading_id, profile))"
    "CREATE TABLE dorg_bm25_postings (root_id BIGINT NOT NULL, heading_id BIGINT NOT NULL, profile VARCHAR(32) NOT NULL, field_name VARCHAR(16) NOT NULL, term VARCHAR(256) COLLATE utf8mb4_0900_bin NOT NULL, term_frequency BIGINT NOT NULL, PRIMARY KEY (root_id, heading_id, profile, field_name, term), KEY dorg_bm25_postings_root_profile_term (root_id, profile, term, field_name, heading_id))"
    "CREATE TABLE dorg_bm25_terms (root_id BIGINT NOT NULL, profile VARCHAR(32) NOT NULL, field_name VARCHAR(16) NOT NULL, term VARCHAR(256) COLLATE utf8mb4_0900_bin NOT NULL, document_frequency BIGINT NOT NULL, PRIMARY KEY (root_id, profile, field_name, term))"
    "CREATE TABLE dorg_bm25_manifest (root_id BIGINT NOT NULL, profile VARCHAR(32) NOT NULL, profile_version VARCHAR(32) NOT NULL, tokenizer_version VARCHAR(32) NOT NULL, schema_version BIGINT NOT NULL, corpus_generation VARCHAR(128) NOT NULL, corpus_size BIGINT NOT NULL, title_length BIGINT NOT NULL, body_length BIGINT NOT NULL, PRIMARY KEY (root_id, profile))")
  "Schema for the DOrg-owned derived lexical tables.")

(defun dorg-bm25-tokenize (text case-mode)
  "Return DOrg v1 lexical tokens from TEXT under CASE-MODE.

Tokens are nonempty runs of Unicode alphanumeric characters or underscore.
Punctuation is a separator; identifiers retain underscores.  `sensitive'
retains the literal Unicode codepoints and `insensitive' delegates to the
same per-character standard-case canonicalization used by DOrg QL.  This
profile intentionally performs no Unicode normalization, accent folding,
stemming, or stop-word removal."
  (unless (stringp text)
    (error "dorg-bm25-tokenize requires a string, got %S" text))
  (let ((input (pcase case-mode
                 ('sensitive text)
                 ('insensitive (dorg-ql--case-canonical-string text))
                 (_ (error "dorg-bm25-tokenize unknown case mode: %S" case-mode)))))
    (cl-remove-if (lambda (token) (> (length token) dorg-bm25-max-token-characters))
                  (split-string input "[^[:alnum:]_]+" t))))

(defun dorg-bm25-score-term (term-frequency document-length average-length
                                            document-frequency corpus-size
                                            &optional k1 b)
  "Return the standard BM25 contribution for one term.

The formula is `log(1 + (N - df + .5) / (df + .5)) *
(tf * (k1 + 1)) / (tf + k1 * (1 - b + b * dl / avgdl))'.
Arguments describe a fixed corpus generation; callers compose independent
title/body field scores with their declared weights.  An absent query term
has document frequency zero and contributes zero rather than changing the
score of the terms present in the captured corpus."
  (let ((k1 (or k1 dorg-bm25-default-k1))
        (b (or b dorg-bm25-default-b)))
    (unless (and (integerp term-frequency) (>= term-frequency 0)
                 (integerp document-length) (>= document-length 0)
                 (numberp average-length) (> average-length 0)
                 (integerp document-frequency) (>= document-frequency 0)
                 (integerp corpus-size) (>= corpus-size document-frequency)
                 (numberp k1) (> k1 0) (numberp b) (<= 0 b 1))
      (error "invalid BM25 corpus statistics"))
    (if (or (zerop term-frequency) (zerop document-frequency))
        0.0
      (let* ((idf (log (+ 1.0 (/ (+ (- corpus-size document-frequency) 0.5)
                                  (+ document-frequency 0.5)))))
             (normalizer (+ 1.0 (- b) (* b (/ (float document-length)
                                                average-length)))))
        (* idf (/ (* term-frequency (+ k1 1.0))
                  (+ term-frequency (* k1 normalizer))))))))

(defun dorg-bm25--direct-body (heading)
  "Return HEADING's directly owned text-item bodies in canonical order.

Planning, drawers, keywords, descendants, and DOrg metadata remain outside the
version-one document boundary."
  (mapconcat
   #'identity
   (delq nil
         (mapcar (lambda (item)
                   (and (cl-typep item 'dorg-text-item)
                        (dorg-text-item-text item)))
                 (dorg-heading-items heading)))
   "\n"))

(defun dorg-bm25--term-frequencies (tokens)
  "Return TOKEN -> count alist for one tokenized field.

The alist is sorted bytewise so the derived effect sequence is independent of
hash-table iteration order."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (token tokens)
      (puthash token (1+ (gethash token counts 0)) counts))
    (sort (hash-table-values
           (let ((rows (make-hash-table :test 'equal)))
             (maphash (lambda (token count) (puthash token (cons token count) rows))
                      counts)
             rows))
          (lambda (left right) (string< (car left) (car right))))))

(defun dorg-bm25--image-headings (root-id image)
  "Return non-root headings from IMAGE in canonical numeric-id order."
  (let (headings)
    (maphash (lambda (id heading)
               (when (and (integerp id) (/= id root-id))
                 (push heading headings)))
             image)
    (sort headings (lambda (left right)
                     (< (dorg-heading-id left) (dorg-heading-id right))))))

(defun dorg-bm25--row (&rest cells)
  "Return CELLS as one immutable-shaped derived row alist."
  cells)

(defun dorg-bm25--index-generation (profile corpus-generation)
  "Return PROFILE's immutable generation token for CORPUS-GENERATION."
  (format "%s/%s/schema-%d/%s" profile dorg-bm25-tokenizer-version
          dorg-bm25-schema-version corpus-generation))

(defconst dorg-bm25--digest-tag "dorg-bm25-corpus-v3"
  "Domain tag of the version-3 per-document corpus digest.")

(defun dorg-bm25-document-digest (heading-id title body)
  "Return the 64-hex SHA-256 identity digest of one lexical document.
The hashed bytes are the domain tag, NUL, the decimal HEADING-ID, NUL, then
TITLE and BODY each as its decimal UTF-8 byte length, a colon and its bytes.
A nil TITLE is the empty title.  Heading ids are globally unique, so equal
documents of different roots never share a digest."
  (let ((title (encode-coding-string (or title "") 'utf-8))
        (body (encode-coding-string (or body "") 'utf-8)))
    (secure-hash 'sha256
                 (concat dorg-bm25--digest-tag "\0" (number-to-string heading-id) "\0"
                         (number-to-string (length title)) ":" title
                         (number-to-string (length body)) ":" body))))

(defun dorg-bm25--corpus-generation (root-id image)
  "Return the searchable-corpus generation of ROOT-ID's documents in IMAGE.
It is the 256-bit xor of every document digest (`dorg-bm25-document-digest'),
so it is order-free, maintained incrementally by xoring each removed and
each added document, and equal to the value a fresh rebuild derives."
  (format "%064x"
          (apply #'logxor 0
                 (mapcar (lambda (heading)
                           (string-to-number
                            (dorg-bm25-document-digest (dorg-heading-id heading)
                                                       (dorg-heading-title heading)
                                                       (dorg-bm25--direct-body heading))
                            16))
                         (dorg-bm25--image-headings root-id image)))))

(defun dorg-bm25-derive-image (root-id image)
  "Derive DOrg's complete lexical rows for ROOT-ID from canonical IMAGE.

This is the single pure derivation used by both mutation projection and exact
rebuild.  Its result contains root-scoped `:documents', `:postings', `:terms',
and `:manifests' row lists; callers lower only these candidate-derived rows.
No caller-supplied indexing payload or store read participates."
  (unless (and (integerp root-id) (> root-id 0) (hash-table-p image))
    (error "dorg-bm25-derive-image requires a positive root and image"))
  (let ((headings (dorg-bm25--image-headings root-id image))
        documents postings terms manifests
        (corpus-generation (dorg-bm25--corpus-generation root-id image)))
    (dolist (mode dorg-bm25--profiles)
      (let ((profile (symbol-name mode))
            (title-total 0)
            (body-total 0)
            (df (make-hash-table :test 'equal)))
        (dolist (heading headings)
          (let* ((heading-id (dorg-heading-id heading))
                 (title (or (dorg-heading-title heading) ""))
                 (body (dorg-bm25--direct-body heading))
                 (fields `(("title" . ,(dorg-bm25-tokenize title mode))
                           ("body" . ,(dorg-bm25-tokenize body mode)))))
            (setq title-total (+ title-total (length (cdr (assoc "title" fields))))
                  body-total (+ body-total (length (cdr (assoc "body" fields)))))
            (push (dorg-bm25--row (cons 'root_id root-id)
                                  (cons 'heading_id heading-id)
                                  (cons 'profile profile)
                                  (cons 'title_length (length (cdr (assoc "title" fields))))
                                  (cons 'body_length (length (cdr (assoc "body" fields)))))
                  documents)
            (dolist (field fields)
              (let ((field-name (car field))
                    (frequencies (dorg-bm25--term-frequencies (cdr field))))
                (dolist (frequency frequencies)
                  (push (dorg-bm25--row (cons 'root_id root-id)
                                        (cons 'heading_id heading-id)
                                        (cons 'profile profile)
                                        (cons 'field_name field-name)
                                        (cons 'term (car frequency))
                                        (cons 'term_frequency (cdr frequency)))
                        postings)
                  (let ((key (list profile field-name (car frequency))))
                    (puthash key (1+ (gethash key df 0)) df)))))))
        (maphash
         (lambda (key document-frequency)
           (push (dorg-bm25--row (cons 'root_id root-id)
                                 (cons 'profile (nth 0 key))
                                 (cons 'field_name (nth 1 key))
                                 (cons 'term (nth 2 key))
                                 (cons 'document_frequency document-frequency))
                 terms))
         df)
        (push (dorg-bm25--row (cons 'root_id root-id)
                              (cons 'profile profile)
                              (cons 'profile_version dorg-bm25-profile-version)
                              (cons 'tokenizer_version dorg-bm25-tokenizer-version)
                              (cons 'schema_version dorg-bm25-schema-version)
                              (cons 'corpus_generation corpus-generation)
                              (cons 'corpus_size (length headings))
                              (cons 'title_length title-total)
                              (cons 'body_length body-total))
              manifests)))
    (list :documents (sort documents (lambda (left right)
                                       (string< (prin1-to-string left)
                                                (prin1-to-string right))))
          :postings (sort postings (lambda (left right)
                                     (string< (prin1-to-string left)
                                              (prin1-to-string right))))
          :terms (sort terms (lambda (left right)
                               (string< (prin1-to-string left)
                                        (prin1-to-string right))))
          :manifests (sort manifests (lambda (left right)
                                       (string< (prin1-to-string left)
                                                (prin1-to-string right)))))))

;; Bounded maintenance.  A v1 document is one heading's title plus its
;; directly owned text, so an edit changes exactly the documents of the
;; context's entries.  Heading-local rows (documents and postings) are
;; replaced per affected document.  Root aggregates are accumulators: each
;; term's document frequency, and the manifest's corpus size, field lengths
;; and corpus generation (an xor of document digests).  DOrg folds them
;; against the rows stored at the plan base, so no whole-root image is
;; derived.  The manifest is the completeness marker: its rows for every
;; profile are emitted for each touched root, so DOrg can tell a built root
;; from an unbuilt one, and a root's first write births all of them.

(defun dorg-bm25--document (heading)
  "Return (TITLE BODY DIGEST) for HEADING's lexical document."
  (let ((title (or (dorg-heading-title heading) ""))
        (body (dorg-bm25--direct-body heading)))
    (list title body (dorg-bm25-document-digest (dorg-heading-id heading) title body))))

(defun dorg-bm25--entry-sides (context entry)
  "Return ((OLD-ROOT . OLD-HEADING) . (NEW-ROOT . NEW-HEADING)) for ENTRY.
Either side is nil when absent: a created heading has no old document, a
deleted one no new document.  In a cross-root context a moved heading's old
document belongs to the source root and its new one to the destination
root.  Any other entry there must keep its document unchanged, because its
root is not established by the context; a changed one fails closed."
  (let* ((kind (dorg-ext-index-entry-kind entry))
         (before (and (memq kind '(change move delete)) (dorg-ext-index-entry-before entry)))
         (after (and (memq kind '(create change move)) (dorg-ext-index-entry-after entry)))
         (from (dorg-ext-index-context-from-root-id context))
         (root (dorg-ext-index-context-root-id context)))
    (cond
     ((null from)
      (cons (and before (cons root before)) (and after (cons root after))))
     ((eq kind 'move)
      (cons (and before (cons from before))
            (and after (cons (dorg-ext-index-context-to-root-id context) after))))
     ((and before after
           (equal (dorg-bm25--document before) (dorg-bm25--document after)))
      nil)
     (t
      (error "BM25 cannot place a changed non-moved document in a cross-root context: %S"
             (dorg-ext-index-entry-id entry))))))

(defun dorg-bm25--context-entries (context)
  "Return CONTEXT's affected entries, each heading once, root excluded."
  (let ((roots (delq nil (list (dorg-ext-index-context-root-id context)
                               (dorg-ext-index-context-from-root-id context)
                               (dorg-ext-index-context-to-root-id context))))
        (seen (make-hash-table :test 'eql))
        entries)
    (dolist (entry (append (dorg-ext-index-context-created context)
                           (dorg-ext-index-context-changed context)
                           (dorg-ext-index-context-moved context)
                           (dorg-ext-index-context-deleted context)))
      (let ((id (dorg-ext-index-entry-id entry)))
        (unless (or (memq id roots) (gethash id seen))
          (puthash id t seen)
          (push entry entries))))
    (sort entries (lambda (a b) (< (dorg-ext-index-entry-id a)
                                   (dorg-ext-index-entry-id b))))))

(defun dorg-bm25--document-fields (heading mode)
  "Return HEADING's tokenized (\"title\" . TOKENS) and (\"body\" . TOKENS) for MODE."
  (pcase-let ((`(,title ,body ,_digest) (dorg-bm25--document heading)))
    `(("title" . ,(dorg-bm25-tokenize title mode))
      ("body" . ,(dorg-bm25-tokenize body mode)))))

(defun dorg-bm25--inserts-by-table (rows)
  "Return the insert effects of ROWS grouped documents first, then postings.
Each table keeps its order.  The projector inserts into no other table, and
any such insert fails closed rather than being dropped."
  (let* ((inserts (cl-remove-if-not (lambda (row) (plist-member row :insert)) rows))
         (grouped (cl-loop for table in '("dorg_bm25_documents" "dorg_bm25_postings")
                           append (cl-remove-if-not
                                   (lambda (row) (equal table (plist-get row :table)))
                                   inserts))))
    (unless (= (length grouped) (length inserts))
      (error "BM25 projected an insert outside its document and posting tables"))
    grouped))

(defun dorg-bm25-project (context)
  "Project CONTEXT's lexical maintenance from its affected documents only.
Each changed document's old rows are deleted and its new rows inserted under
its root; document frequencies and manifest totals change by accumulator
deltas that DOrg folds against the stored rows.  Unchanged documents (a pure
in-root move, a property edit) contribute nothing.  A context with no entry
at all (an empty root's rebuild) still emits each root's zero manifest
accumulators, so the root is born built and empty.  The projector reads no
store state."
  (let ((initial `((profile_version . ,dorg-bm25-profile-version)
                   (tokenizer_version . ,dorg-bm25-tokenizer-version)
                   (schema_version . ,dorg-bm25-schema-version)))
        (manifests (make-hash-table :test 'equal))
        (terms (make-hash-table :test 'equal))
        (entries (dorg-bm25--context-entries context))
        rows)
    (cl-flet ((manifest (root profile)
                (or (gethash (cons root profile) manifests)
                    (puthash (cons root profile) (list 0 0 0 0) manifests))))
      (dolist (entry entries)
        (let* ((sides (dorg-bm25--entry-sides context entry))
               (old (car sides))
               (new (cdr sides)))
          (unless (or (null sides)
                      (and old new (eql (car old) (car new))
                           (equal (dorg-bm25--document (cdr old))
                                  (dorg-bm25--document (cdr new)))))
            (let ((id (dorg-ext-index-entry-id entry)))
              (dolist (mode dorg-bm25--profiles)
                (let ((profile (symbol-name mode)))
                  (dolist (side (list (cons -1 old) (cons 1 new)))
                    (when-let ((placed (cdr side)))
                      (let* ((sign (car side))
                             (root (car placed))
                             (heading (cdr placed))
                             (fields (dorg-bm25--document-fields heading mode))
                             (title-length (length (cdr (assoc "title" fields))))
                             (body-length (length (cdr (assoc "body" fields))))
                             (totals (manifest root profile)))
                        (setf (nth 0 totals) (+ (nth 0 totals) sign)
                              (nth 1 totals) (+ (nth 1 totals) (* sign title-length))
                              (nth 2 totals) (+ (nth 2 totals) (* sign body-length))
                              (nth 3 totals) (logxor (nth 3 totals)
                                                     (string-to-number
                                                      (nth 2 (dorg-bm25--document heading))
                                                      16)))
                        (if (< sign 0)
                            (progn
                              (push (list :table "dorg_bm25_documents"
                                          :delete `((root_id . ,root) (heading_id . ,id)
                                                    (profile . ,profile)))
                                    rows)
                              (push (list :table "dorg_bm25_postings"
                                          :delete `((root_id . ,root) (heading_id . ,id)
                                                    (profile . ,profile)))
                                    rows))
                          (push (list :table "dorg_bm25_documents"
                                      :insert `((root_id . ,root) (heading_id . ,id)
                                                (profile . ,profile)
                                                (title_length . ,title-length)
                                                (body_length . ,body-length)))
                                rows))
                        (dolist (field fields)
                          (dolist (frequency (dorg-bm25--term-frequencies (cdr field)))
                            (let ((key (list root profile (car field) (car frequency))))
                              (puthash key (+ sign (gethash key terms 0)) terms))
                            (when (> sign 0)
                              (push (list :table "dorg_bm25_postings"
                                          :insert `((root_id . ,root) (heading_id . ,id)
                                                    (profile . ,profile)
                                                    (field_name . ,(car field))
                                                    (term . ,(car frequency))
                                                    (term_frequency . ,(cdr frequency))))
                                    rows)))))))))))))
      ;; A context with no entry at all (an empty root's rebuild or import)
      ;; still names its roots' markers.  A context whose entries leave every
      ;; document unchanged emits nothing and reads nothing.
      (when (null entries)
        (dolist (root (delete-dups
                       (delq nil (list (dorg-ext-index-context-root-id context)
                                       (dorg-ext-index-context-from-root-id context)
                                       (dorg-ext-index-context-to-root-id context)))))
          (dolist (mode dorg-bm25--profiles)
            (manifest root (symbol-name mode))))))
    ;; Deletes precede inserts so a replaced document's rows net to its new
    ;; ones.  Inserts are then grouped by table, documents before postings,
    ;; each in emission order, so DOrg merges each table's inserts into a
    ;; few multi-row statements.  These tables declare no foreign keys.
    (setq rows (nreverse rows))
    (append
     (cl-remove-if-not (lambda (row) (plist-member row :delete)) rows)
     (dorg-bm25--inserts-by-table rows)
     (let (accumulators)
       (maphash (lambda (key delta)
                  (unless (zerop delta)
                    (push `(:table "dorg_bm25_terms"
                            :accumulate ((root_id . ,(nth 0 key)) (profile . ,(nth 1 key))
                                         (field_name . ,(nth 2 key)) (term . ,(nth 3 key)))
                            :add ((document_frequency . ,delta))
                            :prune-zero document_frequency)
                          accumulators)))
                terms)
       (maphash (lambda (key totals)
                  (push `(:table "dorg_bm25_manifest"
                          :accumulate ((root_id . ,(car key)) (profile . ,(cdr key)))
                          :add ((corpus_size . ,(nth 0 totals))
                                (title_length . ,(nth 1 totals))
                                (body_length . ,(nth 2 totals)))
                          :xor ((corpus_generation . ,(format "%064x" (nth 3 totals))))
                          :initial ,initial)
                        accumulators))
                manifests)
       (sort accumulators (lambda (a b) (string< (prin1-to-string a)
                                                 (prin1-to-string b))))))))

(defun dorg-bm25--postings-sql (root-id profile terms)
  "Return the term-leading first ranked-read stage for ROOT-ID and TERMS.

This deliberately touches only the postings table.  In a revision database,
Dolt's optimizer may otherwise reorder a multi-table score expression before
the term predicate and scan a root's entire postings set."
  (unless (and (integerp root-id) (> root-id 0)
               (member profile dorg-bm25--profiles)
               (consp terms) (cl-every #'stringp terms))
    (error "invalid BM25 postings-read plan"))
  (format (concat "SELECT heading_id, field_name, term, term_frequency"
                  " FROM dorg_bm25_postings WHERE root_id = %d"
                  " AND profile = %s AND term IN (%s)")
          root-id (dorg-sql-quote (symbol-name profile))
          (mapconcat #'dorg-sql-quote terms ", ")))

(defun dorg-bm25--sql-integers (ids)
  "Return IDS as a nonempty DOrg-owned SQL integer list."
  (unless (and ids (cl-every #'integerp ids))
    (error "BM25 follow-up requires nonempty integer heading ids"))
  (mapconcat #'number-to-string (sort (delete-dups (copy-sequence ids)) #'<) ", "))

(defun dorg-bm25--round-score (score)
  "Round SCORE to the public nine-decimal BM25 representation."
  (/ (float (round (* score 1000000000))) 1000000000))

(defun dorg-bm25--reduce-ranked-rows (manifest postings documents terms headings top-k)
  "Combine bounded BM25 stage rows into ranked result alists.

POSTINGS is already limited to the query terms by the physical first stage;
HEADINGS is the pre-rank QL-filtered candidate subset."
  (let* ((m (car manifest))
         (n (alist-get 'corpus_size m))
         (title-total (alist-get 'title_length m))
         (body-total (alist-get 'body_length m))
         (document-by-id (make-hash-table :test 'eql))
         (df-by-field-term (make-hash-table :test 'equal))
         (heading-by-id (make-hash-table :test 'eql))
         (scores (make-hash-table :test 'eql)))
    (dolist (row documents)
      (puthash (alist-get 'heading_id row) row document-by-id))
    (dolist (row terms)
      (puthash (list (alist-get 'field_name row) (alist-get 'term row))
               (alist-get 'document_frequency row) df-by-field-term))
    (dolist (row headings)
      (puthash (alist-get 'heading_id row) row heading-by-id))
    (dolist (posting postings)
      (let* ((heading-id (alist-get 'heading_id posting))
             (field (alist-get 'field_name posting))
             (document (gethash heading-id document-by-id))
             (heading (gethash heading-id heading-by-id))
             (length-key (if (equal field "title") 'title_length 'body_length))
             (total (if (equal field "title") title-total body-total))
             (weight (if (equal field "title")
                         dorg-bm25-default-title-weight
                       dorg-bm25-default-body-weight))
             (df (gethash (list field (alist-get 'term posting)) df-by-field-term)))
        (when (and document heading df (> n 0) (> total 0))
          (puthash heading-id
                   (+ (gethash heading-id scores 0.0)
                      (* weight
                         (dorg-bm25-score-term
                          (alist-get 'term_frequency posting)
                          (alist-get length-key document)
                          (/ (float total) n) df n)))
                   scores))))
    (let (rows)
      (maphash
       (lambda (heading-id score)
         (when-let ((heading (gethash heading-id heading-by-id)))
           (push (list (cons 'heading_id heading-id)
                       (cons 'title (alist-get 'title heading))
                       (cons 'score (dorg-bm25--round-score score)))
                 rows)))
       scores)
      (setq rows
            (sort rows (lambda (left right)
                         (let ((left-score (alist-get 'score left))
                               (right-score (alist-get 'score right)))
                           (if (= left-score right-score)
                               (< (alist-get 'heading_id left)
                                  (alist-get 'heading_id right))
                             (> left-score right-score))))))
      (cl-subseq rows 0 (min top-k (length rows))))))

(defun dorg-bm25-hydrate (context)
  "Return one revision-safe ranked-read plan from CONTEXT's internal query.

The query bundle is constructed exclusively by the Dolt QL planner.  It carries
the canonical profile, deduplicated tokenizer terms, lowered pre-rank filter,
and bounded top-k.  The manifest statement proves a compatible complete
generation before the score rows are published."
  (let* ((query (dorg-ext-read-context-query context))
         (root-id (dorg-ext-read-context-root-id context))
         (profile (plist-get query :profile))
         (terms (plist-get query :terms))
         (filter-sql (plist-get query :filter-sql))
         (joins (or (plist-get query :joins) ""))
         (top-k (plist-get query :top-k)))
    (unless (and (memq profile dorg-bm25--profiles)
                 (listp terms) (cl-every #'stringp terms)
                 (stringp filter-sql) (stringp joins)
                 (integerp top-k) (<= 1 top-k 100))
      (error "invalid DOrg BM25 hydrate query"))
    (let ((manifest-sql
           (format
            (concat "SELECT profile, profile_version, tokenizer_version, schema_version,"
                    " corpus_generation, corpus_size, title_length, body_length"
                    " FROM dorg_bm25_manifest WHERE root_id = %d AND profile = %s")
            root-id (dorg-sql-quote (symbol-name profile)))))
      (if (null terms)
          (list :sql (list manifest-sql)
                :reduce (lambda (results)
                          (list :manifest (car results) :matches nil)))
        (list
         :sql (list manifest-sql (dorg-bm25--postings-sql root-id profile terms))
         :next
         (lambda (first-stage)
           (let* ((manifest (car first-stage))
                  (postings (cadr first-stage))
                  (ids (delete-dups
                        (mapcar (lambda (row) (alist-get 'heading_id row)) postings))))
             (if (null ids)
                 (list :sql nil
                       :reduce (lambda (_)
                                 (list :manifest manifest :matches nil)))
               (let* ((ids-sql (dorg-bm25--sql-integers ids))
                      (terms-sql (mapconcat #'dorg-sql-quote terms ", "))
                      (profile-sql (dorg-sql-quote (symbol-name profile))))
                 (list
                  :sql
                  (list
                   (format (concat "SELECT heading_id, title_length, body_length"
                                   " FROM dorg_bm25_documents WHERE root_id = %d"
                                   " AND profile = %s AND heading_id IN (%s)")
                           root-id profile-sql ids-sql)
                   (format (concat "SELECT field_name, term, document_frequency"
                                   " FROM dorg_bm25_terms WHERE root_id = %d"
                                   " AND profile = %s AND term IN (%s)")
                           root-id profile-sql terms-sql)
                   (format (concat "SELECT h.id AS heading_id, h.title AS title FROM headings h%s"
                                   " WHERE h.root_id = %d AND h.id IN (%s) AND (%s)")
                           joins root-id ids-sql filter-sql))
                  :reduce
                  (lambda (second-stage)
                    (list :manifest manifest
                          :matches
                          (dorg-bm25--reduce-ranked-rows
                           manifest postings (nth 0 second-stage)
                           (nth 1 second-stage) (nth 2 second-stage) top-k)))))))))))))

(defun dorg-bm25-register-extension ()
  "Register the versioned BM25 derived-state extension and return it.

Callers choose when to register/migrate this optional read model.  Registration
does not create a schema or index by itself; `dorg-init' or the explicit schema
migration path owns that store transition."
  (dorg-register-extension
   dorg-bm25-extension-name
   :schema dorg-bm25-schema
   :version dorg-bm25-schema-version
   :migrations
   `((1 . ,(format (concat "ALTER TABLE dorg_bm25_postings ADD INDEX %s "
                           "(root_id, profile, term, field_name, heading_id)")
                     dorg-bm25-postings-term-index))
     ;; Version-2 generations are not version-3 values, and a surviving v2
     ;; manifest would be folded as if built.  Derived rows are rebuildable,
     ;; so every root becomes unbuilt until `dorg-ext-rebuild-index'.
     (2 . ("ALTER TABLE dorg_bm25_manifest DROP COLUMN index_generation"
           "DELETE FROM dorg_bm25_postings"
           "DELETE FROM dorg_bm25_documents"
           "DELETE FROM dorg_bm25_terms"
           "DELETE FROM dorg_bm25_manifest")))
   :capabilities '(dolt)
   ;; Bounded scope: the projector derives only the changed headings, and the
   ;; root aggregates are accumulators DOrg folds against plan-base priors.
   :read-tables
   `((:name "dorg_bm25_documents" :key-column "heading_id" :scope-column "root_id")
     (:name "dorg_bm25_postings" :key-column "heading_id" :scope-column "root_id")
     (:name "dorg_bm25_terms" :key-column "term" :scope-column "root_id"
      :accumulate (root_id profile field_name term))
     (:name "dorg_bm25_manifest" :key-column "root_id" :scope-column "root_id"
      :accumulate (root_id profile)
      :marker ,(mapcar (lambda (mode) (list (cons 'profile (symbol-name mode))))
                       dorg-bm25--profiles)))
   :project #'dorg-bm25-project
   :hydrate #'dorg-bm25-hydrate))

(provide 'dorg-bm25)
;;; dorg-bm25.el ends here
