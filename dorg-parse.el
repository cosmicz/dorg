;;; dorg-parse.el --- Org buffer to dorg domain model -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Parser for converting org-mode buffers into dorg domain objects
;; (`dorg-heading' + `dorg-heading-item' EIEIO hierarchy).
;;
;; Identity model:
;; - Existing headings are identified by a `:DORG_ID:' property in
;;   their PROPERTIES drawer.  Parser reads the id if present and
;;   leaves it nil otherwise.
;; - `dorg-parse-assign-ids' completes identity in an isolated parse
;;   model for explicit import/recovery and legacy sync callers.
;; - Every heading carries `root_id' pointing at its file's level-0
;;   synthetic root.
;;
;; Main entry point:
;; - `dorg-parse-buffer' — parse buffer, return `dorg-parse-result'

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'dorg-id)
(require 'dorg-model)
(require 'dorg-sortkey)

;;; Result Struct

(cl-defstruct dorg-parse-result
  "Result of a `dorg-parse-buffer' call.

- ROOT-ID: the heading id for the file's synthetic root,
  or nil if no root-id was provided (caller must assign).
- HEADINGS: list of `dorg-heading' structs (all headings
  including root).  New headings have id = nil.
- NEW-HEADING-MARKERS: list of (HEADING-STRUCT . BUFFER-MARKER)
  for headings that had no `:DORG_ID:' property.  Used by
  the deprecated reconciliation adapter for legacy write-back.
- HEADING-MARKERS: hash table of heading-id -> buffer-marker
  for ALL headings with an existing DORG_ID.  New headings
  are added after ID assignment by `dorg-parse-assign-ids'."
  root-id
  headings
  new-heading-markers
  heading-markers)

;;; Internal State

;; Parse accumulators were six dynamically-bound specials used as implicit
;; parameters across the helper call tree.  They are now a single lexical
;; state object threaded explicitly through the helpers (CTX), so parse
;; carries no global mutable state.  CURRENT-ITEMS became a return value
;; (`dorg-parse--process-section' yields its items) and ROOT-ID a plain
;; local in the entry points, leaving four threaded accumulator slots.

(cl-defstruct (dorg-parse--ctx (:constructor dorg-parse--ctx-make))
  "Threaded lexical state for one parse, replacing former dynamic specials.

- HEADINGS: accumulator (reverse document order) of `dorg-heading' structs.
- HEADING-MARKERS: hash of existing heading-id → buffer-marker.
- NEW-HEADING-MARKERS: accumulator of (dorg-heading . buffer-marker) for
  headings lacking a DORG_ID.
- VERBATIM-REGIONS: list of (START . END) buffer positions for verbatim
  block content (from `dorg-parse--verbatim-regions').  A headline whose
  start falls strictly inside one of these regions is sample text inside
  an example/source block (org promotes any column-0 heading-looking line
  to a real headline because a headline can never be contained in a
  block) and is skipped rather than emitted.  Nil means no skipping."
  headings
  heading-markers
  new-heading-markers
  verbatim-regions)

;;; ─── Parse Entry Point ────────────────────────────────────────────────

(defun dorg-parse--parse-accessible ()
  "Parse the accessible region of the current buffer for dorg consumption.
Uses fixed `element' granularity, which preserves every construct DOrg
consumes while skipping inline-object parsing that DOrg discards.  This is
the single parse entry point shared by the full-buffer parse and narrowed
region helpers."
  (org-element-parse-buffer 'element))

;;; Section Processing — builds EIEIO item objects

(defun dorg-parse--trailing-blank-lines (raw)
  "Return the count of blank lines trailing RAW's content.
RAW is a section child's source span [begin,end).  Its final content line is
newline-terminated; any further trailing whitespace-only lines are the
element's blank run.  Counting geometry rather than trusting `org-element'
`:post-blank' corrects org's +1 over-report for `fixed-width' and other
lesser elements, which miscount the content-terminating newline as a blank
line."
  (let ((parts (split-string raw "\n")))
    ;; Drop the single "" split artifact produced by the final content newline.
    (when (and parts (string-empty-p (car (last parts))))
      (setq parts (butlast parts)))
    (let ((n 0) (rev (nreverse parts)))
      (while (and rev (string-blank-p (car rev)))
        (setq n (1+ n) rev (cdr rev)))
      n)))

(defun dorg-parse--element-text-item (element)
  "Return a text item preserving the source geometry of ELEMENT, or nil.
This is the single conversion path for ordinary text-like section children
and for elements that Org classified as planning but that contain no
recognized planning timestamp."
  (let ((beg (org-element-property :begin element))
        (end (org-element-property :end element)))
    (when (and beg end)
      (let* ((raw (buffer-substring-no-properties beg end))
             ;; org-element over-reports :post-blank by 1 for `fixed-width'
             ;; and other lesser elements: it counts the element's own
             ;; content-terminating newline as a blank line.  Derive the
             ;; TRUE trailing blank-line count from source geometry so a
             ;; text item's post-blank is the number of blank lines that
             ;; actually follow its content — uniform across element types
             ;; and idempotent under render -> parse.
             (text-pb (dorg-parse--trailing-blank-lines raw))
             (text raw))
        ;; Remove trailing post-blank newlines to avoid duplication.
        (when (> text-pb 0)
          (setq text (string-trim-right text)))
        ;; Strip comma escapes from verbatim block bodies so the model holds
        ;; true content; `dorg-model--render-item' re-escapes on serialize,
        ;; closing a lossless round-trip.
        (setq text (dorg--unescape-verbatim-bodies text))
        (unless (string-empty-p text)
          (dorg-text-item
           :post-blank text-pb
           :text text))))))

(defun dorg-parse--section-child-item (child)
  "Classify one SECTION CHILD org element into a `dorg-heading-item', or nil.
Returns nil for children the model drops (a PROPERTIES drawer holding only
DORG_ID, or an empty text block).  This is the single per-child classifier
shared by `dorg-parse--process-section' and `dorg-parse-section-item-spans'
so the model item list and its source spans cannot drift."
  (let ((type (org-element-type child))
        (pb (or (org-element-property :post-blank child) 0)))
    (pcase type
      ('planning
       (let ((scheduled
              (when-let ((ts (org-element-property :scheduled child)))
                (org-element-property :raw-value ts)))
             (deadline
              (when-let ((ts (org-element-property :deadline child)))
                (org-element-property :raw-value ts)))
             (closed
              (when-let ((ts (org-element-property :closed child)))
                (org-element-property :raw-value ts))))
         (if (or scheduled deadline closed)
             (dorg-planning-item
              :post-blank pb
              :scheduled scheduled
              :deadline deadline
              :closed closed)
           ;; Org also classifies planning-keyword-shaped prose as a
           ;; `planning' element.  With no recognized timestamp it has no
           ;; planning semantics; preserve the source through the same text
           ;; geometry path as every other text-like element.
           (dorg-parse--element-text-item child))))
      ('property-drawer
       ;; Collect non-DORG_ID node-properties; drop the item if none remain.
       (let ((props (seq-filter
                     (lambda (np)
                       (and (eq (org-element-type np) 'node-property)
                            (not (equal (org-element-property :key np)
                                        dorg-id-property))))
                     (org-element-contents child))))
         (when props
           (dorg-drawer-item
            :post-blank pb
            :name "PROPERTIES"
            :properties (mapcar
                         (lambda (np)
                           (cons (org-element-property :key np)
                                 (or (org-element-property :value np) "")))
                         props)))))
      ('drawer
       (let ((name (org-element-property :drawer-name child))
             (lines nil))
         ;; Extract raw lines from drawer contents
         (let ((cb (org-element-property :contents-begin child))
               (ce (org-element-property :contents-end child)))
           (when (and cb ce (< cb ce))
             (let ((raw (buffer-substring-no-properties cb ce)))
               ;; Org sets drawer contents-end just before the :END: line,
               ;; so RAW always terminates in exactly one newline (the final
               ;; content line's newline).  Splitting on "\n" therefore yields
               ;; one trailing "" artifact — drop only that, preserving every
               ;; interior and trailing blank line.
               (let ((parts (split-string raw "\n")))
                 (when (and parts (string-empty-p (car (last parts))))
                   (setq parts (butlast parts)))
                 (dolist (line parts)
                   (push line lines))))))
         (dorg-drawer-item
          :post-blank pb
          :name name
          :lines (nreverse lines))))
      ('keyword
       (dorg-keyword-item
        :post-blank pb
        :keyword (org-element-property :key child)
        :value (org-element-property :value child)))
      ;; All other section children are text-like.
      (_ (dorg-parse--element-text-item child)))))

(defun dorg-parse--process-section (section)
  "Process a SECTION element, returning its item objects in document order."
  (let ((items nil))
    (dolist (child (org-element-contents section))
      (let ((it (dorg-parse--section-child-item child)))
        (when it (push it items))))
    (nreverse items)))

(defun dorg-parse-section-item-spans (section)
  "Return per-item source spans for SECTION, index-aligned with its items.
Each element is (ITEM BEGIN CONTENT-END END): BEGIN and END are the source
element's buffer positions (END covers its post-blank run); CONTENT-END is
END walked back over the element's post-blank blank lines — the position just
after the item's final content newline, so [CONTENT-END, END) is exactly the
trailing blank run.  Uses the SAME `dorg-parse--section-child-item' classifier
as `dorg-parse--process-section', so the returned ITEMs are that function's
list in order.  The current buffer must hold SECTION (positions are buffer
positions)."
  (let ((out nil))
    (dolist (child (org-element-contents section))
      (let ((it (dorg-parse--section-child-item child)))
        (when it
          (let* ((beg (org-element-property :begin child))
                 (end (org-element-property :end child))
                 (pb (or (org-element-property :post-blank child) 0))
                 (content-end (save-excursion
                                (goto-char end)
                                (forward-line (- pb))
                                (point))))
            (push (list it beg content-end end) out)))))
    (nreverse out)))

;;; ─── Verbatim Region Detection ────────────────────────────────────────

(defun dorg-parse--verbatim-regions ()
  "Return (START . END) buffer-position pairs for verbatim block content.
Scans the accessible region for `#+begin_NAME' / `#+end_NAME' pairs.
Blocks do not nest (org semantics), so the scan is single-level and
name-matched, case-insensitive, with optional leading whitespace: the
first `#+end_NAME' whose NAME equals the open block's NAME closes it;
intervening `#+begin_…' / non-matching `#+end_…' lines are content.

Each returned pair spans from the beginning of the `#+begin' line to the
beginning of its matching `#+end' line.  Two guards keep a region from
bridging across real heading structure — the over-reach that silently
drops beads on a materialize → reparse round-trip:

1. A block left open at end of buffer yields NO region — an unterminated
   block can never bound a heading.
2. A region may contain AT MOST ONE column-0 headline before its matching
   `#+end_NAME'.  Org force-closes a block at the first column-0 headline,
   promoting it; a genuine sample block closes
   its literal `#+end_NAME' before any further headline, so that single
   promoted heading is correctly recognised as block content.  But once a
   SECOND column-0 headline appears before the matching `#+end_NAME', the
   literal end lies beyond real heading structure (an unbalanced or stray
   `#+begin_…' whose true close was split away on import/serialize).
   Bridging to it would engulf every real heading in between, so the
   region is abandoned instead.

This intentionally differs from org's reader (which force-closes at the
next column-0 headline); the literal scan with the one-headline bound is
what lets DOrg recognise example-block sample headings without
over-consuming the real beads that follow an unbalanced block."
  (let ((regions nil) (open-name nil) (open-pos nil) (open-headlines 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position)))
          (cond
           ((null open-name)
            (when (looking-at "[ \t]*#\\+begin_\\([^ \t\n]+\\)")
              (setq open-name (downcase (match-string 1))
                    open-pos bol
                    open-headlines 0)))
           ;; Matching close: record [begin, end-bol].  The lone promoted
           ;; headline (if any) sits strictly between the two, so
           ;; `dorg-parse--position-verbatim-p' flags it.
           ((and (looking-at "[ \t]*#\\+end_\\([^ \t\n]+\\)")
                 (string= open-name (downcase (match-string 1))))
            (push (cons open-pos bol) regions)
            (setq open-name nil open-pos nil open-headlines 0))
           ;; A column-0 headline while open.  The first is the force-closed
           ;; sample heading (block content); a second means the block is
           ;; crossing real heading structure — abandon, do not bridge.
           ((looking-at "\\*+[ \t]")
            (setq open-headlines (1+ open-headlines))
            (when (>= open-headlines 2)
              (setq open-name nil open-pos nil open-headlines 0)))))
        (forward-line 1)))
    (nreverse regions)))

(defun dorg-parse--position-verbatim-p (pos regions)
  "Return non-nil if POS falls strictly inside any of REGIONS.
REGIONS is a list of (START . END) pairs from `dorg-parse--verbatim-regions'."
  (cl-some (lambda (r) (and (> pos (car r)) (< pos (cdr r))))
           regions))

(defun dorg-parse--headline-phantom-p (element ctx)
  "Return non-nil if headline ELEMENT begins inside a verbatim block region.
Verbatim regions are read from CTX.  Such a headline is sample text inside
an example/source block, not a real DOrg heading."
  (let ((regions (dorg-parse--ctx-verbatim-regions ctx)))
    (and regions
         (dorg-parse--position-verbatim-p
          (org-element-property :begin element) regions))))

;;; Heading Processing

(defun dorg-parse--process-headline (element parent-id sibling-sort ctx)
  "Process a HEADLINE element as child of PARENT-ID at SIBLING-SORT.
SIBLING-SORT is a 1-based integer; converted to unibyte sort key.
PARENT-ID may be nil if the parent is a new root without an id yet.
CTX is the threaded `dorg-parse--ctx' accumulating headings and markers."
  (let* ((existing-id (dorg-id-of-element element t))
         (level (org-element-property :level element))
         (title (org-element-property :raw-value element))
         (todo (org-element-property :todo-keyword element))
         (priority (org-element-property :priority element))
         (commentedp (org-element-property :commentedp element))
         (tags (org-element-property :tags element))
         (post-blank (or (org-element-property :post-blank element) 0)))
    ;; Process section — build item objects (one section per heading, but
    ;; collect across any section children to preserve document order).
    (let ((items nil))
      (dolist (child (org-element-contents element))
        (when (eq (org-element-type child) 'section)
          (setq items (nconc items (dorg-parse--process-section child)))))
      ;; Build dorg-heading struct
      (let ((h (make-dorg-heading
                :id existing-id
                :parent-id parent-id
                :sort-key (dorg-sortkey-from-integer sibling-sort 2)
                :level level
                :todo todo
                :priority (when priority (char-to-string priority))
                :commented commentedp
                :title title
                :post-blank post-blank
                :tags (copy-sequence tags)
                :items items)))
        (dorg-heading-compute-items-hash h)
        (let ((marker (copy-marker (org-element-property :begin element))))
          (if existing-id
              (puthash existing-id marker (dorg-parse--ctx-heading-markers ctx))
            (push (cons h marker) (dorg-parse--ctx-new-heading-markers ctx))))
        (push h (dorg-parse--ctx-headings ctx))))
    ;; Recurse into child headlines — parent-id is existing-id (may be nil).
    ;; Phantom (verbatim-block) child headlines are skipped by the sequence
    ;; processor, which splices their real descendants in at this level.
    (dorg-parse--process-headlines
     (org-element-contents element) existing-id 0 ctx)))

(defun dorg-parse--process-headlines (elements parent-id start-sort ctx)
  "Process headline ELEMENTS (document order) as children of PARENT-ID.
ELEMENTS is any element list; non-headline members are ignored.  Sibling
sort numbers continue after START-SORT (1-based); the last sort value
used is returned so callers can resume the sequence.  CTX is the threaded
`dorg-parse--ctx' (accumulators + verbatim regions).

A phantom headline (one beginning inside a verbatim block region; see
CTX's verbatim-regions slot and `dorg-parse--verbatim-regions') is not
emitted: instead its own child headlines are spliced into this sequence
at the same level, sharing PARENT-ID and the running sort counter, so a
real heading org nested under a phantom survives with correct parentage.
In practice example-block phantoms are childless leaves, so this is a
safety measure rather than a hot path."
  (let ((sort start-sort))
    (dolist (el elements)
      (when (eq (org-element-type el) 'headline)
        (if (dorg-parse--headline-phantom-p el ctx)
            (setq sort (dorg-parse--process-headlines
                        (org-element-contents el) parent-id sort ctx))
          (cl-incf sort)
          (dorg-parse--process-headline el parent-id sort ctx))))
    sort))

;;; ─── Shared AST Processors ────────────────────────────────────────────

(defun dorg-parse--build-root-heading (ast root-id)
  "Build the synthetic level-0 root `dorg-heading' from AST.
Processes AST's top-level section children into items and returns the
root heading with id ROOT-ID.  Does not push onto any accumulator — the
caller controls placement."
  (let ((items nil))
    (dolist (child (org-element-contents ast))
      (when (eq (org-element-type child) 'section)
        (setq items (nconc items (dorg-parse--process-section child)))))
    (let ((root-h (make-dorg-heading
                   :id root-id
                   :parent-id nil
                   :sort-key (dorg-sortkey-from-integer 1)
                   :level 0
                   :post-blank 0
                   :items items)))
      (dorg-heading-compute-items-hash root-h)
      root-h)))

(defun dorg-parse--process-toplevel-headlines (ast parent-id ctx)
  "Process AST's top-level headlines as children of PARENT-ID.
Each headline is handled by `dorg-parse--process-headline', which pushes
onto CTX's accumulator slots (headings, heading-markers,
new-heading-markers).  The caller supplies the `dorg-parse--ctx'."
  (dorg-parse--process-headlines (org-element-contents ast) parent-id 0 ctx))

;;; Public API

(defun dorg-parse-buffer (&optional buffer root-id)
  "Parse org BUFFER and return a `dorg-parse-result' struct.
If BUFFER is nil, use the current buffer.  ROOT-ID is the id for
the synthetic file root (nil if not yet allocated — caller must
assign before use)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((ctx (dorg-parse--ctx-make
                :heading-markers (make-hash-table :test 'eql)
                :verbatim-regions (dorg-parse--verbatim-regions))))
      ;; Single parse of the whole buffer; both the root section and the
      ;; top-level headlines are processed from this one AST.
      (let ((ast (dorg-parse--parse-accessible)))
        (push (dorg-parse--build-root-heading ast root-id)
              (dorg-parse--ctx-headings ctx))
        (dorg-parse--process-toplevel-headlines ast root-id ctx))
      (make-dorg-parse-result
       :root-id root-id
       :headings (nreverse (dorg-parse--ctx-headings ctx))
       :new-heading-markers (nreverse (dorg-parse--ctx-new-heading-markers ctx))
       :heading-markers (dorg-parse--ctx-heading-markers ctx)))))

;;; ─── Parse-Model Identity Completion ───────────────────────────────

(define-error 'dorg-parse-duplicate-id
  "Parsed document contains duplicate DORG_ID values")

(defun dorg-parse-assert-unique-ids (result)
  "Return RESULT after verifying every non-nil heading id is unique.
Signals `dorg-parse-duplicate-id' with
\(ID FIRST-TITLE SECOND-TITLE) when two parsed headings share ID.
Identity validation lives beside the parse model so import, typed
mutation, and the deprecated reconciliation adapter cannot drift into
different duplicate-id policies."
  (let ((seen (make-hash-table :test 'eql)))
    (dolist (heading (dorg-parse-result-headings result))
      (when-let ((id (dorg-heading-id heading)))
        (when-let ((first (gethash id seen)))
          (signal 'dorg-parse-duplicate-id
                  (list id
                        (dorg-heading-title first)
                        (dorg-heading-title heading))))
        (puthash id heading seen))))
  result)

(defun dorg-parse-assign-ids (result &optional id-generator)
  "Complete heading identities and parent links in parse RESULT.
ID-GENERATOR returns the next BIGINT id and defaults to
`dorg-id-generate'.  Mutates only the heading structs and marker table
owned by RESULT; it never writes a DORG_ID property or other bytes into
the source buffer.

The synthetic root receives RESULT's existing root id, or one newly
allocated id.  Every idless heading receives one id, parent ids are
reconstructed from preorder levels, and duplicate ids fail through
`dorg-parse-assert-unique-ids'.

Returns an alist of (HEADING-ID . BUFFER-MARKER) for callers that still
need explicit legacy writeback.  Administrative import deliberately
ignores this value."
  (let ((gen (or id-generator #'dorg-id-generate)))
    (unless (dorg-parse-result-root-id result)
      (setf (dorg-parse-result-root-id result) (funcall gen)))
    (let ((root-id (dorg-parse-result-root-id result)))
      (let ((root (car (dorg-parse-result-headings result))))
        (when (and root (= (dorg-heading-level root) 0))
          (setf (dorg-heading-id root) root-id)))
      (dolist (heading (dorg-parse-result-headings result))
        (unless (dorg-heading-id heading)
          (setf (dorg-heading-id heading) (funcall gen))))
      (let ((parents (list (cons 0 root-id))))
        (dolist (heading (dorg-parse-result-headings result))
          (let ((level (dorg-heading-level heading)))
            (when (> level 0)
              (while (and parents (>= (caar parents) level))
                (pop parents))
              (when parents
                (setf (dorg-heading-parent-id heading) (cdar parents))))
            (push (cons level (dorg-heading-id heading)) parents)))))
    (dorg-parse-assert-unique-ids result)
    (let ((markers (dorg-parse-result-heading-markers result)))
      (dolist (entry (dorg-parse-result-new-heading-markers result))
        (puthash (dorg-heading-id (car entry)) (cdr entry) markers)))
    (mapcar (lambda (entry)
              (cons (dorg-heading-id (car entry)) (cdr entry)))
            (dorg-parse-result-new-heading-markers result))))

;;; ─── Narrowed Region Helpers ──────────────────────────────────────────

(defun dorg-parse-root-section-region (root-id beg end)
  "Parse the preamble region [BEG, END) into a root `dorg-heading'.
Narrows the current buffer to the region, parses it at fixed `element'
granularity, and returns a level-0 `dorg-heading' with id ROOT-ID built from
the region's section items.  Used for narrowed preamble parses by the
incremental local snapshot builder and future materialized-view code."
  (save-restriction
    (narrow-to-region beg end)
    (dorg-parse--build-root-heading (dorg-parse--parse-accessible) root-id)))

(defun dorg-parse-section-items-at-heading ()
  "Return the parser's `dorg-heading-item' objects for the heading at point.
Narrows from the heading at point to the next heading at any level
\(found via `outline-next-heading'), parses the narrowed region at fixed
`element' granularity, and reuses `dorg-parse--process-section' on the entry's
own `section' node — so the returned items are exactly those the full parser
would attach to that heading, with the same direct-section-child rules for
planning lines, the PROPERTIES drawer, generic and LOGBOOK drawers, keywords,
and text bodies.  Point may be anywhere on or within the heading's entry; the
buffer's restriction and point are restored on exit.  Returns items in
document order, or nil when the entry has no section.

This is the single section-projection path query adapters reuse so that
plain-org timestamp matching cannot drift from the model parser: a fake
`:LOGBOOK:' inside an example/source block is never a drawer node, a
drawer nested in a quote block or list item is a child of that container
rather than of the section, and body timestamps are read from the same
`dorg-text-item' bodies the parser produces."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((beg (point))
         (end (save-excursion (outline-next-heading) (point))))
     (save-restriction
       (narrow-to-region beg end)
       ;; The narrowed region holds a single heading and its own section
       ;; (no child headings — `outline-next-heading' bounds it), so the
       ;; first `section' node found is the entry's.
       (let ((section (org-element-map (dorg-parse--parse-accessible)
                          'section #'identity nil t)))
         (when section
           (dorg-parse--process-section section)))))))

(defun dorg-parse-subtree-region (parent-id beg end)
  "Parse the heading subtree(s) in region [BEG, END) under PARENT-ID.
Narrows the current buffer to the region and parses its top-level
headlines as children of PARENT-ID.  Returns a list
\(HEADINGS HEADING-MARKERS NEW-HEADING-MARKERS) where HEADINGS is the
list of parsed `dorg-heading' structs in buffer order, HEADING-MARKERS is
a hash of existing-id -> marker, and NEW-HEADING-MARKERS is a list of
\(heading . marker) for headings lacking a DORG_ID.  BEG must sit at a
heading start and END at the next same-or-higher-level heading (or
buffer end) so the region holds complete subtrees."
  (save-restriction
    (narrow-to-region beg end)
    (let ((ctx (dorg-parse--ctx-make
                :heading-markers (make-hash-table :test 'eql)
                :verbatim-regions (dorg-parse--verbatim-regions))))
      (dorg-parse--process-toplevel-headlines
       (dorg-parse--parse-accessible) parent-id ctx)
      (list (nreverse (dorg-parse--ctx-headings ctx))
            (dorg-parse--ctx-heading-markers ctx)
            (nreverse (dorg-parse--ctx-new-heading-markers ctx))))))

(provide 'dorg-parse)
;;; dorg-parse.el ends here
