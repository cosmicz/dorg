;;; dorg-model.el --- Domain model for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Canonical domain structs for dorg.  Source-agnostic — populated from
;; either org buffer parsing or DB loading.
;;
;; `dorg-heading' (cl-defstruct) represents a heading with its metadata.
;; `dorg-heading-item' (EIEIO hierarchy) represents polymorphic section
;; content under a heading: text blocks, keywords, planning lines, and
;; drawers.
;;
;; Each item subclass implements `dorg-item-content-string' for
;; deterministic content hashing.  The heading's `items-hash' is the
;; MD5 of all item content strings joined by newlines.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'org-src)                      ; org-{,un}escape-code-in-string
(require 'dorg-id)                      ; `dorg-id-property'
(require 'dorg-sortkey)
(require 'dorg-timestamp)

;;; ─── Verbatim Block Comma-Escape ──────────────────────────────────────
;;
;; A column-0 line that looks like a headline (`*') or keyword/block
;; (`#+') inside a verbatim block body is force-promoted by org's reader —
;; it closes the block and is read as a real headline.  Org's own on-disk
;; remedy is COMMA ESCAPE: such a body line is written `,*' / `,#+', which
;; the reader strips and never promotes.  We apply this on serialize and
;; reverse it on parse so a DOrg model carrying a verbatim block with
;; embedded column-0 lines survives model → render → parse → model intact
;; and idempotently.
;;
;; The escape/unescape pair MUST be deployed together: render-only escaping
;; would double-escape model text across repeated cycles.
;;
;; Block detection mirrors `dorg-parse--verbatim-regions' (case-insensitive,
;; name-matched, optional leading whitespace, FIRST matching `#+end_NAME'
;; closes).  Consequence of first-match-closes: a same-name literal
;; `#+end_NAME' as block CONTENT is indistinguishable from the real
;; delimiter — exactly org's own limitation — and must itself be comma
;; escaped (`,#+end_NAME') to be carried as content.  A differently named
;; close (e.g. `#+end_src' inside `#+begin_example') is unambiguous content
;; and is escaped/unescaped normally.

(defun dorg--map-verbatim-block-bodies (text fn)
  "Return TEXT with FN applied to the BODY of each verbatim block in it.
A verbatim block is a `#+begin_NAME' line and its first matching
`#+end_NAME' line (case-insensitive, optional leading whitespace).  FN
receives two arguments — the body (the substring strictly between the begin
line's trailing newline and the end line's beginning, as a single string)
and the block's downcased open NAME — and returns the body's replacement.
The NAME lets FN recognise a comma-escaped same-name terminator (irreducible
literal content) and preserve it verbatim.  The `#+begin'/`#+end' delimiter
lines and all text outside any block are returned verbatim.  An
unterminated block contributes no body region, so its lines (and
everything after) pass through unchanged.  Newline structure is
preserved exactly."
  (let ((lines (split-string text "\n"))
        (out nil)
        (open-name nil)
        (body nil))
    (dolist (line lines)
      (cond
       ;; Inside an open block, looking for its matching close.
       (open-name
        (if (and (string-match "\\`[ \t]*#\\+end_\\([^ \t\n]+\\)" line)
                 (string= open-name (downcase (match-string 1 line))))
            ;; Matching close: flush the accumulated body through FN (an
            ;; empty body contributes nothing), then emit the close line.
            (progn
              (when body
                (push (funcall fn (mapconcat #'identity (nreverse body) "\n")
                               open-name)
                      out))
              (push line out)
              (setq open-name nil body nil))
          ;; Still body.
          (push line body)))
       ;; Outside a block: does this line open one?
       ((string-match "\\`[ \t]*#\\+begin_\\([^ \t\n]+\\)" line)
        (setq open-name (downcase (match-string 1 line)))
        (setq body nil)
        (push line out))
       ;; Ordinary non-block line.
       (t (push line out))))
    ;; Unterminated block: emit the begin line's body verbatim (no region).
    (when open-name
      (dolist (l (nreverse body)) (push l out)))
    (mapconcat #'identity (nreverse out) "\n")))

(defun dorg--verbatim-same-name-terminator-p (line open-name)
  "Return non-nil if LINE is a comma-escaped same-name close of OPEN-NAME.
Mirrors the close-token boundary of `dorg--map-verbatim-block-bodies' and
`dorg-parse--verbatim-regions': optional indent, one or more commas, then
`#+end_NAME' whose NAME token (case-insensitive) equals OPEN-NAME.  Trailing
content after the name token is ignored, exactly as the close detectors do.
Such a line is irreducible literal content: stripping its last comma would
leave a bare structural close, which the mapper would then mistake for the
real delimiter — so it must pass through escape/unescape verbatim."
  (and (string-match "\\`[ \t]*,+#\\+end_\\([^ \t\n]+\\)" line)
       (string= open-name (downcase (match-string 1 line)))))

(defun dorg--map-verbatim-body-lines (body open-name fn)
  "Apply FN to each line of BODY, except same-name terminators of OPEN-NAME.
FN is a single-line string transform.  Same-name terminators pass through
verbatim so their full comma run is preserved in both directions; every other
line is transformed normally.  Newline structure is preserved exactly."
  (mapconcat (lambda (line)
               (if (dorg--verbatim-same-name-terminator-p line open-name)
                   line
                 (funcall fn line)))
             (split-string body "\n")
             "\n"))

(defun dorg--escape-verbatim-bodies (text)
  "Comma-escape column-0 `*'/`#+' lines inside each verbatim block of TEXT.
Inverse of `dorg--unescape-verbatim-bodies'.  A comma-escaped SAME-name
`#+end_NAME' terminator is preserved verbatim (no added comma) so its marker
depth survives; every other body line is escaped normally.  No-op when TEXT
has no verbatim block or no escapable body line."
  (dorg--map-verbatim-block-bodies
   text (lambda (body open-name)
          (dorg--map-verbatim-body-lines body open-name
                                         #'org-escape-code-in-string))))

(defun dorg--unescape-verbatim-bodies (text)
  "Strip one comma from comma-escaped lines inside each verbatim block of TEXT.
Inverse of `dorg--escape-verbatim-bodies'.  A comma-escaped SAME-name
`#+end_NAME' terminator is preserved verbatim (its full comma run is kept in
the model) so it is never reduced to a bare structural close."
  (dorg--map-verbatim-block-bodies
   text (lambda (body open-name)
          (dorg--map-verbatim-body-lines body open-name
                                         #'org-unescape-code-in-string))))

;;; Heading Struct

(cl-defstruct dorg-heading
  "A heading in the dorg domain model.

- ID: snowflake BIGINT (from DORG_ID or freshly allocated).
- PARENT-ID: parent heading id, or nil for root.
- SORT-KEY: unibyte string position among siblings (VARBINARY).
- LEVEL: org heading level (0 for synthetic root).
- TODO, PRIORITY, COMMENTED, TITLE, POST-BLANK: heading attributes.
- TAGS: list of tag strings in order.
- ITEMS: list of `dorg-heading-item' EIEIO objects.
- ITEMS-HASH: MD5 hex string of structured item content, or nil."
  id parent-id sort-key level
  todo priority commented title post-blank
  tags
  items
  items-hash)

(defun dorg-model-children-of (parent-id headings)
  "Return direct child headings of PARENT-ID from HEADINGS in sort-key order.
HEADINGS is an id -> `dorg-heading' hash table.  A pathological self-parent
row is excluded, and nil sort keys sort as empty strings."
  (let (children)
    (maphash
     (lambda (_id heading)
       (when (and (eql (dorg-heading-parent-id heading) parent-id)
                  (not (eql (dorg-heading-id heading) parent-id)))
         (push heading children)))
     headings)
    (sort children
          (lambda (a b)
            (string< (or (dorg-heading-sort-key a) "")
                     (or (dorg-heading-sort-key b) ""))))))

;;; Item EIEIO Hierarchy

(defclass dorg-heading-item ()
  ((post-blank :initarg :post-blank :initform 0 :accessor dorg-item-post-blank)
   ;; Internal DB identity, used by item-level diffing to reuse existing
   ;; `heading_items' rows instead of wipe+reinsert.  Both
   ;; default nil: parser-created items leave them nil; the DB loader
   ;; populates them from `heading_items.id'/`sort_key'.
   (id :initarg :id :initform nil :accessor dorg-item-id)
   (sort-key :initarg :sort-key :initform nil :accessor dorg-item-sort-key))
  :abstract t
  :documentation "Base class for polymorphic section content under a heading.
Items are ordered by position in the heading's items list.")

(defclass dorg-text-item (dorg-heading-item)
  ((text :initarg :text :accessor dorg-text-item-text))
  :documentation "Plain text block.")

(defclass dorg-keyword-item (dorg-heading-item)
  ((keyword :initarg :keyword :accessor dorg-keyword-item-keyword)
   (value   :initarg :value   :accessor dorg-keyword-item-value))
  :documentation "File/heading keyword (#+KEY: VALUE).")

(defclass dorg-planning-item (dorg-heading-item)
  ((scheduled :initarg :scheduled :initform nil :accessor dorg-planning-item-scheduled)
   (deadline  :initarg :deadline  :initform nil :accessor dorg-planning-item-deadline)
   (closed    :initarg :closed    :initform nil :accessor dorg-planning-item-closed))
  :documentation "Planning line (SCHEDULED/DEADLINE/CLOSED).")

(defclass dorg-drawer-item (dorg-heading-item)
  ((name       :initarg :name       :accessor dorg-drawer-item-name)
   (lines      :initarg :lines      :initform nil :accessor dorg-drawer-item-lines)
   (properties :initarg :properties :initform nil :accessor dorg-drawer-item-properties))
  :documentation "Named drawer.
LINES is a list of raw line strings (for generic drawers).
PROPERTIES is a list of (KEY . VALUE) cons cells (for PROPERTIES drawers).")

;;; Content String Methods

(cl-defgeneric dorg-item-content-string (item)
  "Return a deterministic string representation of ITEM for hashing.
Format must be stable across sessions — changing it invalidates all hashes.")

(cl-defmethod dorg-item-content-string ((item dorg-text-item))
  (format "text:%d:%s" (dorg-item-post-blank item) (dorg-text-item-text item)))

(cl-defmethod dorg-item-content-string ((item dorg-keyword-item))
  (format "keyword:%d:%s:%s"
          (dorg-item-post-blank item)
          (dorg-keyword-item-keyword item)
          (dorg-keyword-item-value item)))

(cl-defmethod dorg-item-content-string ((item dorg-planning-item))
  (format "planning:%d:%s:%s:%s"
          (dorg-item-post-blank item)
          (or (dorg-planning-item-scheduled item) "")
          (or (dorg-planning-item-deadline item) "")
          (or (dorg-planning-item-closed item) "")))

(cl-defmethod dorg-item-content-string ((item dorg-drawer-item))
  (let ((name (dorg-drawer-item-name item))
        (pb (dorg-item-post-blank item)))
    (if (dorg-drawer-item-properties item)
        (format "drawer:%d:%s:%s"
                pb name
                (mapconcat (lambda (kv)
                             (format "%s=%s" (car kv) (cdr kv)))
                           (dorg-drawer-item-properties item)
                           "\n"))
      (format "drawer:%d:%s:%s"
              pb name
              (mapconcat #'identity
                         (dorg-drawer-item-lines item)
                         "\n")))))

;;; DB Row Conversion

(defun dorg-model--json-nil (val)
  "Normalize a JSON value VAL: return nil for :null and :false, VAL otherwise."
  (if (memq val '(:null :false)) nil val))

(defun dorg-model-from-db-row (row &optional tags)
  "Convert a DB ROW alist to a `dorg-heading' struct.
ROW is an alist with keys: id, parent_id, sort_key (hex string),
level, todo_keyword, priority, commented (0/1), title, post_blank,
items_hash.  TAGS is an optional list of tag strings."
  (make-dorg-heading
   :id (alist-get 'id row)
   :parent-id (dorg-model--json-nil (alist-get 'parent_id row))
   :sort-key (dorg-sortkey-from-hex (alist-get 'sort_key row))
   :level (alist-get 'level row)
   :todo (dorg-model--json-nil (alist-get 'todo_keyword row))
   :priority (dorg-model--json-nil (alist-get 'priority row))
   :commented (not (memq (alist-get 'commented row) '(nil 0 :null :false)))
   :title (dorg-model--json-nil (alist-get 'title row))
   :post-blank (alist-get 'post_blank row)
   :tags (or tags nil)
   :items nil
   :items-hash (dorg-model--json-nil (alist-get 'items_hash row))))

;;; Protected keyword ownership (the single DORG_* rule)

(defconst dorg-protected-keyword-prefix "DORG_"
  "Case-insensitive name prefix marking a keyword/property as DOrg-owned.
DORG_* keyword items (e.g. #+DORG_ID:) are buffer-local authoring markers
that DOrg manages itself; they are excluded from item hashing, SQL
generation, item-level diffing, query results, and history restore.  Org
keywords/properties are case-insensitive, so the rule is applied against
the upcased name — see `dorg-protected-keyword-p'.  This constant is THE
single source of the prefix; the Elisp predicate and the SQL LIKE pattern
both derive from it so they cannot drift.")

(defconst dorg-protected-keyword--sql-escape ?!
  "LIKE ESCAPE character used to build `dorg-protected-keyword-sql-like'.
A non-backslash character is chosen so the derived pattern's escape
semantics are explicit in the SQL text rather than relying on the
server's implicit backslash handling.")

(defun dorg-protected-keyword-p (name)
  "Return non-nil when NAME is a DOrg-owned protected keyword/property.
Case-insensitive: NAME is upcased before the prefix test, matching Org's
case-insensitive keyword/property semantics, so a lowercase #+dorg_id: is
owned exactly like #+DORG_ID:.  Returns nil for a non-string NAME.

THE single predicate for the DORG_* ownership rule.  Every DB-bound path
\(item hashing, SQL generation, item diff), the mutation/API validators,
the QL read filter, and history restore delegate here so the rule cannot
drift again (tracked-regression)."
  (and (stringp name)
       (string-prefix-p dorg-protected-keyword-prefix (upcase name))))

(defun dorg-protected-keyword-sql-like ()
  "Return (PATTERN . ESCAPE) implementing `dorg-protected-keyword-p' in SQL.
PATTERN is a LIKE pattern that matches any upcased keyword owned by DOrg;
ESCAPE is the one-character escape string introducing literal LIKE
metacharacters.  Both are derived from `dorg-protected-keyword-prefix':
the prefix is upcased (to be compared against UPPER(col)) and each of its
LIKE metacharacters — the escape char, `%', and `_' — is escaped, then a
trailing `%' wildcard is appended.  Because the pattern is computed from
the one prefix constant, the SQL and Elisp rules cannot drift.

Use case-insensitively, e.g.:
  UPPER(col) NOT LIKE PATTERN ESCAPE ESCAPE"
  (let* ((esc dorg-protected-keyword--sql-escape)
         (escaped (mapconcat
                   (lambda (ch)
                     (if (memq ch (list esc ?% ?_))
                         (string esc ch)
                       (string ch)))
                   (upcase dorg-protected-keyword-prefix)
                   "")))
    (cons (concat escaped "%") (string esc))))

;;; Items Hash

(defun dorg-item--db-local-p (item)
  "Return non-nil if ITEM is buffer-local metadata that never reaches the DB.
DORG_* keyword items (e.g. #+DORG_ID:) are local authoring markers; they
are excluded from item hashing, SQL generation, and item-level diffing so
they create no inserts/updates/deletes or hash drift.  Delegates the name
check to the shared `dorg-protected-keyword-p' so every DB-bound item path
filters identically (and case-insensitively)."
  (and (cl-typep item 'dorg-keyword-item)
       (dorg-protected-keyword-p (dorg-keyword-item-keyword item))))

(defun dorg-heading-compute-items-hash (heading)
  "Compute and set the items-hash for HEADING from its items list.
Skips DORG_* keyword items since they are buffer-local metadata
that never reaches the DB.  Returns the MD5 hex string, or nil
if no DB-bound items exist."
  (let ((items (cl-remove-if #'dorg-item--db-local-p
                             (dorg-heading-items heading))))
    (if (null items)
        (progn (setf (dorg-heading-items-hash heading) nil) nil)
      (let ((hash (md5 (mapconcat #'dorg-item-content-string items "\n"))))
        (setf (dorg-heading-items-hash heading) hash)
        hash))))

;;; Timestamp Accessors

;; Heading-level projections over the structured timestamp parser in
;; `dorg-timestamp'.  These expose existing model data without mutating
;; any `dorg-heading' slot, planning raw field, text item, or drawer
;; property.  They are pure: no DB, buffer, or render dependencies.

(defun dorg-heading--planning-timestamp (heading accessor)
  "Return the first parseable planning timestamp from HEADING via ACCESSOR.
ACCESSOR is a `dorg-planning-item' field accessor (scheduled,
deadline, or closed).  Walk `dorg-heading-items' in order, parse the
first non-nil raw value that ACCESSOR yields with
`dorg-timestamp-parse', and return that `dorg-timestamp'.  Return nil
when no planning item carries a parseable value for that field."
  (catch 'found
    (dolist (item (dorg-heading-items heading))
      (when (cl-typep item 'dorg-planning-item)
        (let ((raw (funcall accessor item)))
          (when raw
            (let ((ts (dorg-timestamp-parse raw)))
              (when ts (throw 'found ts)))))))
    nil))

(defun dorg-heading-scheduled-timestamp (heading)
  "Return HEADING's SCHEDULED timestamp as a `dorg-timestamp', or nil.
Parses the first parseable planning SCHEDULED raw string found in
`dorg-heading-items'.  Returns nil when absent or unparsable."
  (dorg-heading--planning-timestamp heading #'dorg-planning-item-scheduled))

(defun dorg-heading-deadline-timestamp (heading)
  "Return HEADING's DEADLINE timestamp as a `dorg-timestamp', or nil.
Parses the first parseable planning DEADLINE raw string found in
`dorg-heading-items'.  Returns nil when absent or unparsable."
  (dorg-heading--planning-timestamp heading #'dorg-planning-item-deadline))

(defun dorg-heading-closed-timestamp (heading)
  "Return HEADING's CLOSED timestamp as a `dorg-timestamp', or nil.
Parses the first parseable planning CLOSED raw string found in
`dorg-heading-items'.  Returns nil when absent or unparsable."
  (dorg-heading--planning-timestamp heading #'dorg-planning-item-closed))

(defun dorg-heading--scan-timestamps (text open-char want-active)
  "Return `dorg-timestamp' values in TEXT matching one delimiter family.
Scan single tokens and `--' ranges, consuming each range before its
endpoints so a range yields one `dorg-timestamp' with `end-date'
populated.  OPEN-CHAR is the opening delimiter to accept (?< for
active, ?\\[ for inactive); only candidates whose first and, when
present, second endpoints use OPEN-CHAR are considered, so a mixed
range is consumed and discarded rather than leaking an endpoint back
in as a single.  Each accepted candidate is validated through
`dorg-timestamp-parse' and kept only when its `dorg-timestamp-active'
matches WANT-ACTIVE.  Returns matches in document order."
  (let ((pos 0)
        (re (concat "\\(<[^<>\n]*>\\|\\[[^][\n]*\\]\\)"
                    "\\(?:--\\(<[^<>\n]*>\\|\\[[^][\n]*\\]\\)\\)?"))
        (out nil))
    (while (string-match re text pos)
      (let ((whole (match-string 0 text))
            (first (match-string 1 text))
            (second (match-string 2 text)))
        (setq pos (match-end 0))
        (when (and (eq (aref first 0) open-char)
                   (or (null second) (eq (aref second 0) open-char)))
          (let ((ts (dorg-timestamp-parse whole)))
            (when (and ts (eq (and (dorg-timestamp-active ts) t) want-active))
              (push ts out))))))
    (nreverse out)))

(defun dorg-heading--scan-active-timestamps (text)
  "Return active `dorg-timestamp' values found in TEXT, in document order.
Scans active `<...>' tokens and active `<...>--<...>' ranges.  Inactive
`[...]' tokens, mixed active/inactive ranges, and malformed
timestamp-looking text are excluded.  See `dorg-heading--scan-timestamps'."
  (dorg-heading--scan-timestamps text ?< t))

(defun dorg-heading--scan-inactive-timestamps (text)
  "Return inactive `dorg-timestamp' values found in TEXT, in document order.
Scans inactive `[...]' tokens and inactive `[...]--[...]' ranges.  Active
`<...>' tokens, mixed active/inactive ranges, and malformed
timestamp-looking text are excluded.  See `dorg-heading--scan-timestamps'."
  (dorg-heading--scan-timestamps text ?\[ nil))

(defun dorg-heading-active-timestamps (heading)
  "Return active timestamps in HEADING's text bodies, in document order.
Scans only `dorg-text-item' bodies in `dorg-heading-items' order,
returning a list of active `dorg-timestamp' values.  Active date
ranges accepted by `dorg-timestamp-parse' yield a single timestamp.
Inactive `[...]' timestamps, planning lines, drawers, keywords, and
heading titles are not scanned.  Returns nil when none are present."
  (let ((out nil))
    (dolist (item (dorg-heading-items heading))
      (when (cl-typep item 'dorg-text-item)
        (setq out (nconc out (dorg-heading--scan-active-timestamps
                              (dorg-text-item-text item))))))
    out))

(defun dorg-heading-property-timestamps (heading)
  "Return parseable PROPERTIES drawer timestamps in HEADING, in order.
Scans only `dorg-drawer-item' objects named PROPERTIES, parsing each
property value with `dorg-timestamp-parse'.  Returns a list of plists
shaped (:property NAME :timestamp TS), preserving drawer and property
order.  Both active and inactive parseable values are included since
property values are explicit metadata.  Unparsable values are skipped
without signaling.  Returns nil when none are present."
  (let ((out nil))
    (dolist (item (dorg-heading-items heading))
      (when (and (cl-typep item 'dorg-drawer-item)
                 (equal (dorg-drawer-item-name item) "PROPERTIES"))
        (dolist (kv (dorg-drawer-item-properties item))
          (let ((ts (dorg-timestamp-parse (cdr kv))))
            (when ts
              (push (list :property (car kv) :timestamp ts) out))))))
    (nreverse out)))

;;; LOGBOOK Activity

;; Model-level projection of LOGBOOK drawer lines into structured
;; activity entries (state changes, notes, clock entries, and generic
;; logged events).  Built on the inactive-timestamp scanner and
;; `dorg-timestamp-parse'; additive and pure — no parser, render,
;; schema, or sync changes, and no mutation of `dorg-heading', drawer
;; lines, or item hashes.
;;
;; Round-trip contract.  LOGBOOK is intentionally STORED as the raw
;; drawer lines of a generic `dorg-drawer-item' and RENDERED verbatim
;; (`:LOGBOOK:' + lines joined by newlines + `:END:'); see
;; `dorg-model--render-item'.  Raw storage is canonical for fidelity: it
;; guarantees a byte-exact round-trip across every heterogeneous LOGBOOK
;; form (state changes, notes with multi-line bodies, CLOCK lines with
;; `=> H:MM' summaries, custom drawer text) without a parse->struct->
;; render pipeline that could silently corrupt content on the next save.
;; The structured interpretation below is a PURE, NON-RENDERING
;; projection layered on top of that raw storage; it never participates
;; in rendering and so cannot affect round-trip fidelity.  (Blank lines
;; inside generic drawers now round-trip byte-exact: the parser preserves
;; interior and trailing blanks, dropping only the trailing :END: newline
;; split artifact.)

(cl-defstruct (dorg-logbook-entry
               (:constructor dorg-logbook-entry--create)
               (:copier nil)
               (:predicate dorg-logbook-entry-p))
  "A structured LOGBOOK activity entry projected from a drawer line.

Construct only via `dorg-heading-logbook-entries'.  All slots are
`:read-only'.

- KIND: `state-change', `note', `clock', or `logbook' (generic
  fallback).
- TIMESTAMP: the entry's inactive `dorg-timestamp'.
- RAW: the full LOGBOOK drawer line the entry was projected from.
- FROM-STATE, TO-STATE: state names for `state-change' entries, nil
  otherwise (FROM-STATE is also nil when the line omits a `from'
  clause).
- DURATION: the `H:MM' effort string from a `clock' line's trailing
  `=> H:MM' summary, nil for a running clock (no summary) and for every
  non-clock kind.
- BODY: for a `note' entry, the free-text continuation lines that
  follow the `- Note taken on [..] \\' header, joined by newlines and
  stored VERBATIM (original drawer indentation preserved, consistent
  with RAW).  nil for a note with no continuation lines and for every
  non-note kind.  Body lines are folded into the entry rather than
  projected as their own entries; collection stops at the next sibling
  LOGBOOK entry header (`- ' bullet or `CLOCK:') at indentation no
  deeper than the note header (see `dorg-heading-logbook-entries')."
  (kind nil :read-only t)
  (timestamp nil :read-only t)
  (raw nil :read-only t)
  (from-state nil :read-only t)
  (to-state nil :read-only t)
  (duration nil :read-only t)
  (body nil :read-only t))

(defun dorg-heading--classify-logbook-line (line)
  "Classify LOGBOOK drawer LINE for activity projection.
Return a plist (:kind KIND :from-state FROM :to-state TO :duration DUR).
Matching is anchored to Org's log forms, tolerating only optional
leading whitespace since raw drawer lines are not trimmed:

- `- State \"TO\" from \"FROM\" [DATE]' yields `state-change' with TO and
  FROM captured (FROM nil when the `from' clause is absent).
- `- Note taken on [DATE]' yields `note'.
- `CLOCK: [START]--[END] => H:MM' yields `clock' with DUR set to the
  trailing `H:MM' summary (nil for a running clock with no summary).
- Any other line classifies as the generic `logbook' fallback.

FROM-STATE/TO-STATE are nil for every kind but `state-change'; DURATION
is nil for every kind but `clock'.  Classification ignores the
timestamp itself; the caller pairs each parsed inactive timestamp on the
line with this classification."
  (cond
   ((string-match
     "\\`[ \t]*- State[ \t]+\"\\([^\"]*\\)\"\\(?:[ \t]+from[ \t]+\"\\([^\"]*\\)\"\\)?"
     line)
    (list :kind 'state-change
          :to-state (match-string 1 line)
          :from-state (match-string 2 line)))
   ((string-match "\\`[ \t]*- Note taken on[ \t]" line)
    (list :kind 'note :from-state nil :to-state nil))
   ((string-match "\\`[ \t]*CLOCK:" line)
    (list :kind 'clock :from-state nil :to-state nil
          :duration (when (string-match
                           "=>[ \t]*\\([0-9]+:[0-9]\\{2\\}\\)" line)
                      (match-string 1 line))))
   (t (list :kind 'logbook :from-state nil :to-state nil))))

(defun dorg-heading--logbook-line-indent (line)
  "Return the leading-whitespace width (spaces/tabs) of LINE."
  (if (string-match "\\`[ \t]*" line)
      (match-end 0)
    0))

(defun dorg-heading--logbook-entry-header-p (line)
  "Non-nil when LINE begins a LOGBOOK entry: a `- ' bullet or `CLOCK:'.
Anchored to the start of LINE after optional leading whitespace, matching
the forms `dorg-heading--classify-logbook-line' treats as their own
entries.  Used to delimit a note's continuation body."
  (string-match "\\`[ \t]*\\(- \\|CLOCK:\\)" line))

(defun dorg-heading--logbook-note-body (lines start header-indent)
  "Collect a note's continuation body from LINES beginning at index START.
HEADER-INDENT is the leading-whitespace width of the note header line.  A
line ends the body when it begins a new sibling entry (a `- ' bullet or
`CLOCK:') at indentation no deeper than HEADER-INDENT; an entry-header-like
line indented MORE than the header (e.g. a `  - sub' bullet inside the
note) stays in the body.  Returns a cons (BODY . NEXT) where BODY is the
collected lines joined by newlines verbatim, or nil when none, and NEXT is
the index of the first unconsumed line."
  (let ((i start)
        (n (length lines))
        (body nil))
    (while (and (< i n)
                (let ((line (nth i lines)))
                  (not (and (dorg-heading--logbook-entry-header-p line)
                            (<= (dorg-heading--logbook-line-indent line)
                                header-indent)))))
      (push (nth i lines) body)
      (setq i (1+ i)))
    (cons (when body (mapconcat #'identity (nreverse body) "\n")) i)))

(defun dorg-heading-logbook-entries (heading)
  "Return structured LOGBOOK activity entries for HEADING, in document order.
Scans only `dorg-drawer-item' objects named LOGBOOK in
`dorg-heading-items' order, then each drawer line in order.  Every
inactive timestamp token on a line (including inactive `[...]--[...]'
ranges) becomes one `dorg-logbook-entry' in textual order, sharing that
line's classification and full raw text.  Active timestamps, mixed
active/inactive ranges, malformed timestamp-looking text, and lines with
no parseable inactive timestamp are skipped without signaling.  Pure:
does not mutate HEADING, drawer lines, item hashes, or rendered output.
Returns nil when no LOGBOOK activity is present."
  (let ((out nil))
    (dolist (item (dorg-heading-items heading))
      (when (and (cl-typep item 'dorg-drawer-item)
                 (equal (dorg-drawer-item-name item) "LOGBOOK"))
        (let* ((lines (dorg-drawer-item-lines item))
               (n (length lines))
               (i 0))
          (while (< i n)
            (let* ((line (nth i lines))
                   (cls (dorg-heading--classify-logbook-line line))
                   (kind (plist-get cls :kind))
                   (tss (dorg-heading--scan-inactive-timestamps line))
                   (body nil))
              ;; A note header folds its following continuation lines into
              ;; the entry body; advance past them so they are not
              ;; projected as their own entries.
              (when (eq kind 'note)
                (let ((bn (dorg-heading--logbook-note-body
                           lines (1+ i)
                           (dorg-heading--logbook-line-indent line))))
                  (setq body (car bn)
                        i (1- (cdr bn)))))
              (when tss
                (dolist (ts tss)
                  (push (dorg-logbook-entry--create
                         :kind kind
                         :timestamp ts
                         :raw line
                         :from-state (plist-get cls :from-state)
                         :to-state (plist-get cls :to-state)
                         :duration (plist-get cls :duration)
                         :body body)
                        out)))
              (setq i (1+ i)))))))
    (nreverse out)))

;;; Deep Copy

(cl-defgeneric dorg-item-deep-copy (item)
  "Return a deep copy of ITEM with all strings and structure freshly allocated.")

(defun dorg-item--copy-sort-key (item)
  "Return a freshly allocated copy of ITEM's DB sort key, or nil.
Keeps deep copies and snapshots from aliasing the live key string."
  (when (dorg-item-sort-key item)
    (copy-sequence (dorg-item-sort-key item))))

(cl-defmethod dorg-item-deep-copy ((item dorg-text-item))
  (dorg-text-item :post-blank (dorg-item-post-blank item)
                  :id (dorg-item-id item)
                  :sort-key (dorg-item--copy-sort-key item)
                  :text (copy-sequence (dorg-text-item-text item))))

(cl-defmethod dorg-item-deep-copy ((item dorg-keyword-item))
  (dorg-keyword-item :post-blank (dorg-item-post-blank item)
                     :id (dorg-item-id item)
                     :sort-key (dorg-item--copy-sort-key item)
                     :keyword (copy-sequence (dorg-keyword-item-keyword item))
                     :value (copy-sequence (dorg-keyword-item-value item))))

(cl-defmethod dorg-item-deep-copy ((item dorg-planning-item))
  (dorg-planning-item
   :post-blank (dorg-item-post-blank item)
   :id (dorg-item-id item)
   :sort-key (dorg-item--copy-sort-key item)
   :scheduled (when (dorg-planning-item-scheduled item)
                (copy-sequence (dorg-planning-item-scheduled item)))
   :deadline (when (dorg-planning-item-deadline item)
               (copy-sequence (dorg-planning-item-deadline item)))
   :closed (when (dorg-planning-item-closed item)
             (copy-sequence (dorg-planning-item-closed item)))))

(cl-defmethod dorg-item-deep-copy ((item dorg-drawer-item))
  (dorg-drawer-item
   :post-blank (dorg-item-post-blank item)
   :id (dorg-item-id item)
   :sort-key (dorg-item--copy-sort-key item)
   :name (copy-sequence (dorg-drawer-item-name item))
   :lines (mapcar #'copy-sequence (dorg-drawer-item-lines item))
   :properties (mapcar (lambda (kv)
                         (cons (copy-sequence (car kv))
                               (copy-sequence (cdr kv))))
                       (dorg-drawer-item-properties item))))

(defun dorg-heading-deep-copy (heading)
  "Return a deep copy of HEADING with all nested structure freshly allocated.
Copies tags list (with string copies), items list (with EIEIO clones),
sort-key, title, and all string-bearing fields.  Guarantees that mutating
any field of the copy cannot reach back into the original."
  (make-dorg-heading
   :id (dorg-heading-id heading)
   :parent-id (dorg-heading-parent-id heading)
   :sort-key (when (dorg-heading-sort-key heading)
               (copy-sequence (dorg-heading-sort-key heading)))
   :level (dorg-heading-level heading)
   :todo (when (dorg-heading-todo heading)
           (copy-sequence (dorg-heading-todo heading)))
   :priority (when (dorg-heading-priority heading)
               (copy-sequence (dorg-heading-priority heading)))
   :commented (dorg-heading-commented heading)
   :title (when (dorg-heading-title heading)
            (copy-sequence (dorg-heading-title heading)))
   :post-blank (dorg-heading-post-blank heading)
   :tags (mapcar #'copy-sequence (dorg-heading-tags heading))
   :items (mapcar #'dorg-item-deep-copy (dorg-heading-items heading))
   :items-hash (when (dorg-heading-items-hash heading)
                 (copy-sequence (dorg-heading-items-hash heading)))))

;;; Render Methods

(cl-defgeneric dorg-model--render-item (item &optional dorg-id)
  "Render ITEM as org text string.
DORG-ID is passed only for a heading's local identity drawer (the
property-typed PROPERTIES drawer chosen by `dorg-model--local-identity-drawer')
and injects its `:DORG_ID:'.  A name alone never makes a drawer identity:
a generic raw-lines drawer named PROPERTIES is rendered verbatim.")

(cl-defmethod dorg-model--render-item ((item dorg-text-item) &optional _dorg-id)
  ;; Comma-escape column-0 `*'/`#+' lines inside any verbatim block body so
  ;; org will not force-close/split the block on the next read.
  ;; Paired with `dorg--unescape-verbatim-bodies' on parse.
  (dorg--escape-verbatim-bodies (dorg-text-item-text item)))

(cl-defmethod dorg-model--render-item ((item dorg-keyword-item) &optional _dorg-id)
  (format "#+%s: %s"
          (dorg-keyword-item-keyword item)
          (dorg-keyword-item-value item)))

(cl-defmethod dorg-model--render-item ((item dorg-planning-item) &optional _dorg-id)
  (let ((parts nil))
    (when-let ((s (dorg-planning-item-scheduled item)))
      (push (format "SCHEDULED: %s" s) parts))
    (when-let ((d (dorg-planning-item-deadline item)))
      (push (format "DEADLINE: %s" d) parts))
    (when-let ((c (dorg-planning-item-closed item)))
      (push (format "CLOSED: %s" c) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(cl-defmethod dorg-model--render-item ((item dorg-drawer-item) &optional dorg-id)
  (let ((name (dorg-drawer-item-name item)))
    ;; Render as a key/value PROPERTIES drawer only when the item actually
    ;; carries properties, or when a heading identity is being injected into
    ;; it (its local identity drawer).  A drawer NAMED "PROPERTIES" that
    ;; carries raw LINES instead — a body `:PROPERTIES:' block org parsed as
    ;; generic drawer content, never a heading's local identity drawer —
    ;; renders its lines verbatim so its content (including any literal
    ;; `:DORG_ID:') survives.  DORG-ID is only ever passed for the local
    ;; identity drawer, which is always property-typed.
    (if (and (equal name "PROPERTIES")
             (or (dorg-drawer-item-properties item) dorg-id))
        (let ((lines (mapcar (lambda (kv)
                               (format ":%s: %s" (car kv) (cdr kv)))
                             (dorg-drawer-item-properties item))))
          (when dorg-id
            (setq lines (cons (format ":%s: %s" dorg-id-property dorg-id) lines)))
          (concat ":PROPERTIES:\n"
                  (mapconcat #'identity lines "\n")
                  (if lines "\n" "")
                  ":END:"))
      (let ((lines (dorg-drawer-item-lines item)))
        (concat ":" name ":\n"
                (mapconcat #'identity lines "\n")
                (if lines "\n" "")
                ":END:")))))

;;; Heading Property Snapshot

(defun dorg-model-heading-property-snapshot (heading)
  "Return HEADING's PROPERTIES as a domain-neutral (KEY . VALUE) alist.
Collects every PROPERTIES drawer entry except `DORG_ID' (heading identity,
not a domain property).  Used as the model-neutral snapshot a placement
invariant classifies against (see `dorg-ext-classify-placement'); DOrg
attaches no meaning to any key."
  (let ((props nil))
    (dolist (item (dorg-heading-items heading))
      (when (and (cl-typep item 'dorg-drawer-item)
                 (equal (dorg-drawer-item-name item) "PROPERTIES"))
        (dolist (kv (dorg-drawer-item-properties item))
          (unless (equal (car kv) dorg-id-property)
            (push kv props)))))
    (nreverse props)))

;;; Heading Render

(defun dorg-model--render-heading (heading)
  "Return the org heading line string for HEADING (no trailing newline)."
  (let* ((level (dorg-heading-level heading))
         (stars (make-string level ?*))
         (parts (list stars)))
    (when-let ((todo (dorg-heading-todo heading)))
      (push todo parts))
    (when-let ((pri (dorg-heading-priority heading)))
      (push (format "[#%s]" pri) parts))
    (when (dorg-heading-commented heading)
      (push "COMMENT" parts))
    (when-let ((title (dorg-heading-title heading)))
      (push title parts))
    (let ((line (mapconcat #'identity (nreverse parts) " ")))
      (if-let ((tags (dorg-heading-tags heading)))
          (format "%s  :%s:" line (mapconcat #'identity tags ":"))
        line))))

;;; Topology / Reachability

;; A heading tree is well-formed when every heading's `parent-id' chain
;; terminates at the declared level-0 root.  `dorg-model-render-tree'
;; only renders headings reachable from that root, so a disconnected
;; island or a parent cycle is silently omitted at render time.  The
;; pure classifier below lets both the renderer and `dorg-doctor' detect
;; and report such rows instead of dropping them quietly.

(defun dorg-model-heading-reachability (id parents root-id)
  "Classify heading ID's parent-chain reachability toward ROOT-ID.
PARENTS is a hash table mapping each heading id to its parent id (nil
when there is no parent); a key's mere presence means that heading
exists.  ROOT-ID is the id of the declared level-0 root the chain is
expected to reach.

Walks the `parent-id' chain from ID with a visited set and returns one
of these symbols:

  `reachable'        the chain reaches ROOT-ID.
  `dangling-parent'  the chain hits a parent id absent from PARENTS.
  `cycle'            the chain revisits a heading before reaching root.
  `disconnected'     the chain ends at a nil parent on a heading other
                     than ROOT-ID (e.g. a separate or cross-root tree).

ID itself must be a key in PARENTS.  The walk is bounded by the visited
set, so it always terminates."
  (let ((seen (make-hash-table :test 'eql))
        (cur id))
    (catch 'done
      (while t
        (cond
         ((eql cur root-id) (throw 'done 'reachable))
         ((gethash cur seen) (throw 'done 'cycle))
         (t
          (puthash cur t seen)
          (let ((pid (gethash cur parents)))
            (cond
             ((null pid) (throw 'done 'disconnected))
             ((eq (gethash pid parents 'dorg--absent) 'dorg--absent)
              (throw 'done 'dangling-parent))
             (t (setq cur pid))))))))))

;;; Tree Render

(defun dorg-model-render-tree (headings)
  "Render HEADINGS as org text string.
HEADINGS is a list of `dorg-heading' structs with populated items.
Root heading (level 0) emits its items but not a heading line.
Each non-root heading's :DORG_ID: is injected into its local identity
drawer (see `dorg-model--insert-heading-section')."
  (let ((by-parent (make-hash-table :test 'eql))
        (root nil))
    (dolist (h headings)
      (if (= (dorg-heading-level h) 0)
          (setq root h)
        (let ((pid (dorg-heading-parent-id h)))
          (puthash pid (append (gethash pid by-parent) (list h))
                  by-parent))))
    ;; Sort children by sort-key
    (maphash (lambda (k v)
               (puthash k (sort v (lambda (a b)
                                    (string< (dorg-heading-sort-key a)
                                             (dorg-heading-sort-key b))))
                        by-parent))
             by-parent)
    (with-temp-buffer
      (when root
        (dorg-model--render-subtree root by-parent (current-buffer)))
      (buffer-string))))

(defun dorg-model-render-heading-section (heading)
  "Render HEADING's line + items as org text (no children, no post-blank).
A non-root heading's :DORG_ID: is injected into its local identity drawer
\(see `dorg-model--insert-heading-section').
Root heading (level 0) emits only its items.
Returns a string."
  (with-temp-buffer
    (dorg-model--insert-heading-section heading (current-buffer))
    (buffer-string)))

(defun dorg-model--property-drawer-item-p (item)
  "Return non-nil when ITEM is a property-typed PROPERTIES drawer.
Distinguishes a heading's key/value property drawer from a generic drawer
org parsed into raw `lines' — even one NAMED \"PROPERTIES\" (a body
`:PROPERTIES:' block appearing after content).  A property-typed drawer
carries `properties' rather than `lines'; only such a drawer is a heading's
local identity drawer and may receive an injected `:DORG_ID:'.  The parser
and the store keep the two forms mutually exclusive (properties xor lines),
so a drawer holding raw lines is never a property drawer regardless of its
name."
  (and (cl-typep item 'dorg-drawer-item)
       (equal (dorg-drawer-item-name item) "PROPERTIES")
       (null (dorg-drawer-item-lines item))))

(defun dorg-model--local-identity-drawer (items)
  "Return the local identity PROPERTIES drawer among ITEMS, or nil.
The local identity drawer is the property-typed PROPERTIES `dorg-drawer-item'
\(see `dorg-model--property-drawer-item-p') occurring before any non-planning
body item, mirroring org's own rule that a property drawer is recognized
only immediately after the heading and its optional planning line.  Planning
items may precede it.  A PROPERTIES-shaped drawer that appears only after
some non-planning body item — or that carries raw lines (generic body
`:PROPERTIES:' content) — is domain content, not heading identity: it is not
returned, so it never receives an injected `:DORG_ID:' and is never
rewritten by property ops."
  (catch 'found
    (dolist (item items)
      (cond
       ;; Planning may precede the local drawer — keep scanning.
       ((cl-typep item 'dorg-planning-item))
       ;; First non-planning item: it is the local drawer iff it is a
       ;; property-typed PROPERTIES drawer.  A generic raw-lines drawer
       ;; (including a body `:PROPERTIES:' block) or any other item means the
       ;; heading has no local identity drawer.
       ((dorg-model--property-drawer-item-p item)
        (throw 'found item))
       (t (throw 'found nil))))
    nil))

(defun dorg-model-identity-drawer-string (id)
  "Return the local identity PROPERTIES drawer text carrying `:DORG_ID:' ID.
The single source for the synthetic drawer shape emitted by the renderer
and by the conflict-block placeholders; the drawer key tracks
`dorg-id-property' so all four callers stay in lock-step."
  (format ":PROPERTIES:\n:%s: %d\n:END:\n" dorg-id-property id))

(defun dorg-model--insert-heading-section (heading out)
  "Insert HEADING's line + items into buffer OUT.
A non-root heading's `:DORG_ID:' is rendered into its local identity
PROPERTIES drawer (see `dorg-model--local-identity-drawer').  When the
heading has no local identity drawer, a synthetic one is emitted
immediately after the heading and its planning line — before the first
body item — so org reads it back as the heading's property drawer.  A
PROPERTIES drawer appearing later in the body is treated as domain content
and never receives an injected identity.  Does not emit post-blank or
recurse into children."
  (let* ((hid (dorg-heading-id heading))
         (level (dorg-heading-level heading))
         (is-root (= level 0))
         (items (dorg-heading-items heading))
         (local-drawer (and (not is-root)
                            (dorg-model--local-identity-drawer items))))
    ;; Heading line (not for root)
    (unless is-root
      (with-current-buffer out
        (insert (dorg-model--render-heading heading) "\n")))
    ;; Items, injecting the identity into the local drawer only.
    (let ((needs-synth (and (not is-root) (null local-drawer))))
      (dolist (item items)
        (let* ((is-planning (cl-typep item 'dorg-planning-item))
               (inject-id (and (eq item local-drawer) hid)))
          ;; Inject synthetic PROPERTIES before first non-planning item
          (when (and needs-synth (not is-planning))
            (with-current-buffer out
              (insert (dorg-model-identity-drawer-string hid)))
            (setq needs-synth nil))
          (let ((text (dorg-model--render-item item inject-id)))
            (when text
              (with-current-buffer out
                (insert text)
                (unless (string-suffix-p "\n" text)
                  (insert "\n"))
                (dotimes (_ (dorg-item-post-blank item))
                  (insert "\n")))))))
      ;; Still need synth drawer (no items or only planning)
      (when needs-synth
        (with-current-buffer out
          (insert (dorg-model-identity-drawer-string hid)))))))

(defun dorg-model--insert-post-blank (heading out)
  "Insert HEADING's post-blank newlines into buffer OUT."
  (unless (= (dorg-heading-level heading) 0)
    (let ((pb (or (dorg-heading-post-blank heading) 0)))
      (when (> pb 0)
        (with-current-buffer out
          (dotimes (_ pb)
            (insert "\n")))))))

(defun dorg-model--render-subtree (heading by-parent out)
  "Render HEADING and its subtree into buffer OUT.
BY-PARENT is a hash of parent-id → sorted children list."
  (dorg-model--insert-heading-section heading out)
  ;; Recurse into children
  (dolist (child (gethash (dorg-heading-id heading) by-parent))
    (dorg-model--render-subtree child by-parent out))
  ;; Post-blank after entire subtree (not for root)
  (dorg-model--insert-post-blank heading out))

;;; Item change detection

(defun dorg-model-items-changed-p (a b)
  "Return non-nil if items differ between headings A and B.
Compares items-hash fields.  nil hash means unknown — treated as changed."
  (let ((ha (dorg-heading-items-hash a))
        (hb (dorg-heading-items-hash b)))
    (cond
     ;; Both nil (no items on either side) → unchanged
     ((and (null ha) (null hb)) nil)
     ;; Either nil → changed
     ((or (null ha) (null hb)) t)
     ;; Compare hashes
     (t (not (equal ha hb))))))

;;; Item-Level Plan
;;
;; Reconcile a heading's OLD DB-bound items (carrying
;; `dorg-item-id'/`dorg-item-sort-key') with NEW parsed items so that
;; unchanged or merely edited items reuse their existing
;; `heading_items.id' instead of the wipe+reinsert churn.  The planner is
;; pure: no SQL, no I/O.  SQL lowering (`dorg-sql-gen.el') turns the ops
;; into statements.

(cl-defgeneric dorg-model--item-match-key (item)
  "Return a post-blank-independent identity key for matching ITEM across edits.
Two items with the same key are \"the same item\" for phase-1 matching even
if their `post-blank' (or, for planning, their dates) differ — so a reorder
that shifts trailing blank lines keeps each item bound to its DB row.  This
key governs row reuse only; exact persisted detail equality is decided by
`dorg-model--item-semantic-equal-p'.")

(cl-defmethod dorg-model--item-match-key ((item dorg-text-item))
  ;; Trailing whitespace is position-dependent parser noise (the last
  ;; paragraph keeps its line terminator, earlier ones are trimmed) and
  ;; the renderer re-normalizes it, so it must not split a paragraph's
  ;; row identity across a reorder.  Exact `text_raw' equality separately
  ;; determines whether the reused row needs a detail UPDATE.
  (concat "text:" (string-trim-right (dorg-text-item-text item))))

(cl-defmethod dorg-model--item-match-key ((item dorg-keyword-item))
  (concat "keyword:" (dorg-keyword-item-keyword item)
          ":" (dorg-keyword-item-value item)))

(cl-defmethod dorg-model--item-match-key ((_item dorg-planning-item))
  ;; A heading has at most one planning line, so all planning items share
  ;; one key and the old/new singletons always pair (then update on edit).
  "planning")

(cl-defmethod dorg-model--item-match-key ((item dorg-drawer-item))
  (concat "drawer:" (dorg-drawer-item-name item) ":"
          (if (dorg-drawer-item-properties item)
              (mapconcat (lambda (kv) (format "%s=%s" (car kv) (cdr kv)))
                         (dorg-drawer-item-properties item) "\n")
            (mapconcat #'identity (dorg-drawer-item-lines item) "\n"))))

(defun dorg-model--item-semantic-equal-p (old new)
  "Return non-nil if OLD and NEW have identical persisted content.
Ignores `post-blank', which the item-row lowerer handles separately.  Text
detail equality is exact: trailing-whitespace normalization belongs only to
the row-identity match key.  A matched pair is `:keep' — needing no detail-row
rewrite — exactly when updating its detail table would be a no-op."
  (and (eq (eieio-object-class old) (eieio-object-class new))
       (cl-etypecase new
         (dorg-text-item
          (equal (dorg-text-item-text old)
                 (dorg-text-item-text new)))
         (dorg-keyword-item
          (and (equal (dorg-keyword-item-keyword old)
                      (dorg-keyword-item-keyword new))
               (equal (dorg-keyword-item-value old)
                      (dorg-keyword-item-value new))))
         (dorg-planning-item
          (and (equal (dorg-planning-item-scheduled old)
                      (dorg-planning-item-scheduled new))
               (equal (dorg-planning-item-deadline old)
                      (dorg-planning-item-deadline new))
               (equal (dorg-planning-item-closed old)
                      (dorg-planning-item-closed new))))
         (dorg-drawer-item
          (and (equal (dorg-drawer-item-name old) (dorg-drawer-item-name new))
               (equal (dorg-drawer-item-lines old) (dorg-drawer-item-lines new))
               (equal (dorg-drawer-item-properties old)
                      (dorg-drawer-item-properties new)))))))

(defun dorg-model--items-compatible-p (old new)
  "Return non-nil if OLD can be reused in place as NEW (same DB row).
Compatibility is by item kind, plus name identity for keyword and drawer
items; planning items pair as the heading's singleton planning line.
Incompatible kind or name changes become delete+insert for that item."
  (cond
   ((and (cl-typep old 'dorg-text-item) (cl-typep new 'dorg-text-item)) t)
   ((and (cl-typep old 'dorg-keyword-item) (cl-typep new 'dorg-keyword-item))
    (equal (dorg-keyword-item-keyword old) (dorg-keyword-item-keyword new)))
   ((and (cl-typep old 'dorg-planning-item) (cl-typep new 'dorg-planning-item)) t)
   ((and (cl-typep old 'dorg-drawer-item) (cl-typep new 'dorg-drawer-item))
    (equal (dorg-drawer-item-name old) (dorg-drawer-item-name new)))
   (t nil)))

(defun dorg-model--plan-items-assign-keys (new match)
  "Build ordered item ops with final sort keys.
NEW is the filtered list of parsed items; MATCH is a vector mapping each
new index to its reused OLD item (or nil for an insert).  A matched item
becomes `:keep' when `dorg-model--item-semantic-equal-p' holds (no detail
rewrite needed), or `:update' when its content changed (id still reused).

Reused items keep their DB sort keys while their relative order stays
monotonic; inserts get midpoint keys between reused neighbors.  When the
reused order is non-monotonic (an item reorder) or nothing is reused, all
items get fresh strictly-ordered sequential keys.  Returns the
:keep/:update/:insert ops in new document order."
  (let* ((nlen (length new))
         (reused-keys (cl-loop for i from 0 below nlen
                               for o = (aref match i)
                               when o collect (dorg-item-sort-key o)))
         (monotonic (and (cl-notany #'null reused-keys)
                         (dorg-sortkey-ordered-p reused-keys)))
         (final (make-vector nlen nil)))
    (if (or (null reused-keys) (not monotonic))
        ;; No reuse, or a reorder: fresh ordered keys for every item.
        (let ((w (dorg-sortkey-width-for-count nlen)))
          (cl-loop for i from 0 below nlen do
                   (aset final i (dorg-sortkey-from-integer (1+ i) w))))
      ;; Monotonic reuse: reused items keep DB keys, inserts get midpoints.
      (let ((prev nil))
        (cl-loop for i from 0 below nlen
                 for o = (aref match i) do
                 (if o
                     (progn (aset final i (dorg-item-sort-key o))
                            (setq prev (dorg-item-sort-key o)))
                   (let* ((next (cl-loop for j from (1+ i) below nlen
                                         for oj = (aref match j)
                                         when oj return (dorg-item-sort-key oj)))
                          (mid (dorg-sortkey-midpoint prev next)))
                     (aset final i mid)
                     (setq prev mid))))))
    (cl-loop for n in new
             for i from 0
             for o = (aref match i)
             for k = (aref final i)
             collect (cond
                      ((null o) (list :insert n k))
                      ((dorg-model--item-semantic-equal-p o n) (list :keep o n k))
                      (t (list :update o n k))))))

(defun dorg-model-plan-items (old-items new-items)
  "Plan item ops reconciling OLD-ITEMS (DB-bound) to NEW-ITEMS (parsed).
OLD-ITEMS carry `dorg-item-id'/`dorg-item-sort-key'; NEW-ITEMS leave them
nil.  DORG_* keyword items are filtered from both sides and never appear
in the plan.  Returns a list of ops in final document order (deletes
last):
  (:keep   OLD NEW FINAL-SORT-KEY)  reuse id, content unchanged
  (:update OLD NEW FINAL-SORT-KEY)  reuse id, same kind, content changed
  (:insert NEW FINAL-SORT-KEY)      brand-new item (fresh id at lowering)
  (:delete OLD)                     removed item

Matching is two-phase: post-blank-independent `dorg-model--item-match-key'
identity first (greedy in document order so duplicates map deterministically
and each old id is reused at most once, and a reorder keeps each item bound
to its row), then compatibility pairing of the leftovers so an in-place
content edit reuses its row via `:update'.  Unpaired old items are deleted;
unpaired new items are inserted."
  (let* ((old (cl-remove-if #'dorg-item--db-local-p old-items))
         (new (cl-remove-if #'dorg-item--db-local-p new-items))
         (nlen (length new))
         (match (make-vector nlen nil))
         (used (make-hash-table :test 'eq)))
    ;; Phase 1 — match by identity key, greedy in document order.
    (let ((by-key (make-hash-table :test 'equal)))
      (dolist (o old)
        (let ((k (dorg-model--item-match-key o)))
          (puthash k (nconc (gethash k by-key) (list o)) by-key)))
      (cl-loop for n in new
               for i from 0 do
               (let* ((k (dorg-model--item-match-key n))
                      (q (gethash k by-key)))
                 (when q
                   (puthash k (cdr q) by-key)
                   (aset match i (car q))
                   (puthash (car q) t used)))))
    ;; Phase 2 — compatibility pairing of the remaining items (:update).
    (cl-loop for n in new
             for i from 0 do
             (unless (aref match i)
               (let ((o (cl-find-if
                         (lambda (o) (and (not (gethash o used))
                                          (dorg-model--items-compatible-p o n)))
                         old)))
                 (when o
                   (aset match i o)
                   (puthash o t used)))))
    ;; Phase 3 — sort keys + ops, then deletes for unused old items.
    (append (dorg-model--plan-items-assign-keys new match)
            (cl-loop for o in old
                     unless (gethash o used)
                     collect (list :delete o)))))

(provide 'dorg-model)
;;; dorg-model.el ends here
