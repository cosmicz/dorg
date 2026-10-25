;;; dorg-command.el --- Interactive query & update commands for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; User-facing interactive commands that surface the `dorg-ql' core for
;; querying and updating dorg-backed org trees.
;;
;; Query commands run a `dorg-ql' query against the current buffer's
;; registered backend and present the matches in a read-only
;; `*dorg-query*' results buffer; RET / mouse-1 navigates to a heading.
;; The convenience commands each issue a single atomic predicate; the raw
;; `dorg-query' command exposes the full grammar — atomic aliases or
;; composable v2 forms (`pipe'/`where'/`traverse', boolean combinators).
;;
;; The update command (`dorg-set-property') issues one canonical typed
;; mutation through `dorg-api-execute'.  Plain Org keeps source-preserving
;; live-buffer semantics; Dolt commits through the typed backend and refreshes
;; the established projection at the response's exact token.  `DORG_ID' is
;; identity, not an editable property, so setting it is rejected.
;;
;; Stable public facade for noninteractive / downstream tools (the CLI in
;; `dorg-app.el', a downstream tracker, etc.).  Query and mutation both route
;; through their typed public boundaries:
;;
;;   - `dorg-command-run-query' — run one atomic or composable v2 query
;;     against a buffer, returning the same result plists as
;;     `dorg-ql-select' WITHOUT creating or displaying `*dorg-query*', while
;;     preserving typed `dorg-ql-error' data for machine callers.  Interactive
;;     commands adapt failures to clean message-only `user-error'.
;;   - `dorg-set-property' — execute one `(on (id ...) (set (property ...) ...))'
;;     command, reject `DORG_ID', and return the structured API response.
;;
;; Dependency direction remains acyclic: this module requires the public API,
;; query, and pure registry layers.  It MUST NOT eagerly require `dorg-mode' —
;; the mode requires THIS module for its keymap.  Dolt command execution reaches
;; the mode-owned exact-view refresh adapter through declared lazy functions.

;;; Code:

(require 'dorg-id)
(require 'dorg-api)
(require 'dorg-ql)
(require 'dorg-registry)
(require 'org)

(declare-function dorg-refresh-materialized-view "dorg-mode"
                  (&optional buffer revision))
(declare-function dorg--refresh-materialized-view-context "dorg-mode"
                  (&optional buffer))

(define-error 'dorg-command-error "dorg interactive command failed" 'dorg-error)

;;; ─── Locate ───────────────────────────────────────────────────────────

(defun dorg-command--goto-id-in-buffer (id buffer)
  "Move point in BUFFER to the heading whose `DORG_ID' is ID.
Signals a `user-error' when no such heading is materialized in BUFFER.
Returns the heading position on success."
  (with-current-buffer buffer
    (let ((target (number-to-string id))
          (found nil))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (and (not found) (re-search-forward org-heading-regexp nil t))
         (beginning-of-line)
         (if (equal (org-entry-get (point) dorg-id-property) target)
             (setq found (point))
           (end-of-line))))
      (unless found
        (user-error "dorg: no heading with DORG_ID %s in this buffer" id))
      (goto-char found)
      found)))

(defun dorg-command--reveal ()
  "Make the heading at point visible if the buffer is an org buffer."
  (when (derived-mode-p 'org-mode)
    (cond ((fboundp 'org-fold-show-context) (org-fold-show-context 'link-search))
          ((fboundp 'org-show-context) (org-show-context 'link-search)))))

(defun dorg-command--goto-result (result &optional source-buffer)
  "Navigate to the heading described by RESULT.
RESULT is a `dorg-ql' result plist.  When it carries a live `:marker'
\(plain-org), jump straight to it.  Otherwise locate the heading by its
`:heading-id' in SOURCE-BUFFER (the buffer the query was run against),
mirroring the nil-marker Dolt/model case.  Signals a `user-error' when
the heading cannot be located."
  (let ((marker (plist-get result :marker))
        (id (plist-get result :heading-id)))
    (cond
     ((and (markerp marker) (marker-buffer marker))
      (pop-to-buffer (marker-buffer marker))
      (goto-char marker)
      (dorg-command--reveal))
     ((and id source-buffer (buffer-live-p source-buffer))
      (pop-to-buffer source-buffer)
      (dorg-command--goto-id-in-buffer id source-buffer)
      (dorg-command--reveal))
     (t
      (user-error "dorg: cannot locate a heading for this result")))))

;;; ─── Results Buffer ───────────────────────────────────────────────────

(defvar-local dorg-command--source-buffer nil
  "Buffer the displayed `*dorg-query*' results were drawn from.")

;; KEEP (keymap): mode map bound by `define-derived-mode dorg-query-results-mode'.
(defvar dorg-query-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dorg-command-goto-result-at-point)
    (define-key map [mouse-1]   #'dorg-command-goto-result-at-point)
    map)
  "Keymap for `dorg-query-results-mode'.")

(define-derived-mode dorg-query-results-mode special-mode "Dorg-Query"
  "Major mode for the `*dorg-query*' results buffer.
Each line carries the originating `dorg-ql' result as a text property;
\\[dorg-command-goto-result-at-point] visits the heading.")

(defun dorg-command-goto-result-at-point (&optional event)
  "Visit the heading for the result on the current line.
With a mouse EVENT, move point to the click first."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((result (get-text-property (point) 'dorg-command-result)))
    (unless result
      (user-error "dorg: no query result on this line"))
    (dorg-command--goto-result result dorg-command--source-buffer)))

(defun dorg-command--show (results query source-buffer)
  "Display RESULTS for QUERY (run against SOURCE-BUFFER) in `*dorg-query*'.
Returns the results buffer."
  (let ((buf (get-buffer-create "*dorg-query*")))
    (with-current-buffer buf
      (dorg-query-results-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq dorg-command--source-buffer source-buffer)
        (insert (format "dorg query: %S\n\n" query))
        (if (null results)
            (insert "No matches.\n")
          (dolist (r results)
            (let ((start (point)))
              (insert (if (plist-member r :values)
                          (concat (dorg-ql-result-text r) "\n")
                        (format "%-10s %s\n"
                                (or (plist-get r :heading-id) "—")
                                (or (plist-get r :title) ""))))
              (put-text-property start (point) 'dorg-command-result r))))
        (goto-char (point-min))))
    (display-buffer buf)
    buf))

;;; ─── Query ────────────────────────────────────────────────────────────

(defun dorg-command-run-query (query &optional buffer)
  "Run an atomic or composable v2 QUERY and return `dorg-ql-select' results.
BUFFER defaults to the current buffer.  Returns the same result plists
as `dorg-ql-select' and does NOT create or display the interactive
`*dorg-query*' results buffer.

Typed `dorg-ql-error' conditions propagate unchanged so machine callers
can dispatch on `dorg-error-reason' and inspect `dorg-error-data'.  This
is the stable public noninteractive query facade; interactive commands
use the private message-only adapter below, while the CLI maps the typed
condition to its shared transport envelope."
  (dorg-ql-select query buffer))

(defun dorg-command--run-query-for-user (query &optional buffer)
  "Run QUERY against BUFFER, adapting typed failures for interactive use."
  (condition-case err
      (dorg-command-run-query query buffer)
    (dorg-ql-error
     (user-error "%s" (cadr err)))))

(defun dorg-query (query)
  "Run a dorg-ql QUERY against the current buffer and show matches.
Interactively, read the query form from the minibuffer: an atomic form
such as (tag \"work\") or (property \"CATEGORY\" \"red\"), or a composable
v2 form such as (pipe (where (tag \"work\")) (traverse children))."
  (interactive
   (list (read (read-string
                "dorg query (atomic or composable v2 form): "))))
  (dorg-command--show
   (dorg-command--run-query-for-user query (current-buffer))
   query (current-buffer)))

(defun dorg-query-property (key value)
  "Show headings in the current buffer whose property KEY equals VALUE."
  (interactive (list (read-string "Property key: ")
                     (read-string "Property value: ")))
  (let ((query (list 'property key value)))
    (dorg-command--show
     (dorg-command--run-query-for-user query (current-buffer))
     query (current-buffer))))

(defun dorg-query-tag (tag)
  "Show headings in the current buffer carrying the local TAG."
  (interactive (list (read-string "Tag: ")))
  (let ((query (list 'tag tag)))
    (dorg-command--show
     (dorg-command--run-query-for-user query (current-buffer))
     query (current-buffer))))

(defun dorg-query-title (title)
  "Show headings in the current buffer whose title is exactly TITLE."
  (interactive (list (read-string "Title: ")))
  (let ((query (list 'title title)))
    (dorg-command--show
     (dorg-command--run-query-for-user query (current-buffer))
     query (current-buffer))))

(defun dorg-goto-id (id)
  "Jump to the heading whose `DORG_ID' is ID in the current buffer."
  (interactive (list (read-number "DORG_ID: ")))
  (let* ((query (list 'id id))
         (results (dorg-command--run-query-for-user query (current-buffer))))
    (if results
        (dorg-command--goto-result (car results) (current-buffer))
      (user-error "dorg: no heading with DORG_ID %s in this buffer" id))))

;;; ─── Typed Update ─────────────────────────────────────────────────────

(defun dorg-command--call-mode-boundary (function &rest args)
  "Call mode-owned FUNCTION with ARGS through the lazy refresh boundary."
  (unless (fboundp function)
    (require 'dorg-mode))
  (apply function args))

(defun dorg-command--target-id (id buffer)
  "Return explicit ID or the DOrg heading id at point in BUFFER."
  (or id
      (with-current-buffer buffer
        (save-excursion
          (unless (org-at-heading-p)
            (condition-case nil
                (org-back-to-heading t)
              (error (user-error "dorg: point is not within a heading"))))
          (or (dorg-id-at-point)
              (user-error "dorg: heading has no valid DORG_ID"))))))

(defun dorg-command--raise-refresh-failure (token err)
  "Signal a typed post-commit refresh failure for TOKEN caused by ERR."
  (signal 'dorg-command-error
          (list (format "dorg: commit %s succeeded but view refresh failed: %s"
                        token (error-message-string err))
                (list :reason 'refresh-after-commit-failed
                      :new-token token
                      :cause (error-message-string err)))))

(defun dorg-set-property (key value &optional id buffer)
  "Set property KEY to VALUE through one canonical typed mutation.
Without ID, operate on the heading at point; with ID, address that public
`DORG_ID'.  BUFFER defaults to the current buffer.

Plain Org applies through the source-preserving typed executor, leaving the
live buffer modified for ordinary Emacs saving.  A Dolt-backed projection must
already have a coherent established view: the command validates it before the
commit, executes exactly one `dorg-api-execute' apply, then refreshes the same
whole/window view at the returned `:new-token'.  A post-commit refresh failure
signals `dorg-command-error' with that committed token and never retries.

`DORG_ID' is heading identity, not an editable property, so setting it
is refused.  Returns the structured API response on success."
  (interactive (list (read-string "Property key: ")
                     (read-string "Property value: ")))
  (when (equal (upcase key) dorg-id-property)
    (user-error "dorg: DORG_ID is heading identity and cannot be set as a property"))
  (let* ((buf (or buffer (current-buffer)))
         (target-id (dorg-command--target-id id buf))
         (dolt-p (and (dorg-registered-dolt-dir buf) t))
         (form (list 'on (list 'id target-id)
                     (list 'set (list 'property key) value))))
    (when dolt-p
      (dorg-command--call-mode-boundary
       'dorg--refresh-materialized-view-context buf))
    (let ((response (dorg-api-execute form :mode 'apply :source buf)))
      (when dolt-p
        (let ((token (plist-get response :new-token)))
          (unless token
            (signal 'dorg-command-error
                    (list "dorg: typed mutation returned no revision token"
                          (list :reason 'missing-new-token))))
          (condition-case err
              (dorg-command--call-mode-boundary
               'dorg-refresh-materialized-view buf token)
            ((error quit)
             ;; The commit already exists.  Even a C-g during refresh must
             ;; surface its exact token instead of masquerading as an
             ;; uncommitted cancellation.
             (dorg-command--raise-refresh-failure token err)))))
      response)))

(provide 'dorg-command)
;;; dorg-command.el ends here
