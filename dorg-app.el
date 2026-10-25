;;; dorg-app.el --- Command-line app surface for dorg  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The non-interactive Clime app behind `bin/dorg'.  It is a file-centric
;; DOrg surface with three command families so downstream tools and other
;; consumers can drive DOrg without an Emacs UI:
;;
;;   query — read DOrg-backed org trees (atomic `dorg-ql' aliases or the
;;           composable v2 grammar, plus id/property/tag/title
;;           convenience forms);
;;   mutate — apply explicit typed mutations;
;;   import — administrative full-document bootstrap/recovery ingest;
;;   lock inspect — read-only local saved-file owner diagnostics;
;;   dolt — manage Dolt sql-servers and explicit store identity with
;;            owner-safe lifecycle semantics.
;;
;; Load-path / build honesty
;; -------------------------
;; Unlike the core library, this file requires `clime'.  It lives at the
;; repo root so it compiles and lints with the rest of the tree, but it is
;; built honestly: `make compile' / `make lint' put `lib/clime' on the
;; load path (see the Makefile), and `dorg-app' is NOT `require'd by
;; `dorg.el' nor reloaded by `dorg-reload'.  Therefore `(require \\='dorg)'
;; loads DOrg core WITHOUT pulling in Clime — core gains no Clime
;; dependency.  The app
;; is loaded only when `bin/dorg' (or a test) loads this file explicitly.
;;
;; Reuse boundaries: this surface uses only the stable
;; public facades, never private helpers or a direct SQL path.
;;   - Reads go through `dorg-command-run-query' (UI-free `dorg-ql-select'
;;     results; typed `dorg-ql-error' data maps to the shared envelope).
;;   - Writes go through one `dorg-api-execute' call.  File-backed apply
;;     requests source persistence inside that same typed transaction.
;;   - Administrative ingest goes through `dorg-import-document'; it is
;;     explicit, revision-guarded, and unavailable through HTTP serving.
;;   - Server commands go through the public server-management
;;     surface: `dorg-dolt-server-list', `dorg-dolt-start', `dorg-dolt-stop',
;;     and `dorg-dolt-stop-all'.  Listing is read-only; stop is
;;     owner-safe; explicit starts are manual lifecycle operations.
;;   - Store identification goes through `dorg-api-identify-store'.
;;   - Backend resolution follows the registration order: explicit
;;     stored binding > `#+DORG_DOLT_DIR:' keyword > transient
;;     plain-org.  Query/edit commands that need a concrete store use
;;     the strict `dorg--ensure-registration' (which binds a keyword
;;     buffer); read-only defaulting classifies purely.  `--dolt-dir'
;;     forces `dorg-bind-dolt'.
;;
;; Source selection
;; ----------------
;;   `--file/-f FILE' selects the org source for query/edit commands.  The
;;   app `:env-prefix' "DORG" plus the option `:env' "FILE" derive the env
;;   var `DORG_FILE'.  An explicit `--file' overrides the env var.  Server
;;   commands do NOT require `--file': list/status run without any org
;;   file, and start/stop target an explicit Dolt dir (positional or
;;   `--dolt-dir') or one derived from `--file'.  No batch path prompts.
;;
;; Output
;; ------
;;   Text (default) — one tab-separated line per result row.
;;   `--json' — the shared ok/data/error envelope via the explicit
;;   `dorg-app--json-finalize'.  Command-specific payloads live under
;;   `data'; structured failures live under `error'.
;;
;; Serve (HTTP)
;; ------------
;;   `clime-serve' is a vendored, OPTIONAL Clime module loaded LAZILY only
;;   by the `serve start' handler, so the no-core-HTTP guarantee above
;;   holds for the base CLI: although `lib/clime' is on the load path,
;;   neither `clime-serve' nor `web-server' is PROVIDED/LOADED until a
;;   `serve' command runs (a `require' on demand).  The same query/edit
;;   handlers are exposed over HTTP unchanged because they are serve-clean:
;;   they emit via `clime-out' / `clime-out-text', read no minibuffer, and
;;   touch no `*dorg-query*' buffer.  HTTP is JSON-only with token auth;
;;   see docs/cli.org "Serve (HTTP)" for routes, auth, and lifecycle.
;;
;; Local invocation history
;; ------------------------
;;   One named Clime invocation hook appends privacy-safe lifecycle metadata
;;   to a local JSONL ledger.  `dorg-cli-history-file' controls the ledger;
;;   DORG_CLI_HISTORY_FILE overrides it and an empty value disables logging.
;;   The observer never records argv, parameters, content, output, return
;;   values, error messages, environment, or request/auth data.

;;; Code:

;; KEEP (external): org.el's own cache variable, forward-declared so the byte-compiler
;; does not flag the `setq' that tunes the org-element cache as a free variable.
(defvar org-element-cache-persistent)

(defun dorg-app--serve-start-command-p ()
  "Return non-nil when argv names the long-lived `serve start' command.
Detects the command *positionally* rather than against a hand-maintained
command list (which silently drifts as subcommands are added): scan past
the known global option flags and take the first two positional tokens,
returning non-nil iff they are exactly `serve' then `start'.

The value-taking global options --file/-f, --dolt-dir and --root-id each
consume the FOLLOWING token as their value.  So `--root-id N serve start'
reads N as the option value, leaving `serve' `start' as the positionals
(=> non-nil), while `--root-id serve start' consumes `serve' as the value
and leaves `start' as the sole command (=> nil) — the scan is never fooled
into reading a value token as a command.  Boolean --json and any other
`-'-prefixed flag (including the inline equals forms
--file=/--dolt-dir=/--root-id=) are self-contained and skipped."
  (let ((args command-line-args-left)
        positionals)
    (while (and args (< (length positionals) 2))
      (let ((arg (pop args)))
        (cond
         ((member arg '("--file" "-f" "--dolt-dir" "--root-id"))
          (pop args))
         ((string-prefix-p "-" arg))
         (t (push arg positionals)))))
    (equal (nreverse positionals) '("serve" "start"))))

(defun dorg-app--harden-batch-org-persist ()
  "Disable Org persist exit writes in short-lived batch CLI processes."
  (when (and noninteractive
             (not (dorg-app--serve-start-command-p)))
    (setq org-element-cache-persistent nil)
    (with-eval-after-load 'org-persist
      (let (kept)
        (dolist (hook kill-emacs-hook)
          (unless (and (symbolp hook)
                       (string-prefix-p "org-persist-" (symbol-name hook)))
            (push hook kept)))
        (setq kill-emacs-hook (nreverse kept))))))

(dorg-app--harden-batch-org-persist)

(require 'clime)
(require 'clime-telemetry)
(require 'dorg)
(require 'dorg-envelope)
(require 'json)
(require 'subr-x)

(declare-function dorg-load-extension "dorg-ext" (name))

;; `clime-serve' is an OPTIONAL Clime module, loaded lazily by the
;; `serve start' handler — declare its entry points so this file
;; byte-compiles without requiring the HTTP stack at top level.
(declare-function clime-serve "clime-serve" (app &rest args))
(declare-function clime-serve-stop "clime-serve" (&optional server))

;; KEEP (mutable-state): setq-toggled re-entrancy flag, non-nil while inside the
;; `dorg serve' HTTP loop.  Because `serve' is a command on the very app being
;; served, its lifecycle routes (/serve/*) are reachable over HTTP; this flag makes
;; those handlers refuse so a request can never recursively start/stop a server or
;; block the loop.
(defvar dorg-app--serving nil)

(defcustom dorg-cli-history-file 'default
  "Local privacy-safe invocation-history destination.

The symbol `default' resolves at event time to
$XDG_STATE_HOME/dorg/clime-invocations.jsonl when XDG_STATE_HOME is nonempty,
otherwise ~/.local/state/dorg/clime-invocations.jsonl.  A file name selects an
explicit destination and nil disables history.  DORG_CLI_HISTORY_FILE has
precedence: a nonempty value selects a file and an explicitly empty value
disables history."
  :type '(choice (const :tag "XDG state default" default)
                 (const :tag "Disabled" nil)
                 (file :tag "Explicit JSONL file"))
  :group 'dorg)

(defun dorg-app--history-file ()
  "Return the effective invocation-history file, or nil when disabled."
  (let ((environment (getenv "DORG_CLI_HISTORY_FILE")))
    (cond
     ((stringp environment)
      (unless (string-empty-p environment)
        (expand-file-name environment)))
     ((null dorg-cli-history-file) nil)
     ((eq dorg-cli-history-file 'default)
      (let ((xdg (getenv "XDG_STATE_HOME")))
        (if (and xdg (not (string-empty-p xdg)))
            (expand-file-name "dorg/clime-invocations.jsonl" xdg)
          (expand-file-name
           ".local/state/dorg/clime-invocations.jsonl"
           (or (getenv "HOME") (expand-file-name "~"))))))
     ((stringp dorg-cli-history-file)
      (expand-file-name dorg-cli-history-file))
     (t
      (error "dorg-cli-history-file must be default, nil, or a file name")))))

(defun dorg-app--log-invocation (event)
  "Append privacy-safe metadata for Clime invocation EVENT locally."
  (when-let ((file (dorg-app--history-file)))
    (clime-telemetry-append-jsonl event file)))

;; Structured command error.  A handler that owes the JSON surface a reason
;; and/or details (doctor's findings report, mutate's `dorg-api-error') signals
;; this and lets `dorg-app--with-json-error' route the payload to the right
;; drain.  The payload rides the condition — there is no module-level pending
;; channel to set, read, and reset.
(define-error 'dorg-app-error "dorg command error")

(defun dorg-app--command-error (reason message &optional details usage)
  "Signal `dorg-app-error' carrying REASON, MESSAGE, DETAILS, and USAGE.
The condition data is the plist (:reason :message :details :usage); USAGE
non-nil marks a client-input error (HTTP 400 when served), nil a server
fault (HTTP 500).  Caught and routed by `dorg-app--with-json-error'."
  (signal 'dorg-app-error
          (list :reason reason :message message :details details :usage usage)))

(defun dorg-app--read-exact-form (text command)
  "Read exactly one Lisp form from TEXT for COMMAND.
Emacs rejects reader evaluation syntax.  One form followed only by whitespace is
accepted; atoms and nil are returned for the command's domain validator.
Signals `dorg-app-error' with reason `invalid-form' and stable detail
`empty-input', `malformed-syntax', or `trailing-input'."
  (when (string-blank-p text)
    (dorg-app--command-error
     'invalid-form (format "dorg: %s input is empty" command)
     (list :command command :detail 'empty-input) t))
  (let ((parsed
         (condition-case nil
             (read-from-string text)
           (error nil))))
    (unless parsed
      (dorg-app--command-error
       'invalid-form (format "dorg: %s input is not a valid form" command)
       (list :command command :detail 'malformed-syntax) t))
    (unless (string-blank-p (substring text (cdr parsed)))
      (dorg-app--command-error
       'invalid-form (format "dorg: %s input contains trailing input" command)
       (list :command command :detail 'trailing-input) t))
    (car parsed)))

(defun dorg-app--error-payload-p (x)
  "Return non-nil when X is a structured command-error payload plist.
The JSON drains accept either such a payload or a plain message string."
  (and (consp x) (keywordp (car x)) (plist-member x :message)))

(defmacro dorg-app--with-json-error (&rest body)
  "Run BODY; route a signalled `dorg-app-error' to the JSON error drain.
The structured payload is carried on the condition (no mutable global):
 - served client error (`:usage'): re-signal `clime-usage-error' with the
   payload so the route maps to HTTP 400 and `dorg-app--json-error' renders
   reason/details from it;
 - buffered `--json' (CLI, or a served server fault): hand the payload to the
   finalizer via `clime-out-error' — a non-empty error list yields exit 1
   (CLI) / HTTP 500 (served);
 - text surface: re-signal a plain `error' with the message so the streamed
   report exits nonzero (the human report is already on stdout)."
  (declare (indent 0) (debug t))
  `(condition-case dorg--cmderr (progn ,@body)
     (dorg-app-error
      (let ((payload (cdr dorg--cmderr)))
        (cond
         ((and dorg-app--serving (plist-get payload :usage))
          (signal 'clime-usage-error (list payload)))
         ((eq (clime-out-format) 'json)
          (clime-out-error payload))
         (t
          (signal 'error (list (plist-get payload :message)))))))))

(defmacro dorg-app--client-error-as-usage (&rest body)
  "Run BODY; over the HTTP serve surface, reclassify a clean `user-error'.
A `user-error' from a route handler (e.g. a missing source file) is a
client-input error, not a server fault.  While serving
\(`dorg-app--serving') re-signal it as a `clime-usage-error' so the route
maps to HTTP 400 with the shared JSON error envelope instead of
500.  On the CLI (not serving) the original error propagates unchanged."
  (declare (indent 0) (debug t))
  `(condition-case dorg--cerr (progn ,@body)
     (user-error
      (if dorg-app--serving
          (signal 'clime-usage-error (cdr dorg--cerr))
        (signal (car dorg--cerr) (cdr dorg--cerr))))))

;;; ─── Pure helpers (no I/O) ─────────────────────────────────────────────

(defun dorg-app--text (result)
  "Return the text line \"<id>\\t<title>\" for a `dorg-ql' RESULT plist.
The id renders as the em dash when nil, matching the results-buffer
convention in `dorg-command'."
  (if (plist-member result :values)
      (dorg-ql-result-text result)
    (format "%s\t%s"
            (or (plist-get result :heading-id) "—")
            (or (plist-get result :title) ""))))

(defun dorg-app--error-envelope-json (error-or-msg)
  "Return the JSON error envelope for ERROR-OR-MSG.
ERROR-OR-MSG is either a structured command-error payload carried by
`dorg-app--with-json-error' (reason/message/details ride it) or a plain
message string (rendered with the generic `error' reason)."
  (if (dorg-app--error-payload-p error-or-msg)
      (dorg-envelope-error-json (or (plist-get error-or-msg :reason) 'error)
                                (plist-get error-or-msg :message)
                                (plist-get error-or-msg :details))
    (dorg-envelope-error-json 'error error-or-msg nil)))

(defun dorg-app--json-finalize (items retval errors)
  "Finalize `--json' output as the shared ok/data/error envelope.
On error, ERRORS' first element is either a structured command-error
payload or a plain message string; reason/details ride the payload, so
there is no module-level pending channel to read or reset."
  (cond
   (errors
    (dorg-app--error-envelope-json (car errors)))
   (retval
    (if (and (listp retval) (plist-member retval :ok)
             (plist-member retval :data) (plist-member retval :error))
        (dorg-envelope-serialize retval)
      (dorg-envelope-success-json retval)))
   (items
    (dorg-envelope-success-json
     (list :command 'items :results (vconcat items) :count (length items))))
   (t
    (dorg-envelope-success-json
     (list :command 'items :results [] :count 0)))))

(defun dorg-app--json-error (msg)
  "Emit MSG as the shared JSON error envelope.
MSG is either a structured command-error payload (from a re-signalled
`clime-usage-error' over the serve surface) or a plain message string."
  (princ (concat (json-encode (dorg-app--error-envelope-json msg)) "\n")))

;;; ─── Source helpers ────────────────────────────────────────────────────

(defun dorg-app--require-file (file)
  "Return FILE, or signal a clean usage error when it is nil.
Query/edit commands need an org source; this enforces it without the
app-level `:required' that would also wrongly bind dolt commands.
Over HTTP the missing-file usage error maps to 400 + the JSON envelope
\(`dorg-app--client-error-as-usage')."
  (dorg-app--client-error-as-usage
    (or file
        (user-error
         "dorg: no org file; pass --file/-f FILE or set DORG_FILE"))))

(defun dorg-app--open (file dolt-dir)
  "Open FILE through the shared batch-safe source policy; return its buffer.
When DOLT-DIR is non-nil, force a Dolt binding with `dorg-bind-dolt'
before returning; otherwise no binding is created here.  Opening is a
pure step: a downstream operation that needs a concrete store resolves
the backend strictly via `dorg--ensure-registration' (which binds a
`#+DORG_DOLT_DIR:' keyword buffer), while read-only paths classify it
purely."
  (let ((buf (dorg--find-file-noselect (dorg-app--require-file file))))
    (when dolt-dir
      (dorg-bind-dolt file dolt-dir))
    buf))

(defun dorg-app--emit-query (query buffer)
  "Run atomic or composable v2 QUERY against BUFFER.
In JSON mode return shared-envelope data.  In text mode stream one line
per hit and return nil.
Typed query failures map once to `dorg-app-error'; the shared router emits
one CLI JSON envelope or one HTTP 400 envelope with reason and details."
  (dorg-app--with-json-error
    (condition-case err
        (if (eq (clime-out-format) 'json)
            ;; The envelope and rows must come from one invocation: running the
            ;; command facade first could observe a different HEAD or workflow
            ;; context than the snapshot reported to the caller.
            (dorg-ql-snapshot-public
             (dorg-ql-select-snapshot query buffer))
          (dolist (hit (dorg-command-run-query query buffer))
            (clime-out nil :text (dorg-app--text hit)))
          nil)
      (dorg-ql-error
       (dorg-app--command-error (dorg-error-reason err)
                                (cadr err)
                                (dorg-error-data err)
                                t)))))

(defun dorg-app--mutate-mode (dry-run apply mode)
  "Return the effective mutate mode symbol, or `invalid' when not exactly one.
DRY-RUN / APPLY are CLI boolean flags; MODE is the HTTP `mode' string
\(\"dry-run\"/\"apply\").  Exactly one must be given (`dorg-api-execute'
turns `invalid' into a `mode-error')."
  (let ((n (+ (if dry-run 1 0) (if apply 1 0) (if mode 1 0))))
    (cond ((/= n 1) 'invalid)
          (dry-run 'dry-run)
          (apply 'apply)
          ((member mode '("dry-run" "apply")) (intern mode))
          (t 'invalid))))

(defun dorg-app--parse-expect-targets (s)
  "Parse a comma-separated --expect-targets string S into an identity set.
Each token is an integer heading id or an extern token `key:K=V' (see
`dorg-api-parse-target-token'); mixed lists are allowed.  Extern tokens
percent-encode reserved characters, so a comma/whitespace split is safe.
Returns the symbol `none' when S is nil (no expected set supplied)."
  (if (null s) 'none
    (mapcar #'dorg-api-parse-target-token
            (split-string s "[, \t]+" t))))

(defun dorg-app--strvec (forms)
  "Return a JSON-safe vector of prin1 strings for typed FORMS."
  (vconcat (mapcar #'prin1-to-string forms)))

(defun dorg-app--vec (values)
  "Return VALUES as a JSON array vector, preserving nil as an empty array."
  (vconcat (or values nil)))

(defun dorg-app--affected-json (affected)
  "Return JSON-safe created/changed/deleted fields from AFFECTED plist."
  (list :created (dorg-app--vec (plist-get affected :created))
        :changed (dorg-app--vec (plist-get affected :changed))
        :deleted (dorg-app--vec (plist-get affected :deleted))))

(defun dorg-app--emit-import (receipt)
  "Emit administrative import RECEIPT as text or JSON-safe data."
  (clime-out-text
   (format "import %s: root %s committed at %s"
           (plist-get receipt :mode)
           (plist-get receipt :root-id)
           (plist-get receipt :commit)))
  (when (eq (clime-out-format) 'json)
    (list :command 'import
          :ok (if (plist-get receipt :ok) t :json-false)
          :mode (plist-get receipt :mode)
          :source (plist-get receipt :source)
          :target (plist-get receipt :target)
          :dolt-dir (plist-get receipt :dolt-dir)
          :root-id (plist-get receipt :root-id)
          :base-commit (plist-get receipt :base-commit)
          :commit (plist-get receipt :commit)
          :created (dorg-app--vec (plist-get receipt :created))
          :changed (dorg-app--vec (plist-get receipt :changed))
          :deleted (dorg-app--vec (plist-get receipt :deleted))
          :extensions (plist-get receipt :extensions)
          :invalidation (plist-get receipt :invalidation))))

(defun dorg-app--emit-rebind (receipt)
  "Emit file-binding rebind RECEIPT as text or JSON-safe data."
  (clime-out-text
   (format "rebind root %s: %s -> %s (%s)"
           (plist-get receipt :root-id)
           (plist-get receipt :old-relative-path)
           (plist-get receipt :new-relative-path)
           (if (plist-get receipt :changed) "changed" "no-op")))
  (when (eq (clime-out-format) 'json)
    (list :command 'dolt-rebind
          :ok (if (plist-get receipt :ok) t :json-false)
          :root-id (plist-get receipt :root-id)
          :old-relative-path (plist-get receipt :old-relative-path)
          :new-relative-path (plist-get receipt :new-relative-path)
          :base (plist-get receipt :base)
          :commit (plist-get receipt :commit)
          :changed (if (plist-get receipt :changed) t :json-false))))

(defun dorg-app--emit-mutate (resp)
  "Emit the canonical mutate RESP: a human summary in text, the object in JSON.
Nested typed patches are rendered as prin1 strings so the JSON stays a plain,
auditable object."
  (let ((mode (plist-get resp :mode))
        (targets (plist-get resp :targets)))
    (clime-out-text
     (format "mutate %s: %d patch%s over %d target%s"
             mode (length (plist-get resp :patches))
             (if (= 1 (length (plist-get resp :patches))) "" "es")
             (length targets) (if (= 1 (length targets)) "" "s")))
    (when (eq (clime-out-format) 'json)
      (list :ok (if (plist-get resp :ok) t :json-false)
            :command (or (plist-get resp :command) 'mutate)
            :mode mode
            :intent (dorg-app--strvec (plist-get resp :intent))
            :patches (dorg-app--strvec (plist-get resp :patches))
            ;; Integer ids stay JSON numbers (backward compatible); extern
            ;; identities emit their canonical `key:K=V' wire token string.
            :targets (vconcat (mapcar (lambda (id)
                                         (if (integerp id) id
                                           (dorg-api-target-token id)))
                                       targets))
            :affected (dorg-app--affected-json (plist-get resp :affected))
            :supported (if (plist-get resp :supported) t :json-false)
            :unsupported (dorg-app--vec (plist-get resp :unsupported))
            :base-commit (or (plist-get resp :base-commit) :json-null)
            :base-revision (or (plist-get resp :base-revision) :json-null)
            :new-token (or (plist-get resp :new-token) :json-null)))))

(defun dorg-app--mutate-source (file dolt-dir root-id)
  "Return (SOURCE . BUFFER) for mutate source options.
When DOLT-DIR and ROOT-ID are both supplied, SOURCE is the direct Dolt
descriptor and BUFFER is nil.  Otherwise mutate uses the buffer-backed source
descriptor returned by `dorg-app--open'.  A ROOT-ID without DOLT-DIR is a
usage error."
  (cond
   ((and root-id (not dolt-dir))
    (user-error "dorg mutate: --root-id requires --dolt-dir"))
   (root-id
    (cons (list :dolt-dir dolt-dir :root-id root-id) nil))
   (t
    (let ((buf (dorg-app--open file dolt-dir)))
      (cons (list :buffer buf) buf)))))

;;; ─── Server helpers ──────────────────────────────────────────────────

(defun dorg-app--dolt-json (entry)
  "Return the `--json' object plist for a `dorg-dolt-server-list' ENTRY plist.
Carries the serializable subset of the inventory plist; the embedded
process/connection objects are not included."
  (let (out)
    (dolist (k '(:dolt-dir :port :role :status :policy :protocol
                 :owner-pid :server-pid :host :created-at :last-used :generation))
      (setq out (append out (list k (plist-get entry k)))))
    out))

(defun dorg-app--doctor-finding-json (finding)
  "Return FINDING as a public structured plist."
  (list :check (plist-get finding :check)
        :severity (plist-get finding :severity)
        :message (plist-get finding :message)
        :fixable (if (plist-get finding :fix-sql) t :json-false)))

(defun dorg-app--doctor-data (target clean applied findings &optional environment)
  "Return public doctor data for TARGET."
  (list :command 'doctor
        :dolt-dir target
        :environment (if environment t :json-false)
        :clean (if clean t :json-false)
        :applied applied
        :findings (vconcat (mapcar #'dorg-app--doctor-finding-json findings))))

(defun dorg-app--dolt-text (entry)
  "Return a tab-separated text line for a `dorg-dolt-server-list' ENTRY plist.
Columns: role, status, port, dolt-dir."
  (format "%s\t%s\t%s\t%s"
          (or (plist-get entry :role) "—")
          (or (plist-get entry :status) "—")
          (or (plist-get entry :port) "—")
          (or (plist-get entry :dolt-dir) "—")))

(defun dorg-app--bound-org-files (dir)
  "Return (ORG-FILE . BOUND-DOLT-DIR) for DOrg-bound org files under DIR.
Probes the patterns in `dorg-dolt-server-bound-org-globs' (top-level and
one-level-nested `*.org' by default) and keeps only those that declare a
`#+DORG_DOLT_DIR:' file binding (with its resolved, absolute store).
Used to detect that a positional server DIR is actually a project root
for a DOrg document bound to a DIFFERENT (typically nested) Dolt store."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (candidates (and (file-directory-p dir)
                          (mapcan (lambda (glob)
                                    (file-expand-wildcards
                                     (expand-file-name glob dir) t))
                                  dorg-dolt-server-bound-org-globs)))
         (bound nil))
    (dolist (f (delete-dups candidates))
      (when (file-readable-p f)
        (let ((store (with-temp-buffer
                       (insert-file-contents f)
                       (setq buffer-file-name f)
                       (unwind-protect (dorg--buffer-dolt-dir)
                         (setq buffer-file-name nil)))))
          (when store
            (push (cons f store) bound)))))
    (nreverse bound)))

(defun dorg-app--reject-misbound-dir (dir)
  "Signal a usage error when positional server DIR is a misbound project root.
DIR is rejected when it contains a DOrg-bound org file (top-level or a
tracker nested one level down in a subdirectory) whose declared
`#+DORG_DOLT_DIR:' store differs from DIR.  This stops an ordinary `dorg dolt start .' from a project
root from publishing a managed claim for an accidental root `.dolt'
instead of the document's real nested store.  An
explicit `--dolt-dir' is the raw-Dolt escape hatch and never reaches
this check.  Returns the expanded DIR unchanged when it is a legitimate
target."
  (let* ((canon-dir (dorg-dolt-server--canonical-dir dir))
         (is-dolt (file-directory-p (expand-file-name ".dolt" dir))))
    (pcase-dolist (`(,org . ,store) (dorg-app--bound-org-files dir))
      (let ((canon-bound (dorg-dolt-server--canonical-dir store)))
        (when (not (equal canon-bound canon-dir))
          (user-error
           (concat "dorg: %s is bound to Dolt store %s (via %s), not %s%s; "
                   "refusing to start a server for the project root. "
                   "Use `--file %s' or pass the bound store `%s' explicitly")
           (abbreviate-file-name (directory-file-name (expand-file-name dir)))
           (abbreviate-file-name (directory-file-name canon-bound))
           (abbreviate-file-name org)
           (abbreviate-file-name (directory-file-name canon-dir))
           (if is-dolt " (an accidental Dolt repo)" "")
           (abbreviate-file-name org)
           (abbreviate-file-name (directory-file-name canon-bound))))))
    (expand-file-name dir)))

(defun dorg-app--resolve-dolt-dir (dir file dolt-dir label guard-dir)
  "Resolve a Dolt directory for a batch command without prompting.
Preference order: positional DIR, then `--dolt-dir' DOLT-DIR, then the
Dolt repo registered for `--file' FILE.  Signals a clean usage error —
prefixed by LABEL (e.g. \"dorg dolt\" / \"dorg doctor\") — when none
resolves, so a batch command never falls through to a minibuffer prompt.

When GUARD-DIR is non-nil a positional DIR is routed through
`dorg-app--reject-misbound-dir' (rejecting a misbound project root that
declares a different store); otherwise DIR is taken as-is via
`expand-file-name'.  `--dolt-dir' is the explicit raw-Dolt path and is
never second-guessed; `--file' prefers and displays the file-derived
bound store."
  (cond
   (dir (if guard-dir
            (dorg-app--reject-misbound-dir dir)
          (expand-file-name dir)))
   (dolt-dir dolt-dir)
   (file (or (with-current-buffer (dorg--find-file-noselect file)
               (dorg-registered-dolt-dir))
             (user-error
              "%s: --file %s has no registered Dolt repo; pass a Dolt dir"
              label file)))
   (t (user-error
       "%s needs a Dolt dir (positional DIR, --dolt-dir, or --file with a registered Dolt repo)"
       label))))

(defun dorg-app--dolt-target (dir file dolt-dir)
  "Resolve a Dolt directory for a dolt command without prompting.
Thin guarded wrapper over `dorg-app--resolve-dolt-dir': a positional DIR
is checked by `dorg-app--reject-misbound-dir', and the usage error over
HTTP maps to 400 + the JSON envelope (`dorg-app--client-error-as-usage')."
  (dorg-app--client-error-as-usage
    (dorg-app--resolve-dolt-dir dir file dolt-dir "dorg dolt" t)))

(defun dorg-app--doctor-dolt-dir (dir file dolt-dir)
  "Resolve the Dolt repo dir for `doctor' without prompting.
Thin UNguarded wrapper over `dorg-app--resolve-dolt-dir' (a positional
DIR is taken as-is): `doctor' operates on the Dolt database and has no
plain-org form.  `dorg-app--doctor-run' wraps this in
`dorg-app--client-error-as-usage'."
  (dorg-app--resolve-dolt-dir dir file dolt-dir "dorg doctor" nil))

(defun dorg-app--doctor-run (file dolt-dir dir fix install)
  "Run `doctor' with already-resolved CLI/HTTP args; the command body.
INSTALL (or no repo target given) selects the environment/install
verification branch: `mysql.el' absence is an ERROR (exit 1); Dolt
absence/age is a WARN (exit 0).  Otherwise resolves the repo target and
runs the DB consistency checks; FIX repairs fixable findings in one
transaction then re-checks so the report and exit code reflect the
POST-fix state.  A DB inconsistency signals a structured command error so
the exit code is 1 in BOTH text and --json modes (a server fault =>
HTTP 500).  Wrapped in `dorg-app--client-error-as-usage' so a client-input
`user-error' over HTTP maps to 400 + the JSON envelope."
  (dorg-app--client-error-as-usage
    (if (or install (not (or dir file dolt-dir)))
        ;; Environment / install verification — no repo needed.  mysql.el
        ;; absence is an ERROR (exit 1); Dolt absence/age is a WARN (exit 0).
        (let ((findings (dorg-doctor--env-findings)))
          (clime-out-text (dorg-doctor--format-report findings "install environment"))
          (when (cl-some (lambda (f) (eq 'error (plist-get f :severity))) findings)
            (error "dorg-doctor: install verification failed (%s)"
                   (mapconcat (lambda (f) (format "%s" (plist-get f :check)))
                              (cl-remove-if-not
                               (lambda (f) (eq 'error (plist-get f :severity))) findings)
                              ", ")))
          ;; Clean/WARN-only: exit 0; return the object on the --json surface.
          (when (eq (clime-out-format) 'json)
            (dorg-app--doctor-data "install environment" (null findings) 0
                                   findings t)))
      (let* ((target (dorg-app--doctor-dolt-dir dir file dolt-dir))
             (fixable-p (lambda (fs) (cl-some (lambda (f) (plist-get f :fix-sql)) fs)))
             ;; Delegate run/apply/re-check to the shared core.  Its own
             ;; fixable guard makes `(lambda (_) fix)' byte-equivalent to the
             ;; old inline `(and fix (fixable-p findings))': the seam is only
             ;; consulted when a fixable finding exists.  --fix repairs in one
             ;; transaction, then re-checks so the report and exit code reflect
             ;; the POST-fix state.
             (result (dorg-doctor--run-fix-recheck target (lambda (_findings) fix)))
             (findings (plist-get result :findings))
             (applied (plist-get result :applied)))
        ;; Human report (text surface).
        (clime-out-text (dorg-doctor--format-report findings target))
        (when (> applied 0)
          (clime-out-text (format "Applied %d fix%s."
                                  applied (if (= applied 1) "" "es"))))
        (if findings
            ;; Inconsistent: signal a structured command error so the exit
            ;; code is 1 in BOTH output modes.  `dorg-app--with-json-error'
            ;; routes it: it re-signals a plain `error' on the text surface
            ;; (streams + exits 1) and hands the payload to the buffered
            ;; --json finalizer (reason `doctor-findings' + the doctor data).
            ;; A DB inconsistency is a server fault (usage nil => HTTP 500).
            (let ((msg (format "dorg-doctor: %d finding%s (%s)%s"
                               (length findings)
                               (if (= (length findings) 1) "" "s")
                               (mapconcat (lambda (f) (format "%s" (plist-get f :check)))
                                          findings ", ")
                               (if (and (not fix) (funcall fixable-p findings))
                                   "; run with --fix to repair" ""))))
              (dorg-app--with-json-error
                (dorg-app--command-error
                 'doctor-findings msg
                 (dorg-app--doctor-data target nil applied findings))))
          ;; Clean: exit 0; JSON object on the --json surface.
          (when (eq (clime-out-format) 'json)
            (dorg-app--doctor-data target t applied findings)))))))

;;; ─── Serve (HTTP) — runtime claim + auth helpers ───────────────────────

;; The `serve' group exposes the same query/edit commands over HTTP via the
;; vendored, OPTIONAL `clime-serve'.  It is loaded ONLY by the `serve start'
;; handler (lazy `require'), so the base CLI gains no HTTP dependency.  The
;; vendored web-server is left pristine; because it rejects a standard
;; `Authorization: Bearer' header, the auth token rides a `token' query
;; parameter or an `X-Dorg-Token' request header instead.

(defcustom dorg-serve-runtime-dir
  (expand-file-name "dorg/serve" temporary-file-directory)
  "Directory for same-host `dorg serve' HTTP runtime files.
Holds per-port 0600 state/token files for the local serve daemon.  This
is volatile runtime coordination state, not tracked org/Dolt data; it is
safe to delete when no server is running."
  :type 'directory
  :group 'dorg)

(defconst dorg-serve-claim-protocol 1
  "Version of the `dorg serve' runtime claim-file format.")

(defun dorg-app--serve-token-equal (a b)
  "Return non-nil iff strings A and B are equal, in (near) constant time.
The per-character comparison does not early-exit on the first mismatch,
so it does not leak the match position via timing.  Strings of differing
length are unequal (this does reveal length, acceptable for fixed-length
generated tokens)."
  (and (stringp a) (stringp b)
       (= (length a) (length b))
       (let ((diff 0))
         (dotimes (i (length a))
           (setq diff (logior diff (logxor (aref a i) (aref b i)))))
         (= diff 0))))

(defun dorg-app--serve-make-token ()
  "Return a fresh high-entropy hex token.
Reads 32 bytes from /dev/urandom when available, else falls back to a
mix of process/time/`random' entropy.  The result is a SHA-256 hex string."
  (let ((bytes
         (or (ignore-errors
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally "/dev/urandom" nil 0 32)
                 (buffer-string)))
             (format "%d-%S-%d-%d" (emacs-pid) (current-time)
                     (random most-positive-fixnum)
                     (random most-positive-fixnum)))))
    (secure-hash 'sha256 bytes)))

(defun dorg-app--serve-resolve-token (explicit)
  "Resolve the serve auth token.
Precedence: EXPLICIT (--token) > `DORG_SERVE_TOKEN' env > a freshly
generated token.  Returns the token string."
  (let ((env (getenv "DORG_SERVE_TOKEN")))
    (cond
     ((and explicit (not (string-empty-p explicit))) explicit)
     ((and env (not (string-empty-p env))) env)
     (t (dorg-app--serve-make-token)))))

(defun dorg-app--serve-loopback-p (host)
  "Return non-nil iff HOST is a loopback bind address."
  (member host '("127.0.0.1" "localhost" "::1")))

(defun dorg-app--serve-runtime-dir ()
  "Return the serve runtime directory.
Honors the `DORG_SERVE_RUNTIME_DIR' environment variable (e.g. to relocate
runtime files to `XDG_RUNTIME_DIR'); otherwise `dorg-serve-runtime-dir'."
  (or (let ((e (getenv "DORG_SERVE_RUNTIME_DIR")))
        (and e (not (string-empty-p e)) e))
      dorg-serve-runtime-dir))

(defun dorg-app--serve-state-file (port)
  "Return the runtime state-file path for PORT."
  (expand-file-name (format "serve-%d.state" port)
                    (dorg-app--serve-runtime-dir)))

(defun dorg-app--serve-token-file (port)
  "Return the runtime token-file path for PORT."
  (expand-file-name (format "serve-%d.token" port)
                    (dorg-app--serve-runtime-dir)))

(defun dorg-app--serve-pid-live-p (pid)
  "Return non-nil iff process PID currently exists on this host."
  (and (integerp pid) (> pid 0)
       (consp (process-attributes pid))))

(defun dorg-app--serve-read-claim (port)
  "Return PORT's parsed state alist, or nil when absent/unreadable."
  (let ((f (dorg-app--serve-state-file port)))
    (when (file-readable-p f)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents f)
          (let ((json-object-type 'alist)
                (json-key-type 'symbol))
            (json-read-from-string (buffer-string))))))))

(defun dorg-app--serve-claim-live-p (state)
  "Return non-nil iff STATE names a live, this-protocol serve process."
  (and state
       (equal (alist-get 'protocol state) dorg-serve-claim-protocol)
       (dorg-app--serve-pid-live-p (alist-get 'pid state))))

(defun dorg-app--serve-write-claim (port host token owner)
  "Write 0600 state + token files for PORT under a 0700 runtime dir.
HOST is the bind address, TOKEN the auth token, OWNER this claim's
owner-token (used for owner-checked removal)."
  (let ((rdir (dorg-app--serve-runtime-dir)))
    (unless (file-directory-p rdir)
      (make-directory rdir t))
    (set-file-modes rdir #o700))
  (let ((state `((pid . ,(emacs-pid))
                 (host . ,host)
                 (port . ,port)
                 (started-at . ,(format-time-string "%FT%T%z"))
                 (protocol . ,dorg-serve-claim-protocol)
                 (owner-token . ,owner))))
    ;; Create both files with 0600 from the start (no umask race).
    (with-file-modes #o600
      (with-temp-file (dorg-app--serve-state-file port)
        (insert (json-encode state) "\n"))
      (with-temp-file (dorg-app--serve-token-file port)
        (insert token)))))

(defun dorg-app--serve-remove-claim (port owner)
  "Delete PORT's state+token files iff OWNER matches the recorded owner.
Token-checked so a restart/pid-reuse race cannot clobber a newer claim.
A nil recorded owner (or absent state) permits cleanup of leftover files."
  (let* ((state (dorg-app--serve-read-claim port))
         (recorded (and state (alist-get 'owner-token state))))
    (when (or (null state) (null recorded) (equal owner recorded))
      (dolist (f (list (dorg-app--serve-state-file port)
                       (dorg-app--serve-token-file port)))
        (when (file-exists-p f) (ignore-errors (delete-file f)))))))

(defun dorg-app--serve-auth-predicate (token)
  "Return a `clime-serve' :auth predicate that requires TOKEN.
The presented token is read from a `token' query parameter or an
`X-Dorg-Token' request header (the vendored web-server rejects a standard
`Authorization: Bearer' header).  Comparison is constant-time."
  (lambda (req)
    (let* ((headers (plist-get req :headers))
           (presented (or (cdr (assoc "token" headers))
                          (cdr (assoc :X-DORG-TOKEN headers)))))
      (and (stringp presented)
           (dorg-app--serve-token-equal presented token)))))

(defun dorg-app--serve-emit (obj text)
  "Emit OBJ (an alist) as one JSON line in json mode, else the TEXT line.
Used by `serve start', which streams a single startup line and then
blocks; it must NOT also return a value that Clime would finalize."
  (if (eq (clime-out-format) 'json)
      (princ (concat (clime-json-encode (dorg-envelope-success-json obj)) "\n"))
    (princ (concat text "\n"))))

(defvar dorg-app--serve-exit-spec nil
  "Plist (:server OBJ :port PORT :owner TOKEN) for this process's serve
daemon, or nil.  :server is the EXACT object `clime-serve' returned, so
exit cleanup stops precisely this server and never an unrelated Clime
server sharing the process.  Read by the named `dorg-app--serve-exit-cleanup'
so its `kill-emacs-query-functions' registration stays a literal named
symbol rather than a per-start closure.")

(defun dorg-app--serve-exit-cleanup ()
  "Reap the LOCAL serve daemon this process started, on ordinary exit.
Registered on `kill-emacs-query-functions', which runs on ordinary
interactive exit but NOT when a fatal signal calls `kill-emacs' directly —
so a SIGTERM-during-GC runs no cleanup Lisp (tracked-regression).  Stops the
EXACT `clime-serve' object this process retained (never a no-arg stop that
could reap an unrelated in-process server) and, ONLY when that stop
confirms the local server is gone, removes this owner's claim files
(token-checked).  A stop `error'/`quit' cannot prove death, so the claim is
kept (fail-closed — never a raw-PID signal; a fatal-exit claim is left for
explicit later recovery).  Each seam is isolated against `error' and
`quit'; always returns t so it never vetoes exit."
  (let ((spec dorg-app--serve-exit-spec))
    (when spec
      (let ((stopped (condition-case nil
                         (progn (clime-serve-stop (plist-get spec :server)) t)
                       ((error quit) nil))))
        (when stopped
          (condition-case nil
              (dorg-app--serve-remove-claim (plist-get spec :port)
                                            (plist-get spec :owner))
            ((error quit) nil))))))
  t)

(defun dorg-app--serve-refuse-running (port existing)
  "Idempotent refusal: PORT already has a live serve claim EXISTING.
Emits the already-running line and, on the --json surface, returns the
refusal object.  Returns through Clime's normal finalizer (single
emission); does NOT bind or block."
  (clime-out-text
   (format "dorg serve: already running on port %d (pid %s)"
           port (alist-get 'pid existing)))
  (when (eq (clime-out-format) 'json)
    `((started . :json-false)
      (reason . "already-running")
      (pid . ,(alist-get 'pid existing))
      (port . ,port))))

(defun dorg-app--serve-bind (app port host token owner pred)
  "Bind APP's HTTP server on HOST:PORT and publish PORT's claim; return the server.
PRED is the `clime-serve' :auth predicate.  Captures the EXACT
`clime-serve' object and registers crash-safe ordinary-exit cleanup for it
through the NAMED `kill-emacs-query-functions' entry (tracked-regression) —
never the fatal `kill-emacs-hook', which a fatal signal skips — retaining
the object and registering BEFORE the fallible claim write so a publication
failure can never leak an untracked live server.  Late depth 99: query
functions run until the first nil, so cleanup runs only after every other
exit query agrees.  Then writes the claim under OWNER (token-checked).  On a
claim-write `error'/`quit', stops the EXACT server (guarding the stop
against BOTH conditions); drops the retained spec + query hook ONLY after a
confirmed clean stop, else KEEPS them so ordinary-exit cleanup can still
reap the live server, and surfaces BOTH the publication condition and any
cleanup failure — losing neither."
  ;; Public clime-serve (frcl): default-format json + format-aware 401.
  (let ((server (clime-serve app :port port :host host
                             :default-format 'json :auth pred)))
    (setq dorg-app--serve-exit-spec
          (list :server server :port port :owner owner))
    (add-hook 'kill-emacs-query-functions
              #'dorg-app--serve-exit-cleanup 99)
    (condition-case err
        (dorg-app--serve-write-claim port host token owner)
      ((error quit)
       (let (stop-err)
         (condition-case se
             (clime-serve-stop server)
           ((error quit) (setq stop-err se)))
         (if stop-err
             (signal (car err)
                     (list (format (concat "%s (and stopping the bound server after "
                                           "the failed claim write also failed: %s)")
                                   (error-message-string err)
                                   (error-message-string stop-err))))
           (setq dorg-app--serve-exit-spec nil)
           (remove-hook 'kill-emacs-query-functions
                        #'dorg-app--serve-exit-cleanup)
           (signal (car err) (cdr err))))))
    server))

(defun dorg-app--serve-publish (port host loopback)
  "Emit the serve startup status line for HOST:PORT then BLOCK forever.
LOOPBACK non-nil is a loopback bind; a non-loopback bind adds a
network-exposure warning.  Emits EXACTLY ONCE (`princ' via
`dorg-app--serve-emit'); the persistent loop then keeps the process alive
pumping the web-server's network events (`clime-run' is one-shot batch, so
Clime never finalizes a second line)."
  (dorg-app--serve-emit
   `((started . t)
     (pid . ,(emacs-pid))
     (host . ,host)
     (port . ,port)
     (token-file . ,(dorg-app--serve-token-file port))
     ,@(unless loopback
         '((warning . "bound beyond loopback; store is network-exposed"))))
   (format "dorg serve: listening on %s:%d (token in %s)%s"
           host port (dorg-app--serve-token-file port)
           (if loopback "" " [WARNING: non-loopback bind]")))
  (while t (accept-process-output nil 1)))

(defun dorg-app--serve-start (app port host file token-arg)
  "Start the HTTP serve daemon for APP on HOST:PORT and BLOCK until stopped.
FILE (when non-nil) becomes the process-wide default org source.
TOKEN-ARG is the --token value (may be nil).  Idempotent: refuses to
double-bind a live server on PORT.  Three phases: refuse a live claim
\(`dorg-app--serve-refuse-running'), else bind + publish the claim
\(`dorg-app--serve-bind') then emit status and block
\(`dorg-app--serve-publish')."
  (when dorg-app--serving
    (signal 'clime-usage-error
            (list "dorg serve: the serve lifecycle is not available over HTTP")))
  (let ((existing (dorg-app--serve-read-claim port)))
    (if (dorg-app--serve-claim-live-p existing)
        (dorg-app--serve-refuse-running port existing)
      ;; Fresh start.
      (let* ((token (dorg-app--serve-resolve-token token-arg))
             (owner (dorg-app--serve-make-token))
             (pred (dorg-app--serve-auth-predicate token))
             (loopback (dorg-app--serve-loopback-p host)))
        (when (and file (not (string-empty-p file)))
          (setenv "DORG_FILE" file))
        ;; `dorg-app--serving' is bound across the bind + loop so any
        ;; /serve/* request dispatched from within refuses (see guard above).
        (let ((dorg-app--serving t))
          (dorg-app--serve-bind app port host token owner pred)
          (dorg-app--serve-publish port host loopback))))))

(defun dorg-app--serve-stop (port)
  "Report the serve daemon on PORT; cross-process stop is unsupported (v0.1).
Sends NO raw-PID signal and deletes NO claim (tracked-regression, AC #9): a
serve daemon's owner token and process object live in the daemon's own
process, so this separate CLI invocation can neither prove local ownership
nor confirm the daemon is dead.  A live claim therefore FAILS CLOSED with a
clear error — graceful shutdown is the daemon's own crash-safe exit
cleanup, or later authenticated self-shutdown.  A dead/absent claim is
reported not-running WITHOUT reaping its files, because apparent death is
not proof: under the fail-closed contract a present but apparently-dead
claim blocks replacement until explicit recovery."
  (when dorg-app--serving
    (signal 'clime-usage-error
            (list "dorg serve: the serve lifecycle is not available over HTTP")))
  (let ((state (dorg-app--serve-read-claim port)))
    (if (not (dorg-app--serve-claim-live-p state))
        (progn
          (clime-out-text
           (format "dorg serve: not running on port %d" port))
          (when (eq (clime-out-format) 'json)
            `((stopped . :json-false) (running . :json-false) (port . ,port))))
      ;; A live claim is owned by the daemon's own process; cross-process stop
      ;; is not supported in v0.1.  Never signal a PID read from the claim and
      ;; never delete a foreign claim — fail closed with a clear message that
      ;; neither repeats nor acts on the claim's PID (identity/ownership
      ;; doctrine).  `serve status' remains the public diagnostic surface.
      (signal 'clime-usage-error
              (list (format (concat "dorg serve: a serve daemon is running on port %d; "
                                    "cross-process stop is not supported — stop it through "
                                    "its owning `dorg serve' process")
                            port))))))

(defun dorg-app--serve-status (port)
  "Report the local serve daemon on PORT as JSON/text; never leaks the token."
  (when dorg-app--serving
    (signal 'clime-usage-error
            (list "dorg serve: the serve lifecycle is not available over HTTP")))
  (let* ((state (dorg-app--serve-read-claim port))
         (running (and state (dorg-app--serve-claim-live-p state))))
    (clime-out-text
     (if running
         (format "dorg serve: running on %s:%d (pid %s, since %s)"
                 (alist-get 'host state) port (alist-get 'pid state)
                 (alist-get 'started-at state))
       (format "dorg serve: not running on port %d" port)))
    (when (eq (clime-out-format) 'json)
      (if running
          `((running . t)
            (pid . ,(alist-get 'pid state))
            (host . ,(alist-get 'host state))
            (port . ,port)
            (started-at . ,(alist-get 'started-at state)))
        `((running . :json-false) (port . ,port))))))

;;; ─── App ───────────────────────────────────────────────────────────────

;; The app symbol is `dorg-cli' (prefixed, so it lints clean as a dynamic
;; var); the executable presents as `dorg' via the wrapper's CLIME_ARGV0.
(clime-app dorg-cli
  :version dorg-version
  :env-prefix "DORG"
  :on-invocation #'dorg-app--log-invocation
  :help "Query, mutate, inspect locks, administratively import, and serve dorg-backed org trees."
  :examples '(("dorg query '(tag \"work\")' -f tasks.org" . "Headings tagged :work:")
              ("dorg property CATEGORY red -f tasks.org --json" . "Property match as JSON")
              ("dorg mutate '(on (id 100) (set (property \"STATUS\") \"done\"))' --apply -f tasks.org" . "Apply a mutation")
              ("dorg import -f source.org --dolt-dir .dorg/dolt --target-file projection.org --mode bootstrap" . "Explicit first import")
              ("dorg dolt list --json" . "DOrg-managed Dolt sql-servers as JSON")
              ("DORG_FILE=tasks.org dorg tag work" . "Source file from the environment"))

  ;; ── Root options (global; available to every subcommand) ─────────────

  (clime-output-format json ("--json")
    :help "Output results as JSON using the ok/data/error envelope"
    :finalize #'dorg-app--json-finalize
    :error-handler #'dorg-app--json-error)

  (clime-opt file ("--file" "-f")
    :env "FILE"                          ; :env-prefix "DORG" => DORG_FILE
    :coerce #'expand-file-name
    :help "Org source for query, mutate, or import (env: DORG_FILE)")

  (clime-opt dolt-dir ("--dolt-dir")
    :coerce #'expand-file-name
    :help "Dolt repo for explicit binding, import, or lifecycle commands")

  (clime-opt root-id ("--root-id")
    :type 'integer
    :help "Explicit Dolt root id for mutate or administrative commands")

  ;; ── import (explicit administrative whole-document ingest) ─────────

  (clime-command import
    :help "Administratively ingest one complete declared Org source; never an authoring/save path."
    :examples '(("dorg import -f source.org --dolt-dir .dorg/dolt --target-file canonical.org --mode bootstrap" . "First ingest")
                ("dorg import -f recovery.org --dolt-dir .dorg/dolt --target-file canonical.org --mode replace --base HASH" . "Revision-guarded recovery replace"))
    (clime-opt target-file ("--target-file")
      :coerce #'expand-file-name
      :help "Explicit canonical output file/root identity")
    (clime-opt mode ("--mode")
      :help "Required administrative mode: bootstrap|replace")
    (clime-opt base ("--base")
      :help "Exact expected HEAD; required for replace")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir root-id target-file mode base)
        (when dorg-app--serving
          (signal 'clime-usage-error
                  (list "dorg import is an administrative CLI-only operation")))
        (dorg-app--client-error-as-usage
          (when root-id
            (user-error
             "dorg import: --root-id is not accepted; use --target-file"))
          (unless file
            (user-error "dorg import: --file/-f SOURCE is required"))
          (unless dolt-dir
            (user-error "dorg import: --dolt-dir DIR is required"))
          (unless target-file
            (user-error "dorg import: --target-file FILE is required"))
          (unless (member mode '("bootstrap" "replace"))
            (user-error
             "dorg import: --mode must be bootstrap or replace"))
          (condition-case err
              (dorg-app--emit-import
               (apply
                #'dorg-import-document
                file
                (append
                 (list :dolt-dir dolt-dir
                       :target-file target-file
                       :mode (intern mode))
                 (when base (list :base base)))))
            (dorg-import-error
             (user-error "%s" (error-message-string err)))
            (dorg-transaction-error
             (user-error "%s" (error-message-string err))))))))

  ;; ── query (atomic aliases + composable v2 grammar) ───────────────────

  (clime-command query
    :help "Run one dorg-ql form — atomic alias or composable v2, e.g. '(property \"CATEGORY\" \"red\")' or '(pipe (where (tag \"work\")) (traverse children))'."
    :examples '(("dorg query '(tag \"work\")' -f tasks.org" . "Atomic tag predicate")
                ("dorg query '(title \"Inbox\")' -f tasks.org" . "Atomic exact title")
                ("dorg query '(pipe (where (deadline (this-week))) (traverse children))' -f tasks.org" . "Composable v2 pipeline (plain-org or Dolt)"))
    (clime-arg sexp
      :help "A single dorg-ql form — atomic alias or composable v2 (read as one Lisp expression)")
    (clime-opt extension ("--extension")
      :help "Load installed extension NAME through its standalone dorg entry point")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir sexp extension)
        (dorg-app--with-json-error
          (when extension (dorg-load-extension extension))
          (dorg-app--emit-query (dorg-app--read-exact-form sexp 'query)
                                (dorg-app--open file dolt-dir))))))

  ;; ── convenience predicate subcommands ────────────────────────────────

  (clime-command id
    :help "Find the heading whose DORG_ID is ID."
    (clime-arg id :type 'integer :help "Integer DORG_ID")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir id)
        (dorg-app--emit-query (list 'id id)
                              (dorg-app--open file dolt-dir)))))

  (clime-command property
    :help "Find headings whose property KEY equals VALUE."
    (clime-arg key :help "Property key")
    (clime-arg value :help "Property value")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir key value)
        (dorg-app--emit-query (list 'property key value)
                              (dorg-app--open file dolt-dir)))))

  (clime-command tag
    :help "Find headings carrying the local TAG."
    (clime-arg tag :help "Org tag (without colons)")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir tag)
        (dorg-app--emit-query (list 'tag tag)
                              (dorg-app--open file dolt-dir)))))

  (clime-command title
    :help "Find headings whose title is exactly TITLE."
    (clime-arg title :help "Exact heading title")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir title)
        (dorg-app--emit-query (list 'title title)
                              (dorg-app--open file dolt-dir)))))

  ;; ── mutate (public typed-IR write surface; replaces the removed set) ──

  (clime-command mutate
    :help (dorg-mutate-grammar-cli-help)
    :epilog (dorg-mutate-grammar-cli-epilog)
    :examples '(("dorg mutate '(on (id 100) (set (property \"STATUS\") \"done\"))' --dry-run -f tasks.org" . "Plan a mutation")
                ("dorg mutate '(on (id 100) (set (property \"STATUS\") \"done\"))' --apply -f tasks.org" . "Apply it")
                ("dorg mutate '(on (where (property \"STATUS\" \"OPEN\")) (set (property \"STATUS\") \"DONE\"))' --apply --expect-targets 101,102 -f tasks.org" . "Query-scoped apply, confirmed target set"))
    (clime-arg sexp :help "The mutate DSL form, read as one Lisp expression")
    (clime-opt dry-run ("--dry-run") :type 'boolean :help "Plan only; never write")
    (clime-opt apply ("--apply") :type 'boolean :help "Execute the mutation")
    (clime-opt mode ("--mode") :help "dry-run|apply (HTTP transport spelling)")
    (clime-opt expect-targets ("--expect-targets")
      :help "Comma-separated ids a prior dry-run resolved (query-scoped apply)")
    (clime-opt allow-broad ("--allow-broad") :type 'boolean
      :help "Permit an apply over more than the broad-write threshold")
    (clime-opt base ("--base") :help "Base revision/commit token (staleness check)")
    (clime-opt lock-timeout ("--lock-timeout")
      :help "Maximum seconds to wait for a saved plain-Org file lock")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir root-id sexp dry-run apply mode expect-targets
                       allow-broad base lock-timeout)
        (dorg-app--with-json-error
          (dorg-app--client-error-as-usage
            (let* ((form (dorg-app--read-exact-form sexp 'mutate))
                   (m (dorg-app--mutate-mode dry-run apply mode))
                   (source+buf (dorg-app--mutate-source file dolt-dir root-id))
                   (source (car source+buf))
                   (buf (cdr source+buf))
                   (expect (dorg-app--parse-expect-targets expect-targets))
                   (options
                    (append (list :mode m :source source
                                  :allow-broad allow-broad :base base)
                            ;; A file-backed apply persists inside its sole API
                            ;; execution.  No post-API save/sync/writeback path.
                            (when (and (eq m 'apply) buf) (list :save t))
                            (when lock-timeout
                              (list :lock-timeout
                                    (if (string-match-p
                                         "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'"
                                         lock-timeout)
                                        (string-to-number lock-timeout)
                                      lock-timeout)))
                            (unless (eq expect 'none)
                              (list :expected-targets expect))))
                   (resp (condition-case err
                             (apply #'dorg-api-execute form options)
                           ;; Command-envelope / validation errors are client
                           ;; input errors -> clean nonzero exit (HTTP 400); the
                           ;; reason/details ride the signalled condition.
                           (dorg-api-error
                            (dorg-app--command-error (dorg-api-error-reason err)
                                                     (cadr err)
                                                     (dorg-api-error-data err)
                                                     t)))))
              (dorg-app--emit-mutate resp)))))))

  ;; Explicit read-only operator inspection.  Unlike public contention errors,
  ;; this command intentionally includes owner host, PID and acquisition time.
  (clime-group lock
    :help "Inspect a local saved plain-Org writer gate."
    (clime-command inspect
      :help "Read the writer lock for one canonical Org file."
      (clime-arg path :coerce #'expand-file-name
        :help "Local Org file whose writer lock should be inspected")
      (clime-handler (ctx)
        (clime-let ctx (path)
          (let ((report (dorg-file-lock-inspect (dorg-file-lock-path path))))
            (clime-out-text (format "%S" report))
            (when (eq (clime-out-format) 'json)
              (list :command 'lock-inspect :report report)))))))

  ;; ── doctor (Dolt DB consistency diagnose/repair) ─────────────────────

  (clime-command doctor
    :help "Diagnose dorg DB consistency; exit 1 if inconsistent, --fix to repair."
    :examples '(("dorg doctor -f tasks.org" . "Report findings (exit 1 if any)")
                ("dorg doctor -f tasks.org --fix" . "Apply fixable findings, re-check")
                ("dorg doctor --dolt-dir .dorg/dolt --json" . "JSON (clean case)"))
    (clime-arg dir :optional
      :help "Dolt directory (else --dolt-dir, else --file's bound repo)")
    (clime-opt fix ("--fix") :type 'boolean
      :help "Apply fixable findings in one transaction, then re-check")
    (clime-opt install ("--install") :type 'boolean
      :help "Environment/install verification only (mysql.el + Dolt; no repo)")
    (clime-handler (ctx)
      (clime-let ctx (file dolt-dir dir fix install)
        (dorg-app--doctor-run file dolt-dir dir fix install))))

  ;; ── serve (HTTP serve surface over the same app) ─────────────────────

  (clime-group serve
    :help "Run/stop/inspect the local HTTP serve surface for this app.
Distinct from `server' (Dolt sql-servers): `serve' exposes the query/edit
commands over HTTP via the optional, lazily-loaded clime-serve.  Auth
requires a token via a `token' query parameter or an `X-Dorg-Token'
header (NOT a standard Authorization: Bearer header)."
    :examples '(("dorg serve start -f tasks.org" . "Serve on 127.0.0.1:8080")
                ("dorg serve status --json" . "Report the running server")
                ("dorg serve stop" . "Stop the local server"))

    (clime-command start
      :help "Start the HTTP server (blocks until stopped; token required)."
      (clime-opt port ("--port") :type 'integer
        :help "TCP port (default 8080)")
      (clime-opt bind ("--bind")
        :help "Bind address (default 127.0.0.1)")
      (clime-opt token ("--token")
        :help "Auth token (else DORG_SERVE_TOKEN env, else generated)")
      (clime-handler (ctx)
        ;; Lazy: the HTTP stack loads ONLY here, never for the base CLI.
        (require 'clime-serve)
        (clime-let ctx (file port bind token)
          (dorg-app--serve-start dorg-cli
                                 (or port 8080)
                                 (or bind "127.0.0.1")
                                 file token))))

    (clime-command stop
      :help "Stop the local HTTP server for --port (owner-safe; clean no-op)."
      (clime-opt port ("--port") :type 'integer
        :help "TCP port (default 8080)")
      (clime-handler (ctx)
        (clime-let ctx (port)
          (dorg-app--serve-stop (or port 8080)))))

    (clime-command status
      :help "Report the local HTTP server for --port as JSON/text."
      (clime-opt port ("--port") :type 'integer
        :help "TCP port (default 8080)")
      (clime-handler (ctx)
        (clime-let ctx (port)
          (dorg-app--serve-status (or port 8080))))))

  ;; ── server (DOrg-managed Dolt sql-server management) ─────────────────

  (clime-group dolt
    :help "Manage DOrg-managed Dolt sql-servers."
    :examples '(("dorg dolt list --json" . "Inventory as JSON")
                ("dorg dolt start /repo/dolt" . "Ensure a server for a Dolt dir")
                ("dorg dolt stop /repo/dolt" . "Owner-safe stop"))

    (clime-command list
      :help "List DOrg-managed Dolt sql-servers (read-only; never starts one)."
      (clime-handler (ctx)
        (ignore ctx)
        ;; Pure reader (`dorg-dolt-server-list'); each row is an array item in
        ;; JSON, a tab-separated line in text.
        (let ((entries (dorg-dolt-server-list)))
          (if (eq (clime-out-format) 'json)
              (list :command 'dolt-list
                    :servers (vconcat (mapcar #'dorg-app--dolt-json entries))
                    :count (length entries))
            (dolist (entry entries)
              (clime-out nil :text (dorg-app--dolt-text entry)))
            nil))))

    (clime-alias-for status (dolt list)
      :help "Alias for `dolt list'.")

    (clime-command start
      :help "Ensure a DOrg-managed Dolt sql-server and report its port."
      :examples '(("dorg dolt start /repo/dolt" . "Explicit Dolt dir")
                  ("dorg dolt start -f tasks.org" . "Derive from a bound file"))
      (clime-arg dir :optional
        :help "Dolt directory (else --dolt-dir, else --file's bound repo)")
      (clime-handler (ctx)
        (clime-let ctx (file dolt-dir dir)
          (let* ((target (dorg-app--dolt-target dir file dolt-dir))
                 ;; Explicit user start: a manual lifecycle operation.
                 (port (dorg-dolt-start target)))
            (clime-out-text (format "sql-server for %s ready on port %d"
                                    target port))
            (when (eq (clime-out-format) 'json)
              (list :command 'dolt-start
                    :dolt-dir target
                    :port port
                    :started t))))))

    (clime-command identify
      :help "Set a missing Dolt store identity; --fork replaces and records one."
      :examples '(("dorg dolt identify /repo/dolt" . "Identify an existing store")
                  ("dorg dolt identify /repo/dolt --fork" . "Fork its identity"))
      (clime-arg dir :optional
        :help "Dolt directory (else --dolt-dir, else --file's bound repo)")
      (clime-opt fork ("--fork") :type 'boolean
        :help "Replace the current store ID and record its previous value")
      (clime-handler (ctx)
        (clime-let ctx (file dolt-dir dir fork)
          (dorg-app--client-error-as-usage
            (let* ((target (dorg-app--dolt-target dir file dolt-dir))
                   (receipt (dorg-api-identify-store target :fork fork)))
              (clime-out-text
               (format "store %s identified as %s%s"
                       target (plist-get receipt :store-id)
                       (if (plist-get receipt :previous-id)
                           (format " (prior %s)"
                                   (plist-get receipt :previous-id))
                         "")))
              (when (eq (clime-out-format) 'json)
                (list :command 'dolt-identify :dolt-dir target
                      :store-id (plist-get receipt :store-id)
                      :previous-id (plist-get receipt :previous-id)
                      :fork (if (plist-get receipt :fork) t :json-false)
                      :commit (plist-get receipt :commit))))))))

    (clime-command stop
      :help "Owner-safe stop/detach of this Emacs's sql-server for a Dolt dir."
      (clime-arg dir :optional
        :help "Dolt directory (else --dolt-dir, else --file's bound repo)")
      (clime-handler (ctx)
        (clime-let ctx (file dolt-dir dir)
          (let* ((target (dorg-app--dolt-target dir file dolt-dir))
                 ;; Owner-safe: refuses to kill a live server owned by
                 ;; another DOrg process (signals a clean user-error).
                 (released (and (dorg-dolt-stop target) t)))
            (clime-out-text
             (if released
                 (format "released sql-server resources for %s" target)
               (format "no DOrg sql-server owned here for %s" target)))
            (when (eq (clime-out-format) 'json)
              (list :command 'dolt-stop
                    :dolt-dir target
                    :stopped (if released t :json-false)))))))

    (clime-command stop-all
      :help "Stop every Dolt sql-server this Emacs process owns."
      (clime-handler (ctx)
        (ignore ctx)
        (let ((n (dorg-dolt-stop-all)))
          (clime-out-text (format "stopped %d owned sql-server%s"
                                  n (if (= n 1) "" "s")))
          (when (eq (clime-out-format) 'json)
            (list :command 'dolt-stop-all :stopped n)))))

    (clime-command compact
      :help "Reclaim disk in a Dolt store via history-preserving gc (undo-safe)."
      :examples '(("dorg dolt compact /repo/dolt" . "gc a Dolt dir")
                  ("dorg dolt compact --shallow -f tasks.org" . "Fast newgen-only gc"))
      (clime-arg dir :optional
        :help "Dolt directory (else --dolt-dir, else --file's bound repo)")
      (clime-opt shallow ("--shallow") :type 'boolean
        :help "Faster newgen-only gc pass")
      (clime-handler (ctx)
        (clime-let ctx (file dolt-dir dir shallow)
          (let* ((target (dorg-app--dolt-target dir file dolt-dir))
                 ;; History-preserving gc: removes only unreachable chunks, so
                 ;; the dorg-history as-of/undo window is preserved.  Cannot
                 ;; shrink reachable history — reports before/after honestly.
                 (rep (dorg-dolt-compact target shallow)))
            (clime-out-text
             (format (concat "compacted %s — noms %s -> %s (reclaimed %s); "
                             "reachable history / undo window preserved")
                     target
                     (dorg-dolt-server--format-bytes (plist-get rep :before))
                     (dorg-dolt-server--format-bytes (plist-get rep :after))
                     (dorg-dolt-server--format-bytes (plist-get rep :reclaimed))))
            (when (eq (clime-out-format) 'json)
              (list :command 'dolt-compact
                    :dolt-dir target
                    :before (plist-get rep :before)
                    :after (plist-get rep :after)
                    :reclaimed (plist-get rep :reclaimed)))))))

    (clime-command rebind
      :help "Rebind one root to a new materialized file at an exact Dolt base."
      :examples '(("dorg dolt rebind /repo/dolt projection.org --root-id 42 --base HASH" . "Metadata-only relocation repair"))
      (clime-arg dir
        :help "Dolt directory")
      (clime-arg new-file
        :coerce #'expand-file-name
        :help "New source/materialized Org file")
      (clime-opt base ("--base")
        :help "Exact expected Dolt HEAD")
      (clime-handler (ctx)
        (clime-let ctx (dir new-file root-id base)
          (dorg-app--client-error-as-usage
            (unless root-id
              (user-error "dorg dolt rebind: --root-id is required"))
            (unless base
              (user-error "dorg dolt rebind: --base is required"))
            (condition-case err
                (dorg-app--emit-rebind
                 (dorg-rebind-file
                  (file-name-as-directory (expand-file-name dir))
                  root-id new-file base))
              (dorg-error
               (user-error "%s" (error-message-string err)))
              (dorg-transaction-error
               (user-error "%s" (error-message-string err))))))))

    (clime-alias-for ensure (dolt start)
      :help "Alias for `dolt start'.")))

(provide 'dorg-app)

;;; Entrypoint (batch only; no-op under `require' or interactive Emacs):
(when (clime-main-script-p 'dorg-cli)
  (clime-run-batch dorg-cli))

;;; dorg-app.el ends here
