;;; dorg-dolt.el --- Dolt CLI interface for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Low-level interface to Dolt SQL for dorg.  Provides functions for
;; executing SQL queries and committing changes.
;;
;; Public SQL transport routes through a managed `dolt sql-server' over the
;; MySQL wire protocol.  One-shot `dolt sql' remains only as an internal
;; test/recovery seam for standalone fixture stores.
;;
;; Server mode is the only public SQL transport and avoids DB load on each
;; query; any standalone one-shot CLI use must be explicitly pinned by the
;; internal seam.
;;
;; Repository initialization lives in `dorg-repo.el' (which requires
;; both this module and `dorg-ext') to avoid a circular dependency.

;;; Code:

(require 'json)
(require 'dorg-error)
(require 'dorg-schema)
(require 'cl-lib)
(require 'dorg-model)
(require 'dorg-sortkey)
(require 'dorg-backend)
;; SQL literal quoting is a dependency-free leaf; hard-requiring it keeps the
;; public `dorg-sql-quote' / `dorg-sql-escape-string' resolvable after
;; `(require \\='dorg-dolt)' even though their home file moved.
(require 'dorg-sql-quote)

;; Back-compat load contract: the hydration loaders moved to `dorg-model-load'
;; but stay reachable through `(require \\='dorg-dolt)' for external consumers
;; (census `migrate-caller-then-internalize'), so no downstream migration is
;; forced.  Autoload fires on first call; the symbols' home is the new module.
(autoload 'dorg-model-load-tree "dorg-model-load")
(autoload 'dorg-model-load-headings "dorg-model-load")

;; Standalone mysql.el is a HARD dependency (the native sql-server wire client);
;; `dorg-sql--require-mysql-hard' loads it at load time and fails fatally when
;; it is absent — the managed sql-server is dorg's only supported SQL transport.
(declare-function mysql-connect "mysql")
(declare-function mysql-disconnect "mysql" (conn))
(declare-function mysql-ping "mysql" (conn))
(declare-function mysql-query "mysql" (conn query))
(declare-function mysql-result-rows "mysql" (result))
(declare-function mysql-result-columns "mysql" (result))
(declare-function dorg-registration-backend "dorg-registry" (registration))
(declare-function dorg--preamble-keyword-alist "dorg-registry" (keys &optional buffer))

;;; ─── Error Types ─────────────────────────────────────────────────────

(define-error 'dorg-dolt-query-error
  "Dolt returned unparseable output" 'error)

(define-error 'dorg-sql-transport-error
  "DOrg SQL transport policy could not be satisfied" 'error)

(define-error 'dorg-sql-transport-conflict
  "DOrg SQL transport conflicts with a running sql-server" 'dorg-sql-transport-error)

;; ─── Session-affine lease errors (tracked-case) ───
;; A lease pins one exact mysql session for a linearized critical section (the
;; seam tracked-case's GET_LOCK linearization consumes).  These are typed so
;; callers can map them to a :reason contended / aborted contract.
(define-error 'dorg-sql-lease-contended
  "DOrg store is already exclusively leased" 'dorg-sql-transport-error)

(define-error 'dorg-sql-lease-broken
  "DOrg lease session broke; aborted fail-closed" 'dorg-sql-transport-error)

(define-error 'dorg-sql-lease-generation-drift
  "DOrg store generation changed while a lease was held; write refused"
  'dorg-sql-lease-broken)

(define-error 'dorg-sql-commit-indeterminate
  "DOrg Dolt commit outcome is indeterminate"
  'dorg-sql-lease-broken)

(define-error 'dorg-sql-transaction-rolled-back
  "DOrg SQL transaction failed and was rolled back" 'error)

(define-error 'dorg-sql-transaction-rollback-failed
  "DOrg SQL transaction and its rollback both failed" 'error)

(define-error 'dorg-sql-lease-busy
  "DOrg store is exclusively leased; ordinary SQL refused" 'dorg-sql-transport-error)

(define-error 'dorg-sql-lease-ownership-error
  "DOrg lease release/use failed exact-ownership validation" 'error)

(define-error 'dorg-sql-revision-unsafe
  "DOrg statement would resolve outside the revision scope; refused"
  'error)

;; Signaled by the mutation postimage validator (tracked-regression) inside the
;; pre-commit validate window when in-transaction readback diverges from the
;; validated candidate state.  It rides the ordinary batch rollback path, so
;; the transaction — data, derived rows, and evidence row alike — rolls back
;; with zero durable writes.  Data shape: (MESSAGE PLIST) where PLIST carries
;; only closed facts (`:plane' core|rows, optional `:ids' / `:table').
(define-error 'dorg-sql-postimage-parity
  "DOrg in-transaction readback diverged from the validated candidate" 'error)

(define-error 'dorg-sql-transaction-read-port-error
  "DOrg transaction read port refused a request" 'error)

(defconst dorg-sql-transaction-read-port-max-ids 1024
  "Maximum heading id count accepted by the internal transaction read port.")

(defconst dorg-sql-transaction-read-port-max-checks 256
  "Maximum check count of one batched extension count read.")

(defun dorg-sql-commit-indeterminate-data (err)
  "Return the sanitized fact plist carried by commit-indeterminate ERR.
The private condition retains its established flat (MESSAGE . PLIST) shape so
the administrative transaction boundary can preserve a known commit hash.  Its
facts are closed: `:transaction-phase' and optional `:commit' only."
  (when (eq (car-safe err) 'dorg-sql-commit-indeterminate)
    (let ((data (cddr err)))
      (and (listp data) data))))

(defun dorg-sql-transaction-rollback-data (err)
  "Return the sanitized fact plist carried by transaction condition ERR.
ERR must be `dorg-sql-transaction-rolled-back' or
`dorg-sql-transaction-rollback-failed'.  These private conditions use the
standard DOrg (MESSAGE PLIST) data shape but deliberately never retain the raw
database condition.  The plist includes the closed transaction phase recorded
before failure."
  (when (memq (car-safe err) '(dorg-sql-transaction-rolled-back
                               dorg-sql-transaction-rollback-failed))
    (let ((data (caddr err)))
      (and (listp data) data))))

(defconst dorg-dolt--postimage-parity-id-limit 32
  "Maximum mismatch IDs exposed in a sanitized parity diagnostic.")

(defun dorg-dolt--postimage-parity-id-facts (facts)
  "Return bounded closed core mismatch facts from parity FACTS, or nil.
Only a proper, nonempty, duplicate-free list of positive integer IDs is a
diagnostic identity fact.  Invalid input is omitted rather than repaired."
  (let ((ids (and (listp facts) (plist-get facts :ids))))
    (when (and (eq (plist-get facts :plane) 'core)
               (listp ids) ids
               (condition-case nil (>= (length ids) 1)
                 (wrong-type-argument nil))
               (cl-every (lambda (id) (and (integerp id) (> id 0))) ids)
               (= (length ids) (length (delete-dups (copy-sequence ids)))))
      (list :parity-ids (cl-subseq ids 0 (min (length ids)
                                               dorg-dolt--postimage-parity-id-limit))
            :parity-id-count (length ids)))))

(defun dorg-dolt--classify-db-error (err &optional failed-statement)
  "Return closed, sanitized database-failure facts for ERR and FAILED-STATEMENT.
This is the sole conversion point from raw MySQL condition text to facts used
by higher layers.  Its result contains only `:class', optional numeric
`:db-code', and an optional normalized `:constraint'; it never contains the raw
message, SQL, SQLSTATE prose, duplicate tuple/value, or an `:underlying' field.

Dolt's native 1062 sometimes names no index and reports only
\"duplicate unique key given: [PARENT,HEX-SORT-KEY]\".  FAILED-STATEMENT is the
exact statement active on the leased session; a composite duplicate from an
INSERT/UPDATE of the `headings' table is therefore unambiguously the table's
sole composite unique key, `uq_heading_sibling_order'.  Other tables and a
single-column/unknown statement remain deterministic constraints.

The closed classes are `serialization', `sibling-order-duplicate',
`deterministic-constraint', `postimage-parity', `mutation-validator', and
`database-failure'.
A typed `dorg-sql-postimage-parity' signal (tracked-regression) is classified
by its condition type BEFORE any message-text inspection — it is a DOrg
verdict, not a database condition.  Its core plane may carry a bounded,
validated mismatch-ID list and full count; rows-plane and invalid IDs carry no
identity facts."
  (cond
   ((eq (car-safe err) 'dorg-sql-postimage-parity)
    (let ((facts (car (last err))))
      (append (list :class 'postimage-parity)
              (when-let ((plane (and (listp facts)
                                     (plist-get facts :plane))))
                (list :parity-plane plane))
              (dorg-dolt--postimage-parity-id-facts facts))))
   ((eq (car-safe err) 'dorg-mutate-error)
    (let ((facts (caddr err)))
      (append (list :class 'mutation-validator)
              (when-let ((reason (and (listp facts)
                                      (plist-get facts :reason))))
                (list :mutation-reason reason))
              (when-let ((detail (and (listp facts)
                                      (plist-get facts :detail))))
                (list :mutation-detail detail)))))
   (t (dorg-dolt--classify-db-error-text err failed-statement))))

(defun dorg-dolt--classify-db-error-text (err failed-statement)
  "Classify a raw database condition ERR by its message text.
The text-parsing tail of `dorg-dolt--classify-db-error'; see there for the
closed classes and FAILED-STATEMENT semantics."
  (let* ((msg (condition-case nil
                  (error-message-string err)
                (error "")))
         (case-fold-search t)
         (code (and (string-match "\\[\\([0-9]+\\)\\]" msg)
                    (string-to-number (match-string 1 msg))))
         (sibling-order-p
          (and (= (or code 0) 1062)
               (or (string-match-p
                    "uq[_-]heading[_-]sibling[_-]order"
                    msg)
                   (and failed-statement
                        ;; The native unnamed spelling is useful only when it
                        ;; proves the two-column sibling tuple.  A one-column
                        ;; headings PRIMARY collision must remain a generic
                        ;; deterministic constraint.
                        (string-match-p
                         (concat "duplicate unique key given:[[:space:]]*"
                                 "\\[[^]\n]+,[^]\n]+\\]")
                         msg)
                        (string-match-p
                         (concat
                          "\\_<\\(?:INSERT[[:space:]]+INTO\\|UPDATE\\)"
                          "[[:space:]]+`?headings`?\\(?:[[:space:](]\\|\\'\\)")
                         failed-statement)))))
         (class
          (cond
           ((or (= (or code 0) 1213)
                (and (= (or code 0) 1105)
                     (string-match-p "constraint violations" msg)))
            'serialization)
           (sibling-order-p 'sibling-order-duplicate)
           ((memq code '(1062 3819 4025)) 'deterministic-constraint)
           (t 'database-failure))))
    (append (list :class class)
            (when code (list :db-code code))
            (when sibling-order-p
              (list :constraint 'uq-heading-sibling-order)))))

;;; Dolt Binary Detection

(defconst dorg-dolt-install-url "https://github.com/dolthub/dolt"
  "Where to point users when Dolt is missing or too old.")

(defconst dorg-dolt-minimum-version '(1 86 4)
  "Minimum supported Dolt version as a (MAJOR MINOR PATCH) list.
This is the oldest Dolt release dorg is tested against; older
binaries are rejected by `dorg-dolt--ensure-available'.")

(defun dorg-dolt-minimum-version-string ()
  "Return `dorg-dolt-minimum-version' as a \"MAJOR.MINOR.PATCH\" string."
  (mapconcat #'number-to-string dorg-dolt-minimum-version "."))

(defun dorg-dolt-available-p ()
  "Return non-nil if the dolt binary is available on PATH.
This is a cheap PATH-only check; it does not validate the Dolt
version.  Use `dorg-dolt-supported-p' for the version floor."
  (executable-find "dolt"))

;; KEEP (mutable-state): per-process probe cache, setq-mutated; must persist across calls.
(defvar dorg-dolt--version-cache 'unknown
  "Cached result of probing `dolt version'.
Either the symbol `unknown' (not yet probed) or a plist with a
`:raw' string and exactly one of `:version' (a (MAJOR MINOR PATCH)
list) on success or `:error' (a message string) on failure.
Reset with `dorg-dolt-reset-version-cache'.")

(defun dorg-dolt-reset-version-cache ()
  "Clear the cached `dolt version' probe result.
Forces the next supported-version check to re-spawn `dolt version'."
  (setq dorg-dolt--version-cache 'unknown))

(defun dorg-dolt--parse-version (output)
  "Parse OUTPUT of `dolt version' into a (MAJOR MINOR PATCH) list.
Matches the first dotted numeric version (e.g. \"1.86.1\" in a
line like \"dolt version 1.86.1\").  Returns nil when none found."
  (when (and output
             (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" output))
    (list (string-to-number (match-string 1 output))
          (string-to-number (match-string 2 output))
          (string-to-number (match-string 3 output)))))

(defun dorg-dolt--version-< (a b)
  "Return non-nil if version list A is older than version list B.
Compares (MAJOR MINOR PATCH) lists componentwise."
  (catch 'done
    (cl-mapc (lambda (x y)
               (cond ((< x y) (throw 'done t))
                     ((> x y) (throw 'done nil))))
             a b)
    nil))

(defun dorg-dolt--probe-version ()
  "Return the cached `dolt version' probe result, probing if needed.
Result is a plist: (:version LIST :raw STRING) on success or
(:error MESSAGE :raw STRING) when the version cannot be detected.
The result is cached in `dorg-dolt--version-cache' so repeated SQL
calls do not re-spawn `dolt version'."
  (if (not (eq dorg-dolt--version-cache 'unknown))
      dorg-dolt--version-cache
    (setq dorg-dolt--version-cache
          (condition-case err
              (with-temp-buffer
                (let* ((exit (call-process "dolt" nil t nil "version"))
                       (raw (string-trim (buffer-string)))
                       (version (dorg-dolt--parse-version raw)))
                  (cond
                   ((not (eq exit 0))
                    (list :error (format "`dolt version' exited with %s" exit)
                          :raw raw))
                   (version (list :version version :raw raw))
                   (t (list :error "could not parse a version number from output"
                            :raw raw)))))
            (error
             (list :error (format "`dolt version' could not be run: %s"
                                  (error-message-string err))
                   :raw nil))))))

(defun dorg-dolt-supported-p ()
  "Return non-nil if a supported Dolt version is on PATH.
Requires both PATH presence and a detected version at or above
`dorg-dolt-minimum-version'.  The probe result is cached."
  (and (dorg-dolt-available-p)
       (let ((version (plist-get (dorg-dolt--probe-version) :version)))
         (and version
              (not (dorg-dolt--version-< version dorg-dolt-minimum-version))))))

(defun dorg-dolt--ensure-available ()
  "Signal an error unless a supported Dolt binary is available.
Enforces PATH presence and the `dorg-dolt-minimum-version' floor.
Error text identifies the binary path, the detected version or
the raw/unparseable output, the required minimum, and the install
URL.  The detected version is cached via `dorg-dolt--probe-version'
so normal SQL calls do not re-spawn `dolt version'."
  (let ((path (dorg-dolt-available-p)))
    (unless path
      (error "dolt binary not found on PATH.  Install Dolt %s or newer from %s"
             (dorg-dolt-minimum-version-string) dorg-dolt-install-url))
    (let* ((probe (dorg-dolt--probe-version))
           (version (plist-get probe :version)))
      (cond
       ((null version)
        (let ((raw (plist-get probe :raw)))
          (error "Could not determine Dolt version at %s: %s%s.  dorg requires Dolt %s or newer (%s)"
                 path
                 (or (plist-get probe :error) "unknown error")
                 (if (and raw (not (string-empty-p raw)))
                     (format "; output: %s" raw)
                   "")
                 (dorg-dolt-minimum-version-string) dorg-dolt-install-url)))
       ((dorg-dolt--version-< version dorg-dolt-minimum-version)
        (error "Dolt %s at %s is unsupported; dorg requires %s or newer.  Upgrade from %s"
               (mapconcat #'number-to-string version ".")
               path
               (dorg-dolt-minimum-version-string) dorg-dolt-install-url))))))

(defun dorg-dolt--trim-output (output &optional max-lines)
  "Return subprocess OUTPUT trimmed for inclusion in an error message.
Whitespace is stripped.  When the text exceeds MAX-LINES (default
40) lines only the trailing MAX-LINES are kept, prefixed with a
truncation marker.  Returns \"(no output)\" when empty."
  (let ((trimmed (string-trim (or output ""))))
    (if (string-empty-p trimmed)
        "(no output)"
      (let* ((lines (split-string trimmed "\n"))
             (limit (or max-lines 40)))
        (if (<= (length lines) limit)
            trimmed
          (concat "...\n" (mapconcat #'identity (last lines limit) "\n")))))))

;;; Project Store Convention and Dolt Target Normalization

(defun dorg-default-store-directory (project-root)
  "Return the default DOrg store directory below PROJECT-ROOT.

PROJECT-ROOT must be a non-empty absolute local path supplied explicitly by
the caller.  The result is the normalized, slash-terminated path
`<project>/.dorg/dorg/'.  The outer `.dorg' is the project control directory;
the inner `dorg' is the Dolt repository leaf and therefore its locally served
database name.

This is a pure path helper: it does not consult project.el, implicit directory
or environment state; create directories; initialize or discover a store;
bind a source; or start a process.  Explicit legacy and custom store paths do
not pass through this helper and retain their existing basename-derived
database identity."
  (unless (and (stringp project-root)
               (not (string-empty-p project-root))
               (file-name-absolute-p project-root)
               (not (file-remote-p project-root)))
    (error "dorg: project root must be a non-empty absolute local path: %S"
           project-root))
  (file-name-as-directory
   (expand-file-name ".dorg/dorg" project-root)))

(defun dorg-dolt--target (dolt-dir)
  "Return the normalized Dolt process target for DOLT-DIR.

The returned plist separates concepts Dolt itself treats differently:

- `:repo-dir' is the target Dolt repository directory used as cwd for
  direct one-shot SQL;
- `:server-data-dir' is the `dolt sql-server --data-dir' root: the store
  itself, served as a single database named after its basename;
- `:db-name' is that database name;
- `:cfg-dir' is the repo-local Dolt config directory.

Pointing `--data-dir' AT the store (rather than its parent) keeps every
server-level artifact dolt materializes — `.dolt/sql-server.info',
`.doltcfg', `config.yaml' — INSIDE the store, exactly where a standalone
`dolt sql-server' in a repo already puts them.  For the canonical default
`.dorg/dorg/' this leaves the outer project control directory clean: Dolt
serves the store as database `dorg' (its basename) with no parent-level
`.dolt' or `.doltcfg' that the one-shot CLI paths would otherwise collide
with.  Explicit legacy stores such as `.tracker/dolt/' keep the same law and
therefore continue to serve database `dolt'
\(\"multiple .doltcfg directories detected\").

`:repo-dir' and `:cfg-dir' preserve the caller's logical expanded path
for direct CLI use.  `:server-data-dir' and `:db-name' are derived from
the physical canonical repo path so the served database name and MySQL
connection database stay aligned when symlinks are involved."
  (let* ((repo-dir (file-name-as-directory (expand-file-name dolt-dir)))
         (canonical-repo-dir (file-truename repo-dir))
         (canonical-repo-noslash (directory-file-name canonical-repo-dir))
         (db-name (file-name-nondirectory canonical-repo-noslash))
         (server-data-dir canonical-repo-dir))
    (list :repo-dir repo-dir
          :canonical-repo-dir canonical-repo-dir
          :server-data-dir server-data-dir
          :db-name db-name
          :cfg-dir (expand-file-name ".doltcfg" repo-dir))))

(defun dorg-dolt--target-doltcfg-args (target)
  "Return Dolt global config args for normalized TARGET."
  (list "--doltcfg-dir" (plist-get target :cfg-dir)))

(defun dorg-dolt--target-sql-args (target &rest sql-args)
  "Return Dolt argv suffix for `dolt sql' against TARGET.
SQL-ARGS are appended after the `sql' subcommand."
  (append (dorg-dolt--target-doltcfg-args target)
          (cons "sql" sql-args)))

(defun dorg-dolt--target-sql-server-args (target port)
  "Return Dolt argv suffix for `dolt sql-server' serving TARGET on PORT.
`--doltcfg-dir' is a `sql-server' SUBCOMMAND flag here, not the global
prefix used by `dorg-dolt--target-sql-args': `dolt sql-server' has its
own `--doltcfg-dir' and ignores the global one (it would otherwise
default to `$data-dir/.doltcfg').  Passing it after the subcommand pins
config inside the store explicitly."
  (list "sql-server"
        "--host" "127.0.0.1"
        "--port" (number-to-string port)
        "--data-dir" (plist-get target :server-data-dir)
        "--doltcfg-dir" (plist-get target :cfg-dir)))

(defun dorg-dolt--target-sql-server-command (target port)
  "Return a diagnostic command string for `dolt sql-server' TARGET on PORT."
  (mapconcat #'shell-quote-argument
             (cons "dolt" (dorg-dolt--target-sql-server-args target port))
             " "))

(defun dorg--canonical-existing-case (path)
  "Return PATH with each existing component rewritten to its on-disk letter-case.
`file-truename' resolves symlinks but, on a case-insensitive volume, echoes the
caller's letter-case verbatim — it never repairs a mis-cased component.  A
binding created through a case-alias of the real path therefore bakes the
alias case into `dorg_files.relative_path' and the rendered
`#+DORG_DOLT_DIR:'; that works on a case-insensitive host but does not resolve
on a case-sensitive one (tracked-regression).

This walks PATH from the root and, for each component that ALREADY EXISTS on
disk, substitutes the actual directory-entry spelling (matched
case-insensitively).  A component that does not yet exist is kept verbatim, so a
not-yet-created leaf (e.g. a fresh Dolt store dir) keeps the caller's chosen
case and stays consistent once created.  Symlinks are NOT resolved here — case
correction is layered ON TOP of the caller's `file-truename', preserving its
symlink semantics and only repairing case.

On a genuinely case-sensitive volume PATH is returned unchanged: there exact
spelling is authoritative and folding would be wrong.  Remote (TRAMP) and
non-string PATHs are also returned unchanged.  PATH should already be
absolute (callers pass `file-truename' output)."
  (if (or (not (stringp path))
          (file-remote-p path)
          (not (file-name-case-insensitive-p path)))
      path
    (let ((acc "/")
          (had-slash (and (> (length path) 1)
                          (eq (aref path (1- (length path))) ?/))))
      (dolist (part (split-string (expand-file-name path) "/" t))
        (let* ((parent (directory-file-name acc))
               (real (and (file-accessible-directory-p parent)
                          (catch 'found
                            (dolist (f (ignore-errors
                                         (directory-files parent nil nil t)))
                              (when (string= (downcase f) (downcase part))
                                (throw 'found f)))
                            nil))))
          (setq acc (concat (file-name-as-directory acc) (or real part)))))
      (if had-slash (file-name-as-directory acc) acc))))

;;; SQL Execution

(autoload 'dorg-dolt-server--db-name "dorg-dolt-server")
(autoload 'dorg-dolt-server--canonical-dir "dorg-dolt-server")
;; Endpoint-discovery selectors used by the connection/lease cache below.  These
;; remain resident autoloads because that discovery coupling is intentionally
;; NOT severed here (owned by the dead-declarations / endpoint-ownership scope);
;; dorg-dolt must not `require' dorg-dolt-server (which requires dorg-dolt).  The
;; claim-classification selectors moved OUT with the transport policy to
;; `dorg-transport' (tracked-case), so control flows downward there.
(autoload 'dorg-dolt-server--strict-port "dorg-dolt-server")
(autoload 'dorg-dolt-server--dolt-server-info "dorg-dolt-server")
(autoload 'dorg-dolt-server--kill-process "dorg-dolt-server")
(autoload 'dorg-dolt-server--evict-attached-port "dorg-dolt-server")

;;; ─── Persistent MySQL Connections ──────────────────────────────────────

;; Endpoint ownership (tracked-case): the persistent connection and the endpoint
;; that created it are ONE cache entry.  The port and identity (store db-name +
;; sql-server.info instance UUID) captured at open bind the connection to its
;; resolved endpoint AND to the exact sql-server instance, so a failure can evict
;; both together, a silent same-port instance swap is caught by first-use identity
;; validation, and the indefinite attached-port memo is no longer independently
;; authoritative on the hot path (it is only a discovery hint while the bound
;; connection is live and identity-valid).
(cl-defstruct (dorg-sql--conn-entry (:constructor dorg-sql--make-conn-entry)
                                    (:copier nil))
  conn        ; the live `mysql-conn'
  port        ; the TCP port it was opened against
  identity    ; (:db DB :uuid UUID): store db-name + `sql-server.info' instance
              ; UUID (or nil UUID) captured at open — the store+instance identity
  generation  ; read-only directory/native/source fingerprint captured at open
  validated)  ; non-nil once first-use identity validation has passed

;; KEEP (mutable-state): persistent mysql connection pool, puthash-mutated across calls.
(defvar dorg-sql--connections (make-hash-table :test 'equal)
  "Map of canonical dolt-dir to a `dorg-sql--conn-entry' (conn + endpoint).")

(defun dorg-sql--bundled-mysql-dir ()
  "Return the bundled mysql directory path, or nil if not present."
  (let (seen)
    (catch 'found
      (dolist (this-file (list (locate-library "dorg-dolt")
                               load-file-name
                               buffer-file-name))
        (when (and this-file (not (member this-file seen)))
          (push this-file seen)
          (let* ((source-dir (file-name-directory this-file))
                 (mysql-dir (and source-dir
                                 (expand-file-name "lib/mysql" source-dir))))
            (when (and mysql-dir
                       (file-directory-p mysql-dir)
                       (file-exists-p (expand-file-name "mysql.el" mysql-dir)))
              (throw 'found mysql-dir))))))))

(defun dorg-sql--require-mysql-hard ()
  "Load standalone mysql.el, signaling a clear error if it cannot be loaded.
mysql.el (the native sql-server wire client) is a HARD dependency of dorg:
the managed `dolt sql-server' is the only supported SQL transport, so there
is no CLI-only fallback (see tracked-case / tracked-case).  Ensures the bundled
`lib/mysql' shipped next to the dorg sources is on `load-path' and then
hard-loads mysql, signaling an actionable error when it is absent.

Called as a top-level form when `dorg-dolt' loads, so loading `dorg-dolt'
\(hence `dorg') fails fatally on a host without mysql.el rather than silently
degrading to one-shot CLI."
  (let ((mysql-dir (dorg-sql--bundled-mysql-dir)))
    (when (and mysql-dir (not (member mysql-dir load-path)))
      (add-to-list 'load-path mysql-dir))
    ;; `condition-case', not `require's NOERROR: NOERROR only suppresses the
    ;; missing-file case and returns nil, but a present-yet-broken mysql.el
    ;; (corrupt, byte-comp skew, an error raised during load) SIGNALS mid-load
    ;; and would escape raw.  Catch BOTH and rewrap into the hard-dependency
    ;; diagnostic, preserving the underlying error, so no loader message ever
    ;; escapes without mysql.el/HARD-dependency/no-CLI context.
    (condition-case err
        (require 'mysql)
      (error
       (error "%s" (dorg-sql--mysql-hard-error-message
                    mysql-dir (error-message-string err)))))))

(defun dorg-sql--mysql-hard-error-message (mysql-dir underlying)
  "Return the actionable fatal message for a missing/broken mysql.el.
MYSQL-DIR is the bundled dir that was tried (or nil); UNDERLYING is the
load error text.  Names the DOrg installer as the remediation so the
hard-dependency failure points at the fix, and preserves the mysql.el /
HARD-dependency / lib/mysql context."
  (concat
   "dorg: standalone mysql.el is required but could not be loaded. "
   "mysql.el (the sql-server wire client) is a HARD dependency: the managed "
   "sql-server is dorg's only supported SQL transport, with no CLI-only "
   "fallback. "
   (if mysql-dir
       (format "Tried the bundled copy under %s. " mysql-dir)
     "No bundled lib/mysql/mysql.el was found next to the dorg sources. ")
   "Restore it by re-running the DOrg installer (install.sh) or `make install' "
   "in the source tree -- both verify and restore the bundled lib/mysql -- or "
   "put mysql.el on `load-path' manually. "
   (format "Underlying error: %s" underlying)))

(defun dorg-sql--mysql-available-p ()
  "Return non-nil when mysql.el is loaded.
In a normally-loaded dorg process this is ALWAYS non-nil:
`dorg-sql--require-mysql-hard' hard-loads mysql.el as a top-level form when
`dorg-dolt' loads, and every dorg entry point requires `dorg-dolt', so dorg
cannot load without mysql.el.  Retained as the single explicit predicate seam
for tests — those that skip when mysql.el is genuinely absent from the test
host, and those that stub a degraded environment."
  (featurep 'mysql))

(defcustom dorg-sql-connect-timeout 2.0
  "Seconds bounding a mysql.el connect to a DOrg sql-server.
A wedged server accepts the TCP connection but may stall the MySQL
handshake; without a bound the client parks on mysql.el's default.  Kept
small so a connect to a wedged server fails fast."
  :type 'number
  :group 'dorg)

(defcustom dorg-sql-read-timeout 10.0
  "Seconds of read inactivity tolerated on a mysql.el query before failing.
Bounds the idle wait inside a single query so a server that accepts the
connection but never returns rows cannot hang a read indefinitely.
Generous enough for a large legitimate read, but finite — a wedged server
raises `mysql-timeout' within this window instead of blocking forever."
  :type 'number
  :group 'dorg)

;;; ─── Private capture of failed batch statements ────────────────────────────
;;
;; The sanitized rollback receipt keeps only closed facts, by design: the
;; classifier never lets raw server prose, SQL text or row values cross into
;; the public refusal.  That boundary also discards the one thing an operator
;; needs to diagnose an unexplained `database-failure': the original
;; condition and the exact statement that failed.  The capture below records
;; those privately, unless the operator turns the capture off, into a file
;; under the store's own runtime overlay; the public receipt gains only
;; closed correlation facts (execution ordinal, verb/table class, capture id).

(defcustom dorg-sql-capture-failed-statements t
  "When non-nil, record the original condition of a failed batch statement.
On by default (operator decision, 2026-09-12); bind or set it to nil to turn
the capture off.  When set, every statement-phase database failure inside a
typed mutation batch writes ONE private file under the owning store's
runtime overlay, `<store>/.dorg/failures/<capture-id>.eld', containing the
raw server message, the exact failed SQL statement, its execution ordinal
and the closed transaction facts.

SENSITIVE CONTENTS: the raw message and SQL may quote row values, bead
text and identifiers.  The file is created exclusively with mode 0600
inside a 0700 directory and is never copied into the public receipt, the
error message, stdout or the tracker; the receipt names only the capture
id.  Capture happens after the same-session ROLLBACK and never changes the
refusal, its classification, retry decisions or database effects; a
capture that cannot be written is reported as `:capture failed' and
leaves no partial file.  Old capture files are never removed
automatically."
  :type 'boolean
  :group 'dorg)

(defvar dorg-sql--capture-store-dir nil
  "The owning store of the batch currently executing on a leased session.
Bound by `dorg-sql--lease-batch' from the lease's exact store identity so a
failure can be captured without rediscovering a store through the working
directory or opening another connection.  Nil outside a leased batch, in
which case no capture is attempted.")

(defconst dorg-sql--public-table-set
  '("dorg_files" "dorg_meta" "dorg_mutation_log" "drawer_items" "drawer_lines"
    "heading_items" "heading_tags" "headings" "keyword_items" "planning_items"
    "property_entries" "text_items")
  "The explicit finite set of core table names a public statement class may name.
A failed statement's table is reported by one of these literal strings or as
the symbol `unknown'; nothing is ever copied out of the SQL text itself, so an
arbitrary identifier (extension tables included) cannot cross the privacy
boundary through the class.  The raw statement stays in the private capture.")

(defun dorg-sql--statement-class (sql)
  "Return the closed (VERB TABLE) class of SQL, or (unknown nil).
VERB is one of the symbols `insert', `update' or `delete'.  TABLE is the
member of `dorg-sql--public-table-set' that the statement names (that set's
own string, never a substring of SQL), or the symbol `unknown' for any other
identifier, quoted or not.  Other verbs, leading comments and odd shapes are
\(unknown nil).  The class is a proper two-element list so the public error
envelope can encode it as a JSON array.  No fragment of the statement is ever
returned."
  (let ((case-fold-search t)
        (identifier "\\(?:`\\([A-Za-z_][A-Za-z0-9_]*\\)`\\|\\([A-Za-z_][A-Za-z0-9_]*\\)\\)"))
    (if (and (stringp sql)
             (string-match
              (concat "\\`[[:space:]]*"
                      "\\(?:\\(insert\\)[[:space:]]+into\\|\\(update\\)\\|\\(delete\\)[[:space:]]+from\\)"
                      "[[:space:]]+" identifier
                      "\\(?:[[:space:](]\\|\\'\\)")
              sql))
        (let* ((verb (cond ((match-beginning 1) 'insert)
                           ((match-beginning 2) 'update)
                           (t 'delete)))
               (named (downcase (or (match-string 4 sql) (match-string 5 sql))))
               (public (car (member named dorg-sql--public-table-set))))
          (list verb (or public 'unknown)))
      (list 'unknown nil))))

(defun dorg-sql--capture-path-owner-uid (path)
  "Return the numeric owner uid of PATH without following a final symlink.
A seam for the custody checks below; nil when PATH has no attributes."
  (file-attribute-user-id (file-attributes path 'integer)))

(defun dorg-sql--capture-dir-custody-ok-p (dir forbidden-bits)
  "Return non-nil when the existing directory DIR is safe to write under.
DIR must be a real directory (not a symlink), owned by this process's user,
with none of FORBIDDEN-BITS set in its permission bits: `#o022' rejects a
group- or world-writable parent, `#o077' demands a private directory.
Nothing is created or changed; an unsafe DIR is simply refused."
  (and (file-directory-p dir)
       (not (file-symlink-p dir))
       (eql (dorg-sql--capture-path-owner-uid dir) (user-uid))
       (zerop (logand forbidden-bits (or (file-modes dir) #o777)))))

(defun dorg-sql--capture-failed-statement (store err statement index phase code)
  "Privately record ERR and STATEMENT under STORE's runtime overlay.
INDEX is the execution ordinal, PHASE the closed transaction phase and
CODE the numeric database code, if any.  Returns the capture id (the file's
base name) on success, or nil on ANY failure, after removing only a file
this invocation itself created.

Custody of the destination is checked before anything is created or
written: `<store>/.dorg' must be an owned real directory that is not group-
or world-writable, `<store>/.dorg/failures' an owned real directory with no
group or other bits (either is created 0700 only when missing), and the
exclusively created 0600 capture file must still be an owned plain file
when the payload is written.  Anything else is refused without adjusting
the modes of any existing tree.  Must be called only after the same-session
ROLLBACK has completed; it issues no SQL and touches nothing outside
`<store>/.dorg/failures/'."
  (let ((file nil))
    (condition-case nil
        (let* ((store (file-name-as-directory (expand-file-name store)))
               (overlay (expand-file-name ".dorg" store))
               (dir (expand-file-name "failures" overlay)))
          (unless (file-directory-p store) (error "no store"))
          (when (file-symlink-p overlay) (error "overlay is a link"))
          (unless (file-exists-p overlay)
            (with-file-modes #o700 (make-directory overlay)))
          (unless (dorg-sql--capture-dir-custody-ok-p overlay #o022)
            (error "overlay custody"))
          (when (file-symlink-p dir) (error "capture dir is a link"))
          (unless (file-exists-p dir)
            (with-file-modes #o700 (make-directory dir)))
          (unless (dorg-sql--capture-dir-custody-ok-p dir #o077)
            (error "capture dir custody"))
          ;; Exclusive 0600 creation before any payload is written, then the
          ;; created name is re-checked as an owned plain file before use.
          (setq file (with-file-modes #o600
                       (make-temp-file (expand-file-name "cap-" dir) nil ".eld")))
          (unless (and (file-regular-p file)
                       (not (file-symlink-p file))
                       (eql (dorg-sql--capture-path-owner-uid file) (user-uid))
                       (= #o600 (logand #o777 (or (file-modes file) 0))))
            (error "capture file custody"))
          (with-temp-buffer
            (prin1 (list :captured-at (format-time-string "%FT%T%z")
                         :transaction-phase phase
                         :db-code code
                         :statement-index index
                         :statement statement
                         :message (condition-case nil
                                      (error-message-string err)
                                    (error "")))
                   (current-buffer))
            (insert "\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) file nil 'silent)))
          (file-name-base file))
      (error
       (when (and file (file-exists-p file))
         (ignore-errors (delete-file file)))
       nil))))

(defun dorg-sql--probe-healthy-p (port timeout)
  "Return t if a sql-server on PORT answers `SELECT 1' within TIMEOUT.
Opens a short-lived, no-database mysql.el connection bounded by TIMEOUT
for both connect and read, runs the trivial round-trip, and disconnects.
Returns nil on any timeout/refusal/error and never raises.  Backs
`dorg-dolt-server--port-healthy-p'.  mysql.el is a hard load-time
dependency of a running dorg process (`dorg-sql--require-mysql-hard'),
so callers need no availability gate before invoking it."
  (let ((conn nil))
    (unwind-protect
        (condition-case nil
            (progn
              (setq conn (mysql-connect :host "127.0.0.1"
                                        :port port
                                        :user "root"
                                        :tls nil
                                        :connect-timeout timeout
                                        :read-idle-timeout timeout))
              (mysql-query conn "SELECT 1")
              t)
          (error nil))
      (when conn (ignore-errors (mysql-disconnect conn))))))

(defun dorg-sql--entry-ping-ok-p (entry)
  "Return non-nil if ENTRY's connection answers a `mysql-ping'."
  (condition-case nil
      (progn (mysql-ping (dorg-sql--conn-entry-conn entry)) t)
    (error nil)))

(defun dorg-sql--evict-conn-entry (key entry)
  "Compare-and-remove ENTRY from the pool for KEY, disconnect, drop its port memo.
Removes the pool slot only if the live entry is still `eq' ENTRY, so an older
failure cannot erase a newer connection another path just installed
\(endpoint-ownership compare-and-remove).  Always disconnects ENTRY's own
connection and evicts the matching attached-port hint for its exact port."
  (when (eq (gethash key dorg-sql--connections) entry)
    (remhash key dorg-sql--connections))
  (ignore-errors (mysql-disconnect (dorg-sql--conn-entry-conn entry)))
  (dorg-dolt-server--evict-attached-port key (dorg-sql--conn-entry-port entry)))

(defun dorg-sql--directory-identity (dolt-dir)
  "Return DOLT-DIR's serializable device/inode identity, or nil.
This is intentionally a local filesystem read: a pathname may be reused after
a store replacement, while its directory identity distinguishes generations."
  (condition-case nil
      (let ((attrs (file-attributes (dorg-dolt-server--canonical-dir dolt-dir))))
        (list :device (file-attribute-device-number attrs)
              :inode (file-attribute-inode-number attrs)))
    (error nil)))

(defun dorg-sql--bound-source-hash (dolt-dir)
  "Return DOLT-DIR's live bound Org buffer `DORG_HASH', else nil.
Never visits a file or performs SQL: only an already-loaded explicitly bound
buffer contributes its genuine root-preamble keyword."
  (when (boundp 'dorg--registry)
    (let ((canon (dorg-dolt-server--canonical-dir dolt-dir)))
      (cl-loop for (file . registration) in dorg--registry
               for backend = (dorg-registration-backend registration)
               for registered-dir = (dorg-backend-dolt-dir backend)
               for buffer = (and registered-dir
                                 (or (get-file-buffer file)
                                     (find-buffer-visiting file)))
               when (and buffer
                         (equal canon
                                (dorg-dolt-server--canonical-dir registered-dir)))
               return (cdr (assoc dorg-hash-keyword
                                  (dorg--preamble-keyword-alist
                                   (list dorg-hash-keyword) buffer)))))))

(defun dorg-sql--generation-fingerprint (dolt-dir)
  "Return DOLT-DIR's read-only cache generation fingerprint.
The members are the canonical directory identity, native sql-server PID:port,
and the bound source buffer's `DORG_HASH'.  This deliberately excludes HEAD:
ordinary reads must not issue product SQL merely to validate a warm entry."
  (let ((info (dorg-dolt-server--dolt-server-info dolt-dir)))
    (list :directory (dorg-sql--directory-identity dolt-dir)
          :native (list :pid (plist-get info :pid) :port (plist-get info :port))
          :source-hash (dorg-sql--bound-source-hash dolt-dir))))

(defun dorg-sql--connect-entry (dolt-dir port)
  "Open a fresh `dorg-sql--conn-entry' to PORT for DOLT-DIR.
Captures the `sql-server.info' instance UUID (or nil for a server that
publishes none) so a later same-port instance swap can be detected by first-use
identity validation.  May raise `mysql-connection-error' (and kin) if the
endpoint is unreachable."
  (let ((db (dorg-dolt-server--db-name dolt-dir)))
    (dorg-sql--make-conn-entry
     :conn (mysql-connect :host "127.0.0.1"
                          :port port
                          :user "root"
                          :database db
                          :tls nil
                          :connect-timeout dorg-sql-connect-timeout
                          :read-idle-timeout dorg-sql-read-timeout)
     :port port
     :identity (list :db db
                     :uuid (plist-get (dorg-dolt-server--dolt-server-info dolt-dir)
                                      :uuid))
     :generation (dorg-sql--generation-fingerprint dolt-dir))))

(defun dorg-sql--entry-identity-valid-p (entry dolt-dir)
  "Return non-nil if ENTRY's captured identity (db-name + UUID) still matches.
Both halves are checked: the captured `:db' against the current
`dorg-dolt-server--db-name', and the captured `:uuid' against the live
`sql-server.info'.  Lenient only when a captured half is unavailable — a nil
captured db, a nil captured UUID, or no readable `sql-server.info' (a
managed/non-dolt-native owner that publishes none) trusts that half; any POSITIVE
mismatch (both present and differing) invalidates the entry, catching a silent
same-port instance swap (UUID drift) or a store re-point (db drift)."
  (let* ((identity (dorg-sql--conn-entry-identity entry))
         (db (plist-get identity :db))
         (uuid (plist-get identity :uuid)))
    (and
     ;; db half: a captured db-name that no longer matches is a positive mismatch.
     (or (null db) (equal db (dorg-dolt-server--db-name dolt-dir)))
     ;; uuid half: a captured UUID that no longer matches the live instance is one.
     (or (null uuid)
         (let ((info (dorg-dolt-server--dolt-server-info dolt-dir)))
           (or (null info)
               (equal (plist-get info :uuid) uuid)))))))

(defun dorg-sql--validate-entry (entry dolt-dir)
  "Return ENTRY if it is a usable warm session, else nil.
Usable means the connection pings healthy AND its captured store+instance
identity is still valid (already `:validated', or passing a first-use
`dorg-sql--entry-identity-valid-p' check — which flips ENTRY `:validated' on
success).  A dead ping or a POSITIVE identity mismatch returns nil.  Shared by the
ordinary hot path (`dorg-sql--get-connection') and the lease reserved-checkout so
BOTH acquire a VALIDATED Model-A session and neither can lease/reuse a
stale-identity connection.  Does NOT evict or disconnect — the caller decides."
  (let ((generation (dorg-sql--generation-fingerprint dolt-dir)))
    (and (dorg-sql--entry-ping-ok-p entry)
       ;; C1/C2: validate this cheap, local generation on EVERY warm read.
       ;; A cached connection cannot outlive a directory swap, native endpoint
       ;; rotation, or re-materialized source buffer.
       (or (equal (dorg-sql--conn-entry-generation entry) generation)
           ;; An entry created before this field existed can be resident after
           ;; an in-session reload.  Stamp it once from the same read-only
           ;; snapshot; all later warm reads compare the complete fingerprint.
           (and (null (dorg-sql--conn-entry-generation entry))
                (progn
                  (setf (dorg-sql--conn-entry-generation entry) generation)
                  t)))
       (or (dorg-sql--conn-entry-validated entry)
           (when (dorg-sql--entry-identity-valid-p entry dolt-dir)
             (setf (dorg-sql--conn-entry-validated entry) t)
             t))
       entry)))

(defun dorg-sql--open-entry (key dolt-dir port)
  "Internal (unguarded) endpoint-ownership open: connect to PORT, cache, return.
On the FIRST connect failure before any SQL, self-heals EXACTLY once: evict the
exact attached-port hint, rediscover the authoritative claim
\(`dorg-dolt-server--strict-port'), and reconnect once to the rotated port.  A
second failure propagates — no loop, no server-lifecycle mutation, no replay.
This is the internal opener used by the ordinary guarded path AND by the
lease reserved-checkout; it never consults `dorg-sql--leases'."
  (let ((entry
         (condition-case _err
             (dorg-sql--connect-entry dolt-dir port)
           ((mysql-connection-error mysql-timeout mysql-protocol-error)
            (dorg-dolt-server--evict-attached-port key port)
            (dorg-sql--connect-entry dolt-dir
                                     (dorg-dolt-server--strict-port dolt-dir))))))
    (puthash key entry dorg-sql--connections)
    entry))

(defun dorg-sql--get-connection (dolt-dir &optional port)
  "Return a live mysql-conn for DOLT-DIR, creating if needed.
Reuses the bound warm entry while it pings healthy — NO per-query claim
discovery or lifecycle lock: the endpoint port is resolved lazily (PORT, or
`dorg-dolt-server--strict-port') ONLY on a cache miss, so a warm connection
performs zero strict-port discovery.  A dead ping compare-and-removes the entry
and reconnects (self-healing once through `dorg-sql--open-entry').  On the FIRST
reuse of an entry it validates the sql-server instance identity once
\(`dorg-sql--entry-identity-valid-p'); a mismatch evicts and reopens.  Refuses a
store held under an exclusive lease: ordinary callers must not touch a leased
session (defense-in-depth over the top-level guard in
`dorg-sql'/`dorg-sql-batch'; use `dorg-sql--lease-exec'/`dorg-sql--lease-batch')."
  (dorg-sql--assert-not-leased dolt-dir)
  (let* ((key (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash key dorg-sql--connections)))
    ;; Reuse the warm entry only while it pings healthy AND its identity is
    ;; still valid; a dead ping or an instance/db swap evicts and reopens.
    (when (and entry (not (dorg-sql--validate-entry entry dolt-dir)))
      (dorg-sql--evict-conn-entry key entry)
      (setq entry nil))
    (dorg-sql--conn-entry-conn
     (or entry
         (dorg-sql--open-entry key dolt-dir
                               (or port (dorg-dolt-server--strict-port dolt-dir)))))))

(defun dorg-sql--close-connection (dolt-dir)
  "Close the persistent mysql connection for DOLT-DIR if any."
  (let* ((key (dorg-dolt-server--canonical-dir dolt-dir))
         (entry (gethash key dorg-sql--connections)))
    (when entry
      (ignore-errors (mysql-disconnect (dorg-sql--conn-entry-conn entry)))
      (remhash key dorg-sql--connections))))

(defun dorg-sql--close-all-connections ()
  "Close all persistent mysql connections."
  (maphash (lambda (_key entry)
             (ignore-errors (mysql-disconnect (dorg-sql--conn-entry-conn entry))))
           dorg-sql--connections)
  (clrhash dorg-sql--connections))

(defun dorg-sql--mysql-rows-to-alists (result)
  "Convert mysql RESULT rows to list of alists matching CLI output format."
  (when (mysql-result-rows result)
    (let ((col-names (mapcar (lambda (c) (intern (plist-get c :name)))
                             (mysql-result-columns result))))
      (mapcar (lambda (row) (cl-mapcar #'cons col-names row))
              (mysql-result-rows result)))))

(defun dorg-sql--mysql-exec (query conn)
  "Execute single QUERY on mysql CONN, return list of alists like CLI mode."
  (let ((result (mysql-query conn query)))
    (dorg-sql--mysql-rows-to-alists result)))

;;; Commit Identity

(defcustom dorg-dolt-user-name "dorg"
  "User name for Dolt commits.
Passed to `dolt init --name' and `dolt commit --author'."
  :type 'string
  :group 'dorg)

(defcustom dorg-dolt-user-email "dorg@localhost"
  "User email for Dolt commits.
Passed to `dolt init --email' and `dolt commit --author'."
  :type 'string
  :group 'dorg)

(defun dorg-sql--transport-condition-p (err)
  "Return non-nil when ERR belongs to the closed MySQL transport family.
Classification uses the condition hierarchy only.  It deliberately never
parses condition prose, which may contain database-looking codes that are not
database facts after a response is lost."
  (let ((conditions (get (car-safe err) 'error-conditions)))
    (cl-some (lambda (type) (memq type conditions))
             '(mysql-connection-error mysql-timeout mysql-protocol-error))))

(defun dorg-sql--conn-process-dead-p (conn)
  "Return non-nil when CONN has a known, dead MySQL transport process.
Mock/non-MySQL connection objects are unknown rather than dead, so a generic
logic or SQL error in a unit caller cannot be promoted to transport failure."
  (condition-case nil
      (let ((proc (mysql-conn-process conn)))
        (and (processp proc) (not (process-live-p proc))))
    (error nil)))

(defun dorg-sql--transport-failure-p (err conn)
  "Return non-nil when ERR proves transport failure on CONN.
Typed MySQL transport conditions are sufficient even while the connection
process survives.  A generic error is transport only when the process is
already known dead, matching the lease guard law."
  (or (dorg-sql--transport-condition-p err)
      (dorg-sql--conn-process-dead-p conn)))

(defun dorg-sql--failed-statement-facts (err transaction-phase failed-statement
                                             statement-index rollback-confirmed)
  "Return closed correlation facts for a failed data statement, plus capture.
Empty when there is no FAILED-STATEMENT (the failure was not a data
statement: validation, staging or commit).  Otherwise names the execution
ordinal and the closed verb/table class, and — under
`dorg-sql-capture-failed-statements' inside a leased batch — records the
original condition privately, reporting `:capture written' with its id or
`:capture failed'.  ROLLBACK-CONFIRMED must be t: diagnostic file I/O never
precedes the same-session ROLLBACK, and an unconfirmed rollback yields
`:capture skipped' with no file access at all.  Never carries prose or SQL."
  (when failed-statement
    (append
     (list :statement-index statement-index
           :statement-class (dorg-sql--statement-class failed-statement))
     (when (and dorg-sql-capture-failed-statements dorg-sql--capture-store-dir)
       (if (not (eq t rollback-confirmed))
           (list :capture 'skipped)
         (let ((id (dorg-sql--capture-failed-statement
                    dorg-sql--capture-store-dir err failed-statement
                    statement-index transaction-phase
                    (plist-get (dorg-dolt--classify-db-error err failed-statement)
                               :db-code))))
           (if id
               (list :capture 'written :capture-id id)
             (list :capture 'failed))))))))

(defun dorg-sql--raise-batch-rollback-result
    (conn err transaction-phase failed-statement report-rollback
          &optional statement-index)
  "Roll back ERR on CONN and signal its definite transaction result.
TRANSACTION-PHASE and FAILED-STATEMENT are closed attempt facts;
STATEMENT-INDEX is the execution ordinal counted by the batch loop for
FAILED-STATEMENT (nil when the failure was not a data statement).
REPORT-ROLLBACK selects the sanitized typed receipt used by mutation; ordinary
callers retain the original error family.  The private capture runs only
after the ROLLBACK completed and never alters the signalled outcome.  This
function never returns."
  (let ((rollback-failure
         (condition-case rb
             (progn (mysql-query conn "ROLLBACK") nil)
           (error (error-message-string rb)))))
    (cond
     (rollback-failure
      (if report-rollback
          (signal
           'dorg-sql-transaction-rollback-failed
           (list "Dolt transaction rollback failed"
                 (append
                  (list :rollback-confirmed nil
                        :transaction-phase transaction-phase)
                  (dorg-dolt--classify-db-error err failed-statement))))
        (signal (car err)
                (list (format "%s [ROLLBACK ALSO FAILED: %s]"
                              (error-message-string err)
                              rollback-failure)))))
     (report-rollback
      (signal
       'dorg-sql-transaction-rolled-back
       (list "Dolt transaction rolled back"
             (append
              (list :rollback-confirmed t
                    :transaction-phase transaction-phase)
              (dorg-dolt--classify-db-error err failed-statement)
              ;; The ROLLBACK above completed: the capture may touch files.
              (dorg-sql--failed-statement-facts
               err transaction-phase failed-statement statement-index t)))))
     (t (signal (car err) (cdr err))))))

(defun dorg-sql--handle-batch-failure
    (conn err transaction-phase transaction-open commit-hash
          failed-statement report-rollback &optional statement-index)
  "Classify and signal one commit-bearing batch failure.
CONN and ERR identify the failed operation; TRANSACTION-PHASE and
TRANSACTION-OPEN record the state machine.  COMMIT-HASH is non-nil only after a
returned Dolt commit response.  FAILED-STATEMENT, STATEMENT-INDEX and
REPORT-ROLLBACK feed only the definite rollback path.  This function never
returns."
  (cond
   ;; A returned hash proves DOLT_COMMIT landed.  A transport-class failure
   ;; while its request is in flight proves only ambiguity, including when the
   ;; connection process is still live.  Decide this BEFORE rollback: a trivial
   ;; successful ROLLBACK after Dolt advanced HEAD cannot restore the prior
   ;; commit.
   ((or commit-hash
        (and (eq transaction-phase 'in-commit)
             (dorg-sql--transport-failure-p err conn)))
    (signal
     'dorg-sql-commit-indeterminate
     (append
      (list "Dolt commit outcome is indeterminate"
            :transaction-phase 'in-commit)
      (when commit-hash (list :commit commit-hash)))))
   ;; BEGIN failed: no transaction exists and there is nothing to roll back or
   ;; classify as a rollback receipt.
   ((not transaction-open) (signal (car err) (cdr err)))
   (t
    (dorg-sql--raise-batch-rollback-result
     conn err transaction-phase failed-statement report-rollback
     statement-index))))

(defconst dorg-sql--transaction-read-port-core-tables
  '("headings" "heading_tags" "heading_items" "property_entries"
    "planning_items" "keyword_items" "drawer_items" "drawer_lines"
    "text_items" "dorg_meta" "dorg_files" "dorg_conflicts"
    "dorg_mutation_log")
  "Core tables forbidden through the extension-count read family.")

(defconst dorg-sql--transaction-read-port-identifier-re
  "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'"
  "Identifier grammar accepted by the internal transaction read port.")

(defun dorg-sql--transaction-read-port-raise (reason message &rest data)
  "Signal a transaction read-port refusal with REASON, MESSAGE, and DATA."
  (signal 'dorg-sql-transaction-read-port-error
          (list message (apply #'list :reason reason data))))

(defun dorg-sql--transaction-read-port-keys (request)
  "Return plist keys in REQUEST, or signal when REQUEST is malformed."
  (unless (and (listp request) (proper-list-p request)
               (cl-evenp (length request)))
    (dorg-sql--transaction-read-port-raise
     'bad-request "transaction read request is not a plist" :request request))
  (let (keys)
    (while request
      (unless (keywordp (car request))
        (dorg-sql--transaction-read-port-raise
         'bad-request "transaction read request key is not a keyword"
         :key (car request)))
      (push (car request) keys)
      (setq request (cddr request)))
    (nreverse keys)))

(defun dorg-sql--transaction-read-port-check-keys (request allowed)
  "Refuse REQUEST unless its keys are in ALLOWED, without duplicates."
  (let ((keys (dorg-sql--transaction-read-port-keys request)))
    (when (or (cl-set-difference keys allowed)
              (not (equal keys (delete-dups (copy-sequence keys)))))
      (dorg-sql--transaction-read-port-raise
       'bad-request "transaction read request has unknown or duplicate keys"
       :keys keys))))

(defun dorg-sql--transaction-read-port-ids (request)
  "Return REQUEST's bounded non-empty integer `:ids' list."
  (let ((ids (plist-get request :ids)))
    (unless (and (listp ids) ids
                 (<= (length ids) dorg-sql-transaction-read-port-max-ids)
                 (cl-every (lambda (id) (and (integerp id) (> id 0))) ids))
      (dorg-sql--transaction-read-port-raise
       'bad-ids "transaction read request has unbounded or invalid ids"
       :ids ids))
    ids))

(defun dorg-sql--transaction-read-port-root-id (request)
  "Return REQUEST's optional typed `:root-id'."
  (let ((root-id (plist-get request :root-id)))
    (unless (or (null root-id) (and (integerp root-id) (> root-id 0)))
      (dorg-sql--transaction-read-port-raise
       'bad-root "transaction read request has invalid root id"
       :root-id root-id))
    root-id))

(defun dorg-sql--transaction-read-port-identifier-p (s)
  "Non-nil when S is a safe unqualified SQL identifier."
  (and (stringp s)
       (string-match-p dorg-sql--transaction-read-port-identifier-re s)))

(defun dorg-sql--transaction-read-port-literal-p (value)
  "Non-nil when VALUE is a literal the port can bind."
  (or (null value) (stringp value) (numberp value)))

(defun dorg-sql--transaction-read-port-predicate-sql (where)
  "Lower typed equality predicate WHERE to SQL, or nil for whole-table count."
  (when where
    (unless (and (listp where) (cl-every #'consp where))
      (dorg-sql--transaction-read-port-raise
       'bad-predicate "extension count predicate is not a column alist"
       :where where))
    (dolist (cell where)
      (let ((col (car cell))
            (value (cdr cell)))
        (unless (and (symbolp col)
                     (dorg-sql--transaction-read-port-identifier-p
                      (symbol-name col)))
          (dorg-sql--transaction-read-port-raise
           'bad-column "extension count predicate has invalid column"
           :column col))
        (unless (dorg-sql--transaction-read-port-literal-p value)
          (dorg-sql--transaction-read-port-raise
           'bad-value "extension count predicate has invalid value"
           :column col :value value))))
    (dorg-sql--effect-predicate-sql where)))

(defun dorg-sql--transaction-read-port-extension-table (request allowed-tables)
  "Return REQUEST's extension table after core and ownership checks."
  (let ((table (plist-get request :table)))
    (unless (dorg-sql--transaction-read-port-identifier-p table)
      (dorg-sql--transaction-read-port-raise
       'bad-table "extension count table is not a safe identifier"
       :table table))
    (when (cl-member table dorg-sql--transaction-read-port-core-tables
                     :test #'string-equal-ignore-case)
      (dorg-sql--transaction-read-port-raise
       'core-table "extension count table is a DOrg core table"
       :table table))
    (unless (cl-member table allowed-tables :test #'string-equal-ignore-case)
      (dorg-sql--transaction-read-port-raise
       'unowned-table "extension count table was not in the validated effect set"
       :table table))
    table))

(defun dorg-sql--transaction-read-port-count-from (request allowed-tables)
  "Return the validated `FROM TABLE [WHERE PRED]' of a count REQUEST.
REQUEST carries `:table' and `:where'.  The per-check count and every
subquery of a batched count are built from this one clause, so both read
exactly the same rows."
  (let ((table (dorg-sql--transaction-read-port-extension-table
                request allowed-tables))
        (pred (dorg-sql--transaction-read-port-predicate-sql
               (plist-get request :where))))
    (if pred
        (format "FROM %s WHERE %s" table pred)
      (format "FROM %s" table))))

(defun dorg-sql--transaction-read-port-natural-count (n &rest facts)
  "Return count N when it is a native nonnegative integer, else refuse.
FACTS are added to the refusal."
  ;; COUNT(*) decodes to a native integer; anything else is a broken
  ;; read, never a zero and never a number recovered from its spelling.
  (unless (natnump n)
    (apply #'dorg-sql--transaction-read-port-raise
           'bad-count "extension count result is not a nonnegative integer"
           :count n facts))
  n)

(defun dorg-sql--transaction-read-port-batch-from (request allowed-tables)
  "Return the validated FROM clauses of batched count REQUEST, in order.
`:checks' must be a proper list of 1 to
`dorg-sql-transaction-read-port-max-checks' pairs (TABLE . WHERE).  Every
check is validated before the caller sends any SQL; a refusal names the
check's position."
  (let ((checks (plist-get request :checks)))
    (unless (and (consp checks) (proper-list-p checks)
                 (<= (length checks) dorg-sql-transaction-read-port-max-checks))
      (dorg-sql--transaction-read-port-raise
       'bad-checks "batched count request has an empty, improper or oversized check list"
       :count (and (proper-list-p checks) (length checks))))
    (cl-loop
     for check in checks
     for position from 0
     collect
     (progn
       (unless (consp check)
         (dorg-sql--transaction-read-port-raise
          'bad-checks "batched count check is not a (TABLE . WHERE) pair"
          :position position))
       ;; The predicate is traversed below, so its shape is settled first: a
       ;; proper, finite list of cells (or nil), never an improper or cyclic
       ;; one that would escape as an untyped error or never return.
       (let ((where (cdr check)))
         (unless (or (null where)
                     (and (proper-list-p where) (cl-every #'consp where)))
           (dorg-sql--transaction-read-port-raise
            'bad-predicate "batched count check predicate is not a column alist"
            :position position)))
       (condition-case err
           (dorg-sql--transaction-read-port-count-from
            (list :table (car check) :where (cdr check)) allowed-tables)
         (dorg-sql-transaction-read-port-error
          (signal (car err)
                  (list (nth 1 err)
                        (append (nth 2 err) (list :position position))))))))))

(defun dorg-sql--transaction-read-port-batch-counts (rows count)
  "Return COUNT natural numbers read by alias from the one row in ROWS.
The row must hold exactly the aliases n0 ... n(COUNT-1); any other shape is a
typed refusal."
  (let ((row (car rows)))
    (unless (and (= 1 (length rows))
                 (= count (length row))
                 (equal (sort (mapcar #'car row)
                              (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                        (sort (cl-loop for i below count
                                       collect (intern (format "n%d" i)))
                              (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
      (dorg-sql--transaction-read-port-raise
       'bad-count "batched count result is not one row of the requested aliases"
       :rows (length rows) :columns (mapcar #'car row)))
    (cl-loop for i below count
             collect (dorg-sql--transaction-read-port-natural-count
                      (cdr (assq (intern (format "n%d" i)) row))
                      :position i))))

(defun dorg-sql--transaction-read-port-dispatch (conn request allowed-tables)
  "Execute one closed transaction-local read REQUEST on CONN."
  (unless (and (listp request) (not (stringp request)))
    (dorg-sql--transaction-read-port-raise
     'bad-request "transaction read request is not a typed plist"
     :request request))
  (unless (equal 1 (plist-get request :version))
    (dorg-sql--transaction-read-port-raise
     'bad-version "transaction read request version is unsupported"
     :version (plist-get request :version)))
  (pcase (plist-get request :op)
    ('core-heading-rows
     (dorg-sql--transaction-read-port-check-keys
      request '(:version :op :ids :root-id))
     (let* ((ids (dorg-sql--transaction-read-port-ids request))
            (root-id (dorg-sql--transaction-read-port-root-id request))
            (id-list (mapconcat #'number-to-string ids ","))
            (root-filter (if root-id (format " AND root_id = %d" root-id) "")))
       (dorg-sql--mysql-exec
        (format
         (concat "SELECT id, parent_id, HEX(sort_key) AS sort_key,"
                 " level_offset, todo_keyword, priority, commented, title,"
                 " post_blank, items_hash FROM headings"
                 " WHERE id IN (%s)%s")
         id-list root-filter)
        conn)))
    ('core-heading-tags
     (dorg-sql--transaction-read-port-check-keys request '(:version :op :ids))
     (let* ((ids (dorg-sql--transaction-read-port-ids request))
            (id-list (mapconcat #'number-to-string ids ",")))
       (dorg-sql--mysql-exec
        (format
         (concat "SELECT heading_id, tag FROM heading_tags"
                 " WHERE heading_id IN (%s) ORDER BY heading_id, ordinal")
         id-list)
        conn)))
    ('core-heading-ids
     (dorg-sql--transaction-read-port-check-keys request '(:version :op :ids))
     (let* ((ids (dorg-sql--transaction-read-port-ids request))
            (id-list (mapconcat #'number-to-string ids ",")))
       (dorg-sql--mysql-exec
        (format "SELECT id FROM headings WHERE id IN (%s)" id-list)
        conn)))
    ('owner-model
     (dorg-sql--transaction-read-port-check-keys
      request '(:version :op :root-id :ids))
     (let ((root-id (dorg-sql--transaction-read-port-root-id request))
           (ids (dorg-sql--transaction-read-port-ids request)))
       (unless root-id
         (dorg-sql--transaction-read-port-raise
          'bad-root "owner model read requires a root id"))
       ;; The owner image feeds only offset-carrying canonical tuples, so it
       ;; reads the persisted offsets and derives no effective level.
       (dorg-model-load-headings
        nil root-id ids nil nil
        (lambda (sql) (dorg-sql--mysql-exec sql conn))
        'offsets)))
    ('extension-row-count
     (dorg-sql--transaction-read-port-check-keys
      request '(:version :op :table :where))
     (let* ((from (dorg-sql--transaction-read-port-count-from
                   request allowed-tables))
            (rows (dorg-sql--mysql-exec
                   (concat "SELECT COUNT(*) AS n " from) conn)))
       (dorg-sql--transaction-read-port-natural-count
        (alist-get 'n (car rows)) :table (plist-get request :table))))
    ('extension-row-counts
     ;; One statement answers many checks: each subquery is exactly the
     ;; per-check count above, so the batch reads the same rows on the same
     ;; connection and snapshot.  Every check is validated first.
     (dorg-sql--transaction-read-port-check-keys
      request '(:version :op :checks))
     (let* ((froms (dorg-sql--transaction-read-port-batch-from
                    request allowed-tables))
            (rows (dorg-sql--mysql-exec
                   (concat "SELECT "
                           (mapconcat #'identity
                                      (cl-loop for from in froms
                                               for i from 0
                                               collect (format "(SELECT COUNT(*) %s) AS n%d"
                                                               from i))
                                      ", "))
                   conn)))
       (dorg-sql--transaction-read-port-batch-counts rows (length froms))))
    (_
     (dorg-sql--transaction-read-port-raise
      'bad-op "transaction read request operation is unsupported"
      :op (plist-get request :op)))))

(defun dorg-sql--transaction-read-port (conn &optional allowed-tables)
  "Return the closed transaction-local read port for postimage proof reads.
ALLOWED-TABLES is the already-validated extension effect table set.  The port
does not accept SQL strings; callers submit versioned request plists from the
small internal proof vocabulary."
  (lambda (request)
    (dorg-sql--transaction-read-port-dispatch conn request allowed-tables)))

(defun dorg-sql--validate-query-allowed-p (sql)
  "Non-nil when SQL is one SELECT statement safe for the validate window.
The guard's job is to keep the window read-only and single-statement: SQL
must start with SELECT and contain no STATEMENT-LEVEL semicolon.  A
semicolon INSIDE a single-quoted string literal is data, not a statement
separator (content values legitimately carry them — tracked-regression), so the
scan tracks literal state honoring both quote-doubling ('') and backslash
escapes.  An unterminated literal fails closed."
  (and (stringp sql)
       (string-match-p "\\`[[:space:]]*SELECT\\_>" sql)
       (let ((i 0) (n (length sql)) (in-string nil) (ok t))
         (while (and ok (< i n))
           (let ((c (aref sql i)))
             (cond
              (in-string
               (cond ((eq c ?\\) (setq i (1+ i)))
                     ((eq c ?')
                      (if (and (< (1+ i) n) (eq (aref sql (1+ i)) ?'))
                          (setq i (1+ i))
                        (setq in-string nil)))))
              ((eq c ?') (setq in-string t))
              ((eq c ?\;) (setq ok nil))))
           (setq i (1+ i)))
         (and ok (not in-string)))))

(defun dorg-sql--mysql-batch-on-conn
    (conn statements commit-message &optional validate-function report-rollback)
  "Run STATEMENTS (with optional DOLT commit) on an established mysql CONN.
Shared by the ordinary batch path and the lease batch seam.  With
COMMIT-MESSAGE, wraps BEGIN .. DOLT_ADD .. DOLT_COMMIT .. COMMIT, rolling back
on the SAME connection on definite error (never CLI) and treating DOLT_COMMIT
\"nothing to commit\" as success.  The commit-bearing path records the closed
transaction phases `pre-statement', `statements', `validate', and `in-commit'.
A transport failure in `in-commit' is outcome-indeterminate even when the
DOLT_COMMIT response carrying its hash was lost; it never sends ROLLBACK.

When VALIDATE-FUNCTION is non-nil, call it after every statement has
executed but before DOLT_ADD/DOLT_COMMIT.  The legacy administrative shape
receives one read-only QUERY function accepting SQL plus `format' arguments
and returning rows as alists.  The typed proof shape
\(:mode transaction-read-port :extension-tables TABLES :function FN
[:query-function QFN]) runs FN with a closed typed read port and then, when
QFN is present, runs QFN with the same SELECT-only QUERY function as the legacy
shape.  A validation signal from either function follows the same transaction
rollback path, allowing callers to prove exact pending state without a
post-commit compensation write.

REPORT-ROLLBACK is a private typed-mutation opt-in.  When non-nil, an error
whose same-session ROLLBACK succeeds is converted exactly once into
`dorg-sql-transaction-rolled-back' carrying sanitized classifier facts and
`:rollback-confirmed t'.  Both receipts carry the closed
`:transaction-phase'.  A failed rollback becomes the equally sanitized
`dorg-sql-transaction-rollback-failed'.  Outcome-indeterminate errors are typed
regardless of this flag.  Ordinary callers otherwise retain the original
condition behavior.

Returns the exact hash reported by DOLT_COMMIT, or nil when there is no
commit message or Dolt reports nothing to commit."
  (if commit-message
      (let ((author (format "%s <%s>"
                            dorg-dolt-user-name
                            dorg-dolt-user-email))
            commit-hash
            failed-statement
            ;; Execution ordinal of the statement in flight, counted here at
            ;; the loop (never recovered from text later): cleared together
            ;; with `failed-statement' after each success so no later phase
            ;; inherits a stale index.
            statement-index
            (transaction-phase 'pre-statement)
            transaction-open)
        ;; Manual BEGIN/COMMIT instead of with-mysql-transaction so we
        ;; can catch "nothing to commit" from DOLT_COMMIT without
        ;; triggering a ROLLBACK of the (harmless) SQL statements.
        (condition-case err
            (progn
              (mysql-query conn "BEGIN")
              (setq transaction-open t
                    transaction-phase 'statements)
              (let ((ordinal 0))
                (dolist (stmt statements)
                  (setq ordinal (1+ ordinal)
                        failed-statement stmt
                        statement-index ordinal)
                  (mysql-query conn stmt)
                  (setq failed-statement nil
                        statement-index nil)))
              (setq transaction-phase 'validate)
              (when validate-function
                (let ((query-function
                       (lambda (query &rest args)
                         (let ((formatted
                                (if args (apply #'format query args) query))
                               (case-fold-search t))
                           (unless (dorg-sql--validate-query-allowed-p formatted)
                             (error
                              "dorg: transaction validation query must be SELECT-only"))
                           (dorg-sql--mysql-exec formatted conn)))))
                  (if (and (listp validate-function)
                           (eq 'transaction-read-port
                               (plist-get validate-function :mode)))
                      (progn
                        (funcall
                         (plist-get validate-function :function)
                         (dorg-sql--transaction-read-port
                          conn (plist-get validate-function :extension-tables)))
                        (when-let ((raw-validator
                                    (plist-get validate-function :query-function)))
                          (funcall raw-validator query-function)))
                    (funcall validate-function query-function))))
              (mysql-query conn "CALL DOLT_ADD('.')")
              ;; The outcome becomes ambiguous as soon as the commit request
              ;; is sent, not only after its response returns.
              (setq transaction-phase 'in-commit)
              (condition-case commit-err
                  (let ((rows
                         (dorg-sql--mysql-exec
                          (format
                           "CALL DOLT_COMMIT('-m', %s, '--author', %s)"
                           (dorg-sql-quote commit-message)
                           (dorg-sql-quote author))
                          conn)))
                    (setq commit-hash
                          (and rows (alist-get 'hash (car rows)))))
                (error
                 (unless (dorg-sql-batch--nothing-to-commit-p commit-err)
                   (signal (car commit-err) (cdr commit-err)))))
              (mysql-query conn "COMMIT")
              (setq transaction-open nil)
              commit-hash)
          (error
           (dorg-sql--handle-batch-failure
            conn err transaction-phase transaction-open commit-hash
            failed-statement report-rollback
            (and failed-statement statement-index)))))
    (dolist (stmt statements)
      (mysql-query conn stmt))
    nil))

;;; ─── Exclusive Session-Affine Lease (tracked-case) ────────────────────────

;; A lease binds ONE exact connection + endpoint + ownership identity into a
;; unit held for a single linearized critical section.  It is the transport
;; seam tracked-case consumes so a server-session-scoped GET_LOCK stays pinned to
;; one session: while a lease is held there is NO cache relookup or reconnect,
;; and a mid-lease disconnect ABORTS fail-closed (zero replacement-session SQL)
;; rather than silently swapping the lock-owning session.
;;
;; The lease struct IS the ownership identity: enforcement is object identity
;; (`eq' of the registry-stored struct), so a copied or stale struct cannot pass
;; as the owner.  The `owner' slot is a diagnostic tag only.
(cl-defstruct (dorg-sql--lease (:constructor dorg-sql--make-lease)
                               (:copier nil))
  dolt-dir port conn generation
  (phase :reserved)   ; :reserved -> :active -> :released
  owner)              ; diagnostic uninterned symbol

;; KEEP (mutable-state): per-store active-lease registry, puthash-mutated across calls.
(defvar dorg-sql--leases (make-hash-table :test 'equal)
  "Map of canonical dolt-dir to the store's active `dorg-sql--lease', if any.
A non-nil entry (in phase :reserved or :active) means the store is exclusively
leased: ordinary `dorg-sql'/`dorg-sql-batch' refuse it with
`dorg-sql-lease-busy'; only the lease seams reach the session.")

(defun dorg-sql--store-leased-p (dolt-dir)
  "Return the active lease for DOLT-DIR, or nil."
  (gethash (dorg-dolt-server--canonical-dir dolt-dir) dorg-sql--leases))

(defun dorg-sql--assert-not-leased (dolt-dir)
  "Signal `dorg-sql-lease-busy' if DOLT-DIR is exclusively leased.
The top-of-entry guard for ordinary SQL: a leased store admits no implicit
holder bypass, so a re-entrant callback issuing ordinary same-store SQL during a
lease is refused here rather than multiplexed onto the leased session."
  (when (dorg-sql--store-leased-p dolt-dir)
    (signal 'dorg-sql-lease-busy
            (list (format "dorg: store %s is exclusively leased; ordinary SQL refused (use dorg-sql--lease-exec/--lease-batch)"
                          dolt-dir)))))

(defun dorg-sql--lease-validate (lease)
  "Assert LEASE is the current :active registry lease for its store.
A wrong/absent/stale/copied lease signals `dorg-sql-lease-ownership-error'."
  (let ((canon (dorg-dolt-server--canonical-dir (dorg-sql--lease-dolt-dir lease))))
    (unless (and (eq (gethash canon dorg-sql--leases) lease)
                 (eq (dorg-sql--lease-phase lease) :active))
      (signal 'dorg-sql-lease-ownership-error
              (list (format "dorg: lease for %s is not the active owner"
                            (dorg-sql--lease-dolt-dir lease)))))))

(defun dorg-sql--lease-assert-current-generation (lease)
  "Refuse LEASE if its store generation changed before its next SQL operation.
The lease is connection-affine: it cannot reconnect a mutation planned against
its original generation onto a replacement store.  Comparing the same local C1
fingerprint used by warm reads before every leased operation therefore fails
closed before any product SQL is sent to the replacement generation."
  (let ((actual (dorg-sql--generation-fingerprint
                 (dorg-sql--lease-dolt-dir lease))))
    ;; A lease created by the previous struct layout can remain active during
    ;; an in-session reload.  Like a legacy warm entry, capture its first
    ;; observed generation once; subsequent operations compare it strictly.
    (if (null (dorg-sql--lease-generation lease))
        (setf (dorg-sql--lease-generation lease) actual)
      (unless (equal (dorg-sql--lease-generation lease) actual)
        (signal 'dorg-sql-lease-generation-drift
                (list (format "dorg: store generation changed while lease was held: %s"
                              (dorg-sql--lease-dolt-dir lease))
                      :expected (dorg-sql--lease-generation lease)
                      :actual actual))))))

(defconst dorg-sql--lease-broken-cause-classes
  '(mysql-timeout mysql-connection-error mysql-protocol-error
    dead-connection other)
  "Closed cause vocabulary carried by `dorg-sql-lease-broken'.")

(defun dorg-sql--lease-broken-cause-class (lease err)
  "Return the closed transport cause for ERR raised on LEASE.
Classification uses only the condition symbol and connection liveness; error
messages, SQL text, paths, and server payloads are never inspected."
  (pcase (car-safe err)
    ('mysql-timeout 'mysql-timeout)
    ('mysql-connection-error 'mysql-connection-error)
    ('mysql-protocol-error 'mysql-protocol-error)
    ('error (if (dorg-sql--lease-conn-live-p lease)
                'other
              'dead-connection))
    (_ 'other)))

(defun dorg-sql--raise-lease-broken (lease err)
  "Signal `dorg-sql-lease-broken' for LEASE citing the underlying ERR.
The condition data also carries closed diagnostic symbols.  Its first datum
remains the historical human-readable message."
  (signal 'dorg-sql-lease-broken
          (list (format "dorg: lease session for %s broke mid-operation: %s"
                        (dorg-sql--lease-dolt-dir lease)
                        (error-message-string err))
                :cause-class (dorg-sql--lease-broken-cause-class lease err))))

(defun dorg-sql--lease-conn-live-p (lease)
  "Return non-nil if LEASE's connection process is still live.
A dead process means the transport (socket) is already torn down, so any error
raised while sending to / receiving from it is a BROKEN session, not a SQL
fault — even when it surfaces as a bare Emacs `error' (e.g. `process-send-string'
on a peer-closed socket: \"... not running: connection broken by remote peer\")."
  (let ((conn (dorg-sql--lease-conn lease)))
    (and conn (process-live-p (mysql-conn-process conn)))))

(defmacro dorg-sql--with-broken-guard (lease &rest body)
  "Run BODY; convert a TRANSPORT-level failure into `dorg-sql-lease-broken'.
Connection/timeout/protocol errors (a broken session) always fail closed.  A
generic Emacs `error' raised while the leased connection's process is already
DEAD — the realistic mid-flight socket drop, where the next send signals a bare
\"... not running: connection broken by remote peer\" rather than a typed mysql
error — is likewise a broken transport and fails closed.  A SQL
`mysql-query-error', or ANY error raised while the leased connection is still
LIVE, propagates unchanged so a genuine query fault is NOT mistaken for a broken
lease."
  (declare (indent 1) (debug (form body)))
  (let ((err (make-symbol "err"))
        (l (make-symbol "lease")))
    `(let ((,l ,lease))
       (condition-case ,err
           (progn ,@body)
         (dorg-sql-commit-indeterminate
          (signal (car ,err) (cdr ,err)))
         ((mysql-connection-error mysql-timeout mysql-protocol-error)
          (dorg-sql--raise-lease-broken ,l ,err))
         (error
          ;; A bare Emacs error is a broken transport ONLY when the leased
          ;; session's process has already died; on a LIVE connection it is a
          ;; genuine fault (SQL error, logic bug) and must propagate untouched.
          (if (dorg-sql--lease-conn-live-p ,l)
              (signal (car ,err) (cdr ,err))
            (dorg-sql--raise-lease-broken ,l ,err)))))))

(defun dorg-sql--lease-exec (lease query &rest args)
  "Execute QUERY on LEASE's exact session; ARGS are `format' args for QUERY.
Validates LEASE is the active registry owner and routes DIRECTLY on its
connection — no transport resolution, strict-port, `dorg-sql--get-connection',
or implicit state.  A transport-level failure aborts fail-closed with
`dorg-sql-lease-broken' (no reconnect, no replay)."
  (dorg-sql--lease-validate lease)
  (dorg-sql--lease-assert-current-generation lease)
  (let ((formatted (if args (apply #'format query args) query)))
    (dorg-sql--with-broken-guard lease
      (dorg-sql--mysql-exec formatted (dorg-sql--lease-conn lease)))))

(defun dorg-sql--lease-batch
    (lease statements &optional commit-message validate-function report-rollback)
  "Execute a batch on LEASE's exact session (see `dorg-sql--mysql-batch-on-conn').
Validated and routed like `dorg-sql--lease-exec'; a broken session fails closed
with `dorg-sql-lease-broken'.  VALIDATE-FUNCTION, when non-nil, runs
inside the transaction after STATEMENTS and before the Dolt commit.
REPORT-ROLLBACK is the private opt-in documented by
`dorg-sql--mysql-batch-on-conn'.  Returns the exact hash reported by
DOLT_COMMIT, or nil when no commit was created."
  (dorg-sql--lease-validate lease)
  (dorg-sql--lease-assert-current-generation lease)
  ;; The lease is the established store identity: carry it to the executor
  ;; so a private failure capture never rediscovers a store on its own.
  (let ((dorg-sql--capture-store-dir (dorg-sql--lease-dolt-dir lease)))
    (dorg-sql--with-broken-guard lease
      (dorg-sql--mysql-batch-on-conn (dorg-sql--lease-conn lease)
                                     statements commit-message
                                     validate-function report-rollback))))

(defun dorg-sql--reserved-checkout (canon dolt-dir lease)
  "Check out a VALIDATED endpoint-ownership session for the reserved LEASE.
Authorized by the exact reservation (LEASE must be the current registry entry).
If a warm connection already exists AND passes the SAME Model-A validation the
ordinary hot path uses (`dorg-sql--validate-entry': pings healthy and its
store+instance identity is still valid), LEASE THAT exact connection (remove it
from the pool) rather than opening a fresh one — never leak or silently orphan an
in-use warm conn.  Otherwise (missing, dead, or a UUID/db-drifted stale-identity
entry) disconnect any such entry and open a fresh session through the INTERNAL
unguarded opener (`dorg-sql--open-entry', never the ordinary lease-busy-guarded
path), then remove it from the pool.  A stale-identity warm conn is NEVER leased.
Either way the leased conn is checked OUT of the ordinary warm pool so no other
caller can reach the leased session.  Fills LEASE's conn/port and flips it
:active."
  (unless (eq (gethash canon dorg-sql--leases) lease)
    (signal 'dorg-sql-lease-ownership-error
            (list (format "dorg: reservation for %s lost during checkout" dolt-dir))))
  (let* ((existing (gethash canon dorg-sql--connections))
         (entry
          (if (and existing (dorg-sql--validate-entry existing dolt-dir))
              ;; Lease the existing healthy, identity-validated warm conn.
              (progn (remhash canon dorg-sql--connections) existing)
            ;; No usable warm conn (missing/dead/stale-identity): disconnect any
            ;; such entry, then open+validate a fresh session.
            (progn
              (when existing (dorg-sql--evict-conn-entry canon existing))
              (let ((fresh (dorg-sql--open-entry
                            canon dolt-dir (dorg-dolt-server--strict-port dolt-dir))))
                (remhash canon dorg-sql--connections)   ; checkout the fresh entry
                fresh)))))
    (setf (dorg-sql--lease-port lease) (dorg-sql--conn-entry-port entry)
          (dorg-sql--lease-conn lease) (dorg-sql--conn-entry-conn entry)
          (dorg-sql--lease-generation lease) (dorg-sql--conn-entry-generation entry)
          (dorg-sql--lease-phase lease) :active)
    lease))

(defun dorg-sql--acquire-lease (dolt-dir)
  "Acquire an exclusive session-affine lease for DOLT-DIR; return the lease.
Installs the reservation SYNCHRONOUSLY (no I/O between the empty-registry check
and the install), so a re-entrant caller cannot pass the empty check while this
one validates.  Then checks out a validated session through the internal
exact-reservation helper and flips the lease :active.  Signals
`dorg-sql-lease-contended' if the store is already leased; on any validation
failure unwinds the reservation by exact-`eq' compare-and-remove."
  (dorg-dolt--ensure-available)
  (let ((canon (dorg-dolt-server--canonical-dir dolt-dir)))
    ;; --- atomic reservation region: NO yield/I/O between check and install ---
    (when (gethash canon dorg-sql--leases)
      (signal 'dorg-sql-lease-contended
              (list (format "dorg: store %s is already exclusively leased" dolt-dir))))
    (let ((lease (dorg-sql--make-lease :dolt-dir dolt-dir
                                       :owner (make-symbol "dorg-sql-lease"))))
      (puthash canon lease dorg-sql--leases)
      ;; --- end atomic region; yielding validation now safe (canon reserved) ---
      (condition-case err
          (progn (dorg-sql--reserved-checkout canon dolt-dir lease) lease)
        (error
         (when (eq (gethash canon dorg-sql--leases) lease)
           (remhash canon dorg-sql--leases))
         (signal (car err) (cdr err)))))))

(defun dorg-sql--release-lease (lease &optional broken)
  "Release LEASE, validating exact ownership.
A wrong/absent/stale/copied lease signals `dorg-sql-lease-ownership-error' and
leaves any newer installed lease untouched.  A NORMAL release restores the
still-healthy connection to the warm pool; a BROKEN release disconnects and
discards it (never returned warm)."
  (let* ((canon (dorg-dolt-server--canonical-dir (dorg-sql--lease-dolt-dir lease)))
         (current (gethash canon dorg-sql--leases)))
    (unless (eq current lease)
      (signal 'dorg-sql-lease-ownership-error
              (list (format "dorg: release of a non-owning lease for %s"
                            (dorg-sql--lease-dolt-dir lease)))))
    (remhash canon dorg-sql--leases)
    (setf (dorg-sql--lease-phase lease) :released)
    (let ((conn (dorg-sql--lease-conn lease)))
      (when conn
        (if broken
            (ignore-errors (mysql-disconnect conn))
          (let ((dd (dorg-sql--lease-dolt-dir lease)))
            (puthash canon
                     (dorg-sql--make-conn-entry
                      :conn conn
                      :port (dorg-sql--lease-port lease)
                      ;; The session was held exclusively and is healthy on a
                      ;; normal release, so its instance identity is trusted:
                      ;; capture it and mark validated to skip a redundant
                      ;; first-use re-check on the restored warm entry.
                      :identity (list :db (dorg-dolt-server--db-name dd)
                                      :uuid (plist-get
                                             (dorg-dolt-server--dolt-server-info dd)
                                             :uuid))
                      :validated t)
                     dorg-sql--connections)))))))

(defmacro dorg-sql--with-lease (spec &rest body)
  "Acquire an exclusive lease for a store, bind it, run BODY, and release.
SPEC is (LEASE-VAR DOLT-DIR).  BODY reaches the leased session ONLY by passing
LEASE-VAR to `dorg-sql--lease-exec'/`dorg-sql--lease-batch'; ordinary
`dorg-sql'/`dorg-sql-batch' for the store are refused (`dorg-sql-lease-busy'),
including from a re-entrant callback firing while BODY yields.  A
`dorg-sql-lease-broken' signal from BODY forces a BROKEN release (the session is
discarded); any other exit does a normal release (the connection returns warm)."
  (declare (indent 1) (debug ((symbolp form) body)))
  (let ((lease-var (car spec))
        (dolt-dir (cadr spec))
        (broken (make-symbol "broken"))
        (err (make-symbol "err")))
    `(let ((,lease-var (dorg-sql--acquire-lease ,dolt-dir))
           (,broken nil))
       (unwind-protect
           (condition-case ,err
               (progn ,@body)
             (dorg-sql-lease-broken
              (setq ,broken t)
              (signal (car ,err) (cdr ,err))))
         (dorg-sql--release-lease ,lease-var ,broken)))))

;;; ─── Mutation gate: canonical key + server named lock (tracked-case) ──────
;; The per-store mutation gate is a Dolt-server named lock (`GET_LOCK') taken
;; on a leased session (above), so the linearized critical section
;; [ HEAD re-read -> commit-decision -> DOLT_COMMIT -> post-HEAD ] is
;; indivisible across cooperating processes and auto-releases if the owning
;; session dies (crash-safe; no on-disk artifact).  This is NOT the
;; sql-server lifecycle claim lock (a FILE lock) — it is a DB session lock in
;; a distinct namespace.
;;
;; The lock NAME must be byte-identical for every path/symlink/worktree alias
;; of one store and distinct across stores, derived from SERVER-authoritative
;; inputs (never a client path).  It is a bounded (<=64B, the MySQL GET_LOCK
;; cap) hash of the server's current DATABASE() and the repository ROOT
;; (parentless/initial) commit hash, both read on the leased session.

(defconst dorg-sql--mutate-lock-prefix "dorg/mutate/"
  "Namespace prefix for the per-store mutation named lock (tracked-regression).
12 bytes; leaves 52 bytes for the identity hash under the 64-byte GET_LOCK
name cap.")

(defun dorg-sql--lease-scalar (lease query &rest args)
  "Return the first column of the first row of QUERY on LEASE, or nil.
ARGS are `format' args for QUERY.  Reads through the exact leased session
\(`dorg-sql--lease-exec') so it stays inside the connection-affine gate."
  (let ((rows (apply #'dorg-sql--lease-exec lease query args)))
    (cdar (car rows))))

(defun dorg-sql--mutate-lock-key (lease)
  "Return the canonical server-authoritative mutation lock key for LEASE's store.
Both inputs are read on the leased session so they are the SERVER's own view,
not a client path: the current DATABASE() and the repository ROOT commit (the
single parentless commit — its hash never in `dolt_commit_ancestors').  The
key is `dorg-sql--mutate-lock-prefix' || first-52-hex of
sha256(DATABASE() 0x00 ROOT), so aliases of one store converge byte-identically,
distinct stores differ, and the name is <=64 bytes.  Signals
`dorg-sql-lease-broken' if either input is unreadable."
  (let ((db (dorg-sql--lease-scalar lease "SELECT DATABASE() AS db"))
        ;; The parentless (initial) commit is the one whose `dolt_commit_ancestors'
        ;; row carries a NULL `parent_hash' — it IS present in the table (so a
        ;; "NOT IN commit_hash" test wrongly excludes nothing), just with no
        ;; parent edge.  A normal DOrg store has exactly one; ORDER BY pins a
        ;; deterministic choice for any (grafted) multi-root history.
        (root (dorg-sql--lease-scalar
               lease
               (concat "SELECT commit_hash AS h FROM dolt_commit_ancestors"
                       " WHERE parent_hash IS NULL"
                       " ORDER BY commit_hash LIMIT 1"))))
    (unless (and db root)
      (signal 'dorg-sql-lease-broken
              (list (format "dorg: cannot derive mutation lock key (db=%S root=%S)"
                            db root))))
    (concat dorg-sql--mutate-lock-prefix
            (substring (secure-hash 'sha256 (concat db "\0" root)) 0 52))))

(defun dorg-sql--lock-result (v)
  "Normalize a GET_LOCK/RELEASE_LOCK scalar V to `1', `0', or nil.
The leased-session path returns wire strings (uncoerced); NULL is nil.  Returns
1 or 0 as integers and nil for NULL."
  (and v (truncate (string-to-number (format "%s" v)))))

(defun dorg-sql--lease-get-lock (lease key timeout)
  "Acquire the server named lock KEY on LEASE's exact session.
Returns non-nil ONLY when GET_LOCK returns exactly 1.  A 0 (acquire timed out)
returns nil; a NULL (server-side error, e.g. deadlock) fails closed with
`dorg-sql-lease-broken' — never treated as acquired.  TIMEOUT is seconds."
  (let ((got (dorg-sql--lock-result
              (dorg-sql--lease-scalar
               lease "SELECT GET_LOCK(%s, %d) AS got"
               (dorg-sql-quote key) timeout))))
    (cond ((eql got 1) t)
          ((eql got 0) nil)
          (t (signal 'dorg-sql-lease-broken
                     (list (format "dorg: GET_LOCK(%s) returned NULL (error)" key)))))))

(defun dorg-sql--lease-release-lock (lease key)
  "Release the named lock KEY on LEASE's exact session.
RELEASE_LOCK must return exactly 1 (the lock was held by THIS session).  A
0 (held by another session) or NULL (no such lock) violates the exact-ownership
release contract and signals `dorg-sql-lease-ownership-error' — the design
requires 0/NULL to be an error, not a silent success."
  (let ((rel (dorg-sql--lock-result
              (dorg-sql--lease-scalar lease "SELECT RELEASE_LOCK(%s) AS rel"
                                      (dorg-sql-quote key)))))
    (unless (eql rel 1)
      (signal 'dorg-sql-lease-ownership-error
              (list (format "dorg: RELEASE_LOCK(%s) returned %S (expected 1)"
                            key rel))))))

;;; ─── Exactly-once evidence: dorg_mutation_log (tracked-case) ──────────────
;; One row per committed logical mutation, written INSIDE the committing
;; transaction with the data so a lost-response retry — even from a fresh
;; process — reconciles idempotently under the gate rather than double-applying.
;;
;; This is an OPERATIONAL witness, NOT part of the frozen public-0.1 persisted
;; org format (`dorg-schema-ddl'): it is retention-prunable metadata, so it is
;; created idempotently on demand rather than versioned in the schema.  It is
;; created OUTSIDE any committing transaction (Dolt DDL modifies the working set
;; and cannot be rolled back), and its first-use CREATE is folded into the first
;; mutation's own DOLT_COMMIT.
;;
;; `op_id' is the caller-supplied idempotency key (current-state PRIMARY KEY).
;; `gen_id' is a per-generation nonce (NOT NULL UNIQUE): retention-prune + a
;; later same-`op_id' reinsert produces a DISTINCT generation, so the creation
;; commit is recovered generation-safely from `dolt_diff_dorg_mutation_log' by
;; `to_gen_id' — never by ambiguous commit-date ordering, never a circular
;; self-hash.

(defun dorg-sql--mutation-log-schema-row-field (row field)
  "Read FIELD from ROW under the aliased or uppercase client label."
  (or (alist-get field row)
      (alist-get (intern (upcase (symbol-name field))) row)))

(defun dorg-sql--mutation-log-schema-status (query)
  "Return receipt schema status using read-only QUERY.

QUERY is a function accepting one SQL string and returning rows as alists.
The result is `compatible', `missing-table', `legacy-missing-postimages', or
`(incompatible . DETAIL)'.  This deliberately validates the complete column
shape rather than treating a matching core version as proof of receipt health."
  (let ((tables (funcall query
                         (concat "SELECT table_name FROM information_schema.tables"
                                 " WHERE table_schema = DATABASE()"
                                 " AND table_name = 'dorg_mutation_log'"))))
    (if (null tables)
        'missing-table
      (let* ((rows (funcall query
                            (concat "SELECT column_name AS column_name,"
                                    " column_type AS column_type,"
                                    " is_nullable AS is_nullable"
                                    " FROM information_schema.columns"
                                    " WHERE table_schema = DATABASE()"
                                    " AND table_name = 'dorg_mutation_log'"
                                    " ORDER BY ordinal_position")))
             (actual (mapcar
              (lambda (row)
                        (list (downcase
                               (format "%s" (dorg-sql--mutation-log-schema-row-field
                                             row 'column_name)))
                              (downcase
                               (format "%s" (dorg-sql--mutation-log-schema-row-field
                                             row 'column_type)))
                              (downcase
                               (format "%s" (dorg-sql--mutation-log-schema-row-field
                                             row 'is_nullable)))))
                      rows))
             (legacy '(("op_id" "varchar(255)" "no")
                       ("gen_id" "varchar(64)" "no")
                       ("intent_digest" "varchar(64)" "no")
                       ("ts" "bigint" "no")))
             (current (append legacy '(("postimages_v1" "text" "yes")))))
        (cond ((equal actual current) 'compatible)
              ((equal actual legacy) 'legacy-missing-postimages)
              (t (cons 'incompatible actual)))))))

(defun dorg-sql--require-mutation-log-schema (lease)
  "Refuse ordinary receipt access unless LEASE has the current receipt shape.

This check is read-only.  Fresh init and `dorg-migrate-schema' own CREATE/ALTER
through the versioned core schema; ordinary apply and replay never repair a
store's working set."
  (let* ((query (lambda (sql) (dorg-sql--lease-exec lease sql)))
         (status (dorg-sql--mutation-log-schema-status query)))
    (unless (eq status 'compatible)
      (let* ((version-rows
              (funcall query
                       "SELECT value FROM dorg_meta WHERE `key` = 'schema_version'"))
             (version (and version-rows (alist-get 'value (car version-rows))))
             (current (equal (format "%s" version)
                             (number-to-string dorg-schema-version))))
        (signal 'dorg-schema-migration-required
                (list
                 (if current
                     (format "dorg: schema v%s has incompatible mutation receipt storage %S; repair the store before retrying"
                             version status)
                   (format (concat "dorg: mutation receipt schema is %S; run M-x "
                                   "dorg-migrate-schema for the selected store")
                           status))
                 (list :reason (if current 'receipt-schema-incompatible
                                 'schema-migration-required)
                       :component 'receipt :schema-version version
                       :receipt-status status)))))))

(defun dorg-sql--mutation-log-insert-sql (op-id gen-id intent-digest ts)
  "Return the INSERT statement recording evidence (OP-ID GEN-ID INTENT-DIGEST TS).
Appended to the committing batch so the evidence row lands ATOMICALLY with the
mutation's data in one Dolt commit.  The retained nullable legacy payload
column is omitted.  The caller must have checked receipt compatibility via
`dorg-sql--require-mutation-log-schema'."
  (format (concat "INSERT INTO dorg_mutation_log (op_id, gen_id, intent_digest, ts)"
                  " VALUES (%s, %s, %s, %d)")
          (dorg-sql-quote op-id) (dorg-sql-quote gen-id)
          (dorg-sql-quote intent-digest) ts))

(defun dorg-sql--mutation-log-lookup (lease op-id)
  "Return `(GEN-ID . INTENT-DIGEST)' for OP-ID in LEASE's store, or nil if absent.
The gate-first idempotency preflight.  Reads the CURRENT-STATE row on the leased
session; nil means no live evidence (not committed, or pruned past the retention
window).  Caller must have checked receipt compatibility first."
  (let ((rows (dorg-sql--lease-exec
               lease "SELECT gen_id, intent_digest FROM dorg_mutation_log WHERE op_id = %s"
               (dorg-sql-quote op-id))))
    (when rows
      (cons (alist-get 'gen_id (car rows))
            (alist-get 'intent_digest (car rows))))))

(defun dorg-sql--mutation-log-recover-commit (lease gen-id)
  "Return the creation commit hash for evidence generation GEN-ID in LEASE's store.
Generation-safe: filters `dolt_diff_dorg_mutation_log' to the exact `added'
transition of this immutable `gen_id' (NOT NULL UNIQUE), so it never returns a
pruned older generation and uses no commit-date ordering.  Signals
`dorg-sql-lease-broken' if the added transition is not uniquely resolvable
\(zero or multiple rows) — fail closed rather than choose an arbitrary hash."
  (let* ((rows (dorg-sql--lease-exec
                lease
                (concat "SELECT to_commit AS c FROM dolt_diff_dorg_mutation_log"
                        " WHERE diff_type = 'added' AND to_gen_id = %s")
                (dorg-sql-quote gen-id)))
         (n (length rows)))
    (unless (= n 1)
      (signal 'dorg-sql-lease-broken
              (list (format "dorg: mutation_log generation %s has %d added-transitions (expected 1)"
                            gen-id n))))
    (alist-get 'c (car rows))))

(defun dorg-sql--lease-changed-heading-ids (lease from-commit to-commit)
  "Return the heading ids changed between FROM-COMMIT and TO-COMMIT in LEASE's store.
Unions `headings' row changes with `heading_items' changes mapped to their
heading via the `DOLT_DIFF' table function on the leased session — the DB-side
witness of which headings another writer touched, used by the HEAD-equality
overlap classification (tracked-regression §2.4).  Ids are returned as integers to
match the simulation's typed-IR write-set."
  (let ((ids '()))
    (dolist (q (list
                (format (concat "SELECT DISTINCT COALESCE(to_id, from_id) AS id"
                                " FROM DOLT_DIFF(%s, %s, 'headings')")
                        (dorg-sql-quote from-commit) (dorg-sql-quote to-commit))
                (format (concat "SELECT DISTINCT COALESCE(to_heading_id, from_heading_id) AS id"
                                " FROM DOLT_DIFF(%s, %s, 'heading_items')")
                        (dorg-sql-quote from-commit) (dorg-sql-quote to-commit))))
      (dolist (row (dorg-sql--lease-exec lease q))
        (let ((id (alist-get 'id row)))
          (when id
            (push (if (stringp id) (string-to-number id) id) ids)))))
    (delete-dups ids)))

;;; ─── Transport Policy ──────────────────────────────────────────────────

(defun dorg-sql--default-transport ()
  "Return the packaged default `dorg-sql-transport' policy.
The public transport policy is server-only: both interactive and
noninteractive entry points route SQL through a DOrg-managed sql-server.
Standalone one-shot CLI remains available only through the internal
`dorg-sql--with-cli-seam' test/recovery seam."
  'server)

(defcustom dorg-sql-transport (dorg-sql--default-transport)
  "SQL transport policy for Dolt-backed DOrg repositories.

NOTE: the `server' policy value below is TRANSPORT vocabulary (strict
server-transport) — it is intentionally NOT renamed by the Dolt-server
module rename (`dorg-dolt-server.el').  It names how SQL is routed, not
the lifecycle module.

- `server': strict server mode.  Every post-init
  SQL operation routes through a DOrg-managed sql-server via mysql.el or
  errors — it never falls back to CLI.  Reuses a visible same-host server
  that owns the repo.  On the batch/one-shot no-server path
  (`dorg-dolt-server--strict-port'), whether a durable detached daemon is
  started implicitly is governed by `dorg-dolt-server-autostart' (default
  nil): disabled signals `dorg-sql-transport-error' naming `dorg dolt
  start' as the remedy; enabled autostarts the daemon.  (A warm
  `dorg-session' no-server open goes through
  `dorg-dolt-server--ensure-strict' and is not governed by that option.)
  mysql.el being unavailable on a `server' path is a packaging/configuration
  error, not a silent degrade.

One-shot CLI is not a public transport policy.  It remains available only
through the internal `dorg-sql--with-cli-seam' test/recovery seam for
standalone stores that no managed server owns.

The non-SQL lifecycle calls `dolt version' and the initial `dolt init'
for a missing repo are exempt (they are plain process calls); all
post-init SQL, schema, migration, model-load, commit, rollback, and
HEAD/root metadata reads obey this policy.

Callers may still let-bind this option while the public default remains
strict `server'."
  :type '(const :tag "Managed server" server)
  :group 'dorg)

;; KEEP (internal-seam): let-bound nil only by pre-start/test/recovery code (see docstring).
(defvar dorg-sql--cli-conflict-check t
  "Internal guard for one-shot CLI seam conflict checks.
Bound to nil only by internal standalone-store code: managed-server pre-start
validation (after ownership exclusion), tests, or recovery code that must run
one-shot CLI where no live server owns the repo.  Public `dorg-sql-transport'
never reaches one-shot CLI through this variable.")

(defvar dorg-sql--pinned-transport nil
  "When non-nil, a (EXPANDED-DOLT-DIR . TRANSPORT-CELL) pin.
While bound, `dorg-sql--resolve-transport' returns TRANSPORT-CELL for
EXPANDED-DOLT-DIR instead of re-resolving `dorg-sql-transport'.  Set by
`dorg-sql--with-pinned-transport' so a multi-statement operation and its
rollback/reset run on ONE coherent transport even if the global policy is
rebound mid-operation.  Keyed by dir so an unrelated repo touched inside
the body still resolves normally.")

(defun dorg-sql--pinned-transport-for (dolt-dir)
  "Return the pinned transport cell for DOLT-DIR, or nil when none applies."
  (and dorg-sql--pinned-transport
       (equal (car dorg-sql--pinned-transport) (expand-file-name dolt-dir))
       (cdr dorg-sql--pinned-transport)))

;; ─── Transport-policy seams (tracked-case) ───
;; Transport POLICY (server/claim-aware resolution + the one-shot CLI live-owner
;; refusal) lives ABOVE this primitive, in `dorg-transport' (which requires both
;; `dorg-dolt' and `dorg-dolt-server').  `dorg-dolt' invokes it through these
;; function-variable seams so control flows DOWNWARD (transport -> server ->
;; dolt) instead of the primitive reaching up.  Both keep a nil default so
;; `(require \\='dorg-dolt)' stays load-time pure (no upward/heavy edge); the
;; policy module installs them at load via `(with-eval-after-load \\='dorg-dolt …)'.
(defvar dorg-sql--resolve-transport-fn nil
  "Seam: transport-policy resolver, installed by `dorg-transport'.
When nil, `dorg-sql--ensure-transport-policy' lazy-loads the policy module
on demand so a standalone `(require \\='dorg-dolt)' user (e.g. `dorg-repo'
lifecycle through `dorg-sql--with-pinned-transport') is order-independent.
Called as (FN DOLT-DIR) and returns the resolved transport cell.")

(defvar dorg-sql--cli-preflight-fn nil
  "Seam: one-shot CLI preflight guard, installed by `dorg-transport'.
Called as (FN TARGET) at the `dorg-sql--cli-exec' primitive boundary
immediately before `make-process'.  nil is a no-op — identical to today's
`dorg-sql--with-cli-seam' behavior, where a standalone store has no live
claim to refuse.")

(defun dorg-sql--install-transport-policy (owner resolver preflight)
  "Install OWNER's RESOLVER and PREFLIGHT into the transport-policy seams.
Only `dorg-transport' owns this pair.  Reinstalling the identical symbol-valued
functions is an idempotent no-op; every foreign, partial, malformed, or
different replacement signals `dorg-seam-install-error' before either seam is
changed."
  (unless (and (eq owner 'dorg-transport)
               (functionp resolver)
               (functionp preflight))
    (signal 'dorg-seam-install-error
            (list "dorg: refused foreign or malformed transport seam install"
                  :owner owner)))
  (cond
   ((and (null dorg-sql--resolve-transport-fn)
         (null dorg-sql--cli-preflight-fn))
    (setq dorg-sql--resolve-transport-fn resolver
          dorg-sql--cli-preflight-fn preflight))
   ((and (eq resolver dorg-sql--resolve-transport-fn)
         (eq preflight dorg-sql--cli-preflight-fn))
    nil)
   (t
    (signal 'dorg-seam-install-error
            (list "dorg: refused replacement of installed transport seams"
                  :owner owner)))))

(defun dorg-sql--ensure-transport-policy ()
  "Ensure the transport-policy module is loaded so the resolve seam is installed.
Lazy composition half of the seam contract: when
`dorg-sql--resolve-transport-fn' is nil, runtime-`require' `dorg-transport'
so a direct `(require \\='dorg-dolt)' + pin-macro use self-composes the
policy layer.  A genuinely-absent module (not on load-path) leaves the seam
nil and the caller signals a clear void-function error."
  (unless dorg-sql--resolve-transport-fn
    (require 'dorg-transport nil t)))

(defun dorg-sql--transport-kind (dolt-dir)
  "Return the transport KIND for DOLT-DIR — `mysql' or `cli' — WITHOUT a port.
This is the policy half of `dorg-sql--resolve-transport' with NO strict-port
resolution: the mysql endpoint port is resolved lazily by
`dorg-sql--get-connection' only on a cache miss, so a warm connection performs
zero strict-port discovery (the A1 endpoint-ownership contract).  Honors an
active pin and the strict `server' policy; rejects any other public value."
  (let ((pinned (dorg-sql--pinned-transport-for dolt-dir)))
    (cond
     (pinned (car pinned))
     ((eq dorg-sql-transport 'server)
      'mysql)
     (t
      (signal 'dorg-sql-transport-error
              (list (format "dorg: `%S' is not a public SQL transport for %s; use `server' or the internal `dorg-sql--with-cli-seam' only for tests/recovery"
                            dorg-sql-transport dolt-dir)))))))

(defmacro dorg-sql--with-pinned-transport (dolt-dir &rest body)
  "Resolve transport for DOLT-DIR once and pin it for BODY.
Every `dorg-sql' / `dorg-sql-batch' call in BODY against DOLT-DIR reuses
the single resolved transport instead of re-resolving `dorg-sql-transport'
per call, so a multi-step operation and its rollback/reset run on one
coherent transport even if the policy is rebound mid-body.  Nesting for
the same dir is idempotent (the inner resolve returns the existing pin)."
  (declare (indent 1) (debug (form body)))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (expand-file-name ,dolt-dir))
            (dorg-sql--pinned-transport
             (cons ,dir
                   ;; An already-active pin (notably `dorg-sql--with-cli-seam')
                   ;; short-circuits BEFORE composing the policy layer, so a
                   ;; standalone CLI seam never loads `dorg-transport'.  Only a
                   ;; genuine resolve lazy-composes the policy module.
                   (or (dorg-sql--pinned-transport-for ,dir)
                       (progn (dorg-sql--ensure-transport-policy)
                              (funcall dorg-sql--resolve-transport-fn ,dir))))))
       ,@body)))

(defmacro dorg-sql--with-cli-seam (dolt-dir &rest body)
  "Force one-shot CLI transport for DOLT-DIR within BODY, bypassing policy.
This is the SOLE sanctioned non-`server' transport path — a test/recovery
seam.  It pins the resolved transport for DOLT-DIR to one-shot CLI
\(`dorg-sql--cli-exec', including through `dorg-sql-batch') regardless of
`dorg-sql-transport', by reusing the `dorg-sql--pinned-transport'
short-circuit in transport resolution.  It exists because a
standalone Dolt store that no managed sql-server holds — a server-lifecycle
test fixture, a `bin/dorg' subprocess store, or a disaster-recovery repair
on a store whose server cannot start — must be reached by one-shot CLI, not
the managed server.  `dorg-sql--cli-conflict-check' is bound nil because a
standalone store has no live claim to contend with.

This seam is NEVER reachable through the public `dorg-sql-transport' policy;
production routing stays server-only.  It is the piece the server-only
transport collapse (tracked-case) must preserve — see this bead's Design
`Delete-scope contract'.  Nesting for the same dir is idempotent."
  (declare (indent 1) (debug (form body)))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (expand-file-name ,dolt-dir))
            (dorg-sql--pinned-transport (cons ,dir (cons 'cli nil)))
            (dorg-sql--cli-conflict-check nil))
       ,@body)))

;; The one-shot CLI live-owner refusal guard (`dorg-sql--cli-refuse-live-owner')
;; is transport POLICY — it consults the server/claim layer — so it lives in
;; `dorg-transport' and is invoked here through the `dorg-sql--cli-preflight-fn'
;; seam at the `dorg-sql--cli-exec' primitive boundary.

;;; ─── Query Routing ─────────────────────────────────────────────────────

(defun dorg-sql (dolt-dir query &rest args)
  "Execute SQL QUERY in DOLT-DIR via dolt, return parsed results.
ARGS are `format' arguments applied to QUERY before execution.
Returns a list of alists for SELECT queries, or nil for writes.
Routing obeys `dorg-sql-transport': through a persistent mysql
connection to a sql-server, or one-shot CLI only when an internal seam
has pinned that transport.  Signals an error if the query fails or if the
public policy cannot be satisfied."
  (dorg-dolt--ensure-available)
  (dorg-sql--assert-not-leased dolt-dir)
  (let* ((formatted (if args (apply #'format query args) query))
         (target (dorg-dolt--target dolt-dir))
         (default-directory (plist-get target :repo-dir)))
    ;; Decide transport WITHOUT resolving a port; the warm connection resolves
    ;; strict-port lazily only on a cache miss (A1: no per-query discovery).
    (pcase (dorg-sql--transport-kind dolt-dir)
      ('mysql (dorg-sql--mysql-exec formatted (dorg-sql--get-connection dolt-dir)))
      (_      (dorg-sql--cli-exec formatted target)))))

(defun dorg-sql--revision-db-identifier (dolt-dir revision)
  "Return the quoted revision-database identifier `DB/REVISION' for DOLT-DIR.
A Dolt REVISION DATABASE is the SQL engine's own revision-bound read
scope: with the session database set to `db/<commit>', every unqualified
table reference in a statement — including repeated and nested-subquery
uses — resolves read-only at that committed revision, on both the
sql-server and one-shot CLI transports.  REVISION must be a committed
hash token (alphanumeric, as `dorg-head-commit' returns); anything else
signals, so a caller can never smuggle SQL through the identifier
position or pin to a non-committed token.  DB is the served database
name (`dorg-dolt-server--db-name'), the same name the managed connection
selects."
  (let ((db (dorg-dolt-server--db-name dolt-dir)))
    (unless (and (stringp revision)
                 (string-match-p "\\`[0-9A-Za-z]+\\'" revision))
      (error "dorg: revision-bound SQL requires a committed hash token, got %S"
             revision))
    (when (string-match-p "`" db)
      (error "dorg: database name %S cannot be identifier-quoted" db))
    (format "`%s/%s`" db revision)))

(defun dorg-sql--mask-sql-noncode (sql)
  "Return a length-preserving copy of SQL with non-code spans blanked.
String-literal CONTENTS (`'…'' / \"…\" with backslash escapes and quote
doubling) and comments (`-- ' with trailing whitespace, `#', `/*…*/')
become spaces; literal delimiters and backtick-quoted identifier
contents are kept (identifiers are code).  Offsets are shared with SQL
because the copy never changes length.  Returns nil when SQL ends
inside an unterminated literal, quoted identifier, or block comment —
callers fail closed on nil.  This is a boundary mask, not a SQL parser:
downstream policy stays declarative regexps over the masked text."
  (let* ((len (length sql))
         (masked (copy-sequence sql))
         (i 0) (bad nil))
    (while (and (< i len) (not bad))
      (let ((c (aref sql i)))
        (cond
         ;; String literal: blank contents, keep delimiters.
         ((memq c '(?' ?\"))
          (let ((j (1+ i)) (end nil))
            (while (and (not end) (not bad))
              (cond
               ((>= j len) (setq bad t))
               ((eq (aref sql j) ?\\)
                (aset masked j ?\s)
                (when (< (1+ j) len) (aset masked (1+ j) ?\s))
                (setq j (+ j 2)))
               ((eq (aref sql j) c)
                (if (and (< (1+ j) len) (eq (aref sql (1+ j)) c))
                    (progn (aset masked j ?\s) (aset masked (1+ j) ?\s)
                           (setq j (+ j 2)))
                  (setq end j)))
               (t (aset masked j ?\s) (setq j (1+ j)))))
            (setq i (1+ (or end j)))))
         ;; Backtick identifier: contents are code — keep them.
         ((eq c ?`)
          (let ((j (1+ i)) (end nil))
            (while (and (not end) (not bad))
              (cond
               ((>= j len) (setq bad t))
               ((eq (aref sql j) ?`)
                (if (and (< (1+ j) len) (eq (aref sql (1+ j)) ?`))
                    (setq j (+ j 2))
                  (setq end j)))
               (t (setq j (1+ j)))))
            (setq i (1+ (or end j)))))
         ;; Comments become spaces (so a comment reads as whitespace).
         ((or (eq c ?#)
              (and (eq c ?-) (< (1+ i) len) (eq (aref sql (1+ i)) ?-)
                   (or (>= (+ i 2) len)
                       (memq (aref sql (+ i 2)) '(?\s ?\t ?\n ?\r)))))
          (while (and (< i len) (not (eq (aref sql i) ?\n)))
            (aset masked i ?\s)
            (setq i (1+ i))))
         ((and (eq c ?/) (< (1+ i) len) (eq (aref sql (1+ i)) ?*))
          (let ((end (string-search "*/" sql (+ i 2))))
            (if (not end) (setq bad t)
              (while (<= i (1+ end))
                (aset masked i ?\s)
                (setq i (1+ i))))))
         (t (setq i (1+ i))))))
    (and (not bad) masked)))

(defun dorg-sql--revision-unsafe-reason (sql db-name)
  "Return a reason string when SQL cannot run revision-safely, else nil.
The structural revision scope binds every reference resolved through the
session database; the checks below reject the forms that would resolve
OUTSIDE it — each a declarative regexp over a literal/comment mask
\(`dorg-sql--mask-sql-noncode'), never SQL parsing:

- a second statement (`;'), which could smuggle a scope-switching `USE';
- a leading `USE' statement itself;
- a revision/branch-qualified identifier (any backticked name with `/');
- a DB-NAME-qualified reference — `db.tbl', backticked or with comments
  between name and dot (comments are masked to spaces, so they read as
  whitespace);
- any `AS OF': the DOrg revision scope is the SOLE snapshot authority,
  so a caller-supplied pin is rejected even when it names the captured
  hash (`dorg-ext-read-as-of-suffix' belongs to the separate multi-call
  read-session protocol, not inside a structurally bound plan).

Conservative by design: an alias spelled like DB-NAME followed by a dot
is rejected even though it would be legal SQL — rename the alias rather
than shadow the database."
  (let ((masked (dorg-sql--mask-sql-noncode sql)))
    (if (not masked)
        "unterminated string literal, quoted identifier, or comment"
      (let ((m (downcase masked))
            (qdb (regexp-quote (downcase db-name)))
            (case-fold-search nil))
        (cond
         ((string-search ";" m)
          "multiple statements in one plan element")
         ((string-match-p "\\`[ \t\n\r]*use\\(?:\\'\\|[^a-z0-9_$]\\)" m)
          "USE is not a plan statement")
         ;; Pair backtick spans positionally (left-to-right, non-greedy per
         ;; span) so a bare `/' BETWEEN two quoted identifiers — division —
         ;; can never bridge a closing and an opening delimiter.
         ((let ((start 0) (hit nil))
            (while (and (not hit) (string-match "`\\([^`]*\\)`" m start))
              (if (string-search "/" (match-string 1 m))
                  (setq hit t)
                (setq start (match-end 0))))
            hit)
          "revision/branch-qualified identifier")
         ((string-match-p (concat "`" qdb "`[ \t\n\r]*\\.") m)
          "live-database-qualified reference")
         ((string-match-p (concat "\\(?:\\`\\|[^a-z0-9_$`]\\)" qdb
                                  "[ \t\n\r]*\\.")
                          m)
          "live-database-qualified reference")
         ((string-match-p
           "\\(?:\\`\\|[^a-z0-9_$]\\)as[ \t\n\r]+of\\(?:\\'\\|[^a-z0-9_$]\\)"
           m)
          "AS OF is not allowed in a revision-bound plan (the scope is the pin)")
         (t nil))))))

(defun dorg-sql--lease-exec-at-revision (lease revdb db query)
  "Run QUERY on LEASE pinned to revision database REVDB; restore DB after.
Returns QUERY's rows.  Progress is tracked through explicit STAGES so
the exit path can distinguish PROOF from indeterminacy:

- `pinning' -> only a TYPED completed server rejection of the `USE'
  \(`mysql-query-error', e.g. an unknown revision) proves the server did
  not apply the pin: stage becomes `never-pinned', the error propagates,
  and the still-coherent session may be released normally.
- `pinned' -> the statement runs; only a TYPED completed SQL response
  \(`mysql-query-error') proves the stream is still coherent — then
  restoration is attempted and the fault re-signalled after it.
- `restored' -> the restoring `USE DB' completed: the ONLY state that
  PROVES the session is back on the base database.

A GENERIC error proves nothing, even on a live socket: during the pin
it may fire after the `USE' was sent or applied (client-side mid-read
failure), and during the statement it may leave unread response bytes
that a restoring `USE' would consume and spuriously succeed on.  Such
failures — `dorg-sql-lease-broken' included — and every non-`error'
exit (a `quit' mid-I/O) unwind with `pinning'/`pinned' intact and issue
NO follow-on I/O.  The cleanup then enforces the proof requirement:
unless the stage is `never-pinned' or `restored', the connection is
stripped from the lease (atomically, quits inhibited) and disconnected,
so the subsequent release re-pools NOTHING and a later caller can never
inherit a possibly-pinned session.  A completed-but-failed restoration
after a successful statement additionally fails closed with
`dorg-sql-lease-broken' (rows are never returned off an unproven
session)."
  (let ((stage 'pinning) (rows nil) (fault nil))
    (unwind-protect
        (progn
          (condition-case use-err
              (dorg-sql--lease-exec lease (format "USE %s" revdb))
            (mysql-query-error (setq stage 'never-pinned)
                               (signal (car use-err) (cdr use-err))))
          (setq stage 'pinned)
          (condition-case stmt-err
              (setq rows (dorg-sql--lease-exec lease query))
            (mysql-query-error (setq fault stmt-err)))
          (condition-case rest-err
              (progn (dorg-sql--lease-exec lease (format "USE `%s`" db))
                     (setq stage 'restored))
            (mysql-query-error
             (unless fault
               (dorg-sql--raise-lease-broken lease rest-err))))
          (when fault (signal (car fault) (cdr fault)))
          rows)
      (unless (memq stage '(never-pinned restored))
        ;; No proof of restoration: strip the session before anything else
        ;; can observe it.  The conn slot is cleared first under
        ;; `inhibit-quit' so even a quit storm cannot leave the release a
        ;; connection to re-pool; the disconnect is best-effort after that.
        (let ((inhibit-quit t))
          (let ((conn (dorg-sql--lease-conn lease)))
            (setf (dorg-sql--lease-conn lease) nil)
            (when conn (ignore-errors (mysql-disconnect conn)))))))))

(defun dorg-sql--at-revision (dolt-dir revision query &rest args)
  "Execute SQL QUERY in DOLT-DIR revision-bound to committed REVISION.
ARGS are `format' arguments applied to QUERY before execution.  Returns
parsed rows exactly like `dorg-sql', but the statement executes inside
the Dolt REVISION DATABASE `db/REVISION' — deterministic history — so
every unqualified table reference, repeated or nested, reads at
REVISION regardless of concurrent HEAD advances.  The scope is
DOrg-owned and SOLE: the statement text carries no `AS OF' — any
`AS OF', including one naming REVISION, is rejected by validation.

Fail-closed validation precedes execution: a statement containing a
form that would resolve OUTSIDE the revision scope — database
qualification (live or revision/branch), a smuggled second statement, a
leading `USE', or any `AS OF' (the scope is the sole snapshot
authority) — signals `dorg-sql-revision-unsafe' before any SQL is
issued.  The check is a set of declarative regexps over a
literal/comment mask (`dorg-sql--revision-unsafe-reason'), not a SQL
parser.

Under the managed mysql transport the pin, statement, and restoring
`USE' run on the EXCLUSIVE session lease (`dorg-sql--with-lease'):
re-entrant same-store SQL arriving while any of them yields is refused
typed (`dorg-sql-lease-busy'), never multiplexed onto the pinned
session, and the release re-pools the session ONLY on proof that
restoration completed — any other exit, a quit mid-I/O included,
strips and discards it (`dorg-sql--lease-exec-at-revision').
Under the internal CLI seam the `USE' prefix and statement share one
`dolt sql' invocation, whose process-scoped session cannot leak.  On
either transport a non-committed REVISION fails before the statement
executes (`USE' of an unknown revision database errors), and revision
databases are read-only, so a stray write in a plan fails loudly."
  (dorg-dolt--ensure-available)
  (dorg-sql--assert-not-leased dolt-dir)
  (let* ((formatted (if args (apply #'format query args) query))
         (db (dorg-dolt-server--db-name dolt-dir))
         (revdb (dorg-sql--revision-db-identifier dolt-dir revision))
         (unsafe (dorg-sql--revision-unsafe-reason formatted db)))
    (when unsafe
      (signal 'dorg-sql-revision-unsafe
              (list (format "dorg: revision-unsafe statement (%s): %s"
                            unsafe formatted))))
    (pcase (dorg-sql--transport-kind dolt-dir)
      ('mysql
       (dorg-sql--with-lease (lease dolt-dir)
         (dorg-sql--lease-exec-at-revision lease revdb db formatted)))
      (_ (let* ((target (dorg-dolt--target dolt-dir))
                (default-directory (plist-get target :repo-dir)))
           (dorg-sql--cli-exec (format "USE %s; %s" revdb formatted)
                               target))))))

(defcustom dorg-sql-cli-arg-max-threshold 131072
  "Byte length at or above which CLI SQL is streamed over stdin.
`dorg-sql--cli-exec' passes short queries to dolt as a `-q' argv, but a
large batch (e.g. a multi-megabyte import) exceeds the operating
system's ARG_MAX limit on argument length and fails with \"Argument
list too long\".  Queries whose UTF-8 byte length reaches this
threshold are sent to dolt over stdin instead, where no argv length
limit applies.  Bind this to a small value in tests to exercise the
stdin transport without depending on the host's actual ARG_MAX."
  :type 'integer
  :group 'dorg)

(defcustom dorg-sql-cli-timeout 30.0
  "Hard wall-clock bound, in seconds, for a one-shot `dolt sql' CLI read.
`dorg-sql--cli-exec' runs `dolt' as an asynchronous child and waits at
most this long for it to exit.  A deadlocked one-shot — e.g. one that
contends on a live sql-server's Dolt write lock (the pre-start
deadlock class) — is reaped (SIGTERM->SIGKILL, never left
orphaned) and a `dorg-sql-transport-error' is signalled instead of
hanging forever.  The default is generous for a legitimate batch import
yet far below an infinite block; bind it small in tests to exercise the
deadline path."
  :type 'number
  :group 'dorg)

(defun dorg-sql--cli-exec (query target)
  "Execute QUERY via CLI dolt sql against normalized TARGET.
Short queries are passed as a `dolt sql -q' argv; queries whose
byte length reaches `dorg-sql-cli-arg-max-threshold' are streamed
over stdin so the SQL body never becomes a process argument and
cannot exceed ARG_MAX.  Binds `default-directory' to TARGET's repo dir
so Dolt opens the target repository independently of caller cwd.

The child runs asynchronously and is bounded by `dorg-sql-cli-timeout':
on deadline it is reaped (`dorg-dolt-server--kill-process', SIGTERM->SIGKILL)
so no `dolt sql' child is ever orphaned, and a `dorg-sql-transport-error'
naming the repo and timeout is signalled rather than blocking forever —
the safety net for any transport mis-selection that routes a one-shot CLI
at a repo whose live owner still holds the Dolt write lock.

Managed-server pre-start validation is the one lifecycle caller outside this
module that invokes the primitive directly.  Its claim-lock path has already
excluded a live owner, and it can run inside lease acquisition, so it
dynamically disables the CLI conflict check without entering the ordinary-query
lease guard."
  ;; Transport-policy preflight (claim-aware live-owner refusal) via the seam
  ;; installed by `dorg-transport'.  nil = standalone/no-policy: a no-op,
  ;; identical to the `dorg-sql--with-cli-seam' path where no live claim exists.
  (when dorg-sql--cli-preflight-fn
    (funcall dorg-sql--cli-preflight-fn target))
  (let* ((default-directory (plist-get target :repo-dir))
         (large (>= (string-bytes query) dorg-sql-cli-arg-max-threshold))
         (buf (generate-new-buffer " *dorg-sql*"))
         ;; Large query: stream the SQL over stdin (no `-q').  dolt reads
         ;; statements from stdin and emits no progress text on this path,
         ;; so stderr (errors only) merges cleanly into BUF for
         ;; `dorg-sql--parse-result' just like the -q path.
         (args (if large
                   (dorg-dolt--target-sql-args target "-r" "json")
                 (dorg-dolt--target-sql-args target "-q" query "-r" "json")))
         (proc nil))
    (unwind-protect
        (progn
          (setq proc (make-process
                      :name "dorg-sql-cli"
                      :buffer buf
                      :command (cons "dolt" args)
                      :connection-type 'pipe
                      :noquery t
                      :sentinel #'ignore))
          (when large
            (process-send-string proc query)
            (process-send-eof proc))
          (let ((deadline (+ (float-time) dorg-sql-cli-timeout)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1 nil t)))
          (when (process-live-p proc)
            ;; Deadline blown: the child is wedged (likely on the Dolt
            ;; write lock).  Reap it so nothing is orphaned, then signal a
            ;; bounded transport error.
            (dorg-dolt-server--kill-process proc)
            (signal 'dorg-sql-transport-error
                    (list (format "dorg: one-shot `dolt sql' for %s exceeded `dorg-sql-cli-timeout' (%.1fs) and was reaped; the repo's Dolt write lock is likely held by a live sql-server"
                                  (plist-get target :repo-dir)
                                  dorg-sql-cli-timeout))))
          (dorg-sql--parse-result buf (process-exit-status proc) query nil))
      (when (process-live-p proc)
        (dorg-dolt-server--kill-process proc))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun dorg-sql--parse-result (buf exit-code query &optional _server-p)
  "Parse dolt output in BUF after EXIT-CODE for QUERY.
Row values keep the types the Dolt JSON writer gave them: SQL numbers are
JSON numbers, and text and DECIMAL are JSON strings, so nothing is converted
by its spelling.  A SQL NULL column is an absent member, and raw binary is
not byte-exact in JSON, so byte readers select HEX()."
  (with-current-buffer buf
    (if (/= exit-code 0)
        (error "dolt sql failed (exit %d): %s\nQuery: %s"
               exit-code
               (string-trim (buffer-string))
               query)
      (let ((output (string-trim (buffer-string))))
        (when (not (string-empty-p output))
          ;; Fast path: a single SQL statement yields exactly one JSON
          ;; object and Dolt emits no trailing content, so the whole
          ;; trimmed buffer parses directly.  This preserves literal
          ;; braces, escaped quotes/backslashes, and interpolation-like
          ;; text (e.g. "${VAR}") inside string values, which the
          ;; brace-counting fallback below cannot reconstruct safely.
          ;; `json-parse-string' rejects trailing non-whitespace, so a
          ;; real Dolt error or stray content is not silently accepted —
          ;; it falls through to the legacy fallback and, failing that,
          ;; to a `dorg-dolt-query-error'.
          (let ((parsed (dorg-sql--parse-json-or-nil output)))
            (unless parsed
              ;; Legacy fallback: multi-object batch output or leading
              ;; progress text precedes the final {"rows":...} object.
              ;; Extract the last top-level object with a string/escape
              ;; aware scan, then parse that.
              (let ((to-parse (dorg-sql--last-json-object output)))
                (setq parsed
                      (condition-case err
                          (json-parse-string to-parse
                                             :object-type 'alist
                                             :array-type 'list)
                        (json-parse-error
                         (signal 'dorg-dolt-query-error
                                 (list (format "Failed to parse dolt output: %s\nRaw output: %s"
                                               (error-message-string err)
                                               output))))))))
            (and (consp parsed) (alist-get 'rows parsed))))))))

(defun dorg-sql--parse-json-or-nil (str)
  "Parse STR as JSON, returning the value or nil if STR is not valid JSON.
Objects become alists and arrays become lists, matching the parse
options used throughout `dorg-dolt'.  Returns nil on any parse error so
callers can distinguish the single-object fast path from output that
needs legacy multi-object extraction."
  (condition-case nil
      (json-parse-string str :object-type 'alist :array-type 'list)
    (json-parse-error nil)))

(defun dorg-sql--last-json-object (str)
  "Return the last top-level JSON object in STR.
Legacy Dolt batch output can contain leading progress text or several
JSON objects, one per statement, before the final {\"rows\":...} object.

Scan FORWARD from the start, tracking JSON string and escape state, so
braces appearing inside string values are never counted as structural
delimiters.  A backward scan cannot do this: whether a `\"' is escaped
depends on the parity of the backslashes preceding it, which is
undecidable right-to-left without a left anchor.  Record each top-level
{...} object as it completes and return the last one.  If no complete
top-level object is found, return STR unchanged so the caller surfaces a
parse error on the raw output."
  (let ((len (length str))
        (i 0)
        (depth 0)
        (in-string nil)
        (escaped nil)
        (obj-start nil)
        (last-start nil)
        (last-end nil))
    (while (< i len)
      (let ((c (aref str i)))
        (cond
         ;; Inside a string: a backslash escapes the next char; an
         ;; unescaped quote closes the string.  Braces are inert here.
         (escaped (setq escaped nil))
         (in-string
          (cond ((= c ?\\) (setq escaped t))
                ((= c ?\") (setq in-string nil))))
         ;; Structural context: track string entry and brace depth.
         ((= c ?\") (setq in-string t))
         ((= c ?\{)
          (when (= depth 0) (setq obj-start i))
          (setq depth (1+ depth)))
         ((= c ?\})
          (when (> depth 0)
            (setq depth (1- depth))
            (when (and (= depth 0) obj-start)
              (setq last-start obj-start
                    last-end i
                    obj-start nil))))))
      (setq i (1+ i)))
    (if (and last-start last-end)
        (substring str last-start (1+ last-end))
      str)))

(defun dorg-sql-batch (dolt-dir statements &optional commit-message)
  "Execute multiple SQL STATEMENTS in DOLT-DIR in a single call.
STATEMENTS is a list of SQL strings.

When COMMIT-MESSAGE is non-nil, wraps the batch in a transaction
with DOLT_ADD + DOLT_COMMIT.  On error, issues ROLLBACK and
re-signals.  Returns nil.

Transport obeys `dorg-sql-transport': strict server uses a persistent
mysql connection, while the internal CLI seam joins all statements into
one one-shot process.  Rollback runs on whichever transport this call
selected — strict `server' mode never rolls back via CLI."
  (dorg-dolt--ensure-available)
  (dorg-sql--assert-not-leased dolt-dir)
  (pcase (dorg-sql--transport-kind dolt-dir)
    ('mysql
     ;; Warm-connection coherence: the batch and its ROLLBACK run on the ONE
     ;; cached connection, so no transport pin is needed and the warm path does
     ;; no strict-port discovery.
     (dorg-sql--mysql-batch-on-conn (dorg-sql--get-connection dolt-dir)
                                    statements commit-message)
     nil)
    (_
     ;; Internal CLI seam: join the whole batch into one one-shot process, with
     ;; the transport pinned so the combined SQL and its ROLLBACK reuse it.
     (dorg-sql--with-pinned-transport dolt-dir
       (let* ((all (if commit-message
                       (let ((author (format "%s <%s>"
                                             dorg-dolt-user-name
                                             dorg-dolt-user-email)))
                         (append (list "BEGIN")
                                 statements
                                 (list "CALL DOLT_ADD('.')"
                                       (format "CALL DOLT_COMMIT('-m', %s, '--author', %s)"
                                               (dorg-sql-quote commit-message)
                                               (dorg-sql-quote author)))))
                     statements))
              (combined (mapconcat #'identity all ";\n")))
         (if commit-message
             (condition-case err
                 (progn (dorg-sql dolt-dir combined) nil)
               (error
                (if (dorg-sql-batch--nothing-to-commit-p err)
                    nil
                  (let ((rollback-failure
                         (condition-case rb
                             (progn (dorg-sql dolt-dir "ROLLBACK") nil)
                           (error (error-message-string rb)))))
                    (if rollback-failure
                        (signal (car err)
                                (list (format "%s [ROLLBACK ALSO FAILED: %s]"
                                              (error-message-string err)
                                              rollback-failure)))
                      (signal (car err) (cdr err)))))))
           (dorg-sql dolt-dir combined)
           nil))))))

(defun dorg-sql-batch--nothing-to-commit-p (err)
  "Return non-nil if ERR is a Dolt \"nothing to commit\" error."
  (and (consp (cdr err))
       (stringp (cadr err))
       (string-match-p "nothing to commit" (cadr err))))

;;; Repository Management

(defun dorg-head-commit (dolt-dir)
  "Return the current HEAD commit hash string for DOLT-DIR."
  (let* ((rows (dorg-sql dolt-dir "SELECT HASHOF('HEAD') AS hash"))
         (hash (alist-get 'hash (car rows))))
    (unless hash
      (error "dorg: failed to read HEAD commit hash from %s" dolt-dir))
    hash))

(defun dorg-commit-ancestor-p (dolt-dir commit)
  "Return non-nil if COMMIT is reachable from HEAD in DOLT-DIR.
`dolt_log' lists exactly the commits reachable from the current HEAD (its
ancestors, HEAD included), so a hit means COMMIT is an ancestor of — or equal
to — HEAD.  History and recovery callers use this to reject revisions outside
the current lineage.  nil COMMIT returns nil."
  (and commit
       (consp (dorg-sql dolt-dir
                        (format "SELECT 1 AS a FROM dolt_log WHERE commit_hash = %s LIMIT 1"
                                (dorg-sql-quote commit))))))

(defun dorg-commit (dolt-dir message)
  "Stage all changes and commit in DOLT-DIR with MESSAGE.
Uses DOLT_ADD/DOLT_COMMIT stored procedures, routed through the
persistent sql-server when one is running and otherwise through the
public SQL transport policy (see `dorg-sql').
Does nothing if there are no changes to commit.
Uses `dorg-dolt-user-name' and `dorg-dolt-user-email' as author."
  ;; Choose one coherent transport for the whole commit.  In strict
  ;; `server' mode, resolve the DOrg-managed server up front so the
  ;; DOLT_ADD/DOLT_COMMIT below route through it (never CLI).  An internal
  ;; pinned transport (notably `dorg-sql--with-cli-seam') is already the
  ;; operation's chosen transport, so do not apply public server ensure on
  ;; top of it.
  (unless (dorg-sql--pinned-transport-for dolt-dir)
    (pcase dorg-sql-transport
      ('server (dorg-dolt-server--strict-port dolt-dir))
      (_ (dorg-sql--ensure-transport-policy)
         (funcall dorg-sql--resolve-transport-fn dolt-dir))))
  (let ((author (format "%s <%s>" dorg-dolt-user-name dorg-dolt-user-email)))
    (dorg-sql dolt-dir "CALL DOLT_ADD('.')")
    (condition-case nil
        (dorg-sql dolt-dir "CALL DOLT_COMMIT('-m', %s, '--author', %s)"
                  (dorg-sql-quote message)
                  (dorg-sql-quote author))
      ;; DOLT_COMMIT raises an error when nothing to commit
      (error nil))))

;;; DB → Model Loader
;;
;; The hydration loaders (`dorg-model-load-tree' / `-headings') and the
;; `AS OF' SQL-fragment helper moved out of this transport module to honor the
;; layer boundary (tracked-case): the loaders now live in the `dorg-model-load'
;; bridge (DB↔model), and the SQL-text helper is `dorg-sql-gen--as-of-suffix'.
;; Back-compat autoloads near the top of this file keep the loaders resolvable
;; through `(require \\='dorg-dolt)'.

;; Hard mysql.el dependency.  A top-level CALL runs at load, not at
;; byte-compile, so `make compile'/`lint' (which do not add `-L lib/mysql')
;; stay green while `(require 'dorg-dolt)' fails fatally on a host lacking
;; mysql.el.  See `dorg-sql--require-mysql-hard'.
(dorg-sql--require-mysql-hard)

;;; Transient-error retry (tracked-regression, relocated tracked-regression seam)

(defcustom dorg-dolt-max-retries 3
  "Maximum number of retries for transient Dolt errors.
Covers Dolt serialization failures (error 1213) and
transaction-sequencing constraint rollbacks (error 1105)."
  :type 'integer
  :group 'dorg)

(defcustom dorg-dolt-retry-base-ms 50
  "Base delay in milliseconds for Dolt retry backoff."
  :type 'integer
  :group 'dorg)

(defun dorg-dolt--retryable-error-p (err)
  "Return non-nil if ERR is a transient error worth retrying.
Detects Dolt serialization failures (1213) and transaction-sequencing
constraint violations (1105 with \"constraint violations\").
Private since tracked-regression: `dorg-dolt-with-retry' is the public seam."
  (eq 'serialization
      (plist-get (dorg-dolt--classify-db-error err) :class)))

(defun dorg-dolt--retry-delay (attempt)
  "Return delay in seconds for retry ATTEMPT (0-based).
Exponential backoff with ±50% jitter.
Private since tracked-regression: `dorg-dolt-with-retry' is the public seam."
  (let* ((base-ms dorg-dolt-retry-base-ms)
         (delay-ms (* base-ms (expt 2 attempt)))
         (jitter (- (random (1+ delay-ms)) (/ delay-ms 2)))
         (total-ms (max 10 (+ delay-ms jitter))))
    (/ total-ms 1000.0)))

(defmacro dorg-dolt-with-retry (&rest body)
  "Run BODY, retrying transient Dolt failures with backoff; return its value.
The ONE public cross-module retry seam (tracked-regression): an error accepted
by `dorg-dolt--retryable-error-p' with fewer than `dorg-dolt-max-retries'
failed attempts sleeps `dorg-dolt--retry-delay' and re-runs BODY FROM
SCRATCH — BODY must recompute any state a retry depends on, and a failed
attempt's partial results are never reused.  Any other error re-signals.

Success is tracked by an internal done flag set together with the result,
never by inspecting the value: a body that legitimately returns nil (e.g.
`dorg-restore-to-commit's already-at-that-state path) exits after exactly
one attempt."
  (declare (indent 0) (debug (body)))
  (let ((attempt (make-symbol "attempt"))
        (done (make-symbol "done"))
        (value (make-symbol "value"))
        (err (make-symbol "err")))
    `(let ((,attempt 0) (,done nil) (,value nil))
       (while (not ,done)
         (condition-case ,err
             (setq ,value (progn ,@body)
                   ,done t)
           (error
            (if (and (dorg-dolt--retryable-error-p ,err)
                     (< ,attempt dorg-dolt-max-retries))
                (progn
                  (sleep-for (dorg-dolt--retry-delay ,attempt))
                  (setq ,attempt (1+ ,attempt)))
              (signal (car ,err) (cdr ,err))))))
       ,value)))

(provide 'dorg-dolt)
;;; dorg-dolt.el ends here
