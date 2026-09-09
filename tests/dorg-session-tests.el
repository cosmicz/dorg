;;; dorg-session-tests.el --- Tests for the warm read-only session layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for `dorg-session': the read-only warm-session MVP for dorg-ql
;; Coverage:
;;
;;   - lifecycle: open/close, owns-buffer semantics, closed guard;
;;   - plain-org and Dolt-backed warm reads through `dorg-session-query';
;;   - warm reuse: a second identical query reuses the cache (instrumented
;;     non-vacuously by counting the underlying `dorg-ql-select' calls);
;;   - freshness: an out-of-process file edit and a Dolt HEAD change
;;     invalidate the session, and a stale cached result is never served;
;;   - host-safe projection: the default result shape carries no live
;;     marker (and a raw marker-carrying result is proven NOT print-safe);
;;   - strict transport composition: owner vs attach-only close, strict
;;     reads never CLI-fallback, and the default opens strict server.
;;
;; The plain-org and freshness tests use real temp files.  The strict
;; transport tests are fully mocked (no dolt binary, no sql-server) so the
;; lifecycle/routing contract is asserted directly; the Dolt query and
;; HEAD-change integration tests run only when dolt is installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg)
(require 'dorg-session)
(require 'dorg-ql)
(require 'dorg-registry)
(require 'dorg-backend)

(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el

;;; ─── Plain-org Fixtures ───────────────────────────────────────────────

(defmacro dorg-test-session--with-file (contents bindings &rest body)
  "Write CONTENTS to a temp file and run BODY with file/dir bound.
BINDINGS is a plist (:file FVAR :dir DVAR); either key may be omitted.
The org file is NOT pre-opened, so a session opened on the path owns the
buffer it creates."
  (declare (indent 2))
  (let ((file-var (or (plist-get bindings :file) (gensym "file")))
        (dir-var (or (plist-get bindings :dir) (gensym "dir"))))
    `(let ((dorg--registry nil)
           (,dir-var (make-temp-file "dorg-session-test-" t)))
       (unwind-protect
           (let ((,file-var (expand-file-name "doc.org" ,dir-var)))
             (with-temp-file ,file-var (insert ,contents))
             ,@body)
         (dolist (buf (buffer-list))
           (when (and (buffer-file-name buf)
                      (string-prefix-p (file-truename ,dir-var)
                                       (file-truename (buffer-file-name buf))))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (delete-directory ,dir-var t)))))

(defconst dorg-test-session--sample
  "* Alpha
:PROPERTIES:
:DORG_ID: 100
:CATEGORY: red
:END:
* Beta
:PROPERTIES:
:DORG_ID: 200
:END:
"
  "Two-heading plain-org sample used across session tests.")

;;; ─── Lifecycle ────────────────────────────────────────────────────────

(ert-deftest dorg-test-session-open-close-plain ()
  "Opening a plain-org file yields a live session; close is idempotent."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (should (dorg-session-p s))
      (should (eq 'plain-org (dorg-session-backend-type s)))
      (should (buffer-live-p (dorg-session-buffer s)))
      (should (dorg-session-freshness-token s))
      ;; Close kills the buffer it opened and marks the session closed.
      (let ((buf (dorg-session-buffer s)))
        (dorg-session-close s)
        (should (dorg-session-closed s))
        (should-not (buffer-live-p buf))
        ;; Idempotent.
        (should (null (dorg-session-close s)))))))

(ert-deftest dorg-test-session-open-buffer-not-owned ()
  "A session opened on a caller's buffer must not kill it on close."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((buf (find-file-noselect file)))
      (let ((s (dorg-session-open buf)))
        (should-not (dorg-session-owns-buffer s))
        (dorg-session-close s)
        ;; The caller's buffer survives.
        (should (buffer-live-p buf)))
      (kill-buffer buf))))

(ert-deftest dorg-test-session-closed-guard ()
  "Operations on a closed session signal `dorg-session-error'."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (dorg-session-close s)
      (should-error (dorg-session-query s '(id 100)) :type 'dorg-session-error)
      (should-error (dorg-session-stale-p s) :type 'dorg-session-error)
      (should-error (dorg-session-refresh s) :type 'dorg-session-error))))

(ert-deftest dorg-test-session-open-bad-source ()
  "A non-buffer, non-file source signals `dorg-session-error'."
  (should-error (dorg-session-open 42) :type 'dorg-session-error)
  (should-error (dorg-session-open "/nonexistent/dorg/doc.org")
                :type 'dorg-session-error))

;;; ─── Plain-org Warm Reads ─────────────────────────────────────────────

(ert-deftest dorg-test-session-query-plain ()
  "A plain-org session query returns the same headings as `dorg-ql-select'."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (let ((r (dorg-session-query s '(id 100))))
            (should (= 1 (length r)))
            (should (= 100 (plist-get (car r) :heading-id)))
            (should (equal "Alpha" (plist-get (car r) :title)))
            (should (equal '(("CATEGORY" . "red"))
                           (plist-get (car r) :properties))))
        (dorg-session-close s)))))

(ert-deftest dorg-test-session-query-host-safe-by-default ()
  "The default session result is host-safe: no live marker; round-trips.
Non-vacuous: the RAW result carries a live marker that is NOT print-safe,
so reading it back signals, while the projected default does not."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (let* ((safe (car (dorg-session-query s '(id 100))))
                 (raw  (car (dorg-session-query s '(id 100) :raw t))))
            ;; Raw plain-org result carries a live marker.
            (should (markerp (plist-get raw :marker)))
            ;; A live marker is not read/print-safe.
            (should-error (read (prin1-to-string raw)))
            ;; The default projection drops it and round-trips cleanly.
            (should (null (plist-get safe :marker)))
            (should (equal safe (read (prin1-to-string safe))))
            ;; Stable host-safe fields are preserved.
            (should (= 100 (plist-get safe :heading-id)))
            (should (equal "Alpha" (plist-get safe :title))))
        (dorg-session-close s)))))

(ert-deftest dorg-test-session-query-v2-host-safe ()
  "A composable v2 query also returns host-safe results through a session.
Plain-org v2 results carry live markers like atomic ones, so the default
projection must strip them here too."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (let* ((q '(and (id 100) (field level 1)))
                 (safe (car (dorg-session-query s q)))
                 (raw  (car (dorg-session-query s q :raw t))))
            (should (markerp (plist-get raw :marker)))
            (should (null (plist-get safe :marker)))
            (should (= 100 (plist-get safe :heading-id)))
            (should (equal safe (read (prin1-to-string safe)))))
        (dorg-session-close s)))))

(ert-deftest dorg-test-session-project-verify ()
  "`dorg-session-project' strips markers and `:verify' confirms safety."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (let* ((raw (dorg-session-query s '(id 100) :raw t))
                 (proj (dorg-session-project s raw :verify t)))
            (should (markerp (plist-get (car raw) :marker)))
            (should (null (plist-get (car proj) :marker)))
            (should (equal proj (read (prin1-to-string proj)))))
        (dorg-session-close s)))))

;;; ─── Warm Reuse (non-vacuous) ─────────────────────────────────────────

(ert-deftest dorg-test-session-warm-reuse-plain ()
  "A repeated identical query reuses the cache, not the adapter.
Non-vacuous: `dorg-ql-select' is wrapped with a counter; the second
identical query must NOT invoke it again while the source is unchanged."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file))
          (calls 0)
          (orig (symbol-function 'dorg-ql-select)))
      (unwind-protect
          (cl-letf (((symbol-function 'dorg-ql-select)
                     (lambda (&rest args) (cl-incf calls) (apply orig args))))
            (let ((r1 (dorg-session-query s '(id 100)))
                  (r2 (dorg-session-query s '(id 100))))
              (should (equal r1 r2))
              (should (= 1 calls))
              ;; A DIFFERENT query is a cache miss and does invoke the adapter.
              (dorg-session-query s '(id 200))
              (should (= 2 calls))))
        (dorg-session-close s)))))

;;; ─── Freshness: out-of-process edit ───────────────────────────────────

(ert-deftest dorg-test-session-stale-on-external-edit ()
  "An out-of-process file edit makes the session stale."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (progn
            (should-not (dorg-session-stale-p s))
            ;; Rewrite the file behind the buffer's back.
            (with-temp-file file
              (insert "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
                      "* Gamma\n:PROPERTIES:\n:DORG_ID: 300\n:END:\n"))
            (should (dorg-session-stale-p s)))
        (dorg-session-close s)))))

(ert-deftest dorg-test-session-refresh-serves-fresh-not-stale ()
  "A stale session refreshes before serving: the old cached result is
never returned after an out-of-process edit."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (progn
            ;; Warm the cache: id 200 (Beta) exists.
            (should (= 1 (length (dorg-session-query s '(id 200)))))
            ;; Externally rewrite so id 200 is gone and id 300 appears.
            (with-temp-file file
              (insert "* Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
                      "* Gamma\n:PROPERTIES:\n:DORG_ID: 300\n:END:\n"))
            ;; The stale (cached) id-200 hit must NOT be served.
            (should (null (dorg-session-query s '(id 200))))
            ;; The new content is visible.
            (let ((r (dorg-session-query s '(id 300))))
              (should (= 1 (length r)))
              (should (equal "Gamma" (plist-get (car r) :title)))))
        (dorg-session-close s)))))

(ert-deftest dorg-test-session-token-plain-shape ()
  "The plain-org freshness token records source identity and signatures."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (let ((s (dorg-session-open file)))
      (unwind-protect
          (let ((tok (dorg-session-freshness-token s)))
            (should (eq 'plain-org (plist-get tok :backend)))
            (should (equal (file-truename file) (plist-get tok :source)))
            (should (integerp (plist-get tok :size)))
            (should (plist-get tok :mtime))
            (should (stringp (plist-get tok :hash))))
        (dorg-session-close s)))))

;;; ─── Strict Transport Composition (mocked) ────────────────────────────
;;
;; These tests need no dolt binary: they stub a Dolt registration and the
;; server claim primitives so the session lifecycle/routing contract is
;; asserted directly.

(defmacro dorg-test-session--with-fake-dolt (bindings &rest body)
  "Run BODY with a temp org file and the registry resolvers stubbed to Dolt.
BINDINGS is a plist (:file FVAR :buffer BVAR :dolt-dir DDVAR).  Within
BODY, both the pure `dorg--classify-registration' classifier and the strict
`dorg--ensure-registration' resolver return a Dolt registration over DDVAR
with root id 42 (session open is a strict operation caller and resolves
through the ensure path), and `dorg-head-commit' is stubbed to a constant
so token computation issues no SQL.  The caller adds further mocks per test."
  (declare (indent 1))
  (let ((file-var (or (plist-get bindings :file) (gensym "file")))
        (buf-var (or (plist-get bindings :buffer) (gensym "buf")))
        (dd-var (or (plist-get bindings :dolt-dir) (gensym "dd"))))
    `(let* ((dorg--registry nil)
            (dir (make-temp-file "dorg-session-dolt-" t))
            (,file-var (expand-file-name "doc.org" dir))
            (,dd-var (file-name-as-directory (expand-file-name "dolt" dir)))
            (reg (make-dorg-registration
                  :backend (dorg-backend-dolt ,dd-var 42)
                  :file (file-truename ,file-var))))
       (with-temp-file ,file-var
         (insert "* Root\n:PROPERTIES:\n:DORG_ID: 42\n:END:\n"))
       (let ((,buf-var (find-file-noselect ,file-var)))
         (unwind-protect
             (cl-letf (((symbol-function 'dorg--classify-registration)
                        (lambda (&rest _) reg))
                       ((symbol-function 'dorg--ensure-registration)
                        (lambda (&rest _) reg))
                       ((symbol-function 'dorg-head-commit)
                        (lambda (&rest _) "deadbeef")))
               ,@body)
           (when (buffer-live-p ,buf-var)
             (with-current-buffer ,buf-var (set-buffer-modified-p nil))
             (kill-buffer ,buf-var))
           (delete-directory dir t))))))

(ert-deftest dorg-test-session-strict-open-owner-role ()
  "Strict open that starts the server records the `owner' role.
Owner = no server ran before open, one runs after, and this process owns
the claim.  Close stops the server it started."
  (dorg-test-session--with-fake-dolt (:buffer buf :dolt-dir dd)
    (let ((started nil) (stopped nil) (port-state nil))
      (cl-letf (((symbol-function 'dorg-dolt-server-port)
                 (lambda (&rest _) port-state))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (setq port-state 6000 started t) 6000))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "tok"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--stop)
                 (lambda (&rest _) (setq stopped t))))
        (let ((s (dorg-session-open buf :transport 'server)))
          (should started)
          (should (eq 'owner (dorg-session-server-role s)))
          (should (= 6000 (dorg-session-server-port s)))
          (dorg-session-close s)
          ;; Owner releases the server it owns.
          (should stopped))))))

(ert-deftest dorg-test-session-strict-open-attach-role ()
  "Strict open that attaches to a foreign owner records the `attach' role.
Attach = no local server before or after, and the claim is not owned
here.  Close must NOT stop the foreign owner's server."
  (dorg-test-session--with-fake-dolt (:buffer buf :dolt-dir dd)
    (let ((stopped nil))
      (cl-letf (((symbol-function 'dorg-dolt-server-port)
                 (lambda (&rest _) nil))    ; never a local server
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) 7000))   ; attached to foreign port
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "foreign"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--stop)
                 (lambda (&rest _) (setq stopped t))))
        (let ((s (dorg-session-open buf :transport 'server)))
          (should (eq 'attach (dorg-session-server-role s)))
          (should (= 7000 (dorg-session-server-port s)))
          (dorg-session-close s)
          ;; Attach-only released nothing it does not own.
          (should-not stopped))))))

(ert-deftest dorg-test-session-strict-preexisting-server-attaches ()
  "When this process already runs a server, strict open attaches (not owner).
A pre-existing local server means this session did not start it, so close
must not stop it."
  (dorg-test-session--with-fake-dolt (:buffer buf :dolt-dir dd)
    (let ((stopped nil))
      (cl-letf (((symbol-function 'dorg-dolt-server-port)
                 (lambda (&rest _) 5500))   ; server already running
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) 5500))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "tok"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p)
                 (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--stop)
                 (lambda (&rest _) (setq stopped t))))
        (let ((s (dorg-session-open buf :transport 'server)))
          (should (eq 'attach (dorg-session-server-role s)))
          (dorg-session-close s)
          (should-not stopped))))))

(ert-deftest dorg-test-session-strict-query-binds-transport ()
  "A strict session binds `dorg-sql-transport' to `server' during a query.
This is what forces SQL through the server with no CLI fallback; captured
at the underlying `dorg-ql-select' call."
  (dorg-test-session--with-fake-dolt (:buffer buf)
    (let ((seen nil))
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-strict) (lambda (&rest _) 7000))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "f"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil))
                ((symbol-function 'dorg-ql-select)
                 (lambda (&rest _) (setq seen dorg-sql-transport) nil)))
        (let ((s (dorg-session-open buf :transport 'server)))
          (dorg-session-query s '(id 42))
          (should (eq 'server seen))
          (dorg-session-close s))))))

(ert-deftest dorg-test-session-strict-query-never-cli ()
  "A strict session query routes SQL through mysql and never calls CLI.
Non-vacuous: `dorg-sql--cli-exec' is a sentinel that fails the test if
reached; the real transport resolver runs under the session's binding."
  (dorg-test-session--with-fake-dolt (:buffer buf)
    (let ((mysql-calls 0))
      (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-strict) (lambda (&rest _) 7000))
                ((symbol-function 'dorg-dolt-server-claim) (lambda (&rest _) '((:token . "f"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil))
                ;; Drive a real `dorg-sql' through the session's binding.
                ((symbol-function 'dorg-ql-select)
                 (lambda (&rest _) (dorg-sql "/fake/dolt/" "SELECT 1")))
                ((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                ((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (&rest _) 7000))
                ((symbol-function 'dorg-sql--get-connection) (lambda (&rest _) 'conn))
                ((symbol-function 'dorg-sql--mysql-exec)
                 (lambda (&rest _) (cl-incf mysql-calls) nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (ert-fail "strict session must not fall back to CLI"))))
        (let ((s (dorg-session-open buf :transport 'server)))
          (dorg-session-query s '(id 42))
          (should (= 1 mysql-calls))
          (dorg-session-close s))))))

;;; ─── Session Dolt Transport: Collapsed to Strict Server ───────────────
;;
;; The session transport selector is collapsed (tracked-case): a Dolt-backed
;; `dorg-session' is UNCONDITIONALLY strict `server'.  The
;; `dorg-session-default-dolt-transport' knob and its
;; `dorg-session--resolve-default-transport' resolver are removed — all
;; three former knob values (`prefer-server'/`server'/`inherit') already
;; resolved to `server', so the selector carried no behavioral variation.
;; The only surviving external input is an explicit `:transport', which
;; stays fail-closed through `dorg-session--validate-transport'
;; (server-only).

(ert-deftest dorg-test-session-transport-selector-removed ()
  "The collapsed selector and its resolver are GONE; the global stays server.
RED characterization for tracked-case: the defcustom knob and the 3-way
resolver must not exist after the collapse.  Non-vacuous — each assertion
fails against the pre-collapse code that still defines them."
  (should-not (boundp 'dorg-session-default-dolt-transport))
  (should-not (fboundp 'dorg-session--resolve-default-transport))
  ;; The global transport default is unchanged and remains server-only.
  (should (eq 'server (default-value 'dorg-sql-transport))))

(ert-deftest dorg-test-session-validate-transport-fail-closed ()
  "The surviving explicit-`:transport' validator is fail-closed, server-only.
Accepts strict `server'; rejects the removed `auto'/`cli' transports and
any unknown value with a typed `dorg-session-error' — never a silent
degrade to a default.  This is the retained fail-closed guard for stale
external `:transport' input."
  (should (eq 'server (dorg-session--validate-transport 'server)))
  (should-error (dorg-session--validate-transport 'auto)
                :type 'dorg-session-error)
  (should-error (dorg-session--validate-transport 'cli)
                :type 'dorg-session-error)
  (should-error (dorg-session--validate-transport 'bogus)
                :type 'dorg-session-error)
  (should-error (dorg-session--validate-transport nil)
                :type 'dorg-session-error))

(ert-deftest dorg-test-session-dolt-default-is-server ()
  "A Dolt session with no explicit :transport is unconditionally strict server.
Non-vacuous: with mysql present and `noninteractive' bound nil and no knob
in play, open ensures the managed server, freezes the session transport to
`server', records `owner', and a warm query never touches CLI."
  (dorg-test-session--with-fake-dolt (:buffer buf :dolt-dir dd)
    (let ((started nil) (port-state nil) (seen nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) port-state))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (setq port-state 6000 started t) 6000))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "tok"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil))
                ((symbol-function 'dorg-ql-select)
                 (lambda (&rest _) (setq seen dorg-sql-transport) nil))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (ert-fail "default server session must not use CLI"))))
        (let ((noninteractive nil))
          (let ((s (dorg-session-open buf)))   ; NO explicit :transport
            (should started)
            (should (eq 'server (dorg-session-transport s)))
            (should (eq 'owner (dorg-session-server-role s)))
            ;; The frozen policy reaches the query binding (no CLI fallback).
            (dorg-session-query s '(id 42))
            (should (eq 'server seen))
            (dorg-session-close s)))))))

(ert-deftest dorg-test-session-dolt-default-noninteractive-is-server ()
  "The Dolt default is strict server under `noninteractive' as well."
  (dorg-test-session--with-fake-dolt (:buffer buf)
    (let ((ensured 0) (port-state nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) port-state))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (cl-incf ensured) (setq port-state 6200) 6200))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "tok"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil)))
        (let ((noninteractive t))
          (let ((s (dorg-session-open buf)))
            (should (eq 'server (dorg-session-transport s)))
            (should (= 1 ensured))
            (dorg-session-close s)))))))

(ert-deftest dorg-test-session-dolt-removed-transport-values-fail-closed ()
  "The removed `:transport auto'/`cli' values stay fail-closed with a typed
`dorg-session-error' — there is no knob to route a Dolt session off strict
server.  (mysql.el is a HARD dependency, so a session never degrades on its
absence; that path was removed in tracked-case.)"
  (dorg-test-session--with-fake-dolt (:buffer buf)
    (cl-letf (((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil)))
      (let ((noninteractive nil))
        (should-error (dorg-session-open buf :transport 'auto)
                      :type 'dorg-session-error)
        (should-error (dorg-session-open buf :transport 'cli)
                      :type 'dorg-session-error)))))

(ert-deftest dorg-test-session-explicit-transport-server ()
  "An explicit `:transport server' opens strict server; `auto' fails closed.
There is no knob to override — the explicit value is validated server-only."
  (dorg-test-session--with-fake-dolt (:buffer buf)
    (let ((ensured 0) (port-state nil))
      (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) port-state))
                ((symbol-function 'dorg-dolt-server--ensure-strict)
                 (lambda (&rest _) (cl-incf ensured) (setq port-state 6100) 6100))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "tok"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) t))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil)))
        (let ((noninteractive nil))
          (should-error (dorg-session-open buf :transport 'auto)
                        :type 'dorg-session-error))
        (setq port-state nil)
        (let ((noninteractive t))
          (let ((s (dorg-session-open buf :transport 'server)))
            (should (eq 'server (dorg-session-transport s)))
            (should (= 1 ensured))
            (dorg-session-close s)))))))

(ert-deftest dorg-test-session-dolt-transport-plain-org-unaffected ()
  "The transport collapse never touches plain-org sessions.
A plain-org open keeps the global `dorg-sql-transport' and starts no
server."
  (dorg-test-session--with-file dorg-test-session--sample (:file file)
    (cl-letf (((symbol-function 'dorg-sql--mysql-available-p) (lambda (&rest _) t))
              ((symbol-function 'dorg-dolt-server--ensure-strict)
               (lambda (&rest _) (ert-fail "plain-org open must not ensure a server"))))
      (let ((noninteractive nil)
            (dorg-sql-transport 'server))
        (let ((s (dorg-session-open file)))
          (should (eq 'plain-org (dorg-session-backend-type s)))
          (should (eq 'server (dorg-session-transport s)))
          (should (null (dorg-session-server-role s)))
          (dorg-session-close s))))))

;;; ─── Dolt Integration (real dolt; skipped otherwise) ──────────────────

(ert-deftest dorg-test-session-query-dolt ()
  "A Dolt-backed session returns markerless host-safe results."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((dir (make-temp-file "dorg-session-dolt-" t))
           (dolt-dir (expand-file-name "dolt/" dir))
           (org-file (expand-file-name "doc.org" dir)))
      (unwind-protect
          (let ((dorg--registry nil) root-id)
            (dorg-init dolt-dir)
            (setq root-id (dorg-backend-root-id
                           (dorg-registration-backend
                            (dorg-bind-dolt org-file dolt-dir))))
            (dorg-sql-batch
             dolt-dir
             (dorg-test--parse-to-sql
              "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n* Other\n"
              root-id t))
            (dorg-commit dolt-dir "seed")
            ;; The materialized-view file must exist for path resolution; its
            ;; content is irrelevant since the query routes to SQL.
            (with-temp-file org-file (insert "#+DORG_DOLT_DIR: dolt/\n"))
            (let ((s (dorg-session-open org-file)))
              (unwind-protect
                  (let ((r (dorg-session-query s '(id 555))))
                    (should (= 1 (length r)))
                    (should (= 555 (plist-get (car r) :heading-id)))
                    (should (equal "Find me" (plist-get (car r) :title)))
                    ;; Host-safe: no marker, prints/reads cleanly.
                    (should (null (plist-get (car r) :marker)))
                    (should (equal r (read (prin1-to-string r))))
                    ;; The Dolt token folds in HEAD identity.
                    (should (plist-get (dorg-session-freshness-token s) :head)))
                (dorg-session-close s))))
        (delete-directory dir t)))))

(ert-deftest dorg-test-session-dolt-stale-on-head-change ()
  "A Dolt HEAD change invalidates the session and the stale result is not served.
Non-vacuous: warms the cache with the original title, commits a title
change (advancing HEAD), and proves the new title — not the cached one —
is served afterward."
  (if (not dorg-test--dolt-available)
      (ert-skip "dolt not installed")
    (let* ((dir (make-temp-file "dorg-session-dolt-head-" t))
           (dolt-dir (expand-file-name "dolt/" dir))
           (org-file (expand-file-name "doc.org" dir)))
      (unwind-protect
          (let ((dorg--registry nil) root-id)
            (dorg-init dolt-dir)
            (setq root-id (dorg-backend-root-id
                           (dorg-registration-backend
                            (dorg-bind-dolt org-file dolt-dir))))
            ;; Seed one heading (id 555, title "Find me").
            (dorg-sql-batch
             dolt-dir
             (dorg-test--parse-to-sql
              "* Find me\n:PROPERTIES:\n:DORG_ID: 555\n:END:\n" root-id t))
            (dorg-commit dolt-dir "seed")
            (with-temp-file org-file (insert "#+DORG_DOLT_DIR: dolt/\n"))
            (let ((s (dorg-session-open org-file)))
              (unwind-protect
                  (progn
                    ;; Warm the cache with the original title.
                    (should (equal "Find me"
                                   (plist-get (car (dorg-session-query s '(id 555)))
                                              :title)))
                    (should-not (dorg-session-stale-p s))
                    ;; Mutate the title and commit, advancing HEAD.
                    (dorg-sql dolt-dir
                              "UPDATE headings SET title = 'Renamed' WHERE id = 555 AND root_id = %d"
                              root-id)
                    (dorg-commit dolt-dir "rename 555")
                    (should (dorg-session-stale-p s))
                    ;; The stale cached "Find me" must NOT be served.
                    (let ((r (dorg-session-query s '(id 555))))
                      (should (= 1 (length r)))
                      (should (equal "Renamed" (plist-get (car r) :title)))))
                (dorg-session-close s))))
        (delete-directory dir t)))))

(ert-deftest dorg-test-session-dolt-fail-closed-on-unreadable-head ()
  "An unreadable Dolt HEAD fails closed: cached results are not served while
freshness cannot be proven.

Reproduces the navigator three-query scenario.  `dorg-head-commit' errors
after the cache is warm.  The first unreadable query is detected by both old
and fixed code (a readable HEAD token differs from an unreadable one).  The bug
was that a SECOND unreadable query compared two unreadable tokens as `equal'
\(`:head nil' == `:head nil'), so the session treated itself as fresh and served
the stale cache (select-calls=2 for 3 queries).  Failing closed, an unreadable
HEAD is never a comparable freshness token: the session stays stale and re-runs
`dorg-ql-select' each time.

Non-vacuous: the `dorg-session-stale-p' assertion after the second query and the
final call count both fail on the pre-fix code."
  (dorg-test-session--with-fake-dolt (:file file)
    (let ((calls 0)
          (head "h1"))
      (cl-letf (((symbol-function 'dorg-ql-select)
                 (lambda (&rest _)
                   (cl-incf calls)
                   (list (list :heading-id 42 :title "Root" :marker nil))))
                ((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) (or head (error "HEAD unreadable"))))
                ((symbol-function 'dorg-dolt-server-port) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--ensure-strict) (lambda (&rest _) 7000))
                ((symbol-function 'dorg-dolt-server-claim)
                 (lambda (&rest _) '((:token . "f"))))
                ((symbol-function 'dorg-dolt-server--claim-owned-here-p) (lambda (&rest _) nil))
                ((symbol-function 'dorg-dolt-server--stop) (lambda (&rest _) nil)))
        (let ((s (dorg-session-open file :transport 'server)))
          (unwind-protect
              (progn
                ;; Warm the cache under a readable, unchanged HEAD: the repeat
                ;; query is a cache hit (existing warm behavior stays intact).
                (dorg-session-query s '(id 42))
                (dorg-session-query s '(id 42))
                (let ((warm calls))
                  (should (= warm 1))
                  ;; HEAD becomes unreadable.  q2 re-runs in both old and fixed
                  ;; code because a readable token != an unreadable one.
                  (setq head nil)
                  (dorg-session-query s '(id 42))
                  (should (= (1+ warm) calls))
                  ;; FAIL-CLOSED CORE: with HEAD still unreadable, the session
                  ;; must NOT treat two unreadable tokens as fresh — it stays
                  ;; stale rather than serving the cache captured at q2.
                  (should (dorg-session-stale-p s))
                  ;; So q3 re-runs `dorg-ql-select' rather than serving stale.
                  (dorg-session-query s '(id 42))
                  (should (= (+ 2 warm) calls))))
            (dorg-session-close s)))))))

(provide 'dorg-session-tests)
;;; dorg-session-tests.el ends here
