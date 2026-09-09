;;; dorg-transport.el --- SQL transport policy (above dolt + dolt-server) -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Transport POLICY: server/claim-aware resolution of the SQL transport, and
;; the one-shot CLI live-owner refusal guard.  Both consult the server/claim
;; layer (`dorg-dolt-server'), so they belong ABOVE the low-level SQL
;; primitive (`dorg-dolt'), not inside it — otherwise the primitive reaches
;; UP into the server/claim machinery it should not own (tracked-case Fiction 3).
;;
;; This module requires BOTH `dorg-dolt' and `dorg-dolt-server', so its
;; claim/strict-port calls flow DOWNWARD (transport → server → dolt).  It
;; installs its resolver and CLI-preflight guard into the function-variable
;; seams `dorg-dolt' exposes (`dorg-sql--resolve-transport-fn' /
;; `dorg-sql--cli-preflight-fn') at load time.  `dorg-dolt' lazy-loads this
;; module on demand (`dorg-sql--ensure-transport-policy'), so a standalone
;; `(require \\='dorg-dolt)' user (e.g. `dorg-repo' lifecycle) is order-
;; independent.
;;
;; NOTE: this bead bounds Fiction 3 to the CLAIM-based transport policy.  The
;; connection/lease ENDPOINT-DISCOVERY coupling (`dorg-dolt' → dorg-dolt-server
;; `--strict-port' / `--dolt-server-info' / `--kill-process' / etc.) is
;; intentionally left in place; collapsing it is owned by the dead-declarations
;; / endpoint-ownership scope.

;;; Code:

(require 'dorg-dolt)
(require 'dorg-dolt-server)

;;; ─── Transport resolution ────────────────────────────────────────────

(defun dorg-sql--resolve-transport (dolt-dir &optional _query-class)
  "Resolve the SQL transport for DOLT-DIR per `dorg-sql-transport'.
Returns (mysql . PORT) to route through a sql-server via mysql.el, or
\(cli) only through the internal CLI seam.  Strict `server' mode
ensures/attaches a server and never returns (cli); any other public
policy value is rejected.  This is the single chokepoint where CLI
fallback is allowed or forbidden, so a whole operation routes through one
coherent transport.

An active `dorg-sql--with-pinned-transport' pin for DOLT-DIR short-circuits
this, returning the already-resolved cell so a rollback/reset reuses the
operation's original transport."
  (or (dorg-sql--pinned-transport-for dolt-dir)
      (pcase dorg-sql-transport
        ('server
         (cons 'mysql (dorg-dolt-server--strict-port dolt-dir)))
        (other
         (signal 'dorg-sql-transport-error
                 (list (format "dorg: `%S' is not a public SQL transport for %s; use `server' or the internal `dorg-sql--with-cli-seam' only for tests/recovery"
                               other dolt-dir)))))))

;;; ─── One-shot CLI Guard ───────────────────────────────────────────────

(defun dorg-sql--cli-live-owner-source (claim)
  "Return a human-readable discovery source for live owner CLAIM."
  (if (or (eq (alist-get :owner-kind claim) 'dolt)
          (eq (alist-get :origin claim) 'dolt-native))
      "Dolt-native .dolt/sql-server.info"
    "DOrg server.claim"))

(defun dorg-sql--cli-refuse-live-owner (target)
  "Refuse one-shot CLI for TARGET when a live same-host server owns it.
This guard is installed on the `dorg-sql--cli-preflight-fn' seam and runs
at the primitive `dorg-sql--cli-exec' boundary before `make-process', so
any accidental internal direct-open attempt is stopped even if a caller
bypassed public transport resolution.  It reaches the server/claim layer
DOWNWARD (this module requires `dorg-dolt-server'); the primitive itself
no longer names the claim machinery."
  (when dorg-sql--cli-conflict-check
    (let* ((repo (plist-get target :repo-dir))
           (claim (dorg-dolt-server--effective-claim repo)))
      (when (and claim
                 (dorg-dolt-server--claim-host-local-p claim)
                 (eq (dorg-dolt-server--claim-classify claim) 'live))
        (signal 'dorg-sql-transport-conflict
                (list (format
                       (concat "dorg: refusing one-shot `dolt sql' for %s: "
                               "live same-host sql-server owner found via %s; "
                               "use managed server transport or the supported "
                               "server-directed operator path")
                       repo
                       (dorg-sql--cli-live-owner-source claim))))))))

;;; ─── Seam installation ────────────────────────────────────────────────

;; Install the policy into `dorg-dolt' the moment both are present.  Since this
;; module hard-requires `dorg-dolt', the body runs immediately on load; the
;; `with-eval-after-load' form keeps it correct regardless of relative order.
(with-eval-after-load 'dorg-dolt
  (dorg-sql--install-transport-policy
   'dorg-transport
   #'dorg-sql--resolve-transport
   #'dorg-sql--cli-refuse-live-owner))

(provide 'dorg-transport)
;;; dorg-transport.el ends here
