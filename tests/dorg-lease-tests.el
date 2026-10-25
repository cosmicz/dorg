;;; dorg-lease-tests.el --- Endpoint-ownership + session-affine lease tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED->GREEN acceptance contract for tracked-case: bind endpoint validity to the
;; warm connection (Model A endpoint ownership) and the exclusive session-affine
;; lease (Model B) that tracked-case's GET_LOCK linearization consumes.
;;
;; Most tests are deterministic and fully mocked (no dolt binary, no sql-server):
;; the lease is a pure Elisp state machine (registry, object-identity ownership,
;; atomic reservation, typed-refusal guard) and Model A's compare-and-remove /
;; self-heal-once logic is pure control flow, so they are asserted directly with
;; a mocked mysql.el.  Each proves non-vacuity in its assertions/comment.  One
;; gated integration test drives the whole lease path on a real per-store
;; sql-server.
;;
;; AC map: A1 warm-reuse-no-discovery; A2/A5 selfheal-once (+ no CLI fallback,
;; A6); A3 compare-and-remove; A4 no-replay; B1 acquire-binds-exact-conn;
;; B2 reentrant-acquire-contends; B3/B4 ordinary-apis-lease-busy; B5/B6
;; mid-lease-disconnect-fails-closed; B7 acquire-selfheals + fail-closed;
;; B8 release-exact-ownership; B9 normal/broken release disposition;
;; B10 exact-session (lease-exec routes on lease.conn); B11
;; reentrant-callback-refused-not-multiplexed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-dolt)
(require 'dorg-dolt-server)
(require 'dorg-test-helpers)

(defvar dorg-test--dolt-available)      ; defined in dorg-dolt-tests.el

;;; ─── Mocked-store fixture ────────────────────────────────────────────────

(defmacro dorg-test-lease--env (dd &rest body)
  "Run BODY with DD bound to a fresh temp store dir and mysql.el fully mocked.
Fresh lease/connection/attached-port registries isolate each test.  Defaults:
`mysql-connect' -> \\='conn1, ping ok, `mysql-query' -> \\='result and
`dorg-sql--mysql-rows-to-alists' -> nil (so a SELECT round-trip yields nil rows
without needing a real `mysql-result'), strict-port -> 3307, db-name -> \"db\".
Individual tests `cl-letf' over these to script failures/identities."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dd (make-temp-file "dorg-lease-" t))
         (dorg-sql--leases (make-hash-table :test 'equal))
         (dorg-sql--connections (make-hash-table :test 'equal))
         (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'dorg-dolt--ensure-available) (lambda (&rest _) t))
                   ((symbol-function 'dorg-dolt-server--db-name) (lambda (_d) "db"))
                   ((symbol-function 'dorg-dolt-server--strict-port) (lambda (_d) 3307))
                   ((symbol-function 'mysql-ping) (lambda (_c) t))
                   ((symbol-function 'mysql-disconnect) (lambda (_c) nil))
                   ((symbol-function 'mysql-connect) (lambda (&rest _) 'conn1))
                   ((symbol-function 'mysql-query) (lambda (_c _s) 'result))
                   ((symbol-function 'dorg-sql--mysql-rows-to-alists) (lambda (_r) nil)))
           ,@body)
       (ignore-errors (delete-directory ,dd t)))))

;;; ─── Model A — endpoint ownership ───────────────────────────────────────

(ert-deftest dorg-test-lease-modelA-warm-reuse-no-discovery ()
  "A1: a warm store served through the PUBLIC `dorg-sql' AND `dorg-sql-batch'
entry points does no strict-port discovery or lifecycle lock — the port is
resolved lazily only on a cache miss, never on the warm hot path."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd))
          (strict 0) (attach 0))
      ;; an established (identity-validated) warm entry
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid nil)
                                              :validated t)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-dolt-server--strict-port)
                 (lambda (_d) (cl-incf strict) 3307))
                ((symbol-function 'dorg-dolt-server--attach-only)
                 (lambda (_d) (cl-incf attach) 3307)))
        ;; through the public read paths, not the get-connection helper
        (dorg-sql dd "SELECT 1")
        (dorg-sql dd "SELECT 2")
        (dorg-sql-batch dd (list "SELECT 3"))
        ;; Non-vacuity: pre-fix `dorg-sql' / `dorg-sql-batch' resolved strict-port
        ;; before the warm get-connection, so `strict' would be > 0 (one per
        ;; entry point), not 0.
        (should (= strict 0))
        (should (= attach 0))))))

(ert-deftest dorg-test-10z7y-generation-fingerprint-revalidates-each-warm-read ()
  "A C1 member drift evicts the warm entry; an equal fingerprint does not.
The fingerprint seam is deliberately independent of SQL/HEAD: this control
proves validation observes the source generation without product queries."
  (dorg-test-lease--env dd
    (let* ((key (dorg-dolt-server--canonical-dir dd))
           (old '(:directory (:device 1 :inode 2)
                 :native (:pid 10 :port 3307) :source-hash "old"))
           (current old)
           (opened 0)
           (head-calls 0))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid nil)
                                              :generation old :validated t)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-sql--generation-fingerprint)
                 (lambda (_d) current))
                ((symbol-function 'dorg-head-commit)
                 (lambda (&rest _) (cl-incf head-calls) "forbidden"))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh)))
        (should (eq (dorg-sql--get-connection dd) 'warm))
        (setq current '(:directory (:device 1 :inode 2)
                        :native (:pid 10 :port 3307) :source-hash "new"))
        (should (eq (dorg-sql--get-connection dd) 'fresh))
        (should (= opened 1))
        (should (= head-calls 0))))))

(ert-deftest dorg-test-10z7y-generation-drift-refuses-stale-lease-write ()
  "C2: a plan leased on A cannot issue SQL after B becomes visible.
The assertion is before `mysql-query': removing the generation check would make
the batch reach the replacement connection and fail this exact no-write row."
  (dorg-test-lease--env dd
    (let ((current '(:directory (:device 1 :inode 2)
                     :native (:pid 10 :port 3307) :source-hash "A"))
          (queries 0))
      (cl-letf (((symbol-function 'dorg-sql--generation-fingerprint)
                 (lambda (_d) current))
                ((symbol-function 'mysql-query)
                 (lambda (&rest _) (cl-incf queries) 'result)))
        (let ((lease (dorg-sql--acquire-lease dd)))
          (unwind-protect
              (progn
                ;; B replaces A after the mutation was planned/leased.
                (setq current '(:directory (:device 3 :inode 4)
                                :native (:pid 11 :port 3308) :source-hash "B"))
                (should-error (dorg-sql--lease-batch
                               lease '("INSERT INTO headings VALUES (1)") "stale")
                              :type 'dorg-sql-lease-generation-drift)
                (should (= queries 0)))
            (dorg-sql--release-lease lease t)))))))

(ert-deftest dorg-test-10z7y-legacy-lease-stamps-generation-on-first-use ()
  "G2: a pre-generation active lease remains usable until a later drift."
  (dorg-test-lease--env dd
    (let* ((key (dorg-dolt-server--canonical-dir dd))
           (generation '(:directory (:device 1 :inode 2)
                         :native (:pid 10 :port 3307) :source-hash "A"))
           (lease (dorg-sql--make-lease :dolt-dir dd :conn 'legacy
                                        :phase :active :owner 'legacy)))
      (puthash key lease dorg-sql--leases)
      (cl-letf (((symbol-function 'dorg-sql--generation-fingerprint)
                 (lambda (_d) generation)))
        (dorg-sql--lease-assert-current-generation lease)
        (should (equal generation (dorg-sql--lease-generation lease)))))))

(ert-deftest dorg-test-10z7y-generation-members-reopen-and-hot-reuse ()
  "T3: each fingerprint member reopens once; an unchanged warm read spawns none."
  (dorg-test-lease--env dd
    (let* ((key (dorg-dolt-server--canonical-dir dd))
           (base '(:directory (:device 1 :inode 2)
                  :native (:pid 10 :port 3307) :source-hash "A"))
           (current base)
           (opened 0)
           (strict 0))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid nil)
                                              :generation base :validated t)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-sql--generation-fingerprint)
                 (lambda (_d) current))
                ((symbol-function 'dorg-dolt-server--strict-port)
                 (lambda (_d) (cl-incf strict) 3307))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (intern (format "fresh-%d" (cl-incf opened)))))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (error "hot generation check spawned")))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (error "hot generation check spawned")))
                ((symbol-function 'process-file)
                 (lambda (&rest _) (error "hot generation check spawned"))))
        ;; No strict resolution or process on an unchanged public warm boundary.
        (should (eq (dorg-sql--get-connection dd) 'warm))
        (should (= strict 0))
        (dolist (next
                 (list
                  '(:directory (:device 3 :inode 4)
                    :native (:pid 10 :port 3307) :source-hash "A")
                  '(:directory (:device 3 :inode 4)
                    :native (:pid 11 :port 3308) :source-hash "A")
                  '(:directory (:device 3 :inode 4)
                    :native (:pid 11 :port 3308) :source-hash "B")))
          (setq current next)
          (dorg-sql--get-connection dd))
        (should (= opened 3))
        (should (= strict 3))))))

(ert-deftest dorg-test-10z7y-generation-hash-reads-only-bound-buffer ()
  "A loaded matching source contributes its preamble hash without HEAD SQL."
  (dorg-test-lease--env dd
    (let* ((file (make-temp-file "dorg-10z7y-" nil ".org"))
           (buffer (find-file-noselect file))
           (dorg--registry nil) (canon (file-name-as-directory dd)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "#+DORG_HASH: alpha\n* Root\n") (org-mode))
            (setq dorg--registry (list (cons (file-truename file)
                                            (make-dorg-registration :backend (dorg-backend-dolt canon 1) :file file))))
            (cl-letf (((symbol-function 'dorg-dolt-server--canonical-dir) (lambda (_dir) canon))
                      ((symbol-function 'dorg-head-commit) (lambda (&rest _) (error "HEAD used"))))
              (should (equal "alpha" (dorg-sql--bound-source-hash dd)))))
        (kill-buffer buffer) (delete-file file)))))

(ert-deftest dorg-test-lease-modelA-uuid-drift-reopens ()
  "A2/A5: an entry whose captured sql-server UUID no longer matches
`sql-server.info' (a silent same-port instance swap) is evicted and reopened on
first-use identity validation."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (opened 0))
      (puthash key (dorg-sql--make-conn-entry :conn 'stale :port 3307
                                              :identity (list :db "db" :uuid "old")
                                              :validated nil)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (_d) (list :pid 1 :port 3307 :uuid "new")))
                ((symbol-function 'dorg-dolt-server--strict-port) (lambda (_d) 3307))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh)))
        (should (eq (dorg-sql--get-connection dd) 'fresh))
        (should (= opened 1))
        ;; Non-vacuity: without UUID identity in the entry the stale conn would
        ;; be reused (opened 0) despite the instance swap.
        (should (eq (dorg-sql--conn-entry-conn (gethash key dorg-sql--connections))
                    'fresh))))))

(ert-deftest dorg-test-lease-modelA-uuid-match-reuses ()
  "First-use identity validation with a MATCHING UUID marks the entry validated
and reuses it (no reopen)."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (opened 0))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid "same")
                                              :validated nil)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (_d) (list :pid 1 :port 3307 :uuid "same")))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh)))
        (should (eq (dorg-sql--get-connection dd) 'warm))
        (should (= opened 0))
        (should (dorg-sql--conn-entry-validated (gethash key dorg-sql--connections)))))))

(ert-deftest dorg-test-lease-identity-db-drift-invalid ()
  "Identity validation compares BOTH halves: a captured `:db' that no longer
matches the current db-name is a positive mismatch (store re-point) EVEN when the
UUID still matches — the entry is not identity-valid."
  (dorg-test-lease--env dd
    (let ((entry (dorg-sql--make-conn-entry
                  :conn 'warm :port 3307
                  :identity (list :db "db-A" :uuid "u"))))
      (cl-letf (((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (_d) (list :pid 1 :port 3307 :uuid "u"))))
        ;; db drift (captured db-A, current db-B) + matching UUID -> INVALID
        (cl-letf (((symbol-function 'dorg-dolt-server--db-name) (lambda (_d) "db-B")))
          (should-not (dorg-sql--entry-identity-valid-p entry dd)))
        ;; Non-vacuity: same UUID with a MATCHING db-name -> valid.
        (cl-letf (((symbol-function 'dorg-dolt-server--db-name) (lambda (_d) "db-A")))
          (should (dorg-sql--entry-identity-valid-p entry dd)))))))

(ert-deftest dorg-test-lease-modelA-selfheal-once ()
  "A2/A5/A6: a stale-port connect failure evicts the memo, rediscovers, and
reconnects EXACTLY once over mysql (never CLI)."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd))
          (stale 3307) (fresh 4400)
          (connect-ports nil) (cli-calls 0))
      (puthash key stale dorg-dolt-server--attached-ports)
      (cl-letf (((symbol-function 'dorg-dolt-server--strict-port) (lambda (_d) fresh))
                ((symbol-function 'dorg-sql--cli-exec)
                 (lambda (&rest _) (cl-incf cli-calls) nil))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest args)
                   (let ((p (plist-get args :port)))
                     (push p connect-ports)
                     (if (eql p stale)
                         (signal 'mysql-connection-error (list "stale port down"))
                       'conn-fresh)))))
        (let ((entry (dorg-sql--open-entry key dd stale)))
          (should (eq (dorg-sql--conn-entry-conn entry) 'conn-fresh))
          ;; exactly one retry: stale then the rediscovered fresh port
          (should (equal (reverse connect-ports) (list stale fresh)))
          (should (= cli-calls 0))                 ; A6: no CLI fallback
          ;; A2: the exact stale attached-port memo was evicted
          (should-not (gethash key dorg-dolt-server--attached-ports)))))))

(ert-deftest dorg-test-lease-modelA-selfheal-second-failure-propagates ()
  "A2: self-heal reconnects ONCE — a second failure propagates, no loop."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (n 0))
      (cl-letf (((symbol-function 'dorg-dolt-server--strict-port) (lambda (_d) 4400))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf n)
                   (signal 'mysql-connection-error (list "down")))))
        (should-error (dorg-sql--open-entry key dd 3307) :type 'mysql-connection-error)
        (should (= n 2))))))                       ; tried twice only

(ert-deftest dorg-test-lease-modelA-compare-and-remove ()
  "A3: eviction is compare-and-remove — an older failure cannot erase a newer entry."
  (dorg-test-lease--env dd
    (let* ((key (dorg-dolt-server--canonical-dir dd))
           (e1 (dorg-sql--make-conn-entry :conn 'c1 :port 3307
                                          :identity (list :db "db")))
           (e2 (dorg-sql--make-conn-entry :conn 'c2 :port 3308
                                          :identity (list :db "db"))))
      ;; newer e2 is live; evicting the older e1 must NOT remove e2
      (puthash key e2 dorg-sql--connections)
      (dorg-sql--evict-conn-entry key e1)
      (should (eq (gethash key dorg-sql--connections) e2))
      ;; evicting the live entry removes it
      (dorg-sql--evict-conn-entry key e2)
      (should-not (gethash key dorg-sql--connections)))))

;;; ─── Model B — exclusive session-affine lease ───────────────────────────

(ert-deftest dorg-test-lease-acquire-binds-exact-conn ()
  "B1/B10: a lease binds one exact conn; lease-exec routes on THAT conn."
  (dorg-test-lease--env dd
    (cl-letf (((symbol-function 'mysql-connect) (lambda (&rest _) 'lease-conn)))
      (let ((lease (dorg-sql--acquire-lease dd)) (seen nil))
        (should (eq (dorg-sql--lease-conn lease) 'lease-conn))
        (should (eq (dorg-sql--lease-phase lease) :active))
        (cl-letf (((symbol-function 'mysql-query)
                   (lambda (conn _s) (setq seen conn) 'result)))
          (dorg-sql--lease-exec lease "SELECT 1"))
        (should (eq seen 'lease-conn))
        ;; Non-vacuity: a lease-exec that re-resolved the conn would not be `eq'.
        (dorg-sql--release-lease lease)))))

(ert-deftest dorg-test-lease-reserved-checkout-leases-existing-warm-conn ()
  "Acquire LEASES an existing healthy warm connection (checks it out of the
pool) instead of opening a fresh one and leaking the old — no reopen, no
disconnect of the leased conn."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (opened 0) (disc nil))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid nil)
                                              :validated t)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh))
                ((symbol-function 'mysql-disconnect) (lambda (c) (push c disc))))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should (eq (dorg-sql--lease-conn l) 'warm))
          (should (= opened 0))                     ; leased, did not reopen
          (should-not disc)                         ; warm conn NOT leaked/disconnected
          (should-not (gethash key dorg-sql--connections)) ; checked out of pool
          (dorg-sql--release-lease l))
        ;; normal release RESTORES the same warm conn to the pool (E1 reusable)
        (should (eq (dorg-sql--conn-entry-conn (gethash key dorg-sql--connections))
                    'warm))
        (should (= opened 0))))))                    ; never opened a fresh conn

(ert-deftest dorg-test-lease-reserved-checkout-replaces-stale-warm-conn ()
  "Acquire disconnects a STALE warm entry (no leak) and opens a fresh leased
session when the existing pooled conn is not healthy."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (disc nil))
      (puthash key (dorg-sql--make-conn-entry :conn 'stale :port 3307
                                              :identity (list :db "db" :uuid nil)
                                              :validated t)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'mysql-ping)
                 (lambda (c) (if (eq c 'stale) (error "dead") t)))
                ((symbol-function 'mysql-connect) (lambda (&rest _) 'fresh))
                ((symbol-function 'mysql-disconnect) (lambda (c) (push c disc))))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should (eq (dorg-sql--lease-conn l) 'fresh))
          (should (memq 'stale disc))               ; stale conn disconnected, not leaked
          (dorg-sql--release-lease l))))))

(ert-deftest dorg-test-lease-acquire-uuid-drift-reconnects ()
  "Acquisition must obtain a VALIDATED Model-A session: a ping-healthy but
NOT-yet-`:validated' warm entry whose captured UUID has drifted (a silent
same-port instance swap) is NOT leased — it is evicted and a fresh session is
opened.  Otherwise the lease binds a stale-identity connection."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (opened 0) (disc nil))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid "old")
                                              :validated nil)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (_d) (list :pid 1 :port 3307 :uuid "new")))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh))
                ((symbol-function 'mysql-disconnect) (lambda (c) (push c disc))))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should (eq (dorg-sql--lease-conn l) 'fresh))  ; NOT the stale-identity 'warm
          (should (= opened 1))                          ; reconnected once
          (should (memq 'warm disc))                     ; stale-identity conn evicted
          ;; Non-vacuity: without checkout-time identity validation this would
          ;; lease 'warm with opened=0.
          (dorg-sql--release-lease l))))))

(ert-deftest dorg-test-lease-acquire-uuid-match-leases-warm ()
  "Acquisition LEASES a ping-healthy warm entry whose captured UUID still matches
the live instance (first-use identity validation passes at checkout), opening no
fresh connection."
  (dorg-test-lease--env dd
    (let ((key (dorg-dolt-server--canonical-dir dd)) (opened 0))
      (puthash key (dorg-sql--make-conn-entry :conn 'warm :port 3307
                                              :identity (list :db "db" :uuid "same")
                                              :validated nil)
               dorg-sql--connections)
      (cl-letf (((symbol-function 'dorg-dolt-server--dolt-server-info)
                 (lambda (_d) (list :pid 1 :port 3307 :uuid "same")))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf opened) 'fresh)))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should (eq (dorg-sql--lease-conn l) 'warm))   ; leased the warm conn
          (should (= opened 0))                          ; no reopen
          (dorg-sql--release-lease l))))))

(ert-deftest dorg-test-lease-second-acquire-contends ()
  "B2: a second acquire while one is held signals lease-contended."
  (dorg-test-lease--env dd
    (let ((l (dorg-sql--acquire-lease dd)))
      (should-error (dorg-sql--acquire-lease dd) :type 'dorg-sql-lease-contended)
      (dorg-sql--release-lease l))))

(ert-deftest dorg-test-lease-reentrant-acquire-contends-at-validation ()
  "B2: a re-entrant caller arriving DURING caller-1's yielding validation
contends, because the reservation is installed synchronously before the yield."
  (dorg-test-lease--env dd
    (let* ((canon (dorg-dolt-server--canonical-dir dd))
           (nested nil)
           (orig (symbol-function 'dorg-sql--open-entry)))
      (cl-letf (((symbol-function 'dorg-sql--open-entry)
                 (lambda (k d p)
                   ;; simulate a re-entrant callback during the (yielding) checkout
                   (unless nested
                     (setq nested
                           (condition-case e
                               (progn (dorg-sql--acquire-lease d) 'NO-ERROR)
                             (dorg-sql-lease-contended (car e)))))
                   (funcall orig k d p))))
        (let ((lease (dorg-sql--acquire-lease dd)))
          ;; Non-vacuity: install the reservation AFTER validation and the nested
          ;; acquire would pass the empty check ('NO-ERROR) and install a 2nd lease.
          (should (eq nested 'dorg-sql-lease-contended))
          (should (eq (gethash canon dorg-sql--leases) lease))
          (dorg-sql--release-lease lease))))))

(ert-deftest dorg-test-lease-ordinary-apis-lease-busy ()
  "B4: ordinary dorg-sql/dorg-sql-batch/get-connection are refused for a leased
store; only the explicit seam reaches the leased session."
  (dorg-test-lease--env dd
    (let ((lease (dorg-sql--acquire-lease dd)) (seen nil))
      (should-error (dorg-sql dd "SELECT 1") :type 'dorg-sql-lease-busy)
      (should-error (dorg-sql-batch dd '("SELECT 1")) :type 'dorg-sql-lease-busy)
      (should-error (dorg-sql--get-connection dd 3307) :type 'dorg-sql-lease-busy)
      (cl-letf (((symbol-function 'mysql-query) (lambda (c _s) (setq seen c) 'result)))
        (dorg-sql--lease-exec lease "SELECT 1"))
      (should (eq seen (dorg-sql--lease-conn lease)))
      (dorg-sql--release-lease lease))))

(ert-deftest dorg-test-lease-reentrant-callback-refused-not-multiplexed ()
  "B11: a re-entrant callback firing while the leased statement yields, issuing
ordinary same-store SQL, is refused (lease-busy) and NEVER runs on lease.conn."
  (dorg-test-lease--env dd
    (let ((lease (dorg-sql--acquire-lease dd)) (cb nil) (queries nil))
      (cl-letf (((symbol-function 'mysql-query)
                 (lambda (conn sql)
                   (push (cons conn sql) queries)
                   (when (and (not cb) (equal sql "LEASE-STMT"))
                     (setq cb (condition-case e
                                  (progn (dorg-sql dd "CALLBACK-STMT") 'NO-ERROR)
                                (dorg-sql-lease-busy (car e)))))
                   'result)))
        (dorg-sql--lease-exec lease "LEASE-STMT")
        (should (eq cb 'dorg-sql-lease-busy))
        ;; the callback's ordinary SQL never executed on ANY connection
        (should-not (cl-find "CALLBACK-STMT" queries :key #'cdr :test #'equal))
        ;; the holder's statement ran on the exact leased conn
        (should (cl-find (cons (dorg-sql--lease-conn lease) "LEASE-STMT")
                         queries :test #'equal))
        ;; Non-vacuity: an implicit dynamic-var holder-bypass would let the
        ;; inherited binding multiplex lease.conn — cb would be 'NO-ERROR and
        ;; "CALLBACK-STMT" would appear in `queries'.
        (dorg-sql--release-lease lease)))))

(ert-deftest dorg-test-lease-release-exact-ownership ()
  "B8: release validates OBJECT identity; a copied/stale/absent lease errors and
cannot clear a newer installed lease."
  (dorg-test-lease--env dd
    (let* ((canon (dorg-dolt-server--canonical-dir dd))
           (l1 (dorg-sql--acquire-lease dd))
           (forged (dorg-sql--make-lease :dolt-dir dd
                                         :conn (dorg-sql--lease-conn l1)
                                         :port (dorg-sql--lease-port l1)
                                         :phase :active
                                         :owner (dorg-sql--lease-owner l1))))
      ;; (a) a copy with identical fields (even the same owner symbol) is not eq
      (should-error (dorg-sql--release-lease forged)
                    :type 'dorg-sql-lease-ownership-error)
      (should (eq (gethash canon dorg-sql--leases) l1))
      (dorg-sql--release-lease l1)
      (should-not (gethash canon dorg-sql--leases))
      ;; (c) empty slot
      (should-error (dorg-sql--release-lease l1)
                    :type 'dorg-sql-lease-ownership-error)
      ;; (b) stale-vs-newer: a newer lease survives a stale release
      (let ((l2 (dorg-sql--acquire-lease dd)))
        (should-error (dorg-sql--release-lease l1)
                      :type 'dorg-sql-lease-ownership-error)
        (should (eq (gethash canon dorg-sql--leases) l2))
        (dorg-sql--release-lease l2)))))

(ert-deftest dorg-test-lease-normal-release-restores-broken-discards ()
  "B9: a normal release returns the healthy conn to the warm pool; a broken
release disconnects and discards it (never warm)."
  (dorg-test-lease--env dd
    (let* ((canon (dorg-dolt-server--canonical-dir dd)) (disconnected nil))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c disconnected))))
        (let* ((l1 (dorg-sql--acquire-lease dd)) (c1 (dorg-sql--lease-conn l1)))
          (dorg-sql--release-lease l1)
          (should (eq (dorg-sql--conn-entry-conn (gethash canon dorg-sql--connections))
                      c1))
          (should-not disconnected))
        (let* ((l2 (dorg-sql--acquire-lease dd)) (c2 (dorg-sql--lease-conn l2)))
          (dorg-sql--release-lease l2 t)
          (should-not (gethash canon dorg-sql--connections))
          (should (memq c2 disconnected)))))))

(ert-deftest dorg-test-lease-lease-seam-rejects-non-owner ()
  "B8: lease-exec/lease-batch on a released (non-owning) lease error."
  (dorg-test-lease--env dd
    (let ((l (dorg-sql--acquire-lease dd)))
      (dorg-sql--release-lease l)
      (should-error (dorg-sql--lease-exec l "SELECT 1")
                    :type 'dorg-sql-lease-ownership-error)
      (should-error (dorg-sql--lease-batch l '("X"))
                    :type 'dorg-sql-lease-ownership-error))))

(ert-deftest dorg-test-lease-acquire-selfheals-before-region ()
  "B7(a): acquisition self-heals (evict + rediscover + reconnect once) BEFORE the
lease region begins, yielding a working lease on the rediscovered port."
  (dorg-test-lease--env dd
    (let ((ports (list 9999 3307)))       ; first (stale) then rediscovered
      (cl-letf (((symbol-function 'dorg-dolt-server--strict-port)
                 (lambda (_d) (pop ports)))
                ((symbol-function 'mysql-connect)
                 (lambda (&rest args)
                   (if (eql (plist-get args :port) 3307)
                       'lc
                     (signal 'mysql-connection-error (list "stale"))))))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should (eq (dorg-sql--lease-conn l) 'lc))
          (should (eq (dorg-sql--lease-port l) 3307))
          (dorg-sql--release-lease l))))))

(ert-deftest dorg-test-lease-mid-lease-disconnect-fails-closed ()
  "B5/B6/B7(b): a transport failure after mint aborts fail-closed
\(lease-broken), issues NO replacement-session SQL, and discards the session."
  (dorg-test-lease--env dd
    (let ((connects 0) (disconnected nil))
      (cl-letf (((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf connects) 'lease-conn))
                ((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c disconnected)))
                ((symbol-function 'mysql-query)
                 (lambda (_c _s) (signal 'mysql-connection-error (list "server died")))))
        (should-error
         (dorg-sql--with-lease (l dd)
           (dorg-sql--lease-exec l "SELECT 1"))
         :type 'dorg-sql-lease-broken)
        (should (= connects 1))                ; no replacement session opened
        (should (memq 'lease-conn disconnected))
        (should-not (gethash (dorg-dolt-server--canonical-dir dd)
                             dorg-sql--connections))))))

(ert-deftest dorg-test-0l5rj-lease-broken-carries-only-closed-cause-class ()
  "Broken leases retain a closed cause class without copying error prose."
  (let ((lease (dorg-sql--make-lease :dolt-dir "/private/store"
                                      :conn 'fixture-connection
                                      :port 3307
                                      :phase :active))
        (cases '(((mysql-timeout "private timeout prose") . mysql-timeout)
                 ((mysql-connection-error "private connection prose")
                  . mysql-connection-error)
                 ((mysql-protocol-error "private protocol prose")
                  . mysql-protocol-error)
                 ((error "private dead socket prose") . dead-connection)
                 ((user-error "private unknown prose") . other))))
    (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p)
               (lambda (_lease) nil)))
      (dolist (case cases)
        (let* ((source (car case))
               (expected (cdr case))
               (err (condition-case caught
                        (dorg-sql--raise-lease-broken lease source)
                      (dorg-sql-lease-broken caught)))
               (fields (cddr err)))
          (should (eq expected (plist-get fields :cause-class)))
          (should (memq (plist-get fields :cause-class)
                        '(mysql-timeout mysql-connection-error
                          mysql-protocol-error dead-connection other)))
          (should-not (string-match-p "private" (prin1-to-string fields))))))))

(ert-deftest dorg-test-lease-no-replay-after-first-statement ()
  "A4: a batch whose connection drops mid-run aborts without replaying any
statement or opening a replacement session."
  (dorg-test-lease--env dd
    (let ((sent nil) (connects 0))
      (cl-letf (((symbol-function 'mysql-connect)
                 (lambda (&rest _) (cl-incf connects) 'lc))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (push sql sent)
                   (when (equal sql "S2")
                     (signal 'mysql-connection-error (list "drop")))
                   'result)))
        (let ((l (dorg-sql--acquire-lease dd)))
          (should-error (dorg-sql--lease-batch l '("S1" "S2" "S3") "msg")
                        :type 'dorg-sql-lease-broken)
          (should (= (cl-count "S1" sent :test #'equal) 1))
          (should (= (cl-count "S2" sent :test #'equal) 1))
          (should (= (cl-count "S3" sent :test #'equal) 0))  ; never reached
          (should (= connects 1))                            ; no reconnect/replay
          (dorg-sql--release-lease l t))))))

(ert-deftest dorg-test-lease-sql-error-is-not-a-broken-lease ()
  "A SQL (`mysql-query-error') failure propagates as-is and does NOT masquerade
as a broken lease (only transport-level errors fail closed).

`mysql-query-error' is a sibling of (not a) `mysql-connection-error', so
`dorg-sql--with-broken-guard' routes it through the GENERIC `error' clause,
whose disposition turns on `dorg-sql--lease-conn-live-p': a LIVE session
re-signals the fault unchanged, a DEAD one converts to `dorg-sql-lease-broken'.
The fully-mocked fixture conn (`\\='conn1) is a bare symbol with no real
process, so the transport-liveness probe — which calls `mysql-conn-process' on
it — is stubbed to report LIVE (matching the real contract: the connection is
healthy, the SQL is what failed).  Non-vacuous: were the guard to fail closed on
a live-conn SQL fault, `should-error' would see `dorg-sql-lease-broken', not
`mysql-query-error'."
  (dorg-test-lease--env dd
    (let ((l (dorg-sql--acquire-lease dd)))
      (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p) (lambda (_l) t))
                ((symbol-function 'mysql-query)
                 (lambda (_c _s) (signal 'mysql-query-error (list "bad SQL")))))
        (should-error (dorg-sql--lease-exec l "SELECT bogus")
                      :type 'mysql-query-error))
      (dorg-sql--release-lease l))))

;;; ─── Integration: whole lease path on a real per-store sql-server ───────

(defun dorg-test-10z7y--start-raw-server (dolt-dir tag)
  "Start one test-owned raw sql-server for DOLT-DIR and return (PROCESS . PORT)."
  (let* ((port (dorg-dolt-server--free-port))
         (log (make-temp-file (format "dorg-10z7y-%s-" tag) nil ".log"))
         (proc (dorg-dolt-server--spawn-logged
                (format "dorg-10z7y-%s" tag)
                (list "sql-server" "--host" "127.0.0.1"
                      "--port" (number-to-string port) "--data-dir" dolt-dir)
                log)))
    (set-process-query-on-exit-flag proc nil)
    (dorg-dolt-server--wait-ready port 30 proc)
    (unless (dorg-dolt-server--port-healthy-p port 5)
      (dorg-dolt-server--kill-process proc)
      (error "10z7y raw server did not become healthy: %s" log))
    (cons proc port)))

(defun dorg-test-10z7y--stop-raw-server (server)
  "Stop test-owned raw SERVER, a (PROCESS . PORT) pair."
  (when (and server (process-live-p (car server)))
    (dorg-dolt-server--kill-process (car server))))

(ert-deftest dorg-test-10z7y-store-swap-reconnects-warm-client ()
  "C2/T2: a warm reader follows raw A->B and B->A store swaps.
A remains live while B is started outside DOrg's launcher, reproducing the
incident shape.  The separate lease control proves an A-planned write refuses
before SQL after B is visible."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — strict server path needs it")))
  (let ((root (dorg-test--make-owned-temp-dir "dorg-10z7y-" "store-swap"))
        (dorg-dolt-server--attached-ports (make-hash-table :test 'equal))
        (dorg-sql--connections (make-hash-table :test 'equal))
        (dorg-sql--leases (make-hash-table :test 'equal))
        (a-server nil) (b-server nil))
    (unwind-protect
        (let* ((live (expand-file-name "store" root))
               (prior (expand-file-name "prior-A" root))
               (prior-b (expand-file-name "prior-B" root))
               (a (dorg-test--copy-repo root "store"))
               (b (dorg-test--copy-repo root "replacement"))
               (dorg-sql-transport 'server)
               (dorg-dolt-server-autostart nil))
          (dorg-sql--with-cli-seam a
            (dorg-sql-batch a
                            '("CREATE TABLE generation_marker (id INT PRIMARY KEY, label TEXT)"
                              "INSERT INTO generation_marker VALUES (1, 'A')")
                            "seed A"))
          (dorg-sql--with-cli-seam b
            (dorg-sql-batch b
                            '("CREATE TABLE generation_marker (id INT PRIMARY KEY, label TEXT)"
                              "INSERT INTO generation_marker VALUES (1, 'B')")
                            "seed B"))
          (setq a-server (dorg-test-10z7y--start-raw-server a "A"))
          (should (equal "A" (alist-get 'label
                                        (car (dorg-sql live
                                                        "SELECT label FROM generation_marker")))))
          (let ((a-port (cdr a-server)))
            ;; A keeps serving the renamed inode while an external/raw B starts
            ;; at the original pathname and publishes its native info.
            (rename-file live prior)
            (rename-file b live)
            (setq b-server (dorg-test-10z7y--start-raw-server live "B"))
            (should (process-live-p (car a-server)))
            (should-not (= a-port (cdr b-server)))
            ;; This is the C2 assertion: the established A connection must be
            ;; evicted, not merely survive until a ping happens to fail.
            (should (equal "B" (alist-get 'label
                                            (car (dorg-sql live
                                                            "SELECT label FROM generation_marker")))))
            (should (= (cdr b-server)
                       (dorg-sql--conn-entry-port
                        (gethash (dorg-dolt-server--canonical-dir live)
                                 dorg-sql--connections))))
            ;; Reverse direction: B stops, the still-live A inode returns to
            ;; the pathname, and the next public read follows it.
            (dorg-test-10z7y--stop-raw-server b-server)
            (setq b-server nil)
            (rename-file live prior-b)
            (rename-file prior live)
            (should (equal "A" (alist-get 'label
                                            (car (dorg-sql live
                                                            "SELECT label FROM generation_marker")))))))
      (dorg-test-10z7y--stop-raw-server b-server)
      (dorg-test-10z7y--stop-raw-server a-server)
      (dorg-test--delete-owned-root root))))

(ert-deftest dorg-test-lease-integration-lease-roundtrip ()
  "End-to-end: acquire a lease, write via lease-batch and read via lease-exec on
one exact server session, refuse ordinary SQL during the hold, and reuse the
warm connection after release.  Requires dolt + mysql.el; skips otherwise."
  (cond
   ((not dorg-test--dolt-available) (ert-skip "dolt not installed"))
   ((not (dorg-sql--mysql-available-p))
    (ert-skip "mysql.el unavailable — strict server path needs it")))
  (let ((store-root (dorg-test--make-owned-temp-dir "dorg-lease-" "lease-integration"))
        (dorg-dolt-server--owned-claims (make-hash-table :test 'equal))
        (dorg-dolt-server--attached-ports (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((dolt-dir (dorg-test--copy-repo store-root))
              (dorg-sql-transport 'server)
              (dorg-dolt-server-autostart t))
          (unwind-protect
              (progn
                (dorg-sql--with-lease (lease dolt-dir)
                  (dorg-sql--lease-batch
                   lease
                   (list "INSERT INTO headings (id, root_id, parent_id, sort_key, level_offset, title) VALUES (1, 1, NULL, X'01', 0, 'Root')")
                   "lease insert")
                  (let ((rows (dorg-sql--lease-exec
                               lease "SELECT id, title FROM headings WHERE id = 1")))
                    (should (= (length rows) 1))
                    (should (equal (alist-get 'title (car rows)) "Root")))
                  ;; during the hold, ordinary SQL for this store is refused
                  (should-error (dorg-sql dolt-dir "SELECT 1 AS n")
                                :type 'dorg-sql-lease-busy))
                ;; after release the warm connection is reachable again
                (let ((rows (dorg-sql dolt-dir "SELECT id FROM headings WHERE id = 1")))
                  (should (= (length rows) 1))))
            (dorg-dolt-server--stop dolt-dir)))
      (dorg-test--delete-owned-root store-root))))

;;; ─── Revision-scoped execution holds an exclusive session (tracked-case) ──
;;
;; Acceptance-review P1: `dorg-sql--at-revision' must not pin/query/restore
;; on the SHARED pool connection — each `mysql-query' can yield, so a
;; re-entrant same-store call could reuse that exact session, switch it to
;; another revision, and restore it to live before the outer query runs
;; (the outer rows then execute live yet are tagged with the captured
;; token).  The managed path must run on the exclusive session lease:
;; re-entrant ordinary/nested same-store SQL is refused typed, never
;; multiplexed.

(defconst dorg-test-lease--rev "aaaabbbbccccddddeeeeffffgggghhhh"
  "A well-formed 32-char committed-hash token for revision-scope tests.")

(ert-deftest dorg-test-lease-at-revision-exclusive-session-sequence ()
  "R1: the managed revision scope runs `USE db/REV', the statement, and
the restoring `USE db' in order on ONE leased connection, releases the
lease, and returns the session to the warm pool."
  (dorg-test-lease--env dd
    (let ((seq nil))
      (cl-letf (((symbol-function 'mysql-query)
                 (lambda (conn sql) (push (cons conn sql) seq) 'result)))
        (dorg-sql--at-revision dd dorg-test-lease--rev "SELECT 1"))
      (let ((sqls (nreverse (mapcar #'cdr seq))))
        (should (equal sqls
                       (list (format "USE `db/%s`" dorg-test-lease--rev)
                             "SELECT 1"
                             "USE `db`")))
        (should (= (length (cl-delete-duplicates (mapcar #'car seq))) 1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (gethash (dorg-dolt-server--canonical-dir dd)
                     dorg-sql--connections))))

(ert-deftest dorg-test-lease-at-revision-reentrant-refused-not-multiplexed ()
  "R2: a re-entrant callback firing while the revision-scoped statement
yields is REFUSED typed — ordinary `dorg-sql' with `dorg-sql-lease-busy'
and a nested `dorg-sql--at-revision' likewise — and neither ever reaches
the pinned session.  NON-VACUITY: on the pooled-connection
implementation both inner calls succeeded on the exact outer session
\(observed switching it to another revision and back mid-scope)."
  (dorg-test-lease--env dd
    (let ((inner-plain 'unset) (inner-rev 'unset) (seq nil))
      (cl-letf (((symbol-function 'mysql-query)
                 (lambda (conn sql)
                   (push (cons conn sql) seq)
                   (when (equal sql "SELECT 1")  ; outer statement: yield point
                     (setq inner-plain
                           (condition-case e
                               (progn (dorg-sql dd "INNER-PLAIN") 'NO-ERROR)
                             (dorg-sql-lease-busy (car e))))
                     (setq inner-rev
                           (condition-case e
                               (progn (dorg-sql--at-revision
                                       dd dorg-test-lease--rev "INNER-REV")
                                      'NO-ERROR)
                             (dorg-sql-lease-busy (car e)))))
                   'result)))
        (dorg-sql--at-revision dd dorg-test-lease--rev "SELECT 1"))
      (should (eq inner-plain 'dorg-sql-lease-busy))
      (should (eq inner-rev 'dorg-sql-lease-busy))
      ;; Neither inner statement ever executed on any session.
      (should-not (cl-find "INNER-PLAIN" seq :key #'cdr :test #'equal))
      (should-not (cl-find "INNER-REV" seq :key #'cdr :test #'equal)))))

(ert-deftest dorg-test-lease-at-revision-statement-fault-still-restores ()
  "R3: a SQL fault in the pinned statement propagates unchanged, but the
restoring `USE db' still runs and the healthy session returns to the
warm pool — a query error must not leak a revision-pinned session."
  (dorg-test-lease--env dd
    (let ((seq nil))
      (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p)
                 (lambda (_l) t))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (push sql seq)
                   (if (equal sql "BOOM")
                       (signal 'mysql-query-error (list "bad sql"))
                     'result))))
        (should-error
         (dorg-sql--at-revision dd dorg-test-lease--rev "BOOM")
         :type 'mysql-query-error))
      (should (member "USE `db`" seq)))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (gethash (dorg-dolt-server--canonical-dir dd)
                     dorg-sql--connections))))

(ert-deftest dorg-test-lease-at-revision-restore-failure-discards-session ()
  "R4: when the restoring `USE db' fails, the session's revision pin is
unproven — the call fails closed (`dorg-sql-lease-broken') and the
session is DISCARDED, never returned to the warm pool where a later
caller would inherit a stale revision scope."
  (dorg-test-lease--env dd
    (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p)
               (lambda (_l) t))
              ((symbol-function 'mysql-query)
               (lambda (_c sql)
                 (if (equal sql "USE `db`")
                     (signal 'mysql-query-error (list "restore failed"))
                   'result))))
      (should-error
       (dorg-sql--at-revision dd dorg-test-lease--rev "SELECT 1")
       :type 'dorg-sql-lease-broken))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

;; A raw `quit' aborts the ERT runner itself, so these tests interrupt with
;; a QUIT-ANALOGUE: a condition whose `error-conditions' do NOT include
;; `error', exercising exactly the quit unwind path — no `(error …)'
;; handler catches it, only `unwind-protect' cleanups run.
(put 'dorg-test-lease--interrupt 'error-conditions
     '(dorg-test-lease--interrupt))
(put 'dorg-test-lease--interrupt 'error-message
     "simulated non-error interrupt (quit analogue)")

(ert-deftest dorg-test-lease-at-revision-quit-during-pin-never-repools ()
  "R5: a quit-class signal (which no `error' handler catches) arriving
while the initial `USE db/REV' is mid-I/O leaves the session's revision
scope INDETERMINATE — the server may or may not have applied the pin.
The release must re-pool NOTHING: only proof of completed restoration
may return a session to the warm pool.  NON-VACUITY: the pre-fix
implementation unwound through the lease's error handlers and re-pooled
the possibly-pinned connection as healthy."
  (dorg-test-lease--env dd
    (let ((discs nil))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c discs) nil))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (if (string-prefix-p "USE `db/" sql)
                       (signal 'dorg-test-lease--interrupt nil)
                     'result))))
        ;; `should-error' only catches `error'-descended conditions, so the
        ;; quit-analogue must be caught explicitly.
        (should (eq 'caught
                    (condition-case nil
                        (progn (dorg-sql--at-revision
                                dd dorg-test-lease--rev "SELECT 1")
                               'no-signal)
                      (dorg-test-lease--interrupt 'caught)))))
      ;; Exactly ONE disconnect of the stripped session — clearing the lease
      ;; slot without disconnecting would leak the live pinned connection.
      (should (equal discs '(conn1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

(ert-deftest dorg-test-lease-at-revision-quit-during-statement-never-repools ()
  "R6: a quit-class signal arriving while the PINNED statement is mid-I/O
leaves the protocol stream indeterminate; attempting a restore on it
could read stale bytes and \"succeed\" spuriously, so the session is
discarded without further I/O and never re-pooled."
  (dorg-test-lease--env dd
    (let ((discs nil) (seq nil))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c discs) nil))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (push sql seq)
                   (if (equal sql "SELECT 1")
                       (signal 'dorg-test-lease--interrupt nil)
                     'result))))
        ;; `should-error' only catches `error'-descended conditions, so the
        ;; quit-analogue must be caught explicitly.
        (should (eq 'caught
                    (condition-case nil
                        (progn (dorg-sql--at-revision
                                dd dorg-test-lease--rev "SELECT 1")
                               'no-signal)
                      (dorg-test-lease--interrupt 'caught)))))
      ;; Zero restore I/O on the indeterminate stream, one disconnect.
      (should-not (member "USE `db`" seq))
      (should (equal discs '(conn1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

(ert-deftest dorg-test-lease-at-revision-generic-pin-error-discards ()
  "R7: a GENERIC error during `USE db/REV' on a LIVE socket does not prove
the server rejected the pin — the command may already have been sent or
applied and the failure be client-side mid-read.  Only the typed
`mysql-query-error' (a parsed server response) proves never-pinned;
everything else strips + disconnects, never re-pools.  NON-VACUITY: the
round-3 implementation labeled every `error' never-pinned and re-pooled
\(reviewer diagnostic pin-pooled=t)."
  (dorg-test-lease--env dd
    (let ((discs nil))
      (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p)
                 (lambda (_l) t))
                ((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c discs) nil))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (if (string-prefix-p "USE `db/" sql)
                       (error "socket send failed mid-write")
                     'result))))
        (should-error
         (dorg-sql--at-revision dd dorg-test-lease--rev "SELECT 1")))
      (should (equal discs '(conn1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

(ert-deftest dorg-test-lease-at-revision-generic-statement-error-discards ()
  "R8: a GENERIC error during the pinned statement on a LIVE socket may
leave unread response bytes; a restoring `USE' could consume them and
spuriously appear successful.  ZERO restore I/O is permitted: strip +
disconnect, never re-pool.  Only the typed `mysql-query-error' (a
completed SQL response) permits restoration — R3 is the positive
control.  NON-VACUITY: the round-3 implementation restored and
re-pooled (reviewer diagnostic stmt-pooled=t, restore in stmt-seq)."
  (dorg-test-lease--env dd
    (let ((discs nil) (seq nil))
      (cl-letf (((symbol-function 'dorg-sql--lease-conn-live-p)
                 (lambda (_l) t))
                ((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c discs) nil))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (push sql seq)
                   (if (equal sql "SELECT 1")
                       (error "parser desync mid-read")
                     'result))))
        (should-error
         (dorg-sql--at-revision dd dorg-test-lease--rev "SELECT 1")))
      (should-not (member "USE `db`" seq))
      (should (equal discs '(conn1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

(ert-deftest dorg-test-lease-at-revision-interrupt-during-restore-discards ()
  "R9: a quit-class signal during the restoring `USE db' leaves
restoration UNPROVEN even though the statement completed — rows are
never returned off an unproven session: strip + disconnect, zero
warm-pool entry, and the interrupt propagates."
  (dorg-test-lease--env dd
    (let ((discs nil))
      (cl-letf (((symbol-function 'mysql-disconnect)
                 (lambda (c) (push c discs) nil))
                ((symbol-function 'mysql-query)
                 (lambda (_c sql)
                   (if (equal sql "USE `db`")
                       (signal 'dorg-test-lease--interrupt nil)
                     'result))))
        (should (eq 'caught
                    (condition-case nil
                        (progn (dorg-sql--at-revision
                                dd dorg-test-lease--rev "SELECT 1")
                               'no-signal)
                      (dorg-test-lease--interrupt 'caught)))))
      (should (equal discs '(conn1))))
    (should (null (dorg-sql--store-leased-p dd)))
    (should (null (gethash (dorg-dolt-server--canonical-dir dd)
                           dorg-sql--connections)))))

(provide 'dorg-lease-tests)
;;; dorg-lease-tests.el ends here
