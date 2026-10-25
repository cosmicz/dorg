;;; dorg-git-maintenance.el --- Branch-scoped object-store maintenance -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A publication that commits to a sync branch leaves its objects loose.
;; Nothing in the publication path ever packs them, so a repository that
;; publishes continuously grows loose objects without bound.  This file
;; bounds that growth for the branch the publication actually wrote, and
;; for nothing else.
;;
;; THE COMMITTER DISPATCHES.  The party that lands a sync-branch commit is
;; the party that calls `dorg-git-maintenance-dispatch', which is the only
;; entry point on a writer's path.  It does one bounded, detached launch
;; behind an error firewall and returns nil: no counting, no rev walk, no
;; lock, no packing.  A maintenance failure can therefore never change a
;; publication's outcome, and a committed publication is never revisited.
;;
;; THE STATE IS GIT'S OWN.  Selection is `pack-objects --revs --unpacked
;; --non-empty' fed the SHA the committer captured.  `--unpacked' implies
;; `--revs' and limits the pack to objects that are not already packed, so
;; pack membership *is* the record of what has been done and no durable
;; watermark exists to drift, to verify, or to outlive a crash.  A repeated
;; run writes no pack at all.
;;
;; THE SHA IS CAPTURED, NEVER RECOVERED.  Maintenance is given the commit
;; the committer had in hand and never rereads the branch ref, because the
;; ref can move or vanish between the commit and the run.  The branch is
;; carried only as an opaque label, for reporting.
;;
;; WHAT IT WILL NOT DO.  It never writes or expires refs or reflogs, never
;; prunes unreachable objects, never runs gc, and never rewrites history.
;; `prune-packed' removes only loose copies of objects that some pack
;; already holds, so an object no pack holds cannot be removed by it.
;; Consolidation of this capability's own packs keeps every object of every
;; pack it supersedes — including objects a force-move orphaned, which a
;; superseded pack may be the sole holder of — or it does not consolidate.

;;; Code:

(require 'cl-lib)
(require 'dorg-file-lock)

;;; ─── Configuration ─────────────────────────────────────────────────────

(defgroup dorg-git-maintenance nil
  "Branch-scoped object-store maintenance for publication churn."
  :group 'dorg)

(defcustom dorg-git-maintenance-enabled t
  "When exactly t, a sync-branch publication may dispatch maintenance.
Any other value disables dispatch.  The comparison is strict so a
mis-set override fails closed rather than enabling a detached process."
  :type 'boolean
  :group 'dorg-git-maintenance)

(defcustom dorg-git-maintenance-loose-object-threshold 256
  "Loose-object count at or above which a run packs."
  :type 'integer
  :group 'dorg-git-maintenance)

(defcustom dorg-git-maintenance-loose-byte-threshold (* 256 1024 1024)
  "Loose-object byte total at or above which a run packs."
  :type 'integer
  :group 'dorg-git-maintenance)

(defcustom dorg-git-maintenance-debounce-seconds 300
  "Minimum seconds between packing runs in one object store.
The clock is read from an existing artifact — the newest owned pack's
modification time — never from a marker file this capability writes."
  :type 'number
  :group 'dorg-git-maintenance)

(defcustom dorg-git-maintenance-log-file nil
  "Absolute file the detached worker appends its report to, or nil.
The worker is launched asynchronously and its output is discarded, so
without this its report — including a NAMED safety deferral such as an
unreadable superseded pack or a multi-pack-index standing consolidation
down — is not observable anywhere.  Default nil keeps the invariant that
a run creates no file outside the pack directory and the lock; an
operator who wants the outcomes opts in by naming a path."
  :type '(choice (const :tag "Report nothing" nil) file)
  :group 'dorg-git-maintenance)

(defcustom dorg-git-maintenance-pack-bound 16
  "Number of capability-owned packs above which consolidation runs."
  :type 'integer
  :group 'dorg-git-maintenance)

(defconst dorg-git-maintenance-pack-prefix "dorg-sync"
  "Filename prefix marking a pack as owned by this capability.
Only packs bearing it are ever candidates for consolidation or removal.

This is deliberately a CONSTANT and not a user option.  It is the
reserved namespace that decides what this code may delete: set to
\"pack\" it would classify git's own pack-*.pack files as owned and
eligible for removal, and a value containing directory separators would
move the `pack-objects' output base outside objects/pack entirely.  A
deletion namespace must not be configuration.")

(defconst dorg-git-maintenance--file
  (or load-file-name buffer-file-name)
  "Path to this file, so a detached worker can load it and nothing else.")

;;; ─── Git plumbing (self-contained: the worker loads this file alone) ───

(defun dorg-git-maintenance--git (dir &rest args)
  "Run git ARGS in DIR; return (EXIT . TRIMMED-OUTPUT)."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((exit (apply #'call-process "git" nil t nil args)))
        (cons exit (string-trim (buffer-string)))))))

(defun dorg-git-maintenance--git-ok (dir &rest args)
  "Run git ARGS in DIR; return output on success, nil otherwise."
  (let ((result (apply #'dorg-git-maintenance--git dir args)))
    (when (zerop (car result)) (cdr result))))

(defun dorg-git-maintenance--git-input (dir input &rest args)
  "Run git ARGS in DIR with INPUT on stdin; return (EXIT . TRIMMED-OUTPUT)."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (insert input)
      (let ((exit (apply #'call-process-region
                         (point-min) (point-max) "git" t t nil args)))
        (cons exit (string-trim (buffer-string)))))))

;;; ─── Paths ─────────────────────────────────────────────────────────────

(defun dorg-git-maintenance--pack-dir (common-dir)
  "Return COMMON-DIR's pack directory."
  (file-name-as-directory (expand-file-name "objects/pack" common-dir)))

(defun dorg-git-maintenance--lock-dir (common-dir)
  "Return the single maintenance lock directory for COMMON-DIR.
One lock covers every branch, because `prune-packed' and pack deletion
are common-store operations rather than branch operations."
  (expand-file-name "dorg-git-maintenance.lock" common-dir))

(defun dorg-git-maintenance--owned-packs (common-dir)
  "Return absolute paths of the capability-owned packs under COMMON-DIR."
  (let ((dir (dorg-git-maintenance--pack-dir common-dir)))
    (when (file-directory-p dir)
      (directory-files dir t
                       (concat "\\`" (regexp-quote
                                      dorg-git-maintenance-pack-prefix)
                               "-[0-9a-f]+\\.pack\\'")))))

;;; ─── Lock ownership ────────────────────────────────────────────────────
;; The generic owner identity, atomic publication and dead-owner election
;; live in dorg-file-lock.el.  Production takes the leaf's scoped gate.

;;; ─── Measurement ───────────────────────────────────────────────────────

(defun dorg-git-maintenance--count-loose (common-dir)
  "Return (COUNT . BYTES) for the loose objects under COMMON-DIR."
  (let ((objects (expand-file-name "objects" common-dir))
        (count 0) (bytes 0))
    (dolist (sub (ignore-errors
                   (directory-files objects t "\\`[0-9a-f]\\{2\\}\\'")))
      (dolist (file (ignore-errors
                      (directory-files sub t "\\`[0-9a-f]\\{38\\}\\'")))
        (setq count (1+ count))
        (setq bytes (+ bytes (or (file-attribute-size
                                  (file-attributes file))
                                 0)))))
    (cons count bytes)))

(defun dorg-git-maintenance--newest-owned-pack-time (common-dir)
  "Return the newest mtime among COMMON-DIR's owned packs, or nil when none."
  (let ((times (mapcar (lambda (pack)
                         (float-time (file-attribute-modification-time
                                      (file-attributes pack))))
                       (dorg-git-maintenance--owned-packs common-dir))))
    (when times (apply #'max times))))

(defun dorg-git-maintenance--native-maintenance (common-dir)
  "Return COMMON-DIR's native maintenance lock file, or nil when none.
`git gc' records its pid in `gc.pid' and `git maintenance run' holds
`maintenance.lock'.  Either means another maintainer owns this object
store right now, and racing it is how objects get lost, so this
capability stands down and reports rather than competing."
  (cl-find-if #'file-exists-p
              (list (expand-file-name "gc.pid" common-dir)
                    (expand-file-name "maintenance.lock" common-dir))))

(defun dorg-git-maintenance--stale-keep (common-dir)
  "Return a keep file under COMMON-DIR bearing this capability's name, or nil.
This capability never creates a keep, so one carrying its name is a
leftover from an interrupted run: it blocks with a report rather than
being silently reclaimed."
  (let ((dir (dorg-git-maintenance--pack-dir common-dir)))
    (when (file-directory-p dir)
      (cl-find-if
       (lambda (keep)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents keep)
             (string-match-p "dorg-git-maintenance" (buffer-string)))))
       (directory-files dir t "\\.keep\\'")))))

;;; ─── Pack object lists ─────────────────────────────────────────────────

(defun dorg-git-maintenance--pack-objects-in (dir pack)
  "Return (:ok SHAS) for PACK's contents, or (:failed REASON).
Git runs in DIR.
The distinction matters more than it looks: a pack whose index cannot be
read and a pack that is genuinely empty both used to answer nil, so an
UNREADABLE pack contributed nothing to the union, the replacement was
verified against that incomplete union, and the unreadable pack was then
deleted — losing any object only it held.  Failure must be typed so the
caller can defer instead of deleting on the strength of a silence."
  (let* ((idx (concat (file-name-sans-extension pack) ".idx"))
         (result (dorg-git-maintenance--git dir "verify-pack" "-v" idx))
         (shas nil))
    (cond
     ((not (file-exists-p idx)) (list :failed :missing-index))
     ((not (zerop (car result))) (list :failed :unreadable-index))
     (t
      (dolist (line (split-string (or (cdr result) "") "\n" t))
        (when (string-match
               "\\`\\([0-9a-f]\\{40,\\}\\) \\(commit\\|tree\\|blob\\|tag\\)\\b"
               line)
          (push (match-string 1 line) shas)))
      (list :ok shas)))))

(defun dorg-git-maintenance--reachable-objects (dir commit)
  "Return the object SHAs reachable from COMMIT in DIR."
  (let ((out (dorg-git-maintenance--git-ok dir "rev-list" "--objects" commit))
        (shas nil))
    (dolist (line (split-string (or out "") "\n" t))
      (when (string-match "\\`\\([0-9a-f]\\{40,\\}\\)" line)
        (push (match-string 1 line) shas)))
    shas))

;;; ─── Consolidation ─────────────────────────────────────────────────────

(defun dorg-git-maintenance--verify-replacement (dir replacement union)
  "Return non-nil when REPLACEMENT provably holds every SHA in UNION.
Git runs in DIR.
An enumeration failure is NOT a pass: it returns nil, so the caller
defers rather than deleting anything."
  (and (file-exists-p replacement)
       (file-exists-p (concat (file-name-sans-extension replacement) ".idx"))
       (let ((have (dorg-git-maintenance--pack-objects-in dir replacement)))
         (and (eq :ok (car have))
              (null (cl-set-difference union (cadr have) :test #'equal))))))

(defun dorg-git-maintenance--companions (pack)
  "Return PACK's companion files, including PACK itself."
  (let ((base (file-name-sans-extension pack)))
    (cl-remove-if-not
     #'file-exists-p
     (mapcar (lambda (ext) (concat base ext))
             '(".pack" ".idx" ".rev" ".bitmap" ".promisor" ".mtimes")))))

(defun dorg-git-maintenance--write-pack (dir common-dir object-names
                                             &rest flags)
  "Write a pack of OBJECT-NAMES under COMMON-DIR; return its .pack path.
Git runs in DIR.  FLAGS are extra `pack-objects' options.  Returns nil
when nothing was written, which is what `--non-empty' guarantees for an
empty selection."
  (let* ((base (expand-file-name dorg-git-maintenance-pack-prefix
                                 (dorg-git-maintenance--pack-dir common-dir)))
         (result (apply #'dorg-git-maintenance--git-input
                        dir object-names
                        (append (list "pack-objects" "--non-empty") flags
                                (list base))))
         (name (and (zerop (car result))
                    (car (split-string (cdr result) "\n" t)))))
    (when (and name (not (string-empty-p name)))
      (concat base "-" name ".pack"))))

(defun dorg-git-maintenance--consolidate (dir common-dir)
  "Fold COMMON-DIR's owned packs into one, or keep every one of them.
Git runs in DIR.

The replacement is built from the COMPLETE object list of every owned
pack being superseded, and from nothing else.  The reachable set of the
captured commit is deliberately NOT included: the packing run that
immediately precedes consolidation has just packed every loose reachable
object, so the owned packs already hold everything this capability has
ever written, while adding the reachable term would copy objects that
git's own native packs already hold into a second copy — measured at 27
in-pack objects for 18 distinct ones, turning each consolidation into a
full-branch repack and adding exactly the growth this bead exists to
bound (arc-6lqn 17:07, finding 1).

Nothing is deleted unless every superseded pack was ENUMERABLE and the
replacement provably holds every one of their objects, so an unreadable
pack defers consolidation instead of being deleted on the strength of an
empty read.  A crash or a failed verification therefore removes nothing."
  (let* ((superseded (dorg-git-maintenance--owned-packs common-dir))
         (midx (expand-file-name "multi-pack-index"
                                 (dorg-git-maintenance--pack-dir common-dir)))
         (union nil)
         (failure nil))
    ;; Enumerate first, and refuse to proceed on any failure at all.
    (dolist (pack superseded)
      (unless failure
        (let ((objects (dorg-git-maintenance--pack-objects-in dir pack)))
          (if (eq :ok (car objects))
              (setq union (append (cadr objects) union))
            (setq failure (cadr objects))))))
    (setq union (delete-dups union))
    (cond
     (failure (list :consolidation-deferred failure))
     ;; A multi-pack-index names the packs about to go.  Rewriting one
     ;; correctly BEFORE its packs are removed is the reviewed rule, and
     ;; this capability does not implement it, so it stands down rather
     ;; than deleting packs an index still references (nav 17:03 item 3).
     ((file-exists-p midx) (list :consolidation-deferred :multi-pack-index))
     ((null union) (list :consolidation-deferred :nothing-to-consolidate))
     (t
      (let ((replacement (dorg-git-maintenance--write-pack
                          dir common-dir
                          (mapconcat #'identity union "\n"))))
        (cond
         ((null replacement)
          (list :consolidation-deferred :no-replacement-written))
         ((not (dorg-git-maintenance--verify-replacement dir replacement union))
          (list :consolidation-deferred :verification-failed))
         (t
          (dolist (pack superseded)
            (unless (equal pack replacement)
              (dolist (file (dorg-git-maintenance--companions pack))
                (ignore-errors (delete-file file)))))
          (list :consolidated (length superseded)
                :replacement replacement))))))))

;;; ─── Validation ────────────────────────────────────────────────────────

(defun dorg-git-maintenance--same-repository-p (repo-dir common-dir)
  "Return non-nil when COMMON-DIR is REPO-DIR's own common git directory."
  (let ((actual (dorg-git-maintenance--git-ok
                 repo-dir "rev-parse" "--git-common-dir")))
    (and actual
         (equal (file-truename (expand-file-name actual repo-dir))
                (file-truename common-dir)))))

(defun dorg-git-maintenance--report (status reason &rest extra)
  "Return a report plist with STATUS, REASON and EXTRA."
  (append (list :status status :reason reason) extra))

;;; ─── The worker ────────────────────────────────────────────────────────

;;;###autoload
(defun dorg-git-maintenance-run (repo-dir common-dir branch commit)
  "Bound loose-object growth for the publication BRANCH at COMMIT.
REPO-DIR and COMMON-DIR must name the same physical repository; BRANCH is
an opaque non-empty label that is never resolved; COMMIT is the immutable
SHA the committing party captured.  Returns a report plist and signals
nothing the caller must handle.

ORDERING IS DELIBERATE.  One worker is launched per publication and most
have nothing to do, so the no-op path is the COMMON path: only the checks
that must precede taking a lock run before it.  The identity check does,
because a lock must never be created in a store this caller does not
own; the alternates and native-maintenance probes are file-existence
tests.  Everything that costs a directory scan or another subprocess —
the stale-keep scan, the commit-type check, the loose-object count —
happens after the lock and after the debounce decision, so a debounced
worker exits having run one `rev-parse' and three `file-exists-p'."
  (cond
   ((not (and (stringp repo-dir) (stringp common-dir)
              (stringp branch) (not (string-empty-p branch))
              (stringp commit) (not (string-empty-p commit))
              (file-directory-p repo-dir)))
    (dorg-git-maintenance--report :no-op :invalid-tuple))
   ((not (dorg-git-maintenance--same-repository-p repo-dir common-dir))
    (dorg-git-maintenance--report :no-op :identity-mismatch))
   ((file-exists-p (expand-file-name "objects/info/alternates" common-dir))
    (dorg-git-maintenance--report :no-op :alternates))
   ((dorg-git-maintenance--native-maintenance common-dir)
    (dorg-git-maintenance--report
     :no-op :native-maintenance
     :detail (dorg-git-maintenance--native-maintenance common-dir)))
   (t
    (let* ((lock (dorg-git-maintenance--lock-dir common-dir))
           (run (dorg-file-lock-call
                 lock 0
                 (lambda (_record)
                   (dorg-git-maintenance--run-locked
                    repo-dir common-dir branch commit)))))
      (if (eq :acquired (plist-get run :status))
          (plist-get run :value)
        (let* ((inspection (dorg-file-lock-inspect lock))
               (status (plist-get run :status))
               (reason (if (eq status :reclaim-blocked)
                           :reclaim-blocked :lock-held)))
          ;; This detached maintenance worker records owner identity for
          ;; operators.  Public mutation contention does not expose it.
          (dorg-git-maintenance--report
           :no-op reason :detail (and (eq status :reclaim-blocked)
                                      (dorg-file-lock--tombstone lock))
           :lock-status status :owner (plist-get inspection :owner))))))))

(defun dorg-git-maintenance--run-locked (repo-dir common-dir branch commit)
  "Pack REPO-DIR's COMMON-DIR for BRANCH at COMMIT, with the lock held.
The debounce decision comes FIRST and nothing is scanned or counted
before it, so the common debounced no-op does not walk the object
directories it is about to decline to touch."
  (let ((last (dorg-git-maintenance--newest-owned-pack-time common-dir))
        (keep nil))
    (cond
     ((and last
           (< (- (float-time) last) dorg-git-maintenance-debounce-seconds))
      (dorg-git-maintenance--report :no-op :debounced :branch branch))
     ((setq keep (dorg-git-maintenance--stale-keep common-dir))
      (dorg-git-maintenance--report :no-op :stale-keep :detail keep))
     ((not (equal "commit" (dorg-git-maintenance--git-ok
                            repo-dir "cat-file" "-t" commit)))
      (dorg-git-maintenance--report :no-op :not-a-commit))
     (t
      (let ((loose (dorg-git-maintenance--count-loose common-dir)))
        (if (and (< (car loose) dorg-git-maintenance-loose-object-threshold)
                 (< (cdr loose) dorg-git-maintenance-loose-byte-threshold))
            (dorg-git-maintenance--report :no-op :below-threshold
                                          :loose (car loose) :branch branch)
          (let ((pack (dorg-git-maintenance--write-pack
                       repo-dir common-dir commit "--revs" "--unpacked")))
            (if (null pack)
                (dorg-git-maintenance--report :no-op :nothing-to-pack
                                              :branch branch)
              ;; Remove only loose copies of objects some pack already holds.
              (dorg-git-maintenance--git-ok repo-dir "prune-packed")
              (let ((owned (dorg-git-maintenance--owned-packs common-dir)))
                (append
                 (dorg-git-maintenance--report :packed :packed
                                               :pack pack :branch branch
                                               :owned-packs (length owned))
                 (when (> (length owned) dorg-git-maintenance-pack-bound)
                   (dorg-git-maintenance--consolidate
                    repo-dir common-dir))))))))))))

(defun dorg-git-maintenance--record (report)
  "Append REPORT to `dorg-git-maintenance-log-file' when one is configured.
Returns REPORT.  Never signals: an unwritable log must not turn a
successful maintenance run into a failure."
  (when (and dorg-git-maintenance-log-file report)
    (ignore-errors
      (let ((line (format "%s %S\n"
                          (format-time-string "%FT%T%z") report)))
        (write-region line nil dorg-git-maintenance-log-file t 'silent))))
  report)

;;; ─── The writer's path ─────────────────────────────────────────────────

(defun dorg-git-maintenance--launch (repo-dir common-dir branch commit)
  "Start a detached worker for REPO-DIR, COMMON-DIR, BRANCH and COMMIT.
Returns immediately.  The worker is an Emacs batch process that loads
this file and nothing else.  `call-process' with a destination of 0 runs
it asynchronously and does not wait for it, so the writer is never
blocked by maintenance.

The worker starts from -Q, so it would otherwise run on this file's
compiled defaults and silently ignore whatever the caller configured.
The tunables are therefore transmitted explicitly.  They are CONFIGURATION
and are passed separately from the four-value identity tuple, which is
unchanged and remains the only thing that says which repository, branch
and commit are being maintained."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (call-process
     emacs nil 0 nil
     "-Q" "--batch"
     "-l" dorg-git-maintenance--file
     "--eval"
     (format "(progn (setq dorg-git-maintenance-loose-object-threshold %S \
dorg-git-maintenance-loose-byte-threshold %S \
dorg-git-maintenance-debounce-seconds %S \
dorg-git-maintenance-pack-bound %S \
dorg-git-maintenance-log-file %S) \
(dorg-git-maintenance--record (dorg-git-maintenance-run %S %S %S %S)))"
             dorg-git-maintenance-loose-object-threshold
             dorg-git-maintenance-loose-byte-threshold
             dorg-git-maintenance-debounce-seconds
             dorg-git-maintenance-pack-bound
             dorg-git-maintenance-log-file
             repo-dir common-dir branch commit))
    t))

;;;###autoload
(defun dorg-git-maintenance-dispatch (repo-dir common-dir branch commit)
  "Launch maintenance for REPO-DIR and COMMON-DIR on BRANCH at COMMIT.
Returns nil.  This is the ONLY function a writer calls, and it is
deliberately almost empty: it validates the four values it was handed,
then makes one bounded detached launch inside an error firewall.  It
never counts objects, walks revisions, acquires the maintenance lock or
packs anything, and it never
signals — a maintenance problem must not become a publication problem."
  (condition-case nil
      (when (and (eq dorg-git-maintenance-enabled t)
                 (stringp repo-dir) (stringp common-dir)
                 (stringp branch) (not (string-empty-p branch))
                 (stringp commit) (not (string-empty-p commit)))
        (dorg-git-maintenance--launch repo-dir common-dir branch commit)
        nil)
    (error nil)))

(provide 'dorg-git-maintenance)
;;; dorg-git-maintenance.el ends here
