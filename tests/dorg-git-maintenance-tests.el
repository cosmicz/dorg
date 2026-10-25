;;; dorg-git-maintenance-tests.el --- Tests for dorg-git-maintenance -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Controls for the branch-scoped object-store maintenance capability
;; (`dorg-git-maintenance.el').  Every control runs against a throwaway
;; `git init' repository the test builds: server-free and
;; operator-repository-free.  No control needs the dolt binary, a
;; sql-server, a material window, or any repository outside the temp dir.
;;
;; The capability's contract, in one line: given the four values the
;; committing party already holds (repo dir, common git dir, branch label,
;; and the immutable SHA it captured), pack exactly the still-loose objects
;; reachable from that SHA, drop the loose duplicates, and never lose an
;; object — including one orphaned by a force-move and held only by a pack
;; this capability wrote.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-git)
(require 'dorg-git-maintenance)

;; Legacy lock-form fixtures stay here; production uses dorg-file-lock-call.
(defalias 'dorg-git-maintenance--owner-file
  #'dorg-file-lock--legacy-owner-file)
(defalias 'dorg-git-maintenance--read-owner
  #'dorg-file-lock--read-owner)
(defalias 'dorg-git-maintenance--put-owner
  #'dorg-file-lock--put-legacy-owner)
(defalias 'dorg-git-maintenance--new-token
  #'dorg-file-lock--new-token)
(defalias 'dorg-git-maintenance--valid-time-p
  #'dorg-file-lock--valid-time-p)
(defalias 'dorg-git-maintenance--compare-times
  #'dorg-file-lock--compare-times)
(defalias 'dorg-git-maintenance--linux-scope
  #'dorg-file-lock--linux-scope)
(defalias 'dorg-git-maintenance--linux-identity
  #'dorg-file-lock--linux-identity)
(defalias 'dorg-git-maintenance--valid-identity-p
  #'dorg-file-lock--valid-identity-p)
(defalias 'dorg-git-maintenance--process-identity
  #'dorg-file-lock--process-identity)
(defalias 'dorg-git-maintenance--owner-gone-p
  #'dorg-file-lock--owner-gone-p)
(defalias 'dorg-git-maintenance--tombstone
  #'dorg-file-lock--tombstone)
(defalias 'dorg-git-maintenance--mkdir-exclusive
  #'dorg-file-lock--mkdir-exclusive)

(defun dorg-git-maintenance--write-owner (lock &optional token)
  "Write old directory LOCK metadata for migration controls only."
  (let ((owner (dorg-file-lock--owner token)))
    (dorg-file-lock--put-legacy-owner lock owner)
    (alist-get :token owner)))

(defun dorg-git-maintenance--acquire (lock)
  "Acquire LOCK once for legacy-form fixture controls."
  (let ((ownership (list nil)))
    (when (eq :acquired (dorg-file-lock--acquire lock 0 ownership))
      (alist-get :token (dorg-file-lock--decode-owner (car ownership))))))

(defun dorg-git-maintenance--release (lock token)
  "Release fixture LOCK only when TOKEN matches its current owner."
  (let ((owner (dorg-file-lock--read-owner lock)))
    (when (and token (equal token (alist-get :token owner)))
      (if-let ((record (file-symlink-p lock)))
          (dorg-file-lock--release lock record)
        (when (eq (car (file-attributes lock)) t)
          (delete-directory lock t)
          t)))))

;;; ─── Fixtures ──────────────────────────────────────────────────────────

(defmacro dorg-git-maintenance-tests--with-repo (dir-var &rest body)
  "Create a throwaway git repo, bind DIR-VAR to its toplevel, run BODY.
The repo is removed afterwards.  Thresholds are lowered so a fixture-sized
repository reaches the packing path; the production defaults are proven
separately by `dorg-test-git-maintenance-defaults-are-production-sized'."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((,dir-var (file-name-as-directory (make-temp-file "dorg-gm-" t)))
          (dorg-git-maintenance-loose-object-threshold 1)
          (dorg-git-maintenance-loose-byte-threshold 1)
          (dorg-git-maintenance-debounce-seconds 0))
     (unwind-protect
         (progn
           (dorg-git--run ,dir-var "init" "-q")
           (dorg-git--run ,dir-var "config" "user.email" "test@example.invalid")
           (dorg-git--run ,dir-var "config" "user.name" "DOrg Test")
           (dorg-git--run ,dir-var "config" "gc.auto" "0")
           ,@body)
       (delete-directory ,dir-var t))))

(defun dorg-git-maintenance-tests--commit (dir file content)
  "Write CONTENT to FILE under DIR, commit it, and return the new SHA."
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    (dorg-git--run dir "add" "--" file)
    (dorg-git--run dir "commit" "-q" "--no-verify" "-m" (format "add %s" file))
    (dorg-git--ok dir "rev-parse" "HEAD")))

(defun dorg-git-maintenance-tests--common-dir (dir)
  "Return DIR's common git directory."
  (dorg-git--common-dir dir))

(defun dorg-git-maintenance-tests--loose-objects (dir)
  "Return the sorted list of loose object SHAs in DIR's object store."
  (let* ((objects (expand-file-name "objects" (dorg-git--common-dir dir)))
         (found nil))
    (dolist (sub (ignore-errors (directory-files objects t "\\`[0-9a-f]\\{2\\}\\'")))
      (dolist (file (directory-files sub nil "\\`[0-9a-f]\\{38\\}\\'"))
        (push (concat (file-name-nondirectory sub) file) found)))
    (sort found #'string<)))

(defun dorg-git-maintenance-tests--packs (dir)
  "Return the sorted list of pack basenames (without extension) in DIR."
  (let ((pack-dir (expand-file-name "objects/pack" (dorg-git--common-dir dir))))
    (sort (mapcar #'file-name-base
                  (ignore-errors
                    (directory-files pack-dir t "\\.pack\\'")))
          #'string<)))

(defun dorg-git-maintenance-tests--pack-objects (dir pack)
  "Return the sorted SHAs contained in PACK (a basename) under DIR."
  (let* ((pack-dir (expand-file-name "objects/pack" (dorg-git--common-dir dir)))
         (idx (expand-file-name (concat pack ".idx") pack-dir))
         (out (dorg-git--ok dir "verify-pack" "-v" idx))
         (shas nil))
    (dolist (line (split-string (or out "") "\n" t))
      (when (string-match "\\`\\([0-9a-f]\\{40\\}\\) \\(commit\\|tree\\|blob\\|tag\\)\\b" line)
        (push (match-string 1 line) shas)))
    (sort shas #'string<)))

(defun dorg-git-maintenance-tests--resolvable-p (dir sha)
  "Return non-nil when SHA resolves as an object in DIR."
  (and (dorg-git--ok dir "cat-file" "-e" (concat sha "^{}")) t))

(defun dorg-git-maintenance-tests--run (dir branch commit)
  "Run maintenance against DIR for BRANCH at COMMIT; return the report."
  (dorg-git-maintenance-run dir (dorg-git--common-dir dir) branch commit))

(defun dorg-git-maintenance-tests--tree-snapshot (dir relative)
  "Return a sorted list of (NAME . SIZE) for files under RELATIVE in DIR."
  (let ((root (expand-file-name relative (dorg-git--common-dir dir))))
    (sort
     (mapcar (lambda (f)
               (cons (file-relative-name f root)
                     (file-attribute-size (file-attributes f))))
             (cl-remove-if #'file-directory-p
                           (ignore-errors
                             (directory-files-recursively root ""))))
     (lambda (a b) (string< (car a) (car b))))))

;;; ─── Selection, idempotence and stored state ───────────────────────────

(ert-deftest dorg-test-git-maintenance-packs-only-loose-reachable-objects ()
  "One run packs exactly the still-loose objects reachable from the SHA.
Already-packed objects are not repacked and objects reachable only from
another branch are never packed."
  (dorg-git-maintenance-tests--with-repo dir
    ;; A: committed then packed, so its objects are already in a pack.
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "repack" "-a" "-d" "-q")
    (let ((packed-a (dorg-git-maintenance-tests--packs dir)))
      (should (= 1 (length packed-a)))
      ;; C: a commit on another branch, left loose.
      (dorg-git--run dir "checkout" "-q" "-b" "other")
      (let ((c (dorg-git-maintenance-tests--commit dir "c.txt" "C\n")))
        (dorg-git--run dir "checkout" "-q" "-")
        ;; B: the publication we captured, left loose.
        (let* ((b (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
               (report (dorg-git-maintenance-tests--run dir "main" b))
               (new (cl-set-difference (dorg-git-maintenance-tests--packs dir)
                                       packed-a :test #'equal)))
          (should (eq :packed (plist-get report :status)))
          (should (= 1 (length new)))
          (let ((objects (dorg-git-maintenance-tests--pack-objects
                          dir (car new))))
            ;; Exactly B's commit, its tree and its blob.
            (should (= 3 (length objects)))
            (should (member b objects))
            ;; The other-branch commit is not in it.
            (should-not (member c objects))))))))

(ert-deftest dorg-test-git-maintenance-writes-no-pack-when-nothing-loose ()
  "With no loose reachable objects the run writes no pack at all."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (dorg-git--run dir "repack" "-a" "-d" "-q")
      (let* ((before (dorg-git-maintenance-tests--packs dir))
             (report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :no-op (plist-get report :status)))
        (should (equal before (dorg-git-maintenance-tests--packs dir)))))))

(ert-deftest dorg-test-git-maintenance-second-run-packs-nothing ()
  "An immediately repeated run packs nothing, with no durable record."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (dorg-git-maintenance-tests--run dir "main" sha)
      (let* ((after-first (dorg-git-maintenance-tests--packs dir))
             (report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :no-op (plist-get report :status)))
        (should (equal after-first (dorg-git-maintenance-tests--packs dir)))))))

(ert-deftest dorg-test-git-maintenance-non-empty-suppresses-an-empty-pack ()
  "With loose objects present but none selected, no pack is written.
This reaches `--non-empty' itself rather than stopping at the loose-count
threshold: the store still has loose objects, so the run proceeds, but
nothing reachable from the captured SHA is still unpacked."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                      dir "main" sha)
                                     :status)))
      ;; Loose objects that the captured commit does not reach, so the
      ;; threshold is passed but the selection is empty.
      (dorg-git--run dir "checkout" "-q" "-b" "other")
      (dorg-git-maintenance-tests--commit dir "c.txt" "C\n")
      (dorg-git--run dir "checkout" "-q" "-")
      (should (dorg-git-maintenance-tests--loose-objects dir))
      (let* ((before (dorg-git-maintenance-tests--packs dir))
             (report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :nothing-to-pack (plist-get report :reason)))
        (should (equal before (dorg-git-maintenance-tests--packs dir)))))))

(ert-deftest dorg-test-git-maintenance-creates-no-file-outside-pack-dir-and-lock ()
  "No path creates a marker: only the pack directory and the lock change.
The debounce clock is read from existing artifacts, so a watermark file
anywhere under the common git dir is a failure."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (common (dorg-git--common-dir dir))
           (before (dorg-git-maintenance-tests--tree-snapshot dir "."))
           (_ (dorg-git-maintenance-tests--run dir "main" sha))
           (after (dorg-git-maintenance-tests--tree-snapshot dir "."))
           (added (cl-set-difference after before :test #'equal)))
      (ignore common)
      ;; Every added file must live under objects/pack.
      (dolist (entry added)
        (should (string-prefix-p "objects/pack/" (car entry)))))))

;;; ─── Loose-copy removal, stated as the race-proof guarantee ────────────

(ert-deftest dorg-test-git-maintenance-prune-is-duplicate-only ()
  "Loose copies of packed objects go; an object no pack holds never does."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "checkout" "-q" "-b" "other")
    (let ((c (dorg-git-maintenance-tests--commit dir "c.txt" "C\n")))
      (dorg-git--run dir "checkout" "-q" "-")
      (let* ((b (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
             (before (dorg-git-maintenance-tests--loose-objects dir)))
        (dorg-git-maintenance-tests--run dir "main" b)
        (let ((after (dorg-git-maintenance-tests--loose-objects dir)))
          ;; The other-branch commit was in no pack, so it is still loose.
          (should (member c after))
          ;; The captured commit was packed, so its loose copy is gone.
          (should-not (member b after))
          (should (< (length after) (length before))))))))

(ert-deftest dorg-test-git-maintenance-every-pre-run-object-still-resolves ()
  "The race-proof form: everything resolvable before the run resolves after."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "checkout" "-q" "-b" "other")
    (dorg-git-maintenance-tests--commit dir "c.txt" "C\n")
    (dorg-git--run dir "checkout" "-q" "-")
    (let* ((b (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
           (before (dorg-git-maintenance-tests--loose-objects dir)))
      (dorg-git-maintenance-tests--run dir "main" b)
      (dolist (sha before)
        (should (dorg-git-maintenance-tests--resolvable-p dir sha))))))

;;; ─── Captured identity: never recovered from a moving ref ──────────────

(ert-deftest dorg-test-git-maintenance-packs-captured-sha-after-force-move ()
  "A branch force-moved after capture must still pack the captured commit."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (let ((b (dorg-git-maintenance-tests--commit dir "b.txt" "B\n")))
      ;; The publication landed at B; the branch then moves backwards.
      (dorg-git--run dir "reset" "-q" "--hard" "HEAD~1")
      (let* ((report (dorg-git-maintenance-tests--run dir "main" b))
             (packs (dorg-git-maintenance-tests--packs dir)))
        (should (eq :packed (plist-get report :status)))
        (should (cl-some (lambda (p)
                           (member b (dorg-git-maintenance-tests--pack-objects
                                      dir p)))
                         packs))
        (should (dorg-git-maintenance-tests--resolvable-p dir b))))))

(ert-deftest dorg-test-git-maintenance-packs-captured-sha-after-ref-deletion ()
  "Deleting the branch after capture must still pack the captured commit."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "checkout" "-q" "-b" "doomed")
    (let ((b (dorg-git-maintenance-tests--commit dir "b.txt" "B\n")))
      (dorg-git--run dir "checkout" "-q" "-")
      (dorg-git--run dir "branch" "-q" "-D" "doomed")
      (let ((report (dorg-git-maintenance-tests--run dir "doomed" b)))
        (should (eq :packed (plist-get report :status)))
        ;; Resolvability alone is too weak: an unpacked object stays
        ;; resolvable simply by remaining loose.  The captured commit must
        ;; actually be IN a pack.
        (should (cl-some (lambda (p)
                           (member b (dorg-git-maintenance-tests--pack-objects
                                      dir p)))
                         (dorg-git-maintenance-tests--packs dir)))
        (should (dorg-git-maintenance-tests--resolvable-p dir b))))))

(ert-deftest dorg-test-git-maintenance-rejects-invalid-tuples ()
  "Invalid tuple, empty label and non-commit SHA are each a reported no-op."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (common (dorg-git--common-dir dir))
           (blob (dorg-git--ok dir "rev-parse" "HEAD:a.txt"))
           (before (dorg-git-maintenance-tests--packs dir)))
      ;; Empty branch label.
      (should (eq :no-op (plist-get (dorg-git-maintenance-run
                                     dir common "" sha)
                                    :status)))
      ;; A blob is not a commit.
      (should (eq :no-op (plist-get (dorg-git-maintenance-run
                                     dir common "main" blob)
                                    :status)))
      ;; A SHA that does not exist at all.
      (should (eq :no-op (plist-get (dorg-git-maintenance-run
                                     dir common "main" (make-string 40 ?0))
                                    :status)))
      ;; Nil components.
      (should (eq :no-op (plist-get (dorg-git-maintenance-run
                                     dir common "main" nil)
                                    :status)))
      (should (equal before (dorg-git-maintenance-tests--packs dir))))))

(ert-deftest dorg-test-git-maintenance-rejects-foreign-common-dir ()
  "A common dir belonging to another physical repository is a no-op."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--with-repo other
      (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
             (foreign (dorg-git--common-dir other))
             (before (dorg-git-maintenance-tests--packs dir))
             (report (dorg-git-maintenance-run dir foreign "main" sha)))
        (should (eq :no-op (plist-get report :status)))
        (should (eq :identity-mismatch (plist-get report :reason)))
        (should (equal before (dorg-git-maintenance-tests--packs dir)))))))

;;; ─── Preservation ──────────────────────────────────────────────────────

(ert-deftest dorg-test-git-maintenance-preserves-refs-and-reflogs ()
  "Refs and reflogs are byte-identical across a normal run."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (let* ((sha (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
           (refs-before (dorg-git--ok dir "show-ref"))
           (reflog-before (dorg-git--ok dir "reflog" "show" "--all")))
      (dorg-git-maintenance-tests--run dir "main" sha)
      (should (equal refs-before (dorg-git--ok dir "show-ref")))
      (should (equal reflog-before (dorg-git--ok dir "reflog" "show" "--all"))))))

(ert-deftest dorg-test-git-maintenance-no-op-with-alternates ()
  "With alternates configured the run is a reported no-op."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--with-repo donor
      (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
             (alt (expand-file-name "objects/info/alternates"
                                    (dorg-git--common-dir dir)))
             (before (dorg-git-maintenance-tests--packs dir)))
        (make-directory (file-name-directory alt) t)
        (with-temp-file alt
          (insert (expand-file-name "objects" (dorg-git--common-dir donor)) "\n"))
        (let ((report (dorg-git-maintenance-tests--run dir "main" sha)))
          (should (eq :no-op (plist-get report :status)))
          (should (eq :alternates (plist-get report :reason)))
          (should (equal before (dorg-git-maintenance-tests--packs dir))))))))

;;; ─── Serialization ─────────────────────────────────────────────────────

(ert-deftest dorg-test-git-maintenance-stands-down-for-native-maintenance ()
  "While git's own maintenance owns the store, this capability stands down.
`git gc' records its pid in `gc.pid' and `git maintenance run' holds
`maintenance.lock'; either means another maintainer is working on this
object store, and racing it is exactly how objects get lost."
  (dolist (lock-name '("gc.pid" "maintenance.lock"))
    (dorg-git-maintenance-tests--with-repo dir
      (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
             (common (dorg-git--common-dir dir))
             (lock (expand-file-name lock-name common))
             (before (dorg-git-maintenance-tests--packs dir)))
        (with-temp-file lock (insert (number-to-string (emacs-pid)) "\n"))
        (let ((report (dorg-git-maintenance-tests--run dir "main" sha)))
          (should (eq :no-op (plist-get report :status)))
          (should (eq :native-maintenance (plist-get report :reason)))
          (should (equal before (dorg-git-maintenance-tests--packs dir))))
        ;; Once the other maintainer is gone the next run is ordinary.
        (delete-file lock)
        (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                        dir "main" sha)
                                       :status)))))))

(ert-deftest dorg-test-git-maintenance-held-lock-is-a-successful-no-op ()
  "A second run whose lock is held exits successfully without retrying."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (lock (dorg-git-maintenance--lock-dir (dorg-git--common-dir dir)))
           (before (dorg-git-maintenance-tests--packs dir)))
      ;; Hold the lock as a live owner: this very Emacs process.
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :no-op (plist-get report :status)))
        (should (eq :lock-held (plist-get report :reason)))
        (should (equal before (dorg-git-maintenance-tests--packs dir)))))))

(ert-deftest dorg-test-git-maintenance-reclaims-only-a-dead-owner ()
  "A stale lock is reclaimed only on affirmative proof the owner is gone.
A live owner is never reclaimed, and a recorded pid that is live but whose
birth identity differs — the pid-reuse case — is treated as gone."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common)))
      ;; 1. Live owner (this process): not reclaimable.
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (should-not (dorg-git-maintenance--owner-gone-p lock))
      ;; 2. Same pid, different recorded birth identity: owner gone.
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (let ((identity (alist-get :identity owner)))
          (pcase (plist-get identity :kind)
            ('linux-kernel
             (cl-incf (plist-get identity :start-ticks)))
            ('darwin-time (setf (plist-get identity :start) '(1 1 1 1)))
            (_ (ert-fail (list :unknown-native-identity identity)))))
        (dorg-git-maintenance--put-owner lock owner))
      (should (dorg-git-maintenance--owner-gone-p lock))
      ;; 3. A pid that cannot exist: owner gone.
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 999999999)
        (dorg-git-maintenance--put-owner lock owner))
      (should (dorg-git-maintenance--owner-gone-p lock))
      ;; The run reclaims that dead lock and proceeds.
      (let ((report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :packed (plist-get report :status)))))))

(ert-deftest dorg-test-git-maintenance-releases-only-with-matching-token ()
  "Release only succeeds for the holder's own token."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (token (dorg-git-maintenance--acquire lock)))
      (should token)
      ;; A foreign token must not release the lock.
      (should-not (dorg-git-maintenance--release lock "not-the-token"))
      (should (file-symlink-p lock))
      ;; The holder's own token does.
      (should (dorg-git-maintenance--release lock token))
      (should-not (file-symlink-p lock)))))

(ert-deftest dorg-test-git-maintenance-two-branches-share-one-lock ()
  "One common-store lock serialises different branches, not just one."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common)))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      ;; A different branch label must still be serialised by the same lock.
      (let ((report (dorg-git-maintenance-run dir common "a-different-branch" sha)))
        (should (eq :lock-held (plist-get report :reason)))))))

(ert-deftest dorg-test-git-maintenance-native-self-owner-stays-held ()
  "Repeated native observations must never reclaim this process's lock.
Keep kernel and Emacs start observations in the failure receipt: Linux
Emacs derives a wall-clock start from two separately sampled clocks."
  (let* ((lock (make-temp-file "dorg-gm-live-owner-" t))
         (token (dorg-git-maintenance--write-owner lock))
         (owner (dorg-git-maintenance--read-owner lock))
         (starts nil)
         (kernel-starts nil)
         (gone 0)
         (stolen nil))
    (unwind-protect
        (progn
          (dotimes (_ 100)
            (cl-pushnew (cdr (assq 'start (process-attributes (emacs-pid))))
                        starts :test #'equal)
            (when (eq system-type 'gnu/linux)
              (with-temp-buffer
                (insert-file-contents (format "/proc/%d/stat" (emacs-pid)))
                ;; Field 3 follows the final closing parenthesis of comm.
                (goto-char (point-max))
                (search-backward ") ")
                (let ((ticks (nth 19 (split-string
                                      (buffer-substring-no-properties
                                       (+ (point) 2) (point-max))))))
                  (should (string-match-p "\\`[0-9]+\\'" ticks))
                  (cl-pushnew ticks kernel-starts :test #'equal))))
            (when (dorg-git-maintenance--owner-gone-p lock)
              (cl-incf gone))
            (unless stolen
              (setq stolen (dorg-git-maintenance--acquire lock)))
            (sleep-for 0.01))
          (message "native-owner pid=%s owner=%S starts=%S kernel-starts=%S gone=%s stolen=%S"
                   (emacs-pid) owner (reverse starts) kernel-starts gone stolen)
          (when (eq system-type 'gnu/linux)
            (should (= 1 (length kernel-starts))))
          (should (= 0 gone))
          (should-not stolen)
          (should (equal token (alist-get :token
                                         (dorg-git-maintenance--read-owner lock)))))
      (delete-directory lock t))))

;;; ─── Kernel birth identity ─────────────────────────────────────────────

(defun dorg-git-maintenance-tests--proc-write (root path text)
  "Write TEXT at PATH in the owned procfs fixture ROOT."
  (let ((file (expand-file-name path root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))))

(defun dorg-git-maintenance-tests--proc-stat (pid ticks)
  "Return PID's stat record with a difficult comm and field 22 TICKS."
  (format "%d (worker ) with spaces)) S %s %s 0 0\n"
          pid (mapconcat #'identity (make-list 18 "0") " ") ticks))

(defmacro dorg-git-maintenance-tests--with-proc (&rest body)
  "Run BODY with real procfs readers over an owned fixture on any platform.
Bind proc-root, lock, pid, boot, token, enum, enum-calls and attrs-calls.
Only the procfs root and platform probes are substituted; owner publication,
identity parsing, death decisions and reclamation execute production code."
  (declare (indent 0) (debug (body)))
  `(let* ((proc-root (make-temp-file "dorg-gm-proc-" t))
          (lock (expand-file-name "lock" proc-root))
          (pid (emacs-pid))
          (boot "01234567-89ab-cdef-0123-456789abcdef")
          (enum (list pid)) (enum-calls 0) (attrs-calls 0)
          (read-scope (symbol-function 'dorg-file-lock--linux-scope))
          (read-identity (symbol-function 'dorg-file-lock--linux-identity))
          (system-type 'gnu/linux))
     (unwind-protect
         (progn
           (dorg-git-maintenance-tests--proc-write
            proc-root "sys/kernel/random/boot_id" (concat boot "\n"))
           (dorg-git-maintenance-tests--proc-write
            proc-root (format "%d/stat" pid)
            (dorg-git-maintenance-tests--proc-stat pid "9141"))
           (dolist (path (list "self/ns/pid" (format "%d/ns/pid" pid)))
             (make-directory (file-name-directory (expand-file-name path proc-root)) t)
             (make-symbolic-link "pid:[1234]" (expand-file-name path proc-root)))
           (make-directory lock)
           (cl-letf (((symbol-function 'dorg-file-lock--linux-scope)
                      (lambda (&optional _) (funcall read-scope proc-root)))
                     ((symbol-function 'dorg-file-lock--linux-identity)
                      (lambda (p scope &optional _)
                        (funcall read-identity p scope proc-root)))
                     ((symbol-function 'list-system-processes)
                      (lambda () (cl-incf enum-calls) enum))
                     ((symbol-function 'process-attributes)
                      (lambda (_)
                        (cl-incf attrs-calls)
                        (list (cons 'start (current-time))))))
             (let ((token (dorg-git-maintenance--write-owner lock)))
               (ignore token)
               (setq enum-calls 0 attrs-calls 0)
               ,@body)))
       (delete-directory proc-root t))))

(ert-deftest dorg-test-git-maintenance-linux-stable-kernel-owner ()
  "Kernel ticks, including tricky comm parsing, preserve a live holder."
  (dorg-git-maintenance-tests--with-proc
    (let ((identity (alist-get :identity (dorg-git-maintenance--read-owner lock))))
      (should (equal identity (list :kind 'linux-kernel :boot-id boot
                                    :pid-ns "pid:[1234]" :start-ticks 9141))))
    (dotimes (_ 3)
      (should-not (dorg-git-maintenance--owner-gone-p lock))
      (should-not (dorg-git-maintenance--acquire lock)))
    (should (> enum-calls 0))
    (should (= attrs-calls 0))
    (should (equal token (alist-get :token (dorg-git-maintenance--read-owner lock))))))

(ert-deftest dorg-test-git-maintenance-linux-reused-pid-is-reclaimable ()
  "A different birth counter in the same boot and namespace proves reuse."
  (dorg-git-maintenance-tests--with-proc
    (dorg-git-maintenance-tests--proc-write
     proc-root (format "%d/stat" pid)
     (dorg-git-maintenance-tests--proc-stat pid "9142"))
    (should (dorg-git-maintenance--owner-gone-p lock))
    (let ((replacement (dorg-git-maintenance--acquire lock)))
      (should replacement)
      (should-not (equal token replacement))
      (should-not (dorg-git-maintenance--owner-gone-p lock))
      (should (dorg-git-maintenance--release lock replacement)))))

(ert-deftest dorg-test-git-maintenance-linux-partial-evidence-preserves ()
  "Missing, malformed or unreadable evidence never proves death."
  (dolist (fault '(boot-missing boot-malformed self-missing self-malformed
                  candidate-missing candidate-foreign stat-missing stat-short
                  stat-wrong-pid stat-bad-state stat-nonnumeric stat-unreadable enum-empty))
    (ert-info ((format "procfs fault: %s" fault))
      (dorg-git-maintenance-tests--with-proc
        (let ((boot-file (expand-file-name "sys/kernel/random/boot_id" proc-root))
              (self-link (expand-file-name "self/ns/pid" proc-root))
              (candidate-link (expand-file-name (format "%d/ns/pid" pid) proc-root))
              (stat-path (format "%d/stat" pid)))
          (pcase fault
            ('boot-missing (delete-file boot-file))
            ('boot-malformed (with-temp-file boot-file (insert "not-a-boot")))
            ('self-missing (delete-file self-link))
            ('self-malformed (make-symbolic-link "bad" self-link t))
            ('candidate-missing (delete-file candidate-link))
            ('candidate-foreign (make-symbolic-link "pid:[5678]" candidate-link t))
            ('stat-missing (delete-file (expand-file-name stat-path proc-root)))
            ('stat-short
             (dorg-git-maintenance-tests--proc-write
              proc-root stat-path (format "%d (x) R" pid)))
            ('stat-wrong-pid
             (dorg-git-maintenance-tests--proc-write
              proc-root stat-path (dorg-git-maintenance-tests--proc-stat (1+ pid) "9142")))
            ('stat-nonnumeric
             (dorg-git-maintenance-tests--proc-write
              proc-root stat-path (dorg-git-maintenance-tests--proc-stat pid "9x42")))
            ('stat-bad-state
             (dorg-git-maintenance-tests--proc-write
              proc-root stat-path
              (replace-regexp-in-string ") S " ") 0 "
                                        (dorg-git-maintenance-tests--proc-stat pid "9142"))))
            ('stat-unreadable
             ;; A directory read signals even when the test runs as root.
             (delete-file (expand-file-name stat-path proc-root))
             (make-directory (expand-file-name stat-path proc-root)))
            ('enum-empty (setq enum nil)))
          (should-not (dorg-git-maintenance--owner-gone-p lock))
          (should-not (dorg-git-maintenance--acquire lock))
          (should (equal token (alist-get :token (dorg-git-maintenance--read-owner lock)))))))))

(ert-deftest dorg-test-git-maintenance-linux-absence-requires-observer-scope ()
  "PID absence is affirmative only after validating the recorded namespace."
  (dorg-git-maintenance-tests--with-proc
    (setq enum (list (1+ pid)))
    (delete-file (expand-file-name (format "%d/ns/pid" pid) proc-root))
    (make-symbolic-link "pid:[5678]" (expand-file-name "self/ns/pid" proc-root) t)
    (should-not (dorg-git-maintenance--owner-gone-p lock))
    (should (= enum-calls 0))
    (make-symbolic-link "pid:[1234]" (expand-file-name "self/ns/pid" proc-root) t)
    (should (dorg-git-maintenance--owner-gone-p lock))
    (should (> enum-calls 0))))

(ert-deftest dorg-test-git-maintenance-linux-reboot-proves-death ()
  "A different valid boot on the same host proves death before PID lookup."
  (dorg-git-maintenance-tests--with-proc
    (dorg-git-maintenance-tests--proc-write
     proc-root "sys/kernel/random/boot_id" "11234567-89ab-cdef-0123-456789abcdef")
    (make-symbolic-link "pid:[5678]" (expand-file-name "self/ns/pid" proc-root) t)
    (should (dorg-git-maintenance--owner-gone-p lock))
    (should (= enum-calls 0))
    (let ((owner (dorg-git-maintenance--read-owner lock)))
      (setf (alist-get :host owner) "foreign-host.invalid")
      (dorg-git-maintenance--put-owner lock owner))
    (should-not (dorg-git-maintenance--owner-gone-p lock))))

(ert-deftest dorg-test-git-maintenance-incompatible-records-preserve ()
  "Legacy, incomplete and cross-platform identities never authorize deletion."
  (dorg-git-maintenance-tests--with-proc
    (let ((original (dorg-git-maintenance--read-owner lock)))
      (dolist (identity (list nil '(:kind unknown)
                             (list :kind 'linux-kernel :boot-id boot :pid-ns "pid:[1234]")
                             '(:kind darwin-time :start (1 2 3 4))))
        (let ((owner (copy-tree original)))
          (setf (alist-get :identity owner) identity
                (alist-get :start owner) '(1 2 3 4))
          (dorg-git-maintenance--put-owner lock owner))
        (setq enum (list (1+ pid)))
        (setq enum-calls 0 attrs-calls 0)
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        ;; A malformed holder must not trigger PID enumeration.  The following
        ;; acquire separately observes this process to build its own record.
        (should (= enum-calls 0))
        (should (= attrs-calls 0))
        (should-not (dorg-git-maintenance--acquire lock)))
      (dorg-git-maintenance--put-owner lock original)
      (let ((system-type 'darwin))
        (setq enum-calls 0 attrs-calls 0)
        (should-not (dorg-git-maintenance--owner-gone-p lock)))
      (should (= enum-calls 0))
      (should (= attrs-calls 0)))))

;;; ─── Debounce and bursts ───────────────────────────────────────────────

(ert-deftest dorg-test-git-maintenance-burst-produces-one-packing-run ()
  "N publications inside one debounce window pack once and no-op N-1 times."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-debounce-seconds 300)
          (packed 0)
          (no-ops 0))
      (dotimes (i 4)
        (let* ((sha (dorg-git-maintenance-tests--commit
                     dir (format "f%d.txt" i) (format "%d\n" i)))
               (report (dorg-git-maintenance-tests--run dir "main" sha)))
          (pcase (plist-get report :status)
            (:packed (cl-incf packed))
            (:no-op (cl-incf no-ops)))))
      (should (= 1 packed))
      (should (= 3 no-ops)))))

(ert-deftest dorg-test-git-maintenance-debounce-reads-owned-pack-mtime ()
  "The debounce clock is the newest owned pack's mtime, not a marker file."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((dorg-git-maintenance-debounce-seconds 300)
           (sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                      dir "main" sha)
                                     :status)))
      (let ((sha2 (dorg-git-maintenance-tests--commit dir "b.txt" "B\n")))
        ;; Within the window: debounced.
        (should (eq :debounced (plist-get (dorg-git-maintenance-tests--run
                                           dir "main" sha2)
                                          :reason)))
        ;; Age the owned pack past the window; the next run proceeds.
        (dolist (pack (dorg-git-maintenance--owned-packs
                       (dorg-git--common-dir dir)))
          (set-file-times pack (time-subtract (current-time) 3600)))
        (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                        dir "main" sha2)
                                       :status)))))))

;;; ─── Owned-pack lifecycle ──────────────────────────────────────────────

(ert-deftest dorg-test-git-maintenance-tolerates-native-repack ()
  "After a native repack deletes every owned pack the next run is ordinary."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (dorg-git-maintenance-tests--run dir "main" sha)
      (should (dorg-git-maintenance--owned-packs (dorg-git--common-dir dir)))
      ;; A native repack folds and deletes the owned pack.
      (dorg-git--run dir "repack" "-a" "-d" "-q")
      (should-not (dorg-git-maintenance--owned-packs (dorg-git--common-dir dir)))
      (let* ((sha2 (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
             (report (dorg-git-maintenance-tests--run dir "main" sha2)))
        (should (eq :packed (plist-get report :status)))))))

(ert-deftest dorg-test-git-maintenance-consolidation-keeps-orphaned-objects ()
  "Consolidation must not lose an object held only by a superseded pack.
The fixture force-moves the branch backwards so an earlier tip is packed,
orphaned and reachable from zero refs — the hazard reproduced in the
scratch-Git control on this bead."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 2)
          (orphan nil))
      ;; Build several owned packs, one of which holds the orphan.
      (dorg-git-maintenance-tests--commit dir "base.txt" "base\n")
      (setq orphan (dorg-git-maintenance-tests--commit dir "gone.txt" "gone\n"))
      (dorg-git-maintenance-tests--run dir "main" orphan)
      ;; Force-move away from the orphan: now nothing references it.
      (dorg-git--run dir "reset" "-q" "--hard" "HEAD~1")
      (should (string= "" (or (dorg-git--ok dir "for-each-ref" "--contains" orphan
                                            "--format=%(refname)")
                              "")))
      (dotimes (i 3)
        (let ((sha (dorg-git-maintenance-tests--commit
                    dir (format "n%d.txt" i) (format "%d\n" i))))
          (dorg-git-maintenance-tests--run dir "main" sha)))
      ;; Whatever consolidation did, the orphan must still resolve.
      (should (dorg-git-maintenance-tests--resolvable-p dir orphan)))))

(ert-deftest dorg-test-git-maintenance-never-deletes-foreign-packs ()
  "Only packs bearing the capability's own prefix are ever candidates."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1))
      (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
      (dorg-git--run dir "repack" "-a" "-d" "-q")
      (let ((native (dorg-git-maintenance-tests--packs dir)))
        (should native)
        (dotimes (i 3)
          (let ((sha (dorg-git-maintenance-tests--commit
                      dir (format "n%d.txt" i) (format "%d\n" i))))
            (dorg-git-maintenance-tests--run dir "main" sha)))
        ;; Every native pack that existed before is still present.
        (dolist (pack native)
          (should (member pack (dorg-git-maintenance-tests--packs dir))))))))

(ert-deftest dorg-test-git-maintenance-failed-verification-removes-nothing ()
  "An injected verification failure during consolidation deletes no pack.
The control asserts consolidation was actually ATTEMPTED — an earlier
version of this test passed only because the bound was never crossed, so
the stubbed verifier never ran."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1))
      ;; One owned pack exists; the next run crosses the bound.
      (let ((sha (dorg-git-maintenance-tests--commit dir "n0.txt" "0\n")))
        (dorg-git-maintenance-tests--run dir "main" sha))
      (let ((before (dorg-git-maintenance-tests--packs dir))
            (sha (dorg-git-maintenance-tests--commit dir "n1.txt" "1\n"))
            (report nil))
        (should (= 1 (length (dorg-git-maintenance--owned-packs
                              (dorg-git--common-dir dir)))))
        (cl-letf (((symbol-function 'dorg-git-maintenance--verify-replacement)
                   (lambda (&rest _) nil)))
          (setq report (dorg-git-maintenance-tests--run dir "main" sha)))
        ;; Consolidation ran and deferred, rather than never running.
        (should (eq :verification-failed
                    (plist-get report :consolidation-deferred)))
        ;; And it removed nothing.
        (dolist (pack before)
          (should (member pack (dorg-git-maintenance-tests--packs dir))))))))

(ert-deftest dorg-test-git-maintenance-stale-keep-blocks-with-a-report ()
  "A stale keep bearing the capability's own name blocks rather than reclaims."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (dorg-git-maintenance-tests--run dir "main" sha)
      (let* ((packs (dorg-git-maintenance--owned-packs
                     (dorg-git--common-dir dir)))
             (keep (concat (file-name-sans-extension (car packs)) ".keep")))
        (with-temp-file keep (insert "dorg-git-maintenance\n"))
        (let* ((sha2 (dorg-git-maintenance-tests--commit dir "b.txt" "B\n"))
               (report (dorg-git-maintenance-tests--run dir "main" sha2)))
          (should (eq :no-op (plist-get report :status)))
          (should (eq :stale-keep (plist-get report :reason)))
          ;; It is reported, not silently reclaimed.
          (should (file-exists-p keep)))))))

;;; ─── Dispatch: the committer dispatches, exactly once ──────────────────

(ert-deftest dorg-test-git-maintenance-dispatch-never-signals ()
  "The writer path is firewalled: dispatch returns nil even when it fails."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
          (dorg-git-maintenance-enabled t))
      (cl-letf (((symbol-function 'dorg-git-maintenance--launch)
                 (lambda (&rest _) (error "boom"))))
        (should-not (dorg-git-maintenance-dispatch
                     dir (dorg-git--common-dir dir) "main" sha))))))

(ert-deftest dorg-test-git-maintenance-dispatch-does-no-work-on-the-writer-path ()
  "Dispatch performs only a bounded launch: no counting, no lock, no pack."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
          (dorg-git-maintenance-enabled t)
          (launched 0)
          (lock-calls 0)
          (acquire-calls 0))
      (cl-letf (((symbol-function 'dorg-git-maintenance--launch)
                 (lambda (&rest _) (cl-incf launched) t))
                ((symbol-function 'dorg-git-maintenance-run)
                 (lambda (&rest _) (error "the writer path must not run the worker")))
                ((symbol-function 'dorg-file-lock-call)
                 (lambda (&rest _) (cl-incf lock-calls) nil))
                ((symbol-function 'dorg-file-lock--acquire)
                 (lambda (&rest _) (cl-incf acquire-calls) nil))
                ((symbol-function 'dorg-git-maintenance--count-loose)
                 (lambda (&rest _) (error "the writer path must not count objects"))))
        (dorg-git-maintenance-dispatch dir (dorg-git--common-dir dir) "main" sha)
        (should (= 1 launched))
        (should (= 0 lock-calls))
        (should (= 0 acquire-calls))))))

(ert-deftest dorg-test-git-maintenance-dispatch-honours-the-disable-switch ()
  "A disabled capability launches nothing, and the switch is validated."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
          (launched 0))
      (cl-letf (((symbol-function 'dorg-git-maintenance--launch)
                 (lambda (&rest _) (cl-incf launched) t)))
        (let ((dorg-git-maintenance-enabled nil))
          (dorg-git-maintenance-dispatch dir (dorg-git--common-dir dir) "main" sha))
        (should (= 0 launched))
        ;; A non-boolean override is invalid and must not enable the launch.
        (let ((dorg-git-maintenance-enabled 'bogus))
          (dorg-git-maintenance-dispatch dir (dorg-git--common-dir dir) "main" sha))
        (should (= 0 launched))
        ;; And the switch is what decides: enabled, the same call launches.
        (let ((dorg-git-maintenance-enabled t))
          (dorg-git-maintenance-dispatch dir (dorg-git--common-dir dir) "main" sha))
        (should (= 1 launched))))))

(ert-deftest dorg-test-git-maintenance-launch-is-asynchronous ()
  "The launch is fire-and-forget: `call-process' destination 0, no wait.
This is the mechanism the never-waits guarantee rests on, asserted
directly so it cannot regress into a synchronous call that merely looks
fast on a fast machine."
  (let ((destination 'unset))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile dest &rest _)
                 (setq destination dest)
                 nil)))
      (dorg-git-maintenance--launch "/tmp" "/tmp/.git" "main" "deadbeef"))
    ;; 0 means "run asynchronously and do not wait for it".
    (should (eq 0 destination))))

(ert-deftest dorg-test-git-maintenance-dispatch-returns-without-waiting ()
  "The writer never waits: a REAL dispatch returns immediately.
Nothing is stubbed — this spawns the actual detached worker, because the
no-blocking claim rests on the launch being asynchronous rather than on a
mock saying so.  It opts back into launching (the suite disables it, see
`dorg-run-tests--setup-environment') and then settles once before the
fixture tears the repository down, so this test cannot leave a worker
racing another test's cleanup."
  (skip-unless (file-executable-p (expand-file-name invocation-name
                                                    invocation-directory)))
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((dorg-git-maintenance-enabled t)
           (sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
           (start (float-time))
           (result (dorg-git-maintenance-dispatch
                    dir (dorg-git--common-dir dir) "main" sha))
           (elapsed (- (float-time) start)))
      ;; Returns nil, and without waiting for a worker that must start a
      ;; whole Emacs of its own.
      (should-not result)
      (should (< elapsed 1.0))
      ;; One bounded settle, not a poll: let the worker finish writing
      ;; before the fixture removes the repository underneath it.
      (sleep-for 2))))

(ert-deftest dorg-test-git-maintenance-defaults-are-production-sized ()
  "The shipped thresholds are the reviewed ones, not the fixtures' values."
  (should (= 256 (default-value 'dorg-git-maintenance-loose-object-threshold)))
  (should (= (* 256 1024 1024)
             (default-value 'dorg-git-maintenance-loose-byte-threshold)))
  (should (= 300 (default-value 'dorg-git-maintenance-debounce-seconds)))
  (should (= 16 (default-value 'dorg-git-maintenance-pack-bound))))


;;; ─── Coverage for the remaining revision-5 criteria ────────────────────

(ert-deftest dorg-test-git-maintenance-consolidation-rewrites-a-multi-pack-index ()
  "A multi-pack-index naming a doomed pack is handled, not left dangling.
After consolidation the store must verify and every object must resolve."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1)
          (shas nil))
      (dotimes (i 2)
        (let ((sha (dorg-git-maintenance-tests--commit
                    dir (format "m%d.txt" i) (format "%d\n" i))))
          (push sha shas)
          (dorg-git-maintenance-tests--run dir "main" sha)))
      ;; Index the packs, then force a consolidation over them.
      (dorg-git--run dir "multi-pack-index" "write")
      (let ((sha (dorg-git-maintenance-tests--commit dir "m2.txt" "2\n")))
        (push sha shas)
        (dorg-git-maintenance-tests--run dir "main" sha))
      ;; The store is intact: every commit resolves and git itself agrees.
      (dolist (sha shas)
        (should (dorg-git-maintenance-tests--resolvable-p dir sha)))
      (should (dorg-git--ok dir "fsck" "--connectivity-only"))
      ;; Any surviving multi-pack-index must verify.
      (when (file-exists-p (expand-file-name
                            "objects/pack/multi-pack-index"
                            (dorg-git--common-dir dir)))
        (should (dorg-git--ok dir "multi-pack-index" "verify"))))))

(ert-deftest dorg-test-git-maintenance-companions-travel-with-their-pack ()
  "A removed pack leaves no orphaned .idx or .rev behind."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1))
      (dotimes (i 3)
        (let ((sha (dorg-git-maintenance-tests--commit
                    dir (format "c%d.txt" i) (format "%d\n" i))))
          (dorg-git-maintenance-tests--run dir "main" sha)))
      (let* ((pack-dir (expand-file-name "objects/pack"
                                         (dorg-git--common-dir dir)))
             (packs (mapcar #'file-name-base
                            (directory-files pack-dir t "\\.pack\\'")))
             (idxs (mapcar #'file-name-base
                           (directory-files pack-dir t "\\.idx\\'")))
             (revs (mapcar #'file-name-base
                           (directory-files pack-dir t "\\.rev\\'"))))
        ;; Every index and .rev belongs to a pack that still exists.
        (dolist (idx idxs) (should (member idx packs)))
        (dolist (rev revs) (should (member rev packs)))))))

(ert-deftest dorg-test-git-maintenance-preserves-everything-across-all-paths ()
  "Refs, reflogs and every pre-run object survive all three paths.
The paths are an ordinary packing run, a consolidation run, and a run
whose consolidation verification is injected to fail.  Each snapshot is
taken immediately around the maintenance call, so only maintenance can
account for a difference."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1)
          (orphan nil)
          (stray nil))
      (dorg-git-maintenance-tests--commit dir "base.txt" "base\n")
      ;; An object reachable from no ref, held only by an owned pack.
      (setq orphan (dorg-git-maintenance-tests--commit dir "gone.txt" "gone\n"))
      (dorg-git-maintenance-tests--run dir "main" orphan)
      (dorg-git--run dir "reset" "-q" "--hard" "HEAD~1")
      ;; An unreachable LOOSE object that no pack holds.
      (with-temp-file (expand-file-name "stray.txt" dir) (insert "stray\n"))
      (setq stray (string-trim
                   (dorg-git--ok dir "hash-object" "-w" "--" "stray.txt")))
      (cl-flet ((run-preserving
                  (sha fail-verification)
                  (let ((refs (dorg-git--ok dir "show-ref"))
                        (reflog (dorg-git--ok dir "reflog" "show" "--all")))
                    (if fail-verification
                        (cl-letf (((symbol-function
                                    'dorg-git-maintenance--verify-replacement)
                                   (lambda (&rest _) nil)))
                          (dorg-git-maintenance-tests--run dir "main" sha))
                      (dorg-git-maintenance-tests--run dir "main" sha))
                    ;; Maintenance itself moved neither refs nor reflogs.
                    (should (equal refs (dorg-git--ok dir "show-ref")))
                    (should (equal reflog
                                   (dorg-git--ok dir "reflog" "show" "--all"))))))
        ;; Path 1 ordinary, path 2 consolidation, path 3 injected failure.
        (run-preserving (dorg-git-maintenance-tests--commit dir "p0.txt" "0\n") nil)
        (run-preserving (dorg-git-maintenance-tests--commit dir "p1.txt" "1\n") nil)
        (run-preserving (dorg-git-maintenance-tests--commit dir "p2.txt" "2\n") t))
      ;; The orphan and the unreachable loose object both survive all three.
      (should (dorg-git-maintenance-tests--resolvable-p dir orphan))
      (should (dorg-git-maintenance-tests--resolvable-p dir stray)))))

(ert-deftest dorg-test-git-maintenance-linked-worktree-uses-the-common-store ()
  "Invoked from a linked worktree, the run packs into the common store."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n"))
          (linked (expand-file-name "linked" (make-temp-file "dorg-gm-lw-" t))))
      (ignore sha)
      (dorg-git--run dir "worktree" "add" "-q" "-b" "linked-branch" linked)
      (let* ((common (dorg-git--common-dir linked))
             (before (dorg-git-maintenance-tests--packs dir))
             (sha2 (dorg-git-maintenance-tests--commit linked "lw.txt" "LW\n"))
             (report (dorg-git-maintenance-run linked common
                                               "linked-branch" sha2)))
        ;; The linked worktree resolves to the SAME common store as the main
        ;; one, and that is where the pack lands.
        (should (equal (file-truename common)
                       (file-truename (dorg-git--common-dir dir))))
        (should (eq :packed (plist-get report :status)))
        (should (> (length (dorg-git-maintenance-tests--packs dir))
                   (length before)))))))

(ert-deftest dorg-test-git-maintenance-owned-pack-count-stays-bounded ()
  "The bound is a real bound: repeated publications do not grow packs forever."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 3))
      (dotimes (i 12)
        (let ((sha (dorg-git-maintenance-tests--commit
                    dir (format "b%d.txt" i) (format "%d\n" i))))
          (dorg-git-maintenance-tests--run dir "main" sha)))
      ;; Twelve publications, and the owned packs never run away.
      (should (<= (length (dorg-git-maintenance--owned-packs
                           (dorg-git--common-dir dir)))
                  (1+ dorg-git-maintenance-pack-bound))))))

(ert-deftest dorg-test-git-maintenance-removes-duplicates-of-any-pack ()
  "The documented breadth: a loose duplicate of ANY pack may be removed.
`prune-packed' is a common-store operation, so it drops the loose copy of
an object some OTHER pack already holds even though that object has
nothing to do with the branch being maintained.  Stating this honestly is
the point: the capability is not surgical about loose removal, it is only
safe — an object no pack holds is never touched."
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "checkout" "-q" "-b" "other")
    (let ((c (dorg-git-maintenance-tests--commit dir "c.txt" "C\n")))
      (dorg-git--run dir "checkout" "-q" "-")
      ;; Pack the other branch's commit WITHOUT removing its loose copies,
      ;; which is how a loose duplicate legitimately arises.
      (let* ((pack-dir (expand-file-name "objects/pack"
                                         (dorg-git--common-dir dir)))
             (base (expand-file-name "foreign" pack-dir)))
        (with-temp-buffer
          (insert c "\n")
          (call-process-region (point-min) (point-max) "git" t t nil
                               "-C" (expand-file-name dir)
                               "pack-objects" "--revs" "--non-empty" base)))
      ;; The loose copy is still there and now duplicates a pack.
      (should (member c (dorg-git-maintenance-tests--loose-objects dir)))
      (let ((sha (dorg-git-maintenance-tests--commit dir "b.txt" "B\n")))
        (dorg-git-maintenance-tests--run dir "main" sha))
      ;; Removed as a duplicate of a pack unrelated to the captured branch,
      ;; and still perfectly resolvable.
      (should-not (member c (dorg-git-maintenance-tests--loose-objects dir)))
      (should (dorg-git-maintenance-tests--resolvable-p dir c)))))


;;; ─── Review findings, 2026-09-23 (nav 17:03, arc-6lqn 17:07) ───────────

(defmacro dorg-git-maintenance-tests--observing (enum attrs &rest body)
  "Run BODY through the Darwin observer, counting both platform probes.
ENUM is the `list-system-processes' result or a function; ATTRS is the
`process-attributes' result, a function, or the symbol `error' to signal.
Binds `enum-calls' and `attrs-calls' so a control can assert the
OBSERVATION PRECONDITION itself, not merely the answer that came back —
a scenario whose probe was never consulted proves nothing about it."
  (declare (indent 2) (debug (form form body)))
  `(let ((system-type 'darwin) (enum-calls 0) (attrs-calls 0))
     (cl-letf (((symbol-function 'list-system-processes)
                (lambda () (cl-incf enum-calls)
                  (if (functionp ,enum) (funcall ,enum) ,enum)))
               ((symbol-function 'process-attributes)
                (lambda (_pid) (cl-incf attrs-calls)
                  (cond ((and (symbolp ,attrs) (eq ,attrs 'error))
                         (error "observation failed"))
                        ((functionp ,attrs) (funcall ,attrs))
                        (t ,attrs)))))
       ,@body)))

(ert-deftest dorg-test-git-maintenance-identity-value-boundaries ()
  "Presence of a start value is not identity, and equality must be semantic.
The two failures nav reproduced both authorised reclaim of a LIVE
holder's lock: an observed attribute record whose start cell exists but
holds nil became a valid-looking \"different\" time, and a malformed
RECORDED start compared unequal to a perfectly good observed one.  Each
read as a reused pid.  Meanwhile Lisp times have several equivalent
spellings, so structural equality would call one instant two.

The table is the contract: RECORDED, OBSERVED, and whether the owner is
GONE.  Every invalid value on either side must answer nil — preserve the
lock — and only two genuinely valid, genuinely different times may say t."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (valid '(24623 12345 0 0))
           (same-instant '(24623 12345))      ; the same time, spelled shorter
           (other '(24623 99999 0 0)))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (cl-flet ((gone-with
                  (recorded observed)
                  (let ((owner (dorg-git-maintenance--read-owner lock)))
                    (setf (alist-get :pid owner) 4242)
                    (setf (alist-get :identity owner) (list :kind 'darwin-time :start recorded))
                    (dorg-git-maintenance--put-owner lock owner))
                  (dorg-git-maintenance-tests--observing
                      '(4242) (list (cons 'start observed))
                    (dorg-git-maintenance--owner-gone-p lock))))
        ;; nav failure 1: the start CELL exists but holds nil.
        (should-not (gone-with valid nil))
        ;; nav failure 2: a malformed non-nil RECORDED start.
        (should-not (gone-with 'bogus valid))
        ;; Both sides malformed is still unknown, not death.
        (should-not (gone-with 'bogus 'bogus))
        ;; Equivalent representations denote ONE instant, so the owner lives.
        (should-not (gone-with valid same-instant))
        (should-not (gone-with same-instant valid))
        ;; Two genuinely valid, genuinely different times: reuse.
        (should (gone-with valid other))
        ;; NONFINITE values are not times.  `float-time' accepts NaN and the
        ;; infinities, and a NaN compares unequal to everything including
        ;; itself, so admitting one would manufacture the very difference
        ;; that authorises reclaim.  `time-convert' rejects them.
        (dolist (nonfinite (list 0.0e+NaN 1.0e+INF -1.0e+INF))
          (should-not (dorg-git-maintenance--valid-time-p nonfinite))
          (should-not (gone-with valid nonfinite))
          (should-not (gone-with nonfinite valid))))
      ;; A comparator that SIGNALS on values that passed validation is no
      ;; evidence of a different instant.  Inequality is authorised only
      ;; after a comparison that succeeded.
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 4242)
        (setf (alist-get :identity owner) (list :kind 'darwin-time :start valid))
        (dorg-git-maintenance--put-owner lock owner))
      (cl-letf (((symbol-function 'time-equal-p)
                 (lambda (&rest _) (error "comparison failed"))))
        (should (eq :unknown (dorg-git-maintenance--compare-times valid other)))
        (dorg-git-maintenance-tests--observing '(4242) (list (cons 'start other))
          (should-not (dorg-git-maintenance--owner-gone-p lock))))
      ;; A recorded pid that is not a positive integer is never identity.
      (dolist (bad '(0 -1 "4242" nil))
        (let ((owner (dorg-git-maintenance--read-owner lock)))
          (setf (alist-get :pid owner) bad)
          (setf (alist-get :identity owner) (list :kind 'darwin-time :start valid))
          (dorg-git-maintenance--put-owner lock owner))
        (should-not (dorg-git-maintenance--owner-gone-p lock))))))

(ert-deftest dorg-test-git-maintenance-observation-failure-is-never-death ()
  "Every failed or partial observation is UNKNOWN and preserves the lock.
Folding these into \"absent\" is what lets a live holder's lock be taken:
an enumeration that failed is not an empty world, and an attribute record
without a start time is an INCOMPLETE identity, not a different one."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common)))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 4242)
        (setf (alist-get :identity owner) (list :kind 'darwin-time :start '(1 2 3 4)))
        (dorg-git-maintenance--put-owner lock owner))
      ;; 1. Enumeration unavailable or unsupported.
      (dorg-git-maintenance-tests--observing nil nil
        (should (eq :unknown (car (dorg-git-maintenance--process-identity 4242))))
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        (should (> enum-calls 0)))
      ;; 2. Attributes signal an error.
      (dorg-git-maintenance-tests--observing '(4242) 'error
        (should (eq :unknown (car (dorg-git-maintenance--process-identity 4242))))
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        (should (> attrs-calls 0)))
      ;; 3. Attributes come back empty.
      (dorg-git-maintenance-tests--observing '(4242) nil
        (should (eq :unknown (car (dorg-git-maintenance--process-identity 4242))))
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        (should (> attrs-calls 0)))
      ;; 4. The platform supplied no start time: incomplete, not different.
      (dorg-git-maintenance-tests--observing '(4242) '((comm . "x"))
        (should (eq :unknown (car (dorg-git-maintenance--process-identity 4242))))
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        (should (> attrs-calls 0))))))

(ert-deftest dorg-test-git-maintenance-live-identity-is-never-gone ()
  "A present pid whose observed start matches is alive, and stays alive."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common)))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 4242)
        (setf (alist-get :identity owner) (list :kind 'darwin-time :start '(1 2 3 4)))
        (dorg-git-maintenance--put-owner lock owner))
      (dorg-git-maintenance-tests--observing '(4242) '((start . (1 2 3 4)))
        (let ((identity (dorg-git-maintenance--process-identity 4242)))
          (should (eq :present (car identity)))
          (should (eq 'darwin-time (plist-get (cadr identity) :kind)))
          (should (equal '(1 2 3 4) (plist-get (cadr identity) :start))))
        (should-not (dorg-git-maintenance--owner-gone-p lock))
        (should (> enum-calls 0))
        (should (> attrs-calls 0))
        ;; And the lock is genuinely kept, not merely judged kept.
        (should-not (dorg-git-maintenance--acquire lock))))))

(ert-deftest dorg-test-git-maintenance-absence-and-reuse-are-the-only-deaths ()
  "The two affirmative deaths, each requiring a successful observation.
Absence requires an enumeration that SUCCEEDED and did not contain the
pid; reuse requires a start identity that was positively observed, valid,
and different."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common)))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 4242)
        (setf (alist-get :identity owner) (list :kind 'darwin-time :start '(1 2 3 4)))
        (dorg-git-maintenance--put-owner lock owner))
      ;; Known absent: enumeration succeeded and 4242 is not in it.
      (dorg-git-maintenance-tests--observing '(1 2 3) nil
        (should (eq :absent (car (dorg-git-maintenance--process-identity 4242))))
        (should (dorg-git-maintenance--owner-gone-p lock))
        (should (> enum-calls 0))
        ;; Absence is decided without needing attributes at all.
        (should (= 0 attrs-calls)))
      ;; Reused pid: present, with a valid start that differs.
      (dorg-git-maintenance-tests--observing '(4242) '((start . (9 9 9 9)))
        (let ((identity (dorg-git-maintenance--process-identity 4242)))
          (should (eq :present (car identity)))
          (should (eq 'darwin-time (plist-get (cadr identity) :kind)))
          (should (equal '(9 9 9 9) (plist-get (cadr identity) :start))))
        (should (dorg-git-maintenance--owner-gone-p lock))
        (should (> attrs-calls 0))))))

(ert-deftest dorg-test-git-maintenance-ownerless-lock-is-never-reclaimed ()
  "An ownerless lock is UNKNOWN, not dead, and age does not change that.
It is normally a lock being taken this instant, between the directory
creation and the owner write.  A live process stopped between those two
steps can exceed any age bound, so no bound can distinguish it from a
crash and none is used: the lock is simply never reclaimed."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (make-directory lock t)
      (should-not (dorg-git-maintenance--owner-gone-p lock))
      (should-not (dorg-git-maintenance--acquire lock))
      ;; Backdate it far past any plausible grace bound: still not dead.
      (set-file-times lock (time-subtract (current-time) 864000))
      (should-not (dorg-git-maintenance--owner-gone-p lock))
      (should-not (dorg-git-maintenance--acquire lock))
      (should (eq :lock-held (plist-get (dorg-git-maintenance-tests--run
                                         dir "main" sha)
                                        :reason)))
      ;; Malformed ownership is unknown too, never dead.
      (dorg-git-maintenance--put-owner lock '((:host . "nowhere")))
      (should-not (dorg-git-maintenance--owner-gone-p lock)))))

(ert-deftest dorg-test-git-maintenance-loser-cannot-steal-the-successor-lock ()
  "The ABA: a second contender must not remove the winner's NEW live lock.
Both contenders can prove the ORIGINAL owner dead before either acts.  The
first then completes a full reclaim — removes the dead lock, recreates the
canonical one and publishes its own owner — and only afterwards does the
second proceed.  With per-caller aside names the second moved that live
lock away and both entered; the election on one fixed tombstone plus a
compare-and-swap on the observed owner is what makes the second lose."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (winner nil)
           (interposed nil))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 999999999)
        (dorg-git-maintenance--put-owner lock owner))
      (should (dorg-git-maintenance--owner-gone-p lock))
      (let ((real (symbol-function 'dorg-file-lock--owner-gone-p)))
        (cl-letf (((symbol-function 'dorg-file-lock--owner-gone-p)
                   (lambda (l)
                     (let ((gone (funcall real l)))
                       (when (and gone (not interposed))
                         (setq interposed t)
                         (setq winner (dorg-git-maintenance--acquire lock)))
                       gone))))
          (let ((loser (dorg-git-maintenance--acquire lock)))
            (should interposed)
            (should winner)
            (should-not loser)
            (should (file-symlink-p lock))
            (should (equal winner
                           (alist-get :token
                                      (dorg-git-maintenance--read-owner lock))))))))))

(ert-deftest dorg-test-git-maintenance-tombstone-serialises-read-to-delete ()
  "The fixed tombstone, not the CAS, is what closes the last window.
A contender's compare-and-swap read and the delete that follows it are
not atomic.  If a rival completes an entire reclaim in between — removing
the dead lock, recreating the canonical one and publishing its own owner
— then a contender still holding a STALE view proceeds to delete that
rival's LIVE lock.  The compare-and-swap cannot see it, because the view
it compares is exactly the stale one.

The seam is built deliberately: the rival's full acquire is run inside
the contender's CAS read, and the read then returns the stale owner.
With ONE FIXED tombstone the rival cannot acquire it at all while the
contender holds it, so it never publishes a live lock and nothing live is
destroyed.  With per-caller names the rival succeeds and its lock is
deleted underneath it — which is the ABA this design exists to prevent."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (fired nil)
           (inside nil)
           (stale nil)
           (rival 'unset))
      ;; A lock whose owner is provably dead.
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 999999999)
        (dorg-git-maintenance--put-owner lock owner))
      (setq stale (dorg-git-maintenance--read-owner lock))
      (should (dorg-git-maintenance--owner-gone-p lock))
      (let ((real (symbol-function 'dorg-file-lock--read-owner)))
        (cl-letf (((symbol-function 'dorg-file-lock--read-owner)
                   (lambda (l)
                     (cond
                      ;; The rival's own reads are honest.
                      (inside (funcall real l))
                      ;; The window is defined by the tombstone being HELD:
                      ;; the first read taken under it is the compare-and-swap.
                      ;; Keying on that rather than on a call index keeps this
                      ;; control independent of how many reads precede it.
                      ((and (not fired)
                            (file-exists-p (dorg-git-maintenance--tombstone l)))
                       (setq fired t)
                       ;; SETQ, not a fresh binding: a `let' here would
                       ;; shadow the captured variable and the rival's reads
                       ;; would silently go on being answered with the stale
                       ;; owner, so the control would pass for a reason other
                       ;; than the one it claims.  The byte-compiler caught
                       ;; exactly that.
                       (unwind-protect
                           (progn (setq inside t)
                                  (setq rival
                                        (dorg-git-maintenance--acquire lock)))
                         (setq inside nil))
                       stale)
                      ;; ...and the contender goes on seeing the old world.
                      (fired stale)
                      (t (funcall real l))))))
          (let ((contender (dorg-git-maintenance--acquire lock)))
            (should fired)
            ;; The rival could not hold the election at all: the contender
            ;; has the one fixed tombstone, so no live lock was ever
            ;; published for the contender to destroy.
            (should-not rival)
            (should contender)
            (should (file-symlink-p lock))
            (should (equal contender
                           (alist-get :token
                                      (funcall real lock))))))))))

(ert-deftest dorg-test-git-maintenance-blocked-election-fails-closed ()
  "A tombstone left by a reclaimer that died blocks reclaim; it never steals."
  (dorg-git-maintenance-tests--with-repo dir
    (let* ((common (dorg-git--common-dir dir))
           (lock (dorg-git-maintenance--lock-dir common))
           (tomb (dorg-git-maintenance--tombstone lock))
           (sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (make-directory lock t)
      (dorg-git-maintenance--write-owner lock)
      (let ((owner (dorg-git-maintenance--read-owner lock)))
        (setf (alist-get :pid owner) 999999999)
        (dorg-git-maintenance--put-owner lock owner))
      (should (dorg-git-maintenance--owner-gone-p lock))
      (make-directory tomb t)
      (should-not (dorg-git-maintenance--acquire lock))
      (should (file-directory-p lock))
      (should (eq :reclaim-blocked (plist-get (dorg-git-maintenance-tests--run
                                               dir "main" sha)
                                              :reason))))))

(ert-deftest dorg-test-git-maintenance-unreadable-pack-defers-consolidation ()
  "An unenumerable superseded pack defers; it is never deleted blind.
A pack whose index cannot be read used to answer the same nil as an
empty one, so its objects were omitted from the union, the replacement
was verified against that incomplete union, and the unreadable pack was
deleted — losing whatever only it held.

The failure is injected at the enumeration seam rather than by corrupting
an index on disk: a corrupt index makes git's own `cat-file' fail, so the
run bails at :not-a-commit and never reaches consolidation at all.  That
is safe, but it cannot exercise this path."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1)
          (orphan nil)
          (doomed nil))
      (dorg-git-maintenance-tests--commit dir "base.txt" "base\n")
      ;; An object reachable from no ref, held only by this owned pack.
      (setq orphan (dorg-git-maintenance-tests--commit dir "gone.txt" "gone\n"))
      (dorg-git-maintenance-tests--run dir "main" orphan)
      (dorg-git--run dir "reset" "-q" "--hard" "HEAD~1")
      (setq doomed (car (dorg-git-maintenance--owned-packs
                         (dorg-git--common-dir dir))))
      (should doomed)
      (let ((before (dorg-git-maintenance-tests--packs dir))
            (real (symbol-function 'dorg-git-maintenance--pack-objects-in))
            (sha (dorg-git-maintenance-tests--commit dir "n.txt" "n\n"))
            (report nil))
        ;; Exactly one pack becomes unenumerable: the one holding the orphan.
        (cl-letf (((symbol-function 'dorg-git-maintenance--pack-objects-in)
                   (lambda (d pack)
                     (if (equal pack doomed)
                         (list :failed :unreadable-index)
                       (funcall real d pack)))))
          (setq report (dorg-git-maintenance-tests--run dir "main" sha)))
        ;; It deferred, naming the reason, and removed nothing.
        (should (eq :unreadable-index
                    (plist-get report :consolidation-deferred)))
        (dolist (pack before)
          (should (member pack (dorg-git-maintenance-tests--packs dir))))
        ;; The orphan only that pack holds is still resolvable.
        (should (dorg-git-maintenance-tests--resolvable-p dir orphan))))))

(ert-deftest dorg-test-git-maintenance-consolidation-excludes-native-packs ()
  "The replacement never copies an object a native pack already holds.
Including the reachable set would duplicate natively packed objects into
the owned replacement, turning each consolidation into a full-branch
repack and adding exactly the growth this capability exists to bound."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1)
          (native-objects nil))
      (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
      (dorg-git--run dir "repack" "-a" "-d" "-q")
      (let ((native (dorg-git-maintenance-tests--packs dir)))
        (should (= 1 (length native)))
        (setq native-objects (dorg-git-maintenance-tests--pack-objects
                              dir (car native)))
        (should native-objects)
        (dotimes (i 3)
          (let ((sha (dorg-git-maintenance-tests--commit
                      dir (format "n%d.txt" i) (format "%d\n" i))))
            (dorg-git-maintenance-tests--run dir "main" sha)))
        ;; No owned pack may contain any object the native pack holds.
        (dolist (owned (dorg-git-maintenance--owned-packs
                        (dorg-git--common-dir dir)))
          (let ((objects (dorg-git-maintenance--pack-objects-in dir owned)))
            (should (eq :ok (car objects)))
            (should-not (cl-intersection (cadr objects) native-objects
                                         :test #'equal))))))))

(ert-deftest dorg-test-git-maintenance-multi-pack-index-defers-consolidation ()
  "A multi-pack-index stands consolidation down rather than being broken.
Rewriting an index correctly before its packs are removed is not
implemented, so the capability defers instead of deleting packs the
index still references."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-pack-bound 1))
      (dotimes (i 2)
        (let ((sha (dorg-git-maintenance-tests--commit
                    dir (format "m%d.txt" i) (format "%d\n" i))))
          (dorg-git-maintenance-tests--run dir "main" sha)))
      (dorg-git--run dir "multi-pack-index" "write")
      (should (file-exists-p (expand-file-name
                              "objects/pack/multi-pack-index"
                              (dorg-git--common-dir dir))))
      (let* ((before (dorg-git-maintenance-tests--packs dir))
             (sha (dorg-git-maintenance-tests--commit dir "m2.txt" "2\n"))
             (report (dorg-git-maintenance-tests--run dir "main" sha)))
        (should (eq :multi-pack-index
                    (plist-get report :consolidation-deferred)))
        (dolist (pack before)
          (should (member pack (dorg-git-maintenance-tests--packs dir))))
        (should (dorg-git--ok dir "multi-pack-index" "verify"))))))

(ert-deftest dorg-test-git-maintenance-deletion-namespace-is-fixed ()
  "The reserved namespace is a constant, not configuration.
Were it settable, \"pack\" would classify git's own pack-*.pack as owned
and eligible for deletion, and a value with a separator would move the
pack-objects output base outside objects/pack."
  (should (equal "dorg-sync" dorg-git-maintenance-pack-prefix))
  ;; A constant, not a user option.  `custom-variable-p' is the precise
  ;; predicate: it is non-nil for anything customize will offer to set,
  ;; which is exactly what must never be true of a deletion namespace.
  (should-not (custom-variable-p 'dorg-git-maintenance-pack-prefix))
  (should-not (get 'dorg-git-maintenance-pack-prefix 'standard-value))
  (should-not (string-match-p "/" dorg-git-maintenance-pack-prefix))
  (dorg-git-maintenance-tests--with-repo dir
    (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")
    (dorg-git--run dir "repack" "-a" "-d" "-q")
    ;; Native packs are never classified as owned.
    (should (dorg-git-maintenance-tests--packs dir))
    (should-not (dorg-git-maintenance--owned-packs (dorg-git--common-dir dir)))))

(ert-deftest dorg-test-git-maintenance-byte-threshold-triggers-packing ()
  "Bytes are a trigger in their own right, not decoration.
Object count stays BELOW its threshold while the byte total is above
its own, and the run must still pack."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-loose-object-threshold 1000)
          (dorg-git-maintenance-loose-byte-threshold 1024)
          (dorg-git-maintenance-debounce-seconds 0))
      ;; A handful of large objects, few enough to stay under the count.
      ;; The content must be INCOMPRESSIBLE: loose objects are zlib-deflated,
      ;; so 40000 identical characters occupy under 400 bytes on disk and
      ;; would never reach a byte threshold.
      (let ((sha (dorg-git-maintenance-tests--commit
                  dir "big.txt"
                  (let ((s (make-string 40000 ?a)) (seed 12345))
                    (dotimes (i 40000)
                      (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
                      (aset s i (+ 32 (mod seed 95))))
                    s))))
        (should (< (car (dorg-git-maintenance--count-loose
                         (dorg-git--common-dir dir)))
                   dorg-git-maintenance-loose-object-threshold))
        (should (> (cdr (dorg-git-maintenance--count-loose
                         (dorg-git--common-dir dir)))
                   dorg-git-maintenance-loose-byte-threshold))
        (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                        dir "main" sha)
                                       :status)))))))

(ert-deftest dorg-test-git-maintenance-debounced-run-counts-nothing ()
  "A debounced no-op does not walk the object directories.
The count used to be computed before the debounce test, so the common
no-op scanned 256 directories it had already decided not to touch."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((dorg-git-maintenance-debounce-seconds 300)
          (sha (dorg-git-maintenance-tests--commit dir "a.txt" "A\n")))
      (should (eq :packed (plist-get (dorg-git-maintenance-tests--run
                                      dir "main" sha)
                                     :status)))
      ;; Now debounced: counting at all is a failure.
      (cl-letf (((symbol-function 'dorg-git-maintenance--count-loose)
                 (lambda (&rest _)
                   (error "the debounced path must not count objects"))))
        (should (eq :debounced (plist-get (dorg-git-maintenance-tests--run
                                           dir "main" sha)
                                          :reason)))))))

(ert-deftest dorg-test-git-maintenance-worker-receives-configuration ()
  "The detached worker is told the caller's tunables, not -Q defaults.
It starts from -Q and loads only this module, so without explicit
transmission it would silently run on compiled defaults."
  (let ((argv nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _dest _display &rest args)
                 (setq argv args) nil)))
      (let ((dorg-git-maintenance-loose-object-threshold 7)
            (dorg-git-maintenance-debounce-seconds 11)
            (dorg-git-maintenance-pack-bound 3))
        (dorg-git-maintenance--launch "/tmp" "/tmp/.git" "main" "deadbeef")))
    (let ((form (car (last argv))))
      (should (string-match-p "dorg-git-maintenance-loose-object-threshold 7" form))
      (should (string-match-p "dorg-git-maintenance-debounce-seconds 11" form))
      (should (string-match-p "dorg-git-maintenance-pack-bound 3" form))
      ;; The identity tuple is still carried as its own four values.
      (should (string-match-p "\"main\"" form))
      (should (string-match-p "\"deadbeef\"" form)))))

(ert-deftest dorg-test-git-maintenance-report-is-recordable-and-off-by-default ()
  "Named deferrals are observable when asked for, and silent otherwise."
  (dorg-git-maintenance-tests--with-repo dir
    (let ((log (expand-file-name "report.log" dir)))
      ;; Default: nothing is written anywhere.
      (dorg-git-maintenance--record (list :status :no-op :reason :debounced))
      (should-not (file-exists-p log))
      ;; Opted in: the report, including its reason, lands in the file.
      (let ((dorg-git-maintenance-log-file log))
        (dorg-git-maintenance--record
         (list :status :no-op :consolidation-deferred :unreadable-index)))
      (should (file-exists-p log))
      (with-temp-buffer
        (insert-file-contents log)
        (should (string-match-p ":unreadable-index" (buffer-string)))))))

(provide 'dorg-git-maintenance-tests)
;;; dorg-git-maintenance-tests.el ends here
