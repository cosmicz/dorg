;;; dorg-file-lock.el --- Local file ownership and exclusive gates -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; This file is not part of GNU Emacs.

;;; Commentary:

;; One local-file lock implementation for saved plain-Org writes and git object
;; maintenance.  A symlink atomically publishes a complete owner record; an
;; owner is reclaimed only after affirmative process-birth proof.  The lock
;; name is selected by each client.  No backend or tracker knowledge lives here.
;;
;; A lock is a dangling symlink whose target is metadata, not a path.  Every
;; lock-path probe therefore uses `file-symlink-p' or a no-follow equivalent;
;; `file-exists-p' would incorrectly report the held lock as absent.
;; The old maintenance mkdir lock and this symlink use the same pathname.
;; Old code sees a symlink as an ownerless directory lock and fails closed;
;; new code can inspect and safely reclaim a proven-dead old directory owner.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst dorg-file-lock-default-timeout 2.0
  "Default seconds to wait for a local file lock.")

(defconst dorg-file-lock--record-prefix "dorg-lock-v1:"
  "Versioned, non-path prefix for an atomic symlink owner record.")

(defconst dorg-file-lock--max-record-bytes 512
  "Upper bound on an encoded owner record, below local PATH_MAX values.")

(defun dorg-file-lock--path-from-canonical (canonical)
  "Return the lock path adjacent to CANONICAL without resolving it again."
  (expand-file-name
   (concat "." (file-name-nondirectory canonical) ".dorg-write.lock")
   (file-name-directory canonical)))

(defun dorg-file-lock-path (file)
  "Return FILE's literal hidden sibling writer-lock path at its truename."
  (dorg-file-lock--path-from-canonical (file-truename file)))

(defun dorg-file-lock--host-name ()
  "Return this host's name; this seam supports record encoding controls."
  (system-name))

(defun dorg-file-lock--new-token ()
  "Return a token no other acquisition will reproduce."
  (format "%d-%s-%d" (emacs-pid) (float-time)
          (random (max 1 most-positive-fixnum))))

(defun dorg-file-lock--valid-time-p (value)
  "Return non-nil when VALUE is a CONVERSION-valid Lisp time.
Not merely something `float-time' will accept: that function also takes
NaN and the infinities, and a NaN compares unequal to everything
including itself, so admitting one would manufacture the very
\"different time\" that authorises reclaiming a live holder's lock.
`time-convert' rejects them, so conversion is the test.

nil is rejected explicitly as well, because `float-time' reads nil as the
current time and would turn a missing identity into a plausible one."
  (and value
       (not (and (floatp value)
                 (or (isnan value)
                     (= value 1.0e+INF)
                     (= value -1.0e+INF))))
       (condition-case nil
           (if (fboundp 'time-convert)
               (consp (time-convert value 'list))
             (numberp (float-time value)))
         (error nil))))

(defun dorg-file-lock--compare-times (a b)
  "Compare times A and B; return `:same', `:different' or `:unknown'.
Inequality is only ever reported after a comparison that SUCCEEDED.  The
comparator itself can signal on inputs that survived validation, and an
error is not evidence of a different instant — it is no evidence at all,
so it answers :unknown and the caller preserves the lock.

Comparison is semantic because Lisp times have several equivalent
spellings, and structural equality would report one instant as two."
  (if (not (and (dorg-file-lock--valid-time-p a)
                (dorg-file-lock--valid-time-p b)))
      :unknown
    (condition-case nil
        (if (if (fboundp 'time-equal-p)
                (time-equal-p a b)
              (= (float-time a) (float-time b)))
            :same
          :different)
      (error :unknown))))

(defun dorg-file-lock--linux-scope (&optional proc-root)
  "Read Linux boot and observer PID namespace under PROC-ROOT, or nil."
  (condition-case nil
      (let* ((root (or proc-root "/proc/"))
             (boot (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "sys/kernel/random/boot_id" root))
                     (string-trim (buffer-string))))
             (ns (file-symlink-p (expand-file-name "self/ns/pid" root))))
        (when (and (string-match-p
                    "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'" boot)
                   (stringp ns)
                   (string-match-p "\\`pid:\\[[0-9]+\\]\\'" ns))
          (list :kind 'linux-kernel :boot-id (downcase boot) :pid-ns ns)))
    (error nil)))

(defun dorg-file-lock--linux-identity (pid scope &optional proc-root)
  "Read PID's kernel birth identity in SCOPE under PROC-ROOT, or nil.
Field 22 is counted after the final closing parenthesis of comm; names
can contain both whitespace and parentheses.  Never derive wall time."
  (condition-case nil
      (let* ((root (or proc-root "/proc/"))
             (ns (file-symlink-p
                  (expand-file-name (format "%d/ns/pid" pid) root))))
        (when (and scope (equal ns (plist-get scope :pid-ns)))
          (with-temp-buffer
            (insert-file-contents (expand-file-name (format "%d/stat" pid) root))
            (goto-char (point-min))
            (when (looking-at (format "%d (" pid))
              (goto-char (point-max))
              (when (search-backward ") " nil t)
                (let* ((fields (split-string (buffer-substring-no-properties
                                             (+ (point) 2) (point-max))))
                       (ticks (nth 19 fields)))
                  (when (and (string-match-p "\\`[[:alpha:]]\\'" (or (car fields) ""))
                             (stringp ticks)
                             (string-match-p "\\`[0-9]+\\'" ticks))
                    (append scope (list :start-ticks (string-to-number ticks))))))))))
    (error nil)))

(defun dorg-file-lock--valid-identity-p (identity)
  "Return non-nil when IDENTITY has a known kind and complete birth evidence."
  (condition-case nil
      (and (proper-list-p identity)
           (pcase (plist-get identity :kind)
             ('darwin-time
              (dorg-file-lock--valid-time-p (plist-get identity :start)))
             ('linux-kernel
              (let ((boot (plist-get identity :boot-id))
                    (ns (plist-get identity :pid-ns))
                    (ticks (plist-get identity :start-ticks)))
                (and (stringp boot)
                     (string-match-p
                      "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'" boot)
                     (stringp ns) (string-match-p "\\`pid:\\[[0-9]+\\]\\'" ns)
                     (integerp ticks) (>= ticks 0))))))
    (error nil)))

(defun dorg-file-lock--process-identity (pid &optional recorded)
  "Observe PID; return (:absent), (:present IDENTITY), or (:unknown).
RECORDED, when non-nil, supplies the prior birth identity.  On Linux,
validate the observer's namespace before inferring PID absence or reuse.
A valid different boot on the recorded host proves death independently
of enumeration.  Unsupported platforms and incomplete observations are
unknown.  The detached worker loads no Dolt modules."
  (condition-case nil
      (let* ((kind (pcase system-type
                     ('gnu/linux 'linux-kernel) ('darwin 'darwin-time)))
             (scope (and (eq kind 'linux-kernel)
                         (dorg-file-lock--linux-scope))))
        (cond
         ((or (null kind)
              (and recorded
                   (or (not (dorg-file-lock--valid-identity-p recorded))
                       (not (eq kind (plist-get recorded :kind))))))
          (list :unknown))
         ((and (eq kind 'linux-kernel) (null scope)) (list :unknown))
         ((and scope recorded
               (not (equal (plist-get scope :boot-id)
                           (downcase (plist-get recorded :boot-id)))))
          (list :absent))
         ((and scope recorded
               (not (equal (plist-get scope :pid-ns)
                           (plist-get recorded :pid-ns))))
          (list :unknown))
         (t
          (let ((all (list-system-processes)))
            (cond
             ((null all) (list :unknown))
             ((not (memq pid all)) (list :absent))
             (t
              (let ((identity
                     (if scope
                         (dorg-file-lock--linux-identity pid scope)
                       (list :kind 'darwin-time
                             :start (cdr (assq 'start (process-attributes pid)))))))
                (if (dorg-file-lock--valid-identity-p identity)
                    (list :present identity)
                  (list :unknown)))))))))
    (error (list :unknown))))


(defun dorg-file-lock--owner (&optional token)
  "Return this process's complete owner record with TOKEN."
  (list (cons :host (dorg-file-lock--host-name))
        (cons :pid (emacs-pid))
        (cons :identity (cadr (dorg-file-lock--process-identity (emacs-pid))))
        (cons :token (or token (dorg-file-lock--new-token)))
        (cons :acquired-at (float-time))))

(defun dorg-file-lock--encode-owner (owner)
  "Encode OWNER as a bounded ASCII symlink target."
  (let ((record
         (concat dorg-file-lock--record-prefix
                 (base64-encode-string
                  (encode-coding-string (prin1-to-string owner) 'utf-8-unix) t))))
    (unless (<= (string-bytes record) dorg-file-lock--max-record-bytes)
      (error "dorg file-lock owner record exceeds local symlink bound"))
    record))

(defun dorg-file-lock--decode-owner (record)
  "Decode versioned symlink RECORD, or nil for an unrecognized record."
  (when (and (stringp record)
             (string-prefix-p dorg-file-lock--record-prefix record)
             (<= (string-bytes record) dorg-file-lock--max-record-bytes))
    (condition-case nil
        (let* ((bytes (base64-decode-string
                       (substring record (length dorg-file-lock--record-prefix))))
               (text (decode-coding-string bytes 'utf-8-unix))
               (parsed (read-from-string text)))
          (when (and (string-blank-p (substring text (cdr parsed)))
                     (proper-list-p (car parsed)))
            (car parsed)))
      (error nil))))

(defun dorg-file-lock--present-p (lock)
  "Return non-nil when LOCK names any filesystem entry, including a dangling link."
  (file-attributes lock))

(defun dorg-file-lock--legacy-owner-file (lock)
  "Return the legacy maintenance owner path inside directory LOCK."
  (expand-file-name "owner" lock))

(defun dorg-file-lock--put-legacy-owner (lock owner)
  "Write OWNER in old directory LOCK for migration controls only."
  (let* ((file (dorg-file-lock--legacy-owner-file lock))
         (tmp (concat file ".tmp")))
    (with-temp-file tmp (prin1 owner (current-buffer)))
    (rename-file tmp file t)
    owner))

(defun dorg-file-lock--read-owner (lock)
  "Return LOCK's owner alist, reading atomic links or legacy directories.
Never follow a symlink at LOCK or at a legacy owner path."
  (let* ((attributes (file-attributes lock))
         (link (and (stringp (car attributes)) (car attributes))))
    (cond
     (link (dorg-file-lock--decode-owner link))
     ((eq (car attributes) t)
      (let ((owner-file (dorg-file-lock--legacy-owner-file lock)))
        (when (and (not (file-symlink-p owner-file))
                   (file-readable-p owner-file))
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents-literally owner-file)
                (let ((owner (read (current-buffer))))
                  (and (proper-list-p owner) owner)))
            (error nil)))))
     (t nil))))

(defun dorg-file-lock--owner-gone-p (lock)
  "Return non-nil only on AFFIRMATIVE proof LOCK's owner is gone.
Death must be observed, never inferred, and a failed or partial
observation is not an observation of absence.

GONE means a validated Linux reboot, PID absence in the recorded scope,
or a valid different birth identity for a present PID in that scope.
Identities are compared only within one kind: kernel ticks on Linux,
semantic kernel start times on Darwin.  Namespace mismatches and legacy
untagged records are unknown, even if their PID looks absent here.

Everything else fails closed too: an ownerless record, an owner on
another host, a live matching identity, and every observation error.
There is no age-based override and no fallback to guessed death.
No tolerance, timeout, or PID-only shortcut authorizes reclamation."
  (let* ((owner (dorg-file-lock--read-owner lock))
         (host (alist-get :host owner))
         (pid (alist-get :pid owner))
         (recorded (alist-get :identity owner)))
    (cond
     ((null owner) nil)
     ((not (equal host (dorg-file-lock--host-name))) nil)
     ((not (and (integerp pid) (> pid 0))) nil)
     ((not (dorg-file-lock--valid-identity-p recorded)) nil)
     (t (let ((identity (dorg-file-lock--process-identity pid recorded)))
          (pcase (car identity)
            (:absent t)
            (:present
             (pcase (plist-get recorded :kind)
               ('linux-kernel
                (not (= (plist-get recorded :start-ticks)
                        (plist-get (cadr identity) :start-ticks))))
               ('darwin-time
                (eq :different (dorg-file-lock--compare-times
                                (plist-get recorded :start)
                                (plist-get (cadr identity) :start))))))
            (_ nil)))))))

(defun dorg-file-lock--tombstone (lock)
  "Return LOCK's single fixed reclamation tombstone path."
  (concat lock ".reclaim"))

(defun dorg-file-lock--owner-live-p (owner)
  "Return non-nil only when OWNER is affirmatively the current live process."
  (let ((host (alist-get :host owner))
        (pid (alist-get :pid owner))
        (recorded (alist-get :identity owner)))
    (and (equal host (dorg-file-lock--host-name))
         (integerp pid) (> pid 0)
         (dorg-file-lock--valid-identity-p recorded)
         (let ((observed (dorg-file-lock--process-identity pid recorded)))
           (and (eq (car observed) :present)
                (pcase (plist-get recorded :kind)
                  ('linux-kernel
                   (= (plist-get recorded :start-ticks)
                      (plist-get (cadr observed) :start-ticks)))
                  ('darwin-time
                   (eq :same (dorg-file-lock--compare-times
                              (plist-get recorded :start)
                              (plist-get (cadr observed) :start))))))))))

(defun dorg-file-lock--snapshot (lock)
  "Return LOCK's no-follow identity snapshot, or nil for an absent lock."
  (let* ((attributes (file-attributes lock))
         (link (and (stringp (car attributes)) (car attributes))))
    (cond
     (link (list :kind 'link :record link))
     ((eq (car attributes) t)
      (list :kind 'directory :owner (dorg-file-lock--read-owner lock)))
     (attributes (list :kind 'other))
     (t nil))))

(defun dorg-file-lock--publish (record lock)
  "Publish RECORD as the target of an exclusive symlink at LOCK.
This is the injectable DOrg seam; do not mock the native subr in tests."
  (make-symbolic-link record lock nil))

(defun dorg-file-lock--publish-once (lock record ownership)
  "Try to publish RECORD at LOCK and store it in OWNERSHIP on success.
Return :acquired, :existing or :unsupported.  Even an injected failure from
`dorg-file-lock--publish' after the native create cannot leak this call's link."
  (condition-case err
      (progn
        (unwind-protect
            (let ((inhibit-quit t))
              (dorg-file-lock--publish record lock)
              (setcar ownership record))
          ;; An injected seam can signal after symlink(2) returned but before
          ;; this Lisp frame records ownership.  The exact record is our token.
          (unless (car ownership)
            (when (equal (file-symlink-p lock) record)
              (delete-file lock))))
        :acquired)
    (file-already-exists :existing)
    (file-error
     ;; The native create can fail on volumes without symlinks, permissions,
     ;; or other local filesystem errors.  No critical section is entered.
     (when (equal (file-symlink-p lock) record)
       (delete-file lock))
     (ignore err)
     :unsupported)))

(defun dorg-file-lock--remove-snapshot (lock snapshot)
  "Remove LOCK only while its no-follow identity equals SNAPSHOT."
  (when (equal snapshot (dorg-file-lock--snapshot lock))
    (pcase (plist-get snapshot :kind)
      ('link (delete-file lock) t)
      ('directory (delete-directory lock t) t)
      (_ nil))))

(defun dorg-file-lock--mkdir-exclusive (dir)
  "Create tombstone DIR atomically; return non-nil only if this call won."
  (condition-case nil
      (progn (make-directory dir nil) t)
    (file-already-exists nil)))

(defun dorg-file-lock--classify (lock)
  "Classify a held LOCK without exposing its owner outside the leaf."
  (let ((owner (dorg-file-lock--read-owner lock)))
    (if (dorg-file-lock--owner-live-p owner)
        :live-holder
      :holder-unknown)))

(defun dorg-file-lock--reclaim (lock observed record ownership)
  "Elect one reclaimer for OBSERVED dead LOCK, then publish RECORD.
A fixed tombstone prevents two reclaimers from taking the same dead owner.
The snapshot and death proof are rechecked under the election."
  (let ((tomb (dorg-file-lock--tombstone lock)))
    (if (not (dorg-file-lock--mkdir-exclusive tomb))
        :reclaim-blocked
      (unwind-protect
          (if (and (equal observed (dorg-file-lock--snapshot lock))
                   (dorg-file-lock--owner-gone-p lock))
              (progn
                (dorg-file-lock--remove-snapshot lock observed)
                (pcase (dorg-file-lock--publish-once lock record ownership)
                  (:acquired :acquired)
                  (:unsupported :unsupported)
                  (_ (dorg-file-lock--classify lock))))
            (dorg-file-lock--classify lock))
        ;; Only this call created TOMB.  A killed reclaimer cannot run this
        ;; cleanup; its tombstone remains and every later attempt fails closed.
        (when (eq (car (file-attributes tomb)) t)
          (delete-directory tomb))))))

(defun dorg-file-lock--try (lock record ownership)
  "Attempt one non-blocking acquisition of LOCK for RECORD."
  (if (dorg-file-lock--present-p (dorg-file-lock--tombstone lock))
      :reclaim-blocked
    (pcase (dorg-file-lock--publish-once lock record ownership)
      (:acquired :acquired)
      (:unsupported :unsupported)
      (_
       (let ((observed (dorg-file-lock--snapshot lock)))
         (cond
          ((null observed) :live-holder) ; competitor released; bounded retry
          ((null (dorg-file-lock--read-owner lock)) :holder-unknown)
          ((dorg-file-lock--owner-gone-p lock)
           (dorg-file-lock--reclaim lock observed record ownership))
          (t (dorg-file-lock--classify lock))))))))

(defun dorg-file-lock--valid-timeout-p (timeout)
  "Return non-nil for a finite, nonnegative TIMEOUT in seconds."
  (and (numberp timeout) (>= timeout 0)
       (not (and (floatp timeout)
                 (or (isnan timeout)
                     (= timeout 1.0e+INF)
                     (= timeout -1.0e+INF))))))

(defun dorg-file-lock--acquire (lock timeout ownership)
  "Acquire LOCK within TIMEOUT seconds, storing its record in OWNERSHIP.
Return :acquired, :live-holder, :reclaim-blocked, :holder-unknown or
:unsupported.  A free lock is probed even when TIMEOUT is zero."
  (unless (dorg-file-lock--valid-timeout-p timeout)
    (error "dorg file-lock timeout must be finite and nonnegative"))
  (let* ((owner (dorg-file-lock--owner))
         (record (and (dorg-file-lock--valid-identity-p
                       (alist-get :identity owner))
                      (condition-case nil
                          (dorg-file-lock--encode-owner owner)
                        (error nil))))
         (deadline (+ (float-time) timeout))
         (attempts 0)
         (status nil))
    (unless record (setq status :unsupported))
    (while (null status)
      (setq attempts (1+ attempts))
      (let ((attempt (dorg-file-lock--try lock record ownership)))
        (if (or (memq attempt '(:acquired :unsupported))
                (>= (float-time) deadline)
                (>= attempts 1000))
            (setq status attempt)
          (sleep-for (min 0.05 (max 0.0 (- deadline (float-time))))))))
    status))

(defun dorg-file-lock--release (lock record)
  "Release LOCK only if its symlink still holds exact RECORD."
  (when (and (stringp record) (equal (file-symlink-p lock) record))
    (delete-file lock)
    t))

(defun dorg-file-lock-call (lock timeout fn)
  "Call FN with the held owner record, or return a typed refusal plist.
LOCK is an absolute local path.  Acquisition and BODY share one unwind scope;
quit/error cannot leak a lock owned by this process."
  (let ((ownership (list nil)))
    (unwind-protect
        (let ((status (dorg-file-lock--acquire lock timeout ownership)))
          (if (eq status :acquired)
              (list :status :acquired :value (funcall fn (car ownership)))
            (list :status status :lock lock)))
      (when (car ownership)
        (dorg-file-lock--release lock (car ownership))))))

(defun dorg-file-lock-inspect (lock)
  "Return a read-only operator report for LOCK, including owner metadata."
  (let* ((snapshot (dorg-file-lock--snapshot lock))
         (owner (dorg-file-lock--read-owner lock))
         (tomb (dorg-file-lock--tombstone lock)))
    (list :lock lock
          :status (cond ((dorg-file-lock--present-p tomb) :reclaim-blocked)
                        ((null snapshot) :free)
                        ((null owner) :holder-unknown)
                        ((dorg-file-lock--owner-gone-p lock) :dead-holder)
                        ((dorg-file-lock--owner-live-p owner) :live-holder)
                        (t :holder-unknown))
          :owner owner
          :tombstone (and (dorg-file-lock--present-p tomb) tomb))))

(provide 'dorg-file-lock)
;;; dorg-file-lock.el ends here
