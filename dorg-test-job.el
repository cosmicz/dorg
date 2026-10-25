;;; dorg-test-job.el --- Disposable suite job contract helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pure helpers for the disposable DOrg suite job.  The shell entrypoint owns
;; process orchestration; this module pins the parts that are easiest to test
;; without Docker: commit validation, bundle checkout/connectivity, mount
;; allowlist parsing, fail-fast status shaping, and census comparisons.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst dorg-test-job-commit-regexp "\\`[0-9a-f]\\{40\\}\\'"
  "Regular expression for a full lowercase git object id.")

(defconst dorg-test-job-step-order '(compile lint test)
  "Stable status keys run by the disposable suite job.
The `test' key is intentionally retained in artifacts and fail-fast shaping even
though the unselected command is `make test-full'; changing the key would break
the published job receipt schema rather than merely changing the test target.")

(defconst dorg-test-job-runtime-mount-targets
  '(("/")
    ("/proc" "proc")
    ("/dev" "tmpfs")
    ("/dev/pts" "devpts")
    ("/dev/mqueue" "mqueue")
    ("/dev/shm" "tmpfs")
    ("/sys" "sysfs")
    ("/sys/fs/cgroup" "cgroup" "cgroup2")
    ("/etc/hosts")
    ("/etc/hostname")
    ("/etc/resolv.conf")
    ("/sbin/docker-init")
    ("/usr/sbin/docker-init"))
  "Docker/runtime mount targets that are not caller-controlled project binds.")

(defconst dorg-test-job-allowed-caller-bind-targets '("/artifacts")
  "Caller-controlled bind targets permitted inside the suite job.")

(defun dorg-test-job-validate-commit (commit)
  "Return COMMIT if it is a full 40-character lowercase SHA.
Signal `user-error' for symbolic refs, abbreviations, empty input, or
non-hex strings."
  (unless (and (stringp commit)
               (string-match-p dorg-test-job-commit-regexp commit))
    (user-error "Commit must be a full 40-character lowercase SHA, got %S"
                commit))
  commit)

(defun dorg-test-job-launcher-bundle-command ()
  "Return the git command the launcher uses to stream source.
The source side bundles a real ref, while `--commit' carries the independent
full SHA.  A bare object name would make `git bundle create' refuse an empty
bundle and would collapse the non-vacuity of the commit comparison."
  '("git" "bundle" "create" "-" "HEAD"))

(defun dorg-test-job--call (directory program &rest args)
  "Run PROGRAM ARGS in DIRECTORY and return trimmed stdout.
Signal `user-error' with stderr/stdout when the process exits nonzero."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'call-process program nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (user-error "%s %s failed with %S: %s"
                      program
                      (mapconcat #'identity args " ")
                      status
                      (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun dorg-test-job-checkout-bundle (bundle commit job-root)
  "Clone BUNDLE into JOB-ROOT and verify it names COMMIT with connected history.
Return a plist containing `:checkout' and `:head'.  The bundle's HEAD must
already equal COMMIT; the job does not silently check out a different object
from a mismatched bundle."
  (setq commit (dorg-test-job-validate-commit commit))
  (let* ((root (file-name-as-directory (expand-file-name job-root)))
         (checkout (expand-file-name "checkout" root)))
    (make-directory root t)
    (when (file-exists-p checkout)
      (user-error "Checkout path already exists: %s" checkout))
    (dorg-test-job--call root "git" "clone" bundle checkout)
    (let ((head (dorg-test-job--call checkout "git" "rev-parse" "HEAD")))
      (unless (equal head commit)
        (user-error "Bundle HEAD %s does not match requested commit %s"
                    head commit))
      (dorg-test-job--call checkout "git" "rev-list" "HEAD")
      (dorg-test-job--call checkout "git" "fsck" "--connectivity-only")
      (list :checkout checkout :head head))))

(defun dorg-test-job--decode-mountinfo-path (path)
  "Decode the small subset of mountinfo PATH escaping this job needs."
  (replace-regexp-in-string "\\\\040" " " path t t))

(defun dorg-test-job--mountinfo-entry (line)
  "Return mount target and filesystem type from one mountinfo LINE."
  (let* ((parts (split-string line " - " t))
         (pre (car parts))
         (post (cadr parts))
         (fields (and pre (split-string pre "[ \t]+" t)))
         (post-fields (and post (split-string post "[ \t]+" t))))
    (when (>= (length fields) 5)
      (list :target (dorg-test-job--decode-mountinfo-path (nth 4 fields))
            :fstype (car post-fields)))))

(defun dorg-test-job--mount-fstype-allowed-p (fstype allowed)
  "Return non-nil when FSTYPE is accepted by ALLOWED.
An empty ALLOWED list means any filesystem type is accepted."
  (or (null allowed)
      (member fstype allowed)))

(defun dorg-test-job--runtime-mount-target-p (target fstype)
  "Return non-nil when TARGET/FSTYPE is a Docker runtime mount."
  (or (cl-some
       (lambda (entry)
         (and (equal target (car entry))
              (dorg-test-job--mount-fstype-allowed-p fstype (cdr entry))))
       dorg-test-job-runtime-mount-targets)
      (and (string-prefix-p "/proc/" target)
           (member fstype '("proc" "tmpfs")))
      (and (string-prefix-p "/dev/" target)
           (member fstype '("tmpfs" "devpts" "mqueue")))
      (and (string-prefix-p "/sys/" target)
           (member fstype '("sysfs" "cgroup" "cgroup2" "tmpfs")))))

(defun dorg-test-job-mount-report (mountinfo-lines)
  "Return an allowlist report for MOUNTINFO-LINES.
The primary assertion is that caller-controlled bind targets are exactly
`dorg-test-job-allowed-caller-bind-targets'.  Runtime Docker/system mounts are
reported separately.  Any additional non-runtime target makes `:ok' nil."
  (let (runtime caller unexpected)
    (dolist (line mountinfo-lines)
      (when-let* ((entry (dorg-test-job--mountinfo-entry line))
                  (target (plist-get entry :target))
                  (fstype (plist-get entry :fstype)))
        (cond
         ((dorg-test-job--runtime-mount-target-p target fstype)
          (push target runtime))
         ((member target dorg-test-job-allowed-caller-bind-targets)
          (push target caller))
         (t
          (push target unexpected)))))
    (let* ((caller (sort (delete-dups caller) #'string<))
           (unexpected (sort (delete-dups unexpected) #'string<))
           (missing (cl-set-difference dorg-test-job-allowed-caller-bind-targets
                                       caller
                                       :test #'equal)))
      (list :ok (and (null unexpected) (null missing))
            :caller-bind-targets caller
            :runtime-targets (sort (delete-dups runtime) #'string<)
            :unexpected-targets unexpected
            :missing-targets missing))))

(defun dorg-test-job-step-statuses (completed)
  "Return fail-fast statuses for COMPLETED gate steps.
COMPLETED is an alist containing statuses for steps that actually ran.  Steps
after the first nonzero status are marked `skipped'."
  (let ((failed nil)
        out)
    (dolist (step dorg-test-job-step-order (nreverse out))
      (let ((cell (assq step completed)))
        (cond
         (failed
          (push (cons step 'skipped) out))
         (cell
          (push cell out)
          (unless (eql 0 (cdr cell))
            (setq failed t)))
         (t
          (push (cons step 'skipped) out)
          (setq failed t)))))))

(provide 'dorg-test-job)
;;; dorg-test-job.el ends here
