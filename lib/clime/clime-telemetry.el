;;; clime-telemetry.el --- Privacy-safe local invocation telemetry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a reusable JSONL sink for `clime-invocation-event' values.
;; The default record deliberately stores lifecycle metadata only.  It
;; excludes argv, params, context, handler output, return values, and error
;; messages because any of those may contain credentials or private input.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'clime-core)

;; `file-locked' is defined lazily by userlock.el, which does not provide a
;; feature.  Batch programs can reach the handler before that library loads.
(unless (get 'file-locked 'error-conditions)
  (define-error 'file-locked "File is locked" 'file-error))

(defgroup clime-telemetry nil
  "Privacy-safe local invocation telemetry."
  :group 'clime)

(defcustom clime-telemetry-lock-retry-count 6
  "Maximum number of retries after a transient file-lock conflict."
  :type 'natnum
  :group 'clime-telemetry)

(defcustom clime-telemetry-lock-retry-delay 0.05
  "Seconds to wait between file-lock retries."
  :type 'number
  :group 'clime-telemetry)

(defun clime-telemetry--name (value)
  "Return VALUE as a JSON-friendly name string, preserving nil."
  (cond
   ((symbolp value) (and value (symbol-name value)))
   ((stringp value) value)
   (value (format "%s" value))))

(defun clime-telemetry--output-format-name (event)
  "Return the effective output-format name for EVENT."
  (let ((format (clime-invocation-event-format event)))
    (or (clime-telemetry--name
         (if (clime-output-format-p format)
             (clime-output-format-name format)
           format))
        "text")))

(defun clime-telemetry--safe-path (event)
  "Return EVENT's structured command path when it is safe to persist.

A serve `not-found' path consists of untrusted URL segments rather than a
resolved command path, so omit it."
  (unless (and (eq (clime-invocation-event-surface event) 'serve)
               (eq (clime-invocation-event-phase event) 'not-found))
    (or (clime-invocation-event-display-path event)
        (clime-invocation-event-path event))))

;;;###autoload
(defun clime-telemetry-event-record (event)
  "Return a privacy-safe metadata alist for invocation EVENT.

The record omits all user-controlled values except the structured command
path.  In particular, it never includes argv, params, context, handler output,
return values, or error messages."
  (unless (clime-invocation-event-p event)
    (signal 'wrong-type-argument (list 'clime-invocation-event-p event)))
  (let ((start-time (clime-invocation-event-start-time event)))
    `((timestamp . ,(and start-time
                        (format-time-string "%FT%T%z"
                                            (seconds-to-time start-time))))
      (app . ,(clime-node-name (clime-invocation-event-app event)))
      (surface . ,(clime-telemetry--name
                   (clime-invocation-event-surface event)))
      (phase . ,(clime-telemetry--name
                 (clime-invocation-event-phase event)))
      (path . ,(clime-telemetry--safe-path event))
      (exit_code . ,(clime-invocation-event-exit-code event))
      (duration . ,(clime-invocation-event-duration event))
      (output_format . ,(clime-telemetry--output-format-name event))
      (error_type . ,(clime-telemetry--name
                      (clime-invocation-event-error-type event))))))

(defun clime-telemetry--append-line (file line)
  "Append LINE to FILE, retrying bounded transient lock conflicts."
  (let ((attempt 0))
    (while
        (condition-case err
            (progn
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region line nil file 'append 'silent))
              nil)
          (file-locked
           (if (< attempt (max 0 clime-telemetry-lock-retry-count))
               (progn
                 (cl-incf attempt)
                 (when (> clime-telemetry-lock-retry-delay 0)
                   (sleep-for clime-telemetry-lock-retry-delay))
                 t)
             (signal (car err) (cdr err))))))))

;;;###autoload
(defun clime-telemetry-append-jsonl (event file)
  "Append one privacy-safe JSON line for invocation EVENT to FILE.

Create FILE's parent directory when needed.  The append operation uses UTF-8
and bounded retries for transient Emacs file-lock conflicts."
  (let ((directory (file-name-directory (expand-file-name file))))
    (when directory
      (make-directory directory t))
    (clime-telemetry--append-line
     file
     (concat (json-encode (clime-telemetry-event-record event)) "\n")))
  file)

(provide 'clime-telemetry)
;;; clime-telemetry.el ends here
