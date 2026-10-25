;;; dorg-clock.el --- Persisted clock owner API -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Managed clock intents are immutable requests for the DOrg-owned clock
;; authority.  A named Dolt scope is required for durable replay.

;;; Code:

(require 'cl-lib)
(require 'dorg-api)
(require 'dorg-model)
(require 'dorg-mutate)
(require 'dorg-parse)
(require 'dorg-keywords)

(declare-function dorg-sql "dorg-dolt" (dolt-dir query))
(declare-function dorg-head-commit "dorg-dolt" (dolt-dir))
(declare-function dorg-model-load-headings "dorg-model-load"
                  (dolt-dir root-id ids &optional as-of))
(declare-function dorg-model-load-tree "dorg-model-load"
                  (dolt-dir root-id &optional as-of))
(declare-function dorg-repo-store-id "dorg-repo" (dolt-dir))

(defconst dorg-clock--argument-keys
  '(:source :base :save :message :lock-timeout :op-id
    :scope-id :instant-utc :zone :task-ref :interval-id
    :expected-active :correction :adoption)
  "Closed arguments to `dorg-clock-make-intent'.")

(defun dorg-clock--id-p (value)
  "Return non-nil when VALUE is an exact opaque wire ID."
  (and (stringp value) (not (equal value ""))))

(defun dorg-clock-make-intent (action &rest arguments)
  "Construct an immutable managed clock ACTION from closed ARGUMENTS.
All instants are exact integer UTC epoch seconds.  The request owns values;
execution cannot replace its source, time, scope, or task."
  (unless (memq action '(start stop switch correct cancel adopt))
    (dorg-api--raise 'malformed "unknown clock action" :action action))
  (let* ((args (dorg-api--intent-options arguments dorg-clock--argument-keys
                                        "clock intent"))
         (source (plist-get args :source))
         (resolved (and source (dorg-mutate--resolve-source source)))
         (scope (plist-get args :scope-id))
         (interval (plist-get args :interval-id))
         (active (plist-get args :expected-active))
         (task (plist-get args :task-ref))
         (instant (plist-get args :instant-utc))
         (zone (plist-get args :zone))
         (correction (plist-get args :correction))
         (adoption (plist-get args :adoption)))
    (unless (and resolved (eq 'dolt (dorg-mutate-source-kind resolved)))
      (dorg-api--raise 'unsupported "clock scope requires a Dolt authority"
                       :owner-reason 'unsupported))
    (unless (and (dorg-clock--id-p scope) (dorg-clock--id-p interval)
                 (plist-member args :expected-active)
                 (or (null active) (dorg-clock--id-p active))
                 (integerp instant) (>= instant 0)
                 (stringp zone) (not (equal zone ""))
                 (dorg-clock--id-p (plist-get args :op-id)))
      (dorg-api--raise 'malformed "clock request has missing or invalid identity/time fields"))
    (pcase action
      ('start
       (unless (and (stringp task) (not (equal task ""))
                    (null active) (null correction) (null adoption))
         (dorg-api--raise 'malformed "invalid start clock fields")))
      ('stop
       (unless (and (equal active interval) (null task)
                    (null correction) (null adoption))
         (dorg-api--raise 'malformed "invalid stop clock fields")))
      ('switch
       (unless (and (dorg-clock--id-p active)
                    (not (equal active interval))
                    (stringp task) (not (equal task ""))
                    (null correction) (null adoption))
         (dorg-api--raise 'malformed "invalid switch clock fields")))
      ('correct
       (unless (and (null task) correction (null adoption))
         (dorg-api--raise 'malformed "invalid correction fields")))
      ('cancel
       (unless (and (null task) (null correction) (null adoption))
         (dorg-api--raise 'malformed "invalid cancel fields")))
      ('adopt
       (unless (and (stringp task) (not (equal task ""))
                    adoption (null correction))
         (dorg-api--raise 'malformed "invalid adoption fields"))
       (setq adoption
             (condition-case err
                 (dorg-clock--adoption-normalize adoption zone task)
               (dorg-owner-refusal
                (dorg-api--raise
                 (if (memq (nth 2 err) dorg-api-error-reasons)
                     (nth 2 err) 'malformed)
                 (nth 3 err) :owner-reason (nth 2 err)))))))
    (let ((request (list :action action :scope-id scope
                         :instant-utc instant :zone zone :task-ref task
                         :interval-id interval :expected-active active
                         :correction correction :adoption adoption))
          (options nil))
      (dolist (key '(:source :base :save :message :lock-timeout :op-id))
        (when (plist-member args key)
          (setq options (append options (list key (plist-get args key))))))
      (apply #'dorg-api-make-owner-intent "DORG_CLOCK_" request options))))

(defconst dorg-clock--scope-keys
  '("DORG_CLOCK_KIND" "DORG_CLOCK_ACTIVE_ID")
  "Canonical owned keys on a scope heading.")

(defconst dorg-clock--interval-keys
  '("DORG_CLOCK_KIND" "DORG_CLOCK_SCOPE_ID" "DORG_CLOCK_TASK_REF"
    "DORG_CLOCK_START_UTC" "DORG_CLOCK_END_UTC" "DORG_CLOCK_ZONE"
    "DORG_CLOCK_CANCELLED" "DORG_CLOCK_ADOPTION")
  "Canonical owned keys on an interval heading.")

(defun dorg-clock--refuse (reason message)
  "Signal a typed clock owner REASON with MESSAGE."
  (signal 'dorg-owner-refusal (list "DORG_CLOCK_" reason message)))

(defun dorg-clock--decimal-id-p (value)
  "Return non-nil when VALUE is a canonical positive DORG_ID string."
  (and (stringp value) (string-match-p "\\`[1-9][0-9]*\\'" value)))

(defun dorg-clock--epoch-p (value)
  "Return non-nil when VALUE encodes exact nonnegative epoch seconds."
  (and (stringp value) (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)))

(defun dorg-clock-task-ref-decode (canonical)
  "Decode CANONICAL into a typed clock task identity, or return nil.
For Dolt, identity is dorg_meta store-id plus root and heading DORG_IDs.
For plain Org, identity is root DORG_STORE_ID plus heading DORG_ID.  Paths
and store directory names are never compared as identity."
  (when (stringp canonical)
    (cond
     ((string-match
       "\\`dolt:v1:\\([a-f0-9]\\{32\\}\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\)\\'"
       canonical)
      (list :kind 'dolt :store-id (match-string 1 canonical)
            :root-id (match-string 2 canonical)
            :heading-id (match-string 3 canonical)
            :canonical canonical))
     ((string-match
       "\\`plain:v1:\\([A-Za-z0-9+/=]+\\):\\([1-9][0-9]*\\)\\'"
       canonical)
      (let* ((encoded (match-string 1 canonical))
             (heading-id (match-string 2 canonical))
             (identity (condition-case nil
                           (decode-coding-string
                            (base64-decode-string encoded) 'utf-8 t)
                         (error nil))))
        (when (and (stringp identity) (not (equal identity ""))
                   (equal encoded
                          (base64-encode-string
                           (encode-coding-string identity 'utf-8 t) t)))
          (list :kind 'plain :root-identity identity
                :heading-id heading-id :canonical canonical)))))))

(defun dorg-clock-task-ref (source heading-id)
  "Encode SOURCE's durable identity and HEADING-ID as a stable typed task ref.
Return (:kind unresolved :reason store-unidentified) when a Dolt store lacks
its dorg_meta store-id or a plain file lacks DORG_STORE_ID.  This read path
never creates identity or writes to a task source."
  (when (and (integerp heading-id) (> heading-id 0))
    (setq heading-id (number-to-string heading-id)))
  (unless (dorg-clock--decimal-id-p heading-id)
    (dorg-api--raise 'malformed "task reference needs a stable DORG_ID"))
  (let* ((resolved (dorg-mutate--resolve-source source))
         (kind (dorg-mutate-source-kind resolved)))
    (pcase kind
      ('dolt
       (require 'dorg-repo)
       (let* ((root-id (dorg-mutate-source-root-id resolved))
              (store-id (dorg-repo-store-id
                         (dorg-mutate-source-dolt-dir resolved))))
         (if (null store-id)
             (list :kind 'unresolved :reason 'store-unidentified
                   :heading-id heading-id)
           (dorg-clock-task-ref-decode
            (format "dolt:v1:%s:%s:%s" store-id root-id heading-id)))))
      ('plain-org
       (let* ((descriptor (list :file (dorg-mutate-source-file resolved)))
              (identity (cdr (assoc "DORG_STORE_ID"
                                    (dorg-root-keywords descriptor)))))
         (if (or (not (stringp identity)) (equal identity ""))
             (list :kind 'unresolved :reason 'store-unidentified
                   :heading-id heading-id)
           (dorg-clock-task-ref-decode
            (format "plain:v1:%s:%s"
                    (base64-encode-string
                     (encode-coding-string identity 'utf-8 t) t)
                    heading-id)))))
      (_ (dorg-api--raise 'unsupported
                          "task reference requires a Dolt or plain source")))))

(defun dorg-clock--task-ref-p (value)
  "Return non-nil for a canonical, source-qualified task reference VALUE."
  (and (dorg-clock-task-ref-decode value) t))

(defun dorg-clock--zone-p (value)
  "Return non-nil for an installed exact IANA zone VALUE."
  (and (stringp value)
       (or (equal value "UTC")
           (and (string-match-p
                 "\\`[A-Za-z][A-Za-z0-9_+-]*\\(?:/[A-Za-z0-9_+-]+\\)+\\'"
                 value)
                (cl-some (lambda (root)
                           (file-readable-p (expand-file-name value root)))
                         '("/usr/share/zoneinfo" "/usr/share/lib/zoneinfo"))))))

(defun dorg-clock-render-interval (start-utc end-utc zone)
  "Project exact START-UTC, optional END-UTC and ZONE to one Org CLOCK line.
Both wall instants display their own floored minute.  Duration is computed
from canonical UTC seconds and floored separately; the line is never an ID."
  (unless (and (integerp start-utc) (>= start-utc 0)
               (or (null end-utc)
                   (and (integerp end-utc) (>= end-utc start-utc)))
               (dorg-clock--zone-p zone))
    (dorg-clock--refuse 'invalid-record "invalid clock projection input"))
  (let ((system-time-locale "C")
        (stamp (lambda (instant)
                 (format-time-string "[%Y-%m-%d %a %H:%M]"
                                     (seconds-to-time instant) zone))))
    (if end-utc
        (let* ((minutes (/ (- end-utc start-utc) 60))
               (hours (/ minutes 60))
               (remainder (% minutes 60)))
          (format "CLOCK: %s--%s => %d:%02d"
                  (funcall stamp start-utc) (funcall stamp end-utc)
                  hours remainder))
      (format "CLOCK: %s" (funcall stamp start-utc)))))

(defconst dorg-clock--legacy-stamp-regexp
  "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z] [0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "One exact Org CLOCK wall-minute token, without brackets.")

(defun dorg-clock--wall-minute-utc (wall zone)
  "Resolve WALL in ZONE to one exact UTC minute, refusing DST ambiguity."
  (unless (and (dorg-clock--zone-p zone)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([A-Z][a-z][a-z]\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                wall))
    (dorg-clock--refuse 'invalid-record "malformed legacy clock wall time"))
  (let* ((year (string-to-number (match-string 1 wall)))
         (month (string-to-number (match-string 2 wall)))
         (day (string-to-number (match-string 3 wall)))
         (hour (string-to-number (match-string 5 wall)))
         (minute (string-to-number (match-string 6 wall)))
         (seed (condition-case nil
                   (time-convert (encode-time 0 minute hour day month year zone)
                                 'integer)
                 (error
                  (dorg-clock--refuse 'invalid-record
                                      "legacy clock date is invalid"))))
         (first (- seed (% seed 60) 86400))
         (last (+ first 172800))
         (system-time-locale "C")
         matches)
    (cl-loop for candidate from first to last by 60
             when (equal wall (format-time-string
                               "%Y-%m-%d %a %H:%M"
                               (seconds-to-time candidate) zone))
             do (push candidate matches))
    (unless (= 1 (length matches))
      (dorg-clock--refuse 'ambiguous-adoption
                          "legacy clock wall minute is missing or ambiguous"))
    (car matches)))

(defun dorg-clock--parse-legacy-clock (raw zone)
  "Parse one exact raw Org CLOCK line RAW using explicit ZONE.
The result keeps RAW and canonical whole-minute UTC instants.  A repeated or
nonexistent local minute refuses rather than guessing a DST offset."
  (let ((pattern (concat
                  "\\`[ \\t]*CLOCK:[ \\t]*\\["
                  dorg-clock--legacy-stamp-regexp "\\]"
                  "\\(?:--\\[" dorg-clock--legacy-stamp-regexp
                  "\\][ \\t]*=>[ \\t]*\\([0-9]+\\):\\([0-9][0-9]\\)\\)?"
                  "[ \\t]*\\'")))
    (unless (and (stringp raw) (string-match pattern raw))
      (dorg-clock--refuse 'invalid-record "legacy CLOCK line is malformed"))
    (let* ((start-wall (match-string 1 raw))
           (end-wall (match-string 2 raw))
           (hours (and end-wall (string-to-number (match-string 3 raw))))
           (minutes (and end-wall (string-to-number (match-string 4 raw))))
           (start (dorg-clock--wall-minute-utc start-wall zone))
           (end (and end-wall (dorg-clock--wall-minute-utc end-wall zone))))
      (when (and end (< end start))
        (dorg-clock--refuse 'invalid-record
                            "legacy clock end precedes start"))
      (when (and end (or (>= minutes 60)
                         (/= (+ (* hours 60) minutes)
                             (/ (- end start) 60))))
        (dorg-clock--refuse 'invalid-record
                            "legacy clock duration disagrees with timestamps"))
      (list :raw raw :start-utc start :end-utc end))))

(defun dorg-clock--legacy-lines (heading)
  "Return HEADING's raw LOGBOOK CLOCK lines in exact occurrence order."
  (let (lines)
    (dolist (item (dorg-heading-items heading))
      (when (and (cl-typep item 'dorg-drawer-item)
                 (equal "LOGBOOK" (dorg-drawer-item-name item)))
        (dolist (line (dorg-drawer-item-lines item))
          (when (string-match-p "\\`[ \t]*CLOCK:" line)
            (push line lines)))))
    (nreverse lines)))

(defun dorg-clock--adoption-source (adoption)
  "Return a canonical qualified source from ADOPTION, or refuse."
  (let* ((source (plist-get adoption :source))
         (keys (and (proper-list-p source)
                    (cl-evenp (length source))
                    (cl-loop for (key _) on source by #'cddr collect key))))
    (unless (and keys (= (length keys) (length (delete-dups (copy-sequence keys)))))
      (dorg-clock--refuse 'invalid-record "adoption source has duplicate or malformed keys"))
    (cond
     ((and (equal keys '(:file))
           (stringp (plist-get source :file))
           (file-name-absolute-p (plist-get source :file)))
      (condition-case nil
          (list :file (file-truename (plist-get source :file)))
        (file-error (dorg-clock--refuse 'conflict
                                        "adoption source is unavailable"))))
     ((and (= (length keys) 2)
           (memq :dolt-dir keys) (memq :root-id keys)
           (stringp (plist-get source :dolt-dir))
           (integerp (plist-get source :root-id))
           (> (plist-get source :root-id) 0))
      (condition-case nil
          (list :dolt-dir (file-truename (plist-get source :dolt-dir))
                :root-id (plist-get source :root-id))
        (file-error (dorg-clock--refuse 'conflict
                                        "adoption source is unavailable"))))
     (t (dorg-clock--refuse 'invalid-record
                            "adoption source is not qualified")))))

(defun dorg-clock--adoption-check-source (adoption)
  "Return the exact legacy occurrence named by ADOPTION, or refuse drift."
  (let* ((source (dorg-clock--adoption-source adoption))
         (revision (plist-get adoption :revision))
         (heading-id (plist-get adoption :heading-id))
         (occurrence (plist-get adoption :occurrence))
         (raw (plist-get adoption :raw))
         (heading
          (condition-case nil
              (if (plist-member source :file)
                  (let* ((file (plist-get source :file))
                         (visiting (find-buffer-visiting file)))
                    (when (and visiting (buffer-modified-p visiting))
                      (dorg-clock--refuse 'conflict
                                          "adoption source has unsaved edits"))
                    (when (file-locked-p file)
                      (dorg-clock--refuse 'conflict
                                          "adoption source is locked"))
                    (with-temp-buffer
                      (insert-file-contents-literally file)
                      (unless (equal revision
                                     (secure-hash 'sha256 (current-buffer)))
                        (dorg-clock--refuse 'conflict
                                            "adoption source revision changed"))
                      (let ((parsed (dorg-parse-buffer (current-buffer))))
                        (dorg-parse-assert-unique-ids parsed)
                        (cl-find (string-to-number heading-id)
                                 (dorg-parse-result-headings parsed)
                                 :key #'dorg-heading-id))))
                (let ((dir (plist-get source :dolt-dir))
                      (root (plist-get source :root-id)))
                  (require 'dorg-model-load)
                  (unless (equal revision (dorg-head-commit dir))
                    (dorg-clock--refuse 'conflict
                                        "adoption source revision changed"))
                  (gethash (string-to-number heading-id)
                           (dorg-model-load-headings
                            dir root (list root (string-to-number heading-id))
                            revision))))
            (file-error (dorg-clock--refuse 'conflict
                                            "adoption source cannot be read"))
            (dorg-parse-duplicate-id
             (dorg-clock--refuse 'ambiguous-adoption
                                 "adoption source has duplicate heading IDs")))))
    (let ((lines (and heading (dorg-clock--legacy-lines heading))))
      (unless (and (integerp occurrence) (>= occurrence 0)
                   (stringp raw) (equal raw (nth occurrence lines)))
        (dorg-clock--refuse 'ambiguous-adoption
                            "adoption occurrence is missing or changed"))
      (when (> (cl-count raw lines :test #'equal) 1)
        (dorg-clock--refuse 'ambiguous-adoption
                            "identical legacy CLOCK lines cannot be distinguished")))
    raw))

(defun dorg-clock--adoption-normalize (adoption zone task-ref)
  "Capture a qualified ADOPTION at its named revision in ZONE for TASK-REF."
  (unless (and (proper-list-p adoption)
               (cl-evenp (length adoption))
               (stringp (plist-get adoption :revision))
               (dorg-clock--decimal-id-p (plist-get adoption :heading-id))
               (integerp (plist-get adoption :occurrence))
               (>= (plist-get adoption :occurrence) 0)
               (stringp (plist-get adoption :raw))
               (cl-every (lambda (key)
                           (memq key '(:source :revision :heading-id
                                       :occurrence :raw :open-choice)))
                         (cl-loop for (key _) on adoption by #'cddr
                                  collect key))
               (= (/ (length adoption) 2)
                  (length (delete-dups
                           (cl-loop for (key _) on adoption by #'cddr
                                    collect key)))))
    (dorg-clock--refuse 'invalid-record "adoption provenance is malformed"))
  (let* ((source (dorg-clock--adoption-source adoption))
         (qualified (append (list :source source)
                            (cl-loop for key in '(:revision :heading-id
                                                  :occurrence :raw :open-choice)
                                     append (list key (plist-get adoption key)))))
         (parsed (dorg-clock--parse-legacy-clock
                  (dorg-clock--adoption-check-source qualified) zone))
         (end (plist-get parsed :end-utc)))
    (let ((encoded (dorg-clock-task-ref
                    source (plist-get qualified :heading-id))))
      (unless (plist-get encoded :canonical)
        (dorg-api--raise 'unresolved "adoption source has no DORG_STORE_ID"
                         :detail 'store-unidentified))
      (unless (equal task-ref (plist-get encoded :canonical))
      (dorg-clock--refuse 'invalid-record
                          "adoption task reference differs from source")))
    (unless (if end
                (null (plist-get qualified :open-choice))
              (eq 'takeover (plist-get qualified :open-choice)))
      (dorg-clock--refuse 'ambiguous-adoption
                          "open legacy clock needs explicit takeover"))
    (append qualified
            (list :task-ref task-ref
                  :start-utc (plist-get parsed :start-utc)
                  :end-utc end))))

(defun dorg-clock--adoption-valid-p (adoption zone)
  "Return non-nil when sealed ADOPTION agrees with its raw ZONE minutes."
  (and (proper-list-p adoption)
       (condition-case nil
           (let ((parsed (dorg-clock--parse-legacy-clock
                          (plist-get adoption :raw) zone)))
             (and (equal (plist-get parsed :start-utc)
                         (plist-get adoption :start-utc))
                  (equal (plist-get parsed :end-utc)
                         (plist-get adoption :end-utc))
                  (if (plist-get parsed :end-utc)
                      (null (plist-get adoption :open-choice))
                    (eq 'takeover (plist-get adoption :open-choice)))))
         (dorg-owner-refusal nil))))

(defun dorg-clock--adoption-encoded (adoption)
  "Encode immutable ADOPTION provenance, including its original raw bytes."
  (concat "v1:"
          (secure-hash
           'sha256
           (prin1-to-string
            (list (plist-get adoption :task-ref)
                  (plist-get adoption :heading-id)
                  (plist-get adoption :raw))))
          ":"
          (base64-encode-string
           (prin1-to-string
            (cl-loop for key in '(:source :revision :heading-id :occurrence :raw)
                     append (list key (plist-get adoption key))))
           t)))

(defun dorg-clock--correction-p (value)
  "Return non-nil for a closed, nonempty exact correction VALUE."
  (and (proper-list-p value) (cl-evenp (length value)) value
       (let (seen valid)
         (setq valid t)
         (cl-loop for (key datum) on value by #'cddr
                  do (unless (and (memq key '(:start-utc :end-utc :zone))
                                  (not (memq key seen))
                                  (if (eq key :zone)
                                      (and (stringp datum)
                                           (not (equal datum "")))
                                    (and (integerp datum) (>= datum 0))))
                       (setq valid nil))
                  do (push key seen))
         valid)))

(defun dorg-clock--validate-request (request)
  "Validate the closed action fields in immutable REQUEST without reading HEAD."
  (let ((action (plist-get request :action))
        (scope (plist-get request :scope-id))
        (interval (plist-get request :interval-id))
        (active (plist-get request :expected-active))
        (task (plist-get request :task-ref))
        (instant (plist-get request :instant-utc))
        (zone (plist-get request :zone))
        (correction (plist-get request :correction))
        (adoption (plist-get request :adoption)))
    (unless (and (memq action '(start stop switch correct cancel adopt))
                 (dorg-clock--decimal-id-p scope)
                 (dorg-clock--decimal-id-p interval)
                 (plist-member request :expected-active)
                 (or (null active) (dorg-clock--decimal-id-p active))
                 (integerp instant) (>= instant 0)
                 (dorg-clock--zone-p zone))
      (dorg-clock--refuse 'invalid-record "clock request identity/time is invalid"))
    (pcase action
      ('start
       (unless (and (dorg-clock--task-ref-p task)
                    (null active) (null correction) (null adoption))
         (dorg-clock--refuse 'invalid-record "invalid start request")))
      ('stop
       (unless (and (equal interval active) (null task)
                    (null correction) (null adoption))
         (dorg-clock--refuse 'invalid-record "invalid stop request")))
      ('switch
       (unless (and (dorg-clock--decimal-id-p active)
                    (not (equal interval active))
                    (dorg-clock--task-ref-p task)
                    (null correction) (null adoption))
         (dorg-clock--refuse 'invalid-record "invalid switch request")))
      ('correct
       (unless (and (null task) (dorg-clock--correction-p correction)
                    (null adoption))
         (dorg-clock--refuse 'invalid-record "invalid correction request")))
      ('cancel
       (unless (and (null task) (null correction) (null adoption))
         (dorg-clock--refuse 'invalid-record "invalid cancel request")))
      ('adopt
       (unless (and (dorg-clock--task-ref-p task)
                    (dorg-clock--adoption-valid-p adoption zone)
                    (equal task (plist-get adoption :task-ref))
                    (equal task
                           (plist-get
                            (dorg-clock-task-ref
                             (plist-get adoption :source)
                             (plist-get adoption :heading-id))
                            :canonical))
                    (null correction)
                    (or (plist-get adoption :end-utc) (null active)))
         (dorg-clock--refuse 'invalid-record "invalid adoption request")))))
  request)

(defun dorg-clock--properties (heading)
  "Return HEADING's canonical owned property alist, refusing duplicates."
  (let (seen owned)
    (dolist (cell (dorg-model-heading-property-snapshot heading))
      (when (dorg-owned-metadata-key-p (car cell))
        (unless (and (member (car cell)
                             (append dorg-clock--scope-keys
                                     dorg-clock--interval-keys))
                     (not (member (car cell) seen)))
          (dorg-clock--refuse 'invalid-record "unknown or duplicated clock key"))
        (push (car cell) seen)
        (push cell owned)))
    (nreverse owned)))

(defun dorg-clock--property (properties key)
  "Return KEY from PROPERTIES."
  (cdr (assoc key properties)))

(defun dorg-clock--validate-record (heading properties)
  "Validate one owned HEADING with PROPERTIES independently of siblings."
  (let ((kind (dorg-clock--property properties "DORG_CLOCK_KIND")))
    (pcase kind
      ("scope"
       (unless (and (dorg-clock--decimal-id-p
                     (number-to-string (dorg-heading-id heading)))
                    (cl-every (lambda (cell)
                                (member (car cell) dorg-clock--scope-keys))
                              properties)
                    (let ((active (dorg-clock--property
                                   properties "DORG_CLOCK_ACTIVE_ID")))
                      (or (null active) (dorg-clock--decimal-id-p active))))
         (dorg-clock--refuse 'invalid-record "malformed clock scope")))
      ("interval"
       (let ((scope (dorg-clock--property properties "DORG_CLOCK_SCOPE_ID"))
             (task (dorg-clock--property properties "DORG_CLOCK_TASK_REF"))
             (start (dorg-clock--property properties "DORG_CLOCK_START_UTC"))
             (end (dorg-clock--property properties "DORG_CLOCK_END_UTC"))
             (zone (dorg-clock--property properties "DORG_CLOCK_ZONE"))
             (cancelled (dorg-clock--property properties "DORG_CLOCK_CANCELLED"))
             (adoption (dorg-clock--property properties "DORG_CLOCK_ADOPTION")))
         (unless (and (cl-every (lambda (cell)
                                 (member (car cell) dorg-clock--interval-keys))
                               properties)
                      (dorg-clock--decimal-id-p scope)
                      (integerp (dorg-heading-parent-id heading))
                      (equal scope (number-to-string
                                    (dorg-heading-parent-id heading)))
                      (dorg-clock--task-ref-p task)
                      (dorg-clock--epoch-p start)
                      (or (null end) (and (dorg-clock--epoch-p end)
                                          (>= (string-to-number end)
                                              (string-to-number start))))
                      (dorg-clock--zone-p zone)
                      (or (null cancelled) (equal cancelled "t"))
                      (or (null adoption)
                          (string-match-p "\\`v1:[a-f0-9]\\{64\\}:[A-Za-z0-9+/=]+\\'"
                                          adoption)))
           (dorg-clock--refuse 'invalid-record "malformed clock interval"))))
      (_ (dorg-clock--refuse 'invalid-record "missing clock record kind")))))

(defun dorg-clock--validate-image (image &optional complete)
  "Validate owned records in IMAGE; when COMPLETE check each whole scope."
  (let ((scopes (make-hash-table :test 'eql))
        (children (make-hash-table :test 'eql))
        (open (make-hash-table :test 'eql))
        (adoptions (make-hash-table :test 'equal)))
    (maphash
     (lambda (id heading)
       (let ((props (dorg-clock--properties heading)))
         (when props
           (dorg-clock--validate-record heading props)
           (if (equal "scope" (dorg-clock--property props "DORG_CLOCK_KIND"))
               (puthash id props scopes)
             (let ((scope (dorg-heading-parent-id heading)))
               (push id (gethash scope children))
               (when complete
                 (when-let ((adoption (dorg-clock--property
                                       props "DORG_CLOCK_ADOPTION")))
                   (let ((claim (and (string-match
                                      "\\`v1:\\([a-f0-9]\\{64\\}\\):" adoption)
                                     (match-string 1 adoption))))
                     (unless claim
                       (dorg-clock--refuse 'invalid-record
                                           "adoption provenance has no claim"))
                     (when (gethash claim adoptions)
                       (dorg-clock--refuse
                        'ambiguous-adoption
                        "legacy CLOCK line was already adopted"))
                     (puthash claim t adoptions))))
               (when (and (not (dorg-clock--property props "DORG_CLOCK_END_UTC"))
                          (not (dorg-clock--property props "DORG_CLOCK_CANCELLED")))
                 (push id (gethash scope open))))))))
     image)
    (when complete
      (maphash
       (lambda (scope _ids)
         (unless (gethash scope scopes)
           (dorg-clock--refuse 'invalid-record "orphan clock interval")))
       children)
      (maphash
       (lambda (scope children)
         (let ((props (gethash scope scopes)))
           (unless props
             (dorg-clock--refuse 'invalid-record "orphan clock interval"))
           (unless (and (<= (length children) 1)
                        (equal (dorg-clock--property
                                props "DORG_CLOCK_ACTIVE_ID")
                               (and children (number-to-string (car children)))))
             (dorg-clock--refuse 'invariant "clock pointer/open-child mismatch"))))
       open)
      (maphash
       (lambda (scope props)
         (let ((pointer (dorg-clock--property props "DORG_CLOCK_ACTIVE_ID")))
           (when (and pointer (not (gethash scope open)))
             (dorg-clock--refuse 'invariant "clock pointer names no open child"))))
       scopes))))

(defun dorg-clock--open-child-facts (context scope-id pointer)
  "Return bounded scalar open-child facts for SCOPE-ID at CONTEXT's base.
The three table reads share one frozen Dolt AS OF token.  The caller counts
this SQL statement and its one result row separately from ID hydration."
  (let* ((dolt-dir (dorg-ext-index-context-dolt-dir context))
         (base (dorg-ext-index-context-base-commit context))
         (root-id (dorg-ext-index-context-root-id context)))
    (unless (and (stringp dolt-dir) (stringp base)
                 (integerp root-id))
      (dorg-clock--refuse 'invariant "clock invariant probe lacks frozen base"))
    (require 'dorg-dolt)
    (let* ((sfx (dorg-sql-gen--as-of-suffix base))
           (sql
            (format
             (concat
              "SELECT COUNT(*) AS open_count, "
              "COALESCE(SUM(CASE WHEN h.id = %d THEN 1 ELSE 0 END), 0) "
              "AS pointer_open_count "
              "FROM headings%s h WHERE h.root_id = %d AND h.parent_id = %d "
              "AND EXISTS (SELECT 1 FROM heading_items%s hi "
              "JOIN property_entries%s pe ON pe.drawer_item_id = hi.id "
              "WHERE hi.heading_id = h.id AND pe.prop_key = 'DORG_CLOCK_KIND' "
              "AND pe.prop_value = 'interval') "
              "AND NOT EXISTS (SELECT 1 FROM heading_items%s hi "
              "JOIN property_entries%s pe ON pe.drawer_item_id = hi.id "
              "WHERE hi.heading_id = h.id AND pe.prop_key = 'DORG_CLOCK_END_UTC') "
              "AND NOT EXISTS (SELECT 1 FROM heading_items%s hi "
              "JOIN property_entries%s pe ON pe.drawer_item_id = hi.id "
              "WHERE hi.heading_id = h.id AND pe.prop_key = 'DORG_CLOCK_CANCELLED' "
              "AND pe.prop_value = 't')")
             (or pointer -1) sfx root-id scope-id
             sfx sfx sfx sfx sfx sfx))
           (rows (dorg-sql dolt-dir sql)))
      (unless (and (listp rows) (= (length rows) 1))
        (dorg-clock--refuse 'invariant "clock invariant probe returned no scalar"))
      (let ((row (car rows)))
        (list :open-count (alist-get 'open_count row)
              :pointer-open-count (alist-get 'pointer_open_count row))))))

(defun dorg-clock-read (scope-id &rest options)
  "Read SCOPE-ID at one frozen revision and one captured observation instant.
OPTIONS are :source, optional :snapshot, and required :observed-at, an exact
integer UTC epoch second.  Reading never advances an interval or writes."
  (let* ((args (dorg-api--intent-options
                options '(:source :snapshot :observed-at) "clock read"))
         (source (and (plist-get args :source)
                      (dorg-mutate--resolve-source (plist-get args :source))))
         (observed (plist-get args :observed-at)))
    (unless (and source (eq 'dolt (dorg-mutate-source-kind source)))
      (dorg-api--raise 'unsupported "clock read requires a Dolt authority"
                       :owner-reason 'unsupported))
    (unless (and (dorg-clock--decimal-id-p scope-id)
                 (integerp observed) (>= observed 0))
      (dorg-api--raise 'malformed "clock read needs exact scope and observation"))
    (require 'dorg-model-load)
    (let* ((dolt-dir (dorg-mutate-source-dolt-dir source))
           (root-id (dorg-mutate-source-root-id source))
           (snapshot (or (plist-get args :snapshot)
                         (dorg-head-commit dolt-dir)))
           (scope-n (string-to-number scope-id))
           (scope-image
            (dorg-model-load-headings dolt-dir root-id
                                      (list root-id scope-n) snapshot))
           (scope (gethash scope-n scope-image))
           (props (and scope (dorg-clock--properties scope))))
      (unless (and scope
                   (equal "scope" (dorg-clock--property props
                                                         "DORG_CLOCK_KIND")))
        (dorg-api--raise 'invalid-target "clock scope is missing"))
      (dorg-clock--validate-record scope props)
      (let* ((pointer (dorg-clock--property props "DORG_CLOCK_ACTIVE_ID"))
             (context (dorg-ext-index-context-create
                       :source-kind 'dolt :dolt-dir dolt-dir :root-id root-id
                       :base-commit snapshot))
             (facts (dorg-clock--open-child-facts
                     context scope-n (and pointer (string-to-number pointer)))))
        (unless (and (integerp (plist-get facts :open-count))
                     (= (plist-get facts :open-count) (if pointer 1 0))
                     (= (plist-get facts :pointer-open-count)
                        (if pointer 1 0)))
          (dorg-api--raise 'invariant "clock pointer/open-child mismatch"
                           :owner-reason 'invariant))
        (let* ((interval (and pointer
                              (gethash (string-to-number pointer)
                                       (dorg-model-load-headings
                                        dolt-dir root-id
                                        (list root-id scope-n
                                              (string-to-number pointer))
                                        snapshot))))
               (interval-props (and interval (dorg-clock--properties interval)))
               (start (and interval-props
                           (dorg-clock--property interval-props
                                                 "DORG_CLOCK_START_UTC"))))
          (when pointer
            (unless (and interval
                         (equal scope-id
                                (dorg-clock--property
                                 interval-props "DORG_CLOCK_SCOPE_ID"))
                         (null (dorg-clock--property interval-props
                                                     "DORG_CLOCK_END_UTC")))
              (dorg-api--raise 'invariant "active clock interval is invalid"
                               :owner-reason 'invariant))
            (dorg-clock--validate-record interval interval-props)
            (when (< observed (string-to-number start))
              (dorg-api--raise 'malformed
                               "observation precedes active clock start")))
          (list :scope-id scope-id :interval-id pointer
                :task-ref (and interval-props
                               (dorg-clock--property interval-props
                                                     "DORG_CLOCK_TASK_REF"))
                :start-utc (and start (string-to-number start))
                :zone (and interval-props
                           (dorg-clock--property interval-props
                                                 "DORG_CLOCK_ZONE"))
                :elapsed-seconds (and start (- observed (string-to-number start)))
                :org-clock-line
                (and start
                     (dorg-clock-render-interval
                      (string-to-number start) nil
                      (dorg-clock--property interval-props
                                            "DORG_CLOCK_ZONE")))
                :snapshot-token snapshot :observed-at observed))))))

(defun dorg-clock--overlap-changed-active-p (source request data)
  "Return non-nil if DATA's exact conflicting commit changed REQUEST's pointer.
An unrelated overlap keeps the engine's original typed refusal.  A failed
classification also keeps it; this helper never turns an error into success."
  (let* ((scope-id (plist-get request :scope-id))
         (scope-n (and (dorg-clock--decimal-id-p scope-id)
                       (string-to-number scope-id)))
         (commit (plist-get data :commit)))
    (when (and scope-n (member scope-n (plist-get data :overlap))
               (stringp commit)
               (eq 'dolt (dorg-mutate-source-kind source)))
      (condition-case nil
          (progn
            (require 'dorg-model-load)
            (let* ((image (dorg-model-load-headings
                         (dorg-mutate-source-dolt-dir source)
                         (dorg-mutate-source-root-id source)
                         (list (dorg-mutate-source-root-id source) scope-n)
                         commit))
                   (heading (gethash scope-n image))
                   (props (and heading (dorg-clock--properties heading))))
              (and (equal "scope" (dorg-clock--property props
                                                         "DORG_CLOCK_KIND"))
                   (not (equal (dorg-clock--property props
                                                     "DORG_CLOCK_ACTIVE_ID")
                               (plist-get request :expected-active))))))
        (error nil)))))

(defun dorg-clock--validate-normal (context)
  "Validate affected candidate records without assuming a full history slice."
  (let ((before (dorg-ext-index-context-base-image context))
        (after (dorg-ext-index-context-candidate-postimage context))
        (request (dorg-ext-index-context-owner-request context)))
    (when request
      (let* ((scope-id (plist-get request :scope-id))
             (scope-n (string-to-number scope-id))
             (scope (gethash scope-n before))
             (props (and scope (dorg-clock--properties scope)))
             (pointer (dorg-clock--property props "DORG_CLOCK_ACTIVE_ID"))
             (expected (plist-get request :expected-active))
             (action (plist-get request :action))
             (interval-n (string-to-number
                          (plist-get request :interval-id)))
             (interval (gethash interval-n before)))
        (unless (and scope
                     (equal "scope" (dorg-clock--property
                                     props "DORG_CLOCK_KIND")))
          (dorg-clock--refuse 'invalid-record "clock scope is absent"))
        (unless (equal pointer expected)
          (dorg-clock--refuse 'stale-active-interval
                              "clock active pointer differs from request"))
        (when (eq action 'adopt)
          (dorg-clock--adoption-check-source
           (plist-get request :adoption))
          ;; Adoption is administrative: validate every historical sibling,
          ;; including closed records absent from the bounded mutation slice.
          (require 'dorg-model-load)
          (let ((complete
                 (dorg-model-load-tree
                  (dorg-ext-index-context-dolt-dir context)
                  (dorg-ext-index-context-root-id context)
                  (dorg-ext-index-context-base-commit context))))
            (dorg-clock--validate-image complete t)
            (maphash (lambda (id candidate)
                       (puthash id candidate complete))
                     after)
            (dorg-clock--validate-image complete t)))
        (if (memq action '(start switch adopt))
            (when interval
              (dorg-clock--refuse 'conflict "new interval ID already exists"))
          (unless (and interval
                       (integerp (dorg-heading-parent-id interval))
                       (= scope-n (dorg-heading-parent-id interval))
                       (equal "interval" (dorg-clock--property
                                          (dorg-clock--properties interval)
                                          "DORG_CLOCK_KIND")))
            (dorg-clock--refuse 'invalid-record
                                "named interval is not in clock scope")))))
    (maphash
     (lambda (id heading)
       (when (and (dorg-clock--properties heading)
                  (not (gethash id after)))
         (dorg-clock--refuse 'conflict "generic mutation removed clock record")))
     before)
    (dorg-clock--validate-image after)
    (maphash
     (lambda (id heading)
       (let ((before-props (dorg-clock--properties heading))
             (after-heading (gethash id after)))
         (when (equal "scope" (dorg-clock--property
                                before-props "DORG_CLOCK_KIND"))
           (let* ((after-props (and after-heading
                                    (dorg-clock--properties after-heading)))
                  (old (dorg-clock--property
                        before-props "DORG_CLOCK_ACTIVE_ID"))
                  (new (dorg-clock--property
                        after-props "DORG_CLOCK_ACTIVE_ID")))
             (unless (equal old new)
               (when new
                 (let* ((new-id (string-to-number new))
                        (child (gethash new-id after))
                        (props (and child (dorg-clock--properties child))))
                   (unless (and child (not (gethash new-id before))
                                (= id (dorg-heading-parent-id child))
                                (equal "interval" (dorg-clock--property
                                                   props "DORG_CLOCK_KIND"))
                                (null (dorg-clock--property
                                       props "DORG_CLOCK_END_UTC")))
                     (dorg-clock--refuse
                      'invariant "new active pointer lacks a new open child"))))
               (when old
                 (let* ((old-child (gethash (string-to-number old) after))
                        (old-props (and old-child
                                        (dorg-clock--properties old-child))))
                   (unless (and old-child
                                (or (dorg-clock--property
                                     old-props "DORG_CLOCK_END_UTC")
                                    (dorg-clock--property
                                     old-props "DORG_CLOCK_CANCELLED")))
                     (dorg-clock--refuse
                      'stale-active-interval
                      "active interval was not explicitly closed")))))))))
     before)
    (when (eq (dorg-ext-index-context-source-kind context) 'dolt)
      (maphash
       (lambda (id heading)
         (let ((props (dorg-clock--properties heading)))
           (when (equal "scope" (dorg-clock--property props "DORG_CLOCK_KIND"))
             (let* ((pointer-text
                     (dorg-clock--property props "DORG_CLOCK_ACTIVE_ID"))
                    (pointer (and pointer-text (string-to-number pointer-text)))
                    (facts (dorg-clock--open-child-facts context id pointer)))
               (unless (and (integerp (plist-get facts :open-count))
                            (= (plist-get facts :open-count)
                               (if pointer 1 0))
                            (= (plist-get facts :pointer-open-count)
                               (if pointer 1 0)))
                 (dorg-clock--refuse
                  'invariant "clock pointer/open-child prestate is corrupt"))))))
       before))))

(defun dorg-clock--validate-restore-images (before after)
  "Validate complete BEFORE and AFTER images for safe v1 clock restoration."
  (dorg-clock--validate-image before t)
  (dorg-clock--validate-image after t)
  (maphash
   (lambda (id heading)
     (let ((prior (dorg-clock--properties heading)))
       (when prior
         (let* ((new-heading (gethash id after))
                (new (and new-heading (dorg-clock--properties new-heading)))
                (kind (dorg-clock--property prior "DORG_CLOCK_KIND")))
           (unless (and new
                        (equal kind (dorg-clock--property
                                     new "DORG_CLOCK_KIND")))
             (dorg-clock--refuse 'conflict
                                 "restore would delete a managed clock record"))
           (unless (or (equal kind "scope")
                       (cl-every
                        (lambda (key)
                          (equal (dorg-clock--property prior key)
                                 (dorg-clock--property new key)))
                        '("DORG_CLOCK_SCOPE_ID" "DORG_CLOCK_TASK_REF"
                          "DORG_CLOCK_ADOPTION")))
             (dorg-clock--refuse 'conflict
                                 "restore would change interval identity"))
           (if (equal kind "scope")
               (unless (equal (dorg-clock--property
                               prior "DORG_CLOCK_ACTIVE_ID")
                              (dorg-clock--property
                               new "DORG_CLOCK_ACTIVE_ID"))
                 (dorg-clock--refuse 'stale-active-interval
                                     "restore would change active clock pointer"))
             (when (or (and (dorg-clock--property
                            prior "DORG_CLOCK_END_UTC")
                           (null (dorg-clock--property
                                  new "DORG_CLOCK_END_UTC")))
                       (and (dorg-clock--property
                             prior "DORG_CLOCK_CANCELLED")
                            (null (dorg-clock--property
                                   new "DORG_CLOCK_CANCELLED"))))
               (dorg-clock--refuse 'stale-active-interval
                                   "restore would reopen a settled interval")))))))
   before)
  (maphash
   (lambda (id heading)
     (when (and (dorg-clock--properties heading)
                (not (gethash id before)))
       (dorg-clock--refuse 'conflict
                           "restore would introduce a managed clock record")))
   after))

(defun dorg-clock--validate (context)
  "Validate a lifecycle candidate or a complete historical restore CONTEXT."
  (let ((request (dorg-ext-index-context-owner-request context)))
    (if (eq (plist-get request :action) 'restore)
        (dorg-clock--validate-restore-images
         (dorg-ext-index-context-base-image context)
         (dorg-ext-index-context-candidate-postimage context))
      (dorg-clock--validate-normal context))))

(defun dorg-clock--import (context)
  "Validate complete authority scope during import."
  (let ((before (dorg-ext-index-context-base-image context))
        (after (dorg-ext-index-context-candidate-postimage context)))
    (maphash
     (lambda (id heading)
       (when (and (dorg-clock--properties heading)
                  (not (gethash id after)))
         (dorg-clock--refuse 'conflict "import removed managed clock record")))
     before)
    (dorg-clock--validate-image after t)))

(defun dorg-clock--property-patch (id key value)
  "Return one owned property put on ID, KEY and VALUE."
  (list :op 'put :target (list :property :host (list :heading :id id)
                              :name key)
        :value value))

(defun dorg-clock--lower (request)
  "Lower immutable REQUEST to typed core patches, without reading HEAD."
  (dorg-clock--validate-request request)
  (let* ((action (plist-get request :action))
         (scope (string-to-number (plist-get request :scope-id)))
         (interval (string-to-number (plist-get request :interval-id)))
         (instant (number-to-string (plist-get request :instant-utc)))
         (task (plist-get request :task-ref))
         (zone (plist-get request :zone))
         (active (plist-get request :expected-active)))
    (list
     :patches
     (pcase action
       ('start
        (list
         (list :op 'insert :target (list :tree :under
                                         (list :heading :id scope))
               :heading
               (list :id interval :title "Clock interval"
                     :items
                     (list
                      (list :kind 'drawer :name "PROPERTIES"
                            :properties
                            (list (cons "DORG_CLOCK_KIND" "interval")
                                  (cons "DORG_CLOCK_SCOPE_ID"
                                        (number-to-string scope))
                                  (cons "DORG_CLOCK_TASK_REF" task)
                                  (cons "DORG_CLOCK_START_UTC" instant)
                                  (cons "DORG_CLOCK_ZONE" zone))))))
         (dorg-clock--property-patch
          scope "DORG_CLOCK_ACTIVE_ID" (number-to-string interval))))
       ('stop
        (list (dorg-clock--property-patch interval "DORG_CLOCK_END_UTC" instant)
              (list :op 'delete
                    :target (list :property :host (list :heading :id scope)
                                  :name "DORG_CLOCK_ACTIVE_ID"))))
       ('switch
        (append
         (plist-get (dorg-clock--lower
                     (list :action 'stop :scope-id (number-to-string scope)
                           :interval-id active :instant-utc
                           (plist-get request :instant-utc)
                           :zone zone :expected-active active))
                    :patches)
         (plist-get (dorg-clock--lower
                     (list :action 'start :scope-id (number-to-string scope)
                           :interval-id (number-to-string interval)
                           :task-ref task :instant-utc
                           (plist-get request :instant-utc)
                           :zone zone :expected-active nil))
                    :patches)))
       ('correct
        (let ((correction (plist-get request :correction))
              patches)
          (dolist (field '((:start-utc . "DORG_CLOCK_START_UTC")
                           (:end-utc . "DORG_CLOCK_END_UTC")
                           (:zone . "DORG_CLOCK_ZONE")))
            (when (plist-member correction (car field))
              (push (dorg-clock--property-patch
                     interval (cdr field)
                     (let ((value (plist-get correction (car field))))
                       (if (integerp value) (number-to-string value) value)))
                    patches)))
          (nreverse patches)))
       ('cancel
        (append
         (list (dorg-clock--property-patch
                interval "DORG_CLOCK_CANCELLED" "t"))
         (when (equal active (plist-get request :interval-id))
           (list (list :op 'delete
                       :target (list :property :host (list :heading :id scope)
                                     :name "DORG_CLOCK_ACTIVE_ID"))))))
       ('adopt
        (let* ((adoption (plist-get request :adoption))
               (end (plist-get adoption :end-utc))
               (props
                (append
                 (list (cons "DORG_CLOCK_KIND" "interval")
                       (cons "DORG_CLOCK_SCOPE_ID" (number-to-string scope))
                       (cons "DORG_CLOCK_TASK_REF" task)
                       (cons "DORG_CLOCK_START_UTC"
                             (number-to-string
                              (plist-get adoption :start-utc)))
                       (cons "DORG_CLOCK_ZONE" zone)
                       (cons "DORG_CLOCK_ADOPTION"
                             (dorg-clock--adoption-encoded adoption)))
                 (when end
                   (list (cons "DORG_CLOCK_END_UTC"
                               (number-to-string end)))))))
          (append
           (list (list :op 'insert :target (list :tree :under
                                                (list :heading :id scope))
                       :heading
                       (list :id interval :title "Adopted clock interval"
                             :items
                             (list (list :kind 'drawer :name "PROPERTIES"
                                         :properties props)))))
           (unless end
             (list (dorg-clock--property-patch
                    scope "DORG_CLOCK_ACTIVE_ID"
                    (number-to-string interval)))))))
       (_ (dorg-clock--refuse 'unsupported "clock action is not lowered yet"))))))

(defun dorg-clock--restore (context)
  "Return typed owner patches after validating the complete affected scope."
  (let* ((current (plist-get context :current-image))
         (target (plist-get context :target-image))
         (ids (plist-get context :scope-ids))
         (heading-id (nth 2 (plist-get context :identity)))
         (heading (or (and (hash-table-p current) (gethash heading-id current))
                      (and (hash-table-p target) (gethash heading-id target))))
         (props (and heading (dorg-clock--properties heading)))
         (scope-id
          (if (equal "scope" (dorg-clock--property props "DORG_CLOCK_KIND"))
              heading-id
            (and heading (dorg-heading-parent-id heading))))
         (before (plist-get context :before))
         (after (plist-get context :after))
         patches)
    (unless (and (hash-table-p current) (hash-table-p target)
                 (integerp scope-id) (memq scope-id ids))
      (dorg-clock--refuse 'unsupported
                          "clock restore requires a complete scope image"))
    (dolist (image (list current target))
      (maphash
       (lambda (id candidate)
         (when (or (= id scope-id)
                   (eql scope-id (dorg-heading-parent-id candidate)))
           (unless (memq id ids)
             (dorg-clock--refuse 'unsupported
                                 "clock restore is not a complete scope"))))
       image))
    (dorg-clock--validate-restore-images current target)
    (when after
      (dolist (cell before)
        (unless (assoc (car cell) after)
          (push (list :op 'delete
                      :target (list :property
                                    :host (list :heading :id heading-id)
                                    :name (car cell)))
                patches)))
      (dolist (cell after)
        (unless (equal (cdr (assoc (car cell) before)) (cdr cell))
          (push (dorg-clock--property-patch
                 heading-id (car cell) (cdr cell)) patches))))
    (list :patches (nreverse patches))))

(defun dorg-clock--validate-owner-intent (request options)
  "Validate immutable clock REQUEST and public OPTIONS before execution."
  (let ((source (plist-get options :source)))
    (unless (and source
                 (eq 'dolt
                     (dorg-mutate-source-kind
                      (dorg-mutate--resolve-source source))))
      (dorg-api--raise 'unsupported "clock scope requires a Dolt authority"
                       :owner-reason 'unsupported)))
  (unless (and (stringp (plist-get options :base))
               (not (equal "" (plist-get options :base)))
               (stringp (plist-get options :op-id))
               (not (equal "" (plist-get options :op-id))))
    (dorg-api--raise 'malformed
                     "clock mutation requires exact base and operation ID"))
  (dorg-clock--validate-request request))

(defun dorg-clock--classify-conflict (source request data)
  "Classify a scope-pointer overlap at DATA's exact conflicting commit."
  (when (dorg-clock--overlap-changed-active-p source request data)
    (list :reason 'stale-active-interval
          :message "clock scope pointer changed at the conflicting commit"
          :data (list :detail 'scope-overlap
                      :base-commit (plist-get data :base-commit)
                      :commit (plist-get data :commit)))))

(defun dorg-clock--result-fields (request options _result digest)
  "Return stable clock-domain response fields from REQUEST and DIGEST."
  (list :scope-id (plist-get request :scope-id)
        :interval-id (plist-get request :interval-id)
        :action (plist-get request :action)
        :requested-base (plist-get options :base)
        :intent-digest digest))

(defun dorg-clock-metadata-owner ()
  "Return the registered DORG_CLOCK_ owner participant."
  (list :reasons '(stale-active-interval conflict unsupported invalid-record
                   invariant ambiguous-adoption)
        :validate #'dorg-clock--validate
        :validate-request #'dorg-clock--validate-owner-intent
        :classify-conflict #'dorg-clock--classify-conflict
        :result #'dorg-clock--result-fields
        :import #'dorg-clock--import
        :lower #'dorg-clock--lower
        :restore #'dorg-clock--restore))

(provide 'dorg-clock)
;;; dorg-clock.el ends here
