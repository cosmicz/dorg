;;; dorg-mutate-persist-tests.el --- Atomic plain-org persistence SPEC -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; RED SPEC for tracked-case: one DOrg-owned atomic persistence primitive for
;; every plain-org apply path reached through `dorg-api-execute'.
;;
;; The current executor persists a live source with raw `save-buffer' and a
;; file-only source with a direct destination `write-region'; an interrupted
;; writer truncates/partially replaces the source, and the revision check is
;; stat-shaped (tick/mtime), not a content compare-and-swap — an equal-stat
;; external replacement is silently overwritten by a stale in-memory source.
;;
;; Target seam (bead Design, "Concrete seam design"):
;;   `dorg-mutate--plain-org-commit' + injectable step seams
;;   `dorg-mutate--persist-fingerprint' / `--persist-file-fingerprint' /
;;   `--persist-create-temp' / `--persist-write-temp' / `--persist-rename',
;;   content-fingerprint baselines, `:fingerprint'-bearing revision tokens
;;   AND public plain-org `:base' tokens (OC design correction
;;   2026-07-16T21:27: a stat-only public `:base' cannot see an equal-stat
;;   replacement across dry-run/apply; the post-commit `:new-token' must be
;;   acceptable as the next `:base'), O_EXCL same-directory temp + fsync +
;;   recheck + atomic rename.
;;
;; TEST SAFETY (OC hard constraint while tracked-case is open): deterministic
;; fault injection ONLY — cl-letf over DOrg Lisp seams and real per-test temp
;; directories.  Native subrs are never mocked (native-comp trampoline
;; hazard); no crashing child processes; focused selector only:
;;
;;   make test SELECT='dorg-test-persist-'
;;
;; Every test's docstring states its intended RED reason.  Behavioral tests
;; assert the FIXED contract and today demonstrate the defect (destination
;; clobbered / no typed refusal / no fingerprint / in-place inode); seam
;; tests fail because the seams do not exist yet, so their recordings stay
;; nil.  The bounded post-fix child-interrupt proof is deliberately ABSENT:
;; it lands only after tracked-case certifies child teardown safe.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg-mutate)
(require 'dorg-api)

;;; ─── Fixture and helpers ──────────────────────────────────────────────

(defconst dorg-test-persist--fixture
  (concat "* TODO Alpha\n"
          ":PROPERTIES:\n"
          ":DORG_ID: 100\n"
          ":END:\n"
          "body alpha\n"
          "* DONE Beta\n"
          ":PROPERTIES:\n"
          ":DORG_ID: 200\n"
          ":END:\n"
          "body beta\n")
  "Two-heading plain-org fixture; heading 100 is the surgical target,
heading 200 pins byte-fidelity outside the requested region.")

(defmacro dorg-test-persist--with-dir (var &rest body)
  "Bind VAR to a fresh private directory, run BODY, then clean up.
Kills any buffer visiting a file under VAR (modified flag cleared, no
prompts) and removes the directory recursively."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "dorg-persist-spec-" t)))
     (unwind-protect (progn ,@body)
       (dorg-test-persist--kill-visiting ,var)
       (delete-directory ,var t))))

(defun dorg-test-persist--kill-visiting (dir)
  "Kill every buffer visiting a file under DIR without prompting."
  (let ((prefix (file-name-as-directory (file-truename dir))))
    (dolist (buf (buffer-list))
      (let ((f (buffer-file-name buf)))
        (when (and f (string-prefix-p prefix (file-truename f)))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(defun dorg-test-persist--make-file (dir &optional name)
  "Write the fixture into DIR as NAME (default beads.org); return the path."
  (let ((file (expand-file-name (or name "beads.org") dir))
        (coding-system-for-write 'no-conversion))
    (write-region dorg-test-persist--fixture nil file nil 'silent)
    file))

(defun dorg-test-persist--slurp (file)
  "Return FILE's exact raw bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun dorg-test-persist--inode (file)
  "Return FILE's inode number."
  (file-attribute-inode-number (file-attributes file)))

(defun dorg-test-persist--temp-siblings (dir)
  "Return leftover DOrg persist temp files in DIR."
  (directory-files dir nil "dorg-persist-"))

(defun dorg-test-persist--apply (file &rest options)
  "Public-contract apply on FILE: set STATUS on heading 100 via
`dorg-api-execute'.  OPTIONS are extra `dorg-api-execute' keywords."
  (apply #'dorg-api-execute '(on (id 100) (set (property "STATUS") "DONE"))
         :mode 'apply :source (list :file file) options))

(defun dorg-test-persist--reason+detail (thunk)
  "Call THUNK; return (REASON . DETAIL) of a signalled `dorg-mutate-error'
or `dorg-api-error', or `:no-error' when THUNK returns normally."
  (condition-case err (progn (funcall thunk) :no-error)
    (dorg-mutate-error
     (let ((data (dorg-mutate-error-data err)))
       (cons (plist-get data :reason) (plist-get data :detail))))
    (dorg-api-error
     (let ((data (dorg-api-error-data err)))
       (cons (plist-get data :reason) (plist-get data :detail))))))

(defun dorg-test-persist--replace-equal-stat (file)
  "Replace FILE with same-size different bytes and restore its mtime.
Returns the replacement bytes.  After this, stat (size+mtime) is
indistinguishable from the original — only content differs."
  (let* ((old (dorg-test-persist--slurp file))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (new (copy-sequence old)))
    ;; "body alpha" -> "Xody alpha": one byte differs, length identical.
    (aset new (string-match "body alpha" new) ?X)
    (let ((coding-system-for-write 'no-conversion))
      (write-region new nil file nil 'silent))
    (set-file-times file mtime)
    new))

;;; ─── AC1/AC3: rename-based atomic commit through the public contract ───

(ert-deftest dorg-test-persist-file-apply-commits-atomically ()
  "A file-source apply through `dorg-api-execute' commits by atomic rename:
the destination inode CHANGES (complete new file swapped in — never an
in-place rewrite), and the result is byte-exact: EVERY byte outside the one
inserted property line is pinned by exact prefix/suffix comparison, so even a
byte-different re-render of the touched heading fails.  No temp sibling
remains.
RED reason: today the destination is rewritten in place by a direct
`write-region', so the inode does not change."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file))
           (ino-before (dorg-test-persist--inode file))
           (r (dorg-test-persist--apply file)))
      (should (eq t (plist-get r :ok)))
      (let* ((after (dorg-test-persist--slurp file))
             ;; The ONLY change is one property line inserted inside heading
             ;; 100's PROPERTIES drawer: PRE is every byte before the
             ;; insertion point, POST every byte after it.  A whole-heading
             ;; re-render (realigned drawer, reflowed body, tag column
             ;; drift) fails these pins.
             (pre "* TODO Alpha\n:PROPERTIES:\n:DORG_ID: 100\n")
             (post (concat ":END:\nbody alpha\n"
                           "* DONE Beta\n:PROPERTIES:\n:DORG_ID: 200\n"
                           ":END:\nbody beta\n")))
        ;; Self-check: the pins reconstruct the fixture exactly.
        (should (equal before (concat pre post)))
        (should-not (equal before after))
        (should (string-prefix-p pre after))
        (should (string-suffix-p post after))
        (let ((mid (substring after (length pre)
                              (- (length after) (length post)))))
          (should (string-match-p "\\`:STATUS:[ \t]*DONE\n\\'" mid)))
        ;; Rename-based commit, not in-place write.
        (should-not (eql ino-before (dorg-test-persist--inode file)))
        (should-not (dorg-test-persist--temp-siblings dir))))))

(ert-deftest dorg-test-persist-file-mode-preserved-across-rename ()
  "The committed file keeps the destination's permission bits across the
temp+rename protocol (a temp file's default 0600 must not leak through).
RED reason: the inode assertion — today's in-place write trivially keeps
modes but never renames."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir)))
      (set-file-modes file #o640)
      (let ((ino-before (dorg-test-persist--inode file)))
        (dorg-test-persist--apply file)
        (should (eql #o640 (file-modes file)))
        (should-not (eql ino-before (dorg-test-persist--inode file)))))))

;;; ─── AC2: pre-rename failure leaves the destination byte-identical ─────

(ert-deftest dorg-test-persist-injected-write-failure-leaves-destination-byte-identical ()
  "An injected failure in the temp-write seam (partial temp written, then
error) propagates, leaves the destination byte-identical, and removes the
temp sibling.
RED reason: the seam `dorg-mutate--persist-write-temp' does not exist, so
no error is injected — the apply succeeds and `should-error' fails (and the
destination is clobbered in place)."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file)))
      (cl-letf (((symbol-function 'dorg-mutate--persist-write-temp)
                 (lambda (tmp _bytes)
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region "PARTIAL" nil tmp nil 'silent))
                   (error "dorg-test injected write failure"))))
        (should-error (dorg-test-persist--apply file)))
      (should (equal before (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-injected-rename-failure-cleans-temp ()
  "An injected failure in the rename seam propagates, leaves the destination
byte-identical, and removes the fully-written temp sibling.
RED reason: the seam `dorg-mutate--persist-rename' does not exist, so no
error is injected — the apply succeeds and `should-error' fails."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file)))
      (cl-letf (((symbol-function 'dorg-mutate--persist-rename)
                 (lambda (&rest _) (error "dorg-test injected rename failure"))))
        (should-error (dorg-test-persist--apply file)))
      (should (equal before (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-quit-before-commit-restores-buffer-and-destination ()
  "A `quit' raised before the commit rename propagates; the destination is
byte-identical, the temp is removed, and the live buffer is restored to its
pre-mutation snapshot (text AND modified flag).
RED reason: the seam does not exist, so no quit fires — the apply saves via
`save-buffer' and the `quit-signalled' assertion fails."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file))
           (buf (find-file-noselect file))
           (quit-signalled nil))
      (cl-letf (((symbol-function 'dorg-mutate--persist-rename)
                 (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (dorg-test-persist--apply file :save t)
          (quit (setq quit-signalled t))))
      (should quit-signalled)
      (should (equal before (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir))
      (should (equal dorg-test-persist--fixture
                     (with-current-buffer buf
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (should-not (buffer-modified-p buf)))))

;;; ─── AC4: content CAS — stat equality is not freshness ─────────────────

(ert-deftest dorg-test-persist-equal-stat-replacement-rejected-by-public-base ()
  "A public `:base' captured at dry-run detects an equal-stat content
replacement (same size, same mtime, different bytes) at apply: the plain-org
`:base' token carries the exact content fingerprint, so the command-layer CAS
refuses with exact (stale . base-revision) and the external replacement
survives.  Fully public dry-run -> capture `:base' -> apply flow through
`dorg-api-execute'.
RED reason: today the plain-org `:base' token is stat-shaped (mtime), the
equal-stat comparison passes, and the apply OVERWRITES the replacement — the
exact defect this bead fixes."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (plan (dorg-api-execute
                  '(on (id 100) (set (property "STATUS") "DONE"))
                  :mode 'dry-run :source (list :file file)))
           (base (plist-get plan :base))
           (replacement (dorg-test-persist--replace-equal-stat file)))
      (should base)
      ;; Precondition of the hole: the replacement is invisible to stat.
      (should (equal (plist-get base :mtime)
                     (file-attribute-modification-time
                      (file-attributes file))))
      (should (equal '(stale . base-revision)
                     (dorg-test-persist--reason+detail
                      (lambda () (dorg-test-persist--apply file :base base)))))
      (should (equal replacement (dorg-test-persist--slurp file))))))

(ert-deftest dorg-test-persist-stale-at-entry-clean-buffer-replacement-rejected ()
  "With NO tokens supplied, a clean visiting buffer whose file was replaced
equal-stat externally is refused at entry with exact (stale . stale-at-entry);
the replacement survives and the buffer is untouched (refusal precedes any
mutation).
RED reason: today nothing compares content — `save-buffer' silently clobbers
the external replacement, so no error is signalled."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file))
           (replacement (dorg-test-persist--replace-equal-stat file)))
      (should-not (buffer-modified-p buf))
      ;; Precondition of the hole: stat-based modtime verification still passes.
      (should (verify-visited-file-modtime buf))
      (should (equal '(stale . stale-at-entry)
                     (dorg-test-persist--reason+detail
                      (lambda () (dorg-test-persist--apply file :save t)))))
      (should (equal replacement (dorg-test-persist--slurp file)))
      (should (equal dorg-test-persist--fixture
                     (with-current-buffer buf
                       (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest dorg-test-persist-replacement-racing-read-rejected ()
  "A destination replaced between the executor's read and its commit fails
the pre-rename fingerprint recheck with exact (stale . destination-drift);
the racing replacement survives and the temp is removed.  The baseline is
the exact bytes READ, so the race window is content-checked, not stat-checked.
RED reason: today the executor write-regions straight over the destination —
the racing replacement is clobbered and no error is signalled."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (racing "* TODO Raced\n:PROPERTIES:\n:DORG_ID: 900\n:END:\nraced\n")
           (orig (symbol-function 'dorg-mutate--source-preserving-run)))
      (cl-letf (((symbol-function 'dorg-mutate--source-preserving-run)
                 (lambda (&rest args)
                   ;; Simulate an external writer landing after the read.
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region racing nil file nil 'silent))
                   (apply orig args))))
        (should (equal '(stale . destination-drift)
                       (dorg-test-persist--reason+detail
                        (lambda () (dorg-test-persist--apply file))))))
      (should (equal racing (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

;;; ─── AC5: trustworthy baselines for live buffers ───────────────────────

(ert-deftest dorg-test-persist-modified-buffer-without-baseline-refused ()
  "A user-modified visiting buffer with no recorded baseline is refused with
exact (stale . baseline-untrusted) BEFORE any mutation or write; a late disk
hash is never adopted for stale buffer contents.  The disk stays untouched.
RED reason: today the apply mutates the buffer and `save-buffer' persists it
(user edit included) — no refusal is signalled."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file))
           (buf (find-file-noselect file)))
      ;; A real user edit: outside any programmatic-change guard.
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "user edit\n"))
      (should (buffer-modified-p buf))
      (should (equal '(stale . baseline-untrusted)
                     (dorg-test-persist--reason+detail
                      (lambda () (dorg-test-persist--apply file :save t)))))
      (should (equal before (dorg-test-persist--slurp file))))))

(ert-deftest dorg-test-persist-mutate-then-save-trusted-baseline-commits ()
  "A `:save nil' apply over a CLEAN buffer records the trustworthy baseline;
a following `:save t' apply on the (now DOrg-modified) buffer commits, the
disk carries both edits, and the response `:new-token' carries the committed
content `:fingerprint'.
RED reason: revision tokens carry no `:fingerprint' today."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file)))
      ;; Apply #1: buffer-only mutation records the baseline while clean.
      (dorg-test-persist--apply file)
      (should (buffer-modified-p buf))
      ;; Apply #2: persists the modified buffer under the recorded baseline.
      (let ((r (dorg-api-execute '(on (id 200) (set (property "OWNER") "bob"))
                                 :mode 'apply :source (list :file file) :save t)))
        (let ((disk (dorg-test-persist--slurp file)))
          (should (string-match-p ":STATUS:[ \t]+DONE" disk))
          (should (string-match-p ":OWNER:[ \t]+bob" disk))
          (should-not (buffer-modified-p buf))
          (let ((fp (plist-get (plist-get r :new-token) :fingerprint)))
            (should (stringp fp))
            (should (equal fp (dorg-mutate--persist-fingerprint disk)))))))))

;;; ─── AC6: post-commit coherence ────────────────────────────────────────

(ert-deftest dorg-test-persist-post-commit-buffer-coherence ()
  "After a successful live-buffer `:save t' commit: the buffer is unmodified,
its visited modtime verifies, the fresh revision token's `:fingerprint'
matches the returned `:new-token''s, and the returned `:new-token' is
acceptable as the NEXT public `:base' — an immediate follow-up
`dorg-api-execute' apply under it succeeds (no spurious stale).
RED reason: revision tokens carry no `:fingerprint' today."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file))
           (r (dorg-test-persist--apply file :save t)))
      (should-not (buffer-modified-p buf))
      (should (verify-visited-file-modtime buf))
      ;; Unit isolation of the internal token seam (not a legacy public call):
      ;; the committed fingerprint is the one a fresh token derivation sees.
      (let* ((src (dorg-mutate--resolve-source (list :file file)))
             (tok (plist-get r :new-token))
             (fresh (dorg-mutate--plain-org-revision src)))
        (should (stringp (plist-get tok :fingerprint)))
        (should (equal (plist-get tok :fingerprint)
                       (plist-get fresh :fingerprint)))
        (let ((r2 (dorg-api-execute
                   '(on (id 200) (set (property "OWNER") "bob"))
                   :mode 'apply :source (list :file file)
                   :base tok :save t)))
          (should (eq t (plist-get r2 :ok))))))))

;;; ─── AC3 (protocol details): O_EXCL same-dir temp, fsync, no save-buffer ─

(ert-deftest dorg-test-persist-fsync-requested-temp-same-directory ()
  "The temp sibling is created in the DESTINATION's directory (same
filesystem, `.dorg-persist-' pattern) and the full write runs with
`write-region-inhibit-fsync' nil — bound by `dorg-mutate--plain-org-commit'
AROUND the write seam, so the requested fsync is observable (and injectable)
at the seam boundary.  The variable defaults to t in batch, so the nil
binding is an explicit fsync request.
RED reason: the create/write seams do not exist, so the recordings stay nil."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (rec-tmp nil) (rec-existed 'never) (rec-fsync 'never)
           (orig-create (symbol-function 'dorg-mutate--persist-create-temp))
           (orig-write (symbol-function 'dorg-mutate--persist-write-temp)))
      (cl-letf (((symbol-function 'dorg-mutate--persist-create-temp)
                 (lambda (&rest args)
                   (let ((tmp (apply orig-create args)))
                     (setq rec-tmp tmp
                           rec-existed (file-exists-p tmp))
                     tmp)))
                ((symbol-function 'dorg-mutate--persist-write-temp)
                 (lambda (&rest args)
                   (setq rec-fsync write-region-inhibit-fsync)
                   (apply orig-write args))))
        (dorg-test-persist--apply file))
      (should rec-tmp)
      ;; Exclusive creation: the temp exists the moment the seam returns.
      (should (eq rec-existed t))
      (should (equal (file-name-as-directory (file-name-directory rec-tmp))
                     (file-name-as-directory
                      (file-name-directory (file-truename file)))))
      (should (string-match-p "\\.dorg-persist-"
                              (file-name-nondirectory rec-tmp)))
      (should (eq rec-fsync nil)))))

(ert-deftest dorg-test-persist-temp-creation-exclusive-and-distinct ()
  "Two temp creations for the SAME destination, neither yet consumed, yield
DISTINCT sibling paths that both already exist in the destination's
directory with the `.dorg-persist-' pattern — the O_EXCL semantic contract:
no fixed temp name that a concurrent writer could reuse or clobber.  Direct
internal-seam isolation (not a legacy public call).
RED reason: `dorg-mutate--persist-create-temp' does not exist
(void-function)."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (t1 (dorg-mutate--persist-create-temp file))
           (t2 (dorg-mutate--persist-create-temp file)))
      (unwind-protect
          (progn
            (should-not (equal t1 t2))
            (should (file-exists-p t1))
            (should (file-exists-p t2))
            (dolist (tmp (list t1 t2))
              (should (equal (file-name-as-directory
                              (file-name-directory tmp))
                             (file-name-as-directory
                              (file-name-directory (file-truename file)))))
              (should (string-match-p "\\.dorg-persist-"
                                      (file-name-nondirectory tmp)))))
        (dolist (tmp (list t1 t2))
          (when (and (stringp tmp) (file-exists-p tmp))
            (delete-file tmp)))))))

(ert-deftest dorg-test-persist-symlink-destination-commits-to-target ()
  "A symlink destination commits through its truename: the temp sibling is
created in the TARGET's directory (same filesystem as the target), the
target receives the new content, and the symlink itself is preserved (a
rename over the link path would replace the link with a regular file).
RED reason: the create-temp seam does not exist, so the recording stays nil."
  (dorg-test-persist--with-dir dir
    (let* ((target-dir (file-name-as-directory
                        (expand-file-name "real" dir)))
           (link-dir (file-name-as-directory
                      (expand-file-name "linked" dir)))
           (_ (progn (make-directory target-dir) (make-directory link-dir)))
           (target (dorg-test-persist--make-file target-dir))
           (link (expand-file-name "beads-link.org" link-dir))
           (rec-tmp nil)
           (orig-create (symbol-function 'dorg-mutate--persist-create-temp)))
      (make-symbolic-link target link)
      (cl-letf (((symbol-function 'dorg-mutate--persist-create-temp)
                 (lambda (&rest args)
                   (let ((tmp (apply orig-create args)))
                     (setq rec-tmp tmp)
                     tmp))))
        (dorg-test-persist--apply link))
      (should (file-symlink-p link))
      (should (string-match-p ":STATUS:[ \t]+DONE"
                              (dorg-test-persist--slurp target)))
      (should rec-tmp)
      (should (equal (file-name-as-directory (file-name-directory rec-tmp))
                     (file-name-as-directory
                      (file-name-directory (file-truename target)))))
      (should-not (dorg-test-persist--temp-siblings link-dir)))))

(ert-deftest dorg-test-persist-live-save-avoids-raw-save-buffer ()
  "A successful live-buffer `:save t' apply never calls `save-buffer' (the
persistence primitive replaces it entirely — save-hooks and the backup
machinery must not rewrite the source), yet the disk is updated and the
buffer ends unmodified.
RED reason: `save-buffer' IS today's live-buffer save path — the spy fires."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file))
           (called nil)
           (orig (symbol-function 'save-buffer)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (setq called t)
                   (apply orig args))))
        (dorg-test-persist--apply file :save t))
      (should-not called)
      (should (string-match-p ":STATUS:[ \t]+DONE"
                              (dorg-test-persist--slurp file)))
      (should-not (buffer-modified-p buf)))))

;;; ─── Pre-commit audit blockers (OC 2026-07-16T23:09/23:10) ─────────────

(ert-deftest dorg-test-persist-encode-nil-coding-normalized-roundtrips ()
  "`dorg-mutate--persist-encode' NORMALIZES a nil coding to utf-8-unix rather
than letting nil reach `encode-coding-string' as a no-conversion request: the
result is unibyte utf-8 that decodes back to the original multibyte text, and is
byte-identical to an explicit utf-8-unix encode.
RED reason (pre-fix): nil fell through to the else arm (coding-system-base of
utf-8 is not undecided/prefer-utf-8), so nil was used verbatim and multibyte
content was mis-encoded."
  (let* ((text "* café ☕\n:PROPERTIES:\n:DORG_ID: 1\n:END:\nbody\n")
         (bytes (dorg-mutate--persist-encode text nil)))
    (should-not (multibyte-string-p bytes))
    (should (equal text (decode-coding-string bytes 'utf-8-unix)))
    (should (equal bytes (dorg-mutate--persist-encode text 'utf-8-unix)))
    (should (equal bytes (encode-coding-string text 'utf-8-unix t)))))

(ert-deftest dorg-test-persist-encode-faithful-refuses-lossy-coding ()
  "`dorg-mutate--persist-encode-faithful' PROVES encode/decode round-trip: it
refuses (stale . stale-at-entry) when the coding cannot byte-round-trip the text
\(fidelity unknowable), rather than silently writing lossy bytes; a round-tripping
coding returns the unibyte bytes.
RED reason (pre-fix): no round-trip guard existed — a lossy re-encode produced
corrupt bytes with no refusal."
  ;; us-ascii cannot represent 'é'/'☕'; a faithful encode must refuse.
  (should (equal '(stale . stale-at-entry)
                 (dorg-test-persist--reason+detail
                  (lambda ()
                    (dorg-mutate--persist-encode-faithful "café ☕\n" 'us-ascii)))))
  ;; utf-8 round-trips: returns unibyte bytes that decode back exactly.
  (let ((bytes (dorg-mutate--persist-encode-faithful "café ☕\n" 'utf-8-unix)))
    (should-not (multibyte-string-p bytes))
    (should (equal "café ☕\n" (decode-coding-string bytes 'utf-8-unix)))))

(ert-deftest dorg-test-persist-file-baseline-tied-to-single-read ()
  "The file-source commit baseline is the fingerprint of the SAME single read
that produced the decoded work text — NOT a separate disk read.  When that one
read yields bytes differing from the CURRENT on-disk bytes, the pre-rename
recheck fails (stale . destination-drift), the on-disk bytes survive, and no
temp remains: a mutation is never committed over bytes the executor never read
\(the between-read / ABA hole).
RED reason (pre-fix): the baseline came from a separate persist-file-fingerprint
read of the disk, so the disk matched itself at recheck and the seam-supplied
work text was committed over it (no drift signalled)."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           ;; The disk holds DIFFERENT bytes than the single read returns.
           (on-disk "* TODO Other\n:PROPERTIES:\n:DORG_ID: 100\n:END:\ndisk body\n"))
      (let ((coding-system-for-write 'no-conversion))
        (write-region on-disk nil file nil 'silent))
      ;; Pin the ONE literal read to the ORIGINAL fixture (has heading 100 so the
      ;; mutation resolves), while the disk is `on-disk'.
      (cl-letf (((symbol-function 'dorg-mutate--persist-file-read)
                 (lambda (&rest _) dorg-test-persist--fixture)))
        (should (equal '(stale . destination-drift)
                       (dorg-test-persist--reason+detail
                        (lambda () (dorg-test-persist--apply file))))))
      (should (equal on-disk (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-post-rename-quit-reports-committed ()
  "A `quit' that becomes visible AFTER the atomic rename (surfacing at the
post-rename seam, with the explicit committed flag already set) is reported as
COMMITTED, not a failed uncommitted write: the new image is on disk, the live
buffer holds the NEW text (never restored to the pre-mutation snapshot), is
coherent (unmodified, modtime verifies), and the apply returns :ok.
RED reason (pre-fix): the caller restored the pre-mutation buffer for ANY
error/quit even after the rename, yielding disk=new + buffer=old + a signaled
failure (the quit propagated out of apply)."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file))
           (r nil))
      ;; A deferred interrupt surfaces at the post-rename seam — AFTER the commit
      ;; point (rename done, committed flag set): the realistic post-rename window.
      (cl-letf (((symbol-function 'dorg-mutate--persist-post-rename)
                 (lambda () (signal 'quit nil))))
        (setq r (dorg-test-persist--apply file :save t)))
      ;; Committed + coherent, NOT old-buffer-plus-failure.
      (should (eq t (plist-get r :ok)))
      (should (string-match-p ":STATUS:[ \t]+DONE" (dorg-test-persist--slurp file)))
      (with-current-buffer buf
        (should (string-match-p
                 ":STATUS:[ \t]+DONE"
                 (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (buffer-modified-p buf))
        (should (verify-visited-file-modtime buf)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-file-post-rename-quit-reports-committed ()
  "File-only: a `quit' surfacing at the post-rename seam (after the commit
point) reports the commit (disk = new, apply :ok), not a post-write failure —
the file branch carries the same explicit committed flag as the buffer branch.
RED reason (pre-fix): the file branch had no explicit commit-point flag, so a
post-rename quit propagated as failure though the write had landed."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (r nil))
      (cl-letf (((symbol-function 'dorg-mutate--persist-post-rename)
                 (lambda () (signal 'quit nil))))
        (setq r (dorg-test-persist--apply file)))
      (should (eq t (plist-get r :ok)))
      (should (string-match-p ":STATUS:[ \t]+DONE" (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-noop-content-prerename-failure-propagates ()
  "A no-op / equal-content mutation (rendered bytes == current disk bytes) whose
commit fails BEFORE the rename still PROPAGATES the failure and is NOT reported
saved: the commit point is an explicit post-rename flag, never inferred from
destination==new-bytes (which a no-op satisfies BEFORE the rename).
RED reason (fingerprint-inference impl): destination already equalled the new
fingerprint, so the injected pre-rename failure was misclassified as committed
and swallowed."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file)))
      (cl-letf (((symbol-function 'dorg-mutate--source-preserving-run)
                 (lambda (&rest _) nil))   ; no-op: rendered bytes == disk bytes
                ((symbol-function 'dorg-mutate--persist-rename)
                 (lambda (&rest _) (error "dorg-test injected pre-rename failure"))))
        (should-error (dorg-test-persist--apply file)))
      (should (equal before (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-file-non-roundtrip-raw-refused ()
  "A file whose EXACT on-disk bytes cannot byte-round-trip under their coding
\(here CRLF, whose normalized re-encode is unix) is refused (stale .
stale-at-entry) BEFORE any mutation rather than silently rewriting every line
ending; the file survives byte-identical.
RED reason (text-level round-trip only): decode(encode text)==text holds for a
CRLF file (EOL is canonicalized into the text), so the executor would re-encode
and rewrite all line endings."
  (dorg-test-persist--with-dir dir
    (let* ((file (expand-file-name "crlf.org" dir))
           (raw (concat "* TODO Alpha\r\n:PROPERTIES:\r\n:DORG_ID: 100\r\n"
                        ":END:\r\nbody\r\n")))
      (let ((coding-system-for-write 'no-conversion))
        (write-region raw nil file nil 'silent))
      (let ((before (dorg-test-persist--slurp file)))
        (should (equal '(stale . stale-at-entry)
                       (dorg-test-persist--reason+detail
                        (lambda () (dorg-test-persist--apply file)))))
        (should (equal before (dorg-test-persist--slurp file)))))))

(ert-deftest dorg-test-persist-save-nil-touches-no-destination-seam ()
  "A live-buffer `:save nil' apply performs NO destination I/O: EVERY persist
destination seam (file fingerprint, single read, temp create, temp write,
rename) is stubbed to error, yet the apply succeeds, mutates the buffer IN
PLACE, and leaves the on-disk file byte-unchanged with no temp sibling.
RED reason (pre-fix): the executor unconditionally established the destination
trust baseline (hashing the FILE via `dorg-mutate--plain-org-buffer-baseline')
before the surgical run, so a destination seam fired — and could even refuse
stale-at-entry — when not saving."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (before (dorg-test-persist--slurp file))
           (buf (find-file-noselect file)))
      (cl-letf (((symbol-function 'dorg-mutate--persist-file-fingerprint)
                 (lambda (&rest _) (error "dorg-test: file fingerprint under :save nil")))
                ((symbol-function 'dorg-mutate--persist-file-read)
                 (lambda (&rest _) (error "dorg-test: file read under :save nil")))
                ((symbol-function 'dorg-mutate--persist-create-temp)
                 (lambda (&rest _) (error "dorg-test: temp create under :save nil")))
                ((symbol-function 'dorg-mutate--persist-write-temp)
                 (lambda (&rest _) (error "dorg-test: temp write under :save nil")))
                ((symbol-function 'dorg-mutate--persist-rename)
                 (lambda (&rest _) (error "dorg-test: rename under :save nil"))))
        (let ((r (dorg-test-persist--apply file)))   ; :save absent -> nil
          (should (eq t (plist-get r :ok)))))
      (with-current-buffer buf
        (should (buffer-modified-p buf))
        (should (string-match-p
                 ":STATUS:[ \t]+DONE"
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (should (equal before (dorg-test-persist--slurp file)))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-file-lossy-mutation-refused ()
  "A file-source mutation that introduces text UNREPRESENTABLE in the file's
coding fails closed (stale . stale-at-entry) BEFORE any write: the file branch
encodes the RESULT through the FAITHFUL encoder (the entry raw round-trip only
proved the PRE-mutation bytes).  The file survives byte-identical, no temp
remains.
RED reason (pre-fix): the file branch encoded the result with a non-checking
encoder, silently writing lossy substitutions."
  (dorg-test-persist--with-dir dir
    (let* ((file (expand-file-name "latin1.org" dir))
           ;; A latin-1 file: detected coding is iso-latin-1, under which the
           ;; original bytes round-trip but a `☕' cannot be represented.
           (raw (encode-coding-string
                 "* TODO Alpha\n:PROPERTIES:\n:DORG_ID: 100\n:END:\ncafé\n"
                 'iso-latin-1))
           (orig (symbol-function 'dorg-mutate--source-preserving-run)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region raw nil file nil 'silent))
      (let ((before (dorg-test-persist--slurp file)))
        (cl-letf (((symbol-function 'dorg-mutate--source-preserving-run)
                   (lambda (&rest args)
                     (prog1 (apply orig args)
                       ;; a char outside the file's latin-1 coding enters the result
                       (goto-char (point-max)) (insert "☕")))))
          (should (equal '(stale . stale-at-entry)
                         (dorg-test-persist--reason+detail
                          (lambda () (dorg-test-persist--apply file))))))
        (should (equal before (dorg-test-persist--slurp file)))
        (should-not (dorg-test-persist--temp-siblings dir))))))

(defun dorg-test-persist--assert-persistent-post-commit-fault (fault)
  "Apply `:save t' with the post-commit bookkeeping helper PERSISTENTLY faulted
by FAULT (a nullary thunk that signals on EVERY call), and assert the commit is
reported truthfully with a FULLY COHERENT buffer via the independent recovery
path: disk=new, buffer text=new, modified=nil, visited modtime current,
baseline=the committed fingerprint (a follow-up save under the returned token
succeeds)."
  (dorg-test-persist--with-dir dir
    (let* ((file (dorg-test-persist--make-file dir))
           (buf (find-file-noselect file))
           (r nil))
      (cl-letf (((symbol-function 'dorg-mutate--plain-org-post-commit)
                 (lambda (&rest _) (funcall fault))))
        (setq r (dorg-test-persist--apply file :save t)))
      (should (eq t (plist-get r :ok)))
      (should (string-match-p ":STATUS:[ \t]+DONE" (dorg-test-persist--slurp file)))
      (with-current-buffer buf
        ;; buffer text=new, modified=nil, visited modtime current (full coherence)
        (should (string-match-p
                 ":STATUS:[ \t]+DONE"
                 (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (buffer-modified-p buf))
        (should (verify-visited-file-modtime buf)))
      ;; baseline=new-fp: a follow-up apply under the returned token succeeds.
      (let ((r2 (dorg-api-execute '(on (id 200) (set (property "OWNER") "bob"))
                                  :mode 'apply :source (list :file file)
                                  :base (plist-get r :new-token) :save t)))
        (should (eq t (plist-get r2 :ok))))
      (should-not (dorg-test-persist--temp-siblings dir)))))

(ert-deftest dorg-test-persist-persistent-post-commit-error-reports-committed ()
  "A PERSISTENT post-commit bookkeeping ERROR (every attempt) past the explicit
rename commit point does NOT escape as an API failure and leaves the buffer
FULLY COHERENT: the recovery path completes modtime/modified/baseline through
the primitives directly, not the faulted helper.
RED reason (pre-fix): the handler re-invoked the same (faulted) helper, so a
persistent fault re-raised and escaped after disk=new — or a later
`ignore-errors' swallowed it but left the buffer modified/incoherent."
  (dorg-test-persist--assert-persistent-post-commit-fault
   (lambda () (error "dorg-test persistent post-commit error"))))

(ert-deftest dorg-test-persist-persistent-post-commit-quit-reports-committed ()
  "As above but the persistent post-commit fault is a `quit' (not an error): the
committed handler catches BOTH, never re-signals past the commit point, and the
buffer ends fully coherent.
RED reason (pre-fix): `ignore-errors' does not catch `quit', so a post-commit
quit escaped the apply after disk=new."
  (dorg-test-persist--assert-persistent-post-commit-fault
   (lambda () (signal 'quit nil))))

(provide 'dorg-mutate-persist-tests)
;;; dorg-mutate-persist-tests.el ends here
