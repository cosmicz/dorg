;;; dorg-projection-tests.el --- Tests for dorg-projection -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the projection-buffer utility module (tracked-regression): the
;; Family A relocation contract guard plus the behavioral coverage moved
;; from the sync test file (region calculation and the tracked-case
;; marker-scan SPEC).  The tracked-regression contract also moves the live
;; programmatic-change capability and established-view state here, leaving
;; base/dirty/authored reconciliation state behind for deletion.
;;
;; The guard assembles the retired names from fragments so a post-relocation
;; repository grep for the full old names is zero-hit, including this file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'dorg-projection)

(defconst dorg-projection-tests--repo-dir
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root, captured at load (never derived from implicit cwd).")

(defun dorg-projection-tests--name-hits (names)
  "Return files whose text mentions any full retired name in NAMES.
Scans root `*.el', `tests/*.el', and `tools/**/*.el' under the repo root.
Callers assemble NAMES from fragments so this file is never a hit.
Matches are symbol-bounded, so a longer symbol sharing a retired name
as its prefix (e.g. a private plural sibling) is not a false positive."
  (let ((root dorg-projection-tests--repo-dir)
        (re (concat "\\_<" (regexp-opt names) "\\_>"))
        (hits nil))
    (dolist (f (append
                (directory-files root t "\\.el\\'")
                (directory-files (expand-file-name "tests" root) t "\\.el\\'")
                (directory-files-recursively
                 (expand-file-name "tools" root) "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (push (file-name-nondirectory f) hits))))
    (nreverse hits)))

(ert-deftest dorg-test-projection-relocation-contract ()
  "tracked-regression Family A: buffer helpers live in dorg-projection.el.
RED at SPEC (new names undefined, retired names still defined and
referenced); GREEN after IMPL (public projection names defined, the
sync-prefixed originals deleted repository-wide)."
  (let ((pairs '(("-heading-section-region" . dorg-projection-section-region)
                 ("-heading-subtree-region" . dorg-projection-subtree-region)
                 ("-replace-region"         . dorg-projection-replace-region)
                 ("-insert-point"           . dorg-projection-insert-point)
                 ("-scan-markers"           . dorg-projection-scan-markers)
                 ("-refresh-region-markers" . dorg-projection-refresh-region-markers)))
        (old-prefix (concat "dorg-sync" "-")))
    (dolist (p pairs)
      (let ((old (intern-soft (concat old-prefix (car p)))))
        (should (fboundp (cdr p)))
        (should-not (and old (fboundp old)))))
    (should-not (dorg-projection-tests--name-hits
                 (mapcar (lambda (p) (concat old-prefix (car p))) pairs)))))

;;; ─── Live projection-state relocation (tracked-regression) ─────────────

(ert-deftest dorg-test-projection-state-relocation-contract ()
  "Live view/owned-change state has neutral names and no compatibility aliases.
RED before tracked-regression because the neutral functions are absent and the
sync-state spellings remain defined/referenced.  Retired names are assembled
from fragments so this guard can require a repository-wide zero-hit result."
  (let* ((new '(dorg-projection-call-with-programmatic-changes
                dorg-projection-with-programmatic-changes
                dorg-projection-programmatic-change-p
                dorg-projection-install-view
                dorg-projection-clear-view
                dorg-projection-view-context
                dorg-projection-state-snapshot
                dorg-projection-state-restore))
         (old-prefix (concat "dorg-" "sync-state--"))
         (old (mapcar
               (lambda (suffix) (concat old-prefix suffix))
               '("call-with-programmatic-changes"
                 "with-programmatic-changes"
                 "programmatic-change-p"
                 "install-materialized-view"
                 "clear-materialized-view"
                 "rollback-snapshot"
                 "restore-rollback-snapshot")))
         (old-private
          (mapcar (lambda (suffix) (concat "dorg--" suffix))
                  '("programmatic-change-depth"
                    "materialized-view"
                    "make-mview"
                    "mview-p"
                    "mview-source"
                    "mview-root"
                    "mview-revision"
                    "mview-scope"
                    "mview-window"))))
    (dolist (symbol new)
      (should (fboundp symbol)))
    (dolist (name (append old old-private))
      (let ((symbol (intern-soft name)))
        (should-not (and symbol
                         (or (fboundp symbol) (boundp symbol))))))
    (should-not (dorg-projection-tests--name-hits
                 (append old old-private)))))

(ert-deftest dorg-test-projection-state-capability-is-owned-and-isolated ()
  "Owned changes nest/unwind and view inputs/outputs cannot mutate live state.
The wrong state is seeded twice: the caller mutates its WINDOW after install,
then mutates a returned context.  Canonical state must remain unchanged, and a
snapshot/clear/restore round trip must recover it."
  (let ((source (generate-new-buffer " *dorg-projection-state-source*"))
        (dest (generate-new-buffer " *dorg-projection-state-dest*")))
    (unwind-protect
        (with-current-buffer dest
          (setq buffer-read-only t)
          (should-not (dorg-projection-programmatic-change-p))
          (dorg-projection-call-with-programmatic-changes
           (lambda ()
             (should (dorg-projection-programmatic-change-p))
             (insert "owned")
             (dorg-projection-call-with-programmatic-changes
              (lambda ()
                (should (dorg-projection-programmatic-change-p))))))
          (should-not (dorg-projection-programmatic-change-p))
          (should buffer-read-only)
          (let ((window (list 10 20)))
            (dorg-projection-install-view
             dest source 7 "rev-a" 'partial window)
            (setcar window 999))
          (let ((context (dorg-projection-view-context dest)))
            (should (equal (plist-get context :window) '(10 20)))
            (setcar (plist-get context :window) 888))
          (should (equal (plist-get (dorg-projection-view-context dest) :window)
                         '(10 20)))
          (should (eq (dorg-materialized-view-scope dest) 'partial))
          (let ((snapshot (dorg-projection-state-snapshot dest)))
            (dorg-projection-clear-view dest)
            (should-not (dorg-materialized-view-scope dest))
            (dorg-projection-state-restore snapshot dest)
            (should (equal (dorg-projection-view-context dest)
                           (list :source source :root-id 7 :revision "rev-a"
                                 :scope 'partial :window '(10 20))))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p dest) (kill-buffer dest)))))

;;; ─── Moved behavioral coverage (from the sync test file) ───────────────

(ert-deftest dorg-test-projection-section-region ()
  "Section region spans from heading line to next heading."
  (with-temp-buffer
    (insert "* Alpha\nBody text.\n\n* Beta\n")
    (org-mode)
    (goto-char (point-min))
    (let ((region (dorg-projection-section-region)))
      ;; Should span "* Alpha\nBody text.\n\n"
      (should (= (car region) (point-min)))
      (should (equal (buffer-substring (car region) (cdr region))
                     "* Alpha\nBody text.\n\n")))))

(ert-deftest dorg-test-projection-section-region-last-heading ()
  "Section region of last heading extends to buffer end."
  (with-temp-buffer
    (insert "* Only\nBody.\n")
    (org-mode)
    (goto-char (point-min))
    (let ((region (dorg-projection-section-region)))
      (should (= (cdr region) (point-max))))))

(ert-deftest dorg-test-projection-subtree-region ()
  "Subtree region includes heading and all descendants."
  (with-temp-buffer
    (insert "* Alpha\n** Child\nBody.\n* Beta\n")
    (org-mode)
    (goto-char (point-min))
    (let ((region (dorg-projection-subtree-region)))
      (should (equal (buffer-substring (car region) (cdr region))
                     "* Alpha\n** Child\nBody.\n")))))

(ert-deftest dorg-test-projection-subtree-region-nested ()
  "Subtree region stops at same-level sibling, not children."
  (with-temp-buffer
    (insert "** A\n*** Deep\n** B\n")
    (org-mode)
    (goto-char (point-min))
    (let ((region (dorg-projection-subtree-region)))
      (should (equal (buffer-substring (car region) (cdr region))
                     "** A\n*** Deep\n")))))

(ert-deftest dorg-test-projection-refresh-region-markers ()
  "`refresh-region-markers' populates heading-markers from DORG_ID properties."
  (with-temp-buffer
    (insert "* One\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
            "* Two\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n")
    (org-mode)
    (let ((heading-markers (make-hash-table :test 'eql)))
      (dorg-projection-refresh-region-markers
       (point-min) (point-max) heading-markers)
      (should (gethash 100 heading-markers))
      (should (gethash 200 heading-markers))
      (save-excursion
        (goto-char (marker-position (gethash 100 heading-markers)))
        (should (looking-at "^\\* One")))
      (save-excursion
        (goto-char (marker-position (gethash 200 heading-markers)))
        (should (looking-at "^\\* Two"))))))

;; --- Marker-scan SPEC (tracked-case) -----------------------------------
;;
;; The perf refactor replaces the per-heading `org-entry-get' call in
;; `dorg-projection-refresh-region-markers' with a plain-text drawer scan.  The
;; empirically-verified `org-entry-get' semantics the scan must reproduce
;; (measured against Emacs 30.2 org, in an org-mode buffer with the default
;; `case-fold-search' = t):
;;
;;   - identity drawer only: the `:PROPERTIES:' block immediately after the
;;     heading (past one optional planning line).  A body `:PROPERTIES:'
;;     block after content is NOT read.
;;   - value = `string-to-number' of the raw drawer value, kept when > 0:
;;       "123abc" -> 123 (kept),  "abc" -> 0 (dropped),  "0" -> 0 (dropped),
;;       "444  " -> 444 (trailing ws trimmed),  "" (bare key) -> 0 (dropped).
;;   - duplicate `:DORG_ID:' lines: LAST wins ("111" then "222" -> 222).
;;   - `:DORG_ID+:' lines accumulate onto the base, space-joined, exactly as
;;     `org-entry-get' combines them: base + "222" -> "111 222" -> 111;
;;     a lone `:DORG_ID+: 424242' with no base -> "424242" -> 424242.  The
;;     scan reproduces this in full (base last-wins + ordered `+' accumulation
;;     + space join + `org-not-nil'), so it matches `org-entry-get' for EVERY
;;     input, not only DOrg's own rendered output.
;;   - property keys are case-insensitive: `:dorg_id:' / `:dorg_id+:' are read.
;;
;; DOrg operates on arbitrary user org files, so the contract is full
;; `org-entry-get' equivalence over any drawer — not just the
;; `:PROPERTIES:\n:DORG_ID: <int>\n:END:' the renderer emits.  Every edge
;; above is pinned so the refactor cannot silently drift from org parity.

(defvar dorg-test--refresh-region-fixture
  (concat
   "* Alpha\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 100\n"
   ":END:\n"
   "Alpha body text.\n"
   "** Beta\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 200\n"
   ":CUSTOM_FOO: bar\n"
   ":END:\n"
   "*** Gamma\n"
   "SCHEDULED: <2026-07-16 Thu>\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 300\n"
   ":END:\n"
   "** Delta\n"
   "No drawer, no DORG_ID here.\n"
   "** Epsilon\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 0\n"
   ":END:\n"
   "** Zeta\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 600\n"
   ":END:\n"
   "Zeta body with a decoy drawer below.\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 999999\n"
   ":END:\n"
   "** Eta\n"
   ":PROPERTIES:\n"
   ":dorg_id: 700\n"
   ":END:\n"
   "** Theta\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 111\n"
   ":DORG_ID: 222\n"
   ":END:\n"
   "** Iota\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 123abc\n"
   ":END:\n"
   "** Kappa\n"
   ":PROPERTIES:\n"
   ":DORG_ID: notanumber\n"
   ":END:\n"
   "** Lambda\n"
   ":PROPERTIES:\n"
   ":DORG_ID: 900\n"
   ":DORG_ID+: 950\n"
   ":END:\n"
   "** Mu\n"
   ":PROPERTIES:\n"
   ":DORG_ID+: 800\n"
   ":END:\n")
  "Rich fixture exercising every semantic edge of the marker scan:
nested levels, a planning line before a drawer, extra drawer props, a
heading without DORG_ID (Delta), DORG_ID 0 (Epsilon, filtered), a body
decoy `:PROPERTIES:' block (Zeta, must NOT be picked up), a
case-insensitive key (Eta `:dorg_id:'), duplicate lines where LAST wins
(Theta), a numeric-prefixed value (Iota `123abc' -> 123), a non-numeric
value (Kappa, filtered), base + `:DORG_ID+:' accumulation (Lambda,
\"900 950\" -> 900), and a lone `:DORG_ID+:' with no base (Mu -> 800).")

(defun dorg-test--refresh-region-markers-oracle (beg end)
  "Independent `org-entry-get' oracle for the marker scan over BEG..END.
Replicates the pre-refactor semantics — walk headings and read each
heading's identity DORG_ID via `org-entry-get' (the org-element path) —
returning a sorted (id . line-begin) alist.  This is the reference
`dorg-projection-refresh-region-markers' must reproduce exactly.  It is
computed OUTSIDE any parser spy so the seam tests can compare against a
real org read."
  (let ((pairs nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (let ((heading-begin (line-beginning-position)))
          (when-let ((id-str (org-entry-get heading-begin "DORG_ID")))
            (let ((id (string-to-number id-str)))
              (when (> id 0)
                (push (cons id heading-begin) pairs)))))))
    (sort pairs (lambda (a b) (< (car a) (car b))))))

(defun dorg-test--marker-hash->alist (heading-markers)
  "Return sorted (id . position) alist for HEADING-MARKERS hash."
  (let ((pairs nil))
    (maphash (lambda (id m) (push (cons id (marker-position m)) pairs))
             heading-markers)
    (sort pairs (lambda (a b) (< (car a) (car b))))))

(ert-deftest dorg-test-projection-refresh-region-markers-equivalence ()
  "Marker scan yields identical pairs to the `org-entry-get' oracle.
Transformation-pinning characterization (GREEN on both the old
`org-entry-get' body and the plain-text refactor): asserts
`dorg-projection-refresh-region-markers' matches (1) a hardcoded expected
id->title set and (2) the independent `org-entry-get' oracle over the
full edge-case fixture.  Breaking the scan diverges from the oracle and
fails this test."
  (with-temp-buffer
    (insert dorg-test--refresh-region-fixture)
    (org-mode)
    ;; --- Whole-buffer scan ------------------------------------------
    (let ((heading-markers (make-hash-table :test 'eql)))
      (dorg-projection-refresh-region-markers
       (point-min) (point-max) heading-markers)
      ;; (1) Exactly the identity DORG_IDs, nothing else.  Expected:
      ;;   100 Alpha, 200 Beta, 300 Gamma (planning line), 600 Zeta,
      ;;   700 Eta (lowercase key), 222 Theta (last dup wins),
      ;;   123 Iota (123abc), 900 Lambda (base of base+DORG_ID+ "900 950"),
      ;;   800 Mu (lone DORG_ID+, no base).
      ;; Excluded: Delta (no drawer), Epsilon (0), Zeta decoy (999999),
      ;;   Kappa (non-numeric).
      (let ((got (dorg-test--marker-hash->alist heading-markers)))
        (should (equal (mapcar #'car got)
                       '(100 123 200 222 300 600 700 800 900)))
        (should-not (gethash 0 heading-markers))
        (should-not (gethash 999999 heading-markers))
        (should-not (gethash 111 heading-markers)) ;; shadowed dup
        (should-not (gethash 950 heading-markers))) ;; DORG_ID+ accum tail
      ;; Each marker sits on its heading line.
      (dolist (spec '((100 . "Alpha") (200 . "Beta") (300 . "Gamma")
                      (600 . "Zeta") (700 . "Eta") (222 . "Theta")
                      (123 . "Iota") (900 . "Lambda") (800 . "Mu")))
        (save-excursion
          (goto-char (marker-position (gethash (car spec) heading-markers)))
          (should (looking-at (format "^\\*+ %s$" (cdr spec))))))
      ;; (2) Byte-for-byte agreement with the independent oracle.
      (should (equal (dorg-test--marker-hash->alist heading-markers)
                     (dorg-test--refresh-region-markers-oracle
                      (point-min) (point-max)))))
    ;; --- Region-scoped scan (Beta subtree only) ---------------------
    (let ((heading-markers (make-hash-table :test 'eql))
          (beg (progn (goto-char (point-min))
                      (re-search-forward "^\\*\\* Beta$")
                      (line-beginning-position)))
          (end (progn (goto-char (point-min))
                      (re-search-forward "^\\*\\* Delta$")
                      (line-beginning-position))))
      (dorg-projection-refresh-region-markers beg end heading-markers)
      ;; Only Beta+Gamma fall in-region; Alpha (before) and everything
      ;; from Delta on (after) are excluded.
      (should (equal (mapcar #'car (dorg-test--marker-hash->alist
                                    heading-markers))
                     '(200 300)))
      (should (equal (dorg-test--marker-hash->alist heading-markers)
                     (dorg-test--refresh-region-markers-oracle beg end))))))

(ert-deftest dorg-test-projection-refresh-region-markers-dorg-id-plus ()
  "`:DORG_ID+:' accumulation + literal-nil match `org-entry-get' (GREEN).
DOrg operates on arbitrary user org files, so the scan must reproduce
`org-entry-get' for `:DORG_ID+:' and literal \"nil\" values too — not just
the base `:DORG_ID: <int>' the renderer emits.  Each case asserts the scan
agrees with the independent org oracle AND yields the expected id (nil = the
heading is dropped).  Combining rule (verified byte-for-byte vs org): the
base `:DORG_ID:' takes its LAST value with `org-not-nil' applied, the
`:DORG_ID+:' values accumulate VERBATIM in order (org-not-nil does NOT
filter individual `+' values — it is applied to the whole extra list), the
parts are space-joined, then `string-to-number' kept when > 0.

Accumulation:
  - a lone `:DORG_ID+:' with no base becomes the value (424242);
  - base + `:DORG_ID+:' stays the base-prefixed number (\"111 222\" -> 111);
  - two `:DORG_ID+:' lines accumulate (\"10 20\" -> 10);
  - a lowercase `:dorg_id+:' is read (777);
  - an empty base + `:DORG_ID+:' takes the accumulated tail (\" 222\" -> 222).
Literal-nil (`org-not-nil'):
  - `:DORG_ID: nil' alone -> dropped;
  - `:DORG_ID+: nil' alone -> dropped;
  - `:DORG_ID: nil' + `:DORG_ID+: 222' -> base nil'd, tail wins -> 222;
  - `:DORG_ID: 111' + `:DORG_ID+: nil' -> \"111 nil\" -> 111 (kept verbatim);
  - `:DORG_ID+: nil' + `:DORG_ID+: 20' -> \"nil 20\" -> 0 -> dropped.  This
    last case is the one that a naive per-`+' org-not-nil filter would
    wrongly keep as 20; pinned to forbid that filter."
  (dolist (case '(("* H\n:PROPERTIES:\n:DORG_ID+: 424242\n:END:\n" . 424242)
                  ("* H\n:PROPERTIES:\n:DORG_ID: 111\n:DORG_ID+: 222\n:END:\n" . 111)
                  ("* H\n:PROPERTIES:\n:DORG_ID+: 10\n:DORG_ID+: 20\n:END:\n" . 10)
                  ("* H\n:PROPERTIES:\n:dorg_id+: 777\n:END:\n" . 777)
                  ("* H\n:PROPERTIES:\n:DORG_ID:\n:DORG_ID+: 222\n:END:\n" . 222)
                  ("* H\n:PROPERTIES:\n:DORG_ID: nil\n:END:\n" . nil)
                  ("* H\n:PROPERTIES:\n:DORG_ID+: nil\n:END:\n" . nil)
                  ("* H\n:PROPERTIES:\n:DORG_ID: nil\n:DORG_ID+: 222\n:END:\n" . 222)
                  ("* H\n:PROPERTIES:\n:DORG_ID: 111\n:DORG_ID+: nil\n:END:\n" . 111)
                  ("* H\n:PROPERTIES:\n:DORG_ID+: nil\n:DORG_ID+: 20\n:END:\n" . nil)))
    (with-temp-buffer
      (insert (car case))
      (org-mode)
      (let ((heading-markers (make-hash-table :test 'eql)))
        (dorg-projection-refresh-region-markers
         (point-min) (point-max) heading-markers)
        ;; Scan agrees with the org-entry-get oracle (the reference) ...
        (should (equal (dorg-test--marker-hash->alist heading-markers)
                       (dorg-test--refresh-region-markers-oracle
                        (point-min) (point-max))))
        ;; ... and yields the expected id (or drops the heading for nil).
        (if (cdr case)
            (progn
              (should (gethash (cdr case) heading-markers))
              (should (= 1 (hash-table-count heading-markers))))
          (should (= 0 (hash-table-count heading-markers))))))))

(ert-deftest dorg-test-projection-refresh-region-markers-no-org-parser-RED ()
  "RED seam (tracked-case): the scan must NOT call the org element/property
parser.  With `org-entry-get', `org--property-local-values', and
`org-element-at-point' stubbed to signal, `dorg-projection-refresh-region-markers'
must still return the expected pairs from a pure plain-text read.

RED until IMPL: the current `org-entry-get' body signals under the spy
and this test fails; the plain-text refactor makes it pass.  The
expected pairs are captured from the real oracle BEFORE the spy is
installed, so the comparison is against a genuine org read."
  (with-temp-buffer
    (insert "* One\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n"
            "** Two\n:PROPERTIES:\n:DORG_ID: 200\n:END:\n"
            "* Three\nno id\n"
            "* Four\n:PROPERTIES:\n:DORG_ID: 400\n:END:\n")
    (org-mode)
    (let ((expected (dorg-test--refresh-region-markers-oracle
                     (point-min) (point-max)))
          (heading-markers (make-hash-table :test 'eql))
          (signalled nil))
      (should (equal (mapcar #'car expected) '(100 200 400)))
      (cl-letf (((symbol-function 'org-entry-get)
                 (lambda (&rest _) (setq signalled t)
                   (error "org-entry-get called: scan is not plain-text")))
                ((symbol-function 'org--property-local-values)
                 (lambda (&rest _) (setq signalled t)
                   (error "org--property-local-values called")))
                ((symbol-function 'org-element-at-point)
                 (lambda (&rest _) (setq signalled t)
                   (error "org-element-at-point called"))))
        (dorg-projection-refresh-region-markers
         (point-min) (point-max) heading-markers))
      (should-not signalled)
      (should (equal (dorg-test--marker-hash->alist heading-markers)
                     expected)))))

(ert-deftest dorg-test-projection-refresh-region-markers-call-count-RED ()
  "RED seam (tracked-case): zero org-parser calls at scale.
Builds a synthetic buffer of many DORG_ID headings, counts calls into
`org-entry-get' / `org-element-at-point' during one
`dorg-projection-refresh-region-markers', and asserts the count is ZERO while
all pairs are still recovered.

RED until IMPL: the current `org-entry-get' body makes one call per
heading (count = N); the plain-text refactor makes zero.  This is the
bounded call-count comparison over a synthetic large buffer."
  (let ((n 500))
    (with-temp-buffer
      (dotimes (i n)
        (insert (format "* H%d\n:PROPERTIES:\n:DORG_ID: %d\n:END:\n"
                        i (1+ i))))
      (org-mode)
      (let ((heading-markers (make-hash-table :test 'eql))
            (calls 0)
            (orig-entry-get (symbol-function 'org-entry-get))
            (orig-at-point (symbol-function 'org-element-at-point)))
        (cl-letf (((symbol-function 'org-entry-get)
                   (lambda (&rest args) (cl-incf calls)
                     (apply orig-entry-get args)))
                  ((symbol-function 'org-element-at-point)
                   (lambda (&rest args) (cl-incf calls)
                     (apply orig-at-point args))))
          (dorg-projection-refresh-region-markers
           (point-min) (point-max) heading-markers))
        ;; All N ids recovered ...
        (should (= n (hash-table-count heading-markers)))
        (should (gethash 1 heading-markers))
        (should (gethash n heading-markers))
        ;; ... with zero org element/property parser calls.
        (should (= 0 calls))))))

(provide 'dorg-projection-tests)
;;; dorg-projection-tests.el ends here
