;;; dorg-install-tests.el --- Relocated `make install' behaviour -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `make install' copies the DOrg runtime (production modules, the committed
;; Clime wrapper, vendored Clime/web-server/mysql.el and their notices) into
;; an installation tree, byte-compiles it THERE, verifies the staged tree,
;; and only then publishes it by rename and points `BINDIR/dorg' at it.
;; These tests drive the real Makefile against an owned copy of the source
;; tree and an owned prefix, so nothing outside the test root is touched:
;;
;;   - a fresh install is relocated: the launcher, a clean library load and
;;     every DOrg/Clime/mysql library resolve to bytecode inside the
;;     installation, even after the source copy is poisoned and removed;
;;   - a reinstall replaces the whole tree (no stale module survives);
;;   - every failure or interruption before the link rename (the commit
;;     point) restores the prior tree and link, and an interruption at the
;;     commit point keeps the complete new install;
;;   - preflight refusals fire before any write.
;;
;; Failure injection uses a PATH `mv' fixture that wraps /bin/mv: it fails
;; or signals the recipe shell at exactly one publish rename, chosen by
;; environment variables only the fixture reads.  It writes a hit marker so
;; a test can prove the fault was actually injected.
;;
;; Gate: MEDIUM.  Each install byte-compiles the whole runtime.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'dorg-test-helpers)

(defconst dorg-test-install--source-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root whose tree each test copies.")

(defconst dorg-test-install--timeout 1200
  "Seconds one `make install' (a full runtime byte-compile) may take.")

(defconst dorg-test-install--command-timeout 300
  "Seconds one launcher or census process may take.")

(defconst dorg-test-install--marker "dorg-install-v1\n"
  "Exact bytes of the installation ownership marker.")

(defconst dorg-test-install--plain-org
  (concat "#+TITLE: Sample\n\n"
          "* First task\n:PROPERTIES:\n:DORG_ID: 100\n:END:\n\n"
          "* Second task :work:\n:PROPERTIES:\n:DORG_ID: 101\n:END:\n\n"
          "* Third task :work:\n:PROPERTIES:\n:DORG_ID: 102\n:END:\n")
  "Plain-Org fixture for the installed query.")

;;; ─── Owned test root ─────────────────────────────────────────────────

(defun dorg-test-install--emacs ()
  "Return the absolute Emacs executable running this test."
  (expand-file-name invocation-name invocation-directory))

(defun dorg-test-install--path (root &rest parts)
  "Return ROOT joined with relative PARTS."
  (let ((path root))
    (dolist (part parts path)
      (setq path (expand-file-name part path)))))

(defun dorg-test-install--write (file content &optional mode)
  "Write CONTENT to FILE literally, creating parents; chmod to MODE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert content)))
  (when mode
    (set-file-modes file mode)))

(defun dorg-test-install--read (file)
  "Return FILE's raw bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun dorg-test-install--copy-source (root)
  "Copy the installable source subset into ROOT/build; return that dir.
Root `dorg*.el' includes `dorg-app.el' and `dorg-test-job.el' so the
install's exclusions are exercised.  No `.elc' is copied."
  (let* ((src dorg-test-install--source-root)
         (build (dorg-test-install--path root "build")))
    (make-directory (dorg-test-install--path build "bin") t)
    (dolist (rel '("Makefile" "LICENSE" "bin/dorg"))
      (copy-file (expand-file-name rel src)
                 (dorg-test-install--path build rel) nil t))
    (dolist (file (directory-files src t "\\`dorg.*\\.el\\'"))
      (copy-file file (dorg-test-install--path build (file-name-nondirectory file))
                 nil t))
    (copy-directory (expand-file-name "lib" src)
                    (dorg-test-install--path build "lib") t t t)
    (dolist (elc (directory-files-recursively build "\\.elc\\'"))
      (delete-file elc))
    build))

(defun dorg-test-install--setup (root)
  "Create the fixture layout under owned ROOT; return a context plist.
The plist carries every path a test needs; nothing is global."
  (let* ((emacs (dorg-test-install--emacs))
         (emacs-bin (dorg-test-install--path root "emacs-bin"))
         (elsewhere (dorg-test-install--path root "elsewhere"))
         (prefix (dorg-test-install--path root "prefix")))
    (dolist (dir (list emacs-bin elsewhere
                       (dorg-test-install--path root "home")
                       (dorg-test-install--path root "tmp")
                       (dorg-test-install--path root "hits")))
      (make-directory dir t))
    ;; The wrapper execs `emacs' from PATH and the install compiles with
    ;; EMACS; both must be the very executable running this test.
    (make-symbolic-link emacs (dorg-test-install--path emacs-bin "emacs"))
    (dorg-test-install--write
     (dorg-test-install--path elsewhere "tasks.org") dorg-test-install--plain-org)
    (list :root root
          :emacs emacs
          :emacs-bin emacs-bin
          :build (dorg-test-install--copy-source root)
          :elsewhere elsewhere
          :prefix prefix
          :bindir (dorg-test-install--path prefix "bin")
          :dorg-home (dorg-test-install--path prefix "share/dorg"))))

(defmacro dorg-test-install--with-context (var &rest body)
  "Bind VAR to a fresh fixture context under an owned root; run BODY."
  (declare (indent 1) (debug t))
  `(let ((root (dorg-test--register-owned-root
                (make-temp-file "dorg-test-install-" t) "install")))
     (unwind-protect
         (let ((,var (dorg-test-install--setup root)))
           ,@body)
       (dorg-test--delete-owned-root root))))

;;; ─── Processes ───────────────────────────────────────────────────────

(defun dorg-test-install--env (ctx &optional path-prefix extra)
  "Return a scrubbed `process-environment' for CTX.
HOME and TMPDIR point at empty owned sentinels, `EMACSLOADPATH' and XDG
paths are removed, and PATH holds only the pinned Emacs, optional
PATH-PREFIX directory and system tools.  Make's recursion variables are
removed so a nested make does not inherit the test runner's flags.  EXTRA
is a list of additional NAME=VALUE strings."
  (let ((drop '("EMACSLOADPATH" "HOME" "TMPDIR" "PATH" "MAKEFLAGS" "MFLAGS"
                "MAKELEVEL" "MAKEOVERRIDES" "XDG_CONFIG_HOME" "XDG_CACHE_HOME"
                "XDG_DATA_HOME" "XDG_STATE_HOME" "EMACSDATA" "EMACSPATH"
                "CLIME_MAIN_APP" "CLIME_ARGV0" "PREFIX" "BINDIR"
                "DATADIR" "DORG_HOME")))
    (append
     extra
     (list (concat "HOME=" (dorg-test-install--path (plist-get ctx :root) "home"))
           (concat "TMPDIR=" (dorg-test-install--path (plist-get ctx :root) "tmp"))
           (concat "PATH="
                   (mapconcat #'identity
                              (delq nil (list path-prefix
                                              (plist-get ctx :emacs-bin)
                                              "/usr/bin" "/bin"
                                              "/usr/sbin" "/sbin"))
                              ":")))
     (cl-remove-if (lambda (entry)
                     (cl-some (lambda (name)
                                (string-prefix-p (concat name "=") entry))
                              drop))
                   process-environment))))

(defun dorg-test-install--run (program args env cwd timeout)
  "Run PROGRAM with ARGS under ENV in CWD, bounded by TIMEOUT seconds.
Return a plist (:exit CODE :output TEXT) with stdout and stderr merged.
On timeout the process receives SIGTERM, then SIGKILL, and the test fails."
  (let* ((process-environment env)
         (default-directory (file-name-as-directory cwd))
         (buffer (generate-new-buffer " *dorg-test-install*"))
         (proc (make-process :name "dorg-test-install"
                             :buffer buffer
                             :command (cons program args)
                             :connection-type 'pipe
                             :noquery t
                             :sentinel #'ignore))
         (deadline (+ (float-time) timeout)))
    (unwind-protect
        (progn
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 1))
          (when (process-live-p proc)
            (signal-process proc 'TERM)
            (let ((grace (+ (float-time) 15)))
              (while (and (process-live-p proc) (< (float-time) grace))
                (accept-process-output proc 1)))
            (when (process-live-p proc)
              (signal-process proc 'KILL))
            (ert-fail (format "timed out after %ss: %s %S\n%s" timeout program args
                              (with-current-buffer buffer (buffer-string)))))
          (while (accept-process-output proc 0.1))
          (list :exit (process-exit-status proc)
                :output (with-current-buffer buffer (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun dorg-test-install--make (ctx &rest options)
  "Run `make install' for CTX and return the result plist plus :stage.
OPTIONS: :vars (extra NAME=VALUE make arguments), :path-prefix, :env-extra."
  (let* ((make (or (executable-find "make") (error "make not found")))
         (result (dorg-test-install--run
                  make
                  (append (list "-C" (plist-get ctx :build) "install"
                                (concat "PREFIX=" (plist-get ctx :prefix))
                                (concat "EMACS=" (plist-get ctx :emacs)))
                          (plist-get options :vars))
                  (dorg-test-install--env ctx (plist-get options :path-prefix)
                                          (plist-get options :env-extra))
                  (plist-get ctx :elsewhere)
                  dorg-test-install--timeout))
         (output (plist-get result :output)))
    (append result
            (list :stage (and (string-match "^Staging in \\(.+\\)$" output)
                              (match-string 1 output))))))

(defun dorg-test-install--launch (ctx launcher &rest args)
  "Run LAUNCHER with ARGS for CTX from an unrelated cwd; return result plist."
  (dorg-test-install--run launcher args (dorg-test-install--env ctx)
                          (plist-get ctx :elsewhere)
                          dorg-test-install--command-timeout))

(defun dorg-test-install--should-exit (expected result)
  "Assert RESULT's exit is EXPECTED (0, or `nonzero'), showing its output."
  (ert-info ((plist-get result :output) :prefix "output: ")
    (if (eq expected 'nonzero)
        (should-not (eql 0 (plist-get result :exit)))
      (should (eql expected (plist-get result :exit))))))

;;; ─── Filesystem observations ─────────────────────────────────────────

(defun dorg-test-install--manifest (dir &optional exclude)
  "Return a sorted manifest of DIR: (REL KIND DETAIL) for every entry.
KIND is `link' (DETAIL = target), `dir', or `file' (DETAIL = size and
sha256).  Symlinks are never followed.  Top-level names in EXCLUDE are
skipped.  A missing DIR yields the symbol `absent'."
  (if (not (or (file-directory-p dir) (file-symlink-p dir)))
      'absent
    (let (entries)
      (dolist (path (directory-files-recursively
                     dir "" t
                     (lambda (d)
                       (not (member (car (split-string (file-relative-name d dir) "/"))
                                    exclude)))))
        (let ((rel (file-relative-name path dir)))
          (unless (member (car (split-string rel "/")) exclude)
            (push (cond ((file-symlink-p path) (list rel 'link (file-symlink-p path)))
                        ((file-directory-p path) (list rel 'dir nil))
                        (t (let ((bytes (dorg-test-install--read path)))
                             (list rel 'file (list (length bytes)
                                                   (secure-hash 'sha256 bytes))))))
                  entries))))
      (sort entries (lambda (a b) (string< (car a) (car b)))))))

(defun dorg-test-install--files-matching (dir regexp)
  "Return files under DIR whose name matches REGEXP (no symlink following)."
  (and (file-directory-p dir)
       (directory-files-recursively dir regexp)))

(defun dorg-test-install--leftovers (dir)
  "Return publication temporaries directly in DIR."
  (and (file-directory-p dir)
       (directory-files dir nil "\\`\\.dorg-install-\\(stage\\|link\\)\\.")))

(defun dorg-test-install--parent (path)
  "Return PATH's parent directory without a trailing slash."
  (directory-file-name (file-name-directory (directory-file-name path))))

(defun dorg-test-install--link-target (ctx)
  "Return the target string of CTX's BINDIR/dorg symlink, or nil."
  (file-symlink-p (dorg-test-install--path (plist-get ctx :bindir) "dorg")))

(defun dorg-test-install--inside-p (file dir)
  "Return non-nil when FILE's true name is inside DIR's true name."
  (string-prefix-p (file-name-as-directory (file-truename dir))
                   (file-truename file)))

;;; ─── Library census (same Emacs as the install) ──────────────────────

(defun dorg-test-install--census (ctx dorg-home &optional extra-dirs core-only)
  "Load DOrg from DORG-HOME the way the launcher does; return a plist.
EXTRA-DIRS are put on `load-path' FIRST (the positive control).  When
CORE-ONLY, only `(require \\='dorg)' is loaded.  The plist has :libs
\(NAME . PATH) pairs, :history (non-Emacs `load-history' files) and
:clime (whether Clime was loaded)."
  (let* ((form
          `(progn
             ,@(if core-only
                   '((require 'dorg))
                 '((require 'clime) (require 'clime-telemetry) (require 'dorg)
                   (require 'clime-serve) (clime-serve--ensure-web-server)))
             (let* ((emacs-dirs
                     (mapcar #'file-truename
                             (delq nil (append (list lisp-directory
                                                     installation-directory)
                                               (and (boundp 'native-comp-eln-load-path)
                                                    native-comp-eln-load-path)))))
                    (history
                     (cl-remove-if
                      (lambda (file)
                        (cl-some (lambda (dir)
                                   (string-prefix-p (file-name-as-directory dir)
                                                    (file-truename file)))
                                 emacs-dirs))
                      (cl-remove-if-not
                       (lambda (file) (and (stringp file) (file-name-absolute-p file)))
                       (mapcar #'car load-history)))))
               (prin1 (list :libs (mapcar (lambda (name) (cons name (locate-library name)))
                                          ',(if core-only
                                                '("dorg" "dorg-dolt" "mysql")
                                              '("dorg" "dorg-dolt" "mysql" "clime"
                                                "clime-serve" "web-server")))
                            :history history
                            :clime (featurep 'clime))))))
         (result (dorg-test-install--run
                  (plist-get ctx :emacs)
                  (append (list "--batch" "-Q")
                          (mapcan (lambda (d) (list "-L" d)) extra-dirs)
                          (list "-L" dorg-home
                                "-L" (dorg-test-install--path dorg-home "lib/clime")
                                "--eval" "(require 'cl-lib)"
                                "--eval" (prin1-to-string form)))
                  (dorg-test-install--env ctx)
                  (plist-get ctx :elsewhere)
                  dorg-test-install--command-timeout))
         (output (plist-get result :output)))
    (dorg-test-install--should-exit 0 result)
    (car (read-from-string output (string-match "(:libs " output)))))

(defun dorg-test-install--assert-census-installed (census dorg-home)
  "Assert every library and history file in CENSUS is .elc inside DORG-HOME."
  (ert-info ((format "%S" census) :prefix "census: ")
    (should (plist-get census :history))
    (dolist (lib (plist-get census :libs))
      (should (cdr lib))
      (should (string-suffix-p ".elc" (cdr lib)))
      (should (dorg-test-install--inside-p (cdr lib) dorg-home)))
    (dolist (file (plist-get census :history))
      (should (string-suffix-p ".elc" file))
      (should (dorg-test-install--inside-p file dorg-home)))))

;;; ─── PATH mv fixture ─────────────────────────────────────────────────

(defconst dorg-test-install--mv-fixture
  "#!/bin/sh
# Test fixture: /bin/mv, except at the one publish rename selected by
# DORG_TEST_MV_MODE in directory DORG_TEST_MV_PARENT.  Names are matched by
# glob on the generated .dorg-install-stage./.dorg-install-link. prefixes.
src= dst=
for a do src=$dst; dst=$a; done
action=
case \"$DORG_TEST_MV_MODE\" in
  fail-publish|term-published)
    if [ \"$(dirname \"$src\")\" = \"$DORG_TEST_MV_PARENT\" ]; then
      case \"$(basename \"$src\")\" in
        .dorg-install-stage.*.old) ;;
        .dorg-install-stage.*) action=$DORG_TEST_MV_MODE ;;
      esac
    fi ;;
  term-displaced)
    if [ \"$(dirname \"$dst\")\" = \"$DORG_TEST_MV_PARENT\" ]; then
      case \"$(basename \"$dst\")\" in
        .dorg-install-stage.*.old) action=term-displaced ;;
      esac
    fi ;;
  fail-link|term-committed)
    if [ \"$(dirname \"$src\")\" = \"$DORG_TEST_MV_PARENT\" ]; then
      case \"$(basename \"$src\")\" in
        .dorg-install-link.*) action=$DORG_TEST_MV_MODE ;;
      esac
    fi ;;
esac
if [ -n \"$action\" ]; then : > \"$DORG_TEST_MV_HITS/hit-$action\"; fi
case \"$action\" in
  fail-*) exit 1 ;;
  term-*) /bin/mv \"$@\" || exit $?; kill -TERM \"$PPID\"; exit 0 ;;
esac
exec /bin/mv \"$@\"
"
  "Shell source of the PATH `mv' failure-injection fixture.")

(defun dorg-test-install--mv-fixture-dir (ctx)
  "Install the mv fixture for CTX; return its directory."
  (let ((dir (dorg-test-install--path (plist-get ctx :root) "shim-bin")))
    (dorg-test-install--write (dorg-test-install--path dir "mv")
                              dorg-test-install--mv-fixture #o755)
    dir))

(defun dorg-test-install--make-with-fault (ctx mode &optional shell)
  "Run `make install' for CTX with the mv fixture in MODE.
SHELL, when non-nil, is the recipe shell passed to make as SHELL=."
  (let ((parent (if (member mode '("fail-link" "term-committed"))
                    (plist-get ctx :bindir)
                  (dorg-test-install--parent (plist-get ctx :dorg-home)))))
    (dorg-test-install--make
     ctx
     :vars (and shell (list (concat "SHELL=" shell)))
     :path-prefix (dorg-test-install--mv-fixture-dir ctx)
     :env-extra (list (concat "DORG_TEST_MV_MODE=" mode)
                      (concat "DORG_TEST_MV_PARENT=" parent)
                      (concat "DORG_TEST_MV_HITS="
                              (dorg-test-install--path (plist-get ctx :root) "hits"))))))

(defun dorg-test-install--hit-p (ctx mode)
  "Return non-nil when the mv fixture fired in MODE for CTX."
  (file-exists-p (dorg-test-install--path (plist-get ctx :root) "hits"
                                          (concat "hit-" mode))))

;;; ─── 1. Fresh install is relocated ───────────────────────────────────

(ert-deftest dorg-test-install-default-layout-respects-prefix-origin ()
  "Only an undefined PREFIX selects the personal runtime and launcher layout."
  (dorg-test-install--with-context ctx
    (let* ((root (plist-get ctx :root))
           (home (dorg-test-install--path root "home"))
           (probe (dorg-test-install--path root "layout.mk"))
           (build (plist-get ctx :build)))
      (dorg-test-install--write
       probe "probe-layout:\n\t@printf '%s\\n' '$(PREFIX)' '$(BINDIR)' '$(DORG_HOME)'\n")
      (dolist (case
               (list (list nil nil (list (concat home "/.local")
                                         (concat home "/bin")
                                         (concat home "/.local/share/dorg")))
                     (list (list "PREFIX=/custom") nil
                           '("/custom" "/custom/bin" "/custom/share/dorg"))
                     (list nil '("PREFIX=/environment")
                           '("/environment" "/environment/bin"
                             "/environment/share/dorg"))
                     (list (list "BINDIR=/chosen/bin" "DORG_HOME=/chosen/runtime") nil
                           (list (concat home "/.local") "/chosen/bin"
                                 "/chosen/runtime"))
                     (list (list "DATADIR=/chosen/data") nil
                           (list (concat home "/.local") (concat home "/bin")
                                 "/chosen/data/dorg"))))
        (pcase-let* ((`(,vars ,env ,expected) case)
                     (result (dorg-test-install--run
                              "make" (append (list "-s" "-C" build
                                                   "-f" "Makefile" "-f" probe
                                                   "probe-layout") vars)
                              (dorg-test-install--env ctx nil env)
                              (plist-get ctx :elsewhere) 30)))
          (dorg-test-install--should-exit 0 result)
          (should (equal expected
                         (split-string (string-trim-right
                                        (plist-get result :output)) "\n"))))))))

(ert-deftest dorg-test-install-default-relocates-without-touching-old-runtime ()
  "Bare install uses the new runtime while preserving an old ~/share/dorg."
  (dorg-test-install--with-context ctx
    (let* ((root (plist-get ctx :root))
           (home (dorg-test-install--path root "home"))
           (old (dorg-test-install--path home "share/dorg"))
           (runtime (dorg-test-install--path home ".local/share/dorg"))
           (link (dorg-test-install--path home "bin/dorg")))
      (dorg-test-install--write (dorg-test-install--path old "bin/dorg")
                                "keep old runtime\n")
      (make-directory (file-name-directory link) t)
      (make-symbolic-link (dorg-test-install--path old "bin/dorg") link)
      (let* ((old-before (dorg-test-install--manifest old))
             (result (dorg-test-install--run
                      "make" (list "-C" (plist-get ctx :build) "install"
                                   (concat "EMACS=" (plist-get ctx :emacs)))
                      (dorg-test-install--env ctx)
                      (plist-get ctx :elsewhere) dorg-test-install--timeout)))
        (dorg-test-install--should-exit 0 result)
        (should (equal old-before (dorg-test-install--manifest old)))
        (should (equal dorg-test-install--marker
                       (dorg-test-install--read
                        (dorg-test-install--path runtime ".dorg-install"))))
        (should (file-symlink-p link))
        (should (dorg-test-install--inside-p link runtime))
        (should (string-search runtime (plist-get result :output)))))))

(ert-deftest dorg-test-install-default-refuses-old-source-checkout ()
  "An old install.sh source checkout at the new runtime path is unowned."
  (dorg-test-install--with-context ctx
    (let* ((root (plist-get ctx :root))
           (home (dorg-test-install--path root "home"))
           (old (dorg-test-install--path home ".local/share/dorg"))
           (source (dorg-test-install--path home ".local/src/dorg"))
           (link (dorg-test-install--path home "bin/dorg")))
      (dorg-test-install--write (dorg-test-install--path old ".git/config")
                                "old source checkout\n")
      (make-directory (file-name-directory source) t)
      (rename-file (plist-get ctx :build) source)
      (let* ((before (dorg-test-install--manifest old))
             (source-before (dorg-test-install--manifest source))
             (result (dorg-test-install--run
                      "make" (list "-C" source "install"
                                   (concat "EMACS=" (plist-get ctx :emacs)))
                      (dorg-test-install--env ctx)
                      (plist-get ctx :elsewhere) dorg-test-install--timeout)))
        (dorg-test-install--should-exit 'nonzero result)
        (should (string-search "REFUSE reason=unowned-dorg-home"
                               (plist-get result :output)))
        (should (equal before (dorg-test-install--manifest old)))
        (should (equal source-before (dorg-test-install--manifest source)))
        (should-not (file-exists-p link))
        (should-not (dorg-test-install--leftovers
                     (dorg-test-install--parent old)))))))

(ert-deftest dorg-test-install-script-defaults-separate-source-and-runtime ()
  "install.sh clones into src and passes the personal runtime prefix."
  (let ((script (dorg-test-install--read
                 (dorg-test-install--path dorg-test-install--source-root
                                          "install.sh"))))
    (should (string-search
             "DORG_DIR=\"${DORG_DIR:-$HOME/.local/src/dorg}\"" script))
    (should (string-search "PREFIX=\"${PREFIX:-$HOME/.local}\"" script))
    (should (string-search "BINDIR=\"${BINDIR:-$HOME/bin}\"" script))))

(ert-deftest dorg-test-install-fresh-install-is-relocated ()
  "A fresh install is self-contained and independent of the source tree."
  (dorg-test-install--with-context ctx
    (let* ((root (plist-get ctx :root))
           (build (plist-get ctx :build))
           (home (plist-get ctx :dorg-home))
           (bindir (plist-get ctx :bindir))
           (link (dorg-test-install--path bindir "dorg"))
           (outside-before (dorg-test-install--manifest root '("prefix")))
           (build-before (dorg-test-install--manifest build))
           (result (dorg-test-install--make ctx)))
      (dorg-test-install--should-exit 0 result)
      (should (plist-get result :stage))
      (ert-info ("inventory")
        (let ((expected (sort (cl-set-difference
                               (directory-files build nil "\\`dorg.*\\.el\\'")
                               '("dorg-app.el" "dorg-test-job.el") :test #'equal)
                              #'string<)))
          ;; EXPECTED derives from the copied runtime-file inventory; its
          ;; length moves with shipped modules instead of a numeric pin.
          (should (equal expected (sort (directory-files home nil "\\`dorg.*\\.el\\'")
                                        #'string<))))
        (dolist (el (dorg-test-install--files-matching home "\\.el\\'"))
          (should (file-exists-p (concat el "c"))))
        (dolist (rel '("LICENSE" "bin/dorg" "lib/clime/LICENSE" "lib/clime/PROVENANCE"
                       "lib/clime/VERSION" "lib/clime/lib/web-server/COPYING"
                       "lib/clime/lib/web-server/README" "lib/mysql/LICENSE"
                       "lib/mysql/UPSTREAM.org"))
          (should (file-exists-p (dorg-test-install--path home rel))))
        (should (equal dorg-test-install--marker
                       (dorg-test-install--read
                        (dorg-test-install--path home ".dorg-install"))))
        (dolist (rel '("tests" "tools" "docs" "Makefile"))
          (should-not (file-exists-p (dorg-test-install--path home rel))))
        ;; Neither the wrapper's app source nor the test-only module, in
        ;; either form, is installed.
        (should (directory-files (plist-get ctx :build) nil
                                 "\\`dorg-\\(app\\|test-job\\)\\.el\\'"))
        (should-not (directory-files home nil
                                     "\\`dorg-\\(app\\|test-job\\)\\.elc?\\'")))
      (ert-info ("link resolves inside the install")
        (should (file-symlink-p link))
        (should (dorg-test-install--inside-p link home)))
      (ert-info ("no checkout bytecode, source untouched")
        (should-not (dorg-test-install--files-matching build "\\.elc\\'"))
        (should (equal build-before (dorg-test-install--manifest build))))
      (ert-info ("nothing written outside the prefix")
        (should-not (directory-files (dorg-test-install--path root "home") nil
                                     directory-files-no-dot-files-regexp))
        (should-not (directory-files (dorg-test-install--path root "tmp") nil
                                     directory-files-no-dot-files-regexp))
        (should (equal outside-before (dorg-test-install--manifest root '("prefix"))))
        (should-not (dorg-test-install--leftovers (dorg-test-install--parent home)))
        (should-not (dorg-test-install--leftovers bindir))
        (dolist (elc (dorg-test-install--files-matching home "\\.elc\\'"))
          (should-not (string-search (plist-get result :stage)
                                     (dorg-test-install--read elc)))))
      (ert-info ("census resolves to installed bytecode")
        (dorg-test-install--assert-census-installed
         (dorg-test-install--census ctx home) home)
        (let ((core (dorg-test-install--census ctx home nil t)))
          (dorg-test-install--assert-census-installed core home)
          (should-not (plist-get core :clime)))
        ;; Positive control: the same census sees a tree placed first.
        (let ((control (dorg-test-install--census ctx home (list build))))
          (should (dorg-test-install--inside-p
                   (cdr (assoc "dorg" (plist-get control :libs))) build))))
      (ert-info ("source independence")
        (let ((poisoned (dorg-test-install--path build "dorg-timestamp.el")))
          (dorg-test-install--write
           poisoned (concat "(error \"dorg-install-poisoned-source\")\n"
                            (dorg-test-install--read poisoned))))
        (let ((control (dorg-test-install--launch
                        ctx (dorg-test-install--path build "bin/dorg") "--help")))
          (dorg-test-install--should-exit 'nonzero control)
          (should (string-search "dorg-install-poisoned-source"
                                 (plist-get control :output))))
        (dorg-test-install--should-exit 0 (dorg-test-install--launch ctx link "--help"))
        (rename-file build (concat build ".gone"))
        (dorg-test-install--should-exit 0 (dorg-test-install--launch ctx link "--help"))
        (dorg-test-install--should-exit
         0 (dorg-test-install--launch ctx link "doctor" "--install"))
        (let ((query (dorg-test-install--launch
                      ctx link "query" "(tag \"work\")" "-f"
                      (dorg-test-install--path (plist-get ctx :elsewhere) "tasks.org"))))
          (dorg-test-install--should-exit 0 query)
          (should (string-search "101\tSecond task\n102\tThird task\n"
                                 (plist-get query :output))))))))

;;; ─── 2. Reinstall replaces the whole tree ────────────────────────────

(ert-deftest dorg-test-install-reinstall-replaces-whole-tree ()
  "A reinstall under a custom DORG_HOME replaces the tree, not overlays it."
  (dorg-test-install--with-context ctx
    (let* ((root (plist-get ctx :root))
           (home (dorg-test-install--path root "alt/rt/dorg"))
           (parent (dorg-test-install--parent home))
           (ctx (plist-put (copy-sequence ctx) :dorg-home home))
           (vars (list (concat "DORG_HOME=" home)))
           (link (dorg-test-install--path (plist-get ctx :bindir) "dorg")))
      ;; A broken symlink is a replaceable link entry.
      (make-directory (plist-get ctx :bindir) t)
      (make-symbolic-link (dorg-test-install--path root "nowhere") link)
      (let ((first (dorg-test-install--make ctx :vars vars)))
        (dorg-test-install--should-exit 0 first)
        (should (dorg-test-install--inside-p link home))
        (should (equal parent (dorg-test-install--parent (plist-get first :stage))))
        (should (equal (file-attribute-device-number (file-attributes parent))
                       (file-attribute-device-number (file-attributes home))))
        (should-not (file-exists-p (dorg-test-install--path root "prefix/share"))))
      (let* ((installed (dorg-test-install--manifest home))
             (stale (dorg-test-install--path home "dorg-timestamp.elc"))
             (sentinel "stale bytecode sentinel\n"))
        (dorg-test-install--write (dorg-test-install--path home "dorg-zz-removed.el")
                                  ";; removed module\n")
        (dorg-test-install--write (dorg-test-install--path home "dorg-zz-removed.elc")
                                  ";; removed bytecode\n")
        (dorg-test-install--write stale sentinel)
        (should (file-exists-p (dorg-test-install--path home "dorg-zz-removed.el")))
        (should-not (equal installed (dorg-test-install--manifest home)))
        (let ((second (dorg-test-install--make ctx :vars vars)))
          (dorg-test-install--should-exit 0 second)
          (should (equal parent (dorg-test-install--parent (plist-get second :stage)))))
        (should-not (file-exists-p (dorg-test-install--path home "dorg-zz-removed.el")))
        (should-not (file-exists-p (dorg-test-install--path home "dorg-zz-removed.elc")))
        (should-not (equal sentinel (dorg-test-install--read stale)))
        (should (dorg-test-install--inside-p link home))
        (should-not (dorg-test-install--leftovers parent))))))

;;; ─── 3. Failures and interruptions ───────────────────────────────────

(defun dorg-test-install--assert-preserved (ctx result before-manifest before-link)
  "Assert RESULT failed and CTX's install equals BEFORE-MANIFEST/BEFORE-LINK."
  (let ((home (plist-get ctx :dorg-home)))
    (dorg-test-install--should-exit 'nonzero result)
    (should (equal before-manifest (dorg-test-install--manifest home)))
    (should (equal before-link (dorg-test-install--link-target ctx)))
    (should (file-exists-p (dorg-test-install--path home ".dorg-install")))
    (should-not (dorg-test-install--leftovers (dorg-test-install--parent home)))
    (should-not (dorg-test-install--leftovers (plist-get ctx :bindir)))
    (should-not (dorg-test-install--files-matching (plist-get ctx :build) "\\.elc\\'"))
    (should-not (string-search "ROLLBACK-FAILED" (plist-get result :output)))))

(ert-deftest dorg-test-install-failures-preserve-prior ()
  "Before the commit point every failure restores the prior install; an
interruption at the commit point keeps the complete new install."
  (dorg-test-install--with-context ctx
    (let* ((home (plist-get ctx :dorg-home))
           (module (dorg-test-install--path (plist-get ctx :build) "dorg-timestamp.el"))
           (original (dorg-test-install--read module)))
      (dorg-test-install--should-exit 0 (dorg-test-install--make ctx))
      (let ((manifest (dorg-test-install--manifest home))
            (link (dorg-test-install--link-target ctx)))
        (should (consp manifest))
        (should link)
        (ert-info ("compile failure")
          (dorg-test-install--write module (concat original "\n("))
          (dorg-test-install--assert-preserved
           ctx (dorg-test-install--make ctx) manifest link)
          (dorg-test-install--write module original))
        ;; Signal cases also run under dash when present: bash runs the exit
        ;; trap on a fatal signal by itself, dash needs the explicit traps.
        (dolist (fault (append '(("fail-publish") ("fail-link")
                                 ("term-displaced") ("term-published"))
                               (and (file-executable-p "/bin/dash")
                                    '(("term-displaced" "/bin/dash")
                                      ("term-published" "/bin/dash")))))
          (ert-info ((format "%S" fault) :prefix "fault: ")
            (delete-directory (dorg-test-install--path (plist-get ctx :root) "hits") t)
            (make-directory (dorg-test-install--path (plist-get ctx :root) "hits"))
            (dorg-test-install--assert-preserved
             ctx (apply #'dorg-test-install--make-with-fault ctx fault) manifest link)
            (should (dorg-test-install--hit-p ctx (car fault)))
            (should-not (directory-files home nil "\\`\\.dorg-install-stage\\."))))
        (ert-info ("signal at the commit point keeps the new install")
          (dorg-test-install--write module (concat original "\n;; new install\n"))
          (let ((result (dorg-test-install--make-with-fault ctx "term-committed")))
            (dorg-test-install--should-exit 'nonzero result)
            (should (dorg-test-install--hit-p ctx "term-committed"))
            (should-not (equal manifest (dorg-test-install--manifest home)))
            (should (string-search ";; new install"
                                   (dorg-test-install--read
                                    (dorg-test-install--path home "dorg-timestamp.el"))))
            (should (dorg-test-install--inside-p
                     (dorg-test-install--path (plist-get ctx :bindir) "dorg") home))
            (should-not (dorg-test-install--leftovers (dorg-test-install--parent home)))
            (should-not (dorg-test-install--leftovers (plist-get ctx :bindir)))
            (should (= 1 (cl-count-if
                          (lambda (line) (string-prefix-p "Installed: " line))
                          (split-string (plist-get result :output) "\n"))))))))))

;;; ─── 4. Preflight refusals ───────────────────────────────────────────

(defun dorg-test-install--assert-refused (ctx reason result watched-before watched)
  "Assert RESULT refused with REASON before writing anything.
WATCHED is a path whose manifest must equal WATCHED-BEFORE."
  (dorg-test-install--should-exit 'nonzero result)
  (should (string-search (concat "REFUSE reason=" reason) (plist-get result :output)))
  (should (equal watched-before (dorg-test-install--manifest watched)))
  (should-not (plist-get result :stage))
  (should-not (dorg-test-install--leftovers
               (dorg-test-install--parent (plist-get ctx :dorg-home)))))

(defmacro dorg-test-install--refusal-case (reason watched setup &rest make-options)
  "Run one refusal case: fresh context, SETUP, then expect REASON.
WATCHED is an expression over `ctx' naming the path that must not change."
  (declare (indent 2))
  `(ert-info (,reason :prefix "refusal: ")
     (dorg-test-install--with-context ctx
       ,setup
       (let* ((watched ,watched)
              (before (dorg-test-install--manifest watched)))
         (dorg-test-install--assert-refused
          ctx ,reason (dorg-test-install--make ctx ,@make-options) before watched)))))

(ert-deftest dorg-test-install-preflight-refusals ()
  "Preflight refuses unsafe states before creating a stage."
  (let ((marker-case
         (lambda (bytes)
           (dorg-test-install--refusal-case "unowned-dorg-home"
               (plist-get ctx :prefix)
             (dorg-test-install--write
              (dorg-test-install--path (plist-get ctx :dorg-home) ".dorg-install")
              bytes)))))
    (dorg-test-install--refusal-case "bindir-entry-not-symlink" (plist-get ctx :prefix)
      (dorg-test-install--write (dorg-test-install--path (plist-get ctx :bindir) "dorg")
                                "#!/bin/sh\nexec true\n" #o755))
    (dorg-test-install--refusal-case "bindir-entry-not-symlink" (plist-get ctx :prefix)
      (make-directory (dorg-test-install--path (plist-get ctx :bindir) "dorg") t))
    ;; `mv -f' onto a symlink to a directory would move the new link INTO the
    ;; directory, so a directory-target link is refused; the watched prefix
    ;; holds the target, proving it gains no entry.
    (dorg-test-install--refusal-case "bindir-entry-not-symlink" (plist-get ctx :prefix)
      (let ((target (dorg-test-install--path (plist-get ctx :prefix) "a-directory")))
        (make-directory target t)
        (make-directory (plist-get ctx :bindir) t)
        (make-symbolic-link target (dorg-test-install--path (plist-get ctx :bindir) "dorg"))))
    (dorg-test-install--refusal-case "unowned-dorg-home" (plist-get ctx :prefix)
      (dorg-test-install--write
       (dorg-test-install--path (plist-get ctx :dorg-home) "precious.txt") "keep\n"))
    (funcall marker-case "dorg-install-v0\n")
    (funcall marker-case "dorg-install-v1\n\n")
    (dorg-test-install--refusal-case "unowned-dorg-home" (plist-get ctx :prefix)
      (let ((real (dorg-test-install--path (plist-get ctx :root) "real-home")))
        (dorg-test-install--write (dorg-test-install--path real ".dorg-install")
                                  dorg-test-install--marker)
        (make-directory (dorg-test-install--parent (plist-get ctx :dorg-home)) t)
        (make-symbolic-link real (plist-get ctx :dorg-home))))
    (dorg-test-install--refusal-case "unowned-dorg-home" (plist-get ctx :prefix)
      (progn
        (make-directory (dorg-test-install--parent (plist-get ctx :dorg-home)) t)
        (make-symbolic-link (dorg-test-install--path (plist-get ctx :root) "nowhere")
                            (plist-get ctx :dorg-home))))
    (dorg-test-install--refusal-case "stale-wrapper" (plist-get ctx :prefix)
      (let ((wrapper (dorg-test-install--path (plist-get ctx :build) "bin/dorg")))
        (dorg-test-install--write
         wrapper (concat (dorg-test-install--read wrapper) ";; drift\n") #o755)))
    (dorg-test-install--refusal-case "emacs-mismatch" (plist-get ctx :prefix)
      (let ((shim (dorg-test-install--path (plist-get ctx :root) "other-emacs")))
        (dorg-test-install--write
         (dorg-test-install--path shim "emacs")
         (format "#!/bin/sh\nexec '%s' \"$@\"\n" (plist-get ctx :emacs)) #o755))
      :path-prefix (dorg-test-install--path (plist-get ctx :root) "other-emacs"))))

(provide 'dorg-install-tests)
;;; dorg-install-tests.el ends here
