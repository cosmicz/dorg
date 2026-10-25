;;; dorg-test-job-tests.el --- Disposable suite job contract tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Focused, Docker-free contract tests for the disposable DOrg suite job.
;; These tests pin the launcher/entrypoint contract before the image and
;; workflow implementation lands: full-depth git bundle input, non-vacuous
;; commit comparison, history-connectivity rejection, runtime Dolt identity,
;; mount allowlist, host census residue detection, and the migrated CI shape.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dorg-test-job)

(defconst dorg-test-job-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name)))))

(defconst dorg-test-job-test--dockerfile
  (expand-file-name ".github/dorg-suite-job/Dockerfile"
                    dorg-test-job-test--root))

(defconst dorg-test-job-test--lock
  (expand-file-name ".github/dorg-suite-job/image-lock.org"
                    dorg-test-job-test--root))

(defconst dorg-test-job-test--entrypoint
  (expand-file-name "tools/dorg-suite-job-entrypoint"
                    dorg-test-job-test--root))

(defconst dorg-test-job-test--launcher
  (expand-file-name "tools/dorg-suite-job-local"
                    dorg-test-job-test--root))

(defconst dorg-test-job-test--private-venue
  (expand-file-name "tools/dorg-private-venue"
                    dorg-test-job-test--root))

(defun dorg-test-job-test--file-string (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun dorg-test-job-test--write-executable (path contents)
  "Write executable file PATH with CONTENTS."
  (write-region contents nil path nil 'silent)
  (set-file-modes path #o755))

(defconst dorg-test-job-test--dolt-config-list-fixture
  "user.email = dorg-ci@example.invalid
user.name = dorg-ci
"
  "Verbatim Dolt config output fixture for systems without Dolt on PATH.")

(defconst dorg-test-job-test--image-metadata-fixture
  "{
  \"buildx.build.ref\": \"builder/default/abc123\",
  \"containerimage.digest\": \"sha256:222digest\",
  \"buildx.build.provenance\": {
    \"materials\": [
      {
        \"uri\": \"pkg:docker/debian@bookworm-slim?digest=sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171\"
      }
    ]
  }
}"
  "Representative multi-line Buildx metadata output.")

(defconst dorg-test-job-test--image-config-fixture
  "DORG_SUITE_JOB_TARGETARCH=amd64
DORG_SUITE_JOB_DOLT_ARCHIVE=dolt-linux-amd64.tar.gz
DORG_SUITE_JOB_DOLT_SHA256=88bdca99086bbac7f2414af2756c962d20a8d9b6463861dc2cf35111f13b6dea
DORG_SUITE_JOB_EMACS_VERSION=30.1
DORG_SUITE_JOB_EMACS_SHA256=6ccac1ae76e6af93c6de1df175e8eb406767c23da3dd2a16aa67e3124a6f138f
"
  "Image-owned runner metadata used by Docker-free entrypoint controls.")

(defconst dorg-test-job-test--network-receipt-fixture
  (concat "network_mode=none\n"
          "network_interfaces=lo\n"
          "network_ipv4_routes=loopback-only\n"
          "network_ipv6_routes=loopback-only\n"
          "network_ipv4_connect=ENETUNREACH\n"
          "network_ipv6_connect=ENETUNREACH\n")
  "Observed isolated namespace for Docker-free entrypoint controls.")

(defun dorg-test-job-test--gnu-timeout ()
  "Return a GNU timeout executable, or nil when none is available."
  (or (executable-find "gtimeout")
      (let ((candidate (executable-find "timeout")))
        (when candidate
          (with-temp-buffer
            (when (zerop (call-process candidate nil t nil "--version"))
              (goto-char (point-min))
              (when (search-forward "GNU" nil t)
                candidate)))))))

(ert-deftest dorg-test-job-gnu-timeout-falls-back-to-linux-command-name ()
  "The deadline control accepts GNU timeout named `timeout' on Linux CI."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (and (equal name "timeout") "/usr/bin/timeout")))
            ((symbol-function 'call-process)
             (lambda (&rest _args)
               (insert "timeout (GNU coreutils) 9.4\n")
               0)))
    (should (equal "/usr/bin/timeout" (dorg-test-job-test--gnu-timeout)))))

(ert-deftest dorg-test-job-step-order-keeps-test-status-key-for-full-target ()
  "The receipt schema retains `test' while its unselected command is full.
The artifact key is consumed by job status shaping; it must not be renamed just
because the command moved from `make test' to `make test-full'."
  (should (equal dorg-test-job-step-order '(compile lint test)))
  (should (string-match-p "make test-full"
                          (documentation-property
                           'dorg-test-job-step-order
                           'variable-documentation))))

(defun dorg-test-job-test--real-dolt-config-list ()
  "Return real Dolt config output from a temp HOME, or the fixture."
  (if (not (executable-find "dolt"))
      dorg-test-job-test--dolt-config-list-fixture
    (let ((home (make-temp-file "dorg-test-job-dolt-home." t)))
      (let ((process-environment
             (append (list (concat "HOME=" home))
                     process-environment))
            (default-directory dorg-test-job-test--root))
        (unwind-protect
            (with-temp-buffer
              (dolist (args '(("config" "--global" "--add" "user.name" "dorg-ci")
                              ("config" "--global" "--add" "user.email"
                               "dorg-ci@example.invalid")
                              ("config" "--list")))
                (when (equal args '("config" "--list"))
                  (erase-buffer))
                (let ((status (apply #'call-process "dolt" nil t nil args)))
                  (unless (zerop status)
                    (ert-fail
                     (format "dolt %s failed with %s:\n%s"
                             (mapconcat #'identity args " ")
                             status
                             (buffer-string))))))
              (buffer-string))
          (delete-directory home t))))))

(defun dorg-test-job-test--fake-bin (dir &optional all-make-steps-pass)
  "Create fake job command binaries in DIR.
When ALL-MAKE-STEPS-PASS is non-nil, compile/lint/test all pass.  Otherwise
lint exits 7 and test would pass if the entrypoint wrongly reaches it."
  (let ((sha (make-string 40 ?1))
        (dolt-config-list (dorg-test-job-test--real-dolt-config-list)))
    (make-directory dir t)
    (dorg-test-job-test--write-executable
     (expand-file-name "git" dir)
     (format "#!/usr/bin/env bash
set -euo pipefail
if [[ \"${1:-}\" == \"--version\" ]]; then
  echo \"git version ${DORG_TEST_JOB_FAKE_GIT_VERSION:-2.49.0}\"
  exit 0
fi
if [[ \"${1:-}\" == \"clone\" ]]; then
  mkdir -p \"$3/.git\"
  exit 0
fi
if [[ \"${1:-}\" == \"-C\" ]]; then
  shift 2
  case \"${1:-}\" in
    rev-parse) echo %s ;;
    rev-list) exit 0 ;;
    fsck) exit 0 ;;
    *) exit 0 ;;
  esac
  exit 0
fi
if [[ \"${1:-}\" == \"rev-parse\" ]]; then
  echo %s
  exit 0
fi
if [[ \"${1:-}\" == \"bundle\" && \"${2:-}\" == \"create\" ]]; then
  if [[ -n \"${DORG_TEST_JOB_FAKE_GIT_BUNDLE_EXIT:-}\" ]]; then
    echo partial-bundle
    exit \"$DORG_TEST_JOB_FAKE_GIT_BUNDLE_EXIT\"
  fi
  echo fake-bundle
  exit 0
fi
exit 0
" sha sha))
    (dorg-test-job-test--write-executable
     (expand-file-name "emacs" dir)
     "#!/usr/bin/env bash
set -euo pipefail
if [[ \"${1:-}\" == \"--version\" ]]; then
  echo \"GNU Emacs 30.1\"
  exit 0
fi
case \"$*\" in
  *native-comp-available-p*) printf 'native-comp=nil' ;;
  *) printf '%s' \"${DORG_TEST_JOB_FAKE_GNUTLS:-gnutls=t}\" ;;
esac
")
    (dorg-test-job-test--write-executable
     (expand-file-name "dolt" dir)
     (format "#!/usr/bin/env bash
set -euo pipefail
if [[ -n \"${DORG_TEST_JOB_FAKE_DOLT_ARGV_LOG:-}\" ]]; then
  printf '%%s\\n' \"$*\" >>\"$DORG_TEST_JOB_FAKE_DOLT_ARGV_LOG\"
fi
if [[ \"${1:-}\" == \"version\" ]]; then
  printf 'dolt version 1.86.4\\n'
  case \"${DORG_TEST_JOB_FAKE_DOLT_WARNING:-}\" in
    unterminated) printf 'Warning: unable to query latest released Dolt version' ;;
    terminated) printf 'Warning: unable to query latest released Dolt version\\n' ;;
    extra) printf 'unexpected dolt output\\n' ;;
  esac
  exit 0
fi
if [[ \"${1:-}\" == \"config\" && \"${2:-}\" == \"--global\" && \"${3:-}\" == \"--add\" ]]; then
  if [[ \"${DORG_TEST_JOB_FAKE_DOLT_CONFIG_FAIL:-}\" == \"${4:-}\" ]]; then
    exit 9
  fi
  case \"${4:-}\" in
    metrics.disabled|versioncheck.disabled) printf '%%s' \"${5:-}\" >\"$HOME/.dorg-test-${4}\" ;;
  esac
  exit 0
fi
if [[ \"${1:-}\" == \"config\" && \"${2:-}\" == \"--global\" && \"${3:-}\" == \"--get\" ]]; then
  cat \"$HOME/.dorg-test-${4:-}\"
  exit 0
fi
if [[ \"${1:-}\" == \"config\" && \"${2:-}\" == \"--get\" && \"${3:-}\" == \"user.name\" ]]; then
  echo \"dorg-ci\"
  exit 0
fi
if [[ \"${1:-}\" == \"config\" && \"${2:-}\" == \"--get\" && \"${3:-}\" == \"user.email\" ]]; then
  echo \"dorg-ci@example.invalid\"
  exit 0
fi
if [[ \"${1:-}\" == \"config\" && \"${2:-}\" == \"--list\" ]]; then
  printf '%%s' %s
fi
"
             (shell-quote-argument dolt-config-list)))
    ;; Image capability probes must be hermetic on BSD and Linux hosts.
    (dolist (stub '(("tmux" "DORG_TEST_JOB_FAKE_TMUX" "tmux 3.3a")
                    ("ps" "DORG_TEST_JOB_FAKE_PS" "ps from procps-ng 4.0.2")
                    ("pgrep" "DORG_TEST_JOB_FAKE_PGREP" "pgrep from procps-ng 4.0.2")))
      (dorg-test-job-test--write-executable
       (expand-file-name (nth 0 stub) dir)
       (format "#!/usr/bin/env bash\nif [[ \"${%s:-}\" == absent ]]; then exit 127; fi\nprintf '%%s\\n' \"${%s:-%s}\"\n"
               (nth 1 stub) (nth 1 stub) (nth 2 stub))))
    (dorg-test-job-test--write-executable
     (expand-file-name "make" dir)
     (if all-make-steps-pass
         "#!/usr/bin/env bash
set -euo pipefail
case \"${1:-}\" in
  compile)
    if [[ \"${DORG_TEST_JOB_FAKE_MAKE_SLEEP_STEP:-}\" == compile ]]; then
      sleep \"${DORG_TEST_JOB_FAKE_MAKE_SLEEP:?}\"
    fi
    echo compile-ok
    ;;
  lint)
    if [[ \"${DORG_TEST_JOB_FAKE_MAKE_SLEEP_STEP:-}\" == lint ]]; then
      sleep \"${DORG_TEST_JOB_FAKE_MAKE_SLEEP:?}\"
    fi
    echo lint-ok
    ;;
  test-full)
    if [[ -n \"${DORG_TEST_JOB_FAKE_MAKE_SIGNAL:-}\" ]]; then
      # GNU timeout runs the gate in its own process group.  Kill that group
      # so this control reaches the real full-gate functional-failure path.
      kill \"-${DORG_TEST_JOB_FAKE_MAKE_SIGNAL}\" 0
    fi
    if [[ -n \"${DORG_TEST_JOB_FAKE_MAKE_SLEEP:-}\" ]]; then
      sleep \"$DORG_TEST_JOB_FAKE_MAKE_SLEEP\" &
      child=$!
      if [[ -n \"${DORG_TEST_JOB_FAKE_MAKE_CHILD_PID_FILE:-}\" ]]; then
        printf '%s\\n' \"$child\" >\"$DORG_TEST_JOB_FAKE_MAKE_CHILD_PID_FILE\"
      fi
      wait \"$child\"
    fi
    echo \"Ran 1 tests, 1 results as expected, 0 unexpected\"
    ;;
esac
exit 0
"
       "#!/usr/bin/env bash
set -euo pipefail
case \"${1:-}\" in
  compile) echo compile-ok; exit 0 ;;
  lint) echo lint-failed; exit 7 ;;
  test-full) echo \"Ran 1 tests, 1 results as expected, 0 unexpected\"; exit 0 ;;
esac
exit 0
"))
    ;; The entrypoint always invokes the production command name `timeout'.
    ;; This test-only executable forwards that name to GNU timeout on macOS.
    (dorg-test-job-test--write-executable
     (expand-file-name "timeout" dir)
     "#!/usr/bin/env bash
set -euo pipefail
timeout_bin=\"${DORG_TEST_JOB_TIMEOUT_BIN:-}\"
if [[ -z \"$timeout_bin\" ]]; then
  for candidate in /usr/bin/timeout /opt/homebrew/bin/gtimeout; do
    if [[ -x \"$candidate\" ]]; then
      timeout_bin=\"$candidate\"
      break
    fi
  done
fi
exec \"${timeout_bin:?GNU timeout is unavailable}\" \"$@\"
")
    (dorg-test-job-test--write-executable
     (expand-file-name "docker" dir)
     "#!/usr/bin/env bash
set -euo pipefail
case \"${1:-}\" in
  ps)
    if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_PS_FAIL:-}\" ]]; then
      exit \"$DORG_TEST_JOB_FAKE_DOCKER_PS_FAIL\"
    fi
    if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_CONTAINER_AFTER:-}\" ]]; then
      mkdir -p \"${DORG_TEST_JOB_FAKE_DOCKER_STATE:?}\"
      count_file=\"$DORG_TEST_JOB_FAKE_DOCKER_STATE/ps-count\"
      count=0
      [[ -f \"$count_file\" ]] && count=\"$(<\"$count_file\")\"
      count=$((count + 1))
      echo \"$count\" >\"$count_file\"
      if (( count > 1 )); then
        echo \"$DORG_TEST_JOB_FAKE_DOCKER_CONTAINER_AFTER\"
      fi
    fi
    exit 0
    ;;
  volume)
    if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_VOLUME_FAIL:-}\" ]]; then
      exit \"$DORG_TEST_JOB_FAKE_DOCKER_VOLUME_FAIL\"
    fi
    if [[ \"${2:-}\" == \"ls\" && \"${3:-}\" == \"-q\" && -n \"${DORG_TEST_JOB_FAKE_DOCKER_VOLUME_AFTER:-}\" ]]; then
      mkdir -p \"${DORG_TEST_JOB_FAKE_DOCKER_STATE:?}\"
      count_file=\"$DORG_TEST_JOB_FAKE_DOCKER_STATE/volume-count\"
      count=0
      [[ -f \"$count_file\" ]] && count=\"$(<\"$count_file\")\"
      count=$((count + 1))
      echo \"$count\" >\"$count_file\"
      if (( count > 1 )); then
        echo \"$DORG_TEST_JOB_FAKE_DOCKER_VOLUME_AFTER\"
      fi
    fi
    exit 0
    ;;
  run)
    host_artifacts=
    prev=
    for arg in \"$@\"; do
      if [[ \"$prev\" == \"-v\" && \"$arg\" == *\":/artifacts\" ]]; then
        host_artifacts=\"${arg%%:/artifacts}\"
      fi
      prev=\"$arg\"
    done
    if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS:-}\" && -n \"$host_artifacts\" ]]; then
      mkdir -p \"$host_artifacts\"
      if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_ARTIFACT_SOURCE:-}\" ]]; then
        cp -R \"$DORG_TEST_JOB_FAKE_DOCKER_ARTIFACT_SOURCE\"/. \"$host_artifacts\"/
      else
        fake_overall=\"${DORG_TEST_JOB_FAKE_CONTAINER_OVERALL:-0}\"
        fake_classification=\"${DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION:-pass}\"
        fake_test_status=\"${DORG_TEST_JOB_FAKE_TEST_STATUS:-0}\"
        {
        echo compile=0
        echo lint=0
        echo \"test=$fake_test_status\"
        echo \"overall=$fake_overall\"
        if [[ -n \"${DORG_TEST_JOB_FAKE_OMIT_CLASSIFICATION:-}\" ]]; then
          :
        elif [[ -n \"${DORG_TEST_JOB_FAKE_EMPTY_CLASSIFICATION:-}\" ]]; then
          echo classification=
        else
          echo \"classification=$fake_classification\"
        fi
        } >\"$host_artifacts/exit.status\"
        {
        echo schema=1
        echo artifact_completeness=complete
        } >\"$host_artifacts/manifest.txt\"
        {
        echo '== compile =='
        echo '== lint =='
        echo '== test =='
        echo 'Ran 1 tests, 1 results as expected, 0 unexpected'
        } >\"$host_artifacts/raw.log\"
        if [[ -z \"${DORG_TEST_JOB_FAKE_PARTIAL_ARTIFACTS:-}\" ]]; then
          echo \"${DORG_TEST_JOB_FAKE_ERT_SUMMARY:-Ran 1 tests, 1 results as expected, 0 unexpected}\" >\"$host_artifacts/ert-summary.txt\"
        fi
        if [[ -n \"${DORG_TEST_JOB_FAKE_READONLY_ARTIFACTS:-}\" ]]; then
          chmod a-w \"$host_artifacts/ert-summary.txt\"
        fi
        if [[ -n \"${DORG_TEST_JOB_FAKE_READONLY_ARTIFACT_DIR:-}\" ]]; then
          mkdir -p \"$host_artifacts/fixture-state\"
          echo preserved >\"$host_artifacts/fixture-state/data.txt\"
          chmod a-w \"$host_artifacts/fixture-state\"
        fi
      fi
    fi
    if [[ -n \"${DORG_TEST_JOB_FAKE_JOB_TEMP_AFTER:-}\" ]]; then
      mkdir -p \"${TMPDIR:-/tmp}/dorg-suite-job.fake-residue\"
    fi
    if [[ -n \"${DORG_TEST_JOB_FAKE_DOCKER_RECORD:-}\" ]]; then
      {
        echo \"launcher=${DORG_SUITE_JOB_LAUNCHER:-}\"
        echo \"event=${DORG_SUITE_JOB_EVENT_NAME:-}\"
        echo \"ref=${DORG_SUITE_JOB_REF:-}\"
        echo \"head_ref=${DORG_SUITE_JOB_HEAD_REF:-}\"
        echo \"github_repository=${DORG_SUITE_JOB_GITHUB_REPOSITORY:-}\"
        echo \"github_server_url=${DORG_SUITE_JOB_GITHUB_SERVER_URL:-}\"
        echo \"workspace_sha=${DORG_SUITE_JOB_WORKSPACE_SHA:-}\"
        echo \"image_id=${DORG_SUITE_JOB_IMAGE_ID:-}\"
        echo \"image_digest=${DORG_SUITE_JOB_IMAGE_DIGEST:-}\"
        echo \"image_metadata=${DORG_SUITE_JOB_IMAGE_METADATA:-}\"
        echo \"image_cache_inferred=${DORG_SUITE_JOB_IMAGE_CACHE_INFERRED:-}\"
        echo \"image_cache_threshold=${DORG_SUITE_JOB_IMAGE_CACHE_INFERENCE_THRESHOLD_SECONDS:-}\"
        echo \"image_build_seconds=${DORG_SUITE_JOB_IMAGE_BUILD_SECONDS:-}\"
        printf 'args=%s\n' \"$*\"
      } >\"$DORG_TEST_JOB_FAKE_DOCKER_RECORD\"
    fi
    cat >/dev/null
    exit \"${DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT:-0}\"
    ;;
  *) exit 0 ;;
esac
")
    (dorg-test-job-test--write-executable
     (expand-file-name "stat" dir)
     "#!/usr/bin/env bash
set -euo pipefail
if [[ \"${DORG_TEST_JOB_FAKE_ROOT_OWNED_ARTIFACT:-}\" == 1 && \"${1:-}\" == \"-c\" ]]; then
  echo \"0:0 ${3:-}\"
  exit 0
fi
if [[ -n \"${DORG_TEST_JOB_FAKE_BAD_ARTIFACT_BASENAME:-}\" && \"${1:-}\" == \"-c\" ]]; then
  case \"${3:-}\" in
    */\"$DORG_TEST_JOB_FAKE_BAD_ARTIFACT_BASENAME\")
      echo \"0:0 ${3:-}\"
      exit 0
      ;;
  esac
fi
if [[ -n \"${DORG_TEST_JOB_FAKE_ARTIFACT_OWNER:-}\" && \"${1:-}\" == \"-c\" ]]; then
  echo \"$DORG_TEST_JOB_FAKE_ARTIFACT_OWNER ${3:-}\"
  exit 0
fi
exec /usr/bin/stat \"$@\"
")
    sha))

(defun dorg-test-job-test--write-mountinfo (path &optional extra-target)
  "Write a representative mountinfo fixture to PATH.
When EXTRA-TARGET is non-nil, include it as an unexpected caller mount."
  (with-temp-file path
    (insert "1 0 0:1 / / rw - overlay overlay rw\n")
    (insert "2 1 0:2 / /proc rw shared:12 - proc proc rw\n")
    (insert "3 1 0:3 / /artifacts rw - virtiofs /host/artifacts rw\n")
    (insert "4 1 0:4 / /etc/hosts rw - virtiofs /etc/hosts rw\n")
    (when extra-target
      (insert (format "5 1 0:5 / %s rw - virtiofs /host/extra rw\n"
                      extra-target)))))

(defun dorg-test-job-test--run-entrypoint
    (temp sha &optional all-make-steps-pass extra-mount launcher image-digest
          image-config-contents event ref)
  "Run the real entrypoint under TEMP with fake commands.
Return (STATUS . OUTPUT).  SHA is the requested commit.  ALL-MAKE-STEPS-PASS
and EXTRA-MOUNT are passed through to the fake command/mount fixtures.
LAUNCHER, IMAGE-DIGEST, EVENT and REF override the default provenance env."
  (let* ((bin (expand-file-name "bin" temp))
         (artifacts (expand-file-name "artifacts" temp))
         (mountinfo (expand-file-name "mountinfo" temp))
         (image-config (expand-file-name "image-config" temp))
         (network-receipt (expand-file-name "network-receipt" temp))
         (job-root (expand-file-name "job-root" temp))
         (process-environment
          (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                        (concat "JOB_ROOT=" job-root)
                        (concat "DORG_SUITE_JOB_LAUNCHER="
                                (or launcher "unit-test"))
                        (concat "DORG_SUITE_JOB_EVENT_NAME="
                                (or event "pull_request"))
                        (concat "DORG_SUITE_JOB_REF="
                                (or ref "refs/pull/1/merge"))
                        "DORG_SUITE_JOB_HEAD_REF=feature/testjob"
                        (concat "DORG_SUITE_JOB_WORKSPACE_SHA=" sha)
                        "DORG_SUITE_JOB_IMAGE_ID=sha256:111image"
                        (concat "DORG_SUITE_JOB_IMAGE_DIGEST="
                                (or image-digest "sha256:222digest"))
                        (concat "DORG_SUITE_JOB_IMAGE_METADATA="
                                dorg-test-job-test--image-metadata-fixture)
                        "DORG_SUITE_JOB_IMAGE_CACHE_INFERRED=hit"
                        "DORG_SUITE_JOB_IMAGE_CACHE_INFERENCE_THRESHOLD_SECONDS=600"
                        "DORG_SUITE_JOB_IMAGE_BUILD_SECONDS=123"
                        (concat "DORG_SUITE_JOB_NETWORK_MODE="
                                (or (getenv "DORG_SUITE_JOB_NETWORK_MODE")
                                    "none"))
                        (concat "DORG_SUITE_JOB_NETWORK_PROBE_FIXTURE="
                                network-receipt)
                        (concat "DORG_SUITE_JOB_IMAGE_CONFIG=" image-config)
                        (concat "DORG_SUITE_JOB_MOUNTINFO=" mountinfo)
                        "DORG_SUITE_JOB_FORBIDDEN_PATHS=/definitely-not-a-dorg-host-path")
                  process-environment)))
    (dorg-test-job-test--fake-bin bin all-make-steps-pass)
    (dorg-test-job-test--write-mountinfo mountinfo extra-mount)
    (with-temp-file image-config
      (insert (or image-config-contents dorg-test-job-test--image-config-fixture)))
    (with-temp-file network-receipt
      (insert dorg-test-job-test--network-receipt-fixture))
    (make-directory artifacts t)
    (with-temp-buffer
      (insert "fake bundle")
      (let ((out (generate-new-buffer " *dorg-suite-job-entrypoint*")))
        (unwind-protect
            (let ((status
                   (call-process-region
                    (point-min) (point-max)
                    dorg-test-job-test--entrypoint nil out nil
                    "run" "--source-bundle" "-" "--commit" sha
                    "--artifacts" artifacts "--test-mode")))
              (cons status
                    (with-current-buffer out (buffer-string))))
          (kill-buffer out))))))

(defun dorg-test-job-test--git (dir &rest args)
  "Run git ARGS in DIR, returning stdout or failing the test."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((status (apply #'call-process "git" nil t nil args)))
        (unless (zerop status)
          (ert-fail
           (format "git %s failed with %s:\n%s"
                   (mapconcat #'identity args " ")
                   status
                   (buffer-string))))
        (string-trim (buffer-string))))))

(defun dorg-test-job-test--init-repo (dir)
  "Create a two-commit git repo in DIR and return (OLD NEW)."
  (make-directory dir t)
  (dorg-test-job-test--git dir "init" "-q")
  (dorg-test-job-test--git dir "config" "user.name" "test")
  (dorg-test-job-test--git dir "config" "user.email" "test@example.invalid")
  (with-temp-file (expand-file-name "file.txt" dir)
    (insert "one\n"))
  (dorg-test-job-test--git dir "add" "file.txt")
  (dorg-test-job-test--git dir "commit" "-q" "-m" "one")
  (let ((old (dorg-test-job-test--git dir "rev-parse" "HEAD")))
    (with-temp-file (expand-file-name "file.txt" dir)
      (insert "two\n"))
    (dorg-test-job-test--git dir "commit" "-am" "two" "-q")
    (list old (dorg-test-job-test--git dir "rev-parse" "HEAD"))))

(defmacro dorg-test-job-test--with-tempdir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "dorg-suite-job-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(ert-deftest dorg-test-job-private-venue-real-git-controls ()
  "The venue command pushes only to the verified private repo."
  (should (executable-find "python3"))
  (let ((default-directory dorg-test-job-test--root))
    (with-temp-buffer
      (let ((status (call-process "python3" nil t nil
                                  "tests/test_private_venue.py")))
        (unless (eq status 0)
          (ert-fail (buffer-string)))))))

(ert-deftest dorg-test-job-actions-venue-refuses-untrusted-events-and-repos ()
  "Only the public main push and trusted private venue pass admission."
  (dolist (venue '(("cosmicz/dorg" "https://github.com" "pull_request" "refs/pull/1/merge")
                   ("cosmicz/dorg" "https://github.com" "workflow_dispatch" "refs/heads/main")
                   ("cosmicz/dorg" "https://github.com" "push" "refs/heads/trunk")
                   ("cosmicz/dorg" "https://github.com" "push" "refs/heads/ci/probe")
                   ("someone/dorg" "https://github.com" "push" "refs/heads/main")
                   ("" "https://github.com" "push" "refs/heads/main")
                   ("cosmicz/dorg" "https://other.example" "push" "refs/heads/main")
                   ("cosmicz/dorg" "" "push" "refs/heads/main")
                   ("cosmicz/DOrg.oc" "https://other.example" "pull_request" "refs/pull/1/merge")
                   ("cosmicz/DOrg.oc" "" "pull_request" "refs/pull/1/merge")))
    (dorg-test-job-test--with-tempdir temp
      (let* ((repository (car venue))
             (server-url (cadr venue))
             (event (nth 2 venue))
             (ref (nth 3 venue))
             (sha (make-string 40 ?1))
             (process-environment
              (append (list (concat "DORG_SUITE_JOB_GITHUB_REPOSITORY=" repository)
                            (concat "DORG_SUITE_JOB_GITHUB_SERVER_URL=" server-url))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp sha t nil "actions" nil nil event ref))
             (artifacts (expand-file-name "artifacts" temp))
             (manifest (dorg-test-job-test--file-string
                        (expand-file-name "manifest.txt" artifacts)))
             (raw (dorg-test-job-test--file-string
                   (expand-file-name "raw.log" artifacts))))
        (should (eq 2 (car result)))
        (should (string-match-p "Actions venue mismatch" (cdr result)))
        (should (string-match-p
                 (concat "github_repository=" repository) manifest))
        (should (string-match-p
                 (concat "github_server_url=" server-url) manifest))
        (should-not (string-match-p "== compile ==" raw))))))

(ert-deftest dorg-test-job-actions-venue-records-public-main-push ()
  "The public release main push reaches the actual suite entrypoint."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (process-environment
            (append '("DORG_SUITE_JOB_GITHUB_REPOSITORY=cosmicz/dorg"
                      "DORG_SUITE_JOB_GITHUB_SERVER_URL=https://github.com")
                    process-environment))
           (result (dorg-test-job-test--run-entrypoint
                    temp sha t nil "actions" nil nil "push" "refs/heads/main"))
           (manifest (dorg-test-job-test--file-string
                      (expand-file-name "artifacts/manifest.txt" temp))))
      (should (zerop (car result)))
      (should (string-match-p "github_repository=cosmicz/dorg" manifest))
      (should (string-match-p "event_name=push" manifest))
      (should (string-match-p "github_ref=refs/heads/main" manifest)))))

(ert-deftest dorg-test-job-actions-venue-records-private-pass ()
  "The private Actions venue still admits PRs, dispatches and trunk pushes."
  (dolist (event-ref '(("pull_request" "refs/pull/1/merge")
                      ("workflow_dispatch" "refs/heads/ci/candidate")
                      ("push" "refs/heads/trunk")))
    (dorg-test-job-test--with-tempdir temp
      (let* ((sha (make-string 40 ?1))
             (process-environment
              (append '("DORG_SUITE_JOB_GITHUB_REPOSITORY=cosmicz/DOrg.oc"
                        "DORG_SUITE_JOB_GITHUB_SERVER_URL=https://github.com")
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp sha t nil "actions" nil nil
                      (car event-ref) (cadr event-ref)))
             (manifest (dorg-test-job-test--file-string
                        (expand-file-name "artifacts/manifest.txt" temp))))
        (should (zerop (car result)))
        (should (string-match-p "github_repository=cosmicz/DOrg.oc" manifest))
        (should (string-match-p "github_server_url=https://github.com" manifest))
        (should (string-match-p (concat "event_name=" (car event-ref)) manifest))
        (should (string-match-p (concat "github_ref=" (cadr event-ref)) manifest))))))

(ert-deftest dorg-test-job-network-probe-controls ()
  "The real probe distinguishes no route from reachable or blocked egress."
  (should (executable-find "python3"))
  (let ((default-directory dorg-test-job-test--root))
    (with-temp-buffer
      (let ((status (call-process "python3" nil t nil
                                  "tests/test_suite_job_network.py")))
        (unless (eq status 0)
          (ert-fail (buffer-string)))))))

(ert-deftest dorg-test-job-entrypoint-refuses-nonisolated-network ()
  "A claimed bridge network is refused before source admission."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (process-environment
            (cons "DORG_SUITE_JOB_NETWORK_MODE=bridge" process-environment))
           (result (dorg-test-job-test--run-entrypoint temp sha t))
           (manifest (dorg-test-job-test--file-string
                      (expand-file-name "artifacts/manifest.txt" temp)))
           (raw (dorg-test-job-test--file-string
                 (expand-file-name "artifacts/raw.log" temp))))
      (should (eq 2 (car result)))
      (should (string-match-p "network mode mismatch" (cdr result)))
      (should (string-match-p "network_expected_mode=bridge" manifest))
      (should (string-match-p "network_mode=none" manifest))
      (should-not (string-match-p "== compile ==" raw)))))

(ert-deftest dorg-test-job-launcher-refuses-network-override ()
  "A caller cannot change the suite container network through its environment."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (record (expand-file-name "docker-record.txt" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          "DORG_SUITE_JOB_NETWORK_MODE=host"
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_RECORD=" record))
                    process-environment)))
      (make-directory artifacts t)
      (should (= 64 (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image" "--commit" sha
                                  "--artifacts" artifacts)))
      (should-not (file-exists-p record)))))

(defun dorg-test-job-test--contains-token-p (text token)
  "Return non-nil when TEXT contains TOKEN as a shell/list token."
  (string-match-p
   (format "\\(?:^\\|[^[:alnum:]_.+-]\\)%s\\(?:$\\|[^[:alnum:]_.+-]\\)"
           (regexp-quote token))
   text))

(ert-deftest dorg-test-job-source-packet-real-git-controls ()
  "The packet importer verifies complete exact sources before runtime entry."
  (skip-unless (executable-find "python3"))
  (let ((default-directory dorg-test-job-test--root))
    (with-temp-buffer
      (let ((status (call-process "python3" nil t nil
                                  "tests/test_suite_source_packet.py")))
        (unless (eq status 0)
          (ert-fail (buffer-string)))))))

(ert-deftest dorg-test-job-launcher-cancellation-controls ()
  "Catchable signals retain container custody and a truthful status, no daemon.
The Python controls drive the real host launcher against a fake Docker client
and assert the client and owned container are gone before their own rescue."
  (skip-unless (and (executable-find "python3")
                    (executable-find "timeout")))
  (let ((default-directory dorg-test-job-test--root))
    (with-temp-buffer
      (unless (eq 0 (call-process "timeout" nil t nil "-k" "5" "240"
                                 "python3" "tests/test_suite_job_cancellation.py"))
        (ert-fail (buffer-string))))))

(ert-deftest dorg-test-job-source-packet-invalid-input-never-enters-make ()
  "The real entrypoint refuses invalid transport before executing any target."
  (skip-unless (executable-find "python3"))
  (dorg-test-job-test--with-tempdir temp
    (let* ((artifacts (expand-file-name "artifacts" temp))
           (image-config (expand-file-name "image-config" temp))
           (network-receipt (expand-file-name "network-receipt" temp))
           (process-environment
            (append (list (concat "JOB_ROOT=" (expand-file-name "job" temp))
                          (concat "DORG_SUITE_JOB_IMAGE_CONFIG=" image-config)
                          "DORG_SUITE_JOB_NETWORK_MODE=none"
                          (concat "DORG_SUITE_JOB_NETWORK_PROBE_FIXTURE="
                                  network-receipt))
                    process-environment)))
      (with-temp-file image-config
        (insert dorg-test-job-test--image-config-fixture))
      (with-temp-file network-receipt
        (insert dorg-test-job-test--network-receipt-fixture))
      (with-temp-buffer
        (insert "invalid source packet")
        (let ((out (generate-new-buffer " *packet-refusal*")))
          (unwind-protect
              (should
               (eq 2 (call-process-region
                      (point-min) (point-max) dorg-test-job-test--entrypoint
                      nil out nil "run" "--source-packet" "-"
                      "--commit" (make-string 40 ?1) "--tree" (make-string 40 ?2)
                      "--artifacts" artifacts "--test-mode")))
            (kill-buffer out))))
      (let ((log (dorg-test-job-test--file-string
                  (expand-file-name "raw.log" artifacts)))
            (status (dorg-test-job-test--file-string
                     (expand-file-name "exit.status" artifacts))))
        (should (string-match-p "source packet refused:" log))
        (should-not (string-match-p "== compile ==" log))
        (should (string-match-p "classification=incomplete" status))
        (should-not (file-exists-p
                     (expand-file-name "probe/source-packet.json" artifacts)))))))

(ert-deftest dorg-test-job-source-packet-launcher-separates-candidate-from-tooling ()
  "Packet transport forwards exact identities without substituting tooling HEAD."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (record (expand-file-name "docker-record.txt" temp))
           (packet (expand-file-name "source.tar" temp))
           (tooling (dorg-test-job-test--fake-bin bin t))
           (candidate (make-string 40 ?2))
           (tree (make-string 40 ?3))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" temp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_GIT_BUNDLE_EXIT=99"
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_RECORD=" record))
                    process-environment)))
      (write-region "packet fixture" nil packet nil 'silent)
      (should (eq 0 (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image" "--commit" candidate
                                  "--tree" tree "--source-packet" packet
                                  "--artifacts" artifacts)))
      (let ((text (dorg-test-job-test--file-string record)))
        (dolist (expected (list "run --rm --init --network none --user"
                               "--source-packet -"
                               (concat "--commit " candidate)
                               (concat "--tree " tree)
                               (concat "DORG_SUITE_JOB_WORKSPACE_SHA=" tooling)))
          (should (string-match-p (regexp-quote expected) text)))))))

(ert-deftest dorg-test-job-rejects-symbolic-short-and-non-hex-commits ()
  "The job accepts only independently resolved full 40-hex SHAs."
  (should (equal (make-string 40 ?a)
                 (dorg-test-job-validate-commit (make-string 40 ?a))))
  (dolist (bad '("HEAD"
                 "main"
                 "0123456"
                 "01234567890123456789012345678901234567xz"))
    (should-error (dorg-test-job-validate-commit bad) :type 'user-error)))

(ert-deftest dorg-test-job-launcher-bundles-head-not-bare-sha ()
  "The launcher bundle command records a real ref while --commit is separate."
  (should (equal '("git" "bundle" "create" "-" "HEAD")
                 (dorg-test-job-launcher-bundle-command)))
  (should-not (member "--commit" (dorg-test-job-launcher-bundle-command))))

(ert-deftest dorg-test-job-bundle-checkout-requires-head-equals-commit ()
  "A bundle whose HEAD differs from --commit fails before the gate."
  (dorg-test-job-test--with-tempdir temp
    (let* ((repo (expand-file-name "repo" temp))
           (commits (dorg-test-job-test--init-repo repo))
           (old (car commits))
           (new (cadr commits))
           (bundle (expand-file-name "repo.bundle" temp))
           (job-root (expand-file-name "job" temp)))
      (dorg-test-job-test--git repo "bundle" "create" bundle "HEAD")
      (should (equal new (plist-get (dorg-test-job-checkout-bundle
                                     bundle new job-root)
                                    :head)))
      (should-error (dorg-test-job-checkout-bundle
                     bundle old (expand-file-name "mismatch" temp))
                    :type 'user-error))))

(ert-deftest dorg-test-job-bundle-checkout-rejects-shallow-history-gap ()
  "A shallow-origin bundle that clones but cannot traverse parents is rejected."
  (dorg-test-job-test--with-tempdir temp
    (let* ((repo (expand-file-name "repo" temp))
           (shallow (expand-file-name "shallow" temp))
           (bundle (expand-file-name "shallow.bundle" temp))
           (job-root (expand-file-name "job" temp)))
      (dorg-test-job-test--init-repo repo)
      (dorg-test-job-test--git temp "clone" "--depth" "1"
                               (concat "file://" repo) shallow "-q")
      (let ((sha (dorg-test-job-test--git shallow "rev-parse" "HEAD")))
        (dorg-test-job-test--git shallow "bundle" "create" bundle "HEAD")
        (should-error (dorg-test-job-checkout-bundle bundle sha job-root)
                      :type 'user-error)))))

(ert-deftest dorg-test-job-mountinfo-allows-only-artifact-bind ()
  "The primary isolation assertion is the mountinfo allowlist."
  (let* ((ok-lines '("1 0 0:1 / / rw - overlay overlay rw"
                     "2 1 0:2 / /proc rw - proc proc rw"
                     "3 2 0:3 /bus /proc/bus ro shared:12 - proc proc ro"
                     "4 1 0:4 / /dev rw - tmpfs tmpfs rw"
                     "5 4 0:5 / /dev/pts rw - devpts devpts rw"
                     "6 1 0:6 / /sys rw - sysfs sysfs rw"
                     "3 1 0:3 / /artifacts rw - virtiofs /host/artifacts rw"
                     "4 1 0:4 / /etc/hosts rw - virtiofs /etc/hosts rw"))
         (bad-lines (append ok-lines
                            '("5 1 0:5 / /workspace rw - virtiofs /repo rw")))
         (bad-runtime-fstype (append ok-lines
                                     '("6 1 0:6 / /dev/host rw - virtiofs /host rw")))
         (ok (dorg-test-job-mount-report ok-lines))
         (bad (dorg-test-job-mount-report bad-lines))
         (bad-fstype (dorg-test-job-mount-report bad-runtime-fstype)))
    (should (plist-get ok :ok))
    (should (equal '("/artifacts") (plist-get ok :caller-bind-targets)))
    (should-not (plist-get bad :ok))
    (should (member "/workspace" (plist-get bad :unexpected-targets)))
    (should-not (plist-get bad-fstype :ok))
    (should (member "/dev/host" (plist-get bad-fstype :unexpected-targets)))))

(ert-deftest dorg-test-job-exit-status-records-fail-fast-skips ()
  "Compile/lint/test statuses are distinct and later steps are skipped."
  (should
   (equal '((compile . 0) (lint . 12) (test . skipped))
          (dorg-test-job-step-statuses '((compile . 0) (lint . 12))))))

(ert-deftest dorg-test-job-entrypoint-preserves-real-pipeline-status ()
  "The shipped entrypoint preserves the producer status through tee."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (result (dorg-test-job-test--run-entrypoint temp sha))
           (artifacts (expand-file-name "artifacts" temp))
           (output (cdr result))
           (exit-text (dorg-test-job-test--file-string
                       (expand-file-name "exit.status" artifacts))))
      (should-not (zerop (car result)))
      (should (string-match-p "== compile ==" output))
      (should (string-match-p "== lint ==" output))
      (should (string-match-p "lint-failed" output))
      (should (string-match-p "^compile=0$" exit-text))
      (should (string-match-p "^lint=7$" exit-text))
      (should (string-match-p "^test=skipped$" exit-text))
      (should-not (string-match-p "^test=0$" exit-text)))))

(ert-deftest dorg-test-job-entrypoint-pipeline-uses-pipestatus ()
  "The shipped entrypoint keeps the explicit PIPESTATUS guard."
  (let ((entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint)))
    (should (string-match-p "set -euo pipefail" entrypoint))
    (should (string-match-p
             (regexp-quote "local status=\"${PIPESTATUS[0]}\"")
             entrypoint))))

(ert-deftest dorg-test-job-entrypoint-rejects-git-version-drift ()
  "The image's Git contract is observed before running compile or tests."
  (dolist (version '("2.49.0" "2.39.5"))
    (dorg-test-job-test--with-tempdir temp
      (let* ((process-environment
              (cons (concat "DORG_TEST_JOB_FAKE_GIT_VERSION=" version)
                    process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp (make-string 40 ?1) t))
             (artifacts (expand-file-name "artifacts" temp))
             (versions (dorg-test-job-test--file-string
                        (expand-file-name "probe/versions.txt" artifacts))))
        (should (string-match-p (regexp-quote (concat "git version " version))
                                versions))
        (if (equal version "2.49.0")
            (progn
              (should (zerop (car result)))
              (should (string-match-p
                       "^git_version=git version 2\\.49\\.0$"
                       (dorg-test-job-test--file-string
                        (expand-file-name "manifest.txt" artifacts)))))
          (should-not (zerop (car result)))
          (should (string-match-p "Git version mismatch" (cdr result)))
          (should-not (string-match-p "== compile ==" (cdr result))))))))

(ert-deftest dorg-test-job-entrypoint-rejects-terminal-process-probe-drift ()
  "Capability probes fail before compile and record their green evidence."
  (dolist (row '((nil nil nil 0)
                 ("absent" nil nil 1)
                 (nil "ps from BSD" nil 1)
                 (nil nil "pgrep from BSD" 1)))
    (dorg-test-job-test--with-tempdir temp
      (pcase-let ((`(,tmux ,ps ,pgrep ,ok) row))
        (let* ((process-environment (append (delq nil (list (and tmux (concat "DORG_TEST_JOB_FAKE_TMUX=" tmux)) (and ps (concat "DORG_TEST_JOB_FAKE_PS=" ps)) (and pgrep (concat "DORG_TEST_JOB_FAKE_PGREP=" pgrep)))) process-environment))
               (result (dorg-test-job-test--run-entrypoint temp (make-string 40 ?1) t))
               (manifest (dorg-test-job-test--file-string (expand-file-name "artifacts/manifest.txt" temp)))
               (versions (dorg-test-job-test--file-string (expand-file-name "artifacts/probe/versions.txt" temp)))
               (entrypoint (dorg-test-job-test--file-string dorg-test-job-test--entrypoint)))
          (if (zerop ok)
              (progn (should (zerop (car result))) (should (string-match-p "^tmux 3\\.3a$" versions)) (should (string-match-p "^ps from procps-ng 4\\.0\\.2$" versions)) (should (string-match-p "^tmux_version=" manifest)) (should (string-match-p "^procps_version=" manifest)))
            (progn (should-not (zerop (car result))) (should-not (string-match-p "== compile ==" (cdr result))) (should (string-match-p (cond (tmux "tmux missing") (ps "procps ps missing") (t "procps pgrep missing")) entrypoint)))))))))

(ert-deftest dorg-test-job-entrypoint-dolt-warning-keeps-probes-distinct ()
  "Both forms of the Dolt warning preserve the next real entrypoint probe."
  (dolist (warning '("unterminated" "terminated"))
    (dorg-test-job-test--with-tempdir temp
      (let* ((argv-log (expand-file-name "dolt-argv.log" temp))
             (process-environment
              (append (list (concat "DORG_TEST_JOB_FAKE_DOLT_WARNING=" warning)
                            (concat "DORG_TEST_JOB_FAKE_DOLT_ARGV_LOG=" argv-log))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp (make-string 40 ?1) t))
             (artifacts (expand-file-name "artifacts" temp))
             (versions (dorg-test-job-test--file-string
                        (expand-file-name "probe/versions.txt" artifacts)))
             (manifest (dorg-test-job-test--file-string
                        (expand-file-name "manifest.txt" artifacts)))
             (calls (split-string
                     (dorg-test-job-test--file-string argv-log) "\n" t)))
        (should (zerop (car result)))
        (should (equal (cl-subseq calls 0 (min 2 (length calls)))
                       '("config --global --add metrics.disabled true"
                         "config --global --add versioncheck.disabled true")))
        (should (string-match-p
                 "^Warning: unable to query latest released Dolt version$"
                 versions))
        (should (string-match-p "^tmux 3\\.3a$" versions))
        (should (string-match-p "^dolt_version=1\\.86\\.4$" manifest))
        (should (string-match-p "^tmux_version=tmux 3\\.3a$" manifest))
        (should (string-match-p "^dolt_metrics_disabled=true$" manifest))
        (should (string-match-p "^dolt_versioncheck_disabled=true$"
                                manifest))))))

(ert-deftest dorg-test-job-entrypoint-refuses-failed-dolt-config ()
  "A Dolt config write failure stops before any version or compile call."
  (dolist (key '("metrics.disabled" "versioncheck.disabled"))
    (dorg-test-job-test--with-tempdir temp
      (let* ((argv-log (expand-file-name "dolt-argv.log" temp))
             (process-environment
              (append (list (concat "DORG_TEST_JOB_FAKE_DOLT_CONFIG_FAIL=" key)
                            (concat "DORG_TEST_JOB_FAKE_DOLT_ARGV_LOG=" argv-log))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp (make-string 40 ?1) t))
             (calls (split-string
                     (dorg-test-job-test--file-string argv-log) "\n" t)))
        (should-not (zerop (car result)))
        (should-not (member "version" calls))
        (should-not (string-match-p "== compile ==" (cdr result)))))))

(ert-deftest dorg-test-job-entrypoint-rejects-extra-probe-output ()
  "Dolt and ordinary probes reject extra lines before compile."
  (dolist (setting '(("DORG_TEST_JOB_FAKE_DOLT_WARNING" . "extra")
                     ("DORG_TEST_JOB_FAKE_TMUX" . "tmux 3.3a\nextra")
                     ("DORG_TEST_JOB_FAKE_TMUX" . "tmux 3.3aextra")
                     ("DORG_TEST_JOB_FAKE_GNUTLS" . "gnutls=t\nextra")))
    (dorg-test-job-test--with-tempdir temp
      (let* ((process-environment
              (cons (concat (car setting) "=" (cdr setting))
                    process-environment))
             (result (dorg-test-job-test--run-entrypoint
                      temp (make-string 40 ?1) t)))
        (should-not (zerop (car result)))
        (should-not (string-match-p "== compile ==" (cdr result)))))))

(ert-deftest dorg-test-job-entrypoint-enforces-real-mount-allowlist ()
  "The shipped entrypoint fails on an unexpected mountinfo target."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (result (dorg-test-job-test--run-entrypoint
                    temp sha t "/dev/host"))
           (artifacts (expand-file-name "artifacts" temp)))
      (should-not (zerop (car result)))
      (should (file-exists-p
               (expand-file-name "probe/unexpected-mounts.txt" artifacts)))
      (should (string-match-p
               "/dev/host"
               (dorg-test-job-test--file-string
                (expand-file-name "probe/unexpected-mounts.txt" artifacts))))
      (dolist (file '("raw.log" "probe/failure.txt"))
        (should (string-match-p
                 "unexpected mount targets"
                 (dorg-test-job-test--file-string
                  (expand-file-name file artifacts))))))))

(ert-deftest dorg-test-job-entrypoint-admits-exact-docker-init-mount ()
  "Docker's init file admits both usrmerge layouts, never neighboring paths."
  (dolist (target '("/sbin/docker-init" "/usr/sbin/docker-init"
                    "/sbin/docker-init-copy" "/usr/sbin/docker-init-copy"))
    (dorg-test-job-test--with-tempdir temp
      (let* ((result (dorg-test-job-test--run-entrypoint
                      temp (make-string 40 ?1) t target))
             (report (dorg-test-job-mount-report
                      (split-string
                       (dorg-test-job-test--file-string
                        (expand-file-name "mountinfo" temp)) "\n" t)))
             (allowed (not (null (member target '("/sbin/docker-init"
                                                  "/usr/sbin/docker-init"))))))
        (should (eq allowed (zerop (car result))))
        (should (eq allowed (not (null (plist-get report :ok)))))
        (should (equal '("/artifacts")
                       (plist-get report :caller-bind-targets)))))))

(ert-deftest dorg-test-job-entrypoint-rejects-actions-empty-image-digest ()
  "Actions evidence must not silently omit image digest provenance."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (process-environment
            (append '("DORG_SUITE_JOB_GITHUB_REPOSITORY=cosmicz/DOrg.oc"
                      "DORG_SUITE_JOB_GITHUB_SERVER_URL=https://github.com")
                    process-environment))
           (result (dorg-test-job-test--run-entrypoint
                    temp sha t nil "actions" ""))
           (artifacts (expand-file-name "artifacts" temp)))
      (should (= 2 (car result)))
      (dolist (file '("raw.log" "probe/failure.txt"))
        (should (string-match-p
                 "Actions image_digest missing"
                 (dorg-test-job-test--file-string
                  (expand-file-name file artifacts)))))
      (should (string-match-p
               "launcher=actions"
               (dorg-test-job-test--file-string
                (expand-file-name "manifest.txt" artifacts)))))))

(ert-deftest dorg-test-job-entrypoint-writes-success-artifact-layout ()
  "A fake successful entrypoint run writes the required probe files."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (result (dorg-test-job-test--run-entrypoint temp sha t))
           (artifacts (expand-file-name "artifacts" temp)))
      (should (zerop (car result)))
      (dolist (file '("raw.log"
                      "ert-summary.txt"
                      "exit.status"
                      "manifest.txt"
                      "probe/inside.txt"
                      "probe/mounts.txt"
                      "probe/paths.txt"
                      "probe/env.txt"
                      "probe/versions.txt"
                      "probe/git.txt"
                      "probe/dolt-config.txt"))
        (should (file-exists-p (expand-file-name file artifacts))))
      (should-not (file-directory-p
                   (expand-file-name "fixture-state" artifacts)))
      (let ((manifest (dorg-test-job-test--file-string
                       (expand-file-name "manifest.txt" artifacts))))
        (should (string-match-p "launcher=unit-test" manifest))
        (should (string-match-p "event_name=pull_request" manifest))
        (should (string-match-p "github_ref=refs/pull/1/merge" manifest))
        (should (string-match-p "github_head_ref=feature/testjob" manifest))
        (should (string-match-p (concat "launcher_workspace_sha=" sha)
                                manifest))
        (should (string-match-p "entrypoint_version=1" manifest))
        (should (string-match-p "image_id=sha256:111image" manifest))
        (should (string-match-p "image_digest=sha256:222digest" manifest))
        (should (string-match-p
                 "image_metadata_containerimage_digest=sha256:222digest"
                 manifest))
        (should (string-match-p
                 "image_metadata_build_ref=builder/default/abc123"
                 manifest))
        (should (string-match-p
                 "image_metadata_base_image_digest=sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171"
                 manifest))
        (should-not (string-match-p "^  \\\"" manifest))
        (should-not (string-match-p "^image_metadata={" manifest))
        (should (string-match-p "image_cache_inferred=hit" manifest))
        (should (string-match-p
                 "image_cache_inference_threshold_seconds=600"
                 manifest))
        (should (string-match-p "image_build_seconds=123" manifest))
        (should (string-match-p "test_mode=true" manifest))
        (should (string-match-p "network_mode=none" manifest))
        (should (string-match-p "network_egress_allowlist=none" manifest))
        (should (string-match-p "network_ipv4_connect=ENETUNREACH" manifest))
        (should (string-match-p "network_ipv6_connect=ENETUNREACH" manifest))
        (should (string-match-p "history_connectivity=ok" manifest))
        (should (string-match-p "artifact_completeness=complete" manifest))
        (should (string-match-p "emacs_native_comp=nil" manifest)))
      (should-not (string-match-p "Permission denied" (cdr result)))
      (let ((inside (dorg-test-job-test--file-string
                     (expand-file-name "probe/inside.txt" artifacts))))
        (should (string-match-p "dorg-suite-job-forbidden-canary" inside))))))

(ert-deftest dorg-test-job-launcher-positive-control-detects-and-removes-residue ()
  "The local launcher positive control distinguishes detected residue."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp))
                    process-environment)))
      (ignore sha)
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--artifacts" artifacts
                                  "--positive-control-residue")))
        (should (= 1 status))
        (should (string-match-p
                 "positive-control=detected"
                 (dorg-test-job-test--file-string
                  (expand-file-name
                   "host-census/positive-control.txt" artifacts))))
        (should-not
         (directory-files tmp nil
                          "\\`dorg-suite-job\\.positive-control\\."))))))

(ert-deftest dorg-test-job-launcher-fails-on-host-census-residue ()
  "A passing suite is failed by non-control residue found after the job."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (state (expand-file-name "fake-docker-state" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_STATE=" state)
                          "DORG_TEST_JOB_FAKE_DOCKER_CONTAINER_AFTER=residue-container"
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 1 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts)))
              (manifest (dorg-test-job-test--file-string
                         (expand-file-name "manifest.txt" artifacts)))
              (diff (dorg-test-job-test--file-string
                     (expand-file-name "host-census/diff.txt" artifacts))))
          (should (string-match-p "^compile=0$" exit-text))
          (should (string-match-p "^lint=0$" exit-text))
          (should (string-match-p "^test=0$" exit-text))
          (should (string-match-p "^host_census=fail$" exit-text))
          (should (string-match-p
                   "^host_census_residue_classes=docker-container$"
                   exit-text))
          (should (string-match-p "^overall=1$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text))
          (should (string-match-p "^host_census=fail$" manifest))
          (should (string-match-p "residue-container" diff)))))))

(ert-deftest dorg-test-job-launcher-preserves-incomplete-container-outcome ()
  "Host reconciliation preserves container incomplete status and exit code."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=2"
                          "DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=incomplete"
                          "DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=2")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^compile=0$" exit-text))
          (should (string-match-p "^lint=0$" exit-text))
          (should (string-match-p "^test=0$" exit-text))
          (should (string-match-p "^container_overall=2$" exit-text))
          (should (string-match-p
                   "^container_classification=incomplete$"
                   exit-text))
          (should (string-match-p "^host_census=pass$" exit-text))
          (should (string-match-p "^host_census_status=0$" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-passes-complete-success-evidence ()
  "A launcher pass requires successful process exits and complete evidence."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (zerop status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^bundle_status=0$" exit-text))
          (should (string-match-p "^container_status=0$" exit-text))
          (should (string-match-p "^evidence_completeness=complete$"
                                  exit-text))
          (should (string-match-p "^overall=0$" exit-text))
          (should (string-match-p "^classification=pass$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-fails-on-runtime-exit-after-pass-receipt ()
  "A pass receipt cannot override a nonzero container/runtime exit."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=137")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 137 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^bundle_status=0$" exit-text))
          (should (string-match-p "^container_status=137$" exit-text))
          (should (string-match-p "^receipt_overall=0$" exit-text))
          (should (string-match-p "^receipt_classification=pass$" exit-text))
          (should (string-match-p "^overall=137$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-fails-on-bundle-exit-after-pass-receipt ()
  "A pass receipt cannot override a failing bundle producer."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_GIT_BUNDLE_EXIT=9")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 9 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^bundle_status=9$" exit-text))
          (should (string-match-p "^container_status=0$" exit-text))
          (should (string-match-p "^receipt_overall=0$" exit-text))
          (should (string-match-p "^receipt_classification=pass$" exit-text))
          (should (string-match-p "^overall=9$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-incomplete-with-no-evidence ()
  "A zero process exit with no evidence is incomplete, never pass."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp))
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^bundle_status=0$" exit-text))
          (should (string-match-p "^container_status=0$" exit-text))
          (should (string-match-p "^receipt_present=false$" exit-text))
          (should (string-match-p "^evidence_completeness=incomplete$"
                                  exit-text))
          (should (string-match-p "raw.log" exit-text))
          (should (string-match-p "ert-summary.txt" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-incomplete-with-partial-evidence ()
  "A pass receipt missing required evidence is incomplete."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_PARTIAL_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^receipt_overall=0$" exit-text))
          (should (string-match-p "^receipt_classification=pass$" exit-text))
          (should (string-match-p "^evidence_completeness=incomplete$"
                                  exit-text))
          (should (string-match-p "ert-summary.txt" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-incomplete-with-malformed-ert-summary ()
  "A claimed pass with a malformed ERT summary is incomplete."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_ERT_SUMMARY=ERT summary not found; see raw.log")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^receipt_overall=0$" exit-text))
          (should (string-match-p "^receipt_classification=pass$" exit-text))
          (should (string-match-p "^evidence_completeness=incomplete$"
                                  exit-text))
          (should (string-match-p "ert-summary.txt:summary" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-incomplete-with-contradictory-step-receipt ()
  "A claimed pass whose per-step status failed is incomplete."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_TEST_STATUS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^test=1$" exit-text))
          (should (string-match-p "^receipt_overall=0$" exit-text))
          (should (string-match-p "^receipt_classification=pass$" exit-text))
          (should (string-match-p "^evidence_completeness=incomplete$"
                                  exit-text))
          (should (string-match-p "exit.status:test" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-fails-closed-on-nonnumeric-overall ()
  "A malformed container overall is exported as numeric incomplete evidence."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=not-a-number"
                          "DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=pass")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 2 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^container_overall_raw=not-a-number$"
                                  exit-text))
          (should (string-match-p "^container_overall=2$" exit-text))
          (should (string-match-p
                   "^container_classification=incomplete$"
                   exit-text))
          (should (string-match-p "^host_census=pass$" exit-text))
          (should (string-match-p "^overall=2$" exit-text))
          (should (string-match-p "^classification=incomplete$"
                                  exit-text)))))))

(ert-deftest dorg-test-job-launcher-fails-on-artifact-ownership-mismatch ()
  "Exported artifacts not owned by the launcher uid/gid fail host census."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_ROOT_OWNED_ARTIFACT=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 1 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^compile=0$" exit-text))
          (should (string-match-p "^host_census=fail$" exit-text))
          (should (string-match-p
                   "^host_census_residue_classes=artifact-ownership$"
                   exit-text))
          (should (string-match-p "^overall=1$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-allows-same-uid-readonly-files ()
  "Read-only exported files are removable when the containing directory is."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_READONLY_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (zerop status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^host_census=pass$" exit-text))
          (should (string-match-p "^host_census_residue_classes=$"
                                  exit-text))
          (should (string-match-p "^overall=0$" exit-text))
          (should (string-match-p "^classification=pass$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-fails-on-readonly-artifact-directory ()
  "A non-writable exported directory is host residue."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_READONLY_ARTIFACT_DIR=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (unwind-protect
          (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                      "--image" "fake-image"
                                      "--commit" sha
                                      "--artifacts" artifacts)))
            (should (= 1 status))
            (let ((exit-text (dorg-test-job-test--file-string
                              (expand-file-name "exit.status" artifacts))))
              (should (string-match-p "^host_census=fail$" exit-text))
              (should (string-match-p
                       "^host_census_residue_classes=artifact-ownership$"
                       exit-text))
              (should (string-match-p "^overall=1$" exit-text))
              (should (string-match-p "^classification=fail$" exit-text))))
        (let ((fixture-state (expand-file-name "fixture-state" artifacts)))
          (when (file-directory-p fixture-state)
            (set-file-modes fixture-state #o755)))))))

(ert-deftest dorg-test-job-launcher-fails-on-one-exported-file-wrong-uid ()
  "A single exported artifact with the wrong uid fails host census."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_BAD_ARTIFACT_BASENAME=raw.log")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 1 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^host_census=fail$" exit-text))
          (should (string-match-p
                   "^host_census_residue_classes=artifact-ownership$"
                   exit-text))
          (should (string-match-p "^overall=1$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-accepts-writable-same-uid-artifacts ()
  "Local artifacts may have a different gid when uid ownership is correct."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (uid (number-to-string (user-uid)))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          (concat "DORG_TEST_JOB_FAKE_ARTIFACT_OWNER="
                                  uid ":0"))
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (zerop status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^host_census=pass$" exit-text))
          (should (string-match-p "^host_census_residue_classes=$"
                                  exit-text))
          (should (string-match-p "^overall=0$" exit-text))
          (should (string-match-p "^classification=pass$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-classifies-volume-and-temp-residue ()
  "Volume and job temp root residue are separate host-census classes."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (state (expand-file-name "fake-docker-state" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_STATE=" state)
                          "DORG_TEST_JOB_FAKE_DOCKER_VOLUME_AFTER=residue-volume"
                          "DORG_TEST_JOB_FAKE_JOB_TEMP_AFTER=1"
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 1 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts)))
              (diff (dorg-test-job-test--file-string
                     (expand-file-name "host-census/diff.txt" artifacts))))
          (should (string-match-p
                   "^host_census_residue_classes=docker-volume,job-temp-root$"
                   exit-text))
          (should (string-match-p "residue-volume" diff))
          (should (string-match-p "dorg-suite-job\\.fake-residue" diff)))))))

(ert-deftest dorg-test-job-launcher-fails-on-docker-census-unknown ()
  "Docker census command failure is unknown/non-clean, not empty clean."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          "DORG_TEST_JOB_FAKE_DOCKER_PS_FAIL=3"
                          "DORG_TEST_JOB_FAKE_DOCKER_VOLUME_FAIL=4")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts)))
        (should (= 1 status))
        (let ((exit-text (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts)))
              (after (dorg-test-job-test--file-string
                      (expand-file-name "host-census/after.txt" artifacts))))
          (should (string-match-p
                   "^host_census_residue_classes=docker-container-unknown,docker-volume-unknown$"
                   exit-text))
          (should (string-match-p "unknown exit=3" after))
          (should (string-match-p "unknown exit=4" after))
          (should (string-match-p "^overall=1$" exit-text))
          (should (string-match-p "^classification=fail$" exit-text)))))))

(ert-deftest dorg-test-job-launcher-passes-actions-context-to-container ()
  "The real launcher can identify the Actions leg and requested head SHA."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (record (expand-file-name "docker-record.txt" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          "DORG_SUITE_JOB_EVENT_NAME=pull_request"
                          "DORG_SUITE_JOB_REF=refs/pull/1/merge"
                          "DORG_SUITE_JOB_HEAD_REF=feature/testjob"
                          "DORG_SUITE_JOB_GITHUB_REPOSITORY=cosmicz/DOrg.oc"
                          "DORG_SUITE_JOB_GITHUB_SERVER_URL=https://github.com"
                          "DORG_SUITE_JOB_IMAGE_ID=sha256:111image"
                          "DORG_SUITE_JOB_IMAGE_DIGEST=sha256:222digest"
                          (concat "DORG_SUITE_JOB_IMAGE_METADATA="
                                  dorg-test-job-test--image-metadata-fixture)
                          "DORG_SUITE_JOB_IMAGE_CACHE_INFERRED=hit"
                          "DORG_SUITE_JOB_IMAGE_CACHE_INFERENCE_THRESHOLD_SECONDS=600"
                          "DORG_SUITE_JOB_IMAGE_BUILD_SECONDS=123"
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_RECORD="
                                  record))
                    process-environment)))
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--launcher" "actions"
                                  "--artifacts" artifacts)))
        (should (zerop status))
        (let ((text (dorg-test-job-test--file-string record))
              (case-fold-search nil))
          (should (string-match-p
                   (regexp-quote "DORG_SUITE_JOB_LAUNCHER=actions")
                   text))
          (should (string-match-p "event=pull_request" text))
          (should (string-match-p "ref=refs/pull/1/merge" text))
          (should (string-match-p "head_ref=feature/testjob" text))
          (should (string-match-p "github_repository=cosmicz/DOrg.oc" text))
          (should (string-match-p "github_server_url=https://github.com" text))
          (should (string-match-p "args=run .*--network none " text))
          (should (string-match-p "DORG_SUITE_JOB_NETWORK_MODE=none" text))
          (let* ((args (cadr (split-string text "args=")))
                 (tokens (split-string args "[[:space:]]+" t)))
            (should (= 1 (cl-count "--network" tokens :test #'equal)))
            (should (equal "none" (cadr (member "--network" tokens))))
            (dolist (forbidden '("host" "bridge" "--add-host" "--dns"))
              (should-not (member forbidden tokens))))
          (should (string-match-p
                   (regexp-quote
                    (concat "DORG_SUITE_JOB_WORKSPACE_SHA=" sha))
                   text))
          (should (string-match-p "image_id=sha256:111image" text))
          (should (string-match-p "image_digest=sha256:222digest" text))
          (should (string-match-p "image_metadata={" text))
          (should (string-match-p
                   "containerimage\\.digest.*sha256:222digest"
                   text))
          (should (string-match-p "image_cache_inferred=hit" text))
          (should (string-match-p "image_cache_threshold=600" text))
          (should (string-match-p "image_build_seconds=123" text))
          (dolist (name '("DORG_SUITE_JOB_IMAGE_ID"
                          "DORG_SUITE_JOB_GITHUB_REPOSITORY"
                          "DORG_SUITE_JOB_GITHUB_SERVER_URL"
                          "DORG_SUITE_JOB_IMAGE_DIGEST"
                          "DORG_SUITE_JOB_IMAGE_METADATA"
                          "DORG_SUITE_JOB_IMAGE_CACHE_INFERRED"
                          "DORG_SUITE_JOB_IMAGE_CACHE_INFERENCE_THRESHOLD_SECONDS"
                          "DORG_SUITE_JOB_IMAGE_BUILD_SECONDS"))
            (should (string-match-p
                     (regexp-quote (concat "-e " name))
                     text))))))))

(ert-deftest dorg-test-job-launcher-rejects-workspace-commit-mismatch ()
  "A pull_request merge checkout cannot masquerade as the candidate SHA."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (record (expand-file-name "docker-record.txt" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (other (make-string 40 ?2))
           (process-environment
            (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "DORG_TEST_JOB_FAKE_DOCKER_RECORD="
                                  record))
                    process-environment)))
      (make-directory artifacts t)
      (ignore sha)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" other
                                  "--launcher" "actions"
                                  "--artifacts" artifacts)))
        (should (= 66 status))
        (should-not (file-exists-p record))))))

(ert-deftest dorg-test-job-image-lock-pins-runtime-provenance ()
  "The image definition is build-only and pins current CI stock versions."
  (let ((dockerfile (dorg-test-job-test--file-string dorg-test-job-test--dockerfile))
        (lock (dorg-test-job-test--file-string dorg-test-job-test--lock)))
    (should (string-match-p
             "debian:bookworm-slim@sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171"
             dockerfile))
    (should (string-match-p
             "sha256:5ae3c39ebd15e229dcedd5cee596b2497182493d41ff162e824ba13fc1b2b867"
             lock))
    (should (string-match-p "^ARG EMACS_VERSION$" dockerfile))
    (should-not (string-match-p "30\\.1" dockerfile))
    (should (string-match-p "emacs-30\\.1\\.tar\\.xz" lock))
    (should (string-match-p
             "6ccac1ae76e6af93c6de1df175e8eb406767c23da3dd2a16aa67e3124a6f138f"
             lock))
    (should (string-match-p
             "https://github.com/dolthub/dolt/releases/download/v1\\.86\\.4/\\$dolt_archive"
             dockerfile))
    (should (string-match-p
             "88bdca99086bbac7f2414af2756c962d20a8d9b6463861dc2cf35111f13b6dea"
             dockerfile))
    (dolist (package '("ca-certificates"
                       "curl"
                       "xz-utils"
                       "tar"
                       "gzip"
                       "git"
                       "build-essential"
                       "make"
                       "gcc"
                       "libc6-dev"
                       "pkg-config"
                       "texinfo"
                       "libgnutls28-dev"
                       "zlib1g-dev"
                       "libxml2-dev"
                       "libncurses-dev"))
      (should (dorg-test-job-test--contains-token-p dockerfile package))
      (should (dorg-test-job-test--contains-token-p lock package)))
    (should (string-match-p "Debian bookworm emacs Build-Depends"
                            lock))
    (should (string-match-p "non-X, non-native build path" lock))
    (should (string-match-p "--with-gnutls" dockerfile))
    (should (string-match-p "--with-zlib" dockerfile))
    (should (string-match-p "--with-xml2" dockerfile))
    (should (string-match-p "--without-native-compilation" dockerfile))
    (should-not (dorg-test-job-test--contains-token-p dockerfile
                                                       "libgccjit-dev"))
    (should-not (dorg-test-job-test--contains-token-p dockerfile "sudo"))
    (should-not (string-match-p "RUNNER_TEMP" dockerfile))
    (should (string-match-p "isNoopUpdate" dockerfile))
    (should (string-match-p "native-comp=nil" lock))
    (should-not (string-match-p "docker[ \t]+push\\|ghcr\\.io\\|docker/login-action"
                                dockerfile))))

(ert-deftest dorg-test-job-image-requires-terminal-and-process-tools ()
  "The Meld consumer's image contract names its terminal/process capability."
  (let ((dockerfile (dorg-test-job-test--file-string dorg-test-job-test--dockerfile))
        (lock (dorg-test-job-test--file-string dorg-test-job-test--lock))
        (entrypoint (dorg-test-job-test--file-string dorg-test-job-test--entrypoint)))
    ;; These controls are intentionally red before the image/probe change.
    (let ((python (string-match "apt-get install -y --no-install-recommends python3" dockerfile))
          (tools (string-match "apt-get install -y --no-install-recommends tmux procps" dockerfile))
          (copy (string-match "COPY tools/dorg-suite-job-source.py" dockerfile)))
      (should tools)
      ;; The capability layer must preserve the expensive compiler/toolchain
      ;; cache and sit immediately before the image inputs.
      (should (and python tools copy (< python tools) (< tools copy)))
      (with-temp-buffer
        (insert dockerfile)
        (should (= 1 (how-many "\\<tmux\\>" (point-min) (point-max))))
        (should (= 1 (how-many "\\<procps\\>" (point-min) (point-max)))))
      (should-not (string-match-p "ca-certificates curl xz-utils tar gzip git lsof[[:space:]]+tmux" dockerfile)))
    (should (string-match-p "Process and terminal tools" lock))
    (should (string-match-p "unpinned" lock))
    (dolist (command '("tmux -V" "ps --version" "pgrep --version"
                       "tmux_version=" "procps_version="))
      (should (string-match-p (regexp-quote command) entrypoint)))))

(ert-deftest dorg-test-job-entrypoint-writes-dolt-identity-at-runtime ()
  "Dolt identity belongs in job-private HOME at runtime, not image root HOME."
  (let ((entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint)))
    (should (string-match-p "HOME=\"\\$JOB_ROOT/home\"" entrypoint))
    (should (string-match-p
             "dolt config --global --add user\\.name \"dorg-ci\""
             entrypoint))
    (should (string-match-p
             "dolt config --global --add user\\.email \"dorg-ci@example\\.invalid\""
             entrypoint))
    (should (string-match-p "dolt config --list" entrypoint))
    (should (string-match-p "dolt config --get user\\.name" entrypoint))
    (should (string-match-p "dolt config --get user\\.email" entrypoint))
    (let ((config-list (dorg-test-job-test--real-dolt-config-list)))
      (should (string-match-p "user\\.name = dorg-ci" config-list))
      (should-not (string-match-p "user\\.name=dorg-ci" config-list)))))

(ert-deftest dorg-test-job-entrypoint-gates-test-seams ()
  "Production isolation proof input cannot be overridden by env alone."
  (let ((entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint)))
    (should (string-match-p "--test-mode" entrypoint))
    (should (string-match-p "test_mode=false" entrypoint))
    (should (string-match-p "mountinfo=\"/proc/self/mountinfo\""
                            entrypoint))
    (dolist (name '("DORG_SUITE_JOB_MOUNTINFO"
                    "DORG_SUITE_JOB_FORBIDDEN_PATHS"))
      (should (string-match-p name entrypoint)))
    (should (string-match-p
             (regexp-quote
              "[[ \"$test_mode\" == true && -n \"${DORG_SUITE_JOB_MOUNTINFO:-}\" ]]")
             entrypoint))
    (should (string-match-p
             (regexp-quote
             "[[ \"$test_mode\" == true && -n \"${DORG_SUITE_JOB_FORBIDDEN_PATHS:-}\" ]]")
             entrypoint))))

(ert-deftest dorg-test-job-entrypoint-runs-exhaustive-test-target ()
  "The container/release caller retains exhaustive test coverage.
`make test' is deliberately representative for local feedback, so the suite
job's manifest and executed command must name `make test-full' explicitly."
  (let ((entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint)))
    (should (string-match-p
             (regexp-quote "command_set=make compile;make lint;make test-full")
             entrypoint))
    (should (string-match-p "run_step test make test-full" entrypoint))))

(ert-deftest dorg-test-job-image-uses-locked-inputs-and-target-arch-dolt ()
  "The image has no hard-coded Emacs input and pins both supported Dolt arches."
  (let ((dockerfile (dorg-test-job-test--file-string dorg-test-job-test--dockerfile))
        (workflow (dorg-test-job-test--file-string
                   (expand-file-name ".github/workflows/ci.yml"
                                     dorg-test-job-test--root))))
    (should (string-match-p "ARG TARGETARCH" dockerfile))
    (should (string-match-p "case \\\"\\$TARGETARCH\\\"" dockerfile))
    (should (string-match-p "arm64) dolt_archive=dolt-linux-arm64\\.tar\\.gz" dockerfile))
    (dolist (token '("dolt-linux-amd64.tar.gz"
                     "88bdca99086bbac7f2414af2756c962d20a8d9b6463861dc2cf35111f13b6dea"
                     "dolt-linux-arm64.tar.gz"
                     "49f66b74f2cde9a33a2d1023b569d0b1a0a77571b06bd237662ca7393f4f4fa5"
                     "unsupported target architecture"))
      (should (string-match-p (regexp-quote token) dockerfile)))
    (should (string-match-p "ARG EMACS_VERSION" dockerfile))
    (should (string-match-p "ARG EMACS_SHA256" dockerfile))
    (should-not (string-match-p "ARG EMACS_VERSION=30\\.1" dockerfile))
    (should (string-match-p "Read suite job image lock" workflow))
    (should (string-match-p
             (regexp-quote "EMACS_VERSION=${{ steps.image-lock.outputs.emacs_version }}")
             workflow))
    (should (string-match-p
             (regexp-quote "EMACS_SHA256=${{ steps.image-lock.outputs.emacs_sha256 }}")
             workflow))))

(ert-deftest dorg-test-job-entrypoint-records-image-inputs-and-deadline ()
  "Runtime checks locked Emacs metadata and owns the full-gate deadline."
  (let ((entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint)))
    (dolist (token '("DORG_SUITE_JOB_IMAGE_CONFIG"
                     "DORG_SUITE_JOB_EMACS_VERSION"
                     "DORG_SUITE_JOB_EMACS_SHA256"
                     "expected_emacs_version"
                     "observed_emacs_version"
                     "suite_deadline_seconds=1700"
                     "classification=wall-time"
                     "DORG_SUITE_JOB_TEST_DEADLINE_SECONDS"
                     "suite_started_epoch"
                     "suite_ended_epoch"
                     "suite_elapsed_seconds"))
      (should (string-match-p (regexp-quote token) entrypoint)))
    (should (string-match-p
             (regexp-quote "timeout --signal=TERM --kill-after=10")
             entrypoint))
    (should-not (string-match-p "DORG_SUITE_JOB_TEST_TIMEOUT_COMMAND"
                                entrypoint))
    (should-not (string-match-p "setsid bash" entrypoint))))

(ert-deftest dorg-test-job-entrypoint-refuses-image-emacs-mismatch ()
  "A requested image version mismatch refuses before the make gates."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (config (replace-regexp-in-string
                    "DORG_SUITE_JOB_EMACS_VERSION=30\\.1"
                    "DORG_SUITE_JOB_EMACS_VERSION=30.2"
                    dorg-test-job-test--image-config-fixture))
           (result (dorg-test-job-test--run-entrypoint
                    temp sha t nil nil nil config)))
      (should (= 2 (car result)))
      (should (string-match-p "Emacs version mismatch" (cdr result)))
      (let ((receipt (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "artifacts/exit.status" temp))
                       (buffer-string)))
            (raw-log (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "artifacts/raw.log" temp))
                       (buffer-string))))
        (should (string-match-p "classification=incomplete" receipt))
        (should-not (string-match-p "== compile ==" raw-log))))))

(ert-deftest dorg-test-job-entrypoint-classifies-full-deadline-wall-time ()
  "Only test mode may shorten the full-suite deadline, which is wall-time."
  (dorg-test-job-test--with-tempdir temp
    (let ((timeout-bin (dorg-test-job-test--gnu-timeout)))
      (skip-unless timeout-bin)
      (let* ((sha (make-string 40 ?1))
             (child-pid-file (expand-file-name "make-child.pid" temp))
             (process-environment
              (append '("DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=2"
                        "DORG_TEST_JOB_FAKE_MAKE_SLEEP=5")
                      (list (concat "DORG_TEST_JOB_TIMEOUT_BIN=" timeout-bin)
                            (concat "DORG_TEST_JOB_FAKE_MAKE_CHILD_PID_FILE="
                                    child-pid-file))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint temp sha t)))
        (should (not (zerop (car result))))
        (let ((receipt (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "artifacts/exit.status" temp))
                         (buffer-string))))
          (should (string-match-p "classification=wall-time" receipt))
          (should (string-match-p "overall=124" receipt))
          (should (string-match-p "suite_elapsed_seconds="
                                  (dorg-test-job-test--file-string
                                   (expand-file-name "artifacts/manifest.txt" temp))))
          (should (file-exists-p child-pid-file))
          (sleep-for 0.1)
          (should-not (process-attributes
                       (string-to-number
                        (string-trim
                         (dorg-test-job-test--file-string child-pid-file))))))))))

(ert-deftest dorg-test-job-entrypoint-records-lint-deadline-step ()
  "A deadline during lint records lint and marks only later steps skipped."
  (dorg-test-job-test--with-tempdir temp
    (let ((timeout-bin (dorg-test-job-test--gnu-timeout)))
      (skip-unless timeout-bin)
      (let* ((sha (make-string 40 ?1))
             (process-environment
              (append '("DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=2"
                        "DORG_TEST_JOB_FAKE_MAKE_SLEEP_STEP=lint"
                        "DORG_TEST_JOB_FAKE_MAKE_SLEEP=5")
                      (list (concat "DORG_TEST_JOB_TIMEOUT_BIN=" timeout-bin))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint temp sha t))
             (receipt (dorg-test-job-test--file-string
                       (expand-file-name "artifacts/exit.status" temp))))
        (should (= 124 (car result)))
        (should (string-match-p "^compile=0$" receipt))
        (should (string-match-p "^lint=124$" receipt))
        (should (string-match-p "^test=skipped$" receipt))
        (should (string-match-p "^classification=wall-time$" receipt))))))

(ert-deftest dorg-test-job-launcher-accepts-real-wall-time-receipt ()
  "The launcher exports a complete entrypoint wall-time receipt as exit 124."
  (dorg-test-job-test--with-tempdir temp
    (let ((timeout-bin (dorg-test-job-test--gnu-timeout)))
      (skip-unless timeout-bin)
      (let* ((sha (make-string 40 ?1))
             (producer (expand-file-name "producer" temp))
             (bin (expand-file-name "bin" temp))
             (artifacts (expand-file-name "artifacts" temp))
             (tmp (expand-file-name "tmp" temp))
             (process-environment
              (append '("DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=2"
                        "DORG_TEST_JOB_FAKE_MAKE_SLEEP=5")
                      (list (concat "DORG_TEST_JOB_TIMEOUT_BIN=" timeout-bin))
                      process-environment)))
        (should (= 124 (car (dorg-test-job-test--run-entrypoint producer sha t))))
        (dorg-test-job-test--fake-bin bin t)
        (make-directory tmp t)
        (make-directory artifacts t)
        (let ((process-environment
               (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                             (concat "TMPDIR=" tmp)
                             "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                             (concat "DORG_TEST_JOB_FAKE_DOCKER_ARTIFACT_SOURCE="
                                     (expand-file-name "artifacts" producer))
                             "DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=124")
                       process-environment)))
          (should (= 124
                     (call-process dorg-test-job-test--launcher nil nil nil
                                   "--image" "fake-image" "--commit" sha
                                   "--artifacts" artifacts)))
          (let ((receipt (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
            (should (string-match-p "^test=124$" receipt))
            (should (string-match-p "^evidence_completeness=complete$" receipt))
            (should (string-match-p "^overall=124$" receipt))
            (should (string-match-p "^classification=wall-time$" receipt))))))))

(ert-deftest dorg-test-job-launcher-accepts-sigkill-wall-time-receipt ()
  "A coherent 137 wall-time receipt remains wall-time, not incomplete."
  (dorg-test-job-test--with-tempdir temp
    (let ((timeout-bin (dorg-test-job-test--gnu-timeout)))
      (skip-unless timeout-bin)
      (let* ((sha (make-string 40 ?1))
             (producer (expand-file-name "producer" temp))
             (source (expand-file-name "producer-137" temp))
             (bin (expand-file-name "bin" temp))
             (artifacts (expand-file-name "artifacts" temp))
             (tmp (expand-file-name "tmp" temp))
             (process-environment
              (append '("DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=2"
                        "DORG_TEST_JOB_FAKE_MAKE_SLEEP=5")
                      (list (concat "DORG_TEST_JOB_TIMEOUT_BIN=" timeout-bin))
                      process-environment)))
        (should (= 124 (car (dorg-test-job-test--run-entrypoint producer sha t))))
        (copy-directory (expand-file-name "artifacts" producer) source)
        (let ((receipt (expand-file-name "exit.status" source)))
          (with-temp-file receipt
            (insert
             (replace-regexp-in-string
              "^\\(test\\|overall\\)=124$" "\\1=137"
              (dorg-test-job-test--file-string receipt)))))
        (dorg-test-job-test--fake-bin bin t)
        (make-directory tmp t)
        (make-directory artifacts t)
        (let ((process-environment
               (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                             (concat "TMPDIR=" tmp)
                             "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                             (concat "DORG_TEST_JOB_FAKE_DOCKER_ARTIFACT_SOURCE=" source)
                             "DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=137")
                       process-environment)))
          (should (= 137
                     (call-process dorg-test-job-test--launcher nil nil nil
                                   "--image" "fake-image" "--commit" sha
                                   "--artifacts" artifacts)))
          (let ((receipt (dorg-test-job-test--file-string
                          (expand-file-name "exit.status" artifacts))))
            (should (string-match-p "^overall=137$" receipt))
            (should (string-match-p "^classification=wall-time$" receipt))))))))

(ert-deftest dorg-test-job-launcher-preserves-real-lint-failure ()
  "Real functional lint artifacts stay a coherent launcher failure with rc 1."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (producer (expand-file-name "producer" temp))
           (bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp)))
      (should (= 1 (car (dorg-test-job-test--run-entrypoint producer sha nil))))
      (dorg-test-job-test--fake-bin bin t)
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((process-environment
             (append (list (concat "PATH=" bin ":" (getenv "PATH"))
                           (concat "TMPDIR=" tmp)
                           "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1"
                           (concat "DORG_TEST_JOB_FAKE_DOCKER_ARTIFACT_SOURCE="
                                   (expand-file-name "artifacts" producer))
                           "DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=1")
                     process-environment)))
        (should (= 1
                   (call-process dorg-test-job-test--launcher nil nil nil
                                 "--image" "fake-image" "--commit" sha
                                 "--artifacts" artifacts)))
        (let ((receipt (dorg-test-job-test--file-string
                        (expand-file-name "exit.status" artifacts))))
          (should (string-match-p "^lint=7$" receipt))
          (should (string-match-p "^test=skipped$" receipt))
          (should (string-match-p "^evidence_completeness=complete$" receipt))
          (should (string-match-p "^overall=1$" receipt))
          (should (string-match-p "^classification=fail$" receipt)))))))

(ert-deftest dorg-test-job-entrypoint-classifies-immediate-sigkill-as-fail ()
  "A command SIGKILL before the deadline is failure, not a wall-time timeout."
  (dorg-test-job-test--with-tempdir temp
    (let ((timeout-bin (dorg-test-job-test--gnu-timeout)))
      (skip-unless timeout-bin)
      (let* ((sha (make-string 40 ?1))
             (process-environment
              (append '("DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=2"
                        "DORG_TEST_JOB_FAKE_MAKE_SIGNAL=KILL")
                      (list (concat "DORG_TEST_JOB_TIMEOUT_BIN=" timeout-bin))
                      process-environment))
             (result (dorg-test-job-test--run-entrypoint temp sha t)))
        (should (= 1 (car result)))
        (let ((receipt (dorg-test-job-test--file-string
                        (expand-file-name "artifacts/exit.status" temp))))
          (should (string-match-p "overall=1" receipt))
          (should (string-match-p "classification=fail" receipt)))))))

(ert-deftest dorg-test-job-entrypoint-refuses-gating-deadline-override ()
  "A normal gating invocation cannot shorten or extend its 1700-second cap."
  (dorg-test-job-test--with-tempdir temp
    (let* ((sha (make-string 40 ?1))
           (artifacts (expand-file-name "artifacts" temp))
           (process-environment
            (cons "DORG_SUITE_JOB_TEST_DEADLINE_SECONDS=1" process-environment))
           (out (generate-new-buffer " *dorg-suite-job-deadline-refusal*")))
      (unwind-protect
          (with-temp-buffer
            (insert "ignored")
            (should (= 2 (call-process-region
                          (point-min) (point-max)
                          dorg-test-job-test--entrypoint nil out nil
                          "run" "--source-bundle" "-" "--commit" sha
                          "--artifacts" artifacts)))
            (should (string-match-p "suite deadline override requires --test-mode"
                                    (with-current-buffer out (buffer-string)))))
        (kill-buffer out)))))

(ert-deftest dorg-test-job-workflow-trunk-push-starts-no-run ()
  "A push to the private trunk starts no CI run; other triggers stay.
A trunk landing is certified by a dispatched gate run on the same tree before
the push, so a push-triggered rerun only spends rationed Actions minutes."
  (let ((workflow (dorg-test-job-test--file-string
                   (expand-file-name ".github/workflows/ci.yml"
                                     dorg-test-job-test--root))))
    (should (string-match-p "^  push:\n    branches: \\[main\\]$" workflow))
    (should (string-match-p "^  pull_request:\n    branches: \\[main, trunk\\]$"
                            workflow))
    (should (string-match-p "^  workflow_dispatch:$" workflow))))

(ert-deftest dorg-test-job-workflow-migrates-ci-caller-only ()
  "The CI test job invokes the common job and deletes host gate plumbing."
  (let ((workflow (dorg-test-job-test--file-string
                   (expand-file-name ".github/workflows/ci.yml"
                                     dorg-test-job-test--root)))
        (launcher (dorg-test-job-test--file-string
                   dorg-test-job-test--launcher)))
    (should (string-match-p "workflow_dispatch:" workflow))
    (should (string-match-p "raw_server_evidence_repeat:" workflow))
    (should-not (string-match-p "30\\.1" workflow))
    (should (string-match-p "timeout-minutes: 45" workflow))
    (should (string-match-p "fetch-depth: 0" workflow))
    (should (string-match-p
             (regexp-quote "ref: ${{ github.event.pull_request.head.sha || github.sha }}")
             workflow))
    (should (string-match-p "cache-from: type=gha" workflow))
    (should (string-match-p "cache-to: type=gha,mode=max" workflow))
    (should (string-match-p "id: build-suite-job-image" workflow))
    (should (string-match-p "id: image-build-start" workflow))
    (should (string-match-p "id: image-build-finish" workflow))
    (should-not (string-match-p
                 (regexp-quote "DORG_SUITE_JOB_ARTIFACTS: ${{ runner.temp }}")
                 workflow))
    (should (string-match-p
             "artifacts=\"\\$RUNNER_TEMP/dorg-suite-job-artifacts\""
             workflow))
    (should (string-match-p
             "DORG_SUITE_JOB_ARTIFACTS=\\$artifacts"
             workflow))
    (should (string-match-p
             (regexp-quote "path: ${{ env.DORG_SUITE_JOB_ARTIFACTS }}")
             workflow))
    (should (string-match-p "if: always()" workflow))
    (should (string-match-p
             "mkdir -p \"\\$artifacts\""
             workflow))
    (should (string-match-p "classification=incomplete" workflow))
    (should (string-match-p "stage=before-image-build" workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_COMMIT: ${{ github.event.pull_request.head.sha || github.sha }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_EVENT_NAME: ${{ github.event_name }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_ID: ${{ steps.build-suite-job-image.outputs.imageid }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_DIGEST: ${{ steps.build-suite-job-image.outputs.digest }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_METADATA: ${{ steps.build-suite-job-image.outputs.metadata || steps.build-suite-job-image.outputs['metadata-json'] }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_CACHE_INFERRED: ${{ steps.image-build-finish.outputs.cache-inferred }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_BUILD_SECONDS: ${{ steps.image-build-finish.outputs.seconds }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_IMAGE_CACHE_INFERENCE_THRESHOLD_SECONDS: ${{ steps.image-build-finish.outputs.cache-threshold-seconds }}")
             workflow))
    (should (string-match-p "cache_threshold_seconds=600" workflow))
    (should (string-match-p "seconds=\\$seconds" workflow))
    (should (string-match-p "cache-inferred=\\$cache_inferred" workflow))
    (should (string-match-p
             "cache-threshold-seconds=\\$cache_threshold_seconds"
             workflow))
    (should (string-match-p "--commit \"\\$DORG_SUITE_JOB_COMMIT\""
                            workflow))
    (should (string-match-p "--launcher actions" workflow))
    (should (string-match-p "tools/dorg-suite-job-local" workflow))
    (should (string-match-p "DORG_SUITE_JOB_RAW_SERVER_EVIDENCE_REPEAT" workflow))
    (should (string-match-p "--raw-server-evidence-repeat" workflow))
    (should (string-match-p "git bundle create - HEAD" launcher))
    (should (string-match-p "--commit \"\\$commit\"" launcher))
    (should (string-match-p "DORG_SUITE_JOB_LAUNCHER=\"\\$launcher\""
                            launcher))
    (should (string-match-p "raw_server_evidence_repeat" launcher))
    (should (string-match-p
             (regexp-quote "requires the exact raw-server selector") launcher))
    (should-not (string-match-p "DORG_SUITE_JOB_LAUNCHER=local" launcher))
    (should (string-match-p "--user.*id -u.*id -g" launcher))
    (should-not (string-match-p "purcell/setup-emacs" workflow))
    (should-not (string-match-p "name: Install Dolt 1\\.86\\.4" workflow))
    (should-not (string-match-p "run: make compile\\|run: make lint\\|run: make test\\(?:[-[:space:]]\\|$\\)"
                                workflow))))

(ert-deftest dorg-test-job-workflow-and-entrypoint-pin-trusted-venues ()
  "The workflow admits only private CI and the public main push."
  (let ((workflow (dorg-test-job-test--file-string
                   (expand-file-name ".github/workflows/ci.yml"
                                     dorg-test-job-test--root)))
        (launcher (dorg-test-job-test--file-string dorg-test-job-test--launcher))
        (entrypoint (dorg-test-job-test--file-string
                     dorg-test-job-test--entrypoint))
        (venue (dorg-test-job-test--file-string
                dorg-test-job-test--private-venue)))
    (should (string-match-p
             (regexp-quote
              "if: ${{ (github.repository == 'cosmicz/DOrg.oc' && github.server_url == 'https://github.com') || (github.repository == 'cosmicz/dorg' && github.server_url == 'https://github.com' && github.event_name == 'push' && github.ref == 'refs/heads/main') }}")
             workflow))
    (should (string-match-p
             (regexp-quote "# Public release: main pushes only; PRs, forks and dispatches refuse; no secrets, and artifacts are public.")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_GITHUB_REPOSITORY: ${{ github.repository }}")
             workflow))
    (should (string-match-p
             (regexp-quote "DORG_SUITE_JOB_GITHUB_SERVER_URL: ${{ github.server_url }}")
             workflow))
    (dolist (name '("DORG_SUITE_JOB_GITHUB_REPOSITORY"
                    "DORG_SUITE_JOB_GITHUB_SERVER_URL"))
      (should (string-match-p (concat "-e " name) launcher))
      (should (string-match-p name entrypoint)))
    (should (string-match-p
             (regexp-quote "PRIVATE_REPO = \"cosmicz/DOrg.oc\"") venue))))

;;;; Verdict facts: pass is derived, never a fallthrough

(defun dorg-test-job-test--launch-with (overrides)
  "Run the shipped launcher through the fake harness with OVERRIDES.
OVERRIDES is a list of NAME=VALUE strings prepended to the environment on
top of the complete-success configuration.  Return (STATUS . EXIT-TEXT),
where EXIT-TEXT is the exported exit.status or \"\" when none was written."
  (dorg-test-job-test--with-tempdir temp
    (let* ((bin (expand-file-name "bin" temp))
           (artifacts (expand-file-name "artifacts" temp))
           (tmp (expand-file-name "tmp" temp))
           (sha (dorg-test-job-test--fake-bin bin t))
           (process-environment
            (append overrides
                    (list (concat "PATH=" bin ":" (getenv "PATH"))
                          (concat "TMPDIR=" tmp)
                          "DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=1")
                    process-environment)))
      (make-directory tmp t)
      (make-directory artifacts t)
      (let ((status (call-process dorg-test-job-test--launcher nil nil nil
                                  "--image" "fake-image"
                                  "--commit" sha
                                  "--artifacts" artifacts))
            (receipt (expand-file-name "exit.status" artifacts)))
        ;; A cell may leave a read-only exported directory behind on
        ;; purpose; make the tree removable again so the tempdir cleanup
        ;; is not itself blocked by the condition under test.
        (call-process "chmod" nil nil nil "-R" "u+w" artifacts)
        (cons status (if (file-exists-p receipt)
                         (dorg-test-job-test--file-string receipt)
                       ""))))))

(ert-deftest dorg-test-job-launcher-refuses-unknown-classification-without-summary ()
  "A receipt whose classification is not a classification cannot pass.
The container wrote overall=0 with classification=unknown and no ERT
summary.  Through the shipped launcher the verdict is incomplete, the raw
token is retained, and both gaps are named."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=unknown"
                    "DORG_TEST_JOB_FAKE_PARTIAL_ARTIFACTS=1")))
         (exit-text (cdr result)))
    (should (= 2 (car result)))
    (should (string-match-p "^receipt_overall=0$" exit-text))
    (should (string-match-p "^receipt_classification=unknown$" exit-text))
    (should (string-match-p "exit.status:classification" exit-text))
    (should (string-match-p "ert-summary.txt" exit-text))
    (should (string-match-p "^evidence_completeness=incomplete$" exit-text))
    (should (string-match-p "^overall=2$" exit-text))
    (should (string-match-p "^classification=incomplete$" exit-text))))

(ert-deftest dorg-test-job-launcher-refuses-fail-classification-with-zero-overall ()
  "A receipt that says fail beside overall=0 is contradictory, not a pass."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=fail")))
         (exit-text (cdr result)))
    (should (= 2 (car result)))
    (should (string-match-p "^receipt_overall=0$" exit-text))
    (should (string-match-p "^receipt_classification=fail$" exit-text))
    (should (string-match-p "exit.status:classification" exit-text))
    (should (string-match-p "^overall=2$" exit-text))
    (should (string-match-p "^classification=incomplete$" exit-text))))

(ert-deftest dorg-test-job-launcher-refuses-summary-contradicting-test-exit ()
  "A well-formed summary reporting an unexpected result beside test=0 is
contradictory evidence and cannot pass, even though every other fact is
at its success value."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_ERT_SUMMARY=Ran 1 tests, 0 results as expected, 1 unexpected")))
         (exit-text (cdr result)))
    (should (= 2 (car result)))
    (should (string-match-p "^test=0$" exit-text))
    (should (string-match-p "^receipt_classification=pass$" exit-text))
    (should (string-match-p "ert-summary.txt:unexpected" exit-text))
    (should (string-match-p "^overall=2$" exit-text))
    (should (string-match-p "^classification=incomplete$" exit-text))))

(ert-deftest dorg-test-job-launcher-coherent-failure-is-fail-not-incomplete ()
  "A receipt whose fail claim agrees with its steps and summary is a
fail with the receipt's own status, not an incomplete."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=1"
                    "DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=fail"
                    "DORG_TEST_JOB_FAKE_TEST_STATUS=1"
                    "DORG_TEST_JOB_FAKE_ERT_SUMMARY=Ran 1 tests, 0 results as expected, 1 unexpected")))
         (exit-text (cdr result)))
    (should (= 1 (car result)))
    (should (string-match-p "^evidence_completeness=complete$" exit-text))
    (should (string-match-p "receipt_overall=1" exit-text))
    (should (string-match-p "^overall=1$" exit-text))
    (should (string-match-p "^classification=fail$" exit-text))))

(ert-deftest dorg-test-job-launcher-refuses-absent-classification ()
  "A receipt with no classification line is missing a required fact.
Nothing may be inferred from overall=0: the verdict is incomplete, the
fact is named, and the exported field honestly reads empty."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_OMIT_CLASSIFICATION=1")))
         (exit-text (cdr result)))
    (should (= 2 (car result)))
    (should (string-match-p "^receipt_overall=0$" exit-text))
    (should (string-match-p "^receipt_classification=$" exit-text))
    (should (string-match-p "^container_classification=$" exit-text))
    (should (string-match-p "exit.status:classification" exit-text))
    (should (string-match-p "^evidence_completeness=incomplete$" exit-text))
    (should (string-match-p "^overall=2$" exit-text))
    (should (string-match-p "^classification=incomplete$" exit-text))))

(ert-deftest dorg-test-job-launcher-refuses-empty-classification ()
  "A classification line with an empty value is the same missing fact."
  (let* ((result (dorg-test-job-test--launch-with
                  '("DORG_TEST_JOB_FAKE_EMPTY_CLASSIFICATION=1")))
         (exit-text (cdr result)))
    (should (= 2 (car result)))
    (should (string-match-p "^receipt_classification=$" exit-text))
    (should (string-match-p "exit.status:classification" exit-text))
    (should (string-match-p "^overall=2$" exit-text))
    (should (string-match-p "^classification=incomplete$" exit-text))))

(defconst dorg-test-job-test--one-fact-off-cells
  '(("bundle exits 9"
     ("DORG_TEST_JOB_FAKE_GIT_BUNDLE_EXIT=9") "fail")
    ("runtime exits 137"
     ("DORG_TEST_JOB_FAKE_DOCKER_RUN_EXIT=137") "fail")
    ("no artifact written"
     ("DORG_TEST_JOB_FAKE_DOCKER_WRITE_ARTIFACTS=") "incomplete")
    ("summary absent"
     ("DORG_TEST_JOB_FAKE_PARTIAL_ARTIFACTS=1") "incomplete")
    ("summary malformed"
     ("DORG_TEST_JOB_FAKE_ERT_SUMMARY=ERT summary not found; see raw.log")
     "incomplete")
    ("summary disagrees with test exit"
     ("DORG_TEST_JOB_FAKE_ERT_SUMMARY=Ran 1 tests, 0 results as expected, 1 unexpected")
     "incomplete")
    ("test step failed under a pass claim"
     ("DORG_TEST_JOB_FAKE_TEST_STATUS=1") "incomplete")
    ("classification fail with overall 0"
     ("DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=fail") "incomplete")
    ("classification unknown"
     ("DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=unknown") "incomplete")
    ("classification line absent"
     ("DORG_TEST_JOB_FAKE_OMIT_CLASSIFICATION=1") "incomplete")
    ("classification value empty"
     ("DORG_TEST_JOB_FAKE_EMPTY_CLASSIFICATION=1") "incomplete")
    ("classification incomplete from the container"
     ("DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=2"
      "DORG_TEST_JOB_FAKE_CONTAINER_CLASSIFICATION=incomplete")
     "incomplete")
    ("overall not a number"
     ("DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=not-a-number") "incomplete")
    ("overall 1 with a pass claim"
     ("DORG_TEST_JOB_FAKE_CONTAINER_OVERALL=1") "incomplete")
    ("job temp residue left on the host"
     ("DORG_TEST_JOB_FAKE_JOB_TEMP_AFTER=1") "fail")
    ("exported directory not removable"
     ("DORG_TEST_JOB_FAKE_READONLY_ARTIFACT_DIR=1") "fail"))
  "One control per verdict fact the fake harness can drive.
Each cell moves exactly one fact off its success value from the
complete-success configuration and names the class the verdict must take.
This is a handwritten list, useful coverage over the drivable facts; it is
not a generated proof over every fact or their combinations.")

(ert-deftest dorg-test-job-launcher-one-fact-off-never-passes ()
  "Every single-fact departure from complete success is non-pass, is
exported with the class the fact belongs to, and names at least one
unsatisfied fact; the complete control itself passes.  Each row is one
off-diagonal cell of the verdict's truth table over the facts the fake
harness can drive."
  (let* ((control (dorg-test-job-test--launch-with nil))
         (control-text (cdr control)))
    (ert-info ("complete-success control")
      (should (zerop (car control)))
      (should (string-match-p "^classification=pass$" control-text))
      (should (string-match-p "^verdict_facts=$" control-text))))
  (dolist (cell dorg-test-job-test--one-fact-off-cells)
    (pcase-let ((`(,label ,overrides ,class) cell))
      (ert-info ((format "cell: %s" label))
        (let* ((result (dorg-test-job-test--launch-with overrides))
               (exit-text (cdr result)))
          (should-not (zerop (car result)))
          (should-not (string-match-p "^classification=pass$" exit-text))
          (should-not (string-match-p "^overall=0$" exit-text))
          (should (string-match-p (format "^classification=%s$" class)
                                  exit-text))
          (should (string-match-p "^verdict_facts=.+$" exit-text)))))))

(provide 'dorg-test-job-tests)
;;; dorg-test-job-tests.el ends here
