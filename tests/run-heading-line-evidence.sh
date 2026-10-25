#!/usr/bin/env bash
# Focused, server-free heading-line evidence runner.
#
# This runner is deliberately reviewed before a reservation.  It never calls
# `make test SELECT=...': that Make entry starts the shared Dolt server.  The
# ERT process loads the normal DOrg runner, sets up the normal test isolation,
# and invokes ERT directly for only the named plain-Org controls below.
set -euo pipefail

root=$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
cd "$root"

mode=${1:-census}
candidate=${DORG_1QDTB_CANDIDATE:-}
reservation=${DORG_1QDTB_RESERVATION:-}
receipt_dir=${DORG_1QDTB_RECEIPT_DIR:-}
expires=${DORG_1QDTB_EXPIRES_EPOCH:-}

green_selector='\\(?:^dorg-test-1qdtb-\\|^dorg-test-mutate-exec-update-heading-post-blank-refuses$\\|^dorg-test-idp-unsupported-post-blank-variant-exact-error$\\|^dorg-test-idp-unsupported-update-heading-post-blank$\\|^dorg-test-c8ynt-c3-heading-line-cells-are-supported$\\|^dorg-test-c8ynt-commented-plain-org-round-trip-and-refusal$\\|^dorg-test-api-extern-key-heading-apply-plain-org$\\)'
positive_selector='^dorg-test-1qdtb-\\(?:title\\|todo\\|priority\\|tags\\|sequential-five-fields\\|mixed-typed-field-order-and-dorg-identity\\)'
preflight_selector='^dorg-test-1qdtb-unrepresentable-fields-refuse-before-apply$'
green_expected=42
positive_expected=21
emacs_runner=(emacs --batch -Q -L . -L ./tests -L ./lib/mysql -L ./lib/clime
              -l ./tests/dorg-tests-runner.el)

fail() {
    printf '1qdtb-runner: %s\n' "$*" >&2
    exit 64
}

clean_tree() {
    test -z "$(git status --porcelain)" || fail 'worktree is not clean'
}

runner_argv() {
    printf '%q ' "${emacs_runner[@]}"
}

runner_processes() {
    pgrep -lf '[e]macs.*dorg-tests-runner\.el|[t]ools/test-scratch.*run dorg' || true
}

parse_expression() {
    local label=$1 expression=$2
    if DORG_1QDTB_EXPRESSION="$expression" emacs --batch -Q --eval '
        (let ((expression (or (getenv "DORG_1QDTB_EXPRESSION") "")))
          (condition-case err
              (let* ((read-result (read-from-string expression))
                     (tail (substring expression (cdr read-result))))
                (when (string-match-p "[^[:space:]]" tail)
                  (error "trailing expression input"))
                (kill-emacs 0))
            (error
             (princ (format "EXPR-PARSE-ERROR %S\\n" err))
             (kill-emacs 64))))'; then
        printf 'EXPR-PARSE mode=%s ok=t\n' "$label"
    else
        printf 'EXPR-PARSE mode=%s ok=nil\n' "$label" >&2
        return 64
    fi
}

no_run_census() {
    clean_tree
    printf 'MODE=census\n'
    printf 'HEAD=%s\n' "$(git rev-parse HEAD)"
    printf 'ARGV='
    runner_argv
    printf '\n'
    local runners
    runners=$(runner_processes)
    if [ -n "$runners" ]; then
        printf 'RUNNER_CENSUS=present\n%s\n' "$runners"
    else
        printf 'RUNNER_CENSUS=none\n'
    fi
    local expression_mode
    for expression_mode in green mutant-noop mutant-preflight; do
        DORG_1QDTB_PARSE_ONLY=1 "$0" "$expression_mode"
    done
    env -u EMBEAD_AGENT -u EMBEAD_ACTOR -u EMBEAD_DIR -u MELD_AGENT \
        -u MELD_SESSION -u CLIME_AGENT -u DORG_BENCH -u EMACSLOADPATH \
        "${emacs_runner[@]}" \
        --eval "(let ((valid t)
                       (specs (list (list \"green\" \"${green_selector}\" ${green_expected})
                                    (list \"positive\" \"${positive_selector}\" ${positive_expected})
                                    (list \"preflight\" \"${preflight_selector}\" 1))))
                  (dolist (spec specs)
                    (let* ((label (nth 0 spec))
                           (tests (ert-select-tests (nth 1 spec) t))
                           (expected (nth 2 spec))
                           (names (sort (mapcar (lambda (test)
                                                  (symbol-name (ert-test-name test)))
                                                tests)
                                        #'string<)))
                      (princ (format \"SELECTOR=%s SELECTED=%d EXPECTED=%d\\n\"
                                     label (length tests) expected))
                      (dolist (name names)
                        (princ (format \"SELECTED-NAME[%s]=%s\\n\" label name)))
                      (unless (= (length tests) expected) (setq valid nil))))
                  (ert-deftest heading-runner-regex-equal ()
                    (should (equal \"after\" \"before\")))
                  (ert-deftest heading-runner-regex-count ()
                    (let ((apply-count 1)) (should (= 0 apply-count))))
                  (let* ((equal-test (ert-get-test 'heading-runner-regex-equal))
                         (count-test (ert-get-test 'heading-runner-regex-count))
                         (_ (ert-run-test equal-test))
                         (equal-text
                          (prin1-to-string
                           (ert-test-result-with-condition-condition
                            (ert-test-most-recent-result equal-test))))
                         (_ (ert-run-test count-test))
                         (count-text
                          (prin1-to-string
                           (ert-test-result-with-condition-condition
                            (ert-test-most-recent-result count-test))))
                         (equal-intended
                          (and (string-match-p \"(ert-test-failed ((should (equal \" equal-text) t))
                         (equal-wrong (and (string-match-p \"apply-count\" equal-text) t))
                         (count-intended
                          (and (string-match-p
                                \"(ert-test-failed ((should (= 0 apply-count))\" count-text) t))
                         (count-wrong (and (string-match-p \"(equal \" count-text) t)))
                    (princ (format \"REGEX-PROBE equal-intended=%S equal-wrong=%S count-intended=%S count-wrong=%S\\n\"
                                   equal-intended equal-wrong count-intended count-wrong))
                    (unless (and equal-intended (not equal-wrong)
                                 count-intended (not count-wrong))
                      (setq valid nil)))
                  (kill-emacs (if valid 0 64)))"
}

require_grant() {
    test -n "$reservation" || fail 'DORG_1QDTB_RESERVATION is required'
    test -n "$candidate" || fail 'DORG_1QDTB_CANDIDATE is required'
    test -n "$receipt_dir" || fail 'DORG_1QDTB_RECEIPT_DIR is required'
    [[ $expires =~ ^[0-9]+$ ]] || fail 'DORG_1QDTB_EXPIRES_EPOCH is required'
    test "$(git rev-parse HEAD)" = "$candidate" || fail 'HEAD does not match candidate'
    clean_tree
    mkdir -p -- "$receipt_dir"
}

admit() {
    local log=$1 now margin load_raw load1 runners failure
    now=$(date +%s)
    margin=$((expires - now))
    load_raw=$(/usr/sbin/sysctl -n vm.loadavg)
    load1=$(printf '%s\n' "$load_raw" | awk '{ gsub(/[{}]/, ""); print $1 }')
    runners=$(runner_processes)
    failure=
    if [ "$margin" -lt 190 ]; then
        failure=expiry-margin
    elif ! awk -v value="$load1" 'BEGIN { exit !(value < 7) }'; then
        failure=load1
    elif [ -n "$runners" ]; then
        failure=competing-runner
    fi
    printf 'ADMISSION now=%s expiry=%s margin=%s loadavg=%s load1=%s runners=%s pass-expiry=%s pass-load=%s pass-runners=%s first-failure=%s\n' \
        "$now" "$expires" "$margin" "$load_raw" "$load1" \
        "${runners:+present}" \
        "$([ "$margin" -ge 190 ] && printf t || printf nil)" \
        "$(awk -v value="$load1" 'BEGIN { print (value < 7 ? "t" : "nil") }')" \
        "$([ -z "$runners" ] && printf t || printf nil)" \
        "${failure:-none}" >>"$log"
    if [ -n "$runners" ]; then
        printf 'ADMISSION-RUNNERS-BEGIN\n%s\nADMISSION-RUNNERS-END\n' "$runners" >>"$log"
    fi
    test -z "$failure"
}

run_ert() {
    local selector=$1 expression=$2 log=$3 rc
    if [ "${DORG_1QDTB_PARSE_ONLY:-}" = 1 ]; then
        parse_expression "$mode" "$expression"
        RUN_RC=$?
        return
    fi
    local -a argv=(
        env -u EMBEAD_AGENT -u EMBEAD_ACTOR -u EMBEAD_DIR -u MELD_AGENT
        -u MELD_SESSION -u CLIME_AGENT -u DORG_BENCH -u EMACSLOADPATH
        ./tools/test-scratch run dorg --
        timeout -k 10 180 "${emacs_runner[@]}" --eval "$expression")
    : >"$log"
    if ! admit "$log"; then
        printf 'RC=64\n' >>"$log"
        RUN_RC=64
        return
    fi
    printf 'RESERVATION=%s\nHEAD=%s\nSELECTOR=%s\nARGV=' \
        "$reservation" "$(git rev-parse HEAD)" "$selector" >>"$log"
    printf '%q ' "${argv[@]}" >>"$log"
    printf '\nSTART=%s\n' "$(date -Iseconds)" >>"$log"
    set +e
    "${argv[@]}" >>"$log" 2>&1
    rc=$?
    set -e
    printf 'END=%s\nRC=%d\n' "$(date -Iseconds)" "$rc" >>"$log"
    RUN_RC=$rc
}

case "$mode" in
    census)
        no_run_census
        ;;
    green)
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" != 1 ]; then
            require_grant
        fi
        run_ert "$green_selector" \
            "(progn
                (dorg-setup-test-environment)
                (let ((stats (ert-run-tests-batch \"${green_selector}\")))
                  (princ (format \"GREEN-STATS total=%d expected=%d unexpected=%d skipped=%d\\n\"
                                 (ert-stats-total stats)
                                 (ert-stats-completed-expected stats)
                                 (ert-stats-completed-unexpected stats)
                                 (ert-stats-skipped stats)))
                  (kill-emacs
                   (if (and (= (ert-stats-total stats) ${green_expected})
                            (= (ert-stats-completed-expected stats) ${green_expected})
                            (zerop (ert-stats-completed-unexpected stats))
                            (zerop (ert-stats-skipped stats)))
                       0 1))))" \
            "$receipt_dir/green.log"
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" = 1 ]; then
            exit "$RUN_RC"
        fi
        test "$RUN_RC" -eq 0 || exit "$RUN_RC"
        ;;
    mutant-noop)
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" != 1 ]; then
            require_grant
        fi
        run_ert "$positive_selector" \
            "(progn
                (dorg-setup-test-environment)
                (let ((replacement (lambda (&rest _) nil)))
                  (cl-letf (((symbol-function 'dorg-mutate--source-preserving-update-heading-line)
                             replacement))
                    (princ (format \"MUTANT-INSTALLED symbol=%S installed=%S\\n\"
                                   'dorg-mutate--source-preserving-update-heading-line
                                   (eq (symbol-function 'dorg-mutate--source-preserving-update-heading-line)
                                       replacement)))
                    (let* ((tests (ert-select-tests \"${positive_selector}\" t))
                           (stats (ert-run-tests-batch \"${positive_selector}\"))
                           (valid
                            (and (= (length tests) ${positive_expected})
                                 (= (ert-stats-total stats) ${positive_expected})
                                 (zerop (ert-stats-completed-expected stats))
                                 (= (ert-stats-completed-unexpected stats) ${positive_expected})
                                 (zerop (ert-stats-skipped stats))
                                 (cl-every
                                  (lambda (test)
                                    (let ((result (ert-test-most-recent-result test)))
                                      (let ((condition
                                             (ert-test-result-with-condition-condition result)))
                                        (princ (format \"MUTANT-FAIL test=%S condition=%S\\n\"
                                                       (ert-test-name test) condition))
                                        (and (ert-test-failed-p result)
                                             (eq (car condition) 'ert-test-failed)
                                             (string-match-p
                                              \"(ert-test-failed ((should (equal \"
                                              (prin1-to-string condition))))))
                                  tests))))
                      (princ (format \"MUTANT-VALIDATION kind=noop valid=%S total=%d expected=%d unexpected=%d skipped=%d\\n\"
                                     valid (ert-stats-total stats)
                                     (ert-stats-completed-expected stats)
                                     (ert-stats-completed-unexpected stats)
                                     (ert-stats-skipped stats)))
                      (kill-emacs (if valid 1 2))))))" \
            "$receipt_dir/mutant-noop.log"
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" = 1 ]; then
            exit "$RUN_RC"
        fi
        test "$RUN_RC" -eq 1 || fail "no-op mutant was invalid (rc=$RUN_RC)"
        ;;
    mutant-preflight)
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" != 1 ]; then
            require_grant
        fi
        run_ert "$preflight_selector" \
            "(progn
                (dorg-setup-test-environment)
                (let ((replacement (lambda (line _fields _grammar) line)))
                  (cl-letf (((symbol-function 'dorg-mutate--source-preserving-heading-line-simulate)
                             replacement))
                    (princ (format \"MUTANT-INSTALLED symbol=%S installed=%S\\n\"
                                   'dorg-mutate--source-preserving-heading-line-simulate
                                   (eq (symbol-function 'dorg-mutate--source-preserving-heading-line-simulate)
                                       replacement)))
                    (let* ((stats (ert-run-tests-batch \"${preflight_selector}\"))
                           (test (car (ert-select-tests \"${preflight_selector}\" t)))
                           (result (ert-test-most-recent-result test))
                           (valid
                            (and (= (ert-stats-total stats) 1)
                                 (zerop (ert-stats-completed-expected stats))
                                 (= (ert-stats-completed-unexpected stats) 1)
                                 (zerop (ert-stats-skipped stats))
                                 (ert-test-failed-p result)
                                 (let ((condition
                                        (ert-test-result-with-condition-condition result)))
                                   (princ (format \"MUTANT-FAIL test=%S condition=%S\\n\"
                                                  (ert-test-name test) condition))
                                   (and (eq (car condition) 'ert-test-failed)
                                        (string-match-p
                                         \"(ert-test-failed ((should (= 0 apply-count))\"
                                         (prin1-to-string condition)))))))
                      (princ (format \"MUTANT-VALIDATION kind=preflight valid=%S total=%d expected=%d unexpected=%d skipped=%d\\n\"
                                     valid (ert-stats-total stats)
                                     (ert-stats-completed-expected stats)
                                     (ert-stats-completed-unexpected stats)
                                     (ert-stats-skipped stats)))
                      (kill-emacs (if valid 1 2))))))" \
            "$receipt_dir/mutant-preflight.log"
        if [ "${DORG_1QDTB_PARSE_ONLY:-}" = 1 ]; then
            exit "$RUN_RC"
        fi
        test "$RUN_RC" -eq 1 || fail "preflight mutant was invalid (rc=$RUN_RC)"
        ;;
    *)
        fail "unknown mode: $mode"
        ;;
esac
