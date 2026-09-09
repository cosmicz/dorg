#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TRACKER_CLI="${TRACKER_CLI:?set TRACKER_CLI to the downstream tracker executable}"
TRACKER_STATE_DIR="${TRACKER_STATE_DIR:?set TRACKER_STATE_DIR to its project-relative state directory}"
TRACKER_FILE="${TRACKER_FILE:-$TRACKER_STATE_DIR/beads.org}"
TRACKER_DOLT_DIR="${TRACKER_DOLT_DIR:-$TRACKER_STATE_DIR/dolt}"
DORG="${DORG:-dorg}"
DOLT="${DOLT:-dolt}"
EMACS="${EMACS:-emacs}"

need() {
  command -v "$1" >/dev/null 2>&1 || {
    printf 'semantic-roundtrip: missing required tool: %s\n' "$1" >&2
    exit 2
  }
}

need "$TRACKER_CLI"
need "$DORG"
need "$DOLT"
need "$EMACS"

tmp="$(mktemp -d "${TMPDIR:-/tmp}/dorg-semantic-roundtrip.XXXXXX")"
cleanup() {
  rm -rf "$tmp"
}
trap cleanup EXIT

fixture="$ROOT/tests/fixtures/semantic-roundtrip/tracker-like.org"
project="$tmp/project"
mkdir -p "$project"

(
  cd "$project"
  env -u EMBEAD_ON -u EMBEAD_AGENT -u EMBEAD_DIR -u EMBEAD_FILE "$TRACKER_CLI" init >/dev/null
  cp "$fixture" "$TRACKER_FILE"
  env -u EMBEAD_ON -u EMBEAD_AGENT -u EMBEAD_DIR -u EMBEAD_FILE "$TRACKER_CLI" dolt bind --dolt-dir "$TRACKER_DOLT_DIR" >/dev/null
  env -u EMBEAD_ON -u EMBEAD_AGENT -u EMBEAD_DIR -u EMBEAD_FILE "$TRACKER_CLI" list --all >/dev/null
)

"$EMACS" -Q --batch \
  -L "$ROOT" \
  -L "$ROOT/tests" \
  -l "$ROOT/tests/dorg-roundtrip-corpus-tests.el" \
  --eval "(dorg-test-roundtrip-corpus-batch-assert-files \"$fixture\" \"$project/$TRACKER_FILE\")"

printf 'semantic-roundtrip: PASS downstream tracker scratch project\n'
