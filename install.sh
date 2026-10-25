#!/usr/bin/env bash
# DOrg installer — fetch DOrg, build it, and put the `dorg' CLI on your PATH.
#
# See README.org for requirements and CHANGELOG.org for release and upgrade notes.
# This installs the runtime; it does not migrate existing stores.
#
# Quick start:
#   curl -fsSL https://raw.githubusercontent.com/cosmicz/dorg/main/install.sh | bash
#
# Or from a local checkout:
#   bash install.sh
#
# Environment overrides:
#   DORG_REPO   git URL to clone (set this to the DOrg repository URL)
#   DORG_REF    branch/tag/commit to check out (default: the repo default)
#   DORG_DIR    where to clone the DOrg SOURCE checkout (default: $HOME/.local/src/dorg)
#   PREFIX      installation prefix (default: $HOME/.local); the runtime goes to
#               $PREFIX/share/dorg
#   BINDIR      where the `dorg' launcher link goes (default: $HOME/bin)
#
# What it does: clones (or updates) DOrg — or uses an existing checkout —
# then runs `make install', which copies the runtime into $PREFIX/share/dorg,
# byte-compiles it there, verifies the staged launcher (`dorg --help' and the
# `dorg doctor --install' environment check: mysql.el hard dependency plus
# the optional Dolt binary) BEFORE publishing, and links $BINDIR/dorg to it.
# The installed command does not depend on the source checkout afterwards.
set -euo pipefail

DORG_REPO="${DORG_REPO:-https://github.com/cosmicz/dorg.git}"
DORG_REF="${DORG_REF:-}"
DORG_DIR="${DORG_DIR:-$HOME/.local/src/dorg}"
PREFIX="${PREFIX:-$HOME/.local}"
BINDIR="${BINDIR:-$HOME/bin}"

err() { printf '%s\n' "install: $*" >&2; }
need() {
	command -v "$1" >/dev/null 2>&1 || { err "required command '$1' not found on PATH"; exit 1; }
}

need git
need make
need emacs

# 1. Fetch DOrg — clone fresh, or update an existing checkout in place.
if [ -d "$DORG_DIR/.git" ]; then
	printf '%s\n' "Updating DOrg in $DORG_DIR ..."
	git -C "$DORG_DIR" fetch --quiet --tags origin || true
	if [ -n "$DORG_REF" ]; then
		git -C "$DORG_DIR" checkout --quiet "$DORG_REF"
	fi
	git -C "$DORG_DIR" pull --quiet --ff-only || true
elif [ -f "$(dirname "$0")/Makefile" ] && [ -d "$(dirname "$0")/lib/mysql" ]; then
	# Running from inside a DOrg checkout: build it in place.
	DORG_DIR="$(cd "$(dirname "$0")" && pwd)"
	printf '%s\n' "Installing DOrg from the local checkout at $DORG_DIR ..."
else
	printf '%s\n' "Cloning DOrg into $DORG_DIR ..."
	mkdir -p "$(dirname "$DORG_DIR")"
	git clone --quiet "$DORG_REPO" "$DORG_DIR"
	if [ -n "$DORG_REF" ]; then
		git -C "$DORG_DIR" checkout --quiet "$DORG_REF"
	fi
fi

# 2. Install: copy, byte-compile and verify the runtime, then link onto PATH.
printf '%s\n' "Installing (make install PREFIX=$PREFIX BINDIR=$BINDIR) ..."
make -C "$DORG_DIR" install PREFIX="$PREFIX" BINDIR="$BINDIR"

printf '%s\n' "DOrg installed under $PREFIX/share/dorg; the dorg CLI is linked into $BINDIR."
case ":$PATH:" in
	*":$BINDIR:"*) ;;
	*) printf '%s\n' "Add $BINDIR to your PATH: export PATH=\"$BINDIR:\$PATH\"" ;;
esac
