#!/usr/bin/env bash
# DOrg installer — fetch DOrg, build it, and put the `dorg' CLI on your PATH.
#
# UNRELEASED: DOrg is not currently releasable.  This script exists for
# disposable development checkouts; it is not a released installer and must
# not be used to adopt DOrg for production data.
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
#   DORG_DIR    where to clone/build DOrg (default: $HOME/.local/share/dorg)
#   BINDIR      where to link the `dorg' wrapper (default: $HOME/.local/bin)
#
# What it does: clones (or updates) DOrg — or builds an existing checkout in
# place — runs `make install' (verify the bundled mysql.el hard dependency,
# check the optional Dolt binary, byte-compile, link bin/dorg onto PATH), then
# finishes with a `dorg doctor' environment verification.
set -euo pipefail

DORG_REPO="${DORG_REPO:-https://github.com/cosmicz/dorg.git}"
DORG_REF="${DORG_REF:-}"
DORG_DIR="${DORG_DIR:-$HOME/.local/share/dorg}"
BINDIR="${BINDIR:-$HOME/.local/bin}"

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

# 2. Build and install: dependency checks, byte-compile, and link onto PATH.
printf '%s\n' "Building and installing (make install BINDIR=$BINDIR) ..."
make -C "$DORG_DIR" install BINDIR="$BINDIR"

# 3. Post-install verification: mysql.el hard dep + optional Dolt.
printf '%s\n' "Post-install verification (dorg doctor --install) ..."
"$BINDIR/dorg" doctor --install

printf '%s\n' "DOrg installed at $DORG_DIR; the dorg CLI is linked into $BINDIR."
case ":$PATH:" in
	*":$BINDIR:"*) ;;
	*) printf '%s\n' "Add $BINDIR to your PATH: export PATH=\"$BINDIR:\$PATH\"" ;;
esac
