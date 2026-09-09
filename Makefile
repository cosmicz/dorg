EMACS ?= emacs
# Path to standalone mysql.el for the native MySQL wire protocol
MYSQL_DIR ?= ./lib/mysql
# Path to vendored clime (provides the CLI framework for bin/dorg)
CLIME_DIR ?= ./lib/clime
# Optional: path to a clime-make executable.  When unset, the bin/dorg
# target auto-detects clime-make from PATH or common local checkouts.
CLIME_MAKE ?=
CLIME_MAKE_AUTO := $(firstword $(CLIME_MAKE) $(shell command -v clime-make 2>/dev/null) $(wildcard ./lib/clime/bin/clime-make) $(wildcard ../clime/bin/clime-make))

# End-user install: where the `dorg' wrapper is linked onto PATH.
# Override e.g. `make install BINDIR=/usr/local/bin'.
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
# Minimum supported Dolt version (mirrors `dorg-dolt-minimum-version' in
# dorg-dolt.el; Dolt is optional — only the Dolt backend needs it).
DOLT_MIN ?= 1.86.4

BATCH = $(EMACS) --batch -Q -L . -L ./tests
# `dorg-app.el' (the bin/dorg CLI app) lives at repo root and requires
# clime, so compile/lint/tests must put vendored clime on the load path.
# Core dorg still gains NO clime dependency: dorg-app is not a member of
# `dorg--modules' / `dorg.el', and a dedicated test asserts that
# (require 'dorg) loads cleanly without clime on the path.
BATCH_APP = $(BATCH) $(if $(wildcard $(CLIME_DIR)/clime.el),-L $(CLIME_DIR),)
# Tests load vendored clime (for dorg-app) and mysql.el (for native MySQL).
BATCH_TEST = $(BATCH) $(if $(wildcard $(MYSQL_DIR)/mysql.el),-L $(MYSQL_DIR),) \
	$(if $(wildcard $(CLIME_DIR)/clime.el),-L $(CLIME_DIR),)

.PHONY: all compile lint test tests test-fast test-medium test-clean-scratch \
	perf bench install install-deps clean clean-elc help mutate-grammar

all: compile

# ── End-user install ─────────────────────────────────────────────────────

install-deps:
	@echo "Checking DOrg dependencies (mysql.el loadable + Dolt presence/version)..."
	@test -f bin/dorg || { \
		echo "ERROR: the bin/dorg wrapper is missing." >&2; \
		echo "  Re-clone DOrg, or regenerate it with 'make bin/dorg' (needs clime-make)." >&2; \
		exit 1; \
	}
	@# Authoritative check via the committed wrapper: hard-loads mysql.el
	@# (an ERROR + nonzero exit if it is missing/broken, naming this installer),
	@# and reports Dolt as supported / UNSUPPORTED-too-old / absent-optional via
	@# `dorg-dolt-supported-p' (the $(DOLT_MIN)+ floor).  No network.
	@./bin/dorg doctor --install

install: install-deps compile
	@mkdir -p "$(BINDIR)"
	@ln -sf "$(abspath bin/dorg)" "$(BINDIR)/dorg"
	@echo "Installed: $(BINDIR)/dorg -> $(abspath bin/dorg)"
	@case ":$$PATH:" in \
		*":$(BINDIR):"*) ;; \
		*) echo "NOTE: add $(BINDIR) to your PATH, e.g. 'export PATH=\"$(BINDIR):$$PATH\"'." ;; \
	esac
	@echo "Verifying install (dorg doctor --install)..."
	@"$(BINDIR)/dorg" doctor --install

compile:
	@echo "Compiling dorg Elisp files..."
	@$(BATCH_APP) -f batch-byte-compile *.el
	@$(BATCH) -L tools -f batch-byte-compile tools/dorg-public-scrub.el

lint:
	@echo "Running lint checks..."
	@$(BATCH_APP) --eval '(require (quote bytecomp))' \
		--eval '(setq byte-compile-error-on-warn t byte-compile-warnings (quote (not docstrings-wide)) byte-compile-docstring-max-column 10000)' \
		-f batch-byte-compile *.el
	@$(BATCH) -L tools \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile tools/dorg-public-scrub.el
	@echo "Byte-compile clean."
	@# Checkdoc findings are explicitly NON-GATING until debt project tracked-case closes.
	@$(BATCH) -l tools/dorg-checkdoc-lint.el 2>&1
	@echo "Lint complete."

SEL ?= ^dorg-test-
SELECT ?= $(SEL)
SELECTOR ?= $(SELECT)
# VERBOSE=1 to show message output from tests (not swallowed by ERT)
VERBOSE ?=
TEST_SCRATCH = ./tools/test-scratch
TEST_RUN = $(TEST_SCRATCH) run dorg --
SCRATCH_ROOT ?= $(if $(TMPDIR),$(TMPDIR),/tmp)
SCRATCH_MIN_AGE_SECONDS ?= 86400
SCRATCH_CLEAN_MODE ?= dry-run
SCRATCH_APPLY_FLAG = $(if $(filter apply,$(SCRATCH_CLEAN_MODE)),--apply,)

test: clean-elc
	@DORG_TEST_VERBOSE=$(VERBOSE) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch "$(SELECTOR)")' \
		< /dev/null

tests: test

# Tiered gates so routine loops need not pay the full ~6-minute suite.
# Classification lives in tests/dorg-test-gates.el and is proved exhaustive,
# disjoint, and non-replacing by the dorg-test-project-gate-* meta-tests.
#   test-fast   - Dolt-free unit suites only; skips the shared sql-server, so
#                 it is quick and immune to Dolt contention.  Use after
#                 tracker-only or narrow pure-logic changes.
#   test-medium - the Dolt/sql-server/CLI/serve/transport integration suites
#                 (the ^dorg-test- complement of fast).  Use for merge-fallout
#                 and backend-integration changes.
# `make test' (above) remains the complete acceptance gate; these ADD to it.
test-fast: clean-elc
	@DORG_TEST_VERBOSE=$(VERBOSE) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-gate "fast")' \
		< /dev/null

test-medium: clean-elc
	@DORG_TEST_VERBOSE=$(VERBOSE) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-gate "medium")' \
		< /dev/null

# Performance benchmarks live under the dorg-perf-* namespace and are
# intentionally excluded from the default ^dorg-test- suite (they are
# timing-oriented, slower, and partly gated on Dolt/sql-server).  Run
# them explicitly here, or via `make test SELECT=^dorg-perf-`.
perf: clean-elc
	@DORG_TEST_VERBOSE=$(VERBOSE) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch "^dorg-perf-")' \
		< /dev/null

bench: clean-elc
	@echo "Benchmarking test suite..."
	@time env DORG_TEST_VERBOSE="$(VERBOSE)" $(TEST_RUN) $(BATCH_TEST) \
		-l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch "$(SELECTOR)")' \
		< /dev/null

test-clean-scratch:
	@case "$(SCRATCH_CLEAN_MODE)" in \
		dry-run|apply) ;; \
		*) echo "REFUSE reason=invalid-mode target=$(SCRATCH_CLEAN_MODE)" >&2; exit 64 ;; \
	esac
	@$(TEST_SCRATCH) sweep --root "$(SCRATCH_ROOT)" \
		--min-age-seconds "$(SCRATCH_MIN_AGE_SECONDS)" $(SCRATCH_APPLY_FLAG)

N ?= 3
M ?= 20
K ?= 50
SLEEP_MIN ?= 10
SLEEP_MAX ?= 10

# Regenerate the committed CLI wrapper from dorg-app.el.  The wrapper is
# committed for checkout convenience, but the Make target behaves like a
# normal file target: if bin/dorg is missing or older than dorg-app.el, it
# rebuilds it with clime-make.  Runtime still uses relative load paths into
# this checkout and vendored lib/clime; the wrapper is not a standalone
# distribution artifact.
bin/dorg: dorg-app.el Makefile
	@if [ -z "$(CLIME_MAKE_AUTO)" ]; then \
		echo "ERROR: clime-make not found; set CLIME_MAKE=/path/to/clime-make, install clime-make on PATH, or check out Clime at ../clime" >&2; \
		exit 1; \
	fi
	@mkdir -p bin
	@echo "Regenerating bin/dorg via $(CLIME_MAKE_AUTO)..."
	@"$(CLIME_MAKE_AUTO)" init dorg-app.el -o bin/dorg \
		-R .. -R ../lib/clime --standalone \
		-e CLIME_MAIN_APP=dorg-cli --force

# Regenerate the checked-in public mutation grammar from its semantic inventory.
mutate-grammar:
	@mkdir -p docs/generated
	@$(BATCH) -l tools/generate-mutate-grammar.el > docs/generated/mutate-grammar.org

clean-elc:
	@find . -name '*.elc' -delete

clean: clean-elc
	@echo "Cleaning up all build artifacts..."
	@rm -rf .packages
	@echo "Done."

help:
	@echo "dorg Makefile targets:"
	@echo "  all      - Default target. Same as 'compile'"
	@echo "  install  - Check deps, byte-compile, link bin/dorg onto PATH (BINDIR=), verify"
	@echo "  install-deps - Verify mysql.el is bundled and report Dolt presence/version"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  lint     - Byte-compile with warnings + checkdoc"
	@echo "  test     - Run the full acceptance suite (SEL= to filter, VERBOSE=1)"
	@echo "  tests    - Alias for 'test'"
	@echo "  test-fast   - Dolt-free unit gate (quick, no sql-server); routine loops"
	@echo "  test-medium - Dolt/CLI/serve integration gate; merge-fallout changes"
	@echo "  test-clean-scratch - Audit stale owned test roots (dry-run default)"
	@echo "                       SCRATCH_CLEAN_MODE=apply explicitly deletes eligible roots"
	@echo "                       SCRATCH_MIN_AGE_SECONDS=86400; SCRATCH_ROOT=TMPDIR"
	@echo "  perf     - Run dorg-perf-* benchmarks (excluded from default 'test')"
	@echo "  bench    - Run tests with wall-clock timing"
	@echo "  bin/dorg - Regenerate the CLI wrapper (auto-detects clime-make)"
	@echo "  mutate-grammar - Regenerate docs/generated/mutate-grammar.org"
	@echo "  clean-elc - Remove byte-compiled .elc files recursively"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
