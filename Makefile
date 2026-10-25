EMACS ?= emacs
# Path to standalone mysql.el for the native MySQL wire protocol
MYSQL_DIR ?= ./lib/mysql
# Path to vendored clime (provides the CLI framework for bin/dorg)
CLIME_DIR ?= ./lib/clime
# Optional: path to a clime-make executable.  When unset, the bin/dorg
# target auto-detects clime-make from PATH or common local checkouts.
CLIME_MAKE ?=
CLIME_MAKE_AUTO := $(firstword $(CLIME_MAKE) $(shell command -v clime-make 2>/dev/null) $(wildcard ./lib/clime/bin/clime-make) $(wildcard ../clime/bin/clime-make))

# End-user install: a self-contained runtime tree plus one launcher link.
# `make install' copies the runtime inventory below into a staging
# directory beside $(DORG_HOME), byte-compiles it THERE with the Emacs the
# launcher runs (sources stay beside their bytecode), verifies the staged
# launcher, and only then publishes the tree by rename and points
# $(BINDIR)/dorg at it.  The checkout gains no .elc and the installed
# command never resolves back into it.  Override e.g.
# `make install PREFIX=/usr/local' or `make install DORG_HOME=/opt/dorg'.
ifeq ($(origin PREFIX),undefined)
PREFIX = $(HOME)/.local
BINDIR ?= $(HOME)/bin
else
BINDIR ?= $(PREFIX)/bin
endif
DATADIR ?= $(PREFIX)/share
DORG_HOME ?= $(DATADIR)/dorg
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

.PHONY: all compile lint test tests test-full test-fast test-medium test-derived test-clean-scratch \
	perf bench install install-deps install-root clean clean-elc help mutate-grammar

# A checkout explicitly marked as an active installed runtime must not run
# targets that delete its bytecode or rebuild it as a test side effect.  This
# is a parse-time check: recipe prerequisites may run concurrently under -j,
# so a prerequisite-only guard would be too late.
DORG_INSTALLED_ROOT_MARKER := .installed-root
DORG_GUARDED_GOALS := test tests test-full test-fast test-medium test-derived perf bench lint clean-elc clean
DORG_REQUESTED_GUARDED_GOALS := $(filter $(DORG_GUARDED_GOALS),$(MAKECMDGOALS))
DORG_INSTALL_ROOT_REQUESTED := $(filter install-root,$(MAKECMDGOALS))
DORG_INSTALLED_ROOT_PATH := $(CURDIR)/$(DORG_INSTALLED_ROOT_MARKER)
# `test -L' is required in addition to `test -e': the marker itself is a
# protection boundary, so a dangling symlink counts and is never followed.
DORG_INSTALLED_ROOT_MARKED := $(shell if test -e "$(DORG_INSTALLED_ROOT_PATH)" || test -L "$(DORG_INSTALLED_ROOT_PATH)"; then printf yes; fi)

ifneq ($(strip $(DORG_REQUESTED_GUARDED_GOALS)),)
ifneq ($(DORG_INSTALLED_ROOT_MARKED),)
$(error dorg: REFUSE guarded target $(firstword $(DORG_REQUESTED_GUARDED_GOALS)) in marked installed root $(CURDIR); use an isolated lane worktree)
endif
endif

ifneq ($(strip $(DORG_INSTALL_ROOT_REQUESTED)),)
ifneq ($(strip $(DORG_REQUESTED_GUARDED_GOALS)),)
$(error dorg: REFUSE install-root combined with guarded target $(firstword $(DORG_REQUESTED_GUARDED_GOALS)); run them in separate invocations)
endif
endif

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


# Installed runtime inventory.  Every root dorg*.el module EXCEPT
# dorg-app.el (it ships as the body of the bin/dorg wrapper) and the
# test-only dorg-test-job.el; tests/dorg-project-tests.el keeps this list
# exact.  Vendored dependencies keep their license and provenance notices.
DORG_RUNTIME_MODULES := \
	dorg-api.el \
	dorg-backend.el \
	dorg-bm25.el \
	dorg-command.el \
	dorg-clock.el \
	dorg-clock-emacs.el \
	dorg-doctor.el \
	dorg-dolt-server.el \
	dorg-dolt.el \
	dorg-envelope.el \
	dorg-extension-bm25.el \
	dorg-error.el \
	dorg-ext-read.el \
	dorg-ext-rebuild.el \
	dorg-ext.el \
	dorg-file-lock.el \
	dorg-find.el \
	dorg-freshness.el \
	dorg-git-maintenance.el \
	dorg-git-sync.el \
	dorg-git.el \
	dorg-history.el \
	dorg-id.el \
	dorg-import.el \
	dorg-keywords.el \
	dorg-mode.el \
	dorg-model-load.el \
	dorg-model.el \
	dorg-mutate-grammar.el \
	dorg-mutate.el \
	dorg-parse.el \
	dorg-projection.el \
	dorg-ql-ast.el \
	dorg-ql-core.el \
	dorg-ql-dolt.el \
	dorg-ql-eval.el \
	dorg-ql-model.el \
	dorg-ql-plain.el \
	dorg-ql.el \
	dorg-registry.el \
	dorg-render.el \
	dorg-repo.el \
	dorg-schema.el \
	dorg-session.el \
	dorg-sexp-file.el \
	dorg-sortkey.el \
	dorg-sql-gen.el \
	dorg-sql-quote.el \
	dorg-timestamp.el \
	dorg-transaction.el \
	dorg-transport.el \
	dorg-vfs.el \
	dorg.el
DORG_RUNTIME_LIB_ELISP := $(sort $(wildcard lib/clime/*.el lib/clime/lib/web-server/*.el)) \
	lib/mysql/mysql.el
DORG_RUNTIME_ELISP := $(DORG_RUNTIME_MODULES) $(DORG_RUNTIME_LIB_ELISP)
DORG_RUNTIME_FILES := LICENSE bin/dorg $(DORG_RUNTIME_ELISP) \
	lib/clime/LICENSE lib/clime/PROVENANCE lib/clime/VERSION \
	lib/clime/lib/web-server/COPYING lib/clime/lib/web-server/README \
	lib/mysql/LICENSE lib/mysql/UPSTREAM.org

# Publication protocol (one shell, so the cleanup handler sees every step):
#   preflight  refuse a stale wrapper, an Emacs mismatch, a non-link
#              $(BINDIR)/dorg and an unowned $(DORG_HOME), before any write;
#   stage      copy, compile and verify in a sibling of $(DORG_HOME), so
#              publication is a same-directory rename;
#   publish    rename the old tree aside, rename the stage in, then replace
#              the link by renaming a fresh symlink onto it.  That final
#              rename is the COMMIT POINT.
# The handler runs once on exit, HUP, INT or TERM.  Before the commit point
# it restores the prior tree and link from what is on disk; at or after it
# it keeps the new install and removes the old tree.  SIGKILL cannot be
# handled; depending on the phase it leaves a .dorg-install-stage.* stage
# and/or its .old sibling (the displaced prior tree) for manual recovery.
install:
	@set -e; \
	home_dir='$(DORG_HOME)'; bindir='$(BINDIR)'; link="$$bindir/dorg"; \
	parent=$$(dirname "$$home_dir"); \
	refuse() { printf 'dorg install: REFUSE reason=%s path=%s\n' "$$1" "$$2" >&2; exit 1; }; \
	present() { [ -e "$$1" ] || [ -L "$$1" ]; }; \
	tail -n +3 bin/dorg | cmp -s - dorg-app.el || refuse stale-wrapper "$(CURDIR)/bin/dorg"; \
	launcher_emacs=$$(command -v emacs) || refuse emacs-mismatch emacs; \
	build_emacs=$$(command -v '$(EMACS)') || refuse emacs-mismatch '$(EMACS)'; \
	[ "$$(realpath "$$launcher_emacs")" = "$$(realpath "$$build_emacs")" ] \
		|| refuse emacs-mismatch "$$build_emacs"; \
	if present "$$link" && { [ ! -L "$$link" ] || [ -d "$$link" ]; }; then \
		refuse bindir-entry-not-symlink "$$link"; \
	fi; \
	if present "$$home_dir"; then \
		if [ -L "$$home_dir" ] || [ ! -d "$$home_dir" ] \
			|| [ -L "$$home_dir/.dorg-install" ] || [ ! -f "$$home_dir/.dorg-install" ] \
			|| ! printf 'dorg-install-v1\n' | LC_ALL=C cmp -s - "$$home_dir/.dorg-install"; then \
			refuse unowned-dorg-home "$$home_dir"; \
		fi; \
	fi; \
	mkdir -p "$$parent" "$$bindir"; \
	stage=$$(mktemp -d "$$parent/.dorg-install-stage.XXXXXX"); \
	old="$$stage.old"; verify_dir="$$stage.verify"; tmplink="$$bindir/.dorg-install-link.$$$$"; \
	for reserved in "$$old" "$$verify_dir"; do \
		if present "$$reserved"; then rmdir "$$stage"; refuse reserved-path-present "$$reserved"; fi; \
	done; \
	phase=stage; \
	cleanup() { \
		status=$$1; trap - 0; trap '' HUP INT TERM; set +e; \
		if [ "$$phase" = committed ] \
			|| { [ "$$phase" = link-ready ] && ! present "$$tmplink"; }; then \
			if present "$$old" && ! rm -rf "$$old"; then \
				printf 'dorg install: WARN old-tree-left path=%s\n' "$$old" >&2; \
			fi; \
			printf 'Installed: %s -> %s\n' "$$link" "$$home_dir/bin/dorg"; \
			case ":$$PATH:" in \
				*":$$bindir:"*) ;; \
				*) printf 'NOTE: add %s to your PATH.\n' "$$bindir" ;; \
			esac; \
			exit "$$status"; \
		fi; \
		failed=; \
		rm -f "$$tmplink"; rm -rf "$$verify_dir"; \
		if { [ "$$phase" = publish ] || [ "$$phase" = link-ready ]; } \
			&& ! present "$$stage" && present "$$home_dir"; then \
			mv "$$home_dir" "$$stage" || failed=1; \
		fi; \
		if [ -z "$$failed" ] && present "$$old" && ! present "$$home_dir"; then \
			mv "$$old" "$$home_dir" || failed=1; \
		fi; \
		if [ -n "$$failed" ]; then \
			printf 'dorg install: ROLLBACK-FAILED old=%s stage=%s\n' "$$old" "$$stage" >&2; \
		else \
			rm -rf "$$stage"; \
		fi; \
		[ "$$status" -ne 0 ] || status=1; \
		exit "$$status"; \
	}; \
	trap 'cleanup $$?' 0; trap 'cleanup 129' HUP; trap 'cleanup 130' INT; trap 'cleanup 143' TERM; \
	printf 'Staging in %s\n' "$$stage"; \
	for file in $(DORG_RUNTIME_FILES); do \
		mkdir -p "$$stage/$$(dirname "$$file")"; cp "$$file" "$$stage/$$file"; \
	done; \
	printf 'dorg-install-v1\n' > "$$stage/.dorg-install"; \
	echo "Compiling the installed runtime..."; \
	(cd "$$stage" && env -u EMACSLOADPATH "$$build_emacs" --batch -Q \
		-L . -L lib/clime -L lib/clime/lib/web-server -L lib/mysql \
		--eval '(setq native-comp-jit-compilation nil native-comp-enable-subr-trampolines nil)' \
		-f batch-byte-compile $(DORG_RUNTIME_ELISP)); \
	echo "Verifying the staged install (dorg --help, dorg doctor --install)..."; \
	mkdir "$$verify_dir" "$$verify_dir/home" "$$verify_dir/tmp"; \
	verify() { (cd "$$stage" && env -u EMACSLOADPATH -u XDG_CONFIG_HOME -u XDG_CACHE_HOME \
		-u XDG_DATA_HOME -u XDG_STATE_HOME \
		HOME="$$verify_dir/home" TMPDIR="$$verify_dir/tmp" ./bin/dorg "$$@"); }; \
	verify --help > /dev/null; \
	verify doctor --install; \
	rm -rf "$$verify_dir"; \
	phase=publish; \
	if present "$$home_dir"; then mv "$$home_dir" "$$old"; fi; \
	mv "$$stage" "$$home_dir"; \
	ln -s "$$home_dir/bin/dorg" "$$tmplink"; \
	phase=link-ready; \
	mv -f "$$tmplink" "$$link"; \
	phase=committed


compile:
	@echo "Compiling dorg Elisp files..."
	@$(BATCH_APP) -f batch-byte-compile *.el
	@$(BATCH) -L tools -f batch-byte-compile tools/dorg-public-scrub.el

# Explicitly declare this checkout as the active runtime only after its normal
# compile target has succeeded.  An existing marker (including a symlink) is
# left byte-for-byte alone; its presence already states the desired outcome.
install-root: compile
	@if test -e "$(DORG_INSTALLED_ROOT_PATH)" || test -L "$(DORG_INSTALLED_ROOT_PATH)"; then \
		echo "Installed root remains marked: $(DORG_INSTALLED_ROOT_PATH)"; \
	else \
		: > "$(DORG_INSTALLED_ROOT_PATH)"; \
		echo "Marked installed root: $(DORG_INSTALLED_ROOT_PATH)"; \
	fi

lint:
	@echo "Running lint checks..."
	@$(BATCH_APP) --eval '(require (quote bytecomp))' \
		--eval '(setq byte-compile-error-on-warn t byte-compile-warnings (quote (not docstrings-wide)) byte-compile-docstring-max-column 10000)' \
		-f batch-byte-compile *.el
	@$(BATCH) -L tools \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile tools/dorg-public-scrub.el tools/dorg-derive-families.el
	@echo "Byte-compile clean."
	@# Checkdoc findings are explicitly NON-GATING until debt project tracked-case closes.
	@$(BATCH) -l tools/dorg-checkdoc-lint.el 2>&1
	@echo "Lint complete."

SEL ?=
SELECT ?= $(SEL)
SELECTOR ?= $(SELECT)
# VERBOSE=1 to show message output from tests (not swallowed by ERT)
VERBOSE ?=
TEST_SCRATCH = ./tools/test-scratch
TEST_RUN = $(TEST_SCRATCH) run dorg --
# Unselected defaults must not inherit DORG_BENCH: doing so makes the
# representative's whole-command timing depend on shell state.  An explicitly
# focused selector and the dedicated perf target retain caller environment so a
# deliberate DORG_BENCH=1 benchmark invocation remains available.
DEFAULT_TEST_ENV = env -u DORG_BENCH DORG_TEST_VERBOSE=$(VERBOSE)
FOCUSED_TEST_ENV = DORG_TEST_VERBOSE=$(VERBOSE)
TEST_ENV = $(if $(strip $(SELECTOR)),$(FOCUSED_TEST_ENV),$(DEFAULT_TEST_ENV))
GATE_TEST_ENV = $(DEFAULT_TEST_ENV)
TEST_FULL_ENV = DORG_BENCH=1 DORG_TEST_VERBOSE=$(VERBOSE)
SCRATCH_ROOT ?= $(if $(TMPDIR),$(TMPDIR),/tmp)
SCRATCH_MIN_AGE_SECONDS ?= 86400
SCRATCH_CLEAN_MODE ?= dry-run
SCRATCH_APPLY_FLAG = $(if $(filter apply,$(SCRATCH_CLEAN_MODE)),--apply,)

test: clean-elc
	@$(TEST_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-default-or-selector "$(SELECTOR)")' \
		< /dev/null

tests: test

# Tiered gates so routine loops need not pay the exhaustive suite.
# Classification lives in tests/dorg-test-gates.el.  `make test' is the
# deterministic representative: all FAST files plus three named integration
# files.  It starts the shared sql-server because it exercises live integration.
# `make test-full' is the release gate: it selects every project test namespace
# and enables the otherwise opt-in DORG_BENCH fixtures.  Capability-dependent
# tests can still skip (with ERT's reason) when their prerequisite is absent;
# the project metadata tests prove representative is covered by full and full
# covers the registered project test inventory.
#   test-fast   - Dolt-free unit suites only; skips the shared sql-server, so
#                 it is quick and immune to Dolt contention.  Use after
#                 tracker-only or narrow pure-logic changes.
#   test-medium - the Dolt/sql-server/CLI/serve/transport integration suites
#                 (the ^dorg-test- complement of fast).  Use for merge-fallout
#                 and backend-integration changes.
# An explicit `make test SELECT=...' remains an arbitrary focused selector.
test-full: clean-elc
	@$(TEST_FULL_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-gate "full")' \
		< /dev/null

test-fast: clean-elc
	@$(GATE_TEST_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-gate "fast")' \
		< /dev/null

test-medium: clean-elc
	@$(GATE_TEST_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-gate "medium")' \
		< /dev/null

# Derived pre-ACCEPT families: the whole test files a candidate (HEAD) must
# pass, derived from its changes against the merge-base with BASE, run through
# the real runner in a child, with a receipt in OUT.  DRY=1 stops before the
# run.  The rules, census and exit codes are in tools/dorg-derive-families.el
# and DEVELOPMENT.org.  Arguments reach the tool through the environment, so
# make sets them without shell parsing.
test-derived: export DORG_DERIVED_BASE = $(BASE)
test-derived: export DORG_DERIVED_OUT = $(OUT)
test-derived: export DORG_DERIVED_DRY = $(DRY)
test-derived: clean-elc
	@$(GATE_TEST_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tools/dorg-derive-families.el \
		-f dorg-derive-families-run-batch < /dev/null

# Performance benchmarks remain available as a focused target.  The exhaustive
# `test-full' target also includes them alongside all other project tests.
perf: clean-elc
	@$(FOCUSED_TEST_ENV) $(TEST_RUN) $(BATCH_TEST) -l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch "^dorg-perf-")' \
		< /dev/null

bench: clean-elc
	@echo "Benchmarking test suite..."
	@time $(TEST_ENV) $(TEST_RUN) $(BATCH_TEST) \
		-l ./tests/dorg-tests-runner.el \
		--eval '(dorg-run-tests-batch-default-or-selector "$(SELECTOR)")' \
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
# rebuilds it with clime-make.  The wrapper resolves its runtime through
# relative load paths (.. and ../lib/clime), so `make install' ships it
# unchanged inside $(DORG_HOME) beside the compiled runtime; install refuses
# a wrapper whose body differs from dorg-app.el.
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
	@echo "  install  - Build a self-contained runtime in DORG_HOME and link BINDIR/dorg to it"
	@echo "             PREFIX=$(PREFIX) BINDIR=$(BINDIR) DORG_HOME=$(DORG_HOME)"
	@echo "             Copies, byte-compiles and verifies in a staging dir, then publishes"
	@echo "             by rename.  A failure or signal before the final link rename"
	@echo "             restores the prior install; at or after it the new install is"
	@echo "             kept (a signal there still exits nonzero).  Refuses a stale"
	@echo "             bin/dorg, EMACS differing from PATH emacs, a non-link BINDIR/dorg"
	@echo "             and a DORG_HOME it did not create"
	@echo "  install-deps - Check the checkout: mysql.el bundled, Dolt presence/version"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  install-root - Compile, then mark this checkout as its active installed runtime"
	@echo "                 Marked roots refuse test, lint and cleanup targets; use a lane worktree"
	@echo "  lint     - Byte-compile with warnings + checkdoc"
	@echo "  test     - Run deterministic representative tests; starts shared sql-server"
	@echo "             (SEL= keeps arbitrary focused selection and DORG_BENCH opt-in; VERBOSE=1 shows messages)"
	@echo "  tests    - Alias for 'test'"
	@echo "  test-full - Select exhaustive project tests; enables DORG_BENCH fixtures; starts shared sql-server"
	@echo "              Capability-dependent ERT skips remain reported, not executed coverage"
	@echo "  test-fast   - Dolt-free unit gate (quick, no sql-server); routine loops"
	@echo "  test-medium - Dolt/CLI/serve integration gate; starts shared sql-server"
	@echo "  test-clean-scratch - Audit stale owned test roots (dry-run default)"
	@echo "                       SCRATCH_CLEAN_MODE=apply explicitly deletes eligible roots"
	@echo "                       SCRATCH_MIN_AGE_SECONDS=86400; SCRATCH_ROOT=TMPDIR"
	@echo "  perf     - Run focused dorg-perf-* benchmarks; preserves DORG_BENCH opt-in; starts shared sql-server"
	@echo "  bench    - Time default/focused tests; starts shared sql-server"
	@echo "  bin/dorg - Regenerate the CLI wrapper (auto-detects clime-make)"
	@echo "  mutate-grammar - Regenerate docs/generated/mutate-grammar.org"
	@echo "  clean-elc - Remove byte-compiled .elc files recursively"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
