.PHONY: lint test perf perf-report size dist release-check hooks help

# Targets this project adds, and the ones that should run with `make lint'.
# They live outside this file so that bootstrap can replace the file without
# taking them: `lint: contracts' in targets.mk is enough to wire one in, and
# `help-extra' adds its line to the help output.
-include config/targets.mk

help:
	@echo "make lint              run the check suite"
	@echo "make test              run the ERT test suite"
	@echo "make perf              measure the hot paths against their budgets"
	@echo "make perf-report       print the measurements, never fail"
	@echo "make size              what each module costs and repeats"
	@echo "make hooks             install the commit, commit-msg and pre-push hooks"
	@echo "make dist              build release artifacts from HEAD"
	@echo "make release-check     verify the git tag matches Version:"
	@$(if $(HELP_EXTRA),$(MAKE) --no-print-directory $(HELP_EXTRA))

lint:
	@ci/run-checks.sh

test:
	@ci/run-tests.sh

hooks:
	@pre-commit install
	@pre-commit install --hook-type commit-msg
	@pre-commit install --hook-type pre-push

perf:
	@ci/run-perf.sh

perf-report:
	@ci/run-perf.sh --report

# A number, not a fault: this prints and never fails, and the directories and
# the run length come from the :size table of config/ci.el when a project sets
# one.
size:
	@ci/run-size.sh

dist:
	@ci/build-dist.sh

release-check:
	@ci/check-version-tag.sh
