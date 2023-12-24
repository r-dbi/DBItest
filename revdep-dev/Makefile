all: test

TESTTHAT_REPORTER ?= testthat::ProgressReporter\$$new(max_failures = Inf, update_interval = Inf)

# The REVDEP definition also decides what is run on GitHub Actions in backends.yaml
# FIXME: Re-add duckdb, very slow on GitHub Actions
# FIXME: Re-add odbc-postgres and odbc-sqlite, broken with new Arrow tests
# FIXME: Re-add RPostgres adbi odbc-sql-server, #343
REVDEP := RMariaDB RSQLite RKazam odbc-mysql
TEST_TARGETS := $(patsubst %,test-%,${REVDEP})

LIB_DIR := $(shell Rscript -e "cat(.libPaths()[1])")

install:

RKazam RSQLite RPostgres RMySQL RMariaDB odbc adbi:
	# Use local checkout if available, otherwise clone from r-dbi GitHub
	if [ -d ../../$@ ]; then ln -s $$(realpath ../../$@) .; else git clone https://github.com/r-dbi/$@.git; fi
	head $@/DESCRIPTION

odbc-%: odbc
	@true

duckdb:
	# Use local checkout if available, otherwise clone from duckdb GitHub
	if [ -d ../../../$@-r ]; then ln -s $$(realpath ../../../$@-r) $@; else git clone https://github.com/duckdb/$@-r.git $@; fi
	head $@/DESCRIPTION

bigrquery:
	# Use local checkout if available, otherwise clone from GitHub
	if [ -d ../../$@ ]; then ln -s $$(realpath ../../$@) .; else git clone https://github.com/krlmlr/$@.git -b feature/dbi-test; Rscript -e "devtools::install_deps('$@', dependencies = TRUE)"; fi

test: ${TEST_TARGETS}

# https://stackoverflow.com/a/42449998/946850
test-%: install %
	@echo
	@echo
	@echo "\033[1m=== $* ===\033[0m"
	@echo
	DBITEST_BACKENDS=true Rscript -e 'pkgload::load_all(".."); test_backend("$@", reporter = '"${TESTTHAT_REPORTER}"')'

clean:
	rm -rf ${REVDEP}
