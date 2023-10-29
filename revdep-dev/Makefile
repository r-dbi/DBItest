all: test

TESTTHAT_REPORTER ?= testthat::ProgressReporter\$$new(max_failures = Inf, update_interval = Inf)

# The REVDEP definition also decides what is run on GitHub Actions in backends.yaml
REVDEP := RMariaDB RSQLite RPostgres RKazam duckdb-r odbc-SQLServer odbc-MySQL odbc-PostgreSQL odbc-SQLite
TEST_TARGETS := $(patsubst %,test-%,${REVDEP})

LIB_DIR := $(shell Rscript -e "cat(.libPaths()[1])")

install:

RKazam RSQLite RPostgres RMySQL RMariaDB odbc:
	# Use local checkout if available, otherwise clone from r-dbi GitHub
	if [ -d ../../$@ ]; then ln -s $$(realpath ../../$@) .; else git clone https://github.com/r-dbi/$@.git; fi
	head $@/DESCRIPTION

odbc-%: odbc
	@true

duckdb-r:
	# Use local checkout if available, otherwise clone from duckdb GitHub
	if [ -d ../../$@ ]; then ln -s $$(realpath ../../$@) $@; else git clone https://github.com/duckdb/$@.git; fi
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
	DBITEST_BACKENDS=true Rscript -e 'target <- "$*"; if (grepl(".*-.*", target)) { pkg <- sub("-.*", "", target); filter <- sub(".*-", "", target); cs <- paste0("dsn=", filter); if (filter == "SQLServer") cs <- paste0(cs, ";UID=SA;PWD=Password12"); Sys.setenv(ODBC_CS = cs) } else { pkg <- target; filter <- "DBItest" }; options(crayon.enabled = TRUE); pkgload::load_all(".."); testthat::test_local(pkg, filter = filter, stop_on_failure = TRUE, reporter = '"${TESTTHAT_REPORTER}"')'

clean:
	rm -rf ${REVDEP}
