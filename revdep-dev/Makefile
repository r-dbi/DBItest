all: test

TESTTHAT_REPORTER ?= testthat::ProgressReporter\$$new(max_failures = Inf, update_interval = Inf)

# The REVDEP definition also decides what is run on GitHub Actions in backends.yaml
REVDEP := RMariaDB RSQLite RPostgres RKazam duckdb
TEST_TARGETS := $(patsubst %,test-%,${REVDEP})

LIB_DIR := $(shell Rscript -e "cat(.libPaths()[1])")

${LIB_DIR}/DBItest/DESCRIPTION: ../R/*.R ../DESCRIPTION
	R CMD INSTALL ..

install: ${LIB_DIR}/DBItest/DESCRIPTION

RKazam RSQLite RPostgres RMySQL RMariaDB odbc:
	# Use local checkout if available, otherwise clone from r-dbi GitHub
	if [ -d ../../$@ ]; then ln -s $(realpath ../../$@) .; else git clone https://github.com/r-dbi/$@.git; fi

odbc-%: odbc
	@true

duckdb:
	# Use local checkout if available, otherwise clone from cwida GitHub
	if [ -d ../../../$@ ]; then ln -s $(realpath ../../../$@/tools/rpkg) $@; else git clone https://github.com/cwida/$@.git duckdb-core; ln -s duckdb-core/tools/rpkg duckdb; fi

bigrquery:
	# Use local checkout if available, otherwise clone from GitHub
	if [ -d ../../$@ ]; then ln -s $(realpath ../../$@) .; else git clone https://github.com/krlmlr/$@.git -b feature/dbi-test; Rscript -e "devtools::install_deps('$@', dependencies = TRUE)"; fi

test: ${TEST_TARGETS}

# https://stackoverflow.com/a/42449998/946850
test-%: install %
	@echo
	@echo
	@echo "\033[1m=== $* ===\033[0m"
	@echo
	Rscript -e 'target <- "$*"; if (grepl(".*-.*", target)) { pkg <- sub("-.*", "", target); filter = sub(".*-", "", target) } else { pkg <- target; filter <- "DBItest" }; options(crayon.enabled = TRUE); testthat::test_local(pkg, filter = filter, stop_on_failure = TRUE, reporter = '"${TESTTHAT_REPORTER}"')'

clean:
	rm -rf ${REVDEP}
