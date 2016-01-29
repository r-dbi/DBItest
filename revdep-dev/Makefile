all: test

REVDEP := RPostgres RMySQL RSQLite RKazam
TEST_TARGETS := $(patsubst %,test-%,${REVDEP})

LIB_DIR := $(shell Rscript -e "cat(.libPaths()[1])")

${LIB_DIR}/DBItest/DESCRIPTION: ../R/*.R ../DESCRIPTION
	R CMD INSTALL ..

install: ${LIB_DIR}/DBItest/DESCRIPTION

RPostgres RMySQL RSQLite RKazam:
	# Use local checkout if available, otherwise clone from GitHub
	if [ -d ../../$@ ]; then ln -s ../../$@ .; else git clone https://github.com/krlmlr/$@.git -b feature/dbi-test; fi

test: ${TEST_TARGETS}

test-%: install %
	Rscript --vanilla -e "options(crayon.enabled=TRUE, deparse.max.lines=2); devtools::install_deps('$*', dependencies = TRUE); devtools::test('$*', filter = 'DBItest', reporter = 'check')"

clean:
	rm -rf ${REVDEP}
