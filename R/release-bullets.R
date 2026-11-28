# nocov start

# Extra items for the release checklist.
# `usethis::use_release_issue()` and `devtools::release()` call this unexported function
# and add its bullets to the checklist they generate.
release_bullets <- function() {
  c(
    "Check that every backend that tests against DBItest (r-dbi/RSQLite, r-dbi/RMariaDB, r-dbi/RPostgres, r-dbi/odbc, duckdb/duckdb-r, r-dbi/adbi, r-dbi/RKazam) declares the newest `dbitest_version` tweak, found by grepping `skip_if_not_dbitest()` in `R/spec-*.R`, and passes this version's specs",
    "Run the reverse-dependency check (revdep4, `which: most`, depth 1) with `commit: false` and triage new problems against the CRAN version"
  )
}

# nocov end
