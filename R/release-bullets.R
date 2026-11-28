# nocov start

# Extra items for the release checklist.
# `usethis::use_release_issue()` looks up this unexported function in the namespace
# and adds its bullets to the checklist it generates.
release_bullets <- function() {
  c(
    "Check that every backend that tests against DBItest (r-dbi/RSQLite, r-dbi/RMariaDB, r-dbi/RPostgres, r-dbi/odbc, duckdb/duckdb-r, r-dbi/adbi, r-dbi/RKazam) declares the newest `dbitest_version` tweak (the default is 1.7.1), found by grepping `skip_if_not_dbitest()` in `R/spec-*.R`, and passes this version's specs",
    "Run the reverse-dependency check (revdep4, `which: most`, depth 1) with `commit: false` and triage new problems against the CRAN version"
  )
}

# nocov end
