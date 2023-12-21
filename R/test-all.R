#' Run all tests
#'
#' `test_all()` calls all tests defined in this package (see the section
#' "Tests" below). This function supports running only one test by setting an
#' environment variable, e.g., set the `DBITEST_ONLY_RESULT` to a nonempty
#' value to run only `test_result()`.
#'
#' Internally `^` and `$` are used as prefix and suffix around the
#' regular expressions passed in the `skip` and `run_only` arguments.
#'
#' @section Tests:
#' This function runs the following tests, except the stress tests:
#'
#' @param skip `[character()]`\cr A vector of regular expressions to match
#'   against test names; skip test if matching any.
#'   The regular expressions are matched against the entire test name.
#' @param run_only `[character()]`\cr A vector of regular expressions to match
#'   against test names; run only these tests.
#'   The regular expressions are matched against the entire test name.
#' @param ctx `[DBItest_context]`\cr A test context as created by
#'   [make_context()].
#'
#' @export
test_all <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  run_all <- length(grep("^DBITEST_ONLY_", names(Sys.getenv()))) == 0
  if (run_all || Sys.getenv("DBITEST_ONLY_GETTING_STARTED") != "") test_getting_started(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_DRIVER") != "") test_driver(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_CONNECTION") != "") test_connection(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_RESULT") != "") test_result(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_SQL") != "") test_sql(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_META") != "") test_meta(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_TRANSACTION") != "") test_transaction(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_ARROW") != "") test_arrow(skip = skip, run_only = run_only, ctx = ctx)
  if (run_all || Sys.getenv("DBITEST_ONLY_COMPLIANCE") != "") test_compliance(skip = skip, run_only = run_only, ctx = ctx)
  # stress tests are not tested by default (#92)
  invisible()
}

#' @rdname test_all
#' @description `test_some()` allows testing one or more tests.
#' @param test `[character]`\cr
#'   A character vector of regular expressions
#'   describing the tests to run.
#'   The regular expressions are matched against the entire test name.
#' @export
test_some <- function(test, ctx = get_default_context()) {
  test_all(run_only = test, skip = character(), ctx = ctx)
  invisible()
}
