#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_sql()]:
#' Test SQL methods
NULL

#' Test SQL methods
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-result.R
#' @family tests
#' @export
test_sql <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  run_tests(ctx, spec_sql, skip, run_only, test_suite)
}
