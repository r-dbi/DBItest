#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_compliance()]:
#' Test full compliance to DBI
NULL

#' Test full compliance to DBI
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-arrow.R
#' @family tests
#' @export
test_compliance <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Full compliance"

  run_tests(ctx, spec_compliance, skip, run_only, test_suite)
}
