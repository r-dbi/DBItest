#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_result()]:
#' Test the "Result" class
NULL

#' Test the "Result" class
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-connection.R
#' @family tests
#' @export
test_result <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  run_tests(ctx, spec_result, skip, run_only, test_suite)
}
