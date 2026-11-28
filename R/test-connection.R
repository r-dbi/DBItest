#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_connection()]:
#' Test the "Connection" class
NULL

#' Test the "Connection" class
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-driver.R
#' @family tests
#' @importFrom withr with_temp_libpaths
#' @importFrom methods is
#' @export
test_connection <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  run_tests(ctx, spec_connection, skip, run_only, test_suite)
}
