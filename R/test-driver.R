#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_driver()]:
#' Test the "Driver" class
NULL

#' Test the "Driver" class
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-getting-started.R
#' @family tests
#' @importFrom withr with_temp_libpaths
#' @export
test_driver <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Driver"

  run_tests(ctx, spec_driver, skip, run_only, test_suite)
}
