#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_stress()]:
#' Stress tests (not tested with `test_all`)
NULL

#' Stress tests
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test selected by `run_only`:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-compliance.R
#' @family tests
#' @keywords internal
#' @export
test_stress <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Stress"

  run_tests(ctx, spec_stress, skip, test_suite)
}
