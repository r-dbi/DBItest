#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_stress()]:
#' Stress tests (not tested with `test_all`)
NULL

#' Stress tests
#'
#' @inheritParams test_all
#' @include test-compliance.R
#' @family tests
#' @export
test_stress <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Stress"

  run_tests(ctx, spec_stress, skip, test_suite)
}
