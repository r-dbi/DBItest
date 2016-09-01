#' @name test_all
#' @section Tests:
#' \code{\link{test_stress}}:
#' Stress tests (not tested with \code{test_all})
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
