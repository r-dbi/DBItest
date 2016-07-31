#' @name test_all
#' @section Tests:
#' \code{\link{test_result}}:
#' Test the "Result" class
NULL

#' Test the "Result" class
#'
#' @inheritParams test_all
#' @include test-connection.R
#' @family tests
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  run_tests(ctx, spec_result, skip, test_suite)
}
