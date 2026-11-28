#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_arrow()]:
#' Test Arrow methods
NULL

#' Test Arrow methods
#'
#' @inheritParams test_all
#' @include test-transaction.R
#' @family tests
#' @export
test_arrow <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Arrow"

  run_tests(ctx, spec_arrow, skip, run_only, test_suite)
}
