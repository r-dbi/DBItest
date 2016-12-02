#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_transaction()]:
#' Test transaction functions
NULL

#' Test transaction functions
#'
#' @inheritParams test_all
#' @include test-meta.R
#' @family tests
#' @export
test_transaction <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Transactions"

  run_tests(ctx, spec_transaction, skip, test_suite)
}
