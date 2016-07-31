#' @name test_all
#' @section Tests:
#' \code{\link{test_compliance}}:
#' Test full compliance to DBI
NULL

#' Test full compliance to DBI
#'
#' @inheritParams test_all
#' @include test-meta.R
#' @family tests
#' @export
test_compliance <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Full compliance"

  run_tests(ctx, spec_compliance, skip, test_suite)
}
