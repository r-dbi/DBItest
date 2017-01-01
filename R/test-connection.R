#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_connection()]:
#' Test the "Connection" class
NULL

#' Test the "Connection" class
#'
#' @inheritParams test_all
#' @include test-driver.R
#' @family tests
#' @importFrom withr with_temp_libpaths
#' @importFrom methods is
#' @export
test_connection <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  run_tests(ctx, spec_connection, skip, test_suite)
}
