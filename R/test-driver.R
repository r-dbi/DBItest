#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_driver()]:
#' Test the "Driver" class
NULL

#' Test the "Driver" class
#'
#' @inheritParams test_all
#' @include test-getting-started.R
#' @family tests
#' @importFrom withr with_temp_libpaths
#' @export
test_driver <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Driver"

  run_tests(ctx, spec_driver, skip, test_suite)
}
