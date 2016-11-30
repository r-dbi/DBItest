#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_getting_started()]:
#' Getting started with testing
NULL

#' Getting started with testing
#'
#' Tests very basic features of a DBI driver package, to support testing
#' and test-first development right from the start.
#'
#' @inheritParams test_all
#' @include test-all.R
#' @family tests
#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  run_tests(ctx, spec_getting_started, skip, test_suite)
}
