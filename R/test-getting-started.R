#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_getting_started()]:
#' Getting started with testing
NULL

#' Getting started with testing
#'
#' Tests very basic features of a DBI driver package, to support testing and test-first development right from the start.
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test in the suite:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-all.R
#' @family tests
#' @export
test_getting_started <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  run_tests(ctx, spec_getting_started, skip, run_only, test_suite)
}
