#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_meta()]:
#' Test metadata functions
NULL

#' Test metadata functions
#'
#' @inheritParams test_all
#' @return Called for its side effect of running the tests with testthat.
#'   Returns a logical vector with one element per test in the suite:
#'   `TRUE` for a test that ran without failure, `FALSE` for one that failed or was excluded by `skip`.
#' @include test-sql.R
#' @family tests
#' @export
test_meta <- function(skip = NULL, run_only = NULL, ctx = get_default_context()) {
  test_suite <- "Metadata"

  run_tests(ctx, spec_meta, skip, run_only, test_suite)
}
