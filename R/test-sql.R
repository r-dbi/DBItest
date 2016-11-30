#' @name test_all
#' @aliases NULL
#' @section Tests:
#' [test_sql()]:
#' Test SQL methods
NULL

#' Test SQL methods
#'
#' @inheritParams test_all
#' @include test-result.R
#' @family tests
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  run_tests(ctx, spec_sql, skip, test_suite)
}
