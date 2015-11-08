#' Run all tests
#'
#' This function calls all tests defined in this package (see the section
#' "Tests" below).
#'
#' @section Tests:
#' This function runs the following tests:
#'
#' @param skip \code{[character()]}\cr A list of test names to skip
#' @param ctx \code{[DBItest_context]}\cr A test context as created by
#'   \code{\link{make_context}}.
#'
#' @export
test_all <- function(skip = NULL, ctx = get_default_context()) {
  test_getting_started(skip = skip, ctx = ctx)
  test_driver(skip = skip, ctx = ctx)
  test_connection(skip = skip, ctx = ctx)
  test_result(skip = skip, ctx = ctx)
  test_sql(skip = skip, ctx = ctx)
}
