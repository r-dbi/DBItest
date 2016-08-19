#' Run all tests
#'
#' This function calls all tests defined in this package (see the section
#' "Tests" below).
#'
#' @section Tests:
#' This function runs the following tests:
#'
#' @param skip \code{[character()]}\cr A vector of regular expressions to match
#'   against test names; skip test if matching any.
#' @param ctx \code{[DBItest_context]}\cr A test context as created by
#'   \code{\link{make_context}}.
#'
#' @export
test_all <- function(skip = NULL, ctx = get_default_context()) {
  test_transaction(skip = skip, ctx = ctx)
}
