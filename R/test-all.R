#' Run all tests
#'
#' `test_all()` calls all tests defined in this package (see the section
#' "Tests" below).
#'
#' @section Tests:
#' This function runs the following tests, except the stress tests:
#'
#' @param skip `[character()]`\cr A vector of regular expressions to match
#'   against test names; skip test if matching any.
#' @param ctx `[DBItest_context]`\cr A test context as created by
#'   [make_context()].
#'
#' @export
test_all <- function(skip = NULL, ctx = get_default_context()) {
  test_getting_started(skip = skip, ctx = ctx)
  test_driver(skip = skip, ctx = ctx)
  test_connection(skip = skip, ctx = ctx)
  test_result(skip = skip, ctx = ctx)
  test_sql(skip = skip, ctx = ctx)
  test_meta(skip = skip, ctx = ctx)
  test_transaction(skip = skip, ctx = ctx)
  test_compliance(skip = skip, ctx = ctx)
  # stress tests are not tested by default (#92)
}

#' @rdname test_all
#' @description `test_some()` allows testing one or more tests, it works by
#'   constructing the `skip` argument using negative lookaheads.
#' @param test `[character]`\cr A character vector of regular expressions
#'   describing the tests to run.
#' @export
test_some <- function(test, ctx = get_default_context()) {
  test_all(skip = paste0("(?!", paste(test, collapse = "|"), ").*$"), ctx = ctx)
}
