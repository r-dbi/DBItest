expect_success <- function (object, ..., info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, not(throws_error(...)), info = info, label = label)
}

#' \code{test_driver()} tests the "Driver" class.
#'
#' @rdname test
#' @include test_getting_started.R
#' @export
test_driver <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Driver"

  tests <- list(
    # Driver inherits from "DBIDriver" class
    inherits_from_driver = function() {
      expect_is(ctx$drv, "DBIDriver")
    },

    # dbGetInfo contains necessary elements
    # Q: dbGetInfo for Driver has been removed everywhere except for RSQLite
    #   (where it returns an empty list). dbGetInfo for other objects
    #   is deprecated in RSQLite, but available in other backends. How to
    #   resolve?

    # SQL Data types exist for all basic R data types
    data_type = function() {
      expect_is(dbDataType(ctx$drv, logical(1)), "character")
      expect_is(dbDataType(ctx$drv, integer(1)), "character")
      expect_is(dbDataType(ctx$drv, double(1)), "character")
      expect_is(dbDataType(ctx$drv, character(1)), "character")
      expect_is(dbDataType(ctx$drv, list(1)), "character")
      expect_is(dbDataType(ctx$drv, Sys.Date()), "character")
      expect_is(dbDataType(ctx$drv, Sys.time()), "character")
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
