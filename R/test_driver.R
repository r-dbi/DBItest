#' \code{test_driver()} tests the "Driver" class.
#'
#' @rdname test
#' @include test_getting_started.R
#' @export
test_driver <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  tests <- list(
    # Driver inherits from "DBIDriver" class
    inherits_from_driver = function() {
      expect_is(ctx$drv, "DBIDriver")
    }
  )
  run_tests(tests, skip, test_suite)
}
