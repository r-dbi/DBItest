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



    NULL
  )
  run_tests(tests, skip, test_suite)
}
