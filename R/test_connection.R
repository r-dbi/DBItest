#' \code{test_connection()} tests the "Connection" class.
#'
#' @rdname test
#' @include test_driver.R
#' @export
test_connection <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  tests <- list(
    # Can connect and disconnect
    can_connect_and_disconnect = function() {
      con <- connect(ctx)
      expect_is(con, "DBIConnection")
      expect_success(dbDisconnect(con))
    },

    # Connection inherits from "DBIConnection" class
    inherits_from_connection = function() {
      #expect_is(ctx$drv, "DBIDriver")
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
