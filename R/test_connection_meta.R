#' @name test_all
#' @section Tests:
#' \code{\link{test_connection_meta}}:
#' Test metadata functions for the "Connection" class
NULL

#' Test metadata functions for the "Connection" class
#'
#' @inheritParams test_all
#' @include test_sql.R
#' @family tests
#' @export
test_connection_meta <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection (metadata)"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{can_connect_and_disconnect}}{
    #' Can connect and disconnect, connection object inherits from
    #'   "DBIConnection"
    #' }
    can_connect_and_disconnect = function() {
      con <- connect(ctx)
      expect_is(con, "DBIConnection")
      expect_error(dbDisconnect(con), NA)
      expect_error(dbDisconnect(con))
      expect_error(dbGetQuery(con, "select 1"))
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
