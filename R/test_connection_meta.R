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
    #' \item{\code{is_valid}}{
    #' Only an open connection is valid.
    #' }
    is_valid = function() {
      con <- connect(ctx)
      expect_true(dbIsValid(con))
      expect_error(dbDisconnect(con), NA)
      expect_false(dbIsValid(con))
    },

    #' \item{\code{get_exception}}{
    #' Exception is available after triggering an error, and changes when
    #' triggering a different error.
    #' }
    get_exception = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT SELECT"))
        expect_error(ex1 <- dbGetException(con), NA)
        expect_is(ex1, "character")
        expect_error(dbGetQuery(con, "UPDATE UPDATE"))
        expect_error(ex2 <- dbGetException(con), NA)
        expect_is(ex2, "character")
        expect_true(ex1 != ex2)
      })
    },

    # dbListResult is unsupported in the existing backends

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
