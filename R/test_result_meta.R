#' @name test_all
#' @section Tests:
#' \code{\link{test_result_meta}}:
#' Test metadata functions for the "Result" class
NULL

#' Test metadata functions for the "Result" class
#'
#' @inheritParams test_all
#' @include test_connection_meta.R
#' @family tests
#' @export
test_result_meta <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result (metadata)"

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

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
