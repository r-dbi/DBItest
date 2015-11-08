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
    #' \item{\code{is_valid_result}}{
    #' Only an open result set is valid.
    #' }
    is_valid_result = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        expect_true(dbIsValid(res))
        dbFetch(res)
        expect_true(dbIsValid(res))
        dbClearResult(res)
        expect_false(dbIsValid(res))
      })
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
