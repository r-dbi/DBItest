#' \code{test_result()} tests the "Result" class.
#'
#' @rdname test
#' @include test_connection.R
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  tests <- list(
    # Can issue trivial query, result object inherits from "DBIResult"
    trivial_query = function() {
      con <- connect(ctx)
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(dbClearResult(res), add = TRUE)
      expect_is(res, "DBIResult")
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
