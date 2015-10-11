#' \code{test_sql()} tests SQL methods.
#'
#' @rdname test
#' @include test_result.R
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  tests <- list(
    # Can issue trivial query, result object inherits from "DBIResult"
    trivial_query = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)
        expect_is(res, "DBIResult")
      })
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
