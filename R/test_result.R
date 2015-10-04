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

    # Can issue a command query that creates a table
    command_query = function() {
      con <- connect(ctx)
      res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
      on.exit(add = TRUE, {
        dbClearResult(res)
        res2 <- dbSendQuery(con, "DROP TABLE test")
        dbClearResult(res2)
      })
    },

    # Issuing an invalid query throws error
    invalid_query = function() {
      con <- connect(ctx)
      expect_error(dbSendQuery(con, "RAISE"))
    },

    # Return value of dbGetInfo has necessary elements
    get_info = function() {
      con <- connect(ctx)
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(dbClearResult(res), add = TRUE)

      info <- dbGetInfo(res)
      expect_is(info, "list")
      info_names <- names(info)

      expect_true("statement" %in% info_names)
      expect_true("row.count" %in% info_names)
      expect_true("rows.affected" %in% info_names)
      expect_true("has.completed" %in% info_names)
      expect_true("is.select" %in% info_names)
    },

    # show method for result class is defined
    show = function() {
      con <- connect(ctx)
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(dbClearResult(res), add = TRUE)

      expect_that(res, has_method("show"))
      expect_output(show(res), ".")
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
