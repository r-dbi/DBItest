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
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)
        expect_is(res, "DBIResult")
      })
    },

    # Can issue a command query that creates a table
    command_query = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

      res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
      dbClearResult(res)
      res2 <- dbSendQuery(con, "DROP TABLE test")
      dbClearResult(res2)
    },

    # Issuing an invalid query throws error
    invalid_query = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

      expect_error(dbSendQuery(con, "RAISE"))
    },

    # Return value of dbGetInfo has necessary elements
    get_info = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

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
      on.exit(dbDisconnect(con), add = TRUE)

      res <- dbSendQuery(con, "SELECT 1")
      on.exit(dbClearResult(res), add = TRUE)

      expect_that(res, has_method("show"))
      expect_output(show(res), ".")
    },

    # queries can be fetched
    fetch_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
      })
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, env = parent.frame()) {
  code_sub <- substitute(code)

  eval(bquote({
    con <- connect(ctx)
    on.exit(dbDisconnect(con), add = TRUE)
    local(.(code_sub))
  }), env = env)
}
