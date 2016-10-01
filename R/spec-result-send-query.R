#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{Construction: `dbSendQuery("DBIConnection")` and `dbClearResult("DBIResult")`}{
spec_result_send_query <- list(
  #' Can issue trivial query, result object inherits from "DBIResult".
  trivial_query = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      expect_s4_class(res, "DBIResult")
    })
  },

  #' Return value, currently tests that the return value is always
  #' `TRUE`, and that an attempt to close a closed result set issues a
  #' warning.
  clear_result_return = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      expect_true(dbClearResult(res))
      expect_warning(expect_true(dbClearResult(res)))
    })
  },

  #' Leaving a result open when closing a connection gives a warning.
  stale_result_warning = function(ctx) {
    with_connection({
      expect_warning(dbClearResult(dbSendQuery(con, "SELECT 1")), NA)
      expect_warning(dbClearResult(dbSendQuery(con, "SELECT 2")), NA)
    })

    expect_warning(
      with_connection(dbSendQuery(con, "SELECT 1"))
    )

    with_connection({
      expect_warning(res1 <- dbSendQuery(con, "SELECT 1"), NA)
      expect_true(dbIsValid(res1))
      expect_warning(res2 <- dbSendQuery(con, "SELECT 2"))
      expect_true(dbIsValid(res2))
      expect_false(dbIsValid(res1))
      dbClearResult(res2)
    })
  },

  #' Can issue a command query that creates a table, inserts a row, and
  #' deletes it; the result sets for these query always have "completed"
  #' status.
  command_query = function(ctx) {
    with_connection({
      on.exit({
        res <- dbSendStatement(con, "DROP TABLE test")
        expect_true(dbHasCompleted(res))
        expect_error(dbClearResult(res), NA)
      }
      , add = TRUE)

      res <- dbSendStatement(con, "CREATE TABLE test (a integer)")
      expect_true(dbHasCompleted(res))
      expect_error(dbClearResult(res), NA)

      res <- dbSendStatement(con, "INSERT INTO test SELECT 1")
      expect_true(dbHasCompleted(res))
      expect_error(dbClearResult(res), NA)
    })
  },

  #' Issuing an invalid query throws error (but no warnings, e.g. related to
  #'   pending results, are thrown).
  invalid_query = function(ctx) {
    expect_warning(
      with_connection({
        expect_error(dbSendStatement(con, "RAISE"))
      }),
      NA
    )
  },

  #' }
  NULL
)
