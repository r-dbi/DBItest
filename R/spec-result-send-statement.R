#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{Construction: `dbSendStatement("DBIConnection")`}{
spec_result_send_statement <- list(
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

  #' Issuing an invalid statement throws error (but no warnings, e.g. related to
  #'   pending results, are thrown).
  invalid_statement = function(ctx) {
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
