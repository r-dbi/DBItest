#' spec_result_send_statement
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_send_statement <- list(
  send_statement_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbSendStatement)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbSendStatement()` returns
  send_statement_trivial = function(ctx) {
    with_connection({
      with_remove_test_table({
        res <- expect_visible(dbSendStatement(con, "CREATE TABLE test AS SELECT 1 AS a"))
        #' an S4 object that inherits from [DBIResult-class].
        expect_s4_class(res, "DBIResult")
        #' The result set can be used with [dbGetRowsAffected()] to
        #' determine the number of rows affected by the query.
        expect_error(dbGetRowsAffected(res), NA)
        #' Once you have finished using a result, make sure to clear it
        #' with [dbClearResult()].
        dbClearResult(res)
      })
    })
  },

  #' An error is raised when issuing a statement over a closed
  send_statement_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbSendStatement(con, "CREATE TABLE test AS SELECT 1 AS a"))
    })
  },

  #' or invalid connection,
  send_statement_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbSendStatement(con, "CREATE TABLE test AS SELECT 1 AS a"))
    })
  },

  #' if the syntax of the statement is invalid,
  send_statement_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbSendStatement(con, "CREATE"))
    })
  },

  #' or if the statement is not a non-`NA` string.
  send_statement_non_string = function(ctx) {
    with_connection({
      expect_error(dbSendStatement(con, character()))
      expect_error(dbSendStatement(con, letters))
      expect_error(dbSendStatement(con, NA_character_))
    })
  },

  #' @section Specification:
  send_statement_result_valid = function(ctx) {
    with_connection({
      with_remove_test_table({
        #' No warnings occur under normal conditions.
        expect_warning(res <- dbSendStatement(con, "CREATE TABLE test AS SELECT 1 AS a"), NA)
        #' When done, the DBIResult object must be cleared with a call to
        #' [dbClearResult()].
        dbClearResult(res)
      })
    })
  },

  send_statement_stale_warning = function(ctx) {
    #' Failure to clear the result set leads to a warning
    #' when the connection is closed.
    expect_warning(
      with_connection({
        expect_warning(dbSendStatement(con, "SELECT 1"), NA)
      })
    )
  },

  #' If the backend supports only one open result set per connection,
  send_statement_only_one_result_set = function(ctx) {
    with_connection({
      with_remove_test_table({
        res1 <- dbSendStatement(con, "CREATE TABLE test AS SELECT 1 AS a")
        with_remove_test_table(name = "test2", {
          #' issuing a second query invalidates an already open result set
          #' and raises a warning.
          expect_warning(res2 <- dbSendStatement(con, "CREATE TABLE test2 AS SELECT 1 AS a"))
          expect_false(dbIsValid(res1))
          #' The newly opened result set is valid
          expect_true(dbIsValid(res2))
          #' and must be cleared with `dbClearResult()`.
          dbClearResult(res2)
        })
      })
    })
  },

  NULL
)
