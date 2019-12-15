#' spec_result_send_statement
#' @usage NULL
#' @format NULL
#' @keywords internal
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
        res <- expect_visible(dbSendStatement(con, trivial_statement()))
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
      expect_error(dbSendStatement(con, trivial_statement()))
    })
  },

  #' or invalid connection,
  send_statement_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbSendStatement(con, trivial_statement()))
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

  #' An error is also raised if the syntax of the query is invalid
  #' and all query parameters are given (by passing the `params` argument)
  #' or the `immediate` argument is set to `TRUE`.
  send_statement_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbSendStatement(con, "CREATTE", params = list()))
      expect_error(dbSendStatement(con, "CREATTE", immediate = TRUE))
    })
  },

  #' @section Specification:
  send_statement_result_valid = function(ctx) {
    with_connection({
      with_remove_test_table({
        #' No warnings occur under normal conditions.
        expect_warning(res <- dbSendStatement(con, trivial_statement()), NA)
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
        expect_warning(dbSendStatement(con, trivial_query()), NA)
      })
    )
  },

  #' If the backend supports only one open result set per connection,
  send_statement_only_one_result_set = function(ctx) {
    with_connection({
      with_remove_test_table({
        res1 <- dbSendStatement(con, trivial_statement())
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

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbSendStatement()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `params` (default: `NULL`)
  #' - `immediate` (default: `NULL`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" sections for details on their usage.

  #' @section Specification:
  #'
  #' The `param` argument allows passing query parameters, see [dbBind()] for details.
  send_statement_params = function(ctx) {
    placeholder_funs <- get_placeholder_funs(ctx)

    with_connection({
      for (placeholder_fun in placeholder_funs) {
        with_remove_test_table(name = "test", {
          dbWriteTable(con, "test", data.frame(a = as.numeric(1:3)))
          placeholder <- placeholder_fun(1)
          query <- paste0("DELETE FROM test WHERE a > ", placeholder)
          values <- 1.5
          params <- stats::setNames(list(values), names(placeholder))
          rs <- dbSendStatement(con, query, params = params)
          expect_equal(dbGetRowsAffected(rs), 2, info = placeholder)
          dbClearResult(rs)
        })
      }
    })
  },

  #' @inheritSection spec_result_get_query Specification for the `immediate` argument
  send_statement_immediate = function(ctx) {
    with_connection({
      with_remove_test_table({
        res <- expect_visible(dbSendStatement(con, trivial_statement(), immediate = TRUE))
        expect_s4_class(res, "DBIResult")
        expect_error(dbGetRowsAffected(res), NA)
        dbClearResult(res)
      })
    })
  },

  NULL
)
