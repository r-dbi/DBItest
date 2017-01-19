#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_result_send_statement Specification
NULL

#' spec_result_send_statement
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_send_statement <- list(
  send_statement_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbSendStatement)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbSendStatement()` returns
  send_statement_trivial = function(ctx) {
    with_connection({
      res <- expect_visible(dbSendStatement(con, "SELECT 1"))
      #' an S4 object that inherits from [DBI::DBIResult-class].
      expect_s4_class(res, "DBIResult")
      #' The result set can be used with [DBI::dbFetch()] to extract records.
      expect_equal(dbFetch(res)[[1]], 1)
      #' Once you have finished using a result, make sure to disconnect it
      #' with [DBI::dbClearResult()].
      dbClearResult(res)
    })
  },

  #' An error is raised when issuing a statement over a closed
  send_statement_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbSendStatement(con, "SELECT 1"))
    })
  },

  #' or invalid connection,
  send_statement_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbSendStatement(con, "SELECT 1"))
    })
  },

  #' if the syntax of the statement is invalid,
  send_statement_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbSendStatement(con, "SELECT"))
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
      #' No warnings occur under normal conditions.
      expect_warning(res <- dbSendStatement(con, "SELECT 1"), NA)
      #' The DBIResult object returned by `dbSendStatement()` must be valid, i.e.,
      #' `dbValid()` returns `TRUE`.
      expect_true(dbIsValid(res))
      #' When done, the DBIResult object must be cleared with a call to
      #' [DBI::dbClearResult()].
      dbClearResult(res)
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

  #'
  send_statement_only_one_result_set = function(ctx) {
    with_connection({
      res1 <- dbSendStatement(con, "SELECT 1")
      #' If a result set is open, issuing a second statement
      #' raises a warning
      expect_warning(res2 <- dbSendStatement(con, "SELECT 2"))
      #' and invalidates the open result set.
      expect_false(dbIsValid(res1))
      #' The newly opened result set is valid
      expect_true(dbIsValid(res2))
      #' and must be cleared with `dbClearResult()`.
      dbClearResult(res2)
    })
  },

  NULL
)
