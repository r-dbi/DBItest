#' spec_result_send_query
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_send_query <- list(
  send_query_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbSendQuery)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbSendQuery()` returns
  send_query_trivial = function(ctx) {
    with_connection({
      res <- expect_visible(dbSendQuery(con, "SELECT 1"))
      #' an S4 object that inherits from [DBIResult-class].
      expect_s4_class(res, "DBIResult")
      #' The result set can be used with [dbFetch()] to extract records.
      expect_equal(check_df(dbFetch(res))[[1]], 1)
      #' Once you have finished using a result, make sure to clear it
      #' with [dbClearResult()].
      dbClearResult(res)
    })
  },

  #' An error is raised when issuing a query over a closed
  send_query_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbSendQuery(con, "SELECT 1"))
    })
  },

  #' or invalid connection,
  send_query_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbSendQuery(con, "SELECT 1"))
    })
  },

  #' if the syntax of the query is invalid,
  send_query_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbSendQuery(con, "SELECT"))
    })
  },

  #' or if the query is not a non-`NA` string.
  send_query_non_string = function(ctx) {
    with_connection({
      expect_error(dbSendQuery(con, character()))
      expect_error(dbSendQuery(con, letters))
      expect_error(dbSendQuery(con, NA_character_))
    })
  },

  #' @section Specification:
  send_query_result_valid = function(ctx) {
    with_connection({
      #' No warnings occur under normal conditions.
      expect_warning(res <- dbSendQuery(con, "SELECT 1"), NA)
      #' When done, the DBIResult object must be cleared with a call to
      #' [dbClearResult()].
      dbClearResult(res)
    })
  },

  send_query_stale_warning = function(ctx) {
    #' Failure to clear the result set leads to a warning
    #' when the connection is closed.
    expect_warning(
      with_connection({
        dbSendQuery(con, "SELECT 1")
      })
    )
  },

  #'
  #' If the backend supports only one open result set per connection,
  send_query_only_one_result_set = function(ctx) {
    with_connection({
      res1 <- dbSendQuery(con, "SELECT 1")
      #' issuing a second query invalidates an already open result set
      #' and raises a warning.
      expect_warning(res2 <- dbSendQuery(con, "SELECT 2"))
      expect_false(dbIsValid(res1))
      #' The newly opened result set is valid
      expect_true(dbIsValid(res2))
      #' and must be cleared with `dbClearResult()`.
      dbClearResult(res2)
    })
  },

  NULL
)
