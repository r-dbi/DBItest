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
      res <- expect_visible(dbSendQuery(con, trivial_query()))
      #' an S4 object that inherits from [DBIResult-class].
      expect_s4_class(res, "DBIResult")
      #' The result set can be used with [dbFetch()] to extract records.
      expect_equal(check_df(dbFetch(res))[[1]], 1.5)
      #' Once you have finished using a result, make sure to clear it
      #' with [dbClearResult()].
      dbClearResult(res)
    })
  },

  #' An error is raised when issuing a query over a closed
  send_query_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbSendQuery(con, trivial_query()))
    })
  },

  #' or invalid connection,
  send_query_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbSendQuery(con, trivial_query()))
    })
  },

  #' if the syntax of the query is invalid,
  send_query_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbSendQuery(con, "SELLECT"))
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

  #' @section Additional arguments:
  #' The following argument is not part of the `dbSendQuery()` generic
  #' (to improve compatibility across backends)
  #' but is part of the DBI specification:
  #' - `params` (default: `NULL`)
  #'
  #' It must be provided as named arguments.
  #' See the "Specification" sections for details on its usage.

  #' @section Specification:
  send_query_result_valid = function(ctx) {
    with_connection({
      #' No warnings occur under normal conditions.
      expect_warning(res <- dbSendQuery(con, trivial_query()), NA)
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
        dbSendQuery(con, trivial_query())
      })
    )
  },

  #'
  #' If the backend supports only one open result set per connection,
  send_query_only_one_result_set = function(ctx) {
    with_connection({
      res1 <- dbSendQuery(con, trivial_query())
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

  #'
  #' The `param` argument allows passing query parameters, see [dbBind()] for details.
  send_query_params = function(ctx) {
    placeholder_funs <- get_placeholder_funs(ctx)

    with_connection({
      for (placeholder_fun in placeholder_funs) {
        placeholder <- placeholder_fun(1)
        query <- paste0("SELECT ", placeholder, " + 1.0 AS a")
        values <- trivial_values(3) - 1
        params <- stats::setNames(list(values), names(placeholder))
        rs <- dbSendQuery(con, query, params = params)
        ret <- dbFetch(rs)
        expect_equal(ret, trivial_df(3), info = placeholder)
        dbClearResult(rs)
      }
    })
  },

  NULL
)
