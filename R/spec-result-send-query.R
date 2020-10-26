#' spec_result_send_query
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_result_send_query <- list(
  send_query_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbSendQuery)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbSendQuery()` returns
  send_query_trivial = function(con) {
    res <- expect_visible(dbSendQuery(con, trivial_query()))
    #' an S4 object that inherits from [DBIResult-class].
    expect_s4_class(res, "DBIResult")
    #' The result set can be used with [dbFetch()] to extract records.
    expect_equal(check_df(dbFetch(res))[[1]], 1.5)
    #' Once you have finished using a result, make sure to clear it
    #' with [dbClearResult()].
    dbClearResult(res)
  },

  #' An error is raised when issuing a query over a closed
  send_query_closed_connection = function(ctx, closed_con) {
    expect_error(dbSendQuery(closed_con, trivial_query()))
  },

  #' or invalid connection,
  send_query_invalid_connection = function(ctx, invalid_con) {
    expect_error(dbSendQuery(invalid_con, trivial_query()))
  },

  #' or if the query is not a non-`NA` string.
  send_query_non_string = function(con) {
    expect_error(dbSendQuery(con, character()))
    expect_error(dbSendQuery(con, letters))
    expect_error(dbSendQuery(con, NA_character_))
  },

  #' An error is also raised if the syntax of the query is invalid
  #' and all query parameters are given (by passing the `params` argument)
  #' or the `immediate` argument is set to `TRUE`.
  send_query_syntax_error = function(con) {
    expect_error(dbSendQuery(con, "SELLECT", params = list()))
    expect_error(dbSendQuery(con, "SELLECT", immediate = TRUE))
  },
  #' @section Additional arguments:
  #' The following arguments are not part of the `dbSendQuery()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `params` (default: `NULL`)
  #' - `immediate` (default: `NULL`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" sections for details on their usage.

  #' @section Specification:
  send_query_result_valid = function(con) {
    #' No warnings occur under normal conditions.
    expect_warning(res <- dbSendQuery(con, trivial_query()), NA)
    #' When done, the DBIResult object must be cleared with a call to
    #' [dbClearResult()].
    dbClearResult(res)
  },
  #
  send_query_stale_warning = function(ctx) {
    #' Failure to clear the result set leads to a warning
    #' when the connection is closed.
    con <- connect(ctx)
    on.exit(dbDisconnect(con))
    expect_warning(dbSendQuery(con, trivial_query()), NA)

    expect_warning({ dbDisconnect(con); gc() })
    on.exit(NULL)
  },

  #'
  #' If the backend supports only one open result set per connection,
  send_query_only_one_result_set = function(con) {
    res1 <- dbSendQuery(con, trivial_query())
    #' issuing a second query invalidates an already open result set
    #' and raises a warning.
    expect_warning(res2 <- dbSendQuery(con, "SELECT 2"))
    expect_false(dbIsValid(res1))
    #' The newly opened result set is valid
    expect_true(dbIsValid(res2))
    #' and must be cleared with `dbClearResult()`.
    dbClearResult(res2)
  },

  #'
  #' The `param` argument allows passing query parameters, see [dbBind()] for details.
  send_query_params = function(ctx, con) {
    placeholder_funs <- get_placeholder_funs(ctx)

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
  },

  #' @inheritSection spec_result_get_query Specification for the `immediate` argument
  send_query_immediate = function(con, table_name) {
    res <- expect_visible(dbSendQuery(con, trivial_query(), immediate = TRUE))
    expect_s4_class(res, "DBIResult")
    expect_error(dbGetRowsAffected(res), NA)
    dbClearResult(res)
  },
  #
  NULL
)
