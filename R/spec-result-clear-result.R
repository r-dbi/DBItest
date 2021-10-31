#' spec_result_clear_result
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_result_clear_result <- list(
  clear_result_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbClearResult)), c("res", "..."))
  },

  #' @return
  #' `dbClearResult()` returns `TRUE`, invisibly, for result sets obtained from
  #' both `dbSendQuery()`
  clear_result_return_query = function(con) {
    res <- dbSendQuery(con, trivial_query())
    expect_invisible_true(dbClearResult(res))
  },

  #' and `dbSendStatement()`.
  clear_result_return_statement = function(ctx, con, table_name) {
    res <- dbSendStatement(con, ctx$tweaks$create_table_as(table_name, "SELECT 1"))
    expect_invisible_true(dbClearResult(res))
  },

  #' An attempt to close an already closed result set issues a warning
  cannot_clear_result_twice_query = function(con) {
    res <- dbSendQuery(con, trivial_query())
    dbClearResult(res)
    expect_warning(expect_invisible_true(dbClearResult(res)))
  },

  #' in both cases.
  cannot_clear_result_twice_statement = function(ctx, con, table_name) {
    res <- dbSendStatement(con, ctx$tweaks$create_table_as(table_name, "SELECT 1"))
    dbClearResult(res)
    expect_warning(expect_invisible_true(dbClearResult(res)))
  },

  #' @section Specification:
  #' `dbClearResult()` frees all resources associated with retrieving
  #' the result of a query or update operation.
  #' The DBI backend can expect a call to `dbClearResult()` for each
  #' [dbSendQuery()] or [dbSendStatement()] call.
  NULL
)
