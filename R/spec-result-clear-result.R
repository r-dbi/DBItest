#' spec_result_clear_result
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_clear_result <- list(
  clear_result_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbClearResult)), c("res", "..."))
  },

  clear_result_return_query = function(con) {
    #' @return
    #' `dbClearResult()` returns `TRUE`, invisibly, for result sets obtained from
    #' both `dbSendQuery()`
    res <- dbSendQuery(con, trivial_query())
    expect_invisible_true(dbClearResult(res))
  },

  clear_result_return_statement = function(ctx, con, table_name) {
    #' and `dbSendStatement()`.
    res <- dbSendStatement(con, ctx$tweaks$create_table_as(table_name, "SELECT 1"))
    expect_invisible_true(dbClearResult(res))
  },

  #'
  cannot_clear_result_twice_query = function(con) {
    #' @section Failure modes:
    #' An attempt to close an already closed result set issues a warning
    res <- dbSendQuery(con, trivial_query())
    dbClearResult(res)
    expect_warning(expect_invisible_true(dbClearResult(res)))
  },

  cannot_clear_result_twice_statement = function(ctx, con, table_name) {
    #' in both cases.
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
