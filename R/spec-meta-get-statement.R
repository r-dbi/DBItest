#' spec_meta_get_statement
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_get_statement <- list(
  get_statement_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetStatement)), c("res", "..."))
  },

  get_statement_query = function(con) {
    #' @return
    #' `dbGetStatement()` returns a string, the query used in
    query <- trivial_query()
    #' either [dbSendQuery()] or
    res <- local_result(dbSendQuery(con, query))
    s <- dbGetStatement(res)
    expect_type(s, "character")
    expect_identical(s, query)
  },
  #
  get_statement_statement = function(con, table_name) {
    query <- paste0("CREATE TABLE ", table_name, " (a integer)")
    #' [dbSendStatement()].
    res <- local_result(dbSendStatement(con, query))
    s <- dbGetStatement(res)
    expect_type(s, "character")
    expect_identical(s, query)
  },
  #'
  get_statement_error = function(con) {
    #' @section Failure modes:
    res <- dbSendQuery(con, trivial_query())
    dbClearResult(res)
    #' Attempting to query the statement for a result set cleared with
    #' [dbClearResult()] gives an error.
    expect_error(dbGetStatement(res))
  },
  #
  NULL
)
