#' spec_meta_get_rows_affected
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_meta_get_rows_affected <- list(
  get_rows_affected_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetRowsAffected)), c("res", "..."))
  },

  #' @return
  #' `dbGetRowsAffected()` returns a scalar number (integer or numeric),
  #' the number of rows affected by a data manipulation statement
  rows_affected_statement = function(con, table_name) {
    dbWriteTable(con, table_name, data.frame(a = 1:10))

    query <- paste0(
      "DELETE FROM ", dbQuoteIdentifier(con, table_name), " ",
      "WHERE a < 6"
    )
    with_result(
      #' issued with [dbSendStatement()].
      dbSendStatement(con, query),
      {
        rc <- dbGetRowsAffected(res)
        #' The value is available directly after the call
        expect_equal(rc, 5L)
        expect_warning(check_df(dbFetch(res)))
        rc <- dbGetRowsAffected(res)
        #' and does not change after calling [dbFetch()].
        expect_equal(rc, 5L)
      }
    )
  },
  #
  rows_affected_query = function(con) {
    query <- trivial_query()
    with_result(
      #' For queries issued with [dbSendQuery()],
      dbSendQuery(con, query),
      {
        rc <- dbGetRowsAffected(res)
        #' zero is returned before
        expect_equal(rc, 0L)
        check_df(dbFetch(res))
        rc <- dbGetRowsAffected(res)
        #' and after the call to `dbFetch()`.
        expect_equal(rc, 0L)
      }
    )
  },
  #
  get_rows_affected_error = function(con, table_name) {
    query <- paste0(
      "CREATE TABLE ", dbQuoteIdentifier(con, table_name), " (a integer)"
    )
    res <- dbSendStatement(con, query)
    dbClearResult(res)
    #' Attempting to get the rows affected for a result set cleared with
    #' [dbClearResult()] gives an error.
    expect_error(dbGetRowsAffected(res))
  },
  #
  NULL
)
