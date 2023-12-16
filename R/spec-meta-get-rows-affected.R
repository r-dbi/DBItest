#' spec_meta_get_rows_affected
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_get_rows_affected <- list(
  get_rows_affected_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetRowsAffected)), c("res", "..."))
  },

  rows_affected_statement = function(ctx, con, table_name) {
    #' @return
    #' `dbGetRowsAffected()` returns a scalar number (integer or numeric),
    #' the number of rows affected by a data manipulation statement
    dbWriteTable(con, table_name, data.frame(a = 1:10))

    query <- paste0(
      "DELETE FROM ", dbQuoteIdentifier(con, table_name), " ",
      "WHERE a < 6"
    )
    #' issued with [dbSendStatement()].
    res <- local_result(dbSendStatement(con, query))
    rc <- dbGetRowsAffected(res)
    #' The value is available directly after the call
    if (isTRUE(ctx$tweaks$allow_na_rows_affected)) {
      expect_true((is.na(rc) && is.numeric(rc)) || rc == 5L)
    } else {
      expect_equal(rc, 5L)
    }
    expect_warning(check_df(dbFetch(res)))
    rc <- dbGetRowsAffected(res)
    #' and does not change after calling [dbFetch()].
    if (isTRUE(ctx$tweaks$allow_na_rows_affected)) {
      expect_true((is.na(rc) && is.numeric(rc)) || rc == 5L)
    } else {
      expect_equal(rc, 5L)
    }
    #' `NA_integer_` or `NA_numeric_` are allowed if the number of rows affected is not known.
  },
  #
  rows_affected_query = function(ctx, con) {
    query <- trivial_query()
    #'
    #' For queries issued with [dbSendQuery()],
    res <- local_result(dbSendQuery(con, query))
    rc <- dbGetRowsAffected(res)
    #' zero is returned before
    expect_equal(rc, 0L)
    check_df(dbFetch(res))
    rc <- dbGetRowsAffected(res)
    #' and after the call to `dbFetch()`.
    expect_equal(rc, 0L)
    #' `NA` values are not allowed.
  },
  #'
  get_rows_affected_error = function(con, table_name) {
    #' @section Failure modes:
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
