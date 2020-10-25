#' spec_meta_has_completed
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_meta_has_completed <- list(
  has_completed_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbHasCompleted)), c("res", "..."))
  },

  #' @return
  #' `dbHasCompleted()` returns a logical scalar.
  has_completed_query = function(con) {
    #' For a query initiated by [dbSendQuery()] with non-empty result set,
    with_result(
      dbSendQuery(con, trivial_query()),
      {
        #' `dbHasCompleted()` returns `FALSE` initially
        expect_false(expect_visible(dbHasCompleted(res)))
        #' and `TRUE` after calling [dbFetch()] without limit.
        check_df(dbFetch(res))
        expect_true(expect_visible(dbHasCompleted(res)))
      }
    )
  },
  #
  has_completed_statement = function(con, table_name) {
    #' For a query initiated by [dbSendStatement()],
    with_result(
      dbSendStatement(con, paste0("CREATE TABLE ", table_name, " (a integer)")),
      {
        #' `dbHasCompleted()` always returns `TRUE`.
        expect_true(expect_visible(dbHasCompleted(res)))
      }
    )
  },
  #
  has_completed_error = function(con) {
    res <- dbSendQuery(con, trivial_query())
    dbClearResult(res)
    #' Attempting to query completion status for a result set cleared with
    #' [dbClearResult()] gives an error.
    expect_error(dbHasCompleted(res))
  },

  #' @section Specification:
  has_completed_query_spec = function(con) {
    #' The completion status for a query is only guaranteed to be set to
    #' `FALSE` after attempting to fetch past the end of the entire result.
    #' Therefore, for a query with an empty result set,
    with_result(
      dbSendQuery(con, "SELECT * FROM (SELECT 1 as a) AS x WHERE (1 = 0)"),
      {
        #' the initial return value is unspecified,
        #' but the result value is `TRUE` after trying to fetch only one row.
        check_df(dbFetch(res, 1))
        expect_true(expect_visible(dbHasCompleted(res)))
      }
    )

    #' Similarly, for a query with a result set of length n,
    with_result(
      dbSendQuery(con, trivial_query()),
      {
        #' the return value is unspecified after fetching n rows,
        check_df(dbFetch(res, 1))
        #' but the result value is `TRUE` after trying to fetch only one more
        #' row.
        check_df(dbFetch(res, 1))
        expect_true(expect_visible(dbHasCompleted(res)))
      }
    )
  },
  #
  NULL
)
