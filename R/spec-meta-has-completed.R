#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_meta_has_completed Specification
NULL

#' spec_meta_has_completed
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_has_completed <- list(
  has_completed_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbHasCompleted)), c("res", "..."))
  },

  #' @return
  #' `dbHasCompleted()` returns a logical scalar.
  has_completed_query = function(ctx) {
    with_connection({
      #' For a query initiated by [DBI::dbSendQuery()] with non-empty result set,
      with_result(
        "SELECT 1",
        {
          #' `dbHasCompleted()` returns `FALSE` initially
          expect_false(expect_visible(dbHasCompleted(res)))
          #' and `TRUE` after calling [DBI::dbFetch()] without limit.
          dbFetch(res)
          expect_true(expect_visible(dbHasCompleted(res)))
        }
      )
    })
  },

  has_completed_statement = function(ctx) {
    with_connection({
      name <- random_table_name()

      on.exit(try_silent(dbExecute(paste0("DROP TABLE ", name))), add = TRUE)

      #' For a query initiated by [DBI::dbSendStatement()],
      with_result(
        paste0("CREATE TABLE ", name, " (a integer)"),
        {
          #' `dbHasCompleted()` always returns `TRUE`.
          expect_rue(expect_visible(dbHasCompleted(res)))
        }
      )
    })
  },

  has_completed_error = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      dbClearResult(res)
      #' Attempting to query completion status for a result set cleared with
      #' [DBI::dbClearResult()] gives an error.
      expect_error(dbHasCompleted(res))
    })
  },

  #' @section Specification:
  has_completed_query_spec = function(ctx) {
    with_connection({
      #' The completion status for a query is only guaranteed to be set to
      #' `FALSE` after attempting to fetch past the end of the entire result.
      #' Therefore, for a query with an empty result set,
      with_result(
        "SELECT * FROM (SELECT 1 as a) AS x WHERE (1 = 0)",
        {
          #' the initial return value is unspecified,
          #' but the result value is `TRUE` after trying to fetch only one row.
          dbFetch(res, 1)
          expect_true(expect_visible(dbHasCompleted(res)))
        }
      )

      #' Similarly, for a query with a result set of length n,
      with_result(
        "SELECT 1",
        {
          #' the return value is unspecified after fetching n rows,
          dbFetch(res, 1)
          #' but the result value is `TRUE` after trying to fetch only one more
          #' row.
          dbFetch(res, 1)
          expect_true(expect_visible(dbHasCompleted(res)))
        }
      )
    })
  },

  NULL
)
