#' spec_meta_get_row_count
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_meta_get_row_count <- list(
  get_row_count_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetRowCount)), c("res", "..."))
  },

  #' @return
  #' `dbGetRowCount()` returns a scalar number (integer or numeric),
  #' the number of rows fetched so far.
  row_count_query = function(ctx) {
    with_connection({
      query <- trivial_query()
      with_result(
        #' After calling [dbSendQuery()],
        dbSendQuery(con, query),
        {
          rc <- dbGetRowCount(res)
          #' the row count is initially zero.
          expect_equal(rc, 0L)
          #' After a call to [dbFetch()] without limit,
          check_df(dbFetch(res))
          rc <- dbGetRowCount(res)
          #' the row count matches the total number of rows returned.
          expect_equal(rc, 1L)
        }
      )
    })

    with_connection({
      query <- union(.ctx = ctx, trivial_query(), "SELECT 2", "SELECT 3")
      with_result(
        dbSendQuery(con, query),
        {
          rc1 <- dbGetRowCount(res)
          expect_equal(rc1, 0L)
          #' Fetching a limited number of rows
          check_df(dbFetch(res, 2L))
          #' increases the number of rows by the number of rows returned,
          rc2 <- dbGetRowCount(res)
          expect_equal(rc2, 2L)
          #' even if fetching past the end of the result set.
          check_df(dbFetch(res, 2L))
          rc3 <- dbGetRowCount(res)
          expect_equal(rc3, 3L)
        }
      )
    })

    with_connection({
      #' For queries with an empty result set,
      query <- union(
        .ctx = ctx, "SELECT * FROM (SELECT 1 as a) a WHERE (0 = 1)"
      )
      with_result(
        dbSendQuery(con, query),
        {
          rc <- dbGetRowCount(res)
          #' zero is returned
          expect_equal(rc, 0L)
          check_df(dbFetch(res))
          rc <- dbGetRowCount(res)
          #' even after fetching.
          expect_equal(rc, 0L)
        }
      )
    })
  },

  row_count_statement = function(ctx) {
    with_connection({
      name <- random_table_name()

      with_remove_test_table(name = name, {
        query <- paste0("CREATE TABLE ", name, " (a integer)")
        with_result(
          #' For data manipulation statements issued with
          #' [dbSendStatement()],
          dbSendStatement(con, query),
          {
            rc <- dbGetRowCount(res)
            #' zero is returned before
            expect_equal(rc, 0L)
            expect_warning(check_df(dbFetch(res)))
            rc <- dbGetRowCount(res)
            #' and after calling `dbFetch()`.
            expect_equal(rc, 0L)
          }
        )
      })
    })
  },

  get_row_count_error = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, trivial_query())
      dbClearResult(res)
      #' Attempting to get the row count for a result set cleared with
      #' [dbClearResult()] gives an error.
      expect_error(dbGetRowCount(res))
    })
  },

  NULL
)
