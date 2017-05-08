#' spec_result_fetch
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_fetch <- list(
  fetch_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbFetch)), c("res", "n", "..."))
  },

  #' @return
  #' `dbFetch()` always returns a [data.frame]
  #' with as many rows as records were fetched and as many
  #' columns as fields in the result set,
  #' even if the result is a single value
  fetch_atomic = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = 1L))
        }
      )
    })
  },

  #' or has one
  fetch_one_row = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 2 as b, 3 as c"
      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = 1L, b = 2L, c = 3L))
        }
      )
    })
  },

  #' or zero rows.
  fetch_zero_rows = function(ctx) {
    with_connection({
      query <-
        "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"
      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(class(rows), "data.frame")
        }
      )
    })
  },

  #' An attempt to fetch from a closed result set raises an error.
  fetch_closed = function(ctx) {
    with_connection({
      query <- "SELECT 1"

      res <- dbSendQuery(con, query)
      dbClearResult(res)

      expect_error(dbFetch(res))
    })
  },

  #' If the `n` argument is not an atomic whole number
  #' greater or equal to -1 or Inf, an error is raised,
  fetch_n_bad = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      with_result(
        dbSendQuery(con, query),
        {
          expect_error(dbFetch(res, -2))
          expect_error(dbFetch(res, 1.5))
          expect_error(dbFetch(res, integer()))
          expect_error(dbFetch(res, 1:3))
          expect_error(dbFetch(res, NA_integer_))
        }
      )
    })
  },

  #' but a subsequent call to `dbFetch()` with proper `n` argument succeeds.
  fetch_n_good_after_bad = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      with_result(
        dbSendQuery(con, query),
        {
          expect_error(dbFetch(res, NA_integer_))
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = 1L))
        }
      )
    })
  },

  #' Calling `dbFetch()` on a result set from a data manipulation query
  #' created by [dbSendStatement()]
  #' can be fetched and return an empty data frame, with a warning.
  fetch_no_return_value = function(ctx) {
    with_connection({
      query <- "CREATE TABLE test (a integer)"

      with_remove_test_table({
        with_result(
          dbSendStatement(con, query),
          {
            expect_warning(rows <- check_df(dbFetch(res)))
            expect_identical(rows, data.frame())
          }
        )
      })
    })
  },

  #' @section Specification:
  #' Fetching multi-row queries with one
  fetch_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = 1:3))
        }
      )
    })
  },

  #' or more columns be default returns the entire result.
  fetch_multi_row_multi_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:5, "AS a,", 4:0, "AS b"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = 1:5, b = 4:0))
        }
      )
    })
  },

  #' Multi-row queries can also be fetched progressively
  fetch_n_progressive = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:25, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          #' by passing a whole number ([integer]
          rows <- check_df(dbFetch(res, 10L))
          expect_identical(rows, data.frame(a = 1L:10L))

          #' or [numeric])
          rows <- check_df(dbFetch(res, 10))
          expect_identical(rows, data.frame(a = 11L:20L))

          #' as the `n` argument.
          rows <- check_df(dbFetch(res, n = 5))
          expect_identical(rows, data.frame(a = 21L:25L))
        }
      )
    })
  },

  #' A value of [Inf] for the `n` argument is supported
  #' and also returns the full result.
  fetch_n_multi_row_inf = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res, n = Inf))
          expect_identical(rows, data.frame(a = 1:3))
        }
      )
    })
  },

  #' If more rows than available are fetched, the result is returned in full
  #' without warning.
  fetch_n_more_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res, 5L))
          expect_identical(rows, data.frame(a = 1:3))
          #' If fewer rows than requested are returned, further fetches will
          #' return a data frame with zero rows.
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(a = integer()))
        }
      )
    })
  },

  #' If zero rows are fetched, the columns of the data frame are still fully
  #' typed.
  fetch_n_zero_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res, 0L))
          expect_identical(rows, data.frame(a = integer()))
        }
      )
    })
  },

  #' Fetching fewer rows than available is permitted,
  #' no warning is issued when clearing the result set.
  fetch_n_premature_close = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res, 2L))
          expect_identical(rows, data.frame(a = 1:2))
        }
      )
    })
  },

  #'
  #' A column named `row_names` is treated like any other column.
  fetch_row_names = function(ctx) {
    with_connection({
      query <- "SELECT 1 AS row_names"

      with_result(
        dbSendQuery(con, query),
        {
          rows <- check_df(dbFetch(res))
          expect_identical(rows, data.frame(row_names = 1L))
          expect_identical(.row_names_info(rows), -1L)
        }
      )
    })
  },

  NULL
)
