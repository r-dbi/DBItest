#' spec_result_fetch
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_fetch <- list(
  fetch_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbFetch)), c("res", "n", "..."))
  },

  #' @return
  #' `dbFetch()` always returns a [data.frame]
  #' with as many rows as records were fetched and as many
  #' columns as fields in the result set,
  #' even if the result is a single value
  fetch_atomic = function(con) {
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  #' or has one
  fetch_one_row = function(con) {
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result)
  },

  #' or zero rows.
  fetch_zero_rows = function(con) {
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(class(rows), "data.frame")
  },

  #'
  #' @section Failure modes:
  #' An attempt to fetch from a closed result set raises an error.
  fetch_closed = function(con) {
    query <- trivial_query()

    res <- dbSendQuery(con, query)
    dbClearResult(res)

    expect_error(dbFetch(res))
  },

  #' If the `n` argument is not an atomic whole number
  #' greater or equal to -1 or Inf, an error is raised,
  fetch_n_bad = function(con) {
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    expect_error(dbFetch(res, -2))
    expect_error(dbFetch(res, 1.5))
    expect_error(dbFetch(res, integer()))
    expect_error(dbFetch(res, 1:3))
    expect_error(dbFetch(res, NA_integer_))
  },

  #' but a subsequent call to `dbFetch()` with proper `n` argument succeeds.
  fetch_n_good_after_bad = function(con) {
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    expect_error(dbFetch(res, NA_integer_))
    rows <- check_df(dbFetch(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  #' Calling `dbFetch()` on a result set from a data manipulation query
  #' created by [dbSendStatement()]
  #' can be fetched and return an empty data frame, with a warning.
  fetch_no_return_value = function(con, table_name) {
    query <- paste0("CREATE TABLE ", table_name, " (a integer)")

    res <- local_result(dbSendStatement(con, query))
    expect_warning(rows <- check_df(dbFetch(res)))
    expect_identical(rows, data.frame())
  },

  #' @section Specification:
  #' Fetching multi-row queries with one
  fetch_multi_row_single_column = function(ctx, con) {
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result)
  },

  #' or more columns by default returns the entire result.
  fetch_multi_row_multi_column = function(ctx, con) {
    query <- union(
      .ctx = ctx, paste("SELECT", 1:5 + 0.5, "AS a,", 4:0 + 0.5, "AS b"), .order_by = "a"
    )

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, data.frame(a = 1:5 + 0.5, b = 4:0 + 0.5))
  },

  #' Multi-row queries can also be fetched progressively
  fetch_n_progressive = function(ctx, con) {
    query <- trivial_query(25, .ctx = ctx, .order_by = "a")
    result <- trivial_df(25)

    res <- local_result(dbSendQuery(con, query))
    #' by passing a whole number ([integer]
    rows <- check_df(dbFetch(res, 10L))
    expect_identical(rows, unrowname(result[1:10, , drop = FALSE]))

    #' or [numeric])
    rows <- check_df(dbFetch(res, 10))
    expect_identical(rows, unrowname(result[11:20, , drop = FALSE]))

    #' as the `n` argument.
    rows <- check_df(dbFetch(res, n = 5))
    expect_identical(rows, unrowname(result[21:25, , drop = FALSE]))
  },

  #' A value of [Inf] for the `n` argument is supported
  #' and also returns the full result.
  fetch_n_multi_row_inf = function(ctx, con) {
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, n = Inf))
    expect_identical(rows, result)
  },

  #' If more rows than available are fetched, the result is returned in full
  #' without warning.
  fetch_n_more_rows = function(ctx, con) {
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, 5L))
    expect_identical(rows, result)
    #' If fewer rows than requested are returned, further fetches will
    #' return a data frame with zero rows.
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result[0, , drop = FALSE])
  },

  #' If zero rows are fetched, the columns of the data frame are still fully
  #' typed.
  fetch_n_zero_rows = function(ctx, con) {
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(0)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, 0L))
    expect_identical(rows, result)
  },

  #' Fetching fewer rows than available is permitted,
  #' no warning is issued when clearing the result set.
  fetch_n_premature_close = function(ctx, con) {
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(2)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, 2L))
    expect_identical(rows, result)
  },

  #'
  #' A column named `row_names` is treated like any other column.
  fetch_row_names = function(con) {
    query <- trivial_query(column = "row_names")
    result <- trivial_df(column = "row_names")

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result)
    expect_identical(.row_names_info(rows), -1L)
  },
  #
  NULL
)
