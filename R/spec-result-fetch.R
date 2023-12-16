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

  fetch_atomic = function(con) {
    #' @return
    #' `dbFetch()` always returns a [data.frame]
    #' with as many rows as records were fetched and as many
    #' columns as fields in the result set,
    #' even if the result is a single value
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  fetch_one_row = function(con) {
    #' or has one
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result)
  },

  fetch_zero_rows = function(con) {
    #' or zero rows.
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(class(rows), "data.frame")
  },

  fetch_na_rows = function(ctx, con) {
    if (as.package_version(ctx$tweaks$dbitest_version) < "1.7.4") {
      skip(paste0("tweak: dbitest_version: ", ctx$tweaks$dbitest_version))
    }

    #' Passing `n = NA` is supported and returns an arbitrary number of rows (at least one)
    #' as specified by the driver, but at most the remaining rows in the result set.
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, n = NA))
    expect_equal(rows, data.frame(a = 1.5))
  },

  #'
  fetch_closed = function(con) {
    #' @section Failure modes:
    #' An attempt to fetch from a closed result set raises an error.
    query <- trivial_query()

    res <- dbSendQuery(con, query)
    dbClearResult(res)

    expect_error(dbFetch(res))
  },

  fetch_n_bad = function(con) {
    #' If the `n` argument is not an atomic whole number
    #' greater or equal to -1 or Inf, an error is raised,
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    expect_error(dbFetch(res, -2))
    expect_error(dbFetch(res, 1.5))
    expect_error(dbFetch(res, integer()))
    expect_error(dbFetch(res, 1:3))
  },

  fetch_n_good_after_bad = function(con) {
    #' but a subsequent call to `dbFetch()` with proper `n` argument succeeds.
    query <- trivial_query()
    res <- local_result(dbSendQuery(con, query))
    expect_error(dbFetch(res, -2))
    rows <- check_df(dbFetch(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  fetch_no_return_value = function(con, table_name) {
    #'
    #' Calling `dbFetch()` on a result set from a data manipulation query
    #' created by [dbSendStatement()]
    #' can be fetched and return an empty data frame, with a warning.
    query <- paste0("CREATE TABLE ", table_name, " (a integer)")

    res <- local_result(dbSendStatement(con, query))
    expect_warning(rows <- check_df(dbFetch(res)))
    expect_identical(rows, data.frame())
  },

  fetch_multi_row_single_column = function(ctx, con) {
    #' @section Specification:
    #' Fetching multi-row queries with one
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, result)
  },

  fetch_multi_row_multi_column = function(ctx, con) {
    #' or more columns by default returns the entire result.
    query <- sql_union(
      .ctx = ctx, paste("SELECT", 1:5 + 0.5, "AS a,", 4:0 + 0.5, "AS b"), .order_by = "a"
    )

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res))
    expect_identical(rows, data.frame(a = 1:5 + 0.5, b = 4:0 + 0.5))
  },

  fetch_n_progressive = function(ctx, con) {
    #' Multi-row queries can also be fetched progressively
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

  fetch_n_multi_row_inf = function(ctx, con) {
    #' A value of [Inf] for the `n` argument is supported
    #' and also returns the full result.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, n = Inf))
    expect_identical(rows, result)
  },

  fetch_n_more_rows = function(ctx, con) {
    #' If more rows than available are fetched, the result is returned in full
    #' without warning.
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

  fetch_n_zero_rows = function(ctx, con) {
    #' If zero rows are fetched, the columns of the data frame are still fully
    #' typed.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(0)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, 0L))
    expect_identical(rows, result)
  },

  fetch_n_premature_close = function(ctx, con) {
    #' Fetching fewer rows than available is permitted,
    #' no warning is issued when clearing the result set.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(2)

    res <- local_result(dbSendQuery(con, query))
    rows <- check_df(dbFetch(res, 2L))
    expect_identical(rows, result)
  },

  #'
  fetch_row_names = function(con) {
    #' A column named `row_names` is treated like any other column.
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
