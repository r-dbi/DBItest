#' spec_result_get_query
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_get_query <- list(
  get_query_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetQuery)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbGetQuery()` always returns a [data.frame]
  #' with as many rows as records were fetched and as many
  #' columns as fields in the result set,
  #' even if the result is a single value
  get_query_atomic = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(a=1L))
    })
  },

  #' or has one
  get_query_one_row = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 2 as b, 3 as c"

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
    })
  },

  #' or zero rows.
  get_query_zero_rows = function(ctx) {
    with_connection({
      # Not all SQL dialects seem to support the query used here.
      query <-
        "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(names(rows), letters[1:3])
      expect_identical(dim(rows), c(0L, 3L))
    })
  },


  #' An error is raised when issuing a query over a closed
  get_query_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbGetQuery(con, "SELECT 1"))
    })
  },

  #' or invalid connection,
  get_query_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbGetQuery(con, "SELECT 1"))
    })
  },

  #' if the syntax of the query is invalid,
  get_query_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT"))
    })
  },

  #' or if the query is not a non-`NA` string.
  get_query_non_string = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, character()))
      expect_error(dbGetQuery(con, letters))
      expect_error(dbGetQuery(con, NA_character_))
    })
  },

  #' If the `n` argument is not an atomic whole number
  #' greater or equal to -1 or Inf, an error is raised,
  get_query_n_bad = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      expect_error(dbGetQuery(con, query, -2))
      expect_error(dbGetQuery(con, query, 1.5))
      expect_error(dbGetQuery(con, query, integer()))
      expect_error(dbGetQuery(con, query, 1:3))
      expect_error(dbGetQuery(con, query, NA_integer_))
    })
  },

  #' but a subsequent call to `dbGetQuery()` with proper `n` argument succeeds.
  get_query_good_after_bad_n = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      expect_error(dbGetQuery(con, query, NA_integer_))
      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(a = 1L))
    })
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbGetQuery()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `n` (default: -1)
  #' - `params` (TBD)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  #' @section Specification:
  #' Fetching multi-row queries with one
  get_query_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' or more columns be default returns the entire result.
  get_query_multi_row_multi_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:5, "AS a,", 4:0, "AS b"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(a = 1:5, b = 4:0))
    })
  },

  #' A value of [Inf] for the `n` argument is supported
  #' and also returns the full result.
  get_query_n_multi_row_inf = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query, n = Inf))
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' If more rows than available are fetched, the result is returned in full
  #' without warning.
  get_query_n_more_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query, n = 5L))
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' If zero rows are fetched, the columns of the data frame are still fully
  #' typed.
  get_query_n_zero_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query, n = 0L))
      expect_identical(rows, data.frame(a=integer()))
    })
  },

  #' Fetching fewer rows than available is permitted,
  #' no warning is issued.
  get_query_n_incomplete = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- check_df(dbGetQuery(con, query, n = 2L))
      expect_identical(rows, data.frame(a = 1:2))
    })
  },

  #'
  #' A column named `row_names` is treated like any other column.
  get_query_row_names = function(ctx) {
    with_connection({
      query <- "SELECT 1 AS row_names"

      rows <- check_df(dbGetQuery(con, query))
      expect_identical(rows, data.frame(row_names = 1L))
      expect_identical(.row_names_info(rows), -1L)
    })
  },

  NULL
)
