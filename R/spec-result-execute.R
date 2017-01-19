#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_result_execute Specification
NULL

#' spec_result_execute
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_execute <- list(
  execute_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbExecute)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbExecute()` always returns a [data.frame]
  #' with as many rows as records were fetched and as many
  #' columns as fields in the result set,
  #' even if the result is a single value
  execute_atomic = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      rows <- dbExecute(con, query)
      expect_identical(rows, data.frame(a=1L))
    })
  },

  #' or has one
  execute_one_row = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 2 as b, 3 as c"

      rows <- dbExecute(con, query)
      expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
    })
  },

  #' or zero rows.
  execute_zero_rows = function(ctx) {
    with_connection({
      # Not all SQL dialects seem to support the query used here.
      query <-
        "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

      rows <- dbExecute(con, query)
      expect_identical(names(rows), letters[1:3])
      expect_identical(dim(rows), c(0L, 3L))
    })
  },

  #' @section Specification:
  #' Fetching multi-row queries with one
  execute_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- dbExecute(con, query)
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' or more columns be default returns the entire result.
  execute_multi_row_multi_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:5, "AS a", 4:0, "AS b"), .order_by = "a")

      rows <- dbExecute(con, query)
      expect_identical(rows, data.frame(a = 1:5, b = 4:0))
    })
  },

  #' A value of [Inf] for the `n` argument is supported
  #' and also returns the full result.
  execute_multi_row_inf = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- dbExecute(query, n = Inf)
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' If more rows than available are fetched, the result is returned in full
  #' without warning.
  execute_more_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      expect_warning(rows <- dbExecute(query, 5L), NA)
      expect_identical(rows, data.frame(a = 1:3))
    })
  },

  #' If zero rows are fetched, the columns of the data frame are still fully
  #' typed.
  execute_zero_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      expect_warning(rows <- dbExecute(query, 0L), NA)
      expect_identical(rows, data.frame(a=integer()))
    })
  },

  #' Fetching fewer rows than available is permitted,
  #' no warning is issued.
  execute_incomplete = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- dbExecute(query, 2L)
      expect_identical(rows, data.frame(a = 1:2))
    })
  },

  NULL
)
