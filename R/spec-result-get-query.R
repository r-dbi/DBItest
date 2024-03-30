#' spec_result_get_query
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_get_query <- list(
  get_query_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetQuery)), c("conn", "statement", "..."))
  },

  get_query_atomic = function(con) {
    #' @return
    #' `dbGetQuery()` always returns a [data.frame], with
    #' as many rows as records were fetched and as many
    #' columns as fields in the result set,
    #' even if the result is a single value
    query <- trivial_query()

    rows <- check_df(dbGetQuery(con, query))
    expect_equal(rows, data.frame(a = 1.5))
  },

  get_query_one_row = function(con) {
    #' or has one
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(rows, result)
  },

  get_query_zero_rows = function(con) {
    #' or zero rows.
    # Not all SQL dialects seem to support the query used here.
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(names(rows), letters[1:3])
    expect_identical(dim(rows), c(0L, 3L))
  },

  #'
  get_query_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when issuing a query over a closed
    expect_error(dbGetQuery(closed_con, trivial_query()))
  },

  get_query_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection,
    expect_error(dbGetQuery(invalid_con, trivial_query()))
  },

  get_query_syntax_error = function(con) {
    #' if the syntax of the query is invalid,
    expect_error(dbGetQuery(con, "SELLECT"))
  },

  get_query_non_string = function(con) {
    #' or if the query is not a non-`NA` string.
    expect_error(dbGetQuery(con, character()))
    expect_error(dbGetQuery(con, letters))
    expect_error(dbGetQuery(con, NA_character_))
  },

  get_query_n_bad = function(con) {
    #' If the `n` argument is not an atomic whole number
    #' greater or equal to -1 or Inf, an error is raised,
    query <- trivial_query()
    expect_error(dbGetQuery(con, query, n = -2))
    expect_error(dbGetQuery(con, query, n = 1.5))
    expect_error(dbGetQuery(con, query, n = integer()))
    expect_error(dbGetQuery(con, query, n = 1:3))
  },

  get_query_good_after_bad_n = function(con) {
    #' but a subsequent call to `dbGetQuery()` with proper `n` argument succeeds.
    query <- trivial_query()
    expect_error(dbGetQuery(con, query, n = -2))
    rows <- check_df(dbGetQuery(con, query))
    expect_equal(rows, data.frame(a = 1.5))
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbGetQuery()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `n` (default: -1)
  #' - `params` (default: `NULL`)
  #' - `immediate` (default: `NULL`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  get_query_row_names = function(con) {
    #' @section Specification:
    #'
    #' A column named `row_names` is treated like any other column.
    query <- trivial_query(column = "row_names")
    result <- trivial_df(column = "row_names")

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(rows, result)
    expect_identical(.row_names_info(rows), -1L)
  },

  #'
  get_query_multi_row_single_column = function(ctx, con) {
    #' The `n` argument specifies the number of rows to be fetched.
    #' If omitted, fetching multi-row queries with one
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(rows, result)
  },

  get_query_multi_row_multi_column = function(ctx, con) {
    #' or more columns returns the entire result.
    query <- sql_union(
      .ctx = ctx, paste("SELECT", 1:5 + 0.5, "AS a,", 4:0 + 0.5, "AS b"), .order_by = "a"
    )

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(rows, data.frame(a = 1:5 + 0.5, b = 4:0 + 0.5))
  },

  get_query_n_multi_row_inf = function(ctx, con) {
    #' A value of [Inf] for the `n` argument is supported
    #' and also returns the full result.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    rows <- check_df(dbGetQuery(con, query, n = Inf))
    expect_identical(rows, result)
  },

  get_query_n_more_rows = function(ctx, con) {
    #' If more rows than available are fetched (by passing a too large value for
    #' `n`), the result is returned in full without warning.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    rows <- check_df(dbGetQuery(con, query, n = 5L))
    expect_identical(rows, result)
  },

  get_query_n_zero_rows = function(ctx, con) {
    #' If zero rows are requested, the columns of the data frame are still fully
    #' typed.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(0)

    rows <- check_df(dbGetQuery(con, query, n = 0L))
    expect_identical(rows, result)
  },

  get_query_n_incomplete = function(ctx, con) {
    #' Fetching fewer rows than available is permitted,
    #' no warning is issued.
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(2)

    rows <- check_df(dbGetQuery(con, query, n = 2L))
    expect_identical(rows, result)
  },

  #'
  get_query_params = function(ctx, con) {
    #' The `param` argument allows passing query parameters, see [dbBind()] for details.
    placeholder_funs <- get_placeholder_funs(ctx)

    for (placeholder_fun in placeholder_funs) {
      placeholder <- placeholder_fun(1)
      query <- paste0("SELECT ", placeholder, " + 1.0 AS a")
      values <- trivial_values(3) - 1
      params <- stats::setNames(list(values), names(placeholder))
      ret <- dbGetQuery(con, query, params = params)
      expect_equal(ret, trivial_df(3), info = placeholder)
    }
  },

  get_query_immediate = function(con, table_name) {
    #' @section Specification for the `immediate` argument:
    #'
    #' The `immediate` argument supports distinguishing between "direct"
    #' and "prepared" APIs offered by many database drivers.
    #' Passing `immediate = TRUE` leads to immediate execution of the
    #' query or statement, via the "direct" API (if supported by the driver).
    #' The default `NULL` means that the backend should choose whatever API
    #' makes the most sense for the database, and (if relevant) tries the
    #' other API if the first attempt fails. A successful second attempt
    #' should result in a message that suggests passing the correct
    #' `immediate` argument.
    #' Examples for possible behaviors:
    #' 1. DBI backend defaults to `immediate = TRUE` internally
    #'     1. A query without parameters is passed: query is executed
    #'     1. A query with parameters is passed:
    #'         1. `params` not given: rejected immediately by the database
    #'            because of a syntax error in the query, the backend tries
    #'            `immediate = FALSE` (and gives a message)
    #'         1. `params` given: query is executed using `immediate = FALSE`
    #' 1. DBI backend defaults to `immediate = FALSE` internally
    #'     1. A query without parameters is passed:
    #'         1. simple query: query is executed
    #'         1. "special" query (such as setting a config options): fails,
    #'            the backend tries `immediate = TRUE` (and gives a message)
    #'     1. A query with parameters is passed:
    #'         1. `params` not given: waiting for parameters via [dbBind()]
    #'         1. `params` given: query is executed
    res <- expect_visible(dbGetQuery(con, trivial_query(), immediate = TRUE))
    check_df(res)
  },
  #
  NULL
)
