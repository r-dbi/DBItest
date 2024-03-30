#' spec_arrow_get_query_arrow
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_get_query_arrow <- list(
  arrow_get_query_arrow_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbGetQueryArrow)), c("conn", "statement", "..."))
  },

  arrow_get_query_arrow_atomic = function(con) {
    #' @return
    #' `dbGetQueryArrow()` always returns an object coercible to a [data.frame], with
    #' as many rows as records were fetched and as many
    #' columns as fields in the result set,
    #' even if the result is a single value
    query <- trivial_query()

    rows <- check_arrow(dbGetQueryArrow(con, query))
    expect_equal(rows, data.frame(a = 1.5))
  },

  arrow_get_query_arrow_one_row = function(con) {
    #' or has one
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])

    rows <- check_arrow(dbGetQueryArrow(con, query))
    expect_identical(rows, result)
  },

  arrow_get_query_arrow_zero_rows = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.12")

    #' or zero rows.
    # Not all SQL dialects seem to support the query used here.
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

    rows <- check_arrow(dbGetQueryArrow(con, query))
    expect_identical(names(rows), letters[1:3])
    expect_identical(dim(rows), c(0L, 3L))
  },

  #'
  arrow_get_query_arrow_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when issuing a query over a closed
    expect_error(dbGetQueryArrow(closed_con, trivial_query()))
  },

  arrow_get_query_arrow_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection,
    expect_error(dbGetQueryArrow(invalid_con, trivial_query()))
  },

  arrow_get_query_arrow_syntax_error = function(con) {
    #' if the syntax of the query is invalid,
    expect_error(dbGetQueryArrow(con, "SELLECT"))
  },

  arrow_get_query_arrow_non_string = function(con) {
    #' or if the query is not a non-`NA` string.
    expect_error(dbGetQueryArrow(con, character()))
    expect_error(dbGetQueryArrow(con, letters))
    expect_error(dbGetQueryArrow(con, NA_character_))
  },

  arrow_get_query_arrow_record_batch_reader = function(ctx, con) {
    #' The object returned by `dbGetQueryArrow()` can also be passed to
    #' [nanoarrow::as_nanoarrow_array_stream()] to create a nanoarrow
    #' array stream object that can be used to read the result set
    #' in batches.
    query <- trivial_query(25, .ctx = ctx, .order_by = "a")
    result <- trivial_df(25)

    stream <- dbGetQueryArrow(con, query)

    rbr <- nanoarrow::as_nanoarrow_array_stream(stream)

    #' The chunk size is implementation-specific.
    out <- as.data.frame(rbr$get_next())
    expect_equal(out, head(result, nrow(out)))
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbGetQueryArrow()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `params` (default: `NULL`)
  #' - `immediate` (default: `NULL`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.
  #'
  arrow_get_query_arrow_params = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.1")

    #' The `param` argument allows passing query parameters, see [dbBind()] for details.
    placeholder_funs <- get_placeholder_funs(ctx)

    for (placeholder_fun in placeholder_funs) {
      placeholder <- placeholder_fun(1)
      query <- paste0("SELECT ", placeholder, " + 1.0 AS a")
      values <- trivial_values(3) - 1
      params <- stats::setNames(list(values), names(placeholder))
      ret <- dbGetQueryArrow(con, query, params = params)
      expect_equal(as.data.frame(ret), trivial_df(3), info = placeholder)
    }
  },
  #
  arrow_get_query_arrow_immediate = function(ctx, con, table_name) {
    skip_if_not_dbitest(ctx, "1.8.0.2")

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
    res <- expect_visible(dbGetQueryArrow(con, trivial_query(), immediate = TRUE))
    check_arrow(res)
  },
  #
  NULL
)
