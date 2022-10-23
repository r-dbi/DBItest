#' spec_arrow_stream
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_stream <- list(
  arrow_stream_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbStream)), c("res", "..."))
  },

  arrow_stream_atomic = function(con) {
    #' @return
    #' `dbStream()` always returns an object coercible to a [data.frame]
    #' with as many rows as records were fetched and as many
    #' columns as fields in the result set,
    #' even if the result is a single value
    query <- trivial_query()
    res <- local_result(dbSendQueryStream(con, query))
    rows <- check_stream(dbStream(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  arrow_stream_one_row = function(con) {
    #' or has one
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])
    res <- local_result(dbSendQueryStream(con, query))
    rows <- check_stream(dbStream(res))
    expect_identical(rows, result)
  },

  arrow_stream_zero_rows = function(con) {
    skip("Causes segfault in adbc")

    #' or zero rows.
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"
    res <- local_result(dbSendQueryStream(con, query))
    rows <- check_stream(dbStream(res))
    expect_identical(class(rows), "data.frame")
  },

  #'
  arrow_stream_closed = function(con) {
    skip("Fails in adbc")

    #' @section Failure modes:
    #' An attempt to fetch from a closed result set raises an error.
    query <- trivial_query()

    res <- dbSendQueryStream(con, query)
    dbClearResult(res)

    expect_error(dbStream(res))
  },

  arrow_stream_multi_row_single_column = function(ctx, con) {
    #' @section Specification:
    #' Fetching multi-row queries with one
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQueryStream(con, query))
    rows <- check_stream(dbStream(res))
    expect_identical(rows, result)
  },

  arrow_stream_multi_row_multi_column = function(ctx, con) {
    #' or more columns by default returns the entire result.
    query <- sql_union(
      .ctx = ctx, paste("SELECT", 1:5 + 0.5, "AS a,", 4:0 + 0.5, "AS b"), .order_by = "a"
    )

    res <- local_result(dbSendQueryStream(con, query))
    rows <- check_stream(dbStream(res))
    expect_identical(rows, data.frame(a = 1:5 + 0.5, b = 4:0 + 0.5))
  },

  arrow_stream_record_batch_reader = function(ctx, con) {
    #' The object returned by `dbStream()` can also be passed to
    #' [arrow::as_record_batch_reader()] to create an Arrow
    #' RecordBatchReader object that can be used to read the result set
    #' in batches.
    query <- trivial_query(25, .ctx = ctx, .order_by = "a")
    result <- trivial_df(25)

    res <- local_result(dbSendQueryStream(con, query))
    stream <- dbStream(res)

    rbr <- arrow::as_record_batch_reader(stream)

    #' The chunk size is implementation-specific.
    out <- as.data.frame(rbr$read_next_batch())
    expect_equal(out, head(result, nrow(out)))
  },

  #
  NULL
)
