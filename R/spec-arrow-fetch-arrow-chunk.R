#' spec_arrow_fetch_arrow_chunk
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_fetch_arrow_chunk <- list(
  arrow_fetch_arrow_chunk_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbFetchArrowChunk)), c("res", "..."))
  },

  arrow_fetch_arrow_chunk_atomic = function(con) {
    #' @return
    #' `dbFetchArrowChunk()` always returns an object coercible to a [data.frame]
    #' with as many rows as records were fetched and as many
    #' columns as fields in the result set,
    #' even if the result is a single value
    query <- trivial_query()
    res <- local_result(dbSendQueryArrow(con, query))
    rows <- check_arrow(dbFetchArrowChunk(res))
    expect_equal(rows, data.frame(a = 1.5))
  },

  arrow_fetch_arrow_chunk_one_row = function(con) {
    #' or has one
    query <- trivial_query(3, letters[1:3])
    result <- trivial_df(3, letters[1:3])
    res <- local_result(dbSendQueryArrow(con, query))
    rows <- check_arrow(dbFetchArrowChunk(res))
    expect_identical(rows, result)
  },

  arrow_fetch_arrow_chunk_zero_rows = function(con) {
    #' or zero rows.
    query <-
      "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"
    res <- local_result(dbSendQueryArrow(con, query))
    rows <- check_arrow(dbFetchArrowChunk(res))
    expect_identical(class(rows), "data.frame")
  },

  #'
  arrow_fetch_arrow_chunk_closed = function(con) {
    skip("Fails in adbc")

    #' @section Failure modes:
    #' An attempt to fetch from a closed result set raises an error.
    query <- trivial_query()

    res <- dbSendQueryArrow(con, query)
    dbClearResult(res)

    expect_error(dbFetchArrowChunk(res))
  },

  arrow_fetch_arrow_chunk_multi_row_single_column = function(ctx, con) {
    #' @section Specification:
    #' Fetching multi-row queries with one
    query <- trivial_query(3, .ctx = ctx, .order_by = "a")
    result <- trivial_df(3)

    res <- local_result(dbSendQueryArrow(con, query))
    rows <- check_arrow(dbFetchArrowChunk(res))
    expect_identical(rows, result)
  },

  arrow_fetch_arrow_chunk_multi_row_multi_column = function(ctx, con) {
    #' or more columns returns the next chunk.
    #' The size of the chunk is implementation-specific.
    query <- sql_union(
      .ctx = ctx, paste("SELECT", 1:5 + 0.5, "AS a,", 4:0 + 0.5, "AS b"), .order_by = "a"
    )

    res <- local_result(dbSendQueryArrow(con, query))
    rows <- check_arrow(dbFetchArrowChunk(res))
    expect_identical(rows, data.frame(a = 1:5 + 0.5, b = 4:0 + 0.5))
  },

  arrow_fetch_arrow_chunk_array = function(ctx, con) {
    #' The object returned by `dbFetchArrowChunk()` can also be passed to
    #' [nanoarrow::as_nanoarrow_array()] to create a nanoarrow array object.
    query <- trivial_query(25, .ctx = ctx, .order_by = "a")
    result <- trivial_df(25)

    res <- local_result(dbSendQueryArrow(con, query))
    chunk <- dbFetchArrowChunk(res)

    rbr <- nanoarrow::as_nanoarrow_array(chunk)

    #' The chunk size is implementation-specific.
    out <- as.data.frame(rbr)
    expect_equal(out, head(result, nrow(out)))
  },

  #
  NULL
)
