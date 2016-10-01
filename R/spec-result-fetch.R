#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{`dbFetch("DBIResult")` and `dbHasCompleted("DBIResult")`}{
spec_result_fetch <- list(
  #' Single-value queries can be fetched.
  fetch_single = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame(a=1L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' Multi-row single-column queries can be fetched.
  fetch_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame(a=1L:3L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' Multi-row queries can be fetched progressively.
  fetch_progressive = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:25, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=1L:10L))
      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=11L:20L))
      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=21L:25L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' If more rows than available are fetched, the result is returned in full
  #'   but no warning is issued.
  fetch_more_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      expect_warning(rows <- dbFetch(res, 5L), NA)
      expect_identical(rows, data.frame(a=1L:3L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' If zero rows are fetched, the result is still fully typed.
  fetch_zero_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_warning(rows <- dbFetch(res, 0L), NA)
      expect_identical(rows, data.frame(a=integer()))

      expect_warning(dbClearResult(res), NA)
      on.exit(NULL, add = FALSE)
    })
  },

  #' If less rows than available are fetched, the result is returned in full
  #'   but no warning is issued.
  fetch_premature_close = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_warning(rows <- dbFetch(res, 2L), NA)
      expect_identical(rows, data.frame(a=1L:2L))

      expect_warning(dbClearResult(res), NA)
      on.exit(NULL, add = FALSE)
    })
  },

  #' Side-effect-only queries (without return value) can be fetched.
  fetch_no_return_value = function(ctx) {
    with_connection({
      query <- "CREATE TABLE test (a integer)"

      res <- dbSendStatement(con, query)
      on.exit({
        expect_error(dbClearResult(res), NA)
        expect_error(dbClearResult(dbSendStatement(con, "DROP TABLE test")), NA)
      }
      , add = TRUE)

      expect_true(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame())

      expect_true(dbHasCompleted(res))
    })
  },

  #' Fetching from a closed result set raises an error.
  fetch_closed = function(ctx) {
    with_connection({
      query <- "SELECT 1"

      res <- dbSendQuery(con, query)
      dbClearResult(res)

      expect_error(dbHasCompleted(res))

      expect_error(dbFetch(res))
    })
  },

  #' Querying a disconnected connection throws error.
  cannot_query_disconnected = function(ctx) {
    # TODO: Rename to fetch_disconnected
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(dbGetQuery(con, "SELECT 1"))
  },

  #' }
  NULL
)
