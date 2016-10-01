#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbGetRowCount("DBIResult")`}{
spec_meta_get_row_count <- list(
  #' Row count information is correct.
  row_count = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 0L)
      dbFetch(res)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 1L)
    })

    with_connection({
      query <- union(.ctx = ctx, "SELECT 1 as a", "SELECT 2", "SELECT 3")
      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 0L)
      dbFetch(res, 2L)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 2L)
      dbFetch(res)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 3L)
    })

    with_connection({
      query <- union(
        .ctx = ctx, "SELECT * FROM (SELECT 1 as a) a WHERE (0 = 1)")
      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 0L)
      dbFetch(res)
      rc <- dbGetRowCount(res)
      expect_equal(rc, 0L)
    })
  },

  #' }
  NULL
)
