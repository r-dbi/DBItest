# TODO: Decide where to put this, it's a connection method but requires result methods to be implemented

#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{`dbGetQuery("DBIConnection", "ANY")`}{
spec_result_get_query <- list(
  #' Single-value queries can be read with dbGetQuery
  get_query_single = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L))
    })
  },

  #' Multi-row single-column queries can be read with dbGetQuery.
  get_query_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L:3L))
    })
  },

  #' Empty single-column queries can be read with
  #' [DBI::dbGetQuery()]. Not all SQL dialects support the query
  #' used here.
  get_query_empty_single_column = function(ctx) {
    with_connection({
      query <- "SELECT * FROM (SELECT 1 as a) AS x WHERE (1 = 0)"

      rows <- dbGetQuery(con, query)
      expect_identical(names(rows), "a")
      expect_identical(dim(rows), c(0L, 1L))
    })
  },

  #' Single-row multi-column queries can be read with dbGetQuery.
  get_query_single_row_multi_column = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 2 as b, 3 as c"

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
    })
  },

  #' Multi-row multi-column queries can be read with dbGetQuery.
  get_query_multi = function(ctx) {
    with_connection({
      query <- union(.ctx = ctx, paste("SELECT", 1:2, "AS a,", 2:3, "AS b"),
                     .order_by = "a")

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L:2L, b=2L:3L))
    })
  },

  #' Empty multi-column queries can be read with
  #' [DBI::dbGetQuery()]. Not all SQL dialects support the query
  #' used here.
  get_query_empty_multi_column = function(ctx) {
    with_connection({
      query <-
        "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

      rows <- dbGetQuery(con, query)
      expect_identical(names(rows), letters[1:3])
      expect_identical(dim(rows), c(0L, 3L))
    })
  },

  #' }
  NULL
)
