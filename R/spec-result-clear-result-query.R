#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{Construction: `dbClearResult("DBIResult")`}{
spec_result_clear_result_query <- list(
  #' Return value, currently tests that the return value is always
  #' `TRUE`.
  clear_result_return = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      expect_true(expect_invisible(dbClearResult(res)))
    })
  },

  #' An attempt to close a closed result set issues a warning.
  cannot_clear_result_twice = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      dbClearResult(res)
      expect_warning(expect_true(expect_invisible(dbClearResult(res))))
    })
  },

  #' No warning if result set is closed before connection is closed.
  closed_result_no_warning = function(ctx) {
    expect_warning(
      with_connection({
        expect_warning(dbClearResult(dbSendQuery(con, "SELECT 1")), NA)
        expect_warning(dbClearResult(dbSendQuery(con, "SELECT 2")), NA)
      }),
      NA
    )
  },

  #' Leaving a result open when closing a connection gives a warning.
  stale_result_warning = function(ctx) {
    expect_warning(
      with_connection(dbSendQuery(con, "SELECT 1"))
    )
  },

  #' }
  NULL
)
