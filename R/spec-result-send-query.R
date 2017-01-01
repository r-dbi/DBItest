#' @template dbispec-sub-wip
#' @format NULL
#' @section Result:
#' \subsection{Construction: `dbSendQuery("DBIConnection")`}{
spec_result_send_query <- list(
  #' Can issue trivial query, result object inherits from "DBIResult".
  trivial_query = function(ctx) {
    with_connection({
      res <- expect_visible(dbSendQuery(con, "SELECT 1"))
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      expect_s4_class(res, "DBIResult")
    })
  },

  #' Error is given when issuing a query over an invalid connection.
  send_query_invalid_connection = function(ctx) {
    expect_warning(
      with_invalid_connection({
        expect_error(dbSendQuery(con, "SELECT 1"))
      }),
      NA
    )
  },

  #' Issuing a second query without clearing invalidates an open result set,
  #' with warning.
  only_one_result_set = function(ctx) {
    with_connection({
      expect_warning(res1 <- dbSendQuery(con, "SELECT 1"), NA)
      expect_true(dbIsValid(res1))
      expect_warning(res2 <- dbSendQuery(con, "SELECT 2"))
      expect_true(dbIsValid(res2))
      expect_false(dbIsValid(res1))
      dbClearResult(res2)
    })
  },

  #' Issuing an invalid query throws error (but no warnings, e.g. related to
  #'   pending results, are thrown).
  invalid_query = function(ctx) {
    expect_warning(gc(), NA)
    expect_warning(
      with_connection({
        expect_error(dbSendStatement(con, "SELECT"))
        gc()
      }),
      NA
    )
  },

  #' }
  NULL
)
