#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbIsValid()`}{
spec_meta_is_valid <- list(
  #' Only an open connection is valid.
  is_valid_connection = function(ctx) {
    con <- connect(ctx)
    expect_true(dbIsValid(con))
    expect_error(dbDisconnect(con), NA)
    expect_false(dbIsValid(con))
  },

  #' Only an open result set is valid.
  is_valid_result = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      res <- dbSendQuery(con, query)
      expect_true(dbIsValid(res))
      expect_error(dbFetch(res), NA)
      expect_true(dbIsValid(res))
      dbClearResult(res)
      expect_false(dbIsValid(res))
    })
  },

  #' }
  NULL
)
