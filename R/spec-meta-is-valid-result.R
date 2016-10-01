#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbIsValid("DBIResult")`}{
spec_meta_is_valid_result <- list(
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
