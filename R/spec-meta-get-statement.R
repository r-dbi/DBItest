#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbGetStatement("DBIResult")`}{
spec_meta_get_statement <- list(
  #' SQL query can be retrieved from the result.
  get_statement = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"
      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      s <- dbGetStatement(res)
      expect_is(s, "character")
      expect_identical(s, query)
    })
  },

  #' }
  NULL
)
