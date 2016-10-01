#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbGetRowsAffected("DBIResult")`}{
spec_meta_get_rows_affected <- list(
  #' Information on affected rows is correct.
  rows_affected = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbExecute(con, "DROP TABLE iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)

      local({
        query <- paste0(
          "DELETE FROM iris WHERE (",
          dbQuoteIdentifier(con, "Species"),
          " = ", dbQuoteString(con, "versicolor"),
          ")")
        res <- dbSendStatement(con, query)
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        ra <- dbGetRowsAffected(res)

        expect_identical(ra, sum(iris$Species == "versicolor"))
      })

      local({
        query <- "DELETE FROM iris WHERE (0 = 1)"
        res <- dbSendStatement(con, query)
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        ra <- dbGetRowsAffected(res)

        expect_identical(ra, 0L)
      })
    })
  },

  #' }
  NULL
)
