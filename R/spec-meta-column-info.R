#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbColumnInfo("DBIResult")`}{
spec_meta_column_info <- list(
  #' Column information is correct.
  column_info = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 1.5 as b, NULL"
      expect_warning(res <- dbSendQuery(con, query), NA)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      ci <- dbColumnInfo(res)
      expect_is(ci, "data.frame")
      expect_identical(colnames(ci), c("name", "type"))
      expect_identical(ci$name[1:2], c("a", "b"))
      expect_is(ci$type, "character")
    })
  },

  #' }
  NULL
)
