#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbColumnInfo("DBIResult")`}{
spec_meta_column_info <- list(
  #' Column information is correct.
  column_info = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 1.5 as b, NULL"
      with_result(
        dbSendQuery(con, query),
        {
          ci <- dbColumnInfo(res)
          expect_is(ci, "data.frame")
          expect_identical(colnames(ci), c("name", "type"))
          expect_identical(ci$name[1:2], c("a", "b"))
          expect_is(ci$type, "character")
        }
      )
    })
  },

  #' }
  NULL
)
