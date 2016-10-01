#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{`dbGetInfo("DBIResult")` (deprecated)}{
spec_meta_get_info_result <- list(
  #' Return value of dbGetInfo has necessary elements
  get_info_result = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1 as a")
      info <- dbGetInfo(res)
      expect_is(info, "list")
      info_names <- names(info)

      necessary_names <-
        c("statement", "row.count", "rows.affected", "has.completed")

      for (name in necessary_names) {
        eval(bquote(
          expect_true(.(name) %in% info_names)))
      }
    })
  },

  #' }
  NULL
)
