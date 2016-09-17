#' @template dbispec-sub-wip
#' @format NULL
#' @section Full compliance:
#' \subsection{Read-only access}{
spec_compliance_read_only <- list(
  spec_compliance_methods,

  #' Writing to the database fails.  (You might need to set up a separate
  #' test context just for this test.)
  read_only = function(ctx) {
    with_connection({
      expect_error(dbWriteTable(con, "test", data.frame(a = 1)))
    })
  },

  #' }
  NULL
)
