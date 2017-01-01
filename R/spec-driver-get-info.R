#' @template dbispec-sub-wip
#' @format NULL
#' @section Driver:
#' \subsection{`dbGetInfo("DBIDriver")` (deprecated)}{
spec_driver_get_info <- list(
  #' Return value of dbGetInfo has necessary elements.
  get_info_driver = function(ctx) {
    info <- dbGetInfo(ctx$drv)
    expect_is(info, "list")
    info_names <- names(info)

    necessary_names <-
      c("driver.version", "client.version")

    for (name in necessary_names) {
      eval(bquote(
        expect_true(.(name) %in% info_names)))
    }
  },

  #' }
  NULL
)
