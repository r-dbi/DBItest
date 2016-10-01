#' @template dbispec-sub-wip
#' @format NULL
#' @section Connection:
#' \subsection{`dbGetInfo("DBIConnection")` (deprecated)}{
spec_connection_get_info <- list(
  #' Return value of dbGetInfo has necessary elements
  get_info_connection = function(ctx) {
    con <- connect(ctx)
    on.exit(expect_error(dbDisconnect(con), NA), add = TRUE)

    info <- dbGetInfo(con)
    expect_is(info, "list")
    info_names <- names(info)

    necessary_names <-
      c("db.version", "dbname", "username", "host", "port")

    for (name in necessary_names) {
      eval(bquote(
        expect_true(.(name) %in% info_names)))
    }

    expect_false("password" %in% info_names)
  },

  #' }
  NULL
)
