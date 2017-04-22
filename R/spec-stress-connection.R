#' @template dbispec-sub-wip
#' @format NULL
#' @importFrom withr with_output_sink
#' @section Connection:
#' \subsection{Stress tests}{
spec_stress_connection <- list(
  #' Open 50 simultaneous connections
  simultaneous_connections = function(ctx) {
    cons <- list()
    on.exit(try_silent(lapply(cons, dbDisconnect)), add = TRUE)
    for (i in seq_len(50L)) {
      cons <- c(cons, connect(ctx))
    }

    inherit_from_connection <-
      vapply(cons, is, class2 = "DBIConnection", logical(1))
    expect_true(all(inherit_from_connection))
  },

  #' Open and close 50 connections
  stress_connections = function(ctx) {
    for (i in seq_len(50L)) {
      con <- connect(ctx)
      expect_s4_class(con, "DBIConnection")
      expect_error(dbDisconnect(con), NA)
    }
  },

  #' }
  NULL
)
