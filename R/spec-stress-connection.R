#' @format NULL
#' @importFrom withr with_output_sink
#' @section Connection:
#' \subsection{Stress tests}{
spec_stress_connection <- list(
  simultaneous_connections = function(ctx) {
    #' Open 50 simultaneous connections
    cons <- list()
    on.exit(try_silent(map(cons, dbDisconnect)), add = TRUE)
    for (i in seq_len(50L)) {
      cons <- c(cons, connect(ctx))
    }

    inherit_from_connection <-
      map_lgl(cons, is, class2 = "DBIConnection")
    expect_true(all(inherit_from_connection))
  },

  stress_connections = function(ctx) {
    #' Open and close 50 connections
    for (i in seq_len(50L)) {
      con <- connect(ctx)
      expect_s4_class(con, "DBIConnection")
      expect_error(dbDisconnect(con), NA)
    }
  },

  #' }
  NULL
)
