#' @template dbispec-sub-wip
#' @format NULL
#' @section Connection:
#' \subsection{Stress tests}{
spec_stress_connection <- list(
  #' Open 50 simultaneous connections
  simultaneous_connections = function(ctx) {
    cons <- list()
    on.exit(expect_error(lapply(cons, dbDisconnect), NA), add = TRUE)
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

  #' Repeated load, instantiation, connection, disconnection, and unload of
  #' package in a new R session.
  stress_load_connect_unload = function(ctx) {
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(getRversion() != "3.3.0")

    pkg <- get_pkg(ctx)

    script_file <- tempfile("DBItest", fileext = ".R")
    local({
      sink(script_file)
      on.exit(sink(), add = TRUE)
      cat(
        "devtools::RCMD('INSTALL', ", shQuote(pkg$path), ")\n",
        "library(DBI, quietly = TRUE)\n",
        "connect_args <- ",
        sep = ""
      )
      dput(ctx$connect_args)
      cat(
        "for (i in 1:50) {\n",
        "  drv <- ", pkg$package, "::", deparse(ctx$drv_call), "\n",
        "  con <- do.call(dbConnect, c(drv, connect_args))\n",
        "  dbDisconnect(con)\n",
        "  unloadNamespace(getNamespace(\"", pkg$package, "\"))\n",
        "}\n",
        sep = ""
      )
    })

    with_temp_libpaths({
      expect_equal(system(paste0("R -q --vanilla -f ", shQuote(script_file)),
                          ignore.stdout = TRUE, ignore.stderr = TRUE),
                   0L)
    })
  },

  #' }
  NULL
)
