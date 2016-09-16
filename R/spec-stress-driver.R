#' @template dbispec-sub-wip
#' @format NULL
#' @section Driver:
#' \subsection{Repeated loading, instantiation, and unloading}{
spec_stress_driver <- list(
  #' Repeated load, instantiation, and unload of package in a new R session.
  stress_load_unload = function(ctx) {
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(getRversion() != "3.3.0")

    pkg <- get_pkg(ctx)

    script_file <- tempfile("DBItest", fileext = ".R")
    cat(
      "devtools::RCMD('INSTALL', ", shQuote(pkg$path), ")\n",
      "for (i in 1:50) {\n",
      "  ", pkg$package, "::", deparse(ctx$drv_call), "\n",
      "  unloadNamespace(getNamespace(\"", pkg$package, "\"))\n",
      "}\n",
      sep = "",
      file = script_file
    )

    with_temp_libpaths({
      expect_equal(system(paste0("R -q --vanilla -f ", shQuote(script_file)),
                          ignore.stdout = TRUE, ignore.stderr = TRUE),
                   0L)
    })
  },

  #' }
  NULL
)
