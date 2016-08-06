#' @template dbispec
#' @format NULL
#' @section Getting started:
spec_getting_started <- list(
  package_dependencies = function(ctx) {
    #' A DBI backend is an R package,
    pkg <- get_pkg(ctx)

    pkg_imports <- devtools::parse_deps(pkg$imports)$name

    #' which should import the \pkg{DBI}
    expect_true("DBI" %in% pkg_imports)
    #' and \pkg{methods}
    expect_true("methods" %in% pkg_imports)
    #' packages.
  },

  package_name = function(ctx) {
    pkg_name <- package_name(ctx)

    #' For better or worse, the names of many existing backends start with
    #' \sQuote{R}, e.g., \pkg{RSQLite}, \pkg{RMySQL}, \pkg{RSQLServer}; it is up
    #' to the package author to adopt this convention or not.
    expect_match(pkg_name, "^R")
  },

  NULL
)
