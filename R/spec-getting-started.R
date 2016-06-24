#' @rdname DBIspec
#' @usage NULL
#' @format NULL
#' @section Getting started:
#' This function defines the following tests:
#' \describe{
spec_getting_started <- list(
  #' \item{\code{has_context}}{
  #' \code{make_context()} must be called before calling any of the
  #' \code{test_} functions in this package}
  has_context = function(ctx) {
    expect_is((ctx), "DBItest_context")
  },

  #' \item{\code{package_dependencies}}{
  #' Can relate the driver to an installed (or devtools-loaded) package;
  #' package depends (!) on "DBI" and imports "methods"}. This test requires
  #' the \code{devtools} package and will be skipped if it is not installed.
  package_dependencies = function(ctx) {
    pkg <- get_pkg(ctx)

    pkg_imports <- devtools::parse_deps(pkg$imports)$name
    expect_true("DBI" %in% pkg_imports)
    expect_true("methods" %in% pkg_imports)
  },

  #' \item{\code{package_name}}{
  #' Optional: Package name starts with R.
  #' }
  package_name = function(ctx) {
    pkg_name <- package_name(ctx)
    expect_match(pkg_name, "^R")
  },

  NULL
#'}
)
