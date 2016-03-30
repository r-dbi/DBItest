#' @name test_all
#' @section Tests:
#' \code{\link{test_getting_started}}:
#' Getting started with testing
NULL

#' Getting started with testing
#'
#' Tests very basic features of a DBI driver package, to support testing
#' and test-first development right from the start.
#'
#' @inheritParams test_all
#' @include test_all.R
#' @family tests
#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{has_context}}{
    #' \code{make_context()} must be called before calling any of the
    #' \code{test_} functions in this package}
    has_context = function() {
      expect_is((ctx), "DBItest_context")
    },

    #' \item{\code{package_dependencies}}{
    #' Can relate the driver to an installed (or devtools-loaded) package;
    #' package depends (!) on "DBI" and imports "methods"}. This test requires
    #' the \code{devtools} package and will be skipped if it is not installed.
    package_dependencies = function() {
      pkg <- get_pkg(ctx)

      pkg_imports <- devtools::parse_deps(pkg$imports)$name
      expect_true("DBI" %in% pkg_imports)
      expect_true("methods" %in% pkg_imports)
    },

    #' \item{\code{package_name}}{
    #' Optional: Package name starts with R.
    #' }
    package_name = function() {
      pkg_name <- package_name(ctx)
      expect_match(pkg_name, "^R")
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite, ctx$name)
}

get_pkg <- function(ctx) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    skip("devtools not installed")
  }

  pkg_name <- package_name(ctx)
  expect_is(pkg_name, "character")

  pkg_path <- find.package(pkg_name)

  devtools::as.package(pkg_path)
}
