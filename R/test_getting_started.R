#' Test runners
#'
#' These functions test various aspects of a DBI backend.
#'
#' @param skip \code{[character()]}\cr A list of test names to skip
#' @param ctx \code{[DBItest_context]}\cr A test context as created by
#'   \code{\link{make_context}}.
#'
#' \code{test_getting_started()} tests if \code{make_context} has been called,
#'   and if the DBI driver package has correct dependencies.
#'
#' @rdname test
#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  tests <- list(
    # make_context() must be called before calling any of the test_ functions
    # in this package
    has_context = function() {
      expect_is(ctx, "DBItest_context")
      expect_is(ctx$drv, "DBIDriver")
    },

    # can relate driver to an installed (or devtools-loaded) package;
    # package depends (!) on "DBI" and imports "methods"
    package_dependencies = function() {
      expect_is(pkg_name <- attr(class(ctx$drv), "package"), "character")

      pkg_path <- find.package(pkg_name)

      pkg <- devtools::as.package(pkg_path)
      pkg_depends <- devtools::parse_deps(pkg$depends)$name
      expect_true("DBI" %in% pkg_depends)
      pkg_imports <- devtools::parse_deps(pkg$imports)$name
      expect_true("methods" %in% pkg_imports)
    }
  )
  run_tests(tests, skip, test_suite)
}
