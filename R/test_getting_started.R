#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Getting started"

  tests <- list(
    has_context = function() {
      expect_is(ctx, "DBItest_context")
      expect_is(ctx$drv, "DBIDriver")
    },

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
