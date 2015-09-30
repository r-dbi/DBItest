#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  tests <- list(
    has_context = function() {
      expect_true(
        !is.null(ctx),
        info = "Please call make_context() before using any of the tests in DBItest.")
      expect_true(!is.null(ctx$drv))
    },

    package_dependencies = function() {
      pkg_name <- attr(class(ctx$drv), "package")

      expect_that(
        pkg_path <- find.package(pkg_name), not(throws_error()),
        info = "Please ensure that your driver is an S4 class defined in your package.")

      pkg <- devtools::as.package(pkg_path)
      depends <- devtools::parse_deps(pkg$depends)
      expect_true(
        "DBI" %in% depends$name,
        info = "Please include DBI in the 'Depends' section of your DESCRIPTION."
      )
      imports <- devtools::parse_deps(pkg$imports)
      expect_true(
        "methods" %in% imports$name,
        info = "Please include methods in the 'Imports' section of your DESCRIPTION."
      )
    }
  )
  run_tests(tests, skip)
}
