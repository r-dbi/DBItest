#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  tests <- list(
    package_dependencies = function() {
      expect_true(TRUE)
    }
  )
  run_tests(tests, skip)
}
