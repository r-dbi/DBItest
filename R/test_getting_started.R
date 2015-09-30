test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  tests <- list(
    package_dependencies = function() {
    }
  )
  run_tests(tests, skip)
}
