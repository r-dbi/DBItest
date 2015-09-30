#' @export
test_getting_started <- function(skip = NULL, ctx = get_default_context()) {
  tests <- list(
    has_context = function() {
      expect_true(
        !is.null(ctx),
        label = "Please call make_context() before using any of the tests in DBItest.")
      expect_true(!is.null(ctx$drv))
    },

    package_dependencies = function() {
      expect_true(TRUE)
    }
  )
  run_tests(tests, skip)
}
