run_tests <- function(tests, skip, test_suite) {
  context(paste("DBItest:", test_suite))

  tests <- tests[!vapply(tests, is.null, logical(1L))]

  lapply(names(tests), function(test_name) {
    test_that(paste0("DBItest: ", test_name), {
      if (test_name %in% skip) {
        skip("by request")
      } else {
        test_fun <- tests[[test_name]]
        test_fun()
      }
    })
  })
}
