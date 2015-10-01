run_tests <- function(tests, skip, test_suite) {
  tests <- tests[!names(tests) %in% skip]

  context(paste("DBItest:", test_suite))
  lapply(names(tests), function(test_name) {
    test_that(paste0("DBItest: ", test_name), {
      tests[[test_name]]()
    })
  })
}
