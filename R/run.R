run_tests <- function(tests, skip) {
  tests <- tests[!names(tests) %in% skip]
  lapply(names(tests), function(test_name) {
    test_that(paste0("DBItest: ", test_name), {
      tests[[test_name]]()
    })
  })
}
