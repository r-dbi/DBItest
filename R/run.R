run_tests <- function(tests, skip) {
  tests <- tests[!names(tests) %in% skip]
  lapply(tests, do.call)
}
