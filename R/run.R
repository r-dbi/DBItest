run_tests <- function(tests, skip, test_suite) {
  context(paste("DBItest:", test_suite))

  tests <- tests[!vapply(tests, is.null, logical(1L))]

  skip_rx <- paste0(paste0("(?:^", skip, "$)"), collapse = "|")

  vapply(names(tests), function(test_name) {
    ok <- test_that(paste0("DBItest: ", test_name), {
      if (grepl(skip_rx, test_name, perl = TRUE)) {
        skip("by request")
      } else {
        test_fun <- tests[[test_name]]
        test_fun()
      }
    })

    # Workaround until hadley/testthat#360 is merged.
    if (!is.logical(ok)) {
      ok <- TRUE
    }

    ok
  },
  logical(1L))
}
