run_tests <- function(tests, skip, test_suite, ctx_name) {
  test_context <- paste0(
    "DBItest", if(!is.null(ctx_name)) paste0("[", ctx_name, "]"),
    ": ", test_suite)
  context(test_context)

  tests <- tests[!vapply(tests, is.null, logical(1L))]

  skip_rx <- paste0(paste0("(?:^", skip, "$)"), collapse = "|")

  vapply(names(tests), function(test_name) {
    ok <- test_that(paste0(test_context, ": ", test_name), {
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
