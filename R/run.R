run_tests <- function(ctx, tests, skip, test_suite, ctx_name) {
  test_context <- paste0(
    "DBItest", if(!is.null(ctx_name)) paste0("[", ctx_name, "]"),
    ": ", test_suite)
  context(test_context)

  tests <- tests[!vapply(tests, is.null, logical(1L))]

  skip_rx <- paste0(paste0("(?:^", skip, "$)"), collapse = "|")
  skip_flag <- grepl(skip_rx, names(tests))

  ok <- vapply(seq_along(tests), function(test_idx) {
    test_name <- names(tests)[[test_idx]]
    if (skip_flag[[test_idx]])
      FALSE
    else {
      test_that(paste0(test_context, ": ", test_name), {
        test_fun <- tests[[test_name]]
        test_fun(ctx)
      })
    }
  },
  logical(1L))

  if (any(skip_flag)) {
    test_that(paste0(test_context, ": skipped tests"), {
      skip(paste0("by request: ", paste(names(tests)[skip_flag], collapse = ", ")))
    })
  }

  ok
}
