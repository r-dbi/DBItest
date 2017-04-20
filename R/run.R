run_tests <- function(ctx, tests, skip, test_suite) {
  if (is.null(ctx)) {
    stop("Need to call make_context() to use the test_...() functions.", call. = FALSE)
  }
  if (!inherits(ctx, "DBItest_context")) {
    stop("ctx must be a DBItest_context object created by make_context().", call. = FALSE)
  }

  test_context <- paste0(
    "DBItest", if(!is.null(ctx$name)) paste0("[", ctx$name, "]"),
    ": ", test_suite)
  context(test_context)

  tests <- tests[!vapply(tests, is.null, logical(1L))]

  skip_rx <- paste0(paste0("(?:^", skip, "$)"), collapse = "|")
  skip_flag <- grepl(skip_rx, names(tests), perl = TRUE)

  ok <- vapply(seq_along(tests), function(test_idx) {
    test_name <- names(tests)[[test_idx]]
    if (skip_flag[[test_idx]])
      FALSE
    else {
      test_fun <- patch_test_fun(tests[[test_name]], paste0(test_context, ": ", test_name))
      test_fun(ctx)
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

patch_test_fun <- function(test_fun, desc) {
  body_of_test_fun <- body(test_fun)
  eval(bquote(
    function(ctx) {
      test_that(.(desc), .(body_of_test_fun))
    }
  ))
}
