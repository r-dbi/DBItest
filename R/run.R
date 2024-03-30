run_tests <- function(ctx, tests, skip, run_only, test_suite) {
  tests_sym <- enexpr(tests)
  stopifnot(is_symbol(tests_sym))
  tests_qual <- call2(":::", sym("DBItest"), tests_sym)

  "!DEBUG run_tests(`test_suite`)"

  if (is.null(ctx)) {
    stop("Need to call make_context() to use the test_...() functions.", call. = FALSE)
  }
  if (!inherits(ctx, "DBItest_context")) {
    stop("ctx must be a DBItest_context object created by make_context().", call. = FALSE)
  }

  test_context <- paste0(
    "DBItest", if (!is.null(ctx$name)) paste0("[", ctx$name, "]"),
    ": ", test_suite
  )

  tests <- compact(tests)
  tests <- get_run_only_tests(tests, run_only)

  if (is.null(skip)) {
    skip <- ctx$default_skip
  }

  test_names <- names(tests)

  skipped <- get_skip_names(skip)
  skip_flag <- names(tests) %in% skipped

  if (length(tests) > 0) {
    global_con <- local_connection(ctx)
  }

  ok <- vapply(
    seq_along(tests),
    function(test_idx) {
      test_name <- test_names[[test_idx]]
      if (skip_flag[[test_idx]]) {
        FALSE
      } else {
        test_fun <- tests[[test_idx]]
        fmls <- formals(test_fun)

        test_that(paste0(test_context, ": ", test_name), {
          args <- list()
          if ("ctx" %in% names(fmls)) {
            args <- c(args, list(ctx = expr(ctx)))
          }

          if ("con" %in% names(fmls)) {
            args <- c(args, list(con = expr(global_con)))
          }

          if ("local_con" %in% names(fmls)) {
            local_con <- local_connection(ctx)
            args <- c(args, list(local_con = expr(local_con)))
          }

          if ("closed_con" %in% names(fmls)) {
            closed_con <- local_closed_connection(ctx)
            args <- c(args, list(closed_con = expr(closed_con)))
          }

          if ("invalid_con" %in% names(fmls)) {
            invalid_con <- local_invalid_connection(ctx)
            args <- c(args, list(invalid_con = expr(invalid_con)))
          }

          if ("table_name" %in% names(fmls)) {
            if (is_missing(fmls$table_name)) {
              table_name <- random_table_name()
            } else {
              table_name <- fmls$table_name
            }
            local_remove_test_table(global_con, table_name)
            args <- c(args, list(table_name = table_name))
          }

          # Example of generated expression:
          # DBItest:::spec_arrow$arrow_append_table_arrow_roundtrip_64_bit_roundtrip(...)
          test_fun_expr <- expr(`$`(!!tests_qual, !!test_name)(!!!args))
          expect_warning(
            eval(test_fun_expr), NA
          )
        })
      }
    },
    logical(1L)
  )

  if (any(skip_flag)) {
    test_that(paste0(test_context, ": skipped tests"), {
      skip(paste0("DBItest::run_tests(): by request: ", paste(names(tests)[skip_flag], collapse = ", ")))
    })
  }

  # to isolate test topics
  gc()

  ok
}

get_skip_names <- function(skip) {
  if (length(skip) == 0L) {
    return(character())
  }
  names_all <- names(spec_all)
  names_all <- names_all[names_all != ""]
  skip_flags_all <- map(paste0("(?:^(?:", skip, ")(?:|_[0-9]+)$)"), grepl, names_all, perl = TRUE)
  skip_used <- map_lgl(skip_flags_all, any)
  if (!all(skip_used)) {
    warning("Unused skip expressions: ", paste(skip[!skip_used], collapse = ", "),
      call. = FALSE
    )
  }

  skip_flag_all <- Reduce(`|`, skip_flags_all)
  skip_tests <- names_all[skip_flag_all]

  skip_tests
}

get_run_only_tests <- function(tests, run_only) {
  names_all <- names(tests)
  names_all <- names_all[names_all != ""]
  if (is.null(run_only)) {
    return(tests)
  }

  run_only_flags_all <- map(paste0("(?:^(?:", run_only, ")$)"), grepl, names_all, perl = TRUE)
  run_only_flag_all <- Reduce(`|`, run_only_flags_all)
  run_only_tests <- names_all[run_only_flag_all]

  tests[run_only_tests]
}
