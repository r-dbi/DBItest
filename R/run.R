# Core test runner that executes DBItest specifications
# 
# This function is the main test execution engine for DBItest. It takes a set of
# test specifications, applies filtering (skip/run_only), and executes each test
# with the appropriate database context and test fixtures (connections, tables).
# 
# @param ctx DBItest_context object created by make_context()
# @param tests Named list of test functions to execute
# @param skip Character vector of test name patterns to skip
# @param run_only Character vector of test name patterns to run exclusively
# @param test_suite Name of the test suite for context labeling
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
      skip(paste0("DBItest::run_tests(): by request: ", toString(names(tests)[skip_flag])))
    })
  }

  # to isolate test topics
  gc()

  ok
}

# Resolve skip patterns to actual test names
#
# Converts skip patterns (e.g., "roundtrip", "bind_*") into concrete test names
# by matching against all available test specifications. Uses regex patterns
# to support flexible test filtering and warns about unused skip expressions.
#
# @param skip Character vector of patterns to match against test names
# @return Character vector of resolved test names to skip
get_skip_names <- function(skip) {
  if (length(skip) == 0L) {
    return(character())
  }
  names_all <- names(spec_all)
  names_all <- names_all[names_all != ""]
  skip_flags_all <- purrr::map(paste0("(?:^(?:", skip, ")(?:|_[0-9]+)$)"), grepl, names_all, perl = TRUE)
  skip_used <- purrr::map_lgl(skip_flags_all, any)
  if (!all(skip_used)) {
    warning("These skip expressions did not match any test names: ", toString(skip[!skip_used]),
      call. = FALSE
    )
  }

  skip_flag_all <- Reduce(`|`, skip_flags_all)
  skip_tests <- names_all[skip_flag_all]

  skip_tests
}

# Filter tests to run only those matching specified patterns
#
# Takes a test suite and filters it to include only tests whose names match
# the run_only patterns. If run_only is NULL, returns all tests unchanged.
# This allows running specific subsets of tests for focused testing.
#
# @param tests Named list of test functions
# @param run_only Character vector of patterns to match test names, or NULL
# @return Filtered named list containing only matching tests
get_run_only_tests <- function(tests, run_only) {
  names_all <- names(tests)
  names_all <- names_all[names_all != ""]
  if (is.null(run_only)) {
    return(tests)
  }

  run_only_flags_all <- purrr::map(paste0("(?:^(?:", run_only, ")$)"), grepl, names_all, perl = TRUE)
  run_only_flag_all <- Reduce(`|`, run_only_flags_all)
  run_only_tests <- names_all[run_only_flag_all]

  tests[run_only_tests]
}
