#' spec_meta_bind
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_bind <- list(
  bind_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBind)), c("res", "params", "..."))
  },
  #
  bind_return_value = function(ctx, con) {
    #' @return
    check_return_value <- function(bind_res, res) {
      #' `dbBind()` returns the result set,
      expect_identical(res, bind_res$value)
      #' invisibly,
      expect_false(bind_res$visible)
    }

    #' for queries issued by [dbSendQuery()]
    test_select_bind(
      con,
      ctx,
      1L,
      check_return_value = check_return_value
    )
  },
  #
  bind_return_value_statement = function(ctx, con) {
    check_return_value <- function(bind_res, res) {
      expect_identical(res, bind_res$value)
      expect_false(bind_res$visible)
    }

    #' and also for data manipulation statements issued by
    #' [dbSendStatement()].
    test_select_bind(
      con,
      ctx,
      1L,
      check_return_value = check_return_value,
      query = FALSE
    )
  },
  #'
  bind_empty = function(con) {
    #' @section Failure modes:
    #' Calling `dbBind()` for a query without parameters
    res <- local_result(dbSendQuery(con, trivial_query()))
    #' raises an error.
    expect_error(dbBind(res, list()))
  },
  #
  bind_too_many = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' Binding too many
      if (is.null(names(bind_values))) {
        c(bind_values, bind_values[[1L]])
      } else {
        c(bind_values, bogus = bind_values[[1L]])
      }
    }
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*"
    )
  },
  #
  bind_not_enough = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' or not enough values,
      bind_values[-1L]
    }
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*"
    )
  },
  #
  bind_wrong_name = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' or parameters with wrong names
      stats::setNames(bind_values, paste0("bogus", names(bind_values)))
    }
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = TRUE
    )
  },
  #
  bind_multi_row_unequal_length = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' or unequal length,
      bind_values[[2]] <- bind_values[[2]][-1]
      bind_values
    }
    #' also raises an error.
    test_select_bind(
      con,
      ctx,
      list(1:3, 2:4),
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      query = FALSE
    )
  },
  #
  bind_named_param_unnamed_placeholders = function(ctx, con) {
    #' If the placeholders in the query are named,
    patch_bind_values <- function(bind_values) {
      #' all parameter values must have names
      stats::setNames(bind_values, NULL)
    }
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = TRUE
    )
  },
  #
  bind_named_param_empty_placeholders = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' (which must not be empty
      names(bind_values)[[1]] <- ""
      bind_values
    }
    test_select_bind(
      con,
      ctx,
      list(1L, 2L),
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = TRUE
    )
  },
  #
  bind_named_param_na_placeholders = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
      #' or `NA`),
      names(bind_values)[[1]] <- NA
      bind_values
    }
    test_select_bind(
      con,
      ctx,
      list(1L, 2L),
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = TRUE
    )
  },

  bind_unnamed_param_named_placeholders = function(ctx, con) {
    #' and vice versa,
    patch_bind_values <- function(bind_values) {
      stats::setNames(bind_values, letters[seq_along(bind_values)])
    }
    #' otherwise an error is raised.
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = FALSE
    )
  },

  #' The behavior for mixing placeholders of different types
  #' (in particular mixing positional and named placeholders)
  #' is not specified.
  #'

  bind_premature_clear = function(ctx, con) {
    #' Calling `dbBind()` on a result set already cleared by [dbClearResult()]
    is_premature_clear <- TRUE
    #' also raises an error.
    test_select_bind(
      con,
      ctx,
      1L,
      is_premature_clear = is_premature_clear,
      bind_error = ".*"
    )
  },

  bind_multi_row = function(ctx, con) {
    #' @section Specification:
    #' The elements of the `params` argument do not need to be scalars,
    #' vectors of arbitrary length
    test_select_bind(con, ctx, list(1:3))
  },
  #
  bind_multi_row_zero_length = function(ctx, con) {
    #' (including length 0)
    test_select_bind(con, ctx, list(integer(), integer()))

    #' are supported.
    # This behavior is tested as part of run_bind_tester$fun
    #' For queries, calling `dbFetch()` binding such parameters returns
    #' concatenated results, equivalent to binding and fetching for each set
    #' of values and connecting via [rbind()].
  },
  #
  bind_multi_row_statement = function(ctx, con) {
    # This behavior is tested as part of run_bind_tester$fun
    #' For data manipulation statements, `dbGetRowsAffected()` returns the
    #' total number of rows affected if binding non-scalar parameters.
    test_select_bind(con, ctx, list(1:3), query = FALSE)
  },
  #
  bind_repeated = function(ctx, con) {
    #' `dbBind()` also accepts repeated calls on the same result set
    is_repeated <- TRUE

    #' for both queries
    test_select_bind(con, ctx, 1L, is_repeated = is_repeated)
  },
  #
  bind_repeated_statement = function(ctx, con) {
    is_repeated <- TRUE

    #' and data manipulation statements,
    test_select_bind(con, ctx, 1L, is_repeated = is_repeated, query = FALSE)
  },
  #
  bind_repeated_untouched = function(ctx, con) {
    #' even if no results are fetched between calls to `dbBind()`,
    is_repeated <- TRUE
    is_untouched <- TRUE

    #' for both queries
    test_select_bind(
      con,
      ctx,
      1L,
      is_repeated = is_repeated,
      is_untouched = is_untouched
    )
  },
  #
  bind_repeated_untouched_statement = function(ctx, con) {
    is_repeated <- TRUE
    is_untouched <- TRUE

    #' and data manipulation statements.
    test_select_bind(
      con,
      ctx,
      1L,
      is_repeated = is_repeated,
      is_untouched = is_untouched,
      query = FALSE
    )
  },

  #'
  bind_named_param_shuffle = function(ctx, con) {
    #' If the placeholders in the query are named,
    patch_bind_values <- function(bind_values) {
      #' their order in the `params` argument is not important.
      bind_values[c(3, 1, 2, 4)]
    }
    test_select_bind(
      con,
      ctx,
      c(1:3 + 0.5, NA),
      patch_bind_values = patch_bind_values,
      requires_names = TRUE
    )
  },

  #'
  bind_integer = function(ctx, con) {
    #' At least the following data types are accepted on input (including [NA]):
    #' - [integer]
    test_select_bind(con, ctx, c(1:3, NA))
  },

  bind_numeric = function(ctx, con) {
    #' - [numeric]
    test_select_bind(con, ctx, c(1:3 + 0.5, NA))
  },

  bind_logical = function(ctx, con) {
    #' - [logical] for Boolean values
    test_select_bind(con, ctx, c(TRUE, FALSE, NA))
  },

  bind_character = function(ctx, con) {
    #' - [character]
    test_select_bind(con, ctx, c(get_texts(), NA))
  },

  bind_character_escape = if (getRversion() >= "4.0") function(ctx, con) {
    #'   (also with special characters such as spaces, newlines, quotes, and backslashes)
    test_select_bind(
      con,
      ctx,
      c(" ", "\n", "\r", "\b", "'", '"', "[", "]", "\\", NA)
    )
  },

  bind_factor = function(ctx, con) {
    #' - [factor] (bound as character,
    #' with warning)
    suppressWarnings(expect_warning(
      test_select_bind(
        con,
        ctx,
        lapply(c(get_texts(), NA_character_), factor)
      )
    ))
  },

  bind_date = function(ctx, con) {
    #' - [Date]
    test_select_bind(
      con,
      ctx,
      c(Sys.Date() + 0:2, NA),
      skip_fun = function() !isTRUE(ctx$tweaks$date_typed)
    )
  },

  bind_date_integer = function(ctx, con) {
    #'   (also when stored internally as integer)
    test_select_bind(
      con,
      ctx,
      structure(c(18618:18620, NA), class = "Date"),
      skip_fun = function() !isTRUE(ctx$tweaks$date_typed)
    )
  },

  bind_timestamp = function(ctx, con) {
    #' - [POSIXct] timestamps
    data_in <- as.POSIXct(c(round(Sys.time()) + 0:2, NA))
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$timestamp_typed)
    )
  },

  bind_timestamp_lt = function(ctx, con) {
    #' - [POSIXlt] timestamps
    data_in <- lapply(
      round(Sys.time()) + c(0:2, NA),
      as.POSIXlt
    )
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$timestamp_typed)
    )
  },

  bind_time_seconds = function(ctx, con) {
    #' - [difftime] values
    data_in <- as.difftime(as.numeric(c(1:3, NA)), units = "secs")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_time_hours = function(ctx, con) {
    #'   (also with units other than seconds
    data_in <- as.difftime(as.numeric(c(1:3, NA)), units = "hours")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_time_minutes_integer = function(ctx, con) {
    #'   and with the value stored as integer)
    data_in <- as.difftime(c(1:3, NA), units = "mins")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_raw = function(ctx, con) {
    #' - lists of [raw] for blobs (with `NULL` entries for SQL NULL values)
    test_select_bind(
      con,
      ctx,
      list(list(as.raw(1:10)), list(raw(3)), list(NULL)),
      skip_fun = function() isTRUE(ctx$tweaks$omit_blob_tests),
      cast_fun = ctx$tweaks$blob_cast
    )
  },

  bind_blob = function(ctx, con) {
    #' - objects of type [blob::blob]
    test_select_bind(
      con,
      ctx,
      list(blob::blob(as.raw(1:10)), blob::blob(raw(3)), blob::blob(NULL)),
      skip_fun = function() isTRUE(ctx$tweaks$omit_blob_tests),
      cast_fun = ctx$tweaks$blob_cast
    )
  },
  #
  NULL
)
