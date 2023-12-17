spec_meta_bind <- list(
  bind_return_value = function(ctx, con) {
    check_return_value <- function(bind_res, res) {
      expect_identical(res, bind_res$value)
      expect_false(bind_res$visible)
    }

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

    test_select_bind(
      con,
      ctx,
      1L,
      check_return_value = check_return_value,
      query = FALSE
    )
  },
  #
  bind_too_many = function(ctx, con) {
    patch_bind_values <- function(bind_values) {
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
      bind_values[[2]] <- bind_values[[2]][-1]
      bind_values
    }
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
    patch_bind_values <- function(bind_values) {
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
    patch_bind_values <- function(bind_values) {
      stats::setNames(bind_values, letters[seq_along(bind_values)])
    }
    test_select_bind(
      con,
      ctx,
      1L,
      patch_bind_values = patch_bind_values,
      bind_error = ".*",
      requires_names = FALSE
    )
  },

  #'

  bind_premature_clear = function(ctx, con) {
    is_premature_clear <- TRUE
    test_select_bind(
      con,
      ctx,
      1L,
      is_premature_clear = is_premature_clear,
      bind_error = ".*"
    )
  },

  bind_multi_row = function(ctx, con) {
    test_select_bind(con, ctx, list(1:3))
  },
  #
  bind_multi_row_zero_length = function(ctx, con) {
    test_select_bind(con, ctx, list(integer(), integer()))

    # This behavior is tested as part of run_bind_tester$fun
  },
  #
  bind_multi_row_statement = function(ctx, con) {
    # This behavior is tested as part of run_bind_tester$fun
    test_select_bind(con, ctx, list(1:3), query = FALSE)
  },
  #
  bind_repeated = function(ctx, con) {
    is_repeated <- TRUE

    test_select_bind(con, ctx, 1L, is_repeated = is_repeated)
  },
  #
  bind_repeated_statement = function(ctx, con) {
    is_repeated <- TRUE

    test_select_bind(con, ctx, 1L, is_repeated = is_repeated, query = FALSE)
  },
  #
  bind_repeated_untouched = function(ctx, con) {
    is_repeated <- TRUE
    is_untouched <- TRUE

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
    patch_bind_values <- function(bind_values) {
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
    test_select_bind(con, ctx, c(1:3, NA))
  },

  bind_numeric = function(ctx, con) {
    test_select_bind(con, ctx, c(1:3 + 0.5, NA))
  },

  bind_logical = function(ctx, con) {
    test_select_bind(con, ctx, c(TRUE, FALSE, NA))
  },

  bind_character = function(ctx, con) {
    test_select_bind(con, ctx, c(get_texts(), NA))
  },

  bind_character_escape = if (getRversion() >= "4.0") function(ctx, con) {
    test_select_bind(
      con,
      ctx,
      c(" ", "\n", "\r", "\b", "'", '"', "[", "]", "\\", NA)
    )
  },

  bind_factor = function(ctx, con) {
    test_select_bind(
      con,
      ctx,
      lapply(c(get_texts(), NA_character_), factor),
      warn = TRUE
    )
  },

  bind_date = function(ctx, con) {
    test_select_bind(
      con,
      ctx,
      c(Sys.Date() + 0:2, NA),
      skip_fun = function() !isTRUE(ctx$tweaks$date_typed)
    )
  },

  bind_date_integer = function(ctx, con) {
    test_select_bind(
      con,
      ctx,
      structure(c(18618:18620, NA), class = "Date"),
      skip_fun = function() !isTRUE(ctx$tweaks$date_typed)
    )
  },

  bind_timestamp = function(ctx, con) {
    data_in <- as.POSIXct(c(round(Sys.time()) + 0:2, NA))
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$timestamp_typed)
    )
  },

  bind_timestamp_lt = function(ctx, con) {
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
    data_in <- as.difftime(as.numeric(c(1:3, NA)), units = "secs")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_time_hours = function(ctx, con) {
    data_in <- as.difftime(as.numeric(c(1:3, NA)), units = "hours")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_time_minutes_integer = function(ctx, con) {
    data_in <- as.difftime(c(1:3, NA), units = "mins")
    test_select_bind(
      con,
      ctx,
      data_in,
      skip_fun = function() !isTRUE(ctx$tweaks$time_typed)
    )
  },

  bind_raw = function(ctx, con) {
    test_select_bind(
      con,
      ctx,
      list(list(as.raw(1:10)), list(raw(3)), list(NULL)),
      skip_fun = function() isTRUE(ctx$tweaks$omit_blob_tests),
      cast_fun = ctx$tweaks$blob_cast
    )
  },

  bind_blob = function(ctx, con) {
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
