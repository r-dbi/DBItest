#' spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_bind <- list(
  bind_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBind)), c("res", "params", "..."))
  },

  #' @return
  bind_return_value = function(ctx) {
    extra <- new_bind_tester_extra(
      check_return_value = function(bind_res, res) {
        #' `dbBind()` returns the result set,
        expect_identical(res, bind_res$value)
        #' invisibly,
        expect_false(bind_res$visible)
      }
    )

    with_connection({
      #' for queries issued by [dbSendQuery()]
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
    })

    with_connection({
      #' and also for data manipulation statements issued by
      #' [dbSendStatement()].
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra, query = FALSE)
    })
  },

  bind_empty = function(ctx) {
    with_connection({
      with_result(
        #' Calling `dbBind()` for a query without parameters
        dbSendQuery(con, "SELECT 1"),
        #' raises an error.
        expect_error(dbBind(res, list()))
      )
    })
  },

  bind_too_many = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' Binding too many
        c(bind_values, bind_values[[1L]])
      }
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  bind_not_enough = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or not enough values,
        bind_values[-1L]
      }
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  bind_wrong_name = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or parameters with wrong names
        stats::setNames(bind_values, paste0("bogus", names(bind_values)))
      },

      requires_names = function() TRUE
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  bind_multi_row_unequal_length = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or unequal length,
        bind_values[[2]] <- bind_values[[2]][-1]
        bind_values
      }
    )
    with_connection({
      #' also raises an error.
      expect_error(
        test_select_bind(
          con, ctx$tweaks$placeholder_pattern, list(1:3, 2:4),
          extra = extra, query = FALSE
        )
      )
    })
  },

  #' If the placeholders in the query are named,
  bind_named_param_unnamed_placeholders = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' all parameter values must have names
        stats::setNames(bind_values, NULL)
      },

      requires_names = function() TRUE
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  bind_named_param_empty_placeholders = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' (which must not be empty
        names(bind_values)[[1]] <- ""
      },

      requires_names = function() TRUE
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1L, 2L), extra = extra)
      )
    })
  },

  bind_named_param_na_placeholders = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or `NA`),
        names(bind_values)[[1]] <- NA
      },

      requires_names = function() TRUE
    )
    with_connection({
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1L, 2L), extra = extra)
      )
    })
  },

  #' and vice versa,
  bind_unnamed_param_named_placeholders = function(ctx) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        stats::setNames(bind_values, letters[seq_along(bind_values)])
      },

      requires_names = function() FALSE
    )
    with_connection({
      #' otherwise an error is raised.
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  #' The behavior for mixing placeholders of different types
  #' (in particular mixing positional and named placeholders)
  #' is not specified.
  #'

  bind_premature_clear = function(ctx) {
    extra <- new_bind_tester_extra(
      #' Calling `dbBind()` on a result set already cleared by [dbClearResult()]
      is_premature_clear = function() TRUE
    )
    with_connection({
      #' also raises an error.
      expect_error(
        test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
      )
    })
  },

  #' @section Specification:
  #' The elements of the `params` argument do not need to be scalars,
  bind_multi_row = function(ctx) {
    with_connection({
      #' vectors of arbitrary length
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3))
    })
  },

  bind_multi_row_zero_length = function(ctx) {
    with_connection({
      #' (including length 0)
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(integer(), integer()))
    })

    #' are supported.
    # This behavior is tested as part of run_bind_tester$fun
    #' For queries, calling `dbFetch()` binding such parameters returns
    #' concatenated results, equivalent to binding and fetching for each set
    #' of values and connecting via [rbind()].
  },

  bind_multi_row_statement = function(ctx) {
    with_connection({
      # This behavior is tested as part of run_bind_tester$fun
      #' For data manipulation statements, `dbGetRowsAffected()` returns the
      #' total number of rows affected if binding non-scalar parameters.
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3), query = FALSE)
    })
  },

  bind_repeated = function(ctx) {
    extra <- new_bind_tester_extra(
      #' `dbBind()` also accepts repeated calls on the same result set
      is_repeated = function() TRUE
    )

    with_connection({
      #' for both queries
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
    })

    with_connection({
      #' and data manipulation statements,
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra, query = FALSE)
    })
  },

  bind_repeated_untouched = function(ctx) {
    extra <- new_bind_tester_extra(
      #' even if no results are fetched between calls to `dbBind()`.
      is_repeated = function() TRUE,
      is_untouched = function() TRUE
    )

    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
    })

    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra, query = FALSE)
    })
  },

  #'
  #' At least the following data types are accepted:
  #' - [integer]
  bind_integer = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L)
    })
  },

  #' - [numeric]
  bind_numeric = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1.5)
    })
  },

  #' - [logical] for Boolean values (some backends may return an integer)
  bind_logical = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, TRUE,
        type = NULL,
        transform_input = ctx$tweaks$logical_return,
        transform_output = ctx$tweaks$logical_return
      )
    })
  },

  #' - [NA]
  bind_null = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' - [character]
  bind_character = function(ctx) {
    with_connection({
      test_select_bind(
        con,
        ctx$tweaks$placeholder_pattern,
        texts
      )
    })
  },

  #' - [factor] (bound as character,
  bind_factor = function(ctx) {
    with_connection({
      #' with warning)
      expect_warning(
        test_select_bind(
          con,
          ctx$tweaks$placeholder_pattern,
          lapply(texts, factor)
        )
      )
    })
  },

  #' - [Date]
  bind_date = function(ctx) {
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, Sys.Date())
    })
  },

  #' - [POSIXct] timestamps
  bind_timestamp = function(ctx) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    with_connection({
      data_in <- as.POSIXct(round(Sys.time()))
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, data_in,
        type = dbDataType(con, data_in),
        transform_input = identity,
        transform_output = identity,
        expect = expect_equal)
    })
  },

  #' - [POSIXlt] timestamps
  bind_timestamp_lt = function(ctx) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = as.POSIXct)
    })
  },

  #' - lists of [raw] for blobs (with `NULL` entries for SQL NULL values)
  bind_raw = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = blob::as.blob,
        transform_output = blob::as.blob)
    })
  },

  #' - objects of type [blob::blob]
  bind_blob = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, list(blob::blob(as.raw(1:10))),
        type = NULL,
        transform_input = identity,
        transform_output = blob::as.blob)
    })
  },

  NULL
)
