#' spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_meta_bind <- list(
  bind_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBind)), c("res", "params", "..."))
  },

  #' @return
  bind_return_value = function(ctx, con) {
    extra <- new_bind_tester_extra(
      check_return_value = function(bind_res, res) {
        #' `dbBind()` returns the result set,
        expect_identical(res, bind_res$value)
        #' invisibly,
        expect_false(bind_res$visible)
      }
    )

    #' for queries issued by [dbSendQuery()]
    test_select_bind(con, ctx, 1L, extra = extra)
    #' and also for data manipulation statements issued by
    #' [dbSendStatement()].
    test_select_bind(con, ctx, 1L, extra = extra, query = FALSE)
  },
  #
  bind_empty = function(con) {
    with_result(
      #' Calling `dbBind()` for a query without parameters
      dbSendQuery(con, trivial_query()),
      #' raises an error.
      expect_error(dbBind(res, list()))
    )
  },
  #
  bind_too_many = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' Binding too many
        if (is.null(names(bind_values))) {
          c(bind_values, bind_values[[1L]])
        } else {
          c(bind_values, bogus = bind_values[[1L]])
        }
      },
      bind_error = function() ".*"
    )
    test_select_bind(con, ctx, 1L, extra = extra)
  },
  #
  bind_not_enough = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or not enough values,
        bind_values[-1L]
      },
      bind_error = function() ".*"
    )
    test_select_bind(con, ctx, 1L, extra = extra)
  },
  #
  bind_wrong_name = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or parameters with wrong names
        stats::setNames(bind_values, paste0("bogus", names(bind_values)))
      },
      #
      requires_names = function() TRUE,
      bind_error = function() ".*"
    )
    test_select_bind(con, ctx, 1L, extra = extra)
  },
  #
  bind_multi_row_unequal_length = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or unequal length,
        bind_values[[2]] <- bind_values[[2]][-1]
        bind_values
      },
      bind_error = function() ".*"
    )
    #' also raises an error.
    test_select_bind(
      con, ctx, list(1:3, 2:4),
      extra = extra, query = FALSE
    )
  },

  #' If the placeholders in the query are named,
  bind_named_param_unnamed_placeholders = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' all parameter values must have names
        stats::setNames(bind_values, NULL)
      },
      bind_error = function() ".*",
      #
      requires_names = function() TRUE
    )
    test_select_bind(con, ctx, 1L, extra = extra)
  },
  #
  bind_named_param_empty_placeholders = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' (which must not be empty
        names(bind_values)[[1]] <- ""
      },
      bind_error = function() ".*",
      #
      requires_names = function() TRUE
    )
    test_select_bind(con, ctx, list(1L, 2L), extra = extra)
  },
  #
  bind_named_param_na_placeholders = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' or `NA`),
        names(bind_values)[[1]] <- NA
      },
      bind_error = function() ".*",
      #
      requires_names = function() TRUE
    )
    test_select_bind(con, ctx, list(1L, 2L), extra = extra)
  },

  #' and vice versa,
  bind_unnamed_param_named_placeholders = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        stats::setNames(bind_values, letters[seq_along(bind_values)])
      },
      bind_error = function() ".*",
      #
      requires_names = function() FALSE
    )
    #' otherwise an error is raised.
    test_select_bind(con, ctx, 1L, extra = extra)
  },

  #' The behavior for mixing placeholders of different types
  #' (in particular mixing positional and named placeholders)
  #' is not specified.
  #'

  bind_premature_clear = function(ctx, con) {
    extra <- new_bind_tester_extra(
      #' Calling `dbBind()` on a result set already cleared by [dbClearResult()]
      is_premature_clear = function() TRUE
    )
    #' also raises an error.
    expect_error(
      test_select_bind(con, ctx, 1L, extra = extra)
    )
  },

  #' @section Specification:
  #' The elements of the `params` argument do not need to be scalars,
  bind_multi_row = function(ctx, con) {
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
    extra <- new_bind_tester_extra(
      #' `dbBind()` also accepts repeated calls on the same result set
      is_repeated = function() TRUE
    )

    #' for both queries
    test_select_bind(con, ctx, 1L, extra = extra)
    #' and data manipulation statements,
    test_select_bind(con, ctx, 1L, extra = extra, query = FALSE)
  },
  #
  bind_repeated_untouched = function(ctx, con) {
    extra <- new_bind_tester_extra(
      #' even if no results are fetched between calls to `dbBind()`,
      is_repeated = function() TRUE,
      is_untouched = function() TRUE
    )

    #' for both queries
    test_select_bind(con, ctx, 1L, extra = extra)
    #' and data manipulation statements.
    test_select_bind(con, ctx, 1L, extra = extra, query = FALSE)
  },

  #'
  #' If the placeholders in the query are named,
  bind_named_param_shuffle = function(ctx, con) {
    extra <- new_bind_tester_extra(
      patch_bind_values = function(bind_values) {
        #' their order in the `params` argument is not important.
        bind_values[c(3, 1, 2, 4)]
      },
      #
      requires_names = function() TRUE
    )
    test_select_bind(con, ctx, c(1:3 + 0.5, NA), extra = extra)
  },

  #'
  #' At least the following data types are accepted on input (including [NA]):
  #' - [integer]
  bind_integer = function(ctx, con) {
    test_select_bind(con, ctx, c(1:3, NA))
  },

  #' - [numeric]
  bind_numeric = function(ctx, con) {
    test_select_bind(con, ctx, c(1:3 + 0.5, NA))
  },

  #' - [logical] for Boolean values
  bind_logical = function(ctx, con) {
    test_select_bind(con, ctx, c(TRUE, FALSE, NA))
  },

  #' - [character]
  bind_character = function(ctx, con) {
    test_select_bind(con, ctx, c(texts, NA))
  },

  #' - [factor] (bound as character,
  bind_factor = function(ctx, con) {
    #' with warning)
    expect_warning(
      test_select_bind(
        con,
        ctx,
        lapply(c(texts, NA_character_), factor)
      )
    )
  },

  #' - [Date]
  bind_date = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    test_select_bind(con, ctx, c(Sys.Date() + 0:2, NA))
  },

  #'   (also when stored internally as integer)
  bind_date_integer = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    test_select_bind(con, ctx, structure(c(18618:18620, NA), class = "Date"))
  },

  #' - [POSIXct] timestamps
  bind_timestamp = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    data_in <- as.POSIXct(c(round(Sys.time()) + 0:2, NA))
    test_select_bind(con, ctx, data_in)
  },

  #' - [POSIXlt] timestamps
  bind_timestamp_lt = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    data_in <- lapply(
      round(Sys.time()) + c(0:2, NA),
      as.POSIXlt
    )
    test_select_bind(con, ctx, data_in)
  },

  #' - [difftime] values
  bind_time_seconds = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }

    data_in <- as.difftime(c(1:3, NA), units = "secs")
    test_select_bind(con, ctx, data_in)
  },

  #'   (also with units other than seconds)
  bind_time_hours = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }

    data_in <- as.difftime(c(1:3, NA), units = "hours")
    test_select_bind(con, ctx, data_in)
  },

  #' - lists of [raw] for blobs (with `NULL` entries for SQL NULL values)
  bind_raw = function(ctx, con) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    test_select_bind(
      con, ctx,
      list(list(as.raw(1:10)), list(raw(3)), list(NULL)),
      cast_fun = ctx$tweaks$blob_cast
    )
  },

  #' - objects of type [blob::blob]
  bind_blob = function(ctx, con) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    test_select_bind(
      con, ctx,
      list(blob::blob(as.raw(1:10)), blob::blob(raw(3)), blob::blob(NULL)),
      cast_fun = ctx$tweaks$blob_cast
    )
  },
  #
  NULL
)
