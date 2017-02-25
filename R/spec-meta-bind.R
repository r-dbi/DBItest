#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_meta_bind Specification
NULL

#' spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_bind <- list(
  bind_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbBind)), c("res", "params", "..."))
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
      #' for queries issued by [DBI::dbSendQuery()]
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = extra)
    })

    with_connection({
      #' and also for data manipulation statements issued by
      #' [DBI::dbSendStatement()].
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
  #' Binding of integer values, repeated.
  bind_repeated = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "repeated")
    })
  },

  #' Binding of integer values.
  bind_integer = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L)
    })
  },

  #' Binding of numeric values.
  bind_numeric = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1.5)
    })
  },

  #' Binding of logical values.
  bind_logical = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, TRUE)
    })
  },

  #' Binding of logical values (coerced to integer).
  bind_logical_int = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, TRUE,
        transform_input = function(x) as.character(as.integer(x)))
    })
  },

  #' Binding of `NULL` values.
  bind_null = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' Binding of character values.
  bind_character = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, texts)
    })
  },

  #' Binding of date values.
  bind_date = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, Sys.Date())
    })
  },

  #' Binding of [POSIXct] timestamp values.
  bind_timestamp = function(ctx) {
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

  #' Binding of [POSIXlt] timestamp values.
  bind_timestamp_lt = function(ctx) {
    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = identity)
    })
  },

  #' Binding of raw values.
  bind_raw = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = identity,
        transform_output = identity)
    })
  },

  #' Binding of statements.
  bind_statement = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1), query = FALSE)
    })
  },

  #' Repeated binding of statements.
  bind_statement_repeated = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1), query = FALSE, extra = "repeated")
    })
  },

  NULL
)
