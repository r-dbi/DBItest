#' @template dbispec-sub-wip
#' @format NULL
#' @section Parametrised queries and statements:
#' \subsection{`dbBind("DBIResult")`}{
spec_meta_bind <- list(
  #' Empty binding with check of
  #' return value.
  bind_empty = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      bind_res <- withVisible(dbBind(res, list()))
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    })
  },

  #' Binding of integer values raises an
  #' error if connection is closed.
  bind_error = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L))
  },

  #' Binding of integer values with check of
  #' return value.
  bind_return_value = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "return_value")
    })
  },

  #' Binding of integer values with too many
  #' values.
  bind_too_many = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "too_many")
    })
  },

  #' Binding of integer values with too few
  #' values.
  bind_not_enough = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "not_enough")
    })
  },

  #' Binding of integer values, repeated.
  bind_repeated = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "repeated")
    })
  },

  #' Binding of integer values with wrong names.
  bind_wrong_name = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "wrong_name")
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

  #' }
  NULL
)
