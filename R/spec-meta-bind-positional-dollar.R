#' @template dbispec-sub
#' @section Meta:
spec_meta_bind_positional_dollar <- list(
  #' Empty positional binding (dollar syntax) with check of
  #' return value.
  bind_empty_positional_dollar = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      bind_res <- withVisible(dbBind(res, list()))
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    })
  },

  #' Positional binding of integer values (dollar syntax) raises an
  #' error if connection is closed.
  bind_error_positional_dollar = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(test_select_bind(con, positional_dollar, 1L))
  },

  #' Positional binding of integer values (dollar syntax) with check of
  #' return value.
  bind_return_value_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1L, extra = "return_value")
    })
  },

  #' Positional binding of integer values (dollar syntax) with too many
  #' values.
  bind_too_many_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1L, extra = "too_many")
    })
  },

  #' Positional binding of integer values (dollar syntax) with too few
  #' values.
  bind_not_enough_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1L, extra = "not_enough")
    })
  },

  #' Positional binding of integer values (dollar syntax), repeated.
  bind_repeated_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1L, extra = "repeated")
    })
  },

  #' Positional binding of integer values (dollar syntax).
  bind_integer_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1L)
    })
  },

  #' Positional binding of numeric values (dollar syntax).
  bind_numeric_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, 1.5)
    })
  },

  #' Positional binding of logical values (dollar syntax).
  bind_logical_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, TRUE)
    })
  },

  #' Positional binding of logical values (coerced to integer, dollar
  #' syntax).
  bind_logical_int_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(
        con, positional_dollar, TRUE,
        transform_input = function(x) as.character(as.integer(x)))
    })
  },

  #' Positional binding of \code{NULL} values (dollar syntax).
  bind_null_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(
        con, positional_dollar, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' Positional binding of character values (dollar syntax).
  bind_character_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, texts)
    })
  },

  #' Positional binding of date values (dollar syntax).
  bind_date_positional_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, positional_dollar, Sys.Date())
    })
  },

  #' Positional binding of timestamp values (dollar syntax).
  bind_timestamp_positional_dollar = function(ctx) {
    with_connection({
      data_in <- as.POSIXct(round(Sys.time()))
      test_select_bind(
        con, positional_dollar, data_in,
        type = dbDataType(con, data_in),
        transform_input = identity,
        transform_output = identity,
        expect = expect_equal)
    })
  },

  #' Positional binding of \code{\link{POSIXlt}} timestamp values (dollar
  #' syntax).
  bind_timestamp_lt_positional_dollar = function(ctx) {
    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, positional_dollar, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = identity)
    })
  },

  #' Positional binding of raw values (dollar syntax).
  bind_raw_positional_dollar = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, positional_dollar, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = function(x) x[[1L]],
        transform_output = identity)
    })
  },

  NULL
)
