#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{\code{dbBind("DBIResult")}}{
spec_meta_bind_positional_qm <- list(
  #' Empty positional binding (question mark syntax) with check of
  #' return value.
  bind_empty_positional_qm = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      bind_res <- withVisible(dbBind(res, list()))
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    })
  },

  #' Positional binding of integer values (question mark syntax) raises an
  #' error if connection is closed.
  bind_error_positional_qm = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(test_select_bind(con, positional_qm, 1L))
  },

  #' Positional binding of integer values (question mark syntax) with check of
  #' return value.
  bind_return_value_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1L, extra = "return_value")
    })
  },

  #' Positional binding of integer values (question mark syntax) with too many
  #' values.
  bind_too_many_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1L, extra = "too_many")
    })
  },

  #' Positional binding of integer values (question mark syntax) with too few
  #' values.
  bind_not_enough_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1L, extra = "not_enough")
    })
  },

  #' Positional binding of integer values (question mark syntax), repeated.
  bind_repeated_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1L, extra = "repeated")
    })
  },

  #' Positional binding of integer values (question mark syntax).
  bind_integer_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1L)
    })
  },

  #' Positional binding of numeric values (question mark syntax).
  bind_numeric_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, 1.5)
    })
  },

  #' Positional binding of logical values (question mark syntax).
  bind_logical_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, TRUE)
    })
  },

  #' Positional binding of logical values (coerced to integer, question mark
  #' syntax).
  bind_logical_int_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(
        con, positional_qm, TRUE,
        transform_input = function(x) as.character(as.integer(x)))
    })
  },

  #' Positional binding of \code{NULL} values (question mark syntax).
  bind_null_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(
        con, positional_qm, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' Positional binding of character values (question mark syntax).
  bind_character_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, texts)
    })
  },

  #' Positional binding of date values (question mark syntax).
  bind_date_positional_qm = function(ctx) {
    with_connection({
      test_select_bind(con, positional_qm, Sys.Date())
    })
  },

  #' Positional binding of timestamp values (question mark syntax).
  bind_timestamp_positional_qm = function(ctx) {
    with_connection({
      data_in <- as.POSIXct(round(Sys.time()))
      test_select_bind(
        con, positional_qm, data_in,
        type = dbDataType(con, data_in),
        transform_input = identity,
        transform_output = identity,
        expect = expect_equal)
    })
  },

  #' Positional binding of \code{\link{POSIXlt}} timestamp values
  #' (question mark syntax).
  bind_timestamp_lt_positional_qm = function(ctx) {
    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, positional_qm, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = identity)
    })
  },

  #' Positional binding of raw values (question mark syntax).
  bind_raw_positional_qm = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, positional_qm, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = function(x) x[[1L]],
        transform_output = identity)
    })
  },

  #' }
  NULL
)
