#' @template dbispec-sub
#' @section Meta:
spec_meta_bind_named_colon <- list(
  #' Empty named binding (colon syntax) with check of
  #' return value.
  bind_empty_named_colon = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      bind_res <- withVisible(dbBind(res, list()))
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    })
  },

  #' Named binding of integer values (colon syntax) raises an
  #' error if connection is closed.
  bind_error_named_colon = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(test_select_bind(con, named_colon, 1L))
  },

  #' Named binding of integer values (colon syntax) with check of
  #' return value.
  bind_return_value_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "return_value")
    })
  },

  #' Named binding of integer values (colon syntax) with too many
  #' values.
  bind_too_many_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "too_many")
    })
  },

  #' Named binding of integer values (colon syntax) with too few
  #' values.
  bind_not_enough_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "not_enough")
    })
  },

  #' Named binding of integer values (colon syntax) with wrong names.
  bind_wrong_name_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "wrong_name")
    })
  },

  #' Named binding of integer values (colon syntax), repeated.
  bind_repeated_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "repeated")
    })
  },

  #' Named binding of integer values (colon syntax).
  bind_integer_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L)
    })
  },

  #' Named binding of numeric values (colon syntax).
  bind_numeric_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1.5)
    })
  },

  #' Named binding of logical values (colon syntax).
  bind_logical_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, TRUE)
    })
  },

  #' Named binding of logical values (coerced to integer, colon
  #' syntax).
  bind_logical_int_named_colon = function(ctx) {
    with_connection({
      test_select_bind(
        con, named_colon, TRUE,
        transform_input = function(x) as.character(as.integer(x)))
    })
  },

  #' Named binding of \code{NULL} values (colon syntax).
  bind_null_named_colon = function(ctx) {
    with_connection({
      test_select_bind(
        con, named_colon, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' Named binding of character values (colon syntax).
  bind_character_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, texts)
    })
  },

  #' Named binding of date values (colon syntax).
  bind_date_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, Sys.Date())
    })
  },

  #' Named binding of timestamp values (colon syntax).
  bind_timestamp_named_colon = function(ctx) {
    with_connection({
      data_in <- as.POSIXct(round(Sys.time()))
      test_select_bind(
        con, named_colon, data_in,
        type = dbDataType(con, data_in),
        transform_input = identity,
        transform_output = identity,
        expect = expect_equal)
    })
  },

  #' Named binding of \code{\link{POSIXlt}} timestamp values (colon
  #' syntax).
  bind_timestamp_lt_named_colon = function(ctx) {
    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, named_colon, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = identity)
    })
  },

  #' Named binding of raw values (colon syntax).
  bind_raw_named_colon = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, named_colon, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = function(x) x[[1L]],
        transform_output = identity)
    })
  },

  NULL
)
