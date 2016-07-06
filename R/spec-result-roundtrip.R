#' @template dbispec-sub
#' @section Result:
#' \subsection{Data roundtrip}{}
spec_result_roundtrip <- list(
  #' Data conversion from SQL to R: integer
  data_integer = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1L, -100L)
    })
  },

  #' Data conversion from SQL to R: integer with typed NULL values.
  data_integer_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1L, -100L, .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: integer with typed NULL values
  #' in the first row.
  data_integer_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1L, -100L, .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: numeric.
  data_numeric = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1.5, -100.5)
    })
  },

  #' Data conversion from SQL to R: numeric with typed NULL values.
  data_numeric_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1.5, -100.5, .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: numeric with typed NULL values
  #' in the first row.
  data_numeric_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con, 1.5, -100.5, .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: logical. Optional, conflict with the
  #' \code{data_logical_int} test.
  data_logical = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE)
    })
  },

  #' Data conversion from SQL to R: logical with typed NULL values.
  data_logical_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: logical with typed NULL values
  #' in the first row
  data_logical_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: logical (as integers). Optional,
  #' conflict with the \code{data_logical} test.
  data_logical_int = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L)
    })
  },

  #' Data conversion from SQL to R: logical (as integers) with typed NULL
  #' values.
  data_logical_int_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: logical (as integers) with typed NULL
  #' values
  #' in the first row.
  data_logical_int_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: A NULL value is returned as NA.
  data_null = function(ctx) {
    with_connection({
      check_result <- function(rows) {
        expect_true(is.na(rows$a))
      }

      test_select(.ctx = ctx, con, "NULL" = is.na)
    })
  },

  #' Data conversion from SQL to R: 64-bit integers.
  data_64_bit = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "10000000000" = 10000000000, "-10000000000" = -10000000000)
    })
  },

  #' Data conversion from SQL to R: 64-bit integers with typed NULL values.
  data_64_bit_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "10000000000" = 10000000000, "-10000000000" = -10000000000,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: 64-bit integers with typed NULL values
  #' in the first row.
  data_64_bit_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "10000000000" = 10000000000, "-10000000000" = -10000000000,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: character.
  data_character = function(ctx) {
    with_connection({
      values <- texts
      test_funs <- rep(list(has_utf8_or_ascii_encoding), length(values))
      sql_names <- as.character(dbQuoteString(con, texts))

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names))
      test_select(.ctx = ctx, con, .dots = setNames(test_funs, sql_names))
    })
  },

  #' Data conversion from SQL to R: character with typed NULL values.
  data_character_null_below = function(ctx) {
    with_connection({
      values <- texts
      test_funs <- rep(list(has_utf8_or_ascii_encoding), length(values))
      sql_names <- as.character(dbQuoteString(con, texts))

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                  .add_null = "below")
      test_select(.ctx = ctx, con, .dots = setNames(test_funs, sql_names),
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: character with typed NULL values
  #' in the first row.
  data_character_null_above = function(ctx) {
    with_connection({
      values <- texts
      test_funs <- rep(list(has_utf8_or_ascii_encoding), length(values))
      sql_names <- as.character(dbQuoteString(con, texts))

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                  .add_null = "above")
      test_select(.ctx = ctx, con, .dots = setNames(test_funs, sql_names),
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: raw. Not all SQL dialects support the
  #' syntax of the query used here.
  data_raw = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      values <- list(is_raw_list)
      sql_names <- paste0("cast(1 as ", dbDataType(con, list(raw())), ")")

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names))
    })
  },

  #' Data conversion from SQL to R: raw with typed NULL values.
  data_raw_null_below = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      values <- list(is_raw_list)
      sql_names <- paste0("cast(1 as ", dbDataType(con, list(raw())), ")")

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: raw with typed NULL values
  #' in the first row.
  data_raw_null_above = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      values <- list(is_raw_list)
      sql_names <- paste0("cast(1 as ", dbDataType(con, list(raw())), ")")

      test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: date, returned as integer with class.
  data_date = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "date('2015-01-01')" = as_integer_date("2015-01-01"),
                  "date('2015-02-02')" = as_integer_date("2015-02-02"),
                  "date('2015-03-03')" = as_integer_date("2015-03-03"),
                  "date('2015-04-04')" = as_integer_date("2015-04-04"),
                  "date('2015-05-05')" = as_integer_date("2015-05-05"),
                  "date('2015-06-06')" = as_integer_date("2015-06-06"),
                  "date('2015-07-07')" = as_integer_date("2015-07-07"),
                  "date('2015-08-08')" = as_integer_date("2015-08-08"),
                  "date('2015-09-09')" = as_integer_date("2015-09-09"),
                  "date('2015-10-10')" = as_integer_date("2015-10-10"),
                  "date('2015-11-11')" = as_integer_date("2015-11-11"),
                  "date('2015-12-12')" = as_integer_date("2015-12-12"),
                  "current_date" ~ as_integer_date(Sys.time()))
    })
  },

  #' Data conversion from SQL to R: date with typed NULL values.
  data_date_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "date('2015-01-01')" = as_integer_date("2015-01-01"),
                  "date('2015-02-02')" = as_integer_date("2015-02-02"),
                  "date('2015-03-03')" = as_integer_date("2015-03-03"),
                  "date('2015-04-04')" = as_integer_date("2015-04-04"),
                  "date('2015-05-05')" = as_integer_date("2015-05-05"),
                  "date('2015-06-06')" = as_integer_date("2015-06-06"),
                  "date('2015-07-07')" = as_integer_date("2015-07-07"),
                  "date('2015-08-08')" = as_integer_date("2015-08-08"),
                  "date('2015-09-09')" = as_integer_date("2015-09-09"),
                  "date('2015-10-10')" = as_integer_date("2015-10-10"),
                  "date('2015-11-11')" = as_integer_date("2015-11-11"),
                  "date('2015-12-12')" = as_integer_date("2015-12-12"),
                  "current_date" ~ as_integer_date(Sys.time()),
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: date with typed NULL values
  #' in the first row.
  data_date_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "date('2015-01-01')" = as_integer_date("2015-01-01"),
                  "date('2015-02-02')" = as_integer_date("2015-02-02"),
                  "date('2015-03-03')" = as_integer_date("2015-03-03"),
                  "date('2015-04-04')" = as_integer_date("2015-04-04"),
                  "date('2015-05-05')" = as_integer_date("2015-05-05"),
                  "date('2015-06-06')" = as_integer_date("2015-06-06"),
                  "date('2015-07-07')" = as_integer_date("2015-07-07"),
                  "date('2015-08-08')" = as_integer_date("2015-08-08"),
                  "date('2015-09-09')" = as_integer_date("2015-09-09"),
                  "date('2015-10-10')" = as_integer_date("2015-10-10"),
                  "date('2015-11-11')" = as_integer_date("2015-11-11"),
                  "date('2015-12-12')" = as_integer_date("2015-12-12"),
                  "current_date" ~ as_integer_date(Sys.time()),
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: time.
  data_time = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time '00:00:00'" = "00:00:00",
                  "time '12:34:56'" = "12:34:56",
                  "current_time" ~ is.character)
    })
  },

  #' Data conversion from SQL to R: time with typed NULL values.
  data_time_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time '00:00:00'" = "00:00:00",
                  "time '12:34:56'" = "12:34:56",
                  "current_time" ~ is.character,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: time with typed NULL values
  #' in the first row.
  data_time_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time '00:00:00'" = "00:00:00",
                  "time '12:34:56'" = "12:34:56",
                  "current_time" ~ is.character,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: time (using alternative syntax with
  #' parentheses for specifying time literals).
  data_time_parens = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time('00:00:00')" = "00:00:00",
                  "time('12:34:56')" = "12:34:56",
                  "current_time" ~ is.character)
    })
  },

  #' Data conversion from SQL to R: time (using alternative syntax with
  #' parentheses for specifying time literals) with typed NULL values.
  data_time_parens_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time('00:00:00')" = "00:00:00",
                  "time('12:34:56')" = "12:34:56",
                  "current_time" ~ is.character,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: time (using alternative syntax with
  #' parentheses for specifying time literals) with typed NULL values
  #' in the first row.
  data_time_parens_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "time('00:00:00')" = "00:00:00",
                  "time('12:34:56')" = "12:34:56",
                  "current_time" ~ is.character,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: timestamp.
  data_timestamp = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "timestamp '2015-10-11 00:00:00'" = is_time,
                  "timestamp '2015-10-11 12:34:56'" = is_time,
                  "current_timestamp" ~ is_roughly_current_time)
    })
  },

  #' Data conversion from SQL to R: timestamp with typed NULL values.
  data_timestamp_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "timestamp '2015-10-11 00:00:00'" = is_time,
                  "timestamp '2015-10-11 12:34:56'" = is_time,
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: timestamp with typed NULL values
  #' in the first row.
  data_timestamp_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx, con,
                  "timestamp '2015-10-11 00:00:00'" = is_time,
                  "timestamp '2015-10-11 12:34:56'" = is_time,
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "above")
    })
  },

  #' Data conversion from SQL to R: timestamp with time zone.
  data_timestamp_utc = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "timestamp '2015-10-11 00:00:00+02:00'" =
                    as.POSIXct("2015-10-11 00:00:00+02:00"),
                  "timestamp '2015-10-11 12:34:56-05:00'" =
                    as.POSIXct("2015-10-11 12:34:56-05:00"),
                  "current_timestamp" ~ is_roughly_current_time)
    })
  },

  #' Data conversion from SQL to R: timestamp with time zone with typed NULL
  #' values.
  data_timestamp_utc_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "timestamp '2015-10-11 00:00:00+02:00'" =
                    as.POSIXct("2015-10-11 00:00:00+02:00"),
                  "timestamp '2015-10-11 12:34:56-05:00'" =
                    as.POSIXct("2015-10-11 12:34:56-05:00"),
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "below")
    })
  },

  #' Data conversion from SQL to R: timestamp with time zone with typed NULL
  #' values
  #' in the first row.
  data_timestamp_utc_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "timestamp '2015-10-11 00:00:00+02:00'" =
                    as.POSIXct("2015-10-11 00:00:00+02:00"),
                  "timestamp '2015-10-11 12:34:56-05:00'" =
                    as.POSIXct("2015-10-11 12:34:56-05:00"),
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "above")
    })
  },

  #' Data conversion: timestamp (alternative syntax with parentheses
  #' for specifying timestamp literals).
  data_timestamp_parens = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "datetime('2015-10-11 00:00:00')" =
                    as.POSIXct("2015-10-11 00:00:00Z"),
                  "datetime('2015-10-11 12:34:56')" =
                    as.POSIXct("2015-10-11 12:34:56Z"),
                  "current_timestamp" ~ is_roughly_current_time)
    })
  },

  #' Data conversion: timestamp (alternative syntax with parentheses
  #' for specifying timestamp literals) with typed NULL values.
  data_timestamp_parens_null_below = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "datetime('2015-10-11 00:00:00')" =
                    as.POSIXct("2015-10-11 00:00:00Z"),
                  "datetime('2015-10-11 12:34:56')" =
                    as.POSIXct("2015-10-11 12:34:56Z"),
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "below")
    })
  },

  #' Data conversion: timestamp (alternative syntax with parentheses
  #' for specifying timestamp literals) with typed NULL values
  #' in the first row.
  data_timestamp_parens_null_above = function(ctx) {
    with_connection({
      test_select(.ctx = ctx,
                  con,
                  "datetime('2015-10-11 00:00:00')" =
                    as.POSIXct("2015-10-11 00:00:00Z"),
                  "datetime('2015-10-11 12:34:56')" =
                    as.POSIXct("2015-10-11 12:34:56Z"),
                  "current_timestamp" ~ is_roughly_current_time,
                  .add_null = "above")
    })
  },

  NULL
)
