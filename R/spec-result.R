#' @rdname DBIspec
#' @format NULL
#' @section Result:
spec_result <- list(
  #' Can issue trivial query, result object inherits from "DBIResult".
  trivial_query = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
      expect_s4_class(res, "DBIResult")
    })
  },

  #' Return value, currently tests that the return value is always
  #' \code{TRUE}, and that an attempt to close a closed result set issues a
  #' warning.
  clear_result_return = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      expect_true(dbClearResult(res))
      expect_warning(expect_true(dbClearResult(res)))
    })
  },

  #' Leaving a result open when closing a connection gives a warning.
  stale_result_warning = function(ctx) {
    with_connection({
      expect_warning(dbClearResult(dbSendQuery(con, "SELECT 1")), NA)
      expect_warning(dbClearResult(dbSendQuery(con, "SELECT 2")), NA)
    })

    expect_warning(
      with_connection(dbSendQuery(con, "SELECT 1"))
    )

    with_connection({
      expect_warning(res1 <- dbSendQuery(con, "SELECT 1"), NA)
      expect_true(dbIsValid(res1))
      expect_warning(res2 <- dbSendQuery(con, "SELECT 2"))
      expect_true(dbIsValid(res2))
      expect_false(dbIsValid(res1))
      dbClearResult(res2)
    })
  },

  #' Can issue a command query that creates a table, inserts a row, and
  #' deletes it; the result sets for these query always have "completed"
  #' status.
  command_query = function(ctx) {
    with_connection({
      on.exit({
        res <- dbSendQuery(con, "DROP TABLE test")
        expect_true(dbHasCompleted(res))
        expect_error(dbClearResult(res), NA)
      }
      , add = TRUE)

      res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
      expect_true(dbHasCompleted(res))
      expect_error(dbClearResult(res), NA)

      res <- dbSendQuery(con, "INSERT INTO test SELECT 1")
      expect_true(dbHasCompleted(res))
      expect_error(dbClearResult(res), NA)
    })
  },

  #' Issuing an invalid query throws error (but no warnings, e.g. related to
  #'   pending results, are thrown).
  invalid_query = function(ctx) {
    expect_warning(
      with_connection({
        expect_error(dbSendQuery(con, "RAISE"))
      }),
      NA
    )
  },

  #' Single-value queries can be fetched.
  fetch_single = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame(a=1L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' Multi-row single-column queries can be fetched.
  fetch_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame(a=1L:3L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' Multi-row queries can be fetched progressively.
  fetch_progressive = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:25, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=1L:10L))
      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=11L:20L))
      expect_false(dbHasCompleted(res))

      rows <- dbFetch(res, 10)
      expect_identical(rows, data.frame(a=21L:25L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' If more rows than available are fetched, the result is returned in full
  #'   but no warning is issued.
  fetch_more_rows = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_false(dbHasCompleted(res))

      expect_warning(rows <- dbFetch(res, 5L), NA)
      expect_identical(rows, data.frame(a=1L:3L))
      expect_true(dbHasCompleted(res))
    })
  },

  #' If less rows than available are fetched, the result is returned in full
  #'   but no warning is issued; also tests the corner case of fetching zero
  #'   rows.
  fetch_premature_close = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      res <- dbSendQuery(con, query)
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      expect_warning(rows <- dbFetch(res, 0L), NA)
      expect_identical(rows, data.frame(a=integer()))

      expect_warning(rows <- dbFetch(res, 2L), NA)
      expect_identical(rows, data.frame(a=1L:2L))

      expect_warning(dbClearResult(res), NA)
      on.exit(NULL, add = FALSE)
    })
  },

  #' Side-effect-only queries (without return value) can be fetched.
  fetch_no_return_value = function(ctx) {
    with_connection({
      query <- "CREATE TABLE test (a integer)"

      res <- dbSendQuery(con, query)
      on.exit({
        expect_error(dbClearResult(res), NA)
        expect_error(dbClearResult(dbSendQuery(con, "DROP TABLE test")), NA)
      }
      , add = TRUE)

      expect_true(dbHasCompleted(res))

      rows <- dbFetch(res)
      expect_identical(rows, data.frame())

      expect_true(dbHasCompleted(res))
    })
  },

  #' Fetching from a closed result set raises an error.
  fetch_closed = function(ctx) {
    with_connection({
      query <- "SELECT 1"

      res <- dbSendQuery(con, query)
      dbClearResult(res)

      expect_error(dbHasCompleted(res))

      expect_error(dbFetch(res))
    })
  },

  #' Querying a disconnected connection throws error.
  cannot_query_disconnected = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(dbGetQuery(con, "SELECT 1"))
  },

  #' Single-value queries can be read with dbGetQuery
  get_query_single = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a"

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L))
    })
  },

  #' Multi-row single-column queries can be read with dbGetQuery.
  get_query_multi_row_single_column = function(ctx) {
    with_connection({
      query <- union(
        .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L:3L))
    })
  },

  #' Empty single-column queries can be read with
  #' \code{\link[DBI]{dbGetQuery}}. Not all SQL dialects support the query
  #' used here.
  get_query_empty_single_column = function(ctx) {
    with_connection({
      query <- "SELECT * FROM (SELECT 1 as a) AS x WHERE (1 = 0)"

      rows <- dbGetQuery(con, query)
      expect_identical(names(rows), "a")
      expect_identical(dim(rows), c(0L, 1L))
    })
  },

  #' Single-row multi-column queries can be read with dbGetQuery.
  get_query_single_row_multi_column = function(ctx) {
    with_connection({
      query <- "SELECT 1 as a, 2 as b, 3 as c"

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
    })
  },

  #' Multi-row multi-column queries can be read with dbGetQuery.
  get_query_multi = function(ctx) {
    with_connection({
      query <- union(.ctx = ctx, paste("SELECT", 1:2, "AS a,", 2:3, "AS b"),
                     .order_by = "a")

      rows <- dbGetQuery(con, query)
      expect_identical(rows, data.frame(a=1L:2L, b=2L:3L))
    })
  },

  #' Empty multi-column queries can be read with
  #' \code{\link[DBI]{dbGetQuery}}. Not all SQL dialects support the query
  #' used here.
  get_query_empty_multi_column = function(ctx) {
    with_connection({
      query <-
        "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

      rows <- dbGetQuery(con, query)
      expect_identical(names(rows), letters[1:3])
      expect_identical(dim(rows), c(0L, 3L))
    })
  },

  #' SQL Data types exist for all basic R data types, and the engine can
  #' process them.
  data_type_connection = function(ctx) {
    with_connection({
      check_connection_data_type <- function(value) {
        eval(bquote({
          expect_is(dbDataType(con, .(value)), "character")
          expect_equal(length(dbDataType(con, .(value))), 1L)
          expect_error({
            as_is_type <- dbDataType(con, I(.(value)))
            expect_identical(dbDataType(con, .(value)), as_is_type)
          }
          , NA)
          expect_error({
            unknown_type <- dbDataType(con, structure(.(value),
                                                      class = "unknown1"))
            expect_identical(dbDataType(con, unclass(.(value))), unknown_type)
          }
          , NA)
          query <- paste0("CREATE TABLE test (a ", dbDataType(con, .(value)),
                          ")")
        }))

        eval(bquote({
          expect_error(dbGetQuery(con, .(query)), NA)
          on.exit(expect_error(dbGetQuery(con, "DROP TABLE test"), NA),
                  add = TRUE)
        }))
      }

      expect_conn_has_data_type <- function(value) {
        eval(bquote(
          expect_error(check_connection_data_type(.(value)), NA)))
      }

      expect_conn_has_data_type(logical(1))
      expect_conn_has_data_type(integer(1))
      expect_conn_has_data_type(numeric(1))
      expect_conn_has_data_type(character(1))
      expect_conn_has_data_type(Sys.Date())
      expect_conn_has_data_type(Sys.time())
      if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
        expect_conn_has_data_type(list(raw(1)))
      }
    })
  },

  #' SQL data type for factor is the same as for character.
  data_type_factor = function(ctx) {
    with_connection({
      expect_identical(dbDataType(con, letters),
                       dbDataType(con, factor(letters)))
      expect_identical(dbDataType(con, letters),
                       dbDataType(con, ordered(letters)))
    })
  },

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
