#' @name test_all
#' @section Tests:
#' \code{\link{test_result}}:
#' Test the "Result" class
NULL

#' Test the "Result" class
#'
#' @inheritParams test_all
#' @include test_connection.R
#' @family tests
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{trivial_query}}{
    #' Can issue trivial query, result object inherits from "DBIResult"
    #' }
    trivial_query = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        expect_s4_class(res, "DBIResult")
      })
    },

    #' \item{\code{clear_result_return}}{
    #' Return value, currently tests that the return value is always
    #' \code{TRUE}, and that an attempt to close a closed result set issues a
    #' warning.
    #' }
    clear_result_return = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        expect_true(dbClearResult(res))
        expect_warning(expect_true(dbClearResult(res)))
      })
    },

    #' \item{\code{stale_result_warning}}{
    #' Leaving a result open when closing a connection gives a warning
    #' }
    stale_result_warning = function() {
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

    #' \item{\code{command_query}}{
    #' Can issue a command query that creates a table, inserts a row, and
    #' deletes it; the result sets for these query always have "completed"
    #' status.
    #' }
    command_query = function() {
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

    #' \item{\code{invalid_query}}{
    #' Issuing an invalid query throws error (but no warnings, e.g. related to
    #'   pending results, are thrown)
    #' }
    invalid_query = function() {
      expect_warning(
        with_connection({
          expect_error(dbSendQuery(con, "RAISE"))
        }),
        NA
      )
    },

    #' \item{\code{fetch_single}}{
    #' single-value queries can be fetched
    #' }
    fetch_single = function() {
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

    #' \item{\code{fetch_multi_row_single_column}}{
    #' multi-row single-column queries can be fetched
    #' }
    fetch_multi_row_single_column = function() {
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

    #' \item{\code{fetch_progressive}}{
    #' multi-row queries can be fetched progressively
    #' }
    fetch_progressive = function() {
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

    #' \item{\code{fetch_more_rows}}{
    #' if more rows than available are fetched, the result is returned in full
    #'   but no warning is issued
    #' }
    fetch_more_rows = function() {
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

    #' \item{\code{fetch_premature_close}}{
    #' if less rows than available are fetched, the result is returned in full
    #'   but no warning is issued; also tests the corner case of fetching zero
    #'   rows
    #' }
    fetch_premature_close = function() {
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

    #' \item{\code{fetch_no_return_value}}{
    #' side-effect-only queries (without return value) can be fetched
    #' }
    fetch_no_return_value = function() {
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

    #' \item{\code{fetch_closed}}{
    #' Fetching from a closed result set raises an error
    #' }
    fetch_closed = function() {
      with_connection({
        query <- "SELECT 1"

        res <- dbSendQuery(con, query)
        dbClearResult(res)

        expect_error(dbHasCompleted(res))

        expect_error(dbFetch(res))
      })
    },

    #' \item{\code{cannot_query_disconnected}}{
    #' Querying a disconnected connection throws error.
    #' }
    cannot_query_disconnected = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_error(dbGetQuery(con, "SELECT 1"))
    },

    #' \item{\code{get_query_single}}{
    #' single-value queries can be read with dbGetQuery
    #' }
    get_query_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L))
      })
    },

    #' \item{\code{get_query_multi_row_single_column}}{
    #' multi-row single-column queries can be read with dbGetQuery
    #' }
    get_query_multi_row_single_column = function() {
      with_connection({
        query <- union(
          .ctx = ctx, paste("SELECT", 1:3, "AS a"), .order_by = "a")

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:3L))
      })
    },

    #' \item{\code{get_query_empty_single_column}}{
    #' Empty single-column queries can be read with dbGetQuery
    #' }
    get_query_empty_single_column = function() {
      with_connection({
        query <- "SELECT * FROM (SELECT 1 as a) AS x WHERE (1 = 0)"

        rows <- dbGetQuery(con, query)
        expect_identical(names(rows), "a")
        expect_identical(dim(rows), c(0L, 1L))
      })
    },

    #' \item{\code{get_query_single_row_multi_column}}{
    #' single-row multi-column queries can be read with dbGetQuery
    #' }
    get_query_single_row_multi_column = function() {
      with_connection({
        query <- "SELECT 1 as a, 2 as b, 3 as c"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
      })
    },

    #' \item{\code{get_query_multi}}{
    #' multi-row multi-column queries can be read with dbGetQuery
    #' }
    get_query_multi = function() {
      with_connection({
        query <- union(.ctx = ctx, paste("SELECT", 1:2, "AS a,", 2:3, "AS b"),
                       .order_by = "a")

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:2L, b=2L:3L))
      })
    },

    #' \item{\code{get_query_empty_multi_column}}{
    #' Empty multi-column queries can be read with dbGetQuery
    #' }
    get_query_empty_multi_column = function() {
      with_connection({
        query <-
          "SELECT * FROM (SELECT 1 as a, 2 as b, 3 as c) AS x WHERE (1 = 0)"

        rows <- dbGetQuery(con, query)
        expect_identical(names(rows), letters[1:3])
        expect_identical(dim(rows), c(0L, 3L))
      })
    },

    #' \item{\code{data_type_connection}}{
    #' SQL Data types exist for all basic R data types, and the engine can
    #' process them.
    #' }
    data_type_connection = function() {
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

    #' \item{\code{data_type_factor}}{
    #' SQL data type for factor is the same as for character.
    #' }
    data_type_factor = function() {
      with_connection({
        expect_identical(dbDataType(con, letters),
                         dbDataType(con, factor(letters)))
        expect_identical(dbDataType(con, letters),
                         dbDataType(con, ordered(letters)))
      })
    },

    #' \item{\code{data_integer}}{
    #' data conversion from SQL to R: integer
    #' }
    data_integer = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1L, -100L)
      })
    },

    #' \item{\code{data_integer_null_below}}{
    #' data conversion from SQL to R: integer with typed NULL values
    #' }
    data_integer_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1L, -100L, .add_null = "below")
      })
    },

    #' \item{\code{data_integer_null_above}}{
    #' data conversion from SQL to R: integer with typed NULL values
    #' in the first row
    #' }
    data_integer_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1L, -100L, .add_null = "above")
      })
    },

    #' \item{\code{data_numeric}}{
    #' data conversion from SQL to R: numeric
    #' }
    data_numeric = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1.5, -100.5)
      })
    },

    #' \item{\code{data_numeric_null_below}}{
    #' data conversion from SQL to R: numeric with typed NULL values
    #' }
    data_numeric_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1.5, -100.5, .add_null = "below")
      })
    },

    #' \item{\code{data_numeric_null_above}}{
    #' data conversion from SQL to R: numeric with typed NULL values
    #' in the first row
    #' }
    data_numeric_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con, 1.5, -100.5, .add_null = "above")
      })
    },

    #' \item{\code{data_logical}}{
    #' data conversion from SQL to R: logical
    #' }
    data_logical = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE)
      })
    },

    #' \item{\code{data_logical_null_below}}{
    #' data conversion from SQL to R: logical with typed NULL values
    #' }
    data_logical_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_logical_null_above}}{
    #' data conversion from SQL to R: logical with typed NULL values
    #' in the first row
    #' }
    data_logical_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = TRUE, "cast(0 AS boolean)" = FALSE,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_logical_int}}{
    #' data conversion from SQL to R: logical (as integers)
    #' }
    data_logical_int = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L)
      })
    },

    #' \item{\code{data_logical_int_null_below}}{
    #' data conversion from SQL to R: logical (as integers) with typed NULL
    #' values
    #' }
    data_logical_int_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_logical_int_null_above}}{
    #' data conversion from SQL to R: logical (as integers) with typed NULL
    #' values
    #' in the first row
    #' }
    data_logical_int_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "CAST(1 AS boolean)" = 1L, "cast(0 AS boolean)" = 0L,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_null}}{
    #' data conversion from SQL to R: A NULL value is returned as NA
    #' }
    data_null = function() {
      with_connection({
        check_result <- function(rows) {
          expect_true(is.na(rows$a))
        }

        test_select(.ctx = ctx, con, "NULL" = is.na)
      })
    },

    #' \item{\code{data_64_bit}}{
    #' data conversion from SQL to R: 64-bit integers
    #' }
    data_64_bit = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "10000000000" = 10000000000, "-10000000000" = 10000000000)
      })
    },

    #' \item{\code{data_64_bit_null_below}}{
    #' data conversion from SQL to R: 64-bit integers with typed NULL values
    #' }
    data_64_bit_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "10000000000" = 10000000000, "-10000000000" = 10000000000,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_64_bit_null_above}}{
    #' data conversion from SQL to R: 64-bit integers with typed NULL values
    #' in the first row
    #' }
    data_64_bit_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "10000000000" = 10000000000, "-10000000000" = 10000000000,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_character}}{
    #' data conversion from SQL to R: character
    #' }
    data_character = function() {
      with_connection({
        values <- texts
        sql_names <- as.character(dbQuoteString(con, texts))
        test_select(.ctx = ctx, con, .dots = setNames(values, sql_names))
      })
    },

    #' \item{\code{data_character_null_below}}{
    #' data conversion from SQL to R: character with typed NULL values
    #' }
    data_character_null_below = function() {
      with_connection({
        values <- texts
        sql_names <- as.character(dbQuoteString(con, texts))
        test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                    .add_null = "below")
      })
    },

    #' \item{\code{data_character_null_above}}{
    #' data conversion from SQL to R: character with typed NULL values
    #' in the first row
    #' }
    data_character_null_above = function() {
      with_connection({
        values <- texts
        sql_names <- as.character(dbQuoteString(con, texts))
        test_select(.ctx = ctx, con, .dots = setNames(values, sql_names),
                    .add_null = "above")
      })
    },

    #' \item{\code{data_raw}}{
    #' data conversion from SQL to R: raw
    #' }
    data_raw = function() {
      if (isTRUE(ctx$tweaks$omit_blob_tests)) {
        skip("tweak: omit_blob_tests")
      }

      with_connection({
        values <- list(is_raw_list)
        sql_names <- paste0("cast(1 as ", dbDataType(con, list(raw())), ")")

        test_select(.ctx = ctx, con, .dots = setNames(values, sql_names))
      })
    },

    #' \item{\code{data_raw_null_below}}{
    #' data conversion from SQL to R: raw with typed NULL values
    #' }
    data_raw_null_below = function() {
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

    #' \item{\code{data_raw_null_above}}{
    #' data conversion from SQL to R: raw with typed NULL values
    #' in the first row
    #' }
    data_raw_null_above = function() {
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

    #' \item{\code{data_date}}{
    #' data conversion from SQL to R: date, returned as integer with class
    #' }
    data_date = function() {
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

    #' \item{\code{data_date_null_below}}{
    #' data conversion from SQL to R: date with typed NULL values
    #' }
    data_date_null_below = function() {
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

    #' \item{\code{data_date_null_above}}{
    #' data conversion from SQL to R: date with typed NULL values
    #' in the first row
    #' }
    data_date_null_above = function() {
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

    #' \item{\code{data_time}}{
    #' data conversion from SQL to R: time
    #' }
    data_time = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time '00:00:00'" = "00:00:00",
                    "time '12:34:56'" = "12:34:56",
                    "current_time" ~ is.character)
      })
    },

    #' \item{\code{data_time_null_below}}{
    #' data conversion from SQL to R: time with typed NULL values
    #' }
    data_time_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time '00:00:00'" = "00:00:00",
                    "time '12:34:56'" = "12:34:56",
                    "current_time" ~ is.character,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_time_null_above}}{
    #' data conversion from SQL to R: time with typed NULL values
    #' in the first row
    #' }
    data_time_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time '00:00:00'" = "00:00:00",
                    "time '12:34:56'" = "12:34:56",
                    "current_time" ~ is.character,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_time_parens}}{
    #' data conversion from SQL to R: time (using alternative syntax with
    #' parentheses for specifying time literals)
    #' }
    data_time_parens = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time('00:00:00')" = "00:00:00",
                    "time('12:34:56')" = "12:34:56",
                    "current_time" ~ is.character)
      })
    },

    #' \item{\code{data_time_parens_null_below}}{
    #' data conversion from SQL to R: time (using alternative syntax with
    #' parentheses for specifying time literals) with typed NULL values
    #' }
    data_time_parens_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time('00:00:00')" = "00:00:00",
                    "time('12:34:56')" = "12:34:56",
                    "current_time" ~ is.character,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_time_parens_null_above}}{
    #' data conversion from SQL to R: time (using alternative syntax with
    #' parentheses for specifying time literals) with typed NULL values
    #' in the first row
    #' }
    data_time_parens_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "time('00:00:00')" = "00:00:00",
                    "time('12:34:56')" = "12:34:56",
                    "current_time" ~ is.character,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_timestamp}}{
    #' data conversion from SQL to R: timestamp
    #' }
    data_timestamp = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "timestamp '2015-10-11 00:00:00'" = is_time,
                    "timestamp '2015-10-11 12:34:56'" = is_time,
                    "current_timestamp" ~ is_roughly_current_time)
      })
    },

    #' \item{\code{data_timestamp_null_below}}{
    #' data conversion from SQL to R: timestamp with typed NULL values
    #' }
    data_timestamp_null_below = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "timestamp '2015-10-11 00:00:00'" = is_time,
                    "timestamp '2015-10-11 12:34:56'" = is_time,
                    "current_timestamp" ~ is_roughly_current_time,
                    .add_null = "below")
      })
    },

    #' \item{\code{data_timestamp_null_above}}{
    #' data conversion from SQL to R: timestamp with typed NULL values
    #' in the first row
    #' }
    data_timestamp_null_above = function() {
      with_connection({
        test_select(.ctx = ctx, con,
                    "timestamp '2015-10-11 00:00:00'" = is_time,
                    "timestamp '2015-10-11 12:34:56'" = is_time,
                    "current_timestamp" ~ is_roughly_current_time,
                    .add_null = "above")
      })
    },

    #' \item{\code{data_timestamp_utc}}{
    #' data conversion from SQL to R: timestamp with time zone
    #' }
    data_timestamp_utc = function() {
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

    #' \item{\code{data_timestamp_utc_null_below}}{
    #' data conversion from SQL to R: timestamp with time zone with typed NULL
    #' values
    #' }
    data_timestamp_utc_null_below = function() {
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

    #' \item{\code{data_timestamp_utc_null_above}}{
    #' data conversion from SQL to R: timestamp with time zone with typed NULL
    #' values
    #' in the first row
    #' }
    data_timestamp_utc_null_above = function() {
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

    #' \item{\code{data_timestamp_parens}}{
    #' data conversion: timestamp (alternative syntax with parentheses
    #' for specifying timestamp literals)
    #' }
    data_timestamp_parens = function() {
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

    #' \item{\code{data_timestamp_parens_null_below}}{
    #' data conversion: timestamp (alternative syntax with parentheses
    #' for specifying timestamp literals) with typed NULL values
    #' }
    data_timestamp_parens_null_below = function() {
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

    #' \item{\code{data_timestamp_parens_null_above}}{
    #' data conversion: timestamp (alternative syntax with parentheses
    #' for specifying timestamp literals) with typed NULL values
    #' in the first row
    #' }
    data_timestamp_parens_null_above = function() {
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
  #'}
  run_tests(tests, skip, test_suite, ctx$name)
}

utils::globalVariables("con")
utils::globalVariables("con2")

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    .(con) <- connect(ctx)
    on.exit(expect_error(dbDisconnect(.(con)), NA), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Helper function
union <- function(..., .order_by = NULL, .ctx) {
  if (is.null(.ctx$tweaks$union)) {
    query <- paste(c(...), collapse = " UNION ")
  } else {
    query <- .ctx$tweaks$union(c(...))
  }

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}

# NB: .table = TRUE will not work in bigrquery
test_select <- function(con, ..., .dots = NULL, .add_null = "none",
                        .table = FALSE, .ctx, .envir = parent.frame()) {
  values <- c(list(...), .dots)

  value_is_formula <- vapply(values, is.call, logical(1L))
  names(values)[value_is_formula] <- lapply(values[value_is_formula], "[[", 2L)
  values[value_is_formula] <- lapply(
    values[value_is_formula],
    function(x) {
      eval(x[[3]], envir = .envir)
    }
  )

  if (is.null(names(values))) {
    sql_values <- lapply(values, as.character)
  } else {
    sql_values <- names(values)
  }

  if (isTRUE(.ctx$tweaks$current_needs_parens)) {
    sql_values <- gsub("^(current_(?:date|time|timestamp))$", "\\1()",
                       sql_values)
  }

  sql_names <- letters[seq_along(sql_values)]

  query <- paste("SELECT",
                 paste(sql_values, "as", sql_names, collapse = ", "))
  if (.add_null != "none") {
    query_null <- paste("SELECT",
                        paste("NULL as", sql_names, collapse = ", "))
    query <- c(query, query_null)
    if (.add_null == "above") {
      query <- rev(query)
    }
    query <- paste0(query, ", ", 1:2, " as id")
    query <- union(.ctx = .ctx, query)
  }

  if (.table) {
    query <- paste("CREATE TABLE test AS", query)
    expect_warning(dbGetQuery(con, query), NA)
    on.exit(expect_error(dbGetQuery(con, "DROP TABLE test"), NA), add = TRUE)
    expect_warning(rows <- dbReadTable(con, "test"), NA)
  } else {
    expect_warning(rows <- dbGetQuery(con, query), NA)
  }

  if (.add_null != "none") {
    rows <- rows[order(rows$id), -(length(sql_names) + 1L)]
    if (.add_null == "above") {
      rows <- rows[2:1, ]
    }
  }

  expect_identical(names(rows), sql_names)

  for (i in seq_along(values)) {
    value_or_testfun <- values[[i]]
    if (is.function(value_or_testfun)) {
      eval(bquote(expect_true(value_or_testfun(rows[1L, .(i)]))))
    } else {
      eval(bquote(expect_identical(rows[1L, .(i)], .(value_or_testfun))))
    }
  }

  if (.add_null != "none") {
    expect_equal(nrow(rows), 2L)
    expect_true(all(is.na(unname(unlist(rows[2L, ])))))
  } else {
    expect_equal(nrow(rows), 1L)
  }
}

is_raw_list <- function(x) {
  is.list(x) && is.raw(x[[1L]])
}

is_time <- function(x) {
  inherits(x, "POSIXct")
}

is_roughly_current_time <- function(x) {
  is_time(x) && (Sys.time() - x <= 2)
}

as_integer_date <- function(d) {
  d <- as.Date(d)
  structure(as.integer(unclass(d)), class = class(d))
}
