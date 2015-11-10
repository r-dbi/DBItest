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
        on.exit(dbClearResult(res), add = TRUE)
        expect_is(res, "DBIResult")
      })
    },

    #' \item{\code{command_query}}{
    #' Can issue a command query that creates a table, inserts a row, and
    #' deletes it; the result sets for these query always have "completed"
    #' status.
    #' }
    command_query = function() {
      with_connection({
        res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
        expect_true(dbHasCompleted(res))
        expect_error(dbClearResult(res), NA)

        res <- dbSendQuery(con, "INSERT INTO test SELECT 1")
        expect_true(dbHasCompleted(res))
        expect_error(dbClearResult(res), NA)

        res <- dbSendQuery(con, "DROP TABLE test")
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

    #' \item{\code{get_info}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)

        info <- dbGetInfo(res)
        expect_is(info, "list")
        info_names <- names(info)

        expect_true("statement" %in% info_names)
        expect_true("row.count" %in% info_names)
        expect_true("rows.affected" %in% info_names)
        expect_true("has.completed" %in% info_names)
        expect_true("is.select" %in% info_names)
      })
    },

    #' \item{\code{fetch_single}}{
    #' single-value queries can be fetched
    #' }
    fetch_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

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
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3", .order_by = "a")

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

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
        query <- union(paste("SELECT", 1:25, "AS a"), .order_by = "a")

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

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
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3", .order_by = "a")

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

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
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3", .order_by = "a")

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

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
          dbClearResult(dbSendQuery(con, "DROP TABLE test"))
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
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3", .order_by = "a")

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:3L))
      })
    },

    #' \item{\code{get_query_single_column_multi_row}}{
    #' single-row multi-column queries can be read with dbGetQuery
    #' }
    get_query_single_column_multi_row = function() {
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
        query <- union("SELECT 1 as a, 2 as b", "SELECT 2, 3", .order_by = "a")

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:2L, b=2L:3L))
      })
    },

    #' \item{\code{data_type_connection}}{
    #' SQL Data types exist for all basic R data types, and the engine can
    #' process them.
    #' }
    data_type_connection = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

      check_connection_data_type <- function(value) {
        eval(bquote({
          expect_is(dbDataType(con, .(value)), "character")
          expect_equal(length(dbDataType(con, .(value))), 1L)
          query <- paste0("CREATE TABLE test (a ", dbDataType(con, .(value)),
                          ")")
        }))

        eval(bquote({
          expect_error(dbGetQuery(con, .(query)), NA)
          on.exit(try(dbGetQuery(con, "DROP TABLE test")))
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
      expect_conn_has_data_type(list(raw(1)))
      expect_conn_has_data_type(Sys.Date())
      expect_conn_has_data_type(Sys.time())
    },

    #' \item{\code{data_integer}}{
    #' data conversion from SQL to R: integer
    #' }
    data_integer = function() {
      with_connection({
        query <- "SELECT 1 as a, -100 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=1L, b=-100L))
      })
    },

    #' \item{\code{data_integer_null}}{
    #' data conversion from SQL to R: integer with typed NULL values
    #' }
    data_integer_null = function() {
      with_connection({
        query <- union("SELECT 1 as a, -100 as b, 0 as c",
                       "SELECT NULL, NULL, 1",
                       .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=c(1L, NA), b=c(-100L, NA), c=0:1))
      })
    },

    #' \item{\code{data_numeric}}{
    #' data conversion from SQL to R: numeric
    #' }
    data_numeric = function() {
      with_connection({
        query <- "SELECT 1.0 as a, -100.5 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=1, b=-100.5))
      })
    },

    #' \item{\code{data_numeric_null}}{
    #' data conversion from SQL to R: numeric with typed NULL values
    #' }
    data_numeric_null = function() {
      with_connection({
        query <- union("SELECT 1.0 as a, -100.5 as b, 0 as c",
                       "SELECT NULL, NULL, 1",
                       .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=c(1, NA), b=c(-100.5, NA), c=0:1))
      })
    },

    #' \item{\code{data_logical}}{
    #' data conversion from SQL to R: logical
    #' }
    data_logical = function() {
      with_connection({
        query <- "SELECT CAST(1 AS boolean) as a, cast(0 AS boolean) as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=TRUE, b=FALSE))
      })
    },

    #' \item{\code{data_logical}}{
    #' data conversion from SQL to R: logical with typed NULL values
    #' }
    data_logical = function() {
      with_connection({
        query <- union(
          "SELECT CAST(1 AS boolean) as a, cast(0 AS boolean) as b, 0 as c",
          "SELECT NULL, NULL, 1",
          .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=c(TRUE, NA), b=c(FALSE, NA), c=0:1))
      })
    },

    #' \item{\code{data_logical_int}}{
    #' data conversion from SQL to R: logical (as integers)
    #' }
    data_logical_int = function() {
      with_connection({
        query <- "SELECT CAST(1 AS boolean) as a, cast(0 AS boolean) as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=1L, b=0L))
      })
    },

    #' \item{\code{data_logical_int_null}}{
    #' data conversion from SQL to R: logical (as integers) with typed NULL
    #' values
    #' }
    data_logical_int_null = function() {
      with_connection({
        query <- union(
          "SELECT CAST(1 AS boolean) as a, cast(0 AS boolean) as b, 0 as c",
          "SELECT NULL, NULL, 1",
          .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=c(1L, NA), b=c(0L, NA), c=0:1))
      })
    },

    #' \item{\code{data_null}}{
    #' data conversion from SQL to R: A NULL value is returned as NA
    #' }
    data_null = function() {
      with_connection({
        query <- "SELECT NULL as a"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_true(is.na(rows$a))
      })
    },

    #' \item{\code{data_64_bit}}{
    #' data conversion from SQL to R: 64-bit integers
    #' }
    data_64_bit = function() {
      with_connection({
        query <- "SELECT 10000000000 as a, -10000000000 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(as.numeric(rows$a), 10000000000)
        expect_equal(as.numeric(rows$b), -10000000000)
      })
    },

    #' \item{\code{data_64_bit_null}}{
    #' data conversion from SQL to R: 64-bit integers with typed NULL values
    #' }
    data_64_bit_null = function() {
      with_connection({
        query <- union("SELECT 10000000000 as a, -10000000000 as b, 0 as c",
                       "SELECT NULL, NULL, 1",
                       .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(as.numeric(rows$a), c(10000000000, NA))
        expect_equal(as.numeric(rows$b), c(-10000000000, NA))
      })
    },

    #' \item{\code{data_character}}{
    #' data conversion from SQL to R: character
    #' }
    data_character = function() {
      with_connection({
        query <- paste0("SELECT '", text_cyrillic, "' as a, '",
                        text_latin, "' as b, '",
                        text_chinese, "' as c, '",
                        text_ascii, "' as d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(rows$a, text_cyrillic)
        expect_equal(rows$b, text_latin)
        expect_equal(rows$c, text_chinese)
        expect_equal(rows$d, text_ascii)
      })
    },

    #' \item{\code{data_character_null}}{
    #' data conversion from SQL to R: character with typed NULL values
    #' }
    data_character_null = function() {
      with_connection({
        query <- union(paste0("SELECT '", text_cyrillic, "' as a, '",
                              text_latin, "' as b, '",
                              text_chinese, "' as c, '",
                              text_ascii, "' as d, ",
                              "0 as e"),
                       "SELECT NULL, NULL, NULL, NULL, 1",
                       .order_by = "e")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(rows$a, c(text_cyrillic, NA))
        expect_equal(rows$b, c(text_latin, NA))
        expect_equal(rows$c, c(text_chinese, NA))
        expect_equal(rows$d, c(text_ascii, NA))
      })
    },

    #' \item{\code{data_raw}}{
    #' data conversion from SQL to R: raw
    #' }
    data_raw = function() {
      with_connection({
        query <- paste0("SELECT cast(1 as ",
                        dbDataType(con, list(raw())), ") a")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "list")
        expect_is(rows$a[[1L]], "raw")
      })
    },

    #' \item{\code{data_raw_null}}{
    #' data conversion from SQL to R: raw with typed NULL values
    #' }
    data_raw_null = function() {
      with_connection({
        query <- union(
          paste0("SELECT cast(1 as ", dbDataType(con, list(raw())), ") a, ",
                 "0 as b"),
          paste0("SELECT cast(NULL as ", dbDataType(con, list(raw())), ") a, ",
                 "1"),
          .order_by = "b")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "list")
        expect_is(rows$a[[1L]], "raw")
        expect_true(is.na(rows$a[[2L]]))
      })
    },

    #' \item{\code{data_date}}{
    #' data conversion from SQL to R: date
    #' }
    data_date = function() {
      with_connection({
        query <- "SELECT date('2015-10-10') as a, current_date as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "Date")
        expect_is(rows$b, "Date")
        expect_equal(rows$a, as.Date("2015-10-10"))
      })
    },

    #' \item{\code{data_date_null}}{
    #' data conversion from SQL to R: date with typed NULL values
    #' }
    data_date_null = function() {
      with_connection({
        query <- union(
          "SELECT date('2015-10-10') as a, current_date as b, 0 as c",
          "SELECT NULL, NULL, 1",
          .order_by = "c")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "Date")
        expect_is(rows$b, "Date")
        expect_equal(rows$a, c(as.Date("2015-10-10"), NA))
      })
    },

    #' \item{\code{data_time}}{
    #' data conversion from SQL to R: time
    #' }
    data_time = function() {
      with_connection({
        query <- "SELECT time '00:00:00' as a,
                  time '12:34:56' as b, current_time as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "character")
        expect_is(rows$b, "character")
        expect_is(rows$c, "character")
        expect_equal(rows$a, "00:00:00")
        expect_equal(rows$b, "12:34:56")
      })
    },

    #' \item{\code{data_time_null}}{
    #' data conversion from SQL to R: time with typed NULL values
    #' }
    data_time_null = function() {
      with_connection({
        query <- union("SELECT time '00:00:00' as a,
                        time '12:34:56' as b, current_time as c, 0 as d",
                       "SELECT NULL, NULL, NULL, 1",
                       .order_by = "d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "character")
        expect_is(rows$b, "character")
        expect_is(rows$c, "character")
        expect_equal(rows$a, c("00:00:00", NA))
        expect_equal(rows$b, c("12:34:56", NA))
      })
    },

    #' \item{\code{data_time_parens}}{
    #' data conversion from SQL to R: time (using alternative syntax with
    #' parentheses for specifying time literals)
    #' }
    data_time_parens = function() {
      with_connection({
        query <- "SELECT time('00:00:00') as a,
                  time('12:34:56') as b, current_time as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "character")
        expect_is(rows$b, "character")
        expect_is(rows$c, "character")
        expect_equal(rows$a, "00:00:00")
        expect_equal(rows$b, "12:34:56")
      })
    },

    #' \item{\code{data_time_parens_null}}{
    #' data conversion from SQL to R: time (using alternative syntax with
    #' parentheses for specifying time literals) with typed NULL values
    #' }
    data_time_parens_null = function() {
      with_connection({
        query <- union("SELECT time('00:00:00') as a,
                        time('12:34:56') as b, current_time as c, 0 as d",
                       "SELECT NULL, NULL, NULL, 1",
                       .order_by = "d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "character")
        expect_is(rows$b, "character")
        expect_is(rows$c, "character")
        expect_equal(rows$a, c("00:00:00", NA))
        expect_equal(rows$b, c("12:34:56", NA))
      })
    },

    #' \item{\code{data_timestamp}}{
    #' data conversion from SQL to R: timestamp
    #' }
    data_timestamp = function() {
      with_connection({
        query <- "SELECT timestamp '2015-10-11 00:00:00' as a,
        timestamp '2015-10-11 12:34:56' as b, current_timestamp as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_less_than(Sys.time() - rows$c, 2)
      })
    },

    #' \item{\code{data_timestamp_null}}{
    #' data conversion from SQL to R: timestamp with typed NULL values
    #' }
    data_timestamp_null = function() {
      with_connection({
        query <- union(
          "SELECT
           timestamp '2015-10-11 00:00:00' as a,
           timestamp '2015-10-11 12:34:56' as b,
           current_timestamp as c, 0 as d",
          "SELECT NULL, NULL, NULL, 1",
          .order_by = "d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_less_than(Sys.time() - rows$c[[1L]], 2)
        expect_true(is.na(rows$c[[2L]]))
      })
    },

    #' \item{\code{data_timestamp_utc}}{
    #' data conversion from SQL to R: timestamp with time zone
    #' }
    data_timestamp_utc = function() {
      with_connection({
        query <- "SELECT
        timestamp with time zone '2015-10-11 00:00:00+02:00' as a,
        timestamp with time zone '2015-10-11 12:34:56-05:00' as b,
        current_timestamp as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_equal(rows$a, as.POSIXct("2015-10-11 00:00:00+02:00"))
        expect_equal(rows$b, as.POSIXct("2015-10-11 12:34:56-05:00"))
        expect_less_than(Sys.time() - rows$c, 1)
      })
    },

    #' \item{\code{data_timestamp_utc_null}}{
    #' data conversion from SQL to R: timestamp with time zone with typed NULL
    #' values
    #' }
    data_timestamp_utc_null = function() {
      with_connection({
        query <- union(
          "SELECT
           timestamp with time zone '2015-10-11 00:00:00+02:00' as a,
           timestamp with time zone '2015-10-11 12:34:56-05:00' as b,
           current_timestamp as c, 0 as d",
          "SELECT NULL, NULL, NULL, 1",
          .order_by = "d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_equal(rows$a, c(as.POSIXct("2015-10-11 00:00:00+02:00"), NA))
        expect_equal(rows$b, c(as.POSIXct("2015-10-11 12:34:56-05:00"), NA))
        expect_less_than(Sys.time() - rows$c[[1L]], 1)
        expect_true(is.na(rows$c[[2L]]))
      })
    },

    #' \item{\code{data_timestamp_parens}}{
    #' data conversion: timestamp (alternative syntax with parentheses
    #' for specifying timestamp literals)
    #' }
    data_timestamp_parens = function() {
      with_connection({
        query <- "SELECT datetime('2015-10-11 00:00:00') as a,
                  datetime('2015-10-11 12:34:56') as b, current_timestamp as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_equal(rows$a, as.POSIXct("2015-10-11 00:00:00Z"))
        expect_equal(rows$b, as.POSIXct("2015-10-11 12:34:56Z"))
        expect_less_than(Sys.time() - rows$c, 2)
      })
    },

    #' \item{\code{data_timestamp_parens_null}}{
    #' data conversion: timestamp (alternative syntax with parentheses
    #' for specifying timestamp literals) with typed NULL values
    #' }
    data_timestamp_parens_null = function() {
      with_connection({
        query <- union(
          "SELECT datetime('2015-10-11 00:00:00') as a,
           datetime('2015-10-11 12:34:56') as b, current_timestamp as c,
           0 as d",
          "SELECT NULL, NULL, NULL, 1",
          .order_by = "d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_equal(rows$a, c(as.POSIXct("2015-10-11 00:00:00Z", NA)))
        expect_equal(rows$b, c(as.POSIXct("2015-10-11 12:34:56Z", NA)))
        expect_less_than(Sys.time() - rows$c[[1L]], 2)
        expect_true(is.na(rows$c[[2L]]))
      })
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}

utils::globalVariables("con")

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, env = parent.frame()) {
  code_sub <- substitute(code)

  eval(bquote({
    con <- connect(ctx)
    on.exit(dbDisconnect(con), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Helper function
union <- function(..., .order_by = NULL) {
  query <- paste(c(...), collapse = " UNION ")
  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}
