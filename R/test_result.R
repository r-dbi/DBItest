#' \code{test_result()} tests the "Result" class.
#'
#' @rdname test
#' @include test_connection.R
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  tests <- list(
    # Can issue trivial query, result object inherits from "DBIResult"
    trivial_query = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)
        expect_is(res, "DBIResult")
      })
    },

    # Can issue a command query that creates a table
    command_query = function() {
      with_connection({
        res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
        expect_error(dbClearResult(res), NA)
        dbClearResult(dbSendQuery(con, "DROP TABLE test"))
      })
    },

    # Issuing an invalid query throws error (but no warnings, e.g. related to
    #   pending results, are thrown)
    invalid_query = function() {
      expect_warning(
        with_connection({
          expect_error(dbSendQuery(con, "RAISE"))
        }),
        NA
      )
    },

    # Return value of dbGetInfo has necessary elements
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

    # show method for result class is defined
    show = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)

        expect_that(res, has_method("show"))
        expect_output(show(res), ".")
      })
    },

    # single-value queries can be fetched
    fetch_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        rows <- dbFetch(res)
        expect_identical(rows, data.frame(a=1L))

        expect_true(dbHasCompleted(res))
      })
    },

    # multi-row single-column queries can be fetched
    fetch_multi_row_single_column = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3 ORDER BY a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        rows <- dbFetch(res)
        expect_identical(rows, data.frame(a=1L:3L))

        expect_true(dbHasCompleted(res))
      })
    },

    # if more rows than available are fetched, the result is returned in full
    #   but no warning is issued
    fetch_more_rows = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3 ORDER BY a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        expect_warning(rows <- dbFetch(res, 5L), NA)
        expect_identical(rows, data.frame(a=1L:3L))

        expect_true(dbHasCompleted(res))
      })
    },

    # if less rows than available are fetched, the result is returned in full
    #   but no warning is issued; also tests the corner case of fetching zero
    #   rows
    fetch_premature_close = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3 ORDER BY a"

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

    # single-value queries can be fetched
    fetch_no_return_value = function() {
      with_connection({
        query <- "CREATE TABLE test (a integer)"

        res <- dbSendQuery(con, query)
        on.exit({
          expect_error(dbClearResult(res), NA)
          dbClearResult(dbSendQuery(con, "DROP TABLE test"))
        }
        , add = TRUE)

        rows <- dbFetch(res)
        expect_identical(rows, data.frame())

        expect_true(dbHasCompleted(res))
      })
    },

    # single-value queries can be read with dbGetQuery
    get_query_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L))
      })
    },

    # multi-row single-column queries can be read with dbGetQuery
    get_query_multi_row_single_column = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3 ORDER BY a"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:3L))
      })
    },

    # single-row multi-column queries can be read with dbGetQuery
    get_query_single_column_multi_row = function() {
      with_connection({
        query <- "SELECT 1 as a, 2 as b, 3 as c"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L, b=2L, c=3L))
      })
    },

    # multi-row multi-column queries can be read with dbGetQuery
    get_query_multi = function() {
      with_connection({
        query <- "SELECT 1 as a, 2 as b UNION SELECT 2, 3 ORDER BY a"

        rows <- dbGetQuery(con, query)
        expect_identical(rows, data.frame(a=1L:2L, b=2L:3L))
      })
    },

    # data conversion: integer
    data_integer = function() {
      with_connection({
        query <- "SELECT 1 as a, -100 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=1L, b=-100L))
      })
    },

    # data conversion: numeric
    data_numeric = function() {
      with_connection({
        query <- "SELECT 1.0 as a, -100.5 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=1, b=-100.5))
      })
    },

    # data conversion: logical
    data_logical = function() {
      with_connection({
        query <- "SELECT CAST(1 AS boolean) as a, cast(0 AS boolean) as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows, data.frame(a=TRUE, b=FALSE))
      })
    },

    # data conversion: NULL
    data_null = function() {
      with_connection({
        query <- "SELECT NULL as a"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_true(is.na(rows$a))
      })
    },

    # data conversion: 64-bit integers
    data_64_bit = function() {
      with_connection({
        query <- "SELECT 10000000000 as a, -10000000000 as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(as.numeric(rows$a), 10000000000)
        expect_equal(as.numeric(rows$b), -10000000000)
      })
    },

    # data conversion: character
    data_character = function() {
      with_connection({
        a <- "Кирилл"
        b <- "Müller"
        c <- "我是谁"
        d <- "ASCII"
        query <- paste0("SELECT '", a, "' as a, '",
                        b, "' as b, '",
                        c, "' as c, '",
                        d, "' as d")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(rows$a, a)
        expect_equal(rows$b, b)
        expect_equal(rows$c, c)
        expect_equal(rows$d, d)
      })
    },

    # data conversion: date
    data_date = function() {
      with_connection({
        query <- "SELECT date('2015-10-10') as a, current_date as b"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "Date")
        expect_is(rows$b, "Date")
        expect_equal(rows$a, as.Date("2015-10-10"))
      })
    },

    # data conversion: time
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

    # data conversion: time (with parentheses)
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

    # data conversion: timestamp
    data_timestamp = function() {
      with_connection({
        query <- "SELECT timestamp '2015-10-11 00:00:00' as a,
        timestamp '2015-10-11 12:34:56' as b, current_timestamp as c"

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_is(rows$a, "POSIXct")
        expect_is(rows$b, "POSIXct")
        expect_is(rows$c, "POSIXct")
        expect_equal(rows$a, as.POSIXct("2015-10-11 00:00:00Z"))
        expect_equal(rows$b, as.POSIXct("2015-10-11 12:34:56Z"))
        expect_lt(Sys.time() - rows$c, 1)
      })
    },

    # data conversion: timestamp (with parentheses)
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
        expect_lt(Sys.time() - rows$c, 1)
      })
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}

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
  ), env = env)
}
