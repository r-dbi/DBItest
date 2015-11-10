#' @name test_all
#' @section Tests:
#' \code{\link{test_result_meta}}:
#' Test metadata functions for the "Result" class
NULL

#' Test metadata functions for the "Result" class
#'
#' @inheritParams test_all
#' @include test_connection_meta.R
#' @family tests
#' @export
test_result_meta <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result (metadata)"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{is_valid_result}}{
    #' Only an open result set is valid.
    #' }
    is_valid_result = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        expect_true(dbIsValid(res))
        expect_error(dbFetch(res), NA)
        expect_true(dbIsValid(res))
        dbClearResult(res)
        expect_false(dbIsValid(res))
      })
    },

    #' \item{\code{get_statement}}{
    #' SQL query can be retrieved from the result.
    #' }
    get_statement = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        s <- dbGetStatement(res)
        expect_is(s, "character")
        expect_identical(s, query)
      })
    },

    #' \item{\code{column_info}}{
    #' Column information is correct.
    #' }
    column_info = function() {
      with_connection({
        query <- "SELECT 1 as a, 1.5 as b, NULL"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        ci <- dbColumnInfo(res)
        expect_is(ci, "data.frame")
        expect_equal(colnames(ci), c("name", "type"))
        expect_equal(ci$name[1:2], c("a", "b"))
        expect_is(ci$type, "character")
      })
    },

    #' \item{\code{row_count}}{
    #' Row count information is correct.
    #' }
    row_count = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 1L)
      })

      with_connection({
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3")
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res, 2L)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 2L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 3L)
      })

      with_connection({
        query <- union("SELECT * FROM (SELECT 1 as a) a WHERE (0 = 1)")
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
      })
    },

    #' \item{\code{rows_affected}}{
    #' Information on affected rows is correct.
    #' }
    rows_affected = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(dbGetQuery(con, "DROP TABLE iris"), add = TRUE)
        dbWriteTable(con, "iris", iris)

        local({
          query <- paste0(
            "DELETE FROM iris WHERE (",
            dbQuoteIdentifier(con, "Species"),
            " = ", dbQuoteString(con, "versicolor"),
            ")")
          res <- dbSendQuery(con, query)
          on.exit(dbClearResult(res), add = TRUE)
          ra <- dbGetRowsAffected(res)

          expect_is(ra, "integer")
          expect_identical(ra, sum(iris$Species == "versicolor"))
        })

        local({
          query <- "DELETE FROM iris WHERE (0 = 1)"
          res <- dbSendQuery(con, query)
          on.exit(dbClearResult(res), add = TRUE)
          ra <- dbGetRowsAffected(res)

          expect_is(ra, "integer")
          expect_identical(ra, 0L)
        })
      })
    },

    #' \item{\code{bind_integer_positional_qm}}{
    #' Positional binding of integer values (question mark syntax).
    #' }
    bind_integer_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1L
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_numeric_positional_qm}}{
    #' Positional binding of numeric values (question mark syntax).
    #' }
    bind_numeric_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1.5
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_logical_positional_qm}}{
    #' Positional binding of logical values (question mark syntax).
    #' }
    bind_logical_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT ? as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_logical_int_positional_qm}}{
    #' Positional binding of logical values (coerced to integer, question mark
    #' syntax).
    #' }
    bind_logical_int_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, as.character(as.integer(data_in)))
      })
    },

    #' \item{\code{bind_null_positional_qm}}{
    #' Positional binding of \code{NULL} values (question mark syntax)..
    #' }
    bind_null_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(NA))

        rows <- dbFetch(res)
        expect_true(is.na(rows$a))
      })
    },

    #' \item{\code{bind_character_positional_qm}}{
    #' Positional binding of character values (question mark syntax)..
    #' }
    bind_character_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(
          con,
          "SELECT cast(? as character(10)) as a, cast(? as character(10)) as b,
           cast(? as character(10)) as c, cast(? as character(10)) as d")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(text_cyrillic, text_latin, text_chinese, text_ascii))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), text_cyrillic)
        expect_identical(trimws(rows$b, "right"), text_latin)
        expect_identical(trimws(rows$c, "right"), text_chinese)
        expect_identical(trimws(rows$d, "right"), text_ascii)
      })
    },

    #' \item{\code{bind_date_positional_qm}}{
    #' Positional binding of date values (question mark syntax)..
    #' }
    bind_date_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- Sys.Date()
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_timestamp_positional_qm}}{
    #' Positional binding of timestamp values (question mark syntax)..
    #' }
    bind_timestamp_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as datetime) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXct(round(Sys.time()))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_equal(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_lt_positional_qm}}{
    #' Positional binding of \code{\link{POSIXlt}} timestamp values (question
    #' mark syntax)..
    #' }
    bind_timestamp_lt_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast(? as datetime) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXlt(round(Sys.time()))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_raw_positional_qm}}{
    #' Positional binding of raw values (question mark syntax)..
    #' }
    bind_raw_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT ? as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- list(as.raw(1:10))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },


    #' \item{\code{bind_integer_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax).
    #' }
    bind_integer_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1L
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_numeric_positional_dollar}}{
    #' Positional binding of numeric values (dollar syntax).
    #' }
    bind_numeric_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1.5
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_logical_positional_dollar}}{
    #' Positional binding of logical values (dollar syntax).
    #' }
    bind_logical_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_logical_int_positional_dollar}}{
    #' Positional binding of logical values (coerced to integer, dollar
    #' syntax).
    #' }
    bind_logical_int_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, as.character(as.integer(data_in)))
      })
    },

    #' \item{\code{bind_null_positional_dollar}}{
    #' Positional binding of \code{NULL} values (dollar syntax)..
    #' }
    bind_null_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(NA))

        rows <- dbFetch(res)
        expect_true(is.na(rows$a))
      })
    },

    #' \item{\code{bind_character_positional_dollar}}{
    #' Positional binding of character values (dollar syntax)..
    #' }
    bind_character_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(
          con,
          "SELECT cast($1 as character(10)) as a, cast($2 as character(10)) as b,
          cast($3 as character(10)) as c, cast($4 as character(10)) as d")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(text_cyrillic, text_latin, text_chinese, text_ascii))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), text_cyrillic)
        expect_identical(trimws(rows$b, "right"), text_latin)
        expect_identical(trimws(rows$c, "right"), text_chinese)
        expect_identical(trimws(rows$d, "right"), text_ascii)
      })
    },

    #' \item{\code{bind_date_positional_dollar}}{
    #' Positional binding of date values (dollar syntax)..
    #' }
    bind_date_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as character(10)) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- Sys.Date()
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(trimws(rows$a, "right"), as.character(data_in))
      })
    },

    #' \item{\code{bind_timestamp_positional_dollar}}{
    #' Positional binding of timestamp values (dollar syntax)..
    #' }
    bind_timestamp_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as datetime) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXct(round(Sys.time()))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_equal(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_lt_positional_dollar}}{
    #' Positional binding of \code{\link{POSIXlt}} timestamp values (question
    #' mark syntax)..
    #' }
    bind_timestamp_lt_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT cast($1 as datetime) as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXlt(round(Sys.time()))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_raw_positional_dollar}}{
    #' Positional binding of raw values (dollar syntax)..
    #' }
    bind_raw_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $1 as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- list(as.raw(1:10))
        dbBind(res, list(data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_integer_named_colon}}{
    #' Named binding (colon syntax) of integer values.
    #' }
    bind_integer_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1L
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_numeric_named_colon}}{
    #' Named binding (colon syntax) of numeric values.
    #' }
    bind_numeric_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1.5
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_logical_named_colon}}{
    #' Named binding (colon syntax) of logical values.
    #' }
    bind_logical_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_logical_int_named_colon}}{
    #' Named binding (colon syntax) of logical values (coerced to integer).
    #' }
    bind_logical_int_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, as.integer(data_in))
      })
    },

    #' \item{\code{bind_null_named_colon}}{
    #' Named binding (colon syntax) of \code{NULL} values.
    #' }
    bind_null_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(a = NA))

        rows <- dbFetch(res)
        expect_true(is.na(rows$a))
      })
    },

    #' \item{\code{bind_character_named_colon}}{
    #' Named binding (colon syntax) of character values.
    #' }
    bind_character_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a, :b as b, :c as c, :d as d")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(a = text_cyrillic, b = text_latin, c = text_chinese,
                         d = text_ascii))

        rows <- dbFetch(res)
        expect_identical(rows$a, text_cyrillic)
        expect_identical(rows$b, text_latin)
        expect_identical(rows$c, text_chinese)
        expect_identical(rows$d, text_ascii)
      })
    },

    #' \item{\code{bind_date_named_colon}}{
    #' Named binding (colon syntax) of date values.
    #' }
    bind_date_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- Sys.Date()
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_named_colon}}{
    #' Named binding (colon syntax) of timestamp values.
    #' }
    bind_timestamp_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXct(round(Sys.time()))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_lt_named_colon}}{
    #' Named binding (colon syntax) of \code{\link{POSIXlt}} timestamp values.
    #' }
    bind_timestamp_lt_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXlt(round(Sys.time()))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_raw_named_colon}}{
    #' Named binding (colon syntax) of raw values.
    #' }
    bind_raw_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT :a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- list(as.raw(1:10))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_integer_named_dollar}}{
    #' Named binding (dollar syntax) of integer values.
    #' }
    bind_integer_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1L
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_numeric_named_dollar}}{
    #' Named binding (dollar syntax) of numeric values.
    #' }
    bind_numeric_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- 1.5
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_logical_named_dollar}}{
    #' Named binding (dollar syntax) of logical values.
    #' }
    bind_logical_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_logical_int_named_dollar}}{
    #' Named binding (dollar syntax) of logical values (coerced to integer).
    #' }
    bind_logical_int_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- TRUE
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, as.integer(data_in))
      })
    },

    #' \item{\code{bind_null_named_dollar}}{
    #' Named binding (dollar syntax) of \code{NULL} values.
    #' }
    bind_null_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(a = NA))

        rows <- dbFetch(res)
        expect_true(is.na(rows$a))
      })
    },

    #' \item{\code{bind_character_named_dollar}}{
    #' Named binding (dollar syntax) of character values.
    #' }
    bind_character_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a, $b as b, $c as c, $d as d")
        on.exit(expect_error(dbClearResult(res), NA))

        dbBind(res, list(a = text_cyrillic, b = text_latin, c = text_chinese,
                         d = text_ascii))

        rows <- dbFetch(res)
        expect_identical(rows$a, text_cyrillic)
        expect_identical(rows$b, text_latin)
        expect_identical(rows$c, text_chinese)
        expect_identical(rows$d, text_ascii)
      })
    },

    #' \item{\code{bind_date_named_dollar}}{
    #' Named binding (dollar syntax) of date values.
    #' }
    bind_date_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- Sys.Date()
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_named_dollar}}{
    #' Named binding (dollar syntax) of timestamp values.
    #' }
    bind_timestamp_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXlt(round(Sys.time()))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_timestamp_lt_named_dollar}}{
    #' Named binding (dollar syntax) of \code{\link{POSIXlt}} timestamp values.
    #' }
    bind_timestamp_lt_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- as.POSIXct(round(Sys.time()))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    #' \item{\code{bind_raw_named_dollar}}{
    #' Named binding (dollar syntax) of raw values.
    #' }
    bind_raw_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT $a as a")
        on.exit(expect_error(dbClearResult(res), NA))

        data_in <- list(as.raw(1:10))
        dbBind(res, list(a = data_in))

        rows <- dbFetch(res)
        expect_identical(rows$a, data_in)
      })
    },

    # dbHasCompleted tested in test_result

    # no 64-bit or time input data type yet

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
