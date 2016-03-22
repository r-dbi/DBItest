#' @name test_all
#' @section Tests:
#' \code{\link{test_meta}}:
#' Test metadata functions
NULL

#' Test metadata functions
#'
#' @inheritParams test_all
#' @include test_sql.R
#' @family tests
#' @export
test_meta <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Metadata"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{is_valid_connection}}{
    #' Only an open connection is valid.
    #' }
    is_valid_connection = function() {
      con <- connect(ctx)
      expect_true(dbIsValid(con))
      expect_error(dbDisconnect(con), NA)
      expect_false(dbIsValid(con))
    },

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
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
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
        expect_warning(res <- dbSendQuery(con, query), NA)
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        ci <- dbColumnInfo(res)
        expect_is(ci, "data.frame")
        expect_identical(colnames(ci), c("name", "type"))
        expect_identical(ci$name[1:2], c("a", "b"))
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
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 1L)
      })

      with_connection({
        query <- union(.ctx = ctx, "SELECT 1 as a", "SELECT 2", "SELECT 3")
        res <- dbSendQuery(con, query)
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 0L)
        dbFetch(res, 2L)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 2L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 3L)
      })

      with_connection({
        query <- union(
          .ctx = ctx, "SELECT * FROM (SELECT 1 as a) a WHERE (0 = 1)")
        res <- dbSendQuery(con, query)
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_equal(rc, 0L)
      })
    },

    #' \item{\code{rows_affected}}{
    #' Information on affected rows is correct.
    #' }
    rows_affected = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbGetQuery(con, "DROP TABLE iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        local({
          query <- paste0(
            "DELETE FROM iris WHERE (",
            dbQuoteIdentifier(con, "Species"),
            " = ", dbQuoteString(con, "versicolor"),
            ")")
          res <- dbSendQuery(con, query)
          on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
          ra <- dbGetRowsAffected(res)

          expect_identical(ra, sum(iris$Species == "versicolor"))
        })

        local({
          query <- "DELETE FROM iris WHERE (0 = 1)"
          res <- dbSendQuery(con, query)
          on.exit(expect_error(dbClearResult(res), NA), add = TRUE)
          ra <- dbGetRowsAffected(res)

          expect_identical(ra, 0L)
        })
      })
    },

    #' \item{\code{get_info_result}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info_result = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1 as a")
        info <- dbGetInfo(res)
        expect_is(info, "list")
        info_names <- names(info)

        necessary_names <-
          c("statement", "row.count", "rows.affected", "has.completed")

        for (name in necessary_names) {
          eval(bquote(
            expect_true(.(name) %in% info_names)))
        }
      })
    },

    #' \item{\code{bind_empty_positional_qm}}{
    #' Empty positional binding (question mark syntax) with check of
    #' return value.
    #' }
    bind_empty_positional_qm = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

        bind_res <- withVisible(dbBind(res, list()))
        expect_false(bind_res$visible)
        expect_identical(res, bind_res$value)
      })
    },

    #' \item{\code{bind_error_positional_qm}}{
    #' Positional binding of integer values (question mark syntax) raises an
    #' error if connection is closed.
    #' }
    bind_error_positional_qm = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_error(test_select_bind(con, positional_qm, 1L))
    },

    #' \item{\code{bind_return_value_positional_qm}}{
    #' Positional binding of integer values (question mark syntax) with check of
    #' return value.
    #' }
    bind_return_value_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, 1L, extra = "return_value")
      })
    },

    #' \item{\code{bind_too_many_positional_qm}}{
    #' Positional binding of integer values (question mark syntax) with too many
    #' values.
    #' }
    bind_too_many_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, 1L, extra = "too_many")
      })
    },

    #' \item{\code{bind_not_enough_positional_qm}}{
    #' Positional binding of integer values (question mark syntax) with too few
    #' values.
    #' }
    bind_not_enough_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, 1L, extra = "not_enough")
      })
    },

    #' \item{\code{bind_integer_positional_qm}}{
    #' Positional binding of integer values (question mark syntax).
    #' }
    bind_integer_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, 1L)
      })
    },

    #' \item{\code{bind_numeric_positional_qm}}{
    #' Positional binding of numeric values (question mark syntax).
    #' }
    bind_numeric_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, 1.5)
      })
    },

    #' \item{\code{bind_logical_positional_qm}}{
    #' Positional binding of logical values (question mark syntax).
    #' }
    bind_logical_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, TRUE)
      })
    },

    #' \item{\code{bind_logical_int_positional_qm}}{
    #' Positional binding of logical values (coerced to integer, question mark
    #' syntax).
    #' }
    bind_logical_int_positional_qm = function() {
      with_connection({
        test_select_bind(
          con, positional_qm, TRUE,
          transform_input = function(x) as.character(as.integer(x)))
      })
    },

    #' \item{\code{bind_null_positional_qm}}{
    #' Positional binding of \code{NULL} values (question mark syntax).
    #' }
    bind_null_positional_qm = function() {
      with_connection({
        test_select_bind(
          con, positional_qm, NA,
          transform_input = function(x) TRUE,
          transform_output = is.na)
      })
    },

    #' \item{\code{bind_character_positional_qm}}{
    #' Positional binding of character values (question mark syntax).
    #' }
    bind_character_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, texts)
      })
    },

    #' \item{\code{bind_date_positional_qm}}{
    #' Positional binding of date values (question mark syntax).
    #' }
    bind_date_positional_qm = function() {
      with_connection({
        test_select_bind(con, positional_qm, Sys.Date())
      })
    },

    #' \item{\code{bind_timestamp_positional_qm}}{
    #' Positional binding of timestamp values (question mark syntax).
    #' }
    bind_timestamp_positional_qm = function() {
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

    #' \item{\code{bind_timestamp_lt_positional_qm}}{
    #' Positional binding of \code{\link{POSIXlt}} timestamp values (question
    #' mark syntax).
    #' }
    bind_timestamp_lt_positional_qm = function() {
      with_connection({
        data_in <- as.POSIXlt(round(Sys.time()))
        test_select_bind(
          con, positional_qm, data_in,
          type = dbDataType(con, data_in),
          transform_input = as.POSIXct,
          transform_output = identity)
      })
    },

    #' \item{\code{bind_raw_positional_qm}}{
    #' Positional binding of raw values (question mark syntax).
    #' }
    bind_raw_positional_qm = function() {
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

    #' \item{\code{bind_empty_positional_dollar}}{
    #' Empty positional binding (dollar syntax) with check of
    #' return value.
    #' }
    bind_empty_positional_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

        bind_res <- withVisible(dbBind(res, list()))
        expect_false(bind_res$visible)
        expect_identical(res, bind_res$value)
      })
    },

    #' \item{\code{bind_error_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax) raises an
    #' error if connection is closed.
    #' }
    bind_error_positional_dollar = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_error(test_select_bind(con, positional_dollar, 1L))
    },

    #' \item{\code{bind_return_value_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax) with check of
    #' return value.
    #' }
    bind_return_value_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, 1L, extra = "return_value")
      })
    },

    #' \item{\code{bind_too_many_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax) with too many
    #' values.
    #' }
    bind_too_many_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, 1L, extra = "too_many")
      })
    },

    #' \item{\code{bind_not_enough_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax) with too few
    #' values.
    #' }
    bind_not_enough_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, 1L, extra = "not_enough")
      })
    },

    #' \item{\code{bind_integer_positional_dollar}}{
    #' Positional binding of integer values (dollar syntax).
    #' }
    bind_integer_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, 1L)
      })
    },

    #' \item{\code{bind_numeric_positional_dollar}}{
    #' Positional binding of numeric values (dollar syntax).
    #' }
    bind_numeric_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, 1.5)
      })
    },

    #' \item{\code{bind_logical_positional_dollar}}{
    #' Positional binding of logical values (dollar syntax).
    #' }
    bind_logical_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, TRUE)
      })
    },

    #' \item{\code{bind_logical_int_positional_dollar}}{
    #' Positional binding of logical values (coerced to integer, dollar
    #' syntax).
    #' }
    bind_logical_int_positional_dollar = function() {
      with_connection({
        test_select_bind(
          con, positional_dollar, TRUE,
          transform_input = function(x) as.character(as.integer(x)))
      })
    },

    #' \item{\code{bind_null_positional_dollar}}{
    #' Positional binding of \code{NULL} values (dollar syntax).
    #' }
    bind_null_positional_dollar = function() {
      with_connection({
        test_select_bind(
          con, positional_dollar, NA,
          transform_input = function(x) TRUE,
          transform_output = is.na)
      })
    },

    #' \item{\code{bind_character_positional_dollar}}{
    #' Positional binding of character values (dollar syntax).
    #' }
    bind_character_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, texts)
      })
    },

    #' \item{\code{bind_date_positional_dollar}}{
    #' Positional binding of date values (dollar syntax).
    #' }
    bind_date_positional_dollar = function() {
      with_connection({
        test_select_bind(con, positional_dollar, Sys.Date())
      })
    },

    #' \item{\code{bind_timestamp_positional_dollar}}{
    #' Positional binding of timestamp values (dollar syntax).
    #' }
    bind_timestamp_positional_dollar = function() {
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

    #' \item{\code{bind_timestamp_lt_positional_dollar}}{
    #' Positional binding of \code{\link{POSIXlt}} timestamp values (dollar
    #' syntax).
    #' }
    bind_timestamp_lt_positional_dollar = function() {
      with_connection({
        data_in <- as.POSIXlt(round(Sys.time()))
        test_select_bind(
          con, positional_dollar, data_in,
          type = dbDataType(con, data_in),
          transform_input = as.POSIXct,
          transform_output = identity)
      })
    },

    #' \item{\code{bind_raw_positional_dollar}}{
    #' Positional binding of raw values (dollar syntax).
    #' }
    bind_raw_positional_dollar = function() {
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

    #' \item{\code{bind_empty_named_colon}}{
    #' Empty named binding (colon syntax) with check of
    #' return value.
    #' }
    bind_empty_named_colon = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

        bind_res <- withVisible(dbBind(res, list()))
        expect_false(bind_res$visible)
        expect_identical(res, bind_res$value)
      })
    },

    #' \item{\code{bind_error_named_colon}}{
    #' named binding of integer values (colon syntax) raises an
    #' error if connection is closed.
    #' }
    bind_error_named_colon = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_error(test_select_bind(con, named_colon, 1L))
    },

    #' \item{\code{bind_return_value_named_colon}}{
    #' named binding of integer values (colon syntax) with check of
    #' return value.
    #' }
    bind_return_value_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1L, extra = "return_value")
      })
    },

    #' \item{\code{bind_too_many_named_colon}}{
    #' named binding of integer values (colon syntax) with too many
    #' values.
    #' }
    bind_too_many_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1L, extra = "too_many")
      })
    },

    #' \item{\code{bind_not_enough_named_colon}}{
    #' named binding of integer values (colon syntax) with too few
    #' values.
    #' }
    bind_not_enough_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1L, extra = "not_enough")
      })
    },

    #' \item{\code{bind_wrong_name_named_colon}}{
    #' named binding of integer values (colon syntax) with wrong names.
    #' }
    bind_wrong_name_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1L, extra = "wrong_name")
      })
    },

    #' \item{\code{bind_integer_named_colon}}{
    #' Named binding of integer values (colon syntax).
    #' }
    bind_integer_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1L)
      })
    },

    #' \item{\code{bind_numeric_named_colon}}{
    #' Named binding of numeric values (colon syntax).
    #' }
    bind_numeric_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, 1.5)
      })
    },

    #' \item{\code{bind_logical_named_colon}}{
    #' Named binding of logical values (colon syntax).
    #' }
    bind_logical_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, TRUE)
      })
    },

    #' \item{\code{bind_logical_int_named_colon}}{
    #' Named binding of logical values (coerced to integer, colon
    #' syntax).
    #' }
    bind_logical_int_named_colon = function() {
      with_connection({
        test_select_bind(
          con, named_colon, TRUE,
          transform_input = function(x) as.character(as.integer(x)))
      })
    },

    #' \item{\code{bind_null_named_colon}}{
    #' Named binding of \code{NULL} values (colon syntax).
    #' }
    bind_null_named_colon = function() {
      with_connection({
        test_select_bind(
          con, named_colon, NA,
          transform_input = function(x) TRUE,
          transform_output = is.na)
      })
    },

    #' \item{\code{bind_character_named_colon}}{
    #' Named binding of character values (colon syntax).
    #' }
    bind_character_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, texts)
      })
    },

    #' \item{\code{bind_date_named_colon}}{
    #' Named binding of date values (colon syntax).
    #' }
    bind_date_named_colon = function() {
      with_connection({
        test_select_bind(con, named_colon, Sys.Date())
      })
    },

    #' \item{\code{bind_timestamp_named_colon}}{
    #' Named binding of timestamp values (colon syntax).
    #' }
    bind_timestamp_named_colon = function() {
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

    #' \item{\code{bind_timestamp_lt_named_colon}}{
    #' Named binding of \code{\link{POSIXlt}} timestamp values (colon
    #' syntax).
    #' }
    bind_timestamp_lt_named_colon = function() {
      if (isTRUE(ctx$tweaks$omit_blob_tests)) {
        skip("tweak: omit_blob_tests")
      }

      with_connection({
        data_in <- as.POSIXlt(round(Sys.time()))
        test_select_bind(
          con, named_colon, data_in,
          type = dbDataType(con, data_in),
          transform_input = as.POSIXct,
          transform_output = identity)
      })
    },

    #' \item{\code{bind_raw_named_colon}}{
    #' Named binding of raw values (colon syntax).
    #' }
    bind_raw_named_colon = function() {
      with_connection({
        test_select_bind(
          con, named_colon, list(list(as.raw(1:10))),
          type = NULL,
          transform_input = function(x) x[[1L]],
          transform_output = identity)
      })
    },

    #' \item{\code{bind_empty_named_dollar}}{
    #' Empty named binding (dollar syntax) with check of
    #' return value.
    #' }
    bind_empty_named_dollar = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

        bind_res <- withVisible(dbBind(res, list()))
        expect_false(bind_res$visible)
        expect_identical(res, bind_res$value)
      })
    },

    #' \item{\code{bind_error_named_dollar}}{
    #' named binding of integer values (dollar syntax) raises an
    #' error if connection is closed.
    #' }
    bind_error_named_dollar = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_error(test_select_bind(con, named_dollar, 1L))
    },

    #' \item{\code{bind_return_value_named_dollar}}{
    #' named binding of integer values (dollar syntax) with check of
    #' return value.
    #' }
    bind_return_value_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1L, extra = "return_value")
      })
    },

    #' \item{\code{bind_too_many_named_dollar}}{
    #' named binding of integer values (dollar syntax) with too many
    #' values.
    #' }
    bind_too_many_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1L, extra = "too_many")
      })
    },

    #' \item{\code{bind_not_enough_named_dollar}}{
    #' named binding of integer values (dollar syntax) with too few
    #' values.
    #' }
    bind_not_enough_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1L, extra = "not_enough")
      })
    },

    #' \item{\code{bind_wrong_name_named_dollar}}{
    #' named binding of integer values (dollar syntax) with wrong names.
    #' }
    bind_wrong_name_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1L, extra = "wrong_name")
      })
    },

    #' \item{\code{bind_integer_named_dollar}}{
    #' Named binding of integer values (dollar syntax).
    #' }
    bind_integer_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1L)
      })
    },

    #' \item{\code{bind_numeric_named_dollar}}{
    #' Named binding of numeric values (dollar syntax).
    #' }
    bind_numeric_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, 1.5)
      })
    },

    #' \item{\code{bind_logical_named_dollar}}{
    #' Named binding of logical values (dollar syntax).
    #' }
    bind_logical_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, TRUE)
      })
    },

    #' \item{\code{bind_logical_int_named_dollar}}{
    #' Named binding of logical values (coerced to integer, dollar
    #' syntax).
    #' }
    bind_logical_int_named_dollar = function() {
      with_connection({
        test_select_bind(
          con, named_dollar, TRUE,
          transform_input = function(x) as.character(as.integer(x)))
      })
    },

    #' \item{\code{bind_null_named_dollar}}{
    #' Named binding of \code{NULL} values (dollar syntax).
    #' }
    bind_null_named_dollar = function() {
      with_connection({
        test_select_bind(
          con, named_dollar, NA,
          transform_input = function(x) TRUE,
          transform_output = is.na)
      })
    },

    #' \item{\code{bind_character_named_dollar}}{
    #' Named binding of character values (dollar syntax).
    #' }
    bind_character_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, texts)
      })
    },

    #' \item{\code{bind_date_named_dollar}}{
    #' Named binding of date values (dollar syntax).
    #' }
    bind_date_named_dollar = function() {
      with_connection({
        test_select_bind(con, named_dollar, Sys.Date())
      })
    },

    #' \item{\code{bind_timestamp_named_dollar}}{
    #' Named binding of timestamp values (dollar syntax).
    #' }
    bind_timestamp_named_dollar = function() {
      with_connection({
        data_in <- as.POSIXct(round(Sys.time()))
        test_select_bind(
          con, named_dollar, data_in,
          type = dbDataType(con, data_in),
          transform_input = identity,
          transform_output = identity,
          expect = expect_equal)
      })
    },

    #' \item{\code{bind_timestamp_lt_named_dollar}}{
    #' Named binding of \code{\link{POSIXlt}} timestamp values (dollar
    #' syntax).
    #' }
    bind_timestamp_lt_named_dollar = function() {
      with_connection({
        data_in <- as.POSIXlt(round(Sys.time()))
        test_select_bind(
          con, named_dollar, data_in,
          type = dbDataType(con, data_in),
          transform_input = as.POSIXct,
          transform_output = identity)
      })
    },

    #' \item{\code{bind_raw_named_dollar}}{
    #' Named binding of raw values (dollar syntax).
    #' }
    bind_raw_named_dollar = function() {
      if (isTRUE(ctx$tweaks$omit_blob_tests)) {
        skip("tweak: omit_blob_tests")
      }

      with_connection({
        test_select_bind(
          con, named_dollar, list(list(as.raw(1:10))),
          type = NULL,
          transform_input = function(x) x[[1L]],
          transform_output = identity)
      })
    },

    # dbHasCompleted tested in test_result

    # no 64-bit or time input data type yet

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite, ctx$name)
}

test_select_bind <- function(con, placeholder_fun, values,
                             type = "character(10)",
                             transform_input = as.character,
                             transform_output = function(x) trimws(x, "right"),
                             expect = expect_identical,
                             extra = c("none", "return_value", "too_many",
                                       "not_enough", "wrong_name")) {
  extra <- match.arg(extra)

  placeholder <- placeholder_fun(length(values))

  value_names <- letters[seq_along(values)]
  if (is.null(type)) {
    typed_placeholder <- placeholder
  } else {
    typed_placeholder <- paste0("cast(", placeholder, " as ", type, ")")
  }
  query <- paste0("SELECT ", paste0(
    typed_placeholder, " as ", value_names, collapse = ", "))
  res <- dbSendQuery(con, query)
  on.exit(expect_error(dbClearResult(res), NA))

  bind_values <- values
  if (!is.null(names(placeholder))) {
    names(bind_values) <- names(placeholder)
  }

  error_bind_values <- switch(
    extra,
    too_many = c(bind_values, bind_values[[1L]]),
    not_enough = bind_values[-1L],
    wrong_name = stats::setNames(bind_values, paste0("bogus",
                                                     names(bind_values))),
    bind_values
  )

  if (!identical(bind_values, error_bind_values)) {
    expect_error(dbBind(res, as.list(error_bind_values)))
    return()
  }

  bind_res <- withVisible(dbBind(res, as.list(bind_values)))
  if (extra == "return_value") {
    expect_false(bind_res$visible)
    expect_identical(res, bind_res$value)
  }

  rows <- dbFetch(res)
  expect(transform_output(Reduce(c, rows)), transform_input(unname(values)))
}

positional_qm <- function(n) {
  "?"
}

positional_dollar <- function(n) {
  paste0("$", seq_len(n))
}

named_dollar <- function(n) {
  l <- letters[seq_len(n)]
  stats::setNames(paste0("$", l), l)
}

named_colon <- function(n) {
  l <- letters[seq_len(n)]
  stats::setNames(paste0(":", l), l)
}
