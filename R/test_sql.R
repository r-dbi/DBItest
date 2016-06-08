#' @name test_all
#' @section Tests:
#' \code{\link{test_sql}}:
#' Test SQL methods
NULL

#' Test SQL methods
#'
#' @inheritParams test_all
#' @include test_result.R
#' @family tests
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{quote_string}}{
    #' Can quote strings, and create strings that contain quotes and spaces
    #' }
    quote_string = function() {
      with_connection({
        simple <- dbQuoteString(con, "simple")
        with_spaces <- dbQuoteString(con, "with spaces")
        quoted_simple <- dbQuoteString(con, as.character(simple))
        quoted_with_spaces <- dbQuoteString(con, as.character(with_spaces))
        null <- dbQuoteString(con, NA_character_)
        quoted_null <- dbQuoteString(con, as.character(null))
        na <- dbQuoteString(con, "NA")
        quoted_na <- dbQuoteString(con, as.character(na))

        query <- paste0("SELECT",
                        simple, "as simple,",
                        with_spaces, "as with_spaces,",
                        null, " as null_return,",
                        na, "as na_return,",
                        quoted_simple, "as quoted_simple,",
                        quoted_with_spaces, "as quoted_with_spaces,",
                        quoted_null, "as quoted_null,",
                        quoted_na, "as quoted_na")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(rows$simple, "simple")
        expect_identical(rows$with_spaces, "with spaces")
        expect_true(is.na(rows$null))
        expect_identical(rows$na_return, "NA")
        expect_identical(rows$quoted_simple, as.character(simple))
        expect_identical(rows$quoted_with_spaces, as.character(with_spaces))
        expect_identical(rows$quoted_null, as.character(null))
        expect_identical(rows$quoted_na, as.character(na))
      })
    },

    #' \item{\code{quote_string_vectorized}}{
    #' Can quote more than one string at once by passing a character vector.
    #'
    #' }
    quote_string_vectorized = function() {
      with_connection({
        simple_out <- dbQuoteString(con, "simple")
        expect_equal(length(simple_out), 1L)
        letters_out <- dbQuoteString(con, letters)
        expect_equal(length(letters_out), length(letters))
      })
    },

    #' \item{\code{quote_identifier}}{
    #' Can quote identifiers that consist of letters only
    #' }
    quote_identifier = function() {
      with_connection({
        simple <- dbQuoteIdentifier(con, "simple")

        query <- paste0("SELECT 1 as", simple)

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(names(rows), "simple")
        expect_identical(unlist(unname(rows)), 1L)
      })
    },

    #' \item{\code{quote_identifier_special}}{
    #' Can quote identifiers with special characters, and create identifiers
    #' that contain quotes and spaces
    #' }
    quote_identifier_special = function() {
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        skip("tweak: strict_identifier")
      }

      with_connection({
        simple <- dbQuoteIdentifier(con, "simple")
        with_space <- dbQuoteIdentifier(con, "with space")
        with_dot <- dbQuoteIdentifier(con, "with.dot")
        with_comma <- dbQuoteIdentifier(con, "with,comma")
        quoted_simple <- dbQuoteIdentifier(con, as.character(simple))
        quoted_with_space <- dbQuoteIdentifier(con, as.character(with_space))
        quoted_with_dot <- dbQuoteIdentifier(con, as.character(with_dot))
        quoted_with_comma <- dbQuoteIdentifier(con, as.character(with_comma))

        query <- paste0("SELECT ",
                        "1 as", simple, ",",
                        "2 as", with_space, ",",
                        "3 as", with_dot, ",",
                        "4 as", with_comma, ",",
                        "5 as", quoted_simple, ",",
                        "6 as", quoted_with_space, ",",
                        "7 as", quoted_with_dot, ",",
                        "8 as", quoted_with_comma)

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_identical(names(rows),
                         c("simple", "with space", "with.dot", "with,comma",
                           as.character(simple), as.character(with_space),
                           as.character(with_dot), as.character(with_comma)))
        expect_identical(unlist(unname(rows)), 1:8)
      })
    },

    #' \item{\code{quote_identifier_not_vectorized}}{
    #' Character vectors are treated as a single qualified identifier.
    #' }
    quote_identifier_not_vectorized = function() {
      with_connection({
        simple_out <- dbQuoteIdentifier(con, "simple")
        expect_equal(length(simple_out), 1L)
        letters_out <- dbQuoteIdentifier(con, letters[1:3])
        expect_equal(length(letters_out), 1L)
      })
    },

    #' \item{\code{write_table}}{
    #' Can write the \code{\link[datasets]{iris}} data as a table to the
    #' database, but won't overwrite by default.
    #' }
    write_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris))

        with_connection({
          expect_error(dbGetQuery(con2, "SELECT * FROM iris"), NA)
        }
        , con = "con2")
      })

      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      })
    },

    #' \item{\code{read_table}}{
    #' Can read the \code{\link[datasets]{iris}} data from a database table.
    #' }
    read_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris_in <- get_iris(ctx)
        iris_in$Species <- as.character(iris_in$Species)
        order_in <- do.call(order, iris_in)

        dbWriteTable(con, "iris", iris_in)
        iris_out <- dbReadTable(con, "iris")
        order_out <- do.call(order, iris_out)

        expect_identical(iris_in[order_in, ], iris_out[order_out, ])
      })
    },

    #' \item{\code{overwrite_table}}{
    #' Can write the \code{\link[datasets]{iris}} data as a table to the
    #' database, will overwrite if asked.
    #' }
    overwrite_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE),
                     NA)
        iris_out <- dbReadTable(con, "iris")
        expect_identical(nrow(iris_out), 10L)
      })
    },

    #' \item{\code{append_table}}{
    #' Can write the \code{\link[datasets]{iris}} data as a table to the
    #' database, will append if asked.
    #' }
    append_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], append = TRUE), NA)
        iris_out <- dbReadTable(con, "iris")
        expect_identical(nrow(iris_out), nrow(iris) + 10L)
      })
    },

    #' \item{\code{append_table}}{
    #' Cannot append to nonexisting table.
    #' }
    append_table_error = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(expect_error(dbRemoveTable(con, "iris")))

        iris <- get_iris(ctx)
        expect_error(dbWriteTable(con, "iris", iris[1:20,], append = TRUE))
      })
    },

    #' \item{\code{temporary_table}}{
    #' Can write the \code{\link[datasets]{iris}} data as a temporary table to
    #' the database, the table is not available in a second connection and is
    #' gone after reconnecting.
    #' }
    temporary_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris[1:30, ], temporary = TRUE)
        iris_out <- dbReadTable(con, "iris")
        expect_identical(nrow(iris_out), 30L)

        with_connection({
          expect_error(dbGetQuery(con2, "SELECT * FROM iris"))
        }
        , con = "con2")
      })

      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        try(dbRemoveTable(con, "iris"), silent = TRUE)
      })
    },

    #' \item{\code{table_visible_in_other_connection}}{
    #' A new table is visible in a second connection.
    #' }
    table_visible_in_other_connection = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * from test"))

        on.exit(expect_error(dbRemoveTable(con, "test"), NA),
                add = TRUE)

        data <- data.frame(a = 1L)
        dbWriteTable(con, "test", data)

        with_connection({
          expect_error(rows <- dbGetQuery(con2, "SELECT * FROM test"), NA)
          expect_identical(rows, data)
        }
        , con = "con2")
      })
    },

    #' \item{\code{list_tables}}{
    #' Can list the tables in the database, adding and removing tables affects
    #' the list. Can also check existence of a table.
    #' }
    list_tables = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))

        tables <- dbListTables(con)
        expect_is(tables, "character")
        expect_false("iris" %in% tables)

        expect_false(dbExistsTable(con, "iris"))

        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        tables <- dbListTables(con)
        expect_true("iris" %in% tables)

        expect_true(dbExistsTable(con, "iris"))

        dbRemoveTable(con, "iris")
        on.exit(NULL, add = FALSE)

        tables <- dbListTables(con)
        expect_false("iris" %in% tables)

        expect_false(dbExistsTable(con, "iris"))
      })
    },

    #' \item{\code{list_fields}}{
    #' Can list the fields for a table in the database.
    #' }
    list_fields = function() {
      with_connection({
        on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
                add = TRUE)

        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        fields <- dbListFields(con, "iris")
        expect_identical(fields, names(iris))
      })
    },

    #' \item{\code{roundtrip_keywords}}{
    #' Can create tables with keywords as table and column names.
    #' }
    roundtrip_keywords = function() {
      with_connection({
        tbl_in <- data.frame(SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
                             stringsAsFactors = FALSE)

        on.exit(expect_error(dbRemoveTable(con, "EXISTS"), NA), add = TRUE)
        dbWriteTable(con, "EXISTS", tbl_in)

        tbl_out <- dbReadTable(con, "EXISTS")
        expect_identical(tbl_in, tbl_out)
      })
    },

    #' \item{\code{roundtrip_quotes}}{
    #' Can create tables with quotes, commas, and spaces in column names and
    #' data.
    #' }
    roundtrip_quotes = function() {
      with_connection({
        tbl_in <- data.frame(a = as.character(dbQuoteString(con, "")),
                             b = as.character(dbQuoteIdentifier(con, "")),
                             c = "with space",
                             d = ",",
                             stringsAsFactors = FALSE)

        if (!isTRUE(ctx$tweaks$strict_identifier)) {
          names(tbl_in) <- c(
            as.character(dbQuoteIdentifier(con, "")),
            as.character(dbQuoteString(con, "")),
            "with space",
            ",")
        }

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out)
      })
    },

    #' \item{\code{roundtrip_integer}}{
    #' Can create tables with integer columns.
    #' }
    roundtrip_integer = function() {
      with_connection({
        tbl_in <- data.frame(a = c(1:5, NA), id = 1:6)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_numeric}}{
    #' Can create tables with numeric columns.
    #' }
    roundtrip_numeric = function() {
      with_connection({
        tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA), id = 1:6)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_numeric_special}}{
    #' Can create tables with numeric columns that contain special values such
    #' as \code{Inf} and \code{NaN}.
    #' }
    roundtrip_numeric_special = function() {
      with_connection({
        tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA, -Inf, Inf, NaN),
                             id = 1:9)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal(tbl_in$a, tbl_out$a[order(tbl_out$id)])
      })
    },

    #' \item{\code{roundtrip_logical}}{
    #' Can create tables with logical columns.
    #' }
    roundtrip_logical = function() {
      with_connection({
        tbl_in <- data.frame(a = c(TRUE, FALSE, NA), id = 1:3)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_logical_int}}{
    #' Can create tables with logical columns, returned as integer.
    #' }
    roundtrip_logical_int = function() {
      with_connection({
        tbl_in <- data.frame(a = c(TRUE, FALSE, NA), id = 1:3)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(as.integer(tbl_in$a), tbl_out$a[order(tbl_out$id)])
      })
    },

    #' \item{\code{roundtrip_null}}{
    #' Can create tables with NULL values.
    #' }
    roundtrip_null = function() {
      with_connection({
        tbl_in <- data.frame(a = NA)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_true(is.na(tbl_out$a))
      })
    },

    #' \item{\code{roundtrip_64_bit}}{
    #' Can create tables with 64-bit columns.
    #' }
    roundtrip_64_bit = function() {
      with_connection({
        tbl_in <- data.frame(a = c(-1e14, 1e15, 0.25, NA), id = 1:4)
        tbl_in_trunc <- data.frame(a = trunc(tbl_in$a))

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in, field.types = "bigint")

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in_trunc, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_character}}{
    #' Can create tables with character columns.
    #' }
    roundtrip_character = function() {
      with_connection({
        tbl_in <- data.frame(a = c(text_cyrillic, text_latin,
                                   text_chinese, text_ascii, NA),
                             id = 1:5, stringsAsFactors = FALSE)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_factor}}{
    #' Can create tables with factor columns.
    #' }
    roundtrip_factor = function() {
      with_connection({
        tbl_in <- data.frame(a = factor(c(text_cyrillic, text_latin,
                                          text_chinese, text_ascii, NA)),
                             id = 1:5, stringsAsFactors = FALSE)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(as.character(tbl_in$a), tbl_out$a[order(tbl_out$id)])
      })
    },

    #' \item{\code{roundtrip_raw}}{
    #' Can create tables with raw columns.
    #' }
    roundtrip_raw = function() {
      if (isTRUE(ctx$tweaks$omit_blob_tests)) {
        skip("tweak: omit_blob_tests")
      }

      with_connection({
        tbl_in <- list(a = list(as.raw(1:10), NA), id = 1:2)
        tbl_in <- structure(tbl_in, class = "data.frame",
                            row.names = c(NA, -2L))

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_date}}{
    #' Can create tables with date columns.
    #' }
    roundtrip_date = function() {
      with_connection({
        tbl_in <- data.frame(id = 1:6)
        tbl_in$a <- c(Sys.Date() + 1:5, NA)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal(tbl_in, tbl_out[order(tbl_out$id), ])
        expect_is(unclass(tbl_out$a), "integer")
      })
    },

    #' \item{\code{roundtrip_timestamp}}{
    #' Can create tables with timestamp columns.
    #' }
    roundtrip_timestamp = function() {
      with_connection({
        tbl_in <- data.frame(id = 1:5)
        tbl_in$a <- round(Sys.time()) + c(1, 60, 3600, 86400, NA)
        tbl_in$b <- as.POSIXlt(tbl_in$a, tz = "GMT")
        tbl_in$c <- as.POSIXlt(tbl_in$a, tz = "PST")

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
      })
    },

    #' \item{\code{roundtrip_rownames}}{
    #' Can create tables with row names.
    #' }
    roundtrip_rownames = function() {
      with_connection({
        tbl_in <- data.frame(a = c(1:5, NA),
                             row.names = paste0(LETTERS[1:6], 1:6),
                             id = 1:6)

        on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_identical(rownames(tbl_in), rownames(tbl_out)[order(tbl_out$id)])
      })
    },

    NULL
  )
  #' }
  run_tests(tests, skip, test_suite, ctx$name)
}

get_iris <- function(ctx) {
  datasets_iris <- datasets::iris
  if (isTRUE(ctx$tweaks$strict_identifier)) {
    names(datasets_iris) <- gsub(".", "_", names(datasets_iris), fixed = TRUE)
  }
  datasets_iris
}
