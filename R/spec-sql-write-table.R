#' spec_sql_write_table
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_write_table <- list(
  write_table_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbWriteTable)), c("conn", "name", "value", "..."))
  },

  #' @return
  #' `dbWriteTable()` returns `TRUE`, invisibly.
  write_table_return = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_invisible_true(dbWriteTable(con, "test", data.frame(a = 1L)))
      })
    })
  },

  #' If the table exists (and both `append` and `overwrite` arguments are unset),
  #' an error is raised; the remote table remains unchanged.
  write_table_overwrite = function(ctx) {
    with_connection({
      with_remove_test_table({
        test_in <- data.frame(a = 1L)
        dbWriteTable(con, "test", test_in)
        expect_error(dbWriteTable(con, "test", data.frame(a = 2L)))

        test_out <- dbReadTable(con, "test")
        expect_equal_df(test_out, test_in)
      })
    })
  },

  #'
  #' An error is also raised
  write_table_error = function(ctx) {
    with_connection({
      test_in <- data.frame(a = 1L)
      with_remove_test_table({
        #' if `name` cannot be processed with [dbQuoteIdentifier()]
        expect_error(dbWriteTable(con, NA, test_in))
        #' or if this results in a non-scalar.
        expect_error(dbWriteTable(con, c("test", "test"), test_in))

        #' Invalid values for the additional arguments `row.names`,
        #' `overwrite`, `append`, `field.types`, and `temporary`
        #' (non-scalars,
        expect_error(dbWriteTable(con, "test", test_in, row.names = letters))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = c(TRUE, FALSE)))
        expect_error(dbWriteTable(con, "test", test_in, append = c(TRUE, FALSE)))
        expect_error(dbWriteTable(con, "test", test_in, temporary = c(TRUE, FALSE)))
        #' unsupported data types,
        expect_error(dbWriteTable(con, "test", test_in, row.names = 1L))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = 1L))
        expect_error(dbWriteTable(con, "test", test_in, append = 1L))
        expect_error(dbWriteTable(con, "test", test_in, field.types = 1L))
        expect_error(dbWriteTable(con, "test", test_in, temporary = 1L))
        #' `NA`,
        expect_error(dbWriteTable(con, "test", test_in, row.names = NA))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = NA))
        expect_error(dbWriteTable(con, "test", test_in, append = NA))
        expect_error(dbWriteTable(con, "test", test_in, field.types = NA))
        expect_error(dbWriteTable(con, "test", test_in, temporary = NA))
        #' incompatible values)
        expect_error(dbWriteTable(con, "test", test_in, field.types = letters))
        expect_error(dbWriteTable(con, "test", test_in, field.types = c(b = "INTEGER")))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = TRUE, append = TRUE))
      })

      with_remove_test_table({
        dbWriteTable(con, "test", test_in)
        expect_error(dbWriteTable(con, "test", data.frame(b = 2L, c = 3L), append = TRUE))
      })
      #' also raise an error.
    })
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbWriteTable()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `row.names` (default: `NA`)
  #' - `overwrite` (default: `FALSE`)
  #' - `append` (default: `FALSE`)
  #' - `field.types` (default: `NULL`)
  #' - `temporary` (default: `FALSE`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  #' @section Specification:
  #' The `name` argument is processed as follows,
  write_table_name = function(ctx) {
    with_connection({
      #' to support databases that allow non-syntactic names for their objects:
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- "a"
      } else {
        table_names <- c("a", "with spaces", "with,comma")
      }

      for (table_name in table_names) {
        test_in <- data.frame(a = 1)
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If an unquoted table name as string: `dbWriteTable()` will do the quoting,
          dbWriteTable(con, table_name, test_in)
          test_out <- dbReadTable(con, dbQuoteIdentifier(con, table_name))
          expect_equal_df(test_out, test_in)
          #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
        })

        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
          dbWriteTable(con, dbQuoteIdentifier(con, table_name), test_in)
          test_out <- dbReadTable(con, table_name)
          expect_equal_df(test_out, test_in)
        })
      }
    })
  },

  #'
  #' If the `overwrite` argument is `TRUE`, an existing table of the same name
  #' will be overwritten.
  overwrite_table = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE),
                     NA)
        iris_out <- dbReadTable(con, "iris")
        expect_equal_df(iris_out, iris[1:10, ])
      })
    })
  },

  #' This argument doesn't change behavior if the table does not exist yet.
  overwrite_table = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE),
                     NA)
        iris_out <- dbReadTable(con, "iris")
        expect_equal_df(iris_out, iris_in[1:10, ])
      })
    })
  },

  #'
  #' If the `append` argument is `TRUE`, the rows in an existing table are
  #' preserved, and the new data are appended.
  append_table = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], append = TRUE), NA)
        iris_out <- dbReadTable(con, "iris")
        expect_equal_df(iris_out, rbind(iris, iris[1:10,]))
      })
    })
  },

  #' If the table doesn't exist yet, it is created.
  append_table_new = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], append = TRUE), NA)
        iris_out <- dbReadTable(con, "iris")
        expect_equal_df(iris_out, iris[1:10,])
      })
    })
  },

  #'
  #' If the `temporary` argument is `TRUE`, the table is not available in a
  #' second connection and is gone after reconnecting.
  temporary_table = function(ctx) {
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)[1:30, ]
        dbWriteTable(con, "iris", iris, temporary = TRUE)
        iris_out <- dbReadTable(con, "iris")
        expect_equal_df(iris_out, iris)

        with_connection(
          expect_error(dbReadTable(con2, "iris")),
          con = "con2")
      })
    })

    with_connection({
      expect_error(dbReadTable(con, "iris"))
    })
  },

  #' A regular, non-temporary table is visible in a second connection
  table_visible_in_other_connection = function(ctx) {
    iris <- get_iris(ctx)[1:30,]

    with_connection({
      dbWriteTable(con, "iris", iris)
      iris_out <- dbReadTable(con, "iris")
      expect_equal_df(iris_out, iris)

      with_connection(
        expect_equal_df(dbReadTable(con2, "iris"), iris),
        con = "con2")
    })

    #' and after reconnecting to the database.
    with_connection({
      with_remove_test_table(name = "iris", {
        expect_equal_df(dbReadTable(con, "iris"), iris)
      })
    })
  },

  #'
  #' SQL keywords can be used freely in table names, column names, and data.
  roundtrip_keywords = function(ctx) {
    with_connection({
      with_remove_test_table(name = dbQuoteIdentifier(con, "EXISTS"), {
        tbl_in <- data.frame(SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
                             stringsAsFactors = FALSE)

        dbWriteTable(con, "EXISTS", tbl_in)

        tbl_out <- dbReadTable(con, "EXISTS")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' Quotes, commas, and spaces can also be used in the data,
  #' and, if the database supports non-syntactic identifiers,
  #' also for table names and column names.
  roundtrip_quotes = function(ctx) {
    with_connection({
      if (!isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- c(
          as.character(dbQuoteIdentifier(con, "")),
          as.character(dbQuoteString(con, "")),
          "with space",
          ",")
      } else {
        table_names <- "a"
      }

      for (table_name in table_names) {
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
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

          dbWriteTable(con, table_name, tbl_in)

          tbl_out <- dbReadTable(con, table_name, check.names = FALSE)
          expect_equal_df(tbl_out, tbl_in)
        })
      }
    })
  },

  #'
  #' The following data types must be supported at least,
  #' and be read identically with [dbReadTable()]:
  #' - integer
  roundtrip_integer = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(1:5, NA))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - numeric
  roundtrip_numeric = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #'   (also with `Inf` and `NaN` values,
  roundtrip_numeric_special = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA, -Inf, Inf, NaN))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")

        #' the latter are translated to `NA`)
        tbl_in$a[is.nan(tbl_in$a)] <- NA_real_
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - logical
  roundtrip_logical = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(TRUE, FALSE, NA))

      with_remove_test_table({
        tbl_in <- data.frame(a = c(1:5, NA))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        tbl_out$a <- ctx$tweaks$logical_return(tbl_out$a)
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - `NA` as NULL
  roundtrip_null = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = NA)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_true(is.na(tbl_out$a))
      })
    })
  },

  #' - 64-bit values (using `"bigint"` as field type, after conversion to
  #'   numeric)
  roundtrip_64_bit = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(-1e14, 1e15, NA))
        dbWriteTable(con, "test", tbl_in, field.types = c(a = "bigint"))

        tbl_out <- dbReadTable(con, "test")
        tbl_out$a <- as.numeric(tbl_out$a)
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - character (in both UTF-8
  roundtrip_character = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(texts, NA),
                             stringsAsFactors = FALSE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #'   and native encodings)
  roundtrip_character_native = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = c(enc2native(texts), NA),
                             stringsAsFactors = FALSE)
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - factor (returned as character)
  roundtrip_factor = function(ctx) {
    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(a = factor(c(texts, NA)))
        dbWriteTable(con, "test", tbl_in)

        tbl_exp <- tbl_in
        tbl_exp$a <- as.character(tbl_exp$a)
        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_exp)
      })
    })
  },

  #' - list of raw
  roundtrip_raw = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(id = 1:2, a = I(list(as.raw(1:10), NULL)))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        tbl_in$a <- blob::as.blob(unclass(tbl_in$a))
        tbl_out$a <- blob::as.blob(tbl_out$a)
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - objects of type [blob]
  roundtrip_blob = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      with_remove_test_table({
        tbl_in <- data.frame(id = 1:2, a = blob::blob(as.raw(1:10), NULL))
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        tbl_out$a <- blob::as.blob(tbl_out$a)
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #' - date
  roundtrip_date = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5, NA)))

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
        #'   returned as `Date`)
        expect_is(unclass(tbl_out$a), "numeric")
      })
    })
  },

  #' - time
  roundtrip_time = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }

    now <- Sys.time()
    tbl_in <- data.frame(a = c(now + 1:5, NA) - now)

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")

        #'   returned as objects that inherit from `difftime`)
        expect_is(tbl_out$a, "difftime")
        expect_identical(hms::as.hms(tbl_out$a), hms::as.hms(tbl_in$a))
      })
    })
  },

  #' - timestamp
  roundtrip_timestamp = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    with_connection({
      tbl_in <- data.frame(id = 1:5)
      tbl_in$a <- round(Sys.time()) + c(1, 60, 3600, 86400, NA)
      #'   returned as `POSIXct`
      #'   with time zone support)
      tbl_in$b <- as.POSIXct(tbl_in$a, tz = "GMT")
      tbl_in$c <- as.POSIXct(tbl_in$a, tz = "PST8PDT")
      tbl_in$d <- as.POSIXct(tbl_in$a, tz = "UTC")

      with_remove_test_table({
        dbWriteTable(con, "test", tbl_in)

        tbl_out <- dbReadTable(con, "test")
        expect_equal_df(tbl_out, tbl_in)
      })
    })
  },

  #'
  #' The interpretation of [rownames] depends on the `row.names` argument,
  #' see [sqlRownamesToColumn()] for details:
  write_table_row_names_false = function(ctx) {
    #' - If `FALSE`, row names are ignored.
    row.names <- FALSE

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
        mtcars_out <- dbReadTable(con, "mtcars", row.names = FALSE)

        expect_false("row_names" %in% names(mtcars_out))
        expect_equal_df(mtcars_out, unrowname(mtcars_in))
      })
    })
  },

  write_table_row_names_true_exists = function(ctx) {
    #' - If `TRUE`, row names are converted to a column named "row_names",
    row.names <- TRUE

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
        mtcars_out <- dbReadTable(con, "mtcars", row.names = FALSE)

        expect_true("row_names" %in% names(mtcars_out))
        expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
        expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
        expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
      })
    })
  },

  write_table_row_names_true_missing = function(ctx) {
    #'   even if the input data frame only has natural row names from 1 to `nrow(...)`.
    row.names <- TRUE

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = row.names)
        iris_out <- dbReadTable(con, "iris", row.names = FALSE)

        expect_true("row_names" %in% names(iris_out))
        expect_true(all(rownames(iris_in) %in% iris_out$row_names))
        expect_true(all(iris_out$row_names %in% rownames(iris_in)))
        expect_equal_df(iris_out[names(iris_out) != "row_names"], iris_in)
      })
    })
  },

  write_table_row_names_na_exists = function(ctx) {
    #' - If `NA`, a column named "row_names" is created if the data has custom row names,
    row.names <- NA

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
        mtcars_out <- dbReadTable(con, "mtcars", row.names = FALSE)

        expect_true("row_names" %in% names(mtcars_out))
        expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
        expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
        expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
      })
    })
  },

  write_table_row_names_na_missing = function(ctx) {
    #'   no extra column is created in the case of natural row names.
    row.names <- NA

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = row.names)
        iris_out <- dbReadTable(con, "iris", row.names = FALSE)

        expect_equal_df(iris_out, iris_in)
      })
    })
  },

  write_table_row_names_string_exists = function(ctx) {
    row.names <- "make_model"
    #' - If a string, this specifies the name of the column in the remote table
    #'   that contains the row names,

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars

        dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
        mtcars_out <- dbReadTable(con, "mtcars", row.names = FALSE)

        expect_true("make_model" %in% names(mtcars_out))
        expect_true(all(mtcars_out$make_model %in% rownames(mtcars_in)))
        expect_true(all(rownames(mtcars_in) %in% mtcars_out$make_model))
        expect_equal_df(mtcars_out[names(mtcars_out) != "make_model"], unrowname(mtcars_in))
      })
    })
  },

  write_table_row_names_string_missing = function(ctx) {
    row.names <- "seq"
    #'   even if the input data frame only has natural row names.

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = row.names)
        iris_out <- dbReadTable(con, "iris", row.names = FALSE)

        expect_true("seq" %in% names(iris_out))
        expect_true(all(iris_out$seq %in% rownames(iris_in)))
        expect_true(all(rownames(iris_in) %in% iris_out$seq))
        expect_equal_df(iris_out[names(iris_out) != "seq"], iris_in)
      })
    })
  },

  NULL
)
