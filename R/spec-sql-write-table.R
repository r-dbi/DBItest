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

  #' If the table exists, and both `append` and `overwrite` arguments are unset,
  write_table_overwrite = function(ctx) {
    with_connection({
      with_remove_test_table({
        test_in <- data.frame(a = 1L)
        dbWriteTable(con, "test", test_in)
        expect_error(dbWriteTable(con, "test", data.frame(a = 2L)))

        test_out <- check_df(dbReadTable(con, "test"))
        expect_equal_df(test_out, test_in)
      })
    })
  },

  #' or `append = TRUE` and the data frame with the new data has different
  #' column names,
  #' an error is raised; the remote table remains unchanged.
  write_table_append_incompatible = function(ctx) {
    with_connection({
      with_remove_test_table({
        test_in <- data.frame(a = 1L)
        dbWriteTable(con, "test", test_in)
        expect_error(dbWriteTable(con, "test", data.frame(b = 2L), append = TRUE))

        test_out <- check_df(dbReadTable(con, "test"))
        expect_equal_df(test_out, test_in)
      })
    })
  },

  #'
  #' An error is raised when calling this method for a closed
  write_table_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbWriteTable(con, "test", data.frame(a = 1)))
    })
  },

  #' or invalid connection.
  write_table_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbListTables(con, "test", data.frame(a = 1)))
    })
  },

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
        expect_error(dbWriteTable(con, "test", test_in, row.names = list(1L)))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = 1L))
        expect_error(dbWriteTable(con, "test", test_in, append = 1L))
        expect_error(dbWriteTable(con, "test", test_in, field.types = 1L))
        expect_error(dbWriteTable(con, "test", test_in, temporary = 1L))
        #' `NA`,
        expect_error(dbWriteTable(con, "test", test_in, overwrite = NA))
        expect_error(dbWriteTable(con, "test", test_in, append = NA))
        expect_error(dbWriteTable(con, "test", test_in, field.types = NA))
        expect_error(dbWriteTable(con, "test", test_in, temporary = NA))
        #' incompatible values,
        expect_error(dbWriteTable(con, "test", test_in, field.types = letters))
        expect_error(dbWriteTable(con, "test", test_in, field.types = c(b = "INTEGER")))
        expect_error(dbWriteTable(con, "test", test_in, overwrite = TRUE, append = TRUE))
        expect_error(dbWriteTable(con, "test", test_in, append = TRUE, field.types = c(a = "INTEGER")))
        #' duplicate
        expect_error(dbWriteTable(con, "test", test_in, field.types = c(a = "INTEGER", a = "INTEGER")))
        #' or missing names,
        expect_error(dbWriteTable(con, "test", test_in, field.types = c("INTEGER")))
      })

      with_remove_test_table({
        dbWriteTable(con, "test", test_in)
        #' incompatible columns)
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
          test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
          expect_equal_df(test_out, test_in)
          #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
        })

        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
          dbWriteTable(con, dbQuoteIdentifier(con, table_name), test_in)
          test_out <- check_df(dbReadTable(con, table_name))
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
        iris_out <- check_df(dbReadTable(con, "iris"))
        expect_equal_df(iris_out, iris[1:10, ])
      })
    })
  },

  #' This argument doesn't change behavior if the table does not exist yet.
  overwrite_table_missing = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE),
                     NA)
        iris_out <- check_df(dbReadTable(con, "iris"))
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
        iris_out <- check_df(dbReadTable(con, "iris"))
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
        iris_out <- check_df(dbReadTable(con, "iris"))
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
        iris_out <- check_df(dbReadTable(con, "iris"))
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
      iris_out <- check_df(dbReadTable(con, "iris"))
      expect_equal_df(iris_out, iris)

      with_connection(
        expect_equal_df(dbReadTable(con2, "iris"), iris),
        con = "con2")
    })

    #' and after reconnecting to the database.
    with_connection({
      with_remove_test_table(name = "iris", {
        expect_equal_df(check_df(dbReadTable(con, "iris")), iris)
      })
    })
  },

  #'
  #' SQL keywords can be used freely in table names, column names, and data.
  roundtrip_keywords = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(con, tbl_in, name = "EXISTS")
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
        tbl_in <- data.frame(
          a = as.character(dbQuoteString(con, "")),
          b = as.character(dbQuoteIdentifier(con, "")),
          c = "with space",
          d = ",",
          stringsAsFactors = FALSE
        )

        if (!isTRUE(ctx$tweaks$strict_identifier)) {
          names(tbl_in) <- c(
            as.character(dbQuoteIdentifier(con, "")),
            as.character(dbQuoteString(con, "")),
            "with space",
            ",")
        }

        test_table_roundtrip(con, tbl_in)
      }
    })
  },

  #'
  #' The following data types must be supported at least,
  #' and be read identically with [dbReadTable()]:
  #' - integer
  roundtrip_integer = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(1:5))
      test_table_roundtrip(con, tbl_in)
    })
  },

  #' - numeric
  roundtrip_numeric = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
      test_table_roundtrip(con, tbl_in)
    })
  },

  #'   (also with `Inf` and `NaN` values,
  roundtrip_numeric_special = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), -Inf, Inf, NaN))
      tbl_exp <- tbl_in
      #' the latter are translated to `NA`)
      tbl_exp$a[is.nan(tbl_exp$a)] <- NA_real_
      test_table_roundtrip(con, tbl_in, tbl_exp)
    })
  },

  #' - logical
  roundtrip_logical = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
      tbl_exp <- tbl_in
      tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
      test_table_roundtrip(con, tbl_in, tbl_exp)
    })
  },

  #' - `NA` as NULL
  roundtrip_null = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = NA)
      test_table_roundtrip(
        con, tbl_in,
        transform = function(tbl_out) {
          tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
          tbl_out
        }
      )
    })
  },

  #' - 64-bit values (using `"bigint"` as field type);
  roundtrip_64_bit_numeric = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15))
      test_table_roundtrip(
        con, tbl_in,
        transform = function(tbl_out) {
          #' the result can be converted to a numeric, which may lose precision,
          tbl_out$a <- as.numeric(tbl_out$a)
          tbl_out
        },
        field.types = c(a = "BIGINT")
      )
    })
  },

  roundtrip_64_bit_character = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15))
      tbl_exp <- tbl_in
      tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
      test_table_roundtrip(
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          # ' or to character, which gives the full decimal representation as a
          # ' character vector
          tbl_out$a <- as.character(tbl_out$a)
          tbl_out
        },
        field.types = c(a = "BIGINT")
      )
    })
  },

  #' - character (in both UTF-8
  roundtrip_character = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = c(texts),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(con, tbl_in)
    })
  },

  #'   and native encodings),
  roundtrip_character_native = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = c(enc2native(texts)),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(con, tbl_in)
    })
  },

  #'   supporting empty strings
  roundtrip_character_empty = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = c("", "a"),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(con, tbl_in)
    })

    with_connection({
      tbl_in <- data.frame(
        a = c("a", ""),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(con, tbl_in)
    })
  },

  #' - factor (returned as character)
  roundtrip_factor = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = factor(c(texts))
      )
      tbl_exp <- tbl_in
      tbl_exp$a <- as.character(tbl_exp$a)
      test_table_roundtrip(con, tbl_in, tbl_exp)
    })
  },

  #' - list of raw
  roundtrip_raw = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      tbl_in <- data.frame(id = 1L, a = I(list(as.raw(1:10))))
      tbl_exp <- tbl_in
      tbl_exp$a <- blob::as.blob(unclass(tbl_in$a))
      test_table_roundtrip(
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          tbl_out$a <- blob::as.blob(tbl_out$a)
          tbl_out
        }
      )
    })
  },

  #' - objects of type [blob::blob]
  roundtrip_blob = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(1:10)))
      test_table_roundtrip(
        con, tbl_in,
        transform = function(tbl_out) {
          tbl_out$a <- blob::as.blob(tbl_out$a)
          tbl_out
        }
      )
    })
  },

  #' - date
  roundtrip_date = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    with_connection({
      #'   returned as `Date`)
      tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
      test_table_roundtrip(
        con, tbl_in,
        transform = function(tbl_out) {
          expect_is(unclass(tbl_out$a), "numeric")
          tbl_out
        }
      )
    })
  },

  #' - time
  roundtrip_time = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }


    with_connection({
      now <- Sys.time()
      tbl_in <- data.frame(a = c(now + 1:5) - now)

      tbl_exp <- tbl_in
      tbl_exp$a <- hms::as.hms(tbl_exp$a)

      test_table_roundtrip(
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          #'   returned as objects that inherit from `difftime`)
          expect_is(tbl_out$a, "difftime")
          tbl_out$a <- hms::as.hms(tbl_out$a)
          tbl_out
        }
      )
    })
  },

  #' - timestamp
  roundtrip_timestamp = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    with_connection({
      #'   returned as `POSIXct`
      #'   with time zone support)
      tbl_in <- data.frame(id = 1:5)
      tbl_in$a <- round(Sys.time()) + c(1, 60, 3600, 86400, NA)
      tbl_in$b <- as.POSIXct(tbl_in$a, tz = "GMT")
      tbl_in$c <- as.POSIXct(tbl_in$a, tz = "PST8PDT")
      tbl_in$d <- as.POSIXct(tbl_in$a, tz = "UTC")

      test_table_roundtrip(con, tbl_in)
    })
  },

  #'
  #' Mixing column types in the same table is supported.
  roundtrip_mixed = function(ctx) {
    with_connection({
      data <- list("a", 1L, 1.5)
      data <- lapply(data, c, NA)
      expanded <- expand.grid(a = data, b = data, c = data)
      tbl_in_list <- lapply(
        seq_len(nrow(expanded)),
        function(i) {
          as.data.frame(lapply(expanded[i, ], unlist, recursive = FALSE))
        }
      )

      lapply(tbl_in_list, test_table_roundtrip, con = con)
    })
  },

  #'
  #' The `field.types` argument must be a named character vector with at most
  #' one entry for each column.
  #' It indicates the SQL data type to be used for a new column.
  roundtrip_field_types = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = numeric())
      tbl_exp <- data.frame(a = integer())
      test_table_roundtrip(
        con, tbl_in, tbl_exp,
        field.types = c(a = "INTEGER")
      )
    })
  },

  #'
  #' The interpretation of [rownames] depends on the `row.names` argument,
  #' see [sqlRownamesToColumn()] for details:
  write_table_row_names_false = function(ctx) {
    #' - If `FALSE` or `NULL`, row names are ignored.
    for (row.names in list(FALSE, NULL)) {
      with_connection({
        with_remove_test_table(name = "mtcars", {
          mtcars_in <- datasets::mtcars
          dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
          mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

          expect_false("row_names" %in% names(mtcars_out))
          expect_equal_df(mtcars_out, unrowname(mtcars_in))
        })
      })
    }
  },

  write_table_row_names_true_exists = function(ctx) {
    #' - If `TRUE`, row names are converted to a column named "row_names",
    row.names <- TRUE

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = row.names)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

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
        iris_out <- check_df(dbReadTable(con, "iris", row.names = FALSE))

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
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

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
        iris_out <- check_df(dbReadTable(con, "iris", row.names = FALSE))

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
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

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
        iris_out <- check_df(dbReadTable(con, "iris", row.names = FALSE))

        expect_true("seq" %in% names(iris_out))
        expect_true(all(iris_out$seq %in% rownames(iris_in)))
        expect_true(all(rownames(iris_in) %in% iris_out$seq))
        expect_equal_df(iris_out[names(iris_out) != "seq"], iris_in)
      })
    })
  },

  NULL
)

test_table_roundtrip <- function(...) {
  test_table_roundtrip_one(..., .add_na = "none")
  test_table_roundtrip_one(..., .add_na = "above")
  test_table_roundtrip_one(..., .add_na = "below")
}

test_table_roundtrip_one <- function(con, tbl_in, tbl_expected = tbl_in, transform = identity, name = "test", field.types = NULL, .add_na = "none") {
  force(tbl_expected)
  if (.add_na == "above") {
    tbl_in <- add_na_above(tbl_in)
    tbl_expected <- add_na_above(tbl_expected)
  } else if (.add_na == "below") {
    tbl_in <- add_na_below(tbl_in)
    tbl_expected <- add_na_below(tbl_expected)
  }

  with_remove_test_table(name = dbQuoteIdentifier(con, name), {
    dbWriteTable(con, name, tbl_in, field.types = field.types)

    tbl_out <- check_df(dbReadTable(con, name, check.names = FALSE))
    tbl_out <- transform(tbl_out)
    expect_equal_df(tbl_out, tbl_expected)
  })
}

add_na_above <- function(tbl) {
  tbl <- rbind(tbl, tbl[nrow(tbl) + 1L, , drop = FALSE])
  unrowname(tbl)
}

add_na_below <- function(tbl) {
  tbl <- rbind(tbl[nrow(tbl) + 1L, , drop = FALSE], tbl)
  unrowname(tbl)
}
