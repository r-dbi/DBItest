#' spec_sql_write_table
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @importFrom lubridate with_tz
spec_sql_write_table <- list(
  write_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbWriteTable)), c("conn", "name", "value", "..."))
  },

  write_table_return = function(con, table_name) {
    #' @return
    #' `dbWriteTable()` returns `TRUE`, invisibly.
    expect_invisible_true(dbWriteTable(con, table_name, data.frame(a = 1L)))
  },

  #'
  write_table_overwrite = function(con, table_name) {
    #' @section Failure modes:
    #' If the table exists, and both `append` and `overwrite` arguments are unset,
    test_in <- data.frame(a = 1L)
    dbWriteTable(con, table_name, test_in)
    expect_error(dbWriteTable(con, table_name, data.frame(a = 2L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  write_table_append_incompatible = function(con, table_name) {
    #' or `append = TRUE` and the data frame with the new data has different
    #' column names,
    #' an error is raised; the remote table remains unchanged.
    test_in <- data.frame(a = 1L)
    dbWriteTable(con, table_name, test_in)
    expect_error(dbWriteTable(con, table_name, data.frame(b = 2L), append = TRUE))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #'
  write_table_closed_connection = function(ctx, closed_con) {
    #' An error is raised when calling this method for a closed
    expect_error(dbWriteTable(closed_con, "test", data.frame(a = 1)))
  },

  write_table_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbWriteTable(invalid_con, "test", data.frame(a = 1)))
  },

  write_table_error = function(ctx, con, table_name) {
    #' An error is also raised
    test_in <- data.frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbWriteTable(con, NA, test_in))
    #' or if this results in a non-scalar.
    expect_error(dbWriteTable(con, c(table_name, table_name), test_in))

    #' Invalid values for the additional arguments `row.names`,
    #' `overwrite`, `append`, `field.types`, and `temporary`
    #' (non-scalars,
    expect_error(dbWriteTable(con, table_name, test_in, row.names = letters))
    expect_error(dbWriteTable(con, table_name, test_in, overwrite = c(TRUE, FALSE)))
    expect_error(dbWriteTable(con, table_name, test_in, append = c(TRUE, FALSE)))
    expect_error(dbWriteTable(con, table_name, test_in, temporary = c(TRUE, FALSE)))
    #' unsupported data types,
    expect_error(dbWriteTable(con, table_name, test_in, row.names = list(1L)))
    expect_error(dbWriteTable(con, table_name, test_in, overwrite = 1L))
    expect_error(dbWriteTable(con, table_name, test_in, append = 1L))
    expect_error(dbWriteTable(con, table_name, test_in, field.types = 1L))
    expect_error(dbWriteTable(con, table_name, test_in, temporary = 1L))
    #' `NA`,
    expect_error(dbWriteTable(con, table_name, test_in, overwrite = NA))
    expect_error(dbWriteTable(con, table_name, test_in, append = NA))
    expect_error(dbWriteTable(con, table_name, test_in, field.types = NA))
    expect_error(dbWriteTable(con, table_name, test_in, temporary = NA))
    #' incompatible values,
    expect_error(dbWriteTable(con, table_name, test_in, field.types = letters))
    expect_error(dbWriteTable(con, table_name, test_in, field.types = c(b = "INTEGER")))
    expect_error(dbWriteTable(con, table_name, test_in, overwrite = TRUE, append = TRUE))
    expect_error(dbWriteTable(con, table_name, test_in, append = TRUE, field.types = c(a = "INTEGER")))
    #' duplicate
    expect_error(dbWriteTable(con, table_name, test_in, field.types = c(a = "INTEGER", a = "INTEGER")))
    #' or missing names,
    expect_error(dbWriteTable(con, table_name, test_in, field.types = c("INTEGER")))

    #' incompatible columns)
    dbWriteTable(con, table_name, test_in)
    expect_error(dbWriteTable(con, table_name, data.frame(b = 2L, c = 3L), append = TRUE))

    #' also raise an error.
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbWriteTable()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `row.names` (default: `FALSE`)
  #' - `overwrite` (default: `FALSE`)
  #' - `append` (default: `FALSE`)
  #' - `field.types` (default: `NULL`)
  #' - `temporary` (default: `FALSE`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  write_table_name = function(ctx, con) {
    #' @section Specification:
    #' The `name` argument is processed as follows,
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- data.frame(a = 1)
      local_remove_test_table(con, table_name)
      #' - If an unquoted table name as string: `dbWriteTable()` will do the quoting,
      dbWriteTable(con, table_name, test_in)
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  write_table_name_quoted = function(ctx, con) {
    #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
    skip_if_not_dbitest(ctx, "1.7.2")

    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- data.frame(a = 1)

      local_remove_test_table(con, table_name)
      dbWriteTable(con, dbQuoteIdentifier(con, table_name), test_in)
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_df(test_out, test_in)
    }
  },

  #'
  write_table_value_df = function(con, table_name) {
    #' The `value` argument must be a data frame
    test_in <- trivial_df()
    dbWriteTable(con, table_name, test_in)

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  write_table_value_subset = function(ctx, con, table_name) {
    #' with a subset of the columns of the existing table if `append = TRUE`.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbWriteTable(con, table_name, test_in[2], append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[c(1, 3)] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  write_table_value_shuffle = function(ctx, con, table_name) {
    #' The order of the columns does not matter with `append = TRUE`.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbWriteTable(con, table_name, test_in[c(2, 3, 1)], append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #
  write_table_value_shuffle_subset = function(ctx, con, table_name) {
    test_in <- trivial_df(4, letters[1:4])
    dbCreateTable(con, table_name, test_in)
    dbWriteTable(con, table_name, test_in[c(4, 1, 3)], append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[2] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  #'
  overwrite_table = function(ctx, con, table_name) {
    #' If the `overwrite` argument is `TRUE`, an existing table of the same name
    #' will be overwritten.
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)
    expect_error(
      dbWriteTable(con, table_name, penguins[1, ], overwrite = TRUE),
      NA
    )
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins[1, ])
  },

  overwrite_table_missing = function(ctx, con, table_name) {
    #' This argument doesn't change behavior if the table does not exist yet.
    penguins_in <- get_penguins(ctx)
    expect_error(
      dbWriteTable(con, table_name, penguins_in[1, ], overwrite = TRUE),
      NA
    )
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins_in[1, ])
  },

  #'
  append_table = function(ctx, con, table_name) {
    #' If the `append` argument is `TRUE`, the rows in an existing table are
    #' preserved, and the new data are appended.
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)
    expect_error(dbWriteTable(con, table_name, penguins[1, ], append = TRUE), NA)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, rbind(penguins, penguins[1, ]))
  },

  append_table_new = function(ctx, con, table_name) {
    #' If the table doesn't exist yet, it is created.
    penguins <- get_penguins(ctx)
    expect_error(dbWriteTable(con, table_name, penguins[1, ], append = TRUE), NA)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins[1, ])
  },

  #'
  temporary_table = function(ctx, con, table_name = "dbit08") {
    #' If the `temporary` argument is `TRUE`, the table is not available in a
    #' second connection and is gone after reconnecting.
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins, temporary = TRUE)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins)

    con2 <- local_connection(ctx)
    expect_error(dbReadTable(con2, table_name))
  },
  # second stage
  temporary_table = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    table_name <- "dbit08"
    expect_error(dbReadTable(con, table_name))
  },

  table_visible_in_other_connection = function(ctx, local_con) {
    #' A regular, non-temporary table is visible in a second connection,
    penguins30 <- get_penguins(ctx)

    table_name <- "dbit09"

    dbWriteTable(local_con, table_name, penguins30)
    penguins_out <- check_df(dbReadTable(local_con, table_name))
    expect_equal_df(penguins_out, penguins30)

    con2 <- local_connection(ctx)
    expect_equal_df(dbReadTable(con2, table_name), penguins30)
  },
  # second stage
  table_visible_in_other_connection = function(ctx, con) {
    #' in a pre-existing connection,
    penguins30 <- get_penguins(ctx)

    table_name <- "dbit09"

    expect_equal_df(check_df(dbReadTable(con, table_name)), penguins30)
  },
  # third stage
  table_visible_in_other_connection = function(ctx, local_con, table_name = "dbit09") {
    #' and after reconnecting to the database.
    penguins30 <- get_penguins(ctx)

    expect_equal_df(check_df(dbReadTable(local_con, table_name)), penguins30)
  },

  #'
  roundtrip_keywords = function(ctx, con) {
    #' SQL keywords can be used freely in table names, column names, and data.
    tbl_in <- data.frame(
      select = "unique", from = "join", where = "order",
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in, name = "exists")
  },

  roundtrip_quotes = function(ctx, con, table_name) {
    #' Quotes, commas, spaces, and other special characters such as newlines and tabs,
    #' can also be used in the data,
    tbl_in <- data.frame(
      as.character(dbQuoteString(con, "")),
      as.character(dbQuoteIdentifier(con, "")),
      "with space",
      "a,b", "a\nb", "a\tb", "a\rb", "a\bb",
      "a\\Nb", "a\\tb", "a\\rb", "a\\bb", "a\\Zb",
      stringsAsFactors = FALSE
    )

    names(tbl_in) <- letters[seq_along(tbl_in)]
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_quotes_table_names = function(ctx, con) {
    #' and, if the database supports non-syntactic identifiers,
    #' also for table names
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    table_names <- c(
      as.character(dbQuoteIdentifier(con, "")),
      as.character(dbQuoteString(con, "")),
      "with space",
      "a,b", "a\nb", "a\tb", "a\rb", "a\bb",
      "a\\Nb", "a\\tb", "a\\rb", "a\\bb", "a\\Zb"
    )

    tbl_in <- trivial_df()

    for (table_name in table_names) {
      test_table_roundtrip_one(con, tbl_in, .add_na = "none")
    }
  },

  roundtrip_quotes_column_names = function(ctx, con) {
    #' and column names.
    skip_if_not_dbitest(ctx, "1.7.2")

    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    column_names <- c(
      as.character(dbQuoteIdentifier(con, "")),
      as.character(dbQuoteString(con, "")),
      "with space",
      "a,b", "a\nb", "a\tb", "a\rb", "a\bb",
      "a\\nb", "a\\tb", "a\\rb", "a\\bb", "a\\zb"
    )

    tbl_in <- trivial_df(length(column_names), column_names)

    test_table_roundtrip_one(con, tbl_in, .add_na = "none")
  },

  #'
  roundtrip_integer = function(ctx, con) {
    #' The following data types must be supported at least,
    #' and be read identically with [dbReadTable()]:
    #' - integer
    tbl_in <- data.frame(a = c(1:5))
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_numeric = function(ctx, con) {
    #' - numeric
    tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
    test_table_roundtrip(con, tbl_in)
    #'   (the behavior for `Inf` and `NaN` is not specified)
  },

  roundtrip_logical = function(ctx, con) {
    #' - logical
    tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
    tbl_exp <- tbl_in
    tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
    test_table_roundtrip(con, tbl_in, tbl_exp)
  },

  roundtrip_null = function(ctx, con) {
    #' - `NA` as NULL
    tbl_in <- data.frame(a = NA)
    test_table_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
        tbl_out
      }
    )
  },

  #' - 64-bit values (using `"bigint"` as field type); the result can be
  roundtrip_64_bit_numeric = function(ctx, con) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    test_table_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        #'     - converted to a numeric, which may lose precision,
        tbl_out$a <- as.numeric(tbl_out$a)
        tbl_out
      },
      field.types = c(a = "BIGINT")
    )
  },
  #
  roundtrip_64_bit_character = function(ctx, con) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    tbl_exp <- tbl_in
    tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
    test_table_roundtrip(
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        #'     - converted a character vector, which gives the full decimal
        #'       representation
        tbl_out$a <- as.character(tbl_out$a)
        tbl_out
      },
      field.types = c(a = "BIGINT")
    )
  },
  #
  roundtrip_64_bit_roundtrip = function(con, table_name) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    dbWriteTable(con, table_name, tbl_in, field.types = c(a = "BIGINT"))
    tbl_out <- dbReadTable(con, table_name)
    #'     - written to another table and read again unchanged
    test_table_roundtrip(con, tbl_out, tbl_expected = tbl_out)
  },

  roundtrip_character = function(ctx, con) {
    #' - character (in both UTF-8
    tbl_in <- data.frame(
      id = seq_along(get_texts()),
      a = get_texts(),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_character_native = function(ctx, con) {
    #'   and native encodings),
    tbl_in <- data.frame(
      a = c(enc2native(get_texts())),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_character_empty = function(ctx, con) {
    #'   supporting empty strings
    tbl_in <- data.frame(
      a = c("", "a"),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_character_empty_after = function(ctx, con) {
    #'   before and after a non-empty string
    tbl_in <- data.frame(
      a = c("a", ""),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in)
  },

  roundtrip_factor = function(ctx, con) {
    #' - factor (returned as character)
    tbl_in <- data.frame(
      a = factor(get_texts())
    )
    tbl_exp <- tbl_in
    tbl_exp$a <- as.character(tbl_exp$a)
    test_table_roundtrip(con, tbl_in, tbl_exp)
  },

  roundtrip_raw = function(ctx, con) {
    #' - list of raw
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = I(list(as.raw(0:10))))
    tbl_exp <- tbl_in
    tbl_exp$a <- blob::as_blob(unclass(tbl_in$a))
    test_table_roundtrip(
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  roundtrip_blob = function(ctx, con) {
    #' - objects of type [blob::blob]
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(0:10)))
    test_table_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  roundtrip_date = function(ctx, con) {
    #' - date
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    #'   returned as `Date`),
    tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
    test_table_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  roundtrip_date_extended = function(ctx, con) {
    #'   also for dates prior to 1970 or 1900 or after 2038
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    tbl_in <- data.frame(a = as_numeric_date(c(
      "1811-11-11",
      "1899-12-31",
      "1900-01-01",
      "1950-05-05",
      "1969-12-31",
      "1970-01-01",
      "2037-01-01",
      "2038-01-01",
      "2040-01-01",
      "2999-09-09"
    )))
    test_table_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  roundtrip_time = function(ctx, con) {
    #' - time
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }

    tbl_in <- data.frame(a = hms::hms(minutes = 1:5))
    tbl_in$b <- .difftime(as.numeric(tbl_in$a) / 60, "mins")

    tbl_exp <- tbl_in
    tbl_exp$a <- hms::as_hms(tbl_exp$a)
    tbl_exp$b <- hms::as_hms(tbl_exp$b)

    test_table_roundtrip(
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        #'   returned as objects that inherit from `difftime`)
        expect_s3_class(tbl_out$a, "difftime")
        expect_s3_class(tbl_out$b, "difftime")
        tbl_out$a <- hms::as_hms(tbl_out$a)
        tbl_out$b <- hms::as_hms(tbl_out$b)
        tbl_out
      }
    )
  },

  roundtrip_timestamp = function(ctx, con) {
    #' - timestamp
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    #'   returned as `POSIXct`
    local <- round(Sys.time()) +
      c(
        1, 60, 3600, 86400,
        86400 * 90, 86400 * 180, 86400 * 270,
        1e9, 5e9
      )
    attr(local, "tzone") <- ""
    tbl_in <- data.frame(id = seq_along(local))
    tbl_in$local <- local
    tbl_in$gmt <- lubridate::with_tz(local, tzone = "GMT")
    tbl_in$pst8pdt <- lubridate::with_tz(local, tzone = "PST8PDT")
    tbl_in$utc <- lubridate::with_tz(local, tzone = "UTC")

    #'   respecting the time zone but not necessarily preserving the
    #'   input time zone),
    test_table_roundtrip(
      con, tbl_in,
      transform = function(out) {
        dates <- vapply(out, inherits, "POSIXt", FUN.VALUE = logical(1L))
        tz <- toupper(names(out))
        tz[tz == "LOCAL"] <- ""
        out[dates] <- Map(lubridate::with_tz, out[dates], tz[dates])
        out
      }
    )
  },

  roundtrip_timestamp_extended = function(ctx, con) {
    #'   also for timestamps prior to 1970 or 1900 or after 2038
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    local <- as.POSIXct(c(
      "1811-11-11",
      "1899-12-31",
      "1900-01-01",
      "1950-05-05",
      "1969-12-31",
      "1970-01-01",
      "2037-01-01",
      "2038-01-01",
      "2040-01-01",
      "2999-09-09"
    ))

    attr(local, "tzone") <- ""
    tbl_in <- data.frame(id = seq_along(local))
    tbl_in$local <- local
    tbl_in$gmt <- lubridate::with_tz(local, tzone = "GMT")
    tbl_in$pst8pdt <- lubridate::with_tz(local, tzone = "PST8PDT")
    tbl_in$utc <- lubridate::with_tz(local, tzone = "UTC")

    #'   respecting the time zone but not necessarily preserving the
    #'   input time zone)
    test_table_roundtrip(
      con, tbl_in,
      transform = function(out) {
        dates <- vapply(out, inherits, "POSIXt", FUN.VALUE = logical(1L))
        tz <- toupper(names(out))
        tz[tz == "LOCAL"] <- ""
        out[dates] <- Map(lubridate::with_tz, out[dates], tz[dates])
        out
      }
    )
  },

  #'
  roundtrip_mixed = function(ctx, con) {
    #' Mixing column types in the same table is supported.
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
  },

  #'
  roundtrip_field_types = function(ctx, con) {
    #' The `field.types` argument must be a named character vector with at most
    #' one entry for each column.
    #' It indicates the SQL data type to be used for a new column.
    tbl_in <- data.frame(a = numeric(), b = character())
    #' If a column is missed from `field.types`, the type is inferred
    #' from the input data with [dbDataType()].
    tbl_exp <- data.frame(a = integer(), b = character())
    test_table_roundtrip(
      con, tbl_in, tbl_exp,
      field.types = c(a = "INTEGER")
    )

    tbl_in <- data.frame(a = numeric(), b = integer())
    tbl_exp <- data.frame(a = integer(), b = numeric())
    test_table_roundtrip(
      con, tbl_in, tbl_exp,
      field.types = c(b = "REAL", a = "INTEGER")
    )
  },

  #'
  write_table_row_names_false = function(ctx, con) {
    #' The interpretation of [rownames] depends on the `row.names` argument,
    #' see [sqlRownamesToColumn()] for details:
    #' - If `FALSE` or `NULL`, row names are ignored.
    for (row.names in list(FALSE, NULL)) {
      table_name <- random_table_name()
      local_remove_test_table(con, table_name)
      mtcars_in <- datasets::mtcars
      dbWriteTable(con, table_name, mtcars_in, row.names = row.names)
      mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

      expect_false("row_names" %in% names(mtcars_out))
      expect_equal_df(mtcars_out, unrowname(mtcars_in))
    }
  },
  #
  write_table_row_names_true_exists = function(ctx, con, table_name) {
    #' - If `TRUE`, row names are converted to a column named "row_names",
    row.names <- TRUE

    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in, row.names = row.names)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_true("row_names" %in% names(mtcars_out))
    expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
    expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
    expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
  },
  #
  write_table_row_names_true_missing = function(ctx, con, table_name) {
    #'   even if the input data frame only has natural row names from 1 to `nrow(...)`.
    row.names <- TRUE

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = row.names)
    penguins_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_true("row_names" %in% names(penguins_out))
    expect_true(all(rownames(penguins_in) %in% penguins_out$row_names))
    expect_true(all(penguins_out$row_names %in% rownames(penguins_in)))
    expect_equal_df(penguins_out[names(penguins_out) != "row_names"], penguins_in)
  },
  #
  write_table_row_names_na_exists = function(ctx, con, table_name) {
    #' - If `NA`, a column named "row_names" is created if the data has custom row names,
    row.names <- NA

    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in, row.names = row.names)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_true("row_names" %in% names(mtcars_out))
    expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
    expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
    expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
  },
  #
  write_table_row_names_na_missing = function(ctx, con, table_name) {
    #'   no extra column is created in the case of natural row names.
    row.names <- NA

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = row.names)
    penguins_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_equal_df(penguins_out, penguins_in)
  },
  #
  write_table_row_names_string_exists = function(ctx, con, table_name) {
    row.names <- "make_model"
    #' - If a string, this specifies the name of the column in the remote table
    #'   that contains the row names,

    mtcars_in <- datasets::mtcars

    dbWriteTable(con, table_name, mtcars_in, row.names = row.names)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_true("make_model" %in% names(mtcars_out))
    expect_true(all(mtcars_out$make_model %in% rownames(mtcars_in)))
    expect_true(all(rownames(mtcars_in) %in% mtcars_out$make_model))
    expect_equal_df(mtcars_out[names(mtcars_out) != "make_model"], unrowname(mtcars_in))
  },
  #
  write_table_row_names_string_missing = function(ctx, con, table_name) {
    row.names <- "seq"
    #'   even if the input data frame only has natural row names.

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = row.names)
    penguins_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_true("seq" %in% names(penguins_out))
    expect_true(all(penguins_out$seq %in% rownames(penguins_in)))
    expect_true(all(rownames(penguins_in) %in% penguins_out$seq))
    expect_equal_df(penguins_out[names(penguins_out) != "seq"], penguins_in)
  },
  #
  #'
  write_table_row_names_default = function(ctx, con, table_name) {
    #' The default is `row.names = FALSE`.
    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_false("row_names" %in% names(mtcars_out))
    expect_equal_df(mtcars_out, unrowname(mtcars_in))
  },
  #
  NULL
)

test_table_roundtrip <- function(...) {
  test_table_roundtrip_one(..., .add_na = "none")
  test_table_roundtrip_one(..., .add_na = "above")
  test_table_roundtrip_one(..., .add_na = "below")
}

test_table_roundtrip_one <- function(
    con,
    tbl_in,
    tbl_expected = tbl_in,
    transform = identity,
    name = NULL,
    field.types = NULL,
    use_append = FALSE,
    .add_na = "none") {
  force(tbl_expected)
  if (.add_na == "above") {
    tbl_in <- add_na_above(tbl_in)
    tbl_expected <- add_na_above(tbl_expected)
  } else if (.add_na == "below") {
    tbl_in <- add_na_below(tbl_in)
    tbl_expected <- add_na_below(tbl_expected)
  }

  if (is.null(name)) {
    name <- random_table_name()
  }

  local_remove_test_table(con, name = name)

  if (use_append) {
    dbCreateTable(con, name, field.types %||% tbl_in)
    dbAppendTable(con, name, tbl_in)
  } else {
    dbWriteTable(con, name, tbl_in, field.types = field.types)
  }

  tbl_read <- check_df(dbReadTable(con, name, check.names = FALSE))
  tbl_out <- transform(tbl_read)
  expect_equal_df(tbl_out, tbl_expected)
}

add_na_above <- function(tbl) {
  idx <- c(NA, seq_len(nrow(tbl)))
  tbl <- tbl[idx, , drop = FALSE]
  unrowname(tbl)
}

add_na_below <- function(tbl) {
  idx <- c(seq_len(nrow(tbl)), NA)
  tbl <- tbl[idx, , drop = FALSE]
  unrowname(tbl)
}
