#' spec_arrow_write_table_arrow
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @importFrom lubridate with_tz
spec_arrow_write_table_arrow <- list(
  arrow_write_table_arrow_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbWriteTableArrow)), c("conn", "name", "value", "..."))
  },

  arrow_write_table_arrow_return = function(con, table_name) {
    #' @return
    #' `dbWriteTableArrow()` returns `TRUE`, invisibly.
    expect_invisible_true(dbWriteTableArrow(con, table_name, stream_frame(a = 1L)))
  },

  #'
  arrow_write_table_arrow_error_overwrite = function(con, table_name) {
    skip("Failed in SQLite")

    #' @section Failure modes:
    #' If the table exists, and both `append` and `overwrite` arguments are unset,
    test_in <- data.frame(a = 1L)
    dbWriteTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbWriteTableArrow(con, table_name, stream_frame(a = 2L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  arrow_write_table_arrow_append_incompatible = function(con, table_name) {
    #' or `append = TRUE` and the data frame with the new data has different
    #' column names,
    #' an error is raised; the remote table remains unchanged.
    test_in <- data.frame(a = 1L)
    dbWriteTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbWriteTableArrow(con, table_name, stream_frame(b = 2L), append = TRUE))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #'
  arrow_write_table_arrow_closed_connection = function(ctx, closed_con) {
    #' An error is raised when calling this method for a closed
    expect_error(dbWriteTableArrow(closed_con, "test", stream_frame(a = 1)))
  },

  arrow_write_table_arrow_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbWriteTableArrow(invalid_con, "test", stream_frame(a = 1)))
  },

  arrow_write_table_arrow_error = function(ctx, con, table_name) {
    skip("Failed in SQLite")

    #' An error is also raised
    test_in <- stream_frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbWriteTableArrow(con, NA, test_in %>% stream_frame()))
    #' or if this results in a non-scalar.
    expect_error(dbWriteTableArrow(con, c(table_name, table_name), test_in %>% stream_frame()))

    #' Invalid values for the additional arguments
    #' `overwrite`, `append`, and `temporary`
    #' (non-scalars,
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), overwrite = c(TRUE, FALSE)))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), append = c(TRUE, FALSE)))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), temporary = c(TRUE, FALSE)))
    #' unsupported data types,
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), overwrite = 1L))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), append = 1L))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), temporary = 1L))
    #' `NA`,
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), overwrite = NA))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), append = NA))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), temporary = NA))
    #' incompatible values,
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), overwrite = TRUE, append = TRUE))
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame(), append = TRUE))
    #' duplicate
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame()))
    #' or missing names,
    expect_error(dbWriteTableArrow(con, table_name, test_in %>% stream_frame()))

    #' incompatible columns)
    dbWriteTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbWriteTableArrow(con, table_name, stream_frame(b = 2L, c = 3L), append = TRUE))

    #' also raise an error.
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbWriteTableArrow()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `overwrite` (default: `FALSE`)
  #' - `append` (default: `FALSE`)
  #' - `temporary` (default: `FALSE`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  arrow_write_table_arrow_name = function(ctx, con) {
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
      #' - If an unquoted table name as string: `dbWriteTableArrow()` will do the quoting,
      dbWriteTableArrow(con, table_name, test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  arrow_write_table_arrow_name_quoted = function(ctx, con) {
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
      dbWriteTableArrow(con, dbQuoteIdentifier(con, table_name), test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_df(test_out, test_in)
    }
  },

  #'
  arrow_write_table_arrow_value_df = function(con, table_name) {
    #' The `value` argument must be a data frame
    test_in <- trivial_df()
    dbWriteTableArrow(con, table_name, test_in %>% stream_frame())

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  arrow_write_table_arrow_value_subset = function(ctx, con, table_name) {
    #' with a subset of the columns of the existing table if `append = TRUE`.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbWriteTableArrow(con, table_name, test_in[2] %>% stream_frame(), append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[c(1, 3)] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  arrow_write_table_arrow_value_shuffle = function(ctx, con, table_name) {
    #' The order of the columns does not matter with `append = TRUE`.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbWriteTableArrow(con, table_name, test_in[c(2, 3, 1)] %>% stream_frame(), append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #
  arrow_write_table_arrow_value_shuffle_subset = function(ctx, con, table_name) {
    test_in <- trivial_df(4, letters[1:4])
    dbCreateTable(con, table_name, test_in)
    dbWriteTableArrow(con, table_name, test_in[c(4, 1, 3)] %>% stream_frame(), append = TRUE)

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[2] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  #'
  arrow_write_table_arrow_overwrite = function(ctx, con, table_name) {
    skip("Requires dbBind() on RMariaDB")

    #' If the `overwrite` argument is `TRUE`, an existing table of the same name
    #' will be overwritten.
    penguins <- get_penguins(ctx)
    dbWriteTableArrow(con, table_name, penguins)
    expect_error(
      dbWriteTableArrow(con, table_name, penguins[1, ] %>% stream_frame(), overwrite = TRUE),
      NA
    )
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins[1, ])
  },

  arrow_write_table_arrow_overwrite_missing = function(ctx, con, table_name) {
    skip("Requires dbBind() on RMariaDB")

    #' This argument doesn't change behavior if the table does not exist yet.
    penguins_in <- get_penguins(ctx)
    expect_error(
      dbWriteTableArrow(con, table_name, penguins_in[1, ] %>% stream_frame(), overwrite = TRUE),
      NA
    )
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins_in[1, ])
  },

  #'
  arrow_write_table_arrow_append = function(ctx, con, table_name) {
    skip("Requires dbBind() on RMariaDB")

    #' If the `append` argument is `TRUE`, the rows in an existing table are
    #' preserved, and the new data are appended.
    penguins <- get_penguins(ctx)
    dbWriteTableArrow(con, table_name, penguins)
    expect_error(dbWriteTableArrow(con, table_name, penguins[1, ] %>% stream_frame(), append = TRUE), NA)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, rbind(penguins, penguins[1, ]))
  },

  arrow_write_table_arrow_append_new = function(ctx, con, table_name) {
    skip("Failed in SQLite")

    #' If the table doesn't exist yet, it is created.
    penguins <- get_penguins(ctx)
    expect_error(dbWriteTableArrow(con, table_name, penguins[1, ] %>% stream_frame(), append = TRUE), NA)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins[1, ])
  },

  #'
  arrow_write_table_arrow_temporary = function(ctx, con, table_name = "dbit08") {
    skip("Failed in SQLite")

    #' If the `temporary` argument is `TRUE`, the table is not available in a
    #' second connection and is gone after reconnecting.
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    penguins <- get_penguins(ctx)
    dbWriteTableArrow(con, table_name, penguins %>% stream_frame(), temporary = TRUE)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins)

    con2 <- local_connection(ctx)
    expect_error(dbReadTable(con2, table_name))
  },
  # second stage
  arrow_write_table_arrow_temporary = function(ctx, con) {
    skip("Failed in SQLite")

    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    table_name <- "dbit08"
    expect_error(dbReadTable(con, table_name))
  },

  arrow_write_table_arrow_visible_in_other_connection = function(ctx, local_con) {
    skip("Failed in SQLite")

    #' A regular, non-temporary table is visible in a second connection,
    penguins30 <- get_penguins(ctx)

    table_name <- "dbit09"

    dbWriteTableArrow(local_con, table_name, penguins30 %>% stream_frame())
    penguins_out <- check_df(dbReadTable(local_con, table_name))
    expect_equal_df(penguins_out, penguins30)

    con2 <- local_connection(ctx)
    expect_equal_df(dbReadTable(con2, table_name), penguins30)
  },
  # second stage
  arrow_write_table_arrow_visible_in_other_connection = function(ctx, con) {
    skip("Failed in SQLite")

    #' in a pre-existing connection,
    penguins30 <- get_penguins(ctx)

    table_name <- "dbit09"

    expect_equal_df(check_df(dbReadTable(con, table_name)), penguins30)
  },
  # third stage
  arrow_write_table_arrow_visible_in_other_connection = function(ctx, local_con, table_name = "dbit09") {
    skip("Failed in SQLite")

    #' and after reconnecting to the database.
    penguins30 <- get_penguins(ctx)

    expect_equal_df(check_df(dbReadTable(local_con, table_name)), penguins30)
  },

  #'
  arrow_write_table_arrow_roundtrip_keywords = function(ctx, con) {
    skip("Requires dbBind() on RMariaDB")

    #' SQL keywords can be used freely in table names, column names, and data.
    tbl_in <- data.frame(
      select = "unique", from = "join", where = "order",
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(con, tbl_in, name = "exists")
  },

  arrow_write_table_arrow_roundtrip_quotes = function(ctx, con, table_name) {
    skip("Requires dbBind() on RMariaDB")

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
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_quotes_table_names = function(ctx, con) {
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
      test_arrow_roundtrip_one(con, tbl_in, .add_na = "none")
    }
  },

  arrow_write_table_arrow_roundtrip_quotes_column_names = function(ctx, con) {
    skip("Failed in SQLite")

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

    test_arrow_roundtrip_one(con, tbl_in, .add_na = "none")
  },

  #'
  arrow_write_table_arrow_roundtrip_integer = function(ctx, con) {
    #' The following data types must be supported at least,
    #' and be read identically with [dbReadTable()]:
    #' - integer
    tbl_in <- data.frame(a = c(1:5))
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_numeric = function(ctx, con) {
    #' - numeric
    tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
    test_arrow_roundtrip(con, tbl_in)
    #'   (the behavior for `Inf` and `NaN` is not specified)
  },

  arrow_write_table_arrow_roundtrip_logical = function(ctx, con) {
    skip("Fails in adbc")

    #' - logical
    tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
    tbl_exp <- tbl_in
    tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
    test_arrow_roundtrip(con, tbl_in, tbl_exp)
  },

  arrow_write_table_arrow_roundtrip_null = function(ctx, con) {
    #' - `NA` as NULL
    tbl_in <- data.frame(a = NA)
    test_arrow_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
        tbl_out
      }
    )
  },

  #' - 64-bit values (using `"bigint"` as field type); the result can be
  arrow_write_table_arrow_roundtrip_64_bit_numeric = function(ctx, con) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    test_arrow_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        #'     - converted to a numeric, which may lose precision,
        tbl_out$a <- as.numeric(tbl_out$a)
        tbl_out
      }
    )
  },
  #
  arrow_write_table_arrow_roundtrip_64_bit_character = function(ctx, con) {
    skip("Failed in SQLite")

    tbl_in <- data.frame(a = c(-1e14, 1e15))
    tbl_exp <- tbl_in
    tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
    test_arrow_roundtrip(
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        #'     - converted a character vector, which gives the full decimal
        #'       representation
        tbl_out$a <- as.character(tbl_out$a)
        tbl_out
      }
    )
  },
  #
  arrow_write_table_arrow_roundtrip_64_bit_roundtrip = function(con, table_name) {
    skip("Failed in SQLite")

    tbl_in <- data.frame(a = c(-1e14, 1e15))
    dbWriteTableArrow(con, table_name, tbl_in, field.types = c(a = "BIGINT"))
    tbl_out <- dbReadTable(con, table_name)
    #'     - written to another table and read again unchanged
    test_arrow_roundtrip(con, tbl_out, tbl_expected = tbl_out)
  },

  arrow_write_table_arrow_roundtrip_character = function(ctx, con) {
    skip("Requires dbBind() on RMariaDB")

    #' - character (in both UTF-8
    tbl_in <- data.frame(
      id = seq_along(get_texts()),
      a = get_texts(),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_character_native = function(ctx, con) {
    skip("Requires dbBind() on RMariaDB")

    #'   and native encodings),
    tbl_in <- data.frame(
      a = c(enc2native(get_texts())),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_character_empty = function(ctx, con) {
    #'   supporting empty strings
    tbl_in <- data.frame(
      a = c("", "a"),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_character_empty_after = function(ctx, con) {
    #'   before and after a non-empty string
    tbl_in <- data.frame(
      a = c("a", ""),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(con, tbl_in)
  },

  arrow_write_table_arrow_roundtrip_factor = function(ctx, con) {
    skip("Failed in SQLite")

    #' - factor (returned as character)
    tbl_in <- data.frame(
      a = factor(get_texts())
    )
    tbl_exp <- tbl_in
    tbl_exp$a <- as.character(tbl_exp$a)
    test_arrow_roundtrip(con, tbl_in, tbl_exp)
  },

  arrow_write_table_arrow_roundtrip_raw = function(ctx, con) {
    skip("Failed in SQLite")

    #' - list of raw
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = I(list(as.raw(0:10))))
    tbl_exp <- tbl_in
    tbl_exp$a <- blob::as_blob(unclass(tbl_in$a))
    test_arrow_roundtrip(
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  arrow_write_table_arrow_roundtrip_blob = function(ctx, con) {
    skip("Failed in SQLite")

    #' - objects of type [blob::blob]
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(0:10)))
    test_arrow_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  arrow_write_table_arrow_roundtrip_date = function(ctx, con) {
    #' - date
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    #'   returned as `Date`),
    tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
    test_arrow_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  arrow_write_table_arrow_roundtrip_date_extended = function(ctx, con) {
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
    test_arrow_roundtrip(
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  arrow_write_table_arrow_roundtrip_time = function(ctx, con) {
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

    test_arrow_roundtrip(
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

  arrow_write_table_arrow_roundtrip_timestamp = function(ctx, con) {
    skip("Fails in adbc")

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
    test_arrow_roundtrip(
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

  arrow_write_table_arrow_roundtrip_timestamp_extended = function(ctx, con) {
    skip("Fails in adbc")

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
    test_arrow_roundtrip(
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
  arrow_write_table_arrow_roundtrip_mixed = function(ctx, con) {
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

    lapply(tbl_in_list, test_arrow_roundtrip, con = con)
  },

  #
  NULL
)

test_arrow_roundtrip <- function(...) {
  test_arrow_roundtrip_one(..., .add_na = "none")
  test_arrow_roundtrip_one(..., .add_na = "above")
  test_arrow_roundtrip_one(..., .add_na = "below")
}

test_arrow_roundtrip_one <- function(con, tbl_in, tbl_expected = tbl_in, transform = identity,
                                     name = NULL, use_append = FALSE, .add_na = "none") {
  # Need data frames here because streams can be collected only once
  stopifnot(is.data.frame(tbl_in))
  stopifnot(is.data.frame(tbl_expected))

  force(tbl_expected)
  if (.add_na == "above") {
    tbl_in <- stream_add_na_above(tbl_in)
    tbl_expected <- stream_add_na_above(tbl_expected)
  } else if (.add_na == "below") {
    tbl_in <- stream_add_na_below(tbl_in)
    tbl_expected <- stream_add_na_below(tbl_expected)
  }

  if (is.null(name)) {
    name <- random_table_name()
  }

  local_remove_test_table(con, name = name)

  if (use_append) {
    dbCreateTableArrow(con, name, tbl_in %>% stream_frame())
    dbAppendTableArrow(con, name, tbl_in %>% stream_frame())
  } else {
    dbWriteTableArrow(con, name, tbl_in %>% stream_frame())
  }

  tbl_read <- check_df(dbReadTable(con, name, check.names = FALSE))
  tbl_out <- transform(tbl_read)
  expect_equal_df(tbl_out, tbl_expected)
}

stream_add_na_above <- function(tbl) {
  idx <- c(NA, seq_len(nrow(tbl)))
  tbl <- tbl[idx, , drop = FALSE]
  unrowname(tbl)
}

stream_add_na_below <- function(tbl) {
  idx <- c(seq_len(nrow(tbl)), NA)
  tbl <- tbl[idx, , drop = FALSE]
  unrowname(tbl)
}
