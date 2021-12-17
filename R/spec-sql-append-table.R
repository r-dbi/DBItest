#' spec_sql_append_table
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_append_table <- list(
  append_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbAppendTable)), c("conn", "name", "value", "...", "row.names"))
  },

  #' @return
  #' `dbAppendTable()` returns a
  append_table_return = function(con, table_name) {
    test_in <- trivial_df()
    dbCreateTable(con, table_name, test_in)
    ret <- dbAppendTable(con, table_name, test_in)

    #' scalar
    expect_equal(length(ret), 1)
    #' numeric.
    expect_true(is.numeric(ret))
  },

  #'
  #' @section Failure modes:
  #' If the table does not exist,
  append_table_missing = function(con, table_name) {
    expect_false(dbExistsTable(con, table_name))

    test_in <- trivial_df()
    expect_error(dbAppendTable(con, table_name, data.frame(a = 2L)))
  },

  #' or the new data in `values` is not a data frame or has different column names,
  #' an error is raised; the remote table remains unchanged.
  append_table_invalid_value = function(con, table_name) {
    test_in <- trivial_df()
    dbCreateTable(con, table_name, test_in)
    expect_error(dbAppendTable(con, table_name, unclass(test_in)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in[0, , drop = FALSE])
  },
  append_table_append_incompatible = function(con, table_name) {
    test_in <- trivial_df()
    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in)
    expect_error(dbAppendTable(con, table_name, data.frame(b = 2L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #'
  #' An error is raised when calling this method for a closed
  append_table_closed_connection = function(ctx, closed_con) {
    expect_error(dbAppendTable(closed_con, "test", data.frame(a = 1)))
  },

  #' or invalid connection.
  append_table_invalid_connection = function(ctx, invalid_con) {
    expect_error(dbAppendTable(invalid_con, "test", data.frame(a = 1)))
  },

  #' An error is also raised
  append_table_error = function(con, table_name) {
    test_in <- data.frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbAppendTable(con, NA, test_in))
    #' or if this results in a non-scalar.
    expect_error(dbAppendTable(con, c("test", "test"), test_in))

    #' Invalid values for the `row.names` argument
    #' (non-scalars,
    expect_error(dbAppendTable(con, "test", test_in, row.names = letters))
    #' unsupported data types,
    expect_error(dbAppendTable(con, "test", test_in, row.names = list(1L)))
    #' `NA`)
    expect_error(dbAppendTable(con, "test", test_in, row.names = NA))

    #' also raise an error.
  },

  #'
  #' @section Specification:
  #' SQL keywords can be used freely in table names, column names, and data.
  append_roundtrip_keywords = function(con) {
    tbl_in <- data.frame(
      select = "unique", from = "join", where = "order",
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(use_append = TRUE, con, tbl_in, name = "exists")
  },

  #' Quotes, commas, spaces, and other special characters such as newlines and tabs,
  #' can also be used in the data,
  append_roundtrip_quotes = function(ctx, con, table_name) {
    tbl_in <- data.frame(
      as.character(dbQuoteString(con, "")),
      as.character(dbQuoteIdentifier(con, "")),
      "with space",
      "a,b", "a\nb", "a\tb", "a\rb", "a\bb",
      "a\\Nb", "a\\tb", "a\\rb", "a\\bb", "a\\Zb",
      stringsAsFactors = FALSE
    )

    names(tbl_in) <- letters[seq_along(tbl_in)]
    test_table_roundtrip(con, tbl_in, use_append = TRUE)
  },

  #' and, if the database supports non-syntactic identifiers,
  #' also for table names
  append_roundtrip_quotes_table_names = function(ctx, con) {
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
      test_table_roundtrip_one(con, tbl_in, use_append = TRUE, .add_na = FALSE)
    }
  },

  #' and column names.
  append_roundtrip_quotes_column_names = function(ctx, con) {
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

    test_table_roundtrip_one(con, tbl_in, use_append = TRUE, .add_na = FALSE)
  },

  #'
  #' The following data types must be supported at least,
  #' and be read identically with [dbReadTable()]:
  #' - integer
  append_roundtrip_integer = function(con) {
    tbl_in <- data.frame(a = c(1:5))
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
  },

  #' - numeric
  append_roundtrip_numeric = function(con) {
    tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
    #'   (the behavior for `Inf` and `NaN` is not specified)
  },

  #' - logical
  append_roundtrip_logical = function(ctx, con) {
    tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
    tbl_exp <- tbl_in
    tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
    test_table_roundtrip(use_append = TRUE, con, tbl_in, tbl_exp)
  },

  #' - `NA` as NULL
  append_roundtrip_null = function(con) {
    tbl_in <- data.frame(a = NA)
    test_table_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
        tbl_out
      }
    )
  },

  #' - 64-bit values (using `"bigint"` as field type); the result can be
  append_roundtrip_64_bit_numeric = function(ctx, con) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    test_table_roundtrip(
      use_append = TRUE,
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
  append_roundtrip_64_bit_character = function(ctx, con) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    tbl_exp <- tbl_in
    tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
    test_table_roundtrip(
      use_append = TRUE,
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
  append_roundtrip_64_bit_roundtrip = function(con, table_name) {
    tbl_in <- data.frame(a = c(-1e14, 1e15))
    dbWriteTable(con, table_name, tbl_in, field.types = c(a = "BIGINT"))
    tbl_out <- dbReadTable(con, table_name)
    #'     - written to another table and read again unchanged
    test_table_roundtrip(use_append = TRUE, con, tbl_out, tbl_expected = tbl_out)
  },

  #' - character (in both UTF-8
  append_roundtrip_character = function(con) {
    tbl_in <- data.frame(
      id = seq_along(texts),
      a = c(texts),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
  },

  #'   and native encodings),
  append_roundtrip_character_native = function(con) {
    tbl_in <- data.frame(
      a = c(enc2native(texts)),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
  },

  #'   supporting empty strings
  append_roundtrip_character_empty = function(con) {
    tbl_in <- data.frame(
      a = c("", "a"),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
  },

  #'   (before and after non-empty strings)
  append_roundtrip_character_empty_after = function(con) {
    tbl_in <- data.frame(
      a = c("a", ""),
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(use_append = TRUE, con, tbl_in)
  },

  #' - factor (returned as character,
  append_roundtrip_factor = function(con) {
    tbl_in <- data.frame(
      a = factor(c(texts))
    )
    tbl_exp <- tbl_in
    tbl_exp$a <- as.character(tbl_exp$a)
    #'     with a warning)
    suppressWarnings(
      expect_warning(
        test_table_roundtrip(use_append = TRUE, con, tbl_in, tbl_exp)
      )
    )
  },

  #' - list of raw
  append_roundtrip_raw = function(ctx, con) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = I(list(as.raw(0:10))))
    tbl_exp <- tbl_in
    tbl_exp$a <- blob::as_blob(unclass(tbl_in$a))
    test_table_roundtrip(
      use_append = TRUE,
      con, tbl_in, tbl_exp,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  #' - objects of type [blob::blob]
  append_roundtrip_blob = function(ctx, con) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(0:10)))
    test_table_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  #' - date
  append_roundtrip_date = function(ctx, con) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    #'   returned as `Date`)
    tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
    test_table_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        expect_is(unclass(tbl_out$a), "numeric")
        tbl_out
      }
    )
  },

  #'   also for dates prior to 1970 or 1900 or after 2038
  append_roundtrip_date_extended = function(ctx, con) {
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
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  #' - time
  append_roundtrip_time = function(ctx, con) {
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

  #' - timestamp
  append_roundtrip_timestamp = function(ctx, con) {
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
      use_append = TRUE,
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

  #'   also for timestamps prior to 1970 or 1900 or after 2038
  append_roundtrip_timestamp_extended = function(ctx, con) {
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
      use_append = TRUE,
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
  #' Mixing column types in the same table is supported.
  append_roundtrip_mixed = function(con) {
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

  #' @section Specification:
  #' The `name` argument is processed as follows,
  append_table_name = function(ctx, con) {
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      #' - If an unquoted table name as string: `dbAppendTable()` will do the quoting,
      dbCreateTable(con, table_name, test_in)
      dbAppendTable(con, table_name, test_in)
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
  append_table_name_quoted = function(ctx, con) {
    if (as.package_version(ctx$tweaks$dbitest_version) < "1.7.2") {
      skip(paste0("tweak: dbitest_version: ", ctx$tweaks$dbitest_version))
    }

    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      dbCreateTable(con, dbQuoteIdentifier(con, table_name), test_in)
      dbAppendTable(con, dbQuoteIdentifier(con, table_name), test_in)
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_df(test_out, test_in)
    }
  },

  #'
  #'
  #' The `row.names` argument must be `NULL`, the default value.
  append_table_row_names_false = function(con, table_name) {
    mtcars_in <- datasets::mtcars
    dbCreateTable(con, table_name, mtcars_in)
    dbAppendTable(con, table_name, mtcars_in)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_false("row_names" %in% names(mtcars_out))
    expect_equal_df(mtcars_out, unrowname(mtcars_in))
  },

  #' Row names are ignored.
  append_table_row_names_ignore = function(con, table_name) {
    mtcars_in <- datasets::mtcars
    dbCreateTable(con, table_name, mtcars_in)
    dbAppendTable(con, table_name, mtcars_in, row.names = NULL)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_false("row_names" %in% names(mtcars_out))
    expect_equal_df(mtcars_out, unrowname(mtcars_in))
  },
  #
  append_table_row_names_non_null = function(con, table_name) {
    #'
    #' @section Failure modes:
    #' Passing a `value` argument different to `NULL` to the `row.names` argument
    mtcars_in <- datasets::mtcars
    dbCreateTable(con, table_name, mtcars_in)

    #' (in particular `TRUE`,
    expect_error(dbAppendTable(con, table_name, mtcars_in, row.names = TRUE))
    #' `NA`,
    expect_error(dbAppendTable(con, table_name, mtcars_in, row.names = NA))
    #' and a string)
    expect_error(dbAppendTable(con, table_name, mtcars_in, row.names = "make_model"))

    #' raises an error.
  },

  #'
  #' @section Specification:
  #' The `value` argument must be a data frame
  append_table_value_df = function(con, table_name) {
    test_in <- trivial_df()
    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in)

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #' with a subset of the columns of the existing table.
  append_table_value_subset = function(ctx, con, table_name) {
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in[2])

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[c(1, 3)] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  #' The order of the columns does not matter.
  append_table_value_shuffle = function(ctx, con, table_name) {
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in[c(2, 3, 1)])

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #
  append_table_value_shuffle_subset = function(ctx, con, table_name) {
    test_in <- trivial_df(4, letters[1:4])
    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in[c(4, 1, 3)])

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[2] <- NA_real_
    expect_equal_df(test_out, test_in)
  },

  #
  NULL
)
