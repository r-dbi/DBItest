#' spec_arrow_append_table_arrow
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_append_table_arrow <- list(
  arrow_append_table_arrow_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbAppendTableArrow)), c("conn", "name", "value", "..."))
  },

  arrow_append_table_arrow_return = function(ctx, con, table_name) {
    skip_if_not_dbitest(ctx, "1.8.0.50")

    #' @return
    #' `dbAppendTableArrow()` returns a
    test_in <- stream_frame(trivial_df())
    dbCreateTableArrow(con, table_name, test_in)
    ret <- dbAppendTableArrow(con, table_name, test_in)

    #' scalar
    expect_equal(length(ret), 1)
    #' numeric.
    expect_true(is.numeric(ret))
  },

  #'
  arrow_append_table_arrow_missing = function(con, table_name) {
    #' @section Failure modes:
    #' If the table does not exist,
    expect_false(dbExistsTable(con, table_name))
    expect_error(dbAppendTableArrow(con, table_name, stream_frame(a = 2L)))
  },

  arrow_append_table_arrow_invalid_value = function(con, table_name) {
    #' or the new data in `values` is not a data frame or has different column names,
    #' an error is raised; the remote table remains unchanged.
    test_in <- trivial_df()
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbAppendTableArrow(con, table_name, test_in %>% stream_frame() %>% unclass()))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_arrow(test_out, test_in[0, , drop = FALSE])
  },

  arrow_append_table_arrow_append_incompatible = function(con, table_name) {
    test_in <- trivial_df()
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbAppendTableArrow(con, table_name, stream_frame(b = 2L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_arrow(test_out, test_in)
  },

  #'
  arrow_append_table_arrow_closed_connection = function(ctx, closed_con) {
    #' An error is raised when calling this method for a closed
    expect_error(dbAppendTableArrow(closed_con, "test", stream_frame(a = 1)))
  },

  arrow_append_table_arrow_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbAppendTableArrow(invalid_con, "test", stream_frame(a = 1)))
  },

  arrow_append_table_arrow_error = function(con, table_name) {
    #' An error is also raised
    test_in <- stream_frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()] or
    expect_error(dbAppendTableArrow(con, NA, test_in))
    #' if this results in a non-scalar.
    expect_error(dbAppendTableArrow(con, c("test", "test"), test_in))
  },

  #'
  arrow_append_table_arrow_roundtrip_keywords = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.49")

    #' @section Specification:
    #' SQL keywords can be used freely in table names, column names, and data.
    tbl_in <- data.frame(
      select = "unique", from = "join", where = "order",
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in, name = "exists")
  },

  arrow_append_table_arrow_roundtrip_quotes = function(ctx, con, table_name) {
    skip_if_not_dbitest(ctx, "1.8.0.48")

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
    test_arrow_roundtrip(con, tbl_in, use_append = TRUE)
  },

  arrow_append_table_arrow_roundtrip_quotes_table_names = function(ctx, con) {
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

    tbl_in <- data.frame(trivial_df())

    for (table_name in table_names) {
      test_arrow_roundtrip_one(con, tbl_in, use_append = TRUE, .add_na = FALSE)
    }
  },

  arrow_append_table_arrow_roundtrip_quotes_column_names = function(ctx, con) {
    #' and column names.
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

    tbl_in <- data.frame(trivial_df(length(column_names), column_names))

    test_arrow_roundtrip_one(con, tbl_in, use_append = TRUE, .add_na = FALSE)
  },

  #'
  arrow_append_table_arrow_roundtrip_integer = function(con) {
    #' The following data types must be supported at least,
    #' and be read identically with [dbReadTable()]:
    #' - integer
    tbl_in <- data.frame(a = c(1:5))
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
  },

  arrow_append_table_arrow_roundtrip_numeric = function(con) {
    #' - numeric
    tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
    #'   (the behavior for `Inf` and `NaN` is not specified)
  },

  arrow_append_table_arrow_roundtrip_logical = function(ctx, con) {
    #' - logical
    tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
    tbl_exp <- tbl_in
    tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in, tbl_exp)
  },

  arrow_append_table_arrow_roundtrip_null = function(con) {
    #' - `NA` as NULL
    tbl_in <- data.frame(a = NA)
    test_arrow_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
        tbl_out
      }
    )
  },

  #' - 64-bit values (using `"bigint"` as field type); the result can be
  arrow_append_table_arrow_roundtrip_64_bit_numeric = function(ctx, con) {
    skip("Internal: Need to enhance test_arrow_roundtrip()")

    tbl_in <- data.frame(a = c(-1e14, 1e15))
    test_arrow_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        #'     - converted to a numeric, which may lose precision,
        tbl_out$a <- as.numeric(tbl_out$a)
        tbl_out
      }
    )
  },
  #
  arrow_append_table_arrow_roundtrip_64_bit_character = function(ctx, con) {
    skip("Internal: Need to enhance test_arrow_roundtrip()")

    tbl_in <- data.frame(a = c(-1e14, 1e15))
    tbl_exp <- tbl_in
    tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
    test_arrow_roundtrip(
      use_append = TRUE,
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
  arrow_append_table_arrow_roundtrip_64_bit_roundtrip = function(ctx, con, table_name) {
    skip("Internal: Need to enhance test_arrow_roundtrip()")

    tbl_in <- data.frame(a = c(-1e14, 1e15))
    dbWriteTable(con, table_name, tbl_in, field.types = c(a = "BIGINT"))
    tbl_out <- dbReadTable(con, table_name)
    #'     - written to another table and read again unchanged
    test_arrow_roundtrip(use_append = TRUE, con, tbl_out, tbl_expected = tbl_out)
  },

  arrow_append_table_arrow_roundtrip_character = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.45")

    #' - character (in both UTF-8
    tbl_in <- data.frame(
      id = seq_along(get_texts()),
      a = get_texts(),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
  },

  arrow_append_table_arrow_roundtrip_character_native = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.44")

    #'   and native encodings),
    tbl_in <- data.frame(
      a = c(enc2native(get_texts())),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
  },

  arrow_append_table_arrow_roundtrip_character_empty = function(con) {
    #'   supporting empty strings
    tbl_in <- data.frame(
      a = c("", "a"),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
  },

  arrow_append_table_arrow_roundtrip_character_empty_after = function(con) {
    #'   (before and after non-empty strings)
    tbl_in <- data.frame(
      a = c("a", ""),
      stringsAsFactors = FALSE
    )
    test_arrow_roundtrip(use_append = TRUE, con, tbl_in)
  },

  arrow_append_table_arrow_roundtrip_factor = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.43")

    #' - factor (possibly returned as character)
    tbl_in <- data.frame(
      a = factor(get_texts())
    )
    tbl_exp <- tbl_in
    tbl_exp$a <- as.character(tbl_exp$a)
    test_arrow_roundtrip(
      use_append = TRUE,
      con,
      tbl_in,
      tbl_exp,
      transform = function(tbl_out) {
        tbl_out$a <- as.character(tbl_out$a)
        tbl_out
      }
    )
  },

  arrow_append_table_arrow_roundtrip_blob = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.41")

    #' - objects of type [blob::blob]
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(0:10)))
    test_arrow_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        tbl_out$a <- blob::as_blob(tbl_out$a)
        tbl_out
      }
    )
  },

  arrow_append_table_arrow_roundtrip_date = function(ctx, con) {
    #' - date
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    #'   returned as `Date`)
    tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
    test_arrow_roundtrip(
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  arrow_append_table_arrow_roundtrip_date_extended = function(ctx, con) {
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
      use_append = TRUE,
      con, tbl_in,
      transform = function(tbl_out) {
        expect_type(unclass(tbl_out$a), "double")
        tbl_out
      }
    )
  },

  arrow_append_table_arrow_roundtrip_time = function(ctx, con) {
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

  arrow_append_table_arrow_roundtrip_timestamp = function(ctx, con) {
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
      use_append = TRUE,
      con, tbl_in,
      transform = function(out) {
        dates <- map_lgl(out, inherits, "POSIXt")
        tz <- toupper(names(out))
        tz[tz == "LOCAL"] <- ""
        out[dates] <- Map(lubridate::with_tz, out[dates], tz[dates])
        out
      }
    )
  },

  arrow_append_table_arrow_roundtrip_timestamp_extended = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.40")

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
      use_append = TRUE,
      con, tbl_in,
      transform = function(out) {
        dates <- map_lgl(out, inherits, "POSIXt")
        tz <- toupper(names(out))
        tz[tz == "LOCAL"] <- ""
        out[dates] <- Map(lubridate::with_tz, out[dates], tz[dates])
        out
      }
    )
  },

  #'
  arrow_append_table_arrow_roundtrip_mixed = function(con) {
    #' Mixing column types in the same table is supported.
    data <- list("a", 1L, 1.5)
    data <- map(data, c, NA)
    expanded <- expand.grid(a = data, b = data, c = data)
    tbl_in_list <- map(
      seq_len(nrow(expanded)),
      function(i) {
        data.frame(map(expanded[i, ], unlist, recursive = FALSE))
      }
    )

    map(tbl_in_list, test_arrow_roundtrip, con = con)
  },

  arrow_append_table_arrow_name = function(ctx, con) {
    #' @section Specification:
    #' The `name` argument is processed as follows,
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      #' - If an unquoted table name as string: `dbAppendTableArrow()` will do the quoting,
      dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
      dbAppendTableArrow(con, table_name, test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_arrow(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  arrow_append_table_arrow_name_quoted = function(ctx, con) {
    #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
    skip_if_not_dbitest(ctx, "1.7.2")

    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      dbCreateTableArrow(con, dbQuoteIdentifier(con, table_name), test_in %>% stream_frame())
      dbAppendTableArrow(con, dbQuoteIdentifier(con, table_name), test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_arrow(test_out, test_in)
    }
  },

  #'
  arrow_append_table_arrow_value_df = function(con, table_name) {
    #' @section Specification:
    #' The `value` argument must be a data frame
    test_in <- trivial_df()
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame())

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_arrow(test_out, test_in)
  },

  arrow_append_table_arrow_value_subset = function(ctx, con, table_name) {
    #' with a subset of the columns of the existing table.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame(.select = c(2)))

    test_out <- check_df(dbReadTable(con, table_name))

    test_in[c(1, 3)] <- NA_real_
    expect_equal_arrow(test_out, test_in)
  },

  arrow_append_table_arrow_value_shuffle = function(ctx, con, table_name) {
    #' The order of the columns does not matter.
    test_in <- trivial_df(3, letters[1:3])
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame(.select = c(2, 3, 1)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_arrow(test_out, test_in)
  },

  #
  arrow_append_table_arrow_value_shuffle_subset = function(ctx, con, table_name) {
    test_in <- trivial_df(4, letters[1:4])
    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame(.select = c(4, 1, 3)))

    test_out <- check_df(dbReadTable(con, table_name))
    test_in[2] <- NA_real_
    expect_equal_arrow(test_out, test_in)
  },

  #
  NULL
)
