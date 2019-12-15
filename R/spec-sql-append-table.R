#' spec_sql_append_table
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_append_table <- list(
  append_table_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbAppendTable)), c("conn", "name", "value", "...", "row.names"))
  },

  #' @return
  #' `dbAppendTable()` returns a
  append_table_return = function(ctx) {
    with_connection({
      with_remove_test_table({
        test_in <- trivial_df()
        dbCreateTable(con, "test", test_in)
        ret <- dbAppendTable(con, "test", test_in)

        #' scalar
        expect_equal(length(ret), 1)
        #' numeric.
        expect_true(is.numeric(ret))
      })
    })
  },

  #' If the table does not exist,
  append_table_missing = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_false(dbExistsTable(con, "test"))

        test_in <- trivial_df()
        expect_error(dbAppendTable(con, "test", data.frame(a = 2L)))
      })
    })
  },

  #' or the data frame with the new data has different column names,
  #' an error is raised; the remote table remains unchanged.
  append_table_append_incompatible = function(ctx) {
    with_connection({
      with_remove_test_table({
        test_in <- trivial_df()
        dbCreateTable(con, "test", test_in)
        dbAppendTable(con, "test", test_in)
        expect_error(dbAppendTable(con, "test", data.frame(b = 2L), append = TRUE))

        test_out <- check_df(dbReadTable(con, "test"))
        expect_equal_df(test_out, test_in)
      })
    })
  },

  #'
  #' An error is raised when calling this method for a closed
  append_table_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbAppendTable(con, "test", data.frame(a = 1)))
    })
  },

  #' or invalid connection.
  append_table_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbAppendTable(con, "test", data.frame(a = 1)))
    })
  },

  #' An error is also raised
  append_table_error = function(ctx) {
    with_connection({
      test_in <- data.frame(a = 1L)
      with_remove_test_table({
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
      })

      #' also raise an error.
    })
  },

  #'
  #' SQL keywords can be used freely in table names, column names, and data.
  append_roundtrip_keywords = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(use_append = TRUE, con, tbl_in, name = "EXISTS")
    })
  },

  #' Quotes, commas, and spaces can also be used in the data,
  #' and, if the database supports non-syntactic identifiers,
  #' also for table names and column names.
  append_roundtrip_quotes = function(ctx) {
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

        test_table_roundtrip(use_append = TRUE, con, tbl_in)
      }
    })
  },

  #'
  #' The following data types must be supported at least,
  #' and be read identically with [dbReadTable()]:
  #' - integer
  append_roundtrip_integer = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(1:5))
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })
  },

  #' - numeric
  append_roundtrip_numeric = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5)))
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })
    #'   (the behavior for `Inf` and `NaN` is not specified)
  },

  #' - logical
  append_roundtrip_logical = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(TRUE, FALSE, NA))
      tbl_exp <- tbl_in
      tbl_exp$a <- ctx$tweaks$logical_return(tbl_exp$a)
      test_table_roundtrip(use_append = TRUE, con, tbl_in, tbl_exp)
    })
  },

  #' - `NA` as NULL
  append_roundtrip_null = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = NA)
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in,
        transform = function(tbl_out) {
          tbl_out$a <- as.logical(tbl_out$a) # Plain NA is of type logical
          tbl_out
        }
      )
    })
  },

  #' - 64-bit values (using `"bigint"` as field type); the result can be
  append_roundtrip_64_bit_numeric = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15))
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in,
        transform = function(tbl_out) {
          #'     - converted to a numeric, which may lose precision,
          tbl_out$a <- as.numeric(tbl_out$a)
          tbl_out
        },
        field.types = c(a = "BIGINT")
      )
    })
  },

  append_roundtrip_64_bit_character = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15))
      tbl_exp <- tbl_in
      tbl_exp$a <- format(tbl_exp$a, scientific = FALSE)
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          #'     - converted a character vector, which gives the full decimal
          #'       representation
          tbl_out$a <- as.character(tbl_out$a)
          tbl_out
        },
        field.types = c(a = "BIGINT")
      )
    })
  },

  append_roundtrip_64_bit_roundtrip = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15))
      tbl_out <- with_remove_test_table(
        {
          dbWriteTable(con, "test", tbl_in, field.types = c(a = "BIGINT"))
          dbReadTable(con, "test")
        }
      )
      tbl_exp <- tbl_out
      #'     - written to another table and read again unchanged
      test_table_roundtrip(use_append = TRUE, con, tbl_out, tbl_exp)
    })
  },

  #' - character (in both UTF-8
  append_roundtrip_character = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        id = seq_along(texts),
        a = c(texts),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })
  },

  #'   and native encodings),
  append_roundtrip_character_native = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = c(enc2native(texts)),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })
  },

  #'   supporting empty strings
  append_roundtrip_character_empty = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = c("", "a"),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })

    with_connection({
      tbl_in <- data.frame(
        a = c("a", ""),
        stringsAsFactors = FALSE
      )
      test_table_roundtrip(use_append = TRUE, con, tbl_in)
    })
  },

  #' - factor (returned as character,
  append_roundtrip_factor = function(ctx) {
    with_connection({
      tbl_in <- data.frame(
        a = factor(c(texts))
      )
      tbl_exp <- tbl_in
      tbl_exp$a <- as.character(tbl_exp$a)
      #'     with awarning)
      expect_warning(
        test_table_roundtrip(use_append = TRUE, con, tbl_in, tbl_exp)
      )
    })
  },

  #' - list of raw
  append_roundtrip_raw = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      tbl_in <- data.frame(id = 1L, a = I(list(as.raw(0:10))))
      tbl_exp <- tbl_in
      tbl_exp$a <- blob::as_blob(unclass(tbl_in$a))
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          tbl_out$a <- blob::as_blob(tbl_out$a)
          tbl_out
        }
      )
    })
  },

  #' - objects of type [blob::blob]
  append_roundtrip_blob = function(ctx) {
    #'   (if supported by the database)
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      tbl_in <- data.frame(id = 1L, a = blob::blob(as.raw(0:10)))
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in,
        transform = function(tbl_out) {
          tbl_out$a <- blob::as_blob(tbl_out$a)
          tbl_out
        }
      )
    })
  },

  #' - date
  append_roundtrip_date = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    with_connection({
      #'   returned as `Date`)
      tbl_in <- data.frame(a = as_numeric_date(c(Sys.Date() + 1:5)))
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in,
        transform = function(tbl_out) {
          expect_is(unclass(tbl_out$a), "numeric")
          tbl_out
        }
      )
    })
  },

  #' - time
  append_roundtrip_time = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$time_typed)) {
      skip("tweak: !time_typed")
    }


    with_connection({
      now <- Sys.time()
      tbl_in <- data.frame(a = c(now + 1:5) - now)

      tbl_exp <- tbl_in
      tbl_exp$a <- hms::as_hms(tbl_exp$a)

      test_table_roundtrip(use_append = TRUE,
        con, tbl_in, tbl_exp,
        transform = function(tbl_out) {
          #'   returned as objects that inherit from `difftime`)
          expect_is(tbl_out$a, "difftime")
          tbl_out$a <- hms::as_hms(tbl_out$a)
          tbl_out
        }
    )
    })
  },

  #' - timestamp
  append_roundtrip_timestamp = function(ctx) {
    #'   (if supported by the database;
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    with_connection({
      #'   returned as `POSIXct`
      local <- round(Sys.time()) +
        c(
          1, 60, 3600, 86400,
          86400 * 90, 86400 * 180, 86400 * 270,
          1e9, 5e9
        )
      attr(local, "tzone") <- NULL
      tbl_in <- data.frame(id = seq_along(local))
      tbl_in$local <- local
      tbl_in$GMT <- lubridate::with_tz(local, tzone = "GMT")
      tbl_in$PST8PDT <- lubridate::with_tz(local, tzone = "PST8PDT")
      tbl_in$UTC <- lubridate::with_tz(local, tzone = "UTC")

      #'   respecting the time zone but not necessarily preserving the
      #'   input time zone)
      test_table_roundtrip(use_append = TRUE,
        con, tbl_in,
        transform = function(out) {
          dates <- vapply(out, inherits, "POSIXt", FUN.VALUE = logical(1L))
          zoned <- dates & (names(out) != "local")
          out[zoned] <- Map(lubridate::with_tz, out[zoned], names(out)[zoned])
          out
        }
      )
    })
  },

  #'
  #' Mixing column types in the same table is supported.
  append_roundtrip_mixed = function(ctx) {
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

  #' @section Specification:
  #' The `name` argument is processed as follows,
  append_table_name = function(ctx) {
    with_connection({
      #' to support databases that allow non-syntactic names for their objects:
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- "a"
      } else {
        table_names <- c("a", "with spaces", "with,comma")
      }

      for (table_name in table_names) {
        test_in <- trivial_df()
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If an unquoted table name as string: `dbAppendTable()` will do the quoting,
          dbCreateTable(con, table_name, test_in)
          dbAppendTable(con, table_name, test_in)
          test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
          expect_equal_df(test_out, test_in)
          #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
        })

        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
          dbCreateTable(con, dbQuoteIdentifier(con, table_name), test_in)
          dbAppendTable(con, dbQuoteIdentifier(con, table_name), test_in)
          test_out <- check_df(dbReadTable(con, table_name))
          expect_equal_df(test_out, test_in)
        })
      }
    })
  },

  #'
  #'
  #' The `row.names` argument must be `NULL`, the default value.
  #' Row names are ignored.
  append_table_row_names_false = function(ctx) {
    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbCreateTable(con, "mtcars", mtcars_in)
        dbAppendTable(con, "mtcars", mtcars_in)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

        expect_false("row_names" %in% names(mtcars_out))
        expect_equal_df(mtcars_out, unrowname(mtcars_in))
      })
    })

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbCreateTable(con, "mtcars", mtcars_in)
        dbAppendTable(con, "mtcars", mtcars_in, row.names = NULL)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = FALSE))

        expect_false("row_names" %in% names(mtcars_out))
        expect_equal_df(mtcars_out, unrowname(mtcars_in))
      })
    })
  },

  append_table_row_names_non_null = function(ctx) {
    #' All other values for the `row.names` argument
    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbCreateTable(con, "mtcars", mtcars_in)

        #' (in particular `TRUE`,
        expect_error(dbAppendTable(con, "mtcars", mtcars_in, row.names = TRUE))
        #' `NA`,
        expect_error(dbAppendTable(con, "mtcars", mtcars_in, row.names = NA))
        #' and a string)
        expect_error(dbAppendTable(con, "mtcars", mtcars_in, row.names = "make_model"))
      })

      #' raise an error.
    })
  },

  NULL
)
