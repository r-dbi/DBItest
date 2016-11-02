#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{Roundtrip tests}{
spec_sql_read_write_roundtrip <- list(
  #' Can create tables with keywords as table and column names.
  roundtrip_keywords = function(ctx) {
    with_connection({
      tbl_in <- data.frame(SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
                           stringsAsFactors = FALSE)

      on.exit(expect_error(dbRemoveTable(con, "EXISTS"), NA), add = TRUE)
      dbWriteTable(con, "EXISTS", tbl_in)

      tbl_out <- dbReadTable(con, "EXISTS")
      expect_identical(tbl_in, tbl_out)
    })
  },

  #' Can create tables with quotes, commas, and spaces in column names and
  #' data.
  roundtrip_quotes = function(ctx) {
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

  #' Can create tables with integer columns.
  roundtrip_integer = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(1:5, NA), id = 1:6)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
    })
  },

  #' Can create tables with numeric columns.
  roundtrip_numeric = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA), id = 1:6)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
    })
  },

  #' Can create tables with numeric columns that contain special values such
  #' as `Inf` and `NaN`.
  roundtrip_numeric_special = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(seq(1, 3, by = 0.5), NA, -Inf, Inf, NaN),
                           id = 1:9)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_equal(tbl_in$a, tbl_out$a[order(tbl_out$id)])
    })
  },

  #' Can create tables with logical columns.
  roundtrip_logical = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(TRUE, FALSE, NA), id = 1:3)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])
    })
  },

  #' Can create tables with logical columns, returned as integer.
  roundtrip_logical_int = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(TRUE, FALSE, NA), id = 1:3)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(as.integer(tbl_in$a), tbl_out$a[order(tbl_out$id)])
    })
  },

  #' Can create tables with NULL values.
  roundtrip_null = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = NA)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_true(is.na(tbl_out$a))
    })
  },

  #' Can create tables with 64-bit columns.
  roundtrip_64_bit = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(-1e14, 1e15, 0.25, NA), id = 1:4)
      tbl_in_trunc <- data.frame(a = trunc(tbl_in$a))

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in, field.types = "bigint")

      tbl_out <- dbReadTable(con, "test")
      expect_identical(tbl_in_trunc, tbl_out[order(tbl_out$id), ])
    })
  },

  #' Can create tables with character columns.
  roundtrip_character = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = c(text_cyrillic, text_latin,
                                 text_chinese, text_ascii, NA),
                           id = 1:5, stringsAsFactors = FALSE)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(tbl_in, tbl_out[order(tbl_out$id), ])

      expect_true(all_have_utf8_or_ascii_encoding(tbl_out$a))
    })
  },

  #' Can create tables with factor columns.
  roundtrip_factor = function(ctx) {
    with_connection({
      tbl_in <- data.frame(a = factor(c(text_cyrillic, text_latin,
                                        text_chinese, text_ascii, NA)),
                           id = 1:5, stringsAsFactors = FALSE)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_identical(as.character(tbl_in$a), tbl_out$a[order(tbl_out$id)])

      expect_true(all_have_utf8_or_ascii_encoding(tbl_out$a))
    })
  },

  #' Can create tables with raw columns.
  roundtrip_raw = function(ctx) {
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

  #' Can create tables with date columns.
  roundtrip_date = function(ctx) {
    with_connection({
      tbl_in <- data.frame(id = 1:6)
      tbl_in$a <- c(Sys.Date() + 1:5, NA)

      on.exit(expect_error(dbRemoveTable(con, "test"), NA), add = TRUE)
      dbWriteTable(con, "test", tbl_in)

      tbl_out <- dbReadTable(con, "test")
      expect_equal(tbl_in, tbl_out[order(tbl_out$id), ])
      expect_is(unclass(tbl_out$a), "numeric")
    })
  },

  #' Can create tables with timestamp columns.
  roundtrip_timestamp = function(ctx) {
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

  #' Can create tables with row names.
  roundtrip_rownames = function(ctx) {
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

  #' }
  NULL
)
