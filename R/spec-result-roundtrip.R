#' spec_result_roundtrip
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_roundtrip <- list(
  #' @section Specification:
  #' The column types of the returned data frame depend on the data returned:
  #' - [integer] (or coercible to an integer) for integer values between -2^31 and 2^31 - 1,
  data_integer = function(ctx, con) {
    #' with [NA] for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, 1L ~ equals_one, -100L ~ equals_minus_100)
  },

  #' - [numeric] for numbers with a fractional component,
  data_numeric = function(ctx, con) {
    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, 1.5, -100.5)
  },

  #' - [logical] for Boolean values (some backends may return an integer);
  data_logical = function(ctx, con) {
    int_values <- 1:0
    values <- ctx$tweaks$logical_return(as.logical(int_values))

    sql_names <- paste0("CAST(", int_values, " AS ", dbDataType(con, logical()), ")")

    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(values, sql_names))
  },

  #' - [character] for text,
  data_character = function(ctx, con) {
    values <- texts
    test_funs <- rep(list(has_utf8_or_ascii_encoding), length(values))
    sql_names <- as.character(dbQuoteString(con, values))

    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(values, sql_names))
    test_select_with_null(.ctx = ctx, con, .dots = setNames(test_funs, sql_names))
  },

  #' - lists of [raw] for blobs
  data_raw = function(ctx, con) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    values <- list(is_raw_list)
    sql_names <- ctx$tweaks$blob_cast(quote_literal(con, list(raw(1))))

    #' with [NULL] entries for SQL NULL values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(values, sql_names))
  },

  #' - coercible using [as.Date()] for dates,
  data_date = function(ctx, con) {
    char_values <- paste0("2015-01-", sprintf("%.2d", 1:12))
    values <- as_date_equals_to(as.Date(char_values))
    sql_names <- ctx$tweaks$date_cast(char_values)

    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(values, sql_names))
  },

  #'   (also applies to the return value of the SQL function `current_date`)
  data_date_current = function(ctx, con) {
    test_select_with_null(
      .ctx = ctx, con,
      "current_date" ~ is_roughly_current_date
    )
  },

  #' - coercible using [hms::as_hms()] for times,
  data_time = function(ctx, con) {
    char_values <- c("00:00:00", "12:34:56")
    time_values <- as_hms_equals_to(hms::as_hms(char_values))
    sql_names <- ctx$tweaks$time_cast(char_values)

    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(time_values, sql_names))
  },

  #'   (also applies to the return value of the SQL function `current_time`)
  data_time_current = function(ctx, con) {
    test_select_with_null(
      .ctx = ctx, con,
      "current_time" ~ coercible_to_time
    )
  },

  #' - coercible using [as.POSIXct()] for timestamps,
  data_timestamp = function(ctx, con) {
    char_values <- c("2015-10-11 00:00:00", "2015-10-11 12:34:56")
    time_values <- rep(list(coercible_to_timestamp), 2L)
    sql_names <- ctx$tweaks$timestamp_cast(char_values)

    #' with NA for SQL `NULL` values
    test_select_with_null(.ctx = ctx, con, .dots = setNames(time_values, sql_names))
  },

  #'   (also applies to the return value of the SQL function `current_timestamp`)
  data_timestamp_current = function(ctx, con) {
    test_select_with_null(
      .ctx = ctx, con,
      "current_timestamp" ~ is_roughly_current_timestamp
    )
  },

  #'
  #' If dates and timestamps are supported by the backend, the following R types are
  #' used:
  #' - [Date] for dates
  data_date_typed = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    char_values <- paste0("2015-01-", sprintf("%.2d", 1:12))
    values <- lapply(char_values, as_numeric_date)
    sql_names <- ctx$tweaks$date_cast(char_values)

    test_select_with_null(.ctx = ctx, con, .dots = setNames(values, sql_names))
  },

  #'   (also applies to the return value of the SQL function `current_date`)
  data_date_current_typed = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$date_typed)) {
      skip("tweak: !date_typed")
    }

    test_select_with_null(
      .ctx = ctx, con,
      "current_date" ~ is_roughly_current_date_typed
    )
  },

  #' - [POSIXct] for timestamps
  data_timestamp_typed = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    char_values <- c("2015-10-11 00:00:00", "2015-10-11 12:34:56")
    timestamp_values <- rep(list(is_timestamp), 2L)
    sql_names <- ctx$tweaks$timestamp_cast(char_values)

    test_select_with_null(.ctx = ctx, con, .dots = setNames(timestamp_values, sql_names))
  },

  #'   (also applies to the return value of the SQL function `current_timestamp`)
  data_timestamp_current_typed = function(ctx, con) {
    if (!isTRUE(ctx$tweaks$timestamp_typed)) {
      skip("tweak: !timestamp_typed")
    }

    test_select_with_null(
      .ctx = ctx, con,
      "current_timestamp" ~ is_roughly_current_timestamp_typed
    )
  },

  #'
  #' R has no built-in type with lossless support for the full range of 64-bit
  #' or larger integers. If 64-bit integers are returned from a query,
  #' the following rules apply:
  #' - Values are returned in a container with support for the full range of
  #'   valid 64-bit values (such as the `integer64` class of the \pkg{bit64}
  #'   package)
  #' - Coercion to numeric always returns a number that is as close as possible
  #'   to the true value
  data_64_bit_numeric = function(ctx, con) {
    char_values <- c("10000000000", "-10000000000")
    test_values <- as_numeric_identical_to(as.numeric(char_values))

    test_select_with_null(.ctx = ctx, con, .dots = setNames(test_values, char_values))
  },

  #' - Loss of precision when converting to numeric gives a warning
  data_64_bit_numeric_warning = function(ctx, con) {
    char_values <- c(" 1234567890123456789", "-1234567890123456789")
    num_values <- as.numeric(char_values)
    test_values <- as_numeric_equals_to(num_values)

    suppressWarnings(
      expect_warning(
        test_select(.ctx = ctx, con, .dots = setNames(test_values, char_values), .add_null = "none")
      )
    )
    suppressWarnings(
      expect_warning(
        test_select(.ctx = ctx, con, .dots = setNames(test_values, char_values), .add_null = "above")
      )
    )
    suppressWarnings(
      expect_warning(
        test_select(.ctx = ctx, con, .dots = setNames(test_values, char_values), .add_null = "below")
      )
    )
  },

  #' - Conversion to character always returns a lossless decimal representation
  #'   of the data
  data_64_bit_lossless = function(ctx, con) {
    char_values <- c("1234567890123456789", "-1234567890123456789")
    test_values <- as_character_equals_to(char_values)

    test_select_with_null(.ctx = ctx, con, .dots = setNames(test_values, char_values))
  },
  #
  NULL
)


test_select_with_null <- function(...) {
  test_select(..., .add_null = "none")
  test_select(..., .add_null = "above")
  test_select(..., .add_null = "below")
}

test_select <- function(con, ..., .dots = NULL, .add_null = "none",
                        .ctx, .envir = parent.frame()) {
  values <- c(list(...), .dots)

  value_is_formula <- vapply(values, is.call, logical(1L))
  names(values)[value_is_formula] <- lapply(values[value_is_formula], "[[", 2L)
  values[value_is_formula] <- lapply(
    values[value_is_formula],
    function(x) {
      eval(x[[3]], envir = .envir)
    }
  )

  if (is.null(names(values))) {
    sql_values <- lapply(values, as.character)
  } else {
    sql_values <- names(values)
  }

  if (isTRUE(.ctx$tweaks$current_needs_parens)) {
    sql_values <- gsub(
      "^(current_(?:date|time|timestamp))$", "\\1()",
      sql_values
    )
  }

  sql_names <- letters[seq_along(sql_values)]

  query <- paste(
    "SELECT",
    paste(sql_values, "as", sql_names, collapse = ", ")
  )
  if (.add_null != "none") {
    query_null <- paste(
      "SELECT",
      paste("NULL as", sql_names, collapse = ", ")
    )
    query <- c(query, query_null)
    if (.add_null == "above") {
      query <- rev(query)
    }
    query <- paste0(query, ", ", 1:2, " as id")
    query <- union(.ctx = .ctx, query)
  }

  rows <- check_df(dbGetQuery(con, query))

  if (.add_null != "none") {
    rows <- rows[order(rows$id), -(length(sql_names) + 1L), drop = FALSE]
    if (.add_null == "above") {
      rows <- rows[2:1, , drop = FALSE]
    }
  }

  expect_identical(names(rows), sql_names)

  for (i in seq_along(values)) {
    value_or_testfun <- values[[i]]
    if (is.function(value_or_testfun)) {
      eval(bquote(expect_true(value_or_testfun(rows[1L, .(i)]))))
    } else {
      eval(bquote(expect_identical(rows[1L, .(i)], .(value_or_testfun))))
    }
  }

  if (.add_null != "none") {
    expect_equal(nrow(rows), 2L)
    if (is.list(rows[[1L]])) {
      expect_true(is.null(rows[2L, 1L][[1L]]))
    } else {
      expect_true(is.na(rows[2L, 1L]))
    }
  } else {
    expect_equal(nrow(rows), 1L)
  }
}

equals_one <- function(x) {
  identical(as.integer(x), 1L) && identical(as.numeric(x), 1)
}

equals_minus_100 <- function(x) {
  identical(as.integer(x), -100L) && identical(as.numeric(x), -100)
}

all_have_utf8_or_ascii_encoding <- function(x) {
  all(vapply(x, has_utf8_or_ascii_encoding, logical(1L)))
}

has_utf8_or_ascii_encoding <- function(x) {
  if (Encoding(x) == "UTF-8") {
    TRUE
  } else if (Encoding(x) == "unknown") {
    # Characters encoded as "unknown" must be ASCII only, and remain "unknown"
    # after attempting to assign an encoding. From ?Encoding :
    # > ASCII strings will never be marked with a declared encoding, since their
    # > representation is the same in all supported encodings.
    Encoding(x) <- "UTF-8"
    Encoding(x) == "unknown"
  } else {
    FALSE
  }
}

is_raw_list <- function(x) {
  is.list(x) && is.raw(x[[1L]])
}

coercible_to_date <- function(x) {
  x_date <- try_silent(as.Date(x))
  !is.null(x_date) && all(is.na(x) == is.na(x_date))
}

as_date_equals_to <- function(x) {
  lapply(x, function(xx) {
    function(value) as.Date(value) == xx
  })
}

is_roughly_current_date <- function(x) {
  coercible_to_date(x) && (abs(Sys.Date() - as.Date(x)) <= 1)
}

coercible_to_time <- function(x) {
  x_hms <- try_silent(hms::as_hms(x))
  !is.null(x_hms) && all(is.na(x) == is.na(x_hms))
}

as_hms_equals_to <- function(x) {
  lapply(x, function(xx) {
    function(value) hms::as_hms(value) == xx
  })
}

coercible_to_timestamp <- function(x) {
  x_timestamp <- try_silent(as.POSIXct(x))
  !is.null(x_timestamp) && all(is.na(x) == is.na(x_timestamp))
}

as_timestamp_equals_to <- function(x) {
  lapply(x, function(xx) {
    function(value) as.POSIXct(value) == xx
  })
}

as_numeric_identical_to <- function(x) {
  lapply(x, function(xx) {
    function(value) as.numeric(value) == xx
  })
}

as_numeric_equals_to <- function(x) {
  lapply(x, function(xx) {
    function(value) isTRUE(all.equal(as.numeric(value), xx))
  })
}

as_character_equals_to <- function(x) {
  lapply(x, function(xx) {
    function(value) as.character(value) == xx
  })
}

is_roughly_current_timestamp <- function(x) {
  coercible_to_timestamp(x) && (Sys.time() - as.POSIXct(x, tz = "UTC") <= hms::hms(2))
}

is_date <- function(x) {
  inherits(x, "Date")
}

is_roughly_current_date_typed <- function(x) {
  is_date(x) && (abs(Sys.Date() - x) <= 1)
}

is_timestamp <- function(x) {
  inherits(x, "POSIXct")
}

is_roughly_current_timestamp_typed <- function(x) {
  is_timestamp(x) && (Sys.time() - x <= hms::hms(2))
}

as_numeric_date <- function(d) {
  d <- as.Date(d)
  structure(as.numeric(unclass(d)), class = class(d))
}

quote_literal <- function(con, x) {
  DBI::dbQuoteLiteral(con, x)
}
