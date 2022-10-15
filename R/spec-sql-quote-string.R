#' spec_sql_quote_string
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_quote_string <- list(
  quote_string_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbQuoteString)), c("conn", "x", "..."))
  },

  quote_string_return = function(con) {
    #' @return
    #' `dbQuoteString()` returns an object that can be coerced to [character],
    simple <- "simple"
    simple_out <- dbQuoteString(con, simple)
    expect_error(as.character(simple_out), NA)
    expect_type(as.character(simple_out), "character")
    expect_equal(length(simple_out), 1L)
  },
  #
  quote_string_vectorized = function(con) {
    #' of the same length as the input.
    letters_out <- dbQuoteString(con, letters)
    expect_equal(length(letters_out), length(letters))

    #' For an empty character vector this function returns a length-0 object.
    empty_out <- dbQuoteString(con, character())
    expect_equal(length(empty_out), 0L)
  },
  #
  quote_string_double = function(con) {
    simple <- "simple"
    simple_out <- dbQuoteString(con, simple)

    letters_out <- dbQuoteString(con, letters)

    empty <- character()
    empty_out <- dbQuoteString(con, character())

    #'
    #' When passing the returned object again to `dbQuoteString()`
    #' as `x`
    #' argument, it is returned unchanged.
    expect_identical(dbQuoteString(con, simple_out), simple_out)
    expect_identical(dbQuoteString(con, letters_out), letters_out)
    expect_identical(dbQuoteString(con, empty_out), empty_out)
    #' Passing objects of class [SQL] should also return them unchanged.
    expect_identical(dbQuoteString(con, SQL(simple)), SQL(simple))
    expect_identical(dbQuoteString(con, SQL(letters)), SQL(letters))
    expect_identical(dbQuoteString(con, SQL(empty)), SQL(empty))

    #' (For backends it may be most convenient to return [SQL] objects
    #' to achieve this behavior, but this is not required.)
  },

  quote_string_roundtrip = function(ctx, con) {
    #' @section Specification:
    do_test_string <- function(x) {
      #' The returned expression can be used in a `SELECT ...` query,
      query <- paste0("SELECT ", paste(dbQuoteString(con, x), collapse = ", "))
      #' and for any scalar character `x` the value of
      #' \code{dbGetQuery(paste0("SELECT ", dbQuoteString(x)))[[1]]}
      #' must be identical to `x`,
      x_out <- check_df(dbGetQuery(con, query))
      expect_equal(nrow(x_out), 1L)
      expect_identical(unlist(unname(x_out)), x)
    }

    expand_char <- function(...) {
      df <- expand.grid(..., stringsAsFactors = FALSE)
      do.call(paste0, df)
    }

    test_chars <- c(
      #' even if `x` contains
      "",
      #' spaces,
      " ",
      #' tabs,
      "\t",
      #' quotes (single
      "'",
      #' or double),
      '"',
      #' backticks,
      "`",
      #' or newlines
      "\n"
    )
    #' (in any combination)
    # length(test_chars) ** 3
    test_strings_0 <- expand_char(test_chars, "a", test_chars, "b", test_chars)

    #' or is itself the result of a `dbQuoteString()` call coerced back to
    #' character (even repeatedly).
    test_strings_1 <- as.character(dbQuoteString(con, test_strings_0))
    test_strings_2 <- as.character(dbQuoteString(con, test_strings_1))

    test_strings <- c(test_strings_0, test_strings_1, test_strings_2)
    do_test_string(test_strings)
  },
  #
  quote_string_na = function(ctx, con) {
    null <- dbQuoteString(con, NA_character_)
    quoted_null <- dbQuoteString(con, as.character(null))
    na <- dbQuoteString(con, "NA")
    quoted_na <- dbQuoteString(con, as.character(na))

    query <- paste0(
      "SELECT ",
      null, " AS null_return,",
      na, " AS na_return,",
      quoted_null, " AS quoted_null,",
      quoted_na, " AS quoted_na"
    )

    #' If `x` is `NA`, the result must merely satisfy [is.na()].
    rows <- check_df(dbGetQuery(con, query))
    expect_true(is.na(rows$null_return))
    #' The strings `"NA"` or `"NULL"` are not treated specially.
    expect_identical(rows$na_return, "NA")
    expect_identical(rows$quoted_null, as.character(null))
    expect_identical(rows$quoted_na, as.character(na))
  },
  #
  #'
  quote_string_na_is_null = function(ctx, con) {
    #' `NA` should be translated to an unquoted SQL `NULL`,
    null <- dbQuoteString(con, NA_character_)
    #' so that the query `SELECT * FROM (SELECT 1) a WHERE ... IS NULL`
    rows <- check_df(dbGetQuery(con, paste0("SELECT * FROM (SELECT 1) a WHERE ", null, " IS NULL")))
    #' returns one row.
    expect_equal(nrow(rows), 1L)
  },
  #'
  quote_string_error = function(ctx, con) {
    #' @section Failure modes:
    #'
    #' Passing a numeric,
    expect_error(dbQuoteString(con, c(1, 2, 3)))
    #' integer,
    expect_error(dbQuoteString(con, 1:3))
    #' logical,
    expect_error(dbQuoteString(con, c(TRUE, FALSE)))
    #' or raw vector,
    expect_error(dbQuoteString(con, as.raw(1:3)))
    #' or a list
    expect_error(dbQuoteString(con, as.list(1:3)))
    #' for the `x` argument raises an error.
  },
  #
  NULL
)
