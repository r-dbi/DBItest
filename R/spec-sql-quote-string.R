#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbQuoteString("DBIConnection")`}{
spec_sql_quote_string <- list(
  #' Can quote strings, and create strings that contain quotes and spaces.
  quote_string = function(ctx) {
    with_connection({
      simple <- dbQuoteString(con, "simple")
      with_spaces <- dbQuoteString(con, "with spaces")
      quoted_simple <- dbQuoteString(con, as.character(simple))
      quoted_with_spaces <- dbQuoteString(con, as.character(with_spaces))
      null <- dbQuoteString(con, NA_character_)
      quoted_null <- dbQuoteString(con, as.character(null))
      na <- dbQuoteString(con, "NA")
      quoted_na <- dbQuoteString(con, as.character(na))

      query <- paste0("SELECT",
                      simple, "as simple,",
                      with_spaces, "as with_spaces,",
                      null, " as null_return,",
                      na, "as na_return,",
                      quoted_simple, "as quoted_simple,",
                      quoted_with_spaces, "as quoted_with_spaces,",
                      quoted_null, "as quoted_null,",
                      quoted_na, "as quoted_na")

      expect_warning(rows <- dbGetQuery(con, query), NA)
      expect_identical(rows$simple, "simple")
      expect_identical(rows$with_spaces, "with spaces")
      expect_true(is.na(rows$null_return))
      expect_identical(rows$na_return, "NA")
      expect_identical(rows$quoted_simple, as.character(simple))
      expect_identical(rows$quoted_with_spaces, as.character(with_spaces))
      expect_identical(rows$quoted_null, as.character(null))
      expect_identical(rows$quoted_na, as.character(na))
    })
  },

  #' Can quote more than one string at once by passing a character vector.
  quote_string_vectorized = function(ctx) {
    with_connection({
      simple_out <- dbQuoteString(con, "simple")
      expect_equal(length(simple_out), 1L)
      letters_out <- dbQuoteString(con, letters)
      expect_equal(length(letters_out), length(letters))
    })
  },

  #' }
  NULL
)
