#' \code{test_sql()} tests SQL methods.
#'
#' @rdname test
#' @include test_result.R
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  tests <- list(
    # Can quote strings
    quote_string = function() {
      with_connection({
        simple <- dbQuoteString(con, "simple")
        with_spaces <- dbQuoteString(con, "with spaces")
        quoted_simple <- dbQuoteString(con, as.character(simple))
        quoted_with_spaces <- dbQuoteString(con, as.character(with_spaces))

        query <- paste0("SELECT",
                        simple, "as simple,",
                        with_spaces, "as with_spaces,",
                        quoted_simple, "as quoted_simple,",
                        quoted_with_spaces, "as quoted_with_spaces")

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(rows$simple, "simple")
        expect_equal(rows$with_spaces, "with spaces")
        expect_equal(rows$quoted_simple, as.character(simple))
        expect_equal(rows$quoted_with_spaces, as.character(with_spaces))
      })
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
