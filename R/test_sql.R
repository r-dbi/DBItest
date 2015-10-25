#' Test SQL methods
#'
#' @inheritParams test_all
#' @include test_result.R
#' @family tests
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{quote_string}}{
    #' Can quote strings, and create strings that contain quotes and spaces
    #' }
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

    #' \item{\code{}}{
    #' Can quote identifiers, and create identifiers that contain quotes and
    #' spaces
    #' }
    quote_identifier = function() {
      with_connection({
        simple <- dbQuoteIdentifier(con, "simple")
        with_spaces <- dbQuoteIdentifier(con, "with spaces")
        quoted_simple <- dbQuoteIdentifier(con, as.character(simple))
        quoted_with_spaces <- dbQuoteIdentifier(con, as.character(with_spaces))

        query <- paste0("SELECT ",
                        "1 as", simple, ",",
                        "2 as", with_spaces, ",",
                        "3 as", quoted_simple, ",",
                        "4 as", quoted_with_spaces)

        expect_warning(rows <- dbGetQuery(con, query), NA)
        expect_equal(names(rows), c("simple", "with spaces",
                                    as.character(simple),
                                    as.character(with_spaces)))
        expect_equal(unlist(unname(rows)), 1:4)
      })
    },

    NULL
  )
  #' }
  run_tests(tests, skip, test_suite)
}
