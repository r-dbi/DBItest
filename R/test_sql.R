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

    #' \item{\code{quote_identifier}}{
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

    #' \item{\code{write_table}}{
    #' Can write the \code{\link[datasets]{iris}} data as a table to the
    #' database, but won't overwrite by default.
    #' }
    write_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(dbGetQuery(con, "DROP TABLE iris"), add = TRUE)
        dbWriteTable(con, "iris", iris)
        expect_error(dbWriteTable(con, "iris", iris))
      })
    },

    #' \item{\code{read_table}}{
    #' Can read the \code{\link[datasets]{iris}} data from a database table.
    #' }
    read_table = function() {
      with_connection({
        expect_error(dbGetQuery(con, "SELECT * FROM iris"))
        on.exit(dbGetQuery(con, "DROP TABLE iris"), add = TRUE)

        iris_in <- iris
        iris_in$Species <- as.character(iris_in$Species)
        order_in <- do.call(order, iris_in)

        dbWriteTable(con, "iris", iris_in)
        iris_out <- dbReadTable(con, "iris")
        order_out <- do.call(order, iris_out)

        expect_equal(iris_in[order_in, ], iris_out[order_out, ])
      })
    },

    NULL
  )
  #' }
  run_tests(tests, skip, test_suite)
}
