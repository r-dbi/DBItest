#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbQuoteIdentifier("DBIConnection")`}{
spec_sql_quote_identifier <- list(
  #' Can quote identifiers that consist of letters only.
  quote_identifier = function(ctx) {
    with_connection({
      simple <- dbQuoteIdentifier(con, "simple")

      query <- paste0("SELECT 1 as", simple)

      expect_warning(rows <- dbGetQuery(con, query), NA)
      expect_identical(names(rows), "simple")
      expect_identical(unlist(unname(rows)), 1L)
    })
  },

  #' Can quote identifiers with special characters, and create identifiers
  #' that contain quotes and spaces.
  quote_identifier_special = function(ctx) {
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    with_connection({
      simple <- dbQuoteIdentifier(con, "simple")
      with_space <- dbQuoteIdentifier(con, "with space")
      with_dot <- dbQuoteIdentifier(con, "with.dot")
      with_comma <- dbQuoteIdentifier(con, "with,comma")
      quoted_simple <- dbQuoteIdentifier(con, as.character(simple))
      quoted_with_space <- dbQuoteIdentifier(con, as.character(with_space))
      quoted_with_dot <- dbQuoteIdentifier(con, as.character(with_dot))
      quoted_with_comma <- dbQuoteIdentifier(con, as.character(with_comma))

      query <- paste0("SELECT ",
                      "1 as", simple, ",",
                      "2 as", with_space, ",",
                      "3 as", with_dot, ",",
                      "4 as", with_comma, ",",
                      "5 as", quoted_simple, ",",
                      "6 as", quoted_with_space, ",",
                      "7 as", quoted_with_dot, ",",
                      "8 as", quoted_with_comma)

      expect_warning(rows <- dbGetQuery(con, query), NA)
      expect_identical(names(rows),
                       c("simple", "with space", "with.dot", "with,comma",
                         as.character(simple), as.character(with_space),
                         as.character(with_dot), as.character(with_comma)))
      expect_identical(unlist(unname(rows)), 1:8)
    })
  },

  #' Character vectors are treated as a single qualified identifier.
  quote_identifier_not_vectorized = function(ctx) {
    with_connection({
      simple_out <- dbQuoteIdentifier(con, "simple")
      expect_equal(length(simple_out), 1L)
      letters_out <- dbQuoteIdentifier(con, letters[1:3])
      expect_equal(length(letters_out), 1L)
    })
  },

  #' }
  NULL
)
