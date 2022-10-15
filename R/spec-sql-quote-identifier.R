#' spec_sql_quote_identifier
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_quote_identifier <- list(
  quote_identifier_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbQuoteIdentifier)), c("conn", "x", "..."))
  },

  quote_identifier_return = function(con) {
    #' @return
    #' `dbQuoteIdentifier()` returns an object that can be coerced to [character],
    simple_out <- dbQuoteIdentifier(con, "simple")
    expect_error(as.character(simple_out), NA)
    expect_type(as.character(simple_out), "character")
  },
  #
  quote_identifier_vectorized = function(ctx, con) {
    #' of the same length as the input.
    simple <- "simple"
    simple_out <- dbQuoteIdentifier(con, simple)
    expect_equal(length(simple_out), 1L)

    letters_out <- dbQuoteIdentifier(con, letters)
    expect_equal(length(letters_out), length(letters))

    #' For an empty character vector this function returns a length-0 object.
    empty <- character()
    empty_out <- dbQuoteIdentifier(con, empty)
    expect_equal(length(empty_out), 0L)

    #' The names of the input argument are preserved in the output.
    unnamed <- letters
    unnamed_out <- dbQuoteIdentifier(con, unnamed)
    expect_null(names(unnamed_out))
    named <- stats::setNames(LETTERS[1:3], letters[1:3])
    named_out <- dbQuoteIdentifier(con, named)
    expect_equal(names(named_out), letters[1:3])

    #' When passing the returned object again to `dbQuoteIdentifier()`
    #' as `x`
    #' argument, it is returned unchanged.
    expect_identical(dbQuoteIdentifier(con, simple_out), simple_out)
    expect_identical(dbQuoteIdentifier(con, letters_out), letters_out)
    expect_identical(dbQuoteIdentifier(con, empty_out), empty_out)
    #' Passing objects of class [SQL] should also return them unchanged.
    expect_identical(dbQuoteIdentifier(con, SQL(simple)), SQL(simple))
    expect_identical(dbQuoteIdentifier(con, SQL(letters)), SQL(letters))
    expect_identical(dbQuoteIdentifier(con, SQL(empty)), SQL(empty))

    #' (For backends it may be most convenient to return [SQL] objects
    #' to achieve this behavior, but this is not required.)
  },
  #'
  quote_identifier_error = function(ctx, con) {
    #' @section Failure modes:
    #'
    #' An error is raised if the input contains `NA`,
    expect_error(dbQuoteIdentifier(con, NA))
    expect_error(dbQuoteIdentifier(con, NA_character_))
    expect_error(dbQuoteIdentifier(con, c("a", NA_character_)))
    #' but not for an empty string.
    expect_error(dbQuoteIdentifier(con, ""), NA)
  },

  quote_identifier = function(ctx, con) {
    #' @section Specification:
    #' Calling [dbGetQuery()] for a query of the format `SELECT 1 AS ...`
    #' returns a data frame with the identifier, unquoted, as column name.
    #' Quoted identifiers can be used as table and column names in SQL queries,
    simple <- dbQuoteIdentifier(con, "simple")

    #' in particular in queries like `SELECT 1 AS ...`
    query <- trivial_query(column = simple)
    rows <- check_df(dbGetQuery(con, query))
    expect_identical(names(rows), "simple")
    expect_identical(unlist(unname(rows)), 1.5)

    #' and `SELECT * FROM (SELECT 1) ...`.
    query <- paste0("SELECT * FROM (", trivial_query(), ") ", simple)
    rows <- check_df(dbGetQuery(con, query))
    expect_identical(unlist(unname(rows)), 1.5)
  },

  quote_identifier_string = function(ctx, con) {
    #' The method must use a quoting mechanism that is unambiguously different
    #' from the quoting mechanism used for strings, so that a query like
    #' `SELECT ... FROM (SELECT 1 AS ...)`
    query <- paste0(
      "SELECT ", dbQuoteIdentifier(con, "b"), " FROM (",
      "SELECT 1 AS ", dbQuoteIdentifier(con, "a"), ")"
    )

    #' throws an error if the column names do not match.
    eval(bquote(expect_error(dbGetQuery(con, .(query)))))
  },
  #
  #'
  quote_identifier_special = function(ctx, con) {
    #' The method can quote column names that
    #' contain special characters such as a space,
    with_space_in <- "with space"
    with_space <- dbQuoteIdentifier(con, with_space_in)
    #' a dot,
    with_dot_in <- "with.dot"
    with_dot <- dbQuoteIdentifier(con, with_dot_in)
    #' a comma,
    with_comma_in <- "with,comma"
    with_comma <- dbQuoteIdentifier(con, with_comma_in)
    #' or quotes used to mark strings
    with_quote_in <- as.character(dbQuoteString(con, "a"))
    with_quote <- dbQuoteIdentifier(con, with_quote_in)
    #' or identifiers,
    empty_in <- ""
    empty <- dbQuoteIdentifier(con, empty_in)
    quoted_empty <- dbQuoteIdentifier(con, as.character(empty))
    quoted_with_space <- dbQuoteIdentifier(con, as.character(with_space))
    quoted_with_dot <- dbQuoteIdentifier(con, as.character(with_dot))
    quoted_with_comma <- dbQuoteIdentifier(con, as.character(with_comma))
    quoted_with_quote <- dbQuoteIdentifier(con, as.character(with_quote))

    #' if the database supports this.
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    #' In any case, checking the validity of the identifier
    #' should be performed only when executing a query,
    #' and not by `dbQuoteIdentifier()`.
    query <- paste0(
      "SELECT ",
      "2.5 as", with_space, ",",
      "3.5 as", with_dot, ",",
      "4.5 as", with_comma, ",",
      "5.5 as", with_quote, ",",
      "6.5 as", quoted_empty, ",",
      "7.5 as", quoted_with_space, ",",
      "8.5 as", quoted_with_dot, ",",
      "9.5 as", quoted_with_comma, ",",
      "10.5 as", quoted_with_quote
    )

    rows <- check_df(dbGetQuery(con, query))
    expect_identical(
      names(rows),
      c(
        with_space_in, with_dot_in, with_comma_in,
        with_quote_in,
        as.character(empty), as.character(with_space),
        as.character(with_dot), as.character(with_comma),
        as.character(with_quote)
      )
    )
    expect_identical(unlist(unname(rows)), 2:10 + 0.5)
  },
  #
  NULL
)
