#' spec_sql_unquote_identifier
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_unquote_identifier <- list(
  unquote_identifier_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbUnquoteIdentifier)), c("conn", "x", "..."))
  },

  #' @return
  unquote_identifier_return = function(ctx) {
    with_connection({
      #' `dbUnquoteIdentifier()` returns a list of objects
      simple_in <- dbQuoteIdentifier(con, "simple")
      simple_out <- dbUnquoteIdentifier(con, simple_in)
      expect_is(simple_out, "list")
    })
  },

  unquote_identifier_vectorized = function(ctx) {
    with_connection({
      #' of the same length as the input.
      simple_in <- dbQuoteIdentifier(con, "simple")
      simple_out <- dbUnquoteIdentifier(con, simple_in)
      expect_equal(length(simple_out), 1L)

      letters_in <- dbQuoteIdentifier(con, letters)
      letters_out <- dbUnquoteIdentifier(con, letters_in)
      expect_equal(length(letters_out), length(letters_in))

      #' For an empty character vector this function returns a length-0 object.
      empty <- character()
      empty_in <- dbQuoteIdentifier(con, empty)
      empty_out <- dbUnquoteIdentifier(con, empty_in)
      expect_equal(length(empty_out), 0)

      #' The names of the input argument are preserved in the output.
      unnamed_in <- dbQuoteIdentifier(con, letters)
      unnamed_out <- dbUnquoteIdentifier(con, unnamed_in)
      expect_null(names(unnamed_out))
      named_in <- dbQuoteIdentifier(con, stats::setNames(LETTERS[1:3], letters[1:3]))
      named_out <- dbUnquoteIdentifier(con, named_in)
      expect_equal(names(named_out), letters[1:3])

      #' When passing the first element of a returned object again to
      #' `dbUnquoteIdentifier()` as `x`
      #' argument, it is returned unchanged (but wrapped in a list).
      expect_identical(dbUnquoteIdentifier(con, simple_out[[1]]), simple_out)
      expect_identical(dbUnquoteIdentifier(con, letters_out[[1]]), letters_out[1])
      #' Passing objects of class [Id] should also return them unchanged (but wrapped in a list).
      expect_identical(dbUnquoteIdentifier(con, Id(table = "simple")), list(Id(table = "simple")))

      #' (For backends it may be most convenient to return [Id] objects
      #' to achieve this behavior, but this is not required.)
    })
  },

  unquote_identifier_error = function(ctx) {
    with_connection({
      #'
      #' An error is raised if plain character vectors are passed as the `x`
      #' argument.
      expect_error(dbUnquoteIdentifier(con, NA_character_))
      expect_error(dbUnquoteIdentifier(con, c("a", NA_character_)))
      expect_error(dbUnquoteIdentifier(con, character()))
    })
  },

  #' @section Specification:
  #' For any character vector of length one, quoting (with [dbQuoteIdentifier()])
  #' then unquoting then quoting the first element is identical to just quoting.
  unquote_identifier_roundtrip = function(ctx) {
    with_connection({
      simple_in <- dbQuoteIdentifier(con, "simple")
      simple_out <- dbUnquoteIdentifier(con, simple_in)
      simple_roundtrip <- dbQuoteIdentifier(con, simple_out[[1]])
      expect_identical(simple_in, simple_roundtrip)
    })
  },

  unquote_identifier_special = function(ctx) {
    with_connection({
      #' This is also true for strings that
      #' contain special characters such as a space,
      with_space_in <- dbQuoteIdentifier(con, "with space")
      with_space_out <- dbUnquoteIdentifier(con, with_space_in)
      with_space_roundtrip <- dbQuoteIdentifier(con, with_space_out[[1]])
      #' a dot,
      with_dot_in <- dbQuoteIdentifier(con, "with.dot")
      with_dot_out <- dbUnquoteIdentifier(con, with_dot_in)
      with_dot_roundtrip <- dbQuoteIdentifier(con, with_dot_out[[1]])
      #' a comma,
      with_comma_in <- dbQuoteIdentifier(con, "with,comma")
      with_comma_out <- dbUnquoteIdentifier(con, with_comma_in)
      with_comma_roundtrip <- dbQuoteIdentifier(con, with_comma_out[[1]])
      #' or quotes used to mark strings
      with_quote_in <- dbQuoteIdentifier(con, as.character(dbQuoteString(con, "a")))
      with_quote_out <- dbUnquoteIdentifier(con, with_quote_in)
      with_quote_roundtrip <- dbQuoteIdentifier(con, with_quote_out[[1]])
      #' or identifiers,
      quoted_with_space_in <- dbQuoteIdentifier(con, as.character(with_space_in))
      quoted_with_space_out <- dbUnquoteIdentifier(con, quoted_with_space_in)
      quoted_with_space_roundtrip <- dbQuoteIdentifier(con, quoted_with_space_out[[1]])

      quoted_with_dot_in <- dbQuoteIdentifier(con, as.character(with_dot_in))
      quoted_with_dot_out <- dbUnquoteIdentifier(con, quoted_with_dot_in)
      quoted_with_dot_roundtrip <- dbQuoteIdentifier(con, quoted_with_dot_out[[1]])

      quoted_with_comma_in <- dbQuoteIdentifier(con, as.character(with_comma_in))
      quoted_with_comma_out <- dbUnquoteIdentifier(con, quoted_with_comma_in)
      quoted_with_comma_roundtrip <- dbQuoteIdentifier(con, quoted_with_comma_out[[1]])

      quoted_with_quote_in <- dbQuoteIdentifier(con, as.character(with_quote_in))
      quoted_with_quote_out <- dbUnquoteIdentifier(con, quoted_with_quote_in)
      quoted_with_quote_roundtrip <- dbQuoteIdentifier(con, quoted_with_quote_out[[1]])

      #' if the database supports this.
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        skip("tweak: strict_identifier")
      }

      expect_identical(with_space_in, with_space_roundtrip)
      expect_identical(with_dot_in, with_dot_roundtrip)
      expect_identical(with_comma_in, with_comma_roundtrip)
      expect_identical(with_quote_in, with_quote_roundtrip)
      expect_identical(quoted_with_space_in, quoted_with_space_roundtrip)
      expect_identical(quoted_with_dot_in, quoted_with_dot_roundtrip)
      expect_identical(quoted_with_comma_in, quoted_with_comma_roundtrip)
      expect_identical(quoted_with_quote_in, quoted_with_quote_roundtrip)
    })
  },

  #'
  #' Unquoting simple strings (consisting of only letters) wrapped with [SQL()]
  #' and then quoting via [dbQuoteIdentifier()] gives the same result as just
  #' quoting the string.
  unquote_identifier_simple = function(ctx) {
    with_connection({
      simple_in <- "simple"
      simple_quoted <- dbQuoteIdentifier(con, simple_in)
      simple_out <- dbUnquoteIdentifier(con, SQL(simple_in))
      simple_roundtrip <- dbQuoteIdentifier(con, simple_out[[1]])
      expect_identical(simple_roundtrip, simple_quoted)
    })
  },

  #' Similarly, unquoting expressions of the form `SQL("schema.table")`
  #' and then quoting gives the same result as quoting the identifier
  #' constructed by `Id(schema = "schema", table = "table")`.
  unquote_identifier_table_schema = function(ctx) {
    with_connection({
      schema_in <- "schema"
      table_in <- "table"
      simple_quoted <- dbQuoteIdentifier(con, Id(schema = schema_in, table = table_in))
      simple_out <- dbUnquoteIdentifier(con, SQL(paste0(schema_in, ".", table_in)))
      simple_roundtrip <- dbQuoteIdentifier(con, simple_out[[1]])
      expect_identical(simple_roundtrip, simple_quoted)
    })
  },

  NULL
)
