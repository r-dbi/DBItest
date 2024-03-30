#' spec_sql_quote_literal
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_quote_literal <- list(
  quote_literal_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbQuoteLiteral)), c("conn", "x", "..."))
  },

  quote_literal_return = function(con) {
    #' @return
    #' `dbQuoteLiteral()` returns an object that can be coerced to [character],
    simple <- "simple"
    simple_out <- dbQuoteLiteral(con, simple)
    expect_error(as.character(simple_out), NA)
    expect_type(as.character(simple_out), "character")
    expect_equal(length(simple_out), 1L)
  },
  #
  quote_literal_vectorized = function(con) {
    #' of the same length as the input.
    letters_out <- dbQuoteLiteral(con, letters)
    expect_equal(length(letters_out), length(letters))
  },
  #
  quote_literal_empty = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.7.2")

    #' For an empty
    #' integer,
    expect_equal(length(dbQuoteLiteral(con, integer())), 0L)
    #' numeric,
    expect_equal(length(dbQuoteLiteral(con, numeric())), 0L)
    #' character,
    expect_equal(length(dbQuoteLiteral(con, character())), 0L)
    #' logical,
    expect_equal(length(dbQuoteLiteral(con, logical())), 0L)
    #' date,
    expect_equal(length(dbQuoteLiteral(con, Sys.Date()[0])), 0L)
    #' time,
    expect_equal(length(dbQuoteLiteral(con, Sys.time()[0])), 0L)
    #' or blob vector,
    expect_equal(length(dbQuoteLiteral(con, list())), 0L)
    #' this function returns a length-0 object.
  },
  #
  quote_literal_double = function(con) {
    simple <- "simple"
    simple_out <- dbQuoteLiteral(con, simple)

    letters_out <- dbQuoteLiteral(con, letters)

    empty <- character()
    empty_out <- dbQuoteLiteral(con, character())

    #'
    #' When passing the returned object again to `dbQuoteLiteral()`
    #' as `x`
    #' argument, it is returned unchanged.
    expect_identical(dbQuoteLiteral(con, simple_out), simple_out)
    expect_identical(dbQuoteLiteral(con, letters_out), letters_out)
    expect_identical(dbQuoteLiteral(con, empty_out), empty_out)
    #' Passing objects of class [SQL] should also return them unchanged.
    expect_identical(dbQuoteLiteral(con, SQL(simple)), SQL(simple))
    expect_identical(dbQuoteLiteral(con, SQL(letters)), SQL(letters))
    expect_identical(dbQuoteLiteral(con, SQL(empty)), SQL(empty))

    #' (For backends it may be most convenient to return [SQL] objects
    #' to achieve this behavior, but this is not required.)
  },

  quote_literal_roundtrip = function(ctx, con) {
    #' @section Specification:
    do_test_literal <- function(x) {
      #' The returned expression can be used in a `SELECT ...` query,
      literals <- map_chr(x, dbQuoteLiteral, conn = con)
      query <- paste0("SELECT ", paste(literals, collapse = ", "))
      #' and the value of
      #' \code{dbGetQuery(paste0("SELECT ", dbQuoteLiteral(x)))[[1]]}
      #' must be equal to `x`
      x_out <- check_df(dbGetQuery(con, query))
      expect_equal(nrow(x_out), 1L)

      is_logical <- map_lgl(x, is.logical)
      x_out[is_logical] <- map(x_out[is_logical], as.logical)
      is_numeric <- map_lgl(x, is.numeric)
      x_out[is_numeric] <- map(x_out[is_numeric], as.numeric)
      expect_equal(as.list(unname(x_out)), x)
    }

    #' for any scalar
    test_literals <- list(
      #' integer,
      1L,
      #' numeric,
      2.5,
      #' string,
      "string",
      #' and logical.
      TRUE
    )
    do_test_literal(test_literals)
  },
  #
  quote_literal_na = function(ctx, con) {
    null <- dbQuoteLiteral(con, NA_character_)
    quoted_null <- dbQuoteLiteral(con, as.character(null))
    na <- dbQuoteLiteral(con, "NA")
    quoted_na <- dbQuoteLiteral(con, as.character(na))

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
    #' The literals `"NA"` or `"NULL"` are not treated specially.
    expect_identical(rows$na_return, "NA")
    expect_identical(rows$quoted_null, as.character(null))
    expect_identical(rows$quoted_na, as.character(na))
  },
  #
  #'
  quote_literal_na_is_null = function(ctx, con) {
    #' `NA` should be translated to an unquoted SQL `NULL`,
    null <- dbQuoteLiteral(con, NA_character_)
    #' so that the query `SELECT * FROM (SELECT 1) a WHERE ... IS NULL`
    rows <- check_df(dbGetQuery(con, paste0("SELECT * FROM (SELECT 1) a WHERE ", null, " IS NULL")))
    #' returns one row.
    expect_equal(nrow(rows), 1L)
  },
  #'
  quote_literal_error = function(ctx, con) {
    #' @section Failure modes:
    #'
    #' Passing a list
    expect_error(dbQuoteString(con, as.list(1:3)))
    #' for the `x` argument raises an error.
  },
  #
  NULL
)
