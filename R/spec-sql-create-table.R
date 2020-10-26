#' spec_sql_create_table
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_create_table <- list(
  create_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbCreateTable)), c("conn", "name", "fields", "...", "row.names", "temporary"))
  },

  #' @return
  #' `dbCreateTable()` returns `TRUE`, invisibly.
  create_table_return = function(con, table_name) {
    expect_invisible_true(dbCreateTable(con, table_name, trivial_df()))
  },

  #' If the table exists, an error is raised; the remote table remains unchanged.
  create_table_overwrite = function(con, table_name) {
    test_in <- trivial_df()

    dbCreateTable(con, table_name, test_in)
    dbAppendTable(con, table_name, test_in)
    expect_error(dbCreateTable(con, table_name, data.frame(b = 1L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #'
  #' An error is raised when calling this method for a closed
  create_table_closed_connection = function(ctx, closed_con) {
    expect_error(dbCreateTable(closed_con, "test", data.frame(a = 1)))
  },

  #' or invalid connection.
  create_table_invalid_connection = function(ctx, invalid_con) {
    expect_error(dbCreateTable(invalid_con, "test", data.frame(a = 1)))
  },

  #' An error is also raised
  create_table_error = function(ctx, con, table_name) {
    test_in <- data.frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbCreateTable(con, NA, test_in))
    #' or if this results in a non-scalar.
    expect_error(dbCreateTable(con, c(table_name, table_name), test_in))

    #' Invalid values for the `row.names` and `temporary` arguments
    #' (non-scalars,
    expect_error(dbCreateTable(con, table_name, test_in, row.names = letters))
    expect_error(dbCreateTable(con, table_name, test_in, temporary = c(TRUE, FALSE)))
    #' unsupported data types,
    expect_error(dbCreateTable(con, table_name, test_in, row.names = list(1L)))
    expect_error(dbCreateTable(con, table_name, fields = 1L))
    expect_error(dbCreateTable(con, table_name, test_in, temporary = 1L))
    #' `NA`,
    expect_error(dbCreateTable(con, table_name, test_in, row.names = NA))
    expect_error(dbCreateTable(con, table_name, fields = NA))
    expect_error(dbCreateTable(con, table_name, test_in, temporary = NA))
    #' incompatible values,
    expect_error(dbCreateTable(con, table_name, test_in, fields = letters))
    #' duplicate names)
    expect_error(dbCreateTable(con, table_name, fields = c(a = "INTEGER", a = "INTEGER")))

    #' also raise an error.
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbCreateTable()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `temporary` (default: `FALSE`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  #' @section Specification:
  #' The `name` argument is processed as follows,
  create_table_name = function(ctx, con) {
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
        #' - If an unquoted table name as string: `dbCreateTable()` will do the quoting,
        dbCreateTable(con, table_name, test_in)
        test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
        expect_equal_df(test_out, test_in[0, , drop = FALSE])
        #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
      })

      with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
        #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
        dbCreateTable(con, dbQuoteIdentifier(con, table_name), test_in)
        test_out <- check_df(dbReadTable(con, table_name))
        expect_equal_df(test_out, test_in[0, , drop = FALSE])
      })
    }
  },

  #'
  #' If the `temporary` argument is `TRUE`, the table is not available in a
  #' second connection and is gone after reconnecting.
  create_temporary_table = function(ctx, con, table_name = "dbit03") {
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    iris <- get_iris(ctx)[1:30, ]
    dbCreateTable(con, table_name, iris, temporary = TRUE)
    iris_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(iris_out, iris[0, , drop = FALSE])

    con2 <- local_connection(ctx)
    expect_error(dbReadTable(con2, table_name))
  },
  # second stage
  create_temporary_table = function(con) {
    table_name <- "dbit03"
    expect_error(dbReadTable(con, table_name))
  },

  #' A regular, non-temporary table is visible in a second connection
  create_table_visible_in_other_connection = function(ctx, con) {
    iris <- get_iris(ctx)[1:30, ]

    table_name <- "dbit04"
    dbCreateTable(con, table_name, iris)
    iris_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(iris_out, iris[0, , drop = FALSE])

    con2 <- local_connection(ctx)
    expect_equal_df(dbReadTable(con2, table_name), iris[0, , drop = FALSE])
  },
  # second stage
  create_table_visible_in_other_connection = function(con, table_name = "dbit04") {
    #' and after reconnecting to the database.
    expect_equal_df(check_df(dbReadTable(con, table_name)), iris[0, , drop = FALSE])
  },

  #'
  #' SQL keywords can be used freely in table names, column names, and data.
  create_roundtrip_keywords = function(ctx, con) {
    tbl_in <- data.frame(
      SELECT = "UNIQUE", FROM = "JOIN", WHERE = "ORDER",
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in, name = "EXISTS")
  },

  #' Quotes, commas, and spaces can also be used  for table names and column names,
  #' if the database supports non-syntactic identifiers.
  create_roundtrip_quotes = function(ctx, con) {
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    table_names <- c(
      as.character(dbQuoteIdentifier(con, "")),
      as.character(dbQuoteString(con, "")),
      "with space",
      ","
    )

    for (table_name in table_names) {
      tbl_in <- trivial_df(4, letters[1:4])

      names(tbl_in) <- c(
        as.character(dbQuoteIdentifier(con, "")),
        as.character(dbQuoteString(con, "")),
        "with space",
        ","
      )

      test_table_roundtrip(con, tbl_in)
    }
  },

  #'
  #' The `row.names` argument must be missing
  create_table_row_names_default = function(ctx, con, table_name) {
    mtcars_in <- datasets::mtcars
    dbCreateTable(con, table_name, mtcars_in)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = FALSE))

    expect_false("row_names" %in% names(mtcars_out))
    expect_equal_df(mtcars_out, unrowname(mtcars_in)[0, , drop = FALSE])
  },
  #' or `NULL`, the default value.
  create_table_row_names_null = function(ctx, con, table_name) {
    mtcars_in <- datasets::mtcars
    dbCreateTable(con, table_name, mtcars_in, row.names = NULL)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = NULL))

    expect_false("row_names" %in% names(mtcars_out))
    expect_equal_df(mtcars_out, unrowname(mtcars_in)[0, , drop = FALSE])
  },
  #
  create_table_row_names_non_null = function(ctx, con, table_name) {
    #' All other values for the `row.names` argument
    mtcars_in <- datasets::mtcars

    #' (in particular `TRUE`,
    expect_error(dbCreateTable(con, table_name, mtcars_in, row.names = TRUE))
    #' `NA`,
    expect_error(dbCreateTable(con, table_name, mtcars_in, row.names = NA))
    #' and a string)
    expect_error(dbCreateTable(con, table_name, mtcars_in, row.names = "make_model"))
    #' raise an error.
  },
  #
  NULL
)
