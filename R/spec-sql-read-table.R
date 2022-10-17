#' spec_sql_read_table
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_read_table <- list(
  read_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbReadTable)), c("conn", "name", "..."))
  },

  read_table = function(ctx, con, table_name) {
    #' @return
    #' `dbReadTable()` returns a data frame that contains the complete data
    #' from the remote table, effectively the result of calling [dbGetQuery()]
    #' with `SELECT * FROM <name>`.
    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in)
    penguins_out <- check_df(dbReadTable(con, table_name))

    expect_equal_df(penguins_out, penguins_in)
  },

  #'
  read_table_missing = function(con, table_name) {
    #' @section Failure modes:
    #' An error is raised if the table does not exist.
    stopifnot(!dbExistsTable(con, table_name))
    expect_error(dbReadTable(con, table_name))
  },

  read_table_empty = function(ctx, con, table_name) {
    #' @return
    #' An empty table is returned as a data frame with zero rows.
    penguins_in <- get_penguins(ctx)[integer(), ]
    dbWriteTable(con, table_name, penguins_in)
    penguins_out <- check_df(dbReadTable(con, table_name))

    expect_equal(nrow(penguins_out), 0L)
    expect_equal_df(penguins_out, penguins_in)
  },

  #'
  read_table_row_names_false = function(con) {
    #' The presence of [rownames] depends on the `row.names` argument,
    #' see [sqlColumnToRownames()] for details:
    #' - If `FALSE` or `NULL`, the returned data frame doesn't have row names.
    for (row.names in list(FALSE, NULL)) {
      table_name <- random_table_name()
      local_remove_test_table(con, table_name)
      mtcars_in <- datasets::mtcars
      dbWriteTable(con, table_name, mtcars_in, row.names = TRUE)
      mtcars_out <- check_df(dbReadTable(con, table_name, row.names = row.names))

      expect_true("row_names" %in% names(mtcars_out))
      expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
      expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
      expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
    }
  },
  #
  read_table_row_names_true_exists = function(con, table_name) {
    #' - If `TRUE`, a column named "row_names" is converted to row names.
    row.names <- TRUE

    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in, row.names = NA)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = row.names))

    expect_equal_df(mtcars_out, mtcars_in)
  },
  #'
  read_table_row_names_true_missing = function(ctx, con, table_name) {
    #' @section Failure modes:
    #' An error is raised if `row.names` is `TRUE` and no "row_names" column exists,
    row.names <- TRUE

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = NA)
    expect_error(dbReadTable(con, table_name, row.names = row.names))
  },
  #
  read_table_row_names_na_exists = function(con, table_name) {
    #' @return
    #' - If `NA`, a column named "row_names" is converted to row names if it exists,
    row.names <- NA

    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in, row.names = TRUE)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = row.names))

    expect_equal_df(mtcars_out, mtcars_in)
  },
  #
  read_table_row_names_na_missing = function(ctx, con, table_name) {
    #'   otherwise no translation occurs.
    row.names <- NA

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = FALSE)
    penguins_out <- check_df(dbReadTable(con, table_name, row.names = row.names))

    expect_equal_df(penguins_out, penguins_in)
  },
  #
  read_table_row_names_string_exists = function(con, table_name) {
    #' - If a string, this specifies the name of the column in the remote table
    #'   that contains the row names.
    row.names <- "make_model"

    mtcars_in <- datasets::mtcars
    mtcars_in$make_model <- rownames(mtcars_in)
    mtcars_in <- unrowname(mtcars_in)

    dbWriteTable(con, table_name, mtcars_in, row.names = FALSE)
    mtcars_out <- check_df(dbReadTable(con, table_name, row.names = row.names))

    expect_false("make_model" %in% names(mtcars_out))
    expect_true(all(mtcars_in$make_model %in% rownames(mtcars_out)))
    expect_true(all(rownames(mtcars_out) %in% mtcars_in$make_model))
    expect_equal_df(unrowname(mtcars_out), mtcars_in[names(mtcars_in) != "make_model"])
  },
  #'
  read_table_row_names_string_missing = function(ctx, con, table_name) {
    #' @section Failure modes:
    #' An error is raised if `row.names` is set to a string and no corresponding column exists.
    row.names <- "missing"

    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in, row.names = FALSE)
    expect_error(dbReadTable(con, table_name, row.names = row.names))
  },

  read_table_row_names_default = function(con, table_name) {
    #' @return
    #' The default is `row.names = FALSE`.
    #'
    mtcars_in <- datasets::mtcars
    dbWriteTable(con, table_name, mtcars_in, row.names = TRUE)
    mtcars_out <- check_df(dbReadTable(con, table_name))

    expect_true("row_names" %in% names(mtcars_out))
    expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
    expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
    expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
  },
  #
  #'
  read_table_check_names = function(ctx, con, table_name) {
    #' If the database supports identifiers with special characters,
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    #' the columns in the returned data frame are converted to valid R
    #' identifiers
    test_in <- data.frame(a = 1:3, b = 4:6)
    names(test_in) <- c("with spaces", "with,comma")
    dbWriteTable(con, table_name, test_in)
    #' if the `check.names` argument is `TRUE`,
    test_out <- check_df(dbReadTable(con, table_name, check.names = TRUE))

    expect_identical(names(test_out), make.names(names(test_out), unique = TRUE))
    expect_equal_df(test_out, setNames(test_in, names(test_out)))
  },
  #
  read_table_check_names_false = function(ctx, con, table_name) {
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    #' If `check.names = FALSE`, the returned table has non-syntactic column names without quotes.
    test_in <- data.frame(a = 1:3, b = 4:6)
    names(test_in) <- c("with spaces", "with,comma")
    dbWriteTable(con, table_name, test_in)
    test_out <- check_df(dbReadTable(con, table_name, check.names = FALSE))

    expect_equal_df(test_out, test_in)
  },

  #'
  read_table_closed_connection = function(ctx, con, table_name) {
    #' @section Failure modes:
    #' An error is raised when calling this method for a closed
    dbWriteTable(con, table_name, data.frame(a = 1))
    con2 <- local_closed_connection(ctx = ctx)
    expect_error(dbReadTable(con2, table_name))
  },

  read_table_invalid_connection = function(ctx, con, table_name) {
    #' or invalid connection.
    dbWriteTable(con, table_name, data.frame(a = 1))
    con2 <- local_invalid_connection(ctx)
    expect_error(dbReadTable(con2, table_name))
  },

  read_table_error = function(ctx, con, table_name) {
    #' An error is raised
    dbWriteTable(con, table_name, data.frame(a = 1L))
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbReadTable(con, NA))
    #' or if this results in a non-scalar.
    expect_error(dbReadTable(con, c(table_name, table_name)))

    #' Unsupported values for `row.names` and `check.names`
    #' (non-scalars,
    expect_error(dbReadTable(con, table_name, row.names = letters))
    #' unsupported data types,
    expect_error(dbReadTable(con, table_name, row.names = list(1L)))
    expect_error(dbReadTable(con, table_name, check.names = 1L))
    #' `NA` for `check.names`)
    expect_error(dbReadTable(con, table_name, check.names = NA))
    #' also raise an error.
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbReadTable()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `row.names` (default: `FALSE`)
  #' - `check.names`
  #'
  #' They must be provided as named arguments.
  #' See the "Value" section for details on their usage.

  read_table_name = function(ctx, con) {
    #' @section Specification:
    #' The `name` argument is processed as follows,
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      local_remove_test_table(con, table_name)
      test_in <- data.frame(a = 1L)
      dbWriteTable(con, table_name, test_in)

      #' - If an unquoted table name as string: `dbReadTable()` will do the
      #'   quoting,
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_df(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
      #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in)
    }
  },
  #
  NULL
)
