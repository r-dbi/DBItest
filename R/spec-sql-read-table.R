#' spec_sql_read_table
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_read_table <- list(
  read_table_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbReadTable)), c("conn", "name", "..."))
  },

  #' @return
  #' `dbReadTable()` returns a data frame that contains the complete data
  #' from the remote table, effectively the result of calling [dbGetQuery()]
  #' with `SELECT * FROM <name>`.
  read_table = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in)
        iris_out <- check_df(dbReadTable(con, "iris"))

        expect_equal_df(iris_out, iris_in)
      })
    })
  },

  #' An error is raised if the table does not exist.
  read_table_missing = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_error(dbReadTable(con, "test"))
      })
    })
  },

  #' An empty table is returned as a data frame with zero rows.
  read_table_empty = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)[integer(), ]
        dbWriteTable(con, "iris", iris_in)
        iris_out <- check_df(dbReadTable(con, "iris"))

        expect_equal(nrow(iris_out), 0L)
        expect_equal_df(iris_out, iris_in)
      })
    })
  },

  #'
  #' The presence of [rownames] depends on the `row.names` argument,
  #' see [sqlColumnToRownames()] for details:
  read_table_row_names_false = function(ctx) {
    #' - If `FALSE` or `NULL`, the returned data frame doesn't have row names.
    for (row.names in list(FALSE, NULL)) {
      with_connection({
        with_remove_test_table(name = "mtcars", {
          mtcars_in <- datasets::mtcars
          dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
          mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = row.names))

          expect_true("row_names" %in% names(mtcars_out))
          expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
          expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
          expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
        })
      })
    }
  },

  read_table_row_names_true_exists = function(ctx) {
    #' - If `TRUE`, a column named "row_names" is converted to row names,
    row.names <- TRUE

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = NA)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = row.names))

        expect_equal_df(mtcars_out, mtcars_in)
      })
    })
  },

  read_table_row_names_true_missing = function(ctx) {
    #'   an error is raised if no such column exists.
    row.names <- TRUE

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = NA)
        expect_error(dbReadTable(con, "iris", row.names = row.names))
      })
    })
  },

  read_table_row_names_na_exists = function(ctx) {
    #' - If `NA`, a column named "row_names" is converted to row names if it exists,
    row.names <- NA

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = row.names))

        expect_equal_df(mtcars_out, mtcars_in)
      })
    })
  },

  read_table_row_names_na_missing = function(ctx) {
    #'   otherwise no translation occurs.
    row.names <- NA

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = FALSE)
        iris_out <- check_df(dbReadTable(con, "iris", row.names = row.names))

        expect_equal_df(iris_out, iris_in)
      })
    })
  },

  read_table_row_names_string_exists = function(ctx) {
    #' - If a string, this specifies the name of the column in the remote table
    #'   that contains the row names,
    row.names <- "make_model"

    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        mtcars_in$make_model <- rownames(mtcars_in)
        mtcars_in <- unrowname(mtcars_in)

        dbWriteTable(con, "mtcars", mtcars_in, row.names = FALSE)
        mtcars_out <- check_df(dbReadTable(con, "mtcars", row.names = row.names))

        expect_false("make_model" %in% names(mtcars_out))
        expect_true(all(mtcars_in$make_model %in% rownames(mtcars_out)))
        expect_true(all(rownames(mtcars_out) %in% mtcars_in$make_model))
        expect_equal_df(unrowname(mtcars_out), mtcars_in[names(mtcars_in) != "make_model"])
      })
    })
  },

  read_table_row_names_string_missing = function(ctx) {
    #'   an error is raised if no such column exists.
    row.names <- "missing"

    with_connection({
      with_remove_test_table(name = "iris", {
        iris_in <- get_iris(ctx)
        dbWriteTable(con, "iris", iris_in, row.names = FALSE)
        expect_error(dbReadTable(con, "iris", row.names = row.names))
      })
    })
  },
  #'

  read_table_row_names_default = function(ctx) {
    #'
    #' The default is `row.names = FALSE`.
    #'
    with_connection({
      with_remove_test_table(name = "mtcars", {
        mtcars_in <- datasets::mtcars
        dbWriteTable(con, "mtcars", mtcars_in, row.names = TRUE)
        mtcars_out <- check_df(dbReadTable(con, "mtcars"))

        expect_true("row_names" %in% names(mtcars_out))
        expect_true(all(mtcars_out$row_names %in% rownames(mtcars_in)))
        expect_true(all(rownames(mtcars_in) %in% mtcars_out$row_names))
        expect_equal_df(mtcars_out[names(mtcars_out) != "row_names"], unrowname(mtcars_in))
      })
    })
  },

  read_table_check_names = function(ctx) {
    with_connection({
      #' If the database supports identifiers with special characters,
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        skip("tweak: strict_identifier")
      }

      #' the columns in the returned data frame are converted to valid R
      #' identifiers
      with_remove_test_table({
        test_in <- data.frame(a = 1:3, b = 4:6)
        names(test_in) <- c("with spaces", "with,comma")
        dbWriteTable(con, "test", test_in)
        #' if the `check.names` argument is `TRUE`,
        test_out <- check_df(dbReadTable(con, "test", check.names = TRUE))

        expect_identical(names(test_out), make.names(names(test_out), unique = TRUE))
        expect_equal_df(test_out, setNames(test_in, names(test_out)))
      })

      #' otherwise non-syntactic column names can be returned unquoted.
      with_remove_test_table({
        test_in <- data.frame(a = 1:3, b = 4:6)
        names(test_in) <- c("with spaces", "with,comma")
        dbWriteTable(con, "test", test_in)
        test_out <- check_df(dbReadTable(con, "test", check.names = FALSE))

        expect_equal_df(test_out, test_in)
      })
    })
  },

  #'
  #' An error is raised when calling this method for a closed
  read_table_closed_connection = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1))
        with_closed_connection(con = "con2", {
          expect_error(dbReadTable(con2, "test"))
        })
      })
    })
  },

  #' or invalid connection.
  read_table_invalid_connection = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1))
        with_invalid_connection(con = "con2", {
          expect_error(dbReadTable(con2, "test"))
        })
      })
    })
  },

  #' An error is raised
  read_table_error = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L))
        #' if `name` cannot be processed with [dbQuoteIdentifier()]
        expect_error(dbReadTable(con, NA))
        #' or if this results in a non-scalar.
        expect_error(dbReadTable(con, c("test", "test")))

        #' Unsupported values for `row.names` and `check.names`
        #' (non-scalars,
        expect_error(dbReadTable(con, "test", row.names = letters))
        #' unsupported data types,
        expect_error(dbReadTable(con, "test", row.names = list(1L)))
        expect_error(dbReadTable(con, "test", check.names = 1L))
        #' `NA` for `check.names`)
        expect_error(dbReadTable(con, "test", check.names = NA))
        #' also raise an error.
      })
    })
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

  #' @section Specification:
  #' The `name` argument is processed as follows,
  read_table_name = function(ctx) {
    with_connection({
      #' to support databases that allow non-syntactic names for their objects:
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- "a"
      } else {
        table_names <- c("a", "with spaces", "with,comma")
      }

      for (table_name in table_names) {
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
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
        })
      }
    })
  },

  NULL
)
