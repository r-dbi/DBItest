#' spec_sql_remove_table
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_remove_table <- list(
  remove_table_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbRemoveTable)), c("conn", "name", "..."))
  },

  #' @return
  #' `dbRemoveTable()` returns `TRUE`, invisibly.
  remove_table_return = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        expect_invisible_true(dbRemoveTable(con, "iris"))
      })
    })
  },

  #' If the table does not exist, an error is raised.
  remove_table_missing = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_error(dbRemoveTable(con, "test"))
      })
    })
  },

  #' An attempt to remove a view with this function may result in an error.
  #'
  #'
  #' An error is raised when calling this method for a closed
  remove_table_closed_connection = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1))
        with_closed_connection(con = "con2", {
          expect_error(dbRemoveTable(con2, "test"))
        })
      })
    })
  },

  #' or invalid connection.
  remove_table_invalid_connection = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1))
        with_invalid_connection(con = "con2", {
          expect_error(dbRemoveTable(con2, "test"))
        })
      })
    })
  },

  #' An error is also raised
  remove_table_error = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L))
        #' if `name` cannot be processed with [dbQuoteIdentifier()]
        expect_error(dbRemoveTable(con, NA))
        #' or if this results in a non-scalar.
        expect_error(dbRemoveTable(con, c("test", "test")))
      })
    })
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbRemoveTable()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `temporary` (default: `FALSE`)
  #' - `fail_if_missing` (default: `TRUE`)
  #'
  #' These arguments must be provided as named arguments.


  #'
  #' If `temporary` is `TRUE`, the call to `dbRemoveTable()`
  #' will consider only temporary tables.
  remove_table_temporary_arg = function(ctx) {
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1.5))
        expect_equal(dbReadTable(con, "test"), data.frame(a = 1.5))
        dbCreateTable(con, "test", data.frame(b = 2.5), temporary = TRUE)
        dbRemoveTable(con, "test", temporary = TRUE)
        #' In particular, permanent tables of the same name are left untouched.
        expect_error(dbRemoveTable(con, "test", temporary = TRUE))
        expect_equal(dbReadTable(con, "test"), data.frame(a = 1.5))
      })
    })
  },

  #'
  #' If `fail_if_missing` is `FALSE`, the call to `dbRemoveTable()`
  #' succeeds if the table does not exist.
  remove_table_missing_succeed = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_error(dbRemoveTable(con, "test", fail_if_missing = FALSE), NA)
      })
    })
  },

  #' @section Specification:
  #' A table removed by `dbRemoveTable()` doesn't appear in the list of tables
  #' returned by [dbListTables()],
  #' and [dbExistsTable()] returns `FALSE`.
  remove_table_list = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L))
        expect_true("test" %in% dbListTables(con))
        expect_true(dbExistsTable(con, "test"))

        dbRemoveTable(con, "test")
        expect_false("test" %in% dbListTables(con))
        expect_false(dbExistsTable(con, "test"))
      })
    })
  },

  #' The removal propagates immediately to other connections to the same database.
  remove_table_other_con = function(ctx) {
    with_connection({
      with_connection(con = "con2", {
        with_remove_test_table({
          dbWriteTable(con, "test", data.frame(a = 1L))
          expect_true("test" %in% dbListTables(con2))
          expect_true(dbExistsTable(con2, "test"))

          dbRemoveTable(con, "test")
          expect_false("test" %in% dbListTables(con2))
          expect_false(dbExistsTable(con2, "test"))
        })
      })
    })
  },

  #' This function can also be used to remove a temporary table.
  remove_table_temporary = function(ctx) {
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L), temporary = TRUE)
        if (isTRUE(ctx$tweaks$list_temporary_tables)) {
          expect_true("test" %in% dbListTables(con))
        }
        expect_true(dbExistsTable(con, "test"))

        dbRemoveTable(con, "test")
        if (isTRUE(ctx$tweaks$list_temporary_tables)) {
          expect_false("test" %in% dbListTables(con))
        }
        expect_false(dbExistsTable(con, "test"))
      })
    })
  },

  #'
  #' The `name` argument is processed as follows,
  remove_table_name = function(ctx) {
    with_connection({
      #' to support databases that allow non-syntactic names for their objects:
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- "a"
      } else {
        table_names <- c("a", "with spaces", "with,comma")
      }

      test_in <- data.frame(a = 1L)

      for (table_name in table_names) {
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If an unquoted table name as string: `dbRemoveTable()` will do the
          #'   quoting,
          dbWriteTable(con, table_name, test_in)
          expect_true(dbRemoveTable(con, table_name))
          #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
        })
      }

      for (table_name in table_names) {
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
          dbWriteTable(con, table_name, test_in)
          expect_true(dbRemoveTable(con, dbQuoteIdentifier(con, table_name)))
        })
      }
    })
  },

  NULL
)
