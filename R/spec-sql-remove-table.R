#' spec_sql_remove_table
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_remove_table <- list(
  remove_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbRemoveTable)), c("conn", "name", "..."))
  },

  remove_table_return = function(ctx, con, table_name) {
    #' @return
    #' `dbRemoveTable()` returns `TRUE`, invisibly.
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    expect_invisible_true(dbRemoveTable(con, table_name))
  },

  #'
  remove_table_missing = function(con, table_name) {
    #' @section Failure modes:
    #' If the table does not exist, an error is raised.
    expect_error(dbRemoveTable(con, table_name))
  },

  remove_table_closed_connection = function(ctx, con, table_name) {
    #' An attempt to remove a view with this function may result in an error.
    #'
    #'
    #' An error is raised when calling this method for a closed
    dbWriteTable(con, table_name, data.frame(a = 1))
    con2 <- local_closed_connection(ctx = ctx)
    expect_error(dbRemoveTable(con2, table_name))
  },

  remove_table_invalid_connection = function(ctx, con, table_name) {
    #' or invalid connection.
    dbWriteTable(con, table_name, data.frame(a = 1))
    con2 <- local_invalid_connection(ctx)
    expect_error(dbRemoveTable(con2, table_name))
  },

  remove_table_error = function(con, table_name) {
    #' An error is also raised
    dbWriteTable(con, table_name, data.frame(a = 1L))
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbRemoveTable(con, NA))
    #' or if this results in a non-scalar.
    expect_error(dbRemoveTable(con, c(table_name, table_name)))
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
  remove_table_temporary_arg = function(ctx, con, table_name) {
    #' If `temporary` is `TRUE`, the call to `dbRemoveTable()`
    #' will consider only temporary tables.
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    dbWriteTable(con, table_name, data.frame(a = 1.5))
    expect_equal(dbReadTable(con, table_name), data.frame(a = 1.5))
    dbCreateTable(con, table_name, data.frame(b = 2.5), temporary = TRUE)
    dbRemoveTable(con, table_name, temporary = TRUE)
    #' In particular, permanent tables of the same name are left untouched.
    expect_error(dbRemoveTable(con, table_name, temporary = TRUE))
    expect_equal(dbReadTable(con, table_name), data.frame(a = 1.5))
  },

  #'
  remove_table_missing_succeed = function(con, table_name) {
    #' If `fail_if_missing` is `FALSE`, the call to `dbRemoveTable()`
    #' succeeds if the table does not exist.
    expect_error(dbRemoveTable(con, table_name, fail_if_missing = FALSE), NA)
  },

  remove_table_list = function(con, table_name) {
    #' @section Specification:
    #' A table removed by `dbRemoveTable()` doesn't appear in the list of tables
    #' returned by [dbListTables()],
    #' and [dbExistsTable()] returns `FALSE`.
    dbWriteTable(con, table_name, data.frame(a = 1L))
    expect_true(table_name %in% dbListTables(con))
    expect_true(dbExistsTable(con, table_name))

    dbRemoveTable(con, table_name)
    expect_false(table_name %in% dbListTables(con))
    expect_false(dbExistsTable(con, table_name))
  },

  remove_table_other_con = function(ctx, con, table_name) {
    #' The removal propagates immediately to other connections to the same database.
    con2 <- local_connection(ctx)
    dbWriteTable(con, table_name, data.frame(a = 1L))
    expect_true(table_name %in% dbListTables(con2))
    expect_true(dbExistsTable(con2, table_name))

    dbRemoveTable(con, table_name)
    expect_false(table_name %in% dbListTables(con2))
    expect_false(dbExistsTable(con2, table_name))
  },

  remove_table_temporary = function(ctx, con, table_name) {
    #' This function can also be used to remove a temporary table.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)
    if (isTRUE(ctx$tweaks$list_temporary_tables)) {
      expect_true(table_name %in% dbListTables(con))
    }
    expect_true(dbExistsTable(con, table_name))

    dbRemoveTable(con, table_name)
    if (isTRUE(ctx$tweaks$list_temporary_tables)) {
      expect_false(table_name %in% dbListTables(con))
    }
    expect_false(dbExistsTable(con, table_name))
  },

  #'
  remove_table_name = function(ctx, con) {
    #' The `name` argument is processed as follows,
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    test_in <- data.frame(a = 1L)

    for (table_name in table_names) {
      local_remove_test_table(con, table_name)
      #' - If an unquoted table name as string: `dbRemoveTable()` will do the
      #'   quoting,
      dbWriteTable(con, table_name, test_in)
      expect_true(dbRemoveTable(con, table_name))
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  remove_table_name_quoted = function(ctx, con) {
    #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
    skip_if_not_dbitest(ctx, "1.7.2")

    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    test_in <- data.frame(a = 1L)

    for (table_name in table_names) {
      local_remove_test_table(con, table_name)
      dbWriteTable(con, table_name, test_in)
      expect_true(dbRemoveTable(con, dbQuoteIdentifier(con, table_name)))
    }
  },
  #
  NULL
)
