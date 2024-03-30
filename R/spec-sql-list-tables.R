#' spec_sql_list_tables
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_list_tables <- list(
  list_tables_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListTables)), c("conn", "..."))
  },

  list_tables_1 = function(ctx, con, table_name = "dbit07") {
    #' @return
    #' `dbListTables()`
    tables <- dbListTables(con)
    #' returns a character vector
    expect_type(tables, "character")
    #' that enumerates all tables
    expect_false(table_name %in% tables)

    #' and views
    # TODO
    #' in the database.

    #' Tables added with [dbWriteTable()] are
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    #' part of the list.
    tables <- dbListTables(con)
    expect_true(table_name %in% tables)
  },
  # second stage
  list_tables_2 = function(ctx, con) {
    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit07"

    #' As soon a table is removed from the database,
    #' it is also removed from the list of database tables.
    tables <- dbListTables(con)
    expect_false(table_name %in% tables)
  },
  #'
  list_tables_temporary = function(ctx, con, table_name) {
    #' The same applies to temporary tables if supported by the database.
    if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)
      tables <- dbListTables(con)
      expect_true(table_name %in% tables)
    }
  },

  #'
  list_tables_quote = function(ctx, con) {
    #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      local_remove_test_table(con, table_name)
      dbWriteTable(con, dbQuoteIdentifier(con, table_name), data.frame(a = 2L))
      tables <- dbListTables(con)
      expect_true(table_name %in% tables)
      expect_true(dbQuoteIdentifier(con, table_name) %in% dbQuoteIdentifier(con, tables))
    }
  },

  #'
  list_tables_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when calling this method for a closed
    expect_error(dbListTables(closed_con))
  },

  list_tables_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbListTables(invalid_con))
  },
  #
  NULL
)
