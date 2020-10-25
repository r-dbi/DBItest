#' spec_sql_list_tables
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_list_tables <- list(
  list_tables_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListTables)), c("conn", "..."))
  },

  #' @return
  #' `dbListTables()`
  list_tables = function(ctx, con, table_name = "dbit07") {
    tables <- dbListTables(con)
    #' returns a character vector
    expect_is(tables, "character")
    #' that enumerates all tables
    expect_false(table_name %in% tables)

    #' and views
    # TODO
    #' in the database.

    #' Tables added with [dbWriteTable()]
    iris <- get_iris(ctx)
    dbWriteTable(con, table_name, iris)

    #' are part of the list.
    tables <- dbListTables(con)
    expect_true(table_name %in% tables)
  },
  # second stage
  list_tables = function(ctx, con) {
    #' As soon a table is removed from the database,
    #' it is also removed from the list of database tables.
    table_name <- "dbit07"
    tables <- dbListTables(con)
    expect_false(table_name %in% tables)
  },
  #'
  #' The same applies to temporary tables if supported by the database.
  list_tables_temporary = function(ctx, con, table_name) {
    if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)
      tables <- dbListTables(con)
      expect_true(table_name %in% tables)
    }
  },

  #'
  #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
  list_tables_quote = function(ctx, con) {
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
        dbWriteTable(con, dbQuoteIdentifier(con, table_name), data.frame(a = 2L))
        tables <- dbListTables(con)
        expect_true(table_name %in% tables)
        expect_true(dbQuoteIdentifier(con, table_name) %in% dbQuoteIdentifier(con, tables))
      })
    }
  },

  #' An error is raised when calling this method for a closed
  list_tables_closed_connection = function(ctx, closed_con) {
    expect_error(dbListTables(closed_con))
  },

  #' or invalid connection.
  list_tables_invalid_connection = function(ctx, invalid_con) {
    expect_error(dbListTables(invalid_con))
  },
  #
  NULL
)
