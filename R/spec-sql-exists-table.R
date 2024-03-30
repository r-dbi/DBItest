#' spec_sql_exists_table
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_exists_table <- list(
  exists_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExistsTable)), c("conn", "name", "..."))
  },

  exists_table_1 = function(ctx, con, table_name = "dbit05") {
    #' @return
    #' `dbExistsTable()` returns a logical scalar, `TRUE` if the table or view
    #' specified by the `name` argument exists, `FALSE` otherwise.
    expect_false(expect_visible(dbExistsTable(con, table_name)))
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    expect_true(expect_visible(dbExistsTable(con, table_name)))
  },
  # second stage
  exists_table_2 = function(ctx, con) {
    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit05"
    expect_false(expect_visible(dbExistsTable(con, table_name)))
  },

  #'
  exists_table_temporary = function(ctx, con, table_name) {
    #' This includes temporary tables if supported by the database.
    expect_false(expect_visible(dbExistsTable(con, table_name)))

    if (isTRUE(ctx$tweaks$temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)
      expect_true(expect_visible(dbExistsTable(con, table_name)))
    }
  },

  #'
  exists_table_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when calling this method for a closed
    expect_error(dbExistsTable(closed_con, "test"))
  },

  exists_table_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbExistsTable(invalid_con, "test"))
  },

  exists_table_error = function(con, table_name) {
    #' An error is also raised
    dbWriteTable(con, table_name, data.frame(a = 1L))
    #' if `name` cannot be processed with [dbQuoteIdentifier()] or
    expect_error(dbExistsTable(con, NA))
    #' if this results in a non-scalar.
    expect_error(dbExistsTable(con, c(table_name, table_name)))
  },

  exists_table_name = function(ctx, con) {
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
      expect_false(dbExistsTable(con, table_name))

      test_in <- data.frame(a = 1L)
      dbWriteTable(con, table_name, test_in)

      #' - If an unquoted table name as string: `dbExistsTable()` will do the
      #'   quoting,
      expect_true(dbExistsTable(con, table_name))
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
      #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
      expect_true(dbExistsTable(con, dbQuoteIdentifier(con, table_name)))
    }
  },

  #'
  exists_table_list = function(con, table_name) {
    #' For all tables listed by [dbListTables()], `dbExistsTable()` returns `TRUE`.
    dbWriteTable(con, table_name, data.frame(a = 1))
    for (table_name in dbListTables(con)) {
      eval(bquote(expect_true(dbExistsTable(con, .(table_name)))))
    }
  },
  #
  NULL
)
