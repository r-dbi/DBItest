#' spec_sql_exists_table
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_exists_table <- list(
  exists_table_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExistsTable)), c("conn", "name", "..."))
  },

  #' @return
  #' `dbExistsTable()` returns a logical scalar, `TRUE` if the table or view
  #' specified by the `name` argument exists, `FALSE` otherwise.
  exists_table = function(ctx, con, table_name = "dbit05") {
    expect_false(expect_visible(dbExistsTable(con, table_name)))
    iris <- get_iris(ctx)
    dbWriteTable(con, table_name, iris)

    expect_true(expect_visible(dbExistsTable(con, table_name)))
  },
  # second stage
  exists_table = function(ctx, con) {
    table_name <- "dbit05"
    expect_false(expect_visible(dbExistsTable(con, table_name)))
  },

  #' 
  #' This includes temporary tables if supported by the database.
  exists_table_temporary = function(ctx, con, table_name) {
    expect_false(expect_visible(dbExistsTable(con, table_name)))

    if (isTRUE(ctx$tweaks$temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)
      expect_true(expect_visible(dbExistsTable(con, table_name)))
    }
  },

  #'
  #' An error is raised when calling this method for a closed
  exists_table_closed_connection = function(ctx, closed_con) {
    expect_error(dbExistsTable(closed_con, "test"))
  },

  #' or invalid connection.
  exists_table_invalid_connection = function(ctx, invalid_con) {
    expect_error(dbExistsTable(invalid_con, "test"))
  },

  #' An error is also raised
  exists_table_error = function(con, table_name) {
    dbWriteTable(con, table_name, data.frame(a = 1L))
    #' if `name` cannot be processed with [dbQuoteIdentifier()]
    expect_error(dbExistsTable(con, NA))
    #' or if this results in a non-scalar.
    expect_error(dbExistsTable(con, c(table_name, table_name)))
  },

  #' @section Specification:
  #' The `name` argument is processed as follows,
  exists_table_name = function(ctx, con) {
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      with_remove_test_table(name = table_name, {
        expect_false(dbExistsTable(con, table_name))

        test_in <- data.frame(a = 1L)
        dbWriteTable(con, table_name, test_in)

        #' - If an unquoted table name as string: `dbExistsTable()` will do the
        #'   quoting,
        expect_true(dbExistsTable(con, table_name))
        #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
        #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
        expect_true(dbExistsTable(con, dbQuoteIdentifier(con, table_name)))
      })
    }
  },

  #'
  #' For all tables listed by [dbListTables()], `dbExistsTable()` returns `TRUE`.
  exists_table_list = function(con, table_name) {
    dbWriteTable(con, table_name, data.frame(a = 1))
    for (table_name in dbListTables(con)) {
      eval(bquote(expect_true(dbExistsTable(con, .(table_name)))))
    }
  },
  #
  NULL
)
