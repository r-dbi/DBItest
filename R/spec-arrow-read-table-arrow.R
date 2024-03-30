#' spec_arrow_read_table_arrow
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_read_table_arrow <- list(
  arrow_read_table_arrow_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbReadTableArrow)), c("conn", "name", "..."))
  },

  arrow_read_table_arrow = function(ctx, con, table_name) {
    # Failed on duckdb
    skip_if_not_dbitest(ctx, "1.7.99.2")

    #' @return
    #' `dbReadTableArrow()` returns an Arrow object that contains the complete data
    #' from the remote table, effectively the result of calling [dbGetQueryArrow()] with
    #' `SELECT * FROM <name>`.
    penguins_in <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins_in)
    penguins_out <- check_arrow(dbReadTableArrow(con, table_name))

    expect_equal_df(penguins_out, penguins_in)
  },

  #'
  arrow_read_table_arrow_missing = function(con, table_name) {
    #' @section Failure modes:
    #' An error is raised if the table does not exist.
    expect_error(dbReadTableArrow(con, table_name))
  },

  arrow_read_table_arrow_empty = function(ctx, con, table_name) {
    skip_if_not_dbitest(ctx, "1.8.0.14")

    #' @return
    #' An empty table is returned as an Arrow object with zero rows.
    penguins_in <- get_penguins(ctx)[integer(), ]
    dbWriteTable(con, table_name, penguins_in)
    penguins_out <- check_arrow(dbReadTableArrow(con, table_name))

    expect_equal(nrow(penguins_out), 0L)
    expect_equal_df(penguins_out, penguins_in)
  },

  #'
  arrow_read_table_arrow_closed_connection = function(ctx, con, table_name) {
    #' @section Failure modes:
    #' An error is raised when calling this method for a closed
    dbWriteTable(con, table_name, data.frame(a = 1.5))
    con2 <- local_closed_connection(ctx = ctx)
    expect_error(dbReadTableArrow(con2, table_name))
  },

  arrow_read_table_arrow_invalid_connection = function(ctx, con, table_name) {
    #' or invalid connection.
    dbWriteTable(con, table_name, data.frame(a = 1.5))
    con2 <- local_invalid_connection(ctx)
    expect_error(dbReadTableArrow(con2, table_name))
  },

  arrow_read_table_arrow_error = function(ctx, con, table_name) {
    #' An error is raised
    dbWriteTable(con, table_name, data.frame(a = 1.5))
    #' if `name` cannot be processed with [dbQuoteIdentifier()] or
    expect_error(dbReadTableArrow(con, NA))
    #' if this results in a non-scalar.
    expect_error(dbReadTableArrow(con, c(table_name, table_name)))
  },

  arrow_read_table_arrow_name = function(ctx, con) {
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
      test_in <- data.frame(a = 1.5)
      dbWriteTable(con, table_name, test_in)

      #' - If an unquoted table name as string: `dbReadTableArrow()` will do the
      #'   quoting,
      test_out <- check_arrow(dbReadTableArrow(con, table_name))
      expect_equal_df(test_out, test_in)
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
      #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
      test_out <- check_arrow(dbReadTableArrow(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in)
    }
  },
  #
  NULL
)
