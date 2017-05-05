#' spec_sql_exists_table
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_exists_table <- list(
  exists_table_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExistsTable)), c("conn", "name", "..."))
  },

  #' @return
  #' `dbExistsTable()` returns a logical scalar, `TRUE` if the table or view
  #' specified by the `name` argument exists, `FALSE` otherwise.
  exists_table = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        expect_false(expect_visible(dbExistsTable(con, "iris")))
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        expect_true(expect_visible(dbExistsTable(con, "iris")))

        expect_false(expect_visible(dbExistsTable(con, "test")))

        #' This includes temporary tables if supported by the database.
        if (isTRUE(ctx$tweaks$temporary_tables)) {
          dbWriteTable(con, "test", data.frame(a = 1L), temporary = TRUE)
          expect_true(expect_visible(dbExistsTable(con, "test")))
        }
      })

      expect_false(expect_visible(dbExistsTable(con, "iris")))
    })
  },

  #'
  #' An error is raised when calling this method for a closed
  exists_table_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbExistsTable(con, "test"))
    })
  },

  #' or invalid connection.
  exists_table_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbExistsTable(con, "test"))
    })
  },

  #' An error is also raised
  exists_table_error = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L))
        #' if `name` cannot be processed with [dbQuoteIdentifier()]
        expect_error(dbExistsTable(con, NA))
        #' or if this results in a non-scalar.
        expect_error(dbExistsTable(con, c("test", "test")))
      })
    })
  },

  #' @section Additional arguments:
  #' TBD: `temporary = NA`
  #'
  #' This must be provided as named argument.
  #' See the "Specification" section for details on their usage.

  #' @section Specification:
  #' The `name` argument is processed as follows,
  exists_table_name = function(ctx) {
    with_connection({
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
    })
  },

  #'
  #' For all tables listed by [dbListTables()], `dbExistsTable()` returns `TRUE`.
  exists_table_list = function(ctx) {
    with_connection({
      name <- random_table_name()
      with_remove_test_table(
        name = name,
        {
          dbWriteTable(con, name, data.frame(a = 1))
          for (table_name in dbListTables(con)) {
            expect_true(dbExistsTable(con, table_name))
          }
        }
      )
    })
  },

  NULL
)
