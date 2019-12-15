#' spec_sql_list_fields
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_list_fields <- list(
  list_fields_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListFields)), c("conn", "name", "..."))
  },

  #' @return
  #' `dbListFields()`
  list_fields = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        fields <- dbListFields(con, "iris")
        #' returns a character vector
        expect_is(fields, "character")
        #' that enumerates all fields
        #' in the table in the correct order.
        expect_identical(fields, names(iris))
      })

      with_remove_test_table({
        #' This also works for temporary tables if supported by the database.
        if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
          dbWriteTable(con, "test", data.frame(a = 1L, b = 2L), temporary = TRUE)
          fields <- dbListFields(con, "test")
          expect_equal(fields, c("a", "b"))

          #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
          expect_equal(dbQuoteIdentifier(con, fields), dbQuoteIdentifier(con, c("a", "b")))
        }
      })
    })
  },

  #' If the table does not exist, an error is raised.
  list_fields_wrong_table = function(ctx) {
    with_connection({
      name <- "missing"

      expect_false(dbExistsTable(con, name))
      expect_error(dbListFields(con, name))
    })
  },

  #' Invalid types for the `name` argument
  list_fields_invalid_type = function(ctx) {
    with_connection({
      #' (e.g., `character` of length not equal to one,
      expect_error(dbListFields(con, character()))
      expect_error(dbListFields(con, letters))
      #' or numeric)
      expect_error(dbListFields(con, 1))
      #' lead to an error.
    })
  },

  #' An error is also raised when calling this method for a closed
  list_fields_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbListFields(con, "test"))
    })
  },

  #' or invalid connection.
  list_fields_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbListFields(con, "test"))
    })
  },

  #' @section Specification:
  #'
  #' The `name` argument can be
  #'
  #' - a string
  #' - the return value of [dbQuoteIdentifier()]
  list_fields_quoted = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L, b = 2L))
        expect_identical(
          dbListFields(con, dbQuoteIdentifier(con, "test")),
          c("a", "b")
        )
      })
    })
  },

  #' - a value from the `table` column from the return value of
  #'   [dbListObjects()] where `is_prefix` is `FALSE`
  list_fields_object = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L, b = 2L))
        objects <- dbListObjects(con)
        expect_gt(nrow(objects), 0)
        expect_false(all(objects$is_prefix))
        expect_identical(
          dbListFields(con, objects$table[[1]]),
          dbListFields(con, dbQuoteIdentifier(con, objects$table[[1]]))
        )
      })
    })
  },

  #'
  #' A column named `row_names` is treated like any other column.
  list_fields_row_names = function(ctx) {
    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 1L, row_names = 2L))
        expect_identical(dbListFields(con, "test"), c("a", "row_names"))
      })
    })
  },

  NULL
)
