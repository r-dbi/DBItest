#' spec_sql_list_fields
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_list_fields <- list(
  list_fields_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListFields)), c("conn", "name", "..."))
  },

  list_fields = function(ctx, con, table_name) {
    #' @return
    #' `dbListFields()`
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    fields <- dbListFields(con, table_name)
    #' returns a character vector
    expect_type(fields, "character")
    #' that enumerates all fields
    #' in the table in the correct order.
    expect_identical(fields, names(penguins))
  },
  list_fields_temporary = function(ctx, con, table_name) {
    #' This also works for temporary tables if supported by the database.
    if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L, b = 2L), temporary = TRUE)
      fields <- dbListFields(con, table_name)
      expect_equal(fields, c("a", "b"))

      #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
      expect_equal(dbQuoteIdentifier(con, fields), dbQuoteIdentifier(con, c("a", "b")))
    }
  },

  #'
  list_fields_wrong_table = function(con) {
    #' @section Failure modes:
    #' If the table does not exist, an error is raised.
    name <- "missing"

    stopifnot(!dbExistsTable(con, name))
    expect_error(dbListFields(con, name))
  },

  list_fields_invalid_type = function(con) {
    #' Invalid types for the `name` argument
    #' (e.g., `character` of length not equal to one,
    expect_error(dbListFields(con, character()))
    expect_error(dbListFields(con, letters))
    #' or numeric)
    expect_error(dbListFields(con, 1))
    #' lead to an error.
  },

  list_fields_closed_connection = function(ctx, closed_con) {
    #' An error is also raised when calling this method for a closed
    expect_error(dbListFields(closed_con, "test"))
  },

  list_fields_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbListFields(invalid_con, "test"))
  },

  list_fields_quoted = function(con, table_name) {
    #' @section Specification:
    #'
    #' The `name` argument can be
    #'
    #' - a string
    #' - the return value of [dbQuoteIdentifier()]
    dbWriteTable(con, table_name, data.frame(a = 1L, b = 2L))
    expect_identical(
      dbListFields(con, dbQuoteIdentifier(con, table_name)),
      c("a", "b")
    )
  },

  list_fields_object = function(con, table_name) {
    #' - a value from the `table` column from the return value of
    #'   [dbListObjects()] where `is_prefix` is `FALSE`
    dbWriteTable(con, table_name, data.frame(a = 1L, b = 2L))
    objects <- dbListObjects(con)
    expect_gt(nrow(objects), 0)
    expect_false(all(objects$is_prefix))
    expect_identical(
      dbListFields(con, objects$table[[1]]),
      dbListFields(con, dbQuoteIdentifier(con, objects$table[[1]]))
    )
  },

  #'
  list_fields_row_names = function(con, table_name) {
    #' A column named `row_names` is treated like any other column.
    dbWriteTable(con, table_name, data.frame(a = 1L, row_names = 2L))
    expect_identical(dbListFields(con, table_name), c("a", "row_names"))
  },
  #
  NULL
)
