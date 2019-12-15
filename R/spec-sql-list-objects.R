#' spec_sql_list_objects
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_sql_list_objects <- list(
  list_objects_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListObjects)), c("conn", "prefix", "..."))
  },

  #' @return
  #' `dbListObjects()`
  list_objects = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        objects <- dbListObjects(con)
        #' returns a data frame
        expect_is(objects, "data.frame")
        #' with columns
        cols <- c("table", "is_prefix")
        #' `table` and `is_prefix` (in that order),
        expect_equal(names(objects)[seq_along(cols)], cols)
        #' optionally with other columns with a dot (`.`) prefix.
        expect_true(all(grepl("^[.]", names(objects)[-seq_along(cols)])))

        #' The `table` column is of type list.
        expect_equal(typeof(objects$table), "list")
        #' Each object in this list is suitable for use as argument in [dbQuoteIdentifier()].
        expect_error(lapply(objects$table, dbQuoteIdentifier, conn = con), NA)

        #' The `is_prefix` column is a logical.
        expect_is(objects$is_prefix, "logical")

        #' This data frame contains one row for each object (schema, table
        expect_false("iris" %in% objects)
        #' and view)
        # TODO
        #' accessible from the prefix (if passed) or from the global namespace
        #' (if prefix is omitted).

        #' Tables added with [dbWriteTable()]
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        #' are part of the data frame,
        objects <- dbListObjects(con)
        quoted_tables <- vapply(objects$table, dbQuoteIdentifier, conn = con, character(1))
        expect_true(dbQuoteIdentifier(con, "iris") %in% quoted_tables)
      })

      with_remove_test_table({
        #' including temporary objects if supported by the database.
        if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
          dbWriteTable(con, "test", data.frame(a = 1L), temporary = TRUE)

          objects <- dbListObjects(con)
          quoted_tables <- vapply(objects$table, dbQuoteIdentifier, conn = con, character(1))
          expect_true(dbQuoteIdentifier(con, "test") %in% quoted_tables)
        }
      })

      #' As soon a table is removed from the database,
      #' it is also removed from the data frame of database objects.
      objects <- dbListObjects(con)
      quoted_tables <- vapply(objects$table, dbQuoteIdentifier, conn = con, character(1))
      expect_false(dbQuoteIdentifier(con, "iris") %in% quoted_tables)

      #'
      #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
      if (isTRUE(ctx$tweaks$strict_identifier)) {
        table_names <- "a"
      } else {
        table_names <- c("a", "with spaces", "with,comma")
      }

      for (table_name in table_names) {
        with_remove_test_table(name = dbQuoteIdentifier(con, table_name), {
          dbWriteTable(con, dbQuoteIdentifier(con, table_name), data.frame(a = 2L))
          objects <- dbListObjects(con)
          quoted_tables <- vapply(objects$table, dbQuoteIdentifier, conn = con, character(1))
          expect_true(dbQuoteIdentifier(con, table_name) %in% quoted_tables)
        })
      }
    })
  },

  #' An error is raised when calling this method for a closed
  list_objects_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbListObjects(con))
    })
  },

  #' or invalid connection.
  list_objects_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbListObjects(con))
    })
  },

  #' @section Specification:
  list_objects_features = function(ctx) {
    with_connection({
      objects <- dbListObjects(con)

      #' The `table` object can be quoted with [dbQuoteIdentifier()].
      sql <- lapply(objects$table, dbQuoteIdentifier, conn = con)
      #' The result of quoting can be passed to [dbUnquoteIdentifier()].
      unquoted <- vapply(sql, dbUnquoteIdentifier, conn = con, list(1))
      #' The unquoted results are equal to the original `table` object.
      expect_equal(unquoted, unclass(objects$table))
      #' (For backends it may be convenient to use the [Id] class, but this is
      #' not required.)

      #'
      #' The `prefix` column indicates if the `table` value refers to a table
      #' or a prefix.
      #' For a call with the default `prefix = NULL`, the `table`
      #' values that have `is_prefix == FALSE` correspond to the tables
      #' returned from [dbListTables()],
      non_prefix_objects <- vapply(
        objects$table[!objects$is_prefix],
        dbQuoteIdentifier, conn = con,
        character(1)
      )
      all_tables <- dbQuoteIdentifier(con, dbListTables(con))
      expect_equal(sort(non_prefix_objects), sort(as.character(all_tables)))

      if (!any(objects$is_prefix)) {
        skip("No schemas available")
      }

      #'
      #' Values in `table` column that have `is_prefix == TRUE` can be
      #' passed as the `prefix` argument to another call to `dbListObjects()`.
      #' For the data frame returned from a `dbListObject()` call with the
      #' `prefix` argument set, all `table` values where `is_prefix` is
      #' `FALSE` can be used in a call to [dbExistsTable()] which returns
      #' `TRUE`.
      for (schema in objects$table[objects$is_prefix]) {
        sub_objects <- dbListObjects(con, prefix = schema)
        for (sub_table in sub_objects$table[!sub_objects$is_prefix]) {
          # eval(bquote()) preserves the SQL class, even if it's not apparent
          # in the output
          eval(bquote(expect_true(dbExistsTable(con, .(sub_table)))))
        }
      }
    })
  },

  NULL
)
