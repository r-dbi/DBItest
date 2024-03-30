#' spec_sql_list_objects
#' @family sql specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_sql_list_objects <- list(
  list_objects_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbListObjects)), c("conn", "prefix", "..."))
  },

  list_objects_1 = function(ctx, con, table_name = "dbit06") {
    #' @return
    #' `dbListObjects()`
    objects <- dbListObjects(con)
    #' returns a data frame
    expect_s3_class(objects, "data.frame")
    #' with columns
    cols <- c("table", "is_prefix")
    #' `table` and `is_prefix` (in that order),
    expect_equal(names(objects)[seq_along(cols)], cols)
    #' optionally with other columns with a dot (`.`) prefix.
    expect_true(all(grepl("^[.]", names(objects)[-seq_along(cols)])))

    #' The `table` column is of type list.
    expect_equal(typeof(objects$table), "list")
    #' Each object in this list is suitable for use as argument in [dbQuoteIdentifier()].
    expect_error(map(objects$table, dbQuoteIdentifier, conn = con), NA)

    #' The `is_prefix` column is a logical.
    expect_type(objects$is_prefix, "logical")

    #' This data frame contains one row for each object (schema, table
    expect_false(table_name %in% objects)
    #' and view)
    # TODO
    #' accessible from the prefix (if passed) or from the global namespace
    #' (if prefix is omitted).

    #' Tables added with [dbWriteTable()] are
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    #' part of the data frame.
    objects <- dbListObjects(con)
    quoted_tables <- map_chr(objects$table, dbQuoteIdentifier, conn = con)
    expect_true(dbQuoteIdentifier(con, table_name) %in% quoted_tables)
  },
  # second stage
  list_objects_2 = function(ctx, con) {
    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit06"

    #' As soon a table is removed from the database,
    #' it is also removed from the data frame of database objects.
    objects <- dbListObjects(con)
    quoted_tables <- map_chr(objects$table, dbQuoteIdentifier, conn = con)
    expect_false(dbQuoteIdentifier(con, table_name) %in% quoted_tables)
  },

  #'
  list_objects_temporary = function(ctx, con, table_name) {
    #' The same applies to temporary objects if supported by the database.
    if (isTRUE(ctx$tweaks$temporary_tables) && isTRUE(ctx$tweaks$list_temporary_tables)) {
      dbWriteTable(con, table_name, data.frame(a = 1L), temporary = TRUE)

      objects <- dbListObjects(con)
      quoted_tables <- map_chr(objects$table, dbQuoteIdentifier, conn = con)
      expect_true(dbQuoteIdentifier(con, table_name) %in% quoted_tables)
    }
  },

  #'
  list_objects_quote = function(ctx, con) {
    #' The returned names are suitable for quoting with `dbQuoteIdentifier()`.
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      local_remove_test_table(con, table_name)
      dbWriteTable(con, dbQuoteIdentifier(con, table_name), data.frame(a = 2L))
      objects <- dbListObjects(con)
      quoted_tables <- map_chr(objects$table, dbQuoteIdentifier, conn = con)
      expect_true(dbQuoteIdentifier(con, table_name) %in% quoted_tables)
    }
  },

  #'
  list_objects_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when calling this method for a closed
    expect_error(dbListObjects(closed_con))
  },

  list_objects_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbListObjects(invalid_con))
  },

  list_objects_features = function(ctx, con) {
    #' @section Specification:
    objects <- dbListObjects(con)

    #' The `prefix` column indicates if the `table` value refers to a table
    #' or a prefix.
    #' For a call with the default `prefix = NULL`, the `table`
    #' values that have `is_prefix == FALSE` correspond to the tables
    #' returned from [dbListTables()],
    non_prefix_objects <- map_chr(
      objects$table[!objects$is_prefix],
      dbQuoteIdentifier,
      conn = con
    )
    all_tables <- dbQuoteIdentifier(con, dbListTables(con))
    expect_equal(sort(non_prefix_objects), sort(as.character(all_tables)))

    #'
    #' The `table` object can be quoted with [dbQuoteIdentifier()].
    sql <- map(objects$table[!objects$is_prefix], dbQuoteIdentifier, conn = con)
    #' The result of quoting can be passed to [dbUnquoteIdentifier()].
    expect_error(walk(sql, dbUnquoteIdentifier, conn = con), NA)
    #' (For backends it may be convenient to use the [Id] class, but this is
    #' not required.)

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
    for (schema in utils::head(objects$table[objects$is_prefix])) {
      sub_objects <- dbListObjects(con, prefix = schema)
      for (sub_table in utils::head(sub_objects$table[!sub_objects$is_prefix])) {
        # HACK HACK HACK for RMariaDB on OS X (#188)
        if (!identical(sub_table, Id(schema = "information_schema", table = "FILES"))) {
          # eval(bquote()) preserves the SQL class, even if it's not apparent
          # in the output
          eval(bquote(expect_true(
            dbExistsTable(con, .(sub_table)),
            label = paste0("dbExistsTable(", dbQuoteIdentifier(con, sub_table), ")")
          )))
        }
      }
    }
  },
  #
  NULL
)
