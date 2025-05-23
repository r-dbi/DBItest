#' spec_arrow_create_table_arrow
#' @family Arrow specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_arrow_create_table_arrow <- list(
  arrow_create_table_arrow_formals = function(ctx) {
    skip_if_not_dbitest(ctx, "1.8.0.13")

    # <establish formals of described functions>
    expect_named(formals(dbCreateTableArrow), c("conn", "name", "value", "...", "temporary"))
  },

  arrow_create_table_arrow_return = function(con, table_name) {
    #' @return
    #' `dbCreateTableArrow()` returns `TRUE`, invisibly.
    expect_invisible_true(dbCreateTableArrow(con, table_name, stream_frame(trivial_df())))
  },

  #'
  arrow_create_table_arrow_overwrite = function(con, table_name) {
    #' @section Failure modes:
    #' If the table exists, an error is raised; the remote table remains unchanged.
    test_in <- trivial_df()

    dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
    dbAppendTableArrow(con, table_name, test_in %>% stream_frame())
    expect_error(dbCreateTableArrow(con, table_name, stream_frame(b = 1L)))

    test_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(test_out, test_in)
  },

  #'
  arrow_create_table_arrow_closed_connection = function(ctx, closed_con) {
    #' An error is raised when calling this method for a closed
    expect_error(dbCreateTableArrow(closed_con, "test", stream_frame(a = 1)))
  },

  arrow_create_table_arrow_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbCreateTableArrow(invalid_con, "test", stream_frame(a = 1)))
  },

  arrow_create_table_arrow_error = function(ctx, con, table_name) {
    #' An error is also raised
    test_in <- stream_frame(a = 1L)
    #' if `name` cannot be processed with [dbQuoteIdentifier()] or
    expect_error(dbCreateTableArrow(con, NA, test_in))
    #' if this results in a non-scalar.
    expect_error(dbCreateTableArrow(con, c(table_name, table_name), test_in))

    #' Invalid values for the `temporary` argument
    #' (non-scalars,
    expect_error(dbCreateTableArrow(con, table_name, test_in, temporary = c(TRUE, FALSE)))
    #' unsupported data types,
    expect_error(dbCreateTableArrow(con, table_name, fields = 1L))
    expect_error(dbCreateTableArrow(con, table_name, test_in, temporary = 1L))
    #' `NA`,
    expect_error(dbCreateTableArrow(con, table_name, fields = NA))
    expect_error(dbCreateTableArrow(con, table_name, test_in, temporary = NA))
    #' incompatible values,
    expect_error(dbCreateTableArrow(con, table_name, test_in, fields = letters))
    #' duplicate names)
    # nolint next: duplicate_argument_linter.
    expect_error(dbCreateTableArrow(con, table_name, fields = c(a = "INTEGER", a = "INTEGER")))

    #' also raise an error.
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbCreateTableArrow()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `temporary` (default: `FALSE`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" and "Value" sections for details on their usage.

  arrow_create_table_arrow_name = function(ctx, con) {
    #' @section Specification:
    #' The `name` argument is processed as follows,
    #' to support databases that allow non-syntactic names for their objects:
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      #' - If an unquoted table name as string: `dbCreateTableArrow()` will do the quoting,
      dbCreateTableArrow(con, table_name, test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, dbQuoteIdentifier(con, table_name)))
      expect_equal_df(test_out, test_in[0, , drop = FALSE])
      #'   perhaps by calling `dbQuoteIdentifier(conn, x = name)`
    }
  },

  arrow_create_table_arrow_name_quoted = function(ctx, con) {
    #' - If the result of a call to [dbQuoteIdentifier()]: no more quoting is done
    skip_if_not_dbitest(ctx, "1.7.2")

    if (isTRUE(ctx$tweaks$strict_identifier)) {
      table_names <- "a"
    } else {
      table_names <- c("a", "with spaces", "with,comma")
    }

    for (table_name in table_names) {
      test_in <- trivial_df()

      local_remove_test_table(con, table_name)
      dbCreateTableArrow(con, dbQuoteIdentifier(con, table_name), test_in %>% stream_frame())
      test_out <- check_df(dbReadTable(con, table_name))
      expect_equal_df(test_out, test_in[0, , drop = FALSE])
    }
  },

  arrow_create_table_arrow_value_df = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.5")

    #'
    #' The `value` argument can be:
    #' - a data frame,
    table_name <- "act_df"
    local_remove_test_table(con, table_name)
    df <- data.frame(a = 1)
    dbCreateTableArrow(con, table_name, df)
    expect_equal_df(dbReadTable(con, table_name), data.frame(a = numeric()))
  },

  arrow_create_table_arrow_value_array = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.6")

    #' - a nanoarrow array
    table_name <- "act_array"
    local_remove_test_table(con, table_name)
    array <- nanoarrow::as_nanoarrow_array(data.frame(a = 1))
    dbCreateTableArrow(con, table_name, array)
    expect_equal_df(dbReadTable(con, table_name), data.frame(a = numeric()))
  },

  arrow_create_table_arrow_value_stream = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.7")

    #' - a nanoarrow array stream
    table_name <- "act_stream"
    local_remove_test_table(con, table_name)
    stream <- stream_frame(a = 1)
    dbCreateTableArrow(con, table_name, stream)
    expect_equal(as.data.frame(stream), data.frame(a = 1))
    #'   (which will still contain the data after the call)
    expect_equal_df(dbReadTable(con, table_name), data.frame(a = numeric()))
  },

  arrow_create_table_arrow_value_schema = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.8")

    #' - a nanoarrow schema
    table_name <- "act_schema"
    local_remove_test_table(con, table_name)
    schema <- nanoarrow::infer_nanoarrow_schema(stream_frame(a = 1))
    dbCreateTableArrow(con, table_name, schema)
    expect_equal_df(dbReadTable(con, table_name), data.frame(a = numeric()))
  },

  #'
  arrow_create_table_arrow_temporary_1 = function(ctx, con, table_name = "dbit03") {
    skip_if_not_dbitest(ctx, "1.8.0.4")

    #' If the `temporary` argument is `TRUE`, the table is not available in a
    #' second connection and is gone after reconnecting.
    #' Not all backends support this argument.
    if (!isTRUE(ctx$tweaks$temporary_tables)) {
      skip("tweak: temporary_tables")
    }

    penguins <- get_penguins(ctx)
    dbCreateTableArrow(con, table_name, stream_frame(penguins), temporary = TRUE)
    penguins_out <- check_df(dbReadTable(con, table_name))
    expect_equal_df(penguins_out, penguins[0, , drop = FALSE])

    con2 <- local_connection(ctx)
    expect_error(dbReadTable(con2, table_name))
  },
  # second stage
  arrow_create_table_arrow_temporary_2 = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.4")

    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit03"
    expect_error(dbReadTable(con, table_name))
  },

  arrow_create_table_arrow_visible_in_other_connection_1 = function(ctx, local_con) {
    skip_if_not_dbitest(ctx, "1.8.0.3")

    #' A regular, non-temporary table is visible in a second connection,
    penguins <- get_penguins(ctx)

    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit04"
    dbCreateTableArrow(local_con, table_name, stream_frame(penguins))
    penguins_out <- check_df(dbReadTable(local_con, table_name))
    expect_equal_df(penguins_out, penguins[0, , drop = FALSE])

    con2 <- local_connection(ctx)
    expect_equal_df(dbReadTable(con2, table_name), penguins[0, , drop = FALSE])
  },
  # second stage
  arrow_create_table_arrow_visible_in_other_connection_2 = function(ctx, con) {
    skip_if_not_dbitest(ctx, "1.8.0.3")

    penguins <- get_penguins(ctx)

    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit04"

    #' in a pre-existing connection,
    expect_equal_df(check_df(dbReadTable(con, table_name)), penguins[0, , drop = FALSE])
  },
  # third stage
  arrow_create_table_arrow_visible_in_other_connection_3 = function(ctx, local_con, table_name = "dbit04") {
    skip_if_not_dbitest(ctx, "1.8.0.3")

    penguins <- get_penguins(ctx)

    #' and after reconnecting to the database.
    expect_equal_df(check_df(dbReadTable(local_con, table_name)), penguins[0, , drop = FALSE])
  },

  #'
  arrow_create_table_arrow_roundtrip_keywords = function(ctx, con) {
    #' SQL keywords can be used freely in table names, column names, and data.
    tbl_in <- data.frame(
      select = "unique", from = "join", where = "order",
      stringsAsFactors = FALSE
    )
    test_table_roundtrip(con, tbl_in, name = "exists", use_append = TRUE)
  },

  arrow_create_table_arrow_roundtrip_quotes = function(ctx, con) {
    #' Quotes, commas, and spaces can also be used  for table names and column names,
    #' if the database supports non-syntactic identifiers.
    if (isTRUE(ctx$tweaks$strict_identifier)) {
      skip("tweak: strict_identifier")
    }

    table_names <- c(
      as.character(dbQuoteIdentifier(con, "")),
      as.character(dbQuoteString(con, "")),
      "with space",
      ","
    )

    for (table_name in table_names) {
      tbl_in <- data.frame(trivial_df(4, table_names))

      test_table_roundtrip(con, tbl_in, use_append = TRUE)
    }
  },
  #
  NULL
)
