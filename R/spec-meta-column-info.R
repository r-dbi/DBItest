#' spec_meta_column_info
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_column_info <- list(
  column_info_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbColumnInfo)), c("res", "..."))
  },

  column_info = function(ctx, con, table_name) {
    #' @return
    #' `dbColumnInfo()`
    penguins <- get_penguins(ctx)
    dbWriteTable(con, table_name, penguins)

    res <- local_result(dbSendQuery(con, paste0("SELECT * FROM ", table_name)))
    fields <- dbColumnInfo(res)
    #' returns a data frame
    expect_s3_class(fields, "data.frame")
    #' with at least two columns `"name"` and `"type"` (in that order)
    expect_equal(names(fields)[1:2], c("name", "type"))
    #' (and optional columns that start with a dot).
    expect_true(all(grepl("^[.]", names(fields)[-1:-2])))

    #' The `"name"` and `"type"` columns contain the names and types
    #' of the R columns of the data frame that is returned from [`dbFetch()`].
    penguins_ret <- dbFetch(res)
    expect_identical(fields$name, names(penguins_ret))
    #' The `"type"` column is of type `character` and only for information.
    expect_type(fields$type, "character")
    #' Do not compute on the `"type"` column, instead use `dbFetch(res, n = 0)`
    #' to create a zero-row data frame initialized with the correct data types.
  },


  #'
  column_info_closed = function(con) {
    #' @section Failure modes:
    #' An attempt to query columns for a closed result set raises an error.
    query <- trivial_query()

    res <- dbSendQuery(con, query)
    dbClearResult(res)

    expect_error(dbColumnInfo(res))
  },

  column_info_row_names = function(con, table_name) {
    #' @section Specification:
    #'
    #' A column named `row_names` is treated like any other column.
    dbWriteTable(con, table_name, data.frame(a = 1L, row_names = 2L))
    res <- local_result(dbSendQuery(con, paste0("SELECT * FROM ", table_name)))
    expect_identical(dbColumnInfo(res)$name, c("a", "row_names"))
  },

  #'
  column_info_consistent = function(ctx, con) {
    res <- local_result(dbSendQuery(con, "SELECT 1.5 AS a, 2.5 AS b"))
    #' The column names are always consistent
    info <- dbColumnInfo(res)
    #' with the data returned by `dbFetch()`.
    data <- dbFetch(res)
    expect_identical(info$name, names(data))
  },

  #'
  column_info_consistent_unnamed = function(ctx, con) {
    # odbc package
    skip_if_not_dbitest(ctx, "1.7.2")

    #' If the query returns unnamed columns,
    res <- local_result(dbSendQuery(con, "SELECT 1.5, 2.5 AS a, 1.5, 3.5"))
    info <- dbColumnInfo(res)
    data <- dbFetch(res)
    expect_identical(info$name, names(data))
    expect_equal(data[["a"]], 2.5)
    #' non-empty and non-`NA` names are assigned.
    expect_false(anyNA(names(data)))
    expect_true(all(names(data) != ""))
  },

  #'
  column_info_consistent_keywords = function(ctx, con) {
    #' Column names that correspond to SQL or R keywords are left unchanged.
    res <- local_result(dbSendQuery(con, paste0("SELECT 1.5 AS ", dbQuoteIdentifier(con, "for"))))
    info <- dbColumnInfo(res)
    data <- dbFetch(res)
    expect_identical(info$name, names(data))
    expect_equal(data[["for"]], 1.5)
  },
  #
  NULL
)
