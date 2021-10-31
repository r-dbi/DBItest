#' spec_meta_column_info
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_meta_column_info <- list(
  column_info_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbColumnInfo)), c("res", "..."))
  },

  #' @return
  #' `dbColumnInfo()`
  column_info = function(ctx, con, table_name) {
    iris <- get_iris(ctx)
    dbWriteTable(con, table_name, iris)

    with_result(
      dbSendQuery(con, paste0("SELECT * FROM ", table_name)),
      {
        fields <- dbColumnInfo(res)
        #' returns a data frame
        expect_is(fields, "data.frame")
        #' with at least two columns `"name"` and `"type"` (in that order)
        expect_equal(names(fields)[1:2], c("name", "type"))
        #' (and optional columns that start with a dot).
        expect_true(all(grepl("^[.]", names(fields)[-1:-2])))

        #' The `"name"` and `"type"` columns contain the names and types
        #' of the R columns of the data frame that is returned from [`dbFetch()`].
        iris_ret <- dbFetch(res)
        expect_identical(fields$name, names(iris_ret))
        #' The `"type"` column is of type `character` and only for information.
        expect_is(fields$type, "character")
        #' Do not compute on the `"type"` column, instead use `dbFetch(res, n = 0)`
        #' to create a zero-row data frame initialized with the correct data types.
      }
    )
  },


  #'
  #' An attempt to query columns for a closed result set raises an error.
  column_info_closed = function(con) {
    query <- trivial_query()

    res <- dbSendQuery(con, query)
    dbClearResult(res)

    expect_error(dbColumnInfo(res))
  },

  #' @section Specification:
  #'
  #' A column named `row_names` is treated like any other column.
  column_info_row_names = function(con, table_name) {
    dbWriteTable(con, table_name, data.frame(a = 1L, row_names = 2L))
    with_result(
      dbSendQuery(con, paste0("SELECT * FROM ", table_name)),
      {
        expect_identical(dbColumnInfo(res)$name, c("a", "row_names"))
      }
    )
  },

  #'
  column_info_consistent = function(ctx, con) {
    with_result(
      dbSendQuery(con, "SELECT 1.5 AS a, 2.5 AS b"),
      {
        #' The column names are always consistent
        info <- dbColumnInfo(res)
        #' with the data returned by `dbFetch()`.
        data <- dbFetch(res)
        expect_identical(info$name, names(data))
      }
    )
    with_result(
      #' If the query returns unnamed columns,
      dbSendQuery(con, "SELECT 1.5, 2.5 AS a, 1.5, 3.5"),
      {
        info <- dbColumnInfo(res)
        data <- dbFetch(res)
        expect_identical(info$name, names(data))
        expect_equal(data[["a"]], 2.5)
        #' unique
        expect_equal(anyDuplicated(names(data)), 0)
        #' non-empty and non-`NA` names are assigned.
        expect_false(anyNA(names(data)))
        expect_true(all(names(data) != ""))
      }
    )
    with_result(
      #' In the case of a duplicate column name, the first occurrence
      #' retains the original name, and unique names are assigned for
      #' the other occurrences.
      dbSendQuery(con, "SELECT 1.5 AS a, 2.5 AS a, 3.5 AS a"),
      {
        info <- dbColumnInfo(res)
        data <- dbFetch(res)
        expect_identical(info$name, names(data))
        expect_equal(data[["a"]], 1.5)
        expect_equal(anyDuplicated(names(data)), 0)
        expect_false(anyNA(names(data)))
        expect_true(all(names(data) != ""))
      }
    )
    with_result(
      #' Column names that correspond to SQL or R keywords are left unchanged.
      dbSendQuery(con, paste0("SELECT 1.5 AS ", dbQuoteIdentifier(con, "for"))),
      {
        info <- dbColumnInfo(res)
        data <- dbFetch(res)
        expect_identical(info$name, names(data))
        expect_equal(data[["for"]], 1.5)
      }
    )
  },
  #
  NULL
)
