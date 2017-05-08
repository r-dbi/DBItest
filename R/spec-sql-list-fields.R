#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbListFields("DBIConnection")`}{
spec_sql_list_fields <- list(
  #' Can list the fields for a table in the database.
  list_fields = function(ctx) {
    with_connection({
      with_remove_test_table(name = "iris", {
        iris <- get_iris(ctx)
        dbWriteTable(con, "iris", iris)

        fields <- dbListFields(con, "iris")
        expect_identical(fields, names(iris))
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

  #' }
  NULL
)
