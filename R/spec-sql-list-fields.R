#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbListFields("DBIConnection")`}{
spec_sql_list_fields <- list(
  #' Can list the fields for a table in the database.
  list_fields = function(ctx) {
    with_connection({
      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)

      fields <- dbListFields(con, "iris")
      expect_identical(fields, names(iris))
    })
  },

  #' }
  NULL
)
