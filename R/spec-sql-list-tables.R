#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbListTables("DBIConnection")`}{
spec_sql_list_tables <- list(
  #' Can list the tables in the database, adding and removing tables affects
  #' the list. Can also check existence of a table.
  list_tables = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))

      tables <- dbListTables(con)
      expect_is(tables, "character")
      expect_false("iris" %in% tables)

      expect_false(dbExistsTable(con, "iris"))

      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)

      tables <- dbListTables(con)
      expect_true("iris" %in% tables)

      expect_true(dbExistsTable(con, "iris"))

      dbRemoveTable(con, "iris")
      on.exit(NULL, add = FALSE)

      tables <- dbListTables(con)
      expect_false("iris" %in% tables)

      expect_false(dbExistsTable(con, "iris"))
    })
  },

  #' }
  NULL
)
