#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{Roundtrip tests}{
spec_sql_read_table <- list(
  #' Can read the [datasets::iris] data from a database table.
  read_table = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris_in <- get_iris(ctx)
      iris_in$Species <- as.character(iris_in$Species)
      order_in <- do.call(order, iris_in)

      dbWriteTable(con, "iris", iris_in)
      iris_out <- dbReadTable(con, "iris")
      order_out <- do.call(order, iris_out)

      expect_identical(unrowname(iris_in[order_in, ]), unrowname(iris_out[order_out, ]))
    })
  },

  #' }
  NULL
)
