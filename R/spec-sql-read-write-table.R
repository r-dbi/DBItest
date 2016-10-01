#' @template dbispec-sub-wip
#' @format NULL
#' @section SQL:
#' \subsection{`dbReadTable("DBIConnection")` and `dbWriteTable("DBIConnection")`}{
spec_sql_read_write_table <- list(
  #' Can write the [datasets::iris] data as a table to the
  #' database, but won't overwrite by default.
  write_table = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)
      expect_error(dbWriteTable(con, "iris", iris))

      with_connection({
        expect_error(dbGetQuery(con2, "SELECT * FROM iris"), NA)
      }
      , con = "con2")
    })

    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
    })
  },

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

  #' Can write the [datasets::iris] data as a table to the
  #' database, will overwrite if asked.
  overwrite_table = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)
      expect_error(dbWriteTable(con, "iris", iris[1:10,], overwrite = TRUE),
                   NA)
      iris_out <- dbReadTable(con, "iris")
      expect_identical(nrow(iris_out), 10L)
    })
  },

  #' Can write the [datasets::iris] data as a table to the
  #' database, will append if asked.
  append_table = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbRemoveTable(con, "iris"), NA),
              add = TRUE)

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris)
      expect_error(dbWriteTable(con, "iris", iris[1:10,], append = TRUE), NA)
      iris_out <- dbReadTable(con, "iris")
      expect_identical(nrow(iris_out), nrow(iris) + 10L)
    })
  },

  #' Cannot append to nonexisting table.
  append_table_error = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      on.exit(expect_error(dbRemoveTable(con, "iris")))

      iris <- get_iris(ctx)
      expect_error(dbWriteTable(con, "iris", iris[1:20,], append = TRUE))
    })
  },

  #' Can write the [datasets::iris] data as a temporary table to
  #' the database, the table is not available in a second connection and is
  #' gone after reconnecting.
  temporary_table = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))

      iris <- get_iris(ctx)
      dbWriteTable(con, "iris", iris[1:30, ], temporary = TRUE)
      iris_out <- dbReadTable(con, "iris")
      expect_identical(nrow(iris_out), 30L)

      with_connection({
        expect_error(dbGetQuery(con2, "SELECT * FROM iris"))
      }
      , con = "con2")
    })

    with_connection({
      expect_error(dbGetQuery(con, "SELECT * FROM iris"))
      try(dbRemoveTable(con, "iris"), silent = TRUE)
    })
  },

  #' A new table is visible in a second connection.
  table_visible_in_other_connection = function(ctx) {
    with_connection({
      expect_error(dbGetQuery(con, "SELECT * from test"))

      on.exit(expect_error(dbRemoveTable(con, "test"), NA),
              add = TRUE)

      data <- data.frame(a = 1L)
      dbWriteTable(con, "test", data)

      with_connection({
        expect_error(rows <- dbGetQuery(con2, "SELECT * FROM test"), NA)
        expect_identical(rows, data)
      }
      , con = "con2")
    })
  },

  #' }
  NULL
)
