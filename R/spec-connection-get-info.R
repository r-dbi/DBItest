#' spec_connection_get_info
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @rdname spec_get_info
spec_connection_get_info <- list(
  #' @return
  #' For objects of class [DBIConnection-class], `dbGetInfo()`
  get_info_connection = function(con) {
    info <- dbGetInfo(con)
    #' returns a named list
    expect_type(info, "list")

    info_names <- names(info)

    #' that contains at least the following components:
    #'
    necessary_names <- c(
      #' - `db.version`: version of the database server,
      "db.version",
      #' - `dbname`: database name,
      "dbname",
      #' - `username`: username to connect to the database,
      "username",
      #' - `host`: hostname of the database server,
      "host",
      #' - `port`: port on the database server.
      "port"
    )

    for (name in necessary_names) {
      eval(bquote(
        expect_true(.(name) %in% info_names)
      ))
    }

    #' It must not contain a `password` component.
    expect_false("password" %in% info_names)

    #' Components that are not applicable should be set to `NA`.
  },
  #
  NULL
)
