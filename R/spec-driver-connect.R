#' spec_driver_connect
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_driver_connect <- list(
  connect_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbConnect)), c("drv", "..."))
  },

  #' @return
  connect_can_connect = function(ctx) {
    con <- expect_visible(connect(ctx))
    #' `dbConnect()` returns an S4 object that inherits from [DBIConnection-class].
    expect_s4_class(con, "DBIConnection")
    dbDisconnect(con)
    #' This object is used to communicate with the database engine.
  },
  #
  connect_format = function(con) {
    #'
    #' A [format()] method is defined for the connection object.
    desc <- format(con)
    #' It returns a string that consists of a single line of text.
    expect_is(desc, "character")
    expect_length(desc, 1)
    expect_false(grepl("\n", desc, fixed = TRUE))
  },

  #' @section Specification:
  #' DBI recommends using the following argument names for authentication
  #' parameters, with `NULL` default:
  #' - `user` for the user name (default: current user)
  #' - `password` for the password
  #' - `host` for the host name (default: local connection)
  #' - `port` for the port number (default: local connection)
  #' - `dbname` for the name of the database on the host, or the database file
  #'   name
  #'
  #' The defaults should provide reasonable behavior, in particular a
  #' local connection for `host = NULL`.  For some DBMS (e.g., PostgreSQL),
  #' this is different to a TCP/IP connection to `localhost`.
  #'
  #' In addition, DBI supports the `bigint` argument that governs how
  #' 64-bit integer data is returned.  The following values are supported:
  connect_bigint_integer = function(ctx) {
    #' - `"integer"`: always return as `integer`, silently overflow
    con <- local_connection(ctx, bigint = "integer")
    res <- dbGetQuery(con, "SELECT 10000000000")
    expect_type(res[[1]], "integer")
  },
  #
  connect_bigint_numeric = function(ctx) {
    #' - `"numeric"`: always return as `numeric`, silently round
    con <- local_connection(ctx, bigint = "numeric")
    res <- dbGetQuery(con, "SELECT 10000000000")
    expect_type(res[[1]], "double")
    expect_equal(res[[1]], 1e10)
  },
  #
  connect_bigint_character = function(ctx) {
    #' - `"character"`: always return the decimal representation as `character`
    con <- local_connection(ctx, bigint = "character")
    res <- dbGetQuery(con, "SELECT 10000000000")
    expect_type(res[[1]], "character")
    expect_equal(res[[1]], "10000000000")
  },
  #
  connect_bigint_integer64 = function(ctx) {
    #' - `"integer64"`: return as a data type that can be coerced using
    #'   [as.integer()] (with warning on overflow), [as.numeric()]
    #'   and [as.character()]
    con <- local_connection(ctx, bigint = "integer64")
    res <- dbGetQuery(con, "SELECT 10000000000")
    expect_warning(expect_true(is.na(as.integer(res[[1]]))))
    expect_equal(as.numeric(res[[1]]), 1e10)
    expect_equal(as.character(res[[1]]), "10000000000")
  },
  #
  NULL
)
