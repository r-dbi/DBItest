#' spec_driver_connect
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_driver_connect <- list(
  connect_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbConnect)), c("drv", "..."))
  },

  #' @return
  can_connect = function(ctx) {
    con <- expect_visible(connect(ctx))
    #' `dbConnect()` returns an S4 object that inherits from [DBIConnection-class].
    expect_s4_class(con, "DBIConnection")
    dbDisconnect(con)
    #' This object is used to communicate with the database engine.
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

  NULL
)
