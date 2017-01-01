#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_connection_connect Specification
NULL

#' spec_connection_connect
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_connection_connect <- list(
  connect_and_disconnect_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbConnect)), c("drv", "..."))
    expect_equal(names(formals(DBI::dbDisconnect)), c("conn", "..."))
  },

  #' @return
  can_connect_and_disconnect = function(ctx) {
    con <- connect(ctx)
    #' `dbConnect()` returns an S4 object that inherits from [DBIConnection-class].
    expect_s4_class(con, "DBIConnection")
    #'
    #' `dbDisconnect()` returns `TRUE`, invisibly.
    expect_true(expect_invisible(dbDisconnect(con)))
  },

  #' @section Specification:
  cannot_disconnect_twice = function(ctx) {
    expect_warning(gc(), NA)
    connect(ctx)
    #' A warning is issued when releasing a connection without calling
    #' `dbDisconnect()`,
    expect_warning(gc())
  },

  cannot_disconnect_twice = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    #' or when calling `dbDisconnect()` on an already disconnected connection.
    expect_warning(dbDisconnect(con))
  },

  NULL
)
