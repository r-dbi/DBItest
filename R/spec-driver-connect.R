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
  },

  #' @section Specification:
  #' DBI specifies only the return type for `dbConnect()`.

  NULL
)
