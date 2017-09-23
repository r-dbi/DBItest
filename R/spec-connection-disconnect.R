#' spec_connection_disconnect
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_connection_disconnect <- list(
  disconnect_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbDisconnect)), c("conn", "..."))
  },

  #' @return
  can_disconnect = function(ctx) {
    con <- connect(ctx)
    #' `dbDisconnect()` returns `TRUE`, invisibly.
    expect_invisible_true(dbDisconnect(con))
  },

  #' @section Specification:
  #' A warning is issued on garbage collection when a connection has been
  #' released without calling `dbDisconnect()`,
  #' but this cannot be tested automatically.

  #' A warning is issued immediately when calling `dbDisconnect()` on an
  #' already disconnected
  disconnect_closed_connection = function(ctx) {
    with_closed_connection({
      expect_warning(dbDisconnect(con))
    })
  },

  #' or invalid connection.
  disconnect_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_warning(dbDisconnect(con))
    })
  },

  NULL
)
