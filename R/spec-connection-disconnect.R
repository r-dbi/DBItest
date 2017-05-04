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

  #' An error is raised when attempting to disconnect from an already closed
  disconnect_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbDisconnect(con))
    })
  },

  #' or invalid connection.
  disconnect_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbDisconnect(con))
    })
  },

  #' @section Specification:
  cannot_forget_disconnect = function(ctx) {
    expect_warning(gc(), NA)
    connect(ctx)
    #' A warning is issued on garbage collection when a connection has been
    #' released without calling `dbDisconnect()`.
    expect_warning(gc())
  },

  NULL
)
