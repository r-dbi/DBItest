#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_connection_connect Specification
NULL

#' spec_connection_connect
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Specification:
spec_connection_connect <- list(
  #' Can connect and disconnect, connection object inherits from
  #'   "DBIConnection".
  can_connect_and_disconnect = function(ctx) {
    con <- connect(ctx)
    expect_s4_class(con, "DBIConnection")
    expect_true(dbDisconnect(con))
  },

  #' Repeated disconnect throws warning.
  cannot_disconnect_twice = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_warning(dbDisconnect(con))
  },

  NULL
)
