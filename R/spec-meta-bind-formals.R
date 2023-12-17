#' spec_meta_bind
#' @name spec_meta_bind
#' @family meta specifications
#' @aliases NULL
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_bind_formals <- list(
  bind_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBind)), c("res", "params", "..."))
  },
  #'
  bind_empty = function(con) {
    #' @section Failure modes:
    #' Calling `dbBind()` for a query without parameters
    res <- local_result(dbSendQuery(con, trivial_query()))
    #' raises an error.
    expect_error(dbBind(res, list()))
  },
  NULL
)
