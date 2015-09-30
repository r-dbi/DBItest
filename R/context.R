#' @export
make_context <- function(drv, connect_args, set_as_default = TRUE) {
  ctx <- structure(
    list(
      drv = drv,
      connect_args = connect_args
    ),
    class = "DBItest_context"
  )

  if (set_as_default) {
    set_default_context(ctx)
  }

  ctx
}

#' @export
set_default_context <- function(ctx) {
  old_ctx <- .ctx_env$default_context
  .ctx_env$default_context <- ctx
  invisible(old_ctx)
}

#' @export
get_default_context <- function() {
  .ctx_env$default_context
}

.ctx_env <- new.env(parent = emptyenv())
set_default_context(NULL)
