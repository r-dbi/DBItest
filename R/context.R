#' Test contexts
#'
#' Create a test context, set and query the default context.
#'
#' @param drv \code{[DBIDriver]}\cr A DBI driver.
#' @param connect_args \code{[named list]}\cr Connection arguments (names and
#'   values).
#' @param set_as_default \code{[logical(1)]}\cr Should the created context be
#'   set as default context?
#' @param ctx \code{[DBItest_context]}\cr A test context.
#' @return \code{[DBItest_context]}\cr A test context, for
#'   \code{set_default_context} the previous default context (invisibly) or
#'   \code{NULL}.
#'
#' @rdname context
#' @export
make_context <- function(drv, connect_args, set_as_default = TRUE) {
  if (is.null(drv)) {
    stop("drv cannot be NULL.")
  }

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

#' @rdname context
#' @export
set_default_context <- function(ctx) {
  old_ctx <- .ctx_env$default_context
  .ctx_env$default_context <- ctx
  invisible(old_ctx)
}

#' @rdname context
#' @export
get_default_context <- function() {
  .ctx_env$default_context
}

package_name <- function(ctx) {
  attr(class(ctx$drv), "package")
}

connect <- function(ctx) {
  do.call(dbConnect, c(list(ctx$drv), ctx$connect_args))
}

.ctx_env <- new.env(parent = emptyenv())
set_default_context(NULL)
