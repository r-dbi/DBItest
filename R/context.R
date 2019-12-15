#' Test contexts
#'
#' Create a test context, set and query the default context.
#'
#' @param drv `[DBIConnector]`\cr
#'   An object of class [DBIConnector-class] that describes how to connect
#'   to the database.
#' @param connect_args `[named list]`\cr Deprecated.
#' @param set_as_default `[logical(1)]`\cr Should the created context be
#'   set as default context?
#' @param tweaks `[DBItest_tweaks]`\cr Tweaks as constructed by the
#'   [tweaks()] function.
#' @param ctx `[DBItest_context]`\cr A test context.
#' @param name `[character]`\cr An optional name of the context which will
#'   be used in test messages.
#' @param default_skip `[character]`\cr Default value of `skip` argument
#'   to [test_all()]  and other testing functions.
#'
#' @return `[DBItest_context]`\cr A test context, for
#'   `set_default_context` the previous default context (invisibly) or
#'   `NULL`.
#'
#' @rdname context
#' @importFrom methods is new
#' @export
#' @example examples/make_context.R
make_context <- function(drv, connect_args = NULL, set_as_default = TRUE,
                         tweaks = NULL, name = NULL, default_skip = NULL) {
  if (is.null(drv)) {
    abort("drv cannot be NULL.")
  }

  if (is(drv, "DBIDriver")) {
    if (is.null(connect_args)) {
      connect_args <- list()
    }
    cnr <- new("DBIConnector", .drv = drv, .conn_args = connect_args)
  } else if (is(drv, "DBIConnector")) {
    cnr <- drv
    drv <- cnr@.drv
  } else {
    abort("drv must be of class DBIConnector.")
  }

  if (is.null(tweaks)) {
    tweaks <- tweaks()
  }

  ctx <- structure(
    list(
      cnr = cnr,
      drv = drv,
      tweaks = tweaks,
      name = name,
      default_skip = default_skip
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

connect <- function(ctx, ...) {
  quos <- enquos(...)
  eval_tidy(quo(dbConnect(ctx$cnr, !!!quos)))
}

.ctx_env <- new.env(parent = emptyenv())
set_default_context(NULL)
