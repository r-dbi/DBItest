#' @name test_all
#' @section Tests:
#' \code{\link{test_result}}:
#' Test the "Result" class
NULL

#' Test the "Result" class
#'
#' @inheritParams test_all
#' @include test-connection.R
#' @family tests
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  run_tests(ctx, spec_result, skip, test_suite)
}

utils::globalVariables("con")
utils::globalVariables("con2")

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    .(con) <- connect(ctx)
    on.exit(expect_error(dbDisconnect(.(con)), NA), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Helper function
union <- function(..., .order_by = NULL, .ctx) {
  if (is.null(.ctx$tweaks$union)) {
    query <- paste(c(...), collapse = " UNION ")
  } else {
    query <- .ctx$tweaks$union(c(...))
  }

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}
