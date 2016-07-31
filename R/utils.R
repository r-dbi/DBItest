`%||%` <- function(a, b) if (is.null(a)) b else a

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

get_iris <- function(ctx) {
  datasets_iris <- datasets::iris
  if (isTRUE(ctx$tweaks$strict_identifier)) {
    names(datasets_iris) <- gsub(".", "_", names(datasets_iris), fixed = TRUE)
  }
  datasets_iris
}

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

all_have_utf8_or_ascii_encoding <- function(x) {
  all(vapply(x, has_utf8_or_ascii_encoding, logical(1L)))
}
