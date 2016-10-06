`%||%` <- function(a, b) if (is.null(a)) b else a

get_pkg <- function(ctx) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    skip("devtools not installed")
  }

  pkg_name <- package_name(ctx)
  expect_is(pkg_name, "character")

  pkg_path <- find.package(pkg_name)

  devtools::as.package(pkg_path)
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

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}
