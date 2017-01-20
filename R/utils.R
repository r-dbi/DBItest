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
    on.exit(dbDisconnect(.(con)), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a newly opened and then closed connection. Disconnects on exit.
with_closed_connection <- function(code, con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    .(con) <- connect(ctx)
    dbDisconnect(.(con))
    local(.(code_sub))
  }
  ), envir = env)
}

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a newly opened but invalidated connection. Disconnects on exit.
with_invalid_connection <- function(code, con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  stopifnot(con != "..con")
  con <- as.name(con)

  eval(bquote({
    ..con <- connect(ctx)
    on.exit(dbDisconnect(..con), add = TRUE)
    .(con) <- unserialize(serialize(..con, NULL))
    local(.(code_sub))
  }
  ), envir = env)
}

# Evaluates the code inside local() after defining a variable "res"
# (can be overridden by specifying con argument)
# that points to a result set created by query. Clears on exit.
with_result <- function(query, code, res = "res", env = parent.frame()) {
  code_sub <- substitute(code)
  query_sub <- substitute(query)

  res <- as.name(res)

  eval(bquote({
    .(res) <- .(query_sub)
    on.exit(dbClearResult(.(res)), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Evaluates the code inside local() after defining a variable "res"
# (can be overridden by specifying con argument)
# that points to a result set created by query. Clears on exit.
with_remove_test_table <- function(code, name = "test", con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    on.exit(
      try_silent(
        dbClearResult(dbSendStatement(.(con), paste0("DROP TABLE ", .(name))))), add = TRUE)
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

compact <- function(x) {
  x[!vapply(x, is.null, logical(1L))]
}

try_silent <- function(code) {
  tryCatch(
    code,
    error = function(e) NULL)
}
