`%||%` <- function(a, b) if (is.null(a)) b else a

get_pkg_path <- function(ctx) {
  pkg_name <- package_name(ctx)
  expect_is(pkg_name, "character")

  pkg_path <- find.package(pkg_name)
  pkg_path
}

utils::globalVariables("con")
utils::globalVariables("con2")

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, con = "con", extra_args = list(), env = parent.frame()) {
  quo <- enquo(code)

  con <- as.name(con)

  data <- list2(!!con := connect(get("ctx", env), !!!extra_args))
  on.exit(try_silent(dbDisconnect(data[[1]])), add = TRUE)

  eval_tidy(quo, data)
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

# Evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a connection. Removes the table specified by name on exit,
# if it exists.
with_remove_test_table <- function(code, name = "test", con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    on.exit(
      try_silent(
        dbExecute(.(con), paste0("DROP TABLE ", dbQuoteIdentifier(.(con), .(name))))
      ),
      add = TRUE
    )
    local(.(code_sub))
  }
  ), envir = env)
}

# Evaluates the code inside local() after defining a variable "con"
# (can be overridden by specifying con argument)
# that points to a result set created by query. Clears on exit.
with_rollback_on_error <- function(code, con = "con", env = parent.frame()) {
  code_sub <- substitute(code)

  con <- as.name(con)

  eval(bquote({
    on.exit(
      try_silent(
        dbRollback(.(con))
      ),
      add = TRUE
    )
    local(.(code_sub))
    on.exit(NULL, add = FALSE)
  }
  ), envir = env)
}

get_iris <- function(ctx) {
  datasets_iris <- datasets::iris
  iris$Species <- as.character(iris$Species)
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

expand_char <- function(...) {
  df <- expand.grid(..., stringsAsFactors = FALSE)
  do.call(paste0, df)
}

try_silent <- function(code) {
  tryCatch(
    code,
    error = function(e) NULL)
}

check_df <- function(df) {
  expect_is(df, "data.frame")
  if (length(df) >= 1L) {
    lengths <- vapply(df, length, integer(1L), USE.NAMES = FALSE)
    expect_equal(diff(lengths), rep(0L, length(lengths) - 1L))
    expect_equal(nrow(df), lengths[[1]])
  }

  df_names <- names(df)
  expect_true(all(df_names != ""))
  expect_false(anyNA(df_names))

  df
}
