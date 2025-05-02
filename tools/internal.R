pkgload::load_all()
library(tidyverse)

walk_ast <- function(
  expr,
  env = parent.frame(),
  known_values = character(),
  unknown_funcs = character(),
  unknown_vars = character()
) {
  is_known <- function(name) {
    name == "" || name %in% known_values || exists(name, envir = env, inherits = TRUE)
  }

  process_call <- function(call_expr) {
    fn <- call_expr[[1]]

    if (is.symbol(fn)) {
      fn_name <- as.character(fn)
      # Handle unknown function calls
      if (!is_known(fn_name) && !exists(fn_name, mode = "function", envir = env, inherits = TRUE)) {
        unknown_funcs <<- union(unknown_funcs, fn_name)
      }
    } else if (is.call(fn) && fn[[1]] == quote(`::`)) {
      # Handle package::function form
      if (length(fn) == 3 && is.symbol(fn[[3]])) {
        fn_name <- as.character(fn[[3]])
        # Consider function from package known, but still check args
        # Do nothing for fn_name here
      }
    } else {
      # Walk over anonymous or nested function call
      walk_node(expr = fn)
    }

    # Recursively walk through arguments
    walk(call_expr[-1], walk_node)
  }

  walk_node <- function(expr) {
    if (is.function(expr)) {
      # Handle function definitions
      fun_args <- as.character(names(formals(expr)))
      child <- walk_ast(body(expr), env = env, known_values = fun_args)
      unknown_funcs <<- union(unknown_funcs, child$unknown_functions)
      unknown_vars <<- union(unknown_vars, child$unknown_variables)
    } else if (is.call(expr)) {
      if (identical(expr[[1]], quote(`<-`)) && length(expr) == 3) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]
        if (is.symbol(lhs)) {
          lhs_name <- as.character(lhs)
          known_values <<- union(known_values, lhs_name)
        }
        walk_node(rhs)
      } else if (identical(expr[[1]], quote(`function`))) {
        # Handle closures with a fresh set of known values
        fun_args <- as.character(names(expr[[2]]))
        child <- walk_ast(expr[[3]], env = env, known_values = fun_args)
        unknown_funcs <<- union(unknown_funcs, child$unknown_functions)
        unknown_vars <<- union(unknown_vars, child$unknown_variables)
      } else {
        process_call(expr)
      }
    } else if (is.name(expr)) {
      var_name <- as.character(expr)
      if (!is_known(var_name)) {
        unknown_vars <<- union(unknown_vars, var_name)
      }
    } else if (is.pairlist(expr) || is.expression(expr)) {
      for (e in expr) {
        walk_node(e)
      }
    } else if (is.list(expr)) {
      for (e in expr) {
        walk_node(e)
      }
    }
  }

  walk_node(expr)

  list(
    unknown_functions = unknown_funcs,
    unknown_variables = unknown_vars,
    known_values = known_values
  )
}

fun1 <- function(x) {
  foo::bar(x)
}

walk_ast(fun1)

fun2 <- function(x) {
  foo::bar(y)
}

walk_ast(fun2)

fun3 <- function(x) {
  bar(x)
}

walk_ast(fun3)

fun4 <- function(x) {
  y <- x
  fun3(y)
}

walk_ast(fun4)

fun5 <- function(con) {
  invisible()
}

walk_ast(fun5)

fun6 <- function(x) {
  x[1, , 2]
}

walk_ast(fun6)

base <- ls(baseenv())
DBI <- getNamespaceExports("DBI")
testthat <- getNamespaceExports("testthat")

base_values <- mget(base, ifnotfound = list(NULL), envir = baseenv())
DBI_values <- mget(DBI, ifnotfound = list(NULL), envir = asNamespace("DBI"))
testthat_values <- mget(testthat, ifnotfound = list(NULL), envir = asNamespace("testthat"))

values <- base_values |> modifyList(DBI_values) |> modifyList(testthat_values)

walk_ast(compact(spec_all)[[42]], as.environment(values))

missing <- purrr::map(compact(spec_all), walk_ast, as.environment(values), .progress = TRUE)
