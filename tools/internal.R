pkgload::load_all()
library(tidyverse)

check_missing_objects <- function(fun, env = parent.frame()) {
  if (!is.function(fun)) stop("Input must be a function.")

  # Get the function's body and formal arguments
  body_expr <- body(fun)
  arg_names <- names(formals(fun))

  # Collect all symbols (names) in the function body
  all_symbols <- all.names(body_expr, functions = TRUE, unique = TRUE)

  # Symbols used in function calls
  call_symbols <- unique(unlist(lapply(all_calls(body_expr), function(call) as.character(call[[1]]))))

  # Identify symbols not used as function names (i.e., likely values)
  value_symbols <- setdiff(all_symbols, call_symbols)

  # Remove function arguments from value_symbols
  value_symbols <- setdiff(value_symbols, arg_names)

  # Find assigned variables in the function body
  assigned_vars <- find_assigned_vars(body_expr)

  # Remove assigned variables from value_symbols
  value_symbols <- setdiff(value_symbols, assigned_vars)

  # Now check existence in the environment
  missing_values <- value_symbols[!vapply(value_symbols, exists, logical(1), envir = env, inherits = TRUE)]
  missing_calls <- call_symbols[!vapply(call_symbols, function(f) exists(f, mode = "function", envir = env, inherits = TRUE), logical(1))]

  list(
    missing_values = missing_values,
    missing_calls = missing_calls
  )
}

# Helper function to extract all function calls from an expression
all_calls <- function(expr) {
  calls <- list()
  recurse <- function(e) {
    if (is.call(e)) {
      calls <<- c(calls, list(e))
      lapply(e[-1], recurse)
    }
  }
  recurse(expr)
  calls
}

# Helper function to find variables being assigned to in the function body
find_assigned_vars <- function(expr) {
  assigned <- character()

  find_assignments <- function(e) {
    if (is.call(e)) {
      # Check for assignment operators: <- and =
      if (is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=")) {
        # If left side is a name, add it to assigned vars
        if (is.name(e[[2]])) {
          assigned <<- c(assigned, as.character(e[[2]]))
        }
      }
      # Recursively check all parts of the call
      for (i in seq_along(e)[-1]) {
        find_assignments(e[[i]])
      }
    }
  }

  find_assignments(expr)
  unique(assigned)
}

# check_missing_objects(spec_all[[3]])

base <- ls(baseenv())
DBI <- getNamespaceExports("DBI")
testthat <- getNamespaceExports("testthat")

base_values <- mget(base, ifnotfound = list(NULL), envir = baseenv())
DBI_values <- mget(DBI, ifnotfound = list(NULL), envir = asNamespace("DBI"))
testthat_values <- mget(testthat, ifnotfound = list(NULL), envir = asNamespace("testthat"))

values <- base_values |> modifyList(DBI_values) |> modifyList(testthat_values)

missing <- purrr::map(compact(spec_all), check_missing_objects, as.environment(values), .progress = TRUE)
