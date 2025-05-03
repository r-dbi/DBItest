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
      if (fn_name %in% c("bquote", "$", "@", "test_select_with_null")) {
        # Do nothing for NSE
        return()
      }

      # Handle unknown function calls
      if (!is_known(fn_name) && !exists(fn_name, mode = "function", envir = env, inherits = TRUE)) {
        unknown_funcs <<- union(unknown_funcs, fn_name)
      }
    } else if (is.call(fn) && fn[[1]] == "::") {
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
      } else if (identical(expr[[1]], quote(`=`)) && length(expr) == 3) {
        lhs <- expr[[2]]
        if (is.symbol(lhs)) {
          lhs_name <- as.character(lhs)
          known_values <<- union(known_values, lhs_name)
        }
        rhs <- expr[[3]]
        walk_node(rhs)
      } else if (identical(expr[[1]], quote(`for`))) {
        # Handle for loops
        lhs <- expr[[2]]
        if (is.symbol(lhs)) {
          lhs_name <- as.character(lhs)
          known_values <<- union(known_values, lhs_name)
        }
        walk_node(expr[[3]])
        walk_node(expr[[4]])
      } else if (identical(expr[[1]], quote(`function`))) {
        # Handle closures with a fresh set of known values
        fun_args <- as.character(names(expr[[2]]))
        child <- walk_ast(expr[[3]], env = env, known_values = union(known_values, fun_args))
        unknown_funcs <<- union(unknown_funcs, child$unknown_functions)
        unknown_vars <<- union(unknown_vars, child$unknown_variables)
      } else if (identical(expr[[1]], quote(`::`))) {
        # Do nothing
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

fun7 <- function(x) {
  bquote(.(a))
}

walk_ast(fun7)

fun8 <- function(x) {
  a$b
}

walk_ast(fun8)

fun9 <- function(x) {
  for (i in x) {
    i + j
  }
}

walk_ast(fun9)

fun10 <- function(x) {
  f <- function(y) {
    x + y
  }
  f(3)
}

walk_ast(fun10)

fun11 <- function(x) {
  y <- datasets::mtcars
}

walk_ast(fun11)

fun12 <- function(x) {
  x %>% fun11()
}

walk_ast(fun12, asNamespace("magrittr"))

base <- ls(baseenv(), all.names = TRUE)
stats <- c("setNames")
utils <- c("head")
methods <- c("extends", "getClasses", "is")
DBI <- getNamespaceExports("DBI")
testthat <- getNamespaceExports("testthat")
magrittr <- c("%>%")
DBItest <- c(
  "get_pkg_path",
  "package_name",
  "expect_all_args_have_default_values",
  "expect_arglist_is_empty",
  "test_data_type",
  "connect",
  "local_connection",
  "expect_invisible_true",
  "trivial_query",
  "check_df",
  "get_placeholder_funs",
  "trivial_values",
  "trivial_df",
  "local_result",
  "sql_union",
  "unrowname",
  "skip_if_not_dbitest",
  "trivial_statement",
  "random_table_name",
  "local_remove_test_table",
  "test_select_with_null",
  "get_texts",
  "map",
  "try_silent",
  "test_select",
  "map_chr",
  "map_lgl",
  "get_penguins",
  "expect_equal_df",
  "local_closed_connection",
  "local_invalid_connection",
  "test_table_roundtrip",
  "test_table_roundtrip_one",
  "as_numeric_date",
  "walk",
  "check_arrow",
  "stream_frame",
  "test_arrow_roundtrip",
  "test_arrow_roundtrip_one",
  "expect_equal_arrow",
  "get_key_methods",
  "expect_has_class_method",
  "dbi_generics",
  "s4_methods",
  "has_utf8_or_ascii_encoding",
  "is_timestamp",
  "expect_ellipsis_in_formals",
  NULL
)
DBItest <- character()

base_values <- mget(base, ifnotfound = list(NULL), envir = baseenv())
stats_values <- mget(stats, ifnotfound = list(NULL), envir = asNamespace("stats"))
utils_values <- mget(utils, ifnotfound = list(NULL), envir = asNamespace("utils"))
methods_values <- mget(methods, ifnotfound = list(NULL), envir = asNamespace("methods"))
DBI_values <- mget(DBI, ifnotfound = list(NULL), envir = asNamespace("DBI"))
testthat_values <- mget(testthat, ifnotfound = list(NULL), envir = asNamespace("testthat"))
magrittr_values <- mget(magrittr, ifnotfound = list(NULL), envir = asNamespace("magrittr"))
DBItest_values <- mget(DBItest, ifnotfound = list(NULL), envir = asNamespace("DBItest"))

values <- base_values |>
  modifyList(stats_values) |>
  modifyList(utils_values) |>
  modifyList(methods_values) |>
  modifyList(DBI_values) |>
  modifyList(testthat_values) |>
  modifyList(magrittr_values) |>
  modifyList(DBItest_values)

missing <- purrr::map(compact(spec_all), walk_ast, as.environment(values), .progress = TRUE)

counts <- missing |> map(lengths) |> map_int(sum)

true_missing <- missing[counts > 0]

# No unknown values
true_missing |>
  purrr::map("unknown_values") |>
  enframe(value = "value_name") |>
  unnest(value_name) |>
  count(value_name) |>
  arrange(n)

true_missing_functions_df <-
  true_missing |>
  purrr::map("unknown_functions") |>
  enframe(value = "function_name") |>
  unnest(function_name) |>
  count(function_name) |>
  arrange(n)

true_missing_functions_df |>
  mutate(code = map_chr(
    function_name,
    ~ paste(capture.output(print(get(.x, asNamespace("DBItest")))), collapse = "\n")
  )) |>
  mutate(text = glue::glue("
    # {function_name} ({n})

    `r d(DBItest:::{function_name})`

    ```r
    {code}
    ```
    ")) |>
  summarize(rmd = paste(text, collapse = "\n\n")) |>
  pull() |>
  writeLines("tools/missing_functions.Rmd")
