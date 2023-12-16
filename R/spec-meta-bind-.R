# Helpers -----------------------------------------------------------------

test_select_bind <- function(con, ctx, bind_values, ..., requires_names = NULL) {
  placeholder_funs <- get_placeholder_funs(ctx, requires_names)

  lapply(
    placeholder_funs,
    test_select_bind_one,
    con = con,
    bind_values = bind_values,
    is_null_check = ctx$tweaks$is_null_check,
    allow_na_rows_affected = ctx$tweaks$allow_na_rows_affected,
    ...
  )
}

get_placeholder_funs <- function(ctx, requires_names = NULL) {
  placeholder_fun <- ctx$tweaks$placeholder_pattern
  if (is.character(placeholder_fun)) {
    placeholder_funs <- lapply(placeholder_fun, make_placeholder_fun)
  } else if (is.function(placeholder_fun)) {
    placeholder_funs <- list(placeholder_fun)
  } else {
    placeholder_funs <- placeholder_fun
  }

  if (length(placeholder_funs) == 0) {
    skip("Use the placeholder_pattern tweak, or skip all 'bind_.*' tests")
  }

  if (!is.null(requires_names)) {
    placeholder_fun_values <- map(placeholder_funs, ~ .x(1))
    placeholder_unnamed <- map_lgl(placeholder_fun_values, ~ is.null(names(.x)))

    # run_bind_tester$fun()
    if (isTRUE(requires_names)) {
      placeholder_funs <- placeholder_funs[!placeholder_unnamed]
    }

    if (isFALSE(requires_names)) {
      placeholder_funs <- placeholder_funs[placeholder_unnamed]
    }
  }

  placeholder_funs
}

test_select_bind_one <- function(
    # Run time
    con,
    placeholder_fun,
    ...,
    is_null_check,
    cast_fun = identity,
    allow_na_rows_affected = FALSE,
    # Spec time
    bind_values,
    query = TRUE,
    skip_fun = NULL,
    check_return_value = NULL,
    patch_bind_values = NULL,
    bind_error = NA,
    is_repeated = FALSE,
    is_premature_clear = FALSE,
    is_untouched = FALSE) {

  rlang::check_dots_empty()

  test_expr <- run_bind_tester$fun(
    bind_values = bind_values,
    query = query,
    skip_fun = skip_fun,
    check_return_value = check_return_value,
    patch_bind_values = patch_bind_values,
    bind_error = bind_error,
    is_repeated = is_repeated,
    is_premature_clear = is_premature_clear,
    is_untouched = is_untouched
  )

  rm(bind_values)
  rm(query)
  rm(skip_fun)
  rm(check_return_value)
  rm(patch_bind_values)
  rm(bind_error)
  rm(is_repeated)
  rm(is_premature_clear)
  rm(is_untouched)

  force(placeholder_fun)
  force(is_null_check)
  force(cast_fun)
  force(allow_na_rows_affected)

  rlang::eval_bare(test_expr)
}


# make_placeholder_fun ----------------------------------------------------

#' Create a function that creates n placeholders
#'
#' For internal use by the `placeholder_format` tweak.
#'
#' @param pattern `[character(1)]`\cr Any character, optionally followed by `1` or `name`. Examples: `"?"`, `"$1"`, `":name"`
#'
#' @return `[function(n)]`\cr A function with one argument `n` that
#'   returns a vector of length `n` with placeholders of the specified format.
#'
#' @keywords internal
#' @examples
#' body(DBItest:::make_placeholder_fun("?"))
#' DBItest:::make_placeholder_fun("?")(2)
#' DBItest:::make_placeholder_fun("$1")(3)
#' DBItest:::make_placeholder_fun(":name")(5)
make_placeholder_fun <- function(pattern) {
  format_rx <- "^(.)(.*)$"

  character <- gsub(format_rx, "\\1", pattern)
  kind <- gsub(format_rx, "\\2", pattern)

  if (character == "") {
    stop("placeholder pattern must have at least one character", call. = FALSE)
  }

  if (kind == "") {
    eval(bquote(
      function(n) rep(.(character), n)
    ))
  } else if (kind == "1") {
    eval(bquote(
      function(n) paste0(.(character), seq_len(n))
    ))
  } else if (kind == "name") {
    eval(bquote(
      function(n) {
        l <- letters[seq_len(n)]
        stats::setNames(paste0(.(character), l), l)
      }
    ))
  } else {
    stop("Pattern must be any character, optionally followed by 1 or name. Examples: $1, :name", call. = FALSE)
  }
}

is_na_or_null <- function(x) {
  identical(x, list(NULL)) || any(is.na(x))
}
