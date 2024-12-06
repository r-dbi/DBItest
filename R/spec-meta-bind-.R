# Helpers -----------------------------------------------------------------

test_select_bind_expr <- function(
  bind_values,
  ctx = stop("ctx is available during run time only"),
  ...,
  arrow,
  bind,
  query = TRUE,
  skip_fun = NULL,
  dbitest_version = NULL,
  cast_fun = NULL,
  requires_names = NULL) {
  force(bind_values)
  force(arrow)
  force(bind)

  caller <- sys.function(-1)
  caller_src <- utils::getSrcref(caller)
  caller_ref <- paste0("<R/", utils::getSrcFilename(caller_src), ":", utils::getSrcLocation(caller_src), ">")

  roxygen_bits <- grep("#' .*$", as.character(caller_src), value = TRUE)
  docstring <- gsub("^ +#' *", "", roxygen_bits)

  header <- c(caller_ref, docstring)

  cast_fun <- enquo(cast_fun)
  has_cast_fun <- !quo_is_null(cast_fun)
  cast_fun_expr <- if (has_cast_fun) expr({
    cast_fun <- !!quo_get_expr(cast_fun)
  })

  test_expr <- test_select_bind_expr_one$fun(
    bind_values = bind_values,
    ...,
    arrow = arrow,
    bind = bind,
    query = query,
    has_cast_fun = has_cast_fun
  )

  skip_dbitest_expr <- if (!is.null(dbitest_version)) expr({
    skip_if_not_dbitest(ctx, !!dbitest_version)
  })

  skip_expr <- if (!is.null(skip_fun)) expr({
    skip_if(!!body(skip_fun))
  })

  if (is.null(requires_names)) {
    placeholder_funs_expr <- expr(get_placeholder_funs(ctx))
  } else {
    placeholder_funs_expr <- expr(get_placeholder_funs(ctx, requires_names = !!requires_names))
  }

  allow_na_rows_affected_expr <- if (!query) expr({
    allow_na_rows_affected <- ctx$tweaks$allow_na_rows_affected
  })

  expr({
    !!!header

    !!skip_dbitest_expr
    !!skip_expr
    placeholder_funs <- !!placeholder_funs_expr

    is_null_check <- ctx$tweaks$is_null_check
    !!cast_fun_expr
    !!allow_na_rows_affected_expr

    for (placeholder_fun in placeholder_funs) {
      !!test_expr
    }
  })
}

get_placeholder_funs <- function(ctx, requires_names = NULL) {
  placeholder_fun <- ctx$tweaks$placeholder_pattern
  if (is.character(placeholder_fun)) {
    placeholder_funs <- map(placeholder_fun, make_placeholder_fun)
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
