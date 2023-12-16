# Helpers -----------------------------------------------------------------

test_select_bind <- function(con, ctx, ...) {
  lapply(
    get_placeholder_funs(ctx),
    test_select_bind_one,
    con = con,
    is_null_check = ctx$tweaks$is_null_check,
    allow_na_rows_affected = ctx$tweaks$allow_na_rows_affected,
    ...
  )
}

get_placeholder_funs <- function(ctx) {
  placeholder_fun <- ctx$tweaks$placeholder_pattern
  if (is.character(placeholder_fun)) {
    placeholder_fun <- lapply(placeholder_fun, make_placeholder_fun)
  } else if (is.function(placeholder_fun)) {
    placeholder_fun <- list(placeholder_fun)
  }

  if (length(placeholder_fun) == 0) {
    skip("Use the placeholder_pattern tweak, or skip all 'bind_.*' tests")
  }

  placeholder_fun
}

test_select_bind_one <- function(
    con,
    placeholder_fun,
    is_null_check,
    values,
    query = TRUE,
    extra = "none",
    cast_fun = identity,
    allow_na_rows_affected = FALSE) {

  run_bind_tester$fun(
    con,
    placeholder_fun = placeholder_fun,
    is_null_check = is_null_check,
    cast_fun = cast_fun,
    allow_na_rows_affected = allow_na_rows_affected,
    values = values,
    query = query,
    extra_obj = new_extra_imp(extra)
  )
}

new_extra_imp <- function(extra) {
  if (is.environment(extra)) {
    extra$new()
  } else if (length(extra) == 0) {
    new_extra_imp_one("none")
  } else if (length(extra) == 1) {
    new_extra_imp_one(extra)
  } else {
    stop("need BindTesterExtraMulti")
    # BindTesterExtraMulti$new(lapply(extra, new_extra_imp_one))
  }
}

new_extra_imp_one <- function(extra) {
  extra_imp <- switch(extra,
    none = BindTesterExtra,
    stop("Unknown extra: ", extra, call. = FALSE)
  )

  extra_imp$new()
}


# BindTester --------------------------------------------------------------

BindTester <- R6::R6Class(
  "BindTester",
  portable = FALSE,
  #
  public = list(
    initialize = function(con) {
      self$con <- con
    },
    run = run_bind_tester$fun,
    #
    con = NULL,
    placeholder_fun = NULL,
    is_null_check = NULL,
    cast_fun = NULL,
    allow_na_rows_affected = NULL,
    values = NULL,
    query = TRUE,
    extra_obj = NULL
  )
)


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
