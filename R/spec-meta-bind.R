spec_meta_bind <- c(
  spec_meta_bind_,
  spec_meta_bind_wrong_name,

  NULL
)


# Helpers -----------------------------------------------------------------

test_select_bind <- function(con, placeholder_fun, ...) {
  if (is.character(placeholder_fun))
    placeholder_fun <- lapply(placeholder_fun, make_placeholder_fun)
  else if (is.function(placeholder_fun))
    placeholder_fun <- list(placeholder_fun)

  lapply(placeholder_fun, test_select_bind_one, con = con, ...)
}

test_select_bind_one <- function(con, placeholder_fun, values,
                             type = "character(10)",
                             transform_input = as.character,
                             transform_output = function(x) trimws(x, "right"),
                             expect = expect_identical,
                             extra = c("none", "return_value", "too_many",
                                       "not_enough", "wrong_name", "repeated")) {
  extra <- match.arg(extra)

  placeholder <- placeholder_fun(length(values))

  value_names <- letters[seq_along(values)]
  if (is.null(type)) {
    typed_placeholder <- placeholder
  } else {
    typed_placeholder <- paste0("cast(", placeholder, " as ", type, ")")
  }
  query <- paste0("SELECT ", paste0(
    typed_placeholder, " as ", value_names, collapse = ", "))
  res <- dbSendQuery(con, query)
  on.exit(expect_error(dbClearResult(res), NA))

  bind_values <- values
  if (!is.null(names(placeholder))) {
    names(bind_values) <- names(placeholder)
  }

  error_bind_values <- switch(
    extra,
    too_many = c(bind_values, bind_values[[1L]]),
    not_enough = bind_values[-1L],
    wrong_name = stats::setNames(bind_values, paste0("bogus",
                                                     names(bind_values))),
    bind_values
  )

  if (!identical(bind_values, error_bind_values)) {
    expect_error(dbBind(res, as.list(error_bind_values)))
    return()
  }

  bind_res <- withVisible(dbBind(res, as.list(bind_values)))
  if (extra == "return_value") {
    expect_false(bind_res$visible)
    expect_identical(res, bind_res$value)
  }

  rows <- dbFetch(res)
  expect(transform_output(Reduce(c, rows)), transform_input(unname(values)))

  if (extra == "repeated") {
    dbBind(res, as.list(bind_values))

    rows <- dbFetch(res)
    expect(transform_output(Reduce(c, rows)), transform_input(unname(values)))
  }
}

#' Create a function that creates n placeholders
#'
#' For internal use by the \code{placeholder_format} tweak.
#'
#' @param pattern \code{[character(1)]}\cr Any character, optionally followed by \code{1} or \code{name}. Examples: \code{"?"}, \code{"$1"}, \code{":name"}
#'
#' @return \code{[function(n)]}\cr A function with one argument \code{n} that
#'   returns a vector of length \code{n} with placeholders of the specified format.
#'   Examples: \code{?, ?, ?, ...}, \code{$1, $2, $3, ...}, \code{:a, :b, :c}
#'
#' @keywords internal
make_placeholder_fun <- function(pattern) {
  format_rx <- "^(.)(.*)$"

  character <- gsub(format_rx, "\\1", pattern)
  kind <- gsub(format_rx, "\\2", pattern)

  if (character == "") {
    stop("placeholder pattern must have at least one character", call. = FALSE)
  }

  if (kind == "") {
    eval(bquote(
      function(n) .(character)
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
