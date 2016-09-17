spec_meta_bind <- c(
  spec_meta_bind_wrong_name,
  spec_meta_bind_positional_qm,
  spec_meta_bind_positional_dollar,
  spec_meta_bind_named_colon,
  spec_meta_bind_named_dollar,

  NULL
)


# Helpers -----------------------------------------------------------------

test_select_bind <- function(con, placeholder_fun, ...) {
  if (is.function(placeholder_fun))
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

positional_qm <- function(n) {
  "?"
}

positional_dollar <- function(n) {
  paste0("$", seq_len(n))
}

named_dollar <- function(n) {
  l <- letters[seq_len(n)]
  stats::setNames(paste0("$", l), l)
}

named_colon <- function(n) {
  l <- letters[seq_len(n)]
  stats::setNames(paste0(":", l), l)
}
