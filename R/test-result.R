#' @name test_all
#' @section Tests:
#' \code{\link{test_result}}:
#' Test the "Result" class
NULL

#' Test the "Result" class
#'
#' @inheritParams test_all
#' @include test-connection.R
#' @family tests
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  run_tests(ctx, spec_result, skip, test_suite)
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
    on.exit(expect_error(dbDisconnect(.(con)), NA), add = TRUE)
    local(.(code_sub))
  }
  ), envir = env)
}

# Helper function
union <- function(..., .order_by = NULL, .ctx) {
  if (is.null(.ctx$tweaks$union)) {
    query <- paste(c(...), collapse = " UNION ")
  } else {
    query <- .ctx$tweaks$union(c(...))
  }

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}

# NB: .table = TRUE will not work in bigrquery
test_select <- function(con, ..., .dots = NULL, .add_null = "none",
                        .table = FALSE, .ctx, .envir = parent.frame()) {
  values <- c(list(...), .dots)

  value_is_formula <- vapply(values, is.call, logical(1L))
  names(values)[value_is_formula] <- lapply(values[value_is_formula], "[[", 2L)
  values[value_is_formula] <- lapply(
    values[value_is_formula],
    function(x) {
      eval(x[[3]], envir = .envir)
    }
  )

  if (is.null(names(values))) {
    sql_values <- lapply(values, as.character)
  } else {
    sql_values <- names(values)
  }

  if (isTRUE(.ctx$tweaks$current_needs_parens)) {
    sql_values <- gsub("^(current_(?:date|time|timestamp))$", "\\1()",
                       sql_values)
  }

  sql_names <- letters[seq_along(sql_values)]

  query <- paste("SELECT",
                 paste(sql_values, "as", sql_names, collapse = ", "))
  if (.add_null != "none") {
    query_null <- paste("SELECT",
                        paste("NULL as", sql_names, collapse = ", "))
    query <- c(query, query_null)
    if (.add_null == "above") {
      query <- rev(query)
    }
    query <- paste0(query, ", ", 1:2, " as id")
    query <- union(.ctx = .ctx, query)
  }

  if (.table) {
    query <- paste("CREATE TABLE test AS", query)
    expect_warning(dbGetQuery(con, query), NA)
    on.exit(expect_error(dbGetQuery(con, "DROP TABLE test"), NA), add = TRUE)
    expect_warning(rows <- dbReadTable(con, "test"), NA)
  } else {
    expect_warning(rows <- dbGetQuery(con, query), NA)
  }

  if (.add_null != "none") {
    rows <- rows[order(rows$id), -(length(sql_names) + 1L)]
    if (.add_null == "above") {
      rows <- rows[2:1, ]
    }
  }

  expect_identical(names(rows), sql_names)

  for (i in seq_along(values)) {
    value_or_testfun <- values[[i]]
    if (is.function(value_or_testfun)) {
      eval(bquote(expect_true(value_or_testfun(rows[1L, .(i)]))))
    } else {
      eval(bquote(expect_identical(rows[1L, .(i)], .(value_or_testfun))))
    }
  }

  if (.add_null != "none") {
    expect_equal(nrow(rows), 2L)
    expect_true(all(is.na(unname(unlist(rows[2L, ])))))
  } else {
    expect_equal(nrow(rows), 1L)
  }
}

has_utf8_or_ascii_encoding <- function(x) {
  if (Encoding(x) == "UTF-8")
    TRUE
  else if (Encoding(x) == "unknown") {
    # Characters encoded as "unknown" must be ASCII only, and remain "unknown"
    # after attempting to assign an encoding. From ?Encoding :
    # > ASCII strings will never be marked with a declared encoding, since their
    # > representation is the same in all supported encodings.
    Encoding(x) <- "UTF-8"
    Encoding(x) == "unknown"
  } else
    FALSE
}

is_raw_list <- function(x) {
  is.list(x) && is.raw(x[[1L]])
}

is_time <- function(x) {
  inherits(x, "POSIXct")
}

is_roughly_current_time <- function(x) {
  is_time(x) && (Sys.time() - x <= 2)
}

as_integer_date <- function(d) {
  d <- as.Date(d)
  structure(as.integer(unclass(d)), class = class(d))
}
