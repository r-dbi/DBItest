#' @name test_all
#' @section Tests:
#' \code{\link{test_sql}}:
#' Test SQL methods
NULL

#' Test SQL methods
#'
#' @inheritParams test_all
#' @include test-result.R
#' @family tests
#' @export
test_sql <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "SQL"

  run_tests(ctx, spec_sql, skip, test_suite)
}

get_iris <- function(ctx) {
  datasets_iris <- datasets::iris
  if (isTRUE(ctx$tweaks$strict_identifier)) {
    names(datasets_iris) <- gsub(".", "_", names(datasets_iris), fixed = TRUE)
  }
  datasets_iris
}

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

all_have_utf8_or_ascii_encoding <- function(x) {
  all(vapply(x, has_utf8_or_ascii_encoding, logical(1L)))
}
