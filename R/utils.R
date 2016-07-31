`%||%` <- function(a, b) if (is.null(a)) b else a

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
