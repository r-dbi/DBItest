expect_success <- function (object, ..., info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, not(throws_error(...)), info = info, label = label)
}

all_formals_have_default_values <- function() {
  function(x) {
    args <- formals(x)
    args <- args[names(args) != "..."]
    expectation(
      all(vapply(args, as.character, character(1L)) != ""),
      "has arguments without default values",
      "has only arguments with default values")
  }
}
