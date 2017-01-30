arglist_is_empty <- function() {
  function(x) {
    expect_true(
      is.null(formals(x)),
      "has empty argument list")
  }
}

all_args_have_default_values <- function() {
  function(x) {
    args <- formals(x)
    args <- args[names(args) != "..."]
    expect_true(
      all(vapply(args, as.character, character(1L)) != ""),
      "has arguments without default values")
  }
}

has_method <- function(method_name) {
  function(x) {
    my_class <- class(x)
    expect_true(
      length(findMethod(method_name, my_class)) > 0L,
      paste("object of class", my_class, "has no", method_name, "method"))
  }
}

expect_visible <- function(code) {
  ret <- withVisible(code)
  expect_true(ret$visible)
  ret$value
}

expect_invisible_true <- function(code) {
  ret <- withVisible(code)
  expect_true(ret$value)

  test_that("Visibility", {
    skip("Cannot test for visibility of return value yet (#89)")
    expect_false(ret$visible)
  })

  invisible(ret$value)
}

expect_equal_df <- function(actual, expected) {
  factor_cols <- vapply(expected, is.factor, logical(1L))
  expected[factor_cols] <- lapply(expected[factor_cols], as.character)

  order_actual <- do.call(order, actual)
  order_expected <- do.call(order, expected)

  has_rownames_actual <- is.character(attr(actual, "row.names"))
  has_rownames_expected <- is.character(attr(expected, "row.names"))
  expect_equal(has_rownames_actual, has_rownames_expected)

  if (has_rownames_actual) {
    order_actual <- unrowname(order_actual)
    order_expected <- unrowname(order_expected)
  }

  expect_identical(unrowname(actual[order_actual, ]), unrowname(expected[order_expected, ]))
}
