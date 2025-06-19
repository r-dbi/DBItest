expect_arglist_is_empty <- function(object) {
  act <- quasi_label(enquo(object), arg = "object")
  act$formals <- formals(act$val)
  expect(
    is.null(act$formals),
    sprintf("%s has an empty argument list.", act$lab)
  )

  invisible(act$val)
}

expect_all_args_have_default_values <- function(object) {
  act <- quasi_label(enquo(object), arg = "object")
  act$args <- formals(act$val)
  act$args <- act$args[names(act$args) != "..."]
  act$char_args <- purrr::map_chr(act$args, as.character)
  expect(
    all(nzchar(act$char_args, keepNA = FALSE)),
    sprintf("%s has arguments without default values", act$lab)
  )

  invisible(act$val)
}

has_method <- function(method_name) {
  function(x) {
    my_class <- class(x)
    # nolint next: expect_comparison_linter. Using 'info', absent from expect_gt().
    expect_true(
      length(findMethod(method_name, my_class)) > 0L,
      paste("object of class", my_class, "has no", method_name, "method")
    )
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
  expect_false(ret$visible)

  invisible(ret$value)
}

# Custom expectation for comparing data frames with DBI-specific handling.
# This function performs comprehensive data frame comparison with type coercion.
# It handles factors, AsIs columns, list columns, and row ordering for accurate comparisons.
# The function ensures consistent comparison behavior across different DBI implementations.
expect_equal_df <- function(actual, expected) {
  factor_cols <- purrr::map_lgl(expected, is.factor)
  expected[factor_cols] <- purrr::map(expected[factor_cols], as.character)

  asis_cols <- purrr::map_lgl(expected, inherits, "AsIs")
  expected[asis_cols] <- purrr::map(expected[asis_cols], unclass)

  list_cols <- purrr::map_lgl(expected, is.list)

  if (any(list_cols)) {
    expect_false(all(list_cols))
    expect_equal(anyDuplicated(actual[!list_cols]), 0)
    expect_equal(anyDuplicated(expected[!list_cols]), 0)
    order_actual <- do.call(order, actual[!list_cols])
    order_expected <- do.call(order, expected[!list_cols])
  } else {
    order_actual <- do.call(order, actual)
    order_expected <- do.call(order, expected)
  }

  has_rownames_actual <- is.character(attr(actual, "row.names"))
  has_rownames_expected <- is.character(attr(expected, "row.names"))
  expect_equal(has_rownames_actual, has_rownames_expected)

  if (has_rownames_actual) {
    expect_equal(sort(row.names(actual)), sort(row.names(expected)))
  }

  actual <- unrowname(actual[order_actual, ])
  expected <- unrowname(expected[order_expected, ])

  expect_identical(actual, expected)
}

# Custom expectation for comparing Arrow data structures in DBI Arrow tests.
# This function converts Arrow objects to data frames before comparison.
# It leverages expect_equal_df to ensure consistent comparison behavior.
# Used specifically for testing DBI Arrow interface implementations.
expect_equal_arrow <- function(actual, expected) {
  expect_equal_df(as.data.frame(actual), as.data.frame(expected))
}

# Conditionally skips tests based on DBItest version requirements.
# This function allows tests to specify minimum DBItest version requirements.
# It compares the context's DBItest version against the required version.
# Tests are skipped with descriptive messages when version requirements aren't met.
skip_if_not_dbitest <- function(ctx, version) {
  if (as.package_version(ctx$tweaks$dbitest_version) < version) {
    skip(paste0("tweak: dbitest_version: required: ", version, ", available: ", ctx$tweaks$dbitest_version))
  }
}
