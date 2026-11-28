test_that("expect_all_args_have_default_values() accepts any default value", {
  expect_success(expect_all_args_have_default_values(function() NULL))
  expect_success(expect_all_args_have_default_values(function(...) NULL))
  expect_success(expect_all_args_have_default_values(
    function(x = "", y = NULL, z = character(), w = c("a", "b"), ...) NULL
  ))
  expect_success(expect_all_args_have_default_values(
    function(x = NA, y = x, z = f()) NULL
  ))
})

test_that("expect_all_args_have_default_values() rejects missing defaults", {
  expect_failure(
    expect_all_args_have_default_values(function(x) NULL),
    "has arguments without default values"
  )
  # nolint next: function_argument_linter. The missing default is the point.
  expect_failure(expect_all_args_have_default_values(function(x = "", y) NULL))
  expect_failure(expect_all_args_have_default_values(function(x, ...) NULL))
  expect_failure(expect_all_args_have_default_values(function(..., x) NULL))
})
