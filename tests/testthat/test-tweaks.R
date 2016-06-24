context("tweaks")

test_that("multiplication works", {
  expect_true("..." %in% names(formals(tweaks)))
  expect_warning(tweaks(`_oooops` = 42, `_darn` = -1), "_oooops, _darn")
  expect_warning(tweaks(), NA)
  expect_warning(tweaks(constructor_name = "constr"), NA)
})
