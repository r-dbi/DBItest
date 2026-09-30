skip_if_not_installed("bit64")
skip_if_not_installed("RSQLite")

make_integer64_logical_context <- function(logical_return) {
  DBItest::make_context(
    RSQLite::SQLite(),
    list(
      dbname = tempfile("DBItest", fileext = ".sqlite"),
      bigint = "integer64"
    ),
    tweaks = DBItest::tweaks(logical_return = logical_return)
  )
}

test_that("data_logical accepts integer64 vector tweaks", {
  on.exit(DBItest::set_default_context(NULL), add = TRUE)

  ctx <- make_integer64_logical_context(function(x) bit64::as.integer64(x))

  expect_no_error(DBItest::test_some("data_logical", ctx = ctx))
})

test_that("data_logical accepts integer64 list tweaks", {
  on.exit(DBItest::set_default_context(NULL), add = TRUE)

  ctx <- make_integer64_logical_context(function(x) lapply(x, bit64::as.integer64))

  expect_no_error(DBItest::test_some("data_logical", ctx = ctx))
})
