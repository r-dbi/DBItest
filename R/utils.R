get_pkg_path <- function(ctx) {
  pkg_name <- package_name(ctx)
  expect_type(pkg_name, "character")

  pkg_path <- find.package(pkg_name)
  pkg_path
}

utils::globalVariables("con")
utils::globalVariables("con2")

local_connection <- function(ctx, ..., .local_envir = parent.frame()) {
  con <- connect(ctx, ...)
  withr::local_db_connection(con, .local_envir = .local_envir)
}

local_closed_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  con
}

local_invalid_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  unserialize(serialize(con, NULL))
}

# Calls `dbClearResult()` on `query` after exiting `frame`.
local_result <- function(query, frame = caller_env()) {
  res <- query
  withr::defer(
    {
      dbClearResult(res)
    },
    envir = frame
  )
  res
}

# Calls `try_silent(dbRemoveTable())` after exiting `frame`.
local_remove_test_table <- function(con, name, frame = caller_env()) {
  table_name <- dbQuoteIdentifier(con, name)
  withr::defer(
    try_silent(
      dbRemoveTable(con, table_name)
    ),
    envir = frame
  )
}

get_penguins <- function(ctx) {
  datasets_penguins <- unrowname(palmerpenguins::penguins[c(1, 153, 277), ])
  # FIXME: better handling of DBI backends that do support factors
  datasets_penguins$species <- as.character(datasets_penguins$species)
  datasets_penguins$island <- as.character(datasets_penguins$island)
  datasets_penguins$sex <- as.character(datasets_penguins$sex)
  as.data.frame(datasets_penguins)
}

unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

random_table_name <- function(n = 10) {
  # FIXME: Use parallel-safe sequence of numbers
  paste0("dbit", paste(sample(letters, n, replace = TRUE), collapse = ""))
}

compact <- function(x) {
  x[!vapply(x, is.null, logical(1L))]
}


try_silent <- function(code) {
  tryCatch(
    code,
    error = function(e) NULL
  )
}

check_df <- function(df) {
  expect_s3_class(df, "data.frame")
  if (length(df) >= 1L) {
    lengths <- vapply(df, length, integer(1L), USE.NAMES = FALSE)
    expect_equal(diff(lengths), rep(0L, length(lengths) - 1L))
    expect_equal(nrow(df), lengths[[1]])
  }

  df_names <- names(df)
  expect_true(all(df_names != ""))
  expect_false(anyNA(df_names))

  df
}

check_arrow <- function(stream) {
  check_df(as.data.frame(stream))
}
