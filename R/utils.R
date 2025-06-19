# Get the installation path for a package from test context
#
# Extracts the package name from the test context and locates its installation
# directory. Used for accessing package resources during testing.
#
# @param ctx DBItest_context object
# @return Character string containing the package path
get_pkg_path <- function(ctx) {
  pkg_name <- package_name(ctx)
  expect_type(pkg_name, "character")

  pkg_path <- find.package(pkg_name)
  pkg_path
}

utils::globalVariables("con")
utils::globalVariables("con2")

# Create a database connection that automatically closes when leaving scope
#
# Creates a database connection using the provided context and ensures it's
# automatically closed when exiting the current scope. Built on withr's
# local connection management for reliable resource cleanup in tests.
#
# @param ctx DBItest_context object
# @param ... Additional arguments passed to dbConnect()
# @param .local_envir Environment for local resource management
# @return Database connection object
local_connection <- function(ctx, ..., .local_envir = parent.frame()) {
  con <- connect(ctx, ...)
  withr::local_db_connection(con, .local_envir = .local_envir)
}

# Create a closed database connection for testing error conditions
#
# Creates a database connection, immediately closes it, and returns the closed
# connection object. Used to test how DBI methods handle closed connections.
#
# @param ctx DBItest_context object
# @param ... Additional arguments passed to dbConnect()
# @return Closed database connection object
local_closed_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  con
}

# Create an invalid database connection for testing error conditions  
#
# Creates a database connection, closes it, then serializes and deserializes
# it to create an invalid connection object. Used to test how DBI methods
# handle corrupted or invalid connection objects.
#
# @param ctx DBItest_context object
# @param ... Additional arguments passed to dbConnect()
# @return Invalid database connection object
local_invalid_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  unserialize(serialize(con, NULL))
}

# Calls `dbClearResult()` on `query` after exiting `frame`.
local_result <- function(query, frame = caller_env()) {
  res <- query
  withr::defer( # nolint next: unnecessary_nesting_linter. The braces ensure the srcref.
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

# Generate a random table name for testing
#
# Creates a random table name with 'dbit' prefix followed by random letters.
# Used to create unique table names during testing to avoid conflicts between
# parallel test runs or leftover tables from previous runs.
#
# @param n Number of random letters to append (default: 10)
# @return Character string containing random table name
random_table_name <- function(n = 10) {
  # FIXME: Use parallel-safe sequence of numbers
  paste0("dbit", paste(sample(letters, n, replace = TRUE), collapse = ""))
}

# Execute code silently, suppressing errors
#
# Wraps code execution in a try-catch block that returns NULL if an error
# occurs. Used for cleanup operations and optional functionality where
# failures should not interrupt the main test flow.
#
# @param code Expression to execute
# @return Result of code execution, or NULL if an error occurred
try_silent <- function(code) {
  tryCatch(
    code,
    error = function(e) NULL
  )
}

# Validate data frame structure and properties
#
# Performs comprehensive validation of a data frame object, checking for
# proper S3 class, consistent column lengths, valid row count, and proper
# column naming. Used throughout tests to ensure data integrity.
#
# @param df Data frame object to validate
# @return The validated data frame (for chaining)
check_df <- function(df) {
  expect_s3_class(df, "data.frame")
  if (length(df) >= 1L) {
    lengths <- unname(lengths(df))
    expect_equal(diff(lengths), rep(0L, length(lengths) - 1L))
    expect_equal(nrow(df), lengths[[1]])
  }

  df_names <- names(df)
  expect_true(all(df_names != ""))
  expect_false(anyNA(df_names))

  df
}

# Validate Arrow data structures and convert to data frame
#
# Handles conversion of Apache Arrow array streams or arrays to data frames
# with optional data transformation. Ensures proper resource cleanup for
# streams and validates the resulting data frame structure.
#
# @param stream Arrow array or array stream object
# @param transform Function to apply during conversion (default: identity)
# @return Validated data frame converted from Arrow format
check_arrow <- function(stream, transform = identity) {
  to <- function(schema, ptype) transform(ptype)
  if (inherits(stream, "nanoarrow_array_stream")) {
    on.exit(stream$release())
    df <- nanoarrow::convert_array_stream(stream, to)
  } else if (inherits(stream, "nanoarrow_array")) {
    df <- nanoarrow::convert_array(stream, to)
  } else {
    stop("Unexpected conversion of type ", class(stream), ".", call. = FALSE)
  }

  check_df(df)
}
