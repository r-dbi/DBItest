get_pkg_path <- function(ctx) {
  pkg_name <- package_name(ctx)
  expect_type(pkg_name, "character")

  pkg_path <- find.package(pkg_name)
  pkg_path
}

utils::globalVariables("con")
utils::globalVariables("con2")

# Creates a database connection that is automatically cleaned up when the local environment exits.
# This function establishes a connection using the provided context and additional arguments.
# It leverages withr::local_db_connection to ensure proper cleanup and resource management.
# The connection is automatically closed when the calling function or test completes.
local_connection <- function(ctx, ..., .local_envir = parent.frame()) {
  con <- connect(ctx, ...)
  withr::local_db_connection(con, .local_envir = .local_envir)
}

# Creates a database connection that is immediately closed for testing invalid connection scenarios.
# This function establishes a connection and then disconnects it before returning.
# It's used in tests that need to verify behavior with closed database connections.
# The returned connection object is in a disconnected state but retains its structure.
local_closed_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  con
}

# Creates an invalid connection object for testing error handling with corrupted connections.
# This function creates a connection, closes it, then serializes and deserializes it.
# The serialization process corrupts internal connection state making it truly invalid.
# Used to test how DBI implementations handle severely corrupted connection objects.
local_invalid_connection <- function(ctx, ...) {
  con <- connect(ctx, ...)
  dbDisconnect(con)
  suppressWarnings(unserialize(serialize(con, NULL)))
}

# Calls `dbClearResult()` on `query` after exiting `frame`.
# Ensures automatic cleanup of database result sets when exiting the specified frame.
# This function registers a deferred cleanup action to call dbClearResult on the query result.
# It prevents resource leaks by guaranteeing result set cleanup even if tests fail.
# The cleanup is tied to the calling frame's lifecycle for proper scope management.
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
# Registers automatic cleanup to remove test tables when exiting the specified frame.
# This function ensures test tables are properly cleaned up after test execution.
# It uses deferred execution to attempt table removal even if the test fails.
# The cleanup is wrapped in try_silent to handle cases where the table doesn't exist.
local_remove_test_table <- function(con, name, frame = caller_env()) {
  table_name <- dbQuoteIdentifier(con, name)
  withr::defer(
    try_silent(
      dbRemoveTable(con, table_name)
    ),
    envir = frame
  )
}

# Provides a standardized test dataset based on the Palmer Penguins data.
# This function selects specific rows from the penguins dataset for consistent testing.
# It converts factor columns to character strings for better DBI compatibility.
# Returns a data.frame suitable for testing database operations and data roundtrips.
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

# Generates unique random table names for test isolation and cleanup.
# This function creates table names with a 'dbit' prefix followed by random letters.
# It helps prevent test interference by ensuring each test uses unique table names.
# The randomization reduces the risk of conflicts in concurrent test execution.
random_table_name <- function(n = 10) {
  # FIXME: Use parallel-safe sequence of numbers
  paste0("dbit", paste(sample(letters, n, replace = TRUE), collapse = ""))
}

# Executes code and silently handles any errors by returning NULL.
# This function is used for cleanup operations that may fail safely.
# It prevents error propagation during resource cleanup in test teardown.
# Returns the result of successful execution or NULL if an error occurs.
try_silent <- function(code) {
  tryCatch(
    code,
    error = function(e) NULL
  )
}

# Validates data frame structure and properties for DBI compliance testing.
# This function performs comprehensive checks on data frame consistency.
# It verifies column lengths are equal, row count matches, and column names are valid.
# Returns the validated data frame or throws descriptive errors for invalid structures.
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
