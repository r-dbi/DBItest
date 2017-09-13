#' @template dbispec
#' @format NULL
spec_result <- c(
  spec_result_send_query,
  spec_result_fetch,
  spec_result_clear_result,
  spec_result_get_query,
  spec_result_send_statement,
  spec_result_execute,
  spec_result_create_table_with_data_type,
  spec_result_roundtrip
)


# Helpers -----------------------------------------------------------------

union <- function(..., .order_by = NULL, .ctx) {
  query <- .ctx$tweaks$union(c(...))

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}

trivial_statement <- function(table_name = "test") {
  paste0("CREATE TABLE ", table_name, " AS ", trivial_query())
}

trivial_query <- function() {
  "SELECT 1 AS a"
}
