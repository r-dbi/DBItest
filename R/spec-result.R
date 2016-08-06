#' @template dbispec
#' @format NULL
spec_result <- c(
  spec_result_send_query,
  spec_result_fetch,
  spec_result_get_query,
  spec_result_create_table_with_data_type,
  spec_result_roundtrip
)


# Helpers -----------------------------------------------------------------

union <- function(..., .order_by = NULL, .ctx) {
  if (is.null(.ctx$tweaks$union)) {
    query <- paste(c(...), collapse = " UNION ")
  } else {
    query <- .ctx$tweaks$union(c(...))
  }

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}
