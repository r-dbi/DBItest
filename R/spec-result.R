#' @format NULL
spec_result <- c(
  spec_result_send_query,
  spec_result_fetch,
  spec_result_clear_result,
  spec_result_get_query,
  spec_result_send_statement,
  spec_result_execute,
  spec_result_create_table_with_data_type,
  spec_result_roundtrip,
  #
  NULL
)


# Helpers -----------------------------------------------------------------

sql_union <- function(..., .order_by = NULL, .ctx) {
  query <- .ctx$tweaks$union(c(...))

  if (!missing(.order_by)) {
    query <- paste(query, "ORDER BY", .order_by)
  }
  query
}

trivial_statement <- function(ctx, table_name) {
  ctx$tweaks$create_table_as(table_name, trivial_query())
}

trivial_query <- function(n = 1L, column = "a", .order_by = NULL, .ctx = NULL) {
  value <- trivial_values(n)
  if (length(column) == n) {
    query <- paste0("SELECT ", paste0(value, " AS ", column, collapse = ", "))
  } else {
    query <- sql_union(
      paste0("SELECT ", value, " AS ", column),
      .order_by = .order_by,
      .ctx = .ctx
    )
  }

  query
}

trivial_values <- function(n = 1L) {
  seq_len(n) + 0.5
}

trivial_df <- function(n = 1L, column = "a") {
  values <- trivial_values(n)
  if (length(column) == 1) {
    df <- data.frame(a = values)
  } else {
    df <- as.data.frame(as.list(values))
  }
  names(df) <- column
  df
}
