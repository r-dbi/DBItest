#' @template dbispec
#' @format NULL
spec_meta <- c(
  spec_meta_is_valid_connection,
  spec_meta_is_valid_result,
  spec_meta_get_statement,
  spec_meta_column_info,
  spec_meta_get_row_count,
  spec_meta_get_rows_affected,
  spec_meta_get_info_result,
  spec_meta_bind,
  spec_meta_bind_multi_row,

  # dbHasCompleted tested in test_result

  # no 64-bit or time input data type yet

  NULL
)
