#' @template dbispec
spec_meta <- list(
  spec_meta_is_valid_connection,
  spec_meta_is_valid_result,
  spec_meta_get_statement,
  spec_meta_column_info,
  spec_meta_get_row_count,
  spec_meta_get_rows_affected,
  spec_meta_get_info_result,
  spec_meta_bind_positional_qm,
  spec_meta_bind_positional_dollar,
  spec_meta_bind_named_colon,
  spec_meta_bind_named_dollar,

  # dbHasCompleted tested in test_result

  # no 64-bit or time input data type yet

  NULL
)
