#' @format NULL
spec_arrow <- c(
  spec_arrow_send_query_arrow,
  spec_arrow_fetch_arrow,
  spec_arrow_get_query_arrow,
  spec_arrow_read_table_arrow,
  spec_arrow_write_table_arrow,
  spec_arrow_create_table_arrow,
  spec_arrow_append_table_arrow,
  spec_arrow_bind,
  spec_arrow_roundtrip,
  #
  NULL
)

stream_frame <- function(..., .select = NULL) {
  data <- data.frame(..., stringsAsFactors = FALSE)
  out <- arrow::as_record_batch_reader(data)

  if (!is.null(.select)) {
    out <-
      out %>%
      dplyr::select({{ .select }})
  }

  out
}
