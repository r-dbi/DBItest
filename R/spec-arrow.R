#' @format NULL
spec_arrow <- c(
  spec_arrow_send_query_stream,
  spec_arrow_stream,
  spec_arrow_get_stream,
  spec_arrow_stream_table,
  spec_arrow_write_stream,
  spec_arrow_create_from_stream,
  spec_arrow_append_stream,
  spec_arrow_bind,
  spec_arrow_roundtrip,
  #
  NULL
)

stream_frame <- function(..., .select = NULL) {
  data <- data.frame(...)
  out <- arrow::as_record_batch_reader(data)

  if (!is.null(.select)) {
    out <-
      out %>%
      dplyr::select({{ .select }})
  }

  out
}
