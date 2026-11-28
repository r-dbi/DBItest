#' @format NULL
spec_arrow <- c(
  spec_arrow_send_query_arrow,
  spec_arrow_fetch_arrow,
  spec_arrow_fetch_arrow_chunk,
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

utils::globalVariables("select")

stream_frame <- function(..., .select = NULL) {
  data <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  as_is <- map_lgl(data, inherits, "AsIs")
  data[as_is] <- map(data[as_is], function(.x) {
    class(.x) <- setdiff(class(.x), "AsIs")
    .x
  })

  if (!is.null(.select)) {
    data <- data[.select]
  }

  out <- nanoarrow::as_nanoarrow_array_stream(data)

  out
}
