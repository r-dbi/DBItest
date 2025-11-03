# spec_arrow_fetch_arrow

spec_arrow_fetch_arrow

## Value

`dbFetchArrow()` always returns an object coercible to a
[data.frame](https://rdrr.io/r/base/data.frame.html) with as many rows
as records were fetched and as many columns as fields in the result set,
even if the result is a single value or has one or zero rows.

## Failure modes

An attempt to fetch from a closed result set raises an error.

## Specification

Fetching multi-row queries with one or more columns by default returns
the entire result. The object returned by `dbFetchArrow()` can also be
passed to
[`nanoarrow::as_nanoarrow_array_stream()`](https://arrow.apache.org/nanoarrow/latest/r/reference/as_nanoarrow_array_stream.html)
to create a nanoarrow array stream object that can be used to read the
result set in batches. The chunk size is implementation-specific.

## See also

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
