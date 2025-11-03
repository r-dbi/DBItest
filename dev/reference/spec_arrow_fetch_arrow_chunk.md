# spec_arrow_fetch_arrow_chunk

spec_arrow_fetch_arrow_chunk

## Value

`dbFetchArrowChunk()` always returns an object coercible to a
[data.frame](https://rdrr.io/r/base/data.frame.html) with as many rows
as records were fetched and as many columns as fields in the result set,
even if the result is a single value or has one or zero rows.

## Failure modes

An attempt to fetch from a closed result set raises an error.

## Specification

Fetching multi-row queries with one or more columns returns the next
chunk. The size of the chunk is implementation-specific. The object
returned by `dbFetchArrowChunk()` can also be passed to
[`nanoarrow::as_nanoarrow_array()`](https://arrow.apache.org/nanoarrow/latest/r/reference/as_nanoarrow_array.html)
to create a nanoarrow array object. The chunk size is
implementation-specific.

## See also

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
