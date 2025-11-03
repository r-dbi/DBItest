# spec_arrow_get_query_arrow

spec_arrow_get_query_arrow

## Value

`dbGetQueryArrow()` always returns an object coercible to a
[data.frame](https://rdrr.io/r/base/data.frame.html), with as many rows
as records were fetched and as many columns as fields in the result set,
even if the result is a single value or has one or zero rows.

## Failure modes

An error is raised when issuing a query over a closed or invalid
connection, if the syntax of the query is invalid, or if the query is
not a non-`NA` string. The object returned by `dbGetQueryArrow()` can
also be passed to
[`nanoarrow::as_nanoarrow_array_stream()`](https://arrow.apache.org/nanoarrow/latest/r/reference/as_nanoarrow_array_stream.html)
to create a nanoarrow array stream object that can be used to read the
result set in batches. The chunk size is implementation-specific.

## Additional arguments

The following arguments are not part of the `dbGetQueryArrow()` generic
(to improve compatibility across backends) but are part of the DBI
specification:

- `params` (default: `NULL`)

- `immediate` (default: `NULL`)

They must be provided as named arguments. See the "Specification" and
"Value" sections for details on their usage.

The `param` argument allows passing query parameters, see
[`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html) for
details.

## Specification for the `immediate` argument

The `immediate` argument supports distinguishing between "direct" and
"prepared" APIs offered by many database drivers. Passing
`immediate = TRUE` leads to immediate execution of the query or
statement, via the "direct" API (if supported by the driver). The
default `NULL` means that the backend should choose whatever API makes
the most sense for the database, and (if relevant) tries the other API
if the first attempt fails. A successful second attempt should result in
a message that suggests passing the correct `immediate` argument.
Examples for possible behaviors:

1.  DBI backend defaults to `immediate = TRUE` internally

    1.  A query without parameters is passed: query is executed

    2.  A query with parameters is passed:

        1.  `params` not given: rejected immediately by the database
            because of a syntax error in the query, the backend tries
            `immediate = FALSE` (and gives a message)

        2.  `params` given: query is executed using `immediate = FALSE`

2.  DBI backend defaults to `immediate = FALSE` internally

    1.  A query without parameters is passed:

        1.  simple query: query is executed

        2.  "special" query (such as setting a config options): fails,
            the backend tries `immediate = TRUE` (and gives a message)

    2.  A query with parameters is passed:

        1.  `params` not given: waiting for parameters via
            [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html)

        2.  `params` given: query is executed

## See also

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
