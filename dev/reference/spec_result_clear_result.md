# spec_result_clear_result

spec_result_clear_result

## Value

`dbClearResult()` returns `TRUE`, invisibly, for result sets obtained
from `dbSendQuery()`, `dbSendStatement()`, or `dbSendQueryArrow()`,

## Failure modes

An attempt to close an already closed result set issues a warning for
`dbSendQuery()`, `dbSendStatement()`, and `dbSendQueryArrow()`,

## Specification

`dbClearResult()` frees all resources associated with retrieving the
result of a query or update operation. The DBI backend can expect a call
to `dbClearResult()` for each
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
or
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html)
call.

## See also

Other result specifications:
[`spec_result_create_table_with_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_result_create_table_with_data_type.md),
[`spec_result_execute`](https://dbitest.r-dbi.org/dev/reference/spec_result_execute.md),
[`spec_result_fetch`](https://dbitest.r-dbi.org/dev/reference/spec_result_fetch.md),
[`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md),
[`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md),
[`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md)
