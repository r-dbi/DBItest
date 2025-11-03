# spec_arrow_read_table_arrow

spec_arrow_read_table_arrow

## Value

`dbReadTableArrow()` returns an Arrow object that contains the complete
data from the remote table, effectively the result of calling
[`DBI::dbGetQueryArrow()`](https://dbi.r-dbi.org/reference/dbGetQueryArrow.html)
with `SELECT * FROM <name>`.

An empty table is returned as an Arrow object with zero rows.

## Failure modes

An error is raised if the table does not exist.

An error is raised when calling this method for a closed or invalid
connection. An error is raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar.

## Specification

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbReadTableArrow()` will do the
  quoting, perhaps by calling `dbQuoteIdentifier(conn, x = name)`

- If the result of a call to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html):
  no more quoting is done

## See also

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
