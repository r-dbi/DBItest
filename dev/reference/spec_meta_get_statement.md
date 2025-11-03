# spec_meta_get_statement

spec_meta_get_statement

## Value

`dbGetStatement()` returns a string, the query used in either
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
or
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html).

## Failure modes

Attempting to query the statement for a result set cleared with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
gives an error.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
