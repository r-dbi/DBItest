# spec_meta_get_rows_affected

spec_meta_get_rows_affected

## Value

`dbGetRowsAffected()` returns a scalar number (integer or numeric), the
number of rows affected by a data manipulation statement issued with
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html).
The value is available directly after the call and does not change after
calling
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html).
`NA_integer_` or `NA_numeric_` are allowed if the number of rows
affected is not known.

For queries issued with
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
zero is returned before and after the call to `dbFetch()`. `NA` values
are not allowed.

## Failure modes

Attempting to get the rows affected for a result set cleared with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
gives an error.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
