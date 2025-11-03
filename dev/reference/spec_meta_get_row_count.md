# spec_meta_get_row_count

spec_meta_get_row_count

## Value

`dbGetRowCount()` returns a scalar number (integer or numeric), the
number of rows fetched so far. After calling
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
the row count is initially zero. After a call to
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html) without
limit, the row count matches the total number of rows returned. Fetching
a limited number of rows increases the number of rows by the number of
rows returned, even if fetching past the end of the result set. For
queries with an empty result set, zero is returned even after fetching.
For data manipulation statements issued with
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html),
zero is returned before and after calling `dbFetch()`.

## Failure modes

Attempting to get the row count for a result set cleared with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
gives an error.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
