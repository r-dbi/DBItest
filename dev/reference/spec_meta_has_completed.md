# spec_meta_has_completed

spec_meta_has_completed

## Value

`dbHasCompleted()` returns a logical scalar. For a query initiated by
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
with non-empty result set, `dbHasCompleted()` returns `FALSE` initially
and `TRUE` after calling
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html) without
limit. For a query initiated by
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html),
`dbHasCompleted()` always returns `TRUE`.

## Failure modes

Attempting to query completion status for a result set cleared with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
gives an error.

## Specification

The completion status for a query is only guaranteed to be set to
`FALSE` after attempting to fetch past the end of the entire result.
Therefore, for a query with an empty result set, the initial return
value is unspecified, but the result value is `TRUE` after trying to
fetch only one row.

Similarly, for a query with a result set of length n, the return value
is unspecified after fetching n rows, but the result value is `TRUE`
after trying to fetch only one more row.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
