# spec_meta_is_valid

spec_meta_is_valid

## Value

`dbIsValid()` returns a logical scalar, `TRUE` if the object specified
by `dbObj` is valid, `FALSE` otherwise. A
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
object is initially valid, and becomes invalid after disconnecting with
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html).
For an invalid connection object (e.g., for some drivers if the object
is saved to a file and then restored), the method also returns `FALSE`.
A [DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html)
object is valid after a call to
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
and stays valid even after all rows have been fetched; only clearing it
with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
invalidates it. A
[DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html)
object is also valid after a call to
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html),
and stays valid after querying the number of rows affected; only
clearing it with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
invalidates it. If the connection to the database system is dropped
(e.g., due to connectivity problems, server failure, etc.),
`dbIsValid()` should return `FALSE`. This is not tested automatically.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md)
