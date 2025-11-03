# spec_driver_get_info

spec_driver_get_info

spec_connection_get_info

spec_meta_get_info_result

## Value

For objects of class
[DBI::DBIDriver](https://dbi.r-dbi.org/reference/DBIDriver-class.html),
`dbGetInfo()` returns a named list that contains at least the following
components:

- `driver.version`: the package version of the DBI backend,

- `client.version`: the version of the DBMS client library.

For objects of class
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html),
`dbGetInfo()` returns a named list that contains at least the following
components:

- `db.version`: version of the database server,

- `dbname`: database name,

- `username`: username to connect to the database,

- `host`: hostname of the database server,

- `port`: port on the database server. It must not contain a `password`
  component. Components that are not applicable should be set to `NA`.

For objects of class
[DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html),
`dbGetInfo()` returns a named list that contains at least the following
components:

- `statatment`: the statement used with
  [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  or
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html),
  as returned by
  [`DBI::dbGetStatement()`](https://dbi.r-dbi.org/reference/dbGetStatement.html),

- `row.count`: the number of rows fetched so far (for queries), as
  returned by
  [`DBI::dbGetRowCount()`](https://dbi.r-dbi.org/reference/dbGetRowCount.html),

- `rows.affected`: the number of rows affected (for statements), as
  returned by
  [`DBI::dbGetRowsAffected()`](https://dbi.r-dbi.org/reference/dbGetRowsAffected.html)

- `has.completed`: a logical that indicates if the query or statement
  has completed, as returned by
  [`DBI::dbHasCompleted()`](https://dbi.r-dbi.org/reference/dbHasCompleted.html).

## See also

Other driver specifications:
[`spec_driver_connect`](https://dbitest.r-dbi.org/dev/reference/spec_driver_connect.md),
[`spec_driver_constructor`](https://dbitest.r-dbi.org/dev/reference/spec_driver_constructor.md),
[`spec_driver_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_driver_data_type.md)

Other connection specifications:
[`spec_connection_disconnect`](https://dbitest.r-dbi.org/dev/reference/spec_connection_disconnect.md)

Other meta specifications:
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
