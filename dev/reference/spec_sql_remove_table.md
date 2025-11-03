# spec_sql_remove_table

spec_sql_remove_table

## Value

`dbRemoveTable()` returns `TRUE`, invisibly.

## Failure modes

If the table does not exist, an error is raised. An attempt to remove a
view with this function may result in an error.

An error is raised when calling this method for a closed or invalid
connection. An error is also raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar.

## Additional arguments

The following arguments are not part of the `dbRemoveTable()` generic
(to improve compatibility across backends) but are part of the DBI
specification:

- `temporary` (default: `FALSE`)

- `fail_if_missing` (default: `TRUE`)

These arguments must be provided as named arguments.

If `temporary` is `TRUE`, the call to `dbRemoveTable()` will consider
only temporary tables. Not all backends support this argument. In
particular, permanent tables of the same name are left untouched.

If `fail_if_missing` is `FALSE`, the call to `dbRemoveTable()` succeeds
if the table does not exist.

## Specification

A table removed by `dbRemoveTable()` doesn't appear in the list of
tables returned by
[`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html),
and
[`DBI::dbExistsTable()`](https://dbi.r-dbi.org/reference/dbExistsTable.html)
returns `FALSE`. The removal propagates immediately to other connections
to the same database. This function can also be used to remove a
temporary table.

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbRemoveTable()` will do the
  quoting, perhaps by calling `dbQuoteIdentifier(conn, x = name)`

- If the result of a call to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html):
  no more quoting is done

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md),
[`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md),
[`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
