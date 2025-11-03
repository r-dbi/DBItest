# spec_sql_list_objects

spec_sql_list_objects

## Value

`dbListObjects()` returns a data frame with columns `table` and
`is_prefix` (in that order), optionally with other columns with a dot
(`.`) prefix. The `table` column is of type list. Each object in this
list is suitable for use as argument in
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
The `is_prefix` column is a logical. This data frame contains one row
for each object (schema, table and view) accessible from the prefix (if
passed) or from the global namespace (if prefix is omitted). Tables
added with
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
are part of the data frame. As soon a table is removed from the
database, it is also removed from the data frame of database objects.

The same applies to temporary objects if supported by the database.

The returned names are suitable for quoting with `dbQuoteIdentifier()`.

## Failure modes

An error is raised when calling this method for a closed or invalid
connection.

## Specification

The `prefix` column indicates if the `table` value refers to a table or
a prefix. For a call with the default `prefix = NULL`, the `table`
values that have `is_prefix == FALSE` correspond to the tables returned
from
[`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html),

The `table` object can be quoted with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
The result of quoting can be passed to
[`DBI::dbUnquoteIdentifier()`](https://dbi.r-dbi.org/reference/dbUnquoteIdentifier.html).
(For backends it may be convenient to use the
[DBI::Id](https://dbi.r-dbi.org/reference/Id.html) class, but this is
not required.)

Values in `table` column that have `is_prefix == TRUE` can be passed as
the `prefix` argument to another call to `dbListObjects()`. For the data
frame returned from a `dbListObject()` call with the `prefix` argument
set, all `table` values where `is_prefix` is `FALSE` can be used in a
call to
[`DBI::dbExistsTable()`](https://dbi.r-dbi.org/reference/dbExistsTable.html)
which returns `TRUE`.

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md),
[`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
