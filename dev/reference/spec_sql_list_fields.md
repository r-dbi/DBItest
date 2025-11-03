# spec_sql_list_fields

spec_sql_list_fields

## Value

`dbListFields()` returns a character vector that enumerates all fields
in the table in the correct order. This also works for temporary tables
if supported by the database. The returned names are suitable for
quoting with `dbQuoteIdentifier()`.

## Failure modes

If the table does not exist, an error is raised. Invalid types for the
`name` argument (e.g., `character` of length not equal to one, or
numeric) lead to an error. An error is also raised when calling this
method for a closed or invalid connection.

## Specification

The `name` argument can be

- a string

- the return value of
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)

- a value from the `table` column from the return value of
  [`DBI::dbListObjects()`](https://dbi.r-dbi.org/reference/dbListObjects.html)
  where `is_prefix` is `FALSE`

A column named `row_names` is treated like any other column.

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md),
[`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
