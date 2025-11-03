# spec_sql_create_table

spec_sql_create_table

## Value

`dbCreateTable()` returns `TRUE`, invisibly.

## Failure modes

If the table exists, an error is raised; the remote table remains
unchanged.

An error is raised when calling this method for a closed or invalid
connection. An error is also raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar. Invalid values for the `row.names`
and `temporary` arguments (non-scalars, unsupported data types, `NA`,
incompatible values, duplicate names) also raise an error.

## Additional arguments

The following arguments are not part of the `dbCreateTable()` generic
(to improve compatibility across backends) but are part of the DBI
specification:

- `temporary` (default: `FALSE`)

They must be provided as named arguments. See the "Specification" and
"Value" sections for details on their usage.

## Specification

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbCreateTable()` will do the
  quoting, perhaps by calling `dbQuoteIdentifier(conn, x = name)`

- If the result of a call to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html):
  no more quoting is done

The `value` argument can be:

- a data frame,

- a named list of SQL types

If the `temporary` argument is `TRUE`, the table is not available in a
second connection and is gone after reconnecting. Not all backends
support this argument. A regular, non-temporary table is visible in a
second connection, in a pre-existing connection, and after reconnecting
to the database.

SQL keywords can be used freely in table names, column names, and data.
Quotes, commas, and spaces can also be used for table names and column
names, if the database supports non-syntactic identifiers.

The `row.names` argument must be missing or `NULL`, the default value.
All other values for the `row.names` argument (in particular `TRUE`,
`NA`, and a string) raise an error.

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md),
[`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md),
[`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
