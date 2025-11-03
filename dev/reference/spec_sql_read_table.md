# spec_sql_read_table

spec_sql_read_table

## Value

`dbReadTable()` returns a data frame that contains the complete data
from the remote table, effectively the result of calling
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)
with `SELECT * FROM <name>`.

An empty table is returned as a data frame with zero rows.

The presence of [rownames](https://rdrr.io/r/base/colnames.html) depends
on the `row.names` argument, see
[`DBI::sqlColumnToRownames()`](https://dbi.r-dbi.org/reference/rownames.html)
for details:

- If `FALSE` or `NULL`, the returned data frame doesn't have row names.

- If `TRUE`, a column named "row_names" is converted to row names.

&nbsp;

- If `NA`, a column named "row_names" is converted to row names if it
  exists, otherwise no translation occurs.

- If a string, this specifies the name of the column in the remote table
  that contains the row names.

The default is `row.names = FALSE`.

If the database supports identifiers with special characters, the
columns in the returned data frame are converted to valid R identifiers
if the `check.names` argument is `TRUE`, If `check.names = FALSE`, the
returned table has non-syntactic column names without quotes.

## Failure modes

An error is raised if the table does not exist.

An error is raised if `row.names` is `TRUE` and no "row_names" column
exists,

An error is raised if `row.names` is set to a string and no
corresponding column exists.

An error is raised when calling this method for a closed or invalid
connection. An error is raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar. Unsupported values for `row.names`
and `check.names` (non-scalars, unsupported data types, `NA` for
`check.names`) also raise an error.

## Additional arguments

The following arguments are not part of the `dbReadTable()` generic (to
improve compatibility across backends) but are part of the DBI
specification:

- `row.names` (default: `FALSE`)

- `check.names`

They must be provided as named arguments. See the "Value" section for
details on their usage.

## Specification

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbReadTable()` will do the
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
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
