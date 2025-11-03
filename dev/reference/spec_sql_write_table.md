# spec_sql_write_table

spec_sql_write_table

## Value

`dbWriteTable()` returns `TRUE`, invisibly.

## Failure modes

If the table exists, and both `append` and `overwrite` arguments are
unset, or `append = TRUE` and the data frame with the new data has
different column names, an error is raised; the remote table remains
unchanged.

An error is raised when calling this method for a closed or invalid
connection. An error is also raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar. Invalid values for the additional
arguments `row.names`, `overwrite`, `append`, `field.types`, and
`temporary` (non-scalars, unsupported data types, `NA`, incompatible
values, duplicate or missing names, incompatible columns) also raise an
error.

## Additional arguments

The following arguments are not part of the `dbWriteTable()` generic (to
improve compatibility across backends) but are part of the DBI
specification:

- `row.names` (default: `FALSE`)

- `overwrite` (default: `FALSE`)

- `append` (default: `FALSE`)

- `field.types` (default: `NULL`)

- `temporary` (default: `FALSE`)

They must be provided as named arguments. See the "Specification" and
"Value" sections for details on their usage.

## Specification

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbWriteTable()` will do the
  quoting, perhaps by calling `dbQuoteIdentifier(conn, x = name)`

- If the result of a call to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html):
  no more quoting is done

The `value` argument must be a data frame with a subset of the columns
of the existing table if `append = TRUE`. The order of the columns does
not matter with `append = TRUE`.

If the `overwrite` argument is `TRUE`, an existing table of the same
name will be overwritten. This argument doesn't change behavior if the
table does not exist yet.

If the `append` argument is `TRUE`, the rows in an existing table are
preserved, and the new data are appended. If the table doesn't exist
yet, it is created.

If the `temporary` argument is `TRUE`, the table is not available in a
second connection and is gone after reconnecting. Not all backends
support this argument. A regular, non-temporary table is visible in a
second connection, in a pre-existing connection, and after reconnecting
to the database.

SQL keywords can be used freely in table names, column names, and data.
Quotes, commas, spaces, and other special characters such as newlines
and tabs, can also be used in the data, and, if the database supports
non-syntactic identifiers, also for table names and column names.

The following data types must be supported at least, and be read
identically with
[`DBI::dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html):

- integer

- numeric (the behavior for `Inf` and `NaN` is not specified)

- logical

- `NA` as NULL

- 64-bit values (using `"bigint"` as field type); the result can be

  - converted to a numeric, which may lose precision,

  - converted a character vector, which gives the full decimal
    representation

  - written to another table and read again unchanged

- character (in both UTF-8 and native encodings), supporting empty
  strings before and after a non-empty string

- factor (returned as character)

- list of raw (if supported by the database)

- objects of type
  [blob::blob](https://blob.tidyverse.org/reference/blob.html) (if
  supported by the database)

- date (if supported by the database; returned as `Date`), also for
  dates prior to 1970 or 1900 or after 2038

- time (if supported by the database; returned as objects that inherit
  from `difftime`)

- timestamp (if supported by the database; returned as `POSIXct`
  respecting the time zone but not necessarily preserving the input time
  zone), also for timestamps prior to 1970 or 1900 or after 2038
  respecting the time zone but not necessarily preserving the input time
  zone)

Mixing column types in the same table is supported.

The `field.types` argument must be a named character vector with at most
one entry for each column. It indicates the SQL data type to be used for
a new column. If a column is missed from `field.types`, the type is
inferred from the input data with
[`DBI::dbDataType()`](https://dbi.r-dbi.org/reference/dbDataType.html).

The interpretation of [rownames](https://rdrr.io/r/base/colnames.html)
depends on the `row.names` argument, see
[`DBI::sqlRownamesToColumn()`](https://dbi.r-dbi.org/reference/rownames.html)
for details:

- If `FALSE` or `NULL`, row names are ignored.

- If `TRUE`, row names are converted to a column named "row_names", even
  if the input data frame only has natural row names from 1 to
  `nrow(...)`.

- If `NA`, a column named "row_names" is created if the data has custom
  row names, no extra column is created in the case of natural row
  names.

- If a string, this specifies the name of the column in the remote table
  that contains the row names, even if the input data frame only has
  natural row names.

The default is `row.names = FALSE`.

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
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md)
