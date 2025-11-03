# spec_arrow_write_table_arrow

spec_arrow_write_table_arrow

## Value

`dbWriteTableArrow()` returns `TRUE`, invisibly.

## Failure modes

If the table exists, and both `append` and `overwrite` arguments are
unset, or `append = TRUE` and the data frame with the new data has
different column names, an error is raised; the remote table remains
unchanged.

An error is raised when calling this method for a closed or invalid
connection. An error is also raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar. Invalid values for the additional
arguments `overwrite`, `append`, and `temporary` (non-scalars,
unsupported data types, `NA`, incompatible values, incompatible columns)
also raise an error.

## Additional arguments

The following arguments are not part of the `dbWriteTableArrow()`
generic (to improve compatibility across backends) but are part of the
DBI specification:

- `overwrite` (default: `FALSE`)

- `append` (default: `FALSE`)

- `temporary` (default: `FALSE`)

They must be provided as named arguments. See the "Specification" and
"Value" sections for details on their usage.

## Specification

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbWriteTableArrow()` will do the
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

- factor (possibly returned as character)

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

## See also

Other Arrow specifications:
[`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md),
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
