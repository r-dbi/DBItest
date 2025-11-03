# spec_arrow_append_table_arrow

spec_arrow_append_table_arrow

## Value

`dbAppendTableArrow()` returns a scalar numeric.

## Failure modes

If the table does not exist, or the new data in `values` is not a data
frame or has different column names, an error is raised; the remote
table remains unchanged.

An error is raised when calling this method for a closed or invalid
connection. An error is also raised if `name` cannot be processed with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
or if this results in a non-scalar.

## Specification

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
  strings (before and after non-empty strings)

- factor (possibly returned as character)

- objects of type
  [blob::blob](https://blob.tidyverse.org/reference/blob.html) (if
  supported by the database)

- date (if supported by the database; returned as `Date`) also for dates
  prior to 1970 or 1900 or after 2038

- time (if supported by the database; returned as objects that inherit
  from `difftime`)

- timestamp (if supported by the database; returned as `POSIXct`
  respecting the time zone but not necessarily preserving the input time
  zone), also for timestamps prior to 1970 or 1900 or after 2038
  respecting the time zone but not necessarily preserving the input time
  zone)

Mixing column types in the same table is supported.

The `name` argument is processed as follows, to support databases that
allow non-syntactic names for their objects:

- If an unquoted table name as string: `dbAppendTableArrow()` will do
  the quoting, perhaps by calling `dbQuoteIdentifier(conn, x = name)`

- If the result of a call to
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html):
  no more quoting is done to support databases that allow non-syntactic
  names for their objects:

The `value` argument must be a data frame with a subset of the columns
of the existing table. The order of the columns does not matter.

## See also

Other Arrow specifications:
[`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md),
[`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md),
[`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md),
[`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md),
[`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md),
[`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md),
[`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md),
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
