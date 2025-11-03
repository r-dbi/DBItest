# spec_result_roundtrip

spec_result_roundtrip

## Specification

The column types of the returned data frame depend on the data returned:

- [integer](https://rdrr.io/r/base/integer.html) (or coercible to an
  integer) for integer values between -2^31 and 2^31 - 1, with
  [NA](https://rdrr.io/r/base/NA.html) for SQL `NULL` values

- [numeric](https://rdrr.io/r/base/numeric.html) for numbers with a
  fractional component, with NA for SQL `NULL` values

- [logical](https://rdrr.io/r/base/logical.html) for Boolean values
  (some backends may return an integer); with NA for SQL `NULL` values

- [character](https://rdrr.io/r/base/character.html) for text, with NA
  for SQL `NULL` values

- lists of [raw](https://rdrr.io/r/base/raw.html) for blobs with
  [NULL](https://rdrr.io/r/base/NULL.html) entries for SQL NULL values

- coercible using [`as.Date()`](https://rdrr.io/r/base/as.Date.html) for
  dates, with NA for SQL `NULL` values (also applies to the return value
  of the SQL function `current_date`)

- coercible using
  [`hms::as_hms()`](https://hms.tidyverse.org/reference/hms.html) for
  times, with NA for SQL `NULL` values (also applies to the return value
  of the SQL function `current_time`)

- coercible using
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) for
  timestamps, with NA for SQL `NULL` values (also applies to the return
  value of the SQL function `current_timestamp`)

If dates and timestamps are supported by the backend, the following R
types are used:

- [Date](https://rdrr.io/r/base/Dates.html) for dates (also applies to
  the return value of the SQL function `current_date`)

- [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) for timestamps
  (also applies to the return value of the SQL function
  `current_timestamp`)

R has no built-in type with lossless support for the full range of
64-bit or larger integers. If 64-bit integers are returned from a query,
the following rules apply:

- Values are returned in a container with support for the full range of
  valid 64-bit values (such as the `integer64` class of the bit64
  package)

- Coercion to numeric always returns a number that is as close as
  possible to the true value

- Loss of precision when converting to numeric gives a warning

- Conversion to character always returns a lossless decimal
  representation of the data

## See also

Other result specifications:
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md),
[`spec_result_create_table_with_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_result_create_table_with_data_type.md),
[`spec_result_execute`](https://dbitest.r-dbi.org/dev/reference/spec_result_execute.md),
[`spec_result_fetch`](https://dbitest.r-dbi.org/dev/reference/spec_result_fetch.md),
[`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md),
[`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
