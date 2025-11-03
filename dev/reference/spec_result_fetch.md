# spec_result_fetch

spec_result_fetch

## Value

`dbFetch()` always returns a
[data.frame](https://rdrr.io/r/base/data.frame.html) with as many rows
as records were fetched and as many columns as fields in the result set,
even if the result is a single value or has one or zero rows. Passing
`n = NA` is supported and returns an arbitrary number of rows (at least
one) as specified by the driver, but at most the remaining rows in the
result set.

## Failure modes

An attempt to fetch from a closed result set raises an error. If the `n`
argument is not an atomic whole number greater or equal to -1 or Inf, an
error is raised, but a subsequent call to `dbFetch()` with proper `n`
argument succeeds.

Calling `dbFetch()` on a result set from a data manipulation query
created by
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html)
can be fetched and return an empty data frame, with a warning.

## Specification

Fetching multi-row queries with one or more columns by default returns
the entire result. Multi-row queries can also be fetched progressively
by passing a whole number
([integer](https://rdrr.io/r/base/integer.html) or
[numeric](https://rdrr.io/r/base/numeric.html)) as the `n` argument. A
value of [Inf](https://rdrr.io/r/base/is.finite.html) for the `n`
argument is supported and also returns the full result. If more rows
than available are fetched, the result is returned in full without
warning. If fewer rows than requested are returned, further fetches will
return a data frame with zero rows. If zero rows are fetched, the
columns of the data frame are still fully typed. Fetching fewer rows
than available is permitted, no warning is issued when clearing the
result set.

A column named `row_names` is treated like any other column.

## See also

Other result specifications:
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md),
[`spec_result_create_table_with_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_result_create_table_with_data_type.md),
[`spec_result_execute`](https://dbitest.r-dbi.org/dev/reference/spec_result_execute.md),
[`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md),
[`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md),
[`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
