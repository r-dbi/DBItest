# spec_result_get_query

spec_result_get_query

## Value

`dbGetQuery()` always returns a
[data.frame](https://rdrr.io/r/base/data.frame.html), with as many rows
as records were fetched and as many columns as fields in the result set,
even if the result is a single value or has one or zero rows.

## Failure modes

An error is raised when issuing a query over a closed or invalid
connection, if the syntax of the query is invalid, or if the query is
not a non-`NA` string. If the `n` argument is not an atomic whole number
greater or equal to -1 or Inf, an error is raised, but a subsequent call
to `dbGetQuery()` with proper `n` argument succeeds.

## Additional arguments

The following arguments are not part of the `dbGetQuery()` generic (to
improve compatibility across backends) but are part of the DBI
specification:

- `n` (default: -1)

- `params` (default: `NULL`)

- `immediate` (default: `NULL`)

They must be provided as named arguments. See the "Specification" and
"Value" sections for details on their usage.

## Specification

A column named `row_names` is treated like any other column.

The `n` argument specifies the number of rows to be fetched. If omitted,
fetching multi-row queries with one or more columns returns the entire
result. A value of [Inf](https://rdrr.io/r/base/is.finite.html) for the
`n` argument is supported and also returns the full result. If more rows
than available are fetched (by passing a too large value for `n`), the
result is returned in full without warning. If zero rows are requested,
the columns of the data frame are still fully typed. Fetching fewer rows
than available is permitted, no warning is issued.

The `param` argument allows passing query parameters, see
[`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html) for
details.

## Specification for the `immediate` argument

The `immediate` argument supports distinguishing between "direct" and
"prepared" APIs offered by many database drivers. Passing
`immediate = TRUE` leads to immediate execution of the query or
statement, via the "direct" API (if supported by the driver). The
default `NULL` means that the backend should choose whatever API makes
the most sense for the database, and (if relevant) tries the other API
if the first attempt fails. A successful second attempt should result in
a message that suggests passing the correct `immediate` argument.
Examples for possible behaviors:

1.  DBI backend defaults to `immediate = TRUE` internally

    1.  A query without parameters is passed: query is executed

    2.  A query with parameters is passed:

        1.  `params` not given: rejected immediately by the database
            because of a syntax error in the query, the backend tries
            `immediate = FALSE` (and gives a message)

        2.  `params` given: query is executed using `immediate = FALSE`

2.  DBI backend defaults to `immediate = FALSE` internally

    1.  A query without parameters is passed:

        1.  simple query: query is executed

        2.  "special" query (such as setting a config options): fails,
            the backend tries `immediate = TRUE` (and gives a message)

    2.  A query with parameters is passed:

        1.  `params` not given: waiting for parameters via
            [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html)

        2.  `params` given: query is executed

## See also

Other result specifications:
[`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md),
[`spec_result_create_table_with_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_result_create_table_with_data_type.md),
[`spec_result_execute`](https://dbitest.r-dbi.org/dev/reference/spec_result_execute.md),
[`spec_result_fetch`](https://dbitest.r-dbi.org/dev/reference/spec_result_fetch.md),
[`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md),
[`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
