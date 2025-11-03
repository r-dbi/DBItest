# spec_result_send_query

spec_result_send_query

## Value

`dbSendQuery()` returns an S4 object that inherits from
[DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html).
The result set can be used with
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html) to
extract records. Once you have finished using a result, make sure to
clear it with
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html).

## Failure modes

An error is raised when issuing a query over a closed or invalid
connection, or if the query is not a non-`NA` string. An error is also
raised if the syntax of the query is invalid and all query parameters
are given (by passing the `params` argument) or the `immediate` argument
is set to `TRUE`.

## Additional arguments

The following arguments are not part of the `dbSendQuery()` generic (to
improve compatibility across backends) but are part of the DBI
specification:

- `params` (default: `NULL`)

- `immediate` (default: `NULL`)

They must be provided as named arguments. See the "Specification"
sections for details on their usage.

## Specification

No warnings occur under normal conditions. When done, the DBIResult
object must be cleared with a call to
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html).
Failure to clear the result set leads to a warning when the connection
is closed.

If the backend supports only one open result set per connection, issuing
a second query invalidates an already open result set and raises a
warning. The newly opened result set is valid and must be cleared with
`dbClearResult()`.

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
[`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md),
[`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
