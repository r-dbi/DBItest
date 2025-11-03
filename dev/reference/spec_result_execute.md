# spec_result_execute

spec_result_execute

## Value

`dbExecute()` always returns a scalar numeric that specifies the number
of rows affected by the statement.

## Failure modes

An error is raised when issuing a statement over a closed or invalid
connection, if the syntax of the statement is invalid, or if the
statement is not a non-`NA` string.

## Additional arguments

The following arguments are not part of the `dbExecute()` generic (to
improve compatibility across backends) but are part of the DBI
specification:

- `params` (default: `NULL`)

- `immediate` (default: `NULL`)

They must be provided as named arguments. See the "Specification"
sections for details on their usage.

## Specification

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
[`spec_result_fetch`](https://dbitest.r-dbi.org/dev/reference/spec_result_fetch.md),
[`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md),
[`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md),
[`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md),
[`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
