# spec_sql_unquote_identifier

spec_sql_unquote_identifier

## Value

`dbUnquoteIdentifier()` returns a list of objects of the same length as
the input. For an empty vector, this function returns a length-0 object.
The names of the input argument are preserved in the output. If `x` is a
value returned by `dbUnquoteIdentifier()`, calling
`dbUnquoteIdentifier(..., dbQuoteIdentifier(..., x))` returns `list(x)`.
If `x` is an object of class
[DBI::Id](https://dbi.r-dbi.org/reference/Id.html), calling
`dbUnquoteIdentifier(..., x)` returns `list(x)`. (For backends it may be
most convenient to return
[DBI::Id](https://dbi.r-dbi.org/reference/Id.html) objects to achieve
this behavior, but this is not required.)

Plain character vectors can also be passed to `dbUnquoteIdentifier()`.

## Failure modes

An error is raised if a character vectors with a missing value is passed
as the `x` argument.

## Specification

For any character vector of length one, quoting (with
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html))
then unquoting then quoting the first element is identical to just
quoting. This is also true for strings that contain special characters
such as a space, a dot, a comma, or quotes used to mark strings or
identifiers, if the database supports this.

Unquoting simple strings (consisting of only letters) wrapped with
[`DBI::SQL()`](https://dbi.r-dbi.org/reference/SQL.html) and then
quoting via
[`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html)
gives the same result as just quoting the string. Similarly, unquoting
expressions of the form `SQL("schema.table")` and then quoting gives the
same result as quoting the identifier constructed by
`Id("schema", "table")`.

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
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
