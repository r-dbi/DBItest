# spec_sql_quote_identifier

spec_sql_quote_identifier

## Value

`dbQuoteIdentifier()` returns an object that can be coerced to
[character](https://rdrr.io/r/base/character.html), of the same length
as the input. For an empty character vector this function returns a
length-0 object. The names of the input argument are preserved in the
output. When passing the returned object again to `dbQuoteIdentifier()`
as `x` argument, it is returned unchanged. Passing objects of class
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) should also return
them unchanged. (For backends it may be most convenient to return
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) objects to achieve
this behavior, but this is not required.)

## Failure modes

An error is raised if the input contains `NA`, but not for an empty
string.

## Specification

Calling
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)
for a query of the format `SELECT 1 AS ...` returns a data frame with
the identifier, unquoted, as column name. Quoted identifiers can be used
as table and column names in SQL queries, in particular in queries like
`SELECT 1 AS ...` and `SELECT * FROM (SELECT 1) ...`. The method must
use a quoting mechanism that is unambiguously different from the quoting
mechanism used for strings, so that a query like
`SELECT ... FROM (SELECT 1 AS ...)` throws an error if the column names
do not match.

The method can quote column names that contain special characters such
as a space, a dot, a comma, or quotes used to mark strings or
identifiers, if the database supports this. In any case, checking the
validity of the identifier should be performed only when executing a
query, and not by `dbQuoteIdentifier()`.

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md),
[`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
