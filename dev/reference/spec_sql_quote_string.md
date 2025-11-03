# spec_sql_quote_string

spec_sql_quote_string

## Value

`dbQuoteString()` returns an object that can be coerced to
[character](https://rdrr.io/r/base/character.html), of the same length
as the input. For an empty character vector this function returns a
length-0 object.

When passing the returned object again to `dbQuoteString()` as `x`
argument, it is returned unchanged. Passing objects of class
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) should also return
them unchanged. (For backends it may be most convenient to return
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) objects to achieve
this behavior, but this is not required.)

## Specification

The returned expression can be used in a `SELECT ...` query, and for any
scalar character `x` the value of
`dbGetQuery(paste0("SELECT ", dbQuoteString(x)))[[1]]` must be identical
to `x`, even if `x` contains spaces, tabs, quotes (single or double),
backticks, or newlines (in any combination) or is itself the result of a
`dbQuoteString()` call coerced back to character (even repeatedly). If
`x` is `NA`, the result must merely satisfy
[`is.na()`](https://rdrr.io/r/base/NA.html). The strings `"NA"` or
`"NULL"` are not treated specially.

`NA` should be translated to an unquoted SQL `NULL`, so that the query
`SELECT * FROM (SELECT 1) a WHERE ... IS NULL` returns one row.

## Failure modes

Passing a numeric, integer, logical, or raw vector, or a list for the
`x` argument raises an error.

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
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
