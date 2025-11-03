# spec_sql_quote_literal

spec_sql_quote_literal

## Value

`dbQuoteLiteral()` returns an object that can be coerced to
[character](https://rdrr.io/r/base/character.html), of the same length
as the input. For an empty integer, numeric, character, logical, date,
time, or blob vector, this function returns a length-0 object.

When passing the returned object again to `dbQuoteLiteral()` as `x`
argument, it is returned unchanged. Passing objects of class
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) should also return
them unchanged. (For backends it may be most convenient to return
[DBI::SQL](https://dbi.r-dbi.org/reference/SQL.html) objects to achieve
this behavior, but this is not required.)

## Specification

The returned expression can be used in a `SELECT ...` query, and the
value of `dbGetQuery(paste0("SELECT ", dbQuoteLiteral(x)))[[1]]` must be
equal to `x` for any scalar integer, numeric, string, and logical. If
`x` is `NA`, the result must merely satisfy
[`is.na()`](https://rdrr.io/r/base/NA.html). The literals `"NA"` or
`"NULL"` are not treated specially.

`NA` should be translated to an unquoted SQL `NULL`, so that the query
`SELECT * FROM (SELECT 1) a WHERE ... IS NULL` returns one row.

## Failure modes

Passing a list for the `x` argument raises an error.

## See also

Other sql specifications:
[`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md),
[`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md),
[`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md),
[`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md),
[`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md),
[`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md),
[`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md),
[`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md),
[`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md),
[`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md),
[`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md),
[`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
