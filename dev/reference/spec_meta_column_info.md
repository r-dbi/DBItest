# spec_meta_column_info

spec_meta_column_info

## Value

`dbColumnInfo()` returns a data frame with at least two columns `"name"`
and `"type"` (in that order) (and optional columns that start with a
dot). The `"name"` and `"type"` columns contain the names and types of
the R columns of the data frame that is returned from
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html). The
`"type"` column is of type `character` and only for information. Do not
compute on the `"type"` column, instead use `dbFetch(res, n = 0)` to
create a zero-row data frame initialized with the correct data types.

## Failure modes

An attempt to query columns for a closed result set raises an error.

## Specification

A column named `row_names` is treated like any other column.

The column names are always consistent with the data returned by
`dbFetch()`.

If the query returns unnamed columns, non-empty and non-`NA` names are
assigned.

Column names that correspond to SQL or R keywords are left unchanged.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
