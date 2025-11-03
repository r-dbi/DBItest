# spec_meta_bind

spec_meta_bind

spec_meta_bind

spec_meta_bind

## Value

`dbBind()` returns the result set, invisibly, for queries issued by
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
or
[`DBI::dbSendQueryArrow()`](https://dbi.r-dbi.org/reference/dbSendQueryArrow.html)
and also for data manipulation statements issued by
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html).

## Specification

DBI clients execute parametrized statements as follows:

1.  Call
    [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
    [`DBI::dbSendQueryArrow()`](https://dbi.r-dbi.org/reference/dbSendQueryArrow.html)
    or
    [`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html)
    with a query or statement that contains placeholders, store the
    returned
    [DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html)
    object in a variable. Mixing placeholders (in particular, named and
    unnamed ones) is not recommended. It is good practice to register a
    call to
    [`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
    via [`on.exit()`](https://rdrr.io/r/base/on.exit.html) right after
    calling `dbSendQuery()` or `dbSendStatement()` (see the last
    enumeration item). Until
    [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html) or
    [`DBI::dbBindArrow()`](https://dbi.r-dbi.org/reference/dbBind.html)
    have been called, the returned result set object has the following
    behavior:

    - [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html)
      raises an error (for `dbSendQuery()` and `dbSendQueryArrow()`)

    - [`DBI::dbGetRowCount()`](https://dbi.r-dbi.org/reference/dbGetRowCount.html)
      returns zero (for `dbSendQuery()` and `dbSendQueryArrow()`)

    - [`DBI::dbGetRowsAffected()`](https://dbi.r-dbi.org/reference/dbGetRowsAffected.html)
      returns an integer `NA` (for `dbSendStatement()`)

    - [`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
      returns `TRUE`

    - [`DBI::dbHasCompleted()`](https://dbi.r-dbi.org/reference/dbHasCompleted.html)
      returns `FALSE`

2.  Call [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html)
    or
    [`DBI::dbBindArrow()`](https://dbi.r-dbi.org/reference/dbBind.html):

    - For
      [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html),
      the `params` argument must be a list where all elements have the
      same lengths and contain values supported by the backend. A
      [data.frame](https://rdrr.io/r/base/data.frame.html) is internally
      stored as such a list.

    - For
      [`DBI::dbBindArrow()`](https://dbi.r-dbi.org/reference/dbBind.html),
      the `params` argument must be a nanoarrow array stream, with one
      column per query parameter.

3.  Retrieve the data or the number of affected rows from the
    `DBIResult` object.

    - For queries issued by `dbSendQuery()` or `dbSendQueryArrow()`,
      call
      [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html).

    - For statements issued by `dbSendStatements()`, call
      [`DBI::dbGetRowsAffected()`](https://dbi.r-dbi.org/reference/dbGetRowsAffected.html).
      (Execution begins immediately after the
      [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html)
      call, the statement is processed entirely before the function
      returns.)

4.  Repeat 2. and 3. as necessary.

5.  Close the result set via
    [`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html).

The elements of the `params` argument do not need to be scalars, vectors
of arbitrary length (including length 0) are supported. For queries,
calling `dbFetch()` binding such parameters returns concatenated
results, equivalent to binding and fetching for each set of values and
connecting via [`rbind()`](https://rdrr.io/r/base/cbind.html). For data
manipulation statements, `dbGetRowsAffected()` returns the total number
of rows affected if binding non-scalar parameters. `dbBind()` also
accepts repeated calls on the same result set for both queries and data
manipulation statements, even if no results are fetched between calls to
`dbBind()`, for both queries and data manipulation statements.

If the placeholders in the query are named, their order in the `params`
argument is not important.

At least the following data types are accepted on input (including
[NA](https://rdrr.io/r/base/NA.html)):

- [integer](https://rdrr.io/r/base/integer.html)

- [numeric](https://rdrr.io/r/base/numeric.html)

- [logical](https://rdrr.io/r/base/logical.html) for Boolean values

- [character](https://rdrr.io/r/base/character.html) (also with special
  characters such as spaces, newlines, quotes, and backslashes)

- [factor](https://rdrr.io/r/base/factor.html) (bound as character, with
  warning)

- [Date](https://rdrr.io/r/base/Dates.html) (also when stored internally
  as integer)

- [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) timestamps

- [POSIXlt](https://rdrr.io/r/base/DateTimeClasses.html) timestamps

- [difftime](https://rdrr.io/r/base/difftime.html) values (also with
  units other than seconds and with the value stored as integer)

- lists of [raw](https://rdrr.io/r/base/raw.html) for blobs (with `NULL`
  entries for SQL NULL values)

- objects of type
  [blob::blob](https://blob.tidyverse.org/reference/blob.html)

## Failure modes

Calling `dbBind()` for a query without parameters raises an error.

Binding too many or not enough values, or parameters with wrong names or
unequal length, also raises an error. If the placeholders in the query
are named, all parameter values must have names (which must not be empty
or `NA`), and vice versa, otherwise an error is raised. The behavior for
mixing placeholders of different types (in particular mixing positional
and named placeholders) is not specified.

Calling `dbBind()` on a result set already cleared by
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html)
also raises an error.

## See also

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)

Other meta specifications:
[`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md),
[`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md),
[`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md),
[`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md),
[`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md),
[`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md),
[`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
