# Tweaks for DBI tests

The tweaks are a way to control the behavior of certain tests.
Currently, you need to search the DBItest source code to understand
which tests are affected by which tweaks. This function is usually
called to set the `tweaks` argument in a
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
call.

## Usage

``` r
tweaks(
  ...,
  constructor_name = NULL,
  constructor_relax_args = FALSE,
  strict_identifier = FALSE,
  omit_blob_tests = FALSE,
  current_needs_parens = FALSE,
  union = function(x) paste(x, collapse = " UNION "),
  placeholder_pattern = NULL,
  logical_return = identity,
  date_cast = function(x) paste0("date('", x, "')"),
  time_cast = function(x) paste0("time('", x, "')"),
  timestamp_cast = function(x) paste0("timestamp('", x, "')"),
  blob_cast = identity,
  date_typed = TRUE,
  time_typed = TRUE,
  timestamp_typed = TRUE,
  temporary_tables = TRUE,
  list_temporary_tables = TRUE,
  allow_na_rows_affected = FALSE,
  is_null_check = function(x) paste0("(", x, " IS NULL)"),
  create_table_as = function(table_name, query = trivial_query()) paste0("CREATE TABLE ",
    table_name, " AS ", query),
  create_table_empty = function(table_name) paste0("CREATE TABLE ", table_name,
    " (a integer)"),
  dbitest_version = "1.7.1"
)
```

## Arguments

- ...:

  `[any]`  
  Unknown tweaks are accepted, with a warning. The ellipsis also makes
  sure that you only can pass named arguments.

- constructor_name:

  `[character(1)]`  
  Name of the function that constructs the `Driver` object.

- constructor_relax_args:

  `[logical(1)]`  
  If `TRUE`, allow a driver constructor with default values for all
  arguments; otherwise, require a constructor with empty argument list
  (default).

- strict_identifier:

  `[logical(1)]`  
  Set to `TRUE` if the DBMS does not support arbitrarily-named
  identifiers even when quoting is used.

- omit_blob_tests:

  `[logical(1)]`  
  Set to `TRUE` if the DBMS does not support a `BLOB` data type.

- current_needs_parens:

  `[logical(1)]`  
  Set to `TRUE` if the SQL functions `current_date`, `current_time`, and
  `current_timestamp` require parentheses.

- union:

  `[function(character)]`  
  Function that combines several subqueries into one so that the
  resulting query returns the concatenated results of the subqueries

- placeholder_pattern:

  `[character]`  
  A pattern for placeholders used in
  [`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html), e.g.,
  `"?"`, `"$1"`, or `":name"`. See
  [`make_placeholder_fun()`](https://dbitest.r-dbi.org/dev/reference/make_placeholder_fun.md)
  for details.

- logical_return:

  `[function(logical)]`  
  A vectorized function that converts logical values to the data type
  returned by the DBI backend.

- date_cast:

  `[function(character)]`  
  A vectorized function that creates an SQL expression for coercing a
  string to a date value.

- time_cast:

  `[function(character)]`  
  A vectorized function that creates an SQL expression for coercing a
  string to a time value.

- timestamp_cast:

  `[function(character)]`  
  A vectorized function that creates an SQL expression for coercing a
  string to a timestamp value.

- blob_cast:

  `[function(character)]`  
  A vectorized function that creates an SQL expression for coercing a
  string to a blob value.

- date_typed:

  `[logical(1L)]`  
  Set to `FALSE` if the DBMS doesn't support a dedicated type for dates.

- time_typed:

  `[logical(1L)]`  
  Set to `FALSE` if the DBMS doesn't support a dedicated type for times.

- timestamp_typed:

  `[logical(1L)]`  
  Set to `FALSE` if the DBMS doesn't support a dedicated type for
  timestamps.

- temporary_tables:

  `[logical(1L)]`  
  Set to `FALSE` if the DBMS doesn't support temporary tables.

- list_temporary_tables:

  `[logical(1L)]`  
  Set to `FALSE` if the DBMS doesn't support listing temporary tables.

- allow_na_rows_affected:

  `[logical(1L)]`  
  Set to `TRUE` to allow
  [`DBI::dbGetRowsAffected()`](https://dbi.r-dbi.org/reference/dbGetRowsAffected.html)
  to return `NA`.

- is_null_check:

  `[function(character)]`  
  A vectorized function that creates an SQL expression for checking if a
  value is `NULL`.

- create_table_as:

  `[function(character(1), character(1))]`  
  A function that creates an SQL expression for creating a table from an
  SQL expression.

- create_table_empty:

  `[function(character(1))]`  
  A function that creates an SQL expression for creating an empty table
  with a single integer column named 'a'.

- dbitest_version:

  `[character(1)]`  
  Compatible DBItest version, default: "1.7.1".

## Examples

``` r
if (FALSE) { # \dontrun{
make_context(..., tweaks = tweaks(strict_identifier = TRUE))
} # }
```
