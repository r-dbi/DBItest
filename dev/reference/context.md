# Test contexts

Create a test context, set and query the default context.

## Usage

``` r
make_context(
  drv,
  connect_args = NULL,
  set_as_default = TRUE,
  tweaks = NULL,
  name = NULL,
  default_skip = NULL
)

set_default_context(ctx)

get_default_context()
```

## Arguments

- drv:

  `[DBIConnector]`  
  An object of class
  [DBI::DBIConnector](https://dbi.r-dbi.org/reference/DBIConnector-class.html)
  that describes how to connect to the database.

- connect_args:

  `[named list]`  
  Deprecated.

- set_as_default:

  `[logical(1)]`  
  Should the created context be set as default context?

- tweaks:

  `[DBItest_tweaks]`  
  Tweaks as constructed by the
  [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md)
  function.

- name:

  `[character]`  
  An optional name of the context which will be used in test messages.

- default_skip:

  `[character]`  
  Default value of `skip` argument to
  [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  and other testing functions.

- ctx:

  `[DBItest_context]`  
  A test context.

## Value

`[DBItest_context]`  
A test context, for `set_default_context` the previous default context
(invisibly) or `NULL`.

## Examples

``` r
make_context(
  new(
    "DBIConnector",
    .drv = RSQLite::SQLite(),
    .conn_args = list(dbname = tempfile("DBItest", fileext = ".sqlite"))
  ),
  tweaks = tweaks(
    constructor_relax_args = TRUE,
    placeholder_pattern = c("?", "$1", "$name", ":name"),
    date_cast = function(x) paste0("'", x, "'"),
    time_cast = function(x) paste0("'", x, "'"),
    timestamp_cast = function(x) paste0("'", x, "'"),
    logical_return = function(x) as.integer(x),
    date_typed = FALSE,
    time_typed = FALSE,
    timestamp_typed = FALSE
  ),
  default_skip = c("roundtrip_date", "roundtrip_timestamp")
)
#> $cnr
#> <DBIConnector><SQLiteDriver>
#> Arguments:
#> $dbname
#> [1] "/tmp/Rtmpak6ymy/DBItest2ca37d27d59f.sqlite"
#> 
#> 
#> $drv
#> <SQLiteDriver>
#> 
#> $tweaks
#> DBItest tweaks:
#>   constructor_relax_args: TRUE
#>   strict_identifier: FALSE
#>   omit_blob_tests: FALSE
#>   current_needs_parens: FALSE
#>   union: function (x) 
#>   placeholder_pattern: ?    
#>   logical_return: function (x) 
#>   date_cast: function (x) 
#>   time_cast: function (x) 
#>   timestamp_cast: function (x) 
#>   blob_cast: function (x) 
#>   date_typed: FALSE
#>   time_typed: FALSE
#>   timestamp_typed: FALSE
#>   temporary_tables: TRUE
#>   list_temporary_tables: TRUE
#>   allow_na_rows_affected: FALSE
#>   is_null_check: function (x) 
#>   create_table_as: function (table_name, query = trivial_query()) 
#>   create_table_empty: function (table_name) 
#>   dbitest_version: 1.7.1
#> 
#> $name
#> NULL
#> 
#> $default_skip
#> [1] "roundtrip_date"      "roundtrip_timestamp"
#> 
#> attr(,"class")
#> [1] "DBItest_context"
```
