# Test Arrow methods

Test Arrow methods

## Usage

``` r
test_arrow(skip = NULL, run_only = NULL, ctx = get_default_context())
```

## Arguments

- skip:

  `[character()]`  
  A vector of regular expressions to match against test names; skip test
  if matching any. To improve precision, the regular expressions are
  matched against the entire test name minus a possible suffix `_N`
  where `N` is a number. For example, `skip = "exists_table"` will skip
  both `"exists_table_1"` and `"exists_table_2"`, but not
  `"there_exists_table"`.

- run_only:

  `[character()]`  
  A vector of regular expressions to match against test names; run only
  these tests. The regular expressions are matched against the entire
  test name.

- ctx:

  `[DBItest_context]`  
  A test context as created by
  [`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md).

## See also

Other tests:
[`test_compliance()`](https://dbitest.r-dbi.org/dev/reference/test_compliance.md),
[`test_connection()`](https://dbitest.r-dbi.org/dev/reference/test_connection.md),
[`test_driver()`](https://dbitest.r-dbi.org/dev/reference/test_driver.md),
[`test_getting_started()`](https://dbitest.r-dbi.org/dev/reference/test_getting_started.md),
[`test_meta()`](https://dbitest.r-dbi.org/dev/reference/test_meta.md),
[`test_result()`](https://dbitest.r-dbi.org/dev/reference/test_result.md),
[`test_sql()`](https://dbitest.r-dbi.org/dev/reference/test_sql.md),
[`test_stress()`](https://dbitest.r-dbi.org/dev/reference/test_stress.md),
[`test_transaction()`](https://dbitest.r-dbi.org/dev/reference/test_transaction.md)
