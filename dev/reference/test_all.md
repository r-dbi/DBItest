# Run all tests

`test_all()` calls all tests defined in this package (see the section
"Tests" below). This function supports running only one test by setting
an environment variable, e.g., set the `DBITEST_ONLY_RESULT` to a
nonempty value to run only
[`test_result()`](https://dbitest.r-dbi.org/dev/reference/test_result.md).

`test_some()` allows testing one or more tests.

## Usage

``` r
test_all(skip = NULL, run_only = NULL, ctx = get_default_context())

test_some(test, ctx = get_default_context())
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

- test:

  `[character]`  
  A character vector of regular expressions describing the tests to run.
  The regular expressions are matched against the entire test name.

## Details

Internally `^` and `$` are used as prefix and suffix around the regular
expressions passed in the `skip` and `run_only` arguments.

## Tests

This function runs the following tests, except the stress tests:

[`test_getting_started()`](https://dbitest.r-dbi.org/dev/reference/test_getting_started.md):
Getting started with testing

[`test_driver()`](https://dbitest.r-dbi.org/dev/reference/test_driver.md):
Test the "Driver" class

[`test_connection()`](https://dbitest.r-dbi.org/dev/reference/test_connection.md):
Test the "Connection" class

[`test_result()`](https://dbitest.r-dbi.org/dev/reference/test_result.md):
Test the "Result" class

[`test_sql()`](https://dbitest.r-dbi.org/dev/reference/test_sql.md):
Test SQL methods

[`test_meta()`](https://dbitest.r-dbi.org/dev/reference/test_meta.md):
Test metadata functions

[`test_transaction()`](https://dbitest.r-dbi.org/dev/reference/test_transaction.md):
Test transaction functions

[`test_arrow()`](https://dbitest.r-dbi.org/dev/reference/test_arrow.md):
Test Arrow methods

[`test_compliance()`](https://dbitest.r-dbi.org/dev/reference/test_compliance.md):
Test full compliance to DBI

[`test_stress()`](https://dbitest.r-dbi.org/dev/reference/test_stress.md):
Stress tests (not tested with `test_all`)
