# Testing DBI backends

This document shows how to use the DBItest package when implementing a
new DBI backend or when applying it to an existing backend. The DBItest
package provides a large collection of automated tests.

## Testing a new backend

The test cases in the DBItest package are structured very similarly to
the sections in the “backend” vignette:

``` r
vignette("backend", package = "DBI")
```

Like the “backend” vignette, this vignette assumes that you are
implementing the `RKazam` package that has a `Kazam()` function that
creates a new `DBIDriver` instance for connecting to a “Kazam” database.

You can add the tests in the DBItest package incrementally, as you
proceed with implementing the various parts of the DBI. The DBItest
package builds upon the testthat package. To enable it, run the
following in your package directory (after installing or updating
`devtools`):

``` r
devtools::use_testthat()
devtools::use_test("DBItest")
```

This creates, among others, a file `test-DBItest.R` in the
`tests/testthat` directory. Replace its entire contents by the
following:

``` r
DBItest::make_context(Kazam(), NULL)
DBItest::test_getting_started()
```

Now test your package with
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html). If
you followed at least the “Getting started” section of the DBI “backend”
vignette, all tests should succeed.

By adding the corresponding test function to your `tests/test-DBItest.R`
file *before* implementing a section, you get immediate feedback which
functionality of this section still needs to be implemented by running
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
again. Therefore, proceed by appending the following to
`tests/test-DBItest.R`, to include a test case for the forthcoming
section:

``` r
DBItest::test_driver()
```

Again, all tests should succeed when you are done with the “Driver”
section. Add the call to the next tester function, implement the
following section until all tests succeed, and so forth.

In this scenario, you are usually interested only in the first error the
test suite finds. The `StopReporter` of `testthat` is most helpful here,
activate it by passing `reporter = "stop"` to
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html).
Alternatively, call the relevant `DBItest::test_()` function directly.

The tests are documented with the corresponding functions: For instance,
[`?test_driver`](https://dbitest.r-dbi.org/dev/reference/test_driver.md)
shows a coarse description of all tests for the “Driver” test case. Test
failures will include the name of the test that is failing; in this
case, investigating the documentation or the source code of the DBItest
package will usually lead to the cause of the error.

Not all tests can be satisfied: For example, there is one test that
tests that `logical` variables survive a write-read roundtrip to the
database, whereas another test tests that `logical` variables are
converted to `integer` in such a case. Tests can be skipped by adding
regular expressions for the tests to skip as character vector to the
call, as in the following[¹](#fn1):

``` r
DBItest::test_driver(skip = c(
  "data_type"           # Reason 1...
  "constructor.*",      # Reason 2...
  NULL
))
```

Some other reasons to skip tests are: - your database does not support a
feature - you want to postpone or avoid the implementation of a
feature - the test takes too long to run.

## Testing an existing backend

For an existing backends, simply enabling all tests may be the quickest
way to get started. Run the following in your package directory (after
installing or updating `devtools`):

``` r
devtools::use_testthat()
devtools::use_test("DBItest")
```

This creates, among others, a file `test-DBItest.R` in the
`tests/testthat` directory. Replace its entire contents by the
following:

``` r
DBItest::make_context(Kazam(), NULL)
DBItest::test_all()
```

The notes about “Kazam” and skipping tests from the previous section
apply here as well. The
[`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
function simply calls all test cases.

## External testing

DBItest is currently geared towards usage as part of a package’s test
suite. With some effort it is possible to test a database backend
against a custom database. This can help verify that your database
installation gives expected results when accessed with DBI with specific
connection arguments.

The example below shows how to run tests with the RSQLite backend.

### Preparation

First, we need to define a test context. It contains:

- a connector that describes how to establish the database connection,
  see
  [`` ?DBI::`DBIConnector-class` ``](https://dbi.r-dbi.org/reference/DBIConnector-class.html)
  for details,
- tweaks, see
  [`?tweaks`](https://dbitest.r-dbi.org/dev/reference/tweaks.md),
- tests skipped by default, as a character vector.

Database backends that use DBItest for testing usually have a file
`test/testthat/helper-DBItest.R` or `test/testthat/test-DBItest.R` where
a call to
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
can be found. The help for
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
already contains an example that works for RSQLite. Adapt it to your
needs.

The
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
function must be called before any tests can run.

``` r
library(DBItest)

tweaks <- tweaks(
  constructor_relax_args = TRUE,
  placeholder_pattern = c("?", "$1", "$name", ":name"),
  date_cast = function(x) paste0("'", x, "'"),
  time_cast = function(x) paste0("'", x, "'"),
  timestamp_cast = function(x) paste0("'", x, "'"),
  logical_return = function(x) as.integer(x),
  date_typed = FALSE,
  time_typed = FALSE,
  timestamp_typed = FALSE
)

default_skip <- c("roundtrip_date", "roundtrip_timestamp")

invisible(make_context(
  new(
    "DBIConnector",
    .drv = RSQLite::SQLite(),
    .conn_args = list(dbname = tempfile("DBItest", fileext = ".sqlite"))
  ),
  tweaks = tweaks,
  default_skip = default_skip
))
```

### Testing

Use [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
to run all tests, and
[`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md) to
run a specific test that failed previously. The `test_*` functions need
to be run with a testthat reporter to avoid stopping at the first error
or warning. For interactive use, the “progress” reporter gives good
results. In the example below, the “location” and “stop” reporters are
combined. Review
[`?testthat::Reporter`](https://testthat.r-lib.org/reference/Reporter.html)
for a list of reporters.

``` r
DBItest::test_some("get_query_atomic")
```

    ## Test passed 🥇

DBItest relies heavily on metaprogramming. Unfortunately, this means
that a failing test may give no indication of the reason for the
failure. The
[`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
function now by default integrates the new experimental [dblog
package](https://github.com/r-dbi/dblog) package. It prints the DBI code
that is executed as the tests are run, as seen above.

Another way to scout for the reason of the problem is to review the
sources of DBItest and relate the test name (that is printed with each
failure) with the human-readable specification embedded with the test
code.

``` r
testthat::with_reporter(
  c("location", "fail"),
  DBItest::test_some("get_query_atomic")
)
```

    ## Start test: DBItest: Result: get_query_atomic
    ##    [success]
    ##    [success]
    ##    [success]
    ##    [success]
    ##    [success]
    ##    [success]
    ##    [success]
    ## End test: DBItest: Result: get_query_atomic

------------------------------------------------------------------------

1.  The terminating `NULL` allows appending new lines to the end by
    copy-pasting an existing line, without having to take care of the
    terminating comma.
