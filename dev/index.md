# DBItest

This package is primarily useful for developers of
[DBI](https://dbi.r-dbi.org) backends. It provides a considerable set of
test cases for DBI backends. These test cases correspond to the [DBI
specification](https://dbi.r-dbi.org/articles/spec). Please follow the
steps below to add these test cases to your DBI backend.

## Installation

Install from CRAN via

``` r
install.packages("DBItest")
```

or the development version using

``` r
devtools::install_github("r-dbi/DBItest")
```

## Usage

In your driver package, add `DBItest` to the `Suggests:` and enable the
tests. Run the following code in you package’s directory:

``` r
# install.packages("usethis")
usethis::use_package("DBItest", "suggests")
usethis::use_test("DBItest")
```

This enables testing using `testthat` (if necessary) and creates, among
others, a file `test-DBItest.R` in the `tests/testthat` directory.
Replace its entire contents by the following:

``` r
DBItest::make_context(Kazam(), NULL)
DBItest::test_all()
```

This assumes that `Kazam()` returns an instance of your `DBIDriver`
class. Additional arguments to `dbConnect()` are specified as named list
instead of the `NULL` argument to
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md).
The `default_skip` argument to
[`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
allows skipping tests that are not (yet) satisfied by your backend.

Further reading:

- Detailed instructions in
  [`vignette("DBItest")`](https://dbitest.r-dbi.org/dev/articles/DBItest.md)

- The feature list in the [original
  proposal](https://github.com/r-dbi/DBItest/wiki/Proposal).

------------------------------------------------------------------------

Please note that the ‘DBItest’ project is released with a [Contributor
Code of Conduct](https://dbitest.r-dbi.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
