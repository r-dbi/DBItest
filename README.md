# DBItest [![Travis-CI Build Status](https://travis-ci.org/rstats-db/DBItest.svg?branch=master)](https://travis-ci.org/rstats-db/DBItest) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rstats-db/DBItest?branch=master&svg=true)](https://ci.appveyor.com/project/rstats-db/DBItest) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DBItest)](https://cran.r-project.org/package=DBItest)

This package provides a considerable set of test cases which you can easily incorporate in your DBI driver package.

## Usage

Install from CRAN via

```r
install.packages("DBItest")
```

or the development version using

```r
devtools::install_github("rstats-db/DBItest")
```

In your driver backage, add `DBItest` to the `Suggests:`. Then, enable the tests by running

```r
devtools::use_testthat()
devtools::use_test("DBItest")
```

from your package's directory. This enables testing using `testthat` (if necessary) and creates, among others, a file `test-DBItest.R` in the `tests/testthat` directory. Replace its entire contents by the following:

```r
DBItest::make_context(Kazam(), NULL)
DBItest::test_all()
```

(This assumes that `Kazam()` returns an instance of your `DBIDriver` class. Additional arguments to `dbConnect()` are specified as named list instead of the `NULL` argument to `make_context()`.)

The `skip` argument to `test_all()` allows specifying skipped tests.

See the package's documentation and the [feature list](https://github.com/rstats-db/DBItest/wiki/Proposal) for a description of the tests.
