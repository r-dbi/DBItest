# DBItest

[![Travis-CI Build Status](https://travis-ci.org/r-dbi/DBItest.svg?branch=master)](https://travis-ci.org/r-dbi/DBItest) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-dbi/DBItest?branch=master&svg=true)](https://ci.appveyor.com/project/r-dbi/DBItest) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/DBItest)](https://cran.r-project.org/package=DBItest)

This package provides a considerable set of test cases which you can easily incorporate in your DBI driver package.

## Usage

Install from CRAN via

```r
install.packages("DBItest")
```

or the development version using

```r
devtools::install_github("r-dbi/DBItest")
```

In your driver backage, add `DBItest` to the `Suggests:`. Then, enable the tests by running

```r
usethis::use_test("DBItest")
```

from your package's directory. This enables testing using `testthat` (if necessary) and creates, among others, a file `test-DBItest.R` in the `tests/testthat` directory. Replace its entire contents by the following:

```r
DBItest::make_context(Kazam(), NULL)
DBItest::test_all()
```

(This assumes that `Kazam()` returns an instance of your `DBIDriver` class. Additional arguments to `dbConnect()` are specified as named list instead of the `NULL` argument to `make_context()`.)

The `skip` argument to `test_all()` allows specifying skipped tests.

See the package's documentation and the [feature list](https://github.com/r-dbi/DBItest/wiki/Proposal) for a description of the tests.

---

Please note that the 'DBItest' project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
