Version 0.3 (2015-11-15)
===

- Feature-complete, ready for review
- Tests from the proposal
    - Add missing methods to compliance check
    - Add simple read-only test (#27)
    - Add stress tests for repeated load/unload (with and without connecting) in new R session (#2),
    - Migrate all tests from existing backends (#28)
    - Refactor `data_` tests to use a worker function `test_select()`
    - Test tables with `NA` values above and below the non-`NA` value in `data_` tests
    - Test return values and error conditions for `dbBind()` and `dbClearResult()` (#31)
    - Test vectorization of `dbQuoteString()` and `dbQuoteIdentifier()` (#18)
    - Test that dates have `integer` as underlying data type (#9)
    - Roundtrip tests sort output table to be sure (#32)
    - Test `NA` to `NULL` conversion in `dbQuoteString()`, and false friends (#23)
    - Enhance test for `dbQuoteIdentifier()` (#30)
- Style
    - Avoid using `data.frame()` for date and time columns (#10)
    - Use `expect_identical()` instead of `expect_equal()` in many places (#13)
    - Catch all errors in `on.exit()` handlers via `expect_error()` (#20).
    - Combine "meta" tests into new `test_meta()` (#37)
- Documentation
    - New "test" vignette (#16)
    - Add package documentation (#38)


Version 0.2 (2015-11-11)
===

- Tests from the proposal
    - SQL
    - Metadata
    - DBI compliance (not testing read-only yet)
- Migrate most of the tests from RMySQL
- Test improvements
    - Test BLOB data type (#17)
    - Check actual availability of type returned by `dbDataType()` (#19)
- Testing infrastructure
    - Disambiguate test names (#21)
    - Use regex matching for deciding skipped tests, skip regex must match the entire test name
- Documentation
    - Document all tests in each test function using the new inline documentation feature of roxygen2
    - Improve documentation for `test_all()`: Tests are listed in new "Tests" section
    - Add brief instructions to README
- Move repository to rstats-db namespace
- Same as 0.1-6


Version 0.1 (2015-10-11)
===

- First GitHub release
- Builds successfully on Travis
- Testing infrastructure
    - Test context
    - Skipped tests call `skip()`
    - Function `test_all()` that runs all tests
- Tests from the proposal
    - Getting started
    - Driver
    - Connection
    - Results
- Code formatting is checked with lintr
- Same as 0.0-5
