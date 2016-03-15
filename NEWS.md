Version 1.1-2 (2016-03-15)
===

- The `skip` argument to the test functions is now treated as a Perl regular expression to allow negative lookahead. Use `skip = "(?!test_regex).*"` to choose a single test to run (#33).


Version 1.1-1 (2016-03-10)
===

- Compatibility with dev version of `testthat` (#62).
- Added encoding arguments to non-ASCII string constants (#60, @hoesler).


Version 1.1 (2016-02-12)
===

- New feature: tweaks
    - New argument `tweaks` to `make_context()` (#49).
    - New `tweaks()`, essentially constructs a named list of tweaks but with predefined and documented argument names.
    - `constructor_name`, respected by the `constructor.*` tests.
    - `strict_identifier`, if `TRUE` all identifier must be syntactic names even if quoted. The quoting test is now split, and a part is ignored conditional to this tweak. The `roundtrip_quotes` tests also respects this tweak.
    - `omit_blob_tests` for DBMS that don't have a BLOB data type.
    - `current_needs_parens` -- some SQL dialects (e.g., BigQuery) require parentheses for the functions `current_date`, `current_time` and `current_timestamp`.
    - `union`, for specifying a nonstandard way of combining queries. All union queries now name each column in each subquery (required for `bigrquery`).
- New tests
    - `dbGetInfo(Result)` (rstats-db/DBI#55).
    - `dbListFields()` (#26).
    - New `package_name` test in `test_getting_started()`.
- Improved tests
    - Stress test now installs package in temporary library (before loading `DBI`) using `R CMD INSTALL` before loading DBI (rstats-db/RSQLite#128, #48).
    - Row count is now tested for equality but not identity, so that backends can return a numeric value > 2^31 at their discretion.
    - Call `dbRemoveTable()` instead of issuing `DROP` requests, the latter might be unsupported.
    - Use subqueries in queries that use `WHERE`.
    - Test that `dbClearResult()` on a closed result set raises a warning.
    - Expect a warning instead of an error for double disconnect (#50).
    - Move connection test that requires `dbFetch()` to `test_result()`.
    - Split `can_connect_and_disconnect` test.
    - Expect `DBI` to be in `Imports`, not in `Depends`.
- Removed tests
    - Remove test for `dbGetException()` (rstats-db/DBI#51).
- Bug fixes
    - Fix broken tests for quoting.
- Self-testing
    - Test `RPostgres`, `RMySQL`, `RSQLite` and `RKazam` as part of the Travis-CI tests (#52).
    - Travis CI now installs rstats-db/DBI, updated namespace imports (`dbiCheckCompliance()`, `dbListResults()`).
    - Use fork of `testthat`.
- Utilities
    - Return test results as named array of logical. Requires hadley/testthat#360, gracefully degrades with the CRAN version.
- Internal
    - Refactored the `get_info_()` tests to use a vector of names.
    - Use versioned dependency for DBI
    - Use unqualified calls to `dbBind()` again


Version 1.0 (2015-12-17)
===

- CRAN release
    - Eliminate errors on win-builder
    - Satisfy R CMD check
    - Use LGPL-2 license
    - Add RStudio as copyright holder
    - Move `devtools` package from "Imports" to "Suggests"


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
- Same as 0.2-5


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
