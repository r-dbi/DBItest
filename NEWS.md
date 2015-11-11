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
