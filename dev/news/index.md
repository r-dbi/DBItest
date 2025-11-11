# Changelog

## DBItest 1.8.2.9007 (2025-11-10)

### Continuous integration

- Import from actions-sync, check carefully
  ([\#503](https://github.com/r-dbi/DBItest/issues/503)).

## DBItest 1.8.2.9006 (2025-11-08)

### Continuous integration

- Import from actions-sync, check carefully
  ([\#501](https://github.com/r-dbi/DBItest/issues/501)).

## DBItest 1.8.2.9005 (2025-11-04)

### Bug fixes

- Suppress warnings when creating invalid objects.

## DBItest 1.8.2.9004 (2025-09-06)

### Continuous integration

- Import from actions-sync, check carefully
  ([\#498](https://github.com/r-dbi/DBItest/issues/498)).

## DBItest 1.8.2.9003 (2025-09-05)

### Chore

- Auto-update from GitHub Actions
  ([\#496](https://github.com/r-dbi/DBItest/issues/496)).

## DBItest 1.8.2.9002 (2025-08-19)

### Chore

- Build-ignore.

### Continuous integration

- Import from actions-sync, check carefully
  ([\#476](https://github.com/r-dbi/DBItest/issues/476)).

- Use pak \[ci skip\].

- Allow tools \[ci skip\].

- Post comment on failure \[ci skip\].

- Checkout.

- Add Claude workflow.

### Documentation

- Add comprehensive comments with proper formatting
  ([\#427](https://github.com/r-dbi/DBItest/issues/427),
  [\#431](https://github.com/r-dbi/DBItest/issues/431)).

## DBItest 1.8.2.9001 (2025-05-04)

### Features

- Reinstate lint infrastructure as GHA
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#396](https://github.com/r-dbi/DBItest/issues/396)).

- Assign result in `send_query_stale_warning` to stabilize test
  ([@detule](https://github.com/detule),
  [\#373](https://github.com/r-dbi/DBItest/issues/373)).

- Improve failure mode of missing method test
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#405](https://github.com/r-dbi/DBItest/issues/405)).

- Use `trivial_query()` by default in `create_table_as()`
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#410](https://github.com/r-dbi/DBItest/issues/410),
  [\#420](https://github.com/r-dbi/DBItest/issues/420)).

- Test object returned by `dbUnquoteIdentifier()` is of class `Id`.

- Add an extra test for sanity in spec-sql-write-table
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#411](https://github.com/r-dbi/DBItest/issues/411)).

- New `tweaks(create_table_empty = )` tweak
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#406](https://github.com/r-dbi/DBItest/issues/406),
  [\#408](https://github.com/r-dbi/DBItest/issues/408)).

- Clarify warning message for lack of `skip =` hit among `spec_all`
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#398](https://github.com/r-dbi/DBItest/issues/398)).

### Chore

- Use purrr ([\#421](https://github.com/r-dbi/DBItest/issues/421)).

- IDE.

- Add build-time dependency vctrs
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#394](https://github.com/r-dbi/DBItest/issues/394),
  [\#395](https://github.com/r-dbi/DBItest/issues/395)).

### Continuous integration

- Need to install R.

- Run on noble ([\#425](https://github.com/r-dbi/DBItest/issues/425)).

- Import from actions-sync, check carefully
  ([\#423](https://github.com/r-dbi/DBItest/issues/423)).

- Import from actions-sync, check carefully
  ([\#419](https://github.com/r-dbi/DBItest/issues/419)).

- Import from actions-sync, check carefully
  ([\#418](https://github.com/r-dbi/DBItest/issues/418)).

- Import from actions-sync, check carefully
  ([\#417](https://github.com/r-dbi/DBItest/issues/417)).

- Import from actions-sync, check carefully
  ([\#416](https://github.com/r-dbi/DBItest/issues/416)).

- Import from actions-sync, check carefully
  ([\#415](https://github.com/r-dbi/DBItest/issues/415)).

- Import from actions-sync, check carefully
  ([\#414](https://github.com/r-dbi/DBItest/issues/414)).

- Import from actions-sync, check carefully
  ([\#413](https://github.com/r-dbi/DBItest/issues/413)).

### Documentation

- Fix bad links and a typo.

- Improve wording of `test_all(skip = )` description
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#397](https://github.com/r-dbi/DBItest/issues/397),
  [\#399](https://github.com/r-dbi/DBItest/issues/399)).

## DBItest 1.8.2.9000 (2024-12-09)

### Continuous integration

- Import from actions-sync, check carefully
  ([\#392](https://github.com/r-dbi/DBItest/issues/392)).

## DBItest 1.8.2 (2024-12-07)

### Chore

- More explicit message on constructive errors.

- Bump DBI dependency.

### Documentation

- Set BS version explicitly for now
  ([@maelle](https://github.com/maelle),
  [\#370](https://github.com/r-dbi/DBItest/issues/370)).

- Add package to `\link{}` targets.

### Testing

- Adapt tests to updates of the constructive package.

## DBItest 1.8.1 (2024-03-31)

### Features

- Allow multiple warnings in disconnect tests
  ([\#363](https://github.com/r-dbi/DBItest/issues/363)).

- Fix specification for Arrow tests
  ([\#357](https://github.com/r-dbi/DBItest/issues/357)).

- Show DBItest function in backtrace
  ([\#349](https://github.com/r-dbi/DBItest/issues/349),
  [\#354](https://github.com/r-dbi/DBItest/issues/354)).

- Specify `value` argument for `dbCreateTable()` and
  `dbCreateTableArrow()`
  ([\#345](https://github.com/r-dbi/DBItest/issues/345)).

- Enable two tests for `dbGetQueryArrow()`
  ([\#342](https://github.com/r-dbi/DBItest/issues/342)).

- Relax `dbListObjects()` spec
  ([\#339](https://github.com/r-dbi/DBItest/issues/339),
  [\#341](https://github.com/r-dbi/DBItest/issues/341)).

### Chore

- Avoid dplyr ([\#364](https://github.com/r-dbi/DBItest/issues/364)).

- Remove `.dots` argument to `test_select_with_null()`
  ([\#362](https://github.com/r-dbi/DBItest/issues/362)).

- Prefer `map()` over [`lapply()`](https://rdrr.io/r/base/lapply.html)
  ([\#361](https://github.com/r-dbi/DBItest/issues/361)) and `map_*()`
  over [`vapply()`](https://rdrr.io/r/base/lapply.html)
  ([\#356](https://github.com/r-dbi/DBItest/issues/356)).

- Bump DBI dependency to fix tests
  ([\#359](https://github.com/r-dbi/DBItest/issues/359)).

- Document sources for autogenerated files
  ([\#353](https://github.com/r-dbi/DBItest/issues/353)), add comments
  to generated code
  ([\#358](https://github.com/r-dbi/DBItest/issues/358)).

- Make test names unique, with a numeric suffix
  ([\#355](https://github.com/r-dbi/DBItest/issues/355)).

- Align with RSQLite
  ([\#351](https://github.com/r-dbi/DBItest/issues/351)).

- Replace unconditional skip with versioned skip
  ([\#347](https://github.com/r-dbi/DBItest/issues/347)).

- Consistent use of `skip_if_not_dbitest()`
  ([\#346](https://github.com/r-dbi/DBItest/issues/346)).

### Continuous integration

- Modernize backends checks.

### Documentation

- Use dbitemplate ([@maelle](https://github.com/maelle),
  [\#360](https://github.com/r-dbi/DBItest/issues/360)).

- Mention `dbBindArrow()` in documentation
  ([\#350](https://github.com/r-dbi/DBItest/issues/350)).

- Minor specification fixes
  ([\#344](https://github.com/r-dbi/DBItest/issues/344)).

## DBItest 1.8.0 (2023-12-21)

### Bug fixes

- Fix `create_roundtrip_keywords` and `create_roundtrip_quotes` tests
  ([\#283](https://github.com/r-dbi/DBItest/issues/283)).

### Features

- Relax specification of `dbUnquoteIdentifier()`, character vectors are
  now allowed too.

- Specify `dbFetchChunk()`
  ([\#331](https://github.com/r-dbi/DBItest/issues/331)),
  `dbFetchArrowChunk()`
  ([\#301](https://github.com/r-dbi/DBItest/issues/301)) and
  `dbBindArrow()`
  ([\#328](https://github.com/r-dbi/DBItest/issues/328)).

- Inline all tests for `dbBind()`
  ([\#326](https://github.com/r-dbi/DBItest/issues/326)).

- Require support for `dbFetch(n = NA)`
  ([\#296](https://github.com/r-dbi/DBItest/issues/296),
  [\#316](https://github.com/r-dbi/DBItest/issues/316)).

- New `allow_na_rows_affected` tweak to support `NA` values returned
  from `dbGetRowsAffected()`
  ([\#297](https://github.com/r-dbi/DBItest/issues/297),
  [\#312](https://github.com/r-dbi/DBItest/issues/312)).

- Switch to nanoarrow
  ([\#291](https://github.com/r-dbi/DBItest/issues/291)).

- Basic tests for the new `db*Arrow()` interface
  ([\#287](https://github.com/r-dbi/DBItest/issues/287)).

- New `skip_if_not_dbitest()`
  ([\#289](https://github.com/r-dbi/DBItest/issues/289)).

- `reexport` test uses interface for dev DBI if the backend is
  compatible with DBItest \> 1.7.3.

- Slightly better code generated for
  [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md)
  ([\#313](https://github.com/r-dbi/DBItest/issues/313)).

- Remove interface to dblog in the CRAN version.

### CI/CD

- Add adbi to check matrix
  ([\#314](https://github.com/r-dbi/DBItest/issues/314)).

- Reenable ODBC MySQL tests
  ([\#288](https://github.com/r-dbi/DBItest/issues/288)).

- Tweak `read_table_missing` test
  ([\#285](https://github.com/r-dbi/DBItest/issues/285)).

### Chore

- Remove rlang qualification
  ([\#332](https://github.com/r-dbi/DBItest/issues/332)).

- No longer need
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) twice
  for Arrow ([\#302](https://github.com/r-dbi/DBItest/issues/302),
  [\#330](https://github.com/r-dbi/DBItest/issues/330)).

- Consistent use of `skip_if_not_dbitest()`
  ([\#317](https://github.com/r-dbi/DBItest/issues/317)).

- Disable Arrow skips
  ([\#303](https://github.com/r-dbi/DBItest/issues/303)).

- Modernize `sql_union()`
  ([\#304](https://github.com/r-dbi/DBItest/issues/304)).

- Make better use of `trivial_df()`
  ([\#284](https://github.com/r-dbi/DBItest/issues/284)).

### Documentation

- Avoid error if RSQLite is not installed.

### Testing

- Run DBItest for SQLite as part of the checks here
  ([\#318](https://github.com/r-dbi/DBItest/issues/318)).

- Enable remaining Arrow tests
  ([\#307](https://github.com/r-dbi/DBItest/issues/307)).

- Fix checks without suggested packages
  ([\#300](https://github.com/r-dbi/DBItest/issues/300)).

## DBItest 1.7.3 (2022-10-18)

### Features

- Use and enable compatibility with testthat edition 3
  ([\#263](https://github.com/r-dbi/DBItest/issues/263),
  [\#268](https://github.com/r-dbi/DBItest/issues/268)). Complete
  removal of `expect_is()`
  ([@MichaelChirico](https://github.com/MichaelChirico),
  [\#257](https://github.com/r-dbi/DBItest/issues/257)).

- Adapt to new Arrow DBI generics
  ([\#265](https://github.com/r-dbi/DBItest/issues/265)).

- Better stack traces for visibility tests.

- `dbQuoteIdentifier()` roundtrip is tested for tables only
  ([@dpprdan](https://github.com/dpprdan),
  [\#256](https://github.com/r-dbi/DBItest/issues/256)).

- [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  also tests a test if it would normally be skipped.

### Chore

- Bump minimum DBI version to 1.1.3.

- Refactor DBI tests in preparation for inlining them.

### Bug fixes

- Correct cleanup even if `dbIsValid()` is not implemented.

## DBItest 1.7.2 (2021-12-17)

### Features

- [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md) gains
  `dbitest_version` argument to support targeting a specific version of
  the DBItest package. The default is 1.7.1
  ([\#236](https://github.com/r-dbi/DBItest/issues/236)).
- Reuse database connection for most tests
  ([\#245](https://github.com/r-dbi/DBItest/issues/245)).
- New `roundtrip_date_extended`, `roundtrip_timestamp_extended`,
  `append_roundtrip_date_extended` and
  `append_roundtrip_timestamp_extended` test dates between 1800 and 2999
  ([\#148](https://github.com/r-dbi/DBItest/issues/148),
  [\#249](https://github.com/r-dbi/DBItest/issues/249)).
- New `quote_literal_empty` test
  ([\#248](https://github.com/r-dbi/DBItest/issues/248)).
- New `bind_character_escape` test for binding special characters
  ([\#242](https://github.com/r-dbi/DBItest/issues/242)).
- New `bind_time_minutes_integer` test for integer durations.

### Bug fixes

- All column names are specified using lowercase on input, for
  compatibility with Redshift
  ([\#234](https://github.com/r-dbi/DBItest/issues/234)).
- `column_info_consistent` no longer tests mangling of column names
  ([\#181](https://github.com/r-dbi/DBItest/issues/181)).
- `spec_sql_append_table` test: Remove bad argument.

### Documentation

- Improve documentation: list `spec_` objects in pkgdown help index, add
  cross references
  ([\#128](https://github.com/r-dbi/DBItest/issues/128)).
- Add specification for `value` argument to
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
  ([\#235](https://github.com/r-dbi/DBItest/issues/235)).

### Internal

- Replace internal `with_result()`, `with_remove_test_tables()` and
  `with_rollback_on_error()` for better error traces
  ([\#184](https://github.com/r-dbi/DBItest/issues/184),
  [\#250](https://github.com/r-dbi/DBItest/issues/250),
  [\#251](https://github.com/r-dbi/DBItest/issues/251),
  [\#253](https://github.com/r-dbi/DBItest/issues/253)).
- Use
  [`palmerpenguins::penguins`](https://allisonhorst.github.io/palmerpenguins/reference/penguins.html)
  instead of `iris`
  ([\#241](https://github.com/r-dbi/DBItest/issues/241)).
- Fix MySQL ODBC test on GitHub Actions
  ([\#237](https://github.com/r-dbi/DBItest/issues/237)).
- Improve testthat 3e compatibility: remove
  [`testthat::expect_is()`](https://testthat.r-lib.org/reference/expect_is.html)
  and
  [`testthat::expect_that()`](https://testthat.r-lib.org/reference/expect_that.html)
  from tests ([\#231](https://github.com/r-dbi/DBItest/issues/231),
  [@michaelquinn32](https://github.com/michaelquinn32)).
- Decompose query used for testing `dbBind()`.

## DBItest 1.7.1 (2021-07-30)

### Features

- Many tests now emit simpler stack traces, because the connection is
  opened by the test driver and not by the test itself
  ([\#187](https://github.com/r-dbi/DBItest/issues/187)). Reduce usage
  of `with_remove_test_table()` for better stack traces on error
  ([\#196](https://github.com/r-dbi/DBItest/issues/196)). Remove
  `with_*connection()`
  ([\#193](https://github.com/r-dbi/DBItest/issues/193)).
- [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  shows DBI code via dblog
  ([\#217](https://github.com/r-dbi/DBItest/issues/217)) if
  `dblog = TRUE` ([\#226](https://github.com/r-dbi/DBItest/issues/226)).
- New `"bind_date_integer"`, `"bind_time_seconds"` and
  `"bind_time_hours"` tests
  ([\#218](https://github.com/r-dbi/DBItest/issues/218)).
- New `create_table_as` tweak
  ([\#131](https://github.com/r-dbi/DBItest/issues/131)).
- `"roundtrip_time"` and `"append_roundtrip_time"` tests now also test
  values of class `"difftime"` with units other than `"secs"`
  ([\#199](https://github.com/r-dbi/DBItest/issues/199)).
- All tables created by the tests have the `"dbit"` prefix. Almost all
  tests now use random table names to avoid collisions and unrelated
  test failures ([\#197](https://github.com/r-dbi/DBItest/issues/197)).
- `"roundtrip_timestamp"` tests now accept a time zone set by the
  database backend
  ([\#178](https://github.com/r-dbi/DBItest/issues/178),
  [\#198](https://github.com/r-dbi/DBItest/issues/198)).
- Support more than one class of each type in DBI backend packages.

### Bug fixes

- Fix input dataset in `"overwrite_table_missing"` test
  ([\#210](https://github.com/r-dbi/DBItest/issues/210),
  [@martinstuder](https://github.com/martinstuder)).
- Use original test name to decide if a test is skipped
  ([\#225](https://github.com/r-dbi/DBItest/issues/225)).
- Fix reexport test: skip if package is not installed, remove checks for
  deprecated functions and functions not reexported
  ([\#203](https://github.com/r-dbi/DBItest/issues/203)).

### Internal

- Requires DBI 1.1.1.
- Test odbc as part of the backend tests
  ([\#228](https://github.com/r-dbi/DBItest/issues/228)).
- Dynamic build matrix for backends
  ([\#221](https://github.com/r-dbi/DBItest/issues/221)).
- Compatibility with testthat 3.0.0
  ([\#207](https://github.com/r-dbi/DBItest/issues/207)).
- Switch to GitHub Actions
  ([\#201](https://github.com/r-dbi/DBItest/issues/201)).

## DBItest 1.7.0 (2019-12-16)

### Specifications

- Specify tests for `dbGetInfo()`.
- Specify `immediate` argument (r-dbi/DBI#268).
- Specify `dbCreateTable()` and `dbAppendTable()`
  ([\#169](https://github.com/r-dbi/DBItest/issues/169)).
- New `unquote_identifier_table_schema` test: Identifiers of the form
  `table.schema` can be processed with `dbUnquoteIdentifier()`.
- Fix `has_completed_statement` test
  ([\#176](https://github.com/r-dbi/DBItest/issues/176)).

### Testing infrastructure

- Document how to run tests externally and how to debug tests
  ([\#165](https://github.com/r-dbi/DBItest/issues/165)).
- `test_*()` gain new `run_only = NULL` argument that allow restricting
  the tests to be run with a positive match.
  [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  uses `run_only` instead of constructing a regular expression with
  negative lookahead. This helps troubleshooting a single test with
  `testthat::set_reporter(DebugReporter$new())` .
- [`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
  gains `default_skip` argument and uses the `DBIConnector` class.
- Support `NULL` default value in driver constructor
  ([\#171](https://github.com/r-dbi/DBItest/issues/171)).

### Internal

- Fulfill CII badge requirements
  ([\#179](https://github.com/r-dbi/DBItest/issues/179),
  [@TSchiefer](https://github.com/TSchiefer)).
- Use debugme.
- Require R 3.2.
- Avoid subsetting vectors out of bounds, for consistency with vctrs.

## DBItest 1.6.0 (2018-05-03)

### New checks

- Now checking that `Id()` is reexported.
- Support `temporary` argument in `dbRemoveTable()` (default: `FALSE`)
  (r-dbi/DBI#141).
- Added specification for the behavior in case of duplicate column names
  ([\#137](https://github.com/r-dbi/DBItest/issues/137)).
- The `bigint` argument to `dbConnect()` is now specified. Accepts
  `"integer64"`, `"integer"`, `"numeric"` and `"character"`, large
  integers are returned as values of that type
  ([\#133](https://github.com/r-dbi/DBItest/issues/133)).
- Add specification for partially filled `field.types` argument.
- Specify `dbRemoveTable(fail_if_missing = FALSE)` (r-dbi/DBI#197).
- Add specification for `dbColumnInfo()` (r-dbi/DBI#75).
- Add specification for `dbListFields()` (r-dbi/DBI#75).
- Test that named parameters are actually matched by name in `dbBind()`,
  by shuffling them
  ([\#138](https://github.com/r-dbi/DBItest/issues/138)).
- Explicitly specify default `row.names = FALSE` for `dbReadTable()` and
  `dbWriteTable()`
  ([\#139](https://github.com/r-dbi/DBItest/issues/139)).
- Add specification for writing 64-bit values, backends must support
  roundtripping values returned from the database
  ([\#146](https://github.com/r-dbi/DBItest/issues/146)).
- Add specification for the `params` argument to `dbGetQuery()`,
  `dbSendQuery()`, `dbExecute()` and `dbSendStatement()`
  ([\#159](https://github.com/r-dbi/DBItest/issues/159)).
- Add test for `dbQuoteIdentifier()`: “The names of the input argument
  are preserved in the output” (r-lib/DBI#173).
- Blob tests now also read and write zero bytes ().
- Add string encoded in Latin-1 to the character tests.
- Added test for `dbIsValid()` on stale connections.

### Removed checks

- Don’t test selecting untyped `NULL` anymore.
- Full interface compliance doesn’t require a method for
  `dbGetInfo(DBIDriver)` for now.
- Remove `"cannot_forget_disconnect"` test that fails on R-devel
  ([\#150](https://github.com/r-dbi/DBItest/issues/150)).
- Methods without `db` prefix are not checked for ellipsis in the
  signature anymore.
- Don’t specify `Inf` and `NaN` for lack of consistent support across
  DBMS ([\#142](https://github.com/r-dbi/DBItest/issues/142)).

### Updated/corrected checks

- Fix query that is supposed to generate a syntax error.
- Fix typo ([\#147](https://github.com/r-dbi/DBItest/issues/147),
  [@jonmcalder](https://github.com/jonmcalder)).
- Implement `POSIXlt` bind test correctly.
- Improve error detection for `dbBind()`.
- Redesign tests for `dbBind()`, now queries of the form
  `SELECT CASE WHEN (? = ?) AND (? IS NULL) THEN 1.5 ELSE 2.5` are
  issued. The original tests were inappropriate for RMariaDB, because an
  untyped placeholder is returned as a blob.
- Transaction tests now use `dbWriteTable()` instead of
  `dbCreateTable()`, because some DBMS don’t support transactions for
  DML.
- Fix timestamp tests for RMariaDB.
- Fix string constants.
- The `"roundtrip_timestamp"` test now correctly handles timezone
  information. The output timezone is ignored.
- Clear result in `spec_meta_get_info_result`
  ([\#143](https://github.com/r-dbi/DBItest/issues/143)).
- Use named argument for `n` in `dbGetQuery()` call.
- Minor fixes.

### Tweaks

- New tweak `blob_cast` allows specifying a conversion function to the
  BLOB data type.
- New `is_null_check` tweak that allows specifying a function that is
  used when checking values for `NULL`. Required for RPostgres.
- New `list_temporary_tables` tweak that can be enabled independently of
  `temporary_tables` to indicate that the DBMS does not support listing
  temporary tables.

### Infrastructure

- Allow running only a subset of tests in
  [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md) by
  specifying an environment variable.
- [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  and
  [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  return `NULL` invisibly.

### Internals

- Compatibility code if
  [`DBI::dbQuoteLiteral()`](https://dbi.r-dbi.org/reference/dbQuoteLiteral.html)
  is unavailable.
- New `trivial_query()` replaces many hard-coded queries and uses
  non-integer values for better compatibility with RMariaDB.
- Convert factor to character for iris data
  ([\#141](https://github.com/r-dbi/DBItest/issues/141)).

## DBItest 1.5-2 (2018-01-26)

- Fix test that fails with “noLD”.
- Fix NOTEs on R-devel.

## DBItest 1.5-1 (2017-12-10)

- Remove `"cannot_forget_disconnect"` test that fails on R-devel
  ([\#150](https://github.com/r-dbi/DBItest/issues/150)).

## DBItest 1.5 (2017-06-18)

Finalize specification. Most tests now come with a corresponding prose,
only those where the behavior is not finally decided don’t have a prose
version yet ([\#88](https://github.com/r-dbi/DBItest/issues/88)).

### New tests

- Test behavior of methods in presence of placeholders
  ([\#120](https://github.com/r-dbi/DBItest/issues/120)).
- Test column name mismatch behavior for appending tables
  ([\#93](https://github.com/r-dbi/DBItest/issues/93)).
- Test that `dbBind()` against factor works but raises a warning
  ([\#91](https://github.com/r-dbi/DBItest/issues/91)).
- Test roundtrip of alternating empty and non-empty strings
  ([\#42](https://github.com/r-dbi/DBItest/issues/42)).
- Test multiple columns of different types in one statement or table
  ([\#35](https://github.com/r-dbi/DBItest/issues/35)).
- Test `field.types` argument to `dbWriteTable()`
  ([\#12](https://github.com/r-dbi/DBItest/issues/12)).
- Added tests for invalid or closed connection argument to all methods
  that expect a connection as first argument
  ([\#117](https://github.com/r-dbi/DBItest/issues/117)).
- Enabled test that tests a missing `dbDisconnect()`.
- Add test for unambiguous escaping of identifiers
  (rstats-db/RSQLite#123).
- Reenable tests for visibility
  ([\#89](https://github.com/r-dbi/DBItest/issues/89)).
- Fix and specify 64-bit roundtrip test.
- 64-bit integers only need to be coercible to `numeric` and `character`
  ([\#74](https://github.com/r-dbi/DBItest/issues/74)).
- Added roundtrip test for time values
  ([\#14](https://github.com/r-dbi/DBItest/issues/14)).
- Added tweaks for handling date, time, timestamp, …
  ([\#53](https://github.com/r-dbi/DBItest/issues/53),
  [\#76](https://github.com/r-dbi/DBItest/issues/76)).
- Test that `dbFetch()` on update-only query returns warning
  ([\#66](https://github.com/r-dbi/DBItest/issues/66)).

### Adapted tests

- `NULL` is a valid value for the `row.names` argument, same as `FALSE`.
- A column named `row_names` receives no special handling
  ([\#54](https://github.com/r-dbi/DBItest/issues/54)).
- A warning (not an error anymore) is expected when calling
  `dbDisconnect()` on a closed or invalid connection.
- `row.names = FALSE` is now the default for methods that read or write
  tables.
- Add `NA` to beginning and end of columns in table roundtrip tests
  ([\#24](https://github.com/r-dbi/DBItest/issues/24)).
- Stricter tests for confusion of named and unnamed SQL parameters and
  placeholders ([\#107](https://github.com/r-dbi/DBItest/issues/107)).
- Also check names of all returned data frames.
- The return value for all calls to `dbGetQuery()`, `dbFetch()`, and
  `dbReadTable()` is now checked for consistency (all columns have the
  same length, length matches number of rows)
  ([\#126](https://github.com/r-dbi/DBItest/issues/126)).
- Removed stress tests that start a new session.
- Allow `hms` (or other subclasses of `difftime`) to be returned as time
  class ([\#135](https://github.com/r-dbi/DBItest/issues/135),
  [@jimhester](https://github.com/jimhester)).
- Test that dates are of type `numeric`
  ([\#99](https://github.com/r-dbi/DBItest/issues/99),
  [@jimhester](https://github.com/jimhester)).
- Replace `POSIXlt` by `POSIXct`
  ([\#100](https://github.com/r-dbi/DBItest/issues/100),
  [@jimhester](https://github.com/jimhester)).
- Use `"PST8PDT"` instead of `"PST"` as time zone
  ([\#110](https://github.com/r-dbi/DBItest/issues/110),
  [@thrasibule](https://github.com/thrasibule)).
- Added tests for support of `blob` objects (input and output), but
  backends are not required to return `blob` objects
  ([\#98](https://github.com/r-dbi/DBItest/issues/98)).
- The `logical_return`, `date_typed` and `timestamp_typed` tweaks are
  respected by the bind tests.
- Fixed tests involving time comparison; now uses UTC timezone and
  compares against a `difftime`.
- Tests for roundtrip of character values now includes tabs, in addition
  to many other special characters
  ([\#85](https://github.com/r-dbi/DBItest/issues/85)).
- Make sure at least one table exists in the `dbListTables()` test.
- Fix roundtrip tests for raw columns: now expecting `NULL` and not `NA`
  entries for SQL NULL values.
- Fix `expect_equal_df()` for list columns.
- Testing that a warning is given if the user forgets to call
  `dbDisconnect()` or `dbClearResult()`
  ([\#103](https://github.com/r-dbi/DBItest/issues/103)).
- Numeric roundtrip accepts conversion of `NaN` to `NA`
  ([\#79](https://github.com/r-dbi/DBItest/issues/79)).

### Internal

- Fix R CMD check errors.
- Internal consistency checks
  ([\#114](https://github.com/r-dbi/DBItest/issues/114)).
- Skip patterns that don’t match any of the tests now raise a warning
  ([\#84](https://github.com/r-dbi/DBItest/issues/84)).
- New
  [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  to test individual tests
  ([\#136](https://github.com/r-dbi/DBItest/issues/136)).
- Use desc instead of devtools
  ([\#40](https://github.com/r-dbi/DBItest/issues/40)).
- All unexpected warnings are now reported as test failures
  ([\#113](https://github.com/r-dbi/DBItest/issues/113)).
- `DBItest_tweaks` class gains a `$` method, accessing an undefined
  tweak now raises an error.
- The arguments of the
  [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md)
  function now have default values that further describe their intended
  usage.
- New `with_closed_connection(ctx = ctx, )`,
  `with_invalid_connection(ctx = ctx, )`, `with_result()` and
  `with_remove_test_table()` helpers, and `expect_visible()`,
  `expect_inbisible_true()`, and `expect_equal_df()` expectations for
  more concise tests.

## DBItest 1.4 (2016-12-02)

### DBI specification

- Use markdown in documentation.
- Description of parametrized queries and statements
  ([\#88](https://github.com/r-dbi/DBItest/issues/88)).
- New hidden `DBIspec-wip` page for work-in-progress documentation.
- Get rid of “Format” and “Usage” sections, and aliases, in the specs.

### Tests

- Not testing for presence of `max.connections` element in
  `dbGetInfo(Driver)` (rstats-db/DBI#56).
- Test multi-row binding for queries and statements
  ([\#96](https://github.com/r-dbi/DBItest/issues/96)).
- New `ellipsis` check that verifies that all implemented DBI methods
  contain `...` in their formals. This excludes `show()` and all methods
  defined in this or other packages.
- Refactored `bind_` tests to use the new `parameter_pattern` tweak
  ([\#95](https://github.com/r-dbi/DBItest/issues/95)).
- Rough draft of transaction tests
  ([\#36](https://github.com/r-dbi/DBItest/issues/36)).
- New `fetch_zero_rows` test, split from `fetch_premature_close`.
- The “compliance” test tests that the backend package exports exactly
  one subclass of each DBI virtual class.
- Document and enhance test for `dbDataType("DBIDriver", "ANY")`
  ([\#88](https://github.com/r-dbi/DBItest/issues/88)).
- Minor corrections for “bind” tests.

### Internal

- Isolate stress tests from main test suite
  ([\#92](https://github.com/r-dbi/DBItest/issues/92)).
- Refactor test specification in smaller modules, isolated from actual
  test execution ([\#81](https://github.com/r-dbi/DBItest/issues/81)).
  This breaks the documentation of the tests, which will be substituted
  by a DBI specification in prose.
- Align description of binding with code.
- Refactor tests for `dbBind()`, test is run by `BindTester` class, and
  behavior is specified by members and by instances of the new
  `BindTesterExtra` class.
- The `skip` argument to the `test_()` functions is again evaluated with
  `perl = TRUE` to support negative lookaheads
  ([\#33](https://github.com/r-dbi/DBItest/issues/33)).
- Use `dbSendStatement()` and `dbExecute()` where appropriate.
- Avoid empty subsections in Rd documentation to satisfy `R CMD check`
  ([\#81](https://github.com/r-dbi/DBItest/issues/81)).

## DBItest 1.3 (2016-07-07)

### Bug fixes

- Fix `read_table` test when the backend actually returns the data in a
  different order.

### New tests

- Test `dbDataType()` on connections
  ([\#69](https://github.com/r-dbi/DBItest/issues/69),
  [\#75](https://github.com/r-dbi/DBItest/issues/75),
  [@imanuelcostigan](https://github.com/imanuelcostigan)).
- Check returned strings for UTF-8 encoding
  ([\#72](https://github.com/r-dbi/DBItest/issues/72)).
- Repeated `dbBind()` + `dbFetch()` on the same result set
  ([\#51](https://github.com/r-dbi/DBItest/issues/51)).

### Features

- [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md) gains
  an `...` as first argument to support future/deprecated tweaks (with a
  warning), and also to avoid unnamed arguments
  ([\#83](https://github.com/r-dbi/DBItest/issues/83)).
- `testthat` now shows a more accurate location for the source of
  errors, failures, and skips
  ([\#78](https://github.com/r-dbi/DBItest/issues/78)).
- Aggregate skipped tests, only one `skip()` call per test function.
- Indicate that some tests are optional in documentation
  ([\#15](https://github.com/r-dbi/DBItest/issues/15)).

### Internal

- New `constructor_relax_args` tweak, currently not queried.
- The `ctx` argument is now explicit in the test functions.
- Change underscores to dashes in file names.
- Remove `testthat` compatibility hack.
- New `all_have_utf8_or_ascii_encoding()` which vectorizes
  `has_utf8_or_ascii_encoding()`.
- Test on AppVeyor ([\#73](https://github.com/r-dbi/DBItest/issues/73)).
- Work around regression in R 3.3.0 (fix scheduled for R 3.3.1) which
  affected stress tests.

## DBItest 1.2 (2016-05-21)

- Infrastructure
  - Support names for contexts ([@hoesler](https://github.com/hoesler),
    [\#67](https://github.com/r-dbi/DBItest/issues/67)).
  - The `skip` argument to the test functions is now treated as a Perl
    regular expression to allow negative lookahead. Use
    `skip = "(?!test_regex).*"` to choose a single test to run
    ([\#33](https://github.com/r-dbi/DBItest/issues/33)).
  - Added encoding arguments to non-ASCII string constants
    ([\#60](https://github.com/r-dbi/DBItest/issues/60),
    [@hoesler](https://github.com/hoesler)).
- Improve tests
  - `simultaneous_connections` test always closes all connections on
    exit ([@hoesler](https://github.com/hoesler),
    [\#68](https://github.com/r-dbi/DBItest/issues/68)).
  - More generic compliance check
    ([@hoesler](https://github.com/hoesler),
    [\#61](https://github.com/r-dbi/DBItest/issues/61)).
  - Update documentation to reflect test condition
    ([@imanuelcostigan](https://github.com/imanuelcostigan),
    [\#70](https://github.com/r-dbi/DBItest/issues/70)).
- `testthat` dependency
  - Import all of `testthat` to avoid `R CMD check` warnings.
  - Compatibility with dev version of `testthat`
    ([\#62](https://github.com/r-dbi/DBItest/issues/62)).
- Improve Travis builds
  - Use container-based builds on Travis.
  - Install `RPostgres` and `RMySQL` from `rstats-db`.
  - Install `DBI` and `testthat` from GitHub.

## Version 1.1 (2016-02-12)

- New feature: tweaks
  - New argument `tweaks` to
    [`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
    ([\#49](https://github.com/r-dbi/DBItest/issues/49)).
  - New [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md),
    essentially constructs a named list of tweaks but with predefined
    and documented argument names.
  - `constructor_name`, respected by the `constructor.*` tests.
  - `strict_identifier`, if `TRUE` all identifier must be syntactic
    names even if quoted. The quoting test is now split, and a part is
    ignored conditional to this tweak. The `roundtrip_quotes` tests also
    respects this tweak.
  - `omit_blob_tests` for DBMS that don’t have a BLOB data type.
  - `current_needs_parens` – some SQL dialects (e.g., BigQuery) require
    parentheses for the functions `current_date`, `current_time` and
    `current_timestamp`.
  - `union`, for specifying a nonstandard way of combining queries. All
    union queries now name each column in each subquery (required for
    `bigrquery`).
- New tests
  - `dbGetInfo(Result)` (rstats-db/DBI#55).
  - `dbListFields()`
    ([\#26](https://github.com/r-dbi/DBItest/issues/26)).
  - New `package_name` test in
    [`test_getting_started()`](https://dbitest.r-dbi.org/dev/reference/test_getting_started.md).
- Improved tests
  - Stress test now installs package in temporary library (before
    loading `DBI`) using `R CMD INSTALL` before loading DBI
    (rstats-db/RSQLite#128,
    [\#48](https://github.com/r-dbi/DBItest/issues/48)).
  - Row count is now tested for equality but not identity, so that
    backends can return a numeric value \> 2^31 at their discretion.
  - Call `dbRemoveTable()` instead of issuing `DROP` requests, the
    latter might be unsupported.
  - Use subqueries in queries that use `WHERE`.
  - Test that `dbClearResult()` on a closed result set raises a warning.
  - Expect a warning instead of an error for double disconnect
    ([\#50](https://github.com/r-dbi/DBItest/issues/50)).
  - Move connection test that requires `dbFetch()` to
    [`test_result()`](https://dbitest.r-dbi.org/dev/reference/test_result.md).
  - Split `can_connect_and_disconnect` test.
  - Expect `DBI` to be in `Imports`, not in `Depends`.
- Removed tests
  - Remove test for `dbGetException()` (rstats-db/DBI#51).
- Bug fixes
  - Fix broken tests for quoting.
- Self-testing
  - Test `RPostgres`, `RMySQL`, `RSQLite` and `RKazam` as part of the
    Travis-CI tests
    ([\#52](https://github.com/r-dbi/DBItest/issues/52)).
  - Travis CI now installs rstats-db/DBI, updated namespace imports
    (`dbiCheckCompliance()`, `dbListResults()`).
  - Use fork of `testthat`.
- Utilities
  - Return test results as named array of logical. Requires
    hadley/testthat#360, gracefully degrades with the CRAN version.
- Internal
  - Refactored the `get_info_()` tests to use a vector of names.
  - Use versioned dependency for DBI
  - Use unqualified calls to `dbBind()` again

## Version 1.0 (2015-12-17)

- CRAN release
  - Eliminate errors on win-builder
  - Satisfy R CMD check
  - Use LGPL-2 license
  - Add RStudio as copyright holder
  - Move `devtools` package from “Imports” to “Suggests”

## Version 0.3 (2015-11-15)

- Feature-complete, ready for review
- Tests from the proposal
  - Add missing methods to compliance check
  - Add simple read-only test
    ([\#27](https://github.com/r-dbi/DBItest/issues/27))
  - Add stress tests for repeated load/unload (with and without
    connecting) in new R session
    ([\#2](https://github.com/r-dbi/DBItest/issues/2)),
  - Migrate all tests from existing backends
    ([\#28](https://github.com/r-dbi/DBItest/issues/28))
  - Refactor `data_` tests to use a worker function `test_select()`
  - Test tables with `NA` values above and below the non-`NA` value in
    `data_` tests
  - Test return values and error conditions for `dbBind()` and
    `dbClearResult()`
    ([\#31](https://github.com/r-dbi/DBItest/issues/31))
  - Test vectorization of `dbQuoteString()` and `dbQuoteIdentifier()`
    ([\#18](https://github.com/r-dbi/DBItest/issues/18))
  - Test that dates have `integer` as underlying data type
    ([\#9](https://github.com/r-dbi/DBItest/issues/9))
  - Roundtrip tests sort output table to be sure
    ([\#32](https://github.com/r-dbi/DBItest/issues/32))
  - Test `NA` to `NULL` conversion in `dbQuoteString()`, and false
    friends ([\#23](https://github.com/r-dbi/DBItest/issues/23))
  - Enhance test for `dbQuoteIdentifier()`
    ([\#30](https://github.com/r-dbi/DBItest/issues/30))
- Style
  - Avoid using [`data.frame()`](https://rdrr.io/r/base/data.frame.html)
    for date and time columns
    ([\#10](https://github.com/r-dbi/DBItest/issues/10))
  - Use `expect_identical()` instead of `expect_equal()` in many places
    ([\#13](https://github.com/r-dbi/DBItest/issues/13))
  - Catch all errors in
    [`on.exit()`](https://rdrr.io/r/base/on.exit.html) handlers via
    `expect_error()`
    ([\#20](https://github.com/r-dbi/DBItest/issues/20)).
  - Combine “meta” tests into new
    [`test_meta()`](https://dbitest.r-dbi.org/dev/reference/test_meta.md)
    ([\#37](https://github.com/r-dbi/DBItest/issues/37))
- Documentation
  - New “test” vignette
    ([\#16](https://github.com/r-dbi/DBItest/issues/16))
  - Add package documentation
    ([\#38](https://github.com/r-dbi/DBItest/issues/38))
- Same as 0.2-5

## Version 0.2 (2015-11-11)

- Tests from the proposal
  - SQL
  - Metadata
  - DBI compliance (not testing read-only yet)
- Migrate most of the tests from RMySQL
- Test improvements
  - Test BLOB data type
    ([\#17](https://github.com/r-dbi/DBItest/issues/17))
  - Check actual availability of type returned by `dbDataType()`
    ([\#19](https://github.com/r-dbi/DBItest/issues/19))
- Testing infrastructure
  - Disambiguate test names
    ([\#21](https://github.com/r-dbi/DBItest/issues/21))
  - Use regex matching for deciding skipped tests, skip regex must match
    the entire test name
- Documentation
  - Document all tests in each test function using the new inline
    documentation feature of roxygen2
  - Improve documentation for
    [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md):
    Tests are listed in new “Tests” section
  - Add brief instructions to README
- Move repository to rstats-db namespace
- Same as 0.1-6

## Version 0.1 (2015-10-11)

- First GitHub release
- Builds successfully on Travis
- Testing infrastructure
  - Test context
  - Skipped tests call `skip()`
  - Function
    [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
    that runs all tests
- Tests from the proposal
  - Getting started
  - Driver
  - Connection
  - Results
- Code formatting is checked with lintr
- Same as 0.0-5
