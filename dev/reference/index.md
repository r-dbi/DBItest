# Package index

## Test setup

Functions to include in a backend to enable DBI tests.

- [`DBItest`](https://dbitest.r-dbi.org/dev/reference/DBItest-package.md)
  [`DBItest-package`](https://dbitest.r-dbi.org/dev/reference/DBItest-package.md)
  : DBItest: Testing DBI Backends
- [`make_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
  [`set_default_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
  [`get_default_context()`](https://dbitest.r-dbi.org/dev/reference/context.md)
  : Test contexts
- [`tweaks()`](https://dbitest.r-dbi.org/dev/reference/tweaks.md) :
  Tweaks for DBI tests

## Test runners

Functions that run a set of tests.

- [`test_all()`](https://dbitest.r-dbi.org/dev/reference/test_all.md)
  [`test_some()`](https://dbitest.r-dbi.org/dev/reference/test_all.md) :
  Run all tests

## Getting started

- [`test_getting_started()`](https://dbitest.r-dbi.org/dev/reference/test_getting_started.md)
  : Getting started with testing
- [`spec_getting_started`](https://dbitest.r-dbi.org/dev/reference/spec_getting_started.md)
  : spec_getting_started

## The “Driver” class

- [`test_driver()`](https://dbitest.r-dbi.org/dev/reference/test_driver.md)
  : Test the "Driver" class
- [`spec_driver_connect`](https://dbitest.r-dbi.org/dev/reference/spec_driver_connect.md)
  : spec_driver_connect
- [`spec_driver_constructor`](https://dbitest.r-dbi.org/dev/reference/spec_driver_constructor.md)
  : spec_driver_constructor
- [`spec_driver_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_driver_data_type.md)
  : spec_driver_data_type
- [`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_driver_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_connection_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_meta_get_info_result`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  : spec_driver_get_info

## The “Connection” class

- [`test_connection()`](https://dbitest.r-dbi.org/dev/reference/test_connection.md)
  : Test the "Connection" class
- [`spec_connection_disconnect`](https://dbitest.r-dbi.org/dev/reference/spec_connection_disconnect.md)
  : spec_connection_disconnect
- [`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_driver_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_connection_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_meta_get_info_result`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  : spec_driver_get_info

## The “Result” class

- [`test_result()`](https://dbitest.r-dbi.org/dev/reference/test_result.md)
  : Test the "Result" class
- [`spec_result_clear_result`](https://dbitest.r-dbi.org/dev/reference/spec_result_clear_result.md)
  : spec_result_clear_result
- [`spec_result_create_table_with_data_type`](https://dbitest.r-dbi.org/dev/reference/spec_result_create_table_with_data_type.md)
  : spec_result_create_table_with_data_type
- [`spec_result_execute`](https://dbitest.r-dbi.org/dev/reference/spec_result_execute.md)
  : spec_result_execute
- [`spec_result_fetch`](https://dbitest.r-dbi.org/dev/reference/spec_result_fetch.md)
  : spec_result_fetch
- [`spec_result_get_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_get_query.md)
  : spec_result_get_query
- [`spec_result_roundtrip`](https://dbitest.r-dbi.org/dev/reference/spec_result_roundtrip.md)
  : spec_result_roundtrip
- [`spec_result_send_query`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_query.md)
  : spec_result_send_query
- [`spec_result_send_statement`](https://dbitest.r-dbi.org/dev/reference/spec_result_send_statement.md)
  : spec_result_send_statement

## SQL methods

- [`test_sql()`](https://dbitest.r-dbi.org/dev/reference/test_sql.md) :
  Test SQL methods
- [`spec_sql_append_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_append_table.md)
  : spec_sql_append_table
- [`spec_sql_create_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_create_table.md)
  : spec_sql_create_table
- [`spec_sql_exists_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_exists_table.md)
  : spec_sql_exists_table
- [`spec_sql_list_fields`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_fields.md)
  : spec_sql_list_fields
- [`spec_sql_list_objects`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_objects.md)
  : spec_sql_list_objects
- [`spec_sql_list_tables`](https://dbitest.r-dbi.org/dev/reference/spec_sql_list_tables.md)
  : spec_sql_list_tables
- [`spec_sql_quote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_identifier.md)
  : spec_sql_quote_identifier
- [`spec_sql_quote_literal`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_literal.md)
  : spec_sql_quote_literal
- [`spec_sql_quote_string`](https://dbitest.r-dbi.org/dev/reference/spec_sql_quote_string.md)
  : spec_sql_quote_string
- [`spec_sql_read_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_read_table.md)
  : spec_sql_read_table
- [`spec_sql_remove_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_remove_table.md)
  : spec_sql_remove_table
- [`spec_sql_unquote_identifier`](https://dbitest.r-dbi.org/dev/reference/spec_sql_unquote_identifier.md)
  : spec_sql_unquote_identifier
- [`spec_sql_write_table`](https://dbitest.r-dbi.org/dev/reference/spec_sql_write_table.md)
  : spec_sql_write_table

## Metadata functions

- [`test_meta()`](https://dbitest.r-dbi.org/dev/reference/test_meta.md)
  : Test metadata functions
- [`spec_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_driver_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_connection_get_info`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  [`spec_meta_get_info_result`](https://dbitest.r-dbi.org/dev/reference/spec_get_info.md)
  : spec_driver_get_info
- [`spec_meta_bind`](https://dbitest.r-dbi.org/dev/reference/spec_meta_bind.md)
  : spec_meta_bind
- [`spec_meta_column_info`](https://dbitest.r-dbi.org/dev/reference/spec_meta_column_info.md)
  : spec_meta_column_info
- [`spec_meta_get_row_count`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_row_count.md)
  : spec_meta_get_row_count
- [`spec_meta_get_rows_affected`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_rows_affected.md)
  : spec_meta_get_rows_affected
- [`spec_meta_get_statement`](https://dbitest.r-dbi.org/dev/reference/spec_meta_get_statement.md)
  : spec_meta_get_statement
- [`spec_meta_has_completed`](https://dbitest.r-dbi.org/dev/reference/spec_meta_has_completed.md)
  : spec_meta_has_completed
- [`spec_meta_is_valid`](https://dbitest.r-dbi.org/dev/reference/spec_meta_is_valid.md)
  : spec_meta_is_valid

## Transaction functions

- [`test_transaction()`](https://dbitest.r-dbi.org/dev/reference/test_transaction.md)
  : Test transaction functions
- [`spec_transaction_begin_commit_rollback`](https://dbitest.r-dbi.org/dev/reference/spec_transaction_begin_commit_rollback.md)
  : spec_transaction_begin_commit_rollback
- [`spec_transaction_with_transaction`](https://dbitest.r-dbi.org/dev/reference/spec_transaction_with_transaction.md)
  : spec_transaction_with_transaction

## Arrow functions

- [`test_arrow()`](https://dbitest.r-dbi.org/dev/reference/test_arrow.md)
  : Test Arrow methods
- [`spec_arrow_append_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_append_table_arrow.md)
  : spec_arrow_append_table_arrow
- [`spec_arrow_create_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_create_table_arrow.md)
  : spec_arrow_create_table_arrow
- [`spec_arrow_fetch_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow.md)
  : spec_arrow_fetch_arrow
- [`spec_arrow_fetch_arrow_chunk`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_fetch_arrow_chunk.md)
  : spec_arrow_fetch_arrow_chunk
- [`spec_arrow_get_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_get_query_arrow.md)
  : spec_arrow_get_query_arrow
- [`spec_arrow_read_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_read_table_arrow.md)
  : spec_arrow_read_table_arrow
- [`spec_arrow_send_query_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_send_query_arrow.md)
  : spec_result_send_query
- [`spec_arrow_write_table_arrow`](https://dbitest.r-dbi.org/dev/reference/spec_arrow_write_table_arrow.md)
  : spec_arrow_write_table_arrow

## Full compliance to DBI

- [`test_compliance()`](https://dbitest.r-dbi.org/dev/reference/test_compliance.md)
  : Test full compliance to DBI
- [`spec_compliance_methods`](https://dbitest.r-dbi.org/dev/reference/spec_compliance_methods.md)
  : spec_compliance_methods
