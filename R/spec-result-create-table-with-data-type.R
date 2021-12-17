#' spec_result_create_table_with_data_type
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_create_table_with_data_type <- list(
  #' @section Specification:
  #' All data types returned by `dbDataType()` are usable in an SQL statement
  #' of the form
  data_type_create_table = function(ctx, con) {
    check_connection_data_type <- function(value) {
      table_name <- random_table_name()
      local_remove_test_table(con, table_name)
      #' `"CREATE TABLE test (a ...)"`.
      query <- paste0("CREATE TABLE ", table_name, " (a ", dbDataType(con, value), ")")
      eval(bquote(dbExecute(con, .(query))))
    }

    expect_conn_has_data_type <- function(value) {
      eval(bquote(
        expect_error(check_connection_data_type(.(value)), NA)
      ))
    }

    expect_conn_has_data_type(logical(1))
    expect_conn_has_data_type(integer(1))
    expect_conn_has_data_type(numeric(1))
    expect_conn_has_data_type(character(1))
    expect_conn_has_data_type(Sys.Date())
    expect_conn_has_data_type(Sys.time())
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      expect_conn_has_data_type(list(as.raw(0:10)))
    }
  },
  #
  NULL
)
