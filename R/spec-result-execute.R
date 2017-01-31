#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_result_execute Specification
NULL

#' spec_result_execute
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_execute <- list(
  execute_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(DBI::dbExecute)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbExecute()` always returns a
  execute_atomic = function(ctx) {
    with_connection({
      with_remove_test_table({
        query <- "CREATE TABLE test AS SELECT 1 AS a"

        ret <- dbExecute(con, query)
        #' scalar
        expect_equal(length(ret), 1)
        #' numeric
        expect_true(is.numeric(ret))
        #' that specifies the number of rows affected
        #' by the statement.
      })
    })
  },

  #' An error is raised when issuing a statement over a closed
  execute_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbExecute(con, "CREATE TABLE test AS SELECT 1 AS a"))
    })
  },

  #' or invalid connection,
  execute_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbExecute(con, "CREATE TABLE test AS SELECT 1 AS a"))
    })
  },

  #' if the syntax of the statement is invalid,
  execute_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbExecute(con, "CREATE"))
    })
  },

  #' or if the statement is not a non-`NA` string.
  execute_non_string = function(ctx) {
    with_connection({
      expect_error(dbExecute(con, character()))
      expect_error(dbExecute(con, letters))
      expect_error(dbExecute(con, NA_character_))
    })
  },

  #' @section Specification:
  execute_result_valid = function(ctx) {
    with_connection({
      with_remove_test_table({
        #' No warnings occur under normal conditions.
        expect_warning(dbExecute(con, "CREATE TABLE test AS SELECT 1 AS a"), NA)
      })
    })
  },

  NULL
)
