#' spec_transaction_with_transaction
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_transaction_with_transaction <- list(
  with_transaction_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbWithTransaction)), c("conn", "code", "..."))
  },

  #' @return
  #' `dbWithTransaction()` returns the value of the executed code.
  with_transaction_return_value = function(ctx) {
    name <- random_table_name()
    with_connection({
      expect_identical(dbWithTransaction(con, name), name)
    })
  },

  #' Failure to initiate the transaction
  #' (e.g., if the connection is closed
  with_transaction_error_closed = function(ctx) {
    with_closed_connection({
      expect_error(dbWithTransaction(con, NULL))
    })
  },

  #' or invalid
  with_transaction_error_invalid = function(ctx) {
    with_invalid_connection({
      expect_error(dbWithTransaction(con, NULL))
    })
  },

  #' of if [dbBegin()] has been called already)
  with_transaction_error_nested = function(ctx) {
    with_connection({
      dbBegin(con)
      #' gives an error.
      expect_error(dbWithTransaction(con, NULL))
      dbRollback(con)
    })
  },

  #' @section Specification:
  #' `dbWithTransaction()` initiates a transaction with `dbBegin()`, executes
  #' the code given in the `code` argument, and commits the transaction with
  #' [dbCommit()].
  with_transaction_success = function(ctx) {
    with_connection({
      with_remove_test_table({
        expect_false(dbExistsTable(con, "test"))

        dbWithTransaction(
          con,
          {
            dbExecute(con, paste0("CREATE TABLE test (a ", dbDataType(con, 0L), ")"))
            dbExecute(con, paste0("INSERT INTO test (a) VALUES (1)"))
            expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 1))
          }
        )

        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 1))
      })
    })
  },

  #' If the code raises an error, the transaction is instead aborted with
  #' [dbRollback()], and the error is propagated.
  with_transaction_failure = function(ctx) {
    name <- random_table_name()

    with_connection({
      with_remove_test_table({
        expect_false(dbExistsTable(con, "test"))

        expect_error(
          dbWithTransaction(
            con,
            {
              dbExecute(con, paste0("CREATE TABLE test (a ", dbDataType(con, 0L), ")"))
              dbExecute(con, paste0("INSERT INTO test (a) VALUES (1)"))
              stop(name)
            }
          ),
          name,
          fixed = TRUE
        )

        expect_false(dbExistsTable(con, "test"))
      })
    })
  },

  #' If the code calls `dbBreak()`, execution of the code stops and the
  #' transaction is silently aborted.
  with_transaction_break = function(ctx) {
    name <- random_table_name()

    with_connection({
      with_remove_test_table({
        expect_false(dbExistsTable(con, "test"))

        expect_error(
          dbWithTransaction(
            con,
            {
              dbExecute(con, paste0("CREATE TABLE test (a ", dbDataType(con, 0L), ")"))
              dbExecute(con, paste0("INSERT INTO test (a) VALUES (1)"))
              dbBreak()
            }
          ),
          NA
        )

        expect_false(dbExistsTable(con, "test"))
      })
    })
  },

  #' All side effects caused by the code
  with_transaction_side_effects = function(ctx) {
    with_connection({
      expect_false(exists("a", inherits = FALSE))
      #' (such as the creation of new variables)
      dbWithTransaction(con, a <- 42)
      #' propagate to the calling environment.
      expect_identical(get0("a", inherits = FALSE), 42)
    })
  },

  NULL
)
