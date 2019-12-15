#' spec_transaction_with_transaction
#' @usage NULL
#' @format NULL
#' @keywords internal
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
        dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

        dbWithTransaction(
          con,
          {
            dbWriteTable(con, "test", data.frame(a = 1L), append = TRUE)
            expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0:1))
          }
        )

        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0:1))
      })
    })
  },

  #' If the code raises an error, the transaction is instead aborted with
  #' [dbRollback()], and the error is propagated.
  with_transaction_failure = function(ctx) {
    name <- random_table_name()

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

        expect_error(
          dbWithTransaction(
            con,
            {
              dbWriteTable(con, "test", data.frame(a = 1L), append = TRUE)
              stop(name)
            }
          ),
          name,
          fixed = TRUE
        )

        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0L))
      })
    })
  },

  #' If the code calls `dbBreak()`, execution of the code stops and the
  #' transaction is silently aborted.
  with_transaction_break = function(ctx) {
    name <- random_table_name()

    with_connection({
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

        expect_error(
          dbWithTransaction(
            con,
            {
              dbWriteTable(con, "test", data.frame(a = 1L), append = TRUE)
              dbBreak()
            }
          ),
          NA
        )

        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0L))
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
