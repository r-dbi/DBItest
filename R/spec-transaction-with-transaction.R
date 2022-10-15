#' spec_transaction_with_transaction
#' @family transaction specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_transaction_with_transaction <- list(
  with_transaction_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbWithTransaction)), c("conn", "code", "..."))
  },

  with_transaction_return_value = function(con) {
    #' @return
    #' `dbWithTransaction()` returns the value of the executed code.
    name <- random_table_name()
    expect_identical(dbWithTransaction(con, name), name)
  },

  #'
  with_transaction_error_closed = function(ctx, closed_con) {
    #' @section Failure modes:
    #' Failure to initiate the transaction
    #' (e.g., if the connection is closed
    expect_error(dbWithTransaction(closed_con, NULL))
  },

  with_transaction_error_invalid = function(ctx, invalid_con) {
    #' or invalid
    expect_error(dbWithTransaction(invalid_con, NULL))
  },

  with_transaction_error_nested = function(con) {
    #' of if [dbBegin()] has been called already)
    dbBegin(con)
    #' gives an error.
    expect_error(dbWithTransaction(con, NULL))
    dbRollback(con)
  },

  with_transaction_success = function(con, table_name) {
    #' @section Specification:
    #' `dbWithTransaction()` initiates a transaction with `dbBegin()`, executes
    #' the code given in the `code` argument, and commits the transaction with
    #' [dbCommit()].
    dbWriteTable(con, table_name, data.frame(a = 0L), overwrite = TRUE)

    dbWithTransaction(
      con,
      {
        dbWriteTable(con, table_name, data.frame(a = 1L), append = TRUE)
        expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0:1))
      }
    )

    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0:1))
  },

  with_transaction_failure = function(con, table_name) {
    #' If the code raises an error, the transaction is instead aborted with
    #' [dbRollback()], and the error is propagated.
    dbWriteTable(con, table_name, data.frame(a = 0L), overwrite = TRUE)

    name <- random_table_name()
    expect_error(
      dbWithTransaction(
        con,
        {
          dbWriteTable(con, table_name, data.frame(a = 1L), append = TRUE)
          stop(name)
        }
      ),
      name,
      fixed = TRUE
    )

    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0L))
  },

  with_transaction_break = function(con, table_name) {
    #' If the code calls `dbBreak()`, execution of the code stops and the
    #' transaction is silently aborted.
    dbWriteTable(con, table_name, data.frame(a = 0L), overwrite = TRUE)

    expect_error(
      dbWithTransaction(
        con,
        {
          dbWriteTable(con, table_name, data.frame(a = 1L), append = TRUE)
          dbBreak()
        }
      ),
      NA
    )

    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0L))
  },

  with_transaction_side_effects = function(con) {
    #' All side effects caused by the code
    expect_false(exists("a", inherits = FALSE))
    #' (such as the creation of new variables)
    dbWithTransaction(con, a <- 42)
    #' propagate to the calling environment.
    expect_identical(get0("a", inherits = FALSE), 42)
  },
  #
  NULL
)
