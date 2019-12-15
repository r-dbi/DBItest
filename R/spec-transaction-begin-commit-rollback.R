#' spec_transaction_begin_commit_rollback
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_transaction_begin_commit_rollback <- list(
  begin_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBegin)), c("conn", "..."))
  },

  commit_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbCommit)), c("conn", "..."))
  },

  rollback_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbRollback)), c("conn", "..."))
  },

  #' @return
  #' `dbBegin()`, `dbCommit()` and `dbRollback()` return `TRUE`, invisibly.
  begin_commit_return_value = function(ctx) {
    with_connection({
      expect_invisible_true(dbBegin(con))
      with_rollback_on_error({
        expect_invisible_true(dbCommit(con))
      })
    })
  },

  begin_rollback_return_value = function(ctx) {
    with_connection({
      expect_invisible_true(dbBegin(con))
      expect_invisible_true(dbRollback(con))
    })
  },

  #' The implementations are expected to raise an error in case of failure,
  #' but this is not tested.
  begin_commit_closed = function(ctx) {
    with_closed_connection({
      #' In any way, all generics throw an error with a closed
      expect_error(dbBegin(con))
      expect_error(dbCommit(con))
      expect_error(dbRollback(con))
    })
  },

  begin_commit_invalid = function(ctx) {
    with_invalid_connection({
      #' or invalid connection.
      expect_error(dbBegin(con))
      expect_error(dbCommit(con))
      expect_error(dbRollback(con))
    })
  },

  commit_without_begin = function(ctx) {
    #' In addition, a call to `dbCommit()`
    with_connection({
      expect_error(dbCommit(con))
    })
  },

  rollback_without_begin = function(ctx) {
    #' or `dbRollback()`
    with_connection({
      #' without a prior call to `dbBegin()` raises an error.
      expect_error(dbRollback(con))
    })
  },

  begin_begin = function(ctx) {
    #' Nested transactions are not supported by DBI,
    with_connection({
      #' an attempt to call `dbBegin()` twice
      dbBegin(con)
      with_rollback_on_error({
        #' yields an error.
        expect_error(dbBegin(con))
        dbCommit(con)
      })
    })
  },

  #' @section Specification:
  #' Actual support for transactions may vary between backends.
  begin_commit = function(ctx) {
    with_connection({
      #' A transaction is initiated by a call to `dbBegin()`
      dbBegin(con)
      #' and committed by a call to `dbCommit()`.
      success <- FALSE
      expect_error({dbCommit(con); success <- TRUE}, NA)
      if (!success) dbRollback(con)
    })
  },

  #' Data written in a transaction must persist after the transaction is committed.
  begin_write_commit = function(ctx) {
    with_connection({
      #' For example, a record that is missing when the transaction is started

      dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

      dbBegin(con)
      with_rollback_on_error({
        #' but is created during the transaction
        dbExecute(con, paste0("INSERT INTO test (a) VALUES (1)"))

        #' must exist
        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0:1))

        #' both during
        dbCommit(con)
      })

      #' and after the transaction,
      expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0:1))
    })

    with_connection({
      with_remove_test_table({
        #' and also in a new connection.
        expect_true(dbExistsTable(con, "test"))
        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0:1))
      })
    })
  },

  begin_rollback = function(ctx) {
    with_connection({
      #'
      #' A transaction
      dbBegin(con)
      #' can also be aborted with `dbRollback()`.
      expect_error(dbRollback(con), NA)
    })
  },

  #' All data written in such a transaction must be removed after the
  #' transaction is rolled back.
  begin_write_rollback = function(ctx) {
    with_connection({
      #' For example, a record that is missing when the transaction is started
      with_remove_test_table({
        dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

        dbBegin(con)

        #' but is created during the transaction
        dbWriteTable(con, "test", data.frame(a = 1L), append = TRUE)

        #' must not exist anymore after the rollback.
        dbRollback(con)
        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0L))
      })
    })
  },

  begin_write_disconnect = function(ctx) {
    #'
    #' Disconnection from a connection with an open transaction
    with_connection({
      dbWriteTable(con, "test", data.frame(a = 0L), overwrite = TRUE)

      dbBegin(con)

      dbWriteTable(con, "test", data.frame(a = 1L), append = TRUE)
    })

    with_connection({
      #' effectively rolls back the transaction.
      #' All data written in such a transaction must be removed after the
      #' transaction is rolled back.
      with_remove_test_table({
        expect_equal(check_df(dbReadTable(con, "test")), data.frame(a = 0L))
      })
    })
  },

  #'
  #' The behavior is not specified if other arguments are passed to these
  #' functions. In particular, \pkg{RSQLite} issues named transactions
  #' with support for nesting
  #' if the `name` argument is set.
  #'
  #' The transaction isolation level is not specified by DBI.
  NULL
)
