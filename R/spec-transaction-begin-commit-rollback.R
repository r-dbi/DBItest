#' spec_transaction_begin_commit_rollback
#' @family transaction specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_transaction_begin_commit_rollback <- list(
  begin_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbBegin)), c("conn", "..."))
  },
  #
  commit_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbCommit)), c("conn", "..."))
  },
  #
  rollback_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbRollback)), c("conn", "..."))
  },

  begin_commit_return_value = function(con) {
    #' @return
    #' `dbBegin()`, `dbCommit()` and `dbRollback()` return `TRUE`, invisibly.
    expect_invisible_true(dbBegin(con))
    on.exit({
      dbRollback(con)
    })
    expect_invisible_true(dbCommit(con))
    on.exit(NULL)
  },
  #
  begin_rollback_return_value = function(con) {
    expect_invisible_true(dbBegin(con))
    expect_invisible_true(dbRollback(con))
  },

  #'
  begin_commit_closed = function(ctx, closed_con) {
    #' @section Failure modes:
    #' The implementations are expected to raise an error in case of failure,
    #' but this is not tested.
    #' In any way, all generics throw an error with a closed
    expect_error(dbBegin(closed_con))
    expect_error(dbCommit(closed_con))
    expect_error(dbRollback(closed_con))
  },
  #
  begin_commit_invalid = function(ctx, invalid_con) {
    #' or invalid connection.
    expect_error(dbBegin(invalid_con))
    expect_error(dbCommit(invalid_con))
    expect_error(dbRollback(invalid_con))
  },
  #
  commit_without_begin = function(con) {
    #' In addition, a call to `dbCommit()`
    expect_error(dbCommit(con))
  },
  #
  rollback_without_begin = function(con) {
    #' or `dbRollback()`
    #' without a prior call to `dbBegin()` raises an error.
    expect_error(dbRollback(con))
  },
  #
  begin_begin = function(con) {
    #' Nested transactions are not supported by DBI,
    #' an attempt to call `dbBegin()` twice
    dbBegin(con)
    on.exit({
      dbRollback(con)
    })
    #' yields an error.
    expect_error(dbBegin(con))
    dbCommit(con)
    on.exit(NULL)
  },

  begin_commit = function(con) {
    #' @section Specification:
    #' Actual support for transactions may vary between backends.
    #' A transaction is initiated by a call to `dbBegin()`
    dbBegin(con)
    #' and committed by a call to `dbCommit()`.
    success <- FALSE
    expect_error(
      {
        dbCommit(con)
        success <- TRUE
      },
      NA
    )
    if (!success) dbRollback(con)
  },

  begin_write_commit_1 = function(con) {
    #' Data written in a transaction must persist after the transaction is committed.
    #' For example, a record that is missing when the transaction is started

    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit00"
    dbWriteTable(con, table_name, data.frame(a = 0L), overwrite = TRUE)

    dbBegin(con)
    on.exit({
      dbRollback(con)
    })

    #' but is created during the transaction
    dbExecute(con, paste0("INSERT INTO ", table_name, " (a) VALUES (1)"))

    #' must exist
    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0:1))

    #' both during
    dbCommit(con)
    on.exit(NULL)

    #' and after the transaction,
    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0:1))
  },
  # second stage
  begin_write_commit_2 = function(con, table_name = "dbit00") {
    #' and also in a new connection.
    expect_true(dbExistsTable(con, table_name))
    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0:1))
  },
  #
  #'
  begin_rollback = function(con) {
    #' A transaction
    dbBegin(con)
    #' can also be aborted with `dbRollback()`.
    expect_error(dbRollback(con), NA)
  },

  begin_write_rollback = function(con, table_name) {
    #' All data written in such a transaction must be removed after the
    #' transaction is rolled back.
    #' For example, a record that is missing when the transaction is started
    dbWriteTable(con, table_name, data.frame(a = 0L), overwrite = TRUE)

    dbBegin(con)

    #' but is created during the transaction
    dbWriteTable(con, table_name, data.frame(a = 1L), append = TRUE)

    #' must not exist anymore after the rollback.
    dbRollback(con)
    expect_equal(check_df(dbReadTable(con, table_name)), data.frame(a = 0L))
  },
  #
  begin_write_disconnect_1 = function(local_con) {
    # table_name not in formals on purpose: this means that this table won't be
    # removed at the end of the test
    table_name <- "dbit01"
    #'
    #' Disconnection from a connection with an open transaction
    dbWriteTable(local_con, table_name, data.frame(a = 0L), overwrite = TRUE)

    dbBegin(local_con)

    dbWriteTable(local_con, table_name, data.frame(a = 1L), append = TRUE)
  },
  #
  begin_write_disconnect_2 = function(local_con, table_name = "dbit01") {
    #' effectively rolls back the transaction.
    #' All data written in such a transaction must be removed after the
    #' transaction is rolled back.
    expect_equal(check_df(dbReadTable(local_con, table_name)), data.frame(a = 0L))
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
