#' @template dbispec-sub
#' @format NULL
#' @section Transactions:
#' \subsection{`dbBegin("DBIConnection")` and `dbCommit("DBIConnection")`}{
spec_transaction_begin_commit <- list(
  #' Transactions are available in DBI, but actual support may vary between backends.
  begin_commit = function(ctx) {
    with_connection({
      #' A transaction is initiated by a call to [DBI::dbBegin()]
      dbBegin(con)
      on.exit(dbRollback(con), add = FALSE)
      #' and committed by a call to [DBI::dbCommit()].
      expect_error({dbCommit(con); on.exit(NULL, add = FALSE)}, NA)
    })
  },

  begin_commit_return_value = function(ctx) {
    with_connection({
      #' Both generics expect an object of class \code{\linkS4class{DBIConnection}}
      #' and return `TRUE` (invisibly) upon success.
      expect_invisible_true(dbBegin(con))
      on.exit(dbRollback(con), add = FALSE)
      expect_invisible_true(dbCommit(con))
      on.exit(NULL, add = FALSE)
    })
  },

  #'
  #' The implementations are expected to raise an error in case of failure,
  #' but this is difficult to test in an automated way.
  begin_commit_closed = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)

    #' In any way, both generics should throw an error with a closed connection.
    expect_error(dbBegin(con))
    expect_error(dbCommit(con))
  },

  commit_without_begin = function(ctx) {
    #' In addition, a call to [DBI::dbCommit()] without
    #' a call to [DBI::dbBegin()] should raise an error.
    with_connection({
      expect_error(dbCommit(con))
    })
  },

  begin_begin = function(ctx) {
    #' Nested transactions are not supported by DBI,
    with_connection({
      #' an attempt to call [DBI::dbBegin()] twice
      dbBegin(con)
      on.exit(dbRollback(con), add = FALSE)
      #' should yield an error.
      expect_error(dbBegin(con))
      dbCommit(con)
      on.exit(NULL, add = FALSE)
    })
  },

  #'
  #' Data written in a transaction must persist after the transaction is committed.
  begin_write_commit = function(ctx) {
    with_connection({
      #' For example, a table that is missing when the transaction is started
      expect_false(dbExistsTable(con, "test"))

      dbBegin(con)
      on.exit(dbRollback(con), add = FALSE)

      #' but is created
      dbExecute(con, paste0("CREATE TABLE test (a ", dbDataType(con, 0L), ")"))

      #' and populated during the transaction
      dbExecute(con, paste0("INSERT INTO test (a) VALUES (1)"))

      #' must exist and contain the data added there
      expect_equal(dbReadTable(con, "test"), data.frame(a = 1))

      #' both during
      dbCommit(con)
      on.exit(dbRemoveTable(con, "test"), add = FALSE)

      #' and after the transaction.
      expect_equal(dbReadTable(con, "test"), data.frame(a = 1))
    })
  },

  #'
  #' The behavior is not specified if other arguments are passed to these
  #' functions. In particular, \pkg{RSQLite} issues named transactions
  #' if the `name` argument is set.
  #'
  #' The transaction isolation level is not specified by DBI.
  #'
  #' }
  NULL
)
