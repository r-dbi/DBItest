#' spec_meta_is_valid
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_meta_is_valid <- list(
  is_valid_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbIsValid)), c("dbObj", "..."))
  },

  is_valid_connection = function(ctx) {
    #' @return
    #' `dbIsValid()` returns a logical scalar,
    #' `TRUE` if the object specified by `dbObj` is valid,
    #' `FALSE` otherwise.
    con <- connect(ctx)
    #' A [DBIConnection-class] object is initially valid,
    expect_true(expect_visible(dbIsValid(con)))
    expect_error(dbDisconnect(con), NA)
    #' and becomes invalid after disconnecting with [dbDisconnect()].
    expect_false(expect_visible(dbIsValid(con)))
  },
  #
  is_valid_stale_connection = function(ctx, invalid_con) {
    #' For an invalid connection object (e.g., for some drivers if the object
    #' is saved to a file and then restored), the method also returns `FALSE`.
    expect_false(expect_visible(dbIsValid(invalid_con)))
  },
  #
  is_valid_result_query = function(con) {
    query <- trivial_query()
    res <- dbSendQuery(con, query)
    on.exit(dbClearResult(res))
    #' A [DBIResult-class] object is valid after a call to [dbSendQuery()],
    expect_true(expect_visible(dbIsValid(res)))
    expect_error(dbFetch(res), NA)
    #' and stays valid even after all rows have been fetched;
    expect_true(expect_visible(dbIsValid(res)))
    dbClearResult(res)
    on.exit(NULL)
    #' only clearing it with [dbClearResult()] invalidates it.
    expect_false(dbIsValid(res))
  },
  #
  is_valid_result_statement = function(con, table_name) {
    query <- paste0("CREATE TABLE ", table_name, " (a ", dbDataType(con, 1L), ")")
    res <- dbSendStatement(con, query)
    on.exit(dbClearResult(res))
    #' A [DBIResult-class] object is also valid after a call to [dbSendStatement()],
    expect_true(expect_visible(dbIsValid(res)))
    #' and stays valid after querying the number of rows affected;
    expect_error(dbGetRowsAffected(res), NA)
    expect_true(expect_visible(dbIsValid(res)))
    dbClearResult(res)
    on.exit(NULL)
    #' only clearing it with [dbClearResult()] invalidates it.
    expect_false(dbIsValid(res))
  },

  #' If the connection to the database system is dropped (e.g., due to
  #' connectivity problems, server failure, etc.), `dbIsValid()` should return
  #' `FALSE`. This is not tested automatically.

  NULL
)
