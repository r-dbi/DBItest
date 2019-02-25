#' @include Connection.R
NULL

LoggingDBIResult <- function(connection, statement) {
  # TODO: Initialize result
  new("LoggingDBIResult", connection = connection, statement = statement)
}

#' @rdname DBI
setClass(
  "LoggingDBIResult",
  contains = "DBIResult",
  slots = list(
    connection = "LoggingDBIConnection",
    statement = "character"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
setMethod(
  "show", "LoggingDBIResult",
  function(object) {
    cat("<LoggingDBIResult>\n")
    # TODO: Print more details
  })

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
setMethod(
  "dbClearResult", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbClearResult(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbFetch
setMethod(
  "dbFetch", "LoggingDBIResult",
  function(res, n = -1, ...) {
    testthat::skip("Not yet implemented: dbFetch(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
setMethod(
  "dbHasCompleted", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbHasCompleted(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
setMethod(
  "dbGetInfo", "LoggingDBIResult",
  function(dbObj, ...) {
    # Optional
    getMethod("dbGetInfo", "DBIResult", asNamespace("DBI"))(dbObj, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
setMethod(
  "dbIsValid", "LoggingDBIResult",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
setMethod(
  "dbGetStatement", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetStatement(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbColumnInfo
setMethod(
  "dbColumnInfo", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbColumnInfo(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowCount
setMethod(
  "dbGetRowCount", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetRowCount(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowsAffected
setMethod(
  "dbGetRowsAffected", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetRowsAffected(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbBind
setMethod(
  "dbBind", "LoggingDBIResult",
  function(res, params, ...) {
    testthat::skip("Not yet implemented: dbBind(Result)")
  })
