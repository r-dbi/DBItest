#' @include Connection.R
NULL

LoggingDBIResult <- function(connection, statement) {
  # TODO: Initialize result
  new("LoggingDBIResult", connection = connection, statement = statement)
}

#' @rdname DBI
#' @export
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
#' @export
setMethod(
  "show", "LoggingDBIResult",
  function(object) {
    cat("<LoggingDBIResult>\n")
    # TODO: Print more details
  })

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
#' @export
setMethod(
  "dbClearResult", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbClearResult(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbFetch
#' @export
setMethod(
  "dbFetch", "LoggingDBIResult",
  function(res, n = -1, ...) {
    testthat::skip("Not yet implemented: dbFetch(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
#' @export
setMethod(
  "dbHasCompleted", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbHasCompleted(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "LoggingDBIResult",
  function(dbObj, ...) {
    # Optional
    getMethod("dbGetInfo", "DBIResult", asNamespace("DBI"))(dbObj, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "LoggingDBIResult",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod(
  "dbGetStatement", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetStatement(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbColumnInfo
#' @export
setMethod(
  "dbColumnInfo", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbColumnInfo(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowCount
#' @export
setMethod(
  "dbGetRowCount", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetRowCount(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowsAffected
#' @export
setMethod(
  "dbGetRowsAffected", "LoggingDBIResult",
  function(res, ...) {
    testthat::skip("Not yet implemented: dbGetRowsAffected(Result)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbBind
#' @export
setMethod(
  "dbBind", "LoggingDBIResult",
  function(res, params, ...) {
    testthat::skip("Not yet implemented: dbBind(Result)")
  })
