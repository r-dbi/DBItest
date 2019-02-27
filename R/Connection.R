#' @include Driver.R
NULL

LoggingDBIConnection <- function(conn) {
  new("LoggingDBIConnection", conn = conn)
}

#' @rdname DBI
setClass(
  "LoggingDBIConnection",
  contains = "DBIConnection",
  slots = list(conn = "DBIConnection")
)

#' @rdname DBI
#' @inheritParams methods::show
setMethod(
  "show", "LoggingDBIConnection",
  function(object) {
    cat("<LoggingDBIConnection>\n")
    show(object@conn)
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
setMethod(
  "dbIsValid", "LoggingDBIConnection",
  function(dbObj, ...) {
    log_call(dbIsValid(dbObj@drv, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
setMethod(
  "dbDisconnect", "LoggingDBIConnection",
  function(conn, ...) {
    log_call(dbDisconnect(conn@conn, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
setMethod(
  "dbSendQuery", c("LoggingDBIConnection", "character"),
  function(conn, statement, ...) {
    res <- log_call(dbSendQuery(conn@conn, statement, !!! rlang::enquos(...)))
    LoggingDBIResult(res)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendStatement
setMethod(
  "dbSendStatement", c("LoggingDBIConnection", "character"),
  function(conn, statement, ...) {
    res <- log_call(dbSendStatement(conn@conn, statement, !!! rlang::enquos(...)))
    LoggingDBIResult(res)
  })

#' @rdname DBI
#' @inheritParams DBI::dbDataType
setMethod(
  "dbDataType", "LoggingDBIConnection",
  function(dbObj, obj, ...) {
    log_call(dbDataType(dbObj@conn, obj, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteString
setMethod(
  "dbQuoteString", c("LoggingDBIConnection", "character"),
  function(conn, x, ...) {
    log_call(dbQuoteString(conn@conn, x, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteIdentifier
setMethod(
  "dbQuoteIdentifier", c("LoggingDBIConnection", "character"),
  function(conn, x, ...) {
    log_call(dbQuoteIdentifier(conn@conn, x, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbWriteTable
#' @param overwrite Allow overwriting the destination table. Cannot be
#'   `TRUE` if `append` is also `TRUE`.
#' @param append Allow appending to the destination table. Cannot be
#'   `TRUE` if `overwrite` is also `TRUE`.
setMethod(
  "dbWriteTable", c("LoggingDBIConnection", "character", "data.frame"),
  function(conn, name, value, overwrite = FALSE, append = FALSE, ...) {
    log_call(dbWriteTable(conn@conn, name = name, value = value, overwrite = overwrite, append = append, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
setMethod(
  "dbReadTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    log_call(dbReadTable(conn@conn, name = name, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbListTables
setMethod(
  "dbListTables", "LoggingDBIConnection",
  function(conn, ...) {
    log_call(dbListTables(conn@conn, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
setMethod(
  "dbExistsTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    log_call(dbExistsTable(conn@conn, name, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbListFields
setMethod(
  "dbListFields", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    log_call(dbListFields(conn@conn, name, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
setMethod(
  "dbRemoveTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    log_call(dbRemoveTable(conn@conn, name, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
setMethod(
  "dbGetInfo", "LoggingDBIConnection",
  function(dbObj, ...) {
    log_call(dbGetInfo(dbObj@conn, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbBegin
setMethod(
  "dbBegin", "LoggingDBIConnection",
  function(conn, ...) {
    log_call(dbBegin(conn@conn, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbCommit
setMethod(
  "dbCommit", "LoggingDBIConnection",
  function(conn, ...) {
    log_call(dbCommit(conn@conn, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbRollback
setMethod(
  "dbRollback", "LoggingDBIConnection",
  function(conn, ...) {
    log_call(dbRollback(conn@conn, !!! rlang::enquos(...)))
  })
