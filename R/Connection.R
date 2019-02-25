#' @include Driver.R
NULL

LoggingDBIConnection <- function(conn) {
  # TODO: Add arguments
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
    # TODO: Print more details
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
setMethod(
  "dbIsValid", "LoggingDBIConnection",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
setMethod(
  "dbDisconnect", "LoggingDBIConnection",
  function(conn, ...) {
    print_call("dbDisconnect", conn@conn, ...)

    if (!dbIsValid(conn@conn)) {
      warning("Connection already closed.", call. = FALSE)
    }

    # TODO: Free resources
    TRUE
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
setMethod(
  "dbSendQuery", c("LoggingDBIConnection", "character"),
  function(conn, statement, ...) {
    LoggingDBIResult(connection = conn, statement = statement)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendStatement
setMethod(
  "dbSendStatement", c("LoggingDBIConnection", "character"),
  function(conn, statement, ...) {
    LoggingDBIResult(connection = conn, statement = statement)
  })

#' @rdname DBI
#' @inheritParams DBI::dbDataType
setMethod(
  "dbDataType", "LoggingDBIConnection",
  function(dbObj, obj, ...) {
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Connection)"))
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteString
setMethod(
  "dbQuoteString", c("LoggingDBIConnection", "character"),
  function(conn, x, ...) {
    # Optional
    getMethod("dbQuoteString", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteIdentifier
setMethod(
  "dbQuoteIdentifier", c("LoggingDBIConnection", "character"),
  function(conn, x, ...) {
    # Optional
    getMethod("dbQuoteIdentifier", c("DBIConnection", "character"), asNamespace("DBI"))(conn, x, ...)
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
    testthat::skip("Not yet implemented: dbWriteTable(Connection, character, data.frame)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
setMethod(
  "dbReadTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    testthat::skip("Not yet implemented: dbReadTable(Connection, character)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbListTables
setMethod(
  "dbListTables", "LoggingDBIConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbListTables(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
setMethod(
  "dbExistsTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    testthat::skip("Not yet implemented: dbExistsTable(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbListFields
setMethod(
  "dbListFields", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    testthat::skip("Not yet implemented: dbListFields(Connection, character)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
setMethod(
  "dbRemoveTable", c("LoggingDBIConnection", "character"),
  function(conn, name, ...) {
    testthat::skip("Not yet implemented: dbRemoveTable(Connection, character)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
setMethod(
  "dbGetInfo", "LoggingDBIConnection",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbGetInfo(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbBegin
setMethod(
  "dbBegin", "LoggingDBIConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbBegin(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbCommit
setMethod(
  "dbCommit", "LoggingDBIConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbCommit(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbRollback
setMethod(
  "dbRollback", "LoggingDBIConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbRollback(Connection)")
  })
