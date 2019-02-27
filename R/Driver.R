#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' @name DBI
NULL

#' LoggingDBI driver
#'
#' TBD.
#'
#' @import methods DBI
#' @examples
#' \dontrun{
#' #' library(DBI)
#' RLoggingDBI::LoggingDBI()
#' }
LoggingDBI <- function(drv) {
  quo <- rlang::enquo(drv)
  log_drv <- log_call(!! quo)
  new("LoggingDBIDriver", drv = log_drv)
}

#' @rdname DBI
setClass("LoggingDBIDriver", contains = "DBIDriver", slots = list(drv = "DBIDriver"))

#' @rdname DBI
#' @inheritParams methods::show
setMethod(
  "show", "LoggingDBIDriver",
  function(object) {
    cat("<LoggingDBIDriver>\n")
    show(object@drv)
  })

#' @rdname DBI
#' @inheritParams DBI::dbConnect
setMethod(
  "dbConnect", "LoggingDBIDriver",
  function(drv, ...) {
    conn <- log_call(dbConnect(drv@drv, !!! rlang::enquos(...)))
    LoggingDBIConnection(conn)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
setMethod(
  "dbDataType", "LoggingDBIDriver",
  function(dbObj, obj, ...) {
    log_call(dbDataType(dbObj@drv, obj, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
setMethod(
  "dbIsValid", "LoggingDBIDriver",
  function(dbObj, ...) {
    log_call(dbIsValid(dbObj@drv, !!! rlang::enquos(...)))
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
setMethod(
  "dbGetInfo", "LoggingDBIDriver",
  function(dbObj, ...) {
    log_call(dbGetInfo(dbObj@drv, !!! rlang::enquos(...)))
  })
