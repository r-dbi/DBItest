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
  expr <- rlang::enexpr(drv)
  expr_list <- as.list(expr)

  rlang::eval_tidy(rlang::quo(print_call(expr_list[[1]], !!!expr_list[-1], result = drv)))
  new("LoggingDBIDriver", drv = drv)
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
    # TODO: Print more details
  })

#' @rdname DBI
#' @inheritParams DBI::dbConnect
setMethod(
  "dbConnect", "LoggingDBIDriver",
  function(drv, ...) {
    print_call(
      "dbConnect", drv@drv, ...,
      result = conn <- dbConnect(drv@drv, ...)
    )
    LoggingDBIConnection(conn)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
setMethod(
  "dbDataType", "LoggingDBIDriver",
  function(dbObj, obj, ...) {
    # Optional: Can remove this if all data types conform to SQL-92
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Driver)"))
  })

#' @rdname DBI
#' @inheritParams DBI::dbDataType
setMethod(
  "dbDataType", c("LoggingDBIDriver", "list"),
  function(dbObj, obj, ...) {
    # rstats-db/DBI#70
    testthat::skip("Not yet implemented: dbDataType(Driver, list)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
setMethod(
  "dbIsValid", "LoggingDBIDriver",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Driver)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
setMethod(
  "dbGetInfo", "LoggingDBIDriver",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbGetInfo(Driver)")
  })
