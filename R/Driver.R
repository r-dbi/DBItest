#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' @name DBI
NULL

#' Kazam driver
#'
#' TBD.
#'
#' @export
#' @import methods DBI
#' @examples
#' \dontrun{
#' #' library(DBI)
#' RKazam::Kazam()
#' }
Kazam <- function(drv) {
  expr <- rlang::enexpr(drv)
  expr_list <- as.list(expr)

  rlang::eval_tidy(rlang::quo(print_call(expr_list[[1]], !!!expr_list[-1], result = drv)))
  new("KazamDriver", drv = drv)
}

#' @rdname DBI
#' @export
setClass("KazamDriver", contains = "DBIDriver", slots = list(drv = "DBIDriver"))

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "KazamDriver",
  function(object) {
    cat("<KazamDriver>\n")
    show(object@drv)
    # TODO: Print more details
  })

#' @rdname DBI
#' @inheritParams DBI::dbConnect
#' @export
setMethod(
  "dbConnect", "KazamDriver",
  function(drv, ...) {
    print_call(
      "dbConnect", drv@drv, ...,
      result = conn <- dbConnect(drv@drv, ...)
    )
    KazamConnection(conn)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "KazamDriver",
  function(dbObj, obj, ...) {
    # Optional: Can remove this if all data types conform to SQL-92
    tryCatch(
      getMethod("dbDataType", "DBIObject", asNamespace("DBI"))(dbObj, obj, ...),
      error = function(e) testthat::skip("Not yet implemented: dbDataType(Driver)"))
  })

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", c("KazamDriver", "list"),
  function(dbObj, obj, ...) {
    # rstats-db/DBI#70
    testthat::skip("Not yet implemented: dbDataType(Driver, list)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "KazamDriver",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbIsValid(Driver)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "KazamDriver",
  function(dbObj, ...) {
    testthat::skip("Not yet implemented: dbGetInfo(Driver)")
  })
