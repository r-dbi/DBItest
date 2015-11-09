#' @name test_all
#' @section Tests:
#' \code{\link{test_connection}}:
#' Test the "Connection" class
NULL

#' Test the "Connection" class
#'
#' @inheritParams test_all
#' @include test_driver.R
#' @family tests
#' @export
test_connection <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{can_connect_and_disconnect}}{
    #' Can connect and disconnect, connection object inherits from
    #'   "DBIConnection"
    #' }
    can_connect_and_disconnect = function() {
      con <- connect(ctx)
      expect_is(con, "DBIConnection")
      expect_error(dbDisconnect(con), NA)
      expect_error(dbDisconnect(con))
      expect_error(dbGetQuery(con, "select 1"))
    },

    #' \item{\code{simultaneous_connections}}{
    #' Open 50 simultaneous connections
    #' }
    simultaneous_connections = function() {
      cons <- lapply(seq_len(50L), function(i) connect(ctx))
      inherit_from_connection <-
        vapply(cons, inherits, what = "DBIConnection", logical(1))
      expect_true(all(inherit_from_connection))
      expect_error(lapply(cons, dbDisconnect), NA)
    },

    #' \item{\code{stress_connections}}{
    #' Open and close 50 connections
    #' }
    stress_connections = function() {
      for (i in seq_len(50L)) {
        con <- connect(ctx)
        expect_is(con, "DBIConnection")
        expect_error(dbDisconnect(con), NA)
      }
    },

    #' \item{\code{get_info}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

      info <- dbGetInfo(con)
      expect_is(info, "list")
      info_names <- names(info)

      expect_true("db.version" %in% info_names)
      expect_true("dbname" %in% info_names)
      expect_true("username" %in% info_names)
      expect_true("host" %in% info_names)
      expect_true("port" %in% info_names)
      expect_false("password" %in% info_names)
    },

    #' \item{\code{show}}{
    #' show method for connection class is defined
    #' }
    show = function() {
      con <- connect(ctx)
      on.exit(dbDisconnect(con), add = TRUE)

      expect_that(con, has_method("show"))
      expect_output(show(con), ".")
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
