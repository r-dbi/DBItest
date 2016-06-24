#' @name test_all
#' @section Tests:
#' \code{\link{test_connection}}:
#' Test the "Connection" class
NULL

#' Test the "Connection" class
#'
#' @inheritParams test_all
#' @include test-driver.R
#' @family tests
#' @importFrom withr with_temp_libpaths
#' @importFrom methods is
#' @export
test_connection <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{can_connect_and_disconnect}}{
    #' Can connect and disconnect, connection object inherits from
    #'   "DBIConnection".
    #' }
    can_connect_and_disconnect = function(ctx) {
      con <- connect(ctx)
      expect_s4_class(con, "DBIConnection")
      expect_true(dbDisconnect(con))
    },

    #' \item{\code{data_type_connection}}{
    #' SQL Data types exist for all basic R data types. dbDataType() does not
    #' throw an error and returns a nonempty atomic character
    #' }
    data_type_connection = function(ctx) {
      con <- connect(ctx)
      check_conn_data_type <- function(value) {
        eval(bquote({
          expect_is(dbDataType(con, .(value)), "character")
          expect_equal(length(dbDataType(con, .(value))), 1L)
          expect_match(dbDataType(con, .(value)), ".")
        }))
      }

      expect_conn_has_data_type <- function(value) {
        eval(bquote(
          expect_error(check_conn_data_type(.(value)), NA)))
      }

      expect_conn_has_data_type(logical(1))
      expect_conn_has_data_type(integer(1))
      expect_conn_has_data_type(numeric(1))
      expect_conn_has_data_type(character(1))
      expect_conn_has_data_type(Sys.Date())
      expect_conn_has_data_type(Sys.time())
      if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
        expect_conn_has_data_type(list(raw(1)))
      }
    },

    #' \item{\code{cannot_disconnect_twice}}{
    #' Repeated disconnect throws warning.
    #' }
    cannot_disconnect_twice = function(ctx) {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_warning(dbDisconnect(con))
    },

    #' \item{\code{simultaneous_connections}}{
    #' Open 50 simultaneous connections
    #' }
    simultaneous_connections = function(ctx) {
      cons <- list()
      on.exit(expect_error(lapply(cons, dbDisconnect), NA), add = TRUE)
      for (i in seq_len(50L)) {
        cons <- c(cons, connect(ctx))
      }

      inherit_from_connection <-
        vapply(cons, is, class2 = "DBIConnection", logical(1))
      expect_true(all(inherit_from_connection))
    },

    #' \item{\code{stress_connections}}{
    #' Open and close 50 connections
    #' }
    stress_connections = function(ctx) {
      for (i in seq_len(50L)) {
        con <- connect(ctx)
        expect_s4_class(con, "DBIConnection")
        expect_error(dbDisconnect(con), NA)
      }
    },

    #' \item{\code{get_info_connection}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info_connection = function(ctx) {
      con <- connect(ctx)
      on.exit(expect_error(dbDisconnect(con), NA), add = TRUE)

      info <- dbGetInfo(con)
      expect_is(info, "list")
      info_names <- names(info)

      necessary_names <-
        c("db.version", "dbname", "username", "host", "port")

      for (name in necessary_names) {
        eval(bquote(
          expect_true(.(name) %in% info_names)))
      }

      expect_false("password" %in% info_names)
    },

    #' \item{\code{stress_load_connect_unload}}{
    #' Repeated load, instantiation, connection, disconnection, and unload of
    #' package in a new R session.
    #' }
    stress_load_connect_unload = function(ctx) {
      skip_on_travis()
      skip_on_appveyor()
      skip_if_not(getRversion() != "3.3.0")

      pkg <- get_pkg(ctx)

      script_file <- tempfile("DBItest", fileext = ".R")
      local({
        sink(script_file)
        on.exit(sink(), add = TRUE)
        cat(
          "devtools::RCMD('INSTALL', ", shQuote(pkg$path), ")\n",
          "library(DBI, quietly = TRUE)\n",
          "connect_args <- ",
          sep = ""
        )
        dput(ctx$connect_args)
        cat(
          "for (i in 1:50) {\n",
          "  drv <- ", pkg$package, "::", deparse(ctx$drv_call), "\n",
          "  con <- do.call(dbConnect, c(drv, connect_args))\n",
          "  dbDisconnect(con)\n",
          "  unloadNamespace(getNamespace(\"", pkg$package, "\"))\n",
          "}\n",
          sep = ""
        )
      })

      with_temp_libpaths({
        expect_equal(system(paste0("R -q --vanilla -f ", shQuote(script_file)),
                            ignore.stdout = TRUE, ignore.stderr = TRUE),
                     0L)
      })
    },

    NULL
  )
  #'}
  run_tests(ctx, tests, skip, test_suite)
}
