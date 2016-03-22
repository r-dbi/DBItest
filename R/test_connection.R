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
    can_connect_and_disconnect = function() {
      con <- connect(ctx)
      expect_s4_class(con, "DBIConnection")
      expect_true(dbDisconnect(con))
    },

    #' \item{\code{cannot_disconnect_twice}}{
    #' Repeated disconnect throws error.
    #' }
    cannot_disconnect_twice = function() {
      con <- connect(ctx)
      dbDisconnect(con)
      expect_warning(dbDisconnect(con))
    },

    #' \item{\code{simultaneous_connections}}{
    #' Open 50 simultaneous connections
    #' }
    simultaneous_connections = function() {
      cons <- lapply(seq_len(50L), function(i) connect(ctx))
      inherit_from_connection <-
        vapply(cons, is, class2 = "DBIConnection", logical(1))
      expect_true(all(inherit_from_connection))
      expect_error(lapply(cons, dbDisconnect), NA)
    },

    #' \item{\code{stress_connections}}{
    #' Open and close 50 connections
    #' }
    stress_connections = function() {
      for (i in seq_len(50L)) {
        con <- connect(ctx)
        expect_s4_class(con, "DBIConnection")
        expect_error(dbDisconnect(con), NA)
      }
    },

    #' \item{\code{get_info_connection}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info_connection = function() {
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
    stress_load_connect_unload = function() {
      skip_on_travis()

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
  run_tests(tests, skip, test_suite, ctx$name)
}
