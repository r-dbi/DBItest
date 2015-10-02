#' \code{test_connection()} tests the "Connection" class.
#'
#' @rdname test
#' @include test_driver.R
#' @export
test_connection <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Connection"

  tests <- list(
    # Can connect and disconnect, connection object inherits from
    #   "DBIConnection"
    can_connect_and_disconnect = function() {
      con <- connect(ctx)
      expect_is(con, "DBIConnection")
      expect_success(dbDisconnect(con))
    },

    # Open 100 simultaneous connections
    simultaneous_connections = function() {
      cons <- lapply(seq_len(100L), function(i) connect(ctx))
      inherit_from_connection <-
        vapply(cons, inherits, what = "DBIConnection", logical(1))
      expect_true(all(inherit_from_connection))
      expect_success(lapply(cons, dbDisconnect))
    },

    # Open and close 100 connections
    stress_connections = function() {
      for (i in seq_len(100L)) {
        con <- connect(ctx)
        expect_is(con, "DBIConnection")
        expect_success(dbDisconnect(con))
      }
    },

    # Return value of dbGetInfo has necessary elements
    get_info = function() {
      con <- connect(ctx)
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

    # show method for connection class is defined
    show = function() {
      con <- connect(ctx)
      expect_that(con, has_method("show"))
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}
