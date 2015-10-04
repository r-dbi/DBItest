#' \code{test_result()} tests the "Result" class.
#'
#' @rdname test
#' @include test_connection.R
#' @export
test_result <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result"

  tests <- list(
    # Can issue trivial query, result object inherits from "DBIResult"
    trivial_query = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)
        expect_is(res, "DBIResult")
      })
    },

    # Can issue a command query that creates a table
    command_query = function() {
      with_connection({
        res <- dbSendQuery(con, "CREATE TABLE test (a integer)")
        dbClearResult(res)
        res2 <- dbSendQuery(con, "DROP TABLE test")
        dbClearResult(res2)
      })
    },

    # Issuing an invalid query throws error (but no warnings, e.g. related to
    #   pending results, are thrown)
    invalid_query = function() {
      expect_warning(
        with_connection({
          expect_error(dbSendQuery(con, "RAISE"))
        }),
        NA
      )
    },

    # Return value of dbGetInfo has necessary elements
    get_info = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)

        info <- dbGetInfo(res)
        expect_is(info, "list")
        info_names <- names(info)

        expect_true("statement" %in% info_names)
        expect_true("row.count" %in% info_names)
        expect_true("rows.affected" %in% info_names)
        expect_true("has.completed" %in% info_names)
        expect_true("is.select" %in% info_names)
      })
    },

    # show method for result class is defined
    show = function() {
      with_connection({
        res <- dbSendQuery(con, "SELECT 1")
        on.exit(dbClearResult(res), add = TRUE)

        expect_that(res, has_method("show"))
        expect_output(show(res), ".")
      })
    },

    # single-value queries can be fetched
    fetch_single = function() {
      with_connection({
        query <- "SELECT 1 as a"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        rows <- dbFetch(res)
        expect_identical(rows, data.frame(a=1L))

        expect_true(dbHasCompleted(res))
      })
    },

    # multi-row single-column queries can be fetched
    fetch_multi_row_single_column = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        rows <- dbFetch(res)
        expect_identical(rows, data.frame(a=1L:3L))

        expect_true(dbHasCompleted(res))
      })
    },

    # if more rows than available are fetched, the result is returned in full
    #   but no warning is issued
    fetch_more_rows = function() {
      with_connection({
        query <- "SELECT 1 as a UNION SELECT 2 UNION SELECT 3"

        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)

        expect_warning(rows <- dbFetch(res, 5L), NA)
        expect_identical(rows, data.frame(a=1L:3L))

        expect_true(dbHasCompleted(res))
      })
    },

    NULL
  )
  run_tests(tests, skip, test_suite)
}

# Expects a variable "ctx" in the environment env,
# evaluates the code inside local() after defining a variable "con"
# that points to a newly opened connection. Disconnects on exit.
with_connection <- function(code, env = parent.frame()) {
  code_sub <- substitute(code)

  eval(bquote({
    con <- connect(ctx)
    on.exit(dbDisconnect(con), add = TRUE)
    local(.(code_sub))
  }), env = env)
}
