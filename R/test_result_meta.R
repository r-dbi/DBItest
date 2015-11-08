#' @name test_all
#' @section Tests:
#' \code{\link{test_result_meta}}:
#' Test metadata functions for the "Result" class
NULL

#' Test metadata functions for the "Result" class
#'
#' @inheritParams test_all
#' @include test_connection_meta.R
#' @family tests
#' @export
test_result_meta <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Result (metadata)"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{is_valid_result}}{
    #' Only an open result set is valid.
    #' }
    is_valid_result = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        expect_true(dbIsValid(res))
        expect_error(dbFetch(res), NA)
        expect_true(dbIsValid(res))
        dbClearResult(res)
        expect_false(dbIsValid(res))
      })
    },

    #' \item{\code{get_statement}}{
    #' SQL query can be retrieved from the result.
    #' }
    get_statement = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        s <- dbGetStatement(res)
        expect_is(s, "character")
        expect_identical(s, query)
      })
    },

    #' \item{\code{column_info}}{
    #' Column information is correct.
    #' }
    column_info = function() {
      with_connection({
        query <- "SELECT 1 as a, 1.5 as b, NULL"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        ci <- dbColumnInfo(res)
        expect_is(ci, "data.frame")
        expect_equal(colnames(ci), c("name", "type"))
        expect_equal(ci$name[1:2], c("a", "b"))
        expect_is(ci$type, "character")
      })
    },

    #' \item{\code{row_count}}{
    #' Row count information is correct.
    #' }
    row_count = function() {
      with_connection({
        query <- "SELECT 1 as a"
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 1L)
        print(rc)
      })

      with_connection({
        query <- union("SELECT 1 as a", "SELECT 2", "SELECT 3")
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res, 2L)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 2L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 3L)
        print(rc)
      })

      with_connection({
        query <- union("SELECT * FROM (SELECT 1 as a) a WHERE (0 = 1)")
        res <- dbSendQuery(con, query)
        on.exit(dbClearResult(res), add = TRUE)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
        dbFetch(res)
        rc <- dbGetRowCount(res)
        expect_is(rc, "integer")
        expect_identical(rc, 0L)
      })
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
