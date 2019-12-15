#' spec_result_execute
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_result_execute <- list(
  execute_formals = function(ctx) {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExecute)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbExecute()` always returns a
  execute_atomic = function(ctx) {
    with_connection({
      with_remove_test_table({
        query <- trivial_statement()

        ret <- dbExecute(con, query)
        #' scalar
        expect_equal(length(ret), 1)
        #' numeric
        expect_true(is.numeric(ret))
        #' that specifies the number of rows affected
        #' by the statement.
      })
    })
  },

  #' An error is raised when issuing a statement over a closed
  execute_closed_connection = function(ctx) {
    with_closed_connection({
      expect_error(dbExecute(con, trivial_statement()))
    })
  },

  #' or invalid connection,
  execute_invalid_connection = function(ctx) {
    with_invalid_connection({
      expect_error(dbExecute(con, trivial_statement()))
    })
  },

  #' if the syntax of the statement is invalid,
  execute_syntax_error = function(ctx) {
    with_connection({
      expect_error(dbExecute(con, "CREATTE"))
    })
  },

  #' or if the statement is not a non-`NA` string.
  execute_non_string = function(ctx) {
    with_connection({
      expect_error(dbExecute(con, character()))
      expect_error(dbExecute(con, letters))
      expect_error(dbExecute(con, NA_character_))
    })
  },

  #' @section Additional arguments:
  #' The following arguments are not part of the `dbExecute()` generic
  #' (to improve compatibility across backends)
  #' but are part of the DBI specification:
  #' - `params` (default: `NULL`)
  #' - `immediate` (default: `NULL`)
  #'
  #' They must be provided as named arguments.
  #' See the "Specification" sections for details on their usage.

  #' @section Specification:
  #'
  #' The `param` argument allows passing query parameters, see [dbBind()] for details.
  execute_params = function(ctx) {
    placeholder_funs <- get_placeholder_funs(ctx)

    with_connection({
      for (placeholder_fun in placeholder_funs) {
        with_remove_test_table(name = "test", {
          dbWriteTable(con, "test", data.frame(a = as.numeric(1:3)))
          placeholder <- placeholder_fun(1)
          query <- paste0("DELETE FROM test WHERE a > ", placeholder)
          values <- 1.5
          params <- stats::setNames(list(values), names(placeholder))
          ret <- dbExecute(con, query, params = params)
          expect_equal(ret, 2, info = placeholder)
        })
      }
    })
  },

  #' @inheritSection spec_result_get_query Specification for the `immediate` argument
  execute_immediate = function(ctx) {
    with_connection({
      with_remove_test_table({
        res <- expect_visible(dbExecute(con, trivial_statement(), immediate = TRUE))
        expect_true(is.numeric(res))
      })
    })
  },

  NULL
)
