#' spec_result_execute
#' @usage NULL
#' @format NULL
#' @keywords internal
spec_result_execute <- list(
  execute_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExecute)), c("conn", "statement", "..."))
  },

  #' @return
  #' `dbExecute()` always returns a
  execute_atomic = function(ctx, con, table_name) {
    query <- trivial_statement(ctx, table_name)

    ret <- dbExecute(con, query)
    #' scalar
    expect_equal(length(ret), 1)
    #' numeric
    expect_true(is.numeric(ret))
    #' that specifies the number of rows affected
    #' by the statement.
  },

  #' An error is raised when issuing a statement over a closed
  execute_closed_connection = function(ctx, closed_con) {
    table_name <- "dbit12"
    expect_error(dbExecute(closed_con, trivial_statement(ctx, table_name = table_name)))
  },

  #' or invalid connection,
  execute_invalid_connection = function(ctx, invalid_con) {
    table_name <- "dbit13"
    expect_error(dbExecute(invalid_con, trivial_statement(ctx, table_name = table_name)))
  },

  #' if the syntax of the statement is invalid,
  execute_syntax_error = function(con) {
    expect_error(dbExecute(con, "CREATTE"))
  },

  #' or if the statement is not a non-`NA` string.
  execute_non_string = function(con) {
    expect_error(dbExecute(con, character()))
    expect_error(dbExecute(con, letters))
    expect_error(dbExecute(con, NA_character_))
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
  execute_params = function(ctx, con) {
    placeholder_funs <- get_placeholder_funs(ctx)

    table_name <- random_table_name()
    for (placeholder_fun in placeholder_funs) {
      with_remove_test_table(name = table_name, {
        dbWriteTable(con, table_name, data.frame(a = as.numeric(1:3)))
        placeholder <- placeholder_fun(1)
        query <- paste0("DELETE FROM ", table_name, " WHERE a > ", placeholder)
        values <- 1.5
        params <- stats::setNames(list(values), names(placeholder))
        ret <- dbExecute(con, query, params = params)
        expect_equal(ret, 2, info = placeholder)
      })
    }
  },

  #' @inheritSection spec_result_get_query Specification for the `immediate` argument
  execute_immediate = function(ctx, con, table_name) {
    res <- expect_visible(dbExecute(con, trivial_statement(ctx, table_name), immediate = TRUE))
    expect_true(is.numeric(res))
  },
  #
  NULL
)
