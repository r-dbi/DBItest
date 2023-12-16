#' spec_result_execute
#' @family result specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_result_execute <- list(
  execute_formals = function() {
    # <establish formals of described functions>
    expect_equal(names(formals(dbExecute)), c("conn", "statement", "..."))
  },

  execute_atomic = function(ctx, con, table_name) {
    #' @return
    #' `dbExecute()` always returns a
    query <- trivial_statement(ctx, table_name)

    ret <- dbExecute(con, query)
    #' scalar
    expect_equal(length(ret), 1)
    #' numeric
    expect_true(is.numeric(ret))
    #' that specifies the number of rows affected
    #' by the statement.
  },

  #'
  execute_closed_connection = function(ctx, closed_con) {
    #' @section Failure modes:
    #' An error is raised when issuing a statement over a closed
    table_name <- "dbit12"
    expect_error(dbExecute(closed_con, trivial_statement(ctx, table_name = table_name)))
  },

  execute_invalid_connection = function(ctx, invalid_con) {
    #' or invalid connection,
    table_name <- "dbit13"
    expect_error(dbExecute(invalid_con, trivial_statement(ctx, table_name = table_name)))
  },

  execute_syntax_error = function(con) {
    #' if the syntax of the statement is invalid,
    expect_error(dbExecute(con, "CREATTE"))
  },

  execute_non_string = function(con) {
    #' or if the statement is not a non-`NA` string.
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

  execute_params = function(ctx, con) {
    #' @section Specification:
    #'
    #' The `param` argument allows passing query parameters, see [dbBind()] for details.
    placeholder_funs <- get_placeholder_funs(ctx)

    for (placeholder_fun in placeholder_funs) {
      table_name <- random_table_name()
      local_remove_test_table(con, table_name)
      dbWriteTable(con, table_name, data.frame(a = as.numeric(1:3)))
      placeholder <- placeholder_fun(1)
      query <- paste0("DELETE FROM ", table_name, " WHERE a > ", placeholder)
      values <- 1.5
      params <- stats::setNames(list(values), names(placeholder))
      rc <- dbExecute(con, query, params = params)
      if (isTRUE(ctx$tweaks$allow_na_rows_affected)) {
        expect_true((is.na(rc) && is.numeric(rc)) || rc == 2L, info = placeholder)
      } else {
        expect_equal(rc, 2L, info = placeholder)
      }
    }
  },

  execute_immediate = function(ctx, con, table_name) {
    #' @inheritSection spec_result_get_query Specification for the `immediate` argument
    res <- expect_visible(dbExecute(con, trivial_statement(ctx, table_name), immediate = TRUE))
    expect_true(is.numeric(res))
  },
  #
  NULL
)
