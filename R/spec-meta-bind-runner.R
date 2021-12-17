run_bind_tester <- list()

#' spec_meta_bind
#' @family meta specifications
#' @name spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Specification:
#' \pkg{DBI} clients execute parametrized statements as follows:
#'
run_bind_tester$fun <- function() {
  if ((extra_obj$requires_names() %in% TRUE) && is.null(names(placeholder_fun(1)))) {
    # test only valid for named placeholders
    return()
  }

  if ((extra_obj$requires_names() %in% FALSE) && !is.null(names(placeholder_fun(1)))) {
    # test only valid for unnamed placeholders
    return()
  }

  #' 1. Call [dbSendQuery()] or [dbSendStatement()] with a query or statement
  #'    that contains placeholders,
  #'    store the returned [DBIResult-class] object in a variable.
  #'    Mixing placeholders (in particular, named and unnamed ones) is not
  #'    recommended.
  if (is_query()) {
    res <- send_query()
  } else {
    res <- send_statement()
  }

  #'    It is good practice to register a call to [dbClearResult()] via
  #'    [on.exit()] right after calling `dbSendQuery()` or `dbSendStatement()`
  #'    (see the last enumeration item).
  if (extra_obj$is_premature_clear()) {
    dbClearResult(res)
  } else {
    on.exit(expect_error(dbClearResult(res), NA))
  }

  #'    Until `dbBind()` has been called, the returned result set object has the
  #'    following behavior:
  #'     - [dbFetch()] raises an error (for `dbSendQuery()`)
  if (is_query()) expect_error(dbFetch(res))
  #'     - [dbGetRowCount()] returns zero (for `dbSendQuery()`)
  if (is_query()) expect_equal(dbGetRowCount(res), 0)
  #'     - [dbGetRowsAffected()] returns an integer `NA` (for `dbSendStatement()`)
  if (!is_query()) expect_identical(dbGetRowsAffected(res), NA_integer_)
  #'     - [dbIsValid()] returns `TRUE`
  expect_true(dbIsValid(res))
  #'     - [dbHasCompleted()] returns `FALSE`
  expect_false(dbHasCompleted(res))

  #' 1. Construct a list with parameters
  #'    that specify actual values for the placeholders.
  bind_values <- values
  #'    The list must be named or unnamed,
  #'    depending on the kind of placeholders used.
  #'    Named values are matched to named parameters, unnamed values
  #'    are matched by position in the list of parameters.

  if (!is.null(names(placeholder_fun(1)))) {
    names(bind_values) <- names(placeholder_fun(length(bind_values)))
  }
  #'    All elements in this list must have the same lengths and contain values
  #'    supported by the backend; a [data.frame] is internally stored as such
  #'    a list.
  #'    The parameter list is passed to a call to `dbBind()` on the `DBIResult`
  #'    object.
  bind(res, bind_values)
  if (!is.na(extra_obj$bind_error())) {
    return()
  }

  # Safety net: returning early if dbBind() should have thrown an error but
  # didn't
  if (!identical(bind_values, extra_obj$patch_bind_values(bind_values))) {
    return()
  }
  if (extra_obj$is_premature_clear()) {
    return()
  }

  #' 1. Retrieve the data or the number of affected rows from the `DBIResult` object.
  retrieve <- function() {
    #'     - For queries issued by `dbSendQuery()`,
    #'       call [dbFetch()].
    if (is_query()) {
      rows <- check_df(dbFetch(res))
      compare(rows)
    } else {
      #'     - For statements issued by `dbSendStatements()`,
      #'       call [dbGetRowsAffected()].
      #'       (Execution begins immediately after the `dbBind()` call,
      #'       the statement is processed entirely before the function returns.)
      rows_affected <- dbGetRowsAffected(res)
      compare_affected(rows_affected, values)
    }
  }

  if (!extra_obj$is_untouched()) retrieve()

  #' 1. Repeat 2. and 3. as necessary.
  if (extra_obj$is_repeated()) {
    bind(res, bind_values)
    retrieve()
  }

  #' 1. Close the result set via [dbClearResult()].
}
