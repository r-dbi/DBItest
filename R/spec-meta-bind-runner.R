run_bind_tester <- list()

#' spec_meta_bind
#' @name spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Specification:
#' \pkg{DBI} clients execute parametrized statements as follows:
#'
run_bind_tester$fun <- function() {
  if (extra_obj$requires_names() && is.null(names(placeholder))) {
    # wrong_name test only valid for named placeholders
    return()
  }

  #' 1. Call [DBI::dbSendQuery()] or [DBI::dbSendStatement()] with a query or statement
  #'    that contains placeholders,
  #'    store the returned [DBI::DBIResult-class] object in a variable.
  #'    Mixing placeholders (in particular, named and unnamed ones) is not
  #'    recommended.
  if (is_query())
    res <- send_query()
  else
    res <- send_statement()

  #'    It is good practice to register a call to [DBI::dbClearResult()] via
  #'    [on.exit()] right after calling `dbSendQuery()` or `dbSendStatement()`
  #'    (see the last enumeration item).
  on.exit(expect_error(dbClearResult(res), NA))

  #' 1. Construct a list with parameters
  #'    that specify actual values for the placeholders.
  bind_values <- values
  #'    The list must be named or unnamed,
  #'    depending on the kind of placeholders used.
  #'    Named values are matched to named parameters, unnamed values
  #'    are matched by position.
  if (!is.null(names(placeholder))) {
    names(bind_values) <- names(placeholder)
  }
  #'    All elements in this list must have the same lengths and contain values
  #'    supported by the backend; a [data.frame] is internally stored as such
  #'    a list.
  #'    The parameter list is passed to a call to `dbBind()` on the `DBIResult`
  #'    object.
  if (!bind(res, bind_values))
    return()

  #' 1. Retrieve the data or the number of affected rows from the `DBIResult` object.
  retrieve <- function() {
    #'     - For queries issued by `dbSendQuery()`,
    #'       call [DBI::dbFetch()].
    if (is_query()) {
      rows <- dbFetch(res)
      compare(rows, values)
    } else {
      #'     - For statements issued by `dbSendStatements()`,
      #'       call [DBI::dbGetRowsAffected()].
      #'       (Execution begins immediately after the `dbBind()` call,
      #'       the statement is processed entirely before the function returns.)
      rows_affected <- dbGetRowsAffected(res)
      compare_affected(rows_affected, values)
    }
  }
  retrieve()

  #' 1. Repeat 2. and 3. as necessary.
  if (extra_obj$is_repeated()) {
    bind(res, bind_values)
    retrieve()
  }

  #' 1. Close the result set via [DBI::dbClearResult()].
}
