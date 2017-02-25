run_bind_tester <- list()

#' @template dbispec-sub
#' @format NULL
#' @section Parametrized queries and statements:
#' \pkg{DBI} supports parametrized (or prepared) queries and statements
#' via the [DBI::dbBind()] generic.
#' Parametrized queries are different from normal queries
#' in that they allow an arbitrary number of placeholders,
#' which are later substituted by actual values.
#' Parametrized queries (and statements) serve two purposes:
#'
#' - The same query can be executed more than once with different values.
#'   The DBMS may cache intermediate information for the query,
#'   such as the execution plan,
#'   and execute it faster.
#' - Separation of query syntax and parameters protects against SQL injection.
#'
#' The placeholder format is currently not specified by \pkg{DBI};
#' in the future, a uniform placeholder syntax may be supported.
#' Consult the backend documentation for the supported formats.
#' For automated testing, backend authors specify the placeholder syntax with
#' the `placeholder_pattern` tweak.
#' Known examples are:
#'
#' - `?` (positional matching in order of appearance) in \pkg{RMySQL} and \pkg{RSQLite}
#' - `$1` (positional matching by index) in \pkg{RPostgres} and \pkg{RSQLite}
#' - `:name` and `$name` (named matching) in \pkg{RSQLite}
#'
#' \pkg{DBI} clients execute parametrized statements as follows:
#'
run_bind_tester$fun <- function() {
  if (extra_obj$requires_names() && is.null(names(placeholder))) {
    # wrong_name test only valid for named placeholders
    return()
  }

  # FIXME
  #' 1. Call [DBI::dbSendQuery()] or [DBI::dbSendStatement()] with a query or statement
  #'    that contains placeholders,
  #'    store the returned \code{\linkS4class{DBIResult}} object in a variable.
  #'    Mixing placeholders (in particular, named and unnamed ones) is not
  #'    recommended.
  if (is_query())
    res <- send_query()
  else
    res <- send_statement()
  #'    It is good practice to register a call to [DBI::dbClearResult()] via
  #'    [on.exit()] right after calling `dbSendQuery()`, see the last
  #'    enumeration item.
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
  #'    supported by the backend; a [data.frame()] is internally stored as such
  #'    a list.
  # FIXME

  #'    The parameter list is passed a call to [dbBind()] on the `DBIResult`
  #'    object.
  if (!bind(res, bind_values))
    return()

  #' 1. Retrieve the data or the number of affected rows from the  `DBIResult` object.
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
    #'       the statement is processed entirely before the function returns.
    #'       Calls to `dbFetch()` are ignored.)
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



#' @template dbispec-sub-wip
#' @format NULL
#' @section Parametrised queries and statements:
#' \subsection{`dbBind("DBIResult")`}{
spec_meta_bind <- list(
  #' Empty binding with check of
  #' return value.
  bind_empty = function(ctx) {
    with_connection({
      res <- dbSendQuery(con, "SELECT 1")
      on.exit(expect_error(dbClearResult(res), NA), add = TRUE)

      bind_res <- withVisible(dbBind(res, list()))
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    })
  },

  #' Binding of integer values raises an
  #' error if connection is closed.
  bind_error = function(ctx) {
    con <- connect(ctx)
    dbDisconnect(con)
    expect_error(test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L))
  },

  #' Binding of integer values with check of
  #' return value.
  bind_return_value = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "return_value")
    })
  },

  #' Binding of integer values with too many
  #' values.
  bind_too_many = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "too_many")
    })
  },

  #' Binding of integer values with too few
  #' values.
  bind_not_enough = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "not_enough")
    })
  },

  #' Binding of integer values, repeated.
  bind_repeated = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "repeated")
    })
  },

  #' Binding of integer values with wrong names.
  bind_wrong_name = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L, extra = "wrong_name")
    })
  },

  #' Binding of integer values.
  bind_integer = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1L)
    })
  },

  #' Binding of numeric values.
  bind_numeric = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, 1.5)
    })
  },

  #' Binding of logical values.
  bind_logical = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, TRUE)
    })
  },

  #' Binding of logical values (coerced to integer).
  bind_logical_int = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, TRUE,
        transform_input = function(x) as.character(as.integer(x)))
    })
  },

  #' Binding of `NULL` values.
  bind_null = function(ctx) {
    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, NA,
        transform_input = function(x) TRUE,
        transform_output = is.na)
    })
  },

  #' Binding of character values.
  bind_character = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, texts)
    })
  },

  #' Binding of date values.
  bind_date = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, Sys.Date())
    })
  },

  #' Binding of [POSIXct] timestamp values.
  bind_timestamp = function(ctx) {
    with_connection({
      data_in <- as.POSIXct(round(Sys.time()))
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, data_in,
        type = dbDataType(con, data_in),
        transform_input = identity,
        transform_output = identity,
        expect = expect_equal)
    })
  },

  #' Binding of [POSIXlt] timestamp values.
  bind_timestamp_lt = function(ctx) {
    with_connection({
      data_in <- as.POSIXlt(round(Sys.time()))
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, data_in,
        type = dbDataType(con, data_in),
        transform_input = as.POSIXct,
        transform_output = identity)
    })
  },

  #' Binding of raw values.
  bind_raw = function(ctx) {
    if (isTRUE(ctx$tweaks$omit_blob_tests)) {
      skip("tweak: omit_blob_tests")
    }

    with_connection({
      test_select_bind(
        con, ctx$tweaks$placeholder_pattern, list(list(as.raw(1:10))),
        type = NULL,
        transform_input = identity,
        transform_output = identity)
    })
  },

  #' Binding of statements.
  bind_statement = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1), query = FALSE)
    })
  },

  #' Repeated binding of statements.
  bind_statement_repeated = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1), query = FALSE, extra = "repeated")
    })
  },

  #' }
  NULL
)
