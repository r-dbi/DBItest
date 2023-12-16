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
run_bind_tester$fun <- function(
    con,
    ...,
    # Run time
    placeholder_fun,
    is_null_check,
    cast_fun,
    allow_na_rows_affected,
    # Spec time
    bind_values,
    query,
    skip_fun,
    check_return_value,
    patch_bind_values,
    bind_error,
    is_repeated,
    is_premature_clear,
    is_untouched) {
  rlang::check_dots_empty()
  force(placeholder_fun)
  force(is_null_check)
  force(cast_fun)
  force(allow_na_rows_affected)
  force(bind_values)
  force(query)
  force(skip)
  force(check_return_value)
  force(patch_bind_values)
  force(bind_error)
  force(is_repeated)
  force(is_premature_clear)
  force(is_untouched)

  if (is.null(patch_bind_values)) {
    patch_bind_values <- identity
  }

  bind_values_expr <- rlang::expr({
    bind_values <- !!construct_expr(bind_values)
  })

  skip_expr <- if (!is.null(skip_fun) && skip_fun()) rlang::expr({
    skip(rlang::expr_deparse(body(skip_fun)))
  })

  #' 1. Call [dbSendQuery()] or [dbSendStatement()] with a query or statement
  #'    that contains placeholders,
  #'    store the returned [DBIResult-class] object in a variable.
  #'    Mixing placeholders (in particular, named and unnamed ones) is not
  #'    recommended.
  send_expr <- if (query) rlang::expr({
    ret_values <- trivial_values(2)
    placeholder <- placeholder_fun(length(bind_values))
    is_na <- vapply(bind_values, is_na_or_null, logical(1))
    placeholder_values <- vapply(bind_values, function(x) DBI::dbQuoteLiteral(con, x[1]), character(1))
    result_names <- letters[seq_along(bind_values)]

    sql <- paste0(
      "SELECT ",
      paste0(
        "CASE WHEN ",
        ifelse(
          is_na,
          paste0("(", is_null_check(cast_fun(placeholder)), ")"),
          paste0("(", cast_fun(placeholder), " = ", placeholder_values, ")")
        ),
        " THEN ", ret_values[[1]],
        " ELSE ", ret_values[[2]], " END",
        " AS ", result_names,
        collapse = ", "
      )
    )

    res <- dbSendQuery(con, sql)
  }) else rlang::expr({
    data <- data.frame(a = rep(1:5, 1:5))
    data$b <- seq_along(data$a)
    table_name <- random_table_name()
    dbWriteTable(con, table_name, data, temporary = TRUE)

    value_names <- letters[seq_along(bind_values)]
    placeholder <- placeholder_fun(length(bind_values))
    sql <- paste0(
      "UPDATE ", dbQuoteIdentifier(con, table_name), " SET b = b + 1 WHERE ",
      paste(value_names, " = ", placeholder, collapse = " AND ")
    )

    res <- dbSendStatement(con, sql)
  })

  #'    It is good practice to register a call to [dbClearResult()] via
  #'    [on.exit()] right after calling `dbSendQuery()` or `dbSendStatement()`
  #'    (see the last enumeration item).
  clear_expr <- if (is_premature_clear) rlang::expr({
    dbClearResult(res)
  }) else {
    on_exit_expr <- rlang::expr({
      on.exit(expect_error(dbClearResult(res), NA))
    })

    #'    Until `dbBind()` has been called, the returned result set object has the
    #'    following behavior:
    initial_state_expr <- if (query) rlang::expr({
      #'     - [dbFetch()] raises an error (for `dbSendQuery()`)
      expect_error(dbFetch(res))
      #'     - [dbGetRowCount()] returns zero (for `dbSendQuery()`)
      expect_equal(dbGetRowCount(res), 0)
    }) else rlang::expr({
      #'     - [dbGetRowsAffected()] returns an integer `NA` (for `dbSendStatement()`)
      expect_identical(dbGetRowsAffected(res), NA_integer_)
    })

    initial_state_expr_2 <- rlang::expr({
      #'     - [dbIsValid()] returns `TRUE`
      expect_true(dbIsValid(res))
      #'     - [dbHasCompleted()] returns `FALSE`
      expect_false(dbHasCompleted(res))
    })

    rlang::expr({
      !!!on_exit_expr
      !!!initial_state_expr
      !!!initial_state_expr_2
    })
  }

  #' 1. Construct a list with parameters
  #'    that specify actual values for the placeholders.
  #'    The list must be named or unnamed,
  #'    depending on the kind of placeholders used.
  #'    Named values are matched to named parameters, unnamed values
  #'    are matched by position in the list of parameters.
  name_values_expr <- rlang::expr(if (!is.null(names(placeholder_fun(1)))) {
    names(bind_values) <- names(placeholder_fun(length(bind_values)))
  })

  #'    All elements in this list must have the same lengths and contain values
  #'    supported by the backend; a [data.frame] is internally stored as such
  #'    a list.
  #'    The parameter list is passed to a call to `dbBind()` on the `DBIResult`
  #'    object.
  bind_expr <- if (is.na(bind_error)) rlang::expr({
    bind_res <- withVisible(dbBind(res, patch_bind_values(bind_values)))
    if (!is.null(check_return_value)) {
      check_return_value(bind_res, res)
    }
  }) else rlang::expr({
    expect_error(
      withVisible(dbBind(res, patch_bind_values(bind_values))),
      bind_error
    )
  })

  #' 1. Retrieve the data or the number of affected rows from the `DBIResult` object.
  #'     - For queries issued by `dbSendQuery()`,
  #'       call [dbFetch()].
  retrieve_expr <- if (query) rlang::expr({
    rows <- check_df(dbFetch(res))
    expect_equal(nrow(rows), length(bind_values[[1]]))
    if (nrow(rows) > 0) {
      result_names <- letters[seq_along(bind_values)]
      expected <- c(trivial_values(1), rep(trivial_values(2)[[2]], nrow(rows) - 1))
      all_expected <- rep(list(expected), length(bind_values))
      result <- as.data.frame(setNames(all_expected, result_names))

      expect_equal(rows, result)
    }
  }) else rlang::expr({
    #'     - For statements issued by `dbSendStatements()`,
    #'       call [dbGetRowsAffected()].
    #'       (Execution begins immediately after the `dbBind()` call,
    #'       the statement is processed entirely before the function returns.)
    rows_affected <- dbGetRowsAffected(res)
    # Allow NA value for dbGetRowsAffected(), #297
    if (!isTRUE(allow_na_rows_affected) || !is.na(rows_affected)) {
      expect_equal(rows_affected, sum(bind_values[[1]]))
    }
  })

  not_untouched_expr <- if (!is_untouched) rlang::expr({
    !!retrieve_expr
  })

  #' 1. Repeat 2. and 3. as necessary.
  repeated_expr <- if (is_repeated) rlang::expr({
    bind_res <- withVisible(dbBind(res, patch_bind_values(bind_values)))
    if (!is.null(check_return_value)) {
      check_return_value(bind_res, res)
    }
    !!retrieve_expr
  })

  #' 1. Close the result set via [dbClearResult()].

  early_exit <-
    is_premature_clear ||
      !is.na(bind_error) ||
      !identical(bind_values, patch_bind_values(bind_values))

  post_bind_expr <- if (!early_exit) rlang::expr({
    !!not_untouched_expr
    !!repeated_expr
  })

  test_expr <- rlang::expr({
    !!skip_expr
    !!bind_values_expr
    !!name_values_expr
    !!send_expr
    !!clear_expr
    !!bind_expr
    !!post_bind_expr
  })

  rm(bind_values)
  rlang::eval_bare(test_expr)
}

construct_expr <- function(x) {
  xc <- constructive::construct(x)
  rlang::parse_expr(format(xc$code))
}
