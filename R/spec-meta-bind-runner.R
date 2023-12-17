test_select_bind_expr_one <- list()

#' spec_meta_bind
#' @family meta specifications
#' @name spec_meta_bind
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Specification:
#' \pkg{DBI} clients execute parametrized statements as follows:
#'
test_select_bind_expr_one$fun <- function(
    bind_values,
    ...,
    query = TRUE,
    has_cast_fun = FALSE,
    check_return_value = NULL,
    patch_bind_values = NULL,
    bind_error = NA,
    warn = FALSE,
    is_repeated = FALSE,
    is_premature_clear = FALSE,
    is_untouched = FALSE) {
  rlang::check_dots_empty()
  force(bind_values)
  force(query)
  force(check_return_value)
  force(patch_bind_values)
  force(bind_error)
  force(is_repeated)
  force(is_premature_clear)
  force(is_untouched)

  bind_values_expr <- rlang::expr({
    bind_values <- !!construct_expr(bind_values)
  })

  bind_values_patched_expr <- if (is.null(patch_bind_values)) rlang::expr({
    bind_values_patched <- bind_values
  }) else rlang::expr({
    bind_values_patched <- !!body(patch_bind_values)
  })

  cast_fun_placeholder_expr <- if (has_cast_fun) rlang::expr({
    cast_fun(placeholder)
  }) else rlang::expr({
    placeholder
  })

  #' 1. Call [dbSendQuery()] or [dbSendStatement()] with a query or statement
  #'    that contains placeholders,
  #'    store the returned [DBIResult-class] object in a variable.
  #'    Mixing placeholders (in particular, named and unnamed ones) is not
  #'    recommended.
  send_expr <- if (query) rlang::expr({
    placeholder <- placeholder_fun(length(bind_values))
    is_na <- map_lgl(bind_values, is_na_or_null)
    placeholder_values <- map_chr(bind_values, function(x) DBI::dbQuoteLiteral(con, x[1]))
    result_check <- ifelse(
      is_na,
      paste0("(", is_null_check(!!cast_fun_placeholder_expr), ")"),
      paste0("(", !!cast_fun_placeholder_expr, " = ", placeholder_values, ")")
    )
    result_names <- letters[seq_along(bind_values)]

    sql <- paste0(
      "SELECT ",
      paste0(
        "CASE WHEN ",
        result_check,
        !!paste0(" THEN ", trivial_values(2)[[1]], " ELSE ", trivial_values(2)[[2]], " END", " AS "),
        result_names,
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
      on.exit(if (!is.null(res)) expect_error(dbClearResult(res), NA))
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

  check_return_value_expr <- if (!is.null(check_return_value)) rlang::expr({
    !!body(check_return_value)
  })

  #'    All elements in this list must have the same lengths and contain values
  #'    supported by the backend; a [data.frame] is internally stored as such
  #'    a list.
  #'    The parameter list is passed to a call to `dbBind()` on the `DBIResult`
  #'    object.
  bind_expr <- if (isTRUE(warn)) rlang::expr({
    bind_res <- withVisible(suppressWarnings(expect_warning(dbBind(res, bind_values_patched))))
    !!check_return_value_expr
  }) else if (is.na(bind_error)) rlang::expr({
    bind_res <- withVisible(dbBind(res, bind_values_patched))
    !!check_return_value_expr
  }) else rlang::expr({
    expect_error(
      withVisible(dbBind(res, bind_values_patched)),
      !!bind_error
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
      expected <- c(!!trivial_values(1), rep(!!trivial_values(2)[[2]], nrow(rows) - 1))
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
    !!bind_expr
    !!retrieve_expr
  })

  early_exit <-
    is_premature_clear ||
      !is.na(bind_error) ||
      (!is.null(patch_bind_values) && !identical(bind_values, patch_bind_values(bind_values)))

  post_bind_expr <- if (!early_exit) rlang::expr({
    !!not_untouched_expr
    !!repeated_expr
  })

  #' 1. Close the result set via [dbClearResult()].
  clear_now_expr <- if (!is_premature_clear) rlang::expr({
    expect_error(dbClearResult(res), NA)
    res <- NULL
  })

  test_expr <- rlang::expr({
    !!bind_values_expr
    !!name_values_expr
    !!bind_values_patched_expr
    !!send_expr
    !!clear_expr
    !!bind_expr
    !!post_bind_expr
    !!clear_now_expr
  })

  test_expr
}

construct_expr <- function(x) {
  xc <- constructive::construct(x)
  rlang::parse_expr(format(xc$code))
}
