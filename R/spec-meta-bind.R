#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
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
        transform_input = function(x) x[[1L]],
        transform_output = identity)
    })
  },

  #' }
  NULL
)


# Helpers -----------------------------------------------------------------

test_select_bind <- function(con, placeholder_fun, ...) {
  if (is.character(placeholder_fun))
    placeholder_fun <- lapply(placeholder_fun, make_placeholder_fun)
  else if (is.function(placeholder_fun))
    placeholder_fun <- list(placeholder_fun)

  if (length(placeholder_fun) == 0) {
    skip("Use the placeholder_pattern tweak, or skip all 'bind_.*' tests")
  }

  lapply(placeholder_fun, test_select_bind_one, con = con, ...)
}

test_select_bind_one <- function(con, placeholder_fun, values,
                                 type = "character(10)",
                                 transform_input = as.character,
                                 transform_output = function(x) trimws(x, "right"),
                                 expect = expect_identical,
                                 extra = c("none", "return_value", "too_many",
                                           "not_enough", "wrong_name", "repeated")) {
  extra <- match.arg(extra)

  bind_tester <- BindTester$new(con)
  bind_tester$placeholder_fun <- placeholder_fun
  bind_tester$values <- values
  bind_tester$type <- type
  bind_tester$transform$input <- transform_input
  bind_tester$transform$output <- transform_output
  bind_tester$expect$fun <- expect
  bind_tester$extra_imp <- switch(
    extra,
    return_value = BindTesterExtraReturnValue,
    too_many = BindTesterExtraTooMany,
    not_enough = BindTesterExtraNotEnough,
    wrong_name = BindTesterExtraWrongName,
    repeated = BindTesterExtraRepeated,
    BindTesterExtra
  )
  bind_tester$run()
}

run_bind_tester <- function() {
  extra_obj <- self$extra_imp$new()

  placeholder <- placeholder_fun(length(values))

  if (extra_obj$requires_names() && is.null(names(placeholder))) {
    # wrong_name test only valid for named placeholders
    return()
  }

  value_names <- letters[seq_along(values)]
  if (is.null(type)) {
    typed_placeholder <- placeholder
  } else {
    typed_placeholder <- paste0("cast(", placeholder, " as ", type, ")")
  }
  query <- paste0("SELECT ", paste0(
    typed_placeholder, " as ", value_names, collapse = ", "))
  res <- dbSendQuery(con, query)
  on.exit(expect_error(dbClearResult(res), NA))

  bind_values <- values
  if (!is.null(names(placeholder))) {
    names(bind_values) <- names(placeholder)
  }

  error_bind_values <- extra_obj$patch_bind_values(bind_values)

  if (!identical(bind_values, error_bind_values)) {
    expect_error(dbBind(res, as.list(error_bind_values)))
    return()
  }

  bind_res <- withVisible(dbBind(res, as.list(bind_values)))
  extra_obj$check_return_value(bind_res, res)

  rows <- dbFetch(res)
  expect$fun(transform$output(Reduce(c, rows)), transform$input(unname(values)))

  if (extra_obj$is_repeated()) {
    dbBind(res, as.list(bind_values))

    rows <- dbFetch(res)
    expect$fun(transform$output(Reduce(c, rows)), transform$input(unname(values)))
  }
}


# BindTesterExtra ---------------------------------------------------------

BindTesterExtra <- R6::R6Class(
  "BindTesterExtra",
  portable = TRUE,

  public = list(
    check_return_value = function(bind_res, res) invisible(NULL),
    patch_bind_values = identity,
    requires_names = function() FALSE,
    is_repeated = function() FALSE
  )
)


# BindTesterExtraReturnValue ----------------------------------------------

BindTesterExtraReturnValue <- R6::R6Class(
  "BindTesterExtraReturnValue",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    check_return_value = function(bind_res, res) {
      expect_false(bind_res$visible)
      expect_identical(res, bind_res$value)
    }
  )
)


# BindTesterExtraTooMany --------------------------------------------------

BindTesterExtraTooMany <- R6::R6Class(
  "BindTesterExtraTooMany",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    patch_bind_values = function(bind_values) {
      c(bind_values, bind_values[[1L]])
    }
  )
)


# BindTesterExtraNotEnough --------------------------------------------------

BindTesterExtraNotEnough <- R6::R6Class(
  "BindTesterExtraNotEnough",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    patch_bind_values = function(bind_values) {
      bind_values[-1L]
    }
  )
)


# BindTesterExtraWrongName ------------------------------------------------

BindTesterExtraWrongName <- R6::R6Class(
  "BindTesterExtraWrongName",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    patch_bind_values = function(bind_values) {
      stats::setNames(bind_values, paste0("bogus", names(bind_values)))
    },

    requires_names = function() TRUE
  )
)


# BindTesterExtraRepeated -------------------------------------------------

BindTesterExtraRepeated <- R6::R6Class(
  "BindTesterExtraRepeated",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    is_repeated = function() TRUE
  )
)


# BindTester --------------------------------------------------------------

BindTester <- R6::R6Class(
  "BindTester",
  portable = FALSE,

  public = list(
    initialize = function(con) {
      self$con <- con
    },
    run = run_bind_tester,

    con = NULL,
    placeholder_fun = NULL,
    values = NULL,
    type = "character(10)",
    transform = list(input = as.character, output = function(x) trimws(x, "right")),
    expect = list(fun = expect_identical),
    extra_imp = BindTesterExtra
  )
)


# make_placeholder_fun ----------------------------------------------------

#' Create a function that creates n placeholders
#'
#' For internal use by the `placeholder_format` tweak.
#'
#' @param pattern `[character(1)]`\cr Any character, optionally followed by `1` or `name`. Examples: `"?"`, `"$1"`, `":name"`
#'
#' @return `[function(n)]`\cr A function with one argument `n` that
#'   returns a vector of length `n` with placeholders of the specified format.
#'   Examples: `?, ?, ?, ...`, `$1, $2, $3, ...`, `:a, :b, :c`
#'
#' @keywords internal
make_placeholder_fun <- function(pattern) {
  format_rx <- "^(.)(.*)$"

  character <- gsub(format_rx, "\\1", pattern)
  kind <- gsub(format_rx, "\\2", pattern)

  if (character == "") {
    stop("placeholder pattern must have at least one character", call. = FALSE)
  }

  if (kind == "") {
    eval(bquote(
      function(n) .(character)
    ))
  } else if (kind == "1") {
    eval(bquote(
      function(n) paste0(.(character), seq_len(n))
    ))
  } else if (kind == "name") {
    eval(bquote(
      function(n) {
        l <- letters[seq_len(n)]
        stats::setNames(paste0(.(character), l), l)
      }
    ))
  } else {
    stop("Pattern must be any character, optionally followed by 1 or name. Examples: $1, :name", call. = FALSE)
  }
}
