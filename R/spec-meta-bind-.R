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
                                 query = TRUE,
                                 transform_input = as.character,
                                 transform_output = function(x) trimws(x, "right"),
                                 expect = expect_identical,
                                 extra = "none") {
  bind_tester <- BindTester$new(con)
  bind_tester$placeholder <- placeholder_fun(length(values))
  bind_tester$values <- values
  bind_tester$type <- type
  bind_tester$query <- query
  bind_tester$transform$input <- transform_input
  bind_tester$transform$output <- transform_output
  bind_tester$expect$fun <- expect
  bind_tester$extra_obj <- new_extra_imp(extra)

  bind_tester$run()
}

new_extra_imp <- function(extra) {
  if (length(extra) == 0)
    new_extra_imp_one("none")
  else if (length(extra) == 1)
    new_extra_imp_one(extra)
  else {
    stop("need BindTesterExtraMulti")
    # BindTesterExtraMulti$new(lapply(extra, new_extra_imp_one))
  }
}

new_extra_imp_one <- function(extra) {
  extra_imp <- switch(
    extra,
    return_value = BindTesterExtraReturnValue,
    too_many = BindTesterExtraTooMany,
    not_enough = BindTesterExtraNotEnough,
    wrong_name = BindTesterExtraWrongName,
    unequal_length = BindTesterExtraUnequalLength,
    repeated = BindTesterExtraRepeated,
    none = BindTesterExtra,
    stop("Unknown extra: ", extra, call. = FALSE)
  )

  extra_imp$new()
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


# BindTesterExtraUnequalLength --------------------------------------------

BindTesterExtraUnequalLength <- R6::R6Class(
  "BindTesterExtraUnequalLength",
  inherit = BindTesterExtra,
  portable = TRUE,

  public = list(
    patch_bind_values = function(bind_values) {
      bind_values[[2]] <- bind_values[[2]][-1]
      bind_values
    }
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
    run = run_bind_tester$fun,

    con = NULL,
    placeholder = NULL,
    values = NULL,
    type = "character(10)",
    query = TRUE,
    transform = list(input = as.character, output = function(x) trimws(x, "right")),
    expect = list(fun = expect_identical),
    extra_obj = NULL
  ),

  private = list(
    is_query = function() {
      query
    },

    send_query = function() {
      value_names <- letters[seq_along(values)]
      if (is.null(type)) {
        typed_placeholder <- placeholder
      } else {
        typed_placeholder <- paste0("cast(", placeholder, " as ", type, ")")
      }
      query <- paste0("SELECT ", paste0(
        typed_placeholder, " as ", value_names, collapse = ", "))

      dbSendQuery(con, query)
    },

    send_statement = function() {
      data <- data.frame(a = rep(1:5, 1:5))
      data$b <- seq_along(data$a)
      table_name <- random_table_name()
      dbWriteTable(con, table_name, data, temporary = TRUE)

      value_names <- letters[seq_along(values)]
      statement <- paste0(
        "UPDATE ", dbQuoteIdentifier(con, table_name), "SET b = b + 1 WHERE ",
        paste(value_names, " = ", placeholder, collapse = " AND "))

      dbSendStatement(con, statement)
    },

    bind = function(res, bind_values) {
      error_bind_values <- extra_obj$patch_bind_values(bind_values)

      if (!identical(bind_values, error_bind_values)) {
        expect_error(dbBind(res, error_bind_values))
        return(FALSE)
      }

      bind_res <- withVisible(dbBind(res, bind_values))
      extra_obj$check_return_value(bind_res, res)

      TRUE
    },

    compare = function(rows, values) {
      expect$fun(lapply(unname(rows), transform$output),
                 lapply(unname(values), transform$input))
    },

    compare_affected = function(rows_affected, values) {
      expect_equal(rows_affected, sum(values[[1]]))
    }
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
