#' Tweaks for DBI tests
#'
#' The tweaks are a way to control the behavior of certain tests. Currently,
#' you need to search the \pkg{DBItest} source code to understand which tests
#' are affected by which tweaks. This function is usually called to set the
#' `tweaks` argument in a [make_context()] call.
#'
#' @name tweaks
#' @aliases NULL
#' @examples
#' \dontrun{
#' make_context(..., tweaks = tweaks(strict_identifier = TRUE))
#' }
{
  # nolint
  tweak_names <- alist(
    #' @param ... `[any]`\cr
    #'   Unknown tweaks are accepted, with a warning.  The ellipsis
    #'   also makes sure that you only can pass named arguments.
    "..." = ,

    #' @param constructor_name `[character(1)]`\cr
    #'   Name of the function that constructs the `Driver` object.
    "constructor_name" = NULL,

    #' @param constructor_relax_args `[logical(1)]`\cr
    #'   If `TRUE`, allow a driver constructor with default values for all
    #'   arguments; otherwise, require a constructor with empty argument list
    #'   (default).
    "constructor_relax_args" = FALSE,

    #' @param strict_identifier `[logical(1)]`\cr
    #'   Set to `TRUE` if the DBMS does not support arbitrarily-named
    #'   identifiers even when quoting is used.
    "strict_identifier" = FALSE,

    #' @param omit_blob_tests `[logical(1)]`\cr
    #'   Set to `TRUE` if the DBMS does not support a `BLOB` data
    #'   type.
    "omit_blob_tests" = FALSE,

    #' @param current_needs_parens `[logical(1)]`\cr
    #'   Set to `TRUE` if the SQL functions `current_date`,
    #'   `current_time`, and `current_timestamp` require parentheses.
    "current_needs_parens" = FALSE,

    #' @param union `[function(character)]`\cr
    #'   Function that combines several subqueries into one so that the
    #'   resulting query returns the concatenated results of the subqueries
    "union" = function(x) paste(x, collapse = " UNION "),

    #' @param placeholder_pattern `[character]`\cr
    #'   A pattern for placeholders used in [dbBind()], e.g.,
    #'   `"?"`, `"$1"`, or `":name"`. See
    #'   [make_placeholder_fun()] for details.
    "placeholder_pattern" = NULL,

    #' @param logical_return `[function(logical)]`\cr
    #'   A vectorized function that converts logical values to the data type
    #'   returned by the DBI backend.
    "logical_return" = identity,

    #' @param date_cast `[function(character)]`\cr
    #'   A vectorized function that creates an SQL expression for coercing a
    #'   string to a date value.
    "date_cast" = function(x) paste0("date('", x, "')"),

    #' @param time_cast `[function(character)]`\cr
    #'   A vectorized function that creates an SQL expression for coercing a
    #'   string to a time value.
    "time_cast" = function(x) paste0("time('", x, "')"),

    #' @param timestamp_cast `[function(character)]`\cr
    #'   A vectorized function that creates an SQL expression for coercing a
    #'   string to a timestamp value.
    "timestamp_cast" = function(x) paste0("timestamp('", x, "')"),

    #' @param blob_cast `[function(character)]`\cr
    #'   A vectorized function that creates an SQL expression for coercing a
    #'   string to a blob value.
    "blob_cast" = identity,

    #' @param date_typed `[logical(1L)]`\cr
    #'   Set to `FALSE` if the DBMS doesn't support a dedicated type for dates.
    "date_typed" = TRUE,

    #' @param time_typed `[logical(1L)]`\cr
    #'   Set to `FALSE` if the DBMS doesn't support a dedicated type for times.
    "time_typed" = TRUE,

    #' @param timestamp_typed `[logical(1L)]`\cr
    #'   Set to `FALSE` if the DBMS doesn't support a dedicated type for
    #'   timestamps.
    "timestamp_typed" = TRUE,

    #' @param temporary_tables `[logical(1L)]`\cr
    #'   Set to `FALSE` if the DBMS doesn't support temporary tables.
    "temporary_tables" = TRUE,

    #' @param list_temporary_tables `[logical(1L)]`\cr
    #'   Set to `FALSE` if the DBMS doesn't support listing temporary tables.
    "list_temporary_tables" = TRUE,

    #' @param allow_na_rows_affected `[logical(1L)]`\cr
    #'   Set to `TRUE` to allow [dbGetRowsAffected()] to return `NA`.
    "allow_na_rows_affected" = FALSE,

    #' @param is_null_check `[function(character)]`\cr
    #'   A vectorized function that creates an SQL expression for checking if a
    #'   value is `NULL`.
    "is_null_check" = function(x) paste0("(", x, " IS NULL)"),

    #' @param create_table_as `[function(character(1), character(1))]`\cr
    #'   A function that creates an SQL expression for creating a table
    #'   from an SQL expression.
    "create_table_as" = function(table_name, query) paste0("CREATE TABLE ", table_name, " AS ", query),

    #' @param dbitest_version `[character(1)]`\cr
    #'   Compatible DBItest version, default: "1.7.1".
    "dbitest_version" = "1.7.1",

    # Dummy argument
    NULL
  )
}

# A helper function that constructs the tweaks() function in a DRY fashion.
make_tweaks <- function(envir = parent.frame()) {
  fmls <- tweak_names[-length(tweak_names)]

  tweak_quoted <- map(setNames(nm = names(fmls)), as.name)
  tweak_quoted <- c(tweak_quoted)
  list_call <- as.call(c(quote(list), tweak_quoted[-1]))

  fun <- eval(bquote(
    function() {
      unknown <- list(...)
      if (length(unknown) > 0) {
        if (is.null(names(unknown)) || any(names(unknown) == "")) {
          warning("All tweaks must be named", call. = FALSE)
        } else {
          warning("Unknown tweaks: ", paste(names(unknown), collapse = ", "),
            call. = FALSE
          )
        }
      }
      ret <- .(list_call)
      ret <- compact(ret)
      structure(ret, class = "DBItest_tweaks")
    },
    as.environment(list(list_call = list_call))
  ))

  formals(fun) <- fmls
  environment(fun) <- envir
  fun
}

#' @export
#' @rdname tweaks
tweaks <- make_tweaks()

#' @export
format.DBItest_tweaks <- function(x, ...) {
  if (length(x) == 0L) {
    return("DBItest tweaks: Empty")
  }
  c(
    "DBItest tweaks:",
    unlist(mapply(
      function(name, value) {
        paste0("  ", name, ": ", format(value)[[1]])
      },
      names(x), unclass(x)
    ))
  )
}

#' @export
print.DBItest_tweaks <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
`$.DBItest_tweaks` <- function(x, tweak) {
  if (!(tweak %in% names(tweak_names))) {
    stop("Tweak not found: ", tweak, call. = FALSE)
  }
  NextMethod()
}
