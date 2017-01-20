#' Tweaks for DBI tests
#'
#' TBD.
#' @name tweaks
#' @aliases NULL
{ # nolint
  tweak_names <- alist(
    #' @param ... `[any]`\cr
    #'   Unknown tweaks are accepted, with a warning.  The ellipsis
    #'   also asserts that all arguments are named.
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
    #'   A pattern for placeholders used in [DBI::dbBind()], e.g.,
    #'   `"?"`, `"$1"`, or `":name"`. See
    #'   [make_placeholder_fun()] for details.
    "placeholder_pattern" = NULL,

    #' @param logical_return `[function(logical)]`\cr
    #'   A function that converts logical values to the data type returned
    #'   by the DBI backend.
    "logical_return" = identity,

    # Dummy argument
    NULL
  )
}

# A helper function that constructs the tweaks() function in a DRY fashion.
make_tweaks <- function(envir = parent.frame()) {
  fmls <- tweak_names[-length(tweak_names)]

  tweak_quoted <- lapply(setNames(nm = names(fmls)), as.name)
  tweak_quoted <- c(tweak_quoted)
  list_call <- as.call(c(quote(list), tweak_quoted))

  fun <- eval(bquote(function() {
    unknown <- list(...)
    if (length(unknown) > 0) {
      if (is.null(names(unknown)) || any(names(unknown) == "")) {
        warning("All tweaks must be named", call. = FALSE)
      } else {
        warning("Unknown tweaks: ", paste(names(unknown), collapse = ", "),
                call. = FALSE)
      }
    }
    ret <- .(list_call)
    ret <- ret[!vapply(ret, is.null, logical(1L))]
    structure(ret, class = "DBItest_tweaks")
  }
  , as.environment(list(list_call = list_call))))

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
      names(x), unclass(x)))
  )
}

#' @export
print.DBItest_tweaks <- function(x, ...) {
  cat(format(x), sep = "\n")
}
