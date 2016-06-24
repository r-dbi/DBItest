#' Tweaks for DBI tests
#'
#' TBD.
#' @name tweaks
{ # nolint
  tweak_names <- c(
    #' @param ... \code{[any]}\cr
    #'   Unknown tweaks are accepted, with a warning.  The ellipsis
    #'   also asserts that all arguments are named.
    "...",

    #' @param constructor_name \code{[character(1)]}\cr
    #'   Name of the function that constructs the \code{Driver} object.
    "constructor_name",

    #' @param constructor_relax_args \code{[logical(1)]}\cr
    #'   If \code{TRUE}, allow a driver constructor with default values for all
    #'   arguments; otherwise, require a constructor with empty argument list
    #'   (default).
    "constructor_relax_args",

    #' @param strict_identifier \code{[logical(1)]}\cr
    #'   Set to \code{TRUE} if the DBMS does not support arbitrarily-named
    #'   identifiers even when quoting is used.
    "strict_identifier",

    #' @param omit_blob_tests \code{[logical(1)]}\cr
    #'   Set to \code{TRUE} if the DBMS does not support a \code{BLOB} data
    #'   type.
    "omit_blob_tests",

    #' @param current_needs_parens \code{[logical(1)]}\cr
    #'   Set to \code{TRUE} if the SQL functions \code{current_date},
    #'   \code{current_time}, and \code{current_timestamp} require parentheses.
    "current_needs_parens",

    #' @param union \code{[function(character)]}\cr
    #'   Function that combines several subqueries into one so that the
    #'   resulting query returns the concatenated results of the subqueries
    "union",

    # Dummy argument
    NULL
  )
}

# A helper function that constructs the tweaks() function in a DRY fashion.
make_tweaks <- function(envir = parent.frame()) {
  fmls <- vector(mode = "list", length(tweak_names))
  names(fmls) <- tweak_names
  fmls["..."] <- alist(`...` = )

  tweak_quoted <- lapply(setNames(nm = tweak_names), as.name)
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
