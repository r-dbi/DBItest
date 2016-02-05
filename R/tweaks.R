#' Tweaks for DBI tests
#'
#' TBD.
#' @name tweaks
{ # nolint
  tweak_names <- c(
    #' @param constructor_name \code{[character(1)]}\cr
    #'   Name of the function that constructs the \code{Driver} object.
    "constructor_name",

    #' @param union \code{[function(character)]}\cr
    #'   Function that combines several subqueries into one so that the
    #'   resulting query returns the concatenated results of the subqueries
    "union",
    NULL
  )
}

# A helper function that constructs the tweaks() function in a DRY fashion.
make_tweaks <- function(envir = parent.frame()) {
  fmls <- vector(mode = "list", length(tweak_names))
  names(fmls) <- tweak_names

  tweak_quoted <- lapply(setNames(nm = tweak_names), as.name)
  list_call <- as.call(c(quote(list), tweak_quoted))

  fun <- eval(bquote(function() {
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
        paste0("  ", name, ": ", value)
      },
      names(x), unclass(x)))
  )
}

#' @export
print.DBItest_tweaks <- function(x, ...) {
  cat(format(x), sep = "\n")
}
