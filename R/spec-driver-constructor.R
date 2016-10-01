#' @template dbispec-sub
#' @format NULL
#' @section Driver:
#' \subsection{Construction}{
spec_driver_constructor <- list(
  constructor = function(ctx) {
    pkg_name <- package_name(ctx)

    #' The backend must support creation of an instance of this driver class
    #' with a \dfn{constructor function}.
    #' By default, its name is the package name without the leading \sQuote{R}
    #' (if it exists), e.g., `SQLite` for the \pkg{RSQLite} package.
    default_constructor_name <- gsub("^R", "", pkg_name)

    #' For the automated tests, the constructor name can be tweaked using the
    #' `constructor_name` tweak.
    constructor_name <- ctx$tweaks$constructor_name %||% default_constructor_name

    #'
    #' The constructor must be exported, and
    pkg_env <- getNamespace(pkg_name)
    eval(bquote(
      expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))))

    #' it must be a function
    eval(bquote(
      expect_true(exists(.(constructor_name), mode = "function", pkg_env))))
    constructor <- get(constructor_name, mode = "function", pkg_env)

    #' that is callable without arguments.
    #' For the automated tests, unless the
    #' `constructor_relax_args` tweak is set to `TRUE`,
    if (!isTRUE(ctx$tweaks$constructor_relax_args)) {
      #' an empty argument list is expected.
      expect_that(constructor, arglist_is_empty())
    } else {
      #' Otherwise, an argument list where all arguments have default values
      #' is also accepted.
      expect_that(constructor, all_args_have_default_values())
    }
    #'
  },

  #' }
  NULL
)
