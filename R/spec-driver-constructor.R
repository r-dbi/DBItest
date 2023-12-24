#' spec_driver_constructor
#' @family driver specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @section Construction of the DBIDriver object:
spec_driver_constructor <- list(
  constructor = function(ctx) {
    pkg_name <- package_name(ctx)

    #' The backend must support creation of an instance of its [DBIDriver-class] subclass
    #' with a \dfn{constructor function}.
    #' By default, its name is the package name without the leading \sQuote{R}
    #' (if it exists), e.g., `SQLite` for the \pkg{RSQLite} package.
    default_constructor_name <- gsub("^R", "", pkg_name)

    #' However, backend authors may choose a different name.
    constructor_name <- ctx$tweaks$constructor_name %||% default_constructor_name

    #' The constructor must be exported, and
    pkg_env <- getNamespace(pkg_name)
    eval(bquote(
      expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))
    ))

    #' it must be a function
    eval(bquote(
      expect_true(exists(.(constructor_name), mode = "function", pkg_env))
    ))
    constructor <- get(constructor_name, mode = "function", pkg_env)

    #' that is callable without arguments.
    expect_all_args_have_default_values(constructor)
    #' DBI recommends to define a constructor with an empty argument list.
    if (!isTRUE(ctx$tweaks$constructor_relax_args)) {
      expect_arglist_is_empty(constructor)
    }
  },
  #
  NULL
)
