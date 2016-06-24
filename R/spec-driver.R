#' @rdname DBIspec
#' @format NULL
#' @section Driver:
spec_driver <- list(
  inherits_from_driver = function(ctx) {
    #' Each DBI backend implements a "driver" class,
    #' which must be an S4 class and inherit from the \code{DBIDriver} class.
    expect_s4_class(ctx$drv, "DBIDriver")
  },

  #' SQL Data types exist for all basic R data types. dbDataType() does not
  #' throw an error and returns a nonempty atomic character
  data_type_driver = function(ctx) {
    check_driver_data_type <- function(value) {
      eval(bquote({
        expect_is(dbDataType(ctx$drv, .(value)), "character")
        expect_equal(length(dbDataType(ctx$drv, .(value))), 1L)
        expect_match(dbDataType(ctx$drv, .(value)), ".")
      }))
    }

    expect_driver_has_data_type <- function(value) {
      eval(bquote(
        expect_error(check_driver_data_type(.(value)), NA)))
    }

    expect_driver_has_data_type(logical(1))
    expect_driver_has_data_type(integer(1))
    expect_driver_has_data_type(numeric(1))
    expect_driver_has_data_type(character(1))
    expect_driver_has_data_type(Sys.Date())
    expect_driver_has_data_type(Sys.time())
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      expect_driver_has_data_type(list(raw(1)))
    }
  },

  #' Package exports constructor function that has no arguments.
  #'   The name of the constructor can be tweaked via \code{constructor_name}
  #'   in the context's \code{\link{tweaks}}, default: package name without
  #'   the leading R.
  #'   This test is optional, the
  #'   \code{constructor} test is a slightly weaker version.
  constructor_strict = function(ctx) {
    pkg_name <- package_name(ctx)

    constructor_name <- ctx$tweaks$constructor_name %||%
      gsub("^R", "", pkg_name)

    pkg_env <- getNamespace(pkg_name)
    eval(bquote(
      expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))))
    eval(bquote(
      expect_true(exists(.(constructor_name), mode = "function", pkg_env))))
    constructor <- get(constructor_name, mode = "function", pkg_env)
    expect_that(constructor, arglist_is_empty())
  },

  #' Package exports constructor function, all arguments have default values.
  #'   The name of the constructor can be tweaked via \code{constructor_name}
  #'   in the context's \code{\link{tweaks}}, default: package name without
  #'   the leading R (if it exists).
  constructor = function(ctx) {
    pkg_name <- package_name(ctx)

    constructor_name <- ctx$tweaks$constructor_name %||%
      gsub("^R", "", pkg_name)

    pkg_env <- getNamespace(pkg_name)
    eval(bquote(
      expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))))
    eval(bquote(
      expect_true(exists(.(constructor_name), mode = "function", pkg_env))))
    constructor <- get(constructor_name, mode = "function", pkg_env)
    expect_that(constructor, all_args_have_default_values())
  },

  #' Return value of dbGetInfo has necessary elements
  get_info_driver = function(ctx) {
    info <- dbGetInfo(ctx$drv)
    expect_is(info, "list")
    info_names <- names(info)

    necessary_names <-
      c("driver.version", "client.version", "max.connections")

    for (name in necessary_names) {
      eval(bquote(
        expect_true(.(name) %in% info_names)))
    }
  },

  #' Repeated load, instantiation, and unload of package in a new R session.
  stress_load_unload = function(ctx) {
    skip_on_travis()
    skip_on_appveyor()
    skip_if_not(getRversion() != "3.3.0")

    pkg <- get_pkg(ctx)

    script_file <- tempfile("DBItest", fileext = ".R")
    cat(
      "devtools::RCMD('INSTALL', ", shQuote(pkg$path), ")\n",
      "for (i in 1:50) {\n",
      "  ", pkg$package, "::", deparse(ctx$drv_call), "\n",
      "  unloadNamespace(getNamespace(\"", pkg$package, "\"))\n",
      "}\n",
      sep = "",
      file = script_file
    )

    with_temp_libpaths({
      expect_equal(system(paste0("R -q --vanilla -f ", shQuote(script_file)),
                          ignore.stdout = TRUE, ignore.stderr = TRUE),
                   0L)
    })
  },

  NULL
)
