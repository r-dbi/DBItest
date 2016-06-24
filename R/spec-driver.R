#' @rdname DBIspec
#' @format NULL
#' @usage NULL
#' @aliases NULL
#' @section Driver:
spec_driver_class <- list(
  inherits_from_driver = function(ctx) {
    #' Each DBI backend implements a "driver" class,
    #' which must be an S4 class and inherit from the \code{DBIDriver} class.
    expect_s4_class(ctx$drv, "DBIDriver")
  },

  NULL
)

#' @rdname DBIspec
#' @format NULL
#' @usage NULL
#' @aliases NULL
#' @section Driver:
spec_driver_constructor <- list(
  constructor = function(ctx) {
    pkg_name <- package_name(ctx)

    #' The backend must support creation of an instance of this driver class
    #' with a \dfn{constructor function}.
    #' By default, its name is the package name without the leading \sQuote{R}
    #' (if it exists), e.g., \code{SQLite} for the \pkg{RSQLite} package.
    default_constructor_name <- gsub("^R", "", pkg_name)

    #' For the automated tests, the constructor name can be tweaked using the
    #' \code{constructor_name} tweak.
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
    #' \code{constructor_relax_args} tweak is set to \code{TRUE},
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

  NULL
)

#' @rdname DBIspec
#' @format NULL
#' @usage NULL
#' @aliases NULL
#' @section Driver:
spec_driver_data_type <- list(
  #' SQL Data types exist for all basic R data types. dbDataType() does not
  #' throw an error and returns a nonempty atomic character.
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

  NULL
)

#' @rdname DBIspec
#' @format NULL
#' @usage NULL
#' @aliases NULL
#' @section Driver:
spec_driver_get_info <- list(
  #' Return value of dbGetInfo has necessary elements.
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

  NULL
)

#' @rdname DBIspec
#' @format NULL
#' @usage NULL
#' @aliases NULL
#' @section Driver:
spec_driver_stress <- list(
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

#' @rdname DBIspec
#' @format NULL
spec_driver <- c(
  spec_driver_class,
  spec_driver_constructor,
  spec_driver_data_type,
  spec_driver_get_info,
  spec_driver_stress
)
