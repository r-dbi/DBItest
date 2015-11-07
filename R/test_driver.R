#' Test the "Driver" class
#'
#' @inheritParams test_all
#' @include test_getting_started.R
#' @family tests
#' @export
test_driver <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Driver"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{inherits_from_driver}}{
    #' Driver inherits from "DBIDriver" class
    #' }
    inherits_from_driver = function() {
      expect_is(ctx$drv, "DBIDriver")
    },

    #' \item{\code{data_type}}{
    #' SQL Data types exist for all basic R data types. dbDataType() does not
    #' throw an error and returns a nonempty atomic character
    #' }
    data_type = function() {
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

      # Q: Should the "raw" type be matched to BLOB?
      expect_driver_has_data_type(logical(1))
      expect_driver_has_data_type(integer(1))
      expect_driver_has_data_type(numeric(1))
      expect_driver_has_data_type(character(1))
      expect_driver_has_data_type(list(1))
      expect_driver_has_data_type(Sys.Date())
      expect_driver_has_data_type(Sys.time())
    },

    #' \item{\code{constructor_strict}}{
    #' package name starts with R;
    #' package exports constructor function, named like the package without the
    #'   leading R, that has no arguments
    #' }
    constructor_strict = function() {
      pkg_name <- package_name(ctx)

      expect_match(pkg_name, "^R")
      constructor_name <- gsub("^R", "", pkg_name)

      pkg_env <- getNamespace(pkg_name)
      eval(bquote(
        expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))))
      eval(bquote(
        expect_true(exists(.(constructor_name), mode = "function", pkg_env))))
      constructor <- get(constructor_name, mode = "function", pkg_env)
      expect_that(constructor, arglist_is_empty())
    },

    #' \item{\code{constructor}}{
    #' package exports constructor function, named like the package without the
    #'   leading R (if it exists), where all arguments have default values
    #' }
    constructor = function() {
      pkg_name <- package_name(ctx)

      constructor_name <- gsub("^R", "", pkg_name)

      pkg_env <- getNamespace(pkg_name)
      eval(bquote(
        expect_true(.(constructor_name) %in% getNamespaceExports(pkg_env))))
      eval(bquote(
        expect_true(exists(.(constructor_name), mode = "function", pkg_env))))
      constructor <- get(constructor_name, mode = "function", pkg_env)
      expect_that(constructor, all_args_have_default_values())
    },

    #' \item{\code{get_info}}{
    #' Return value of dbGetInfo has necessary elements
    #' }
    get_info = function() {
      info <- dbGetInfo(ctx$drv)
      expect_is(info, "list")
      info_names <- names(info)

      expect_true("driver.version" %in% info_names)
      expect_true("client.version" %in% info_names)
      expect_true("max.connections" %in% info_names)
    },

    #' \item{\code{show}}{
    #' show method for driver class is defined
    #' }
    show = function() {
      expect_that(ctx$drv, has_method("show"))
      expect_output(show(ctx$drv), ".")
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite)
}
