expect_success <- function (object, ..., info = NULL, label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  expect_that(object, not(throws_error(...)), info = info, label = label)
}

#' \code{test_driver()} tests the "Driver" class.
#'
#' @rdname test
#' @include test_getting_started.R
#' @export
test_driver <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Driver"

  tests <- list(
    # Driver inherits from "DBIDriver" class
    inherits_from_driver = function() {
      expect_is(ctx$drv, "DBIDriver")
    },

    # SQL Data types exist for all basic R data types. dbDataType() does not
    # throw an error and returns a nonempty atomic character
    data_type = function() {
      expect_driver_data_type_is_character <- function(value) {
        eval(bquote({
          expect_is(dbDataType(ctx$drv, .(value)), "character")
          expect_equal(length(dbDataType(ctx$drv, .(value))), 1L)
          expect_match(dbDataType(ctx$drv, .(value)), ".")
        }))
      }

      expect_driver_has_data_type <- function(value) {
        eval(bquote(
          expect_success(expect_driver_data_type_is_character(.(value)))))
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

    NULL
  )
  run_tests(tests, skip, test_suite)
}
