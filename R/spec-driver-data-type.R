#' @template dbispec-sub
#' @section Driver:
#' \subsection{\code{dbDataType("DBIDriver", "ANY")}}{}
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
