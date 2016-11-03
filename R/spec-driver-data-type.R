#' @template dbispec-sub
#' @format NULL
#' @section Driver:
#' \subsection{`dbDataType("DBIDriver", "ANY")`}{
spec_driver_data_type <- list(
  #' The backend can override the [DBI::dbDataType()] generic
  #' for its driver class.
  data_type_driver = function(ctx) {
    #' This generic expects an arbitrary object as second argument
    #' and returns a corresponding SQL type
    check_driver_data_type <- function(value) {
      eval(bquote({
        #' as atomic
        expect_equal(length(dbDataType(ctx$drv, .(value))), 1L)
        #' character value
        expect_is(dbDataType(ctx$drv, .(value)), "character")
        #' with at least one character.
        expect_match(dbDataType(ctx$drv, .(value)), ".")
        #' As-is objects (i.e., wrapped by [base::I()]) must be
        #' supported and return the same results as their unwrapped counterparts.
        expect_identical(dbDataType(ctx$drv, I(.(value))),
                         dbDataType(ctx$drv, .(value)))
      }))
    }

    #'
    #' To query the values returned by the default implementation,
    #' run `example(dbDataType, package = "DBI")`.
    #' If the backend needs to override this generic,
    #' it must accept all basic R data types as its second argument, namely
    expect_driver_has_data_type <- function(value) {
      eval(bquote(
        expect_error(check_driver_data_type(.(value)), NA)))
    }

    #' [base::logical()],
    expect_driver_has_data_type(logical(1))
    #' [base::integer()],
    expect_driver_has_data_type(integer(1))
    #' [base::numeric()],
    expect_driver_has_data_type(numeric(1))
    #' [base::character()],
    expect_driver_has_data_type(character(1))
    #' dates (see [base::Dates()]),
    expect_driver_has_data_type(Sys.Date())
    #' date-time (see [base::DateTimeClasses()]),
    expect_driver_has_data_type(Sys.time())
    #' and [base::difftime()].
    expect_driver_has_data_type(Sys.time() - Sys.time())
    #' It also must accept lists of `raw` vectors
    #' and map them to the BLOB (binary large object) data type.
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      expect_driver_has_data_type(blob::as.blob(raw(1)))
    }
    #' The behavior for other object types is not specified.
  },

  #' }
  NULL
)
