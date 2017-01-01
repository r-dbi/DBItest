#' @template dbispec-sub
#' @format NULL
#' @inheritSection spec_driver_data_type Specification
NULL

#' spec_driver_data_type
#' @usage NULL
#' @format NULL
#' @keywords NULL
spec_driver_data_type <- list(
  data_type_driver = function(ctx) {
    # <establish formals of described function>
    expect_equal(names(formals(DBI::dbDataType)), c("dbObj", "obj", "..."))

    #' @return
    #' `dbDataType()` returns the SQL type that corresponds to the `obj` argument
    check_driver_data_type <- function(value) {
      eval(bquote({
        #' as a non-empty
        expect_match(dbDataType(ctx$drv, .(value)), ".")
        #' character string.
        expect_equal(length(dbDataType(ctx$drv, .(value))), 1L)
        expect_is(dbDataType(ctx$drv, .(value)), "character")
      }))
    }

    #' An error is raised for invalid values for the `obj` argument.
    eval(bquote(
      expect_error(dbDataType(
        ctx$drv, structure(NULL, class = .(random_table_name(20)))))))

    #' @section Specification:
    #' The backend can override the [DBI::dbDataType()] generic
    #' for its driver class.
    #'
    #' This generic expects an arbitrary object as second argument.
    #' To query the values returned by the default implementation,
    #' run `example(dbDataType, package = "DBI")`.
    #' If the backend needs to override this generic,
    #' it must accept all basic R data types as its second argument, namely
    expect_driver_has_data_type <- function(value) {
      eval(bquote(
        expect_error(check_driver_data_type(.(value)), NA)))
    }

    expected_data_types <- list(
      #' [base::logical()],
      logical(1),
      #' [base::integer()],
      integer(1),
      #' [base::numeric()],
      numeric(1),
      #' [base::character()],
      character(1),
      #' dates (see [base::Dates]),
      Sys.Date(),
      #' date-time (see [base::DateTimeClasses]),
      Sys.time(),
      #' and [base::difftime].
      Sys.time() - Sys.time(),
      #' It also must accept lists of `raw` vectors
      #' and map them to the BLOB (binary large object) data type,
      #' unless the `omit_blob_tests` tweak is set to `TRUE`.
      if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
        list(raw(1))
      }
    )

    lapply(
      compact(expected_data_types),
      expect_driver_has_data_type
    )

    #' As-is objects (i.e., wrapped by [base::I()]) must be
    #' supported and return the same results as their unwrapped counterparts.
    lapply(
      compact(expected_data_types),
      function(value) {
        if (!is.null(value)) {
          eval(bquote(
            expect_error(
              expect_identical(dbDataType(ctx$drv, I(.(value))),
                               dbDataType(ctx$drv, .(value))),
              NA)))
        }
      }
    )

    #' The behavior for other object types is not specified.
  },

  NULL
)
