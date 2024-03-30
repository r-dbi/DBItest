#' spec_driver_data_type
#' @family driver specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @inherit test_data_type
spec_driver_data_type <- list(
  data_type_formals = function() {
    # <establish formals of described function>
    expect_equal(names(formals(dbDataType)), c("dbObj", "obj", "..."))
  },
  #
  data_type_driver = function(ctx) {
    test_data_type(ctx, ctx$drv)
  },
  #
  NULL
)

#' test_data_type
#' @param ctx,dbObj Arguments to internal test function
#' @keywords internal
test_data_type <- function(ctx, dbObj) {
  #' @return
  #' `dbDataType()` returns the SQL type that corresponds to the `obj` argument
  check_data_type <- function(value) {
    eval(bquote({
      #' as a non-empty
      expect_match(dbDataType(dbObj, .(value)), ".")
      #' character string.
      if (!is.data.frame(value)) {
        expect_equal(length(dbDataType(dbObj, .(value))), 1L)
      } else {
        #' For data frames, a character vector with one element per column
        #' is returned.
        expect_equal(length(dbDataType(dbObj, value)), .(ncol(value)))
      }
      expect_type(dbDataType(dbObj, .(value)), "character")
      expect_visible(dbDataType(dbObj, .(value)))
    }))
  }

  #'
  #' @section Failure modes:
  #' An error is raised for invalid values for the `obj` argument such as a
  #' `NULL` value.
  expect_error(dbDataType(dbObj, NULL))

  #' @section Specification:
  #' The backend can override the [dbDataType()] generic
  #' for its driver class.
  #'
  #' This generic expects an arbitrary object as second argument.
  #' To query the values returned by the default implementation,
  #' run `example(dbDataType, package = "DBI")`.
  #' If the backend needs to override this generic,
  #' it must accept all basic R data types as its second argument, namely
  expect_has_data_type <- function(value) {
    eval(bquote(
      expect_error(check_data_type(.(value)), NA)
    ))
  }

  expected_data_types <- list(
    #' [logical],
    logical(1),
    #' [integer],
    integer(1),
    #' [numeric],
    numeric(1),
    #' [character],
    character(1),
    #' dates (see [Dates]),
    Sys.Date(),
    #' date-time (see [DateTimeClasses]),
    Sys.time(),
    #' and [difftime].
    Sys.time() - Sys.time(),
    #' If the database supports blobs,
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      #' this method also must accept lists of [raw] vectors,
      list(as.raw(0:10))
    },
    if (!isTRUE(ctx$tweaks$omit_blob_tests)) {
      #' and [blob::blob] objects.
      blob::blob(as.raw(0:10))
    }
  )

  map(
    compact(expected_data_types),
    expect_has_data_type
  )

  expect_has_data_type(data.frame(a = 1, b = "2", stringsAsFactors = FALSE))

  #' As-is objects (i.e., wrapped by [I()]) must be
  #' supported and return the same results as their unwrapped counterparts.
  map(
    compact(expected_data_types),
    function(value) {
      if (!is.null(value)) {
        eval(bquote(
          expect_error(
            expect_identical(
              dbDataType(dbObj, I(.(value))),
              dbDataType(dbObj, .(value))
            ),
            NA
          )
        ))
      }
    }
  )

  #' The SQL data type for [factor] and
  expect_identical(
    dbDataType(dbObj, letters),
    dbDataType(dbObj, factor(letters))
  )
  #' [ordered] is the same as for character.
  expect_identical(
    dbDataType(dbObj, letters),
    dbDataType(dbObj, ordered(letters))
  )

  #' The behavior for other object types is not specified.
}
