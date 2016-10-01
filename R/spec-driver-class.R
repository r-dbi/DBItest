#' @template dbispec-sub
#' @format NULL
#' @section Driver:
spec_driver_class <- list(
  inherits_from_driver = function(ctx) {
    #' Each DBI backend implements a \dfn{driver class},
    #' which must be an S4 class and inherit from the `DBIDriver` class.
    expect_s4_class(ctx$drv, "DBIDriver")
  },

  #' This section describes the construction of, and the methods defined for,
  #' this driver class.
  NULL
)
