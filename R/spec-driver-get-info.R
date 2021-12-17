#' spec_driver_get_info
#' @family driver specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @name spec_get_info
spec_driver_get_info <- list(
  #' @return
  #' For objects of class [DBIDriver-class], `dbGetInfo()`
  get_info_driver = function(ctx) {
    info <- dbGetInfo(ctx$drv)

    #' returns a named list
    expect_type(info, "list")

    info_names <- names(info)

    #' that contains at least the following components:
    #'
    necessary_names <- c(
      #' - `driver.version`: the package version of the DBI backend,
      "driver.version",
      #' - `client.version`: the version of the DBMS client library.
      "client.version"
    )

    for (name in necessary_names) {
      eval(bquote(
        expect_true(.(name) %in% info_names)
      ))
    }
  },
  #
  NULL
)
