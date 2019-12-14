#' spec_meta_get_info_result
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @name spec_get_info
spec_meta_get_info_result <- list(
  get_info_result = function(ctx) {
    #' @return
    #' For objects of class [DBIResult-class], `dbGetInfo()`
    with_connection({
      with_result(
        dbSendQuery(con, trivial_query()),
        {
          info <- dbGetInfo(res)

          #' returns a named list
          expect_type(info, "list")

          info_names <- names(info)

          #' that contains at least the following components:
          #'
          necessary_names <- c(
            #' - `statatment`,
            "statement",
            #' - `row.count`,
            "row.count",
            #' - `rows.affected`,
            "rows.affected",
            #' - `has.completed`.
            "has.completed"
          )

          for (name in necessary_names) {
            eval(bquote(
              expect_true(.(name) %in% info_names)))
          }
        }
      )
    })
  },

  NULL
)
