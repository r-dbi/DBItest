#' spec_meta_get_info_result
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @name spec_get_info
spec_meta_get_info_result <- list(
  get_info_result = function(ctx) {
    #' @return
    #' For objects of class [DBIResult-class], `dbGetInfo()`
    with_connection({
      info <- with_result(
        dbSendQuery(con, trivial_query()),
        dbGetInfo(res)
      )

      #' returns a named list
      expect_type(info, "list")

      info_names <- names(info)

      #' that contains at least the following components:
      #'
      necessary_names <- c(
        #' - `statatment`: the statement used with [dbSendQuery()] or [dbExecute()],
        #'   as returned by [dbGetStatement()],
        "statement",
        #' - `row.count`: the number of rows fetched so far (for queries),
        #'   as returned by [dbGetRowCount()],
        "row.count",
        #' - `rows.affected`: the number of rows affected (for statements),
        #'   as returned by [dbGetRowsAffected()]
        "rows.affected",
        #' - `has.completed`: a logical that indicates
        #'   if the query or statement has completed,
        #'   as returned by [dbHasCompleted()].
        "has.completed"
      )

      for (name in necessary_names) {
        eval(bquote(
          expect_true(.(name) %in% info_names)))
      }
    })
  },

  NULL
)
