#' spec_meta_get_info_result
#' @family meta specifications
#' @usage NULL
#' @format NULL
#' @keywords NULL
#' @name spec_get_info
spec_meta_get_info_result <- list(
  get_info_result = function(ctx, con) {
    #' @return
    #' For objects of class [DBIResult-class], `dbGetInfo()`
    res <- local_result(dbSendQuery(con, trivial_query()))
    info <- dbGetInfo(res)

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
        expect_true(.(name) %in% info_names)
      ))
    }
  },
  #
  NULL
)
