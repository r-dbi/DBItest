#' @template dbispec-sub
#' @section Meta:
#' \subsection{\code{dbIsValid("DBIConnection")}}{}
spec_meta_is_valid_connection <- list(
  #' Only an open connection is valid.
  is_valid_connection = function(ctx) {
    con <- connect(ctx)
    expect_true(dbIsValid(con))
    expect_error(dbDisconnect(con), NA)
    expect_false(dbIsValid(con))
  },

  NULL
)
