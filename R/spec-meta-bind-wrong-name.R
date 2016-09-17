#' @template dbispec-sub-wip
#' @format NULL
#' @section Meta:
#' \subsection{\code{dbBind("DBIResult")}}{
spec_meta_bind_wrong_name <- list(
  #' Named binding of integer values (colon syntax) with wrong names.
  bind_wrong_name_named_colon = function(ctx) {
    with_connection({
      test_select_bind(con, named_colon, 1L, extra = "wrong_name")
    })
  },

  #' Named binding of integer values (dollar syntax) with wrong names.
  bind_wrong_name_named_dollar = function(ctx) {
    with_connection({
      test_select_bind(con, named_dollar, 1L, extra = "wrong_name")
    })
  },

  #' }
  NULL
)
