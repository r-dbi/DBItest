#' @template dbispec-sub-wip
#' @format NULL
#' @section Parametrised queries and statements:
#' \subsection{`dbBind("DBIResult")`}{
spec_meta_bind_multi_row <- list(
  #' Binding of multi-row integer values.
  bind_multi_row = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3))
    })
  },

  #' Binding of multi-row integer values with zero rows.
  bind_multi_row_zero_length = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(integer(), integer()))
    })
  },

  #' Binding of multi-row integer values with unequal length.
  bind_multi_row_unequal_length = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3, 2:4), extra = "unequal_length")
    })
  },

  #' Binding of multi-row statements.
  bind_multi_row_statement = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3), query = FALSE)
    })
  },

  #' }
  NULL
)

#' @noRd
#' @details
list(
  #' Binding of multi-row integer values with group column.
  bind_multi_row_group_column = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3), extra = "group_column")
    })
  },

  #' Binding of multi-row integer values with group column and zero rows.
  bind_multi_row_group_column_zero_length = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(integer(), integer()), extra = "group_column")
    })
  },

  #' Binding of multi-row integer values, groupwise fetching.
  bind_multi_row_groupwise_fetch = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3), extra = "groupwise_fetch")
    })
  },

  #' Binding of multi-row integer values, groupwise fetching, with group column.
  bind_multi_row_group_column_groupwise_fetch = function(ctx) {
    with_connection({
      test_select_bind(con, ctx$tweaks$placeholder_pattern, list(1:3), extra = c("group_column", "groupwise_fetch"))
    })
  },

  NULL
)
