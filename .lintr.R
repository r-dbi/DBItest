linters <- modify_defaults(
  all_linters(),
  absolute_path_linter = NULL,
  cyclocomp_linter = NULL,
  expect_identical_linter = NULL,
  expect_shape_linter = NULL, # 94 occurrences, to fix
  if_switch_linter = NULL, # false positive: r-lib/lintr#2835
  # expect_warning(x <- ...) is idiomatic, see r-lib/lintr#2267
  implicit_assignment_linter = implicit_assignment_linter(
    except = c("bquote", "expression", "expr", "quo", "quos", "quote", "expect_warning")
  ),
  implicit_integer_linter = NULL,
  keyword_quote_linter = NULL,
  line_length_linter = NULL,
  namespace_linter = NULL,
  nonportable_path_linter = NULL,
  nzchar_linter = NULL, # x != "" reads better than nzchar(x)
  object_length_linter = NULL,
  object_name_linter = NULL,
  object_overwrite_linter = NULL,
  object_usage_linter = NULL,
  one_call_pipe_linter = NULL,
  pipe_consistency_linter = NULL, # 51 occurrences, to fix
  todo_comment_linter = NULL,
  undesirable_function_linter = NULL,
  unused_import_linter = NULL
)
