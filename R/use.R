use_dbitest <- function(path) {
  usethis::local_project(path)
  local_options(usethis.overwrite = TRUE)
  usethis::use_testthat(edition = 2)
  usethis::use_package_doc(open = FALSE)
  usethis::use_import_from("rlang", c(
    "quo",
    "enquo",
    "enquos",
    "expr",
    "enexpr",
    "eval_tidy",
    "list2",
    "has_length",
    ":=",
    "abort",
    "is_interactive",
    "as_function",
    "local_options",
    "seq2",
    "set_names",
    "%||%",
    NULL
  ))
  usethis::use_import_from("DBItest", c(
    "get_default_context",
    "connect",
    "local_connection",
    "local_result",
    "local_remove_test_table",
    "try_silent",
    "package_name",
    "get_pkg_path",
    "s4_methods",
    "expect_invisible_true",
    "expect_equal_df",
    "expect_all_args_have_default_values",
    "expect_ellipsis_in_formals",
    "expect_has_class_method",
    "get_key_methods",
    "dbi_generics",
    "check_df",
    "random_table_name",
    "get_placeholder_funs",
    "has_utf8_or_ascii_encoding",
    #
    "trivial_query",
    "trivial_df",
    "trivial_values",
    "get_penguins",
    #
    "get_texts",
    "unrowname",
    #
    "test_data_type",
    "test_select_with_null",
    "test_select",
    "test_select_bind",
    "new_bind_tester_extra",
    "test_table_roundtrip",
    "test_table_roundtrip_one",
    NULL
  ))

  invisible(map2(nested_spec_all, names(nested_spec_all), use_dbitest_spec))
}

use_dbitest_spec <- function(spec, name) {
  if (name == "") {
    return()
  }

  path <- file.path("tests/testthat", paste0("test-dbitest-", name, ".R"))
  header <- c(
    "# Created by DBItest::use_dbitest(), do not edit by hand",
    "ctx <- get_default_context()",
    "con <- local_connection(ctx)",
    NULL
  )

  tests <- compact(map2(spec, names(spec), get_dbitest_test))
  tests_flat <- map_chr(tests, paste, collapse = "\n")
  tests_all <- paste(c(header, tests_flat), collapse = "\n\n")

  usethis::write_over(path, tests_all)
}

get_dbitest_test <- function(fun, name) {
  if (is.null(fun)) {
    return(NULL)
  }

  # FIXME: make more inclusive
  if (!all(names(formals(fun)) %in% c("ctx", "con"))) {
    return(NULL)
  }

  body_code <- format_body(fun)

  c(
    paste0('test_that("', name, '", {'),
    body_code,
    "})"
  )
}

format_body <- function(fun) {
  construct <- constructive::construct(fun, check = TRUE, ignore_function_env = TRUE)
  flat <- unlist(strsplit(format(construct$code), "\n", fixed = TRUE))
  trimws(flat[seq2(2, length(flat) - 1)], "right")
}
