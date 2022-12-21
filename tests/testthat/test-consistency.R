test_that("no unnamed specs", {
  tests <- spec_all[!vapply(spec_all, is.null, logical(1L))]
  vicinity <- NULL
  if (any(names(tests) == "")) {
    vicinity <- sort(unique(unlist(
      lapply(which(names(tests) == ""), "+", -1:1)
    )))
    vicinity <- vicinity[names(tests)[vicinity] != ""]
  }
  expect_null(vicinity)
})

test_that("no duplicate spec names expect known exceptions", {
  all_names <- names(spec_all)

  all_names <- all_names[!(all_names %in% c(
    "create_temporary_table",
    "create_table_visible_in_other_connection",
    "list_tables",
    "exists_table",
    "temporary_table",
    "list_objects",
    "table_visible_in_other_connection",
    "arrow_write_table_arrow_temporary",
    "arrow_write_table_arrow_visible_in_other_connection",
    "arrow_create_table_arrow_visible_in_other_connection",
    "begin_write_disconnect",
    "begin_write_commit",
    NULL
  ))]

  dupe_names <- unique(all_names[duplicated(all_names)])
  expect_equal(dupe_names, rep("", length(dupe_names)))
})

test_that("all specs used", {
  env <- asNamespace("DBItest")
  defined_spec_names <- ls(env, pattern = "^spec_")
  defined_specs <- mget(defined_spec_names, env)
  defined_spec_names <- unlist(sapply(defined_specs, names), use.names = FALSE)
  new_names <- setdiff(defined_spec_names, names(spec_all))
  expect_equal(new_names, rep("", length(new_names)))
})
