update_dbi <- function() {
  dbitest_path <- getwd()
  dbi_path <- normalizePath(file.path(getwd(), "..", "DBI"), mustWork = FALSE)
  if (!dir.exists(file.path(dbi_path, "man")))
    return()

  if (spec_rd_too_old(dbitest_path, dbi_path)) {
    warning("Please recreate documentation to update DBI specs.", call. = FALSE)
  }

  copy_spec(dbitest_path, dbi_path)
}

spec_rd_too_old <- function(dbitest_path, dbi_path) {
  source_files <- dir(path = file.path(dbitest_path, "R"), pattern = "^spec-", full.names = TRUE)
  rd_file <- file.path(dbi_path, "man", "DBIspec.Rd")
  source_info <- file.info(source_files, extra_cols = FALSE)
  rd_info <- file.info(rd_file, extra_cols = FALSE)

  is.na(rd_info$mtime) || any(source_info$mtime > rd_info$mtime)
}

copy_spec <- function(dbitest_path, dbi_path) {
  spec <- readLines(file.path(dbitest_path, "man", "DBIspec.Rd"))
  if (update_file(spec[-1:-2], file.path(dbi_path, "man", "DBIspec.Rd"))) {
    message("Updated DBI documentation in ", dbi_path)
  }
}

update_file <- function(contents, file) {
  old_contents <- tryCatch(readLines(file), error = function(e) character())
  # Always write to update timestamp
  writeLines(contents, file)
  !identical(contents, old_contents)
}
