test_backend <- function(target, reporter = NULL) {
  target <- sub("^test-", "", target)
  message("Target: ", target)
  rx <- "^([^-]+)-(.*)$"

  # odbc
  if (grepl(rx, target)) {
    message("ODBC detected")
    pkg <- sub(rx, "\\1", target)
    message("pkg: ", pkg)
    driver <- sub(rx, "\\2", target)
    message("driver: ", driver)
    filter <- paste0("driver-", driver)
    message("filter: ", filter)
    dsn <- toupper(gsub("-", "", driver))
    message("dsn: ", dsn)
    cs <- paste0("dsn=", dsn)
    if (filter == "driver-sql-server") {
      cs <- paste0(cs, ";UID=SA;PWD=Password12")
    }
    names(cs) <- paste0("ODBC_CS_", dsn)
    do.call(Sys.setenv, as.list(cs))
  } else {
    pkg <- target
    filter <- "DBItest"
  }

  local_options(crayon.enabled = TRUE)
  pkgload::load_all("..")
  testthat::test_local(pkg, filter = paste0("^", filter, "$"), stop_on_failure = TRUE, reporter = reporter)
}
