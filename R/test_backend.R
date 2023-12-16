test_backend <- function(target, reporter = NULL) {
  target <- sub("^test-", "", target)
  rx <- "^([^-]+)-(.*)$"

  # odbc
  if (grepl(rx, target)) {
    pkg <- sub(rx, "\\1", target)
    filter <- sub(rx, "\\2", target)
    dsn <- toupper(gsub("-", "", sub("^driver-", "", filter)))
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

  options(crayon.enabled = TRUE)
  pkgload::load_all("..")
  testthat::test_local(pkg, filter = filter, stop_on_failure = TRUE, reporter = reporter)
}
