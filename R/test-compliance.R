#' @name test_all
#' @section Tests:
#' \code{\link{test_compliance}}:
#' Test full compliance to DBI
NULL

#' Test full compliance to DBI
#'
#' @inheritParams test_all
#' @include test-meta.R
#' @family tests
#' @export
test_compliance <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Full compliance"

  run_tests(ctx, spec_compliance, skip, test_suite)
}

#' @importFrom methods hasMethod
expect_has_class_method <- function(name, class, args, driver_package) {
  full_args <- c(class, args)
  eval(bquote(
    expect_true(hasMethod(.(name), .(full_args), driver_package))
  ))
}

test_has_methods <- function(generic, class, where) {
  mapply(function(x, args) expect_has_class_method(x, class, args, where),
         names(generic), generic)
}

compliance_message <- function(methods, name) {
  if (all(methods)) return(paste0(name, ": OK"))

  methods <- paste0(names(methods)[!methods], collapse = ", ")
  paste0(name, ": NOT OK\n",
         paste0(strwrap(methods, indent = 2, exdent = 2), collapse = "\n"))
}

key_methods <- list(
  Driver = list(
    "dbGetInfo" = NULL,
    "dbConnect" = NULL,
    "dbDataType" = NULL
  ),
  Connection = list(
    "dbDisconnect" = NULL,
    "dbGetInfo" = NULL,
    "dbSendQuery" = "character",
    "dbListFields" = "character",
    "dbListTables" = NULL,
    "dbReadTable" = "character",
    "dbWriteTable" = c("character", "data.frame"),
    "dbExistsTable" = "character",
    "dbRemoveTable" = "character",
    "dbBegin" = NULL,
    "dbCommit" = NULL,
    "dbRollback" = NULL,
    "dbIsValid" = NULL,
    "dbQuoteString" = "character",
    "dbQuoteIdentifier" = "character"
  ),
  Result = list(
    "dbIsValid" = NULL,
    "dbFetch" = NULL,
    "dbClearResult" = NULL,
    "dbColumnInfo" = NULL,
    "dbGetRowsAffected" = NULL,
    "dbGetRowCount" = NULL,
    "dbHasCompleted" = NULL,
    "dbGetStatement" = NULL,
    "dbBind" = NULL
  )
)
