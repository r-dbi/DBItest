#' @name test_all
#' @section Tests:
#' \code{\link{test_compliance}}:
#' Test full compliance to DBI
NULL

#' Test full compliance to DBI
#'
#' @inheritParams test_all
#' @include test_meta.R
#' @family tests
#' @export
test_compliance <- function(skip = NULL, ctx = get_default_context()) {
  test_suite <- "Full compliance"

  #' @details
  #' This function defines the following tests:
  #' \describe{
  tests <- list(
    #' \item{\code{compliance}}{
    #' The package defines three classes that implement the required methods.
    #' }
    compliance = function() {
      pkg <- package_name(ctx)

      where <- asNamespace(pkg)

      sapply(names(key_methods), function(name) {
        dbi_class <- paste0("DBI", name)
        
        classes <- Filter(function(class) {
          extends(class, dbi_class) && getClass(class)@virtual == FALSE
        }, getClasses(where))

        expect_gt(length(classes), 0)
        
        sapply(classes, function(class) {
          mapply(function(method, args) {
            expect_has_class_method(method, class, args, where)
          }, names(key_methods[[name]]), key_methods[[name]])
        })
      })
    },

    #' \item{\code{read_only}}{
    #' Writing to the database fails.  (You might need to set up a separate
    #' test context just for this test.)
    #' }
    read_only = function() {
      with_connection({
        expect_error(dbWriteTable(con, "test", data.frame(a = 1)))
      })
    },

    NULL
  )
  #'}
  run_tests(tests, skip, test_suite, ctx$name)
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
