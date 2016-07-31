#' @template dbispec-sub
#' @section Full compliance:
spec_compliance_methods <- list(
  #' The package defines three classes that implement the required methods.
  compliance = function(ctx) {
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

  NULL
)


# Helpers -----------------------------------------------------------------

#' @importFrom methods hasMethod
expect_has_class_method <- function(name, class, args, driver_package) {
  full_args <- c(class, args)
  eval(bquote(
    expect_true(hasMethod(.(name), .(full_args), driver_package))
  ))
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
