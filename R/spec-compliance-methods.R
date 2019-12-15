#' @format NULL
#' @importFrom callr r
#' @section DBI classes and methods:
spec_compliance_methods <- list(
  #' A backend defines three classes,
  compliance = function(ctx) {
    #' which are subclasses of
    expect_identical(
      names(key_methods),
      c(
        #' [DBIDriver-class],
        "Driver",
        #' [DBIConnection-class],
        "Connection",
        #' and [DBIResult-class].
        "Result"
      )
    )

    pkg <- package_name(ctx)

    where <- asNamespace(pkg)

    sapply(names(key_methods), function(name) {
      dbi_class <- paste0("DBI", name)

      classes <- Filter(function(class) {
        extends(class, dbi_class) && getClass(class)@virtual == FALSE
      }, getClasses(where))

      expect_equal(length(classes), 1)

      class <- classes[[1]]

      #' The backend provides implementation for all methods
      #' of these base classes
      #' that are defined but not implemented by DBI.
      mapply(function(method, args) {
        expect_has_class_method(method, class, args, where)
      }, names(key_methods[[name]]), key_methods[[name]])
    })
  },

  #' All methods defined in \pkg{DBI} are reexported (so that the package can
  #' be used without having to attach \pkg{DBI}),
  reexport = function(ctx) {
    pkg <- package_name(ctx)

    where <- asNamespace(pkg)
    dbi <- asNamespace("DBI")

    dbi_generics <- grep("^[.]__T__db", getNamespaceExports(dbi), value = TRUE)
    . <- gsub("^[.]__T__(.*):DBI$", "\\1", dbi_generics)
    . <- setdiff(., c("dbListConnections", "dbSetDataMappings", "dbGetException", "dbCallProc", "dbGetConnectArgs"))
    . <- c(., "Id")
    dbi_names <- .

    exported_names <- callr::r(
      function(pkg) {
        tryCatch(
          getNamespaceExports(getNamespace(pkg)),
          error = function(e) character()
        )
      },
      args = list(pkg = pkg)
    )

    # Guard against scenarios where package is not installed
    if (length(exported_names) > 0) {
      missing <- setdiff(dbi_names, exported_names)
      expect_equal(paste(missing, collapse = ", "), "")
    }
  },

  #' and have an ellipsis `...` in their formals for extensibility.
  ellipsis = function(ctx) {
    pkg <- package_name(ctx)

    where <- asNamespace(pkg)

    methods <- s4_methods(where, function(x) x == "DBI")
    methods <- methods[grep("^db", names(methods))]
    Map(expect_ellipsis_in_formals, methods, names(methods))
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

expect_ellipsis_in_formals <- function(method, name) {
  sym <- as.name(name)
  eval(bquote({
    .(sym) <- method
    expect_true("..." %in% s4_real_argument_names(.(sym)))
  }))
}

key_methods <- list(
  Driver = list(
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
