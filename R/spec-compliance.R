#' @rdname DBIspec
#' @format NULL
#' @section Full compliance:
spec_compliance <- list(
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

  #' Writing to the database fails.  (You might need to set up a separate
  #' test context just for this test.)
  read_only = function(ctx) {
    with_connection({
      expect_error(dbWriteTable(con, "test", data.frame(a = 1)))
    })
  },

  NULL
)
