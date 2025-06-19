# http://stackoverflow.com/a/39880324/946850
# Introspects S4 methods from a specified environment for DBI implementation verification.
# This function discovers all S4 generic methods and their implementations.
# It supports filtering by package through an optional pkg_fun predicate.
# Returns a comprehensive list of S4 methods available for testing and validation.
s4_methods <- function(env, pkg_fun = NULL) {
  generics <- methods::getGenerics(env)

  if (is.null(pkg_fun)) {
    ok <- TRUE
  } else {
    ok <- pkg_fun(generics@package)
  }


  res <- Map(
    generics@.Data[ok], generics@package[ok],
    USE.NAMES = TRUE,
    f = function(name, package) {
      what <- methods::methodsPackageMetaName("T", paste(name, package, sep = ":"))

      table <- get(what, envir = env)

      mget(ls(table, all.names = TRUE), envir = table)
    }
  )
  unlist(res, recursive = FALSE)
}

# Extracts the actual argument names from S4 method definitions.
# This function unwraps S4 method wrappers to access the underlying function signature.
# It handles the internal .local function pattern used in S4 method definitions.
# Returns the formal argument names for accurate method signature validation.
s4_real_argument_names <- function(s4_method) {
  expect_s4_class(s4_method, "function")
  expect_s4_class(s4_method, "MethodDefinition")
  unwrapped <- s4_unwrap(s4_method)
  names(formals(unwrapped))
}

# Unwraps S4 method definitions to access the underlying implementation function.
# This function extracts the .local function from S4 method body structures.
# It handles the standard pattern where S4 methods wrap their logic in .local functions.
# Returns the actual implementation function or the original method if unwrapping fails.
s4_unwrap <- function(s4_method) {
  # Only unwrap if body is of the following form:
  # {
  #   .local <- function(x, y, z, ...) {
  #     ...
  #   }
  #   ...
  # }
  method_body <- body(s4_method)
  if (inherits(method_body, "{")) {
    local_def <- method_body[[2]]
    if (inherits(local_def, "<-") && local_def[[2]] == quote(.local)) {
      local_fun <- local_def[[3]]
      if (inherits(local_fun, "function")) {
        return(local_fun)
      }
    }
  }

  s4_method
}
