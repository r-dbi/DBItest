# Extract all S4 methods from an environment
#
# Retrieves all S4 methods defined in the given environment, with optional
# filtering by package. This is used to introspect DBI implementations and
# verify that required methods are properly defined.
#
# Source: http://stackoverflow.com/a/39880324/946850
#
# @param env Environment to search for S4 methods
# @param pkg_fun Optional function to filter by package names
# @return Named list of S4 method definitions
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

# Extract argument names from S4 method definition
#
# Gets the actual argument names from an S4 method by unwrapping it to find
# the underlying function definition. This is necessary because S4 methods
# may be wrapped in additional layers that obscure the true parameters.
#
# @param s4_method S4 method definition object
# @return Character vector of argument names
s4_real_argument_names <- function(s4_method) {
  expect_s4_class(s4_method, "function")
  expect_s4_class(s4_method, "MethodDefinition")
  unwrapped <- s4_unwrap(s4_method)
  names(formals(unwrapped))
}

# Unwrap S4 method to find the underlying function definition
#
# S4 methods are often wrapped in a structure that defines a .local function
# containing the actual implementation. This function unwraps that structure
# to access the core function definition for introspection purposes.
#
# @param s4_method S4 method definition object
# @return Unwrapped function, or original method if unwrapping not possible
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
