# http://stackoverflow.com/a/39880324/946850
s4_methods <- function(env, pkg_fun = NULL) {
  generics <- methods::getGenerics(env)

  if (is.null(pkg_fun)) {
    ok <- TRUE
  } else {
    ok <- pkg_fun(generics@package)
  }


  res <- Map(
    generics@.Data[ok], generics@package[ok], USE.NAMES = TRUE,
    f = function(name, package) {
      what <- methods::methodsPackageMetaName("T", paste(name, package, sep = ":"))

      table <- get(what, envir = env)

      mget(ls(table, all.names = TRUE), envir = table)
    })
  unlist(res, recursive = FALSE)
}

s4_real_argument_names <- function(s4_method) {
  expect_is(s4_method, c("function", "MethodDefinition", "derivedDefaultMethod"))
  unwrapped <- s4_unwrap(s4_method)
  names(formals(unwrapped))
}

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
      if (inherits(local_fun, "function"))
        return(local_fun)
    }
  }

  s4_method
}
