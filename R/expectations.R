arglist_is_empty <- function() {
  function(x) {
    expect_true(
      is.null(formals(x)),
      "has empty argument list")
  }
}

all_args_have_default_values <- function() {
  function(x) {
    args <- formals(x)
    args <- args[names(args) != "..."]
    expect_true(
      all(vapply(args, as.character, character(1L)) != ""),
      "has arguments without default values")
  }
}

has_method <- function(method_name) {
  function(x) {
    my_class <- class(x)
    expect_true(
      length(findMethod(method_name, my_class)) > 0L,
      paste("object of class", my_class, "has no", method_name, "method"))
  }
}
