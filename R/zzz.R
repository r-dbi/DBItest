.onLoad <- function(libname, pkgname) {
  if (is_installed("debugme")) {
    # Necessary to re-parse environment variable
    get(".onLoad", asNamespace("debugme"))(libname, pkgname)

    debugme::debugme()
  }

  debug_info()
}

activate_debugme <- function(bangs = 2) {
  old_debugme <- remove_from_logging(get_debugme())
  old_debugme <- gsub("(.)$", "\\1,", old_debugme)

  my_debugme <- paste0(strrep("!", bangs), get_pkgname())

  set_debugme(paste0(old_debugme, my_debugme))
}

deactivate_debugme <- function() {
  new_debugme <- remove_from_logging(get_debugme())
  set_debugme(new_debugme)
}

get_debugme <- function() {
  Sys.getenv("DEBUGME")
}

set_debugme <- function(debugme) {
  Sys.setenv("DEBUGME" = debugme)
  message("DEBUGME=", debugme)
}

remove_from_logging <- function(spec) {
  spec <- gsub(paste0("!*", get_pkgname(), ""), "", spec)
  spec <- gsub(",,+", ",", spec)
  spec
}

debug_info <- function(pkgname) {
  "!DEBUG `get_pkgname()` loaded"
  "!!DEBUG Two bangs"
  "!!!DEBUG Three bangs"
  "!!!!DEBUG Four bangs"
}

get_pkgname <- function() {
  environmentName(topenv(environment()))
}
