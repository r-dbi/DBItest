fetch_dbi_generics <- function() {
  dbi <- asNamespace("DBI")

  dbi_generics <- grep("^[.]__T__db", getNamespaceExports(dbi), value = TRUE)
  clean_dbi_generics <- gsub("^[.]__T__(.*):DBI$", "\\1", dbi_generics)
  active_dbi_generics <- setdiff(clean_dbi_generics, c(
    "dbDriver",
    "dbUnloadDriver",
    "dbListConnections",
    "dbListResults",
    "dbSetDataMappings",
    "dbGetException",
    "dbCallProc",
    "dbGetConnectArgs"
  ))
  dbi_names <- sort(c(active_dbi_generics, "Id"))
  dbi_names
}

create_generics <- function() {
  withr::local_collate("C")

  dbi_names <- fetch_dbi_generics()
  text <- paste0(
    "# Created by create_generics(), do not edit by hand\nall_dbi_generics <- function() {\n  c(\n",
    paste0('    "', dbi_names, '",\n', collapse = ""),
    "    NULL\n  )\n}"
  )
  writeLines(text, "R/generics.R")
}

dbi_generics <- function(version) {
  version <- as.package_version(version)

  generics <- all_dbi_generics()

  if (version < "1.7.99.1") {
    generics <- setdiff(generics, c(
      "dbGetQueryArrow",
      "dbAppendTableArrow",
      "dbFetchArrow",
      "dbFetchArrowChunk",
      "dbWriteTableArrow",
      "dbSendQueryArrow",
      "dbReadTableArrow",
      "dbCreateTableArrow"
    ))
  }

  if (version < "1.7.99.11") {
    generics <- setdiff(generics, c(
      "dbBindArrow"
    ))
  }

  generics
}
