#' DBI specification
#'
#' @description
#' The \pkg{DBI} package defines the generic DataBase Interface for R.
#' The connection to individual DBMS is made by packages that import \pkg{DBI}
#' (so-called \emph{DBI backends}).
#' This document formalizes the behavior expected by the functions declared in
#' \pkg{DBI} and implemented by the individal backends.
#'
#' To ensure maximal portability and exchangeability, and to reduce the effort
#' for implementing a new DBI backend, the \pkg{DBItest} package defines
#' a comprehensive set of test cases that test conformance to the DBI
#' specification.
#' In fact, this document is derived from comments in the test definitions of
#' the \pkg{DBItest} package.
#' This ensures that an extension or update to the tests will be reflected in
#' this document.
#'
#' @name DBIspec
NULL
