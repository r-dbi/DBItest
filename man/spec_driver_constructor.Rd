% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-driver-constructor.R
\docType{data}
\name{spec_driver_constructor}
\alias{spec_driver_constructor}
\title{spec_driver_constructor}
\description{
spec_driver_constructor
}
\section{Construction of the DBIDriver object}{

The backend must support creation of an instance of its \link[DBI:DBIDriver-class]{DBI::DBIDriver} subclass
with a \dfn{constructor function}.
By default, its name is the package name without the leading \sQuote{R}
(if it exists), e.g., \code{SQLite} for the \pkg{RSQLite} package.
However, backend authors may choose a different name.
The constructor must be exported, and
it must be a function
that is callable without arguments.
DBI recommends to define a constructor with an empty argument list.
}

\seealso{
Other driver specifications: 
\code{\link{spec_driver_connect}},
\code{\link{spec_driver_data_type}},
\code{\link{spec_get_info}}
}
\concept{driver specifications}
