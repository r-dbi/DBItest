% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-driver-data-type.R
\docType{data}
\name{spec_driver_data_type}
\alias{spec_driver_data_type}
\title{spec_driver_data_type}
\value{
\code{dbDataType()} returns the SQL type that corresponds to the \code{obj} argument
as a non-empty
character string.
For data frames, a character vector with one element per column
is returned.
}
\description{
spec_driver_data_type
}
\section{Failure modes}{

An error is raised for invalid values for the \code{obj} argument such as a
\code{NULL} value.
}

\section{Specification}{

The backend can override the \code{\link[DBI:dbDataType]{DBI::dbDataType()}} generic
for its driver class.

This generic expects an arbitrary object as second argument.
To query the values returned by the default implementation,
run \code{example(dbDataType, package = "DBI")}.
If the backend needs to override this generic,
it must accept all basic R data types as its second argument, namely
\link{logical},
\link{integer},
\link{numeric},
\link{character},
dates (see \link{Dates}),
date-time (see \link{DateTimeClasses}),
and \link{difftime}.
If the database supports blobs,
this method also must accept lists of \link{raw} vectors,
and \link[blob:blob]{blob::blob} objects.
As-is objects (i.e., wrapped by \code{\link[=I]{I()}}) must be
supported and return the same results as their unwrapped counterparts.
The SQL data type for \link{factor} and
\link{ordered} is the same as for character.
The behavior for other object types is not specified.
}

\seealso{
Other driver specifications: 
\code{\link{spec_driver_connect}},
\code{\link{spec_driver_constructor}},
\code{\link{spec_get_info}}
}
\concept{driver specifications}
