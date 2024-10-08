% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-driver-connect.R
\docType{data}
\name{spec_driver_connect}
\alias{spec_driver_connect}
\title{spec_driver_connect}
\value{
\code{dbConnect()} returns an S4 object that inherits from \link[DBI:DBIConnection-class]{DBI::DBIConnection}.
This object is used to communicate with the database engine.

A \code{\link[=format]{format()}} method is defined for the connection object.
It returns a string that consists of a single line of text.
}
\description{
spec_driver_connect
}
\section{Specification}{

DBI recommends using the following argument names for authentication
parameters, with \code{NULL} default:
\itemize{
\item \code{user} for the user name (default: current user)
\item \code{password} for the password
\item \code{host} for the host name (default: local connection)
\item \code{port} for the port number (default: local connection)
\item \code{dbname} for the name of the database on the host, or the database file
name
}

The defaults should provide reasonable behavior, in particular a
local connection for \code{host = NULL}.  For some DBMS (e.g., PostgreSQL),
this is different to a TCP/IP connection to \code{localhost}.

In addition, DBI supports the \code{bigint} argument that governs how
64-bit integer data is returned.  The following values are supported:
\itemize{
\item \code{"integer"}: always return as \code{integer}, silently overflow
\item \code{"numeric"}: always return as \code{numeric}, silently round
\item \code{"character"}: always return the decimal representation as \code{character}
\item \code{"integer64"}: return as a data type that can be coerced using
\code{\link[=as.integer]{as.integer()}} (with warning on overflow), \code{\link[=as.numeric]{as.numeric()}}
and \code{\link[=as.character]{as.character()}}
}
}

\seealso{
Other driver specifications: 
\code{\link{spec_driver_constructor}},
\code{\link{spec_driver_data_type}},
\code{\link{spec_get_info}}
}
\concept{driver specifications}
