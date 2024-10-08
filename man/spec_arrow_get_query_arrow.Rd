% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-arrow-get-query-arrow.R
\docType{data}
\name{spec_arrow_get_query_arrow}
\alias{spec_arrow_get_query_arrow}
\title{spec_arrow_get_query_arrow}
\value{
\code{dbGetQueryArrow()} always returns an object coercible to a \link{data.frame}, with
as many rows as records were fetched and as many
columns as fields in the result set,
even if the result is a single value
or has one
or zero rows.
}
\description{
spec_arrow_get_query_arrow
}
\section{Failure modes}{

An error is raised when issuing a query over a closed
or invalid connection,
if the syntax of the query is invalid,
or if the query is not a non-\code{NA} string.
The object returned by \code{dbGetQueryArrow()} can also be passed to
\code{\link[nanoarrow:as_nanoarrow_array_stream]{nanoarrow::as_nanoarrow_array_stream()}} to create a nanoarrow
array stream object that can be used to read the result set
in batches.
The chunk size is implementation-specific.
}

\section{Additional arguments}{

The following arguments are not part of the \code{dbGetQueryArrow()} generic
(to improve compatibility across backends)
but are part of the DBI specification:
\itemize{
\item \code{params} (default: \code{NULL})
\item \code{immediate} (default: \code{NULL})
}

They must be provided as named arguments.
See the "Specification" and "Value" sections for details on their usage.

The \code{param} argument allows passing query parameters, see \code{\link[DBI:dbBind]{DBI::dbBind()}} for details.
}

\section{Specification for the \code{immediate} argument}{


The \code{immediate} argument supports distinguishing between "direct"
and "prepared" APIs offered by many database drivers.
Passing \code{immediate = TRUE} leads to immediate execution of the
query or statement, via the "direct" API (if supported by the driver).
The default \code{NULL} means that the backend should choose whatever API
makes the most sense for the database, and (if relevant) tries the
other API if the first attempt fails. A successful second attempt
should result in a message that suggests passing the correct
\code{immediate} argument.
Examples for possible behaviors:
\enumerate{
\item DBI backend defaults to \code{immediate = TRUE} internally
\enumerate{
\item A query without parameters is passed: query is executed
\item A query with parameters is passed:
\enumerate{
\item \code{params} not given: rejected immediately by the database
because of a syntax error in the query, the backend tries
\code{immediate = FALSE} (and gives a message)
\item \code{params} given: query is executed using \code{immediate = FALSE}
}
}
\item DBI backend defaults to \code{immediate = FALSE} internally
\enumerate{
\item A query without parameters is passed:
\enumerate{
\item simple query: query is executed
\item "special" query (such as setting a config options): fails,
the backend tries \code{immediate = TRUE} (and gives a message)
}
\item A query with parameters is passed:
\enumerate{
\item \code{params} not given: waiting for parameters via \code{\link[DBI:dbBind]{DBI::dbBind()}}
\item \code{params} given: query is executed
}
}
}
}

\seealso{
Other Arrow specifications: 
\code{\link{spec_arrow_append_table_arrow}},
\code{\link{spec_arrow_create_table_arrow}},
\code{\link{spec_arrow_fetch_arrow}},
\code{\link{spec_arrow_fetch_arrow_chunk}},
\code{\link{spec_arrow_read_table_arrow}},
\code{\link{spec_arrow_send_query_arrow}},
\code{\link{spec_arrow_write_table_arrow}},
\code{\link{spec_result_clear_result}}
}
\concept{Arrow specifications}
