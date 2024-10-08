% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/context.R
\name{make_context}
\alias{make_context}
\alias{set_default_context}
\alias{get_default_context}
\title{Test contexts}
\usage{
make_context(
  drv,
  connect_args = NULL,
  set_as_default = TRUE,
  tweaks = NULL,
  name = NULL,
  default_skip = NULL
)

set_default_context(ctx)

get_default_context()
}
\arguments{
\item{drv}{\verb{[DBIConnector]}\cr
An object of class \link[DBI:DBIConnector-class]{DBI::DBIConnector} that describes how to connect
to the database.}

\item{connect_args}{\verb{[named list]}\cr Deprecated.}

\item{set_as_default}{\verb{[logical(1)]}\cr Should the created context be
set as default context?}

\item{tweaks}{\verb{[DBItest_tweaks]}\cr Tweaks as constructed by the
\code{\link[=tweaks]{tweaks()}} function.}

\item{name}{\verb{[character]}\cr An optional name of the context which will
be used in test messages.}

\item{default_skip}{\verb{[character]}\cr Default value of \code{skip} argument
to \code{\link[=test_all]{test_all()}}  and other testing functions.}

\item{ctx}{\verb{[DBItest_context]}\cr A test context.}
}
\value{
\verb{[DBItest_context]}\cr A test context, for
\code{set_default_context} the previous default context (invisibly) or
\code{NULL}.
}
\description{
Create a test context, set and query the default context.
}
\examples{
\dontshow{if (requireNamespace("RSQLite", quietly = TRUE)) withAutoprint(\{ # examplesIf}
make_context(
  new(
    "DBIConnector",
    .drv = RSQLite::SQLite(),
    .conn_args = list(dbname = tempfile("DBItest", fileext = ".sqlite"))
  ),
  tweaks = tweaks(
    constructor_relax_args = TRUE,
    placeholder_pattern = c("?", "$1", "$name", ":name"),
    date_cast = function(x) paste0("'", x, "'"),
    time_cast = function(x) paste0("'", x, "'"),
    timestamp_cast = function(x) paste0("'", x, "'"),
    logical_return = function(x) as.integer(x),
    date_typed = FALSE,
    time_typed = FALSE,
    timestamp_typed = FALSE
  ),
  default_skip = c("roundtrip_date", "roundtrip_timestamp")
)
\dontshow{\}) # examplesIf}
}
