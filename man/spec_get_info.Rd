% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec-driver-get-info.R,
%   R/spec-connection-get-info.R, R/spec-meta-get-info-result.R
\docType{data}
\name{spec_get_info}
\alias{spec_get_info}
\alias{spec_driver_get_info}
\alias{spec_connection_get_info}
\alias{spec_meta_get_info_result}
\title{spec_driver_get_info}
\value{
For objects of class \link[DBI:DBIDriver-class]{DBI::DBIDriver}, \code{dbGetInfo()}
returns a named list
that contains at least the following components:
\itemize{
\item \code{driver.version}: the package version of the DBI backend,
\item \code{client.version}: the version of the DBMS client library.
}

For objects of class \link[DBI:DBIConnection-class]{DBI::DBIConnection}, \code{dbGetInfo()}
returns a named list
that contains at least the following components:
\itemize{
\item \code{db.version}: version of the database server,
\item \code{dbname}: database name,
\item \code{username}: username to connect to the database,
\item \code{host}: hostname of the database server,
\item \code{port}: port on the database server.
It must not contain a \code{password} component.
Components that are not applicable should be set to \code{NA}.
}

For objects of class \link[DBI:DBIResult-class]{DBI::DBIResult}, \code{dbGetInfo()}
returns a named list
that contains at least the following components:
\itemize{
\item \code{statatment}: the statement used with \code{\link[DBI:dbSendQuery]{DBI::dbSendQuery()}} or \code{\link[DBI:dbExecute]{DBI::dbExecute()}},
as returned by \code{\link[DBI:dbGetStatement]{DBI::dbGetStatement()}},
\item \code{row.count}: the number of rows fetched so far (for queries),
as returned by \code{\link[DBI:dbGetRowCount]{DBI::dbGetRowCount()}},
\item \code{rows.affected}: the number of rows affected (for statements),
as returned by \code{\link[DBI:dbGetRowsAffected]{DBI::dbGetRowsAffected()}}
\item \code{has.completed}: a logical that indicates
if the query or statement has completed,
as returned by \code{\link[DBI:dbHasCompleted]{DBI::dbHasCompleted()}}.
}
}
\description{
spec_driver_get_info

spec_connection_get_info

spec_meta_get_info_result
}
\seealso{
Other driver specifications: 
\code{\link{spec_driver_connect}},
\code{\link{spec_driver_constructor}},
\code{\link{spec_driver_data_type}}

Other connection specifications: 
\code{\link{spec_connection_disconnect}}

Other meta specifications: 
\code{\link{spec_meta_bind}},
\code{\link{spec_meta_column_info}},
\code{\link{spec_meta_get_row_count}},
\code{\link{spec_meta_get_rows_affected}},
\code{\link{spec_meta_get_statement}},
\code{\link{spec_meta_has_completed}},
\code{\link{spec_meta_is_valid}}
}
\concept{connection specifications}
\concept{driver specifications}
\concept{meta specifications}
