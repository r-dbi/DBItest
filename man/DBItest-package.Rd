% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBItest.R
\docType{package}
\name{DBItest-package}
\alias{DBItest}
\alias{DBItest-package}
\title{DBItest: Testing DBI Backends}
\description{
A helper that tests DBI back ends for conformity to the interface.
}
\details{
The two most important functions are \code{\link[=make_context]{make_context()}} and
\code{\link[=test_all]{test_all()}}.  The former tells the package how to connect to your
DBI backend, the latter executes all tests of the test suite. More
fine-grained test functions (all with prefix \code{test_}) are available.

See the package's vignette for more details.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://dbitest.r-dbi.org}
  \item \url{https://github.com/r-dbi/DBItest}
  \item Report bugs at \url{https://github.com/r-dbi/DBItest/issues}
}

}
\author{
Kirill Müller
}
