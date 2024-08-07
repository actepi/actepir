% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episerver_connect.R
\name{episerver_connect}
\alias{episerver_connect}
\title{Establish a connection to EpiServer}
\usage{
episerver_connect()
}
\description{
This function will allow the user to establish an ODBC connection to the EpiServer, using \code{DBI} and \code{odbc} standards.  This is generally the first step in creating a table alias for a \code{dbplyr} piped query.
}
\details{
Note that this function is hard-coded to reference the EpiServer and cannot be used to establish a connection to a different server.
}
\examples{
# Connect to EpiServer
conn = episerver_connect()
 
}
\keyword{episerver}
