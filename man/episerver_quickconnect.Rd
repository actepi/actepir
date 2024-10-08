% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episerver_quickconnect.R
\name{episerver_quickconnect}
\alias{episerver_quickconnect}
\title{Establish a quick connection to EpiServer table}
\usage{
episerver_quickconnect(table, schema = "dbo", db = "Analysis")
}
\arguments{
\item{table}{The name of the table to be connected to. Argument should be supplied as character.}

\item{schema}{The name of the schema to be connected to. Argument should be supplied as character. If not supplied, will default to \code{dbo}.}

\item{db}{The name of the database to be connected to. Argument should be supplied as character. If not supplied, will default to \code{Analysis}.}
}
\description{
This function allows the user to quickly establish an ODBC connection to the EpiServer (using \code{DBI} and \code{odbc} standards) and then alias a specific table (with a \code{dbplyr} lazy query).
}
\details{
In essence, this function combines \code{actepir::episerver_connect} with \code{dplyr::tbl(dbplyr::in_catalog())}.  Both steps are integrated into a single wraparound function for convenience.

Note that this function is hard-coded to reference the EpiServer and cannot be used to contact a different server.  It will default to the \code{Analysis.dbo} namespace unless \code{schema} and \code{db} arguments are also supplied.
}
\examples{
# Connect to APC table in default `Analysis` database
apc_tbl = episerver_quickconnect("APC")

# Connect to `TestTable` table in non-default `AnalysisArchive` database
test_tbl = episerver_quickconnect("TestTable", db="AnalysisArchive")

}
\keyword{episerver}
