#' Establish a connection to EpiServer
#' 
#' This function will allow the user to quickly establish an ODBC connection to a names table on the EpiServer, using `DBI` and `odbc` standards.  This is generally the first step in creating a table alias for a `dbplyr` query.
#' 
#' Note that this function is hard-coded to reference the EpiServer and cannot be used to contact a different server.  It will default to the `Analysis.dbo` namespace unless arguments are supplied.
#' 
#' @param table The name of the table to be connected to. Argument should be supplied as character.
#' @param schema The name of the schema to be connected to. Argument should be supplied as character. If not supplied, will defaulto to `dbo`.
#' @param db The name of the database to be connected to. Argument should be supplied as character. If not supplied, will defaulto to `Analysis`.
#' 
#' @keywords episerver
#' 
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_catalog
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # Connect to APC table in default `Analysis` database
#' conn = episerver_connect("APC")
#' 
#' # Connect to `TestTable` table in non-default `AnalysisArchive` database
#' conn = episerver_connect("TestTable", db="AnalysisArchive")
#'  
episerver_connect <- function(table, schema="dbo", db="Analysis") {
  
  # helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }
  
  # check required arguments present
  if (is_invalid(table) | table == "") {
    stop("Function requires argument 'table' to be supplied")
  }
  
  DBI::dbConnect(
    odbc::odbc(), 
    driver = episerver_serverdetails("driver"), 
    server = episerver_serverdetails("server"), 
    port = episerver_serverdetails("port")
    ) %>% 
    dplyr::tbl(dbplyr::in_catalog(db,schema,table))
}