#' Establish a quick connection to EpiServer table
#' 
#' This function allows the user to quickly establish an ODBC connection to the EpiServer (using `DBI` and `odbc` standards) and then alias a specific table (with a `dbplyr` lazy query).  
#' 
#' In essence, this function combines `actepir::episerver_connect` with `dplyr::tbl(dbplyr::in_catalog())`.  Both steps are integrated into a single wraparound function for convenience.
#' 
#' Note that this function is hard-coded to reference the EpiServer and cannot be used to contact a different server.  It will default to the `Analysis.dbo` namespace unless `schema` and `db` arguments are also supplied.
#' 
#' @param table The name of the table to be connected to. Argument should be supplied as character.
#' @param schema The name of the schema to be connected to. Argument should be supplied as character. If not supplied, will default to `dbo`.
#' @param db The name of the database to be connected to. Argument should be supplied as character. If not supplied, will default to `Analysis`.
#' @param driver Your choice of driver if you wish to override automatic selection
#' @param register Whether to register the connection with `rstudioapi` in the connections window.
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
#' apc_tbl = episerver_quickconnect("APC")
#' 
#' # Connect to `TestTable` table in non-default `AnalysisArchive` database
#' test_tbl = episerver_quickconnect("TestTable", db="AnalysisArchive")
#'
episerver_quickconnect <- function(table, schema="dbo", db="Analysis", driver = NULL, register = TRUE) {
  
  # helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }
  
  # check required arguments present
  if (is_invalid(table) | table == "") {
    stop("Function requires argument 'table' to be supplied")
  }
  
  # establish connection and pass to lazy query
  conn = episerver_connect(driver=driver, register=register) %>% 
    dplyr::tbl(dbplyr::in_catalog(db,schema,table))
  
}
