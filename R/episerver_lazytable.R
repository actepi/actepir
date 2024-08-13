#' Alias an EpiServer table as a `dbplyr` lazy table
#' 
#' This function allows the user to alias a specific table (with a `dbplyr` lazy query).  Note that this function is hard-coded to reference the EpiServer and cannot be used to contact a different server.  It will default to the `Analysis.dbo` namespace unless `schema` and `db` arguments are also supplied.
#' 
#' @param table The name of the table to be aliased. Argument should be supplied as character.
#' @param schema The name of the schema to be aliased. Argument should be supplied as character. If not supplied, will default to `dbo`.
#' @param db The name of the database to be aliased. Argument should be supplied as character. If not supplied, will default to `Analysis`.
#' 
#' @keywords episerver
#' 
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_catalog
#' 
#' @export
#' 
#' @examples 
#' # Alias the APC table in default `Analysis` database
#' apc_tbl = episerver_lazytable("APC")
#' 
#' # Alias the `TestTable` table in non-default `AnalysisArchive` database
#' test_tbl = episerver_lazytable("TestTable", db="AnalysisArchive")
#'
episerver_lazytable <- function(table, schema="dbo", db="Analysis") {
  
  # helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }
  
  # check required arguments present
  if (is_invalid(table) | table == "") {
    stop("Function requires argument 'table' to be supplied")
  }
  
  # establish connection and pass to lazy query
  lazytable = dplyr::tbl(dbplyr::in_catalog(db,schema,table))
  
  return(lazytable)
  
}
