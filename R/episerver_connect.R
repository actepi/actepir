#' Establish a connection to EpiServer
#' 
#' This function will allow the user to establish an ODBC connection to the EpiServer, using `DBI` and `odbc` standards.  This is generally the first step in creating a table alias for a `dbplyr` piped query.
#' 
#' Note that this function is hard-coded to reference the EpiServer and cannot be used to establish a connection to a different server.
#' 
#' @keywords episerver
#' 
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' 
#' @export
#' 
#' @examples 
#' # Connect to EpiServer
#' conn = episerver_connect()
#'  
episerver_connect <- function() {
  
  #establish connection to episerver
  conn = DBI::dbConnect(
    odbc::odbc(), 
    driver = episerver_serverdetails("driver"), 
    server = episerver_serverdetails("server"), 
    port = episerver_serverdetails("port")
    )
  
  return(conn)
  
}
