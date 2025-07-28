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
#' @import rstudioapi
#' 
#' @export
#' 
#' @examples 
#' # Connect to EpiServer
#' conn = episerver_connect()
#'  
episerver_connect <- function() {
  
  # Define params
  drv = episerver_serverdetails("driver")
  srv = episerver_serverdetails("server")
  prt = episerver_serverdetails("port")
  
  # Establish connection to Episerver
  conn = DBI::dbConnect(odbc::odbc(), 
                        driver = drv, 
                        server = srv, 
                        port   = prt,
                        Trusted_Connection = "Yes"
                        )

  # Use rstudioapi to register the connection in the Connections pane
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::sendToConsole(
      paste0(
        "conn <- DBI::dbConnect(odbc::odbc(), driver = '",drv,"', server = '",srv,"', port = ",prt,")"
        )
    )
    }
  
  return(conn)
  
}
