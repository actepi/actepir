#' Establish a Connection to EpiServer Database
#' 
#' @description
#' Creates an ODBC database connection to the EpiServer using DBI and odbc standards.
#' This function serves as the foundation for database operations and is typically 
#' the first step before creating table references for \code{dbplyr} queries. The 
#' connection uses Windows Authentication (Trusted_Connection) and automatically 
#' retrieves server configuration details.
#' 
#' @param driver Character string or \code{NULL}. ODBC driver to use for the connection.
#'   If \code{NULL} (default), the function will automatically select the appropriate 
#'   driver using \code{episerver_serverdetails("driver")}. See \code{\link[odbc]{odbcListDrivers}}
#'   to identify which drivers you have access to.
#' @param register Logical. Whether to register the database connection in RStudio's 
#'   Connections pane for interactive use. When \code{TRUE} (default), the connection 
#'   will appear in the RStudio Connections tab, allowing for GUI-based database 
#'   exploration. Set to \code{FALSE} for programmatic or automated workflows.
#' 
#' @return A \code{DBIConnection} object representing the database connection to 
#'   EpiServer. This connection object can be used with DBI functions or passed to 
#'   \code{dplyr::tbl()} for creating table references.
#' 
#' @details
#' This function is specifically configured for EpiServer databases and cannot be 
#' used to connect to other database servers. The connection parameters (server, 
#' port, and default driver) are retrieved automatically using the 
#' \code{\link{episerver_serverdetails}} helper function, which should be configured 
#' with the appropriate EpiServer environment details.
#' 
#' The function uses Windows Authentication (\code{Trusted_Connection = "Yes"}), 
#' meaning it will authenticate using the credentials of the currently logged-in 
#' Windows user. This requires that the user has appropriate database access 
#' permissions configured in EpiServer.
#' 
#' When \code{register = TRUE}, the function attempts to register the connection 
#' with RStudio's Connections pane by sending a connection command to the console. 
#' This feature requires the \code{rstudioapi} package and will only work when 
#' running in RStudio.
#' 
#' @note
#' \itemize{
#'   \item Requires Windows Authentication - the current Windows user must have 
#'     database access permissions
#'   \item Network connectivity to the EpiServer is required
#'   \item Connection registration in RStudio requires the \code{rstudioapi} package
#'   \item Remember to close connections using \code{DBI::dbDisconnect()} when finished
#'   \item For security, avoid leaving connections open longer than necessary
#' }
#' 
#' @seealso 
#' \code{\link{episerver_quickconnect}} for direct table connections,
#' \code{\link{collect_withlabels}} collect data in dplyr pipelines with data attached,
#' \code{\link[DBI]{dbConnect}} for general database connections,
#' \code{\link[DBI]{dbDisconnect}} for closing connections,
#' \code{\link[dplyr]{tbl}} for creating table references
#' 
#' @keywords database episerver connection odbc
#' 
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @import rstudioapi
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # Basic connection with default settings
#' conn <- episerver_connect()
#' 
#' # Check connection status
#' DBI::dbIsValid(conn)
#' 
#' # List available tables
#' DBI::dbListTables(conn)
#' 
#' # Connect without registering in RStudio (for automated scripts)
#' conn_auto <- episerver_connect(register = FALSE)
#' 
#' # Connect with specific driver
#' conn_custom <- episerver_connect(
#'   driver = "ODBC Driver 17 for SQL Server"
#' )
#' 
#' # Use connection for table reference
#' library(dplyr)
#' my_table <- tbl(conn, "TableName")
#' 
#' # Query data using the connection
#' result <- tbl(conn, "APC") %>%
#'   filter(admissiondate >= "2023-01-01") %>%
#'   collect()
#' 
#' # Example workflow: connect, query, disconnect
#' tryCatch({
#'   conn <- episerver_connect()
#'   data <- tbl(conn, "MyTable") %>%
#'     select(id, value, date) %>%
#'     collect()
#'   print(nrow(data))
#' }, finally = {
#'   if (exists("conn") && DBI::dbIsValid(conn)) {
#'     DBI::dbDisconnect(conn)
#'   }
#' })
#' }
#' 
#' @author Warren Holroyd
#'
episerver_connect <- function(driver = NULL, register = TRUE) {
  
  # Define params
  srv = episerver_serverdetails("server")
  prt = episerver_serverdetails("port")
  drv = if(is.null(driver)){
    episerver_serverdetails("driver") 
  } else {
    driver 
  }
  
  # Establish connection to Episerver
  conn = DBI::dbConnect(odbc::odbc(), 
                        driver = drv, 
                        server = srv, 
                        port   = prt,
                        Trusted_Connection = "Yes"
  )
  # Use rstudioapi to register the connection in the Connections pane
  if (register) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::sendToConsole(
        paste0(
          "conn <- DBI::dbConnect(odbc::odbc(), driver = '",drv,"', server = '",srv,"', port = ",prt,",Trusted_Connection = 'Yes')"
        )
      )
    }
  }
  
  return(conn)
  
}