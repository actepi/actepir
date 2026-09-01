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
#' @param max_attempts Integer to specify the number of attempts which will be made
#'   to connect to the server. Defaults to 10.  Workaround for concurrency bug in
#'   rstudio.
#' @param encrypt Character string (\code{"Yes"} / \code{"No"}) or \code{NULL}. Sets the
#'   ODBC \code{Encrypt} keyword, and takes effect only for ODBC Driver 18 or later.
#'   When \code{NULL} (default) the value is resolved, in order, from
#'   \code{getOption("actepir.encrypt")}, then the \code{ACTEPIR_ENCRYPT} environment
#'   variable, then \code{"Yes"}. Set the option or the environment variable to change
#'   the default for every EpiServer connection on a machine or in a session without
#'   passing this argument to each function (see Details).
#' @param trust_certificate Character string (\code{"Yes"} / \code{"No"}) or \code{NULL}.
#'   Sets the ODBC \code{TrustServerCertificate} keyword, for ODBC Driver 18 or later.
#'   When \code{NULL} (default) the value is resolved from
#'   \code{getOption("actepir.trust_certificate")}, then \code{ACTEPIR_TRUST_CERTIFICATE},
#'   then \code{"Yes"}. EpiServer presents a self-signed certificate, so this must be
#'   \code{"Yes"} whenever \code{encrypt = "Yes"} on Driver 18.
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
#' ODBC Driver 18 changed the default of the \code{Encrypt} keyword from \code{"no"}
#' to \code{"yes"}. With encryption on, the driver validates the server certificate,
#' which fails against EpiServer's self-signed certificate. From Driver 18 onward the
#' function therefore adds \code{Encrypt} and \code{TrustServerCertificate} to the
#' connection. Driver 17 and earlier default \code{Encrypt} to \code{"no"} and need
#' neither keyword, so the function leaves that path exactly as it was.
#' 
#' A refused TLS handshake is a property of a machine or server rather than of an
#' individual query, so the encryption settings are resolved from options and
#' environment variables, not only from arguments. Every function that connects to
#' EpiServer does so through \code{episerver_connect}, so a single machine-level
#' setting changes them all at once. For example, on a machine whose server refuses
#' the encrypted handshake, adding \code{ACTEPIR_ENCRYPT=No} to \code{.Renviron}, or
#' \code{options(actepir.encrypt = "No")} to \code{.Rprofile}, makes
#' \code{episerver_quickconnect}, \code{collect_withlabels}, \code{episerver_getlabels},
#' \code{episerver_browse} and the rest all connect unencrypted, with no argument
#' passed to any of them. An explicit argument to \code{episerver_connect} still
#' overrides both.
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
#' # One-off override for a single connection
#' conn_noenc <- episerver_connect(encrypt = "No")
#' 
#' # Machine-wide override (put in .Renviron, then restart R):
#' #   ACTEPIR_ENCRYPT=No
#' # every EpiServer function then connects unencrypted, no argument needed.
#' 
#' # Connect with specific driver
#' conn_custom <- episerver_connect(driver = "ODBC Driver 17 for SQL Server")
#' }
#' 
#' @author Warren Holroyd
#'
episerver_connect <- function(driver = NULL, max_attempts = 10,
                              encrypt = NULL, trust_certificate = NULL) {
  
  if(!is.integer(max_attempts)){
    max_attempts = 10
  }
  
  # Resolve the driver-18 encryption settings.
  # Precedence: explicit argument > R option > environment variable > "Yes".
  # This lets a machine or session set the behaviour once (in .Renviron or
  # .Rprofile) and have every EpiServer function inherit it, since they all
  # connect through this function.
  if (is.null(encrypt)) {
    encrypt <- getOption("actepir.encrypt",
                         Sys.getenv("ACTEPIR_ENCRYPT", unset = "Yes"))
  }
  if (is.null(trust_certificate)) {
    trust_certificate <- getOption("actepir.trust_certificate",
                                   Sys.getenv("ACTEPIR_TRUST_CERTIFICATE", unset = "Yes"))
  }
  
  # Close any existing connections with the same signature first
  tryCatch({
    existing_cons <- dbListConnections(odbc::odbc())
    if(length(existing_cons) > 0) {
      lapply(existing_cons, dbDisconnect)
    }
  }, error = function(e) invisible(NULL))
  
  # Define params
  srv = episerver_serverdetails("server")
  prt = episerver_serverdetails("port")
  drv = if(is.null(driver)){
    episerver_serverdetails("driver") 
  } else {
    driver 
  }
  
  # Base connection arguments (Windows authentication)
  conn_args <- list(
    odbc::odbc(),
    driver = drv,
    server = srv,
    port   = prt,
    Trusted_Connection = "Yes"
  )
  
  # ODBC Driver 18 defaults Encrypt to "yes" and then validates the server
  # certificate, which fails against EpiServer's self-signed certificate. Add
  # Encrypt and TrustServerCertificate from driver 18 onward, and leave driver 17
  # and earlier (which default Encrypt to "no") exactly as before. The version is
  # read from the driver name; non-versioned fallbacks (SQL Server Native Client,
  # "SQL Server") parse to NA and are treated as pre-18.
  odbc_ver <- suppressWarnings(
    as.integer(sub(".*ODBC Driver ([0-9]+).*", "\\1", drv))
  )
  if (!is.na(odbc_ver) && odbc_ver >= 18) {
    conn_args$Encrypt <- encrypt
    conn_args$TrustServerCertificate <- trust_certificate
  }
  
  # Establish connection with retry logic and suppressed output
  for(attempt in 1:max_attempts) {
    
    # Suppress all the ODBC driver noise
    result <- tryCatch({
      capture.output({
        conn <- suppressMessages(suppressWarnings({
          do.call(DBI::dbConnect, conn_args)
        }))
      }, type = "message")
      
      return(conn)
      
    }, error = function(e) {
      if(attempt == max_attempts) {
        stop("Failed to connect after ", max_attempts, " attempts. Last error: ", e$message)
      }
      Sys.sleep(0.5)  # Brief pause before retry
      return(NULL)  # Signal to continue loop
    })
    
    # If we got a valid connection, return it
    if(!is.null(result)) {
      return(result)
    }
    
  }
  
}