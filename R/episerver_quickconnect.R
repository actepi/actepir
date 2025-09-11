#' Establish a Quick Connection to EpiServer Table
#' 
#' @description
#' This function provides a convenient wrapper to establish an ODBC connection to the 
#' EpiServer database and create a lazy query reference to a specific table. It combines 
#' the functionality of \code{\link{episerver_connect}} with \code{dplyr::tbl(dbplyr::in_catalog())} 
#' in a single function call, eliminating the need for multiple steps when accessing 
#' database tables.
#' 
#' The function returns a \code{dbplyr} lazy query object that can be used with 
#' standard \code{dplyr} verbs for data manipulation without immediately executing 
#' queries against the database.
#' 
#' @param table Character string. The name of the database table to connect to. 
#'   This parameter is required and cannot be empty or \code{NULL}.
#' @param schema Character string. The database schema name. Defaults to \code{"dbo"} 
#'   if not specified.
#' @param db Character string. The database name. Defaults to \code{"Analysis"} 
#'   if not specified.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the connection. 
#'   If \code{NULL} (default), the function will use automatic driver selection.
#' 
#' @return A \code{tbl_dbi} object representing a lazy query to the specified database 
#'   table. This object can be used with \code{dplyr} verbs for data manipulation.
#' 
#' @details
#' This function is specifically designed for connecting to EpiServer databases and 
#' cannot be used with other database servers. The connection uses ODBC standards 
#' via the \code{DBI} and \code{odbc} packages.
#' 
#' The default namespace is \code{Analysis.dbo}, but this can be customized using 
#' the \code{db} and \code{schema} parameters. The function performs basic validation 
#' on the required \code{table} parameter and will throw an error if it's missing, 
#' empty, or \code{NULL}.
#' 
#' @note
#' \itemize{
#'   \item This function requires an active network connection to the EpiServer
#'   \item Appropriate database permissions are required to access the specified table
#'   \item The returned object is a lazy query - no data is retrieved until explicitly collected
#'   \item Use \code{dplyr::collect()} to execute the query and retrieve data into memory
#' }
#' 
#' @seealso 
#' \code{\link{episerver_connect}} for establishing database connections,
#' \code{\link[dplyr]{tbl}} for creating table references,
#' \code{\link[dbplyr]{in_catalog}} for specifying database catalogs,
#' \code{\link[dplyr]{collect}} for executing lazy queries
#' 
#' @keywords database episerver connection
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
#' \dontrun{
#' # Basic usage: Connect to APC table in default Analysis.dbo namespace
#' apc_tbl <- episerver_quickconnect("APC")
#' 
#' # Use with dplyr operations (still lazy, not collected yet)
#' filtered_data <- apc_tbl %>%
#'   filter(date >= "2023-01-01") %>%
#'   select(id, date, value)
#' 
#' # Execute query and collect results
#' results <- filtered_data %>% collect()
#' 
#' # Connect to table in different database
#' archive_tbl <- episerver_quickconnect(
#'   table = "TestTable", 
#'   db = "AnalysisArchive"
#'   )
#' 
#' # Specify custom driver (if needed)
#' special_tbl <- episerver_quickconnect(
#'   table = "ED",
#'   driver = "ODBC Driver 13 for SQL Server"
#'   )
#' 
#' # Don't register connection in RStudio (for programmatic use)
#' automated_tbl <- episerver_quickconnect(
#'   table = "ED",
#'   register = FALSE
#'   )
#' }
#' 
#' @author Warren Holroyd
#'
episerver_quickconnect <- function(table, schema="dbo", db="Analysis", driver = NULL, max_attempts = NULL) {
  
  # check required arguments present
  if (is_invalid(table) | table == "") {
    stop("Function requires argument 'table' to be supplied")
  }
  
  # establish connection and pass to lazy query
  conn = episerver_connect(driver=driver,max_attempts=max_attempts) %>% 
    dplyr::tbl(dbplyr::in_catalog(db,schema,table))
  
  return(conn)
}