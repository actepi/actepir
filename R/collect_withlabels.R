#' Collect in dbplyr pipe with labels
#' 
#' This function is a wraparound function which combines the `episerver_getlabels` and `dplyr::collect` functions and feeds them to the `applydatalabels` function.  It will allow the user to collect the result of a lazy query with associated labels when the query is placed against an EpiServer location.  The function will always search EpiServer and cannot be used for other servers of file locations.
#' 
#' Almost all non-administrative users will not need to use the lbl_* arguments.  In most cases, when accessing the "Analysis" database, you will not need to supply the "dataset" argument.
#' 
#' @param dataset The name of the dataset that labels are associated with. Argument 
#'   should be supplied as character or object.
#' @param lbl_table The name of the table where the labels exist. Argument should 
#'   be supplied as character.
#' @param lbl_schema The name of the schema to be connected to. Argument should be 
#'   supplied as character.
#' @param lbl_db The name of the database to be connected to. Argument should be 
#'   supplied as character.
#' @param driver Character string or \code{NULL}. ODBC driver to use for the connection.
#'   If \code{NULL} (default), the function will automatically select the appropriate 
#'   driver using \code{episerver_serverdetails("driver")}. See \code{\link[odbc]{odbcListDrivers}}
#'   to identify which drivers you have access to.
#' @param max_attempts Integer to specify the number of attempts which will be made
#'   to connect to the server. Defaults to \code{episerver_connect} default.  Workaround 
#'   for concurrency bug in RStudio.
#'   
#' @keywords episerver
#' 
#' @importFrom dplyr collect
#' 
#' @export
#' 
#' @examples 
#' # One-step process: apply labels as part of collect() 
#' # statement when a dbplyr lazy object is passed to it
#' dat = episerver_connect("APC") %>% 
#'   collect_withlabels()
#' 
#' # One-step process: apply labels as part of collect() 
#' # statement but source labels from another location
#' dat = episerver_connect("APC") %>% 
#'   collect_withlabels(
#'     dataset = "APC", 
#'     lbl_table = "DataLabelsNew", 
#'     lbl_schema = "dbo", 
#'     lbl_db = "HospitalAPC"
#'     )
#'   
collect_withlabels <- function(dataset, lbl_db = NULL, lbl_schema = NULL, lbl_table = NULL, driver = NULL, max_attempts = NULL) {
  
  return(
    applydatalabels(
      data   = dplyr::collect(dataset),
      labels = episerver_getlabels(dataset, lbl_table, lbl_schema, lbl_db,driver=driver,max_attempts=max_attempts)
    )
  ) 
  
}