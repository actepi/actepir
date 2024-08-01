#' Collect in dbplyr pipe with labels
#' 
#' This function is a wraparound function which combines the `episerver_getlabels` and `dplyr::collect` functions and feeds them to the `applydatalabels` function.  It will allow the user to collect the result of a lazy query with associated labels when the query is placed against an EpiServer location.  The function will always search EpiServer and cannot be used for other servers of file locations.
#' 
#' Almost all non-administrative users will not need to use the lbl_* arguments.  In most cases, when accessing the "Analysis" database, you will not need to supply the "dataset" argument.
#' 
#' @param dataset The name of the dataset that labels are associated with. Argument should be supplied as character or object.
#' @param lbl_table The name of the table where the labels exist. Argument should be supplied as character.
#' @param lbl_schema The name of the schema to be connected to. Argument should be supplied as character.
#' @param lbl_db The name of the database to be connected to. Argument should be supplied as character.
#' 
#' @keywords episerver
#' 
#' @import dplyr
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
collect_withlabels <- function(dataset, lbl_db = NULL, lbl_schema = NULL, lbl_table = NULL) {
  
  return(
    applydatalabels(
      data   = dplyr::collect(dataset),
      labels = episerver_getlabels(dataset, lbl_db, lbl_schema, lbl_table)
    )
  ) 
  
}