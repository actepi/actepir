#' Download labels from EpiServer for a given dataset
#' 
#' This function will allow the user to download the labels file for a given dataset.  The function will always search EpiServer and cannot be used for other servers of file locations.
#' 
#' Almost all non-administrative users will not need to use the lbl_* arguments.
#' 
#' @param dataset The name of the dataset that labels are associated with. Argument should be supplied as character or object.
#' @param lbl_table The name of the table where the labels exist. Argument should be supplied as character. If not supplied, will default to `DataLabels`.
#' @param lbl_schema The name of the schema to be connected to. Argument should be supplied as character. If not supplied, will default to `ref`.
#' @param lbl_db The name of the database to be connected to. Argument should be supplied as character. If not supplied, will default to `Analysis`.
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