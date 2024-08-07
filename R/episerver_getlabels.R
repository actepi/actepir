#' Download labels from EpiServer for a given dataset
#' 
#' This function will allow the user to download the labels file for a given dataset.  The function will always search EpiServer and cannot be used for other servers of file locations.
#' 
#' Almost all non-administrative users will not need to use the lbl_* arguments.
#' 
#' @param dataset The name of the dataset that labels are associated with. Argument should be supplied as character or object.  Mandatory.
#' @param lbl_table The name of the table where the labels exist. Argument should be supplied as character. If not supplied, will default to `DataLabels`.
#' @param lbl_schema The name of the schema to be connected to. Argument should be supplied as character. If not supplied, will default to `ref`.
#' @param lbl_db The name of the database to be connected to. Argument should be supplied as character. If not supplied, will default to `Analysis`.
#' 
#' @keywords episerver
#' 
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr tbl
#' @importFrom dplyr collect
#' @importFrom dbplyr in_catalog
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # Import APC labels from default `Analysis` database
#' labels = episerver_getlabels("APC")
#' 
#' # Import APC labels from non-default location
#' labels = episerver_getlabels(
#'   dataset = "APC", 
#'   lbl_table = "DataLabelsNew", 
#'   lbl_schema = "dbo", 
#'   lbl_db = "HospitalAPC"
#'   )
#'  

episerver_getlabels <- function(dataset, lbl_table = NULL, lbl_schema = NULL, lbl_db = NULL) {
  
  # helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }
  
  # check required arguments present
  if (is_invalid(dataset)) {
    stop("Function requires argument 'dataset' to be supplied")
  }
  
  
  # Recursive function to find the 'x' with class 'dbplyr_table_path'
  find_dbplyr_table_path <- function(query_structure) {
    # Check if the current structure is a list and has an element named 'x'
    if (is.list(query_structure) && "x" %in% names(query_structure)) {
      # Access the 'x' element
      x_element <- query_structure$x
      
      # Check if the class of 'x' is 'dbplyr_table_path'
      if ("dbplyr_table_path" %in% class(x_element)) {
        return(x_element)
      }
      
      # If 'x' is a list, continue searching recursively
      return(find_dbplyr_table_path(x_element))
    }
    
    # Return NULL if no match is found
    return(NULL)
  }
  
  
  # Check if the dataset is a lazy query
  if (inherits(dataset, "tbl_sql")) {
    
    # Extract the table info from the lazy query
    table_info <- find_dbplyr_table_path(dataset$lazy_query)
    
    # Remove any leading and trailing double quotes and split by "."
    table_parts <- strsplit(gsub("\"", "", table_info), "\\.")[[1]]
    
    # Extract the database, schema, and table names
    if (length(table_parts) == 3) {
      
      dta_db <- table_parts[1]
      dta_schema <- table_parts[2]
      dta_table <- table_parts[3]
      
    } else {
      
      stop("The structure of lazy_query does not match expected format. Labels cannot be applied.")
      
    }
    
    conn       = dataset$src$con
    collection = dta_table
    lbl_db     = dplyr::case_when(is_invalid(lbl_db)    ~dta_db,       .default=lbl_db)
    lbl_schema = dplyr::case_when(is_invalid(lbl_schema)~"ref",        .default=lbl_schema)
    lbl_table  = dplyr::case_when(is_invalid(lbl_table) ~"DataLabels", .default=lbl_table)
    
  } else if(!inherits(dataset, "tbl_sql") &
            !is_invalid(dataset) & 
            is.character(dataset) & 
            !is_invalid(lbl_db) & 
            !is_invalid(lbl_schema) & 
            !is_invalid(lbl_table)) {
    
    conn = DBI::dbConnect(
      odbc::odbc(), 
      driver = episerver_serverdetails("driver"), 
      server = episerver_serverdetails("server"), 
      port = episerver_serverdetails("port")
    )
    collection = dataset
    
  } else {
    
    stop("The supplied arguments do not resolve to a valid labels file. Labels cannot be loaded")
    
  }
  
  # load labels from inherited dbplyr/lazy_load object 
  labs = dplyr::tbl(conn, dbplyr::in_catalog(lbl_db,lbl_schema,lbl_table))  %>%
    dplyr::filter(
      Dataset == collection &
        tolower(LabelType) %in% c("opt","var")
    ) %>% 
    dplyr::collect() 
  
  return(labs)
  
}