#' Internal & non-exported functions
#' 
#' @keywords internal
#' 
#' @details
#' This function  allows the user quickly locate details about the EpiServer, such as address and port.
#' The feature/detail to be returned. Can be one of: "driver", "server", "port", "default_db" or "default_schema". Argument should be supplied as character.
#' episerver_serverdetails("port")
#' 
episerver_serverdetails <- function(feature) {
  
  # helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }
  
  feature_list = list(
    driver         = "ODBC Driver 17 for SQL Server",
    server         = "PRDSQL121vs.act.gov.au",
    port           = 1433,
    default_db     = "Analysis",
    default_schema = "dbo"
  )
  feature_list_names <- paste(names(feature_list), collapse = ", ")
  
  # check required arguments present
  if (is_invalid(feature)) {
    output = feature_list
  } else if (!feature %in% names(feature_list)) {
    stop(paste0("Function 'episerver_serverdetails' requires argument 'feature' to be supplied as a string from the following options: ",feature_list_names) )
  } else if (feature %in% names(feature_list)) {
    output = as.character(feature_list[[feature]])
  }
  
  return(output)
  
}

# helper function to encapsulate argument validity logic
is_invalid <- function(arg) {
  
  missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  
}