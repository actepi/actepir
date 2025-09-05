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
  
  # Helper function to encapsulate argument validity logic
  is_invalid <- function(arg) {
    missing(arg) || is.null(arg) || (is.atomic(arg) && length(arg) == 1 && is.na(arg))
  }

  # Dynamically select best driver
  drivers <- unique(odbc::odbcListDrivers()$name)
  odbc_drivers <- grep("ODBC Driver.*SQL Server", drivers, value = TRUE)
  snac_driver <- "SQL Server Native Client 11.0"
  sql_server_driver <- "SQL Server"
  
  selected_driver <- if (length(odbc_drivers) > 0) {
    versions <- as.numeric(gsub("[^0-9.]", "", odbc_drivers))
    odbc_drivers[which.max(versions)]
  } else if (snac_driver %in% drivers) {
    snac_driver
  } else if (sql_server_driver %in% drivers) {
    sql_server_driver
  } else {
    NA
  }
  
  # Construct feature list
  feature_list = list(
    driver         = selected_driver,
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