#' Get ACTHD colour details
#' 
#' This function will allow the user to get colour details.
#' 
#' @param detail What level of detail to be returned.  Valid options are `names` or `all`. Default is `names`. 
#' @param objtype The type of object to be returned.  Valid options are `list` or `df`.  Default is `list`.
#' 
#' @keywords colors
#' 
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename
#' 
#' @export
#' 
#' @examples 
#' # get a simple list of color names
#' acthd_list_colors()
#' 
acthd_list_colors <- function(detail = "names", objtype = "list") {
  
  # test inputs 
  detail = tolower(detail)
  objtype = tolower(objtype)
  valid_objtype = c("list","df")
  valid_detail  = c("all","names")
  
  if(!objtype %in% valid_objtype){
    stop(paste0("Function 'acthd_list_palettes' requires argument 'objtype' to be supplied as a string from the following options: ",paste(valid_objtype, collapse = ", ")))
  }
  
  if(!detail %in% valid_detail){
    stop(paste0("Function 'acthd_list_palettes' requires argument 'detail' to be supplied as a string from the following options: ",paste(valid_detail, collapse = ", ")))
  }
  
  #import palettes
  palette = as.list(.acthd_colors())
  
  # define result
  if (objtype == "list") {
    
    if(detail == "all") result = palette
    if(detail == "names") result = names(palette)
    
  } else if (objtype == "df") {
    
    result = palette |> 
      base::unlist() %>% 
      base::as.data.frame() |>
      tibble::rownames_to_column("color_name") |>
      dplyr::rename(hex_code=".")
    
    if (detail == "all") {
      result = result
    } else if (detail == "names") {
      result = result$color_name
    }
    
  }
  
  return(result)
  
}