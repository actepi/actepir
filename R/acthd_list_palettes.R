#' Get ACTHD palette details
#' 
#' This function will allow the user to get palette details.
#' 
#' @param detail What level of detail to be returned.  Valid options are `names` or `all`. Default is `names`. 
#' @param objtype The type of object to be returned.  Valid options are `list` or `df`.  Default is `list`.
#' 
#' @keywords palettes
#' 
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr row_number
#' @importFrom stringr str_split_i
#' @importFrom magrittr %>%
#' 
#' @export
#' 
#' @examples 
#' # get a simple list of palette names
#' acthd_list_palettes()
#' 
acthd_list_palettes <- function(detail = "names", objtype = "list") {
  
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
  palette = .acthd_palettes()
  
  # define result
  if (objtype == "list") {
    
    if(detail == "all") result = palette
    if(detail == "names") result = names(palette)
    
    } else if (objtype == "df") {
    
    result = palette |> 
      base::unlist() %>% 
      base::as.data.frame() |>
      tibble::rownames_to_column("rn") |>
      dplyr::rename(hex_code=".") |>
      dplyr::mutate(
        palette_name = sub('\\.[^\\.]*$', '', rn),
        color_name = stringr::str_split_i(string=rn,pattern="\\.",-1)
        ) |> 
      dplyr::select(-rn) |> 
      dplyr::relocate(palette_name,color_name) |> 
      dplyr::group_by(palette_name) |>
      dplyr::mutate(
        color_order = dplyr::row_number()
        ) |>
      dplyr::ungroup()
    
    if (detail == "all") {
      result = result
    } else if (detail == "names") {
      result = result$palette_name
    }
    
    }
  
  return(result)
  
}